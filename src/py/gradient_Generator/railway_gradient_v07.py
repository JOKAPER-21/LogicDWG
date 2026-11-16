#!/usr/bin/env python3
"""
Railway Gradient Designer
=========================
Builds a railway gradient (design rail level) from a survey CSV, in the same
layout as the sheet "MDU - NLL DN" of your MDU-NLL DN LINE GRADIENT workbook.

Input  : CSV (or the xlsx template itself) with 3 columns, same as template A,B,C
            A  Railway Chainage        (text like 496/480.000, optional)
            B  Our company Chainage    (metres)
            C  Railway Elevation       (existing rail level, RL)
Rule   : column G = C - E (existing - new) must stay inside [MIN, MAX]
         (railway requirement, e.g. -0.25 ... +0.25).
Method : template rule, per segment:  N = length / (end existing RL - start design RL)
         O = ROUND(|N|)  (so column N and column O always agree), gradient = 1 in O.
         The longest run of points for which G stays inside [MIN, MAX] with that
         gradient is one segment; then it is cut and the next segment starts from the
         design RL reached (same as the template: start = design RL, end = existing RL).
Zones  : marker rows (text in the chainage column, e.g. "C-2 TTC 1", "SRJ 50A") are kept in the
         table.  No new gradient segment is cut inside a 50 m zone (25 m each side) around every SRJ,
         every TTC and every TCC marker; the cut is made before or after the zone instead.
         (Optional: the whole transition TTC1->TCC1 and TCC2->TTC2.)
AutoCAD: "Export AutoCAD CSV" writes vid_ch, rw_ch, railLevel, lineType, before, current, note.
Signs  : RISE -> E = E(prev) + F      FALL -> E = E(prev) - F      (F always +)

Theme   : Light / System / Dark selector in the top bar (System follows Windows, remembered).

Requires: pip install PySide6 matplotlib numpy openpyxl
Run     : python railway_gradient.py [file.csv]
"""
import csv
import math
import os
import re
import sys
import warnings as _warnings
from dataclasses import dataclass

import numpy as np
import openpyxl
from openpyxl.formatting.rule import FormulaRule
from openpyxl.styles import Alignment, Border, Font, PatternFill, Side
from openpyxl.utils import get_column_letter

from PySide6 import QtCore, QtGui, QtWidgets

import matplotlib
matplotlib.use("QtAgg")
from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg, NavigationToolbar2QT
from matplotlib.figure import Figure
from matplotlib.patches import Patch
from matplotlib.ticker import FuncFormatter
from matplotlib.transforms import offset_copy

SHEET_MAIN = "MDU - NLL DN"
SHEET_ABS = "ABSTRACT"
STEP = 20.0          # template works per 20 m
TOL = 1e-6           # metres, tolerance when checking the band
FLAT_EPS = 1e-4      # metres (0.1 mm, below survey precision): |RL diff| smaller = exactly flat (N blank, O = 0)


# =============================================================================
#  1. INPUT
# =============================================================================
def to_float(x):
    if x is None:
        return None
    if isinstance(x, (int, float)):
        return float(x)
    s = str(x).strip()
    if not s:
        return None
    try:
        return float(s)
    except ValueError:
        return None


def compact_rail(t):
    """'496/480.000' -> '496/480' ; '497/007.110' -> '497/007.11' ; None if not km/m text."""
    if not t:
        return None
    m = re.match(r"^(-?\d+)\s*[/+]\s*(-?\d+(?:\.\d+)?)$", str(t).strip())
    if not m:
        return None
    mm = float(m.group(2))
    ip = int(abs(mm))
    frac = f"{abs(mm) - ip:.3f}"[1:].rstrip("0").rstrip(".")
    return f"{m.group(1)}/{'-' if mm < 0 else ''}{ip:03d}{frac}"


def _map_columns(header):
    """Find the railway-chainage, company-chainage and RL columns from the header names."""
    roles = {}
    for i, h in enumerate(header):
        t = str(h or "").strip().lower()
        if not t or "diff" in t:
            continue
        if "elev" in t or "level" in t or t in ("rl", "c"):
            roles.setdefault("rl", i)
        elif "railway" in t or "rail ch" in t:
            roles.setdefault("ra", i)
        elif "company" in t or t.startswith("our") or t == "ch" or t.startswith("chain"):
            roles.setdefault("co", i)
    return roles


_KM_M = re.compile(r"^(-?\d+)\s*[/+]\s*(-?\d+(?:\.\d+)?)$")


def parse_km_m(t):
    """'0/659.000' -> (0, 659.0) ; None when it is not km/m text."""
    m = _KM_M.match(str(t).strip()) if t else None
    return (int(m.group(1)), float(m.group(2))) if m else None


def locate_markers(markers, rail, b):
    """Company chainage of every marker row.  A marker row (TTC 1, SRJ 50A ...) only carries a
    railway chainage, so it is measured from the neighbouring rows of the same km (km posts are
    not always 1000 m apart, so railway chainage is never converted to one continuous number)."""
    n = len(b)
    for m in markers:
        p = m["prev"]
        nx = p + 1 if p + 1 < n else None
        km = parse_km_m(m["ra"])
        pos = None
        if km:
            kp = parse_km_m(rail[p]) if p >= 0 else None
            kn = parse_km_m(rail[nx]) if nx is not None else None
            if kp and kp[0] == km[0]:
                pos = b[p] + (km[1] - kp[1])
            elif kn and kn[0] == km[0]:
                pos = b[nx] - (kn[1] - km[1])
            elif kp:
                pos = b[p] + (km[0] - kp[0]) * 1000.0 + (km[1] - kp[1])
        if pos is None:                                   # no usable railway chainage: between neighbours
            if p >= 0 and nx is not None:
                pos = (b[p] + b[nx]) / 2.0
            elif p >= 0:
                pos = b[p]
            elif nx is not None:
                pos = b[nx]
        if pos is not None:
            if p >= 0:
                pos = max(pos, float(b[p]))
            if nx is not None:
                pos = min(pos, float(b[nx]))
        m["b"] = None if pos is None else float(pos)


def list_sheets(path):
    with _warnings.catch_warnings():
        _warnings.simplefilter("ignore")
        wb = openpyxl.load_workbook(path, read_only=True)
        names = list(wb.sheetnames)
        wb.close()
    return names


def load_table(path, sheet=None):
    """Return dict(rail_txt, b, c, markers) from csv / txt / xlsx.

    Rows whose chainage cell holds text (e.g. 'C-2 TTC 1', 'SRJ 50A') and have no RL are markers."""
    ext = os.path.splitext(path)[1].lower()
    if ext in (".xlsx", ".xlsm"):
        with _warnings.catch_warnings():
            _warnings.simplefilter("ignore")
            wb = openpyxl.load_workbook(path, data_only=True, read_only=True)
            name = sheet if sheet in wb.sheetnames else (SHEET_MAIN if SHEET_MAIN in wb.sheetnames else wb.sheetnames[0])
            ws = wb[name]
            rows = [(list(r) + [None] * 3)[:3] for r in ws.iter_rows(values_only=True)]
            wb.close()
    else:
        with open(path, "r", encoding="utf-8-sig", newline="") as f:
            text = f.read()
        try:
            dialect = csv.Sniffer().sniff(text[:4096], delimiters=",;\t")
        except csv.Error:
            dialect = csv.excel
        rows = [r for r in csv.reader(text.splitlines(), dialect) if any(x.strip() for x in r)]
    if not rows:
        raise ValueError("The file is empty.")

    first = rows[0]
    has_header = not (len(first) >= 2 and to_float(first[-2]) is not None and to_float(first[-1]) is not None) \
        if len(first) >= 3 else any(to_float(x) is None for x in first)
    roles = _map_columns(first) if has_header else {}
    data = rows[1:] if has_header else rows
    ncols = max(len(r) for r in rows)
    if "co" not in roles or "rl" not in roles:           # fall back to template positions
        if ncols >= 3:
            roles = {"ra": 0, "co": 1, "rl": 2}
        elif ncols == 2:
            roles = {"co": 0, "rl": 1}
        else:
            raise ValueError("Need at least 2 columns: company chainage and elevation.")

    rail, b, c, markers = [], [], [], []
    for r in data:
        r = list(r) + [None] * (ncols - len(r))
        cell_b = r[roles["co"]]
        bv, cv = to_float(cell_b), to_float(r[roles["rl"]])
        rt = r[roles["ra"]] if "ra" in roles else None
        rt = None if rt is None or str(rt).strip() == "" else str(rt).strip()
        if bv is None or cv is None:
            label = "" if cell_b is None else str(cell_b).strip()
            if label and bv is None and cv is None:          # marker row: text label in the chainage column
                markers.append(dict(label=label, ra=rt, prev=len(b) - 1))
            continue
        rail.append(rt)
        b.append(bv)
        c.append(cv)
    if len(b) < 2:
        raise ValueError("Fewer than 2 valid rows (company chainage + elevation).")
    b, c = np.array(b), np.array(c)
    if np.any(np.diff(b) <= 0):
        raise ValueError("Company chainage (column B) must be strictly increasing.")
    locate_markers(markers, rail, b)
    return dict(rail_txt=rail, b=b, c=c, markers=markers)


# =============================================================================
#  2. ENGINE
# =============================================================================
@dataclass
class Segment:
    s: int            # start point index (boundary)
    e: int            # end point index (boundary)
    g: float          # signed gradient used, metres per metre (+ rise, - fall)
    n: int            # O = ROUND(|N|), the whole "1 in N" used (0 = exactly flat)
    forced: bool = False   # True if band could not be kept (see warnings)
    level: bool = False    # shown as LEVEL (very flat)

    @property
    def direction(self):
        return "LEVEL" if self.level else ("RISE" if self.g > 0 else "FALL")

    @property
    def label(self):
        return "LEVEL" if self.level else f"{'R' if self.g > 0 else 'F'} {self.n}"


def round_half_up(x):
    """Excel ROUND(x, 0) for x >= 0."""
    return int(math.floor(x + 0.5))


def rounded_gradient(length, rl_diff):
    """Template rule:  N = length / RL diff ;  O = ROUND(|N|) ;  gradient = sign(N) / O.
    Returns (O, signed gradient per metre)."""
    if abs(rl_diff) < FLAT_EPS:
        return 0, 0.0
    n_calc = length / abs(rl_diff)
    o = max(1, round_half_up(n_calc))
    return o, math.copysign(1.0 / o, rl_diff)


def feasible_ends(b, c, s, es, gmin, gmax, allowed):
    """Every end point i (ascending) for a segment starting at point s with design RL es, such that the
    template gradient (O = ROUND(N), N = length / (end existing RL - start design RL)) keeps
    C - E inside [gmin, gmax] for all points s+1..i.  Points where cutting is not allowed are skipped."""
    out = []
    lo, hi = -math.inf, math.inf               # exact-gradient cone: no gradient at all beyond it
    for i in range(s + 1, len(b)):
        d = b[i] - b[s]
        nlo = max(lo, (c[i] - es - gmax) / d)
        nhi = min(hi, (c[i] - es - gmin) / d)
        if nlo > nhi + 1e-15:
            break
        lo, hi = nlo, nhi
        if not allowed[i]:
            continue
        o, g = rounded_gradient(d, c[i] - es)
        dev = c[s + 1:i + 1] - es - g * (b[s + 1:i + 1] - b[s])
        if dev.min() >= gmin - TOL and dev.max() <= gmax + TOL:
            out.append((i, g, o))
    return out


def _dfs(b, c, gmin, gmax, allowed, s0, es0, max_alt=15, max_pops=800):
    """Longest segment first, from point s0 (design RL es0) to the end of the line.

    When no allowed end point exists any more (a protected zone cannot be crossed inside the band)
    the previous segment is ended earlier (before the zone) and tried again.
    Returns (plan, deepest_plan, deepest_s, deepest_es); plan is None when the line cannot be finished,
    deepest_* is then the furthest point reached without breaking the band."""
    n = len(b)

    def frame(s, es):
        cands = feasible_ends(b, c, s, es, gmin, gmax, allowed)
        return [s, es, cands, len(cands), max_alt, None]      # s, es, cands, next pos, tries left, chosen

    stack, pops = [frame(s0, es0)], 0
    deep_s, deep_es, deep_plan = s0, es0, []
    while stack:
        fr = stack[-1]
        if fr[3] == 0 or fr[4] == 0:
            stack.pop()
            pops += 1
            if pops > max_pops:
                break
            continue
        fr[3] -= 1
        fr[4] -= 1
        i, g, o = fr[2][fr[3]]
        fr[5] = (i, g, o)
        if i == n - 1:
            return [(f[0], f[5][0], f[5][1], f[5][2], False) for f in stack], None, None, None
        es_next = fr[1] + g * (b[i] - b[fr[0]])
        if i > deep_s:
            deep_s, deep_es = i, es_next
            deep_plan = [(f[0], f[5][0], f[5][1], f[5][2], False) for f in stack]
        stack.append(frame(i, es_next))
    return None, deep_plan, deep_s, deep_es


def design_profile(b, c, gmin, gmax, level_n=10000, allowed=None):
    """Cut the line into segments (see feasible_ends).  allowed[i] = False marks points inside a
    protected zone where no new gradient segment may start or end (the line start / end are always ok).
    If a zone cannot be crossed inside [gmin, gmax], only that one segment keeps going (flagged
    'forced', warning + orange cells) and the next segment starts after the zone."""
    n = len(b)
    allowed = np.ones(n, bool) if allowed is None else np.array(allowed, bool)
    allowed[0] = allowed[n - 1] = True
    warnings = []
    plan, s, es = [], 0, float(c[0])
    while s < n - 1:
        done, deep_plan, deep_s, deep_es = _dfs(b, c, gmin, gmax, allowed, s, es)
        if done is not None:
            plan += done
            break
        plan += deep_plan
        s, es = deep_s, deep_es
        i = next(k for k in range(s + 1, n) if allowed[k])      # first point where a cut is allowed
        o, g = rounded_gradient(b[i] - b[s], c[i] - es)
        why = " without a cut inside a no-cut zone" if not allowed.all() else ""
        warnings.append(f"Segment at CH {b[s]:.0f}: MIN/MAX cannot be kept{why}.")
        plan.append((s, i, g, o, True))
        es += g * (b[i] - b[s])
        s = i
    E = np.empty(n)
    E[0] = c[0]
    es = float(c[0])
    segs = []
    for s, i, g, o, forced in plan:
        E[s + 1:i + 1] = es + g * (b[s + 1:i + 1] - b[s])
        level = (o == 0) or (level_n > 0 and o >= level_n)
        segs.append(Segment(s, i, g, o, forced, level))
        es = float(E[i])
    return E, segs, warnings


# =============================================================================
#  2b. NO-CUT ZONES  (SRJ zone length, curve transitions)
# =============================================================================
@dataclass
class ZoneConfig:
    srj_on: bool = True
    srj_kw: str = "SRJ"
    srj_length: float = 50.0          # total zone length, centred on the point (25 m each side)
    ttc_on: bool = True
    ttc_kw: str = "TTC"               # every TTC marker (TTC1 and TTC2 of every curve)
    ttc_length: float = 50.0
    tcc_on: bool = True
    tcc_kw: str = "TCC"               # every TCC marker (TCC1 and TCC2 of every curve)
    tcc_length: float = 50.0
    curve_on: bool = False            # optional extra: also no cut anywhere between the two ends of a transition
    curve1_from: str = "TTC1"
    curve1_to: str = "TCC1"
    curve2_from: str = "TCC2"
    curve2_to: str = "TTC2"


def _where(m):
    return m["ra"] or f"CH {m['b']:.0f}"


def _norm(t):
    """Compare labels ignoring case, spaces, hyphens, dots and underscores: 'TTC-1' = 'TTC 1' = 'TTC1'."""
    return re.sub(r"[\s_\-.]+", "", str(t)).upper()


def _point_zones(markers, zones, kw_text, length, kind):
    """One zone per marker whose label contains kw_text: marker +/- length/2 (50 m -> 25 m each side)."""
    kw = _norm(kw_text)
    if not kw or length <= 0:
        return
    half = length / 2.0
    for m in markers:
        if m.get("b") is not None and kw in _norm(m["label"]):
            m["type"] = kind
            zones.append(dict(kind=kind, lo=m["b"] - half, hi=m["b"] + half, name=m["label"].strip()))


def build_zones(markers, cfg):
    """Zones (company chainage) where no gradient segment may be cut.
    TTC / TCC : every marker whose label contains the text -> marker +/- length/2   (both TTC and both TCC
                of every curve; 50 m -> 25 m each side)
    SRJ       : same, for the SRJ markers (50 m -> SRJ at 423/500 gives 423/475 to 423/525)
    Whole transition (optional): From-text ... next To-text (TTC1 -> TCC1 and TCC2 -> TTC2)
    Also tags each marker with its type ('SRJ' / 'CURVE' / '') for colouring."""
    zones, warns = [], []
    for m in markers:
        m["type"] = ""
    if cfg is None:
        return zones, warns
    if cfg.ttc_on:
        _point_zones(markers, zones, cfg.ttc_kw, cfg.ttc_length, "CURVE")
    if cfg.tcc_on:
        _point_zones(markers, zones, cfg.tcc_kw, cfg.tcc_length, "CURVE")
    if cfg.srj_on:
        _point_zones(markers, zones, cfg.srj_kw, cfg.srj_length, "SRJ")
    if cfg.curve_on:
        for f, t in ((cfg.curve1_from, cfg.curve1_to), (cfg.curve2_from, cfg.curve2_to)):
            kf, kt = _norm(f), _norm(t)
            if not kf or not kt:
                continue
            open_m = None
            for m in markers:
                if m.get("b") is None:
                    continue
                nl = _norm(m["label"])
                if kf in nl:
                    if open_m is not None:
                        warns.append(f"'{open_m['label'].strip()}' ({_where(open_m)}) is followed by another "
                                     f"{f.strip()} instead of {t.strip()} - label typo?")
                    open_m = m
                elif kt in nl:
                    if open_m is None:
                        warns.append(f"'{m['label'].strip()}' ({_where(m)}): no {f.strip()} before it.")
                    else:
                        zones.append(dict(kind="CURVE", lo=open_m["b"], hi=m["b"],
                                          name=f"{open_m['label'].strip()} to {m['label'].strip()}"))
                        open_m["type"] = m["type"] = "CURVE"
                        open_m = None
            if open_m is not None:
                warns.append(f"'{open_m['label'].strip()}' ({_where(open_m)}): no {t.strip()} after it.")
    return zones, warns


def allowed_mask(b, zones):
    """False for points strictly inside a zone (a boundary exactly on the edge is fine: before / after)."""
    ok = np.ones(len(b), bool)
    for z in zones:
        ok &= ~((b > z["lo"] + 1e-9) & (b < z["hi"] - 1e-9))
    ok[0] = ok[-1] = True
    return ok


# =============================================================================
#  3. RESULT  (template layout, table values, graph data)
# =============================================================================
HEADERS = [                                   # updated template: segment block H..T sits on the label row
    "Railway Chainage", "Our company Chainage", "Railway Elevation (every 20m)",
    "Railway Elevation Differents (before - current)", "CALCULATION (new rail level)",
    "Step per 20m", "C - E", "Final Gradient", "Start CH", "End CH", "Length", "Start RL", "End RL",
    "RL diff", "1 in N (calc)", "1 in N (round Value)", "converted to 1m", "converted to 20m",
    "Direction", "Gradient",
]
HEADERS_UI = [
    "Railway\nChainage", "Our company\nChainage", "Railway Elevation\n(every 20m)",
    "Elevation Differents\n(before - current)", "CALCULATION\n(new rail level)", "Step\nper 20m",
    "C - E", "Final\nGradient", "Start CH", "End CH", "Length", "Start RL", "End RL",
    "RL diff", "1 in N\n(calc)", "1 in N\n(round Value)", "converted\nto 1m", "converted\nto 20m",
    "Direction", "Gradient",
]
COL_W = [95, 100, 118, 138, 118, 80, 70, 85, 80, 80, 70, 85, 85, 75, 75, 100, 90, 90, 80, 75]
NCOLS = len(HEADERS)


class Result:
    def __init__(self, data, gmin, gmax, level_n, zones=None):
        self.rail_txt = data["rail_txt"]
        self.b, self.c = data["b"], data["c"]
        self.gmin, self.gmax, self.level_n = gmin, gmax, level_n
        self.markers = [dict(m) for m in data.get("markers", [])]
        self.zones, zone_warn = build_zones(self.markers, zones)
        self.allowed = allowed_mask(self.b, self.zones)
        self.zone_kind = [""] * len(self.b)              # which zone (if any) each data row lies in, edges included
        for kind in ("CURVE", "SRJ"):                    # SRJ wins where they overlap
            for z in (z for z in self.zones if z["kind"] == kind):
                for i in np.where((self.b >= z["lo"] - 1e-9) & (self.b <= z["hi"] + 1e-9))[0]:
                    self.zone_kind[int(i)] = kind
        self.E, self.segs, eng_warn = design_profile(self.b, self.c, gmin, gmax, level_n, self.allowed)
        self.warnings = list(dict.fromkeys(zone_warn + eng_warn))
        self.G = self.c - self.E
        d = np.diff(self.b)
        if np.any(np.abs(d - STEP) > 1e-9):
            self.warnings.append("Chainage spacing is not always 20 m: E uses the real distance.")
        viol = np.where((self.G < gmin - TOL) | (self.G > gmax + TOL))[0]
        if len(viol):
            self.warnings.append(f"{len(viol)} points are outside MIN/MAX.")
        self._layout()

    # ---- sheet layout (updated template) --------------------------------------
    # Every segment has ONE label row (the whole segment block H..T).  It comes straight after the last
    # data row of the previous segment, i.e. ABOVE any marker rows (SRJ / TTC / TCC ...) that follow.
    def _layout(self):
        self.rows = []          # (kind, index, seg_idx, first_in_seg, excel_row); kind data/label/marker
        self.prow = {}          # point idx -> excel row
        self.lrow = {}          # seg idx -> excel row of its label
        n = len(self.b)
        seg_of = np.zeros(n, int)                       # segment each data row belongs to
        for k, sg in enumerate(self.segs):
            seg_of[sg.s + 1:sg.e + 1] = k
        seg_start = {sg.s: k for k, sg in enumerate(self.segs)}
        by_prev = {}
        for mi, m in enumerate(self.markers):
            by_prev.setdefault(m["prev"], []).append(mi)
        r = 2

        def add_markers(prev):
            nonlocal r
            k = int(seg_of[prev + 1]) if prev + 1 < n else int(seg_of[n - 1])
            for mi in by_prev.get(prev, []):
                self.rows.append(("marker", mi, k, False, r))
                r += 1

        def add_label(k):
            nonlocal r
            self.rows.append(("label", None, k, False, r))
            self.lrow[k] = r
            r += 1

        add_label(0)
        add_markers(-1)
        for j in range(n):
            k = int(seg_of[j])
            self.rows.append(("data", j, k, j > 0 and j == self.segs[k].s + 1, r))
            self.prow[j] = r
            r += 1
            kk = seg_start.get(j)
            if kk is not None and kk > 0:
                add_label(kk)
            add_markers(j)

    def _label_cells(self, k, v):
        sg = self.segs[k]
        K, L = float(self.E[sg.s]), float(self.c[sg.e])
        J = float(self.b[sg.e] - self.b[sg.s])
        v[7] = sg.label
        v[8], v[9], v[10] = float(self.b[sg.s]), float(self.b[sg.e]), J
        v[11], v[12], v[13] = K, L, L - K
        v[14] = J / (L - K) if abs(L - K) >= FLAT_EPS else None
        v[15] = sg.n
        v[16] = (1.0 / sg.n) if sg.n else 0.0
        v[17] = v[16] * STEP
        v[18] = sg.direction
        v[19] = sg.n

    def table_values(self):
        out = []
        for kind, j, k, first, _ in self.rows:
            v = [None] * NCOLS
            if kind == "label":
                self._label_cells(k, v)
            elif kind == "marker":
                v[0], v[1] = self.markers[j]["ra"], self.markers[j]["label"]
            else:
                v[0], v[1], v[2] = self.rail_txt[j], float(self.b[j]), float(self.c[j])
                if j == 0:
                    v[3] = 0.0
                else:
                    sg = self.segs[k]
                    v[3] = float(self.c[j] - self.c[j - 1])
                    v[4], v[5], v[6] = float(self.E[j]), abs(sg.g) * STEP, float(self.G[j])
            out.append(v)
        return out

    def summary(self):
        nr = sum(1 for s in self.segs if s.direction == "RISE")
        nf = sum(1 for s in self.segs if s.direction == "FALL")
        nl = sum(1 for s in self.segs if s.direction == "LEVEL")
        nsrj = sum(1 for z in self.zones if z["kind"] == "SRJ")
        ncur = sum(1 for z in self.zones if z["kind"] == "CURVE")
        zt = f"   |   no-cut zones: SRJ {nsrj}, curve {ncur}" if (nsrj or ncur) else ""
        return (f"{len(self.segs)} segments (rise {nr} / fall {nf} / level {nl})   |   "
                f"max |C-E| = {np.max(np.abs(self.G)):.3f} m   |   "
                f"band {self.gmin:+.3f} ... {self.gmax:+.3f} m{zt}")


# =============================================================================
#  4. EXCEL EXPORT (same sheets/columns as template, live formulas)
# =============================================================================
# (light tint for every row of a segment, stronger tint for label / summary cells)
PALETTE = {
    "RISE": ("#E3F4E6", "#8FD19E"),    # green
    "FALL": ("#FCE4E1", "#F28B82"),    # red
    "LEVEL": ("#E3EDFD", "#8AB4F8"),   # blue
}
# =============================================================================
#  THEME  (Light / System / Dark)
# =============================================================================
PALETTE_DARK = {                      # (row tint, stronger tint) on a dark background
    "RISE": ("#1E3A2B", "#2E7D4F"),
    "FALL": ("#40242A", "#A5413C"),
    "LEVEL": ("#1F3050", "#3566B8"),
}
THEMES = {
    False: dict(                      # ---- light
        window="#F3F3F3", window_text="#1B1B1B", base="#FFFFFF", alt_base="#F7F7F7", text="#1B1B1B",
        button="#EDEDED", button_text="#1B1B1B", tip_base="#FFFFDC", tip_text="#000000",
        highlight="#2F6FDE", highlight_text="#FFFFFF", placeholder="#7A7A7A", link="#0B5CD5",
        disabled="#8A8A8A",
        cell_text="#000000", strong_text="#000000", out_bg="#FFB74D", out_fg="#7A0000",
        hdr_letter_bg="#305496", hdr_letter_fg="#FFFFFF", hdr_bg="#D9E1F2", hdr_fg="#1F2A44",
        hdr_line="#8EA9DB",
        mk_srj="#FFE0B2", mk_curve="#E1D0F2", mk_other="#E8E8E8", mk_fg="#000000",
        z_srj="#F5A623", z_curve="#9B59B6",
        fig_bg="#FFFFFF", fg="#222222", grid="#C8C8C8", spine="#444444", bound="#8C8C8C", zero="#808080",
        c_existing="#1f6feb", c_new="#e8590c", c_dev="#6f42c1", c_limit="#d62728",
        l_rise="#1a7f37", l_fall="#c0392b", l_level="#555555"),
    True: dict(                       # ---- dark
        window="#202124", window_text="#E8EAED", base="#1A1B1E", alt_base="#222327", text="#E8EAED",
        button="#2D2F34", button_text="#E8EAED", tip_base="#2D2F34", tip_text="#E8EAED",
        highlight="#3B6FD4", highlight_text="#FFFFFF", placeholder="#8B9099", link="#6EA8FE",
        disabled="#6B7078",
        cell_text="#E8EAED", strong_text="#FFFFFF", out_bg="#8A4B08", out_fg="#FFE0B2",
        hdr_letter_bg="#2B4A7F", hdr_letter_fg="#FFFFFF", hdr_bg="#2A2E37", hdr_fg="#DDE3EE",
        hdr_line="#4A5568",
        mk_srj="#5C3D14", mk_curve="#41305F", mk_other="#34373D", mk_fg="#FFFFFF",
        z_srj="#FFB74D", z_curve="#B392F0",
        fig_bg="#1A1B1E", fg="#E8EAED", grid="#3A3D43", spine="#6B7078", bound="#5A5E66", zero="#8B9099",
        c_existing="#58A6FF", c_new="#FF9F5A", c_dev="#B392F0", c_limit="#FF6B6B",
        l_rise="#56D364", l_fall="#FF7B72", l_level="#9AA7B8"),
}
THEME_MODES = ("Light", "System", "Dark")
_STARTUP_DARK = None


def make_palette(dark):
    t = THEMES[dark]
    P = QtGui.QPalette
    pal = QtGui.QPalette()
    col = QtGui.QColor
    roles = {
        P.ColorRole.Window: t["window"], P.ColorRole.WindowText: t["window_text"],
        P.ColorRole.Base: t["base"], P.ColorRole.AlternateBase: t["alt_base"],
        P.ColorRole.Text: t["text"], P.ColorRole.Button: t["button"],
        P.ColorRole.ButtonText: t["button_text"], P.ColorRole.ToolTipBase: t["tip_base"],
        P.ColorRole.ToolTipText: t["tip_text"], P.ColorRole.Highlight: t["highlight"],
        P.ColorRole.HighlightedText: t["highlight_text"], P.ColorRole.PlaceholderText: t["placeholder"],
        P.ColorRole.Link: t["link"], P.ColorRole.BrightText: "#FF5555",
    }
    for role, value in roles.items():
        pal.setColor(role, col(value))
    btn = col(t["button"])
    pal.setColor(P.ColorRole.Light, btn.lighter(150 if dark else 120))
    pal.setColor(P.ColorRole.Midlight, btn.lighter(125 if dark else 110))
    pal.setColor(P.ColorRole.Mid, btn.darker(125 if dark else 115))
    pal.setColor(P.ColorRole.Dark, btn.darker(170 if dark else 150))
    pal.setColor(P.ColorRole.Shadow, col("#000000"))
    for role in (P.ColorRole.WindowText, P.ColorRole.Text, P.ColorRole.ButtonText):
        pal.setColor(P.ColorGroup.Disabled, role, col(t["disabled"]))
    return pal


def system_is_dark():
    """True when the operating system is in dark mode (Qt 6.5+), else the start-up palette decides."""
    app = QtWidgets.QApplication.instance()
    try:
        scheme = app.styleHints().colorScheme()
        if scheme == QtCore.Qt.ColorScheme.Dark:
            return True
        if scheme == QtCore.Qt.ColorScheme.Light:
            return False
    except AttributeError:
        pass
    return bool(_STARTUP_DARK)


def legend_html(dark):
    pal, t = (PALETTE_DARK if dark else PALETTE), THEMES[dark]
    spans = "&nbsp;".join(f'<span style="background:{pal[n][1]}; color:{t["strong_text"]};">&nbsp;{n}&nbsp;</span>'
                          for n in ("RISE", "FALL", "LEVEL"))
    chips = [(t["mk_srj"], t["mk_fg"], "SRJ"), (t["mk_curve"], t["mk_fg"], "curve"),
             (t["out_bg"], t["out_fg"], "outside MIN/MAX")]
    return spans + "".join(f'&nbsp;<span style="background:{bg}; color:{fg};">&nbsp;{n}&nbsp;</span>'
                           for bg, fg, n in chips)


def export_excel(res, path):
    """Two sheets in the updated template (Calibri 10, dark header, merged marker rows, direction colours).
    Every calculated cell is a live formula; only the survey data and the settings are typed values."""
    wb = openpyxl.Workbook()
    ws = wb.active
    ws.title = SHEET_MAIN
    ab = wb.create_sheet(SHEET_ABS, 0)

    def font(bold=False, color=None):
        return Font(name="Calibri", size=10, bold=bold, color=color)

    BLACK, BOLD, WHITE_B, BLUE = font(), font(True), font(True, "FFFFFF"), font(color="0000FF")
    thin = Side(style="thin")
    BOX = Border(left=thin, right=thin, top=thin, bottom=thin)
    DARK = PatternFill("solid", start_color="404040", end_color="404040")
    RIGHT = Alignment(horizontal="right")
    CENTER = Alignment(horizontal="center")
    HDR_ALIGN = Alignment(horizontal="center", vertical="center", wrap_text=True)

    def solid(hex_color):
        return PatternFill("solid", start_color=hex_color[1:], end_color=hex_color[1:])

    fills = {d: (solid(lt), solid(st)) for d, (lt, st) in PALETTE.items()}       # (row tint, label tint)
    mk_fill = {t: solid(THEMES[False][key]) for t, key in (("SRJ", "mk_srj"), ("CURVE", "mk_curve"), ("", "mk_other"))}
    for sheet in (ws, ab):
        sheet.sheet_view.zoomScale = 85
        sheet.sheet_view.showGridLines = False

    # ---- header ------------------------------------------------------------------------------------
    for ci, h in enumerate(HEADERS, 1):
        c = ws.cell(1, ci, h)
        c.font, c.fill, c.alignment, c.border = WHITE_B, DARK, HDR_ALIGN, BOX
    ws.row_dimensions[1].height = 45
    ws.freeze_panes = "A2"
    widths = [12.6, 13.4, 15.7, 18.4, 15.7, 10.6, 10, 10, 12, 10.6, 10, 12, 12.6, 10.5, 10.9, 11, 9.9, 11, 9, 10]
    for ci, wdt in enumerate(widths, 1):
        ws.column_dimensions[get_column_letter(ci)].width = wdt

    # ---- settings used by the formulas (U:V); edit here and the whole sheet follows ----------------------
    ws.column_dimensions["U"].width = 26
    ws.column_dimensions["V"].width = 11
    ws.cell(1, 21, "Settings").font = BOLD
    for rr, (name, val) in ((2, ("G MIN (m)", res.gmin)), (3, ("G MAX (m)", res.gmax)),
                            (4, ("LEVEL if 1 in N >= (0 = off)", res.level_n))):
        ws.cell(rr, 21, name).font = BLACK
        ws.cell(rr, 22, val).font = BLUE
    LEVEL_LIMIT, GMIN_CELL, GMAX_CELL = "$V$4", "$V$2", "$V$3"

    FMT = {3: "0.000", 4: "0.000", 5: "0.000", 6: "0.00000", 7: "0.000", 12: "0.000", 13: "0.000",
           14: "0.0000", 15: "0.00", 16: "0", 17: "0.0000000", 18: "0.00000", 20: "0"}

    def put(r, col, val, fnt=BLACK, align=RIGHT):
        c = ws.cell(r, col, val)
        c.font, c.alignment = fnt, align
        if col in FMT:
            c.number_format = FMT[col]
        return c

    b, c_, prow, lrow = res.b, res.c, res.prow, res.lrow
    for kind, j, k, first, r in res.rows:
        # ---- marker rows (SRJ / TTC / TCC / yard limit ...): text merged over B:G ---------------------
        if kind == "marker":
            m = res.markers[j]
            fl = mk_fill[m["type"]]
            a = ws.cell(r, 1, m["ra"] if m["ra"] else None)
            a.font, a.alignment, a.fill = BLACK, RIGHT, fl
            t = ws.cell(r, 2, m["label"])
            t.font, t.alignment, t.fill = BOLD, CENTER, fl
            ws.merge_cells(start_row=r, start_column=2, end_row=r, end_column=7)
            continue
        sg = res.segs[k]
        light, strong = fills[sg.direction]
        # ---- label row: the whole segment block, directly above the segment's first data row -----------
        if kind == "label":
            br, er = prow[sg.s], prow[sg.e]
            for col in range(1, 21):
                ws.cell(r, col).fill = strong
                ws.cell(r, col).alignment = RIGHT
                ws.cell(r, col).font = BLACK
            put(r, 8, f'=IF(S{r}="LEVEL","LEVEL",IF(S{r}="RISE","R ","F ")&T{r})', BOLD)
            put(r, 9, f"=B{br}")
            put(r, 10, f"=B{er}")
            put(r, 11, f"=J{r}-I{r}")
            put(r, 12, f"=C{br}" if k == 0 else f"=E{br}")
            put(r, 13, f"=C{er}")
            put(r, 14, f"=M{r}-L{r}")
            put(r, 15, f'=IF(ABS(N{r})<0.0001,"",K{r}/N{r})')
            put(r, 16, f'=IF(O{r}="",0,MAX(1,ROUND(ABS(O{r}),0)))')
            put(r, 17, f"=IF(P{r}=0,0,1/P{r})")
            put(r, 18, f"=Q{r}*20")
            put(r, 19, f'=IF(O{r}="","LEVEL",IF(AND({LEVEL_LIMIT}>0,P{r}>={LEVEL_LIMIT}),"LEVEL",'
                       f'IF(O{r}>0,"RISE","FALL")))')
            put(r, 20, f"=P{r}")
            continue
        # ---- data row: A..G only, tinted with the segment's direction colour ---------------------------
        zone_fill = mk_fill[res.zone_kind[j]] if res.zone_kind[j] else None
        for col in range(1, 8):
            ws.cell(r, col).fill = zone_fill if (zone_fill and col <= 2) else light
        if res.rail_txt[j] is not None:
            put(r, 1, res.rail_txt[j])
        bv = float(b[j])
        put(r, 2, int(bv) if bv.is_integer() else bv)
        put(r, 3, float(c_[j]))
        if j == 0:
            put(r, 4, 0)
            continue
        pr = prow[j - 1]
        put(r, 4, f"=C{r}-C{pr}")
        sign = "-" if sg.g < 0 else "+"
        d = float(b[j] - b[j - 1])
        scaled = abs(d - STEP) > 1e-9
        base = f"C{pr}" if (k == 0 and first) else f"E{pr}"
        put(r, 6, f"=R{lrow[k]}" if first else f"=F{pr}")
        if first or scaled:
            f = f"={base}{sign}F{r}" + (f"*(B{r}-B{pr})/20" if scaled else "")
        else:
            f = f"=E{pr}{sign}F{pr}"
        put(r, 5, f)
        put(r, 7, f"=C{r}-E{r}")

    last = res.rows[-1][4]
    ws.conditional_formatting.add(
        f"G2:G{last}",
        FormulaRule(formula=[f"AND(ISNUMBER(G2),OR(G2<{GMIN_CELL}-0.000001,G2>{GMAX_CELL}+0.000001))"],
                    fill=PatternFill("solid", start_color="FFB74D", end_color="FFB74D"),
                    font=Font(name="Calibri", size=10, color="7A0000", bold=True)))

    # ---- ABSTRACT ------------------------------------------------------------------------------------
    for ci, (h, wdt) in enumerate((("STARTING CH", 16), ("RL", 10), ("ENDING CH", 16), ("RL", 10),
                                   ("GRADIENT", 14)), 1):
        cell = ab.cell(1, ci, h)
        cell.font, cell.fill, cell.alignment, cell.border = WHITE_B, DARK, HDR_ALIGN, BOX
        ab.column_dimensions[get_column_letter(ci)].width = wdt
    ab.row_dimensions[1].height = 22
    ab.freeze_panes = "A2"
    q = f"'{SHEET_MAIN}'!"
    for k, sg in enumerate(res.segs):
        r = k + 2
        br, er = prow[sg.s], prow[sg.e]
        values = (f'=IF({q}A{br}="",{q}B{br},{q}A{br})',
                  f"={q}C{br}" if k == 0 else f"={q}E{br}",
                  f'=IF({q}A{er}="",{q}B{er},{q}A{er})',
                  f"={q}E{er}",
                  f"={q}H{lrow[k]}")
        for ci, val in enumerate(values, 1):
            cell = ab.cell(r, ci, val)
            cell.font, cell.alignment, cell.border = BLACK, CENTER, BOX
            if ci in (2, 4):
                cell.number_format = "0.000"
        ab.cell(r, 5).fill = fills[sg.direction][1]          # gradient cell: green / red / blue
    wb.save(path)


ACAD_HEADER = ["vid_ch", "rw_ch", "railLevel", "lineType", "before", "current", "note"]


def export_autocad_csv(res, path, line_type="(DN LINE)", note="FOR EXIST - ALL LINES"):
    """CSV for AutoCAD: one row per gradient change point (line start, every cut, line end).
        vid_ch = company chainage, rw_ch = 'KM: km/m', railLevel = 'RL: new rail level' (column E),
        before / current = gradient before and after the point ('F 1 IN 357', 'R 1 IN 540', 'LEVEL')."""
    def trim(v, nd=3):
        return f"{v:.{nd}f}".rstrip("0").rstrip(".")

    def grad(sg):
        return "LEVEL" if sg.level else f"{'R' if sg.g > 0 else 'F'} 1 IN {sg.n}"

    def km(i):
        t = res.rail_txt[i]
        if t:
            return t
        bi = float(res.b[i])                                   # no railway chainage in the data: km/m of company ch
        return f"{int(bi // 1000)}/{bi % 1000:07.3f}"

    pts = [res.segs[0].s] + [sg.e for sg in res.segs]
    with open(path, "w", encoding="utf-8", newline="") as f:
        wr = csv.writer(f, lineterminator="\r\n")
        wr.writerow(ACAD_HEADER)
        for t, i in enumerate(pts):
            bi = float(res.b[i])
            wr.writerow([int(bi) if bi.is_integer() else trim(bi), f"KM: {km(i)}", f"RL: {trim(float(res.E[i]))}",
                         line_type, grad(res.segs[t - 1]) if t > 0 else "",
                         grad(res.segs[t]) if t < len(res.segs) else "", note])


# =============================================================================
#  5. TABLE (tab 1)
# =============================================================================
DEC = {2: 3, 3: 3, 4: 3, 5: 5, 6: 3, 11: 3, 12: 3, 13: 4, 14: 2, 16: 7, 17: 5}
INTLIKE = (1, 8, 9, 10)          # chainages / length: as typed, no trailing zeros
WHOLE = (15, 19)                 # 1 in N (round Value), Gradient


def fmt_cell(col, v, kind=None):
    if v is None:
        return ""
    if isinstance(v, str):
        return v
    if col in INTLIKE:
        return f"{v:.3f}".rstrip("0").rstrip(".")
    if col in WHOLE:
        return f"{v:.0f}"
    return f"{v:.{DEC.get(col, 3)}f}"


class TwoRowHeader(QtWidgets.QHeaderView):
    """Column header in two bands: column name (A, B, C ...) on top, heading below."""
    LETTER_H = 22
    HEADING_H = 44

    def __init__(self, parent=None):
        super().__init__(QtCore.Qt.Orientation.Horizontal, parent)
        self.setSectionsClickable(False)
        self.setHighlightSections(False)
        self.setFixedHeight(self.LETTER_H + self.HEADING_H)
        self.th = THEMES[False]

    def set_dark(self, dark):
        self.th = THEMES[dark]
        self.viewport().update()

    def paintSection(self, painter, rect, logical_index):
        if not rect.isValid():
            return
        painter.save()
        top = QtCore.QRect(rect.left(), rect.top(), rect.width(), self.LETTER_H)
        bot = QtCore.QRect(rect.left(), rect.top() + self.LETTER_H, rect.width(), rect.height() - self.LETTER_H)
        painter.fillRect(top, QtGui.QColor(self.th["hdr_letter_bg"]))
        painter.fillRect(bot, QtGui.QColor(self.th["hdr_bg"]))
        font = QtGui.QFont(self.font())
        font.setBold(True)
        painter.setFont(font)
        painter.setPen(QtGui.QColor(self.th["hdr_letter_fg"]))
        painter.drawText(top, int(QtCore.Qt.AlignmentFlag.AlignCenter), get_column_letter(logical_index + 1))
        font.setBold(False)
        painter.setFont(font)
        painter.setPen(QtGui.QColor(self.th["hdr_fg"]))
        heading = self.model().headerData(logical_index, QtCore.Qt.Orientation.Horizontal) or ""
        painter.drawText(bot.adjusted(3, 0, -3, 0),
                         int(QtCore.Qt.AlignmentFlag.AlignCenter | QtCore.Qt.TextFlag.TextWordWrap), heading)
        painter.setPen(QtGui.QColor(self.th["hdr_line"]))
        painter.drawRect(rect.adjusted(0, 0, -1, -1))
        painter.drawLine(rect.left(), rect.top() + self.LETTER_H, rect.right(), rect.top() + self.LETTER_H)
        painter.restore()


class TableModel(QtCore.QAbstractTableModel):
    def __init__(self):
        super().__init__()
        self.res, self.vals = None, []
        self._search = None
        self.dark = False
        self._bold = None

    def set_dark(self, dark):
        self.dark = dark
        if self.vals:
            self.dataChanged.emit(self.index(0, 0), self.index(len(self.vals) - 1, NCOLS - 1))

    def set_result(self, res):
        self.beginResetModel()
        self._search = None
        self.res = res
        self.vals = res.table_values() if res else []
        self.endResetModel()

    def rowCount(self, parent=QtCore.QModelIndex()):
        return 0 if parent.isValid() else len(self.vals)

    def columnCount(self, parent=QtCore.QModelIndex()):
        return 0 if parent.isValid() else NCOLS

    def headerData(self, section, orientation, role=QtCore.Qt.ItemDataRole.DisplayRole):
        if role != QtCore.Qt.ItemDataRole.DisplayRole:
            return None
        if orientation == QtCore.Qt.Orientation.Horizontal:
            return HEADERS_UI[section]          # heading only; the letter is drawn by TwoRowHeader
        return str(self.res.rows[section][4]) if self.res else None

    def data(self, idx, role=QtCore.Qt.ItemDataRole.DisplayRole):
        """Same look as the Excel export: label rows A..T in the direction colour (H bold), data rows
        A..G in the light tint, marker rows A + merged B..G (text bold, centred)."""
        if not idx.isValid():
            return None
        kind, j, k, first, _ = self.res.rows[idx.row()]
        col, v = idx.column(), self.vals[idx.row()][idx.column()]
        R = QtCore.Qt.ItemDataRole
        A = QtCore.Qt.AlignmentFlag
        th = THEMES[self.dark]
        if role == R.DisplayRole:
            return fmt_cell(col, v)
        if role == R.TextAlignmentRole:
            if kind == "marker" and col == 1:
                return int(A.AlignHCenter | A.AlignVCenter)
            return int(A.AlignRight | A.AlignVCenter)
        if role == R.FontRole:
            if (kind == "label" and col == 7) or (kind == "marker" and col == 1):
                if self._bold is None:
                    self._bold = QtGui.QFont(QtWidgets.QApplication.font())
                    self._bold.setBold(True)
                return self._bold
            return None
        if role not in (R.BackgroundRole, R.ForegroundRole):
            return None
        pal = PALETTE_DARK if self.dark else PALETTE
        if kind == "marker":
            if col > 6:
                return None
            if role == R.ForegroundRole:
                return QtGui.QColor(th["mk_fg"])
            key = {"SRJ": "mk_srj", "CURVE": "mk_curve"}.get(self.res.markers[j]["type"], "mk_other")
            return QtGui.QColor(th[key])
        light, strong = pal[self.res.segs[k].direction]
        if kind == "label":
            return QtGui.QColor(strong if role == R.BackgroundRole else th["strong_text"])
        if col > 6:                                                       # data rows: only A..G are coloured
            return None
        zone = self.res.zone_kind[j]
        if role == R.BackgroundRole:
            if col == 6 and v is not None and (v < self.res.gmin - TOL or v > self.res.gmax + TOL):
                return QtGui.QColor(th["out_bg"])                        # orange: outside MIN/MAX
            if zone and col <= 1:                                        # SRJ / curve zone: railway + company chainage
                return QtGui.QColor(th["mk_srj"] if zone == "SRJ" else th["mk_curve"])
            return QtGui.QColor(light)
        if col == 6 and v is not None and (v < self.res.gmin - TOL or v > self.res.gmax + TOL):
            return QtGui.QColor(th["out_fg"])
        return QtGui.QColor(th["mk_fg"] if (zone and col <= 1) else th["cell_text"])

    def search_strings(self):
        """Lower-case text of every row (as displayed), built once per result."""
        if self._search is None:
            self._search = [
                " | ".join(str(self.data(self.index(i, c)) or "") for c in range(NCOLS)).lower()
                for i in range(len(self.vals))
            ]
        return self._search


# =============================================================================
#  6. GRAPH (tab 2)
# =============================================================================
def fmt_km(v, _=None):
    km = int(v // 1000)
    m = int(round(v - km * 1000))
    if m >= 1000:
        km, m = km + 1, 0
    return f"{km}/{m:03d}"


class CleanToolbar(NavigationToolbar2QT):
    """Matplotlib toolbar without the 'Configure subplots' button."""
    toolitems = [t for t in NavigationToolbar2QT.toolitems if t[0] != "Subplots"]


class GraphTab(QtWidgets.QWidget):
    def __init__(self):
        super().__init__()
        self.fig = Figure(figsize=(12, 6))
        self.canvas = FigureCanvasQTAgg(self.fig)
        self.toolbar = CleanToolbar(self.canvas, self)
        lay = QtWidgets.QVBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        lay.addWidget(self.toolbar)
        lay.addWidget(self.canvas)
        self.res = None
        self.sec = None
        self.texts = []
        self._busy = False
        self.dark = False
        self.canvas.mpl_connect("scroll_event", self._on_scroll)

    # ---------------------------------------------------------------------
    def _refresh_toolbar_icons(self):
        """matplotlib colours its toolbar icons when the toolbar is created: redo that after a theme change."""
        tb = self.toolbar
        actions = getattr(tb, "_actions", {})
        for text, _tip, image_file, callback in tb.toolitems:
            act = actions.get(callback) if text else None
            if act is not None:
                try:
                    act.setIcon(tb._icon(image_file + ".png"))
                except Exception:                                # noqa: BLE001
                    pass

    def set_dark(self, dark):
        self.dark = dark
        self._refresh_toolbar_icons()
        if self.res is not None:
            xlim = self.ax.get_xlim()                           # keep the current zoom
            self.plot(self.res)
            self.ax.set_xlim(xlim)
        else:
            self.fig.patch.set_facecolor(THEMES[dark]["fig_bg"])
        self.canvas.draw_idle()

    def plot(self, res):
        self.res = res
        fig = self.fig
        th = THEMES[self.dark]
        fig.clear()
        fig.patch.set_facecolor(th["fig_bg"])
        self.texts = []
        gs = fig.add_gridspec(2, 1, height_ratios=[4, 1], hspace=0.07,
                              left=0.06, right=0.99, top=0.79, bottom=0.10)
        ax = fig.add_subplot(gs[0])
        ax2 = fig.add_subplot(gs[1], sharex=ax)
        self.ax, self.ax2 = ax, ax2
        b, c, e = res.b, res.c, res.E

        l1, = ax.plot(b, c, color=th["c_existing"], lw=1.1, label="Existing rail level (col C)")
        l2, = ax.plot(b, e, color=th["c_new"], lw=1.1, label="New rail level (col E)")
        handles = [l1, l2]
        if any(z["kind"] == "SRJ" for z in res.zones):
            handles.append(Patch(facecolor=th["z_srj"], alpha=0.45, label="SRJ zone (no cut)"))
        if any(z["kind"] == "CURVE" for z in res.zones):
            handles.append(Patch(facecolor=th["z_curve"], alpha=0.45, label="Curve zone (no cut)"))
        leg = fig.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.06, 0.995),
                         ncol=len(handles), fontsize=8, frameon=False)
        for t in leg.get_texts():
            t.set_color(th["fg"])
        ax.set_ylabel("Rail level RL (m)", color=th["fg"])
        ax.grid(True, color=th["grid"], alpha=0.6, linewidth=0.6)
        ax.tick_params(labelbottom=False)

        bounds = [b[sg.s] for sg in res.segs] + [b[-1]]
        for a in (ax, ax2):
            a.vlines(bounds, 0, 1, transform=a.get_xaxis_transform(),
                     colors=th["bound"], linestyles=":", linewidth=0.6)

        for z in res.zones:                              # protected zones: no gradient cut inside
            zc = th["z_srj"] if z["kind"] == "SRJ" else th["z_curve"]
            for a in (ax, ax2):
                a.axvspan(z["lo"], z["hi"], facecolor=zc, alpha=0.20, linewidth=0, zorder=0)

        # deviation panel
        ax2.plot(b, res.G, color=th["c_dev"], lw=0.8)
        ax2.axhline(res.gmin, color=th["c_limit"], ls="--", lw=0.9)
        ax2.axhline(res.gmax, color=th["c_limit"], ls="--", lw=0.9)
        ax2.axhline(0, color=th["zero"], lw=0.5)
        pad = 0.15 * (res.gmax - res.gmin)
        ax2.set_ylim(min(res.gmin, res.G.min()) - pad, max(res.gmax, res.G.max()) + pad)
        ax2.set_ylabel("C - E (m)", color=th["fg"])
        ax2.grid(True, color=th["grid"], alpha=0.6, linewidth=0.6)
        ax2.set_xlabel("Our company chainage (m)", color=th["fg"])
        ax2.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:.0f}"))

        for a in (ax, ax2):
            a.set_facecolor(th["fig_bg"])
            a.tick_params(colors=th["fg"])
            for sp in a.spines.values():
                sp.set_edgecolor(th["spine"])

        # railway chainage on a second x axis (top): real railway chainage text
        # (km posts are not exactly 1000 m, so it is shown as given, not converted)
        self.sec = None
        lab = [(float(bb), compact_rail(t)) for bb, t in zip(b, res.rail_txt)]
        lab = [(x, t) for x, t in lab if t]
        if len(lab) >= 2:
            self.rail_pos = np.array([x for x, _ in lab])
            self.rail_lbl = [t for _, t in lab]
            self.sec = ax.secondary_xaxis("top", functions=(lambda x: x, lambda x: x))
            self.sec.set_xlabel("Railway chainage (km/m)", color=th["fg"])
            self.sec.tick_params(colors=th["fg"])
            for sp in self.sec.spines.values():
                sp.set_edgecolor(th["spine"])

        # gradient labels (one per segment)
        trans = offset_copy(ax.transData, fig=fig, x=0, y=5, units="points")
        for sg in res.segs:
            col = th["l_rise"] if sg.direction == "RISE" else (th["l_fall"] if sg.direction == "FALL" else th["l_level"])
            t = ax.text(0, 0, sg.label, rotation=90, ha="center", va="bottom", fontsize=7,
                        color=col, transform=trans, clip_on=True)
            self.texts.append(t)

        ax.set_xlim(b[0], b[-1])
        ax.callbacks.connect("xlim_changed", self._update_view)
        self._update_view()
        self.canvas.draw_idle()

    # ---------------------------------------------------------------------
    def _update_view(self, _=None):
        if self._busy or self.res is None:
            return
        self._busy = True
        try:
            res, ax = self.res, self.ax
            b, c, e = res.b, res.c, res.E
            top = np.maximum(c, e)
            x0, x1 = ax.get_xlim()
            i0 = max(int(np.searchsorted(b, x0, "left")) - 1, 0)
            i1 = min(int(np.searchsorted(b, x1, "right")) + 1, len(b))
            if i1 - i0 < 2:
                i0, i1 = max(i0 - 1, 0), min(i1 + 1, len(b))
            lo = float(min(c[i0:i1].min(), e[i0:i1].min()))
            hi = float(max(c[i0:i1].max(), e[i0:i1].max()))
            rng = max(hi - lo, 0.5)
            ax.set_ylim(lo - 0.06 * rng, hi + 0.30 * rng)
            px_per_m = ax.bbox.width / max(x1 - x0, 1e-9)
            if self.sec is not None:
                vis = np.where((self.rail_pos >= x0) & (self.rail_pos <= x1))[0]
                step = max(1, int(math.ceil(len(vis) / max(2.0, ax.bbox.width / 34.0))))
                sel = [g for g in vis if g % step == 0]
                self.sec.set_xticks([float(self.rail_pos[g]) for g in sel])
                self.sec.set_xticklabels([self.rail_lbl[g] for g in sel], fontsize=8,
                                         rotation=90, ha="center", va="bottom",
                                         color=THEMES[self.dark]["fg"])
            for sg, t in zip(res.segs, self.texts):
                xa, xb = max(b[sg.s], x0), min(b[sg.e], x1)
                if xb <= xa or (xb - xa) * px_per_m < 6:
                    t.set_visible(False)
                    continue
                xm = (xa + xb) / 2.0
                t.set_position((xm, float(np.interp(xm, b, top))))
                t.set_visible(True)
        finally:
            self._busy = False

    def _on_scroll(self, ev):
        if self.res is None or ev.inaxes not in (self.ax, self.ax2) or ev.xdata is None:
            return
        x0, x1 = self.ax.get_xlim()
        f = 0.8 if ev.button == "up" else 1.25
        nx0, nx1 = ev.xdata - (ev.xdata - x0) * f, ev.xdata + (x1 - ev.xdata) * f
        full = self.res.b[-1] - self.res.b[0]
        if nx1 - nx0 > full * 1.05:
            nx0, nx1 = self.res.b[0], self.res.b[-1]
        self.ax.set_xlim(nx0, nx1)
        self.canvas.draw_idle()


# =============================================================================
#  7. MAIN WINDOW
# =============================================================================
class MainWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Railway Gradient Designer")
        self.resize(1450, 880)
        self.data, self.res, self.path = None, None, None
        global _STARTUP_DARK
        app = QtWidgets.QApplication.instance()
        if _STARTUP_DARK is None:                     # remember how the OS looked before we style anything
            _STARTUP_DARK = app.palette().color(QtGui.QPalette.ColorRole.Window).lightness() < 128
        app.setStyle("Fusion")                        # same look on every Windows version; palettes below
        self.settings = QtCore.QSettings("VID", "RailwayGradientDesigner")

        bar = QtWidgets.QGroupBox("Settings")
        h = QtWidgets.QHBoxLayout(bar)
        self.btn_open = QtWidgets.QPushButton("Open CSV…")
        self.lbl_file = QtWidgets.QLabel("No file loaded")
        self.lbl_file.setMinimumWidth(220)
        self.sp_min = QtWidgets.QDoubleSpinBox()
        self.sp_min.setRange(-5.0, 0.0)
        self.sp_min.setDecimals(3)
        self.sp_min.setSingleStep(0.01)
        self.sp_min.setValue(-0.25)
        self.sp_min.setSuffix(" m")
        self.sp_max = QtWidgets.QDoubleSpinBox()
        self.sp_max.setRange(0.0, 5.0)
        self.sp_max.setDecimals(3)
        self.sp_max.setSingleStep(0.01)
        self.sp_max.setValue(0.25)
        self.sp_max.setSuffix(" m")
        for sp in (self.sp_min, self.sp_max):
            sp.setToolTip("Limit for column G = C - E (existing - new). MIN <= 0 <= MAX.")
        self.sp_level = QtWidgets.QSpinBox()
        self.sp_level.setRange(0, 10_000_000)
        self.sp_level.setValue(10000)
        self.sp_level.setSpecialValueText("off")
        self.sp_level.setToolTip("Segments whose O (1 in N used) is this flat or flatter are labelled LEVEL (0 = off).")
        self.cb_theme = QtWidgets.QComboBox()
        self.cb_theme.addItems(THEME_MODES)
        self.cb_theme.setToolTip("Light, System (follows Windows) or Dark")
        self.btn_calc = QtWidgets.QPushButton("Calculate")
        self.btn_export = QtWidgets.QPushButton("Export Excel…")
        self.btn_calc.setEnabled(False)
        self.btn_export.setEnabled(False)
        h.addWidget(self.btn_open)
        h.addWidget(self.lbl_file, 1)
        for text, w in (("G MIN", self.sp_min), ("G MAX", self.sp_max),
                        ("LEVEL if flatter than 1 in", self.sp_level), ("Theme", self.cb_theme)):
            h.addWidget(QtWidgets.QLabel(text))
            h.addWidget(w)
        h.addWidget(self.btn_calc)
        h.addWidget(self.btn_export)

        zbar = QtWidgets.QGroupBox("No new gradient segment inside   (a cut is made before or after the zone instead)")
        zv = QtWidgets.QVBoxLayout(zbar)
        zh, zh2 = QtWidgets.QHBoxLayout(), QtWidgets.QHBoxLayout()
        zv.addLayout(zh)
        zv.addLayout(zh2)

        def zone_row(title, kw, tip):
            ck = QtWidgets.QCheckBox(title)
            ck.setChecked(True)
            ed = QtWidgets.QLineEdit(kw)
            ed.setFixedWidth(60)
            ed.setToolTip("Marker rows whose label contains this text (spaces and hyphens ignored).")
            sp = QtWidgets.QDoubleSpinBox()
            sp.setRange(0.0, 1000.0)
            sp.setDecimals(1)
            sp.setValue(50.0)
            sp.setSuffix(" m zone")
            sp.setToolTip(tip)
            ck.toggled.connect(lambda on, e=ed, s_=sp: (e.setEnabled(on), s_.setEnabled(on)))
            for wdg in (ck, ed, sp):
                zh.addWidget(wdg)
            zh.addSpacing(24)
            return ck, ed, sp

        tip = ("Total length of the zone, centred on the marker: 50 m = 25 m before + 25 m after "
               "(marker at 423/500 -> zone 423/475 to 423/525). Cuts exactly on the edge are allowed.")
        self.ck_srj, self.ed_srj, self.sp_srj = zone_row("SRJ", "SRJ", tip)
        self.ck_ttc, self.ed_ttc, self.sp_ttc = zone_row("TTC", "TTC", tip + " Applies to every TTC of every curve (TTC1 and TTC2).")
        self.ck_tcc, self.ed_tcc, self.sp_tcc = zone_row("TCC", "TCC", tip + " Applies to every TCC of every curve (TCC1 and TCC2).")
        zh.addStretch(1)

        self.ck_curve = QtWidgets.QCheckBox("Also the whole transition:")
        self.ck_curve.setChecked(False)
        self.ed_c1f, self.ed_c1t = QtWidgets.QLineEdit("TTC1"), QtWidgets.QLineEdit("TCC1")
        self.ed_c2f, self.ed_c2t = QtWidgets.QLineEdit("TCC2"), QtWidgets.QLineEdit("TTC2")
        zh2.addWidget(self.ck_curve)
        for text, ed in (("From", self.ed_c1f), ("To", self.ed_c1t), ("   and From", self.ed_c2f), ("To", self.ed_c2t)):
            ed.setFixedWidth(60)
            ed.setEnabled(False)
            ed.setToolTip("No cut anywhere between these two markers (spaces and hyphens ignored).")
            zh2.addWidget(QtWidgets.QLabel(text))
            zh2.addWidget(ed)
        zh2.addStretch(1)
        self.ck_curve.toggled.connect(lambda on: [e.setEnabled(on) for e in
                                                  (self.ed_c1f, self.ed_c1t, self.ed_c2f, self.ed_c2t)])

        abar = QtWidgets.QGroupBox("AutoCAD CSV")
        ah = QtWidgets.QHBoxLayout(abar)
        self.ed_line = QtWidgets.QLineEdit("(DN LINE)")
        self.ed_line.setFixedWidth(110)
        self.ed_note = QtWidgets.QLineEdit("FOR EXIST - ALL LINES")
        self.ed_note.setMinimumWidth(240)
        self.btn_acad = QtWidgets.QPushButton("Export AutoCAD CSV…")
        self.btn_acad.setEnabled(False)
        self.btn_acad.setToolTip("vid_ch, rw_ch, railLevel, lineType, before, current, note - one row per gradient change point")
        ah.addWidget(QtWidgets.QLabel("lineType"))
        ah.addWidget(self.ed_line)
        ah.addWidget(QtWidgets.QLabel("note"))
        ah.addWidget(self.ed_note)
        ah.addWidget(self.btn_acad)
        ah.addStretch(1)

        self.model = TableModel()
        self.table = QtWidgets.QTableView()
        self.table.setHorizontalHeader(TwoRowHeader(self.table))
        self.table.setModel(self.model)
        self.table.verticalHeader().setDefaultSectionSize(22)
        for i, w in enumerate(COL_W):
            self.table.setColumnWidth(i, w)
        self.graph = GraphTab()

        self.tabs = QtWidgets.QTabWidget()
        table_tab = QtWidgets.QWidget()
        tv = QtWidgets.QVBoxLayout(table_tab)
        tv.setContentsMargins(0, 4, 0, 0)
        sb = QtWidgets.QHBoxLayout()
        self.search = QtWidgets.QLineEdit()
        self.search.setClearButtonEnabled(True)
        self.search.setPlaceholderText("Search table (chainage, RL, R 590, FALL, LEVEL …)   Enter = next, F3 / Shift+F3")
        self.btn_prev = QtWidgets.QToolButton()
        self.btn_prev.setText("▲")
        self.btn_prev.setToolTip("Previous match (Shift+F3)")
        self.btn_next = QtWidgets.QToolButton()
        self.btn_next.setText("▼")
        self.btn_next.setToolTip("Next match (F3 / Enter)")
        self.lbl_hits = QtWidgets.QLabel("")
        self.lbl_hits.setMinimumWidth(80)
        legend = QtWidgets.QLabel()
        legend.setTextFormat(QtCore.Qt.TextFormat.RichText)
        self.legend = legend
        sb.addWidget(self.search, 1)
        sb.addWidget(self.btn_prev)
        sb.addWidget(self.btn_next)
        sb.addWidget(self.lbl_hits)
        sb.addSpacing(16)
        sb.addWidget(legend)
        tv.addLayout(sb)
        tv.addWidget(self.table, 1)
        self.hits, self.hit_i = [], -1
        self.search.textChanged.connect(self.on_search_changed)
        self.search.returnPressed.connect(lambda: self.goto_hit(+1))
        self.btn_next.clicked.connect(lambda: self.goto_hit(+1))
        self.btn_prev.clicked.connect(lambda: self.goto_hit(-1))
        QtGui.QShortcut(QtGui.QKeySequence("F3"), self, activated=lambda: self.goto_hit(+1))
        QtGui.QShortcut(QtGui.QKeySequence("Shift+F3"), self, activated=lambda: self.goto_hit(-1))
        QtGui.QShortcut(QtGui.QKeySequence("Ctrl+F"), self, activated=self.focus_search)
        self.tabs.addTab(table_tab, "Table")
        self.tabs.addTab(self.graph, "Graph")

        self.lbl_sum = QtWidgets.QLabel("Open a CSV with: Railway chainage, Company chainage, Elevation.")
        self.lbl_sum.setWordWrap(True)

        central = QtWidgets.QWidget()
        v = QtWidgets.QVBoxLayout(central)
        v.addWidget(bar)
        v.addWidget(zbar)
        v.addWidget(abar)
        v.addWidget(self.tabs, 1)
        v.addWidget(self.lbl_sum)
        self.setCentralWidget(central)

        self.btn_open.clicked.connect(self.on_open)
        self.btn_calc.clicked.connect(self.calculate)
        self.btn_export.clicked.connect(self.on_export)
        self.btn_acad.clicked.connect(self.on_export_acad)

        # ---- theme: remembered between runs; System follows the operating system live
        saved = str(self.settings.value("theme", "System"))
        saved = saved if saved in THEME_MODES else "System"
        self.theme_mode, self.dark = saved, False
        self.cb_theme.blockSignals(True)
        self.cb_theme.setCurrentText(saved)
        self.cb_theme.blockSignals(False)
        self.cb_theme.currentTextChanged.connect(lambda mode: self.apply_theme(mode))
        try:
            app.styleHints().colorSchemeChanged.connect(self._on_os_scheme_changed)
        except AttributeError:                        # Qt older than 6.5: no live OS notification
            pass
        self.apply_theme(saved, save=False)

    # ---------------------------------------------------------------------
    def _on_os_scheme_changed(self, *_):
        if self.theme_mode == "System":
            self.apply_theme("System", save=False)

    def apply_theme(self, mode, save=True):
        dark = {"Light": False, "Dark": True}.get(mode)
        if dark is None:
            dark = system_is_dark()
        self.theme_mode, self.dark = mode, dark
        QtWidgets.QApplication.instance().setPalette(make_palette(dark))
        self.model.set_dark(dark)
        self.table.horizontalHeader().set_dark(dark)
        self.graph.set_dark(dark)
        self.legend.setText(legend_html(dark))
        if save:
            self.settings.setValue("theme", mode)

    # ---------------------------------------------------------------------
    def focus_search(self):
        self.tabs.setCurrentIndex(0)
        self.search.setFocus()
        self.search.selectAll()

    def on_search_changed(self):
        q = self.search.text().strip().lower()
        self.hits, self.hit_i = [], -1
        if q and self.res is not None:
            self.hits = [i for i, t in enumerate(self.model.search_strings()) if q in t]
        if not q:
            self.lbl_hits.setText("")
            self.table.clearSelection()
        elif not self.hits:
            self.lbl_hits.setText("No match")
            self.table.clearSelection()
        else:
            self.goto_hit(+1)

    def goto_hit(self, step):
        if not self.hits:
            return
        self.hit_i = (self.hit_i + step) % len(self.hits)
        row = self.hits[self.hit_i]
        self.table.selectRow(row)
        self.table.scrollTo(self.model.index(row, 0), QtWidgets.QAbstractItemView.ScrollHint.PositionAtCenter)
        self.lbl_hits.setText(f"{self.hit_i + 1} / {len(self.hits)}")

    # ---------------------------------------------------------------------
    def on_open(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, "Open survey file", "", "Survey data (*.csv *.txt *.xlsx *.xlsm);;All files (*)")
        if path:
            self.load_file(path)

    def _apply_spans(self):
        """Marker rows show their text merged over B:G, like the Excel export."""
        self.table.clearSpans()
        for i, row in enumerate(self.res.rows):
            if row[0] == "marker":
                self.table.setSpan(i, 1, 1, 6)

    def zone_config(self):
        return ZoneConfig(
            srj_on=self.ck_srj.isChecked(), srj_kw=self.ed_srj.text(), srj_length=self.sp_srj.value(),
            ttc_on=self.ck_ttc.isChecked(), ttc_kw=self.ed_ttc.text(), ttc_length=self.sp_ttc.value(),
            tcc_on=self.ck_tcc.isChecked(), tcc_kw=self.ed_tcc.text(), tcc_length=self.sp_tcc.value(),
            curve_on=self.ck_curve.isChecked(), curve1_from=self.ed_c1f.text(), curve1_to=self.ed_c1t.text(),
            curve2_from=self.ed_c2f.text(), curve2_to=self.ed_c2t.text())

    def load_file(self, path, sheet=None):
        try:
            if sheet is None and os.path.splitext(path)[1].lower() in (".xlsx", ".xlsm"):
                names = list_sheets(path)
                if len(names) > 1:                               # several sheets: ask which one
                    default = names.index(SHEET_MAIN) if SHEET_MAIN in names else 0
                    sheet, ok = QtWidgets.QInputDialog.getItem(self, "Choose sheet", "Sheet to use:",
                                                               names, default, False)
                    if not ok:
                        return
            self.data = load_table(path, sheet)
        except Exception as ex:                                  # noqa: BLE001
            QtWidgets.QMessageBox.critical(self, "Cannot read file", str(ex))
            return
        self.path = path
        where = f"  [{sheet}]" if sheet else ""
        self.lbl_file.setText(f"{os.path.basename(path)}{where}  ({len(self.data['b'])} points, "
                              f"{len(self.data['markers'])} markers)")
        self.btn_calc.setEnabled(True)
        self.calculate()

    def calculate(self):
        if self.data is None:
            return
        gmin, gmax = self.sp_min.value(), self.sp_max.value()
        if not (gmin <= 0.0 <= gmax) or gmax - gmin <= 0:
            QtWidgets.QMessageBox.warning(self, "MIN / MAX", "MIN must be <= 0 and MAX >= 0 (e.g. -0.25 and +0.25).")
            return
        QtWidgets.QApplication.setOverrideCursor(QtCore.Qt.CursorShape.WaitCursor)
        try:
            self.res = Result(self.data, gmin, gmax, self.sp_level.value(), self.zone_config())
            self.model.set_result(self.res)
            self._apply_spans()
            self.graph.plot(self.res)
            self.on_search_changed()
        except Exception as ex:                                  # noqa: BLE001
            QtWidgets.QApplication.restoreOverrideCursor()
            QtWidgets.QMessageBox.critical(self, "Calculation failed", f"{type(ex).__name__}: {ex}")
            return
        finally:
            QtWidgets.QApplication.restoreOverrideCursor()
        msg = self.res.summary()
        if self.res.warnings:
            shown = self.res.warnings[:4]
            if len(self.res.warnings) > 4:
                shown.append(f"... {len(self.res.warnings) - 4} more")
            msg += "\n⚠ " + "  ⚠ ".join(shown)
        self.lbl_sum.setText(msg)
        self.btn_export.setEnabled(True)
        self.btn_acad.setEnabled(True)

    def on_export_acad(self):
        if self.res is None:
            return
        base = os.path.splitext(self.path or "gradient")[0] + "_AUTOCAD.csv"
        path, _ = QtWidgets.QFileDialog.getSaveFileName(self, "Export AutoCAD CSV", base, "CSV (*.csv)")
        if not path:
            return
        try:
            export_autocad_csv(self.res, path, self.ed_line.text(), self.ed_note.text())
        except Exception as ex:                                  # noqa: BLE001
            QtWidgets.QMessageBox.critical(self, "Export failed", str(ex))
            return
        self.statusBar().showMessage(f"Saved {path}", 8000)

    def on_export(self):
        if self.res is None:
            return
        base = os.path.splitext(self.path or "gradient")[0] + "_GRADIENT.xlsx"
        path, _ = QtWidgets.QFileDialog.getSaveFileName(self, "Export Excel", base, "Excel (*.xlsx)")
        if not path:
            return
        try:
            export_excel(self.res, path)
        except Exception as ex:                                  # noqa: BLE001
            QtWidgets.QMessageBox.critical(self, "Export failed", str(ex))
            return
        self.statusBar().showMessage(f"Saved {path}", 8000)


def main():
    app = QtWidgets.QApplication(sys.argv)
    w = MainWindow()
    w.show()
    if len(sys.argv) > 1 and os.path.exists(sys.argv[1]):
        w.load_file(sys.argv[1])
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
