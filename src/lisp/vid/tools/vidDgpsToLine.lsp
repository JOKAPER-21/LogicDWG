;;; ============================================================================
;;; Vid Dgps To Line
;;; Release: 1.5.8 | Civil 3D 2026
;;; Version: 18
;;;   v18: * Codes are matched ignoring case, spaces, "_" and "-":
;;;            R1 / r1 / R 1  = the same line.
;;;          A BS or END code is joined to its base code, BS / END can be
;;;          written before or after the base code:
;;;            R1 + R1 BS / BS R1         -> one line "R1"
;;;            R3 + R3 END / END R3       -> one line "R3"
;;;            R5 + BS R5 + END R5        -> one line "R5"
;;;          (the list in window 01 auto-selects all of them)
;;;        * Window 02: new section "Symbol" (between Line Types and Settings)
;;;            [x] Symbol                       (check box, default ON)
;;;            Symbol Name [vid_bufferStop_002] (default)
;;;          The block is imported from vid_blocks_tools.dwg when the drawing
;;;          does not have it.  It is placed on the TRACK CENTER polyline
;;;          (the offset polyline of "Polyline Center"; when that type is not
;;;          ticked a temporary one is made and erased again) at the end that
;;;          has a BS / END point:
;;;            block rotation = (polyline tangent pointing INTO the line) + 180:
;;;                start of the polyline = tangent + 180,  end = tangent + 360
;;;            bufferStop_rot = polyline tangent + 180 + 180:
;;;                start of the polyline = tangent + 360,  end = tangent + 180
;;;            Offsets (degrees): DGPS-SymRotOffset = 180 (block rotation),
;;;                               DGPS-SymPropOffset = 180 (bufferStop_rot)
;;;          <block(a+180, bs a)>-------------<block(a, bs a+180)>
;;;          The polylines are never changed or reversed.
;;;   v17: Two windows.
;;;        Window 01 "Selected layers" : list of CSV codes.
;;;        Window 02 : Line Types (Polyline / 3D Polyline / Polyline Center),
;;;                    Settings (Track offset + LHS/RHS, Angle Filter, Use Local Time),
;;;                    Layers (Custom Layer [1-TRACK] / CSV Code).
;;;        Custom Layer name ending in TRACK is the Polyline Center layer:
;;;              1-TRACK -> "1-TRACK SIDE" / "1-TRACK" / "1-TRACK 3D"
;;;        Any other name gets " TRACK ..." added:
;;;              5-Pro   -> "5-Pro TRACK SIDE" / "5-Pro TRACK" / "5-Pro TRACK 3D"
;;;        Blank Custom Layer = 1-TRACK.
;;;   v16: * "Rail Track - Select Layer": label is now "Line Types"; the radio buttons
;;;          are check boxes  [Polyline] [3D Polyline] [Polyline Center]
;;;          (any combination).  Polyline Center has a [LHS/RHS] drop-down and a
;;;          "Track Offset" box (default 0.86), both enabled only when it is ticked.
;;;        * Default layers / colours (no list any more):
;;;              Polyline         "1-TRACK SIDE"   yellow
;;;              Polyline Center  "1-TRACK"        magenta
;;;              3D Polyline      "1-TRACK 3D"     green
;;;        * Layer Options: [Default layer list + radio] removed.
;;;              Custom Layer "5-Pro"  -> "5-Pro TRACK SIDE" / "5-Pro TRACK" / "5-Pro TRACK 3D"
;;;              Custom Layer blank    -> the default layers above
;;;              CSV Code (R1, R2 ...) -> "R1 TRACK SIDE" / "R1 TRACK" / "R1 TRACK 3D" ...
;;;        * Polyline Center = the 2D polyline offset by "Track Offset" to the LHS or
;;;          RHS (looking along the polyline direction).  Never from the 3D polyline.
;;;          If [Polyline] is not ticked, a temporary polyline is made on layer 0,
;;;          offset, then erased - no polyline / polyline layer is left behind.
;;;        * Spaces are kept in layer names (no "_" for space).
;;;   v15: * Layer Options dialog re-laid out: (o) Default Layers [full list, "1-TRACK"
;;;          pre-selected], ( ) Custom Layer [text box], ( ) CSV Code.
;;;        * Blank Custom Layer name keeps the dialog open instead of silently
;;;          falling back to a default layer.
;;;   v14: * Angle filter now has its own whole-number box (+/- deg, default 5).
;;;        * Codes R, R1, R2, R3 ... R<n> are selected automatically in the list.
;;;        * Layer Options: new radio [From CSV] -> layer "1-TRACK_<Code>" is
;;;          created for every Code.  [Default layers] and Custom are unchanged.
;;;   v13: the temporary HP spline is erased after all track lines are created
;;;        (set DGPS-HPKeep to T to keep it).  v14b: the HP curve AND the HP layer are
;;;        now removed in both polyline and 3D polyline mode.
;;;   v12: HP (hectometre post) rows are detected automatically after the CSV is
;;;        read - no layer selection needed.  Accepted names (PName or Code):
;;;            HP 71/600   HP 71-600   HP 71_600   HP 71 600   HP71/600
;;;        All HP points are sorted ascending (km*1000 + m), joined with a
;;;        smooth spline-style curve (polyline / 3D polyline, per the dialog
;;;        radio) on layer "HP".  The curve tangent is then used to give every
;;;        newly created track polyline the SAME direction as the HP chainage.
;;;   v11: "Local Time" OFF now joins NEAREST points from both ends of the
;;;        chain (no CSV-order dependence) - fixes zig-zag like 1..132 then 133..486.
;;; ============================================================================
;;;
;;; CHANGE LOG v1.5.4 (Version 10)
;;;   - "Rail Track - Select Layer" dialog: two new check boxes
;;;       [x] Angle filter (+/- 5 deg)  ON  = remove spike vertices (old behaviour)
;;;                                     OFF = keep every vertex
;;;       [ ] Use "Local Time" column to order and connect points
;;;                                     ON  = points sorted by Local Time and
;;;                                           joined in that order (no
;;;                                           nearest-neighbour, no edge removal)
;;;                                     OFF = nearest-neighbour chain (old behaviour)
;;;   - Both options apply to Polyline and 3D Polyline output.
;;;   - Last used settings are remembered (DGPS-UseAngle / DGPS-UseTime).
;;;
;;; CHANGE LOG v1.5.0 (Version 06)
;;;   - Added angular-deviation filter (DGPS-AngleFilter) as a post-processing
;;;     pass after nearest-neighbour chaining and longest-edge removal.
;;;   - Algorithm:
;;;       For each interior vertex P(i), compute the bearing change between
;;;       edge P(i-1)→P(i) and edge P(i)→P(i+1).
;;;       If the absolute bearing change is > 5°, vertex P(i) is a bad GPS
;;;       point — remove it.
;;;       Repeat iteratively until no more vertices are removed in a full pass
;;;       (stable state).
;;;       First and last vertices are always preserved.
;;;   - Report now shows: Points (after NN chain), Filtered (removed by angle),
;;;       Final Points (used in polyline).
;;;   - DGPS-Bearing2D helper added.
;;;   - DGPS-AngleDiff helper added (handles 0°/360° wrap correctly).
;;; ============================================================================
;;;
;;; WORKFLOW:
;;;   1. User opens LogicDWG dialog, clicks Rail Track [Generate].
;;;   2. User selects a CSV file (getfiled).
;;;   3. CSV is parsed; unique Codes are extracted.
;;;   4. Layer-selection dialog appears:
;;;        - "Line Types": [Polyline] [3D Polyline] [Polyline Center]
;;;          (Polyline Center: LHS/RHS drop-down + Track Offset box)
;;;        - Scrollable list of grouped Code names (R1, R2, OHE, ...)
;;;        - [Select Layers] button
;;;        - Then a second dialog: Custom Layer name (blank = default layers)
;;;          or CSV Code, and confirm.
;;;   5. The ticked line types are created on their layers.
;;;
;;; ============================================================================

(vl-load-com)

;; ---------------------------------------------------------------------------
;; Globals used by error handler
;; ---------------------------------------------------------------------------
(setq dgps-*fh*        nil)
(setq dgps-*old-error* nil)
(setq dgps-*old-clayer*   nil)
(setq dgps-*old-cmdecho*  nil)
(setq dgps-*old-osmode*   nil)

;; Forward re-ordering look-ahead window
(if (null DGPS-ForwardWindow) (setq DGPS-ForwardWindow 8))

;; HP (hectometre post) settings
(if (null DGPS-HPLayer) (setq DGPS-HPLayer "HP"))   ; layer for the HP curve
(if (null DGPS-HPSegs)  (setq DGPS-HPSegs  10))     ; spline samples per HP span
(if (null DGPS-HPKeep)  (setq DGPS-HPKeep  nil))    ; T = keep HP spline after run
(setq DGPS-HPSamples nil)                           ; (x y z tx ty) of HP curve
(setq DGPS-LastFlipped nil)

;; Dialog options (remembered between runs)
(if (null DGPS-UseAngle) (setq DGPS-UseAngle T))      ; angle filter on
(if (null DGPS-UseTime)  (setq DGPS-UseTime  nil))    ; Local Time ordering off
(if (null DGPS-AngleThreshold) (setq DGPS-AngleThreshold 5.0))
;; Line Types (remembered between runs)
(if (null DGPS-UsePoly)      (setq DGPS-UsePoly   T))      ; [Polyline]
(if (null DGPS-Use3D)        (setq DGPS-Use3D     nil))    ; [3D Polyline]
(if (null DGPS-UseCenter)    (setq DGPS-UseCenter nil))    ; [Polyline Center]
(if (null DGPS-CenterSide)   (setq DGPS-CenterSide "LHS")) ; LHS / RHS
(if (null DGPS-CenterOffTxt) (setq DGPS-CenterOffTxt "0.86"))
(if (null DGPS-CenterOffset) (setq DGPS-CenterOffset 0.86))
(if (null DGPS-CustomLayerTxt) (setq DGPS-CustomLayerTxt "1-TRACK"))
(setq dgps-*old-offerase* nil)
;; Symbol (placed at BS / END points)
(if (null DGPS-SymbolName)  (setq DGPS-SymbolName  "vid_bufferStop_002")) ; blank = no symbol
(if (null DGPS-SymRotProp)  (setq DGPS-SymRotProp  "bufferStop_rot"))     ; dynamic rotation property
(if (null DGPS-UseSymbol)   (setq DGPS-UseSymbol   T))                    ; Symbol check box
;; These two are set every time the file is loaded (not "if null"), so an old
;; value left over from an earlier load in the same AutoCAD session can never
;; override the value written here.
(setq DGPS-SymRotOffset  180.0)   ; degrees added to the BLOCK rotation (block rotation + 180)
(setq DGPS-SymPropOffset 180.0)   ; degrees added to bufferStop_rot
(setq DGPS-SymbolOK nil)
(setq DGPS-SymTotal 0)
(setq DGPS-SymFail  0)
(setq DGPS-RegKey "HKEY_CURRENT_USER\\Software\\VID\\Tools")   ; same as vidBankCutting

;; ---------------------------------------------------------------------------
;; Basic string helpers
;; ---------------------------------------------------------------------------
(defun DGPS-Trim (s)
  (if s (vl-string-trim " \t\r\n" s) "")
)

(defun DGPS-Upper (s)
  (strcase (DGPS-Trim s))
)

(defun DGPS-BlankP (s)
  (= (DGPS-Trim s) "")
)

(defun DGPS-GetField (fields idx)
  (if (and idx (>= idx 0) (< idx (length fields)))
    (DGPS-Trim (nth idx fields))
    ""
  )
)

;; ---------------------------------------------------------------------------
;; Numeric validation
;; ---------------------------------------------------------------------------
(defun DGPS-NumericP (s / t1 n i ch code seenDot seenExp ok)
  (setq t1 (DGPS-Trim s))
  (if (= t1 "")
    nil
    (progn
      (setq n (strlen t1)
            i 1
            seenDot nil
            seenExp nil
            ok T)
      (if (or (= (substr t1 1 1) "+") (= (substr t1 1 1) "-"))
        (setq i 2)
      )
      (while (and ok (<= i n))
        (setq ch (substr t1 i 1)
              code (ascii ch))
        (cond
          ((and (or (= ch "e") (= ch "E")) (not seenExp))
           (setq seenExp T)
           (if (= i n)
             (setq ok nil)
             (progn
               (setq i (1+ i))
               (if (or (= (substr t1 i 1) "+") (= (substr t1 i 1) "-"))
                 (if (= i n) (setq ok nil))
               )
             )
           )
          )
          ((= ch ".")
           (if (or seenDot seenExp)
             (setq ok nil)
             (setq seenDot T)
           )
          )
          ((and (>= code 48) (<= code 57)))
          (T (setq ok nil))
        )
        (setq i (1+ i))
      )
      ok
    )
  )
)

;; ---------------------------------------------------------------------------
;; Layer-name sanitization
;; ---------------------------------------------------------------------------
;; Spaces are kept; other illegal layer-name characters become "_".
(defun DGPS-SanitizeLayerName (s / src i ch out)
  (setq src (DGPS-Trim s)
        i   1
        out "")
  (while (<= i (strlen src))
    (setq ch (substr src i 1))
    (if (or (= ch "<") (= ch ">") (= ch "/") (= ch "\\")
            (= ch "\"") (= ch ":") (= ch ";") (= ch "?")
            (= ch "*") (= ch "|") (= ch ",") (= ch "=")
            (= ch "`"))
      (setq out (strcat out "_"))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  (if (= out "") (setq out "_NOCODE"))
  (vl-string-trim " " out)
)

;; ---------------------------------------------------------------------------
;; Layer creation / reuse
;; ---------------------------------------------------------------------------
(defun DGPS-EnsureLayer (name color / result)
  (if (tblsearch "LAYER" name)
    name
    (progn
      (setq result
        (entmakex
          (list
            '(0 . "LAYER")
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord")
            (cons 2 name)
            '(70 . 0)
            (cons 62 color)
            '(6 . "Continuous")
          )
        )
      )
      (if result name nil)
    )
  )
)

;; ---------------------------------------------------------------------------
;; 2D polyline (LWPOLYLINE)
;; ---------------------------------------------------------------------------
(defun DGPS-Make2DPolyline (pts layer / data e)
  (if (>= (length pts) 2)
    (progn
      (setq data
        (list
          '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          (cons 8 layer)
          '(100 . "AcDbPolyline")
          (cons 90 (length pts))
          '(70 . 0)
          '(43 . 0.0)
        )
      )
      (foreach p pts
        (setq data
          (append data
            (list (cons 10 (list (car p) (cadr p))))
          )
        )
      )
      (setq e (entmakex data))
      (if (and e (= (cdr (assoc 0 (entget e))) "LWPOLYLINE")) e nil)
    )
    nil
  )
)

;; ---------------------------------------------------------------------------
;; True 3D POLYLINE
;; ---------------------------------------------------------------------------
(defun DGPS-Make3DPolyline (pts layer / head verts e)
  (if (>= (length pts) 2)
    (progn
      (setq head
        (entmakex
          (list
            '(0 . "POLYLINE")
            '(100 . "AcDbEntity")
            (cons 8 layer)
            '(100 . "AcDb3dPolyline")
            '(66 . 1)
            '(70 . 8)
            '(210 0.0 0.0 1.0)
          )
        )
      )
      (if head
        (progn
          (setq verts T)
          (foreach p pts
            (if
              (not
                (entmakex
                  (list
                    '(0 . "VERTEX")
                    '(100 . "AcDbEntity")
                    (cons 8 layer)
                    '(100 . "AcDbVertex")
                    '(100 . "AcDb3dPolylineVertex")
                    (cons 10 p)
                    '(70 . 32)
                  )
                )
              )
              (setq verts nil)
            )
          )
          (if verts
            (progn
              (setq e (entmakex (list '(0 . "SEQEND") (cons 8 layer))))
              (if e head nil)
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

;; ---------------------------------------------------------------------------
;; CSV parser - handles quoted commas and escaped quotes.
;; ---------------------------------------------------------------------------
(defun DGPS-ParseCSV (s / i n ch inquote cur fields)
  (setq i 1 n (strlen s) inquote nil cur "" fields '())
  (while (<= i n)
    (setq ch (substr s i 1))
    (cond
      ((and (= ch "\"") inquote
            (< i n)
            (= (substr s (1+ i) 1) "\""))
       (setq cur (strcat cur "\""))
       (setq i (+ i 2))
      )
      ((= ch "\"")
       (setq inquote (not inquote))
       (setq i (1+ i))
      )
      ((and (= ch ",") (not inquote))
       (setq fields (cons cur fields))
       (setq cur "")
       (setq i (1+ i))
      )
      (T
       (setq cur (strcat cur ch))
       (setq i (1+ i))
      )
    )
  )
  (reverse (cons cur fields))
)

(defun DGPS-QuoteCount (s / i n count)
  (setq i 1 n (strlen s) count 0)
  (while (<= i n)
    (if (= (substr s i 1) "\"")
      (setq count (1+ count))
    )
    (setq i (1+ i))
  )
  count
)

;; Reads one physical line (binary mode; safe against Ctrl-Z / 0x1A).
(defun DGPS-ReadPhysicalLine (fh / b s gotLine)
  (setq s "" gotLine nil)
  (while (and (not gotLine) (setq b (read-char fh)))
    (cond
      ((= b 10)  (setq gotLine T))
      ((= b 13)
       (setq b (read-char fh))
       (if (and b (/= b 10))
         (setq s (strcat s (chr b)))
       )
       (setq gotLine T)
      )
      (T (setq s (strcat s (chr b))))
    )
  )
  (if (or gotLine (> (strlen s) 0)) s nil)
)

;; Reads one logical CSV record (supports quoted multiline fields).
(defun DGPS-ReadRecord (fh / s next)
  (setq s (DGPS-ReadPhysicalLine fh))
  (if s
    (while (= (rem (DGPS-QuoteCount s) 2) 1)
      (setq next (DGPS-ReadPhysicalLine fh))
      (if next
        (setq s (strcat s "\n" next))
        (setq s nil)
      )
      (if (null next) (setq s nil))
    )
  )
  s
)

;; ---------------------------------------------------------------------------
;; Open CSV in binary mode.
;; ---------------------------------------------------------------------------
(defun DGPS-OpenCSV (file / fh)
  (setq fh (open file "rb"))
  (if fh
    (list fh "ANSI/BINARY")
    (list nil nil)
  )
)

;; ---------------------------------------------------------------------------
;; UTF-8 BOM stripping from the first header field.
;; ---------------------------------------------------------------------------
(defun DGPS-StripBOM (s)
  (if (and s (>= (strlen s) 3)
           (= (ascii (substr s 1 1)) 239)
           (= (ascii (substr s 2 1)) 187)
           (= (ascii (substr s 3 1)) 191))
    (substr s 4)
    s
  )
)

;; ---------------------------------------------------------------------------
;; Header lookup
;; ---------------------------------------------------------------------------
(defun DGPS-HeaderIndex (headers wanted / i found)
  (setq i 0 found nil)
  (while (and (< i (length headers)) (null found))
    (if (= (DGPS-Upper (nth i headers)) (DGPS-Upper wanted))
      (setq found i)
    )
    (setq i (1+ i))
  )
  found
)

;; ---------------------------------------------------------------------------
;; Record accessors
;; record = (pname code north east elev time row sortkey)
;; ---------------------------------------------------------------------------
(defun DGPS-R-PName (r) (nth 0 r))
(defun DGPS-R-Code  (r) (nth 1 r))
(defun DGPS-R-North (r) (nth 2 r))
(defun DGPS-R-East  (r) (nth 3 r))
(defun DGPS-R-Elev  (r) (nth 4 r))
(defun DGPS-R-Time  (r) (nth 5 r))
(defun DGPS-R-Row   (r) (nth 6 r))
(defun DGPS-R-Key   (r) (nth 7 r))

;; ---------------------------------------------------------------------------
;; Time sort key
;; ---------------------------------------------------------------------------
(defun DGPS-TimeKey (s / timeStr pos h m sec ms p2 p3)
  (setq timeStr (DGPS-Trim s))
  (setq pos (vl-string-search " " timeStr))
  (if pos (setq timeStr (substr timeStr (+ pos 2))))
  (if (>= (strlen timeStr) 8)
    (progn
      (setq h   (substr timeStr 1 2)
            m   (substr timeStr 4 2)
            sec (substr timeStr 7 2))
      (if (and (DGPS-NumericP h) (DGPS-NumericP m) (DGPS-NumericP sec))
        (progn
          (setq ms "000")
          (setq p2 (vl-string-search "." timeStr))
          (if p2
            (progn
              (setq p3 (substr timeStr (+ p2 2)))
              (setq ms (substr (strcat p3 "000") 1 3))
            )
          )
          (list (atoi h) (atoi m) (atoi sec) (atoi ms))
        )
        nil
      )
    )
    nil
  )
)

(defun DGPS-TimeLess (a b / ka kb)
  (setq ka (DGPS-R-Key a) kb (DGPS-R-Key b))
  (cond
    ((and ka kb)
     (cond
       ((< (nth 0 ka) (nth 0 kb)) T)
       ((> (nth 0 ka) (nth 0 kb)) nil)
       ((< (nth 1 ka) (nth 1 kb)) T)
       ((> (nth 1 ka) (nth 1 kb)) nil)
       ((< (nth 2 ka) (nth 2 kb)) T)
       ((> (nth 2 ka) (nth 2 kb)) nil)
       ((< (nth 3 ka) (nth 3 kb)) T)
       ((> (nth 3 ka) (nth 3 kb)) nil)
       (T (< (DGPS-R-Row a) (DGPS-R-Row b)))
     )
    )
    ((and ka (null kb)) T)
    ((and (null ka) kb) nil)
    (T (< (DGPS-R-Row a) (DGPS-R-Row b)))
  )
)

;; ---------------------------------------------------------------------------
;; Group records by Code.
;; Result: ((code rec1 rec2 ...) ...)
;; ---------------------------------------------------------------------------
(defun DGPS-GroupByCode (records / groups code cell)
  (setq groups '())
  (foreach rec records
    (setq code (DGPS-R-Code rec))
    (setq cell (assoc code groups))
    (if cell
      (setq groups (subst (append cell (list rec)) cell groups))
      (setq groups (append groups (list (cons code (list rec)))))
    )
  )
  groups
)

;; ---------------------------------------------------------------------------
;; Forward ordering
;; ---------------------------------------------------------------------------
(defun DGPS-Dist2D (a b / dx dy)
  (setq dx (- (atof (DGPS-R-East  a)) (atof (DGPS-R-East  b)))
        dy (- (atof (DGPS-R-North a)) (atof (DGPS-R-North b))))
  (sqrt (+ (* dx dx) (* dy dy)))
)

(defun DGPS-OrderForward (recs win / remaining result cur cand k best bestD d c)
  (if (or (null recs) (null (cdr recs)))
    recs
    (progn
      (if (or (null win) (< win 2)) (setq win 2))
      (setq cur (car recs)
            remaining (cdr recs)
            result (list cur))
      (while remaining
        (setq cand '() k 0)
        (foreach c remaining
          (if (< k win)
            (progn (setq cand (cons c cand)) (setq k (1+ k)))
          )
        )
        (setq cand (reverse cand))
        (setq best nil bestD nil)
        (foreach c cand
          (setq d (DGPS-Dist2D cur c))
          (if (or (null bestD) (< d bestD))
            (setq best c bestD d)
          )
        )
        (setq result (cons best result))
        (setq remaining (vl-remove best remaining))
        (setq cur best)
      )
      (reverse result)
    )
  )
)

(defun DGPS-CountMoved (a b / n)
  (setq n 0)
  (while (and a b)
    (if (/= (DGPS-R-Row (car a)) (DGPS-R-Row (car b)))
      (setq n (1+ n))
    )
    (setq a (cdr a) b (cdr b))
  )
  n
)

;; ---------------------------------------------------------------------------
;; Error handler
;; ---------------------------------------------------------------------------
(defun DGPS-Error (msg)
  (if dgps-*fh*
    (progn
      (vl-catch-all-apply 'close (list dgps-*fh*))
      (setq dgps-*fh* nil)
    )
  )
  (if dgps-*old-clayer*  (setvar "CLAYER"  dgps-*old-clayer*))
  (if dgps-*old-cmdecho* (setvar "CMDECHO" dgps-*old-cmdecho*))
  (if dgps-*old-osmode*  (setvar "OSMODE"  dgps-*old-osmode*))
  (if dgps-*old-offerase*
    (progn
      (vl-catch-all-apply 'setvar (list "OFFSETERASE" dgps-*old-offerase*))
      (setq dgps-*old-offerase* nil)
    )
  )
  (setq *error* dgps-*old-error*)
  (if (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort"))
    (princ (strcat "\nVIDDGPSTOLINE ERROR: " msg))
    (princ "\nVIDDGPSTOLINE cancelled.")
  )
  (princ)
)

;; ---------------------------------------------------------------------------
;; v14 helpers
;; ---------------------------------------------------------------------------
;; Normalised text: upper case, spaces / tabs / "_" / "-" removed
(defun DGPS-NormCode (code / u i ch out)
  (setq u (strcase (DGPS-Trim code)) i 1 out "")
  (while (<= i (strlen u))
    (setq ch (substr u i 1))
    (if (not (member ch '(" " "\t" "_" "-")))
      (setq out (strcat out ch)))
    (setq i (1+ i))
  )
  out
)

;; split on spaces / tabs / "_" / "-" (empty pieces dropped, case kept)
(defun DGPS-SplitTokens (code / s i ch cur out)
  (setq s (DGPS-Trim code) i 1 cur "" out '())
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (member ch '(" " "\t" "_" "-"))
      (progn
        (if (/= cur "") (setq out (cons cur out)))
        (setq cur ""))
      (setq cur (strcat cur ch)))
    (setq i (1+ i))
  )
  (if (/= cur "") (setq out (cons cur out)))
  (reverse out)
)

;; T for  R  or  R<digits>  (upper case text)
(defun DGPS-RNameP (u / rest)
  (cond
    ((= u "R") T)
    ((and (> (strlen u) 1) (= (substr u 1 1) "R"))
     (setq rest (substr u 2))
     (= (vl-string-trim "0123456789" rest) ""))
    (T nil)
  )
)

;; Split a code into its BASE and its BS / END marker.
;; Returns (base marker display)
;;   base    = normalised key (upper case, no separators)  "R1"
;;   marker  = "BS" / "END" / nil
;;   display = the base text of the code (original case)   "R1"
;; BS / END may come after OR before the base code:
;;   "R1 BS"  "BS R1"  "r1 bs"  "R1-BS"  "R1BS"  "BSR1"  -> ("R1" "BS" "R1")
;; A glued BS / END (no space) is only taken for R codes (R1BS, ENDR3), so
;; codes such as SLABS or TREND are never split.
(defun DGPS-ParseCode (code / tk n u first lastT rest tkU)
  (setq tk (DGPS-SplitTokens code)
        n  (length tk))
  (cond
    ((= n 0) (list "" nil ""))
    ;; several words: BS / END as the last or the first word
    ((and (>= n 2) (member (strcase (last tk)) '("BS" "END")))
     (setq rest (reverse (cdr (reverse tk))))
     (list (DGPS-NormCode (apply 'strcat rest)) (strcase (last tk))
           (DGPS-JoinTokens rest)))
    ((and (>= n 2) (member (strcase (car tk)) '("BS" "END")))
     (setq rest (cdr tk))
     (list (DGPS-NormCode (apply 'strcat rest)) (strcase (car tk))
           (DGPS-JoinTokens rest)))
    ;; one word, glued:  R1BS  R3END  BSR1  ENDR3
    ((= n 1)
     (setq first (car tk) u (strcase first))
     (cond
       ((and (> (strlen u) 3) (= (substr u (- (strlen u) 2)) "END")
             (DGPS-RNameP (substr u 1 (- (strlen u) 3))))
        (list (substr u 1 (- (strlen u) 3)) "END" (substr first 1 (- (strlen first) 3))))
       ((and (> (strlen u) 2) (= (substr u (- (strlen u) 1)) "BS")
             (DGPS-RNameP (substr u 1 (- (strlen u) 2))))
        (list (substr u 1 (- (strlen u) 2)) "BS" (substr first 1 (- (strlen first) 2))))
       ((and (> (strlen u) 3) (= (substr u 1 3) "END")
             (DGPS-RNameP (substr u 4)))
        (list (substr u 4) "END" (substr first 4)))
       ((and (> (strlen u) 2) (= (substr u 1 2) "BS")
             (DGPS-RNameP (substr u 3)))
        (list (substr u 3) "BS" (substr first 3)))
       (T (list u nil first))
     )
    )
    (T (list (DGPS-NormCode (apply 'strcat tk)) nil (DGPS-JoinTokens tk)))
  )
)

(defun DGPS-JoinTokens (tk / out)
  (setq out "")
  (foreach w tk (setq out (strcat out (if (= out "") "" " ") w)))
  out
)

(defun DGPS-CodeBase    (code) (car   (DGPS-ParseCode code)))   ; "R1"
(defun DGPS-CodeMarker  (code) (cadr  (DGPS-ParseCode code)))   ; "BS" / "END" / nil
(defun DGPS-CodeDisplay (code) (caddr (DGPS-ParseCode code)))   ; text for layer names

;; T when the code is  R  or  R<digits>  (R1, R2 ... R25 ...), also
;; "R1 BS", "r1 end", "R 1" ...  (case, spaces and BS / END ignored)
(defun DGPS-AutoRP-P (code / u rest)
  (setq u (DGPS-CodeBase code))
  (cond
    ((= u "R") T)
    ((and (> (strlen u) 1) (= (substr u 1 1) "R"))
     (setq rest (substr u 2))
     (= (vl-string-trim "0123456789" rest) "")
    )
    (T nil)
  )
)

;; Group records by the BASE code (case / spaces / BS / END ignored):
;;   R1 + R1 BS -> one group.   Result: ((displayName rec1 rec2 ...) ...)
(defun DGPS-GroupMerged (records / groups key cell rec out disp first)
  (setq groups '())
  (foreach rec records
    (setq key  (DGPS-CodeBase (DGPS-R-Code rec)))
    (setq cell (assoc key groups))
    (if cell
      (setq groups (subst (append cell (list rec)) cell groups))
      (setq groups (append groups (list (cons key (list rec)))))
    )
  )
  (setq out '())
  (foreach cell groups
    ;; display name: from the first record that is not a BS / END record
    (setq first nil)
    (foreach rec (cdr cell)
      (if (and (null first) (null (DGPS-CodeMarker (DGPS-R-Code rec))))
        (setq first rec)))
    (setq disp (DGPS-CodeDisplay (DGPS-R-Code (if first first (cadr cell)))))
    (setq out (append out (list (cons disp (cdr cell)))))
  )
  out
)

;; ---------------------------------------------------------------------------
;; Symbol block: locate vid_blocks_tools.dwg and import the block
;; (same lookup as vidBankCutting.lsp)
;; ---------------------------------------------------------------------------
(defun DGPS-FindBlocksDwg (/ hit f dir n rel)
  (if (and *VID-BLOCKS-DWG* (findfile *VID-BLOCKS-DWG*))
    (setq hit (findfile *VID-BLOCKS-DWG*))
  )
  (if (not hit)
    (progn
      (setq f (vl-registry-read DGPS-RegKey "BlocksDwg"))
      (if (and f (= (type f) 'STR) (findfile f))
        (setq hit (findfile f))
      )
    )
  )
  (if (not hit)
    (foreach n '("vidGradient.lsp" "vidGradientAlign.lsp" "vidBankCutting.lsp"
                 "vidDgpsToLine.lsp" "vidLC.lsp")
      (if (and (not hit) (setq f (findfile n)))
        (progn
          (setq dir (vl-filename-directory f)
                f   (findfile (strcat dir "/vid_blocks_tools.dwg")))
          (if f (setq hit f))
        )
      )
    )
  )
  (if (not hit)
    (foreach rel '("support/vid/tools/vid_blocks_tools.dwg"
                   "vid/tools/vid_blocks_tools.dwg"
                   "vid_blocks_tools.dwg")
      (if (and (not hit) (setq f (findfile rel)))
        (setq hit f)
      )
    )
  )
  (if (not hit)
    (setq hit (getfiled "Locate vid_blocks_tools.dwg" "" "dwg" 0))
  )
  (if hit
    (progn
      (setq *VID-BLOCKS-DWG* hit)
      (vl-catch-all-apply 'vl-registry-write
        (list DGPS-RegKey "BlocksDwg" hit))
    )
  )
  hit
)

;; Import block <name> from <dwg> when the drawing does not have it yet.
(defun DGPS-ImportBlock (dwg name / acad doc ver dbx blks item objs path)
  (if (not (tblsearch "BLOCK" name))
    (progn
      (setq acad (vlax-get-acad-object)
            doc  (vla-get-ActiveDocument acad)
            ver  (itoa (atoi (getvar "ACADVER")))
            path (vl-string-translate "/" "\\" dwg))
      (setq dbx (vl-catch-all-apply 'vla-GetInterfaceObject
                  (list acad (strcat "ObjectDBX.AxDbDocument." ver))))
      (if (vl-catch-all-error-p dbx)
        (setq dbx (vl-catch-all-apply 'vla-GetInterfaceObject
                    (list acad "ObjectDBX.AxDbDocument"))))
      (cond
        ((vl-catch-all-error-p dbx)
         (prompt "\nCannot start ObjectDBX - block not imported.")
         (setq dbx nil))
        ((vl-catch-all-error-p
           (vl-catch-all-apply 'vla-Open (list dbx path)))
         (prompt (strcat "\nCannot open " path))
         (vl-catch-all-apply 'vlax-release-object (list dbx))
         (setq dbx nil))
        (T
         (setq blks (vla-get-Blocks dbx))
         (setq item (vl-catch-all-apply 'vla-Item (list blks name)))
         (if (vl-catch-all-error-p item)
           (prompt (strcat "\nBlock not found in DWG: " name))
           (progn
             (setq objs (vlax-make-safearray vlax-vbObject '(0 . 0)))
             (vlax-safearray-put-element objs 0 item)
             (if (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-CopyObjects
                     (list dbx objs (vla-get-Blocks doc))))
               (prompt (strcat "\nCould not import block: " name))
               (prompt (strcat "\nImported block: " name))
             )
           )
         )
         (vl-catch-all-apply 'vlax-release-object (list dbx))
        )
      )
    )
  )
  (if (tblsearch "BLOCK" name) T nil)
)

;; Set a dynamic block property by name (case-insensitive). T on success.
(defun DGPS-SetDynProp (ent name val / obj props p res)
  (setq obj (vlax-ename->vla-object ent))
  (setq props (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p props) (setq props nil))
  (setq p
    (car
      (vl-remove-if-not
        '(lambda (q) (= (strcase (vla-get-PropertyName q)) (strcase name)))
        props)))
  (if p
    (progn
      (setq res (vl-catch-all-apply 'vla-put-Value
                  (list p (vlax-make-variant val vlax-vbDouble))))
      (not (vl-catch-all-error-p res))
    )
    nil
  )
)

(defun DGPS-DynPropNames (ent / obj props out)
  (setq obj (vlax-ename->vla-object ent) out "")
  (setq props (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (not (vl-catch-all-error-p props))
    (foreach q props
      (setq out (strcat out (if (= out "") "" ", ") (vla-get-PropertyName q)))))
  out
)

;; T when one of the records is a BS / END record
(defun DGPS-HasMarker (recs)
  (vl-some '(lambda (r) (DGPS-CodeMarker (DGPS-R-Code r))) recs)
)

;; Reads the Symbol part of window 02. T = OK to continue.
(defun DGPS-SymbolBoxOK (/ nm)
  (setq nm (DGPS-Trim (get_tile "edt_sym")))
  (cond
    ((and (= (get_tile "tg_sym") "1") (= nm ""))
     (alert "Enter the Symbol Name, or untick Symbol.")
     (mode_tile "edt_sym" 2)
     nil)
    (T
     (setq DGPS-UseSymbol (= (get_tile "tg_sym") "1"))
     (if (/= nm "") (setq DGPS-SymbolName nm))
     T)
  )
)

;; Place the symbol on the TRACK CENTER polyline <ce> at the end that has a
;; BS / END point (the nearest end to that point):
;;     block rotation  = tangent pointing INTO the line + DGPS-SymRotOffset (180):
;;                       start = tangent + 180, end = tangent + 360
;;     bufferStop_rot  = polyline tangent (start + 180, end) + DGPS-SymPropOffset (180)
;;                       = the same angle as the block rotation
;; The polyline is only read.  Returns the number of blocks placed.
(defun DGPS-PlaceSymbols (recs ce lay / sp ep sDone eDone cnt rec mk x y z
                          atStart pt drv tang prop brot ent)
  (setq cnt 0 sDone nil eDone nil)
  (if (and DGPS-SymbolOK ce)
    (progn
      (setq sp (vlax-curve-getStartPoint ce)
            ep (vlax-curve-getEndPoint ce))
      (foreach rec recs
        (setq mk (DGPS-CodeMarker (DGPS-R-Code rec)))
        (if mk
          (progn
            (setq x (atof (DGPS-R-East  rec))
                  y (atof (DGPS-R-North rec))
                  z (atof (DGPS-R-Elev  rec)))
            ;; end of the track center polyline nearest to this BS / END point
            (setq atStart
              (<= (DGPS-Dist2DPt (list x y) sp) (DGPS-Dist2DPt (list x y) ep)))
            (if (not (if atStart sDone eDone))
              (progn
                (if atStart (setq sDone T) (setq eDone T))
                (setq pt (if atStart sp ep))
                ;; polyline tangent at that end (as drawn)
                (setq drv
                  (vl-catch-all-apply 'vlax-curve-getFirstDeriv
                    (list ce (if atStart
                               (vlax-curve-getStartParam ce)
                               (vlax-curve-getEndParam ce)))))
                (setq tang
                  (if (vl-catch-all-error-p drv)
                    0.0
                    (angle (list 0.0 0.0) (list (car drv) (cadr drv)))))
                ;; bufferStop_rot: start of the polyline = tangent + 180, end = tangent
                (setq prop (+ (if atStart (+ tang pi) tang)
                              (* DGPS-SymPropOffset (/ pi 180.0))))
                (while (>= prop (* 2.0 pi)) (setq prop (- prop (* 2.0 pi))))
                (while (< prop 0.0)         (setq prop (+ prop (* 2.0 pi))))
                ;; block rotation: tangent pointing into the line
                ;; (start = tangent, end = tangent + 180)
                (setq brot (+ (if atStart tang (+ tang pi))
                              (* DGPS-SymRotOffset (/ pi 180.0))))
                (while (>= brot (* 2.0 pi)) (setq brot (- brot (* 2.0 pi))))
                (while (< brot 0.0)         (setq brot (+ brot (* 2.0 pi))))
                (setq ent
                  (entmakex
                    (list '(0 . "INSERT")
                          (cons 8 lay)
                          (cons 2 DGPS-SymbolName)
                          (cons 10 (list (car pt) (cadr pt) z))
                          '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
                          (cons 50 brot))))
                (if ent
                  (progn
                    (setq cnt (1+ cnt))
                    (if (not (DGPS-SetDynProp ent DGPS-SymRotProp prop))
                      (progn
                        (setq DGPS-SymFail (1+ DGPS-SymFail))
                        (if (= DGPS-SymFail 1)
                          (princ (strcat "\n  Could not set block property \"" DGPS-SymRotProp
                                         "\". Dynamic properties found: "
                                         (DGPS-DynPropNames ent))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (setq DGPS-SymTotal (+ DGPS-SymTotal cnt))
  cnt
)

;; Reads the angle box in the open dialog. T = OK to continue.
(defun DGPS-AngleOK (/ txt)
  (setq txt (vl-string-trim " " (get_tile "edt_angle")))
  (cond
    ((= (get_tile "tg_angle") "0") T)          ; filter off - value not needed
    ((and (/= txt "")
          (= (vl-string-trim "0123456789" txt) "")
          (> (atoi txt) 0))
     (setq DGPS-AngleThreshold (float (atoi txt)))
     T
    )
    (T
     (alert "Angle must be a whole number (1 or more).")
     (mode_tile "edt_angle" 2)
     nil
    )
  )
)

;; ===========================================================================
;; WINDOW 01 - "Selected layers"      list of CSV codes (multiple selection)
;; Returns (selectedCodes) - selectedCodes nil = all codes; nil = cancelled.
;; ===========================================================================

;; Reads the Line Types part of window 02. T = OK to continue.
(defun DGPS-LineTypesOK (/ txt)
  (setq txt (vl-string-trim " " (get_tile "edt_off")))
  (cond
    ((and (= (get_tile "tg_poly") "0")
          (= (get_tile "tg_3d") "0")
          (= (get_tile "tg_center") "0"))
     (alert "Select at least one Line Type.")
     nil
    )
    ((and (= (get_tile "tg_center") "1")
          (or (not (DGPS-NumericP txt)) (<= (atof txt) 0.0)))
     (alert "Track offset must be a number greater than 0.")
     (mode_tile "edt_off" 2)
     nil
    )
    (T
     (setq DGPS-UsePoly    (= (get_tile "tg_poly")   "1")
           DGPS-Use3D      (= (get_tile "tg_3d")     "1")
           DGPS-UseCenter  (= (get_tile "tg_center") "1")
           DGPS-CenterSide (if (= (get_tile "pop_side") "1") "RHS" "LHS"))
     (if (DGPS-NumericP txt) (setq DGPS-CenterOffTxt txt))
     (setq DGPS-CenterOffset (atof DGPS-CenterOffTxt))
     T
    )
  )
)

(defun DGPS-LayerSelectDialog (codenames / dcl_id dlgName result
                                selectedList dcl_text f
                                autoIdx i sortedCodes dlgRet tkns nm)
  (setq dlgName (vl-filename-mktemp "dgpsLine" (getvar "TEMPPREFIX") ".dcl"))

  (setq dcl_text
    (strcat
      "dgps_layer_sel : dialog {\n"
      "  label = \"Selected layers\";\n"
      "  : list_box {\n"
      "    key             = \"lst_codes\";\n"
      "    height          = 20;\n"
      "    width           = 48;\n"
      "    fixed_width_font = false;\n"
      "    multiple_select  = true;\n"
      "    allow_accept     = false;\n"
      "  }\n"
      "  spacer_1;\n"
      "  ok_cancel;\n"
      "}\n"
    )
  )

  (setq f (open dlgName "w"))
  (write-line dcl_text f)
  (close f)

  (setq dcl_id (load_dialog dlgName))
  (setq result nil)

  (if (and dcl_id (new_dialog "dgps_layer_sel" dcl_id))
    (progn
      ;; Populate list - sorted ascending
      (setq sortedCodes (vl-sort codenames '(lambda (a b) (< (strcase a) (strcase b)))))
      (start_list "lst_codes" 3)
      (foreach c sortedCodes (add_list c))
      (end_list)

      ;; Auto-select  R, R1, R2 ... R<n>
      (setq autoIdx "" i 0)
      (foreach c sortedCodes
        (if (DGPS-AutoRP-P c)
          (setq autoIdx (strcat autoIdx (if (= autoIdx "") "" " ") (itoa i)))
        )
        (setq i (1+ i))
      )
      (if (/= autoIdx "") (set_tile "lst_codes" autoIdx))

      (setq selIdxStr "")
      (action_tile "accept"
        "(setq selIdxStr (get_tile \"lst_codes\"))(done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")

      (setq dlgRet (start_dialog))

      (if (= dlgRet 1)
        (progn
          (setq selectedList '())
          (if (and selIdxStr (/= (DGPS-Trim selIdxStr) ""))
            (progn
              (setq tkns (read (strcat "(" selIdxStr ")")))
              (foreach idx tkns
                (setq nm (nth idx sortedCodes))
                (if nm (setq selectedList (cons nm selectedList)))
              )
              (setq selectedList (reverse selectedList))
            )
          )
          (setq result (list selectedList))
        )
        (setq result nil)
      )
    )
    (progn
      (princ "\nVIDDGPSTOLINE: could not open the Selected layers window.")
      (setq result nil)
    )
  )

  (unload_dialog dcl_id)
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  result
)

;; ===========================================================================
;; WINDOW 02
;;   Line Types : Polyline / 3D Polyline / Polyline Center
;;   Settings   : Track offset [0.86] [LHS/RHS] / Angle Filter [5] / Use Local Time
;;   Layers     : Custom Layer [1-TRACK] / CSV Code
;;
;; Returns (types layMode layBase) or nil if cancelled.
;;   types   = list of "Polyline" "3Dpolyline" "Center"
;;   layMode = "custom" or "csv"
;;   layBase = custom layer name (blank -> "1-TRACK")
;; ===========================================================================
(defun DGPS-SettingsDialog (/ dcl_id dlgName result modeChoice customName
                              dcl_text f dlgRet)

  (setq dlgName (vl-filename-mktemp "dgpsOpt" (getvar "TEMPPREFIX") ".dcl"))

  (setq dcl_text
    (strcat
      "dgps_settings : dialog {\n"
      "  : boxed_column {\n"
      "    label = \"Line Types\";\n"
      "    alignment = left;\n"
      "    : toggle { key = \"tg_poly\";   label = \"Polyline\";        alignment = left; }\n"
      "    : toggle { key = \"tg_3d\";     label = \"3D Polyline\";     alignment = left; }\n"
      "    : toggle { key = \"tg_center\"; label = \"Polyline Center\"; alignment = left; }\n"
      "  }\n"
      "\n"
      "  : boxed_column {\n"
      "    label = \"Symbol\";\n"
      "    : toggle { key = \"tg_sym\"; label = \"Symbol\"; alignment = left; }\n"
      "    : row {\n"
      "      : text     { label = \"Symbol Name\"; width = 14; fixed_width = true; }\n"
      "      : edit_box { key = \"edt_sym\"; label = \"\"; edit_width = 24; }\n"
      "    }\n"
      "  }\n"
      "\n"
      "  : boxed_column {\n"
      "    label = \"Settings\";\n"
      "    : row {\n"
      "      : edit_box   { key = \"edt_off\";  label = \"Track offset:\"; edit_width = 8; }\n"
      "      : popup_list { key = \"pop_side\"; edit_width = 8; fixed_width = true; }\n"
      "    }\n"
      "    : row {\n"
      "      : toggle   { key = \"tg_angle\"; label = \"Angle Filter:\"; }\n"
      "      : edit_box { key = \"edt_angle\"; label = \"\"; edit_width = 5; width = 8; fixed_width = true; }\n"
      "    }\n"
      "    : toggle { key = \"tg_time\"; label = \"Use Local Time\"; }\n"
      "  }\n"
      "\n"
      "  : boxed_column {\n"
      "    label = \"Layers\";\n"
      "    : row {\n"
      "      : radio_button { key = \"rb_custom\"; label = \"Custom Layer:\"; value = \"1\"; }\n"
      "      : edit_box     { key = \"edt_custom\"; label = \"\"; edit_width = 24; }\n"
      "    }\n"
      "    : radio_button { key = \"rb_csv\"; label = \"CSV Code\"; value = \"0\"; }\n"
      "  }\n"
      "\n"
      "  spacer_1;\n"
      "  ok_cancel;\n"
      "}\n"
    )
  )

  (setq f (open dlgName "w"))
  (write-line dcl_text f)
  (close f)

  (setq dcl_id (load_dialog dlgName))
  (setq result nil)

  (if (and dcl_id (new_dialog "dgps_settings" dcl_id))
    (progn
      (setq modeChoice "custom")
      (setq customName DGPS-CustomLayerTxt)

      ;; Line Types (last used values)
      (set_tile "tg_poly"   (if DGPS-UsePoly   "1" "0"))
      (set_tile "tg_3d"     (if DGPS-Use3D     "1" "0"))
      (set_tile "tg_center" (if DGPS-UseCenter "1" "0"))

      ;; Symbol
      (set_tile "edt_sym" DGPS-SymbolName)
      (set_tile "tg_sym"  (if DGPS-UseSymbol "1" "0"))
      (mode_tile "edt_sym" (if DGPS-UseSymbol 0 1))

      ;; Settings
      (set_tile "edt_off" DGPS-CenterOffTxt)
      (start_list "pop_side")
      (add_list "LHS")
      (add_list "RHS")
      (end_list)
      (set_tile "pop_side" (if (= DGPS-CenterSide "RHS") "1" "0"))
      (mode_tile "edt_off"  (if DGPS-UseCenter 0 1))
      (mode_tile "pop_side" (if DGPS-UseCenter 0 1))
      (set_tile "tg_angle"  (if DGPS-UseAngle "1" "0"))
      (set_tile "edt_angle" (itoa (fix DGPS-AngleThreshold)))
      (mode_tile "edt_angle" (if DGPS-UseAngle 0 1))
      (set_tile "tg_time"   (if DGPS-UseTime "1" "0"))

      ;; Layers
      (set_tile "rb_custom" "1")
      (set_tile "rb_csv"    "0")
      (set_tile "edt_custom" customName)
      (mode_tile "edt_custom" 0)

      (action_tile "tg_center"
        (strcat
          "(mode_tile \"edt_off\"  (if (= $value \"1\") 0 1))"
          "(mode_tile \"pop_side\" (if (= $value \"1\") 0 1))"
        )
      )
      (action_tile "tg_sym"
        "(mode_tile \"edt_sym\" (if (= $value \"1\") 0 1))"
      )
      (action_tile "tg_angle"
        "(mode_tile \"edt_angle\" (if (= $value \"1\") 0 1))"
      )
      (action_tile "rb_custom"
        (strcat
          "(setq modeChoice \"custom\")"
          "(set_tile \"rb_custom\" \"1\")"
          "(set_tile \"rb_csv\"    \"0\")"
          "(mode_tile \"edt_custom\" 0)"
          "(mode_tile \"edt_custom\" 2)"
        )
      )
      (action_tile "rb_csv"
        (strcat
          "(setq modeChoice \"csv\")"
          "(set_tile \"rb_custom\" \"0\")"
          "(set_tile \"rb_csv\"    \"1\")"
          "(mode_tile \"edt_custom\" 1)"
        )
      )
      (action_tile "accept"
        (strcat
          "(if (and (DGPS-LineTypesOK) (DGPS-AngleOK) (DGPS-SymbolBoxOK)) (progn"
          "(setq DGPS-UseAngle (= (get_tile \"tg_angle\") \"1\"))"
          "(setq DGPS-UseTime  (= (get_tile \"tg_time\")  \"1\"))"
          "(setq customName (get_tile \"edt_custom\"))"
          "(done_dialog 1)))"
        )
      )
      (action_tile "cancel" "(done_dialog 0)")

      (setq dlgRet (start_dialog))

      (if (= dlgRet 1)
        (progn
          (if (= (DGPS-Trim customName) "")
            (setq customName "1-TRACK")
          )
          (setq DGPS-CustomLayerTxt (DGPS-Trim customName))
          (setq result
            (list
              (append
                (if DGPS-UsePoly   '("Polyline"))
                (if DGPS-Use3D     '("3Dpolyline"))
                (if DGPS-UseCenter '("Center"))
              )
              modeChoice
              (DGPS-Trim customName)
            )
          )
        )
        (setq result nil)
      )
    )
    (progn
      (princ "\nVIDDGPSTOLINE: could not open the settings window.")
      (setq result nil)
    )
  )

  (unload_dialog dcl_id)
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  result
)

;; ===========================================================================
;; DGPS-Dist2DPt  –  2-D distance between two raw (x y ...) point lists
;; ===========================================================================
(defun DGPS-Dist2DPt (a b / dx dy)
  (setq dx (- (car a) (car b))
        dy (- (cadr a) (cadr b)))
  (sqrt (+ (* dx dx) (* dy dy)))
)

;; ===========================================================================
;; DGPS-FormatDist  –  format a real as a string with 2 decimal places
;; ===========================================================================
(defun DGPS-FormatDist (d)
  ;; AutoLISP rtos: unit 2 = decimal, precision 2
  (rtos d 2 2)
)

;; ===========================================================================
;; Build geometry for one group of records
;;
;; Algorithm (spec §4-§9):
;;   1. Sort records by Local Time (row as tie-breaker).
;;   2. Build temporary closed loop:  P0→P1→…→Pn-1→P0
;;   3. Compute 2-D plan length of every edge.
;;   4. Find the edge with the greatest length.
;;   5. Remove that edge → open polyline starting from the vertex *after*
;;      the removed edge and traversing around to the vertex *before* it.
;;   6. Create LWPOLYLINE or 3D POLYLINE on targetLayer.
;;
;; Edge cases (spec §10):
;;   0 points → skip, report.
;;   1 point  → skip (no polyline).
;;   2 points → direct open line, no edge-removal step.
;;
;; Returns: (entity 0 singleCt failures)
;;   entity    = AutoCAD entity name, or nil
;;   0         = placeholder (was "moved" in v03, unused in v04)
;;   singleCt  = 1 if skipped because < 2 valid pts, else 0
;;   failures  = 1 if entmakex failed, else 0
;; ===========================================================================
;; ---------------------------------------------------------------------------
;; DGPS-NNChain   (v11: nearest-point chain, grows from BOTH ends)
;;   Old version walked from the first CSV row only, so a line numbered
;;   132..1 (start in the middle) 133..486 was joined 1->132 then jumped
;;   132->133 (zig-zag).
;;   New: start at the first record, then repeatedly attach the unvisited
;;   point that is nearest (2-D plan distance) to EITHER end of the chain,
;;   at that end.  Row / ascending order is never used for connecting.
;;   Result runs 132 ... 3 2 1 133 ... 486.  Direction is normalised so the
;;   lower CSV row number is at the start of the polyline.
;; ---------------------------------------------------------------------------
(defun DGPS-NNChain (recs / items start remaining headEnd tailEnd
                            headStack tailStack best bestD bestSide
                            it dh dt dx dy chain)
  (if (or (null recs) (null (cdr recs)))
    recs
    (progn
      ;; items = (x y rec)
      (setq items
        (mapcar
          '(lambda (r) (list (atof (DGPS-R-East r)) (atof (DGPS-R-North r)) r))
          recs))
      (setq start     (car items)
            remaining (cdr items)
            headEnd   start
            tailEnd   start
            headStack '()
            tailStack '())
      (while remaining
        (setq best nil bestD nil bestSide nil)
        (foreach it remaining
          ;; squared distance to head end / tail end
          (setq dx (- (car it) (car headEnd))
                dy (- (cadr it) (cadr headEnd))
                dh (+ (* dx dx) (* dy dy)))
          (setq dx (- (car it) (car tailEnd))
                dy (- (cadr it) (cadr tailEnd))
                dt (+ (* dx dx) (* dy dy)))
          (if (or (null bestD) (< dh bestD))
            (setq best it bestD dh bestSide 'HEAD))
          (if (< dt bestD)
            (setq best it bestD dt bestSide 'TAIL))
        )
        (if (eq bestSide 'HEAD)
          (setq headStack (cons best headStack)
                headEnd   best)
          (setq tailStack (cons best tailStack)
                tailEnd   best)
        )
        (setq remaining (vl-remove best remaining))
      )
      ;; head end ... start ... tail end
      (setq chain (append headStack (list start) (reverse tailStack)))
      ;; stable direction: lower CSV row first
      (if (> (DGPS-R-Row (caddr (car chain)))
             (DGPS-R-Row (caddr (last chain))))
        (setq chain (reverse chain)))
      (mapcar 'caddr chain)
    )
  )
)

;; ===========================================================================
;; HP (hectometre post) support
;; ===========================================================================

(defun DGPS-DigitP (c / a)
  (setq a (ascii c))
  (and (>= a 48) (<= a 57))
)

;; ---------------------------------------------------------------------------
;; DGPS-ParseHP
;;   Returns chainage in metres (km*1000 + m, as a real) when the text looks
;;   like an HP name, otherwise nil.
;;     HP 71/600  HP 71-600  HP 71_600  HP 71 600  HP71/600   (any case)
;; ---------------------------------------------------------------------------
(defun DGPS-ParseHP (s / u n i km m sepSeen)
  (if (and s (= (type s) 'STR))
    (progn
      (setq u (strcase (vl-string-trim " \t" s))
            n (strlen u))
      (if (and (>= n 4) (= (substr u 1 2) "HP"))
        (progn
          (setq i 3 km "" m "" sepSeen nil)
          (while (and (<= i n) (= (substr u i 1) " ")) (setq i (1+ i)))
          (while (and (<= i n) (DGPS-DigitP (substr u i 1)))
            (setq km (strcat km (substr u i 1)) i (1+ i)))
          (while (and (<= i n)
                      (vl-string-position (ascii (substr u i 1)) "/-_ \\"))
            (setq sepSeen T i (1+ i)))
          (while (and (<= i n) (DGPS-DigitP (substr u i 1)))
            (setq m (strcat m (substr u i 1)) i (1+ i)))
          (while (and (<= i n) (= (substr u i 1) " ")) (setq i (1+ i)))
          (if (and (> i n) (/= km "") (/= m "") sepSeen)
            (+ (* (atoi km) 1000.0) (atoi m))
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

;; ---------------------------------------------------------------------------
;; DGPS-HPBuildSamples
;;   Smooth curve through the HP points (cubic Hermite, tangent at each HP =
;;   average of the two neighbouring chord directions).  Returns a list of
;;   (x y z tx ty): sample position + unit tangent (in ascending HP order).
;;   pts = list of (x y z), at least 2, no coincident neighbours.
;; ---------------------------------------------------------------------------
(defun DGPS-HPBuildSamples (pts segs / n i j u dirs a b dx dy d h tt tt2 tt3
                                      p0 p1 d0 d1 m0x m0y m1x m1y
                                      h00 h10 h01 h11 g00 g10 g01 g11
                                      x y z tx ty out)
  (setq n (length pts))
  ;; unit chord vectors  (ux uy len)
  (setq u '() i 0)
  (while (< i (1- n))
    (setq a (nth i pts) b (nth (1+ i) pts)
          dx (- (car b) (car a)) dy (- (cadr b) (cadr a))
          d  (sqrt (+ (* dx dx) (* dy dy))))
    (setq u (append u (list (list (/ dx d) (/ dy d) d))))
    (setq i (1+ i))
  )
  ;; direction at every HP
  (setq dirs '() i 0)
  (while (< i n)
    (cond
      ((= i 0)       (setq dx (car (nth 0 u))       dy (cadr (nth 0 u))))
      ((= i (1- n))  (setq dx (car (nth (- n 2) u)) dy (cadr (nth (- n 2) u))))
      (T
       (setq dx (+ (car  (nth (1- i) u)) (car  (nth i u)))
             dy (+ (cadr (nth (1- i) u)) (cadr (nth i u))))
       (setq d (sqrt (+ (* dx dx) (* dy dy))))
       (if (< d 1e-9)
         (setq dx (car (nth i u)) dy (cadr (nth i u)))
         (setq dx (/ dx d) dy (/ dy d))
       )
      )
    )
    (setq dirs (append dirs (list (list dx dy))))
    (setq i (1+ i))
  )
  ;; sample every span
  (setq out '() i 0)
  (while (< i (1- n))
    (setq p0 (nth i pts) p1 (nth (1+ i) pts)
          h  (caddr (nth i u))
          d0 (nth i dirs) d1 (nth (1+ i) dirs)
          m0x (* h (car d0)) m0y (* h (cadr d0))
          m1x (* h (car d1)) m1y (* h (cadr d1)))
    (setq j 0)
    (while (< j segs)
      (setq tt  (/ (float j) segs)
            tt2 (* tt tt)
            tt3 (* tt2 tt))
      (setq h00 (+ (- (* 2.0 tt3) (* 3.0 tt2)) 1.0)
            h10 (+ (- tt3 (* 2.0 tt2)) tt)
            h01 (+ (* -2.0 tt3) (* 3.0 tt2))
            h11 (- tt3 tt2)
            g00 (- (* 6.0 tt2) (* 6.0 tt))
            g10 (+ (- (* 3.0 tt2) (* 4.0 tt)) 1.0)
            g01 (+ (* -6.0 tt2) (* 6.0 tt))
            g11 (- (* 3.0 tt2) (* 2.0 tt)))
      (setq x  (+ (* h00 (car p0))  (* h10 m0x) (* h01 (car p1))  (* h11 m1x))
            y  (+ (* h00 (cadr p0)) (* h10 m0y) (* h01 (cadr p1)) (* h11 m1y))
            z  (+ (caddr p0) (* tt (- (caddr p1) (caddr p0))))
            tx (+ (* g00 (car p0))  (* g10 m0x) (* g01 (car p1))  (* g11 m1x))
            ty (+ (* g00 (cadr p0)) (* g10 m0y) (* g01 (cadr p1)) (* g11 m1y)))
      (setq d (sqrt (+ (* tx tx) (* ty ty))))
      (if (< d 1e-12)
        (setq tx (car d0) ty (cadr d0))
        (setq tx (/ tx d) ty (/ ty d))
      )
      (setq out (cons (list x y z tx ty) out))
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  ;; last HP
  (setq p1 (nth (1- n) pts) d1 (nth (1- n) dirs))
  (setq out (cons (list (car p1) (cadr p1) (caddr p1) (car d1) (cadr d1)) out))
  (reverse out)
)

;; Unit tangent of the HP curve nearest to (x,y)  ->  (tx ty)  or nil
(defun DGPS-HPTangentAt (x y / best bestD d s)
  (setq best nil bestD nil)
  (foreach s DGPS-HPSamples
    (setq d (+ (* (- (car s) x) (- (car s) x))
               (* (- (cadr s) y) (- (cadr s) y))))
    (if (or (null bestD) (< d bestD))
      (setq best s bestD d)
    )
  )
  (if best (list (nth 3 best) (nth 4 best)) nil)
)

;; ---------------------------------------------------------------------------
;; DGPS-OrientAlongHP
;;   Returns pts in the direction of increasing HP chainage: each segment is
;;   compared with the HP-curve tangent nearest to it; if the polyline runs
;;   against it overall, the vertex list is reversed.
;;   No HP curve -> pts returned unchanged.
;; ---------------------------------------------------------------------------
(defun DGPS-OrientAlongHP (pts / n stride i p q mx my tg sum)
  (setq DGPS-LastFlipped nil)
  (if (and DGPS-HPSamples pts (>= (length pts) 2))
    (progn
      (setq n      (length pts)
            stride (max 1 (/ n 60))
            sum    0.0
            i      0)
      (while (< (1+ i) n)
        (setq p  (nth i pts)
              q  (nth (1+ i) pts)
              mx (/ (+ (car p)  (car q))  2.0)
              my (/ (+ (cadr p) (cadr q)) 2.0)
              tg (DGPS-HPTangentAt mx my))
        (if tg
          (setq sum (+ sum (* (- (car q)  (car p))  (car tg))
                           (* (- (cadr q) (cadr p)) (cadr tg))))
        )
        (setq i (+ i stride))
      )
      (if (< sum 0.0)
        (progn (setq DGPS-LastFlipped T) (reverse pts))
        pts
      )
    )
    pts
  )
)

;; ---------------------------------------------------------------------------
;; DGPS-RemoveHP
;;   Deletes the HP curve (polyline OR 3D polyline) and then the HP layer.
;;   Every HP object is first moved to layer 0, then erased - an old-style 3D
;;   polyline keeps its erased VERTEX / SEQEND records on the layer, which
;;   otherwise makes the layer "in use" and impossible to delete.
;; ---------------------------------------------------------------------------
(defun DGPS-MoveToZeroAndErase (en / e ed t0 done)
  (if (and en (entget en))
    (progn
      (if (= (cdr (assoc 0 (entget en))) "POLYLINE")
        (progn
          (setq e (entnext en) done nil)
          (while (and e (not done))
            (setq ed (entget e) t0 (cdr (assoc 0 ed)))
            (if (assoc 8 ed) (entmod (subst (cons 8 "0") (assoc 8 ed) ed)))
            (if (= t0 "SEQEND") (setq done T) (setq e (entnext e)))
          )
        )
      )
      (setq ed (entget en))
      (if (assoc 8 ed) (entmod (subst (cons 8 "0") (assoc 8 ed) ed)))
      (entdel en)
      T
    )
  )
)

(defun DGPS-RemoveHP (geom / lay lays obj ss i en n delErr)
  (setq lay DGPS-HPLayer n 0)
  ;; unlock + thaw so nothing is skipped; never leave it as the current layer
  (setq lays (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq obj  (vl-catch-all-apply 'vla-Item (list lays lay)))
  (if (not (vl-catch-all-error-p obj))
    (progn
      (vl-catch-all-apply 'vla-put-Lock   (list obj :vlax-false))
      (vl-catch-all-apply 'vla-put-Freeze (list obj :vlax-false))
    )
  )
  (if (= (strcase (getvar "CLAYER")) (strcase lay)) (setvar "CLAYER" "0"))

  ;; the HP curve made by this run
  (if (DGPS-MoveToZeroAndErase geom) (setq n (1+ n)))

  ;; anything else still on the HP layer (leftover HP curves)
  (setq ss (ssget "_X" (list (cons 8 lay))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (if (DGPS-MoveToZeroAndErase (ssname ss i)) (setq n (1+ n)))
        (setq i (1+ i))
      )
    )
  )
  (princ (strcat "\nHP curve removed (" (itoa n) " object(s))."))

  ;; delete the layer
  (if (tblsearch "LAYER" lay)
    (progn
      (setq obj (vl-catch-all-apply 'vla-Item (list lays lay)))
      (setq delErr
        (if (vl-catch-all-error-p obj)
          T
          (vl-catch-all-error-p (vl-catch-all-apply 'vla-Delete (list obj)))))
      (if (and delErr (tblsearch "LAYER" lay))
        (vl-catch-all-apply
          '(lambda () (command "_.-PURGE" "_LA" lay "_N")))
      )
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (if (tblsearch "LAYER" lay)
        (princ (strcat "\nLayer " lay " could not be deleted (still in use)."))
        (princ (strcat "\nLayer " lay " removed."))
      )
    )
  )
  T
)

;; ---------------------------------------------------------------------------
;; DGPS-BuildHP
;;   hpItems = list of (chainage rec).  Sorts ascending, builds the HP curve
;;   as a polyline / 3D polyline on DGPS-HPLayer and stores the samples for
;;   DGPS-OrientAlongHP.  Returns the entity or nil.
;; ---------------------------------------------------------------------------
(defun DGPS-BuildHP (hpItems outType / sorted pts prev x y z rec it
                                      samples ptsOut result firstN lastN)
  (setq sorted
    (vl-sort hpItems
      '(lambda (a b)
         (if (/= (car a) (car b))
           (< (car a) (car b))
           (< (DGPS-R-Row (cadr a)) (DGPS-R-Row (cadr b)))))))

  (setq pts '() prev nil)
  (foreach it sorted
    (setq rec (cadr it)
          x (atof (DGPS-R-East  rec))
          y (atof (DGPS-R-North rec))
          z (atof (DGPS-R-Elev  rec)))
    (if (or (null prev)
            (> (distance (list x y 0.0) (list (car prev) (cadr prev) 0.0)) 1e-6))
      (progn
        (setq pts (cons (list x y z) pts))
        (setq prev (list x y z))
      )
    )
  )
  (setq pts (reverse pts))

  (princ "\nHP (hectometre posts)")
  (princ (strcat "\n  HP points        : " (itoa (length sorted))))
  (setq firstN (DGPS-R-PName (cadr (car sorted)))
        lastN  (DGPS-R-PName (cadr (last sorted))))
  (princ (strcat "\n  Chainage order   : " firstN " ... " lastN))

  (if (< (length pts) 2)
    (progn
      (princ "\n  Result            : SKIPPED (need at least 2 HP points)")
      nil
    )
    (progn
      (DGPS-EnsureLayer DGPS-HPLayer 6)
      (setq samples (DGPS-HPBuildSamples pts DGPS-HPSegs))
      (setq DGPS-HPSamples samples)
      (setq ptsOut
        (if (= outType "3Dpolyline")
          (mapcar '(lambda (s) (list (car s) (cadr s) (caddr s))) samples)
          (mapcar '(lambda (s) (list (car s) (cadr s))) samples)))
      (setq result
        (if (= outType "3Dpolyline")
          (DGPS-Make3DPolyline ptsOut DGPS-HPLayer)
          (DGPS-Make2DPolyline ptsOut DGPS-HPLayer)))
      (princ (strcat "\n  Layer            : " DGPS-HPLayer))
      (princ (strcat "\n  Type             : " outType " (spline through HP)"))
      (princ (strcat "\n  Result            : " (if result "CREATED" "FAILED")))
      result
    )
  )
)

;; ---------------------------------------------------------------------------
;; DGPS-Bearing2D
;;   Returns the bearing in degrees (0–360) from point A to point B.
;;   A and B are (x y) or (x y z) lists.
;;   Returns 0.0 when A and B are coincident (avoid atan2 undefined).
;; ---------------------------------------------------------------------------
(defun DGPS-Bearing2D (a b / dx dy)
  (setq dx (- (car  b) (car  a))
        dy (- (cadr b) (cadr a)))
  (if (and (= dx 0.0) (= dy 0.0))
    0.0
    (progn
      ;; atan in AutoLISP: (atan y x) = standard atan2
      (setq ang (* (/ (atan dy dx) (* 4.0 (atan 1.0))) 180.0))
      ;; Convert maths angle (CCW from East) to bearing (CW from North)
      (setq ang (- 90.0 ang))
      ;; Normalise to [0, 360)
      (while (< ang   0.0) (setq ang (+ ang 360.0)))
      (while (>= ang 360.0) (setq ang (- ang 360.0)))
      ang
    )
  )
)

;; ---------------------------------------------------------------------------
;; DGPS-AngleDiff
;;   Smallest signed difference between two bearings, in degrees.
;;   Result is in (-180, +180].
;;   Handles the 0°/360° wrap correctly.
;;   Example: DGPS-AngleDiff(350, 5) → +15  (not -345)
;;            DGPS-AngleDiff(5, 350) → -15
;; ---------------------------------------------------------------------------
(defun DGPS-AngleDiff (b1 b2 / d)
  (setq d (- b2 b1))
  (while (>  d  180.0) (setq d (- d 360.0)))
  (while (<= d -180.0) (setq d (+ d 360.0)))
  d
)

;; ---------------------------------------------------------------------------
;; DGPS-AngleFilter
;;   Post-processing pass on an ordered list of 2-D (x y) or 3-D (x y z) pts.
;;   Removes interior vertices where the bearing change between the incoming
;;   and outgoing edge exceeds DGPS-AngleThreshold degrees (default 5°).
;;   Runs iteratively until no vertices are removed in a complete pass.
;;   First and last vertices are always preserved.
;;   Returns: (filteredPts removedCount)
;; ---------------------------------------------------------------------------
(if (null DGPS-AngleThreshold) (setq DGPS-AngleThreshold 5.0))

;; ---------------------------------------------------------------------------
;; DGPS-AngleFilter
;;
;; Removes interior spike vertices — GPS points that cause a sharp direction
;; change — by removing ONE vertex per pass (the worst offender), then
;; re-evaluating the shortened list. Repeats until stable.
;;
;; WHY one-per-pass (fixes the p4/p5/p6 cascade bug):
;;
;;   Consider:  p3 -- p4 -- p5(spike) -- p6 -- p7  (track goes straight)
;;
;;   If ALL vertices exceeding the threshold are removed in one pass, p4 and
;;   p6 are also flagged because their bearing to p5 looks sharp. But once
;;   p5 is removed, p4 and p6 reconnect cleanly and are NOT spikes.
;;
;;   Removing only the WORST spike per pass lets the list heal before the
;;   next evaluation. After p5 is gone, pass 2 sees p4--p6 as a straight
;;   segment and correctly keeps both.
;;
;; Algorithm per pass:
;;   1. Scan all interior vertices.
;;   2. For each, compute abs(bearing_change) = |diff(b_in, b_out)|.
;;   3. Track the vertex with the LARGEST bearing change that exceeds
;;      DGPS-AngleThreshold.
;;   4. If found, remove that one vertex and go to the next pass.
;;   5. If none found, done (stable).
;;
;; First and last vertices are always preserved.
;; Returns: (filteredPts totalRemovedCount)
;; ---------------------------------------------------------------------------
(defun DGPS-AngleFilter (pts / done totalRemoved
                               nPts i prev curr next
                               b1 b2 diff
                               worstIdx worstDiff
                               newPts)
  (setq totalRemoved 0
        done         nil)

  (while (not done)
    (setq nPts (length pts))

    (if (< nPts 3)
      (setq done T)  ; nothing left to filter

      (progn
        ;; Find the single worst spike in this pass
        (setq worstIdx  -1
              worstDiff DGPS-AngleThreshold  ; must EXCEED threshold to qualify
              i         1)

        (while (< i (1- nPts))
          (setq prev (nth (1- i) pts)
                curr (nth i       pts)
                next (nth (1+ i)  pts))

          ;; Nil-coord guard
          (if (and prev curr next
                   (car prev) (cadr prev)
                   (car curr) (cadr curr)
                   (car next) (cadr next))
            (progn
              (setq b1   (DGPS-Bearing2D prev curr)
                    b2   (DGPS-Bearing2D curr next)
                    diff (abs (DGPS-AngleDiff b1 b2)))
              ;; Keep track of the worst offender (strictly greater)
              (if (> diff worstDiff)
                (setq worstDiff diff  worstIdx i)
              )
            )
          )
          (setq i (1+ i))
        )

        (if (= worstIdx -1)
          ;; No spike found – list is stable
          (setq done T)

          ;; Remove the single worst vertex and loop again
          (progn
            (setq newPts '()
                  i       0)
            (while (< i nPts)
              (if (/= i worstIdx)
                (setq newPts (append newPts (list (nth i pts))))
              )
              (setq i (1+ i))
            )
            (setq pts           newPts
                  totalRemoved  (1+ totalRemoved))
          )
        )
      )
    )
  )

  (list pts totalRemoved)
)

;; ===========================================================================
;; Layer names / colours for the three line types
;;   Polyline         "1-TRACK SIDE"   yellow   (2)
;;   Polyline Center  "1-TRACK"        magenta  (6)
;;   3D Polyline      "1-TRACK 3D"     green    (3)
;; DGPS-TypeLayers returns (polyLayer centerLayer layer3D)
;; ===========================================================================
;; base ending in TRACK is already the Polyline Center layer name
(defun DGPS-TypeLayers (mode base / b)
  (setq b (DGPS-Trim base))
  (if (wcmatch (strcase b) "*TRACK")
    (list (DGPS-SanitizeLayerName (strcat b " SIDE"))
          (DGPS-SanitizeLayerName b)
          (DGPS-SanitizeLayerName (strcat b " 3D")))
    (list (DGPS-SanitizeLayerName (strcat b " TRACK SIDE"))
          (DGPS-SanitizeLayerName (strcat b " TRACK"))
          (DGPS-SanitizeLayerName (strcat b " TRACK 3D")))
  )
)

;; "Polyline, 3D Polyline, Polyline Center (LHS 0.86)"
(defun DGPS-TypesText (types side offTxt / out)
  (setq out "")
  (if (member "Polyline" types)
    (setq out (strcat out (if (= out "") "" ", ") "Polyline")))
  (if (member "3Dpolyline" types)
    (setq out (strcat out (if (= out "") "" ", ") "3D Polyline")))
  (if (member "Center" types)
    (setq out (strcat out (if (= out "") "" ", ")
                      "Polyline Center (" side " " offTxt ")")))
  out
)

;; ---------------------------------------------------------------------------
;; DGPS-OffsetCenter
;;   Offsets the 2D polyline `src` by `dist` to the LHS or RHS (looking along
;;   the polyline direction, first vertex -> last vertex) and puts the result
;;   on layer `lay`.  Returns the new entity name, or nil.
;;   Works on the longest segment so the side is unambiguous.
;; ---------------------------------------------------------------------------
(defun DGPS-OffsetCenter (src side dist lay / ed pts i p q len best bi
                                           dx dy nx ny sg mx my sidePt
                                           before new oe)
  (setq ed (entget src) pts '())
  (foreach d ed
    (if (= (car d) 10) (setq pts (append pts (list (cdr d)))))
  )
  (setq i 0 best -1.0 bi nil)
  (while (< (1+ i) (length pts))
    (setq p   (nth i pts)
          q   (nth (1+ i) pts)
          len (distance (list (car p) (cadr p)) (list (car q) (cadr q))))
    (if (> len best) (setq best len bi i))
    (setq i (1+ i))
  )
  (if (and bi (> best 1e-9))
    (progn
      (setq p  (nth bi pts)
            q  (nth (1+ bi) pts)
            dx (/ (- (car q) (car p)) best)
            dy (/ (- (cadr q) (cadr p)) best)
            nx (- dy)                      ; left-hand normal
            ny dx
            sg (if (= side "RHS") -1.0 1.0)
            mx (/ (+ (car p) (car q)) 2.0)
            my (/ (+ (cadr p) (cadr q)) 2.0)
            sidePt (list (+ mx (* sg nx dist)) (+ my (* sg ny dist)) 0.0))

      ;; make sure the source polyline is kept
      (setq oe (vl-catch-all-apply 'getvar (list "OFFSETERASE")))
      (if (vl-catch-all-error-p oe)
        (setq oe nil)
        (progn
          (setq dgps-*old-offerase* oe)
          (vl-catch-all-apply 'setvar (list "OFFSETERASE" 0))
        )
      )

      (setq before (entlast))
      (command "_.OFFSET" dist src (trans sidePt 0 1) "")
      (while (> (getvar "CMDACTIVE") 0) (command ""))

      (if oe
        (progn
          (vl-catch-all-apply 'setvar (list "OFFSETERASE" oe))
          (setq dgps-*old-offerase* nil)
        )
      )

      (setq new (entlast))
      (if (and new (not (equal new before)))
        (progn
          (setq ed (entget new))
          (entmod (subst (cons 8 lay) (assoc 8 ed) ed))
          new
        )
        nil
      )
    )
    nil
  )
)

;; ---------------------------------------------------------------------------
;; DGPS-EmitGeometry
;;   Creates the requested line types for one code.
;;   pts2D / pts3D : final ordered vertices (same order)
;;   types         : list of "Polyline" "3Dpolyline" "Center"
;;   lays          : (polyLayer centerLayer layer3D)
;;   Returns (polyEnt 3dEnt centerEnt made failed)
;; ---------------------------------------------------------------------------
(defun DGPS-EmitGeometry (pts2D pts3D types lays side offset
                          / pe te ce src tmp made fails)
  (setq made 0 fails 0)

  ;; Polyline (2D)
  (if (member "Polyline" types)
    (progn
      (DGPS-EnsureLayer (nth 0 lays) 2)
      (setq pe (DGPS-Make2DPolyline pts2D (nth 0 lays)))
      (if pe (setq made (1+ made)) (setq fails (1+ fails)))
    )
  )

  ;; Polyline Center = 2D polyline offset LHS / RHS
  (if (member "Center" types)
    (progn
      (DGPS-EnsureLayer (nth 1 lays) 6)
      (setq src pe tmp nil)
      (if (null src)
        ;; [Polyline] not ticked: temporary polyline on layer 0, erased afterwards
        (setq src (DGPS-Make2DPolyline pts2D "0") tmp T)
      )
      (if src
        (setq ce (DGPS-OffsetCenter src side offset (nth 1 lays)))
      )
      (if (and tmp src) (DGPS-MoveToZeroAndErase src))
      (if ce (setq made (1+ made)) (setq fails (1+ fails)))
    )
  )

  ;; 3D Polyline
  (if (member "3Dpolyline" types)
    (progn
      (DGPS-EnsureLayer (nth 2 lays) 3)
      (setq te (DGPS-Make3DPolyline pts3D (nth 2 lays)))
      (if te (setq made (1+ made)) (setq fails (1+ fails)))
    )
  )

  (list pe te ce made fails)
)

;; ===========================================================================
;; Build geometry for one group of records
;;
;; Pipeline:
;;   1. Nearest-neighbour chain  (spatial ordering, no zig-zag)
;;   2. Longest-edge removal     (open the loop at the biggest gap)
;;   3. Angular-deviation filter (iteratively remove bad GPS vertices)
;;   4. Create the ticked line types (Polyline / Polyline Center / 3D Polyline)
;;
;; Edge cases:
;;   0 points -> skip
;;   1 point  -> skip
;;   2 points -> direct open line (no loop removal or angle filter)
;;
;; Returns: (created 0 singleCt failures)
;; ===========================================================================
(defun DGPS-BuildGeometry (codeRecs code types lays side offset
                           / chainedRecs nPts i x y z
                             allPts3D allPts2D
                             loopPts loopLen
                             maxLen maxIdx edgeLen
                             fromPt toPt startIdx
                             openPts3D openPts2D
                             pairedPts only2D survivedPairs filterQueue
                             filterResult filteredPts removedCt
                             final2D final3D emit made fails
                             fromName toName symCt symCe symSrc symTmp symLay)

  (setq nPts (length codeRecs))

  (cond

    ;; -- 0 points -----------------------------------------------------------
    ((= nPts 0)
     (princ (strcat "\n" code))
     (princ         "\n  Points           : 0")
     (princ         "\n  Result            : SKIPPED (no valid points)")
     (list nil 0 1 0)
    )

    ;; -- 1 point ------------------------------------------------------------
    ((= nPts 1)
     (princ (strcat "\n" code))
     (princ         "\n  Points           : 1")
     (princ         "\n  Result            : SKIPPED (single point, no polyline)")
     (list nil 0 1 0)
    )

    ;; -- 2+ points ----------------------------------------------------------
    (T
     ;; Local Time ON  -> sort by Local Time (row = tie-breaker), join in order
     ;; Local Time OFF -> nearest-neighbour chain
     (setq chainedRecs
       (if DGPS-UseTime
         (vl-sort codeRecs '(lambda (a b) (DGPS-TimeLess a b)))
         (DGPS-NNChain codeRecs)
       )
     )
     (setq nPts (length chainedRecs))

     ;; Build coordinate arrays in chained order
     (setq allPts3D '()
           allPts2D '())
     (foreach rec chainedRecs
       (setq x (atof (DGPS-R-East  rec))
             y (atof (DGPS-R-North rec))
             z (atof (DGPS-R-Elev  rec)))
       (setq allPts3D (append allPts3D (list (list x y z))))
       (setq allPts2D (append allPts2D (list (list x y))))
     )

     (if (= nPts 2)

       ;; -- 2 points: direct open line, skip loop removal + angle filter ----
       (setq openPts3D allPts3D
             openPts2D allPts2D
             removedCt 0)

       ;; -- 3+ points ------------------------------------------------------
       (progn

         ;; Step 2: Longest-edge removal (open the loop)
         (if DGPS-UseTime
           ;; Local Time order: open polyline in time order, nothing removed
           (setq openPts3D allPts3D
                 openPts2D allPts2D
                 maxLen    0.0
                 fromName  "NONE"
                 toName    "NONE")
           (progn
             (setq loopPts allPts2D
                   loopLen nPts
                   maxLen  -1.0
                   maxIdx   0
                   i        0)

             (while (< i loopLen)
               (setq fromPt  (nth i loopPts)
                     toPt    (nth (rem (1+ i) loopLen) loopPts)
                     edgeLen (DGPS-Dist2DPt fromPt toPt))
               (if (> edgeLen maxLen)
                 (setq maxLen edgeLen  maxIdx i)
               )
               (setq i (1+ i))
             )

             ;; Re-order starting from vertex after the longest edge
             (setq startIdx (rem (1+ maxIdx) nPts))
             (setq openPts3D '()
                   openPts2D '()
                   i          0)
             (while (< i nPts)
               (setq openPts3D
                 (append openPts3D
                   (list (nth (rem (+ startIdx i) nPts) allPts3D))))
               (setq openPts2D
                 (append openPts2D
                   (list (nth (rem (+ startIdx i) nPts) allPts2D))))
               (setq i (1+ i))
             )

             ;; Names of the removed edge endpoints (for report)
             (setq fromName (DGPS-R-PName (nth maxIdx chainedRecs)))
             (setq toName   (DGPS-R-PName (nth (rem (1+ maxIdx) nPts) chainedRecs)))
           )
         )

         ;; Step 3: Angular-deviation filter - run on paired (2D . 3D) cells so
         ;; both lists stay in sync.
         (setq pairedPts '()
               i          0)
         (while (< i nPts)
           (setq pairedPts
             (append pairedPts
               (list (cons (nth i openPts2D) (nth i openPts3D)))))
           (setq i (1+ i))
         )
         (setq only2D (mapcar 'car pairedPts))

         (setq filterResult
           (if DGPS-UseAngle
             (DGPS-AngleFilter only2D)
             (list only2D 0)))
         (setq filteredPts (car  filterResult)
               removedCt   (cadr filterResult))

         ;; Rebuild paired list keeping only survivors (walk in parallel)
         (if (> removedCt 0)
           (progn
             (setq survivedPairs '()
                   filterQueue   filteredPts)
             (foreach pair pairedPts
               (if (and filterQueue
                        (equal (car pair) (car filterQueue) 1e-9))
                 (progn
                   (setq survivedPairs (append survivedPairs (list pair)))
                   (setq filterQueue   (cdr filterQueue))
                 )
               )
             )
             (setq openPts2D (mapcar 'car survivedPairs))
             (setq openPts3D (mapcar 'cdr survivedPairs))
           )
         )
       )
     )

     ;; Step 4: direction along the HP chainage (same flip for 2D and 3D),
     ;;         then create the ticked line types.
     (setq final2D (DGPS-OrientAlongHP openPts2D))
     (setq final3D (if DGPS-LastFlipped (reverse openPts3D) openPts3D))

     (setq emit  (DGPS-EmitGeometry final2D final3D types lays side offset))
     (setq made  (nth 3 emit)
           fails (nth 4 emit))

     ;; Symbol on the TRACK CENTER polyline, at the end with a BS / END point
     (setq symCt 0)
     (if (and DGPS-SymbolOK (DGPS-HasMarker chainedRecs))
       (progn
         (setq symCe (nth 2 emit) symTmp nil)
         (if (null symCe)
           (progn
             ;; Polyline Center not ticked: temporary center polyline,
             ;; erased again after the symbols are placed
             (setq symSrc (DGPS-Make2DPolyline final2D "0"))
             (if symSrc
               (progn
                 (setq symCe (DGPS-OffsetCenter symSrc side offset "0")
                       symTmp T)
                 (DGPS-MoveToZeroAndErase symSrc)
               )
             )
           )
         )
         (setq symLay
           (cond ((member "Center"   types) (nth 1 lays))
                 ((member "Polyline" types) (nth 0 lays))
                 (T                         (nth 2 lays))))
         (if symCe
           (setq symCt (DGPS-PlaceSymbols chainedRecs symCe symLay)))
         (if (and symTmp symCe) (DGPS-MoveToZeroAndErase symCe))
       )
     )

     ;; Report
     (princ (strcat "\n" code))
     (princ (strcat "\n  Points (chained) : " (itoa nPts)))
     (princ (strcat "\n  Order            : " (if DGPS-UseTime "LOCAL TIME" "NEAREST POINT (both ends)")))
     (if (= nPts 2)
       (progn
         (princ "\n  Longest Edge     : N/A (2-point direct line)")
         (princ "\n  Removed Edge     : NONE")
         (princ "\n  Angle Filter     : N/A")
       )
       (progn
         (princ (strcat "\n  Longest Edge     : "
                        (if DGPS-UseTime "N/A (Local Time order)" (DGPS-FormatDist maxLen))))
         (princ (strcat "\n  Removed Edge     : "
                        (if DGPS-UseTime "NONE" (strcat fromName " -> " toName))))
         (princ
           (if DGPS-UseAngle
             (strcat "\n  Angle Filter     : " (itoa removedCt)
                     " vertex(es) removed  (threshold "
                     (rtos DGPS-AngleThreshold 2 1) (chr 176) ")")
             "\n  Angle Filter     : OFF"))
       )
     )
     (princ (strcat "\n  Final Points     : " (itoa (length final2D))))
     (if (> symCt 0)
       (princ (strcat "\n  Symbols placed   : " (itoa symCt) "  (" DGPS-SymbolName ")")))
     (if (member "Polyline" types)
       (princ (strcat "\n  Polyline layer   : " (nth 0 lays))))
     (if (member "Center" types)
       (princ (strcat "\n  Center layer     : " (nth 1 lays)
                      "  (" side " " (vl-princ-to-string offset) ")")))
     (if (member "3Dpolyline" types)
       (princ (strcat "\n  3D layer         : " (nth 2 lays))))
     (princ (strcat "\n  Direction        : "
                    (if DGPS-HPSamples
                      (if DGPS-LastFlipped "REVERSED to follow HP" "already along HP")
                      "N/A (no HP rows)")))
     (princ (strcat "\n  Result            : "
                    (cond ((= fails 0) "CREATED")
                          ((> made 0)  "PARTIAL (some objects failed)")
                          (T           "FAILED"))))

     (list (if (> made 0) T nil) 0 0 fails)
    )
  )
)

;; ===========================================================================
;; Main command
;; ===========================================================================
(defun c:VIDDGPSTOLINE
  (/ csvFile openResult enc fh
     headerLine headers
     idxP idxC idxN idxE idxZ idxT
     line fields rowNo dataRows validRows skippedRows
     allRev allRecords
     rec pname code north east elev localTime key
     groups codenames
     dlgResult types selectedCodes layOpt layMode layBase lays codeLays
     cSide cOff
     g codeRecs geomResult
     geomCount singleCount geomFailures totalMoved
     layersCreated i sampleCount lyr
     hpRecs otherRecs hpCh hpGeom symDwg mergedGroups selRecs)

  (setq dgps-*old-error* *error*)
  (setq *error* DGPS-Error)

  (setq dgps-*old-clayer*  (getvar "CLAYER"))
  (setq dgps-*old-cmdecho* (getvar "CMDECHO"))
  (setq dgps-*old-osmode*  (getvar "OSMODE"))

  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)

  ;; -------------------------------------------------------------------------
  ;; 1. Select CSV file
  ;; -------------------------------------------------------------------------
  (setq csvFile
    (if (boundp 'LogicDWG:RequireCsv)
      (LogicDWG:RequireCsv)                              ; main CSV
      (getfiled "Select DGPS CSV File" "" "csv" 4)       ; fallback
    )
  )
  (if (null csvFile)
    (progn (DGPS-Error "No CSV file selected.") (exit))
  )

  ;; -------------------------------------------------------------------------
  ;; 2. Open and read CSV
  ;; -------------------------------------------------------------------------
  (setq openResult (DGPS-OpenCSV csvFile))
  (setq fh  (car  openResult))
  (setq enc (cadr openResult))
  (if (null fh)
    (progn (DGPS-Error "Could not open CSV in binary mode.") (exit))
  )
  (setq dgps-*fh* fh)

  ;; Header
  (setq headerLine (DGPS-ReadRecord fh))
  (if (null headerLine)
    (progn (DGPS-Error "CSV file is empty.") (exit))
  )
  (setq headerLine (DGPS-StripBOM headerLine))
  (setq headers    (DGPS-ParseCSV headerLine))

  (setq idxP (DGPS-HeaderIndex headers "Point Name"))
  (setq idxC (DGPS-HeaderIndex headers "Code"))
  (setq idxN (DGPS-HeaderIndex headers "Northing"))
  (setq idxE (DGPS-HeaderIndex headers "Easting"))
  (setq idxZ (DGPS-HeaderIndex headers "Elevation"))
  (setq idxT (DGPS-HeaderIndex headers "Local Time"))

  (if (or (null idxP) (null idxC) (null idxN) (null idxE) (null idxZ))
    (progn
      (DGPS-Error
        "Required headers missing. Need: Point Name, Code, Northing, Easting, Elevation.")
      (exit)
    )
  )

  ;; Read all rows
  (setq rowNo      1
        dataRows   0
        validRows  0
        skippedRows 0
        allRev     '())

  (while (setq line (DGPS-ReadRecord fh))
    (setq rowNo (1+ rowNo))
    (if (not (DGPS-BlankP line))
      (progn
        (setq dataRows (1+ dataRows))
        (setq fields    (DGPS-ParseCSV line))
        (setq pname     (DGPS-GetField fields idxP))
        (setq code      (DGPS-GetField fields idxC))
        (setq north     (DGPS-GetField fields idxN))
        (setq east      (DGPS-GetField fields idxE))
        (setq elev      (DGPS-GetField fields idxZ))
        (setq localTime (if (null idxT) "" (DGPS-GetField fields idxT)))

        (if (and (/= pname "")
                 (/= code  "")
                 (DGPS-NumericP north)
                 (DGPS-NumericP east)
                 (DGPS-NumericP elev))
          (progn
            (setq key    (DGPS-TimeKey localTime))
            (setq allRev
              (cons
                (list pname code north east elev localTime rowNo key)
                allRev
              )
            )
            (setq validRows (1+ validRows))
          )
          (setq skippedRows (1+ skippedRows))
        )
      )
    )
  )

  (close fh)
  (setq dgps-*fh* nil)
  (setq allRecords (reverse allRev))

  (if (= validRows 0)
    (progn (DGPS-Error "No valid records found in CSV.") (exit))
  )

  ;; Group and extract code names
  ;; HP rows (HP 71/600, HP 71-600, HP 71_600, HP 71 600) are picked out
  ;; automatically - they never appear in the layer list.
  (setq hpRecs '() otherRecs '())
  (foreach rec allRecords
    (setq hpCh
      (cond
        ((DGPS-ParseHP (DGPS-R-PName rec)))
        ((DGPS-ParseHP (DGPS-R-Code  rec)))
        (T nil)
      )
    )
    (if hpCh
      (setq hpRecs (cons (list hpCh rec) hpRecs))
      (setq otherRecs (cons rec otherRecs))
    )
  )
  (setq hpRecs    (reverse hpRecs)
        otherRecs (reverse otherRecs))
  (setq groups    (DGPS-GroupByCode otherRecs))
  (setq codenames '())
  (foreach g groups
    (setq codenames (append codenames (list (car g))))
  )

  ;; -------------------------------------------------------------------------
  ;; 3. Window 01: Selected layers
  ;; -------------------------------------------------------------------------
  (setq dlgResult (DGPS-LayerSelectDialog codenames))
  (if (null dlgResult)
    (progn (DGPS-Error "Cancelled by user.") (exit))
  )

  (setq selectedCodes (car dlgResult))

  ;; -------------------------------------------------------------------------
  ;; 4. Window 02: Line Types / Settings / Layers
  ;; -------------------------------------------------------------------------
  (setq layOpt (DGPS-SettingsDialog))
  (if (null layOpt)
    (progn (DGPS-Error "Cancelled by user.") (exit))
  )
  (setq types   (nth 0 layOpt))     ; "Polyline" "3Dpolyline" "Center"
  (setq layMode (nth 1 layOpt))     ; "custom" / "csv"
  (setq layBase (nth 2 layOpt))     ; custom layer name
  (setq cSide   DGPS-CenterSide)
  (setq cOff    DGPS-CenterOffset)

  ;; Layers for Polyline / Polyline Center / 3D Polyline
  ;; (CSV mode builds them per Code inside the loop below)
  (setq lays (DGPS-TypeLayers layMode layBase))

  ;; Symbol: import the block when a Symbol Name was typed (blank = none)
  (setq DGPS-SymbolOK nil DGPS-SymTotal 0 DGPS-SymFail 0)
  (if (and DGPS-UseSymbol (/= DGPS-SymbolName ""))
    (if (tblsearch "BLOCK" DGPS-SymbolName)
      (setq DGPS-SymbolOK T)
      (progn
        (setq symDwg (DGPS-FindBlocksDwg))
        (if (and symDwg (DGPS-ImportBlock symDwg DGPS-SymbolName))
          (setq DGPS-SymbolOK T)
          (princ (strcat "\nSymbol \"" DGPS-SymbolName
                         "\" not found - no symbols will be placed."))
        )
      )
    )
  )

  ;; Merge the selected codes: R1 + R1 BS / R3 + R3 END / R5 + R5 BS + R5 END
  ;; (case, spaces, BS and END ignored) -> one line per base code
  (setq selRecs '())
  (foreach rec otherRecs
    (if (or (null selectedCodes)
            (= (length selectedCodes) 0)
            (member (DGPS-R-Code rec) selectedCodes))
      (setq selRecs (cons rec selRecs))
    )
  )
  (setq selRecs      (reverse selRecs))
  (setq mergedGroups (DGPS-GroupMerged selRecs))

  ;; -------------------------------------------------------------------------
  ;; 5. Report input summary
  ;; -------------------------------------------------------------------------
  (princ "\n========================================")
  (princ "\nVIDDGPSTOLINE CSV IMPORT")
  (princ "\n========================================")
  (princ (strcat "\nFile:         " csvFile))
  (princ (strcat "\nEncoding:     " enc))
  (princ (strcat "\nLine types:   " (DGPS-TypesText types cSide DGPS-CenterOffTxt)))
  (if (= layMode "csv")
    (princ "\nLayers:       <Code> TRACK SIDE / <Code> TRACK / <Code> TRACK 3D (from CSV)")
    (progn
      (if (member "Polyline"   types) (princ (strcat "\nLayer (Polyline):        " (nth 0 lays))))
      (if (member "Center"     types) (princ (strcat "\nLayer (Polyline Center): " (nth 1 lays))))
      (if (member "3Dpolyline" types) (princ (strcat "\nLayer (3D Polyline):     " (nth 2 lays))))
    )
  )
  (princ (strcat "\nAngle filter: " (if DGPS-UseAngle
                                        (strcat "ON (+/- " (rtos DGPS-AngleThreshold 2 0) " deg)")
                                        "OFF")))
  (princ (strcat "\nPoint order:  " (if DGPS-UseTime "Local Time" "Nearest point")))
  (princ (strcat "\nData rows:    " (itoa dataRows)))
  (princ (strcat "\nValid:        " (itoa validRows)))
  (princ (strcat "\nSkipped:      " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes: " (itoa (length groups))))
  (princ (strcat "\nLines (BS / END merged): " (itoa (length mergedGroups))))
  (princ (strcat "\nSymbol:       "
                 (cond ((or (not DGPS-UseSymbol) (= DGPS-SymbolName "")) "none")
                       (DGPS-SymbolOK DGPS-SymbolName)
                       (T (strcat DGPS-SymbolName " (not available)")))))

  (if (and selectedCodes (> (length selectedCodes) 0))
    (progn
      (princ "\nFiltered to codes:")
      (foreach c selectedCodes (princ (strcat "\n  " c)))
    )
    (princ "\nProcessing ALL codes.")
  )

  (princ "\n----------------------------------------")
  (foreach g groups
    (princ (strcat "\n  " (car g) " -> " (itoa (length (cdr g))) " pts"))
  )

  ;; Sample records
  (princ "\n\nFIRST FIVE RECORDS")
  (setq sampleCount (min 5 (length allRecords)) i 0)
  (while (< i sampleCount)
    (setq rec (nth i allRecords))
    (princ
      (strcat
        "\n" (itoa (1+ i))
        ": " (DGPS-R-PName rec)
        " | " (DGPS-R-Code rec)
        " | E=" (DGPS-R-East  rec)
        " | N=" (DGPS-R-North rec)
        " | Z=" (DGPS-R-Elev  rec)
        " | T=" (DGPS-R-Time  rec)
      )
    )
    (setq i (1+ i))
  )

  ;; -------------------------------------------------------------------------
  ;; 6. Build geometry
  ;; -------------------------------------------------------------------------
  (setq geomCount    0
        singleCount  0
        geomFailures 0
        totalMoved   0
        layersCreated '())

  ;; HP curve first, so its tangent can orient the track polylines
  (setq DGPS-HPSamples nil)
  (setq hpGeom nil)
  (if hpRecs
    (setq hpGeom (DGPS-BuildHP hpRecs "Polyline"))
  )

  (foreach g mergedGroups
    (setq code     (car  g))
    (setq codeRecs (cdr  g))

    ;; the groups were already filtered by the selected codes (see above)
    (if T
      (progn
        (setq codeLays
          (if (= layMode "csv")
            (DGPS-TypeLayers "csv" code)      ; <Code> TRACK SIDE / TRACK / TRACK 3D
            lays
          )
        )
        (setq geomResult
          (DGPS-BuildGeometry codeRecs code types codeLays cSide cOff)
        )
        (setq totalMoved   (+ totalMoved   (nth 1 geomResult)))
        (setq singleCount  (+ singleCount  (nth 2 geomResult)))
        (setq geomFailures (+ geomFailures (nth 3 geomResult)))
        (if (nth 0 geomResult)
          (setq geomCount (1+ geomCount))
        )
        (foreach pr '(("Polyline" . 0) ("Center" . 1) ("3Dpolyline" . 2))
          (if (member (car pr) types)
            (progn
              (setq lyr (nth (cdr pr) codeLays))
              (if (null (member lyr layersCreated))
                (setq layersCreated (cons lyr layersCreated))
              )
            )
          )
        )
      )
      ;; Skipped (not in filter)
      (princ (strcat "\nSkipped (not selected): " code))
    )
  )

  ;; Remove the temporary HP spline and the HP layer (2D and 3D polyline mode)
  (if (and hpRecs (not DGPS-HPKeep))
    (DGPS-RemoveHP hpGeom)
  )
  (setq DGPS-HPSamples nil)

  ;; -------------------------------------------------------------------------
  ;; 7. Final report
  ;; -------------------------------------------------------------------------
  (princ "\n\n========================================")
  (princ "\nVIDDGPSTOLINE COMPLETE")
  (princ "\n========================================")
  (princ (strcat "\nLine types:       " (DGPS-TypesText types cSide DGPS-CenterOffTxt)))
  (princ (strcat "\nLayers used:      " (itoa (length layersCreated))))
  (princ (strcat "\nRows read:        " (itoa dataRows)))
  (princ (strcat "\nValid records:    " (itoa validRows)))
  (princ (strcat "\nSkipped rows:     " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes:     " (itoa (length groups))))
  (princ (strcat "\nGeometry created: " (itoa geomCount)))
  (princ (strcat "\nGeometry failed:  " (itoa geomFailures)))
  (princ (strcat "\nSingle-point:     " (itoa singleCount)))
  (princ (strcat "\nSymbol offsets:   block +" (rtos DGPS-SymRotOffset 2 0)
                 " deg, bufferStop_rot +" (rtos DGPS-SymPropOffset 2 0) " deg"))
  (princ (strcat "\nSymbols placed:   " (itoa DGPS-SymTotal)
                 (if (and DGPS-UseSymbol (/= DGPS-SymbolName "")) (strcat "  (" DGPS-SymbolName ")") "")))
  (if (> DGPS-SymFail 0)
    (princ (strcat "\nSymbol property \"" DGPS-SymRotProp "\" not set on "
                   (itoa DGPS-SymFail) " block(s).")))
  (princ (strcat "\nHP rows:          " (itoa (length hpRecs))
                 (if hpGeom (if DGPS-HPKeep "  (HP curve kept)" "  (HP curve used, then removed)") "")))
  (princ (strcat "\nAlgorithm:        "
                 (if DGPS-UseTime
                   "Local Time order"
                   "NN-chain + longest-edge removal")
                 (if DGPS-UseAngle " + angle filter" "")))

  (if (> geomFailures 0)
    (princ "\nWARNING: One or more geometry entities failed to create.")
  )
  (princ "\n========================================")

  ;; Restore AutoCAD state
  (setvar "CLAYER"  dgps-*old-clayer*)
  (setvar "CMDECHO" dgps-*old-cmdecho*)
  (setvar "OSMODE"  dgps-*old-osmode*)
  (setq *error* dgps-*old-error*)
  (princ "\nVIDDGPSTOLINE finished successfully.")
  (princ)
)

(princ "\nVIDDGPSTOLINE_v28 loaded. Type VIDDGPSTOLINE to run.")
(princ)
