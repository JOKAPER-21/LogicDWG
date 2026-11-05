;;; ============================================================================
;;; vidLC.lsp                                           Command : VIDLC
;;; ----------------------------------------------------------------------------
;;; Places level-crossing blocks from the DGPS CSV:
;;;    - gate blocks       (default "vid_lc_gate_001")
;;;    - height gauge blocks (default "vid_lc_heightGuage_001")
;;;
;;;   1. Select the DGPS CSV file  (DYNMODE = OFF only while selecting it,
;;;      DYNMODE = ON afterwards; the original value is restored at the end).
;;;   2. WINDOW 01  "Selected layers" - list of the CSV codes (multiple
;;;      selection).  Selected by default (auto detect):
;;;        - GATE / LOCK codes  (gate, Gate, gates, lock, gatelock, gate lock)
;;;        - height gauge codes (height guage, heightguage, heightgauge,
;;;          height gauge, HEIGHT GUAGE ... = HEIGHT + GUAGE/GAUGE)
;;;      Codes containing LODGE are ignored by the auto detect.
;;;   3. WINDOW 02  "LC"
;;;        Settings
;;;          LC Symbol            [vid_lc_gate_001]
;;;          Height Guage Symbol  [vid_lc_heightGuage_001]
;;;          Measurement          [ ] (default off)   [Nearest / LHS / RHS / Both Sides]
;;;        Track Polyline section (height gauge order and Measurement):
;;;          (o) Select layers  - every layer of the drawing
;;;          ( ) Manual Selection - pick the track polyline(s) in the drawing
;;;
;;;      MEASUREMENT (gate blocks "vid_lc_gate_NNN"): the gate blocks of one level
;;;      crossing form a GROUP of 2, 3 (rarely 4) blocks around the selected track:
;;;            vid_lc_gate_NNN (LHS)   |  track  |   vid_lc_gate_NNN (RHS)
;;;      Gates closer than LC-GroupDist (30 m) belong to the same group.  The
;;;      MIDPOINTS of the segments of the polyline inside a block are measured
;;;      perpendicular to the track with a DIMALIGNED on layer "1-LC".
;;;      Sides are seen from the track (looking along the track as drawn):
;;;        Nearest    : only the block of the group that is nearest to the track
;;;        LHS        : only the LHS block(s) of the group
;;;        RHS        : only the RHS block(s) of the group
;;;        Both Sides : the LHS and the RHS blocks of the group
;;;   4. The blocks are imported from vid_blocks_tools.dwg (same path lookup
;;;      as vidBankCutting.lsp).
;;;   5. Selected codes are split:
;;;        height gauge code   -> HEIGHT GAUGE point
;;;        code contains LOCK  -> LOCK point
;;;        any other selected  -> GATE point
;;;
;;;      GATE: the block is inserted at every GATE point.  The nearest LOCK
;;;      within LC-LockDist of that GATE is used:
;;;        "lc_gate_rot"  = angle GATE -> LOCK    (block faces the LOCK)
;;;        "lc_gate_move" = distance GATE -> LOCK (block reaches the LOCK)
;;;      A GATE without a LOCK nearby keeps the default block values.
;;;
;;;      HEIGHT GAUGE: the points are connected in pairs (nearest two points,
;;;      within LC-HGDist).  For every pair the block is inserted at the first
;;;      point and
;;;        "lc_heightGuage_rot"  = angle to the next height gauge point
;;;        "lc_heightGuage_move" = distance to the next height gauge point
;;;      ODD number of points nearby (e.g. 3): the point left over without a
;;;      partner gets its own block, connected to its nearest height gauge
;;;      point.  3 points -> 2 blocks:
;;;            p1 ---<block1>--- p2
;;;                               |
;;;                           <block2>
;;;                               |
;;;                              p3
;;;      A point with no other height gauge point within LC-HGDist is
;;;      reported and skipped.
;;;
;;;      TANGENT ORDER (needs the track polyline from window 02): for every
;;;      connection the track tangent at the nearest track point decides the
;;;      side and the order.  The tangent is read as drawn - the selected
;;;      polylines are NEVER changed or reversed:
;;;        RHS (right of the track) : first point = first along the tangent,
;;;                                   the block is moved there, then rotated
;;;                                   / moved to the next nearby point
;;;        LHS (left of the track)  : the order is switched - first point =
;;;                                   last along the tangent
;;;      Without a track polyline the CSV order is used.
;;;   6. The whole run is ONE undo step.
;;;
;;; CSV columns needed : Code, Northing, Easting, Elevation
;;; ============================================================================

(vl-load-com)

;; ---- settings (edit freely) ------------------------------------------------
(setq LC-BlockName "vid_lc_gate_001")   ; LC Symbol - window 02 default (stays as last used)
(setq LC-PropRot   "lc_gate_rot")       ; dynamic property: rotation
(setq LC-PropMove  "lc_gate_move")      ; dynamic property: move / distance
(setq LC-HGBlockName "vid_lc_heightGuage_001") ; Height Guage Symbol - window 02 default
(setq LC-HGPropRot   "lc_heightGuage_rot")     ; height gauge dynamic property: rotation
(setq LC-HGPropMove  "lc_heightGuage_move")    ; height gauge dynamic property: move
(setq LC-HGDist      30.0)                     ; max distance (m) between the 2 height gauge points
(setq LC-HGRotOffset 0.0)                      ; radians added to the height gauge rotation value
(setq LC-HGMoveOffset 0.0)                     ; added to the height gauge move value (units)
(setq LC-TrackMode   "layers")                 ; track polyline source: "layers" or "manual"
(setq LC-TrackLayers nil)                      ; track layers chosen last time (remembered)
(setq LC-MeasOn      nil)                      ; Measurement check box (always starts False)
(setq LC-MeasMode    "Nearest")                ; Nearest / LHS / RHS / Both Sides
(setq LC-MeasLayer   "1-LC")                   ; layer of the Measurement dimensions
(setq LC-GroupDist   30.0)                     ; gates closer than this (m) are one group
(setq LC-LockDist  20.0)                ; max distance (m) GATE -> LOCK
(setq LC-RotOffset 0.0)                 ; radians added to the rotation value
(setq LC-MoveOffset 0.0)                ; added to the move value (units)
(setq LC-UseElev   T)                   ; T = insert at the CSV elevation, nil = Z 0

(setq LC-RegKey "HKEY_CURRENT_USER\\Software\\VID\\Tools")   ; same as vidBankCutting

;; ---------------------------------------------------------------------------
;; Small helpers
;; ---------------------------------------------------------------------------
(defun LC-Trim (s)
  (if s (vl-string-trim " \t\r\n" s) "")
)

(defun LC-GetField (fields idx)
  (if (and idx (>= idx 0) (< idx (length fields)))
    (LC-Trim (nth idx fields))
    ""
  )
)

(defun LC-Num (s)
  (if (and s (/= (LC-Trim s) ""))
    (distof (LC-Trim s) 2)
    nil
  )
)

(defun LC-HeaderIndex (headers wanted / i found)
  (setq i 0 found nil)
  (while (and (< i (length headers)) (null found))
    (if (= (strcase (LC-Trim (nth i headers))) (strcase wanted))
      (setq found i)
    )
    (setq i (1+ i))
  )
  found
)

(defun LC-StripBOM (s)
  (if (and s (>= (strlen s) 3)
           (= (ascii (substr s 1 1)) 239)
           (= (ascii (substr s 2 1)) 187)
           (= (ascii (substr s 3 1)) 191))
    (substr s 4)
    s
  )
)

;; ---------------------------------------------------------------------------
;; CSV reading (binary safe - DGPS files can contain 0x1A in text fields)
;; ---------------------------------------------------------------------------
(defun LC-ParseCSV (s / i n ch inquote cur fields)
  (setq i 1 n (strlen s) inquote nil cur "" fields '())
  (while (<= i n)
    (setq ch (substr s i 1))
    (cond
      ((and (= ch "\"") inquote (< i n) (= (substr s (1+ i) 1) "\""))
       (setq cur (strcat cur "\"") i (+ i 2)))
      ((= ch "\"")
       (setq inquote (not inquote) i (1+ i)))
      ((and (= ch ",") (not inquote))
       (setq fields (cons cur fields) cur "" i (1+ i)))
      (T (setq cur (strcat cur ch) i (1+ i)))
    )
  )
  (reverse (cons cur fields))
)

(defun LC-ReadLine (fh / b s gotLine)
  (setq s "" gotLine nil)
  (while (and (not gotLine) (setq b (read-char fh)))
    (cond
      ((= b 10) (setq gotLine T))
      ((= b 13)
       (setq b (read-char fh))
       (if (and b (/= b 10)) (setq s (strcat s (chr b))))
       (setq gotLine T))
      (T (setq s (strcat s (chr b))))
    )
  )
  (if (or gotLine (> (strlen s) 0)) s nil)
)

;; ---------------------------------------------------------------------------
;; Locate vid_blocks_tools.dwg  (same lookup as vidBankCutting.lsp)
;;  1. path remembered from the last run (session + registry)
;;  2. same folder as vidGradient.lsp (found through the support path)
;;  3. support/vid/tools relative to the support path
;;  4. ask the user once, then remember the path
;; ---------------------------------------------------------------------------
(defun LC-find-dwg (/ hit f dir n rel)
  (if (and *VID-BLOCKS-DWG* (findfile *VID-BLOCKS-DWG*))
    (setq hit (findfile *VID-BLOCKS-DWG*))
  )
  (if (not hit)
    (progn
      (setq f (vl-registry-read LC-RegKey "BlocksDwg"))
      (if (and f (= (type f) 'STR) (findfile f))
        (setq hit (findfile f))
      )
    )
  )
  (if (not hit)
    (foreach n '("vidGradient.lsp" "vidGradientAlign.lsp" "vidBankCutting.lsp" "vidLC.lsp")
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
        (list LC-RegKey "BlocksDwg" hit))
    )
  )
  hit
)

;; Import the block <name> if it is missing from the current drawing
(defun LC-import-block (dwg name / acad doc ver dbx blks item objs path)
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

;; ---------------------------------------------------------------------------
;; Dynamic block helpers
;; ---------------------------------------------------------------------------
(defun LC-GetProps (obj / r)
  (setq r (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p r) nil r)
)

(defun LC-FindProp (props name)
  (car
    (vl-remove-if-not
      '(lambda (p) (= (strcase (vla-get-PropertyName p)) (strcase name)))
      props))
)

;; Set a dynamic property by name.  The property list is read again each time
;; because changing one value can regenerate the block.  Returns T on success.
(defun LC-SetProp (obj name val / p res)
  (setq p (LC-FindProp (LC-GetProps obj) name))
  (if p
    (progn
      (setq res (vl-catch-all-apply 'vla-put-Value
                  (list p (vlax-make-variant val vlax-vbDouble))))
      (not (vl-catch-all-error-p res))
    )
    nil
  )
)

(defun LC-PropNames (obj / out)
  (setq out "")
  (foreach p (LC-GetProps obj)
    (setq out (strcat out (if (= out "") "" ", ") (vla-get-PropertyName p))))
  out
)

;; ---------------------------------------------------------------------------
;; Geometry helpers
;; ---------------------------------------------------------------------------
;; records:  (code x y z)
(defun LC-Dist2D (a b)
  (distance (list (car a) (cadr a)) (list (car b) (cadr b)))
)

;; nearest LOCK point (x y z) to pt within LC-LockDist, or nil
(defun LC-NearestLock (pt locks / best bd d)
  (setq best nil bd LC-LockDist)
  (foreach l locks
    (setq d (LC-Dist2D pt l))
    (if (<= d bd) (setq bd d best l))
  )
  best
)

(defun LC-NormAngle (a)
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  (while (>= a (* 2.0 pi)) (setq a (- a (* 2.0 pi))))
  a
)

;; drawing space the block goes into (model, or paper when a layout is active)
(defun LC-Space (doc)
  (if (or (= (getvar "TILEMODE") 1) (> (getvar "CVPORT") 1))
    (vla-get-ModelSpace doc)
    (vla-get-PaperSpace doc)
  )
)

;; ===========================================================================
;; WINDOW 01 - "Selected layers"      list of CSV codes (multiple selection)
;; Codes containing GATE or LOCK are selected by default.
;; Returns (selectedCodes) or nil if cancelled.
;; ===========================================================================
(defun LC-GateLockP (code / u)
  (setq u (strcase code))
  (or (vl-string-search "GATE" u) (vl-string-search "LOCK" u))
)

;; height gauge code: HEIGHT + GUAGE or GAUGE  (height guage, heightguage,
;; heightgauge, height gauge, HEIGHT GUAGE ...)
(defun LC-HeightGaugeP (code / u)
  (setq u (strcase code))
  (and (vl-string-search "HEIGHT" u)
       (or (vl-string-search "GUAGE" u) (vl-string-search "GAUGE" u)))
)

;; auto detect: GATE / LOCK / height gauge codes, but ignore anything with LODGE
(defun LC-AutoSelectP (code)
  (and (not (vl-string-search "LODGE" (strcase code)))
       (or (LC-GateLockP code) (LC-HeightGaugeP code)))
)

(defun LC-CodeDialog (codenames / dcl_id dlgName result f sortedCodes
                       autoIdx i dlgRet nm selectedList idx)
  (setq dlgName (vl-filename-mktemp "vidLcSel" (getvar "TEMPPREFIX") ".dcl"))
  (setq f (open dlgName "w"))
  (write-line
    (strcat
      "lc_layer_sel : dialog {\n"
      "  label = \"Selected layers\";\n"
      "  : list_box {\n"
      "    key              = \"lst_codes\";\n"
      "    height           = 20;\n"
      "    width            = 48;\n"
      "    fixed_width_font = false;\n"
      "    multiple_select  = true;\n"
      "    allow_accept     = false;\n"
      "  }\n"
      "  spacer_1;\n"
      "  ok_cancel;\n"
      "}\n")
    f)
  (close f)

  (setq dcl_id (load_dialog dlgName))
  (setq result nil)
  (if (and (> dcl_id 0) (new_dialog "lc_layer_sel" dcl_id))
    (progn
      (setq sortedCodes
        (vl-sort codenames '(lambda (a b) (< (strcase a) (strcase b)))))
      (start_list "lst_codes" 3)
      (foreach c sortedCodes (add_list c))
      (end_list)

      ;; default selection: GATE / LOCK / height gauge codes (LODGE ignored)
      (setq autoIdx "" i 0)
      (foreach c sortedCodes
        (if (LC-AutoSelectP c)
          (setq autoIdx (strcat autoIdx (if (= autoIdx "") "" " ") (itoa i))))
        (setq i (1+ i))
      )
      (if (/= autoIdx "") (set_tile "lst_codes" autoIdx))

      (setq LC-selIdx "")
      ;; OK stays open until at least one code is selected
      (action_tile "accept"
        (strcat "(setq LC-selIdx (get_tile \"lst_codes\"))"
                "(if (= (LC-Trim LC-selIdx) \"\")"
                "  (alert \"Select at least one layer.\")"
                "  (done_dialog 1))"))
      (action_tile "cancel" "(done_dialog 0)")

      (setq dlgRet (start_dialog))
      (if (= dlgRet 1)
        (progn
          (setq selectedList '())
          (foreach idx (read (strcat "(" LC-selIdx ")"))
            (setq nm (nth idx sortedCodes))
            (if nm (setq selectedList (append selectedList (list nm)))))
          (setq result (list selectedList))
        )
      )
    )
    (princ "\nVIDLC: could not open the Selected layers window.")
  )
  (if (> dcl_id 0) (unload_dialog dcl_id))
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  result
)

;; ===========================================================================
;; TRACK POLYLINE helpers - tangent / side (RHS, LHS) / chainage
;; The track entities are only READ (never modified, never reversed).
;; ===========================================================================
(defun LC-AllLayers (/ rec lst)
  (setq rec (tblnext "LAYER" T) lst '())
  (while rec
    (setq lst (cons (cdr (assoc 2 rec)) lst))
    (setq rec (tblnext "LAYER"))
  )
  (acad_strlsort lst)
)

;; escape wildcard characters so a layer name matches only itself
(defun LC-EscLayer (s / i ch out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (setq out
      (strcat out
        (if (member ch '("`" "#" "@" "." "*" "?" "~" "[" "]" ","))
          (strcat "`" ch)
          ch)))
    (setq i (1+ i))
  )
  out
)

(defun LC-SSToList (ss / i out)
  (setq out '() i 0)
  (if ss
    (while (< i (sslength ss))
      (setq out (cons (ssname ss i) out))
      (setq i (1+ i))
    )
  )
  (reverse out)
)

;; Track curves from window 02: all curves on the chosen layers, or picked
;; manually.  Returns a list of enames (nil if none).
(defun LC-GetTracks (/ ss filt)
  (setq ss nil)
  (if (= LC-TrackMode "manual")
    (progn
      (princ "\nSelect TRACK polyline(s) <Enter = none>: ")
      (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,LINE,ARC,SPLINE"))))
    )
    (if LC-TrackLayers
      (progn
        (setq filt "")
        (foreach l LC-TrackLayers
          (setq filt (strcat filt (if (= filt "") "" ",") (LC-EscLayer l))))
        (setq ss
          (ssget "_X"
            (list '(0 . "LWPOLYLINE,POLYLINE,LINE,ARC,SPLINE")
                  (cons 8 filt))))
      )
    )
  )
  (LC-SSToList ss)
)

;; closest point of ONE track curve to pt (nil on failure)
(defun LC-ClosestOn (en pt / cp)
  (setq cp (vl-catch-all-apply 'vlax-curve-getClosestPointTo
             (list en (list (car pt) (cadr pt) 0.0))))
  (if (vl-catch-all-error-p cp) nil cp)
)

;; nearest track curve to pt: (ename closestPoint) or nil
(defun LC-TrackLocate (pt tracks / best bd cp d)
  (setq best nil bd nil)
  (foreach en tracks
    (setq cp (LC-ClosestOn en pt))
    (if cp
      (progn
        (setq d (LC-Dist2D pt cp))
        (if (or (null bd) (< d bd)) (setq bd d best (list en cp)))
      )
    )
  )
  best
)

;; tangent (unit, as the polyline is drawn) and chainage at point cp on curve en
;; Returns (tx ty chainage) or nil
(defun LC-TrackFrame (en cp / prm drv ch len)
  (setq prm (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list en cp)))
  (if (vl-catch-all-error-p prm) (setq prm nil))
  (if prm
    (progn
      (setq drv (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list en prm)))
      (setq ch  (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list en cp)))
      (if (or (vl-catch-all-error-p drv) (vl-catch-all-error-p ch))
        nil
        (progn
          (setq len (sqrt (+ (* (car drv) (car drv)) (* (cadr drv) (cadr drv)))))
          (if (> len 1e-9)
            (list (/ (car drv) len) (/ (cadr drv) len) ch)
            nil
          )
        )
      )
    )
    nil
  )
)

;; Order two connected height gauge points with the track tangent.
;; Side = side of the track the connection lies on (looking along the tangent).
;;   RHS : first point = lower chainage  (first along the tangent)
;;   LHS : order switched - first point = higher chainage
;; Returns (first second "RHS"/"LHS") or nil (no track / not found)
(defun LC-OrderPair (p1 p2 tracks / mid loc en cp fr tx ty cross side
                     cp1 cp2 fr1 fr2 c1 c2 swap)
  (if tracks
    (progn
      (setq mid (list (/ (+ (car p1) (car p2)) 2.0)
                      (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
      (setq loc (LC-TrackLocate mid tracks))
      (if loc
        (progn
          (setq en (car loc) cp (cadr loc))
          (setq fr (LC-TrackFrame en cp))
          (setq cp1 (LC-ClosestOn en p1) cp2 (LC-ClosestOn en p2))
          (setq fr1 (if cp1 (LC-TrackFrame en cp1))
                fr2 (if cp2 (LC-TrackFrame en cp2)))
          (if (and fr fr1 fr2)
            (progn
              (setq tx (car fr) ty (cadr fr))
              ;; cross > 0 : left of the tangent,  < 0 : right of the tangent
              (setq cross (- (* tx (- (cadr mid) (cadr cp)))
                             (* ty (- (car mid) (car cp)))))
              (setq side (if (< cross 0.0) "RHS" "LHS"))
              (setq c1 (caddr fr1) c2 (caddr fr2))
              (setq swap
                (if (= side "RHS") (> c1 c2) (< c1 c2)))
              (if swap
                (list p2 p1 side)
                (list p1 p2 side)
              )
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

;; ===========================================================================
;; MEASUREMENT - DIMALIGNED from the polyline MIDPOINTS inside the gate block
;; ===========================================================================
(defun LC-EnsureLayer (name)
  (if (not (tblsearch "LAYER" name))
    (entmakex
      (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbLayerTableRecord") (cons 2 name)
            '(70 . 0) '(62 . 7) '(6 . "Continuous")))
  )
)

;; midpoint of every SEGMENT of one LWPOLYLINE (local block coordinates).
;; Arc segments (bulge) give the midpoint of the arc.
(defun LC-PolySegMids (en / ed verts n segs i p q b mx my dx dy c s nx ny out
                            closed elev lastv)
  (setq ed     (entget en)
        verts  '()
        closed (= 1 (logand 1 (cdr (assoc 70 ed))))
        elev   (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0))
  (foreach d ed
    (cond
      ((= (car d) 10)
       (setq verts (append verts (list (list (cadr d) (caddr d) 0.0)))))
      ((and (= (car d) 42) verts)
       (setq lastv (last verts))
       (setq verts (append (reverse (cdr (reverse verts)))
                           (list (list (car lastv) (cadr lastv) (cdr d))))))
    )
  )
  (setq n (length verts) out '())
  (if (> n 1)
    (progn
      (setq segs (if closed n (1- n)) i 0)
      (while (< i segs)
        (setq p  (nth i verts)
              q  (nth (rem (1+ i) n) verts)
              b  (caddr p)
              mx (/ (+ (car p) (car q)) 2.0)
              my (/ (+ (cadr p) (cadr q)) 2.0))
        (if (> (abs b) 1e-9)
          (progn
            (setq dx (- (car q) (car p)) dy (- (cadr q) (cadr p))
                  c  (sqrt (+ (* dx dx) (* dy dy))))
            (if (> c 1e-12)
              (progn
                (setq s  (/ (* (abs b) c) 2.0)
                      nx (/ dy c)
                      ny (/ (- dx) c))
                (if (< b 0.0) (setq nx (- nx) ny (- ny)))
                (setq mx (+ mx (* nx s)) my (+ my (* ny s)))
              )
            )
          )
        )
        (setq out (cons (list mx my elev) out))
        (setq i (1+ i))
      )
    )
  )
  (reverse out)
)

;; WCS midpoints of all polyline segments inside block reference blk, read from
;; the CURRENT block geometry (dynamic block properties already applied).
(defun LC-BlockPolyMids (blk / en ed bn bt base e ent typ ins sx sy sz ang ext
                               out m lx ly x y)
  (vl-catch-all-apply 'vla-Update (list blk))
  (setq en  (vlax-vla-object->ename blk)
        ed  (entget en)
        bn  (cdr (assoc 2 ed))
        bt  (tblobjname "BLOCK" bn)
        out '())
  (if bt
    (progn
      (setq base (cdr (assoc 10 (entget bt))))
      (setq ins (cdr (assoc 10 ed))
            sx  (if (assoc 41 ed) (cdr (assoc 41 ed)) 1.0)
            sy  (if (assoc 42 ed) (cdr (assoc 42 ed)) 1.0)
            sz  (if (assoc 43 ed) (cdr (assoc 43 ed)) 1.0)
            ang (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)
            ext (if (assoc 210 ed) (cdr (assoc 210 ed)) '(0.0 0.0 1.0)))
      (setq e (entnext bt))
      (while e
        (setq ent (entget e) typ (cdr (assoc 0 ent)))
        (cond
          ((= typ "ENDBLK") (setq e nil))
          (T
           (if (= typ "LWPOLYLINE")
             (foreach m (LC-PolySegMids e)
               (setq lx (* sx (- (car m)  (car base)))
                     ly (* sy (- (cadr m) (cadr base))))
               (setq x (- (* lx (cos ang)) (* ly (sin ang)))
                     y (+ (* lx (sin ang)) (* ly (cos ang))))
               (setq out
                 (cons (trans (list (+ (car ins) x)
                                    (+ (cadr ins) y)
                                    (+ (caddr ins)
                                       (* sz (- (caddr m)
                                                (if (caddr base) (caddr base) 0.0)))))
                              ext 0)
                       out))
             )
           )
           (setq e (entnext e))
          )
        )
      )
    )
  )
  (reverse out)
)

;; candidates for point pt: ((dist ename closestPoint side) ...)  side = "LHS"/"RHS"
;; side = side of the track the point lies on, looking along the track as drawn
(defun LC-TrackCands (pt tracks / out cp fr cross)
  (setq out '())
  (foreach en tracks
    (setq cp (LC-ClosestOn en pt))
    (if cp
      (progn
        (setq fr (LC-TrackFrame en cp))
        (setq cross
          (if fr
            (- (* (car fr)  (- (cadr pt) (cadr cp)))
               (* (cadr fr) (- (car pt)  (car cp))))
            nil))
        (setq out
          (cons (list (LC-Dist2D pt cp) en cp
                      (cond ((null cross) nil)
                            ((< cross 0.0) "RHS")
                            (T "LHS")))
                out))
      )
    )
  )
  (reverse out)
)

;; pair = (midpoint dist trackEname closestPoint side); shortest distance,
;; only pairs on the given side (side nil = any side)
(defun LC-BestPair (pairs side / best)
  (setq best nil)
  (foreach c pairs
    (if (and (or (null side) (= (nth 4 c) side))
             (or (null best) (< (nth 1 c) (nth 1 best))))
      (setq best c))
  )
  best
)

;; DIMALIGNED from p1 to p2 on layer lay (text on the line between them).
(defun LC-AddDimAligned (space p1 p2 lay / r)
  (setq r (vl-catch-all-apply 'vla-AddDimAligned
            (list space
                  (vlax-3d-point p1)
                  (vlax-3d-point p2)
                  (vlax-3d-point (/ (+ (car p1) (car p2)) 2.0)
                                 (/ (+ (cadr p1) (cadr p2)) 2.0)
                                 (caddr p1)))))
  (if (vl-catch-all-error-p r)
    nil
    (progn
      (vl-catch-all-apply 'vla-put-Layer (list r lay))
      r
    )
  )
)

;; WCS insertion point of a block reference
(defun LC-InsWCS (blk / ed)
  (setq ed (entget (vlax-vla-object->ename blk)))
  (trans (cdr (assoc 10 ed))
         (if (assoc 210 ed) (cdr (assoc 210 ed)) '(0.0 0.0 1.0))
         0)
)

;; info of one gate block: (blk insertionPoint bestPair) or nil
;;   bestPair = (midpoint dist trackEname footPoint side)  - the polyline
;;   midpoint nearest to a track; side = side of the track the block is on
(defun LC-GateInfo (blk tracks / mids pairs best)
  (setq pairs '())
  (foreach m (LC-BlockPolyMids blk)
    (foreach c (LC-TrackCands m tracks)
      (setq pairs (cons (cons m c) pairs))))
  (setq best (LC-BestPair pairs nil))
  (if best (list blk (LC-InsWCS blk) best) nil)
)

;; split gate infos into groups (gates closer than dist are linked)
(defun LC-GateGroups (infos dist / groups grp changed)
  (setq groups '())
  (while infos
    (setq grp (list (car infos)) infos (cdr infos) changed T)
    (while changed
      (setq changed nil)
      (foreach r infos
        (if (vl-some '(lambda (g) (< (LC-Dist2D (nth 1 g) (nth 1 r)) dist)) grp)
          (progn
            (setq grp   (append grp (list r))
                  infos (vl-remove r infos)
                  changed T)
          )
        )
      )
    )
    (setq groups (append groups (list grp)))
  )
  groups
)

;; Measure all gate blocks of this run, group by group.
;; Returns the number of dimensions made.
(defun LC-MeasureGroups (space blks tracks mode / infos groups k grp picks n best
                                                 p1 p2 side nNone)
  (setq n 0 nNone 0 infos '())
  (foreach b blks
    (setq best (LC-GateInfo b tracks))
    (if best (setq infos (append infos (list best))) (setq nNone (1+ nNone)))
  )
  (setq groups (LC-GateGroups infos LC-GroupDist) k 0)
  (foreach grp groups
    (setq k (1+ k))
    ;; blocks of the group that get a dimension
    (setq picks
      (cond
        ((= mode "LHS")
         (vl-remove-if-not '(lambda (i) (= (nth 4 (nth 2 i)) "LHS")) grp))
        ((= mode "RHS")
         (vl-remove-if-not '(lambda (i) (= (nth 4 (nth 2 i)) "RHS")) grp))
        ((= mode "Both Sides")
         (vl-remove-if-not '(lambda (i) (member (nth 4 (nth 2 i)) '("LHS" "RHS"))) grp))
        (T                                    ; Nearest: one block of the group
         (list (car (vl-sort grp '(lambda (a b) (< (nth 1 (nth 2 a)) (nth 1 (nth 2 b))))))))
      )
    )
    (princ (strcat "\n  LC group " (itoa k) " (" (itoa (length grp)) " gate block"
                   (if (= (length grp) 1) "" "s") ")"))
    (foreach i picks
      (setq best (nth 2 i)
            p1   (nth 0 best)
            p2   (list (car (nth 3 best)) (cadr (nth 3 best)) (caddr p1))
            side (nth 4 best))
      (if (LC-AddDimAligned space p1 p2 LC-MeasLayer)
        (progn
          (setq n (1+ n))
          (princ (strcat "\n    " (if side side "") "  "
                         (rtos (nth 1 best) 2 2) " m"))
        )
        (princ "\n    DIMALIGNED failed")
      )
    )
    (if (null picks)
      (princ (strcat "\n    no " mode " block in this group - ignored")))
  )
  (if (> nNone 0)
    (princ (strcat "\n  " (itoa nNone)
                   " gate block(s) not measured (no polyline or no track).")))
  n
)

;; ===========================================================================
;; WINDOW 02 - "LC"
;;   Settings
;;     LC Symbol            [vid_lc_gate_001]
;;     Height Guage Symbol  [vid_lc_heightGuage_001]
;;     Measurement          [checkbox, default False]  [Nearest/LHS/RHS/Both Sides]
;;   Track Polyline
;;     (o) Select layers     ( ) Manual Selection
;; hgOn   = T when height gauge codes were selected
;; gateOn = T when gate codes were selected (Measurement available)
;; The track section is enabled when hgOn, or when Measurement is ticked.
;; Returns (symbol hgSymbol) or nil if cancelled.  The track choice is kept in
;; LC-TrackMode / LC-TrackLayers, Measurement in LC-MeasOn / LC-MeasMode.
;; ===========================================================================
(defun LC-TrackUI (/ need)
  (setq need (or LC-tHgOn
                 (and LC-tGateOn (= (get_tile "tg_meas") "1"))))
  (mode_tile "rb_lay" (if need 0 1))
  (mode_tile "rb_man" (if need 0 1))
  (mode_tile "lst_trk" (if (and need (/= LC-tMode "manual")) 0 1))
)

(defun LC-SettingsDialog (hgOn gateOn / dcl_id dlgName result f dlgRet layers
                          initSel i idx lst need)
  (setq layers (LC-AllLayers))
  (setq dlgName (vl-filename-mktemp "vidLcOpt" (getvar "TEMPPREFIX") ".dcl"))
  (setq f (open dlgName "w"))
  (write-line
    (strcat
      "lc_settings : dialog {\n"
      "  label = \"LC\";\n"
      "  : boxed_column {\n"
      "    label = \"Settings\";\n"
      "    : row {\n"
      "      : text     { label = \"LC Symbol\"; width = 20; fixed_width = true; }\n"
      "      : edit_box { key = \"edt_sym\"; label = \"\"; edit_width = 28; }\n"
      "    }\n"
      "    : row {\n"
      "      : text     { label = \"Height Guage Symbol\"; width = 20; fixed_width = true; }\n"
      "      : edit_box { key = \"edt_hg\"; label = \"\"; edit_width = 28; }\n"
      "    }\n"
      "    : row {\n"
      "      : text       { label = \"Measurement\"; width = 20; fixed_width = true; }\n"
      "      : toggle     { key = \"tg_meas\"; label = \"\"; }\n"
      "      : popup_list { key = \"pop_meas\"; edit_width = 14; fixed_width = true; }\n"
      "    }\n"
      "  }\n"
      "  : boxed_column {\n"
      "    label = \"Track Polyline\";\n"
      "    : radio_button { key = \"rb_lay\"; label = \"Select layers\"; value = \"1\"; }\n"
      "    : row {\n"
      "      : spacer { width = 3; fixed_width = true; }\n"
      "      : list_box {\n"
      "        key = \"lst_trk\"; height = 10; width = 40;\n"
      "        fixed_width_font = false; multiple_select = true; allow_accept = false;\n"
      "      }\n"
      "    }\n"
      "    : radio_button { key = \"rb_man\"; label = \"Manual Selection\"; value = \"0\"; }\n"
      "  }\n"
      "  spacer_1;\n"
      "  ok_cancel;\n"
      "}\n")
    f)
  (close f)

  (setq dcl_id (load_dialog dlgName))
  (setq result nil)
  (if (and (> dcl_id 0) (new_dialog "lc_settings" dcl_id))
    (progn
      (set_tile "edt_sym" LC-BlockName)
      (set_tile "edt_hg"  LC-HGBlockName)

      ;; Measurement: check box starts False, drop-down remembers the last mode
      (start_list "pop_meas")
      (foreach m '("Nearest" "LHS" "RHS" "Both Sides") (add_list m))
      (end_list)
      (setq idx 0 i 0)
      (foreach m '("Nearest" "LHS" "RHS" "Both Sides")
        (if (= m LC-MeasMode) (setq idx i))
        (setq i (1+ i)))
      (set_tile "pop_meas" (itoa idx))
      (set_tile "tg_meas" "0")
      (mode_tile "pop_meas" 1)
      (if (not gateOn)
        (mode_tile "tg_meas" 1))            ; no gate codes selected

      ;; track section
      (start_list "lst_trk" 3)
      (foreach l layers (add_list l))
      (end_list)
      (setq initSel "" i 0)
      (foreach l layers
        (if (member l (if LC-TrackLayers LC-TrackLayers '("1-TRACK")))
          (setq initSel (strcat initSel (if (= initSel "") "" " ") (itoa i))))
        (setq i (1+ i))
      )
      (if (/= initSel "") (set_tile "lst_trk" initSel))
      (setq LC-tMode LC-TrackMode LC-tHgOn hgOn LC-tGateOn gateOn)
      (set_tile "rb_lay" (if (= LC-TrackMode "manual") "0" "1"))
      (set_tile "rb_man" (if (= LC-TrackMode "manual") "1" "0"))
      (LC-TrackUI)

      (action_tile "tg_meas"
        (strcat "(mode_tile \"pop_meas\" (if (= $value \"1\") 0 1))"
                "(LC-TrackUI)"))
      (action_tile "rb_lay"
        (strcat "(setq LC-tMode \"layers\")"
                "(set_tile \"rb_lay\" \"1\") (set_tile \"rb_man\" \"0\")"
                "(LC-TrackUI)"))
      (action_tile "rb_man"
        (strcat "(setq LC-tMode \"manual\")"
                "(set_tile \"rb_lay\" \"0\") (set_tile \"rb_man\" \"1\")"
                "(LC-TrackUI)"))

      ;; OK stays open while a box is blank / no track layer is selected
      (action_tile "accept"
        (strcat "(setq LC-tSym (LC-Trim (get_tile \"edt_sym\")))"
                "(setq LC-tHg  (LC-Trim (get_tile \"edt_hg\")))"
                "(setq LC-tTrk (get_tile \"lst_trk\"))"
                "(setq LC-tMeas (get_tile \"tg_meas\"))"
                "(setq LC-tMeasPop (get_tile \"pop_meas\"))"
                "(cond"
                "  ((= LC-tSym \"\") (alert \"Enter the LC Symbol (block name).\") (mode_tile \"edt_sym\" 2))"
                "  ((= LC-tHg \"\")  (alert \"Enter the Height Guage Symbol (block name).\") (mode_tile \"edt_hg\" 2))"
                "  ((and (or LC-tHgOn (and LC-tGateOn (= LC-tMeas \"1\")))"
                "        (= LC-tMode \"layers\") (= (LC-Trim LC-tTrk) \"\"))"
                "   (alert \"Select at least one track layer, or choose Manual Selection.\"))"
                "  (T (done_dialog 1)))"))
      (action_tile "cancel" "(done_dialog 0)")

      (setq dlgRet (start_dialog))
      (if (= dlgRet 1)
        (progn
          (setq LC-MeasOn   (and gateOn (= LC-tMeas "1")))
          (setq LC-MeasMode (nth (atoi LC-tMeasPop)
                                 '("Nearest" "LHS" "RHS" "Both Sides")))
          (setq need (or hgOn LC-MeasOn))
          (if need
            (progn
              (setq LC-TrackMode LC-tMode)
              (setq lst '())
              (foreach idx (read (strcat "(" LC-tTrk ")"))
                (if (nth idx layers) (setq lst (append lst (list (nth idx layers))))))
              (if lst (setq LC-TrackLayers lst))
            )
          )
          (setq result (list LC-tSym LC-tHg))
        )
      )
    )
    (princ "\nVIDLC: could not open the LC window.")
  )
  (if (> dcl_id 0) (unload_dialog dcl_id))
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  result
)

;; ---------------------------------------------------------------------------
;; Height gauge helpers
;; ---------------------------------------------------------------------------
;; Connect the height gauge points: the closest two points first (within
;; LC-HGDist), every point used once.  An odd point out (no free partner) is
;; connected to its nearest height gauge point instead (3 points -> 2 blocks).
;; Returns (pairs extras isolatedCount)
;;   pairs  = ((p1 p2) ...)   p1 = earlier CSV row
;;   extras = ((pLeft pNear) ...)  block goes at pLeft, aims at pNear
(defun LC-PairHG (pts / n i j d cands used pairs extras isolated c a b best bd)
  (setq n (length pts) cands '() i 0)
  (while (< i n)
    (setq j (1+ i))
    (while (< j n)
      (setq d (LC-Dist2D (nth i pts) (nth j pts)))
      (if (<= d LC-HGDist) (setq cands (cons (list d i j) cands)))
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  (setq cands (vl-sort cands '(lambda (a b) (< (car a) (car b)))))
  (setq used '() pairs '())
  (foreach c cands
    (setq a (cadr c) b (caddr c))
    (if (and (not (member a used)) (not (member b used)))
      (setq used  (cons a (cons b used))
            pairs (cons (list (nth a pts) (nth b pts)) pairs))
    )
  )
  ;; points left over: connect to the nearest other height gauge point
  (setq extras '() isolated 0 i 0)
  (while (< i n)
    (if (not (member i used))
      (progn
        (setq best nil bd LC-HGDist j 0)
        (while (< j n)
          (if (/= j i)
            (progn
              (setq d (LC-Dist2D (nth i pts) (nth j pts)))
              (if (<= d bd) (setq bd d best j))
            )
          )
          (setq j (1+ j))
        )
        (if best
          (setq extras (cons (list (nth i pts) (nth best pts)) extras))
          (setq isolated (1+ isolated))
        )
      )
    )
    (setq i (1+ i))
  )
  (list (reverse pairs) (reverse extras) isolated)
)

;; Insert <name> at p1, rotate it towards p2 and set its move value to the
;; p1 -> p2 distance.  Returns (ok angle dist blk).
(defun LC-InsertLinked (space name p1 p2 propRot propMove rotOff moveOff
                        / blk ang dist)
  (setq blk
    (vla-InsertBlock space
      (vlax-3d-point (car p1) (cadr p1) (caddr p1))
      name 1.0 1.0 1.0 0.0))
  (setq ang  (LC-NormAngle
               (+ (angle (list (car p1) (cadr p1)) (list (car p2) (cadr p2)))
                  rotOff))
        dist (+ (LC-Dist2D p1 p2) moveOff))
  ;; rotation first, then move
  (list (and (LC-SetProp blk propRot ang) (LC-SetProp blk propMove dist))
        ang dist blk)
)

;; ===========================================================================
;; MAIN COMMAND
;; ===========================================================================
(defun c:VIDDGPSTOLC
  (/ *error* csvFile dwg fh headerLine headers idxC idxN idxE idxZ
     line fields code north east elev pt recs codes selRes selCodes setRes sym hgSym
     gates locks hgPts pairRes hgPairs hgExtras hgLeft tracks ord pa pb sideTxt doc space blk lock ang dist res
     nGate nLock nNoLock nPropFail nHG nHGExtra nHGFail nMeas gateBlks olddyn oldecho undoOn)

  (defun *error* (msg)
    (if fh (vl-catch-all-apply 'close (list fh)))
    (if undoOn (command "_.undo" "_end"))
    (if olddyn  (setvar "DYNMODE" olddyn))
    (if oldecho (setvar "CMDECHO" oldecho))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nVIDLC ERROR: " msg))
      (princ "\nVIDLC cancelled.")
    )
    (princ)
  )

  (setq olddyn  (getvar "DYNMODE")
        oldecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; 1. CSV file - DYNMODE OFF only while the file is selected ----------------
  (setvar "DYNMODE" 0)
  (setq csvFile
    (if (boundp 'LogicDWG:RequireCsv)
      (LogicDWG:RequireCsv)
      (getfiled "Select DGPS CSV File" "" "csv" 4)))
  (setvar "DYNMODE" 3)                       ; ON again
  (if (null csvFile)
    (progn (princ "\nNo CSV file selected.") (exit))
  )

  ;; 2. Read the CSV: (code x y z) for every row, list of distinct codes ------
  (setq fh (open csvFile "rb"))
  (if (null fh)
    (progn (alert "Could not open the CSV file.") (exit))
  )
  (setq headerLine (LC-ReadLine fh))
  (if (null headerLine)
    (progn (alert "CSV file is empty.") (exit))
  )
  (setq headers (LC-ParseCSV (LC-StripBOM headerLine)))
  (setq idxC (LC-HeaderIndex headers "Code")
        idxN (LC-HeaderIndex headers "Northing")
        idxE (LC-HeaderIndex headers "Easting")
        idxZ (LC-HeaderIndex headers "Elevation"))
  (if (or (null idxC) (null idxN) (null idxE))
    (progn
      (alert "Required headers missing. Need: Code, Northing, Easting (Elevation optional).")
      (exit))
  )

  (setq recs '() codes '())
  (while (setq line (LC-ReadLine fh))
    (if (/= (LC-Trim line) "")
      (progn
        (setq fields (LC-ParseCSV line))
        (setq code  (LC-GetField fields idxC)
              north (LC-Num (LC-GetField fields idxN))
              east  (LC-Num (LC-GetField fields idxE))
              elev  (if idxZ (LC-Num (LC-GetField fields idxZ)) nil))
        (if (and (/= code "") north east)
          (progn
            (setq pt (list east north (if (and LC-UseElev elev) elev 0.0)))
            (setq recs (cons (cons code pt) recs))
            (if (not (member code codes)) (setq codes (cons code codes)))
          )
        )
      )
    )
  )
  (close fh)
  (setq fh nil)
  (setq recs (reverse recs))
  (if (null codes)
    (progn (alert "No valid points found in the CSV file.") (exit))
  )

  ;; 3. WINDOW 01 - select the CSV layers (codes) -----------------------------
  (setq selRes (LC-CodeDialog codes))
  (if (null selRes)
    (progn (princ "\nCancelled.") (exit))
  )
  (setq selCodes (car selRes))

  ;; 4. WINDOW 02 - Settings (symbols, Measurement) / Track Polyline ------------
  (setq setRes
    (LC-SettingsDialog
      (if (vl-some 'LC-HeightGaugeP selCodes) T nil)
      (if (vl-some '(lambda (c) (and (not (LC-HeightGaugeP c))
                                     (not (vl-string-search "LOCK" (strcase c)))))
                   selCodes)
        T nil)))
  (if (null setRes)
    (progn (princ "\nCancelled.") (exit))
  )
  (setq sym   (car setRes)
        hgSym (cadr setRes))
  (setq LC-BlockName sym
        LC-HGBlockName hgSym)

  ;; 5. Split the selected codes:
  ;;      height gauge code -> height gauge point
  ;;      contains LOCK     -> lock point
  ;;      anything else     -> gate point
  (setq gates '() locks '() hgPts '())
  (foreach r recs
    (if (member (car r) selCodes)
      (cond
        ((LC-HeightGaugeP (car r)) (setq hgPts (cons (cdr r) hgPts)))
        ((vl-string-search "LOCK" (strcase (car r)))
         (setq locks (cons (cdr r) locks)))
        (T (setq gates (cons (cdr r) gates)))
      )
    )
  )
  (setq gates (reverse gates) locks (reverse locks) hgPts (reverse hgPts))

  (princ "\n========================================")
  (princ "\nVIDLC")
  (princ "\n========================================")
  (princ (strcat "\nFile  : " csvFile))
  (princ (strcat "\nGATE points: " (itoa (length gates))
                 "     LOCK points: " (itoa (length locks))
                 "     HEIGHT GAUGE points: " (itoa (length hgPts))))

  (if (and (null gates) (< (length hgPts) 2))
    (progn
      (alert "Nothing to place.\nNeed GATE points, or at least 2 height gauge points, in the selected layers.\n(Codes containing LOCK are used as locks.)")
      (exit)
    )
  )

  ;; 6. Import the blocks that are needed from vid_blocks_tools.dwg -----------
  (setq dwg (LC-find-dwg))
  (if (not dwg)
    (progn (alert "vid_blocks_tools.dwg not found!") (exit))
  )
  (if (and gates (not (LC-import-block dwg LC-BlockName)))
    (progn
      (alert (strcat "Block " LC-BlockName " could not be imported from\n" dwg))
      (exit)
    )
  )
  (if (and (>= (length hgPts) 2) (not (LC-import-block dwg LC-HGBlockName)))
    (progn
      (alert (strcat "Block " LC-HGBlockName " could not be imported from\n" dwg))
      (exit)
    )
  )

  (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object))
        space (LC-Space doc))
  (setq nGate 0 nLock 0 nNoLock 0 nPropFail 0 nHG 0 nHGExtra 0 nHGFail 0 hgLeft 0 nMeas 0 gateBlks nil)

  (command "_.undo" "_begin")
  (setq undoOn T)

  ;; track polyline(s) for the height gauge order and for Measurement
  (setq tracks nil)
  (if (or (>= (length hgPts) 2) (and LC-MeasOn gates))
    (progn
      (setq tracks (LC-GetTracks))
      (if tracks
        (princ (strcat "\nTrack polyline(s): " (itoa (length tracks))))
        (princ "\nNo track polyline selected.")
      )
    )
  )

  (if (and LC-MeasOn tracks) (LC-EnsureLayer LC-MeasLayer))

  ;; 7. GATE: one block per gate, face / move it to the nearest LOCK ----------
  (foreach g gates
    (setq blk
      (vla-InsertBlock space
        (vlax-3d-point (car g) (cadr g) (caddr g))
        LC-BlockName 1.0 1.0 1.0 0.0))
    (setq nGate (1+ nGate))

    (setq lock (LC-NearestLock g locks))
    (if lock
      (progn
        (setq ang  (LC-NormAngle
                     (+ (angle (list (car g) (cadr g)) (list (car lock) (cadr lock)))
                        LC-RotOffset))
              dist (+ (LC-Dist2D g lock) LC-MoveOffset))
        ;; rotation first (face the LOCK), then move (reach the LOCK)
        (if (and (LC-SetProp blk LC-PropRot  ang)
                 (LC-SetProp blk LC-PropMove dist))
          (progn
            (setq nLock (1+ nLock))
            (princ (strcat "\n  GATE #" (itoa nGate) ": LOCK at "
                           (rtos (LC-Dist2D g lock) 2 2) " m, rotation "
                           (rtos (* 180.0 (/ ang pi)) 2 2) " deg"))
          )
          (progn
            (setq nPropFail (1+ nPropFail))
            (princ (strcat "\n  GATE #" (itoa nGate) ": could not set \""
                           LC-PropRot "\" / \"" LC-PropMove "\"."))
            (if (= nPropFail 1)
              (princ (strcat "\n  Dynamic properties found in the block: "
                             (LC-PropNames blk))))
          )
        )
      )
      (progn
        (setq nNoLock (1+ nNoLock))
        (princ (strcat "\n  GATE #" (itoa nGate) ": no LOCK within "
                       (rtos LC-LockDist 2 1) " m - default block values."))
      )
    )

    (setq gateBlks (cons blk gateBlks))
  )

  ;; Measurement: gate blocks are grouped (2-4 per level crossing) and measured
  ;; to the selected track with DIMALIGNED (see header)
  (if (and LC-MeasOn tracks gateBlks)
    (progn
      (princ (strcat "\nMeasurement (" LC-MeasMode "):"))
      (setq nMeas (LC-MeasureGroups space (reverse gateBlks) tracks LC-MeasMode))
    )
  )

  ;; 8. HEIGHT GAUGE: connect the nearby points -----------------------------
  ;;    the track tangent (read only, never reversed) gives RHS / LHS order
  ;;    pairs first; an odd point out gets its own block to its nearest point
  (if (>= (length hgPts) 2)
    (progn
      (if (null tracks)
        (princ "\nNo track polyline - height guage points use the CSV order.")
        (princ "\nHeight guage order: track tangent RHS / LHS.")
      )
      (setq pairRes  (LC-PairHG hgPts)
            hgPairs  (car pairRes)
            hgExtras (cadr pairRes)
            hgLeft   (caddr pairRes))
      (foreach p (append hgPairs hgExtras)
        ;; tangent order: RHS = first along the tangent, LHS = order switched
        (setq ord (LC-OrderPair (car p) (cadr p) tracks))
        (setq pa (if ord (car ord)  (car p))
              pb (if ord (cadr ord) (cadr p))
              sideTxt (if ord (strcat " " (caddr ord)) ""))
        (setq res
          (LC-InsertLinked space LC-HGBlockName pa pb
                           LC-HGPropRot LC-HGPropMove
                           LC-HGRotOffset LC-HGMoveOffset))
        (setq nHG (1+ nHG))
        (if (> nHG (length hgPairs)) (setq nHGExtra (1+ nHGExtra)))
        (if (car res)
          (princ (strcat "\n  HEIGHT GAUGE #" (itoa nHG) sideTxt
                         (if (> nHG (length hgPairs)) " (odd point)" "")
                         ": next point at "
                         (rtos (caddr res) 2 2) " m, rotation "
                         (rtos (* 180.0 (/ (cadr res) pi)) 2 2) " deg"))
          (progn
            (setq nHGFail (1+ nHGFail))
            (princ (strcat "\n  HEIGHT GAUGE #" (itoa nHG) ": could not set \""
                           LC-HGPropRot "\" / \"" LC-HGPropMove "\"."))
            (if (= nHGFail 1)
              (princ (strcat "\n  Dynamic properties found in the block: "
                             (LC-PropNames (cadddr res)))))
          )
        )
      )
      (if (> hgLeft 0)
        (princ (strcat "\n  " (itoa hgLeft) " height gauge point(s) had no other point within "
                       (rtos LC-HGDist 2 1) " m - skipped."))
      )
    )
    (if (= (length hgPts) 1)
      (princ "\n  Only 1 height gauge point found - need 2 to connect.")
    )
  )

  (command "_.undo" "_end")
  (setq undoOn nil)

  (princ "\n----------------------------------------")
  (princ (strcat "\nGate blocks inserted         : " (itoa nGate)))
  (princ (strcat "\n  faced / moved to a LOCK    : " (itoa nLock)))
  (princ (strcat "\n  no LOCK nearby             : " (itoa nNoLock)))
  (if (> nPropFail 0)
    (princ (strcat "\n  dynamic property failed    : " (itoa nPropFail))))
  (if LC-MeasOn
    (princ (strcat "\nMeasurements (DIMALIGNED)     : " (itoa nMeas))))
  (princ (strcat "\nHeight gauge blocks inserted : " (itoa nHG)))
  (if (> nHGExtra 0)
    (princ (strcat "\n  of which odd-point links    : " (itoa nHGExtra))))
  (if (> nHGFail 0)
    (princ (strcat "\n  dynamic property failed    : " (itoa nHGFail))))
  (princ "\n========================================")

  (vla-Regen doc 1)
  (setvar "DYNMODE" olddyn)
  (setvar "CMDECHO" oldecho)
  (princ)
)

(princ "\nvidLC loaded. Type VIDLC to run.")
(princ)
