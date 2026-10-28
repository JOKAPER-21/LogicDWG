;;; ============================================================================
;;; Vid Dgps To Line
;;; Release: 1.5.8 | Civil 3D 2026
;;; Version: 14
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
;;;        - [Polyline] / [3D Polyline] toggle
;;;        - Scrollable list of grouped Code names (R1, R2, OHE, …)
;;;        - [Select Layers] button
;;;        - Then a second dialog: choose Default layers or Custom layer name,
;;;          and confirm.
;;;   5. Polylines / 3D polylines are created on the chosen layer.
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
(setq DGPS-LayerFromCsv nil)                          ; T = layer "1-TRACK_<Code>"

;; ---------------------------------------------------------------------------
;; Default layer list (shown in the layer-options dialog)
;; ---------------------------------------------------------------------------
(setq DGPS-DefaultLayers
  '("1-BANK CUTTING"
    "1-BOUNDARY"
    "1-BRIDGES"
    "1-BUILDINGS"
    "1-C-WALL R-WALL"
    "1-CABLE"
    "1-DRAINAGE ARRANGEMENT"
    "1-ECW"
    "1-FENCE"
    "1-LAYOUT"
    "1-LC"
    "1-LWR"
    "1-OHE"
    "1-PF SHELTER"
    "1-PLATFORM"
    "1-ROAD & TROLLEY PATH"
    "1-SURVEY"
    "1-TEXT"
    "1-TOE OF EMBANKMENT"
    "1-TR"
    "1-TRACK"
    "1-WATERWAY"
  )
)

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
(defun DGPS-SanitizeLayerName (s / src i ch out)
  (setq src (DGPS-Trim s)
        i   1
        out "")
  (while (<= i (strlen src))
    (setq ch (substr src i 1))
    (if (or (= ch " ")
            (= ch "<") (= ch ">") (= ch "/") (= ch "\\")
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
;; T when the code is  R  or  R<digits>  (R1, R2 ... R25 ...)
(defun DGPS-AutoRP-P (code / u rest)
  (setq u (strcase (DGPS-Trim code)))
  (cond
    ((= u "R") T)
    ((and (> (strlen u) 1) (= (substr u 1 1) "R"))
     (setq rest (substr u 2))
     (= (vl-string-trim "0123456789" rest) "")
    )
    (T nil)
  )
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
;; DIALOG 1 – Layer Selection
;;
;; Shows:
;;   • [Polyline] / [3D Polyline] toggle buttons  (radio-button feel)
;;   • Scrollable list of Code groups from the CSV
;;   • [Select Layers] button
;;
;; Returns a list:  (outType selectedCodes)
;;   outType       = "Polyline" | "3Dpolyline"
;;   selectedCodes = list of code strings the user highlighted, or nil = all
;;
;; Returns nil when the user cancels.
;; ===========================================================================
(defun DGPS-LayerSelectDialog (codenames / dcl_id dlgName result
                                outTypeChoice selectedList
                                allKeys autoIdx i)
  ;;
  ;; Write a temporary DCL file so we do not need a permanent .dcl on disk.
  ;;
  (setq dlgName (vl-filename-mktemp "dgpsLine" (getvar "TEMPPREFIX") ".dcl"))

  ;; Build DCL text
  (setq dcl_text
    (strcat
      "dgps_layer_sel : dialog {\n"
      "  label = \"Rail Track - Select Layer\";\n"
      "\n"
      "  : row {\n"
      "    label = \"Select layer\";\n"
      "    alignment = left;\n"
      "    : radio_button { key = \"rb_poly\";   label = \"Polyline\";    value = \"1\"; }\n"
      "    : radio_button { key = \"rb_3dpoly\"; label = \"3D Polyline\"; value = \"0\"; }\n"
      "  }\n"
      "\n"
      "  : boxed_column {\n"
      "    label = \"Options\";\n"
      "    : row {\n"
      "      : toggle   { key = \"tg_angle\"; label = \"Angle filter  +/-\"; }\n"
      "      : edit_box { key = \"edt_angle\"; label = \"\"; edit_width = 5; width = 8; fixed_width = true; }\n"
      "    }\n"
      "    : toggle { key = \"tg_time\";  label = \"Use Local Time column to order and connect points\"; }\n"
      "  }\n"
      "\n"
      "  : list_box {\n"
      "    key             = \"lst_codes\";\n"
      "    height          = 20;\n"
      "    width           = 48;\n"
      "    fixed_width_font = false;\n"
      "    multiple_select  = true;\n"
      "    allow_accept     = false;\n"
      "  }\n"
      "\n"
      "  spacer_1;\n"
      "  : row {\n"
      "    alignment = centered;\n"
      "    : button  { key = \"btn_select\"; label = \"Select Layers\"; width = 16; fixed_width = true; is_default = true; }\n"
      "    : button  { key = \"cancel\";     label = \"Cancel\";        width = 10; fixed_width = true; is_cancel  = true; }\n"
      "  }\n"
      "}\n"
    )
  )

  ;; Write DCL file
  (setq f (open dlgName "w"))
  (write-line dcl_text f)
  (close f)

  ;; Load DCL
  (setq dcl_id (load_dialog dlgName))
  (setq result nil)

  (if (and dcl_id (new_dialog "dgps_layer_sel" dcl_id))
    (progn
      ;; Default: Polyline selected
      (setq outTypeChoice "Polyline")
      (set_tile "rb_poly"   "1")
      (set_tile "rb_3dpoly" "0")

      ;; Option check boxes (last used values)
      (set_tile "tg_angle" (if DGPS-UseAngle "1" "0"))
      (set_tile "edt_angle" (itoa (fix DGPS-AngleThreshold)))
      (mode_tile "edt_angle" (if DGPS-UseAngle 0 1))
      (set_tile "tg_time"  (if DGPS-UseTime  "1" "0"))

      ;; Populate list – sorted ascending
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

      ;; Radio button callbacks
      (action_tile "rb_poly"
        "(setq outTypeChoice \"Polyline\")
         (set_tile \"rb_poly\"   \"1\")
         (set_tile \"rb_3dpoly\" \"0\")"
      )
      (action_tile "rb_3dpoly"
        "(setq outTypeChoice \"3Dpolyline\")
         (set_tile \"rb_poly\"   \"0\")
         (set_tile \"rb_3dpoly\" \"1\")"
      )

      ;; Angle toggle enables / disables the angle box
      (action_tile "tg_angle"
        "(mode_tile \"edt_angle\" (if (= $value \"1\") 0 1))"
      )

      ;; Select Layers button
      (action_tile "btn_select"
        (strcat
          "(if (DGPS-AngleOK) (progn"
          "(setq selIdxStr (get_tile \"lst_codes\"))"
          "(setq DGPS-UseAngle (= (get_tile \"tg_angle\") \"1\"))"
          "(setq DGPS-UseTime  (= (get_tile \"tg_time\")  \"1\"))"
          "(done_dialog 1)))"
        )
      )

      ;; Cancel
      (action_tile "cancel" "(done_dialog 0)")

      ;; Run dialog
      (setq dlgRet (start_dialog))

      (if (= dlgRet 1)
        (progn
          ;; Parse selected indices (space-separated string from list_box)
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
          (setq result (list outTypeChoice selectedList))
        )
        (setq result nil)  ; cancelled
      )
    )
    (progn
      (princ "\nVIDDGPSTOLINE: could not open layer-selection dialog.")
      (setq result nil)
    )
  )

  (unload_dialog dcl_id)
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  result
)

;; ===========================================================================
;; DIALOG 2 – Layer Options
;;
;; Called after the user clicks [Select Layers].
;; Shows:
;;   • radio: "Default layers"  -> scrollable list of DGPS-DefaultLayers
;;   • radio: "Custom layer for <codes>"
;;   • text edit for custom name
;;   • [OK] / [Cancel]
;;
;; Returns chosen layer name string, or nil if cancelled.
;; ===========================================================================
(defun DGPS-LayerOptionsDialog (selectedCodes / dcl_id dlgName result
                                 modeChoice customName defaultChoice
                                 codeLabel dcl_text f selIdxStr dlgRet idx nm)

  (setq dlgName (vl-filename-mktemp "dgpsOpt" (getvar "TEMPPREFIX") ".dcl"))

  ;; Build label for codes in use
  (if (and selectedCodes (> (length selectedCodes) 0))
    (progn
      (setq codeLabel "")
      (foreach c selectedCodes
        (setq codeLabel (strcat codeLabel (if (= codeLabel "") "" ", ") c))
      )
      (if (> (strlen codeLabel) 40)
        (setq codeLabel (strcat (substr codeLabel 1 37) "..."))
      )
    )
    (setq codeLabel "all codes")
  )

  (setq dcl_text
    (strcat
      "dgps_layer_opt : dialog {\n"
      "  label = \"Layer Options\";\n"
      "\n"
      "  : row {\n"
      "    : radio_button { key = \"rb_default\"; label = \"Default layers\";  value = \"1\"; }\n"
      "    : radio_button { key = \"rb_csv\";     label = \"From CSV\"; value = \"0\"; }\n"
      "  }\n"
      "\n"
      "  : list_box {\n"
      "    key             = \"lst_default\";\n"
      "    height          = 20;\n"
      "    width           = 48;\n"
      "    fixed_width_font = false;\n"
      "    multiple_select  = false;\n"
      "    allow_accept     = false;\n"
      "  }\n"
      "\n"
      "  : radio_button { key = \"rb_custom\"; label = \"Custom layer for ("
      codeLabel
      ")\"; value = \"0\"; }\n"
      "\n"
      "  : edit_box {\n"
      "    key   = \"edt_custom\";\n"
      "    label = \"Custom layer name\";\n"
      "    width = 40;\n"
      "    edit_width = 36;\n"
      "    is_enabled = false;\n"
      "  }\n"
      "\n"
      "  spacer_1;\n"
      "  : row {\n"
      "    alignment = centered;\n"
      "    : button { key = \"accept\"; label = \"OK\";     width = 10; fixed_width = true; is_default = true; }\n"
      "    : button { key = \"cancel\"; label = \"Cancel\"; width = 10; fixed_width = true; is_cancel  = true; }\n"
      "  }\n"
      "}\n"
    )
  )

  ;; Write DCL
  (setq f (open dlgName "w"))
  (write-line dcl_text f)
  (close f)

  (setq dcl_id (load_dialog dlgName))
  (setq result nil)

  (if (and dcl_id (new_dialog "dgps_layer_opt" dcl_id))
    (progn
      (setq modeChoice "default")
      (setq defaultChoice nil)
      (setq customName "")

      ;; Radio defaults
      (setq DGPS-LayerFromCsv nil)
      (set_tile "rb_default" "1")
      (set_tile "rb_csv"     "0")
      (set_tile "rb_custom"  "0")

      ;; Populate default layer list
      (start_list "lst_default" 3)
      (foreach lyr DGPS-DefaultLayers (add_list lyr))
      (end_list)

      ;; Pre-select first item
      (set_tile "lst_default" "0")
      (setq defaultChoice (nth 0 DGPS-DefaultLayers))

      ;; Radio callbacks
      (action_tile "rb_default"
        (strcat
          "(setq modeChoice \"default\")"
          "(set_tile \"rb_default\" \"1\")"
          "(set_tile \"rb_csv\"     \"0\")"
          "(set_tile \"rb_custom\"  \"0\")"
          "(mode_tile \"lst_default\" 0)"
          "(mode_tile \"edt_custom\"  1)"
        )
      )
      (action_tile "rb_csv"
        (strcat
          "(setq modeChoice \"csv\")"
          "(set_tile \"rb_default\" \"0\")"
          "(set_tile \"rb_csv\"     \"1\")"
          "(set_tile \"rb_custom\"  \"0\")"
          "(mode_tile \"lst_default\" 1)"
          "(mode_tile \"edt_custom\"  1)"
        )
      )
      (action_tile "rb_custom"
        (strcat
          "(setq modeChoice \"custom\")"
          "(set_tile \"rb_default\" \"0\")"
          "(set_tile \"rb_csv\"     \"0\")"
          "(set_tile \"rb_custom\"  \"1\")"
          "(mode_tile \"lst_default\" 1)"
          "(mode_tile \"edt_custom\"  0)"
          "(mode_tile \"edt_custom\"  2)"
        )
      )

      ;; List selection callback
      (action_tile "lst_default"
        (strcat
          "(setq selIdxStr (get_tile \"lst_default\"))"
          "(if (and selIdxStr (/= (vl-string-trim \" \" selIdxStr) \"\"))"
          "  (setq defaultChoice (nth (atoi selIdxStr) DGPS-DefaultLayers))"
          ")"
        )
      )

      ;; Custom name edit callback
      (action_tile "edt_custom"
        "(setq customName (get_tile \"edt_custom\"))"
      )

      ;; OK
      (action_tile "accept"
        (strcat
          "(setq customName (get_tile \"edt_custom\"))"
          "(setq selIdxStr  (get_tile \"lst_default\"))"
          "(if (and selIdxStr (/= (vl-string-trim \" \" selIdxStr) \"\"))"
          "  (setq defaultChoice (nth (atoi selIdxStr) DGPS-DefaultLayers))"
          ")"
          "(done_dialog 1)"
        )
      )
      (action_tile "cancel" "(done_dialog 0)")

      (setq dlgRet (start_dialog))

      (if (= dlgRet 1)
        (progn
          (cond
            ((= modeChoice "csv")
             (setq DGPS-LayerFromCsv T)
             (setq result "1-TRACK_")
            )
            ((= modeChoice "custom")
             (setq customName (DGPS-Trim customName))
             (if (= customName "")
               (progn
                 (alert "Custom layer name cannot be blank. Using default.")
                 (setq result defaultChoice)
               )
               (setq result customName)
             )
            )
            (T  ; "default"
             (if defaultChoice
               (setq result defaultChoice)
               (setq result (nth 0 DGPS-DefaultLayers))
             )
            )
          )
        )
        (setq result nil)
      )
    )
    (progn
      (princ "\nVIDDGPSTOLINE: could not open layer-options dialog.")
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
;; Build geometry for one group of records
;;
;; Pipeline:
;;   1. Nearest-neighbour chain  (spatial ordering, no zig-zag)
;;   2. Longest-edge removal     (open the loop at the biggest gap)
;;   3. Angular-deviation filter (iteratively remove bad GPS vertices >5°)
;;   4. Create open polyline
;;
;; Edge cases:
;;   0 points → skip
;;   1 point  → skip
;;   2 points → direct open line (no loop removal or angle filter)
;;
;; Returns: (entity 0 singleCt failures)
;; ===========================================================================
(defun DGPS-BuildGeometry (codeRecs code outType targetLayer
                           / chainedRecs nPts i x y z
                             allPts3D allPts2D
                             loopPts loopLen
                             maxLen maxIdx edgeLen
                             fromPt toPt startIdx
                             openPts3D openPts2D
                             filterResult filteredPts removedCt
                             finalPts fromName toName result)

  (setq nPts (length codeRecs))

  (cond

    ;; ── 0 points ────────────────────────────────────────────────────────────
    ((= nPts 0)
     (princ (strcat "\n" code))
     (princ         "\n  Points           : 0")
     (princ         "\n  Result            : SKIPPED (no valid points)")
     (list nil 0 1 0)
    )

    ;; ── 1 point ─────────────────────────────────────────────────────────────
    ((= nPts 1)
     (princ (strcat "\n" code))
     (princ         "\n  Points           : 1")
     (princ         "\n  Result            : SKIPPED (single point, no polyline)")
     (list nil 0 1 0)
    )

    ;; ── 2+ points ───────────────────────────────────────────────────────────
    (T
     (DGPS-EnsureLayer targetLayer 3)

     ;; ── Step 1: Nearest-neighbour chain ─────────────────────────────────
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

       ;; ── 2 points: direct open line, skip loop removal + angle filter ───
       (progn
         (setq finalPts (if (= outType "3Dpolyline") allPts3D allPts2D))
         (setq finalPts (DGPS-OrientAlongHP finalPts))
         (setq result
           (if (= outType "3Dpolyline")
             (DGPS-Make3DPolyline finalPts targetLayer)
             (DGPS-Make2DPolyline finalPts targetLayer)
           )
         )
         (princ (strcat "\n" code))
         (princ         "\n  Points (chained) : 2")
         (princ (strcat "\n  Order            : " (if DGPS-UseTime "LOCAL TIME" "NEAREST POINT (both ends)")))
         (princ         "\n  Longest Edge     : N/A (2-point direct line)")
         (princ         "\n  Removed Edge     : NONE")
         (princ         "\n  Angle Filter     : N/A")
         (princ         "\n  Final Points     : 2")
         (princ (strcat "\n  Layer            : " targetLayer))
         (princ (strcat "\n  Type             : " outType))
         (princ (strcat "\n  Direction        : "
                        (if DGPS-HPSamples
                          (if DGPS-LastFlipped "REVERSED to follow HP" "already along HP")
                          "N/A (no HP rows)")))
         (princ (strcat "\n  Result            : "
                        (if result "OPEN POLYLINE" "FAILED")))
         (list result 0 0 (if result 0 1))
       )

       ;; ── 3+ points ──────────────────────────────────────────────────────
       (progn

         ;; ── Step 2: Longest-edge removal (open the loop) ─────────────────
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
               (list (nth (rem (+ startIdx i) nPts) allPts3D)))
           )
           (setq openPts2D
             (append openPts2D
               (list (nth (rem (+ startIdx i) nPts) allPts2D)))
           )
           (setq i (1+ i))
         )

         ;; Names of the removed edge endpoints (for report)
         (setq fromName
           (DGPS-R-PName (nth maxIdx chainedRecs)))
         (setq toName
           (DGPS-R-PName (nth (rem (1+ maxIdx) nPts) chainedRecs)))
         ))

         ;; ── Step 3: Angular-deviation filter (iterative, >5° → remove) ──
         ;;
         ;; Run the filter on paired (2D . 3D) cons cells so both lists
         ;; stay perfectly in sync — no coordinate matching required.
         ;;
         ;; Build a paired list: each element is (pt2D . pt3D)
         (setq pairedPts '()
               i          0)
         (while (< i nPts)
           (setq pairedPts
             (append pairedPts
               (list (cons (nth i openPts2D) (nth i openPts3D)))
             )
           )
           (setq i (1+ i))
         )

         ;; Extract just the 2D half for the angle filter
         (setq only2D (mapcar 'car pairedPts))

         (setq filterResult
           (if DGPS-UseAngle
             (DGPS-AngleFilter only2D)
             (list only2D 0)))
         (setq filteredPts  (car  filterResult)
               removedCt    (cadr filterResult))

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

         ;; ── Step 4: Create open polyline ─────────────────────────────────
         (setq finalPts (if (= outType "3Dpolyline") openPts3D openPts2D))
         (setq finalPts (DGPS-OrientAlongHP finalPts))
         (setq result
           (if (= outType "3Dpolyline")
             (DGPS-Make3DPolyline finalPts targetLayer)
             (DGPS-Make2DPolyline finalPts targetLayer)
           )
         )

         ;; Report
         (princ (strcat "\n" code))
         (princ (strcat "\n  Points (chained) : " (itoa nPts)))
         (princ (strcat "\n  Order            : " (if DGPS-UseTime "LOCAL TIME" "NEAREST POINT (both ends)")))
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
         (princ (strcat "\n  Final Points     : "
                        (itoa (length finalPts))))
         (princ (strcat "\n  Layer            : " targetLayer))
         (princ (strcat "\n  Type             : " outType))
         (princ (strcat "\n  Direction        : "
                        (if DGPS-HPSamples
                          (if DGPS-LastFlipped "REVERSED to follow HP" "already along HP")
                          "N/A (no HP rows)")))
         (princ (strcat "\n  Result            : "
                        (if result "OPEN POLYLINE" "FAILED")))

         (list result 0 0 (if result 0 1))
       )
     )
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
     dlgResult outType selectedCodes targetLayer
     g codeRecs geomResult
     geomCount singleCount geomFailures totalMoved
     layersCreated i sampleCount codeLayer
     hpRecs otherRecs hpCh hpGeom)

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
  ;; 3. Dialog 1: Layer selection
  ;; -------------------------------------------------------------------------
  (setq dlgResult (DGPS-LayerSelectDialog codenames))
  (if (null dlgResult)
    (progn (DGPS-Error "Cancelled by user.") (exit))
  )

  (setq outType       (nth 0 dlgResult))
  (setq selectedCodes (nth 1 dlgResult))

  ;; -------------------------------------------------------------------------
  ;; 4. Dialog 2: Layer options
  ;; -------------------------------------------------------------------------
  (setq targetLayer (DGPS-LayerOptionsDialog selectedCodes))
  (if (null targetLayer)
    (progn (DGPS-Error "Cancelled by user.") (exit))
  )

  ;; Sanitize user-supplied name just in case
  (setq targetLayer (DGPS-SanitizeLayerName targetLayer))

  ;; -------------------------------------------------------------------------
  ;; 5. Report input summary
  ;; -------------------------------------------------------------------------
  (princ "\n========================================")
  (princ "\nVIDDGPSTOLINE CSV IMPORT")
  (princ "\n========================================")
  (princ (strcat "\nFile:         " csvFile))
  (princ (strcat "\nEncoding:     " enc))
  (princ (strcat "\nOutput type:  " outType))
  (princ (strcat "\nTarget layer: "
                 (if DGPS-LayerFromCsv "1-TRACK_<Code> (from CSV)" targetLayer)))
  (princ (strcat "\nAngle filter: " (if DGPS-UseAngle
                                        (strcat "ON (+/- " (rtos DGPS-AngleThreshold 2 0) " deg)")
                                        "OFF")))
  (princ (strcat "\nPoint order:  " (if DGPS-UseTime "Local Time" "Nearest point")))
  (princ (strcat "\nData rows:    " (itoa dataRows)))
  (princ (strcat "\nValid:        " (itoa validRows)))
  (princ (strcat "\nSkipped:      " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes: " (itoa (length groups))))

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

  ;; Ensure target layer exists once (From CSV mode creates one per Code below)
  (if (not DGPS-LayerFromCsv) (DGPS-EnsureLayer targetLayer 3))

  ;; HP curve first, so its tangent can orient the track polylines
  (setq DGPS-HPSamples nil)
  (setq hpGeom nil)
  (if hpRecs
    (setq hpGeom (DGPS-BuildHP hpRecs outType))
  )

  (foreach g groups
    (setq code     (car  g))
    (setq codeRecs (cdr  g))

    ;; Filter: skip codes not in the user's selection (if a selection was made)
    (if (or (null selectedCodes)
            (= (length selectedCodes) 0)
            (member code selectedCodes))
      (progn
        (setq codeLayer
          (if DGPS-LayerFromCsv
            (DGPS-SanitizeLayerName (strcat "1-TRACK_" code))
            targetLayer
          )
        )
        (if DGPS-LayerFromCsv (DGPS-EnsureLayer codeLayer 3))
        (setq geomResult
          (DGPS-BuildGeometry codeRecs code outType codeLayer)
        )
        (setq totalMoved   (+ totalMoved   (nth 1 geomResult)))
        (setq singleCount  (+ singleCount  (nth 2 geomResult)))
        (setq geomFailures (+ geomFailures (nth 3 geomResult)))
        (if (nth 0 geomResult)
          (setq geomCount (1+ geomCount))
        )
        (if (null (member codeLayer layersCreated))
          (setq layersCreated (cons codeLayer layersCreated))
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
  (princ (strcat "\nOutput type:      " outType))
  (princ (strcat "\nTarget layer:     "
                 (if DGPS-LayerFromCsv "1-TRACK_<Code> (from CSV)" targetLayer)))
  (princ (strcat "\nRows read:        " (itoa dataRows)))
  (princ (strcat "\nValid records:    " (itoa validRows)))
  (princ (strcat "\nSkipped rows:     " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes:     " (itoa (length groups))))
  (princ (strcat "\nGeometry created: " (itoa geomCount)))
  (princ (strcat "\nGeometry failed:  " (itoa geomFailures)))
  (princ (strcat "\nSingle-point:     " (itoa singleCount)))
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

(princ "\nVIDDGPSTOLINE_v25 loaded. Type VIDDGPSTOLINE to run.")
(princ)
