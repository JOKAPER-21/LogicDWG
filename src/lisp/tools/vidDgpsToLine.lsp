;;; ============================================================================
;;; Vid Dgps To Line
;;; Release: 1.4.0 | Civil 3D 2026
;;; Version: 05
;;; ============================================================================
;;;
;;; CHANGE LOG v1.4.0 (Version 05)
;;;   - Removed Local Time ordering entirely.
;;;   - Points within each Code are now chained by nearest-neighbour:
;;;       Start from the first point in CSV row order.
;;;       Repeatedly pick the closest unvisited point in the same Code.
;;;       This eliminates zig-zag artefacts caused by GPS survey order.
;;;   - Longest-edge removal preserved: after nearest-neighbour chaining,
;;;       a temporary closed loop is still evaluated to remove the largest
;;;       gap edge and produce a clean open polyline.
;;;   - DGPS-TimeKey / DGPS-TimeLess / DGPS-OrderForward / DGPS-CountMoved
;;;       kept as dead code for compatibility; not invoked.
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
                                allKeys)
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

      ;; Populate list – sorted ascending
      (setq sortedCodes (vl-sort codenames '(lambda (a b) (< (strcase a) (strcase b)))))
      (start_list "lst_codes" 3)
      (foreach c sortedCodes (add_list c))
      (end_list)

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

      ;; Select Layers button
      (action_tile "btn_select"
        (strcat
          "(setq selIdxStr (get_tile \"lst_codes\"))"
          "(done_dialog 1)"
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
      "  : radio_button { key = \"rb_default\"; label = \"Default layers\";  value = \"1\"; }\n"
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
      (set_tile "rb_default" "1")
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
          "(set_tile \"rb_custom\"  \"0\")"
          "(mode_tile \"lst_default\" 0)"
          "(mode_tile \"edt_custom\"  1)"
        )
      )
      (action_tile "rb_custom"
        (strcat
          "(setq modeChoice \"custom\")"
          "(set_tile \"rb_default\" \"0\")"
          "(set_tile \"rb_custom\"  \"1\")"
          "(mode_tile \"lst_default\" 1)"
          "(mode_tile \"edt_custom\"  0)"
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
;; DGPS-NNChain
;;   Nearest-neighbour greedy chain over a list of records.
;;   Starts from the first record in the list (CSV row order).
;;   At each step picks the closest unvisited record by 2-D plan distance.
;;   Returns the re-ordered list of records.
;; ---------------------------------------------------------------------------
(defun DGPS-NNChain (recs / remaining result cur best bestD d cx cy bx by rx ry)
  (if (or (null recs) (null (cdr recs)))
    recs
    (progn
      (setq cur       (car recs)
            remaining (cdr recs)
            result    (list cur))
      (while remaining
        (setq cx     (atof (DGPS-R-East  cur))
              cy     (atof (DGPS-R-North cur))
              best   nil
              bestD  nil)
        (foreach r remaining
          (setq rx (atof (DGPS-R-East  r))
                ry (atof (DGPS-R-North r))
                d  (+ (* (- rx cx) (- rx cx))
                       (* (- ry cy) (- ry cy))))  ; squared dist, no sqrt needed
          (if (or (null bestD) (< d bestD))
            (setq best r  bestD d)
          )
        )
        (setq result    (append result (list best))
              remaining (vl-remove best remaining)
              cur       best)
      )
      result
    )
  )
)

;; ===========================================================================
;; Build geometry for one group of records
;;
;; Algorithm:
;;   1. Take records in CSV row order (no time sort).
;;   2. Chain them by nearest-neighbour (greedy, no zig-zag).
;;   3. Build a temporary closed loop and find the longest edge (the gap).
;;   4. Remove that edge → final OPEN polyline.
;;
;; Edge cases:
;;   0 points → skip
;;   1 point  → skip
;;   2 points → direct open line (no loop/removal needed)
;;
;; Returns: (entity 0 singleCt failures)
;; ===========================================================================
(defun DGPS-BuildGeometry (codeRecs code outType targetLayer
                           / chainedRecs nPts i rec x y z
                             allPts3D allPts2D
                             loopPts loopLen
                             maxLen maxIdx edgeLen
                             fromPt toPt startIdx
                             orderedPts finalPts
                             fromName toName result)

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

     ;; Step 1: nearest-neighbour chain (eliminates zig-zag)
     (setq chainedRecs (DGPS-NNChain codeRecs))
     (setq nPts (length chainedRecs))

     ;; Step 2: build coordinate arrays from chained order
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

       ;; ── 2 points: direct open line ──────────────────────────────────────
       (progn
         (setq finalPts (if (= outType "3Dpolyline") allPts3D allPts2D))
         (setq result
           (if (= outType "3Dpolyline")
             (DGPS-Make3DPolyline finalPts targetLayer)
             (DGPS-Make2DPolyline finalPts targetLayer)
           )
         )
         (princ (strcat "\n" code))
         (princ         "\n  Points           : 2")
         (princ         "\n  Order            : NEAREST-NEIGHBOUR")
         (princ         "\n  Longest Edge     : N/A (2-point direct line)")
         (princ         "\n  Removed Edge     : NONE")
         (princ (strcat "\n  Layer            : " targetLayer))
         (princ (strcat "\n  Type             : " outType))
         (princ (strcat "\n  Result            : "
                        (if result "OPEN POLYLINE" "FAILED")))
         (list result 0 0 (if result 0 1))
       )

       ;; ── 3+ points: temporary closed loop → remove longest edge ──────────
       ;; The chained points form a clean spatial sequence.
       ;; Close the loop (Pn-1 → P0), find the longest gap, then open it.
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

         ;; Open the loop: start from the vertex after the longest edge
         (setq startIdx (rem (1+ maxIdx) nPts))
         (setq orderedPts '()
               i           0)
         (while (< i nPts)
           (setq orderedPts
             (append orderedPts
               (list (nth (rem (+ startIdx i) nPts)
                          (if (= outType "3Dpolyline") allPts3D allPts2D)))
             )
           )
           (setq i (1+ i))
         )

         ;; Point names for report
         (setq fromName (DGPS-R-PName (nth maxIdx chainedRecs)))
         (setq toName   (DGPS-R-PName (nth (rem (1+ maxIdx) nPts) chainedRecs)))

         ;; Create entity
         (setq result
           (if (= outType "3Dpolyline")
             (DGPS-Make3DPolyline orderedPts targetLayer)
             (DGPS-Make2DPolyline orderedPts targetLayer)
           )
         )

         (princ (strcat "\n" code))
         (princ (strcat "\n  Points           : " (itoa nPts)))
         (princ         "\n  Order            : NEAREST-NEIGHBOUR")
         (princ (strcat "\n  Longest Edge     : " (DGPS-FormatDist maxLen)))
         (princ (strcat "\n  Removed Edge     : " fromName " -> " toName))
         (princ (strcat "\n  Layer            : " targetLayer))
         (princ (strcat "\n  Type             : " outType))
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
     layersCreated i sampleCount)

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
  (setq csvFile (getfiled "Select DGPS CSV File" "" "csv" 4))
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
  (setq groups    (DGPS-GroupByCode allRecords))
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
  (princ (strcat "\nTarget layer: " targetLayer))
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

  ;; Ensure target layer exists once
  (DGPS-EnsureLayer targetLayer 3)

  (foreach g groups
    (setq code     (car  g))
    (setq codeRecs (cdr  g))

    ;; Filter: skip codes not in the user's selection (if a selection was made)
    (if (or (null selectedCodes)
            (= (length selectedCodes) 0)
            (member code selectedCodes))
      (progn
        (setq geomResult
          (DGPS-BuildGeometry codeRecs code outType targetLayer)
        )
        (setq totalMoved   (+ totalMoved   (nth 1 geomResult)))
        (setq singleCount  (+ singleCount  (nth 2 geomResult)))
        (setq geomFailures (+ geomFailures (nth 3 geomResult)))
        (if (nth 0 geomResult)
          (setq geomCount (1+ geomCount))
        )
        (if (null (member targetLayer layersCreated))
          (setq layersCreated (cons targetLayer layersCreated))
        )
      )
      ;; Skipped (not in filter)
      (princ (strcat "\nSkipped (not selected): " code))
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 7. Final report
  ;; -------------------------------------------------------------------------
  (princ "\n\n========================================")
  (princ "\nVIDDGPSTOLINE COMPLETE")
  (princ "\n========================================")
  (princ (strcat "\nOutput type:      " outType))
  (princ (strcat "\nTarget layer:     " targetLayer))
  (princ (strcat "\nRows read:        " (itoa dataRows)))
  (princ (strcat "\nValid records:    " (itoa validRows)))
  (princ (strcat "\nSkipped rows:     " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes:     " (itoa (length groups))))
  (princ (strcat "\nGeometry created: " (itoa geomCount)))
  (princ (strcat "\nGeometry failed:  " (itoa geomFailures)))
  (princ (strcat "\nSingle-point:     " (itoa singleCount)))
  (princ (strcat "\nAlgorithm:        Nearest-neighbour + longest-edge removal (v05)"))

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

(princ "\nVIDDGPSTOLINE_v18 loaded. Type VIDDGPSTOLINE to run.")
(princ)
