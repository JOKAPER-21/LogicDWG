;;; ============================================================================
;;; Vid Dgps To Box
;;; Command : VIDDGPSTOBOX
;;; ----------------------------------------------------------------------------
;;; Builds a PERFECT RECTANGLE from the 3 or 4 DGPS points of every Code group
;;; (e.g. "Motor Room") in a CSV file.
;;;
;;; WORKFLOW
;;;   1. Select the DGPS CSV file.
;;;   2. Dialog 1 [Select Layers] : pick the Code groups (one or many).
;;;   3. Dialog 2 [Layer Options] : pick a default layer or type a custom layer.
;;;      Press [OK].
;;;   4. One closed LWPOLYLINE rectangle is created for each group.
;;;
;;; RECTANGLE RULE  (points are sorted by "Local Time" inside each group)
;;;   rec(0) = 1st point  -> start corner
;;;   rec(1) = 2nd point  -> gives the DIRECTION and the LENGTH (rec0 to rec1)
;;;   rec(2) = 3rd point  -> gives the WIDTH (its offset across the direction),
;;;                          on whichever side rec(2) lies
;;;   rec(3) = computed automatically (4th DGPS point, if any, is not needed)
;;;
;;;        rec(3) *------------------* rec(2)
;;;               |                  |
;;;               |                  |
;;;        rec(0) *------------------* rec(1)
;;;
;;; GROUP SIZE
;;;   3 or 4 points -> one rectangle.
;;;   More points   -> split in time order: multiple of 4 -> sets of 4,
;;;                    else multiple of 3 -> sets of 3, else sets of 4 and the
;;;                    remainder is reported (a remainder of 3 is still used).
;;;   Fewer than 3  -> skipped and reported.
;;;
;;; Elevation of the rectangle = average elevation of the points used.
;;; Dialog 1 auto-selects room / building style groups (VDB-AutoSelectList);
;;; the CSV word APANDENT is written as ABANDONED BUILDING in the MTEXT.
;;; Each box also gets an MTEXT (Code) at its centre, middle-centre justified,
;;; width = text length, rotated to the tangent of the nearest curve on layer
;;; 1-TRACK (box direction if none), on the same layer as the box.
;;; ============================================================================

(vl-load-com)

(setq vdb-*fh*          nil)
;; MTEXT settings (edit freely)
(setq VDB-TrackLayer  "1-TRACK")   ; layer whose tangent sets the text rotation
(setq VDB-TextHeight  0.5)         ; text height in drawing units
(setq VDB-TrackSS     nil)
(setq VDB-CsvPrefix   "1-TRACK_")  ; [From CSV] layer = prefix + Code
(setq VDB-FromCsv     nil)
(setq VDB-TextFlip180 T)           ; T = turn the label 180 deg from the track tangent

;; Groups selected automatically in dialog 1 when a Code contains one of these.
;;   "word"  = the Code contains the entry as a whole word  (Motor Room -> ROOM)
;;   "exact" = the whole Code must equal the entry
(setq VDB-AutoMatchMode "word")
(setq VDB-AutoSelectList
  '("ROOM" "ROOMS" "BUILD" "BUILDING" "BUILDINGS" "TOILET" "TOILETS" "LAV"
    "APANDENT" "ABANDONED" "STATION" "STATION BUILDING" "STATION BUILDINGS"
    "BLDG" "SHED" "SHET" "OFFICE" "OFFICES" "PARKING" "PARKINGS"
    "CONSTRUCTION" "QUARTER" "QUARTERS" "HOTEL" "HOTELS" "SHOP" "SHOPS"))

;; MTEXT wording replacements  (CSV word . text placed in the drawing)
(setq VDB-TextReplace '(("APANDENT" . "ABANDONED BUILDING")))
(setq vdb-*old-error*   nil)
(setq vdb-*old-clayer*  nil)
(setq vdb-*old-cmdecho* nil)
(setq vdb-*old-osmode*  nil)

;; Layers offered in dialog 2 (edit freely)
(setq VDB-DefaultLayers
  '("1-BUILDINGS"
    "1-BOUNDARY"
    "1-BRIDGES"
    "1-C-WALL R-WALL"
    "1-DRAINAGE ARRANGEMENT"
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
    "1-TR"
    "1-TRACK"
    "1-WATERWAY"
  )
)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------
(defun VDB-Trim (s)
  (if s (vl-string-trim " \t\r\n" s) "")
)

(defun VDB-Upper (s) (strcase (VDB-Trim s)))

(defun VDB-GetField (fields idx)
  (if (and idx (>= idx 0) (< idx (length fields)))
    (VDB-Trim (nth idx fields))
    ""
  )
)

(defun VDB-Num (s)
  (if (and s (/= (VDB-Trim s) ""))
    (distof (VDB-Trim s) 2)
    nil
  )
)

(defun VDB-HeaderIndex (headers wanted / i found)
  (setq i 0 found nil)
  (while (and (< i (length headers)) (null found))
    (if (= (VDB-Upper (nth i headers)) (VDB-Upper wanted))
      (setq found i)
    )
    (setq i (1+ i))
  )
  found
)

(defun VDB-StripBOM (s)
  (if (and s (>= (strlen s) 3)
           (= (ascii (substr s 1 1)) 239)
           (= (ascii (substr s 2 1)) 187)
           (= (ascii (substr s 3 1)) 191))
    (substr s 4)
    s
  )
)

(defun VDB-SanitizeLayerName (s / src i ch out)
  (setq src (VDB-Trim s) i 1 out "")
  (while (<= i (strlen src))
    (setq ch (substr src i 1))
    (if (or (= ch "<") (= ch ">") (= ch "/") (= ch "\\")
            (= ch "\"") (= ch ":") (= ch ";") (= ch "?")
            (= ch "*") (= ch "|") (= ch ",") (= ch "=") (= ch "`"))
      (setq out (strcat out "_"))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  (if (= out "") (setq out "_NOCODE"))
  (vl-string-trim " " out)
)

(defun VDB-EnsureLayer (name color)
  (if (tblsearch "LAYER" name)
    name
    (if (entmakex
          (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                '(100 . "AcDbLayerTableRecord") (cons 2 name)
                '(70 . 0) (cons 62 color) '(6 . "Continuous")))
      name
      nil
    )
  )
)

;; ---------------------------------------------------------------------------
;; CSV reading (binary safe - DGPS files contain 0x1A in lat/long text)
;; ---------------------------------------------------------------------------
(defun VDB-ParseCSV (s / i n ch inquote cur fields)
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

(defun VDB-ReadLine (fh / b s gotLine)
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
;; Records:  (code x y z time row)     x = Easting, y = Northing
;; ---------------------------------------------------------------------------
(defun VDB-RCode (r) (nth 0 r))
(defun VDB-RPt   (r) (list (nth 1 r) (nth 2 r) (nth 3 r)))
(defun VDB-RTime (r) (nth 4 r))
(defun VDB-RRow  (r) (nth 5 r))

;; "Local Time" is ISO (yyyy-mm-dd hh:mm:ss.mmm) so plain string compare sorts it
(defun VDB-TimeLess (a b / ta tb)
  (setq ta (VDB-RTime a) tb (VDB-RTime b))
  (cond
    ((and (/= ta "") (= tb "")) T)
    ((and (= ta "") (/= tb "")) nil)
    ((/= ta tb) (< ta tb))
    (T (< (VDB-RRow a) (VDB-RRow b)))
  )
)

(defun VDB-GroupByCode (records / groups code cell)
  (setq groups '())
  (foreach rec records
    (setq code (VDB-RCode rec))
    (setq cell (assoc code groups))
    (if cell
      (setq groups (subst (append cell (list rec)) cell groups))
      (setq groups (append groups (list (cons code (list rec)))))
    )
  )
  groups
)

;; ---------------------------------------------------------------------------
;; Rectangle maths
;; recs = 3 or 4 records, already time-sorted.  Returns 4 corner points or nil.
;; ---------------------------------------------------------------------------
(defun VDB-RectCorners (recs / p0 p1 p2 dx dy d ux uy nx ny vx vy L W z c1 c2 c3 cnt)
  (setq p0 (VDB-RPt (nth 0 recs))
        p1 (VDB-RPt (nth 1 recs))
        p2 (VDB-RPt (nth 2 recs)))
  (setq dx (- (car p1) (car p0))
        dy (- (cadr p1) (cadr p0))
        d  (sqrt (+ (* dx dx) (* dy dy))))
  (if (< d 1e-6)
    nil
    (progn
      (setq ux (/ dx d) uy (/ dy d)
            nx (- uy)   ny ux)
      (setq vx (- (car p2) (car p0))
            vy (- (cadr p2) (cadr p0)))
      (setq L d                           ; length = distance rec(0)->rec(1)
            W (+ (* vx nx) (* vy ny)))    ; width  = rec(2) offset across it
      (if (< (abs W) 1e-6)
        nil
        (progn
          (setq z 0.0 cnt 0)
          (foreach r recs (setq z (+ z (nth 3 r)) cnt (1+ cnt)))
          (setq z (/ z cnt))
          (list
            (list (car p0) (cadr p0) z)
            (list (+ (car p0) (* ux L)) (+ (cadr p0) (* uy L)) z)
            (list (+ (car p0) (* ux L) (* nx W)) (+ (cadr p0) (* uy L) (* ny W)) z)
            (list (+ (car p0) (* nx W)) (+ (cadr p0) (* ny W)) z)
          )
        )
      )
    )
  )
)

(defun VDB-MakeRect (corners layer / data)
  (setq data
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)                              ; closed
      (cons 38 (caddr (car corners)))        ; elevation
    )
  )
  (foreach p corners
    (setq data (append data (list (cons 10 (list (car p) (cadr p))))))
  )
  (entmakex data)
)

;; ---------------------------------------------------------------------------
;; Auto-select + wording helpers
;; ---------------------------------------------------------------------------
;; " ROOM " style normal form: separators become spaces, upper case
(defun VDB-NormWords (s / i ch out)
  (setq s (strcase (VDB-Trim s)) i 1 out " ")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (member ch '(" " "_" "-" "/" "." "," "(" ")" "&" "\t"))
      (if (/= (substr out (strlen out) 1) " ") (setq out (strcat out " ")))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  (if (= (substr out (strlen out) 1) " ") out (strcat out " "))
)

(defun VDB-AutoMatchP (code / norm hit)
  (setq hit nil)
  (if (= VDB-AutoMatchMode "exact")
    (setq hit (member (strcase (VDB-Trim code)) VDB-AutoSelectList))
    (progn
      (setq norm (VDB-NormWords code))
      (foreach w VDB-AutoSelectList
        (if (and (not hit) (vl-string-search (strcat " " w " ") norm))
          (setq hit T)
        )
      )
    )
  )
  hit
)

;; text placed in the drawing (APANDENT -> ABANDONED BUILDING)
(defun VDB-LabelText (code / out pos start)
  (setq out code)
  (foreach pr VDB-TextReplace
    (setq start 0)
    (while (setq pos (vl-string-search (car pr) (strcase out) start))
      (setq out (strcat (substr out 1 pos)
                        (cdr pr)
                        (substr out (+ pos (strlen (car pr)) 1))))
      (setq start (+ pos (strlen (cdr pr))))
    )
  )
  out
)

;; ---------------------------------------------------------------------------
;; MTEXT label: Code text at box centre, rotated to the tangent of the nearest
;; polyline on layer TRACK, middle-centre justified, width = text length.
;; ---------------------------------------------------------------------------
(defun VDB-MTextSafe (s / i ch out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (member ch '("\\" "{" "}")) (setq out (strcat out "\\")))
    (setq out (strcat out ch) i (1+ i))
  )
  out
)

;; keep text readable (never upside down)
(defun VDB-ReadableAngle (a)
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  (while (>= a (* 2.0 pi)) (setq a (- a (* 2.0 pi))))
  (if (and (> a (+ (/ pi 2.0) 1e-9)) (<= a (+ (* 1.5 pi) 1e-9)))
    (setq a (- a pi))
  )
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  a
)

;; tangent direction (radians) of nearest TRACK curve at pt, or nil
(defun VDB-TrackAngle (pt / i en best bd cp d par dv ang)
  (setq best nil bd 1e99 i 0)
  (if VDB-TrackSS
    (while (< i (sslength VDB-TrackSS))
      (setq en (ssname VDB-TrackSS i))
      (setq cp (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list en pt)))
      (if (and cp (not (vl-catch-all-error-p cp)))
        (progn
          (setq d (distance (list (car pt) (cadr pt) 0.0)
                            (list (car cp) (cadr cp) 0.0)))
          (if (< d bd) (setq bd d best (list en cp)))
        )
      )
      (setq i (1+ i))
    )
  )
  (if best
    (progn
      (setq par (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list (car best) (cadr best))))
      (if (and par (not (vl-catch-all-error-p par)))
        (progn
          (setq dv (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list (car best) par)))
          (if (and dv (not (vl-catch-all-error-p dv))
                   (or (> (abs (car dv)) 1e-9) (> (abs (cadr dv)) 1e-9)))
            (setq ang (atan (cadr dv) (car dv)))
          )
        )
      )
    )
  )
  ang
)

(defun VDB-MakeLabel (corners code layer fallbackAng / cx cy cz ang e ed tb w)
  (setq cx (/ (+ (car (nth 0 corners)) (car (nth 1 corners))
                 (car (nth 2 corners)) (car (nth 3 corners))) 4.0)
        cy (/ (+ (cadr (nth 0 corners)) (cadr (nth 1 corners))
                 (cadr (nth 2 corners)) (cadr (nth 3 corners))) 4.0)
        cz (caddr (car corners)))
  (setq ang (VDB-TrackAngle (list cx cy cz)))
  (if (null ang) (setq ang fallbackAng))
  (setq ang (VDB-ReadableAngle ang))
  (if VDB-TextFlip180 (setq ang (+ ang pi)))
  (while (>= ang (* 2.0 pi)) (setq ang (- ang (* 2.0 pi))))
  (setq e
    (entmakex
      (list
        '(0 . "MTEXT")
        '(100 . "AcDbEntity")
        (cons 8 layer)
        '(100 . "AcDbMText")
        (cons 10 (list cx cy cz))
        (cons 40 VDB-TextHeight)
        (cons 41 0.0)                       ; no wrap, refined below
        '(71 . 5)                           ; attachment = middle centre
        '(72 . 1)
        (cons 1 (VDB-MTextSafe (VDB-LabelText code)))
        (cons 7 (getvar "TEXTSTYLE"))
        (cons 50 ang)
      )
    )
  )
  ;; defined width = actual text length
  (if e
    (progn
      (setq ed (entget e))
      (setq tb (vl-catch-all-apply 'textbox (list ed)))
      (if (and tb (not (vl-catch-all-error-p tb)))
        (progn
          (setq w (- (car (cadr tb)) (car (car tb))))
          (if (> w 1e-6)
            (entmod (subst (cons 41 w) (assoc 41 ed) ed))
          )
        )
      )
    )
  )
  e
)

;; Split sorted list into sets. Returns (list sets leftoverCount)
(defun VDB-Split (lst / n step sets i k chunk)
  (setq n (length lst))
  (setq step (cond ((= (rem n 4) 0) 4)
                   ((= (rem n 3) 0) 3)
                   (T 4)))
  (setq sets '() i 0)
  (while (<= (+ i step) n)
    (setq chunk '() k 0)
    (while (< k step)
      (setq chunk (append chunk (list (nth (+ i k) lst))))
      (setq k (1+ k))
    )
    (setq sets (append sets (list chunk)))
    (setq i (+ i step))
  )
  ;; a leftover of exactly 3 is still a valid rectangle
  (if (= (- n i) 3)
    (progn
      (setq chunk '() k 0)
      (while (< k 3)
        (setq chunk (append chunk (list (nth (+ i k) lst))))
        (setq k (1+ k))
      )
      (setq sets (append sets (list chunk)))
      (setq i n)
    )
  )
  (list sets (- n i))
)

;; Build all rectangles for one code group.  Returns (made failed leftover)
(defun VDB-BuildGroup (code recs layer / sorted res sets left made failed idx corners)
  (setq sorted (vl-sort recs 'VDB-TimeLess))
  (setq made 0 failed 0 left 0)
  (if (< (length sorted) 3)
    (progn
      (princ (strcat "\n  " code ": only " (itoa (length sorted))
                     " point(s) - need 3 or 4, skipped."))
      (setq left (length sorted))
    )
    (progn
      (setq res  (VDB-Split sorted)
            sets (car res)
            left (cadr res)
            idx  0)
      (foreach s sets
        (setq idx (1+ idx))
        (setq corners (VDB-RectCorners s))
        (if (and corners (VDB-MakeRect corners layer))
          (progn
            (setq made (1+ made))
            (VDB-MakeLabel corners code layer
              (atan (- (cadr (nth 1 corners)) (cadr (nth 0 corners)))
                    (- (car  (nth 1 corners)) (car  (nth 0 corners)))))
          )
          (progn
            (setq failed (1+ failed))
            (princ (strcat "\n  " code " set " (itoa idx)
                           ": degenerate points, rectangle not created."))
          )
        )
      )
      (if (> left 0)
        (princ (strcat "\n  " code ": " (itoa left)
                       " extra point(s) ignored."))
      )
      (princ (strcat "\n  " code " -> " (itoa made) " rectangle(s) from "
                     (itoa (length sorted)) " pts"))
    )
  )
  (list made failed left)
)

;; ---------------------------------------------------------------------------
;; Error handler
;; ---------------------------------------------------------------------------
(defun VDB-Error (msg)
  (if vdb-*fh*
    (progn (vl-catch-all-apply 'close (list vdb-*fh*)) (setq vdb-*fh* nil))
  )
  (if vdb-*old-clayer*  (setvar "CLAYER"  vdb-*old-clayer*))
  (if vdb-*old-cmdecho* (setvar "CMDECHO" vdb-*old-cmdecho*))
  (if vdb-*old-osmode*  (setvar "OSMODE"  vdb-*old-osmode*))
  (setq *error* vdb-*old-error*)
  (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
    (princ (strcat "\nVIDDGPSTOBOX ERROR: " msg))
    (princ "\nVIDDGPSTOBOX cancelled.")
  )
  (princ)
)

;; ===========================================================================
;; DIALOG 1 - Select Layers (code groups).  Returns list of codes, or nil if
;; cancelled.  If nothing is highlighted, ALL codes are returned.
;; labels = list of display strings (same order as codes)
;; ===========================================================================
(defun VDB-SelectDialog (codes labels / dlgName f dcl_id dlgRet result tkns idx autoIdx i)
  (setq dlgName (vl-filename-mktemp "vdbSel" (getvar "TEMPPREFIX") ".dcl"))
  (setq f (open dlgName "w"))
  (write-line
    (strcat
      "vdb_sel : dialog {\n"
      "  label = \"Dgps To Box - Select Layers\";\n"
      "  : list_box {\n"
      "    key = \"lst_codes\"; height = 20; width = 52;\n"
      "    fixed_width_font = false; multiple_select = true; allow_accept = false;\n"
      "  }\n"
      "  spacer_1;\n"
      "  : row {\n"
      "    alignment = centered;\n"
      "    : button { key = \"btn_select\"; label = \"Select Layers\"; width = 16; fixed_width = true; is_default = true; }\n"
      "    : button { key = \"cancel\"; label = \"Cancel\"; width = 10; fixed_width = true; is_cancel = true; }\n"
      "  }\n"
      "}\n")
    f)
  (close f)

  (setq dcl_id (load_dialog dlgName))
  (setq result nil)
  (if (and (> dcl_id 0) (new_dialog "vdb_sel" dcl_id))
    (progn
      (start_list "lst_codes" 3)
      (foreach l labels (add_list l))
      (end_list)
      ;; auto-select building / room style groups
      (setq autoIdx "" i 0)
      (foreach c codes
        (if (VDB-AutoMatchP c)
          (setq autoIdx (strcat autoIdx (if (= autoIdx "") "" " ") (itoa i)))
        )
        (setq i (1+ i))
      )
      (if (/= autoIdx "") (set_tile "lst_codes" autoIdx))
      (setq VDB-selIdx "")
      (action_tile "btn_select"
        "(setq VDB-selIdx (get_tile \"lst_codes\")) (done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq dlgRet (start_dialog))
      (if (= dlgRet 1)
        (progn
          (setq result '())
          (if (/= (VDB-Trim VDB-selIdx) "")
            (progn
              (setq tkns (read (strcat "(" VDB-selIdx ")")))
              (foreach idx tkns
                (if (nth idx codes) (setq result (append result (list (nth idx codes)))))
              )
            )
            (setq result codes)
          )
        )
      )
    )
    (princ "\nVIDDGPSTOBOX: could not open selection dialog.")
  )
  (if (> dcl_id 0) (unload_dialog dcl_id))
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  result
)

;; ===========================================================================
;; DIALOG 2 - Layer Options.  Returns layer name or nil if cancelled.
;; ===========================================================================
(defun VDB-LayerDialog (selCodes / dlgName f dcl_id dlgRet result codeLabel)
  (setq dlgName (vl-filename-mktemp "vdbOpt" (getvar "TEMPPREFIX") ".dcl"))
  (setq codeLabel "")
  (foreach c selCodes
    (setq codeLabel (strcat codeLabel (if (= codeLabel "") "" ", ") c))
  )
  (if (> (strlen codeLabel) 40)
    (setq codeLabel (strcat (substr codeLabel 1 37) "..."))
  )
  (setq f (open dlgName "w"))
  (write-line
    (strcat
      "vdb_opt : dialog {\n"
      "  label = \"Dgps To Box - Layer Options\";\n"
      "  : row {\n"
      "    : radio_button { key = \"rb_default\"; label = \"Default layers\"; value = \"1\"; }\n"
      "    : radio_button { key = \"rb_csv\"; label = \"From CSV\"; value = \"0\"; }\n"
      "  }\n"
      "  : list_box {\n"
      "    key = \"lst_default\"; height = 16; width = 48;\n"
      "    fixed_width_font = false; multiple_select = false; allow_accept = false;\n"
      "  }\n"
      "  : radio_button { key = \"rb_custom\"; label = \"Custom layer for (" (VDB-Safe codeLabel) ")\"; value = \"0\"; }\n"
      "  : edit_box { key = \"edt_custom\"; label = \"Custom layer name\"; width = 40; edit_width = 36; is_enabled = false; }\n"
      "  spacer_1;\n"
      "  : row {\n"
      "    alignment = centered;\n"
      "    : button { key = \"accept\"; label = \"OK\"; width = 10; fixed_width = true; is_default = true; }\n"
      "    : button { key = \"cancel\"; label = \"Cancel\"; width = 10; fixed_width = true; is_cancel = true; }\n"
      "  }\n"
      "}\n")
    f)
  (close f)

  (setq dcl_id (load_dialog dlgName))
  (setq result nil)
  (if (and (> dcl_id 0) (new_dialog "vdb_opt" dcl_id))
    (progn
      (setq VDB-mode "default" VDB-defIdx "0" VDB-custom "")
      (setq VDB-FromCsv nil)
      (set_tile "rb_default" "1")
      (set_tile "rb_csv" "0")
      (set_tile "rb_custom" "0")
      (start_list "lst_default" 3)
      (foreach l VDB-DefaultLayers (add_list l))
      (end_list)
      (set_tile "lst_default" "0")

      (action_tile "rb_default"
        (strcat "(setq VDB-mode \"default\")"
                "(set_tile \"rb_default\" \"1\") (set_tile \"rb_csv\" \"0\") (set_tile \"rb_custom\" \"0\")"
                "(mode_tile \"lst_default\" 0) (mode_tile \"edt_custom\" 1)"))
      (action_tile "rb_csv"
        (strcat "(setq VDB-mode \"csv\")"
                "(set_tile \"rb_default\" \"0\") (set_tile \"rb_csv\" \"1\") (set_tile \"rb_custom\" \"0\")"
                "(mode_tile \"lst_default\" 1) (mode_tile \"edt_custom\" 1)"))
      (action_tile "rb_custom"
        (strcat "(setq VDB-mode \"custom\")"
                "(set_tile \"rb_default\" \"0\") (set_tile \"rb_csv\" \"0\") (set_tile \"rb_custom\" \"1\")"
                "(mode_tile \"lst_default\" 1) (mode_tile \"edt_custom\" 0)"
                "(mode_tile \"edt_custom\" 2)"))
      (action_tile "accept"
        (strcat "(setq VDB-custom (get_tile \"edt_custom\"))"
                "(setq VDB-defIdx (get_tile \"lst_default\"))"
                "(done_dialog 1)"))
      (action_tile "cancel" "(done_dialog 0)")

      (setq dlgRet (start_dialog))
      (if (= dlgRet 1)
        (cond
          ((= VDB-mode "csv")
           (setq VDB-FromCsv T)
           (setq result (strcat VDB-CsvPrefix "<Code>")))
          ((= VDB-mode "custom")
           (setq VDB-custom (VDB-Trim VDB-custom))
           (if (= VDB-custom "")
             (progn
               (alert "Custom layer name is blank - using the first default layer.")
               (setq result (nth 0 VDB-DefaultLayers)))
             (setq result VDB-custom)))
          (T
           (setq result
             (if (and (/= (VDB-Trim VDB-defIdx) "") (nth (atoi VDB-defIdx) VDB-DefaultLayers))
               (nth (atoi VDB-defIdx) VDB-DefaultLayers)
               (nth 0 VDB-DefaultLayers))))
        )
      )
    )
    (princ "\nVIDDGPSTOBOX: could not open layer dialog.")
  )
  (if (> dcl_id 0) (unload_dialog dcl_id))
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  result
)

;; remove characters that would break a DCL string
(defun VDB-Safe (s / i ch out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (or (= ch "\"") (= ch "\\")) (setq ch "'"))
    (setq out (strcat out ch) i (1+ i))
  )
  out
)

;; ===========================================================================
;; MAIN COMMAND
;; ===========================================================================
(defun c:VIDDGPSTOBOX
  (/ csvFile fh headerLine headers idxC idxN idxE idxZ idxT
     line fields rowNo code north east elev tm allRev records
     groups codes labels selCodes targetLayer codeLayer
     g res totMade totFail totSkip dataRows validRows skippedRows)

  (setq vdb-*old-error* *error*)
  (setq *error* VDB-Error)
  (setq vdb-*old-clayer*  (getvar "CLAYER"))
  (setq vdb-*old-cmdecho* (getvar "CMDECHO"))
  (setq vdb-*old-osmode*  (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)

  ;; 1. CSV file ---------------------------------------------------------------
  (setq csvFile
    (if (boundp 'LogicDWG:RequireCsv)
      (LogicDWG:RequireCsv)
      (getfiled "Select DGPS CSV File" "" "csv" 4)))
  (if (null csvFile)
    (progn (VDB-Error "No CSV file selected.") (exit))
  )

  ;; 2. Read CSV --------------------------------------------------------------
  (setq fh (open csvFile "rb"))
  (if (null fh)
    (progn (VDB-Error "Could not open CSV file.") (exit))
  )
  (setq vdb-*fh* fh)

  (setq headerLine (VDB-ReadLine fh))
  (if (null headerLine)
    (progn (VDB-Error "CSV file is empty.") (exit))
  )
  (setq headers (VDB-ParseCSV (VDB-StripBOM headerLine)))
  (setq idxC (VDB-HeaderIndex headers "Code")
        idxN (VDB-HeaderIndex headers "Northing")
        idxE (VDB-HeaderIndex headers "Easting")
        idxZ (VDB-HeaderIndex headers "Elevation")
        idxT (VDB-HeaderIndex headers "Local Time"))
  (if (or (null idxC) (null idxN) (null idxE) (null idxZ))
    (progn
      (VDB-Error "Required headers missing. Need: Code, Northing, Easting, Elevation.")
      (exit))
  )

  (setq rowNo 1 dataRows 0 validRows 0 skippedRows 0 allRev '())
  (while (setq line (VDB-ReadLine fh))
    (setq rowNo (1+ rowNo))
    (if (/= (VDB-Trim line) "")
      (progn
        (setq dataRows (1+ dataRows))
        (setq fields (VDB-ParseCSV line))
        (setq code  (VDB-GetField fields idxC)
              north (VDB-Num (VDB-GetField fields idxN))
              east  (VDB-Num (VDB-GetField fields idxE))
              elev  (VDB-Num (VDB-GetField fields idxZ))
              tm    (if idxT (VDB-GetField fields idxT) ""))
        (if (and (/= code "") north east elev)
          (progn
            (setq allRev (cons (list code east north elev tm rowNo) allRev))
            (setq validRows (1+ validRows)))
          (setq skippedRows (1+ skippedRows))
        )
      )
    )
  )
  (close fh)
  (setq vdb-*fh* nil)
  (setq records (reverse allRev))
  (if (= validRows 0)
    (progn (VDB-Error "No valid records (with a Code) found in CSV.") (exit))
  )

  (setq groups (VDB-GroupByCode records))
  (setq codes '() labels '())
  (foreach g groups
    (setq codes  (append codes  (list (car g))))
    (setq labels (append labels
                  (list (strcat (car g) "   (" (itoa (length (cdr g))) " pts)"))))
  )

  ;; 3. Dialog 1 : Select Layers ----------------------------------------------
  (setq selCodes (VDB-SelectDialog codes labels))
  (if (or (null selCodes) (= (length selCodes) 0))
    (progn (VDB-Error "Cancelled by user.") (exit))
  )

  ;; 4. Dialog 2 : Layer Options ----------------------------------------------
  (setq targetLayer (VDB-LayerDialog selCodes))
  (if (null targetLayer)
    (progn (VDB-Error "Cancelled by user.") (exit))
  )
  (if (not VDB-FromCsv)
    (progn
      (setq targetLayer (VDB-SanitizeLayerName targetLayer))
      (if (null (VDB-EnsureLayer targetLayer 3))
        (progn (VDB-Error (strcat "Could not create layer " targetLayer)) (exit))
      )
    )
  )

  ;; 5. Build rectangles ------------------------------------------------------
  (princ "\n========================================")
  (princ "\nVIDDGPSTOBOX")
  (princ "\n========================================")
  (princ (strcat "\nFile:         " csvFile))
  (princ (strcat "\nTarget layer: "
                 (if VDB-FromCsv (strcat VDB-CsvPrefix "<Code> (from CSV)") targetLayer)))
  (princ (strcat "\nRows: " (itoa dataRows) "  valid: " (itoa validRows)
                 "  skipped (no code / bad number): " (itoa skippedRows)))
  (if (null idxT)
    (princ "\nWARNING: no 'Local Time' column - CSV row order is used.")
  )
  (princ "\n----------------------------------------")

  (setq VDB-TrackSS
    (ssget "_X" (list (cons 8 VDB-TrackLayer)
                      '(0 . "LWPOLYLINE,POLYLINE,LINE,ARC,SPLINE"))))
  (if VDB-TrackSS
    (princ (strcat "\nTrack curves on " VDB-TrackLayer ": " (itoa (sslength VDB-TrackSS))))
    (princ (strcat "\nWARNING: no curves on layer " VDB-TrackLayer
                   " - text follows the box direction."))
  )

  (setq totMade 0 totFail 0 totSkip 0)
  (foreach g groups
    (if (member (car g) selCodes)
      (progn
        (setq codeLayer
          (if VDB-FromCsv
            (VDB-SanitizeLayerName (strcat VDB-CsvPrefix (car g)))
            targetLayer))
        (if VDB-FromCsv (VDB-EnsureLayer codeLayer 3))
        (setq res (VDB-BuildGroup (car g) (cdr g) codeLayer))
        (setq totMade (+ totMade (nth 0 res))
              totFail (+ totFail (nth 1 res))
              totSkip (+ totSkip (nth 2 res)))
      )
    )
  )

  (princ "\n----------------------------------------")
  (setq VDB-TrackSS nil)
  (princ (strcat "\nRectangles created: " (itoa totMade) " (each with an MTEXT label)"))
  (princ (strcat "\nFailed (degenerate): " (itoa totFail)))
  (princ (strcat "\nPoints not used:     " (itoa totSkip)))
  (princ "\n========================================")

  (setvar "CLAYER"  vdb-*old-clayer*)
  (setvar "CMDECHO" vdb-*old-cmdecho*)
  (setvar "OSMODE"  vdb-*old-osmode*)
  (setq *error* vdb-*old-error*)
  (princ)
)

(princ "\nVIDDGPSTOBOX loaded. Type VIDDGPSTOBOX to run.")
(princ)
