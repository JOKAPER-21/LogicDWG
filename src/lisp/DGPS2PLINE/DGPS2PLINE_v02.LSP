;;;===========================================================================
;;; DGPS2PLINE_v02.LSP
;;;
;;; Command : DGPS2PLINE
;;; Purpose : Import DGPS survey CSV data into AutoCAD / Civil 3D.
;;;
;;; V02 FIXES (vs V01):
;;;   1. Survey-point creation is now a completely separate pass over the
;;;      FULL record list (allRecords), fully decoupled from the Code
;;;      grouping / polyline pass. Every valid CSV row produces exactly
;;;      one POINT + 3 MTEXT, regardless of how many Codes exist.
;;;   2. Code grouping is only used to decide/build gpl_<code> geometry
;;;      (2D or 3D polyline). It never gates survey-point creation.
;;;   3. Local Time is parsed into a numeric sort key (date+time down to
;;;      milliseconds where present) instead of relying on lexical string
;;;      comparison, with a safe fallback if parsing is inconclusive.
;;;   4. Debug reporting of every discovered Code and its record count is
;;;      printed, so a single-Code regression is immediately visible.
;;;
;;; Load    : APPLOAD -> DGPS2PLINE_v02.LSP
;;; Run     : DGPS2PLINE
;;;===========================================================================

(vl-load-com)

;;; ---------------------------------------------------------------------
;;; GENERIC STRING / LIST HELPERS
;;; ---------------------------------------------------------------------

(defun DGPS:ws-char-p (ch)
  (or (= ch " ") (= ch "\t") (= ch "\r") (= ch "\n"))
)

(defun DGPS:str-trim (s / len st en)
  (if (not s) (setq s ""))
  (setq len (strlen s) st 1 en len)
  (while (and (<= st en) (DGPS:ws-char-p (substr s st 1)))
    (setq st (1+ st))
  )
  (while (and (>= en st) (DGPS:ws-char-p (substr s en 1)))
    (setq en (1- en))
  )
  (if (> st en) "" (substr s st (1+ (- en st))))
)

(defun DGPS:str-ieq (a b)
  (= (strcase (DGPS:str-trim a)) (strcase (DGPS:str-trim b)))
)

(defun DGPS:member? (item lst)
  (if lst
      (if (equal item (car lst)) T (DGPS:member? item (cdr lst)))
      nil
  )
)

(defun DGPS:dedup (lst / result)
  (setq result '())
  (foreach x lst
    (if (not (DGPS:member? x result)) (setq result (append result (list x))))
  )
  result
)

;; Split one CSV line into a list of fields, honouring double-quoted
;; fields (including embedded commas and escaped "" quotes).
(defun DGPS:parse-csv-line (line / len i ch inq cur fields)
  (setq len (strlen line) i 1 inq nil cur "" fields '())
  (while (<= i len)
    (setq ch (substr line i 1))
    (cond
      ((and inq (= ch "\""))
       (if (and (< i len) (= (substr line (1+ i) 1) "\""))
           (progn (setq cur (strcat cur "\"")) (setq i (1+ i)))
           (setq inq nil)
       )
      )
      ((and (not inq) (= ch "\""))
       (setq inq T)
      )
      ((and (not inq) (= ch ","))
       (setq fields (append fields (list cur)))
       (setq cur "")
      )
      (T (setq cur (strcat cur ch)))
    )
    (setq i (1+ i))
  )
  (setq fields (append fields (list cur)))
  fields
)

(defun DGPS:get-nth-safe (lst idx)
  (if (and idx (>= idx 0) (< idx (length lst)))
      (nth idx lst)
      ""
  )
)

(defun DGPS:find-col-index (headers target / i idx)
  (setq i 0 idx nil)
  (foreach h headers
    (if (and (not idx) (DGPS:str-ieq h target)) (setq idx i))
    (setq i (1+ i))
  )
  idx
)

;; Loosely validated, locale-independent numeric-string test.
(defun DGPS:numeric-string-p (s / len i ch dotcount ecount digitcount valid)
  (setq s (DGPS:str-trim s) len (strlen s))
  (if (= len 0)
      nil
      (progn
        (setq i 1 dotcount 0 ecount 0 digitcount 0 valid T)
        (while (and valid (<= i len))
          (setq ch (substr s i 1))
          (cond
            ((and (>= (ascii ch) 48) (<= (ascii ch) 57))
             (setq digitcount (1+ digitcount))
            )
            ((= ch ".")
             (setq dotcount (1+ dotcount))
             (if (> dotcount 1) (setq valid nil))
            )
            ((or (= ch "e") (= ch "E"))
             (setq ecount (1+ ecount))
             (if (> ecount 1) (setq valid nil))
            )
            ((or (= ch "+") (= ch "-")) T)
            (T (setq valid nil))
          )
          (setq i (1+ i))
        )
        (and valid (> digitcount 0))
      )
  )
)

(defun DGPS:to-real (s)
  (if (DGPS:numeric-string-p s) (atof (DGPS:str-trim s)) nil)
)

;; Sanitize a Code value into a safe AutoCAD layer-name fragment.
;; The original Code text is never altered - only the layer-name copy is.
(defun DGPS:sanitize-name (s / len i ch out)
  (setq s (DGPS:str-trim s) len (strlen s) i 1 out "")
  (while (<= i len)
    (setq ch (substr s i 1))
    (if (or (and (>= (ascii ch) 65) (<= (ascii ch) 90))
            (and (>= (ascii ch) 97) (<= (ascii ch) 122))
            (and (>= (ascii ch) 48) (<= (ascii ch) 57))
            (= ch "_") (= ch "-") (= ch "$")
        )
        (setq out (strcat out ch))
        (setq out (strcat out "_"))
    )
    (setq i (1+ i))
  )
  (if (= out "") "_" out)
)

(defun DGPS:sublist (lst start end / i result)
  (setq result '() i start)
  (while (< i end)
    (setq result (cons (nth i lst) result))
    (setq i (1+ i))
  )
  (reverse result)
)

(defun DGPS:merge-lists (a b less-p / result)
  (setq result '())
  (while (and a b)
    (if (less-p (car b) (car a))
        (progn (setq result (cons (car b) result)) (setq b (cdr b)))
        (progn (setq result (cons (car a) result)) (setq a (cdr a)))
    )
  )
  (append (reverse result) a b)
)

;; Stable merge sort - preserves original relative order for equal keys.
(defun DGPS:stable-sort (lst less-p / n mid left right)
  (setq n (length lst))
  (cond
    ((<= n 1) lst)
    (T
     (setq mid (/ n 2))
     (setq left (DGPS:stable-sort (DGPS:sublist lst 0 mid) less-p))
     (setq right (DGPS:stable-sort (DGPS:sublist lst mid n) less-p))
     (DGPS:merge-lists left right less-p)
    )
  )
)

;; Extract every run of decimal digits from a string, as a list of ints,
;; in left-to-right order. Used to parse Local Time into sortable parts.
(defun DGPS:extract-numbers (s / len i ch cur nums)
  (setq nums '() cur "" len (strlen s) i 1)
  (while (<= i len)
    (setq ch (substr s i 1))
    (if (and (>= (ascii ch) 48) (<= (ascii ch) 57))
        (setq cur (strcat cur ch))
        (progn
          (if (/= cur "") (setq nums (cons (atoi cur) nums)))
          (setq cur "")
        )
    )
    (setq i (1+ i))
  )
  (if (/= cur "") (setq nums (cons (atoi cur) nums)))
  (reverse nums)
)

;; Convert a Local Time string into a single sortable real number.
;; Handles "YYYY-MM-DD HH:MM:SS.mmm", "YYYY-MM-DD HH:MM:SS", plain
;; "HH:MM:SS.mmm", "HH:MM:SS" and "HH:MM" layouts. Returns nil if the
;; string cannot be interpreted as a time at all (caller falls back to
;; raw string comparison in that case).
(defun DGPS:localtime-key (s / nums n y mo d h mi se ms)
  (setq nums (DGPS:extract-numbers s))
  (setq n (length nums))
  (cond
    ((>= n 7)
     (setq y (nth 0 nums) mo (nth 1 nums) d (nth 2 nums)
           h (nth 3 nums) mi (nth 4 nums) se (nth 5 nums) ms (nth 6 nums))
     (+ (* (float y) 31622400000.0) (* (float mo) 2678400000.0)
        (* (float d) 86400000.0) (* (float h) 3600000.0)
        (* (float mi) 60000.0) (* (float se) 1000.0) (float ms))
    )
    ((= n 6)
     (setq y (nth 0 nums) mo (nth 1 nums) d (nth 2 nums)
           h (nth 3 nums) mi (nth 4 nums) se (nth 5 nums))
     (+ (* (float y) 31622400000.0) (* (float mo) 2678400000.0)
        (* (float d) 86400000.0) (* (float h) 3600000.0)
        (* (float mi) 60000.0) (* (float se) 1000.0))
    )
    ((= n 4)
     (setq h (nth 0 nums) mi (nth 1 nums) se (nth 2 nums) ms (nth 3 nums))
     (+ (* (float h) 3600000.0) (* (float mi) 60000.0)
        (* (float se) 1000.0) (float ms))
    )
    ((= n 3)
     (setq h (nth 0 nums) mi (nth 1 nums) se (nth 2 nums))
     (+ (* (float h) 3600000.0) (* (float mi) 60000.0) (* (float se) 1000.0))
    )
    ((= n 2)
     (setq h (nth 0 nums) mi (nth 1 nums))
     (+ (* (float h) 3600000.0) (* (float mi) 60000.0))
    )
    ((= n 1) (float (nth 0 nums)))
    (T nil)
  )
)

;; Record layout (0-based nth):
;;  0 PointName  1 Code  2 LocalTime(raw)  3 Northing  4 Easting
;;  5 Elevation  6 RowIndex  7 LocalTimeKey(or nil)
(defun DGPS:rec-less (r1 r2 / k1 k2)
  (setq k1 (nth 7 r1) k2 (nth 7 r2))
  (cond
    ((and k1 k2)
     (if (= k1 k2) (< (nth 6 r1) (nth 6 r2)) (< k1 k2))
    )
    (T
     (if (= (nth 2 r1) (nth 2 r2))
         (< (nth 6 r1) (nth 6 r2))
         (< (nth 2 r1) (nth 2 r2))
     )
    )
  )
)

;; Group records by Code, preserving first-appearance order of codes
;; and original relative order of records within each code. Every
;; record belongs to exactly one group; nothing is discarded/overwritten.
(defun DGPS:build-groups (records / groups codeval entry)
  (setq groups '())
  (foreach rec records
    (setq codeval (nth 1 rec))
    (setq entry (assoc codeval groups))
    (if entry
        (setq groups (subst (list codeval (cons rec (cadr entry))) entry groups))
        (setq groups (append groups (list (list codeval (list rec)))))
    )
  )
  (mapcar '(lambda (g) (list (car g) (reverse (cadr g)))) groups)
)

;; Best-effort, fully guarded attempt to read a coordinate-system name
;; stored in the drawing's ACAD_GEOGRAPHICDATA dictionary object.
;; Never errors; returns nil if nothing usable is found.
(defun DGPS:get-drawing-coordsys ( / nod geoent csname res)
  (setq csname nil)
  (setq res (vl-catch-all-apply 'namedobjdict '()))
  (if (not (vl-catch-all-error-p res))
      (progn
        (setq nod res)
        (setq res (vl-catch-all-apply 'dictsearch (list nod "ACAD_GEOGRAPHICDATA")))
        (if (and (not (vl-catch-all-error-p res)) res)
            (progn
              (setq geoent res)
              (setq csname (cdr (assoc 301 geoent)))
              (if (not csname) (setq csname (cdr (assoc 302 geoent))))
            )
        )
      )
  )
  (if (and csname (/= (DGPS:str-trim csname) "")) csname nil)
)

;; Reads an entire text file into a list of strings (one per line).
(defun DGPS:read-all-lines (path / f lines ln)
  (setq lines '())
  (setq f (open path "r"))
  (if (not f) (error (strcat "Cannot open CSV file: " path)))
  (while (setq ln (read-line f))
    (setq lines (cons ln lines))
  )
  (close f)
  (reverse lines)
)

;;; ---------------------------------------------------------------------
;;; LAYER / ENTITY CREATION HELPERS
;;; ---------------------------------------------------------------------

(defun DGPS:ensure-layer (name)
  (if (not (tblsearch "LAYER" name))
      (entmake
        (list
          (cons 0 "LAYER")
          (cons 100 "AcDbSymbolTableRecord")
          (cons 100 "AcDbLayerTableRecord")
          (cons 2 name)
          (cons 70 0)
          (cons 62 7)
          (cons 6 "Continuous")
        )
      )
  )
  name
)

(defun DGPS:make-mtext (layer inspt txt attach)
  (entmake
    (list
      (cons 0 "MTEXT")
      (cons 100 "AcDbEntity")
      (cons 8 layer)
      (cons 100 "AcDbMText")
      (cons 10 inspt)
      (cons 40 0.5)
      (cons 41 0.0)
      (cons 71 attach)
      (cons 72 1)
      (cons 1 txt)
      (cons 50 0.0)
    )
  )
)

(defun DGPS:make-lwpolyline (layer ptlist / edata)
  (setq edata
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 layer)
      (cons 100 "AcDbPolyline")
      (cons 90 (length ptlist))
      (cons 70 0)
    )
  )
  (foreach pt ptlist
    (setq edata (append edata (list (cons 10 (list (car pt) (cadr pt))))))
  )
  (entmake edata)
)

(defun DGPS:make-3dpolyline (layer ptlist)
  (entmake
    (list
      (cons 0 "POLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 layer)
      (cons 100 "AcDb3dPolyline")
      (cons 66 1)
      (cons 70 8)
      (cons 10 (list 0.0 0.0 0.0))
    )
  )
  (foreach pt ptlist
    (entmake
      (list
        (cons 0 "VERTEX")
        (cons 100 "AcDbEntity")
        (cons 8 layer)
        (cons 100 "AcDbVertex")
        (cons 100 "AcDb3dPolylineVertex")
        (cons 10 pt)
        (cons 70 32)
      )
    )
  )
  (entmake (list (cons 0 "SEQEND") (cons 8 layer)))
)

;; Creates POINT + 3 MTEXT (Point Name / Code / Elevation) for one record.
;; This is called directly from the FULL record list - never from a group
;; summary - so exactly one call happens per valid CSV row.
(defun DGPS:create-survey-point (rec layer / ptname codeval nval eval2 zval basept ins)
  (setq ptname (nth 0 rec))
  (setq codeval (nth 1 rec))
  (setq nval (nth 3 rec))
  (setq eval2 (nth 4 rec))
  (setq zval (nth 5 rec))
  (setq basept (list eval2 nval zval))

  (entmake (list (cons 0 "POINT") (cons 8 layer) (cons 10 basept)))

  ;; Point Name - Middle Right, offset above the point
  (setq ins (list (car basept) (+ (cadr basept) 2.0) (caddr basept)))
  (DGPS:make-mtext layer ins ptname 6)

  ;; Code - Top Left, offset lower-left of the point
  (setq ins (list (- (car basept) 1.5) (- (cadr basept) 1.0) (caddr basept)))
  (DGPS:make-mtext layer ins codeval 1)

  ;; Elevation - Bottom Left, offset further lower-left of the point
  (setq ins (list (- (car basept) 1.5) (- (cadr basept) 2.0) (caddr basept)))
  (DGPS:make-mtext layer ins (rtos zval 2 3) 7)
)

;;; ---------------------------------------------------------------------
;;; MAIN COMMAND
;;; ---------------------------------------------------------------------

(defun c:DGPS2PLINE
       (/ *error* old-cmdecho old-osmode old-clayer old-pdmode old-pdsize
        typekw is3d csvfile alllines headerline datalines
        headers idx-ptname idx-code idx-localtime idx-northing idx-easting
        idx-elevation records rowindex skipcount skipreasons fields
        ptname codeval ltime nstr estr zstr nval eval2 zval ltimekey
        groups csname totalcsv validcnt codescnt
        totalpts singlecodes layersreport splayers gplayers
        g gcode recs splayer gplayer pts totalpoly rec
       )

  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osmode (getvar "OSMODE"))
  (setq old-clayer (getvar "CLAYER"))
  (setq old-pdmode (getvar "PDMODE"))
  (setq old-pdsize (getvar "PDSIZE"))

  (defun *error* (msg)
    (cond
      ((or (not msg)
           (= (strcase msg) "CANCELLED")
           (wcmatch (strcase msg) "*CANCEL*")
           (wcmatch (strcase msg) "*QUIT*")
           (wcmatch (strcase msg) "*ESC*")
      )
       (princ "\nCommand cancelled.")
      )
      (T (princ (strcat "\nDGPS2PLINE ERROR: " msg)))
    )
    (setvar "CMDECHO" old-cmdecho)
    (setvar "OSMODE" old-osmode)
    (setvar "CLAYER" old-clayer)
    (setvar "PDMODE" old-pdmode)
    (setvar "PDSIZE" old-pdsize)
    (princ)
  )

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  ;; -------------------------------------------------------------
  ;; STEP 1 : ask output geometry type (no entity selection at all)
  ;; -------------------------------------------------------------
  (initget "Polyline 3Dpolyline")
  (setq typekw (getkword "\nSelect output type [Polyline/3Dpolyline] <Polyline>: "))
  (if (or (not typekw) (= typekw "")) (setq typekw "Polyline"))
  (setq is3d (= typekw "3Dpolyline"))
  (princ (if is3d "\nOutput type: 3D Polyline." "\nOutput type: Polyline (2D)."))

  ;; -------------------------------------------------------------
  ;; DRAWING COORDINATE SYSTEM (informational, best-effort)
  ;; -------------------------------------------------------------
  (setq csname (DGPS:get-drawing-coordsys))
  (if csname
      (princ (strcat "\nDrawing coordinate system: " csname))
      (princ
        "\nWARNING: No coordinate system assigned to this drawing. Northing/Easting will be used as-is from the CSV."
      )
  )

  ;; -------------------------------------------------------------
  ;; STEP 2 : select CSV file
  ;; -------------------------------------------------------------
  (setq csvfile (getfiled "Select DGPS CSV File" "" "csv" 8))
  (if (not csvfile) (error "CANCELLED"))

  ;; -------------------------------------------------------------
  ;; STEP 3 : read entire file / header
  ;; -------------------------------------------------------------
  (setq alllines (DGPS:read-all-lines csvfile))
  (if (not alllines) (error (strcat "CSV file is empty: " csvfile)))
  (setq headerline (car alllines))
  (setq datalines (cdr alllines))
  (setq datalines (vl-remove-if '(lambda (l) (= (DGPS:str-trim l) "")) datalines))
  (if (not datalines) (error "CSV file has no data rows."))

  (setq headers (mapcar 'DGPS:str-trim (DGPS:parse-csv-line headerline)))

  ;; -------------------------------------------------------------
  ;; STEP 4 : locate required columns by header name
  ;; -------------------------------------------------------------
  (setq idx-ptname (DGPS:find-col-index headers "Point Name"))
  (setq idx-code (DGPS:find-col-index headers "Code"))
  (setq idx-localtime (DGPS:find-col-index headers "Local Time"))
  (setq idx-northing (DGPS:find-col-index headers "Northing"))
  (setq idx-easting (DGPS:find-col-index headers "Easting"))
  (setq idx-elevation (DGPS:find-col-index headers "Elevation"))

  (if (not idx-ptname) (error "Missing required CSV column: Point Name"))
  (if (not idx-code) (error "Missing required CSV column: Code"))
  (if (not idx-localtime) (error "Missing required CSV column: Local Time"))
  (if (not idx-northing) (error "Missing required CSV column: Northing"))
  (if (not idx-easting) (error "Missing required CSV column: Easting"))
  (if (not idx-elevation) (error "Missing required CSV column: Elevation"))

  ;; -------------------------------------------------------------
  ;; STEP 5 : parse + validate EVERY data row into allRecords.
  ;; No row is ever skipped because of a Code change, and no
  ;; earlier row is ever overwritten - this is a single flat pass.
  ;; -------------------------------------------------------------
  (setq records '() rowindex 0 skipcount 0 skipreasons '())
  (foreach line datalines
    (setq rowindex (1+ rowindex))
    (setq fields (DGPS:parse-csv-line line))
    (setq ptname (DGPS:str-trim (DGPS:get-nth-safe fields idx-ptname)))
    (setq codeval (DGPS:str-trim (DGPS:get-nth-safe fields idx-code)))
    (setq ltime (DGPS:str-trim (DGPS:get-nth-safe fields idx-localtime)))
    (setq nstr (DGPS:get-nth-safe fields idx-northing))
    (setq estr (DGPS:get-nth-safe fields idx-easting))
    (setq zstr (DGPS:get-nth-safe fields idx-elevation))
    (setq nval (DGPS:to-real nstr))
    (setq eval2 (DGPS:to-real estr))
    (setq zval (DGPS:to-real zstr))
    (cond
      ((= codeval "")
       (setq skipcount (1+ skipcount))
       (setq skipreasons (cons (strcat "Row " (itoa rowindex) ": empty Code") skipreasons))
      )
      ((= ptname "")
       (setq skipcount (1+ skipcount))
       (setq skipreasons (cons (strcat "Row " (itoa rowindex) ": empty Point Name") skipreasons))
      )
      ((not nval)
       (setq skipcount (1+ skipcount))
       (setq skipreasons (cons (strcat "Row " (itoa rowindex) ": invalid Northing") skipreasons))
      )
      ((not eval2)
       (setq skipcount (1+ skipcount))
       (setq skipreasons (cons (strcat "Row " (itoa rowindex) ": invalid Easting") skipreasons))
      )
      ((not zval)
       (setq skipcount (1+ skipcount))
       (setq skipreasons (cons (strcat "Row " (itoa rowindex) ": invalid Elevation") skipreasons))
      )
      (T
       (setq ltimekey (DGPS:localtime-key ltime))
       (setq records
         (cons (list ptname codeval ltime nval eval2 zval rowindex ltimekey) records)
       )
      )
    )
  )
  (setq records (reverse records))
  (setq skipreasons (reverse skipreasons))
  (setq totalcsv (length datalines))
  (setq validcnt (length records))

  (princ (strcat "\n\nCSV rows read: " (itoa totalcsv)))
  (if (= validcnt 0) (error "No valid records found in CSV."))

  ;; -------------------------------------------------------------
  ;; STEP 6 : create every survey point directly from allRecords.
  ;; This pass is completely independent from Code grouping below -
  ;; it is a single foreach over the full flat record list, so every
  ;; valid CSV row always gets its POINT + 3 MTEXT.
  ;; -------------------------------------------------------------
  (setvar "PDMODE" 0)
  (setvar "PDSIZE" 0.5)

  (setq totalpts 0 splayers '())
  (foreach rec records
    (setq splayer (DGPS:ensure-layer (strcat "sp_" (DGPS:sanitize-name (nth 1 rec)))))
    (setq splayers (cons splayer splayers))
    (DGPS:create-survey-point rec splayer)
    (setq totalpts (1+ totalpts))
  )
  (setq splayers (DGPS:dedup (reverse splayers)))

  ;; -------------------------------------------------------------
  ;; STEP 7 : group allRecords by Code (grouping is ONLY used for
  ;; geometry decisions below - it never re-creates survey points).
  ;; -------------------------------------------------------------
  (setq groups (DGPS:build-groups records))
  (setq groups
    (mapcar
      '(lambda (g) (list (car g) (DGPS:stable-sort (cadr g) 'DGPS:rec-less)))
      groups
    )
  )
  (setq codescnt (length groups))

  (princ "\n\nCodes discovered:")
  (foreach g groups
    (princ (strcat "\n  " (car g) "  ->  " (itoa (length (cadr g))) " record(s)"))
  )

  ;; -------------------------------------------------------------
  ;; STEP 8 : for each Code group with >= 2 records, build gpl_<code>
  ;; geometry (2D or 3D per the chosen output type).
  ;; -------------------------------------------------------------
  (setq totalpoly 0 singlecodes '() gplayers '())
  (foreach g groups
    (setq gcode (car g))
    (setq recs (cadr g))
    (if (>= (length recs) 2)
        (progn
          (setq gplayer (strcat "gpl_" (DGPS:sanitize-name gcode)))
          (DGPS:ensure-layer gplayer)
          (setq gplayers (cons gplayer gplayers))
          (if is3d
              (progn
                (setq pts (mapcar '(lambda (r) (list (nth 4 r) (nth 3 r) (nth 5 r))) recs))
                (DGPS:make-3dpolyline gplayer pts)
              )
              (progn
                (setq pts (mapcar '(lambda (r) (list (nth 4 r) (nth 3 r))) recs))
                (DGPS:make-lwpolyline gplayer pts)
              )
          )
          (setq totalpoly (1+ totalpoly))
        )
        (setq singlecodes (cons gcode singlecodes))
    )
  )
  (setq singlecodes (reverse singlecodes))
  (setq gplayers (DGPS:dedup (reverse gplayers)))
  (setq layersreport (append splayers gplayers))

  ;; -------------------------------------------------------------
  ;; STEP 9 : completion report - all values computed dynamically.
  ;; -------------------------------------------------------------
  (princ "\n========================================")
  (princ "\nDGPS2PLINE COMPLETE")
  (princ "\n========================================")
  (princ (strcat "\n\nCSV:\n" csvfile))
  (princ (strcat "\n\nOutput type:\n" (if is3d "3D Polyline" "Polyline")))
  (princ (strcat "\n\nTotal CSV rows:\n" (itoa totalcsv)))
  (princ (strcat "\n\nValid records:\n" (itoa validcnt)))
  (princ (strcat "\n\nSkipped records:\n" (itoa skipcount)))
  (if (> skipcount 0) (foreach r skipreasons (princ (strcat "\n  - " r))))
  (princ (strcat "\n\nUnique Codes:\n" (itoa codescnt)))
  (princ (strcat "\n\nSurvey POINT entities:\n" (itoa totalpts)))
  (princ (strcat "\n\nPoint Name MTEXT:\n" (itoa totalpts)))
  (princ (strcat "\n\nCode MTEXT:\n" (itoa totalpts)))
  (princ (strcat "\n\nElevation MTEXT:\n" (itoa totalpts)))
  (princ (strcat "\n\nSurvey point layers:\n" (itoa (length splayers))))
  (princ (strcat "\n\nPolyline geometries:\n" (itoa totalpoly)))
  (princ (strcat "\n\nSingle-point Codes:\n" (itoa (length singlecodes))))
  (if singlecodes
      (foreach c singlecodes (princ (strcat "\n  " c ": 1 survey point - geometry skipped.")))
  )
  (princ "\n\nLayers created/reused:")
  (foreach l layersreport (princ (strcat "\n  " l)))
  (princ "\n========================================")

  ;; -------------------------------------------------------------
  ;; CLEANUP
  ;; -------------------------------------------------------------
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  (setvar "CLAYER" old-clayer)
  (setvar "PDMODE" old-pdmode)
  (setvar "PDSIZE" old-pdsize)
  (setq *error* nil)
  (princ)
)

(princ "\nDGPS2PLINE_v02.LSP loaded. Type DGPS2PLINE to run.")
(princ)
