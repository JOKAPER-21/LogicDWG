;;; ============================================================================
;;;  DGPS2PLINE_v04.LSP
;;;
;;;  Imports a DGPS survey CSV (Point Name, Code, Northing, Easting,
;;;  Elevation, Local Time - plus any number of other ignored columns) and:
;;;
;;;    1. Processes EVERY valid data row in the CSV (no early stop, no
;;;       filtering, no de-duplication).
;;;    2. Creates a survey-point graphic (POINT + 3 MTEXT) for every valid
;;;       row, with a clean, non-overlapping text layout.
;;;    3. Groups all rows by Code, sorts each group by Local Time
;;;       (falling back to CSV row number), and builds a Polyline or
;;;       3Dpolyline per Code.
;;;
;;;  Command:  DGPS2PLINE
;;; ============================================================================

(vl-load-com)

;; ----------------------------------------------------------------------------
;; 0. SMALL GENERAL UTILITIES
;; ----------------------------------------------------------------------------

(defun DGPS-Trim (s)
  (if s (vl-string-trim " \t\r\n" s) "")
)

(defun DGPS-RTrimCR (s)
  ;; strip a single trailing CR left over from CRLF files
  (if (and s (> (strlen s) 0) (= (substr s (strlen s) 1) "\r"))
    (substr s 1 (1- (strlen s)))
    s
  )
)

(defun DGPS-IsBlank (s)
  (= (DGPS-Trim s) "")
)

;; true numeric test: optional sign, digits, optional single decimal point
(defun DGPS-IsNumeric (raw / s n i c ok dotc code)
  (setq s (DGPS-Trim raw))
  (setq ok T)
  (if (= s "")
    (setq ok nil)
    (progn
      (setq n (strlen s) i 1 dotc 0)
      (if (or (= (substr s 1 1) "-") (= (substr s 1 1) "+"))
        (setq i 2)
      )
      (if (> i n) (setq ok nil))
      (while (and ok (<= i n))
        (setq c (substr s i 1))
        (setq code (ascii c))
        (cond
          ((= c ".")
           (setq dotc (1+ dotc))
           (if (> dotc 1) (setq ok nil))
          )
          ((not (and (>= code 48) (<= code 57)))
           (setq ok nil)
          )
        )
        (setq i (1+ i))
      )
    )
  )
  ok
)

;; sanitize a string for use as a layer name (Code text itself stays untouched)
(defun DGPS-SanitizeLayer (raw / s n i c out bad)
  (setq s (DGPS-Trim raw))
  (setq bad (list "<" ">" "/" "\\" "\"" ":" ";" "?" "*" "|" "," "=" "`"))
  (setq out "" n (strlen s) i 1)
  (while (<= i n)
    (setq c (substr s i 1))
    (if (or (= c " ") (vl-position c bad))
      (setq out (strcat out "_"))
      (setq out (strcat out c))
    )
    (setq i (1+ i))
  )
  (if (= out "") (setq out "_NOCODE"))
  out
)

;; ----------------------------------------------------------------------------
;; 1. LAYER HELPER
;; ----------------------------------------------------------------------------

(defun DGPS-AddLayer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake
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
  name
)

;; ----------------------------------------------------------------------------
;; 2. MTEXT / POINT ENTITY CREATION  (direct entmake, no COMMAND)
;; ----------------------------------------------------------------------------

;; attach: 1=TL 2=TC 3=TR 4=ML 5=MC 6=MR 7=BL 8=BC 9=BR
(defun DGPS-MakeMText (pt txt ht layer attach)
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbMText")
      (cons 10 pt)
      (cons 40 ht)
      (cons 41 0.0)
      (cons 71 attach)
      (cons 72 1)
      (cons 1 txt)
      '(7 . "Standard")
      '(210 0.0 0.0 1.0)
    )
  )
)

(defun DGPS-MakePoint (pt layer)
  (entmakex
    (list
      '(0 . "POINT")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbPoint")
      (cons 10 pt)
      '(210 0.0 0.0 1.0)
    )
  )
)

;; ----------------------------------------------------------------------------
;; 3. GEOMETRY (2D LWPOLYLINE / 3D POLYLINE)
;; ----------------------------------------------------------------------------

(defun DGPS-MakeLWPolyline (pts layer / edata)
  (setq edata
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))
      '(70 . 0)
    )
  )
  (foreach p pts
    (setq edata (append edata (list (cons 10 (list (car p) (cadr p))))))
  )
  (entmakex edata)
)

(defun DGPS-Make3DPolyline (pts layer / poly)
  (setq poly
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
  (foreach p pts
    (entmakex
      (list
        '(0 . "VERTEX")
        (cons 8 layer)
        '(100 . "AcDbEntity")
        '(100 . "AcDbVertex")
        '(100 . "AcDb3dPolylineVertex")
        (cons 10 p)
        '(70 . 32)
      )
    )
  )
  (entmakex (list '(0 . "SEQEND") (cons 8 layer)))
  poly
)

;; ----------------------------------------------------------------------------
;; 4. CSV LOW-LEVEL PARSER
;; ----------------------------------------------------------------------------

(defun DGPS-CountQuotes (s / n i cnt)
  (setq cnt 0 n (strlen s) i 1)
  (while (<= i n)
    (if (= (substr s i 1) "\"") (setq cnt (1+ cnt)))
    (setq i (1+ i))
  )
  cnt
)

;; reads one logical CSV record (may span several physical lines if a
;; quoted field contains embedded newlines); returns raw string or nil at EOF
(defun DGPS-ReadLogicalLine (f / raw nxt more)
  (setq raw (read-line f))
  (if raw
    (progn
      (setq raw (DGPS-RTrimCR raw))
      (setq more T)
      (while (and more (oddp (DGPS-CountQuotes raw)))
        (setq nxt (read-line f))
        (if nxt
          (setq raw (strcat raw "\n" (DGPS-RTrimCR nxt)))
          (setq more nil)
        )
      )
    )
  )
  raw
)

;; splits one logical CSV line into fields, honouring quotes, escaped
;; double-quotes ("") and commas inside quoted fields
(defun DGPS-ParseCSVLine (s / i n ch cur inquote fields)
  (setq fields '() cur "" inquote nil i 1 n (strlen s))
  (while (<= i n)
    (setq ch (substr s i 1))
    (cond
      ((and inquote (= ch "\"") (<= (1+ i) n) (= (substr s (1+ i) 1) "\""))
       (setq cur (strcat cur "\""))
       (setq i (+ i 2))
      )
      ((= ch "\"")
       (setq inquote (not inquote))
       (setq i (1+ i))
      )
      ((and (not inquote) (= ch ","))
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
  (setq fields (cons cur fields))
  (reverse fields)
)

;; strip a UTF-8 BOM (as 3 raw bytes/chars EF BB BF) from the very first field
(defun DGPS-StripBOM (s)
  (if (and s (>= (strlen s) 3)
           (= (ascii (substr s 1 1)) 239)
           (= (ascii (substr s 2 1)) 187)
           (= (ascii (substr s 3 1)) 191)
      )
    (substr s 4)
    s
  )
)

(defun DGPS-DetectBOM (path / f b1 b2 b3 bom)
  (setq bom nil)
  (setq f (open path "r"))
  (if f
    (progn
      (setq b1 (read-char f))
      (setq b2 (read-char f))
      (setq b3 (read-char f))
      (if (and (= b1 239) (= b2 187) (= b3 191)) (setq bom T))
      (close f)
    )
  )
  bom
)

;; ----------------------------------------------------------------------------
;; 5. RECORD ACCESSORS  (record = pname code north east elev time row sortkey)
;; ----------------------------------------------------------------------------

(defun rec-pname   (r) (nth 0 r))
(defun rec-code    (r) (nth 1 r))
(defun rec-north   (r) (nth 2 r))
(defun rec-east    (r) (nth 3 r))
(defun rec-elev    (r) (nth 4 r))
(defun rec-time    (r) (nth 5 r))
(defun rec-row     (r) (nth 6 r))
(defun rec-sortkey (r) (nth 7 r))

;; a Local Time string is treated as a sortable key only when it plausibly
;; looks like "YYYY-MM-DD HH:MM:SS[.mmm]" - lexicographic order = chrono order
(defun DGPS-TimeSortKey (raw / s)
  (setq s (DGPS-Trim raw))
  (if (and (>= (strlen s) 10)
           (= (substr s 5 1) "-")
           (= (substr s 8 1) "-")
      )
    s
    nil
  )
)

(defun DGPS-RecLess (a b / ka kb)
  (setq ka (rec-sortkey a) kb (rec-sortkey b))
  (cond
    ((and ka kb)
     (cond
       ((string< ka kb) T)
       ((= ka kb) (< (rec-row a) (rec-row b)))
       (T nil)
     )
    )
    (T (< (rec-row a) (rec-row b)))
  )
)

;; group a list of records by Code -> ( (code . recs) (code . recs) ... )
(defun DGPS-GroupByCode (allrecs / sorted groups curcode curlist r)
  (setq sorted
    (vl-sort allrecs (function (lambda (a b) (< (rec-code a) (rec-code b)))))
  )
  (setq groups '() curcode nil curlist '())
  (foreach r sorted
    (cond
      ((null curcode)
       (setq curcode (rec-code r))
       (setq curlist (list r))
      )
      ((= (rec-code r) curcode)
       (setq curlist (cons r curlist))
      )
      (T
       (setq groups (cons (cons curcode (reverse curlist)) groups))
       (setq curcode (rec-code r))
       (setq curlist (list r))
      )
    )
  )
  (if curcode (setq groups (cons (cons curcode (reverse curlist)) groups)))
  (reverse groups)
)

;; ----------------------------------------------------------------------------
;; 6. ERROR HANDLER
;; ----------------------------------------------------------------------------

(defun DGPS-Error (msg / )
  (if (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort")
      )
    (princ (strcat "\nDGPS2PLINE error: " msg))
  )
  (if (and (boundp 'dgps-fh) dgps-fh) (progn (close dgps-fh) (setq dgps-fh nil)))
  (if (boundp 'dgps-oldclayer)  (if dgps-oldclayer  (setvar "CLAYER"  dgps-oldclayer)))
  (if (boundp 'dgps-oldcmdecho) (if dgps-oldcmdecho (setvar "CMDECHO" dgps-oldcmdecho)))
  (if (boundp 'dgps-oldosmode)  (if dgps-oldosmode  (setvar "OSMODE"  dgps-oldosmode)))
  (if (boundp 'dgps-oldpdsize)  (if dgps-oldpdsize  (setvar "PDSIZE"  dgps-oldpdsize)))
  (if (boundp 'dgps-oldpdmode)  (if dgps-oldpdmode  (setvar "PDMODE"  dgps-oldpdmode)))
  (setq *error* dgps-olderr)
  (princ)
)

;; ----------------------------------------------------------------------------
;; 7. MAIN COMMAND
;; ----------------------------------------------------------------------------

(defun c:DGPS2PLINE
       (/ dgps-olderr dgps-fh dgps-oldclayer dgps-oldcmdecho dgps-oldosmode
          dgps-oldpdsize dgps-oldpdmode
          outType csvFile bom headerLine headerFields
          idxPName idxCode idxNorth idxEast idxElev idxTime maxIdx
          rawLine fields lineNo dataRowCount parsedRowCount skipCount
          pname code northS eastS elevS timeS sortkey rec
          allRecordsRev allRecords n5 i
          groupsForReport gAll gRec
          spPtCount spNameCount spCodeCount spElevCount
          spLayer gplLayer x y z pt code2
          groups grp gCode gRecs sortedGrp pts p
          polyCount singleCount uniqueCodes
          spLayerSet gplLayerSet
       )

  (setq dgps-olderr *error*)
  (setq *error* DGPS-Error)

  (setq dgps-oldclayer  (getvar "CLAYER"))
  (setq dgps-oldcmdecho (getvar "CMDECHO"))
  (setq dgps-oldosmode  (getvar "OSMODE"))
  (setq dgps-oldpdsize  (getvar "PDSIZE"))
  (setq dgps-oldpdmode  (getvar "PDMODE"))

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "PDSIZE" 0.5)
  (setvar "PDMODE" 3)

  ;; --- 7.1 ask output type ------------------------------------------------
  (initget "Polyline 3Dpolyline")
  (setq outType
    (getkword "\nSelect output type [Polyline/3Dpolyline] <Polyline>: ")
  )
  (if (not outType) (setq outType "Polyline"))

  ;; --- 7.2 select CSV file -------------------------------------------------
  (setq csvFile (getfiled "Select DGPS CSV File" "" "csv" 4))
  (if (not csvFile)
    (progn
      (princ "\nNo file selected. Command cancelled.")
      (setq *error* dgps-olderr)
      (setvar "CLAYER" dgps-oldclayer)
      (setvar "CMDECHO" dgps-oldcmdecho)
      (setvar "OSMODE" dgps-oldosmode)
      (setvar "PDSIZE" dgps-oldpdsize)
      (setvar "PDMODE" dgps-oldpdmode)
      (exit)
    )
  )

  ;; --- 7.3 encoding detection ----------------------------------------------
  (setq bom (DGPS-DetectBOM csvFile))

  ;; --- 7.4 open file --------------------------------------------------------
  (setq dgps-fh (open csvFile "r"))
  (if (not dgps-fh)
    (progn
      (princ "\nCould not open CSV file.")
      (DGPS-Error nil)
      (exit)
    )
  )

  ;; --- 7.5 header -------------------------------------------------------
  (setq rawLine (DGPS-ReadLogicalLine dgps-fh))
  (if (not rawLine)
    (progn
      (princ "\nCSV file is empty.")
      (DGPS-Error nil)
      (exit)
    )
  )
  (setq rawLine (DGPS-StripBOM rawLine))
  (setq headerFields (mapcar 'DGPS-Trim (DGPS-ParseCSVLine rawLine)))
  (setq lineNo 1)

  (setq idxPName (vl-position "POINT NAME" (mapcar 'strcase headerFields)))
  (setq idxCode  (vl-position "CODE"       (mapcar 'strcase headerFields)))
  (setq idxNorth (vl-position "NORTHING"   (mapcar 'strcase headerFields)))
  (setq idxEast  (vl-position "EASTING"    (mapcar 'strcase headerFields)))
  (setq idxElev  (vl-position "ELEVATION"  (mapcar 'strcase headerFields)))
  (setq idxTime  (vl-position "LOCAL TIME" (mapcar 'strcase headerFields)))

  ;; NOTE: index 0 (first column) is a perfectly valid, FOUND result.
  ;; Only (= idxX nil) means "column not found". Never test with (not idxX)
  ;; alone against a numeric-0-as-false assumption from other languages -
  ;; in AutoLISP 0 is truthy, so explicit (= idxX nil) checks are used below.
  (if (or (= idxPName nil) (= idxCode nil) (= idxNorth nil)
          (= idxEast nil) (= idxElev nil))
    (progn
      (princ "\nERROR: Required CSV column(s) missing. Need: Point Name, Code, Northing, Easting, Elevation.")
      (DGPS-Error nil)
      (exit)
    )
  )

  ;; --- 7.6 read ALL data rows until EOF ------------------------------------
  (setq allRecordsRev '() dataRowCount 0 parsedRowCount 0 skipCount 0)

  (while (setq rawLine (DGPS-ReadLogicalLine dgps-fh))
    (setq lineNo (1+ lineNo))
    (if (not (DGPS-IsBlank rawLine))
      (progn
        (setq dataRowCount (1+ dataRowCount))
        (setq fields (DGPS-ParseCSVLine rawLine))
        (setq maxIdx (apply 'max (list idxPName idxCode idxNorth idxEast idxElev)))
        (if (> (length fields) maxIdx)
          (progn
            (setq parsedRowCount (1+ parsedRowCount))
            (setq pname  (DGPS-Trim (nth idxPName fields)))
            (setq code   (DGPS-Trim (nth idxCode  fields)))
            (setq northS (DGPS-Trim (nth idxNorth fields)))
            (setq eastS  (DGPS-Trim (nth idxEast  fields)))
            (setq elevS  (DGPS-Trim (nth idxElev  fields)))
            (setq timeS
              (if (and idxTime (/= idxTime nil) (> (length fields) idxTime))
                (DGPS-Trim (nth idxTime fields))
                ""
              )
            )
            (if (and (/= pname "") (/= code "")
                     (DGPS-IsNumeric northS)
                     (DGPS-IsNumeric eastS)
                     (DGPS-IsNumeric elevS)
                )
              (progn
                (setq sortkey (DGPS-TimeSortKey timeS))
                (setq rec (list pname code northS eastS elevS timeS lineNo sortkey))
                (setq allRecordsRev (cons rec allRecordsRev))
              )
              (setq skipCount (1+ skipCount))
            )
          )
          (setq skipCount (1+ skipCount))
        )
      )
    )
  )

  (close dgps-fh)
  (setq dgps-fh nil)

  (setq allRecords (reverse allRecordsRev))

  ;; --- 7.7 group by code once (used for the report AND the geometry pass) --
  (setq groups (DGPS-GroupByCode allRecords))
  (setq uniqueCodes (length groups))

  ;; --- 7.8 VALIDATION / DEBUG REPORT ---------------------------------------
  (princ "\n========================================")
  (princ "\nDGPS2PLINE CSV VALIDATION")
  (princ "\n========================================")
  (princ (strcat "\nFile:\n" csvFile))
  (princ (strcat "\n\nEncoding:\n" (if bom "UTF-8 (BOM detected)" "ANSI/Windows-1252 (assumed)")))
  (princ (strcat "\n\nPhysical lines read:\n" (itoa lineNo)))
  (princ (strcat "\n\nData rows:\n" (itoa dataRowCount)))
  (princ (strcat "\n\nParsed rows:\n" (itoa parsedRowCount)))
  (princ (strcat "\n\nValid records:\n" (itoa (length allRecords))))
  (princ (strcat "\n\nSkipped records:\n" (itoa skipCount)))
  (princ (strcat "\n\nUnique Codes:\n" (itoa uniqueCodes)))
  (princ "\n\n========================================")
  (princ "\nCODE COUNTS")
  (princ "\n========================================")
  (princ "\nCode                         Count")
  (princ "\n----------------------------------------")
  (foreach grp groups
    (princ (strcat "\n" (car grp) "  ->  " (itoa (length (cdr grp)))))
  )
  (princ "\n========================================")

  ;; --- 7.9 FIRST FIVE RECORD DEBUG -----------------------------------------
  (princ "\n\nFIRST FIVE PARSED RECORDS")
  (princ "\n----------------------------------------")
  (setq n5 (min 5 (length allRecords)))
  (setq i 0)
  (repeat n5
    (setq rec (nth i allRecords))
    (princ (strcat "\nRecord " (itoa (1+ i)) ":"))
    (princ (strcat "\n  Point Name: " (rec-pname rec)))
    (princ (strcat "\n  Code: " (rec-code rec)))
    (princ (strcat "\n  Northing: " (rec-north rec)))
    (princ (strcat "\n  Easting: " (rec-east rec)))
    (princ (strcat "\n  Elevation: " (rec-elev rec)))
    (princ (strcat "\n  Local Time: " (rec-time rec)))
    (setq i (1+ i))
  )
  (princ "\n----------------------------------------\n")

  ;; --- 7.10 SURVEY POINT GRAPHICS - EVERY RECORD ---------------------------
  (setq spLayerSet '() gplLayerSet '())
  (setq spPtCount 0 spNameCount 0 spCodeCount 0 spElevCount 0)

  (foreach rec allRecords
    (setq code2   (rec-code rec))
    (setq spLayer (strcat "sp_" (DGPS-SanitizeLayer code2)))
    (if (not (vl-position spLayer spLayerSet))
      (progn (DGPS-AddLayer spLayer 4) (setq spLayerSet (cons spLayer spLayerSet)))
    )

    (setq x (atof (rec-east rec)))
    (setq y (atof (rec-north rec)))
    (setq z (atof (rec-elev rec)))
    (setq pt (list x y z))

    ;; POINT entity
    (if (DGPS-MakePoint pt spLayer) (setq spPtCount (1+ spPtCount)))

    ;; Point Name -> Middle Right, close offset above-right of the point
    (if (DGPS-MakeMText
          (list (+ x 0.75) (+ y 0.50) z)
          (rec-pname rec) 0.5 spLayer 6
        )
      (setq spNameCount (1+ spNameCount))
    )

    ;; Code -> Top Left, lower-left of the point
    (if (DGPS-MakeMText
          (list (- x 0.75) (- y 0.75) z)
          code2 0.5 spLayer 1
        )
      (setq spCodeCount (1+ spCodeCount))
    )

    ;; Elevation -> Bottom Left, lower-right of the point
    (if (DGPS-MakeMText
          (list (+ x 0.75) (- y 0.75) z)
          (rec-elev rec) 0.5 spLayer 7
        )
      (setq spElevCount (1+ spElevCount))
    )
  )

  ;; --- 7.11 FINAL SURVEY-POINT VALIDATION ----------------------------------
  (princ "\n========================================")
  (princ "\nSURVEY POINT RESULT")
  (princ "\n========================================")
  (princ (strcat "\n\nValid CSV records:       " (itoa (length allRecords))))
  (princ (strcat "\n\nPOINT entities:          " (itoa spPtCount)))
  (princ (strcat "\nPoint Name MTEXT:        " (itoa spNameCount)))
  (princ (strcat "\nCode MTEXT:              " (itoa spCodeCount)))
  (princ (strcat "\nElevation MTEXT:         " (itoa spElevCount)))
  (princ "\n========================================")

  (if (or (/= spPtCount (length allRecords))
          (/= spNameCount (length allRecords))
          (/= spCodeCount (length allRecords))
          (/= spElevCount (length allRecords))
      )
    (princ
      (strcat
        "\nERROR: "
        (itoa (- (length allRecords) spPtCount)) " survey POINT entities, "
        (itoa (- (length allRecords) spNameCount)) " Point Name MTEXT, "
        (itoa (- (length allRecords) spCodeCount)) " Code MTEXT, and "
        (itoa (- (length allRecords) spElevCount)) " Elevation MTEXT failed to create."
      )
    )
  )

  ;; --- 7.12 GEOMETRY - GROUP BY CODE, SORT, BUILD POLYLINES ----------------
  (setq polyCount 0 singleCount 0)

  (foreach grp groups
    (setq gCode  (car grp))
    (setq gRecs  (cdr grp))
    (setq gplLayer (strcat "gpl_" (DGPS-SanitizeLayer gCode)))
    (if (not (vl-position gplLayer gplLayerSet))
      (progn (DGPS-AddLayer gplLayer 3) (setq gplLayerSet (cons gplLayer gplLayerSet)))
    )

    (setq sortedGrp (vl-sort gRecs (function DGPS-RecLess)))

    (if (< (length sortedGrp) 2)
      (setq singleCount (1+ singleCount))
      (progn
        (setq pts '())
        (foreach rec sortedGrp
          (setq x (atof (rec-east rec)))
          (setq y (atof (rec-north rec)))
          (setq z (atof (rec-elev rec)))
          (setq pts (cons (if (= outType "3Dpolyline") (list x y z) (list x y)) pts))
        )
        (setq pts (reverse pts))
        (if (= outType "3Dpolyline")
          (DGPS-Make3DPolyline pts gplLayer)
          (DGPS-MakeLWPolyline pts gplLayer)
        )
        (setq polyCount (1+ polyCount))
      )
    )
  )

  ;; --- 7.13 FINAL COMPLETION REPORT -----------------------------------------
  (princ "\n\n========================================")
  (princ "\nDGPS2PLINE COMPLETE")
  (princ "\n========================================")
  (princ (strcat "\n\nCSV:\n" csvFile))
  (princ (strcat "\n\nEncoding:\n" (if bom "UTF-8 (BOM detected)" "ANSI/Windows-1252 (assumed)")))
  (princ (strcat "\n\nTotal data rows:\n" (itoa dataRowCount)))
  (princ (strcat "\n\nValid records:\n" (itoa (length allRecords))))
  (princ (strcat "\n\nSkipped records:\n" (itoa skipCount)))
  (princ (strcat "\n\nUnique Codes:\n" (itoa uniqueCodes)))
  (princ (strcat "\n\nSurvey POINT entities:\n" (itoa spPtCount)))
  (princ (strcat "\n\nPoint Name MTEXT:\n" (itoa spNameCount)))
  (princ (strcat "\n\nCode MTEXT:\n" (itoa spCodeCount)))
  (princ (strcat "\n\nElevation MTEXT:\n" (itoa spElevCount)))
  (princ (strcat "\n\nSurvey point layers:\n" (itoa (length spLayerSet))))
  (princ (strcat "\n\nGeometry layers:\n" (itoa (length gplLayerSet))))
  (princ (strcat "\n\nPolylines created:\n" (itoa polyCount)))
  (princ (strcat "\n\nSingle-point Codes:\n" (itoa singleCount)))
  (princ "\n\n========================================")

  ;; --- 7.14 restore state ---------------------------------------------------
  (setvar "CLAYER" dgps-oldclayer)
  (setvar "CMDECHO" dgps-oldcmdecho)
  (setvar "OSMODE" dgps-oldosmode)
  (setvar "PDSIZE" dgps-oldpdsize)
  (setvar "PDMODE" dgps-oldpdmode)
  (setq *error* dgps-olderr)

  (princ)
)

(princ "\nDGPS2PLINE_v04 loaded. Type DGPS2PLINE to run.")
(princ)
