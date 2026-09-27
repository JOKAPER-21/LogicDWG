;;; ============================================================================
;;; VIDDGPSTOSP_v01.LSP
;;; Production DGPS CSV importer for AutoCAD / Civil 3D
;;;
;;; Command: VIDDGPSTOSP
;;;
;;; Features:
;;;   - Reads ALL CSV rows until EOF
;;;   - ANSI/Windows-1252 and UTF-8/UTF-8 BOM support
;;;   - Header-name based column lookup
;;;   - Proper CSV quoted-field parser
;;;   - Creates POINT + Point Name + Code + Elevation MTEXT for every row
;;;   - Groups ALL records by Code
;;;   - Sorts geometry by Local Time, then CSV row number
;;;   - Creates 2D LWPOLYLINE or true 3D POLYLINE
;;;   - Verifies entity creation and reports counts
;;; ============================================================================

(vl-load-com)

;; ---------------------------------------------------------------------------
;; Globals used by error handler
;; ---------------------------------------------------------------------------
(setq dgps-*fh* nil)
(setq dgps-*old-error* nil)
(setq dgps-*old-clayer* nil)
(setq dgps-*old-cmdecho* nil)
(setq dgps-*old-osmode* nil)
(setq dgps-*old-pdsize* nil)
(setq dgps-*old-pdmode* nil)

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
;; Accepts integer / decimal / signed decimal / scientific notation.
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
;; Original Code is never changed for displayed text.
;; ---------------------------------------------------------------------------
(defun DGPS-SanitizeLayerName (s / src i ch out)
  (setq src (DGPS-Trim s)
        i 1
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
  ;; Layer names should not begin with a space.
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
;; POINT / MTEXT creation
;; ---------------------------------------------------------------------------
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

;; attach:
;; 1 TopLeft, 2 TopCenter, 3 TopRight
;; 4 MiddleLeft, 5 MiddleCenter, 6 MiddleRight
;; 7 BottomLeft, 8 BottomCenter, 9 BottomRight
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
      (cons 1 (if txt txt ""))
      '(7 . "Standard")
      '(210 0.0 0.0 1.0)
    )
  )
)

;; ---------------------------------------------------------------------------
;; 2D polyline
;; Uses LWPOLYLINE because it is a real 2D polyline and is compact.
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
            (list
              (cons 10 (list (car p) (cadr p)))
            )
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
(defun DGPS-Make3DPolyline (pts layer / head verts e v)
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
;; CSV parser
;; Properly handles quoted commas and escaped quotes.
;; ---------------------------------------------------------------------------
(defun DGPS-ParseCSV (s / i n ch next inquote cur fields)
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

;; Reads one logical CSV record. Supports quoted multiline fields.
(defun DGPS-ReadPhysicalLine (fh / b s gotLine)
  ;; Binary reader: unlike text read-line, this does NOT treat byte 0x1A
  ;; (Ctrl-Z / SUB) as end-of-file. The survey CSV contains 0x1A in
  ;; Latitude/Longitude fields, which was the reason previous versions
  ;; stopped after the first data row.
  (setq s ""
        gotLine nil)
  (while (and (not gotLine) (setq b (read-char fh)))
    (cond
      ;; LF
      ((= b 10)
       (setq gotLine T)
      )
      ;; CR: consume optional LF for CRLF files.
      ((= b 13)
       (setq b (read-char fh))
       (if (and b (/= b 10))
         ;; This file is expected to use CRLF. If a lone CR is encountered,
         ;; the next byte cannot be unread, so retain it in the current line.
         (setq s (strcat s (chr b)))
       )
       (setq gotLine T)
      )
      ;; Normal byte, including 0x1A.
      (T
       (setq s (strcat s (chr b)))
      )
    )
  )
  (if (or gotLine (> (strlen s) 0)) s nil)
)

;; Reads one logical CSV record. Supports quoted multiline fields.
(defun DGPS-ReadRecord (fh / s next)
  (setq s (DGPS-ReadPhysicalLine fh))
  (if s
    (progn
      (while (= (rem (DGPS-QuoteCount s) 2) 1)
        (setq next (DGPS-ReadPhysicalLine fh))
        (if next
          (setq s (strcat s "\n" next))
          (setq next nil)
        )
        (if (null next) (setq s nil))
      )
    )
  )
  s
)

;; ---------------------------------------------------------------------------
;; UTF-8 BOM detection
;; ---------------------------------------------------------------------------
(defun DGPS-HasUTF8BOM (file / f a b c result)
  (setq result nil)
  (setq f (open file "r"))
  (if f
    (progn
      (setq a (read-char f) b (read-char f) c (read-char f))
      (close f)
      (if (and (= a 239) (= b 187) (= c 191))
        (setq result T)
      )
    )
  )
  result
)

;; ---------------------------------------------------------------------------
;; Open CSV using AutoCAD text encoding support.
;; If UTF-8 is explicitly detected, use UTF-8.
;; For no BOM, try UTF-8 and fall back to ANSI if opening/reading fails.
;; ---------------------------------------------------------------------------
(defun DGPS-OpenCSV (file / fh)
  ;; IMPORTANT:
  ;; Use binary mode. AutoLISP text-mode reading can interpret byte 0x1A
  ;; (Ctrl-Z / SUB) as EOF. This DGPS CSV contains 0x1A characters in its
  ;; latitude/longitude fields, so text-mode reading stops after Pt1.
  ;; Binary mode lets DGPS-ReadPhysicalLine handle the actual CR/LF bytes.
  (setq fh (open file "rb"))
  (if fh
    (list fh "ANSI/BINARY")
    (list nil nil)
  )
)

;; ---------------------------------------------------------------------------
;; BOM stripping from first header field when necessary.
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
;; record = pname code north east elev time row sortkey
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
;; Time sort key.
;; Extract only the time part when possible. For this CSV, the date portion
;; is constant/irrelevant; time is what controls survey order.
;; ---------------------------------------------------------------------------
(defun DGPS-TimeKey (s / timeStr pos h m sec ms parts p2 p3)
  (setq timeStr (DGPS-Trim s))
  (setq pos (vl-string-search " " timeStr))
  (if pos
    (setq timeStr (substr timeStr (+ pos 2)))
  )
  (if (>= (strlen timeStr) 8)
    (progn
      (setq h (substr timeStr 1 2)
            m (substr timeStr 4 2)
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
;; Group records by Code WITHOUT sorting the master list.
;; This prevents string comparison/type bugs and guarantees no record loss.
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
;; Error handler
;; ---------------------------------------------------------------------------
(defun DGPS-Error (msg)
  (if (and dgps-*fh* (not (vl-catch-all-error-p
                            (vl-catch-all-apply 'close (list dgps-*fh*)))))
    nil
  )
  (setq dgps-*fh* nil)
  (if dgps-*old-clayer* (setvar "CLAYER" dgps-*old-clayer*))
  (if dgps-*old-cmdecho* (setvar "CMDECHO" dgps-*old-cmdecho*))
  (if dgps-*old-osmode* (setvar "OSMODE" dgps-*old-osmode*))
  (if dgps-*old-pdsize* (setvar "PDSIZE" dgps-*old-pdsize*))
  (if dgps-*old-pdmode* (setvar "PDMODE" dgps-*old-pdmode*))
  (setq *error* dgps-*old-error*)
  (if (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort"))
    (princ (strcat "\nDGPS2PLINE ERROR: " msg))
    (princ "\nDGPS2PLINE cancelled.")
  )
  (princ)
)

;; ---------------------------------------------------------------------------
;; Main command
;; ---------------------------------------------------------------------------
(defun c:VIDDGPSTOSP
  (/ csvFile openResult enc fh headerLine headers
     idxP idxC idxN idxE idxZ idxT
     line fields rowNo dataRows validRows skippedRows
     allRev allRecords rec pname code north east elev localTime
     groups g spLayer spPt spName spCode spElev spLayers
     p x y z sampleCount i)

  (setq dgps-*old-error* *error*)
  (setq *error* VIDDGPSTOSP-Error)
  (setq dgps-*old-clayer* (getvar "CLAYER"))
  (setq dgps-*old-cmdecho* (getvar "CMDECHO"))
  (setq dgps-*old-osmode* (getvar "OSMODE"))
  (setq dgps-*old-pdsize* (getvar "PDSIZE"))
  (setq dgps-*old-pdmode* (getvar "PDMODE"))

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "PDSIZE" 0.5)
  (setvar "PDMODE" 3)

  ;; Select CSV.
  (setq csvFile (getfiled "Select DGPS CSV File" "" "csv" 4))
  (if (null csvFile)
    (progn (VIDDGPSTOSP-Error "No CSV file selected.") (exit))
  )

  ;; Same binary-safe CSV reader as DGPS2PLINE v12.
  (setq openResult (DGPS-OpenCSV csvFile))
  (setq fh (car openResult))
  (setq enc (cadr openResult))

  (if (null fh)
    (progn (VIDDGPSTOSP-Error "Could not open CSV in binary mode.") (exit))
  )
  (setq dgps-*fh* fh)

  ;; Header.
  (setq headerLine (DGPS-ReadRecord fh))
  (if (null headerLine)
    (progn (VIDDGPSTOSP-Error "CSV file is empty.") (exit))
  )
  (setq headerLine (DGPS-StripBOM headerLine))
  (setq headers (DGPS-ParseCSV headerLine))

  (setq idxP (DGPS-HeaderIndex headers "Point Name"))
  (setq idxC (DGPS-HeaderIndex headers "Code"))
  (setq idxN (DGPS-HeaderIndex headers "Northing"))
  (setq idxE (DGPS-HeaderIndex headers "Easting"))
  (setq idxZ (DGPS-HeaderIndex headers "Elevation"))
  (setq idxT (DGPS-HeaderIndex headers "Local Time"))

  (if (or (null idxP) (null idxC) (null idxN) (null idxE) (null idxZ))
    (progn
      (VIDDGPSTOSP-Error
        "Required headers missing. Required: Point Name, Code, Northing, Easting, Elevation.")
      (exit)
    )
  )

  ;; Read every row.
  (setq rowNo 1 dataRows 0 validRows 0 skippedRows 0 allRev '())

  (while (setq line (DGPS-ReadRecord fh))
    (setq rowNo (1+ rowNo))
    (if (not (DGPS-BlankP line))
      (progn
        (setq dataRows (1+ dataRows))
        (setq fields (DGPS-ParseCSV line))
        (setq pname (DGPS-GetField fields idxP))
        (setq code  (DGPS-GetField fields idxC))
        (setq north (DGPS-GetField fields idxN))
        (setq east  (DGPS-GetField fields idxE))
        (setq elev  (DGPS-GetField fields idxZ))
        (setq localTime
          (if (null idxT) "" (DGPS-GetField fields idxT))
        )

        (if (and (/= pname "")
                 (/= code "")
                 (DGPS-NumericP north)
                 (DGPS-NumericP east)
                 (DGPS-NumericP elev))
          (progn
            (setq allRev
              (cons
                (list pname code north east elev localTime rowNo nil)
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
  (setq groups (DGPS-GroupByCode allRecords))

  ;; Import report.
  (princ "\n========================================")
  (princ "\nVIDDGPSTOSP - SURVEY POINT IMPORT")
  (princ "\n========================================")
  (princ (strcat "\nFile: " csvFile))
  (princ (strcat "\nEncoding: " enc))
  (princ (strcat "\nData rows read: " (itoa dataRows)))
  (princ (strcat "\nValid records: " (itoa validRows)))
  (princ (strcat "\nSkipped rows: " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes: " (itoa (length groups))))
  (princ "\n----------------------------------------")

  (foreach g groups
    (princ
      (strcat "\n" (car g) " -> " (itoa (length (cdr g))))
    )
  )

  ;; First five records.
  (princ "\n\nFIRST FIVE RECORDS")
  (setq sampleCount (min 5 (length allRecords)) i 0)
  (while (< i sampleCount)
    (setq rec (nth i allRecords))
    (princ
      (strcat
        "\n" (itoa (1+ i))
        ": " (DGPS-R-PName rec)
        " | " (DGPS-R-Code rec)
        " | E=" (DGPS-R-East rec)
        " | N=" (DGPS-R-North rec)
        " | Z=" (DGPS-R-Elev rec)
        " | T=" (DGPS-R-Time rec)
      )
    )
    (setq i (1+ i))
  )

  ;; SURVEY POINTS ONLY.
  ;; Each valid row creates one POINT and three MTEXT objects.
  ;; No Polyline and no 3D Polyline is created.
  (setq spPt 0 spName 0 spCode 0 spElev 0 spLayers '())

  (foreach rec allRecords
    (setq code (DGPS-R-Code rec))
    (setq spLayer (strcat "sp_" (DGPS-SanitizeLayerName code)))

    (if (not (tblsearch "LAYER" spLayer))
      (DGPS-EnsureLayer spLayer 4)
    )
    (if (null (member spLayer spLayers))
      (setq spLayers (cons spLayer spLayers))
    )

    ;; Exact E,N,Z survey coordinate.
    (setq x (atof (DGPS-R-East rec)))
    (setq y (atof (DGPS-R-North rec)))
    (setq z (atof (DGPS-R-Elev rec)))
    (setq p (list x y z))

    ;; POINT.
    (if (DGPS-MakePoint p spLayer)
      (setq spPt (1+ spPt))
    )

    ;; Point Name - Middle Right, exact survey coordinate.
    (if (DGPS-MakeMText p (DGPS-R-PName rec) 0.5 spLayer 6)
      (setq spName (1+ spName))
    )

    ;; Code - Top Left, exact survey coordinate.
    (if (DGPS-MakeMText p code 0.5 spLayer 1)
      (setq spCode (1+ spCode))
    )

    ;; Elevation - Bottom Left, exact survey coordinate.
    (if (DGPS-MakeMText p (DGPS-R-Elev rec) 0.5 spLayer 7)
      (setq spElev (1+ spElev))
    )
  )

  ;; Final report.
  (princ "\n\n========================================")
  (princ "\nVIDDGPSTOSP COMPLETE")
  (princ "\n========================================")
  (princ (strcat "\nRows read:              " (itoa dataRows)))
  (princ (strcat "\nValid records:          " (itoa validRows)))
  (princ (strcat "\nSkipped rows:           " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes:           " (itoa (length groups))))
  (princ (strcat "\nPOINT entities:         " (itoa spPt)))
  (princ (strcat "\nPoint Name MTEXT:       " (itoa spName)))
  (princ (strcat "\nCode MTEXT:             " (itoa spCode)))
  (princ (strcat "\nElevation MTEXT:        " (itoa spElev)))
  (princ (strcat "\nSurvey layers:           " (itoa (length spLayers))))
  (princ "\nPolyline created:        0")
  (princ "\n3D Polyline created:     0")

  (if (and (= validRows spPt)
           (= validRows spName)
           (= validRows spCode)
           (= validRows spElev))
    (princ "\n\nSurvey record integrity: OK")
    (princ "\n\nWARNING: Survey entity count does not match valid record count.")
  )

  (princ "\n========================================")

  ;; Restore AutoCAD state.
  (setvar "CLAYER" dgps-*old-clayer*)
  (setvar "CMDECHO" dgps-*old-cmdecho*)
  (setvar "OSMODE" dgps-*old-osmode*)
  (setvar "PDSIZE" dgps-*old-pdsize*)
  (setvar "PDMODE" dgps-*old-pdmode*)
  (setq *error* dgps-*old-error*)

  (princ "\nVIDDGPSTOSP finished successfully.")
  (princ)
)

(defun VIDDGPSTOSP-Error (msg)
  (if dgps-*fh*
    (progn
      (vl-catch-all-apply 'close (list dgps-*fh*))
      (setq dgps-*fh* nil)
    )
  )
  (if dgps-*old-clayer* (setvar "CLAYER" dgps-*old-clayer*))
  (if dgps-*old-cmdecho* (setvar "CMDECHO" dgps-*old-cmdecho*))
  (if dgps-*old-osmode* (setvar "OSMODE" dgps-*old-osmode*))
  (if dgps-*old-pdsize* (setvar "PDSIZE" dgps-*old-pdsize*))
  (if dgps-*old-pdmode* (setvar "PDMODE" dgps-*old-pdmode*))
  (setq *error* dgps-*old-error*)
  (if (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort"))
    (princ (strcat "\nVIDDGPSTOSP ERROR: " msg))
    (princ "\nVIDDGPSTOSP cancelled.")
  )
  (princ)
)


(princ "\nVIDDGPSTOSP_v01 loaded. Type VIDDGPSTOSP to run.")
(princ)
