;;; ============================================================================
;;; vidDgpsToLine.lsp  (VIDDGPSTOLINE v14)
;;; DGPS CSV -> Polyline / 3D Polyline (one per Code)
;;;
;;; Command: VIDDGPSTOLINE
;;;
;;; Features:
;;;   - Reads ALL CSV rows until EOF (binary reader, 0x1A safe)
;;;   - Header-name based column lookup, quoted-field CSV parser
;;;   - Groups ALL records by Code
;;;   - Sorts each Code by Local Time, then CSV row number
;;;   - Creates 2D LWPOLYLINE on layer pl_<Code>, or
;;;     true 3D POLYLINE on layer 3dpl_<Code>
;;;   - NO points and NO text are created
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

;; Reads one physical line. Binary reader: does NOT treat byte 0x1A
;; (Ctrl-Z) as end-of-file, unlike text-mode read-line.
(defun DGPS-ReadPhysicalLine (fh / b s gotLine)
  (setq s ""
        gotLine nil)
  (while (and (not gotLine) (setq b (read-char fh)))
    (cond
      ((= b 10)
       (setq gotLine T)
      )
      ((= b 13)
       (setq b (read-char fh))
       (if (and b (/= b 10))
         (setq s (strcat s (chr b)))
       )
       (setq gotLine T)
      )
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
;; Open CSV in binary mode (0x1A safe).
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
;; Time sort key (time part only; falls back to CSV row number).
;; ---------------------------------------------------------------------------
(defun DGPS-TimeKey (s / timeStr pos h m sec ms p2 p3)
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
;; Group records by Code (master list is not sorted or modified).
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
  (if dgps-*fh*
    (progn
      (vl-catch-all-apply 'close (list dgps-*fh*))
      (setq dgps-*fh* nil)
    )
  )
  (if dgps-*old-clayer* (setvar "CLAYER" dgps-*old-clayer*))
  (if dgps-*old-cmdecho* (setvar "CMDECHO" dgps-*old-cmdecho*))
  (if dgps-*old-osmode* (setvar "OSMODE" dgps-*old-osmode*))
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
;; Main command
;; ---------------------------------------------------------------------------
(defun c:VIDDGPSTOLINE
  (/ outType csvFile openResult enc fh
     headerLine headers
     idxP idxC idxN idxE idxZ idxT
     line fields rowNo dataRows validRows skippedRows
     allRev allRecords rec pname code north east elev localTime key
     groups g codeRecs sortedRecs pts x y z
     plLayer geomCount singleCount plLayers geomFailures
     result i sampleCount)

  (setq dgps-*old-error* *error*)
  (setq *error* DGPS-Error)

  (setq dgps-*old-clayer* (getvar "CLAYER"))
  (setq dgps-*old-cmdecho* (getvar "CMDECHO"))
  (setq dgps-*old-osmode* (getvar "OSMODE"))

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  ;; -------------------------------------------------------------------------
  ;; Output type
  ;; -------------------------------------------------------------------------
  (initget "Polyline 3Dpolyline")
  (setq outType
    (getkword "\nSelect output type [Polyline/3Dpolyline] <Polyline>: ")
  )
  (if (null outType) (setq outType "Polyline"))

  ;; -------------------------------------------------------------------------
  ;; CSV selection
  ;; -------------------------------------------------------------------------
  (setq csvFile (getfiled "Select DGPS CSV File" "" "csv" 4))
  (if (null csvFile)
    (progn
      (DGPS-Error "No CSV file selected.")
      (exit)
    )
  )

  ;; -------------------------------------------------------------------------
  ;; Open CSV
  ;; -------------------------------------------------------------------------
  (setq openResult (DGPS-OpenCSV csvFile))
  (setq fh (car openResult))
  (setq enc (cadr openResult))
  (if (null fh)
    (progn
      (DGPS-Error "Could not open CSV in binary mode.")
      (exit)
    )
  )
  (setq dgps-*fh* fh)

  ;; -------------------------------------------------------------------------
  ;; Header
  ;; -------------------------------------------------------------------------
  (setq headerLine (DGPS-ReadRecord fh))
  (if (null headerLine)
    (progn
      (DGPS-Error "CSV file is empty.")
      (exit)
    )
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
      (DGPS-Error
        "Required headers missing. Required: Point Name, Code, Northing, Easting, Elevation.")
      (exit)
    )
  )

  ;; -------------------------------------------------------------------------
  ;; Read EVERY row until EOF
  ;; -------------------------------------------------------------------------
  (setq rowNo 1
        dataRows 0
        validRows 0
        skippedRows 0
        allRev '())

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
        (setq localTime (if (null idxT) "" (DGPS-GetField fields idxT)))

        (if (and (/= pname "")
                 (/= code "")
                 (DGPS-NumericP north)
                 (DGPS-NumericP east)
                 (DGPS-NumericP elev))
          (progn
            (setq key (DGPS-TimeKey localTime))
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
  (setq groups (DGPS-GroupByCode allRecords))

  ;; -------------------------------------------------------------------------
  ;; Report input
  ;; -------------------------------------------------------------------------
  (princ "\n========================================")
  (princ "\nVIDDGPSTOLINE CSV IMPORT")
  (princ "\n========================================")
  (princ (strcat "\nFile: " csvFile))
  (princ (strcat "\nEncoding: " enc))
  (princ (strcat "\nOutput type: " outType))
  (princ (strcat "\nData rows read: " (itoa dataRows)))
  (princ (strcat "\nValid records: " (itoa validRows)))
  (princ (strcat "\nSkipped rows: " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes: " (itoa (length groups))))
  (princ "\n----------------------------------------")
  (foreach g groups
    (princ (strcat "\n" (car g) " -> " (itoa (length (cdr g)))))
  )

  (princ "\n\nFIRST FIVE RECORDS")
  (setq sampleCount (min 5 (length allRecords))
        i 0)
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

  ;; -------------------------------------------------------------------------
  ;; Geometry: one pass over every Code group (no points, no text)
  ;; -------------------------------------------------------------------------
  (setq geomCount 0 singleCount 0 geomFailures 0 plLayers '())

  (foreach g groups
    (setq codeRecs (cdr g))
    (setq code (car g))

    (if (< (length codeRecs) 2)
      (progn
        (setq singleCount (1+ singleCount))
        (princ
          (strcat "\nGeometry: " code " | Points: 1 | SKIPPED (single point)")
        )
      )
      (progn
        ;; Layer prefix depends on the selected output type:
        ;;   Polyline   -> pl_<Code>
        ;;   3Dpolyline -> 3dpl_<Code>
        (setq plLayer
          (strcat
            (if (= outType "3Dpolyline") "3dpl_" "pl_")
            (DGPS-SanitizeLayerName code)
          )
        )
        (if (not (tblsearch "LAYER" plLayer))
          (DGPS-EnsureLayer plLayer 3)
        )
        (if (null (member plLayer plLayers))
          (setq plLayers (cons plLayer plLayers))
        )

        (setq sortedRecs (vl-sort codeRecs 'DGPS-TimeLess))
        (setq pts '())

        (foreach rec sortedRecs
          (setq x (atof (DGPS-R-East rec)))
          (setq y (atof (DGPS-R-North rec)))
          (setq z (atof (DGPS-R-Elev rec)))
          (setq pts
            (cons
              (if (= outType "3Dpolyline")
                (list x y z)
                (list x y)
              )
              pts
            )
          )
        )
        (setq pts (reverse pts))

        (setq result
          (if (= outType "3Dpolyline")
            (DGPS-Make3DPolyline pts plLayer)
            (DGPS-Make2DPolyline pts plLayer)
          )
        )

        (if result
          (progn
            (setq geomCount (1+ geomCount))
            (princ
              (strcat "\nGeometry: " code
                      " | Points: " (itoa (length sortedRecs))
                      " | " outType " | CREATED")
            )
          )
          (progn
            (setq geomFailures (1+ geomFailures))
            (princ
              (strcat "\nGeometry: " code
                      " | Points: " (itoa (length sortedRecs))
                      " | " outType " | FAILED")
            )
          )
        )
      )
    )
  )

  ;; -------------------------------------------------------------------------
  ;; Final report
  ;; -------------------------------------------------------------------------
  (princ "\n\n========================================")
  (princ "\nVIDDGPSTOLINE COMPLETE")
  (princ "\n========================================")
  (princ (strcat "\nRows read:              " (itoa dataRows)))
  (princ (strcat "\nValid records:          " (itoa validRows)))
  (princ (strcat "\nSkipped rows:           " (itoa skippedRows)))
  (princ (strcat "\nUnique Codes:           " (itoa (length groups))))
  (princ (strcat "\nGeometry layers:        " (itoa (length plLayers))))
  (princ (strcat "\nGeometry created:       " (itoa geomCount)))
  (princ (strcat "\nGeometry failures:      " (itoa geomFailures)))
  (princ (strcat "\nSingle-point Codes:     " (itoa singleCount)))

  (if (> geomFailures 0)
    (princ "\nWARNING: One or more geometry entities failed to create.")
  )

  (princ "\n========================================")

  ;; Restore AutoCAD state
  (setvar "CLAYER" dgps-*old-clayer*)
  (setvar "CMDECHO" dgps-*old-cmdecho*)
  (setvar "OSMODE" dgps-*old-osmode*)
  (setq *error* dgps-*old-error*)
  (princ "\nVIDDGPSTOLINE finished successfully.")
  (princ)
)

(princ "\nVIDDGPSTOLINE_v14 loaded. Type VIDDGPSTOLINE to run.")
(princ)