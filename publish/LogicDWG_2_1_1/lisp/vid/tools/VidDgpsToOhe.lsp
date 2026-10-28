;;; ============================================================================
;;; Vid Dgps To Ohe
;;; Release: 1.1.5 | Civil 3D 2026
;;; Version: 05
;;;   v05: CSV reading rebuilt with the reader used in vidDgpsToLine.lsp:
;;;        * file is read in BINARY mode (char by char) - a stray Ctrl-Z (0x1A),
;;;          NUL byte or odd character no longer stops the read half-way.
;;;        * UTF-8 / UTF-16 byte-order-mark is stripped (first header cell was
;;;          unreadable before, so the Code / Northing / Easting header failed).
;;;        * LF, CRLF and old CR-only line endings are all accepted.
;;;        * quoted fields that contain commas, doubled quotes ("") or line
;;;          breaks are handled (one logical record at a time).
;;;        * delimiter auto-detected from the header: comma, semicolon or tab.
;;;        * header may be on any line (title / blank lines above it are
;;;          ignored); alternative names accepted: Code / Point Code / Feature
;;;          Code, Northing / North / N / Y, Easting / East / E / X.
;;;        * numbers are validated strictly (no more silent 0.0 from text);
;;;          "1234,56" is accepted as 1234.56 when the delimiter is not comma.
;;;        * read summary printed: data rows, OH/OM/OHE/PORTAL rows, valid,
;;;          invalid (first few listed with their record number).
;;;        * file handle is always closed, even on error / Esc.
;;;   v04: rectangle V3/V4 always point AWAY from the nearest track
;;;        (LHS point -> box on the left, RHS point -> box on the right).
;;;        Nearest track is chosen by 2D distance among all track polylines.
;;; ============================================================================

(vl-load-com)

(setq *DGO-RECT-SIZE* 0.300)
(setq *DGO-TEXT-HEIGHT* 1.500)
(setq *DGO-TRACK-LAYER* "1-track")
(setq *DGO-RECT-LAYER* "1-OHE-CSV")
(setq *DGO-TEXT-LAYER* "1-OHE-CSV")

;; CSV reader state
(setq *DGO-FH* nil)         ; open file handle (closed by the error handler)
(setq *DGO-PENDING* nil)    ; physical lines read ahead but not yet used
(setq *DGO-PUSHBACK* nil)   ; one byte read ahead (CR followed by non-LF)

;;; ------------------------------------------------
;;; String helpers
;;; ------------------------------------------------

(defun DGO-Trim (s)
  (if s
    (vl-string-trim " \t\r\n\"" s)
    ""
  )
)

(defun DGO-Lower (s)
  (if s (strcase s T) "")
)

(defun DGO-RemoveBadChars (s / i c n out)
  ;; Keep normal printable characters and TAB (tab can be the CSV delimiter).
  ;; Remove other control/unreadable characters.
  (if (null s) (setq s ""))
  (setq i 1
        n (strlen s)
        out "")
  (while (<= i n)
    (setq c (ascii (substr s i 1)))
    (if (and c (or (>= c 32) (= c 9)) (/= c 127))
      (setq out (strcat out (substr s i 1)))
    )
    (setq i (1+ i))
  )
  out
)

(defun DGO-CleanField (s)
  (DGO-Trim (DGO-RemoveBadChars s))
)

;;; ------------------------------------------------
;;; CSV splitter
;;; Handles quoted fields, delimiters inside quotes and doubled quotes.
;;; delim = "," | ";" | "\t"   (nil = comma)
;;; ------------------------------------------------

(defun DGO-SplitCSV (s delim / i n c field result quoted)
  (if (or (null delim) (= delim "")) (setq delim ","))
  (setq s (DGO-RemoveBadChars s))
  (setq i 1
        n (strlen s)
        field ""
        result '()
        quoted nil)

  (while (<= i n)
    (setq c (substr s i 1))

    (cond
      ((= c "\"")
       (if (and quoted
                (< i n)
                (= (substr s (1+ i) 1) "\""))
         (progn
           (setq field (strcat field "\""))
           (setq i (+ i 2))
         )
         (progn
           (setq quoted (not quoted))
           (setq i (1+ i))
         )
       )
      )

      ((and (= c delim) (not quoted))
       (setq result (append result (list (DGO-CleanField field))))
       (setq field "")
       (setq i (1+ i))
      )

      (T
       (setq field (strcat field c))
       (setq i (1+ i))
      )
    )
  )

  (append result (list (DGO-CleanField field)))
)

;;; ------------------------------------------------
;;; Header matching
;;; ------------------------------------------------

(defun DGO-NormalHeader (s)
  (setq s (DGO-Lower (DGO-CleanField s)))
  ;; remove spaces, underscore and hyphen for flexible matching
  (while (vl-string-search " " s)
    (setq s (vl-string-subst "" " " s))
  )
  (while (vl-string-search "_" s)
    (setq s (vl-string-subst "" "_" s))
  )
  (while (vl-string-search "-" s)
    (setq s (vl-string-subst "" "-" s))
  )
  s
)

(defun DGO-HeaderIndex (fields wanted / w f i result)
  ;; wanted = one normalised name ("code") or a list of alternatives in
  ;; priority order ("code" "pointcode").  Returns the column index or nil.
  (if (= (type wanted) 'STR) (setq wanted (list wanted)))
  (setq result nil)
  (foreach w wanted
    (if (null result)
      (progn
        (setq i 0)
        (foreach f fields
          (if (and (null result) (= (DGO-NormalHeader f) w))
            (setq result i)
          )
          (setq i (1+ i))
        )
      )
    )
  )
  result
)

(defun DGO-NthSafe (lst idx)
  (if (and idx (>= idx 0) (< idx (length lst)))
    (nth idx lst)
    nil
  )
)

;;; ------------------------------------------------
;;; Numeric validation
;;; ------------------------------------------------

(defun DGO-NumericP (s / n i ch seenDot seenDigit seenExp ok)
  ;; Strict check: [+-]digits[.digits][e[+-]digits]   (".5" and "5." accepted)
  (setq s (DGO-CleanField s)
        n (strlen s))
  (if (= n 0)
    nil
    (progn
      (setq i 1 ok T seenDot nil seenDigit nil seenExp nil)
      (if (member (substr s 1 1) '("+" "-")) (setq i 2))
      (while (and ok (<= i n))
        (setq ch (substr s i 1))
        (cond
          ((and (>= (ascii ch) 48) (<= (ascii ch) 57))
           (setq seenDigit T))
          ((and (= ch ".") (not seenDot) (not seenExp))
           (setq seenDot T))
          ((and (member ch '("e" "E")) seenDigit (not seenExp) (< i n))
           (setq seenExp T)
           (if (member (substr s (1+ i) 1) '("+" "-"))
             (progn
               (setq i (1+ i))
               (if (= i n) (setq ok nil))
             )
           )
          )
          (T (setq ok nil))
        )
        (setq i (1+ i))
      )
      (and ok seenDigit)
    )
  )
)

(defun DGO-Number (s / p)
  (setq s (DGO-CleanField s))
  ;; decimal comma ("1234,56") -> decimal point
  (if (and (/= s "") (null (vl-string-search "." s)))
    (if (setq p (vl-string-search "," s))
      (if (null (vl-string-search "," s (1+ p)))
        (setq s (vl-string-subst "." "," s))
      )
    )
  )
  (if (DGO-NumericP s)
    (atof s)
    nil
  )
)

;;; ------------------------------------------------
;;; Code filtering / identifier cleaning
;;; ------------------------------------------------

(defun DGO-GetPrefix (code / s u)
  (setq s (DGO-Trim code))
  (setq u (DGO-Lower s))
  (cond
    ((wcmatch u "ohe*") "OHE")
    ((wcmatch u "portal*") "PORTAL")
    ((wcmatch u "om*") "OM")
    ((wcmatch u "oh*") "OH")
    (T nil)
  )
)

(defun DGO-RemovePrefix (code prefix / s)
  (setq s (DGO-Trim code))
  (setq s (substr s (1+ (strlen prefix))))
  (DGO-Trim s)
)

(defun DGO-NormalizeIdentifier (code / p s)
  (setq code (DGO-RemoveBadChars code))
  (setq p (DGO-GetPrefix code))

  (if p
    (progn
      (setq s (DGO-RemovePrefix code p))

      ;; Remove repeated spaces
      (while (vl-string-search "  " s)
        (setq s (vl-string-subst " " "  " s))
      )

      ;; Normalize common separators to /
      (setq s (vl-string-subst "/" "-" s))
      (setq s (vl-string-subst "/" "." s))

      ;; If identifier contains a space between two parts,
      ;; convert the separating space to /
      ;; Example: 40 22A -> 40/22A
      (if (and (vl-string-search " " s)
               (not (vl-string-search "/" s)))
        (setq s (vl-string-subst "/" " " s))
      )

      ;; Clean spaces around /
      (while (vl-string-search " /" s)
        (setq s (vl-string-subst "/" " /" s))
      )
      (while (vl-string-search "/ " s)
        (setq s (vl-string-subst "/" "/ " s))
      )

      (DGO-Trim s)
    )
    nil
  )
)

;;; ------------------------------------------------
;;; Layers
;;; ------------------------------------------------

(defun DGO-MakeLayer (name / doc layers obj)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))
  (if (tblsearch "LAYER" name)
    (setq obj (vla-Item layers name))
    (setq obj (vla-Add layers name))
  )
  name
)

;;; ------------------------------------------------
;;; Track utilities
;;; ------------------------------------------------

(defun DGO-TrackObjects (/ ss i e result)
  (setq result '())
  (if (setq ss (ssget "_X"
                     (list
                       '(0 . "LWPOLYLINE,POLYLINE")
                       (cons 8 *DGO-TRACK-LAYER*)
                     )))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (setq result (cons e result))
        (setq i (1+ i))
      )
    )
  )
  (reverse result)
)

(defun DGO-ClosestPoint (e p)
  (vl-catch-all-apply
    'vlax-curve-getClosestPointTo
    (list e p)
  )
)

(defun DGO-SafeDeriv (e param / r)
  (setq r
    (vl-catch-all-apply
      'vlax-curve-getFirstDeriv
      (list e param)
    )
  )
  (if (vl-catch-all-error-p r)
    nil
    (if (and (= (type r) 'LIST)
             (= (length r) 3)
             (numberp (car r))
             (numberp (cadr r)))
      r
      nil
    )
  )
)

(defun DGO-Unit2D (v / x y d)
  (if (and v (= (length v) 3))
    (progn
      (setq x (car v)
            y (cadr v)
            d (sqrt (+ (* x x) (* y y))))
      (if (> d 1e-12)
        (list (/ x d) (/ y d) 0.0)
        nil
      )
    )
  )
)

(defun DGO-TrackDirection (e cp / param der p1 p2 d)
  ;; First try exact tangent.
  (setq param
    (vl-catch-all-apply
      'vlax-curve-getParamAtPoint
      (list e cp)
    )
  )

  (if (not (vl-catch-all-error-p param))
    (progn
      (setq der (DGO-SafeDeriv e param))
      (if (setq d (DGO-Unit2D der))
        d
        nil
      )
    )
  )
)

(defun DGO-BestTrack (p tracks / best bestd e cp r d)
  (setq best nil
        bestd nil)

  (foreach e tracks
    (setq r (DGO-ClosestPoint e p))
    (if (not (vl-catch-all-error-p r))
      (progn
        (setq cp r)
        (setq d (distance (list (car p) (cadr p) 0.0) (list (car cp) (cadr cp) 0.0)))
        (if (or (null bestd) (< d bestd))
          (progn
            (setq best e)
            (setq bestd d)
          )
        )
      )
    )
  )

  (if best
    (list best bestd)
    nil
  )
)

;;; ------------------------------------------------
;;; Geometry
;;; ------------------------------------------------

(defun DGO-VecAdd (a b)
  (list (+ (car a) (car b))
        (+ (cadr a) (cadr b))
        (+ (caddr a) (caddr b)))
)

(defun DGO-VecSub (a b)
  (list (- (car a) (car b))
        (- (cadr a) (cadr b))
        (- (caddr a) (caddr b)))
)

(defun DGO-VecScale (v s)
  (list (* (car v) s)
        (* (cadr v) s)
        (* (caddr v) s))
)

(defun DGO-Rot90 (v)
  (list (- (cadr v))
        (car v)
        0.0)
)

(defun DGO-MakeRectangle (p tangent sideSign / half side v1 v2 v3 v4)
  ;; DGPS point = midpoint V1-V2
  ;;
  ;; V1 ---- V2
  ;; |        |
  ;; |        |
  ;; V4 ---- V3
  ;;
  ;; V1->V2 follows track direction.
  ;; V2->V3 is perpendicular and points AWAY from the track
  ;; (sideSign = +1 -> left of track direction, -1 -> right).

  (setq half (/ *DGO-RECT-SIZE* 2.0))
  (setq side (DGO-VecScale (DGO-Rot90 tangent) sideSign))

  (setq v1 (DGO-VecSub p (DGO-VecScale tangent half)))
  (setq v2 (DGO-VecAdd p (DGO-VecScale tangent half)))
  (setq v3 (DGO-VecAdd v2 (DGO-VecScale side *DGO-RECT-SIZE*)))
  (setq v4 (DGO-VecAdd v1 (DGO-VecScale side *DGO-RECT-SIZE*)))

  (list v1 v2 v3 v4)
)

(defun DGO-Mid (a b)
  (list (/ (+ (car a) (car b)) 2.0)
        (/ (+ (cadr a) (cadr b)) 2.0)
        (/ (+ (caddr a) (caddr b)) 2.0))
)

(defun DGO-AddLWPoly (pts layer / data)
  (setq data
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbPolyline")
      (cons 90 4)
      '(70 . 1)
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
  (entmakex data)
)

(defun DGO-AddMText (pt txt rotation layer / data)
  (setq data
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbMText")
      (cons 10 pt)
      (cons 40 *DGO-TEXT-HEIGHT*)
      (cons 41 0.0)
      (cons 71 4) ; Middle Left
      (cons 50 rotation)
      (cons 1 txt)
      (cons 7 (getvar "TEXTSTYLE"))
    )
  )
  (entmakex data)
)

;;; ------------------------------------------------
;;; CSV low-level reader  (binary mode, same method as vidDgpsToLine.lsp)
;;; ------------------------------------------------

;; Next byte of the file.  NUL bytes are skipped, which also lets UTF-16 text
;; (Excel "Unicode Text") read correctly for ordinary ASCII content.
(defun DGO-ReadByte (fh / b)
  (if *DGO-PUSHBACK*
    (setq b *DGO-PUSHBACK*
          *DGO-PUSHBACK* nil)
    (progn
      (setq b (read-char fh))
      (while (and b (= b 0))
        (setq b (read-char fh))
      )
    )
  )
  b
)

;; One physical line.  Safe against Ctrl-Z (0x1A); accepts LF, CRLF, CR.
(defun DGO-ReadPhysicalLine (fh / b s gotLine)
  (if *DGO-PENDING*
    (progn
      (setq s *DGO-PENDING*)
      (setq *DGO-PENDING* (cdr *DGO-PENDING*))
      (car s)
    )
    (progn
      (setq s "" gotLine nil)
      (while (and (not gotLine) (setq b (DGO-ReadByte fh)))
        (cond
          ((= b 10) (setq gotLine T))
          ((= b 13)
           (setq b (DGO-ReadByte fh))
           (if (and b (/= b 10)) (setq *DGO-PUSHBACK* b))
           (setq gotLine T)
          )
          (T (setq s (strcat s (chr b))))
        )
      )
      (if (or gotLine (> (strlen s) 0)) s nil)
    )
  )
)

(defun DGO-QuoteCount (s / i n count)
  (setq i 1 n (strlen s) count 0)
  (while (<= i n)
    (if (= (substr s i 1) "\"") (setq count (1+ count)))
    (setq i (1+ i))
  )
  count
)

;; One logical CSV record: a quoted field may continue on the next line(s).
;; A stray, never-closed quote cannot swallow the file: after 8 extra lines the
;; record is cut back to its first line and the extra lines are given back.
(defun DGO-ReadRecord (fh / ln1 s extras next k)
  (setq ln1 (DGO-ReadPhysicalLine fh))
  (if (null ln1)
    nil
    (progn
      (setq s ln1 extras '() k 0)
      (while (and (= (rem (DGO-QuoteCount s) 2) 1)
                  (< k 8)
                  (setq next (DGO-ReadPhysicalLine fh)))
        (setq extras (append extras (list next)))
        (setq s (strcat s "\n" next))
        (setq k (1+ k))
      )
      (if (= (rem (DGO-QuoteCount s) 2) 1)
        (progn
          ;; quote never closed - treat as a single-line record
          (setq *DGO-PENDING* (append extras *DGO-PENDING*))
          (setq s ln1)
        )
      )
      s
    )
  )
)

;; Remove UTF-8 (EF BB BF) / UTF-16 (FF FE, FE FF) byte-order mark.
(defun DGO-StripBOM (s / a b c)
  (if (and s (>= (strlen s) 2))
    (progn
      (setq a (ascii (substr s 1 1))
            b (ascii (substr s 2 1)))
      (cond
        ((and (>= (strlen s) 3)
              (= a 239) (= b 187)
              (= (ascii (substr s 3 1)) 191))
         (substr s 4))
        ((or (and (= a 255) (= b 254))
             (and (= a 254) (= b 255)))
         (substr s 3))
        (T s)
      )
    )
    s
  )
)

;; Looks for the Code / Northing / Easting header in one record, trying
;; comma, semicolon and tab.  Returns (delimiter codeIdx northIdx eastIdx) or nil.
(defun DGO-FindHeader (line / d f ci ni ei result)
  (setq result nil)
  (foreach d '("," ";" "\t")
    (if (null result)
      (progn
        (setq f  (DGO-SplitCSV line d)
              ci (DGO-HeaderIndex f '("code" "pointcode" "featurecode"))
              ni (DGO-HeaderIndex f '("northing" "north" "n" "y"))
              ei (DGO-HeaderIndex f '("easting" "east" "e" "x")))
        (if (and ci ni ei (/= ci ni) (/= ci ei) (/= ni ei))
          (setq result (list d ci ni ei))
        )
      )
    )
  )
  result
)

(defun DGO-DelimName (d)
  (cond
    ((= d ",") "comma")
    ((= d ";") "semicolon")
    (T "tab")
  )
)

;;; ------------------------------------------------
;;; CSV file reader
;;; returns list of (prefix identifier Easting Northing) or nil
;;; ------------------------------------------------

(defun DGO-ReadCSV (/ path fh rec hdr delim ci ni ei fields code north east
                      prefix ident rows recNo dataRows ohRows badCount
                      badList firstLine)
  (setq rows '()
        hdr nil
        delim ","
        recNo 0
        dataRows 0
        ohRows 0
        badCount 0
        badList '()
        firstLine nil)

  (setq path
    (if (boundp 'LogicDWG:RequireCsv)
      (LogicDWG:RequireCsv)                          ; main CSV (new logicDwg.lsp)
      (getfiled "Select DGPS CSV file" "" "csv" 0)   ; fallback (old logicDwg.lsp)
    )
  )

  (if (null path)
    nil
    (progn
      (princ (strcat "\nReading: " path))

      ;; binary mode: not stopped by Ctrl-Z / odd characters
      (setq fh (open path "rb"))

      (if (null fh)
        (progn
          (princ "\nCould not open the CSV file.")
          nil
        )
        (progn
          (setq *DGO-FH* fh
                *DGO-PENDING* nil
                *DGO-PUSHBACK* nil)

          (while (setq rec (DGO-ReadRecord fh))
            (setq recNo (1+ recNo))
            (if (= recNo 1) (setq rec (DGO-StripBOM rec)))

            (cond
              ;; blank record
              ((= (DGO-Trim (DGO-RemoveBadChars rec)) "")
               nil)

              ;; still looking for the header (title lines above it are ignored)
              ((null hdr)
               (if (null firstLine) (setq firstLine rec))
               (if (setq hdr (DGO-FindHeader rec))
                 (progn
                   (setq delim (nth 0 hdr)
                         ci    (nth 1 hdr)
                         ni    (nth 2 hdr)
                         ei    (nth 3 hdr))
                   (princ
                     (strcat
                       "\nHeader detected (record " (itoa recNo)
                       ", " (DGO-DelimName delim) "-separated): Code col "
                       (itoa (1+ ci))
                       ", Northing col "
                       (itoa (1+ ni))
                       ", Easting col "
                       (itoa (1+ ei))
                     )
                   )
                 )
               )
              )

              ;; data record
              (T
               (setq dataRows (1+ dataRows))
               (setq fields (DGO-SplitCSV rec delim))
               (setq code   (DGO-NthSafe fields ci))
               (setq prefix (if (and code (/= code "")) (DGO-GetPrefix code) nil))

               ;; Only OH / OM / OHE / PORTAL rows.
               (if prefix
                 (progn
                   (setq ohRows (1+ ohRows))
                   (setq ident (DGO-NormalizeIdentifier code))
                   (setq north (DGO-Number (DGO-NthSafe fields ni)))
                   (setq east  (DGO-Number (DGO-NthSafe fields ei)))

                   (if (and ident north east)
                     (setq rows
                       (cons (list prefix ident east north) rows))
                     (progn
                       (setq badCount (1+ badCount))
                       (if (< (length badList) 5)
                         (setq badList
                           (append badList
                             (list (strcat "record " (itoa recNo) ": " code)))))
                     )
                   )
                 )
               )
              )
            )
          )

          (close fh)
          (setq *DGO-FH* nil)

          (cond
            ;; header never found
            ((null hdr)
             (princ "\nHeader not found: the CSV needs columns Code, Northing, Easting.")
             (if firstLine
               (princ (strcat "\nFirst line read: " (DGO-RemoveBadChars firstLine)))
             )
             nil
            )

            (T
             (setq rows (reverse rows))

             (princ (strcat "\nData rows read: " (itoa dataRows)))
             (princ
               (strcat
                 "\nValid OH/OM/OHE/PORTAL rows: "
                 (itoa (length rows))
               )
             )

             (if (> badCount 0)
               (progn
                 (princ
                   (strcat
                     "\nInvalid OHE rows skipped (bad Northing/Easting/identifier): "
                     (itoa badCount)
                   )
                 )
                 (foreach code badList
                   (princ (strcat "\n  " code))
                 )
               )
             )

             rows
            )
          )
        )
      )
    )
  )
)

;;; ------------------------------------------------
;;; Main command
;;; ------------------------------------------------

(defun c:vidDgpsToOhe (/ *error* oldcmdecho oldosmode rows tracks
                       mode ss i e item p best cp tangent rect
                       v1 v2 v3 v4 textpt ident textstr ent1 ent2
                       madeRect madeText failCount tooFar sideSign trackAng)

  (vl-load-com)

  (setq oldcmdecho (getvar "CMDECHO"))
  (setq oldosmode  (getvar "OSMODE"))

  (defun *error* (msg)
    (if *DGO-FH*
      (progn
        (vl-catch-all-apply 'close (list *DGO-FH*))
        (setq *DGO-FH* nil)
      )
    )
    (setvar "CMDECHO" oldcmdecho)
    (setvar "OSMODE" oldosmode)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort"))
      (princ (strcat "\nDgpsToOhe stopped: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)

  (princ "\n=== DgpsToOhe : DGPS CSV -> OHE rectangles + stacked text ===")

  (setq rows (DGO-ReadCSV))

  (if (null rows)
    (progn
      (princ "\nNo valid OH/OM/OHE/PORTAL rows found.")
      (*error* nil)
    )
    (progn

      (DGO-MakeLayer *DGO-RECT-LAYER*)
      (DGO-MakeLayer *DGO-TEXT-LAYER*)

      (initget "Auto Select")
      (setq mode
        (getkword
          "\nTrack polylines [Auto/Select] <Auto>: "
        )
      )

      (if (or (null mode) (= mode "Auto"))
        (setq tracks (DGO-TrackObjects))
        (progn
          (princ "\nSelect track polylines on layer 1-track: ")
          (setq ss
            (ssget
              '((0 . "LWPOLYLINE,POLYLINE"))
            )
          )
          (setq tracks '())
          (if ss
            (progn
              (setq i 0)
              (while (< i (sslength ss))
                (setq e (ssname ss i))
                (if (= (strcase (cdr (assoc 8 (entget e))))
                       (strcase *DGO-TRACK-LAYER*))
                  (setq tracks (cons e tracks))
                )
                (setq i (1+ i))
              )
            )
          )
          (setq tracks (reverse tracks))
        )
      )

      (if (null tracks)
        (progn
          (princ
            (strcat
              "\nNo track polylines found on layer "
              *DGO-TRACK-LAYER*
              "."
            )
          )
        )
        (progn

          (princ
            (strcat
              "\nUsing "
              (itoa (length tracks))
              " track polyline(s). Processing..."
            )
          )

          (setq madeRect 0
                madeText 0
                failCount 0
                tooFar 0)

          (foreach item rows

            ;; item = (prefix identifier Easting Northing)
            (setq ident (cadr item))
            (setq p
              (list
                (nth 2 item) ; Easting = X
                (nth 3 item) ; Northing = Y
                0.0
              )
            )

            (setq best (DGO-BestTrack p tracks))

            (if best
              (progn
                (setq e (car best))
                (setq cp
                  (DGO-ClosestPoint e p)
                )

                (setq tangent
                  (DGO-TrackDirection e cp)
                )

                ;; If tangent unavailable, use a safe default.
                (if (null tangent)
                  (setq tangent (list 1.0 0.0 0.0))
                )

                ;; Which side of the track is the point on?
                ;; cross(tangent, cp->p) > 0 = LHS (left), < 0 = RHS (right).
                ;; Box always extends away from the track.
                (setq sideSign
                  (if (< (- (* (car tangent)  (- (cadr p) (cadr cp)))
                            (* (cadr tangent) (- (car p)  (car cp))))
                         -1e-9)
                    -1.0
                    1.0))

                (setq rect (DGO-MakeRectangle p tangent sideSign))

                (setq v1 (nth 0 rect))
                (setq v2 (nth 1 rect))
                (setq v3 (nth 2 rect))
                (setq v4 (nth 3 rect))

                ;; Create 0.3m square.
                (setq ent1
                  (DGO-AddLWPoly
                    (list v1 v2 v3 v4)
                    *DGO-RECT-LAYER*
                  )
                )

                (if ent1
                  (setq madeRect (1+ madeRect))
                  (setq failCount (1+ failCount))
                )

                ;; Text = midpoint(V2,V3)
                (setq textpt (DGO-Mid v2 v3))

                ;; MText stacked format.
                (setq textstr
                  (strcat "\\A1;\\S" ident ";")
                )

                ;; Track angle.
                (setq trackAng
                  (atan (cadr tangent) (car tangent))
                )

                ;; Keep text readable.
                (if (and (> trackAng (/ pi 2.0))
                         (< trackAng (* 1.5 pi)))
                  (progn
                    (setq trackAng (+ trackAng pi))
                    ;; Text now runs the opposite way, so anchor it on the
                    ;; V1-V4 edge (keeps it outside the rectangle).
                    (setq textpt (DGO-Mid v1 v4))
                  )
                )

                (setq ent2
                  (DGO-AddMText
                    textpt
                    textstr
                    trackAng
                    *DGO-TEXT-LAYER*
                  )
                )

                (if ent2
                  (setq madeText (1+ madeText))
                  (setq failCount (1+ failCount))
                )
              )
              (setq failCount (1+ failCount))
            )
          )

          (princ "\n")
          (princ "\n=== DgpsToOhe completed ===")
          (princ (strcat "\nValid OHE rows:      " (itoa (length rows))))
          (princ (strcat "\nRectangles created:  " (itoa madeRect)))
          (princ (strcat "\nTexts created:       " (itoa madeText)))
          (princ (strcat "\nFailed/skipped:      " (itoa failCount)))
        )
      )

      (setvar "CMDECHO" oldcmdecho)
      (setvar "OSMODE" oldosmode)
      (princ)
    )
  )
)

(princ "\nDgpsToOhe.lsp v05 loaded. Type vidDgpsToOhe to run.")
(princ)
