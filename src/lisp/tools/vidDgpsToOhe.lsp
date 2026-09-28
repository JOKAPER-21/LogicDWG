;;; ============================================================================
;;; Vid Dgps To Ohe
;;; Release: 1.1.2 | Civil 3D 2026
;;; Version: 02
;;; ============================================================================

(vl-load-com)

(setq *DGO-RECT-SIZE* 0.300)
(setq *DGO-TEXT-HEIGHT* 1.500)
(setq *DGO-TRACK-LAYER* "1-track")
(setq *DGO-RECT-LAYER* "DGPS_OHE_RECTANGLE")
(setq *DGO-TEXT-LAYER* "DGPS_OHE_TEXT")

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
  ;; Keep normal printable ASCII characters.
  ;; Remove control/unreadable characters.
  (if (null s) (setq s ""))
  (setq i 1
        n (strlen s)
        out "")
  (while (<= i n)
    (setq c (ascii (substr s i 1)))
    (if (and c (>= c 32) (/= c 127))
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
;;; Handles quoted fields and commas inside quotes.
;;; ------------------------------------------------

(defun DGO-SplitCSV (s / i n c field result quoted)
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

      ((and (= c ",") (not quoted))
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

(defun DGO-HeaderIndex (fields wanted / i result f)
  (setq i 0 result nil)
  (foreach f fields
    (if (= (DGO-NormalHeader f) wanted)
      (setq result i)
    )
    (setq i (1+ i))
  )
  result
)

(defun DGO-NthSafe (lst idx)
  (if (and (>= idx 0) (< idx (length lst)))
    (nth idx lst)
    nil
  )
)

;;; ------------------------------------------------
;;; Numeric validation
;;; ------------------------------------------------

(defun DGO-Number (s / v)
  (setq s (DGO-CleanField s))
  (if (= s "")
    nil
    (progn
      (setq v (distof s 2))
      (if (numberp v) v nil)
    )
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
        (setq d (distance p cp))
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

(defun DGO-MakeRectangle (p tangent / half side v1 v2 v3 v4)
  ;; DGPS point = midpoint V1-V2
  ;;
  ;; V1 ---- V2
  ;; |        |
  ;; |        |
  ;; V4 ---- V3
  ;;
  ;; V1->V2 follows track direction.
  ;; V2->V3 is perpendicular.

  (setq half (/ *DGO-RECT-SIZE* 2.0))
  (setq side (DGO-Rot90 tangent))

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
      (cons 71 5) ; Middle Center
      (cons 50 rotation)
      (cons 1 txt)
      (cons 7 (getvar "TEXTSTYLE"))
    )
  )
  (entmakex data)
)

;;; ------------------------------------------------
;;; CSV file reader
;;; ------------------------------------------------

(defun DGO-ReadCSV (/ path fh line fields codei northi easti
                       rows headerFound badCount row fields2 code
                       north east ident prefix)
  (setq rows '()
        headerFound nil
        badCount 0)

  (setq path (getfiled "Select DGPS CSV file" "" "csv" 0))

  (if (null path)
    nil
    (progn
      (princ (strcat "\nReading: " path))

      (setq fh (open path "r"))

      (if (null fh)
        (progn
          (princ "\nCould not open the CSV file.")
          nil
        )
        (progn
          (while (setq line (read-line fh))

            ;; Remove control/unreadable characters.
            ;; IMPORTANT: do not stop reading because of them.
            (setq line (DGO-RemoveBadChars line))

            (if (and (not headerFound) (/= (DGO-Trim line) ""))
              (progn
                (setq fields (DGO-SplitCSV line))

                (setq codei   (DGO-HeaderIndex fields "code"))
                (setq northi  (DGO-HeaderIndex fields "northing"))
                (setq easti   (DGO-HeaderIndex fields "easting"))

                (if (and codei northi easti)
                  (progn
                    (setq headerFound T)
                    (princ
                      (strcat
                        "\nHeader detected: Code col "
                        (itoa (1+ codei))
                        ", Northing col "
                        (itoa (1+ northi))
                        ", Easting col "
                        (itoa (1+ easti))
                      )
                    )
                  )
                )
              )
            )

            (if headerFound
              (progn
                (setq fields2 (DGO-SplitCSV line))

                ;; Skip header itself.
                (if (or
                      (/= (DGO-NormalHeader (DGO-NthSafe fields2 codei)) "code")
                      (/= (DGO-NormalHeader (DGO-NthSafe fields2 northi)) "northing")
                      (/= (DGO-NormalHeader (DGO-NthSafe fields2 easti)) "easting")
                    )
                  (progn
                    (setq code  (DGO-NthSafe fields2 codei))
                    (setq north (DGO-Number (DGO-NthSafe fields2 northi)))
                    (setq east  (DGO-Number (DGO-NthSafe fields2 easti)))

                    (setq prefix (if code (DGO-GetPrefix code) nil))

                    ;; Only OH / OM / OHE / PORTAL rows.
                    (if prefix
                      (progn
                        (setq ident (DGO-NormalizeIdentifier code))

                        (if (and ident north east)
                          (setq rows
                            (cons
                              (list prefix ident east north)
                              rows
                            )
                          )
                          (setq badCount (1+ badCount))
                        )
                      )
                    )
                  )
                )
              )
            )
          )

          (close fh)

          (setq rows (reverse rows))

          (princ
            (strcat
              "\nValid OH/OM/OHE/PORTAL rows: "
              (itoa (length rows))
            )
          )

          (if (> badCount 0)
            (princ
              (strcat
                "\nInvalid OHE rows skipped: "
                (itoa badCount)
              )
            )
          )

          rows
        )
      )
    )
  )
)

;;; ------------------------------------------------
;;; Main command
;;; ------------------------------------------------

(defun c:DgpsToOhe (/ *error* oldcmdecho oldosmode rows tracks
                       mode ss i e item p best cp tangent rect
                       v1 v2 v3 v4 textpt ident textstr ent1 ent2
                       madeRect madeText failCount tooFar)

  (vl-load-com)

  (setq oldcmdecho (getvar "CMDECHO"))
  (setq oldosmode  (getvar "OSMODE"))

  (defun *error* (msg)
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

                (setq rect (DGO-MakeRectangle p tangent))

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
                  (setq trackAng (+ trackAng pi))
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

(princ "\nDgpsToOhe.lsp loaded. Type DgpsToOhe to run.")
(princ)
