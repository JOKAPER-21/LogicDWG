;;; ============================================================
;;; VIDINDEXCURVE - Railway Track Curve Index Plotter
;;; Command : VIDINDEXCURVE
;;;
;;; CSV header: segment_types, vid_ch, lhs_rhs, degree  (header row required)
;;;
;;; 1. Select the CSV
;;; 2. Enter the horizontal scale (m per 1000 m) in the dialog
;;; 3. Pick the insertion point (chainage 0 of the plot = picked point)
;;;
;;; Draws the track geometry line on the picked point's Y, with the curve
;;; labels 5 below it.
;;; ============================================================

(vl-load-com)

;; Layers
(setq VIDC:LYR-TRACK "1-VIC_Rail_Track"   ; track geometry polyline
      VIDC:LYR-TEXT  "1-VIC_Rail_Text"    ; curve labels (radius / degree)
)

;; Layer colours (AutoCAD colour index): magenta, cyan
(setq VIDC:CLR-TRACK 6   ; 1-VIC_Rail_Track = magenta
      VIDC:CLR-TEXT  4   ; 1-VIC_Rail_Text  = cyan
)

;; Last used dialog value: metres per 1000 m (defaults to the gradient tool's value)
(if (not *VIDC:h*)
  (setq *VIDC:h* (if (and *VID:vals* (car *VID:vals*)) (car *VID:vals*) "20"))
)

;;; ------------------------------------------------------------
;;; Main command
;;; ------------------------------------------------------------
(defun c:VIDINDEXCURVE (/ file rows hVal ip)
  (if (setq file (getfiled "Select Track Geometry CSV File" (getvar "DWGPREFIX") "csv" 16))
    (if (setq rows (VIDC:read-csv file))
      (if (setq hVal (VIDC:show-dialog))
        (if (setq ip (getpoint "\nPick insertion point: "))
          (VIDC:plot rows hVal ip)
          (princ "\nNo insertion point picked.")
        )
        (princ "\nCancelled.")
      )
      (princ "\nNo data rows found.")
    )
    (princ "\nNo file selected.")
  )
  (princ)
)

;;; ------------------------------------------------------------
;;; Dialog (DCL is written to a temp file, so only this .lsp is needed)
;;; ------------------------------------------------------------
(defun VIDC:write-dcl (/ path f)
  (setq path (vl-filename-mktemp "vidcurve" nil ".dcl"))
  (if (setq f (open path "w"))
    (progn
      (foreach ln
        (list
          "vidcurve : dialog {"
          "  label = \"VID Index Curve\";"
          "  : boxed_row {"
          "    label = \"Horizontal\";"
          "    : text { label = \"1 KM (1000 m)  =\"; width = 18; }"
          "    : edit_box { key = \"h_val\"; edit_width = 8; }"
          "    : text { label = \"m\"; width = 2; }"
          "  }"
          "  spacer;"
          "  ok_cancel_err;"
          "}"
        )
        (write-line ln f)
      )
      (close f)
      path
    )
  )
)

(defun VIDC:show-dialog (/ dclPath dclId status res)
  (setq VIDC:result nil)
  (setq dclPath (VIDC:write-dcl))
  (setq res
    (cond
      ((not dclPath)
       (princ "\nCannot create dialog file.")
       nil)
      ((< (setq dclId (load_dialog dclPath)) 0)
       (princ "\nCannot load dialog file.")
       nil)
      ((not (new_dialog "vidcurve" dclId))
       (unload_dialog dclId)
       (princ "\nCannot open dialog.")
       nil)
      (t
       (set_tile "h_val" *VIDC:h*)
       (action_tile "accept" "(VIDC:on-ok)")
       (setq status (start_dialog))
       (unload_dialog dclId)
       (if (= status 1) VIDC:result nil)
      )
    )
  )
  (if (and dclPath (findfile dclPath)) (vl-file-delete dclPath))
  res
)

;; Returns a real > 0, or nil for anything else (empty, text, 0, negative)
(defun VIDC:parse-pos (str / v)
  (setq v (distof (vl-string-trim " " str) 2))
  (if (and v (> v 0.0)) v nil)
)

(defun VIDC:on-ok (/ hv)
  (setq hv (VIDC:parse-pos (get_tile "h_val")))
  (if (not hv)
    (set_tile "error" "Horizontal value must be a number greater than 0.")
    (progn
      (setq *VIDC:h* (get_tile "h_val"))
      (setq VIDC:result hv)
      (done_dialog 1)
    )
  )
)

;;; ------------------------------------------------------------
;;; Scaling:  "1000 m = <val> m"  ->  drawn value = original * val / 1000
;;; ------------------------------------------------------------
(defun VIDC:scale (v val)
  (* v (/ val 1000.0))
)

;;; ------------------------------------------------------------
;;; CSV
;;; ------------------------------------------------------------
(defun VIDC:split (str / result pos)
  (setq result '())
  (while (setq pos (vl-string-search "," str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (append result (list str))
)

;; Index of the header cell containing <name> (case-insensitive), else <default>
(defun VIDC:find-col (hdrParts name default / i found)
  (setq i 0 found nil)
  (while (and (not found) (< i (length hdrParts)))
    (if (vl-string-search name (strcase (nth i hdrParts) T))
      (setq found i)
    )
    (setq i (1+ i))
  )
  (if found found default)
)

;; Trimmed text of cell <i>, or "" when the row is too short
(defun VIDC:cell (parts i)
  (if (and i (< i (length parts)))
    (vl-string-trim " \t\r\n" (nth i parts))
    ""
  )
)

;; Columns are found by header name (segment_types, vid_ch, lhs_rhs, degree);
;; if a name is missing, column order 1,2,3,4 is used.
;; Returns a list of (type km dir degree) rows, or nil if there are none.
(defun VIDC:read-csv (file / f line hdr cT cK cD cG rows parts dir)
  (setq rows '())
  (if (setq f (open file "r"))
    (progn
      (setq hdr (VIDC:split (if (setq line (read-line f)) line "")))
      (setq cT (VIDC:find-col hdr "segment_types" 0)
            cK (VIDC:find-col hdr "vid_ch" 1)
            cD (VIDC:find-col hdr "lhs_rhs" 2)
            cG (VIDC:find-col hdr "degree" 3))
      (while (setq line (read-line f))
        (setq line (vl-string-trim " \t\r\n" line))
        (if (> (strlen line) 0)
          (progn
            (setq parts (VIDC:split line))
            (if (> (length parts) (max cT cK))
              (progn
                (setq dir (VIDC:cell parts cD))
                (setq rows
                  (append rows
                    (list
                      (list
                        (strcase (VIDC:cell parts cT))
                        (atof (VIDC:cell parts cK))
                        (if (= dir "") "-" (strcase dir))
                        (if (= (VIDC:cell parts cG) "")
                          nil
                          (atof (VIDC:cell parts cG)))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      (close f)
    )
    (princ "\nCannot open CSV file.")
  )
  rows
)

;;; ------------------------------------------------------------
;;; Geometry
;;; ------------------------------------------------------------
(defun VIDC:find-curve (rows kmlo kmhi / r typ km dir deg found)
  (setq found nil)
  (foreach r rows
    (setq typ (nth 0 r) km (nth 1 r) dir (nth 2 r) deg (nth 3 r))
    (if (and (= typ "CURVE") (> km kmlo) (< km kmhi) (not found))
      (setq found (list dir deg))
    )
  )
  found
)

;; LH bulges down (-Y), RH bulges up (+Y); sag is capped at 3.6
(defun VIDC:bulge (kmlo kmhi dir / chord sag bulge)
  (setq chord (- kmhi kmlo))
  (setq sag (/ chord 2.0))
  (if (> sag 3.6) (setq sag 3.6))
  (setq bulge (/ (* 2.0 sag) chord))
  (if (= dir "LH") (- bulge) bulge)
)

(defun VIDC:ensure-layer (name color / ent d old)
  (if (setq ent (tblobjname "LAYER" name))
    ;; layer exists: set its colour (a negative colour = layer off, keep that)
    (progn
      (setq d   (entget ent)
            old (cdr (assoc 62 d)))
      (if (and old (/= (abs old) color))
        (entmod (subst (cons 62 (if (< old 0) (- color) color)) (assoc 62 d) d))
      )
    )
    ;; new layer
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 color) '(6 . "Continuous")))
  )
)

;; Build segments from the CSV rows, scale them horizontally and draw them
;; from the picked point <ip>
(defun VIDC:plot (rows hVal ip / vertices curves prevkm row typ km scaled)
  (setq vertices (list 0.0))
  (setq curves '())
  (setq prevkm 0.0)
  (foreach row rows
    (setq typ (nth 0 row))
    (setq km  (nth 1 row))
    (if (= typ "TRANSITION")
      (progn
        (setq vertices (append vertices (list km)))
        (setq curves (append curves (list (VIDC:find-curve rows prevkm km))))
        (setq prevkm km)
      )
    )
  )
  (if (< (length vertices) 2)
    (princ "\nNo TRANSITION rows found - nothing to plot.")
    (progn
      (VIDC:ensure-layer VIDC:LYR-TRACK VIDC:CLR-TRACK)
      (VIDC:ensure-layer VIDC:LYR-TEXT VIDC:CLR-TEXT)
      (setq scaled
        (mapcar (function (lambda (v) (+ (car ip) (VIDC:scale v hVal)))) vertices))
      (VIDC:draw scaled curves (cadr ip) VIDC:LYR-TRACK VIDC:LYR-TEXT)
      (princ (strcat "\nDone. " (itoa (1- (length vertices))) " segments plotted."))
    )
  )
)

;; <vertices> are the final X positions (scaled + picked point); <y0> = picked Y
(defun VIDC:draw (vertices curves y0 geolayer txtlayer
                  / dxf i arcinfo kmlo kmhi dir deg bulge midx txt radius)
  (setq dxf
    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 geolayer)
          '(100 . "AcDbPolyline") (cons 90 (length vertices)) '(70 . 0) '(38 . 0.0))
  )
  (setq i 0)
  (while (< i (length vertices))
    (setq bulge 0.0)
    (if (and (< i (length curves)) (nth i curves))
      (setq bulge (VIDC:bulge (nth i vertices) (nth (1+ i) vertices)
                              (nth 0 (nth i curves))))
    )
    (setq dxf (append dxf (list (cons 10 (list (nth i vertices) y0)) (cons 42 bulge))))
    (setq i (1+ i))
  )
  (entmake dxf)

  (setq i 0)
  (while (< i (length curves))
    (setq arcinfo (nth i curves))
    (if arcinfo
      (progn
        (setq kmlo (nth i vertices))
        (setq kmhi (nth (1+ i) vertices))
        (setq dir  (nth 0 arcinfo))
        (setq deg  (nth 1 arcinfo))
        (setq midx (/ (+ kmlo kmhi) 2.0))
        (setq radius (if (and deg (/= deg 0.0)) (/ 1750.0 deg) nil))
        (setq txt
          (strcat (if radius (rtos radius 2 1) "-") "m" "\\P"
                  (rtos (if deg deg 0.0) 2 2) "\\U+00B0")
        )
        ;; all labels aligned 5 below the picked point
        (entmake
          (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 txtlayer)
                '(100 . "AcDbMText")
                (cons 10 (list midx (- y0 5.0) 0.0))
                '(40 . 2.0)
                '(41 . 11.0)
                '(71 . 6)
                (cons 1 txt)
                '(50 . 1.5707963))
        )
      )
    )
    (setq i (1+ i))
  )
)

(princ "\nVIDINDEXCURVE loaded. Type VIDINDEXCURVE to run.")
(princ)
