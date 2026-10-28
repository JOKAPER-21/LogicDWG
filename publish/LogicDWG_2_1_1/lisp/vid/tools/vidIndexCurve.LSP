;;; TCPLOT2.LSP - Railway track geometry plotter for Civil 3D 2026
;;; CSV format: SEGMENT TYPE,FROM KM,LH/RH,DEGREE  (header row required)

(defun c:VIDINDEXCURVE ( / file f line rows parts n i row typ km
                     vertices curves prevkm geolayer txtlayer)

  (vl-load-com)
  (setq geolayer "TRACK-GEOMETRY")
  (setq txtlayer "TRACK-CURVE-LABELS")

  (setq file (getfiled "Select Track Geometry CSV File" "" "csv" 16))
  (if (not file) (progn (princ "\nNo file selected.") (exit)))

  (setq f (open file "r"))
  (setq line (read-line f))            ; skip header
  (setq rows '())
  (while (setq line (read-line f))
    (setq line (vl-string-trim " \t\r\n" line))
    (if (> (strlen line) 0)
      (progn
        (setq parts (tcplot2-split line))
        (if (>= (length parts) 2)
          (setq rows
            (append rows
              (list
                (list
                  (strcase (vl-string-trim " \t" (nth 0 parts)))
                  (atof   (vl-string-trim " \t" (nth 1 parts)))
                  (if (>= (length parts) 3)
                    (strcase (vl-string-trim " \t" (nth 2 parts))) "-")
                  (if (and (>= (length parts) 4)
                           (> (strlen (vl-string-trim " \t" (nth 3 parts))) 0))
                    (atof (vl-string-trim " \t" (nth 3 parts))) nil)
                )
              )
            )
          )
        )
      )
    )
  )
  (close f)

  (setq n (length rows))
  (if (= n 0) (progn (princ "\nNo data rows found.") (exit)))

  (tcplot2-ensure-layer geolayer 7)
  (tcplot2-ensure-layer txtlayer 3)

  (setq vertices (list 0.0))
  (setq curves '())
  (setq prevkm 0.0)
  (setq i 0)
  (while (< i n)
    (setq row (nth i rows))
    (setq typ (nth 0 row))
    (setq km  (nth 1 row))
    (if (= typ "TRANSITION")
      (progn
        (setq vertices (append vertices (list km)))
        (setq curves (append curves (list (tcplot2-find-curve rows prevkm km))))
        (setq prevkm km)
      )
    )
    (setq i (1+ i))
  )

  (tcplot2-draw vertices curves geolayer txtlayer)
  (princ (strcat "\nDone. " (itoa (1- (length vertices))) " segments plotted."))
  (princ)
)

;;; ---- helpers ----

(defun tcplot2-split (str / result pos)
  (setq result '())
  (while (setq pos (vl-string-search "," str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (append result (list str))
)

(defun tcplot2-find-curve (rows kmlo kmhi / r typ km dir deg found)
  (setq found nil)
  (foreach r rows
    (setq typ (nth 0 r) km (nth 1 r) dir (nth 2 r) deg (nth 3 r))
    (if (and (= typ "CURVE") (> km kmlo) (< km kmhi) (not found))
      (setq found (list dir deg))
    )
  )
  found
)

;; RH/LH switched: LH now bulges down (-Y), RH now bulges up (+Y)
(defun tcplot2-bulge (kmlo kmhi dir / chord sag bulge)
  (setq chord (- kmhi kmlo))
  (setq sag (/ chord 2.0))
  (if (> sag 3.6) (setq sag 3.6))
  (setq bulge (/ (* 2.0 sag) chord))
  (if (= dir "LH") (- bulge) bulge)
)

(defun tcplot2-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 color) '(6 . "Continuous")))
  )
)

(defun tcplot2-draw (vertices curves geolayer txtlayer
                      / dxf i arcinfo kmlo kmhi dir deg bulge
                        midx insy txt radius)
  (setq dxf
    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 geolayer)
          '(100 . "AcDbPolyline") (cons 90 (length vertices)) '(70 . 0) '(38 . 0.0))
  )
  (setq i 0)
  (while (< i (length vertices))
    (setq bulge 0.0)
    (if (and (< i (length curves)) (nth i curves))
      (setq bulge (tcplot2-bulge (nth i vertices) (nth (1+ i) vertices)
                                  (nth 0 (nth i curves))))
    )
    (setq dxf (append dxf (list (cons 10 (list (nth i vertices) 0.0)) (cons 42 bulge))))
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
        (setq insy -5.0)   ; all labels aligned at Y = -5, globally
        (setq radius (if (and deg (/= deg 0.0)) (/ 1750.0 deg) nil))
        (setq txt
          (strcat (if radius (rtos radius 2 1) "-") "m" "\\P"
                  (rtos (if deg deg 0.0) 2 2) "\\U+00B0")
        )
        (entmake
          (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 txtlayer)
                '(100 . "AcDbMText")
                (cons 10 (list midx insy 0.0))
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

(princ "\nTCPLOT2 loaded. Type TCPLOT2 to run.")
(princ)