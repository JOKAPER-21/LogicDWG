;; ============================================================
;; vidBoundaryMarker  -  Civil 3D / AutoCAD AutoLISP
;; Command : vidBoundaryMarker   (alias: VBM)
;;
;; Layers  :
;;   1-VBM_TEXT      -> L= / R= value text (aligned along the perpendicular line)
;;   1-VBM_BOUNDARY  -> Polyline joining all Left ends / all Right ends
;;   1-VBM_DISTANCE  -> Perpendicular line for every chainage value
;;
;; Flow    : 1) CSV select  2) Dialog (Start Chainage + Single/Double)
;;           3) Polyline pick  4) Draw
;; CSV     : Chainage, Left, Right   (header row auto-skipped)
;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; MAIN COMMAND
;; ------------------------------------------------------------
(defun c:vidBoundaryMarker ( / *error* csv_file csv_data opts start_ch mode
                               pl1 plR plL dist r left_pts right_pts cnt)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (prompt (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; --- Layers ---
  (VBM-MAKE-LAYER "1-VBM_TEXT"     7)
  (VBM-MAKE-LAYER "1-VBM_BOUNDARY" 7)
  (VBM-MAKE-LAYER "1-VBM_DISTANCE" 7)

  ;; --- STEP 1 : CSV first ---
  (setq csv_file (getfiled "CSV File Select pannunga" "" "csv" 0))
  (cond
    ((null csv_file) (prompt "\nCSV file select pannalai."))
    (t
     (setq csv_data (VBM-READ-CSV csv_file))
     (cond
       ((null csv_data) (prompt "\nCSV-la valid data illai."))
       (t
        (prompt (strcat "\n" (itoa (length csv_data)) " rows found."))

        ;; --- STEP 2 : Dialog ---
        (setq opts (VBM-DIALOG))
        (cond
          ((null opts) (prompt "\nCancelled."))
          (t
           (setq start_ch (VBM-NORM-CH (car opts))
                 mode     (cadr opts))

           ;; --- STEP 3 : Pick polyline(s) ---
           (if (= mode "single")
             (progn
               (setq pl1 (VBM-PICK-PL "\nPolyline select pannunga (Left & Right ithula varum): "))
               (setq plR pl1 plL pl1)
             )
             (progn
               (setq plR (VBM-PICK-PL "\n1st Polyline select pannunga (RIGHT side): "))
               (if plR
                 (setq plL (VBM-PICK-PL "\n2nd Polyline select pannunga (LEFT side): "))
               )
             )
           )

           ;; --- STEP 4 : Draw ---
           (if (and plR plL)
             (progn
               (setq left_pts '() right_pts '() cnt 0)
               (foreach row csv_data
                 (setq dist (- (car row) start_ch))
                 (if (< dist 0.0)
                   (prompt (strcat "\nChainage " (rtos (car row) 2 2) " - skip (start-ku munnadi)"))
                   (progn
                     (setq r (VBM-PROCESS-ROW plR plL dist (cadr row) (caddr row)))
                     (if (car r)  (setq left_pts  (append left_pts  (list (car r)))))
                     (if (cadr r) (setq right_pts (append right_pts (list (cadr r)))))
                     (setq cnt (1+ cnt))
                   )
                 )
               )

               (if (> (length left_pts) 1)
                 (VBM-DRAW-BOUNDARY left_pts 1)
               )
               (if (> (length right_pts) 1)
                 (VBM-DRAW-BOUNDARY right_pts 3)
               )

               (prompt (strcat "\n>>> Done! Chainages processed: " (itoa cnt)
                               " | Left pts: "  (itoa (length left_pts))
                               " | Right pts: " (itoa (length right_pts))))
             )
             (prompt "\nPolyline select pannalai - cancelled.")
           )
          )
        )
       )
     )
    )
  )
  (princ)
)

;; Short alias
(defun c:VBM () (c:vidBoundaryMarker))

;; ------------------------------------------------------------
;; DIALOG  (DCL is written to a temp file automatically)
;; returns (start_chainage "single"|"double") or nil
;; ------------------------------------------------------------
(defun VBM-DIALOG ( / dcl_file f dcl_id status res)
  (setq dcl_file (vl-filename-mktemp "vbm" nil ".dcl"))
  (setq f (open dcl_file "w"))
  (foreach l
    '("vbm : dialog {"
      "  label = \"vidBoundaryMarker\";"
      "  : edit_box {"
      "    key = \"ch\";"
      "    label = \"Starting Chainage :\";"
      "    edit_width = 14;"
      "  }"
      "  : boxed_column {"
      "    label = \"Line Type\";"
      "    : toggle {"
      "      key = \"single\";"
      "      label = \"Single Line  (1 line click = Left + Right)\";"
      "    }"
      "    : toggle {"
      "      key = \"double\";"
      "      label = \"Double Line  (1st = Right, 2nd = Left)\";"
      "    }"
      "  }"
      "  : errtile { width = 40; }"
      "  ok_cancel;"
      "}")
    (write-line l f)
  )
  (close f)

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "vbm" dcl_id))
    (progn (prompt "\nDialog load aagala.") (setq res nil))
    (progn
      (set_tile "ch" "0")
      (set_tile "single" "1")
      (set_tile "double" "0")

      ;; tick boxes - always exactly one selected
      (action_tile "single"
        "(set_tile \"double\" (if (= $value \"1\") \"0\" \"1\"))")
      (action_tile "double"
        "(set_tile \"single\" (if (= $value \"1\") \"0\" \"1\"))")

      (action_tile "accept"
        (strcat
          "(if (distof (get_tile \"ch\"))"
          "  (progn"
          "    (setq *vbm_ch* (get_tile \"ch\") *vbm_single* (get_tile \"single\"))"
          "    (done_dialog 1))"
          "  (set_tile \"error\" \"Valid chainage number enter pannunga\"))"))
      (action_tile "cancel" "(done_dialog 0)")

      (setq status (start_dialog))
      (if (= status 1)
        (setq res (list (distof *vbm_ch*)
                        (if (= *vbm_single* "1") "single" "double")))
      )
    )
  )
  (if dcl_id (unload_dialog dcl_id))
  (vl-file-delete dcl_file)
  res
)

;; ------------------------------------------------------------
;; Pick polyline (Enter = cancel)
;; ------------------------------------------------------------
(defun VBM-PICK-PL (msg / sel ent typ done res)
  (while (not done)
    (setq sel (entsel msg))
    (cond
      ((null sel) (setq done T))
      (t
       (setq ent (car sel)
             typ (cdr (assoc 0 (entget ent))))
       (if (member typ '("LWPOLYLINE" "POLYLINE" "LINE" "ARC" "SPLINE"))
         (setq res ent done T)
         (prompt "\nPolyline illai! Marupadi select pannunga.")
       )
      )
    )
  )
  res
)

;; ------------------------------------------------------------
;; Process one CSV row -> returns (left_end right_end)
;; ------------------------------------------------------------
(defun VBM-PROCESS-ROW (plR plL dist lb rb / pt n lpt rpt)
  ;; LEFT
  (if (> (abs lb) 1e-9)
    (progn
      (setq pt (VBM-PT-AT plL dist)
            n  (VBM-NORMAL plL dist))
      (if (and pt n)
        (progn
          (setq lpt (list (+ (car pt)  (* (car n)  lb))
                          (+ (cadr pt) (* (cadr n) lb))
                          (if (caddr pt) (caddr pt) 0.0)))
          (VBM-DRAW-SIDE pt lpt (strcat "L=" (rtos lb 2 3)) 1)
        )
        (prompt (strcat "\nLeft: dist " (rtos dist 2 2) " polyline-ku veliya - skip"))
      )
    )
  )
  ;; RIGHT
  (if (> (abs rb) 1e-9)
    (progn
      (setq pt (VBM-PT-AT plR dist)
            n  (VBM-NORMAL plR dist))
      (if (and pt n)
        (progn
          (setq rpt (list (- (car pt)  (* (car n)  rb))
                          (- (cadr pt) (* (cadr n) rb))
                          (if (caddr pt) (caddr pt) 0.0)))
          (VBM-DRAW-SIDE pt rpt (strcat "R=" (rtos rb 2 3)) 3)
        )
        (prompt (strcat "\nRight: dist " (rtos dist 2 2) " polyline-ku veliya - skip"))
      )
    )
  )
  (list lpt rpt)
)

;; ------------------------------------------------------------
;; Point on curve at distance
;; ------------------------------------------------------------
(defun VBM-PT-AT (ent dist / param)
  (setq param (vlax-curve-getParamAtDist ent dist))
  (if param (vlax-curve-getPointAtParam ent param) nil)
)

;; ------------------------------------------------------------
;; Unit LEFT normal at distance
;; ------------------------------------------------------------
(defun VBM-NORMAL (ent dist / param tg tx ty len)
  (setq param (vlax-curve-getParamAtDist ent dist))
  (if param
    (progn
      (setq tg  (vlax-curve-getFirstDeriv ent param)
            tx  (car tg)
            ty  (cadr tg)
            len (sqrt (+ (* tx tx) (* ty ty))))
      (if (> len 1e-12)
        (list (/ (- ty) len) (/ tx len) 0.0)
        nil
      )
    )
    nil
  )
)

;; ------------------------------------------------------------
;; Draw perpendicular line (1-VBM_DISTANCE) + text (1-VBM_TEXT)
;; Text is aligned along the line, kept readable
;; ------------------------------------------------------------
(defun VBM-DRAW-SIDE (p1 p2 label clr / ang tang mid tpt th)
  (setq th 1.5)

  ;; perpendicular line
  (entmakex (list '(0 . "LINE")
                  (cons 8  "1-VBM_DISTANCE")
                  (cons 62 clr)
                  (cons 10 p1)
                  (cons 11 p2)))

  ;; text along the line
  (setq ang (angle p1 p2))
  (setq tang ang)
  (if (and (> ang (+ (/ pi 2.0) 1e-6))
           (<= ang (+ (* 1.5 pi) 1e-6)))
    (setq tang (- ang pi))
  )
  (if (< tang 0.0) (setq tang (+ tang (* 2.0 pi))))

  (setq mid (polar p1 ang (/ (distance p1 p2) 2.0)))
  ;; small offset so text sits just above the line
  (setq tpt (polar mid (+ tang (/ pi 2.0)) (* th 0.8)))

  (entmakex (list '(0 . "TEXT")
                  (cons 8  "1-VBM_TEXT")
                  (cons 62 clr)
                  (cons 10 tpt)
                  (cons 11 tpt)
                  (cons 40 th)
                  (cons 1  label)
                  (cons 50 tang)
                  (cons 72 1)
                  (cons 73 2)))
)

;; ------------------------------------------------------------
;; Boundary polyline joining all end points (open)
;; ------------------------------------------------------------
(defun VBM-DRAW-BOUNDARY (pts clr / data)
  (setq data
    (list '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          (cons 8  "1-VBM_BOUNDARY")
          (cons 62 clr)
          '(100 . "AcDbPolyline")
          (cons 90 (length pts))
          (cons 70 0)))
  (foreach p pts
    (setq data (append data (list (cons 10 (list (car p) (cadr p))))))
  )
  (entmakex data)
)

;; ------------------------------------------------------------
;; Layer create if missing
;; ------------------------------------------------------------
(defun VBM-MAKE-LAYER (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   '(70 . 0)
                   (cons 62 color)
                   '(6 . "Continuous")))
  )
)

;; ------------------------------------------------------------
;; CSV reader (header auto-skip, quoted numbers OK)
;; ------------------------------------------------------------
(defun VBM-READ-CSV (filepath / f line rows fields)
  (setq rows '())
  (setq f (open filepath "r"))
  (if (null f)
    (prompt "\nFile open error!")
    (progn
      (while (setq line (read-line f))
        (setq fields (VBM-SPLIT line ","))
        (if (and (>= (length fields) 3)
                 (wcmatch (car fields) "*#*"))   ; first field has a digit -> data row
          (setq rows (cons (list (VBM-NORM-CH (VBM-ATOF (car fields)))
                                 (VBM-ATOF (cadr fields))
                                 (VBM-ATOF (caddr fields)))
                           rows))
        )
      )
      (close f)
    )
  )
  (reverse rows)
)

(defun VBM-SPLIT (str delim / result token i c in_q)
  (setq result '() token "" i 1 in_q nil)
  (while (<= i (strlen str))
    (setq c (substr str i 1))
    (cond
      ((= c "\"") (setq in_q (not in_q)))
      ((and (= c delim) (not in_q))
       (setq result (append result (list (vl-string-trim " \t" token))))
       (setq token ""))
      (t (setq token (strcat token c)))
    )
    (setq i (1+ i))
  )
  (append result (list (vl-string-trim " \t" token)))
)

(defun VBM-ATOF (s / clean i)
  (setq clean "" i 1)
  (while (<= i (strlen s))
    (cond
      ((= (substr s i 1) ",") nil)
      ((= (substr s i 1) "/") (setq clean (strcat clean ".")))
      (t (setq clean (strcat clean (substr s i 1))))
    )
    (setq i (1+ i))
  )
  (atof clean)
)

;; ------------------------------------------------------------
;; Chainage normalise:  608.65 -> 608650 ,  608700 -> 608700
;; (0 < value < 10000  => treated as km format, x 1000)
;; ------------------------------------------------------------
(defun VBM-NORM-CH (v)
  (if (and (> (abs v) 1e-9) (< (abs v) 10000.0))
    (/ (fix (+ (* v 1000.0) (if (< v 0) -0.5 0.5))) 1.0)
    v
  )
)

;; ------------------------------------------------------------
(prompt "\n=== vidBoundaryMarker loaded ===  Command: vidBoundaryMarker  (alias: VBM)")
(princ)
