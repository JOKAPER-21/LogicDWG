;;; ============================================================================
;;; Vid Line To Export Level
;;; Command : VIDLINETOEXPORTLEVEL   (EXPORTLEVEL20 still works)
;;; Version : v22  (was EXPORTLEVEL20)
;;; Works in AutoCAD and AutoCAD Civil 3D (core AutoLISP / ActiveX only).
;;;   Select a polyline / 3D polyline, then a dialog asks for
;;;       Start Chainage : <km> / <m>   (whole numbers)
;;;       Interval       : <value>      (whole number, metres)
;;;       [Cancel] [Ok]
;;;   A CSV "Chainage,Elevation" is written automatically to the drawing folder:
;;;       EL_<drawing name>_v01_exportLevel.csv
;;;   If that file already exists the version goes up (v02, v03 ...).
;;;   (Unsaved drawing / folder not writable -> a Save dialog is shown.)
;;;   A CSV "Chainage,Elevation" is written.  Chainage = start chainage in
;;;   metres (km*1000 + m) + distance along the polyline.
;;;   Tab order: km, m, Interval, Cancel, Ok.
;;; ============================================================================

(vl-load-com)

(if (null VEL-Km)       (setq VEL-Km 0))
(if (null VEL-M)        (setq VEL-M 0))
(if (null VEL-Interval) (setq VEL-Interval 20))

(defun VEL-IntP (s)
  (and s
       (/= (vl-string-trim " " s) "")
       (= (vl-string-trim "0123456789" (vl-string-trim " " s)) "")
  )
)

;; OK button check - stores the values, returns T / nil
(defun VEL-Validate (/ k m iv)
  (setq k  (get_tile "edt_km")
        m  (get_tile "edt_m")
        iv (get_tile "edt_int"))
  (cond
    ((not (VEL-IntP k))
     (alert "KM must be a whole number (0 or more).")
     (mode_tile "edt_km" 2) nil)
    ((or (not (VEL-IntP m)) (> (atoi (vl-string-trim " " m)) 999))
     (alert "Metre must be a whole number between 0 and 999.")
     (mode_tile "edt_m" 2) nil)
    ((or (not (VEL-IntP iv)) (< (atoi (vl-string-trim " " iv)) 1))
     (alert "Interval must be a whole number (1 or more).")
     (mode_tile "edt_int" 2) nil)
    (T
     (setq VEL-Km       (atoi (vl-string-trim " " k))
           VEL-M        (atoi (vl-string-trim " " m))
           VEL-Interval (atoi (vl-string-trim " " iv)))
     T)
  )
)

;; 1 -> "01", 12 -> "12", 100 -> "100"
(defun VEL-Pad2 (n)
  (if (< n 10) (strcat "0" (itoa n)) (itoa n))
)

;; EL_<dwg name>_vNN_exportLevel.csv in the drawing folder, next free version
(defun VEL-AutoName (/ dir base n path)
  (setq dir  (getvar "DWGPREFIX")
        base (vl-filename-base (getvar "DWGNAME"))
        n    1)
  (if (or (null dir) (= dir ""))
    nil
    (progn
      (if (/= (substr dir (strlen dir)) "\\") (setq dir (strcat dir "\\")))
      (setq path (strcat dir "EL_" base "_v" (VEL-Pad2 n) "_exportLevel.csv"))
      (while (findfile path)
        (setq n (1+ n))
        (setq path (strcat dir "EL_" base "_v" (VEL-Pad2 n) "_exportLevel.csv"))
      )
      path
    )
  )
)

(defun VEL-Dialog (lenTxt / dlgName f dcl_id ret)
  (setq dlgName (vl-filename-mktemp "velUi" (getvar "TEMPPREFIX") ".dcl"))
  (setq f (open dlgName "w"))
  (write-line
    (strcat
      "vel_ui : dialog {\n"
      "  label = \"Export Level\";\n"
      "  : text { key = \"lbl_len\"; label = \"\"; width = 40; }\n"
      "  : boxed_row {\n"
      "    label = \"Start Chainage\";\n"
      "    : edit_box { key = \"edt_km\"; label = \"\"; edit_width = 8; }\n"
      "    : text { label = \"/\"; width = 2; fixed_width = true; }\n"
      "    : edit_box { key = \"edt_m\"; label = \"\"; edit_width = 6; }\n"
      "  }\n"
      "  : edit_box { key = \"edt_int\"; label = \"Interval\"; edit_width = 8; }\n"
      "  spacer_1;\n"
      "  : row {\n"
      "    alignment = centered;\n"
      "    : button { key = \"cancel\"; label = \"Cancel\"; width = 10; fixed_width = true; is_cancel = true; }\n"
      "    : button { key = \"accept\"; label = \"Ok\";     width = 10; fixed_width = true; is_default = true; }\n"
      "  }\n"
      "}\n")
    f)
  (close f)
  (setq dcl_id (load_dialog dlgName))
  (setq ret nil)
  (if (and dcl_id (new_dialog "vel_ui" dcl_id))
    (progn
      (set_tile "lbl_len" lenTxt)
      (set_tile "edt_km"  (itoa VEL-Km))
      (set_tile "edt_m"   (itoa VEL-M))
      (set_tile "edt_int" (itoa VEL-Interval))
      (mode_tile "edt_km" 2)
      (action_tile "accept" "(if (VEL-Validate) (done_dialog 1))")
      (action_tile "cancel" "(done_dialog 0)")
      (setq ret (= (start_dialog) 1))
    )
    (princ "\nVIDLINETOEXPORTLEVEL: could not open dialog.")
  )
  (if dcl_id (unload_dialog dcl_id))
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  ret
)

(defun c:VIDLINETOEXPORTLEVEL (/ ent obj len dist startM ch elev pt data file f)
  (vl-load-com)
  (setq ent (car (entsel "\nSelect polyline with elevation: ")))

  (cond
    ((null ent) (prompt "\nNo entity selected."))
    ((not (member (vla-get-ObjectName (setq obj (vlax-ename->vla-object ent)))
                  '("AcDbPolyline" "AcDb3dPolyline" "AcDb2dPolyline")))
     (prompt "\nSelected object is not a supported polyline."))
    (T
     (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
     (if (VEL-Dialog (strcat "Polyline length: " (rtos len 2 3) " m"))
       (progn
         (setq startM (+ (* VEL-Km 1000) VEL-M))
         (setq dist 0 data '("Chainage,Elevation"))
         (while (<= dist len)
           (setq pt (vlax-curve-getPointAtDist obj dist))
           (if pt
             (setq data
               (cons (strcat (rtos (+ startM dist) 2 3) ","
                             (rtos (caddr pt) 2 3))
                     data))
           )
           (setq dist (+ dist VEL-Interval))
         )
         (setq file (VEL-AutoName))
         (if (or (null file) (null (setq f (open file "w"))))
           (setq file (getfiled "Save CSV As" "EL_exportLevel.csv" "csv" 1))
           (close f)
         )
         (if file
           (progn
             (setq f (open file "w"))
             (foreach line (reverse data) (write-line line f))
             (close f)
             (princ (strcat "\nExported: " file))
             (alert (strcat "Export successful to:\n" file))
           )
           (prompt "\nCancelled export.")
         )
       )
       (prompt "\nCancelled.")
     )
    )
  )
  (princ)
)

(defun c:EXPORTLEVEL20 () (c:VIDLINETOEXPORTLEVEL))

(princ "\nVIDLINETOEXPORTLEVEL loaded. Type VIDLINETOEXPORTLEVEL to run.")
(princ)
