;;; ============================================================================
;;; Vid Line To Export Level
;;; Command : VIDLINETOEXPORTLEVEL   (EXPORTLEVEL20 still works)
;;; Version : v24  (was EXPORTLEVEL20)
;;; Works in AutoCAD and AutoCAD Civil 3D (core AutoLISP / ActiveX only).
;;;   Select a polyline / 3D polyline, then a dialog asks for
;;;       Start Chainage : [ 21/000 ]   (km / metre in ONE box, "/" delimiter)
;;;       Interval       : <value>      (whole number, metres)
;;;       Track Type     : [ Up v ]     (Up / Down)
;;;       [Cancel] [Ok]
;;;   A CSV "Chainage,Elevation" is written automatically to the drawing folder:
;;;       <drawing name>_<track type>_railLevel_v01.csv   e.g.  MyLine_Up_railLevel_v01.csv
;;;   If that file already exists the version goes up (v02, v03 ...);
;;;   every <drawing name> + <track type> combination counts its own versions.
;;;   (Unsaved drawing / folder not writable -> a Save dialog is shown.)
;;;   Chainage = start chainage in metres (km*1000 + m) + distance along the
;;;   polyline.
;;;   Tab order: Start Chainage, Interval, Track Type, Cancel, Ok.
;;; ============================================================================

(vl-load-com)

(if (null VEL-Km)       (setq VEL-Km 0))
(if (null VEL-M)        (setq VEL-M 0))
(if (null VEL-Interval) (setq VEL-Interval 20))
(if (null VEL-Type)     (setq VEL-Type "Up"))

(defun VEL-IntP (s)
  (and s
       (/= (vl-string-trim " " s) "")
       (= (vl-string-trim "0123456789" (vl-string-trim " " s)) "")
  )
)

;; "21/000" style split on any delimiter
(defun VEL-Split (str delim / pos res)
  (setq res '())
  (while (setq pos (vl-string-search delim str))
    (setq res (cons (substr str 1 pos) res)
          str (substr str (+ pos 1 (strlen delim)))))
  (reverse (cons str res))
)

;; metre as 3 digits (5 -> "005")
(defun VEL-Pad3 (m / s)
  (setq s (strcat "000" (itoa m)))
  (substr s (- (strlen s) 2))
)

;; OK button check - stores the values, returns T / nil
(defun VEL-Validate (/ kmm parts iv)
  (setq kmm   (vl-string-trim " " (get_tile "edt_kmm"))
        iv    (get_tile "edt_int")
        parts (VEL-Split kmm "/"))
  (cond
    ((or (/= (length parts) 2)
         (not (VEL-IntP (car parts)))
         (not (VEL-IntP (cadr parts))))
     (alert "Enter the start chainage as whole numbers separated by \"/\", e.g. 21/000.")
     (mode_tile "edt_kmm" 2) nil)
    ((> (atoi (vl-string-trim " " (cadr parts))) 999)
     (alert "Metre (after the \"/\") must be between 0 and 999.")
     (mode_tile "edt_kmm" 2) nil)
    ((or (not (VEL-IntP iv)) (< (atoi (vl-string-trim " " iv)) 1))
     (alert "Interval must be a whole number (1 or more).")
     (mode_tile "edt_int" 2) nil)
    (T
     (setq VEL-Km       (atoi (vl-string-trim " " (car parts)))
           VEL-M        (atoi (vl-string-trim " " (cadr parts)))
           VEL-Interval (atoi (vl-string-trim " " iv))
           VEL-Type     (if (= (get_tile "pop_type") "1") "Down" "Up"))
     T)
  )
)

;; 1 -> "01", 12 -> "12", 100 -> "100"
(defun VEL-Pad2 (n)
  (if (< n 10) (strcat "0" (itoa n)) (itoa n))
)

;; <dwg name>_<track type>_railLevel_vNN.csv in the drawing folder, next free version
(defun VEL-AutoName (/ dir base n path)
  (setq dir  (getvar "DWGPREFIX")
        base (vl-filename-base (getvar "DWGNAME"))
        n    1)
  (if (or (null dir) (= dir ""))
    nil
    (progn
      (if (/= (substr dir (strlen dir)) "\\") (setq dir (strcat dir "\\")))
      (setq path (strcat dir base "_" VEL-Type "_railLevel_v" (VEL-Pad2 n) ".csv"))
      (while (findfile path)
        (setq n (1+ n))
        (setq path (strcat dir base "_" VEL-Type "_railLevel_v" (VEL-Pad2 n) ".csv"))
      )
      path
    )
  )
)

(defun VEL-Dialog (/ dlgName f dcl_id ret ln)
  (setq dlgName (vl-filename-mktemp "velUi" (getvar "TEMPPREFIX") ".dcl"))
  (setq f (open dlgName "w"))
  (foreach ln
    (list
      "vel_ui : dialog {"
      "  label = \"Export Level\";"
      "  : edit_box { key = \"edt_kmm\"; label = \"Start Chainage (e.g. 21/000):\"; edit_width = 10; }"
      "  : edit_box { key = \"edt_int\"; label = \"Interval:\";                     edit_width = 10; }"
      "  : popup_list { key = \"pop_type\"; label = \"Track Type:\";               edit_width = 10; }"
      "  spacer_1;"
      "  : row {"
      "    alignment = centered;"
      "    : button { key = \"cancel\"; label = \"Cancel\"; width = 10; fixed_width = true; is_cancel = true; }"
      "    : button { key = \"accept\"; label = \"Ok\";     width = 10; fixed_width = true; is_default = true; }"
      "  }"
      "}")
    (write-line ln f))
  (close f)
  (setq dcl_id (load_dialog dlgName))
  (setq ret nil)
  (if (and dcl_id (new_dialog "vel_ui" dcl_id))
    (progn
      (set_tile "edt_kmm" (strcat (itoa VEL-Km) "/" (VEL-Pad3 VEL-M)))
      (set_tile "edt_int" (itoa VEL-Interval))
      (start_list "pop_type")
      (add_list "Up")
      (add_list "Down")
      (end_list)
      (set_tile "pop_type" (if (= VEL-Type "Down") "1" "0"))
      (mode_tile "edt_kmm" 2)
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
     (if (VEL-Dialog)
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
           (setq file (getfiled "Save CSV As"
                                (strcat (vl-filename-base (getvar "DWGNAME")) "_" VEL-Type "_railLevel_v01.csv")
                                "csv" 1))
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
