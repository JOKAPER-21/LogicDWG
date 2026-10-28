;;; ============================================================================
;;; vidRwBoundary.lsp   (was setBoundaryYard_v02.lsp / SETBOUNDARYYD)
;;; Command: VIDRWBOUNDARY
;;; Labels the distance from a boundary polyline to a rail track, with an
;;; optional chainage line ("@ KM 21/000").
;;;
;;; v03: Distance mode places a label every <spacing> along the boundary curve
;;;      (not only at vertices). Text is placed on the side INSIDE the boundary.
;;; ============================================================================

(vl-load-com)

;; ---- remembered dialog values ---------------------------------------------
(if (not *vrb-mode*)  (setq *vrb-mode*  "Distance"))
(if (not *vrb-dist*)  (setq *vrb-dist*  "40"))
(if (not *vrb-usekm*) (setq *vrb-usekm* "1"))
(if (not *vrb-km*)    (setq *vrb-km*    "0/000"))

;; ---- small helpers ----------------------------------------------------------
(defun vrb:roundval (val decimals)
  (/ (float (fix (+ (* val (expt 10 decimals)) 0.5))) (expt 10 decimals))
)

(defun vrb:digits-p (s / ok)
  (setq ok (> (strlen s) 0))
  (foreach c (vl-string->list s)
    (if (or (< c 48) (> c 57)) (setq ok nil))
  )
  ok
)

(defun vrb:number-p (s / ok dots)
  (setq ok (> (strlen s) 0) dots 0)
  (foreach c (vl-string->list s)
    (cond
      ((= c 46) (setq dots (1+ dots)))
      ((or (< c 48) (> c 57)) (setq ok nil))
    )
  )
  (and ok (<= dots 1) (/= s "."))
)

;; "21/000" -> (21 0.0)   "21/5.5" -> (21 5.5)   invalid -> nil
(defun vrb:parse-km (str / s p k m)
  (setq s (vl-string-trim " " str))
  (if (setq p (vl-string-position 47 s))
    (progn
      (setq k (vl-string-trim " " (substr s 1 p)))
      (setq m (vl-string-trim " " (substr s (+ p 2))))
      (if (and (vrb:digits-p k) (vrb:number-p m))
        (list (atoi k) (atof m))
      )
    )
  )
)

(defun vrb:type (en)
  (cdr (assoc 0 (entget en)))
)

;; Boundary vertices as (x y 0.0) list. LWPOLYLINE and POLYLINE (2D/3D).
(defun vrb:vertices (en / ed pts e v)
  (setq ed (entget en))
  (if (= (cdr (assoc 0 ed)) "LWPOLYLINE")
    (foreach d ed
      (if (= (car d) 10)
        (setq pts (cons (list (cadr d) (caddr d) 0.0) pts))
      )
    )
    (progn
      (setq e (entnext en))
      (while (and e (= (cdr (assoc 0 (setq v (entget e)))) "VERTEX"))
        ;; skip spline-frame control points (flag 16)
        (if (= 0 (logand 16 (cdr (assoc 70 v))))
          (setq pts (cons (list (cadr (assoc 10 v)) (caddr (assoc 10 v)) 0.0) pts))
        )
        (setq e (entnext e))
      )
    )
  )
  (reverse pts)
)

(defun vrb:curve-p (en)
  (and en
       (not (vl-catch-all-error-p
              (vl-catch-all-apply 'vlax-curve-getEndParam (list en))))
  )
)

;; points every <spacing> along the boundary curve
(defun vrb:sample-curve (en spacing / total d pts p)
  (setq total (vlax-curve-getDistAtParam en (vlax-curve-getEndParam en))
        d 0.0)
  (while (< d (- total 0.01))
    (if (setq p (vlax-curve-getPointAtDist en d))
      (setq pts (cons (list (car p) (cadr p) 0.0) pts))
    )
    (setq d (+ d spacing))
  )
  (reverse pts)
)

;; vertices, skipping ones closer than minLen to the previous kept one
(defun vrb:filter-vertices (vlist minLen / out last)
  (foreach p vlist
    (if (or (null last) (>= (distance last p) minLen))
      (setq out (cons p out) last p)
    )
  )
  (reverse out)
)

;; ray-casting point-in-polygon (2D)
(defun vrb:inside-p (pt vlist / x y n i a b in)
  (setq x (car pt) y (cadr pt) n (length vlist) i 0 in nil)
  (while (< i n)
    (setq a (nth i vlist)
          b (nth (if (= i 0) (1- n) (1- i)) vlist))
    (if (and (/= (> (cadr a) y) (> (cadr b) y))
             (< x (+ (car a)
                     (/ (* (- (car b) (car a)) (- y (cadr a)))
                        (- (cadr b) (cadr a))))))
      (setq in (not in))
    )
    (setq i (1+ i))
  )
  in
)

;; ---- selection --------------------------------------------------------------
(defun vrb:pick (what boundary / sel en ok)
  (setq ok nil)
  (while (not ok)
    (setq sel (entsel (strcat "\nSelect " what " (Enter = back to dialog): ")))
    (cond
      ((null sel)
       (setq en nil ok T))
      ((and boundary (wcmatch (vrb:type (car sel)) "LWPOLYLINE,POLYLINE"))
       (setq en (car sel) ok T))
      ((and (not boundary) (vrb:curve-p (car sel)))
       (setq en (car sel) ok T))
      (T
       (princ (if boundary
                "\nPlease select a polyline."
                "\nPlease select a line / polyline / curve.")))
    )
  )
  en
)

;; ---- dialog ------------------------------------------------------------------
(defun vrb:write-dcl (/ f dcl)
  (setq dcl (vl-filename-mktemp "vidRwBoundary" nil ".dcl"))
  (setq f (open dcl "w"))
  (foreach l
    '("vrbBtn : button { width = 18; height = 1.6; fixed_width = true; fixed_height = true; }"
      "vidRwBoundary : dialog {"
      "  label = \"VID RW Boundary\";"
      "  : boxed_radio_row {"
      "    label = \"Label position\";"
      "    key = \"mode\";"
      "    : radio_button { key = \"r_vertex\"; label = \"Vertex\"; }"
      "    : radio_button { key = \"r_dist\";   label = \"Distance\"; }"
      "  }"
      "  : edit_box { key = \"e_dist\"; label = \"Distance between labels\"; edit_width = 10; }"
      "  : toggle { key = \"t_km\"; label = \"@KM/METER\"; }"
      "  : edit_box { key = \"e_km\"; label = \"Enter starting KM (eg. 21/000)\"; edit_width = 10; }"
      "  spacer_1;"
      "  : row {"
      "    : vrbBtn { key = \"b_bnd\"; label = \"Select Boundary\"; }"
      "    : text { key = \"x_bnd\"; label = \"Not selected\"; width = 26; }"
      "  }"
      "  : row {"
      "    : vrbBtn { key = \"b_trk\"; label = \"Select Track\"; }"
      "    : text { key = \"x_trk\"; label = \"Not selected\"; width = 26; }"
      "  }"
      "  : errtile { width = 44; }"
      "  : row {"
      "    alignment = centered;"
      "    : vrbBtn { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; }"
      "    : vrbBtn { key = \"accept\"; label = \"Generate\"; is_default = true; }"
      "  }"
      "}")
    (write-line l f)
  )
  (close f)
  dcl
)

;; enable / disable the fields that depend on the radio and the check box
(defun vrb:update-tiles ()
  (mode_tile "e_dist" (if (= (get_tile "r_dist") "1") 0 1))
  (mode_tile "e_km"   (if (= (get_tile "t_km") "1") 0 1))
)

;; copy tile values into the remembered globals
(defun vrb:save-tiles ()
  (setq *vrb-mode*  (if (= (get_tile "r_vertex") "1") "Vertex" "Distance")
        *vrb-dist*  (get_tile "e_dist")
        *vrb-usekm* (get_tile "t_km")
        *vrb-km*    (get_tile "e_km"))
)

;; Generate pressed: validate, only close when everything is ok
(defun vrb:accept (/ msg d)
  (vrb:save-tiles)
  (setq msg
    (cond
      ((null *vrb-bnd*) "Select the boundary polyline.")
      ((null *vrb-trk*) "Select the track.")
      ((and (= *vrb-mode* "Distance")
            (or (not (vrb:number-p (vl-string-trim " " *vrb-dist*)))
                (<= (atof *vrb-dist*) 0.0)))
       "Enter a distance greater than 0.")
      ((and (= *vrb-usekm* "1") (null (vrb:parse-km *vrb-km*)))
       "Starting KM must look like 21/000 (KM/METER).")
    )
  )
  (if msg
    (set_tile "error" msg)
    (done_dialog 1)
  )
)

;; returns 1 = generate, 0 = cancel
(defun vrb:dialog (/ dcl id r go)
  (setq dcl (vrb:write-dcl) go T r 0)
  (while go
    (setq id (load_dialog dcl))
    (if (not (new_dialog "vidRwBoundary" id))
      (setq go nil r 0)
      (progn
        (set_tile (if (= *vrb-mode* "Vertex") "r_vertex" "r_dist") "1")
        (set_tile "e_dist" *vrb-dist*)
        (set_tile "t_km"   *vrb-usekm*)
        (set_tile "e_km"   *vrb-km*)
        (set_tile "x_bnd" (if *vrb-bnd* (strcat "Selected (" (vrb:type *vrb-bnd*) ")") "Not selected"))
        (set_tile "x_trk" (if *vrb-trk* (strcat "Selected (" (vrb:type *vrb-trk*) ")") "Not selected"))
        (vrb:update-tiles)
        (action_tile "r_vertex" "(vrb:update-tiles)")
        (action_tile "r_dist"   "(vrb:update-tiles)")
        (action_tile "t_km"     "(vrb:update-tiles)")
        (action_tile "b_bnd"    "(vrb:save-tiles) (done_dialog 2)")
        (action_tile "b_trk"    "(vrb:save-tiles) (done_dialog 3)")
        (action_tile "accept"   "(vrb:accept)")
        (action_tile "cancel"   "(done_dialog 0)")
        (setq r (start_dialog))
      )
    )
    (unload_dialog id)
    (cond
      ((= r 2) (setq *vrb-bnd* (cond ((vrb:pick "Boundary polyline" T)) (T *vrb-bnd*))))
      ((= r 3) (setq *vrb-trk* (cond ((vrb:pick "Track" nil)) (T *vrb-trk*))))
      (T (setq go nil))
    )
  )
  (if (findfile dcl) (vl-file-delete dcl))
  r
)

;; ---- label generation --------------------------------------------------------
(defun vrb:generate (bnd trk placeMode spacing useKM startKM startM /
                     vlist ptlist pt param dist dir alt
                     txtpt txtang prevTxtAng endParam trackStart trackEnd trackDir
                     chainDist totalDist totalDistRounded km m kmStr mStr txtstr
                     distRounded count perp
                     txth offset minSegLen stylename)

  (setq txth       1.25
        offset    -2.5
        minSegLen  2.0
        stylename "ARIAL_TXT"
        count      0)

  (if (not (tblsearch "STYLE" stylename))
    (entmake
      (list
        '(0 . "STYLE")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbTextStyleTableRecord")
        (cons 2 stylename)
        (cons 3 "arial.ttf")
        (cons 40 0.0)
        (cons 41 1.0)
        (cons 50 0.0)
        (cons 71 0)
      )
    )
  )

  ;; layer for the labels
  (if (not (tblsearch "LAYER" "1-boundary"))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        '(2 . "1-boundary")
        '(70 . 0)
        '(62 . 7)
      )
    )
  )

  (setq endParam   (vlax-curve-getEndParam trk))
  (setq trackStart (vlax-curve-getPointAtParam trk 0.0))
  (setq trackEnd   (vlax-curve-getPointAtParam trk endParam))
  (setq trackDir   (angle trackStart trackEnd))
  (setq vlist      (vrb:vertices bnd))

  ;; where to put labels
  (setq ptlist
    (if (= placeMode "Vertex")
      (vrb:filter-vertices vlist minSegLen)
      (vrb:sample-curve bnd spacing)
    )
  )

  (setq prevTxtAng nil)
  (foreach pt ptlist
    (setq perp  (vlax-curve-getClosestPointTo trk pt))
    (setq param (vlax-curve-getParamAtPoint trk perp))

    (if (and param (> param 0.001) (< param (- endParam 0.001)))
      (progn
        (setq dist (distance pt perp))
        (setq dir  (angle pt perp))

        (setq txtpt (polar pt dir offset))
        (setq distRounded (vrb:roundval dist 2))

        (setq txtang
          (angle perp
            (vlax-curve-getPointAtParam trk (min (+ param 0.01) endParam))))
        (if (> (abs (- txtang trackDir)) (/ pi 2))
          (setq txtang (+ txtang pi))
        )
        (if prevTxtAng
          (progn
            (while (> (- txtang prevTxtAng) pi)
              (setq txtang (- txtang (* 2 pi))))
            (while (< (- txtang prevTxtAng) (- pi))
              (setq txtang (+ txtang (* 2 pi))))
          )
        )
        (setq prevTxtAng txtang)

        ;; label text
        (setq txtstr (strcat "{\\pxql;" (rtos distRounded 2 2) "m"))
        (if useKM
          (progn
            (setq chainDist (vlax-curve-getDistAtParam trk param))
            (setq totalDist (+ chainDist startM (* startKM 1000)))
            (setq totalDistRounded (vrb:roundval totalDist 2))
            (setq km (fix (/ totalDistRounded 1000)))
            (setq m  (vrb:roundval (- totalDistRounded (* km 1000)) 0))
            (if (>= m 1000) (setq km (1+ km) m (- m 1000)))
            (setq kmStr (itoa km))
            (setq mStr
              (cond
                ((< m 10)  (strcat "00" (itoa (fix m))))
                ((< m 100) (strcat "0"  (itoa (fix m))))
                (T         (itoa (fix m)))
              )
            )
            (setq txtstr (strcat txtstr "@\\PKM " kmStr "/" mStr))
          )
        )
        (setq txtstr (strcat txtstr "}"))

        (entmake
          (list
            '(0 . "MTEXT")
            '(100 . "AcDbEntity")
            '(8 . "1-boundary")
            '(100 . "AcDbMText")
            (cons 7 stylename)
            (cons 10 txtpt)
            (cons 40 txth)
            (cons 41 11.00)
            (cons 1 txtstr)
            (cons 50 txtang)
            (cons 71 5)
            '(73 . 1)
          )
        )
        (setq count (1+ count))
      )
    )
  )
  count
)

;; ---- command -------------------------------------------------------------------
(defun c:VIDRWBOUNDARY (/ *error* kmv n undoOpen)
  (setq *vrb-bnd* nil *vrb-trk* nil)   ; selections are not remembered
  (defun *error* (msg)
    (if (> (getvar "CMDACTIVE") 0) (command))
    (if undoOpen (command "_.UNDO" "_End"))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nVIDRWBOUNDARY error: " msg)))
    (princ)
  )

  (if (= (vrb:dialog) 1)
    (progn
      (setq kmv (if (= *vrb-usekm* "1") (vrb:parse-km *vrb-km*)))
      (command "_.UNDO" "_Begin")
      (setq undoOpen T)
      (setq n
        (vrb:generate
          *vrb-bnd* *vrb-trk* *vrb-mode*
          (atof *vrb-dist*)
          (= *vrb-usekm* "1")
          (if kmv (car kmv) 0)
          (if kmv (cadr kmv) 0.0)
        )
      )
      (command "_.UNDO" "_End")
      (setq undoOpen nil)
      (princ (strcat "\n" (itoa n) " label(s) added."))
    )
    (princ "\nVIDRWBOUNDARY cancelled.")
  )
  (princ)
)

(princ "\nType VIDRWBOUNDARY to run the command.")
(princ)
