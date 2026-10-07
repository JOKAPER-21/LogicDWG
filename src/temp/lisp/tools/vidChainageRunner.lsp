;;; ============================================================================
;;; Vid Chainage Runner
;;; Command : VIDCHAINAGERUNNER
;;; Version : v2
;;;   v2: * Select ONE OR MORE polylines first.
;;;       * Dialog:  ( ) Forward  ( ) Backward
;;;                  KM: [   ] / [   ]        (KM / metre)
;;;                  Increment: [   ]
;;;                  [Cancel] [Ok]
;;;         Tab order = Forward, Backward, KM, metre, Increment, Cancel, Ok.
;;;       * Every selected polyline is run separately with the same KM / metre
;;;         start value.
;;;   Forward : KM / metre is the value at the START of the polyline.
;;;   Backward: KM / metre is the value at the END of the polyline; chainage
;;;             decreases as the run walks from the end back to the start.
;;; ============================================================================

(vl-load-com)

;; Last used values (remembered between runs)
(if (null VCR-Dir)  (setq VCR-Dir  "Forward"))
(if (null VCR-Km)   (setq VCR-Km   0))
(if (null VCR-M)    (setq VCR-M    0))
(if (null VCR-Step) (setq VCR-Step 100))

(setq VCR-OldError nil)
(setq VCR-OldLay   nil)
(setq VCR-OldEcho  nil)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------
(defun VCR-IntP (s)
  (and s
       (/= (vl-string-trim " " s) "")
       (= (vl-string-trim "0123456789" (vl-string-trim " " s)) "")
  )
)

;; Called by the OK button: validates the tiles, stores them, returns T / nil
(defun VCR-Validate (/ k m st)
  (setq k  (get_tile "edt_km")
        m  (get_tile "edt_m")
        st (get_tile "edt_inc"))
  (cond
    ((not (VCR-IntP k))
     (alert "KM must be a whole number (0 or more).")
     (mode_tile "edt_km" 2) nil)
    ((not (VCR-IntP m))
     (alert "Metre must be a whole number between 0 and 999.")
     (mode_tile "edt_m" 2) nil)
    ((> (atoi (vl-string-trim " " m)) 999)
     (alert "Metre must be between 0 and 999.")
     (mode_tile "edt_m" 2) nil)
    ((or (not (VCR-IntP st)) (< (atoi (vl-string-trim " " st)) 1))
     (alert "Increment must be a whole number (1 or more).")
     (mode_tile "edt_inc" 2) nil)
    (T
     (setq VCR-Km   (atoi (vl-string-trim " " k))
           VCR-M    (atoi (vl-string-trim " " m))
           VCR-Step (atoi (vl-string-trim " " st))
           VCR-Dir  (if (= (get_tile "rb_bwd") "1") "Backward" "Forward"))
     T)
  )
)

;; Dialog.  Returns T = Ok, nil = Cancel.
(defun VCR-Dialog (/ dlgName f dcl_id ret)
  (setq dlgName (vl-filename-mktemp "vcrUi" (getvar "TEMPPREFIX") ".dcl"))
  (setq f (open dlgName "w"))
  (write-line
    (strcat
      "vcr_ui : dialog {\n"
      "  label = \"Chainage Runner\";\n"
      "  : boxed_radio_row {\n"
      "    label = \"Direction\";\n"
      "    : radio_button { key = \"rb_fwd\"; label = \"Forward\";  value = \"1\"; }\n"
      "    : radio_button { key = \"rb_bwd\"; label = \"Backward\"; value = \"0\"; }\n"
      "  }\n"
      "  : row {\n"
      "    : edit_box { key = \"edt_km\"; label = \"KM:\"; edit_width = 8; }\n"
      "    : text { label = \"/\"; width = 2; fixed_width = true; }\n"
      "    : edit_box { key = \"edt_m\"; label = \"\"; edit_width = 6; }\n"
      "  }\n"
      "  : edit_box { key = \"edt_inc\"; label = \"Increment:\"; edit_width = 8; }\n"
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
  (if (and dcl_id (new_dialog "vcr_ui" dcl_id))
    (progn
      (set_tile "rb_fwd" (if (= VCR-Dir "Backward") "0" "1"))
      (set_tile "rb_bwd" (if (= VCR-Dir "Backward") "1" "0"))
      (set_tile "edt_km"  (itoa VCR-Km))
      (set_tile "edt_m"   (itoa VCR-M))
      (set_tile "edt_inc" (itoa VCR-Step))
      (mode_tile "edt_km" 2)
      (action_tile "accept" "(if (VCR-Validate) (done_dialog 1))")
      (action_tile "cancel" "(done_dialog 0)")
      (setq ret (= (start_dialog) 1))
    )
    (princ "\nVIDCHAINAGERUNNER: could not open dialog.")
  )
  (if dcl_id (unload_dialog dcl_id))
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  ret
)

(defun VCR-Error (msg)
  (if VCR-OldLay  (setvar "CLAYER" VCR-OldLay))
  (if VCR-OldEcho (setvar "CMDECHO" VCR-OldEcho))
  (setq *error* VCR-OldError)
  (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
    (princ (strcat "\nVIDCHAINAGERUNNER ERROR: " msg))
    (princ "\nVIDCHAINAGERUNNER cancelled.")
  )
  (princ)
)

;; Runs the chainage along ONE polyline entity.
(defun VCR-RunOne (ent km m step dir / ms len dist pt param deriv ang
                   txt txtPt circ hatch loop mtx offset sideVec
                   endPt endParam endDeriv endAng totTxt totPt totSide
                   totMtx chStep count)
  (setq ms     (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq offset 1.5)
  (setq chStep (if (= dir "Forward") step (- step)))
  (setq len    (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
  (setq dist   (if (= dir "Forward") 0 len))
  (setq count  0)

  (while (and (>= dist 0) (<= dist len))
    (setq pt    (vlax-curve-getPointAtDist ent dist))
    (setq param (vlax-curve-getParamAtDist ent dist))
    (setq deriv (vlax-curve-getFirstDeriv ent param))
    (setq ang   (angle '(0 0 0) deriv))

    ;; circle + solid hatch
    (setq circ (vla-AddCircle ms (vlax-3d-point pt) 0.30))
    (vla-put-Layer circ "1-CHAINAGE")
    (setq hatch (vla-AddHatch ms acHatchPatternTypePreDefined "SOLID" :vlax-true))
    (vla-put-Layer hatch "1-CHAINAGE")
    (setq loop (vlax-make-safearray vlax-vbObject '(0 . 0)))
    (vlax-safearray-put-element loop 0 circ)
    (vla-AppendOuterLoop hatch loop)
    (vla-put-Color hatch 7)
    (vla-Evaluate hatch)

    ;; text on the right-hand side of the travel direction
    (setq sideVec (list (cadr deriv) (- (car deriv)) 0))
    (setq txtPt   (polar pt (angle '(0 0 0) sideVec) offset))
    (setq txt
      (strcat (itoa km) "/"
              (substr (strcat "000" (itoa m))
                      (- (strlen (strcat "000" (itoa m))) 2))))
    (setq mtx (vla-AddMText ms (vlax-3d-point txtPt) 10 txt))
    (vla-put-AttachmentPoint mtx acAttachmentPointMiddleCenter)
    (vla-put-InsertionPoint mtx (vlax-3d-point txtPt))
    (vla-put-Rotation mtx ang)
    (vla-put-Layer mtx "1-CHAINAGE")
    (vla-put-Height mtx 2)
    (setq count (1+ count))

    ;; next chainage value (with KM roll-over / roll-under)
    (setq m (+ m chStep))
    (if (= dir "Forward")
      (while (>= m 1000) (setq km (1+ km) m (- m 1000)))
      (while (< m 0)     (setq km (1- km) m (+ m 1000)))
    )
    (setq dist (if (= dir "Forward") (+ dist step) (- dist step)))
  )

  ;; total length text at the last vertex
  (setq endParam (vlax-curve-getEndParam ent))
  (setq endPt    (vlax-curve-getEndPoint ent))
  (setq endDeriv (vlax-curve-getFirstDeriv ent endParam))
  (setq endAng   (angle '(0 0 0) endDeriv))
  (setq totSide  (list (cadr endDeriv) (- (car endDeriv)) 0))
  (setq totPt    (polar endPt (angle '(0 0 0) totSide) offset))
  (setq totTxt   (strcat "Total Length = " (rtos len 2 2) " m"))
  (setq totMtx   (vla-AddMText ms (vlax-3d-point totPt) 20 totTxt))
  (vla-put-AttachmentPoint totMtx acAttachmentPointMiddleCenter)
  (vla-put-InsertionPoint totMtx (vlax-3d-point totPt))
  (vla-put-Rotation totMtx endAng)
  (vla-put-Layer totMtx "1-CHAINAGE")
  (vla-put-Height totMtx 2)
  count
)

;; ---------------------------------------------------------------------------
;; Command
;; ---------------------------------------------------------------------------
(defun c:VIDCHAINAGERUNNER (/ ss i ent n marks total)
  (vl-load-com)
  (setq VCR-OldError *error*)
  (setq *error* VCR-Error)
  (setq VCR-OldLay  (getvar "CLAYER"))
  (setq VCR-OldEcho (getvar "CMDECHO"))

  (princ "\nSelect polyline(s) (chainage runs in the increasing-chainage direction of each): ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))

  (cond
    ((null ss)
     (princ "\nSelect at least one valid polyline."))
    ((not (VCR-Dialog))
     (princ "\nVIDCHAINAGERUNNER cancelled."))
    (T
     (setvar "CMDECHO" 0)
     (if (not (tblsearch "LAYER" "1-CHAINAGE"))
       (command "_.LAYER" "_M" "1-CHAINAGE" "")
     )
     (setvar "CLAYER" VCR-OldLay)

     (setq i 0 total 0)
     (while (< i (sslength ss))
       (setq ent (ssname ss i))
       (setq n (VCR-RunOne ent VCR-Km VCR-M VCR-Step VCR-Dir))
       (setq total (+ total n))
       (setq i (1+ i))
     )
     (setvar "CLAYER"  VCR-OldLay)
     (setvar "CMDECHO" VCR-OldEcho)
     (princ (strcat "\nVIDCHAINAGERUNNER completed: "
                    (itoa (sslength ss)) " polyline(s), "
                    (itoa total) " chainage mark(s), "
                    VCR-Dir " from "
                    (itoa VCR-Km) "/" (itoa VCR-M)
                    ", increment " (itoa VCR-Step) "."))
    )
  )
  (setq *error* VCR-OldError)
  (princ)
)

(princ "\nVIDCHAINAGERUNNER loaded. Type VIDCHAINAGERUNNER to run.")
(princ)
