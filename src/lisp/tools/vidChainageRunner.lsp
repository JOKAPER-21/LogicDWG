(defun c:VIDCHAINAGERUNNER (/ ent obj len dist step km m pt param deriv ang
                   txt txtPt doc ms circ hatch loop mtx oldLay
                   offset sideVec endPt endParam endDeriv endAng
                   totTxt totPt totSide totMtx dir chStep)
  (vl-load-com)
  ;; Select polyline
  (setq ent (car (entsel "\nSelect polyline (increasing chainage direction): ")))
  (if (and ent (member (cdr (assoc 0 (entget ent))) '("LWPOLYLINE" "POLYLINE")))
    (progn
      (setq obj (vlax-ename->vla-object ent))

      ;; ---- Ask direction FIRST ----
      (initget 1 "Forward Backward F B")
      (setq dir (getkword "\nRunning chainage direction [Forward/Backward]: "))
      (if (or (= dir "F") (= dir "Forward"))
        (setq dir "Forward")
        (setq dir "Backward")
      )

      ;; Inputs
      (setq km (getint "\nEnter KM: "))
      (setq m  (getint "\nEnter Meter: "))
      (setq step (getint "\nEnter chainage increment <100>: "))
      (if (not step) (setq step 100))

      ;; chStep is signed: +step for Forward, -step for Backward
      (setq chStep (if (= dir "Forward") step (- step)))

      ;; Settings
      (setq offset 1.5)

      ;; Ensure CHAINAGE layer
      (if (not (tblsearch "LAYER" "1-CHAINAGE"))
        (command "_.LAYER" "_M" "1-CHAINAGE" "")
      )

      ;; Doc
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq ms  (vla-get-ModelSpace doc))

      ;; Store layer
      (setq oldLay (getvar "CLAYER"))

      ;; Length
      (setq len (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))

      ;; ---- Set starting distance based on direction ----
      ;; Forward: entered km/m is the value AT THE START of the polyline (dist=0),
      ;;          increasing as we travel toward the end.
      ;; Backward: entered km/m is the value AT THE END of the polyline (dist=len),
      ;;          and we walk from the end back toward the start, decreasing km/m
      ;;          as we go - so along the polyline (start->end) values read low->high.
      (setq dist (if (= dir "Forward") 0 len))

      (while (and (>= dist 0) (<= dist len))
        ;; Point
        (setq pt (vlax-curve-getPointAtDist ent dist))
        ;; Tangent
        (setq param (vlax-curve-getParamAtDist ent dist))
        (setq deriv (vlax-curve-getFirstDeriv ent param))
        ;; Angle (for text rotation)
        (setq ang (angle '(0 0 0) deriv))

        ;; ---- Circle ----
        (setq circ (vla-AddCircle ms (vlax-3d-point pt) 0.30))
        (vla-put-Layer circ "1-CHAINAGE")

        ;; ---- Hatch ----
        (setq hatch (vla-AddHatch ms acHatchPatternTypePreDefined "SOLID" :vlax-true))
        (vla-put-Layer hatch "1-CHAINAGE")
        (setq loop (vlax-make-safearray vlax-vbObject '(0 . 0)))
        (vlax-safearray-put-element loop 0 circ)
        (vla-AppendOuterLoop hatch loop)
        (vla-put-Color hatch 7)
        (vla-Evaluate hatch)

        ;; ---- RHS vector (dy, -dx) ----
        (setq sideVec (list (cadr deriv) (- (car deriv)) 0))
        ;; Text point
        (setq txtPt (polar pt (angle '(0 0 0) sideVec) offset))

        ;; ---- Chainage format ----
        (setq txt
          (strcat
            (itoa km) "/"
            (substr (strcat "000" (itoa m))
                    (- (strlen (strcat "000" (itoa m))) 2))
          )
        )

        ;; ---- MText ----
        (setq mtx (vla-AddMText ms (vlax-3d-point txtPt) 10 txt))
        (vla-put-AttachmentPoint mtx acAttachmentPointMiddleCenter)
        (vla-put-InsertionPoint mtx (vlax-3d-point txtPt))
        (vla-put-Rotation mtx ang)
        (vla-put-Layer mtx "1-CHAINAGE")
        (vla-put-Height mtx 2)

        ;; ---- Increment/Decrement chainage with KM rollover/rollunder ----
        (setq m (+ m chStep))
        (if (= dir "Forward")
          (while (>= m 1000)
            (setq km (1+ km))
            (setq m (- m 1000))
          )
          (while (< m 0)
            (setq km (1- km))
            (setq m (+ m 1000))
          )
        )

        ;; ---- Advance/retreat along the polyline ----
        (setq dist (if (= dir "Forward") (+ dist step) (- dist step)))
      )

      ;; ---- Total length text at final polyline vertex ----
      (setq endParam (vlax-curve-getEndParam ent))
      (setq endPt    (vlax-curve-getEndPoint ent))
      (setq endDeriv (vlax-curve-getFirstDeriv ent endParam))
      (setq endAng   (angle '(0 0 0) endDeriv))
      (setq totSide  (list (cadr endDeriv) (- (car endDeriv)) 0))
      (setq totPt    (polar endPt (angle '(0 0 0) totSide) offset))

      (setq totTxt (strcat "Total Length = " (rtos len 2 2) " m"))

      (setq totMtx (vla-AddMText ms (vlax-3d-point totPt) 20 totTxt))
      (vla-put-AttachmentPoint totMtx acAttachmentPointMiddleCenter)
      (vla-put-InsertionPoint totMtx (vlax-3d-point totPt))
      (vla-put-Rotation totMtx endAng)
      (vla-put-Layer totMtx "1-CHAINAGE")
      (vla-put-Height totMtx 2)

      (setvar "CLAYER" oldLay)
      (princ (strcat "\n✅ VIDCHAINAGERUNNER completed (" dir " chainage, RHS of travel direction)."))
    )
    (princ "\n❌ Select valid polyline.")
  )
  (princ)
)