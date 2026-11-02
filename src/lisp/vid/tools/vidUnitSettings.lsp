;;; ================== USER SETTINGS (edit here) =====================
(defun vid:settings ()
  (list
    ;; Length type: 1=Scientific 2=Decimal 3=Engineering 4=Architectural 5=Fractional
    (cons "LUNITS" 2)
    ;; Length precision (decimal places): 2 = 0.00
    (cons "LUPREC" 2)
    ;; Angle type: 0=Decimal deg 1=Deg/Min/Sec 2=Grads 3=Radians 4=Surveyor
    (cons "AUNITS" 1)
    ;; Angle precision: set the AUPREC number directly, or use nil to
    ;; auto-detect the value that shows 0d00' (only for AUNITS 1)
    (cons "AUPREC" nil)
    ;; Angle direction: 0 = counter-clockwise (clockwise disabled), 1 = clockwise
    (cons "ANGDIR" 0)
    ;; Angle base direction: 0 = East (0 deg), see ANGBASE; nil = leave unchanged
    (cons "ANGBASE" 0)
    ;; Unit scale (insertion units): 0=Unitless 1=Inches 2=Feet 4=Millimeters
    ;; 5=Centimeters 6=Meters 7=Kilometers
    (cons "INSUNITS" 6)
    ;; Lighting units: 0=Generic 1=American 2=International
    (cons "LIGHTINGUNITS" 2)
  )
)
;;; ================== END USER SETTINGS =============================

;; Safe setvar: returns T on success, NIL if var missing/read-only/failed
(defun vid:safe-setvar (var val / r)
  (if (getvar var)
    (progn
      (setq r (vl-catch-all-apply 'setvar (list var val)))
      (not (vl-catch-all-error-p r))
    )
    nil
  )
)

;; Find the AUPREC value that displays as 0d00' in Deg/Min/Sec mode
(defun vid:dms-prec-min (/ p found)
  (setq p 0)
  (while (and (not found) (<= p 4))
    (if (= (angtos 0.0 1 p) "0d00'")
      (setq found p)
    )
    (setq p (1+ p))
  )
  (if found found 2)
)

(defun c:VIDUNITSETTINGS (/ cmde cfg item var val ok fails)
  (setq cmde (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq cfg (vid:settings))

  ;; Insertion units: also set the insert source/target defaults
  (if (assoc "INSUNITS" cfg)
    (progn
      (vid:safe-setvar "INSUNITSDEFSOURCE" (cdr (assoc "INSUNITS" cfg)))
      (vid:safe-setvar "INSUNITSDEFTARGET" (cdr (assoc "INSUNITS" cfg)))
    )
  )

  (princ "\n---- VIDSETUP ----")
  (foreach item cfg
    (setq var (car item) val (cdr item))
    ;; AUPREC nil = auto-detect 0d00'
    (if (and (= var "AUPREC") (null val))
      (setq val (vid:dms-prec-min))
    )
    (if val
      (progn
        (setq ok (vid:safe-setvar var val))
        (princ (strcat "\n" var " = " (vl-princ-to-string val)
                       (if ok "  [OK]" "  [SKIPPED - not supported/failed]")))
      )
    )
  )

  (setvar "CMDECHO" cmde)
  (princ "\nUnits setup complete.\n")
  (princ)
)

(princ "\nVidSetup loaded. Type VIDSETUP to run.")
(princ)
