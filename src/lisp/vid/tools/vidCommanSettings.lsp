;;; ============================================================================
;;; VIDCOMMANSETTINGS
;;; Command : VIDCOMMANSETTINGS
;;; ----------------------------------------------------------------------------
;;; Common drawing settings.
;;;   - UCSFOLLOW = 1   (UCS follow: view automatically changes to the plan
;;;                      view of the current UCS)
;;;   - GRIDMODE  = 1   (grid ON,  F7)
;;;   - OSMODE    = 16383 (object snap ON (F3) + ALL object snap modes ticked)
;;; Add more settings inside the command as needed.
;;; ============================================================================

(vl-load-com)

;; Set a system variable safely and report the result.
(defun VIDComman:SetVar (name value / res)
  (setq res (vl-catch-all-apply 'setvar (list name value)))
  (if (vl-catch-all-error-p res)
    (princ (strcat "\n  " name ": could not be set."))
    (princ (strcat "\n  " name " = " (vl-princ-to-string (getvar name))))
  )
)

(defun c:VIDCOMMANSETTINGS (/ oldEcho)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (princ "\nVIDCOMMANSETTINGS:")
  (VIDComman:SetVar "UCSFOLLOW" 1)
  (VIDComman:SetVar "GRIDMODE"  1)      ; enable grid
  ;; Object snap: bit 16384 clear = object snap ON (F3);
  ;; 16383 = every object snap mode turned on
  (VIDComman:SetVar "OSMODE" 16383)

  (setvar "CMDECHO" oldEcho)
  (princ "\nVIDCOMMANSETTINGS done.")
  (princ)
)

(princ "\n[OK] VIDCOMMANSETTINGS loaded. Type VIDCOMMANSETTINGS to run.")
(princ)
