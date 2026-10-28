;;; ------------------------------------------------------------
;;; VIDDIMMTOFT - Dimension text: meters -> feet & inches
;;; Format  : #' ##"   ##' ##"   ###' ##"   (whole numbers only)
;;; Options : Prefix / Suffix / Override / Reset
;;; Works on aligned (and rotated/linear) dimensions
;;; Assumes drawing units = meters
;;; ------------------------------------------------------------
(vl-load-com)

;; Meters -> feet' inches"  (3.92 -> 12' 10", 1.53 -> 5' 00")
;; Rounded to whole inches, inches always 2 digits, 12" carries into feet.
(defun vid:ftin (m / tin ft inch istr)
  (setq tin  (fix (+ (/ (abs m) 0.0254) 0.5))   ; total inches, rounded
        ft   (/ tin 12)                          ; integer division
        inch (rem tin 12)
        istr (if (< inch 10)
               (strcat "0" (itoa inch))
               (itoa inch)))
  (strcat (if (< m 0) "-" "")
          (itoa ft) "' "
          istr "\""))

(defun c:VIDDIMMTOFT (/ opt ss i ent ed obj m ftxt cnt)
  (initget "Prefix Suffix Override Reset")
  (setq opt (getkword "\nChoose option [Prefix/Suffix/Override/Reset] <Override>: "))
  (if (null opt) (setq opt "Override"))

  (prompt "\nSelect aligned dimensions: ")
  (setq ss (ssget '((0 . "DIMENSION"))))
  (if (null ss)
    (prompt "\nNothing selected.")
    (progn
      (setq i 0 cnt 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              ed  (entget ent)
              i   (1+ i))
        ;; dim type 0 = rotated/linear, 1 = aligned
        (if (member (logand 7 (cdr (assoc 70 ed))) '(0 1))
          (progn
            (setq obj  (vlax-ename->vla-object ent)
                  m    (vla-get-measurement obj)
                  ftxt (vid:ftin m))
            (cond
              ((= opt "Prefix")   ; feet on top / meter on bottom
               (vla-put-primaryunitsprecision obj 0)   ; meter value = whole number
               (vla-put-textoverride obj (strcat ftxt "\\X<>")))
              ((= opt "Suffix")   ; meter on top / feet on bottom
               (vla-put-primaryunitsprecision obj 0)   ; meter value = whole number
               (vla-put-textoverride obj (strcat "<>\\X" ftxt)))
              ((= opt "Override") ; feet replaces meter
               (vla-put-textoverride obj ftxt))
              ((= opt "Reset")    ; back to measured value
               (vla-put-textoverride obj "")))
            (setq cnt (1+ cnt)))))
      (prompt (strcat "\n" (itoa cnt) " dimension(s) updated (" opt ")."))))
  (princ))

(princ "\nVIDDIMMTOFT loaded. Type VIDDIMMTOFT to run.")
(princ)