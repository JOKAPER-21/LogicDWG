;;; ------------------------------------------------------------------
;;; VIDROTTEXT
;;; Rotate TEXT and MTEXT. Works in AutoCAD and Civil 3D.
;;;
;;;   Rotation angle : degrees to ADD to each object's current rotation
;;;                    (default 180, last value is remembered)
;;;   Select         : any number of TEXT and MTEXT objects, mixed;
;;;                    other object types in the selection are ignored
;;;
;;; One undo (U) reverts the whole command.
;;; ------------------------------------------------------------------

(vl-load-com)

;; keep an angle inside 0 .. 2*pi
(defun VID:RotText-Norm (a)
  (setq a (rem a (* 2.0 pi)))
  (if (< a 0.0) (+ a (* 2.0 pi)) a)
)

;; whole numbers show as integers (180), others as decimals (22.5)
(defun VID:RotText-Fmt (v / s)
  (cond
    ((equal v (float (fix v)) 1e-9) (itoa (fix v)))
    (T
     (setq s (rtos v 2 6))
     (while (and (wcmatch s "*.*") (= (substr s (strlen s)) "0"))
       (setq s (substr s 1 (1- (strlen s)))))
     s)
  )
)

(defun c:VIDROTTEXT (/ *error* doc ss i obj res rot radd cnt skip)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (prompt (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; rotation value (degrees) - default 180 / last used
  (setq *VIDROT-ANG* (cond (*VIDROT-ANG*) (T 180.0)))
  (setq rot
    (getreal (strcat "\nRotation angle to add in degrees <"
                     (VID:RotText-Fmt *VIDROT-ANG*) ">: ")))
  (if (null rot) (setq rot *VIDROT-ANG*))
  (setq *VIDROT-ANG* rot)

  ;; TEXT + MTEXT, any number of objects
  (prompt "\nSelect TEXT / MTEXT objects: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  (cond
    ((null ss)
     (prompt "\nNo TEXT or MTEXT selected."))
    (T
     (setq radd (* pi (/ rot 180.0))
           cnt  0
           skip 0
           i    0)
     (vla-StartUndoMark doc)
     (repeat (sslength ss)
       (setq obj (vlax-ename->vla-object (ssname ss i)))
       (setq res
         (vl-catch-all-apply
           '(lambda ()
              (vla-put-Rotation obj
                (VID:RotText-Norm (+ (vla-get-Rotation obj) radd))))
           nil))
       (if (vl-catch-all-error-p res)
         (setq skip (1+ skip))
         (setq cnt (1+ cnt)))
       (setq i (1+ i))
     )
     (vla-EndUndoMark doc)
     (prompt (strcat "\nDone. Rotated " (itoa cnt) " object(s) by "
                     (VID:RotText-Fmt rot) " degrees."))
     (if (> skip 0)
       (prompt (strcat "\n" (itoa skip)
                       " object(s) skipped (locked layer or read-only).")))
    )
  )
  (princ)
)

(prompt "\nvidRotText loaded. Type VIDROTTEXT to run.")
(princ)
