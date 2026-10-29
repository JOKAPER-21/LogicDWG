(defun c:VIDTR ( / pt1 pt2 pt3
                 dx dy angle text-angle
                 length width
                 ux uy px py
                 c1 c2 c3 c4
                 cx cy
                 old-clayer )

  ;; Save current layer
  (setq old-clayer (getvar "CLAYER"))

  ;; --- Create layer "1-TR" if it doesn't exist ---
  (if (null (tblsearch "LAYER" "1-TR"))
    (command "._-LAYER" "MAKE" "1-TR" "COLOR" "7" "1-TR" "")
  )

  (setvar "CLAYER" "1-TR")

  ;; ---- Get 3 points ----
  (setq pt1 (getpoint "\nPoint 1 click pannunga (Start corner): "))
  (if (null pt1) (progn (setvar "CLAYER" old-clayer) (exit)))

  (setq pt2 (getpoint pt1 "\nPoint 2 click pannunga (Direction + Length): "))
  (if (null pt2) (progn (setvar "CLAYER" old-clayer) (exit)))

  (setq pt3 (getpoint pt2 "\nPoint 3 click pannunga (Height side): "))
  (if (null pt3) (progn (setvar "CLAYER" old-clayer) (exit)))

  ;; ---- Angle from P1 to P2 ----
  (setq dx (- (car pt2)  (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq length (sqrt (+ (* dx dx) (* dy dy))))

  ;; Unit vector along P1->P2
  (setq ux (/ dx length))
  (setq uy (/ dy length))

  ;; Perpendicular unit vector (90 deg CCW)
  (setq px (- uy))
  (setq py ux)

  ;; ---- Height from P3 perpendicular projection ----
  (setq dx (- (car pt3)  (car pt1)))
  (setq dy (- (cadr pt3) (cadr pt1)))
  (setq width (+ (* dx px) (* dy py)))

  ;; ---- 4 corners ----
  (setq c1 pt1)
  (setq c2 pt2)
  (setq c3 (list (+ (car pt2) (* width px))
                 (+ (cadr pt2) (* width py))
                 0.0))
  (setq c4 (list (+ (car pt1) (* width px))
                 (+ (cadr pt1) (* width py))
                 0.0))

  ;; ---- Draw LWPOLYLINE ----
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 "1-TR")
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)
      (cons 10 (list (car c1) (cadr c1)))
      (cons 10 (list (car c2) (cadr c2)))
      (cons 10 (list (car c3) (cadr c3)))
      (cons 10 (list (car c4) (cadr c4)))
    )
  )

  ;; ---- Center ----
  (setq cx (/ (+ (car c1) (car c2) (car c3) (car c4)) 4.0))
  (setq cy (/ (+ (cadr c1) (cadr c2) (cadr c3) (cadr c4)) 4.0))

  ;; ---- Rectangle angle (radians) ----
  (setq angle (atan uy ux))

  ;; ---- MText angle = Rectangle angle + 180 degrees ----
  (setq text-angle (+ angle pi))   ;; pi = 180 degrees in radians

  ;; ---- Place TR as MTEXT ----
  (entmake
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 "1-TR")
      '(100 . "AcDbMText")
      (cons 10 (list cx cy 0.0))
      (cons 40 2.0)                 ;; text height
      (cons 41 0.0)                 ;; reference width 0 = no wrapping
      (cons 71 5)                   ;; attachment: Middle Center
      (cons 72 1)                   ;; drawing direction: left to right
      (cons 1 "TR")
      (cons 50 text-angle)          ;; angle + 180 deg
    )
  )

  (setvar "CLAYER" old-clayer)

  (princ "\n[OK] VIDTR done! MText angle = Rectangle angle + 180 deg")
  (princ)
)

(princ "\n[VIDTR Loaded] Command: VIDTR")
(princ "\nP1=Start | P2=Direction | P3=Height | MText = angle+180deg")
(princ)
