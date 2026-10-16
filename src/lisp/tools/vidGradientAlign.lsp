;;; ------------------------------------------------------------------
;;; VIDGRADIENTALIGN
;;; Copy selected blocks into a Row x Column grid with even spacing.
;;; Works in AutoCAD and AutoCAD Civil 3D.
;;;
;;; Prompts (in order):
;;;   Even space : distance between blocks (default 45)
;;;   Row        : number of rows
;;;   Column     : number of columns
;;;   Pick point : insertion point of the FIRST block (top-left of grid)
;;;   Select     : blocks to copy
;;;
;;; Grid fills left -> right, top -> bottom (along the current UCS).
;;; Originals stay in place. Every copy gets Rotation = 0.
;;; ------------------------------------------------------------------

(defun c:VIDGRADIENTALIGN (/ *error* VID:GetIP doc ss i en ip data
                        spacing rows cols n cap byY final chunk k
                        pt idx item oldpt cell w newpt obj cp)

  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (prompt (strcat "\nError: " msg))
    )
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))
    (princ)
  )

  (defun VID:GetIP (en / p)
    (setq p (cdr (assoc 10 (entget en))))
    (if (and (listp p) (numberp (car p)) (numberp (cadr p)))
      (list (car p) (cadr p) (if (numberp (caddr p)) (caddr p) 0.0))
      nil
    )
  )

  ;; ---- Inputs ----
  (initget 6)
  (setq spacing (getreal "\nEven space <45>: "))
  (if (null spacing) (setq spacing 45.0))

  (initget 7)
  (setq rows (getint "\nRow: "))
  (initget 7)
  (setq cols (getint "\nColumn: "))

  (initget 1)
  (setq pt (getpoint "\nPick point (first block, top-left of grid): "))

  (prompt "\nSelect blocks to copy: ")
  (setq ss (ssget '((0 . "INSERT"))))

  (cond
    ((null ss)
     (prompt "\nNo blocks selected."))

    (T
     (setq data nil i 0)
     (repeat (sslength ss)
       (setq en (ssname ss i)
             ip (VID:GetIP en))
       (if ip (setq data (cons (list en ip) data)))
       (setq i (1+ i))
     )

     (setq n   (length data)
           cap (* rows cols))

     (cond
       ((< n 1)
        (prompt "\nCould not read any block insertion points."))

       ((> n cap)
        (prompt
          (strcat "\n" (itoa n) " blocks selected but grid only holds "
                  (itoa cap) " (" (itoa rows) " x " (itoa cols)
                  "). Increase Row/Column or select fewer blocks.")))

       (T
        ;; Order: top -> bottom, split into rows of <cols>, each row left -> right
        (setq byY (vl-sort data
                    (function (lambda (a b)
                                (> (cadr (cadr a)) (cadr (cadr b)))))))
        (setq final nil)
        (while byY
          (setq chunk nil k 0)
          (while (and byY (< k cols))
            (setq chunk (cons (car byY) chunk)
                  byY   (cdr byY)
                  k     (1+ k))
          )
          (setq final
            (append final
              (vl-sort chunk
                (function (lambda (a b)
                            (< (car (cadr a)) (car (cadr b))))))))
        )

        (vla-StartUndoMark doc)
        (setq idx 0)
        (foreach item final
          (setq en    (car item)
                oldpt (cadr item)
                ;; grid cell in UCS, then convert to WCS
                cell  (list (+ (car pt)  (* (rem idx cols) spacing))
                            (- (cadr pt) (* (fix (/ idx cols)) spacing))
                            (caddr pt))
                w     (trans cell 1 0)
                newpt (list (car w) (cadr w) (caddr oldpt)))

          (setq obj (vlax-ename->vla-object en)
                cp  (vla-Copy obj))
          (vla-Move cp (vlax-3d-point oldpt) (vlax-3d-point newpt))
          (vla-put-Rotation cp 0.0)
          (setq idx (1+ idx))
        )
        (vla-EndUndoMark doc)

        (prompt
          (strcat "\nDone: " (itoa n) " blocks copied (rotation 0) into "
                  (itoa rows) " row(s) x " (itoa cols) " column(s), spacing "
                  (rtos spacing 2 3) "."))
       )
     )
    )
  )
  (princ)
)

(prompt "\nVIDGRADIENTALIGN loaded. Type VIDGRADIENTALIGN to run.")
(princ)
