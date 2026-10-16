;;; ------------------------------------------------------------------
;;; VIDGRADIENTALIGN
;;; Copy selected blocks into a staggered grid. Works in AutoCAD and
;;; AutoCAD Civil 3D.
;;;
;;; Dialog fields (every field accepts 0):
;;;   Horizontal space : X distance between blocks   b1 --H-- b2
;;;   Vertical space   : Y distance between blocks
;;;   Row count        : number of ROWS    (blocks per column)
;;;   Column count     : number of COLUMNS (blocks per row)
;;;                      0 = no limit in that direction
;;;   Row offset       : X shift applied to offset rows
;;;   Column offset    : Y shift, each next column drops by this much
;;;   Offset step      : offset cycle length. Row r (0-based, inside one
;;;                      Row count x Column count set) is shifted by
;;;                      (r mod step) x Row offset.
;;;                      step 2 -> 0, 1, 0, 1 ...   step 3 -> 0, 1, 2, 0, 1, 2 ...
;;;                      0 = no row offset
;;;   Direction        : Vertical   = fill down each column, then next column
;;;                      Horizontal = fill across each row, then next row
;;;
;;; When Row count x Column count blocks are placed, the next set starts
;;; directly below the previous one.
;;;
;;; Blocks are ordered left -> right (ties top -> bottom) by insertion
;;; point = b1, b2, b3 ...  Originals stay; every copy gets Rotation = 0.
;;; ------------------------------------------------------------------

;;; ---------- helpers (global: dialog callbacks need them) ----------

;; Whole numbers show as integers (45), others as decimals (2.5)
(defun VID:GA-Fmt (v / s)
  (cond
    ((equal v (float (fix v)) 1e-9) (itoa (fix v)))
    (T
     (setq s (rtos v 2 6))
     (while (and (wcmatch s "*.*") (= (substr s (strlen s)) "0"))
       (setq s (substr s 1 (1- (strlen s)))))
     s)
  )
)

(defun VID:GA-Validate (/ hs vs rw cl ro co st)
  (setq hs (distof (get_tile "hs"))
        vs (distof (get_tile "vs"))
        rw (distof (get_tile "rows"))
        cl (distof (get_tile "cols"))
        ro (distof (get_tile "ro"))
        co (distof (get_tile "co"))
        st (distof (get_tile "st")))
  (cond
    ((not (and hs vs rw cl ro co st))
     (set_tile "error" "Enter a valid number in every field.") nil)
    ((or (< hs 0.0) (< vs 0.0))
     (set_tile "error" "Spaces cannot be negative.") nil)
    ((or (< (fix rw) 0) (< (fix cl) 0) (< (fix st) 0))
     (set_tile "error" "Row count, Column count and Offset step cannot be negative.") nil)
    (T
     (setq *VIDGA-HS* hs *VIDGA-VS* vs
           *VIDGA-ROWS* (fix rw) *VIDGA-COLS* (fix cl)
           *VIDGA-RO* ro *VIDGA-CO* co
           *VIDGA-ST* (fix st)
           *VIDGA-DIR* (atoi (get_tile "dir")))
     (set_tile "error" "")
     T)
  )
)

(defun VID:GA-Dialog (/ fn f id res)
  (setq *VIDGA-HS*   (cond (*VIDGA-HS*)   (T 45.0))
        *VIDGA-VS*   (cond (*VIDGA-VS*)   (T 45.0))
        *VIDGA-ROWS* (cond (*VIDGA-ROWS*) (T 3))
        *VIDGA-COLS* (cond (*VIDGA-COLS*) (T 500))
        *VIDGA-RO*   (cond (*VIDGA-RO*)   (T 5.0))
        *VIDGA-CO*   (cond (*VIDGA-CO*)   (T 0.0))
        *VIDGA-ST*   (cond (*VIDGA-ST*)   (T 2))
        *VIDGA-DIR*  (cond (*VIDGA-DIR*)  (T 1)))

  (setq fn (vl-filename-mktemp "vidga" nil ".dcl"))
  (setq f (open fn "w"))
  (foreach l
    (list
      "vidga : dialog {"
      "  label = \"VIDGRADIENTALIGN\";"
      "  : column {"
      "    : edit_box { key = \"hs\"; label = \"Horizontal space:\"; edit_width = 14; }"
      "    : edit_box { key = \"vs\"; label = \"Vertical space:\"; edit_width = 14; }"
      "    : edit_box { key = \"rows\"; label = \"Row count:\"; edit_width = 14; }"
      "    : edit_box { key = \"cols\"; label = \"Column count:\"; edit_width = 14; }"
      "    : edit_box { key = \"ro\"; label = \"Row offset:\"; edit_width = 14; }"
      "    : edit_box { key = \"co\"; label = \"Column offset:\"; edit_width = 14; }"
      "    : edit_box { key = \"st\"; label = \"Offset step:\"; edit_width = 14; }"
      "    : popup_list { key = \"dir\"; label = \"Direction:\"; list = \"Horizontal\\nVertical\"; edit_width = 14; }"
      "  }"
      "  : errtile { key = \"error\"; width = 44; }"
      "  ok_cancel;"
      "}"
    )
    (write-line l f)
  )
  (close f)

  (setq id (load_dialog fn))
  (cond
    ((not (new_dialog "vidga" id))
     (alert "Could not open the VIDGRADIENTALIGN dialog.")
     (setq res 0))
    (T
     (set_tile "hs"   (VID:GA-Fmt *VIDGA-HS*))
     (set_tile "vs"   (VID:GA-Fmt *VIDGA-VS*))
     (set_tile "rows" (VID:GA-Fmt *VIDGA-ROWS*))
     (set_tile "cols" (VID:GA-Fmt *VIDGA-COLS*))
     (set_tile "ro"   (VID:GA-Fmt *VIDGA-RO*))
     (set_tile "co"   (VID:GA-Fmt *VIDGA-CO*))
     (set_tile "st"   (VID:GA-Fmt *VIDGA-ST*))
     (set_tile "dir"  (itoa *VIDGA-DIR*))
     (action_tile "accept" "(if (VID:GA-Validate) (done_dialog 1))")
     (action_tile "cancel" "(done_dialog 0)")
     (setq res (start_dialog)))
  )
  (unload_dialog id)
  (vl-file-delete fn)
  (= res 1)
)

;;; ---------- command ----------

(defun c:VIDGRADIENTALIGN (/ *error* doc ss i en p data final
                             nr nc per local band r c idx item oldpt
                             cell w newpt obj cp pt)

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

  (if (VID:GA-Dialog)
    (progn
      (initget 1)
      (setq pt (getpoint "\nPick point (first block, top-left of layout): "))
      (prompt "\nSelect blocks to copy: ")
      (setq ss (ssget '((0 . "INSERT"))))

      (if (null ss)
        (prompt "\nNo blocks selected.")
        (progn
          ;; collect (ename insertion-point)
          (setq data nil i 0)
          (repeat (sslength ss)
            (setq en (ssname ss i)
                  p  (cdr (assoc 10 (entget en))))
            (if (and (listp p) (numberp (car p)) (numberp (cadr p)))
              (setq data (cons (list en
                                     (list (car p) (cadr p)
                                           (if (numberp (caddr p)) (caddr p) 0.0)))
                               data)))
            (setq i (1+ i))
          )

          ;; order b1, b2, ... : left -> right, ties top -> bottom
          (setq final
            (vl-sort data
              (function
                (lambda (a b)
                  (cond
                    ((< (car (cadr a)) (- (car (cadr b)) 1e-9)) T)
                    ((> (car (cadr a)) (+ (car (cadr b)) 1e-9)) nil)
                    (T (> (cadr (cadr a)) (cadr (cadr b)))))))))

          ;; 0 = no limit (large number)
          (setq nr  (if (< *VIDGA-ROWS* 1) 10000 *VIDGA-ROWS*)
                nc  (if (< *VIDGA-COLS* 1) 10000 *VIDGA-COLS*)
                per (* nr nc))

          (vla-StartUndoMark doc)
          (setq idx 0)
          (foreach item final
            (setq en    (car item)
                  oldpt (cadr item)
                  band  (/ idx per)
                  local (rem idx per))

            (if (= *VIDGA-DIR* 1)
              ;; Vertical: down each column (Row count blocks), then next column
              (setq r (rem local nr)  c (/ local nr))
              ;; Horizontal: across each row (Column count blocks), then next row
              (setq r (/ local nc)    c (rem local nc))
            )

            (setq cell
                   (list
                     ;; X: column spacing + row offset by (r mod step)
                     (+ (car pt)
                        (* c *VIDGA-HS*)
                        (* (if (> *VIDGA-ST* 0) (rem r *VIDGA-ST*) 0)
                           *VIDGA-RO*))
                     ;; Y: row spacing (sets stack downward) + column drop
                     (- (cadr pt)
                        (* (+ (* band nr) r) *VIDGA-VS*)
                        (* c *VIDGA-CO*))
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

          (prompt (strcat "\nDone: " (itoa idx) " blocks copied (rotation 0), "
                          (if (= *VIDGA-DIR* 1) "vertical" "horizontal")
                          ", rows " (if (< *VIDGA-ROWS* 1) "no limit" (itoa *VIDGA-ROWS*))
                          ", columns " (if (< *VIDGA-COLS* 1) "no limit" (itoa *VIDGA-COLS*))
                          "."))
        )
      )
    )
    (prompt "\nCancelled.")
  )
  (princ)
)

(prompt "\nVIDGRADIENTALIGN loaded. Type VIDGRADIENTALIGN to run.")
(princ)
