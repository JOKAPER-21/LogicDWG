;;; ------------------------------------------------------------------
;;; VIDBLOCKLAYER
;;; Move selected objects - and everything inside any selected block - to
;;; one target layer. Works in AutoCAD and Civil 3D.
;;;
;;;   1. A dialog lists ALL layers of the current drawing (Civil 3D layers
;;;      included, xref layers excluded). Pick one and press OK, or
;;;      double-click a layer.
;;;   2. Select any objects.
;;;
;;; One undo (U) reverts the whole command.
;;; ------------------------------------------------------------------

(vl-load-com)

;; all layer names of the drawing, sorted A-Z (xref layers "xx|yy" left out)
(defun VID:BL-Layers (/ rec nm lst)
  (setq rec (tblnext "LAYER" T))
  (while rec
    (setq nm (cdr (assoc 2 rec)))
    (if (and nm (not (vl-string-search "|" nm)))
      (setq lst (cons nm lst)))
    (setq rec (tblnext "LAYER"))
  )
  (vl-sort lst (function (lambda (a b) (< (strcase a) (strcase b)))))
)

;; dialog callback (global: action_tile strings run in the global scope)
(defun VID:BL-Accept (/ sel)
  (setq sel (get_tile "lay"))
  (if (= sel "")
    (set_tile "error" "Select a layer from the list.")
    (progn
      (setq *VID-BL-SEL* (nth (atoi sel) *VID-BL-LIST*))
      (done_dialog 1)
    )
  )
)

;; show the layer list, return the chosen layer name or nil
(defun VID:BL-Pick (/ fn f id res idx)
  (setq *VID-BL-LIST* (VID:BL-Layers)
        *VID-BL-SEL*  nil)
  (cond
    ((null *VID-BL-LIST*)
     (alert "No layers found in this drawing.")
     nil)
    (T
     (setq fn (vl-filename-mktemp "vidbl" nil ".dcl"))
     (setq f (open fn "w"))
     (foreach l
       (list
         "vidbl : dialog {"
         "  label = \"Block Layer\";"
         "  : text { key = \"cnt\"; label = \"\"; width = 46; }"
         "  : list_box { key = \"lay\"; width = 46; height = 20; multiple_select = false; }"
         "  : errtile { key = \"error\"; width = 46; }"
         "  ok_cancel;"
         "}"
       )
       (write-line l f)
     )
     (close f)

     (setq id (load_dialog fn))
     (cond
       ((not (new_dialog "vidbl" id))
        (alert "Could not open the Block Layer dialog.")
        (setq res 0))
       (T
        (set_tile "cnt"
          (strcat "Select the target layer  (" (itoa (length *VID-BL-LIST*))
                  " layers in this drawing)"))
        (start_list "lay")
        (mapcar 'add_list *VID-BL-LIST*)
        (end_list)
        ;; preselect: last used layer, otherwise the current layer
        (setq idx (vl-position
                    (cond (*VID-BL-LAST*) (T (getvar "CLAYER")))
                    *VID-BL-LIST*))
        (set_tile "lay" (itoa (if idx idx 0)))
        (action_tile "lay"    "(if (= $reason 4) (VID:BL-Accept))")
        (action_tile "accept" "(VID:BL-Accept)")
        (action_tile "cancel" "(done_dialog 0)")
        (setq res (start_dialog)))
     )
     (unload_dialog id)
     (vl-file-delete fn)
     (if (= res 1) *VID-BL-SEL* nil)
    )
  )
)

(defun c:VIDBLOCKLAYER (/ *error* doc blks lay ss i obj bn bd done
                          cnt fail bcnt res e)

  (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
        blks (vla-get-Blocks doc))

  (defun *error* (msg)
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (prompt (strcat "\nError: " msg)))
    (princ)
  )

  ;; 1. choose the layer in the dialog
  (setq lay (VID:BL-Pick))

  (cond
    ((null lay)
     (prompt "\nCancelled."))

    ((not (tblsearch "LAYER" lay))
     (prompt "\nLayer does not exist."))

    (T
     (setq *VID-BL-LAST* lay)
     (prompt (strcat "\nTarget layer: " lay))

     ;; 2. select ANY objects
     (prompt "\nSelect objects: ")
     (setq ss (ssget))

     (if (null ss)
       (prompt "\nNothing selected.")
       (progn
         (setq i 0 cnt 0 fail 0 bcnt 0 done nil)
         (vla-StartUndoMark doc)
         (repeat (sslength ss)
           (setq obj (vlax-ename->vla-object (ssname ss i)))

           ;; the object itself (locked layers / read-only are counted, not fatal)
           (setq res (vl-catch-all-apply 'vla-put-Layer (list obj lay)))
           (if (vl-catch-all-error-p res)
             (setq fail (1+ fail))
             (setq cnt (1+ cnt)))

           ;; block reference: also change everything inside its definition
           (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
             (progn
               (setq bn (vla-get-Name obj))
               (if (not (member bn done))
                 (progn
                   (setq done (cons bn done))
                   (setq bd (vl-catch-all-apply 'vla-Item (list blks bn)))
                   ;; skip xrefs
                   (if (and (not (vl-catch-all-error-p bd))
                            (= (vla-get-IsXRef bd) :vlax-false))
                     (vlax-for e bd
                       (if (not (vlax-erased-p e))
                         (if (not (vl-catch-all-error-p
                                    (vl-catch-all-apply 'vla-put-Layer (list e lay))))
                           (setq bcnt (1+ bcnt)))))
                   )
                 )
               )
             )
           )
           (setq i (1+ i))
         )
         (vla-EndUndoMark doc)
         (vla-Regen doc 1)

         (prompt (strcat "\nDone: " (itoa cnt) " object(s) and "
                         (itoa bcnt) " block content object(s) moved to layer "
                         lay "."))
         (if (> fail 0)
           (prompt (strcat "\n" (itoa fail)
                           " object(s) skipped (locked layer or read-only).")))
       )
     )
    )
  )
  (princ)
)

(prompt "\nvidBlockLayer loaded. Type VIDBLOCKLAYER to run.")
(princ)
