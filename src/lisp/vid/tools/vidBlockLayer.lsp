;;; ------------------------------------------------------------------
;;; VIDBLOCKLAYER   (v2)
;;; Move selected objects - and everything inside any selected block,
;;; INCLUDING blocks nested inside that block - to one target layer.
;;; Works in AutoCAD and Civil 3D.
;;;
;;;   1. A dialog shows a DROP-DOWN list of all existing layers of the
;;;      drawing (xref layers excluded). Pick one, tick / untick
;;;      "Include nested blocks", press OK.
;;;   2. Select any objects.
;;;
;;; Nested blocks: block inside block inside block ... every level is
;;; followed (each block definition is processed once, loops are safe).
;;; NOTE: a block definition is shared, so every other insert of the same
;;; block in the drawing changes too.
;;;
;;; One undo (U) reverts the whole command.
;;; ------------------------------------------------------------------

(vl-load-com)

(if (null *VID-BL-NEST*) (setq *VID-BL-NEST* "1"))   ; default: nested ON

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
    (set_tile "error" "Select a layer from the drop-down list.")
    (progn
      (setq *VID-BL-SEL*  (nth (atoi sel) *VID-BL-LIST*)
            *VID-BL-NEST* (get_tile "nest"))
      (done_dialog 1)
    )
  )
)

;; show the layer drop-down, return the chosen layer name or nil
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
         "  : text { key = \"cnt\"; label = \"\"; width = 50; }"
         "  : popup_list { key = \"lay\"; label = \"Target layer\"; width = 50; edit_width = 34; }"
         "  : toggle { key = \"nest\"; label = \"Include blocks nested inside the selected blocks\"; value = \"1\"; }"
         "  : errtile { key = \"error\"; width = 50; }"
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
          (strcat (itoa (length *VID-BL-LIST*)) " layers in this drawing"))
        (start_list "lay")
        (mapcar 'add_list *VID-BL-LIST*)
        (end_list)
        ;; preselect: last used layer, otherwise the current layer
        (setq idx (vl-position
                    (cond (*VID-BL-LAST*) (T (getvar "CLAYER")))
                    *VID-BL-LIST*))
        (set_tile "lay" (itoa (if idx idx 0)))
        (set_tile "nest" *VID-BL-NEST*)
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

;; Put everything inside block definition BN on layer LAY.
;; If NEST is true, blocks found inside it are followed recursively.
;; Uses globals *VID-BL-DONE* (names already processed) and *VID-BL-BCNT*.
(defun VID:BL-Def (blks bn lay nest / bd e nm)
  (if (not (member (strcase bn) *VID-BL-DONE*))
    (progn
      (setq *VID-BL-DONE* (cons (strcase bn) *VID-BL-DONE*))
      (setq bd (vl-catch-all-apply 'vla-Item (list blks bn)))
      ;; skip xrefs
      (if (and (not (vl-catch-all-error-p bd))
               (= (vla-get-IsXRef bd) :vlax-false))
        (vlax-for e bd
          (if (not (vlax-erased-p e))
            (progn
              (if (not (vl-catch-all-error-p
                         (vl-catch-all-apply 'vla-put-Layer (list e lay))))
                (setq *VID-BL-BCNT* (1+ *VID-BL-BCNT*)))
              ;; a block inside this block -> go one level deeper
              (if (and nest
                       (member (vla-get-ObjectName e)
                               '("AcDbBlockReference" "AcDbMInsertBlock")))
                (progn
                  (setq nm (vl-catch-all-apply 'vla-get-Name (list e)))
                  (if (not (vl-catch-all-error-p nm))
                    (VID:BL-Def blks nm lay nest))
                )
              )
            )
          )
        )
      )
    )
  )
)

(defun c:VIDBLOCKLAYER (/ *error* doc blks lay ss i obj cnt fail bcnt res nest a)

  (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
        blks (vla-get-Blocks doc))

  (defun *error* (msg)
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (prompt (strcat "\nError: " msg)))
    (princ)
  )

  ;; 1. choose the layer in the dialog (drop-down)
  (setq lay (VID:BL-Pick))

  (cond
    ((null lay)
     (prompt "\nCancelled."))

    ((not (tblsearch "LAYER" lay))
     (prompt "\nLayer does not exist."))

    (T
     (setq *VID-BL-LAST* lay
           nest          (= *VID-BL-NEST* "1"))
     (prompt (strcat "\nTarget layer: " lay
                     (if nest "   (nested blocks included)" "")))

     ;; 2. select ANY objects
     (prompt "\nSelect objects: ")
     (setq ss (ssget))

     (if (null ss)
       (prompt "\nNothing selected.")
       (progn
         (setq i 0 cnt 0 fail 0
               *VID-BL-BCNT* 0
               *VID-BL-DONE* nil)
         (vla-StartUndoMark doc)
         (repeat (sslength ss)
           (setq obj (vlax-ename->vla-object (ssname ss i)))

           ;; the object itself (locked layers / read-only are counted, not fatal)
           (setq res (vl-catch-all-apply 'vla-put-Layer (list obj lay)))
           (if (vl-catch-all-error-p res)
             (setq fail (1+ fail))
             (setq cnt (1+ cnt)))

           ;; block reference: change everything inside its definition
           ;; (and, when ticked, inside every block nested in it)
           (if (member (vla-get-ObjectName obj)
                       '("AcDbBlockReference" "AcDbMInsertBlock"))
             (progn
               ;; attributes of the inserted block
               (vl-catch-all-apply
                 '(lambda ()
                    (if (= (vla-get-HasAttributes obj) :vlax-true)
                      (foreach a (vlax-invoke obj 'GetAttributes)
                        (vla-put-Layer a lay))))
                 nil)
               (setq res (vl-catch-all-apply 'vla-get-Name (list obj)))
               (if (not (vl-catch-all-error-p res))
                 (VID:BL-Def blks res lay nest))
             )
           )
           (setq i (1+ i))
         )
         (vla-EndUndoMark doc)
         (vla-Regen doc 1)

         (prompt (strcat "\nDone: " (itoa cnt) " object(s) and "
                         (itoa *VID-BL-BCNT*) " block content object(s) moved to layer "
                         lay "."))
         (prompt (strcat "\nBlock definitions processed: "
                         (itoa (length *VID-BL-DONE*))))
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
