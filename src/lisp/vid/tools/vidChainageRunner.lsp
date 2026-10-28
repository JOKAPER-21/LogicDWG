;;; ============================================================================
;;; Vid Chainage Runner
;;; Command : VIDCHAINAGERUNNER
;;; Version : v3
;;;   v3: * Dialog:  ( ) Forward  ( ) Backward
;;;                  KM / metre : [ 21/000 ]      (one box, "/" delimiter)
;;;                  Increment  : [   ]
;;;                  [x] Add Symbol   (off = the three fields below are disabled)
;;;                  Symbol Name   : [ vid_kmArrow_001 ]
;;;                  Symbol Height : [ 120 ]
;;;                  Direction     : [ Up v ]
;;;                  [Cancel] [Ok]
;;;       * The "N polyline(s) selected" / "Select objects" labels are gone.
;;;       * The "Total Length = ..." text is replaced by the block
;;;         symbol block (imported from vid_blocks_tools.dwg), placed at the
;;;         END of every selected polyline:
;;;            Up   -> left  side of the polyline
;;;            Down -> right side of the polyline
;;;         Symbol Height = distance of the block from the polyline.
;;;       * Block attributes (tag kmArrow):
;;;            top    : KM <entered km + 1>        e.g.  KM 22
;;;            bottom : (<entered km>/<length>M)   e.g.  (21/999.85M)
;;;   v2: * Select ONE OR MORE polylines first; each is run separately with
;;;         the same KM / metre start value.
;;;   Forward : KM / metre is the value at the START of the polyline.
;;;   Backward: KM / metre is the value at the END of the polyline; chainage
;;;             decreases as the run walks from the end back to the start.
;;; ============================================================================

(vl-load-com)

;; Last used values (remembered between runs)
(if (null VCR-Dir)    (setq VCR-Dir    "Forward"))
(if (null VCR-Km)     (setq VCR-Km     0))
(if (null VCR-M)      (setq VCR-M      0))
(if (null VCR-Step)   (setq VCR-Step   100))
(if (null VCR-Arrow)  (setq VCR-Arrow  "vid_kmArrow_001"))
(if (null VCR-ArrowH) (setq VCR-ArrowH 120.0))
(if (null VCR-Side)   (setq VCR-Side   "Up"))
(if (null VCR-UseSym) (setq VCR-UseSym "1"))   ;; "1" = place symbol, "0" = no symbol

(setq VCR-DwgName  "vid_blocks_tools.dwg")
(setq VCR-Tried    nil)
(setq VCR-Folder   nil)
(setq VCR-ArrowCnt 0)

(setq VCR-OldError nil)
(setq VCR-OldLay   nil)
(setq VCR-OldEcho  nil)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------
(defun VCR-IntP (s)
  (and s
       (/= (vl-string-trim " " s) "")
       (= (vl-string-trim "0123456789" (vl-string-trim " " s)) "")
  )
)

;; "21/000" style split on any delimiter
(defun VCR-Split (str delim / pos res)
  (setq res '())
  (while (setq pos (vl-string-search delim str))
    (setq res (cons (substr str 1 pos) res)
          str (substr str (+ pos 1 (strlen delim)))))
  (reverse (cons str res))
)

;; metre as 3 digits (5 -> "005")
(defun VCR-Pad3 (m / s)
  (setq s (strcat "000" (itoa m)))
  (substr s (- (strlen s) 2))
)

;; Called by the OK button: validates the tiles, stores them, returns T / nil
(defun VCR-Validate (/ kmm parts st nm ht)
  (setq kmm   (vl-string-trim " " (get_tile "edt_kmm"))
        st    (get_tile "edt_inc")
        nm    (vl-string-trim " " (get_tile "edt_arrow"))
        ht    (distof (vl-string-trim " " (get_tile "edt_arrowh")) 2)
        parts (VCR-Split kmm "/"))
  (cond
    ((or (/= (length parts) 2)
         (not (VCR-IntP (car parts)))
         (not (VCR-IntP (cadr parts))))
     (alert "Enter KM / metre as whole numbers separated by \"/\", e.g. 21/000.")
     (mode_tile "edt_kmm" 2) nil)
    ((> (atoi (vl-string-trim " " (cadr parts))) 999)
     (alert "Metre (after the \"/\") must be between 0 and 999.")
     (mode_tile "edt_kmm" 2) nil)
    ((or (not (VCR-IntP st)) (< (atoi (vl-string-trim " " st)) 1))
     (alert "Increment must be a whole number (1 or more).")
     (mode_tile "edt_inc" 2) nil)
    ((and (= (get_tile "tgl_sym") "1") (= nm ""))
     (alert "Enter the Symbol Name (block name).")
     (mode_tile "edt_arrow" 2) nil)
    ((and (= (get_tile "tgl_sym") "1") (or (not ht) (<= ht 0.0)))
     (alert "Symbol Height must be a number greater than 0.")
     (mode_tile "edt_arrowh" 2) nil)
    (T
     (setq VCR-Km     (atoi (vl-string-trim " " (car parts)))
           VCR-M      (atoi (vl-string-trim " " (cadr parts)))
           VCR-Step   (atoi (vl-string-trim " " st))
           VCR-UseSym (get_tile "tgl_sym")
           VCR-Dir    (if (= (get_tile "rb_bwd") "1") "Backward" "Forward"))
     ;; symbol settings are only taken over when the check box is on
     (if (= VCR-UseSym "1")
       (setq VCR-Arrow  nm
             VCR-ArrowH ht
             VCR-Side   (if (= (get_tile "pop_side") "1") "Down" "Up")))
     T)
  )
)

;; check box: enable / disable the three symbol fields
(defun VCR-SymToggle (val / m)
  (setq m (if (= val "1") 0 1))
  (mode_tile "edt_arrow"  m)
  (mode_tile "edt_arrowh" m)
  (mode_tile "pop_side"   m)
)

;; Dialog.  Returns T = Ok, nil = Cancel.
(defun VCR-Dialog (/ dlgName f dcl_id ret ln)
  (setq dlgName (vl-filename-mktemp "vcrUi" (getvar "TEMPPREFIX") ".dcl"))
  (setq f (open dlgName "w"))
  (foreach ln
    (list
      "vcr_ui : dialog {"
      "  label = \"Chainage Runner\";"
      "  : boxed_radio_row {"
      "    label = \"Direction\";"
      "    : radio_button { key = \"rb_fwd\"; label = \"Forward\";  value = \"1\"; }"
      "    : radio_button { key = \"rb_bwd\"; label = \"Backward\"; value = \"0\"; }"
      "  }"
      "  : edit_box { key = \"edt_kmm\";    label = \"KM / metre (e.g. 21/000):\"; edit_width = 10; }"
      "  : edit_box { key = \"edt_inc\";    label = \"Increment:\";                edit_width = 10; }"
      "  : toggle { key = \"tgl_sym\"; label = \"Add Symbol\"; }"
      "  : edit_box { key = \"edt_arrow\";  label = \"Symbol Name:\";   edit_width = 20; }"
      "  : edit_box { key = \"edt_arrowh\"; label = \"Symbol Height:\"; edit_width = 10; }"
      "  : popup_list { key = \"pop_side\"; label = \"Direction:\";     edit_width = 10; }"
      "  spacer_1;"
      "  : row {"
      "    alignment = centered;"
      "    : button { key = \"cancel\"; label = \"Cancel\"; width = 10; fixed_width = true; is_cancel = true; }"
      "    : button { key = \"accept\"; label = \"Ok\";     width = 10; fixed_width = true; is_default = true; }"
      "  }"
      "}")
    (write-line ln f))
  (close f)
  (setq dcl_id (load_dialog dlgName))
  (setq ret nil)
  (if (and dcl_id (new_dialog "vcr_ui" dcl_id))
    (progn
      (set_tile "rb_fwd" (if (= VCR-Dir "Backward") "0" "1"))
      (set_tile "rb_bwd" (if (= VCR-Dir "Backward") "1" "0"))
      (set_tile "edt_kmm"    (strcat (itoa VCR-Km) "/" (VCR-Pad3 VCR-M)))
      (set_tile "edt_inc"    (itoa VCR-Step))
      (set_tile "edt_arrow"  VCR-Arrow)
      (set_tile "edt_arrowh" (rtos VCR-ArrowH 2 (if (equal VCR-ArrowH (fix VCR-ArrowH) 1e-9) 0 2)))
      (start_list "pop_side")
      (add_list "Up")
      (add_list "Down")
      (end_list)
      (set_tile "pop_side" (if (= VCR-Side "Down") "1" "0"))
      (set_tile "tgl_sym" VCR-UseSym)
      (VCR-SymToggle VCR-UseSym)
      (action_tile "tgl_sym" "(VCR-SymToggle $value)")
      (mode_tile "edt_kmm" 2)
      (action_tile "accept" "(if (VCR-Validate) (done_dialog 1))")
      (action_tile "cancel" "(done_dialog 0)")
      (setq ret (= (start_dialog) 1))
    )
    (princ "\nVIDCHAINAGERUNNER: could not open dialog.")
  )
  (if dcl_id (unload_dialog dcl_id))
  (vl-catch-all-apply 'vl-file-delete (list dlgName))
  ret
)

(defun VCR-Error (msg)
  (if VCR-OldLay  (setvar "CLAYER" VCR-OldLay))
  (if VCR-OldEcho (setvar "CMDECHO" VCR-OldEcho))
  (setq *error* VCR-OldError)
  (if (and msg (/= msg "Function cancelled") (/= msg "quit / exit abort"))
    (princ (strcat "\nVIDCHAINAGERUNNER ERROR: " msg))
    (princ "\nVIDCHAINAGERUNNER cancelled.")
  )
  (princ)
)

;; ---------------------------------------------------------------------------
;; FIND vid_blocks_tools.dwg  (no user prompt)
;;   1. folder of this LSP        2. folder of vidGradient.lsp (if loaded)
;;   3. <Support>\vid\tools folders, Trusted Paths, drawing folder
;; ---------------------------------------------------------------------------
(defun VCR-VidDirs (/ res ap d)
  (setq res "")
  (if (setq ap (getenv "APPDATA"))
    (progn
      (setq res (strcat ap "/Autodesk/C3D 2026/enu/Support/vid/tools"))
      (foreach d (vl-directory-files (strcat ap "/Autodesk") "*" -1)
        (if (and (/= d ".") (/= d ".."))
          (setq res (strcat res ";" ap "/Autodesk/" d "/enu/Support/vid/tools"))))))
  (foreach d (VCR-Split (getvar "ACADPREFIX") ";")
    (if (/= d "")
      (setq res (strcat res ";" d "/vid/tools"))))
  res
)

(defun VCR-SearchFile (fname / f dirs d res)
  (setq res nil)
  (setq VCR-Tried (cons (strcat "findfile: " fname) VCR-Tried))
  (cond
    ((setq f (findfile fname)) (setq res f))
    (T
     (setq dirs (VCR-Split (strcat (VCR-VidDirs) ";" (getvar "TRUSTEDPATHS") ";" (getvar "DWGPREFIX")) ";"))
     (foreach d dirs
       (if (and (not res) (/= d ""))
         (progn
           (while (and (> (strlen d) 0) (member (substr d (strlen d)) (list "." "\\" "/")))
             (setq d (substr d 1 (1- (strlen d)))))
           (setq VCR-Tried (cons (strcat d "/" fname) VCR-Tried))
           (if (and (/= d "") (setq f (findfile (strcat d "/" fname))))
             (setq res f)))))))
  res
)

;; remember the folder of this LSP as soon as it is loaded
(if (setq VCR-Tmp (VCR-SearchFile "vidChainageRunner.lsp"))
  (setq VCR-Folder (vl-filename-directory VCR-Tmp))
)
(setq VCR-Tmp nil VCR-Tried nil)

(defun VCR-FindDwg (/ f)
  (cond
    ((and VCR-Folder
          (setq f (findfile (strcat VCR-Folder "/" VCR-DwgName))))
     f)
    ((and (boundp '*vg-dir*) *vg-dir*
          (setq f (findfile (strcat *vg-dir* "/" VCR-DwgName))))
     f)
    ((setq f (VCR-SearchFile VCR-DwgName)) f)
    (T nil))
)

;; ---------------------------------------------------------------------------
;; IMPORT BLOCK FROM THE DWG - overrides the existing definition.
;; The DWG is inserted under a TEMP name (avoids the "block references
;; itself" problem), the temp insert is erased, and the named block that came
;; in with it is used.  Returns the block's table entry, or nil.
;; ---------------------------------------------------------------------------
(defun VCR-ImportBlock (blk dwg / tmp old existed oldAtt oldEcho lastEnt r)
  (setq dwg     (vl-string-translate "\\" "/" dwg)
        tmp     "VCR_TMP_IMPORT"
        existed (tblsearch "BLOCK" blk)
        old     (strcat blk "_OLD_" (itoa (fix (* 1000000.0 (rem (getvar "DATE") 1.0)))))
        oldAtt  (getvar "ATTREQ")
        oldEcho (getvar "CMDECHO"))
  (setvar "ATTREQ" 0)
  (setvar "CMDECHO" 0)

  (setq r
    (vl-catch-all-apply
      '(lambda ()
         ;; leftovers from an earlier failed run
         (if (tblsearch "BLOCK" tmp)
           (vl-catch-all-apply 'command (list "_.-PURGE" "_Blocks" tmp "_No")))
         ;; move the existing definition out of the way so the new one is not ignored
         (if existed
           (command "_.-RENAME" "_Block" blk old))
         ;; import the DWG under the temp name, then erase the temp insert
         (setq lastEnt (entlast))
         (command "_.-INSERT" (strcat tmp "=" dwg) "0,0,0" 1 1 0)
         (while (> (getvar "CMDACTIVE") 0) (command ""))
         (if (and (entlast) (not (equal (entlast) lastEnt)))
           (entdel (entlast)))
         ;; tidy up (silently ignored if still referenced)
         (if (tblsearch "BLOCK" tmp)
           (vl-catch-all-apply 'command (list "_.-PURGE" "_Blocks" tmp "_No")))
         (if (tblsearch "BLOCK" old)
           (vl-catch-all-apply 'command (list "_.-PURGE" "_Blocks" old "_No")))
         (while (> (getvar "CMDACTIVE") 0) (command "")))
      nil)
  )

  ;; ALWAYS restore system variables, even after an error
  (while (> (getvar "CMDACTIVE") 0) (command nil))
  (setvar "ATTREQ" oldAtt)
  (setvar "CMDECHO" oldEcho)

  ;; import failed halfway: put the old definition back
  (if (and existed (not (tblsearch "BLOCK" blk)) (tblsearch "BLOCK" old))
    (vl-catch-all-apply 'command (list "_.-RENAME" "_Block" old blk)))

  (if (vl-catch-all-error-p r)
    (prompt (strcat "\nImport error: " (vl-catch-all-error-message r))))
  (if (not (tblsearch "BLOCK" blk))
    (prompt (strcat "\n! Block " blk " was not found inside " dwg)))
  (tblsearch "BLOCK" blk)
)

;; ---------------------------------------------------------------------------
;; Attribute helpers for the arrow block
;; ---------------------------------------------------------------------------
(defun VCR-AttPt (att / p)
  (setq p (vl-catch-all-apply
            '(lambda ()
               (vlax-safearray->list
                 (vlax-variant-value
                   (if (= (vla-get-Alignment att) 0)
                     (vla-get-InsertionPoint att)
                     (vla-get-TextAlignmentPoint att)))))
            nil))
  (if (vl-catch-all-error-p p) (list 0.0 0.0 0.0) p)
)

;; Fills the "top" and "bottom" attributes of an inserted symbol block.
;; Block attribute tag: KM_ARROW (two attributes). Their default values tell
;; them apart:   "KM 000"  -> top        "(000/0000.00M)" -> bottom
;; Fallbacks: "TOP"/"BOT" in the tag, then position in the block (higher = top),
;; then the order of the attributes in the block.
;; Returns number of attributes set.
(defun VCR-SetArrowAtts2 (obj topTxt botTxt rot ip / atts cand a tg top bot pairs p dx dy tp bp pr)
  (setq atts (vlax-safearray->list (vlax-variant-value (vla-GetAttributes obj))))
  (setq cand '())
  (foreach a atts
    (if (or (wcmatch (strcase (vla-get-TagString a)) "KM_ARROW*")
            (wcmatch (strcase (vla-get-TagString a)) "KMARROW*"))
      (setq cand (append cand (list a)))))
  (if (< (length cand) 2) (setq cand atts))

  ;; 0) default values: "KM ..." = top, "(...)" = bottom
  (foreach a cand
    (setq tg (strcase (vl-string-trim " " (vla-get-TextString a))))
    (if (and (null top) (wcmatch tg "KM*")) (setq top a))
    (if (and (null bot) (wcmatch tg "(*")) (setq bot a)))

  ;; 1) explicit TOP / BOTTOM in the tag name
  (if (or (null top) (null bot) (equal top bot))
    (progn
      (setq top nil bot nil)
      (foreach a cand
        (setq tg (strcase (vla-get-TagString a)))
        (if (and (null top) (wcmatch tg "*TOP*")) (setq top a))
        (if (and (null bot) (or (wcmatch tg "*BOT*") (wcmatch tg "*BTM*"))) (setq bot a)))))

  ;; 2) otherwise by position inside the block (ties keep the block's own order)
  (if (or (null top) (null bot) (equal top bot))
    (progn
      (setq top nil bot nil pairs '())
      (foreach a cand
        (setq p  (VCR-AttPt a)
              dx (- (car p)  (car ip))
              dy (- (cadr p) (cadr ip)))
        (setq pairs (append pairs (list (cons (- (* dy (cos rot)) (* dx (sin rot))) a)))))
      (if pairs
        (progn
          (setq tp (car pairs))
          (foreach pr pairs (if (> (car pr) (+ (car tp) 1e-6)) (setq tp pr)))
          (foreach pr pairs
            (if (not (eq pr tp))
              (if (or (null bp) (< (car pr) (- (car bp) 1e-6))) (setq bp pr))))
          (setq top (cdr tp))
          (if bp (setq bot (cdr bp)))))))

  (if top (vla-put-TextString top topTxt))
  (if bot (vla-put-TextString bot botTxt))
  (if (not (and top bot))
    (progn
      (princ (strcat "\n! Symbol attributes in block: "
                     (if atts
                       (apply 'strcat
                         (mapcar '(lambda (x) (strcat "[" (vla-get-TagString x) "] ")) atts))
                       "none")))))
  (cond ((and top bot) 2) (top 1) (T 0))
)

;; Main entry.  The block has ONE multi-line attribute KM_ARROW whose default
;; value is  KM 000\P(000/0000.00M)  -> it is filled with  KM 22\P(21/999.85M).
;; (\P = line break).  Blocks that still have two separate attributes are
;; handled by VCR-SetArrowAtts2.
(defun VCR-SetArrowAtts (obj topTxt botTxt rot ip / atts a comb tagged)
  (setq atts (vlax-safearray->list (vlax-variant-value (vla-GetAttributes obj))))
  (setq tagged '())
  (foreach a atts
    (if (and (null comb) (vl-string-search "\\P" (strcase (vla-get-TextString a))))
      (setq comb a))
    (if (or (wcmatch (strcase (vla-get-TagString a)) "KM_ARROW*")
            (wcmatch (strcase (vla-get-TagString a)) "KMARROW*"))
      (setq tagged (append tagged (list a)))))
  ;; a single KM_ARROW attribute is the combined top + bottom one
  (if (and (null comb) (= (length tagged) 1)) (setq comb (car tagged)))
  (if comb
    (progn
      (vla-put-TextString comb (strcat topTxt "\\P" botTxt))
      2)
    (VCR-SetArrowAtts2 obj topTxt botTxt rot ip))
)

;; Runs the chainage along ONE polyline entity.
(defun VCR-RunOne (ent km m step dir useArrow / ms len dist pt param deriv ang
                   txt txtPt circ hatch loop mtx offset sideVec
                   endPt endParam endDeriv endAng sideAng arrPt arr nAtt
                   chStep count km0)
  (setq ms     (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq offset 1.5)
  (setq km0    km)
  (setq chStep (if (= dir "Forward") step (- step)))
  (setq len    (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
  (setq dist   (if (= dir "Forward") 0 len))
  (setq count  0)

  (while (and (>= dist 0) (<= dist len))
    (setq pt    (vlax-curve-getPointAtDist ent dist))
    (setq param (vlax-curve-getParamAtDist ent dist))
    (setq deriv (vlax-curve-getFirstDeriv ent param))
    (setq ang   (angle '(0 0 0) deriv))

    ;; circle + solid hatch
    (setq circ (vla-AddCircle ms (vlax-3d-point pt) 0.30))
    (vla-put-Layer circ "1-CHAINAGE")
    (setq hatch (vla-AddHatch ms acHatchPatternTypePreDefined "SOLID" :vlax-true))
    (vla-put-Layer hatch "1-CHAINAGE")
    (setq loop (vlax-make-safearray vlax-vbObject '(0 . 0)))
    (vlax-safearray-put-element loop 0 circ)
    (vla-AppendOuterLoop hatch loop)
    (vla-put-Color hatch 7)
    (vla-Evaluate hatch)

    ;; text on the right-hand side of the travel direction
    (setq sideVec (list (cadr deriv) (- (car deriv)) 0))
    (setq txtPt   (polar pt (angle '(0 0 0) sideVec) offset))
    (setq txt
      (strcat (itoa km) "/"
              (substr (strcat "000" (itoa m))
                      (- (strlen (strcat "000" (itoa m))) 2))))
    (setq mtx (vla-AddMText ms (vlax-3d-point txtPt) 10 txt))
    (vla-put-AttachmentPoint mtx acAttachmentPointMiddleCenter)
    (vla-put-InsertionPoint mtx (vlax-3d-point txtPt))
    (vla-put-Rotation mtx ang)
    (vla-put-Layer mtx "1-CHAINAGE")
    (vla-put-Height mtx 2)
    (setq count (1+ count))

    ;; next chainage value (with KM roll-over / roll-under)
    (setq m (+ m chStep))
    (if (= dir "Forward")
      (while (>= m 1000) (setq km (1+ km) m (- m 1000)))
      (while (< m 0)     (setq km (1- km) m (+ m 1000)))
    )
    (setq dist (if (= dir "Forward") (+ dist step) (- dist step)))
  )

  ;; arrow block at the last vertex:  Up = left side, Down = right side
  (if useArrow
    (progn
      (setq endParam (vlax-curve-getEndParam ent))
      (setq endPt    (vlax-curve-getEndPoint ent))
      (setq endDeriv (vlax-curve-getFirstDeriv ent endParam))
      (setq endAng   (angle '(0 0 0) endDeriv))
      (setq sideAng  (if (= VCR-Side "Down") (- endAng (/ pi 2.0)) (+ endAng (/ pi 2.0))))
      (setq arrPt    (polar endPt sideAng VCR-ArrowH))
      (setq arr (vl-catch-all-apply 'vla-InsertBlock
                  (list ms (vlax-3d-point arrPt) VCR-Arrow 1.0 1.0 1.0 endAng)))
      (if (vl-catch-all-error-p arr)
        (princ (strcat "\n! Could not insert " VCR-Arrow ": " (vl-catch-all-error-message arr)))
        (progn
          (vla-put-Layer arr "1-CHAINAGE")
          (setq nAtt
            (VCR-SetArrowAtts
              arr
              (strcat "KM " (itoa (1+ km0)))
              (strcat "(" (itoa km0) "/" (rtos len 2 2) "M)")
              endAng
              arrPt))
          (if (< nAtt 2)
            (princ (strcat "\n! " VCR-Arrow ": only " (itoa nAtt) " attribute(s) filled, expected top + bottom.")))
          (setq VCR-ArrowCnt (1+ VCR-ArrowCnt))))
    )
  )
  count
)

;; ---------------------------------------------------------------------------
;; Command
;; ---------------------------------------------------------------------------
(defun c:VIDCHAINAGERUNNER (/ ss i ent n total dwg useArrow d)
  (vl-load-com)
  (setq VCR-OldError *error*)
  (setq *error* VCR-Error)
  (setq VCR-OldLay  (getvar "CLAYER"))
  (setq VCR-OldEcho (getvar "CMDECHO"))

  (princ "\nSelect polyline(s) (chainage runs in the increasing-chainage direction of each): ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))

  (cond
    ((null ss)
     (princ "\nSelect at least one valid polyline."))
    ((not (VCR-Dialog))
     (princ "\nVIDCHAINAGERUNNER cancelled."))
    (T
     (setvar "CMDECHO" 0)
     (if (not (tblsearch "LAYER" "1-CHAINAGE"))
       (command "_.LAYER" "_M" "1-CHAINAGE" "")
     )
     (setvar "CLAYER" VCR-OldLay)

     ;; import the arrow block once (replaces an older definition)
     (setq useArrow nil)
     (setq VCR-Tried nil)
     (if (= VCR-UseSym "1")
     (if (setq dwg (VCR-FindDwg))
       (if (VCR-ImportBlock VCR-Arrow dwg)
         (setq useArrow T)
         (princ (strcat "\nSymbol " VCR-Arrow " not imported - run continues without symbols.")))
       (progn
         (princ (strcat "\n" VCR-DwgName " not found. Locations checked:"))
         (foreach d (reverse VCR-Tried) (princ (strcat "\n  " d)))
         (princ "\nRun continues without symbols."))))
     (setq VCR-ArrowCnt 0)

     (setq i 0 total 0)
     (while (< i (sslength ss))
       (setq ent (ssname ss i))
       (setq n (VCR-RunOne ent VCR-Km VCR-M VCR-Step VCR-Dir useArrow))
       (setq total (+ total n))
       (setq i (1+ i))
     )
     (setvar "CLAYER"  VCR-OldLay)
     (setvar "CMDECHO" VCR-OldEcho)
     (princ (strcat "\nVIDCHAINAGERUNNER completed: "
                    (itoa (sslength ss)) " polyline(s), "
                    (itoa total) " chainage mark(s), "
                    (itoa VCR-ArrowCnt) " symbol(s), "
                    VCR-Dir " from "
                    (itoa VCR-Km) "/" (VCR-Pad3 VCR-M)
                    ", increment " (itoa VCR-Step) "."))
    )
  )
  (setq *error* VCR-OldError)
  (princ)
)

(princ "\nVIDCHAINAGERUNNER loaded. Type VIDCHAINAGERUNNER to run.")
(princ)
