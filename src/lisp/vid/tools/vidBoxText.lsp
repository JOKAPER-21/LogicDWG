;;; ============================================================================
;;; vidBoxText - Rectangle (3 points) + optional center MTEXT, dialog (ONE time)
;;;
;;;   3------------2
;;;   |            |
;;;   |            |
;;;   0------------1
;;;
;;;   Pick order:  P1 = corner 3  (top-left)
;;;                P2 = corner 2  (top-right)
;;;                P3 = corner 1  (bottom-right, height side)
;;;
;;;   Dialog (only once):  [x] Add Text
;;;                        Text      Name  [ TR   ]
;;;                                  Layer [ 1-TR ] [v]
;;;                        Rectangle Layer [ 1-TR ] [v]
;;;   SPACE / ENTER / ESC at the "Point 1" prompt = finish.
;;; ============================================================================

(vl-load-com)

(setq *VBT-LIST*   nil)
(setq *VBT-PICKED* nil)
(setq *VBT-DCLID*  nil)

;; ---- helpers ---------------------------------------------------------------

(defun VBT-Trim (s)
  (if s (vl-string-trim " \t\r\n\"" s) "")
)

(defun VBT-LayerList (/ r lst)
  (setq lst '())
  (setq r (tblnext "LAYER" T))
  (while r
    (setq lst (cons (cdr (assoc 2 r)) lst))
    (setq r (tblnext "LAYER"))
  )
  (if lst (acad_strlsort lst) nil)
)

(defun VBT-MakeLayer (name / layers)
  (if (not (tblsearch "LAYER" name))
    (progn
      (setq layers
        (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
      (vla-Add layers name)
    )
  )
  name
)

;; ---- enable / disable text fields according to the tick box ----------------

(defun VBT-ToggleText (val)
  (setq val (if (= val "1") 0 1))   ; 0 = enabled, 1 = disabled
  (mode_tile "txt"    val)
  (mode_tile "tlayer" val)
  (mode_tile "tpick"  val)
)

;; ---- "v" button: list of existing layers -----------------------------------

(defun VBT-PickOK (/ s)
  (setq s (get_tile "laylist"))
  (if (/= s "")
    (setq *VBT-PICKED* (nth (atoi s) *VBT-LIST*))
  )
  (done_dialog 1)
)

;; key = "tlayer" (text layer) or "rlayer" (rectangle layer)
(defun VBT-PickLayer (key / cur i idx l)
  (setq cur (strcase (VBT-Trim (get_tile key))))
  (setq *VBT-PICKED* nil)
  (if (new_dialog "vbt_pick" *VBT-DCLID*)
    (progn
      (start_list "laylist")
      (foreach l *VBT-LIST* (add_list l))
      (end_list)
      (setq i 0 idx nil)
      (foreach l *VBT-LIST*
        (if (and (null idx) (= (strcase l) cur)) (setq idx i))
        (setq i (1+ i))
      )
      (if idx (set_tile "laylist" (itoa idx)))
      (action_tile "accept" "(VBT-PickOK)")
      (action_tile "cancel" "(done_dialog 0)")
      (start_dialog)
      (if *VBT-PICKED* (set_tile key *VBT-PICKED*))
    )
  )
)

;; ---- OK button -------------------------------------------------------------

(defun VBT-Accept (/ useTxt tx tl rl)
  (setq useTxt (get_tile "usetext"))
  (setq tx (VBT-Trim (get_tile "txt")))
  (setq tl (VBT-Trim (get_tile "tlayer")))
  (setq rl (VBT-Trim (get_tile "rlayer")))
  (cond
    ((and (= useTxt "1") (= tx ""))
     (set_tile "error" "Name (text) type pannunga."))
    ((and (= useTxt "1") (= tl ""))
     (set_tile "error" "Text layer name type pannunga."))
    ((and (= useTxt "1") (not (snvalid tl)))
     (set_tile "error" "Text layer name la thappu character irukku."))
    ((= rl "")
     (set_tile "error" "Rectangle layer name type pannunga."))
    ((not (snvalid rl))
     (set_tile "error" "Rectangle layer name la thappu character irukku."))
    (T
     (setq *VBT-USE* (= useTxt "1"))
     ;; keep old text values if text is turned off
     (if (= useTxt "1")
       (progn
         (setq *VBT-TXT*  tx)
         (setq *VBT-TLAY* tl)
       )
     )
     (setq *VBT-RLAY* rl)
     (done_dialog 1))
  )
)

;; ---- Show dialog: returns T if OK, nil if cancelled ------------------------

(defun VBT-Dialog (/ fn f id res l)
  (setq *VBT-LIST* (VBT-LayerList))
  (setq fn (vl-filename-mktemp "vbt" nil ".dcl"))
  (setq f (open fn "w"))
  (if (null f)
    (progn (princ "\nDialog file create aagala.") nil)
    (progn
      (foreach l
        (list
          "vbt_dlg : dialog {"
          "  label = \"vidBoxText\";"
          "  : boxed_column {"
          "    label = \"Text\";"
          "    : toggle { key = \"usetext\"; label = \"Add Text (text venum)\"; }"
          "    : row {"
          "      children_alignment = centered;"
          "      : text { label = \"Name\"; width = 10; fixed_width = true; }"
          "      : edit_box { key = \"txt\"; edit_width = 26; fixed_width = true; }"
          "      : spacer { width = 4; fixed_width = true; }"
          "    }"
          "    : row {"
          "      children_alignment = centered;"
          "      : text { label = \"Layer\"; width = 10; fixed_width = true; }"
          "      : edit_box { key = \"tlayer\"; edit_width = 26; fixed_width = true; }"
          "      : button { key = \"tpick\"; label = \"v\"; width = 4; fixed_width = true; }"
          "    }"
          "  }"
          "  : boxed_column {"
          "    label = \"Rectangle\";"
          "    : row {"
          "      children_alignment = centered;"
          "      : text { label = \"Layer\"; width = 10; fixed_width = true; }"
          "      : edit_box { key = \"rlayer\"; edit_width = 26; fixed_width = true; }"
          "      : button { key = \"rpick\"; label = \"v\"; width = 4; fixed_width = true; }"
          "    }"
          "  }"
          "  spacer_1;"
          "  ok_cancel_err;"
          "}"
          "vbt_pick : dialog {"
          "  label = \"Select Layer\";"
          "  : list_box { key = \"laylist\"; width = 34; height = 14; allow_accept = true; }"
          "  ok_cancel;"
          "}"
        )
        (write-line l f)
      )
      (close f)

      (setq id (load_dialog fn))
      (setq *VBT-DCLID* id)

      (if (new_dialog "vbt_dlg" id)
        (progn
          (set_tile "usetext" (if *VBT-USE* "1" "0"))
          (set_tile "txt"     *VBT-TXT*)
          (set_tile "tlayer"  *VBT-TLAY*)
          (set_tile "rlayer"  *VBT-RLAY*)
          (VBT-ToggleText (if *VBT-USE* "1" "0"))
          (if *VBT-USE* (mode_tile "txt" 2))
          (action_tile "usetext" "(VBT-ToggleText $value)")
          (action_tile "tpick"   "(VBT-PickLayer \"tlayer\")")
          (action_tile "rpick"   "(VBT-PickLayer \"rlayer\")")
          (action_tile "accept"  "(VBT-Accept)")
          (action_tile "cancel"  "(done_dialog 0)")
          (setq res (start_dialog))
        )
        (progn (princ "\nDialog open aagala.") (setq res 0))
      )

      (unload_dialog id)
      (vl-file-delete fn)
      (= res 1)
    )
  )
)

;; ---- Draw one rectangle (+ optional text) ----------------------------------
;; pt1 = corner 3, pt2 = corner 2, pt3 = height side
;; useTxt = T -> add MTEXT, nil -> rectangle only
;; returns T if drawn, nil if P1 and P2 are the same

(defun VBT-Draw (pt1 pt2 pt3 tlay rlay txt useTxt
                 / dx dy len ux uy px py width c0 c1 c2 c3 cx cy angle)
  (setq dx  (- (car pt2)  (car pt1)))
  (setq dy  (- (cadr pt2) (cadr pt1)))
  (setq len (sqrt (+ (* dx dx) (* dy dy))))

  (if (equal len 0.0 1e-9)
    (progn (princ "\nPoint 1 & Point 2 same irukku!") nil)
    (progn
      (setq ux (/ dx len))
      (setq uy (/ dy len))

      ;; perpendicular (90 deg CCW)
      (setq px (- uy))
      (setq py ux)

      ;; signed height from P3
      (setq dx (- (car pt3)  (car pt1)))
      (setq dy (- (cadr pt3) (cadr pt1)))
      (setq width (+ (* dx px) (* dy py)))

      ;; corners (numbering as in the sketch)
      (setq c3 (list (car pt1) (cadr pt1) 0.0))
      (setq c2 (list (car pt2) (cadr pt2) 0.0))
      (setq c1 (list (+ (car pt2) (* width px))
                     (+ (cadr pt2) (* width py))
                     0.0))
      (setq c0 (list (+ (car pt1) (* width px))
                     (+ (cadr pt1) (* width py))
                     0.0))

      ;; rectangle
      (entmake
        (list
          '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          (cons 8 rlay)
          '(100 . "AcDbPolyline")
          '(90 . 4)
          '(70 . 1)
          (cons 10 (list (car c0) (cadr c0)))
          (cons 10 (list (car c1) (cadr c1)))
          (cons 10 (list (car c2) (cadr c2)))
          (cons 10 (list (car c3) (cadr c3)))
        )
      )

      ;; text (only if tick box is ON)
      (if useTxt
        (progn
          (setq cx (/ (+ (car c0) (car c1) (car c2) (car c3)) 4.0))
          (setq cy (/ (+ (cadr c0) (cadr c1) (cadr c2) (cadr c3)) 4.0))
          (setq angle (atan uy ux))
          (entmake
            (list
              '(0 . "MTEXT")
              '(100 . "AcDbEntity")
              (cons 8 tlay)
              '(100 . "AcDbMText")
              (cons 10 (list cx cy 0.0))
              (cons 40 2.0)                ; text height
              (cons 41 0.0)                ; no wrapping
              (cons 71 5)                  ; Middle Center
              (cons 72 1)
              (cons 1 txt)
              (cons 50 angle)
            )
          )
        )
      )
      T
    )
  )
)

;; ---- Main command ----------------------------------------------------------

(defun c:vidBoxText ( / pt1 pt2 pt3 go count tlay rlay useTxt )

  ;; remembered values (first time defaults)
  (if (null *VBT-TXT*)  (setq *VBT-TXT*  "TR"))
  (if (null *VBT-TLAY*) (setq *VBT-TLAY* "1-TR"))
  (if (null *VBT-RLAY*) (setq *VBT-RLAY* "1-TR"))
  (if (null *VBT-USE*)  (setq *VBT-USE*  T))   ; default: text ON

  ;; dialog ONE time
  (if (not (VBT-Dialog))
    (princ "\nCancelled.")
    (progn
      (setq useTxt *VBT-USE*)
      (setq tlay   *VBT-TLAY*)
      (setq rlay   *VBT-RLAY*)
      (VBT-MakeLayer rlay)
      (if useTxt (VBT-MakeLayer tlay))

      (setq count 0)
      (setq go T)

      ;; repeat until SPACE / ENTER / ESC
      (while go
        (setq pt1 (getpoint
                    (strcat "\n["
                            (if useTxt *VBT-TXT* "No text")
                            "] Point 1 (Corner 3 - top-left)"
                            " <SPACE = finish>: ")))
        (if (null pt1)
          (setq go nil)
          (progn
            (setq pt2 (getpoint pt1 "\nPoint 2 (Corner 2 - top-right): "))
            (if (null pt2)
              (setq go nil)
              (progn
                (setq pt3 (getpoint pt2 "\nPoint 3 (Corner 1 - bottom-right): "))
                (if (null pt3)
                  (setq go nil)
                  (if (VBT-Draw pt1 pt2 pt3 tlay rlay *VBT-TXT* useTxt)
                    (setq count (1+ count))
                  )
                )
              )
            )
          )
        )
      )

      (princ (strcat "\n[OK] vidBoxText finished. Boxes = " (itoa count)
                     (if useTxt
                       (strcat " | Text = " *VBT-TXT* " | Text layer = " tlay)
                       " | Text = OFF")
                     " | Rect layer = " rlay))
    )
  )
  (princ)
)

(princ "\n[vidBoxText Loaded] Command: vidBoxText")
(princ "\nPick: Corner 3 -> Corner 2 -> Corner 1 | SPACE = finish")
(princ)