;;; ===========================================================================
;;; vidSurveySettings.lsp      Command: VIDSURVEYSETTINGS
;;;
;;; Dialog "Survey Height":
;;;   - Layers list (multiple selection)          -> layers to change
;;;   - Text Height : [ number ]                  -> TEXT / MTEXT height on those layers
;;;   - Point style : [ PDMODE drop-down ]        -> all 20 PDMODE values
;;;   - Point Size  : [ number ]                  -> PDSIZE
;;; Defaults: Text Height 0.5, Point Size 0.5 (whole numbers are shown without
;;; decimals, e.g. 1 not 1.0000).
;;; Leave Text Height or Point Size blank to leave that setting unchanged.
;;; PDMODE / PDSIZE are drawing-wide; they are applied when at least one POINT
;;; exists on the selected layers (same rule as v02).
;;; Pure AutoLISP (entmod) - no ActiveX, works in AutoCAD and Civil 3D.
;;; ===========================================================================

(setq VSH-*lastLayers* nil)   ; layer choice remembered between runs
(setq VSH-*defH*  0.5)        ; default Text Height
(setq VSH-*defHP* 0.5)        ; default Point Size

;; PDMODE values and their names (shape + optional circle / square)
(setq VSH-*pdmodes*
  '((0   . "0  -  Dot")
    (1   . "1  -  Nothing")
    (2   . "2  -  Plus (+)")
    (3   . "3  -  Cross (X)")
    (4   . "4  -  Tick")
    (32  . "32  -  Dot + Circle")
    (33  . "33  -  Circle")
    (34  . "34  -  Plus + Circle")
    (35  . "35  -  Cross + Circle")
    (36  . "36  -  Tick + Circle")
    (64  . "64  -  Dot + Square")
    (65  . "65  -  Square")
    (66  . "66  -  Plus + Square")
    (67  . "67  -  Cross + Square")
    (68  . "68  -  Tick + Square")
    (96  . "96  -  Dot + Circle + Square")
    (97  . "97  -  Circle + Square")
    (98  . "98  -  Plus + Circle + Square")
    (99  . "99  -  Cross + Circle + Square")
    (100 . "100  -  Tick + Circle + Square"))
)

;; ---------------------------------------------------------------- helpers
;; number -> text: whole numbers as integers (1), others without trailing zeros (0.5)
(defun VSH:Fmt (n / s)
  (if (equal n (fix n) 1e-9)
    (itoa (fix n))
    (progn
      (setq s (rtos n 2 4))
      (while (and (wcmatch s "*`.*") (= (substr s (strlen s) 1) "0"))
        (setq s (substr s 1 (1- (strlen s)))))
      (if (= (substr s (strlen s) 1) ".") (setq s (substr s 1 (1- (strlen s)))))
      s
    )
  )
)

(defun VSH:Trim (s)
  (while (and (> (strlen s) 0) (= (substr s 1 1) " "))
    (setq s (substr s 2)))
  (while (and (> (strlen s) 0) (= (substr s (strlen s) 1) " "))
    (setq s (substr s 1 (1- (strlen s)))))
  s
)

;; "0 3 5" -> (0 3 5)
(defun VSH:SplitIdx (s / out tok i ch)
  (setq out '() tok "" i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (= ch " ")
      (progn (if (/= tok "") (setq out (cons (atoi tok) out))) (setq tok ""))
      (setq tok (strcat tok ch))
    )
    (setq i (1+ i))
  )
  (if (/= tok "") (setq out (cons (atoi tok) out)))
  (reverse out)
)

;; escape wildcard characters so a layer name matches only itself in ssget
(defun VSH:Esc (s / i ch out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (setq out
      (strcat out
        (if (member ch '("`" "#" "@" "." "*" "?" "~" "[" "]" ","))
          (strcat "`" ch)
          ch)))
    (setq i (1+ i))
  )
  out
)

;; sorted list of layer names (xref layers "xx|yy" left out - cannot be edited)
(defun VSH:Layers (/ rec nm lst)
  (setq rec (tblnext "LAYER" T) lst '())
  (while rec
    (setq nm (cdr (assoc 2 rec)))
    (if (not (wcmatch nm "*|*")) (setq lst (cons nm lst)))
    (setq rec (tblnext "LAYER"))
  )
  (acad_strlsort lst)
)

(defun VSH:IndexOf (item lst / i pos)
  (setq i 0)
  (foreach x lst
    (if (and (null pos) (equal x item)) (setq pos i))
    (setq i (1+ i))
  )
  pos
)

;; validate the open dialog; T = ok to close
(defun VSH:Ok (/ sel h hp)
  (setq sel (VSH:SplitIdx (get_tile "lyr")))
  (cond
    ((null sel)
     (alert "Select at least one layer.") nil)
    ((and (/= (setq h (VSH:Trim (get_tile "h"))) "")
          (or (null (distof h 2)) (<= (distof h 2) 0.0)))
     (alert "Text Height must be a number greater than 0.")
     (mode_tile "h" 2) nil)
    ((and (/= (setq hp (VSH:Trim (get_tile "hp"))) "")
          (null (distof hp 2)))
     (alert "Point Size must be a number.")
     (mode_tile "hp" 2) nil)
    (T
     (setq VSH-*sel* sel
           VSH-*h*   h
           VSH-*pm*  (atoi (get_tile "pdm"))
           VSH-*hp*  hp)
     T)
  )
)

;; ---------------------------------------------------------------- dialog
;; Returns (layerNames heightStr pdmodeValue sizeStr) or nil if cancelled.
(defun VSH:Dialog (layers / dclfile fp id res initSel curPm pmIdx i)
  (setq dclfile (vl-filename-mktemp "vidSurveySettings" nil ".dcl"))
  (setq fp (open dclfile "w"))
  (if (null fp)
    (progn (princ "\nVIDSURVEYSETTINGS: cannot create the dialog file.") nil)
    (progn
      (foreach ln
        '("vsh_dlg : dialog {"
          "  label = \"Survey Height\";"
          "  : list_box {"
          "    key = \"lyr\"; label = \"Layers:\";"
          "    multiple_select = true; width = 40; height = 12;"
          "  }"
          "  : edit_box { key = \"h\";  label = \"Text Height: \"; edit_width = 12; }"
          "  : popup_list { key = \"pdm\"; label = \"PDMODE: \"; edit_width = 30; }"
          "  : edit_box { key = \"hp\"; label = \"Point Size: \";  edit_width = 12; }"
          "  ok_cancel;"
          "}")
        (write-line ln fp)
      )
      (close fp)
      (setq id (load_dialog dclfile))
      (if (and id (> id 0) (new_dialog "vsh_dlg" id))
        (progn
          ;; layer list + default selection (last used, else 1-SURVEY)
          (start_list "lyr")
          (mapcar 'add_list layers)
          (end_list)
          (setq initSel "")
          (foreach nm
            (if VSH-*lastLayers* VSH-*lastLayers* '("1-SURVEY"))
            (if (setq i (VSH:IndexOf nm layers))
              (setq initSel (strcat initSel (if (= initSel "") "" " ") (itoa i)))
            )
          )
          (if (/= initSel "") (set_tile "lyr" initSel))

          (set_tile "h" (VSH:Fmt VSH-*defH*))

          ;; PDMODE drop-down, current value preselected
          (start_list "pdm")
          (foreach p VSH-*pdmodes* (add_list (cdr p)))
          (end_list)
          (setq curPm (getvar "PDMODE") pmIdx 0 i 0)
          (foreach p VSH-*pdmodes*
            (if (= (car p) curPm) (setq pmIdx i))
            (setq i (1+ i))
          )
          (set_tile "pdm" (itoa pmIdx))

          (set_tile "hp" (VSH:Fmt VSH-*defHP*))

          (action_tile "accept" "(if (VSH:Ok) (done_dialog 1))")
          (action_tile "cancel" "(done_dialog 0)")
          (setq res (start_dialog))
        )
        (princ "\nVIDSURVEYSETTINGS: unable to load the dialog.")
      )
      (if (and id (> id 0)) (unload_dialog id))
      (vl-file-delete dclfile)
      (if (= res 1)
        (list (mapcar '(lambda (n) (nth n layers)) VSH-*sel*)
              VSH-*h*
              (car (nth VSH-*pm* VSH-*pdmodes*))
              VSH-*hp*)
      )
    )
  )
)

;; ---------------------------------------------------------------- command
(defun c:VIDSURVEYSETTINGS (/ *error* layers opt lays h pm hp filt ss i ent ed nTxt
                    nFail undoOn)
  (defun *error* (msg)
    (if undoOn (command "_.UNDO" "_End"))
    (if (and msg
             (not (member (strcase msg) '("FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nVIDSURVEYSETTINGS error: " msg)))
    (princ)
  )

  (setq layers (VSH:Layers))
  (if (setq opt (VSH:Dialog layers))
    (progn
      (setq lays (car opt)
            h    (cadr opt)
            pm   (caddr opt)
            hp   (cadddr opt))
      (setq VSH-*lastLayers* lays)

      ;; one filter string for all chosen layers
      (setq filt "")
      (foreach l lays
        (setq filt (strcat filt (if (= filt "") "" ",") (VSH:Esc l))))

      (command "_.UNDO" "_Begin")
      (setq undoOn T)

      ;; ---- TEXT / MTEXT height
      (if (/= h "")
        (progn
          (setq nTxt 0 nFail 0)
          (if (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT") (cons 8 filt))))
            (progn
              (setq i 0)
              (repeat (sslength ss)
                (setq ent (ssname ss i))
                (setq ed  (entget ent))
                (if (entmod (subst (cons 40 (distof h 2)) (assoc 40 ed) ed))
                  (progn (entupd ent) (setq nTxt (1+ nTxt)))
                  (setq nFail (1+ nFail)))
                (setq i (1+ i))
              )
              (princ (strcat "\nText height set to " (VSH:Fmt (distof h 2)) " on " (itoa nTxt)
                             " TEXT/MTEXT object(s)."))
              (if (> nFail 0)
                (princ (strcat "\n" (itoa nFail)
                               " object(s) not changed (locked layer).")))
            )
            (princ "\nNo TEXT or MTEXT found on the selected layers.")
          )
        )
      )

      ;; ---- POINT style / size (drawing-wide)
      (if (ssget "_X" (list '(0 . "POINT") (cons 8 filt)))
        (progn
          (setvar "PDMODE" pm)
          (if (/= hp "") (setvar "PDSIZE" (distof hp 2)))
          (command "_.REGEN")
          (princ (strcat "\nPDMODE set to " (itoa pm)
                         (if (/= hp "") (strcat ", point size " (VSH:Fmt (distof hp 2))) "") "."))
        )
        (princ "\nNo POINT found on the selected layers - PDMODE / PDSIZE not changed.")
      )

      (command "_.UNDO" "_End")
      (setq undoOn nil)
    )
  )
  (princ)
)

(princ "\nVIDSURVEYSETTINGS loaded.  Type VIDSURVEYSETTINGS.")
(princ)
