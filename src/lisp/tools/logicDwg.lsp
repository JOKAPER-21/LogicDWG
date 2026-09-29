;;; ============================================================================
;;; LogicDWG.lsp
;;; Release: 1.1.2 | Civil 3D 2026
;;; Version: 05
;;; ============================================================================
;;;
;;; Commands: LOGICDWG, VIDLOGICDWG
;;;
;;; HOW TO CHANGE THINGS - everything you normally edit is in the two
;;; SETTINGS blocks below (no need to touch the code underneath):
;;;
;;;   LogicDWG:Layout   boxes, buttons, and what each button does
;;;   LogicDWG:ZoneCS   coordinate-system code used by each Map Zone button
;;;
;;; To add a button:
;;;   1. add one line to LogicDWG:Layout
;;;   2. add the same key / label line to logicDwg.dcl
;;;      (if logicDwg.dcl is not found, the dialog is built from
;;;       LogicDWG:Layout automatically)
;;; ============================================================================

(vl-load-com)


;;; ============================================================================
;;; SETTINGS  (edit here)
;;; ============================================================================

;; Dialog layout.  ("Box label"  (key "Button label" TYPE "argument") ...)
;;   TYPE ZONE = Map Zone button   (argument = key in LogicDWG:ZoneCS, or "OFF")
;;   TYPE CMD  = run a command     (argument = command name, without "c:")
(setq LogicDWG:Layout
  '(("Map Zone"
      ("z43"   "43"           ZONE "43")
      ("z44"   "44"           ZONE "44")
      ("zoff"  "Off"          ZONE "OFF")
    )
    ("Generate from CSV"
      ("sp"        "Survey Point"    CMD "VIDDGPSTOSP")
      ("track"     "Rail Track"      CMD "VIDDGPSTOLINE")
      ("chainage"  "Chainage Runner" CMD "VIDCHAINAGERUNNER")
      ("ohe"       "Ohe"             CMD "VIDDGPSTOOHE")
    )
  )
)

;; Coordinate system assigned by each Map Zone button.
(setq LogicDWG:ZoneCS
  '(("43" . "UTM84-43N")
    ("44" . "UTM84-44N")
  )
)

;; Where the dialog comes from.
;;   nil = built from LogicDWG:Layout above (default: no file needed)
;;   T   = use logicDwg.dcl (falls back to the built-in dialog if the file
;;         cannot be used, and prints which file it tried)
(setq LogicDWG:UseDclFile nil)


;;; ============================================================================
;;; DIALOG FILE (logicDwg.dcl, or generated from LogicDWG:Layout)
;;; ============================================================================

(defun LogicDWG:TryDir (baseFile subPath / p)
  (if (setq p (findfile baseFile))
    (findfile (strcat (vl-filename-directory p) subPath))
    nil
  )
)

(defun LogicDWG:FindDcl ( / p)
  (cond
    ((setq p (LogicDWG:TryDir "logicDwg.lsp" "\\logicDwg.dcl")) p)
    ((setq p (LogicDWG:TryDir "vidLoader.lsp" "\\tools\\logicDwg.dcl")) p)
    ((setq p (LogicDWG:TryDir "vidLoader.lsp" "\\logicDwg.dcl")) p)
    ((setq p (findfile "logicDwg.dcl")) p)
    (T nil)
  )
)

;; DCL text built from LogicDWG:Layout (same output as logicDwg.dcl).
(defun LogicDWG:DclLines (/ lines)
  (setq lines (list "logicDwg : dialog {" "  label = \"Logic DWG\";"))
  (foreach box LogicDWG:Layout
    (setq lines
      (append lines
        (list "  : boxed_row {" (strcat "    label = \"" (car box) "\";"))
      )
    )
    (foreach b (cdr box)
      (setq lines
        (append lines
          (list
            (strcat "    : button { key = \"" (nth 0 b) "\"; label = \"" (nth 1 b)
                    "\"; width = 14; fixed_width = true; }")
          )
        )
      )
    )
    (setq lines (append lines (list "  }")))
  )
  (append lines
    (list
      "  spacer_1;"
      "  : row {"
      "    alignment = centered;"
      "    : button { key = \"close\"; label = \"Close\"; width = 12; fixed_width = true; is_cancel = true; }"
      "  }"
      "}"
    )
  )
)

(defun LogicDWG:WriteTempDcl (/ dir path f)
  (setq dir
    (cond
      ((getenv "TEMP"))
      ((getenv "TMP"))
      (T (getvar "DWGPREFIX"))
    )
  )
  (setq path (strcat dir "\\logicDwg_" (itoa (getvar "MILLISECS")) ".dcl"))
  (setq f (open path "w"))
  (if f
    (progn
      (foreach ln (LogicDWG:DclLines) (write-line ln f))
      (close f)
      path
    )
    nil
  )
)


;;; ============================================================================
;;; DIALOG
;;; ============================================================================

;; All buttons as one flat list: (key label type arg) ...
(defun LogicDWG:Buttons ()
  (apply 'append (mapcar 'cdr LogicDWG:Layout))
)

;; Try to open the dialog from one DCL file.
;; Returns (opened . code): opened = T when the dialog was really displayed,
;; code = number of the clicked button (0 = closed).
(defun LogicDWG:TryShow (dclFile buttons / dclId code n opened)
  (setq opened nil
        code 0)
  (setq dclId (load_dialog dclFile))
  (if (and dclId (>= dclId 1))
    (progn
      (if (new_dialog "logicDwg" dclId)
        (progn
          ;; every button returns its position in the list (1, 2, 3 ...)
          (setq n 0)
          (foreach b buttons
            (setq n (1+ n))
            (vl-catch-all-apply
              'action_tile
              (list (car b) (strcat "(done_dialog " (itoa n) ")"))
            )
          )
          (action_tile "close" "(done_dialog 0)")
          (setq opened T)
          (setq code (start_dialog))
        )
      )
      (unload_dialog dclId)
    )
  )
  (cons opened (if (numberp code) code 0))
)

;; Show the dialog once.
;; Returns the clicked button (key label type arg), or nil if closed.
;; The dialog is built from LogicDWG:Layout unless LogicDWG:UseDclFile is T.
;; If logicDwg.dcl is then missing or has any error, the built-in dialog
;; is used instead, so the dialog always opens.
(defun LogicDWG:Show (/ buttons dclFile tempFile res)
  (setq buttons (LogicDWG:Buttons))
  (setq dclFile (if LogicDWG:UseDclFile (LogicDWG:FindDcl) nil))

  (if dclFile
    (setq res (LogicDWG:TryShow dclFile buttons))
  )

  (if (or (null res) (not (car res)))
    (progn
      (if dclFile
        (princ (strcat "\nLogicDWG: could not use " dclFile " - using built-in dialog."))
      )
      (setq tempFile (LogicDWG:WriteTempDcl))
      (if tempFile
        (progn
          (setq res (LogicDWG:TryShow tempFile buttons))
          (vl-file-delete tempFile)
        )
      )
      (if (or (null res) (not (car res)))
        (alert "LogicDWG: the dialog could not be opened.")
      )
    )
  )

  (if (and res (> (cdr res) 0))
    (nth (1- (cdr res)) buttons)
    nil
  )
)


;;; ============================================================================
;;; ACTIONS
;;; ============================================================================

;; Map Zone: assign coordinate system + GeoMap, or turn GeoMap off.
(defun LogicDWG:ZoneApply (mode / cs)
  (if (= mode "OFF")
    (command "._GEOMAP" "_Off")
    (progn
      (setq cs (cdr (assoc mode LogicDWG:ZoneCS)))
      (if (null cs) (error (strcat "No coordinate system set for zone " mode)))
      (command "._MAPCSASSIGN" cs)
      (command "._GEOMAP" "_Hybrid")
      (command "._ZOOM" "_E")
    )
  )
  T
)

(defun LogicDWG:Zone (mode / oldEcho res)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq res (vl-catch-all-apply 'LogicDWG:ZoneApply (list mode)))
  ;; If a command was left waiting for input, cancel it.
  (repeat 3
    (if (> (getvar "CMDACTIVE") 0) (command))
  )
  (setvar "CMDECHO" oldEcho)
  (if (vl-catch-all-error-p res)
    (princ (strcat "\nLogicDWG zone error: " (vl-catch-all-error-message res)))
    (princ
      (if (= mode "OFF")
        "\nGeoMap turned Off."
        (strcat "\nZone " mode " (" (cdr (assoc mode LogicDWG:ZoneCS)) ") set.")
      )
    )
  )
  (princ)
)

;; Start a tool command by name (called after the dialog has closed).
(defun LogicDWG:RunCmd (name / sym)
  (setq sym (read (strcat "c:" name)))
  (if (and (boundp sym)
           (member (type (eval sym)) '(USR SUBR EXRXSUBR))
      )
    (eval (list sym))
    (alert
      (strcat
        "Command " (strcase name) " is not loaded.\n\n"
        "Load the LogicDWG tools first (vidLoader.lsp)."
      )
    )
  )
)


;;; ============================================================================
;;; COMMANDS
;;; ============================================================================

(defun c:LOGICDWG (/ *error* again picked kind arg)

  ;; Quiet handler: ESC / cancel inside a tool is not reported as an error.
  (defun *error* (msg)
    (if (and msg
             (not (member (strcase msg) '("FUNCTION CANCELLED" "QUIT / EXIT ABORT")))
        )
      (princ (strcat "\nLogicDWG error: " msg))
    )
    (princ)
  )

  (setq again T)
  (while again
    (setq picked (LogicDWG:Show))
    (if (null picked)
      (setq again nil)
      (progn
        (setq kind (nth 2 picked)
              arg  (nth 3 picked))
        (cond
          ;; Zone buttons keep the dialog open
          ((eq kind 'ZONE) (LogicDWG:Zone arg))
          ;; Tool buttons close the dialog, then run the command
          ((eq kind 'CMD)
           (setq again nil)
           (LogicDWG:RunCmd arg)
          )
          (T (setq again nil))
        )
      )
    )
  )
  (princ)
)

(defun c:VIDLOGICDWG ()
  (c:LOGICDWG)
)

(princ "\n[OK] LogicDWG loaded. Type VIDLOGICDWG to open.")
(princ)