;;; ============================================================================
;;; LogicDWG.lsp   |   Release 1.1.4   |   Civil 3D 2026   |   Version 11
;;; Commands: LOGICDWG, VIDLOGICDWG
;;;
;;; The dialog is drawn in logicDwg.dcl (same folder as this file).
;;; To add a button:  1) add the button in logicDwg.dcl
;;;                   2) add ONE line for its key in LogicDWG:Buttons below
;;; ============================================================================

(vl-load-com)

;; One line per button:  (key  TYPE  argument  [lsp file to load first])
;;   ZONE = set Map Zone      argument = coordinate system, nil = GeoMap Off
;;   CMD  = run command       argument = command name (no "c:"), dialog closes
;;   CAD  = AutoCAD command   argument = built-in command name (e.g. "REVERSE"), dialog closes
;;   SET  = run command       same as CMD, but the dialog opens again after
(setq LogicDWG:Buttons
  '(("z43"      ZONE "UTM84-43N")
    ("z44"      ZONE "UTM84-44N")
    ("zoff"     ZONE nil)
    ("sp"       CMD  "VIDDGPSTOSP")
    ("track"    CMD  "VIDDGPSTOLINE")
    ("chainRev" CAD  "REVERSE")
    ("chain"    CMD  "VIDCHAINAGERUNNER")
    ("ohe"      CMD  "VIDDGPSTOOHE")
    ("building" CMD  "VIDDGPSTOBOX")
    ("cadMerge" CMD  "VIDCADMERGELAYERS")
    ("settings" SET  "VIDDIMSETTINGS" "vidDimSettings.lsp")
  )
)


;; Tools folder (set by vidLoader.lsp). Keep it if already set.
(if (not (boundp 'LogicDWG:ToolsDir)) (setq LogicDWG:ToolsDir nil))

;; Find logicDwg.dcl. NOTE: AutoLISP "or" / "and" return only T or nil, never a
;; value, so "cond" is used wherever a path has to be returned.
(defun LogicDWG:FindDcl ( / f)
  (cond
    ((and LogicDWG:ToolsDir
          (setq f (findfile (strcat LogicDWG:ToolsDir "\\logicDwg.dcl"))))
     f)
    ((setq f (findfile "logicDwg.dcl")) f)
    ((and (setq f (findfile "logicDwg.lsp"))
          (setq f (findfile (strcat (vl-filename-directory f) "\\logicDwg.dcl"))))
     f)
    (T nil)
  )
)

;; Show the dialog. Returns the clicked button line, or nil if cancelled.
(defun LogicDWG:Show (/ dcl id n code b)
  (if (not (setq dcl (LogicDWG:FindDcl)))
    (alert "logicDwg.dcl not found.\nKeep it in the same folder as logicDwg.lsp.")
    (progn
      (setq id (load_dialog dcl))
      (if (new_dialog "logicDwg" id)
        (progn
          (setq n 0)
          (foreach b LogicDWG:Buttons
            (setq n (1+ n))
            (action_tile (car b) (strcat "(done_dialog " (itoa n) ")"))
          )
          (action_tile "cancel" "(done_dialog 0)")
          (setq code (start_dialog))
        )
        (alert "logicDwg.dcl has an error - the dialog could not open.")
      )
      (unload_dialog id)
    )
  )
  (if (and code (> code 0)) (nth (1- code) LogicDWG:Buttons))
)

;; Map Zone: set coordinate system + GeoMap (cs = nil turns GeoMap off).
(defun LogicDWG:Zone (cs / echo r)
  (setq echo (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  ;; "command" cannot be passed to vl-catch-all-apply, so wrap it in a lambda.
  (setq r
    (vl-catch-all-apply
      '(lambda (cs)
         (if cs
           (progn
             (command "._MAPCSASSIGN" cs)
             (command "._GEOMAP" "_Hybrid")
             (command "._ZOOM" "_E")
           )
           (command "._GEOMAP" "_Off")
         )
       )
      (list cs)
    )
  )
  (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
  (setvar "CMDECHO" echo)
  (princ
    (cond
      ((vl-catch-all-error-p r) (strcat "\nLogicDWG zone error: " (vl-catch-all-error-message r)))
      (cs (strcat "\nCoordinate system " cs " set."))
      (T "\nGeoMap turned Off.")
    )
  )
)

;; Start a built-in AutoCAD command (the command then asks you for input).
(defun LogicDWG:Cad (name)
  (command (strcat "._" name))
)

;; Run a command by name (loads its lsp file first if it is not loaded yet).
(defun LogicDWG:Run (name file / sym f)
  (setq sym (read (strcat "c:" name)))
  (if (and file (not (boundp sym)))
    (progn
      (setq f
        (cond
          ((findfile file))
          (LogicDWG:ToolsDir (findfile (strcat LogicDWG:ToolsDir "\\" file)))
        )
      )
      (if f (load f))
    )
  )
  (if (boundp sym)
    (eval (list sym))
    (alert (strcat "Command " (strcase name) " is not loaded.\nLoad the tools first (vidLoader.lsp)."))
  )
)

;; Run one step. An error is printed with the step name, so you can see
;; whether the dialog or a tool failed. Cancel / ESC is silent.
;; (No local *error* here: the tools set their own *error* and would replace it.)
(defun LogicDWG:Try (step fn args / r)
  (setq r (vl-catch-all-apply fn args))
  (cond
    ((not (vl-catch-all-error-p r)) r)
    (T
     (setq r (vl-catch-all-error-message r))
     (if (not (wcmatch (strcase r) "*CANCEL*,*EXIT*"))
       (princ (strcat "\nLogicDWG [" step "]: " r))
     )
     nil
    )
  )
)

(defun c:LOGICDWG (/ again b kind)
  (setq again T)
  (while (and again (setq b (LogicDWG:Try "dialog" 'LogicDWG:Show nil)))
    (setq kind (cadr b))
    (cond
      ((eq kind 'ZONE)
       (LogicDWG:Try (car b) 'LogicDWG:Zone (list (caddr b))))
      ((eq kind 'CAD)
       (LogicDWG:Try (caddr b) 'LogicDWG:Cad (list (caddr b))))
      (T
       (LogicDWG:Try (caddr b) 'LogicDWG:Run (list (caddr b) (cadddr b))))
    )
    (if (member kind '(CMD CAD)) (setq again nil))
  )
  (princ)
)

(defun c:VIDLOGICDWG () (c:LOGICDWG))

(princ "\n[OK] LogicDWG loaded. Type VIDLOGICDWG to open.")
(princ)
