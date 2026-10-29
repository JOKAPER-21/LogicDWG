;;; ============================================================================
;;; LogicDWG.lsp   |   Release 1.2.1   |   AutoCAD / Civil 3D 2026   |   Version 23
;;; Commands: LOGICDWG, VIDLOGICDWG
;;;
;;; The dialog is drawn in logicDwg.dcl (same folder as this file).
;;; To add a button:  1) add the button in logicDwg.dcl
;;;                   2) add ONE line for its key in LogicDWG:Buttons below
;;; ============================================================================

(vl-load-com)

;; One line per button:  (key  TYPE  argument  [lsp file to load first])
;;   ZONE = set Map Zone      argument = coordinate system, nil = GeoMap Off (runs VID:MapZone from vidMapZone.lsp, dialog closes)
;;   CMD  = run command       argument = command name (no "c:"), dialog closes
;;   CAD  = AutoCAD command   argument = built-in command name (e.g. "REVERSE"), dialog closes
;;   SET  = run command       same as CMD, but the dialog opens again after
(setq LogicDWG:Buttons
  '(("z43"      ZONE "UTM84-43N")
    ("z44"      ZONE "UTM84-44N")
    ("zoff"     ZONE nil)
    ("sp"       CMD  "VIDDGPSTOSP")
    ("trackEx"  CMD  "VIDLINETOEXPORTLEVEL" "vidDgpsToExportLevel.lsp")
    ("track"    CMD  "VIDDGPSTOLINE")
    ("chain"    CMD  "VIDCHAINAGERUNNER")
    ("ohe"      CMD  "VIDDGPSTOOHE")
    ("building" CMD  "VIDDGPSTOBOX")
    ("boundary" CMD  "VIDRWBOUNDARY" "vidRwBoundary.lsp")
    ("bankCutting" CMD "VIDBANKCUTTING" "vidBankCutting.lsp")
    ("tr"       CMD  "VIDTR" "vidTr.lsp")
    ("gradClean" CMD "VIDGRADIENTCLEANUP" "vidGradientCleanup.lsp")
    ("indexGrad" CMD "VIDINDEXGRADIENT" "vidIndexGradient.lsp")
    ("indexCurve" CMD "VIDINDEXCURVE" "vidIndexCurve.lsp")
    ("gradAlign" CMD "VIDGRADIENTALIGN" "vidGradientAlign.lsp")
    ("grad"     CMD  "VIDGRADIENT" "vidGradient.lsp")
    ("cadMerge" CMD  "VIDCADMERGELAYERS")
    ("cadOptimize" CMD "VIDCADOPTIMIZE" "vidCadOptimize.lsp")
    ("blockLayer" CMD "VIDBLOCKLAYER" "vidBlockLayer.lsp")
    ("settings" SET  "VIDDIMSETTINGS" "vidDimSettings.lsp")
    ("dimMToFt" CMD  "VIDDIMMTOFT" "vidDimMToFt.lsp")
    ("rotText" CMD   "VIDROTTEXT" "vidRotText.lsp")
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

;; Map Zone: runs the same code as the stand-alone VIDMAPZONE command
;; (VID:MapZone in vidMapZone.lsp), so both always behave the same.
;;   cs = "UTM84-43N" -> Z43,  "UTM84-44N" -> Z44,  nil -> GeoMap Off
(defun LogicDWG:Zone (cs / zone f)
  (setq zone
    (cond
      ((null cs) "Off")
      ((wcmatch cs "*44*") "Z44")
      (T "Z43")
    )
  )
  ;; load vidMapZone.lsp if it is not loaded yet
  (if (not (boundp 'VID:MapZone))
    (progn
      (setq f
        (cond
          ((findfile "vidMapZone.lsp"))
          (LogicDWG:ToolsDir (findfile (strcat LogicDWG:ToolsDir "\\vidMapZone.lsp")))
        )
      )
      (if f (load f))
    )
  )
  (if (boundp 'VID:MapZone)
    (VID:MapZone zone)
    (alert "vidMapZone.lsp is not loaded (or is an old version).\nLoad the tools first (vidLoader.lsp).")
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
    (if (member kind '(CMD CAD ZONE)) (setq again nil))
  )
  (princ)
)

(defun c:VIDLOGICDWG () (c:LOGICDWG))

(princ "\n[OK] LogicDWG loaded. Type VIDLOGICDWG to open.")
(princ)
