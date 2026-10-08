;;; ============================================================================
;;; LogicDWG Loader
;;; Release: 1.1.3 | Civil 3D 2026
;;; ============================================================================

(vl-load-com)

;; Tool files, in load order.
(setq LogicDWG:Files
  '("logicDwg.lsp"
    "vidCadMergeLayers.lsp"
    "vidChainageRunner.lsp"
    "vidDgpsToBox.lsp"
    "vidDgpsToExportLevel.lsp"
    "vidDgpsToLine.lsp"
    "vidDgpsToOhe.lsp"
    "vidDgpsToSp.lsp"
    "vidDimSettings.lsp"
    "vidMapZone.lsp"
  )
)

;; Find the tools folder. Works when this loader is on the Support File
;; Search Path, when it was loaded with APPLOAD / drag-and-drop from any
;; folder, or (last resort) by asking the user to pick logicDwg.lsp.
(defun LogicDWG:FindToolsDir (/ p dir)
  (cond
    ;; vid\tools\logicDwg.lsp  or  tools\logicDwg.lsp on the search path
    ((setq p (findfile "vid/tools/logicDwg.lsp")) (vl-filename-directory p))
    ((setq p (findfile "tools/logicDwg.lsp"))     (vl-filename-directory p))
    ((setq p (findfile "logicDwg.lsp"))           (vl-filename-directory p))
    ;; loader found -> .\tools next to it
    ((and (setq p (findfile "vidLoader.lsp"))
          (setq dir (strcat (vl-filename-directory p) "\\tools"))
          (findfile (strcat dir "\\logicDwg.lsp"))
     )
     dir
    )
    ;; Same folder as the last-used DWG / current folder
    ((and (setq dir (strcat (getvar "DWGPREFIX") "tools"))
          (findfile (strcat dir "\\logicDwg.lsp"))
     )
     dir
    )
    ;; Ask the user
    ((setq p (getfiled "Locate LogicDWG tools folder (select logicDwg.lsp)"
                       "" "lsp" 4))
     (vl-filename-directory p)
    )
    (T nil)
  )
)

;; Diagnostic / fallback: evaluate a file one top-level form at a time
;; (a top-level form starts with "(" in column 1) and report exactly which
;; form fails. Returns T when every form evaluated without error.
(defun LogicDWG:LoadByForm (full / f ln chunk chunks fail res)
  (setq f (open full "r"))
  (if (null f)
    nil
    (progn
      (while (setq ln (read-line f))
        (if (and (> (strlen ln) 0) (= (substr ln 1 1) "("))
          (progn
            (if chunk (setq chunks (cons chunk chunks)))
            (setq chunk ln)
          )
          (if chunk (setq chunk (strcat chunk "\n" ln)))
        )
      )
      (close f)
      (if chunk (setq chunks (cons chunk chunks)))
      (setq fail 0)
      (foreach c (reverse chunks)
        (setq res
          (vl-catch-all-apply
            '(lambda (x) (eval (read x)))
            (list c)
          )
        )
        (if (vl-catch-all-error-p res)
          (progn
            (setq fail (1+ fail))
            (princ (strcat "\n      FAILED FORM: "
                           (substr c 1 (min 70 (strlen c)))))
            (princ (strcat "\n      MESSAGE    : "
                           (vl-catch-all-error-message res)))
          )
        )
      )
      (= fail 0)
    )
  )
)

;; Load one file. An error in one file is reported but does not stop the rest.
;; If load fails, the file is retried form by form to show what is wrong.
(defun LogicDWG:LoadFile (folder filename / full res)
  (setq full (strcat folder "\\" filename))
  (cond
    ((not (findfile full))
     (princ (strcat "\n  [MISSING] " filename))
     nil
    )
    (T
     (setq res (vl-catch-all-apply 'load (list full)))
     (cond
       ((not (vl-catch-all-error-p res))
        (princ (strcat "\n  [OK]      " filename))
        T
       )
       (T
        (princ (strcat "\n  [ERROR]   " filename " : "
                       (vl-catch-all-error-message res)))
        (princ "\n    retrying form by form ...")
        (if (LogicDWG:LoadByForm full)
          (progn
            (princ (strcat "\n  [OK*]     " filename
                           " (loaded form by form)"))
            T
          )
          nil
        )
       )
     )
    )
  )
)

(defun LogicDWG:LoadAll (/ loaded missing)
  (setq LogicDWG:ToolsDir (LogicDWG:FindToolsDir))
  (setq loaded 0 missing 0)

  (princ "\n============================================================")
  (princ "\n LogicDWG 1.1.4")
  (princ "\n============================================================")

  (if (null LogicDWG:ToolsDir)
    (princ "\nTools folder not found. Put the 'tools' folder next to vidLoader.lsp.")
    (progn
      (princ (strcat "\nFolder: " LogicDWG:ToolsDir))
      (foreach f LogicDWG:Files
        (if (LogicDWG:LoadFile LogicDWG:ToolsDir f)
          (setq loaded (1+ loaded))
          (setq missing (1+ missing))
        )
      )
    )
  )

  (princ "\n------------------------------------------------------------")
  (princ (strcat "\nLoaded : " (itoa loaded) " / " (itoa (length LogicDWG:Files))))
  (princ (strcat "\nFailed : " (itoa (- (length LogicDWG:Files) loaded))))
  (if (= loaded (length LogicDWG:Files))
    (progn
      (princ "\n\nLogicDWG is ready. Type VIDLOGICDWG to open.")
      (princ "\nCommands: VIDCHAINAGERUNNER, VIDDGPSTOBOX, VIDLINETOEXPORTLEVEL, VIDDGPSTOLINE, VIDDGPSTOOHE, VIDDGPSTOSP, VIDDIMSETTINGS, VIDCADMERGELAYERS")
    )
    (princ "\n\nWARNING: LogicDWG did not load completely - see messages above.")
  )
  (princ "\n============================================================\n")
  (princ)
)

(LogicDWG:LoadAll)
(princ)
