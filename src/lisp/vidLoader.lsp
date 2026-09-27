;;; ============================================================================
;;; LogicDWG - vidLoader_v02.lsp
;;; Release: 1.1.2 | Civil 3D 2026
;;;
;;; Folder structure:
;;;
;;; vid\
;;; │   vidLoader_v02.lsp
;;; │
;;; └───tools\
;;;         logicDwg_v01.lsp
;;;         vidDgpsToLine_v01.lsp
;;;         vidDgpsToSp_v01.lsp
;;;         vidMapZone_v01.lsp
;;; ============================================================================

(vl-load-com)

(defun LogicDWG:GetLoaderDir (/ p)
  (setq p (findfile "vidLoader.lsp"))
  (if p
    (vl-filename-directory p)
    nil
  )
)

(defun LogicDWG:LoadFile (folder filename / full result)
  (setq full (strcat folder "\\" filename))

  (if (findfile full)
    (progn
      (setq result (load full))
      (princ (strcat "\n  [OK] " filename))
      T
    )
    (progn
      (princ (strcat "\n  [MISSING] " filename))
      nil
    )
  )
)

(defun LogicDWG:LoadAll (/ root toolsFolder files loaded missing)

  ;; Get folder where this loader is located
  (setq root (LogicDWG:GetLoaderDir))

  ;; Tools folder relative to loader
  (if root
    (setq toolsFolder (strcat root "\\tools"))
    (setq toolsFolder nil)
  )

  ;; Tool files
  (setq files
    '(
      "logicDwg.lsp"
      "vidDgpsToLine.lsp"
      "vidDgpsToSp.lsp"
      "vidMapZone.lsp"
    )
  )

  (setq loaded 0)
  (setq missing 0)

  (princ "\n============================================================")
  (princ "\n LogicDWG 1.1.2 - Loader v02")
  (princ "\n============================================================")

  (princ "\nFolder: .\\tools\\")

  (foreach f files
    (if
      (LogicDWG:LoadFile toolsFolder f)
      (setq loaded (1+ loaded))
      (setq missing (1+ missing))
    )
  )

  (princ "\n------------------------------------------------------------")

  (princ
    (strcat
      "\nLoaded : "
      (itoa loaded)
      " / "
      (itoa (length files))
    )
  )

  (princ
    (strcat
      "\nMissing: "
      (itoa missing)
    )
  )

  (if (= missing 0)
    (progn
      (princ "\n\nLogicDWG is ready.")
      (princ "\nCommands: VIDLOGICDWG, VIDDGPSTOLINE, VIDDGPSTOSP, VIDMAPZONE")
    )
    (princ "\n\nWARNING: One or more LogicDWG files are missing.")
  )

  (princ "\n============================================================\n")
  (princ)
)

(LogicDWG:LoadAll)

(princ)