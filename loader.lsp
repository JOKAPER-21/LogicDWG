;;; ============================================================================
;;; LogicDWG - loader.lsp
;;; ============================================================================
;;; Purpose:
;;;   Load the LogicDWG LISP tools from the standard Civil 3D 2026
;;;   user Support\vid folder.
;;;
;;; Default folder:
;;;   %APPDATA%\Autodesk\C3D 2026\enu\Support\vid
;;;
;;; Files expected in that folder:
;;;   logicDwg.lsp
;;;   vidDgpsToLine.lsp
;;;   vidDgpsToSp.lsp
;;;   vidMapZone.lsp
;;;
;;; Commands provided:
;;;   VIDLOGICDWG
;;;   VIDDGPSTOLINE
;;;   VIDDGPSTOSP
;;;   VIDMAPZONE
;;; ============================================================================

(vl-load-com)

;;; ---------------------------------------------------------------------------
;;; Get the LogicDWG installation folder.
;;; ---------------------------------------------------------------------------
(defun LogicDWG:GetInstallPath (/ appdata)
  (setq appdata (getenv "APPDATA"))
  (if (and appdata (/= appdata ""))
    (strcat appdata "\\Autodesk\\C3D 2026\\enu\\Support\\vid")
    nil
  )
)

;;; ---------------------------------------------------------------------------
;;; Load one LISP file and report the result.
;;; ---------------------------------------------------------------------------
(defun LogicDWG:LoadFile (folder filename / full result)
  (setq full (strcat folder "\\" filename))
  (if (findfile full)
    (progn
      (setq result (load full))
      (princ (strcat "\n  [OK] " filename))
      T
    )
    (progn
      (princ (strcat "\n  [MISSING] " full))
      nil
    )
  )
)

;;; ---------------------------------------------------------------------------
;;; Main loader.
;;; ---------------------------------------------------------------------------
(defun LogicDWG:LoadAll (/ folder loaded missing)
  (setq folder (LogicDWG:GetInstallPath))
  (setq loaded 0
        missing 0)

  (princ "\n")
  (princ "\n============================================================")
  (princ "\n LogicDWG - Civil 3D 2026 Loader")
  (princ "\n============================================================")

  (if (null folder)
    (progn
      (princ "\nERROR: Windows APPDATA environment variable was not found.")
      (princ)
    )
    (progn
      (princ (strcat "\nFolder: " folder))

      ;; Main launcher
      (if (LogicDWG:LoadFile folder "logicDwg.lsp")
        (setq loaded (1+ loaded))
        (setq missing (1+ missing))
      )

      ;; DGPS CSV -> line
      (if (LogicDWG:LoadFile folder "vidDgpsToLine.lsp")
        (setq loaded (1+ loaded))
        (setq missing (1+ missing))
      )

      ;; DGPS CSV -> survey points
      (if (LogicDWG:LoadFile folder "vidDgpsToSp.lsp")
        (setq loaded (1+ loaded))
        (setq missing (1+ missing))
      )

      ;; UTM / GeoMap zone
      (if (LogicDWG:LoadFile folder "vidMapZone.lsp")
        (setq loaded (1+ loaded))
        (setq missing (1+ missing))
      )

      (princ "\n------------------------------------------------------------")
      (princ (strcat "\nLoaded : " (itoa loaded) " / 4"))
      (princ (strcat "\nMissing: " (itoa missing)))

      (if (= missing 0)
        (progn
          (princ "\n")
          (princ "\nLogicDWG is ready.")
          (princ "\nCommands:")
          (princ "\n  VIDLOGICDWG")
          (princ "\n  VIDDGPSTOLINE")
          (princ "\n  VIDDGPSTOSP")
          (princ "\n  VIDMAPZONE")
        )
        (progn
          (princ "\n")
          (princ "\nWARNING: One or more LogicDWG files are missing.")
          (princ "\nCopy the required published LISP files into:")
          (princ (strcat "\n" folder))
        )
      )

      (princ "\n============================================================")
      (princ)
    )
  )
)

;;; Run automatically when APPLOAD loads loader.lsp.
(LogicDWG:LoadAll)

(princ)
