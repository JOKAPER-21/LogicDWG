;;;===========================================================================
;;; loader.lsp
;;;
;;; Loads the entire LogicDWG toolset in one shot.
;;;
;;;   Load this file (APPLOAD, or via a startup suite / acaddoc.lsp entry)
;;;   and then type VIDLOGICDWG to open the launcher dialog.
;;;
;;; Commands made available after loading:
;;;   VIDLOGICDWG   - launcher dialog (Zone / DGPS to Survey Point / Rail Tracks)
;;;   SM          - Zone / GeoMap tool (43 / 44 / Off)
;;;   VIDDGPSTOSP   - DGPS CSV -> survey points
;;;   DGPS2PLINE  - DGPS CSV -> survey points + polyline / 3D polyline
;;;===========================================================================

(defun LogicDWG:this-dir ( / p)
  ;; Directory this loader.lsp itself lives in, so the package can be
  ;; loaded from any drive/path without editing this file.
  (setq p (findfile "loader.lsp"))
  (if p
      (vl-filename-directory p)
      (getvar "DWGPREFIX")
  )
)

(defun LogicDWG:load1 (relpath / base full)
  (setq base (LogicDWG:this-dir))
  (setq full (strcat base "\\" relpath))
  (if (findfile full)
      (progn (load full) T)
      (progn (princ (strcat "\nLogicDWG WARNING: file not found: " full)) nil)
  )
)

(princ "\n----------------------------------------")
(princ "\nLoading LogicDWG toolset...")
(princ "\n----------------------------------------")

(LogicDWG:load1 "src\\lisp\\sm\\sm.lsp")
(LogicDWG:load1 "src\\lisp\\vdgpstosp\\vdgpstosp_v01.lsp")
(LogicDWG:load1 "src\\lisp\\dgps2pline\\DGPS2PLINE_v12.LSP")
(LogicDWG:load1 "src\\lisp\\logicdwg\\logicdwg_v03.lsp")

(princ "\n----------------------------------------")
(princ "\nLogicDWG ready. Type VIDLOGICDWG to open the launcher.")
(princ "\n----------------------------------------")
(princ)
