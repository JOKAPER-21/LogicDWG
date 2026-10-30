```lisp
;;; ============================================================================
;;; LogicDWG - vidOpenLogicDWGGuide.lsp
;;; Release: 1.0.0 | Civil 3D 2026
;;; Description: Opens LogicDWG Online Guide.
;;; ============================================================================

(vl-load-com)

(defun c:VIDLOGICDWGGUIDE (/ url result)

  (setq url
    "https://jokaper-21.github.io/LogicDWG/src/guide/index.html"
  )

  (setq result
    (startapp "explorer.exe" url)
  )

  (if result
    (princ "\nLogicDWG Online Guide opened successfully.")
    (princ "\nERROR: Unable to open LogicDWG Online Guide.")
  )

  (princ)
)

(princ "\nLogicDWG Guide loaded.")
(princ "\nCommand: VIDLOGICDWGGUIDE")
(princ)
```