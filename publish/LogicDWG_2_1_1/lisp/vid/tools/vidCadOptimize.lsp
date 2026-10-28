;;; ==========================================================================
;;; vidCadOptimize.lsp
;;; One-command optimizer for AutoCAD Civil 3D (also works in plain AutoCAD).
;;; No dialogs, no questions - every prompt is answered by code.
;;;
;;; Command:  VIDCADOPTIMIZE   (short alias: VCO)
;;;
;;;   PURGE : all unused objects/types, nested items (repeated passes),
;;;           zero-length geometry, empty text, orphaned data - no confirmation
;;;   AUDIT : Fix any errors detected? = Yes
;;; ==========================================================================

(vl-load-com)

(defun vco:civil-p ()
  (wcmatch (strcase (getvar "PRODUCT")) "*CIVIL*")
)

;; Decide the answer for whatever prompt the command is currently showing.
(defun vco:answer (/ p)
  (setq p (strcase (getvar "LASTPROMPT")))
  (cond
    ;; "Enter type of unused objects to purge [...]" -> Enter = finish command
    ((wcmatch p "*ENTER TYPE OF*")      "")
    ;; "Verify each name to be purged? [Yes/No]" -> No (purge without asking)
    ((wcmatch p "*VERIFY*")             "_No")
    ;; any other Yes/No question (audit fix, purge confirmation) -> Yes
    ((wcmatch p "*YES/NO*,*[YES*")      "_Yes")
    ;; name prompts etc. -> accept default (*)
    (T                                  "")
  )
)

;; Answer prompts automatically until the running command ends.
(defun vco:drain (/ n)
  (setq n 0)
  (while (and (> (getvar "CMDACTIVE") 0) (< n 40))
    (command (vco:answer))
    (setq n (1+ n))
  )
  (if (> (getvar "CMDACTIVE") 0) (command))   ; safety: cancel if still active
)

;; Run one -PURGE option (A = All, Z = Zero-length, E = Empty text, O = Orphaned)
(defun vco:purge-opt (opt)
  (command "_.-PURGE" opt)
  (vco:drain)
)

(defun vco:purge (/ i)
  ;; repeated passes so nested items are purged too
  (setq i 0)
  (while (< i 3)
    (vco:purge-opt "_A")
    (setq i (1+ i))
  )
  (vco:purge-opt "_Z")
  (vco:purge-opt "_E")
  (vco:purge-opt "_O")
  (vco:purge-opt "_A")   ; final pass: previous steps may free more items
)

(defun vco:audit ()
  (command "_.AUDIT")    ; "Fix any errors detected?" is answered Yes by vco:answer
  (vco:drain)
)

(defun c:VIDCADOPTIMIZE ( / *error* oldecho)
  (setq oldecho (getvar "CMDECHO"))
  (defun *error* (msg)
    (if (> (getvar "CMDACTIVE") 0) (command))
    (setvar "CMDECHO" oldecho)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nVID error: " msg)))
    (princ)
  )
  (setvar "CMDECHO" 0)
  (princ (strcat "\nVID CAD Optimize (" (if (vco:civil-p) "Civil 3D" "AutoCAD") ") ..."))
  (command "_.UNDO" "_Begin")
  (princ "\n  Purging ...")
  (vco:purge)
  (princ "\n  Auditing (fix errors = Yes) ...")
  (vco:audit)
  (command "_.UNDO" "_End")
  (setvar "CMDECHO" oldecho)
  (princ "\nVID CAD Optimize finished.")
  (princ)
)

(defun c:VCO () (c:VIDCADOPTIMIZE))

(princ "\nvidCadOptimize.lsp loaded. Type VIDCADOPTIMIZE (or VCO) to optimize the drawing.")
(princ)
