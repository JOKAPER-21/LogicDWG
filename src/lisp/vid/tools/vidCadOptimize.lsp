;;; ==========================================================================
;;; vidCadOptimize.lsp  (final)
;;; One-command optimizer for AutoCAD Civil 3D (also works in plain AutoCAD).
;;; No dialogs, no questions - every prompt is answered by code.
;;;
;;; Command:  VIDCADOPTIMIZE   (short alias: VCO)
;;;
;;;   PURGE : ALL unused objects/types, repeated until nothing more can be
;;;           removed (this clears nested items), plus zero-length geometry,
;;;           empty text and orphaned data - no confirmation
;;;   AUDIT : Fix any errors detected? = Yes
;;;
;;; The whole run is one UNDO group. The drawing is NOT saved automatically.
;;; ==========================================================================

(vl-load-com)

(setq *vco-max-passes* 8)   ; safety cap for repeated purge passes
(setq *vco-autorun* nil)    ; set to T to start cleaning automatically on load

;; --------------------------------------------------------------------------
;; Helpers
;; --------------------------------------------------------------------------
(defun vco:civil-p ( / r)
  (setq r (vl-catch-all-apply 'getvar '("PRODUCT")))
  (and (= (type r) 'STR) (wcmatch (strcase r) "*CIVIL*"))
)

;; Decide the answer for whatever prompt the command is currently showing.
(defun vco:answer ( / p)
  (setq p (strcase (getvar "LASTPROMPT")))
  (cond
    ;; "Enter type of unused objects to purge [...]" -> Enter = finish command
    ((wcmatch p "*ENTER TYPE OF*")   "")
    ;; "Enter name(s) to purge <*>:" -> * (everything)
    ((wcmatch p "*ENTER NAME*")      "*")
    ;; "Verify each name to be purged? [Yes/No]" -> No (purge without asking)
    ((wcmatch p "*VERIFY*")          "_No")
    ;; "Enter name(s) to purge <*>:" -> * (everything)
    ((wcmatch p "*NAME*")            "*")
    ;; any other Yes/No question (audit fix, nested/orphaned, confirmation) -> Yes
    ((wcmatch p "*YES/NO*,*`[YES*")  "_Yes")
    ;; name prompts etc. -> accept default (*)
    (T                               "")
  )
)

;; Answer prompts automatically until the running command ends.
(defun vco:drain ( / n)
  (setq n 0)
  (while (and (> (getvar "CMDACTIVE") 0) (< n 40))
    (command (vco:answer))
    (setq n (1+ n))
  )
  (if (> (getvar "CMDACTIVE") 0) (command))   ; safety: cancel if still active
)

;; Number of entries in a dictionary.
(defun vco:dictcount (d / n x)
  (setq n 0)
  (foreach x (entget d)
    (if (= (car x) 3) (setq n (1+ n)))
  )
  n
)

;; Rough size of the drawing database: symbol tables + named dictionaries
;; + entities. Used only to detect whether a purge pass removed anything.
(defun vco:count ( / n tn e ss x sub)
  (setq n 0)
  (foreach tn '("BLOCK" "LAYER" "LTYPE" "STYLE" "DIMSTYLE" "APPID" "VIEW" "UCS" "VPORT")
    (setq e (tblnext tn T))
    (while e
      (setq n (1+ n))
      (setq e (tblnext tn))
    )
  )
  (if (setq ss (ssget "_X"))
    (setq n (+ n (sslength ss)))
  )
  (vl-catch-all-apply
    '(lambda ()
       (foreach x (entget (namedobjdict))
         (if (member (car x) '(350 360))
           (progn
             (setq sub (cdr x))
             (if (= (cdr (assoc 0 (entget sub))) "DICTIONARY")
               (setq n (+ n (vco:dictcount sub)))
             )
           )
         )
       )
     )
  )
  n
)

;; --------------------------------------------------------------------------
;; PURGE
;; --------------------------------------------------------------------------
;; Run one -PURGE option (A = All, Z = Zero-length, E = Empty text, O = Orphaned).
;; For All the prompts are answered: type = All, name(s) = *, verify = No.
(defun vco:purge-opt (opt)
  (command "_.-PURGE" opt)                       ; type of unused objects = All / Z / E / O
  (if (and (> (getvar "CMDACTIVE") 0)
           (wcmatch (strcase (getvar "LASTPROMPT")) "*NAME*"))
    (command "*")                                ; name(s) to purge = *
  )
  (vco:drain)                                    ; verify each name = No, then finish
)

;; Repeat "Purge All" until the drawing stops shrinking (handles nested items).
(defun vco:purge-all-stable ( / i prev cur)
  (setq i 0 prev -1 cur (vco:count))
  (while (and (< i *vco-max-passes*) (/= cur prev))
    (setq prev cur)
    (vco:purge-opt "_A")
    (setq cur (vco:count))
    (setq i (1+ i))
  )
)

(defun vco:purge ()
  (vco:purge-all-stable)
  (vco:purge-opt "_Z")                ; zero-length geometry
  (vco:purge-opt "_E")                ; empty text objects
  (vco:purge-opt "_O")                ; orphaned data
  (vco:purge-all-stable)              ; previous steps may free more items
)

;; --------------------------------------------------------------------------
;; AUDIT
;; --------------------------------------------------------------------------
(defun vco:audit ()
  (command "_.AUDIT")   ; "Fix any errors detected?" is answered Yes by vco:answer
  (vco:drain)
)

;; --------------------------------------------------------------------------
;; Commands
;; --------------------------------------------------------------------------
(defun vco:restore ()
  (if oldecho  (setvar "CMDECHO" oldecho))
  (if oldexp   (setvar "EXPERT"  oldexp))
  (if olddia   (setvar "CMDDIA"  olddia))
  (if oldfile  (setvar "FILEDIA" oldfile))
)

(defun c:VIDCADOPTIMIZE ( / *error* oldecho oldexp olddia oldfile undo-open c0 c1)
  (setq oldecho (getvar "CMDECHO")
        oldexp  (getvar "EXPERT")
        olddia  (getvar "CMDDIA")
        oldfile (getvar "FILEDIA")
        undo-open nil)

  (defun *error* (msg)
    ;; newer AutoCAD needs this before (command) is allowed inside *error*
    (if (boundp '*push-error-using-command*) (*push-error-using-command*))
    (while (> (getvar "CMDACTIVE") 0) (command))
    (if undo-open (command "_.UNDO" "_End"))
    (vco:restore)
    (if (boundp '*pop-error-mode*) (*pop-error-mode*))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nVID error: " msg))
    )
    (princ)
  )

  ;; no dialogs, no confirmations
  (setvar "CMDECHO" 0)
  (setvar "EXPERT"  5)
  (setvar "CMDDIA"  0)
  (setvar "FILEDIA" 0)
  (princ (strcat "\nVID CAD Optimize ("
                 (if (vco:civil-p) "Civil 3D" "AutoCAD")
                 ") ..."))

  (setq c0 (vco:count))
  (command "_.UNDO" "_Begin")
  (setq undo-open T)

  (princ "\n  Purging (all, nested, orphaned) ...")
  (vco:purge)

  (princ "\n  Auditing (fix errors = Yes) ...")
  (vco:audit)

  (command "_.UNDO" "_End")
  (setq undo-open nil)
  (vco:restore)

  (setq c1 (vco:count))
  (princ (strcat "\n  Removed approx. " (itoa (max 0 (- c0 c1))) " item(s)."))
  (princ "\nVID CAD Optimize finished. Save the drawing to keep the result.")
  (princ)
)

(defun c:VCO () (c:VIDCADOPTIMIZE))

(princ "\nvidCadOptimize.lsp loaded. Type VIDCADOPTIMIZE (or VCO) to optimize the drawing.")
(if *vco-autorun* (c:VIDCADOPTIMIZE))
(princ)
