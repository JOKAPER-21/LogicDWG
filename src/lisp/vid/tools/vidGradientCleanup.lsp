;;; ============================================================================
;;; Vid Gradient Cleanup
;;; Command : VIDGRADIENTCLEANUP
;;; Version : v4 (silent - no report)    (AutoCAD and AutoCAD Civil 3D)
;;;
;;;   1. Select the gradient block(s)  (blocks made by VIDGRADIENT).
;;;   2. Bursts them with a built-in burst (same result as the Express Tools
;;;      BURST command): the block is exploded and every visible attribute
;;;      becomes MTEXT.  The Express BURST command is not used unless
;;;      VGC-UseBurstCommand is set to T (it is "Unknown command" in some
;;;      installs and printed errors on the command line).
;;;   3. Every TEXT created by the burst is converted to MTEXT (same position,
;;;      height, rotation, style, layer, colour and justification).
;;;
;;; Set VGC-OnlyGradient to T to burst only blocks named G_NEW_FINAL and skip
;;; any other selected block.
;;; ============================================================================

(vl-load-com)

(setq VGC-BlockName    "G_NEW_FINAL")
(setq VGC-OnlyGradient nil)
(setq VGC-UseBurstCommand nil)   ; T = try the Express Tools BURST command first
(setq VGC-OldError     nil)
(setq VGC-Doc          nil)
(setq VGC-OldEcho      nil)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------
(defun VGC-MTextSafe (s / i ch out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (member ch '("\\" "{" "}")) (setq out (strcat out "\\")))
    (setq out (strcat out ch) i (1+ i))
  )
  out
)

(defun VGC-Error (msg)
  (if VGC-Doc  (vl-catch-all-apply 'vla-EndUndoMark (list VGC-Doc)))
  (if VGC-OldEcho (setvar "CMDECHO" VGC-OldEcho))
  (setq *error* VGC-OldError)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
    (princ (strcat "\nVIDGRADIENTCLEANUP ERROR: " msg))
    (princ "\nVIDGRADIENTCLEANUP cancelled.")
  )
  (princ)
)

;; effective (non-anonymous) block name of an INSERT entity
(defun VGC-EffName (en / obj nm)
  (setq obj (vlax-ename->vla-object en))
  (setq nm (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p nm)
    (cdr (assoc 2 (entget en)))
    nm
  )
)

;; selection set -> selection set holding only gradient blocks
(defun VGC-FilterGradient (ss / out i en n)
  (setq out (ssadd) i 0 n 0)
  (while (< i (sslength ss))
    (setq en (ssname ss i))
    (if (= (strcase (VGC-EffName en)) (strcase VGC-BlockName))
      (progn (ssadd en out) (setq n (1+ n))))
    (setq i (1+ i))
  )
  (if (> n 0) out nil)
)

(defun VGC-BurstAvailable ()
  (if (or (getcname "BURST") (boundp 'c:burst)) T nil)
)

;; TEXT and MTEXT measure "middle" / "baseline" differently (descender space),
;; which leaves a small offset.  Fix: move the new MTEXT so the centre of its
;; bounding box sits exactly on the centre of the original text's bounding box.
(defun VGC-BoxCenter (en / obj mn mx a b)
  (setq obj (vlax-ename->vla-object en))
  (vla-GetBoundingBox obj 'mn 'mx)
  (setq a (vlax-safearray->list mn)
        b (vlax-safearray->list mx))
  (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0))
)

(defun VGC-AlignTo (newEn refEn height / c1 c2 dx dy ed p)
  (setq c1 (vl-catch-all-apply 'VGC-BoxCenter (list refEn))
        c2 (vl-catch-all-apply 'VGC-BoxCenter (list newEn)))
  (if (and (not (vl-catch-all-error-p c1)) (not (vl-catch-all-error-p c2)))
    (progn
      (setq dx (- (car c1) (car c2))
            dy (- (cadr c1) (cadr c2)))
      ;; only correct small differences (never move text far away)
      (if (and (< (sqrt (+ (* dx dx) (* dy dy))) (* 2.0 (max height 0.001)))
               (> (sqrt (+ (* dx dx) (* dy dy))) 1e-9))
        (progn
          (setq ed (entget newEn)
                p  (cdr (assoc 10 ed)))
          (entmod (subst (list 10 (+ (car p) dx) (+ (cadr p) dy) (caddr p))
                         (assoc 10 ed) ed))
          (entupd newEn)
        )
      )
    )
  )
  T
)

;; TEXT / ATTRIB entity data -> new MTEXT (same look).  Returns new ename or nil.
;; refEn (optional) = the original entity, used to correct the alignment.
(defun VGC-EdToMText (ed isAtt refEn / norm h v col row att ins txt data new)
  (setq norm (if (assoc 210 ed) (cdr (assoc 210 ed)) (list 0.0 0.0 1.0))
        h    (if (assoc 72 ed) (cdr (assoc 72 ed)) 0)
        v    (if isAtt
               (if (assoc 74 ed) (cdr (assoc 74 ed)) 0)    ; ATTRIB: vertical = 74
               (if (assoc 73 ed) (cdr (assoc 73 ed)) 0))   ; TEXT  : vertical = 73
        txt  (cdr (assoc 1 ed)))
  (if (null txt) (setq txt ""))

  ;; text justification -> MTEXT attachment point (1-9)
  (setq col (cond ((= h 1) 1) ((= h 2) 2) (T 0)))
  (setq row (cond ((= v 3) 0) ((= v 2) 1) (T 2)))
  (setq att (if (= h 4) 5 (+ 1 (* row 3) col)))

  ;; insertion point: group 10 for left/baseline, otherwise the alignment point
  (setq ins (if (and (= h 0) (= v 0)) (cdr (assoc 10 ed)) (cdr (assoc 11 ed))))
  (if (null ins) (setq ins (cdr (assoc 10 ed))))
  (setq ins (trans ins norm 0))

  (setq data
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 (cdr (assoc 8 ed)))
      '(100 . "AcDbMText")
      (cons 10 ins)
      (cons 40 (cdr (assoc 40 ed)))
      '(41 . 0.0)
      (cons 71 att)
      '(72 . 1)
      (cons 1 (VGC-MTextSafe txt))
      (cons 7 (if (assoc 7 ed) (cdr (assoc 7 ed)) "STANDARD"))
      (cons 210 norm)
      (cons 50 (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0))
    )
  )
  (if (assoc 62 ed)  (setq data (append data (list (assoc 62 ed)))))
  (if (assoc 420 ed) (setq data (append data (list (assoc 420 ed)))))
  (if (assoc 6 ed)   (setq data (append data (list (assoc 6 ed)))))
  (setq new (entmakex data))
  (if (and new refEn)
    (VGC-AlignTo new refEn (if (assoc 40 ed) (cdr (assoc 40 ed)) 1.0)))
  new
)

;; TEXT entity -> MTEXT, original erased.  T when converted.
(defun VGC-TextToMText (en)
  (if (VGC-EdToMText (entget en) nil en)
    (progn (entdel en) T)
    nil
  )
)

;; BURST command.  Raises an error (caught by the caller) when it cannot run.
(defun VGC-RunBurst (ss / guard)
  (setvar "CMDECHO" 0)
  (command "BURST" ss "")
  (setq guard 0)
  (while (and (> (getvar "CMDACTIVE") 0) (< guard 5))
    (command "")
    (setq guard (1+ guard))
  )
  T
)

(defun VGC-Erased (en / r)
  (setq r (vl-catch-all-apply
            '(lambda () (vlax-erased-p (vlax-ename->vla-object en)))
            nil))
  (cond
    ((vl-catch-all-error-p r) T)
    (r T)
    (T nil)
  )
)

;; Built-in burst for ONE block: visible attributes -> MTEXT, geometry exploded.
;; The MTEXT is made BEFORE the explode so it can be aligned to the live ATTRIB.
(defun VGC-OwnBurst (en / ad a made new obj res)
  (setq made '() a (entnext en))
  (while (and a (= (cdr (assoc 0 (setq ad (entget a)))) "ATTRIB"))
    (if (/= (logand 1 (if (assoc 70 ad) (cdr (assoc 70 ad)) 0)) 1)
      (if (setq new (VGC-EdToMText ad T a))
        (setq made (cons new made))))
    (setq a (entnext a))
  )
  (setq obj (vlax-ename->vla-object en))
  (setq res (vl-catch-all-apply 'vla-Explode (list obj)))
  (if (vl-catch-all-error-p res)
    (progn
      (foreach new made (entdel new))     ; undo the attribute text made above
      nil
    )
    (progn
      (vl-catch-all-apply 'vla-Delete (list obj))
      T
    )
  )
)

;; ---------------------------------------------------------------------------
;; Command
;; ---------------------------------------------------------------------------
(defun c:VIDGRADIENTCLEANUP
  (/ ss ss2 marker en newTexts attdefs n conv failed guard skipped res i own d t1)

  (vl-load-com)
  (setq VGC-OldError *error*)
  (setq *error* VGC-Error)
  (setq VGC-Doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq VGC-OldEcho (getvar "CMDECHO"))

  (cond
    (T
     (princ "\nSelect gradient block(s) to clean up: ")
     (setq ss (ssget '((0 . "INSERT"))))
     (cond
       ((null ss) nil)
       (T
        (setq skipped 0)
        (if VGC-OnlyGradient
          (progn
            (setq ss2 (VGC-FilterGradient ss))
            (setq skipped (- (sslength ss) (if ss2 (sslength ss2) 0)))
            (setq ss ss2)
          )
        )
        (if (null ss)
          nil
          (progn
            (vla-StartUndoMark VGC-Doc)

            ;; marker entity: everything created after it comes from the burst
            (setq marker
              (entmakex (list '(0 . "POINT") '(8 . "0") (list 10 0.0 0.0 0.0))))

            ;; 1. BURST ------------------------------------------------------
            (if (and VGC-UseBurstCommand (VGC-BurstAvailable))
              (setq res (vl-catch-all-apply 'VGC-RunBurst (list ss)))
              (setq res nil)
            )
            (if (vl-catch-all-error-p res)
              (progn
                (setq guard 0)
                (while (and (> (getvar "CMDACTIVE") 0) (< guard 5))
                  (vl-catch-all-apply 'command nil)
                  (setq guard (1+ guard))
                )
              )
            )
            (setvar "CMDECHO" VGC-OldEcho)

            ;; blocks BURST did not take -> built-in burst
            (setq i 0 own 0)
            (while (< i (sslength ss))
              (setq en (ssname ss i))
              (if (not (VGC-Erased en))
                (if (VGC-OwnBurst en) (setq own (1+ own)))
              )
              (setq i (1+ i))
            )

            ;; 2. collect the new TEXT, convert to MTEXT ---------------------
            (setq newTexts '() attdefs '() n 0)
            (setq en (entnext marker))
            (while en
              (setq n (1+ n))
              (cond
                ((= (cdr (assoc 0 (entget en))) "TEXT")
                 (setq newTexts (cons en newTexts)))
                ((= (cdr (assoc 0 (entget en))) "ATTDEF")
                 (setq attdefs (cons en attdefs)))
              )
              (setq en (entnext en))
            )

            (setq conv 0 failed 0)
            (foreach t1 (reverse newTexts)
              (if (VGC-TextToMText t1)
                (setq conv (1+ conv))
                (setq failed (1+ failed))
              )
            )

            (foreach d attdefs (entdel d))   ; ATTDEFs left by a plain explode
            (entdel marker)
            (vla-EndUndoMark VGC-Doc)

           )
        )
       )
     )
    )
  )
  (setq *error* VGC-OldError)
  (princ)
)

(princ "\nVIDGRADIENTCLEANUP loaded. Type VIDGRADIENTCLEANUP to run.")
(princ)
