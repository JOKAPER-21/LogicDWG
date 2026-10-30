;;; ===========================================================================
;;; Txt2Mtext.lsp
;;; Command : VIDTEXTTOMTEXT
;;;
;;; Converts every selected single-line TEXT into its own MTEXT object.
;;;
;;; Pure AutoLISP (entget / entmakex / entdel / trans only):
;;;   - no Express Tools (TXT2MTXT) needed
;;;   - no ActiveX / vla- / vlax- calls
;;;   so it runs the same in AutoCAD and Civil 3D (any version with entmakex,
;;;   i.e. AutoCAD 2000i and later, and LT versions that support AutoLISP).
;;;
;;; Kept from each TEXT : layer, colour (index / true colour / colour book),
;;;   linetype, lineweight, transparency, visibility, text style, height,
;;;   rotation, justification (mapped to the MTEXT attachment point),
;;;   paper-space / model-space placement, OCS extrusion,
;;;   width factor and oblique angle (written as \W / \Q codes when they differ
;;;   from the text style), underline / overline (%%u / %%o).
;;; Special characters  \  {  }  are escaped so the text looks the same.
;;; Not convertible (MTEXT has no equivalent): backward and upside-down flags.
;;; TEXT on a locked layer is skipped and reported.
;;; The whole command is one UNDO step.
;;; ===========================================================================

;; Escape a TEXT string for use inside MTEXT.
(defun VTM:Escape (s / i n ch out ul ol)
  (setq i 1 n (strlen s) out "" ul nil ol nil)
  (while (<= i n)
    (setq ch (substr s i 1))
    (cond
      ;; backslash: keep \U+XXXX unicode codes, double every other one
      ((= ch "\\")
       (if (= (strcase (substr s (1+ i) 2)) "U+")
         (setq out (strcat out ch))
         (setq out (strcat out "\\\\"))
       )
      )
      ((= ch "{") (setq out (strcat out "\\{")))
      ((= ch "}") (setq out (strcat out "\\}")))
      ;; %%u / %%o toggles -> MTEXT underline / overline codes
      ((and (= ch "%")
            (= (substr s i 2) "%%")
            (member (strcase (substr s (+ i 2) 1)) '("U" "O"))
       )
       (if (= (strcase (substr s (+ i 2) 1)) "U")
         (progn
           (setq out (strcat out (if ul "\\l" "\\L")))
           (setq ul (not ul))
         )
         (progn
           (setq out (strcat out (if ol "\\o" "\\O")))
           (setq ol (not ol))
         )
       )
       (setq i (+ i 2))
      )
      (T (setq out (strcat out ch)))
    )
    (setq i (1+ i))
  )
  (if ul (setq out (strcat out "\\l")))
  (if ol (setq out (strcat out "\\o")))
  out
)

;; Split a string into pieces of at most 240 characters (DXF limit is 250).
;; The LAST item of the returned list is the final piece (group 1).
(defun VTM:Chunks (s / out)
  (setq out '())
  (while (> (strlen s) 240)
    (setq out (cons (substr s 1 240) out))
    (setq s   (substr s 241))
  )
  (reverse (cons s out))
)

;; TEXT justification (72 = horizontal, 73 = vertical) -> MTEXT attachment point
;; MTEXT: 1 TL 2 TC 3 TR / 4 ML 5 MC 6 MR / 7 BL 8 BC 9 BR
(defun VTM:Attach (h v / row)
  (cond
    ((member h '(3 5)) 7)            ; Aligned / Fit -> bottom-left
    ((= h 4) 5)                      ; Middle        -> middle-center
    (T
     (setq row (cond ((= v 3) 0)     ; top
                     ((= v 2) 1)     ; middle
                     (T 2)))         ; bottom / baseline
     (+ (* row 3) h 1)
    )
  )
)

(defun VTM:LayerLockedP (lay / rec)
  (and lay
       (setq rec (tblsearch "LAYER" lay))
       (= 4 (logand 4 (cdr (assoc 70 rec))))
  )
)

;; Convert one TEXT entity. Returns 'OK 'EMPTY or 'FAIL.
(defun VTM:Convert (en / ed txt lay sty tst ow sw ob sob pre ext hj vj pt a h
                         wpt dir ents code val chunks new)
  (setq ed  (entget en)
        txt (cdr (assoc 1 ed))
        h   (cdr (assoc 40 ed))
  )
  (cond
    ((or (null txt) (= txt "")) 'EMPTY)
    ((or (null h) (<= h 0.0))   'FAIL)
    (T
     (setq lay (cdr (assoc 8 ed)))
     (setq sty (cdr (assoc 7 ed)))
     (setq tst (if sty (tblsearch "STYLE" sty)))
     (setq sw  (if tst (cdr (assoc 41 tst)) 1.0))
     (if (or (null sw) (<= sw 0.0)) (setq sw 1.0))
     (setq sob (if tst (cdr (assoc 50 tst)) 0.0))
     (if (null sob) (setq sob 0.0))
     (setq ow  (cdr (assoc 41 ed)))
     (if (or (null ow) (<= ow 0.0)) (setq ow sw))
     (setq ob  (cdr (assoc 51 ed)))
     (if (null ob) (setq ob 0.0))

     ;; width factor / oblique that differ from the style
     (setq pre "")
     (if (> (abs (- ow sw)) 1e-6)
       (setq pre (strcat pre "\\W" (rtos (/ ow sw) 2 6) ";"))
     )
     (if (> (abs (- ob sob)) 1e-3)
       (setq pre (strcat pre "\\Q" (rtos ob 2 4) ";"))
     )

     ;; geometry (TEXT is in OCS, MTEXT in WCS)
     (setq ext (cdr (assoc 210 ed)))
     (if (null ext) (setq ext '(0.0 0.0 1.0)))
     (setq hj (cdr (assoc 72 ed)))
     (if (null hj) (setq hj 0))
     (setq vj (cdr (assoc 73 ed)))
     (if (null vj) (setq vj 0))
     (setq pt
       (if (and (or (/= hj 0) (/= vj 0))
                (not (member hj '(3 5)))
                (assoc 11 ed))
         (cdr (assoc 11 ed))          ; alignment point of justified text
         (cdr (assoc 10 ed))          ; plain left / baseline text
       )
     )
     (setq a (cdr (assoc 50 ed)))
     (if (null a) (setq a 0.0))
     (setq wpt (trans pt ext 0))
     (setq dir (trans (list (cos a) (sin a) 0.0) ext 0 T))

     ;; text, split in DXF-sized pieces
     (setq chunks (VTM:Chunks (strcat pre (VTM:Escape txt))))

     ;; entity data
     (setq ents (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 lay)))
     (foreach code '(67 410 6 62 420 430 370 440 60)
       (if (setq val (assoc code ed))
         (setq ents (append ents (list val)))
       )
     )
     (setq ents
       (append ents
         (list '(100 . "AcDbMText")
               (cons 10 wpt)
               (cons 40 h)
               (cons 41 0.0)                       ; no word-wrap width
               (cons 71 (VTM:Attach hj vj))
               '(72 . 1)                           ; left-to-right
         )
         (mapcar '(lambda (c) (cons 3 c)) (reverse (cdr (reverse chunks))))
         (list (cons 1 (last chunks)))
         (if sty (list (cons 7 sty)))
         (list (cons 210 ext)
               (cons 11 dir)
               '(73 . 1)
               '(44 . 1.0)
         )
       )
     )

     (setq new (entmakex ents))
     (if new
       (progn (entdel en) 'OK)
       'FAIL
     )
    )
  )
)

(defun c:VIDTEXTTOMTEXT (/ *error* ss i en ed res nOK nLock nEmpty nFail
                          undoOn oldEcho)
  (defun *error* (msg)
    (if undoOn  (command "_.UNDO" "_End"))
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if (and msg
             (not (member (strcase msg) '("FUNCTION CANCELLED" "QUIT / EXIT ABORT"))))
      (princ (strcat "\nVIDTEXTTOMTEXT error: " msg))
    )
    (princ)
  )

  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (if (setq ss (ssget '((0 . "TEXT"))))
    (progn
      (command "_.UNDO" "_Begin")
      (setq undoOn T)
      (setq nOK 0 nLock 0 nEmpty 0 nFail 0 i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i))
        (setq ed (entget en))
        (if (VTM:LayerLockedP (cdr (assoc 8 ed)))
          (setq nLock (1+ nLock))
          (progn
            (setq res (VTM:Convert en))
            (cond
              ((= res 'OK)    (setq nOK    (1+ nOK)))
              ((= res 'EMPTY) (setq nEmpty (1+ nEmpty)))
              (T              (setq nFail  (1+ nFail)))
            )
          )
        )
        (setq i (1+ i))
      )
      (command "_.UNDO" "_End")
      (setq undoOn nil)

      (princ (strcat "\n" (itoa nOK) " TEXT converted to MTEXT."))
      (if (> nLock  0) (princ (strcat "\n" (itoa nLock)  " skipped (locked layer).")))
      (if (> nEmpty 0) (princ (strcat "\n" (itoa nEmpty) " skipped (empty text).")))
      (if (> nFail  0) (princ (strcat "\n" (itoa nFail)  " could not be converted.")))
    )
    (princ "\nNo TEXT objects selected.")
  )

  (setvar "CMDECHO" oldEcho)
  (princ)
)

(princ "\nVIDTEXTTOMTEXT loaded.  Type VIDTEXTTOMTEXT to convert TEXT to MTEXT.")
(princ)
