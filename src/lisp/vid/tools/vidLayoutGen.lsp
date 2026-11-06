;;; ======================================================================
;;; vidPageLayout.lsp  (final, rev 21)
;;; Commands : VIDPAGELAYOUT - new layout + PDF page setup + viewport,
;;;                            all driven by the selected block
;;;            VIDLAYOUTGEN  - dialog + rough box -> inserts the block, then runs VIDPAGELAYOUT
;;;            VIDMEDIA      - list media sizes of "DWG To PDF.pc3"
;;; Targets  : AutoCAD / Civil 3D 2020+ (Windows). Not AutoCAD LT / Mac.
;;; ======================================================================
(vl-load-com)

(setq *blkpdf-device*      "DWG To PDF.pc3")
(setq *blkpdf-ctb*         "acad.ctb")
(setq *blkpdf-tol*         0.05)      ; mm tolerance when matching media sizes
(setq *blkpdf-maxdim*      100000.0)  ; sanity limit (mm)
(setq *blkpdf-require-ver* T)         ; file name must end in _vNN to give a sheet name
(setq *blkpdf-zoom-margin* 0.0)       ; zoom exactly to the block box (fit width AND height)
(setq *blkpdf-warn*        nil)
(setq *blkpdf-step*        "")
(setq *blkpdf-fatal*       nil)
(setq *blkpdf-saved*       nil)
(setq *blkpdf-mspace*      nil)
(setq *vidgen-auto-ename* nil)      ; set by VIDLAYOUTGEN: block for VIDPAGELAYOUT to use (no pick)
(setq *vidgen-auto-sheet* nil)      ; set by VIDLAYOUTGEN: sheet name for the layout name
(setq *vidgen-auto-na*    nil)      ; set by VIDLAYOUTGEN: north arrow block for this run only
;; ---- north arrow (block imported from vid_blocks_tools.dwg, as vidGradient.lsp does)
(setq *blkpdf-g-dwg-name* "vid_blocks_tools.dwg")
(setq *blkpdf-g-tried*    nil)
(setq *blkpdf-na-block*   "vid_north_arrow_001")
(setq *blkpdf-na-size-m*  50.0)   ; box size in METERS (converted with INSUNITS)
(setq *blkpdf-na-margin*  0.0)    ; gap from the viewport's top/right edge (paper mm); 0 = flush
(setq *blkpdf-na-fill*    0.9)    ; block fills this fraction of the box
(setq *blkpdf-bw*          nil)       ; block layout_width  (set per run)
(setq *blkpdf-bh*          nil)       ; block layout_height (set per run)

;; ---- tool config taken from the reference screenshots (Plot dialog + Viewport palette)
(setq *blkpdf-vp-scale*    1.0)           ; viewport Misc > Custom scale = 1.00 (nil = keep zoom-fit scale)
(setq *blkpdf-vp-anno*     "1\" = 1'")    ; viewport Annotation scale; nil = leave alone
(setq *blkpdf-vp-layer*    "0")           ; viewport layer

;;; ---------------------------------------------------------------- helpers
(defun blkpdf:step (s) (setq *blkpdf-step* s))

;; AutoLISP has no (error "msg") function. Store the message, then (exit):
;; (exit) runs the command's local *error* handler, which prints it and cleans up.
(defun blkpdf:fail (msg)
  (setq *blkpdf-fatal* msg)
  (exit))

(defun or* (a b) (if a a b))
(defun blkpdf:trim (s) (vl-string-trim " \t\r\n" s))

;; T when the function symbol really exists in this AutoCAD.
;; (Calling a missing vla- function raises an UNCATCHABLE "bad function" error.)
(defun blkpdf:exists (fn)
  (if (eval fn) T nil))

(defun blkpdf:vval (v / r)
  (if (= (type v) 'VARIANT)
    (progn
      (setq r (vl-catch-all-apply 'vlax-variant-value (list v)))
      (if (vl-catch-all-error-p r) nil r))
    v))

(defun blkpdf:var->list (v / r)
  (setq r (vl-catch-all-apply
            (function
              (lambda (x)
                (if (= (type x) 'VARIANT) (setq x (vlax-variant-value x)))
                (if (= (type x) 'SAFEARRAY) (vlax-safearray->list x))))
            (list v)))
  (if (vl-catch-all-error-p r) nil r))

;; Safe getter: nil if the function does not exist or raises an error.
(defun blkpdf:get (fn obj / r)
  (if (blkpdf:exists fn)
    (progn
      (setq r (vl-catch-all-apply fn (list obj)))
      (if (vl-catch-all-error-p r) nil r))
    nil))

(defun blkpdf:warn (msg) (setq *blkpdf-warn* (cons msg *blkpdf-warn*)))

;; Critical setter: aborts the command on failure (used only for device,
;; units, rotation and paper size, which are known to exist).
(defun blkpdf:put (fn obj val label / r)
  (blkpdf:step (strcat "setting " label))
  (if (not (blkpdf:exists fn))
    (blkpdf:fail (strcat "Cannot set " label ": not available in this AutoCAD.")))
  (setq r (vl-catch-all-apply fn (list obj val)))
  (if (vl-catch-all-error-p r)
    (blkpdf:fail (strcat "Cannot set " label ": " (vl-catch-all-error-message r)))
    T))

(defun blkpdf:same (a b)
  (cond
    ((and (numberp a) (numberp b)) (equal a b 0.0001))
    ((and (= (type a) 'STR) (= (type b) 'STR)) (= (strcase a) (strcase b)))
    (T (equal a b))))

;; Tolerant setter: skips if already correct, tries once, re-reads, never aborts.
;; Missing functions and refused values become warnings.
(defun blkpdf:ensure (putfn getfn obj val label / cur r)
  (blkpdf:step (strcat "setting " label))
  (cond
    ((not (and (blkpdf:exists putfn) (blkpdf:exists getfn)))
     (blkpdf:warn (strcat label " NOT applied: not available in this AutoCAD ActiveX interface."))
     nil)
    (T
     (setq cur (blkpdf:get getfn obj))
     (cond
       ((and cur (blkpdf:same cur val)) T)
       (T
        (setq r   (vl-catch-all-apply putfn (list obj val))
              cur (blkpdf:get getfn obj))
        (cond
          ((and cur (blkpdf:same cur val)) T)
          ((not (vl-catch-all-error-p r)) T)
          (T (blkpdf:warn (strcat label " NOT applied: " (vl-catch-all-error-message r)))
             nil)))))))

(defun blkpdf:find-ci (lst s / hit)
  (foreach x lst
    (if (and (null hit) (= (strcase x) (strcase s))) (setq hit x)))
  hit)

(defun blkpdf:s (v)
  (cond
    ((= (type v) 'STR)  v)
    ((= (type v) 'INT)  (itoa v))
    ((= (type v) 'REAL) (rtos v 2 2))
    (T "n/a")))

(defun blkpdf:onoff (v)
  (cond ((= v :vlax-true) "ON") ((= v :vlax-false) "OFF") (T "n/a")))

(defun blkpdf:pt2 (x y)
  (vlax-make-variant
    (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble '(0 . 1)) (list x y))))

(defun blkpdf:origin-ok (obj / o)
  (setq o (blkpdf:var->list (blkpdf:get 'vla-get-PlotOrigin obj)))
  (and o
       (numberp (car o)) (numberp (cadr o))
       (equal (car o) 0.0 0.0001) (equal (cadr o) 0.0 0.0001)))

(defun blkpdf:scale-ok (obj / sn sd)
  (setq sn nil sd nil)
  (if (blkpdf:exists 'vla-GetCustomScale)
    (vl-catch-all-apply 'vla-GetCustomScale (list obj 'sn 'sd)))
  (and (numberp sn) (numberp sd)
       (equal sn 1.0 0.0001) (equal sd 1.0 0.0001)
       (equal (blkpdf:get 'vla-get-UseStandardScale obj) :vlax-false)))

;; effective ratio plotted-mm : drawing-units is 1.0
(defun blkpdf:scale-ratio-ok (lay / sn sd)
  (setq sn nil sd nil)
  (if (blkpdf:exists 'vla-GetCustomScale)
    (vl-catch-all-apply 'vla-GetCustomScale (list lay 'sn 'sd)))
  (and (numberp sn) (numberp sd) (> sd 0.0)
       (equal (/ sn sd) 1.0 0.0001)))

;; paper units = mm AND ratio 1:1 AND (custom mode OR standard entry 1:1 = enum 16)
(defun blkpdf:scale-final-ok (lay / us ss)
  (setq us (blkpdf:get 'vla-get-UseStandardScale lay)
        ss (blkpdf:get 'vla-get-StandardScale lay))
  (and (equal (blkpdf:get 'vla-get-PaperUnits lay) 1)
       (blkpdf:scale-ratio-ok lay)
       (or (equal us :vlax-false) (equal ss 16))))

;; Final scale enforcement, in this order (same as doing it by hand in the Plot dialog):
;;  1) change the unit dropdown to MM FIRST
;;  2) reset stale standard entry (e.g. 1" = 1') to the standard 1:1, now that units are mm
;;  3) switch to Custom and type 1 (mm) = 1 (unit)
;;  4) re-check; repeat the sequence up to 3 times
;;  5) only if AutoCAD still refuses "custom": standard 1:1 (numerically identical)
(defun blkpdf:scale-seq (lay tag)
  (blkpdf:step (strcat tag " scale: 1) unit dropdown = MM"))
  (blkpdf:ensure 'vla-put-PaperUnits 'vla-get-PaperUnits lay 1
                 (strcat tag " paper units (mm)"))
  (blkpdf:step (strcat tag " scale: 2) reset stale standard scale"))
  (vl-catch-all-apply 'vla-put-UseStandardScale (list lay :vlax-true))
  (vl-catch-all-apply 'vla-put-StandardScale    (list lay 16))
  (blkpdf:step (strcat tag " scale: 3) custom, 1 mm = 1 unit"))
  (vl-catch-all-apply 'vla-put-UseStandardScale (list lay :vlax-false))
  (if (blkpdf:exists 'vla-SetCustomScale)
    (vl-catch-all-apply 'vla-SetCustomScale (list lay 1.0 1.0))))

(defun blkpdf:force-scale (lay tag / i)
  (setq i 0)
  (while (and (< i 3)
              (not (and (blkpdf:scale-final-ok lay)
                        (equal (blkpdf:get 'vla-get-UseStandardScale lay) :vlax-false))))
    (blkpdf:scale-seq lay tag)
    (setq i (1+ i)))
  (cond
    ((and (blkpdf:scale-final-ok lay)
          (equal (blkpdf:get 'vla-get-UseStandardScale lay) :vlax-false))
     T)
    (T
     (blkpdf:step (strcat tag " scale: standard 1:1 fallback"))
     (blkpdf:ensure 'vla-put-UseStandardScale 'vla-get-UseStandardScale lay :vlax-true
                    (strcat tag " standard scale mode"))
     (blkpdf:ensure 'vla-put-StandardScale 'vla-get-StandardScale lay 16
                    (strcat tag " standard scale 1:1"))
     (if (blkpdf:scale-final-ok lay)
       (blkpdf:warn (strcat tag ": AutoCAD refused 'Custom'; scale set to the standard 1:1 entry (identical to 1 mm = 1 unit)."))
       (blkpdf:warn (strcat tag ": plot scale could NOT be set to 1 mm = 1 unit."))))))

(defun blkpdf:vec-equal (a b)
  (and a b
       (equal (car a) (car b) 0.0001)
       (equal (cadr a) (cadr b) 0.0001)
       (equal (caddr a) (caddr b) 0.0001)))

(defun blkpdf:tag (v)
  (if (equal v (fix v) 0.000001)
    (itoa (fix v))
    (vl-string-subst "p" "." (rtos v 2 2))))

(defun blkpdf:cancel-msg-p (msg)
  (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*"))

;; ---- system variable save/restore + viewport state cleanup
(defun blkpdf:save-vars (names)
  (setq *blkpdf-saved* nil)
  (foreach n names
    (setq *blkpdf-saved* (cons (cons n (getvar n)) *blkpdf-saved*))))

(defun blkpdf:restore (doc)
  (if (and *blkpdf-mspace* doc)
    (vl-catch-all-apply 'vla-put-MSpace (list doc :vlax-false)))
  (setq *blkpdf-mspace* nil)
  (foreach p *blkpdf-saved*
    (vl-catch-all-apply 'setvar (list (car p) (cdr p))))
  (setq *blkpdf-saved* nil))

;; Shared body of the local *error* handlers: print fatal text or real errors.
(defun blkpdf:report-error (tag msg / txt)
  (setq txt (cond
              (*blkpdf-fatal* *blkpdf-fatal*)
              ((and msg (not (blkpdf:cancel-msg-p msg))) msg)
              (T nil)))
  (setq *blkpdf-fatal* nil)
  (if txt
    (prompt (strcat "\n" tag ": " txt
                    "\n  (failed during: " *blkpdf-step* ")")))
  txt)

;;; ------------------------------------------------- number parsing/validation
(defun blkpdf:num (v / s)
  (cond
    ((numberp v) (float v))
    ((= (type v) 'STR)
     (setq s (blkpdf:trim v))
     (if (and (> (strlen s) 2) (= (strcase (substr s (- (strlen s) 1))) "MM"))
       (setq s (blkpdf:trim (substr s 1 (- (strlen s) 2)))))
     (if (/= s "") (distof s 2)))
    (T nil)))

(defun blkpdf:dim (name v src / n)
  (setq n (blkpdf:num v))
  (cond
    ((null n)
     (blkpdf:fail (strcat "Property \"" name "\" (" src ") is not numeric: "
                          (vl-prin1-to-string v))))
    ((<= n 0.0)
     (blkpdf:fail (strcat "Property \"" name "\" must be greater than zero (got "
                          (rtos n 2 4) ").")))
    ((> n *blkpdf-maxdim*)
     (blkpdf:fail (strcat "Property \"" name "\" is unreasonably large ("
                          (rtos n 2 2) " mm).")))
    (T n)))

;;; ------------------------------------------------- block property reading
;;; Reads the selected block REFERENCE only (never the definition/geometry).
;;; Search order: dynamic block properties, then block attributes.
(defun blkpdf:dyn-prop (obj key / res found nm)
  (setq res (vl-catch-all-apply 'vla-get-IsDynamicBlock (list obj)))
  (if (and (not (vl-catch-all-error-p res)) (= res :vlax-true))
    (foreach p (blkpdf:var->list
                 (vl-catch-all-apply 'vla-GetDynamicBlockProperties (list obj)))
      (if (and (null found)
               (setq nm (blkpdf:get 'vla-get-PropertyName p))
               (= (strcase (blkpdf:trim nm)) key))
        (setq found (list (blkpdf:vval (blkpdf:get 'vla-get-Value p)))))))
  found)

(defun blkpdf:att-prop (obj key / res found nm)
  (setq res (vl-catch-all-apply 'vla-get-HasAttributes (list obj)))
  (if (and (not (vl-catch-all-error-p res)) (= res :vlax-true))
    (foreach a (blkpdf:var->list (vl-catch-all-apply 'vla-GetAttributes (list obj)))
      (if (and (null found)
               (setq nm (blkpdf:get 'vla-get-TagString a))
               (= (strcase (blkpdf:trim nm)) key))
        (setq found (list (blkpdf:get 'vla-get-TextString a))))))
  found)

(defun blkpdf:find-prop (obj key / hit)
  (cond
    ((setq hit (blkpdf:dyn-prop obj key)) (list (car hit) "dynamic block property"))
    ((setq hit (blkpdf:att-prop obj key)) (list (car hit) "block attribute"))
    (T nil)))

;; Returns the ENAME of the picked block reference.
(defun blkpdf:pick (/ sel ed)
  (setvar "ERRNO" 0)
  (setq sel (entsel "\nSelect block reference carrying layout_width / layout_height: "))
  (while (and (null sel) (= (getvar "ERRNO") 7))
    (setvar "ERRNO" 0)
    (prompt "\nNothing selected at that point. Try again.")
    (setq sel (entsel "\nSelect block reference: ")))
  (cond
    ((null sel) (blkpdf:fail "No block selected."))
    ((/= (cdr (assoc 0 (setq ed (entget (car sel))))) "INSERT")
     (blkpdf:fail (strcat "Selected object is a " (cdr (assoc 0 ed))
                          ", not a block reference.")))
    (T (car sel))))

;;; ------------------------------------------------------ layout naming
(defun blkpdf:split (s sep / out pos)
  (while (setq pos (vl-string-search sep s))
    (setq out (cons (substr s 1 pos) out)
          s   (substr s (+ pos 1 (strlen sep)))))
  (reverse (cons s out)))

(defun blkpdf:clean-name (s)
  (vl-list->string
    (vl-remove-if
      (function (lambda (c) (member c (vl-string->list "<>/\\\":;?*|,=`"))))
      (vl-string->list s))))

(defun blkpdf:ver-token-p (s)
  (wcmatch (strcase s) "V#,V##,V###,V####"))

;; "yd_TEN_thirunelvali_v01.dwg" -> "TEN" ; otherwise nil
(defun blkpdf:sheet-base (/ base parts nm)
  (setq base  (vl-filename-base (getvar "DWGNAME"))
        parts (blkpdf:split base "_"))
  (cond
    ((and *vidgen-auto-sheet*
          (/= (setq nm (blkpdf:clean-name (blkpdf:trim *vidgen-auto-sheet*))) ""))
     nm)
    ((and (>= (length parts) 3)
          (or (not *blkpdf-require-ver*) (blkpdf:ver-token-p (last parts)))
          (/= (setq nm (blkpdf:clean-name (blkpdf:trim (cadr parts)))) ""))
     nm)
    (T nil)))

(defun blkpdf:layout-exists (layouts name / found)
  (vlax-for lay layouts
    (if (= (strcase (vla-get-Name lay)) (strcase name)) (setq found T)))
  found)

(defun blkpdf:pad2 (n) (strcat (if (< n 10) "0" "") (itoa n)))

;; TEN -> TEN (free) or TEN_01, TEN_02 ... ; no pattern -> V Layout 01, 02 ...
(defun blkpdf:layout-name (layouts / base n cand)
  (setq base (blkpdf:sheet-base))
  (setq *vidgen-auto-sheet* nil)
  (cond
    ((and base (not (blkpdf:layout-exists layouts base))) base)
    (base
     (setq n 1)
     (while (blkpdf:layout-exists layouts (setq cand (strcat base "_" (blkpdf:pad2 n))))
       (setq n (1+ n)))
     cand)
    (T
     (setq n 1)
     (while (blkpdf:layout-exists layouts (setq cand (strcat "V Layout " (blkpdf:pad2 n))))
       (setq n (1+ n)))
     cand)))

;;; ------------------------------------------------------ media enumeration
(defun blkpdf:get-loc (pc cn / r)
  (setq r (vl-catch-all-apply 'vla-GetLocaleMediaName (list pc cn)))
  (if (and (not (vl-catch-all-error-p r)) (= (type r) 'STR)) r cn))

;; list of (canonical locale width height) in mm, native orientation.
;; PC needs ConfigName set, PaperUnits = mm, PlotRotation = 0.
(defun blkpdf:media (pc / out pw ph)
  (foreach cn (blkpdf:var->list
                (vl-catch-all-apply 'vla-GetCanonicalMediaNames (list pc)))
    (setq pw nil ph nil)
    (if (and (not (vl-catch-all-error-p
                    (vl-catch-all-apply 'vla-put-CanonicalMediaName (list pc cn))))
             (not (vl-catch-all-error-p
                    (vl-catch-all-apply 'vla-GetPaperSize (list pc 'pw 'ph))))
             (numberp pw) (numberp ph))
      (setq out (cons (list cn (blkpdf:get-loc pc cn) pw ph) out))))
  (reverse out))

;; zero-margin sizes first: "Final" = 0, "full bleed" = 1, everything else = 2
(defun blkpdf:media-score (loc / u)
  (setq u (strcase loc))
  (cond
    ((vl-string-search "FINAL" u) 0)
    ((vl-string-search "FULL BLEED" u) 1)
    (T 2)))

;; Exact match -> (media-entry rotation) or nil.
;; NOTE: AutoLISP (or a b) returns T/nil, not the value, so no (or ...) here.
(defun blkpdf:exact (media wd ht / best bscore sc mw mh rot)
  (foreach it media
    (setq mw (nth 2 it) mh (nth 3 it) rot nil)
    (cond
      ((and (equal mw wd *blkpdf-tol*) (equal mh ht *blkpdf-tol*)) (setq rot 0))
      ((and (equal mw ht *blkpdf-tol*) (equal mh wd *blkpdf-tol*)) (setq rot 1)))
    (if rot
      (progn
        (setq sc (+ (* 10 (blkpdf:media-score (cadr it))) rot))
        (if (or (null bscore) (< sc bscore))
          (setq bscore sc best (list it rot))))))
  best)

;; Smallest existing media that fully contains wd x ht -> (media-entry rotation)
(defun blkpdf:larger (media wd ht / best barea mw mh a rot)
  (foreach it media
    (setq mw (nth 2 it) mh (nth 3 it) rot nil)
    (cond
      ((and (>= mw (- wd *blkpdf-tol*)) (>= mh (- ht *blkpdf-tol*))) (setq rot 0))
      ((and (>= mh (- wd *blkpdf-tol*)) (>= mw (- ht *blkpdf-tol*))) (setq rot 1)))
    (if rot
      (progn
        (setq a (* mw mh))
        (if (or (null barea) (< a barea))
          (setq barea a best (list it rot))))))
  best)

;;; ------------------------------------------------------- viewport helpers
;; WCS points (origin x-axis-point y-axis-point) for the UCS whose X axis runs along
;; the block's own line (0,0) -> (layout_width,0), so that line becomes HORIZONTAL.
;; Uses insertion point + rotation (+ X-scale sign), honouring the block's OCS.
(defun blkpdf:block-ucs (ename / ed ip rot xs sg ux uy)
  (setq ed  (entget ename)
        ip  (cdr (assoc 10 ed))
        rot (cdr (assoc 50 ed))
        xs  (cdr (assoc 41 ed)))
  (if (null rot) (setq rot 0.0))
  (setq sg (if (and xs (< xs 0.0)) -1.0 1.0)
        ux (list (* sg (cos rot)) (* sg (sin rot)))
        uy (list (- (cadr ux)) (car ux)))
  (list
    (trans ip ename 0)
    (trans (list (+ (car ip) (car ux)) (+ (cadr ip) (cadr ux)) (caddr ip)) ename 0)
    (trans (list (+ (car ip) (car uy)) (+ (cadr ip) (cadr uy)) (caddr ip)) ename 0)))

;; The block's own reference box (0,0)-(W,H) in block coordinates, mapped through the
;; insert (scale, rotation, OCS) into the CURRENT UCS -> (xmin ymin xmax ymax) or nil.
(defun blkpdf:block-rect-ucs (ename w h / ed ip rot xs ys co si xl yl pts xs2 ys2 c)
  (if (and ename (numberp w) (numberp h))
    (progn
      (setq ed  (entget ename)
            ip  (cdr (assoc 10 ed))
            rot (cdr (assoc 50 ed))
            xs  (cdr (assoc 41 ed))
            ys  (cdr (assoc 42 ed)))
      (if (null rot) (setq rot 0.0))
      (if (null xs)  (setq xs 1.0))
      (if (null ys)  (setq ys 1.0))
      (setq co (cos rot) si (sin rot))
      (foreach lx (list 0.0 w)
        (foreach ly (list 0.0 h)
          (setq xl (* lx xs) yl (* ly ys)
                c  (trans (trans (list (+ (car ip) (- (* xl co) (* yl si)))
                                       (+ (cadr ip) (+ (* xl si) (* yl co)))
                                       (caddr ip))
                                 ename 0)
                          0 1)
                xs2 (cons (car c) xs2)
                ys2 (cons (cadr c) ys2))))
      (list (apply 'min xs2) (apply 'min ys2) (apply 'max xs2) (apply 'max ys2)))
    nil))

;; Block extents in the CURRENT UCS -> (xmin ymin xmax ymax) or nil.
(defun blkpdf:bbox-ucs (obj / mn mx r xs ys c x y z)
  (setq mn nil mx nil)
  (setq r (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx)))
  (if (vl-catch-all-error-p r)
    nil
    (progn
      (setq mn (blkpdf:var->list mn)
            mx (blkpdf:var->list mx))
      (if (and mn mx)
        (progn
          (foreach x (list (car mn) (car mx))
            (foreach y (list (cadr mn) (cadr mx))
              (foreach z (list (caddr mn) (caddr mx))
                (setq c  (trans (list x y z) 0 1)
                      xs (cons (car c) xs)
                      ys (cons (cadr c) ys)))))
          (list (apply 'min xs) (apply 'min ys) (apply 'max xs) (apply 'max ys)))
        nil))))

;; Aligns the ACTIVE viewport (model space inside it) to the block:
;; UCS = block orientation, plan view, zoom to block. Warns, never aborts.
(defun blkpdf:vp-align (obj ename cmdfn / ucs lcs ex len r bb m dx dy)
  (blkpdf:step "6f UCS = block orientation")
  (setq ucs (blkpdf:block-ucs ename)
        lcs (mapcar (function (lambda (p) (trans p 0 1))) ucs))
  ;; UCS > OBject > the selected block; 3-point fallback if that fails
  (setq r (vl-catch-all-apply cmdfn (list "_.UCS" "_OB" ename)))
  (if (vl-catch-all-error-p r)
    (setq r (vl-catch-all-apply cmdfn
              (list "_.UCS" (car lcs) (cadr lcs) (caddr lcs)))))
  (if (vl-catch-all-error-p r)
    (blkpdf:warn "Viewport UCS could not be set to the block orientation.")
    (progn
      (setq ex  (mapcar '- (cadr ucs) (car ucs))
            len (distance '(0.0 0.0 0.0) ex))
      (if (and (> len 0.000000001)
               (not (blkpdf:vec-equal
                      (mapcar (function (lambda (v) (/ v len))) ex)
                      (getvar "UCSXDIR"))))
        (blkpdf:warn "VERIFY: viewport UCS X axis does not match the block."))
      (blkpdf:step "6g UCSFOLLOW = 1 (view follows UCS)")
      (if (vl-catch-all-error-p (vl-catch-all-apply 'setvar (list "UCSFOLLOW" 1)))
        (blkpdf:warn "UCSFOLLOW could not be set to 1."))
      (blkpdf:step "6g plan view")
      (setq r (vl-catch-all-apply cmdfn (list "_.PLAN" "_C")))
      (if (vl-catch-all-error-p r)
        (blkpdf:warn "Viewport could not be rotated to the block's plan view."))
      (blkpdf:step "6h zoom to block")
      (setq bb (or* (blkpdf:block-rect-ucs ename *blkpdf-bw* *blkpdf-bh*) (blkpdf:bbox-ucs obj)))
      (if (null bb)
        (blkpdf:warn "Zoom skipped: block extents are not available.")
        (progn
          (setq dx (- (nth 2 bb) (car bb))
                dy (- (nth 3 bb) (cadr bb)))
          (if (and (<= dx 0.000001) (<= dy 0.000001))
            (blkpdf:warn "Zoom skipped: block has no measurable size.")
            (progn
              (setq m (max 0.000001 (* *blkpdf-zoom-margin* (max dx dy))))
              (setq r (vl-catch-all-apply cmdfn
                        (list "_.ZOOM" "_W"
                              (list (- (car bb) m) (- (cadr bb) m) 0.0)
                              (list (+ (nth 2 bb) m) (+ (nth 3 bb) m) 0.0))))
              (if (vl-catch-all-error-p r)
                (blkpdf:warn "Zoom to the block failed.")))))))))

;; After the zoom (which changes the scale): fix viewport Custom scale and
;; Annotation scale as in the reference Viewport palette.
(defun blkpdf:vp-finish (vp / cs)
  (if *blkpdf-vp-scale*
    (progn
      (blkpdf:step "6i viewport custom scale")
      (blkpdf:ensure 'vla-put-CustomScale 'vla-get-CustomScale vp *blkpdf-vp-scale*
                     (strcat "viewport custom scale " (rtos *blkpdf-vp-scale* 2 2)))
      (setq cs (blkpdf:get 'vla-get-CustomScale vp))
      (if (not (and (numberp cs) (equal cs *blkpdf-vp-scale* 0.001)))
        (blkpdf:warn (strcat "VERIFY: viewport custom scale is not " (rtos *blkpdf-vp-scale* 2 2) ".")))))
  (if *blkpdf-vp-anno*
    (progn
      (blkpdf:step "6j viewport annotation scale")
      (if (vl-catch-all-error-p
            (vl-catch-all-apply 'setvar (list "CANNOSCALE" *blkpdf-vp-anno*)))
        (blkpdf:warn (strcat "Annotation scale \"" *blkpdf-vp-anno* "\" not applied (scale not in the drawing's scale list?).")))))
  T)

;; Viewport created: make it display, activate it, align it, return to paper space.
(defun blkpdf:vp-setup (doc vp vw vh obj ename / okm cmdfn)
  (prompt (strcat "\n  Viewport created: " (rtos vw 2 2) " x " (rtos vh 2 2)
                  " mm, center " (rtos (/ vw 2.0) 2 2) ", " (rtos (/ vh 2.0) 2 2) ", 0"))
  (blkpdf:ensure 'vla-put-Display 'vla-get-Display vp :vlax-true "viewport display ON")
  (blkpdf:ensure 'vla-put-UCSPerViewport 'vla-get-UCSPerViewport vp :vlax-true "UCS per viewport")
  (blkpdf:ensure 'vla-put-Layer 'vla-get-Layer vp *blkpdf-vp-layer* "viewport layer")
  (blkpdf:ensure 'vla-put-color 'vla-get-color vp 256 "viewport color ByLayer")
  (blkpdf:ensure 'vla-put-Linetype 'vla-get-Linetype vp "ByLayer" "viewport linetype ByLayer")
  (blkpdf:ensure 'vla-put-LinetypeScale 'vla-get-LinetypeScale vp 1.0 "viewport linetype scale 1")
  (blkpdf:ensure 'vla-put-Lineweight 'vla-get-Lineweight vp -1 "viewport lineweight ByLayer")
  (blkpdf:ensure 'vla-put-ViewportOn 'vla-get-ViewportOn vp :vlax-true "viewport On")
  (blkpdf:ensure 'vla-put-DisplayLocked 'vla-get-DisplayLocked vp :vlax-false "viewport display locked: No")
  (blkpdf:ensure 'vla-put-ShadePlot 'vla-get-ShadePlot vp 0 "viewport shade plot: As displayed")
  (blkpdf:step "6e activate viewport")
  (blkpdf:save-vars '("CMDECHO" "OSMODE" "CANNOSCALE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setq cmdfn (if (blkpdf:exists 'command-s) 'command-s 'command))
  (setq okm nil)
  (if (and (blkpdf:exists 'vla-put-MSpace) (blkpdf:exists 'vla-put-ActivePViewport))
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vla-put-MSpace (list doc :vlax-true))))
      (progn
        (setq *blkpdf-mspace* T)
        (if (not (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-put-ActivePViewport (list doc vp))))
          (setq okm T)))))
  (if okm
    (progn
      (blkpdf:vp-align obj ename cmdfn)
      (blkpdf:vp-finish vp)
      (blkpdf:north-arrow doc vp vw vh cmdfn))
    (blkpdf:warn "Viewport could not be activated; UCS and zoom were skipped."))
  (blkpdf:restore doc))

;; Creates the viewport on the (already current) new layout.
;; Never aborts: every problem becomes a warning.
(defun blkpdf:viewport (doc lname wd ht obj ename / vw vh ps vp old e)
  (blkpdf:step "6e create viewport")
  (setq vw (- wd 10.0)
        vh (- ht 10.0))
  (cond
    ((or (<= vw 0.0) (<= vh 0.0))
     (blkpdf:warn "Viewport skipped: layout_width and layout_height must both exceed 10 mm."))
    ((/= (getvar "TILEMODE") 0)
     (blkpdf:warn "Viewport skipped: the new layout is not the current tab."))
    ((not (blkpdf:same (blkpdf:get 'vla-get-Name (blkpdf:get 'vla-get-ActiveLayout doc)) lname))
     (blkpdf:warn "Viewport skipped: the new layout is not the active layout."))
    ((equal (cdr (assoc 67 (entget ename))) 1)
     (blkpdf:warn "Viewport skipped: the selected block is in paper space; pick the block in model space."))
    ((not (blkpdf:exists 'vla-AddPViewport))
     (blkpdf:warn "Viewport skipped: AddPViewport is not available in this AutoCAD."))
    (T
     (setq ps (vla-get-PaperSpace doc))
     ;; remove any default viewport AutoCAD put on the brand-new layout
     (vlax-for e ps
       (if (= (blkpdf:s (blkpdf:get 'vla-get-ObjectName e)) "AcDbViewport")
         (setq old (cons e old))))
     (foreach e old (vl-catch-all-apply 'vla-Delete (list e)))
     ;; viewport: center ((W-10)/2,(H-10)/2,0)  width W-10  height H-10
     (setq vp (vl-catch-all-apply 'vla-AddPViewport
                (list ps (vlax-3d-point (/ vw 2.0) (/ vh 2.0) 0.0) vw vh)))
     (if (vl-catch-all-error-p vp)
       (blkpdf:warn (strcat "Viewport NOT created: " (vl-catch-all-error-message vp)))
       (blkpdf:vp-setup doc vp vw vh obj ename))))
  (blkpdf:step "viewport done"))

;;; ------------------------------------------------------------ north arrow
;;; DWG lookup + import (copied from vidGradient.lsp, renamed to avoid clashes)
(defun blkpdf:g-csv-split (str delimiter / pos result temp)
  (setq result '()
        temp   str)
  (while (setq pos (vl-string-search delimiter temp))
    (setq result (append result
                         (list (vl-string-trim " \r\n" (substr temp 1 pos)))))
    (setq temp (substr temp (+ pos (strlen delimiter) 1))))
  (append result (list (vl-string-trim " \r\n" temp)))
)

(defun blkpdf:g-vid-dirs (/ res ap d)
  (setq res "")
  (if (setq ap (getenv "APPDATA"))
    (setq res (strcat ap "/Autodesk/C3D 2026/enu/Support/vid/tools")))
  (if ap
    (foreach d (vl-directory-files (strcat ap "/Autodesk") "*" -1)
      (if (and (/= d ".") (/= d ".."))
        (setq res (strcat res ";" ap "/Autodesk/" d "/enu/Support/vid/tools")))))
  (foreach d (blkpdf:g-csv-split (getvar "ACADPREFIX") ";")
    (if (/= d "")
      (setq res (strcat res ";" d "/vid/tools"))))
  res
)

(defun blkpdf:g-search-file (fname / f dirs d res)
  (setq res nil)
  (setq *blkpdf-g-tried* (cons (strcat "findfile: " fname) *blkpdf-g-tried*))
  (cond
    ((setq f (findfile fname)) (setq res f))
    (T
     (setq dirs (blkpdf:g-csv-split (strcat (blkpdf:g-vid-dirs) ";" (getvar "TRUSTEDPATHS") ";" (getvar "DWGPREFIX")) ";"))
     (foreach d dirs
       (if (and (not res) (/= d ""))
         (progn
           (while (and (> (strlen d) 0) (member (substr d (strlen d)) (list "." "\\" "/")))
                  (setq d (substr d 1 (1- (strlen d)))))
           (setq *blkpdf-g-tried* (cons (strcat d "/" fname) *blkpdf-g-tried*))
           (if (and (/= d "") (setq f (findfile (strcat d "/" fname))))
             (setq res f)))))))
  res
)

(defun blkpdf:g-find-dwg (/ f)
  (cond
    ((and *blkpdf-g-dir* (setq f (findfile (strcat *blkpdf-g-dir* "/" *blkpdf-g-dwg-name*)))) f)
    ((and (not *blkpdf-g-dir*)
          (setq f (blkpdf:g-search-file "vidPageLayout.lsp"))
          (setq *blkpdf-g-dir* (vl-filename-directory f))
          (setq f (findfile (strcat *blkpdf-g-dir* "/" *blkpdf-g-dwg-name*))))
     f)
    ((setq f (blkpdf:g-search-file *blkpdf-g-dwg-name*)) f)
    (T nil))
)

(defun blkpdf:g-import-block (blk dwg / tmp old existed old-attreq old-cmdecho last-ent r)
  (setq dwg     (vl-string-translate "\\" "/" dwg)
        tmp     "VG_TMP_IMPORT"
        existed (tblsearch "BLOCK" blk)
        old     (strcat blk "_OLD_" (itoa (fix (* 1000000.0 (rem (getvar "DATE") 1.0)))))
        old-attreq  (getvar "ATTREQ")
        old-cmdecho (getvar "CMDECHO"))

  (setvar "ATTREQ" 0)
  (setvar "CMDECHO" 0)

  (setq r
    (vl-catch-all-apply
      '(lambda ()
         ;; leftovers from an earlier failed run
         (if (tblsearch "BLOCK" tmp)
           (vl-catch-all-apply 'command (list "_.-PURGE" "_Blocks" tmp "_No")))

         ;; move the existing definition out of the way so the new one is not ignored
         (if existed
           (command "_.-RENAME" "_Block" blk old))

         ;; import the DWG under the temp name, then erase the temp insert
         (setq last-ent (entlast))
         (command "_.-INSERT" (strcat tmp "=" dwg) "0,0,0" 1 1 0)
         (while (> (getvar "CMDACTIVE") 0) (command ""))
         (if (and (entlast) (not (equal (entlast) last-ent)))
           (entdel (entlast)))

         ;; geometry-only DWG: use the temp block itself as the entered block
         (if (and (not (tblsearch "BLOCK" blk)) (tblsearch "BLOCK" tmp))
           (progn
             (prompt (strcat "\n  ! No block named " blk " inside the DWG - using the DWG geometry as " blk))
             (command "_.-RENAME" "_Block" tmp blk)))

         ;; tidy up (silently ignored if still referenced)
         (if (tblsearch "BLOCK" tmp)
           (vl-catch-all-apply 'command (list "_.-PURGE" "_Blocks" tmp "_No")))
         (if (tblsearch "BLOCK" old)
           (vl-catch-all-apply 'command (list "_.-PURGE" "_Blocks" old "_No")))
         (while (> (getvar "CMDACTIVE") 0) (command "")))
    nil)   ;; <- argument list for vl-catch-all-apply
  )

  ;; ALWAYS restore system variables, even after an error
  (while (> (getvar "CMDACTIVE") 0) (command nil))
  (setvar "ATTREQ" old-attreq)
  (setvar "CMDECHO" old-cmdecho)

  (if (vl-catch-all-error-p r)
    (prompt (strcat "\nImport error: " (vl-catch-all-error-message r))))

  (if (tblsearch "BLOCK" blk) T nil)
)

;; remember this LSP's folder at load time (used to find vid_blocks_tools.dwg)
(if (setq *blkpdf-g-tmp* (blkpdf:g-search-file "vidPageLayout.lsp"))
  (setq *blkpdf-g-dir* (vl-filename-directory *blkpdf-g-tmp*)))
(setq *blkpdf-g-tmp* nil)

;; DWG lookup: LSP folder first, then support/trusted/drawing folders
(defun blkpdf:na-find-dwg (/ f)
  (cond
    ((and *blkpdf-g-dir*
          (setq f (findfile (strcat *blkpdf-g-dir* "/" *blkpdf-g-dwg-name*)))) f)
    (T (blkpdf:g-search-file *blkpdf-g-dwg-name*))))

;; meters -> drawing units (INSUNITS); nil when unitless/unknown
(defun blkpdf:m->units ( / u)
  (setq u (getvar "INSUNITS"))
  (cond
    ((= u 1)  39.37007874)    ; inches
    ((= u 2)  3.280839895)    ; feet
    ((= u 3)  (/ 1.0 1609.344)) ; miles
    ((= u 4)  1000.0)         ; mm
    ((= u 5)  100.0)          ; cm
    ((= u 6)  1.0)            ; m
    ((= u 7)  0.001)          ; km
    ((= u 10) 1.0936133)      ; yards
    (T nil)))

;; Import the north arrow block (overrides the existing definition). Never aborts.
(defun blkpdf:na-import ( / dwg)
  (blkpdf:step "2c import north arrow block")
  (cond
    ((tblsearch "BLOCK" *blkpdf-na-block*)
     (prompt (strcat "\n  Block " *blkpdf-na-block* " already in drawing - import skipped.")))
    ((null (setq dwg (blkpdf:na-find-dwg)))
     (blkpdf:warn (strcat "North arrow NOT imported: \"" *blkpdf-g-dwg-name* "\" was not found.")))
    ((not (blkpdf:g-import-block *blkpdf-na-block* dwg))
     (blkpdf:warn (strcat "North arrow NOT imported: block \"" *blkpdf-na-block* "\" could not be loaded from the DWG.")))
    (T (prompt (strcat "\n  Imported block " *blkpdf-na-block* " from " dwg)))))

;; Local extents of a block definition -> (xmin ymin xmax ymax) or nil.
(defun blkpdf:def-bbox (doc name / def mn mx r lo hi x0 y0 x1 y1)
  (setq def (vl-catch-all-apply 'vla-Item (list (vla-get-Blocks doc) name)))
  (if (vl-catch-all-error-p def)
    nil
    (progn
      (vlax-for e def
        (setq mn nil mx nil)
        (setq r (vl-catch-all-apply 'vla-GetBoundingBox (list e 'mn 'mx)))
        (if (not (vl-catch-all-error-p r))
          (progn
            (setq lo (blkpdf:var->list mn)
                  hi (blkpdf:var->list mx))
            (if (and lo hi)
              (setq x0 (if x0 (min x0 (car lo)) (car lo))
                    y0 (if y0 (min y0 (cadr lo)) (cadr lo))
                    x1 (if x1 (max x1 (car hi)) (car hi))
                    y1 (if y1 (max y1 (cadr hi)) (cadr hi)))))))
      (if (and x0 (> (- x1 x0) 0.0) (> (- y1 y0) 0.0))
        (list x0 y0 x1 y1)
        nil))))

;; PAPER SPACE (layout): draw a 50 m x 50 m (real-world) box flush with the viewport's
;; top-right corner and place the north arrow in it. The box is axis-aligned on the sheet;
;; its paper size = 50 m * INSUNITS factor * viewport scale. The arrow is rotated so that it
;; points to true north (WCS +Y) as displayed inside the viewport. Called while the viewport
;; is still ACTIVE (model space), so the view direction can be read from the current UCS.
(defun blkpdf:north-arrow (doc vp vw vh cmdfn / bb f sz szp cs ps p1x p1y p2x p2y m arr pl
                                              c cx cy lw lh s bcx bcy v ang rot co si ox oy ip r)
  (blkpdf:step "6k north arrow box + block (paper space)")
  (cond
    ((not (tblsearch "BLOCK" *blkpdf-na-block*))
     (blkpdf:warn (strcat "North arrow skipped: block \"" *blkpdf-na-block* "\" is not in the drawing.")))
    ((null (setq bb (blkpdf:def-bbox doc *blkpdf-na-block*)))
     (blkpdf:warn "North arrow skipped: block has no measurable geometry."))
    (T
     (setq f (blkpdf:m->units))
     (if (null f)
       (progn
         (setq f 1.0)
         (blkpdf:warn "INSUNITS is unitless/unknown: north arrow box assumed 50 x 50 drawing units.")))
     (setq sz  (* *blkpdf-na-size-m* f)
           cs  (blkpdf:get 'vla-get-CustomScale vp))
     (if (not (and (numberp cs) (> cs 0.0))) (setq cs 1.0))
     ;; box side on paper (mm) and its top-right corner = viewport top-right (minus margin)
     (setq szp (* sz cs)
           m   *blkpdf-na-margin*
           p2x (- vw m)
           p2y (- vh m)
           p1x (- p2x szp)
           p1y (- p2y szp)
           cx  (+ p1x (/ szp 2.0))
           cy  (+ p1y (/ szp 2.0))
           ps  (vla-get-PaperSpace doc))
     ;; the box: closed polyline in PAPER SPACE
     (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 7)))
     (vlax-safearray-fill arr (list p1x p1y p2x p1y p2x p2y p1x p2y))
     (setq pl (vl-catch-all-apply 'vla-AddLightWeightPolyline (list ps arr)))
     (if (vl-catch-all-error-p pl)
       (blkpdf:warn "North arrow box (rectangle) could not be drawn in the layout.")
       (vl-catch-all-apply 'vla-put-Closed (list pl :vlax-true)))
     ;; true-north direction as seen in the viewport: WCS +Y expressed in the viewport UCS
     ;; (view = plan of UCS), block artwork points up at rotation 0
     (setq v   (trans '(0.0 1.0 0.0) 0 1 T)
           ang (angle '(0.0 0.0 0.0) (list (car v) (cadr v) 0.0))
           rot (- ang (/ pi 2.0))
           co  (cos rot)
           si  (sin rot))
     ;; block scaled to fill the box, bbox centre placed on the box centre
     (setq lw  (- (nth 2 bb) (car bb))
           lh  (- (nth 3 bb) (cadr bb))
           s   (* *blkpdf-na-fill* (min (/ szp lw) (/ szp lh)))
           bcx (/ (+ (car bb) (nth 2 bb)) 2.0)
           bcy (/ (+ (cadr bb) (nth 3 bb)) 2.0)
           ox  (* s bcx)
           oy  (* s bcy)
           ip  (list (- cx (- (* ox co) (* oy si)))
                     (- cy (+ (* ox si) (* oy co)))
                     0.0))
     (setq r (vl-catch-all-apply 'vla-InsertBlock
               (list ps (vlax-3d-point ip) *blkpdf-na-block* s s s rot)))
     (if (vl-catch-all-error-p r)
       (blkpdf:warn (strcat "North arrow block NOT inserted: " (vl-catch-all-error-message r)))
       (prompt (strcat "\n  North arrow placed in the layout (paper space) in a "
                       (rtos *blkpdf-na-size-m* 2 0) " x " (rtos *blkpdf-na-size-m* 2 0)
                       " m box, flush with the viewport top-right (block scale "
                       (rtos s 2 4) ")."))))))

;;; ------------------------------------------------- verify + final report
;; Reads the NEW LAYOUT back and warns about anything that differs from spec.
(defun blkpdf:verify (lay entry rot ctb)
  (if (not (blkpdf:same (blkpdf:get 'vla-get-CanonicalMediaName lay) (car entry)))
    (blkpdf:warn "VERIFY: layout reports a different paper size than the one selected."))
  (if (not (blkpdf:same (blkpdf:get 'vla-get-PlotRotation lay) rot))
    (blkpdf:warn "VERIFY: layout orientation/rotation differs from the one selected."))
  (if (not (blkpdf:same (blkpdf:get 'vla-get-StyleSheet lay) ctb))
    (blkpdf:warn (strcat "VERIFY: plot style table is not " ctb ".")))
  (if (not (blkpdf:origin-ok lay))
    (blkpdf:warn "VERIFY: plot offset is not confirmed as 0,0."))
  (if (equal (blkpdf:get 'vla-get-CenterPlot lay) :vlax-true)
    (blkpdf:warn "VERIFY: 'Center the plot' is ON."))
  (if (not (blkpdf:scale-final-ok lay))
    (blkpdf:warn "VERIFY: plot scale is not confirmed as 1 mm = 1 unit (mm paper units).")))

(defun blkpdf:report (lay lname / pw ph n d org cn rt pt)
  (setq cn (blkpdf:get 'vla-get-CanonicalMediaName lay))
  (vl-catch-all-apply 'vla-GetPaperSize (list lay 'pw 'ph))
  (if (blkpdf:exists 'vla-GetCustomScale)
    (vl-catch-all-apply 'vla-GetCustomScale (list lay 'n 'd)))
  (setq org (blkpdf:var->list (blkpdf:get 'vla-get-PlotOrigin lay))
        rt  (blkpdf:get 'vla-get-PlotRotation lay)
        pt  (blkpdf:get 'vla-get-PlotType lay))
  (prompt (strcat "\n--- Page setup on new layout \"" lname "\" (read back) ---"))
  (prompt (strcat "\n  Device ............ " (blkpdf:s (blkpdf:get 'vla-get-ConfigName lay))))
  (prompt (strcat "\n  Paper size ........ " (blkpdf:s (if cn (blkpdf:get-loc lay cn)))
                  "  [native " (blkpdf:s pw) " x " (blkpdf:s ph) " mm]"))
  (prompt (strcat "\n  Rotation .......... " (if (numberp rt) (itoa (* 90 rt)) "n/a")
                  " deg (sheet width along the horizontal edge)"))
  (prompt (strcat "\n  Plot area ......... "
                  (cond ((equal pt 2) "Limits") ((equal pt 5) "Layout") (T (blkpdf:s pt)))))
  (prompt (strcat "\n  Offset / centered . "
                  (if org (strcat (blkpdf:s (car org)) ", " (blkpdf:s (cadr org))) "n/a")
                  " / " (blkpdf:onoff (blkpdf:get 'vla-get-CenterPlot lay))))
  (prompt (strcat "\n  Scale ............. " (blkpdf:s n) " = " (blkpdf:s d)
                  " unit(s); paper units: "
                  (if (equal (blkpdf:get 'vla-get-PaperUnits lay) 1) "mm"
                      (strcat "code " (blkpdf:s (blkpdf:get 'vla-get-PaperUnits lay))))
                  "; mode: "
                  (cond
                    ((equal (blkpdf:get 'vla-get-UseStandardScale lay) :vlax-false) "Custom")
                    ((equal (blkpdf:get 'vla-get-UseStandardScale lay) :vlax-true)
                     (strcat "Standard #" (blkpdf:s (blkpdf:get 'vla-get-StandardScale lay))))
                    (T "n/a"))))
  (prompt (strcat "\n  Plot style table .. " (blkpdf:s (blkpdf:get 'vla-get-StyleSheet lay))))
  (prompt (strcat "\n  Lineweights/Styles  " (blkpdf:onoff (blkpdf:get 'vla-get-PlotWithLineweights lay))
                  " / " (blkpdf:onoff (blkpdf:get 'vla-get-PlotWithPlotStyles lay))))
  (prompt (strcat "\n  Paperspace last / hide paperspace: "
                  (blkpdf:onoff (blkpdf:get 'vla-get-PlotViewportsFirst lay)) " / "
                  (blkpdf:onoff (blkpdf:get 'vla-get-PlotHidden lay))))
  (prompt (strcat "\n  Shade plot (0 = As displayed): " (blkpdf:s (blkpdf:get 'vla-get-ShadePlot lay))))
  (prompt (strcat "\n  BACKGROUNDPLOT .... " (blkpdf:s (getvar "BACKGROUNDPLOT"))))
  (prompt "\n  Note: shade plot QUALITY is not settable via ActiveX (default Normal)."))

;;; ============================================================ VIDPAGELAYOUT
(defun c:VIDPAGELAYOUT ( / *error* doc qlay layouts lname newlay ename obj r wr hr wd ht
                          pcs pcname pc newpc devs dev ctbs ctb media hit larger
                          ans entry rot undo done pt bp wmsg na-old)

  (defun *error* (msg)
    (setq *vidgen-auto-ename* nil *vidgen-auto-sheet* nil *vidgen-auto-na* nil)
    (if na-old (setq *blkpdf-na-block* na-old na-old nil))
    (blkpdf:restore doc)
    (if (and newlay (not done)) (vl-catch-all-apply 'vla-Delete (list newlay)))
    (if (and newpc (not done))  (vl-catch-all-apply 'vla-Delete (list newpc)))
    (if (and undo doc) (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (blkpdf:report-error "VIDPAGELAYOUT" msg)
      (prompt (if done
                "\n  The layout WAS created before this error; check the layout tabs."
                "\n  No new layout was created.")))
    (princ))

  (setq *blkpdf-warn* nil *blkpdf-fatal* nil *blkpdf-saved* nil *blkpdf-mspace* nil)
  (blkpdf:step "start")
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= (getvar "BLOCKEDITOR") 1)
    (blkpdf:fail "Not available while the Block Editor is open."))

  ;; ---- 1. select block + read/validate (nothing modified yet)
  (blkpdf:step "1 read block properties")
  (setq ename (if *vidgen-auto-ename* *vidgen-auto-ename* (blkpdf:pick)))
  (setq *vidgen-auto-ename* nil)
  (if *vidgen-auto-na*
    (setq na-old *blkpdf-na-block*
          *blkpdf-na-block* *vidgen-auto-na*
          *vidgen-auto-na* nil))
  (setq obj   (vlax-ename->vla-object ename))
  (setq wr (blkpdf:find-prop obj "LAYOUT_WIDTH"))
  (if (null wr)
    (blkpdf:fail "Block has no \"layout_width\" property (checked dynamic block properties and attributes)."))
  (setq hr (blkpdf:find-prop obj "LAYOUT_HEIGHT"))
  (if (null hr)
    (blkpdf:fail "Block has no \"layout_height\" property (checked dynamic block properties and attributes)."))
  (setq wd (blkpdf:dim "layout_width"  (car wr) (cadr wr))
        ht (blkpdf:dim "layout_height" (car hr) (cadr hr)))
  (setq *blkpdf-bw* wd *blkpdf-bh* ht)
  (prompt (strcat "\n  layout_width  = " (rtos wd 2 2) " mm  [" (cadr wr) "]"))
  (prompt (strcat "\n  layout_height = " (rtos ht 2 2) " mm  [" (cadr hr) "]"))

  ;; ---- 2. pre-flight: device and plot style table (read-only queries)
  (blkpdf:step "2a active layout / plot device list")
  (setq qlay (vla-get-ActiveLayout doc))
  (setq devs (blkpdf:var->list (vl-catch-all-apply 'vla-GetPlotDeviceNames (list qlay))))
  (setq dev  (blkpdf:find-ci devs *blkpdf-device*))
  (if (null dev)
    (blkpdf:fail (strcat "Plot device \"" *blkpdf-device* "\" is not available in this installation.")))
  (blkpdf:step "2b plot style table list")
  (if (/= (getvar "PSTYLEMODE") 1)
    (blkpdf:fail (strcat "This drawing uses named plot styles (PSTYLEMODE=0); \"" *blkpdf-ctb*
                         "\" cannot be attached. Convert with CONVERTPSTYLES first.")))
  (setq ctbs (blkpdf:var->list (vl-catch-all-apply 'vla-GetPlotStyleTableNames (list qlay))))
  (setq ctb  (blkpdf:find-ci ctbs *blkpdf-ctb*))
  (if (null ctb)
    (blkpdf:fail (strcat "Plot style table \"" *blkpdf-ctb* "\" was not found.")))

  ;; ---- 2c. import the north arrow block (warns only; never aborts)
  (blkpdf:na-import)

  ;; ---- 3. named page setup (only BLKPDF_* setups are ever touched)
  (blkpdf:step "3 named page setup")
  (setq undo (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-StartUndoMark (list doc)))))
  (setq pcs    (vla-get-PlotConfigurations doc)
        pcname (strcat "BLKPDF_" (blkpdf:tag wd) "x" (blkpdf:tag ht)))
  (setq r (vl-catch-all-apply 'vla-Item (list pcs pcname)))
  (if (vl-catch-all-error-p r)
    (setq pc (vla-Add pcs pcname :vlax-false)
          newpc pc)
    (setq pc r))

  ;; ---- 4. device, units (mm), media: reuse existing size in the PC3 (critical)
  (blkpdf:put 'vla-put-ConfigName pc dev "plot device")
  (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list pc))
  (blkpdf:put 'vla-put-PaperUnits pc 1 "paper units (mm)")
  (blkpdf:put 'vla-put-PlotRotation pc 0 "plot rotation (scan)")
  (blkpdf:step "4b media scan")
  (setq media (blkpdf:media pc))
  (if (null media)
    (blkpdf:fail (strcat "Device \"" dev "\" reported no usable paper sizes.")))
  (blkpdf:step "4c media match")
  (setq hit (blkpdf:exact media wd ht))

  (if hit
    (prompt (strcat "\n  Using existing paper size: " (cadr (car hit))))
    (progn
      (prompt (strcat "\n*** \"" dev "\" has NO paper size of " (rtos wd 2 2) " x "
                      (rtos ht 2 2) " mm."))
      (prompt "\n    AutoLISP cannot add media to a PC3. Add the size once in the Plotter")
      (prompt "\n    Configuration Editor; this command will then find it automatically.")
      (setq larger (blkpdf:larger media wd ht))
      (if (null larger)
        (blkpdf:fail "No existing paper size is large enough either."))
      (initget "Yes No")
      (setq ans (getkword (strcat "\nUse nearest LARGER existing size \""
                                  (cadr (car larger)) "\" ("
                                  (rtos (nth 2 (car larger)) 2 2) " x "
                                  (rtos (nth 3 (car larger)) 2 2)
                                  " mm) instead? [Yes/No] <No>: ")))
      (if (= ans "Yes")
        (setq hit larger)
        (blkpdf:fail "Cancelled: exact paper size not available."))))

  (setq entry (car hit) rot (cadr hit))
  (blkpdf:put 'vla-put-CanonicalMediaName pc (car entry) "paper size")
  (blkpdf:put 'vla-put-PlotRotation pc rot "drawing orientation (landscape)")

  ;; ---- 5. remaining settings: tolerant (warn, never abort), verified later
  ;; plot area FIRST (centering/offset can depend on it)
  (blkpdf:step "setting plot area")
  (vl-catch-all-apply 'vla-put-PlotType (list pc 2))
  (setq pt (blkpdf:get 'vla-get-PlotType pc))
  (if (not (and (numberp pt) (= pt 2)))
    (progn
      (blkpdf:ensure 'vla-put-PlotType 'vla-get-PlotType pc 5 "plot area (Layout)")
      (blkpdf:warn "Paper-space layouts do not offer \"Limits\"; plot area set to \"Layout\" instead.")))

  (blkpdf:ensure 'vla-put-CenterPlot 'vla-get-CenterPlot pc :vlax-false "center plot OFF")
  (blkpdf:step "setting plot offset 0,0")
  (if (not (blkpdf:origin-ok pc))
    (progn
      (vl-catch-all-apply 'vla-put-PlotOrigin (list pc (blkpdf:pt2 0.0 0.0)))
      (if (not (blkpdf:origin-ok pc))
        (blkpdf:warn "Plot offset 0,0 could not be applied to the page setup."))))


  (blkpdf:ensure 'vla-put-StyleSheet 'vla-get-StyleSheet pc ctb "plot style table")
  (blkpdf:ensure 'vla-put-PlotWithLineweights 'vla-get-PlotWithLineweights pc :vlax-true  "plot object lineweights")
  (blkpdf:ensure 'vla-put-PlotWithPlotStyles  'vla-get-PlotWithPlotStyles  pc :vlax-true  "plot with plot styles")
  (blkpdf:ensure 'vla-put-PlotViewportsFirst  'vla-get-PlotViewportsFirst  pc :vlax-true  "plot paperspace last")
  (blkpdf:ensure 'vla-put-PlotHidden          'vla-get-PlotHidden          pc :vlax-false "hide paperspace objects")
  (blkpdf:ensure 'vla-put-ScaleLineweights    'vla-get-ScaleLineweights    pc :vlax-false "scale lineweights OFF")
  (blkpdf:ensure 'vla-put-ShadePlot           'vla-get-ShadePlot           pc 0 "shade plot: As displayed")
  (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list pc))
  (blkpdf:force-scale pc "page setup")

  ;; ---- 6. create the NEW layout and apply the page setup
  (blkpdf:step "6a layout name")
  (setq layouts (vla-get-Layouts doc))
  (setq lname   (blkpdf:layout-name layouts))
  (blkpdf:step "6b create layout")
  (setq newlay  (vla-Add layouts lname))
  (blkpdf:step "6c apply page setup (CopyFrom)")
  (setq r (vl-catch-all-apply 'vla-CopyFrom (list newlay pc)))
  (if (vl-catch-all-error-p r)
    (blkpdf:fail (strcat "Applying the page setup to layout \"" lname "\" failed: "
                         (vl-catch-all-error-message r))))
  (setq done T)
  (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list newlay))
  (blkpdf:step "6d make layout current")
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-ActiveLayout (list doc newlay)))
    (blkpdf:warn (strcat "Layout \"" lname "\" created but could not be made current.")))

  ;; ---- 6e. viewport: (W-10) x (H-10), UCS = block orientation, zoom to block
  (blkpdf:viewport doc lname wd ht obj ename)

  ;; ---- 6f. overall layout zoom > Extents (paper space)
  (blkpdf:step "6f layout zoom extents")
  (if (vl-catch-all-error-p
        (vl-catch-all-apply (if (blkpdf:exists 'command-s) 'command-s 'command)
                            (list "_.ZOOM" "_E")))
    (blkpdf:warn "Layout zoom Extents could not be run."))

  ;; ---- 7. background plotting OFF (application setting)
  (blkpdf:step "7 background plot")
  (setq bp (getvar "BACKGROUNDPLOT"))
  (if (and (numberp bp) (/= (logand bp 1) 0))
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'setvar (list "BACKGROUNDPLOT" (logand bp 2))))
      (blkpdf:warn "BACKGROUNDPLOT could not be cleared; background plotting may still be ON.")))

  ;; ---- 7b. FINAL plot scale on the layout (after everything else could reset it)
  (blkpdf:force-scale newlay "layout")

  ;; ---- 8. verify + report
  (blkpdf:step "8 verify / report")
  (blkpdf:verify newlay entry rot ctb)
  (blkpdf:report newlay lname)
  (foreach wmsg (reverse *blkpdf-warn*) (prompt (strcat "\n  ! " wmsg)))
  (prompt (strcat "\nVIDPAGELAYOUT: layout \"" lname "\" created. Page setup \"" pcname "\" saved."))

  (if undo (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
  (setq undo nil)
  (if na-old (setq *blkpdf-na-block* na-old na-old nil))

  ;; ---- 9. everything done: REGENALL
  (blkpdf:step "9 REGENALL")
  (if (vl-catch-all-error-p
        (vl-catch-all-apply (if (blkpdf:exists 'command-s) 'command-s 'command)
                            (list "_.REGENALL")))
    (prompt "\n  ! REGENALL could not be run."))
  (princ))

;;; ================================================================== UCSF
;; UCSF -> UCSFOLLOW = 1 (view follows UCS) in the current viewport / tab
(defun c:UCSF ()
  (setvar "UCSFOLLOW" 1)
  (prompt "\nUCSFOLLOW = 1")
  (princ))

;;; ============================================================= VIDMEDIA
(defun c:VIDMEDIA ( / *error* doc qlay pcs pc r dev devs media)
  (defun *error* (msg)
    (if pc (vl-catch-all-apply 'vla-Delete (list pc)))
    (blkpdf:report-error "VIDMEDIA" msg)
    (princ))
  (setq *blkpdf-fatal* nil)
  (blkpdf:step "VIDMEDIA start")
  (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
        qlay (vla-get-ActiveLayout doc))
  (setq devs (blkpdf:var->list (vl-catch-all-apply 'vla-GetPlotDeviceNames (list qlay))))
  (setq dev  (blkpdf:find-ci devs *blkpdf-device*))
  (if (null dev) (blkpdf:fail (strcat "Plot device \"" *blkpdf-device* "\" is not available.")))
  (setq pcs (vla-get-PlotConfigurations doc))
  (setq r (vl-catch-all-apply 'vla-Item (list pcs "BLKPDF_TMP_MEDIA")))
  (if (not (vl-catch-all-error-p r)) (vl-catch-all-apply 'vla-Delete (list r)))
  (setq pc (vla-Add pcs "BLKPDF_TMP_MEDIA" :vlax-false))
  (blkpdf:put 'vla-put-ConfigName pc dev "plot device")
  (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list pc))
  (blkpdf:put 'vla-put-PaperUnits pc 1 "paper units")
  (blkpdf:put 'vla-put-PlotRotation pc 0 "rotation")
  (setq media (blkpdf:media pc))
  (prompt (strcat "\nMedia in \"" dev "\" (" (itoa (length media)) " entries, mm):"))
  (foreach it media
    (prompt (strcat "\n  " (rtos (nth 2 it) 2 2) " x " (rtos (nth 3 it) 2 2) "   " (cadr it))))
  (vl-catch-all-apply 'vla-Delete (list pc))
  (setq pc nil)
  (princ))

;;; ============================================================ VIDLAYOUTGEN
;;; Dialog (Layout Block + Sheet Name) -> import block -> rough dashed box ->
;;; nearest paper size of the PDF device -> insert block with layout_width/height.
(setq *vidgen-blk-default* "vid_layout_generate_001")
(setq *vidgen-mode-default* "Draw")      ; "Draw" or "Select"
(setq *vidgen-mode*  nil)
(setq *vidgen-na-default*  "vid_north_arrow_001")
(setq *vidgen-blk*   nil)
(setq *vidgen-na*    nil)
(setq *vidgen-sheet* nil)

;; ---- dialog: writes a temporary DCL file, returns T on OK (values in *vidgen-blk* / *vidgen-sheet*)
(defun vidgen:ui (mode blk na sheet / fn f dcl res)
  (setq *vidgen-blk* nil *vidgen-na* nil *vidgen-sheet* nil *vidgen-mode* nil)
  (setq fn (vl-filename-mktemp "vidgen.dcl"))
  (setq f  (open fn "w"))
  (if (null f)
    (blkpdf:fail "Cannot create the temporary dialog file."))
  (foreach l (list
               "vidgen_ui : dialog {"
               "  label = \"VID Layout Generate\";"
               "  : boxed_radio_row {"
               "    label = \"Layout Methods\";"
               "    : radio_button { key = \"sel\"; label = \"Select Layout\"; }"
               "    : radio_button { key = \"drw\"; label = \"Draw Layout\"; }"
               "  }"
               "  : boxed_column {"
               "    label = \"Symbols\";"
               "    : edit_box { key = \"blk\"; label = \"Layout Block\"; edit_width = 30; }"
               "    : edit_box { key = \"na\"; label = \"North Arrow Symbol\"; edit_width = 30; }"
               "  }"
               "  : boxed_column {"
               "    label = \"Sheet\";"
               "    : edit_box { key = \"sheet\"; label = \"Sheet Name\"; edit_width = 30; }"
               "  }"
               "  : errtile { width = 44; }"
               "  ok_cancel;"
               "}")
    (write-line l f))
  (close f)
  (setq dcl (load_dialog fn))
  (cond
    ((or (null dcl) (< dcl 0))
     (vl-file-delete fn)
     (blkpdf:fail "Cannot load the dialog."))
    ((not (new_dialog "vidgen_ui" dcl))
     (unload_dialog dcl)
     (vl-file-delete fn)
     (blkpdf:fail "Cannot open the dialog."))
    (T
     (set_tile "blk" blk)
     (set_tile "sel" (if (= mode "Select") "1" "0"))
     (set_tile "drw" (if (= mode "Select") "0" "1"))
     (mode_tile "blk" (if (= mode "Select") 1 0))
     (action_tile "sel" "(mode_tile \"blk\" 1)")
     (action_tile "drw" "(mode_tile \"blk\" 0)")
     (set_tile "na" na)
     (set_tile "sheet" sheet)
     (mode_tile "blk" 2)
     (action_tile "accept"
       (strcat "(progn"
               " (setq *vidgen-blk* (vl-string-trim \" \" (get_tile \"blk\")))"
               " (setq *vidgen-na* (vl-string-trim \" \" (get_tile \"na\")))"
               " (setq *vidgen-sheet* (vl-string-trim \" \" (get_tile \"sheet\")))"
               " (setq *vidgen-mode* (if (= (get_tile \"sel\") \"1\") \"Select\" \"Draw\"))"
               " (cond"
               "   ((and (= *vidgen-mode* \"Draw\") (= *vidgen-blk* \"\")) (set_tile \"error\" \"Layout Block name is required.\"))"
               "   ((= *vidgen-na* \"\") (set_tile \"error\" \"North Arrow Symbol name is required.\"))"
               "   (T (done_dialog 1))))"))
     (action_tile "cancel" "(done_dialog 0)")
     (setq res (start_dialog))
     (unload_dialog dcl)
     (vl-file-delete fn)
     (= res 1))))

;; ---- dashed rectangle (like a crossing-selection box) as a GRVECS list
(defun vidgen:dash-edge (a b dl / len ux uy s e out)
  (setq len (distance a b))
  (if (> len 0.000000001)
    (progn
      (setq ux (/ (- (car b) (car a)) len)
            uy (/ (- (cadr b) (cadr a)) len)
            s  0.0)
      (while (< s len)
        (setq e (min (+ s dl) len)
              out (append out
                          (list (list (+ (car a) (* ux s)) (+ (cadr a) (* uy s)) (caddr a))
                                (list (+ (car a) (* ux e)) (+ (cadr a) (* uy e)) (caddr a)))))
        (setq s (+ s (* 2.0 dl))))))
  out)

(defun vidgen:dash-rect (p1 p2 / z c1 c2 c3 c4 dl)
  (setq z  (caddr p1)
        c1 (list (car p1) (cadr p1) z)
        c2 (list (car p2) (cadr p1) z)
        c3 (list (car p2) (cadr p2) z)
        c4 (list (car p1) (cadr p2) z)
        dl (/ (getvar "VIEWSIZE") 60.0))
  (cons 3 (append (vidgen:dash-edge c1 c2 dl) (vidgen:dash-edge c2 c3 dl)
                  (vidgen:dash-edge c3 c4 dl) (vidgen:dash-edge c4 c1 dl))))

;; Two corners picked with a live dashed rubber-band box. Returns (p1 p2) in the current UCS
;; or nil on Esc / right-click.
(defun vidgen:pick-box ( / p1 g code val last p2 done)
  (setq p1 (getpoint "\nSpecify first corner of the rough layout box: "))
  (if p1
    (progn
      (prompt "\nSpecify opposite corner (click; Esc or right-click to cancel): ")
      (while (not done)
        (setq g (grread T 15 0) code (car g) val (cadr g))
        (cond
          ((= code 5)
           (setq last val)
           (redraw)
           (grvecs (vidgen:dash-rect p1 val)))
          ((= code 3) (setq p2 val done T))
          ((= code 2)
           (cond
             ((and (member val '(13 32)) last) (setq p2 last done T))
             ((= val 27) (setq done T))))
          ((member code '(11 12)) (setq done T))))
      (redraw)
      (if p2 (list p1 p2)))))

;; ---- media of the PDF device (same method as VIDMEDIA) -> list or nil
(defun vidgen:media (doc / qlay devs dev pcs r pc media)
  (setq qlay (vla-get-ActiveLayout doc)
        devs (blkpdf:var->list (vl-catch-all-apply 'vla-GetPlotDeviceNames (list qlay)))
        dev  (blkpdf:find-ci devs *blkpdf-device*))
  (if dev
    (progn
      (setq pcs (vla-get-PlotConfigurations doc))
      (setq r (vl-catch-all-apply 'vla-Item (list pcs "BLKPDF_TMP_MEDIA")))
      (if (not (vl-catch-all-error-p r)) (vl-catch-all-apply 'vla-Delete (list r)))
      (setq pc (vl-catch-all-apply 'vla-Add (list pcs "BLKPDF_TMP_MEDIA" :vlax-false)))
      (if (not (vl-catch-all-error-p pc))
        (progn
          (vl-catch-all-apply 'vla-put-ConfigName (list pc dev))
          (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list pc))
          (vl-catch-all-apply 'vla-put-PaperUnits (list pc 1))
          (vl-catch-all-apply 'vla-put-PlotRotation (list pc 0))
          (setq media (blkpdf:media pc))
          (vl-catch-all-apply 'vla-Delete (list pc))))))
  media)

;; nearest media to dx x dy (either orientation) -> (width height name) oriented like the box
(defun vidgen:nearest (media dx dy / best bd w h d1 d2 cw ch d sc)
  (foreach it media
    (setq w  (nth 2 it)
          h  (nth 3 it)
          d1 (+ (expt (- w dx) 2) (expt (- h dy) 2))
          d2 (+ (expt (- h dx) 2) (expt (- w dy) 2)))
    (if (<= d1 d2)
      (setq cw w ch h d d1)
      (setq cw h ch w d d2))
    (setq sc (+ d (* 0.0001 (blkpdf:media-score (cadr it)))))
    (if (or (null bd) (< sc bd))
      (setq bd sc best (list cw ch (cadr it)))))
  best)

;; ---- set a property on the inserted reference (dynamic property first, attribute too)
(defun vidgen:set-dyn (obj key val / res done nm v)
  (setq res (vl-catch-all-apply 'vla-get-IsDynamicBlock (list obj)))
  (if (and (not (vl-catch-all-error-p res)) (= res :vlax-true))
    (foreach p (blkpdf:var->list
                 (vl-catch-all-apply 'vla-GetDynamicBlockProperties (list obj)))
      (if (and (null done)
               (setq nm (blkpdf:get 'vla-get-PropertyName p))
               (= (strcase (blkpdf:trim nm)) key))
        (progn
          (setq v (if (= (type val) 'STR)
                    (vlax-make-variant val vlax-vbString)
                    (vlax-make-variant (float val) vlax-vbDouble)))
          (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-Value (list p v))))
            (setq done T))))))
  done)

(defun vidgen:set-att (obj key val / res done nm)
  (setq res (vl-catch-all-apply 'vla-get-HasAttributes (list obj)))
  (if (and (not (vl-catch-all-error-p res)) (= res :vlax-true))
    (foreach a (blkpdf:var->list (vl-catch-all-apply 'vla-GetAttributes (list obj)))
      (if (and (null done)
               (setq nm (blkpdf:get 'vla-get-TagString a))
               (= (strcase (blkpdf:trim nm)) key))
        (if (not (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-put-TextString
                     (list a (if (= (type val) 'STR) val (rtos val 2 2))))))
          (setq done T)))))
  done)

(defun vidgen:set-prop (obj key val / a b)
  (setq a (vidgen:set-dyn obj key val)
        b (vidgen:set-att obj key val))
  (or a b))

(defun c:VIDLAYOUTGEN ( / *error* doc undo mode blk na sheet def dwg bx p1 p2 x1 y1 dx dy
                          media near w h ip rot obj)
  (defun *error* (msg)
    (redraw)
    (if (and undo doc) (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (blkpdf:report-error "VIDLAYOUTGEN" msg)
    (princ))

  (setq *blkpdf-warn* nil *blkpdf-fatal* nil *vidgen-auto-ename* nil *vidgen-auto-sheet* nil *vidgen-auto-na* nil)
  (blkpdf:step "start")
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= (getvar "BLOCKEDITOR") 1)
    (blkpdf:fail "Not available while the Block Editor is open."))
  (if (/= (getvar "TILEMODE") 1)
    (blkpdf:fail "Switch to the Model tab first; the layout block is drawn/selected in model space."))

  ;; ---- 1. UI
  (blkpdf:step "1 dialog")
  (setq def (blkpdf:sheet-base))
  (if (not (vidgen:ui *vidgen-mode-default* *vidgen-blk-default* *vidgen-na-default* (if def def "")))
    (progn (prompt "\nVIDLAYOUTGEN: cancelled.") (exit)))
  (setq mode  *vidgen-mode*
        blk   *vidgen-blk*
        na    *vidgen-na*
        sheet *vidgen-sheet*)

  (setq undo (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-StartUndoMark (list doc)))))

  ;; ---- 2-5 only in "Draw" mode (in "Select" mode the block is picked by VIDPAGELAYOUT)
  (if (= mode "Draw")
   (progn
  ;; ---- 2. import the block from vid_blocks_tools.dwg (skipped if already in the drawing)
  (blkpdf:step "2 import layout block")
  (cond
    ((tblsearch "BLOCK" blk)
     (prompt (strcat "\n  Block " blk " already in drawing - import skipped.")))
    ((null (setq dwg (blkpdf:na-find-dwg)))
     (blkpdf:fail (strcat "\"" *blkpdf-g-dwg-name* "\" was not found; cannot import " blk ".")))
    ((not (blkpdf:g-import-block blk dwg))
     (blkpdf:fail (strcat "Block \"" blk "\" could not be loaded from the DWG.")))
    (T (prompt (strcat "\n  Imported block " blk " from " dwg))))

  ;; ---- 3. rough box (dashed rubber band)
  (blkpdf:step "3 rough box")
  (setq bx (vidgen:pick-box))
  (if (null bx) (blkpdf:fail "Cancelled: no layout box drawn."))
  (setq p1 (car bx) p2 (cadr bx)
        x1 (min (car p1) (car p2))
        y1 (min (cadr p1) (cadr p2))
        dx (abs (- (car p2) (car p1)))
        dy (abs (- (cadr p2) (cadr p1))))
  (if (or (< dx 0.001) (< dy 0.001))
    (blkpdf:fail "The rough box has no width or height."))
  (prompt (strcat "\n  Rough box: " (rtos dx 2 2) " x " (rtos dy 2 2) " (drawing units = sheet mm)"))

  ;; ---- 4. nearest paper size of the PDF device
  (blkpdf:step "4 nearest paper size")
  (setq media (vidgen:media doc))
  (if media
    (progn
      (setq near (vidgen:nearest media dx dy)
            w (car near) h (cadr near))
      (prompt (strcat "\n  Nearest paper size: " (caddr near)
                      "  ->  layout_width " (rtos w 2 2) ", layout_height " (rtos h 2 2))))
    (progn
      (setq w (float (fix (+ dx 0.5)))
            h (float (fix (+ dy 0.5))))
      (blkpdf:warn "Paper sizes of the PDF device unavailable: rough box rounded to whole mm.")))

  ;; ---- 5. insert block at the box's lower-left corner, aligned to the current UCS
  (blkpdf:step "5 insert block")
  (setq ip  (trans (list x1 y1 (caddr p1)) 1 0)
        rot (angle '(0.0 0.0 0.0) (trans '(1.0 0.0 0.0) 1 0 T)))
  (setq obj (vla-InsertBlock (vla-get-ModelSpace doc) (vlax-3d-point ip) blk 1.0 1.0 1.0 rot))
  (if (not (and (vidgen:set-prop obj "LAYOUT_WIDTH" w)
                (vidgen:set-prop obj "LAYOUT_HEIGHT" h)))
    (progn
      (vl-catch-all-apply 'vla-Delete (list obj))
      (blkpdf:fail "Block has no \"layout_width\" / \"layout_height\" property or attribute.")))
  (if (/= sheet "")
    (if (not (vidgen:set-prop obj "SHEET_NAME" sheet))
      (blkpdf:warn "Block has no \"sheet_name\" property/attribute: sheet name not stored in the block.")))
  (vl-catch-all-apply 'vla-Update (list obj))

  (if undo (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
  (setq undo nil)
  (foreach wmsg (reverse *blkpdf-warn*) (prompt (strcat "\n  ! " wmsg)))
  (prompt (strcat "\nVIDLAYOUTGEN: " blk " inserted, layout_width " (rtos w 2 2)
                  ", layout_height " (rtos h 2 2)
                  (if (/= sheet "") (strcat ", sheet \"" sheet "\"") "")
                  ". Creating layout ..."))
  (setq *vidgen-auto-ename* (vlax-vla-object->ename obj))))

  ;; ---- 6. create the layout: Draw = from the block just inserted (no pick);
  ;;         Select = VIDPAGELAYOUT asks the user to select the layout block
  (blkpdf:step "6 create layout")
  (if undo (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
  (setq undo nil)
  (setq *vidgen-auto-sheet* (if (/= sheet "") sheet nil)
        *vidgen-auto-na*    na)
  (c:VIDPAGELAYOUT)
  (setq *vidgen-auto-ename* nil *vidgen-auto-sheet* nil *vidgen-auto-na* nil)
  (princ))

(prompt "\nVIDPAGELAYOUT rev 21 loaded. Commands: VIDPAGELAYOUT, VIDLAYOUTGEN, VIDMEDIA, UCSF.")
(princ)
