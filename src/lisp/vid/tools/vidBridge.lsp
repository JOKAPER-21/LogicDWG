;;; ------------------------------------------------------------------
;;; VIDBRIDGE  (v5)
;;; Top-view bridge drawing tool for AutoCAD and AutoCAD Civil 3D.
;;; Load with APPLOAD (or drag & drop), then type VIDBRIDGE.
;;;
;;; The command opens one window:
;;;
;;;  Settings
;;;    Type         (radio) Mid Point     = pillars run parallel to the line
;;;                         that joins the two edge midpoints
;;;                         Perpendicular = pillars run perpendicular to the
;;;                         LARGER edge, inside the stretch where both edges
;;;                         overlap
;;;    Select Track Pick the main track (line, polyline, arc, spline)
;;;  Leader Settings   multileader text, side (LHS/RHS), Leader Height
;;;                    (= length of the leader line)
;;;  Bridges Settings  LHS BOX and RHS BOX, each with
;;;                    Draw (3-point box), Pillar Count, Pillar Width,
;;;                    Pillar Align Type (flex-start, flex-end, center,
;;;                    space-between, space-around, space-evenly, custom)
;;;                    and Gap (used by "custom" only)
;;;  Create           draws the pillars and the leader
;;;
;;; The facing, parallel edges of the two boxes are detected automatically.
;;; Every pillar is ONE pair of 2-point polylines that runs from the LHS box
;;; edge to the RHS box edge. The pillar is laid out on each edge with that
;;; side's own width / align type, so when LHS and RHS widths differ the
;;; pillar tapers from one width to the other. Both sides need the same
;;; Pillar Count. Nothing is hatched or filled.
;;; The leader starts where the track crosses the line between the two edge
;;; midpoints and runs perpendicular to the track on the chosen side.
;;; "flex-start" is the side that comes first along the track direction.
;;;
;;; One undo (U) reverts everything the command did.
;;; Everything is drawn in the WCS plane (Z = 0).
;;; ------------------------------------------------------------------

(vl-load-com)

;; ---- settings that persist between runs (within a session) -------
(setq *VIDBR-TYP*  (cond (*VIDBR-TYP*)  (T 0))                  ; 0 Mid Point, 1 Perpendicular
      *VIDBR-SIDE* (cond (*VIDBR-SIDE*) (T 0))                  ; 0 LHS, 1 RHS
      *VIDBR-TXT*  (cond (*VIDBR-TXT*)  (T "BR.00 @KM 00/000"))
      *VIDBR-HGT*  (cond (*VIDBR-HGT*)  (T "80"))
      *VIDBR-LCNT* (cond (*VIDBR-LCNT*) (T "1"))
      *VIDBR-LWID* (cond (*VIDBR-LWID*) (T "2"))
      *VIDBR-LJUS* (cond (*VIDBR-LJUS*) (T 2))                  ; 2 = center
      *VIDBR-LGAP* (cond (*VIDBR-LGAP*) (T ""))
      *VIDBR-RCNT* (cond (*VIDBR-RCNT*) (T "1"))
      *VIDBR-RWID* (cond (*VIDBR-RWID*) (T "2"))
      *VIDBR-RJUS* (cond (*VIDBR-RJUS*) (T 2))
      *VIDBR-RGAP* (cond (*VIDBR-RGAP*) (T "")))

;; ---- colours (AutoCAD colour index) ----------------------------------
(setq *VIDBR-COL-LINE* 7      ; pillar lines (white / black)
      *VIDBR-COL-BOX*  1)     ; red boxes

;; ---- multileader settings -----------------------------------------------
(setq *VIDBR-ML-STYLE* "asset name"   ; multileader style
      *VIDBR-ML-LAYER* "1-TEXT"
      *VIDBR-ML-LTSCALE* 5.0
      *VIDBR-ML-TEXTH*  2.0           ; text height
      *VIDBR-ML-TEXTW*  70.0          ; text width
      *VIDBR-ML-ARROW*  2.5           ; arrowhead size (arrowhead = none)
      *VIDBR-ML-GAP*    2.0           ; landing gap
      *VIDBR-ML-LSPACE* 3.33)         ; line space distance (at least)

;; ---- small 2D vector helpers ----------------------------------------
(defun VID:Br-Sub (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun VID:Br-Add (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(defun VID:Br-Mul (a k) (list (* (car a) k) (* (cadr a) k)))
(defun VID:Br-Dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun VID:Br-Crs (a b) (- (* (car a) (cadr b)) (* (cadr a) (car b))))
(defun VID:Br-Len (a) (sqrt (VID:Br-Dot a a)))
(defun VID:Br-Mid (a b) (VID:Br-Mul (VID:Br-Add a b) 0.5))

(defun VID:Br-Unit (a / l)
  (setq l (VID:Br-Len a))
  (if (> l 1e-12) (VID:Br-Mul a (/ 1.0 l)) (list 0.0 0.0))
)

;; centre of a 4-corner box
(defun VID:Br-Cen (c)
  (VID:Br-Mul (VID:Br-Add (VID:Br-Add (nth 0 c) (nth 1 c))
                          (VID:Br-Add (nth 2 c) (nth 3 c))) 0.25)
)

;; UCS point -> WCS 2D point
(defun VID:Br-Wcs (p)
  (setq p (trans p 1 0))
  (list (car p) (cadr p))
)

;; keep an angle inside 0 .. 2*pi
(defun VID:Br-Norm (a)
  (setq a (rem a (* 2.0 pi)))
  (if (< a 0.0) (+ a (* 2.0 pi)) a)
)

;; number text: 3 -> "3", 2.5 -> "2.5"
(defun VID:Br-Fmt (v / s)
  (cond
    ((equal v (float (fix v)) 1e-9) (itoa (fix v)))
    (T
     (setq s (rtos v 2 6))
     (while (and (wcmatch s "*.*") (= (substr s (strlen s)) "0"))
       (setq s (substr s 1 (1- (strlen s)))))
     s)
  )
)

;; text -> number, or nil when it is not a plain number
(defun VID:Br-Num (s)
  (if (= (type s) 'STR) (distof (vl-string-trim " " s) 2))
)

;; text -> number > 0, or nil
(defun VID:Br-Pos (s / v)
  (setq v (VID:Br-Num s))
  (if (and v (> v 0.0)) v)
)

;; text -> number >= 0, or nil
(defun VID:Br-Gap (s / v)
  (setq v (VID:Br-Num s))
  (if (and v (>= v 0.0)) v)
)

;; text -> whole number >= 0, or nil
(defun VID:Br-IntStr (s / n)
  (setq n (atoi s))
  (if (and (>= n 0) (= (itoa n) (vl-string-trim " " s))) n)
)

;; T when the entity exists and is not erased
(defun VID:Br-Live (en / r)
  (if en
    (progn
      (setq r (vl-catch-all-apply
                '(lambda () (not (vlax-erased-p (vlax-ename->vla-object en))))
                nil))
      (if (vl-catch-all-error-p r) nil r)
    )
  )
)

;; ---- drawing helpers ---------------------------------------------------
(defun VID:Br-Space (doc)
  (if (or (= (getvar "TILEMODE") 1) (> (getvar "CVPORT") 1))
    (vla-get-ModelSpace doc)
    (vla-get-PaperSpace doc))
)

;; LWPOLYLINE through 2D points (closed when closed is T)
(defun VID:Br-Poly (pts col closed)
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
            (cons 90 (length pts)) (cons 70 (if closed 1 0)) (cons 62 col))
      (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts)))
)

;; ---- 3-point box -----------------------------------------------------
;; P1-P2 = one edge, P3 = width and side. Returns 4 corners (WCS 2D),
;; or nil when the user presses Enter instead of picking a point.
(defun VID:Br-Box3P (title / u1 u2 u3 p1 p2 p3 u n d)
  (setq u1 (getpoint (strcat "\n" title " - first point of edge: ")))
  (if u1 (setq u2 (getpoint u1 "\nSecond point of edge: ")))
  (if u2 (setq u3 (getpoint u2 "\nThird point (width and side): ")))
  (cond
    ((not (and u1 u2 u3)) nil)
    (T
     (setq p1 (VID:Br-Wcs u1)
           p2 (VID:Br-Wcs u2)
           p3 (VID:Br-Wcs u3)
           u  (VID:Br-Unit (VID:Br-Sub p2 p1))
           n  (list (- (cadr u)) (car u))
           d  (VID:Br-Dot (VID:Br-Sub p3 p2) n))
     (if (or (< (VID:Br-Len (VID:Br-Sub p2 p1)) 1e-9) (< (abs d) 1e-9))
       (progn
         (prompt "\nInvalid box (zero length or width). Try again.")
         (VID:Br-Box3P title))
       (list p1
             p2
             (VID:Br-Add p2 (VID:Br-Mul n d))
             (VID:Br-Add p1 (VID:Br-Mul n d))))
    )
  )
)

;; draw (or redraw) the LHS / RHS box; ESC or Enter keeps the old one
(defun VID:Br-DrawBox (side / c en)
  (setq c (vl-catch-all-apply 'VID:Br-Box3P (list (strcat side " box"))))
  (if (and c (not (vl-catch-all-error-p c)))
    (progn
      (setq en (VID:Br-Poly c *VIDBR-COL-BOX* T))
      (if (= side "LHS")
        (progn
          (if (VID:Br-Live *VIDBR-LBOXEN*) (entdel *VIDBR-LBOXEN*))
          (setq *VIDBR-LBOX* c *VIDBR-LBOXEN* en))
        (progn
          (if (VID:Br-Live *VIDBR-RBOXEN*) (entdel *VIDBR-RBOXEN*))
          (setq *VIDBR-RBOX* c *VIDBR-RBOXEN* en))
      )
    )
    (prompt (strcat "\n" side " box not drawn."))
  )
)

;; ---- edges -------------------------------------------------------------
(defun VID:Br-Edge (c i)
  (list (nth i c) (nth (rem (1+ i) 4) c))
)

;; Auto: nearest FACING, PARALLEL pair of edges (one edge per box).
;; Parallel = within about 15 degrees. If no pair qualifies, the pair with
;; the closest midpoints is used.
(defun VID:Br-AutoEdges (ba bb / bd best bd2 best2 i j ea eb d ma mb ua ub)
  (setq bd 1e99 bd2 1e99 i 0)
  (repeat 4
    (setq ea (VID:Br-Edge ba i)
          ma (VID:Br-Mid (car ea) (cadr ea))
          ua (VID:Br-Unit (VID:Br-Sub (cadr ea) (car ea)))
          j  0)
    (repeat 4
      (setq eb (VID:Br-Edge bb j)
            mb (VID:Br-Mid (car eb) (cadr eb))
            ub (VID:Br-Unit (VID:Br-Sub (cadr eb) (car eb)))
            d  (distance ma mb))
      (if (< d bd2) (setq bd2 d best2 (list ea eb)))
      (if (and (< (abs (VID:Br-Crs ua ub)) 0.26)
               (> (VID:Br-Dot (VID:Br-Sub ma (VID:Br-Cen ba)) (VID:Br-Sub mb ma)) 0.0)
               (> (VID:Br-Dot (VID:Br-Sub mb (VID:Br-Cen bb)) (VID:Br-Sub ma mb)) 0.0)
               (< d bd))
        (setq bd d best (list ea eb)))
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  (if best best best2)
)

;; ---- pillars ------------------------------------------------------------
;; Start offset of pillar i inside a free length len (like CSS
;; justify-content). mode: 0 flex-start, 1 flex-end, 2 center,
;; 3 space-between, 4 space-around, 5 space-evenly, 6 custom (fixed gap,
;; group centred)
(defun VID:Br-Just (len n w mode gap i / g)
  (cond
    ((= mode 0) (* i w))
    ((= mode 1) (+ (- len (* n w)) (* i w)))
    ((= mode 2) (+ (/ (- len (* n w)) 2.0) (* i w)))
    ((= mode 3) (if (= n 1) 0.0 (/ (* i (- len w)) (1- n))))
    ((= mode 4)
     (setq g (/ (- len (* n w)) n))
     (+ (/ g 2.0) (* i (+ w g))))
    ((= mode 5)
     (setq g (/ (- len (* n w)) (1+ n)))
     (+ g (* i (+ w g))))
    (T
     (+ (/ (- len (+ (* n w) (* (1- n) gap))) 2.0) (* i (+ w gap))))
  )
)

;; y of the edge line (p-q seen in the pv/nv frame) at coordinate x
(defun VID:Br-EdgeY (xp yp xq yq x)
  (if (< (abs (- xq xp)) 1e-9)
    (/ (+ yp yq) 2.0)
    (+ yp (* (- yq yp) (/ (- x xp) (- xq xp)))))
)

;; projection range of an edge on the pv axis
(defun VID:Br-Ext (e pv / a b)
  (setq a (VID:Br-Dot (car e) pv)
        b (VID:Br-Dot (cadr e) pv))
  (list (min a b) (max a b))
)

;; point from frame coordinates (x along pv, y along nv)
(defun VID:Br-Pt (pv nv x y)
  (VID:Br-Add (VID:Br-Mul pv x) (VID:Br-Mul nv y))
)

;; edge p-q seen in the pv/nv frame: (xp yp xq yq)
(defun VID:Br-EdgeFrame (ed pv nv / p q)
  (setq p (car ed) q (cadr ed))
  (list (VID:Br-Dot p pv) (VID:Br-Dot p nv) (VID:Br-Dot q pv) (VID:Br-Dot q nv))
)

(defun VID:Br-EdgeYf (f x)
  (VID:Br-EdgeY (nth 0 f) (nth 1 f) (nth 2 f) (nth 3 f) x)
)

;; Start positions (on the pv axis) of n pillars of width w laid out inside
;; [lo, hi]. Returns a list of n numbers, or nil (with a message) when the
;; pillars do not fit.
(defun VID:Br-Layout (lo hi n w mode gap lbl / len need i out)
  (setq len  (- hi lo)
        need (if (= mode 6) (+ (* n w) (* (1- n) gap)) (* n w))
        i    0)
  (if (> need (+ len 1e-9))
    (progn
      (prompt (strcat "\n" lbl ": the pillars need " (VID:Br-Fmt need)
                      " but only " (VID:Br-Fmt len)
                      " is available between the box edges."
                      " Reduce Pillar Width / Count (or the Gap), or move the boxes."))
      nil)
    (progn
      (repeat n
        (setq out (cons (+ lo (VID:Br-Just len n w mode gap i)) out)
              i   (1+ i)))
      (reverse out))
  )
)

;; Draw the pillars. Pillar i starts at xsl[i] (LHS edge, width wl) and at
;; xsr[i] (RHS edge, width wr). Each pillar = 2 polylines, every one running
;; from the LHS edge to the RHS edge (so the pillar tapers when wl <> wr).
;; Returns the number of pillars.
(defun VID:Br-Connect (el er xsl wl xsr wr pv nv / fl fr done cnt xl0 xr0 pr)
  (setq fl  (VID:Br-EdgeFrame el pv nv)
        fr  (VID:Br-EdgeFrame er pv nv)
        cnt 0)
  (while (and xsl xsr)
    (setq xl0 (car xsl)
          xr0 (car xsr))
    (foreach pr (list (cons xl0 xr0) (cons (+ xl0 wl) (+ xr0 wr)))
      (if (not (vl-some '(lambda (v) (and (< (abs (- (car v) (car pr))) 1e-9)
                                          (< (abs (- (cdr v) (cdr pr))) 1e-9)))
                        done))
        (progn
          (VID:Br-Poly
            (list (VID:Br-Pt pv nv (car pr) (VID:Br-EdgeYf fl (car pr)))
                  (VID:Br-Pt pv nv (cdr pr) (VID:Br-EdgeYf fr (cdr pr))))
            *VIDBR-COL-LINE* nil)
          (setq done (cons pr done))
        )
      )
    )
    (setq cnt (1+ cnt)
          xsl (cdr xsl)
          xsr (cdr xsr))
  )
  cnt
)

;; ---- main track crossing ---------------------------------------------
;; signed side of the track point at parameter prm, relative to the line m1 + d
(defun VID:Br-SideF (en prm m1 d / p)
  (setq p (vlax-curve-getPointAtParam en prm))
  (VID:Br-Crs d (VID:Br-Sub (list (car p) (cadr p)) m1))
)

;; crossing of the track with the line through m1-m2 (2D, elevation ignored)
;; returns (param point-2D inside-m1-m2 found)
(defun VID:Br-FindCross (en m1 m2 / d lm s0 s1 n i pr0 pr1 f0 f1 a b fa fm mid
                                     roots p tt bi dd bd inr best cp prm)
  (setq d  (VID:Br-Unit (VID:Br-Sub m2 m1))
        lm (distance m1 m2)
        s0 (vlax-curve-getStartParam en)
        s1 (vlax-curve-getEndParam en)
        n  (max 2000 (min 20000 (fix (* 4.0 (- s1 s0)))))
        pr0 s0
        f0 (VID:Br-SideF en s0 m1 d)
        i  1)
  (repeat n
    (setq pr1 (+ s0 (* (- s1 s0) (/ (float i) n)))
          f1  (VID:Br-SideF en pr1 m1 d))
    (cond
      ((= f1 0.0)
       (setq roots (cons pr1 roots)))
      ((< (* f0 f1) 0.0)
       (setq a pr0 b pr1 fa f0)
       (repeat 40
         (setq mid (* 0.5 (+ a b))
               fm  (VID:Br-SideF en mid m1 d))
         (if (< (* fa fm) 0.0)
           (setq b mid)
           (setq a mid fa fm)))
       (setq roots (cons (* 0.5 (+ a b)) roots)))
    )
    (setq pr0 pr1 f0 f1 i (1+ i))
  )
  ;; best root: inside the m1-m2 span, closest to its centre
  (setq bd 1e99 inr nil)
  (foreach r roots
    (setq p  (vlax-curve-getPointAtParam en r)
          tt (VID:Br-Dot (VID:Br-Sub (list (car p) (cadr p)) m1) d)
          bi (and (>= tt -1e-6) (<= tt (+ lm 1e-6)))
          dd (abs (- tt (* 0.5 lm))))
    (if (or (and bi (not inr))
            (and (eq bi inr) (< dd bd)))
      (setq inr bi
            bd  dd
            best (list r (list (car p) (cadr p)) bi T)))
  )
  (if (null best)
    ;; no crossing at all: use the track point closest to the midline centre
    (progn
      (setq cp  (VID:Br-Mid m1 m2)
            p   (vlax-curve-getClosestPointTo en (list (car cp) (cadr cp) 0.0))
            prm (vlax-curve-getParamAtPoint en p)
            best (list prm (list (car p) (cadr p)) nil nil))))
  best
)

;; unit direction of the track at parameter prm
(defun VID:Br-Tangent (en prm / d s0 s1 a b pa pb)
  (setq d (vlax-curve-getFirstDeriv en prm))
  (if (> (VID:Br-Len (list (car d) (cadr d))) 1e-12)
    (VID:Br-Unit (list (car d) (cadr d)))
    (progn
      (setq s0 (vlax-curve-getStartParam en)
            s1 (vlax-curve-getEndParam en)
            a  (max s0 (- prm (* 0.001 (- s1 s0))))
            b  (min s1 (+ prm (* 0.001 (- s1 s0))))
            pa (vlax-curve-getPointAtParam en a)
            pb (vlax-curve-getPointAtParam en b))
      (VID:Br-Unit (list (- (car pb) (car pa)) (- (cadr pb) (cadr pa))))
    )
  )
)

;; ---- leader text (MLEADER) ---------------------------------------------
(defun VID:Br-EnsureLayer (name)
  (if (not (tblsearch "LAYER" name))
    (entmakex (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                    '(100 . "AcDbLayerTableRecord") (cons 2 name)
                    '(70 . 0) '(62 . 7) '(6 . "Continuous"))))
)

(defun VID:Br-EnsureLtype (doc name)
  (if (not (tblsearch "LTYPE" name))
    (foreach f '("acad.lin" "acadiso.lin")
      (if (not (tblsearch "LTYPE" name))
        (vl-catch-all-apply 'vla-Load (list (vla-get-Linetypes doc) name f)))))
)

;; ByLayer colour object for the leader line colour (nil if unavailable)
(defun VID:Br-ByLayerColor (/ c)
  (setq c (vl-catch-all-apply
            'vla-GetInterfaceObject
            (list (vlax-get-acad-object)
                  (strcat "AutoCAD.AcCmColor." (itoa (atoi (getvar "ACADVER")))))))
  (if (vl-catch-all-error-p c)
    nil
    (progn (vl-catch-all-apply 'vla-put-ColorMethod (list c 192)) c))
)

;; set one property (T = done); remember the name when AutoCAD refuses it
(defun VID:Br-Set (fn obj val)
  (if (vl-catch-all-error-p (vl-catch-all-apply fn (list obj val)))
    (progn
      (setq *VIDBR-FAILS* (cons (substr (vl-symbol-name fn) 9) *VIDBR-FAILS*))
      nil)
    T)
)

;; read one property as text ("?" when it cannot be read)
(defun VID:Br-Get (fn obj / r)
  (setq r (vl-catch-all-apply fn (list obj)))
  (if (vl-catch-all-error-p r) "?" (vl-princ-to-string r))
)

;; Multileader with its tip at p1 and the text running perpendicular to the
;; track. It is built horizontally and then rotated about p1:
;;   flip = nil : leader goes +X from p1, text to its right, rotated by ang
;;   flip = T   : leader goes -X from p1, text to its left,  rotated by ang+pi
;; (ang = direction of the leader on the chosen side, in radians; flip is used
;; when that direction would make the text upside down)
;; len = leader line length.
;; Falls back to LINE + TEXT when the multileader cannot be created.
(defun VID:Br-Leader (doc p1 str len ang flip / sp pts ml bc p2 rot)
  (VID:Br-EnsureLayer *VIDBR-ML-LAYER*)
  (VID:Br-EnsureLtype doc "HIDDEN")
  (setq *VIDBR-FAILS* nil
        sp  (VID:Br-Space doc)
        rot (if flip (VID:Br-Norm (+ ang pi)) ang)
        p2  (list (if flip (- (car p1) len) (+ (car p1) len)) (cadr p1))
        pts (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill pts (list (car p1) (cadr p1) 0.0 (car p2) (cadr p2) 0.0))
  (setq ml (vl-catch-all-apply 'vla-AddMLeader (list sp pts 0)))
  (if (vl-catch-all-error-p ml) (setq ml nil))
  (cond
    (ml
     ;; style first (it resets overrides), then the overrides
     (if (vl-catch-all-error-p
           (vl-catch-all-apply 'vla-put-StyleName (list ml *VIDBR-ML-STYLE*)))
       (prompt (strcat "\nMultileader style \"" *VIDBR-ML-STYLE*
                       "\" not found - the current style is used.")))
     ;; ---- General
     (VID:Br-Set 'vla-put-Color ml 256)                           ; ByLayer
     (VID:Br-Set 'vla-put-Layer ml *VIDBR-ML-LAYER*)
     (VID:Br-Set 'vla-put-Linetype ml "ByLayer")
     (VID:Br-Set 'vla-put-LinetypeScale ml *VIDBR-ML-LTSCALE*)
     ;; plot style: "ByColor" is built in when PSTYLEMODE = 1
     (if (= (getvar "PSTYLEMODE") 0)
       (VID:Br-Set 'vla-put-PlotStyleName ml "ByColor"))
     (VID:Br-Set 'vla-put-Lineweight ml -1)                       ; ByLayer
     (VID:Br-Set 'vla-put-EntityTransparency ml "ByLayer")
     ;; ---- Misc
     (VID:Br-Set 'vla-put-ScaleFactor ml 1.0)
     (if (vl-catch-all-error-p
           (vl-catch-all-apply 'vlax-put-property (list ml 'Annotative :vlax-false)))
       (setq *VIDBR-FAILS* (cons "ANNOTATIVE" *VIDBR-FAILS*)))
     ;; ---- Leaders
     (VID:Br-Set 'vla-put-LeaderType ml 1)                        ; straight
     (setq bc (VID:Br-ByLayerColor))
     (if bc (VID:Br-Set 'vla-put-LeaderLineColor ml bc))
     (VID:Br-Set 'vla-put-LeaderLinetype ml "HIDDEN")
     (VID:Br-Set 'vla-put-LeaderLineWeight ml -1)                 ; ByLayer
     ;; arrowhead = none: by block name (the "_NONE" arrow); the numeric
     ;; arrow type is only a fallback
     (if (not (VID:Br-Set 'vla-put-ArrowheadBlock ml "_NONE"))
       (VID:Br-Set 'vla-put-ArrowheadType ml 19))
     (VID:Br-Set 'vla-put-ArrowheadSize ml *VIDBR-ML-ARROW*)
     (VID:Br-Set 'vla-put-DogLegged ml :vlax-false)
     (VID:Br-Set 'vla-put-DoglegLength ml 0.0)
     ;; ---- Text
     (VID:Br-Set 'vla-put-TextString ml str)
     (VID:Br-Set 'vla-put-TextJustify ml 1)                       ; left
     (VID:Br-Set 'vla-put-TextWidth ml *VIDBR-ML-TEXTW*)
     (VID:Br-Set 'vla-put-TextHeight ml *VIDBR-ML-TEXTH*)
     (VID:Br-Set 'vla-put-TextLineSpacingFactor ml 1.0)
     (VID:Br-Set 'vla-put-TextLineSpacingDistance ml *VIDBR-ML-LSPACE*)
     (VID:Br-Set 'vla-put-TextLineSpacingStyle ml 1)              ; at least
     (VID:Br-Set 'vla-put-TextBackgroundFill ml :vlax-false)
     (VID:Br-Set 'vla-put-TextAttachmentDirection ml 0)           ; horizontal
     (VID:Br-Set 'vla-put-TextLeftAttachmentType ml 4)            ; middle of text
     (VID:Br-Set 'vla-put-TextRightAttachmentType ml 4)           ; middle of text
     (VID:Br-Set 'vla-put-LandingGap ml *VIDBR-ML-GAP*)
     (VID:Br-Set 'vla-put-TextFrameDisplay ml :vlax-false)
     ;; rotation: turn the whole multileader about its tip, so the text ends
     ;; up perpendicular to the track
     (if (vl-catch-all-error-p
           (vl-catch-all-apply 'vla-Rotate
                               (list ml (vlax-3d-point (car p1) (cadr p1) 0.0) rot)))
       (setq *VIDBR-FAILS* (cons "ROTATION" *VIDBR-FAILS*)))
     (if *VIDBR-FAILS*
       (prompt (strcat "\nMultileader: these properties could not be set: "
                       (apply 'strcat
                              (cdr (apply 'append
                                          (mapcar '(lambda (x) (list ", " x))
                                                  (reverse *VIDBR-FAILS*))))))))
     ;; read-back, to compare with the Properties palette
     (prompt (strcat "\nMultileader check - arrowhead block: "
                     (VID:Br-Get 'vla-get-ArrowheadBlock ml)
                     ", arrowhead type: " (VID:Br-Get 'vla-get-ArrowheadType ml)
                     ", left/right attachment: "
                     (VID:Br-Get 'vla-get-TextLeftAttachmentType ml) "/"
                     (VID:Br-Get 'vla-get-TextRightAttachmentType ml)
                     ", leader type: " (VID:Br-Get 'vla-get-LeaderType ml)))
     T)
    (T
     (prompt "\nMultileader could not be created - a line and plain text were drawn instead.")
     (setq p2 (VID:Br-Add p1 (VID:Br-Mul (list (cos ang) (sin ang)) len)))
     (entmakex (list '(0 . "LINE")
                     (list 10 (car p1) (cadr p1) 0.0)
                     (list 11 (car p2) (cadr p2) 0.0)))
     (entmakex (list '(0 . "TEXT")
                     (list 10 (car p2) (cadr p2) 0.0)
                     (cons 40 *VIDBR-ML-TEXTH*) (cons 1 str) (cons 50 rot)
                     (cons 7 (getvar "TEXTSTYLE"))
                     (cons 72 (if flip 2 0))
                     (list 11 (car p2) (cadr p2) 0.0)
                     '(73 . 2)))
     nil)
  )
)

;; ---- dialog ---------------------------------------------------------------
(defun VID:Br-TrkLabel ()
  (if (VID:Br-Live *VIDBR-TRK*)
    (strcat "Track: " (cdr (assoc 0 (entget *VIDBR-TRK*))) " selected")
    "Track: (not selected)")
)

;; the Gap box is only usable for "custom"
(defun VID:Br-GapSync ()
  (mode_tile "lgap" (if (= (atoi (get_tile "ljus")) 6) 0 1))
  (mode_tile "rgap" (if (= (atoi (get_tile "rjus")) 6) 0 1))
)

(defun VID:Br-Read ()
  (setq *VIDBR-SIDE* (atoi (get_tile "side"))
        *VIDBR-TXT*  (get_tile "txt")
        *VIDBR-HGT*  (get_tile "hgt")
        *VIDBR-LCNT* (get_tile "lcnt")
        *VIDBR-LWID* (get_tile "lwid")
        *VIDBR-LJUS* (atoi (get_tile "ljus"))
        *VIDBR-LGAP* (get_tile "lgap")
        *VIDBR-RCNT* (get_tile "rcnt")
        *VIDBR-RWID* (get_tile "rwid")
        *VIDBR-RJUS* (atoi (get_tile "rjus"))
        *VIDBR-RGAP* (get_tile "rgap"))
)

(defun VID:Br-Valid (/ lc rc)
  (setq lc (VID:Br-IntStr *VIDBR-LCNT*)
        rc (VID:Br-IntStr *VIDBR-RCNT*))
  (cond
    ((not (VID:Br-Live *VIDBR-LBOXEN*))
     (alert "Draw the LHS box first (LHS box > Draw).") nil)
    ((not (VID:Br-Live *VIDBR-RBOXEN*))
     (alert "Draw the RHS box first (RHS box > Draw).") nil)
    ((not (VID:Br-Live *VIDBR-TRK*))
     (alert "Pick the track first (Select Track > Pick Track).") nil)
    ((not (VID:Br-Pos *VIDBR-HGT*))
     (alert "Leader Height must be a number greater than 0.") nil)
    ((or (null lc) (null rc) (< lc 1) (< rc 1))
     (alert "Pillar Count must be a whole number of 1 or more.") nil)
    ((/= lc rc)
     (alert "LHS and RHS need the same Pillar Count, because every pillar connects the two boxes.") nil)
    ((not (VID:Br-Pos *VIDBR-LWID*))
     (alert "LHS Pillar Width must be a number greater than 0.") nil)
    ((not (VID:Br-Pos *VIDBR-RWID*))
     (alert "RHS Pillar Width must be a number greater than 0.") nil)
    ((and (= *VIDBR-LJUS* 6) (not (VID:Br-Gap *VIDBR-LGAP*)))
     (alert "LHS Gap must be a number (0 or more) for the custom align type.") nil)
    ((and (= *VIDBR-RJUS* 6) (not (VID:Br-Gap *VIDBR-RGAP*)))
     (alert "RHS Gap must be a number (0 or more) for the custom align type.") nil)
    (T T)
  )
)

;; returns T when Create was pressed
(defun VID:Br-Dialog (/ tmp f dcl res sel)
  (setq tmp (vl-filename-mktemp "vidbridge.dcl")
        f   (open tmp "w"))
  (foreach l
    '("vidbridge : dialog {"
      "  label = \"VID Bridge\";"
      "  : boxed_column {"
      "    label = \"Settings\";"
      "    : row {"
      "      : text { label = \"Type\"; width = 14; }"
      "      : radio_row {"
      "        : radio_button { key = \"typ_mid\"; label = \"Mid Point\"; }"
      "        : radio_button { key = \"typ_perp\"; label = \"Perpendicular\"; }"
      "      }"
      "    }"
      "    : row {"
      "      : text { label = \"Select Track\"; width = 14; }"
      "      : button { key = \"trk\"; label = \"Pick Track\"; width = 20; fixed_width = true; }"
      "      : text { key = \"trkst\"; label = \"\"; width = 30; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Leader Settings\";"
      "    : edit_box { key = \"txt\"; label = \"Leader Text\"; edit_width = 56; }"
      "    : popup_list { key = \"side\"; label = \"Leader Text Side\"; list = \"LHS\\nRHS\"; edit_width = 8; }"
      "    : edit_box { key = \"hgt\"; label = \"Leader Height\"; edit_width = 8; }"
      "  }"
      "  : boxed_column {"
      "    label = \"Bridges Settings\";"
      "    : row {"
      "      : boxed_column {"
      "        label = \"LHS BOX\";"
      "        : row {"
      "          : text { label = \"LHS box\"; width = 9; }"
      "          : button { key = \"ldraw\"; label = \"Draw\"; width = 9; fixed_width = true; }"
      "          : text { key = \"lst\"; label = \"\"; width = 11; }"
      "        }"
      "        : edit_box { key = \"lcnt\"; label = \"Pillar Count\"; edit_width = 8; }"
      "        : edit_box { key = \"lwid\"; label = \"Pillar Width\"; edit_width = 8; }"
      "        : popup_list { key = \"ljus\"; label = \"Pillar Align Type\"; list = \"flex-start\\nflex-end\\ncenter\\nspace-between\\nspace-around\\nspace-evenly\\ncustom\"; edit_width = 16; }"
      "        : edit_box { key = \"lgap\"; label = \"Gap\"; edit_width = 8; }"
      "      }"
      "      : boxed_column {"
      "        label = \"RHS BOX\";"
      "        : row {"
      "          : text { label = \"RHS box\"; width = 9; }"
      "          : button { key = \"rdraw\"; label = \"Draw\"; width = 9; fixed_width = true; }"
      "          : text { key = \"rst\"; label = \"\"; width = 11; }"
      "        }"
      "        : edit_box { key = \"rcnt\"; label = \"Pillar Count\"; edit_width = 8; }"
      "        : edit_box { key = \"rwid\"; label = \"Pillar Width\"; edit_width = 8; }"
      "        : popup_list { key = \"rjus\"; label = \"Pillar Align Type\"; list = \"flex-start\\nflex-end\\ncenter\\nspace-between\\nspace-around\\nspace-evenly\\ncustom\"; edit_width = 16; }"
      "        : edit_box { key = \"rgap\"; label = \"Gap\"; edit_width = 8; }"
      "      }"
      "    }"
      "  }"
      "  : row {"
      "    : spacer { width = 1; }"
      "    : button { key = \"accept\"; label = \"Create\"; is_default = true; width = 14; fixed_width = true; }"
      "    : button { key = \"cancel\"; label = \"Close\"; is_cancel = true; width = 14; fixed_width = true; }"
      "    : spacer { width = 1; }"
      "  }"
      "}")
    (write-line l f))
  (close f)
  (setq dcl (load_dialog tmp)
        res 2)
  (while (>= res 2)
    (if (not (new_dialog "vidbridge" dcl))
      (setq res 0)
      (progn
        (set_tile "typ_mid"  (if (= *VIDBR-TYP* 0) "1" "0"))
        (set_tile "typ_perp" (if (= *VIDBR-TYP* 1) "1" "0"))
        (set_tile "side"  (itoa *VIDBR-SIDE*))
        (set_tile "txt"   *VIDBR-TXT*)
        (set_tile "hgt"   *VIDBR-HGT*)
        (set_tile "lcnt"  *VIDBR-LCNT*)
        (set_tile "lwid"  *VIDBR-LWID*)
        (set_tile "ljus"  (itoa *VIDBR-LJUS*))
        (set_tile "lgap"  *VIDBR-LGAP*)
        (set_tile "rcnt"  *VIDBR-RCNT*)
        (set_tile "rwid"  *VIDBR-RWID*)
        (set_tile "rjus"  (itoa *VIDBR-RJUS*))
        (set_tile "rgap"  *VIDBR-RGAP*)
        (set_tile "trkst" (VID:Br-TrkLabel))
        (set_tile "lst"   (if (VID:Br-Live *VIDBR-LBOXEN*) "drawn" "not drawn"))
        (set_tile "rst"   (if (VID:Br-Live *VIDBR-RBOXEN*) "drawn" "not drawn"))
        (VID:Br-GapSync)
        (action_tile "typ_mid"  "(setq *VIDBR-TYP* 0)")
        (action_tile "typ_perp" "(setq *VIDBR-TYP* 1)")
        (action_tile "ljus"     "(VID:Br-GapSync)")
        (action_tile "rjus"     "(VID:Br-GapSync)")
        (action_tile "trk"      "(progn (VID:Br-Read) (done_dialog 2))")
        (action_tile "ldraw"    "(progn (VID:Br-Read) (done_dialog 3))")
        (action_tile "rdraw"    "(progn (VID:Br-Read) (done_dialog 4))")
        (action_tile "accept"   "(progn (VID:Br-Read) (if (VID:Br-Valid) (done_dialog 1)))")
        (action_tile "cancel"   "(done_dialog 0)")
        (setq res (start_dialog))
        (cond
          ((= res 2)
           (setq sel (vl-catch-all-apply 'entsel (list "\nSelect main track: ")))
           (cond
             ((vl-catch-all-error-p sel))
             ((null sel))
             ((vl-catch-all-error-p
                (vl-catch-all-apply 'vlax-curve-getEndParam (list (car sel))))
              (alert "That object is not a curve. Select a line, polyline, arc or spline."))
             (T (setq *VIDBR-TRK* (car sel)))
           ))
          ((= res 3) (VID:Br-DrawBox "LHS"))
          ((= res 4) (VID:Br-DrawBox "RHS"))
        )
      )
    )
  )
  (unload_dialog dcl)
  (vl-file-delete tmp)
  (= res 1)
)

;; ---- build the pillars and the leader ------------------------------------
(defun VID:Br-Build (doc / bl br trk hgt eds el er ml mr lenl lenr cx prm cpt
                           tdir nv pv e extl extr lo1 hi1 lo2 hi2 n wl wr xsl xsr
                           sl sr side ang flip nc)
  (setq bl  *VIDBR-LBOX*
        br  *VIDBR-RBOX*
        trk *VIDBR-TRK*
        hgt (VID:Br-Num *VIDBR-HGT*)
        n   (atoi *VIDBR-LCNT*)
        wl  (VID:Br-Num *VIDBR-LWID*)
        wr  (VID:Br-Num *VIDBR-RWID*))

  ;; facing, parallel edges of the two boxes
  (setq eds (VID:Br-AutoEdges bl br)
        el  (car eds)
        er  (cadr eds)
        ml  (VID:Br-Mid (car el) (cadr el))
        mr  (VID:Br-Mid (car er) (cadr er))
        lenl (distance (car el) (cadr el))
        lenr (distance (car er) (cadr er)))
  (if (< (distance ml mr) 1e-9)
    (progn (prompt "\nThe two boxes touch or overlap - nothing to draw.") (exit)))

  ;; cross point of the track with the edge-midpoint line
  (setq cx   (VID:Br-FindCross trk ml mr)
        prm  (car cx)
        cpt  (cadr cx)
        tdir (VID:Br-Tangent trk prm))
  (if (not (nth 3 cx))
    (prompt "\nThe track does not cross the edge-midpoint line; its closest point is used."))
  (if (and (nth 3 cx) (not (nth 2 cx)))
    (prompt "\nThe track crosses outside the two edges; the nearest crossing is used."))

  ;; warn when LHS / RHS do not match the track direction
  (setq sl (VID:Br-Crs tdir (VID:Br-Sub (VID:Br-Cen bl) cpt))
        sr (VID:Br-Crs tdir (VID:Br-Sub (VID:Br-Cen br) cpt)))
  (if (not (and (> sl 0.0) (< sr 0.0)))
    (prompt "\nNote: the LHS box is not on the left of the track direction (or the RHS box not on the right)."))

  ;; pillar direction nv (from the LHS edge towards the RHS edge)
  (if (= *VIDBR-TYP* 0)
    ;; Mid Point: parallel to the line joining the two edge midpoints
    (setq nv (VID:Br-Unit (VID:Br-Sub mr ml)))
    ;; Perpendicular: perpendicular to the LARGER edge
    (progn
      (setq e  (if (>= lenl lenr) el er)
            nv (VID:Br-Unit (VID:Br-Sub (cadr e) (car e)))
            nv (list (- (cadr nv)) (car nv)))
      (if (< (VID:Br-Dot nv (VID:Br-Sub mr ml)) 0.0)
        (setq nv (VID:Br-Mul nv -1.0)))))
  (if (< (VID:Br-Dot nv (VID:Br-Sub mr ml)) 1e-9)
    (progn (prompt "\nThe detected edges do not face each other.") (exit)))

  ;; pv = axis along the pillar group, pointing the same way as the track
  (setq pv (list (- (cadr nv)) (car nv)))
  (if (< (VID:Br-Dot pv tdir) 0.0) (setq pv (VID:Br-Mul pv -1.0)))

  ;; where pillars may sit on each edge (pv axis)
  ;;   Mid Point     : the whole of each edge
  ;;   Perpendicular : only where both edges overlap, so a perpendicular
  ;;                   line meets both edges
  (setq extl (VID:Br-Ext el pv)
        extr (VID:Br-Ext er pv))
  (if (= *VIDBR-TYP* 0)
    (setq lo1 (car extl) hi1 (cadr extl)
          lo2 (car extr) hi2 (cadr extr))
    (setq lo1 (max (car extl) (car extr)) hi1 (min (cadr extl) (cadr extr))
          lo2 lo1 hi2 hi1))
  (if (or (<= (- hi1 lo1) 1e-9) (<= (- hi2 lo2) 1e-9))
    (progn
      (prompt "\nThe two edges do not overlap along the pillar direction - nothing to draw.")
      (exit)))

  (setq xsl (VID:Br-Layout lo1 hi1 n wl *VIDBR-LJUS*
                           (cond ((VID:Br-Gap *VIDBR-LGAP*)) (T 0.0)) "LHS box")
        xsr (VID:Br-Layout lo2 hi2 n wr *VIDBR-RJUS*
                           (cond ((VID:Br-Gap *VIDBR-RGAP*)) (T 0.0)) "RHS box"))
  (if (not (and xsl xsr))
    (progn (prompt "\nNothing drawn.") (exit)))

  (setq nc (VID:Br-Connect el er xsl wl xsr wr pv nv))

  ;; leader: perpendicular to the track, on the chosen side; the text runs
  ;; perpendicular to the track too (never upside down)
  (setq side (if (= *VIDBR-SIDE* 0) 1.0 -1.0)                 ; LHS = +90, RHS = -90
        ang  (VID:Br-Norm (+ (angle '(0.0 0.0) tdir) (* side (/ pi 2.0))))
        flip (and (> ang (+ (/ pi 2.0) 1e-4)) (<= ang (+ (* 1.5 pi) 1e-4))))
  (VID:Br-Leader doc cpt *VIDBR-TXT* hgt ang flip)

  (prompt (strcat "\nDone. Pillars: " (itoa nc) ", leader: " *VIDBR-TXT*))
)

;; ---- command ----------------------------------------------------------------
(defun c:VIDBRIDGE (/ *error* doc)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (vl-catch-all-apply 'vla-EndUndoMark (list doc))
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (prompt (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; boxes and track belong to one run of the command
  (setq *VIDBR-LBOX* nil *VIDBR-LBOXEN* nil
        *VIDBR-RBOX* nil *VIDBR-RBOXEN* nil
        *VIDBR-TRK*  nil)

  (vla-StartUndoMark doc)
  (if (VID:Br-Dialog)
    (VID:Br-Build doc)
    (prompt "\nClosed. Any boxes already drawn are kept (U removes them)."))
  (vla-EndUndoMark doc)
  (princ)
)

(prompt "\nvidBridge loaded. Type VIDBRIDGE to run.")
(princ)
