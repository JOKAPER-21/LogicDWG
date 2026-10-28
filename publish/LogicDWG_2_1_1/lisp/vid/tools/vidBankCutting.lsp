;; ============================================
;; BANK CUTTING TOOL v25
;; Blocks are imported from vid_blocks_tools.dwg
;; (support/vid/tools - same folder as vidGradient.lsp)
;;   bank_cutting_up / bank_cutting_dn
;;   bank_cutting_up_lg / bank_cutting_dn_lg
;; ============================================

(vl-load-com)

(setq *BC-BLOCK-NAMES*
  '("bank_cutting_up" "bank_cutting_dn" "bank_cutting_up_lg" "bank_cutting_dn_lg"))

(setq *BC-REG-KEY* "HKEY_CURRENT_USER\\Software\\VID\\Tools")

(defun BC-clean (txt / res i ch)
  (setq res "" i 1)
  (while (<= i (strlen txt))
    (setq ch (substr txt i 1))
    (if (or (and (>= ch "0") (<= ch "9"))
            (= ch ".")
            (= ch "-"))
      (setq res (strcat res ch))
    )
    (setq i (1+ i))
  )
  res
)

;; Locate vid_blocks_tools.dwg
;;  1. path remembered from the last run (session + registry)
;;  2. same folder as vidGradient.lsp (found through the support path)
;;  3. support/vid/tools relative to the support path
;;  4. ask the user once, then remember the path
(defun BC-find-dwg (/ hit f dir n rel)
  (if (and *VID-BLOCKS-DWG* (findfile *VID-BLOCKS-DWG*))
    (setq hit (findfile *VID-BLOCKS-DWG*))
  )
  (if (not hit)
    (progn
      (setq f (vl-registry-read *BC-REG-KEY* "BlocksDwg"))
      (if (and f (= (type f) 'STR) (findfile f))
        (setq hit (findfile f))
      )
    )
  )
  (if (not hit)
    (foreach n '("vidGradient.lsp" "vidGradientAlign.lsp" "vidBankCutting.lsp")
      (if (and (not hit) (setq f (findfile n)))
        (progn
          (setq dir (vl-filename-directory f)
                f   (findfile (strcat dir "/vid_blocks_tools.dwg")))
          (if f (setq hit f))
        )
      )
    )
  )
  (if (not hit)
    (foreach rel '("support/vid/tools/vid_blocks_tools.dwg"
                   "vid/tools/vid_blocks_tools.dwg"
                   "vid_blocks_tools.dwg")
      (if (and (not hit) (setq f (findfile rel)))
        (setq hit f)
      )
    )
  )
  (if (not hit)
    (setq hit (getfiled "Locate vid_blocks_tools.dwg" "" "dwg" 0))
  )
  (if hit
    (progn
      (setq *VID-BLOCKS-DWG* hit)
      (vl-catch-all-apply 'vl-registry-write
        (list *BC-REG-KEY* "BlocksDwg" hit))
    )
  )
  hit
)

;; Import any of <names> that are missing from the current drawing
(defun BC-import-blocks (dwg names / acad doc ver dbx need blks item objs n path)
  (setq need (vl-remove-if '(lambda (x) (tblsearch "BLOCK" x)) names))
  (if need
    (progn
      (setq acad (vlax-get-acad-object)
            doc  (vla-get-ActiveDocument acad)
            ver  (itoa (atoi (getvar "ACADVER")))
            path (vl-string-translate "/" "\\" dwg))
      (setq dbx (vl-catch-all-apply 'vla-GetInterfaceObject
                  (list acad (strcat "ObjectDBX.AxDbDocument." ver))))
      (if (vl-catch-all-error-p dbx)
        (setq dbx (vl-catch-all-apply 'vla-GetInterfaceObject
                    (list acad "ObjectDBX.AxDbDocument"))))
      (cond
        ((vl-catch-all-error-p dbx)
         (prompt "\nCannot start ObjectDBX - blocks not imported.")
         (setq dbx nil))
        ((vl-catch-all-error-p
           (vl-catch-all-apply 'vla-Open (list dbx path)))
         (prompt (strcat "\nCannot open " path))
         (vl-catch-all-apply 'vlax-release-object (list dbx))
         (setq dbx nil))
        (T
         (setq blks (vla-get-Blocks dbx))
         (foreach n need
           (setq item (vl-catch-all-apply 'vla-Item (list blks n)))
           (if (vl-catch-all-error-p item)
             (prompt (strcat "\nBlock not found in DWG: " n))
             (progn
               (setq objs (vlax-make-safearray vlax-vbObject '(0 . 0)))
               (vlax-safearray-put-element objs 0 item)
               (if (vl-catch-all-error-p
                     (vl-catch-all-apply 'vla-CopyObjects
                       (list dbx objs (vla-get-Blocks doc))))
                 (prompt (strcat "\nCould not import block: " n))
                 (prompt (strcat "\nImported block: " n))
               )
             )
           )
         )
         (vl-catch-all-apply 'vlax-release-object (list dbx))
        )
      )
    )
  )
  (princ)
)

(defun C:vidBankCutting ( / e1 e2 t1 t2 n1 n2 diff h rev
                 pt pl foot dist dir ang blk dwg e d nxt mark pick-pt
                 ucsang oldos oldecho *error*)

  (defun *error* (msg)
    (if oldos   (setvar "osmode" oldos))
    (if oldecho (setvar "cmdecho" oldecho))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (prompt (strcat "\nError: " msg)))
    (princ)
  )

  (prompt "\n--- BC TOOL v25 ---")

  ;; 0. IMPORT BLOCKS FROM vid_blocks_tools.dwg
  (setq dwg (BC-find-dwg))
  (if (not dwg)
    (progn (alert "vid_blocks_tools.dwg not found!") (exit))
  )
  (BC-import-blocks dwg *BC-BLOCK-NAMES*)

  ;; 1. SELECT TEXTS
  (setq e1 (car (entsel "\nSelect FT or BE value ")))
  (setq e2 (car (entsel "\nSelect TOE or GL value ")))
  (if (or (null e1) (null e2))
    (progn (alert "Text not selected!") (exit))
  )

  ;; GET VALUES
  (setq t1 (cdr (assoc 1 (entget e1))))
  (setq t2 (cdr (assoc 1 (entget e2))))
  (if (not (and (= (type t1) 'STR) (= (type t2) 'STR)))
    (progn (alert "Please select TEXT or MTEXT objects!") (exit))
  )
  (setq n1 (atof (BC-clean t1)))
  (setq n2 (atof (BC-clean t2)))

  (if (or (= n1 0.0) (= n2 0.0))
    (progn
      (alert "Text value read agala! Check text format!")
      (exit)
    )
  )

  ;; HEIGHT DIFF
  (setq diff (- n1 n2))
  (cond
    ((and (>= diff -0.3) (<= diff 0.3))
      (setq h "LG" rev nil)
    )
    ((< diff -0.3)
      (setq h (rtos (abs diff) 2 2) rev T)
    )
    (T
      (setq h (rtos diff 2 2) rev nil)
    )
  )

  ;; INSERT POINT = e2 text assoc 10
  (setq pt (cdr (assoc 10 (entget e2))))

  ;; 2. USER CLICKED POINT for distance measurement
  (setq pick-pt (getpoint "\nPick point for distance measurement: "))

  ;; 3. SELECT POLYLINE
  (setq pl (car (entsel "\nSelect Track: ")))
  (if (or (null pick-pt) (null pl))
    (progn (alert "Point / Track not selected!") (exit))
  )

  ;; PERPENDICULAR FOOT from picked point to polyline (force Z=0)
  (setq foot (vl-catch-all-apply 'vlax-curve-getClosestPointTo
               (list pl (list (car pick-pt) (cadr pick-pt) 0.0))))
  (if (or (vl-catch-all-error-p foot) (null foot))
    (progn (alert "Track must be a polyline / line / arc!") (exit))
  )

  ;; 2D DISTANCE ONLY (XY - ignore Z)
  (setq dist (rtos (distance
                     (list (car pick-pt) (cadr pick-pt))
                     (list (car foot)    (cadr foot)))
                   2 2))

  ;; 4. DIRECTION
  (initget "UP DN")
  (setq dir (getkword "\nDirection [UP/DN]: "))
  (if (null dir)
    (progn (alert "Direction not selected!") (exit))
  )
  (if rev
    (setq dir (if (= dir "UP") "DN" "UP"))
  )

  ;; BLOCK NAME
  (setq blk
    (cond
      ((and (= dir "UP") (not (= h "LG"))) "bank_cutting_up")
      ((and (= dir "DN") (not (= h "LG"))) "bank_cutting_dn")
      ((and (= dir "UP") (= h "LG"))       "bank_cutting_up_lg")
      ((and (= dir "DN") (= h "LG"))       "bank_cutting_dn_lg")
    )
  )

  ;; BLOCK MUST EXIST (imported from vid_blocks_tools.dwg)
  (if (not (tblsearch "BLOCK" blk))
    (progn
      (alert (strcat "Block " blk " not found in vid_blocks_tools.dwg!"))
      (exit)
    )
  )

  ;; ANGLE - pt to foot = perpendicular direction to polyline (WCS)
  (setq ang (+ (angle pt foot) (/ pi 2)))
  (if (= dir "DN") (setq ang (+ ang pi)))
  (if rev          (setq ang (+ ang pi)))

  ;; WCS angle -> current UCS angle (so it also works when UCS is not World)
  (setq ucsang (angle '(0.0 0.0 0.0) (trans '(1.0 0.0 0.0) 1 0 T)))

  (setq oldos   (getvar "osmode")
        oldecho (getvar "cmdecho"))
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)

  ;; INSERT BLOCK at e2 point (by name - already imported)
  (setq mark (entlast))
  (command "-insert" blk (trans pt 0 1) 1 1
           (* 180.0 (/ (- ang ucsang) pi)))

  ;; EXPLODE & UPDATE TEXT (only the entities created by this explode)
  (command "explode" (entlast))
  (setq e (if mark (entnext mark) (entnext)))
  (while e
    (setq nxt (entnext e)
          d   (entget e))
    (if (and d (member (cdr (assoc 0 d)) '("TEXT" "MTEXT")))
      (cond
        ((= (cdr (assoc 1 d)) "H-0.00")
          (entmod (subst (cons 1 (if (= h "LG") "H-LG" (strcat "H-" h)))
                         (assoc 1 d) d))
        )
        ((= (cdr (assoc 1 d)) "D-0.00")
          (if (= h "LG")
            (entdel e)
            (entmod (subst (cons 1 (strcat "D-" dist))
                           (assoc 1 d) d))
          )
        )
      )
    )
    (setq e nxt)
  )

  (setvar "osmode" oldos)
  (setvar "cmdecho" oldecho)

  (prompt "\nDone macha!")
  (princ)
)

(prompt "\nvidBankCutting loaded. Type vidBankCutting to run.")
(princ)
