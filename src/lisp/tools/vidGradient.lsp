;;; ============================================================================
;;; Vid Gradient
;;; Command : VIDGRADIENT
;;; Version : v3   (AutoCAD and AutoCAD Civil 3D - core AutoLISP / ActiveX only)
;;;
;;; Inserts the dynamic block G_NEW_FINAL beside a rail track at every G_POSITION
;;; listed in a CSV file.
;;;
;;; BLOCK : G_NEW_FINAL
;;; FILE  : vidGradient_block.dwg  (same folder as vidGradient.lsp, normally the
;;;         LogicDWG "tools" folder).  If the drawing does not contain the block
;;;         yet, it is imported from that file automatically (ObjectDBX).
;;;
;;; CSV  (first row = header; columns are found BY NAME, any order):
;;;   G_POSITION, G_CHAINAGE, G_RAILLEVEL, G_LINE, G_TAIL, G_HEAD, G_NOTE
;;;   0,KM:72/112.329,RL:85.996,(DN MAIN),LEVEL,R 132,FOR EXIST - ALL LINES
;;;   (see vidGradient_template.csv)
;;;   G_POSITION = distance along the selected track (drawing units, from its
;;;                start point) where the block is placed.  G_CHAINAGE is only
;;;                written into the block attribute - it is NOT used to place it.
;;;   Every other column is written to the block attribute with the same tag.
;;;
;;; Prompts : select rail track -> gradient height (offset from the track,
;;;           default 80) -> CSV file.
;;;
;;; Rotations: G_TAIL / G_HEAD text starting with R -> 21 deg, F -> 339 deg,
;;;            LEVEL (or anything else) -> 0 deg.  Set on the dynamic properties
;;;            G_TAIL_ROT and G_HEAD_ROT.
;;; ============================================================================

(vl-load-com)

(setq VGR-BlockName    "G_NEW_FINAL")
(setq VGR-BlockFile    "vidGradient_block.dwg")
(setq VGR-DefaultOff   80.0)
(setq VGR-RotRise      21.0)     ; degrees when the text starts with R
(setq VGR-RotFall      339.0)    ; degrees when the text starts with F

(setq VGR-OldError nil)
(setq VGR-Doc      nil)
(setq VGR-NoNested  nil)   ; T when the DWG has no block named G_NEW_FINAL inside

;; ---------------------------------------------------------------------------
;; Small helpers
;; ---------------------------------------------------------------------------
(defun VGR-Trim (s)
  (if s (vl-string-trim " \t\r\n" s) "")
)

;; Quote-aware CSV line split
(defun VGR-ParseCSV (s / i n ch inq cur fields)
  (setq i 1 n (strlen s) inq nil cur "" fields '())
  (while (<= i n)
    (setq ch (substr s i 1))
    (cond
      ((and (= ch "\"") inq (< i n) (= (substr s (1+ i) 1) "\""))
       (setq cur (strcat cur "\"") i (+ i 2)))
      ((= ch "\"")
       (setq inq (not inq) i (1+ i)))
      ((and (= ch ",") (not inq))
       (setq fields (cons (VGR-Trim cur) fields) cur "" i (1+ i)))
      (T (setq cur (strcat cur ch) i (1+ i)))
    )
  )
  (reverse (cons (VGR-Trim cur) fields))
)

(defun VGR-Num (txt)
  (if (and txt (/= (VGR-Trim txt) ""))
    (distof (VGR-Trim txt) 2)
    nil
  )
)

;; index of a header name in the header list, or nil
(defun VGR-ColIndex (hdr name / i found)
  (setq i 0 found nil)
  (foreach h hdr
    (if (and (null found) (= h (strcase name))) (setq found i))
    (setq i (1+ i))
  )
  found
)

;; value of column "name" in a row ("" when missing)
(defun VGR-Field (hdr row name / i)
  (setq i (VGR-ColIndex hdr name))
  (if (and i (nth i row)) (nth i row) "")
)

;; degrees for a G_TAIL / G_HEAD value
(defun VGR-RotDeg (txt / u)
  (setq u (strcase (VGR-Trim txt)))
  (cond
    ((= u "")              0.0)
    ((wcmatch u "LEVEL*")  0.0)
    ((wcmatch u "R*")      VGR-RotRise)
    ((wcmatch u "F*")      VGR-RotFall)
    (T                     0.0)
  )
)

(defun VGR-Error (msg)
  (if VGR-Doc (vl-catch-all-apply 'vla-EndUndoMark (list VGR-Doc)))
  (setq *error* VGR-OldError)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
    (princ (strcat "\nVIDGRADIENT ERROR: " msg))
    (princ "\nVIDGRADIENT cancelled.")
  )
  (princ)
)

;; ---------------------------------------------------------------------------
;; Block file + import
;; ---------------------------------------------------------------------------
;; Full path of vidGradient_block.dwg, or nil.  (cond - "or" returns only T/nil)
(defun VGR-FindBlockFile (/ p)
  (cond
    ((and (boundp 'LogicDWG:ToolsDir) LogicDWG:ToolsDir
          (setq p (findfile (strcat LogicDWG:ToolsDir "\\" VGR-BlockFile))))
     p)
    ((setq p (findfile VGR-BlockFile)) p)
    ((setq p (findfile (strcat "tools/" VGR-BlockFile))) p)
    ((setq p (findfile (strcat "vid/tools/" VGR-BlockFile))) p)
    ((and (setq p (findfile "vidGradient.lsp"))
          (setq p (findfile (strcat (vl-filename-directory p) "\\" VGR-BlockFile))))
     p)
    (T nil)
  )
)

;; Copy block definition VGR-BlockName from a DWG into the active drawing.
(defun VGR-ImportFromDwg (file / acad doc dbx ver ids prog blk arr res ok)
  (setq acad (vlax-get-acad-object)
        doc  (vla-get-ActiveDocument acad)
        ver  (substr (getvar "ACADVER") 1 2)
        dbx  nil ok nil)
  (setq VGR-NoNested nil)
  ;; ObjectDBX document (versioned id first, then the generic one)
  (foreach prog (list (strcat "ObjectDBX.AxDbDocument." ver) "ObjectDBX.AxDbDocument")
    (if (null dbx)
      (progn
        (setq dbx (vl-catch-all-apply 'vla-GetInterfaceObject (list acad prog)))
        (if (vl-catch-all-error-p dbx) (setq dbx nil))
      )
    )
  )
  (if (null dbx)
    (progn (princ "\nObjectDBX is not available in this AutoCAD.") nil)
    (progn
      (setq res (vl-catch-all-apply 'vla-Open (list dbx file)))
      (if (vl-catch-all-error-p res)
        (princ (strcat "\nCould not open " file))
        (progn
          (setq blk (vl-catch-all-apply 'vla-Item (list (vla-get-Blocks dbx) VGR-BlockName)))
          (if (vl-catch-all-error-p blk)
            (progn
              (setq VGR-NoNested T)
              (princ (strcat "\nBlock " VGR-BlockName " is not inside " VGR-BlockFile "."))
            )
            (progn
              (setq arr (vlax-make-safearray vlax-vbObject '(0 . 0)))
              (vlax-safearray-put-element arr 0 blk)
              ;; try 3-argument form, then the form with an IdPairs variable
              (setq res (vl-catch-all-apply 'vla-CopyObjects
                          (list dbx (vlax-make-variant arr) (vla-get-Blocks doc))))
              (if (vl-catch-all-error-p res)
                (setq res (vl-catch-all-apply 'vla-CopyObjects
                            (list dbx arr (vla-get-Blocks doc) 'ids))))
              (setq ok (and (not (vl-catch-all-error-p res))
                            (tblsearch "BLOCK" VGR-BlockName)))
            )
          )
        )
      )
      (vl-catch-all-apply 'vlax-release-object (list dbx))
      ok
    )
  )
)

;; Fallback: the DWG itself is the block drawing (no nested G_NEW_FINAL inside).
;; "-INSERT name=file" defines the block; a bare (command) cancels the insert.
(defun VGR-DefineFromWholeDwg (file / old)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (vl-catch-all-apply 'command (list "_.-INSERT" (strcat VGR-BlockName "=" file)))
  (command)
  (command)
  (setvar "CMDECHO" old)
  (tblsearch "BLOCK" VGR-BlockName)
)

;; T when the block is available in the drawing (imports it when missing)
(defun VGR-EnsureBlock (/ file)
  (cond
    ((tblsearch "BLOCK" VGR-BlockName) T)
    ((null (setq file (VGR-FindBlockFile)))
     (princ (strcat "\n" VGR-BlockFile " not found - keep it in the same folder as vidGradient.lsp."))
     nil)
    (T
     (princ (strcat "\nImporting block " VGR-BlockName " from " file " ..."))
     (cond
       ((VGR-ImportFromDwg file) (princ " done.") T)
       ((and VGR-NoNested (VGR-DefineFromWholeDwg file))
        (princ " done (whole drawing used as the block).") T)
       (T (princ (strcat "\nBlock " VGR-BlockName " could not be loaded.")) nil)
     )
    )
  )
)

;; ---------------------------------------------------------------------------
;; Dynamic block property (angle in degrees -> radians)
;; ---------------------------------------------------------------------------
(defun VGR-SetDynAngle (blk propName deg / props p found)
  (setq found nil)
  (if (= (vla-get-IsDynamicBlock blk) :vlax-true)
    (progn
      (setq props (vlax-invoke blk 'GetDynamicBlockProperties))
      (foreach p props
        (if (= (strcase (vla-get-PropertyName p)) (strcase propName))
          (progn
            (vla-put-Value p (vlax-make-variant (* deg (/ pi 180.0)) vlax-vbDouble))
            (setq found T)
          )
        )
      )
    )
  )
  found
)

(defun VGR-ListDynProps (blk / props p out)
  (setq out "")
  (if (= (vla-get-IsDynamicBlock blk) :vlax-true)
    (foreach p (vlax-invoke blk 'GetDynamicBlockProperties)
      (setq out (strcat out (if (= out "") "" ", ") (vla-get-PropertyName p)))
    )
    (setq out "(block is not dynamic)")
  )
  out
)

;; ---------------------------------------------------------------------------
;; CSV -> (header rows).  header = upper-case column names, rows = field lists.
;; Rows whose G_POSITION is not a number are skipped.
;; ---------------------------------------------------------------------------
(defun VGR-ReadCsv (file / fp line f hdr n rows bad first idxPos head tail k)
  (setq rows '() bad 0 first T hdr nil)
  (if (setq fp (open file "r"))
    (progn
      (while (setq line (read-line fp))
        (setq line (VGR-Trim line))
        ;; strip UTF-8 BOM
        (if (and first (>= (strlen line) 3) (= (ascii (substr line 1 1)) 239))
          (setq line (substr line 4)))
        (if (/= line "")
          (progn
            (setq f (VGR-ParseCSV line))
            (cond
              (first
               (setq hdr (mapcar 'strcase f))
               (setq n   (length hdr))
               (setq idxPos (VGR-ColIndex hdr "G_POSITION")))
              (T
               ;; unquoted commas inside the last column: merge the extras
               (if (> (length f) n)
                 (progn
                   (setq head '() k 0)
                   (foreach x f
                     (if (< k (1- n)) (setq head (append head (list x))))
                     (setq k (1+ k)))
                   (setq tail (apply 'strcat
                                (cons (nth (1- n) f)
                                      (mapcar '(lambda (x) (strcat "," x))
                                              (nthcdr n f)))))
                   (setq f (append head (list tail)))))
               (if (and idxPos (VGR-Num (nth idxPos f)))
                 (setq rows (cons f rows))
                 (setq bad (1+ bad))))
            )
            (setq first nil)
          )
        )
      )
      (close fp)
      (list hdr (reverse rows) bad)
    )
    nil
  )
)

;; ---------------------------------------------------------------------------
;; Command
;; ---------------------------------------------------------------------------
(defun c:VIDGRADIENT
  (/ pl pl-len off file csv hdr rows bad row idxPos dist
     param deriv ang pt ptIns obj att tag col txt
     mspace tailTxt headTxt okCount before after noprop sel)

  (vl-load-com)
  (setq VGR-OldError *error*)
  (setq *error* VGR-Error)
  (setq VGR-Doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace  (vla-get-ModelSpace VGR-Doc))

  (cond
    ;; 1. block -----------------------------------------------------------
    ((not (VGR-EnsureBlock)) nil)

    ;; 2. rail track ------------------------------------------------------
    ((null (setq sel (entsel "\\nSelect Rail Track: "))) nil)
    ((vl-catch-all-error-p
       (vl-catch-all-apply 'vlax-curve-getEndParam (list (car sel))))
     (princ "\\nThat object is not a polyline / curve."))

    (T
     (setq pl (car sel))
     (setq pl-len (vlax-curve-getDistAtParam pl (vlax-curve-getEndParam pl)))
     (princ (strcat "\\nTrack length: " (rtos pl-len 2 3)))

     ;; 3. gradient height --------------------------------------------------
     (initget 2)
     (setq off (getreal (strcat "\\nEnter Gradient Height <"
                                (rtos VGR-DefaultOff 2 0) ">: ")))
     (if (null off) (setq off VGR-DefaultOff))

     ;; 4. CSV --------------------------------------------------------------
     (setq file (getfiled "Select CSV File" "" "csv" 0))
     (cond
       ((null file) (princ "\\nNo CSV selected."))
       ((null (setq csv (VGR-ReadCsv file)))
        (princ "\\nCould not open the CSV file."))
       ((null (setq idxPos (VGR-ColIndex (car csv) "G_POSITION")))
        (princ "\\nThe CSV has no G_POSITION column (see vidGradient_template.csv)."))
       (T
        (setq hdr  (car csv)
              rows (cadr csv)
              bad  (caddr csv))
        (if (> bad 0)
          (princ (strcat "\\n" (itoa bad) " CSV row(s) skipped (G_POSITION is not a number).")))

        (vla-StartUndoMark VGR-Doc)
        (setq okCount 0 before 0 after 0 noprop nil)

        (foreach row rows
          (setq dist (VGR-Num (nth idxPos row)))
          (cond
            ((< dist -0.0005) (setq before (1+ before)))
            ((> dist (+ pl-len 0.0005)) (setq after (1+ after)))
            (T
             (setq dist (max 0.0 (min dist pl-len)))
             ;; tangent from the first derivative (safe at the end point too)
             (setq pt    (vlax-curve-getPointAtDist pl dist))
             (setq param (vlax-curve-getParamAtDist pl dist))
             (setq deriv (if param (vlax-curve-getFirstDeriv pl param) nil))
             (setq ang
               (if (and deriv (or (> (abs (car deriv)) 1e-12)
                                  (> (abs (cadr deriv)) 1e-12)))
                 (atan (cadr deriv) (car deriv))
                 0.0))
             ;; right-hand side of the travel direction
             (setq ptIns (polar pt (- ang (/ pi 2.0)) off))

             (setq obj
               (vla-InsertBlock mspace (vlax-3d-point ptIns)
                                VGR-BlockName 1.0 1.0 1.0 ang))

             ;; attributes: tag = CSV column name (G_POSITION is not written)
             (setq tailTxt (VGR-Field hdr row "G_TAIL")
                   headTxt (VGR-Field hdr row "G_HEAD"))
             (if (= (vla-get-HasAttributes obj) :vlax-true)
               (foreach att (vlax-invoke obj 'GetAttributes)
                 (setq tag (strcase (vla-get-TagString att)))
                 (if (and (/= tag "G_POSITION") (VGR-ColIndex hdr tag))
                   (vla-put-TextString att (VGR-Field hdr row tag))
                 )
               )
             )

             ;; dynamic rotations
             (if (not (VGR-SetDynAngle obj "G_TAIL_ROT" (VGR-RotDeg tailTxt)))
               (setq noprop (cons "G_TAIL_ROT" noprop)))
             (if (not (VGR-SetDynAngle obj "G_HEAD_ROT" (VGR-RotDeg headTxt)))
               (setq noprop (cons "G_HEAD_ROT" noprop)))

             (vla-Update obj)
             (setq okCount (1+ okCount))
            )
          )
        )
        (vla-EndUndoMark VGR-Doc)

        (princ "\\n------------------------------------------------")
        (princ (strcat "\\nBlocks inserted : " (itoa okCount)))
        (if (> before 0)
          (princ (strcat "\\nNegative G_POSITION : " (itoa before) " row(s) skipped")))
        (if (> after 0)
          (princ (strcat "\\nBeyond the track length (" (rtos pl-len 2 3) ") : "
                         (itoa after) " row(s) skipped")))
        (if (= okCount 0)
          (princ "\\nNothing inserted - check G_POSITION values against the track length."))
        (if noprop
          (progn
            (princ (strcat "\\nWARNING: dynamic property not found in the block: "
                           (car noprop)))
            (princ (strcat "\\n  Properties available: " (VGR-ListDynProps obj)))
          )
        )
       )
     )
    )
  )
  (setq *error* VGR-OldError)
  (princ)
)

(princ "\nVIDGRADIENT loaded. Type VIDGRADIENT to run.")
(princ)
