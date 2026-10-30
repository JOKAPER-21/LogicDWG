;;; ======================================================================
;;; vidGradient.lsp
;;; Command : VIDGRADIENT
;;; Block   : name asked at run time, default vid_gradient_001
;;;           (imported from vid_blocks_tools.dwg, searched in the same
;;;           folder as this LSP, then the vid/tools support folders)
;;; CSV     : vid_ch,rw_ch,railLevel,lineType,before,current,note
;;; ======================================================================

;; ---- locate this LSP's folder at load time (used to find the DWG) ----
(setq *vg-blk-name* "vid_gradient_001")   ;; default block name
(setq *vg-tried* nil)
(setq *vg-dwg-name* "vid_blocks_tools.dwg")

;; ----------------------------------------------------------------------
;; SAFE CSV SPLIT
;; ----------------------------------------------------------------------
(defun vg:csv-split (str delimiter / pos result temp)
  (setq result '()
        temp   str)
  (while (setq pos (vl-string-search delimiter temp))
    (setq result (append result
                         (list (vl-string-trim " \r\n" (substr temp 1 pos)))))
    (setq temp (substr temp (+ pos (strlen delimiter) 1))))
  (append result (list (vl-string-trim " \r\n" temp)))
)

;; ----------------------------------------------------------------------
;; NTHCDR (not available in AutoLISP)
;; ----------------------------------------------------------------------
(defun vg:nthcdr (n lst)
  (repeat n (setq lst (cdr lst)))
  lst
)

;; ----------------------------------------------------------------------
;; KM TO METERS   "KM:72/112.329" -> 72112.329
;; ----------------------------------------------------------------------
(defun vg:km-to-meters (km-str / parts i)
  (setq i 1)
  (while (and (<= i (strlen km-str))
              (not (wcmatch (substr km-str i 1) "#")))
    (setq i (1+ i)))
  (setq km-str (substr km-str i))
  (setq parts (vg:csv-split km-str "/"))
  (if (= (length parts) 2)
    (+ (* (atof (car parts)) 1000.0) (atof (cadr parts)))
    (atof km-str))
)

;; ----------------------------------------------------------------------
;; CURVE LENGTH
;; ----------------------------------------------------------------------
(defun vg:curve-length (ent)
  (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent))
)

;; ----------------------------------------------------------------------
;; SET DYNAMIC PROPERTY (raw value, no unit conversion). Returns T if found
;; ----------------------------------------------------------------------
(defun vg:set-dyn (blk propName value / found p)
  (setq found nil)
  (if (= (vla-get-IsDynamicBlock blk) :vlax-true)
    (foreach p (vlax-invoke blk 'GetDynamicBlockProperties)
      (if (= (strcase (vla-get-PropertyName p)) (strcase propName))
        (progn
          (vla-put-Value p (vlax-make-variant value vlax-vbDouble))
          (setq found T)))))
  found
)

;; ----------------------------------------------------------------------
;; ROTATION FROM "LEVEL" / "R nnn" / "F nnn"  ->  degrees
;; ----------------------------------------------------------------------
(defun vg:grad-rot (val / v)
  (if (and val (/= val ""))
    (progn
      (setq v (strcase (vl-string-trim " " val)))
      (cond
        ((vl-string-search "LEVEL" v) 0.0)
        ((= (substr v 1 1) "R") 21.0)
        ((= (substr v 1 1) "F") 339.0)
        (T 0.0)))
    0.0)
)

;; ----------------------------------------------------------------------
;; KNOWN INSTALL FOLDER: <Support>\vid\tools  (Civil 3D 2026 roaming Support)
;; ----------------------------------------------------------------------
(defun vg:vid-dirs (/ res ap d)
  (setq res "")
  (if (setq ap (getenv "APPDATA"))
    (setq res (strcat ap "/Autodesk/C3D 2026/enu/Support/vid/tools")))
  (if ap
    (foreach d (vl-directory-files (strcat ap "/Autodesk") "*" -1)
      (if (and (/= d ".") (/= d ".."))
        (setq res (strcat res ";" ap "/Autodesk/" d "/enu/Support/vid/tools")))))
  (foreach d (vg:csv-split (getvar "ACADPREFIX") ";")
    (if (/= d "")
      (setq res (strcat res ";" d "/vid/tools"))))
  res
)

;; ----------------------------------------------------------------------
;; FIND A FILE: support path, then Trusted Paths, then drawing folder
;; ----------------------------------------------------------------------
(defun vg:search-file (fname / f dirs d res)
  (setq res nil)
  (setq *vg-tried* (cons (strcat "findfile: " fname) *vg-tried*))
  (cond
    ((setq f (findfile fname)) (setq res f))
    (T
     (setq dirs (vg:csv-split (strcat (vg:vid-dirs) ";" (getvar "TRUSTEDPATHS") ";" (getvar "DWGPREFIX")) ";"))
     (foreach d dirs
       (if (and (not res) (/= d ""))
         (progn
           (while (and (> (strlen d) 0) (member (substr d (strlen d)) (list "." "\\" "/")))
                  (setq d (substr d 1 (1- (strlen d)))))
           (setq *vg-tried* (cons (strcat d "/" fname) *vg-tried*))
           (if (and (/= d "") (setq f (findfile (strcat d "/" fname))))
             (setq res f)))))))
  res
)

;; remember the LSP folder as soon as the file is loaded
(if (setq *vg-tmp* (vg:search-file "vidGradient.lsp"))
  (setq *vg-dir* (vl-filename-directory *vg-tmp*))
)
(setq *vg-tmp* nil)

;; ----------------------------------------------------------------------
;; FIND THE BLOCK DWG (same folder as this LSP) - no user prompt
;; ----------------------------------------------------------------------
(defun vg:find-dwg (/ f)
  (cond
    ((and *vg-dir* (setq f (findfile (strcat *vg-dir* "/" *vg-dwg-name*)))) f)
    ((and (not *vg-dir*)
          (setq f (vg:search-file "vidGradient.lsp"))
          (setq *vg-dir* (vl-filename-directory f))
          (setq f (findfile (strcat *vg-dir* "/" *vg-dwg-name*))))
     f)
    ((setq f (vg:search-file *vg-dwg-name*)) f)
    (T nil))
)

;; ----------------------------------------------------------------------
;; IMPORT BLOCK FROM DWG - overrides the existing definition
;;
;; The DWG is first inserted under a TEMP name. This avoids the
;; "Block the entered block name references itself" error, which happens when the DWG's
;; model space already contains an insert of the entered block name.
;;   - If the DWG carries a the entered block name definition, that one is used.
;;   - If the DWG just contains geometry, the temp block is renamed the entered block name.
;; ----------------------------------------------------------------------
(defun vg:import-block (blk dwg / tmp old existed old-attreq old-cmdecho last-ent r)
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

;; ----------------------------------------------------------------------
;; MAIN COMMAND
;; ----------------------------------------------------------------------
(defun c:VIDGRADIENT (/ blk-name hdr delim bad dwg pl pl-length perp-offset file fp line
                        csv-data km-list offset-dist
                        doc mspace pt1 pt2 tang-angle pt-final obj atts att tag
                        tail-value head-value km-data g-pos pos-done count skipped d)

  (vl-load-com)

  (setq doc    (vla-get-ActiveDocument (vlax-get-acad-object))
        mspace (vla-get-ModelSpace doc))

  ;; ---- 1. inputs (in this order) ----
  (if (setq pl (car (entsel "\nSelect Rail Track: ")))
    (progn
      (setq pl-length (vg:curve-length pl))
      (prompt (strcat "\nPolyline Length: " (rtos pl-length 2 3) " m"))

      (if (setq file (getfiled "Select CSV file: "
                               (cond (*vg-last-csv* *vg-last-csv*)          ;; folder of last CSV
                                     (*vg-dir* (strcat *vg-dir* "/"))       ;; else the LSP folder
                                     (T ""))
                               "csv" 0))
        (progn
          ;; remember this folder for next time
          (setq *vg-last-csv*
                (vl-filename-directory (vl-string-translate "\\" "/" file)))
          (if (and *vg-last-csv* (/= *vg-last-csv* ""))
            (setq *vg-last-csv* (strcat *vg-last-csv* "/")))

          (setq blk-name (getstring T (strcat "\nEnter Gradient Block name <" *vg-blk-name* ">: ")))
          (if (= (vl-string-trim " " blk-name) "") (setq blk-name *vg-blk-name*))
          (setq blk-name (vl-string-trim " " blk-name))

          (setq perp-offset (getreal "\nEnter Gradient Height <80>: "))
          (if (not perp-offset) (setq perp-offset 80.0))

          ;; ---- 2. import block from the DWG (override if it exists) ----
          (if (not (setq dwg (vg:find-dwg)))
            (progn
              (prompt (strcat "\n" *vg-dwg-name* " not found. Locations checked:"))
              (foreach d (reverse *vg-tried*) (prompt (strcat "\n  " d)))
              (prompt (strcat "\nLSP folder detected: " (if *vg-dir* *vg-dir* "none")))
              (setq *vg-tried* nil)
              (princ))
            (if (not (vg:import-block blk-name dwg))
              (progn (prompt (strcat "\nFailed to import block " blk-name " from " dwg)) (princ))
              (progn
                (prompt (strcat "\nBlock " blk-name " imported from " dwg))
                (progn
                  (progn
                    ;; ---- 3. read CSV ----
                    ;; vid_ch,rw_ch,railLevel,lineType,before,current,note
                    (setq fp (open file "r"))
                    (setq hdr (read-line fp)) ;; header: also used to detect the delimiter
                    (setq delim (if (and hdr (vl-string-search "\t" hdr)) "\t" ","))
                    (setq km-list '() bad 0)
                    (while (setq line (read-line fp))
                      (setq csv-data (vg:csv-split line delim))
                      ;; notes may contain commas -> merge everything from field 7 on
                      (if (> (length csv-data) 7)
                        (setq csv-data
                              (append (list (nth 0 csv-data) (nth 1 csv-data) (nth 2 csv-data)
                                            (nth 3 csv-data) (nth 4 csv-data) (nth 5 csv-data))
                                      (list (apply 'strcat
                                              (cons (nth 6 csv-data)
                                                    (mapcar '(lambda (x) (strcat delim x))
                                                            (vg:nthcdr 7 csv-data))))))))
                      (cond
                        ;; unreadable row (blank line is ignored silently)
                        ((or (< (length csv-data) 2)
                             (= (vl-string-trim " " (nth 0 csv-data)) ""))
                         (if (/= (vl-string-trim " \t\r\n" line) "")
                           (progn (setq bad (1+ bad))
                                  (prompt (strcat "\n! Row not read (check delimiter): " line)))))
                        (T (setq km-list (append km-list (list csv-data))))))
                    (close fp)

                    ;; ---- 4. place blocks ----
                    (setq count 0
                          skipped 0)

                    (foreach km-data km-list
                      ;; vid_ch = distance along the selected polyline
                      (setq offset-dist (atof (nth 0 km-data)))

                      (if (and (>= offset-dist 0.0) (<= offset-dist pl-length))
                        (progn
                          (prompt (strcat "\n\nProcessing " (nth 1 km-data) "  at " (rtos offset-dist 2 3) " m"))

                          ;; tangent angle
                          (setq pt1 (vlax-curve-getPointAtDist pl offset-dist))
                          (setq pt2 (vlax-curve-getPointAtDist
                                      pl (min pl-length (+ offset-dist 0.01))))
                          (if (equal pt1 pt2 1e-9)       ;; at the very end: look backwards
                            (setq pt2 pt1
                                  pt1 (vlax-curve-getPointAtDist pl (max 0.0 (- offset-dist 0.01)))))
                          (setq tang-angle (angle pt1 pt2))

                          ;; offset point (right of track)
                          (setq pt-final
                                (vlax-3d-point
                                  (polar (vlax-curve-getPointAtDist pl offset-dist)
                                         (- tang-angle (/ pi 2.0))
                                         perp-offset)))

                          (setq obj (vla-InsertBlock mspace pt-final blk-name
                                                     1.0 1.0 1.0 tang-angle))

                          ;; attributes
                          (setq tail-value nil
                                head-value nil
                                pos-done   nil
                                g-pos      (nth 0 km-data))

                          (if (= (vla-get-HasAttributes obj) :vlax-true)
                            (foreach att (vlax-invoke obj 'GetAttributes)
                              (setq tag (strcase (vla-get-TagString att)))
                              (cond
                                ((= tag "G_POSITION")
                                 (vla-put-TextString att g-pos)
                                 (setq pos-done T))
                                ((= tag "G_CHAINAGE")
                                 (vla-put-TextString att (nth 1 km-data)))
                                ((= tag "G_RAILLEVEL")
                                 (if (nth 2 km-data) (vla-put-TextString att (nth 2 km-data))))
                                ((= tag "G_LINE")
                                 (if (nth 3 km-data) (vla-put-TextString att (nth 3 km-data))))
                                ((= tag "G_TAIL")
                                 (setq tail-value (nth 4 km-data))
                                 (if tail-value (vla-put-TextString att tail-value)))
                                ((= tag "G_HEAD")
                                 (setq head-value (nth 5 km-data))
                                 (if head-value (vla-put-TextString att head-value)))
                                ((= tag "G_NOTE")
                                 (vla-put-TextString att (if (nth 6 km-data) (nth 6 km-data) ""))))))

                          ;; The block is already placed at its exact distance on the polyline,
                          ;; so the block's G_POSITION dynamic property must NOT shift it again.
                          (vg:set-dyn obj "G_POSITION" 0.0)

                          ;; G_TAIL_ROT / G_HEAD_ROT (degrees -> radians)
                          (if (not (vg:set-dyn obj "G_TAIL_ROT" (* (vg:grad-rot tail-value) (/ pi 180.0))))
                            (prompt "\n  ! G_TAIL_ROT not found"))
                          (if (not (vg:set-dyn obj "G_HEAD_ROT" (* (vg:grad-rot head-value) (/ pi 180.0))))
                            (prompt "\n  ! G_HEAD_ROT not found"))

                          (vla-update obj)
                          (setq count (1+ count))
                          (prompt "\n  Block inserted"))
                        (progn
                          (setq skipped (1+ skipped))
                          (prompt (strcat "\n! " (nth 1 km-data) " (" (rtos offset-dist 2 3) " m) skipped: polyline is only "
                                          (rtos pl-length 2 3) " long")))))

                    (prompt (strcat "\n\nCompleted. Inserted: " (itoa count)
                                    "  Skipped (outside track): " (itoa skipped)))
)))))))))

  (princ)
)

(prompt "\nvidGradient loaded. Type VIDGRADIENT to run.")
(princ)
