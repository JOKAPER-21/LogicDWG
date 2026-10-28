;;; ============================================================
;;; VIDINDEXGRADIENT - Railway Gradient Index Plan Generator
;;; Command : VIDINDEXGRADIENT
;;; ============================================================

(vl-load-com)

;; Last used dialog values: (csv hMode hValue vMode vValue)
(if (not *VID:last*)
  (setq *VID:last* (list "" "DEC" "20" "INC" "1"))
)

;;; ------------------------------------------------------------
;;; Main command
;;; ------------------------------------------------------------
(defun c:VIDINDEXGRADIENT (/ opts ip rows)
  (VID:ensure-layer "RAIL_PROFILE")
  (VID:ensure-layer "1 INDEX PLAN GRADIENT")

  (if (setq opts (VID:show-dialog))
    (if (setq ip (getpoint "\nPick insertion point: "))
      (if (setq rows (VID:read-csv (nth 0 opts)))
        (progn
          (VID:draw-profile rows opts ip)
          (princ "\nVIDINDEXGRADIENT complete.")
        )
        (princ "\nNo valid data rows found in CSV.")
      )
      (princ "\nNo insertion point picked.")
    )
    (princ "\nCancelled.")
  )
  (princ)
)

;;; ------------------------------------------------------------
;;; Dialog (DCL is written to a temp file, so only this .lsp is needed)
;;; ------------------------------------------------------------
(defun VID:write-dcl (/ path f)
  (setq path (vl-filename-mktemp "vidindex" nil ".dcl"))
  (if (setq f (open path "w"))
    (progn
      (foreach ln
        (list
          "vidindex : dialog {"
          "  label = \"VID Index Gradient\";"
          "  : row {"
          "    : button { key = \"btn_csv\"; label = \"Select CSV...\"; width = 16; fixed_width = true; }"
          "    : edit_box { key = \"csv_path\"; edit_width = 45; }"
          "  }"
          "  : boxed_column {"
          "    label = \"Horizontal\";"
          "    : radio_row {"
          "      : radio_button { key = \"h_inc\"; label = \"Increase\"; }"
          "      : radio_button { key = \"h_dec\"; label = \"Decrease\"; }"
          "    }"
          "    : edit_box { key = \"h_val\"; label = \"Horizontal Value:\"; edit_width = 12; }"
          "  }"
          "  : boxed_column {"
          "    label = \"Vertical\";"
          "    : radio_row {"
          "      : radio_button { key = \"v_inc\"; label = \"Increase\"; }"
          "      : radio_button { key = \"v_dec\"; label = \"Decrease\"; }"
          "    }"
          "    : edit_box { key = \"v_val\"; label = \"Vertical Value:\"; edit_width = 12; }"
          "  }"
          "  spacer;"
          "  ok_cancel_err;"
          "}"
        )
        (write-line ln f)
      )
      (close f)
      path
    )
  )
)

(defun VID:show-dialog (/ dclPath dclId status res lst)
  (setq VID:result nil)
  (setq dclPath (VID:write-dcl))
  (setq res
    (cond
      ((not dclPath)
       (princ "\nCannot create dialog file.")
       nil)
      ((< (setq dclId (load_dialog dclPath)) 0)
       (princ "\nCannot load dialog file.")
       nil)
      ((not (new_dialog "vidindex" dclId))
       (unload_dialog dclId)
       (princ "\nCannot open dialog.")
       nil)
      (t
       (setq lst *VID:last*)
       (set_tile "csv_path" (nth 0 lst))
       (set_tile (if (= (nth 1 lst) "INC") "h_inc" "h_dec") "1")
       (set_tile "h_val" (nth 2 lst))
       (set_tile (if (= (nth 3 lst) "DEC") "v_dec" "v_inc") "1")
       (set_tile "v_val" (nth 4 lst))
       (action_tile "btn_csv" "(VID:pick-csv)")
       (action_tile "accept"  "(VID:on-ok)")
       (setq status (start_dialog))
       (unload_dialog dclId)
       (if (= status 1) VID:result nil)
      )
    )
  )
  (if (and dclPath (findfile dclPath)) (vl-file-delete dclPath))
  res
)

(defun VID:pick-csv (/ cur f)
  (setq cur (get_tile "csv_path"))
  (setq f (getfiled "Select CSV File" (if cur cur "") "csv" 16))
  (if f (set_tile "csv_path" f))
)

;; Returns a real > 0, or nil for anything else (empty, text, 0, negative)
(defun VID:parse-pos (str / v)
  (setq v (distof (vl-string-trim " " str) 2))
  (if (and v (> v 0.0)) v nil)
)

(defun VID:on-ok (/ csv hv vv hm vm)
  (setq csv (vl-string-trim " \"" (get_tile "csv_path")))
  (setq hv  (VID:parse-pos (get_tile "h_val")))
  (setq vv  (VID:parse-pos (get_tile "v_val")))
  (setq hm  (if (= (get_tile "h_inc") "1") "INC" "DEC"))
  (setq vm  (if (= (get_tile "v_dec") "1") "DEC" "INC"))
  (cond
    ((= csv "")
     (set_tile "error" "Select a CSV file."))
    ((not (findfile csv))
     (set_tile "error" "CSV file not found."))
    ((not hv)
     (set_tile "error" "Horizontal value must be a number greater than 0."))
    ((not vv)
     (set_tile "error" "Vertical value must be a number greater than 0."))
    (t
     (setq *VID:last* (list csv hm (get_tile "h_val") vm (get_tile "v_val")))
     (setq VID:result (list csv hm hv vm vv))
     (done_dialog 1))
  )
)

;;; ------------------------------------------------------------
;;; Scaling
;;;   Horizontal : scale = 1000 / value
;;;                Decrease -> CH / scale   (reduce)
;;;                Increase -> CH * scale   (enlarge)
;;;   Vertical   : Increase -> RL * value
;;;                Decrease -> RL / value
;;; ------------------------------------------------------------
(defun VID:scale-h (ch mode scale)
  (if (= mode "INC") (* ch scale) (/ ch scale))
)

(defun VID:scale-v (rl mode val)
  (if (= mode "INC") (* rl val) (/ rl val))
)

;;; ------------------------------------------------------------
;;; Drawing
;;; ------------------------------------------------------------
(defun VID:draw-profile (rows opts ip / hMode hVal vMode vVal hScale
                         i row ch rl rf newCH newRL baseNewCH xOff basePt
                         topVerts baseXs rfVals lyrProf lyrLbl)
  (setq lyrProf "RAIL_PROFILE")
  (setq lyrLbl  "1 INDEX PLAN GRADIENT")
  (setq hMode (nth 1 opts) hVal (nth 2 opts)
        vMode (nth 3 opts) vVal (nth 4 opts))
  (setq hScale (/ 1000.0 hVal))
  (setq baseNewCH (VID:scale-h (float (car (car rows))) hMode hScale))
  (setq topVerts '() baseXs '() rfVals '())

  (setq i 0)
  (while (< i (length rows))
    (setq row (nth i rows))
    (setq ch  (float (car row)))
    (setq rl  (cadr row))
    (setq rf  (caddr row))
    (setq newCH (VID:scale-h ch hMode hScale))
    (setq newRL (VID:scale-v rl vMode vVal))
    (setq xOff  (- newCH baseNewCH))
    (setq basePt (list (+ (car ip) xOff) (cadr ip) 0.0))
    (setq topVerts (append topVerts (list (VID:draw-station basePt rl newRL lyrProf lyrLbl))))
    (setq baseXs   (append baseXs (list (car basePt))))
    (setq rfVals   (append rfVals (list (VID:format-rf rf))))
    (setq i (1+ i))
  )

  (VID:draw-gradient-line topVerts lyrProf)
  (VID:draw-rf-labels baseXs rfVals (cadr ip) lyrLbl)
)

(defun VID:ensure-layer (name)
  (if (not (tblsearch "LAYER" name))
    (command "._-LAYER" "_M" name "")
  )
)

(defun VID:split (str delim / pos lst)
  (setq lst '())
  (while (setq pos (vl-string-search delim str))
    (setq lst (append lst (list (substr str 1 pos))))
    (setq str (substr str (+ pos 1 (strlen delim))))
  )
  (append lst (list str))
)

(defun VID:parse-row (ln / parts chStr rlStr rfStr)
  (setq parts (VID:split ln ","))
  (if (< (length parts) 3)
    nil
    (progn
      (setq chStr (vl-string-trim " " (nth 0 parts)))
      (setq rlStr (vl-string-trim " " (nth 1 parts)))
      (setq rfStr (vl-string-trim " " (nth 2 parts)))
      (if (or (= chStr "") (= rlStr "") (= rfStr ""))
        nil
        (list (atof chStr) (atof rlStr) rfStr)
      )
    )
  )
)

;; Reads the CSV (CH,RL,RF with a header line). Returns a list of
;; (ch rl rf) rows, or nil if there are none.
(defun VID:read-csv (csvFile / f ln rows row lineNum skipped)
  (setq rows '() lineNum 0 skipped 0)
  (if (setq f (open csvFile "r"))
    (progn
      (read-line f) ; header
      (while (setq ln (read-line f))
        (setq lineNum (1+ lineNum))
        (setq ln (vl-string-trim " \r\t" ln))
        (if (/= ln "")
          (if (setq row (VID:parse-row ln))
            (setq rows (append rows (list row)))
            (progn
              (setq skipped (1+ skipped))
              (princ (strcat "\nWarning: skipped malformed CSV row #" (itoa lineNum) ": " ln))
            )
          )
        )
      )
      (close f)
    )
    (princ "\nCannot open CSV file.")
  )
  (if (> skipped 0)
    (princ (strcat "\n" (itoa skipped) " row(s) skipped due to formatting issues."))
  )
  rows
)

(defun VID:format-rf (rf / parts letter num)
  (if (or (not rf) (not (= (type rf) 'STR)))
    "?"
    (progn
      (setq parts  (VID:split (vl-string-trim " " rf) " "))
      (setq letter (nth 0 parts))
      (setq num    (nth 1 parts))
      (if (and letter num)
        (strcat letter " 1 IN " num)
        rf
      )
    )
  )
)

(defun VID:draw-station (basePt ogRL newRL lyrProf lyrLbl /
                         topPt1 labelPt poly2Start poly2Top txt)
  (setq topPt1     (list (car basePt) (+ (cadr basePt) 1.0) 0.0))
  (setq labelPt    (list (car topPt1) (+ (cadr topPt1) 0.2) 0.0))
  (setq poly2Start (list (car topPt1) (+ (cadr topPt1) 12.0) 0.0))
  (setq poly2Top   (list (car poly2Start) (+ (cadr poly2Start) newRL) 0.0))
  (setq txt (rtos ogRL 2 3))

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lyrProf)
                 '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0)
                 (cons 10 (list (car basePt) (cadr basePt)))
                 (cons 10 (list (car topPt1) (cadr topPt1)))))

  ;; RailLevel MTEXT ($mTextRailLevel: width 11.5)
  (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 lyrLbl)
                 '(100 . "AcDbMText")
                 (cons 10 labelPt)
                 (cons 40 2.0) (cons 41 11.5) (cons 50 (/ pi 2.0))
                 (cons 71 4) (cons 1 txt)))

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lyrProf)
                 '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0)
                 (cons 10 (list (car poly2Start) (cadr poly2Start)))
                 (cons 10 (list (car poly2Top) (cadr poly2Top)))))

  poly2Top
)

(defun VID:draw-gradient-line (pts lyr / verts p)
  (if (< (length pts) 2)
    (princ "\nNot enough points to draw gradient line.")
    (progn
      (setq verts '())
      (foreach p pts
        (setq verts (append verts (list (cons 10 (list (car p) (cadr p))))))
      )
      (entmake (append
                 (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lyr)
                       '(100 . "AcDbPolyline")
                       (cons 90 (length pts)) '(70 . 0))
                 verts))
    )
  )
)

;; RF of row N is placed between CH row N and CH row N+1, 12m above baseline
;; ($mTextRiseFall: width 21)
(defun VID:draw-rf-labels (baseXs rfVals baseY lyrLbl / i midX ypt pt txt)
  (setq i 1)
  (while (< i (length baseXs))
    (setq midX (/ (+ (nth (1- i) baseXs) (nth i baseXs)) 2.0))
    (setq ypt  (+ baseY 1.0 12.0)) ; 1m buffer tick + 12m offset above it
    (setq pt   (list midX ypt 0.0))
    (setq txt  (nth (1- i) rfVals))
    (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 lyrLbl)
                   '(100 . "AcDbMText")
                   (cons 10 pt)
                   (cons 40 2.0) (cons 41 21.0) (cons 50 (/ pi 2.0))
                   (cons 71 4)
                   (cons 1 txt)))
    (setq i (1+ i))
  )
)

(princ "\nVIDINDEXGRADIENT loaded. Type VIDINDEXGRADIENT to run.")
(princ)
