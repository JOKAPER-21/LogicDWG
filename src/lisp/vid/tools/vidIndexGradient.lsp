;;; ============================================================
;;; VIDINDEXGRADIENT - Railway Gradient Index Plan Generator
;;; Command : VIDINDEXGRADIENT
;;; ============================================================

(vl-load-com)

;; Layers
(setq VID:LYR-BAR "1-VIG VID_CH SCALE BAR"   ; scale bar boxes + hatch, index mtext, 2 m and 3 m ticks
      VID:LYR-RL  "1-VIG RAIL_LEVEL"         ; rail_level mtext + rail_level polylines
      VID:LYR-RF  "1-VIG RISE_FALL"          ; rise_fall mtext + rise_fall polyline
)

;; Layer colours (AutoCAD colour index): green, red, yellow
(setq VID:CLR-BAR 3   ; 1-VIG VID_CH SCALE BAR = green
      VID:CLR-RL  1   ; 1-VIG RAIL_LEVEL       = red
      VID:CLR-RF  2   ; 1-VIG RISE_FALL        = yellow
)

;; Last used dialog values: (hValue vValue), in metres per 1000 m
(if (not *VID:vals*)
  (setq *VID:vals* (list "20" "1000"))
)

;;; ------------------------------------------------------------
;;; Main command
;;; ------------------------------------------------------------
(defun c:VIDINDEXGRADIENT (/ csv rows opts ip)
  (VID:ensure-layer VID:LYR-BAR VID:CLR-BAR)
  (VID:ensure-layer VID:LYR-RL VID:CLR-RL)
  (VID:ensure-layer VID:LYR-RF VID:CLR-RF)

  ;; 1) CSV file, 2) scale dialog, 3) insertion point
  (if (setq csv (getfiled "Select CSV File" (getvar "DWGPREFIX") "csv" 16))
    (if (setq rows (VID:read-csv csv))
      (if (setq opts (VID:show-dialog))
        (if (setq ip (getpoint "\nPick insertion point: "))
          (progn
            (VID:draw-profile rows opts ip)
            (princ "\nVIDINDEXGRADIENT complete.")
          )
          (princ "\nNo insertion point picked.")
        )
        (princ "\nCancelled.")
      )
      (princ "\nNo valid data rows found in CSV.")
    )
    (princ "\nNo CSV selected.")
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
          "  : boxed_row {"
          "    label = \"Horizontal\";"
          "    : text { label = \"1 KM (1000 m)  =\"; width = 18; }"
          "    : edit_box { key = \"h_val\"; edit_width = 8; }"
          "    : text { label = \"m\"; width = 2; }"
          "  }"
          "  : boxed_row {"
          "    label = \"Vertical\";"
          "    : text { label = \"1 KM (1000 m)  =\"; width = 18; }"
          "    : edit_box { key = \"v_val\"; edit_width = 8; }"
          "    : text { label = \"m\"; width = 2; }"
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
       (setq lst *VID:vals*)
       (set_tile "h_val" (nth 0 lst))
       (set_tile "v_val" (nth 1 lst))
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

;; Returns a real > 0, or nil for anything else (empty, text, 0, negative)
(defun VID:parse-pos (str / v)
  (setq v (distof (vl-string-trim " " str) 2))
  (if (and v (> v 0.0)) v nil)
)

(defun VID:on-ok (/ hv vv)
  (setq hv (VID:parse-pos (get_tile "h_val")))
  (setq vv (VID:parse-pos (get_tile "v_val")))
  (cond
    ((not hv)
     (set_tile "error" "Horizontal value must be a number greater than 0."))
    ((not vv)
     (set_tile "error" "Vertical value must be a number greater than 0."))
    (t
     (setq *VID:vals* (list (get_tile "h_val") (get_tile "v_val")))
     (setq VID:result (list hv vv))
     (done_dialog 1))
  )
)

;;; ------------------------------------------------------------
;;; Scaling (same rule for horizontal and vertical)
;;;   "1000 m = <val> m"  ->  drawn value = original * val / 1000
;;;   val = 1000 -> no change, val < 1000 -> reduce, val > 1000 -> enlarge
;;; ------------------------------------------------------------
(defun VID:scale (v val)
  (* v (/ val 1000.0))
)

;;; ------------------------------------------------------------
;;; Drawing
;;; ------------------------------------------------------------
(defun VID:draw-profile (rows opts ip / hVal vVal
                         i row ch rl rf newCH newRL baseNewCH xOff basePt
                         topVerts baseXs rfVals lyrBar lyrRL lyrRF)
  (setq lyrBar VID:LYR-BAR lyrRL VID:LYR-RL lyrRF VID:LYR-RF)
  (setq hVal (nth 0 opts) vVal (nth 1 opts))
  (setq baseNewCH (VID:scale (float (car (car rows))) hVal))
  (setq topVerts '() baseXs '() rfVals '())

  (setq i 0)
  (while (< i (length rows))
    (setq row (nth i rows))
    (setq ch  (float (car row)))
    (setq rl  (cadr row))
    (setq rf  (caddr row))
    (setq newCH (VID:scale ch hVal))
    (setq newRL (VID:scale rl vVal))
    (setq xOff  (- newCH baseNewCH))
    (setq basePt (list (+ (car ip) xOff) (cadr ip) 0.0))
    (setq topVerts (append topVerts (list (VID:draw-station basePt rl newRL lyrBar lyrRL))))
    (setq baseXs   (append baseXs (list (car basePt))))
    (setq rfVals   (append rfVals (list rf)))
    (setq i (1+ i))
  )

  (VID:draw-gradient-line topVerts lyrRF)
  (VID:draw-rf-labels baseXs rfVals (cadr ip) lyrRF)
  ;; xOff now holds the drawn length of the whole profile (last row)
  (VID:draw-scalebar ip hVal xOff lyrBar)
)

(defun VID:ensure-layer (name color / ent d old)
  (if (setq ent (tblobjname "LAYER" name))
    ;; layer exists: set its colour (a negative colour = layer off, keep that)
    (progn
      (setq d   (entget ent)
            old (cdr (assoc 62 d)))
      (if (and old (/= (abs old) color))
        (entmod (subst (cons 62 (if (< old 0) (- color) color)) (assoc 62 d) d))
      )
    )
    ;; new layer
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 color) '(6 . "Continuous")))
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

(defun VID:parse-row (ln cols / parts chStr rlStr rfStr)
  (setq parts (VID:split ln ","))
  (if (< (length parts) (1+ (max (nth 0 cols) (nth 1 cols) (nth 2 cols))))
    nil
    (progn
      (setq chStr (vl-string-trim " " (nth (nth 0 cols) parts)))
      (setq rlStr (vl-string-trim " " (nth (nth 1 cols) parts)))
      (setq rfStr (vl-string-trim " " (nth (nth 2 cols) parts)))
      (if (or (= chStr "") (= rlStr "") (= rfStr ""))
        nil
        (list (atof chStr) (atof rlStr) rfStr)
      )
    )
  )
)

;; Index of the header cell containing <name> (case-insensitive), else <default>
(defun VID:find-col (hdrParts name default / i found)
  (setq i 0 found nil)
  (while (and (not found) (< i (length hdrParts)))
    (if (vl-string-search name (strcase (nth i hdrParts) T))
      (setq found i)
    )
    (setq i (1+ i))
  )
  (if found found default)
)

;; Reads the CSV (header: vid_ch, cal_rail_level, rise_fall). Columns are found
;; by header name; if a name is missing, column order 1,2,3 is used.
;; Returns a list of (ch rl rf) rows, or nil if there are none.
(defun VID:read-csv (csvFile / f ln hdr hdrParts cols rows row lineNum skipped)
  (setq rows '() lineNum 0 skipped 0)
  (if (setq f (open csvFile "r"))
    (progn
      (setq hdr (read-line f))
      (setq hdrParts (VID:split (if hdr hdr "") ","))
      (setq cols (list (VID:find-col hdrParts "vid_ch" 0)
                       (VID:find-col hdrParts "cal_rail_level" 1)
                       (VID:find-col hdrParts "rise_fall" 2)))
      (while (setq ln (read-line f))
        (setq lineNum (1+ lineNum))
        (setq ln (vl-string-trim " \r\t" ln))
        (if (/= ln "")
          (if (setq row (VID:parse-row ln cols))
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

(defun VID:draw-station (basePt ogRL newRL lyrBar lyrRL /
                         topPt1 labelPt poly2Start poly2Top txt)
  (setq topPt1     (list (car basePt) (+ (cadr basePt) 2.0) 0.0))
  (setq labelPt    (list (car topPt1) (+ (cadr topPt1) 0.2) 0.0))
  (setq poly2Start (list (car topPt1) (+ (cadr topPt1) 12.0) 0.0))
  (setq poly2Top   (list (car poly2Start) (+ (cadr poly2Start) newRL) 0.0))
  (setq txt (rtos ogRL 2 3))

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lyrBar)
                 '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0)
                 (cons 10 (list (car basePt) (cadr basePt)))
                 (cons 10 (list (car topPt1) (cadr topPt1)))))

  ;; RailLevel MTEXT ($mTextRailLevel: width 11.5)
  (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 lyrRL)
                 '(100 . "AcDbMText")
                 (cons 10 labelPt)
                 (cons 40 2.0) (cons 41 11.5) (cons 50 (/ pi 2.0))
                 (cons 71 4) (cons 1 txt)))

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lyrRL)
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

;; Solid hatch inside a rectangle (falls back to a SOLID entity if HATCH fails)
(defun VID:fill-box (x0 y0 x1 y1 lyr)
  (if (not
        (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") (cons 8 lyr)
                       '(100 . "AcDbHatch")
                       '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0)
                       '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1)
                       '(92 . 7) '(72 . 0) '(73 . 1) '(93 . 4)
                       (cons 10 (list x0 y0)) (cons 10 (list x1 y0))
                       (cons 10 (list x1 y1)) (cons 10 (list x0 y1))
                       '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 0))))
    (entmake (list '(0 . "SOLID") '(100 . "AcDbEntity") (cons 8 lyr)
                   '(100 . "AcDbTrace")
                   (cons 10 (list x0 y0 0.0)) (cons 11 (list x1 y0 0.0))
                   (cons 12 (list x0 y1 0.0)) (cons 13 (list x1 y1 0.0))))
  )
)

;; Scale bar below the baseline.
;;   - boxes 2.5 high, each <hVal> / 2 wide, alternating filled / empty
;;   - one 3 m tick under the bar at every <hVal> (= 1 KM) with index 0,1,2...
;;   - covers the whole profile, rounded up to a full KM
(defun VID:draw-scalebar (ip hVal totalLen lyr /
                          n k segW x0 x1 y0 y1 yTick)
  (setq n (max 1 (fix (+ (/ totalLen hVal) 0.999999)))) ; KM count, rounded up
  (setq segW (/ hVal 2.0))
  (setq y1 (cadr ip))        ; bar top    = baseline
  (setq y0 (- y1 2.5))       ; bar bottom = 2.5 below
  (setq yTick (- y0 3.0))    ; tick end   = 3 below the bar

  ;; bar boxes
  (setq k 0)
  (while (< k (* 2 n))
    (setq x0 (+ (car ip) (* k segW)))
    (setq x1 (+ x0 segW))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lyr)
                   '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                   (cons 10 (list x0 y0)) (cons 10 (list x1 y0))
                   (cons 10 (list x1 y1)) (cons 10 (list x0 y1))))
    (if (= (rem k 2) 0)      ; 1st, 3rd, ... box is filled with a solid hatch
      (VID:fill-box x0 y0 x1 y1 lyr)
    )
    (setq k (1+ k))
  )

  ;; ticks + index numbers
  (setq k 0)
  (while (<= k n)
    (setq x0 (+ (car ip) (* k hVal)))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lyr)
                   '(100 . "AcDbPolyline") '(90 . 2) '(70 . 0)
                   (cons 10 (list x0 y0)) (cons 10 (list x0 yTick))))
    (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 lyr)
                   '(100 . "AcDbMText")
                   (cons 10 (list x0 (- yTick 0.5) 0.0))
                   (cons 40 2.0) (cons 71 2) (cons 1 (itoa k))))
    (setq k (1+ k))
  )
)

;; RF of row N is placed between CH row N and CH row N+1, 12m above baseline
;; ($mTextRiseFall: width 21)
(defun VID:draw-rf-labels (baseXs rfVals baseY lyrLbl / i midX ypt pt txt)
  (setq i 1)
  (while (< i (length baseXs))
    (setq midX (/ (+ (nth (1- i) baseXs) (nth i baseXs)) 2.0))
    (setq ypt  (+ baseY 2.0 12.0)) ; 2m tick + 12m offset above it
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
