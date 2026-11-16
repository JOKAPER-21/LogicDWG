;;; ======================================================================
;;; vidCh.lsp - VID Chainage Calculator (AutoCAD / AutoCAD Civil 3D)
;;; Command : VIDCH
;;;
;;; REMOVE section (top of dialog)
;;;   Select : pick TEXT / MTEXT / MULTILEADER
;;;   Delete : removes every (CH:(+)...m) / (CH:(-)...m) from the picked text
;;;            "SIGNAL SH9 @KM 608/391 (CH:(-)229m)" -> "SIGNAL SH9 @KM 608/391 "
;;;
;;; CALCULATE
;;;   1. Pick Side (Left / Right) and Type (With / Without Chainage)
;;;   2. Enter Flag KM and up to 5 Arrow Chainage KM values (meter only)
;;;   3. Click "Select" (Current KM) and pick TEXT / MTEXT / MULTILEADER
;;;      containing  @KM ##/###  (km/meter)
;;;   4. Meter value after "/" = Current KM; result is appended to the text
;;;
;;; Result format
;;;   Without Chainage : (CH:(-)123m)     (CH:(+)123m)       whole meters
;;;   With Chainage    : (CH:(-)123.12m)  (CH:(+)123.12m)    2 decimals
;;;   Integer part is padded to 3 digits (5.5 -> 005.50 / 005)
;;;
;;; Formulas (same as Chainage_Calculator.html)
;;;   Without Chainage : Flag - Current
;;;   With, Right      : (Sum(Arrow Chainage) - Flag) + Current
;;;   With, Left       : (Flag + Sum(Arrow Chainage)) - Current
;;;   Sign shown: Left = (-), Right = (+). Number shown is the absolute value.
;;;
;;; Notes
;;;  - DCL is written to %TEMP% at runtime (no static DCL file needed)
;;;  - Re-running on text that already has a (CH:...) result replaces it
;;;  - One UNDO step per batch
;;;  - Text is normalized first: formatting codes ({}, \A \H \C \F \S ...)
;;;    are ignored for all logic (plainText); the drawing text keeps its
;;;    original formatting (only the (CH:...) result is added or cut out)
;;;  - Plain ASCII only (no smart quotes)
;;; ======================================================================

(vl-load-com)

;;; ---------- settings (remembered during the session) ----------
(defun vch-init ()
  (if (not (member *vch-side* '("Left" "Right")))
    (setq *vch-side* "Left"))
  (if (not (member *vch-type* '("With" "Without")))
    (setq *vch-type* "Without"))
  (if (not (numberp *vch-flag*))
    (setq *vch-flag* 0.0))
  (if (not (and (listp *vch-chs*) (= (length *vch-chs*) 5)))
    (setq *vch-chs* (list 0.0 0.0 0.0 0.0 0.0)))
  (setq *vch-rmss* nil)
  (princ)
)

;;; ---------- number formatting ----------
(defun vch-pad3 (s)
  (while (< (strlen s) 3)
    (setq s (strcat "0" s)))
  s
)

;;; padp = T   -> absolute value, integer part padded to 3 digits (012.50)
;;; padp = nil -> plain signed 2-decimal number (used in the dialog)
(defun vch-fmt (n padp / neg sc ip fp s)
  (setq neg (< n 0.0)
        sc  (fix (+ (* (abs n) 100.0) 0.5))
        ip  (/ sc 100)
        fp  (rem sc 100)
        s   (itoa ip))
  (if padp
    (setq s (vch-pad3 s)))
  (strcat (if (and neg (not padp)) "-" "")
          s "."
          (if (< fp 10) "0" "")
          (itoa fp))
)

;;; ---------- calculation ----------
(defun vch-calc (cur / tot)
  (setq tot (apply '+ *vch-chs*))
  (cond
    ((= *vch-type* "Without") (- *vch-flag* cur))
    ((= *vch-side* "Right")   (+ (- tot *vch-flag*) cur))
    (T                        (- (+ *vch-flag* tot) cur))
  )
)

;;; With Chainage    -> (CH:(-)123.12m)
;;; Without Chainage -> (CH:(-)123m)
(defun vch-result-str (r / a body)
  (setq a (abs r))
  (setq body
    (if (= *vch-type* "With")
      (vch-fmt a T)
      (vch-pad3 (itoa (fix (+ a 0.5))))))
  (strcat "(CH:"
          (if (= *vch-side* "Right") "(+)" "(-)")
          body
          "m)")
)

;;; ---------- text parsing ----------
(defun vch-skip-sp (s i / n)
  (setq n (strlen s))
  (while (and (<= i n) (= (substr s i 1) " "))
    (setq i (1+ i)))
  i
)

(defun vch-scan-digits (s i / n)
  (setq n (strlen s))
  (while (and (<= i n) (wcmatch (substr s i 1) "#"))
    (setq i (1+ i)))
  i
)

;;; Finds "@KM ##/###" and returns the meter value (real) or nil
(defun vch-parse-meter (s / p n i st)
  (setq p (vl-string-search "@KM" (strcase s)))
  (if p
    (progn
      (setq n  (strlen s)
            i  (vch-skip-sp s (+ p 4))
            st i
            i  (vch-scan-digits s i))
      (if (and (> i st) (<= i n))
        (progn
          (setq i (vch-skip-sp s i))
          (if (and (<= i n) (= (substr s i 1) "/"))
            (progn
              (setq i  (vch-skip-sp s (1+ i))
                    st i
                    i  (vch-scan-digits s i))
              (if (and (<= i n)
                       (= (substr s i 1) ".")
                       (wcmatch (substr s (1+ i) 1) "#"))
                (setq i (vch-scan-digits s (1+ i))))
              (if (> i st)
                (atof (substr s st (- i st))))
            )
          )
        )
      )
    )
  )
)

;;; ---------- TEXT NORMALIZATION LAYER ----------
;;; rawText   = TextString exactly as stored in the drawing (with codes)
;;; plainText = readable text only; ALL tool logic (@KM parsing, finding
;;;             existing (CH:...) results) runs on plainText.
;;; The drawing is never changed by the extraction. When the tool writes
;;; back, it edits rawText minimally (see vch-remove-ch-raw), so local
;;; formatting of the original object is preserved.
;;;
;;; mtextp = T   MTEXT / MULTILEADER : full MTEXT inline codes are handled
;;;                \A \H \C \c \F \f \W \Q \T \p ...;  (dropped)
;;;                \L \l \O \o \K \k                   (dropped)
;;;                \S n/d; \S n#d; \S n^d;             (kept as n/d, n^d)
;;;                \P \N \X (newline)  \~ (space)  \\ \{ \}  (literal)
;;;                \U+XXXX  \M+nXXXX                   (single character)
;;;                { }  group braces                   (dropped, text kept)
;;; mtextp = nil TEXT : no inline codes, braces/backslashes stay literal
;;; Both : %%d %%p %%c %%% %%nnn are converted, %%u %%o toggles dropped
;;;
;;; Besides the returned string, each call fills three globals so a
;;; plainText position can be traced back to rawText:
;;;   *vch-os* / *vch-oe* = per plain character, 1-based raw start index
;;;                         and exclusive raw end index

(defun vch-chr (n)
  (if (and (> n 0) (< n 256)) (chr n) "?")
)

(defun vch-hex4 (s / v k d)
  (setq v 0 k 1)
  (while (and v (<= k (strlen s)))
    (setq d (vl-string-search (strcase (substr s k 1)) "0123456789ABCDEF"))
    (setq v (if d (+ (* v 16) d) nil)
          k (1+ k)))
  v
)

(defun vch-emit (str si ei / k)
  (setq k 1)
  (repeat (strlen str)
    (setq *vch-ob* (strcat *vch-ob* (substr str k 1))
          *vch-os* (cons si *vch-os*)
          *vch-oe* (cons ei *vch-oe*)
          k        (1+ k)))
)

(defun vch-extract-plain-text (rawText mtextp / n i c nx v p k ch content)
  (setq *vch-ob* "" *vch-os* nil *vch-oe* nil
        n (strlen rawText)
        i 1)
  (while (<= i n)
    (setq c (substr rawText i 1))
    (cond
      ;; %% codes (TEXT and MTEXT)
      ((and (= c "%") (<= (+ i 2) n) (= (substr rawText (1+ i) 1) "%"))
       (setq nx (strcase (substr rawText (+ i 2) 1)))
       (cond
         ((= nx "D") (vch-emit (chr 176) i (+ i 3)) (setq i (+ i 3)))
         ((= nx "P") (vch-emit (chr 177) i (+ i 3)) (setq i (+ i 3)))
         ((= nx "C") (vch-emit (chr 216) i (+ i 3)) (setq i (+ i 3)))
         ((member nx '("U" "O")) (setq i (+ i 3)))
         ((= nx "%") (vch-emit "%" i (+ i 3)) (setq i (+ i 3)))
         ((and (<= (+ i 4) n) (wcmatch (substr rawText (+ i 2) 3) "###"))
          (vch-emit (vch-chr (atoi (substr rawText (+ i 2) 3))) i (+ i 5))
          (setq i (+ i 5)))
         (T (vch-emit "%" i (1+ i)) (setq i (1+ i)))))
      ;; \U+XXXX unicode escape (TEXT and MTEXT)
      ((and (= c "\\")
            (<= (+ i 6) n)
            (= (strcase (substr rawText (1+ i) 2)) "U+")
            (setq v (vch-hex4 (substr rawText (+ i 3) 4))))
       (vch-emit (vch-chr v) i (+ i 7))
       (setq i (+ i 7)))
      ;; TEXT: everything else is literal
      ((not mtextp)
       (vch-emit c i (1+ i))
       (setq i (1+ i)))
      ;; MTEXT: group braces are dropped, their content is kept
      ((or (= c "{") (= c "}"))
       (setq i (1+ i)))
      ;; MTEXT: backslash codes
      ((= c "\\")
       (if (= i n)
         (setq i (1+ i))
         (progn
           (setq nx (substr rawText (1+ i) 1))
           (cond
             ((member nx '("\\" "{" "}"))
              (vch-emit nx i (+ i 2))
              (setq i (+ i 2)))
             ((member nx '("P" "N" "X"))
              (vch-emit (chr 10) i (+ i 2))
              (setq i (+ i 2)))
             ((= nx "~")
              (vch-emit " " i (+ i 2))
              (setq i (+ i 2)))
             ((member nx '("L" "l" "O" "o" "K" "k"))
              (setq i (+ i 2)))
             ((and (= nx "M") (<= (+ i 7) n) (= (substr rawText (+ i 2) 1) "+"))
              (vch-emit "?" i (+ i 8))
              (setq i (+ i 8)))
             ;; stacked text: keep the readable characters
             ((= nx "S")
              (setq p (vl-string-search ";" rawText i))
              (if p
                (progn
                  (setq content (substr rawText (+ i 2) (- p i 1))
                        k 1)
                  (repeat (strlen content)
                    (setq ch (substr content k 1))
                    (vch-emit (if (= ch "#") "/" ch) (+ i 1 k) (+ i 2 k))
                    (setq k (1+ k)))
                  (setq i (+ p 2)))
                (setq i (+ i 2))))
             ;; codes that end with ";" - drop code and its parameters
             ((member nx '("A" "H" "C" "c" "F" "f" "W" "Q" "T" "p"))
              (setq p (vl-string-search ";" rawText i))
              (setq i (if p (+ p 2) (+ i 2))))
             ;; unknown escape: keep the character, drop the backslash
             (T
              (vch-emit nx i (+ i 2))
              (setq i (+ i 2)))
           )
         )
       ))
      ;; MTEXT: normal character
      (T
       (vch-emit c i (1+ i))
       (setq i (1+ i)))
    )
  )
  (setq *vch-os* (reverse *vch-os*)
        *vch-oe* (reverse *vch-oe*))
  *vch-ob*
)

;;; Finds every "(CH:(+)...m)" / "(CH:(-)...m)" in plainText.
;;; Returns a list of (start . end) spans, 0-based, end exclusive.
(defun vch-find-ch (plainText / u pos p q res go)
  (setq u   (strcase plainText)
        pos 0
        go  T
        res nil)
  (while (and go (setq p (vl-string-search "(CH:(" u pos)))
    (setq q (vl-string-search "M)" u p))
    (if (and q (member (substr plainText (+ p 6) 2) '("+)" "-)")))
      (setq res (cons (cons p (+ q 2)) res)
            pos (+ q 2))
      (setq go nil)))
  (reverse res)
)

;;; Removes every (CH:...) result from rawText. The result is located in
;;; plainText, then exactly that span is cut out of rawText, so every other
;;; character and all formatting codes stay untouched. Surrounding spaces
;;; are kept.
(defun vch-remove-ch-raw (rawText mtextp / plainText spans sp st en)
  (setq plainText (vch-extract-plain-text rawText mtextp)
        spans     (reverse (vch-find-ch plainText)))
  (foreach sp spans
    (setq st (nth (car sp) *vch-os*)
          en (nth (1- (cdr sp)) *vch-oe*))
    (if (and st en)
      (setq rawText (strcat (substr rawText 1 (1- st))
                            (substr rawText en)))))
  rawText
)

;;; ---------- DCL (written to %TEMP% at runtime) ----------
(defun vch-write-dcl (/ f fh i l)
  (setq f  (vl-filename-mktemp "vidch" nil ".dcl")
        fh (open f "w"))
  (if fh
    (progn
      (foreach l
        '("vidch : dialog {"
          "  label = \"VID Chainage Calculator\";"
          "  : boxed_column {"
          "    label = \"Configuration\";"
          "    : row {"
          "      : text { label = \"Side\"; width = 8; fixed_width = true; }"
          "      : radio_row {"
          "        : radio_button { key = \"side_l\"; label = \"Left\"; value = \"1\"; width = 20; fixed_width = true; }"
          "        : radio_button { key = \"side_r\"; label = \"Right\"; width = 20; fixed_width = true; }"
          "      }"
          "    }"
          "    : row {"
          "      : text { label = \"Type\"; width = 8; fixed_width = true; }"
          "      : radio_row {"
          "        : radio_button { key = \"type_w\"; label = \"With Chainage\"; width = 20; fixed_width = true; }"
          "        : radio_button { key = \"type_n\"; label = \"Without Chainage\"; value = \"1\"; width = 20; fixed_width = true; }"
          "      }"
          "    }"
          "  }"
          "  : boxed_column {"
          "    label = \"Values\";"
          "    : row {"
          "      : text { label = \"Flag KM (only meter value)\"; width = 46; fixed_width = true; }"
          "      : edit_box { key = \"flag\"; edit_width = 12; edit_limit = 16; value = \"0.00\"; }"
          "    }"
          "    : boxed_column {"
          "      label = \"Arrow Chainage KM\";")
        (write-line l fh))
      (setq i 1)
      (repeat 5
        (write-line
          (strcat "      : row { : text { label = \"(#" (itoa i)
                  ") Arrow Chainage KM (only meter value)\"; width = 44; fixed_width = true; }"
                  " : edit_box { key = \"ch" (itoa i)
                  "\"; edit_width = 12; edit_limit = 16; value = \"0.00\"; } }")
          fh)
        (setq i (1+ i)))
      (foreach l
        '("    }"
          "    : row {"
          "      : text { label = \"Current KM\"; width = 46; fixed_width = true; }"
          "      : button { key = \"select\"; label = \"Select\"; width = 12; fixed_width = true; }"
          "    }"
          "  }"
          "  : boxed_column {"
          "    label = \"Remove\";"
          "    : row {"
          "      : text { label = \"Remove Existing CH\"; width = 22; fixed_width = true; }"
          "      : edit_box { key = \"rm_pat\"; edit_width = 22; value = \"(CH:([+,-])####m)\"; is_enabled = false; }"
          "    }"
          "    : row {"
          "      alignment = right;"
          "      : button { key = \"rm_sel\"; label = \"Select\"; width = 12; fixed_width = true; }"
          "      : button { key = \"rm_del\"; label = \"Delete\"; width = 12; fixed_width = true; }"
          "    }"
          "  }"
          "  : text { key = \"status\"; label = \" \"; width = 70; }"
          "  : button { key = \"cancel\"; label = \"Close\"; is_cancel = true; width = 12; fixed_width = true; alignment = centered; }"
          "}")
        (write-line l fh))
      (close fh)
      f
    )
    nil
  )
)

;;; ---------- dialog helpers ----------
(defun vch-sync-type (/ on k)
  (setq on (= (get_tile "type_w") "1"))
  (foreach k '("ch1" "ch2" "ch3" "ch4" "ch5")
    (mode_tile k (if on 0 1)))
)

(defun vch-init-tiles (msg / i)
  (set_tile (if (= *vch-side* "Right") "side_r" "side_l") "1")
  (set_tile (if (= *vch-type* "With") "type_w" "type_n") "1")
  (set_tile "flag" (vch-fmt *vch-flag* nil))
  (setq i 1)
  (repeat 5
    (set_tile (strcat "ch" (itoa i)) (vch-fmt (nth (1- i) *vch-chs*) nil))
    (setq i (1+ i)))
  (set_tile "status"
            (if (= msg "") " " msg))
  (vch-sync-type)
)

;;; Reads a numeric tile: blank = 0.0, invalid = nil
(defun vch-num (key / v n)
  (setq v (vl-string-trim " " (get_tile key)))
  (cond
    ((= v "") 0.0)
    ((setq n (distof v 2)) n)
    (T nil)
  )
)

;;; Stores every tile into the session settings.
;;; Returns the key of the first invalid field (or nil when all are valid).
;;; An invalid field keeps its previous value.
(defun vch-store-tiles (/ with bad v chs i k n)
  (setq with (= (get_tile "type_w") "1")
        bad  nil)
  (setq *vch-side* (if (= (get_tile "side_r") "1") "Right" "Left")
        *vch-type* (if with "With" "Without"))
  (setq v (vch-num "flag"))
  (if v
    (setq *vch-flag* v)
    (setq bad "flag"))
  (setq chs '() i 1)
  (repeat 5
    (setq k (strcat "ch" (itoa i))
          n (vch-num k))
    (if (null n)
      (progn
        (if (and with (null bad))
          (setq bad k))
        (setq n (nth (1- i) *vch-chs*))))
    (setq chs (append chs (list n))
          i   (1+ i)))
  (setq *vch-chs* chs)
  bad
)

;;; Current KM "Select": validate, store, close with code 2
(defun vch-on-select (/ bad)
  (setq bad (vch-store-tiles))
  (if bad
    (progn
      (set_tile "status" "Invalid number - enter plain meter values only.")
      (mode_tile bad 2))
    (done_dialog 2))
)

;;; Remove "Select" (code 3) / "Delete" (code 4)
(defun vch-on-rm (code)
  (vch-store-tiles)
  (done_dialog code)
)

(defun vch-on-cancel ()
  (vch-store-tiles)
  (done_dialog 0)
)

;;; ---------- CALCULATE: select objects + write results ----------
(defun vch-run-select (/ ss doc i ent obj s rawText plainText mtextp m new err upd nokm fail)
  (princ "\nVIDCH: Select TEXT/MTEXT/MULTILEADER containing @KM ##/###: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT,MULTILEADER,MLEADER"))))
  (if (null ss)
    "Nothing selected."
    (progn
      (setq doc (vla-get-activedocument (vlax-get-acad-object))
            upd 0 nokm 0 fail 0 i 0)
      (vla-startundomark doc)
      (repeat (sslength ss)
        (setq ent     (ssname ss i)
              i       (1+ i)
              obj     (vlax-ename->vla-object ent)
              rawText (vl-catch-all-apply 'vla-get-textstring (list obj)))
        (cond
          ((or (vl-catch-all-error-p rawText) (/= (type rawText) 'STR))
           (setq fail (1+ fail)))
          (T
           ;; all logic runs on plainText; rawText is only edited minimally
           (setq mtextp    (/= (cdr (assoc 0 (entget ent))) "TEXT")
                 plainText (vch-extract-plain-text rawText mtextp)
                 m         (vch-parse-meter plainText))
           (if (null m)
             (setq nokm (1+ nokm))
             (progn
               (setq new (strcat (vl-string-right-trim " " (vch-remove-ch-raw rawText mtextp))
                                 " "
                                 (vch-result-str (vch-calc m)))
                     err (vl-catch-all-apply 'vla-put-textstring (list obj new)))
               (if (vl-catch-all-error-p err)
                 (setq fail (1+ fail))
                 (progn
                   (vl-catch-all-apply 'vla-update (list obj))
                   (setq upd (1+ upd)))))))
        )
      )
      (vla-endundomark doc)
      (setq s (strcat "Updated " (itoa upd)
                      " | No @KM ##/###: " (itoa nokm)
                      " | Failed: " (itoa fail)))
      (princ (strcat "\nVIDCH: " s))
      s
    )
  )
)

;;; ---------- REMOVE: select ----------
(defun vch-run-rm-select (/ ss)
  (princ "\nVIDCH: Select TEXT/MTEXT/MULTILEADER to remove (CH:...) from: ")
  (setq ss (ssget '((0 . "TEXT,MTEXT,MULTILEADER,MLEADER"))))
  (setq *vch-rmss* ss)
  (if ss
    (strcat (itoa (sslength ss)) " object(s) selected. Click Delete to remove (CH:...).")
    "Nothing selected."
  )
)

;;; ---------- REMOVE: delete ----------
(defun vch-run-delete (/ ss doc i n ent obj s new err del none fail)
  (setq ss *vch-rmss*)
  (if (or (null ss) (= (sslength ss) 0))
    "Nothing to delete - click Select under Remove first."
    (progn
      (setq doc (vla-get-activedocument (vlax-get-acad-object))
            del 0 none 0 fail 0 i 0 n (sslength ss))
      (vla-startundomark doc)
      (repeat n
        (setq ent (ssname ss i)
              i   (1+ i)
              obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p obj)
          (setq fail (1+ fail))
          (progn
            (setq s (vl-catch-all-apply 'vla-get-textstring (list obj)))
            (cond
              ((or (vl-catch-all-error-p s) (/= (type s) 'STR))
               (setq fail (1+ fail)))
              (T
               (setq new (vch-remove-ch-raw
                           s
                           (/= (cdr (assoc 0 (entget ent))) "TEXT")))
               (if (= new s)
                 (setq none (1+ none))
                 (progn
                   (setq err (vl-catch-all-apply 'vla-put-textstring (list obj new)))
                   (if (vl-catch-all-error-p err)
                     (setq fail (1+ fail))
                     (progn
                       (vl-catch-all-apply 'vla-update (list obj))
                       (setq del (1+ del)))))))
            )
          )
        )
      )
      (vla-endundomark doc)
      (setq *vch-rmss* nil)
      (setq s (strcat "Removed from " (itoa del)
                      " | No (CH:...) found: " (itoa none)
                      " | Failed: " (itoa fail)))
      (princ (strcat "\nVIDCH: " s))
      s
    )
  )
)

;;; ---------- command ----------
(defun C:VIDCHCAL (/ *error* dclf dclid what msg)
  (defun *error* (m)
    (if (and m (not (wcmatch (strcase m) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nVIDCH error: " m)))
    (if (and dclid (> dclid 0))
      (unload_dialog dclid))
    (if (and dclf (findfile dclf))
      (vl-file-delete dclf))
    (princ)
  )
  (vch-init)
  (setq dclf (vch-write-dcl))
  (cond
    ((null dclf)
     (alert "VIDCH: could not write the temporary DCL file."))
    (T
     (setq dclid (load_dialog dclf)
           what  2
           msg   "")
     (if (or (null dclid) (< dclid 1))
       (alert "VIDCH: could not load the dialog.")
       (progn
         (while (> what 1)
           (if (new_dialog "vidch" dclid)
             (progn
               (vch-init-tiles msg)
               (action_tile "type_w" "(vch-sync-type)")
               (action_tile "type_n" "(vch-sync-type)")
               (action_tile "select" "(vch-on-select)")
               (action_tile "rm_sel" "(vch-on-rm 3)")
               (action_tile "rm_del" "(vch-on-rm 4)")
               (action_tile "cancel" "(vch-on-cancel)")
               (setq what (start_dialog))
               (cond
                 ((= what 2) (setq msg (vch-run-select)))
                 ((= what 3) (setq msg (vch-run-rm-select)))
                 ((= what 4) (setq msg (vch-run-delete)))
               ))
             (progn
               (alert "VIDCH: could not open the dialog.")
               (setq what 0))))
         (unload_dialog dclid)))
     (if (findfile dclf)
       (vl-file-delete dclf))
    )
  )
  (princ)
)

(princ "\nVIDCH loaded. Type VIDCH to start.")
(princ)
