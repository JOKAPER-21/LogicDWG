;;; ============================================================================
;;; vidDimSettings.lsp
;;; Command: VIDDIMSETTINGS
;;; ============================================================================
;;;
;;; Sets up the dimension style "Standard" (Dimension Style Manager >
;;; Modify) and the text style "Standard".  If either does not exist it is
;;; created first.  When everything is done, REGENALL is run.
;;;
;;;   Symbols and Arrows   Arrow size              1.5
;;;   Text                 Text style              Standard  (font: Arial)
;;;                        Text height             1.5
;;;                        Text alignment          Aligned with dimension line
;;;   Primary Units        Precision               0.00
;;;
;;; HOW TO CHANGE THINGS - edit the SETTINGS block below.
;;; ============================================================================

(vl-load-com)

;;; ============================================================================
;;; SETTINGS  (edit here)
;;; ============================================================================

(setq VIDDim:Settings
  '(("STYLE"     . "Standard")   ; dimension style to modify
    ("ARROW"     . 1.5)          ; arrow size            (DIMASZ)
    ("TEXTSTYLE" . "Standard")   ; text style used by the dimension style
    ("FONT"      . "Arial")      ; font name of that text style
    ("TEXTHT"    . 1.5)          ; text height           (DIMTXT)
    ("PRECISION" . 2)            ; decimals: 2 = 0.00    (DIMDEC)
  )
)


;;; ============================================================================
;;; CODE
;;; ============================================================================

(defun VIDDim:Get (key)
  (cdr (assoc key VIDDim:Settings))
)

;; Set the font of an existing text style.  Returns T on success.
(defun VIDDim:SetFont (styleName face / doc st res)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq res
    (vl-catch-all-apply
      '(lambda ()
         (setq st (vla-Item (vla-get-TextStyles doc) styleName))
         (vla-SetFont st face :vlax-false :vlax-false 0 34)
       )
    )
  )
  (not (vl-catch-all-error-p res))
)

;; Create the text style if it does not exist.  Returns T if it exists afterwards.
(defun VIDDim:EnsureTextStyle (styleName / doc res)
  (if (not (tblsearch "STYLE" styleName))
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq res
        (vl-catch-all-apply
          '(lambda () (vla-Add (vla-get-TextStyles doc) styleName))
        )
      )
      (if (vl-catch-all-error-p res)
        (princ (strcat "\nCould not create text style " styleName "."))
        (princ (strcat "\nText style " styleName " did not exist - created."))
      )
    )
  )
  (if (tblsearch "STYLE" styleName) T nil)
)

;; Create the dimension style if it does not exist.  Returns T if it exists afterwards.
(defun VIDDim:EnsureDimStyle (styleName / doc res)
  (if (not (tblsearch "DIMSTYLE" styleName))
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq res
        (vl-catch-all-apply
          '(lambda () (vla-Add (vla-get-DimStyles doc) styleName))
        )
      )
      (if (vl-catch-all-error-p res)
        (princ (strcat "\nCould not create dimension style " styleName "."))
        (princ (strcat "\nDimension style " styleName " did not exist - created."))
      )
    )
  )
  (if (tblsearch "DIMSTYLE" styleName) T nil)
)

;; REGENALL - regenerate the drawing and all viewports.
(defun VIDDim:RegenAll (/ res)
  (setq res (vl-catch-all-apply '(lambda () (command "._REGENALL"))))
  (if (vl-catch-all-error-p res)
    (vl-catch-all-apply
      '(lambda ()
         (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 1) ; acAllViewports
       )
    )
  )
  (princ "\nREGENALL done.")
  (princ)
)

(defun c:VIDDIMSETTINGS (/ *error* oldEcho oldStyle dimName tsName fontName styleHt)

  (defun *error* (msg)
    (if (and msg
             (not (member (strcase msg) '("FUNCTION CANCELLED" "QUIT / EXIT ABORT")))
        )
      (princ (strcat "\nVIDDIMSETTINGS error: " msg))
    )
    (repeat 3
      (if (> (getvar "CMDACTIVE") 0) (command))
    )
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (princ)
  )

  (setq oldEcho  (getvar "CMDECHO")
        dimName  (VIDDim:Get "STYLE")
        tsName   (VIDDim:Get "TEXTSTYLE")
        fontName (VIDDim:Get "FONT")
  )
  (setvar "CMDECHO" 0)

  ;; Create the styles first if they do not exist yet
  (VIDDim:EnsureTextStyle tsName)
  (VIDDim:EnsureDimStyle dimName)

  (cond
    ((not (tblsearch "STYLE" tsName))
     (princ (strcat "\nVIDDIMSETTINGS: text style \"" tsName "\" could not be created."))
    )
    ((not (tblsearch "DIMSTYLE" dimName))
     (princ (strcat "\nVIDDIMSETTINGS: dimension style \"" dimName "\" could not be created."))
    )
    (T
     (setq oldStyle (getvar "DIMSTYLE"))

     ;; 1. Text style: font name = Arial
     (if (VIDDim:SetFont tsName fontName)
       (princ (strcat "\nText style " tsName ": font set to " fontName "."))
       (princ (strcat "\nCould not set the font of text style " tsName "."))
     )

     ;; 2. Dimension style: make it current, change it, save it back
     (command "._-DIMSTYLE" "_Restore" dimName)
     (setvar "DIMASZ"   (VIDDim:Get "ARROW"))      ; arrow size
     (setvar "DIMTXSTY" tsName)                    ; text style
     (setvar "DIMTXT"   (VIDDim:Get "TEXTHT"))     ; text height
     (setvar "DIMTIH"   0)                         ; text inside: aligned with dim line
     (setvar "DIMTOH"   0)                         ; text outside: aligned with dim line
     (setvar "DIMDEC"   (VIDDim:Get "PRECISION"))  ; primary units precision
     (command "._-DIMSTYLE" "_Save" dimName "_Yes")
     (repeat 3
       (if (> (getvar "CMDACTIVE") 0) (command))
     )

     ;; 3. Put the previously current dimension style back
     (if (and oldStyle (/= (strcase oldStyle) (strcase dimName)))
       (progn
         (command "._-DIMSTYLE" "_Restore" oldStyle)
         (repeat 3
           (if (> (getvar "CMDACTIVE") 0) (command))
         )
       )
     )

     (princ (strcat "\nDimension style " dimName " updated: arrow "
                    (rtos (VIDDim:Get "ARROW") 2 3)
                    ", text height "
                    (rtos (VIDDim:Get "TEXTHT") 2 3)
                    ", precision "
                    (rtos (VIDDim:Get "PRECISION") 2 0) " decimals."))

     ;; A fixed height in the text style makes DIMTXT ignored.
     (setq styleHt (cdr (assoc 40 (tblsearch "STYLE" tsName))))
     (if (and styleHt (/= styleHt 0.0))
       (princ (strcat "\nNote: text style " tsName " has a fixed height of "
                      (rtos styleHt 2 3)
                      " - dimension text uses that instead of DIMTXT."))
     )
    )
  )

  ;; All functions done - regenerate everything
  (VIDDim:RegenAll)

  (setvar "CMDECHO" oldEcho)
  (princ)
)

(princ "\n[OK] VIDDIMSETTINGS loaded.")
(princ)
