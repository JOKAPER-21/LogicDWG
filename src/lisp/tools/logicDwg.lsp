;;; ============================================================================
;;; logicDwg.lsp  -  Logic DWG launcher
;;;
;;; Commands : LOGICDWG, VIDLOGICDWG
;;; Dialog   : logicDwg.dcl (same folder as this file)
;;;
;;;   Zone                  [43] [44] [Off]     -> assigns UTM84-43N / 44N and
;;;                                                turns GeoMap on / off
;;;   DGPS to Survey Point  [Generate Points]   -> VIDDGPSTOSP
;;;   Rail Tracks           [Generate Track]    -> VIDDGPSTOLINE
;;;
;;; Notes
;;;   - Tools are started AFTER the dialog has closed (never from inside a
;;;     dialog callback), and a fresh new_dialog is created for every show.
;;;   - The DCL is looked up next to this file / vidLoader.lsp. If it cannot
;;;     be found, an identical copy is written to the TEMP folder, so the
;;;     dialog always opens.
;;;   - No usernames, no absolute paths, no hard-coded Civil 3D version.
;;; ============================================================================

(vl-load-com)


;;; ----------------------------------------------------------------------------
;;; DCL lookup
;;; ----------------------------------------------------------------------------

(defun LogicDWG:TryDir (baseFile subPath / p)
  ;; Directory of (findfile baseFile) + subPath, returned only if it exists.
  (if (setq p (findfile baseFile))
    (findfile (strcat (vl-filename-directory p) subPath))
    nil
  )
)

(defun LogicDWG:FindDcl ( / p)
  (cond
    ((setq p (findfile "logicDwg.dcl")) p)
    ((setq p (LogicDWG:TryDir "logicDwg.lsp" "\\logicDwg.dcl")) p)
    ((setq p (LogicDWG:TryDir "vidLoader.lsp" "\\tools\\logicDwg.dcl")) p)
    ((setq p (LogicDWG:TryDir "vidLoader.lsp" "\\logicDwg.dcl")) p)
    (T nil)
  )
)


;;; ----------------------------------------------------------------------------
;;; Fallback DCL (identical to logicDwg.dcl) - used only if the file is missing
;;; ----------------------------------------------------------------------------

(defun LogicDWG:DclLines ()
  (list
    "logicDwg : dialog {"
    "  label = \"Logic DWG\";"
    "  : boxed_row {"
    "    label = \"Zone\";"
    "    : button { key = \"z43\";  label = \"43\";  width = 10; fixed_width = true; }"
    "    : button { key = \"z44\";  label = \"44\";  width = 10; fixed_width = true; }"
    "    : button { key = \"zoff\"; label = \"Off\"; width = 10; fixed_width = true; }"
    "  }"
    "  : boxed_row {"
    "    label = \"DGPS to Survey Point\";"
    "    : button { key = \"sp\"; label = \"Generate Points\"; width = 34; fixed_width = true; }"
    "  }"
    "  : boxed_row {"
    "    label = \"Rail Tracks\";"
    "    : button { key = \"track\"; label = \"Generate Track\"; width = 34; fixed_width = true; }"
    "  }"
    "  spacer_1;"
    "  : row {"
    "    alignment = centered;"
    "    : button { key = \"close\"; label = \"Close\"; width = 12; fixed_width = true; is_cancel = true; }"
    "  }"
    "}"
  )
)

(defun LogicDWG:WriteTempDcl (/ dir path f)
  (setq dir
    (cond
      ((getenv "TEMP"))
      ((getenv "TMP"))
      (T (getvar "DWGPREFIX"))
    )
  )
  (setq path (strcat dir "\\logicDwg_" (itoa (getvar "MILLISECS")) ".dcl"))
  (setq f (open path "w"))
  (if f
    (progn
      (foreach ln (LogicDWG:DclLines) (write-line ln f))
      (close f)
      path
    )
    nil
  )
)


;;; ----------------------------------------------------------------------------
;;; Show the dialog once. Returns the done_dialog code (0 = closed).
;;;   1 Generate Points   2 Generate Track   3 Zone 43   4 Zone 44   5 Zone Off
;;; ----------------------------------------------------------------------------

(defun LogicDWG:Show (/ dclFile tempFile dclId code)
  (setq code 0)
  (setq dclFile (LogicDWG:FindDcl))

  (if (null dclFile)
    (progn
      (setq tempFile (LogicDWG:WriteTempDcl))
      (setq dclFile tempFile)
    )
  )

  (if (null dclFile)
    (alert "LogicDWG: the dialog file could not be found or created.")
    (progn
      (setq dclId (load_dialog dclFile))
      (if (or (null dclId) (< dclId 1))
        (alert (strcat "LogicDWG: unable to load dialog file:\n" dclFile))
        (progn
          (if (new_dialog "logicDwg" dclId)
            (progn
              (action_tile "z43"   "(done_dialog 3)")
              (action_tile "z44"   "(done_dialog 4)")
              (action_tile "zoff"  "(done_dialog 5)")
              (action_tile "sp"    "(done_dialog 1)")
              (action_tile "track" "(done_dialog 2)")
              (action_tile "close" "(done_dialog 0)")
              (setq code (start_dialog))
            )
            (alert (strcat "LogicDWG: dialog \"logicDwg\" not found in:\n" dclFile))
          )
          (unload_dialog dclId)
        )
      )
    )
  )

  (if tempFile (vl-file-delete tempFile))
  (if (numberp code) code 0)
)


;;; ----------------------------------------------------------------------------
;;; Zone buttons (same native commands the old SM tool used)
;;; ----------------------------------------------------------------------------

(defun LogicDWG:ZoneApply (mode / cs)
  (cond
    ((= mode "OFF")
     (command "._GEOMAP" "_Off")
    )
    (T
     (setq cs (if (= mode "44") "UTM84-44N" "UTM84-43N"))
     (command "._MAPCSASSIGN" cs)
     (command "._GEOMAP" "_Hybrid")
     (command "._ZOOM" "_E")
    )
  )
  T
)

(defun LogicDWG:Zone (mode / oldEcho res)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq res (vl-catch-all-apply 'LogicDWG:ZoneApply (list mode)))
  ;; If a command was left waiting for input, cancel it.
  (repeat 3
    (if (> (getvar "CMDACTIVE") 0) (command))
  )
  (setvar "CMDECHO" oldEcho)
  (if (vl-catch-all-error-p res)
    (princ (strcat "\nLogicDWG zone error: " (vl-catch-all-error-message res)))
    (princ
      (cond
        ((= mode "OFF") "\nGeoMap turned Off.")
        ((= mode "44") "\nZone 44 (UTM84-44N) set.")
        (T "\nZone 43 (UTM84-43N) set.")
      )
    )
  )
  (princ)
)


;;; ----------------------------------------------------------------------------
;;; Start a tool command by name (no "._" prefix - these are custom commands).
;;; Called only after the dialog is closed.
;;; ----------------------------------------------------------------------------

(defun LogicDWG:RunCmd (name / sym)
  (setq sym (read (strcat "c:" name)))
  (if (and (boundp sym)
           (member (type (eval sym)) '(USR SUBR EXRXSUBR))
      )
    (eval (list sym))
    (alert
      (strcat
        "Command " (strcase name) " is not loaded.\n\n"
        "Load the LogicDWG tools first (vidLoader.lsp)."
      )
    )
  )
)


;;; ----------------------------------------------------------------------------
;;; Command
;;; ----------------------------------------------------------------------------

(defun c:LOGICDWG (/ *error* again code)

  ;; Quiet handler: ESC / cancel inside a tool is not reported as an error.
  (defun *error* (msg)
    (if (and msg
             (not (member (strcase msg) '("FUNCTION CANCELLED" "QUIT / EXIT ABORT")))
        )
      (princ (strcat "\nLogicDWG error: " msg))
    )
    (princ)
  )

  (setq again T)
  (while again
    (setq code (LogicDWG:Show))
    (cond
      ((= code 3) (LogicDWG:Zone "43"))
      ((= code 4) (LogicDWG:Zone "44"))
      ((= code 5) (LogicDWG:Zone "OFF"))
      ((= code 1)
       (setq again nil)
       (LogicDWG:RunCmd "VIDDGPSTOSP")
      )
      ((= code 2)
       (setq again nil)
       (LogicDWG:RunCmd "VIDDGPSTOLINE")
      )
      (T (setq again nil))
    )
  )
  (princ)
)

(defun c:VIDLOGICDWG ()
  (c:LOGICDWG)
)


;;; ----------------------------------------------------------------------------
;;; Load message
;;; ----------------------------------------------------------------------------

(princ "\n[OK] LogicDWG loaded. Type VIDLOGICDWG to open.")
(princ)
