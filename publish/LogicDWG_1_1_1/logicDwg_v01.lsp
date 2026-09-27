;;;===========================================================================
;;; logicdwg_v03.lsp
;;;
;;; Command : VIDLOGICDWG
;;; Purpose : Launcher dialog ("Logic DWG") for the survey toolset.
;;;
;;;   Zone                    -> [43] [44] [Off]      -> MAPCSASSIGN/GEOMAP
;;;   DGPS to Survey Point    -> [Generate Points]     -> VIDDGPSTOSP
;;;   Rail Tracks             -> [Generate Track]      -> DGPS2PLINE
;;;
;;; V03 FIX: calling a custom LISP command by name through (command "...")
;;; from deep inside another already-running command (here, right after
;;; this dialog's start_dialog returns) can fail with AutoCAD reporting
;;; "Unknown command", even though the same command works fine when typed
;;; directly at the command line straight afterwards. To avoid this:
;;;
;;;   - VIDDGPSTOSP and DGPS2PLINE are now invoked as plain LISP function
;;;     calls - (c:VIDDGPSTOSP) / (c:DGPS2PLINE) - instead of dispatching
;;;     through AutoCAD's command-name lookup. A command defined with
;;;     (defun c:NAME ...) is just a normal function named "C:NAME" and
;;;     can always be called directly like any other function, which
;;;     sidesteps the command-table timing issue entirely.
;;;   - The Zone buttons no longer re-dispatch to SM (which would hit the
;;;     same issue, plus SM reads its answer via getkword which can't be
;;;     pre-fed from a direct function call). Instead the launcher issues
;;;     the same native AutoCAD/Civil3D commands SM itself uses for each
;;;     branch (MAPCSASSIGN / GEOMAP / ZOOM). These are built-in commands,
;;;     not custom ones, so dispatching to them via (command) is reliable.
;;;     sm.lsp itself is unmodified and still works standalone as before.
;;;
;;; The DCL definition is written to a temp file at runtime (no
;;; separate .dcl file to distribute) and cleaned up afterwards. The
;;; dialog stays open after each run so any tool can be launched
;;; repeatedly; click Close to exit.
;;;
;;; Load : APPLOAD -> logicdwg_v03.lsp   (or via loader.lsp)
;;; Run  : VIDLOGICDWG
;;;===========================================================================

(vl-load-com)

(defun c:VIDLOGICDWG (/ *error* dclfile dclid f code running old-cmdecho)

  (setq old-cmdecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*"))
             (not (wcmatch (strcase msg) "*QUIT*"))
             (not (wcmatch (strcase msg) "*ESC*"))
        )
        (princ (strcat "\nVIDLOGICDWG ERROR: " msg))
    )
    (if (and dclid (> dclid 0)) (unload_dialog dclid))
    (if (and dclfile (findfile dclfile)) (vl-file-delete dclfile))
    (setvar "CMDECHO" old-cmdecho)
    (princ)
  )

  (setvar "CMDECHO" 0)

  ;; -------------------------------------------------------------
  ;; Write the DCL definition to a temp file.
  ;; -------------------------------------------------------------
  (setq dclfile (strcat (getenv "TEMP") "\\vlogicdwg_" (itoa (getvar "MILLISECS")) ".dcl"))
  (setq f (open dclfile "w"))
  (if (not f) (error (strcat "Could not create temporary DCL file: " dclfile)))

  (write-line "vlogicdwg_dlg : dialog {" f)
  (write-line "  label = \"Logic DWG\";" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"\";" f)

  ;; --- Zone row (on top of everything else) ---
  (write-line "    : row {" f)
  (write-line "      : text { label = \"Zone\"; width = 10; }" f)
  (write-line "      : button { key = \"btn_z43\"; label = \"43\"; width = 8; fixed_width = true; }" f)
  (write-line "      : button { key = \"btn_z44\"; label = \"44\"; width = 8; fixed_width = true; }" f)
  (write-line "      : button { key = \"btn_zoff\"; label = \"Off\"; width = 8; fixed_width = true; }" f)
  (write-line "    }" f)
  (write-line "    spacer_1;" f)

  ;; --- DGPS to Survey Point row ---
  (write-line "    : row {" f)
  (write-line "      : text { label = \"DGPS to Survey Point\"; width = 28; }" f)
  (write-line "      : button { key = \"btn_sp\"; label = \"Generate Points\"; width = 18; fixed_width = true; }" f)
  (write-line "    }" f)
  (write-line "    spacer_1;" f)

  ;; --- Rail Tracks row ---
  (write-line "    : row {" f)
  (write-line "      : text { label = \"Rail Tracks\"; width = 28; }" f)
  (write-line "      : button { key = \"btn_track\"; label = \"Generate Track\"; width = 18; fixed_width = true; }" f)
  (write-line "    }" f)

  (write-line "  }" f)
  (write-line "  spacer_1;" f)
  (write-line "  : row {" f)
  (write-line "    fixed_width = true;" f)
  (write-line "    alignment = centered;" f)
  (write-line "    : button { key = \"cancel\"; label = \"Close\"; is_cancel = true; width = 12; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)

  ;; -------------------------------------------------------------
  ;; Load and run the dialog. It is only built once; start_dialog
  ;; is called repeatedly so the dialog reopens after each tool run.
  ;; -------------------------------------------------------------
  (setq dclid (load_dialog dclfile))
  (if (or (not dclid) (< dclid 0))
      (error (strcat "Could not load dialog definition: " dclfile))
  )

  (if (not (new_dialog "vlogicdwg_dlg" dclid))
      (error "Could not initialize the Logic DWG dialog.")
  )

  (action_tile "btn_z43" "(done_dialog 3)")
  (action_tile "btn_z44" "(done_dialog 4)")
  (action_tile "btn_zoff" "(done_dialog 5)")
  (action_tile "btn_sp" "(done_dialog 1)")
  (action_tile "btn_track" "(done_dialog 2)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq running T)
  (while running
    (setq code (start_dialog))
    (cond
      ((= code 1)
       (princ "\nRunning: DGPS to Survey Point...")
       (c:VIDDGPSTOSP)
      )
      ((= code 2)
       (princ "\nRunning: Rail Tracks...")
       (c:DGPS2PLINE)
      )
      ((= code 3)
       (princ "\nSetting Zone 43...")
       (command "._MAPCSASSIGN" "UTM84-43N")
       (command "._GEOMAP" "_Hybrid")
       (command "._ZOOM" "_E")
      )
      ((= code 4)
       (princ "\nSetting Zone 44...")
       (command "._MAPCSASSIGN" "UTM84-44N")
       (command "._GEOMAP" "_Hybrid")
       (command "._ZOOM" "_E")
      )
      ((= code 5)
       (princ "\nTurning Zone Off...")
       (command "._GEOMAP" "_Off")
      )
      (T (setq running nil))
    )
  )

  (unload_dialog dclid)
  (setq dclid nil)
  (if (findfile dclfile) (vl-file-delete dclfile))

  (setvar "CMDECHO" old-cmdecho)
  (setq *error* nil)
  (princ)
)

(princ "\nlogicdwg_v03.lsp loaded. Type VIDLOGICDWG to run.")
(princ)
