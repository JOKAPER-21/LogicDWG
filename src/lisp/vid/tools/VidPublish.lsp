;;==========================================================================
;; VidPublish.lsp
;; AutoCAD Civil 3D 2026 - VID Publish Pipeline
;;
;; WHAT THIS DOES:
;;   1) Run command VIDPUBLISH (alias PUBLISH) in any open drawing.
;;   2) A dialog shows:
;;        - Artist Name dropdown (Ananthan, Divya, Rishi --
;;          edit *VP-ARTIST-LIST* near the top of this file to change
;;          the list)
;;        - Yard Names list, grouped by section (MEJ_TEN, TEN_TCN, ...);
;;          a "Target:" line under it previews the server version folder
;;          the publish will land in. The yard is pre-selected from the
;;          drawing's file name when it can be detected unambiguously.
;;        - Commands text box (written into the drawing's DWGPROPS
;;          Comments field before saving)
;;        - "Generate PDF" checkbox (checked by default)
;;        - Publish / Cancel
;;   3) On OK, for the selected yard:
;;        a) The Commands text is written into DWGPROPS -> Comments.
;;        b) The current drawing is SAVED (QSAVE) in place first.
;;        c) The drawing's OWN version is read straight from its
;;           filename ("..._v02" -> version 2; no "_vNN" tag at all
;;           is treated as an implicit version 1). NO folder scan is
;;           done -- the filename is the single source of truth.
;;        d) FIRST the server is checked: if that version's DWG is ALREADY
;;           on the server, a warning is shown and NOTHING happens (no
;;           save, no PDF, no copy, no log, no new version).
;;           Otherwise (PDF in e) is made first, then) the DWG is copied
;;           to the SERVER, straight into that
;;           version's folder (a sibling of "dwg\yard\yd_<code>",
;;           NOT a "publish" subfolder -- that subfolder concept is
;;           gone):
;;             yd_<code>\v<NN>\yd_<code>_v<NN>.dwg
;;           (the filename in the version folder always carries the
;;           matching "_vNN" tag, even if the local file being
;;           published didn't have one yet.) An existing server version
;;           is never overwritten.
;;        e) If "Generate PDF" is checked, the drawing's SINGLE
;;           paper-space layout (besides Model) is plotted to PDF
;;           using whatever plot device that layout already has, into
;;           the SAME version folder with the SAME base name:
;;             yd_<code>\v<NN>\yd_<code>_v<NN>.pdf
;;           If the drawing has 0 or 2+ paper-space layouts (besides
;;           Model), PDF is SKIPPED with a warning -- the DWG still
;;           publishes normally either way.
;;        f) If a PDF was generated successfully, a COPY of that same
;;           PDF is also saved LOCALLY, right next to the local DWG
;;           (same folder, same base name).
;;        g) The LOCAL drawing is then bumped to the NEXT version
;;           (v02 -> v03) and SAVED into a matching NEW local version
;;           folder (sibling of whatever folder it was just in, or a
;;           new "v01" folder nested under its current folder if it
;;           wasn't sitting in a "vNN" folder yet). This becomes the
;;           active open drawing. UNLIKE the old behavior, the OLD
;;           local DWG (and its PDF from step f) are NEVER deleted or
;;           moved -- both versions sit side by side locally. This
;;           only happens AFTER the server copy in (d) succeeds, so a
;;           failed local save-as can never affect what already
;;           reached the server.
;;        h) A line is appended to the publish log (*VP-LOG-PATH*), recording
;;           the SERVER-published version (curver) -- NOT the new local
;;           bumped version from (g). See CSV header note below.
;;        i) LAST STEP: once everything above has finished (PDF, server
;;        copy, local PDF copy, local version bump, log row written),
;;        vid_publish_pipeline.py is launched in the background (hidden
;;        window, AutoCAD is NOT blocked) so the new log row is uploaded
;;        to Telegram. It is only launched after the log row was written
;;        successfully; if the publish stopped earlier (version already on
;;        server, copy failed, ...) the pipeline is NOT triggered.
;;
;;   CSV COLUMNS (in order):
;;     timestamp, computer_name, artistname, code, filename, srcpath,
;;     destdwg, destpdf, commands, latestversion, publishedtelebot, remarks
;;   Header row (written automatically when publish_log.csv does not exist yet):
;;     DATE TIME,PC NAME,ARTIST NAME,YARD CODE NAME,FILE NAME,LOCAL DWG,
;;     SERVER DWG,SERVER PDF,COMMANDS,VERSION,TELEGRAM,REMARKS
;;   Column letters: A=DATE TIME ... J=VERSION, K=TELEGRAM, L=REMARKS.
;;   COLUMN K (TELEGRAM) is the telegram status of THAT row: this script writes
;;   "teleNotPublished" there; the Python pipeline/bot must change K -- and
;;   only K -- to "telePublished" on the SAME row once it has uploaded it.
;;   "remarks" is the 12th and LAST column. This script NEVER fills it: every
;;   row it writes ends with a trailing comma and an EMPTY remarks field
;;   (reserved for manual / bot use). Older 11-column rows are left as they
;;   are; readers must tolerate rows with or without the 12th field.
;;   "artistname" is whatever was selected in the Artist dropdown.
;;   "publishedtelebot" starts as "teleNotPublished" on every row this
;;   script writes; the separate Telegram bot script watches for that exact
;;   value, uploads the row, then rewrites it to "telePublished" so it
;;   is never re-uploaded.
;;
;;   Example (local file at v02, server already has v01 published),
;;   with "Generate PDF" checked:
;;     BEFORE:
;;       local : ...\yd_TDN_tiruparankundram\v02\yd_TDN_tiruparankundram_v02.dwg
;;       server: ...\yd_TDN_tiruparankundram\v01\yd_TDN_tiruparankundram_v01.dwg
;;     AFTER:
;;       local : ...\yd_TDN_tiruparankundram\v02\yd_TDN_tiruparankundram_v02.dwg  (unchanged)
;;               ...\yd_TDN_tiruparankundram\v02\yd_TDN_tiruparankundram_v02.pdf  (new local copy)
;;               ...\yd_TDN_tiruparankundram\v03\yd_TDN_tiruparankundram_v03.dwg  (new, now the active/open drawing)
;;       server: ...\yd_TDN_tiruparankundram\v02\yd_TDN_tiruparankundram_v02.dwg
;;               ...\yd_TDN_tiruparankundram\v02\yd_TDN_tiruparankundram_v02.pdf
;;
;; INSTALL:
;;   1) Put VidPublish.lsp and VidPublish.dcl in the SAME folder.
;;   2) In AutoCAD:  APPLOAD  ->  add VidPublish.lsp  -> Load
;;      (or drag-and-drop the .lsp onto the AutoCAD window)
;;   3) Type:  VIDPUBLISH   (alias: PUBLISH)
;;
;; ======================================================================
;; SETTINGS  (edit here only)
;; In AutoLISP strings every backslash must be doubled ( \\ ).
;; No trailing slash at the end of a path.
;; ======================================================================

;; ---------------------------- PATHS -----------------------------------
;; All paths for this tool are set right here -- nothing is read from any
;; other file (no JSON, no config).

;; Yard root: where the yard folders (w1\..., w2\...) are published to.
(setq *VP-ROOT-PATH*   "\\\\Desktop-igi8ekn\\vid_20d\\share\\madurai_Railway\\section")

;; Telegram upload pipeline, launched as the very last step of a publish.
;; *VP-PYTHON-EXE* is the command used to start Python: "python" if it is
;; on PATH, "py" for the Windows launcher, or a full path such as
;; "C:\\Python314\\python.exe" (doubled backslashes, as in any LISP string).
;; Set *VP-PIPELINE-SHOW-WINDOW* to T to see the console window (useful
;; while debugging); nil runs it hidden in the background.
(setq *VP-PIPELINE-PATH*
  "\\\\Desktop-igi8ekn\\vid_20d\\share\\asset_Library\\dev\\vidPublish\\vid_publish_cad_pipeline.py")
(setq *VP-PYTHON-EXE* "python")
(setq *VP-PIPELINE-SHOW-WINDOW* nil)

;; Publish log (CSV) written after every publish.
(setq *VP-LOG-FOLDER*  "\\\\Desktop-igi8ekn\\vid_20d\\share\\asset_Library\\dev\\vidPublish\\db")
(setq *VP-LOG-PATH*    (strcat *VP-LOG-FOLDER* "\\publish_log.csv"))

;;--------------------------------------------------------------------------
;; ARTIST LIST
;; Populates the "Artist" dropdown (popup_list) in VidPublish.dcl.
;; Add/remove names here to change what shows up in the dialog -- order
;; here is the order they appear in the dropdown.
;;--------------------------------------------------------------------------
(setq *VP-ARTIST-LIST* (list "Ananthan" "Divya" "Rishi" ))

;; Ensure Visual LISP ActiveX extensions (vla-*, vlax-*) are loaded.
;; Modern AutoCAD auto-loads these, but this is cheap, standard
;; insurance across different install/profile configurations.
(vl-load-com)

;; Shared install folder of VidPublish.lsp / VidPublish.dcl, used only as a
;; last-resort place to look for the .dcl (see vp-show-dialog).
(setq *VP-DCL-FALLBACK-DIR*
  "\\\\Desktop-igi8ekn\\vid_20d\\share\\asset_Library\\dev\\vidPublish\\software\\autocad")

;; Full path to this LSP's own folder, resolved at load time, so the
;; .dcl file is found regardless of where the user's support paths point.
(setq *VP-LSP-DIR* (if (findfile "VidPublish.lsp")
                       (vl-filename-directory (findfile "VidPublish.lsp"))
                       "."))

;;--------------------------------------------------------------------------
;; YARD MAP
;; Each entry: (CODE . SEGMENT-SUBPATH)
;; SEGMENT-SUBPATH is everything between ROOT and "\dwg\yard\yd_<code>"
;; The full target folder is built as:
;;   ROOT \ SEGMENT \ dwg \ yard \ yd_<CODE> \ v<NN>   (version folder)
;;--------------------------------------------------------------------------
(setq *VP-YARD-MAP*
  (list
    (cons "NRK_naraikkinar" "w1\\MEJ_TEN")
    (cons "GDN_gangaikondan" "w1\\MEJ_TEN")
    (cons "TAY_talaiyuthu" "w1\\MEJ_TEN")
    (cons "TEN_tirunelveli" "w1\\MEJ_TEN")

    (cons "PCO_palayankottai" "w1\\TEN_TCN")
    (cons "SDNR_seydunganallur" "w1\\TEN_TCN")
    (cons "TTQ_thathankulam" "w1\\TEN_TCN")
    (cons "SVV_srivaikuntam" "w1\\TEN_TCN")
    (cons "AWT_alwarTirunagari" "w1\\TEN_TCN")
    (cons "NZT_nazareth" "w1\\TEN_TCN")
    (cons "KCHV_kachchanavilai" "w1\\TEN_TCN")
    (cons "KZB_kurumbur" "w1\\TEN_TCN")
    (cons "ANY_arumuganeri" "w1\\TEN_TCN")
    (cons "KZY_kayalpattinam" "w1\\TEN_TCN")
    (cons "TCN_tiruchendur" "w1\\TEN_TCN")

    (cons "TYT_tirunelveliTown" "w1\\TEN_TSI")
    (cons "PEA_pettai" "w1\\TEN_TSI")
    (cons "SMD_cheranmahadevi" "w1\\TEN_TSI")
    (cons "KARK_karraikkurichichi" "w1\\TEN_TSI")
    (cons "VVR_viravanallur" "w1\\TEN_TSI")
    (cons "KIC_kallidaikurichi" "w1\\TEN_TSI")
    (cons "ASD_ambasamudram" "w1\\TEN_TSI")
    (cons "KIB_kizhaAmbur" "w1\\TEN_TSI")
    (cons "AZK_azhwarkurichi" "w1\\TEN_TSI")
    (cons "RVS_ravanasamudram" "w1\\TEN_TSI")
    (cons "KKY_kilakadaiyam" "w1\\TEN_TSI")

    (cons "NLL_nalli" "w2\\NLL_TN")
    (cons "CVP_kovilpatti" "w2\\NLL_TN")
    (cons "KPM_kumarapuram" "w2\\NLL_TN")
    (cons "KDU_kadambur" "w2\\NLL_TN")
    (cons "MEJ_vanchiManiyachchi" "w2\\NLL_TN")
    (cons "KLPM_kailasapuram" "w2\\NLL_TN")
    (cons "TIP_tattapparai" "w2\\NLL_TN")
    (cons "MVN_milavittan" "w2\\NLL_TN")
    (cons "TME_tutimelur" "w2\\NLL_TN")
    (cons "TN_tuticorin" "w2\\NLL_TN")
    (cons "MMDR_melmarudur" "w2\\NLL_TN")

    (cons "TDN_tiruparankundram" "w3\\MDU_NLL")
    (cons "TMQ_tirumangalam" "w3\\MDU_NLL")
    (cons "KGD_kalligudi" "w3\\MDU_NLL")
    (cons "VPT_virudunagar" "w3\\MDU_NLL")
    (cons "TY_tulikapatti" "w3\\MDU_NLL")
    (cons "SRT_sattur" "w3\\MDU_NLL")

    (cons "TTL_tiruttangal" "w3\\VPT_SNKL")
    (cons "SVKS_sivakasi" "w3\\VPT_SNKL")
    (cons "SVPR_srivilliputtur" "w3\\VPT_SNKL")
    (cons "RJPM_rajapalayam" "w3\\VPT_SNKL")

    (cons "SNKL_sankarankovil" "w4\\SNKL_EDP")
    (cons "PBKS_pambakovilShandy" "w4\\SNKL_EDP")
    (cons "KDNL_kadayanallur" "w4\\SNKL_EDP")
    (cons "TSI_tenkasi" "w4\\SNKL_EDP")
    (cons "SCT_sengottai" "w4\\SNKL_EDP")
    (cons "BJM_bhagavathipuram" "w4\\SNKL_EDP")
    (cons "AYV_aryankavu" "w4\\SNKL_EDP")
    (cons "AYVN_newAryankavu" "w4\\SNKL_EDP")
    (cons "EDN_edapalayam" "w4\\SNKL_EDP")

    (cons "KTHY_kalthurutty" "w4\\EDP_QLN")
    (cons "TML_tenmalai" "w4\\EDP_QLN")
    (cons "OKL_ottakkal" "w4\\EDP_QLN")
    (cons "EDN_edaman" "w4\\EDP_QLN")
    (cons "PUU_punalur" "w4\\EDP_QLN")
    (cons "AVS_avaneeswarem" "w4\\EDP_QLN")
    (cons "KIF_kuri" "w4\\EDP_QLN")
    (cons "KKZ_kottarakkara" "w4\\EDP_QLN")
    (cons "EKN_ezhukone" "w4\\EDP_QLN")
    (cons "KFV_kundaraEast" "w4\\EDP_QLN")
    (cons "KUV_kundara" "w4\\EDP_QLN")
    (cons "CTPE_chandanattop" "w4\\EDP_QLN")
    (cons "KLQ_kilikollur" "w4\\EDP_QLN")
    (cons "QLN_kollam" "w4\\EDP_QLN")
  )
)

;;==========================================================================
;; PATH HELPERS
;; Handle the "different users, different path styles" problem:
;;   - Local drive      J:\Folder\Sub
;;   - Mapped drive      Z:\Folder\Sub
;;   - UNC server path   \\server\share\Folder\Sub
;;   - Forward slashes   J:/Folder/Sub   (rare, but some users paste these)
;; All internal joins use backslash, and we normalize/clean any path
;; that gets built or read so double slashes, trailing slashes, or
;; forward slashes do not break file/dir operations.
;;==========================================================================

;; Replace every forward slash with a backslash (Windows-safe)
(defun vp-slashfix (s / out i ch)
  (setq out "")
  (setq i 0)
  (while (< i (strlen s))
    (setq ch (substr s (1+ i) 1))
    (setq out (strcat out (if (= ch "/") "\\" ch)))
    (setq i (1+ i))
  )
  out
)

;; Collapse any run of multiple backslashes into one, EXCEPT a leading
;; "\\\\" (UNC server prefix), which must be preserved.
(defun vp-collapse-slashes (s / unc body out i ch prev)
  (setq unc "")
  (setq body s)
  (if (and (>= (strlen s) 2) (= (substr s 1 2) "\\\\"))
    (progn
      (setq unc "\\\\")
      (setq body (substr s 3))
    )
  )
  (setq out "")
  (setq prev "")
  (setq i 0)
  (while (< i (strlen body))
    (setq ch (substr body (1+ i) 1))
    (if (not (and (= ch "\\") (= prev "\\")))
      (setq out (strcat out ch))
    )
    (setq prev ch)
    (setq i (1+ i))
  )
  (strcat unc out)
)

;; Strip a single trailing slash/backslash, if present
(defun vp-strip-trailing-slash (s)
  (if (and (> (strlen s) 0)
           (member (substr s (strlen s) 1) '("\\" "/")))
    (substr s 1 (1- (strlen s)))
    s
  )
)

;; Full normalize: fix slash direction, collapse doubles, strip trailing
(defun vp-normpath (s)
  (vp-strip-trailing-slash (vp-collapse-slashes (vp-slashfix s)))
)

;; Join path parts with a single backslash between them, normalizing
;; the final result. Accepts any number of string args via a list.
(defun vp-pathjoin (parts / result)
  (setq result "")
  (foreach p parts
    (setq p (vp-strip-trailing-slash (vp-slashfix p)))
    (setq result
      (if (= result "")
        p
        (strcat result "\\" p)
      )
    )
  )
  (vp-normpath result)
)

;;==========================================================================
;; FOLDER / FILE HELPERS
;;==========================================================================

;; Recursively create a folder path, one level at a time.
;; vl-mkdir fails if the parent doesn't exist, so we walk up from root.
(defun vp-mkdir-p (path / parts cur unc rest)
  (setq path (vp-normpath path))
  (setq unc "")
  (setq rest path)
  (if (= (substr path 1 2) "\\\\")
    (progn
      (setq unc "\\\\")
      (setq rest (substr path 3))
    )
  )
  (setq parts (vp-strsplit rest "\\"))
  (setq cur unc)
  (foreach p parts
    (setq cur (if (= cur "") p
                (if (= cur unc) (strcat cur p) (strcat cur "\\" p))))
    (if (and (/= cur "") (not (vl-file-directory-p cur)))
      (vl-mkdir cur)
    )
  )
  (vl-file-directory-p path)
)

;; Simple string split on a single-character delimiter
(defun vp-strsplit (s delim / result pos)
  (setq result '())
  (while (setq pos (vl-string-search delim s))
    (setq result (append result (list (substr s 1 pos))))
    (setq s (substr s (+ pos 1 (strlen delim))))
  )
  (if (/= s "") (setq result (append result (list s))))
  result
)

;; Build YYYYMMDD_HHMMSS from the current system clock
(defun vp-timestamp ( / dt y mo d h mi sec)
  (setq dt (rtos (getvar "CDATE") 2 6))
  (setq y  (substr dt 1 4))
  (setq mo (substr dt 5 2))
  (setq d  (substr dt 7 2))
  (setq h  (substr dt 10 2))
  (setq mi (substr dt 12 2))
  (setq sec (substr dt 14 2))
  (strcat y mo d "_" h mi sec)
)

;; Return file name without path, WITHOUT extension
(defun vp-filename-only (fullpath)
  (vl-filename-base fullpath)
)

;; Return file extension WITH the leading dot, e.g. ".dwg"
(defun vp-fileext-only (fullpath)
  (vl-filename-extension fullpath)
)

;; Sanitize a string for safe use as a Windows folder/file name:
;; strip characters that are illegal in Windows file/folder names
;; ( \ / : * ? " < > | ) -- the timestamp+filename string should
;; already be clean in normal use, but this guards against odd
;; original DWG names (e.g. containing a colon or question mark).
(defun vp-sanitize-name (s / out i ch bad)
  (setq bad '("\\" "/" ":" "*" "?" "\"" "<" ">" "|"))
  (setq out "")
  (setq i 0)
  (while (< i (strlen s))
    (setq ch (substr s (1+ i) 1))
    (setq out (strcat out (if (member ch bad) "_" ch)))
    (setq i (1+ i))
  )
  out
)

;; Append one line of text to a log file, creating it if needed
(defun vp-log-line (logpath line / f)
  (setq f (open logpath "a"))
  (if f
    (progn
      (write-line line f)
      (close f)
      T
    )
    nil
  )
)

;;==========================================================================
;; FILENAME VERSION HANDLING ("New Version" feature)
;;
;; Expected filename grammar (base name, no extension):
;;   yd_<yardCode>_<yardName>[_v<NN>][_<command>]
;; i.e. an optional "_v<NN>" version tag, optionally followed by an
;; optional "_<command>" suffix tag. Examples:
;;   yd_GDN_gangaikondan_v01_ROR  -> prefix="yd_GDN_gangaikondan"
;;                                   version=1  suffix="_ROR"
;;   yd_NRK_naraikkinar_v02       -> prefix="yd_NRK_naraikkinar"
;;                                   version=2  suffix=""
;;   yd_KARK_karraikkurichichi_v04 -> version=4, suffix=""
;;   yd_NRK_naraikkinar           -> no version found at all
;;                                   (prefix is the whole name, version=nil)
;;==========================================================================

;; Find the LAST occurrence of the pattern "_v" followed by 2+ digits
;; in a base filename, where what follows those digits is either the
;; end of the string or another "_" (so "_v01_ROR" matches "_v01", and
;; "_v01x" does NOT match -- avoids false positives on names that just
;; happen to contain "v01" as part of a word).
;; Returns a list (PREFIX VERSIONNUM SUFFIX) if found, else nil.
;;   PREFIX     = everything before "_v<NN>"
;;   VERSIONNUM = the integer version number (e.g. 1, 2, 4)
;;   SUFFIX     = everything from the "_" after the digits onward,
;;                or "" if nothing follows (e.g. "_ROR" or "")
(defun vp-parse-version (basename / i found startpos digitstart digitend
                                     j ch prefix numstr suffix)
  (setq found nil)
  (setq i (1- (strlen basename)))  ;; scan from the end -> find LAST match
  (while (and (not found) (> i 0))
    (if (and (= (substr basename i 2) "_v")
             (vp-is-digit (substr basename (+ i 2) 1)))
      (progn
        (setq startpos i)            ;; position of "_" in "_v"
        (setq digitstart (+ i 2))
        (setq j digitstart)
        (while (and (<= j (strlen basename))
                     (vp-is-digit (substr basename j 1)))
          (setq j (1+ j))
        )
        (setq digitend (1- j))       ;; last digit position
        ;; require at least 2 digits, and what follows is either end
        ;; of string or "_"
        (if (and (>= (- digitend digitstart) 1)
                 (or (> j (strlen basename))
                     (= (substr basename j 1) "_")))
          (progn
            (setq found T)
            (setq prefix (substr basename 1 (1- startpos)))
            (setq numstr (substr basename digitstart (1+ (- digitend digitstart))))
            (setq suffix (if (> j (strlen basename))
                            ""
                            (substr basename j)))
          )
        )
      )
    )
    (if (not found) (setq i (1- i)))
  )
  (if found
    (list prefix (atoi numstr) suffix)
    nil
  )
)

;; T if a 1-character string is an ASCII digit 0-9
(defun vp-is-digit (ch)
  (and (>= (ascii ch) 48) (<= (ascii ch) 57))
)

;; Format an integer version number as "v01", "v02", ... "v10", "v123"
;; (always at least 2 digits, matching the examples given).
(defun vp-format-version (n / s)
  (setq s (itoa n))
  (if (< (strlen s) 2)
    (setq s (strcat "0" s))
  )
  (strcat "v" s)
)

;; Given a base filename (no extension), return the NEW base filename
;; with its version bumped by 1, or "_v01" inserted if it had none.
;; Also returns the new version NUMBER as the second list item, so
;; callers don't have to re-parse the result.
;; Returns (list NEWBASENAME NEWVERSIONNUM).
(defun vp-bump-filename-version (basename / parsed prefix oldver suffix newver)
  (setq parsed (vp-parse-version basename))
  (if parsed
    (progn
      (setq prefix (nth 0 parsed))
      (setq oldver (nth 1 parsed))
      (setq suffix (nth 2 parsed))
      (setq newver (1+ oldver))
      (list (strcat prefix "_" (vp-format-version newver) suffix) newver)
    )
    ;; no existing version tag -- insert _v01 at the end
    (list (strcat basename "_" (vp-format-version 1)) 1)
  )
)

;; Like vp-parse-version, but NEVER returns nil: if the basename has no
;; "_vNN" tag, it is treated as an IMPLICIT version 1 (prefix = the
;; whole basename, suffix = ""). This is the "current version" used to
;; pick the SERVER destination folder -- so an as-yet-unversioned file
;; still publishes into a well-defined "v01" folder rather than needing
;; a special case everywhere else.
;; Returns (list PREFIX VER SUFFIX), VER always >= 1.
(defun vp-effective-version (basename / parsed)
  (setq parsed (vp-parse-version basename))
  (if parsed
    parsed
    (list basename 1 "")
  )
)

;; T if a bare folder NAME (no path) matches "v" + 2 or more digits,
;; case-insensitive (e.g. "v02", "V123"). Used to recognize whether a
;; LOCAL drawing is already sitting inside a version folder.
(defun vp-is-version-foldername (name / up digits ok i ch)
  (setq up (strcase name))
  (setq ok (and (>= (strlen up) 2) (= (substr up 1 1) "V")))
  (if ok
    (progn
      (setq digits (substr up 2))
      (setq ok (> (strlen digits) 0))
      (setq i 1)
      (while (and ok (<= i (strlen digits)))
        (setq ch (substr digits i 1))
        (if (not (vp-is-digit ch)) (setq ok nil))
        (setq i (1+ i))
      )
    )
  )
  ok
)

;; Last path component (folder or file name) of a path string.
(defun vp-last-path-component (path / parts)
  (setq parts (vp-strsplit (vp-normpath path) "\\"))
  (if parts (nth (1- (length parts)) parts) path)
)

;; Parent folder of a folder path (UNC-safe: preserves a leading
;; "\\server" prefix). If PATH has nothing to strip, PATH is returned
;; unchanged rather than guessing.
(defun vp-parent-folder (path / norm unc rest parts n)
  (setq norm (vp-normpath path))
  (setq unc "")
  (setq rest norm)
  (if (= (substr norm 1 2) "\\\\")
    (progn (setq unc "\\\\") (setq rest (substr norm 3)))
  )
  (setq parts (vp-strsplit rest "\\"))
  (setq n (length parts))
  (if (<= n 1)
    norm
    (progn
      (setq parts (reverse (cdr (reverse parts))))
      (strcat unc (vp-joinparts parts))
    )
  )
)

;; Join a list of path-part strings with a single backslash.
(defun vp-joinparts (parts / result)
  (setq result "")
  (foreach p parts
    (setq result (if (= result "") p (strcat result "\\" p)))
  )
  result
)

;; Where should the LOCAL drawing's NEXT version folder be created?
;;   - If the drawing's CURRENT folder is already a "vNN" folder
;;     (e.g. "...\yd_CODE\v02"), the next version folder is a SIBLING
;;     of it (i.e. under "...\yd_CODE\").
;;   - Otherwise (first time this file is being versioned locally),
;;     the next version folder is nested INSIDE the current folder,
;;     so from then on the current-folder-is-a-"vNN"-folder rule kicks
;;     in for every publish after that.
(defun vp-local-version-parent (dir)
  (if (vp-is-version-foldername (vp-last-path-component dir))
    (vp-parent-folder dir)
    dir
  )
)

;; Return this PC's SYSTEM/COMPUTER name (i.e. what Windows shows as
;; %COMPUTERNAME%, or "hostname" in a terminal) -- NOT the AutoCAD/
;; Windows login (user) name, which is a different value returned by
;; the LOGINNAME system variable.
;;
;; AutoLISP has no built-in system variable for the computer name, so
;; this reads it via the Windows Script Host COM object, which is
;; always available on any Windows install (no extra setup needed).
;; Falls back to the %COMPUTERNAME% environment variable, then to
;; LOGINNAME, if WSH is ever unavailable, so the log field is never
;; left blank.
(defun vp-computer-name ( / wsh result name)
  (setq name nil)
  (setq result (vl-catch-all-apply
    '(lambda ()
       (setq wsh (vlax-create-object "WScript.Network"))
       (setq name (vlax-get-property wsh "ComputerName"))
       (vlax-release-object wsh)
     )
    '()
  ))
  (if (or (vl-catch-all-error-p result) (not name) (= name ""))
    (progn
      (setq name (getenv "COMPUTERNAME"))
      (if (or (not name) (= name ""))
        (setq name (getvar "LOGINNAME"))
      )
    )
  )
  name
)

;; Make a free-text string safe to store as ONE CSV field: strip commas
;; and newlines (replace with space) and turn double quotes into single
;; quotes, since the log format here is plain comma-separated with no
;; quoting (one stray " would make a CSV reader swallow the following
;; commas/lines and merge several log rows into one). Keeps the publish_log.csv simple and
;; readable in any text editor or Excel without needing quoted-field
;; parsing for this column.
(defun vp-csv-safe (s / out i ch)
  (if (not s) (setq s ""))
  (setq out "")
  (setq i 0)
  (while (< i (strlen s))
    (setq ch (substr s (1+ i) 1))
    (setq out (strcat out
      (cond ((= ch ",") " ")
            ((= ch "\n") " ")
            ((= ch "\r") "")
            ((= ch "\"") "'")
            (T ch))))
    (setq i (1+ i))
  )
  out
)

;;==========================================================================
;; DWGPROPS / SUMMARY INFO HELPER
;; The AutoCAD command DWGPROPS just opens the Drawing Properties dialog
;; (the same dialog you reach via File > Drawing Properties). It cannot
;; be driven non-interactively/silently from a script -- there is no
;; command-line way to "type text into" that dialog's Comments field
;; without it popping up and waiting for clicks.
;;
;; The reliable, silent equivalent is to write directly to the drawing's
;; SummaryInfo COM object, which is the SAME data DWGPROPS reads and
;; writes. Setting SummaryInfo.Comments here updates exactly the field
;; you'd see under DWGPROPS -> Summary tab -> Comments, with no dialog
;; popping up and no extra clicks needed.
;;==========================================================================
(defun vp-set-dwgprops-comments (docobj text / si result)
  (setq result (vl-catch-all-apply
    '(lambda ()
       (setq si (vla-get-SummaryInfo docobj))
       (vla-put-Comments si text)
     )
    '()
  ))
  (not (vl-catch-all-error-p result))
)

;;==========================================================================
;; PDF PLOT HELPER
;; Plots the CURRENT (active) layout to a PDF file using the documented
;; AutoCAD ActiveX pattern:
;;   1) Ensure BACKGROUNDPLOT = 0 so the plot runs in the foreground and
;;      finishes (with a result we can check) before this function
;;      returns -- otherwise PlotToFile queues the job and returns
;;      immediately, before the PDF file actually exists.
;;   2) Set the active layout's ConfigName to the PDF driver, UNLESS it
;;      already has a real device assigned, in which case that device
;;      is left alone (so existing paper size / plot style on that
;;      layout is preserved).
;;   3) Call Plot.PlotToFile(pdfpath) -- this plots whichever layout is
;;      currently active, exactly as documented by Autodesk.
;;
;; WHY NOT THE OLD COMMAND-LINE -PLOT SCRIPT:
;; The classic "(command "_.-PLOT" "answer1" "answer2" ...)" approach
;; depends on an EXACT, fixed number of prompts appearing in order. That
;; sequence changes depending on whether the layout already has a
;; plotter assigned and other per-layout settings, so a hardcoded
;; answer list reliably goes out of sync (this is what produced the
;; "Unknown command Y" / "Unknown command PDF" errors -- the script's
;; answers landed on the wrong prompts partway through).
;;
;; PlotToFile has no prompts to get out of sync with -- it is the
;; documented, version-stable way to do unattended/silent plotting from
;; AutoLISP/VBA. Reference: Autodesk ActiveX Reference, "PlotToFile
;; Method (ActiveX)".
;;==========================================================================
;;==========================================================================
;; LAYOUT DISCOVERY
;; Finds the drawing's paper-space layouts, excluding "Model". Per spec:
;;   - exactly 1 paper-space layout -> that's the one to plot
;;   - 0 or 2+ paper-space layouts  -> ambiguous, caller should skip PDF
;;     and warn rather than guess
;; Returns the layout NAME (string) if exactly one is found, else nil.
;;==========================================================================
(defun vp-find-single-paper-layout (docobj / layouts lyt names nm count)
  (setq layouts (vla-get-Layouts docobj))
  (setq names '())
  (vlax-for lyt layouts
    (setq nm (vla-get-Name lyt))
    (if (/= (strcase nm) "MODEL")
      (setq names (append names (list nm)))
    )
  )
  (setq count (length names))
  (cond
    ((= count 1) (car names))
    (T nil)   ;; 0 or 2+ -> caller handles the warning with the count
  )
)

;; Same as above but also returns the count, for clearer warning messages.
(defun vp-count-paper-layouts (docobj / layouts lyt nm count)
  (setq layouts (vla-get-Layouts docobj))
  (setq count 0)
  (vlax-for lyt layouts
    (setq nm (vla-get-Name lyt))
    (if (/= (strcase nm) "MODEL")
      (setq count (1+ count))
    )
  )
  count
)

;;==========================================================================
;; PDF PLOT HELPER
;; Plots a SPECIFIC named paper-space layout (the single non-Model layout
;; found by vp-find-single-paper-layout) to PDF via the documented
;; AutoCAD ActiveX Plot.PlotToFile method:
;;   1) Switch the active layout to the target layout by name (ActiveX
;;      ActiveLayout assignment), so PlotToFile plots THAT layout
;;      regardless of which tab the user had open when they ran
;;      VIDPUBLISH.
;;   2) Use whatever plot device/page setup the layout ALREADY has --
;;      nothing about the layout's existing plot configuration is
;;      touched (device, paper size, plot style table, plot area, all
;;      exactly as the user already set them up). If the layout has NO
;;      device assigned at all, the PDF is SKIPPED with a warning
;;      rather than forcing any device onto the layout.
;;   3) Plot.PlotToFile(pdfpath) writes the PDF using that existing
;;      configuration, just redirected to a file instead of a printer.
;;   4) The original active layout is restored afterward, so the user's
;;      drawing view doesn't change as a side effect of publishing.
;;
;; Returns T on success, nil on failure or if skipped (no device).
;;==========================================================================
(defun vp-plot-current-to-pdf (pdfpath layoutname / acadobj docobj
                                          targetlayout plotobj result
                                          success origactivelayout
                                          origbackgroundplot existingdevice
                                          alreadyexplained)
  (setq acadobj (vlax-get-acad-object))
  (setq docobj (vla-get-ActiveDocument acadobj))
  (setq plotobj (vla-get-Plot docobj))
  (setq success nil)
  (setq alreadyexplained nil)

  ;; Force foreground plotting so PlotToFile blocks until the file is
  ;; actually written, and restore the user's original setting after.
  (setq origbackgroundplot (getvar "BACKGROUNDPLOT"))
  (setvar "BACKGROUNDPLOT" 0)

  ;; Remember whatever layout tab the user currently has open, so we
  ;; can switch back to it after publishing -- this tool should not
  ;; leave the user's drawing sitting on a different tab than before.
  (setq origactivelayout (vla-get-ActiveLayout docobj))

  (setq result (vl-catch-all-apply
    '(lambda ()
       ;; Switch to the target paper-space layout by name so PlotToFile
       ;; plots THAT layout, not whatever tab happened to be active.
       (setq targetlayout (vla-Item (vla-get-Layouts docobj) layoutname))
       (vla-put-ActiveLayout docobj targetlayout)

       ;; Use whatever plot device the layout already has -- do NOT
       ;; change ConfigName, plot style table, or anything else. If
       ;; there's genuinely no device assigned, skip rather than guess.
       (setq existingdevice (vla-get-ConfigName targetlayout))
       (if (or (not existingdevice) (= existingdevice ""))
         (progn
           (setq alreadyexplained T)
           (princ (strcat "\nVIDPUBLISH WARNING: Layout \"" layoutname
                           "\" has no plot device assigned. Skipping PDF "
                           "rather than forcing one."))
         )
         (progn
           (vla-put-QuietErrorMode plotobj :vlax-true)

           ;; vla-PlotToFile returns T (success) or nil (failure) directly
           ;; in Visual LISP -- no variant unwrapping needed.
           (setq success (vla-PlotToFile plotobj pdfpath))
         )
       )
     )
    '()
  ))

  ;; Always restore the user's original active layout, even on error.
  (vl-catch-all-apply
    '(lambda () (vla-put-ActiveLayout docobj origactivelayout))
    '()
  )

  (setvar "BACKGROUNDPLOT" origbackgroundplot)

  (if (vl-catch-all-error-p result)
    (progn
      (princ (strcat "\nVIDPUBLISH: PDF plot error - "
                      (vl-catch-all-error-message result)))
      nil
    )
    (progn
      ;; The file actually existing on disk is the real proof of
      ;; success -- COM boolean returns can come back as T/nil or
      ;; -1/0 depending on AutoCAD version, so don't rely on `success`
      ;; alone if the file is plainly there.
      (if (findfile pdfpath)
        T
        (progn
          (if (and (not success) (not alreadyexplained))
            (princ "\nVIDPUBLISH: PlotToFile reported failure and no PDF file was found.")
          )
          nil
        )
      )
    )
  )
)



;;==========================================================================
;; TELEGRAM PIPELINE TRIGGER
;; Starts vid_publish_pipeline.py via the Windows Script Host, NOT
;; waiting for it to finish (bWaitOnReturn = false), so AutoCAD stays
;; responsive while the upload runs. Window style 0 = hidden, 1 = normal
;; (see *VP-PIPELINE-SHOW-WINDOW*).
;; Returns T if the process was launched, nil otherwise. Never raises:
;; a failure here must not affect the already-completed publish.
;;==========================================================================
(defun vp-run-pipeline ( / cmd sh result)
  (cond
    ((not (findfile *VP-PIPELINE-PATH*))
     (princ (strcat "\nVIDPUBLISH WARNING: Telegram pipeline script not found, upload NOT triggered:\n  "
                    *VP-PIPELINE-PATH*))
     nil
    )
    (T
     (setq cmd (strcat "\"" *VP-PYTHON-EXE* "\" \"" *VP-PIPELINE-PATH* "\""))
     (setq result (vl-catch-all-apply
       '(lambda ()
          (setq sh (vlax-create-object "WScript.Shell"))
          (vlax-invoke-method sh "Run" cmd (if *VP-PIPELINE-SHOW-WINDOW* 1 0) :vlax-false)
          (vlax-release-object sh)
        )
       '()
     ))
     (if (vl-catch-all-error-p result)
       (progn
         (princ (strcat "\nVIDPUBLISH WARNING: Could not start Telegram pipeline - "
                        (vl-catch-all-error-message result)))
         nil
       )
       (progn
         (princ "\nVIDPUBLISH: Telegram pipeline started in background (vid_publish_pipeline.py).")
         T
       )
     )
    )
  )
)

;;==========================================================================
;; DIALOG
;; Returns a list (CODE ARTISTNAME CMDTEXT PDFFLAG) on OK, or nil if
;; cancelled.
;;   CODE       - selected yard code string
;;   ARTISTNAME - selected name from the Artist dropdown (e.g. "Dinesh")
;;   CMDTEXT    - text typed into the Commands box (string, may be empty)
;;   PDFFLAG    - T if "Generate PDF" was checked, nil otherwise
;;
;; LIST GROUPING: the list box shows segment names (MEJ_TEN, TEN_TCN,
;; ...) as plain-text header rows, with that segment's yard codes
;; indented underneath -- one flat list, visually grouped, no separate
;; picker step. Header rows are not real yards, so they are treated as
;; non-selectable: clicking or arrowing onto a header automatically
;; jumps to the nearest real yard row instead of leaving a header
;; "selected".
;;
;; *VP-DISPLAY-ROWS* is built once per dialog call as a list of
;; (CODE . DISPLAY-STRING) pairs, in list-box row order. CODE is nil
;; for header rows. This keeps "what row index am I on" and "what code
;; does that map to" in exact sync with what's actually drawn on screen.
;;
;; NOTE ON GLOBALS: action_tile callback strings run as standalone
;; AutoLISP expressions, NOT inside vp-show-dialog's local scope. So we
;; stage results in global *VP-TMP-*  variables, then copy them into
;; proper locals immediately after start_dialog returns. This is the
;; standard, reliable DCL pattern.
;;==========================================================================
(defun vp-show-dialog (currentfilename / dcl_id result sel dclfile
                                          firstyardrow detectedcode
                                          detectedrow initialrow)
  (setq dclfile (vp-pathjoin (list *VP-LSP-DIR* "VidPublish.dcl")))
  ;; If the LSP was loaded from a folder that is not on AutoCAD's support
  ;; path, *VP-LSP-DIR* is "." and the line above misses. Try the support
  ;; path, then the shared install folder, before giving up.
  (if (not (findfile dclfile))
    (cond
      ((findfile "VidPublish.dcl")
       (setq dclfile (findfile "VidPublish.dcl")))
      ((findfile (vp-pathjoin (list *VP-DCL-FALLBACK-DIR* "VidPublish.dcl")))
       (setq dclfile (findfile (vp-pathjoin (list *VP-DCL-FALLBACK-DIR* "VidPublish.dcl")))))
    )
  )
  (if (not (findfile dclfile))
    (progn
      (princ (strcat "\nERROR: Cannot find VidPublish.dcl at: " dclfile))
      (princ "\nMake sure VidPublish.dcl is in the same folder as VidPublish.lsp.")
      nil
    )
    (progn
      (setq *VP-DISPLAY-ROWS* (vp-build-display-rows))
      (setq firstyardrow (vp-nearest-yard-row 0))

      ;; Try to auto-detect the yard from the current filename. This is
      ;; only ever a convenience pre-pick -- if detection fails or is
      ;; ambiguous, fall back to the same "first yard in the list"
      ;; behavior as before. The user can change the selection freely
      ;; either way, before clicking OK.
      (setq detectedcode
        (if (and currentfilename (/= currentfilename ""))
          (vp-detect-yard-from-filename currentfilename)
          nil
        )
      )
      (setq detectedrow (if detectedcode (vp-row-index-for-code detectedcode) nil))
      (setq initialrow (if detectedrow detectedrow firstyardrow))

      (setq dcl_id (load_dialog dclfile))
      (if (not (new_dialog "vid_publish_dlg" dcl_id))
        (progn
          (princ "\nERROR: Could not load dialog vid_publish_dlg.")
          (unload_dialog dcl_id)
          nil
        )
        (progn
          (setq *VP-TMP-CODE* nil)
          (setq *VP-TMP-ARTIST* (car *VP-ARTIST-LIST*))
          (setq *VP-TMP-CMDTEXT* "")
          (setq *VP-TMP-PDFFLAG* T)
          (setq *VP-TMP-CURFILENAME* currentfilename)

          (start_list "artist_list")
          (mapcar 'add_list *VP-ARTIST-LIST*)
          (end_list)
          (set_tile "artist_list" "0")

          (start_list "yard_list")
          (mapcar 'add_list (mapcar 'cdr *VP-DISPLAY-ROWS*))
          (end_list)
          (set_tile "yard_list" (itoa initialrow))
          (set_tile "target_preview"
            (strcat (if detectedrow "Target [auto]:  " "Target:  ")
                    (vp-preview-path (vp-row-code initialrow) currentfilename)))
          (set_tile "pdf_check" "1")
          (set_tile "cmd_text" "")

          (action_tile "yard_list" "(vp-update-preview)")
          (action_tile "accept" "(vp-on-accept)")
          (action_tile "cancel" "(done_dialog 0)")

          (setq sel (start_dialog))
          (unload_dialog dcl_id)
          (if (= sel 1)
            (list *VP-TMP-CODE* *VP-TMP-ARTIST* *VP-TMP-CMDTEXT* *VP-TMP-PDFFLAG*)
            nil
          )
        )
      )
    )
  )
)

;;==========================================================================
;; YARD AUTO-DETECTION FROM FILENAME
;; Given the current drawing's filename (no path, no extension), try to
;; guess which yard it belongs to, so the dialog can pre-select it. The
;; user can always change the selection before clicking OK -- this is
;; just a convenience pre-pick, never a hard lock-in.
;;
;; Matching tiers, in priority order (higher tier wins outright; ties
;; WITHIN a tier mean "ambiguous" and no auto-pick is made at all):
;;   Tier 1 - FULL CODE match: filename contains "<CODE>_<name>" exactly
;;            as it appears in the map, e.g. "TIP_tattapparai"
;;   Tier 2 - CODE-ONLY match: filename contains the short code alone,
;;            bounded so "TIP" doesn't false-match inside "TIPTOP" --
;;            bounded by start/end of string or a non-alphanumeric char
;;            (underscore, dash, dot, etc.) on both sides
;;   Tier 3 - NAME-ONLY match: filename contains the yard's name part
;;            alone (e.g. "tattapparai"), same boundary rule
;; All matching is case-insensitive.
;;
;; Returns the matched CODE string, or nil if no match / ambiguous.
;;==========================================================================
(defun vp-detect-yard-from-filename (filename / upper tier1 tier2 tier3
                                                pair code seg parts shortcode
                                                yardname)
  (setq upper (strcase filename))
  (setq tier1 '())
  (setq tier2 '())
  (setq tier3 '())
  (foreach pair *VP-YARD-MAP*
    (setq code (car pair))
    (setq parts (vp-strsplit code "_"))
    (setq shortcode (car parts))
    (setq yardname (if (> (length parts) 1)
                      (vp-strsplit-rejoin code "_")
                      ""))
    ;; Tier 1: full code as written, e.g. "TIP_TATTAPPARAI"
    (if (vp-string-contains-bounded upper (strcase code))
      (setq tier1 (append tier1 (list code)))
    )
    ;; Tier 2: short code alone, e.g. "TIP"
    (if (vp-string-contains-bounded upper (strcase shortcode))
      (setq tier2 (append tier2 (list code)))
    )
    ;; Tier 3: yard name alone, e.g. "TATTAPPARAI"
    (if (and (/= yardname "") (vp-string-contains-bounded upper (strcase yardname)))
      (setq tier3 (append tier3 (list code)))
    )
  )
  (cond
    ((= (length tier1) 1) (car tier1))
    ((= (length tier2) 1) (car tier2))
    ((= (length tier3) 1) (car tier3))
    (T nil)  ;; no match, or ambiguous (2+ candidates) within the winning tier
  )
)

;; Rejoin everything AFTER the first underscore in a yard code, i.e.
;; the "name" part. "TIP_tattapparai" -> "tattapparai".
;; "KARK_karraikkurichichi" -> "karraikkurichichi" (only splits on the
;; FIRST underscore, so names with no further underscores are returned
;; whole, which matches every entry in *VP-YARD-MAP*).
(defun vp-strsplit-rejoin (code delim / pos)
  (setq pos (vl-string-search delim code))
  (if pos
    (substr code (+ pos 1 (strlen delim)))
    ""
  )
)

;; T if NEEDLE is found inside HAYSTACK at a position bounded by
;; start/end-of-string or a non-alphanumeric character on both sides
;; (so "TIP" matches "yd_TIP_tattapparai" and "TIP-v04" but NOT the
;; "TIP" inside "TIPTOP_yard"). Both args assumed already uppercased.
(defun vp-string-contains-bounded (haystack needle / pos startok endok
                                                       hlen nlen found)
  (setq hlen (strlen haystack))
  (setq nlen (strlen needle))
  (setq pos 1)
  (setq found nil)
  (while (and (not found) (<= pos (1+ (- hlen nlen))))
    (if (= (substr haystack pos nlen) needle)
      (progn
        (setq startok (or (= pos 1)
                           (not (vp-is-alnum (substr haystack (1- pos) 1)))))
        (setq endok (or (= (+ pos nlen) (1+ hlen))
                        (not (vp-is-alnum (substr haystack (+ pos nlen) 1)))))
        (if (and startok endok) (setq found T))
      )
    )
    (setq pos (1+ pos))
  )
  found
)

;; T if a 1-character string is an ASCII letter or digit
(defun vp-is-alnum (ch / code)
  (setq code (ascii ch))
  (or (and (>= code 48) (<= code 57))   ;; 0-9
      (and (>= code 65) (<= code 90))   ;; A-Z (haystack/needle pre-uppercased)
      (and (>= code 97) (<= code 122))  ;; a-z (defensive, in case of mixed case)
  )
)

;; Find the display-row index for a given yard CODE, or nil if not found.
(defun vp-row-index-for-code (code / i n)
  (setq n (length *VP-DISPLAY-ROWS*))
  (setq i 0)
  (while (and (< i n) (not (equal (vp-row-code i) code)))
    (setq i (1+ i))
  )
  (if (< i n) i nil)
)

;; Build the (CODE . DISPLAY-STRING) row list: a header row (CODE=nil)
;; before each segment's yards, yard rows indented underneath.
(defun vp-build-display-rows ( / rows curseg pair code seg)
  (setq rows '())
  (setq curseg nil)
  (foreach pair *VP-YARD-MAP*
    (setq code (car pair))
    (setq seg (cdr pair))
    (if (not (equal seg curseg))
      (progn
        (setq curseg seg)
        (setq rows (append rows (list (cons nil (vp-segment-label seg)))))
      )
    )
    (setq rows (append rows (list (cons code (strcat "     " code)))))
  )
  rows
)

;; Turn a segment subpath like "w1\MEJ_TEN" into a clean header label,
;; e.g. "MEJ_TEN" -- strips the "w#\" prefix so the header matches the
;; plain segment names as given (MEJ_TEN, TEN_TCN, TEN_TSI, NLL_TN,
;; MDU_NLL, VPT_SNKL, SNKL_EDP, EDP_QLN).
(defun vp-segment-label (seg / parts)
  (setq parts (vp-strsplit seg "\\"))
  (if (> (length parts) 1) (nth 1 parts) seg)
)

;; Given a row index, return the nearest row index that is a REAL yard
;; row (code /= nil), searching forward first, then backward. Used both
;; to pick the initial selection and to auto-correct a click/arrow onto
;; a header row.
(defun vp-nearest-yard-row (idx / n total i)
  (setq total (length *VP-DISPLAY-ROWS*))
  (setq n idx)
  ;; search forward
  (setq i n)
  (while (and (< i total) (not (vp-row-code i)))
    (setq i (1+ i))
  )
  (if (< i total)
    i
    (progn
      ;; not found going forward (idx was on/after the last header with
      ;; no yards after it) -- search backward instead
      (setq i n)
      (while (and (>= i 0) (not (vp-row-code i)))
        (setq i (1- i))
      )
      (max i 0)
    )
  )
)

;; CODE at a given display-row index, or nil if that row is a header.
(defun vp-row-code (idx)
  (car (nth idx *VP-DISPLAY-ROWS*))
)

;; Called when OK is clicked (or Enter pressed, since OK is the dialog's
;; default button) -- stages all three field values into globals, then
;; closes the dialog with status 1 (accepted).
(defun vp-on-accept ( / artistidx)
  (setq *VP-TMP-CODE* (vp-selected-code))
  (setq artistidx (atoi (get_tile "artist_list")))
  (setq *VP-TMP-ARTIST* (nth artistidx *VP-ARTIST-LIST*))
  (setq *VP-TMP-CMDTEXT* (get_tile "cmd_text"))
  (setq *VP-TMP-PDFFLAG* (= (get_tile "pdf_check") "1"))
  (done_dialog 1)
)

;; Called from the DCL list_box action_tile whenever selection changes
;; (by click OR arrow keys). If the newly selected row is a header,
;; silently jump the selection to the nearest real yard row instead --
;; this is what makes headers "unselectable" despite DCL list_box
;; having no built-in concept of a disabled row.
(defun vp-update-preview ( / idx code fixedidx)
  (setq idx (atoi (get_tile "yard_list")))
  (setq code (vp-row-code idx))
  (if (not code)
    (progn
      (setq fixedidx (vp-nearest-yard-row idx))
      (set_tile "yard_list" (itoa fixedidx))
      (setq code (vp-row-code fixedidx))
    )
  )
  (if code
    (set_tile "target_preview" (strcat "Target:  " (vp-preview-path code *VP-TMP-CURFILENAME*)))
  )
)

;; Currently highlighted yard code (never a header, thanks to
;; vp-update-preview always correcting the selection immediately).
(defun vp-selected-code ( / idx)
  (setq idx (atoi (get_tile "yard_list")))
  (vp-row-code idx)
)

;; Build a short human-readable preview of the target folder for a code.
;; CURRENTFILENAME (no path/extension) is used to work out which
;; version folder this publish would actually land in, e.g. "v02" if
;; the open drawing is named "..._v02", or "v01" if it has no version
;; tag yet. Falls back to a generic "v<NN>" placeholder if no filename
;; is available.
(defun vp-preview-path (code currentfilename / seg parts vertag)
  (setq seg (cdr (assoc code *VP-YARD-MAP*)))
  (setq vertag
    (if (and currentfilename (/= currentfilename ""))
      (vp-format-version (nth 1 (vp-effective-version currentfilename)))
      "v<NN>"
    )
  )
  (if seg
    (vp-pathjoin (list seg "dwg" "yard" (strcat "yd_" code) vertag))
    "(unknown)"
  )
)

;;==========================================================================
;; MAIN COMMAND
;;==========================================================================
(defun c:VIDPUBLISH ( / dlgresult code artistname seg cmdtext pdfflag
                        srcpath srcname srcext docobj rootnorm yardfolder
                        curparts curprefix curver cursuffix curverfoldername
                        verfolderpath destdwg destpdf localpdfpath
                        ok fnresult pdfresult layoutcount targetlayoutname
                        newbasename newver newverfoldername
                        curlocaldir locbase newlocaldir renamedpath rensuccess
                        finalsrcpath finalfilename latestversion
                        logfolder logpath logline)

  (setq docobj (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq fnresult (vl-catch-all-apply 'vla-get-FullName (list docobj)))
  (setq srcpath
    (if (vl-catch-all-error-p fnresult) "" fnresult))

  (cond
    ((or (= srcpath "") (not srcpath))
     (princ "\nVIDPUBLISH: Current drawing has never been saved. Save it first with a real file name, then run VIDPUBLISH again.")
    )
    (T
     (setq dlgresult (vp-show-dialog (vp-filename-only srcpath)))
     (cond
       ((not dlgresult)
        (princ "\nVIDPUBLISH: Cancelled.")
       )
       (T
        (setq code (nth 0 dlgresult))
        (setq artistname (nth 1 dlgresult))
        (setq cmdtext (nth 2 dlgresult))
        (setq pdfflag (nth 3 dlgresult))
        (setq seg (cdr (assoc code *VP-YARD-MAP*)))

        (cond
          ((not seg)
           (princ (strcat "\nVIDPUBLISH: No folder mapping found for code: " code))
          )
          (T
           ;; ---- 1) Work out the SERVER target FIRST (nothing is changed
           ;; yet): yard folder + the drawing's CURRENT version, read
           ;; straight from the filename (no "_vNN" tag = implicit v01).
           (setq srcname (vp-filename-only srcpath))
           (setq srcext  (vp-fileext-only srcpath))
           (if (not srcext) (setq srcext ".dwg"))

           (setq rootnorm (vp-normpath *VP-ROOT-PATH*))
           (setq yardfolder
             (vp-pathjoin (list rootnorm seg "dwg" "yard" (strcat "yd_" code))))

           (setq curparts (vp-effective-version srcname))
           (setq curprefix (nth 0 curparts))
           (setq curver (nth 1 curparts))
           (setq cursuffix (nth 2 curparts))
           (setq curverfoldername (vp-format-version curver))
           (setq verfolderpath (vp-pathjoin (list yardfolder curverfoldername)))

           ;; Server-side name ALWAYS carries the matching "_vNN" tag,
           ;; even if the local file doesn't have one yet (implicit v01).
           (setq destdwg
             (vp-pathjoin (list verfolderpath
               (strcat curprefix "_" curverfoldername cursuffix srcext))))
           (setq destpdf
             (vp-pathjoin (list verfolderpath
               (strcat curprefix "_" curverfoldername cursuffix ".pdf"))))

           (cond
             ;; ---- 2) Version already on the server? Warn and STOP.
             ;; Nothing is saved, plotted, copied, logged or versioned.
             ((findfile destdwg)
              (princ (strcat "\nVIDPUBLISH WARNING: Already version available in server (" curverfoldername ").\n  " destdwg "\n  Nothing was published."))
              (alert (strcat "WARNING: Already version available in server.\n\nVersion: " curverfoldername
                             "\nFile: " destdwg
                             "\n\nNothing was published and no new version was created."))
             )
             (T
              ;; ---- 3) Write Commands text into DWGPROPS Comments ----
              (if (and cmdtext (/= cmdtext ""))
                (if (vp-set-dwgprops-comments docobj cmdtext)
                  (princ "\nVIDPUBLISH: Drawing properties (Comments) updated.")
                  (princ "\nVIDPUBLISH: Note - could not update drawing properties (non-fatal).")
                )
              )

              ;; ---- 4) Save current drawing (current file stays open) ----
              (princ "\nVIDPUBLISH: Saving current drawing...")
              (command "_.QSAVE")
              (setq fnresult (vl-catch-all-apply 'vla-get-FullName (list docobj)))
              (setq srcpath (if (vl-catch-all-error-p fnresult) srcpath fnresult))

              ;; ---- 5) Auto-create the version folder on the server ----
              (if (not (vl-file-directory-p verfolderpath))
                (progn
                  (princ (strcat "\nVIDPUBLISH: Creating folder: " verfolderpath))
                  (vp-mkdir-p verfolderpath)
                )
              )

              (cond
                ((not (vl-file-directory-p verfolderpath))
                 (princ (strcat "\nVIDPUBLISH ERROR: Could not create or access target folder:\n  "
                                verfolderpath
                                "\nCheck that the root path is correct and the drive/server is connected."))
                )
                (T
                 ;; ---- 6) Generate PDF (directly into the server version
                 ;; folder) if requested ----
                 (setq pdfresult "skipped")
                 (if pdfflag
                   (progn
                     (setq layoutcount (vp-count-paper-layouts docobj))
                     (setq targetlayoutname (vp-find-single-paper-layout docobj))
                     (cond
                       ((not targetlayoutname)
                        (setq pdfresult "skipped")
                        (princ (strcat "\nVIDPUBLISH WARNING: Expected exactly 1 paper-space layout "
                                       "(besides Model) to plot, found " (itoa layoutcount)
                                       ". Skipping PDF."))
                       )
                       (T
                        (princ (strcat "\nVIDPUBLISH: Generating PDF for layout: " targetlayoutname "..."))
                        (if (vp-plot-current-to-pdf destpdf targetlayoutname)
                          (progn
                            (setq pdfresult "ok")
                            (princ (strcat "\nVIDPUBLISH: PDF created: " destpdf))
                          )
                          (progn
                            (setq pdfresult "failed")
                            (princ "\nVIDPUBLISH: PDF failed (the DWG will still be published).")
                          )
                        )
                       )
                     )
                   )
                 )

                 ;; ---- 7) Copy DWG to server ----
                 (cond
                   ((not (vl-file-copy srcpath destdwg))
                    (princ (strcat "\nVIDPUBLISH ERROR: File copy failed.\n  From: "
                                   srcpath "\n  To:   " destdwg))
                   )
                   (T
                    (princ (strcat "\nVIDPUBLISH: DWG published.\n  From: "
                                   srcpath "\n  To:   " destdwg))

                    ;; ---- 8) Local DWG stays exactly where it is --
                    ;; publishing does not rename or move it. It was
                    ;; already re-saved in place via QSAVE above.
                    (setq finalsrcpath srcpath)
                    (setq finalfilename (strcat srcname srcext))
                    (setq latestversion curver)

                    ;; ---- 8b) If a PDF was generated, also save a copy of
                    ;; it LOCALLY, next to the local DWG (same base name). ----
                    (if (= pdfresult "ok")
                      (progn
                        (setq localpdfpath
                          (vp-pathjoin (list (vl-filename-directory srcpath) (strcat srcname ".pdf"))))
                        (if (vl-file-copy destpdf localpdfpath)
                          (princ (strcat "\nVIDPUBLISH: Local PDF copy saved: " localpdfpath))
                          (princ (strcat "\nVIDPUBLISH WARNING: Could not save local PDF copy to: "
                                         localpdfpath
                                         "\n  (Server publish already completed successfully.)"))
                        )
                      )
                    )

                    ;; ---- 8c) Create the NEXT local version folder and
                    ;; switch the open drawing to a bumped copy saved
                    ;; there (v02 -> v03). Only reached AFTER a successful
                    ;; server publish of a NEW version, so a version that
                    ;; already exists on the server never creates one. The
                    ;; old local DWG/PDF are left untouched. ----
                    (setq fnresult (vp-bump-filename-version srcname))
                    (setq newbasename (nth 0 fnresult))
                    (setq newver (nth 1 fnresult))
                    (setq newverfoldername (vp-format-version newver))

                    (setq curlocaldir (vl-filename-directory srcpath))
                    (setq locbase (vp-local-version-parent curlocaldir))
                    (setq newlocaldir (vp-pathjoin (list locbase newverfoldername)))
                    (vp-mkdir-p newlocaldir)

                    (setq renamedpath (vp-pathjoin (list newlocaldir (strcat newbasename srcext))))

                    (if (not (vl-file-directory-p newlocaldir))
                      (princ (strcat "\nVIDPUBLISH WARNING: Could not create local version folder: "
                                     newlocaldir
                                     "\n  (Server publish already completed successfully; old local file untouched.)"))
                      (progn
                        (princ (strcat "\nVIDPUBLISH: Saving new local version: " renamedpath))
                        (setq rensuccess (vl-catch-all-apply
                          '(lambda ()
                             (vla-SaveAs docobj renamedpath)
                           )
                          '()
                        ))
                        (if (vl-catch-all-error-p rensuccess)
                          (princ (strcat "\nVIDPUBLISH WARNING: Could not save new local version - "
                                         (vl-catch-all-error-message rensuccess)
                                         "\n  (Server publish already completed successfully; old local file untouched.)"))
                          (princ (strcat "\nVIDPUBLISH: Local drawing is now: " renamedpath))
                        )
                      )
                    )

                    ;; ---- 9) Append to log CSV (best effort, never blocks).
                    ;; Always records the SERVER-published version (curver),
                    ;; not the new local-only bumped version. ----
                    (setq logfolder (vp-normpath *VP-LOG-FOLDER*))
                    (vp-mkdir-p logfolder)
                    (setq logpath (vp-normpath *VP-LOG-PATH*))
                    ;; First ever publish: create the CSV with its header row.
                    (if (not (findfile logpath))
                      (vp-log-line logpath
                        "DATE TIME,PC NAME,ARTIST NAME,YARD CODE NAME,FILE NAME,LOCAL DWG,SERVER DWG,SERVER PDF,COMMANDS,VERSION,TELEGRAM,REMARKS")
                    )
                    (setq logline
                      (strcat (vp-timestamp) ","
                              (vp-csv-safe (vp-computer-name)) ","
                              (vp-csv-safe artistname) ","
                              code ","
                              (vp-csv-safe finalfilename) ","
                              (vp-csv-safe finalsrcpath) ","
                              (vp-csv-safe destdwg) ","
                              (vp-csv-safe (if (= pdfresult "ok") destpdf "")) ","
                              (vp-csv-safe cmdtext) ","
                              (itoa latestversion) ","
                              "teleNotPublished" "," ))  ;; publishedtelebot, then the empty remarks column (trailing comma, nothing after it) -- the separate telegram bot watches for this exact value, then rewrites it to "telePublished" once uploaded
                    (setq ok (vp-log-line logpath logline))
                    (if ok
                      (princ (strcat "\nVIDPUBLISH: Logged to " logpath))
                      (princ "\nVIDPUBLISH: Note - could not write to publish_log.csv (non-fatal).")
                    )

                    ;; ---- 10) LAST STEP: everything is done -- trigger the
                    ;; Telegram upload pipeline in the background. Only if
                    ;; the log row was written, since the pipeline reads it.
                    (if ok
                      (vp-run-pipeline)
                      (princ "\nVIDPUBLISH: Telegram pipeline NOT triggered (log row was not written).")
                    )
                   )
                 )
                )
              )
             )
           )
          )
        )
       )
     )
    )
  )
  (princ)
)

;; Convenience alias
(defun c:PUBLISH () (c:VIDPUBLISH))

(princ "\nVidPublish loaded. Type VIDPUBLISH or PUBLISH to run.")
(princ)
