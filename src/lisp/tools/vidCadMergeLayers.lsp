;;; ============================================================================
;;; Merge To Layer 0
;;; Version: v01
;;; Purpose:
;;;   Merge specified Civil 3D / AutoCAD layers into Layer "0".
;;;
;;; Command:
;;;   VIDCADMERGELAYERS
;;;
;;; Notes:
;;;   - Only the layers listed below are processed.
;;;   - Missing layers are skipped.
;;;   - Source layers are unlocked/thawed before merging.
;;;   - Layer "0" is made current before processing.
;;;   - Uses native AutoCAD -LAYMRG command.
;;; ============================================================================


(defun VIDCADMERGELAYERS:LayerList ( / )
  (list

    "A-BLDG"
    "A-BLDG-FPRT"
    "A-PROP-LINE"
    "A-BLDG-UTIL"
    "A-BLDG-SITE"

    "C-ANNO-TABL"
    "C-ANNO-TABL-PATT"
    "C-ANNO-TABL-TEXT"
    "C-ANNO-TABL-TITL"
    "C-ANNO-TABL-TTBL"
    "C-ANNO"
    "C-ANNO-MATC"
    "C-ANNO-MATC-TEXT"
    "C-ANNO-VFRM"
    "C-ANNO-VFRM-TEXT"
    "C-ANNO-MATC-PATT"

    "C-BRDG-PIER"
    "C-BRDG-FOUNDATION"
    "C-BRDG-GIRDER"
    "C-BRDG-ABUTMENT"
    "C-BRDG-DECK"
    "C-BRDG-GENERICOBJECT"

    "C-ESMT-ROAD"

    "C-HYDR-CTCH"
    "C-HYDR-CTCH-BNDY"
    "C-HYDR-CTCH-DSPT"
    "C-HYDR-CTCH-FPTH"
    "C-HYDR-CTCH-FPTH-TEXT"
    "C-HYDR-CTCH-FSPT"
    "C-HYDR-CTCH-HDPT"
    "C-HYDR-CTCH-TEXT"
    "C-HYDR-TEXT"

    "C-PROP-BRNG"
    "C-PROP-TEXT"
    "C-PROP-LINE"
    "C-PROP-PATT"
    "C-PROP-RSRV"
    "C-PROP-BNDY"
    "C-PROP-LOTS"
    "C-PROP-LINE-TEXT"

    "C-ROAD-ASSM"
    "C-ROAD-ASSM-BLIN"
    "C-ROAD-ASSM-OFFS"
    "C-ROAD-ASSM-TEXT"
    "C-ROAD-BRNG"
    "C-ROAD-CNTR"
    "C-ROAD-CORR"
    "C-ROAD-CORR-BNDY"
    "C-ROAD-FEAT"
    "C-ROAD-LABL"
    "C-ROAD-LINE-EXTN"
    "C-ROAD-LINK"
    "C-ROAD-LINK-TEXT"
    "C-ROAD-MARK"
    "C-ROAD-PROF"
    "C-ROAD-PROF-DIAG"
    "C-ROAD-PROF-GRID-GEOM"
    "C-ROAD-PROF-GRID-MAJR"
    "C-ROAD-PROF-GRID-MINR"
    "C-ROAD-PROF-LINE-EXTN"
    "C-ROAD-PROF-PNTS"
    "C-ROAD-PROF-STAN-MAJR"
    "C-ROAD-PROF-STAN-MINR"
    "C-ROAD-PROF-TEXT"
    "C-ROAD-PROF-TICK"
    "C-ROAD-PROF-TITL"
    "C-ROAD-PROF-TTLB"
    "C-ROAD-SAMP"
    "C-ROAD-SAMP-TEXT"
    "C-ROAD-SECT"
    "C-ROAD-SECT-DIAG"
    "C-ROAD-SECT-GRID"
    "C-ROAD-SECT-TEXT"
    "C-ROAD-SECT-TICK"
    "C-ROAD-SECT-TITL"
    "C-ROAD-SECT-TTLB"
    "C-ROAD-SHAP"
    "C-ROAD-SHAP-PATT"
    "C-ROAD-STAN"
    "C-ROAD-STAN-MAJR"
    "C-ROAD-STAN-MINR"
    "C-ROAD-PROF-STAN-GEOM"
    "C-ROAD-PROF-GRID"
    "C-ROAD-SECT-TABL"
    "C-ROAD-TABL"
    "C-ROAD-CORR-PATT"
    "C-ROAD-SECT-LABL"
    "C-ROAD-CNTR-N"
    "C-ROAD-LINE"
    "C-ROAD-CURV"
    "C-ROAD-SPIR"
    "C-ROAD-TEXT"
    "C-ROAD-PROF-LINE"
    "C-ROAD-PROF-CURV"
    "C-ROAD-PROF-PARB"
    "C-ROAD-PROF-ASMC"
    "C-ROAD-PROF-PROP"
    "C-ROAD-PROF-LTOF"
    "C-ROAD-PROF-RTOF"
    "C-ROAD-SECT-N"
    "C-ROAD-MASS-LINE"
    "C-ROAD-MASS-VIEW"
    "C-ROAD-MASS-LINE-FREE"
    "C-ROAD-MASS-LINE-OVER"
    "C-ROAD-MASS-VIEW-TITL"
    "C-ROAD-MASS-VIEW-TEXT"
    "C-ROAD-MASS-VIEW-TTLB"
    "C-ROAD-MASS-VIEW-GRID-MAJR"
    "C-ROAD-MASS-VIEW-GRID-MINR"
    "C-ROAD-INTS"
    "C-ROAD-INTS-TEXT"
    "C-ROAD-PROF-PROJ"
    "C-ROAD-SECT-PROJ"
    "C-ROAD-PROF-LABL"
    "C-ROAD"
    "C-ROAD-SELV-VIEW"
    "C-ROAD-SELV-VIEW-TEXT"
    "C-ROAD-SELV-VIEW-TITL"
    "C-ROAD-SELV-VIEW-TTLB"
    "C-ROAD-SELV-VIEW-TICK"

    "C-RAIL-CANT-VIEW-TITL"
    "C-RAIL-CANT-VIEW-TTLB"
    "C-RAIL-CANT-VIEW-TEXT"
    "C-RAIL-CANT-VIEW-TICK"
    "C-RAIL-CANT-VIEW-RGHT"
    "C-RAIL-CANT-VIEW-LEFT"
    "C-RAIL-CANT-VIEW-CNTR"
    "C-RAIL-CANT-VIEW-EQLB"

    "C-ROAD-SGHT-VIS-LINE"
    "C-ROAD-SGHT-OBST-LINE"
    "C-ROAD-SGHT-OBST-PATH"
    "C-ROAD-SGHT-EYE-PATH"
    "C-ROAD-SGHT-LIMT-LINE"
    "C-ROAD-SGHT-OBST-AREA"
    "C-ROAD-SECT-BUFR"

    "C-RAIL-TURNOUT"
    "C-ROAD-PROF-VIEW"

    "C-STRM-CNTR"
    "C-STRM-PIPE"
    "C-STRM-PROF"
    "C-STRM-STRC"
    "C-STRM-TEXT"

    "C-TINN"
    "C-TINN-BNDY"
    "C-TINN-VIEW"

    "C-TOPO-GRAD"
    "C-TOPO-MAJR"
    "C-TOPO-MINR"
    "C-TOPO-TEXT"
    "C-TOPO-USER"
    "C-TOPO-WSHD"
    "C-TOPO-WSHD-TEXT"
    "C-TOPO-CONT-TEXT"

    "C-STRM-PIPE-PATT"
    "C-STRM-STRC-PATT"
    "C-TOPO-FEAT"
    "C-STRM-TABL"
    "C-TOPO-MAJR-N"
    "C-TOPO-MINR-N"
    "C-TOPO-CONT-TEXT-N"
    "C-TOPO-GRAD-CUTS"
    "C-TOPO-GRAD-FILL"

    "C-SSWR-CNTR"
    "C-SSWR-PIPE"
    "C-SSWR-PIPE-PATT"
    "C-SSWR-PROF"
    "C-SSWR-STRC"
    "C-SSWR-STRC-PATT"
    "C-SSWR-TEXT"

    "C-TOPO-WDRP"
    "C-TOPO-GRAD-TEXT"

    "C-STRM-PIPE-TEXT"
    "C-STRM-SCTN"

    "C-WATR-CNTR"
    "C-WATR-PIPE"
    "C-WATR-PIPE-PATT"
    "C-WATR-PROF"
    "C-WATR-FITT"
    "C-WATR-TEXT"
    "C-WATR-APPT"
    "C-WATR-SCTN"
    "C-WATR-FITT-PATT"
    "C-WATR-APPT-PATT"
    "C-WATR-TABL"

    "V-CTRL-LINE-DIRC"
    "V-CTRL-LINE-NETW"
    "V-CTRL-LINE-SHOT"
    "V-CTRL-TRAV"
    "V-CTRL-TRAV-ERRO"

    "V-NODE"
    "V-NODE-TEXT"
    "V-SURV-LABL"
    "V-SURV-LINE"
    "V-SURV-FIGR"
    "V-SURV-NTWK"

    "V-CTRL-NODE-KNOW"
    "V-CTRL-NODE-UNKN"
    "V-CTRL-NODE-SHOT"

    "V-NODE-SIGN"
    "V-CTRL-VCPT"
    "V-NODE-WATR"
    "V-NODE-BNDY"
    "V-CTRL-HCPT"
    "V-NODE-TREE"
    "V-NODE-SSWR"
    "V-NODE-STRM"
    "V-NODE-NGAS"
    "V-NODE-POLE"

    "V-CTRL-BMRK"

    "V-SITE-FNCE"
    "V-SITE-VEGE"
    "V-BLDG-OTLN"
    "V-ROAD-CURB"
    "V-ROAD-CNTR"
    "V-NODE-BORE"
    "V-SITE-SCAN"

  )
)


(defun VIDCADMERGELAYERS:LayerExists (lay)
  (if (tblsearch "LAYER" lay)
    T
    nil
  )
)


(defun VIDCADMERGELAYERS:MergeLayer (lay / )
  ;; Skip Layer 0
  (if (/= (strcase lay) "0")
    (progn

      ;; Unlock and thaw the source layer
      (command
        "_.-LAYER"
        "_UNLOCK"
        lay
        ""
      )

      (command
        "_.-LAYER"
        "_THAW"
        lay
        ""
      )

      ;; Merge source layer into Layer 0
      (command
        "_.-LAYMRG"
        "_N"
        lay
        ""
        "_N"
        "0"
        "_Y"
      )

      ;; Finish any remaining command activity
      (while (> (getvar "CMDACTIVE") 0)
        (command "_Y")
      )

      T
    )
  )
)


(defun C:VIDCADMERGELAYERS
  (
    /
    oldCmdecho
    oldClayer
    layerList
    lay
    foundCount
    mergedCount
    skippedCount
  )

  (vl-load-com)

  ;; Save settings
  (setq oldCmdecho (getvar "CMDECHO"))
  (setq oldClayer  (getvar "CLAYER"))

  (setvar "CMDECHO" 0)

  ;; Make Layer 0 current.
  ;; This is important because AutoCAD does not allow
  ;; the current layer to be merged.
  (setvar "CLAYER" "0")

  (setq layerList   (VIDCADMERGELAYERS:LayerList))
  (setq foundCount  0)
  (setq mergedCount 0)
  (setq skippedCount 0)

  (princ "\n")
  (princ "\n==============================================")
  (princ "\n MERGE LAYERS TO 0 - v01")
  (princ "\n==============================================")
  (princ "\nTarget Layer : 0")
  (princ "\nProcessing specified layers...")

  ;; Process every specified layer
  (foreach lay layerList

    (if (VIDCADMERGELAYERS:LayerExists lay)

      (progn
        (setq foundCount (1+ foundCount))

        (princ
          (strcat
            "\nMerging: "
            lay
          )
        )

        (if (VIDCADMERGELAYERS:MergeLayer lay)
          (setq mergedCount (1+ mergedCount))
          (setq skippedCount (1+ skippedCount))
        )
      )

      ;; Layer does not exist
      (progn
        (setq skippedCount (1+ skippedCount))

        (princ
          (strcat
            "\nSkip - Layer not found: "
            lay
          )
        )
      )
    )
  )

  ;; Restore previous current layer if it still exists.
  ;; If it was one of the merged layers, keep Layer 0.
  (if
    (and
      oldClayer
      (/= (strcase oldClayer) "0")
      (tblsearch "LAYER" oldClayer)
    )
    (setvar "CLAYER" oldClayer)
    (setvar "CLAYER" "0")
  )

  ;; Restore CMDECHO
  (setvar "CMDECHO" oldCmdecho)

  ;; Final report
  (princ "\n")
  (princ "\n==============================================")
  (princ "\n MERGE COMPLETE")
  (princ "\n==============================================")

  (princ
    (strcat
      "\nLayers found     : "
      (itoa foundCount)
    )
  )

  (princ
    (strcat
      "\nMerge attempts   : "
      (itoa mergedCount)
    )
  )

  (princ
    (strcat
      "\nSkipped/not found: "
      (itoa skippedCount)
    )
  )

  (princ "\nTarget layer     : 0")
  (princ "\n==============================================")

  (princ)
)


(princ
  "\nVIDCADMERGELAYERS loaded. Type VIDCADMERGELAYERS to merge specified layers into Layer 0."
)

(princ)
