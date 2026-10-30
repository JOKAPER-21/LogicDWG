(defun VID:MapZone (zone)
  (cond
    ;; Off -> switch off GeoMap
    ((= zone "Off")
     (command "._GEOMAP" "_Off")
     (princ "\nGeoMap turned Off.")
    )
    ;; Zone 43
    ((= zone "Z43")
     (command "._MAPCSASSIGN" "UTM84-43N")
     (command "._GEOMAP" "_Hybrid")
    )
    ;; Zone 44
    ((= zone "Z44")
     (command "._MAPCSASSIGN" "UTM84-44N")
     (command "._GEOMAP" "_Hybrid")
    )
  )
  (princ)
)

;; ---- MapZone dialog (temporary DCL, deleted after use)
;; ---- Returns "Z43" / "Z44" / "Off", or nil if closed with Esc / X.
;; ---- Button [43] is_default = true (satisfies the DCL OK/CANCEL rule).
(defun VID:MapZoneDialog (/ dclfile fp id res zone)
  (setq dclfile (vl-filename-mktemp "vidmapzone" nil ".dcl"))
  (setq fp (open dclfile "w"))
  (if fp
    (progn
      (foreach ln
        '("vid_mapzone : dialog {"
          "  label = \"MapZone\";"
          "  : row {"
          "    alignment = centered;"
          "    : button { key = \"z43\"; label = \"43\";  width = 10; fixed_width = true; is_default = true; }"
          "    : button { key = \"z44\"; label = \"44\";  width = 10; fixed_width = true; }"
          "    : button { key = \"off\"; label = \"Off\"; width = 10; fixed_width = true; }"
          "  }"
          "}")
        (write-line ln fp)
      )
      (close fp)
      (setq id (load_dialog dclfile))
      (if (and id (> id 0) (new_dialog "vid_mapzone" id))
        (progn
          (action_tile "z43" "(done_dialog 11)")
          (action_tile "z44" "(done_dialog 12)")
          (action_tile "off" "(done_dialog 13)")
          (setq res (start_dialog))
          (setq zone (cond ((= res 11) "Z43")
                           ((= res 12) "Z44")
                           ((= res 13) "Off")))
        )
        (prompt "\nMapZone: unable to load the dialog.")
      )
      (if (and id (> id 0)) (unload_dialog id))
      (vl-file-delete dclfile)
    )
    (prompt "\nMapZone: unable to create the temporary dialog file.")
  )
  zone
)

(defun c:vidmapzone (/ zone)
  (if (setq zone (VID:MapZoneDialog))
    (VID:MapZone zone)
  )
  (princ)
)
(princ)
