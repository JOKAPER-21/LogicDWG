(defun c:VIDGRADIENT 
  (/ pl km-start file fp line csv-data km-list
     dist-start dist-km offset-dist
     blk-name doc mspace pl-length
     perp-offset pt1 pt2 tang-angle pt-final
     obj atts att tag
     tail-value head-value
     tail-rot-value head-rot-value props p prop-name km-data note-value)

  (vl-load-com)

  ;; -------------------------------
  ;; SAFE CSV SPLIT
  ;; -------------------------------
  (defun csv-split (str delimiter / pos result temp)
    (setq result '()
          temp   str)
    (while (setq pos (vl-string-search delimiter temp))
      (setq result (append result
                           (list (vl-string-trim " "
                                 (substr temp 1 pos)))))
      (setq temp (substr temp (+ pos (strlen delimiter) 1))))
    (append result (list (vl-string-trim " " temp)))
  )

  ;; -------------------------------
  ;; KM TO METERS
  ;; -------------------------------
  (defun km-to-meters (km-str / parts i)
    ;; remove any prefix like "KM:" so "KM:72/112.329" -> "72/112.329"
    (setq i 1)
    (while (and (<= i (strlen km-str))
                (not (wcmatch (substr km-str i 1) "#")))
      (setq i (1+ i)))
    (setq km-str (substr km-str i))
    (setq parts (csv-split km-str "/"))
    (if (= (length parts) 2)
      (+ (* (atof (car parts)) 1000.0)
         (atof (cadr parts)))
      (atof km-str))
  )

  ;; -------------------------------
  ;; GET POLYLINE LENGTH
  ;; -------------------------------
  (defun get-polyline-length (ent)
    (vlax-curve-getDistAtParam
      ent
      (vlax-curve-getEndParam ent))
  )

  ;; -------------------------------
  ;; SET DYNAMIC PROPERTY (IMPROVED)
  ;; -------------------------------
  (defun set-dyn-prop (blk propName value / props p prop-name rad-value found)
    (if (= (vla-get-IsDynamicBlock blk) :vlax-true)
      (progn
        (setq props (vlax-invoke blk 'GetDynamicBlockProperties))
        (setq found nil)
        (setq rad-value (* value (/ pi 180.0)))  ;; Convert degrees to radians
        (foreach p props
          (setq prop-name (vla-get-PropertyName p))
          (if (= (strcase prop-name)
                 (strcase propName))
            (progn
              (vla-put-Value
                p
                (vlax-make-variant rad-value vlax-vbDouble))
              (setq found T)
              (prompt (strcat "\n    ✓ " propName " = " (rtos value 2 1) "° (" (rtos rad-value 2 6) " rad)"))
            )
          )
        )
        (if (not found)
          (prompt (strcat "\n    ✗ Property '" propName "' not found in block"))
        )
      )
    )
  )

  ;; -------------------------------
  ;; LIST ALL DYNAMIC PROPERTIES (DEBUG)
  ;; -------------------------------
  (defun list-dyn-props (blk / props p)
    (if (= (vla-get-IsDynamicBlock blk) :vlax-true)
      (progn
        (setq props (vlax-invoke blk 'GetDynamicBlockProperties))
        (prompt "\n  Available Dynamic Properties:")
        (foreach p props
          (prompt (strcat "\n    - " (vla-get-PropertyName p)))
        )
      )
      (prompt "\n  Not a dynamic block!")
    )
  )

  ;; -------------------------------
  ;; INITIAL SETUP
  ;; -------------------------------
  (setq blk-name "G_NEW_FINAL")
  (setq doc     (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq mspace  (vla-get-ModelSpace doc))

  (if (not (tblsearch "BLOCK" blk-name))
    (progn
      (prompt "\nBlock not found.")
      (princ)
    )

    (progn
      ;; Select polyline
      (if (setq pl (car (entsel "\nSelect Rail Track: ")))

        (progn
          (setq pl-length (get-polyline-length pl))
          (prompt (strcat "\nPolyline Length: "
                          (rtos pl-length 2 3) " m"))

          ;; Starting KM
          (if (setq km-start
                    (getstring T
                      "\nEnter Starting CH KM (30/000): "))

            (progn
              ;; Offset
              (setq perp-offset
                    (getreal
                      "\nEnter Grdient Hight [80]: "))
              (if (not perp-offset)
                (setq perp-offset 80.0))

              ;; CSV
              (if (setq file
                        (getfiled "Select CSV File"
                                  "" "csv" 0))

                (progn
                  (setq fp (open file "r"))
                  (read-line fp) ;; skip header
                  (setq km-list '())

                  (while (setq line (read-line fp))
                    (setq csv-data (csv-split line ","))
                    ;; merge extra fields into G_NOTE (in case the note contains commas)
                    (if (> (length csv-data) 6)
                      (setq csv-data
                        (append (list (nth 0 csv-data) (nth 1 csv-data) (nth 2 csv-data)
                                      (nth 3 csv-data) (nth 4 csv-data))
                                (list (apply 'strcat
                                        (cons (nth 5 csv-data)
                                              (mapcar '(lambda (x) (strcat "," x)) (nthcdr 6 csv-data))))))))
                    ;; skip blank lines
                    (if (and (> (length csv-data) 0) (/= (car csv-data) ""))
                      (setq km-list
                            (append km-list
                                    (list csv-data))))
                  )
                  (close fp)

                  ;; PROCESS
                  (setq dist-start
                        (km-to-meters km-start))

                  (foreach km-data km-list

                    (setq dist-km
                          (km-to-meters (car km-data)))
                    (setq offset-dist
                          (- dist-km dist-start))

                    (if (and (>= offset-dist 0.0)
                             (<= offset-dist pl-length))

                      (progn
                        (prompt (strcat "\n\nProcessing: " (car km-data)))
                        (prompt (strcat "\n  CSV Data: " 
                                       (car km-data) ", "
                                       (nth 1 km-data) ", "
                                       (nth 2 km-data) ", "
                                       (nth 3 km-data) ", "
                                       (nth 4 km-data) ", "
                                       (if (nth 5 km-data) (nth 5 km-data) "")))

                        ;; SAFE TANGENT ANGLE
                        (setq pt1
                          (vlax-curve-getPointAtDist
                            pl offset-dist))
                        (setq pt2
                          (vlax-curve-getPointAtDist
                            pl (+ offset-dist 0.01)))
                        (setq tang-angle
                          (angle pt1 pt2))

                        ;; OFFSET POINT
                        (setq pt-final
                          (polar pt1
                                 (- tang-angle (/ pi 2.0))
                                 perp-offset))

                        (setq pt-final
                              (vlax-3d-point pt-final))

                        ;; INSERT BLOCK (radians)
                        (setq obj
                          (vla-InsertBlock
                            mspace
                            pt-final
                            blk-name
                            1.0 1.0 1.0
                            tang-angle))

                        ;; -----------------------
                        ;; SET ATTRIBUTES SAFELY
                        ;; -----------------------
                        (if (= (vla-get-HasAttributes obj)
                               :vlax-true)
                          (progn
                            (setq atts
                              (vlax-invoke obj
                                           'GetAttributes))
                            (setq tail-value nil)
                            (setq head-value nil)
                            
                            (foreach att atts
                              (setq tag
                                (strcase
                                  (vla-get-TagString att)))

                              (cond
                                ((= tag "G_CHAINAGE")
                                 (if (>= (length km-data) 1)
                                   (vla-put-TextString
                                     att (nth 0 km-data))))
                                ((= tag "G_RAILLEVEL")
                                 (if (>= (length km-data) 2)
                                   (vla-put-TextString
                                     att (nth 1 km-data))))
                                ((= tag "G_LINE")
                                 (if (>= (length km-data) 3)
                                   (vla-put-TextString
                                     att (nth 2 km-data))))
                                ((= tag "G_TAIL")
                                 (if (>= (length km-data) 4)
                                   (progn
                                     (setq tail-value (nth 3 km-data))
                                     (vla-put-TextString
                                       att tail-value))))
                                ((= tag "G_HEAD")
                                 (if (>= (length km-data) 5)
                                   (progn
                                     (setq head-value (nth 4 km-data))
                                     (vla-put-TextString
                                       att head-value))))
                                ((= tag "G_NOTE")
                                 (vla-put-TextString
                                   att (if (nth 5 km-data) (nth 5 km-data) "")))
                              )
                            )
                          )
                        )

                        ;; -----------------------
                        ;; DETERMINE ROTATION VALUES
                        ;; -----------------------
                        (setq tail-rot-value 0)
                        (setq head-rot-value 0)
                        
                        ;; Determine G_TAIL_ROT based on G_TAIL value
                        (if tail-value
                          (cond
                            ((vl-string-search "R" tail-value)
                             (setq tail-rot-value 21)
                             (prompt "\n  G_TAIL_ROT: 21° (R detected)"))
                            ((vl-string-search "F" tail-value)
                             (setq tail-rot-value 339)
                             (prompt "\n  G_TAIL_ROT: 339° (F detected)"))
                            ((vl-string-search "LEVEL" tail-value)
                             (setq tail-rot-value 0)
                             (prompt "\n  G_TAIL_ROT: 0° (LEVEL detected)"))
                            (T
                             (setq tail-rot-value 0)
                             (prompt "\n  G_TAIL_ROT: 0° (default)"))
                          )
                          (progn
                            (setq tail-rot-value 0)
                            (prompt "\n  G_TAIL_ROT: 0° (no value)"))
                        )
                        
                        ;; Determine G_HEAD_ROT based on G_HEAD value
                        (if head-value
                          (cond
                            ((vl-string-search "R" head-value)
                             (setq head-rot-value 21)
                             (prompt "\n  G_HEAD_ROT: 21° (R detected)"))
                            ((vl-string-search "F" head-value)
                             (setq head-rot-value 339)
                             (prompt "\n  G_HEAD_ROT: 339° (F detected)"))
                            ((vl-string-search "LEVEL" head-value)
                             (setq head-rot-value 0)
                             (prompt "\n  G_HEAD_ROT: 0° (LEVEL detected)"))
                            (T
                             (setq head-rot-value 0)
                             (prompt "\n  G_HEAD_ROT: 0° (default)"))
                          )
                          (progn
                            (setq head-rot-value 0)
                            (prompt "\n  G_HEAD_ROT: 0° (no value)"))
                        )

                        ;; -----------------------
                        ;; SET DYNAMIC ROTATIONS
                        ;; -----------------------
                        (prompt "\n  Setting Dynamic Properties:")
                        (prompt (strcat "\n    CSV G_TAIL value: " (if tail-value tail-value "")))
                        (prompt (strcat "\n    CSV G_HEAD value: " (if head-value head-value "")))
                        
                        (set-dyn-prop
                          obj
                          "G_TAIL_ROT"
                          tail-rot-value)
                        
                        (set-dyn-prop
                          obj
                          "G_HEAD_ROT"
                          head-rot-value)

                        (vla-update obj)
                        (prompt "\n  ✓ Block inserted")
                      )
                    )
                  )

                  (prompt "\n\nCompleted.")
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