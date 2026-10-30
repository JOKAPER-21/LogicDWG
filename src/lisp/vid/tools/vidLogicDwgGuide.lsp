(vl-load-com)

(defun c:VIDLOGICDWGGUIDE (/ url result)
  (setq url "https://jokaper-21.github.io/LogicDWG/src/guide/index.html")

  (setq result
    (startapp "explorer.exe" url)
  )

  (if result
    (princ "\nLogicDWG online homepage opened in default browser.")
    (princ "\nUnable to open LogicDWG homepage.")
  )

  (princ)
)

(princ "\nVIDLOGICDWGONLINE loaded. Type VIDLOGICDWGONLINE to open homepage.")
(princ)
