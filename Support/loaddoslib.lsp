(defun LOADDOSLIB ()

  ;;; Load doslib if necessary.
  (setq DOSLIBFOUND 0)

  (if (= "Visual LISP 2023 (en)xx" (ver)) 
    (progn 
      (if (not (member "DOSLib24x64.arx" (arx))) 
        (progn 
          (arxload (findfile "DOSLib24x64.arx"))
          (princ)
          (setq DOSLIBFOUND 1)
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End PROGN
  ) ;_ End if

  (if (= "Visual LISP 2018 (en)" (ver)) 
    (progn 
      (if (not (member "DOSLib22x64.arx" (arx))) 
        (progn 
          (arxload (findfile "DOSLib22x64.arx"))
          (princ)
          (setq DOSLIBFOUND 1)
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End PROGN
  ) ;_ End if

  (if (= "Visual LISP 2015 (en)" (ver)) 
    (progn 
      (if (not (member "DOSLib20x64.arx" (arx))) 
        (progn 
          (arxload (findfile "DOSLib20x64.arx"))
          (princ)
          (setq DOSLIBFOUND 1)
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End PROGN
  ) ;_ End if

  (if 
    (or 
      (= "Visual LISP 2004 (en)" (ver))
      (= "Visual LISP 2006 (en)" (ver))
    ) ;_ End or
    (progn 
      (if (not (member "doslib16.arx" (arx))) 
        (progn 
          (arxload (findfile "doslib16.arx"))
          (princ)
          (setq DOSLIBFOUND 1)
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End PROGN
  ) ;_ End if
  (if 
    (or 
      (= "Visual LISP 2000 (en)" (ver))
      (= "Visual LISP 2002 (en)" (ver))
    ) ;_ End or
    (progn 
      (if (not (member "doslib15.arx" (arx))) 
        (progn 
          (arxload (findfile "doslib15.arx"))
          (princ)
          (setq DOSLIBFOUND 1)
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End progn
  ) ;_ End IF

  (if (= DOSLIBFOUND 0) 
    (progn 
      (princ "\nDos lib failed to load in 'create-text-styles'")
      (princ (strcat "\nVersion: " (ver) "\n"))
      ;(exit)
    )
  ) ;_ End IF
)