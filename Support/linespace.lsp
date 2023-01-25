(defun LINESPACE (ENT1 / CSTYL DIST PT1 PT2 TSTYL) 
  (vl-load-com)
  (setq CSTYL   (getvar "textstyle")
        ENT1DEF (entget ENT1)
        TSTYL   (cdr (assoc 7 ENT1DEF))
  ) ;_ End setq

  ;;; Switched the following method of changing tstyles to avoid the A2K bug.
  ;;;    (setvar "textstyle" TSTYL)
  (command "_.-style" TSTYL)
  (while (= (logand (getvar "cmdactive") 1) 1) 
    (command "")
  )

  (if (= (cdr (assoc 40 (tblsearch "style" TSTYL))) 0) 
    (command 
      "_.text"
      "0,0"
      (cdr (assoc 40 ENT1DEF))
      "0"
      "LINESPACE"
      "_.text"
      ""
      "BY LANCE A. ENDRES"
    ) ;_ End command
    (command "_.text" "0,0" "0" "LINESPACE" "_.text" "" "BY LANCE A. ENDRES") ;_ End command
  ) ;_ End if
  (setq PT2 (cdr (assoc 10 (entget (entlast)))))
  (vla-delete (vlax-ename->vla-object (entlast)))
  (setq PT1 (cdr (assoc 10 (entget (entlast)))))
  (vla-delete (vlax-ename->vla-object (entlast)))

  ;;; Switched the following method of changing tstyles to avoid the A2K bug.
  ;;;    (setvar "textstyle" CSTYL)
  (command "_.-style" CSTYL)
  (while (= (logand (getvar "cmdactive") 1) 1) 
    (command "")
  )

  (setq DIST (distance PT1 PT2))
) ;_ End defun