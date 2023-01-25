;;; Copyright Lance A. Endres

(defun C:GLUE (/ ANGL1 ANGL2 CCOL CLAY CMD COL1 COL2 CLT LAY1 LAY2 LINE1 LINE2 
               LINE1DEF LINE2DEF LUPREC PT1 PT2 PT3 PT4 PTE PTS TEST
              ) 
  (setq OLDERR  *ERROR*
        *ERROR* GLUEERR
        CMD     (getvar "cmdecho")
  ) ;_ End setq
  (setvar "cmdecho" 0)
  (while (= LINE1 NIL) 
    (setq LINE1 (entsel "\nSelect the first line to be glued: "))
    (if LINE1 
      (setq TEST (cdr (assoc 0 (entget (car LINE1)))))
    ) ;_ End if
    (if (/= TEST "LINE") 
      (progn 
        (setq LINE1 NIL)
        (princ "\nEntity is not a line")
      ) ;_ End progn
    ) ;_ End if test for selecting a line
  ) ;_ End while
  (redraw (car LINE1) 3)
  (while (= LINE2 NIL) 
    (setq LINE2 (entsel "\nSelect the second line to be glued: "))
    (if LINE2 
      (setq TEST (cdr (assoc 0 (entget (car LINE2)))))
    ) ;_ End if
    (if (/= TEST "LINE") 
      (progn 
        (setq LINE2 NIL)
        (princ "\nEntity is not a line")
      ) ;_ End progn
    ) ;_ End if test for selecting a line
  ) ;_ End while
  (setq CLAY     (getvar "clayer")
        CCOL     (getvar "cecolor")
        CLT      (getvar "celtype")
        LINE1DEF (entget (car LINE1))
        PT1      (cdr (assoc 10 LINE1DEF))
        PT2      (cdr (assoc 11 LINE1DEF))
        LAY1     (cdr (assoc 8 LINE1DEF))
        COL1     (cdr (assoc 62 LINE1DEF))
        LT1      (cdr (assoc 6 LINE1DEF))
        LINE2DEF (entget (car LINE2))
        PT3      (cdr (assoc 10 LINE2DEF))
        PT4      (cdr (assoc 11 LINE2DEF))
        LAY2     (cdr (assoc 8 LINE2DEF))
        COL2     (cdr (assoc 62 LINE2DEF))
        LT2      (cdr (assoc 6 LINE2DEF))
        ANGL1    (angle PT1 PT3)
        ANGL2    (angle PT1 PT4)
        DIST13   (distance PT1 PT3)
        DIST14   (distance PT1 PT4)
        DIST23   (distance PT2 PT3)
        DIST24   (distance PT2 PT4)
  ) ;_ End setq
  (if (equal ANGL1 (* pi 2) 0.00000001) 
    (setq ANGL1 0)
  ) ;_ End if
  (if (equal ANGL2 (* pi 2) 0.00000001) 
    (setq ANGL2 0)
  ) ;_ End if
  (cond 
    ((and (> DIST13 DIST14) (> DIST13 DIST23) (> DIST13 DIST24))
     (setq PTS PT1
           PTE PT3
     ) ;_ End setq
    ) ;_ End cond for 1 to 3 greatest
    ((and (> DIST14 DIST13) (> DIST14 DIST23) (> DIST14 DIST24))
     (setq PTS PT1
           PTE PT4
     ) ;_ End setq
    ) ;_ End cond for 1 to 4 greatest
    ((and (> DIST23 DIST14) (> DIST23 DIST13) (> DIST23 DIST24))
     (setq PTS PT2
           PTE PT3
     ) ;_ End setq
    ) ;_ End cond for 2 to 3 greatest
    ((and (> DIST24 DIST13) (> DIST24 DIST14) (> DIST24 DIST23))
     (setq PTS PT2
           PTE PT4
     ) ;_ End setq
    ) ;_ End cond for 1 to 3 greatest
  ) ;_ End cond
  (command "_.undo" "begin")
  (setvar "clayer" LAY1)
  (if COL1 
    (setvar "cecolor" (itoa COL1))
    (setvar "cecolor" "bylayer")
  ) ;_ End if for setting color to same as line1
  (if LT1 
    (setvar "celtype" LT1)
    (setvar "celtype" "bylayer")
  ) ;_ End if for setting linetype to same as line1
  (if (or (/= LAY1 LAY2) (/= COL1 COL2) (/= LT1 LT2)) 
    (progn 
      (princ "\nThe lines do not have the same properties")
      (princ 
        "\nThe new line inherits the properties of the first line"
      ) ;_ End princ
    ) ;_ End progn
  ) ;_ End if for suppling
  (cond 
    ((equal ANGL1 ANGL2 0.00000001)
     (command "_.line" PTS PTE "" "_.erase" LINE1 LINE2 "") ;_ End command
    ) ;_ End command for parallel and colinear lines
    ((/= ANGL1 ANGL2)
     (princ "\nThe lines are not parallel and colinear")
    ) ;_ End cond for lines that are not parallel and colinear
  ) ;_ End cond statement
  (redraw (car LINE1) 4)
  (command "_.undo" "end")
  (setq *ERROR* OLDERR)
  (setvar "clayer" CLAY)
  (setvar "cecolor" CCOL)
  (setvar "celtype" CLT)
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun

(defun GLUEERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (redraw (car LINE1) 4)
  (setvar "clayer" CLAY)
  (setvar "cecolor" CCOL)
  (setvar "celtype" CLT)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun