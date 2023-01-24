;;; fchamfer.lsp
;;; Copyright 1997-2000 by Lance A. Endres

(defun C:FCHAMFER (/ ENT1 ENT2) 
  (setvar "cmdecho" 0)
  (setq OLDERR  *ERROR*
        *ERROR* FERR
        CHAMA   (getvar "chamfera")
        CHAMB   (getvar "chamferb")
        TRM     (getvar "trimmode")
  ) ;_ End setq
  (while (= ENT1 NIL) 
    (setq ENT1 (entsel "\nSelect first line "))
    (if (/= ENT1 NIL) 
      (progn 
        (setq ENTTYPE (cdr (assoc 0 (entget (car ENT1)))))
        (if (and (/= ENTTYPE "LINE") (/= ENTTYPE "LWPOLYLINE")) 
          (progn 
            (setq ENT1 NIL)
            (princ "\nEntity is not a line or pline")
          ) ;_ End then progn
        ) ;_ End if
      ) ;_ End then progn
      (princ "\nNo entity found") ;_ End else progn
    ) ;_ End if
  ) ;_ End while
  (redraw (car ENT1) 3)
  (while (= ENT2 NIL) 
    (setq ENT2 (entsel "\nSelect second line "))
    (if (/= ENT2 NIL) 
      (progn 
        (setq ENTTYPE (cdr (assoc 0 (entget (car ENT2)))))
        (if (and (/= ENTTYPE "LINE") (/= ENTTYPE "LWPOLYLINE")) 
          (progn 
            (setq ENT2 NIL)
            (princ "\nEntity is not a line or pline")
          ) ;_ End then progn
        ) ;_ End if
      ) ;_ End then progn
      (princ "\nNo entity found") ;_ End else progn
    ) ;_ End if
  ) ;_ End while
  (setvar "chamfera" 0)
  (setvar "chamferb" 0)
  (setvar "trimmode" 1)
  (command "chamfer" ENT1 ENT2)
  (setvar "chamfera" CHAMA)
  (setvar "chamferb" CHAMB)
  (setvar "trimmode" TRM)
  (setvar "cmdecho" 1)
  (princ)
) ;_ End defun

(defun FERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (if ENT1 
    (redraw (car ENT1) 4)
  ) ;_ End if
  (setvar "chamfera" CHAMA)
  (setvar "chamferb" CHAMB)
  (setvar "trimmode" TRM)
  (setvar "cmdecho" 1)
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun