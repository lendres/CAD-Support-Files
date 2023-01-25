;;; Copyright Lance A. Endres

(defun C:BLOCKSCALE (/ CMD CNT ENT1 ENT1DEF PT1 SC SS) 
  (setq OLDERR  *ERROR*
        *ERROR* BLOCKSCALEERR
        CMD     (getvar "cmdecho")
  ) ;_ End setq
  (setvar "cmdecho" 0)
  (command "_.undo" "begin")
  (princ "\nSelect blocks/text to scale at the insertion point")

  ;;; Create selection set.
  (setq SS  (ssget) ;;; Filter out blocks
        SS  (ssget 
              "P"
              (list 
                '(-4 . "<OR")
                '(0 . "INSERT")
                '(0 . "TEXT")
                '(0 . "MTEXT")
                '(-4 . "OR>")
              )
            ) ;_ End ssget
        CNT 0
  ) ;_ End setq
  (while (or (= SC NIL) (<= SC 0)) 
    (setq SC (getreal "\nEnter scale factor: "))
    (if (<= SC 0) 
      (princ "\nValue must be positive and nonzero")
    ) ;_ End if
  ) ;_ End while
  (if SS 
    (setq ENT1 (ssname SS CNT))
  ) ;_ End if
  (while ENT1 
    (setq ENT1DEF (entget ENT1)
          PT1     (cdr (assoc 10 ENT1DEF))
    ) ;_ End setq
    (command "_.scale" ENT1 "" PT1 SC)
    (setq CNT  (1+ CNT)
          ENT1 (ssname SS CNT)
    ) ;_ End setq
  ) ;_ End while
  (setq *ERROR* OLDERR)
  (command "_.undo" "end")
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun

(defun BLOCKSCALEERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (command "_.undo" "end")
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun