;;; Copyright Lance A. Endres

(defun c:TEXTBLOCKS (/ ANG BLCK DIMS DIST ENT1 ENT1DEF INSPT ORTH PT1 PT2 PT3 TXT) 
  (setq OLDERR  *ERROR*
        *ERROR* TEXTBLOCKSERR
        DIMS    (getvar "dimscale")
        ORTH    (getvar "orthomode")
        BLCK    (strcase (getstring "\nBlock to insert: "))
        TXT     (strcase (getstring "\nText to insert: "))
        INSPT   (getpoint "\nInsertion point: ")
        CMD     (getvar "cmdecho")
  ) ;End setq
  (setvar "orthomode" 1)
  (command "insert" BLCK INSPT DIMS "" pause)
  (setvar "cmdecho" 0)
  (if (= (substr (strcase BLCK) 1 16) "LIEBERT_CONTROLS") 
    (progn 
      (cond 
        ((= TXT "1")
         (setq DIST 0.3125)
        ) ;_ End cond for switch room block
        ((= TXT "2")
         (setq DIST 0.29166667)
        ) ;_ End cond for battery room block
      ) ;_ End cond
      (setq ENT1    (ssname (ssget "l") 0)
            ENT1DEF (entget ENT1)
            ANG     (cdr (assoc 50 ENT1DEF))
            PT1     (polar INSPT ANG (* DIMS DIST))
            PT2     (polar PT1 (- ANG (/ pi 2)) (* DIMS 0.0520833))
            PT3     (polar PT1 (- ANG (/ pi 2)) (* DIMS 0.15625))
      ) ;_ End setq
      (command "insert" "w" PT2 DIMS "" "0" "insert" "r" PT3 DIMS "" "0") ;_ End command
    ) ;_ End progn
    (command "insert" TXT INSPT DIMS "" "0")
  ) ;_ End if
  (setvar "orthomode" ORTH)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun

(defun TEXTBLOCKERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;End if
  (setvar "orthomode" ORTH)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
)