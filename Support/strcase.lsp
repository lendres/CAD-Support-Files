;;; Copyright Lance A. Endres

(defun C:STRCASE (/ CMD CNT1 CNT2 ENT1 ENT1A1 ENT1DEF ENT1TEXT SS1 TEMP TEMP2 VL-ADOC) 
  (vl-load-com)
  (setq OLDERR  *ERROR*
        *ERROR* STRCASEERR
        VL-ADOC (vla-get-activedocument (vlax-get-acad-object))
        CMD     (getvar "cmdecho")
  ) ;_ End setq
  (vla-startundomark VL-ADOC)
  (setvar "cmdecho" 0)
  (if (= CASE t) 
    (princ "\nCase for text? Upper/<Lower>: ")
    (princ "\nCase for text? <Upper>/Lower: ")
  ) ;_ End if
  (initget "Upper Lower")
  (setq TEMP (getkword))
  (if (or (= TEMP "Lower") (and (= CASE t) (= TEMP NIL))) 
    (setq CASE t)
    (setq CASE NIL)
  ) ;_ End if
  (princ "\nSelect text to be case converted: ")
  (setq SS1 (ssget)
        SS1 (ssget 
              "P"
              (list 
                '(-4 . "<OR")
                '(0 . "TEXT")
                '(0 . "MTEXT")
                '(-4 . "OR>")
              ) ;_ End list
            ) ;_ End ssget
  ) ;_ End setq
  (if SS1 
    (setq CNT1 0
          ENT1 (ssname SS1 CNT1)
    ) ;_ End setq
  ) ;_ End if
  (while ENT1 
    (setq VL-ENT1  (vlax-ename->vla-object ENT1)
          ENT1TEXT (strcase (vla-get-textstring VL-ENT1) CASE)
          CNT2     1
          ENT1DEF  (entget ENT1)
    ) ;_ End setq
    (if (and CASE (= (cdr (assoc 0 ENT1DEF)) "MTEXT")) 
      (progn 
        (setq TEMP2 "")
        (repeat (strlen ENT1TEXT) 
          (if (= (setq TEMP (substr ENT1TEXT CNT2 2)) "\\p") 
            (setq TEMP  (strcase TEMP)
                  TEMP2 (strcat TEMP2 TEMP)
                  CNT2  (+ CNT2 2)
            ) ;_ End setq
            (setq TEMP2 (strcat TEMP2 (substr TEMP 1 1))
                  CNT2  (1+ CNT2)
            ) ;_ End setq
          ) ;_ End if
        ) ;_ End repeat
        (setq ENT1TEXT TEMP2)
      ) ;_ End progn
    ) ;_ End if
    (vla-put-textstring VL-ENT1 ENT1TEXT)
    (redraw ENT1 1)
    (setq CNT1 (1+ CNT1)
          ENT1 (ssname SS1 CNT1)
    ) ;_ End setq
  ) ;_ End while
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

(defun STRCASEERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun