;;; Copyright Lance A. Endres

(load "linespace")
(load "versorttext")

(defun C:TOMTEXT (/ CMD CNT DIMS DIST FNT INSPT1 INSPT2 LINE1 LOOP1 NEWASSOC NUMSPAC 
                  OLDASSOC PT1 PT2 SLEN SS1 SS1LEN TEMP TSIZE XPT YPT
                 ) 
  (princ "\nSelect text to be fixed.")
  (setq OLDERR  *ERROR*
        *ERROR* TOMTEXTERR
        SS1     (ssget)
        SS1     (ssget "P" (list '(0 . "TEXT")))
        LINE1   ""
        CMD     (getvar "cmdecho")
        DIMS    (getvar "dimscale")
        TSIZE   (getvar "textsize")
        LOOP1   "True"
  ) ;_ End setq
  (vl-load-com)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setvar "cmdecho" 0)

  ;_ Check for selection set

  (if (null SS1) 
    (progn 
      (princ "\nNo text objects selected.")
      (exit)
    ) ;_ End progn
    (progn 
      (setq SS1LEN (sslength SS1))
      (print SS1LEN)
      (princ "text items found.")
    ) ;_ End progn
  ) ;_ End if

  ; Sort and extract text.
  (setq SS1    (VERSORTTEXT SS1)
        SS1LEN (sslength SS1)
        TEMP   (entget (ssname SS1 0))
        SLEN   (strlen (cdr (assoc 1 TEMP)))
        FNT    (strcase 
                 (cdr (assoc 3 (tblsearch "style" (cdr (assoc 7 TEMP)))))
               ) ;_ End strcase
        CNT    0
        PT1    (cdr (assoc 10 TEMP))
        XPT    (car PT1)
        YPT    (cadr PT1)
        ZPT    (caddr PT1)
        PT1    (list XPT (+ YPT TSIZE) ZPT)
        PT2    (list (+ XPT (* (/ DIMS 8.5) SLEN)) (- YPT 8.5) ZPT)
  ) ;_ End setq
  (setq YSPAC (LINESPACE (ssname SS1 0)))
  (while (< CNT SS1LEN) 
    (setq TEMP  (cdr (assoc 1 (entget (ssname SS1 CNT))))
          CNT   (1+ CNT)
          LINE1 (strcat LINE1 TEMP)
    ) ;_ End setq
    (if (/= SS1LEN CNT) 
      (progn 
        (if (/= (substr LINE1 (1- (strlen LINE1))) " ") 
          (setq LINE1 (strcat LINE1 " "))
        )
        (setq INSPT1  (cdr (assoc 10 (entget (ssname SS1 (1- CNT))))) ;_ End cdr
              INSPT2  (cdr (assoc 10 (entget (ssname SS1 CNT))))
              DIST    (distance INSPT1 INSPT2)
              NUMSPAC (/ DIST YSPAC)
        ) ;_ End if
        (if (> NUMSPAC 1.5) 
          (progn 
            (setq LINE1 (strcat LINE1 "\\P\\P"))
            (repeat (1- (fix NUMSPAC)) 
              (setq LINE1 (strcat LINE1 "\\P"))
            ) ;_ End repeat
          ) ;_ End progn
        ) ;_ End if
      ) ;_ End progn
    ) ;_ End if
  ) ;_ End while

  ; Create the mtext entity.


  ;;;    (command "_.erase" SS1)
  ;;;    (vla-addmtext (vlax-3d-point PT1) (* (/ DIMS 8.5) SLEN) TEMP)
  (command "_.erase" SS1 "" "_.-mtext" PT1 PT2 "MTEXT CONVERTER BY LANCE A. ENDRES" 
           ""
  ) ;_ End command
  (setq TEMP (vlax-ename->vla-object (entlast)))
  (vla-put-textstring TEMP LINE1)
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)
) ;_ End defun

(defun TOMTEXTERR (S) 
  (if (and (/= S "Function cancelled") (/= S "quit / exit abort")) 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)
) ;_ End defun