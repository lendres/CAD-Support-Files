;;; Copyright Lance A. Endres

(load "versorttext")

(defun C:EXTRACT-TEXT-TO-FILE (/ ACADDOC CMD CNT1 FILE_HAND SS1) 
  (vl-load-com)
  (setq OLDERR  *ERROR*
        *ERROR* EXTRTEXTERR
        ACADDOC (vla-get-activedocument (vlax-get-acad-object))
        CMD     (vla-getvariable ACADDOC "cmdecho")
        SS1     (ssget 
                  (list 
                    '(-4 . "<OR")
                    '(0 . "TEXT")
                    '(0 . "MTEXT")
                    '(-4 . "OR>")
                  )
                )
        CNT1    0
  ) ;_ End setq
  (vla-setvariable ACADDOC "cmdecho" 0)
  (if SS1 
    (setq SS1 (VERSORTTEXT SS1))
    (progn 
      (princ "\nNo text entities selected.")
      (exit)
    ) ;_ End progn
  ) ;_ End if
  (setq FILE_HAND (open 
                    (strcat 
                      (vlax-variant-value 
                        (vla-getvariable ACADDOC "dwgprefix")
                      ) ;_ End vlax-variant-value
                      (vlax-variant-value 
                        (vla-getvariable ACADDOC "dwgname")
                      ) ;_ End vlax-variant-value
                      " - Extracted Text.txt"
                    ) ;_ End strcat
                    "w"
                  ) ;_ End open
  ) ;_ End setq
  (repeat (sslength SS1) 
    (write-line 
      (vla-get-textstring (vlax-ename->vla-object (ssname SS1 CNT1)))
      FILE_HAND
    ) ;_ End write-line
    (setq CNT1 (1+ CNT1))
  ) ;_ End repeat
  (close FILE_HAND)
  (setq *ERROR* OLDERR)
  (vla-setvariable ACADDOC "cmdecho" CMD)
  (princ)
) ;_ End defun

(defun EXTRTEXTERR (ERR) 
  (if 
    (and 
      (/= ERR "Function cancelled")
      (/= ERR "quit / exit abort")
    ) ;_ End and
    (princ (strcat "\nError: " ERR))
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (vla-setvariable ACADDOC "cmdecho" CMD)
  (princ)
) ;_ End defun