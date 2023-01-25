;;; Copyright Lance A. Endres

(defun VERSORTTEXT (SS2 / CNT LOOP1 SS1 SS1LEN TEMP TEMPYPNT TEMP2 TEMP2YPNT) 
  (setq SS1LEN (sslength SS2)
        LOOP1  "True"
  ) ;_ End setq
  (if (> SS1LEN 25) 
    (princ "\nSorting text...")
  ) ;_ End if
  (while (and (= LOOP1 "True") (> SS1LEN 1)) 
    (setq SS1      SS2
          SS2      (ssadd)
          SS1LEN   (sslength SS1)
          LOOP1    NIL
          CNT      0
          TEMP     (ssname SS1 CNT)
          TEMPYPNT (cadr (cdr (assoc 10 (entget TEMP))))
    ) ;_ End setq
    (while (< CNT (1- SS1LEN)) 
      (setq CNT       (1+ CNT)
            TEMP2     (ssname SS1 CNT)
            TEMP2YPNT (cadr (cdr (assoc 10 (entget TEMP2))))
      ) ;_ End setq
      (cond 
        ((<= TEMP2YPNT TEMPYPNT)
         (ssadd TEMP SS2)
         (setq TEMP     TEMP2
               TEMPYPNT TEMP2YPNT
         ) ;_ End setq
        ) ;_ End temp2 greater
        ((> TEMP2YPNT TEMPYPNT)
         (ssadd TEMP2 SS2)
         (setq LOOP1 "True")
        ) ;_ End temp greater
      ) ;_ End cond
      (if (= CNT (1- SS1LEN)) 
        (ssadd TEMP SS2)
      ) ;_ End if for including last line of text
    ) ;_ End while
  ) ;_ End while
  SS2
) ;_ End defun

(defun VL-VERSORTTEXT (SS2 / ADDTEMPARRAY CNT LOOP1 SS1 SS1LEN TEMP TEMPYPNT TEMP2 
                       TEMP2YPNT
                      ) 
  (setq SS1LEN (vla-get-count SS2)
        LOOP1  "True"
  ) ;_ End setq
  (if (> SS1LEN 25) 
    (princ "\nSorting text...")
  ) ;_ End if
  (setq ADDTEMPARRAY (vlax-make-safearray vlax-vbobject '(0 . 0)))
  (while (and (= LOOP1 "True") (> SS1LEN 1)) 
    (setq SS1 (EST-VL-SS-BY-NAME "VERTSORTTEXT1"))
    (vlax-for TEMP SS2 
      (vlax-safearray-put-element ADDTEMPARRAY 0 TEMP)
      (vla-additems SS1 (vlax-make-variant ADDTEMPARRAY))
    )
    (setq SS2      (EST-VL-SS-BY-NAME "VERTSORTTEXT2")
          SS1LEN   (vla-get-count SS1)
          LOOP1    NIL
          CNT      0
          TEMP     (vla-item SS1 CNT)
          TEMPYPNT (vlax-safearray-get-element 
                     (vlax-variant-value 
                       (vla-get-insertionpoint TEMP)
                     ) ;_ End vlax-variant-value
                     1
                   ) ;_ End vlax-safearray-get-element
    ) ;_ End setq
    (while (< CNT (1- SS1LEN)) 
      (setq CNT       (1+ CNT)
            TEMP2     (vla-item SS1 CNT)
            TEMP2YPNT (vlax-safearray-get-element 
                        (vlax-variant-value 
                          (vla-get-insertionpoint TEMP2)
                        ) ;_ End vlax-variant-value
                        1
                      ) ;_ End vlax-safearray-get-element
      ) ;_ End setq
      (cond 
        ((<= TEMP2YPNT TEMPYPNT)
         (vlax-safearray-put-element ADDTEMPARRAY 0 TEMP)
         (vla-additems SS2 (vlax-make-variant ADDTEMPARRAY))
         (setq TEMP     TEMP2
               TEMPYPNT TEMP2YPNT
         ) ;_ End setq
        ) ;_ End temp2 greater
        ((> TEMP2YPNT TEMPYPNT)
         (vlax-safearray-put-element ADDTEMPARRAY 0 TEMP2)
         (vla-additems SS2 (vlax-make-variant ADDTEMPARRAY))
         (setq LOOP1 "True")
        ) ;_ End temp greater
      ) ;_ End cond
      (if (= CNT (1- SS1LEN)) 
        (progn 
          (vlax-safearray-put-element ADDTEMPARRAY 0 TEMP)
          (vla-additems SS2 (vlax-make-variant ADDTEMPARRAY))
        ) ;_ End progn
      ) ;_ End if for including last line of text
    ) ;_ End while
  ) ;_ End while
  SS2
) ;_ End defun