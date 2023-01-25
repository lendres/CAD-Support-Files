;;; Written by Ian Bryant

(defun GET_PST_NAME (LAYOUT_NAME / VAR1) 
  (if 
    (setq VAR1 (cdr 
                 (assoc 
                   350
                   (member 
                     (cons 3 LAYOUT_NAME)
                     (dictsearch (namedobjdict) "ACAD_LAYOUT")
                   ) ;_ End member
                 ) ;_ End assoc
               ) ;_ End cdr
    ) ;_ End setq
    (if (= (setq VAR1 (cdr (assoc 7 (entget VAR1)))) "") 
      "None"
      VAR1
    ) ;_ End if
  ) ;_ End if
) ;_ End defun

(defun SET_PST_NAME (LAYOUT_NAME PSTYLE_NAME / VAR1) 
  (if 
    (setq VAR1 (cdr 
                 (assoc 
                   350
                   (member 
                     (cons 3 LAYOUT_NAME)
                     (dictsearch (namedobjdict) "ACAD_LAYOUT")
                   ) ;_ End member
                 ) ;_ End assoc
               ) ;_ End cdr
    ) ;_ End setq
    (entmod 
      (subst 
        (cons 
          7
          (if (= (strcase PSTYLE_NAME) "NONE") 
            ""
            PSTYLE_NAME
          ) ;_ End if
        ) ;_ End cons
        (assoc 7 (entget VAR1))
        (entget VAR1)
      ) ;_ End subst
    ) ;_ End entmod
  ) ;_ End if
) ;_ End defun