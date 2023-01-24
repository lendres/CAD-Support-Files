;;; Copyright Lance A. Endres

(defun SET-FRZN-LOCK-LAY (VL-ENT1 / LAY-STAT-LIST) 
  (setq LAY           (vla-item 
                        (vla-get-layers 
                          (vla-get-activedocument 
                            (vlax-get-acad-object)
                          ) ;_ End vla-get-activedocument
                        ) ;_ End vla-get-layers
                        (vla-get-layer VL-ENT1)
                      ) ;_ End vla-item
        LAY-STAT-LIST (list (vla-get-freeze LAY))
        LAY-STAT-LIST (append LAY-STAT-LIST (list (vla-get-lock LAY)))
  ) ;_ End setq
  (if (= (vla-get-freeze LAY) :vlax-true) 
    (vla-put-freeze LAY :vlax-false)
  ) ;_ End if
  (if (= (vla-get-lock LAY) :vlax-true) 
    (vla-put-lock LAY :vlax-false)
  ) ;_ End if
  LAY-STAT-LIST
) ;_ End defun

(defun RESET-FRZN-LOCK-LAY (VL-ENT1 LAY-STAT-LIST / LAY) 
  (setq LAY (vla-item 
              (vla-get-layers 
                (vla-get-activedocument 
                  (vlax-get-acad-object)
                ) ;_ End vla-get-activedocument
              ) ;_ End vla-get-layers
              (vla-get-layer VL-ENT1)
            ) ;_ End vla-item
  ) ;_ End setq
  (if (/= (vla-get-freeze LAY) (car LAY-STAT-LIST)) 
    (vla-put-freeze LAY (car LAY-STAT-LIST))
  ) ;_ End if
  (if (/= (vla-get-lock LAY) (cadr LAY-STAT-LIST)) 
    (vla-put-lock LAY (cadr LAY-STAT-LIST))
  ) ;_ End if
) ;_ End defun