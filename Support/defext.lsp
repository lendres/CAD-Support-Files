;;; Copyright Lance A. Endres

(defun C:DEFEXT (/ ENT) 
  (while (not ENT) 
    (setq ENT (car 
                (entsel 
                  "\nSelect entity to extract the definition of: "
                ) ;_ End entsel
              ) ;_ End car
    ) ;_ End setq
  ) ;_ End while
  (DEFEXT-MAIN ENT)
  (princ)
) ;_ End defun

(defun C:NDEFEXT (/ ENT) 
  (while (not ENT) 
    (setq ENT (car 
                (nentsel 
                  "\nSelect entity to extract the definition of: "
                ) ;_ End nentsel
              ) ;_ End car
    ) ;_ End setq
  ) ;_ End while
  (DEFEXT-MAIN ENT)
  (princ)
) ;_ End defun

(defun DEFEXT-MAIN (ENT / CNT1) 
  (setq ENTDEF (entget ENT)
        CNT1   0
  ) ;_ End setq
  (textscr)
  (princ "\nThe entities definition is:")
  (repeat (length ENTDEF) 
    (princ "\n ")
    (princ (nth CNT1 ENTDEF))
    (setq CNT1 (1+ CNT1))
  ) ;_ End repeat
) ;_ End defun