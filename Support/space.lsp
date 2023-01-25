;;; Copyright Lance A. Endres

(defun C:MS () 
  (setvar "tilemode" 1)
  (princ)
) ;_ End defun

(defun C:PS () 
  (setvar "tilemode" 0)
  (princ)
) ;_ End defun