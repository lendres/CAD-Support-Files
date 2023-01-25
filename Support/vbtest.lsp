;;; Copyright Lance A. Endres

(defun C:VBTEST () 
  (vla-runmacro (vlax-get-acad-object) "vbtest.dvb!runvbtest")
  (princ)
) ;_ End defun