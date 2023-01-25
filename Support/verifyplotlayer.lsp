;;; Copyright Lance A. Endres

(defun C:VERIFYPLOTLAYER ()
    (vla-runmacro (vlax-get-acad-object) "verifyplotlayer.dvb!verifyplotlayer")
    (princ)
) ;_ End defun