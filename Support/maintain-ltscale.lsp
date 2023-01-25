;;; Copyright Lance A. Endres

(load "reactortools")

(defun C:INIT-MAINTAIN-LTSCALE () 
  (vl-load-com)

  ;;; Clear any active reactors with the same name.
  (REACTOR_REMOVE "MAINTAINLTSCALE")

  ;;; Establish reactor.
  (setq LTSCREACT (vlr-miscellaneous-reactor 
                    "MAINTAINLTSCALE"
                    '((:vlr-layoutswitched . MAINTAIN-LTSCALE))
                  ) ;_ End vlr-miscellaneous-reactor
  ) ;_ End setq
  (princ)
) ;_ End defun

(defun MAINTAIN-LTSCALE (REACT-NAME DATA) 
  (if (= (getvar "tilemode") 1) 
    (progn 
      (setvar "ltscale" (getvar "dimscale"))
      (vla-regen 
        (vla-get-activedocument (vlax-get-acad-object))
        acallviewports
      ) ;_ End vla-regen
    ) ;_ progn
    (progn 
      (setvar "ltscale" 1)
      (vla-regen 
        (vla-get-activedocument (vlax-get-acad-object))
        acallviewports
      ) ;_ End vla-regen
    ) ;_ progn
  ) ;_ End if
) ;_ End defun

(defun C:REMOVE-MAINTAIN-LTSCALE () 
  (REACTOR_REMOVE "MAINTAINLTSCALE")
  (princ)
) ;_ End defun