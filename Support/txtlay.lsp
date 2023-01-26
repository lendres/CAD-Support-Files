;;; Copyright Lance A. Endres

(vl-load-com)
(load "autolayerset")

(defun c:DTEXT_LAYER (/ CMD) 
  (TEXT_LAYER_MAIN "DTEXT")
  (princ)
) ;_ End defun

(defun c:MTEXT_LAYER (/ CMD LYTEST) 
  (TEXT_LAYER_MAIN "MTEXT")
  (princ)
) ;_ End defun

(defun TEXT_LAYER_MAIN (TYPE / CMD CLAY OLDERR) 
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setq OLDERR  *ERROR*
        *ERROR* TEXTLAYERERR
        CMD     (getvar "cmdecho")
        CLAY    (getvar "clayer")
  ) ;_ End setq
  (setvar "cmdecho" 1)
  ;;;(setvar "cmdecho" 1)	;;; For debugging.
  (AUTOLAYERSET)
  (cond 
    ((= TYPE "DTEXT")
     (command "._dtext")
    ) ;_End dtext
    ((= TYPE "MTEXT")
     (initdia 1)
     (command "._mtext")
    ) ;_End mtext
  ) ;_ End cond
  (while (= (logand (getvar "cmdactive") 1) 1) 
    (command pause)
  ) ;_ End while
  (setvar "clayer" CLAY)
  (setvar "cmdecho" CMD)
  ;;;(setvar "cmdecho" 0)    ;;; For debugging.
  (setq *ERROR* OLDERR)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
) ;_ End defun

(defun TEXTLAYERERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;End if
  (setq *ERROR* OLDERR)
  (setvar "clayer" CLAY)
  (setvar "cmdecho" CMD)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
) ;_ End defun   