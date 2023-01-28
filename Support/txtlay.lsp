;;; Copyright Lance A. Endres
;;; Text commands that automatically put the text on a specified layer.

(vl-load-com)
(load "autolayerset")

(defun C:DTEXT_LAYER (/ CMD) 
  (TEXT_LAYER_MAIN "._dtext")
  (princ)
) ;_ End defun

(defun C:MTEXT_LAYER (/ CMD LYTEST)
	; Force the dialog box for MTEXT to be shown.  Command line only is typically used for lisp.
  (initdia 1)
  (TEXT_LAYER_MAIN "._mtext")
  (princ)
) ;_ End defun

(defun TEXT_LAYER_MAIN (TEXTCOMMAND / CMD CLAY OLDERR) 
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setq OLDERR  *ERROR*
        *ERROR* TEXTLAYERERR
        CMD     (getvar "cmdecho")
        CLAY    (getvar "clayer")
  ) ;_ End setq
  (setvar "cmdecho" 1)

  ; Activate the layer used for autolayer commands.
  (AUTOLAYERSET)
  
  ; Start the text command using the argument passed.
  (command TEXTCOMMAND)
  
  ; Wait while the command is still active (wait for user input).
  (while (= (logand (getvar "cmdactive") 1) 1) 
    (command pause)
  ) ;_ End while

	; Restore settings.
  (setvar "clayer" CLAY)
  (setvar "cmdecho" CMD)
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