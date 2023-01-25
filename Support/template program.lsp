;;; Copyright Lance A. Endres

(vl-load-com)

;;; Delete any variables not used
;;; Add program name
(defun C: (/ CMD DIMS ORTH OSM VL-ADOC VL-ACAD) 
  (setq VL-ACAD                            (vlax-get-acad-object)
        VL-ADOC                            (vla-get-activedocument VL-ACAD) ;;; Delete anything not used 4 places
        DIMS                               (vla-getvariable VL-ADOC "dimscale")
        ORTH                               (vla-getvariable VL-ADOC "orthomode")
        OSM                                (vla-getvariable VL-ADOC "osmode")
        ---------------------------------- CMD
        (vla-getvariable VL-ADOC "cmdecho")
  ) ;_ End setq
  (vla-startundomark VL-ADOC)
  (vla-setvariable VL-ADOC "cmdecho" 0)


  ;;; Delete anything not used 4 places
  (vla-setvariable VL-ADOC "orthomode" 0)
  (vla-setvariable VL-ADOC "osmode" 0)
  ----------------------------------



  ;;; BODY




  ;;; Delete anything not used 4 places
  (vla-setvariable VL-ADOC "orthomode" ORTH)
  (vla-setvariable VL-ADOC "osmode" OSM)
  ----------------------------------

  (vla-setvariable VL-ADOC "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun



(defun *ERROR* (ERR) 
  (if (/= ERR "Function cancelled") 
    (princ (strcat "\nError: " ERR))
  ) ;_ End if


  ;;; Delete anything not used 4 places
  (vla-setvariable VL-ADOC "orthomode" ORTH)
  (vla-setvariable VL-ADOC "osmode" OSM)
  ----------------------------------

  (vla-setvariable VL-ADOC "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
