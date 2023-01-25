;;; Copyright Lance A. Endres

(vl-load-com)

;;; Delete any variables not used.
;;; Add program name.
(defun C:BGTOGGLE (/ CMD VL-ADOC VL-BG-COL VL-PREF-DISP) 
  (setq VL-ACAD      (vlax-get-acad-object)
        VL-PREF-DISP (vla-get-display 
                       (vla-get-preferences VL-ACAD)
                     ) ;_ End vla-get-display
        VL-ADOC      (vla-get-activedocument VL-ACAD)
        CMD          (vla-getvariable VL-ADOC "cmdecho")
  ) ;_ End setq
  (vla-startundomark VL-ADOC)
  (vla-setvariable VL-ADOC "cmdecho" 0)
  (vla-put-graphicswinmodelbackgrndcolor 
    VL-PREF-DISP

    ;;; Create the variant to use, the value will depend on the current status of the background color.
    (vlax-make-variant 
      (if 
        (= 
          (vlax-variant-value 

            ;;; Change the value so that vlax-variant-value will work.
            (setq VL-BG-COL (vlax-variant-change-type 

                              ;;; Get the background color.

                              (vla-get-graphicswinmodelbackgrndcolor 
                                VL-PREF-DISP
                              ) ;_ End vla-get-GraphicsWinModelBackgrndColor

                              vlax-vblong
                            ) ;_ End vlax-variant-change-type
            ) ;_ End setq
          ) ;_ End vlax-variant-value

          ;;; If 0, then the current is black.
          0
        ) ;_ End =

        ;;; 0 for black, 16777215 for white.
        16777215
        0
      ) ;_ End if
      19
    ) ;_ End vlax-make-variant
  ) ;_ End vla-put-GraphicsWinModelBackgrndColor

  ;;; Toggle the crosshair color also.  Set it to what the background was.
  (vla-put-ModelCrosshairColor VL-PREF-DISP VL-BG-COL)

  (vla-setvariable VL-ADOC "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

(defun *ERROR* (ERR) 
  (if (/= ERR "Function cancelled") 
    (princ (strcat "\nError: " ERR))
  ) ;_ End if
  (vla-setvariable VL-ADOC "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun