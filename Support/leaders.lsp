;;; Copyright Lance A. Endres

;;; March 8, 2004
;;; Added the used of EXPERT and eliminated having to decide how to call the
;;; adding of dimstyles depending on if they were already loaded.

(vl-load-com)
(load "auto_layerset")
(load "leader_base")
(load "createdimstyles")

(defun C:ELL () 
  (LEADER_POINTS "ELLIP1_LEAD")
) ;_ End defun

(defun C:LL () 
  (LEADER_POINTS "ARROW1_LEAD")
) ;_ End defun

(defun LEADER_POINTS (LEAD_TYPE / CLAY CMD LMBD1 LMBD2 OSM ORTH VL-ADOC) 
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark VL-ADOC)
  (setq CMD   (vla-getvariable VL-ADOC "cmdecho")
        ORTH  (vla-getvariable VL-ADOC "orthomode")
        OSM   (vla-getvariable VL-ADOC "osmode")
        CLAY  (vla-getvariable VL-ADOC "clayer")
        EXPRT (vla-getvariable VL-ADOC "expert")
  ) ;_ End setq
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "cmdecho" "orthomode" "expert")
    (list 1 0 5)
  ) ;_ End mapcar
  (if (= LEAD_TYPE "ELLIP1_LEAD") 
    (progn 
      (vla-setvariable VL-ADOC "osmode" 512)
    ) ;_ End progn
  ) ;_ End if
  (setq PT1 (getpoint "\nStart of Leader "))
  (vla-setvariable VL-ADOC "osmode" 0)
  (setq PT2 (getpoint PT1 "\nEndpoint of Leader: "))

  ;;; If leaders are always to be placed on text layer, create layer if
  ;;; necessary and set text layer current.
  (AUTO_LAYERSET)

  ;;; Make the leader.
  (LEADER_BASE LEAD_TYPE PT1 PT2)

  ;;; Clean up any changed variables, setup undo mark, and exit quitly.
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "cmdecho" "orthomode" "osmode" "clayer" "expert")
    (list CMD ORTH OSM CLAY EXPRT)
  ) ;_ End mapcar.
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

(defun *ERROR* (MSG / LMBD1 LMBD2) 
  (if (/= MSG "Function cancelled") 
    (princ (strcat "\nError: " MSG))
  ) ;_ End if

  ;;; Reset UCS if necessary
  (if (and (/= ROT_ANG 0) (/= ROT_ANG NIL)) 
    (command "_.ucs" "world" "_.ucs" "3point" UORG UXDIR UYDIR) ;_ End command
  ) ;_ End if

  ;;; Reset dimscale if necessary
  (if (not (= DIMSC NIL)) 
    (setvar "dimscale" DIMSC)
  ) ;_ End if

  ;;; Clean up the rest of the variables that were changed, set undo mark and exit quitly.
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "osmode" "orthomode" "clayer" "cmdecho" "expert")
    (list OSM ORTH CLAY CMD EXPRT)
  ) ;_ End mapcar

  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun