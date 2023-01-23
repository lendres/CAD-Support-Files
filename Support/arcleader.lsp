;;; arcleader.lsp
;;; Copyright 1998-2000 by Lance A. Endres

(defun C:AL ()
    (if	(not (tblsearch "block" "arclead"))
	(ALMAKE)
    ) ;_ End if
    (ARCLEADER "arclead")
) ;_ End defun

;;;(defun C:ALL ()
;;;    (if	(not (tblsearch "block" "homerun"))
;;;	(HRMAKE)
;;;    ) ;_ End if
;;;    (ARCLEADER "homerun")
;;;) ;_ End defun

(defun ARCLEADER (INSBLOCK	  /	  ARK	  CENPT	  CMD
		  DIMS	  ENDANG  LASTPT  OSM     PT1	  PT2
		  ROT     STANG	  TESTANG
		  )
		  
;;;                  CURLAY ENT1 ENT1DEF ENT1LAY

    (vl-load-com)
    (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
    (setq OLDERR  *ERROR*
	  *ERROR* ALERR
	  DIMS	  (getvar "dimscale")
	  ORTH	  (getvar "orthomode")
	  OSM	  (getvar "osmode")
	  CMD	  (getvar "cmdecho")
;;;          CURLAY (getvar "clayer")
    ) ;_ End setq
    (setvar "cmdecho" 0)
    (setvar "orthomode" 0)
    (setq PT1 (getpoint "\nStart point: ")
          PT2 (getpoint PT1 "\nSecond point: ")
    )

;;;          ENT1 (ssget PT1) ;_ To be part of the setq
;;;    (if ENT1
;;;        (progn
;;;            (setq ENT1 (ssname ENT1 0)
;;;                  ENT1DEF (entget ENT1)
;;;                  ENT1LAY (cdr (assoc 8 ENT1DEF))
;;;            ) ;End setq
;;;            (setvar "clayer" ENT1LAY)
;;;        ) ;End progn
;;;    ) ;End if
    
    (setvar "cmdecho" 1)
    (setvar "osmode" 0)
    (command "_.arc" PT1 PT2 PAUSE)
    (setq ARK	  (entlast) ;_ Gather data about arc
      CENPT	  (cdr (assoc 10 (entget ARK)))
      ENDANG  (cdr (assoc 51 (entget ARK)))
      ENDANG  (/ (* ENDANG 180) pi)
      LASTPT  (getvar "lastpoint")
      TESTANG (/ (* (angle CENPT LASTPT) 180) pi)
    ) ;_ End setq
    (if	(= (fix ENDANG) (fix TESTANG)) ;_ If arc drawn CCW then use start point
      (setq ROT (- ENDANG -90))
      (setq STANG (cdr (assoc 50 (entget ARK)))
	      ROT   (+ (* (/ STANG pi) 180) -90)
      ) ;_ End setq
    ) ;_ End if
    (setvar "cmdecho" 0)
    (command "_.insert" INSBLOCK "@" DIMS DIMS ROT)

;;;    (setvar "clayer" CURLAY)

    (setvar "orthomode" ORTH)
    (setvar "osmode" OSM)
    (setq *ERROR* OLDERR)
    (setvar "cmdecho" CMD)
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (princ)
) ;_ End defun

(defun ALMAKE ()
    (entmake '((0 . "SOLID")
	       (100 . "AcDbEntity")
	       (67 . 0)
	       (410 . "Model")
	       (8 . "0")
	       (100 . "AcDbTrace")
	       (10 0.0 0.0208333 0.0)
	       (11 0.125 1.13687e-013 0.0)
	       (12 0.0 -0.0208333 0.0)
	       (13 0.0 -0.0208333 0.0)
	       (39 . 0.0)
	       (210 0.0 0.0 1.0)
	      )
    ) ;_ End entmake
    (command "block" "arclead" "0,0,0" (entlast) "")
) ;_ End defun

(defun HRMAKE ()
    (entmake '((0 . "SOLID")
	       (100 . "AcDbEntity")
	       (67 . 0)
	       (410 . "Model")
	       (8 . "0")
	       (100 . "AcDbTrace")
	       (10 0.0 0.0 0.0)
	       (11 0.142619 0.0 0.0)
	       (12 0.0 -0.061489 0.0)
	       (13 0.0 -0.061489 0.0)
	       (39 . 0.0)
	       (210 0.0 0.0 1.0)
	      )
    ) ;_ End entmake
    (command "block" "homerun" "0,0,0" (entlast) "")
) ;_ End defun

(defun ALERR (S)
    (if	(/= S "Function cancelled")
      (princ (strcat "\nError: " S))
    ) ;_ End if
    
;;;    (setvar "clayer" CURLAY)
    
    (setvar "orthomode" ORTH)
    (setvar "osmode" OSM)
    (setq *ERROR* OLDERR)
    (setvar "cmdecho" CMD)
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (princ)
) ;_ End defun
