;;; Copyright Lance A. Endres

(defun C:TRIMP (/	ADOC	ANGL1	ANGL2	CMD	COUNT	DIMS
		ENT1	INTS	NEWTRIM	OSM	PT1	PT2	PT3
		PT4	SSTOTRIM	STRIMDIST	TEMP	TRMDEF
		TRMENT
	       )
    (setq OLDERROR  *ERROR*
	  *ERROR*   TRGERR
	  ADOC	    (vla-get-activedocument (vlax-get-acad-object))
	  DIMS	    (getvar "dimscale")
	  STRIMDIST (/ DIMS 12)
	  OSM	    (getvar "osmode")
	  COUNT	    0
	  CMD	    (getvar "cmdecho")
    ) ;_ End setq
    (vl-load-com)
    (vla-startundomark ADOC)
    (setvar "cmdecho" 0)
    (setvar "osmode" 0)
    (if	(or (= TRIMDIST NIL) (= TRIMDIST 0))
	(setq TRIMDIST STRIMDIST)
    ) ;_ End if for trimdist
    (princ "\nTrim distance = ")
    (princ TRIMDIST)
    (while (= ENT1 NIL)
	(initget "Trim")
	(setq ENT1 (entsel "\nTrim distance/<Pick cutting line>: "))
	(if (= ENT1 "Trim")
	    (progn
		(mapcar	'princ
			(list
			    "\nStandard trim distance = "
			    STRIMDIST
			    "\nTrim distance <"
			    TRIMDIST
			    ">: "
			   ) ;_ End list
		) ;_ End mapcar
		(initget 2)
		(setq TEMP (getreal)
		      ENT1 NIL
		) ;_ End setq
		(if TEMP
		    (setq TRIMDIST TEMP)
		) ;_ End if
	    ) ;_ End progn
	    (if	ENT1
		(if (/= (cdr (assoc 0 (entget (car ENT1)))) "LINE")
		    (progn
			(princ "\nThat entity is not a line, select again."
			) ;_ End princ
			(setq ENT1 NIL)
		    ) ;_ End progn
		) ;_ End if
	    ) ;_ End if
	) ;_ End if
    ) ;_ End while to ensure an object is selected
    (setq PT1 (cdr (assoc 10 (entget (car ENT1))))
	  PT2 (cdr (assoc 11 (entget (car ENT1))))
    ) ;_ End setq
    (redraw (car ENT1) 3)
    (princ "\nSelect lines to be trimmed: ")
    (setq SSTOTRIM (ssget (list '(0 . "LINE"))))
    (if	SSTOTRIM			;Test for selection set
	(progn
	    (repeat (sslength SSTOTRIM)
		(setq TRMENT (ssname SSTOTRIM COUNT))
		(if (not (eq TRMENT (car ENT1)))
		    (progn
			(setq TRMDEF (entget TRMENT)
			      PT3    (cdr (assoc 10 TRMDEF))
			      PT4    (cdr (assoc 11 TRMDEF))
			      INTS   (inters PT1 PT2 PT3 PT4 NIL)
			      ANGL1  (angle PT3 PT4)
			      ANGL2  (- ANGL1 pi)
			      BP1    (polar INTS ANGL1 TRIMDIST)
			      BP2    (polar INTS ANGL2 TRIMDIST)
			      COUNT  (1+ COUNT)
			) ;_ End setq
			(command "break" TRMENT BP1 BP2)
		    ) ;_ End progn
		    (setq COUNT (1+ COUNT))
		) ;_ End if
	    ) ;_ End repeat
	) ;_ End then progn
	(princ "\nNothing in selection set.") ;_ Else print error
    ) ;_ End if for a selection set
    (redraw (car ENT1) 4)
    (setq *ERROR* OLDERROR)
    (setvar "osmode" OSM)
    (setvar "cmdecho" CMD)
    (vla-endundomark ADOC)
    (princ)
) ;_ End defun

(defun TRGERR (S)
    (if	(/= S "Function cancelled")
	(princ (strcat "\nError: " S))
    ) ;_ End if
    (if	ENT1
	(redraw (car ENT1) 4)
    ) ;_ End if
    (setq *ERROR* OLDERROR)
    (setvar "osmode" OSM)
    (setvar "cmdecho" CMD)
    (vla-endundomark ADOC)
    (princ)
) ;_ End defun