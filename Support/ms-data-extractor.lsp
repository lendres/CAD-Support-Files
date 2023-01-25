;;; MS-DATA-EXTRACTOR
;;; Extracts in use layer, linetype, fonts, hatches, and
;;; colors for use in conversions or other informational
;;; purposes.

(defun C:MS-DATA-EXTRACTOR
			   (/	     ACSSTS   CMD      CNT
			    DWG_NAME FILE_HAND	       FILTLST
			    SSVL     SSXPL    PREFIX   TEMP
			    VERS
			   )
    (setq OLDERR  *ERROR*
	  *ERROR* MS-DATA-EXTRACTOR-ERR
	  CMD	  (getvar "cmdecho")
	  PREFIX  (strcat (getvar "dwgprefix")
			  (substr (getvar "dwgname")
				  1
				  (- (strlen (getvar "dwgname")) 4)
			  ) ;_ End substr
		  ) ;_ End strcat
	  SHXLST  '()
	  TTFLST  '()
	  TEMPLST '()
	  VERS	  (substr (getvar "acadver") 1 2)
    ) ;_ End setq
    (setvar "cmdecho" 0)

;;; Load visual lisp functions

    (vl-load-com)

;;; Write layer names to a file.

    (GET-TBL-DATA
	"layer"
	(open (strcat PREFIX "-layers.txt") "w")
	2
    ) ;_ End GET-TBL-DATA

;;; Write linetype names to a file.

    (GET-TBL-DATA
	"ltype"
	(open (strcat PREFIX "-linetypes.txt") "w")
	2
    ) ;_ End GET-TBL-DATA

;;; Write font names to a file based on the type of font.

    (setq TEMP (cdr (assoc 3 (tblnext "style" t))))
    (if	TEMP
	(progn
	    (MAKE-FONT-LISTS (strcase TEMP))
	    (while (setq TEMP (cdr (assoc 3 (tblnext "style"))))
		(MAKE-FONT-LISTS (strcase TEMP))
	    ) ;_ End while
	) ;_ End progn
    ) ;_ End if
    (WRITE-LIST SHXLST (strcat PREFIX "-shxfonts.txt"))
    (WRITE-LIST TTFLST (strcat PREFIX "-ttffonts.txt"))

;;; Gather all hatches in the drawing and gather the names
;;; of the hatch patterns used.

    (setq TEMP (ssget "x" (list '(0 . "HATCH"))))
    (if	TEMP
	(progn
	    (setq CNT 0)
	    (repeat (sslength TEMP)
		(if (not (member
			     (cdr (assoc 2 (entget (ssname TEMP CNT))))
			     TEMPLST
			 ) ;_ End member
		    ) ;_ End not
		    (setq TEMPLST
			     (append
				 TEMPLST
				 (list
				     (cdr
					 (assoc
					     2
					     (entget (ssname TEMP CNT)
					     ) ;_ End entget
					 ) ;_ End assoc
				     ) ;_ End cdr
				 ) ;_ End list
			     ) ;_ End append
		    ) ;_ End setq
		) ;_ End if
		(setq CNT (1+ CNT))
	    ) ;_ End repeat
	) ;_ End progn
    ) ;_ End if

;;; Write hatch names to a file.

    (WRITE-LIST TEMPLST (strcat PREFIX "-hatchpat.txt"))

;;; Gather colors.

;;; First, search the block table and create a filter list for inserts,
;;; excluding inserts that have the names of x-reference names.

    (setq TEMP	  (tblnext "BLOCK" t)
	  FILTLST (append (list '(0 . "INSERT")) (list '(-4 . "<AND")))
    ) ;_ End setq
    (if	(assoc 1 TEMP)
	(setq FILTLST (append FILTLST
			      (list '(-4 . "<NOT"))
			      (list (assoc 2 TEMP))
			      (list '(-4 . "NOT>"))
		      ) ;_ End append
	) ;_ End setq
    ) ;_ End if
    (while (setq TEMP (tblnext "BLOCK"))
	(if (assoc 1 TEMP)
	    (setq FILTLST (append FILTLST
				  (list '(-4 . "<NOT"))
				  (list (assoc 2 TEMP))
				  (list '(-4 . "NOT>"))
			  ) ;_ End append
	    ) ;_ End setq
	) ;_ End if
    ) ;_ End while
    (setq FILTLST (append FILTLST (list '(-4 . "AND>"))))

;;; Second, search the layers and gather any colors used.

    (setq TEMPLST (list (itoa (cdr (assoc 62 (tblnext "layer" t))))))
    (while (setq TEMP (cdr (assoc 62 (tblnext "layer"))))
	(if (not (member (itoa TEMP) TEMPLST))
	    (setq TEMPLST (append TEMPLST (list (itoa TEMP))))
	) ;_ End if
    ) ;_ End while

;;; Third, cycle through all colors using a ssget with the color
;;; number as the filter list.  If the ssget returns a value then
;;; an entity with that color exists and the color number is saved.

    (MAKE-COLOR-LIST)

;;; Fourth, while an insert (excluding xrefs) exist, explode them
;;; Then cycle through the colors again to test for colors that were
;;; assigned to nested entities.

    (command "_.undo" "begin")
    (if	(not (= VERS "15"))
	(alert
	    "AutoCAD version is does not\nsupport color extraction."
	) ;_ End alert
	(progn
	    (setq ACSSTS (vla-get-selectionsets
			     (vla-get-activedocument
				 (vlax-get-acad-object)
			     ) ;_ End vla-get-activedocument
			 ) ;_ End vla-get-selectionsets
	    ) ;_ End setq
	    (if	(SSETEXISTS-P "TEMP_SS1")
		(vla-delete (vla-item ACSSTS "TEMP_SS1"))
	    ) ;_ End if
	    (setq SSVL (vla-add ACSSTS "TEMP_SS1"))
	    (while (ssget "x" FILTLST)
		(vla-select SSVL acselectionsetprevious)
		(vlax-for ITEM SSVL (vla-explode ITEM))
		(vlax-for ITEM SSVL (vla-erase ITEM))
		(vla-delete SSVL)
		(vla-add ACSSTS "TEMP_SS1")
		(setq SSVL (vla-item ACSSTS "TEMP_SS1"))
		(MAKE-COLOR-LIST)
	    ) ;_ End while
	    (vla-delete SSVL)
	) ;_ End progn
    ) ;_ End if

;;; Write colors to a file

    (WRITE-LIST
	(acad_strlsort TEMPLST)
	(strcat PREFIX "-colors.txt")
    ) ;_ End WRITE-LIST
    (command "_.undo" "end" "_.u")
    (setvar "cmdecho" CMD)
    (setq *ERROR* OLDERR)
    (princ)
) ;_ End defun

(defun GET-TBL-DATA (TBLE FILE_HAND ASSC / LINET)
    (setq LINET (cdr (assoc ASSC (tblnext TBLE t))))
    (if	LINET
	(write-line (strcase LINET) FILE_HAND)
    ) ;_ End if
    (while (setq LINET (cdr (assoc ASSC (tblnext TBLE))))
	(write-line (strcase LINET) FILE_HAND)
    ) ;_ End while
    (close FILE_HAND)
) ;_ End defun

(defun WRITE-LIST (TEMP FILE / CNT)
    (setq CNT	    0
	  FILE_HAND (open FILE "w")
    ) ;_ End setq
    (repeat (length TEMP)
	(write-line (nth CNT TEMP) FILE_HAND)
	(setq CNT (1+ CNT))
    ) ;_ End repeat
    (close FILE_HAND)
) ;_ End defun

(defun MAKE-FONT-LISTS (TEMP)
    (setq EXTEN (strlen TEMP))
    (if	(<= EXTEN 3)
	(setq EXTEN NIL)
	(setq EXTEN (substr TEMP (- (strlen TEMP) 3))
	) ;_ End setq
    ) ;_ End if
    (if	(/= TEMP "")
	(if (= EXTEN ".TTF")
	    (if	(not (member TEMP TTFLST))
		(setq TTFLST (append TTFLST (list TEMP)))
	    ) ;_ End if
	    (if	(not (member TEMP SHXLST))
		(if (= EXTEN ".SHX")
		    (setq SHXLST (append SHXLST (list TEMP)))
		    (setq
			SHXLST (append SHXLST
				       (list (strcat TEMP ".shx"))
			       ) ;_ End append
		    ) ;_ End setq
		) ;_ End if
	    ) ;_ End if
	) ;_ End if
    ) ;_ End if
) ;_ End defun

(defun MAKE-COLOR-LIST (/ CNT TEMP)
    (setq CNT 1)
    (repeat 256
	(setq TEMP (ssget "x" (list (cons 62 CNT))))
	(if TEMP
	    (if	(not (member (itoa CNT) TEMPLST))
		(setq TEMPLST (append TEMPLST (list (itoa CNT))))
	    ) ;_ End if
	) ;_ End if
	(setq CNT (1+ CNT))
    ) ;_ End repeat
) ;_ End defun


(defun MS-DATA-EXTRACTOR-ERR (S)
    (if	(/= S "Function cancelled")
	(princ (strcat "\nError: " S))
    ) ;_ End if
    (setq *ERROR* OLDERR)
    (setvar "cmdecho" CMD)
    (princ)
) ;_ End defun