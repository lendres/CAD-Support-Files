;;; create-text-styles.lsp
;;; Copyright 2000-2004 by Lance A. Endres

;;; Revision History
;;; March 8, 2004
;;; Updated for AutoCAD 2004
;;; Changed doslib loading to load a 2000 or a 2004 depending on
;;; what is running.

(defun CREATE-TEXT-STYLES (/	       DIMS	   DOSLIBFOUND
			   FONTFILE    RMNS	   SIZE3-32
			   SIZE3-16    SIZE10PT	   SIZE12PT
			   SIZE14PT    SIZE16PT	   SIZE18PT
			   SIZE20PT    SIZE22PT	   SIZE24PT
			   SIZE28PT    SIZE32PT	   TEMP1
			  )

;;; Load doslib if necessary.

    (setq DOSLIBFOUND 0)

    (if	(= "Visual LISP 2018 (en)" (ver))
	(progn
	    (if	(not (member "DOSLib22x64.arx" (arx)))
		(progn
		    (arxload (findfile "DOSLib22x64.arx"))
		    (princ)
		    (setq DOSLIBFOUND 1)
		) ;_ End progn
	    ) ;_ End if
	) ;_ End PROGN
    ) ;_ End if


    (if	(= "Visual LISP 2015 (en)" (ver))
	(progn
	    (if	(not (member "DOSLib20x64.arx" (arx)))
		(progn
		    (arxload (findfile "DOSLib20x64.arx"))
		    (princ)
		    (setq DOSLIBFOUND 1)
		) ;_ End progn
	    ) ;_ End if
	) ;_ End PROGN
    ) ;_ End if

    (if	(or (= "Visual LISP 2004 (en)" (ver))
	    (= "Visual LISP 2006 (en)" (ver))
	) ;_ End or
	(progn
	    (if	(not (member "doslib16.arx" (arx)))
		(progn
		    (arxload (findfile "doslib16.arx"))
		    (princ)
		    (setq DOSLIBFOUND 1)
		) ;_ End progn
	    ) ;_ End if
	) ;_ End PROGN
    ) ;_ End if
    (if	(or (= "Visual LISP 2000 (en)" (ver))
	    (= "Visual LISP 2002 (en)" (ver))
	) ;_ End or
	(progn
	    (if	(not (member "doslib15.arx" (arx)))
		(progn
		    (arxload (findfile "doslib15.arx"))
		    (princ)
		    (setq DOSLIBFOUND 1)
		) ;_ End progn
	    ) ;_ End if
	) ;_ End progn
    ) ;_ End IF

					;    (if (= DOSLIBFOUND 0)
					;	(princ "Dos lib failed to load in 'create-text-styles'")
					;    ) ;_ End IF

;;; Establish constants for the text sizes.  These constants are the
;;; only location modifications are needed to change the height of fonts.

    (setq SIZE3-32 (/ 3.0 32.0)
	  SIZE3-16 (/ 3.0 16.0)
	  SIZE10PT 0.0937500
	  SIZE12PT 0.1093750
	  SIZE14PT 0.1328125
	  SIZE16PT 0.1406250
	  SIZE18PT 0.1640625
	  SIZE20PT 0.1875000
	  SIZE22PT 0.2031250
	  SIZE24PT 0.2187500
	  SIZE28PT 0.2656250
	  SIZE32PT 0.3125000
    ) ;_ End setq

;;; If the output (plotting) units are System Internation, then convert
;;; the font sizes to millimeters.

    (if	(= (getcfg "AppData/Setup_Data/Plot_Unts") "si")
	(setq SIZE3-32 (* SIZE3-32 25.4)
	      SIZE3-16 (* SIZE3-16 25.4)
	      SIZE10PT (* SIZE10PT 25.4)
	      SIZE12PT (* SIZE12PT 25.4)
	      SIZE14PT (* SIZE14PT 25.4)
	      SIZE16PT (* SIZE16PT 25.4)
	      SIZE18PT (* SIZE18PT 25.4)
	      SIZE20PT (* SIZE20PT 25.4)
	      SIZE22PT (* SIZE22PT 25.4)
	      SIZE24PT (* SIZE24PT 25.4)
	      SIZE28PT (* SIZE28PT 25.4)
	      SIZE32PT (* SIZE32PT 25.4)
	) ;_ End setq
    ) ;_ End if

    (setq  DIMS		(getvar "dimscale")
	  RMNS		(* DIMS SIZE3-32)
	  SIZE10PT	(* DIMS SIZE10PT)
	  SIZE12PT	(* DIMS SIZE12PT)
	  SIZE14PT	(* DIMS SIZE14PT)
	  SIZE16PT	(* DIMS SIZE16PT)
	  SIZE18PT	(* DIMS SIZE18PT)
	  SIZE20PT	(* DIMS SIZE20PT)
	  SIZE22PT	(* DIMS SIZE22PT)
	  SIZE24PT	(* DIMS SIZE24PT)
	  SIZE28PT	(* DIMS SIZE28PT)
	  SIZE32PT	(* DIMS SIZE32PT)
    ) ;_ End setq

;;; Add font styles.
;;; LMBD1 is the style name.
;;; LMBD2 is the font file name.
;;; LMBD3 is the font height.
;;; LMBD4 is the text width

    (mapcar 'CREATE-STYLE
	    (list "Title"	   "TitlePS"	    "RomansPS"
		  "Standard"	   "Romans"	    "Arial"
		  "ArialPS"	   "Symbol"	    "Arial10Pt"
		  "Arial12Pt"	   "Arial14Pt"	    "Arial16Pt"
		  "Arial18Pt"	   "Arial20Pt"	    "Arial22Pt"
		  "Arial24Pt"	   "Arial28Pt"	    "Arial32Pt"
		  "Romans10Pt"	   "Romans12Pt"	    "Romans14Pt"
		  "Romans16Pt"	   "Romans18Pt"	    "Romans20Pt"
		  "Romans22Pt"	   "Romans24Pt"	    "Romans28Pt"
		  "Romans32Pt"	   "Symbol10Pt"	    "Symbol12Pt"
		  "Symbo14lPt"	   "Symbol16Pt"	    "Symbol18Pt"
		  "Symbol20Pt"	   "Symbol22Pt"	    "Symbol24Pt"
		  "Symbol28Pt"	   "Symbol32Pt"
		 ) ;_ End list
 	    (list "arial.ttf"	    "arial.ttf"	      "romans.shx"
		  "romans.shx"	    "romans.shx"      "arial.ttf"
		  "arial.ttf"	    "symbol.ttf"      "arial.ttf"
		  "arial.ttf"	    "arial.ttf"	      "arial.ttf"
		  "arial.ttf"	    "arial.ttf"	      "arial.ttf"
		  "arial.ttf"	    "arial.ttf"	      "arial.ttf"
		  "romans.shx"	    "romans.shx"      "romans.shx"
		  "romans.shx"	    "romans.shx"      "romans.shx"
		  "romans.shx"	    "romans.shx"      "romans.shx"
		  "romans.shx"	    "symbol.ttf"      "symbol.ttf"
		  "symbol.ttf"	    "symbol.ttf"      "symbol.ttf"
		  "symbol.ttf"	    "symbol.ttf"      "symbol.ttf"
		  "symbol.ttf"	    "symbol.ttf"
		 ) ;_ End list
 	    (list (* DIMS SIZE3-16)
		  SIZE3-16
		  SIZE3-32
		  RMNS
		  RMNS
		  RMNS
		  SIZE3-32
		  RMNS
		  SIZE10PT
		  SIZE12PT
		  SIZE14PT
		  SIZE16PT
		  SIZE18PT
		  SIZE20PT
		  SIZE22PT
		  SIZE24PT
		  SIZE28PT
		  SIZE32PT
		  SIZE10PT
		  SIZE12PT
		  SIZE14PT
		  SIZE16PT
		  SIZE18PT
		  SIZE20PT
		  SIZE22PT
		  SIZE24PT
		  SIZE28PT
		  SIZE32PT
		  SIZE10PT
		  SIZE12PT
		  SIZE14PT
		  SIZE16PT
		  SIZE18PT
		  SIZE20PT
		  SIZE22PT
		  SIZE24PT
		  SIZE28PT
		  SIZE32PT
	    ) ;_ End list
	    (list 1.0	1.0   0.9   0.9	  0.9	1.0   1.0   1.0	  1.0
		  1.0	1.0   1.0   1.0	  1.0	1.0   1.0   1.0	  1.0
		  1.0	1.0   1.0   1.0	  1.0	1.0   1.0   1.0	  1.0
		  1.0	1.0   1.0   1.0	  1.0	1.0   1.0   1.0	  1.0
		  1.0	1.0
		 ) ;_ End list
     ) ;_ End mapcar
) ;_ End defun

(defun CREATE-STYLE (STYLENAME FILENAME FONTHEIGHT TEXTWIDTH / TEMP1 VL-TEXTSTYLES VL-TSTYLE)
    (setq VL-TEXTSTYLES
	     (vla-get-textstyles
		 (vla-get-activedocument
		     (vlax-get-acad-object)
		 ) ;_ End vla-get-activedocument
	     ) ;_ End vla-get-textstyles
    ) ;_ End setq
    (setq VL-TSTYLE (vla-add VL-TEXTSTYLES STYLENAME)
	  TEMP1	    (findfile FILENAME)
    ) ;_ End setq
    (if	(not TEMP1)
	(setq TEMP1
		 (findfile
		     (strcat (dos_windir) "FONTS\\" FILENAME)
		 ) ;_ End findfile
	) ;_ End setq
    ) ;_ End if
    (if	TEMP1
	(vla-put-fontfile VL-TSTYLE TEMP1)
	(princ (strcat "\nThe font file " FILENAME " was not found."))
    ) ;_ End if
    (vla-put-height VL-TSTYLE FONTHEIGHT)
    (vla-put-width VL-TSTYLE TEXTWIDTH)
) ;_ End defun

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;