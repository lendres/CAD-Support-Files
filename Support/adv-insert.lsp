;;; Copyright 1998-2000 by Lance A. Endres

(defun C:IN () 
  (INNAME "DIMSCALE" "ROTATE")
) ;_ End defun

(defun C:INN () 
  (INNAME 1 "ROTATE")
) ;_ End defun

(defun C:INNR () 
  (INNAME "DIMSCALE" 0)
) ;_ End defun

(defun C:INR () 
  (INNAME "DIMSCALE" 0)
) ;_ End defun

(defun INNAME (INSSCALE ROTANG / ATT BLKDEF CMD N_INS_NAME ORTH VL-ADOC) 
  (setq OLDERR  *ERROR*
        *ERROR* ADV-INSERTERR
        VL-ADOC (vla-get-activedocument (vlax-get-acad-object))
        ATT     (vla-getvariable VL-ADOC "attdia")
        CMD     (vla-getvariable VL-ADOC "cmdecho")
        ORTH    (vla-getvariable VL-ADOC "orthomode")
  ) ;_ End setq
  (princ 
    (strcat "\nEnter block name or [?] <" (getvar "insname"))
  ) ;_ End princ
  (initget 128 "?")
  (setq N_INS_NAME (getkword ">: "))
  (if (= N_INS_NAME "?") 
    (progn 
      (textscr)
      (command "_.-insert" "?" "")

      ;;;	    (setq BLKDEF (tblnext "block" t))
      ;;;	    (if	(and (= (logand 16 (cdr (assoc 70 BLKDEF))) 0)
      ;;;		     (= (logand 4 (cdr (assoc 70 BLKDEF))) 0)
      ;;;		) ;_ End and
      ;;;		(princ (cdr (assoc 2 BLKDEF)))
      ;;;	    ) ;_ End if
      ;;;	    (while (setq BLKDEF (tblnext "block"))
      ;;;		(if (and (= (logand 16 (cdr (assoc 70 BLKDEF))) 0)
      ;;;			 (= (logand 4 (cdr (assoc 70 BLKDEF))) 0)
      ;;;		    ) ;_ End and
      ;;;		    (print (cdr (assoc 2 BLKDEF)))
      ;;;		) ;_ End if
      ;;;	    ) ;_ End while
    ) ;_ End progn
    (progn 
      (if (or (= N_INS_NAME "") (null N_INS_NAME)) 
        (setq INS_NAME (getvar "insname"))
        (setq INS_NAME N_INS_NAME)
      ) ;_ End if
      (setq INS_SCALE INSSCALE
            ROT_ANG   ROTANG
      ) ;_ End setq
      (ADV-INSERT-SUB)
    ) ;_ End progn
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun

;;; ADV-INSERT is a dummy program created so that buttons can call it and the
;;;	INNAME program can call the same subroutine as the buttons.  The buttons
;;;	have to call an actual program, not a subroutine to make them repeatable.
;;; ADV-INSERT-SUB requires the following global variables to be set correctly.
;;; INS_NAME - The name of the block to be inserted.  It must be in the drawing
;;;	or in the search path.
;;; INS_SCALE - The insert scale of the block to be inserted or the string
;;;	"DIMSCALE".
;;; ROT_ANG - The rotation angle of the block, or the string "ROTATE" which
;;;	will cause the program to pause for the user to set the rotation angle.

(defun C:ADV-INSERT () 
  (ADV-INSERT-SUB)
  (princ)
) ;_ End defun

(defun ADV-INSERT-SUB (/ ATT DIMS LMBD1 LMBD2 TEMP1 ORTH VL-ADOC) 
  (vl-load-com)
  (setq OLDERR  *ERROR*
        *ERROR* ADV-INSERTERR
        VL-ADOC (vla-get-activedocument (vlax-get-acad-object))
        ATT     (vla-getvariable VL-ADOC "attdia")
        CMD     (vla-getvariable VL-ADOC "cmdecho")
        ORTH    (vla-getvariable VL-ADOC "orthomode")
  ) ;_ End setq

  ;;; Exit if variables are not set.

  (if (or (null INS_NAME) (null INS_SCALE) (null ROT_ANG)) 
    (progn 
      (princ "\nInsert name, scale, or rotation angle not set.")
      (exit)
    ) ;_ End progn
  ) ;_ End if

  ;;; Ensure that the INS_SCALE and ROT_ANG are capitalize if they are set
  ;;; to "DIMSCALE" or "ROTATE" respectively.

  (foreach TEMP1 '(INS_SCALE ROT_ANG) 
    (if (not (numberp (eval TEMP1))) 
      (set TEMP1 (strcase (eval TEMP1)))
    ) ;_ End if
  ) ;_ End foreach

  ;;; Set the INS_SCALE to the dimscale if the key word "DIMSCALE" was used.

  (if (= INS_SCALE "DIMSCALE") 
    (setq INS_SCALE (getvar "dimscale"))
  ) ;_ End if

  ;;; Set system variables.  Turn of attribute dialog box so that it doesn't
  ;;; interfere with the insertion.  Turn of command line echoing and set
  ;;; orthomode to on to make setting the rotation easier.

  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "attdia" "cmdecho" "orthomode")
    (list 0 0 1)
  ) ;_ End mapcar

  ;;; Restore command line echoing so that the prompts of the insert can
  ;;; be viewed.

  (vla-setvariable VL-ADOC "cmdecho" 1)
  (if (= ROT_ANG "ROTATE") 
    (command "insert" INS_NAME PAUSE INS_SCALE "" PAUSE)
    (command "insert" INS_NAME PAUSE INS_SCALE "" ROT_ANG)
  ) ;_ End if
  (vla-setvariable VL-ADOC "cmdecho" 0)

  ;;; Restore system variables.

  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "attdia" "cmdecho" "orthomode")
    (list ATT CMD ORTH)
  ) ;_ End mapcar
  (setq *ERROR* OLDERR)
) ;_ End defun

(defun ADV-INSERTERR (MSG / LMBD1 LMBD2 VL-ADOC) 
  (if (/= MSG "Function cancelled") 
    (princ (strcat "\nError: " MSG))
  ) ;_ End if
  (mapcar 
    '(lambda (LMBD1 LMBD2) 
       (vla-setvariable 
         (vla-get-activedocument (vlax-get-acad-object))
         LMBD1
         LMBD2
       ) ;_ End vla-setvariable
     ) ;_ End lambda
    (list "orthomode" "attdia" "cmdecho")
    (list ORTH ATT CMD)
  ) ;_ End mapcar
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun