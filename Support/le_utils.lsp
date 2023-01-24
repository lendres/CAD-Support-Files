;;; le_utils.lsp
;;; Copyright Lance A. Endres

;;; Requires the use of ai_utils.lsp

;;; le_utils is a miscellaneous group of routines used
;;; in various files.

;;; The following program counts the number of items in a
;;; DCL list box that are highlighted.  Used for error handling

(defun CNT_TILE_ITEMS (WHICH / CNT1 CNT2 LOC SLEN TEMP) 
  (setq LOC  (get_tile WHICH)
        CNT1 0
        SLEN (strlen LOC)
  ) ;_ End setq
  (while (and (/= LOC NIL) (/= LOC "")) 
    (setq TEMP (substr LOC 1 1)
          CNT2 2
    ) ;_ End setq
    (while (and (/= TEMP " ") (/= TEMP "")) 
      (setq TEMP (substr LOC CNT2 1)
            CNT2 (1+ CNT2)
      ) ;_ End setq
    ) ;_ End while
    (setq LOC  (substr LOC CNT2 SLEN)
          CNT1 (1+ CNT1)
          SLEN (strlen LOC)
    ) ;_ End setq
  ) ;_ End while
  CNT1
) ;_ End defun

;;; The following programs are used to extract text from
;;; a text file, removing preceding and ending tabs and
;;; spaces.  Lines without text are ignored.
(defun GETNEWLINE (FILE_HAND / LINET) 
  (setq LINET (read-line FILE_HAND))
  (if LINET 
    (progn 
      (setq LINET (AI_STRTRIM LINET)
            LINET (REMSPTABS LINET FILE_HAND)
      ) ;_ End setq
      (while (= LINET "") 
        (setq LINET (REMSPTABS LINET FILE_HAND))
      ) ;_ End while
      LINET
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun

(defun REMSPTABS (LINET FILE_HAND / LOOP1) 
  (setq LOOP1 t)
  (while (= LINET "") 
    (setq LINET (read-line FILE_HAND)) ;_ End setq
    (if LINET 
      (setq LINET (AI_STRTRIM LINET))
    ) ;_ End if
  ) ;_ End while
  (while (and LINET LOOP1) 
    (if (= (substr LINET 1 1) "\t") 
      (setq LINET (substr LINET 2))
      (setq LOOP1 NIL)
    ) ;_ End if
    (if (/= LINET "") 
      (if (= (substr LINET (strlen LINET)) "\t") 
        (setq LINET (substr LINET 1 (- (strlen LINET) 1))
              LOOP1 t
        ) ;_ End setq
      ) ;_ End if
      (setq LOOP1 NIL)
    ) ;_ End if
    (setq LINET (AI_STRTRIM LINET))
  ) ;_ End while
  LINET
) ;_ End defun

;;; Truncate trailing zeros past the value of luprec.  The return value is based on the units
;;; of UNITS passed to the function.
(defun LE_RTOS (RL UNITS / DMZN RET1 RET2 RETURN) 

  ;;; Store the value of dimzin.
  (setq DMZN (getvar "dimzin"))

  ;;; Turn on bit 3 to ensure trailing 0 inches are included.
  (setvar "dimzin" (logior DMZN 3))

  (if (or (= UNITS 4) (= UNITS 5)) 

    ;;; Arch of fractional units are ok.
    (setq RETURN (rtos RL UNITS))
    (progn 

      ;;; Turn off bit 8
      (setvar "dimzin" (logand DMZN (~ 8)))
      (setq RET1 (rtos RL UNITS))

      ;;; Turn on bit 8
      (setvar "dimzin" (logior DMZN 8))
      (setq RET2 (rtos RL UNITS 15))

      ;;; Check to insure that the values are equal
      (setq RETURN (if (equal (distof RET1) (distof RET2) 0.000001) 
                     RET1
                     RET2
                   ) ;_ End if
      ) ;_ End setq
    ) ;_ End progn
  ) ;_ End if

  ;;; Restore dimzin
  (setvar "dimzin" DMZN)

  RETURN
) ;_ End defun

(load "AI_UTILS")