;;; Copyright Lance A. Endres

(defun C:UNMIRROR (/ ATT-INS-LIST CMD CNT1 CNT2 ENT1 ENT1DEF EPT PREC SC1 SC2 SC3 
                   SS_BLCKS SS_LINES SPT UNITS XEPT XSPT YEPT YSPT
                  ) 
  (setq OLDERR  *ERROR*
        *ERROR* UNMIRRERR
        CMD     (getvar "cmdecho")
        PREC    (getvar "luprec")
        UNITS   (getvar "lunits")
        ROTLST  NIL
  ) ;_ End setq
  (setvar "cmdecho" 0)
  (command "_.undo" "begin")
  (princ "\nSelect blocks or lines to unmirror.")

  ;;; Create selection set.
  ;;; Filter out blocks that are mirrored.
  (setq SS_BLCKS (ssget)
        SS_BLCKS (ssget 
                   "P"
                   (list 
                     '(0 . "INSERT")
                     '(-4 . "<OR")
                     '(-4 . "<")
                     '(41 . 0.0)
                     '(-4 . "<")
                     '(42 . 0.0)
                     '(-4 . "<")
                     '(43 . 0.0)
                     '(-4 . "OR>")
                   ) ;_ End list
                 ) ;_ End ssget
         ;;; Filter out lines
        SS_LINES (ssget "P" (list '(0 . "LINE")))
        CNT1     0
        CNT2     41
  ) ;_ End setq
  (if SS_BLCKS 
    (setq ENT1 (ssname SS_BLCKS CNT1))
  ) ;_ End if
  (while ENT1 
    (setq ENT1DEF (entget ENT1))
    (repeat 3 
      (setq SC1     (assoc CNT2 ENT1DEF)
            SC2     (cdr SC1)
            SC2     (abs SC2)
            SC3     (cons CNT2 SC2)
            CNT2    (1+ CNT2)
            ENT1DEF (subst SC3 SC1 ENT1DEF)
      ) ;_ End setq
    ) ;_ End repeat
    (entmod ENT1DEF)
    (entupd ENT1)
    (ROTATE-SUB ENT1 ENT1DEF)
    (if (= (cdr (assoc 66 ENT1DEF)) 1) 
      (progn 
        (setq ATT-INS-LIST (BUILD-ATT-INS-LIST 
                             (cdr (assoc 2 ENT1DEF))
                           ) ;_ End BUILD-ATT-INS-LIST
        ) ;_ End setq
        (UNMIRROR-ATTRIBS ATT-INS-LIST ENT1)
      ) ;_ End progn
    ) ;_ End if
    (setq CNT1 (1+ CNT1)
          CNT2 41
          ENT1 (ssname SS_BLCKS CNT1)
    ) ;_ End setq
  ) ;_ End while

  ;;; Unmirror lines.
  (if SS_LINES 
    (setq ENT1 (ssname SS_LINES CNT1)
          CNT1 0
    ) ;_ End setq
    (setq ENT1 NIL)
  ) ;_ End if
  (while ENT1 
    (setq ENT1DEF (entget ENT1)
          SPT     (cdr (assoc 10 ENT1DEF))
          XSPT    (atof (rtos (car SPT) UNITS PREC))
          YSPT    (atof (rtos (cadr SPT) UNITS PREC))
          EPT     (cdr (assoc 11 ENT1DEF))
          XEPT    (atof (rtos (car EPT) UNITS PREC))
          YEPT    (atof (rtos (cadr EPT) UNITS PREC))
    ) ;_ End setq
    (cond 
      ((or 
         (> XSPT XEPT)
         (and (= XSPT XEPT) (> YSPT YEPT))
       ) ;_ End or
       (setq ENT1DEF (subst (cons 10 EPT) (cons 10 SPT) ENT1DEF)
             ENT1DEF (subst (cons 11 SPT) (cons 11 EPT) ENT1DEF)
       ) ;_ End setq
       (entmod ENT1DEF)
       (entupd ENT1)
      ) ;_ End cond test for end points
    ) ;_ End cond
    (setq CNT1 (1+ CNT1)
          ENT1 (ssname SS_LINES CNT1)
    ) ;_ End setq
  ) ;_ End while

  ;;; Exit

  (setq *ERROR* OLDERR)
  (command "_.undo" "end")
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun

(defun BUILD-ATT-INS-LIST (BLCK-NM / ATT-INS-LIST BLCK-REC-ENT-NM-1 BLCK-REC-ENT-NM-2 
                           ENT1DEF ENT1TYPE TBLDEF
                          ) 
  (setq TBLDEF            (tblsearch "BLOCK" BLCK-NM)
        ENT1DEF           (entget (cdr (assoc -2 TBLDEF)))
        ENT1TYPE          (cdr (assoc 0 ENT1DEF))
        BLCK-REC-ENT-NM-1 (cdr 
                            (assoc 
                              5
                              (entget (cdr (assoc 330 ENT1DEF)))
                            ) ;_ End assoc
                          ) ;_ End cdr
        BLCK-REC-ENT-NM-2 BLCK-REC-ENT-NM-1
  ) ;_ End setq
  (while 
    (and 
      (or 
        (= BLCK-REC-ENT-NM-1 BLCK-REC-ENT-NM-2)
        (= ENT1TYPE "ATTRIB")
        (= ENT1TYPE "SEQEND")
      ) ;_ End or
      (/= BLCK-REC-ENT-NM-2 NIL)
    ) ;_ End and
    (if (= ENT1TYPE "ATTDEF") 
      (setq ATT-INS-LIST (append 
                           ATT-INS-LIST
                           (list 
                             (cons 
                               (cdr (assoc 2 ENT1DEF))
                               (assoc 10 ENT1DEF)
                             ) ;_ End cons
                           ) ;_ End list
                         ) ;_ End append
      ) ;_ End setq
    ) ;_ End if
    (setq ENT1DEF (entnext (cdr (assoc -1 ENT1DEF))))
    (if ENT1DEF 
      (setq ENT1DEF           (entget ENT1DEF)
            ENT1TYPE          (cdr (assoc 0 ENT1DEF))
            BLCK-REC-ENT-NM-2 (cdr 
                                (assoc 
                                  5
                                  (entget 
                                    (cdr (assoc 330 ENT1DEF))
                                  ) ;_ End entget
                                ) ;_ End assoc
                              ) ;_ End cdr
      ) ;_ End setq
      (setq BLCK-REC-ENT-NM-2 NIL)
    ) ;_ End if
  ) ;_ End while
  ATT-INS-LIST
) ;_ End defun

(defun UNMIRROR-ATTRIBS (ATT-INS-LIST ENT1 / ATTR1 ATTR1DEF ATTINSPT ENT1DEF INS 
                         NATTINSPT ROT
                        ) 
  (setq ENT1DEF  (entget ENT1)
        ROT      (cdr (assoc 50 ENT1DEF))
        INS      (cdr (assoc 10 ENT1DEF))
        ATTR1    (entnext ENT1)
        ATTR1DEF (entget ATTR1)
  ) ;_ End setq
  (while (/= (cdr (assoc 0 ATTR1DEF)) "SEQEND") 
    (setq ATTINSPT  (cdr (assoc (cdr (assoc 2 ATTR1DEF)) ATT-INS-LIST))
          ATTR1DEF  (subst 
                      (cons 41 (- 0 (cdr (assoc 41 ATTR1DEF))))
                      (assoc 41 ATTR1DEF)
                      ATTR1DEF
                    ) ;_ End subst
          NATTINSPT (polar 
                      (polar INS ROT (cadr ATTINSPT))
                      (+ ROT (/ pi 2))
                      (caddr ATTINSPT)
                    ) ;_ End polar
          ATTR1DEF  (subst 
                      (cons 
                        10
                        (list 
                          (car NATTINSPT)
                          (cadr NATTINSPT)
                          (+ (caddr INS) (caddr NATTINSPT))
                        ) ;_ End list
                      ) ;_ End cons
                      (assoc 10 ATTR1DEF)
                      ATTR1DEF
                    ) ;_ End subst
    ) ;_ End setq
    (entmod ATTR1DEF)
    (entupd ATTR1)
    (setq ATTR1    (entnext ATTR1)
          ATTR1DEF (entget ATTR1)
    ) ;_ End setq
  ) ;_ End while
) ;_ End defun

(defun ROTATE-SUB (ENT1 ENT1DEF / TEMP) 
  (if (setq TEMP (assoc (cdr (assoc 2 ENT1DEF)) ROTLST)) 
    (if (= (cdr TEMP) "Rotate") 
      (ROTATE-BLOCK-180 ENT1 ENT1DEF)
    ) ;_ End if
    (progn 
      (redraw ENT1 3)
      (initget "Yes No")
      (setq TEMP (getkword 
                   "\nRotate the block 180 degrees? Yes/<No>: "
                 ) ;_ End getkword
      ) ;_ End setq
      (redraw ENT1 4)
      (if (= TEMP "Yes") 
        (progn 
          (ROTATE-BLOCK-180 ENT1 ENT1DEF)
          (APPEND-ROTLST ENT1DEF "Rotate")
        ) ;_ End progn
        (APPEND-ROTLST ENT1DEF "Do Not Rotate")
      ) ;_ End if
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun

(defun APPEND-ROTLST (ENT1DEF VALUE) 
  (setq ROTLST (append 
                 ROTLST
                 (list 
                   (cons 
                     (cdr (assoc 2 ENT1DEF))
                     VALUE
                   ) ;_ End cons
                 ) ;_ End list
               ) ;_ End append
  ) ;_ End setq
) ;_ End defun

(defun ROTATE-BLOCK-180 (ENT1 ENT1DEF / ROT1 ROT2 ROT3) 
  (setq ROT1 (assoc 50 ENT1DEF)
        ROT2 (cdr ROT1)
  ) ;_ End setq
  (if (<= ROT2 pi) 
    (setq ROT2 (+ ROT2 pi))
    (setq ROT2 (- ROT2 pi))
  ) ;_ End if
  (setq ROT3    (cons 50 ROT2)
        ENT1DEF (subst ROT3 ROT1 ENT1DEF)
  ) ;_ End setq
  (entmod ENT1DEF)
  (entupd ENT1)
) ;_ End defun

(defun UNMIRRERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun