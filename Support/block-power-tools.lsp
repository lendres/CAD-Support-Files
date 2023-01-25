;;; Copyright Lance A. Endres

(vl-load-com)
(load "sstools")
(load "frozen-locked-layer")

(defun C:RIP-NENT (/ CMD NENT NENTDEF SS1 VL-ADOC) 
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark VL-ADOC)
  (setq CMD (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (while (setq NENT (car (nentsel))) 
    (if 
      (= 
        (cdr (assoc 0 (setq NENTDEF (entget NENT))))
        "ATTRIB"
      ) ;_ End =
      (MOD-ATTRIB NENT 1)
    ) ;_ End if
    (if (entget NENT) 
      (vla-delete (vlax-ename->vla-object NENT))
    ) ;_ End if
    (vla-regen VL-ADOC acallviewports)
  ) ;_ End while
  (setvar "cmdecho" CMD)
  (vla-endundomark VL-ADOC) ;_ End vla-endundomark
  (princ)
) ;_ End defun

(defun C:SET-NENT-COLOR-BYLAYER (/ CMD CNT NENT SS1 VL-ADOC) 
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark VL-ADOC)
  (setq CMD (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (initget "Yes No")
  (setq MOVETOLAY0 (getkword 
                     "\nMove entities in block to layer zero also? [Yes/No] <Yes>: "
                   ) ;_ End getkword
  ) ;_ End setq
  (while (setq NENT (car (nentsel))) 
    (if (= (cdr (assoc 0 (entget NENT))) "ATTRIB") 
      (progn 
        (MOD-ATTRIB NENT 2)
      ) ;_ End progn
      (ENT-COLOR-BYLAYER (vlax-ename->vla-object NENT))
    ) ;_ End if
    (vla-regen VL-ADOC acallviewports)
  ) ;_ End while
  (setvar "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

(defun C:SET-BLOCK-COLOR-BYLAYER (/ CMD ENT1 ENT1DEF TBLDEF VLENT VL-ADOC) 
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark VL-ADOC)
  (setq CMD (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (while (not ENT1) 
    (setq ENT1 (entsel 
                 "\nSelect a block to change the color to bylayer: "
               ) ;_ End entsel
    ) ;_ End setq
    (if ENT1 
      (progn 
        (initget "Yes No")
        (setq MOVETOLAY0 (getkword 
                           "\nMove entities in block to layer zero also? [Yes/No] <Yes>: "
                         ) ;_ End getkword
        ) ;_ End setq
        (setq ENT1DEF (entget (car ENT1)))
        (if (= (cdr (assoc 0 ENT1DEF)) "INSERT") 
          (progn 
            (setq BLIST (list (cdr (assoc 2 ENT1DEF))))
          ) ;_ End progn
          (progn 
            (setq ENT1 NIL)
            (princ "\nThat entity is not a block")
          ) ;_ End progn
        ) ;_ End if
      ) ;_ End progn
    ) ;_ End if
  ) ;_ End while
  (while BLIST 
    (setq TBLDEF (tblsearch "BLOCK" (nth 0 BLIST)))
    (BLOCK-COLOR-BYLAYER TBLDEF)
    (MOD-BLCK-ATTRIBS (cdr (assoc 2 TBLDEF)) 1 NIL)
    (setq BLIST (vl-remove (nth 0 BLIST) BLIST))
  ) ;_ End while
  (vla-regen VL-ADOC acallviewports)
  (setvar "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

(defun C:ALL-BLOCKS-COLOR-BYLAYER (/ CMD TBLDEF VL-ADOC) 
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark VL-ADOC)
  (setq CMD (getvar "cmdecho")) ;_ End setq
  (setvar "cmdecho" 0)
  (initget "Yes No")
  (setq MOVETOLAY0 (getkword 
                     "\nMove entities in block to layer zero also? [Yes/No] <Yes>: "
                   ) ;_ End getkword
  ) ;_ End setq
  (setq TBLDEF (tblnext "BLOCK" t))
  (if TBLDEF 
    (progn 
      (BLOCK-COLOR-BYLAYER TBLDEF)
      (MOD-BLCK-ATTRIBS (cdr (assoc 2 TBLDEF)) 1 NIL)
    ) ;_ End progn
  ) ;_ End if
  (while (setq TBLDEF (tblnext "BLOCK")) 
    (BLOCK-COLOR-BYLAYER TBLDEF)
    (MOD-BLCK-ATTRIBS (cdr (assoc 2 TBLDEF)) 1 NIL)
  ) ;_ End while
  (vla-regen VL-ADOC acallviewports)
  (setvar "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

;;; This modifies the attribute width.  It needs to be fully tested for
;;; the getreal.  It also needs to modify the block definition so that
;;; an attsync will not revert the attributes back to the block definition.

(defun C:MOD-ALL-ATT-WIDTH (/ CMD ITEM VL-ADOC WID) 
  (vl-load-com)
  (initget 7)
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object))
        CMD     (vla-getvariable VL-ADOC "cmdecho")
        WID     (getreal "\nWidth for all attribute entities: ")
  ) ;_ End setq
  (vla-startundomark VL-ADOC)
  (vla-setvariable VL-ADOC "cmdecho" 0)
  (vlax-for ITEM (vla-get-blocks VL-ADOC) 
    (MOD-BLCK-ATTRIBS (vla-get-name ITEM) 2 WID)
  ) ;_ End vlax-for
  (vla-setvariable VL-ADOC "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

(defun BLOCK-COLOR-BYLAYER (BLCK-TBL-DEF / ACSSTS BLCK-REC-ENT-NM-1 BLCK-REC-ENT-NM-2 
                            ENT1DEF ENT1TYPE SSVL VLENT X
                           ) 
  (vl-load-com)
  (setq ENT1DEF           (entget (cdr (assoc -2 BLCK-TBL-DEF)))
        ENT1TYPE          (cdr (assoc 0 ENT1DEF))
        VLENT             (vlax-ename->vla-object (cdr (assoc -1 ENT1DEF)))
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
    (if (= (cdr (assoc 0 ENT1DEF)) "INSERT") 
      (setq BLIST (append BLIST (list (cdr (assoc 2 ENT1DEF))))) ;_ End setq
    ) ;_ End if
    (ENT-COLOR-BYLAYER VLENT)
    (setq ENT1DEF (entnext (cdr (assoc -1 ENT1DEF))))
    (if ENT1DEF 
      (setq ENT1DEF           (entget ENT1DEF)
            ENT1TYPE          (cdr (assoc 0 ENT1DEF))
            VLENT             (vlax-ename->vla-object (cdr (assoc -1 ENT1DEF)))
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
) ;_ End defun

(defun ENT-COLOR-BYLAYER (ENT1 / LAY-STAT-LIST) 
  (setq LAY-STAT-LIST (SET-FRZN-LOCK-LAY ENT1))
  (if (not (= (vla-get-color ENT1) 256)) 
    (vla-put-color ENT1 acbylayer)
  ) ;_ End if
  (if 
    (and 
      (/= MOVETOLAY0 "No")
      (not (= "0" (vla-get-layer ENT1)))
    ) ;_ End and
    (vla-put-layer ENT1 "0")
  ) ;_ End if
  (RESET-FRZN-LOCK-LAY ENT1 LAY-STAT-LIST)
) ;_ End defun

(defun MOD-BLCK-ATTRIBS (ENT1 BIT WID / SS1 ITEM SSVL VLENT X) 
  (setq SS1 (ssget 
              "x"
              (list 
                '(0 . "INSERT")
                (cons 2 ENT1)
              ) ;_ End list
            ) ;_ End ssget
  ) ;_ End setq
  (if SS1 
    (progn 
      (setq VLENT (vlax-ename->vla-object (ssname SS1 0)))
      (if (= (vla-get-hasattributes VLENT) :vlax-true) 
        (progn 
          (setq SSVL (EST-VL-SS))
          (vla-select SSVL acselectionsetprevious)
          (vlax-for ITEM SSVL 
            (mapcar 
              '(lambda (X) 
                 (if (= (logand BIT 1) 1) 
                   (ENT-COLOR-BYLAYER X)
                 ) ;_ End if
                 (if (= (logand BIT 2) 2) 
                   (vla-put-scalefactor X WID)
                 ) ;_ End if
               ) ;_ End lambda
              (vlax-safearray->list 
                (vlax-variant-value 
                  (vla-getattributes ITEM)
                ) ;_ End vlax-variant-value
              ) ;_ End vlax-safearray->list
            ) ;_ End mapcar
          ) ;_ End vlax-for
          (vla-delete SSVL)
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun

(defun MOD-ATTRIB (ENT1 BIT / ATTTAG CNT ENT1DEF SS1) 
  (setq ENT1DEF (entget ENT1)
        ATTTAG  (cdr (assoc 2 ENT1DEF))
        SS1     (ssget 
                  "x"
                  (list 
                    '(0 . "INSERT")
                    (assoc 2 (entget (cdr (assoc 330 ENT1DEF))))
                  ) ;_ End list
                ) ;_ End ssget
        CNT     0
  ) ;_ End setq
  (repeat (sslength SS1) 
    (setq ENT1DEF (entget (entnext (ssname SS1 CNT))))
    (while (= (cdr (assoc 0 ENT1DEF)) "ATTRIB") 
      (if (= (cdr (assoc 2 ENT1DEF)) ATTTAG) 
        (progn 
          (if (= (logand BIT 1) 1) 
            (vla-delete 
              (vlax-ename->vla-object 
                (cdr (assoc -1 ENT1DEF))
              ) ;_ End vlax-ename->vla-object
            ) ;_ End vla-delete
          ) ;_ End if
          (if (= (logand BIT 2) 2) 
            (ENT-COLOR-BYLAYER 
              (vlax-ename->vla-object 
                (cdr (assoc -1 ENT1DEF))
              ) ;_ End vlax-ename->vla-object
            ) ;_ End ENT-COLOR-BYLAYER
          ) ;_ End if
        ) ;_ End progn
      ) ;_ End if
      (setq ENT1DEF (entget (entnext (cdr (assoc -1 ENT1DEF)))))
    ) ;_ End while
    (setq CNT (1+ CNT))
  ) ;_ End repeat
) ;_ End defun

(defun *ERROR* (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (vla-endundomark 
    (vla-get-activedocument (vlax-get-acad-object))
  ) ;_ End vla-endundomark
  (princ)
) ;_ End defun

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
