;;; Copyright Lance A. Endres

(defun C:ADDPSTYLES (/ ACADDOC CMD COL ENT1 ISXREF STYLSHEET VL-ENT1 VL-BLOCKS-REF XREFFOUND) 
  (vl-load-com)
  (setq CMD           (getvar "cmdecho")
        ACADDOC       (vla-get-activedocument (vlax-get-acad-object))
        VL-BLOCKS-REF (vla-get-blocks ACADDOC)
        STYLSHEET     (vla-get-stylesheet (vla-get-activelayout ACADDOC))
  ) ;_ End setq
  (vla-startundomark ACADDOC)
  (setvar "cmdecho" 0)
  (if 
    (/= 
      (vlax-variant-value (vla-getvariable ACADDOC "pstylemode"))
      0
    ) ;_ End /=
    (progn 
      (alert "This drawing is not a named plot style drawing.")
      (exit)
    ) ;_ End progn
  ) ;_ End if
  (if 
    (and 
      (/= STYLSHEET "Standard - Full Size.stb")
      (/= STYLSHEET "Standard - Half Size.stb")
      (/= STYLSHEET "Color.stb")
      (/= STYLSHEET "No Weight.stb")
    ) ;_ End or
    (progn 
      (vla-refreshplotdeviceinfo (vla-get-activelayout ACADDOC))
      (vla-put-stylesheet 
        (vla-get-activelayout ACADDOC)
        "Standard - Full Size.stb"
      ) ;_ End vla-put-stylesheet
    ) ;_ End progn
  ) ;_ End if
  (setq ENT1 (entnext))
  (while ENT1 
    (setq VL-ENT1 (vlax-ename->vla-object ENT1)
          COL     (vla-get-color VL-ENT1)
    ) ;_ End setq
    (if (and (/= COL 256) (/= COL 0)) 
      (ENT-PSTYLE VL-ENT1)
    ) ;_ End if
    (if (= (vla-get-objectname VL-ENT1) "AcDbBlockReference") 
      (if 
        (= 
          (vla-get-isxref (vla-item VL-BLOCKS-REF (vla-get-name VL-ENT1)))
          :vlax-true
        )
        (setq XREFFOUND "True"
              ISXREF    "True"
        ) ;_ End setq
        (setq ISXREF "False")
      ) ;_ End if
    ) ;_ End if
    (if 
      (and 
        (= (vla-get-objectname VL-ENT1) "AcDbBlockReference")
        (= ISXREF "False")
      ) ;_ End and
      (progn 
        (INSERT-PSTYLE 
          (cdr 
            (assoc 
              -2
              (tblsearch 
                "BLOCK"
                (vla-get-name VL-ENT1)
                ;(cdr (assoc 2 (entget ENT1)))
              ) ;_ End tblsearch
            ) ;_ End assoc
          ) ;_ End cdr
        ) ;_ End INSERT-PSTYLE
        (while BLIST 
          (INSERT-PSTYLE 
            (cdr 
              (assoc 
                -2
                (tblsearch 
                  "BLOCK"
                  (nth 0 BLIST)
                ) ;_ End tblsearch
              ) ;_ End assoc
            ) ;_ End cdr
          ) ;_ End INSERT-PSTYLE
          (setq BLIST (vl-remove (nth 0 BLIST) BLIST))
        ) ;_ End while
      ) ;_ End progn
    ) ;_ End if
    (setq ENT1 (entnext ENT1))
  ) ;_ End while

  ;;; Fix layers
  (LAYER-PSTYLES)

  (if (= XREFFOUND "True") 
    (alert "X-references found.\nPlease update them also.")
  ) ;_ End if
  (setvar "cmdecho" CMD)
  (vla-endundomark ACADDOC)
  (princ)
) ;_ End defun

(defun ENT-PSTYLE (VL-ENT1 / LAY-STAT PLOTSTYLE) 
  (setq PLOTSTYLE (vla-get-plotstylename VL-ENT1))
  (if 
    (and 
      (/= PLOTSTYLE "Text")
      (/= PLOTSTYLE "New Work")
      (/= PLOTSTYLE "Exist to Remain")
      (/= PLOTSTYLE "NW Bold")
      (/= PLOTSTYLE "Text")
      (/= PLOTSTYLE "Furniture")
      (/= PLOTSTYLE "Shade 60%")
      (/= PLOTSTYLE "Shade 20%")
      (/= PLOTSTYLE "Background")
    ) ;_ End and
    (progn 
      (setq LAY-STAT (SET-FRZN-LOCK-LAY VL-ENT1))
      (vla-put-plotstylename VL-ENT1 (COLOR->PSTYLE COL))
      (RESET-FRZN-LOCK-LAY VL-ENT1 LAY-STAT)
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun

(defun INSERT-PSTYLE (ENT1 / BLCK-REC-ENT-NM-1 BLCK-REC-ENT-NM-2 COL ENT1DEF ENT1TYPE 
                      VL-ENT1
                     ) 
  (setq VL-ENT1           (vlax-ename->vla-object ENT1)
        ENT1DEF           (entget ENT1)
        ENT1TYPE          (cdr (assoc 0 ENT1DEF))
        BLCK-REC-ENT-NM-1 (cdr 
                            (assoc 
                              5
                              (entget (cdr (assoc 330 ENT1DEF)))
                            ) ;_ End assoc
                          ) ;_ End cdr
        BLCK-REC-ENT-NM-2 BLCK-REC-ENT-NM-1
        COL               (vla-get-color VL-ENT1)
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
    (if (and (/= COL 256) (/= COL 0)) 
      (ENT-PSTYLE VL-ENT1)
    ) ;_ End if
    (setq ENT1DEF (entnext (cdr (assoc -1 ENT1DEF))))
    (if ENT1DEF 
      (setq ENT1DEF           (entget ENT1DEF)
            ENT1TYPE          (cdr (assoc 0 ENT1DEF))
            VL-ENT1           (vlax-ename->vla-object 
                                (cdr (assoc -1 ENT1DEF))
                              ) ;_ End vlax-ename->vla-object
            COL               (vla-get-color VL-ENT1)
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

(defun COLOR->PSTYLE (COL) 
  (cond 
    ((or (= COL 1) (= COL 10))
     "Text"
    )
    ((or 
       (= COL 2)
       (= COL 3)
       (= COL 4)
       (= COL 5)
       (= COL 6)
       (= COL 30)
       (= COL 54)
       (= COL 100)
     ) ;_ End or
     "New Work"
    )
    ((or (= COL 7) (= COL 11) (= COL 12) (= COL 32))
     "Exist to Remain"
    )
    ((= COL 101)
     "NW Bold"
    )
    ((= COL 200)
     "Text"
    )
    ((= COL 247)
     "Furniture"
    )
    ((= COL 249)
     "Shade 60%"
    )
    ((or 
       (= COL 250)
       (= COL 251)
       (= COL 252)
       (= COL 253)
       (= COL 254)
       (= COL 255)
     ) ;_ End or
     "Shade 20%"
    )
    (COL
     "Background"
    )
  ) ;_ End cond
) ;_ End defun

(defun LAYER-PSTYLES (/ ADOC ITEM LAYERS PLOTSTYLE) 
  (setq ADOC   (vla-get-activedocument (vlax-get-acad-object))
        LAYERS (vla-get-layers ADOC) ;_ End vla-get-layers
  ) ;_ End setq
  (vlax-for ITEM LAYERS 
    (setq PLOTSTYLE (vla-get-plotstylename ITEM))
    (if 
      (and 
        (/= PLOTSTYLE "Text")
        (/= PLOTSTYLE "New Work")
        (/= PLOTSTYLE "Exist to Remain")
        (/= PLOTSTYLE "NW Bold")
        (/= PLOTSTYLE "Text")
        (/= PLOTSTYLE "Furniture")
        (/= PLOTSTYLE "Shade 60%")
        (/= PLOTSTYLE "Shade 20%")
        (/= PLOTSTYLE "Background")
      ) ;_ End and
      (command 
        "_.-layer"
        "ps"
        (COLOR->PSTYLE (vla-get-color ITEM))
        (vla-get-name ITEM)
        ""
      )

      ;;; Need to be able to load plot styles that are not loaded.  It gives an
      ;;; error on plot styles that are not loaded "key not found".
      ;;;	    (vla-put-plotstylename ITEM (COLOR->PSTYLE (vla-get-color ITEM)))
    ) ;_ End if
  ) ;_ End vlax-for
) ;_ End defun

(defun *ERROR* (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (vla-endundomark 
    (vla-get-activedocument (vlax-get-acad-object))
  ) ;_ End vla-endundomark
  (princ)
) ;_ End defun

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
