;;; Copyright Lance A. Endres

(defun C:DLAYER (/ CMD LAYERNAME LYTEST SS PT1 PT2 ENT) 
  (setq OLDERR  *ERROR*
        *ERROR* DLAYERERR
        CMD     (getvar "cmdecho")
  ) ;_ End setq
  (setvar "cmdecho" 0)
  (while (= ENT NIL) 
    (initget "Enter")
    (setq ENT (entsel 
                "\nEnter name/<Pick an object on the layer to be deleted>: "
              ) ;_ End entsel
    ) ;_ End setq
  ) ;_ End while - ensures selected object or keyword entered
  (if (= ENT "Enter") 
    (progn 
      (while (= LYTEST NIL)  ;_ Ensure layer selected is a valid layer
        (setq LAYERNAME (strcase (getstring "\nLayer name: ")))
        (setq LYTEST (tblsearch "layer" LAYERNAME))
        (if (= LYTEST NIL) 
          (princ "\nNot a valid layer name:")
        ) ;_ End if
      ) ;_ End while
    ) ;_ End progn
    (setq LAYERNAME (assoc 8 (entget (car ENT))))
  ) ;_ End if
  (initget 1)
  (setq PT1 (getpoint "\nFirst corner of window: "))
  (initget 1)
  (setq PT2 (getcorner PT1 "\nSecond corner: ")
        SS  (ssget "w" PT1 PT2 (list LAYERNAME)) ;_ Create selection set
  ) ;_ End setq
  
  ;_ Test for nil set, meaning nothing was selected.
  (if (/= SS NIL)
    (progn 
      (princ 
        (strcat 
          "\n "
          (itoa (sslength SS))
          "entities on layer \""
          (cdr LAYERNAME)
          "\" found and deleted."
        ) ;_ End strcat
      ) ;_ End princ
      (command "erase" SS "") ;_ Erase matching entities
    ) ;_ End progn
    (princ 
      (strcat "\nNo entities on layer \"" (cdr LAYERNAME) "\" found.")
    ) ;_ End princ
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun

(defun DLAYERERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun