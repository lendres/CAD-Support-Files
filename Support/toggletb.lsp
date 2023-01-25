;;; Copyright Lance A. Endres

(defun C:ALL-TB-TOGGLE (/ CMD TBLIST TOOLBAR TRADE WHICH) 
  (setq CMD (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (initget 1 "ELEC MECH")
  (setq TRADE (getkword "\nToggle which trade? [ELEC/MECH]: "))
  (initget 1 "ON OFF")
  (setq WHICH (getkword "\nToggle toolbars [ON/OFF]: "))
  (cond 
    ((= TRADE "ELEC")
     (setq TBLIST (list "Electrical Auxiliary" "Electrical Fire Protection" 
                        "Electrical Health Care" "Electrical Lighting" "Electrical Power" 
                        "Electrical Receptacles"
                  ) ;_ End list
     ) ;_ End setq
    )
    ((= TRADE "MECH")
     (setq TBLIST (list 
                    "Mechanical Plumbing"
                    "Mechanical HVAC"
                  ) ;_ End list
     ) ;_ End setq
    )
  ) ;_ End cond
  (cond 
    ((= WHICH "ON")
     (foreach TOOLBAR TBLIST 
       (command "_.-toolbar" TOOLBAR "show")
     ) ;_ End foreach
    )
    ((= WHICH "OFF")
     (foreach TOOLBAR TBLIST 
       (command "_.-toolbar" TOOLBAR "hide")
     ) ;_ End foreach
    )
  ) ;_ End cond
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun