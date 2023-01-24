;;; Copyright Lance A. Endres

(defun LEADER_BASE (LEAD_TYPE PT1 PT2 / ANG_MODE AUTO_PS AX1 AX2 DIMSC DSTYL LMCH MAJ 
                    MNR OSM PT3 ROT ROT_ANG UORG UXDIR UYDIR XD XPT YD
                   ) 
  (setq DSTYL (getvar "dimstyle") ; LMCH	(getvar "limcheck")
        OSM   (getvar "osmode")
        PT3   (list (car PT1) (cadr PT2))
  ) ;_ End setq
  (setvar "osmode" 0)

  ;;; Gather data from configuration file.
  (setq AUTO_PS  (getcfg "AppData/Leader_Utils/Auto_PS")
        ANG_MODE (getcfg "AppData/Leader_Utils/Ang_Mode")
        ROT_ANG  (getcfg "AppData/Leader_Utils/Rot_Ang")
  ) ;_ End setq

  ;;; If data does not exist set defaults.
  (if (or (null ANG_MODE) (= ANG_MODE "")) 
    (setq ANG_MODE (getvar "aunits"))
    (setq ANG_MODE (atoi ANG_MODE))
  ) ;_ End if
  (if (or (null AUTO_PS) (= AUTO_PS "")) 
    (setq AUTO_PS "0")
  ) ;_ End if
  (if (null ROT_ANG) 
    (setq ROT_ANG 0)
    (setq ROT_ANG (angtof ROT_ANG ANG_MODE))
  ) ;_ End if

  ;;; Test for rotation angle.
  (if (and (/= ROT_ANG 0) (/= ROT_ANG NIL)) 
    (progn 
      (setq XD    (cos ROT_ANG)
            YD    (sin ROT_ANG)
            XPT   (list XD YD 0.0)
            UORG  (getvar "ucsorg")
            UXDIR (getvar "ucsxdir")
            UYDIR (getvar "ucsydir")
            PT1   (trans PT1 1 0 0)
            PT2   (trans PT2 1 0 0)
      ) ;_ End setq
      (command "_.ucs" "world" "_.ucs" "3point" '(0 0 0) XPT "")
      (setq PT1 (trans PT1 0 1 0)
            PT2 (trans PT2 0 1 0)
            PT3 (list (car PT1) (cadr PT2))
      )
    ) ;_ End progn
  ) ;_ End if

  (cond 
    ((and 
       (= LEAD_TYPE "ELLIP1_LEAD")
       (>= (substr (getvar "acadver") 1 2) "15")
     ) ;_ End and

     ;;; If the dimstyle for elliptical leaders does not exist create it.
     (if (not (tblsearch "dimstyle" "DPM - ELL")) 
       (CREATEDIMSTYLES 
         (vla-get-activedocument (vlax-get-acad-object))
         1.0
       ) ;_ End CREATEDIMSTYLES
     ) ;_ End if

     (command "_.-dimstyle" "restore" "DPM - ELL")
    ) ;_ End cond for elliptical leader R15
    ((and 
       (= LEAD_TYPE "ARROW1_LEAD")
       (>= (substr (getvar "acadver") 1 2) "15")
     ) ;_ End and

     ;;; If the dimstyle for leaders does not exist create it.
     (if (not (tblsearch "dimstyle" "DPM - LL")) 
       (CREATEDIMSTYLES 
         (vla-get-activedocument (vlax-get-acad-object))
       ) ;_ End CREATEDIMSTYLES
     ) ;_ End if
     (command "_.-dimstyle" "restore" "DPM - LL")
    )
  ) ;_ End cond

  ;;; The following lines are for paperspace scaling.  This section follows the condition
  ;;; statement for the type of leader so that a change in dimstyle will not reset the dimscale.
  (if 
    (and 
      (= AUTO_PS "1")
      (= (getvar "tilemode") 0)
      (= (getvar "cvport") 1)
    ) ;_ End and
    (progn 
      (setq DIMSC (getvar "dimscale"))
      (setvar "dimscale" 1)
    ) ;_ End then progn
    (progn 
      (setq DIMSC NIL)
    ) ;_ End else progn
  ) ;_ End if

  ;;; Create the leader.
  (command 
    "leader"
    PT1
    (polar 
      PT2
      (angle PT2 PT3)
      (* 
        (getvar "dimscale")
        0.09
        (if (= (getvar "insunits") 4) 
          25.4
          1
        ) ;_ End if - If inits are metric, convert.
      ) ;_ End *
    ) ;_ End polar
    PT2
    ""
    ""
    "n"
  ) ;_ End command

  ;;; Reset the dimstyle.
  (command "_.-dimstyle" "restore" DSTYL)

  ;;; Reset UCS if necessary.
  (if (and (/= ROT_ANG 0) (/= ROT_ANG NIL)) 
    (command "_.ucs" "world" "_.ucs" "3point" UORG UXDIR UYDIR) ;_ End command
  ) ;_ End if

  ;;; Reset dimscale if necessary.
  (if (not (= DIMSC NIL)) 
    (setvar "dimscale" DIMSC)
  ) ;_ End if

  ;;; Exit program.
  (setvar "osmode" OSM)
) ;_ End defun