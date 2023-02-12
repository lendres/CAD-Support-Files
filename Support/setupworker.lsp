;;; Copyright Lance A. Endres

;;; The main work of the setup routine.  See "setup.lsp" for the user interface.

;;; Now it's time to get some actual work done.  Above this point are the subroutines
;;; to run the dialog box.  Below are the routines to modify the drawing.
(defun SETUP (/ ADJ_FOR_PAPERS CNT1 DWG_SCALE FILE1 IMP_LYTS LMBD1 L_UNITS L_UPREC 
              LMBD2 PLW PT1 PT2 SET_LAYS SET_LMTS VL-ADOC VL-LAY-DISP-MARGINS 
              VL-LAY-DISP-PAPER VL-PREF-DISPLAY
             ) 
  (setq ADJ_FOR_PAPERS  (atoi (getcfg (strcat DATAPREFIX "Adj_For_Papers")))
        DWG_SCALE       (getcfg (strcat DATAPREFIX "Dwg_Scale"))
        L_UNITS         (atoi (getcfg (strcat DATAPREFIX "L_Units")))
        L_UPREC         (atoi (getcfg (strcat DATAPREFIX "Dim_Prec")))
        DWG_SCALE       (distof DWG_SCALE L_UNITS)
        SET_LMTS        (getcfg (strcat DATAPREFIX "Set_Lmts"))
        SET_LAYS        (getcfg (strcat DATAPREFIX "Set_Lays"))
        IMP_LYTS        (getcfg (strcat DATAPREFIX "Imp_Lyts"))
        PLW             (/ DWG_SCALE 32)
        VL-ADOC         (vla-get-activedocument (vlax-get-acad-object))
        PAPER_ADJ_SCALE 1.25
  ) ;_ End setq

  ;;; Set limits.
  (if (= SET_LMTS "1") 
    (progn 
      (setq PT1 (getpoint "\nLower left corner of Limits: ")
            PT2 (getpoint "\nUpper right corner of Limits: ")
      ) ;_ End setq

      ;;; Test to make sure that the second point is above and to the right
      ;;; of the first point.
      (while (or (<= (car PT2) (car PT1)) (<= (cadr PT2) (cadr PT1))) 
        (princ 
          "\nSecond point must be above and to the right of first point."
        ) ;_ End princ
        (setq PT1 (getpoint "\nLower left corner of Limits: ")
              PT2 (getpoint "\nUpper right corner of Limits: ")
        ) ;_ End setq
      ) ;_ End while
      (setq PT1 (reverse (cdr (reverse PT1)))
            PT2 (reverse (cdr (reverse PT2)))
            PT1 (mapcar 
                  '(lambda (LMBD1) 
                     (setq LMBD1 (vlax-make-variant 
                                   (vlax-safearray-fill 
                                     (vlax-make-safearray 
                                       vlax-vbdouble
                                       '(0 . 1)
                                     ) ;_ End vlax-make-safearray
                                     LMBD1
                                   ) ;_ End vlax-safearray-fill
                                 ) ;_ End vlax-make-variant
                     ) ;_ End setq
                   ) ;_ End lambda
                  (list PT1 PT2)
                ) ;_ End mapcar
      ) ;_ End setq

      ;;; Limits cannot be set in a layout if the margins or the paper are showing.  If a layout besides model
      ;;; is active, turn off the margins and paper.
      (if (/= (vla-get-taborder (GET-CURRENT-LAYOUT)) 0) 
        (progn 
          (setq VL-PREF-DISPLAY     (vla-get-display 
                                      (vla-get-preferences 
                                        (vlax-get-acad-object)
                                      ) ;_ End vla-get-preferences
                                    ) ;_ End vla-get-display
                VL-LAY-DISP-MARGINS (vla-get-layoutdisplaymargins 
                                      VL-PREF-DISPLAY
                                    ) ;_ End vla-get-layoutdisplaymargins
                VL-LAY-DISP-PAPER   (vla-get-layoutdisplaypaper 
                                      VL-PREF-DISPLAY
                                    ) ;_ End vla-get-layoutdisplaypaper
          ) ;_ End setq
          (if (= VL-LAY-DISP-MARGINS :vlax-true) 
            (vla-put-layoutdisplaymargins 
              VL-PREF-DISPLAY
              :vlax-false
            ) ;_ End vla-put-layoutdisplaymargins
          ) ;_ End if
          (if (= VL-LAY-DISP-PAPER :vlax-true) 
            (vla-put-layoutdisplaypaper 
              VL-PREF-DISPLAY
              :vlax-false
            ) ;_ End vla-put-layoutdisplaypaper
          ) ;_ End if
        ) ;_ End progn
      ) ;_ End if

      ;;; Set the limits.
      (mapcar 
        '(lambda (LMBD1 LMBD2) 
           (vla-setvariable VL-ADOC LMBD1 LMBD2)
         ) ;_ End lambda
        (list "limmin" "limmax")
        PT1
      ) ;_ End mapcar

      ;;; Restore the margins or the paper if the layout is not model.
      (if (/= (vla-get-taborder (GET-CURRENT-LAYOUT)) 0) 
        (progn 
          (vla-put-layoutdisplaymargins 
            VL-PREF-DISPLAY
            VL-LAY-DISP-MARGINS
          ) ;_ End vla-put-layoutdisplaymargins
          (vla-put-layoutdisplaypaper 
            VL-PREF-DISPLAY
            VL-LAY-DISP-PAPER
          ) ;_ End vla-put-layoutdisplaypaper
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End progn
  ) ;_ End if test for limits

  ;;; Set system variables.
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "attdia" "blipmode" "coords" "dimscale" "lunits" "luprec" "angbase" 
          "angdir" "aunits" "auprec" "mirrtext" "plinewid" "visretain" "ucsicon" "ltscale" 
          "limcheck" "insunits"
    ) ;_ End list
    (list 
      0
      0
      2 ; Coords.
      DWG_SCALE ; Dim scale.
      L_UNITS ; Units.
      L_UPREC ; Precision.
      0
      0
      0
      4
      0
      PLW
      1
      1
      DWG_SCALE
      0
      (if (= (getcfg "AppData/Setup_Data/Plot_Unts") "si") 
        4
        1
      ) ;_ End if
    ) ;_ End list
  ) ;_ End mapcar

  ;;; Import layouts.
  (if (= IMP_LYTS "1") 
    (progn 
      (IMPORT_LAYOUTS)
    ) ;_ End progn
  ) ;_ End if

  ;;; View resolution.
  (command "viewres" "Y" "1000")

  ;;; Text style settings.
  (C:CREATETEXTSTYLES)

  ;;; Load line types.
  (C:LOADLINETYPES)

  ;;; Layers.
  ;;; NOTE:  Need to varify that the heading exists within the layer standard file.
  (if (= SET_LAYS "1") 
    (progn
      (setq CNT1            1
            FILEPATH        (getcfg (strcat DATAPREFIX "Lay_Data"))
      ) ;_ End setq

			; Default layers file if one was not found in the settings.
      (if (= FILEPATH NIL)
        (setq FILEPATH "Standard Layers.lst")
			) ;_ End setq
      (setq FILE1 (findfile FILEPATH))

      (while (< CNT1 7)
        (if
          (=
            (getcfg (strcat "AppData/Setup_Data/Genre0" (itoa CNT1)))
            "1"
          ) ;_ End =
          (MAKELAYERS (strcat "**GENRE0" (itoa CNT1)) FILE1)
        ) ;_ End if
        (setq CNT1 (1+ CNT1))
      ) ;_ End while.
    ) ;_ End progn.
  )

  (if (= ADJ_FOR_PAPERS 1) 
    (command "_.-layer" "m" "Text" "c" "white" "Text" "pstyle" "Text Bold" "Text" "")
    (command "_.-layer" "m" "Text" "c" "red" "Text" "pstyle" "Text" "Text" "")
  ) ;_ End if.

  ;;; Dimension settings.
  (if (= ADJ_FOR_PAPERS 1) 
    (CREATEDIMSTYLES PAPER_ADJ_SCALE)
    (CREATEDIMSTYLES 1.0)
  ) ;_ End if.
) ;_ End defun.

(defun MAKELAYERS (HEADING FILE1 ADJ_FOR_PAPERS / ACTLAYOUT FILE_HAND FRZTEST 
                   LAYERNAME LAYERCOLOR LAYERLINETYPE LAYERPLOTORNOT LAYERPLOTSTYLE
                  ) 
  (if (= (getvar "pstylemode") 0) 
    (progn 
      (setq ACTLAYOUT (GET-CURRENT-LAYOUT))

      ;;; Refresh the plot information to make sure that the
      ;;; correct information is being used.
      (vla-refreshplotdeviceinfo ACTLAYOUT)

      (if 
        (not 
          (member 
            (vla-get-stylesheet ACTLAYOUT)
            '("Color.stb" "No Weight.stb" "Standard - Full Size.stb" 
              "Standard - Half Size.stb"
             )
          ) ;_ End member.
        ) ;_ End not.
        (if (= ADJ_FOR_PAPERS 1) 
          (vla-put-stylesheet 
            ACTLAYOUT
            "Standard - Full Size - Color.stb"
          ) ;_ End vla-put-stylesheet.
          (vla-put-stylesheet 
            ACTLAYOUT
            "Standard - Full Size.stb"
          ) ;_ End vla-put-stylesheet.
        ) ;_ End if.
      ) ;_ End if.
    ) ;_ End progn.
  ) ;_ End if.
  (setq FILE_HAND (open FILE1 "r")
        LAYERNAME (GETNEWLINE FILE_HAND)
  ) ;_ End setq.

  ;;; Read until we find the heading.
  (while (/= (substr LAYERNAME 1 (strlen HEADING)) HEADING) 
    (setq LAYERNAME (GETNEWLINE FILE_HAND))
  ) ;_ End while.

  ;;; Read the lines that make up a layer data set.
  (setq LAYERNAME      (GETNEWLINE FILE_HAND)
        LAYERCOLOR     (GETNEWLINE FILE_HAND)
        LAYERLINETYPE  (GETNEWLINE FILE_HAND)
        LAYERPLOTORNOT (GETNEWLINE FILE_HAND)
        LAYERPLOTSTYLE (GETNEWLINE FILE_HAND)
  ) ;_ End setq.
  (if (and LAYERNAME LAYERCOLOR LAYERLINETYPE LAYERPLOTORNOT LAYERPLOTSTYLE) 
    (while 
      (and 
        (/= (substr LAYERNAME 1 2) "**")
        (/= (substr LAYERCOLOR 1 2) "**")
        (/= (substr LAYERLINETYPE 1 2) "**")
        (/= (substr LAYERPLOTORNOT 1 2) "**")
        (/= (substr LAYERPLOTSTYLE 1 2) "**")
      ) ;_ End and.
      (LAYER_DATA_CHECK LAYERNAME LAYERCOLOR LAYERLINETYPE LAYERPLOTORNOT 
                        LAYERPLOTSTYLE
      )
      (setq LAYERNAME      (GETNEWLINE FILE_HAND)
            LAYERCOLOR     (GETNEWLINE FILE_HAND)
            LAYERLINETYPE  (GETNEWLINE FILE_HAND)
            LAYERPLOTORNOT (GETNEWLINE FILE_HAND)
            LAYERPLOTSTYLE (GETNEWLINE FILE_HAND)
      ) ;_ End setq.
      (if 
        (or 
          (= LAYERNAME NIL)
          (= LAYERCOLOR NIL)
          (= LAYERLINETYPE NIL)
          (= LAYERPLOTORNOT NIL)
          (= LAYERPLOTSTYLE NIL)
        ) ;_ End or.
        (setq LAYERNAME "**")
      ) ;_ End if.
    ) ;_ End while.
  ) ;_ End if.
  (close FILE_HAND)
) ;_ End defun.

(defun LAYER_DATA_CHECK (LAYERNAME LAYERCOLOR LAYERLINETYPE LAYERPLOTORNOT 
                         LAYERPLOTSTYLE / PSMODE TEST TEST2
                        ) 
  (setq PSMODE (getvar "pstylemode"))
  (if LAYERCOLOR 
    (if (numberp (read LAYERCOLOR)) 
      (setq LAYERCOLOR (read LAYERCOLOR)
            TEST       1
      ) ;_ End setq.
      (setq TEST 2)
    ) ;_ End if.
  ) ;_ End if.
  (if (and (= TEST 1) (and (> LAYERCOLOR 0) (< LAYERCOLOR 250))) 
    (setq TEST2 1)
  ) ;_ End if.
  (if 
    (and 
      (= TEST 2)
      (or 
        (= (strcase LAYERCOLOR) "RED")
        (= (strcase LAYERCOLOR) "YELLOW")
        (= (strcase LAYERCOLOR) "GREEN")
        (= (strcase LAYERCOLOR) "CYAN")
        (= (strcase LAYERCOLOR) "BLUE")
        (= (strcase LAYERCOLOR) "CYAN")
        (= (strcase LAYERCOLOR) "MAGENTA")
        (= (strcase LAYERCOLOR) "WHITE")
      ) ;_ End or.
    ) ;_ End and.
    (setq TEST2 1)
  ) ;_ End if.
  (cond 
    ((/= TEST2 1)
     (princ "\nInvalid layer color found in layer setup file.")
     (princ "\nCheck file and re-run setup.")
    ) ;_ End cond for color check.
    ((if LAYERLINETYPE 
       (not (tblsearch "ltype" LAYERLINETYPE))
     ) ;_ End if.
     (princ "\nInvalid linetype or a linetype not loaded found in layer setup file.")
     (princ "\nCheck file and re-run setup.")
    ) ;_ End cond for linetype check.
    ((if LAYERNAME 
       (not (snvalid LAYERNAME))
     ) ;_ End if.
     (princ "\nInvalid layer name found in layer setup file.")
     (princ "\nCheck file and re-run setup.")
    ) ;_ End cond for layer check.
    ((and (/= (strcase LAYERPLOTORNOT) "PLOT") 
          (/= (strcase LAYERPLOTORNOT) "NO")
     )
     (princ 
       "\Invalid plot/no plot referance found in layer setup file."
     ) ;_ End princ.
     (princ "\nCheck file and re-run setup.")
    ) ;_ End princ.

    ;;; Need to add a check for valid plot styles.

    ((if LAYERNAME 
       (snvalid LAYERNAME)
     ) ;_ End if.
     (setq FRZTEST (tblsearch "layer" LAYERNAME)
           FRZTEST (cdr (assoc 70 FRZTEST))
     ) ;_ End setq.
     (if 
       (or 
         (= FRZTEST 1)
         (= FRZTEST 3)
         (= FRZTEST 5)
         (= FRZTEST 7)
       ) ;_ End or.
       (command "_.layer" "thaw" LAYERNAME "")
     ) ;_ End if.
     (command "_.-layer" "m" LAYERNAME "c" LAYERCOLOR LAYERNAME "l" LAYERLINETYPE 
              LAYERNAME "p" LAYERPLOTORNOT LAYERNAME
     )
     (if (= PSMODE 0) 
       (command "ps" LAYERPLOTSTYLE LAYERNAME "")
       (command "")
     ) ;_ End if.
     (if 
       (or 
         (= FRZTEST 1)
         (= FRZTEST 3)
         (= FRZTEST 5)
         (= FRZTEST 7)
       ) ;_ End or.
       (command "_.layer" "set" "0" "freeze" LAYERNAME "")
     ) ;_ End if.
    ) ;_ End cond making layer.
  ) ;_ End cond check for valid data in layer setup file.
) ;_ End defun.

(defun IMPORT_LAYOUTS (/ CLAYOUT FILE FILE_HAND LAYDISPST TEMP) 
  (setq LAYDISPST (GET-LAYOUTS-DISP-STATE)
        CLAYOUT   (GET-CURRENT-LAYOUT)
  ) ;_ End setq.

  ;;; Check the state of the layout tabs.
  ;;; If they are off - turn them on so program can function.

  (if (= LAYDISPST :vlax-false) 
    (PUT-LAYOUTS-DISP-STATE :vlax-true)
  ) ;_ End if.
  (foreach FILE 
    (list 
      "Plotter Layouts.txt"
      "Scaled Layouts.txt"
      "Non-Scaled Layouts.txt"
    ) ;_ End list.
    (setq FILE_HAND (open (findfile FILE) "r"))
    (while (setq TEMP (GETNEWLINE FILE_HAND)) 
      (if (= FILE "Plotter Layouts.txt") 
        (progn 
          (MAKE_LAYOUT 
            (strcat 
              TEMP
              " "
              (substr 
                (getcfg "AppData/Setup_Data/P_Size")
                3
              ) ;_ End substr.
              " Draft"
            ) ;_ End strcat.
            FILE
          ) ;_ End MAKE_LAYOUT.
          (MAKE_LAYOUT 
            (strcat 
              TEMP
              " "
              (substr 
                (getcfg "AppData/Setup_Data/P_Size")
                3
              ) ;_ End substr.
              " Final"
            ) ;_ End strcat.
            FILE
          ) ;_ End MAKE_LAYOUT.
        ) ;_ End progn.
        (MAKE_LAYOUT TEMP FILE)
      ) ;_ End if.
    ) ;_ End while.
  ) ;_ End foreach.
  (PUT-CURRENT-LAYOUT CLAYOUT)
  (if (= LAYDISPST :vlax-false) 
    (PUT-LAYOUTS-DISP-STATE LAYDISPST)
  ) ;_ End if.
) ;_ End defun.

(defun MAKE_LAYOUT (LYOUT FILE / TEMPLATEFILE) 
  (if (= (getvar "pstylemode") 0) 
    (setq TEMPLATEFILE (findfile "Named PS Plotting Templates.dwt"))
    (setq TEMPLATEFILE (findfile "Color PS Plotting Templates.dwt"))
  ) ;_ End if.

  ;;; Import System International units if they were specified.
  (if (= (getcfg "AppData/Setup_Data/Plot_Unts") "si") 
    (setq LYOUT (strcat LYOUT " - SI"))
  ) ;_ End if.

  (command 
    "_.layout"
    "template"
    TEMPLATEFILE
    LYOUT
  ) ;_ End command.

  ;;; Set last layout active.

  (AX:ACTIVATELASTLAYOUT)

  (command "_.zoom" (getvar "limmin") (getvar "limmax"))
  (if 
    (or 
      (= FILE "Scaled Layouts.txt")
      (= FILE "Plotter Layouts.txt")
    ) ;_ End or.
    (command 
      "_.zoom"
      (strcat (rtos (/ 1 (getvar "dimscale")) 2 8) "xp")
    ) ;_ End command.
  ) ;_ End if.
  (command "_.pspace")
) ;_ End defun.

(defun ARCH_ACTIVATED (/ INDEX DWG_SCL) 

  ;;; If S.I. units are active, change the units to U.S. Customary.  Arch units
  ;;; cannot be used with S.I. units.
  (if (= (get_tile "si") "1") 
    (set_tile "uscust" "1")
  ) ;_ End if.

  ;;; Convert the units of the drawing scale box.
  (set_tile 
    "dwg_scale"
    (LE_RTOS (distof (get_tile "dwg_scale") 2) 4)
  ) ;_ End set_tile.

  ;;; Store the value of the dimprec drop down box.
  (setq INDEX (get_tile "dimprec"))

  ;;; Remap the values of the drop down box to match the values currently set.
  (start_list "dimprec")
  (mapcar 'add_list PREC_LIST_ARCH)
  (end_list)

  ;;; Restore the value of the drop down box.
  (set_tile "dimprec" INDEX)
) ;_ End defun.

(defun DECIM_ACTIVATED (/ INDEX) 

  ;;; Convert the units of the drawing scale box.

  (set_tile 
    "dwg_scale"
    (LE_RTOS (distof (get_tile "dwg_scale") 4) 2)
  ) ;_ End set_tile.

  ;;; Store the value of the dimprec drop down box.

  (setq INDEX (get_tile "dimprec"))

  ;;; Remap the values of the drop down box to match the values currently set.

  (start_list "dimprec")
  (mapcar 'add_list PREC_LIST_DIM)
  (end_list)

  ;;; Restore the value of the drop down box.

  (set_tile "dimprec" INDEX)
) ;_ End defun.

(defun SI_ACTIVATED (/ INDEX) 
  (if (= (get_tile "arch") "1") 
    (progn 
      (alert 
        "S.I. units cannot be set while units are architectural"
      ) ;_ End alert.
      (set_tile "uscust" "1")
    ) ;_ End progn.
  ) ;_ End if.
) ;_ End defun.