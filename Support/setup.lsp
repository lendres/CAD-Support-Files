;;; Copyright Lance A. Endres

;;; The user interface portion of the setup routine.  See "setupworker.lsp" the work portion.

;;; Revision History
;;; July 4, 2001
;;; Plotting in SI added.
;;; Selection of drawing units added.
;;; Selection of dimension precision added.
;;;
;;; March 8, 2004
;;; Updates for use with AutoCAD 2004
;;;
;;; Fixed a bug in the used of EXPERT and eliminated having to decide how to call
;;; the adding of dimstyles and linetypes depending on if they were already loaded.
;;;
;;; February 2, 2005
;;; Fixed a bug that allowed layers to be imported regardless of check box on dialog box.
;;;
;;; Added support for 12pt fonts.
;;;
;;; December 1, 2005
;;; The luprec (displayed precision) is now controled by the setting of the dimension precision.
;;; Added the variable DATAPREFIX which stores the configuration data prefix string.
;;;
;;; October 09, 2006
;;; Added an option to allow adjustments for making figures for papers.  Especially for when using
;;; LaTeX.  Adjustments made:
;;;     1) Added an option to the dialog box.
;;;
;;; January 23, 2023
;;; Updated to work with GStarCAD.  No longer compiled.  Loads all required files in the userdoc.lsp.
;;;
;;; NOTE: When debugging this file, be sure to include a reference to the path that the source
;;;       dcl file is located.  I.e. c:\storage\programming\lisp source files\

(defun C:SETUP (/ ADJ_FOR_PAPERS CMD CLAY DCL_ID DWG_SCALE EXPRT GENRE01 GENRE02 
                GENRE03 GENRE04 GENRE05 GENRE06 IMP_LYTS L_UNITS L_UNITS_HOLDER LMBD1 
                LMBD2 OSM P_SIZE RGMOD SET_LAYS SET_LMTS TEMP VL-ADOC
               ) 
  (setq DCL_ID     (load_dialog "SETUP.DCL")
        RUN_SETUP  "No"
        VL-ADOC    (vla-get-activedocument (vlax-get-acad-object))
        CLAY       (vla-getvariable VL-ADOC "clayer")
        CMD        (vla-getvariable VL-ADOC "cmdecho")
        RGMOD      (vla-getvariable VL-ADOC "regenmode")
        EXPRT      (vla-getvariable VL-ADOC "expert")
        OSM        (vla-getvariable VL-ADOC "osmode")
        DATAPREFIX "AppData/Setup_Data/"
  ) ;_ End setq
  (vl-load-com)
  (vla-startundomark VL-ADOC)
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "cmdecho" "regenmode" "expert")
    (list 1 0 5)
  ) ;_ End mapcar

  ;;; Load dialog file and verify loading.
  (if (not (new_dialog "SETUP" DCL_ID)) 
    (exit)
  ) ;_ End if test to insure dialog box loaded

  ;;; Gather data from configuration file.

  ;;; DWG_SCALE		Controls system variable "dimscale".
  ;;; L_UNITS		Controls system variable "lunits" (arch / decimal / etc.).
  ;;;    			Used to set the drawing units and to convert units for controlling
  ;;;    			the dialog box.
  ;;; SET_LMTS 		Determines if the user wants to set limits during the setup process.
  ;;; SET_LAYS 		Determines if the user wants to import layers during the setup process.
  ;;; IMP_LYTS 		Determines if the user wants to import layouts during the setup process.
  ;;; P_SIZE   		Setting for the paper sized used (i.e. D or E size).
  ;;; GENRE01		Import lighting layers.
  ;;; GENRE02 		Import power layers.
  ;;; GENRE03		Import HVAC layers.
  ;;; GENRE04		Import pluming layers.
  ;;; GENRE05		Import equipment layers.
  ;;; GENRE06		Import general layers.
  ;;; LAY_DATA		Path of last used layer data file.
  ;;; DIM_PREC		Dimension precision to use.  LUPREC (precision displayed on screen) also uses this setting.
  ;;; PLOT_UNTS		Plotting units, US Customary or System International (store value "uscust" / "si")
  ;;; ADJ_FOR_PAPERS	Adjust dimensiions, etc. for making figures for papers.

  (setq DWG_SCALE      (getcfg (strcat DATAPREFIX "Dwg_Scale"))
        L_UNITS        (getcfg (strcat DATAPREFIX "L_Units"))
        DIM_PREC       (getcfg (strcat DATAPREFIX "Dim_Prec"))
        SET_LMTS       (getcfg (strcat DATAPREFIX "Set_Lmts"))
        SET_LAYS       (getcfg (strcat DATAPREFIX "Set_Lays"))
        IMP_LYTS       (getcfg (strcat DATAPREFIX "Imp_Lyts"))
        P_SIZE         (getcfg (strcat DATAPREFIX "P_Size"))
        GENRE01        (getcfg (strcat DATAPREFIX "Genre01"))
        GENRE02        (getcfg (strcat DATAPREFIX "Genre02"))
        GENRE03        (getcfg (strcat DATAPREFIX "Genre03"))
        GENRE04        (getcfg (strcat DATAPREFIX "Genre04"))
        GENRE05        (getcfg (strcat DATAPREFIX "Genre05"))
        GENRE06        (getcfg (strcat DATAPREFIX "Genre06"))
        LAY_DATA       (getcfg (strcat DATAPREFIX "Lay_Data"))
        PLOT_UNTS      (getcfg (strcat DATAPREFIX "Plot_Unts"))
        ADJ_FOR_PAPERS (getcfg (strcat DATAPREFIX "Adj_For_Papers"))
  ) ;_ End setq

  ;;; If data does not exist set defaults.

  ;;; If L_UNITS does not exist then the drawing scale did not exist.
  (if (or (null L_UNITS) (= L_UNITS "")) 
    (setq L_UNITS (getvar "lunits"))
    (setq L_UNITS (atoi L_UNITS))
  ) ;_ End if

  ;;; Store the value to use to initilize the tiles.
  (setq L_UNITS_HOLDER L_UNITS)

  ;;; Drawing scale.
  ;;; If it does not exist set it to 1.
  ;;; If it does exist the convert it to a real based on the units it was stored in
  ;;; then use the ai_utils to convert is to a string for the dialog box using the
  ;;; current drawing units.  L_UNITS then has to be set to the current drawing units
  ;;; so that it is compatible with the new drawing scale format.

  (if (or (null DWG_SCALE) (= DWG_SCALE "")) 
    (setq DWG_SCALE "1")
    (setq  ;DWG_SCALE	(AI_RTOS (distof DWG_SCALE L_UNITS))
          L_UNITS (getvar "lunits")
    ) ;_ End setq
  ) ;_ End if

  (if (or (null DIM_PREC) (= DIM_PREC "")) 
    (if (= L_UNITS 4)  ;_ If the active units are architectural.
      (setq DIM_PREC "4") ;_ Set primary dim units to 1/16th precision.
      (setq DIM_PREC "0") ;_ Else set primary dim units to 0 precision.
    ) ;_ End if for determining which units are active.
  ) ;_ End if for primary dimension precision units.
  (if (or (null P_SIZE) (= P_SIZE "")) 
    (setq P_SIZE "ps36x24")
  ) ;_ End if
  (if (or (null PLOT_UNTS) (= PLOT_UNTS "")) 
    (setq PLOT_UNTS "uscust")
  ) ;_ End if
  (foreach TEMP 
    '(SET_LMTS IMP_LYTS SET_LAYS GENRE01 GENRE02 GENRE03 GENRE04 GENRE05 GENRE06 
      ADJ_FOR_PAPERS
     )
    (if (or (null (eval TEMP)) (= (eval TEMP) "")) 
      (set TEMP "0")
    ) ;_ End if
  ) ;_ End foreach

  ;;; Add elements to dimension precision list

  ;;; Create global variables
  (setq PREC_LIST_ARCH (list "0'-0\"" "0'-0 1/2\"" "0'-0 1/4\"" "0'-0 1/8\"" 
                             "0'-0 1/16\"" "0'-0 1/32\"" "0'-0 1/64\"" "0'-0 1/128\"" 
                             "0'-0 1/256\""
                       ) ;_ End list
        PREC_LIST_DIM  (list "0" "0.0" "0.00" "0.000" "0.0000" "0.00000" "0.000000" 
                             "0.0000000" "0.00000000"
                       ) ;_ End list
  ) ;_ End setq

  (start_list "dimprec")
  (mapcar 
    'add_list
    (cond 
      ((= L_UNITS_HOLDER 4)
       PREC_LIST_ARCH
      )
      ((= L_UNITS_HOLDER 2)
       PREC_LIST_DIM
      )
      ((t)
       PREC_LIST_ARCH
      )
    ) ;_ End cond
  ) ;_ End mapcar
  (end_list)

  ;;; INITIATE TILES
  ;;; Initilize the plotting units (us / si)
  (set_tile PLOT_UNTS "1")

  ;;; Initilize the dimension precision
  (set_tile "dimprec" DIM_PREC)

  ;;; Initilize the proper tile for the display units (arch / decim).
  (cond 
    ((= L_UNITS_HOLDER 4)
     (set_tile "arch" "1")
    )
    ((= L_UNITS_HOLDER 2)
     (set_tile "decim" "1")
    )
    ((t)
     (set_tile "arch" "1")
    ) ;_ If neither has been selected set a default.
  ) ;_ End cond

  (mapcar 
    'set_tile
    (list "dwg_scale" "set_lmts" "imp_lyts" "set_lays" "genre01" "genre02" "genre03" 
          "genre04" "genre05" "genre06" "adj_for_papers"
    ) ;_ End list
    (list DWG_SCALE SET_LMTS IMP_LYTS SET_LAYS GENRE01 GENRE02 GENRE03 GENRE04 
          GENRE05 GENRE06 ADJ_FOR_PAPERS
    ) ;_ End list
  ) ;_ End mapcar
  (mapcar 
    'action_tile
    (list "dwg_scale" "imp_lyts" "set_lays" "lay_data" "brws_lay_data" "accept" 
          "arch" "si" "decim"
    ) ;_ End list
    (list "(DWG_SCALE_CHECK $value)" "(TOGGLE_PSIZES $value)" 
          "(TOGGLE_LAYERS $value)" "(EDITLAYDATA $value)" "(BRWSLAYDATA)" "(DDSETUP_OUT)" 
          "(ARCH_ACTIVATED)" "(SI_ACTIVATED)" "(DECIM_ACTIVATED)"
    ) ;_ End list
  ) ;_ End mapcar

  (TOGGLE_LAYERS (get_tile "set_lays"))
  (TOGGLE_PSIZES (get_tile "imp_lyts"))

  ;;; Initialize the proper paper size tile.
  (set_tile P_SIZE "1")

  ;;; Initialize the proper units tile.
  (set_tile PLOT_UNTS "1")

  (if LAY_DATA 
    (set_tile "lay_data" LAY_DATA)
    (set_tile "lay_data" "")
  ) ;_ End if

  ;;; Run dialog session.
  (start_dialog)
  (unload_dialog DCL_ID)

  ;;; Test for running setup.
  (if (= RUN_SETUP "Yes")
    (progn
      (load "setupworker")
      (SETUP)
		)
  ) ;_ End if

  ;;; Exit.
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "clayer" "regenmode" "expert" "osmode")
    (list CLAY RGMOD EXPRT OSM)
  ) ;_ End mapcar

  (princ "Setup complete.")
  (vla-setvariable VL-ADOC "cmdecho" CMD)
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

(defun TOGGLE_LAYERS (TEMP / TOG_LIST) 
  (setq TOG_LIST (list "genre01" "genre02" "genre03" "genre04" "genre05" "genre06" 
                       "lay_label" "lay_data" "brws_lay_data"
                 ) ;_ End list
  ) ;_ End setq
  (if (= TEMP "1") 
    (mapcar '(lambda (TEMP) (mode_tile TEMP 0)) TOG_LIST)
    (mapcar '(lambda (TEMP) (mode_tile TEMP 1)) TOG_LIST)
  ) ;_ End if
) ;_ End defun

;;; Switch the status of the layout tiles when the import button is
;;; toggled.
(defun TOGGLE_PSIZES (TEMP) 
  (if (= TEMP "1") 
    (mapcar 
      '(lambda (TEMP) (mode_tile TEMP 0))
      (list "ps36x24" "ps42x30")
    ) ;_ End mapcar
    (mapcar 
      '(lambda (TEMP) (mode_tile TEMP 1))
      (list "ps36x24" "ps42x30")
    ) ;_ End mapcar
  ) ;_ End if
) ;_ End defun

(defun DWG_SCALE_CHECK (VALUE / UNITS) 
  (setq UNITS (if (= (get_tile "arch") "1") 
                4
                2
              ) ;_ End if
        VALUE (distof VALUE UNITS)
  ) ;_ End setq
  (if (> VALUE 0) 
    (progn 
      (set_tile "dwg_scale" (LE_RTOS VALUE UNITS))
      (if 
        (= 
          (get_tile "error")
          "Drawing scale has to be greater than zero."
        ) ;_ End =
        (set_tile "error" "")
      ) ;_ End if to clear error if it is a increment analysis error
    ) ;_ End progn
    (set_tile 
      "error"
      "Drawing scale has to be greater than zero."
    ) ;_ End set_tile
  ) ;_ End if
) ;_ End defun

(defun EDITLAYDATA (LAY_DATA_NEW) 
  (if (not (findfile LAY_DATA_NEW)) 
    (progn 
      (set_tile "error" "Layer data file not found.")
      (mode_tile "lay_data" 3)
    ) ;_ End prong
    (if (= (get_tile "error") "Layer data file not found.") 
      (set_tile "error" "")
    ) ;_ End if to clear error if it is a material property file error.
  ) ;_ End if
) ;_ End defun

(defun BRWSLAYDATA (/ LAY_DATA_NEW) 
  (if LAY_DATA 
    (setq LAY_DATA_DIR (car (fnsplitl LAY_DATA)))
  ) ;_ End if
  (if (= LAY_DATA_DIR NIL) 
    (setq LAY_DATA_DIR "")
  ) ;_ End if
  (setq LAY_DATA_NEW (getfiled 
                       "Select a layer data file"
                       LAY_DATA_DIR
                       "lst"
                       6
                     ) ;_ End getfiled
  ) ;_ End setq
  (if LAY_DATA_NEW 
    (progn 
      (set_tile "lay_data" LAY_DATA_NEW)
      (if (= (get_tile "error") "Layer data file not found.") 
        (set_tile "error" "")
      ) ;_ End if to clear error if it is a material property file error.
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun


(defun DDSETUP_OUT (/ ADJ_FOR_PAPERS DIM_PREC DWG_SCALE GENRE01 GENRE02 GENRE03 
                    GENRE04 GENRE05 GENRE06 IMP_LYTS L_UNITS LAY_DATA LAY_DATA_NEW 
                    PLOT_UNTS SET_LAYS SET_LMTS TEMP
                   ) 
  (setq LAY_DATA_NEW (findfile (get_tile "lay_data"))
        SET_LAYS     (get_tile "set_lays")
  ) ;_ End set
  (if 
    (or 
      (and LAY_DATA_NEW (= SET_LAYS "1"))
      (= SET_LAYS "0")
    ) ;_ End or
    (progn 
      (setq TEMP    (get_tile "dwg_scale")
            L_UNITS (getvar "lunits")
      ) ;_ End setq
      (DWG_SCALE_CHECK TEMP)
      (if 
        (/= 
          (get_tile "error")
          "Drawing scale has to be greater than zero."
        ) ;_ End /=
        (progn 
          (setq DWG_SCALE      (get_tile "dwg_scale")
                DIM_PREC       (get_tile "dimprec")
                PLOT_UNTS      (get_tile "uscust")
                ADJ_FOR_PAPERS (get_tile "adj_for_papers")
                SET_LMTS       (get_tile "set_lmts")
                IMP_LYTS       (get_tile "imp_lyts")
                SET_LAYS       (get_tile "set_lays")
                GENRE01        (get_tile "genre01")
                GENRE02        (get_tile "genre02")
                GENRE03        (get_tile "genre03")
                GENRE04        (get_tile "genre04")
                GENRE05        (get_tile "genre05")
                GENRE06        (get_tile "genre06")
                RUN_SETUP      "Yes"
          ) ;_ End setq

          (foreach TEMP (list "ps36x24" "ps42x30") 
            (if (= (get_tile TEMP) "1") 
              (setcfg (strcat DATAPREFIX "P_Size") TEMP)
            ) ;_ End if
          ) ;_ End foreach

          (cond 
            ((= (get_tile "arch") "1")
             (setq L_UNITS 4)
            )
            ((= (get_tile "decim") "1")
             (setq L_UNITS 2)
            )
          ) ;_ End cond

          (foreach TEMP (list "uscust" "si") 
            (if (= (get_tile TEMP) "1") 
              (setcfg (strcat DATAPREFIX "Plot_Unts") TEMP)
            ) ;_ End if
          ) ;_ End foreach

          (done_dialog)

          (mapcar 
            'setcfg
            (mapcar 
              '(lambda (TEMP) 
                 (strcat DATAPREFIX TEMP)
               ) ;_ End lambda
              (list "Dwg_Scale" "L_Units" "Dim_Prec" "Set_Lmts" "Imp_Lyts" "Set_Lays" 
                    "Genre01" "Genre02" "Genre03" "Genre04" "Genre05" "Genre06" 
                    "Adj_For_Papers"
              ) ;_ End list
            ) ;_ End mapcar
            (list 
              DWG_SCALE
              (itoa L_UNITS)
              DIM_PREC
              SET_LMTS
              IMP_LYTS
              SET_LAYS
              GENRE01
              GENRE02
              GENRE03
              GENRE04
              GENRE05
              GENRE06
              ADJ_FOR_PAPERS
            ) ;_ End list
          ) ;_ End mapcar
          (if LAY_DATA_NEW 
            (setcfg 
              (strcat DATAPREFIX "Lay_Data")
              LAY_DATA_NEW
            ) ;_ End setcfg
          ) ;_ End if
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End then progn for layer file test
    (set_tile "error" "Layer data file not found.")
  ) ;_ End if
) ;_ End defun


(defun *ERROR* (MSG / LMBD1 LMBD2 VL-ADOC) 
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "clayer" "regenmode" "expert" "cmdecho" "osmode")
    (list CLAY RGMOD EXPRT CMD OSM)
  ) ;_ End mapcar.
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun.

(load "ai_utils")