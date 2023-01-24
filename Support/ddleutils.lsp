;;; ddleutils.lsp
;;; Portions taken from ddmodify.lsp
;;; Remainder copyright 1999-2000 by Lance A. Endres

(defun C:DDLEUTILS (/ CMD)
    (setq OLDERR  *ERROR*
          *ERROR* LEUTILSERR
          CMD     (getvar "cmdecho")
          DCL_ID  (load_dialog "DDLEUTILS.DCL")
    ) ;_ End setq
    (setvar "cmdecho" 0)

;;; Gather data from configuration file

    (setq AUTO_PS  (getcfg "AppData/Leader_Utils/Auto_PS")
          TXT_LAY  (getcfg "AppData/Leader_Utils/Txt_Lay")
          ANG_MODE (getcfg "AppData/Leader_Utils/Ang_Mode")
          ROT_ANG  (getcfg "AppData/Leader_Utils/Rot_Ang")
          LD_LAY   (getcfg "AppData/Leader_Utils/Ld_Lay")
    ) ;_ End setq

;;; If data does not exist set defaults

    (if (or (null ANG_MODE) (= ANG_MODE ""))
        (setq ANG_MODE (getvar "aunits"))
        (setq ANG_MODE (atoi ANG_MODE))
    ) ;_ End if
    (if (or (null AUTO_PS) (= AUTO_PS ""))
        (setq AUTO_PS "0")
    ) ;_ End if
    (if (or (null TXT_LAY) (= TXT_LAY ""))
        (setq TXT_LAY "1")
    ) ;_ End if
    (if (null ROT_ANG)
        (setq ROT_ANG 0)
        (setq ROT_ANG (angtof ROT_ANG ANG_MODE))
    ) ;_ End if
    (if (or (null LD_LAY) (= LD_LAY ""))
        (setq LD_LAY "TEXT")
    ) ;_ End if

;;; Run dialog

    (RUN_DIALOG)

;;; Exit program

    (unload_dialog DCL_ID)
    (setvar "cmdecho" CMD)
    (setq *ERROR* OLDERR)
    (princ)
) ;_ End defun

(defun RUN_DIALOG ()

;;; Load dialog file and verify loading 

    (if (not (new_dialog "LEUTILS" DCL_ID))
        (exit)
    ) ;_ End if test to insure dialog box loaded

;;; Initiate Tiles

    (set_tile "auto_ps" AUTO_PS)
    (set_tile "txt_lay" TXT_LAY)
    (action_tile "txt_lay" "(TOGGLE_LD_LAY)")
    (set_tile "rot_ang" (angtos ROT_ANG))
    (action_tile "rot_ang" "(ROT_ANG_ACT $value)")
    (set_tile "ld_lay" LD_LAY)
    (action_tile "ld_lay" "(LD_LAY_CHECK $value)")
    (action_tile "accept" "(LEUTILS_OUT)")
    (action_tile
        "getlayer"
        "(set_tile \"ld_lay\" (GETLAYER (get_tile \"ld_lay\")))"
    ) ;_ End action_tile
    (action_tile "picklayer" "(PICKLAYER)")
    (TOGGLE_LD_LAY)

;;; Run dialog

    (start_dialog)
) ;_ End defun

(defun PICKLAYER ()
    (done_dialog)
    (setq LD_LAY (SET_AUTO_LAYER))
    (RUN_DIALOG)
) ;_ End defun

(defun TOGGLE_LD_LAY (/ TEMP)
    (setq TEMP (get_tile "txt_lay"))
    (if (= TEMP "1")
        (progn
            (mode_tile "ld_lay" 0)
            (mode_tile "getlayer" 0)
            (mode_tile "txt_lbl" 0)
        ) ;_ End progn
        (progn
            (mode_tile "ld_lay" 1)
            (mode_tile "getlayer" 1)
            (mode_tile "txt_lbl" 1)
        ) ;_ End progn
    ) ;_ End if
) ;_ End defun

(defun LD_LAY_CHECK (TEMP)
    (if (snvalid TEMP)
        (progn
            (set_tile "ld_lay" (strcase TEMP))
            (if (= (get_tile "error") "Not a valid layer name.")
                (set_tile "error" "")
            ) ;_ End if test for resetting error tile
        ) ;_ End progn
        (set_tile "error" "Not a valid layer name.")
    ) ;_ End if
) ;_ End defun

(defun ROT_ANG_ACT (TEMP / TEMP TEMP_MODE)
    (setq TEMP      (angtof TEMP)
          TEMP_MODE (getvar "aunits")
    ) ;_ End setq
    (if (numberp TEMP)
        (progn
            (set_tile "rot_ang" (angtos TEMP TEMP_MODE))
            (if (= (get_tile "error") "Not a valid angle.")
                (set_tile "error" "")
            ) ;_ End if for resetting error tile
        ) ;_ End progn
        (set_tile "error" "Not a valid angle.")
    ) ;_ End if
) ;_ End defun

(defun LEUTILS_OUT (/ TEMP TEMP2 TEMP_MODE)
    (setq TEMP      (angtof (get_tile "rot_ang"))
          TEMP_MODE (getvar "aunits")
          TEMP2     (get_tile "ld_lay")
    ) ;_ End setq
    (if (and (numberp TEMP) (snvalid TEMP2))
        (progn
            (setq AUTO_PS  (get_tile "auto_ps")
                  TXT_LAY  (get_tile "txt_lay")
                  ANG_MODE (getvar "aunits")
                  ROT_ANG  (angtof (get_tile "rot_ang") ANG_MODE)
                  LD_LAY   (strcase (get_tile "ld_lay"))
            ) ;_ End setq
            (done_dialog)
            (setcfg "AppData/Leader_Utils/Auto_PS" AUTO_PS)
            (setcfg "AppData/Leader_Utils/Txt_Lay" TXT_LAY)
            (setcfg "AppData/Leader_Utils/Ang_Mode" (itoa ANG_MODE))
            (setcfg "AppData/Leader_Utils/Rot_Ang"
                    (angtos ROT_ANG ANG_MODE)
            ) ;_ End setcfg
            (setcfg "AppData/Leader_Utils/Ld_Lay" LD_LAY)
        ) ;_ End progn
    ) ;_ End if
) ;_ End defun

(defun C:LAYATO (/ CMD)
    (setq OLDERR  *ERROR*
          *ERROR* LEUTILSERR
          CMD     (getvar "cmdecho")
    ) ;_ End setq
    (setvar "cmdecho" 0)
    (setq LD_LAY (SET_AUTO_LAYER))
    (setcfg "AppData/Leader_Utils/Ld_Lay" LD_LAY)
    (print LD_LAY)
    (princ "set as LE autolayer.")
    (setvar "cmdecho" CMD)
    (princ)
) ;_ End defun

(defun SET_AUTO_LAYER (/ ENT1 LAY)
    (while (not ENT1)
        (setq ENT1
                 (entsel "\nPick entity to set as current LE autolayer: "
                 ) ;_ End entsel
        ) ;_ End setq
    ) ;_ End while
    (setq LD_LAY (cdr (assoc 8 (entget (car ENT1)))))
) ;_ End defun

(defun GETLAYER (ELAYER / OLD-IDX LAYNAME ON OFF FROZTH LINETYPE)
    ;; Create layer list the first time the layer
    ;; dialogue is called.
    (MAKE_LAY_LISTS)
    (if (tblsearch "layer" ELAYER)
        (progn
            (setq LAY-IDX (GETINDEX ELAYER LAYNMLST))
        ) ;_ End progn
        (setq LAY-IDX 0)
    ) ;_ End if
    (if (= (get_tile "error") "")
        (progn
            (if (not (new_dialog "setlayer" DCL_ID))
                (exit)
            ) ;_ End if
            (set_tile "cur_layer" (getvar "clayer"))
            (start_list "list_lay")
            (mapcar 'add_list LAYNMLST) ; initialize list box
            (end_list)
            (setq OLD-IDX LAY-IDX)
            (LAYLIST_ACT (itoa LAY-IDX))
            (action_tile "list_lay" "(laylist_act $value)")
            (action_tile "edit_lay" "(layedit_act $value)")
            (action_tile "accept" "(test_ok)")
            (action_tile "cancel" "(reset_lay)")
            (if (= (start_dialog) 1)    ; User pressed OK
                (progn
                    (set_tile "t_layer" LAYNAME)
                    (setq ELAYER LAYNAME)
                    ;; If layer equals bylayer reset color tile
                    (if (= ECOLOR 256)
                        (COL_TILE "show_image" (BYLAYER_COL) NIL)
                    ) ;_ End if
                    LAYNAME
                ) ;_ End progn
                ELAYER
            ) ;_ End if
        ) ;_ End progn
        ELAYER
    ) ;_ End if
) ;_ End defun

(defun MAKE_LAY_LISTS (/ LAYNAME SORTLIST NAME TEMPLIST LAYER_NUMBER)
    (setq SORTLIST nil)
    (setq TEMPLIST (tblnext "LAYER" t))
    (setq LAYER_NUMBER 1)
    (while TEMPLIST

;;; No xref dependent layers, please

        (if (/= (logand 16 (cdr (assoc 70 TEMPLIST))) 16)
            (progn
                (setq NAME (strcase (cdr (assoc 2 TEMPLIST))))
                (setq SORTLIST (cons NAME SORTLIST))
            ) ;_ End progn
        ) ;_ End if

;;; Get the next layer

        (setq TEMPLIST (tblnext "LAYER"))

;;; Not dead message

        (if (= (/ LAYER_NUMBER 50.0) (fix (/ LAYER_NUMBER 50.0)))
            (set_tile "error"
                      (strcat "Collecting..." (itoa LAYER_NUMBER))
            ) ;_ End set_tile
        ) ;_ End if
        (setq LAYER_NUMBER (1+ LAYER_NUMBER))
    ) ;_ End while
    (set_tile "error" "")
    (if (>= (getvar "maxsort") (length SORTLIST))
        (progn
            (if (> LAYER_NUMBER 50)
                (set_tile "error" "Sorting...")
            ) ;_ End if
            (setq SORTLIST (acad_strlsort SORTLIST))
        ) ;_ End progn
        (setq SORTLIST (reverse SORTLIST))
    ) ;_ End if
    (set_tile "error" "")
    (setq LAYNMLST SORTLIST)
) ;_ End defun

(defun GETINDEX (ITEM ITEMLIST / M N)
    (setq N (length ITEMLIST))
    (if (> (setq M (length (member ITEM ITEMLIST))) 0)
        (- N M)
        NIL
    ) ;_ End if
) ;_ End defun

(defun LAYLIST_ACT (INDEX / LAYINFO COLOR DASHDATA)

;;; Update the list box, edit box, and color tile.

    (set_tile "error" "")
    (setq LAY-IDX (atoi INDEX))
    (setq LAYNAME (nth LAY-IDX LAYNMLST))
    (setq LAYINFO (tblsearch "layer" LAYNAME))
    (setq COLOR (cdr (assoc 62 LAYINFO)))
    (setq COLOR (abs COLOR))
    (setq COLNAME (COLORNAME COLOR))
    (set_tile "list_lay" (itoa LAY-IDX))
    (set_tile "edit_lay" LAYNAME)
) ;_ End defun

(defun COLORNAME (COLNUM / CN)
    (setq CN (abs COLNUM))
    (cond ((= CN 1) "red")
          ((= CN 2) "yellow")
          ((= CN 3) "green")
          ((= CN 4) "cyan")
          ((= CN 5) "blue")
          ((= CN 6) "magenta")
          ((= CN 7) "white")
          (t (itoa CN))
    ) ;_ End cond
) ;_ End defun

(defun LAYEDIT_ACT (LAYVALUE)
    (setq LAYVALUE (xstrcase LAYVALUE))
    (if (setq LAY-IDX (GETINDEX LAYVALUE LAYNMLST))
        (progn
            (set_tile "error" "")
            (LAYLIST_ACT (itoa LAY-IDX))
        ) ;_ End progn
        (progn
            (set_tile "error" "Invalid layer name.")
            (setq LAY-IDX OLD-IDX)
        ) ;_ End progn
    ) ;_ End if
) ;_ End defun

(defun TEST_OK ()
    (if (= (get_tile "error") "")
        (done_dialog 1)
    ) ;_ End if
) ;_ End defun

(defun RESET_LAY ()
    (setq LAY-IDX OLD-IDX)
    (done_dialog 0)
) ;_ End defun

(defun LEUTILSERR (S)
    (if (/= S "Function cancelled")
        (princ (strcat "\nError: " S))
    ) ;_ End if
    (setq *ERROR* OLDERR)
    (setvar "cmdecho" CMD)
    (princ)
) ;_ End defun