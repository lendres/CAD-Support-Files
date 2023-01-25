;;; Copyright Lance A. Endres

(load "layouttools")
(load "le_utils")
(load "ai_utils")
(vl-load-com)

(defun C:CHANGE8X11 (/ ADD_1 ADD_2 ATTD ATTRQ ATTR1 ATTR2 CLAY CNT DATA_FILE 
                     DATA_LIST DEFLAY DIMS FILE_HAND FLAG INSPT ISSFOR LAYDISPST 
                     LAYOUTNM LAYR LINE1 OWN OWN_FILE OWN_LIST PLOTTEMPL PLTSC PROJ_1 
                     PROJ_2 PT1 PT2 ROTANG TEMP VERS XDIST XPT YDIST YPT ZMPT ZPT
                    ) 

  (setq OLDERR    *ERROR*
        *ERROR*   CHANGE8X11ERR
        CMD       (getvar "cmdecho")
        ATTD      (getvar "attdia")
        ATTRQ     (getvar "attreq")
        DATA_FILE (strcat (getvar "tempprefix") "8X11CHG1.DAT")
        DATA_FILE (findfile DATA_FILE)
        OWN_FILE  (strcat (getvar "tempprefix") "8X11CHG2.DAT")
        OWN_FILE  (findfile OWN_FILE)
        VERS      (substr (getvar "acadver") 1 2)
        LAYDISPST (GET-LAYOUTS-DISP-STATE)
        CLAY      (vla-get-activelayer 
                    (vla-get-activedocument (vlax-get-acad-object))
                  ) ;_ End vla-get-activelayer
  ) ;_ End setq
  (setvar "cmdecho" 0)
  (command "_.undo" "begin")
  (setvar "attdia" 0)
  (setvar "attreq" 1)

  ;;; Check the state of the layout tabs.
  ;;; If they are off - turn them on so program can function.

  (if (= LAYDISPST :vlax-false) 
    (PUT-LAYOUTS-DISP-STATE :vlax-true)
  ) ;_ End if
  (if (null DATA_FILE) 
    (progn 
      (setq DATA_FILE (strcat (getvar "tempprefix") "8X11CHG1.DAT")
            FILE_HAND (open DATA_FILE "w")
      ) ;_ End setq
      (close FILE_HAND)
    ) ;_ End then progn
  ) ;_ End if to ensure that data file exists
  (if (null OWN_FILE) 
    (progn 
      (setq OWN_FILE  (strcat (getvar "tempprefix") "8X11CHG2.DAT")
            FILE_HAND (open OWN_FILE "w")
      ) ;_ End setq
      (close FILE_HAND)
    ) ;_ End then progn
  ) ;_ End if to ensure that owner file exists

  ;;; Establish the layer for the view port.
  ;;; Use the defpoints layer for pre-AutoCAD 2000
  ;;; Use a layer called View Ports for AutoCAD 2000
  (if (= VERS "14") 
    (setq LAYR "defpoints")
    (setq LAYR "View Ports")
  ) ;_ End if
  (setq DEFLAY (tblsearch "layer" LAYR)
        FRZEN  (cdr (assoc 70 DEFLAY))
  ) ;_ End setq
  (if FRZEN 
    (progn 
      (if (= (logand FRZEN 1) 1) 
        (command "_.-layer" "thaw" LAYR "")
      ) ;_ End if for a frozen layer
      (if (= (logand FRZEN 4) 4) 
        (command "_.-layer" "unlock" LAYR "")
      ) ;_ End if for a locked layer
    ) ;_ End progn
  ) ;_ End if

  ;;; Get information for attributes.
  (setq FILE_HAND (open DATA_FILE "r")
        FLAG      1
        DATA_LIST '()
        PRIN_LIST '("Project Title 1" "Project Title 2" "Project Address 1" 
                    "Project Address 2" "Issued For" "Reference Drawing" "Issued Date" 
                    "Drawing No." "Scale"
                   ) ;_ End list
  ) ;_ End setq
  (while (setq LINE1 (read-line FILE_HAND)) 
    (setq DATA_LIST (append DATA_LIST (list LINE1)))
  ) ;_ End while for creating a list from data in file
  (close FILE_HAND)
  (setq FILE_HAND (open DATA_FILE "w")
        CNT       0
  ) ;_ End setq
  (foreach TEMP PRIN_LIST 
    (setq LINE1 (nth CNT PRIN_LIST))
    (if DATA_LIST 
      (setq ATTR1 (nth CNT DATA_LIST))
      (setq ATTR1 "")
    ) ;_ End if
    (if ATTR1 
      (princ (strcat LINE1 " <" ATTR1 ">: "))
      (princ (strcat LINE1 " <>: "))
    ) ;_ End if
    (setq ATTR2 (getstring t))
    (if (/= ATTR2 "") 
      (progn 
        (if (= TEMP "Issued For") 
          (setq ISSFOR ATTR2)
        ) ;_ End if
        (write-line ATTR2 FILE_HAND)
      ) ;_ End progn
      (progn 
        (if (= TEMP "Issued For") 
          (setq ISSFOR ATTR1)
        ) ;_ End if
        (write-line ATTR1 FILE_HAND)
      ) ;_ End progn
    ) ;_ End if
    (setq CNT (1+ CNT))
  ) ;_ End foreach
  (setq FLAG NIL)
  (close FILE_HAND)

  ;;; Insert title block and set attributes.
  (if (= VERS "14") 
    (progn 

      ;;; Test for a switch to paperspace.
      (if (= (getvar "tilemode") 1) 
        (progn 
          (setvar "ltscale" 1)
          (command "_.tilemode" 0)
        ) ;_ End progn
      ) ;_ End if test for paperspace

      ;;; Get insertion point.
      (while (= INSPT NIL) 
        (setq INSPT (getpoint 
                      "\nInsertion point for 8 x 11 Title Block: "
                    ) ;_ End getpoint
        ) ;_ End setq
      ) ;_ End while to ensure a point is picked
    ) ;_ End progn
  ) ;_ End if
  (initget "0 90")
  (setq ROTANG (getkword "\nRotation angle <0>/90: "))
  (if (= ROTANG "90") 
    (setq ROTANG   90
          LAYOUTNM "Land"
    ) ;_ End setq
    (setq ROTANG   0
          LAYOUTNM "Port"
    ) ;_ End setq
  ) ;_ End if for setting rotation angle

  ;;; If AutoCAD 2000 is being used establish a new layout.
  (if (/= VERS "14") 
    (progn 
      (if (= (getvar "pstylemode") 0) 
        (setq PLOTTEMPL (findfile "Named PS Plotting Templates.dwt"))
        (setq PLOTTEMPL (findfile "Color PS Plotting Templates.dwt"))
      ) ;_ End if
      (if PLOTTEMPL 
        (progn 
          (setq LAYOUTNM (strcat 
                           (GETPRINTER)
                           " 8.5x11 "
                           LAYOUTNM
                         ) ;_ End strcat
          ) ;_ End setq
          (command "_.layout" "template" PLOTTEMPL LAYOUTNM)
          (AX:ACTIVATELASTLAYOUT)
          (command "_.pspace")
          (setq XDIST (cdr (assoc 40 (entget (entlast))))
                YDIST (cdr (assoc 41 (entget (entlast))))
          ) ;_ End setq
          (if (= ROTANG 0) 
            (setq INSPT (list 
                          (/ (- XDIST 7.9) 2)
                          (/ (- YDIST 10.25) 2)
                          0
                        ) ;_ End list
            ) ;_ End setq
            (setq INSPT (list 
                          (- XDIST (/ (- XDIST 10.25) 2))
                          (/ (- YDIST 7.9) 2)
                          0
                        ) ;_ End list
            ) ;_ End setq
          ) ;_ End if
          (command "_.erase" (entlast) "")
        ) ;_ End progn
        (progn 
          (alert 
            "Plotting Template file not found.\nA blank layout will be used."
          ) ;_ End alert
          (MAKE-NEW-LAYOUT)
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End progn
  ) ;_ End if

  (command "_.insert" "8x11_change" INSPT "1" "1" ROTANG)
  (setq FILE_HAND (open DATA_FILE "r"))
  (foreach ATTR1 PRIN_LIST 
    (setq LINE1 (read-line FILE_HAND))
    (command LINE1)
  ) ;_ End foreach
  (close FILE_HAND)

  ;;; Get owner information.
  (initget "Nextel Other NOne")
  (setq OWN (getkword "\nProject Owner <Nextel>/Other/NOne: "))
  (cond 
    ((= OWN "Other")
     (setq FILE_HAND (open OWN_FILE "r")
           FLAG      2
           OWN_LIST  '()
           PRIN_LIST '("Owner" "Owner Address 1" "Owner Address 2")
     ) ;_ End setq
     (while (setq LINE1 (read-line FILE_HAND)) 
       (setq OWN_LIST (append OWN_LIST (list LINE1)))
     ) ;_ End while for creating a list from data in file
     (close FILE_HAND)
     (setq FILE_HAND (open OWN_FILE "w")
           CNT       0
     ) ;_ End setq
     (foreach TEMP PRIN_LIST 
       (setq LINE1 (nth CNT PRIN_LIST))
       (if OWN_LIST 
         (setq ATTR1 (nth CNT OWN_LIST))
         (setq ATTR1 "")
       ) ;_ End if
       (if ATTR1 
         (princ (strcat LINE1 " <" ATTR1 ">: "))
         (princ (strcat LINE1 " <>: "))
       ) ;_ End if
       (setq ATTR2 (getstring t))
       (if (/= ATTR2 "") 
         (write-line ATTR2 FILE_HAND)
         (write-line ATTR1 FILE_HAND)
       ) ;_ End if
       (setq CNT (1+ CNT))
     ) ;_ End foreach
     (setq FLAG NIL)
     (close FILE_HAND)
     (command "_.insert" "8x11_own_other" INSPT "1" "1" ROTANG)
     (setq FILE_HAND (open OWN_FILE "r"))
     (foreach ATTR1 PRIN_LIST 
       (setq LINE1 (read-line FILE_HAND))
       (command LINE1)
     ) ;_ End foreach
     (close FILE_HAND)
    ) ;_ End cond for other
    ((or (= OWN "Nextel") (= OWN NIL))
     (command "_.insert" "8x11_nextel_logo" INSPT "1" "1" ROTANG)
    ) ;_ End cond for Nextel
  ) ;_ End cond

  (setq DIMS  (getvar "dimscale")
        DIMS  (/ 1 DIMS)
        PLTSC (strcat (rtos DIMS 2 8) "xp")
        XPT   (car INSPT)
        YPT   (cadr INSPT)
        ZPT   (caddr INSPT)
  ) ;_ End setq
  (if (= ROTANG 0) 
    (setq PT1 (list 
                (+ XPT 0.13587862)
                (+ YPT 1.05927862)
                ZPT
              ) ;_ End list
          PT2 (list 
                (+ XPT 7.76412138)
                (+ YPT 10.11412138)
                ZPT
              ) ;_ End list
    ) ;_ End setq
    (setq PT1 (list 
                (- XPT 1.05927862)
                (+ YPT 0.13587862)
                ZPT
              ) ;_ End list
          PT2 (list 
                (- XPT 10.11412138)
                (+ YPT 7.76412138)
                ZPT
              ) ;_ End list
    ) ;_ End setq
  ) ;_ End if for setting vport points

  ;;; Set the layer for the view port.
  (if (not DEFLAY) 
    (progn 
      (command "_.-layer" "make" LAYR "")
      (if (/= VERS "14") 
        (command "_.-layer" "plot" "no" LAYR "")
      ) ;_ End if
    ) ;_ End progn
    (command "_.-layer" "set" LAYR "")
  ) ;_ End if

  ;;; Create a view port, enter model space and zoom to plot scale.
  (command 
    "_.mview"
    PT1
    PT2
    "_.mspace"
  ) ;_ End command
  (while (= ZMPT NIL) 
    (setq ZMPT (getpoint "\nCenter point for zoom: ")) ;_ End setq
  ) ;_ End while to insure a point
  (command "_.zoom" "c" ZMPT PLTSC "_.pspace")
  (if (/= VERS "14") 
    (if (/= (AI_STRTRIM ISSFOR) "") 
      (command 
        "_.layout"
        "rename"
        ""
        (GET-NEW-LAYOUT-NAME (strcat ISSFOR "(") ")")
      ) ;_ End command
    ) ;_ End if
  ) ;_ End if
  (if (= LAYDISPST :vlax-false) 
    (PUT-LAYOUTS-DISP-STATE LAYDISPST)
  ) ;_ End if
  (vla-put-activelayer 
    (vla-get-activedocument (vlax-get-acad-object))
    CLAY
  ) ;_ End vla-put-activelayer
  (setvar "attdia" ATTD)
  (setvar "attreq" ATTRQ)
  (setq *ERROR* OLDERR)
  (command "_.undo" "end")
  (setvar "cmdecho" CMD)
  (princ)
) ;_ End defun

(defun GETPRINTER (/ DCL_ID FILE_HAND LINE1 PRINT_FILE PRINT_LIST) 
  (setq DCL_ID     (load_dialog "CHANGE8X11.DCL")
        PRINT_FILE (findfile "PrinterNames.txt")
  ) ;_ End setq
  (if (not (new_dialog "PRINTSELECT" DCL_ID)) 
    (exit)
  ) ;_ End if test to insure dialog box loaded
  (if PRINT_FILE 
    (progn 
      (setq FILE_HAND  (open PRINT_FILE "r")
            PRINT_LIST '()
      ) ;_ End setq
      (while (setq LINE1 (GETNEWLINE FILE_HAND)) 
        (if (/= (substr LINE1 1 1) ";") 
          (setq PRINT_LIST (append PRINT_LIST (list LINE1)))
        ) ;_ End if
      ) ;_ End while
      (close FILE_HAND)

      ;;; Initiate tiles.
      (start_list "print_sel")
      (mapcar 'add_list PRINT_LIST)
      (end_list)
      (set_tile "print_sel" "1")
      (action_tile "print_sel" "(CLEAR_ERROR)")
      (action_tile "exit" "(GETPRINT_OUT)")

      ;;; Run dialog session.
      (start_dialog)
      (unload_dialog DCL_ID)
      (setq PRINT_LIST (nth LOC PRINT_LIST))
    ) ;_ End progn
    (progn 
      (alert "\nPrinter file not found.")
      (exit)
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun

(defun CLEAR_ERROR () 
  (if (/= (CNT_TILE_ITEMS "print_sel") 0) 
    (set_tile "error" "")
  ) ;_ End if
) ;_ End defun

(defun GETPRINT_OUT () 
  (if (/= (CNT_TILE_ITEMS "print_sel") 0) 
    (progn 
      (setq LOC (atoi (get_tile "print_sel")))
      (done_dialog)
    ) ;_ End progn
    (set_tile "error" "Printer not selected.")
  ) ;_ End if
) ;_ End defun

(defun CHANGE8X11ERR (S / CNT LINE1 TEMP) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (cond 
    ((= FLAG 1)
     (close FILE_HAND)
     (setq FILE_HAND (open DATA_FILE "w")
           CNT       0
     ) ;_ End setq
     (if DATA_LIST 
       (progn 
         (foreach TEMP DATA_LIST 
           (setq LINE1 (nth CNT DATA_LIST))
           (write-line LINE1 FILE_HAND)
           (setq CNT (1+ CNT))
         ) ;_ End foreach
       ) ;_ End progn
     ) ;_ End if
     (close FILE_HAND)
    ) ;_ End cond for data_list
    ((= FLAG 2)
     (close FILE_HAND)
     (setq FILE_HAND (open OWN_FILE "w")
           CNT       0
     ) ;_ End setq
     (if OWN_LIST 
       (progn 
         (foreach TEMP OWN_LIST 
           (setq LINE1 (nth CNT OWN_LIST))
           (write-line LINE1 FILE_HAND)
           (setq CNT (1+ CNT))
         ) ;_ End foreach
       ) ;_ End progn
     ) ;_ End if
     (close FILE_HAND)
    ) ;_ End cond for data_list
  ) ;_ End cond
  (command "regen")
  (if (= LAYDISPST :vlax-false) 
    (PUT-LAYOUTS-DISP-STATE LAYDISPST)
  ) ;_ End if
  (setvar "attdia" ATTD)
  (setvar "attreq" ATTRQ)
  (vla-put-activelayer 
    (vla-get-activedocument (vlax-get-acad-object))
    CLAY
  ) ;_ End vla-put-activelayer
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun