;;; Copyright Lance A. Endres

(defun c:DDXRMOD (/ CMD DCL_ID TEMP_PREFIX) 
  (setq OLDERR      *ERROR*
        *ERROR*     XRMODERR
        CMD         (getvar "cmdecho")
        DCL_ID      (load_dialog "DDXRMOD.DCL")
        TEMP_PREFIX (getvar "tempprefix")
        FROM_FILE   (strcat TEMP_PREFIX "DDXRMOD1.DAT")
        FROM_FILE   (findfile FROM_FILE)
        TO_FILE     (strcat TEMP_PREFIX "DDXRMOD2.DAT")
        TO_FILE     (findfile TO_FILE)
        MODE_FILE   (strcat TEMP_PREFIX "DDXRMOD3.DAT")
        MODE_FILE   (findfile MODE_FILE)
        MOD         nil
  ) ;End setq
  (setvar "cmdecho" 0)

  ;;; Load dialog file and verify loading.
  (if (not (new_dialog "XRMOD" DCL_ID)) 
    (exit)
  ) ;End if test to insure dialog box loaded

  ;;; Check for data files and if found extract a list of data files from them.  Then update the list boxes.
  (INIATE_BOX FROM_FILE 1)
  (INIATE_BOX TO_FILE 2)
  (INIATE_BOX MODE_FILE 3)

  ;;; Initiate tiles.
  (TOGGLE_REM)

  (action_tile "extract_path" "(TOGGLE_REM) (CLEAR_ERR)")
  (action_tile "new_path" "(TOGGLE_REM) (CLEAR_ERR)")

  (action_tile "find_extract_path" "(FIND_NEW_FILE 1)")
  (action_tile "find_new_path" "(FIND_NEW_FILE 2)")

  (action_tile "add_extract_path" "(GET_NEW_PATH 1)")
  (action_tile "add_new_path" "(GET_NEW_PATH 2)")

  (action_tile "rem_extract_path" "(REMOVE_FILE 1)")
  (action_tile "rem_new_path" "(REMOVE_FILE 2)")

  (action_tile "modify" "(XRMOD_OUT)")

  ;;; Run dialog session.
  (start_dialog)
  (unload_dialog DCL_ID)

  ;;; Check for modification and run if necessary

  (if (= MOD "Yes") 
    (XRMOD)
  ) ;End if test for running xrmod

  ;;; Exit ddxrmod

  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (princ)
)

(defun INIATE_BOX (FILE WHICH / FILE_HAND TEMP_LIST) 
  (if (null FILE) 
    (progn 
      (cond 
        ((= WHICH 1)
         (setq FILE      (strcat TEMP_PREFIX "DDXRMOD1.DAT")
               FROM_FILE FILE
               FILE_HAND (open FILE "w")
         ) ;End setq
        ) ;End cond for from file
        ((= WHICH 2)
         (setq FILE      (strcat TEMP_PREFIX "DDXRMOD2.DAT")
               TO_FILE   FILE
               FILE_HAND (open FILE "w")
         ) ;End setq
        ) ;End cond for from file
        ((= WHICH 3)
         (setq FILE      (strcat TEMP_PREFIX "DDXRMOD3.DAT")
               MODE_FILE FILE
               FILE_HAND (open FILE "w")
         ) ;End setq
        ) ;End cond for from file
      ) ;End cond
      (close FILE_HAND)
    ) ;End then progn
  ) ;End if to ensure that data file exists
  (setq FILE_HAND (open FILE "r")
        TEMP_LIST '()
  ) ;End setq
  (while (setq LINE1 (read-line FILE_HAND)) 
    (setq TEMP_LIST (append TEMP_LIST (list LINE1)))
  ) ;End while for creating a list form data in data files
  (cond 
    ((= WHICH 1)
     (setq FROM_LIST TEMP_LIST)
    ) ;End cond for from list
    ((= WHICH 2)
     (setq TO_LIST TEMP_LIST)
    ) ;End cond for to list
    ((= WHICH 3)
     (if TEMP_LIST 
       (progn 
         (set_tile "extract_path" (nth 0 TEMP_LIST))
         (set_tile "new_path" (nth 1 TEMP_LIST))
       ) ;End progn
     ) ;End if
    ) ;End cond for to list
  ) ;End cond
  (close FILE_HAND)
  (UPDATE_LIST_BOXES WHICH)
)

(defun UPDATE_LIST_BOXES (WHICH) 
  (cond 
    ((= WHICH 1)
     (start_list "extract_path")
     (mapcar 'add_list FROM_LIST)
     (end_list)
    ) ;End which is 1
    ((= WHICH 2)
     (start_list "new_path")
     (mapcar 'add_list TO_LIST)
     (end_list)
    ) ;End which is 2
  ) ;End cond
)

(defun UPDATE_DATA_FILE (WHICH / CNT FILE_HAND LEN) 
  (cond 
    ((= WHICH 1)
     (setq FILE_HAND (open FROM_FILE "w")
           PLIST     FROM_LIST
     ) ;End setq
    ) ;End cond for from_file
    ((= WHICH 2)
     (setq FILE_HAND (open TO_FILE "w")
           PLIST     TO_LIST
     ) ;End setq
    ) ;End cond for to_file
  ) ;End cond
  (setq CNT 0
        LEN (length PLIST)
  ) ;End setq
  (if (/= LEN 0) 
    (progn 
      (while (< CNT LEN) 
        (setq TEMP (nth CNT PLIST)
              CNT  (1+ CNT)
        ) ;End setq
        (write-line TEMP FILE_HAND)
      ) ;End while
    ) ;End progn
  ) ;End if
  (close FILE_HAND)
)

(defun TOGGLE_REM () 
  (if (read (get_tile "extract_path")) 
    (mode_tile "rem_extract_path" 0)
    (mode_tile "rem_extract_path" 1)
  ) ;End if
  (if (read (get_tile "new_path")) 
    (mode_tile "rem_new_path" 0)
    (mode_tile "rem_new_path" 1)
  ) ;End if
)

(defun FIND_NEW_FILE (WHICH / FILE_NEW LST TL) 
  (cond 
    ((= WHICH 1)
     (setq LST FROM_LIST
           TL  "extract_path"
     ) ;End setq
    ) ;End cond which 1
    ((= WHICH 2)
     (setq LST TO_LIST
           TL  "new_path"
     ) ;End setq
    ) ;End cond which 2
  ) ;End cond
  (if (= FILE_DIR nil) 
    (setq FILE_DIR "")
  ) ;End if
  (setq FILE_NEW (getfiled "Select a X-reference file" FILE_DIR "dwg" 6))
  (if 
    (or 
      (and FILE_NEW (= WHICH 1) (not (member FILE_NEW LST)))
      (and FILE_NEW (= WHICH 2))
    ) ;End or
    (progn 
      (setq FILE_DIR (car (fnsplitl FILE_NEW))
            LST      (append LST (list FILE_NEW))
      ) ;End setq
      (cond 
        ((= WHICH 1)
         (setq FROM_LIST LST)
        ) ;End cond which 1
        ((= WHICH 2)
         (setq TO_LIST LST)
        ) ;End cond which 2
      ) ;End cond
      (UPDATE_LIST_BOXES WHICH)
      (UPDATE_DATA_FILE WHICH)
      (if (/= WHICH) 
        (set_tile TL (itoa (LOC_IN_LIST FILE_NEW WHICH)))
      ) ;End if test to allow for multiple same path
      (TOGGLE_REM)
    ) ;End progn
  ) ;End if
)

(defun GET_NEW_PATH (WHICH) 
  (if (new_dialog "ADD_PATH" DCL_ID) 
    (progn 
      (action_tile "accept" "(NEW_PATH_OUT WHICH)")
      (start_dialog)
    ) ;End progn
  ) ;End if test to insure dialog box loaded
)

(defun NEW_PATH_OUT (WHICH / TL) 
  (setq PATH (strcase (get_tile "edit_path")))
  (if (/= PATH "") 
    (progn 
      (cond 
        ((and (= WHICH 1) (not (member PATH FROM_LIST)))
         (setq FROM_LIST (append FROM_LIST (list PATH))
               TL        "extract_path"
         ) ;End setq
        ) ;End cond for from_list
        ((= WHICH 2)
         (setq TO_LIST (append TO_LIST (list PATH))
               TL      "new_path"
         ) ;End setq
        ) ;End cond for to_list
      ) ;End cond
      (done_dialog)
      (UPDATE_LIST_BOXES WHICH)
      (if (/= WHICH 2) 
        (set_tile TL (itoa (LOC_IN_LIST PATH WHICH)))
      ) ;End if test to allow for multiple same path
      (UPDATE_DATA_FILE WHICH)
      (TOGGLE_REM)
    ) ;End progn
    (progn 
      (set_tile "error" "No path has has been indicated.")
      (mode_tile "edit_path" 2)
    ) ;End progn
  ) ;End if
)

(defun LOC_IN_LIST (PATH WHICH / LST) 
  (cond 
    ((= WHICH 1)
     (setq LST FROM_LIST)
    ) ;End cond which 1
    ((= WHICH 2)
     (setq LST TO_LIST)
    ) ;End cond which 2
  ) ;End cond
  (- (length LST) (length (member PATH LST)))
)

(defun REMOVE_FILE (WHICH / CNT LEN LOC PLIST TILE TLIST) 
  (cond 
    ((= WHICH 1)
     (setq TILE  "extract_path"
           PLIST FROM_LIST
     ) ;End setq
    ) ;End extract path cond
    ((= WHICH 2)
     (setq TILE  "new_path"
           PLIST TO_LIST
     ) ;End setq
    ) ;End new path cond
  ) ;End cond
  (MAKE_LISTS WHICH)
  (setq NOS_LIST (reverse NOS_LIST))
  (foreach LOC NOS_LIST 
    (setq LEN   (length PLIST)
          CNT   0
          TLIST '()
    ) ;End setq
    (while (< CNT LEN) 
      (if (/= CNT LOC) 
        (setq TLIST (append TLIST (list (nth CNT PLIST))))
      ) ;End if
      (setq CNT (1+ CNT))
    ) ;End while
    (setq PLIST TLIST)
  ) ;End foreach
  (cond 
    ((= WHICH 1)
     (setq FROM_LIST PLIST)
    ) ;End extract path cond
    ((= WHICH 2)
     (setq TO_LIST PLIST)
    ) ;End new path cond
  ) ;End cond
  (UPDATE_LIST_BOXES WHICH)
  (TOGGLE_REM)
  (UPDATE_DATA_FILE WHICH)
)

(defun MAKE_LISTS (WHICH / LEN LINET NOS PLIST TL) 
  (cond 
    ((= WHICH 1)
     (setq TL    "extract_path"
           PLIST FROM_LIST
     ) ;End setq
    ) ;End extract path cond
    ((= WHICH 2)
     (setq TL    "new_path"
           PLIST TO_LIST
     ) ;End setq
    ) ;End new path cond
  ) ;End cond
  (setq NOS       (get_tile TL)
        NOS_LIST  '()
        CONV_LIST '()
  ) ;End setq
  (if NOS 
    (setq LINET (read NOS))
  ) ;End if
  (while LINET 
    (setq NOS_LIST  (append NOS_LIST (list LINET))
          CONV_LIST (append CONV_LIST (list (nth LINET PLIST)))
          LEN       (strlen (itoa LINET))
          NOS       (substr NOS (+ LEN 2))
    ) ;End setq
    (if NOS 
      (setq LINET (read NOS))
    ) ;End if
  ) ;End while
)

(defun XRMOD_OUT (/ CNT1 CNT2 FILE_HAND) 
  (setq LOC1 (get_tile "extract_path")
        CNT1 (CNT_TILE_ITEMS "extract_path")
        CNT2 (CNT_TILE_ITEMS "new_path")
        LOC2 (get_tile "new_path")
  ) ;End setq
  (cond 
    ((and (= CNT1 CNT2) (/= CNT1 0))
     (setq FILE_HAND (open MODE_FILE "w"))
     (write-line LOC1 FILE_HAND)
     (write-line LOC2 FILE_HAND)
     (close FILE_HAND)
     (MAKE_LISTS 1)
     (setq CONV_FROM_LIST CONV_LIST)
     (MAKE_LISTS 2)
     (setq CONV_TO_LIST CONV_LIST
           MOD          "Yes"
     ) ;End setq
     (done_dialog)
    ) ;End cond for verified
    ((= CNT1 0)
     (set_tile "error" "No path has has been indicated for extraction.")
    ) ;End cond for no extract path selection
    ((= CNT2 0)
     (set_tile "error" "No path has has been indicated for replacement.")
    ) ;End cond for no new path selection
    ((/= CNT1 CNT2)
     (set_tile 
       "error"
       "The number of extraction paths and replacement paths are not equal."
     )
    ) ;End cond for non equal number of paths
  ) ;End cond
)

(defun CLEAR_ERR (/ CNT1 CNT2 ERR) 
  (setq CNT1 (CNT_TILE_ITEMS "extract_path")
        CNT2 (CNT_TILE_ITEMS "new_path")
        ERR  (get_tile "error")
  ) ;End setq
  (cond 
    ((and (= CNT1 CNT2) (/= CNT1 0))
     (set_tile "error" "")
    ) ;End cond for verified
    ((and (/= CNT1 0) (= ERR "No path has has been indicated for extraction."))
     (set_tile "error" "")
    ) ;End cond for no extract path selection
    ((and (/= CNT2 0) (= ERR "No path has has been indicated for replacement."))
     (set_tile "error" "")
    ) ;End cond for no new path selection
    ((and 
       (= CNT1 CNT2)
       (= 
         ERR
         "The number of extraction paths and replacement paths are not equal."
       )
     )
     (set_tile "error" "")
    ) ;End cond for non equal number of paths
  ) ;End cond
)

(defun XRMOD (/ BLK CNTMODXR CONV_TO_INCRM LEN_EXTR_PATH NEW_PATH OLD_PATH 
              OLD_PATH_ASSOC PATH_EXTRACT PATH_NEW XL_INCRM XL_LEN XLIST X_NAME
             ) 
  (setq CNTMODXR       0
        CONV_TO_INCRM  0
        BLCK           (tblnext "block" T)
        OLD_PATH_ASSOC (assoc 1 BLCK)
        XLIST          '()
  ) ;End setq
  (while BLCK 
    (if OLD_PATH_ASSOC 
      (setq XLIST (append XLIST (list (cdr (assoc 2 BLCK)))))
    ) ;End if test to see if block is an xref
    (setq BLCK           (tblnext "block")
          OLD_PATH_ASSOC (assoc 1 BLCK)
    ) ;End setq
  ) ;End while blocks exist in drawing
  (setq XLIST (reverse XLIST))
  (foreach PATH_EXTRACT CONV_FROM_LIST 
    (setq LEN_EXTR_PATH (strlen PATH_EXTRACT)
          PATH_NEW      (nth CONV_TO_INCRM CONV_TO_LIST)
          CONV_TO_INCRM (1+ CONV_TO_INCRM)
    ) ;End setq
    (setq XL_LEN   (length XLIST)
          XL_INCRM 0
    ) ;End setq

    (while 
      (and 
        (not (>= XL_INCRM XL_LEN))
        (or (/= XL_LEN 0) (/= XL_LEN nil))
      ) ;End and
      (setq BLCK           (nth XL_INCRM XLIST)
            OLD_PATH_ASSOC (assoc 1 (tblsearch "block" BLCK))
            OLD_PATH       (cdr OLD_PATH_ASSOC)
            OLD_PATH_EXTR  (substr OLD_PATH 1 LEN_EXTR_PATH)
            XL_INCRM       (1+ XL_INCRM)
      ) ;End setq
      (if (equal (strcase PATH_EXTRACT) (strcase OLD_PATH_EXTR)) 
        (progn 
          (setq NEW_PATH (strcat PATH_NEW (substr OLD_PATH (1+ LEN_EXTR_PATH)))
                NEW_PATH (findfile NEW_PATH)
          ) ;End setq
          (if NEW_PATH 
            (progn 
              (command "_.-xref" "path" BLCK NEW_PATH)
              (setq CNTMODXR (1+ CNTMODXR))
            ) ;End progn
            (progn 
              (princ "\nPath not found for x-reference ")
              (princ BLCK)
            ) ;End else progn
          ) ;End if test for path
        ) ;End progn
      ) ;End if
    ) ;End while
  ) ;End foreach
  (print CNTMODXR)
  (princ "X-references have been modified")
)

(defun XRMODERR (S) 
  (if (/= S "Function cancelled") 
    (progn 
      (princ (strcat "\nError: " S))
      (princ "\nCannot resolve x-references made in an attatched drawing.")
      (princ "\nResolve any x-references in attatched drawings and try again.")
    ) ;End progn
  ) ;End if
  (setq *ERROR* OLDERR)
  (setvar "cmdecho" CMD)
  (princ)
)