;;; Copyright Lance A. Endres

(load "versorttext")
(load "linespace")
(load "ai_utils")

(defun C:NUMBER (/ ADOC CMD CNT CNT2 DIST ENT1 ENT2 ENT1DEF FHGHT FORMAT FRMMTXT HGHT 
                 LINE_SPAC LMBD1 LMBD2 LOOP1 OFFDIST OSM MTXT PASS PNTLST SS1 SS2 SST 
                 SSMT SS1LEN TEMP TEMP1 TLAY TSTYLE XPT YPT1 YPT2
                ) 
  (vl-load-com)
  (setq ADOC  (vla-get-activedocument (vlax-get-acad-object))
        OSM   (vla-getvariable ADOC "osmode")
        CMD   (vla-getvariable ADOC "cmdecho")
        LOOP1 "True"
        PASS  1
  ) ;_ End setq
  (vla-startundomark ADOC)
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable ADOC LMBD1 LMBD2))
    (list "cmdecho" "osmode")
    (list 0 0)
  ) ;_ End mapcar

  ;;; Get selection set
  (princ "\nSelect text to be numbered.")
  (setq SS1 (ssget)
        SST (ssget "P" (list '(0 . "TEXT")))
  ) ;_ End setq
  (command "select" SS1 "")
  (setq SSMT (ssget "P" (list '(0 . "MTEXT"))))

  ;;; Check for selection set
  (cond 
    ((and (null SST) (null SSMT))
     (princ "\nNo text objects selected.")
     (exit)
    ) ;_ End if
    ((null SST)
     (setq SST (ssadd))
    )
  ) ;_ End cond

  ;;; Get layer of first text object.
  (if (> (sslength SST) 0) 
    (setq SST  (VERSORTTEXT SST)
          YPT1 (caddr 
                 (assoc 
                   10
                   (entget 
                     (ssname SST 0)
                   ) ;_ End entget
                 ) ;_ End assoc for insertion point
               ) ;_ End caddr
    ) ;_ End setq
  ) ;_ End if
  (if SSMT 
    (setq SSMT (VERSORTTEXT SSMT)
          YPT2 (caddr 
                 (assoc 
                   10
                   (entget 
                     (ssname SSMT 0)
                   ) ;_ End entget
                 ) ;_ End assoc for insertion point
               ) ;_ End caddr
    ) ;_ End setq
  ) ;_ End if
  (if (> YPT1 YPT2) 
    (setq TLAY (cdr (assoc 8 (entget (ssname SST 0)))))
    (setq TLAY (cdr (assoc 8 (entget (ssname SSMT 0)))))
  ) ;_ End if

  ;;; Get offset distance.
  ;;; Set default number to start counting from.
  (setq OFFDIST (getdist "\nOffset distance: ")
        CNT2    1
  ) ;_ End setq

  ;;; Format to use, number or use a construction note block
  (initget "Number Block Start")
  (setq FORMAT (getkword 
                 "\nNumbering format to use [Block / Start] <Number>: "
               ) ;_ End getkword
  ) ;_ End setq
  (while (= FORMAT "Start") 
    (initget 1)
    (princ (strcat "\nCount start number = " (itoa CNT2)))
    (setq CNT2 (getint "\nNumber to start counting from: "))
    (initget "Number Block Start")
    (setq FORMAT (getkword 
                   "\nNumbering format to use [Block / Start] <Number>: "
                 ) ;_ End getkword
    ) ;_ End setq
  ) ;_ End while
  (if (= FORMAT "Block") 
    (progn 
      (setq FORMAT (GETINS))
      (if (= FORMAT "Cancel") 
        (exit)
      ) ;_ End if
    ) ;_ End progn
    (setq FORMAT "Number")
  ) ;_ End if

  ;;; Convert mtext to text to extract ins points
  (if SSMT 
    (progn 
      (setq SSMT    (EXTRACT_NAMES SSMT)
            FRMMTXT (ssadd)
      ) ;_ End setq
      (foreach ENT1 SSMT 
        (command "_.copy" ENT1 "" "0,0" "@" "_.explode" "last")
        (setq TEMP1 (ssget "p")
              TEMP1 (EXTRACT_NAMES TEMP1)
        ) ;_ End setq
        (foreach ENT2 TEMP1 
          (ssadd ENT2 SST)
          (ssadd ENT2 FRMMTXT)
        ) ;_ End foreach
      ) ;_ End foreach
    ) ;_ End progn
  ) ;_ End if

  ;;; Eliminate text that is only spaces.
  (setq CNT 0)
  (repeat (sslength SST) 
    (setq ENT1 (ssname SST CNT))
    (if (= (AI_STRTRIM (cdr (assoc 1 (entget ENT1)))) "") 
      (progn 
        (vla-delete (vlax-ename->vla-object ENT1))
        (setq SST (ssdel ENT1 SST))
      ) ;_ End progn
      (setq CNT (1+ CNT))
    ) ;_ End if
  ) ;_ End repeat

  ;;; Sort text.
  (setq SS2 (VERSORTTEXT SST))

  ;;; Extract insertion points.
  (setq CNT     0
        ENT1DEF (entget (ssname SS2 0))
        HGHT    (/ (cdr (assoc 40 ENT1DEF)) 2)
  ) ;_ End setq
  (repeat (sslength SS2) 
    (setq TEMP (caddr 
                 (assoc 
                   10
                   (entget 
                     (ssname SS2 CNT)
                   ) ;_ End entget
                 ) ;_ End assoc for insertion point
               ) ;_ End caddr for extracting y point
          CNT  (1+ CNT) ;_ increment counter
    ) ;_ End setq
    (if (= FORMAT "Number") 
      (setq PNTLST (append 
                     (list TEMP) ;_ End list for converting y point to list for append
                     PNTLST
                   ) ;_ End append y points to a list
      ) ;_ End setq
      (setq PNTLST (append 
                     (list (+ TEMP HGHT)) ;_ End list for converting y point to list for append
                     PNTLST
                   ) ;_ End append y points to a list
      ) ;_ End setq
    ) ;_ End if
  ) ;_ End repeat
  (setq PNTLST (reverse PNTLST)
        XPT    (- (cadr (assoc 10 ENT1DEF)) OFFDIST)
        HGHT   (cdr (assoc 40 ENT1DEF))
        TSTYLE (cdr (assoc 7 ENT1DEF))
        FHGHT  (cdr 
                 (assoc 
                   40
                   (tblsearch 
                     "style"
                     (cdr (assoc 7 ENT1DEF))
                   ) ;_ End tblsearch
                 ) ;_ End assoc
               ) ;_ End cdr
  ) ;_ End setq

  ;;; Check for styles that have the font height set to zero.
  (if (= FHGHT 0) 
    (setq FHGHT HGHT)
  ) ;_ End if

  ;;; Set line spacing based on the scale of the text in relation to the scale of the font style.
  (setq LINE_SPAC (* (LINESPACE (ssname SS2 0)) (/ HGHT FHGHT)))

  ;;; Erase items that came from mtext
  (command "_.erase" FRMMTXT "")

  ;;; Number points
  (setq CNT 0) ;_ End setq
  (NUMBERPOINT CNT CNT2 FORMAT HGHT PNTLST TLAY TSTYLE XPT)
  (repeat (- (sslength SS2) 1) 
    (setq CNT  (1+ CNT)
          DIST (- (nth (1- CNT) PNTLST) (nth CNT PNTLST))
    ) ;_ End setq
    (if (> DIST (* LINE_SPAC 1.25)) 
      (progn 
        (setq CNT2 (1+ CNT2))
        (NUMBERPOINT CNT CNT2 FORMAT HGHT PNTLST TLAY TSTYLE XPT) ;_ End NUMBERPOINT
      ) ;_ End progn
    ) ;_ End if
  ) ;_ End repeat

  ;;; Restore system variables and exit.
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable ADOC LMBD1 LMBD2))
    (list "osmode" "cmdecho")
    (list OSM CMD)
  ) ;_ End mapcar
  (vla-endundomark ADOC)
  (princ)
) ;_ End defun

(defun NUMBERPOINT (CNT CNT2 FORMAT HGHT PNTLST TLAY TSTYLE XPT / COLLECT VL-ADOC 
                    VL-ENT1
                   ) 
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))

  ;;; Get correct collection of entities.
  (if (= (vla-get-activespace VL-ADOC) 1) 
    (setq COLLECT (vla-get-modelspace VL-ADOC))
    (setq COLLECT (vla-get-paperspace VL-ADOC))
  ) ;_ End if

  (if (= FORMAT "Number") 
    (progn 

      ;;; Add text.
      (setq VL-ENT1 (vla-addtext 
                      COLLECT
                      (strcat (itoa CNT2) ".")
                      (vlax-3d-point 
                        (list XPT (nth CNT PNTLST) 0)
                      ) ;_ End vlax-3d-point
                      HGHT
                    ) ;_ End vla-addtext
      ) ;_ End setq

      ;;; Set the style.
      (vla-put-stylename VL-ENT1 TSTYLE)
    ) ;_ End progn
    (progn 

      ;;; Add block.
      (setq VL-ENT1 (vla-insertblock 
                      COLLECT
                      (vlax-3d-point 
                        (list XPT (nth CNT PNTLST) 0)
                      ) ;_ End vlax-3d-point
                      FORMAT
                      BLKSCL
                      BLKSCL
                      BLKSCL
                      0
                    ) ;_ End vla-insertblock
      ) ;_ End setq

      ;;; Check to make sure block has attributes.
      (if (/= (vla-get-hasattributes VL-ENT1) :vlax-true) 
        (progn 
          (vla-delete VL-ENT1)
          (princ "\nThis block does not contain attributes.")
          (exit)
        ) ;_ End progn
      ) ;_ End if

      ;;; Set the attribute.
      (vla-put-textstring 
        (nth 
          0
          (vlax-safearray->list 
            (vlax-variant-value (vla-getattributes VL-ENT1))
          ) ;_ End vlax-safearray->list
        ) ;_ End nth
        (itoa CNT2)
      ) ;_ End vla-put-textstring
    ) ;_ End progn
  ) ;_ End if

  ;;; Set the layer for either text or block.
  (vla-put-layer VL-ENT1 TLAY)
) ;_ End defun

(defun EXTRACT_NAMES (SS1 / CNT SS2) 
  (setq CNT        0
        NUMB_BLOCK NIL
  ) ;_ End setq
  (while (< CNT (sslength SS1)) 
    (setq SS2 (append SS2 (list (ssname SS1 CNT)))
          CNT (1+ CNT)
    ) ;_ End setq
  ) ;_ End while
  (setq SS1 SS2)
) ;_ End defun

(defun GETINS (/ DCL_ID) 
  (setq DCL_ID (load_dialog "number.dcl"))

  ;;; Load dialog.
  (if (not (new_dialog "NMBRINS" DCL_ID)) 
    (exit)
  ) ;_ End if test to insure dialog box loaded

  ;;; Create a list of all blocks in the drawing.
  (CREATE_BLOCK_LIST)
  (RESET_LIST_BOX)

  ;;; Initiate tiles.
  (mapcar 'set_tile (list "path" "dimsc") (list "" "1"))
  (TOGGLE_TILES (get_tile "dimsc"))
  (mapcar 
    'action_tile
    (list "name_list" "dimsc" "user_dimsc" "browse" "accept")
    (list "(CLEAR_PATH $value)" "(TOGGLE_TILES $value)" "(DWG_SCALE_CHECK $value)" 
          "(BRWSINS)" "(GETINSOUT)"
    ) ;_ End list
  ) ;_ End mapcar
  (start_dialog)
  (unload_dialog DCL_ID)
  (if NUMB_BLOCK 
    NUMB_BLOCK
    "Cancel"
  ) ;_ End if
) ;_ End defun

;;; Function to clear the path if the name list is changed after a file has been browsed for.
(defun CLEAR_PATH (TEMP) 
  (if (and (/= TEMP "0") (/= (get_tile "path") "")) 
    (progn 
      (set_tile "path" "")
      (CREATE_BLOCK_LIST)
      (RESET_LIST_BOX)
      (set_tile "name_list" (itoa (1- (atoi TEMP))))
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun

;;; Toggle manual dimscale edit box based on if the drawing dimscale is to be used.
(defun TOGGLE_TILES (MODE) 
  (if (= MODE "1") 
    (progn 
      (mode_tile "user_dimsc" 1)
      (set_tile "error" "")
    ) ;_ End progn
    (mode_tile "user_dimsc" 0)
  ) ;_ End if
) ;_ End defun

(defun DWG_SCALE_CHECK (TEMP) 
  (setq TEMP (distof TEMP))
  (if (> TEMP 0) 
    (progn 
      (set_tile "user_dimsc" (AI_RTOS TEMP))
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

;;; Browse for an external block
(defun BRWSINS () 
  (CREATE_BLOCK_LIST)

  ;;; If there is a path saved from the last browse, use it.
  ;;; Other wise set the lastpath to a null string to prevent an error.
  (if (not LASTPATH) 
    (setq LASTPATH "")
  ) ;_ End if
  (setq FFILE (getfiled "Select a file to insert" LASTPATH "dwg" 0))
  (if FFILE 
    (progn 
      (set_tile "path" (nth 0 (fnsplitl FFILE)))
      (setq LASTPATH   (nth 0 (fnsplitl FFILE))
            BLOCK_LIST (append 
                         (list (nth 1 (fnsplitl FFILE)))
                         BLOCK_LIST
                       ) ;_ End append
      ) ;_ End setq
      (RESET_LIST_BOX)
      (set_tile "name_list" "0")
    ) ;_ End progn
    (progn 
      (setq FFILE NIL)
      (set_tile "path" "")
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun

(defun RESET_LIST_BOX () 
  (start_list "name_list")
  (mapcar 'add_list BLOCK_LIST)
  (end_list)
) ;_ End defun

(defun CREATE_BLOCK_LIST (/ BLKDEF TEMP) 
  (if 
    (setq TEMP   (cdr (assoc 2 (tblnext "block" t)))
          BLKDEF (entget (tblobjname "block" TEMP))
    ) ;_ End setq
    (if 
      (and 
        (= (logand 2 (cdr (assoc 70 BLKDEF))) 2)
        (/= (logand 32 (cdr (assoc 70 BLKDEF))) 32)
      ) ;_ End and
      (setq BLOCK_LIST (list TEMP))
      (setq BLOCK_LIST NIL)
    ) ;_ End if
  ) ;_ End if
  (while (setq TEMP (cdr (assoc 2 (tblnext "block")))) 
    (setq BLKDEF (entget (tblobjname "block" TEMP)))
    (if 
      (and 
        (= (logand 2 (cdr (assoc 70 BLKDEF))) 2)
        (/= (logand 32 (cdr (assoc 70 BLKDEF))) 32)
      ) ;_ End and
      (setq BLOCK_LIST (append 
                         BLOCK_LIST
                         (list TEMP)
                       ) ;_ End append
      ) ;_ End setq
    ) ;_ End if
  ) ;_ End while
) ;_ End defun

(defun GETINSOUT () 
  (setq NUMB_BLOCK (nth (atoi (get_tile "name_list")) BLOCK_LIST))
  (if (= (get_tile "user_dimsc") "1") 
    (DWG_SCALE_CHECK (get_tile "user_dimsc"))
  ) ;_ End if
  (cond 
    ((not BLOCK_LIST)
     (set_tile "error" "Block not selected.")
    )
    ((/= (get_tile "error") ""))
    (t
     (if FFILE 
       (setq NUMB_BLOCK FFILE)
     ) ;_ End if
     (if (= (get_tile "dimsc") "1") 
       (setq BLKSCL (getvar "dimscale"))
       (setq BLKSCL (distof (get_tile "user_dimsc")))
     ) ;_ End if
     (done_dialog)
    )
  ) ;_ End cond
) ;_ End defun

(defun *ERROR* (MSG / ADOC LMBD1 LMBD2) 
  (setq ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable ADOC LMBD1 LMBD2))
    (list "osmode" "cmdecho")
    (list OSM CMD)
  ) ;_ End mapcar

  ;;; Erase items that came from mtext

  (if FRMMTXT 
    (command "_.erase" FRMMTXT "")
  ) ;_ End if

  (vla-endundomark ADOC)
  (princ)
) ;_ End defun