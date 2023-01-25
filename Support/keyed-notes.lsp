;;; Copyright Lance A. Endres

(load "leader_base")
(load "le_utils")
(load "auto_layerset")

(defun C:KEYED-NOTES (/ ATTD ATTNO CMD CLAY CMD CNT DIMS FILE FILE_HAND INSPT LMBD1 
                      LMBD2 OSM PT1 PT2 TEMP1 TEMP2 VL-ADOC VL-COLLECT VL-ENT1
                     ) 
  (vl-load-com)
  (setq INSBLOCK (strcase INSBLOCK)
        LEADTYPE (strcase LEADTYPE)
        VL-ADOC  (vla-get-activedocument (vlax-get-acad-object))
        CLAY     (vla-getvariable VL-ADOC "clayer")
        DIMS     (vla-getvariable VL-ADOC "dimscale")
        ATTD     (vla-getvariable VL-ADOC "attdia")
        CMD      (vla-getvariable VL-ADOC "cmdecho")
        OSM      (vla-getvariable VL-ADOC "osmode")
        FILE     (findfile "keyed-notes.dat")
  ) ;_ End setq

  ;;; Ensure that the data file is found.
  (if (not FILE) 
    (progn 
      (alert "Data file not found.")
      (exit)
    ) ;_ End progn
    (setq FILE_HAND (open FILE "r"))
  ) ;_ End if

  ;;; Start undo mark and store system variables.
  (vla-startundomark VL-ADOC)
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "cmdecho" "attdia")
    (list 0 0)
  ) ;_ End mapcar

  ;;; If leaders are always to be placed on text layer, create layer if necessary and set text layer current.
  (AUTO_LAYERSET)

  ;;; Get data about block from the keyed-notes.dat file.
  (setq TEMP1 (GETNEWLINE FILE_HAND))
  (while (and TEMP1 (/= TEMP1 INSBLOCK)) 
    (setq TEMP1 (GETNEWLINE FILE_HAND))
  ) ;_ End while

  ;;; If block name is not found then exit.
  (if (null TEMP1) 
    (progn 
      (alert "Block name not found in data file.")
      (exit)
    ) ;_ End progn
  ) ;_ End if

  ;;; Get required points.
  (if (/= LEADTYPE "NONE") 
    (setq PT1   (getpoint "\nEndpoint of Leader ")
          INSPT (getpoint PT1 "\nInsertion Point of Symbol ")
          PT2   (polar 
                  INSPT
                  (angle INSPT (list (car PT1) (cadr INSPT)))
                  (* 
                    (vlax-variant-value DIMS)
                    (atof (GETNEWLINE FILE_HAND))
                  ) ;_ End *
                ) ;_ End polar
    ) ;_ End setq
    (progn 
      (setq INSPT (getpoint "\nInsertion Point of Symbol "))
      (GETNEWLINE FILE_HAND)
    ) ;_ End progn
  ) ;_ End if test for a leader

  ;;; Get point data and number of attributes.
  (setq ATTNO (atoi (GETNEWLINE FILE_HAND))
        CNT   0
  ) ;_ End setq
  (cond 
    ((null ATT_LIST)
     (repeat ATTNO 
       (setq ATT_LIST (append ATT_LIST (list "1")))
     ) ;_ End repeat
    )
    ((< (length ATT_LIST) ATTNO)
     (repeat (length ATT_LIST) 
       (setq ATT_LIST (append ATT_LIST (list "1")))
     ) ;_ End repeat
    )
  ) ;_ End cond

  ;;; Gather attribute information.
  (repeat ATTNO 
    (setq TEMP1 (getstring 
                  (strcat 
                    "\n"
                    (GETNEWLINE FILE_HAND)
                    " <"
                    (nth CNT ATT_LIST)
                    ">: "
                  ) ;_ End strcat
                ) ;_ End getstring
    ) ;_ End setq
    (if (or (= TEMP1 "") (null TEMP1)) 
      (setq TEMP2 (append TEMP2 (list (nth CNT ATT_LIST))))
      (setq TEMP2 (append TEMP2 (list TEMP1)))
    ) ;_ End if
    (setq CNT (1+ CNT))
  ) ;_ End repeat
  (close FILE_HAND)
  (setq ATT_LIST TEMP2)

  ;;; Get correct collection of entities.
  (if (= (vla-get-activespace VL-ADOC) 1) 
    (setq VL-COLLECT (vla-get-modelspace VL-ADOC))
    (setq VL-COLLECT (vla-get-paperspace VL-ADOC))
  ) ;_ End if

  ;;; If the block is not in this drawing, then find the file for use with the insertblock method.
  (setq TEMP2 NIL)
  (vlax-for TEMP1 (vla-get-blocks VL-ADOC) 
    (if (= (strcase (vla-get-name TEMP1)) INSBLOCK) 
      (setq TEMP2 t)
    ) ;_ End if
  ) ;_ End vlax-for
  (if (/= TEMP2 t) 
    (setq INSBLOCK (findfile (strcat INSBLOCK ".dwg")))
  ) ;_ End if
  (if (null INSBLOCK) 
    (progn 
      (alert "Block not found.")
      (exit)
    ) ;_ End progn
  ) ;_ End if

  ;;; Insert block.

  (setq VL-ENT1 (vla-insertblock 
                  VL-COLLECT
                  (vlax-3d-point INSPT)
                  INSBLOCK
                  DIMS
                  DIMS
                  DIMS
                  0
                ) ;_ End vla-insertblock
  ) ;_ End setq

  ;;; Extract the name of the block in case the block was not previously defined within the
  ;;; drawing.  This will prevent the block from being reinserted.
  (setq INSBLOCK (nth 1 (fnsplitl INSBLOCK)))

  ;;; If the block has attributes set them to the entered values.
  (if (= (vla-get-hasattributes VL-ENT1) :vlax-true) 
    (mapcar 
      '(lambda (LMBD1 LMBD2) 
         (vla-put-textstring 
           LMBD1
           LMBD2
         ) ;_ End vla-put-textstring
       ) ;_ End lambda
      (vlax-safearray->list 
        (vlax-variant-value 
          (vla-getattributes VL-ENT1)
        ) ;_ End vlax-variant-value
      ) ;_ End vlax-safearray->list
      ATT_LIST
    ) ;_ End mapcar
  ) ;_ End if

  ;;; Create leader.

  (if (/= LEADTYPE "NONE") 
    (LEADER_BASE LEADTYPE PT1 PT2)
  ) ;_ End if

  ;;; Restore settings.

  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "clayer" "attdia" "cmdecho" "osmode")
    (list CLAY ATTD CMD OSM)
  ) ;_ End mapcar
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun

(defun *ERROR* (MSG / LMBD1 LMBD2 VL-ADOC) 
  (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "clayer" "attdia" "cmdecho" "osmode")
    (list CLAY ATTD CMD OSM)
  ) ;_ End mapcar
  (vla-endundomark VL-ADOC)
  (princ)
) ;_ End defun