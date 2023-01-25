;;; Copyright Lance A. Endres

;|
   1) Set a way to change the plot style in the font section
   2) Test for a valid plot style
   3) Test for a valid font
   4) Test the entire program without an override
|;

(defun C:PIPELABEL (/ ADOC CMD CSTYL DIMS LUPRC LSTTXT LTXTROT OPTION OSM PSFLEN 
                    PSTYL PSTYLFNT
                   ) 
  (setq OLDERR   *ERROR*
        *ERROR*  PIPELABELERR
        ADOC     (vla-get-activedocument (vlax-get-acad-object))
        ORTH     (getvar "orthomode")
        OSM      (getvar "osmode")
        CMD      (getvar "cmdecho")
        LSTTXT   (getcfg "AppData/PipeLabel/LstTxt")
        COL      (getcfg "AppData/PipeLabel/Col")
        FNT      (getcfg "AppData/PipeLabel/Fnt")
        HGHT     (getcfg "AppData/PipeLabel/Hght")
        WDTH     (getcfg "AppData/PipeLabel/Wdth")
        PLTSTYL  (getcfg "AppData/PipeLabel/PltStyl")
        CSTYL    (getvar "textstyle")
        OPTION   t
        OVERRIDE (findfile "ploverride.txt")
        PSTYL    (tblsearch "style" "pipelabeling")
        LUPRC    (getvar "luprec")
        DIMS     (getvar "dimscale")
  ) ;_ End setq
  (vla-startundomark ADOC)
  (if PSTYL 
    (setq PSTYLFNT (cdr (assoc 3 PSTYL))
          PSFLEN   (strlen PSTYLFNT)
    ) ;_ End setq
  ) ;_ End if for getting style information if style exists
  (setvar "cmdecho" 0)
  (command "undo" "be")
  (if (or (null LSTTXT) (= LSTTXT "")) 
    (progn 
      (setq LSTTXT (getstring "\nPiping label: "))
      (setcfg "AppData/PipeLabel/LstTxt" LSTTXT)
    ) ;_ End progn
  ) ;_ End if for setting label if not present
  (setvar "luprec" 8)
  (if OVERRIDE 
    (progn 
      (setq OVERRIDE (open OVERRIDE "r")
            COL      (GETNEWLINE OVERRIDE)
            FNT      (GETNEWLINE OVERRIDE)
            HGHT     (rtos (distof (GETNEWLINE OVERRIDE)))
            WDTH     (GETNEWLINE OVERRIDE)
            PLTSTYL  (GETNEWLINE OVERRIDE)
      ) ;_ End setq
      (close OVERRIDE)
    ) ;_ End progn
    (progn 
      (if (or (null COL) (= COL "")) 
        (progn 
          (CHANGECOLOR)
        ) ;_ End progn
      ) ;_ End if for setting color if not present
      (if (or (null FNT) (= FNT "")) 
        (progn 
          (GETFONT)
        ) ;_ End progn
      ) ;_ End if
      (if (or (null HGHT) (= HGHT "")) 
        (progn 
          (setq HGHT (rtos (getreal "\nPlotted text height: "))) ;_ End setq
        ) ;_ End progn
      ) ;_ End if for setting text height if not present
      (if (or (null WDTH) (= WDTH "")) 
        (progn 
          (setq WDTH (getstring "\nWidth factor: "))
          (TESTWDTH)
        ) ;_ End progn
      ) ;_ End if
      (if 
        (and 
          (or 
            (null PLTSTYL)
            (= PLTSTYL "")
            ;|(not (TESTPLOTSTYLE))|;
          ) ;_ End or
          (= (getvar "pstylemode") 0)
        ) ;_ End and
        (progn 
          (setq PLTSTYL (getstring "\nPlot style: "))

          ;;;		    (while (not (TESTPLOTSTYLE))
          ;;;			(princ "\nNot a valid plot style: ")
          ;;;			(setq PLTSTYLE (getstring "\nPlot style: "))
          ;;;			)
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End progn
  ) ;_ End if
  (if 
    (or 
      (= PSTYL NIL)
      (and 
        (/= FNT PSTYLFNT)
        (/= FNT (substr PSTYLFNT 1 (- PSFLEN 4)))
      ) ;_ End and test for font
      (/= (* (distof HGHT) DIMS) (cdr (assoc 40 PSTYL)))
      (/= (distof WDTH) (cdr (assoc 41 PSTYL)))
    ) ;_ End or test
    (progn 
      (CREATESTYLE)
      (setvar "textstyle" CSTYL)
    ) ;_ End progn
  ) ;_ End if for testing if it is necessary to create font
  (setvar "luprec" LUPRC)
  (vla-endundomark ADOC)
  (princ "\nPiping label: ")
  (princ LSTTXT)
  (while (and OPTION (/= OPTION "Exit")) 
    (vla-startundomark ADOC)
    (setvar "osmode" 512)
    (initget "Exit Text Font")
    (setq OPTION (getpoint 
                   "\nSelect piping label location or [set Text/set Font options] <Exit>: "
                 ) ;_ End getpoint
    ) ;_ End setq
    (cond 
      ((= OPTION "Text")
       (princ "\nPiping label: ")
       (princ LSTTXT)
       (setq LSTTXT (getstring "\nNew piping label: "))
       (setcfg "AppData/PipeLabel/LstTxt" LSTTXT)
      ) ;_ End cond for changing text
      ((= OPTION "Font")
       (SETFONT)
      ) ;_ End cond for font option
      ((and OPTION (/= OPTION "Exit"))
       (LINETEST LSTTXT OPTION)
      ) ;_ End cond for a picked point
    ) ;_ End cond
    (setvar "osmode" OSM)
    (vla-endundomark ADOC)
  ) ;_ End while
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun

(defun GETFONT () 
  (setq FNT (getstring "\nFont for pipe labels: "))
  (setcfg "AppData/Pipelabel/Fnt" FNT)
) ;_ End defun

(defun TESTWDTH () 
  (while (not (and (> (atof WDTH) 0) (<= (atof WDTH) 1.0))) 
    (princ "\nWidth factor must be between 0 and 1")
    (setq WDTH (getstring "\nWidth factor: "))
  ) ;_ End while
  (setcfg "AppData/PipeLabel/Wdth" WDTH)
) ;_ End defun

(defun SETFONT (/ LUPRC OPTION) 
  (setq OPTION  t
        *ERROR* PIPELABELERR
        LUPRC   (getvar "luprec")
  ) ;_ End setq
  (command "undo" "be")
  (setvar "luprec" 8)
  (while (and OPTION (/= OPTION "Exit")) 
    (princ "\nColor = ")
    (princ COL)
    (princ "     Font = ")
    (princ FNT)
    (princ "     Height = ")
    (princ HGHT)
    (princ "     Width = ")
    (princ WDTH)
    (initget "Color Font Height Width Exit")
    (setq OPTION (getkword 
                   "\n[set Color/set Font/set Height/set Width factor] <Exit>: "
                 ) ;_ End getkword
    ) ;_ End setq
    (cond 
      ((= OPTION "Color")
       (CHANGECOLOR)
      ) ;_ End cond for changing color
      ((= OPTION "Font")
       (GETFONT)
      ) ;_ End cond for changing font
      ((= OPTION "Height")
       (setq HGHT (rtos (getreal "\nNew plotted text height: "))) ;_ End setq
       (setcfg "AppData/PipeLabel/Hght" HGHT)
      ) ;_ End cond for changing height
      ((= OPTION "Width")
       (setq WDTH (getstring "\nNew width factor: "))
       (TESTWDTH)
      ) ;_ End cond for changing width
    ) ;_ End cond
  ) ;_ End while
  (setvar "luprec" LUPRC)
  (CREATESTYLE)
  (command "undo" "e")
) ;_ End defun

(defun CREATESTYLE (/ CMDTST) 
  (command 
    "_.-style"
    "pipelabeling"
    FNT
    (* (getvar "dimscale") (distof HGHT))
    WDTH
  ) ;_ End command
  (setq CMDTST (getvar "cmdactive"))
  (while (= CMDTST 1) 
    (command "")
    (setq CMDTST (getvar "cmdactive"))
  ) ;_ End while
) ;_ End defun

(defun CHANGECOLOR (/ TEST) 
  (while (null TEST) 
    (setq COL (strcase (getstring "\nNew color for piping label: "))) ;_ End setq
    (if (numberp (read COL)) 
      (setq TEST 1)
      (setq TEST 2)
    ) ;_ End if
    (if (= TEST 1) 
      (if (and (> (read COL) 0) (< (read COL) 250)) 
        (setq TEST "Pass")
        (progn 
          (setq TEST NIL)
          (princ "\nNot a valid color")
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End if
    (if (= TEST 2) 
      (if 
        (or 
          (= COL "RED")
          (= COL "YELLOW")
          (= COL "GREEN")
          (= COL "CYAN")
          (= COL "BLUE")
          (= COL "CYAN")
          (= COL "MAGENTA")
          (= COL "WHITE")
        ) ;_ End or
        (setq TEST "Pass")
        (progn 
          (setq TEST NIL)
          (princ "Not a valid color")
        ) ;_ End progn
      ) ;_ End if
    ) ;_ End if
  ) ;_ End while for color loop
  (setcfg "AppData/PipeLabel/Col" COL)
) ;_ End defun

(defun LINETEST (LSTTXT PT1 / ENT1 ENT1DEF) 
  (setq ENT1 (ssget PT1))
  (if ENT1 
    (progn 
      (setq ENT1     (ssname ENT1 0)
            ENT1DEF  (entget ENT1)
            ENT1TYPE (cdr (assoc 0 ENT1DEF))
      ) ;_ End setq
      (if (= ENT1TYPE "LINE") 
        (INSERTLABEL LSTTXT ENT1 PT1)
        (princ "\nEntity selected was not a line")
      ) ;_ End if
    ) ;_ End progn
    (princ "\nNo entity was selected")
  ) ;_ End if
) ;_ End defun

(defun INSERTLABEL (LSTTXT ENT1 PT1 / ANGL COLTMP COLLECT DIMS ENT1DEF ENT1LAY 
                    NEWASSOC OLDASSOC PTB PTE PTT TRMENTPT XPTB XPTE YPTB YPTE
                   ) 
  (setq ENT1DEF  (entget ENT1)
        TRMENTPT (list ENT1 PT1)
        ENT1LAY  (cdr (assoc 8 ENT1DEF))
        PTB      (cdr (assoc 10 ENT1DEF))
        PTE      (cdr (assoc 11 ENT1DEF))
        XPTB     (car PTB)
        XPTE     (car PTE)
        YPTB     (cadr PTB)
        YPTE     (cadr PTE)
        CSHGHT   (cdr (assoc 40 (tblsearch "style" (getvar "textstyle"))))
        DIMS     (getvar "dimscale")
  ) ;_ End setq
  (if 
    (or 
      (if (not (equal XPTB XPTE 0.001)) 
        (> XPTB XPTE)
      ) ;_ End if
      (and (equal XPTB XPTE 0.001) (> YPTB YPTE))
    ) ;_ End or
    (setq PTT PTB
          PTB PTE
          PTE PTT
    ) ;_ End setq
  ) ;_ End if
  (cond 
    ((= COL "RED")
     (setq COLTMP 1)
    ) ;_ End cond for red
    ((= COL "YELLOW")
     (setq COLTMP 2)
    ) ;_ End cond for yellow
    ((= COL "GREEN")
     (setq COLTMP 3)
    ) ;_ End cond for green
    ((= COL "CYAN")
     (setq COLTMP 4)
    ) ;_ End cond for cyan
    ((= COL "BLUE")
     (setq COLTMP 5)
    ) ;_ End cond for blue
    ((= COL "MAGENTA")
     (setq COLTMP 6)
    ) ;_ End cond for magenta
    ((= COL "WHITE")
     (setq COLTMP 7)
    ) ;_ End cond for white
    ((numberp COL)
     (setq COLTMP COL)
    ) ;_ End cond for a number
  ) ;_ End cond
  (setq ANGL (angle PTB PTE))

  ;;; Get correct collection of entities
  (if 
    (= 
      (vla-get-activespace 
        (vla-get-activedocument (vlax-get-acad-object))
      ) ;_ End vla-get-activespace
      1
    ) ;_ End =
    (setq COLLECT (vla-get-modelspace 
                    (vla-get-activedocument (vlax-get-acad-object))
                  ) ;_ End vla-get-modelspace
    ) ;_ End setq
    (setq COLLECT (vla-get-paperspace 
                    (vla-get-activedocument (vlax-get-acad-object))
                  ) ;_ End vla-get-paperspace
    ) ;_ End setq
  ) ;_ End if

  ;;; Add text.
  (setq ENT1 (vla-addtext 
               COLLECT
               LSTTXT
               (vlax-3d-point PT1)
               (* (distof HGHT) DIMS)
             ) ;_ End vla-addtext
  ) ;_ End setq

  ;;; Move justification.
  (vla-put-alignment ENT1 acalignmentmiddle)

  ;;; Move insertion point to correct location.
  (vla-put-textalignmentpoint ENT1 (vlax-3d-point PT1))

  ;;; Set the color.
  (vla-put-color ENT1 COLTMP)

  ;;; Set the rotation.
  (vla-put-rotation ENT1 ANGL)

  ;;; Set the layer.
  (vla-put-layer ENT1 ENT1LAY)

  ;;; Set the style.
  (vla-put-stylename ENT1 "pipelabeling")

  ;;; If drawing is a named plot style drawing set the plotstylename.
  (if (= (getvar "pstylemode") 0) 
    (vla-put-plotstylename ENT1 "Text")
  ) ;_ End if
  (setvar "osmode" 0)
  (command 
    "_.trim"
    (vlax-vla-object->ename ENT1)
    ""
    TRMENTPT
    ""
  ) ;_ End command
) ;_ End defun

(defun PIPELABELERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setvar "textstyle" CSTYL)
  (setvar "osmode" OSM)
  (setvar "luprec" LUPRC)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun