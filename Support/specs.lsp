;;; The following program is a modified version of ASCTEXT.LSP provided by
;;; Autodesk, Inc.  The program has been altered to work specifically for
;;; inserting specifications into a drawing that have been created from a
;;; program call Speclink.  The files are exported out of Speclink into a rtf
;;; format and are then converted into asc text.  This modified program uses
;;; predefined knowledge about our process to spead it further.

;;; --------------------------------------------------------------------------;
;;;   ASCTEXT.LSP
;;;   Copyright (C) 1986-1992 by Autodesk, Inc.
;;; --------------------------------------------------------------------------;


;;;
;;; --------: GLOBAL variables
;;;  at_ang    : Last angle (for default value)
;;;  at_fnm    : Last file  (for default value)
;;;  rtfile : Read File
;;;  ang    : Rotation angle
;;;  c      : Total line count
;;;  cd     : Column distance
;;;  d      : Distance between lines
;;;  eof    : End of file flag
;;;  l1     : First line to read
;;;  h      : Text height
;;;  j      : Text justification
;;;  lc     : Column line count
;;;  n      : Number of lines to read
;;;  nl     : Number of lines per column
;;;  opt    : Options list
;;;  pt     : Text insertion point
;;;  pt1    : First text insertion point
;;;  rf     : File to read
;;;  s      : Text string
;;;  ts     : Text style list
;;;  ul     : Upper/lower case flag
;;;
;;;---------------------------------------------------------------------------;

;;;
;;; Save modes
;;;
(defun MODES (a) 
  (setq MLST '
        ()
  )
  (repeat (length a) 
    (setq MLST (append MLST (list (list (car a) (getvar (car a))))))
    (setq a (cdr a))
  )
) 
;;;
;;; Restore modes
;;;
(defun MODER () 
  (repeat (length MLST) 
    (setvar (caar MLST) (cadar MLST))
    (setq MLST (cdr MLST))
  )
) 
;;;
;;; Ascii Text error handler
;;;
(defun at_err (st)  ; If an error (such as CTRL-C) occurs
  ; while this command is active...
  (if (/= st "Function cancelled") 
    (princ (strcat "\nError: " st))
  )
  (moder) ; Restore modified modes
  (if (= (type rtfile) 'FILE) 
    (close rtfile)
  )
  (setq rtfile nil)
  (setq *error* olderr) ; Restore old *error* handler
  (princ)
) 
;;;
;;; Function for inserting text a recalculation of insertion point.
;;;
(defun 1LTXT () 
  (if (member '1 opt) 
    (setq s (strcat "%%u" s "%%u"))
  )
  (if (member '2 opt) 
    (setq s (strcat "%%o" s "%%o"))
  )
  (if (member '4 opt) 
    (setq s (strcase s))
  )
  (if (member '8 opt) 
    (setq s (strcase s T))
  )
  (if (and (= lc (1+ nl)) (/= nl 0)) 
    (if (= cntcol noofcol) 
      (setq pt     (getpoint "\nNew start point: ")
            lc     1
            cntcol 1
            pt1    pt
      ) ;End setq
      (setq lc     1
            cntcol (1+ cntcol)
            pt     (polar pt1 ang cd)
            pt1    pt
      ) ;End setq
    )
  )
  (cond 
    ((and (= j "Aligned") h) (command "TEXT" j pt pt2 s))
    ((and (= j "Aligned") (null h)) (command "TEXT" j pt pt2 s))
    ((and (= j "Fit") h) (command "TEXT" j pt pt2 h s))
    ((and (= j "Fit") (null h)) (command "TEXT" j pt pt2 s))
    ((and (= j "L") h) (command "TEXT" pt h (rtd ang) s))
    ((and (= j "L") (null h)) (command "TEXT" pt (rtd ang) s))
    ((and (/= j "L") h) (command "TEXT" j pt h (rtd ang) s))
    ((and (/= j "L") (null h)) (command "TEXT" j pt (rtd ang) s))
  )
  (if (/= d "Auto") 
    (if (= (cdr (assoc 70 ts)) 4) 
      (setq pt (polar pt (+ (dtr 90) ang) d))
      (setq pt (polar pt (+ (dtr 270) ang) d))
    )
  )
  (setq c (1+ c))
  (if (= c n) 
    (setq eof T)
  )
) 
;;;
;;; Degrees to radians conversion
;;;
(defun DTR (y) 
  (* pi (/ y 180.0))
) 
;;;
;;; Radians to degrees conversion
;;;
(defun RTD (Y) 
  (* 180.0 (/ y pi))
) 
;;;
;;; List the options.
;;;
(defun justpn () 
  (if (getvar "DIMCLRD") (textpage))
  (princ "\nAlignment options: ")
  (princ "\n\t TLeft   TCenter   TRight ")
  (princ "\n\t MLeft   MCenter   MRight ")
  (princ "\n\t BLeft   BCenter   BRight ")
  (princ "\n\t  Left    Center    Right")
  (princ "\n\tAligned   Middle    Fit")
  (if (not (getvar "DIMCLRD")) (textscr))
  (princ "\n\nPress any key to return to your drawing. ")
  (grread)
  (princ "\r                                           ")
  (graphscr)
)
;;;
;;; -------------------------- MAIN PROGRAM ----------------------------------
;;;
(defun SPECS (/ olderr ang c cd cntcol d dims eof noofcol rtfile rf rfa h j l1 opt pt 
              pt1 ts n nl lc s ul
             )  ; at_ang holds default ANGLE
  ; at_fnm holds default fILE



  ; Altered setq for specs
  (setq olderr  *error*
        *error* at_err
        DIMS    (getvar "dimscale")
  ) ;End setq
  (command "undo" "be")
  (modes '("BLIPMODE" "CMDECHO" "HIGHLIGHT"))
  (while (null rtfile)  ; Prompt for file to be inserted
    (if (null at_fnm) 
      (progn 
        (initget 1)
        (princ "\nFile to read (including extension): ")
        (setq rf (getfiled "ASCII Import" "" "asc" 4))
      )
      (progn 
        (princ 
          (strcat "\nFile to read (including extension) <" at_fnm ">/Browse: ")
        )
        (initget "Browse")
        (setq brow (getkword))
        (if (= brow "Browse") 
          (setq rf (getfiled "ASCII Import" "" "asc" 4))
          (setq rf at_fnm)
        )
      )
    )
    (if (and (= rf "") (/= nil at_fnm)) 
      (setq rf at_fnm)
    )
    (setq rfa (findfile rf))
    (if rfa 
      (progn 
        (setq at_fnm rfa)
        (if (null (setq rtfile (open rfa "r"))) 
          (princ 
            (strcat 
              "\n\tFile found, but couldn't open "
              at_fnm
              " for reading. "
            )
          )
        )
      )
      (if (/= (substr rf (- (strlen rf) 3) 1) ".") 
        (princ "\nFile not found.  Extension may be missing.")
        (princ "\nFile not found. ")
      )
    )
  )
  (setq cont T)
  ;; Prompt for start point or justification
  (while cont 
    (if (getvar "DIMCLRD") 
      ;      (initget 1 (strcat "TLeft TCenter TRight "
      ;                         "MLeft MCenter Mright "
      ;                         "BLeft BCenter Bright "
      ;                         "Aligned Center Fit Left Middle Right ?"))
      (initget 1) ; "Aligned Center Fit Left Middle Right ?")
    )
    (setq pt (getpoint "\nStart point: ")) ;or Center/Middle/Right/?: "))
    (if (/= (type pt) 'LIST) 
      (if (= pt "?") 
        (progn 
          (justpn)
          (setq cont T)
        )
        (progn 
          (setq cont nil)
          (setq j (substr pt 1 2))
          (setq j pt)
          (if (= j "Center") (setq j "MCenter"))
          (initget 1)
          (setq pt (getpoint (strcat "\n" pt " point: ")))
          (if (or (= j "Aligned") (= j "Fit")) 
            (progn 
              (initget 1)
              (setq pt2 (getpoint pt (strcat "\nOther point: ")))
              (setq at_ang (* (/ 180 pi) (angle pt pt2)))
            )
          )
        )
      )
      (setq j    "L"
            cont nil
      )
    )
  ) ; Prompt for an insertion point
  (setq pt1 pt) ; First insertion point

  ;; Prompt for a text height

  (setq ts (tblsearch "STYLE" (getvar "TEXTSTYLE"))
        h  nil
  )
  (if (and (/= j "Aligned") (= (cdr (assoc 40 ts)) 0.0)) 
    (progn 
      (initget 6)



      ; The following setq has been removed to prevent ASCTEXT from prompting
      ; for a text height
      ;      (setq h (getdist pt (strcat "\nHeight <"
      ;                                  (rtos (getvar "TEXTSIZE")) ">: ")))



      (if (null h) 
        (setq h (getvar "TEXTSIZE"))
      )
    )
  ) ;Prompt for rotation angle of text




  ; The following setq has been removed to prevent ASCTEXT from prompting
  ; for a rotation angle
  ;  (if (null at_ang)
  ;    (progn
  ;      (if (= (cdr (assoc 70 ts)) 4)   ; Vertical style text
  ;        (progn
  ;          (setq at_ang 270)
  ;          (princ "\nRotation angle <270>: ")
  ;        )
  ;        (progn
  ;          (setq at_ang 0)
  ;          (princ "\nRotation angle <0>: ")
  ;        )
  ;      )
  ;    )
  ;    (if (and (/= j "Aligned") (/= j "Fit"))
  ;      (progn
  ;        (princ "\nRotation angle <")
  ;        (princ (strcat (angtos at_ang) ">: "))
  ;        (setq ang (getangle pt))
  ;      )
  ;    )
  ;  )
  ;  (if (null ang)
  ;    (setq ang at_ang)
  ;  )
  ;  (setq at_ang ang)

  ; The following lines have been added to set the angle to 0.
  (setq at_ang 0
        ang    0
  )




  (setq d   "Auto"
        l1  1
        n   "All"
        opt nil
        lc  0
        nl  0
        c   0
  )



  ; The following lines have been altered or removed to prevent some options from appearing.
  ;  (initget "Yes No")
  (if (= "Yes" "Yes")  ; (getkword "\nChange text options? <N>: "))
    (progn  ; Prompt for distance between lines.
      ;      (initget "Auto")
      ;      (setq d (getdist pt "\nDistance between lines/<Auto>: "))
      (if (= d nil) 
        (setq d "Auto")
      ) ; Prompt for first line to read.
      ;      (initget (+ 2 4))
      ;      (setq l1 (getint "\nFirst line to read/<1>: "))
      (if (null l1) 
        (setq l1 1)
      ) ; Prompt for number of following lines.
      ;      (initget (+ 2 4) "All")
      ;      (setq n (getint "\nNumber of lines to read/<All>: "))
      (if (= n "All") 
        (setq n nil)
      )
      ;      (initget "Yes No")
      ;      (if (= "Yes" (getkword "\nUnderscore each line? <N>: "))
      ;        (setq opt (append opt '(1)))
      ;      )
      ;      (initget "Yes No")
      ;      (if (= "Yes" (getkword "\nOverscore each line? <N>: "))
      ;        (setq opt (append opt '(2)))
      ;      )                               ; Option for global redefinition of text-case.
      ;      (initget "Upper Lower No")
      ;      (princ "\nChange text case? ")
      ;      (setq ul (getkword "  Upper/Lower/<N>: "))
      ;      (cond ((= ul "Upper") (setq opt (append opt '(4))))
      ;        ((= ul "Lower") (setq opt (append opt '(8))))
      ;      )                               ; Option for setting up columns.
      ;      (initget "Yes No")
      (if (= "Yes" "Yes")  ; (getkword "\nSet up columns? <N>: "))
        (progn 
          (setq opt (append opt '(16)))
          ;          (initget (+ 1 2))
          (setq cd (getdist pt "\nDistance between columns: ")) ;(* DIMS 7.5))
          (initget (+ 2 4))
          (setq nl (getint "\nNumber of lines per column <140>: "))
          (initget (+ 2 4) "Infinite")
          (setq noofcol (getint "\nNumber of columns per drawing <4>/Infinite: ")
                cntcol  1
          ) ;End setq
          (if (= noofcol "Infinite") 
            (setq noofcol -1)
          )
          (if (null noofcol) 
            (setq noofcol 4)
          ) ;End if
          (if (null nl) 
            (setq nl 140)
          ) ;End if
        )
      )
    )
  )
  (setvar "BLIPMODE" 0)
  (setvar "HIGHLIGHT" 0)
  (setvar "CMDECHO" 0)
  ;*****************************************************************
  (cond 
    ((and *debug* (/= 0 *debug*))
     (setq *error* nil)
     (setvar "cmdecho" 1)
    )
  )
  ;*****************************************************************
  (setq eof nil)
  (setq s (repeat l1 
            (read-line rtfile)
          )
  )
  (setq lc (1+ lc))
  (1ltxt)
  (while (null eof) 
    (if (= d "Auto") 
      (progn 
        (setq s (read-line rtfile))
        (setq lc (1+ lc))
        (if s 
          (progn 
            (if (= lc (1+ nl)) 
              (1ltxt)
              (progn 
                (if (member '1 opt) 
                  (setq s (strcat "%%u" s "%%u"))
                )
                (if (member '2 opt) 
                  (setq s (strcat "%%o" s "%%o"))
                )
                (if (member '4 opt) 
                  (setq s (strcase s))
                )
                (if (member '8 opt) 
                  (setq s (strcase s T))
                )
                (command "TEXT" "" s)
                (setq c (1+ c))
                (if (= c n) 
                  (setq eof T)
                )
              )
            )
          )
          (setq eof T)
        )
      )
      (progn 
        (setq s (read-line rtfile))
        (setq lc (1+ lc))
        (if s 
          (1ltxt)
          (setq eof T)
        )
      )
    )
  )


  (close rtfile)
  (setq rtfile nil)
  (moder) ; Restore modified modes
  (command "undo" "e")
  (setq *error* olderr) ; Restore old *error* handler
  (princ)
) 

(defun c:SPECS () (vmon) (SPECS))
(princ)