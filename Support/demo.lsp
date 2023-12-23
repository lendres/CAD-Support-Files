;;; Original author unknown.
;;; Modifications copyright Lance A. Endres

(defun c:DEMO (/ CMD RP1 RP2 RP3 RP4) 
  (setq CMD (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq RP1 (getpoint "\nFirst Point: ")
        RP3 (getcorner RP1 "\nSecond Point: ")
        RP2 (list (car RP1) (cadr RP3))
        RP4 (list (car RP3) (cadr RP1))
  )
  (command 
    "-hatch"
	"P"
    "ansi31"
    (* (getvar "dimscale") 0.6)
    "0"
    "W"
    "n"
    RP1
    RP2
    RP3
    RP4
    "c"
    ""
	""
  )
  (setvar "cmdecho" CMD)
  (princ)
)

(defun DEMOERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setvar "clayer" CLAY)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
)