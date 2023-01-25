;;; Original author unknown
;;; Modifications copyright Lance A. Endres

(defun c:GAP (/ A CMD IP OSM PNT1 PNT2 PNT3 SGAPDIST) 
  (setq OLDERROR *ERROR*
        *ERROR*  GAPERR
        SGAPDIST (/ (getvar "dimscale") 12)
        OSM      (getvar "osmode")
        IP       "Initiate While Loop"
        CMD      (getvar "cmdecho")
  ) ;_ End setq
  (setvar "cmdecho" 0)
  (defun DTR (A)  ;The dtr function converts degrees to radians
    (* PI (/ A 180.0))
  )
  (defun RTD (A)  ;The rtd function converts radians to degrees
    (/ (* a 180.0) PI)
  )
  (graphscr)
  (if (or (= GAPDIST nil) (= GAPDIST 0)) 
    (setq GAPDIST SGAPDIST)
  )
  (princ "\nGap distance = ")
  (princ GAPDIST)
  (while (/= IP nil) 
    (initget "Gap")
    (setvar "osmode" 1056)
    (setq IP (getpoint "\nGap distance/<Pick intersection point>: "))
    (if (= IP "Gap") 
      (progn 
        (princ "\nStandard gap distance = ")
        (princ SGAPDIST)
        (princ "\nGap distance <")
        (princ GAPDIST)
        (princ ">:")
        (initget 2)
        (setq GAPDIST (getreal)
              IP      (getpoint "\nPick intersection point: ")
        ) ;_ End setq
      ) ;_ End progn
    ) ;_ End if
    (setvar "osmode" 1536)
    (if (/= IP nil) 
      (progn 
        (setq FP (getpoint "\nPick line to break: ")
              A  (angle IP FP)
        ) ;_ End setq
        (setq AL   A
              PNT2 (polar IP A GAPDIST)
              PNT3 (polar IP (- a (DTR 180)) GAPDIST)
        ) ;_ End setq
        (command "break" FP "f" PNT2 PNT3)
      ) ;_ End progn
    ) ;_ End if
  ) ;_ End while
  (setvar "osmode" OSM)
  (setvar "cmdecho" CMD)
  (princ)
)

(defun GAPERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setvar "osmode" OSM)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
)