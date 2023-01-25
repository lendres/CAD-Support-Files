;;; Copyright Lance A. Endres

(defun C:MINSP (/ SS OSM PT1 PT2 PT3) 
  (setvar "cmdecho" 0)
  (princ "\nSelect objects to move")
  (setq SS  (ssget)
        OSM (getvar "osmode")
  ) ;End setq
  (setvar "osmode" 192)
  (setq PT1 (getpoint "\nSelect insertion to move FROM")
        PT2 (getpoint PT1 "\nSelect INS or PER to move perpendicular TO")
        PT3 (list (car PT2) (cadr PT1))
  ) ;End setq
  (setvar "osmode" OSM)
  (command "move" SS "" pt1 pt3)
  (setvar "cmdecho" 1)
  (princ)
)