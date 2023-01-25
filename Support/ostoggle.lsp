;;; Copyright Lance A. Endres

(defun c:OSTOGGLE (/ OSM ORTH) 
  (setq OLDERR  *ERROR*
        *ERROR* OSTOGGLEERR
        ORTH    (getvar "orthomode")
        CMD     (getvar "cmdecho")
        OSM     (getvar "osmode")
  ) ;End setq
  (setvar "cmdecho" 0)
  (setvar "orthomode" 0)
  (cond 
    ((= (logand OSM 16384) 16384)
     (setvar "osmode" (- OSM 16384))
    ) ;_ End cond for osmode set but off
    ((and (/= (logand OSM 16384) 16384) (/= OSM 0))
     (setvar "osmode" (+ OSM 16384))
    ) ;_ End cond for osmode on
    ((= OSM 0)
     (command "_.ddosnap")
    ) ;_ End cond for osmode 0
  ) ;_ End cond
  (setvar "orthomode" ORTH)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun

(defun OSTOGGLEERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;End if
  (setvar "orthomode" ORTH)
  (setq *ERROR* OLDERR)
  (princ)
) ;_ End defun