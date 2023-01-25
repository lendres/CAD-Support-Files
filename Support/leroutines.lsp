(defun C:COPY_MULT (/ SS) 
  (setq OLDERR  *ERROR*
        *ERROR* CMERR
        SS      (ssget)
  ) ;_ End setq
  (command "copy" SS "" "m")
  (setq *ERROR* OLDERR)
  (princ)
)

(defun CMERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;_ End if
  (setq *ERROR* OLDERR)
  (princ)
)

(defun C:CRSTRETCH () 
  (command "stretch" "c")
  (princ)
)

(defun C:CLEAN () 
  (command "_.purge" "all" "" "n")
  (princ)
)

(defun C:UCSOB () 
  (command "_.ucs" "ob")
  (princ)
)

(defun C:UPDATE () 
  (command "dim1" "update" "all" "")
  (princ)
)

(defun C:LINSB (/ PT1) 
  (setq PT1 (getvar "insbase"))
  (command "line" PT1)
  (princ)
)