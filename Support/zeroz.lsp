;;; =======================================================
;;;
;;; ZeroZ.lsp
;;;
;;; Change Z coordinate of all selected entities to 0 (OCS)
;;;
;;; Copyright (c) 2000 Michael Puckett All Rights Reserved
;;;
;;; =======================================================

(defun C:ZEROZ (/ ; local functions (defuns)
                *ERROR* *BEGIN* *END* *ZEROZ* *CHILDREN* ; local vars
                SS I ENT ENTS
               ) 

  ;;; local defun *error*.
  (defun *ERROR* (S) 
    (*END*)
    (princ (strcat "Error: " S ".\n"))
    (princ)
  ) ;_ End defun

  ;;; local defun *begin*.
  (defun *BEGIN* () 
    (setvar "cmdecho" 0)
    (while (eq 8 (logand 8 (getvar "undoctl"))) 
      (command ".undo" "_end")
    ) ;_ End while
    (if (zerop (logand 2 (getvar "undoctl"))) 
      (if (eq 1 (logand 1 (getvar "undoctl"))) 
        (command ".undo" "_begin")
      ) ;_ End if
    ) ;_ End if
  ) ;_ End defun

  ;;; local defun *end*.
  (defun *END* () 
    (if (eq 8 (logand 8 (getvar "undoctl"))) 
      (command ".undo" "_end")
    ) ;_ End if
    (setvar "cmdecho" 1)
  ) ;_ End defun

  ;;; local defun *zeroz*.
  (defun *ZEROZ* (ENT) 
    (entmod 
      (mapcar 
        '(lambda (X) 
           (cond 
             ((member (car X) '(10 11 12 13 14))
              (cons (car X) (list (cadr X) (caddr X) 0.0))
             )
             ((eq 38 (car X)) '(38 . 0.0))
             (t X)
           ) ;_ End cond
         ) ;_ End lambda
        (entget ENT)
      ) ;_ End mapcar
    ) ;_ End entmod
  ) ;_ End defun

  ;;; local defun *children*.
  (defun *CHILDREN* (ENT / D R) 
    (if (assoc 66 (entget ENT)) 
      (reverse 
        (while 
          (/= 
            "SEQEND"
            (cdr 
              (assoc 
                0
                (setq D (entget (setq ENT (entnext ENT)))) ;_ End setq
              ) ;_ End assoc
            ) ;_ End cdr
          ) ;_ End /=
          (setq R (cons (cdr (assoc -1 D)) R))
        ) ;_ End while
      ) ;_ End reverse
    ) ;_ End if
  ) ;_ End defun

  ;;; main

  (cond 
    ((setq I  -1
           SS (ssget)
     ) ;_ End setq
     (*BEGIN*)
     (princ "\nZeroing Z's for entity(s) ...")
     (repeat (sslength SS) 
       (*ZEROZ* (setq ENT (ssname SS (setq I (1+ I)))))
       (foreach X (setq ENTS (*CHILDREN* ENT)) (*ZEROZ* X))
       (if ENTS 
         (entupd ENT)
       ) ;_ End if

       ;;; In case a bazillion entities were selected let the user know we have not died.
       (if (zerop (rem I 100)) 
         (princ ".")
       ) ;_ End if
     ) ;_ End repeat
     (princ " [Done]")
     (*END*)
    )
    (t (princ "\nNothing selected."))
  ) ;_ End cond

  ;;; Terminate.

  (princ)
) ;_ End defun
