;;   MastrMnd.LSP   Version 2.5
;;
;;   (C) Copyright 1999 by Design Point
;;   All rights reserved
;;
;;   This program is copyrighted by Design Point and is licensed
;;   to you under the following conditions.  You may not distribute
;;   or publish the source code of this program in any form.
;;
;;   DESIGN POINT PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;   DESIGN POINT SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MER-
;;   CHANTABILITY OR FITNESS FOR A PARTICULAR USE.  DESIGN POINT
;;   DOES NOT WARRANT THAT THE OPERATION OF THIS OR PARTS OF THIS
;;   PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.
;;

;; This file to be used in conjuction with MASTRMND.DCL

(defun c:ld () (load "mastrmnd"))

(defun dpmm_about ()
   (new_dialog "dpmm_about" DPMM_DCL_FILE)
   (start_dialog)
)
(defun dpmm_init (a)
   (setq DPMM_DCL_FILE (load_dialog a))
)

;   Function Usage   : (repatom [Integer] [List] [Value])
;   Function Purpose : Replace the nth atom of list with a value
;   Global variables : none
;   Returns          : Returns new list, list is created if null
;
(defun dpmm_replace ( x y z / a b )
   (cond
      ((null y) (list z))
      ((/= (type x) 'int) y)
      ((setq x (abs x) a nil))
      ((zerop x) (append (list z) (cdr y)))
      ((>= x (length y)) (append y (list z)))
      ((= (1+ x) (length y)) (reverse (append (list z) (cdr (reverse y)))))
      (t
         (repeat (setq a (list (car y)) x (1- x))
            (setq a (append a (list (car (cdr y)))))
            (setq y (cdr y))
         )
         (append a (list z) (cdr (cdr y)))
      )
   )
)

(defun dp_random_num (x y / a b c)
   (if (not DP_SEED) (setq DP_SEED (getvar "DATE")))
   (setq a 65536 b 25173 c 13849 DP_SEED (rem (+ (* b DP_SEED) c) a))
   (setq a (fix (* 1000000 (/ DP_SEED a))))
   (rem a (1+ (- x y)))
)

(defun dpmm_set_color (x y / a)
   (start_image (setq a (strcat "dp_guess_ib" (itoa DPMM_ROW_CNTR) x)))
   (fill_image 0 0
      (dimx_tile a)
      (dimy_tile a)
      DPMM_COLOR
   )
   (end_image)
   (setq dpmm_user_guess (dpmm_replace y dpmm_user_guess DPMM_COLOR))
)
(defun dpmm_reveal_guess ()
   (setq a (mapcar '= dpmm_user_guess dpmm_master))
   (setq b '())
   (foreach c dpmm_user_guess
      (if (member c dpmm_master)
         (setq b (append b (list T)))
      )
   )
   (setq d 0 e '(9 9 9 9) f 0)
   (foreach c a
      (if (and c)
         (progn
            (setq b (cdr b))
            (setq d (1+ d))
         )
      )
   )
   (while (/= f d)
      (setq c (dp_random_num 3 0))
      (if (= (nth c e) 9)
         (progn
            (setq e (dpmm_replace c e 7))
            (setq f (1+ f))
         )
      )
   )
   (setq f 0)
   (while (/= f (length b))
      (setq c (dp_random_num 3 0))
      (if (= (nth c e) 9)
         (progn
            (setq e (dpmm_replace c e 0))
            (setq f (1+ f))
         )
      )
   )
   (mapcar '(lambda (x y / a)
         (start_image (setq a (strcat "dp_answer_ib" (itoa DPMM_ROW_CNTR) x)))
         (fill_image 0 0
            (dimx_tile a)
            (dimy_tile a)
            y
         )
         (end_image)
      )
      '("a" "b" "c" "d")
      e
   )
)
(defun dpmm_get_master (/ a b c)
   (setq a '() b 0)
   (while (/= b 4)
      (setq c (dp_random_num 7 0))
      (if (not (member c a))
         (progn
            (setq a (append a (list c)))
            (setq b (1+ b))
         )
      )
   )
   a
)
(defun dpmm_compare ()
   (cond
      ((member nil dpmm_user_guess)
         (alert "\Need more data")
      )
      ((equal dpmm_master dpmm_user_guess)
         (new_dialog "dpmm_winner" DPMM_DCL_FILE)
         (start_dialog)
         (dpmm_reveal_master)
      )
      (t
         (dpmm_reveal_guess)
         (setq DPMM_ROW_CNTR (1- DPMM_ROW_CNTR))
         (setq dpmm_user_guess '(nil nil nil nil))
         (if (= 0 DPMM_ROW_CNTR)
            (dpmm_reveal_master)
         )
      )
   )
)
(defun dpmm_reveal_master ()
   (mapcar '(lambda (x y)
         (start_image (setq a (strcat "dp_master_" x)))
         (fill_image 0 0
            (dimx_tile a)
            (dimy_tile a)
            y
         )
         (end_image)
      )
      '("a" "b" "c" "d")
      dpmm_master
   )
)
(defun dpmm_clear (/ a b)
   (setq DPMM_ROW_CNTR 8)
   (setq dpmm_user_guess '(nil nil nil nil))
   (setq dpmm_master (dpmm_get_master))
   (mapcar '(lambda (x / a)
         (start_image (setq a (strcat "dp_master_" x)))
         (fill_image 0 0
            (dimx_tile a)
            (dimy_tile a)
            9
         )
         (end_image)
      )
      '("a" "b" "c" "d")
   )
   (setq b 1)
   (repeat 8
      (mapcar '(lambda (x / a)
            (start_image (setq a (strcat "dp_guess_ib" (itoa b) x)))
            (fill_image 0 0
               (dimx_tile a)
               (dimy_tile a)
               9
            )
            (end_image)
            (start_image (setq a (strcat "dp_answer_ib" (itoa b) x)))
            (fill_image 0 0
               (dimx_tile a)
               (dimy_tile a)
               9
            )
            (end_image)
         )
         '("a" "b" "c" "d")
      )
      (setq b (1+ b))
   )
)

(defun c:dp_master_mind (/ a b c)
   (setvar "cmdecho" 0)
   (setq dpmm_err *error*
         *error* '((a)
            (cond
               ((= (setq a (strcase a)) "FUNCTION CANCELLED") (princ "\n*Function Cancelled by User*"))
               (t (princ (strcat "\n*AutoLisp Error* [" a "]")))
            )
            (setq *error* dpmm_err)
            (princ)
         )
   )
   (cond
      (DPMM_DCL_FILE)
      ((setq a (findfile "mastrmnd.dcl")) (dpmm_init a))   ; Assumed Startup -- Initialize globals
      ((setq a (findfile "mastrmnd.dcl")))
      (t (alert "Can not find file MASTRMND.DCL\n\nMake sure this file is in your ACAD search path."))
   )
   (if (not DPMM_COLOR) (setq DPMM_COLOR 0))
   (setq DPMM_ROW_CNTR 8)
   (setq dpmm_user_guess '(nil nil nil nil))
   (setq dpmm_master (dpmm_get_master))
   (cond
      (DPMM_DCL_FILE
         (while (not c)
            (new_dialog "dp_master_mind" DPMM_DCL_FILE)
            (action_tile "dp_guess_ib8a" "(dpmm_set_color \"a\" 0)")
            (action_tile "dp_guess_ib8b" "(dpmm_set_color \"b\" 1)")
            (action_tile "dp_guess_ib8c" "(dpmm_set_color \"c\" 2)")
            (action_tile "dp_guess_ib8d" "(dpmm_set_color \"d\" 3)")

            (action_tile "dp_guess_ib7a" "(dpmm_set_color \"a\" 0)")
            (action_tile "dp_guess_ib7b" "(dpmm_set_color \"b\" 1)")
            (action_tile "dp_guess_ib7c" "(dpmm_set_color \"c\" 2)")
            (action_tile "dp_guess_ib7d" "(dpmm_set_color \"d\" 3)")

            (action_tile "dp_guess_ib6a" "(dpmm_set_color \"a\" 0)")
            (action_tile "dp_guess_ib6b" "(dpmm_set_color \"b\" 1)")
            (action_tile "dp_guess_ib6c" "(dpmm_set_color \"c\" 2)")
            (action_tile "dp_guess_ib6d" "(dpmm_set_color \"d\" 3)")

            (action_tile "dp_guess_ib5a" "(dpmm_set_color \"a\" 0)")
            (action_tile "dp_guess_ib5b" "(dpmm_set_color \"b\" 1)")
            (action_tile "dp_guess_ib5c" "(dpmm_set_color \"c\" 2)")
            (action_tile "dp_guess_ib5d" "(dpmm_set_color \"d\" 3)")

            (action_tile "dp_guess_ib4a" "(dpmm_set_color \"a\" 0)")
            (action_tile "dp_guess_ib4b" "(dpmm_set_color \"b\" 1)")
            (action_tile "dp_guess_ib4c" "(dpmm_set_color \"c\" 2)")
            (action_tile "dp_guess_ib4d" "(dpmm_set_color \"d\" 3)")

            (action_tile "dp_guess_ib3a" "(dpmm_set_color \"a\" 0)")
            (action_tile "dp_guess_ib3b" "(dpmm_set_color \"b\" 1)")
            (action_tile "dp_guess_ib3c" "(dpmm_set_color \"c\" 2)")
            (action_tile "dp_guess_ib3d" "(dpmm_set_color \"d\" 3)")

            (action_tile "dp_guess_ib2a" "(dpmm_set_color \"a\" 0)")
            (action_tile "dp_guess_ib2b" "(dpmm_set_color \"b\" 1)")
            (action_tile "dp_guess_ib2c" "(dpmm_set_color \"c\" 2)")
            (action_tile "dp_guess_ib2d" "(dpmm_set_color \"d\" 3)")

            (action_tile "dp_guess_ib1a" "(dpmm_set_color \"a\" 0)")
            (action_tile "dp_guess_ib1b" "(dpmm_set_color \"b\" 1)")
            (action_tile "dp_guess_ib1c" "(dpmm_set_color \"c\" 2)")
            (action_tile "dp_guess_ib1d" "(dpmm_set_color \"d\" 3)")

            (action_tile "dpmm_about" "(dpmm_about)")
            (action_tile "dp_selecta" "(setq DPMM_COLOR 0)")
            (action_tile "dp_selectb" "(setq DPMM_COLOR 1)")
            (action_tile "dp_selectc" "(setq DPMM_COLOR 2)")
            (action_tile "dp_selectd" "(setq DPMM_COLOR 3)")
            (action_tile "dp_selecte" "(setq DPMM_COLOR 4)")
            (action_tile "dp_selectf" "(setq DPMM_COLOR 5)")
            (action_tile "dp_selectg" "(setq DPMM_COLOR 6)")
            (action_tile "dp_selecth" "(setq DPMM_COLOR 7)")
            (action_tile "dp_reveal" "(dpmm_reveal_master)")
            (action_tile "dpmm_restart" "(dpmm_clear)")
            (action_tile "dpmm_guess" "(dpmm_compare)")
            (action_tile "dpmm_exit" "(progn (setq c t) (done_dialog))")
            (start_dialog)
         )
      )
   )
   (princ)
)

(defun c:mm () (c:dp_master_mind))
(princ "\nDesign Point's MasterMind now loaded.")
(princ"\nType MM to start command.")
(princ)
