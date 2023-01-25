;;; Copyright Lance A. Endres

; Break at an intersection.
(defun c:BA ()
    (command "break" pause "f" "int" pause "@")
    (princ)
)

; Break an arc or circle at quadrant points.
(defun c:BQ ()
    (command "break" pause "f" "qua" pause "qua" pause)
    (princ)
)

; Break between two intersections.
(defun c:BZ ()
    (command "break" pause "f" "int" pause "int")
    (princ)
)