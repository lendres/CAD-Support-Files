;;; angle.lsp
;;; Copyright 1998-2000 by Lance A. Endres

(defun c:ANGLE (/ VERT PT1 PT2 ANG1 ANG2 ANG3)
    (setvar "cmdecho" 0)
    (setq VERT (getpoint "\nVertex of Angle: ")
          PT1 (getpoint "\nFirst End Point of Angle: ")
          PT2 (getpoint "\nSecond End Point of Angle: ")
          ANG1 (angle VERT PT1)
          ANG2 (angle VERT PT2)
          ANG3 (abs (- ANG2 ANG1))
    ) ;End setq
    (if (> ANG3 PI)
        (setq ANG4 (- ANG3 PI)
              ANG3 (- PI ANG4)
        ) ;End setq
    ) ;End if
    (setq ANGL (/ (* ANG3 180) PI))
    (princ "\nThe angle is: ")
    (princ ANGL)
    (setvar "cmdecho" 1)
    (princ)
)