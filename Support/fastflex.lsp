;;; Original author unknown
;;; Modifications and additions copyright Lance A. Endres

;;; Curve flex commands.
(defun c:FC () 
  (START-FAST-CURVE-FLEX "SAFETYON")
) ;End fast flex for new

(defun c:EFC () 
  (START-FAST-CURVE-FLEX "SAFETYOFF")
) ;End fast flex for existing

;;; Linear flex commands.
(defun c:FL () 
  (DRAW-FAST-LINEAR-FLEX "SAFETYON")
) ;End fast flex for new

(defun c:EFL () 
  (DRAW-FAST-LINEAR-FLEX "SAFETYOFF")
) ;End fast flex for existing

;;; S-shape flex commands.
(defun c:FS () 
  (DRAW-FAST-S-FLEX "SAFETYON")
) ;End fast flex for new

(defun c:EFS () 
  (DRAW-FAST-S-FLEX "SAFETYOFF")
) ;End fast flex for existing

;;; Radians and degree coversions.
(defun DEG2RAD (DEGREES /)  ;Convert degrees to radians
  (/ (* DEGREES PI) 180.0)
)

(defun RAD2DEG (RADIANS /)  ;Convert radians to degrees
  (/ (* RADIANS 180.0) PI)
)

;;; Curve subroutines.
(defun START-FAST-CURVE-FLEX (SAFETY / CMD DIST LOOP2 ORTH OSM PLWID PE PT1 PT2 PT3 STEST) 
  (setq OLDERR  *ERROR*
        *ERROR* FASTFLEXERR
        LOOP2   t
        STEST   nil
        ORTH    (getvar "orthomode")
        PLWID   (getvar "plinewid")
        CMD     (getvar "cmdecho")
        OSM     (getvar "osmode")
  ) ;End setq
  (setvar "cmdecho" 0)
  (setvar "orthomode" 0)
  (setvar "plinewid" 0)
  (while LOOP2 
    (initget 1)
    (setq PT1 (getpoint "\nStart point: "))
    (initget 1)
    (setq PT2 (getpoint PT1 "\nSecond point: "))
    (initget 1)
    (setq PT3  (getpoint PT2 "\nEnd point: ")
          DIST (distance PT1 PT3)
    ) ;End setq
    (if (= SAFETY "SAFETYOFF") 
      (setq DIST 1)
    ) ;End if
    (if (> DIST 60) 
      (princ "\n*Flex distance can't be more than 5'*")
      (setq LOOP2 nil)
    ) ;End if
  ) ;End while
  (setvar "osmode" 0)
  (DRAW-FAST-CURVE-FLEX PT1 PT2 PT3)
  (setq PE (polar CENPT (- (+ STANG (* INCR COUNT)) INCR2) DISTM))
  (command PE PT3 "")
  (setvar "orthomode" ORTH)
  (setvar "plinewid" PLWID)
  (setvar "osmode" OSM)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
)

(defun DRAW-FAST-CURVE-FLEX (PT1 PT2 PT3 / ANG ARCDIST ARK DISTF DISTN ENDANG FLAG HW 
                             REPS PE PS SEG-NUM SF STEST SWITCH TESTANG YCENPT YPT1 
                             YPT2 YPT3
                            ) 
  (command "arc" PT1 PT2 PT3)
  (setq ARK     (entlast)
        SF      (getvar "dimscale")
        CENPT   (cdr (assoc 10 (entget ARK)))
        STANG   (cdr (assoc 50 (entget ARK)))
        DISTM   (distance CENPT PT1)
        HW      (* SF (/ 1 16.0))
        DISTN   (- DISTM HW)
        DISTF   (+ DISTM HW)
        TESTANG (rtos (angle CENPT PT1))
        YPT1    (car (cdr PT1))
        YPT2    (car (cdr PT2))
        YPT3    (car (cdr PT3))
        YCENPT  (car (cdr CENPT))
        ANG     nil
  ) ;End setq
  (if (= (rtos STANG) TESTANG) 
    (setq ENDANG (cdr (assoc 51 (entget ARK)))
          SWITCH nil
    ) ;End setq
    (setq ENDANG STANG
          STANG  (cdr (assoc 51 (entget ARK)))
          SWITCH t
    ) ;End setq
  ) ;End if - test for CW and CCW angles
  (cond 
    ((and (> YPT1 YCENPT) (< YPT3 YCENPT) (= SWITCH t))
     (setq ANG (abs (+ (- (* 2 PI) ENDANG) STANG)))
    )
    ((and (< YPT1 YCENPT) (> YPT3 YCENPT) (= SWITCH nil))
     (setq ANG (abs (+ (- (* 2 PI) STANG) ENDANG)))
    )
    ((and (< YPT1 YCENPT) (> YPT2 YCENPT) (< YPT3 YCENPT) (= SWITCH t))
     (setq ANG (abs (+ (- (* 2 PI) ENDANG) STANG)))
    )
    ((and (< YPT1 YCENPT) (> YPT3 YCENPT) (< YPT3 YCENPT) (= SWITCH nil))
     (setq ANG (abs (+ (- (* 2 PI) STANG) ENDANG)))
    )
    ((= ANG nil)
     (setq ANG (abs (- ENDANG STANG)))
    )
  ) ;End cond to set ANG
  (command "erase" "last" "")
  (setq ARCDIST (/ (* (RAD2DEG ANG) DISTM PI) 180)
        SEG-NUM (fix (/ ARCDIST (* SF (/ 1 32.0))))
        REPS    (- SEG-NUM 2)
        COUNT   2
        FLAG    T
  ) ;End setq
  (if (= STEST t) 
    (setq SEG-NUM (- SEG-NUM 0.5))
  ) ;End if
  (if (= SWITCH t) 
    (setq INCR (- 0 (/ ANG SEG-NUM))) ;Then
    (setq INCR (/ ANG SEG-NUM)) ;Else
  ) ;End if
  (setq INCR2 (/ INCR 2)
        PT2   (polar CENPT (+ STANG INCR2) DISTM)
        PE    (polar CENPT (+ STANG INCR) DISTF)
  ) ;End setq
  (command "PLINE" PT1 "W" 0 0 PT2 PE)
  (repeat REPS 
    (if (= FLAG T) 
      (setq PS    PE
            PE    (polar CENPT (+ STANG (* INCR COUNT)) DISTN)
            FLAG  nil
            COUNT (1+ COUNT)
      ) ;End setq
      (setq PS    PE
            PE    (polar CENPT (+ STANG (* INCR COUNT)) DISTF)
            FLAG  t
            COUNT (1+ COUNT)
      ) ;End setq
    ) ;End if
    (command PE)
  ) ;End repeat
)

;;; Draw linear flex

(defun DRAW-FAST-LINEAR-FLEX (SAFETY / A CMD DIST FLAG HW INCR LOOP1 ORTH OSM PE PHI 
                              PS PLWID P1 P2 PX SEG-NUM SF THETA
                             ) 
  (setq OLDERR  *ERROR*
        *ERROR* FASTFLEXERR
        LOOP1   t
        ORTH    (getvar "orthomode")
        PLWID   (getvar "plinewid")
        CMD     (getvar "cmdecho")
        OSM     (getvar "osmode")
  ) ;End setq
  (setvar "cmdecho" 0)
  (while LOOP1 
    (initget 1) ;no null
    (setq P1 (getpoint "\nStart point: "))
    (initget 1) ;no null
    (setq P2 (getpoint P1 "\nEnd point: "))
    (setq DIST (distance P1 P2))
    (if (= SAFETY "SAFETYOFF") 
      (setq DIST 1)
    ) ;End if
    (if (> DIST 60) 
      (prompt "\n*Flex distance can't be more than 5'*")
      (setq LOOP1 nil) ;else
    ) ;End if
  ) ;End while
  (setq SF      (getvar "dimscale")
        DIST    (distance P1 P2)
        HW      (* SF (/ 1 16.0))
        PHI     (angle p1 p2)
        THETA   (+ PHI (DEG2RAD 90))
        SEG-NUM (fix (/ DIST (* SF (/ 1 32.0))))
        INCR    (/ DIST SEG-NUM)
        PS      P1
        PE      (polar P1 PHI (/ INCR 2.0))
  ) ;End setq
  (setvar "osmode" 0)
  (command "PLINE" PS "W" 0 0 PE)
  (setq PS PE
        PX (polar PS PHI (/ INCR 2.0))
        PE (polar PX THETA HW)
  ) ;End setq
  (command PE)
  (setq A    1
        FLAG nil
  ) ;End setq
  (while (< A (1- SEG-NUM)) 
    (if FLAG 
      (progn 
        (setq THETA (+ PHI (DEG2RAD 90))
              FLAG  nil
        ) ;End setq
      ) ;End progn
      (progn 
        (setq THETA (- PHI (DEG2RAD 90))
              FLAG  t
        ) ;End setq
      ) ;End progn
    ) ;End if
    (setq PS PE
          PX (polar PX PHI INCR)
          PE (polar PX THETA HW)
    ) ;End setq
    (command PE)
    (setq A (1+ A))
  ) ;End while
  (setq PX (polar PX PHI (/ INCR 2.0)))
  (command PX P2 "")
  (setq *ERROR* OLDERR)
  (setvar "plinewid" PLWID)
  (setvar "cmdecho" CMD)
  (setvar "osmode" OSM)
  (princ)
)

;;; Draw s-shape flex

(defun DRAW-FAST-S-FLEX (SAFETY / ANGL CMD DIRANGL DIST LOOP2 ORTH OSM PLWID PT1 PT2 
                         PT3 PT4 PT5 PTDIR STEST SS
                        ) 
  (setq OLDERR  *ERROR*
        *ERROR* FASTFLEXERR
        LOOP2   t
        STEST   t
        CMD     (getvar "cmdecho")
        OSM     (getvar "osmode")
        PLWID   (getvar "plinewid")
        ORTH    (getvar "orthomode")
  ) ;End setq
  (setvar "cmdecho" 0)
  (setvar "plinewid" 0)
  (while LOOP2 
    (initget 1)
    (setq PT1 (getpoint "\nStart point: "))
    (setvar "orthomode" 1)
    (initget 1)
    (setq PTDIR (getpoint PT1 "\nDirection: "))
    (setvar "orthomode" 0)
    (initget 1)
    (setq PT2     (getpoint PT1 "\nEnd point: ")
          DIST    (distance PT1 PT2)
          LOOP2   nil
          ANGL    (angle PT1 PT2)
          DIRANGL (angle PT1 PTDIR)
          PT3     (polar PT1 ANGL (* DIST 0.5))
          PT4     (polar PT1 DIRANGL 0.01)
          PT5     (polar PT2 (+ DIRANGL PI) 0.01)
    ) ;End setq
    (if (= SAFETY "SAFETYOFF") 
      (setq DIST 1)
    ) ;End if
    (setvar "orthomode" ORTH)
    (if (> DIST 60) 
      (progn 
        (princ "\n*Flex distance can't be more than 5'*")
        (setq LOOP2 t)
      ) ;End progn
    ) ;End if test for dist
  ) ;End while
  (setvar "osmode" 0)
  (DRAW-FAST-CURVE-FLEX PT1 PT4 PT3)
  (command PT3 "")
  (setq SS (ssadd))
  (ssadd (entlast) SS)
  (DRAW-FAST-CURVE-FLEX PT2 PT5 PT3)
  (command 
    PT3
    ""
    "pedit"
    (entlast)
    "join"
    SS
    ""
    ""
  ) ;End command
  (setq *ERROR* OLDERR)
  (setvar "osmode" OSM)
  (setvar "plinewid" PLWID)
  (setvar "cmdecho" CMD)
  (princ)
)

(defun FASTFLEXERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  ) ;End if
  (setq *ERROR* OLDERR)
  (setvar "osmode" OSM)
  (setvar "plinewid" PLWID)
  (setvar "orthomode" ORTH)
  (setvar "cmdecho" CMD)
  (princ)
)