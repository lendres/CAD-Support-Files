;;; Copyright Lance A. Endres

;Investigate as a possible way to break lines if needed.
;^CINSERT;PIPE12;NEA;\1.0;;90;ERASE;L;;BREAK;@-1',0;F;+
;@7",0;@10",0;oops;

(defun c:PIPEIN (/ ;ANGL1 ANGL2 ANGLTST1 ANGLTST2 ANGLTST3 BP1 BP2 BLK CLAY CMD
                 ;DIMS ENT1 ENT2 ENT1DEF ENT2DEF ENT1LAY ENT1TYPE ENT2TYPE INSPT
                 ;PT1 PT2 PT3 PT4 PTT XPT1 XPT2
                ) 
  ;    (setq OLDERR *ERROR*
  ;          *ERROR* PIPEINERR
  (setq OSM  (getvar "OSMODE")
        DIMS (getvar "dimscale")
        CLAY (getvar "clayer")
        CMD  (getvar "cmdecho")
        ent1 nil
  ) ;_ End setq
  ;    (setvar "cmdecho" 0)
  (princ "\nBlock name <")
  (princ LSTINS)
  (setq BLK (getstring ">: "))
  (if (= BLK "") 
    (setq BLK LSTINS)
  ) ;_ End if
  (setq BLK (strcase BLK))
  (cond 
    ((= BLK "MS-0104")
     (setvar "osmode" 1)
     (BLKPROCESSOR1)
     (setq BRKDIST (* 0.02636701875 DIMS)
           BP1     (polar INSPT ANGL1 BRKDIST)
           BP2     INSPT
           ANGL1   (+ ANGL1 PI)
     )
    ) ;_ End cond for block ms-0104
    ((= BLK "MS-0105")
     (setvar "osmode" 512)
     (BLKPROCESSOR1)
     (setq BRKDIST (* 0.0292966875 DIMS)
           BP1     (polar INSPT ANGL1 BRKDIST)
           BP2     (polar INSPT ANGL2 BRKDIST)
     )
    ) ;_ End cond for block ms-0105
    ((or 
       (= BLK "MS-0108")
       (= BLK "MS-0109")
       (= BLK "MS-0111")
       (= BLK "MS-0113A")
       (= BLK "MS-0115")
       (= BLK "MS-0116")
       (= BLK "MS-0117")
       (= BLK "MS-0118")
       (= BLK "MS-0123")
       (= BLK "MS-0124")
       (= BLK "MS-0133")
       (= BLK "MS-0134")
       (= BLK "MS-0143")
       (= BLK "MS-0158")
     ) ;_ End or
     (setvar "osmode" 512)
     (BLKPROCESSOR1)
     (setq BRKDIST (* 0.0585993375 DIMS)
           BP1     (polar INSPT ANGL1 BRKDIST)
           BP2     (polar INSPT ANGL2 BRKDIST)
     )
    ) ;_ End cond for blocks made from gate valve - non three way valves
    ((or (= BLK "MS-0141") (= BLK "MS-0142"))
     (BLKPROCESSOR2)
     (setq BRKDIST (* 0.0585993375 DIMS)
           BP1     (polar INSPT ANGL1 BRKDIST)
           BP2     (polar INSPT ANGL2 BRKDIST)
           BP3     PT3
           BP4     (polar INSPT ANGL3 BRKDIST)
     )
    ) ;_ End cond for three way valves
    ((= BLK "MS-0110")
     (setvar "osmode" 512)
     (BLKPROCESSOR1)
     (setq BRKDIST (* 0.0175780125 DIMS)
           BP1     (polar INSPT ANGL1 BRKDIST)
           BP2     (polar INSPT ANGL2 BRKDIST)
     )
    ) ;_ End cond for block ms-0110
    ((= BLK "MS-0122")
     (setvar "osmode" 512)
     (BLKPROCESSOR1)
     (setq BRKDIST (* 0.029296625 DIMS)
           BP1     (polar INSPT ANGL1 BRKDIST)
           BP2     (polar INSPT ANGL2 BRKDIST)
     )
    ) ;_ End cond for block ms-0122
    ((= BLK "MS-0144")
     (setvar "osmode" 512)
     (BLKPROCESSOR1)
     (setq BRKDIST (* 0.05859375 DIMS)
           BP1     (polar INSPT ANGL1 BRKDIST)
           BP2     (polar INSPT ANGL2 BRKDIST)
     )
    ) ;_ End cond for block ms-0144
    ((= BLK "SFIT")
     (setvar "osmode" 512)
     (BLKPROCESSOR1)
     (setq BRKDIST (* 0.03125 DIMS)
           BP1     (polar INSPT ANGL1 BRKDIST)
           BP2     (polar INSPT ANGL2 BRKDIST)
     )
    ) ;_ End cond for block sfit
  ) ;_ End cond
  (setvar "osmode" 0)
  (if INSPT  ;_ If test for a block supported by this routine
    (progn 
      (if (= ENT1 "LINE")  ;_ If there is a line to break
        (progn 
          (setq ANGL1 (/ (* ANGL1 180.0) PI))
          (command "_.insert" BLK INSPT DIMS DIMS ANGL1 "_.break" ENT1 BP1 BP2) ;_ End command
          (if (= ENT2 "LINE") 
            (command "_.break" ENT2 BP3 BP4)
          ) ;_ End if check for breaking the second line
        ) ;_ End then progn
        (command "_.insert" BLK INSPT DIMS DIMS pause) ;_ Else only insert
      ) ;_ End if
    ) ;_ End then progn
    (princ "\nThat block is not supported by this AutoLISP routine ")
  ) ;_ End if, routine will not go to subroutine without a block
  (setvar "osmode" OSM)
  (setvar "clayer" CLAY)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
)

(defun BLKPROCESSOR1 () 
  (setq ENT1     (entsel "\nPick the insertion point: ")
        INSPT    (cadr ENT1)
        ENT1     (car ENT1)
        ENT1DEF  (entget ENT1)
        ENT1TYPE (cdr (assoc 0 ENT1DEF))
        ENT1LAY  (cdr (assoc 8 ENT1DEF))
  ) ;_ End setq
  (setvar "clayer" ENT1LAY)
  (if (= ENT1TYPE "LINE") 
    (progn 
      (setq PT1  (cdr (assoc 10 ENT1DEF))
            PT2  (cdr (assoc 11 ENT1DEF))
            XPT1 (car PT1)
            XPT2 (car PT2)
      ) ;_ End setq
      (if (> XPT1 XPT2) 
        (setq PTT PT1
              PT1 PT2
              PT2 PTT
        ) ;_ End then
      ) ;_ End if
      (cond 
        ((equal INSPT PT1)
         (setq ANGL1 (angle INSPT PT2)
               TEST  1
         )
        ) ;_ End cond 1 for an end inserted block
        ((equal INSPT PT2)
         (setq ANGL1 (angle INSPT PT1)
               TEST  2
         )
        ) ;_ End cond 2 for an end inserted block
        (INSPT
         (setq ANGL1 (angle PT1 PT2)
               ANGL2 (- ANGL1 PI)
         ) ;_ End setq
        ) ;_ End cond for blocks not inserted at endpoints
      ) ;_ End cond
    ) ;_ End then progn
    (princ "\nThat entity is not a line")
  ) ;_ End if
) ;_ End defun

(defun BLKPROCESSOR2 () 
  (setq ENT1     (entsel "\nPick the first line: ")
        ENT2     (entsel "\nPick the second line: ")
        ENT1     (car ENT1)
        ENT2     (car ENT2)
        ENT1DEF  (entget ENT1)
        ENT2DEF  (entget ENT2)
        ENT1LAY  (cdr (assoc 8 ENT1DEF))
        ENT1TYPE (cdr (assoc 0 ENT1DEF))
        ENT2TYPE (cdr (assoc 0 ENT2DEF))
  )
  (setvar "clayer" ENT1LAY)
  (if (and (= ENT1TYPE "LINE") (= ENT2TYPE "LINE")) 
    (progn 
      (setq PT1   (cdr (assoc 10 ENT1DEF))
            PT2   (cdr (assoc 11 ENT1DEF))
            PT3   (cdr (assoc 10 ENT2DEF))
            PT4   (cdr (assoc 11 ENT2DEF))
            INSPT (inters PT1 PT2 PT3 PT4 nil)
            DIST1 (distance INTS PT1)
      ) ;_ End setq
      (foreach POINTS (list PT2 PT3 PT4) 
        (setq DIST2 (distance INSPT POINTS))
        (if (< DIST2 DIST1) 
          (setq DIST1    DIST2
                SHORTEST POINTS
          ) ;_ End setq
        ) ;_ End if
      ) ;_ End foreach
      (if (or (= SHORTEST PT1) (= SHORTEST PT2)) 
        (setq ENTT ENT1
              ENT1 ENT2
              ENT2 ENTT
              PTT  PT1
              PT1  PT3
              PT3  PTT
              PTT  PT2
              PT2  PT4
              PT4  PTT
        ) ;_ End setq entity reversal
      ) ;_ End if
      (setq DIST1 (distance INSPT PT3)
            DIST2 (distance INSPT PT4)
      ) ;_ End setq
      (if (> DIST1 DIST2) 
        (setq PTT PT3
              PT3 PT4
              PT4 PTT
        ) ;_ End then
      ) ;_ End if
      (setq ANGL1    (angle PT1 PT2)
            ANGL2    (angle PT3 PT4)
            ANGLTST1 (angle INSPT PT1)
            ANGLTST2 (angle INSPT PT3)
            ANGLTST3 (abs (- ANGLTST2 ANGLTST1))
            ANGLTST3 (/ (* ANGLTST3 180) PI)
      ) ;_ End setq
      (if (or (/= ANGLTST3 "270") (/= ANGLTST3 "90")) 
        (progn 
          (setq ENT2 nil)
          (princ "\nThe lines are not perpendicular to each other")
        ) ;_ End then progn
        (progn 
          (setq ANGL1 (+ ANGL2 (/ PI 2)))
        ) ;_ End else progn
      ) ;_ End if
    ) ;_ End then progn
    (progn 
      (if (= ENT1TYPE "LINE") 
        (progn 
          (setvar "clayer" ENT1LAY)
          (if (= ENT1TYPE "LINE") 
            (progn 
              (setq PT1  (cdr (assoc 10 ENT1DEF))
                    PT2  (cdr (assoc 11 ENT1DEF))
                    XPT1 (car PT1)
                    XPT2 (car PT2)
              ) ;_ End setq
              (if (> XPT1 XPT2) 
                (setq PTT PT1
                      PT1 PT2
                      PT2 PTT
                ) ;_ End then
              ) ;_ End if
              (cond 
                ((equal INSPT PT1)
                 (setq ANGL1 (angle INSPT PT2)
                       TEST  1
                 )
                ) ;_ End cond 1 for an end inserted block
                ((equal INSPT PT2)
                 (setq ANGL1 (angle INSPT PT1)
                       TEST  2
                 )
                ) ;_ End cond 2 for an end inserted block
                (INSPT
                 (setq ANGL1 (angle PT1 PT2)
                       ANGL2 (- ANGL1 PI)
                 ) ;_ End setq
                ) ;_ End cond for blocks not inserted at endpoints
              ) ;_ End cond
            ) ;_ End then progn
          ) ;_ End if
        ) ;_ End then progn
        (princ "\nThat entity is not a line") ;_ End then progn
      ) ;_ End if
    ) ;_ End else progn
  ) ;_ End if
) ;_ End defun

(defun PIPEINERR (S) 
  (if (/= S "Function cancelled") 
    (princ (strcat "\nError: " S))
  )
  (setvar "osmode" OSM)
  (setvar "clayer" CLAY)
  (setvar "cmdecho" CMD)
  (setq *ERROR* OLDERR)
  (princ)
)