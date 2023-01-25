;;; Written by Bill Kramer
;;; Published in Cadence Magazine September 1999
;;; "Reactors - Making Aware Programs"

(defun REACTOR_REMOVE (NAME / REACTORSINDWG REACTORGROUP REACTOROBJECT) 
  (vl-load-com)
  (setq REACTORSINDWG (vlr-reactors))
  (foreach REACTORGROUP REACTORSINDWG 
    (foreach REACTOROBJECT (cdr REACTORGROUP) 
      (if (= (vlr-data REACTOROBJECT) NAME) 
        (vlr-remove REACTOROBJECT)
      ) ;_ End if
    ) ;_ End foreach
  ) ;_ End foreach
) ;_ End defun