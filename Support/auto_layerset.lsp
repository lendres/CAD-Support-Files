;;; Copyright 2000 by Lance A. Endres

(defun AUTO_LAYERSET (/ LD_LAY LYTEST TXT_LAY VL-ADOC VL-LAYER) 
  (vl-load-com)
  (setq LD_LAY  (getcfg "AppData/Leader_Utils/Ld_Lay")
        TXT_LAY (getcfg "AppData/Leader_Utils/Txt_Lay")
        VL-ADOC (vla-get-activedocument (vlax-get-acad-object))
  ) ;_ End setq
  (if (or (null LD_LAY) (= LD_LAY "")) 
    (setq LD_LAY "Text")
  ) ;_ End if
  (if (= TXT_LAY "1") 
    (progn 
      (if (= (tblsearch "layer" LD_LAY) NIL) 
        (setq VL-LAYER (vla-add (vla-get-layers VL-ADOC) LD_LAY))
        (setq VL-LAYER (vla-item (vla-get-layers VL-ADOC) LD_LAY))
      ) ;_ End if
      (vla-put-activelayer VL-ADOC VL-LAYER)
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun