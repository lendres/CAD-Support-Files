; Copyright Lance A. Endres

; Sets the Plot Style for all layouts.
; Arguments
;    PLOTSTYLENAME : String
;        The name of the plot style sheet (ctb or stb).  Example: "Standard - Full Size.stb".

(defun C:SETPLOTSTYLEFORALLLAYOUTS (PLOTSTYLENAME) 
  (vlax-for LAYOUT 
    (vla-get-layouts 
      (vla-get-activedocument 
        (vlax-get-acad-object)
      )
    )
    (vla-put-stylesheet LAYOUT PLOTSTYLENAME)
  )
  (princ)
)