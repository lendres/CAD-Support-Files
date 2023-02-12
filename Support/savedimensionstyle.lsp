(defun SAVEDIMENSIONSTYLE (STYLENAME) 
	; Search for the existing style.
  (setq RESULT (tblsearch "dimstyle" STYLENAME))

  ; Save your dimension style or overwrite if exists:
  (if (= RESULT nil) 
    (command ".dim1" "save" STYLENAME)
    (command ".dim1" "save" STYLENAME "Y")
  )
)