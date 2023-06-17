
;;;===== Raster Image Support for Clipboard Paste Special =====
;;
;; IMAGEFILE
;;
(defun imagefile (filename / filedia-save cmdecho-save)
  (setq filedia-save (getvar "FILEDIA"))
  (setq cmdecho-save (getvar "CMDECHO"))
  (setvar "FILEDIA" 0)
  (setvar "CMDECHO" 0)
  (command "_.-image" "_attach" filename)
  (setvar "FILEDIA" filedia-save)
  (setvar "CMDECHO" cmdecho-save)
  (princ)
)

(defun zw_findfile (zwapp)
  (or (findfile (strcat zwapp ".lsp"))      
      (findfile (strcat zwapp ".exe"))
      (findfile (strcat zwapp ".zrx"))
      (findfile (strcat zwapp ".zel"))
      (findfile (strcat zwapp ".zelx"))
      (findfile (strcat zwapp ".vls"))
      (findfile zwapp)
  )
)

(defun zw_filenotfound (filename)
	(princ (strcat filename "not found!\n"))
)

(defun _autoload (zwapp)
	(load zwapp)
)

(defun autoload (zwapp cmdlinfo / zappf sname)
	(setq zappf (strcat "\"" zwapp "\""))
	(setq initPrompt "\nInitializing...")
	(mapcar
		'(lambda (lcmd / cmdname)
		(progn
			(setq cmdname (strcat "C:" lcmd))
			(if (not (eval (read cmdname)))
				(eval
					(read 
						(strcat
							"(defun " cmdname "( / resret)"
							"(if (zw_findfile " zappf ")"
							"(progn (princ initPrompt)"
							"(_auto" "" "load " zappf ")"
							"(setq resret (" cmdname ")))"
							"(zw_filenotfound " zappf "))"
							"resret)"
						)
					)
				)
			)
		)
		)
	cmdlinfo)
	nil
)

(load "../Express/ZWExpress.lsp")


; Added to support autoload of custom lisp files.
(load "userdoc.lsp")