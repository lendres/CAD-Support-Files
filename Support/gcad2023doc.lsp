(load "ai3d.fas")
(load "ai_cylinder.fas")

(defun load_std_3d()
    (defun C:BOX() (C:AI_BOX))
    (defun C:CONE() (C:AI_CONE))
    (defun C:DISH() (C:AI_DISH))
    (defun C:DOME() (C:AI_DOME))
    (defun C:WEDGE() (C:AI_WEDGE))
    (defun C:TORUS() (C:AI_TORUS))
    (defun C:SPHERE() (C:AI_SPHERE))
    (defun C:PYRAMID() (C:AI_PYRAMID))
    (defun C:CYLINDER() (C:AI_CYLINDER))
)

(if (= (InnercheckVer) "Un3DModel")
    (load_std_3d)
)

(defun LoadGcadLsp(/ strFile)
	(setq strFile (findfile "gcad.lsp"))
	(if (/= strFile nil)
		(progn
			(vl-load-com)
			(load strFile)
		)
	)
)

(LoadGcadLsp)

(load "dyjt.lsp")
(load "sp2pl.lsp")
(load "sptpl.lsp")
(load "alignspace.fas")
(vl-load-com) (princ)

; Added to support autoload of custom lisp files.
(load "userdoc.lsp")

(princ)