(defun C:DRAWLOG () 
  (vla-runmacro (vlax-get-acad-object) "drawinglog.dvb!rundrawlog")
  (princ)
) ;_ End defun

(defun C:DRAWLOGSWEXPORT () 
  (vla-runmacro (vlax-get-acad-object) "drawinglog.dvb!SolidWorksExport")
  (princ)
) ;_ End defun

;;;    (vl-vbaload "c:\\custom cad files\\drawinglog.dvb")
;;;    (vl-vbarun "rundrawlog")