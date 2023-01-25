(vl-load-com)

;;; Written by Frank Oquendo
;;; http://www2.stonemedia.com/franko
;;; Adds a new layoutx with the number x 1 larger than the largest previously created.
(defun AX:ADDLAYOUT (/ DOC LAYOUT) 
  (setq DOC    (vla-get-activedocument (vlax-get-acad-object))
        LAYOUT (vla-add 
                 (vla-get-layouts DOC)
                 (strcat "Layout" (itoa (vla-get-count LAYOUTS)))
               ) ;_ End vla-add
  ) ;_ End setq
  (vla-put-activelayout DOC LAYOUT)
  LAYOUT
) ;_ End defun

;;; Written by Lance A. Endres
;;; Create a new layout and set it current.
(defun MAKE-NEW-LAYOUT (/ ADOC LAYOUT) 
  (setq LAYOUT (GET-NEW-LAYOUT-NAME "Layout" "")
        ADOC   (vla-get-activedocument (vlax-get-acad-object))
        LAYOUT (vla-add (vla-get-layouts ADOC) LAYOUT)
  ) ;_ End setq
  (vla-put-activelayout ADOC LAYOUT)
) ;_ End defun

;;; Written by Tony Tanzillo
;;; http://ourworld.compuserve.com/homepages/tonyt
;;; Adds a new layoutx were x is the first layout number available.
;;; Moddified by Lance A. Endres to allow more flexability for layout names.F
(defun GET-NEW-LAYOUT-NAME (PREFIX SUFFIX / I LAYOUTS NAME) 
  (setq I 0)
  (setq LAYOUTS (vla-get-layouts 
                  (vla-get-activedocument (vlax-get-acad-object))
                ) ;_ End vla-get-layouts
  ) ;_ End setq
  (vl-catch-all-apply 
    '(lambda () 
       (while 
         (vla-item 
           LAYOUTS
           (setq NAME (strcat 
                        PREFIX
                        (itoa (setq I (1+ I)))
                        SUFFIX
                      ) ;_ End strcat
           ) ;_ End setq
         ) ;_ End vla-item
       ) ;_ End while
     ) ;_ End lambda
  ) ;_ End vl-catch-all-apply
  NAME
) ;_ End defun

;;; Written by Frank Oquendo
;;; http://www2.stonemedia.com/franko
;;; Activates the last created layout.
(defun AX:ACTIVATELASTLAYOUT (/ DOC LAYOUTS) 
  (setq DOC     (vla-get-activedocument (vlax-get-acad-object))
        LAYOUTS (vla-get-layouts DOC)
  ) ;_ End setq
  (vla-put-activelayout 
    DOC
    (vla-item LAYOUTS (last (AX:LISTLAYOUTS)))
  ) ;_ End vla-put-activelayout
) ;_ End defun

(defun AX:LISTLAYOUTS (/ LAYOUTS C LST LAY) 
  (setq LAYOUTS (vla-get-layouts 
                  (vla-get-activedocument (vlax-get-acad-object))
                ) ;_ End vla-get-layouts
        C       -1
  ) ;_ End setq
  (repeat (vla-get-count LAYOUTS) 
    (setq LST (cons (setq C (1+ C)) LST))
  ) ;_ End repeat
  (vlax-for LAY LAYOUTS 
    (setq LST (subst (vla-get-name LAY) (vla-get-taborder LAY) LST))
  ) ;_ End vlax-for
  (reverse LST)
) ;_ End defun

;;; Written by Tony Tanzillo
;;; http://ourworld.compuserve.com/homepages/tonyt
;;; Activates the last created layout.
(defun SET-NEW-LAYOUT-CURRENT () 
  (vla-put-activelayout 
    (vla-get-activedocument (vlax-get-acad-object))
    (GET-NEW-LAYOUT)
  ) ;_ End vla-put-activeLayout
) ;_ End defun

(defun GET-NEW-LAYOUT (/ TABS) 
  (vlax-for LAYOUT 
    (vla-get-layouts 
      (vla-get-activedocument (vlax-get-acad-object))
    ) ;_ End vla-get-layouts
    (setq TABS (cons (cons (vla-get-taborder LAYOUT) LAYOUT) TABS))
  ) ;_ End vlax-for
  (cdr (assoc (apply 'max (mapcar 'car TABS)) TABS))
) ;_ End defun

;;; Written by Frank Oquendo
;;; http://www2.stonemedia.com/franko
;;; Toggles the display state of the layout tabs
(defun AX:TOGGLELAYOUTS (/ PREFDISPLAY) 
  (setq PREFDISPLAY (vla-get-display 
                      (vla-get-preferences 
                        (vlax-get-acad-object)
                      ) ;_ End vla-get-Preferences
                    ) ;_ End vla-get-Display
  ) ;_ End setq
  (if (= (vla-get-displaylayouttabs PREFDISPLAY) :vlax-true) 
    (vla-put-displaylayouttabs PREFDISPLAY :vlax-false)
    (vla-put-displaylayouttabs PREFDISPLAY :vlax-true)
  ) ;_ End if
  (princ)
) ;_ End defun

;;; Modified by Lance A. Endres from code by Frank Oquendo.
;;; Returns the value of the display state of the layout tabs
(defun GET-LAYOUTS-DISP-STATE () 
  (vla-get-displaylayouttabs 
    (vla-get-display 
      (vla-get-preferences 
        (vlax-get-acad-object)
      ) ;_ End vla-get-Preferences
    ) ;_ End vla-get-Display
  ) ;_ End vla-get-DisplayLayoutTabs
) ;_ End defun

;;; Modified by Lance A. Endres from code by Frank Oquendo.
;;; Sets the value of the display state of the layout tabs based on which
(defun PUT-LAYOUTS-DISP-STATE (WHICH) 
  (vla-put-displaylayouttabs 
    (vla-get-display 
      (vla-get-preferences 
        (vlax-get-acad-object)
      ) ;_ End vla-get-Preferences
    ) ;_ End vla-get-Display
    WHICH
  ) ;_ End vla-put-displaylayouttabs
) ;_ End defun

;;; Written by Lance A. Endres.
;;; Retrieves the current active layout.
(defun GET-CURRENT-LAYOUT () 
  (vla-get-activelayout 
    (vla-get-activedocument (vlax-get-acad-object))
  ) ;_ End vla-get-activelayout
) ;_ End defun

;;; Written by Lance A. Endres.
;;; Sets a layout active.
(defun PUT-CURRENT-LAYOUT (WHICH) 
  (vla-put-activelayout 
    (vla-get-activedocument (vlax-get-acad-object))
    WHICH
  ) ;_ End vla-put-activelayout
) ;_ End defun