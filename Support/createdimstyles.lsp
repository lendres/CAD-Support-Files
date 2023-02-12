;;; Copyright Lance A. Endres

;;; Run as part of setup.

;;; Revision History
;;; March 8, 2004
;;; Fixed an error in the use of EXPERT mode and eliminated having to have
;;; to calls to add dimstyle depending on if the dimstyle was previously loaded.

(vl-load-com)
(load "savedimensionstyle")

(defun CREATEDIMSTYLES (PAPER_ADJ_FACTOR / INCH2MM ITEM LMBD1 LMBD2 TEMP VL-BLOCK) 

  ;;; If System International units are to be used set the variable to 25.4
  ;;; to convert the values established in inches to millimeters.  If U.S.
  ;;; customary units are to be used, the set the value to 1 to prevent conversion.
  (setq INCH2MM (if (= (getcfg "AppData/Setup_Data/Plot_Unts") "si") 
                  25.4
                  1
                ) ;_ End if
        VL-ADOC (vla-get-activedocument (vlax-get-acad-object))
  ) ;_ End setq

  (if (not (tblsearch "style" "romans")) 
    (C:CREATETEXTSTYLES)
  ) ;_ End if
  (mapcar 
    '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
    (list "dimblk"   ; Closed fill arrowheads
          "dimblk1"  ; Closed fill arrowheads
          "dimblk2"  ; Closed fill arrowheads
          "dimtoh"   ; Text outside horizontal to off
          "dimtih"   ; Text inside horizontal to off
          "dimtad"   ; Vertical position of text above line
          "dimjust"  ; Horizontal position of text centered
          "dimzin"   ; Zero supression to supress feet and not inches
          "dimunit"  ; Dimension unit set based on stored value
          "dimtofl"  ; Force line inside
          "dimtxt"   ; Text height
          "dimgap"   ; Text gap
          "dimasz"   ; Arrowheads size
          "dimsho"   ; Update while dragging dimension on
          "dimexe"   ; Extension line extension
          "dimexo"   ; Extension origin offset
          "dimclrd"  ; Dimension line color by layer
          "dimclre"  ; Dimension extension line color by layer
          "dimclrt"  ; Dimension text by layer
          "dimassoc" ; Dimension to act as single unit
          "dimtxsty" ; Text style
          "dimdec"   ; Dimension precision for primary units
    ) ;_ End list
    (list 
      "."                                            ; (dimblk) Closed fill arrowheads
      "."                                            ; (dimblk1) Closed fill arrowheads
      "."                                            ; (dimblk2) Closed fill arrowheads
      0                                              ; (dimtoh) Text outside horizontal to off
      0                                              ; (dimtih) Text inside horizontal to off
      1                                              ; (dimtad) Vertical position of text above line
      0                                              ; (dimjust) Horizontal position of text centered
      3                                              ; (dimzin) Zero supression to supress feet and not inches
      (atoi (getcfg "AppData/Setup_Data/L_Units"))   ; (dimunit) Dimension unit set based on stored value
      1                                              ; (dimtofl) Force line inside
      (* 0.10 INCH2MM)                               ; (dimtxt) Text height
      (* 0.0625 INCH2MM)                             ; (dimgap) Text gap
      (* (* 0.125 INCH2MM) PAPER_ADJ_FACTOR)         ; (dimasz) Arrowheads size
      1                                              ; (dimsho) Update while dragging dimension on
      (* 0.0625 INCH2MM)                             ; (dimexe) Extension line extension
      (* 0.0625 INCH2MM)                             ; (dimexo) Extension origin offset
      256                                            ; (dimclrd) Dimension line color by layer
      256                                            ; (dimclre) Dimension extension line color by layer
      256                                            ; (dimclrt) Dimension text by layer
      2                                              ; (dimassoc) Dimension to act as single unit
      "romans"                                       ; (dimtxsty) Text style
      (atoi (getcfg "AppData/Setup_Data/Dim_Prec"))  ; (dimdec) Dimension precision for primary units
    ) ;_ End list
  ) ;_ End mapcar

  ;;; Save standard dimstyles
  ;;; For A2K a seperate dimstyle is made for elliptical leaders
  (if (= (substr (getvar "acadver") 1 2) "14")
    ;;; Creation method for R14 (no support for pre-R14).
    (command "_.dimstyle" "s" "DPM")
    ;;; Creation method for R15 and greater.
    (progn
      (vla-setvariable VL-ADOC "dimldrblk" ".") ;_ Sets closed fill arrowheads

      ;;; Create a dimstyle for leaders.
      ;;; ZWCAD doesn't honor the "expert" mode "5", so we have to work around.
      ;(command "_.-dimstyle" "s" "DPM - LL")
			(SAVEDIMENSIONSTYLE "DPM - LL")

      ;;; Create a block for the elliptical leader.  If it exists, delete all the entities
      ;;; in it.  If it doesn't exist create it.  Then add the ellipse to it.
      (if (tblsearch "block" "ellip_lead")
        (progn
          (setq VL-BLOCK (vla-item
                           (vla-get-blocks VL-ADOC)
                           "ellip_lead"
                         ) ;_ End vla-item
          ) ;_ End setq
          (vlax-for ITEM VL-BLOCK (vla-delete ITEM))
        ) ;_ End progn
        (setq VL-BLOCK (vla-add
                         (vla-get-blocks VL-ADOC)
                         (vlax-3d-point (list 0 0 0))
                         "ellip_lead"
                       ) ;_ End vla-add
        ) ;_ End setq
      ) ;_ End if

      ;;; Get the active layer.
      (setq TEMP (vla-get-activelayer VL-ADOC))

      ;;; Set the active layer to 0.  The ellipse will get made on what ever layer is active.
      (vla-put-activelayer
        VL-ADOC
        (vla-item (vla-get-layers VL-ADOC) "0")
      ) ;_ End vla-put-activelayer

      ;;; Add the ellipse to the block.
      (vla-addellipse 
        VL-BLOCK
        (vlax-3d-point (list 0 0 0))
        (vlax-3d-point (list 1 0 0))
        0.5
      ) ;_ End vla-addellipse

      ;;; Restore the active layer to the previous status.
      (vla-put-activelayer VL-ADOC TEMP)

      (vla-setvariable VL-ADOC "dimldrblk" "ellip_lead") ;_ Set leader arrowhead to ell
      (vla-setvariable VL-ADOC "dimasz" (* 0.0625 INCH2MM)) ;_ Adjust arrowhead size for ell

      ;;; Create a dimstyle for elliptical leader
      ;(command "_.-dimstyle" "s" "DPM - ELL")
      (SAVEDIMENSIONSTYLE "DPM - ELL")

      ;;; Reset the dimstyle to leader
      (command "_.-dimstyle" "restore" "DPM - LL")
    ) ;_ End progn
  ) ;_ End if
) ;_ End defun