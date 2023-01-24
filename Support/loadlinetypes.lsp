;;; Copyright Lance A. Endres

;;; Run as part of setup.
;;; Can also be run as stand alone.

(defun C:LOADLINETYPES (/ MYLINETYPES) 
  ;;; Line types.
  (foreach MYLINETYPES (list "center" "dashdot" "hidden" "phantom" "dashdot2") 
    (command "_.-linetype" "load" MYLINETYPES "acad.lin" "")
  ) ;_ End foreach
  (foreach MYLINETYPES 
    (list "hidden25" "fin_tube" "dashdot_dpm" "dashed_dpm" "hidden_dpm" "phantom3" 
          "fenceline_dpm" 
          ;;; "vent2" "hot_water" "hot_water_supply" "hot_water_return" "cold_water"
          ;;; "gas" "vent" "sanitary" "non_potable_cw" "non_potable_hw" "non_potable_hwr"
    ) ;_ End list
    (command "_.-linetype" "load" MYLINETYPES "dpm.lin" "")
  ) ;_ End foreach
)