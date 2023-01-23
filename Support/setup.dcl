SETUP : dialog {
    label = "Drawing Setup";
    initial_focus = "accept";

/*Start Column*/
    : spacer {
        height = 0;
    }

    : boxed_column {
        label = "Required";
        : edit_box {
           label = "Drawing Scale";
           key = "dwg_scale";
           edit_width = 10;
        }
        : spacer {
            height = 0;
        }
        : row {
            : text_part {
                label = " Display Units ";
                key = "display_units_label";
            }
            : spacer {
                width = 23;
            }
            : radio_row {
                : radio_button {
                    label = "Architectural";
                    key = "arch";
                }
                : radio_button {
                    label = "Decimal ";
                    key = "decim";
                }
            }
        } /* End row for display units */
        : row {
            : text_part {
                label = " Plotting Units";
                key = "plot_units_label";
            }
            : spacer {
                width = 23;
            }
            : radio_row {
                : radio_button {
                    label = "U.S.             ";
                    key = "uscust";
                }
                : radio_button {
                    label = "S.I.        ";
                    key = "si";
                }
            }
        } /* End row for plotting units */
        : popup_list {
            label = "Dimension Precision";
            key = "dimprec";
            edit_width = 20;
        }
        : spacer {
            height = 0;
        }
        : toggle {
            label = "Adjust Properties For Paper Writing";
            key = "adj_for_papers";
        }
    } /*End boxed column for requried data*/
    : spacer {
        height = 0;
    }
    : boxed_column {
        label = "Quondam Functions";
        : toggle {
            label = "Set Limits";
            key = "set_lmts";
        }
        : spacer {
            height = 0;
        }
        : row {
            : toggle {
                label = "Import Layout Templates";
                key = "imp_lyts";
            }
            : spacer {
                width = 15;
            }
            : radio_row {
                : radio_button {
                    label = "36\"x24\"";
                    key = "ps36x24";
                }
                : radio_button {
                    label = "42\"x30\"";
                    key = "ps42x30";
                }
            } /*End radio row*/
        } /*End row for layouts*/
    } /*End boxed_column for quondam functions*/
    : spacer {
        height = 0;
    }
    : boxed_column {
        label = "Layers";
        : row {
            : toggle {
                label = "Set layers";
                key = "set_lays";
            }
        }
        : spacer {
            height = 0;
        }
        : row {
            : column {
                : toggle {
                    label = "Genre 01";
                    key = "genre01";
                }
                : toggle {
                    label = "Genre 02";
                    key = "genre02";
                }
            } /*End column for elec*/
            : spacer {
                width = 11;
            }
            : column {
                : toggle {
                    label = "Genre 03";
                    key = "genre03";
                }
                : toggle {
                    label = "Genre 04";
                    key = "genre04";
                }
            } /*End column for mech*/
            : spacer {
                width = 10;
            }
            : column {
                : toggle {
                    label = "Genre 05";
                    key = "genre05";
                    fixed_width = true;
                    width = 5;
                }
                : toggle {
                    label = "Genre 06";
                    key = "genre06";
                    fixed_width = true;
                    width = 5;
                }
            } /*End column for other*/
        } /*End row for layers*/
        : spacer {
            height = 0.25;
        }
        : text {
            label = "Layer data file:";
            key = "lay_label";
        }
        : row {
            : edit_box {
                key = "lay_data";
                edit_width = 55;
            }
            : button {
                label = "B&rowse...";
                key = "brws_lay_data";
                width = 3;
                fixed_width = true;
            }
        } /*End row for layer data file*/
        : spacer {
            height = 0;
        }
    } /*End boxed column*/

    : spacer {
       height = 0.5;
    }
    ok_cancel;
    errtile;
}

// ==================================================