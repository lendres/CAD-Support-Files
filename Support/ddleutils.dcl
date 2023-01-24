LEUTILS : dialog {
    label = "Controls for LE Automatic Functions";
    initial_focus = "rot_ang";

/*Start Column*/
    : column {
        : spacer {
            height = 0.5;
        }
        : boxed_column {
            label = "Automatic Layer Control";
            : toggle {
                label = "Always draw automated functions on designated layer";
                key = "txt_lay";
            }
            : spacer {
                height = 0.01;
            }
            : text {
                key = "txt_lbl";
                label = "Layer for automated functions:";
            }
            : row {
                : button {
                    label = "&Select...";
                    key = "getlayer";
                    fixed_width = true;
                    width = 11;
                }
//                : button {
//                    label = "&Pick...";
//                    key = "picklayer";
//                    fixed_width = true;
//                    width = 11;
//                }
                : edit_box {
                    key = "ld_lay";
                    edit_width = 25;
                    allow_accept = true;
                }
            } /*End row for layer edit box and button*/
            : spacer {
                height = 0.20;
            }
        } /*End column*/
        : boxed_column {
            label = "Leader Control";
            : spacer {
                height = 0.01;
            }
            : toggle {
                label = "Automatically scale to paper space";
                key = "auto_ps";
            }
            : edit_box {
                label = "Text rotation angle:";
                key = "rot_ang";
                edit_width = 10;
                allow_accept = true;
            }
            : spacer {
                height = 0.25;
            }
        } /*End column for leaders*/
    } /*End Column*/
    ok_cancel;
    errtile;
}

// ==================================================

setlayer : dialog {
    subassembly = 0;
    label = "Select Layer";
    initial_focus = "listbox";
    : concatenation {
        children_fixed_width = true;
        key = "clayer";
        : text_part {
            label = "Current Layer: ";
            width = 15;
        }
        : text_part {
            key = "cur_layer";
            width = 35;
        }
    }
    : list_box {
        height = 12;
        key = "list_lay";
        allow_accept = true;
    }
    : row {
        key = "controls";
        : column {
            key = "lname";
            fixed_width = true;
            : edit_box {
                label = "Set Layer Name:";
                mnemonic = "S";
                key = "edit_lay";
                edit_width = 32;
                edit_limit = 217;
                allow_accept = true;
            }
        }
    }
    ok_cancel_err;
}

// ==================================================