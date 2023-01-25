XRMOD : dialog {
    label = "Global Xref Path Modifier";

/*Row for extract path*/
    : row {
        : list_box {
            label = "Path to &extract";
            key = "extract_path";
            width = 100;
            height = 5;
            multiple_select = true;
        }
        : column {
            : spacer {
                height = 1;
            }
            : button {
                label = "Browse...";
                key = "find_extract_path";
            }
            : button {
                label = "Add...";
                key = "add_extract_path";
            }
            : button {
                label = "Remove";
                key = "rem_extract_path";
            }
            : spacer {
                height = 1;
            }
        } /*End column for adding and removing files*/
    }/*End row extract path*/

//Row for new path

    : row {
        : list_box {
            label = "&New path";
            key = "new_path";
            width = 100;
            height = 5;
            multiple_select = true;
        }
        : column {
            : spacer {
                height = 1;
            }
            : button {
                label = "Browse...";
                key = "find_new_path";
            }
            : button {
                label = "Add...";
                key = "add_new_path";
            }
            : button {
                label = "Remove";
                key = "rem_new_path";
            }
            : spacer {
                height = 1;
            }
        } /*End column for adding and removing files*/
    } /*End row for new path*/

// Bottom row buttons

    : spacer {
        height = 0;
    }
    : row {
        spacer_0;
        spacer_0;
        : button {
            label = "&Modify";
            key = "modify";
            fixed_width = true;
            width = 12;
            is_default = true;
        }
        spacer_0;
        spacer_0;
        : button {
            label = "&Exit";
            key = "exit";
            is_cancel = true;
            fixed_width = true;
            width = 12;
        }
        spacer_0;
        spacer_0;
    } /*End row for converting or exiting*/
    errtile;
}

// ==================================================

ADD_PATH : dialog {
    label = "Add a path";
    initial_focus = "edit_path";
    : edit_box {
        key = "edit_path";
        edit_width = 60;
        allow_accept = true;
    }
    : spacer {
        height = 0.1;
    }
    ok_cancel;
    errtile;
}

// ==================================================