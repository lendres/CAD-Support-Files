LAYRENAME : dialog {
    label = "Auto layer rename";
    : row {
        : list_box {
            label = "Layer conversion &data file";
            key = "data_file";
            width = 50;
            height = 10;
            multiple_select = false;
        }
        : column {
            : spacer {
                height = 1.5;
            }
            : button {
                label = "&File...";
                key = "new_file";
            }
            : button {
                label = "&Remove";
                key = "rem_file";
            }
            : spacer {
                height = 1;
            }
        } /*End column for adding and removing files*/
    } /*End row for file area*/
    : spacer {
        height = 0;
    }
    : row {
        spacer_0;
        spacer_0;
        : button {
            label = "&Convert";
            key = "convert";
            fixed_width = true;
            width = 12;
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