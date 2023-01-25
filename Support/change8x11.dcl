PRINTSELECT : dialog {
    label = "Printer Selection";
    : spacer {
        height = 0;
    }
    : row {
        : list_box {
            label = "Select a printer";
            key = "print_sel";
            width = 40;
            height = 8;
            multiple_select = false;
        }
    } /*End row for file area*/
    : spacer {
        height = 0;
    }
    : row {
        spacer_0;
        : button {
            label = "&OK";
            key = "exit";
            is_default = true;
            fixed_width = true;
            width = 12;
        }
        spacer_0;
    }
    errtile;
}