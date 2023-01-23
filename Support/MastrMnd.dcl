//   MasterMind Dialog Box  Version 2.5
//
//   Filename: MastrMnd.dcl
//
//   (C) Copyright 1999 by Design Point
//   All rights reserved
//
//   This program is copyrighted by Design Point and is licensed
//   to you under the following conditions.  You may not distribute
//   or publish the source code of this program in any form.
//
//   DESIGN POINT PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
//   DESIGN POINT SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MER-
//   CHANTABILITY OR FITNESS FOR A PARTICULAR USE.  DESIGN POINT
//   DOES NOT WARRANT THAT THE OPERATION OF THIS OR PARTS OF THIS
//   PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.
//

// This file to be used in conjuction with MastrMnd.lsp

dp_master_mind : dialog {
   label = "Design Point's MasterMind";
   : row {
      : boxed_row {
         label = "Master";
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_master_a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_master_b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_master_c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_master_d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : button {
         label = "Reveal";
         width = 15;
         fixed_width = true;
         key   = "dp_reveal";
      }
   }
   spacer_1;
   : row {
      : boxed_row {
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib1a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib1b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib1c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib1d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : boxed_row {
         fixed_width = true;
         fixed_height = true;
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib1a";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib1b";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib1c";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib1d";
            is_tab_stop = false;
         }
      }
   }
   : row {
      : boxed_row {
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib2a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib2b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib2c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib2d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : boxed_row {
         fixed_width = true;
         fixed_height = true;
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib2a";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib2b";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib2c";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib2d";
            is_tab_stop = false;
         }
      }
   }
   : row {
      : boxed_row {
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib3a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib3b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib3c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib3d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : boxed_row {
         fixed_width = true;
         fixed_height = true;
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib3a";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib3b";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib3c";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib3d";
            is_tab_stop = false;
         }
      }
   }
   : row {
      : boxed_row {
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib4a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib4b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib4c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib4d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : boxed_row {
         fixed_width = true;
         fixed_height = true;
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib4a";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib4b";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib4c";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib4d";
            is_tab_stop = false;
         }
      }
   }
   : row {
      : boxed_row {
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib5a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib5b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib5c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib5d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : boxed_row {
         fixed_width = true;
         fixed_height = true;
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib5a";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib5b";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib5c";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib5d";
            is_tab_stop = false;
         }
      }
   }
   : row {
      : boxed_row {
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib6a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib6b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib6c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib6d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : boxed_row {
         fixed_width = true;
         fixed_height = true;
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib6a";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib6b";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib6c";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib6d";
            is_tab_stop = false;
         }
      }
   }
   : row {
      : boxed_row {
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib7a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib7b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib7c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib7d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : boxed_row {
         fixed_width = true;
         fixed_height = true;
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib7a";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib7b";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib7c";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib7d";
            is_tab_stop = false;
         }
      }
   }
   : row {
      : boxed_row {
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib8a";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib8b";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib8c";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_guess_ib8d";
            is_tab_stop = false;
         }
      }
      spacer_1;
      : boxed_row {
         fixed_width = true;
         fixed_height = true;
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib8a";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib8b";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib8c";
            is_tab_stop = false;
         }
         : image {
            width = 2;
            height = 1;
            fixed_width = true;
            fixed_height = true;
            color = 9;
            key = "dp_answer_ib8d";
            is_tab_stop = false;
         }
      }
   }

   spacer_1;
   : row {
      : boxed_row {
         label = "Color selection:";
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 0;
            key = "dp_selecta";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 1;
            key = "dp_selectb";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 2;
            key = "dp_selectc";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 3;
            key = "dp_selectd";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 4;
            key = "dp_selecte";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 5;
            key = "dp_selectf";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 6;
            key = "dp_selectg";
            is_tab_stop = false;
         }
         : image_button {
            width  = 4;
            height = 2;
            fixed_width = true;
            fixed_height = true;
            color = 7;
            key = "dp_selecth";
            is_tab_stop = false;
         }
      }
   }

   spacer_1;
   : row {
      alignment = centered;
      fixed_width = true;
      : button {
         label       = "Guess";
         width       = 10;
         fixed_width = true;
         key         = "dpmm_guess";
         mnemonic    = "G";
      }
      : button {
         label       = "Restart";
         width       = 10;
         fixed_width = true;
         key         = "dpmm_restart";
         mnemonic    = "R";
      }
      : button {
         label       = "About";
         width       = 10;
         fixed_width = true;
         key         = "dpmm_about";
         mnemonic    = "A";
      }
      : button {
         label       = "Exit";
         width       = 10;
         fixed_width = true;
         key         = "dpmm_exit";
         mnemonic    = "E";
         is_cancel   = true;
      }
   }
}
dpmm_winner : dialog {
   label = "Winner";
   : boxed_row {
      : paragraph {
         : text_part {
            alignment = centered;
            label     = "Congratulations!";
         }
         : text_part {
            alignment = centered;
            label     = "You're the winner, this time."; }
         : text_part {
            alignment = centered;
            label     = "";
         }
         : text_part {
            alignment = centered;
            label     = "Hope you enjoyed playing";
         }
         : text_part {
            alignment = centered;
            label     = "Design Point's MasterMind";
         }
         spacer_1;
      }
   }
   ok_only;
}

dpmm_about : dialog {
   label = "About Design Point's MasterMind";
   : boxed_row {
      : paragraph {
         : text_part {
            alignment   = centered;
            label       = "Design Point's MasterMind";
         }
         : text_part {
            alignment   = centered;
            label       = "Version 2.5";
         }
         : text_part {
            alignment   = centered;
            label       = "(c) Copyright Design Point 1998";
         }
         : text_part {
            alignment   = centered;
            label       = "All Rights Reserved";
            is_cancel = true;
         }
         spacer_1;
         : text_part {
            alignment   = centered;
            label       = "Visit our web site at";
            is_cancel = true;
         }
         : text_part {
            alignment   = centered;
            label       = "http://www.designpt.com";
            is_cancel = true;
         }
         spacer_1;
         : text_part {
            alignment   = centered;
            label       = "Address comments to luke@designpt.com";
            is_cancel = true;
         }
         spacer_1;
      }
   }
   ok_only;
}
