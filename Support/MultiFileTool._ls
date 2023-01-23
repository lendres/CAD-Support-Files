; MultiFileTool - build: 031011
; ==============================================================================
;                          *** AutoCAD MFT v3.1 ***
;                 Copyleft ;) 2004-2020 | Roland Rothenhöfer
; ==============================================================================
;     PROJECT : AutoCAD Multi File Tool (MFT)
;    FUNCTION : Run a LISP file/function, macro, script and/or command on a
;               selection of AutoCAD drawings
;     VERSION : 3.1
;   LAST EDIT : 27th June 2020 | 23:57 CEST
;      AUTHOR : Roland Rothenhöfer (roland.r71@gmail.com)
;    REQUIRES : DOSlib
; ==============================================================================
;  This program is free software. You can redistribute it and/or modify it under
;  the terms of the GNU General Public License as published by the Free Software
;  Foundation; either version 2 of the License, or any later version.
;  To read the license please visit : http://www.gnu.org/copyleft/gpl.html
; ==============================================================================
;
; Description:
; ------------
; For processing multiple drawings - To select & manipulate a list of files for
; processing, in any way possible. Select your lisp file and/or function to be
; loaded/run with each drawing. (or) Add some script/macro/commands or short &
; simple lispcode (e.g. setting a var, calling a function) to run. You can even
; paste the code into the editor or import text based files like .scr
;
; Realtime checking for the fileopen function allows for retries & skipping files
; during the process. Filenames which get skipped during the process can
; optionally be logged. Its possible to import a logfile, in case you need to
; retry those files.
;
; MFT offers extensive options for selecting files. incl. (all) current, a single
; directory or whole directory trees, based on a wilcard. You can even write an
; import filter to get the filenames from virtually any source. (DWG & DXF)
;
; It can remember a list. You can save/load lists, and even use comments (;) to
; exclude files inside listfiles. Besides that it can warn for Read-only files,
; remove _recover files, sort list of filenames and allow/prevent duplicates.
;
; You can edit the list by moving files up and down the list or select & remove
; file(s) from the (displayed) list.
;
; Options:
; --------
; - scan and warn for Read-only files
; - remove _recover files
; - Extensive options for file selection.
; - sort list of filenames
; - save/load lists
; - autosave & load (remember) list
; - allow/prevent duplicate filenames*
; - Supports custom made import functions to get lists from anywhere you like
; - Uses realtime checking for fileopen function
;   Allowing for user action or skipping files during the process.
;
; * = By adding the same directory twice, you can process each file twice
;     which might be usefull if you need to collect data first & write back
;     info later. (example: crossreference information)
;
; This allows to do virtually anything to a selection of AutoCAD drawings.
;
; Note:
; -----
; If running this file on a network, be sure to set the path for data & ini files
; to a local or user specific network path for each user to have his/her own config.
; If set to a shared network path all users will share the same config & data
; files, which wil result in conflicts!
;
; How it works:
; -------------
; MFT turns off the reinitialisation of lisp in order to function.
; LISPINIT 0, and uses the fileopen command to open (& close) files.
; This allows the function to remain loaded, while processing all the drawings,
; sequential. (much like a script (*.scr))
;
; The file selection happens inside the MFT_getFiles function which you can wrap
; inside your own function(s). When called, it will return a list of files.
; It takes 1 argument, which can be several things:
; nil - This will open the file selector with an empty list, or a saved one.
; (setq list (MFT_getFiles nil))
; ("c:/dwg/drawing1.dwg" "c:/dwg/drawing2.dwg")
;
; (setq list (MFT_getFiles [name of import filter]))
; Will return a list of files using the specified import filter, bypassing the dialog
; 
; (setq list ([dir.name] [recursive "1" yes "0" no] [wildcard "*"] [filetype "1" dwg, "2" dxf "3" both])
; (MFT_getfiles (list "c:/path/to/import/" "1" "*" "1"))
; Will return a list of dwg files from supplied directory tree, bypassing the dialog
;
; The processing of files & code is done inside MFT_Process. This function can also
; be wrapped inside your own functions. It will expect a list of files and a list of
; script/commands.
; (MFT_Process [list with files] [list with commands/arguments])
; e.g:(MFT_Process (list "c:/dwg/drawing1.dwg" "c:/dwg/drawing2.dwg") (list "zoom" "e"))
;
; It is possible to completely bypass the main & file selection dialogs:
; (MFT_Process (MFT_getFiles (list "c:/path/to/import/" "1" "*" "1")) (list "zoom" "e"))
; This will start the process to zoom extent all dwg's within the supplied directory tree.
;
; For opening files MFT uses MFT_Open which will do all needed checks before calling
; the fileopen command, to ensure a smooth process. It is possible to use this function
; on its own, even in conjunction with a script. It will return T if succesfull (file has
; been opened), nil if not.
;
; - Checks if the file really exists
; - Checks if the file doesn't require a higher acad version
; - Checks if the current file needs to, and can, be saved
; - Ignores new-unsaved drawings (closed without saving)
; - Open (& Close) writeable files
; - Open (& Close) Read-Only files
; - Open (& Close) Open (other user:network) files. (read-only)
;
; - Option to warn user about missing files (dialog closes after 10 sec.)
; - Option to warn user about Read-Only & Open(network) files and ask user to skip, retry or open Read-Only
; - Option to automatically skip Read-Only & Open(network) files (open read-only is default)
; - Option to log files which where not processed.
; - Option to ignore changes (no saving)
;
; MFT will generate all files it needs to operate.  Including:
; - MFT.dcl           : DCL file containing the main and fileselection dialogs
; - MFT_info.txt      : First generated when clicking the "info" button
; - DATA/MFT.ini      : INI file for saving all settings/user preferences
; - DATA/MFT.scr      : File holding the manually added code to run
; - DATA/MFT_log.csv  : CSV file used by default for logging skipped files (user selectable)
; - DATA/MFTFiles.lst : Filelist file, for storing active list of files
; - IMPORT/Import MFT logfile.lsp : An example import filter, which will allow to import files from a log file 
;
; Version history:
; ----------------
; 3.0     - this is a complete rewrite from earlier work (using a *.scr for processing)
; 3.1     - MFT can now find itself and use the found location, if its inside search path
;         - Improved processing of entered script
;         - Version check for MFT files added, for easy updating
;         - Changed/Added various info
;         - No longer turns SDI on
;         - added version check for drawings.vs.acad
;         - Fixed a few bugs
;
; Credits:
; --------
; - AfraLisp, for the great info (Tutorials & Manuals etc.) on AutoLISP
;
; - Lee Mac, for all his work: tutorials, examples, functions, etc. Respect!
;
; - to Robert McNeel & Associates for creating the DOSlib ARX
;   MFT would not have existed without it.
;
; - Some code (and much knowledge) from AutoDesk's Visual LISP, AutoLISP And General
;   Customization forum, & for that...
;   Thanks! to all the people who give the answers and sample code ;)
;   Most notably to:
;    - John Uhden, for his help on making the script part function like a charm.
;    - Lee Mac, for his excelent list box functions, which helped to get it working
;    - scot-65, for enhancing/reviewing the dcl code
;    - CodeDing, for his help on detecting and handling lisp
;    - CADffm & mrrushcadtech for testing!
;
; ==============================================================================
; DOSlib functions used:
; ------------------------------------------------------------------------------
; DOS_DIR         : Returns list with filenames "file.ext"
; DOS_DIRP        : Checks if directory exists
; DOS_DIRTREE     : Returns list with directory tree from given starting directory
; DOS_EDITBOX     : Dialog to edit multiline text string (can import ascii file), returns new string
; DOS_EDITLIST    : Dialog to edit list items (add, delete, change order), returns new list
; DOS_FILE        : Returns list with file attributes. Read-Only, Archive, Hidden etc. nil on error
; DOS_FILEP       : Checks if file exists. Returns T when found
; DOS_GETDIR      : Dialog to select directory. Returns path "D:\path\"
; DOS_GETFILED    : Allows user to browse for a file. Returns path&filename "Drive:\path\to\file.ext"
; DOS_GETINI      : Returns value from a INI file
; DOS_GETPROGRESS : Create & adjust progress indication bar
; DOS_MKDIR       : MaKe directory
; DOS_MSGBOX      : Creates and displays a custom dialog, with 6 button pre-sets & optional timer
; DOS_MSGBOXEX    : Creates and displays a custom dialog, with custom buttons
; DOS_OPENP       : Checks if file open. Returns T if so.
; DOS_PWDIR       : Returns Programs Working Directory
; DOS_SETINI      : Writes value to a INI file
; DOS_SHELLEXE    : Starts an executable and loads a file
; DOS_UNCPATH     : Returns UNC path name : (DOS_UNCPATH "P:\\Drawing1.dwg") returns "\\\\cadserver\\projects\\drawings\\drawing1.dwg"
; DOS_VERSION     : Returns the DOSlib version number
; DOS_WAITCURSOR  : Changes mouse cursor into sandclock (& back)
; ==============================================================================
;                           *** MultiFileTool ***
; ==============================================================================

(vl-load-com)

; =============================== MFT_Config ===================================

(defun MFT_Config ( rw / file_w DLpath MFT_nfo fndPath)

   ; rw = R or W, Read or Write
   ; returns T on succesfull config

; --- CONFIG -------------------------------------------------------------------

   ; Function for main configuration of MFT.
   ; It can be set to R, read, for setting up the configuration incl. all checks,
   ; or it can be set to W, write, for writing/saving the current settings.

   ; Here you can set various paths to be used by MFT.
   ; By default it is set to use subdirectories from the main MFT path set.
   ; But you can fully customize these paths.
   ; Note that it will try to locate itself, before using the supplied path.

   ; In case you are using a shared network location, you should set the location
   ; of the DATA directory and MFT.ini file to a local path, or use a user specific
   ; network location, to avoid conflicts.

   ; This function will check all path's, DOSlib & support files.
   ; It will create all subdirs & files needed, such as:
   ; subdir ./DATA for saving filelists & ini file
   ; subdir ./IMPORT for custom import filters
   ; MFT.ini, containing all user preferences
   ; MFT.dcl, dialogs used by MFT
   ; Import MFT logfile.lsp, Import filter for log files

; --- setDefaults --------------------------------------------------------------

   (defun setDefaults ( / )

   ; *******************************************
   ; *** start of user configurable settings ***
   ; *******************************************

      ; Note: MFT will first try to find itself within the searchpath

      ; Set path to this file - NOTE: use either \\ or / instead of \
      ; Only required in case its outside the acad search paths
      (setq MFT_files "c:/Lisp/MFT/")

      ; NOTE: Its NOT recommended to change the MultiFileTool.lsp filename, but
      ;       if you must, set the correct name here too, or it will fail
      (setq MFT_lsp "MultiFileTool.lsp")

      ; Optional path to be used to find/load doslib
      (setq DLpath "c:/Lisp/DOSLIB/")

      ; Path for data files. Must be writeable (see Note in header!)
      ; Used for storing logfile & filelist
      (setq MFT_data (strcat MFT_files "DATA/"))

      ; Paths to the MFT.dcl dialog file.
      (setq MFT_dcl (strcat MFT_files "MFT.dcl"))

      ; Paths to the MFT.ini config file. Must be writeable
      (setq MFT_ini (strcat MFT_data "MFT.ini"))

      ; Paths to MFT import filters.
      (setq MFT_import (strcat MFT_files "IMPORT/"))

      ; Command line comments (0 = off, 1 = basic, 2 = extended)
      (setq comLineNfo 0)

      ; All default settings used for MFT
      ; (ONLY!) the first time you run MFT or the INI file can't be found, these settings are used.
      ; It will create an ini file using these settings. The ini file will be used afterwards.
      (if (not pathname)        (setq pathname      "c:\\"))                    ; start dir
      (if (not user_path)       (setq user_path     pathname))                  ; same as above
      (if (not wildcard)        (setq wildcard      "*"))                       ; all files
      (if (not filter)          (setq filter        "0"))                       ; not filtered
      (if (not filetype)        (setq filetype      "1"))                       ; DWG only
      (if (not recursive)       (setq recursive     "1"))                       ; include subdirs
      (if (not noDuplicate)     (setq noDuplicate   "1"))                       ; no duplicates allowed
      (if (not noRecover)       (setq noRecover     "1"))                       ; remove '_recover' files
      (if (not autosort)        (setq autosort      "0"))                       ; no automatic sorting
      (if (not autosave)        (setq autosave      "0"))                       ; clean list at each start
      (if (not lispFile)        (setq lispFile      " "))                       ; lisp filename
      (if (not loadLisp)        (setq loadLisp      "0"))                       ; no lisp loaded with drawings
      (if (not alwaysLoad)      (setq alwaysLoad    "0"))                       ; Load lisp with each drawing
      (if (not loadScr)         (setq loadScr       "1"))                       ; execute command(s)
      (if (not logFile)         (setq logFile (strcat MFT_data "MFT_log.csv"))) ; logfile name
      (if (not logSkip)         (setq logSkip       "1"))                       ; log skipped files
      (if (not warn_notfound)   (setq warn_notfound "1"))                       ; warn
      (if (not warn_fileopen)   (setq warn_fileopen "1"))                       ; warn
      (if (not warn_readonly)   (setq warn_readonly "1"))                       ; warn
      (if (not skip_readonly)   (setq skip_readonly "0"))                       ; no skip
      (if (not skip_savefile)   (setq skip_savefile "0"))                       ; save
      (if (not progBar)         (setq progBar       "1"))                       ; show progbar

      ; *****************************************
      ; *** end of user configurable settings ***
      ; *****************************************

   )

; --- loadDOSlib ---------------------------------------------------------------

   (defun loadDOSlib (path / relNumA CPUxBit libName envFile usrFile loaded)
      ; Function to load DOSlib
      ; path = "optional" alternative path (you can set it to nil)
      ; returns: T if succesful, nil if not.
      ; Checks if DOSlib is loaded, if not it will determine the name for the
      ; correct arx to load for your system. It will try to find the file within
      ; the AutoCAD search path or an optional alternative location (if set).
      
      (if DOS_VERSION
         (setq loaded T)                                                        ; it is already loaded
      ; else, we need to load it
         (progn
            ; --- Get info & determine arx name
            (setq relNumA (atoi (substr (getvar "acadver") 1 2))                ; ACAD release nr
                  CPUxBit (if (wcmatch (getenv "PROCESSOR_ARCHITECTURE") "*64") "x64" "") ; Processor arc.
                  libName (strcat "doslib" (itoa relNumA) CPUxBit)              ; construct doslib filename
                  envFile (findfile (strcat libName ".arx"))                    ; check if inside search paths
                  usrFile (if path (findfile (strcat path libName ".arx")) nil) ; opt. check for user path
            )
            ; --- Check & load
            (cond
               ; file not found
               ((and (not envFile)(not usrFile))
                  (if path (setq path (strcat "( " path " ) ")))
                  (princ (strcat "\nError: Couldn't locate " path libName ".arx"))
               )
               ; ACAD R14 or older versions
               ((< relNumA 15)
                  (princ "\nDinosaur: ACAD 2000 or higher is required")
               )
               ; R15 & up (32 & 64 bit)
               ((> relNumA 14)
                  (if envFile (arxload libName))                                   ; Load found doslib
                  (if (and usrFile (not envFile)) (arxload (strcat path libName))) ; Load doslib from supplied path
                  (setq loaded T)                                                  ; it should be loaded now
               )
            )
         )
      )
      loaded
   )

; --- read_ini -----------------------------------------------------------------

   (defun read_ini ( / )
      ; function to read settings from .ini file
      (if (> comLineNfo 1)(princ "\nReading settings from ini file "))
      (setq lispFile      (DOS_GETINI "config" "lispFile"      MFT_ini)         ; path and name of lisp to load
            logFile       (DOS_GETINI "config" "logFile"       MFT_ini)         ; path and location of log file
            pathname      (DOS_GETINI "config" "lastdir"       MFT_ini)         ; last used path (add dir.)
            user_path     (DOS_GETINI "config" "userpath"      MFT_ini)         ; user path (for add 'all' files)
            progBar       (DOS_GETINI "config" "progBar"       MFT_ini)         ; progess bar on/off
            logSkip       (DOS_GETINI "config" "logSkip"       MFT_ini)         ; log skipped files yes/no
            warn_notfound (DOS_GETINI "config" "warn_notfound" MFT_ini)         ; warn user if not found
            warn_fileopen (DOS_GETINI "config" "warn_fileopen" MFT_ini)         ; warn if the file is open (by other)
            warn_readonly (DOS_GETINI "config" "warn_readonly" MFT_ini)         ; warn if it is read-only
            skip_readonly (DOS_GETINI "config" "skip_readonly" MFT_ini)         ; skip read-only files
            skip_savefile (DOS_GETINI "config" "skip_savefile" MFT_ini)         ; do not save files
            wildcard      (DOS_GETINI "config" "wildcard"      MFT_ini)         ; MS-winDOwS style wildcard
            recursive     (DOS_GETINI "config" "recursive"     MFT_ini)         ; Boolean : include subdirs (y/n)
            autosort      (DOS_GETINI "config" "autosort"      MFT_ini)         ; Boolean : autosort (y/n)
            autosave      (DOS_GETINI "config" "autosave"      MFT_ini)         ; Boolean : autosave (y/n)
            noDuplicate   (DOS_GETINI "config" "noDuplicate"   MFT_ini)         ; Boolean : remove duplicates (y/n)
            noRecover     (DOS_GETINI "config" "noRecover"     MFT_ini)         ; Boolean : remove '_recover' files (y/n)
            filetype      (DOS_GETINI "config" "filetype"      MFT_ini)         ; Integer : 1 = DWG, 2 = DXF, 3 = Both
            loadLisp      (DOS_GETINI "config" "loadLisp"      MFT_ini)         ; load lisp file yes/no
            alwaysLoad    (DOS_GETINI "config" "alwaysLoad"    MFT_ini)         ; always load lisp file
            loadScr       (DOS_GETINI "config" "loadScr"       MFT_ini)         ; execute script code
      )
   )

; =============================== MAIN SEGMENT =================================

   (cond
      ((= (strcase rw) "R")
         ; Set all default settings
         (if (> comLineNfo 0)(prompt "\nSet Configuration"))
         (setDefaults)

         ; Check for and/or try to load DOSlib
         (if (> comLineNfo 1)(prompt "\nCheck for DOSlib"))
         (if (not (loadDOSlib DLpath))
            (progn
               (alert "This function requires DOSlib to run. Please Download & install DOSlib, or supply the correct path.")
               (exit)
            )
         )

         ; Find MFT location, or check if path to this file was set
         ; If the MFT.lsp was found within search path, we use the found location
         ; As the other locations can be customized, we check if thats the case
         ; if not we set it to default, using the found location.
         (if (> comLineNfo 1)(prompt "\nCheck MFT location"))
         (if (setq fndPath (findfile MFT_lsp))
            (progn
               (setq fndPath (substr fndPath 1 (- (strlen fndPath) (strlen MFT_lsp))))
               (if (> comLineNfo 1)(prompt (strcat "\nMFT found at: " fndPath)))
               (if (/= (parsepath fndPath)(parsepath MFT_files))
                  (progn
                     (if (= MFT_data   (strcat MFT_files "DATA/"))  (setq MFT_data   (strcat fndPath "DATA/")))
                     (if (= MFT_import (strcat MFT_files "IMPORT/"))(setq MFT_import (strcat fndPath "IMPORT/")))
                     (if (= MFT_dcl    (strcat MFT_files "MFT.dcl"))(setq MFT_dcl    (strcat fndPath "MFT.dcl")))
                     (if
                        (and
                           (= MFT_data   (strcat MFT_files "DATA/"))
                           (= MFT_ini    (strcat MFT_data "MFT.ini"))
                        )
                        (setq MFT_ini    (strcat fndPath "DATA/MFT.ini"))
                     )
                     (setq MFT_files  fndPath)
                  )
               )
            )
         )
       
         ; if we can't find it within searchpath and the supplied path is incorrect
         ; we can not continue
         (if (and (not fndPath)(not (findfile (strcat MFT_files MFT_lsp))))
            (progn
               (alert (strcat "\nError: Incorrect Path set: " MFT_files MFT_lsp "\nPlease revise the path to this file set within init section, or add it inside the acad search path."))
               (exit)
            )
         )

         ; Check if DATA directory exists, if not create it
         (if (> comLineNfo 1)(prompt "\nCheck for DATA directory"))
         (if (not (DOS_DIRP MFT_data))
            (DOS_MKDIR MFT_data)
         )

         ; Retrieve MultiFileTool.lsp buildnr (for checking dcl version)
         (setq file_r  (open (strcat MFT_files MFT_lsp) "r"))
         (setq line (read-line file_r))
         (close file_r)
         (setq buildNr (atoi (substr line 10 4)))

         ; Read settings from ini file, if it exists, create it using defaults if not
         (if (> comLineNfo 1)(prompt "\nCheck for ini file"))
         (if (not (findfile MFT_ini))
            (progn
               ; Write .ini template
               (if (> comLineNfo 1)(prompt "\nCollect header"))
               (setq file_r (open (strcat MFT_files MFT_lsp) "R"))
               (setq header (read-line file_r))
               (repeat 4
                  (setq header (strcat header "\n" (read-line file_r) ))
               )             
               (close file_r)
               (if (> comLineNfo 1)(prompt "\nCreate ini file"))
               (setq file_w (open MFT_ini "W"))
               (write-line header file_w)
               (write-line "; Configuration file for MultiFileTool (MFT)" file_w)
               (write-line ";" file_w)
               (write-line "; ------------------------------------------------------------------------------" file_w)
               (write-line "[config]" file_w)
               (write-line "; ------------------------------------ END -------------------------------------" file_w)
               (close file_w)
               ; Fill it up with all the default settings
               (write_ini)
            (setq MFTwelcome T)
            )
            (progn
               ; If there is one already, read it
               (if (> comLineNfo 1)(prompt "\nRead ini file"))
               (read_ini)
            )
         )
         ; Check if dcl file present
         (if (> comLineNfo 1)(prompt "\nCheck for DCL file"))
         (if (findfile MFT_dcl)
            (progn
               (setq file_r  (open MFT_dcl "r"))
               (setq line (read-line file_r))
               (close file_r)
               (if (= (substr line 4 26) "MultiFileTool.lsp - Build:")
                  (if (< (atoi (substr line 11 6)) (atoi buildNr))
                     (progn
                        (if (> comLineNfo 1)(princ (strcat "\nDelete old DCL build " (substr line 11 4) " vs " buildNr)))
                        (vl-file-delete MFT_dcl)
                     )
                  )
                  (progn
                     (if (> comLineNfo 1)(princ "\nDelete old DCL"))
                     (vl-file-delete MFT_dcl)
                  )
               )
            )
         )
         (if (not (findfile MFT_dcl))
            (progn
               (if (> comLineNfo 1)(princ "\nCreate (new) DCL file"))
               (setq file_r (open (strcat MFT_files MFT_lsp) "R"))
               (setq header (strcat "//" (substr (read-line file_r) 2)))
               (repeat 17
                  (setq header (strcat header "\n//" (substr (read-line file_r) 2)))
               )             
               (close file_r)
               (setq file_w (open MFT_dcl "W"))
               (write-line header file_w)
               (write-line "//     CREDITS : scot-65 (AutoDesk forum VLALGC) for reviewing the dcl code" file_w)
               (write-line "// =============================================================================" file_w)
               (write-line "//                     *** MFT.dcl : Dialogs for MFT ***" file_w)
               (write-line "// -----------------------------------------------------------------------------" file_w)
               (write-line "" file_w)
               (write-line "swh0   :spacer   {width=0.0;  height=0.0;}" file_w)
               (write-line "swh1   :spacer   {width=1.0;  height=1.0;}" file_w)
               (write-line "but10  :button   {width=10.0;}" file_w)
               (write-line "but12  :button   {width=12.0;}" file_w)
               (write-line "but16  :button   {width=16.0;}" file_w)
               (write-line "tog16  :toggle   {width=16.0;}" file_w)
               (write-line "tog32  :toggle   {width=32.0;}" file_w)
               (write-line "edi56  :edit_box {width=56.0;}" file_w)
               (write-line "rfwt   :row      {fixed_width=true;}" file_w)
               (write-line "rfwtac :row      {fixed_width=true; alignment=centered;}" file_w)
               (write-line "" file_w)
               (write-line "// -----------------------------------------------------------------------------" file_w)
               (write-line "// MFT: Dialog for setting options" file_w)
               (write-line "// -----------------------------------------------------------------------------" file_w)
               (write-line "" file_w)
               (write-line "MFT_Dia" file_w)
               (write-line ":dialog {label=\"MFT: Multi File Tool v3.1, by Roland.R71\";" file_w)
               (write-line "   swh0;" file_w)
               (write-line "   :text_part {key=\"caption\"; alignment=centered; height=3;}" file_w)
               (write-line "   swh0;" file_w)
               (write-line "   :boxed_column {label=\" Processing Options \";" file_w)
               (write-line "      :row {" file_w)
               (write-line "         :tog32 {key=\"loadLisp\"; label=\" Load AutoCAD LISP (*.lsp)\";}" file_w)
               (write-line "         :row   {key=\"llsp\";" file_w)
               (write-line "            :edi56 {key=\"lispFile\";}" file_w)
               (write-line "            :but12 {key=\"browselsp\"; label=\"&Browse\";}" file_w)
               (write-line "         }" file_w)
               (write-line "      }" file_w)
               (write-line "      :row {" file_w)
               (write-line "         :tog32 {key=\"loadScr\"; label=\" Run AutoCAD Commands\";}" file_w)
               (write-line "         :row   {key=\"lscr\";" file_w)
               (write-line "            :edi56 {key=\"scrFile\"; is_enabled=1;}" file_w)
               (write-line "            :but12 {key=\"editScr\"; label=\"Edi&t\";}" file_w)
               (write-line "         }" file_w)
               (write-line "      }" file_w)
               (write-line "      :row {" file_w)
               (write-line "         :tog32 {key=\"logSkip\"; label=\" Log Skipped Filename(s)\";}" file_w)
               (write-line "         :row   {key=\"lfil\";" file_w)
               (write-line "            :edi56 {key=\"logFile\";}" file_w)
               (write-line "            :but12 {key=\"browselog\"; label=\"Bro&wse\";}" file_w)
               (write-line "         }" file_w)
               (write-line "      }" file_w)
               (write-line "      swh0;" file_w)
               (write-line "      :rfwtac {" file_w)
               (write-line "         :column {" file_w)
               (write-line "            :tog32 {key=\"warn_notfound\"; label=\" Warn if file not found\";}" file_w)
               (write-line "            swh0;" file_w)
               (write-line "            :tog32 {key=\"warn_fileopen\"; label=\" Warn if file open\";}" file_w)
               (write-line "         }" file_w)
               (write-line "         :column {" file_w)
               (write-line "            :tog32 {key=\"warn_readonly\"; label=\" Warn if file is Read-Only\";}" file_w)
               (write-line "            swh0;" file_w)
               (write-line "            :tog32 {key=\"skip_readonly\"; label=\" Skip Read-Only (or open) files\";}" file_w)
               (write-line "         }" file_w)
               (write-line "         :column {" file_w)
               (write-line "            :tog32 {key=\"skip_savefile\"; label=\" Don't save changes to files\";}" file_w)
               (write-line "            swh0;" file_w)
               (write-line "            :tog32 {key=\"progBar\";       label=\" Show progress bar\";}" file_w)
               (write-line "         }" file_w)
               (write-line "      }" file_w)
               (write-line "      swh0;" file_w)
               (write-line "   }" file_w)
               (write-line "   swh0;" file_w)
               (write-line "   :row {" file_w)
               (write-line "      swh1;" file_w)
               (write-line "      :rfwt {" file_w)
               (write-line "         :text {label=\"0\"; key=\"num_dwgs\"; width=6;}" file_w)
               (write-line "         :text {label=\"file(s) selected.\"; width=24;}" file_w)
               (write-line "      }" file_w)
               (write-line "      swh1;" file_w)
               (write-line "      :rfwt {" file_w)
               (write-line "         :but12 {key=\"select\"; label=\"&Select files\"; is_default=true;}" file_w)
               (write-line "         :but12 {key=\"accept\"; label=\"&Process\"; is_enabled=1;}" file_w)
               (write-line "         :but12 {key=\"cancel\"; label=\"&Cancel\"; is_cancel=true;}" file_w)
               (write-line "         :but12 {key=\"info\";   label=\"&Info\";}" file_w)
               (write-line "      }" file_w)
               (write-line "   }" file_w)
               (write-line "   swh0;" file_w)
               (write-line "}" file_w)
               (write-line "" file_w)
               (write-line "// -----------------------------------------------------------------------------" file_w)
               (write-line "// MFT: Dialog for selecting files" file_w)
               (write-line "// -----------------------------------------------------------------------------" file_w)
               (write-line "" file_w)
               (write-line "MFT_getFiles" file_w)
               (write-line ":dialog {label=\"MFT: Multi File Tool v3.1, by Roland.R71\";" file_w)
               (write-line "   swh0;" file_w)
               (write-line "   :text_part {key=\"caption\"; alignment=centered; height=3;}" file_w)
               (write-line "   swh0;" file_w)
               (write-line "   :row { label=\" File List Options \";" file_w)
               (write-line "      :but10 {key= \"load_lst\";    label=\"&Load\";}" file_w)
               (write-line "      :but10 {key= \"impt_lst\";    label=\"&Import\";}" file_w)
               (write-line "      :but10 {key= \"save_lst\";    label=\"&Save\";}" file_w)
               (write-line "      :but10 {key= \"clr_lst\";     label=\"&Clear\";}" file_w)
               (write-line "      :tog16 {key= \"autosave\";    label=\"Remember\";}" file_w)
               (write-line "      :tog16 {key= \"autosort\";    label=\"Auto order\";}" file_w)
               (write-line "      :tog16 {key= \"noRecover\";   label=\"No '_recover'\";}" file_w)
               (write-line "      :tog16 {key= \"noDuplicate\"; label=\"No duplicates\";}" file_w)
               (write-line "   }" file_w)
               (write-line "   swh0;" file_w)
               (write-line "   :row {label=\" Edit list \";" file_w)
               (write-line "      :column {" file_w)
               (write-line "         :list_box {key=\"filelist\"; list=\"< select files >\"; width=98; multiple_select=true;}" file_w)
               (write-line "      }" file_w)
               (write-line "      :column {" file_w)
               (write-line "         :but16 {key=\"top\";      label=\"Top\";}" file_w)
               (write-line "         :but16 {key=\"up\";       label=\"Up\";}" file_w)
               (write-line "         :but16 {key=\"down\";     label=\"Down\";}" file_w)
               (write-line "         :but16 {key=\"bottom\";   label=\"Bottom\";}" file_w)
               (write-line "         :but16 {key=\"remove\";   label=\"Remove\";}" file_w)
               (write-line "         :but16 {key=\"sort_lst\"; label=\"&Order\";}" file_w)
               (write-line "         :spacer {height = 2; }" file_w)
               (write-line "         :but16 {key=\"browse\";   label=\"Select &file(s)\";}" file_w)
               (write-line "         :but16 {key=\"addcur\";   label=\"Add current\";}" file_w)
               (write-line "         :but16 {key=\"addopen\";  label=\"Add all open\";}" file_w)
               (write-line "      }" file_w)
               (write-line "   }" file_w)
               (write-line "   swh0;" file_w)
               (write-line "   :row {label = \" Add files from directory \";" file_w)
               (write-line "      :column {" file_w)
               (write-line "         :row {" file_w)
               (write-line "            :edit_box {key=\"user_path\";  value=\"< select directory >\"; width=60; is_enabled=1;}" file_w)
               (write-line "            :but16    {key=\"browse_dir\"; label=\"Select &Dir.\";}" file_w)
               (write-line "            :but16    {key=\"add_lst\";    label=\"&Add all files\";}" file_w)
               (write-line "            :tog16    {key=\"recursive\";  label=\"Recursive\";}" file_w)
               (write-line "         }" file_w)
               (write-line "         :rfwt {" file_w)
               (write-line "            :edit_box {key=\"wildcard\"; label=\"Filename filter\"; width=40;}" file_w)
               (write-line "            :radio_row {" file_w)
               (write-line "               :radio_button {key=\"DWG\"; label=\"DWG\";}" file_w)
               (write-line "               :radio_button {key=\"DXF\"; label=\"DXF\";}" file_w)
               (write-line "               :radio_button {key=\"all\"; label=\"Both\";}" file_w)
               (write-line "            }" file_w)
               (write-line "         }" file_w)
               (write-line "      }" file_w)
               (write-line "   }" file_w)
               (write-line "   swh0;" file_w)
               (write-line "   :row {" file_w)
               (write-line "      swh1;" file_w)
               (write-line "      :rfwt {" file_w)
               (write-line "         :text  {label=\"0\";       key=\"num_dwgs\"; width=6;}" file_w)
               (write-line "         :text  {label=\"file(s) selected.\";       width=56;}" file_w)
               (write-line "      }" file_w)
               (write-line "      swh1;" file_w)
               (write-line "      :rfwt {" file_w)
               (write-line "         :but12 {label=\"&OK\";     key=\"accept\"; is_enabled=1;}" file_w)
               (write-line "         :but12 {label=\"&Cancel\"; key=\"cancel\"; is_default=true; is_cancel=true;}" file_w)
               (write-line "      }" file_w)
               (write-line "   }" file_w)
               (write-line "   swh0;" file_w)
               (write-line "}" file_w)
               (write-line "" file_w)
               (write-line "// -----------------------------------------------------------------------------" file_w)
               (write-line "//                                 --- END ---" file_w)
               (write-line "// -----------------------------------------------------------------------------" file_w)
               (close file_w)
            )
         )
         ; Check if the path to IMPORT is set & valid, if not create it
         (if (> comLineNfo 1)(prompt "\nCheck IMPORT"))
         (if (not (DOS_DIRP MFT_import))
            (progn
               (DOS_MKDIR MFT_import)
               ; create lisp for MFT logfile import filter
               (if (> comLineNfo 1)(prompt "\nCreate logfile import filter"))
               (setq file_w (open (strcat MFT_import "Import MFT logfile.lsp") "W"))
               (write-line "; Import filter for MFT logfiles" file_w)
               (write-line "; Allows to select a logfile & import files which where skipped\n" file_w)
               (write-line "(princ \"\\nImport Filter: MFT Log to list, by Roland.R71\")\n" file_w)
               (write-line "; --- MFT_ImportFilter ---------------------------------------------------------\n" file_w)
               (write-line "(defun MFT_ImportFilter ( / f_list data_list f_name logfile file_r record values csvLst delimit)" file_w)
               (write-line "   (setq logfile (DOS_GETFILED \"Select logfile to import:\" MFT_data \"logfile (*.csv)|\*.csv|All files (*.*)|*.*||\"))" file_w)
               (write-line "   (if logfile" file_w)
               (write-line "      (progn" file_w)
               (write-line "         (setq delimit \";\" csvLst nil file_r (open logfile \"r\"))" file_w)
               (write-line "         (while (setq record (read-line file_r))" file_w)
               (write-line "            (setq values nil)" file_w)
               (write-line "            (while (> (setq pos (vl-string-position (ascii delimit) record)) 0)" file_w)
               (write-line "               (setq values (append values (list (substr record 1 pos))) record (substr record (+ 2 pos)))" file_w)
               (write-line "            )" file_w)
               (write-line "            (if (> (strlen record) 0)(setq values (append values (list record))))" file_w)
               (write-line "            (if values (setq csvLst (append csvLst (list values))))" file_w)
               (write-line "         )" file_w)
               (write-line "         (close file_r)" file_w)
               (write-line "         (foreach record csvLst (setq f_name (nth 0 record))" file_w)
               (write-line "            (if (/= f_name \"filename\")(setq f_list (append f_list (list f_name))))" file_w)
               (write-line "         )" file_w)
               (write-line "      )" file_w)
               (write-line "   )" file_w)
               (write-line "   f_list" file_w)
               (write-line ")\n" file_w)
               (close file_w)
            )
         )
         ; Check and create info file
         (setq nfoFile (strcat MFT_files "MFT_info.txt"))
         (if (findfile nfoFile)
            (progn
               (setq file_r  (open MFT_dcl "r"))
               (setq line (read-line file_r))
               (close file_r)
               (if (= (substr line 2 26) "MultiFileTool.lsp - Build:")
                  (if (< (atoi (substr line 9 6)) (atoi buildNr))
                     (progn
                        (if (> comLineNfo 1)(princ (strcat "\nDelete old info file " (substr line 11 4) " vs " buildNr)))
                        (vl-file-delete MFT_dcl)
                     )
                  )
                  (progn
                     (if (> comLineNfo 1)(princ "\nDelete old info file"))
                     (vl-file-delete nfoFile)
                  )
               )
            )
         )
         (if (not (findfile nfoFile))
            (progn
               (if (> comLineNfo 0)(princ "\nCreate info file"))
               (setq file_r  (open (strcat MFT_files MFT_lsp) "r"))
               (while (and (setq line (read-line file_r))(not rdy))
                  (if (not (= line "; DOSlib functions used:"))
                     (setq nfo_lst (append nfo_lst (list line)))
                     (setq rdy T)
                  )
               )
               (close file_r)
               (setq file_w (open nfoFile "w"))
               (foreach line nfo_lst
                  (if (= line "; Description:")
                     (progn
                        (write-line "                              W E L C O M E" file_w)
                        (write-line "                                 to the" file_w)
                        (write-line "                              MultiFileTool" file_w)
                        (write-line "" file_w)
                     )
                  )
                  (write-line (substr line 2) file_w)
               )
               (write-line "                                  -={ END }=-" file_w)
               (write-line "                                     '-=-'" file_w)
               (close file_w)
            )
         )
         ; Set script file
         (setq cmd_var ""
               MFT_scr (strcat MFT_data "MFT.scr")
         )
         (if (> comLineNfo 0)(prompt "\nConfiguration ready"))
      )
      ((= (strcase rw) "W")
         (write_ini)
      )
   )
   ; end of config. Return True as signal we are all set
   T
)

; ============================== END MFT_Config ================================
; /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
; ============================= Common functions ===============================

; --- parsePath ----------------------------------------------------------------

   (defun parsePath (path / newPath i s c)
      ; function to change "\" to "/" in a given path
      ; This function takes a MS-winDOwS style resource location
      ; "[drive:|\\server\share]\[path]\[filename].[extension]"
      ; and returns a more AutoCAD friendly UNIX style path using /
      ; it will take any form of path using \ or \\
      ; Examples:
      ; C:\myDocs\drawing.dwg -or- C:\\myDocs\\drawing.dwg
      ; returns: C:/myDocs/drawing.dwg
      ; \\myServer\Share\drawing.dwg -or- \\\\myServer\\Share\\drawing.dwg
      ; returns: //myServer/Share/drawing.dwg
      
      (setq i 1 newPath "" s 0 c "")
      (repeat (strlen path)
         (setq c (substr path i 1))
         (cond
            ((and (= c "\\")(= s 0))
               (setq newPath (strcat newPath "/"))
               (if (> i 2)(setq s 1))
            )
            ((not (= c "\\"))
               (setq newPath (strcat newPath c))
               (setq s 0)
            )
         )
         (setq i (1+ i))
      )
      newPath
   )

; --- fileattrib ---------------------------------------------------------------

   (defun fileattrib (filename / dlv returnval)
      ; Function to determine if file is ReadOnly
      ; The return value of DOSlib function DOS_FILE changed with version 7
      ; So an extra check is needed to maintain compatibility across all versions.
      ; This function will check for doslib version and use the appropriate method
      ; it will return "R" if file is read-only, "" if not, nil on error
      
      (setq returnval nil)
      (setq dlv (DOS_VERSION))
      (if (< (nth 0 dlv) 7)
         ; Prior to version 7 it was like this
         (progn
            (setq returnval (nth 4 (DOS_FILE filename)))
         )
         (progn
            ; as of version 7 they return a bitcode containing all attributes
            (if (= (logand 1 (nth 3 (DOS_FILE filename))) 1)
               ; the bitcode contains 1 (Read-only) in combination with other file attributes
               (setq returnval "R")
               ; otherwise we set an empty value (not nil)
               (setq returnval "")
            )
         )
      )
      returnval
   )

; --- execLsp ------------------------------------------------------------------

   (defun execLsp ( lispFile / alwaysLoad autosave autosort cmd_lst cmd_str cmd_var countr
                    comLineNfo confdia cur_dwg dcl_id dia_abort DLpath drawing_name
                    duplct extno file_w filetype filter ftabstate isOpen
                    LISPINIT logSkip MFT_dcl MFT_lst MFT_SkipIt msg
                    noDuplicate noIni noRecover pathname progBar recursive rcvr
                    skip_readonly skip_savefile user_path warn_notfound warn_readonly
                    rdonly SDI warn_fileopen wildcard loadLisp loadScr start_dwg
                  )
      ; (attempting) to prevent any lisp from accidentally or deliberately messing with
      ; any of the MFT global variables, they are declared as local to this function,
      ; before loading it.

      (load lispFile)
   )

; --- write_ini ----------------------------------------------------------------

   (defun write_ini ( / )
      ; function to save settings to .ini file
      (if (> comLineNfo 1)(princ "\nWriting settings to ini file "))
      (DOS_SETINI "config" "lispFile"      lispFile      MFT_ini)
      (DOS_SETINI "config" "logFile"       logfile       MFT_ini)
      (DOS_SETINI "config" "lastdir"       pathname      MFT_ini)               ; last used path
      (DOS_SETINI "config" "userpath"      user_path     MFT_ini)               ; user path (for add 'all' files)
      (DOS_SETINI "config" "progBar"       progBar       MFT_ini)
      (DOS_SETINI "config" "logSkip"       logSkip       MFT_ini)
      (DOS_SETINI "config" "warn_notfound" warn_notfound MFT_ini)
      (DOS_SETINI "config" "warn_readonly" warn_readonly MFT_ini)
      (DOS_SETINI "config" "warn_fileopen" warn_fileopen MFT_ini)
      (DOS_SETINI "config" "skip_readonly" skip_readonly MFT_ini)
      (DOS_SETINI "config" "skip_savefile" skip_savefile MFT_ini)
      (DOS_SETINI "config" "wildcard"      wildcard      MFT_ini)               ; wildcard
      (DOS_SETINI "config" "recursive"     recursive     MFT_ini)               ; Boolean : include subdirs (y/n)
      (DOS_SETINI "config" "autosort"      autosort      MFT_ini)               ; Boolean : autosort (y/n)
      (DOS_SETINI "config" "autosave"      autosave      MFT_ini)               ; Boolean : autosave (y/n)
      (DOS_SETINI "config" "noDuplicate"   noDuplicate   MFT_ini)               ; Boolean : no duplicates (y/n)
      (DOS_SETINI "config" "noRecover"     noRecover     MFT_ini)               ; Boolean : no '_recover' files (y/n)
      (DOS_SETINI "config" "filetype"      filetype      MFT_ini)               ; Integer : 1 = DWG, 2 = DXF, 3 = Both
      (DOS_SETINI "config" "loadLisp"      loadLisp      MFT_ini)
      (DOS_SETINI "config" "alwaysLoad"    alwaysLoad    MFT_ini)
      (DOS_SETINI "config" "loadScr"       loadScr       MFT_ini)
   )

; --- load_list ----------------------------------------------------------------

   (defun load_list ( filename / file_r file new_list)
      ; function to read File List (file.lst)
      (if (= filename "")
         (setq filename (DOS_GETFILED "Select list: " (DOS_PWDIR) "MFT MFT-FileList (*.lst)|*.lst|All files (*.*)|*.*||"))
      )
      (if (and filename (findfile filename))                                    ; if there is a file
         (progn
            (if (> comLineNfo 0)(princ (strcat "\nReading filename list: " filename)))
            (DOS_WAITCURSOR T)                                                  ; turn on sandclock
            (setq file_r (open filename "r"))                                   ; open selected file for reading
            (setq flnf 0)                                                       ; init 'files' (lines) skipped counter
            (while (setq file (read-line file_r))                               ; for each line in the file
               (if (and (/= (substr file 1 1) "")                               ; skip empty line (no count)
                        (/= (substr file 1 1) ";"))                             ; skip comment lines (no count)
                  (if (= (DOS_FILEP file) T)                                    ; check if file out there
                     (setq new_list (append new_list (list (parsePath file))))  ; add it to the list
                     (setq flnf (1+ flnf))                                      ; else we skip it, and up the skipped count
                  )
               )
            )
            (close file_r)                                                      ; close the selected file
            (DOS_WAITCURSOR)                                                    ; turn off sandclock
            (check_list new_list nil)                                           ; check files
         )
         (Princ "\nError: Filelist file not found")
      )
   )

; --- check_list ---------------------------------------------------------------

   (defun check_list ( new_list lst_import / add file verChk)
      ; function to check the file list for duplicates / recover files / filetype etc.
      (if (> comLineNfo 0)(princ "\nChecking ..."))
      ; clean start
      (setq rcvr 0 duplct 0 extno 0 rdonly 0 i 0 flnf 0 newVer 0)
      (DOS_WAITCURSOR T)                                                        ; turn on sandclock
      ; in case no list was supplied, check the existing list
      (if (= new_list nil)
         (setq new_list MFT_lst MFT_lst nil)
      )
      (foreach file new_list
         (setq add T)
         (setq file (parsepath (strcase file)))
         ; check if it exists (for imported lists)
         (if (and lst_import (not (DOS_FILEP file)))
            (setq add nil flnf (1+ flnf))
         )
         ; check if recover file
         (if (and add (= noRecover "1"))
            (progn
               (setq len (strlen file))
               (if (and (> len 12)(= (substr file (- len 11) 8) "_recover"))
                  (setq add nil rcvr (1+ rcvr))
               )
            )
         )
         ; check file extension
         (if (and add (/= filetype "3"))
            (progn
               (if (> comLineNfo 1)(princ "\nChecking if DWG/DXF file "))
               (setq ext (strcase (substr file (- (strlen file) 3))))
               (if
                  (or
                     (and (= ftnum "1")(/= ext ".DWG"))
                     (and (= ftnum "2")(/= ext ".DXF"))
                  )
                  (setq add nil extno (1+ extno))
               )
            )
         )
         ; check if duplicate
         (if (and add (= noDuplicate "1"))
            (if (member file MFT_lst)
               (progn
                  (if (> comLineNfo 1)(princ "\nDuplicate file "))
                  (setq add nil duplct (1+ duplct))
               )
            )
         )
         ; check for file & acad version match
         (if (> comLineNfo 1)(princ "\nChecking versions"))
         (setq verChk (versionMatch (atoi (substr (getvar "acadver") 1 2)) file))
         (cond
            ((= verChk 0) ; fail
               (if (> comLineNfo 1)(princ ": File is newer"))
               (setq add nil newVer (1+ newVer))
            )
            ((= verChk 1) ; unknown
               (if (> comLineNfo 1)(princ ": Unknown/new versions"))
               (setq add nil newVer (1+ newVer))
            )
            ((= verChk 2) ; Versions match
               (if (> comLineNfo 1)(princ ": Ok"))
            )
         )
         ; check for "read only" files
         (if (= (fileattrib file) "R")
            (progn
               (if (> comLineNfo 1)(princ "\nRead-Only file "))
               (setq rdonly (1+ rdonly))
            )
         )
         ; if it passed all the tests, add file to list
         (if add
            (setq MFT_lst (append MFT_lst (list file)))
         )
      )
      ; automatically sort the list
      (if (and (> comLineNfo 1)(= autosort "1"))(princ "\nSorting list"))
      (if (= autosort "1")
         (setq MFT_lst (acad_strlsort MFT_lst))
      )
      (DOS_WAITCURSOR)                                                          ; turn off sandclock
      ; present results
      (if (> comLineNfo 0)
         (progn
            (princ (strcat "\nFiles not found.........: " (itoa flnf)))
            (princ (strcat "\n_recover files removed..: " (itoa rcvr)))
            (princ (strcat "\nDWG/DXF files removed...: " (itoa extno)))
            (princ (strcat "\nDuplicate files removed.: " (itoa duplct)))
            (princ (strcat "\nNewer versions removed..: " (itoa newVer)))
            (princ (strcat "\nRead-Only files detected: " (itoa rdonly)))
         )
      )
      (setq list_count (length MFT_lst))
      (setq msg "")
      (if (> flnf 0)
         (setq msg (strcat (itoa flnf) " file(s) not found\n"))
      )
      (if (> rcvr 0)
         (setq msg (strcat msg (itoa rcvr) " recovery file(s)\n"))
      )
      (if (> extno 0)
         (setq msg (strcat msg (itoa extno) " non " ft_ext " file(s)\n"))
      )
      (if (> duplct 0)
         (setq msg (strcat msg (itoa duplct) " duplicate file name(s)\n"))
      )
      (if (> newVer 0)
         (progn
            (setq msg (strcat msg (itoa newVer) " file(s) with higher version\n"))
            (alert (strcat "WARNING: " (itoa newVer) " file(s) you're trying to add require a higher version AutoCAD to open!"))
         )
      )
      (if (/= msg "")
         (DOS_MSGBOX msg "Warning: file(s) removed" 1 1)
      )
      (set_tile "num_dwgs" (itoa list_count))                                   ; display the number of files
      (if (> list_count 0)                                                      ; if there is a file in the list
         (progn
            (mode_tile "sort_lst" 0)                                            ; enable sort
            (mode_tile "save_lst" 0)                                            ; enable save
            (mode_tile "view_lst" 0)                                            ; enable view
            (mode_tile "clr_lst"  0)                                            ; enable clear
         )
      )
      (showList)
      (if (> comLineNfo 0)(princ "\nCheck done."))
      (if MFT_lst (mode_tile "accept" 0)(mode_tile "accept" 1))
   )

; ------------------------------------------------------------------------------

   (defun showList ( / file )
      (start_list "filelist")
      (foreach file MFT_lst (add_list file))
      (end_list)
      (set_tile "num_dwgs" (itoa (length MFT_lst)))
   )

; ------------------------------------------------------------------------------

   (defun versionMatch (progVer dwg / file_r vrs dwgVer curVer ff)
      ; Check which version the drawing was made in and check if currently
      ; running version of AutoCAD will be able to open it or not.

      ; This function is a heavily modified version of Lee-Mac's DWGVersion
      ; function as found at: http://www.lee-mac.com/drawingversion.html

      ; progVer = INT   : ACAD main version - (atoi (substr (getvar "acadver") 1 2))
      ; dwg     = STRING: Name of file to open, including path

      ; returns 0,1 or 2 (0=fail,1=unknown,2=ok)

      (setq file_r (open dwg "r"))
      (cond
         (  (wcmatch (strcase dwg t) "*`.dw[gst]")
            (setq vrs (strcase (substr (read-line file_r) 1 6)))
         )
         (  (wcmatch (strcase dwg t) "*`.dxf")
            (repeat 7 (read-line file_r))
            (setq vrs (strcase (read-line file_r)))
         )
      )
      (close file_r)
      ; Get the acad file format version used to save the drawing
      (setq dwgVer
         (cdr
            (assoc vrs
              '(
                  ("AC1032" . "2018")
                  ("AC1027" . "2013")
                  ("AC1024" . "2010")
                  ("AC1021" . "2007")
                  ("AC1018" . "2004")
                  ("AC1015" . "2000")
                  ("AC1014" . "14")
                  ("AC1012" . "13")
                  ("AC1009" . "11")
                  ("AC1006" . "10")
                  ("AC1004" . "9")
                  ("AC1003" . "2.60")
                  ("AC1002" . "2.50")
                  ("AC1001" . "2.22")
                  ("AC2.22" . "2.22")
                  ("AC2.21" . "2.21")
                  ("AC2.10" . "2.10")
                  ("AC1.50" . "2.05")
                  ("AC1.40" . "1.40")
                  ("AC1.2"  . "1.2")
                  ("MC0.0"  . "1.0")
               )
            )
         )
      )
      (if (not dwgVer)(setq dwgVer "new"))
      (if (> comLineNfo 1)(princ (strcat "\nThe selected file is an AutoCAD " dwgVer " format file.")))
      ; get the current (lowest) acad version for mayor release number active
      ; it is enough for the match to know the first versions for mayor release numbers
      (setq curVer
         (cdr
            (assoc progVer
              '(
                  (24 . 2021)
                  (23 . 2019)
                  (22 . 2018)
                  (21 . 2017)
                  (20 . 2015)
                  (19 . 2013)
                  (18 . 2010)
                  (17 . 2007)
                  (16 . 2004)
                  (15 . 2000)

               )
            )
         )
      )
      (if (numberp (read dwgVer))(setq dwgVer (atoi dwgVer))(setq dwgVer 0))
      (cond
         ((or (< curVer dwgVer)(and (< progVer 26)(= dwgVer 0))) ; newer file
            (if (> comLineNfo 1)(princ "\nFile version is new/unknown to MFT!"))
            ; The version of the file is newer as current acad version.
            (setq retVal 0)
         )
         ((and (> progVer 25)(= dwgver 0)) ; new acad, new file
            (if (> comLineNfo 1)(princ "\nWARNING: Both ACAD and file version are new to MFT!"))
            ; Current ACAD is newer as 2021 and doesn't yet exist while writing MFT
            ; the file is probably also newer, so at some point in the future
            ; this could be a problem, so we skip it by default to be sure.
            (setq retVal 1)
            ; now lets see if we can 'guess' a bit here.
            ; If we take the last 2 or 3 digits from the file version code, - 5 we get
            ; the AutoCad release number where it was introduced. At least AFTER acad2000 until 2021
            (setq dwgVer (substr vrs 4 3))
            (if (numberp dwgVer)
               (if (< (- (atoi dwgVer) 5) progVer)
                  (setq retVal 4) ; it looks like the acad version used is equal or higher as the file
               )
            )
         )
         ((or (>= curVer dwgVer)(and (> progVer 25)(/= dwgVer 0))) ; new acad, old file
            (if (> comLineNfo 1)(princ "\nACAD version is equal or higher as file version"))
            ; Even if current ACAD is newer as 2021 and doesn't yet exist while
            ; writing MFT the file is older so that should be no problem
            (setq retVal 2)
         )
      )
      retVal
   )

; =========================== END Common functions =============================
; /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
; ============================ Start MFT_getFiles ==============================

(defun MFT_getFiles ( arg / MFT_lst dia_abort noDia)

   ; Function to allow user to select files in any way thinkable
   ; arg = STRING: Name of import filter to use
   ;       LIST  : Pathname to read files from incl. settings to use and return as list
   ;               ("c:/path/to/import/" [recursive "1" yes "0" no] [wildcard "*"] [filetype "1" dwg, "2" dxf "3" both])
   ;               ("c:/path/to/import/" 1 "*" 1)
   ;       LIST  : List with filenames to be preselected at start
   ;       nil   : Show dialog for file selection

   ; Returns: The list of selected files, nil on cancel or if nothing selected

   ; Uses MFT_getFiles dialog, if no directory or importfilter was supplied

   ; For the file selection there are various options for adding files. incl. (all)
   ; currently open, a single directory, based on a wilcard or whole directory trees.
   ; Supports DWG & DXF

   ; Option to:
   ; - scan for Read-only files
   ; - scan for _recover files
   ; - sort list of filenames
   ; - save/load lists
   ; - autosave & load (remember) list
   ; - allow/prevent duplicate filenames*
   ; - import filenames with custom importfilters
   ; - start with a given list of filenames
   ; - supply a directory and settings and get a list of filenames in return.
   ; You can edit the list by moving files up and down the list or remove files.

   ; By passing a directory to the function, you can bypass the dialog and get a
   ; list of filenames in return. It will use the settings from the .ini file
   ; for use of wildcards, filetype, recursive etc.

   ; * = By adding the same directory twice, you can process each file twice
   ;     which might be usefull if you need to collect data first & write back
   ;     info later. (example: crossreference information)

   ; Usage examples:
   ; (MFT_getFiles nil)
   ;   start normal, with dialog & empty or saved list

   ; (MFT_getFiles (list "c:/testdwg/" "1" "*" "1"))
   ;   returns filenames from given directory including subdirs, with wildcard *
   ;   & DWG filetype setting (no dialog)

   ; (MFT_getFiles "Import MFT logfile")
   ;   runs import filter "Import MFT logfile" & returns filenames (no dialog)

   ; (MFT_getFiles (list "c:/testdwg/drw1.dwg" "c:/testdwg/drw2.dwg"))
   ;   directly adds files to selection at startup

; --- error function -----------------------------------------------------------

   (defun *error* ( msg / )
      ; turn off the sandclock cursor in case its on
      (if DOS_VERSION
         (DOS_WAITCURSOR)
      )
      (if
         (or
            (= msg "Function cancelled")
            (= msg "quit / exit abort")
         )
         (princ)
         (princ (strcat "\nError: " msg))
      )
   )

; --- get_files ----------------------------------------------------------------

   (defun get_files ( / temp_list filename)
      ; function to select (multiple) file(s)
      (if (> comLineNfo 1)(princ "\nSelect file(s):"))
      (setq temp_list (DOS_GETFILEM "Select file(s): " pathname (strcat "AutoCAD Drawing (" ft_ext ") | " ft_ext "||")))
      (if temp_list
         (progn
            (DOS_WAITCURSOR T)
            (setq pathname (parsePath (car temp_list)))
            (foreach filename (cdr temp_list)
               (setq new_list (append new_list (list (strcat pathname filename))))
            )
            (DOS_WAITCURSOR)
            (check_list new_list nil)
         )
      )
   )

; --- add_current --------------------------------------------------------------

   (defun add_current ( / filename new_list)
      ; function to add current file
      (if (> comLineNfo 1)(princ "\nAdding current file"))
      (setq filename (strcat (parsePath (getvar "DWGPREFIX"))(getvar "DWGNAME")))
      (cond
         ((= (DOS_FILEP filename) T)
            (setq new_list (append new_list (list filename)))
            (check_list new_list nil)
         )
         ((/= (DOS_FILEP filename) T)
            (if (> comLineNfo 0)(princ "\nWarning: Ignoring unsaved new drawing"))
            (alert "Warning: You can't add a new, unsaved (empty?) drawing.")
         )
      )
   )

; --- add_allOpen --------------------------------------------------------------

   (defun add_allOpen ( / acdocs ao dc dwg filename new_list)
      ; function to add all currently open files
      (if (> comLineNfo 1)(princ "\nAdding current file"))
      (setq acdocs nil)
      (setq ao (vlax-get-Acad-Object))
      (setq dc (vla-get-documents ao))
      (vlax-for <doc> dc
         (if (eq "" (setq filename (vla-get-fullname <doc>))) (setq filename (vla-get-name <doc>)))
         (setq acdocs (cons (parsePath filename) acdocs))
      )
      (foreach filename (cdr acdocs)
         (cond
            ((= (DOS_FILEP filename) T)
               (setq new_list (append new_list (list filename)))
               (setq modified 1)
            )
            ((/= (DOS_FILEP filename) T)
               (if (> comLineNfo 0)(princ "\nWarning: Ignoring unsaved new drawing"))
               (alert "Warning: You can't add a new, unsaved (empty?) drawing.")
            )
         )
      )
      (check_list new_list nil)
   )

; --- get_dir ------------------------------------------------------------------

   (defun get_dir ( / dir_path path)
      ; function to select directory (to add all files)
      (if (> comLineNfo 1)(princ "\nSelect directory:"))
      (if user_path
         (setq dir_path user_path)
         (setq dir_path pathname)
      )
      (setq path (DOS_GETDIR "Select directory: " dir_path))
      (if (/= path nil)
         (progn
            (setq user_path path pathname path)
            (set_tile "user_path" user_path)
            (mode_tile "add_lst" 0)
         )
      )
   )

; --- dir_list -----------------------------------------------------------------

   (defun dir_list ( / temp_list filename paths i l new_drList)
      ; function to add all files inside directory (& subdirs)
      (setq temp_list nil paths nil filename nil)                               ; clean start
      (if (= recursive "1")                                                     ; Include subdirs
         (progn
            (if (> comLineNfo 0)(princ (strCat "\nAdding directory tree for: " user_path)))
            (DOS_WAITCURSOR T)
            ; Get the directory tree (from current)
            (setq i 0 paths (DOS_DIRTREE user_path))
            ; for each dir:
            (while (< i (length paths))
               (if (or (= filetype "1") (= filetype "3")) ; DWG's
                  (progn
                     ; get the DWG drawings
                     (if (> comLineNfo 1)(princ "\nAdding DWG's"))
                     (readDir (nth i paths) (strcat wildcard ".dwg"))
                  )
               )
               (if (or (= filetype "2") (= filetype "3")) ; DXF's
                  (progn
                     ; get the DXF drawings
                     (if (> comLineNfo 1)(princ "\nAdding DXF's"))
                     (readDir (nth i paths) (strcat wildcard ".dxf"))
                  )
               )
               (setq i (1+ i))
            )
            (DOS_WAITCURSOR)
         )
      ;Else Current dir only
         (progn
            (if (> comLineNfo 0)(princ (strcat "\nAdding current directory only: " user_path)))
            (DOS_WAITCURSOR T)
            (if (or (= filetype "1") (= filetype "3")) ; DWG's
               (progn
                  ; get the drawings for current directory
                  (if (> comLineNfo 1)(princ "\nAdding DWG's"))
                  (readDir user_path (strcat wildcard ".dwg"))
               )
            )
            (if (or (= filetype "2") (= filetype "3")) ; DXF's
               (progn
                  ; get the drawings for current directory
                  (if (> comLineNfo 1)(princ "\nAdding DXF's"))
                  (readDir user_path (strcat wildcard ".dxf"))
               )
            )
            (DOS_WAITCURSOR)
         )
      )
      (if new_drList
         (check_list new_drList nil) ; check files
      )
   )

; --- readDir ------------------------------------------------------------------

   (defun readDir ( dirPath filter / i temp_list filepath)
      ; function to add filtered files inside given directory
      (if (> comLineNfo 0)(princ (strcat "\nAdding file(s) in: \"" dirPath "\" based on filter: \"" filter "\"")))
      (setq i 0 temp_list (DOS_DIR (strcat dirPath filter)))
      (while (< i (length temp_list))
         (setq filepath (strcat (parsePath dirPath)(nth i temp_list)))
         (setq new_drList (append new_drList (list filepath)))
         (setq i (1+ i))
      )
   )

; --- save_list ----------------------------------------------------------------

   (defun save_list ( filename / file_w)
      ; function to write File List (file.lst)
      (if (> comLineNfo 0)(princ "\nSave filename list: "))
      (if MFT_lst
         (progn
            (if (= filename "")                                                 ; if no filename
               (setq filename (GETFILED "Select list: " (DOS_PWDIR) "lst" 1))   ; ask user for filename
            )
            (if filename                                                        ; In case user clicked 'cancel'
               (progn
                  (if (> comLineNfo 0)(princ filename))
                  (DOS_WAITCURSOR T)                                            ; turn on sandclock
                  (setq file_w (open filename "w"))                             ; open the file (for (over) writing))
                  (setq l 0)
                  (while (< l (length MFT_lst))
                     (setq filepath (nth l MFT_lst))
                     (write-line filepath file_w)                               ; write each filename to the file
                     (setq l (1+ l))
                  )
                  (close file_w)
                  (DOS_WAITCURSOR)                                              ; turn off sandclock
               )
            )
         )
      )
   )

; --- import_list --------------------------------------------------------------

   (defun import_list ( / temp_list file modName ModList choice f_list lispFile)
      ; function to import a list of filenames
      (if (> comLineNfo 1)(princ "\nSelect Import Filter"))
      (setq temp_list (DOS_DIR (strcat MFT_import "*.lsp")))
      (foreach file temp_list
         (setq modName (substr file 1 (- (strlen file) 4)))
         (setq modList (append modList (list modName)))
      )
      (setq choice (DOS_LISTBOX "Import list" "Choose your import filter to add files" modList))
      (if choice
         (progn
            (setq lispFile (strcat MFT_import choice ".lsp"))
            (execLsp lispFile)
            (if MFT_ImportFilter
               (setq f_list (MFT_ImportFilter))
            )
            (if (> comLineNfo 1)(progn (princ "\nImported list\n")(print f_list)))
            (if f_list
               (check_list f_list T)                                            ; check files
            )
         )
      )
   )

; --- clear_list ---------------------------------------------------------------

   (defun clear_list ( / )
      ; function to clear file list
      (if (> comLineNfo 0)(princ "\nClearing list"))
      (setq list_count 0 modified 0 length_list 0 MFT_lst nil)
      (set_tile  "num_dwgs" "0")
      (set_tile  "lst_view" "")
      (showList)
      (mapcar 'mode_tile '("top" "up" "down" "bottom" "remove" "sort_lst" "clr_lst" "save_lst" "accept") '(1 1 1 1 1 1 1 1 1))
   )

; --- duplicate_yn -------------------------------------------------------------

   (defun duplicate_yn ( / file temp_lst duplct)
      ; function to check for duplicates, when selected
      (setq noDuplicate $value)                                                 ; set duplicate check on/off
      (if (= noDuplicate "1")
         (progn
            (setq temp_lst nil)
            (foreach file MFT_lst
               (if (member file temp_lst)
                  (setq duplct (1+ duplct))
             (setq temp_lst (append temp_lst (list file)))
          )
       )
            (if (> duplct 0)
               (progn
                  (setq msg (strcat msg (itoa duplct) " duplicate file name(s)\n"))
                  (setq MFT_lst temp_lst)
                  (dcl-list MFT_lst)
                  (set_tile "num_dwgs" (itoa (length MFT_lst)))                 ; display the number of files
                  (mode_tile "remove" 1)
               )
            )
         )
      )
   )

; --- Recover_yn ---------------------------------------------------------------

   (defun Recover_yn ( / file temp_lst rcvr)
      ; function to check for '_recover' files, when selected
      (setq noRecover $value)                                                   ; set '_recover' check on/off
      (if (= noRecover "1")                                                     ; when on, check the list for recover files
         (progn
            (setq temp_lst nil)
            (foreach file MFT_lst
               (setq len (strlen file))
               (if (and (> len 12)(= (substr file (- len 11) 8) "_recover"))
                  (setq rcvr (1+ rcvr))
                  (setq temp_lst (append temp_lst (list file)))
               )
            )
            (if (> rcvr 0)
               (progn
                  (setq msg (strcat msg (itoa rcvr) " recover file(s)\n"))
                  (setq MFT_lst temp_lst)
                  (dcl-list MFT_lst)
                  (set_tile "num_dwgs" (itoa (length MFT_lst)))                 ; display the number of files
                  (mode_tile "remove" 1)
               )
            )
         )
      )
   )

; --- sort_yn ------------------------------------------------------------------

   (defun sort_yn ( / )
      ; function to sort filenames, when selected
      (setq autosort $value)                                                    ; set auto order on/off
      (if (= autosort "1")
         (progn
            (if MFT_lst (setq MFT_lst (acad_strlsort MFT_lst)))                 ; when on, sort the list
            (mapcar 'mode_tile '("top" "up" "down" "bottom" "remove" "sort_lst") '(1 1 1 1 1 1))
            (dcl-list MFT_lst)
         )
         (if (> (length MFT_lst) 0)
            (progn
               (mode_tile "sort_lst" 0)
            )
         )
      )
   )

; --- set_ftype ----------------------------------------------------------------

   (defun set_ftype ( ftnum / )
      ; function to set filetypes
      (if (= ftnum "1") (setq filetype "1" ft_ext "*.dwg"))                     ; to .DWG
      (if (= ftnum "2") (setq filetype "2" ft_ext "*.dxf"))                     ; to .DXF
      (if (= ftnum "3") (setq filetype "3" ft_ext "*.dwg;*.dxf"))               ; to both
      (if (/= ftnum "3")(count_list))
   )

; --- Lee Mac's List Box functions ---------------------------------------------
   ; This is a slightly modified version of Lee Mac's List Box Functions
   ; For original see: http://www.lee-mac.com/listboxfunctions.html

   (defun LM:listup ( idx lst / foo )
      ;; List Box: List Up  -  Lee Mac
      ;; Shifts the items at the supplied indexes by one position to a lower index
      ;; idx - [lst] List of zero-based indexes
      ;; lst - [lst] List of items
      ;; Returns: [lst] List of ((idx) (lst)) following operation
      (defun foo ( cnt idx lst idx-out lst-out )
         (cond
            ((not  (and idx lst))
               (list (reverse idx-out) (append (reverse lst-out) lst))
            )
            ((= 0 (car idx))
               (foo (1+  cnt) (mapcar '1- (cdr idx)) (cdr lst) (cons cnt idx-out) (cons (car lst) lst-out))
            )
            ((= 1 (car idx))
               (foo (1+  cnt) (mapcar '1- (cdr idx)) (cons (car lst) (cddr lst)) (cons cnt idx-out) (cons (cadr lst) lst-out))
            )
            ((foo (1+  cnt) (mapcar '1- idx) (cdr lst) idx-out (cons (car lst) lst-out)))
         )
      )
      (foo 0 idx lst nil nil)
   )
   (defun LM:listdown ( idx lst / bar foo len )
      ;; List Box: List Down  -  Lee Mac
      ;; Shifts the items at the supplied indexes by one position to a higher index
      ;; idx - [lst] List of zero-based indexes
      ;; lst - [lst] List of items
      ;; Returns: [lst] List of ((idx) (lst)) following operation
      (setq len (length lst)
            foo (lambda ( x ) (- len x 1))
            bar (lambda ( a b ) (list (reverse (mapcar 'foo a)) (reverse b)))
      )
      (apply 'bar (apply 'LM:listup (bar idx lst)))
   )
   (defun LM:listtop ( idx lst / i )
      ;; List Box: List Top  -  Lee Mac
      ;; Shifts the items at the supplied indexes to the lowest index
      ;; idx - [lst] List of zero-based indexes
      ;; lst - [lst] List of items
      ;; Returns: [lst] List of ((idx) (lst)) following operation
      (setq i -1)
      (list
         (mapcar '(lambda ( x ) (setq i (1+ i))) idx)
         (append (mapcar '(lambda ( x ) (nth x lst)) idx) (LM:removeitems idx lst))
      )
   )
   (defun LM:listbottom ( idx lst / i )
      ;; List Box: List Bottom -  Lee Mac
      ;; Shifts the items at the supplied indexes to the highest index
      ;; idx - [lst] List of zero-based indexes
      ;; lst - [lst] List of items
      ;; Returns: [lst] List of ((idx) (lst)) following operation
      (setq i (length lst))
      (list
         (reverse (mapcar '(lambda ( x ) (setq i (1- i))) idx))
         (append (LM:removeitems idx lst) (mapcar '(lambda ( x ) (nth x lst)) idx))
      )
   )
   (defun LM:removeitems ( idx lst / i )
      ;; Remove Items  -  Lee Mac
      ;; Removes the items at the supplied indexes from a given list
      ;; idx - [lst] List of zero-based indexes
      ;; lst - [lst] List from which items are to be removed
      ;; Returns: [lst] List with items at the supplied indexes removed
      (setq i -1)
      (vl-remove-if '(lambda ( x ) (member (setq i (1+ i)) idx)) lst)
   )
   (defun key-mode ( idx lst / foo )
      (setq foo (lambda ( a b / r ) (repeat a (setq r (cons (setq b (1- b)) r)))))
      (cond
         ((or (null idx) (= (length idx) (length lst)))
            '(1 1 1 1 1)
         )
         ((equal idx (foo (length idx) (length idx)))
            '(1 1 0 0 0)
         )
         ((equal idx (foo (length idx) (length lst)))
            '(0 0 1 1 0)
         )
         (  '(0 0 0 0 0))
      )
   )

; ------------------------------------------------------------------------------

   (defun flRemove ( idx / nr)
      (foreach nr (reverse (@str2list idx " "))
         (setq MFT_lst (vl-remove (nth (atoi nr) MFT_lst) MFT_lst))
      )
      (mapcar 'mode_tile '("top" "up" "down" "bottom" "remove") '(1 1 1 1 1))
      (showList)
   )

; ------------------------------------------------------------------------------

   (defun dcllist ( lst / )
      (setq MFT_lst lst)
      (showList)
   )

; ------------------------------------------------------------------------------

   (defun sort_list ( / )
      (if MFT_lst (setq MFT_lst (acad_strlsort MFT_lst)))
      (mapcar 'mode_tile '("top" "up" "down" "bottom" "remove") '(1 1 1 1 1))
      (showList)
   )

; ------------------------------------------------------------------------------

   (defun pathEnd ( path / lc)
      (setq lc (substr path (- (strlen path) 1)))
      (if(and (/= lc "\\")(/= lc "/"))
         (setq path (strcat path "/"))
      )
      path
   )

; =============================== MAIN SEGMENT =================================

   ; --- INIT ------------------------------------------------------------------
   (if (not cfgLoaded)(setq cfgLoaded (MFT_Config "r")))

      ; user supplied a directory or import filter to use
      (if arg 
      (cond
         ((listp arg) ; its a list
            (if (DOS_DIRP (nth 0 arg)) ; the first item is a (existing) directory
               ; return filenames from supplied directory
               (progn
                  (setq user_path (pathEnd (parsePath (nth 0 arg)))
                        recursive (nth 1 arg)
                        wildcard  (nth 2 arg)
                        filetype  (nth 3 arg)
                  )
                  (dir_list)
                  (setq noDia T)                                                ; skip the dialog
               )
               ; add supplied list of files
               (check_list arg T)                                               ; check the list
            )
         )
         
         ; return filenames from supplied import filter
         ((and (not (listp arg))(findfile (setq lispFile (strcat MFT_import arg ".lsp"))))
            (load lispFile)
            (if MFT_ImportFilter
               (setq f_list (MFT_ImportFilter))
            )
            (if f_list
               (check_list f_list T)                                            ; check the list
            )
            (setq noDia T)                                                      ; skip the dialog
         )
      )
   )

; --- Interaction --------------------------------------------------------------

   (if (not noDia) ; if we are not skipping the dialog
      (progn
         (if (> comLineNfo 1)(princ "\nInit dialog"))
         (setq dcl_id (load_dialog MFT_dcl))
         (if (not (new_dialog "MFT_getFiles" dcl_id)) (exit))
         (set_tile "caption" "MFT File Selector\nHere you can select your files in pretty much any way possible. You can use wildcards, add single directories or a whole tree.\n Load/Import/Save your list. Order the list or manually move files up & down the list.")
         (mode_tile "clr_lst"  1)                                               ; disable clear
         (mode_tile "save_lst" 1)                                               ; save
         (mode_tile "sort_lst" 1)                                               ; & sort list options
         (cond
            ((= filetype "1")                                                   ; if filetype is 1 ...
               (setq ft_ext "*.dwg")
               (set_tile "DWG" "1")                                             ; we look for DWG's
               (set_tile "DXF" "0")
               (set_tile "all" "0")
            )
            ((= filetype "2")                                                   ; if it's 2 ...
               (setq ft_ext "*.dxf")
               (set_tile "DWG" "0")
               (set_tile "DXF" "1")                                             ; we look for DXF's
               (set_tile "all" "0")
            )
            ((= filetype "3")                                                   ; otherwise ...
               (setq ft_ext "*.dwg;*.dxf")
               (set_tile "DWG" "0")
               (set_tile "DXF" "0")
               (set_tile "all" "1")                                             ; we look for both
            )
         )
         (set_tile "user_path"   user_path)
         (set_tile "wildcard"    wildcard)
         (set_tile "recursive"   recursive)
         (set_tile "autosort"    autosort)
         (set_tile "autosave"    autosave)
         (set_tile "noDuplicate" noDuplicate)
         (set_tile "noRecover"   noRecover)

         ; auto load last filelist, when selected
         (if (and (= autosave "1") (= (DOS_FILEP (strcat MFT_data "MFTFiles.lst")) T)(not MFT_lst))
            (load_list (strcat MFT_data "MFTFiles.lst"))
            (check_list nil nil)
         )
         (showList)
         (if (> comLineNfo 1)(princ "\nStarting dialog actions"))
         (setq dialog_action 7)
         (while (/= dialog_action 4)
            ; --- Lee Mac's List Box functions ---
            ; This is a slightly modified version of Lee Mac's List Box Functions
            ; For original see: http://www.lee-mac.com/listboxfunctions.html
            ;
            ; Dialog action code: Modified version of Lee Mac's List Box functions example code
            (mapcar 'mode_tile '("top" "up" "down" "bottom" "remove") '(1 1 1 1 1))
            (mapcar
                (function
                    (lambda ( key fun )
                        (action_tile key
                            (vl-prin1-to-string
                                (list 'if 'idx
                                    (list 'apply
                                      ''(lambda ( idx MFT_lst )
                                            (showList)
                                            (set_tile "filelist" (vl-string-trim "()" (vl-princ-to-string idx)))
                                            (mapcar 'mode_tile '("top" "up" "down" "bottom" "remove") (key-mode idx MFT_lst))
                                        )
                                        (list 'mapcar ''set ''(idx MFT_lst) (list fun 'idx 'MFT_lst))
                                    )
                                )
                            )
                        )
                    )
                )
               '("top" "up" "down" "bottom" "remove")
               '(LM:listtop LM:listup LM:listdown LM:listbottom remove)
            )
            (action_tile "filelist"
               (strcat "(mode_tile \"remove\" 0)"
                  (vl-prin1-to-string
                     '(progn
                        (setq idx (read (strcat "(" $value ")")))
                        (if (= autosort "0")
                           (mapcar 'mode_tile '("top" "up" "down" "bottom" "remove") (key-mode idx MFT_lst))
                        )
                     )
                  )
               )
            )
            ; --- End modified Lee Mac's Dialog action code example ---

            ; if autosort is enabled, files can't be moved
            (if (= autosort "1") 
               (progn
                  (mapcar 'mode_tile '("top" "up" "down" "bottom")(1 1 1 1))
               )
            )
            (action_tile "remove"        "(flRemove (get_tile \"filelist\"))")
            (action_tile "recursive"     "(setq recursive $value)")             ; in/ex-clude sub-directories
            (action_tile "wildcard"      "(setq wildcard $value)")              ; set (dos) wildcard ( ? = 1 char, * = multiple )
            (action_tile "DWG"           "(set_ftype \"1\")")                   ; only include DWG's
            (action_tile "DXF"           "(set_ftype \"2\")")                   ; only include DXF's
            (action_tile "all"           "(set_ftype \"3\")")                   ; include both DWG & DXF
            (action_tile "addcur"        "(add_current)")                       ; add the current drawing
            (action_tile "addopen"       "(add_allOpen)")                       ; add all the currently open drawings
            (action_tile "browse"        "(get_files)")                         ; select (one or more) file(s)
            (action_tile "browse_dir"    "(get_dir)")                           ; select directory
            (action_tile "add_lst"       "(dir_list)")                          ; add all files inside directory

            (action_tile "load_lst"      "(load_list \"\")")                    ; load a saved list
            (action_tile "impt_lst"      "(import_list)")                       ; import list using a filter mod
            (action_tile "save_lst"      "(save_list \"\")")                    ; save the current list
            (action_tile "clr_lst"       "(clear_list)")                        ; clear (reset) the list
            (action_tile "sort_lst"      "(sort_list)")                         ; sort the list (alphanumeric)

            (action_tile "noDuplicate"   "(duplicate_yn)")                      ; remove duplicate filenames from the list
            (action_tile "noRecover"     "(recover_yn)")                        ; remove '_recover' files from the list
            (action_tile "autosort"      "(sort_yn)")                           ; auto sort the list
            (action_tile "autosave"      "(setq autosave $value)")              ; autosave (remember) the list

            (action_tile "accept"        "(done_dialog 4)")                     ; user accepts
            (action_tile "cancel"        "(setq dia_abort 1 MFT_lst nil)(done_dialog 4)") ; user aborts
            (setq dialog_action (start_dialog))
         )
         (unload_dialog dcl_id)

         ; ------ Confirmation ------
         (if (and (/= dia_abort 1)(= autosave "1"))
            ; Once done with selecting files, save the list of files if so required,
            ; to be reloaded next time.
            (progn
               (save_list (strcat MFT_data "MFTFiles.lst"))                     ; save it
               (if (> comLineNfo 1)(princ "\nAutosaved list"))
            )
         )
      )
   )
   ; return list of selected files, or nil
   MFT_lst
)

; ============================= END MFT_getFiles ===============================
; /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
; ============================== Start MFT_Open ================================

(defun MFT_Open (get_file wnf wfo wro sro ssf / cur_file drwOpen SkipIt
                 cur_att get_att fileopen cmd retry usr_choice buttons)

   ; Function to open file

   ; get_file = string: name of file to open
   ; wnf      = int: 1 = warn if not found
   ; wfo      = int: 1 = warn if file open
   ; wro      = int: 1 = warn if file read-only
   ; sro      = int: 1 = skip if file read-only
   ; ssf      = int: 1 = do not save (current) file

   ; returns: T on succes, nil if not

   ; Requires: No more then 1 drawing to be open

   ; Description:
   ; Function to (sequentialy) open files using various checks to always
   ; correctly feed 'answers' to the fileopen command.

   ; - Checks if the file really exists
   ; - Checks if the file doesn't require a higher acad version
   ; - Checks if the current file needs to, and can, be saved
   ; - Ignores new-unsaved drawings (closed without saving)
   ; - Open (& Close) writeable files, which we save when changed
   ; - Open (& Close) Read-Only files, which we CAN'T save
   ; - Open (& Close) Open (other user:network) files, which we CAN'T save either

   ; - Option to warn user about missing files (dialog closes after 10 sec.)
   ; - Option to warn user about Read-Only & Open(network) files and ask user to skip, retry or open Read-Only
   ; - Option to automatically skip Read-Only & Open(network) files (open read-only is default)
   ; - Option to log files which where not processed.
   ; - Option to ignore changes (no saving)

   ; After executing the MFTOpen function, it wil return T when the requested file
   ; has been opened. It will return nil if the file gets skipped and will
   ; NOT be open for further processing.
   ; If the file is already open, the MFTOpen function will act as if succesfull.

   ; This is to allow executing from the first drawing in your list & for
   ; processing the same file more then once, if user wishes. It will asume
   ; the user is not stupid, and has his reasons for doing so. :P

   ; NOTE:
   ; This function requires SDI mode, so it's NOT going to work if multiple drawings
   ; are open at the moment of execution.

   ; When using "fileopen", closing the current drawing happens automatically.
   ; When changed it will ask to "really discard changes?", which we must
   ; if the file is Read-Only.
   ; *** ! this is contrary to the dialog, which asks to "save changes?" !

   ; The filenames passed to the MFTOpen function will automatically have the \
   ; swapped for / to prevent problems (\ = escape char)

   ; Usage example:
   ; (MFT_Open "c:/temp/test.dwg" 1 1 1 0 0)

; =============================== MAIN SEGMENT =================================

   ; --- INIT ------------------------------------------------------------------
   (if (not cfgLoaded)(setq cfgLoaded (MFT_Config "r")))

   (setq cur_file (strcase (parsePath (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))))
   (setq get_file (strcase get_file))

   (if (> comLineNfo 1)(progn(princ "\nwarn_notfound ")(princ wnf)))
   (if (> comLineNfo 1)(progn(princ "\nwarn_fileopen ")(princ wfo)))
   (if (> comLineNfo 1)(progn(princ "\nwarn_readonly ")(princ wro)))
   (if (> comLineNfo 1)(progn(princ "\nskip_readonly ")(princ sro)))
   (if (> comLineNfo 1)(progn(princ "\nskip_savefile ")(princ ssf)))

   (setq *MFT_logtext nil)

   ; Check if file exists and/or if user has it already open (network follows later)
   ; ---------------------------------------------------------------------------
   ; When the file does not exist, there is no need to continue
   ; When the file is already open (local), it won't have to save and/or open
   ; anything. It checks the UNC Path, to make sure any drive letter supplied
   ; compares to a \\server\share name, when the file is on a network server.

   (if (> comLineNfo 1)(princ "\nCheck if exists & not open"))
   (if
      (or
         (/= (DOS_FILEP get_file) T)
         (= (DOS_UNCPATH get_file) cur_file)
         (= cur_file get_file)
      )
      (progn
         (if
            (or
               (= (DOS_FILEP get_file) T)
               (= (DOS_UNCPATH get_file) cur_file)
            )
            (setq *MFT_logtext "File already open (local);Skip"
                  skipIt T
                  drwOpen T
            )
         ; else
            (progn
               (setq *MFT_logtext "File not found;Skip" skipIt T)
               (if (= wfo 1)
                  (DOS_MSGBOX "File not found." "Warning" 1 1 10)
               )
            )
         )
      )
   ; else...
      (progn
         ; Check if the versions of ACAD and the file to open actually match
         (setq verChk (versionMatch (atoi (substr (getvar "acadver") 1 2)) get_file))
         (cond
            ((= verChk 0) ; fail
               (setq *MFT_logtext "File version newer:Skip" skipIt T)
               (alert "This drawing requires a higher version of AutoCAD")
            )
            ((= verChk 1) ; unknown
               (setq *MFT_logtext "File version unknown/new:Skip" skipIt T)
               (alert "Your ACAD & File version are both newer as MFT currently supports.")
            )
            ((or (= verChk 2)(= verChk 4)) ; Versions match
               ; Check the currently open file & file to open for read-only attribute
               ; ---------------------------------------------------------------------
               ; Although DWGPREFIX & DWGNAME return a complete file location & name,
                ; the file might not exist! Such is the case with a new (blank) unsaved
               ; file. (drawing1.dwg etc.) We exclude new / unsaved files from saving
               ; by 'forcing' them as 'Read-Only'. This means the answer will be
               ; 'Yes' when asked to 'Really discard changes?'
               (if (> comLineNfo 1)(princ "\nCheck if current read-only"))
               (if (= (DOS_FILEP cur_file) T)
                  (progn
                     (setq cur_att (fileattrib cur_file))
                     (if (and (/= cur_att "R")(= ssf 1))
                        (progn
                           (if (> comLineNfo 1)(princ "\nCurrent file read-only"))
                           (setq cur_att "R")
                        )
                     )
                  )
                  ; Non existing (new) file
                  (setq cur_att "R")
               )
               ; in case of a read-only file:
               ; ---------------------------------------------------------------------
               ; Warn user if a file is read-only, when warnings are desired
               ; Optionally user can open it read-only or skip the file
               ; Can be automatically skipped (&logged)/opened (read-only) for
               ; unatained processing.
               (if (> comLineNfo 1)(princ "\nCheck if next read-only"))
               (setq get_att (fileattrib get_file))                                   ; get fileattribute (R for read-only)
               (if (= get_att "R")                                                    ; when file is Read-Only
                  (progn
                     (if (> comLineNfo 1)(princ "\nNext file read-only"))
                     (setq retry T)
                     (while (= retry T)
                        (setq retry nil)
                        (if (= wro 1)
                           (progn
                              (if (= sro 1)
                                 (setq buttons (list "Skip" "Retry"))
                                 (setq buttons (list "Skip" "Retry" "Open Read-Only"))
                              ) ; end if/else
                              (setq usr_choice (DOS_MSGBOXEX (strcat "File is Read-Only\n" get_file) "Warning" buttons 1))
                              (if (= usr_choice 0)(setq *MFT_logtext "File is Read-only;skipped by user" skipIt T)) ; skip the file
                              (if (= usr_choice 1)(setq retry T))                     ; retry
                           )
                        ; else
                           (if (= sro 1)
                              (setq *MFT_logtext "Read-Only;skipped auto" skipIt T)
                           )
                        )
                     )
                  )
               )
               ; in case its already open : by other user on a network
               ; ---------------------------------------------------------------------
               ; Its possible somebody else opens a drawing while user is processing
               ; the same files if they are stored on a network drive.
               (if (> comLineNfo 1)(princ "\nCheck for other users"))
               (setq fileopen (DOS_OPENP get_file))                                   ; check if open on network
               (if (and (not skipIt)(= fileopen T))                                   ; when file open : network
                  (if (= wfo 1)
                     (progn
                        (setq retry T)
                        (while (= retry T)
                           (if (= sro 1)
                              (setq buttons (list "Skip" "Retry"))
                              (setq buttons (list "Skip" "Retry" "Open Read-Only"))
                           )
                           (setq usr_choice (DOS_MSGBOXEX (strcat "File is already open (by another user)\n" get_file) "Warning" buttons 1))
                           (if (= usr_choice 0)(setq *MFT_logtext "File is open(:network);skip by user" skipIt T)) ; skip the file
                           (if (= usr_choice 1)(setq retry T))                        ; retry
                        )
                     )
                  ; else
                     (if (= sro 1)
                        (setq *MFT_logtext "File is open(:network);auto skip" skipIt T)
                     )
                  )
               )
               ; Determine "answer variant" for the _.fileopen command
               ; ---------------------------------------------------------------------
               ; Now we have all our info on the current file and the one to open
               ; automatically or by user interaction, the correct answers for the
               ; fileopen command can be determined.
               (if (> comLineNfo 1)(princ (strcat "\nFile to open is: " (if SkipIt "Skipped\n" "Ready to open\n"))))
               (if (not SkipIt)
                  (progn
                     (princ (getvar "DBMOD"))
                     (setq cmd "(command \"_.fileopen\" ")
                     (setq file_ext (strcase (substr get_file (- (strlen get_file) 2) 3)))

                     ; if current file changed
                     (if
                        (and
                           (/= (getvar "DBMOD") 0)
                           (/= (getvar "DBMOD") 32)
                        )
                        (if (= cur_att "R")
                           (setq cmd (strcat cmd "\"_Y\" "))
                           (setq cmd (strcat cmd "\"_N\" "))
                        )
                     )
                     (if (= file_ext "DXF")
                        (setq cmd (strcat cmd "\"DXF\" \"16\" \"_Y\" "))
                     )
                     (princ (strcat "\n" get_att))
                     (setq cmd (strcat cmd "\"" get_file "\""))
                     (if (= get_att "R")
                        (setq cmd (strcat cmd " \"_Y\""))
                     )
                     (setq cmd (strcat cmd ")"))
                     (if (> comLineNfo 1)(print cmd))
                     (eval (read cmd))
                     (setq drwOpen T)
                  )
               )
            )
         )
      )
   )

   (if *MFT_logtext (princ *MFT_logtext))
   drwOpen
)

; =============================== END MFT_Open =================================
; /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
; =============================== MFT_Process ==================================

(defun MFT_Process ( MFT_lst cmd_lst / dia_abort SDI LISPINIT )

; function to process a list of filenames with a list of commands
; MFT_lst = list of filenames
; cmd_lst = list of commands
;
; The files will be processed with a function around fileopen for realtime checking.
; This allows for user action mid process, in case of read-only files.
; and to skip any subsequent action associated with the file.

; --- error function -----------------------------------------------------------

   (defun *error* ( msg / )
      ; turn off any progression bar that might be active
      ; turn off the sandclock cursor in case its on
      ; Restore sysvars
      (if DOS_VERSION
         (progn
            (DOS_GETPROGRESS T)
            (DOS_WAITCURSOR)
         )
      )
;      (if sdi      (setvar "SDI"      sdi))
      (if lispinit (setvar "LISPINIT" lispinit))
      (if
         (or
            (= msg "Function cancelled")
            (= msg "quit / exit abort")
         )
         (princ)
         (princ (strcat "\nError: " msg))
      )
   )

; --- CloseAllButActive --------------------------------------------------------

   (defun CloseAllButActive ( TorF / cntr vlaxtf name)
      ; Close all documents except active with or without saving
      ; TorF = :vlax-True or :vlax-False
      ; Use T to save and close, F/nil for close without saving
      ; Returns number of documents that were closed
      (setq cntr 0 vlaxtf (if TorF :vlax-True :vlax-False))
      (vlax-for Item (vla-get-Documents (vlax-get-acad-object))
         (if (= (vla-get-Active Item) :vlax-False)
            (progn
               (setq
                  name (vla-get-fullname item)
                  name (if (= name "") (vla-get-name item) name)                ; if not saved yet use file name only
               )
               (princ (strcat "\nClosing -> " name))
               (vla-close Item TorF)
               (setq cntr (1+ cntr))
            )
         )
      )
      cntr
   )

; --- cns ----------------------------------------------------------------------

   (defun cns ( / cntr)
      ; Closes all documents without save except the active
      (setq cntr (CloseAllButActive F))
      (if (> cntr 0)
         (princ (strcat "\n" (itoa cntr) " document" (if (> cntr 1) "s" "") " | closed without saving."))
         (princ "\nNo documents closed.")
      )
      (princ)
   )

; --- cas ----------------------------------------------------------------------

   (defun cas ( / cntr)
      ; Saves and closes all documents except the active
      (setq cntr (CloseAllButActive T))
      (if (> cntr 0)
         (princ (strcat "\n" (itoa cntr) " document" (if (> cntr 1) "s" "") " saved and closed."))
         (princ "\nNo documents closed.")
      )
      (princ)
   )

; --- numDocs ------------------------------------------------------------------

   (defun numDocs ( / ao dc cntr)
      ; Check for open drawings, returns number
      (setq cntr 0)
      (setq ao (vlax-get-Acad-Object))
      (setq dc (vla-get-documents ao))
      (vlax-for <doc> dc
         (setq cntr (1+ cntr))
      )
      cntr
   )

; --- @numerify ----------------------------------------------------------------

   (defun @numerify (str)
      ; By John Uhden
      ; example: (mapcar '@numerify '("kjsdlkj" "#123" "#23'4.5" "#245.67" "#pi"))
      ; returns: ("kjsdlkj" 123 280.5 245.67 PI)
      (cond
         ((wcmatch str "`##,`###,`####,`######")
            (atoi (substr str 2))
         )
         ((wcmatch str "`##'*,`###'*,`####'*,`#####'*")
            (distof (substr str 2) 4)
         )
         ((wcmatch str "`##*")
            (distof (substr str 2))
         )
         ((= (strcase str) "#PI")
            (read (substr str 2))
         )
         (1 str)
      )
   )

; --- execScr ------------------------------------------------------------------

   (defun execScr ( cmd_lst / part lsp_str first alwaysLoad autosave autosort cmd_var countr
                    comLineNfo confdia cur_dwg dcl_id dia_abort DLpath drawing_name
                    duplct extno file_w FILEDIA filetype filter ftabstate isOpen lispFile
                    LISPINIT logSkip MFT_data MFT_dcl MFT_lst MFT_files MFT_logfile
                    MFT_ini MFT_SkipIt msg noDuplicate noIni noRecover pathname progBar
                    recursive rcvr skip_readonly skip_savefile user_path warn_notfound
                    warn_readonly rdonly SDI warn_fileopen wildcard loadLisp loadScr start_dwg
                    first part scr_lst
                  )
      ; to prevent any script code to accidentally or deliberately mess with
      ; any of the MFT variables, they are declared as local to this function.

      (foreach part cmd_lst
         (setq first (substr part 1 1))
         (if (= first "(")
            (progn
               (if scr_lst
                  (progn
                     (vl-catch-all-apply 'eval (apply 'command (mapcar '@numerify scr_lst)))
                     (setq scr_lst nil)
                  )
               )
               (vl-catch-all-apply 'eval (read part))
            )
         ; else
            (setq scr_lst (append scr_lst (list part)))
         )
      )
      (if scr_lst
         (vl-catch-all-apply 'eval (apply 'command (mapcar '@numerify scr_lst)))
      )
   )

; =============================== MAIN SEGMENT =================================

   ; For processing all dwg's in sequence, SDI mode needs to be on
   ; This means no more then one file can be open before starting.
   ; If there are, ask user what to do: close all open drawing
   ; except current with or without saving or cancel the operation.

   ; --- INIT ------------------------------------------------------------------
   (if (not cfgLoaded)(setq cfgLoaded (MFT_Config "r")))

   ; --- Interaction -----------------------------------------------------------
   
   (if (> (numDocs) 1)
      (progn
         (setq dia_txt "\nThis function requires SDI mode to run.\nWould you like to close all other drawings?")
         (if (> comLineNfo 1)(princ dia_txt))
         (setq buttons (list "Save & Close" "Close (no save)" "Cancel"))
         (setq confirm (DOS_MSGBOXEX dia_txt "MFT" buttons 1))
         (princ confirm)
         (if (/= confirm 2)
            (progn
               (if (= confirm 0) (cas))
               (if (= confirm 1) (cns))
            )
            (setq dia_abort 1)
         )
      )
   )

   (if (/= dia_abort 1)
      (progn
         (setq dia_txt (strcat "Ok to start processing " (itoa (length MFT_lst)) " file(s) ?"))
         (if (> comLineNfo 1)(princ dia_txt))
         (setq confirm (DOS_MSGBOX dia_txt "MFT" 2 4))
         (if (= confirm 1)
            (setq dia_abort 1)
         )
      )
   )

   ; --- Process ---------------------------------------------------------------
   ; Its time to begin the process, unless user canceled along the way
   (if (/= dia_abort 1)
      (progn
         (if (> comLineNfo 0)(princ "\nProcessing files..."))
         (setq SDI       (getvar "SDI")
               LISPINIT  (getvar "LISPINIT")
               cmdecho   (getvar "CMDECHO")
               countr    0
               start_dwg (parsePath (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
         )
;         (setvar "SDI"      1)
         (setvar "LISPINIT" 0)
         (setvar "CMDECHO"  0)

         (if (= progBar "1")
            (DOS_GETPROGRESS "MFT" "Please wait..." (length MFT_lst))           ; initiate progression bar
         )

         (if (= logSkip "1")
            (progn
               (setq output (open logFile "w"))                                 ; open log (over)write
               (write-line "filename;reason;action" output)                     ; write header
               (close output)                                                   ; close log
            )
         )

         ; walk through the files
         (foreach file MFT_lst
            ; Open the file (& close current)
            (if (> comLineNfo 1)(princ (strcat "\nOpening file: " file)))
            (setq isOpen (MFT_Open file warn_notfound warn_fileopen warn_readonly skip_readonly skip_savefile))
            (if isOpen
               (progn
                  (if (= loadLisp "1")
                     (progn
                        (if (> comLineNfo 0)(princ (strcat "\nLoading lisp file: " lispFile)))
                        (execLsp lispFile)
                     )
                  )
                  (if (= loadScr "1")
                     (progn
                        (if (> comLineNfo 0)(princ "\nProcessing script"))
                        (execScr cmd_lst)
                     )
                  )
               )
               (progn
                  ; In case the file to be opened got skipped
                  (setq countr (1+ countr))                                     ; file was skipped
                  ; it can be written to a logfile including the reason why
                  (princ (strcat "\n! Warning: " file ";" *MFT_logtext))        ; - Command line warning text
                  (if (and (= (DOS_FILEP logFile) T)(= logSkip "1"))            ; log exists
                     (progn
                        (if (> comLineNfo 1)(princ "\nWriting log"))
                        (setq output (open logFile "a"))                        ; open log (add)
                        (write-line (strcat file ";" *MFT_logtext) output)      ; write info
                        (close output)                                          ; close log
                     )
                  )
               )
            )
            (if (= progBar "1")
               (DOS_GETPROGRESS -1)                                             ; advance the progression bar
            )
         )
         (if (> comLineNfo 0)(princ "\nAll files processed"))

         ; (re)open start drawing, if it exists (not so if run from new drawing)
         (if (> comLineNfo 1)(princ "\nReopen start drawing or leave at last"))
         (if (= (DOS_FILEP start_dwg) T)
            (MFT_Open start_dwg warn_notfound warn_fileopen warn_readonly skip_readonly skip_savefile)
            ; more likely, the function gets run from a new/blank drawing
            ; when using SDI mode we could create a new/blank drawing
         ; but this is no longer the case, so we leave it at the last drawing
;            (if (= (fileattrib (strcat (getvar "DWGPREFIX") (getvar "DWGNAME"))) "R")
;               (command "_.new" "y")
;               (command "_.new")
;            )
         )
         (if (= progBar "1")
            (DOS_GETPROGRESS T)                                                 ; close the progression bar

         )
         ; Check and report if files where skipped
         (setq msg "MFT : Process Complete")
         (if (> comLineNfo 1)(princ (strcat "\nReport skipped: " (itoa countr))))
         (if (/= countr 0)
            (progn
               (setq msg (strcat msg "\n" (itoa countr) " file(s) skipped"))
               (if (= logSkip "1")
                  (progn
                     (setq confdia (DOS_MSGBOX (strcat msg "\nWould you like to view the logfile ?") "Done" 2 1))
                     (if (/= confdia 1)
                        (DOS_SHELLEXE "notepad.exe" MFT_logfile)
                     )
                  )
               )
            )
         )
         (DOS_MSGBOX msg "Done" 1 1)

         ; restore sysvars
;         (setvar "SDI"      SDI)
         (setvar "LISPINIT" LISPINIT)
         (setvar "CMDECHO"  cmdecho)
         (princ "\nMFT : Process Complete")
      )
   )
)

; ============================= END MFT_Process ================================
; /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
; =================================== MFT ======================================

(defun c:MFT ( / alwaysLoad cmd_lst cmd_str cmd_var comLineNfo dcl_id dia_abort dialog_action
                 ftabstate lispFile loadLisp
                 loadScr logFile logSkip MFT_data MFT_dcl MFT_filelist MFT_files
                 MFT_ini MFT_lst MFT_scr MFT_SkipIt MFTwelcome progbar skip_readonly
                 skip_savefile warn_fileopen warn_notfound warn_readonly)

   ; Main Function for MFT.
   ; Main dialog for setting the various filehandling options, selecting a AutoCAD
   ; LISP file to load with each drawing and/or script/commands/macro's to be executed,
   ; and to select all the files, in various ways, to be processed.

; --- error function -----------------------------------------------------------

   (defun *error* ( msg / )
      (if
         (or
            (= msg "Function cancelled")
            (= msg "quit / exit abort")
         )
         (princ)
         (princ (strcat "\nError: " msg))
      )
   )

; ------------------------------------------------------------------------------

   (defun getLisp ( / file)
      ; function to select lisp file (*.lsp)
      (if (> comLineNfo 1)(princ "\nGet LISP file"))
      (setq file (DOS_GETFILED "Select AutoCAD LISP file:" MFT_data "AutoCAD LISP file (*.lsp)|*.lsp|All files (*.*)|*.*||"))
      (if file
         (progn
            (setq lispFile (parsePath file))
            (set_tile "lispFile" lispFile)
         )
      )
   )

; ------------------------------------------------------------------------------

   (defun checkLisp ( / )
      ; function to check for user lisp file (*.lsp)
      (if (> comLineNfo 1)(princ "\nChecking for LISP"))
      (if (not (findfile lispFile))
         (progn
            (setq lispFile nil)
            (set_tile "lispFile" "")
         )
      )
   )

; ------------------------------------------------------------------------------

   (defun getLog ( / file)
      ; function to select logfile (*.csv)
      (if (> comLineNfo 1)(princ "\nSelect logfile"))
      (setq file (DOS_GETFILED "Select logfile:" MFT_data "logfile (*.csv)|*.csv|All files (*.*)|*.*||"))
      (if file
         (progn
            (setq logfile (parsePath file))
            (set_tile "logFile" logfile)
         )
      )
   )
   
; ------------------------------------------------------------------------------

   (defun enableLog (togglethis / )
      ; function to toggle Dialog entities (Enabled/Disabled) for setting logfile
      (if (= togglethis "skiplog")
         (if (= logSkip "1")
            (progn
               (mode_tile "logFile" 1)
               (mode_tile "browselog" 1)
               (setq logSkip "0")
            )
         ; else
            (progn
               (mode_tile "logFile" 0)
               (mode_tile "browselog" 0)
               (setq logSkip "1")
            )
         )
      )
   )

; ------------------------------------------------------------------------------

   (defun end_check ( / err contro)
      ; function to check if anything was selected & saved
      (if (> comLineNfo 1)(princ "\nDialog end check"))
      (cond
         ; Check for lisp file
         ((and (= loadLisp "1")(not lispFile))
            (setq err T)
            (DOS_MSGBOX "No Lisp.\nPlease select a lisp file first." "Error" 1 1)
         )
         ; Check for commands
         ((and (= loadScr "1")(not cmd_str))
            (setq err T)
            (DOS_MSGBOX "No script.\nPlease supply commands first." "Error" 1 1)
         )
         ; Check if lisp file and/or commands where set active
         ((and (= loadLisp "0")(= loadScr "0"))
            (setq err T)
            (DOS_MSGBOX "Nothing to do.\nPlease select a lisp file or commands to execute." "Error" 1 1)
         )
         ; Check for logfile
         ((and (= logSkip "1")(not logFile))
            (setq err T)
            (DOS_MSGBOX "No logfile.\nPlease supply a logfile first." "Error" 1 1)
         )
         ; Check if file's where selected
         ((not MFT_lst)
            (setq err T)
            (DOS_MSGBOX "No file(s) selected.\nPlease select files first." "Error" 1 1)
         )
      )
      (if (not err)
         (progn
            (if (> comLineNfo 1)(princ "\nCheck ok"))
            (write_ini)
            (done_dialog 4)
         )
         (if (> comLineNfo 1)(princ "\nCheck failed"))
      )
   )

; --- setCMD -------------------------------------------------------------------

   (defun setCMD ( box / file_w i lnnr line first eolc inlist str)

      ; sets cmd_var = complete file, for editing. Holds the original string including macro's & comments
      ; sets cmd_str = string for script preview in main dialog, macro style using ;
      ; sets cmd_lst = list of code used for eval (stripped of comments, macro's converted

      (if (> comLineNfo 1)(princ "\nSet commands list"))
      (if (= box 1)
         (progn
            (setq cmd_var (cond ((DOS_EDITBOX "Script editor" "Enter script/macro/command code" cmd_var))(cmd_var)))
            (if cmd_var
               (progn
                  (setq file_w (open MFT_scr "w"))
                  (write-line cmd_var file_w)
                  (close file_w)
               )
            )
         )
      )
      (if (> comLineNfo 1)(princ "\nRead .scr"))
      (setq cmd_str "" cmd_lst nil)
      (if (findfile MFT_scr)
         (progn
            (setq file_r  (open MFT_scr "r")
                  lnnr    1
                  cmd_var ""
            )
            (while (setq line (read-line file_r))
               (if (> comLineNfo 1)(princ "\nReading line"))
               (if (= lnnr 1)
                  (setq cmd_var (strcat cmd_var line))
                  (setq cmd_var (strcat cmd_var "\n" line))
               )
               (setq line (vl-string-left-trim " " line))
               (setq lnnr 2)
               (setq first (substr line 1 1))
               (if (/= first ";")
                  (if (= first "^")
                     ; Convert macro to list
                     (progn
                        (if (> comLineNfo 1)(princ "\nMacro found"))
                        (setq cmd_lst (append cmd_lst (@str2list (substr line 5) ";")))
                     )
                  ; else...
                     (progn
                        (setq line (cutCmnt line))
                        (cond
                           ; Lisp detection method by CodeDing (AutoCad forum)
                           ; If we're in a list, keep accumulating lines until list completed.
                           (inList
                              (setq str (strcat str " " line))
                              (if (listp (vl-catch-all-apply 'read (list str)))
                                 (setq inList  nil
                                       cmd_lst (append cmd_lst (list (strcat cmd_str str)))
                                       str     nil
                                 )
                              )
                           )
                           ; If we detect a list for the current line, add it as is
                           ((eq "(" (substr line 1 1))
                              (if (listp (vl-catch-all-apply 'read (list line)))
                                 (setq cmd_lst (append cmd_lst (list (strcat cmd_str line))))
                              ;else we are in a list
                                 (setq str line inList t)
                              )
                           )
                           (t
                              (progn
                                 ; convert script to list
                                 (if (vl-string-position (ascii " ") line)
                                    (setq line (@str2list line " "))
                                    (setq line (list line))
                                 )
                                 (setq cmd_lst (append cmd_lst line))
                              )
                           )
                        )
                     )
                  )
                  ; else skip comment
               )
            )
            (close file_r)
            (if inlist
               ; if we are still supposed to be inside a list (lisp)
               ; we are dealing with a malformed list
               (alert "WARNING: Malformed list detected!")
            )
            (setq lnnr 1)
            (if cmd_lst
               (foreach line cmd_lst
                  (if (= lnnr 1)
                     (setq cmd_str (strcat cmd_str line))
                     (setq cmd_str (strcat cmd_str ";" line))
                  )
                  (setq lnnr 2)
               )
            )
         )
      )
      (if cmd_str (set_tile "scrFile" cmd_str))
   )

; ------------------------------------------------------------------------------

   (defun cutCmnt (line / pt pos spos qpos p2pos rdy)
      (setq pt (vl-string-subst "\\\'" "\\\"" line); replace nested "
            pos 0
      )
      (while (not rdy)
         (setq qpos (vl-string-position (ascii "\"") pt)
               spos (vl-string-position (ascii ";") pt)
         )
         (if spos ; there is a ;
            (cond
               ((not qpos) ; there is no "
                  (setq pos (+ pos spos) rdy T)
               )
               ; the first ; comes after the first "
               ((and (> qpos 0)(< qpos spos))
                  (setq pt (substr pt (+ 2 qpos))
                        q2pos (vl-string-position (ascii "\"") pt)
                  )
                  (if q2pos
                     (setq pt (substr pt (+ 2 q2pos))
                           pos (+ pos (+ 2 (+ qpos q2pos)))
                     )
                     ; else there is no comment
                     (setq pos 0 rdy T)
                  )
               )
               ; the first ; comes before the first "
               ((> qpos spos)
                  (setq pos (+ pos spos) rdy T)
               )
               (t (setq rdy T)) ; just in case
            )
            (setq rdy T) ; there is no comment
         )
      )
      (if (> pos 0) ; there is a comment
         (setq line (substr line 1 pos))
      )
      line
   )

; ------------------------------------------------------------------------------

   (defun @str2list (str pat / i j n lst)
      ; By John.Uhden
      ; Function to convert a string with delimiters into a list
      ; pat is the delimiter and can contain multiple characters
      (cond
         ((/= (type str)(type pat) 'STR))
         ((= str pat)'(""))
         (T
            (setq i 0 n (strlen pat))
            (while (setq j (vl-string-search pat str i))
               (setq lst (cons (substr str (1+ i)(- j i)) lst)
                     i (+ j n)
               )
            )
            (reverse (cons (substr str (1+ i)) lst))
         )
      )
   )

; ------------------------------------------------------------------------------

   (defun enableLisp ( / )
      ; function to toggle Dialog entities (Enabled/Disabled) for setting lispfile
      (if (= loadLisp "1")
         (progn
            (mode_tile "lispFile" 1)
            (mode_tile "browselsp" 1)
            (setq loadLisp "0")
         )
      ; else
         (progn
            (mode_tile "lispFile" 0)
            (mode_tile "browselsp" 0)
            (setq loadLisp "1")
         )
      )
   )

; ------------------------------------------------------------------------------

   (defun enableScr ( / )
      ; function to toggle Dialog entities (Enabled/Disabled) for setting commands (scr file)
      (if (= loadScr "1")
         (progn
            (mode_tile "editScr" 1)
            (setq loadScr "0")
         )
      ; else
         (progn
            (mode_tile "editScr" 0)
            (setq loadScr "1")
         )
      )
   )

; ------------------------------------------------------------------------------

   (defun getFiles ( / GF_lst)
      (setq GF_lst (MFT_getFiles MFT_lst))
      (if GF_lst (setq MFT_lst GF_lst))
      (if MFT_lst
         (progn
            (set_tile "num_dwgs" (itoa (length MFT_lst)))                       ; display the number of files
            (mode_tile "accept" 0)
         )
         (mode_tile "accept" 1)
      )
   )

; ------------------------------------------------------------------------------

   (defun MFTnfo ( / nfoFile line nfo_lst rdy)
      (if (> comLineNfo 0)(princ "\nDisplay info"))
      (setq nfoFile (strcat MFT_files "MFT_info.txt"))
      (if (findfile nfoFile)
         (DOS_SHELLEXE "notepad.exe" nfoFile)
      )
   )

; =============================== MAIN SEGMENT =================================

   ; --- Init ------------------------------------------------------------------
   (setq cfgLoaded (MFT_Config "r"))

   ; pop-up the info text file, if this is (likely) the first run
   (if MFTwelcome (MFTnfo))

   ; --- Interaction -----------------------------------------------------------
    (if (> comLineNfo 1)(princ "\nInit dialog"))
   (setq dcl_id (load_dialog MFT_dcl))
   (if (not (new_dialog "MFT_Dia" dcl_id)) (exit))
   (set_tile "caption" "Welcome to the Multi File Tool\nPlease supply a lisp file to load and/or enter/import the command/script/macro/lisp code you wish to run on each file.\nYou can set options for how to handle various situations during the process & select your files for processing.")
   (set_tile "warn_notfound" warn_notfound)
   (set_tile "warn_readonly" warn_readonly)
   (set_tile "warn_fileopen" warn_fileopen)
   (set_tile "skip_readonly" skip_readonly)
   (set_tile "skip_savefile" skip_savefile)
   (set_tile "logSkip"       logSkip)
   (set_tile "loadLisp"      loadLisp)
   (set_tile "alwaysLoad"    alwaysLoad)
   (set_tile "loadScr"       loadScr)
   (set_tile "progBar"       progBar)

   (if logFile
      (set_tile "logFile" logFile)
   )
   (if (= logSkip "0")
      (progn
         (mode_tile "logFile"   1)
         (mode_tile "browselog" 1)
      )
   )
   (if lispFile
      (progn
         (set_tile "lispFile" lispFile)
         (checkLisp)
      )
   )
   (if (= loadLisp "0")
      (progn
         (mode_tile "lispFile"  1)
         (mode_tile "browselsp" 1)
      )
   )
   (if (= loadScr "0")
      (progn
         (mode_tile "editScr" 1)
      )
   )

   ; Process the scriptfile, if present
   (mode_tile "scrFile" 1)
   (if (findfile MFT_scr)
      (setCMD 0)
   )

   ; if a script is present, show it
   (if cmd_str (set_tile "scrFile" cmd_str))

   ; auto load last filelist, if required
   (if (and (= autosave "1") (= (DOS_FILEP (strcat MFT_data "MFTFiles.lst")) T))
      (load_list (strcat MFT_data "MFTFiles.lst"))
      (check_list nil nil)
   )

   ;if a list is present, count & show
   (if MFT_lst
      (progn
         (set_tile "num_dwgs" (itoa (length MFT_lst)))                          ; display the number of files
         (mode_tile "accept" 0)
      )
      (mode_tile "accept" 1)
   )

   ; ------ Dialog actions ------
   
   (if (> comLineNfo 1)(princ "\nStarting dialog actions"))
   (setq dialog_action 7)

   (while (/= dialog_action 4)
      (action_tile "loadLisp"      "(enableLisp)")
      (action_tile "lispFile"      "(setq lispFile $value)(checkLisp)")
      (action_tile "browselsp"     "(getLisp)")
      (action_tile "loadScr"       "(enableScr)")
      (action_tile "editScr"       "(setCMD 1)")
      (action_tile "logSkip"       "(enableLog \"skiplog\")" )
      (action_tile "logFile"       "(setq logFile $value)" )
      (action_tile "browselog"     "(getLog)")
      (action_tile "info"          "(MFTnfo)")

      (action_tile "warn_notfound" "(setq warn_notfound $value)" )
      (action_tile "warn_readonly" "(setq warn_readonly $value)" )
      (action_tile "warn_fileopen" "(setq warn_fileopen $value)" )
      (action_tile "skip_readonly" "(setq skip_readonly $value)" )
      (action_tile "skip_savefile" "(setq skip_savefile $value)" )
      (action_tile "progBar"       "(setq progBar       $value)" )
      (action_tile "select"        "(getFiles)" )

      (action_tile "accept"        "(end_check)")                               ; auto save the current list (when on), on user accept
      (action_tile "cancel"        "(setq dia_abort 1)(done_dialog 4)")         ; user aborts

      (setq dialog_action (start_dialog))
   )
   (unload_dialog dcl_id)

   ; ------ Confirmation ------
   
   (if (/= dia_abort 1)
      (progn
         (MFT_Config "w")
         (MFT_Process MFT_lst cmd_lst)
      )
      (princ "\n*Cancel*")
   )
   (princ)
)

; ================================== END MFT ===================================

; --- On Load ------------------------------------------------------------------

(princ "\nMulti File Tool loaded. Use: MFT to start.")
(princ)

; ==============================================================================
;                                  -={ END }=-
;                                     '-=-'
