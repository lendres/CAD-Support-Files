;;; Copyright Lance A. Endres

(defun S::STARTUP (/ )

		;;; Suppress the data base modified status.
    (acad-push-dbmod)
    
		;;; Establish maintain-ltscale reactor.
    (mapcar 'load (list "maintain-ltscale"))
    (C:INIT-MAINTAIN-LTSCALE)
    (setvar "regenmode" 1)

		;;; Restore the data base modified status.
    (acad-pop-dbmod)
    
		;;; Silent exit.
    (princ)

) ;_ End defun for startup

;;; Autoload Functions
(AUTOLOAD "adv-insert" '("in" "inn" "innr" "inr" "adv-insert"))
(AUTOLOAD "addpstyles" '("addpstyles"))
(AUTOLOAD "angle" '("angle"))
(AUTOLOAD "arced" '("arced"))
(AUTOLOAD "arcleader" '("al"))
(AUTOLOAD "asctext" '("asctext"))
(AUTOLOAD "autolayercommands" '("dtextlayer" "mtextlayer" "dimalignedlayer" "dimhorizontallayer" "dimverticallayer"))
(AUTOLOAD "autoortho" '("mirror_ortho_off" "mirror_ortho_on" "rotate_ortho_off" "rotate_ortho_on"))

(AUTOLOAD "bgtoggle" '("bgtoggle"))
(AUTOLOAD "blockscale" '("blockscale"))
(AUTOLOAD "block-power-tools" '("rip-nent" "set-nent-color-bylayer" "set-block-color-bylayer" "all-blocks-color-bylayer"))
(AUTOLOAD "breaksnaps" '("ba" "bz"))

(AUTOLOAD "change8x11" '("change8x11"))
(AUTOLOAD "color-bylayer" '("color-bylayer"))
(AUTOLOAD "createtextstyles" '("createtextstyles"))

(AUTOLOAD "ddlayren" '("ddlayren"))
(AUTOLOAD "ddleutils" '("ddleutils" "layato"))
(AUTOLOAD "ddxrmod" '("ddxrmod"))
(AUTOLOAD "defext" '("defext" "ndefext"))
(AUTOLOAD "demo" '("demo"))
(AUTOLOAD "dlayer" '("dlayer"))
(AUTOLOAD "drawinglog" '("drawlog" "drawlogswexport"))

(AUTOLOAD "extract-text-to-file" '("extract-text-to-file"))

(AUTOLOAD "fastdists" '("distend" "distper"))
(AUTOLOAD "fastflex" '("efc" "efl" "efs" "fc" "fl" "fs"))
(AUTOLOAD "fastviews" '("vr" "vs"))
(AUTOLOAD "fchamfer" '("fchamfer"))

(AUTOLOAD "gap" '("gap"))
(AUTOLOAD "glue" '("glue"))

(AUTOLOAD "keyed-notes" '("keyed-notes"))

(AUTOLOAD "leaders" '("ll" "ell"))
(AUTOLOAD "leroutines" '("copy_mult"	"crstretch" "clean" "ucsob" "update" "linsb"))
(AUTOLOAD "linemid" '("linemid"))
(AUTOLOAD "loadlinetypes" '("loadlinetypes"))

(AUTOLOAD "maintain-ltscale" '("init-maintain-ltscale" "remove-maintain-ltscale"))
(AUTOLOAD "minsp" '("minsp"))
(AUTOLOAD "MultiFileTool" '("mft"))

(AUTOLOAD "number" '("number"))

(AUTOLOAD "ostoggle" '("ostoggle"))

(AUTOLOAD "pipelabel" '("pipelabel"))

(AUTOLOAD "setup" '("setup"))
(AUTOLOAD "space" '("ms" "ps"))
(AUTOLOAD "specs" '("specs"))

(AUTOLOAD "toggletb" '("all-tb-toggle"))
(AUTOLOAD "totallength" '("totallength"))
(AUTOLOAD "textblocks" '("textblocks"))
(AUTOLOAD "tomtext" '("tomtext"))
(AUTOLOAD "trimp" '("trimp"))

(AUTOLOAD "unmirror" '("unmirror"))

(AUTOLOAD "verifyplotlayer" '("verifyplotlayer"))

(AUTOLOAD "vbtest" '("vbtest"))

(AUTOLOAD "zeroz" '("zeroz"))
(AUTOLOAD "zooms" '("za" "ze" "zl" "zx" "zz" "-" "*"))