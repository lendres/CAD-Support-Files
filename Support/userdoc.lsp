;;; Copyright Lance A. Endres

(defun S::STARTUP (/ )

		;;; Suppress the data base modified status.
    (acad-push-dbmod)
    
		;;; Establish maintain-ltscale reactor.
    (mapcar 'load (list "maintain-ltscale.vlx"))
    (C:INIT-MAINTAIN-LTSCALE)
    (setvar "regenmode" 1)

		;;; Restore the data base modified status.
    (acad-pop-dbmod)
    
		;;; Silent exit.
    (princ)

) ;_ End defun for startup

;;; Autoload Functions

(AUTOLOAD "adv-insert.vlx" '("in" "inn" "innr" "inr" "adv-insert"))

(AUTOLOAD "addpstyles.vlx" '("addpstyles"))

(AUTOLOAD "angle.lsp" '("angle"))

(AUTOLOAD "arced.lsp" '("arced"))

(AUTOLOAD "arcleader.lsp" '("al"))

(AUTOLOAD "asctext.lsp" '("asctext"))

(AUTOLOAD "autoortho.vlx"
	  '("mirror_ortho_off"
	    "mirror_ortho_on"
	    "rotate_ortho_off"
	    "rotate_ortho_on"
	   )
) ;_ End autoload

(AUTOLOAD "bgtoggle.fas" '("bgtoggle"))

(AUTOLOAD "blockscale.fas" '("blockscale"))

(AUTOLOAD "block-power-tools.vlx"
	  '("rip-nent"
	    "set-nent-color-bylayer"
	    "set-block-color-bylayer"
	    "all-blocks-color-bylayer"
	   )
) ;_ End AUTOLOAD

(AUTOLOAD "breaksnaps.fas" '("ba" "bz"))

(AUTOLOAD "change8x11.vlx" '("change8x11"))

(AUTOLOAD "color-bylayer.fas" '("color-bylayer"))

(AUTOLOAD "ddlayren.vlx" '("ddlayren"))

(AUTOLOAD "ddleutils.vlx" '("ddleutils" "layato"))

(AUTOLOAD "ddxrmod.vlx" '("ddxrmod"))

(AUTOLOAD "defext.fas" '("defext" "ndefext"))

(AUTOLOAD "demo.fas" '("demo"))

(AUTOLOAD "dlayer.fas" '("dlayer"))

(AUTOLOAD "drawinglog.fas" '("drawlog" "drawlogswexport"))

(AUTOLOAD "extract-text-to-file.vlx" '("extract-text-to-file"))

(AUTOLOAD "fastdists.fas" '("distend" "distper"))

(AUTOLOAD "fastflex.fas" '("efc" "efl" "efs" "fc" "fl" "fs"))

(AUTOLOAD "fastviews.fas" '("vr" "vs"))

(AUTOLOAD "fchamfer.lsp" '("fchamfer"))

(AUTOLOAD "gap.fas" '("gap"))

(AUTOLOAD "glue.fas" '("glue"))

(AUTOLOAD "keyed-notes.vlx" '("keyed-notes"))

(AUTOLOAD "leaders.vlx" '("ll" "ell"))

(AUTOLOAD "leroutines.fas"
	  '("copy_mult"	"crstretch" "clean" "ucsob" "update"
	    "linsb")
) ;_ End autoload

(AUTOLOAD "linemid.fas" '("linemid"))

(AUTOLOAD "maintain-ltscale.vlx" '("init-maintain-ltscale" "remove-maintain-ltscale"))

(AUTOLOAD "minsp.fas" '("minsp"))

(AUTOLOAD "MultiFileTool.lsp" '("mft"))

(AUTOLOAD "number.vlx" '("number"))

(AUTOLOAD "ostoggle.fas" '("ostoggle"))

(AUTOLOAD "pipelabel.vlx" '("pipelabel"))

(AUTOLOAD "sestext.vlx" '("sestext"))

(AUTOLOAD "ses_install.vlx" '("roam" "ses_install"))

(AUTOLOAD "setup.vlx" '("setup"))

(AUTOLOAD "space.fas" '("ms" "ps"))

(AUTOLOAD "specs.fas" '("specs"))

(AUTOLOAD "toggletb.fas" '("all-tb-toggle"))

(AUTOLOAD "totallength.fas" '("totallength"))

(AUTOLOAD "txtlay.fas" '("dtext_layer" "mtext_layer"))

(AUTOLOAD "textblocks.fas" '("textblocks"))

(AUTOLOAD "tomtext.vlx" '("tomtext"))

(AUTOLOAD "trimp.fas" '("trimp"))

(AUTOLOAD "unmirror.fas" '("unmirror"))

(AUTOLOAD "verifyplotlayer.fas" '("verifyplotlayer"))

(AUTOLOAD "vbtest.fas" '("vbtest"))

(AUTOLOAD "zeroz.fas" '("zeroz"))

(AUTOLOAD "zooms.fas" '("za" "ze" "zl" "zx" "zz" "-" "*"))