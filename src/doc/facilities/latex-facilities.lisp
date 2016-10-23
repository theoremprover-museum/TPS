(let* ((show-all-packages T)
       (*print-pretty* t) ; added because of bad pprint function
       (ordered-catlist1 ; these should appear first
	'(MEXPR SRULE EXTSEQCMD AUTO:TACTIC TACTICal MATEOP EXTMATECMD mtreeop unifop testcmd MODELSCMD EDOP  REPSYMBOL
		%THEOREM%  ABBREV BINDER LOGCONST PMPROPSYM TYPECONST TYPEABBREV
		LIBRARYCMD LIBOBJECT CLASS-SCHEME UNIX-LIBRARYCMD
		REVIEWCMD REVIEW-SUBJECT FLAG FLAG-MODE INFO 
		teacher:gexpr
					 ;SAVEDWFF
		EVENT  
					;LISP-PACKAGE PACKAGE
		LISP-PACK MODULE TPS-FILE TOPLEVEL CONTEXT
		ARGTYPE UTILITY WFFOP core::WFFREC% ;WFFREC
		GETGWFFTYPE FLAVOR
		DEVICE-STYLE PRINTPROP PRINT-FACE
	  ))
       (full-ordered-catlist
	(append ordered-catlist1
		;())))
		(set-difference global-categorylist
				ordered-catlist1))))
  (reorganize)
  (latex-cats
   full-ordered-catlist
   global-contextlist
;;; Modify this pathname to put the result in the facilities dir, e.g.
;;;(pathname "/usr/tps/doc/facilities/facilities.tex")
;;;   (pathname "/home/theorem/project/doc/facilities/facilities-short.tex")
   (pathname "/home/theorem/project/doc/facilities/latex-facilities.tex")
     ))
