(let ((show-all-packages T)
      (*print-pretty* t)) ; added because of bad pprint function

;;; Last Modified: Wed Jun  1 01:39:09 1988
  (reorganize)
  (scribe-cats
	'(MEXPR SRULE EXTSEQCMD TACTIC TACTICal MATEOP EXTMATECMD mtreeop unifop testcmd MODELSCMD EDOP  REPSYMBOL
	   %THEOREM%  ABBREV BINDER LOGCONST PMPROPSYM TYPECONST TYPEABBREV
	   LIBRARYCMD LIBOBJECT CLASS-SCHEME UNIX-LIBRARYCMD
           REVIEWCMD REVIEW-SUBJECT FLAG FLAG-MODE 
	   teacher:gexpr
	   ; SAVEDWFF
	   EVENT  DEVICE-STYLE
)
   global-contextlist
;;; Modify this pathname to put the result in the facilities dir, e.g.
;;;(pathname "/usr/theorem/doc/facilities/facilities-short.mss")
;;;(pathname "/afs/cs.cmu.edu/project/tps/tps/doc/facilities/facilities-short.mss")
   (pathname "/home/theorem/project/doc/facilities/scribe-facilities-short.mss")
   ))
