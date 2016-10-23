;temporary facilities-guide-maker, for packages we don't always load

(let ((show-all-packages T)
      (*print-pretty* t)) ; added because of bad pprint function

;;; Last Modified: Wed Jun  1 01:39:09 1988
  (reorganize)
  (scribe-cats
   '(;MEXPR SRULE TACTIC TACTICal auto::MATEOP 
	   auto::mtreeop 
	  ; auto::unifop testcmd EDOP  REPSYMBOL
	  ; %THEOREM%  ABBREV BINDER LOGCONST PMPROPSYM TYPECONST TYPEABBREV
	  ; LIBRARYCMD 
          ; REVIEWCMD REVIEW-SUBJECT 
	   FLAG 
	  ; FLAG-MODE 
	  ; teacher:gexpr
	  ; SAVEDWFF
	  ; EVENT  DEVICE-STYLE
)
   global-contextlist
;;; Modify this pathname to put the result in the facilities dir, e.g.
;;;(pathname "/usr/theorem/doc/facilities/facilities-temp.mss")
   (pathname "/home/theorem/project/tps/tps/doc/facilities/scribe-facilities-temp.mss")
   ))
