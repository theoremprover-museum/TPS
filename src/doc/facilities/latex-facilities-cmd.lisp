;very short facilities guide
;just commands in TPS (but not grader) and flags, nothing else.

(let ((show-all-packages T)
      (*print-pretty* t))
;;; Last Modified: Thu Jun 25 17:18:28 1998
  (reorganize)
  (latex-cats
   '(MEXPR SRULE auto::MATEOP auto::mtreeop auto::unifop testcmd EDOP
	   LIBRARYCMD REVIEWCMD FLAG)
   global-contextlist
;;; Modify this pathname to put the result in the facilities dir, e.g.
;;;(pathname "/usr/theorem/doc/facilities/facilities-temp.tex")
   (pathname "/home/theorem/project/doc/facilities/latex-facilities-cmd.tex")
   ))

