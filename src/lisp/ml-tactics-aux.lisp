;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ml)
(part-of ml-tactics)

(deffile ml-tactics-aux
  (part-of ml-tactics)
  (extension lisp)
  (mhelp "Auxiliary functions/tactics needed by ML tactics."))

(context aux-tactics)

(deftactic universal-goal-p
  (nat-ded
   (lambda (pline)
     (let ()
       (if (and (boundwff-q (line-assertion pline))
		(eq 'forall (cdar (line-assertion pline))))
	   (progn
	     (tactic-output "Goal is universal." t)
	     (values (list pline) "Goal is universal." 'succeed))
	   (progn
	     (tactic-output "Goal not universal." nil) 
	     (values (list pline) "Goal not universal." 'fail)
	    ))))
   "Returns success if planned line is universally quantified."))

