;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of ext-dags)

;;;
;;; File: EXT-SEQ-TACTICS  - cebrown - 6/2/03
;;; Tactics for constructing extensional sequent derivations

(deffile ext-seq-tactics
    (part-of EXT-DAGS)
  (extension clisp)
  (mhelp "Tactics for Extensional Sequent Calculus."))

(context compound-tactics)

(defextseq go2
  (extseq-argtypes tactic-mode)
  (extseq-argnames tacmode)
  (extseq-arghelp "Mode")
  (extseq-defaultfns
    (lambda (%1)
      (list (if (eq %1 '$) tacmode %1))))
  (extseq-mainfns extseq-go2)
  (mhelp "Apply all possible extensional sequent tactics."))

(defun extseq-go2 (tm)
  (use-tactic 'go2-tac 'ext-seq tm))

(deftactic go2-tac
  (ext-seq
   (repeat
    (orelse
     (call print-routines)
     true+tac
     false-tac
     init-tac
     refl+tac
     lambda-tac
     not-tac
     or+tac
     and-tac
     implies+tac
     all+tac
     exists-tac
     extfunc+tac
     or-tac
     and+tac
     implies-tac
     equiv+tac
     equiv-tac
     exto+tac
     eqo-tac
     all-tac
     exists+tac
     eqfunc-tac
     equivwffs+tac
     equivwffs-tac
     initeq-tac
     dec+tac
     eunif1-tac
     eunif2-tac
     contract-tac
     internalize+tac
     internalize-tac))))

(defun extseq-tactic-choose-wff (wffs &optional (m "") (mend ""))
  (let ((wff-choices (remove-duplicates wffs :test #'wffeq)))
    (extseq-tactic-choose-wff-1 wff-choices m mend)))

(defun extseq-tactic-choose-wff-1 (wffs &optional (m "") (mend ""))
  (if wffs
      (let ((yesno nil))
	(prompt-read yesno nil
		     (msgf m " " ((car wffs) . gwff) mend "?")
		     'yesno t ((? (mhelp 'yesno))))
	(if yesno
	    (car wffs)
	  (extseq-tactic-choose-wff-1 (cdr wffs) m mend)))
    nil))

(defun extseq-tactic-choose-wff-2 (pwff-nwffs &optional (m "") (mmid "") (mend ""))
  (let ((wff-choices (remove-duplicates pwff-nwffs :test #'(lambda (x y)
							     (and (wffeq-ab (car x) (car y))
								  (wffeq-ab (cdr x) (cdr y)))))))
    (extseq-tactic-choose-wff-2-1 wff-choices m mmid mend)))

(defun extseq-tactic-choose-wff-2-1 (pwff-nwffs &optional (m "") (mmid " and ") (mend ""))
  (if pwff-nwffs
      (let ((yesno nil))
	(prompt-read yesno nil
		     (msgf m " " ((caar pwff-nwffs) . gwff) mmid ((cdar pwff-nwffs) . gwff) mend "?")
		     'yesno t ((? (mhelp 'yesno))))
	(if yesno
	    (car pwff-nwffs)
	  (extseq-tactic-choose-wff-2-1 (cdr pwff-nwffs) m mend)))
    nil))

(deftactic true+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (if (member 'TRUTH (ext-seq-wffs es))
	       (let ((msg "Applied TRUE+, closing a subgoal."))
		 (extseq-true (linealias pline))
		 (tactic-output msg t)
		 (values nil msg 'succeed))
	     (let ((msg "Can't apply TRUE+"))
	       (tactic-output msg nil)
	       (values (list pline) msg 'fail)))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic false-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (if (member '(NOT . FALSEHOOD) (ext-seq-wffs es) :test #'wffeq-ab)
	       (let ((msg "Applied FALSE-, closing a subgoal."))
		 (extseq-false (linealias pline))
		 (tactic-output msg t)
		 (values nil msg 'succeed))
	     (let ((msg "Can't apply FALSE-"))
	       (tactic-output msg nil)
	       (values (list pline) msg 'fail)))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic init-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let ((pwff (find-if #'(lambda (x)
				    (member (cons 'NOT x) (ext-seq-wffs es)
					    :test #'wffeq-ab))
				(ext-seq-wffs es))))
	     (if pwff
		 (let ((msg "Applied Init, closing a subgoal."))
		   (extseq-init (linealias pline) pwff)
		   (tactic-output msg t)
		   (values nil msg 'succeed))
	       (let ((msg "Can't apply Init"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic refl+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let ((pwff (find-if #'(lambda (x)
				    (and (equals-p x)
					 (wffeq-ab (cdar x) (cdr x))))
				(ext-seq-wffs es))))
	     (if pwff
		 (let ((msg "Applied Refl, closing a subgoal."))
		   (extseq-refl (linealias pline) pwff)
		   (tactic-output msg t)
		   (values nil msg 'succeed))
	       (let ((msg "Can't apply Refl"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic lambda-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if #'(lambda (x)
				  (wffeq-ab (etanorm (lambda-norm x)) x))
			      (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Lambda Normalize")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied LAMBDA"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-lam ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply LAMBDA"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic not-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (and (not-p x) (not-p (cdr x))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Remove double negation from" " using DNEG")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied DNEG"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-dneg ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply DNEG"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic or+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (or-p x))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply OR+ to")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied OR+"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-or+ ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply OR+"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic and-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (and (not-p x) (and-p (cdr x))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply AND- to")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied AND-"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-and- ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply AND-"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic implies+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (implies-p x))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply IMPLIES+ to")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied IMPLIES+"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-implies+ ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply IMPLIES+"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic all+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (a-bd-wff-p x))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply ALL+ to")
			    (car pwffs)))))
	     (if pwff
		 (let* ((wffs (ext-seq-wffs es))
			(x (bindvar pwff))
			(y (if (find-if #'(lambda (w) (free-in x w)) wffs)
			       (fresh-var (type x) (getnameroot x))
			     x)))
		   (when (eq tacmode 'INTERACTIVE)
		     (prompt-read y nil
				  (msgf "Variable for ALL+")
				  'gwff y ((? (mhelp 'gwff)))))
		   (if (and (symbolp y) (not (anyabbrev-p y)) (not (logconst-p y))
			    (not (binder-p y)) (not (equality-p y))
			    (equal (unabbreviated-type x) (unabbreviated-type y))
			    (not (find-if #'(lambda (w) (free-in y w)) wffs)))
		       (let ((msg "Applied ALL+"))
			 (es-introduce-gap pline 1)
			 (let ((ln (linealias pline)))
			   (extseq-all+ ln (- ln 1) y pwff)
			   (tactic-output msg t)
			   (values (list (esnumalias (- ln 1))) msg 'succeed)))
		     (let ((msg "Can't apply ALL+"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply ALL+"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic exists-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (and (not-p x) (e-bd-wff-p (cdr x))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply EXISTS- to")
			    (car pwffs)))))
	     (if pwff
		 (let* ((wffs (ext-seq-wffs es))
			(x (bindvar (cdr pwff)))
			(y (if (find-if #'(lambda (w) (free-in x w)) wffs)
			       (fresh-var (type x) (getnameroot x))
			     x)))
		   (when (eq tacmode 'INTERACTIVE)
		     (prompt-read y nil
				  (msgf "Variable for EXISTS-")
				  'gwff y ((? (mhelp 'gwff)))))
		   (if (and (symbolp y) (not (anyabbrev-p y)) (not (logconst-p y))
			    (not (binder-p y)) (not (equality-p y))
			    (equal (unabbreviated-type x) (unabbreviated-type y))
			    (not (find-if #'(lambda (w) (free-in y w)) wffs)))
		       (let ((msg "Applied EXISTS-"))
			 (es-introduce-gap pline 1)
			 (let ((ln (linealias pline)))
			   (extseq-exists- ln (- ln 1) y pwff)
			   (tactic-output msg t)
			   (values (list (esnumalias (- ln 1))) msg 'succeed)))
		     (let ((msg "Can't apply EXISTS-"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply EXISTS-"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic extfunc+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x)
				      (and (equals-p x)
					   (let ((atp (unabbreviated-type (cdr x))))
					     (consp atp))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Show extensional equality of functions" " using EXTFUNC")
			    (car pwffs)))))
	     (if pwff
		 (let* ((wffs (ext-seq-wffs es))
			(atp (unabbreviated-type (cdr pwff)))
			(y (fresh-var-1 (cdr atp))))
		   (when (eq tacmode 'INTERACTIVE)
		     (prompt-read y nil
				  (msgf "Variable for EXTFUNC")
				  'gwff y ((? (mhelp 'gwff)))))
		   (if (and (symbolp y) (not (anyabbrev-p y)) (not (logconst-p y))
			    (not (binder-p y)) (not (equality-p y))
			    (equal (cdr atp) (unabbreviated-type y))
			    (not (find-if #'(lambda (w) (free-in y w)) wffs)))
		       (let ((msg "Applied EXTFUNC"))
			 (es-introduce-gap pline 1)
			 (let ((ln (linealias pline)))
			   (extseq-extfunc ln (- ln 1) y pwff)
			   (tactic-output msg t)
			   (values (list (esnumalias (- ln 1))) msg 'succeed)))
		     (let ((msg "Can't apply EXTFUNC"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply EXTFUNC"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic or-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (and (not-p x) (or-p (cdr x))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply OR- to")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied OR- creating two new subgoals."))
		   (es-introduce-gap pline 2)
		   (let ((ln (linealias pline)))
		     (extseq-or- ln (- ln 2) (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply OR-"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic and+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (and-p x))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply AND+ to")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied AND+ creating two new subgoals."))
		   (es-introduce-gap pline 2)
		   (let ((ln (linealias pline)))
		     (extseq-and+ ln (- ln 2) (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply AND+"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic implies-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (and (not-p x) (implies-p (cdr x))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply IMPLIES- to")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied IMPLIES- creating two new subgoals."))
		   (es-introduce-gap pline 2)
		   (let ((ln (linealias pline)))
		     (extseq-implies- ln (- ln 2) (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply IMPLIES-"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic equiv+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (equiv-p x))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Show equivalence" " using EQUIV+")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied EQUIV+ creating two new subgoals."))
		   (es-introduce-gap pline 2)
		   (let ((ln (linealias pline)))
		     (extseq-equiv+ ln (- ln 2) (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply EQUIV+"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic equiv-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (and (not-p x) (equiv-p (cdr x))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Split into cases using equivalence" " with rule EQUIV-")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied EQUIV- creating two new subgoals."))
		   (es-introduce-gap pline 2)
		   (let ((ln (linealias pline)))
		     (extseq-equiv- ln (- ln 2) (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply EQUIV-"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic exto+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x)
				      (and (equals-p x)
					   (eq (unabbreviated-type (cdr x)) 'O)))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Show equality of propositions" " using EXTO")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied EXTO creating two new subgoals."))
		   (es-introduce-gap pline 2)
		   (let ((ln (linealias pline)))
		     (extseq-exto ln (- ln 2) (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply EXTO"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic eqo-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x)
				      (and (not-p x) (equals-p (cdr x))
					   (eq (unabbreviated-type (cddr x)) 'O)))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Split into cases using equality" " with rule EQO")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied EQO creating two new subgoals."))
		   (es-introduce-gap pline 2)
		   (let ((ln (linealias pline)))
		     (extseq-eqo ln (- ln 2) (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply EQO"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic all-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (and (ext-seq-p es) (eq tacmode 'INTERACTIVE))
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (and (not-p x) (a-bd-wff-p (cdr x))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (extseq-tactic-choose-wff pwffs "Apply ALL- to"))))
	     (if pwff
		 (let ((trm nil))
		   (prompt-read trm nil
				(msgf "Instantiation Term for ALL- " ((cdr pwff) . gwff))
				'gwff '$ ((? (mhelp 'gwff))))
		   (if (equal (unabbreviated-type trm) (unabbreviated-type (bindvar (cdr pwff))))
		       (let ((msg "Applied ALL-"))
			 (es-introduce-gap pline 1)
			 (let ((ln (linealias pline)))
			   (extseq-all- ln (- ln 1) trm pwff)
			   (tactic-output msg t)
			   (values (list (esnumalias (- ln 1))) msg 'succeed)))
		     (let ((msg "Type Mismatch - Can't apply ALL-"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply ALL-"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Can't apply ALL-"))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic exists+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (and (ext-seq-p es) (eq tacmode 'INTERACTIVE))
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (e-bd-wff-p x))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (extseq-tactic-choose-wff pwffs "Apply EXISTS+ to"))))
	     (if pwff
		 (let ((trm nil))
		   (prompt-read trm nil
				(msgf "Instantiation Term for EXISTS+ " (pwff . gwff))
				'gwff '$ ((? (mhelp 'gwff))))
		   (if (equal (unabbreviated-type trm) (unabbreviated-type (bindvar pwff)))
		       (let ((msg "Applied EXISTS+"))
			 (es-introduce-gap pline 1)
			 (let ((ln (linealias pline)))
			   (extseq-exists+ ln (- ln 1) trm pwff)
			   (tactic-output msg t)
			   (values (list (esnumalias (- ln 1))) msg 'succeed)))
		     (let ((msg "Type Mismatch - Can't apply EXISTS+"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply EXISTS+"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Can't apply EXISTS+"))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic eqfunc-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (and (ext-seq-p es) (eq tacmode 'INTERACTIVE))
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x)
				      (and (not-p x)
					   (equals-p (cdr x))
					   (consp (unabbreviated-type (cddr x)))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (extseq-tactic-choose-wff pwffs "Apply equality of functions" " to an argument with rule EQFUNC"))))
	     (if pwff
		 (let* ((atp (unabbreviated-type (cddr pwff)))
			(btp (when (consp atp) (cdr atp)))
			(trm nil))
		   (prompt-read trm nil
				(msgf "Instantiation Term for EQFUNC " ((cdr pwff) . gwff))
				'gwff '$ ((? (mhelp 'gwff))))
		   (if (and btp (equal (unabbreviated-type trm) btp))
		       (let ((msg "Applied EQFUNC"))
			 (es-introduce-gap pline 1)
			 (let ((ln (linealias pline)))
			   (extseq-eqfunc ln (- ln 1) trm pwff)
			   (tactic-output msg t)
			   (values (list (esnumalias (- ln 1))) msg 'succeed)))
		     (let ((msg "Type Mismatch - Can't apply EQFUNC"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply EQFUNC"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Can't apply EQFUNC"))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic equivwffs+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x) (atom-head-abbrev-p x))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Expand abbreviation in" " using EQUIVWFFS+")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied EQUIVWFFS+"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-equivwffs+ ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply EQUIVWFFS+"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic equivwffs-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x)
				      (and (not-p x) (atom-head-abbrev-p (cdr x))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Expand abbreviation in" " using EQUIVWFFS-")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied EQUIVWFFS-"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-equivwffs- ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply EQUIVWFFS-"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic internalize+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x)
				      (not (wffeq (externalize-wff1 x) x)))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply INTERNALIZE+ to")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied INTERNALIZE+"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-internalize+ ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply INTERNALIZE+"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic internalize-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let* ((pwffs 
		   (remove-if-not #'(lambda (x)
				      (and (not-p x)
					   (not (wffeq (externalize-wff1 (cdr x)) (cdr x)))))
				  (ext-seq-wffs es)))
		  (pwff (when pwffs
			  (if (eq tacmode 'INTERACTIVE)
			      (extseq-tactic-choose-wff pwffs "Apply INTERNALIZE- to")
			    (car pwffs)))))
	     (if pwff
		 (let ((msg "Applied INTERNALIZE-"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-internalize- ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply INTERNALIZE-"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic dec+tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let ((pwffs (remove-if-not #'(lambda (x)
					   (and (equals-p x)
						(let ((atp (unabbreviated-type (cdr x))))
						  (and (not (or (consp atp) (eq atp 'O)))
						       (let ((h1 (head (cdar x)))
							     (h2 (head (cdr x))))
							 (and (eq h1 h2)
							      (not (anyabbrev-p h1))))))))
				       (ext-seq-wffs es))))
	     (if pwffs
		 (let ((pwff
			(if (eq tacmode 'INTERACTIVE)
			    (extseq-tactic-choose-wff pwffs "Decompose")
			  (car pwffs))))
		   (if pwff
		       (let* ((n (length (args (cdr pwff))))
			      (msg (if (= n 0)
				       "Applied DEC, closing a subgoal."
				     (if (= n 1)
					 "Applied DEC creating one new subgoal."
				       (format nil "Applied DEC creating new ~d subgoals." n)))))
			 (es-introduce-gap pline n)
			 (let ((ln (linealias pline))
			       (pll nil))
			   (dotimes (i n)
			     (push (- ln (1+ i)) pll))
			   (extseq-dec ln pll pwff)
			   (tactic-output msg t)
			   (values (mapcar #'(lambda (x) (esnumalias x)) pll) msg 'succeed)))
		     (let ((msg "Can't apply DEC"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply DEC"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic eunif1-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let ((pwff-nwffs nil))
	     (dolist (pwff (remove-if-not #'(lambda (x)
					      (and (equals-p x)
						   (let ((atp (unabbreviated-type (cdr x))))
						     (not (or (consp atp) (eq atp 'O))))))
					  (ext-seq-wffs es)))
	       (let ((atp (unabbreviated-type (cdr pwff))))
		 (dolist (nwff (remove-if-not #'(lambda (x)
						  (and (not-p x)
						       (equals-p (cdr x))
						       (equal atp (unabbreviated-type (cddr x)))))
					      (ext-seq-wffs es)))
		   (push (cons pwff nwff) pwff-nwffs))))
	     (if pwff-nwffs
		 (let ((pwff-nwff
			(if (eq tacmode 'INTERACTIVE)
			    (extseq-tactic-choose-wff-2 pwff-nwffs "Show equation" " using " " with rule EUNIF1")
			  (car pwff-nwffs))))
		   (if pwff-nwff
		       (let ((msg "Applied EUNIF1 creating two new subgoals."))
			 (es-introduce-gap pline 2)
			 (let ((ln (linealias pline)))
			   (extseq-eunif1 ln (- ln 2) (- ln 1) (cdr pwff-nwff) (car pwff-nwff))
			   (tactic-output msg t)
			   (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
		     (let ((msg "Can't apply EUNIF1"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply EUNIF1"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic eunif2-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let ((pwff-nwffs nil))
	     (dolist (pwff (remove-if-not #'(lambda (x)
					      (and (equals-p x)
						   (let ((atp (unabbreviated-type (cdr x))))
						     (not (or (consp atp) (eq atp 'O))))))
					  (ext-seq-wffs es)))
	       (let ((atp (unabbreviated-type (cdr pwff))))
		 (dolist (nwff (remove-if-not #'(lambda (x)
						  (and (not-p x)
						       (equals-p (cdr x))
						       (equal atp (unabbreviated-type (cddr x)))))
					      (ext-seq-wffs es)))
		   (push (cons pwff nwff) pwff-nwffs))))
	     (if pwff-nwffs
		 (let ((pwff-nwff
			(if (eq tacmode 'INTERACTIVE)
			    (extseq-tactic-choose-wff-2 pwff-nwffs "Show equation" " using symmetry and " " with rule EUNIF2")
			  (car pwff-nwffs))))
		   (if pwff-nwff
		       (let ((msg "Applied EUNIF2 creating two new subgoals."))
			 (es-introduce-gap pline 2)
			 (let ((ln (linealias pline)))
			   (extseq-eunif2 ln (- ln 2) (- ln 1) (cdr pwff-nwff) (car pwff-nwff))
			   (tactic-output msg t)
			   (values (list (esnumalias (- ln 2)) (esnumalias (- ln 1))) msg 'succeed)))
		     (let ((msg "Can't apply EUNIF2"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply EUNIF2"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic initeq-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (ext-seq-p es)
	   (let ((pwff-nwffs nil))
	     (dolist (pwff (remove-if-not #'(lambda (x)
					      (and (not (or (not-p x) (equals-p x)))
						   (let ((h (head x)))
						     (not (or (member h '(OR AND IMPLIES EQUIV))
							      (anyabbrev-p h))))))
					  (ext-seq-wffs es)))
	       (let ((h (head pwff)))
		 (dolist (nwff (remove-if-not #'(lambda (x)
						  (and (not-p x)
						       (not (or (not-p (cdr x)) (equals-p (cdr x))))
						       (let ((h1 (head x)))
							 (and (eq h1 h)
							      (not (or (member h1 '(OR AND IMPLIES EQUIV))
								       (anyabbrev-p h1)))))))
					      (ext-seq-wffs es)))
		   (push (cons pwff nwff) pwff-nwffs))))
	     (if pwff-nwffs
		 (let ((pwff-nwff
			(if (eq tacmode 'INTERACTIVE)
			    (extseq-tactic-choose-wff-2 pwff-nwffs "Show the arguments of" " and " " are equal.")
			  (car pwff-nwffs))))
		   (if pwff-nwff
		       (let* ((n (length (args (car pwff-nwff))))
			      (msg (if (= n 0)
				       "Applied INITEQ, closing a subgoal."
				     (if (= n 1)
					 "Applied INITEQ creating one new subgoal."
				       (format nil "Applied INITEQ creating new ~d subgoals." n)))))
			 (es-introduce-gap pline n)
			 (let ((ln (linealias pline))
			       (pll nil))
			   (dotimes (i n)
			     (push (- ln (1+ i)) pll))
			   (extseq-initeq ln pll (cdr pwff-nwff) (car pwff-nwff))
			   (tactic-output msg t)
			   (values (mapcar #'(lambda (x) (esnumalias x)) pll) msg 'succeed)))
		     (let ((msg "Can't apply INITEQ"))
		       (tactic-output msg nil)
		       (values (list pline) msg 'fail))))
	       (let ((msg "Can't apply INITEQ"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Line is not associated with an extensional sequent derivation."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

(deftactic contract-tac
  (ext-seq
   (lambda (pline)
     (let ((es (get pline 'ext-seq)))
       (if (and (ext-seq-p es) (eq tacmode 'INTERACTIVE))
	   (let ((pwff (extseq-tactic-choose-wff (ext-seq-wffs es) "Apply CONTRACT to")))
	     (if pwff
		 (let ((msg "Applied CONTRACT"))
		   (es-introduce-gap pline 1)
		   (let ((ln (linealias pline)))
		     (extseq-contr ln (- ln 1) pwff)
		     (tactic-output msg t)
		     (values (list (esnumalias (- ln 1))) msg 'succeed)))
	       (let ((msg "Can't apply CONTRACT"))
		 (tactic-output msg nil)
		 (values (list pline) msg 'fail))))
	 (let ((msg "Can't apply CONTRACT"))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))))

