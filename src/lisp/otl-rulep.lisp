;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLRULEP)

;;; Things in CORE useful for RULEP.  Calls exported function
;;; from AUTO package with qualifier explicitly.

;;; Used are AUTO:VALID-P

;;; File massively overhauled, adding RULEP-ENTER, RULEP-LEGAL-HYPS,
;;; RULEP-DELUXE, RULEP-MINIMIZE-SUPPORTS, RULEP-SIMPLE -- 7/31/87 DAN
;;; Use of RULEP-SIMPLE or RULEP-DELUXE is controlled by the flag
;;; RULEP-MAINFN (see file ML2-PRIOR).

(deffile otl-rulep
;  (part-of tps2-rulep)
  (part-of otlrulep)
  (extension clisp)
  (mhelp "Things useful for RULEP, including the RULEP mainfns,
defaultfn, and enterfn."))

(defun deepify-list (deep-list connective)
  (if (not deep-list) nil
      (do ((deep-list (cdr deep-list) (cdr deep-list))
	   (deep (car deep-list)
		 (cons (cons connective deep) (car deep-list))))
	  ((null deep-list) deep))))


#+comment(defun rulep-defaults (plan-line existing-linelist)
  (list (pline-default plan-line)
	(if (and (eq existing-linelist '$) (neq plan-line '$))
	    (cdr (assoc plan-line (proof-plans dproof)))
	    existing-linelist)))

#+comment(defun rulep-enter (plan-line existing-linelist)
  (let ((new-label (car (line-label-vec 1))))
    ;; Next dolist is a 
    ;; temporary hack until update-plan is fixed;
    ;; otherwise if one of the existing-lines is itself
    ;; justified by plan-line, though update-plan will
    ;; throwfail, the justification for plan-line
    ;; will be changed to what it would be if the
    ;; function were successful. DAN
    (dolist (x existing-linelist)
      (ck-just x (linealias x) t))
    (setf (line-linenumber new-label) (line-linenumber plan-line))
    (setf (line-assertion new-label) (line-assertion plan-line))
    (setf (line-justification new-label) (list "RuleP" nil 
					       (sort existing-linelist
						     #'< :key #'linealias)))
    (update-plan (list (cons new-label (cons 'ss existing-linelist)))
		 nil)))


;;; Redefined rulep-defaults and rulep-enter 7NOV90. DAN
;;; Now they don't require that the lines actually exist in the proof.
;;; See ml2-prior.

(defun rulep-defaults (plan-line existing-linelist)
  (list (if (eq plan-line '$)
	    (linealias (pline-default plan-line))
	  plan-line)
	(if (and (eq existing-linelist '$) (neq plan-line '$)
		 (numalias plan-line))
	    (mapcar #'linealias
		    (cdr (assoc (numalias plan-line) (proof-plans dproof))))
	    existing-linelist)))

(defun rulep-enter (plan-line existing-linelist &optional (just-lines
							   nil just-lines-p))
  (let ((new-label (car (line-label-vec 1))))
    ;; Next dolist is a 
    ;; temporary hack until update-plan is fixed;
    ;; otherwise if one of the existing-lines is itself
    ;; justified by plan-line, though update-plan will
    ;; throwfail, the justification for plan-line
    ;; will be changed to what it would be if the
    ;; function were successful. DAN
    (dolist (x existing-linelist)
      (ck-just x (linealias x) t))
    (setf (line-linenumber new-label) (line-linenumber plan-line))
    (setf (line-assertion new-label) (line-assertion plan-line))
    (setf (line-hypotheses new-label)
      (if just-lines-p
	  (sort (delete-duplicates
		 (mapcan #'(lambda (x) (copy-list (line-hypotheses x)))
			 just-lines))
		#'< :key #'linealias)
	(line-hypotheses plan-line)))
    (setf (line-support new-label) (line-support plan-line))
    (setf (line-justification new-label) (list "RuleP" nil 
					       (sort (copy-list
						      (if just-lines-p
							  just-lines
							existing-linelist))
						     #'< :key #'linealias)))
    (update-plan (list (cons new-label (cons 'ss existing-linelist)))
		 nil)))

(defun rulep-legal-hyps (plan-line existing-linelist)
  (do ((linelist existing-linelist (cdr linelist))
       (hyps (line-hypotheses plan-line)))
      ((null linelist) t)
    (if (not (subsetp (line-hypotheses (car linelist)) hyps))
	(throwfail t "Hypotheses for line " 
		   ((car linelist) . existing-line)
		   " are not contained in hypotheses for line "
		   (plan-line . existing-line) "."))))






(defun rulep-simple (plan-line existing-linelist)
  (if (auto::valid-p
       (if existing-linelist
	   (cons (cons 'implies
		       (deepify-list
			(mapcar #'(lambda (label)
				    (line-assertion label))
				existing-linelist)
			'and))
		 (line-assertion plan-line))
	   (line-assertion plan-line)))
      (list plan-line existing-linelist)
      (throwfail "Line " (plan-line . existing-line)
		      " doesn't follow by RULEP from "
		      (existing-linelist . existing-linelist) ".")))


;;; Differs from RULEP-SIMPLE only in that RULEP-MINIMIZE-SUPPORTS is
;;; called if it is found that plan-line follows from existing-linelist
(defun rulep-deluxe (plan-line existing-linelist)
  (if (auto::valid-p 
       (if existing-linelist
	   (cons (cons 'implies
		       (deepify-list
			(mapcar #'(lambda (x) (line-assertion x))
				existing-linelist)
			'and))
		 (line-assertion plan-line))
	   (line-assertion plan-line)))
      (rulep-minimize-supports plan-line existing-linelist)
      (throwfail "Line " (plan-line . existing-line) 
		 " doesn't follow by RULEP from "
		 (existing-linelist . existing-linelist) ".")))




;;; We know that plan-line follows from existing-linelist by RuleP.
;;; First a list of all proper subsets of existing-list is generated, 
;;; in order of cardinality.
;;; In addition, support lines which are justified are given preferential
;;; treatment.
;;; Then min-set-position is set to the
;;; position of the first subset in the list that implies plan-line.  If
;;; min-set-position is nil, none of the proper subsets is sufficient,
;;; so min-set is existing-linelist itself; otherwise min-set is set to
;;; the member of subsets which corresponds to the number min-set-position.

#+comment(defun rulep-minimize-supports (plan-line existing-linelist)
  (let* ((proper-subsets (remove existing-linelist
			  (sort (all-subsets 
				 (sort existing-linelist
				       #'(lambda (x y) (and (planp x)
							    (not (planp y))))))
				#'< :key #'length)))
	 (min-set-position
	  (position-if
	    #'(lambda (x)
		(auto::valid-p 
		 (if x
		     (cons (cons 'implies
			     (deepify-list 
			      (mapcar #'(lambda (y) (line-assertion y))
				      x)
			      'and))
		       (line-assertion plan-line))
		     (line-assertion plan-line))))
		 proper-subsets))
	 (min-set (if min-set-position
		      (nth min-set-position proper-subsets) 
		      existing-linelist)))
    (list plan-line min-set)))

;;; Changed 7NOV90 as follows. Now prefers lines with fewer hyps.
;;; Also, returns three things.  The plan-line, the existing-linelist,
;;; and the justification lines that it discovers.  These are used by
;;; rulep-enter so that even if one of the lines the user specified
;;; doesn't exist, and isn't necessary, it will be created and added
;;; to the proof.  DAN

#+comment(defun rulep-minimize-supports (plan-line existing-linelist)
  (let* ((proper-subsets (butlast
			  (sort (all-subsets 
				 (sort (copy-list existing-linelist)
				       #'(lambda (x y) 
					   (or (and (planp x)
						    (not (planp y)))
					       (> (length
						   (line-hypotheses
						    x))
						  (length
						   (line-hypotheses y)))))))
				#'< :key #'length)))
	 (min-set-position
	  (position-if
	    #'(lambda (x)
		(auto::valid-p 
		 (if x
		     (cons (cons 'implies
			     (deepify-list 
			      (mapcar #'(lambda (y) (line-assertion y))
				      x)
			      'and))
		       (line-assertion plan-line))
		     (line-assertion plan-line))))
		 proper-subsets))
	 (min-set (if min-set-position
		      (nth min-set-position proper-subsets) 
		      existing-linelist)))
    (list plan-line existing-linelist min-set)))

(defun better-rulep-line-p (x y)
  "Returns T if X is a planned line and Y isn't, or X has more hypotheses
than Y does."
  (or (and (planp x)
	   (not (planp y)))
      (> (length
	  (line-hypotheses
	   x))
	 (length
	  (line-hypotheses y)))))

;;; Changed 20FEB91. Now instead of working up from empty set, we see
;;; if lines in existing-linelist can be discarded.  We keep track of
;;; lines that are required (that is, without them, valid-p returns
;;; nil), and those are what is returned.  We prefer to throw out planned
;;; lines and those with more hyps, so we sort the supports in that
;;; way first. DAN

(defun rulep-minimize-supports (plan-line existing-linelist)
  (do* ((supports (sort (copy-list existing-linelist)
			#'better-rulep-line-p)
		  (cdr supports))
	(required nil))
      ((null supports)
       (list plan-line 
	     existing-linelist 
	     required))
    (let ((supps (append (cdr supports) required)))
      (unless (auto::valid-p 
	       (if (null supps)
		   (line-assertion plan-line)
		 (cons (cons 'implies
			     (deepify-list 
			      (mapcar #'(lambda (y) (line-assertion y))
				      supps)
			      'and))
		       (line-assertion plan-line))))
	(push (car supports) required)))))


