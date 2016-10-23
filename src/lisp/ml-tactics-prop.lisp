;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ml)
(part-of ml-tactics)

(context prop-tactics)
(deffile ml-tactics-prop
  (extension lisp)
  (part-of ml-tactics)
  (mhelp "Defines tactics for use with propositional rules."))

(deftactic same-tac
  (nat-ded 
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	    (matched-support
	     (find-if #'(lambda (x)
			  (and
			   (subsetp (line-hypotheses x)
				    (line-hypotheses pline))
			   (same-match1 pline x)))
		      supports)))
       (if matched-support
	   (let ((msg "Applied SAME."))
	     (tactic-output msg t)
	     (same-short pline matched-support)
	     (values nil
		     msg
		     'succeed))
	   (let ((msg "Can't apply SAME."))
	     (tactic-output msg nil)
	     (values (list pline)
		     msg
		     'fail))
	   )))
   "Applies SAME if planned line is the same as a support line."
	   ))



(deftactic deduct-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'deduct-match1 (list pline)))
	    (matched-plan
	     (if (eq tacmode 'interactive)
		 (find-if #'(lambda (x) (query (format nil  "Apply DEDUCT to line ~D?"
						       (linealias x)) t))
			  matched-plans)
		 (car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied DEDUCT."))
	     (tactic-output msg t)
	     (deduct-short pline)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-plans
			  "Not applying DEDUCT."
			  "Can't apply DEDUCT.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
	   )))
   "Applies DEDUCT if planned line is an implication."))

(deftactic mp-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'mp-match1 supports))
	    (matched-support (car matched-supports))
	    (oldplans (mapcar #'car (proof-plans dproof))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil  "Apply MP to line ~D?"
					  (linealias x)) t))
			    matched-supports)))
       (if matched-support
	   (let ((msg "Applied MP.")
		 (newplan nil))
	     (tactic-output msg t)
	     (funcall #'comdecode
		      (list 'lemma (linealias pline) '$
			    (list 'quote (line-assertion matched-support))
			    '$ '$ '$))
	     (setq newplan
		   (car
		    (set-difference 
		     (mapcar #'car (proof-plans dproof))
		     oldplans)))
	     (same-short newplan matched-support)
	     (funcall #'comdecode
		      (list 'unsponsor (linealias pline)
			    (list (linealias matched-support))))
	     (mp-short newplan)
	     (setq newplan
		   (car
		    (set-difference 
		     (mapcar #'car (proof-plans dproof))
		     oldplans)))
	     (values (list newplan pline) msg 'succeed))
	   (let ((msg (if matched-supports
			  "Not applying MP."
			  "Can't apply MP.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies MP if a support line is an implication."))


(deftactic econj-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'econj-match1 supports))
	    (matched-support (car matched-supports))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil  "Apply ECONJ to line ~D?"
					  (linealias x)) t))
			    matched-supports)))
       (if matched-support
	   (let ((msg "Applied ECONJ."))
	     (tactic-output msg t)
	     (econj-short matched-support)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-supports
			  "Not applying ECONJ."
			  "Can't apply ECONJ.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies ECONJ if a support line is a conjunction."))


(deftactic econj*-tac
  (nat-ded
   (lambda (pline)
     (econj*-tac-nat-ded-fn pline))
   "Applies conjunction elimination to a support line if applicable.
If support line is a multiple conjunction, completely breaks it up."))



(defun econj*-tac-nat-ded-fn (pline)
  (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-supports (remove-if-not #'econj-match1 supports))
	 (matched-support (car matched-supports))
	 (wff-list
	  (if matched-support 
	      (find-all-conjuncts (line-assertion matched-support))))
	 (oldplans (mapcar #'car (proof-plans dproof))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply ECONJ* to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-support
	(progn (make-room-after matched-support (length wff-list))
	(let* ((msg "Applied ECONJ*.")
	       (newplans nil))
	  (tactic-output msg t)
	  (setq newplans
		(do ((wff-list wff-list (cdr wff-list))
		     (line (1+ (linealias matched-support)) (1+ line))
		     (newplans nil (cons newplan newplans))
		     (hyps (mapcar #'linealias 
				   (line-hypotheses matched-support)))
		     (oldplans oldplans (cons newplan oldplans))
		     (newplan nil))
		    ((null wff-list) newplans)
		  (funcall #'comdecode 
			   (list 'lemma (linealias pline) line
				 (list 'quote (car wff-list)) '$
				 '$ hyps))
		  (setq newplan
			(car (set-difference 
			      (mapcar #'car (proof-plans dproof))
			      oldplans)))
		  (funcall #'comdecode
			   (list 'rulep (linealias newplan)
				 (list (linealias matched-support))))))
	  (update-plan `((pp ,matched-support ss))
		       `((pp ,@newplans ss)))
	  (values (list pline) msg 'succeed)))
	(let ((msg (if matched-supports
		       "Not applying ECONJ*."
		       "Can't apply ECONJ*.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(deftactic iconj-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'iconj-match1 (list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x) (query (format nil  "Apply ICONJ to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied ICONJ."))
	     (tactic-output msg t)
	     (iconj-short pline)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-plans
			  "Not applying ICONJ."
			  "Can't apply ICONJ.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))
   "Applies ICONJ if the planned line is a conjunction."))

(deftactic iconj*-tac
  (nat-ded
   (lambda (pline)
     (iconj*-tac-nat-ded-fn pline))
   "If planned line corresponds to a conjunction node, splits into
subgoals.  Will break up a multiple conjunction into separate conjuncts."
))


(defun iconj*-tac-nat-ded-fn (pline)
  (let* ((matched-plans (remove-if-not #'iconj-match1 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply ICONJ* to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (wff-list
	  (if matched-plan
	      (find-all-conjuncts (line-assertion matched-plan))))
	 (oldplans (mapcar #'car (proof-plans dproof))))
    (if matched-plan
	(progn (make-room-before matched-plan (length wff-list))
	(let* ((msg "Applied ICONJ*.")
	       (space-available (- (linealias matched-plan) (find-room-before matched-plan)))
	       (line-jump-1 (truncate (/ space-available (1+ (length wff-list)))))
	       (line-jump (if (= 0 line-jump-1) 1 line-jump-1))
	       (newplans nil))
	  (tactic-output msg t)	
	  (setq newplans
		(do ((wff-list wff-list (cdr wff-list))
		     (line (- (linealias matched-plan)
			      (* (length wff-list) line-jump))
			   (+ line line-jump))
		     (newplans nil (cons newplan newplans))
		     (oldplans oldplans (cons newplan oldplans))
		     (newplan nil))
		    ((null wff-list) newplans)
		  (funcall #'comdecode 
			   (list 'lemma (linealias pline) line
				 (list 'quote (car wff-list)) '$
				 '$ '$))
		  (setq newplan
			(car (set-difference 
			      (mapcar #'car (proof-plans dproof))
			      oldplans)))))
	  (funcall #'comdecode
		   (list 'rulep (linealias matched-plan)
				(mapcar #'linealias newplans)))
	  (values newplans msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying ICONJ*."
		       "Can't apply ICONJ*.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(defun find-all-conjuncts (wff)
  (if (and-p wff)
      (nconc 
       (find-all-conjuncts (glr wff))
       (find-all-conjuncts (grr wff)))
      (list wff)))



(deftactic cases-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'(lambda (x) 
						 (cases-match1 pline x))
					     supports))
	    (matched-support (car matched-supports))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil  "Apply CASES to line ~D?"
					  (linealias x)) t))
			    matched-supports)))
       (if matched-support
	   (let ((msg "Applied CASES."))
	     (tactic-output msg t)
	     (cases-short pline matched-support)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-supports
			  "Not applying CASES."
			  "Can't apply CASES.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies CASES if a support line is a disjunction."))

(deftactic indirect-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans 
	     (remove-if #'(lambda (x) (wffeq (line-assertion x) 'FALSEHOOD))
			(list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x) (query (format nil  "Apply INDIRECT to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied INDIRECT."))
	     (tactic-output msg t)
	     (indirect-short pline)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-plans
			  "Not applying INDIRECT."
			  "Can't apply INDIRECT.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))
   "Applies INDIRECT as long as planned line is not FALSEHOOD."))



(deftactic pushneg-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'pushneg-match1 supports))
	    (matched-support (car matched-supports))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil  "Apply PUSHNEG to line ~D?"
					  (linealias x)) t))
			    matched-supports)))
       (if matched-support
	   (if (pushneg-short matched-support)
	       (progn
		 (tactic-output "Applied PUSHNEG." t)
		 (values (set-difference (mapcar #'car (proof-plans dproof))
					oldplans)
			"Applied PUSHNEG." 'succeed))
	       (progn 
		 (tactic-output "PUSHNEG failed." nil)
		(values (list pline) "PUSHNEG failed." 'fail)))
	   (let ((msg (if matched-supports
			  "Not applying PUSHNEG."
			  "Can't apply PUSHNEG.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies PUSHNEG if a support line is a negated non-literal formula."))


(deftactic pullneg-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'pullneg-match1 (list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x) (query (format nil  "Apply PULLNEG to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (if (pullneg-short pline)
	       (progn
		 (tactic-output "Applied PULLNEG." t)
		 (values (set-difference (mapcar #'car (proof-plans dproof))
					oldplans)
			"Applied PULLNEG." 'succeed))
	       (progn
		 (tactic-output  "PULLNEG failed." nil)
		 (values (list pline) "PULLNEG failed." 'fail)))
	   (let ((msg (if matched-plans
			  "Not applying PULLNEG."
			  "Can't apply PULLNEG.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))
   "Applies PULLNEG if the planned line is a negated non-literal formula."))

(deftactic ineg-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'ineg-match1 (list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x) (query (format nil  "Apply INEG to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (progn
	     (ineg-short matched-plan)
	     (tactic-output "Applied INEG." t)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     "Applied INEG." 'succeed))
	 (let ((msg (if matched-plans
			"Not applying INEG."
		      "Can't apply INEG.")))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail))
)))
   "Applies INEG if the planned line is a negated formula."))



(deftactic disj-imp-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'disj-imp-match1 supports))
	    (matched-support (car matched-supports))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil  "Apply DISJ-IMP to line ~D?"
					  (linealias x)) t))
			    matched-supports)))
       (if matched-support
	   (let ((msg "Applied DISJ-IMP."))
	     (tactic-output msg t)
	     (disj-imp-short matched-support)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-supports
			  "Not applying MP."
			  "Can't apply MP.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies DISJ-IMP if a support line is of the form \"~A or B\"."))


(deftactic imp-disj-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'imp-disj-match1 supports))
	    (matched-support (car matched-supports))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil  "Apply IMP-DISJ to line ~D?"
					  (linealias x)) t))
			    matched-supports)))
       (if matched-support
	   (let ((msg "Applied IMP-DISJ."))
	     (tactic-output msg t)
	     (imp-disj-short matched-support)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-supports
			  "Not applying IMP-DISJ."
			  "Can't apply IMP-DISJ.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies IMP-DISJ if a support line is an implication."))


(deftactic implics-equiv-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'implics-equiv-match1 (list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x) (query (format nil  "Apply IMPLICS-EQUIV to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied IMPLICS-EQUIV."))
	     (tactic-output msg t)
	     (implics-equiv-short pline)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-plans
			  "Not applying IMPLICS-EQUIV."
			  "Can't apply IMPLICS-EQUIV.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))
   "Applies IMPLICS-EQUIV if planned line is an equivalence."))


(deftactic equiv-implics-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'equiv-implics-match1 supports))
	    (matched-support (car matched-supports))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil
	   			        "Apply EQUIV-IMPLICS to line ~D?"
					  (linealias x)) t))
			    matched-supports)))
       (if matched-support
	   (let ((msg "Applied EQUIV-IMPLICS."))
	     (tactic-output msg t)
	     (equiv-implics-short matched-support)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-supports
			  "Not applying EQUIV-IMPLICS."
			  "Can't apply EQUIV-IMPLICS.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies EQUIV-IMPLICS if a support line is an equivalence."))


(deftactic absurd-tac
  (nat-ded
   (lambda (pline)
     (absurd-nat-ded-fn pline))
   "If a support line is FALSEHOOD applies absurdity rule."))

(defun absurd-match3 (support)
  (wffeq 'falsehood (line-assertion support)))


(defun absurd-nat-ded-fn (pline)
  (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-support
	  (find-if #'absurd-match3 supports)))
    (if matched-support
	(let ((msg "Applied ABSURD."))
	  (tactic-output msg t)
	  (comdecode (list 'absurd 
			   (linealias pline) (linealias matched-support)
			   '!))
	  (values nil msg 'succeed))
	(let ((msg "Can't apply ABSURD."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))



(deftactic truth-tac
  (nat-ded
   (lambda (pline)
     (truth-tac-nat-ded-fn pline))
   "Applies RuleP if the planned line is TRUTH."))


(defun truth-match3 (pline)
  (wffeq 'truth (line-assertion pline)))


(defun truth-tac-nat-ded-fn (pline)
  (let* ((matched-plans (remove-if-not #'truth-match3 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply TRUTH to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans))))
    (if matched-plan
	(let ((msg "Applied TRUTH."))
	  (tactic-output msg t)
	  (rulep-enter pline nil)
	  (values nil msg 'succeed))
	(let ((msg (if matched-plans
		       "Not applying TRUTH."
		       "Can't apply TRUTH.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(deftactic indirect2-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-support-lines
	     (do ((supports supports (cdr supports))
		  (match 
		   (some
		    #'(lambda (x) 
			(if (wffeq 
			      (cons 'not (line-assertion (car supports)))
			      (line-assertion x))
			     (list x (car supports))
			     (if (wffeq (cons 'not (line-assertion x))
				    (line-assertion (car supports)))
				 (list (car supports) x))))
		    (cdr supports))
		   (some
		    #'(lambda (x) 
			(if (wffeq 
			      (cons 'not (line-assertion (car supports)))
			      (line-assertion x))
			     (list x (car supports))
			     (if (wffeq (cons 'not (line-assertion x))
				    (line-assertion (car supports)))
				 (list (car supports) x))))
		    (cdr supports))))
		 ((or (null supports) match)
		  match)))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (if matched-support-lines
	   (let ((msg "Applied INDIRECT2."))
	     (tactic-output msg t)
	     (funcall #'comdecode 
		      (append `(indirect2 ,(linealias pline))
			      (mapcar #'linealias 
				      matched-support-lines)
			      (list (1- (linealias pline)))
			      ))
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg "Can't apply INDIRECT2."))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies INDIRECT2 if two support lines are contradictory."))


(deftactic prop-prim
  (nat-ded
    (orelse same-tac truth-tac absurd-tac indirect2-tac 
	    make-room econj-tac iconj-tac equiv-implics-tac
	    implics-equiv-tac pushneg-tac pullneg-tac deduct-tac
	    mp-tac  cases-tac  indirect-tac)
    "Much like tactic defined in Felty's master's thesis, p. 64."))   


(deftactic propositional
  (nat-ded
   (repeat
    (orelse make-room 
     (try (repeat prop-prim))
     (then indirect-tac propositional)))
   "First tries PROP-PRIM repeatedly.  If any goals remain, what work
was done is thrown away, indirect proof is applied, and PROPOSITIONAL
is called recursively on the new goal."))



(deftactic rulep-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	   (good-hyps
	    (remove-if-not #'(lambda (line)
			       (subsetp (line-hypotheses line)
					(line-hypotheses pline)))
			   supports))
	   (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if (%catch% (let ((*standard-output* (make-string-output-stream)))
		      (comdecode `(rulep ,(linealias pline)
					 ,(mapcar #'linealias good-hyps)))
		      ;; (funcall rulep-mainfn pline good-hyps)
		      ;; (rulep-enter pline good-hyps)
		      )
		    (fail nil))
	   (progn 
	     (tactic-output "Applied RULEP." t)
	    (values (set-difference (mapcar #'car (proof-plans dproof))
				    oldplans)
		    "Applied RULEP." 'succeed))
	   (let ((msg "Can't apply RULEP."))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
	   )))
   "Attempts to apply RULEP; fails if planned line doesn't follow
from supports by RuleP."))

;;;The next tactic was sent by Dan in email 1991 Sept. 11

(deftactic eneg-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'(lambda (x)
						 (eneg-match1 pline x))
					     supports))
	    (matched-support (car matched-supports))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil  "Apply ENEG to line ~D?"
					  (linealias x)) t))
			    matched-supports)))
       (if matched-support
	   (let ((msg "Applied ENEG."))
	     (tactic-output msg t)
	     (eneg-short pline matched-support)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-supports
			  "Not applying ENEG."
			  "Can't apply ENEG.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "Applies ENEG if a support line is a negation and planned line is
FALSEHOOD."))


