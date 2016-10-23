;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ml)
(part-of ml-etr-tactics)

(deffile ml-etr-tactics-symsimp2
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Defines tactics for symmetric simplification."))

(deftactic backchain-lemma-tac
  (etree-nat
   (lambda (pline)
     (backchain-lemma-tac-etree-nat-fn pline))
   "If a support line is an implication, sets up a symmetric simplification
problem using the antecedent of the implication in the lemma.  Then
symmetric simplification is performed."
   ))



(defun backchain-lemma-tac-etree-nat-fn (pline)
  (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-support 
	   (find-if #'auto::implication-p supports 
		    :key #'(lambda (x) (line-node x))))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (unless (and matched-support 
		 (if (eq tacmode 'interactive)
		     (query (format nil "Apply BACKCHAIN-LEMMA to line ~D?"
				    (linealias matched-support)) t)))
      (setq matched-support nil))
    (if matched-support
	(let ((msg "Applied BACKCHAIN-LEMMA.")
	      (new-plan-line nil)
	      (disj-support-line nil)
	      (support-nodes nil)
	      (auto::leaf-name (gentemp))
	      (auto::*ignore-statuses* t))
	  (tactic-output msg t)
	  (comdecode (list 'same (linealias pline) (1- (linealias pline)) '!))
	  (setq new-plan-line 
		(car (set-difference (current-plan-lines dproof) oldplans)))
	  (sponsor new-plan-line (copy-list supports))
	  (setf (line-node new-plan-line)
		(line-node pline))
	  (setf (line-mating new-plan-line) (line-mating pline))
	  (unsponsor new-plan-line (list matched-support))
	  (setq matched-support (copy-support-line matched-support))
	  (setq support-nodes
		(let ((nodes nil))
		  (dolist (support (cdr (assoc new-plan-line
					  (proof-plans dproof)))
				   nodes)
		    (push (line-node support) nodes))))
	  (sponsor new-plan-line (list matched-support))
;	  make disj
	  (multiple-value-bind (disj a b support-nodes conn-list)
	    (make-backchain-lemma-nodes (line-node matched-support)
					(line-node new-plan-line)
					support-nodes
					(line-mating new-plan-line))
	    (setq conn-list
		  (auto::symmetric-simplification 
			  support-nodes
			  disj a b conn-list))
;           make disj-support-line, with shallow-formula of disj, no hyps
	    (comdecode (list 'lemma (linealias new-plan-line) '$
			     (list 'quote (auto::get-shallow disj))
			     (list 'quote (line-assertion new-plan-line))
			     (mapcar #'linealias 
				     (line-hypotheses new-plan-line)) '()))
	    (setf (line-node matched-support) a)
	    (setq disj-support-line
		  (car (remove new-plan-line
			       (set-difference (current-plan-lines dproof)
					       oldplans))))
	    (setf (line-node disj-support-line) disj)
	    (setf (line-just-rule disj-support-line) "RuleP")
	    (setf (proof-plans dproof)
		  (delete disj-support-line (proof-plans dproof) :key #'car))
	    (setf (line-mating new-plan-line) conn-list)
	    (values (list new-plan-line ) msg 'succeed)))
	(let ((msg (if matched-support
		       "Not applying BACKCHAIN-LEMMA."
		       "Can't apply BACKCHAIN-LEMMA.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))


(defun make-backchain-lemma-nodes (imp-node pline-node support-nodes conn-list)
  (let* ((left (car (auto::etree-components imp-node)))
	 (right (cadr (auto::etree-components imp-node)))
	 (disj (auto::make-edisjunction :junctive 'auto::dis :positive t
					:components nil))
;	 (b (auto::make-true :positive nil :junctive 'auto::con))
	 (b (auto::make-true :positive nil :junctive 'auto::dis)) ; cebrown 11/19/00 - changed from CON to DIS, because otherwise it doesn't make sense
	 (not-c (auto::copy-etree-rec left nil))
	 (not-c* (auto::make-negation :junctive 'auto::neutral
		   :components (list not-c)
		   :positive t))
	 (a (auto::make-implication :positive t :junctive 'auto::dis
				    :components
				    (list (auto::copy-etree-rec not-c t)
					  right)))
	 (c nil)
	 )
    (multiple-value-setq (c conn-list) 
      (dup-etree-neg (car (auto::etree-components a)) disj conn-list)) 
    (setf (auto::etree-parent not-c) not-c*)
    (setf (auto::etree-parent not-c*) disj)
    (setf (auto::etree-components disj) (list c not-c*))
    (values disj a b (cons pline-node support-nodes) conn-list)))


(deftactic subst=-backchain-lemma-tac
  (etree-nat
   (lambda (pline)
     (subst=-backchain-lemma-tac-etree-nat-fn pline))
   "If substitution of equality can be applied to a support line, creates
a new disjunctive lemma based on the formula to which the equality can
be applied.  Then symmetric simplification is used to simplify the lemma."
   ))



(defun subst=-backchain-lemma-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-support 
	  (find-if #'(lambda (x)
		       (or (subst=r-match2 x 
				  pline
				  (remove x supports)
				  (line-mating pline))
			   (subst=l-match2 x 
				  pline
				  (remove x supports)
				  (line-mating pline))))
		   supports))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (unless (and matched-support 
		 (if (eq tacmode 'interactive)
		     (query 
		       (format nil "Apply SUBST=-BACKCHAIN-LEMMA to line ~D?"
			       (linealias matched-support)) t)))
      (setq matched-support nil))
    (if matched-support
	(let ((msg "Applied SUBST=-BACKCHAIN-LEMMA.")
	      (new-plan-line nil)
	      (disj-support-line nil)
	      (support-nodes nil)
	      (auto::leaf-name (gentemp))
	      (matched-exp nil)
	      (temp nil)
	      (imp-node nil))
	  (tactic-output msg t)
	  (comdecode (list 'same (linealias pline) (1- (linealias pline)) '!))
	  (setq new-plan-line 
		(car (set-difference (current-plan-lines dproof) oldplans)))
	  (sponsor new-plan-line (copy-list supports))
	  (setf (line-node new-plan-line)
		(line-node pline))
	  (setf (line-mating new-plan-line) (line-mating pline))
	  (unsponsor new-plan-line (list matched-support))
	  (setq matched-support (copy-support-line matched-support))
	  (setq supports (cdr (assoc new-plan-line (proof-plans dproof))))
	  (setq temp (or (subst=r-match2 matched-support new-plan-line
				     supports
				     (line-mating new-plan-line))
			 (subst=l-match2 matched-support new-plan-line
				     supports
				     (line-mating new-plan-line))))
	  (setq matched-exp (auto::etree-parent (cadr temp)))
	  (setq imp-node (caddr temp))
	  (setq support-nodes
		(let ((nodes nil))
		  (dolist (support (cdr (assoc new-plan-line
					  (proof-plans dproof)))
				   nodes)
		    (push (line-node support) nodes))))
	  (setq support-nodes
		(append (remove imp-node 
				(mapcan #'(lambda (x y) 
					    (if (auto::admissible-p 
						  (cons (line-node new-plan-line)
							support-nodes)
						  x)
						(list y)))
					(auto::expansion-terms matched-exp)
					(auto::etree-components matched-exp)))
			support-nodes))
	  (sponsor new-plan-line (list matched-support))
	  ;make disj
	  (multiple-value-bind (disj a b support-nodes conn-list)
	    (make-backchain-lemma-nodes imp-node
					(line-node new-plan-line)
					support-nodes
					(line-mating new-plan-line))
	    (setq conn-list
		  (auto::symmetric-simplification 
			  support-nodes
			  disj a b conn-list))
	    ; make disj-support-line, with shallow-formula of disj, no hyps
	    (comdecode (list 'lemma (linealias new-plan-line) '$
			     (list 'quote (auto::get-shallow disj))
			     (list 'quote (line-assertion new-plan-line))
			     (mapcar #'linealias 
				     (line-hypotheses new-plan-line)) '()))
	    (setf (auto::etree-components 
		    (auto::etree-parent imp-node))
		  (subst a imp-node (auto::etree-components 
				      (auto::etree-parent imp-node))))
	    (setq disj-support-line
		  (car (remove new-plan-line
			       (set-difference (current-plan-lines dproof)
					       oldplans))))
	    (setf (line-node disj-support-line) disj)
	    (setf (line-just-rule disj-support-line) "RuleP")
	    (setf (proof-plans dproof)
		  (delete disj-support-line (proof-plans dproof) :key #'car))
	    (setf (line-mating new-plan-line) conn-list)
	    (values (list new-plan-line ) msg 'succeed)))
	(let ((msg (if matched-support
		       "Not applying SUBST=-BACKCHAIN-LEMMA."
		       "Can't apply SUBST=-BACKCHAIN-LEMMA.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))



