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

;;; Written by Dan Nesmith

(context etr-nat)

(deffile ml-etr-tactics-neg
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Defines tactics for negations as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs."))

(context prop-tactics)


(deftactic eneg-tac
  (etree-nat
    (orelse neg-atom-elim-tac neg-neg-elim-tac neg-and-elim-tac
	    neg-imp-elim-tac neg-univ-elim-tac
	    neg-exists-elim-simple-tac neg-exists-elim-dup-tac
	    neg-or-elim-simple-tac neg-or-elim-dup-tac
	    neg-equal-elim-tac)))



(deftactic neg-atom-elim-tac
  (etree-nat
   (lambda (pline)
     (neg-atom-elim-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD and it has two complementary support lines
which are mated, applies eneg rule.  Same as Pfenning's tactic 212."))



(defun neg-atom-elim-match2 (support other-supports conn-list)
  (let ((supp-bottom (chain-of-negs (line-node support))))
    (when supp-bottom
      (dolist (other other-supports nil)
	(let* ((supp-node (line-node other))
	       (other-bottom (chain-of-negs supp-node)))
	  (if (and other-bottom
		   (auto::mated-to-node other-bottom supp-bottom conn-list)
		   (or (wffeq-ab (cons 'not (line-assertion support))
			      (line-assertion other))
		       (wffeq-ab (line-assertion support)
			      (cons 'not (line-assertion other)))))
	      (return other)))))))

(defun neg-atom-elim-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-support-lines
	   (if (wffeq 'falsehood (line-assertion pline))
	       (dolist (supp supports nil)
		 (let ((match (neg-atom-elim-match2 supp (remove supp supports)
						(line-mating pline))))
		   (if match (return (list match supp)))))))
	 (dummy-support 
	  (if (and (eq tacmode 'interactive) matched-support-lines)
	      (find-if #'(lambda (x) (query (format nil  "Apply NEG-ATOM-ELIM to line ~D?" (linealias x)) t))
		       matched-support-lines)
	    matched-support-lines))
	 (matched-support-lines (if (eq dummy-support nil) nil matched-support-lines)))
    (if matched-support-lines
	(let ((msg "Applied NEG-ATOM-ELIM."))
	  (setq matched-support-lines
		(if (wffeq-ab (cons 'not (line-assertion
				       (car matched-support-lines)))
			   (line-assertion (cadr matched-support-lines)))
		    (reverse matched-support-lines)
		    matched-support-lines))
	  (tactic-output msg t)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias (car matched-support-lines))
			    ,(linealias (cadr matched-support-lines))
			    !))
	  (values nil msg 'succeed))
	(let ((msg "Can't apply NEG-ATOM-ELIM."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))

(deftactic neg-neg-elim-tac
  (etree-nat
   (lambda (pline)
     (neg-neg-elim-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD and it has doubly-negated support line,
applies eneg rule.  Same as Pfenning's tactic 214."))



(defun neg-neg-elim-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof))))
	 (mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline)))) ; cebrown 2/19/01
	 (matched-support-lines
	   (if (wffeq 'falsehood (line-assertion pline))
	       (remove-if-not #'(lambda (x)
				  (and (neg-neg-match2 x)
				       (not (neg-mated-line x mated-nodes))))
			      supports)))
	 (matched-support (car matched-support-lines))
	 (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-NEG-ELIM to line ~D?"
					(linealias x)) t))
		     matched-support-lines)))
    (if matched-support-lines
	(let ((msg "Applied NEG-NEG-ELIM.")
	      (newplan nil))
	  (tactic-output msg t)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq newplan (car (set-difference 
			       (mapcar #'car (proof-plans dproof))
			       oldplans)))
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan) (car (auto::etree-components
					   (line-node matched-support))))
	  (setf (get newplan 'duplicate) (get matched-support 'duplicate)) ; cebrown 8/30/01
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply NEG-NEG-ELIM."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(deftactic neg-and-elim-tac
  (etree-nat
   (lambda (pline)
     (neg-and-elim-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD, and a support line is a negated conjunction,
applies eneg rule.  Same as Pfenning's tactic 215."))

(defun neg-and-elim-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline)))) ; cebrown 2/19/01
	 (matched-supports 
	   (if (wffeq 'falsehood (line-assertion pline))
	       (remove-if-not #'(lambda (x)
				  (and (neg-and-match2 x)
				       (not (neg-mated-line x mated-nodes)))) ; cebrown 2/19/01
			      supports)))
	 (matched-support (car matched-supports))
	 (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-AND-ELIM to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-supports
	(let ((msg "Applied NEG-AND-ELIM.")
	      (newplan nil))
	  (tactic-output msg t)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq newplan (car (set-difference 
			       (mapcar #'car (proof-plans dproof))
			       oldplans)))
	  (setf (get newplan 'duplicate) (get matched-support 'duplicate)) ; cebrown 8/30/01
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan) (car (auto::etree-components
					   (line-node matched-support))))
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply NEG-AND-ELIM."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))

(deftactic neg-imp-elim-tac
  (etree-nat
   (lambda (pline)
     (neg-imp-elim-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD, and a support line is a negated implication,
applies eneg rule.  Same as Pfenning's tactic 216."))

(defun neg-imp-elim-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline)))) ; cebrown 2/19/01
	 (matched-supports 
	  (if (wffeq 'falsehood (line-assertion pline))
	      (remove-if-not #'(lambda (x)
				 (and (neg-imp-match2 x)
				      (not (neg-mated-line x mated-nodes)))) ; cebrown 2/19/01
			     supports)))
	 (matched-support (car matched-supports))
	 (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-IMP-ELIM to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-supports
	(let ((msg "Applied NEG-IMP-ELIM.")
	      (newplan nil))
	  (tactic-output msg t)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq newplan (car (set-difference 
			       (mapcar #'car (proof-plans dproof))
			       oldplans)))
	  (setf (get newplan 'duplicate) (get matched-support 'duplicate)) ; cebrown 8/30/01
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan) (car (auto::etree-components
					   (line-node matched-support))))
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply NEG-IMP-ELIM."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))

(deftactic neg-univ-elim-tac
  (etree-nat
   (lambda (pline)
     (neg-univ-elim-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD, and a support line is a negated universally
quantified formula, applies eneg rule.  Same as Pfenning's tactic 217."))

(defun neg-univ-elim-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof))))
	 (mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline)))) ; cebrown 2/19/01	 
	 (matched-supports 
	   (if (wffeq 'falsehood (line-assertion pline))
	       (remove-if-not #'(lambda (x)
				  (and (neg-sel-match2 x)
				       (not (neg-mated-line x mated-nodes)))) ; cebrown 2/19/01
			      supports)))
	 (matched-support (car matched-supports))
	 (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-UNIV-ELIM to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-supports
	(let ((msg "Applied NEG-UNIV-ELIM.")
	      (newplan nil))
	  (tactic-output msg t)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq newplan (car (set-difference 
			       (mapcar #'car (proof-plans dproof))
			       oldplans)))
	  (setf (get newplan 'duplicate) (get matched-support 'duplicate)) ; cebrown 8/30/01
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan) (car (auto::etree-components
					   (line-node matched-support))))
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply NEG-UNIV-ELIM."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(deftactic neg-exists-elim-simple-tac
  (etree-nat
   (lambda (pline)
     (neg-exists-elim-simple-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD, and a support line is a negated existentially
quantified formula with exactly one admissible expansion, applies eneg rule.
Same as Pfenning's tactic 220."))

(defun neg-exists-simple-match2 (support other-supps pline)
  (let ((node (line-node support))
	(mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline))))) ; cebrown 2/19/01
    (and (neg-exp-match2 support)
	 (not (neg-mated-line support mated-nodes))
	 (let* ((exp-node (car (auto::etree-components node)))
		(plan-node (line-node pline))
		(other-nodes (mapcar #'(lambda (x) (line-node x))
				     other-supps)))
	   (and (= 1 (length (auto::etree-components exp-node)))
		(auto::admissible-p (cons plan-node other-nodes)
				    (car (auto::expansion-terms exp-node))))))))
		




(defun neg-exists-elim-simple-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-supports 
	   (if (wffeq 'falsehood (line-assertion pline))
	       (remove-if-not #'(lambda (x) (neg-exists-simple-match2 
					      x (remove x supports) pline))
			      supports)))
	 (matched-support (car matched-supports))
	 (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-EXISTS-ELIM-SIMPLE to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-supports
	(let ((msg "Applied NEG-EXISTS-ELIM-SIMPLE.")
	      (newplan nil))
	  (tactic-output msg t)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq newplan (car (set-difference 
			       (mapcar #'car (proof-plans dproof))
			       oldplans)))
	  (setf (get newplan 'duplicate) (get matched-support 'duplicate)) ; cebrown 8/30/01
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan) (car (auto::etree-components
					   (line-node matched-support))))
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply NEG-EXISTS-ELIM-SIMPLE."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(deftactic neg-exists-elim-dup-tac
  (etree-nat
   (lambda (pline)
     (neg-exists-elim-dup-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD, and a support line is a negated existentially
quantified formula with more than one expansion, one of which is admissible,
applies eneg rule, adding the line with its other expansions as a support.
Same as Pfenning's tactic 221."))

(defun neg-exists-elim-dup-match2 (support other-supps pline)
  (let ((node (line-node support))
	(mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline))))) ; cebrown 2/19/01
    (and (neg-exp-match2 support)
	 (not (neg-mated-line support mated-nodes))
	 (let* ((exp-node (car (auto::etree-components node)))
		(plan-node (line-node pline))
		(other-nodes (mapcar #'(lambda (x) (line-node x))
				     other-supps)))
	   (and (> (length (auto::etree-components exp-node)) 1)
		(position-if #'(lambda (x)
				 (auto::admissible-p 
				   (cons plan-node other-nodes)
				   x))
			     (auto::expansion-terms exp-node)))))))
		




(defun neg-exists-elim-dup-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-supports 
	   (if (wffeq 'falsehood (line-assertion pline))
	       (remove-if-not #'(lambda (x) (neg-exists-elim-dup-match2 
					      x (remove x supports) pline))
			      supports)))
	 (matched-support (car matched-supports))
	 (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-EXISTS-ELIM-DUP to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-supports
	(let ((msg "Applied NEG-EXISTS-ELIM-DUP.")
	      (new-neg-node (auto::copy-negation 
			      (line-node matched-support)))
	      (new-exp-node (auto::copy-expansion
			      (car (etree-components
				     (line-node matched-support)))))
	      (new-exp-node* (auto::copy-expansion
			      (car (etree-components
				     (line-node matched-support)))))
	      (pos (neg-exists-elim-dup-match2 matched-support
					  (remove matched-support supports)
					  pline))
	      (new-neg-exp-line nil)
	      (newplan nil))
	  (setf (auto::etree-components new-neg-node)
		(list new-exp-node))
	  (setf (auto::etree-parent new-exp-node) new-neg-node)
	  (setf (auto::expansion-terms new-exp-node)
		(append (subseq (auto::expansion-terms new-exp-node) 0 pos)
			(subseq (auto::expansion-terms new-exp-node) (1+ pos))))
	  (setf (auto::etree-components new-exp-node)
		(append (subseq (auto::etree-components new-exp-node) 0 pos)
			(subseq (auto::etree-components new-exp-node) (1+ pos))))
	  (setf (auto::expansion-terms new-exp-node*)
		(list (nth pos (auto::expansion-terms new-exp-node*))))
	  (setf (auto::etree-components new-exp-node*)
		(list (nth pos (auto::etree-components new-exp-node*))))
	  (tactic-output msg t)
	  (comdecode
	    (list 'lemma (linealias pline) '$ 
		  (list 'quote 
			(line-assertion matched-support)) '$
		  '$ (mapcar #'linealias
			     (line-hypotheses matched-support))))
	  (setq new-neg-exp-line
		(car (delete pline
			     (set-difference
			       (mapcar #'car (proof-plans dproof))
			       oldplans))))
	  (same-short new-neg-exp-line matched-support)
	  (setf (line-node new-neg-exp-line) new-neg-node)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq newplan (car (set-difference 
			       (mapcar #'car (proof-plans dproof))
			       oldplans)))
	  (setf (get newplan 'duplicate) (get matched-support 'duplicate)) ; cebrown 8/30/01
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan) new-exp-node*)
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply NEG-EXISTS-ELIM-DUP."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(deftactic neg-or-elim-simple-tac
  (etree-nat
   (lambda (pline)
     (neg-or-elim-simple-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD, and a support line is a negated disjunction,
one of whose disjuncts is inessential (but not both), applies eneg rule.
Same as Pfenning's tactic 218."))

(defun neg-or-elim-simple-match2 (support other-supps pline conn-list)
  (let ((node (line-node support))
	(mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline))))) ; cebrown 2/19/01
    (and (neg-or-match2 support)
	 (not (neg-mated-line support mated-nodes))
	 (let* ((disj-node (car (etree-components node)))
		(left-node (car (etree-components disj-node)))
		(right-node (cadr (etree-components disj-node)))
		(other-nodes (mapcar #'(lambda (x) (line-node x)) other-supps))
		(plan-node (line-node pline))
		(left-iness (auto::spans plan-node (cons right-node other-nodes)
				   conn-list))
		(right-iness (auto::spans plan-node (cons left-node other-nodes)
				    conn-list)))
	   (if left-iness 
	       t ; cebrown 9/10/01 - this used to be (if right-iness nil t),
					; but if either disj works, this should apply
	     (if right-iness t nil))))))

			    


(defun neg-or-elim-simple-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-supports 
	   (if (wffeq 'falsehood (line-assertion pline))
	       (remove-if-not #'(lambda (x) (neg-or-elim-simple-match2 
					      x (remove x supports) pline
					      (line-mating pline)))
			      supports)))
	 (matched-support (car matched-supports))
	 (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-OR-ELIM-SIMPLE to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-supports
	(let ((msg "Applied NEG-OR-ELIM-SIMPLE.")
	      (newplan nil))
	  (tactic-output msg t)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq newplan (car (set-difference 
			       (mapcar #'car (proof-plans dproof))
			       oldplans)))
	  (setf (get newplan 'duplicate) (get matched-support 'duplicate)) ; cebrown 8/30/01
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan) (car (auto::etree-components
					   (line-node matched-support))))
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply NEG-OR-ELIM-SIMPLE."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))





(deftactic neg-or-elim-dup-tac
  (etree-nat
   (lambda (pline)
     (neg-or-elim-dup-tac-etree-nat-fn pline))
   "If planned line is FALSEHOOD, and a support line is a negated disjunction
both of whose disjuncts is essential, applies eneg rule, 
adding the line with its other expansions as a support.
Same as Pfenning's tactic 219."))

(defun neg-or-elim-dup-match2 (support other-supps pline conn-list)
  (let ((node (line-node support))
	(mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline))))) ; cebrown 2/19/01
    (and (neg-or-match2 support)
	 (not (neg-mated-line support mated-nodes))
	 (let* ((disj-node (car (etree-components node)))
		(left-node (car (etree-components disj-node)))
		(right-node (cadr (etree-components disj-node)))
		(other-nodes (mapcar #'(lambda (x) (line-node x)) other-supps))
		(plan-node (line-node pline))
		(left-iness (auto::spans plan-node (cons right-node other-nodes)
				   conn-list))
		(right-iness (auto::spans plan-node (cons left-node other-nodes)
				    conn-list)))
	   (and (not left-iness) (not right-iness))))))

(defun neg-or-elim-dup-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-supports 
	   (if (wffeq 'falsehood (line-assertion pline))
	       (remove-if-not #'(lambda (x) (neg-or-elim-dup-match2 
					      x (remove x supports) pline
					      (line-mating pline)))
			      supports)))
	 (matched-support (car matched-supports))
	 (oldplans (mapcar #'car (proof-plans dproof))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-OR-ELIM-DUP to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-support
	(let* ((msg "Applied NEG-OR-ELIM-DUP.")
               (new-neg-node (auto::copy-negation (line-node matched-support)))
               (new-disj-node (auto::copy-edisjunction
			       (car (etree-components new-neg-node))))
               (new-disj-node2 (auto::copy-edisjunction
                                (car (etree-components new-neg-node))))
               (new-left-node (auto::make-leaf :shallow
                                               (auto::get-shallow 
                                                (car (auto::etree-components
                                                      new-disj-node)))
                                               :parent new-disj-node2
                                               :positive 
                                               (positive-p
                                                new-disj-node2)))
               (new-right-node (auto::make-leaf :shallow
                                               (auto::get-shallow 
                                                (cadr (auto::etree-components
                                                      new-disj-node)))
                                               :parent new-disj-node
                                               :positive 
                                               (positive-p
                                                new-disj-node)))
               (new-left-node2 
                (auto::copy-etree-rec (car (auto::etree-components
                                            new-disj-node))
                                      nil))
               (new-right-node2 
                (auto::copy-etree-rec (cadr (auto::etree-components
                                             new-disj-node))
                                      nil))
               (new-disj-line nil)
               (new-neg-disj-line nil))
	  (tactic-output msg t)
          (setf (auto::etree-parent new-disj-node2) new-neg-node)
          (setf (auto::etree-parent new-disj-node) nil)
          (setf (auto::etree-parent new-left-node2) new-disj-node)
          (setf (auto::etree-parent new-right-node2) new-disj-node2)
          (setf (auto::etree-components new-disj-node)
                (list new-left-node2 new-right-node))
          (setf (auto::etree-components new-disj-node2)
                (list new-left-node new-right-node2))
          (setf (auto::etree-components new-neg-node)
                (list new-disj-node2))
          (setf (auto::etree-parent new-right-node2) new-disj-node2)
          (setf (auto::etree-parent new-left-node2) new-disj-node)
          (comdecode
           (list 'lemma (linealias pline) '$ 
                 (list 'quote 
                       (line-assertion matched-support)) '$
                 '$ '$))
          (setq new-neg-disj-line
                (car (set-difference
                      (current-plan-lines dproof)
                      oldplans)))
          (same-short new-neg-disj-line matched-support)
          (comdecode
           (list 'unsponsor (linealias pline)
                 (list (linealias matched-support))))
          (setq matched-support new-neg-disj-line)
          (setf (line-node matched-support) new-neg-node)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq new-disj-line (car (set-difference 
                                    (current-plan-lines dproof)
                                    oldplans)))
	  (sponsor new-disj-line (list matched-support))
	  (setf (line-mating new-disj-line) (line-mating pline))
	  (setf (line-node new-disj-line) new-disj-node)
	  (values (list new-disj-line) msg 'succeed))
	(let ((msg "Can't apply NEG-OR-ELIM-DUP."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(defun neg-equal-elim-match2 (support pline)
  (let ((node (line-node support))
	(mated-nodes (append (mapcar #'car (line-mating pline)) (mapcar #'cdr (line-mating pline))))) ; cebrown 2/19/01
    (and (auto::negation-p node)
	 (not (neg-mated-line support mated-nodes))
	 (not-p (line-assertion support))
	 (equals-p (cdr (line-assertion support)))
	 (auto::rewrite-p (car (auto::etree-components node)))
	 (memq (auto::rewrite-justification (car (auto::etree-components node)))
	       '(auto::refl= auto::subst= auto::leibniz=)))))


(deftactic neg-equal-elim-tac
  (etree-nat
   (lambda (pline)
     (neg-equal-elim-tac-etree-nat-fn pline))
   "If a support line is a negated equality and planned line is falsehood,
applies eneg.  Similar to Pfenning's tactic 217."))


(defun neg-equal-elim-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-supports 
	   (if (wffeq 'falsehood (line-assertion pline))
	       (remove-if-not #'(lambda (x) 
				  (neg-equal-elim-match2 x pline)) supports)))
	 (matched-support (car matched-supports))
	 (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-EQUAL-ELIM to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-support
	(let ((msg "Applied NEG-EQUAL-ELIM.")
	      (newplan nil))
	  (tactic-output msg t)
	  (comdecode `(eneg ,(linealias pline)
			    ,(linealias matched-support)
			    !))
	  (setq newplan (car (set-difference 
			       (mapcar #'car (proof-plans dproof))
			       oldplans)))
	  (setf (get newplan 'duplicate) (get matched-support 'duplicate)) ; cebrown 8/30/01
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan) (car (auto::etree-components
					   (line-node matched-support))))
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply NEG-EQUAL-ELIM."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))
