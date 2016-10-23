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

(deffile ml-etr-tactics-symsimp
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Defines tactics for symmetric simplification."))

(context quant-tactics)
		    
(deftactic symsimp-tac
  (etree-nat
    (orelse exists-lemma-tac or-lemma-tac)
   "Pfenning's symmetric simplification tactics."))

(deftactic exists-lemma-tac
  (etree-nat
   (lambda (pline)
     (exists-lemma-tac-etree-nat-fn pline))
   "Pfenning's tactic 264."))


(defun exists-lemma-match2 (supports pline)
  (let ((node (line-node pline))
	(support-nodes (mapcar #'(lambda (x) (line-node x)) supports)))
    (and (ml::egen-match1 pline)
	 (auto::expansion-p node)
	 (> (length (auto::expansion-terms node)) 1)
	 (position-if #'(lambda (y) 
			  (auto::admissible-p (cons node support-nodes) y))
		      (auto::expansion-terms node)))))
			    

(defun exists-lemma-tac-etree-nat-fn (pline)
  (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-plans (remove-if-not 
			 #'(lambda (x) (exists-lemma-match2 supports x))
			 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) 
			   (query (format nil  
					  "Apply EXISTS-LEMMA to line ~D?"
					  (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (if matched-plan
	(let ((msg "Applied EXISTS-LEMMA.")
	      (new-plan-line nil)
	      (disj-line nil)
	      (new-supports nil)
	      (support-nodes nil)
	      (auto::leaf-name (gentemp))
	      (auto::*ignore-statuses* t)
	     )
	  (tactic-output msg t)
;;; Need to put in all the stuff here
	  (comdecode (list 'same (linealias pline) (1- (linealias pline)) '!))
	  (setq new-plan-line 
		(car (set-difference (current-plan-lines dproof) oldplans)))
	  (unsponsor new-plan-line supports)
	  (dolist (support supports)
	    (push (copy-support-line support) new-supports))
	  (sponsor new-plan-line (copy-list new-supports))
	  (setq support-nodes
		(let ((nodes nil))
		  (dolist (support new-supports nodes)
		    (push (line-node support) nodes))))
;	  make disj, a and b
	  (multiple-value-bind (disj b a conn-list)
	      (make-exists-lemma-nodes (auto::copy-etree-rec
					(line-node pline) nil)
				       support-nodes
				       (line-mating pline))
	    (setq conn-list
		  (auto::symmetric-simplification support-nodes disj a b conn-list))
;           make disj-line, with shallow-formula of disj, no hyps
	    (comdecode (list 'lemma (linealias new-plan-line) '$
			     (list 'quote (auto::get-shallow disj))
			     (list 'quote (line-assertion new-plan-line))
			     (mapcar #'linealias 
				     (line-hypotheses new-plan-line)) '()))
	    (setq disj-line
		  (car (remove new-plan-line
			       (set-difference (current-plan-lines dproof)
					       oldplans))))
	    (setf (line-just-rule disj-line) "RuleP")
	    (setf (proof-plans dproof)
		  (delete disj-line (proof-plans dproof) :key #'car))
	    (setf (line-node disj-line) disj)
	    (setf (line-mating disj-line) conn-list)
	    (setf (auto::etree-components b)
		(append (auto::etree-components a)
			(auto::etree-components b)))
	    (setf (auto::expansion-terms b)
		  (append (auto::expansion-terms a)
			  (auto::expansion-terms b)))
	    (setf (line-node new-plan-line) b) 
	    (setf (line-mating new-plan-line) conn-list)
	    (values (list new-plan-line) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying EXISTS-LEMMA."
		       "Can't apply EXISTS-LEMMA.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(defun copy-support-line (support)
  (let ((supp-node (auto::copy-etree-rec (line-node support) nil)))
    (make-room-after support 1)
    (comdecode (list 'same (1+ (linealias support)) (linealias support) '!))
    (setf (line-node (numalias (1+ (linealias support)))) supp-node)
    (numalias (1+ (linealias support)))))

(defun find-best-child (exp support-nodes conn-list)
  (do ((comps (auto::etree-components exp)
	      (cdr comps))
       (terms (auto::expansion-terms exp)
	      (cdr terms))
       (max 0)
       (temp -1)
       (max-comp (car (auto::etree-components exp))))
      ((null comps) 
       (position max-comp (auto::etree-components exp)))
    (setq temp (count-if 
		 #'(lambda (x)
		     (let ((node (or (auto::find-etree-node-name
				       (car x) (car comps) t)
				     (auto::find-etree-node-name
				       (cdr x) (car comps) t))))
		       (positive-p node)))
		 (remove-if-not #'(lambda (x)
				    (auto::contains-elt-in 
				      x (car comps)))
				conn-list)))
    (when (and (auto::admissible-p (cons exp support-nodes)
				   (car terms))
	       (> temp max))
      (setq max temp max-comp (car comps)))))  

(defun make-exists-lemma-nodes (exp support-nodes conn-list)
  (let* ((pos (find-best-child exp support-nodes conn-list))
	 (not-c-exp (auto::copy-etree-rec exp nil))
	 (not-c (auto::make-negation :junctive 'auto::neutral
		  :components (list not-c-exp)
		  :positive t))
	 (c nil)
	 (a (auto::copy-etree-rec exp nil))
	 (b nil)
	 (disj (auto::make-edisjunction :junctive 'auto::dis
		 :positive t
		 :components nil)))
    (setf (auto::etree-components not-c-exp)
	  (list (nth pos (auto::etree-components not-c-exp))))
    (setf (auto::expansion-terms not-c-exp)
	  (list (nth pos (auto::expansion-terms not-c-exp))))
    (setf (auto::etree-components a)
	  (nconc (subseq (auto::etree-components a) 0 pos)
		 (subseq (auto::etree-components a) (1+ pos))))
    (setf (auto::expansion-terms a)
	  (nconc (subseq (auto::expansion-terms a) 0 pos)
		 (subseq (auto::expansion-terms a) (1+ pos))))

    (setq b (auto::copy-expansion not-c-exp))
    (setf (auto::expansion-terms b) nil
          (auto::etree-components b) nil)
    (auto::duplicate-var b)
    (auto::strip-exp-vars-from-etree b)     ; added 15AUG90 DAN
    (setf (auto::etree-components b)
          (list (auto::deepen-to-literals (car (auto::etree-components b)))))
    (auto::strip-sk-terms-from-etree b)
    (multiple-value-setq (c conn-list) (dup-etree-neg b disj conn-list))
    (setf (auto::etree-parent not-c-exp) not-c)
    (setf (auto::etree-parent not-c) disj)
    (setf (auto::etree-components disj) (list c not-c))
    (values disj a b conn-list)))

;;; makes a copy of node and makes it the child of parent
;;; assume no rewrite nodes !!!!!!!!!!
;;; at expansion nodes makes just one child

(defun dup-etree (node parent shallow)
  (let ((newnode (auto::copy-etree-rec node nil))
	(shallow (auto::strip-sk-terms shallow)))
    (setf (auto::etree-parent newnode) parent)
    (typecase node
      (leaf 
	(setf (auto::leaf-shallow newnode) shallow)
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring leaf-name))))
      (negation
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring neg-name)))
	(setf (auto::etree-components newnode)
	      (list (dup-etree (car (auto::etree-components node))
			       newnode
			       (gdr shallow)))))
      (true 
	(setf (auto::etree-name newnode)
	      (create-namestring true-name)))
      (false 
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring false-name))))
      (expansion
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring expansion-name)))
	(setf (auto::expansion-shallow newnode) shallow)
	(setf (auto::expansion-terms newnode)
	      (list (funcall ren-var-fn (bindvar shallow))))
	(setf (auto::etree-components newnode)
	      (list (dup-etree (car (auto::etree-components node)) 
			       newnode
			       (substitute-term-var 
				 (car (auto::expansion-terms newnode))
				 (bindvar shallow)
				 (gdr shallow))))))
      (skolem
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring skolem-selection-name)))
	(setf (auto::skolem-shallow newnode) shallow)
	(setf (auto::skolem-terms newnode)
	      (list (funcall ren-var-fn (bindvar shallow))))
	(setf (auto::etree-components newnode)
	      (list (dup-etree (car (auto::etree-components node)) 
			       newnode
			       (substitute-term-var 
				 (car (auto::skolem-terms newnode))
				 (bindvar shallow)
				 (gdr shallow))))))
      (selection
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring selection-name)))
	(setf (auto::selection-shallow newnode) shallow)
	(setf (auto::selection-terms newnode)
	      (list (funcall ren-var-fn (bindvar shallow))))
	(setf (auto::etree-components newnode)
	      (list (dup-etree (car (auto::etree-components node)) 
			       newnode
			       (substitute-term-var 
				 (car (auto::selection-terms newnode))
				 (bindvar shallow)
				 (gdr shallow))))))
      ((or edisjunction econjunction implication)
	(setf (auto::etree-name newnode)
	  (intern-str 
	   (typecase newnode
		(edisjunction (create-namestring edisj-name))
		(econjunction (create-namestring econj-name))
		(implication (create-namestring imp-name)))))
	(setf (auto::etree-components newnode)
	      (list (dup-etree (car (auto::etree-components node)) 
			       newnode
			       (glr shallow))
		    (dup-etree (cadr (auto::etree-components node)) 
			       newnode
			       (grr shallow)))))
      (rewrite (throwfail "DUP-ETREE can't handle rewrite nodes now.")))
    newnode))


;;; duplicates nodes, but with opposite polarity.
;;; if node is expansion, makes a skolem; if node is skolem or selection,
;;; makes an expansion, using the same term.
;;; also adds connection of two leaves to conn-list

(defun dup-etree-neg (node parent conn-list)
  (let ((newnode nil))
    (typecase node
      (leaf 
	(setq newnode (auto::copy-etree-rec node nil))
	(setf (auto::leaf-shallow newnode)
	  (auto::strip-sk-terms (auto::leaf-shallow newnode)))
	(setf (auto::etree-parent newnode) parent)
	(setf (auto::etree-name newnode)
	      (intern-str (create-namestring leaf-name)))
	(push (cons (auto::etree-name newnode)
		    (auto::etree-name node))
	      conn-list))
      (negation
	(setq newnode (auto::copy-etree-rec node nil))
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring neg-name)))
	(multiple-value-bind (kid conn-list*)
	  (dup-etree-neg (car (auto::etree-components node)) newnode
			 conn-list)
	  (setq conn-list conn-list*)
	  (setf (auto::etree-components newnode)
		(list kid))))
      (true 
	(setq newnode (auto::copy-etree-rec node nil))
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring true-name))))
      (false 
	(setq newnode (auto::copy-etree-rec node nil))
	(setf (auto::etree-name newnode)
	  (intern-str (create-namestring false-name))))
      ((or skolem selection)
       (setq newnode
	     (auto::make-expansion
	       :shallow (auto::strip-sk-terms (auto::get-shallow node))
	       :terms (mapcar #'auto::strip-sk-terms 
			      (if (auto::skolem-p node)
				  (auto::skolem-terms node)
				(auto::selection-terms node)))
	       :positive (not (auto::etree-positive node))
	       :name (intern-str (create-namestring expansion-name))))
       (multiple-value-bind (kid conn-list*)
	 (dup-etree-neg (car (auto::etree-components node)) newnode
			conn-list)
	 (setq conn-list conn-list*)
	 (setf (auto::etree-components newnode) (list kid))))
      (rewrite 
       (setq newnode
	     (auto::make-rewrite
	       :shallow (auto::strip-sk-terms (auto::get-shallow node))
	       :justification (auto::rewrite-justification node)
	       :junctive 'auto::neutral
	       :positive (not (auto::etree-positive node))
	       :name (intern-str (create-namestring rewrite-name))))
       (multiple-value-bind (kid conn-list*)
	 (dup-etree-neg (car (auto::etree-components node)) newnode
			conn-list)
	 (setq conn-list conn-list*)
	 (setf (auto::etree-components newnode) (list kid))))
      (expansion
       (setq newnode
	     (auto::make-skolem
	       :shallow (auto::strip-sk-terms (auto::get-shallow node))
	       :terms (mapcar #'auto::strip-sk-terms
			      (auto::expansion-terms node))
	       :junctive 'auto::neutral
	       :name (intern-str (create-namestring skolem-selection-name))))
       (multiple-value-bind (kid conn-list*)
	 (dup-etree-neg (car (auto::etree-components node)) newnode
			conn-list)
	 (setq conn-list conn-list*)
	 (setf (auto::etree-components newnode) (list kid))))
      ((or edisjunction econjunction implication)
	(setq newnode (auto::copy-etree-rec node nil))
	(setf (auto::etree-name newnode)
	      (typecase newnode
		(edisjunction (intern-str (create-namestring edisj-name)))
		(econjunction (intern-str (create-namestring econj-name)))
		(implication (intern-str (create-namestring imp-name)))))
	(multiple-value-bind (kid conn-list*)
	  (dup-etree-neg (car (auto::etree-components node)) newnode
			 conn-list)
	  (multiple-value-bind (kid* conn-list**)
	    (dup-etree-neg (cadr (auto::etree-components node)) newnode
			 conn-list*)
	    (setq conn-list conn-list**)
	    (setf (auto::etree-components newnode)
		  (list kid kid*))))))
    (setf (auto::etree-positive newnode)
	  (not (auto::etree-positive newnode)))
    (setf (auto::etree-parent newnode) parent)
    (setf (auto::etree-junctive newnode)
	  (case (auto::etree-junctive node)
	    (auto::dis 'auto::con)
	    (auto::con 'auto::dis)
	    (auto::neutral 'auto::neutral)
	    (otherwise nil)))
    (values newnode conn-list)))
	
(context prop-tactics)
(deftactic or-lemma-left-tac
  (etree-nat
   (lambda (pline)
     (or-lemma-left-tac-etree-nat-fn pline))
   "Pfenning's tactic 265."))


(defun or-lemma-left-tac-etree-nat-fn (pline)
  (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-plans
	  (remove-if-not #'(lambda (x) 
			     (class-disj-match2 
			       x supports))
			 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) 
			   (query (format nil  
					  "Apply OR-LEMMA-LEFT to line ~D?"
					  (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (if matched-plan
	(let ((msg "Applied OR-LEMMA-LEFT.")
	      (new-plan-line nil)
	      (disj-line nil)
	      (new-supports nil)
	      (support-nodes nil)
	      (auto::leaf-name (gentemp))
	      (auto::*ignore-statuses* t)
)
	  (tactic-output msg t)
	  (comdecode (list 'same (linealias pline) (1- (linealias pline)) '!))
	  (setq new-plan-line 
		(car (set-difference (current-plan-lines dproof) oldplans)))
	  (setf (line-node new-plan-line)
		(auto::copy-etree-rec
		  (line-node pline) nil))
	  (setf (line-mating new-plan-line) (line-mating pline))
	  (unsponsor new-plan-line supports)
	  (dolist (support supports)
	    (push (copy-support-line support) new-supports))
	  (sponsor new-plan-line (copy-list new-supports))
	  (setq support-nodes
		(let ((nodes nil))
		  (dolist (support new-supports nodes)
		    (push (line-node support) nodes))))
;	  make disj, a and b
	  (multiple-value-bind (disj a b conn-list)
	    (make-or-left-lemma-nodes (line-node new-plan-line)
				      (line-mating new-plan-line))
	    (setq conn-list
		  (auto::symmetric-simplification support-nodes disj a b conn-list))
;	    (setq disj (remove-unnec-negs disj))
;           make disj-line, with shallow-formula of disj, no hyps
	    (comdecode (list 'lemma (linealias new-plan-line) '$
			     (list 'quote (auto::get-shallow disj))
			     (list 'quote (line-assertion new-plan-line))
			     (mapcar #'linealias 
				     (line-hypotheses new-plan-line)) '()))
	    (setq disj-line
		  (car (remove new-plan-line
			       (set-difference (current-plan-lines dproof)
					       oldplans))))
	    (setf (line-just-rule disj-line) "RuleP")
	    (setf (proof-plans dproof)
		  (delete disj-line (proof-plans dproof) :key #'car))
	    (setf (line-node disj-line) disj)
	    (setf (line-mating new-plan-line) conn-list)
	    (setf (line-node new-plan-line)
		  (auto::make-edisjunction :junctive 'auto::con
					   :positive nil
					   :components (list a b)))
	    (values (list new-plan-line ) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying OR-LEMMA-LEFT."
		       "Can't apply OR-LEMMA-LEFT.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(deftactic or-lemma-right-tac
  (etree-nat
   (lambda (pline)
     (or-lemma-right-tac-etree-nat-fn pline))
   "Pfenning's tactic 265."))


(defun or-lemma-right-tac-etree-nat-fn (pline)
  (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-plans
	  (remove-if-not #'(lambda (x) 
			     (class-disj-match2 
			       x supports))
			 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) 
			   (query (format nil  
					  "Apply OR-LEMMA-RIGHT to line ~D?"
					  (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (if matched-plan
	(let ((msg "Applied OR-LEMMA-RIGHT.")
	      (new-plan-line nil)
	      (disj-line nil)
	      (new-supports nil)
	      (support-nodes nil)
	      (auto::leaf-name (gentemp))
	      (auto::*ignore-statuses* t)
)
	  (tactic-output msg t)
	  (comdecode (list 'same (linealias pline) (1- (linealias pline)) '!))
	  (setq new-plan-line 
		(car (set-difference (current-plan-lines dproof) oldplans)))
	  (setf (line-node new-plan-line)
		(auto::copy-etree-rec
		  (line-node pline) nil))
	  (setf (line-mating new-plan-line) (line-mating pline))
	  (unsponsor new-plan-line supports)
	  (dolist (support supports)
	    (push (copy-support-line support) new-supports))
	  (sponsor new-plan-line (copy-list new-supports))
	  (setq support-nodes
		(let ((nodes nil))
		  (dolist (support new-supports nodes)
		    (push (line-node support) nodes))))
;	  make disj, a and b
	  (multiple-value-bind (disj b a conn-list)
	    (make-or-right-lemma-nodes (line-node new-plan-line)
				      (line-mating new-plan-line))
	    (setq conn-list
		  (auto::symmetric-simplification support-nodes disj a b conn-list))

;           make disj-line, with shallow-formula of disj, no hyps
	    (comdecode (list 'lemma (linealias new-plan-line) '$
			     (list 'quote (auto::get-shallow disj))
			     (list 'quote (line-assertion new-plan-line))
			     (mapcar #'linealias 
				     (line-hypotheses new-plan-line)) '()))
	    (setq disj-line
		  (car (remove new-plan-line
			       (set-difference (current-plan-lines dproof)
					       oldplans))))
	    (setf (line-just-rule disj-line) "RuleP")
	    (setf (proof-plans dproof)
		  (delete disj-line (proof-plans dproof) :key #'car))
	    (setf (line-node disj-line) disj)
	    (setf (line-mating new-plan-line) conn-list)
	    (setf (line-node new-plan-line)
		  (auto::make-edisjunction :junctive 'auto::con
					   :positive nil
					   :components (list b a)))
	    (values (list new-plan-line ) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying OR-LEMMA-RIGHT."
		       "Can't apply OR-LEMMA-RIGHT.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))





(defun make-or-right-lemma-nodes (plan-disj conn-list)
  (make-or-lemma-nodes plan-disj conn-list))

(defun make-or-left-lemma-nodes (plan-disj conn-list)
  (setf (auto::etree-components plan-disj)
	(nreverse (auto::etree-components plan-disj)))
  (multiple-value-bind (disj b a conn-list*)
    (make-or-lemma-nodes plan-disj conn-list)
    (setf (auto::etree-components plan-disj)
	  (nreverse (auto::etree-components plan-disj)))
    (values disj a b conn-list*)))



(defun make-or-lemma-nodes (plan-disj conn-list)
  (let* ((not-c (auto::copy-etree-rec (cadr (auto::etree-components plan-disj))
				      nil))
		
	 (a (auto::copy-etree-rec (car (auto::etree-components plan-disj))
			      nil))
	 (not-c* (auto::make-negation :junctive 'auto::neutral
		   :components (list not-c)
		   :positive t))
	 (c nil)
	 (b (auto::copy-etree-rec not-c t))
	 (disj (auto::make-edisjunction :junctive 'auto::dis
		 :positive t
		 :components nil)))
    (multiple-value-setq (c conn-list) (dup-etree-neg b disj conn-list))
    (setf (auto::etree-parent not-c) not-c*)
    (setf (auto::etree-parent not-c*) disj)
    (setf (auto::etree-components disj) (list c not-c*))
    (values disj a b conn-list)))

(deftactic or-lemma-tac
  (etree-nat
   (lambda (pline)
     (or-lemma-tac-etree-nat-fn pline))
   "Applies either or-lemma-right-tac or or-lemma-left-tac if applicable."))

(defun or-lemma-tac-etree-nat-fn (pline)
  (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-plans
	  (remove-if-not #'(lambda (x) 
			     (class-disj-match2 
			       x supports))
			 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) 
			   (query (format nil  
					  "Apply OR-LEMMA to line ~D?"
					  (linealias x)) t))
		       matched-plans)
	      (car matched-plans))))
    (if matched-plan
	(let* ((msg "Applied OR-LEMMA.")
	       (left (car (etree-components (line-node pline))))
	       (right (cadr (etree-components (line-node pline))))
	       (conns (line-mating pline)))
	  (tactic-output msg t)
	  ;; Heuristic used to decide whether to use the left disjunct
	  ;; or the right disjunct as the starting point of the lemma is 
	  ;; to count the number of connections below each in which the
	  ;; node involved is positive.  Why?  Just seems like we want these
	  ;; nodes to be supports rather than conclusions.
	  (if (>= (count-if #'(lambda (x)
				(let ((node (or (auto::find-etree-node-name
						  (car x) left t)
						(auto::find-etree-node-name
						  (cdr x) left t))))
				  (positive-p node)))
			    (remove-if-not #'(lambda (x)
					       (auto::contains-elt-in x left))
					   conns))
		  (count-if #'(lambda (x)
				(let ((node (or (auto::find-etree-node-name
						  (car x) right t)
						(auto::find-etree-node-name
						  (cdr x) right t))))
				  (positive-p node)))
			    (remove-if-not #'(lambda (x)
					       (auto::contains-elt-in x right))
					   conns)))
	      (or-lemma-left-tac-etree-nat-fn pline)
	      (or-lemma-right-tac-etree-nat-fn pline)))
	(let ((msg (if matched-plans
		       "Not applying OR-LEMMA."
		       "Can't apply OR-LEMMA.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))
