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

(deffile ml-etr-tactics-eq
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Defines tactics for equalities as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs."))

(context equality-tactics)



(defun refl=-match2 (pline)
  (and (auto::rewrite-p (line-node pline))
       (eq (auto::rewrite-justification (line-node pline)) 'auto::refl=)))
      
(deftactic refl=-tac 
  (etree-nat
    (lambda (pline)
      (refl=-tac-etree-nat-fn pline))
    "If the planned line is a rewrite node with justification REFL=, applies
the ASSERT rule for reflexivity of equality.  See Pfenning's theorem 141.1."))

(defun refl=-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'refl=-match2 (list pline)))
	 (matched-plan (car matched-plans)))
    (when (eq tacmode 'interactive)
	  (setq matched-plan
		(find-if #'(lambda (x)
			     (query (format nil  "Apply REFL= to line ~D?"
					    (linealias x)) t))
			 matched-plans)))
    (if matched-plan
	(let ((msg "Applied REFL=."))
	  (tactic-output msg t)
	  (comdecode (list 'assert 'refl= (linealias matched-plan)))
	  (values nil msg 'succeed))
      (let ((msg (if matched-plans 
		     "Not applying REFL=."
		     "Can't apply REFL=.")))
	(tactic-output msg nil)
	(values (list pline) msg 'fail)))))

(defun etree-name-2 (x)
  (when (etree-p x) (etree-name x)))

(defun subst=l-match2 (support pline other-supports conn-list)
  (let* ((plan-node (line-node pline))
	 (support-node (line-node support))
	 (exp-node (auto::find-etree-node #'auto::expansion-p support-node))
	 (components (if exp-node (auto::etree-components exp-node)))
	 (imp-nodes (mapcar #'(lambda (x) (auto::find-etree-node #'implication-p x)) components))
	 (other-nodes (mapcar #'(lambda (x) (line-node x)) other-supports)))
    (and (subst=l-match1 support)
	 (not (mated-line support (line-mating pline))) ; cebrown 9/5/00
	 (auto::rewrite-p support-node)
	 (eq 'auto::subst= (auto::rewrite-justification support-node))
	 components
	 (find-if #'(lambda (x)
		      (and (auto::admissible-p (cons plan-node other-nodes) (car x))
			   (not (auto::spans plan-node
					     (append (remove (caddr x) imp-nodes) other-nodes)
					     conn-list))
			   (caddr x)
			   (let ((left-node (car (auto::etree-components (caddr x))))
				 (right-node (cadr (auto::etree-components (caddr x)))))
			     (and 
			      (auto::spans left-node
					       (append (remove (caddr x) imp-nodes) other-nodes)
					       conn-list)
				  (auto::spans (line-node pline)
					       (cons right-node
						     (append (remove (caddr x) imp-nodes)
							     other-nodes))
					       conn-list)))))
		  ;;the line below produces a list of 3-element lists (term component imp-node)
		  (map 'list #'list (auto::expansion-terms exp-node)
		       components imp-nodes)))))

(defun subst=-tac-etree-nat-fn (pline)
  (let ((slist (cdr (assoc pline (proof-plans dproof)))))
    (dolist (s slist)
      (if (subst=l-match2 s pline (remove s slist) (line-mating pline)) 
	  (return (subst=l-tac-etree-nat-fn pline))
	(when (subst=r-match2 s pline (remove s slist) (line-mating pline))
	  (return (subst=r-tac-etree-nat-fn pline)))))))
	 

(defun subst=l-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-support 
	  (find-if #'(lambda (x) (subst=l-match2 x pline (remove x supports) (line-mating pline)))
		   supports))
	 (matched-support 
	  (if (and (eq tacmode 'interactive) matched-support)
	      (find-if #'(lambda (x) (query (format nil  "Apply SUBST=L to line ~D?" (linealias x)) t))
		       (list matched-support))
	    matched-support))
	 (node (if matched-support (line-node matched-support)))
	 (exp-node (if node (auto::find-etree-node #'expansion-p node)))
	 (otherplans (delete pline (current-plan-lines dproof)))
	 matched-kid temp imp-node pos left-node right-node)
    (when matched-support
	  (setq temp (subst=l-match2 matched-support pline (remove matched-support supports) (line-mating pline)))
	  (setq matched-kid (cadr temp))
	  (setq imp-node (caddr temp))
	  (setq pos (position matched-kid (auto::etree-components exp-node)))
	  (setq left-node (car (auto::etree-components imp-node)))
	  (setq right-node (cadr (auto::etree-components imp-node))))
    (if matched-support
	(if (cdr (auto::etree-components exp-node))
	    ;; Expansion node has more than one expansion
	   (let ((msg "Applied SUBST=L.")
		 (new-rew-node (auto::copy-rewrite (line-node matched-support)))
		 (new-exp-node (auto::copy-expansion exp-node))
		 (new-eq-line nil)
		 (left-line nil)
		 (right-line nil))
	     (setf (auto::expansion-terms new-exp-node)
		   (append (subseq (auto::expansion-terms new-exp-node) 0 pos)
			   (subseq (auto::expansion-terms new-exp-node) (1+ pos))))
	     (setf (auto::etree-components new-exp-node)
		   (append (subseq (auto::etree-components new-exp-node) 0 pos)
			   (subseq (auto::etree-components new-exp-node) (1+ pos))))
	     (setf (auto::etree-parent new-exp-node) new-rew-node)
	     (setf (auto::etree-components new-rew-node) (list new-exp-node))
	     (tactic-output msg t)
	     (comdecode (list 'lemma (linealias pline) (car (auto::get-useful-range pline)) ;'$
			      (list 'quote (line-assertion matched-support)) '$
			      '$ (mapcar #'linealias (line-hypotheses matched-support))))
	     (setq new-eq-line (car (delete pline (set-difference (current-plan-lines dproof) otherplans))))
	     (same-short new-eq-line matched-support)
	     (comdecode (list 'subst=l (linealias new-eq-line) '$ '$
			      (list 'quote (auto::get-shallow left-node))
			      (list 'quote (auto::get-shallow right-node))
			      '$ '$ '$ 
			      (mapcar #'linealias (line-hypotheses pline))
			      (mapcar #'linealias (line-hypotheses pline))))
	     (setq left-line (car (delete pline (set-difference (current-plan-lines dproof) otherplans))))
	     (setq right-line (car (remove left-line (remove new-eq-line 
							     (set-difference (cdr (assoc pline (proof-plans dproof)))
									     supports)))))
	     ;; make sure left-line has the right supports
	     (comdecode (list 'sponsor (linealias left-line)
			      (remove-if #'(lambda (x) (>= x (linealias left-line)))
					 (mapcar #'linealias (cdr (assoc pline (proof-plans dproof)))))))
	     ;; remove the matched-support line as a support for pline
	     (comdecode (list 'unsponsor (linealias pline) (list (linealias matched-support) (linealias left-line))))
	     ;; remove matched-support line as support for left-line
	     (comdecode (list 'unsponsor (linealias left-line) (list (linealias matched-support))))
	     ;; add the new eq line as a support for left-line
	     (comdecode (list 'sponsor (linealias left-line) (list (linealias new-eq-line))))
	     ;; remove left-line and right-line from supports of all other
	     ;; planned lines -- it might not yet be admissible
	     (dolist (pl otherplans) 
	       (comdecode (list 'unsponsor (linealias pl) (list (linealias right-line) (linealias left-line)))))
	     (setf (line-node new-eq-line) new-rew-node)
	     (setf (line-node left-line) left-node)
	     (setf (line-node right-line) right-node)
	     (setf (line-mating left-line) (line-mating pline))
	     (values (list pline left-line) msg 'succeed))
	   ;; only one expansion term, admissible for current goal
	   (let ((msg "Applied SUBST=L.")
		 (left-line nil)
		 (right-line nil))
	     (tactic-output msg t)
	     (comdecode (list 'subst=l (linealias matched-support) '$ '$
			      (list 'quote (auto::get-shallow left-node))
			      (list 'quote (auto::get-shallow right-node))
			      '$ '$ '$ 
			      (mapcar #'linealias (line-hypotheses pline))
			      (mapcar #'linealias (line-hypotheses pline))))
	     (setq left-line
		   (car (delete pline 
				(set-difference 
				  (current-plan-lines dproof)
				  otherplans))))
	     (setq right-line
		   (car (remove left-line 
				(set-difference 
				  (cdr (assoc pline (proof-plans dproof)))
				  supports))))
	     ;; make sure left-line has the right supports
	     (comdecode
	       (list 'sponsor (linealias left-line)
		     (remove-if #'(lambda (x) (>= x (linealias left-line)))
				(mapcar #'linealias 
					(cdr (assoc pline (proof-plans dproof)))))))
	     ;; remove the matched-support line as a support for pline
	     (comdecode (list 'unsponsor (linealias pline) (list (linealias matched-support) (linealias left-line))))
	     (comdecode (list 'unsponsor (linealias left-line) (list (linealias matched-support))))
	     ;; remove right-line and left-line from supports of all other
	     ;; planned lines -- it might not yet be admissible
	     (dolist (pl otherplans)
	       (comdecode (list 'unsponsor (linealias pl) (list (linealias right-line) (linealias left-line)))))
	     (setf (line-node right-line) right-node)
	     (setf (line-node left-line) left-node)	     
	     (setf (line-mating left-line) (line-mating pline))
	     (values (list pline left-line) msg 'succeed)))
	(let ((msg "Can't apply SUBST=L." ))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))

(deftactic subst=-tac
  (etree-nat
   (lambda (pline)
     (subst=-tac-etree-nat-fn pline))
   "Applies either SUBST=L-TAC or SUBST=R-TAC as appropriate."))

(deftactic subst=l-tac
  (etree-nat
   (lambda (pline)
     (subst=l-tac-etree-nat-fn pline))
   "If a support line is an equality, and the planned line follows from the
substituting the right-hand-side for the left-hand-side in some wff provable
from the other supports, applies Subst=L.  See Pfenning's theorem 141."))

;;;;;;;;;;;;;;;;;;;;



(defun subst=r-match2 (support pline other-supports conn-list)
  (let* ((plan-node (line-node pline))
	 (support-node (line-node support))
	 (exp-node (auto::find-etree-node #'auto::expansion-p support-node))
	 (components (if exp-node (auto::etree-components exp-node)))
	 (imp-nodes (mapcar #'(lambda (x) (auto::find-etree-node #'implication-p x)) components))
	 (other-nodes (mapcar #'(lambda (x) (line-node x)) other-supports)))
    (and (subst=r-match1 support)
	 (not (mated-line support (line-mating pline))) ; cebrown 9/5/00
	 (auto::rewrite-p support-node)
	 (eq 'auto::subst= (auto::rewrite-justification support-node))
	 components
	 (find-if #'(lambda (x)
		      (and (auto::admissible-p (cons plan-node other-nodes) (car x))
			   (not (auto::spans plan-node
					     (append (remove (caddr x) imp-nodes) other-nodes)
					     conn-list))
			   (caddr x)
			   (let ((left-node (car (auto::etree-components (caddr x))))
				 (right-node (cadr (auto::etree-components (caddr x)))))
			     (and (auto::spans right-node
					       (append (remove (caddr x) imp-nodes) other-nodes)
					       conn-list)
				  (auto::spans (line-node pline)
					       (cons left-node
						     (append (remove (caddr x) imp-nodes)
							     other-nodes))
					       conn-list)))))
		  (map 'list #'list (auto::expansion-terms exp-node)
		       components imp-nodes)))))

(defun subst=r-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-support 
	  (find-if #'(lambda (x)
		       (subst=r-match2 x pline (remove x supports) (line-mating pline)))
		   supports))
	 (matched-support 
	  (if (and (eq tacmode 'interactive) matched-support)
	      (find-if #'(lambda (x) (query (format nil  "Apply SUBST=R to line ~D?" (linealias x)) t))
		       (list matched-support))
	    matched-support))
	 (node (when matched-support (line-node matched-support)))
	 (exp-node (when node (auto::find-etree-node #'expansion-p node)))
	 (otherplans (delete pline (current-plan-lines dproof)))
	 pos matched-kid temp imp-node left-node right-node)
    (when matched-support
	  (setq temp (subst=r-match2 matched-support pline (remove matched-support supports) (line-mating pline)))
	  (setq matched-kid (cadr temp))
	  (setq imp-node (caddr temp))
	  (setq pos (position matched-kid (auto::etree-components exp-node)))
	  (setq left-node (car (auto::etree-components imp-node)))
	  (setq right-node (cadr (auto::etree-components imp-node))))
    (if matched-support
	(if (cdr (auto::etree-components exp-node))
	    ;; Expansion node has more than one expansion
	   (let ((msg "Applied SUBST=R.")
		 (new-rew-node (auto::copy-rewrite 
				 (line-node matched-support)))
		 (new-exp-node (auto::copy-expansion exp-node))
		 (new-eq-line nil)
		 (left-line nil)
		 (right-line nil))
	     (setf (auto::expansion-terms new-exp-node)
		   (append (subseq (auto::expansion-terms new-exp-node) 0 pos)
			   (subseq (auto::expansion-terms new-exp-node) 
				   (1+ pos))))
	     (setf (auto::etree-components new-exp-node)
		   (append (subseq (auto::etree-components new-exp-node) 0 pos)
			   (subseq (auto::etree-components new-exp-node)
				   (1+ pos))))
	     (setf (auto::etree-parent new-exp-node) new-rew-node)
	     (setf (auto::etree-components new-rew-node) (list new-exp-node))
	     (tactic-output msg t)
	     (comdecode
		   (list 'lemma (linealias pline) (car (auto::get-useful-range pline)) ;'$
			 (list 'quote 
			       (line-assertion matched-support)) '$
			 '$ (mapcar #'linealias
				    (line-hypotheses matched-support))))
	     (setq new-eq-line
		   (car (delete pline
				(set-difference
				 (current-plan-lines dproof)
				 otherplans))))
	     (same-short new-eq-line matched-support)
	     ;; wff is of the form [b implies a], we want
	     ;; to prove ~a, then derive ~b by subst=, then get pline
	     ;; from ~b
	     ;; We can, however, remove any resulting double negation
	     ;; from ~a, ~b
	     (comdecode (list 'subst=r (linealias new-eq-line) '$ '$
			      (list 'quote 
				    (if (auto::negation-p right-node)
					(cdr (auto::get-shallow
					      right-node))
				      (cons 'not 
					    (auto::get-shallow right-node))))
			      (list 'quote 
				    (if (auto::negation-p left-node)
					(cdr (auto::get-shallow
					      left-node))
				      (cons 'not 
					    (auto::get-shallow left-node))))
			      '$ '$ '$ 
			      (mapcar #'linealias (line-hypotheses pline))
			      (mapcar #'linealias (line-hypotheses pline))))
	     (setq left-node 
		   (cond ((auto::negation-p left-node)
			  (car (auto::etree-components left-node)))
			 #+comment((auto::rewrite-p left-node)
				   (let ((left-rew-node (auto::copy-rewrite left-node)))
				     (setf (auto::etree-components left-rew-node)
					   (auto::etree-components 
					    (car (auto::etree-components left-rew-node))))
				     left-rew-node))
			 (t
			  (let ((new-neg (auto::make-negation 
					  :components (list left-node)
					  :positive (auto::positive-p
						     left-node))))
			    new-neg))))
	     (setq right-node 
	       (cond  ((auto::negation-p right-node)
		       (car (auto::etree-components right-node)))
		      #+comment((auto::rewrite-p right-node)
		       (let ((right-rew-node (auto::copy-rewrite right-node)))
			 (setf (auto::etree-components right-rew-node)
			   (auto::etree-components 
			    (car (auto::etree-components right-rew-node))))
			 right-rew-node))
		      (t
		       (let ((new-neg (auto::make-negation 
				       :components (list right-node)
				       :positive (auto::positive-p
						  right-node))))
			 new-neg))))		      
	     (setq right-line
		   (car (delete pline (set-difference 
					(current-plan-lines dproof)
					otherplans))))
	     (setq left-line
		   (car (remove right-line
				(remove new-eq-line
					(set-difference 
					  (cdr (assoc pline 
						      (proof-plans dproof)))
					  supports)))))
	     ;; make sure right-line has the right supports
	     (comdecode
	       (list 'sponsor (linealias right-line)
		     (remove-if #'(lambda (x) (>= x (linealias right-line)))
				(mapcar #'linealias 
					(cdr (assoc pline (proof-plans dproof)))))))
	       ;; remove the matched-support line as a support for pline
	     (comdecode (list 'unsponsor (linealias pline) (list (linealias matched-support) (linealias right-line))))
	     ;; remove left-line and right-line from supports of all other
	     ;; planned lines -- it might not yet be admissible
	     (dolist (pl otherplans)
	       (comdecode (list 'unsponsor (linealias pl) (list (linealias right-line) (linealias left-line)))))
	     (setf (line-node new-eq-line) new-rew-node)
	     (setf (line-node left-line) left-node)
	     (setf (line-node right-line) right-node)
	     (setf (line-mating right-line) (line-mating pline))
	     (values (list pline right-line) msg 'succeed))
	   ;; only one expansion term, admissible for current goal
	   (let ((msg "Applied SUBST=R.")
		 (left-line nil)
		 (right-line nil))
	     (tactic-output msg t)
	     ;; wff is of the form [~b implies ~a], we want
	     ;; to prove a, then derive b by subst=, then get pline from b
	     (comdecode (list 'subst=r (linealias matched-support) '$ '$ 
			      (list 'quote 
				    (if (auto::negation-p right-node)
					(cdr (auto::get-shallow
					      right-node))
				      (cons 'not 
					    (auto::get-shallow right-node))))
				    
			      (list 'quote 
				    (if (auto::negation-p left-node)
					(cdr (auto::get-shallow
					      left-node))
				      (cons 'not 
					    (auto::get-shallow left-node))))
			      '$ '$ '$ 
			      (mapcar #'linealias (line-hypotheses pline))
			      (mapcar #'linealias (line-hypotheses pline))))
	     (setq left-node 
		   (cond ((auto::negation-p left-node)
			  (car (auto::etree-components left-node)))
			 #+comment((auto::rewrite-p left-node)
			  (let ((left-rew-node (auto::copy-rewrite left-node)))
			    (setf (auto::etree-components left-rew-node)
			      (auto::etree-components 
			       (car (auto::etree-components left-rew-node))))
			    left-rew-node))
			 (t
			  (let ((new-neg (auto::make-negation 
					  :components (list left-node)
					  :positive (auto::positive-p
						     left-node))))
			    new-neg))))
	     (setq right-node 
	       (cond  ((auto::negation-p right-node)
		       (car (auto::etree-components right-node)))
		      #+comment((auto::rewrite-p right-node)
		       (let ((right-rew-node (auto::copy-rewrite right-node)))
			 (setf (auto::etree-components right-rew-node)
			   (auto::etree-components 
			    (car (auto::etree-components right-rew-node))))
			 right-rew-node))
			 (t
			  (let ((new-neg (auto::make-negation 
					  :components (list right-node)
					  :positive (auto::positive-p
						     right-node))))
			    new-neg))))		      
	     (setq right-line
		   (car (delete pline 
				(set-difference 
				  (current-plan-lines dproof)
				  otherplans))))
	     (setq left-line
		   (car (remove right-line 
				(set-difference 
				  (cdr (assoc pline (proof-plans dproof)))
				  supports))))
	     ;; make sure right-line has the right supports
	     (comdecode
	       (list 'sponsor (linealias right-line)
		     (remove-if #'(lambda (x) (>= x (linealias right-line)))
				(mapcar #'linealias 
					(cdr (assoc pline (proof-plans dproof)))))))
	       ;; remove the matched-support line as a support for pline
	     (comdecode (list 'unsponsor (linealias pline) (list (linealias matched-support) (linealias right-line))))
	     ;; remove right-line and left-line from supports of all other
	     ;; planned lines -- it might not yet be admissible
	     (dolist (pl otherplans)
	       (comdecode (list 'unsponsor (linealias pl) (list (linealias right-line) (linealias left-line)))))
	     (setf (line-node right-line) right-node)
	     (setf (line-node left-line) left-node)	     
	     (setf (line-mating right-line) (line-mating pline))
	     (values (list pline right-line) msg 'succeed)))
	(let ((msg "Can't apply SUBST=R." ))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))



(deftactic subst=r-tac
  (etree-nat
   (lambda (pline)
     (subst=r-tac-etree-nat-fn pline))
   "If a support line is an equality, and the planned line follows from the
substituting the left-hand-side for the right-hand-side in some wff provable
from the other supports, applies Subst=R.  See Pfenning's theorem 141."))


;;;;;;;;;;;;;;;;;;

(defun neg-equal-match2 (support pline)
  (let ((node (line-node support)))
    (and (wffeq 'falsehood (line-assertion pline))
	 (not (mated-line support (line-mating pline))) ; cebrown 9/5/00
	 (auto::negation-p node)
	 (not-p (line-assertion support))
	 (equals-p (cdr (line-assertion support))))))


(deftactic neg-equal-sline-tac
  (etree-nat
   (lambda (pline)
     (neg-equal-sline-tac-etree-nat-fn pline))
   "If a support line is a negated equality and planned line is falsehood,
applies indirect proof.  Similar to Pfenning's tactic 217."))

(defun neg-equal-sline-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof)))) 
	 (matched-supports 
	  (remove-if-not #'(lambda (x) (neg-equal-match2 x pline))
			 supports))
	 (matched-support (car matched-supports))
	 (oldplans (remove pline (current-plan-lines dproof))))
    (when (eq tacmode 'interactive)
      (setq matched-support
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-EQUAL-SLINE to line ~D?"
					(linealias x)) t))
		     matched-supports)))
    (if matched-support
	(let ((msg "Applied NEG-EQUAL-SLINE."))
	  (tactic-output msg t)
	  (comdecode 
	    (list 'indirect2 (linealias pline)
		  (linealias matched-support) '$
			 '!))  
	  (let* ((newplan
		  (car (set-difference (current-plan-lines dproof)
				       oldplans)))
		 (new-support
		  (car (set-difference (cdr (assoc newplan 
						   (proof-plans dproof)))
				       supports))))
	    (update-plan `((pp ,new-support ss)) '((pp ss)))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node matched-support))))
	    (setf (line-mating newplan)
		  (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-supports
		       "Not applying NEG-EQUAL-SLINE."
		       "Can't apply NEG-EQUAL-SLINE.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))
