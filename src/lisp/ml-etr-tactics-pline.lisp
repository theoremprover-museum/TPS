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
(context etr-nat)

;;; Written by Dan Nesmith


(deffile ml-etr-tactics-pline
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Defines planned line tactics as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs."))

(defun mated-line (line mating)
  (let ((node (line-node line))
	(litnames (append (mapcar #'car mating)
			  (mapcar #'cdr mating))))
    (and (not (get line 'duplicate))
	 (member (etree-name node) litnames))))

(context prop-tactics)

(deftactic deduct-tac
 (etree-nat
   (lambda (pline)
     (deduct-tac-etree-nat-fn pline))
   "Applies deduction rule if planned line corresponds to an implication node.
Same as Pfenning's tactic 191."))

(defun deduct-match2 (pline)
  (and (deduct-match1 pline)
       (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
       (auto::implication-p (line-node pline))))

(defun deduct-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'deduct-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply DEDUCT to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (if matched-plan
	(let ((msg "Applied DEDUCT.")
	      (old-supports (cdr (assoc matched-plan (proof-plans dproof)))))
	  (tactic-output msg t)
	  (deduct-short pline)
	  (let* ((newplan (car (set-difference
				(current-plan-lines dproof)
				oldplans)))
		 (new-support (car (set-difference
				    (cdr (assoc newplan (proof-plans dproof)))
				    old-supports))))
				   
	    (setf (line-node new-support) 
		  (car (etree-components (line-node matched-plan))))
	    (setf (line-node newplan)
		  (cadr (etree-components (line-node matched-plan))))
	    (setf (line-mating newplan)
		  (line-mating matched-plan))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying DEDUCT."
		       "Can't apply DEDUCT.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))))


(deftactic iconj-tac
  (etree-nat
   (lambda (pline)
     (iconj-tac-etree-nat-fn pline))
   "Applies ICONJ if planned line corresponds to  a conjunction node.
Same as Pfenning's tactic 186."))

(defun iconj-match2 (pline)
  (and (iconj-match1 pline)
       (not (mated-line pline (line-mating pline)))
       (auto::econjunction-p (line-node pline))))

(defun iconj-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'iconj-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply ICONJ to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied ICONJ."))
	  (tactic-output msg t)
	  (iconj-short pline)
	  (let ((newplans (delete-if #'(lambda (x) (memq x oldplans))
			   (current-plan-lines dproof)))
		(left-child (car (auto::etree-components
				  (line-node pline))))
		(right-child (cadr (auto::etree-components 
				    (line-node pline)))))
	    ;; assume that ICONJ will put the lines in numerical order, left
	    ;; conjunct first
	    (if (< (linealias (car newplans))
		   (linealias (cadr newplans)))
		(progn (setf (line-node (car newplans)) left-child)
		       (setf (line-node (cadr newplans)) right-child))
	      (progn (setf (line-node (car newplans)) right-child)
		     (setf (line-node (cadr newplans)) left-child)))
	    (dolist (line newplans)
	      (setf (line-mating line) (line-mating pline)))
	    (values newplans msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying ICONJ."
		       "Can't apply ICONJ.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))



(deftactic iconj*-tac
  (etree-nat
   (lambda (pline)
     (iconj*-tac-etree-nat-fn pline))
   "If planned line corresponds to a conjunction node, splits into
subgoals.  Will break up a multiple conjunction into separate conjuncts."
))

(defun iconj*-match2 (pline)
  (and (iconj-match1 pline)
       (not (mated-line pline (line-mating pline)))
       (auto::econjunction-p (line-node pline))))

(defun iconj*-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'iconj*-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply ICONJ* to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (node (if matched-plan (line-node matched-plan)))
	 (node-wff-list
	  (if matched-plan
	      (find-all-conjunct-nodes
	       node (line-assertion matched-plan))))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(progn (make-room-before matched-plan (length node-wff-list))
	(let* ((msg "Applied ICONJ*.")
	       (space-available (- (linealias matched-plan) (find-room-before matched-plan)))
	       (line-jump-1 (truncate (/ space-available (1+ (length node-wff-list)))))
	       (line-jump (if (= 0 line-jump-1) 1 line-jump-1))
	       (newplans nil))
	  (tactic-output msg t)
	  (setq newplans
		(do ((node-wff-list node-wff-list (cdr node-wff-list))
		     (line (- (linealias matched-plan)
			      (* (length node-wff-list) line-jump))
			   (+ line line-jump))
		     (newplans nil (cons newplan newplans))
		     (oldplans oldplans (cons newplan oldplans))
		     (newplan nil))
		    ((null node-wff-list) newplans)
		  (comdecode 
			   (list 'lemma (linealias pline) line
				 (list 'quote (cdar node-wff-list)) '$
				 '$ '$))
		  (setq newplan
			(car (set-difference 
			      (current-plan-lines dproof)
			      oldplans)))
		  (setf (line-node newplan) (caar node-wff-list))))
	  (comdecode
		   (list 'rulep (linealias matched-plan)
				(mapcar #'linealias newplans)))
	  (dolist (line newplans)
	    (setf (line-mating line) (line-mating pline)))
	  (values newplans msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying ICONJ*."
		       "Can't apply ICONJ*.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))


(defun find-all-conjunct-nodes (node wff)
  (if (auto::econjunction-p node)
      (nconc 
       (find-all-conjunct-nodes (car (auto::etree-components node))
				(glr wff))
       (find-all-conjunct-nodes (cadr (auto::etree-components node))
				(grr wff)))
      (list (cons node wff))))




(context aux-tactics)
(deftactic iness-pline-tac
  (etree-nat
   (lambda (pline)
     (iness-pline-etree-nat-fn pline))
   "If planned line is not FALSEHOOD and it is inessential, applies
absurdity rule.  Same as Pfenning's tactic 224."))


(defun iness-pline-match2 (pline supports)
  (and (not (wffeq (line-assertion pline) 'FALSEHOOD))
       (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
       (auto::spans 
	(auto::make-false :positive NIL :junctive 'CON)
	(mapcar #'(lambda (x) (line-node x)) supports)
	(line-mating pline))))

(defun iness-pline-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan
	  (find-if #'(lambda (x) 
		       (iness-pline-match2 
			 x (cdr (assoc x (proof-plans dproof)))))
		   (list pline)))
	 (oldplans (delete pline (current-plan-lines dproof)))
	 (matched-plan 
	  (if (and (eq tacmode 'interactive) matched-plan)
	      (find-if #'(lambda (x) (query (format nil  "Apply INESS-PLINE to line ~D?" (linealias x)) t))
		       (list matched-plan))
	    matched-plan)))
    (if matched-plan
	(let ((msg "Applied INESS-PLINE."))
	  (tactic-output msg t)
	  (absurd-short pline)
	  (let* ((newplan (car 
			   (set-difference 
			    (current-plan-lines dproof)
			    oldplans))))
	    (setf (line-mating newplan) (line-mating pline))
	    (setf (line-node newplan)	; fixed a bug - cebrown 5/22/02
	      (auto::make-false :positive nil :junctive 'CON))
	    (values (list newplan) msg 'succeed)))
	(let ((msg "Can't apply INESS-PLINE."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(context prop-tactics)

(deftactic class-disj-tac
  (etree-nat
   (lambda (pline)
     (class-disj-etree-nat-fn pline))
   "If planned line corresponds to a disjunction, and both of the disjuncts
are essential, applies indirect proof.  Same as Pfenning's tactic 229."))

(defun class-disj-match2 (pline other-supps)
  (and (auto::edisjunction-p (line-node pline))
       (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
       (let* ((disj-node (line-node pline))
	      (left-node (car (etree-components disj-node)))
	      (right-node (cadr (etree-components disj-node)))
	      (other-nodes (mapcar #'(lambda (x) (line-node x)) other-supps))
	      (conn-list (line-mating pline))
	      (left-iness (auto::spans right-node other-nodes conn-list))
	      (right-iness (auto::spans left-node other-nodes conn-list)))
	 (and (not left-iness) (not right-iness)))))

(defun class-disj-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan
	  (find-if #'(lambda (x) 
		       (class-disj-match2 
			 x (cdr (assoc x (proof-plans dproof)))))
		   (list pline)))
	 (oldplans (delete pline (current-plan-lines dproof)))
	 (matched-plan 
	  (if (and (eq tacmode 'interactive) matched-plan)
	      (find-if #'(lambda (x) (query (format nil  "Apply CLASS-DISJ to line ~D?" (linealias x)) t))
		       (list matched-plan))
	    matched-plan)))
    (if matched-plan
	(let ((msg "Applied CLASS-DISJ.")
	      (old-supports (cdr (assoc pline (proof-plans dproof)))))
	  (tactic-output msg t)
	  (indirect-short pline)
	  (let* ((newplan (car 
			   (set-difference 
			    (current-plan-lines dproof)
			    oldplans)))
		 (new-support
		  (car (set-difference
			(cdr (assoc newplan (proof-plans dproof)))
			old-supports)))
		 (neg-node 
		  (auto::make-negation 
		   :junctive 'auto::neutral
		   :positive (not (auto::positive-p (line-node pline)))
		   :components (list (line-node pline)))))
	    (setf (line-node new-support) neg-node)
	    (setf (line-mating newplan) (line-mating pline))
	    (auto::update-status nil neg-node 1)
	    (setf (line-node newplan) ; simplified this code 5/22/02
	      (auto::make-false :positive nil :junctive 'CON))
	    (auto::update-status nil (line-node newplan) 1)
	    (values (list newplan) msg 'succeed)))
	(let ((msg "Can't apply CLASS-DISJ."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))


(deftactic idisj-left-tac
  (etree-nat
   (lambda (pline)
     (idisj-left-tac-etree-nat-fn pline))
   "If planned line corresponds to a disjunction, and the right disjunct
is inessential, infers the planned line from the left disjunct by RuleP.
Same as Pfenning's tactic 188."))

(defun idisj-left-match2 (pline supports)
  (and (auto::edisjunction-p (line-node pline))
       (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
       (let ((conn-list (line-mating pline))
	     (left-node (car (auto::etree-components (line-node pline))))
	     (supp-nodes (mapcar #'(lambda (x) (line-node x))
				 supports)))
	 (auto::spans left-node supp-nodes conn-list))))


(defun idisj-left-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans 
	   (remove-if-not 
			  #'(lambda (x) 
			      (idisj-left-match2 x 
				(cdr (assoc x (proof-plans dproof)))))
			  (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply IDISJ-LEFT to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied IDISJ-LEFT."))
	  (tactic-output msg t)
	  (comdecode
;		   (list 'lemma (linealias matched-plan) 
;			 (1- (linealias matched-plan)) 
;			 (list 'quote 
;			       (auto::get-shallow 
;				(car (auto::etree-components 
;				      (line-node matched-plan))))) '$
;					 '$ '$))
;the lines above were the old definition, before the new command IDISJ-LEFT was introduced.
	           (list 'idisj-left (linealias matched-plan) (1- (linealias matched-plan)) '!))
;	  (lemma-short pline (auto::get-shallow 
;			      (car (auto::etree-components 
;				    (line-node pline)))))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
;	    (rulep-enter pline (list newplan))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying IDISJ-LEFT."
		       "Can't apply IDISJ-LEFT.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(deftactic idisj-right-tac
  (etree-nat
   (lambda (pline)
     (idisj-right-tac-etree-nat-fn pline))
   "If the planned line corresponds to a disjunction and the left disjunct
is inessential, infers the planned line from the right disjunct by RuleP.
Same as Pfenning's tactic 189."))

(defun idisj-right-match2 (pline supports)
  (and (auto::edisjunction-p (line-node pline))
       (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
       (let ((conn-list (line-mating pline))
	     (right-node (cadr (auto::etree-components (line-node pline))))
	     (supp-nodes (mapcar #'(lambda (x) (line-node x))
				 supports)))
	 (auto::spans right-node supp-nodes conn-list))))

(defun idisj-right-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not 
			  #'(lambda (x) 
			      (idisj-right-match2 x 
				(cdr (assoc x (proof-plans dproof)))))
			  (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply IDISJ-RIGHT to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied IDISJ-RIGHT."))
	  (tactic-output msg t)
	  (comdecode
;		   (list 'lemma (linealias matched-plan) 
;			 (1- (linealias matched-plan)) 
;			 (list 'quote 
;			       (auto::get-shallow 
;				(cadr (auto::etree-components 
;				      (line-node matched-plan))))) '$
;					 '$ '$))
;the lines above were used before the introduction of IDISJ-RIGHT
	           (list 'idisj-right (linealias matched-plan)  (1- (linealias matched-plan)) '!))
;	  (lemma-short pline (auto::get-shallow 
;			      (cadr (auto::etree-components 
;				     (line-node pline)))))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
;	    (rulep-enter pline (list newplan))
	    (setf (line-node newplan)
		  (cadr (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying IDISJ-RIGHT."
		       "Can't apply IDISJ-RIGHT.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(context quant-tactics)
(deftactic ugen-tac
  (etree-nat
   (lambda (pline)
     (ugen-tac-etree-nat-fn pline))
   "If the planned line is a skolem or selection node, applies UGEN.
Same as Pfenning's tactic 194."))


(defun ugen-match2 (pline)
  (and (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00 
       (or (auto::skolem-p (line-node pline))
	   (auto::selection-p (line-node pline)))))

(defun ugen-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'ugen-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply UGEN to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let* ((msg "Applied UGEN.")
	      (plan-node (line-node matched-plan))
	      (new-node
	       (if (auto::skolem-p plan-node)
		   (auto::copy-skolem plan-node)
		   (auto::copy-selection plan-node)))
	      (newplan nil))
	  (tactic-output msg t)
	  ;; may require renaming of bound var first
	  (when (not (wffeq (bindvar (line-assertion matched-plan))
			    (car (auto::sel-exp-terms plan-node))))
	    (comdecode
		   (list 'lemma (linealias matched-plan) 
			 (1- (linealias matched-plan)) 
			 (list 'quote 
			       (ab-change (line-assertion matched-plan)
					  (car 
					   (auto::sel-exp-terms plan-node)))
			       '$ '$ '$)))
	    (setq newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans)))
	    (comdecode (list 'ab* (linealias newplan) 
				       (linealias pline)
				       '$ '$ '$ '$))
	    (if (auto::skolem-p new-node)
		(setf (auto::skolem-shallow new-node)
		      (auto::get-shallow 
			(car (auto::etree-components 
			      (line-node matched-plan)))))
		(setf (auto::selection-shallow new-node)
		      (auto::get-shallow 
			(car (auto::etree-components 
			      (line-node matched-plan))))))
	    (setf (line-node newplan) new-node)
	    (setf (line-mating newplan) (line-mating pline))
	    (setq oldplans (current-plan-lines dproof))
	    (setq pline newplan))
	  (ugen-short pline)
	  (setq newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans)))
	  (setf (line-node newplan)
		(car (auto::etree-components (line-node pline))))
	  (setf (line-mating newplan) (line-mating pline))
	  (values (list newplan) msg 'succeed))
	(let ((msg (if matched-plans
		       "Not applying UGEN."
		       "Can't apply UGEN.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(deftactic egen-tac
  (etree-nat
   (lambda (pline)
     (egen-tac-etree-nat-fn pline))
   "If the planned line corresponds to a expansion node with a single
admissible expansion term, applies EGEN using that term.  Same as Pfenning's
tactic 195."))


(defun egen-match2 (supports pline)
  (let ((node (line-node pline)))
    (and (egen-match1 pline)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (auto::expansion-p node)
	 (= (length (auto::expansion-terms node)) 1)
	 (auto::admissible-p (mapcar #'(lambda (x) (line-node x)) supports) 
		       (car (auto::expansion-terms node))))))

(defun egen-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-plans (remove-if-not 
			 #'(lambda (x) (egen-match2 supports x))
			 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply EGEN to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (if matched-plan
	(let ((msg "Applied EGEN."))
	  (tactic-output msg t)
	  (egen-short pline (car (auto::expansion-terms (line-node pline))))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying EGEN."
		       "Can't apply EGEN.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(context prop-tactics)

(deftactic implics-equiv-tac
  (etree-nat
   (lambda (pline)
     (implics-equiv-tac-etree-nat-fn pline))
   "If the planned line corresponds to a rewrite node with justification
equiv-implics, applies implics-equiv rule."))

(defun implics-equiv-match2 (pline)
  (let ((node (line-node pline)))
    (and ;(implics-equiv-match1 pline)
     (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (auto::rewrite-p node)
	 (eq (auto::rewrite-justification node) 
	     'auto::equiv-implics))))


(defun implics-equiv-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'implics-equiv-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply IMPLICS-EQUIV to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied IMPLICS-EQUIV."))
	  (tactic-output msg t)
	  (implics-equiv-short pline)
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying IMPLICS-EQUIV."
		       "Can't apply IMPLICS-EQUIV.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

					; cebrown, 9/5/00
(defun disj-equiv-match2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline)))
	 (eq (auto::rewrite-justification node) 
	     'auto::equiv-disjs))))

(deftactic disj-equiv-tac
  (etree-nat
   (lambda (pline)
     (let* ((auto::*ignore-statuses* t)
	    (matched-plan
	     (find-if #'disj-equiv-match2 (list pline)))  ; cebrown 9/5/00, changed equiv-disj-match2 to disj-equiv-match2, written above
	    (oldplans (delete pline (current-plan-lines dproof)))
	    (matched-plan 
	     (if (and (eq tacmode 'interactive) matched-plan)
		 (find-if #'(lambda (x) (query (format nil  "Apply DISJ-EQUIV to line ~D?" (linealias x)) t))
			  (list matched-plan))
	       matched-plan)))
       (declare (ignore oldplans))
       (if matched-plan
	   (let ((msg "Applied DISJ-EQUIV."))
	     (tactic-output msg t)
	     (comdecode
		      (list 'lemma (linealias pline) (1- (linealias pline)) 
			    (list 'quote 
				  (auto::get-shallow 
				   (car (auto::etree-components 
					 (line-node matched-plan))))) '$
					 '$ '$))
	     (let ((newplan (numalias (1- (linealias pline)))))
	       (rulep-enter matched-plan (list newplan))
	       (setf (line-just-rule matched-plan) "EquivConj") ; cebrown 9/9/01
	       (setf (line-node newplan)
		     (car (auto::etree-components (line-node matched-plan))))
	       (setf (line-mating newplan)
		     (line-mating matched-plan))
	       (values (list newplan) msg 'succeed)))
	   (let ((msg "Can't apply DISJ-EQUIV."))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))))

(context lambda-tactics)

(deftactic lexpd*-beta-tac
  (etree-nat
   (lambda (pline)
     (lexpd*-beta-tac-etree-nat-fn pline))
   "If the planned line corresponds to a rewrite node with justification
beta, applies lexpd*-beta rule."))

(deftactic lexpd*-tac
  (etree-nat
   (lambda (pline)
     (lexpd*-tac-etree-nat-fn pline))
   "If the planned line corresponds to a rewrite node with justification
lambda, applies lexpd* rule."))

(deftactic lexpd*-eta-tac
  (etree-nat
   (lambda (pline)
     (lexpd*-eta-tac-etree-nat-fn pline))
   "If the planned line corresponds to a rewrite node with justification
eta, applies lexpd*-eta rule."))

(defun lambda-match2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (eq (auto::rewrite-justification node) 
	     'lambda))))

(defun lambda-match-eta2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (eq (auto::rewrite-justification node) 'eta))))

(defun lambda-match-beta2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (eq (auto::rewrite-justification node) 'beta))))

(defun lexpd*-beta-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'lambda-match-beta2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply LEXPD*-BETA to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (if matched-plan
	(let ((msg "Applied LEXPD*-BETA."))
	  (tactic-output msg t)
;	  (lexpd*-short pline)
	  (comdecode
	   (list 'lemma (linealias matched-plan) 
		 (1- (linealias matched-plan)) 
		 (list 'quote 
		       (auto::get-shallow 
			(car (auto::etree-components 
			      (line-node matched-plan))))) '$
			      '$ '$))
	  (comdecode (list 'lexpd*-beta (linealias matched-plan)
			   (1- (linealias matched-plan)) '$ '$ '$ '$))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying LEXPD*-BETA."
		       "Can't apply LEXPD*-BETA.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(defun lexpd*-eta-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'lambda-match-eta2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply LEXPD*-ETA to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (if matched-plan
	(let ((msg "Applied LEXPD*-ETA."))
	  (tactic-output msg t)
;	  (lexpd*-short pline)
	  (comdecode
	   (list 'lemma (linealias matched-plan) 
		 (1- (linealias matched-plan)) 
		 (list 'quote 
		       (auto::get-shallow 
			(car (auto::etree-components 
			      (line-node matched-plan))))) '$
			      '$ '$))
	  (comdecode (list 'lexpd*-eta (linealias matched-plan)
			   (1- (linealias matched-plan)) '$ '$ '$ '$))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying LEXPD*-ETA."
		       "Can't apply LEXPD*-ETA.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(defun lexpd*-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'lambda-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply LEXPD* to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (delete pline (current-plan-lines dproof))))
    (if matched-plan
	(let ((msg "Applied LEXPD*."))
	  (tactic-output msg t)
;	  (lexpd*-short pline)
	  (comdecode
	   (list 'lemma (linealias matched-plan) 
		 (1- (linealias matched-plan)) 
		 (list 'quote 
		       (auto::get-shallow 
			(car (auto::etree-components 
			      (line-node matched-plan))))) '$
			      '$ '$))
	  (comdecode (list 'lexpd* (linealias matched-plan)
			   (1- (linealias matched-plan)) '$ '$ '$ '$))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying LEXPD*."
		       "Can't apply LEXPD*.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))


(context quant-tactics)

(deftactic ab-plan-tac
  (etree-nat
   (lambda (pline)
     (ab-plan-tac-etree-nat-fn pline))
   "If the planned line corresponds to a rewrite node with justification
ab, applies the ab* rule."))

(defun ab-match2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (eq (auto::rewrite-justification node) 
	     'auto::ab))))

(defun ab-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'ab-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply AB* to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied AB-PLAN."))
	  (tactic-output msg t)
	  (comdecode
		   (list 'lemma (linealias matched-plan) 
			 (1- (linealias matched-plan)) 
			 (list 'quote 
			       (auto::get-shallow 
				(car (auto::etree-components 
				      (line-node matched-plan))))) '$
					 '$ '$))
;	  (lemma-short pline (auto::get-shallow 
;			      (car (auto::etree-components 
;				    (line-node pline)))))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (comdecode (list 'ab* (linealias newplan) 
				       (linealias pline)
				       '$ '$ '$ '$))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying AB-PLAN."
		       "Can't apply AB-PLAN.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))
(context defn-tactics)

(deftactic equiv-wffs-plan-tac
  (etree-nat
   (lambda (pline)
     (equiv-wffs-plan-tac-etree-nat-fn pline))
   "If the planned line corresponds to a rewrite node with justification
equivwffs (instantiated definitions), applies equiv-wffs rule."))

(defun equiv-wffs-match2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (member (auto::rewrite-justification node) 
		 '(auto::equivwffs dual)))))

(defun equiv-wffs-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'equiv-wffs-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply EQUIV-WFFS-PLAN to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied EQUIV-WFFS-PLAN."))
	  (tactic-output msg t)
	  (comdecode
		   (list 'lemma (linealias matched-plan) 
			 (1- (linealias matched-plan)) 
			 (list 'quote 
			       (auto::get-shallow 
				(car (auto::etree-components 
				      (line-node matched-plan))))) '$
					 '$ '$))
;	  (lemma-short pline (auto::get-shallow 
;			      (car (auto::etree-components 
;				    (line-node pline)))))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (comdecode (list 'equiv-wffs (linealias newplan) 
				       (linealias pline)
				       '$ '$ '$ '$))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying EQUIV-WFFS-PLAN."
		       "Can't apply EQUIV-WFFS-PLAN.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(context quant-tactics)

(deftactic ruleq-plan-tac
  (etree-nat
   (lambda (pline)
     (ruleq-plan-tac-etree-nat-fn pline))
   "If the planned line corresponds to a rewrite node with justification
ruleq (minimized quantifier scopes), justifies the line by ad hoc RuleQ,
and makes a new planned line with the rewritten wff."))

(defun ruleq-match2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (member (auto::rewrite-justification node) '(auto::ruleq auto::ruleq-univ)))))

(defun ruleq-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'ruleq-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply RULEQ-PLAN to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied RULEQ-PLAN."))
	  (tactic-output msg t)
	  (comdecode
		   (list 'lemma (linealias matched-plan) 
			 (1- (linealias matched-plan)) 
			 (list 'quote 
			       (auto::get-shallow 
				(car (auto::etree-components 
				      (line-node matched-plan)))))
			 '$ (mapcar #'linealias (line-hypotheses matched-plan))
			 '$))
;					 '$ '$))
;	  (lemma-short pline (auto::get-shallow 
;			      (car (auto::etree-components 
;				    (line-node pline)))))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (setf (line-justification pline)
		  (list "RuleQ" nil (list newplan)))
	    (update-plan (list (list pline 'SS)) (list (list newplan 'SS)))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying RULEQ-PLAN."
		       "Can't apply RULEQ-PLAN.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(context equality-tactics)

(deftactic equality-plan-tac
  (etree-nat
    (orelse ext=-plan-tac leibniz=-plan-tac)
    "If the planned line corresponds to rewrite node with justification
for a rewritten equality, justifies the line appropriately,
and makes a new planned line with the rewritten wff."))

(defun leibniz=-match2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (eq (auto::rewrite-justification node) 
	     'auto::leibniz=)
	 )))

(defun ext=-match2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (eq (auto::rewrite-justification node) 
	     'auto::ext=)
	 )))

(deftactic leibniz=-plan-tac
  (etree-nat
   (lambda (pline)
     (leibniz=-plan-tac-etree-nat-fn pline))
   "If the planned line corresponds to rewrite node with justification
for a rewritten equality using the Leibniz definition, justifies the line 
appropriately, and makes a new planned line with the rewritten wff."))


(defun leibniz=-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'leibniz=-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply LEIBNIZ=-PLAN to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied LEIBNIZ=-PLAN."))
	  (tactic-output msg t)
	  (comdecode
		   (list 'lemma (linealias matched-plan) 
			 (1- (linealias matched-plan)) 
			 (list 'quote 
			       (auto::get-shallow 
				(car (auto::etree-components 
				      (line-node matched-plan))))) '$
					 '$ '$))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (setf (line-justification pline)
		  (list "Equality" nil (list newplan)))
	    (update-plan (list (list pline 'SS)) (list (list newplan 'SS)))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying LEIBNIZ=-PLAN."
		       "Can't apply LEIBNIZ=-PLAN.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(deftactic ext=-plan-tac
  (etree-nat
   (lambda (pline)
     (ext=-plan-tac-etree-nat-fn pline))
   "If the planned line corresponds to rewrite node with justification
for a rewritten equality using extensionality, justifies the line 
appropriately, and makes a new planned line with the rewritten wff."))


(defun ext=-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'ext=-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply EXT=-PLAN to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied EXT=-PLAN."))
	  (tactic-output msg t)
	  (comdecode
	    (list 'lemma (linealias matched-plan) 
		  (1- (linealias matched-plan)) 
		  (list 'quote 
			(auto::get-shallow 
			  (car (auto::etree-components 
				 (line-node matched-plan))))) '$
		  '$ '$))
	  (let ((newplan (car (set-difference 
			       (current-plan-lines dproof)
			       oldplans))))
	    (comdecode (list (if (equiv-p (line-assertion newplan))
				 'ext=0
				 'ext=)
			     (linealias pline)
			     (linealias newplan) '!))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying EXT=-PLAN."
		       "Can't apply EXT=-PLAN.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(context prop-tactics)
(deftactic truth-tac
  (etree-nat
   (lambda (pline)
     (truth-tac-etree-nat-fn pline))
   "Applies ITruth if the planned line is TRUTH."))


(defun truth-match2 (pline)
  (or (wffeq 'truth (line-assertion pline))
      (auto::true-p (line-node pline))))

(defun truth-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'truth-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply TRUTH to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans))))
    (if matched-plan
	(let ((msg "Applied TRUTH."))
	  (tactic-output msg t)
	  (itruth-short pline)
	  (values nil msg 'succeed))
	(let ((msg (if matched-plans
		       "Not applying TRUTH."
		       "Can't apply TRUTH.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(deftactic ineg-tac
  (etree-nat
   (lambda (pline)
     (ineg-tac-etree-nat-fn pline))
   "Applies INEG if planned line is a negation."))

(defun ineg-match2 (pline)
  (let ((node (line-node pline)))
    (and (ineg-match1 pline)
	 (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
	 (or (auto::negation-p node)
	     (auto::expansion-p node)
	     (auto::skolem-p node)
	     (auto::selection-p node)
	     (and (auto::rewrite-p node)
		  (eq 'auto::subst= (auto::rewrite-justification node)))))))


(defun ineg-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (when (ineg-match2 pline) pline))
	 (oldplans (delete pline (current-plan-lines dproof)))
	 (matched-plan 
	  (if (and (eq tacmode 'interactive) matched-plan)
	      (find-if #'(lambda (x) (query (format nil  "Apply INEG to line ~D?" (linealias x)) t))
		       (list matched-plan))
	    matched-plan)))
    (if matched-plan
	(let* ((msg "Applied INEG.")
	       (old-supports (cdr (assoc pline (proof-plans dproof))))
	       (newplan nil)
	       (new-support nil)
	       (node (line-node pline)))
	  (tactic-output msg t)
	  (ineg-short pline)
	  (setq newplan 
		(car 
		  (set-difference 
		    (current-plan-lines dproof)
		    oldplans)))
	  (setq new-support
		(car (set-difference
		       (cdr (assoc newplan (proof-plans dproof)))
		       old-supports)))
	  (setf (line-node new-support) 
		(if (auto::negation-p node)
		    (car (auto::etree-components node))
		   node))
	  (setf (line-mating newplan) (line-mating pline))
	  (setf (line-node newplan)  ; simplifed this cebrown 5/22/02
	    (auto::make-false :positive nil :junctive 'CON))
	  (values (list newplan) msg 'succeed))
	(let ((msg "Can't apply INEG."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))



(deftactic indirect-tac
  (etree-nat
   (lambda (pline)
     (indirect-tac-etree-nat-fn pline))
   "Applies indirect proof.  This can almost always be applied when the
planned line is not FALSEHOOD.  It does not apply if the planned line
corresponds to a mated node and one of the support line corresponds
to the negation of that node."))

(defun indirect-match2 (pline)
  (if (wffeq (line-assertion pline) 'falsehood)
      nil
    (let ((pn (line-node pline)))
      (not (find-if #'(lambda (supp)
			(let ((n (line-node supp)))
			  (and (negation-p n)
			       (eq (car (etree-components n)) pn))))
		    (cdr (assoc pline (proof-plans dproof))))))))

(defun indirect-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (when (indirect-match2 pline) pline))
	 (oldplans (delete pline (current-plan-lines dproof)))
	 (matched-plan 
	  (if (and (eq tacmode 'interactive) matched-plan)
	      (find-if #'(lambda (x) (query (format nil  "Apply INDIRECT to line ~D?" (linealias x)) t))
		       (list matched-plan))
	    matched-plan)))
    (if matched-plan
	(let ((msg "Applied INDIRECT.")
	      (old-supports (cdr (assoc pline (proof-plans dproof)))))
	  (tactic-output msg t)
	  (indirect-short pline)
	  (let* ((newplan (car 
			   (set-difference 
			    (current-plan-lines dproof)
			    oldplans)))
		 (new-support
		  (car (set-difference
			(cdr (assoc newplan (proof-plans dproof)))
			old-supports)))
		 (neg-node 
		  (auto::make-negation 
		   :junctive 'auto::neutral
		   :positive (not (auto::positive-p (line-node pline)))
		   :components (list (line-node pline)))))
	    (setf (line-node new-support) 
		  neg-node)
	    (setf (line-mating newplan) (line-mating pline))
	    (setf (line-node newplan) ; simplified this - cebrown 5/22/02
	      (auto::make-false :positive nil :junctive 'CON))
	    (values (list newplan) msg 'succeed)))
	(let ((msg "Can't apply INDIRECT."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))

(deftactic neg-neg-plan-tac
  (etree-nat
   (lambda (pline)
     (neg-neg-plan-tac-etree-nat-fn pline))
   "If planned line is a double negation, applies the pullneg
rule."))

(defun neg-neg-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (if (and (neg-neg-match2 pline)
				(not (mated-line pline (line-mating pline)))) ; cebrown 9/5/00
			   pline))
	 (oldplans (remove pline (current-plan-lines dproof))))
    (when (and matched-plan (eq tacmode 'interactive))
      (setq matched-plan
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-NEG-PLAN to line ~D?"
					(linealias x)) t))
		     (list pline))))
    (if matched-plan
	(let ((msg "Applied NEG-NEG-PLAN."))
	  (tactic-output msg t)
	  (pullneg-short matched-plan)
	  (let ((newplan
		 (car (set-difference 
		       (current-plan-lines dproof)
		       oldplans))))
	    (setf (line-node newplan)
		  (car (auto::etree-components
			(car (auto::etree-components
			      (line-node matched-plan))))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) "Applied NEG-NEG-PLAN." 'succeed)))
	(let ((msg "Can't apply NEG-NEG-PLAN."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(deftactic neg-or-plan-tac
  (etree-nat
   (lambda (pline)
     (neg-or-plan-tac-etree-nat-fn pline))
   "If planned line is a negated disjunction, applies pullneg rule."))

(defun neg-or-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (if (and (neg-or-match2 pline)
				(not (mated-line pline (line-mating pline)))) ; cebrown 9/5/00
			   pline))
	 (oldplans (remove pline (current-plan-lines dproof))))
    (when (and matched-plan (eq tacmode 'interactive))
      (setq matched-plan
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-OR-PLAN to line ~D?"
					(linealias x)) t))
		     (list pline))))
    (if matched-plan
	(let ((msg "Applied NEG-OR-PLAN."))
	  (tactic-output msg t)
	  (pullneg-short matched-plan)
	  (let* ((newplan
		  (car
		   (set-difference 
		    (current-plan-lines dproof)
		    oldplans)))
		 (neg-node (line-node matched-plan))
		 (disj-node (car (auto::etree-components 
				  neg-node)))
		 (conj-node
		  (auto::make-econjunction
		   :positive (auto::positive-p neg-node)
		   :free-vars (auto::etree-free-vars neg-node)
		   :junctive (auto::etree-junctive disj-node)))
		 (left-node (auto::copy-negation neg-node))
		 (right-node (auto::copy-negation neg-node))
		 (juncts (auto::etree-components disj-node)))
	    (if (auto::negation-p (cadr juncts))		   
		(setq right-node
		      (car (auto::etree-components (cadr juncts))))
		(setf (auto::etree-components right-node)
		      (list (cadr juncts))))
	    (if (auto::negation-p (car juncts))		   
		(setq left-node
		      (car (auto::etree-components (car juncts))))
		(setf (auto::etree-components left-node)
		      (list (car juncts))))
	    (setf (auto::etree-components conj-node)
		  (list left-node right-node))
	    (setf (line-node newplan) conj-node)
	    (setf (line-mating newplan) (line-mating matched-plan))
	    (values (list newplan) "Applied NEG-OR-PLAN." 'succeed)))
	(let ((msg "Can't apply NEG-OR-PLAN."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(deftactic neg-and-plan-tac
  (etree-nat
   (lambda (pline)
     (neg-and-plan-tac-etree-nat-fn pline))
   "If planned line is a negated conjunction, applies indirect proof,
assuming negated planned line with new goal of falsehood."))

(defun neg-and-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (if (and (neg-and-match2 pline)
				(not (mated-line pline (line-mating pline)))) ; cebrown 9/5/00
			   pline))
	 (oldplans (remove pline (current-plan-lines dproof)))
	 (supports (cdr (assoc pline (proof-plans dproof)))))
    (when (and matched-plan (eq tacmode 'interactive))
      (setq matched-plan
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-AND-PLAN to line ~D?"
					(linealias x)) t))
		     (list pline))))
    (if matched-plan
	(let ((msg "Applied NEG-AND-PLAN."))
	  (tactic-output msg t)
	  (indirect-short matched-plan)
	  (let* ((newplan
		  (car (set-difference (current-plan-lines dproof)
				       oldplans)))
		 (new-support
		  (car (set-difference (cdr (assoc newplan 
						   (proof-plans dproof)))
				       supports)))
		 (new-node (auto::copy-negation (line-node matched-plan))))
	    (setf (auto::etree-components new-node)
		  (list (line-node matched-plan)))
	    (setf (auto::etree-positive new-node)
		  (not (auto::etree-positive new-node)))
	    (setf (line-node new-support) new-node)
	    (setf (line-node newplan) ; simplified - cebrown 5/22/02
	      (auto::make-false :positive nil :junctive 'CON))
	    (setf (line-mating newplan)
		  (line-mating pline))
	    (values (list newplan) "Applied NEG-AND-PLAN." 'succeed)))
	(let ((msg "Can't apply NEG-AND-PLAN."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(deftactic neg-imp-plan-tac
  (etree-nat
   (lambda (pline)
     (neg-imp-plan-tac-etree-nat-fn pline))
   "If planned is a negated implication, applies pullneg rule."))

(defun neg-imp-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (if (and (neg-imp-match2 pline)
				(not (mated-line pline (line-mating pline)))) ; cebrown 9/5/00
				pline))
	 (oldplans (remove pline (current-plan-lines dproof))))
    (when (and matched-plan (eq tacmode 'interactive))
      (setq matched-plan
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-IMP-PLAN to line ~D?"
					(linealias x)) t))
		     (list pline))))
    (if matched-plan
	(let ((msg "Applied NEG-IMP-PLAN."))
	  (tactic-output msg t)
	  (pullneg-short matched-plan)
	  (let* ((newplan
		  (car
		   (set-difference 
		    (current-plan-lines dproof)
		    oldplans)))
		 (neg-node (line-node matched-plan))
		 (disj-node (car (auto::etree-components 
				  neg-node)))
		 (conj-node
		  (auto::make-econjunction
		   :positive (auto::positive-p neg-node)
		   :free-vars (auto::etree-free-vars neg-node)
		   :junctive (auto::etree-junctive disj-node)))
		 (right-node (auto::copy-negation neg-node))
		 (juncts (auto::etree-components disj-node))
		 (left-node (car juncts)))
	    (if (auto::negation-p (cadr juncts))		   
		(setq right-node
		      (car (auto::etree-components (cadr juncts))))
		(setf (auto::etree-components right-node)
		      (list (cadr juncts))))
	    (setf (auto::etree-components conj-node)
		  (list left-node right-node))
	    (setf (line-node newplan) conj-node)
	    (setf (line-mating newplan) (line-mating matched-plan))
	    (values (list newplan) "Applied NEG-IMP-PLAN." 'succeed)))
	(let ((msg "Can't apply NEG-IMP-PLAN."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))

(context quant-tactics)

(deftactic neg-sel-plan-tac
  (etree-nat
   (lambda (pline)
     (neg-sel-plan-tac-etree-nat-fn pline))
   "If planned is a negated selection node, applies pullneg."))

(defun neg-sel-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (if (and (neg-sel-match2 pline)
				(not (mated-line pline (line-mating pline)))) ; cebrown 9/5/00
			   pline))
	 (oldplans (remove pline (current-plan-lines dproof))))
    (when (and matched-plan (eq tacmode 'interactive))
      (setq matched-plan
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-SEL-PLAN to line ~D?"
					(linealias x)) t))
		     (list pline))))
    (if matched-plan
	(let* ((msg "Applied NEG-SEL-PLAN.")
	      (neg-node (auto::copy-negation 
			 (line-node matched-plan)))
	      (sel-node 
	       (car (auto::etree-components neg-node))))
	  (setq sel-node (if (auto::skolem-p sel-node)
			     (auto::copy-skolem sel-node)
			     (auto::copy-selection sel-node)))
	  (setf (auto::etree-components neg-node)
		(auto::etree-components sel-node))
	  (setf (auto::etree-parent neg-node) sel-node)
	  (tactic-output msg t)
	  (pullneg-short matched-plan)
	  (let ((newplan
		 (car (set-difference 
		       (current-plan-lines dproof)
		       oldplans))))
	    (if (auto::negation-p
		 (car (auto::etree-components sel-node)))
		(setf (auto::etree-components sel-node)
		      (auto::etree-components
		       (car (auto::etree-components sel-node))))
		(setf (auto::etree-components sel-node)
		      (list neg-node)))
	    (setf (line-node newplan) sel-node)
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg "Can't apply NEG-SEL-PLAN."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))



(deftactic neg-exp-plan-tac
  (etree-nat
   (lambda (pline)
     (neg-exp-plan-tac-etree-nat-fn pline))
   "If planned line is a negated expansion node with only one expansion term,
applies pullneg rule."))

(defun neg-exp-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (if (and (neg-exp-match2 pline)
				(not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
				(= 1 (length
				      (auto::etree-components
					(car (auto::etree-components
					       (line-node pline)))))))
			   pline))
	 (oldplans (remove pline (current-plan-lines dproof))))
    (when (and matched-plan (eq tacmode 'interactive))
      (setq matched-plan
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-EXP-PLAN to line ~D?"
					(linealias x)) t))
		     (list pline))))
    (if matched-plan
	(let* ((msg "Applied NEG-EXP-PLAN.")
	       (neg-node (auto::copy-negation (line-node matched-plan)))
	       (exp-node (auto::copy-expansion
			  (car (auto::etree-components neg-node)))))
	  (setf (auto::etree-components neg-node)
		(auto::etree-components exp-node))
	  (setf (auto::etree-parent neg-node) exp-node)
	  (tactic-output msg t)
	  (pullneg-short matched-plan)
	  (let ((newplan
		 (car (set-difference 
		       (current-plan-lines dproof)
		       oldplans))))
	    (if (auto::negation-p (car (auto::etree-components exp-node)))
		(setf (auto::etree-components exp-node)
		      (auto::etree-components
		       (car (auto::etree-components exp-node))))
		(setf (auto::etree-components exp-node)
		      (list neg-node)))
	    (setf (line-node newplan) exp-node)
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg "Can't apply NEG-EXP-PLAN."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))

(context defn-tactics)

;(deftactic neg-rew-plan-tac
;  (etree-nat
;   (lambda (pline)
;     (neg-rew-plan-tac-etree-nat-fn pline))
;   "If planned line is a negated rewrite node, move the negation under
;the rewrite node."))
;
;(defun neg-rew-match2 (pline)
;  (and (auto::negation-p (line-node pline))
;       (auto::rewrite-p (car (auto::etree-components
;			      (line-node pline))))
;       (memq (auto::rewrite-justification
;	       (car (auto::etree-components
;			      (line-node pline))))
;	     '(lambda auto::ab auto::equivwffs auto::equality auto::ruleq))))
;
;(defun neg-rew-plan-tac-etree-nat-fn (pline)
;  (let* ((auto::*ignore-statuses* t)
;	 (matched-plan (if (neg-rew-match2 pline) pline)))
;    (when (and matched-plan (eq tacmode 'interactive))
;      (setq matched-plan
;	    (find-if #'(lambda (x)
;			 (query (format nil  "Apply NEG-REW-PLAN to line ~D?"
;					(linealias x)) t))
;		     (list pline))))
;    (if matched-plan
;	(let* ((msg "Applied NEG-REW-PLAN.")
;	       (save-node (line-node matched-plan))
;	       (neg-node (auto::copy-negation save-node))
;	       (rew-node (auto::copy-rewrite 
;			  (car (auto::etree-components neg-node)))))
;	  (setf (auto::etree-components neg-node)
;		(auto::etree-components rew-node))
;	  (setf (auto::etree-components rew-node)
;		(list neg-node))
;	  (setf (line-node matched-plan) rew-node)
;	  (tactic-output msg t)
;	  (values (list pline) msg 'succeed))
;	(let ((msg "Can't apply NEG-REW-PLAN."))
;	  (tactic-output msg nil)
;	  (values (list pline) msg 'fail)))
;    ))

;;; Above code was totally bogus.  Actually, we want to carry out the
;;; rewrite in the nat-ded proof, and update the expansion tree
;;; accordingly. DAN 24FEB90


(deftactic neg-rew-plan-tac
  (etree-nat
   (lambda (pline)
     (neg-rew-plan-tac-etree-nat-fn pline))
   "If planned line is a negated rewrite node, carry out the rewrite,
leaving the negation."))

(defun neg-rew-match2 (pline)
  (and (auto::negation-p (line-node pline))
       (not (mated-line pline (line-mating pline))) ; cebrown 9/5/00
       (auto::rewrite-p (car (auto::etree-components
			      (line-node pline))))))

(defun neg-rew-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan (if (neg-rew-match2 pline) pline)))
    (when (and matched-plan (eq tacmode 'interactive))
      (setq matched-plan
	    (find-if #'(lambda (x)
			 (query (format nil  "Apply NEG-REW-PLAN to line ~D?"
					(linealias x)) t))
		     (list pline))))
    (if matched-plan
	(let* ((msg "Applied NEG-REW-PLAN.")
	       (save-node (line-node matched-plan))
               (rew-node (car (etree-components save-node)))
	       (neg-node (auto::copy-negation save-node))
               (kid-node (car (etree-components rew-node)))
               (oldplans (mapcar #'car (proof-plans dproof)))
               (newplan nil)
               )
	  (setf (auto::etree-components neg-node)
		(auto::etree-components rew-node))
          (make-room-before matched-plan 1) 
          (apply #'lemma-enter 
                 (lemma-build (linealias pline)
                              (1- (linealias matched-plan))
                              (cons 'not (get-shallow kid-node))
                              (line-assertion pline)
                              (mapcar #'linealias (line-hypotheses pline))
                              (mapcar #'linealias 
                                      (line-hypotheses matched-plan))))
          (setq newplan
                (car (set-difference (mapcar #'car (proof-plans
                                                    dproof))
                                     oldplans)))
          (setf (line-just-rule matched-plan)
                (case (auto::rewrite-justification rew-node)
                  (auto::lambda "Lambda")
                  (auto::beta "Beta rule")
                  (auto::eta "Eta rule")
                  (auto::ab "AB")
                  ((auto::equiv-disjs auto::equiv-implics) "RuleP")
                  ((equivwffs auto::dual) "EquivWffs")
                  (ext= "Ext=")
                  (leibniz= "Equality")
                  (refl= "Refl=")
                  (else (line-just-rule newplan))))
          (setf (line-just-lines matched-plan)
                (list newplan))
          (sponsor newplan (cdr (assoc matched-plan (proof-plans dproof))))
          (setf (proof-plans dproof)
                (delete matched-plan (proof-plans dproof) :key #'car))
          (setf (line-node newplan) neg-node)
          (setf (line-mating newplan) (line-mating matched-plan))
	  (tactic-output msg t)
	  (values (list newplan) "Applied NEG-REW-PLAN." 'succeed))
	(let ((msg "Can't apply NEG-REW-PLAN."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))
    ))


(context prop-tactics)

(deftactic idisj-tac
  (etree-nat
   (orelse idisj-right-tac idisj-left-tac)))

(context compound-tactics)
(deftactic pline-tac
  (etree-nat
   (orelse deduct-tac iconj-tac idisj-right-tac idisj-left-tac
	   implics-equiv-tac lexpd*-vary-tac ab-plan-tac equiv-wffs-plan-tac 
	   equality-plan-tac ruleq-plan-tac ugen-tac egen-tac truth-tac )))

(deftactic rewrite-pline-p-tac
  (etree-nat
   (lambda (pline)
     (if (auto::rewrite-p (line-node pline))
	   (values (list pline) "Rewrite plan node found." 'succeed)
	   (values (list pline) "Rewrite plan node not found." 'fail)))
   "Returns success if planned line represents a rewrite node."))

(context aux-tactics)
(deftactic neg-pline-p-tac
  (etree-nat
   (lambda (pline)
     (if (auto::negation-p (line-node pline))
	   (values (list pline) "Negated plan node found." 'succeed)
	   (values (list pline) "Negated plan node not found." 'fail)))
   "Returns success if planned line represents a negation node."))

(deftactic restrict-mating-tac
  (etree-nat
   (lambda (pline)
     (let ((auto::*ignore-statuses* t)
	   (supports (cdr (assoc pline (proof-plans dproof)))))
       (setf (line-mating pline)
	     (auto::restrict-mating-to 
	      (line-mating pline)
	      (cons (line-node pline)
		    (mapcar #'(lambda (x) (line-node x)) supports))))
       (values (list pline) "Restricted mating." 'succeed)))
   "Restricts the mating of the planned line to only those connections
involving the line and its supports.  Always succeeds."))

