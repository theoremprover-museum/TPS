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

(deffile ml-etr-tactics-main
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Defines main tactics for translating expansion proofs to
natural deduction proofs."))

(context compound-tactics)

(deftactic rewrite-pline-tac
  (etree-nat
   (ifthen (rewrite-pline-p-tac)
      (orelse ab-plan-tac equality-plan-tac equiv-wffs-plan-tac
	      ruleq-plan-tac lexpd*-vary-tac implics-equiv-tac truthp-rewrite-plan-tac
	      disj-equiv-tac ; cebrown 9/9/01 (this tactic existed already, but wasn't being used at all)
	      )))
  (mhelp "If the planned line corresponds to a rewrite node, calls the appropriate
tactic."))

(deftactic rewrite-sline-tac
  (etree-nat
   (ifthen (rewrite-sline-p-tac)
     (orelse ab-sline-tac equality-sline-tac equiv-wffs-sline-tac
	     ruleq-sline-tac lcontr*-vary-tac equiv-disj-tac equiv-implics-tac)))
  (mhelp "If any support line corresponds to a rewrite node, calls the
appropriate tactic."))

(context prop-tactics)

(deftactic pullneg-tac
  (etree-nat
   (ifthen (neg-pline-p-tac)
     (orelse neg-neg-plan-tac neg-or-plan-tac neg-imp-plan-tac
	     neg-sel-plan-tac neg-exp-plan-tac neg-rew-plan-tac )))
  (mhelp "If planned line is a negated formula, calls the appropriate tactic
for pulling the negation out."))

(deftactic pushneg-tac
  (etree-nat
   (ifthen (neg-sline-p-tac)
     (orelse neg-neg-sline-tac neg-or-sline-tac neg-imp-sline-tac
	     neg-sel-sline-tac neg-exp-sline-tac neg-rew-sline-tac
	     )))
  (mhelp "If any support line is a negated formula, calls the appropriate tactic
for pushing the negation in."))

(context compound-tactics)

(deftactic complete-transform-tac
  (etree-nat
   (repeat complete-transform*-tac)))


;(deftactic complete-transform*-tac
;  (etree-nat
;    (orelse show-plans
;	    show-current-plan
;	    same-tac
;	    (ifthen use-rulep-tac rulep-tac)
;	    (make-room :use nat-ded)
;	    iness-pline-tac
;	    (then** (ifthen use-rulep-tac econj*-tac econj-tac)
;		    unsponsor-tac)
;	    deduct-tac
;	    rewrite-sline-tac
;	    rewrite-pline-tac 
;	    rulec-tac
;	    ugen-tac
;	    egen-tac
;	    (then** ui-tac
;		    unsponsor-tac)
;	    (then** unnec-exp-tac 
;		    (sequence unsponsor-tac restrict-mating-tac))
;	    (then** idisj-tac 
;		    (sequence unsponsor-tac restrict-mating-tac))
;	    (then** (ifthen use-rulep-tac iconj*-tac iconj-tac)
;		    unsponsor-tac)
;	    (then** cases-tac 
;		    (sequence unsponsor-tac restrict-mating-tac))
;	    pushneg-tac
;	    pullneg-tac
;	    mp-tac
;	    class-disj-tac
;	    truth-tac
;	    indirect2-tac
;	    indirect-tac
;	    neg-and-sline-tac neg-and-plan-tac)))

;;; Added equality tactics DAN 29OCT88

(deftactic complete-transform*-tac
  (etree-nat
    (orelse ;show-plans
	    ;show-current-plan
            (call print-routines)
	    same-tac
	    nnf-tac ; cebrown 2/17/01
	    absurd-tac
	    truth-tac
	    refl=-tac
	    (ifthen use-rulep-tac rulep-tac)
	    (make-room :use nat-ded)
	    duplicate-support-tac ; cebrown 9/5/00
	    (then** (ifthen use-rulep-tac econj*-tac econj-tac)
		    unsponsor-tac)
	    deduct-tac
	    rewrite-sline-tac
	    rewrite-pline-tac 
	    rulec-tac
	    ugen-tac
	    egen-tac
	    (then** ui-tac
		    unsponsor-tac)
	    (then** unnec-exp-tac 
		    unsponsor-tac )
	    (then** idisj-tac 
		    unsponsor-tac)
	    (then** (ifthen use-rulep-tac iconj*-tac iconj-tac)
		    unsponsor-tac)
	    (then** cases-tac 
		    unsponsor-tac )
	    pushneg-tac
	    neg-equiv-sline-tac ; cebrown 9/9/01
;	    pullneg-tac
	    neg-neg-plan-tac
	    ineg-tac 
	    subst=-tac
;	    subst=l-tac
;	    subst=r-tac
	    mp-tac
	    class-disj-tac
	    indirect2-tac
	    iness-pline-tac
	    neg-and-sline-tac neg-and-plan-tac neg-equal-sline-tac
	    indirect-tac
)))

;;; Added 02NOV88 DAN

(deftactic pfenning*-tac
  (etree-nat
    (orelse ;show-plans
	    ;show-current-plan
            (call print-routines)
	    same-tac
	    nnf-tac ; cebrown 2/17/01
	    absurd-tac
	    truth-tac
	    refl=-tac
	    (make-room :use nat-ded)
	    duplicate-support-tac ; cebrown 9/5/00
	    (then** econj-tac
		    unsponsor-tac)
	    deduct-tac
	    rewrite-sline-tac
	    rewrite-pline-tac 
	    rulec-tac
	    ugen-tac
	    egen-tac
	    (then** ui-tac
		    unsponsor-tac)
	    (then** unnec-exp-tac 
		    unsponsor-tac)
	    (then** idisj-tac 
		    unsponsor-tac )
	    (then** iconj-tac
		    unsponsor-tac)
	    (then** cases-tac 
		    unsponsor-tac )
	    ineg-tac
	    eneg-tac
	    subst=l-tac
	    subst=r-tac
	    mp-tac
	    (ifthen use-symsimp-tac symsimp-tac class-disj-tac) ; added 3APR89
	    iness-pline-tac
	    indirect-tac
            neg-rew-sline-tac ; added 24FEB90 DAN
	    neg-equiv-sline-tac ; cebrown 9/9/01
)
    "Intended to be the same as the tactics advocated in Pfenning's thesis."))

(deftactic pfenning-tac
  (etree-nat
    (repeat pfenning*-tac)
    "Intended to be the same as the tactics advocated in Pfenning's thesis."))

(context etr-nat)

(context prop-tactics)

					; cebrown 2/18/01 - wrote basic-prop-tac to build propositional proofs without using any 'fancy' rules
					; like RuleP, Assoc, etc.
					; It's basically a subset of pfenning*-tac using only basic propositional rules
(deftactic basic-prop*-tac
    (etree-nat
     (orelse ; (call print-routines)
	     same-tac
	     absurd-tac
	     truth-tac
	     neg-atom-elim-tac
	     (make-room :use nat-ded)
	     duplicate-support-tac
	     (then** econj-tac
		     unsponsor-tac)
	     deduct-tac
	     (then** idisj-tac unsponsor-tac)
	     (then** iconj-tac unsponsor-tac)
	     (then** cases-tac unsponsor-tac)
	     ineg-tac
	     mp-tac
	     implics-equiv-tac
	     equiv-implics-tac
	     neg-equiv-sline-tac ; cebrown 9/9/01
	     class-disj-tac
	     neg-neg-elim-tac neg-and-elim-tac
	     neg-imp-elim-tac neg-or-elim-simple-tac neg-or-elim-dup-tac
	     iness-pline-tac
	     indirect-tac)
     "Similar to a subset of Pfenning*-tac using only basic propositional rules, avoiding rules such as RuleP"))

(deftactic basic-prop-tac
    (etree-nat 
     (repeat basic-prop*-tac)
     "Similar to a subset of Pfenning*-tac using only basic propositional rules, avoiding rules such as RuleP"))

(deftactic rulep-tac
  (etree-nat
   (lambda (pline)
     (rulep-tac-etree-nat-fn pline))
   "Applies RuleP if possible."))

(defun remove-conns-unless-eqab (conn-list enodes)
  (let ((new-conn-list nil))
    (dolist (conn conn-list new-conn-list)
      (let ((node1 (dolist (node enodes)
		     (let ((n (auto::find-etree-node #'(lambda (x)
							 (eq (etree-name x) (car conn)))
						     node)))
		       (when n
			 (return n)))))
	    (node2 (dolist (node enodes)
		     (let ((n (auto::find-etree-node #'(lambda (x)
							 (eq (etree-name x) (cdr conn)))
						     node)))
		       (when n
			 (return n))))))
	(when (and node1 node2)
	  (when (wffeq-ab (get-shallow node1) (get-shallow node2))
	    (push conn new-conn-list)))))))


(defun rulep-match2 (pline)
  (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	 (conn-list (remove-conns-unless-eqab
		     (line-mating pline)
		     (cons (line-node pline)
			   (mapcar #'(lambda (x) (line-node x))
				   supports))))
	 (auto::*fps-succeeded* nil))	; FPS works up to NNF - which we don't want here
    (declare (special auto::*fps-succeeded*))
    (auto::spanning-clist-p
     (lines-to-rulep-jform pline supports
			   (connections-to-lits conn-list))
     conn-list)))

(defun rulep-tac-etree-nat-fn (pline)
  (if (rulep-match2 pline)
      (let ((supports (cdr (assoc pline (proof-plans dproof)))))
	(funcall #'comdecode
		 (list 'rulep (linealias pline)
		       (mapcar #'linealias supports)))
	(tactic-output "Applied RuleP." t)
	(values nil "Applied RuleP." 'succeed))
      (progn
	(tactic-output "Can't apply RuleP." nil)
        (values (list pline) "Can't apply RuleP." 'fail))))


(deftactic truthp-rewrite-plan-tac
  (etree-nat
   (lambda (pline)
     (truthp-rewrite-plan-tac-etree-nat-fn pline))
   "If the planned line corresponds to a rewrite node with justification
truthp, justifies the line by ad hoc Truthp,
and makes a new planned line with the rewritten wff."))


(defun truthp-rewrite-match2 (pline)
  (let ((node (line-node pline)))
    (and (auto::rewrite-p node)
	 (eq (auto::rewrite-justification node) 
	     'auto::truthp))))

(defun truthp-rewrite-plan-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plans (remove-if-not #'truthp-rewrite-match2 (list pline)))
	 (matched-plan
	  (if (eq tacmode 'interactive)
	      (find-if #'(lambda (x) (query (format nil  "Apply TRUTHP-REWRITE-PLAN to line ~D?"
						    (linealias x)) t))
		       matched-plans)
	      (car matched-plans)))
	 (oldplans (current-plan-lines dproof)))
    (if matched-plan
	(let ((msg "Applied TRUTHP-REWRITE-PLAN."))
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
	    (setf (line-justification pline)
		  (list "Truthp" nil (list newplan)))
            (update-plan (list (list pline 'SS)) (list (list newplan 'SS)))
	    (setf (line-node newplan)
		  (car (auto::etree-components (line-node pline))))
	    (setf (line-mating newplan) (line-mating pline))
	    (values (list newplan) msg 'succeed)))
	(let ((msg (if matched-plans
		       "Not applying TRUTHP-REWRITE-PLAN."
		       "Can't apply TRUTHP-REWRITE-PLAN.")))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail))
	)))


