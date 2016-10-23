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

(context prop-tactics)
(deffile ml-etr-tactics-book
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Defines bookkeeping tactics as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs."))


; changed this to return any node at the bottom of a chain of neg's - since we now allow nonleaf conns - cebrown 2/20/01
(defun chain-of-negs (node)
;  "If node is a chain of negation nodes followed by a leaf, returns that
;leaf, otherwise returns nil."
  "If node is a chain of negation nodes followed by a node, returns that node"
  (do ((node node (car (auto::etree-components node))))
      ((or (null (auto::etree-components node))
	   (not (auto::negation-p node)))
       node)))

(defun same-match2 (supports pline)
  (let ((conn-list (line-mating pline))
	(plan-bottom (chain-of-negs (line-node pline))))
    (dolist (support supports nil)
      (let* ((supp-node (line-node support))
	     (supp-bottom (chain-of-negs supp-node)))
	(if (and (or (and plan-bottom supp-bottom
			  (auto::mated-to-node supp-bottom plan-bottom conn-list))
		     (auto::mated-to-node (line-node pline) supp-node conn-list)) ; cebrown - 8/7/00 - in case nodes are nonleaves, but mated
		 (wffeq-ab (line-assertion support) 
			   (line-assertion pline)))
	      (return support))))))

(deftactic same-tac
  (etree-nat
   (lambda (pline)
     (same-tac-etree-nat-fn pline))
   "If planned line is the same as a support line, and they are mated,
applies SAME.  Pfenning's tactic 173."))

(defun same-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-support (same-match2 supports pline)))
    (if matched-support
	(let ((msg "Applied SAME."))
	  (tactic-output msg t)
	  (same-short pline matched-support)
	  (update-plan `((pp ,pline ,matched-support ss))
		       `((pp ,pline ss)))
	  (values nil msg 'succeed))
	(let ((msg "Can't apply SAME."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))))

(context aux-tactics)

					; mated-to-node-or-a-neg checks if the two nodes are mated or, if a neg, looks if children are mated
					; NNF should apply in this case
(defun mated-to-node-or-a-neg (supp-node plan-node conn-list)
  (let ((supp-nodes (list supp-node))
	(plan-nodes (list plan-node)))
    (do ()
	((not (negation-p (car supp-nodes))))
      (push (car (etree-components (car supp-nodes)))
	    supp-nodes))
    (do ()
	((not (negation-p (car plan-nodes))))
      (push (car (etree-components (car plan-nodes)))
	    plan-nodes))
    (dolist (snode supp-nodes)
      (dolist (pnode plan-nodes)
	(when (auto::mated-to-node snode pnode conn-list)
	  (return-from mated-to-node-or-a-neg t))))))

					; cebrown 2/17/01 - wrote NNF-TAC for nonleaf-matings of formulas that
					; are only the same up to NNF
(defun nnf-match2 (supports pline)
  (let ((conn-list (line-mating pline))
	(plan-node (line-node pline)))
    (dolist (support supports nil)
      (let ((supp-node (line-node support)))
	(if (and (mated-to-node-or-a-neg supp-node plan-node conn-list)
		 (wffeq-ab (neg-norm (line-assertion support))
			   (neg-norm (line-assertion pline))))
	    (when (or (not (eq tacmode 'interactive))
		      (query (format nil "Apply NNF to line ~D?" (linealias support)) t))
	      (return support)))))))

(deftactic nnf-tac
    (etree-nat
     (lambda (pline)
       (nnf-tac-etree-nat-fn pline))
     "Closes a gap when a support line is the same as the planned line up to NNF, and the nodes are mated."))

(defun nnf-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-support (nnf-match2 supports pline)))
    (if matched-support
	(let ((msg "Applied NNF."))
	  (tactic-output msg t)
	  (cond ((wffeq-ab (neg-norm (line-assertion pline)) (line-assertion pline))
		 (comdecode (list 'nnf (linealias matched-support) (linealias pline) '$ '$)))
		((wffeq-ab (neg-norm (line-assertion matched-support)) (line-assertion matched-support))
		 (comdecode (list 'nnf-expand (linealias pline) (linealias matched-support) '$ '$)))
		(t
		 (let* ((pln (linealias pline))
			(ln (- pln 1)))
		   (when (numalias ln)
		     (introduce-gap pline 1)
		     (setq ln (+ ln 1))
		     (setq pln (+ pln 1)))
		   (comdecode (list 'nnf (linealias matched-support) ln '$ '$))
		   (comdecode (list 'nnf-expand pln ln '$ '$)))))
	  (update-plan `((pp ,pline ,matched-support ss))
		       `((pp ,pline ss)))
	  (values nil msg 'succeed))
      (let ((msg "Can't apply NNF."))
	(tactic-output msg nil)
	(values (list pline) msg 'fail)))))

(deftactic unsponsor-tac
  (etree-nat
   (lambda (pline)
     (unsponsor-tac-etree-nat-fn pline))
   "Removes any support lines which are not required for the planned line."))

(defun connections-to-lits (conn-list)
  (append (mapcar #'car conn-list)
	  (mapcar #'cdr conn-list)))

					; for checking if rulep applies, in spite of the fact that there may be quantifiers -
					; this creates a jform without looking past exp/sel/rew nodes
(defun lines-to-rulep-jform (pline supports used-lits)
  (let ((allow-nonleaf-conns used-lits))
    (declare (special allow-nonleaf-conns))
    (let ((conjs (list (auto::etree-to-rulep-jform (line-node pline)))))
      (dolist (l supports)
	(when (auto::etree-positive (line-node l)) ; rule out supports that were previously proven planned lines 6/08/2002
	  (push (auto::etree-to-rulep-jform (line-node l)) conjs)))
      (if (cdr conjs)
	  (let ((j (auto::make-conjunction :components conjs :type 'conjunction)))
	    (dolist (conj conjs (auto::normalize-jform j))
		    (setf (auto::jform-parent conj) j)))
	(auto::normalize-jform (car conjs))))))

					; for checking if the supports are enough to prove the planned line
(defun lines-to-jform (pline supports other-nodes used-lits)
  (let ((allow-nonleaf-conns used-lits))
    (declare (special allow-nonleaf-conns))
    (let ((conjs (list (auto::etree-to-prop-jform (line-node pline)))))
      (dolist (l supports)
	(when (auto::etree-positive (line-node l)) ; rule out supports that were previously proven planned lines 6/08/2002
	  (if (mated-line l (line-mating pline)) ; must not look beneath this line, only look beneath its duplicate
					; otherwise, UNSPONSOR will unsponsor the duplicate we need. - cebrown 3/1/01
	      (push (auto::make-literal :name (etree-name (line-node l))
					:type 'literal)
		    conjs)
	    (push (auto::etree-to-prop-jform (line-node l)) conjs))))
      (dolist (n other-nodes)
	      (push (auto::etree-to-prop-jform n) conjs))
      (if (cdr conjs)
	  (let ((j (auto::make-conjunction :components conjs :type 'conjunction)))
	    (dolist (conj conjs (auto::normalize-jform j))
		    (setf (auto::jform-parent conj) j)))
	(auto::normalize-jform (car conjs))))))
      
(defun sufficient-lines-p (pline supports other-nodes conn-list)
  (auto::spanning-clist-p
   (lines-to-jform pline supports other-nodes (connections-to-lits conn-list))
   conn-list))
    
(defun unsponsor-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (supports (cdr (assoc pline (proof-plans dproof))))
	 (matched-supports
	  (do* ((conn-list (line-mating pline))
		(supps 	     
		  supports
;		 (remove-if #'(lambda (x) 
;				(auto::inessential-p conn-list
;						     (line-node x)))
;			    supports)
		 (cdr supps))
		(nec-supps nil)
		(unnec-supps (set-difference supports supps)))
	      ((null supps) unnec-supps)
	    (if (sufficient-lines-p pline (append (cdr supps) nec-supps) nil conn-list)
		(push (car supps) unnec-supps)
		(push (car supps) nec-supps)))))
    (if matched-supports
	(let ((msg "Applied UNSPONSOR to "))
	  (tactic-output 
	    (conc-strings msg 
			  (format nil "~A" 
				  (mapcar #'linealias matched-supports)))
	    t)
	  (funcall #'comdecode 
		   (list 'unsponsor (linealias pline)
			 (mapcar #'linealias matched-supports)))
	  (values (list pline) msg 'succeed))
	(let ((msg "Can't apply UNSPONSOR."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))))


(defun unnec-exp-match (pline)
  (and (auto::expansion-p (line-node pline))
       (> (length (auto::etree-components (line-node pline))) 1)))

(context quant-tactics)

(deftactic unnec-exp-tac
  (etree-nat
   (lambda (pline)
     (unnec-exp-tac-etree-nat-fn pline))
   "If the planned line is an expansion node, deletes any unnecessary
expansion terms."))

(defun unnec-exp-tac-etree-nat-fn (pline)
  (let* ((auto::*ignore-statuses* t)
	 (matched-plan 
	  (if (unnec-exp-match pline) pline))
	 (node (line-node pline))
	 (supports 
	  (if matched-plan (cdr (assoc pline (proof-plans dproof)))))
	 (unnec-exp
	  (if matched-plan
	      (let ((kids (auto::etree-components node))
		    (conn-list (line-mating pline))
		    (support-nodes 
		     (let ((support-nodes nil))
		       (dolist (support supports support-nodes)
			 (push (line-node support) support-nodes))))
		    (newnode (auto::copy-expansion node))
		    (unnec-exp nil))
		(dotimes (i (length kids) unnec-exp)
		  (let ((new-kids
			 (remove-if 
			  #'(lambda (x) 
			      (dolist (j (cons i unnec-exp) nil)
				(when (eq x (nth j kids)) 
				  (return t))))
			  kids)))
		    (setf (auto::etree-components newnode) new-kids)
		    (when (auto::spans newnode support-nodes conn-list)
		      (push i unnec-exp))))))))
    (if (and matched-plan unnec-exp)
	(let ((msg "Applied UNNEC-EXP.")
	      (newnode (auto::copy-expansion node))
	      (terms (auto::expansion-terms node))
	      (kids (auto::etree-components node))
	      (nec-exp (set-difference 
			(do* ((j (1- (length 
				      (auto::etree-components node)))
				 (1- j))
			      (l (list j) (cons j l)))
			    ((zerop j) l))
			unnec-exp)))
	  (setf (auto::expansion-terms newnode) nil)
	  (setf (auto::etree-components newnode) nil)
	  (dolist (i nec-exp)
	    (setf (auto::expansion-terms newnode)
		  (push (nth i terms) 
			(auto::expansion-terms newnode)))
	    (setf (auto::etree-components newnode)
		  (push (nth i kids) 
			(auto::etree-components newnode))))
	  (tactic-output msg t)
	  (setf (line-node pline) newnode)
	  (values (list pline) msg 'succeed))
	(let ((msg "Can't apply UNNEC-EXP."))
	  (tactic-output msg nil)
	  (values (list pline) msg 'fail)))))

(context compound-tactics)

(deftactic book-tac
  (etree-nat
    (orelse same-tac unsponsor-tac unnec-exp-tac)))


