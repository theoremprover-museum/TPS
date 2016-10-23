;;; -*- Mode:LISP; Package:AUTO; Mode:EDITOR -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of mating-transform)
;;;Filename: MATING-MERGE

(context etr-nat)

(deffile mating-merge2
  (extension lisp)
  (part-of mating-transform)
  (mhelp "Contains additional functions for merging expansion trees."))

;;; Written by Dan Nesmith
;;; Moves lambda rewrite nodes up in the tree as far as possible, to
;;; minimize the number of lexpd and lcontr steps in the natural deduction
;;; proof.

(defun lambda-rewrite-node-p (etree)
  (and (rewrite-p etree)
       (eq 'lambda (rewrite-justification etree))))


(defun raise-lambda-nodes-aux1 (etree connective)
 (let* ((kids (etree-components etree))
 (parent (etree-parent etree))
 (newnode (make-rewrite :parent parent
			:positive (positive-p etree)
			:components (list etree)
			:junctive 'neutral
			:justification 'lambda
			:shallow 
			(cons (cons connective 
					    (get-shallow (car kids)))
				      (get-shallow (cadr kids))))))
    (setf (etree-parent etree) newnode)
    (when parent
      (setf (etree-components parent)
	    (subst newnode etree (etree-components parent))))
    (setf (etree-components etree)
	  (list (if (lambda-rewrite-node-p (car kids))
		    (car (etree-components (car kids)))
		    (car kids))
		(if (lambda-rewrite-node-p (cadr kids))
		    (car (etree-components (cadr kids)))
		    (cadr kids))))
    (dolist (kid (etree-components etree))
      (setf (etree-parent kid) etree))
  newnode))

(defun raise-lambda-nodes-neg (etree)
  (let* ((kid (car (etree-components etree)))
	 (parent (etree-parent etree))
	 (newnode (make-rewrite :parent parent
				:positive (positive-p etree)
				:components (list etree)
				:junctive 'neutral
				:justification 'lambda
				:shallow 
				(cons 'not (get-shallow kid)))))
    (setf (etree-parent etree) newnode)
    (when parent
      (setf (etree-components parent)
	    (subst newnode etree (etree-components parent))))
    (setf (etree-components etree)
	  (etree-components kid))
    (dolist (kid (etree-components etree))
      (setf (etree-parent kid) etree))
    newnode))

(defun raise-lambda-nodes-ab (etree)
  (let* ((kid (car (etree-components etree)))
	 (kidkid (car (etree-components kid)))
	 (kidkidwff (get-shallow kidkid)))
    (setf (rewrite-justification etree) 'lambda)
    (setf (rewrite-justification kid) 'ab)
    (setf (rewrite-shallow kid)
	  (cons (car (get-shallow etree))
		(cdr kidkidwff)))
    etree))

(defun raise-lambda-nodes-equiv (etree)
  (let* ((kid (car (etree-components etree)))
	 (kidkid (car (etree-components kid)))
	 (firstkidkidwff (cdar (get-shallow kidkid))))
    (setf (rewrite-justification kid) (rewrite-justification etree))
    (setf (rewrite-justification etree) 'lambda)
    (setf (rewrite-shallow kid)
	  (cons (cons 'equiv (cdar firstkidkidwff))
		(cdr firstkidkidwff)))
    etree))

(defun raise-lambda-nodes-skol (etree)
  (let* ((kid (car (etree-components etree)))
	 (shallow (get-shallow etree))
	 (kidkidwff (get-shallow (car (etree-components kid))))
	 (parent (etree-parent etree))
	 (newnode (make-rewrite :parent parent
				:positive (positive-p etree)
				:components (list etree)
				:junctive 'neutral
				:justification 'lambda
				:shallow shallow)))
    (setf (etree-parent etree) newnode)
    (if (selection-p etree)
	(setf (selection-shallow etree)
	      (cons (car shallow) 
		    (substitute-term-var (caar shallow)
					 (car (selection-terms etree))
					 kidkidwff)))
	(setf (skolem-shallow etree)
	      (cons (car shallow) 
		    (substitute-term-var (caar shallow)
					 (car (skolem-terms etree))
					 kidkidwff))))
    (when parent
      (setf (etree-components parent)
	    (subst newnode etree (etree-components parent))))
    (setf (etree-components etree)
	  (etree-components kid))
    (dolist (kid (etree-components etree))
      (setf (etree-parent kid) etree))
    newnode))  


;;; Moves lambda-rewrite nodes up, over any propositional connectives,
;;; and skolem/selection nodes.  Stops at expansion nodes, and rewrite
;;; nodes other than equiv-implics, equiv-disjs and lambda.
					; cebrown - 11/3/00 - changed this to take a connection list and destructively modify it
					; by moving connections from lambda rewrite nodes to an appropriate node
					; invariant: the conn-list returned has no connections to any lambda rewrites below the etree returned

					; cebrown - 10/14/01 - modified this so that it doesn't
					; raise lambda nodes above lemmas.
					; cebrown - 6/12/2004 - added num-support lines to prevent raising lambda nodes
					; above implication and conjunction connecting nodes for support lines to node for goal line
(defun raise-lambda-nodes (etree conn-list lemmas &key (num-support-lines 0))
  (if lemmas
      (if (econjunction-p etree)
	  (multiple-value-bind
	   (etree2 conn-list2)
	   (raise-lambda-nodes-2 (car (etree-components etree)) conn-list lemmas)
	   (declare (ignore etree2))
	   (let ((imp (cadr (etree-components etree))))
	     (if (implication-p imp)
		 (multiple-value-bind
		  (etree3 conn-list3)
		  (raise-lambda-nodes-3 (car (etree-components imp)) conn-list2 lemmas)
		  (declare (ignore etree3))
		  (multiple-value-bind
		   (etree4 conn-list4)
		   (raise-lambda-nodes-supp (cadr (etree-components imp)) conn-list3 num-support-lines)
		   (declare (ignore etree4))
		   (values etree conn-list4)))
	       (throwfail "Merging Problem: expected " etree " to be an imp node for lemmas, as in [LEMMAS implies THM]"))))
	(throwfail "Merging Problem: expected " etree " to be an conj node giving lemmas, as in [LEMMAS and [LEMMAS implies THM]]"))
    (raise-lambda-nodes-supp etree conn-list num-support-lines)))

; cebrown - 6/12/2004
(defun raise-lambda-nodes-supp (etree conn-list num-support-lines)
  (if (> num-support-lines 0)
      (if (implication-p etree)
	  (let ((snodes (car (etree-components etree)))
		(cnode (cadr (etree-components etree))))
	    (multiple-value-bind
		(etree2 conn-list2)
		(raise-lambda-nodes-supp-1 snodes conn-list num-support-lines)
	      (declare (ignore etree2))
	      (multiple-value-bind
		  (etree3 conn-list3)
		  (raise-lambda-nodes-1 cnode conn-list2)
		(declare (ignore etree3))
		(values etree conn-list3))))
	(throwfail "Merging Problem: expected " etree " to be an imp node for support lines, as in [SUPPORT implies GOAL]"))
    (raise-lambda-nodes-1 etree conn-list)))

; cebrown - 6/12/2004
(defun raise-lambda-nodes-supp-1 (etree conn-list num-support-lines)
  (if (> num-support-lines 1)
      (if (econjunction-p etree)
	  (let ((snode (car (etree-components etree)))
		(snodes (cadr (etree-components etree))))
	    (multiple-value-bind
		(etree2 conn-list2)
		(raise-lambda-nodes-supp-1 snodes conn-list (- num-support-lines 1))
	      (declare (ignore etree2))
	      (multiple-value-bind
		  (etree3 conn-list3)
		  (raise-lambda-nodes-1 snode conn-list2)
		(declare (ignore etree3))
		(values etree conn-list3))))
	(throwfail "Merging Problem: expected "
		   etree " to be an conj node for multiple support lines, as in [SUPPORT1 AND SUPPORTREST]"))
    (raise-lambda-nodes-1 etree conn-list)))

(defun raise-lambda-nodes-2 (etree conn-list lemmas)
  (if lemmas
      (if (cdr lemmas)
	  (if (econjunction-p etree)
	      (let ((lem1 (car (etree-components etree)))
		    (rlems (cadr (etree-components etree))))
		(multiple-value-bind
		 (etree2 conn-list2)
		 (raise-lambda-nodes lem1 conn-list (cdar lemmas))
		 (declare (ignore etree2))
		 (multiple-value-bind
		  (etree3 conn-list3)
		  (raise-lambda-nodes-2 rlems conn-list2 (cdr lemmas))
		  (declare (ignore etree3))
		  (values etree conn-list3))))
	    (throwfail "Merging Problem: expected " etree " to be a conjunction of lemmas"))
	(raise-lambda-nodes etree conn-list (cdar lemmas)))
    (throwfail "Merging Problem with Lemmas: " etree))) ; should never happen

(defun raise-lambda-nodes-3 (etree conn-list lemmas)
  (if lemmas
      (if (cdr lemmas)
	  (if (econjunction-p etree)
	      (let ((lem1 (car (etree-components etree)))
		    (rlems (cadr (etree-components etree))))
		(multiple-value-bind
		 (etree2 conn-list2)
		 (raise-lambda-nodes-1 lem1 conn-list)
		 (declare (ignore etree2))
		 (multiple-value-bind
		  (etree3 conn-list3)
		  (raise-lambda-nodes-3 rlems conn-list2 (cdr lemmas))
		  (declare (ignore etree3))
		  (values etree conn-list3))))
	    (throwfail "Merging Problem: expected " etree " to be a conjunction of lemmas"))
	(raise-lambda-nodes-1 etree conn-list))
    (throwfail "Merging Problem with Lemmas: " etree))) ; should never happen

(defun raise-lambda-nodes-1 (etree conn-list)
  (typecase etree
    ((or leaf true false) (values etree conn-list))
    (expansion
     (dolist (e (etree-components etree))
       (multiple-value-bind
	   (etree2 conn-list2)
	   (raise-lambda-nodes-1 e conn-list)
	 (declare (ignore etree2))
	 (setq conn-list conn-list2)))
     (values etree conn-list))
    ((or selection skolem) 
     (multiple-value-bind
	 (etree2 conn-list2)
	 (raise-lambda-nodes-1 (car (etree-components etree)) conn-list)
       (if (lambda-rewrite-node-p etree2)
	   (values (raise-lambda-nodes-skol etree) conn-list2)
	 (values etree conn-list2))))
    (econjunction
     (dolist (e (etree-components etree))
       (multiple-value-bind
	   (etree2 conn-list2)
	   (raise-lambda-nodes-1 e conn-list)
	 (declare (ignore etree2))
	 (setq conn-list conn-list2)))
     (if (or (lambda-rewrite-node-p (car (etree-components etree)))
	     (lambda-rewrite-node-p (cadr (etree-components etree))))
	 (values (raise-lambda-nodes-aux1 etree 'and) conn-list)
       (values etree conn-list)))
    (edisjunction
     (dolist (e (etree-components etree))
       (multiple-value-bind
	   (etree2 conn-list2)
	   (raise-lambda-nodes-1 e conn-list)
	 (declare (ignore etree2))
	 (setq conn-list conn-list2)))
     (if (or (lambda-rewrite-node-p (car (etree-components etree)))
	     (lambda-rewrite-node-p (cadr (etree-components etree))))
	 (values (raise-lambda-nodes-aux1 etree 'or) conn-list)
       (values etree conn-list)))
    (implication
     (dolist (e (etree-components etree))
       (multiple-value-bind
	   (etree2 conn-list2)
	   (raise-lambda-nodes-1 e conn-list)
	 (declare (ignore etree2))
	 (setq conn-list conn-list2)))
     (if (or (lambda-rewrite-node-p (car (etree-components etree)))
	     (lambda-rewrite-node-p (cadr (etree-components etree))))
	 (values (raise-lambda-nodes-aux1 etree 'implies) conn-list)
       (values etree conn-list)))
    (negation
     (multiple-value-bind
	 (etree2 conn-list2)
	 (raise-lambda-nodes-1 (car (etree-components etree)) conn-list)
       (if (lambda-rewrite-node-p etree2)
	   (values (raise-lambda-nodes-neg etree) conn-list2)
	 (values etree conn-list2))))
    (rewrite
     (when (eq 'subst= (rewrite-justification etree)) ; to make sure no connections are made to lambda rewrite nodes,
					; in all other cases this is ensured by calling raise-lambda-nodes on the children of the node
       (dolist (l-node (find-etree-nodes #'lambda-rewrite-node-p etree))
	 (nsubst (car (etree-components l-node)) l-node conn-list)))
     (unless (eq 'subst= (rewrite-justification etree))
       (multiple-value-bind
	   (etree2 conn-list2)
	   (raise-lambda-nodes-1 (car (etree-components etree)) conn-list)
	 (declare (ignore etree2))
	 (setq conn-list conn-list2)))
     (when (and (lambda-rewrite-node-p (car (etree-components etree)))
		(eq 'lambda (rewrite-justification etree)))
       (setf (etree-components etree)
	 (etree-components (car (etree-components etree))))
       (setf (etree-parent (car (etree-components etree)))
	 etree))
     (when (and (lambda-rewrite-node-p (car (etree-components etree)))
		(eq 'ab (rewrite-justification etree)))
       (setq etree (raise-lambda-nodes-ab etree))) ; this commutes the rewrites
     (when (and (lambda-rewrite-node-p (car (etree-components etree)))
		(or (eq 'equiv-implics (rewrite-justification etree))
		    (eq 'equiv-disjs (rewrite-justification etree))))
       (setq etree (raise-lambda-nodes-equiv etree)))
     (when (eq 'lambda (rewrite-justification etree))
       (nsubst (car (etree-components etree)) etree conn-list)) ; key step in pushing down connections to lambda rewrites
					; note that we are replacing the node in the mating by its child, which cannot be
					; a lambda rewrite because if it had been, the first "when" block in this case
					; would have changed it. - cebrown 11/3/00
     (values etree conn-list))
    (otherwise (values etree conn-list))))


(defun replace-pi-and-sigma-in-wff (wff)
  (or (replace-pi-and-sigma-in-wff-rec wff) wff))

(defun replace-pi-and-sigma-in-wff-rec (wff)
  (declare (special binder-sigma-string binder-pi-string))
  (cond ((skolem-term-p wff)
	 (apply-label wff (replace-pi-and-sigma-in-wff-rec wff)))
	((lsymbol-q wff) nil)
	(t (let ((first (car wff))
		 (sec (cdr wff)))
	     (if (and (symbolp first)
		      (or (string= binder-sigma-string (nameroot first))
			  (string= binder-pi-string (nameroot first)))
		      (consp (get first 'core::unabbreviated-type))
		      (eq 'O (car (get first 'core::unabbreviated-type)))
		      (consp (cdr (get first 'core::unabbreviated-type)))
		      (eq 'O (cadr (get first 'core::unabbreviated-type))))
		 (let ((newsec (replace-pi-and-sigma-in-wff sec)))
		   (if (lambda-bd-p newsec)
		       (let ((var (caar newsec)))
			 (cons (cons var (if (string= binder-sigma-string 
						      (nameroot first))
					     'exists
					   'forall))
			       (cdr newsec)))
		     (let ((var (ren-var-x1 'cl-user::|x<I>|
					    (cddr (get first
						       'core::unabbreviated-type))))) 
		       (cons (cons var (if (string= binder-sigma-string 
						    (nameroot first))
					   'exists
					 'forall))
			     (cons newsec var)))))
	       (let ((newfirst (replace-pi-and-sigma-in-wff-rec first))
		     (newsec (replace-pi-and-sigma-in-wff-rec sec)))
		 (when (or newfirst newsec)
		   (cons (or newfirst first) (or newsec sec)))))))))
