;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of etr-nat)

(context etr-nat)

(deffile hx-natree-aux
  (extension lisp)
  (part-of etr-nat)
  (mhelp "Auxiliary functions for translating from natural deduction
proofs to expansion proofs."))

(defun rename-justification (just)
  (case just
     ((DEFN BINDER-DEFN) 'EQUIVWFFS)
     ((EQUIV-EQ EQUALITY) 'EQUIVWFFS)
     (|ETA RULE| 'ETA)
     (|BETA RULE| 'BETA)
     (LAMBDA= 'LAMBDA)
     ((IMPEQUIV EQUIVIMP) 'EQUIV-IMPLICS)
     (otherwise just)))

(defun deepenable-leaf-node-natree (node)
  (let ((wff (get-shallow node)))
    (or (not-p wff) (and-p wff) (or-p wff) (implies-p wff) (equiv-p wff)
	(wffeq wff 'TRUTH) (wffeq wff 'FALSEHOOD) (e-bd-wff-p wff) (a-bd-wff-p wff))))


;(defun updatable-dup-info (dup-info)
;  (cond ((leaf-p dup-info) (deepenable-dup-info dup-info))
;	((rewrite-p dup-info) (updatable-dup-info (first (etree-components dup-info))))))
;the above is never used, which is good since deepenable-dup-info is never defined! MB Wed Aug 28 13:05:38 1996

;;; at the beginning, slave-info must be a leaf!
(defun dup-info-rewrite-exp-ctr (up just master-info slave-info)
  (cond ((empty-dup-info-p master-info)
	 (let ((new-master-info (empty-dup-info-1 (get-shallow slave-info)
						  (etree-positive master-info))))
	   (setf (etree-parent new-master-info) (etree-parent master-info))
	   new-master-info))
	((leaf-p master-info) ;;;slave-info must be a leaf!!!
	 (if (wffeq (leaf-shallow master-info)
		    (leaf-shallow slave-info)) master-info
	     (let ((new-master-info (make-rewrite :components (list slave-info)
						  :positive (etree-positive master-info)
						  :junctive 'NEUTRAL
						  :parent (etree-parent master-info)
						  :shallow (leaf-shallow master-info)
						  :justification (rename-justification just)
						  :leaf (not up)
						  :reverse T)))
	       (setf (etree-parent slave-info) new-master-info) new-master-info)))
	((and (rewrite-p master-info)
	      (deepenable-leaf-node-natree slave-info))
	 (setf (etree-components master-info)
	       (list (dup-info-rewrite-exp-ctr up just
					       (first (etree-components master-info)) slave-info)))
	 master-info)
	(otherwise (let ((new-slave-info (update-dup-info slave-info)))
		     (cond ((leaf-p new-slave-info)
			    (let ((new-master-info (make-rewrite :components (list master-info)
								 :positive (etree-positive master-info)
								 :junctive 'NEUTRAL
								 :parent (etree-parent master-info)
								 :shallow (leaf-shallow new-slave-info)
								 :justification (rename-justification just)
								 :leaf up)))
			      (setf (etree-parent master-info) new-master-info)
			      (setq master-info new-master-info)))
			   #+comment
			   ((rewrite-p master-info)
			    (setf (etree-components master-info)
				  (list (dup-info-rewrite-exp-ctr up just
								  (first (etree-components master-info)) slave-info))))
			   ((negation-p master-info)
			    (setf (etree-components master-info)
				  (list (dup-info-rewrite-exp-ctr up just
								  (first (etree-components master-info))
								  (first (etree-components new-slave-info))
								  ))))
			   ((or (econjunction-p master-info)
				(edisjunction-p master-info)
				(implication-p master-info))
			    (setf (etree-components master-info)
				  (list (dup-info-rewrite-exp-ctr up just
								  (first (etree-components master-info))
								  (first (etree-components new-slave-info)))
					(dup-info-rewrite-exp-ctr up just
								  (second (etree-components master-info))
								  (second (etree-components new-slave-info))))))
			   ((selection-p master-info)
			    (let ((scom (first (etree-components new-slave-info)))
				  (var (first (selection-terms new-slave-info)))
				  newcomponents)
			      (do ((terms (selection-terms master-info) (cdr terms))
				   (mcoms (etree-components master-info) (cdr mcoms)))
				  ((null terms) (setf (selection-shallow master-info)
						      (get-shallow slave-info)
						      (etree-components master-info)
						      (nreverse newcomponents)))
				  (push (dup-info-rewrite-exp-ctr
					   up just (car mcoms) (substitute-in-etree-main (car terms) var (copy-dup-info scom)))
					newcomponents))))
			   ((expansion-p master-info)
			    (let ((scom (first (etree-components new-slave-info)))
				  (var (first (expansion-terms new-slave-info)))
				  newcomponents)
			      (do ((terms (expansion-terms master-info) (cdr terms))
				   (mcoms (etree-components master-info) (cdr mcoms)))
				  ((null terms) (setf (expansion-shallow master-info)
						      (get-shallow slave-info)
						      (etree-components master-info)
						      (nreverse newcomponents)))
				  (push (dup-info-rewrite-exp-ctr
					   up just (car mcoms) (substitute-in-etree-main (car terms) var (copy-dup-info scom)))
					newcomponents))))
			   (otherwise (display-dup-info master-info) (display-dup-info new-slave-info)
				      (throwfail "unexpected case occurred in DUP-INFO-REWRITE-EXP-CTR!!!")))
		     (display-dup-info master-info)
		     master-info))))

(defun merge-dup-info (master-info slave-info)
  (cond ((empty-dup-info-p master-info)
	 (setf (etree-parent slave-info)
	       (etree-parent master-info))
	 slave-info)
	((empty-dup-info-p slave-info) master-info)
	(otherwise
	   (cond ((and (negation-p master-info) (negation-p slave-info))
		  (setf (etree-components master-info)
			(list (merge-dup-info (first (etree-components master-info))
					      (first (etree-components slave-info))))
			(etree-parent (first (etree-components master-info))) master-info))
		 ((or (and (econjunction-p master-info) (econjunction-p slave-info))
		      (and (edisjunction-p master-info) (edisjunction-p slave-info))
		      (and (implication-p master-info) (implication-p slave-info)))
		  (let ((mcomponents (etree-components master-info))
			(scomponents (etree-components slave-info)))
		    (setf (etree-components master-info)
			  (list (merge-dup-info (first mcomponents) (first scomponents))
				(merge-dup-info (second mcomponents) (second scomponents)))
		          (etree-parent (first (etree-components master-info))) master-info)
		          (etree-parent (second (etree-components master-info))) master-info))
		 ;;; A merge is necessary here, but let us postpone it.
		 ((and (skolem-p master-info) (skolem-p slave-info))
		  (setf (etree-parent (first (etree-components slave-info))) master-info)
		  (setf (etree-components master-info)
			(append (etree-components master-info) (etree-components slave-info))
			(skolem-terms master-info)
			(append (skolem-terms master-info) (skolem-terms slave-info))))
		 ((and (selection-p master-info) (selection-p slave-info))
		  (setf (etree-parent (first (etree-components slave-info))) master-info)
		  (setf (etree-components master-info)
			(append (etree-components master-info) (etree-components slave-info))
			(selection-terms master-info)
			(append (selection-terms master-info) (selection-terms slave-info)))
		  ;;; A merge is necessary here, but let us postpone it, until shallow-merge.
		  (msg T "========================" T (selection-terms master-info) T "========================" T))
		 ((and (expansion-p master-info) (expansion-p slave-info))
		  (setf (etree-parent (first (etree-components slave-info))) master-info)
		  (setf (etree-components master-info)
			(append (etree-components master-info) (etree-components slave-info))
			(expansion-terms master-info)
			(append (expansion-terms master-info) (expansion-terms slave-info))))
		 ;;;this is the most difficult part; let us not jump too deep
		 ((and (rewrite-p master-info) (rewrite-p slave-info))
		  (setf (etree-components master-info)
			(list (merge-dup-info (first (etree-components master-info))
					      (first (etree-components slave-info))))
			(etree-parent (first (etree-components master-info))) master-info))
		 ((and (leaf-p master-info) (leaf-p slave-info)
		       (wffeq (leaf-shallow master-info) (leaf-shallow slave-info))))
		 ((leaf-p master-info)
		  (setf (etree-leaf slave-info) T)
		  (setq master-info slave-info))
		 ((leaf-p slave-info)
		  (setf (etree-leaf master-info) T))
;;;risky, risky
		 ((rewrite-p master-info)
		  (setf (etree-components master-info)
			(list (merge-dup-info (first (etree-components master-info)) slave-info))))
;;;risky, risky
		#+comment
		((rewrite-p slave-info)
		  (let ((new-master-info (merge-dup-info master-info
							 (first (etree-components slave-info)))))
		    (setf (etree-components slave-info) (list new-master-info)
			  (etree-parent new-master-info) slave-info)
		    (setq master-info slave-info)))
		((rewrite-p slave-info)
		 (merge-dup-info master-info
				 (first (etree-components slave-info))))
		(otherwise (describe master-info)
			    (describe slave-info)
			    (throwfail "A mismatch happened in merge-dup-info!!!")))
;	   (describe master-info)
	   master-info)))


(defun merge-selection-node (dup-info)
  (declare (special merge-theta))
; (xdisplay-dup-info dup-info)
  (cond ((selection-p dup-info)
	 (let ((fst (car (etree-components dup-info)))
	       (rep (car (selection-terms dup-info)))
	       (rest (cdr (etree-components dup-info)))
	       (others (cdr (selection-terms dup-info))))
	   (dolist (other others)
		   (push (cons other rep) merge-theta))
	   (dolist (snd rest)
		   (setq fst (shallow-merge fst snd dup-info)))
	   (setf (etree-components dup-info)
		 (list (merge-selection-node fst)))))
	 (otherwise (mapc #'merge-selection-node (etree-components dup-info))))
  dup-info)

(defun pre-merge-dup-info (dup-info)
  (let (merge-theta merge-list merge-mating)
    (declare (special merge-theta merge-list merge-mating))
    (let ((new-dup-info (merge-selection-node dup-info)))
      (simul-substitute-in-etree-main merge-theta new-dup-info)
      (display-dup-info new-dup-info)
      new-dup-info)))


(defun etree-to-jform-rec (etree junctive)
  (let ((j (etree-to-jform-rec-2 etree 
				 (if (ALLOW-NODE-IN-JFORM etree)
				     'CON
				   junctive))))
    (if (and (ALLOW-NODE-IN-JFORM etree) (not (leaf-p* etree)))
	(let ((lit (make-nnf-literal (etree-name* etree)
				     (strip-exp-vars (get-shallow etree))
				     (positive-p etree)
				     (etree-status* etree))))
	  (when (memq '& j)
	    (return-from etree-to-jform-rec (list '&)))
	  (setq j (delete '$ j))
	  (if (eq junctive 'CON)
	      (cons lit j)
	    (if j
		(let ((node (make-conjunction :components (cons lit j) :type 'conjunction)))
		  (setf (jform-parent lit) node)
		  (dolist (son j node)
		    (setf (jform-parent son) node))
		  (setf (jform-progress node)
		    (max (jform-progress lit) (jform-progress (car j))))
		  (list node))
	      (list lit))))
      j)))

(defun etree-to-jform-rec-2 (etree junctive)
  (when (plusp (etree-status* etree))
    (let (jformlist literal)
      (setq jformlist
	    (if (and (expansion-p etree) (not (leaf-p* etree)))
		(exp-node-to-jform etree junctive)
	      (if (etree-junctive* etree)
		  (cond 
		   ((cdr (etree-components* etree))
		    (let* ((flavor (if (etree-leaf etree) 'CON (etree-junctive* etree)))
			   (successors (mapcan #'(lambda (etree)
						   (etree-to-jform-rec etree flavor))
					       (etree-components* etree))))
		      (when successors
			    (if (eq junctive flavor) successors
			      (let ((node (if (eq flavor 'dis)
					      (make-disjunction :components successors
								:type 'disjunction)
					    (make-conjunction :components successors
							      :type 'conjunction))))
				(dolist (son successors)
					(setf (jform-parent son) node))
				(setf (jform-progress node)
				      (max (etree-status* etree)
					   (apply-fn-with-key #'max successors
							      #'jform-progress)))
				(list node))))))
		   ((etree-components* etree)
		    (etree-to-jform-rec (car (etree-components* etree))
					(if (etree-leaf etree) 'CON junctive)))
		   (t (list 
		       (case (etree-junctive* etree)
			     (con (make-conjunction :components nil :type 'conjunction
						    :represents
						    (if (wffeq (get-shallow etree) 'TRUTH)
							'TRUTH
						      (cons 'not 'FALSEHOOD))
						    :progress (etree-status* etree)))
			     (dis (make-disjunction :components nil :type 'disjunction
						    :represents
						    (if (wffeq (get-shallow etree) 'FALSEHOOD)
							'FALSEHOOD
						      (cons 'not 'TRUTH))
						    :progress (etree-status* etree))))))))))
      (setq literal (make-nnf-literal (etree-name* etree)
				      (strip-exp-vars (get-shallow etree))
				      (positive-p etree)
				      (etree-status* etree)))
      (if (etree-leaf etree)
	  (cond ((null jformlist) (list literal))
		((eq junctive 'CON) (cons literal jformlist))
		((null (second jformlist))
		 (list (make-conjunction :type 'CONJUNCTION
					 :components (list literal (first jformlist)))))
		((second jformlist)
		 (list (make-conjunction :type 'CONJUNCTION
					 :components (cons literal jformlist))))
		(otherwise (describe etree)
			   (throwfail "Unexpected case occurred in etree-to-jform-rec!")))
	(if jformlist jformlist (list literal))))))

(defun rewrite-equiv-natree (wff)
  (let ((left-arg (cdar wff))
	(rt-arg (cdr wff)))
    (acons 'and
	   (acons 'implies left-arg rt-arg)
	   (acons 'implies  rt-arg left-arg))))

;;;the function is in nat-etr. It is better to put it here if you decide
;;;to move the code here.

(defun deepen-leaf-node-rulep (node)
  (let ((wff (get-shallow node))
	(free-vars (etree-free-vars node))
	(parent (etree-parent node))
	(positive (positive-p node))
	(rename-all-bd-vars t)
	newnode)
    (declare (special rename-all-bd-vars))
    (setq newnode
	  (cond 
		((not-p wff)
		 (create-negation-node wff positive free-vars parent node))
		((and-p wff)
		 (create-econjunction-node wff positive free-vars parent node))
		((or-p wff)
		 (create-edisjunction-node wff positive free-vars parent node))
		((implies-p wff)
		 (create-implication-node wff positive free-vars parent node))
		((equiv-p wff)
		 (create-rewrite-node
		  wff positive (rewrite-equiv-natree wff) 'equiv-implics
		  free-vars parent node))
		((wffeq wff 'TRUTH)
		 (create-true-node wff positive free-vars parent node))
		((wffeq wff 'FALSEHOOD)
		 (create-false-node wff positive free-vars parent node))
		(t nil)))
    (cond (newnode
	    (update-global-lists node newnode)
	    newnode)
	  (T node))))

(defun deepen-leaf-node-with-dup-info (node dup-info)
  (let ((newnode (deepen-leaf-node-real node)))
    (cond ((expansion-p dup-info)
	   (dolist (term (cdr (expansion-terms dup-info)) newnode)
		   (declare (ignore term))
		   (duplicate-var newnode)))
	  (otherwise newnode))))

(defun deepen-leaf-node-with-dup-info* (node dup-info)
  (let ((newnode (deepen-leaf-node-with-dup-info node dup-info)))
    (mapc #'deepen-leaf-node-with-dup-info*
	  (etree-components newnode)
	  (etree-components dup-info))
    newnode))

;;;this is dumb, but a delicate one needs time.
(defun natree-subst-l-cntxt (orgin result lhs [])
  (if (wffeq orgin result) result
    (cond ((wffeq orgin lhs) [])
	  ((consp orgin) (cons (natree-subst-l-cntxt (car orgin) (car result) lhs [])
			       (natree-subst-l-cntxt (cdr orgin) (cdr result) lhs [])))
	  (otherwise (throwfail "This is not a right subst= application!")))))