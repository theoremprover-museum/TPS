;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(context ms98-1)

(deffile ms98-rewrite
  (part-of ms98)
  (extension lisp)
  (mhelp "Functions that implement rewriting of equalities in MS98-1"))

(defun get-ccs-rewrites ()
  (let ((nodes1 (find-etree-nodes #'(lambda (x) (and (rewrite-p x) (etree-positive x) (equals-p (get-shallow x))))
				 current-topnode))
;	gwff
	vars-to-unify nodes)
    (setq disj-assoc (fiddle-disjs (ccs-reorganize)))
    (setq nodes (remove-if #'disjunctive-above nodes1)) ; global rewrites
    (setq nodes1 (remove-if-not #'disjunctive-above nodes1)) ; local rewrites
    (setq nodes (remove-if #'(lambda (x) (or (> (length (find-etree-nodes #'leaf-p x)) 2)
					     (let ((exp-node (find-etree-node #'expansion-p x)))
					       (or (not exp-node) ; eg, if it is an EXT0
								  ; rewrite - cebrown 8/8/01
						   (not (= (length (expansion-terms exp-node)) 1))
						   (not (exp-var-p (car (expansion-terms exp-node))))
						   (not (eq (exp-var-var (car (expansion-terms exp-node)))
							    (exp-var-subst (car (expansion-terms exp-node)))))))))
					; cebrown 11/18/00 - added check to ensure evar q as eterm
			   nodes))	; only intended to work when 1 expansion w/ evar q as eterm
    (setq nodes1 (remove-if #'(lambda (x) (or (> (length (find-etree-nodes #'leaf-p x)) 2)
					     (let ((exp-node (find-etree-node #'expansion-p x)))
					       (or (not exp-node) ; eg, if it is an EXT0
								  ; rewrite - cebrown 8/8/01
						   (not (= (length (expansion-terms exp-node)) 1))
						   (not (exp-var-p (car (expansion-terms exp-node))))
						   (not (eq (exp-var-var (car (expansion-terms exp-node)))
							    (exp-var-subst (car (expansion-terms exp-node)))))))))
					; cebrown 11/18/00 - added check to ensure evar q as eterm
			    nodes1))	; only intended to work when 1 expansion w/ evar q as eterm
    (setq *rewrite-unif-hash* (make-hash-table :test 'equal))
    (setq *rewrite-varcheck* nil *rrule-fo-list* nil *rrule-sub-list* nil *usc-list* nil)
    (setq rcount 0 rconsts (find-consts))
    (deactivate-rules global-rewrite-rule-list)
    ; mkaminski 10/1/2005 -- added the following line
    (when ms98-external-rewrites (activate-rules *active-external-rewrites*))
    (dolist (n nodes)
      (setq *ccs-rewrites* t)
;      (setq gwff (cdr (jform-to-gwff (etree-to-jform n)))) ; changed the way we obtain left and right - cebrown 10/00
      ;; this will be ((OR NOT q left) q . right) ; we need to extract left and right.
      (let* ((leaves (find-etree-nodes #'leaf-p n))  ; cebrown 11/18/00 - changed to use leaves of etree instead of lits of jform, since now jforms may contain nonleaf literals, and lits may be dissolved in jform, replacing (setq gwff ...) commented out above
	     (left nil) 
	     (right nil))
	(dolist (le leaves)
	  (if (etree-positive le)
	      (setq right (strip-exp-vars (gdr (leaf-shallow le))))
	    (setq left (strip-exp-vars (gdr (leaf-shallow le))))))
	(when (and left right)
	  (create-rewrite-rule (etree-name n) left right
			       nil nil T nil "")
	  (let ((trouble-left (setdiff (intersection (free-vars-of left) (free-vars-in-etree current-eproof))
				       (intersection (free-vars-of right) (free-vars-in-etree current-eproof))))
	    ;;vars occurring on the left that aren't on the right.
		(trouble-right (setdiff (intersection (free-vars-of right) (free-vars-in-etree current-eproof))
					(intersection (free-vars-of left) (free-vars-in-etree current-eproof)))))
	    (when trouble-left
	      (setq vars-to-unify (append trouble-left vars-to-unify))
	      (push (cons (cons (etree-name n) t) trouble-left) *rewrite-varcheck*))
	    (when trouble-right
	      (setq vars-to-unify (append trouble-right vars-to-unify))
	      (push (cons (cons (etree-name n) nil) trouble-right) *rewrite-varcheck*))))))
    (setq *global-rewrites* (remove-if-not 'active-p global-rewrite-rule-list))
    ; mkaminski 10/1/2005 -- added the following line
    (unless (null *global-rewrites*) (setq *ccs-rewrites* t))
    (setq *local-rewrites* nil)
    (dolist (n nodes1)
;      (setq gwff (cdr (jform-to-gwff (etree-to-jform n))))
      ;; this will be ((OR NOT q left) q . right) ; we need to extract left and right.
      (let* ((leaves (find-etree-nodes #'leaf-p n))  ; cebrown 11/18/00 - changed to use leaves of etree instead of lits of jform, since now jforms may contain nonleaf literals, and lits may be dissolved in jform, replacing (setq gwff ...) commented out above
	     (left nil) 
	     (right nil))
	(dolist (le leaves)
	  (if (etree-positive le)
	      (setq right (strip-exp-vars (gdr (leaf-shallow le))))
	    (setq left (strip-exp-vars (gdr (leaf-shallow le))))))
	(when (and left right)
	  (let* ((local-leaves (setdiff (get-local-leaves n) leaves))
		 (trouble-left (when local-leaves
				 (setdiff (intersection (free-vars-of left) (free-vars-in-etree current-eproof))
					  (intersection (free-vars-of right) (free-vars-in-etree current-eproof)))))
	     ;;vars occurring on the left that aren't on the right.
		 (trouble-right (when local-leaves
				  (setdiff (intersection (free-vars-of right) (free-vars-in-etree current-eproof))
					   (intersection (free-vars-of left) (free-vars-in-etree current-eproof))))))
	    (when local-leaves		;(and local-leaves (not trouble-left) (not trouble-right))
	  ;;if nothing is local to this rewrite, don't bother
	  ;;the other clauses are hacks to make PA-THM2 tractable. Really, we need to delay 
	  ;;unifying where there are no interpretations. MB Mon Jun  1 16:55:24 1998
	      (setq *ccs-rewrites* t)
	      (create-rewrite-rule (etree-name n) left right
				   nil nil T nil "")
	      (dolist (l local-leaves)
		(if (assoc (leaf-name l) *local-rewrites*)
		    (rplacd (assoc (leaf-name l) *local-rewrites*) 
			    (cons (etree-name n) (cdr (assoc (leaf-name l) *local-rewrites*))))
		  (push (cons (leaf-name l) (list (etree-name n))) *local-rewrites*)))
	      (when trouble-left
		(setq vars-to-unify (append trouble-left vars-to-unify))
		(push (cons (cons (etree-name n) t) trouble-left) *rewrite-varcheck*))
	      (when trouble-right
		(setq vars-to-unify (append trouble-right vars-to-unify))
		(push (cons (cons (etree-name n) nil) trouble-right) *rewrite-varcheck*)))))))
    (setq *rewrite-dtree-list* nil)
    (if *ccs-rewrites* (setq *global-rewrite-dtree* (make-dtree :rewrites t))
      (setq *global-rewrite-dtree* nil))
    (when *ccs-rewrites* 
      (setq *all-rewrites* (append *global-rewrites* (remove-duplicates (reduce 'append (mapcar 'cdr *local-rewrites*)))))
      (setq *live-leaves* (find-leaves-not-below-rewrites))
      (dolist (d disj-assoc)
	(when (null (intersection (cadr d) *live-leaves*)) (push (car d) *dead-fragments*)))
      (setq *allowed-subs-list* nil)
      (unify-all-var-pairs (remove-duplicates 
			    (remove-if #'not-higher-order-var-p vars-to-unify)))
      (setq *all-rewrites* (append *global-rewrites* (remove-duplicates (reduce 'append (mapcar 'cdr *local-rewrites*)))))
      (first-order-ify))
    (setq *rewrite-model* nil *global-rewrite-model* nil)
    (setq *fo-delete* (copy-list *rrule-sub-list*)) ;;just temporarily...
    (setq *rrule-sub-list*  (make-hash-table :test 'equal))
    (dolist (elt *fo-delete*) (setf (gethash (car elt) *rrule-sub-list*) (cdr elt)))
    (setq *fo-delete* (remove-duplicates (mapcar #'cdr *rrule-fo-list*)))
    (deactivate-rules global-rewrite-rule-list)
    (activate-rules *global-rewrites*)
    (setq *active-rewrites* *global-rewrites*)
    (when *ccs-rewrites*
      (do-parity-check)
      (when ms98-rewrite-model (get-and-test-model-auto))
      (when *rewrite-model* (setq *global-rewrite-model* (cons (copy-list *active-rewrites*) (copy-list *rewrite-model*))))
      (when ms98-verbose (msgf "Parity check OK for " *parity-preserved*))
      (dolist (var (remove-duplicates 
		    (remove-if-not #'not-higher-order-var-p vars-to-unify)))
	(setf (gethash var *rewrite-unif-hash*)
	      (prune-out-rewrites-from (remove-duplicates (mapcar 'cdr (unify-var-rew (cons var -1))) :test 'wffeq-ab)))))))

(defun prune-rewrites-2 (key val)
  ;;val is a list of integers n; (gethash n *ccs-substs*) is the associated substitution
  (when ms98-rewrite-prune
    (let ((retlist val)
	  (varlist (free-vars-in-etree current-eproof)))
      (do ((vals (sort (mapcar #'(lambda (x) (cons x (gethash x *ccs-substs*))) val)
		       #'(lambda (x y) (< (length (wff-to-list (cdr x))) (length (wff-to-list (cdr y))))))
		 (cdr vals)))
	  ((null vals) (progn (when ms98-verbose (msg "(reduced to " (length retlist) ")")
				    (dolist (r retlist) (msgf ((gethash r *ccs-substs*) . gwff))))
			      (setf (gethash key *full-unifhash*) retlist)))
	(when (member (caar vals) retlist :test 'equal)
	  (dolist (x (cdr vals))
	    (when (and (or (not *rewrite-model*)
			   (replace-with-interps (cdr x) (cdar vals)))
		       (ccs-instance-of-rewriting (cdr x) (cdar vals) varlist))
	      (msg "/")
	      (setq retlist (delete (car x) retlist :test 'equal)
		    vals (delete x vals :test 'equalp))
	      )))))))

(defun prune-out-rewrites-from (terms)
  (if ms98-rewrite-prune
      (let ((retlist terms)
	    (varlist (free-vars-in-etree current-eproof)))
	(do ((terms terms (cdr terms)))
	    ((null terms) retlist)
	  (when (member (car terms) retlist :test 'fast-wffeq)
	    (dolist (x (cdr terms))
	      (when (and (or (not *rewrite-model*)
			     (replace-with-interps x (car terms)))
			 (ccs-instance-of-rewriting x (car terms) varlist))
		(let ((a (length (princ-to-string (car terms))))
		      (b (length (princ-to-string x))))
		  (if (< b a) (progn (setq retlist (delete (car terms) retlist :test 'fast-wffeq))
				     (return nil))
		    (setq retlist (delete x retlist :test 'fast-wffeq)
			  terms (delete x terms :test 'fast-wffeq)))))))))
    terms))

(defun unify-var-rew (var)
  (runcount 'unification)
  (let ((root (make-qnode :dpairs (list (cons var var)) :other 0))
	(real-var (find-fhead var)))
    (do ((ignore-nodes nil)
	 (max-substs-var (or ms98-rewrite-unif max-substs-var))
	 (nodes nil (cdr nodes))
	 (son root (car nodes))
	 (simpl nil))
	((null son)
	 (if (null ignore-nodes) nil
	   (progn (breakcount 'unification) 
		  (reduce 'append (mapcar 'qnode-substs ignore-nodes)))))
	(setq simpl (ho-simpl-uavp son))
	(when simpl
	      (if (zerop simpl)
		  (if (null (qnode-dpairs son))
		      (progn 		      
			(setf (qnode-substs son)
			      (list (assoc real-var (full-normalize-subs (qnode-substs son)) 
					   :test 'equal)))
			(push son ignore-nodes))
		    (setq nodes (nconc (ccs-rew-match-ff son) nodes)))
		(setq nodes (nconc (ccs-uavp-match son) nodes)))))))

(defun ccs-rew-match-ff (unode)
  (when (< (qnode-other unode) (or ms98-rewrite-unif max-substs-var))
	(let* ((dpair (choose-dpair (qnode-dpairs unode)))
	       (fhead1 (find-fhead (car dpair)))
	       (bdvars (ccs-get-binder (car dpair)))
	       (substitutions nil))
	  (dolist (symbol (cdr (assoc (get-resulttype fhead1) *global-constlist*)))
	    (setq substitutions (nconc (ccs-match-i symbol bdvars fhead1) substitutions)))
	  (setq substitutions (nconc (ccs-match-p fhead1) substitutions))
	  (create-successors-uavp substitutions unode))))

(defun symdiff (a b)
  (append (setdiff a b) (setdiff b a)))

(defun disjunctive-above (node)
  (if (eq node current-topnode) nil
    (if (etree-parent node)
	(if (eq (etree-junctive node) 'dis)
	    t
	  (disjunctive-above (etree-parent node)))
      nil)))

(defun generate-two-rewrite-trees (gwff1 gwff2 depth banned &optional (neg t))
  (unless (and neg
	       (or (and banned (or (contains-banned-const gwff1 banned)
				   (contains-banned-const gwff2 banned)))
		   (parity-check-fails gwff1 gwff2)
		   (not-remotely-plausible gwff1 gwff2)
		   (and *rewrite-model* (not (verify-by-model gwff1 gwff2)))
		   ))
    (let* ((rtree1 (make-rtree :rule nil :right nil :gwff gwff1 :sons nil 
			       :code nil :parent nil))
	   (local-dtree1 (make-dtree :rewrites t :refers (cons rtree1 t)))
	   (rtree2 (make-rtree :rule nil :right nil :gwff gwff2 :sons nil 
			       :code nil :parent nil))
	   (local-dtree2 (make-dtree :rewrites t :refers (cons rtree2 t)))
	   (varlist (free-vars-in-etree current-eproof)))
      (insert-rews-in-dtree gwff1 t local-dtree1 rtree1 t)
      (insert-rews-in-dtree gwff2 t local-dtree2 rtree2 t)
      (or (match-modulo-neg local-dtree1 local-dtree2 neg)
	  (do ((count 0 (1+ count))
	       (leaves1 (get-leaves-of-rtree rtree1))
	       (leaves2 (get-leaves-of-rtree rtree2))
	       (length1 1)
	       (length2 1))
	      ((>= count depth) nil)
	    (let ((result (if (< length1 length2)
			      (extend-rtree-2 leaves1 varlist local-dtree1 local-dtree2 nil banned neg)
			    (extend-rtree-2 leaves2 varlist local-dtree2 local-dtree1 t banned neg))))
	      (when result (return result))
	      (if (< length1 length2)
		  (progn (setq leaves1 (get-leaves-of-rtree rtree1)) (setq length1 (length leaves1)))
		(progn (setq leaves2 (get-leaves-of-rtree rtree2)) (setq length2 (length leaves2))))))))))

(defun get-leaves-of-rtree (rtree)
  (let (leaves)
    (do ((nodes (list rtree)))
	((null nodes) leaves)
      (let ((node (pop nodes)))
	(if (rtree-sons node)
	    (setq nodes (append (rtree-sons node) nodes))
	  (unless (rtree-code node) (push node leaves)))))))

(defun extend-rtree-2 (leaves varlist local-dtree compare-dtree mmn-flip banned neg)
  (when ms98-verbose (msg "|" (length leaves)))
  (dolist (leaf leaves)
    (let ((rewrites (get-rews-from-dtree *global-rewrite-dtree* (wff-to-list (rtree-gwff leaf))))
	  (count 0)
	  temp-rewrites insert)
      (setq insert (not (listp rewrites)))
      (when (eq rewrites t)
	(setq rewrites 
	      (get-all-one-step-rewrites (rtree-gwff leaf) *active-rewrites* varlist)))
      (when insert
	(setq rewrites (remove-duplicates rewrites :test 'fast-wffeq :key 'car))
	(when ms98-rewrite-size 
	  (setq rewrites (remove-if #'(lambda (x) (> (measure-size (car x)) ms98-rewrite-size)) rewrites)))
	(insert-rews-in-dtree (rtree-gwff leaf) rewrites))
      (dolist (r rewrites)
	(unless (or (and banned (contains-banned-const (car r) banned))
		    (exists-in-tree local-dtree (car r)))
	  (push r temp-rewrites)
	  (insert-rews-in-dtree (car r) t local-dtree leaf count)
	  (incf count)))
      (setq rewrites (nreverse temp-rewrites))
      (setf (rtree-sons leaf)
	    (mapcar #'(lambda (x) (make-rtree :gwff (car x) :code nil :rule (cadr x) :right (caddr x) :sons nil
					      :parent leaf))
		    rewrites))
      (when (null (rtree-sons leaf)) (setf (rtree-code leaf) t))
      (setq rewrites (if mmn-flip (match-modulo-neg compare-dtree local-dtree neg) 
		       (match-modulo-neg local-dtree compare-dtree neg)))
      (when rewrites (return rewrites)))))

(defun contains-banned-const (wff banned)
  (cond ((lsymbol-q wff) (member wff banned :test 'equal))
	((boundwff-q wff)
	 (contains-banned-const (cdr wff) banned))
	(t (or (contains-banned-const (car wff) banned)
	       (contains-banned-const (cdr wff) banned)))))

(defun subforall (wfflist var sub)
  (mapcar #'(lambda (x) (lnorm-q (substitute-l-term-var sub var x))) wfflist))

(defun get-all-one-step-rewrites (gwff rules varlist)
  (let ((results nil) temp)
    (dolist (r rules)
      (setq core::*binding-subs* nil)
      (when (ccs-rrule-instance-real gwff (get r 'before) varlist)
	(setq temp (cdr (assoc (cons r nil) *rewrite-varcheck* :test 'equalp))) ;list of variables to fix, or NIL.
	(if temp
	    (let ((eltlist (list (ccs-replace-rrule-poly (get r 'after)))))
	      (dolist (var temp 
			   (setq results (append (mapcar #'(lambda (x) (list x r nil)) eltlist) results)))
		(setq eltlist (make-list (length (gethash var *rewrite-unif-hash*)) :initial-element eltlist))
		(setq eltlist (reduce 'append 
				      (mapcar #'(lambda (wfflist sub) (subforall wfflist var sub)) eltlist
					      (gethash var *rewrite-unif-hash*))))))
	  (push (list (ccs-replace-rrule-poly (get r 'after)) r nil) results)))
      (setq core::*binding-subs* nil)
      (when (ccs-rrule-instance-real gwff (get r 'after) varlist)
	(setq temp (cdr (assoc (cons r t) *rewrite-varcheck* :test 'equalp)))
	(if temp
	    (let ((eltlist (list (ccs-replace-rrule-poly (get r 'before)))))
	      (dolist (var temp
			   (setq results (append (mapcar #'(lambda (x) (list x r t)) eltlist) results)))
		(setq eltlist (make-list (length (gethash var *rewrite-unif-hash*)) :initial-element eltlist))
		(setq eltlist (reduce 'append 
				      (mapcar #'(lambda (wfflist sub) (subforall wfflist var sub)) eltlist
					      (gethash var *rewrite-unif-hash*))))))
	  (push (list (ccs-replace-rrule-poly (get r 'before)) r t) results))))
    (when (and (listp gwff) (not (boundwff-q gwff)))
      (let ((new-results (get-all-one-step-rewrites (car gwff) rules varlist)))
	(dolist (n new-results)
	  (rplaca n (cons (car n) (cdr gwff))))
	(setq results (append new-results results))
	(setq new-results (get-all-one-step-rewrites (cdr gwff) rules varlist))
	(dolist (n new-results)
	  (rplaca n (cons (car gwff) (car n))))
	(setq results (append new-results results))))
    results))

(defun rtree-print (rtree &optional (depth 0))
  (msgf depth ": " ((rtree-gwff rtree) . gwff))
  (when (rtree-rule rtree)
    (msg " by the " (if (rtree-right rtree) "<--" "-->") " direction of " (rtree-rule rtree)))
  (when (rtree-sons rtree) (mapcar #'(lambda (x) (rtree-print x (1+ depth))) (rtree-sons rtree))))

(defun ccs-rrule-instance-real (inwff before varlist)
  (cond ((lsymbol-q before)
	 (or (fast-wffeq inwff before)
	     (and (member before varlist :test 'equal)
		  (type-equal inwff before)
		  (or (and (not (assoc before core::*binding-subs* :test 'equal))
			   (push (cons before inwff) core::*binding-subs*))
		      (fast-wffeq (cdr (assoc before core::*binding-subs* :test 'equal))
				  inwff)))))
	((boundwff-q before)
	 (and (boundwff-q inwff)
	      (eq (binder inwff) (binder before))
	      (push (cons (bdvar before) (bdvar inwff)) core::*binding-subs*)
	      (ccs-rrule-instance-real (gdr inwff) (gdr before) varlist)))
	((boundwff-q inwff) nil)
	(t (and (consp inwff) (consp before)
		(ccs-rrule-instance-real (gar inwff) (gar before) varlist)
		(ccs-rrule-instance-real (gdr inwff) (gdr before) varlist)))))

(defun ccs-replace-rrule-poly (after)
  (dolist (bs core::*binding-subs* after)
    (setq after (substitute-l-term-var (cdr bs) (car bs) after))))

(defun sort-of-simpl (a b)
  (or (and (not-p a) (wffeq (cdr a) b))
      (and (not-p b) (wffeq (cdr b) a))))

(defun fast-wffeq (wff1 wff2)
  (or (eq wff1 wff2)
  (cond ((lsymbol-q wff1) (eq wff1 wff2))
	((lsymbol-q wff2) nil)
	((boundwff-q wff1)
	 (if (boundwff-q wff2) (and (fast-wffeq (car wff1) (car wff2))
				    (fast-wffeq (cdr wff1) (cdr wff2))) 
	     nil))
	((boundwff-q wff2) nil)
	(t (and (fast-wffeq (car wff1) (car wff2))
		(fast-wffeq (cdr wff1) (cdr wff2)))))))

(defun wff-to-list (wff)
  (if (consp wff)
      (nconc (wff-to-list (car wff)) (wff-to-list (cdr wff)))
    (list wff)))

(defun measure-size (wff)
  (if (consp wff)
      (+ (measure-size (car wff)) (measure-size (cdr wff)))
    1))

(defun flatten-rtree (rtree &optional (pathsofar nil))
  (if (null rtree) (list nil)
    (cons (cons pathsofar (rtree-gwff rtree))
	  (reduce 'append 
		  (mapcar #'(lambda (x) (flatten-rtree x (cons (cons (rtree-rule x) (rtree-right x)) pathsofar)))
			  (rtree-sons rtree))))))

(defun flatten-rtree2 (rtree)
  (if (null rtree) (list nil)
    (cons (rtree-gwff rtree)
	  (reduce 'append 
		  (mapcar #'(lambda (x) (flatten-rtree2 x))
			  (rtree-sons rtree))))))

(defun find-leaves-not-below-rewrites ()
  (let ((leaves (find-etree-nodes #'leaf-p* current-topnode)) ;(eproof-etree current-eproof)))
	real-leaves)
    (dolist (l leaves real-leaves)
      (when (not-below-rewrite l) (push l real-leaves)))))

(defun not-below-rewrite (node)
  (if node
      (if (and (rewrite-p node) (memq (etree-name node) *all-rewrites*))
	  nil
	(not-below-rewrite (etree-parent node)))
    t))

    
(defun expand-by-rewriting (clist)
  ;;*successful-sub* contains the relevant substitution
  (let (newclist hash)
    (dolist (c clist 
	       (if *use-chain*
		   (dolist (s *successful-sub* newclist)
		     (when (member (car s) (free-vars-in-etree current-eproof))
		       (substitute-in-etree (gethash (cdr s) *ccs-substs*) (car s)
					    (eproof-etree current-eproof) nil)))
		 newclist))
      (setq hash (gethash c *rewrite-hash*))
      (if (null hash) (push c newclist)
	(dolist (h hash (push c newclist))
	  (when (subsetp (car h) *successful-sub* :test 'equalp)
	    (setq newclist (nconc (expand-tree-using (cdr h) c) newclist))
	    (return t)))))
    ))

(defun get-gwff-chain-from (list &optional (flip t))
  (if (null list) nil
    (if (cdr list)
	(if (sort-of-simpl (car list) (cadr list))
	    (cons (if flip 
		      (if (not-p (car list)) (cdar list) (cons 'not (car list)))
		    (car list))
		  (get-gwff-chain-from (cddr list) nil))
	  (cons (if flip 
		    (if (not-p (car list)) (cdar list) (cons 'not (car list)))
		  (car list))
		(get-gwff-chain-from (cdr list) flip)))
      (if flip 
	  (list (if (not-p (car list)) (cdar list) (cons 'not (car list))))
	list))))

(defun really-represents (wff)
  (lnorm-q (ho-bind-vars wff (append (mapcar #'(lambda (x) (cons x -1)) (free-vars-in-etree (eproof-etree current-eproof)))
				     '((var . -1)))
			 nil)
	   *successful-sub*))

(defun expand-tree-using (rewlist c)
  (let* ((curr-lit (car c))
	 (newgwff (really-represents (jform-represents (car c))))
	 (gwff-chain (append (get-gwff-chain-from (cons (if (jform-pos (car c)) newgwff
							    (cons 'not newgwff))
							(mapcar 'caddr rewlist)))
			     (list (really-represents (jform-represents (cdr c))))))
	 (gwff-chain (cdr gwff-chain))
	 (neg (jform-pos (car c))) ;do we need the mate to have a NOT on it?
	 newchain jf-left jf-right leaves edisj)
;    (msgf ((car c) . gwff)) (dolist (g gwff-chain) (msgf (g . gwff)))
    (dolist (r rewlist)
;      (msgf r "  " (assoc (car r) *rrule-fo-list* :test 'equal))
      (let ((enode (find-exp-node-above-rew (or (cdr (assoc (car r) *rrule-fo-list* :test 'equal)) (car r)))))
	(duplicate-var enode)
	(setq edisj enode)
	(setq enode (car (last (etree-components enode))))
	(deepen-to-literals enode)
	(setq enode (car (last (etree-components edisj))))
	(fiddle-with-jform)
	(setq leaves (jform-to-literal-list ms90-3-jform nil))
	(setq edisj (find-etree-node #'(lambda (x) (eq (etree-junctive x) 'dis)) enode))
	(setq jf-left (leaf-name (find-etree-node #'leaf-p (car (etree-components edisj)))))
	(setq jf-right (leaf-name (find-etree-node #'leaf-p (cadr (etree-components edisj)))))
	(setq jf-left (car (remove-if-not #'(lambda (x) (eq (literal-name x) jf-left)) leaves)))
	(setq jf-right (car (remove-if-not #'(lambda (x) (eq (literal-name x) jf-right)) leaves)))
	(if (cadr r) ;it's <--
	    (progn (push (cons curr-lit jf-right) newchain)
		   (when *use-chain*
		     (let ((newvar (ren-var-uni-ho w-var-prefix (cdr (type (head (jform-represents jf-right))))))
			   (foo (eproof-free-vars-in-etree current-eproof))
			   newsub)
		       (setq newsub (create-new-sub-rew newgwff (car gwff-chain) 
							(get (car r) 'after) (get (car r) 'before) newvar neg))
		       (when ms98-verbose
			 (msgf "Rewriting " (newgwff . gwff) t
			       "       to " ((car gwff-chain) . gwff) " using " (car r) " <--")
			 (msgf "  Subst is " (head (jform-represents jf-right)) "  " (newsub . gwff)))
		       (substitute-in-etree newsub (head (jform-represents jf-right)) (eproof-etree current-eproof) nil)
		       (setf (eproof-free-vars-in-etree current-eproof) foo)
		       (setq newgwff (pop gwff-chain))))
		   (setq curr-lit jf-left))
	  (progn (push (cons curr-lit jf-left) newchain)
		 (when *use-chain*
		   (let ((newvar (ren-var-uni-ho w-var-prefix (cdr (type (head (jform-represents jf-right))))))
			 (foo (eproof-free-vars-in-etree current-eproof))
			 newsub)
		     (setq newsub (create-new-sub-rew newgwff (car gwff-chain) 
						      (get (car r) 'before) (get (car r) 'after) newvar (not neg)))
		     (when ms98-verbose
		       (msgf "Rewriting " (newgwff . gwff) t
			     "       to " ((car gwff-chain) . gwff) " using " (car r) " -->")
		       (msgf "  Subst is " (head (jform-represents jf-right)) "  " (newsub . gwff)))
		     (substitute-in-etree newsub (head (jform-represents jf-right)) (eproof-etree current-eproof) nil)
		     (setf (eproof-free-vars-in-etree current-eproof) foo)
		     (setq newgwff (pop gwff-chain))))
		 (setq curr-lit jf-right)))))
    (push (cons curr-lit (cdr c)) newchain)
    (msgf "Replacing " c " with " newchain t)
    newchain))

(defun create-new-sub-rew (oldwff newwff oldpart newpart var1 neg)
  (let ((awff (if (not-p oldwff) (cdr oldwff) oldwff))
	(bwff (if (not-p newwff) (cdr newwff) newwff)))
    (if neg
	(cons (cons var1 'LAMBDA) (cons 'NOT (get-real-sub awff bwff oldpart newpart var1)))
      (cons (cons var1 'LAMBDA) (get-real-sub awff bwff oldpart newpart var1)))))

(defun get-real-sub (wff1 wff2 p1 p2 v)
  (setq core::*binding-subs* nil)
  (cond ((fast-wffeq wff1 wff2) wff1)
	((and (ccs-rrule-instance-real wff1 p1 (free-vars-in-etree current-eproof))
;	      (or (setq core::*binding-subs* nil)
		  (ccs-rrule-instance-real wff2 p2 (free-vars-in-etree current-eproof)));)
	 v)
	(t (cons (get-real-sub (gar wff1) (gar wff2) p1 p2 v)
		 (get-real-sub (gdr wff1) (gdr wff2) p1 p2 v)))))

(defun find-exp-node-above-rew (rew)
  (let ((node (follow-branch-upwards (find-etree-node-name rew))))
    (if (expansion-p node) node (find-etree-node #'expansion-p node))))

(defun follow-branch-upwards (node)
  (if (etree-parent node)
      (if (and (memq (etree-junctive (etree-parent node)) '(CON DIS)) (not (expansion-p (etree-parent node))))
	  node
	(follow-branch-upwards (etree-parent node)))
    node))

(defun fiddle-with-jform ()
  (setq ms90-3-jform (init-position (etree-to-jform (eproof-etree current-eproof))))
  (pathnum-init-jform ms90-3-jform)
  (remove-sk-labels ms90-3-jform)
  (mark-ext-exp-nodes ms90-3-jform))

(defun insert-rews-in-dtree (wff rewrites &optional (tree *global-rewrite-dtree*) (leaf nil) (count nil))
  (insert-in-dtree-real tree (wff-to-list wff) rewrites leaf count))

(defun insert-in-dtree-real (dtree wfflist rewrites leaf count)
  (if (cdr wfflist)
      (let ((son (cdr (assoc (car wfflist) (dtree-sons dtree) :test 'equal))))
	(if son (insert-in-dtree-real son (cdr wfflist) rewrites leaf count)
	  (let ((newson (make-dtree :symbol (car wfflist) :rewrites t :sons nil)))
	    (push (cons (car wfflist) newson) (dtree-sons dtree))
	    (insert-in-dtree-real newson (cdr wfflist) rewrites leaf count))))
    (let ((son (cdr (assoc (car wfflist) (dtree-sons dtree) :test 'equal))))
      (if son (setf (dtree-rewrites son) rewrites)
	(let ((newson (make-dtree :symbol (car wfflist) :rewrites rewrites :sons nil :refers (cons leaf count))))
	  (push (cons (car wfflist) newson) (dtree-sons dtree)))))))

(defun get-rews-from-dtree (dtree wfflist)
  (if (cdr wfflist)
      (let ((son (cdr (assoc (car wfflist) (dtree-sons dtree) :test 'equal))))
	(if son (get-rews-from-dtree son (cdr wfflist))
	  t)) ;t means "don't know"
    (let ((son (cdr (assoc (car wfflist) (dtree-sons dtree) :test 'equal))))
      (if son (dtree-rewrites son) t))))
      
(defun exists-in-tree (tree wff)
  (exists-in-tree-real tree (wff-to-list wff)))

(defun exists-in-tree-real (tree wfflist)
  (if (null tree) nil
    (if (null wfflist) 
	(null (dtree-sons tree))
      (exists-in-tree-real (cdr (assoc (car wfflist) (dtree-sons tree) :test 'equal)) (cdr wfflist)))))

(defun get-and-test-model ()
  (msgf "We have the following rewrite rules:" t)
  (dolist (rew global-rewrite-rule-list)
    (when (and (rewrule-p rew) (active-p rew))
      (core::rewrite-mhelp rew 'rewrite-rule)))
  (msgf t "Please input a model:")
  (let ((consts (remove-if #'(lambda (x) (get x 'mhelp)) (reduce 'append (mapcar 'cdr *global-constlist*))))
	temp)
    (dolist (c consts)
      (prompt-read temp nil (msgf "Interpretation for " (c . gwff)) 'anything nil nil)
      (when temp (push (cons c temp) *rewrite-model*)))))

(defun ident (x) x)

(defun verify-by-model (wff1 wff2)
  (let ((result (if (not-p wff1)
		    (replace-with-interps (cdr wff1) wff2)
		  (if (not-p wff2)
		      (replace-with-interps wff1 (cdr wff2))
		    nil))))
    result))

(defun replace-with-interps (wff1 wff2)
  (if (and (dolist (w (wff-to-list wff1) t)
	     (unless (cdr (assoc w *rewrite-model* :test 'equal))
	       (return nil)))
	   (dolist (w (wff-to-list wff2) t)
	     (unless (cdr (assoc w *rewrite-model* :test 'equal))
	       (return nil))))
      (same-interpretation wff1 wff2)
    (if (and (consp wff1) (consp wff2))
	(and (replace-with-interps (gar wff1) (gar wff2))
	     (replace-with-interps (gdr wff1) (gdr wff2)))
      (wffeq wff1 wff2))))
	
(defun same-interpretation (wff1 wff2)
  (if *model-flag* (equivalent-interp wff1 wff2)
    (let ((int1 (interpret wff1))
	  (int2 (interpret wff2)))
      (or (and (consp int1) (consp int2))
	  (equal int1 int2)))))

(defun equivalent-interp (wff1 wff2)
  (valid-p (cons (cons 'equiv (interpret-pred wff1)) (interpret-pred wff2))))

(defun interpret-pred (wff1)
  (let ((a (or (cdr (assoc (head wff1) *rewrite-model* :test 'equal))
	       'fail))
	(a-args (mapcar #'interpret-pred (args wff1))))
    (if a-args
	(progn (push a a-args)
	       (reduce 'cons a-args))
      a)))

(defun interpret (wff1)
  (let ((a (or (cdr (assoc (head wff1) *rewrite-model* :test 'equal))
	       'fail))
	(a-args (mapcar #'interpret (args wff1))))
    (if a-args
	(progn (push a a-args)
	       (if (or (member 'fail a-args :test 'equal) (not (symbolp a)) (not (fboundp a)))
		   'fail ;a-args 
		 (eval a-args)))
      a)))

(defun args (gwff)
  (and (consp gwff)
       (append (args (car gwff)) (list (cdr gwff)))))



;;*********************************

(defun ccs-instance-of-rewriting (inwff outwff varlist)
  (dolist (rule *active-rewrites* nil)
	  (when (rewrule-p rule)
		(setq core::*hopeless-fail* nil)
		(when (ccs-apply-rrule-once inwff outwff
					    (get rule 'before) (get rule 'after) varlist)
		      (when (not core::*hopeless-fail*) (return t)))
		(setq core::*hopeless-fail* nil)
		(when (get rule 'bidirectional)
		  (when (ccs-apply-rrule-once inwff outwff
					      (get rule 'after) (get rule 'before) varlist)
		    (when (not core::*hopeless-fail*) (return t)))))))

(defun ccs-apply-rrule-once (inwff outwff before after varlist)
  (setq core::*binding-subs* nil)
  (if core::*hopeless-fail* nil
    (cond ((lsymbol-q inwff)
	   (if (ccs-rrule-instance-real inwff before varlist)
	       (let ((newwff (ccs-replace-rrule-poly after)))
		 (if (wffeq-ab newwff outwff)
		     t
		   (if (wffeq-ab inwff outwff) nil (progn (setq core::*hopeless-fail* t) nil))))
	     (if (wffeq-ab inwff outwff) nil (progn (setq core::*hopeless-fail* t) nil))))
	  ((boundwff-q inwff)
	   (if (ccs-rrule-instance-real inwff before varlist)
	       (let ((newwff (ccs-replace-rrule-poly after)))
		 (if (wffeq-ab newwff outwff)
		     t
		   (ccs-apply-rrule-once (gdr inwff) (gdr outwff) before after varlist)))
	     (when (and (consp outwff))
	       (ccs-apply-rrule-once (gdr inwff) (gdr outwff) before after varlist))))
	  (t (if (ccs-rrule-instance-real inwff before varlist)
		 (let ((newwff (ccs-replace-rrule-poly after)))
		   (if (wffeq-ab newwff outwff)
		       t
		     (and (consp outwff)
			  (or (ccs-apply-rrule-once (car inwff) (car outwff) before after varlist)
			      (ccs-apply-rrule-once (cdr inwff) (cdr outwff) before after varlist)))))
	       (and (consp outwff)
		    (or (ccs-apply-rrule-once (car inwff) (car outwff) before after varlist)
			(ccs-apply-rrule-once (cdr inwff) (cdr outwff) before after varlist))))))))

(defun do-parity-check ()
  (setq *parity-preserved* (remove-if #'(lambda (x) (null (cdr x)))
				      (mapcar #'cons '(2 3 5 7) (mapcar #'do-parity-check-real '(2 3 5 7))))))

(defun do-parity-check-real (base)
  ;;idea: check for the parity of each constant on the rhs and lhs of the rules 
  ;;(remembering that a variable can be instantiated with any arbitrary rubbish)
  ;;constants that will have the same parity on both sides are returned in *parity-preserved*
  (let ((rrules *active-rewrites*)
	(varlist (free-vars-in-etree current-topnode))
	(rconsts (remove-if #'(lambda (x) (get x 'mhelp)) (find-consts)))
	(to-delete nil)
	(pp (remove-if #'(lambda (x) (get x 'mhelp)) (find-consts))))
    (dolist (r rrules)
      (setq to-delete (parity-check (get r 'before) (get r 'after) varlist rconsts base))
      (dolist (del to-delete)
	(setq pp (delete del pp :test 'equal)))
      (when (null pp) (return nil)))
    pp))

(defun parity-check (lwff rwff vars consts &optional (base 2))
  (let ((vcount (mapcar #'(lambda (x) (cons x 0)) vars))
	(ccount (mapcar #'(lambda (x) (cons x 0)) consts))
	(llist (wff-to-list lwff))
	(rlist (wff-to-list rwff)))
    (dolist (l llist)
      (if (member l vars :test 'equal)
	  (incf (cdr (assoc l vcount :test 'equal)))
	(when (member l consts :test 'equal)
	  (incf (cdr (assoc l ccount :test 'equal))))))
    (dolist (r rlist)
      (if (member r vars :test 'equal)
	  (decf (cdr (assoc r vcount :test 'equal)))
	(when (member r consts :test 'equal)
	  (decf (cdr (assoc r ccount :test 'equal))))))
    (if (dolist (v vcount t)
	  (unless (zerop (mod (cdr v) base)) (return nil))) 
					;if some variable has odd parity, we're stuffed...
	(let (retlist)
	  (dolist (c ccount retlist)
	    (unless (zerop (mod (cdr c) base)) (push (car c) retlist)))) ;delete all constants with odd parity.
      consts))) ;o/w delete all constants... :-(

(defun parity-check-fails (gwff1 gwff2)
  (dolist (elt *parity-preserved* nil)
    (let* ((odd-parity-consts 
	    (parity-check gwff1 gwff2 nil (remove-if #'(lambda (x) (get x 'mhelp)) (find-consts)) (car elt)))
	   (result (intersection odd-parity-consts (cdr elt) :test 'equal)))
      (when result (return result)))))

(defun reciprocal (x)
  (if (equal x 0) 'fail
    (/ 1 x)))

(defun plus1 (x)
  (+ x 1))

(defun squarex (x)
  (* x x))

(defun get-and-test-model-auto ()
  (let* ((iconsts (find-consts-in-rrules
		   (remove-if #'(lambda (x) (get x 'mhelp)) (reduce 'append (mapcar 'cdr *global-constlist*)))))
	 (model-list (list '(2 - * +) '(1 - reciprocal plus1) '(0 0 1)))
	 (generic-consts (make-primes (length ;(append (setdiff *parity-preserved* iconsts)
						      (free-vars-in-etree current-topnode))));)
	 (temp-rewrite-model1 (mapcar 'cons 
				      (remove-if #'(lambda (x) (consp (type x))) 
;						 (append (setdiff *parity-preserved* iconsts)
							 (free-vars-in-etree current-topnode));)
				      generic-consts))
	 (temp-rewrite-model2 (mapcar 'cons 
				      (reverse (remove-if #'(lambda (x) (consp (type x))) 
;							  (append (setdiff *parity-preserved* iconsts)
								  (free-vars-in-etree current-topnode)));)
				      generic-consts)))
    (setq *rewrite-model* nil)
    (setq *model-flag* nil)
    (if (find-interpretation-for-rrules model-list iconsts *active-rewrites* temp-rewrite-model1 temp-rewrite-model2)
	(let ((extra (remove-if #'(lambda (x) (or (member x iconsts :test 'equal)
						  (consp (type x)))) (find-consts))))
	  (setq extra (mapcar 'cons extra (make-primes (length extra))))
	  (setq *rewrite-model* (append extra *rewrite-model*))
	  (when ms98-verbose (msgf "Found the interpretation: " *rewrite-model*)))
      (get-and-test-model-2))))


(defun get-and-test-model-2 ()
  (let* ((iconsts (find-consts-in-rrules
		   (remove-if #'(lambda (x) (get x 'mhelp)) (reduce 'append (mapcar 'cdr *global-constlist*)))))
	 (model-list (list '(2 and or implies equiv) 
			   '(1 not) '(0 TRUTH FALSEHOOD)))
	 (generic-consts (make-predicates (length ;(append *parity-preserved* 
							  (free-vars-in-etree current-topnode))));)
	 (temp-rewrite-model1 (mapcar 'cons 
				      (remove-if #'(lambda (x) (consp (type x))) 
;						 (append (setdiff *parity-preserved* iconsts)
							 (free-vars-in-etree current-topnode));)
				      generic-consts)))
    (setq *rewrite-model* nil)
    (setq *model-flag* t)
    (if (find-interpretation-for-rrules model-list iconsts *active-rewrites* temp-rewrite-model1 nil)
	(let ((extra (remove-if #'(lambda (x) (or (member x iconsts :test 'equal)
						  (consp (type x)))) (find-consts))))
	  (setq extra (mapcar 'cons extra (make-predicates (length extra))))
	  (setq *rewrite-model* (append extra *rewrite-model*))
	  (when ms98-verbose (msgf "Found the interpretation: " *rewrite-model*)))
      (setq *rewrite-model* nil))))

(defun make-predicates (n)
  (if (> n 0)
      (cons (ren-var-uni-ho (gensym) 'O) (make-predicates (1- n)))
    nil))

(defun find-interpretation-for-rrules (possible consts rules tr1 tr2)
  (let ((old-rewrite-model (copy-list *rewrite-model*))
	(real-consts (choose-consts (caar possible) (mapcar #'(lambda (x) (cons (type x) x)) consts))))
    (dolist (newstuff (assign-consts-all-possible-ways (cdar possible) real-consts) nil)
      (setq *rewrite-model* (append newstuff old-rewrite-model))
      (if (cdr possible)
	  (when (find-interpretation-for-rrules (cdr possible) consts rules tr1 tr2) (return t))
	(let ((*whole-rewrite* *rewrite-model*))
	  (when (and (setq *rewrite-model* (append *whole-rewrite* tr1))
		     (verify-this-interpretation rules)
		     (or (null tr2)
			 (setq *rewrite-model* (append *whole-rewrite* tr2))
			 (verify-this-interpretation rules)))
	    (return t)))))))

(defun choose-consts (num consts &optional (fixtype nil))
  ;;consts is a list of (type . const)
  (let (clist)
    (if (= num 0)
	(progn (setq clist (remove-if #'(lambda (x) (consp (car x))) consts))
	       (when fixtype (setq clist (remove-if-not #'(lambda (x) (equalp (car x) fixtype)) clist)))
	       (mapcar 'cdr clist))
      (progn (setq clist (remove-if-not #'(lambda (x) (consp (car x))) consts))
	     (if fixtype (progn
			   (setq clist (remove-if-not #'(lambda (x) (equalp fixtype (cdar x))) clist))
			   (choose-consts (1- num) (mapcar #'(lambda (x) (cons (caar x) (cdr x))) clist)
					  fixtype))
	       (let ((newclist nil))
		 (dolist (ft (remove-duplicates (mapcar 'cdar clist) :test 'equalp) newclist)
		   (setq newclist (append (choose-consts (1- num) 
							 (mapcar #'(lambda (x) (cons (caar x) (cdr x)))
								 (remove-if-not #'(lambda (x) (equalp ft (cdar x))) clist))
							 ft)
					  newclist)))))))))

(defun assign-consts-all-possible-ways (interps consts)
  (if (null consts) (list nil)
    (let ((retlist (assign-consts-all-possible-ways interps (cdr consts)))
	  temp)
      (dolist (i interps (reduce 'append temp))
	(push (mapcar #'(lambda (x) (cons (cons (car consts) i) x)) retlist) temp)))))

(defun verify-this-interpretation (rules)
  (dolist (r rules t)
    (unless (replace-with-interps (get r 'before) (get r 'after)) (return nil))))

(defun find-consts-in-rrules (consts)
  (let ((rlist (remove-duplicates (reduce 'append (mapcar #'(lambda (x) (append (wff-to-list (get x 'before))
										(wff-to-list (get x 'after))))
							  *active-rewrites*))
				  :test 'equal)))
    (intersection rlist consts)))

(defun first-order-ify ()
  ;; plug in values for any higher-order variables in the rewrite rules
  (dolist (rule *all-rewrites*)
    (let ((vars (remove-if #'(lambda (x) (and (not-higher-order-var-p x) (covers-two x)))
			   (remove-duplicates (append (free-vars-of (get rule 'before)) 
						      (free-vars-of (get rule 'after))))))
	  (rrule-list (list (list (cons (get rule 'before) (get rule 'after)) nil))))
      (when vars
	(setq rrule-list (plug-ho-vars rrule-list))
	(dolist (r rrule-list)
	  (let ((name (new-rrule-name)))
	    (create-rewrite-rule name (caar r) (cdar r) rule nil t nil "")
	    (push (cons name (sort (cadr r) #'string< :key 'car)) *rrule-sub-list*)
	    (push (cons name rule) *rrule-fo-list*)))
	(setf (get rule 'active) nil) ;deactivate the old rule
	)))
  (dolist (r (collapse-fo-list *rrule-fo-list*))
    (setq *all-rewrites* (append (cdr r) (delete (car r) *all-rewrites* :test 'equal)))
    (if (member (car r) *global-rewrites* :test 'equal)
	(setq *global-rewrites* (append (cdr r) (delete (car r) *global-rewrites* :test 'equal)))
      ;;o/w it's somewhere in *local-rewrites*
      (dolist (l *local-rewrites*) 
	(when (member (car r) (cdr l) :test 'equal)
	  (rplacd l (append (cdr r) (delete (car r) (cdr l) :test 'equal))))))))


(defun collapse-fo-list (list)
  (let (retlist)
  (dolist (elt list retlist)
    (if (assoc (cdr elt) retlist) (push (car elt) (cdr (assoc (cdr elt) retlist)))
      (push (cons (cdr elt) (list (car elt))) retlist)))))

(defun plug-ho-vars (rrule-list)
  (do ((rrules (cdr rrule-list))
       (rule (car rrule-list) (if rrules (pop rrules) nil))
       (done nil))
      ((null rule) done)
    (let* ((vars (remove-if #'(lambda (x) (and (not-higher-order-var-p x) (covers-two x))) ;'not-higher-order-var-p
			    (remove-duplicates (append (free-vars-of (caar rule)) 
						       (free-vars-of (cdar rule))))))
	   (vl (list (cons (car vars) -1) (cons 'var -1))))
      (if vars
	  (dolist (sub (get-correct-subs (car vars)))
	    (push (list (cons (lnorm-q (ho-bind-vars (caar rule) vl nil) (list (cons (car vars) sub)))
			      (lnorm-q (ho-bind-vars (cdar rule) vl nil) (list (cons (car vars) sub))))
			(cons (cons (car vars) sub) (copy-list (cadr rule))))
		  rrules))
	(push rule done)))))

(defun not-higher-order-var-p (var)
  (or (member var rconsts :test 'equal)
      (not (consp (type var)))))

(defun covers-two (var)
  (let ((node (find-if #'(lambda (x) (member var (mapcar #'exp-var-var (expansion-terms x))))
		       (find-etree-nodes 'expansion-p current-topnode))))
    (or (null node)
	(<= (length (find-etree-nodes #'leaf-p node)) 2))))

(defun new-rrule-name ()
  (declare (special rcount))
  (make-symbol (concatenate 'string "CR" (princ-to-string (incf rcount)))))

(defun not-remotely-plausible (gwff1 gwff2)
  (let ((wff1 (if (not-p gwff1) (cdr gwff1) gwff1))
	(wff2 (if (not-p gwff2) (cdr gwff2) gwff2))
	(v (free-vars-in-etree current-eproof))
	(both-neg (and (not-p gwff1) (not-p gwff2)))
	(both-pos (and (not (not-p gwff1)) (not (not-p gwff2)))))
    (if both-neg
	(or (not (exists-neg-rewrite))
	    (not (or (exists-top-level-rewrite gwff1 v) (exists-top-level-rewrite gwff2 v)
		     (exists-top-level-rewrite wff1 v) (exists-top-level-rewrite wff2 v))))
      (if both-pos
	  (or (not (exists-neg-rewrite))
	      (not (or (exists-top-level-rewrite wff1 v) (exists-top-level-rewrite wff2 v))))
	(not (or (equal (head wff1) (head wff2)) ;have same head, or the head can be rewritten
		 (exists-top-level-rewrite wff1 v)
		 (exists-top-level-rewrite wff2 v)))))))

(defun exists-top-level-rewrite (wff varlist)
  ;;need not be able to rewrite the top level all at once -- maybe we need some lower-level
  ;;rewrites first -- but there should be a rewrite of the same type as the whole wff, and 
  ;;its head should be either the same symbol or a variable.
  (let ((tp (type wff))
	(hd (head wff))
	rtp rhd)
  (dolist (r *active-rewrites* nil)
    (setq rtp (type (get r 'before)) rhd (head (get r 'before)))
    (when (and (equal tp rtp)
	       (or (equal hd rhd)
		   (member rhd varlist)))
      (return t))
    (setq rtp (type (get r 'after)) rhd (head (get r 'after)))
    (when (and (equal tp rtp)
	       (or (equal hd rhd)
		   (member rhd varlist)))
      (return t)))))

(defun exists-neg-rewrite ()
  ;;there is a top-level rewrite, and there is a rewrite that introduces or deletes a negation
  ;;Need not be the same rewrite... perhaps there's a chain of rewrites.
  (dolist (r *active-rewrites* nil)
    (when (or (and (not-p (get r 'before)) (not (not-p (get r 'after))))
	      (and (not (not-p (get r 'before))) (not-p (get r 'after))))
      (return t))))

