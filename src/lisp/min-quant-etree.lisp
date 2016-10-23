;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of ms90-3)

(deffile min-quant-etree
  (extension lisp)
  (part-of ms90-3)
  (mhelp "Contains functions for minimizing quantifier scopes in
primsubs appearing in expansion trees. This allows the corresponding
instantiation terms in the ND proof to be in non-prenex form.  When
flag MIN-QUANT-ETREE is T, these functions are applied after searching
is done and before propositional proof checker starts."))

(defun elim-extra-exp-terms (expansion)
   (let (newterms (coms* (etree-components* expansion)))
     (do ((coms (etree-components expansion) (cdr coms))
          (terms (expansion-terms expansion) (cdr terms)))
         ((null coms*) (nreverse newterms))
       (when (eq (car coms) (car coms*))
             (pop coms*)
             (push (car terms) newterms)))))

(defun cleanup-all-inactive-nodes (etree)
   (let ((newcomponents (etree-components* etree)))
     (if (expansion-p etree) 
	 (setf (expansion-terms etree) (elim-extra-exp-terms etree)))
     (setf (etree-components etree) newcomponents)
     (mapc #'cleanup-all-inactive-nodes newcomponents)))
   
(defun min-quant-etree (etree)
   (cond ((leaf-p etree)
;;;since some exp-vars are instantiated, only handling expansion nodes
;;;is not enough.
          (if (mqs-applicable (leaf-shallow etree))
	      (setf (leaf-shallow etree) (min-quant-scope (leaf-shallow etree))))
	  etree)
         ((or (expansion-p etree) (skolem-p etree))
          (mapc #'min-quant-etree (etree-components etree))
          (one-step-mqe-bd etree))
         ((or (econjunction-p etree) (edisjunction-p etree) (implication-p etree))
          (mapc #'min-quant-etree (etree-components etree))
          (one-step-mqe-infix etree))
         (t (mapc #'min-quant-etree (etree-components etree)))))

(defun modular-over-rew (etree)
   (do ((node etree))
       ((not (rewrite-p node)) node)
       (setq node (car (etree-components node)))))

(defun rep-of-sons (etree)
  (car (etree-components etree)))

(defun merge-of-components (components)
   (let ((rep (car components)))
     (if (null (cdr components))
	 rep
         (cond ((expansion-p rep)
                (setf (expansion-terms rep) (mapcan #'expansion-terms components))
		(setf (etree-components rep) (mapcan #'etree-components components))
		(dolist (com (cdr components))
			(dolist (subcom (etree-components com))
				(setf (etree-parent subcom) rep)))
		rep)
               ((skolem-p rep) 
		(multiple-value-bind (oldsk newsk)
 	           (create-new-skfn rep)
		   (replace-old-with-new-skterm oldsk newsk components))
		rep)
	       (T (let ((comlist (mapcar #'etree-components components)))
		    (do ((list comlist (mapcar #'cdr list)))
			((null (car list)) rep)
			(merge-of-components (mapcar #'car list)))))))))

;;;rep's components have to be reset after applying the following function.
;;;applying setf-etree-components immediately after applying exchange-position.
(defun exchange-position (etree rep components)
   (let ((parent (etree-parent etree))
         (rparent (etree-parent rep)))
     (if parent 
	 (progn (setf (etree-parent rep) parent) 
		(rplaca (member etree (etree-components parent)) rep))
         (progn (setf (etree-parent rep) nil)
                (setf (eproof-etree current-eproof) rep)))
     (setf (etree-parent etree) rep)
     (dolist (com components) 
       (setf (etree-parent com) rparent))))

(defun setf-etree-components (etree rep components)
   (let (newcomponents)
     (dolist (com components)
       (do ((parent (etree-parent com) (etree-parent parent)))
           ((eq parent rep) (push com newcomponents))))
     (setf (etree-components etree) (nreverse newcomponents))))

;;;Using the following function may result in inefficiency; But under
;;;some weird situation, it might be necessary. Currently, I cannot
;;;give a good example to show this, ... I really wish that it would be
;;;my hallucination. Sigh!

(defun one-step-mqe-bd (etree)
  (let* ((exp (expansion-p etree))
#+comment(shallow (if exp (expansion-shallow etree) (skolem-shallow etree)))
         (shallow (strip-exp-vars (if exp (expansion-shallow etree) (skolem-shallow etree))))
         (head (car shallow))
         (scope (core::min-quant-scope (cdr shallow))))
    (cond ((not-p scope)
           (let* ((components (mapcar #'modular-over-rew (etree-components etree)))
                  (rep (car components))
                  (newcomponents (mapcar #'rep-of-sons components)))
              (if exp
                  (setf (expansion-shallow etree) (cons (cohead head) (cdr scope)))
                  (setf (skolem-shallow etree) (cons (cohead head) (cdr scope))))
              (exchange-position etree rep newcomponents)
              (setf-etree-components etree rep newcomponents)
              (setf (etree-components rep) (list etree))
              (one-step-mqe-bd etree)))
	  ((and (infix-p scope) (memq (caar scope) '(and or implies)))
           (let ((left (free-in (car head) (cdar scope)))
		 (right (free-in (car head) (cdr scope))))
             (cond ((and left right)
	            (cond ((or (and (eq (cdr head) 'forall) (eq (caar scope) 'and))
			       (and (eq (cdr head) 'exists) 
				    (or (eq (caar scope) 'or)
					(eq (caar scope) 'implies))))
			   (let* ((newetree (if exp (make-new-expansion etree) (make-new-skolem etree)))
				  (components (mapcar #'modular-over-rew (etree-components etree)))
				  (rep (car components))
				  (lcomponents 
				      (mapcar #'(lambda (x) (first (etree-components x)))
					      components))
				  (rcomponents 
				      (mapcar #'(lambda (x) (second (etree-components x)))
					      components)))

			     (if exp
				 (case (caar scope)
				   ((and or)
				    (setf (expansion-shallow etree) 
					  (cons head (cdar scope)))
				    (setf (expansion-shallow newetree) 
					  (cons head (cdr scope))))
				   (implies 
				    (setf (expansion-shallow etree) 
					  (cons (cohead head) (cdar scope)))
				    (setf (expansion-shallow etree) 
					  (cons head (cdr scope)))))
			         (case (caar scope)
				   ((and or)
				    (setf (skolem-shallow etree) 
					  (cons head (cdar scope)))
				    (setf (skolem-shallow newetree) 
					  (cons head (cdr scope))))
				   (implies 
				    (setf (skolem-shallow etree) 
					  (cons (cohead head) (cdar scope)))
				    (setf (skolem-shallow etree) 
					  (cons head (cdr scope))))))

			     (exchange-position etree rep lcomponents)
			     (setf (etree-components rep) (list etree newetree))
			     (setf-etree-components etree rep lcomponents)
                             (do ((lcoms lcomponents (cdr lcoms))
				  (rcoms rcomponents (cdr rcoms)))
                                 ((null lcoms))
				 (let ((lcom (car lcoms))
				       (rcom (car rcoms)))
				   (do ((lcom lcom (etree-parent lcom))
					(rcom rcom (etree-parent rcom)))
                                       ((eq (etree-parent lcom) etree) 
				        (setf (etree-parent rcom) newetree))
                                     (let ((new (make-new-rewrite (etree-parent lcom))))
                                       (setf (etree-components new) (list rcom))
				       (setf (etree-parent rcom) new)))))
			     (setf (etree-parent newetree) rep)
			     (setf-etree-components newetree newetree rcomponents)
			     (one-step-mqe-bd etree) 
			     (one-step-mqe-bd newetree)))
			  (T (if exp 
				 (setf (expansion-shallow etree) (cons head scope))
			         (setf (skolem-shallow etree) (cons head scope)))
                             etree)))
		    ((or left right)
		     (let* ((components (mapcar #'modular-over-rew (etree-components etree)))
			    (rep (car components))
			    (lcomponents 
			        (mapcar #'(lambda (x) (first (etree-components x)))
					components))
			    (rcomponents 
			        (mapcar #'(lambda (x) (second (etree-components x)))
					components)))

			     (if exp
				 (case (caar scope)
				   ((and or)
				    (setf (expansion-shallow etree) 
					  (cons head (if left (cdar scope) (cdr scope)))))
				   (implies 
				    (setf (expansion-shallow etree) 
					  (if left 
					      (cons (cohead head) (cdar scope))
					      (cons head (cdr scope))))))
			         (case (caar scope)
				   ((and or)
				    (setf (skolem-shallow etree) 
					  (cons head (if left (cdar scope) (cdr scope)))))
				   (implies 
				    (setf (skolem-shallow etree) 
					  (if left 
					      (cons (cohead head) (cdar scope))
					      (cons head (cdr scope)))))))

			     (exchange-position etree rep (if left lcomponents rcomponents))
                             (setf-etree-components etree rep (if left lcomponents rcomponents))
                             (setf (etree-components rep) 
				   (if left 
				       (list etree (merge-of-components rcomponents)) 
				       (list (merge-of-components lcomponents) etree)))
			     (do ((lcoms (if left lcomponents rcomponents) (cdr lcoms))
				  (rcoms (if left rcomponents lcomponents) (cdr rcoms)))
                                 ((null lcoms))
				 (let ((lcom (car lcoms))
				       (rcom (car rcoms)))
				   (do ((lcom lcom (etree-parent lcom))
					(rcom rcom (etree-parent rcom)))
                                       ((eq (etree-parent lcom) etree) 
				        (setf (etree-parent rcom) rep))
                                     (let ((new (make-new-rewrite (etree-parent lcom))))
                                       (setf (etree-components new) (list rcom))
				       (setf (etree-parent rcom) new)))))

			     (one-step-mqe-bd etree)))

		    (T (let ((parent (etree-parent etree))
			     (rep (car (etree-components etree))))
			 (if parent 
			     (progn (setf (etree-parent rep) parent) 
				    (rplaca (member etree (etree-components parent)) rep))
			     (progn (setf (etree-parent rep) nil)
				    (setf (eproof-etree current-eproof) rep))))))))
	  (T (if exp 
		 (setf (expansion-shallow etree) (min-quant-scope shallow))
	         (setf (skolem-shallow etree) (min-quant-scope shallow)))))))

(defun create-new-skfn (sknode)
   (let ((head (do ((hd (car (skolem-term-term (car (skolem-terms sknode)))) (car hd)))
		   ((symbolp hd) hd)))
	 (var (caar (strip-exp-vars (skolem-shallow sknode)))))
     (values head (funcall name-skolem-fn var (car (get head 'type))))))

;;;Please don't trust the following function. It relies thoroghly on
;;;the machine internal storage of some data structures. Now it is
;;;a miracle that the function is working!
(defun rpla-head-in-skterm (oldhead head term)
  (cond ((exp-var-p term)
	 (rpla-head-in-skterm oldhead head (exp-var-subst term)))
	((skolem-term-p term) 
;;;Cannot break skolem terms before prettifying is done.
	 (rpla-head-in-skterm oldhead head (skolem-term-term term))
	 term)
        ((consp term) 
	 (let ((elt (list term)))
	   (rpla-head-in-skterm oldhead head (cdar elt))
	   (do ((hd (caar elt) (car hd)))
	       ((symbolp hd) 
		(if (eq hd oldhead) (rplaca elt head)))
	       (setq elt (car elt))
	       (rpla-head-in-skterm oldhead head (cdr hd)))))
	(T term)))


(defun replace-old-with-new-skterm (oldhead head skcoms)
  (dolist (skcom skcoms)
     (rpla-head-in-skterm oldhead head (car (skolem-terms skcom)))))

(defun mqe-delete-negation (node)
  (let (nodelist)
    (do ((node node (rep-of-sons node)))
	((negation-p node) 
         (let ((parent (etree-parent node))
	       (sons (etree-components node)))
	   (setf (etree-components parent) sons)
	   (setf (etree-parent (car sons)) parent)
	   (if nodelist (car (last nodelist)) (car sons))))
	(push node nodelist)
	(setf (rewrite-shallow node) (cdr (rewrite-shallow node))))))

(defun one-step-mqe-infix (etree)
   (cond ((econjunction-p etree) etree)
	 ((edisjunction-p etree) 
          (let* ((pair (etree-components etree))
		 (lson (car pair))
		 (rson (cadr pair)))
           (if (or (negation-p lson)
		   (and (rewrite-p lson) (not-p (rewrite-shallow lson))))
	       (let* ((lsonson (mqe-delete-negation lson))
		      (parent (etree-parent etree))
		      (new (make-implication :positive (etree-positive etree)
					     :junctive (etree-junctive etree)
					     :components (list lsonson rson)
					     :free-vars (etree-free-vars etree)
					     :status (etree-status etree)
					     :parent parent)))
		  (setf (gethash (etree-name new) (eproof-statuses current-eproof)) 1)
		  (if parent
		      (rplaca (member etree (etree-components parent)) new)
		      (setf (eproof-etree current-eproof) new))
                  (setf (etree-parent lsonson) new)
		  (setf (etree-parent rson) new)
		  (one-step-mqe-infix new))
	          (if (or (negation-p rson)
			  (and (rewrite-p rson) (not-p (rewrite-shallow rson))))
		      (let* ((rsonson  (mqe-delete-negation rson))
			     (parent (etree-parent etree))
			     (new (make-implication :positive (etree-positive etree)
						    :junctive (etree-junctive etree)
						    :components (list rsonson lson)
						    :free-vars (etree-free-vars etree)
						    :status (etree-status etree)
						    :parent parent)))
			 (setf (gethash (etree-name new) (eproof-statuses current-eproof)) 1)
			 (if parent
			     (rplaca (member etree (etree-components parent)) new)
			     (setf (eproof-etree current-eproof) new))
			 (setf (etree-parent lson) new)
			 (setf (etree-parent rsonson) new)
		         (one-step-mqe-infix new))
		       etree))))
	 ((implication-p etree)
	  (let* ((pair (etree-components etree))
		 (lson (car pair))
		 (rson (cadr pair)))
             (if (and (negation-p lson) (negation-p rson))
		 (let ((lsonson (rep-of-sons lson))
		       (rsonson (rep-of-sons rson)))
		   (setf (etree-components etree) (list rsonson lsonson))
		   (setf (etree-parent lsonson) etree)
		   (setf (etree-parent rsonson) etree)
		   (one-step-mqe-infix etree))
	         etree)))))
		      
(defun make-new-expansion (etree)
  (let ((new (make-expansion  :positive (etree-positive etree)
			      :junctive 'con
			      :components (etree-components etree)
			      :free-vars (etree-free-vars etree)
			      :status (etree-status etree)
			      :terms (expansion-terms etree)
			      :prim-vars (expansion-prim-vars etree))))
     (setf (gethash (etree-name new) (eproof-statuses current-eproof)) 1)
     new))

(defun make-new-skolem (etree)
   (let ((new (make-skolem :positive (etree-positive etree)
			   :junctive 'con
			   :free-vars (etree-free-vars etree)
			   :status (etree-status etree)
			   :terms (skolem-terms etree))))
     (setf (gethash (etree-name new) (eproof-statuses current-eproof)) 1)
     new)) 

(defun make-new-rewrite (etree)
   (let ((new (make-rewrite :positive (etree-positive etree)
			    :junctive 'neutral
			    :free-vars (etree-free-vars etree)
			    :status (etree-status etree)
			    :justification (rewrite-justification etree))))
     (setf (gethash (etree-name new) (eproof-statuses current-eproof)) 1)
     new))

(defflag min-quant-etree
  (flagtype boolean)
  (default T)
;;;some day I may make distinction between beta and eta conversions
;;;while min-quant-etree is T. I just got tired of playing with the
;;;flag. SIGH!
  (change-fn (lambda (flag value pvalue) 
                     (declare (ignore flag))
                     (if (neq value pvalue)
			 (if value (setq lambda-conv 'beta-eta-together)))))
  (subjects etrees mating-search ms90-3 ms90-9 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "Only affects path-focused search procedures. When this flag
is T, the scope of quantifiers is minimized in primsubs appearing in
the expansion proof after searching is done and before the
propositional proof checker starts. This allows the corresponding
instantiation terms in the ND proof to be in non-prenex form, often
giving more readable proofs."))

(defun min-quant-etree-app (etree)
   (if (expansion-p etree)
       (do ((terms (expansion-terms etree) (cdr terms))
	    (components (etree-components etree) (cdr components)))
	   ((null components))
	 (if (mqs-applicable (car terms))
           (let ((com (car components))
		 (shallow (expansion-shallow etree))
	         (newterm (min-quant-scope (car terms))))
	     (rplaca terms newterm)
	     (mqe-app (modular-over-rew com) (caar shallow)
		      newterm (cdr shallow))
		      (min-quant-etree-app com))
	   (min-quant-etree-app (car components))))
       (mapc #'min-quant-etree-app (etree-components etree))))

(defun mqe-app (com var subst scope)
  (if (free-in var scope)
      (if (non-atomic scope)
	  (let ((sons (mapcar #'modular-over-rew (etree-components com))))
	    (cond ((not-p scope)
		   (mqe-app (car sons) var subst (cdr scope)))
		  ((infix-p scope) 
		   (mqe-app (car sons) var subst (cdar scope))
		   (mqe-app (cadr sons) var subst (cdr scope)))
		  ((boundwff-p scope)
		   (dolist (son sons 
			     (if (expansion-p com)
				 (setf (expansion-shallow com) 
				       (lnorm (subst subst var scope)))
			         (setf (skolem-shallow com) 
				       (lnorm (subst subst var scope)))))
			   (mqe-app son var subst (cdr scope))))))
	  (min-quant-etree com))))
	  
(eval-when (compile load eval)
(defmacro lambda-rew (node)
  `(and (rewrite-p ,node) (eq (rewrite-justification ,node) 'lambda)))
)

;;The function should be applied to nodes under an expansion node.
(defun elim-lambda-rew (etree)
  (cond ((lambda-rew etree)
	 (let ((son (car (etree-components etree)))
	       (parent (etree-parent etree)))
	   (setf (etree-parent son) parent)
	   (rplaca (member etree (etree-components parent)) son)
	   (elim-lambda-rew son)))
        ((rewrite-p etree)
         (setf (rewrite-shallow etree) (lnorm (rewrite-shallow etree)))
         (mapc #'elim-lambda-rew (etree-components etree)))
	((expansion-p etree) 
	 (setf (expansion-shallow etree) (lnorm (expansion-shallow etree)))
         (mapc #'elim-lambda-rew (etree-components etree)))
	((selection-p etree)
	 (setf (selection-shallow etree) (lnorm (selection-shallow etree)))
         (mapc #'elim-lambda-rew (etree-components etree)))
        ((skolem-p etree)
	 (setf (skolem-shallow etree) (lnorm (skolem-shallow etree)))
         (mapc #'elim-lambda-rew (etree-components etree)))
	((leaf-p etree)
	 (setf (leaf-shallow etree) (lnorm (leaf-shallow etree))))
	(T (mapc #'elim-lambda-rew (etree-components etree)))))

(defun add-lambda-rew (etree)
  (cond ((expansion-p etree)
	 (let ((shallow (expansion-shallow etree)))
	   (do ((components (etree-components etree) (cdr components))
		(terms (expansion-terms etree) (cdr terms)))
	       ((null components))
	     (let ((com (car components)))
	       (if (and (lambda-bd-p (car terms))
			(funvar (caar shallow) (cdr shallow)))
		   (let ((rew (make-rewrite :positive (etree-positive etree)
					    :free-vars (etree-free-vars etree)
					    :junctive 'neutral
					    :parent etree
					    :components (list com)
					    :shallow (subst (car terms) (caar shallow) (cdr shallow))
					    :status (etree-status etree)
					    :justification 'lambda)))
		     (setf (gethash (etree-name rew) (eproof-statuses current-eproof)) 1)
		     (setf (etree-parent com) rew)
		     (rplaca (member com (etree-components etree)) rew)
		     (add-ruleq-rew rew)))
	       (add-lambda-rew com)))))
	((rewrite-p etree)
	 (let ((shallow (rewrite-shallow etree))
	       (son (rep-of-sons etree)))
	   (case (rewrite-justification etree)
	      (ext= (let* ((newshallow (let ((rewrite-equalities 'auto::all)) (expand-top= shallow)))
			   (rew (make-rewrite :positive (etree-positive etree)
					      :free-vars (etree-free-vars etree)
					      :junctive 'neutral
					      :parent etree
					      :components (list son)
					      :shallow newshallow
					      :status (etree-status etree)
					      :justification 'lambda)))
		      (setf (gethash (etree-name rew) (eproof-statuses current-eproof)) 1)
		      (setf (etree-components etree) (list rew)
			    (etree-parent son) rew)))
	      ((subst= leibniz=)))
	   (add-lambda-rew son)))
	(T (mapc #'add-lambda-rew (etree-components etree)))))

(defun funvar (var wff)
   (if (consp wff)
       (if (eq var (car wff)) T
	   (or (funvar var (car wff)) (funvar var (cdr wff))))
       nil))


(defun add-ruleq-rew (lambda)
  (let* ((son (rep-of-sons lambda))
	 (ssh (get-shallow son))
	 (nsh (lnorm (rewrite-shallow lambda))))
    (unless (wffeq ssh nsh)
       (let ((rew (make-rewrite :positive (etree-positive lambda)
				:free-vars (etree-free-vars lambda)
				:junctive 'neutral
				:parent lambda
				:components (etree-components lambda)
				:shallow nsh
				:status (etree-status lambda)
				:justification 'ruleq
				:ruleq-shallow (strip-exp-vars ssh))))
	 (setf (gethash (etree-name rew) (eproof-statuses current-eproof)) 1)
	 (setf (etree-parent son) rew)
	 (setf (etree-components lambda) (list rew))))))


(defun elim-lambda-rew-app (etree)
  (if (expansion-p etree)
      (elim-lambda-rew etree)
      (mapc #'elim-lambda-rew-app (etree-components etree))))

(defun add-lambda-rew-app (etree)
  (if (expansion-p etree)
      (add-lambda-rew etree)
      (mapc #'add-lambda-rew-app (etree-components etree))))

       

