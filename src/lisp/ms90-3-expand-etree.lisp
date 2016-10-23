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

(deffile ms90-3-expand-etree
  (extension lisp)
  (part-of ms90-3)
  (mhelp "Contains functions for converting from a jform created by
ms90-3 to an expansion tree."))

;;; Written by Dan Nesmith
;;; Assumption: ms90-3-jform has not been rearranged from what it
;;; would be if generated directly from the expansion tree.  We rely
;;; on the order of the universal-nodes and expansion nodes in the
;;; jform and etree, respectively, remaining the same.

(context ms90-3)

(defmateop expand-etree
  (mate-result-> ignore)
  (mhelp "Convert the jform proof found by path-focused duplication
procedures MS90-3 and MS90-9 into an expansion proof."))

(defvar *exp-etree-jform-to-etree-subst-list* nil)
(defvar ms90-3-mating)

;;;The following variable is used to fix a bug, which often occurs
;;;when (as least) one component of a disjunction is a UNIVERSAL.
;;;Check function ADD-EXP-NODE-JFORM.
(defvar *changed-parents*)

(defvar *instantiate-fiddle* nil)

(defun expand-etree ()
  (declare (special min-quant-etree *instantiate-fiddle*))
  (setq *instantiate-fiddle* nil)
  (let ((etree (eproof-etree current-eproof))
	(real-renumber-leaves renumber-leaves)
	(renumber-leaves nil)
	(*instantiate-fiddle* t))
    (setq *exp-etree-jform-to-etree-subst-list* nil)
    (setq *changed-parents* nil)
;;;This is used designed to prove the jform consisting
;;;of only the empty disjunction.
    (unless ms90-3-mating 
            (initialize-mating-search)
            (setq active-mating (car (mating-list)))
            (setf (mating-completep active-mating) t)
            (return-from expand-etree T))
    (unless (eproof-skolem-method current-eproof)
	    (msg t "SKOLEM-DEFAULT is NIL; attempting to construct etree" t)
	    (construct-ms88-mating ms90-3-mating dup-record)
	    (return-from expand-etree T))
    (msg t "Constructing jform with final substitutions applied" t)
    (stringdtl)
    (finish-output)
    (setq ms90-3-jform
	  (find-all-dups ms90-3-jform dup-record (unode-substs
						  unif-prob)
			 (unode-fo-mode unif-prob)))
    (msg t "Duplicating expansion tree from the jform" t)
    (stringdtl)
    (finish-output)
    (exp-etree-jform-to-etree (make-exp-etree-jform ms90-3-jform nil) etree)
    (msg t "Attaching expansion terms to the expansion tree" t)
    (stringdtl)
    (finish-output)
    (dolist (sub (remove-duplicates 
		  (skolemize-free-vars-in-subst-list
		   (make-subst-list
		    (make-mating :utree 
				 *exp-etree-jform-to-etree-subst-list*)))
		 :key #'car))
      (substitute-in-etree (cdr sub) (car sub) etree nil))
    (when (lazy2-used) (unfiddle-with-def-leaves2 etree))
    (deepen-to-literals etree)
    (when (lazy2-used) (fiddle-with-def-leaves etree))
    (rm-unused-exps etree)
    (when (and min-quant-etree (not first-order-mode-ms))
       (msg "Put primitive substitutions in their mini-quantifier-scope form.")
       (stringdtl)
       (finish-output)
       (cleanup-all-inactive-nodes etree)
       (elim-lambda-rew-app etree)
       (min-quant-etree-app etree)
       (add-lambda-rew-app etree)
       (setq current-topnode (eproof-etree current-eproof))
       (setq etree current-topnode))
    (setq renumber-leaves real-renumber-leaves)
    (let ((prop-jform (etree-to-prop-jform etree)))
      (when (neq mating-verbose 'silent)
            (msg  "Calling propositional search to reconstruct the mating" t)
            (display-vp-diag prop-jform)
            (finish-output)
            (stringdtl)
            (finish-output))
      (setq ms90-3-mating (prop-msearch prop-jform)))

    (when (neq mating-verbose 'silent)
          (msg  "Reorganizing mating in ms88 form" t)
          (stringdtl)
          (finish-output))

    (unless (eq ms90-3-mating 'fail)
      (initialize-mating-search)
      (setq active-mating (car (mating-list)))
      (setf (mating-completep active-mating) t)
      (dolist (conn ms90-3-mating)
        (let ((connection (find-potential-connection
                           (car conn) (list (cdr conn)) (cgraph) nil)))
          (add-connection connection active-mating)))
    
      ms90-3-mating)))

;;; Unification may have returned some substitutions with dummy
;;; variables. We do not want them to be interpreted as new exp-vars,
;;; so we get rid of them by making them skolem terms.

(defun skolemize-free-vars-in-subst-list (subst-list) 
  (dolist (sub subst-list subst-list) 
    (let ((free-vars (free-vars-of (cdr sub))))
      (dolist (var free-vars)
	(unless (or (exp-var-p var) (skolem-term-p var)
		    (assoc var 
			   (eproof-free-vars-in-etree master-eproof)
			   :key #'exp-var-var)
		    (member var
			    (eproof-substitution-list master-eproof)
			    :key #'exp-var-var)
		    (assoc var
			   (eproof-skolem-constants master-eproof)
			   :key #'skolem-term-parameter)
		    (assoc var
			    (eproof-skolem-constants master-eproof)
			    :key #'(lambda (x)
				     (head (skolem-term-term x)))))
	  (push (cons (make-skolem-term :parameter var 
					:term var) 
		      0)
		(eproof-skolem-constants master-eproof)))))))

;;; We want to get rid of any expansions in the tree that were not
;;; used by ms90-3 in getting the proof, otherwise when we call the
;;; propositional prover, they may get used there.  So any expansion
;;; term that contains variables that have not been substituted for
;;; gets trashed.  See function above.

(defun rm-unused-exps (etree)
  (if (expansion-p etree)
      (let ((terms (sel-exp-terms etree))
	    (kids (etree-components* etree)))
	(do ((terms terms (cdr terms))
	     (kids kids (cdr kids))
	     (term (car terms) (car terms))
	     (kid (car kids) (car kids)))
	    ((null term))
	  (when (and (substitutable-vars-of term)
		     (not (zerop (etree-status* kid))))
	    (modify-status 0 kid)))
	(mapc #'rm-unused-exps (etree-components* etree)))
    (mapc #'rm-unused-exps (etree-components* etree))))

(defun exp-etree-jform-to-etree (jform etree)
  (let ((outer-univ-parents
	 (find-outer-univ-parents jform))
	(outer-exp-nodes
	 (find-outer-exp-nodes etree)))
    (do* ((univ-parents outer-univ-parents (cdr univ-parents))
	  (exp-nodes outer-exp-nodes (cdr exp-nodes))
	  (univ-parent (car univ-parents) (car univ-parents))
	  (exp-node (car exp-nodes) (car exp-nodes)))
	((null univ-parent) etree)
      (dolist (univ-node-list (divvy-up-univ-nodes univ-parent))
	(let* ((first-node (car univ-node-list))
	       (real-nodes (if (universal-substs first-node) 
			       univ-node-list
			     (cdr univ-node-list)))
	       (vars (universal-qvars first-node))
	       (initial-exp-node 
		(qvar-to-exp-node (car vars)))
	       (subst 
		(find-if #'(lambda (term) (free-in (car vars) term))
			 (expansion-terms initial-exp-node)))
	       (new-child nil)
	       (new-subst nil)
	       (rename-alist nil))
	  (dolist (real-node real-nodes)
	    (duplicate-var exp-node)
	    (setq new-child (car (last (etree-components exp-node))))
;;;Why deepen new-child before substitution? This causes troubles
;;;when min-quantifier-scope is T.
;;;	    (deepen-to-literals new-child)
;;;	    (setq new-child (car (last (etree-components exp-node))))
	    (multiple-value-setq (new-subst rename-alist)
	      (make-new-subst subst vars))
	    (substitute-in-etree new-subst
				 (car (last (expansion-terms exp-node)))
				 new-child
				 nil)
	    (deepen-to-literals new-child)
	    (dolist (var vars)
	      (push (cons (cdr (assoc var rename-alist))
			  (subst-eta-reduction (get-subst-from-univ-node real-node var)))
		    *exp-etree-jform-to-etree-subst-list*))
	    (setq new-child (car (last (etree-components exp-node))))
	    (exp-etree-jform-to-etree (universal-scope real-node) new-child)))))))

(defun make-new-subst (subst vars)
  (let* ((subst (strip-exp-vars
		(if (exp-var-p subst) (exp-var-subst subst) subst)))
	 (rename-alist (mapcar #'cons vars (mapcar #'ren-var-x1 vars))))
    (values (simul-substitute-term-var rename-alist subst)
	    rename-alist)))

(defun subst-eta-reduction (term)
  (cond ((or (lsymbol-q term) (skolem-term-p term)) term)
        ((and (boundwff-q term) (eq (cdar term) 'lambda))
         (let ((bdvar (bdvar term))
               (scope (subst-eta-reduction (cdr term))))
            (if (or (lsymbol-q scope) (boundwff-q scope))
                (cons (car term) scope)
	      (if (and (eq bdvar (cdr scope))
		       (not (bdvar-free-in-term bdvar (car scope) nil)))
		  (car scope)
		(cons (car term) scope)))))
         (t (cons (subst-eta-reduction (car term)) 
                  (subst-eta-reduction (cdr term))))))

(defun get-subst-from-univ-node (univ-node var)
  (let* ((substs (cdr (universal-substs univ-node)))
	 (sub-vars (car (universal-substs univ-node)))
	 (subst (cdr (assoc (assoc var sub-vars) substs))))
    (if subst
	subst
      (let* ((new-const (generic-constant var))
	     (skol-const (assoc new-const (eproof-skolem-constants
					   master-eproof)
				:key #'skolem-term-parameter)))
	(if (and skol-const (zerop (cdr skol-const)))
	    (car skol-const)
	  (progn
	    (setq skol-const (make-skolem-term :term new-const
					       :parameter new-const))
	    (push (cons skol-const 0)
		  (eproof-skolem-constants
		   master-eproof))
	    skol-const))))))

(defun generic-constant (var)
  (let ((const (intern-str (format nil "a~A" (type-to-string (type var))))))
    (setf (get const 'type) (get var 'type))
    const)
)

;;; Here I'm assuming that the univ nodes will always occur below top
;;; level. This is pretty safe as long as we have more than one univ
;;; node at a level, which is how exp-to-jform makes them (remember
;;; that the first univ node is always just a place keeper.)

(defun find-outer-univ-parents (jform)
  (etypecase jform
    (literal nil)
    (disjunction
     (if (find-if #'universal-p (disjunction-components jform))
	 (list jform)
       (reduce 'append (mapcar #'find-outer-univ-parents 
			       (disjunction-components jform)))))
    (conjunction
     (if (find-if #'universal-p (conjunction-components jform))
	 (list jform)
       (reduce 'append (mapcar #'find-outer-univ-parents 
			       (conjunction-components jform)))))))

;;;to make sure that outer expansion nodes beginnig with
;;;quantifiers.
(defun find-outer-exp-nodes (etree)
  (if (and (expansion-p etree) (expansion-prim-vars etree))
      (list etree)
    (reduce 'append (mapcar #'find-outer-exp-nodes
			    (etree-components* etree)))))

(defun divvy-up-univ-nodes (parent)
  (let* ((kids (if (conjunction-p parent)
		   (conjunction-components parent)
		 (disjunction-components parent)))
	 (univ-kids (remove-if-not #'universal-p kids)))
    (do ((kid (car univ-kids) (car kids))
	 (kids (cdr univ-kids) (cdr kids))
	 (tmp nil)
	 (lists nil))
	((null kid) (nreverse (mapcar #'nreverse lists)))
      (setq tmp
	(assoc (universal-qvars kid)
	       lists :test #'equal :key #'universal-qvars))
      (setq lists 
	    (if tmp
		(substitute (cons kid tmp) tmp lists)
	      (cons (list kid) lists))))))


;;; Here we will change the jform so that 
;;; 1. Wherever a univ-node appears, all of its siblings will be univ
;;; nodes of the same expansion node.
;;; 2. A univ-node will contain only qvars which originate from the
;;; same expansion term.


(defun make-exp-etree-jform (jform parent)
  (typecase jform
    (disjunction
     (let ((new-disj (copy-disjunction jform)))
       (setf (jform-parent new-disj) parent)
       (setf (disjunction-components new-disj)
	 (mapcar #'(lambda (x) (make-exp-etree-jform x new-disj))
		 (disjunction-components jform)))
       new-disj))
    (literal (let ((new-lit (copy-literal jform)))
	       (setf (jform-parent new-lit) parent)
	       new-lit))
    (conjunction
     (let ((new-conj (copy-conjunction jform)))
       (setf (jform-parent new-conj) parent)
       (let ((univ-node-lists
	      (divvy-up-univ-nodes new-conj))
	     (new-kids nil))
	 (dolist (kid (conjunction-components jform)
		   (setf (conjunction-components new-conj)
		     (nreverse new-kids)))
	   (cond ((not (universal-p kid))
		  (push (make-exp-etree-jform kid new-conj)
			new-kids))
		 ((assoc kid univ-node-lists) ;first one
		  (let ((new-conj-kid
			 (make-conjunction :parent new-conj
					   :pos (not (jform-pos
						      new-conj))))
			(univ-node-list 
			 (assoc kid univ-node-lists)))
		    (setf (conjunction-components new-conj-kid)
		      (mapcar #'(lambda (x)
				  (make-exp-etree-jform x
							new-conj-kid))
			      univ-node-list))
		    (push new-conj-kid new-kids)))
		 (t nil))))
       (finish-conj-node new-conj)))
    (universal
     (labels ((make-qvar-lists (qvars exp-nodes)
		(unless (null qvars)
		  (let ((pos-diff 
			 (position-if #'(lambda (x) 
					  (not (eq (car exp-nodes) x)))
				      exp-nodes)))
		    (if pos-diff
			(cons (subseq qvars 0 pos-diff)
			      (make-qvar-lists (subseq qvars pos-diff)
					       (subseq exp-nodes pos-diff)))
		      (list qvars))))))
     (let* ((new-univ (copy-universal jform))
	    (qvars (universal-qvars new-univ))
	    (exp-nodes (mapcar #'qvar-to-exp-node qvars))
	    (qvar-lists
	     (reverse (make-qvar-lists qvars exp-nodes))))
       (setf (universal-scope new-univ)
	 (make-exp-etree-jform (universal-scope new-univ) new-univ))
       (dotimes (i (1- (length qvar-lists)))
	 (let* ((new-univ* (copy-universal new-univ))
		(new-conj (make-conjunction :components (list new-univ)
					    :parent new-univ*)))
	   (setf (universal-qvars new-univ) (nth i qvar-lists))
	   (setf (universal-scope new-univ*) new-conj)
	   (setf (jform-parent new-univ) new-conj)
	   (setq new-univ new-univ*)))
       (setf (universal-qvars new-univ) 
	 (car (last qvar-lists)))
       (setf (jform-parent new-univ) parent)
       new-univ)))))

(defun qvar-to-exp-node (qvar)
  (cdr (assoc qvar (eproof-free-vars-in-etree current-eproof)
	      :key #'exp-var-var)))
	 

    
;;; conj-node is a conjunction.  If it is the parent of conjunctions
;;; which are themselves the parents of universal nodes which come
;;; from the same expansion node, then we want to merge the child
;;; conjunctions together.  I.e., want to put all universal nodes
;;; which come from same expansion node together under the same
;;; conjunction node.

(defun finish-conj-node (conj-node)
  (let ((kids (conjunction-components conj-node))
	(finished-kids nil))
    (do* ((kid (car kids) (car kids))
	  (kids (cdr kids) (cdr kids))
	  (exp-node
	   (if (and (conjunction-p kid)
		    (universal-p (car (conjunction-components kid))))
	       (qvar-to-exp-node 
		(car (universal-qvars 
		      (car (conjunction-components kid))))))
	   (if (and (conjunction-p kid)
		    (universal-p (car (conjunction-components kid))))
	       (qvar-to-exp-node 
		(car (universal-qvars 
		      (car (conjunction-components kid)))))))
	  (earlier-kid
	   (if exp-node
	       (find-if #'(lambda (x)
			    (and (conjunction-p x)
				 (universal-p (car
					       (conjunction-components
						x)))
				 (eq exp-node
				     (qvar-to-exp-node 
				      (car (universal-qvars 
					    (car (conjunction-components
						  x))))))))
			finished-kids))
		     
	   (if exp-node
	       (find-if #'(lambda (x)
			    (and (conjunction-p x)
				 (universal-p (car
					       (conjunction-components
						x)))
				 (eq exp-node
				     (qvar-to-exp-node 
				      (car (universal-qvars 
					    (car (conjunction-components
						  x))))))))
			finished-kids))))
	((null kid) 
	 (setf (conjunction-components conj-node)
	   (reverse finished-kids))
	 (when (= 1 (length finished-kids))
	   (setf (conjunction-components conj-node)
	     (conjunction-components (car (conjunction-components
					   conj-node))))
	   (dolist (x (conjunction-components conj-node))
	     (setf (jform-parent x) conj-node)))
	 conj-node)
      (if exp-node
	  (if earlier-kid
	      (setf (conjunction-components earlier-kid)
		(append (conjunction-components earlier-kid)
			(conjunction-components kid)))
	    (push kid finished-kids))
	(push kid finished-kids)))))

(defun construct-ms88-mating (ms90-3-mating dup-record)
;  (dolist (d dup-record) (msgf d))
;  (msgf t)
;  (dolist (m ms90-3-mating) (msgf m))
  (initialize-mating-search)
  (do ((dup-record (nreverse dup-record) (cdr dup-record))
       (renumber-leaves t)
       (current 0)
       (dup-numbers (list (cons -2 (eproof-etree current-eproof)))))
      ((or (null dup-record)
           (and (consp (car dup-record))
                (eq (cdar dup-record) ms90-3-jform)))
       (deepen-to-literals (eproof-etree current-eproof))
       (generate-new-mating ms90-3-mating dup-numbers))
      (let ((elt (car dup-record)))
	(when (and (consp elt) (numberp (cdr elt)))
	      (setq current (cdr elt)))
	(when (and (listp elt) (not (numberp (cdr elt))) 
		   (not (jform-p elt)) (represents-expansion-node elt))
	      (let ((expnode (find-etree-node #'(lambda (x) (and (expansion-p x)
								 (member (car elt)
									 (mapcar #'exp-var-var 
										 (expansion-terms x)))))
					      (eproof-etree current-eproof))))
		(when expnode (duplicate-var expnode)
		      (deepen-one (car (last (etree-components expnode))))
		      (push (cons (1+ current) (car (last (etree-components expnode)))) dup-numbers))))
	(when (and (jform-p elt) (eq (jform-type elt) 'universal))
	      (let ((expnode (find-etree-node #'(lambda (x) (and (expansion-p x)
								 (member (car (universal-qvars elt))
									 (mapcar #'exp-var-var 
										 (expansion-terms x)))))
					      (eproof-etree current-eproof))))
		(when expnode (duplicate-var expnode)
		      (deepen-one (car (last (etree-components expnode))))
		      (push (cons (cdadr dup-record) (car (last (etree-components expnode)))) dup-numbers))))))
  (when active-mating (complete-p))
  (unless (and active-mating (mating-completep active-mating))
	  (msgf "(BUG) Failed to reconstruct mating. Calling MS88 to patch it up.")
	  (ms88-patch-up ms90-3-mating dup-record))
  (if (and (mating-completep active-mating) 
	   (unifiable-p active-mating))
      (msgf "Done!")
    (throwfail t "Something broke... this is a bug with SKOLEM-DEFAULT NIL in ms90-3.")))


(defun generate-new-mating (ms90-3-mating dup-numbers)
  (cr-eproof-jform)
  (do ((mating ms90-3-mating (cddr mating))
       (max-mates 10000)
       (renumbering (mapcar #'(lambda (x) (cons (princ-to-string (cdr x)) (princ-to-string (car x))))
			    *renamed-leaves-list*)))
      ((null mating))
      (let* ((conn1 (caaar mating))
	     (conn1-poss (cons (princ-to-string (literal-name conn1))
			       (mapcar 'cdr (remove-if-not 
					     #'(lambda (x) (string-equal (literal-name conn1)
								    (string-right-trim "."
										       (string-right-trim "1234567890" 
													  (car x)))))
					     renumbering))))
	     (conn2 (cdaar mating))
	     (conn2-poss (cons (princ-to-string (literal-name conn2))
			       (mapcar 'cdr (remove-if-not 
					     #'(lambda (x) (string-equal (literal-name conn2)
								    (string-right-trim "."
										       (string-right-trim "1234567890" 
													  (car x)))))
					     renumbering))))
	     (num1list 
	      (mapcar 'find-etree-node-name 
		      (mapcar 'etree-name 
			      (list (cdar (remove-if #'(lambda (x) (> (car x) (caadr mating))) dup-numbers))))))
	     (num2list
	      (mapcar 'find-etree-node-name 
		      (mapcar 'etree-name 
			      (list (cdar
				     (remove-if #'(lambda (x) (> (car x) (cdadr mating))) dup-numbers))))))
	     (real-conn1 (remove-if 'null
				     (mapcar #'(lambda (y) 
						 (find-etree-node #'(lambda (x) 
								      (and (leaf-p x)
									   (member (leaf-name x) conn1-poss 
										   :test 'string-equal))) y))
					     num1list)))
	     (real-conn1 (or real-conn1 (list (find-etree-node #'(lambda (x) 
								   (and (leaf-p x)
									(member (leaf-name x) conn1-poss 
										:test 'string-equal))) 
							       (cdar (last dup-numbers))))))
	     (real-conn2 (remove-if 'null
				     (mapcar #'(lambda (y) 
						 (find-etree-node #'(lambda (x) 
								      (and (leaf-p x)
									   (member (leaf-name x) conn2-poss 
										   :test 'string-equal))) y))
					     num2list)))
	     (real-conn2 (or real-conn2 (list (find-etree-node #'(lambda (x) 
								   (and (leaf-p x)
									(member (leaf-name x) conn2-poss 
										:test 'string-equal))) 
							       (cdar (last dup-numbers)))))))
;	(msgf conn1 " : " conn2 t num1list " : " num2list t real-conn1 " : " real-conn2)
	(dolist (rc1 real-conn1)
		(dolist (rc2 real-conn2)
			(add-conn (leaf-name rc1) (leaf-name rc2)))))))

(defun represents-expansion-node (list &optional (jform ms90-3-jform))
  (case (jform-type jform)
	(universal (or (equal list (universal-qvars jform))
		       (represents-expansion-node list (universal-scope jform))))
	(conjunction (dolist (c (conjunction-components jform) nil)
			     (when (represents-expansion-node list c) (return t))))
	(disjunction (dolist (c (disjunction-components jform) nil)
			     (when (represents-expansion-node list c) (return t))))
	(literal nil)))

(defun ms88-patch-up (ms90-3-mating dup-record)
  (cr-eproof-jform)
  (clrhash (cgraph))
  (clrhash (connections-array))
  (initialize-mating-search)
  (do ((lits (find-etree-nodes #'leaf-p current-topnode) (cdr lits)))
      ((null lits))
      (dolist (l2 (cdr lits))
	      (setf (gethash (cons (leaf-name l2) (leaf-name (car lits))) (cgraph)) nil)
	      (setf (gethash (cons (leaf-name (car lits)) (leaf-name l2)) (cgraph)) nil)))
  (do ((mating ms90-3-mating (cddr mating))
       (renumbering (mapcar #'(lambda (x) (cons (cdr x) (car x)))
			    *renamed-leaves-list*)))
      ((null mating))
      (let* ((conn1 (caaar mating))
	     (conn1-poss (cons (literal-name conn1)
			       (mapcar 'cdr (remove-if-not 
					     #'(lambda (x) (string-equal (literal-name conn1)
									 (string-right-trim "."
											    (string-right-trim "1234567890" 
													       (car x)))))
					     renumbering))))
	     (conn2 (cdaar mating))
	     (conn2-poss (cons (literal-name conn2)
			       (mapcar 'cdr (remove-if-not 
					     #'(lambda (x) (string-equal (literal-name conn2)
									 (string-right-trim "."
											    (string-right-trim "1234567890" 
													       (car x)))))
					     renumbering)))))
	(dolist (c1 conn1-poss)
		(dolist (c2 conn2-poss)
			(msgf "Allowing " (show-other-name c1) ":" (show-other-name c2))
			(remhash (cons c1 c2) (cgraph))
			(remhash (cons c2 c1) (cgraph))))))
  (let ((max-mates 100)) (ms88-controller)))

