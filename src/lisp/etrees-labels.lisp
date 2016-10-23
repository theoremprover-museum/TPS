;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)

;(part-of expansion-tree)

;;; Changed the substitute-in-etree-main and simul-substitute-in-etree-main
;;; routines DAN 29OCT88


(context expansion-trees)
(deffile etrees-labels
  (part-of mating)
  (extension clisp)
  (mhelp "Defines flavors of expansion tree labels."))

;;; ETREE flavor contains things common to most or all of
;;; of the flavors of expansion tree nodes, and is included in
;;; all of them.  

(eval-when (compile load eval)
(defflavor etree
  (mhelp "Defines common properties of expansion tree nodes.")
  (structured t)
  (instance-attributes
   (name '|| :type symbol)
   components				; a node's children
   (positive nil) 			; true if node is positive in the formula
   (junctive nil :type symbol)		; whether node acts as neutral, conjunction, or disjunction
   free-vars				; expansion variables in whose scope node occurs, used for skolemizing
   parent				; parent of the node
   (leaf nil)				;the etree can be treated as a LEAF; this is mainly used in NAT-ETREE
					;display command is sensitive to this
   (predecessor nil)                    ;to keep track of nodes from which this node orignated when copying a subtree
   (status 1)
   (colors nil) ; 9/25/01
   )))

(eval-when (compile load eval)
(defflavor etree
  (printfn print-etree)
  (printwff (lambda (wff bracket depth)
	      (if print-nodenames (pp-symbol-space (etree-name wff))
		(printwff 
		 (if print-deep (get-deep wff)
		   (get-shallow wff))
		 bracket depth))))
  (prt-symbol-p (lambda (gwff) (declare (ignore gwff)) T))
  (prt-infix-op (lambda (gwff) (declare (ignore gwff)) nil))
  (prt-prefix-op (lambda (gwff) (declare (ignore gwff)) nil))
  (prt-associative-p (lambda (gwff) (declare (ignore gwff)) nil))
  (type (lambda (gwff) (declare (ignore gwff)) 'O))
  (gwff-p (lambda (gwff) (declare (ignore gwff)) T))
  (gwff-q (lambda (gwff) (declare (ignore gwff)) T))
  (legal-type-p1 (lambda (gwff) (declare (ignore gwff)) T))
  (infix-p (lambda (label) (declare (ignore label)) nil))
  (boundwff-p (lambda (label) (declare (ignore label)) nil))
  (gar (lambda (gwff) (gar (get-shallow gwff))))
  (gdr (lambda (gwff) (gdr (get-shallow gwff))))
  (glr (lambda (gwff) (car (etree-components gwff))))
  (grr (lambda (gwff) (cadr (etree-components gwff))))
  (free-vars-of (lambda (gwff) (etree-free-vars gwff)))
  (free-vars (lambda (gwff bindlist) (declare (ignore bindlist))
	       (etree-free-vars gwff)))
  (intern-subst (lambda (gwff var) (declare (ignore var)) gwff))
  (positive-p (lambda (label) (etree-positive label)))
  (topnode-p (lambda (label) (null (etree-parent label))))
  (duplicate (lambda (gwff) (throwfail (format nil "Can't duplicate a ~A node." 
					       (type-of gwff)))))
  (get-justification (lambda (label)
		       (throwfail (format nil "A ~A node doesn't have a justification."
					  (type-of label)))))
  (expand (lambda (term bdwff) (declare (ignore term))
	    (throwfail (format nil "Can't expand a ~A node." 
			       (type-of bdwff)))))
  (rewrite (lambda (label) 
	     (throwfail (format nil "Can't rewrite a ~A node (yet)." 
				(type-of label)))))
  (deepen (lambda (label) 
	    (throwfail (format nil "Can't deepen a ~A node." 
			       (type-of label)))))
  (strip-exp-vars-from-etree 
   (lambda (label) (declare (ignore label)) nil))
  (get-all-defns-rec (lambda (gwff) (declare (ignore gwff)) nil))
  (default-boolean-result (lambda (label) (get-shallow label)))
  ))

(defflavor edisjunction
  (mhelp "An edisjunction label stands for a disjunction node.")
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring edisj-name))))
  (instance-attributes )
  (infix-p (lambda (label) (declare (ignore label)) t))
  (substitute-in-etree-main
   (lambda (term var label)
     (mapc #'(lambda(x) (substitute-in-etree-main term var x))
	   (etree-components label))
     label))
  (simul-substitute-in-etree-main
   (lambda (theta label)
     (mapc #'(lambda(x) (simul-substitute-in-etree-main theta x))
	   (etree-components label))
     label))
  (get-shallow 
    (lambda (label)
      (let ((juncts (etree-components* label)))
	(cond 
	  ((cdr juncts)
	   (cons (cons 'or (get-shallow (car juncts)))
		  (get-shallow (cadr juncts))))
	  ((car juncts)
	   (get-shallow (car juncts)))
	  (t (if (positive-p label) 'falsehood 'truth ))))))
  (get-deep 
    (lambda (label)
      (let ((juncts (etree-components* label)))
	(cond 
	  ((cdr juncts)
	   (cons (cons 'or (get-deep (car juncts)))
		  (get-deep (cadr juncts))))
	  ((car juncts)
	   (get-deep (car juncts)))
	  (t (if (positive-p label) 'falsehood 'truth))))))
  (copy-etree (lambda (etree) (copy-edisjunction etree)))
)

(defvar econj-name)
(defflavor econjunction
  (mhelp "An econjunction label stands for a conjunction node.")
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring econj-name))))
  (instance-attributes )
  (infix-p (lambda (label) (declare (ignore label)) t))
;;; Expansion Trees
  (get-shallow 
    (lambda (label)
      (let ((juncts (etree-components* label)))
	(cond 
	  ((cdr juncts)
	   (cons (cons 'and (get-shallow (car juncts)))
		  (get-shallow (cadr juncts))))
	  ((car juncts)
	   (get-shallow (car juncts)))
	  (t (if (positive-p label) 'truth 'falsehood))))))
  (get-deep 
    (lambda (label)
      (let ((juncts (etree-components* label)))
	(cond 
	  ((cdr juncts)
	   (cons (cons 'and (get-deep (car juncts)))
		  (get-deep (cadr juncts))))
	  ((car juncts)
	   (get-deep (car juncts)))
	  (t (if (positive-p label) 'truth 'falsehood))))))
  (substitute-in-etree-main
   (lambda (term var label)
     (mapc #'(lambda(x) (substitute-in-etree-main term var x))
	   (etree-components label))
     label))
  (simul-substitute-in-etree-main
   (lambda (theta label)
     (mapc #'(lambda(x) (simul-substitute-in-etree-main theta x))
	   (etree-components label))
     label))
  (copy-etree (lambda (etree) (copy-econjunction etree)))
)

(eval-when (load compile eval)
(defflavor rewrite
  (mhelp "A rewrite node stands for a node which has been rewritten.")
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring rewrite-name))))
  (instance-attributes
   shallow
   justification
   ruleq-shallow
   reverse)))

(defflavor rewrite
  (get-shallow (lambda (label) (rewrite-shallow label)))
  (get-deep (lambda (label)
	      (let ((kids (etree-components* label)))
		(if kids (get-deep (car kids)) (get-shallow label)))))
  (substitute-in-etree-main
   (lambda (term var label)
     (let ((newwff (substitute-term-var-etree term var (rewrite-shallow label))))
       (setf (rewrite-shallow label) newwff)
       (if (rewrite-ruleq-shallow label)
	   (setf (rewrite-ruleq-shallow label)
                 (substitute-term-var-etree term var (rewrite-ruleq-shallow label))))
       (mapc #'(lambda (x) (substitute-in-etree-main term var x))
	     (etree-components label))
     label)))
  (simul-substitute-in-etree-main
   (lambda (theta label)
     (let ((newwff (simul-substitute-term-var-etree theta (rewrite-shallow label))))
       (mapc #'(lambda (x) (simul-substitute-in-etree-main theta x))
	     (etree-components label))
       (setf (rewrite-shallow label) newwff)
       (if (rewrite-ruleq-shallow label)
	   (setf (rewrite-ruleq-shallow label)
                 (simul-substitute-term-var-etree theta (rewrite-ruleq-shallow label))))    
       label)))
  (get-justification (lambda (label) (rewrite-justification label)))
  (rewrite (lambda (label) (declare (ignore label)) (throwfail "Not yet implemented.")))
  (strip-exp-vars-from-etree (lambda (label)
			       (setf (rewrite-shallow label) (strip-exp-vars (rewrite-shallow label)))))
  (copy-etree (lambda (etree) (copy-rewrite etree))))

(eval-when (load eval compile)
(defflavor leaf
  (mhelp "A leaf label stands for a leaf node of an etree.")
  (inherit-properties etree)
  (instance-attributes
   shallow)
  (include etree (name (intern-str (create-namestring leaf-name))))))

;;; Must do the defstruct for leaf before using access functions.

(eval-when (load eval compile)
(defflavor leaf
  (get-shallow (lambda (label) (leaf-shallow label)))
  (get-deep (lambda (label) (leaf-shallow label)))
  (not-p (lambda (label) (not-p (get-shallow label))))
  (gar (lambda (label) (gar (leaf-shallow label))))
  (gdr (lambda (label) (gdr (leaf-shallow label))))
  (glr (lambda (label) (glr (leaf-shallow label))))
  (grr (lambda (label) (grr (leaf-shallow label))))
;;; Primitive Substitution
;  (head-var-of (lambda (bdwff) (head-var-of (leaf-shallow bdwff))))
;  (head-vars-of (lambda (gwff) (head-vars-of (leaf-shallow gwff))))
;neither of these exist anywhere else... MB Thu Jul  3 19:14:52 1997
;;; Expansion trees
  (substitute-in-etree-main 
   (lambda (term var label)
     (let* ((new-shallow (substitute-term-var-etree term var (leaf-shallow label)))
	    (normed-shallow (lambda-norm new-shallow))
	    (rew-node nil))
     (setf (leaf-shallow label) normed-shallow)
     (unless (or (wffeq-ab-etree new-shallow normed-shallow)
		 (and (rewrite-p (etree-parent label))
		       (eq 'lambda (rewrite-justification 
				   (etree-parent label)))))
	     (setq rew-node (make-rewrite :shallow new-shallow 
					  :positive (positive-p label)
					  :justification 'lambda
					  :free-vars (etree-free-vars label)
					  :junctive 'neutral
					  :predecessor (etree-name label)
					  :parent (etree-parent label)))
	     (setf (etree-components rew-node) (list label))
	     (update-global-lists label rew-node)
	     (update-status nil rew-node (etree-status* label))
	     (push label (eproof-leaf-list current-eproof))
	     (setf (etree-parent label) rew-node)
	     (setq label rew-node))
     label)))
  (simul-substitute-in-etree-main 
   (lambda (theta label)
     (let* ((new-shallow (simul-substitute-term-var-etree
			   theta (leaf-shallow label)))
	    (normed-shallow (lambda-norm new-shallow))
	    (rew-node nil))
     (setf (leaf-shallow label) normed-shallow)
     (unless (or (wffeq-ab-etree new-shallow normed-shallow)
		 (and (rewrite-p (etree-parent label))
		       (eq 'lambda (rewrite-justification 
				     (etree-parent label)))))
       (setq rew-node (make-rewrite :shallow new-shallow 
				    :positive (positive-p label)
				    :justification 'lambda
				    :free-vars (etree-free-vars label)
				    :junctive 'neutral
				    :predecessor (etree-name label)
				    :parent (etree-parent label)))
       (update-global-lists label rew-node)
       (push label (eproof-leaf-list current-eproof))
       (update-status nil rew-node (etree-status* label))
       (setf (etree-parent label) rew-node)
       (setf (etree-components rew-node) (list label))
       (setq label rew-node))
     label)))
  ;;;Unification
  (hnf-rec
   (lambda (pos term args binder bdvars stack subst-stack)
     (hnf-rec pos (leaf-shallow term) args binder bdvars stack subst-stack)))
  (free-in-var-term
   (lambda (var term bdvars stack subst-stack rigid-path free-vars)
     (free-in-var-term var (leaf-shallow term) bdvars stack subst-stack
		       rigid-path free-vars)))
  (strip-exp-vars-from-etree 
   (lambda (label)
     (setf (leaf-shallow label) (strip-exp-vars (leaf-shallow
                                                    label)))))
  (copy-etree (lambda (etree) (copy-leaf etree)))))

(defflavor empty-dup-info
  (mhelp "EMPTY is solely used in translation part of code!")
  (inherit-properties etree)
  (instance-attributes shallow)
  (include etree (name (intern-str (create-namestring EMPTY-DUP-INFO-name))))
  (get-shallow (lambda (label) (empty-dup-info-shallow label)))
  (substitute-in-etree-main
   (lambda (term var label)
     (setf (empty-dup-info-shallow label)
	   (substitute-term-var-etree term var
				      (empty-dup-info-shallow label)))
     label))
  (simul-substitute-in-etree-main
   (lambda (theta label)
     (setf (empty-dup-info-shallow label)
	   (simul-substitute-term-var-etree theta
					    (empty-dup-info-shallow label)))
     label))
  (copy-etree (lambda (etree) (copy-empty-dup-info etree))))

(defflavor negation
  (mhelp "A negation label stands for a negation node.")
  (inherit-properties etree)
  (instance-attributes )
  (include etree (name (intern-str (create-namestring neg-name))))
  ;;; Expansion trees
  (gdr (lambda (label) (car (etree-components* label))))
  (get-shallow 
    (lambda (label)
      (let ((kid (car (etree-components* label))))
	(if kid (cons 'not (get-shallow kid)) 
	    (if (positive-p label) 'truth 'falsehood)))))
  (get-deep 
    (lambda (label)
      (let ((kid (car (etree-components* label))))
	(if kid (cons 'not (get-deep kid)) 
	    (if (positive-p label) 'truth 'falsehood)))))
  (substitute-in-etree-main
    (lambda (term var label)
      (mapc #'(lambda(x) (substitute-in-etree-main term var x))
	    (etree-components label))
      label))
  (simul-substitute-in-etree-main
    (lambda (theta label)
      (mapc #'(lambda(x) (simul-substitute-in-etree-main theta x))
	    (etree-components label))
      label))
  (copy-etree (lambda (etree) (copy-negation etree))))

(defflavor implication
  (mhelp "An implication node stands for an implication node.")
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring imp-name))))
  (instance-attributes )
  (infix-p (lambda (label) (declare (ignore label)) t))
  ;;; Expansion trees
  (get-shallow 
    (lambda (label)
      (let ((left (car (etree-components label)))
	    (right (cadr (etree-components label))))
	(cond ((and (not (zerop (etree-status* left)))
		    (not (zerop (etree-status* right))))
	       (cons (cons 'implies (get-shallow left))
				      (get-shallow right)))
	      ((not (zerop (etree-status* left)))
	       (cons 'not (get-shallow left)))
	      ((not (zerop (etree-status* right))) (get-shallow right))
	      (t (if (positive-p label) 'truth 'falsehood))))))
  (get-deep
    (lambda (label)
      (let ((left (car (etree-components label)))
	    (right (cadr (etree-components label))))
	(cond ((and 
		 (not (zerop (etree-status* left)))
		 (not (zerop (etree-status* right))))
	       (cons (cons 'implies (get-deep left))
		     (get-deep right)))
	      ((not (zerop (etree-status* left))) (cons 'not (get-deep left)))
	      ((not (zerop (etree-status* right))) (get-deep right))
	      (t (if (positive-p label) 'truth 'falsehood))))))
  (substitute-in-etree-main
   (lambda (term var label)
     (mapc #'(lambda (x) (substitute-in-etree-main term var x))
	   (etree-components label))
     label))
  (simul-substitute-in-etree-main
   (lambda (theta label)
     (mapc #'(lambda (x) (simul-substitute-in-etree-main theta x))
	   (etree-components label))
     label))
  (copy-etree (lambda (etree) (copy-implication etree))))

(defflavor true
  (mhelp "A true node stands for the logical constant TRUTH.")
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring true-name))))
  (instance-attributes )
  ;;; Expansion trees
  (get-shallow (lambda (label) (declare (ignore label)) 'TRUTH))
  (get-deep (lambda (label) (declare (ignore label)) 'TRUTH))
  (substitute-in-etree-main (lambda (term var label) 
			      (declare (ignore term var))
			      label))
  (simul-substitute-in-etree-main 
    (lambda (theta label) (declare (ignore theta)) label))
  (copy-etree (lambda (etree) (copy-true etree)))
)

(defflavor false
  (mhelp "A false node stands for the logical constant FALSEHOOD.")
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring false-name))))
  (instance-attributes )
  ;;; Expansion trees
  (get-shallow (lambda (label) (declare (ignore label)) 'FALSEHOOD))
  (get-deep (lambda (label) (declare (ignore label)) 'FALSEHOOD))
  (substitute-in-etree-main (lambda (term var label) 
			      (declare (ignore term var))
			      label))
  (simul-substitute-in-etree-main (lambda (theta label) 
				    (declare (ignore theta))
				    label))
  (copy-etree (lambda (etree) (copy-false etree)))
)


;;;;
;;;; The next section deals with selection nodes.
;;;;

(eval-when (load compile eval)
(defflavor selection
  (mhelp "A SELECTION label stands for a selection node in a (non-skolem)
expansion tree")
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring selection-name))))
  (instance-attributes
   shallow
   terms))
)

(defflavor selection
  (boundwff-p (lambda (label) (declare (ignore label)) t))
  (get-shallow (lambda (label) (selection-shallow label)))
  (get-deep
   (lambda (label)
     (let ((kid (car (etree-components* label))))
       (if kid (get-deep kid) (selection-shallow label)))))
  (substitute-in-etree-main
   (lambda (term var label)
     (setf (selection-shallow label) 
	   (substitute-term-var-etree term var (selection-shallow label)))
     (setf (selection-terms label)
	   (mapcar #'(lambda (sel-term)
		       (substitute-term-var-etree term var sel-term))
		   (selection-terms label)))
     (mapc #'(lambda (x) 
	       (substitute-in-etree-main term var x))
	   (etree-components label))
     label))
  (simul-substitute-in-etree-main
   (lambda (theta label)
     (setf (selection-shallow label)
	   (simul-substitute-term-var-etree theta
					    (selection-shallow label)))
     (setf (selection-terms label)
	   (mapcar #'(lambda (sel-term)
		       (simul-substitute-term-var-etree 
				      theta sel-term))
		   (selection-terms label)))
     (mapc #'(lambda (x) 
	       (simul-substitute-in-etree-main theta x))
	   (etree-components label))
     label))
  ;;
  (skolems1-main
   (lambda (gwff univflag govuniv exsubs)
     (declare (ignore gwff univflag govuniv exsubs))
     (throwfail "Don't skolemize when you have selection nodes!")))
  ;; The next section gives the default for test operations.
  ;; The next is useful for editor stepping functions
  (sel-exp-terms 
    (lambda (gwff) 
      (when (etree-components* gwff)
	(selection-terms gwff))))
  (prepare-for
   (lambda (var term tfree inwff occurs)
	    (declare (special occs val-occs)
		     (ignore var tfree))
     (cond ((eq term inwff)
	    (setq occs (1+ occs))
	    (and (complain "Shouldn't be doing this; I don't know what good-occs is!")
		 (good-occs occs occurs)
		 (setq val-occs (1+ val-occs)))
	    inwff)
	   (t inwff))))
  (intern-subst (lambda (gwff var)
		  (intern-subst (get-shallow gwff) var)))
  (strip-exp-vars-from-etree
   (lambda (label)
     (setf (selection-terms label)
           (mapcar #'strip-exp-vars (selection-terms label)))
     (setf (selection-shallow label)
           (strip-exp-vars (selection-shallow label)))))
  (copy-etree (lambda (etree) (copy-selection etree)))
  )

(eval-when (load eval compile)
(defflavor skolem
  (mhelp "A skolem node stands for a skolemized node in a (skolem) expansion
tree.")
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring skolem-selection-name))))
  (instance-attributes
   shallow
   terms)))

(defflavor skolem
  (boundwff-p (lambda (label) (declare (ignore label)) t))
  (get-shallow (lambda (label) (skolem-shallow label)))
  (get-deep
    (lambda (label)
      (let ((kid (car (etree-components* label))))
	(if kid (get-deep kid) (skolem-shallow label)))))
  (sel-exp-terms 
    (lambda (gwff) 
      (when (etree-components* gwff)
	(skolem-terms gwff))))
  (substitute-in-etree-main
   (lambda (term var label)
     (setf (skolem-shallow label) 
	   (substitute-term-var-etree term var (skolem-shallow label)))
     (setf (skolem-terms label)
	   (mapcar #'(lambda (sel-term)
		       (substitute-term-var-etree term var sel-term))
		   (skolem-terms label)))
     (mapc #'(lambda (x) 
	       (substitute-in-etree-main term var x))
	   (etree-components label))
     label))
  (simul-substitute-in-etree-main
   (lambda (theta label)
     (setf (skolem-shallow label)
	   (simul-substitute-term-var-etree theta
					    (skolem-shallow label)))
     (setf (skolem-terms label)
	   (mapcar #'(lambda (sel-term)
		       (simul-substitute-term-var-etree theta sel-term))
		   (skolem-terms label)))
     (mapc #'(lambda (x) 
	       (simul-substitute-in-etree-main theta x))
	   (etree-components label))
     label))
  (strip-exp-vars-from-etree
   (lambda (label)
     (setf (skolem-terms label)
           (mapcar #'(lambda (x) (if (skolem-term-p x)
                                     (progn (setf (skolem-term-term x)
                                                  (strip-exp-vars
                                                   (skolem-term-term x)))
                                            x)
                                     (strip-exp-vars x)))
                   (skolem-terms label)))
     (setf (skolem-shallow label)
           (strip-exp-vars (skolem-shallow label)))))
  (copy-etree (lambda (etree) (copy-skolem etree)))
)
;;;;
;;;; The next section deals with expansion nodes.
;;;;

(eval-when (load compile eval)
(defflavor expansion
  (mhelp "An EXPANSION label stands for an expansion node.")
  ;; The following section inherits everything from selections, except
  ;; what is explicitly specified.
  (inherit-properties etree)
  (include etree (name (intern-str (create-namestring expansion-name))))
  (instance-attributes 
   shallow
   (terms nil)
   ;;primitive substitutions
   (prim-vars nil)
   )))

(defflavor expansion
  ;; The next section has some expansion specific functions.
  (boundwff-p (lambda (label) (declare (ignore label)) t))
  (get-shallow (lambda (label) (expansion-shallow label)))
  (sel-exp-terms 
    (lambda (gwff) 
      (let ((kids (etree-components* gwff)))
	(mapcar #'(lambda (x) 
		    (nth (position x (etree-components gwff))
			 (expansion-terms gwff)))
		kids))))
;;; Assume that theta is not going to cause problems, i.e., the substitution
;;; are free for their variables, etc.
;;; We may have the case that an expansion term which is lambda-bd may
;;; require normalization of the child, so may need a rewrite node.
  (substitute-in-etree-main
   (lambda (term var label)
     (let ((newwff (substitute-term-var-etree term var (expansion-shallow label))))
       (mapc #'(lambda (x) (substitute-in-etree-main term var x))
	     (etree-components label))
       (setf (expansion-terms label)
	     (mapcar 
	      #'(lambda (wff)
		  (substitute-term-var-etree term var wff))
	      (expansion-terms label)))
       (setf (expansion-shallow label) newwff)
     label)))
  (simul-substitute-in-etree-main
   (lambda (theta label)
     (let* ((wff (expansion-shallow label))
	    (newwff nil))
       (setf (expansion-terms label)
	     (mapcar 
	       #'(lambda (x)
		   (simul-substitute-term-var-etree theta x))
	       (expansion-terms label)))
       (setq newwff 
	     (simul-substitute-term-var-etree theta wff))
       (setf (expansion-shallow label) newwff)
       (mapc #'(lambda (x) (simul-substitute-in-etree-main theta x))
	     (etree-components label))
      label)))
  (expand (lambda (term bdwff) (add-expansion bdwff term)))
  (get-deep
   (lambda (label)
     (let ((kids (etree-components* label)))
       (cond
	 ((cdr kids)
	  (deepify-list (mapcar #'get-deep kids) 
			(if (etree-positive label) 'and 'or))) ; cebrown 1/19/01
	 (kids
	   (get-deep (car kids)))
	 (t
	   (expansion-shallow label))))))
  (duplicate duplicate-expansion)
  (duplicate-var duplicate-expansion-var)
  (strip-exp-vars-from-etree
   (lambda (label)
     (setf (expansion-terms label)
           (mapcar #'strip-exp-vars (expansion-terms label)))
     (setf (expansion-shallow label)
           (strip-exp-vars (expansion-shallow label)))))
  (copy-etree (lambda (etree) (copy-expansion etree))))






