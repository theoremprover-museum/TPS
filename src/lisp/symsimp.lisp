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

(deffile symsimp
  (part-of etr-nat)
  (mhelp "Defines functions used for symmetric simplification."))


;;; Assumption: We have merged the proof, so that the mating-list is
;;; list of ordered pairs of strings.

(defun single-instantiation-eligible-p (exists-node nodes)
  "Non-nil if EXISTS-NODE is expansion node with exactly one term 
which is admissible. Returns the term if so, nil otherwise."
  (and (etree-p exists-node)
       (expansion-p exists-node)
       (let ((terms (expansion-terms exists-node)))
	 (and (= (length terms) 1)
	      (admissible-p* (car terms) nodes)))))

(defun mated-p* (node conn-list)
  "True only if NODE is mated in CONN-LIST.  CONN-LIST is assumed to 
be a list of ordered pairs of strings."
  (let ((name (etree-name node)))
    (dolist (conn conn-list nil)
      (if (or (string-equal name (car conn))
	      (string-equal name (cdr conn)))
	  (return t)))))


(defun admissible-p* (term nodes)
  (let* ((skol-nodes 
	   (mapcan #'(lambda (x)
		       (find-etree-nodes #'(lambda (y) (or (skolem-p y)
							   (selection-p y)))
					 x nil t))
		   nodes))
	 (params (mapcar #'(lambda (x) (if (skolem-p x)
					   (car (skolem-terms x))
					   (car (selection-terms x))))
			 skol-nodes)))
    (null (setintersect (free-vars-of term) params))))



(defun single-instantiation-aux (exists-node forall-node all-nodes)
  "Algorithm 252 of Pfenning's thesis. Assumes that EXISTS-NODE and
FORALL-NODE satisfy the preconditions for the algorithm."
  (let ((term (car (expansion-terms exists-node)))
	(ex-kid (car (etree-components exists-node)))
	(fo-kid (car (etree-components forall-node)))
	(param (car (if (skolem-p forall-node)
			(skolem-terms forall-node)
			(selection-terms forall-node)))))
    (update-global-lists exists-node ex-kid)
    (setf (etree-parent ex-kid) (etree-parent exists-node))
    (update-global-lists forall-node fo-kid)
    (setf (etree-parent fo-kid) (etree-parent forall-node))
    (mapc #'(lambda (x) (substitute-in-etree-main term param x))
	  all-nodes)
    ;; added because doing the substitution above in higher-order
    ;; problems may mean that now some leaves are negated.  This can
    ;; screw up the correspondence between C and ~C.
    (mapc #'deepen-negated-leaves all-nodes)
    ex-kid))


;;; This may not be correct if there are negation nodes in the tree
;;; which will throw off the stepping through the tree.  Recall that
;;; we explicitly will insert a negation node in the tree for ~C.
;;; We assume that we have put the negation node at the very top of not-c
;;; and that we will adjust the shallow formula of not-c later if
;;; necessary.

(defun single-instantiation (c not-c conn-list all-nodes)
  "First phase of Pfenning's Algorithm 261.  C is the etree node corresponding
to the formula C, NOT-C is its mirror-image counterpart.  Traverses both
trees simultaneously, stopping at mated interior nodes and expansion
nodes, applying Algorithm 252 if possible.  If it was possible, traverses
the new expansion proof from that point.  Destructively alters the
expansion tree.  Traversal is breadthfirst."
  (do* ((c-list nil (if flag 
			(append c-list (etree-components c))
			c-list))
	(c c (pop c-list))
	(not-c-list nil (if flag
			    (append not-c-list (etree-components not-c))
			    not-c-list))
	(not-c (car (etree-components not-c)) (pop not-c-list))
	(flag nil))
       ((null c))
       (cond ((or (mated-p* c conn-list) (mated-p* not-c conn-list))
	      (setq flag nil))
	     ((single-instantiation-eligible-p c all-nodes)
	      (single-instantiation-aux c not-c all-nodes)
	      (setq flag t))
	     ((single-instantiation-eligible-p not-c all-nodes)
	      (single-instantiation-aux not-c c all-nodes)
	      (setq flag t))
	     (t (setq flag nil)))))

;;; Assume that leaves in C and ~C will be enumerated in the same order
;;; so can match them up

#+comment(defun single-deletion (supports disj a b conn-list)
  (do* ((c (car (etree-components disj))
	   (car (etree-components disj)))
	(not-c (cadr (etree-components disj))
	       (cadr (etree-components disj)))
	(conn-list* 
	  (append (mapcar #'(lambda (x) (cons (etree-name x)
					      (etree-name x)))
			  (find-etree-nodes 
			    #'(lambda (x) (or (and (true-p x)
						   (not (positive-p x)))
					      (and (false-p x)
						   (positive-p x))))
			    disj nil t))
		  conn-list))
       (tried-list nil
		   (list* c-node not-c-node tried-list))
	(useless-list
	   (find-etree-nodes 
	     #'(lambda (x) (or (eq c x) (eq not-c x) (eq disj x)
			       (not (conjunct-node-p x))
			       (some #'(lambda (y) (mated-p* y conn-list*))
				     (find-etree-nodes #'always-true
						       x nil t))))
	     disj nil t)
	   (find-etree-nodes 
	     #'(lambda (x) (or (eq c x) (eq not-c x) (eq disj x)
			       (not (conjunct-node-p x))
			       (some #'(lambda (y) (mated-p* y conn-list*))
				     (find-etree-nodes #'always-true
						       x nil t))))
	     disj nil t))
	(c-node 
	 (car (reverse (find-etree-nodes #'always-true c nil t)))
	 (car (reverse (remove-if #'(lambda (x) (memq x tried-list))
				   (find-etree-nodes #'always-true c nil t)))))
       (not-c-node
	 (car (reverse (find-etree-nodes #'always-true not-c nil t)))
	 (car (reverse (remove-if #'(lambda (x) (memq x tried-list))
				   (find-etree-nodes #'always-true not-c nil t)))))
       (flag nil))
  ((null c-node) 
   (when flag 
     (adjust-shallow-wff disj)
     (setq c (car (etree-components disj)))
     (setq not-c (cadr (etree-components disj))))
   (if flag 
       (minimize-conn-list supports c not-c a b conn-list)
       conn-list))
  (when (and (not (memq c-node useless-list))
	     (some #'(lambda (x) (has-mated-descendant x conn-list))
		   (find-syms c-node)))
    (push c-node useless-list))
  (when (and (not (memq not-c-node useless-list))
	     (some #'(lambda (x) (has-mated-descendant x conn-list))
		   (find-syms not-c-node)))
    (push not-c-node useless-list))
  (unless (and (memq c-node useless-list)
	       (memq not-c-node useless-list))
    (setq flag t)
    (setq tried-list nil)
    (single-deletion-aux c-node not-c-node))))

(defun single-deletion (supports disj a b conn-list)
  (do* ((c (car (etree-components disj))
	   (car (etree-components disj)))
	(not-c (cadr (etree-components disj))
	       (cadr (etree-components disj)))
	(conn-list* 
	  (append (mapcar #'(lambda (x) (cons (etree-name x)
					      (etree-name x)))
			  (find-etree-nodes 
			    #'(lambda (x) (or (and (true-p x)
						   (not (positive-p x)))
					      (and (false-p x)
						   (positive-p x))))
			    disj nil t))
		  conn-list))
       (tried-list nil
		   (list* c-node not-c-node tried-list))
	(useless-list
	   (find-etree-nodes 
	     #'(lambda (x) (or (eq c x) (eq not-c x) (eq disj x)
			       (not (conjunct-node-p x))
			       (some #'(lambda (y) (mated-p* y conn-list*))
				     (find-etree-nodes #'always-true
						       x nil t))))
	     disj nil t)
	   (find-etree-nodes 
	     #'(lambda (x) (or (eq c x) (eq not-c x) (eq disj x)
			       (not (conjunct-node-p x))
			       (some #'(lambda (y) (mated-p* y conn-list*))
				     (find-etree-nodes #'always-true
						       x nil t))))
	     disj nil t))
	(c-node 
	 (car (reverse (find-etree-nodes #'always-true c nil t)))
	 (car (reverse (remove-if #'(lambda (x) (memq x tried-list))
				   (find-etree-nodes #'always-true c nil t)))))
       (not-c-node
	 (car (reverse (find-etree-nodes #'always-true not-c nil t)))
	 (car (reverse (remove-if #'(lambda (x) (memq x tried-list))
				   (find-etree-nodes #'always-true not-c nil t)))))
       (flag nil))
  ((null c-node) 
   (when flag 
;     (adjust-shallow-wff disj)
;     (setq c (car (etree-components disj)))
;     (setq not-c (cadr (etree-components disj)))
)
   (if flag 
       (minimize-conn-list supports c not-c a b conn-list)
       conn-list))
  (when (and (not (memq c-node useless-list))
	     (some #'(lambda (x) (has-mated-descendant x conn-list))
		   (find-syms c-node)))
    (push c-node useless-list))
  (when (and (not (memq not-c-node useless-list))
	     (some #'(lambda (x) (has-mated-descendant x conn-list))
		   (find-syms not-c-node)))
    (push not-c-node useless-list))
  (unless (and (memq c-node useless-list)
	       (memq not-c-node useless-list))
    (setq flag t)
    (setq tried-list nil)
    (multiple-value-setq (c not-c)
      (single-deletion-aux c not-c c-node not-c-node))
     (setf (etree-components disj) (list c not-c))
     (setf (etree-parent c) disj)
     (setf (etree-parent not-c) disj)

)))

(defun has-mated-descendant (node conn-list)
  (some #'(lambda (x) (mated-p* x conn-list))
	(find-etree-nodes #'always-true
			  node nil t)))



(defun single-deletion-aux (c not-c node1 node2)
  (let ((syms1 (find-syms node1))
	(syms2 (find-syms node2)))
    (labels ((raise (node) ; if we are going to delete a node, then also
                           ; delete any negation or rewrite immed. above it
               (let ((parent (etree-parent node)))
                 (if (or (rewrite-p parent)
                         (negation-p parent))
                     (raise parent)
                     node))))
      (setq c (carry-out-deletion* c (mapcar #'raise (cons node1 syms1)) 
                           (get-shallow c) nil))
      (setq not-c (carry-out-deletion* not-c (mapcar #'raise (cons node2 syms2))
                           (get-shallow not-c) nil))
      (values c not-c)
)))



(defun carry-out-deletion* (top-node nodes-to-delete current-shallow bindings)
  (typecase top-node
    ((or econjunction edisjunction) 
     (carry-out-deletion-junction
      top-node nodes-to-delete current-shallow bindings))
    (implication
     (carry-out-deletion-implication
      top-node nodes-to-delete current-shallow bindings))
    ((or expansion skolem selection)
     (carry-out-deletion-quantifier
      top-node nodes-to-delete current-shallow bindings))
    (rewrite
     (carry-out-deletion-rewrite
      top-node nodes-to-delete current-shallow bindings))
    ((or true false leaf)
     (carry-out-deletion-leaf
      top-node nodes-to-delete current-shallow bindings))
    (negation
     (carry-out-deletion-negation
      top-node nodes-to-delete current-shallow bindings))
    (otherwise
     (error "Bad node type (~S) encountered in carry-out-deletion*."
            (type-of top-node))))
)


(defun carry-out-deletion-junction (top-node nodes-to-delete 
                                             current-shallow bindings)
  (let* ((juncts (etree-components top-node))
         (pos-to-keep (cond ((memq (car juncts) nodes-to-delete)
                             1)
                           ((memq (cadr juncts) nodes-to-delete)
                            0)
                           (t nil))))
    (if pos-to-keep
      (let ((sibling (nth pos-to-keep juncts)))
        (carry-out-deletion* sibling nodes-to-delete
                             (if (zerop pos-to-keep)
                                 (glr current-shallow)
                                 (grr current-shallow))
                             bindings)
        )
      (multiple-value-bind (left-node left-shallow)
        (carry-out-deletion* (car juncts) nodes-to-delete 
                             (glr current-shallow) bindings)
        (multiple-value-bind (right-node right-shallow)
          (carry-out-deletion* (cadr juncts) nodes-to-delete 
                               (grr current-shallow)
                               bindings)
          (setf (etree-components top-node)
                (list left-node right-node))
          (dolist (x (etree-components top-node))
            (setf (etree-parent x) top-node))
          (values top-node
                  `((,(gar (gar current-shallow)) . ,left-shallow) 
                    . ,right-shallow)))))))


;; making assumption that we aren't deleting any of the nodes immediately
;; below the quantifier.  Have to think about this, because it's probably
;; wrong.  
;; assume that the correct shallow formula (minus quantifier) 
;; will be returned by the first child.  Otherwise have to worry about
;; what to do if they disagree.  I think that for first order this should
;; be okay.


(defun carry-out-deletion-quantifier (top-node nodes-to-delete 
                                               current-shallow bindings)
  (let* ((kids (etree-components top-node))
         (new-kids-and-shallows 
          (mapcar #'(lambda (term kid) 
                      (multiple-value-list 
                       (carry-out-deletion* kid nodes-to-delete
                                            (gdr
                                             current-shallow)
                                            (acons (bindvar
                                                   current-shallow)
                                                  term
                                                  bindings))))
                  (sel-exp-terms top-node) kids)))
    (setf (etree-components top-node)
          (mapcar #'car new-kids-and-shallows))
    (dolist (kid (etree-components top-node))
      (setf (etree-parent kid) top-node))
    (let ((wff
           (do ((l (reverse bindings) (cdr l))
                (wff (cadar new-kids-and-shallows)
                     (substitute-term-var (cdar l) (caar l) wff)))
               ((null l) wff))))
    (typecase top-node
      (expansion 
       (setf (expansion-shallow top-node)
          (cons (car (get-shallow top-node)) wff)))
      (skolem
       (setf (skolem-shallow top-node)
          (cons (car (get-shallow top-node)) wff)))
      (selection
       (setf (selection-shallow top-node)
          (cons (car (get-shallow top-node)) wff))))
    (values top-node
            (cons (car current-shallow) (cadar new-kids-and-shallows))))))



(defun carry-out-deletion-leaf (top-node nodes-to-delete 
                                         current-shallow bindings)
  (declare (ignore nodes-to-delete bindings))
  (values top-node current-shallow))

;;; assume that child of this node is not to be deleted itself
(defun carry-out-deletion-negation (top-node nodes-to-delete
                                             current-shallow bindings)
  (let* ((kid (car (etree-components top-node))))
    (multiple-value-bind (new-kid new-shallow)
      (carry-out-deletion* kid nodes-to-delete
                           (gdr current-shallow) bindings)
      (setf (etree-parent new-kid) top-node)
      (setf (etree-components top-node) (list new-kid))
      (values top-node (cons 'not new-shallow)))))



(defun carry-out-deletion-implication (top-node nodes-to-delete 
                                                current-shallow bindings)
  (let* ((juncts (etree-components top-node))
         (pos-to-keep (cond ((memq (car juncts) nodes-to-delete)
                             1)
                           ((memq (cadr juncts) nodes-to-delete)
                            0)
                           (t nil))))
    (if pos-to-keep
      (let ((parent (etree-parent top-node))
            (sibling (nth pos-to-keep juncts)))
        (if (zerop pos-to-keep)
            (let ((neg-node 
		      (make-negation :parent parent
				     :junctive 'neutral
				     :positive (positive-p top-node)
				     :components nil)))
              (multiple-value-bind (node wff)
                (carry-out-deletion* sibling nodes-to-delete
                                     (glr current-shallow)
                                     bindings)
                (setf (etree-parent node) neg-node)
                (setf (etree-components neg-node) (list node))
                (values neg-node (cons 'not wff))))
            (carry-out-deletion* sibling nodes-to-delete
                                 (grr current-shallow)
                                 bindings))
        )
      (multiple-value-bind (left-node left-shallow)
        (carry-out-deletion* (car juncts) nodes-to-delete 
                             (glr current-shallow) bindings)
        (multiple-value-bind (right-node right-shallow)
          (carry-out-deletion* (cadr juncts) nodes-to-delete 
                               (grr current-shallow)
                               bindings)
          (setf (etree-components top-node)
                (list left-node right-node))
          (dolist (x (etree-components top-node))
            (setf (etree-parent x) top-node))
          (values top-node
                  `((implies . ,left-shallow) . ,right-shallow)))))))

(defun carry-out-deletion-rewrite (top-node nodes-to-delete 
                                            current-shallow
                                            bindings)
  (case (rewrite-justification top-node)
    (equiv-implics
     (let* ((child-node nil) 
            (child-wff nil)
            (left (glr current-shallow))
            (right (grr current-shallow)))
       (multiple-value-setq (child-node child-wff)
         (carry-out-deletion* (car (etree-components top-node))
                              nodes-to-delete 
                              `(( and . ((implies . ,left) . ,right))
                                 . ((implies . ,right) . ,left))
                              bindings))
       
       (if (and (and-p child-wff)
                (implies-p (glr child-wff))
                (implies-p (grr child-wff))
                (wffeq (glr (glr child-wff))
                       (grr (grr child-wff)))
                (wffeq (grr (glr child-wff))
                       (glr (grr child-wff))))
           (progn
             (setf (etree-parent child-node) top-node)
             (setf (etree-components top-node) (list child-node))
             (values top-node `((equiv . ,(glr (glr child-wff)))
                                 . ,(grr (glr child-wff)))))
           (values child-node child-wff))))
    (equiv-disjs
     (let* ((child-node nil) 
            (child-wff nil)
            (left (glr current-shallow))
            (right (grr current-shallow)))
       (multiple-value-setq (child-node child-wff)
         (carry-out-deletion* (car (etree-components top-node))
                              nodes-to-delete 
                              `((or . ((and . ,left) . ,right))
                                 . ((and . (not . ,left)) . (not . ,right)))
                              bindings))
       (if (and (or-p child-wff)
                (and-p (glr child-wff))
                (and-p (grr child-wff))
                (wffeq (glr (glr child-wff))
                       (glr (grr child-wff)))
                (not-p (glr (grr child-wff)))
                (not-p (grr (grr child-wff)))
                (wffeq (gdr (grr (glr child-wff)))
                       (gdr (grr (grr child-wff)))))
           (progn
             (setf (etree-parent child-node) top-node)
             (setf (etree-components top-node) (list child-node))
             (values top-node `((equiv . ,(glr (glr child-wff)))
                                 . ,(grr (glr child-wff)))))
           (values child-node child-wff))))
    (ab
     (let* ((child-node nil) 
            (child-wff nil)
            (var (bindvar current-shallow))
            (kid-shallow (get-shallow (car (etree-components top-node))))
            (new-var (bindvar kid-shallow)))
       (multiple-value-setq (child-node child-wff)
         (carry-out-deletion* (car (etree-components top-node))
                              nodes-to-delete 
                              (cons (car kid-shallow)
                                    (substitute-term-var new-var var
                                                    (cdr current-shallow)))
                              bindings))
       (if (wffeq-ab child-wff current-shallow)
           (progn
             (setf (etree-parent child-node) top-node)
             (setf (etree-components top-node) (list child-node))
             (values top-node current-shallow))
           (values child-node child-wff))))
    (equivwffs
     (let* ((child-node nil) 
            (child-wff nil))
       (multiple-value-setq (child-node child-wff)
         (carry-out-deletion* (car (etree-components top-node))
                              nodes-to-delete 
                              (if (contains-defn-not-equiv current-shallow)
                                  (instantiate-1 current-shallow)
                                  (instantiate-all current-shallow '(equiv)))
                              bindings))
       (if (wffeq-def child-wff current-shallow)
           (progn
             (setf (etree-parent child-node) top-node)
             (setf (etree-components top-node) (list child-node))
             (values top-node current-shallow))
           (values child-node child-wff)))
       
)
    (otherwise 
     (error "Can't handle rewrite nodes like ~A now. Justification: ~A
" (etree-name top-node) (rewrite-justification top-node))))
  )


(defun adjust-shallow-wff (node)
  (when (etree-components node)
    (dolist (x (etree-components node))
      (adjust-shallow-wff x))
    (typecase node
      ((or econjunction edisjunction implication negation) nil)
      ((or expansion skolem selection)
       (adjust-quantifier-wff node))
      (rewrite
	(adjust-rewrite-wff node)))))

(defun adjust-rewrite-wff (node)
  (let* ((kid (car (etree-components node)))
	(kidwff (get-shallow kid)))
  (case (rewrite-justification node) 
    (equiv-implics 
      (let* ((kid-kids (etree-components kid))
	     (kid-kid-kids (mapcar #'get-shallow
				   (apply #'append
					  (mapcar #'etree-components 
						  kid-kids)))))
	(if (and (econjunction-p kid)
		 (implication-p (car kid-kids))
		 (implication-p (cadr kid-kids))
		 (wffeq-ab (car kid-kid-kids) (cadddr kid-kid-kids))
		 (wffeq-ab (cadr kid-kid-kids) (caddr kid-kid-kids)))
	    (setf (rewrite-shallow node)
		  (cons (cons 'equiv (car kid-kid-kids)) 
			(cadr kid-kid-kids)))
	    (progn
	      (setf (etree-parent kid) (etree-parent node))
	      (setf (etree-components (etree-parent node))
		    (subst kid node (etree-components (etree-parent node))))))))
    (equiv-disjs
      (let* ((kid-kids (etree-components kid))
	     (kid-kid-kids (mapcar #'get-shallow 
				   (apply #'append
					  (mapcar #'etree-components 
						  kid-kids)))))
	(if (and (edisjunction-p kid)
		 (econjunction-p (car kid-kids))
		 (econjunction-p (cadr kid-kids))
		 (wffeq-ab (car kid-kid-kids) (cdaddr kid-kid-kids))
		 (wffeq-ab (cadr kid-kid-kids) (cdr (cadddr kid-kid-kids))))
	    (setf (rewrite-shallow node)
		  (cons (cons 'equiv (car kid-kid-kids)) 
			(cadr kid-kid-kids)))
	    (progn
	      (setf (etree-parent kid) (etree-parent node))
	      (setf (etree-components (etree-parent node))
		    (subst kid node (etree-components (etree-parent node))))))))
    (lambda
      (let* ((shallow (rewrite-shallow node))
	     (norm (lambda-norm shallow)))
	(unless (wffeq-ab norm kidwff)
	  (setf (etree-parent kid) (etree-parent node))
	  (setf (etree-components (etree-parent node))
		(subst kid node (etree-components (etree-parent node)))))))
    (equivwffs
      (let* ((shallow (rewrite-shallow node)))
	(unless (wffeq-def shallow kidwff)
	  (setf (etree-parent kid) (etree-parent node))
	  (setf (etree-components (etree-parent node))
		(subst kid node (etree-components (etree-parent node)))))))
    (ab
      (let* ((shallow (rewrite-shallow node)))
	(unless (wffeq-ab shallow kidwff)
	  (setf (etree-parent kid) (etree-parent node))
	  (setf (etree-components (etree-parent node))
		(subst kid node (etree-components (etree-parent node)))))))
    (ext=
      (let* ((shallow (rewrite-shallow node))
	     (rewrite-equalities 'auto::all)
	     (eqwff (expand-top= shallow)))
	(unless (wffeq-ab eqwff kidwff)
	  (setf (etree-parent kid) (etree-parent node))
	  (setf (etree-components (etree-parent node))
		(subst kid node (etree-components (etree-parent node)))))))
    (leibniz=
      (let* ((shallow (rewrite-shallow node))
	     (rewrite-equalities 'auto::leibniz)
	     (eqwff (expand-top= shallow)))
	(unless (wffeq-ab eqwff kidwff)
	  (setf (etree-parent kid) (etree-parent node))
	  (setf (etree-components (etree-parent node))
		(subst kid node (etree-components (etree-parent node)))))))
    (refl=
      (unless (wffeq kidwff 'truth)
	(setf (etree-parent kid) (etree-parent node))
	(setf (etree-components (etree-parent node))
	      (subst kid node (etree-components (etree-parent node))))))
    (t (error "Can't handle rewrite nodes like ~A now: " (etree-name node))))))


(defun adjust-quantifier-wff (node)
  (let* ((kid (car (etree-components node)))
	 (term (car (typecase node
		      (skolem (skolem-terms node))
		      (selection (selection-terms node))
		      (expansion (expansion-terms node)))))
	 (shallow (get-shallow node))
	 (kid-shallow (get-shallow kid))
	 (var (bindvar shallow)))
    (do ((wfflist nil)
	 (wff (gdr shallow) (pop wfflist))
	 (goodwff nil))
	((or goodwff (null wff))
	 (if goodwff
	     (typecase node
	       (expansion (setf (expansion-shallow node)
				(cons (bindhead shallow) goodwff)))
	       (skolem (setf (skolem-shallow node)
			     (cons (bindhead shallow) goodwff)))
	       (selection (setf (selection-shallow node)
				(cons (bindhead shallow) goodwff))))
	     (error "Problem in adjust-quantifier-wff at node ~A:"
		    (etree-name node))))
	(if (wffeq-ab (substitute-term-var term var wff)
		      kid-shallow)
	    (setq goodwff wff)
	    (cond ((or (and-p wff) (or-p wff))
		   (setq wfflist
		      (cons (grr wff) 
			    (cons (glr wff) wfflist))))
		  ((implies-p wff)
		   (setq wfflist
			 (cons (cons 'not (glr wff))
			       (cons (grr wff) wfflist))))
		  ((equiv-p wff)
		   (setq wfflist
			 (cons (cons 'not (glr wff))
			       (cons (grr wff) wfflist)))
		   (setq wfflist
			 (cons (cons 'not (grr wff))
			       (cons (glr wff) wfflist))))
		  ((not-p wff)
		   (push (gdr wff) wfflist))
)))))



(defun conjunct-node-p (node)
  (do ((parent (etree-parent node)
	       (etree-parent parent)))
      ((not (eq (etree-junctive parent) 'neutral))
       (if (eq (etree-junctive parent) 'con) t nil))))

;; want to reduce conn-list by taking out superfluous connections from
;; supports to a and b which make c or not-c unnecessary

(defun minimize-conn-list (supports c not-c a b conn-list)
  (let ((conn-list1 
	  (minimize-conn-list* 
	    a supports c 
	    (new-restrict-mating-to 
	      conn-list (cons a (cons c supports)))))
	(conn-list2
	  (minimize-conn-list* 
	    b supports not-c
	    (new-restrict-mating-to 
	      conn-list (cons b (cons not-c supports))))))
    (union conn-list1 conn-list2 :test #'equal)))
	       

(defun minimize-conn-list* (a supports c conn-list)
  (do* ((non-c-conns 
	 (remove-if
	   #'(lambda (x) (contains-elt-in x c))
	   conn-list))
	(conn-list*
	 (set-difference conn-list non-c-conns :test #'equal))
	(flag (null non-c-conns))
	(path nil))
      (flag conn-list*)
    (multiple-value-setq (flag path)
      (spans a (cons c supports) conn-list*))
    (unless flag
      (let* ((names (mapcar #'literal-name path))
	     (conn (find-if #'(lambda (x) (and (member (car x) names
						      :test #'string=)
					       (member (cdr x) names
						      :test #'string=)))
			    non-c-conns)))
	(setq non-c-conns (remove conn non-c-conns))
	(push conn conn-list*)))
))


;;; for debugging
(defun print-stuff (disj a b conn-list)
  (msgf "DISJ:" t)
  (display-etree-main disj)
  (msgf "A:" t)
  (display-etree-main a)
  (msgf "B:" t)
  (display-etree-main b)
  (msgf conn-list t)
  (read))


(defun symmetric-simplification (supports disj a b conn-list)
  "Pfenning's algorithm 261.  DISJ should be the disjunction which is
the parent of C and ~C."
  (do ((c (car (etree-components disj))
	  (car (etree-components disj)))
       (not-c (cadr (etree-components disj))
	      (cadr (etree-components disj)))
       (list-of-nodes (cons disj (cons a (cons b supports))))
       (flag t))
      ((null flag) (make-nice-lemma disj) conn-list)
    (setq conn-list (new-restrict-mating-to conn-list list-of-nodes))
    (single-instantiation c not-c conn-list (append (list disj a b) 
						    supports))
;      (when cl-user::*debug*
;	(print-stuff disj a b conn-list))
    (setq conn-list (single-deletion supports disj a b conn-list))
;      (when cl-user::*debug*
;	(print-stuff disj a b conn-list))
    (setq conn-list (new-restrict-mating-to conn-list list-of-nodes))
    (multiple-value-setq (flag conn-list)
      (single-mating-change supports disj a b conn-list))
;      (when cl-user::*debug*
;	(print-stuff disj a b conn-list))
))

(defun new-restrict-mating-to (conn-list node-list)
  "Returns connections from the conn-list both of whose ends are
below some node in the given node-list."
  (let ((new-conn-list nil))
    (dolist (conn conn-list new-conn-list)
      (if (and (find-if #'(lambda (x) (find-etree-node-name (car conn) x t))
			node-list)
	       (find-if #'(lambda (x) (find-etree-node-name (cdr conn) x t))
			node-list))
	(push conn new-conn-list)))))


;;; Here want to make the lemma that we're using as good as possible
;;; don't have much idea how, except to remove any double negations

(defun make-nice-lemma (disj)
  (setf (etree-components disj)
	(mapcar #'remove-double-negations (etree-components disj))))

;;; for now, just top-level
(defun remove-double-negations (etree)
 (if (and (negation-p etree)
	  (negation-p (car (etree-components etree))))
     (car (etree-components (car (etree-components etree))))
   etree))

