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
;;;Filename: MATING-MERGE-EQ
(context etr-nat)

(deffile mating-merge-eq
  (extension lisp)
  (part-of mating-transform)
  (mhelp "Contains functions for removing Leibniz equalities from
expansion trees."))

(defun deepen-negated-leaves (etree)
  (dolist (leaf (find-etree-nodes #'leaf-p etree nil t) etree)
    (do ()
	((not (not-p (leaf-shallow leaf))))
      (let ((neg-node (make-negation :junctive 'neutral 
				     :parent (etree-parent leaf)
				     :components (list leaf)
				     :positive (positive-p leaf))))
	(update-status nil neg-node (etree-status* leaf))
	(update-global-lists leaf neg-node)
	(setf (etree-parent leaf) neg-node)
	(setf (leaf-shallow leaf) (cdr (leaf-shallow leaf)))
	(setf (etree-positive leaf) (not (etree-positive leaf)))
))))

;;; Currently have no heuristics for deciding which of the two possible
;;; substitutions we should use for the selection parameters 

(defun make-left-side-refl () t)

(defun make-sub-eq-wff (wff equality)
  (let ((new-x (fresh-var (type wff) '\x)))
    (if (make-left-side-refl)
	`((,new-x . lambda) (,equality . ,wff) . ,new-x)
      `((,new-x . lambda) not (,equality . ,new-x) . ,wff))))
;where q is a Leibniz exp var, in forall q . qA IMPLIES qB,
;apply the subst q<--[lambda x. A = x]
;or    the subst q<--[lambda x. ~ x = B]

; See Prog Guide
(defun remove-leibniz-nodes (conn-list &optional (etree (eproof-etree current-eproof)))
  (let* ((neg-eq-rew-nodes 
	   (find-etree-nodes 
	     #'(lambda (x) (and (rewrite-p x)
				(not (positive-p x))
				(equals-p (rewrite-shallow x)) ; make sure the expanded = is at the head
				(eq (rewrite-justification x) 'leibniz=)))
	     etree nil t))
	 (pos-eq-rew-nodes
	  (find-etree-nodes #'(lambda (x) (and (rewrite-p x)
					       (positive-p x)
					       (equals-p (rewrite-shallow x)) ; make sure the expanded = is at the head
					       (eq (rewrite-justification x) 'leibniz=)
					; cebrown 10/14/01, added this check so
					; leibniz rewrites with nonleaf matings below them
					; are not changed to subst= rewrites
					       (not (nonleaf-mating-below x conn-list))))
			    etree nil t))) ; cebrown, moved this to here from cleanup-leibniz-expansion and passed it into cleanup-leibniz-expansion
					; as an argument, 11/3/00
    (setq conn-list (pre-process-nonleaf-leibniz-connections neg-eq-rew-nodes conn-list)) ; cebrown 11/3/00
    (dolist (eq-rew-node neg-eq-rew-nodes)
	    (setq conn-list (remove-leibniz conn-list eq-rew-node)))
    (deepen-negated-leaves etree)
    (setq conn-list (cleanup-leibniz-expansions pos-eq-rew-nodes etree conn-list))
    conn-list))

; cebrown, 10/14/01
(defun nonleaf-mating-below (e conn-list)
  (dolist (conn conn-list nil)
	  (let ((a (car conn)))
	    (when (and (not (leaf-p a))
		       (find-etree-node-name (etree-name a) e))
	      (return-from nonleaf-mating-below t)))
	  (let ((a (cdr conn)))
	    (when (and (not (leaf-p a))
		       (find-etree-node-name (etree-name a) e))
	      (return-from nonleaf-mating-below t)))))
		     

; See Prog Guide
(defun remove-leibniz (conn-list eq-rew-node)
  (declare (special etree))  ; cebrown, so we can modify etree
					;riddled with comments because it's impossible to debug otherwise... MB
  (let* ((param-node (find-etree-node #'(lambda (x) (or (skolem-p x) (selection-p x)))
				      eq-rew-node t))
	 (param (car (sel-exp-terms param-node)))
	 (imp-node (find-etree-node #'implication-p param-node t))
	 ;; may have a lambda-rewrite node immediately below the imp-node
	 (new-refl-node (find-etree-node #'leaf-p 
					 (if (make-left-side-refl)
					     (car (etree-components imp-node))
					   (cadr (etree-components imp-node)))))
	 (subwff (make-sub-eq-wff (gdr (get-shallow new-refl-node))
				  (gar (gar (rewrite-shallow eq-rew-node)))))
	 (new-non-refl-node (find-etree-node #'leaf-p
					     (if (make-left-side-refl)
						 (cadr (etree-components imp-node))
					       (car (etree-components imp-node)))))
	 (non-refl-branch (if (make-left-side-refl)
			      (cadr (etree-components imp-node))
			    (car (etree-components imp-node))))
	 (mated-to-refl-nodes (mapcan #'(lambda (conn)
					  (cond ((eq (car conn) new-refl-node) (list (cdr conn)))
						((eq (cdr conn) new-refl-node) (list (car conn)))
						(t nil)))
				      conn-list))
	 (temp nil))
    (when merge-debug
      (msgf t t "Rewrite node " eq-rew-node " has shallow formula " ((rewrite-shallow eq-rew-node) . gwff)
	    t "param node: " param-node " , param: " param " , imp-node: " imp-node t
	    "left half (new-refl-node): " new-refl-node " , right half (new-non-refl-node): " new-non-refl-node t
	    "left is mated to: " mated-to-refl-nodes " and subwff is: " (subwff . gwff) t
	    "This rewrite node has below it:" t
	    (find-etree-nodes #'rewrite-p eq-rew-node nil t) t))
    (if (memq new-non-refl-node mated-to-refl-nodes)
	;; left hand side and right hand side of equality are the
	;; same, but maybe only after some rewrites done.
	(let ((lhs (cdar (rewrite-shallow eq-rew-node)))
	      (rhs (cdr (rewrite-shallow eq-rew-node)))
	      (original-eq-rew-node eq-rew-node)) ; cebrown 11/5/00, so we can remove conns to this node from the mating
	  (when merge-debug (msgf "lhs= " (lhs . gwff) " , rhs= " (rhs . gwff) t))
	  (when (not (wffeq lhs rhs)) ; not really same
		(let* ((lhs* (lnorm lhs))
		       (rhs* (lnorm rhs))
		       (norm-equality (cons (cons (caar (rewrite-shallow eq-rew-node)) lhs*) rhs*))
		   ;; this will probably not work in some weird case
		   ;; where min-quantifier-scoping was used or
		   ;; something similar happened , but I am just going
		   ;; to assume that the only rewrites we have are either
		   ;; lambda-norms or equivwffs (definitions)
; cebrown -- 4/25/99, The list-of-rewrites was being set to
; something like ( <x = x> (<x = x> . EQUIVWFFS)), and is not being set to
; something like ( <x = x> (<ID x = x> . EQUIVWFFS) (<x = x> . REFL=))
		       (list-of-rewrites (do ((wff norm-equality)
					      (rewrite-list (if (not (wffeq norm-equality (rewrite-shallow eq-rew-node)))
								(list (cons (rewrite-shallow eq-rew-node) 'lambda)))))
					     ((not (contains-defn-not-equiv wff))
					      (when merge-debug
						(msgf "eventual wff: " (wff . gwff) t
						      "norm-equality: " (norm-equality . gwff) t
						      "list of rewrites: " rewrite-list t))
					      (cons wff (reverse 
							 (cons
							  (cons wff 'REFL=) ; cebrown, 4/25/99
							  rewrite-list))))
					     (push (cons wff 'equivwffs) rewrite-list) ; cebrown, 4/25/99, commuted these operations
					     (setq wff (instantiate-all wff '(equiv)))))
		       (final-wff (pop list-of-rewrites))
		       (new-rewrites (mapcar #'(lambda (x) (make-rewrite :positive (etree-positive eq-rew-node)
									 :shallow (car x)
									 :junctive 'neutral
									 :justification (cdr x)))
					     list-of-rewrites))
		       (new-rewrite (do ((new-rewrites (cdr new-rewrites) (cdr new-rewrites))
					 (rewrite (car new-rewrites) (car new-rewrites))
					 (previous nil rewrite)
					 (first (car new-rewrites)))
					((null rewrite) (cons first previous))
					(setf (etree-parent rewrite) previous)
					(setf (etree-components rewrite)
					      (if (car new-rewrites)
						  (list (car new-rewrites))))
					(when previous
					      (setf (etree-components previous) (list rewrite))))))
		  (declare (ignore final-wff))
		  ;the defn of new-rewrite and the following three setf/setq's were fixed on Wed Aug 28 14:47:48 1996 [MB]
		  ;at this point, (car new-rewrite) is the first elt of a chain ending in the leaf
  		  ;at (cdr new-rewrite); we put this chain in place of the old rewrite.
		  (unless (null (car new-rewrite))
			  (if (etree-parent eq-rew-node) ; cebrown -- 4/5/99, to deal with the case where eq-rew-node is the root of the tree
			      (progn
				(setf (etree-components (etree-parent eq-rew-node)) ; If eq-rew-node is the root of the tree, 
					; then (etree-parent eq-rew-node) is NIL and we cannot call (etree-components NIL).
				      (mapcar #'(lambda (x) (if (eq x eq-rew-node) (car new-rewrite) x))
					      (etree-components (etree-parent eq-rew-node)))) 
				(setf (etree-parent (car new-rewrite)) (etree-parent eq-rew-node)))
			    (setq etree (car new-rewrite)))
			  (setq eq-rew-node (cdr new-rewrite)))
		  ))
	  (setf (rewrite-justification eq-rew-node) 'refl=)
	  (setf (etree-components eq-rew-node)
		(list (create-true-node nil (positive-p eq-rew-node) (etree-free-vars eq-rew-node)
					eq-rew-node new-non-refl-node)))
	  (setq conn-list (delete-if #'(lambda (x) (or (memq (car x) (list new-refl-node new-non-refl-node original-eq-rew-node)) ; cebrown 11/6/00
						       (memq (cdr x) (list new-refl-node new-non-refl-node original-eq-rew-node)))) ; also delete conns to rew
				     conn-list)))
      (progn
	(when merge-debug
	  (msgf "Shallow fmla of new-non-refl-node is: " ((get-shallow new-non-refl-node) . gwff) t))
	(substitute-in-etree-main subwff param (eproof-etree current-eproof))
	(setf (eproof-skolem-constants current-eproof)
	      (delete-if #'(lambda (x) (wffeq (car x) param))
			 (eproof-skolem-constants current-eproof)))
	(setf (eproof-skolem-node-list current-eproof)
	  (delete param-node (eproof-skolem-node-list current-eproof)))
	(when (not-p (get-shallow new-non-refl-node))
	      (when merge-debug
		(msgf "Negation found. Changing conn-list " conn-list t
		      "and mated-to-refl-nodes."))
	      (setq temp (deepen-leaf-node-rulep new-non-refl-node))
	      (setq mated-to-refl-nodes 
		    (nsubstitute (car (etree-components temp)) new-non-refl-node mated-to-refl-nodes))
	      (setq conn-list (nsubst (car (etree-components temp)) new-non-refl-node conn-list))
	      (setq new-non-refl-node (car (etree-components temp)))
	      (when merge-debug
		(msgf "Conn-list is now: " conn-list t "mated-to-refl-nodes is: " mated-to-refl-nodes t
		      "new-non-refl-node is: " new-non-refl-node t)))
	(if (wffeq-ab (get-shallow eq-rew-node) (get-shallow new-non-refl-node))
	    (progn
	      (if (etree-parent eq-rew-node) ; cebrown 9/18/01
		  (setf (etree-parent new-non-refl-node) (etree-parent eq-rew-node)) ; cebrown 9/18/01
		(progn ; cebrown 9/18/01
		  (setf (etree-parent new-non-refl-node) nil) ; cebrown 9/18/01
		  (setq etree new-non-refl-node))) ; cebrown 9/18/01
	      (update-global-lists eq-rew-node new-non-refl-node)
	      (nsubst new-non-refl-node eq-rew-node conn-list)) ; to replace conn's to eq-rew-node to the new node w/ same shallow fmla
	  (progn
	    (when merge-debug 
	      (msgf "shallow fmlas are different: " ((get-shallow eq-rew-node) . gwff) t
		    ((get-shallow new-non-refl-node) . gwff) t))
	    (if (wffeq (lnorm (get-shallow eq-rew-node)) (get-shallow new-non-refl-node))
		(setf (rewrite-justification eq-rew-node) 'lambda)
	      (setf (rewrite-justification eq-rew-node) 'equivwffs))
	    ;the line above is new Wed Aug 28 14:49:41 1996 [MB]
	    ;it used to just put 'equivwffs under all circumstances.
	    ;(setf (etree-parent new-non-refl-node) eq-rew-node)
	    ;(setf (etree-components eq-rew-node) (list new-non-refl-node))
	    (setf (etree-parent non-refl-branch) eq-rew-node)
	    (setf (etree-components eq-rew-node) (list non-refl-branch))
	    (setf eq-rew-node new-non-refl-node)))
	(dolist (node mated-to-refl-nodes)
		  ;; assume node is a leaf node
		(setq conn-list (delete-if #'(lambda (x) (or (and (eq node (car x)) (eq new-refl-node (cdr x)))
							     (and (eq node (cdr x)) (eq new-refl-node (car x)))))
					   conn-list))
		(setq node (deepen-to-literals-rulep node))
		(setq node (find-etree-node #'leaf-p node t))
		(let ((newnode (create-rewrite-node (get-shallow node) (positive-p node)
						    'TRUTH 'refl= (etree-free-vars node)
						    (etree-parent node) node)))
		  (update-global-lists node newnode)
		  (deepen-leaf-node (car (etree-components newnode)))
		  (setq conn-list (delete-if #'(lambda (x) (or (eq node (car x)) (eq node (cdr x))))
					     conn-list))))))
    conn-list))
		  
	    
; See Prog Guide	 
(defun cleanup-leibniz-expansions (eq-rew-nodes etree conn-list)
  (let ((exp-node nil))
    (dolist (eq-rew-node eq-rew-nodes (remove-spurious-connections etree conn-list))
      (when merge-debug (msgf "Cleaning up " eq-rew-node "  " ((get-shallow eq-rew-node) . gwff))
	    (tr-print-etree* eq-rew-node)
)
      (when (find-etree-node #'expansion-p eq-rew-node t)
	;; assume we have a chain of rewrites down to the expansion node
	(do ((top eq-rew-node middle)
	     (middle (car (etree-components eq-rew-node)) bottom)
	     (bottom (car (etree-components 
			   (car (etree-components eq-rew-node))))
		     (car (etree-components bottom)))
	     (equality (gar (gar (get-shallow eq-rew-node)))))
	    ((expansion-p middle) 
	     (when merge-debug (msgf "LEAVING with Top: " top "  Middle: " middle "  Bottom: " bottom "  Equality: " (equality . gwff)))
	     (setf (rewrite-justification top) 'subst=)
	     (setq exp-node middle)
	     (when merge-debug 
	       (tr-print-etree* eq-rew-node)))
	   ;; node must be a rewrite node
	  (when merge-debug (msgf "Top: " top "  Middle: " middle "  Bottom: " bottom "  Equality: " (equality . gwff)))
	  (let* ((n (get-shallow bottom)) ; n should be a wff forall q . q L imp q R
		 (left-wff (if (and (a-bd-wff-p n)
				    (implies-p (gdr n)))
			       (gdr (gdr (gar (gdr n))))
			     (if (implies-p n)
				 (gdr (gdr (gar n)))
			       (gdr (gar n)))))
		 (right-wff (if (and (a-bd-wff-p n)
				     (implies-p (gdr n)))
				(gdr (gdr (gdr n)))
			      (if (implies-p n)
				  (gdr (gdr n))
				(gdr n)))))
	    (when merge-debug (msgf "   left-wff: " (left-wff . gwff) "  right-wff: " (right-wff . gwff)))
	    (setf (rewrite-justification top) (rewrite-justification middle))
	    (setf (rewrite-shallow middle) (cons (cons equality left-wff) right-wff))))
	(when merge-debug (msgf "  Fixing exp-node: " exp-node "  " ((get-shallow exp-node) . gwff)))
	(dolist (son (etree-components exp-node))
	  (let ((imp-node (find-etree-node #'implication-p son t)))
	    (when imp-node
;	      (reduce-rewrites imp-node) ; could move these two back to here
;	      (remove-double-negations-merge imp-node) ; instead of in cleanup-etree
	      (setf (etree-parent imp-node) exp-node)
	      (update-global-lists son imp-node)
	      ;; added to make sure the equality rule can be applied
	      (when merge-debug (msgf "Calling check-shallow-formulas with " eq-rew-node " " exp-node " " imp-node t
				      "  imp-node: " ((get-shallow imp-node) . gwff)))
	      (check-shallow-formulas eq-rew-node exp-node imp-node)
	      )))
	(when merge-debug (tr-print-etree* eq-rew-node))
	;; See theorem 146 in Pfenning's thesis
	(apply-thm-146 exp-node eq-rew-node)))))

; See Prog Guide
(defun check-shallow-formulas (eq-rew-node exp-node imp-node)
  (let* ((equality (rewrite-shallow eq-rew-node))
         (left-term (glr equality))
         (right-term (grr equality))
         (left-child (car (etree-components imp-node)))
         (right-child (cadr (etree-components imp-node)))
         (left-wff (get-shallow left-child))
         (right-wff (get-shallow right-child))
         (pos (position imp-node (etree-components exp-node)))
         (exp-term (lambda-norm (nth pos (expansion-terms exp-node)))))
    (when merge-debug (msgf "  Expansion term: " (exp-term . gwff) "  Equality: " (equality . gwff) t
			    "  Left: " (left-term . gwff) " Right: " (right-term . gwff) t
			    "  Left-wff: " ((lnorm left-wff) . gwff) " Right-wff: " ((lnorm right-wff) . gwff)))
    (unless (apply #'subst-occs 
                   (nconc (mapcar #'strip-sk-terms
                                  (mapcar #'lnorm (list left-term left-wff right-term
							  right-wff)))
                          (list nil)))
      (let ((new-left-rew
             (make-rewrite :parent imp-node :components (list
                                                         left-child)
                           :shallow 
                           (if (lambda-bd-p exp-term)
                               (core::lambda-contr (cons exp-term
                                                      left-term))
                               (cons exp-term left-term))
                           :free-vars (etree-free-vars imp-node)
                           :junctive 'neutral
                           :justification 'lambda))
            (new-right-rew
             (make-rewrite :parent imp-node :components (list
                                                         right-child)
			   :parent t ; cebrown 8/26/99 I have no idea why this code ever worked without this fix
                           :shallow 
                           (if (lambda-bd-p exp-term)
                               (core::lambda-contr (cons exp-term
                                                      right-term))
                               (cons exp-term right-term))
                           :free-vars (etree-free-vars imp-node)
                           :junctive 'neutral
                           :justification 'lambda)))
	(when merge-debug (msgf "Created two new children with shallow fmlas: " t
				((rewrite-shallow new-left-rew) . gwff) t
				((rewrite-shallow new-right-rew) . gwff)))
        (setf (etree-parent left-child) new-left-rew)
        (setf (etree-parent right-child) new-right-rew)
        (setf (etree-components imp-node) (list new-left-rew new-right-rew))))))

(defun strip-sk-terms (wff)
  (cond ((skolem-term-p wff) (strip-sk-terms (skolem-term-term wff)))
        ((lsymbol-q wff) wff)
        (t (cons (strip-sk-terms (car wff))
                 (strip-sk-terms (cdr wff))))))

; See Prog Guide
(defun apply-thm-146 (exp-node eq-rew-node)
  (when (= 1 (length (etree-components exp-node)))
    (let* ((imp-node (car (etree-components exp-node)))
	   (left-node (find-etree-node #'leaf-p
				       (car (etree-components imp-node)) t))
	   (right-node (find-etree-node #'leaf-p
					(cadr (etree-components imp-node)) t)))
      (if (and left-node
	       (wffeq-ab (leaf-shallow left-node) 
			 (rewrite-shallow eq-rew-node))
	       (eq (positive-p eq-rew-node) (positive-p left-node)))
	  (progn (when merge-debug (msgf t "THM146 is applicable to " exp-node
					 "Replacing " eq-rew-node "  " ((rewrite-shallow eq-rew-node) . gwff) t
					 "     with " left-node "  " ((leaf-shallow left-node) . gwff) )
		       (tr-print-etree* exp-node))
		 (replace-with left-node eq-rew-node))
	(if (and right-node
		 (wffeq-ab (leaf-shallow right-node) 
			   (rewrite-shallow eq-rew-node))
		 (eq (positive-p eq-rew-node) (positive-p right-node)))
	  (progn (when merge-debug (msgf t "THM146 is applicable to " exp-node
					 "Replacing " eq-rew-node "  " ((rewrite-shallow eq-rew-node) . gwff) t
					 "     with " right-node "  " ((leaf-shallow right-node) . gwff) )
		       (tr-print-etree* exp-node))
		 (replace-with right-node eq-rew-node))
	  )))))



(defun replace-with (new-node old-node)
  (when merge-debug (msgf "  Replacing " old-node " with " new-node))
  (setf (leaf-shallow new-node)
	(rewrite-shallow old-node))
  (setf (etree-parent new-node) (etree-parent old-node))
  (when (etree-parent old-node)
	(setf (etree-components (etree-parent old-node))
	      (nsubst new-node old-node
		      (etree-components 
			(etree-parent old-node))))))

					; cebrown 11/3/00
					; the following function changes the mating
					; so that there are no connections to nodes
					; occuring strictly between a negative Leibniz rewrite
					; and the leaves beneath it.
					; This is necessary for remove-leibniz to work properly
					; See Prog Guide
(defun pre-process-nonleaf-leibniz-connections (eq-rew-nodes conn-list)
  (let ((retval nil)
	moved)
    (dolist (conn conn-list retval)
      (setq moved nil)
      (unless (and (leaf-p (car conn)) (leaf-p (cdr conn)))
	(dolist (eq-rew-node eq-rew-nodes)
	  (when (or (and (not (eq (car conn) eq-rew-node))
			 (find-etree-node #'(lambda (x) (eq x (car conn))) eq-rew-node))
		    (and (not (eq (cdr conn) eq-rew-node))
			 (find-etree-node #'(lambda (x) (eq x (cdr conn))) eq-rew-node)))
	    (setq retval (append (mapcar #'cons ; push connection to connections between corresponding leaves
					 (find-etree-nodes #'leaf-p (car conn))
					 (find-etree-nodes #'leaf-p (cdr conn)))
				 retval)
		  moved t)
	    (return nil))))
      (unless moved
	(setq retval (cons conn retval))))))

;;; Here want to reduce the rewrite node to as lambda-normal as possible
;;; without touching the wff which is part of the original equality
(defun reduce-rewrites (imp-node)
  (do ((changed t))
      ((not changed))
    (setq changed nil)
    (dolist (child (etree-components imp-node))
      ;; should always be the case, since the substitution is for
      ;; a variable of type (O . A), and will be a lambda term
      (when (and (rewrite-p child)
		 (memq (rewrite-justification child) '(lambda beta eta)))
	(let* ((kid (car (etree-components child)))
	       (wff (rewrite-shallow child))
	       (var (gensym))
	       (other-wff (if (eq 'eta (rewrite-justification child)) 
			      (lnorm-eta (cons (car wff) var))
			    (if (eq 'beta (rewrite-justification child)) 
				(lnorm-beta (cons (car wff) var))
			      (lnorm (cons (car wff) var)))))
	       (newwff (substitute-term-var (cdr wff) var other-wff)))
	  (if (wffeq-ab newwff (get-shallow kid))
	      (progn (update-global-lists child kid)
		     (setf (etree-parent kid) imp-node)
		     (setq changed t))
	    (setf (rewrite-shallow child) newwff)))))))

;;; If both sides of the implication (substitution) begin with double
;;; negations, might as well remove them

(defun remove-double-negations-merge (imp-node)
  (do* ((left1 (car (etree-components imp-node))
	       (car (etree-components left2)))
        (left2 (car (etree-components left1))
	       (car (etree-components left1)))
	(right1 (cadr (etree-components imp-node))
		(car (etree-components right2)))
        (right2 (car (etree-components right1))
		(car (etree-components right1))))
       ((not (and (negation-p left1)
		  (negation-p left2)
		  (negation-p right1)
		  (negation-p right2)))
	(setf (etree-components imp-node)
	      (list left1 right1))
	(setf (etree-parent left1) imp-node)
	(setf (etree-parent right1) imp-node))))


;;; We may have created connections between leaves whose shallow formulas
;;; are of the form "a = a". 
					; See Prog Guide
(defun remove-spurious-connections (etree conn-list)
  (do ((quit nil)
       (bad-conn nil))
      (quit (dolist (rew-node
		      (find-etree-nodes
			#'(lambda (x)
			    (and (rewrite-p x)
				 (eq 'subst= (rewrite-justification x))))
			etree nil t))
	       (let ((exp-node (find-etree-node #'expansion-p rew-node t)))
		 (when exp-node
		   (do ((imp-nodes (reverse (etree-components exp-node))
				   (cdr imp-nodes))
			(terms (reverse (expansion-terms exp-node))
			       (cdr terms))
			(new-nodes nil)
			(new-terms nil)
			(connected-nodes (mapcan #'(lambda (x)
						     (list (car x) (cdr x)))
						 conn-list)))
		       ((null imp-nodes)
			(setf (etree-components exp-node) new-nodes)
			(setf (expansion-terms exp-node) new-terms))
		       (when (find-etree-node
			      #'(lambda (x) (memq x connected-nodes))
			      (car imp-nodes) t)
			 (push (car imp-nodes) new-nodes)
			 (push (car terms) new-terms)))
		   (apply-thm-146 exp-node rew-node))))
	    conn-list)
    (setq bad-conn
	  (dolist (conn conn-list nil)
	    (let* ((first (car conn))
		   (first-wff (get-shallow first))
		   (sec (cdr conn))
		   (sec-wff (get-shallow sec)))
	      (when (or (and (equals-p first-wff)
			 (wffeq (cdr first-wff) (cdar first-wff)))
			(and (equals-p sec-wff)
			 (wffeq (cdr sec-wff) (cdar sec-wff))))
		    (return conn)))))
    (if bad-conn
	(let* ((first (car bad-conn))
	       (sec (cdr bad-conn))
	       (temp nil))
	  (setq conn-list (delete bad-conn conn-list :test #'equal))
	  (when (leaf-p first)
	    (setq temp (deepen-leaf-node first))
	    (deepen-leaf-node (car (etree-components temp))))
	  (when (leaf-p sec)
	    (setq temp (deepen-leaf-node sec))
	    (deepen-leaf-node (car (etree-components temp)))))
      (setq quit t))))

; major changes to prettify code - cebrown 11/22/00
; now all it does is rename variables (sel vars, free evars, and bd vars) globally in the etree
; it was lambda-normalizing exp terms, but this is now done in raise-lambda-nodes code
; also, it was processing subst= rewrites, which is now done in remove-leibniz code

; See the Programmer's Guide for a description of the algorithm and a proof that the renamings do
; not lead to a variable capture.

(defun prettify-etree (etree)
  (let ((fixed-frees nil) ; list of frees
	(frees-to-rename nil) ; list of frees
	(fixed-bounds nil) ; list of bounds
	(bounds-to-rename nil) ; list of bounds
	(alpha nil)  ; renaming for bounds
	(theta nil) ; renaming for frees
	(used-frees nil)) ; cod of theta
    (declare (special fixed-frees frees-to-rename fixed-bounds bounds-to-rename used-frees theta alpha))
    (prettify-process-vars-in-etree etree)
    (when merge-debug
      (msgf "fixed frees: " fixed-frees)
      (msgf "frees to rename: " frees-to-rename)
      (msgf "fixed bounds: " fixed-bounds)
      (msgf "bounds to rename: " bounds-to-rename))
    (dolist (z bounds-to-rename)
      (setf (get z 'not-alpha-image) nil)) ; as we go along, we collect var names to which a bd cannot be sent
    (dolist (y fixed-frees) ; send frees in original wff to themselves (guaranteed to be legal)
      (prettify-free-rename y y))
    (dolist (x fixed-bounds) ; send bounds in original wff to themselves (guaranteed to be legal)
      (prettify-bound-rename x x))
    (dolist (y frees-to-rename) ; on the first pass, find sel var y's which correspond to a bound var in the original wff,
					; and send these to the bv if we can
      (let ((x (get y 'sel-var-bound)))
	(when (and (member x fixed-bounds)
		   (prettify-free-legal-p y x))
	  (prettify-free-rename y x))))
    (dolist (y frees-to-rename) ; on the second pass, find sel var y's which correspond to a bound var x which may be renamed.
					; send x and y to the same (pretty) bv, if we can
      (let ((x (get y 'sel-var-bound)))
	(when x
	  (prettify-identify-free-bound y x))))
    (dolist (z bounds-to-rename) ; on third pass, check to see if any bounds have a preferred var with which to be identified,
					; and try to send them to the same pretty var, if we can
      (dolist (x (get z 'bound-try-to-equate-to-bound))
	(prettify-identify-bound-bound x z))
      (dolist (y (get z 'bound-try-to-equate-to-free))
	(prettify-identify-free-bound y z)))
    (dolist (y frees-to-rename) ; finally, choose names for the rest of the frees
      (when (not (assoc y theta))
	(let ((yp (get-best-alt-free-name y)))
	  (prettify-free-rename y yp))))
    (dolist (z bounds-to-rename) ; and choose names for the rest of the frees
      (when (not (assoc z alpha))
	(let ((zp (get-best-alt-bound-name z)))
	  (prettify-bound-rename z zp))))
    (when merge-debug
      (msgf "renaming for sel vars and free evars:" t theta)
      (msgf "renaming for bound vars:" t alpha))
    (rename-all-vars-in-etree theta alpha etree)
    (remove-unnecessary-ab-rews etree)))

(defun remove-unnecessary-ab-rews (etree &optional parent)
  (if (unnecessary-ab-rew-p etree)
      (remove-unnecessary-ab-rews (car (etree-components etree)) parent)
    (progn
      (setf (etree-parent etree) parent)
      (setf (etree-components etree)
	    (mapcar #'(lambda (kid)
			(remove-unnecessary-ab-rews kid etree))
		    (etree-components etree)))
      etree)))

(defun unnecessary-ab-rew-p (etree)
  (and (rewrite-p etree)
       (eq (rewrite-justification etree) 'AB)
       (wffeq (rewrite-shallow etree)
	      (get-shallow (car (etree-components etree))))))

(defun prettify-process-vars-in-etree (etree)
  (prettify-process-top-shallow
   (if (eproof-lemmas current-eproof)
       (cddr (get-shallow etree))
     (get-shallow etree)))
  (prettify-process-vars-in-etree-rec etree))

(defun prettify-process-vars-in-etree-rec (etree)
  (declare (special fixed-frees frees-to-rename))
  (when (member (type-of etree) '(leaf expansion selection skolem rewrite))
    (prettify-process-wff (get-shallow etree))
    (typecase etree
      (rewrite
					; sometimes a rewrite introduces a free var, e.g., when an abbrev has a free in it (PLUS has S free)
					; this is a bit unusual, but when it happens it should not be renamed,
					; because we are sort of thinking of it as being a constant in the signature
       (let ((extra-frees (setdiff (free-vars-of (get-shallow (car (etree-components etree))))
				   (free-vars-of (rewrite-shallow etree)))))
	 (dolist (v extra-frees)
	   (if (member v frees-to-rename)
	       (progn
		 (when merge-debug (msgf "Found free " v " introduced by rewrite " etree t))
		 (setq frees-to-rename (remove v frees-to-rename))
		 (push v fixed-frees))
	     (unless (member v fixed-frees)
	       (when merge-debug (msgf "Found free " v " introduced by rewrite " etree t))
	       (pushnew v fixed-frees)
	       (setf (get v 'sel-var-bound) nil)
	       (setf (get v 'free-must-avoid) nil)))))
       (when (rewrite-ruleq-shallow etree)
	 (prettify-process-wff (rewrite-ruleq-shallow etree))))
      (expansion
       (dolist (e (expansion-terms etree))
	 (prettify-process-wff e)))
      (skolem
       (let ((a (car (skolem-terms etree))))
	 (setf (get a 'sel-var-bound) (caar (get-shallow etree)))
	 (unless (or (member a fixed-frees) (member a frees-to-rename))
	   (pushnew a frees-to-rename)
	   (setf (get a 'free-must-avoid) nil))))
      (selection
       (let ((a (car (selection-terms etree))))
	 (setf (get a 'sel-var-bound) (caar (get-shallow etree)))
	 (unless (or (member a fixed-frees) (member a frees-to-rename))
	   (setf (get a 'free-must-avoid) nil)
	   (push a frees-to-rename))))
      (otherwise
       nil)))
  (dolist (kid (etree-components etree))
    (prettify-process-vars-in-etree-rec kid)))

(defun prettify-process-top-shallow (wff &optional gamma) ; gamma is context of bounds
  (declare (special fixed-frees fixed-bounds))
  (cond ((label-q wff) (apply-label wff (prettify-process-top-shallow wff gamma)))
	((lsymbol-q wff)
	 (unless (get wff 'abbrev)
	   (when (and (not (logconst-q wff)) (propsym-q wff)
		      (not (member wff gamma)))
	     (unless (member wff fixed-frees)
	       (pushnew wff fixed-frees)
	       (setf (get wff 'free-must-avoid) nil)))))
	((boundwff-q wff)
	 (unless (member (caar wff) fixed-bounds)
	   (push (caar wff) fixed-bounds)
	   (setf (get (caar wff) 'bound-must-avoid) nil))
	 (prettify-process-top-shallow (cdr wff) (cons (caar wff) gamma)))
	(t
	 (prettify-process-top-shallow (car wff) gamma)
	 (prettify-process-top-shallow (cdr wff) gamma))))

					; gamma is context of bounds, argstack associates lambda bindings w/arg, if the wff is a redex
(defun prettify-process-wff (wff &optional gamma argstack)
  (declare (special fixed-frees frees-to-rename fixed-bounds bounds-to-rename))
  (cond ((label-q wff) (apply-label wff (prettify-process-wff wff gamma argstack)))
	((lsymbol-q wff)
	 (unless (get wff 'abbrev)
	   (when (and (not (logconst-q wff)) (propsym-q wff))
	     (if (member wff gamma)
		 (setf (get wff 'bound-must-avoid) (union (list-up-to wff gamma) ; those bound between binder for wff and occurrence
							  (get wff 'bound-must-avoid)))
	       (progn
		 (unless (or (member wff fixed-frees)
			     (member wff frees-to-rename))
		   (push wff frees-to-rename)
		   (setf (get wff 'sel-var-bound) nil)
		   (setf (get wff 'free-must-avoid) nil))
		 (setf (get wff 'free-must-avoid) (union gamma (get wff 'free-must-avoid))))))))
	((boundwff-p wff)
	 (unless (or (member (caar wff) fixed-bounds)
		     (member (caar wff) bounds-to-rename))
	   (push (caar wff) bounds-to-rename)
	   (setf (get (caar wff) 'bound-try-to-equate-to-bound) nil)
	   (setf (get (caar wff) 'bound-try-to-equate-to-free) nil)
	   (setf (get (caar wff) 'bound-must-avoid) nil))
	 (when (and argstack
		    (lsymbol-p (car argstack))
		    (not (get (car argstack) 'abbrev))
		    (not (logconst-p (car argstack)))
		    (propsym-p (car argstack)))
	   (if (member (car argstack) gamma)
	       (pushnew (car argstack) (get (caar wff) 'bound-try-to-equate-to-bound))
	     (pushnew (car argstack) (get (caar wff) 'bound-try-to-equate-to-free))))
	 (prettify-process-wff (cdr wff) (cons (caar wff) gamma) (cdr argstack)))
	(t
	 (prettify-process-wff (car wff) gamma (cons (cdr wff) argstack))
	 (prettify-process-wff (cdr wff) gamma nil))))

(defun list-up-to (elt lst)
  (let (retval)
    (do ((l lst (cdr l)))
	((or (not l)
	     (eq (car l) elt))
	 retval)
      (push (car l) retval))))

(defun prettify-free-rename (oldfree newfree)
  (declare (special used-frees theta))
  (dolist (b (get oldfree 'free-must-avoid))
    (push newfree (get b 'not-alpha-image)))
  (push newfree used-frees)
  (setq theta (acons oldfree newfree theta)))
	      
(defun prettify-bound-rename (oldbound newbound)
  (declare (special alpha))
  (dolist (b (get oldbound 'bound-must-avoid))
    (push newbound (get b 'not-alpha-image)))
  (setq alpha (acons oldbound newbound alpha)))
  
(defun prettify-free-legal-p (oldfree newfree)
  (declare (special used-frees theta alpha))
  (and (not (assoc oldfree theta)) ; not already set
       (not (member newfree used-frees)) ; not in cod of theta
       (dolist (z-zp alpha t)
	 (when (and (eq (cdr z-zp) newfree)
		    (member (car z-zp) (get oldfree 'free-must-avoid)))
	   (return nil)))))

(defun prettify-bound-legal-p (oldbound newbound)
  (declare (special theta alpha))
  (and (not (assoc oldbound alpha)) ; not already set
       (not (member newbound (get oldbound 'not-alpha-image))) ; not banned by earlier commitment
       (dolist (z-zp alpha t)
	 (when (and (eq (cdr z-zp) newbound)
		    (member (car z-zp) (get oldbound 'bound-must-avoid)))
	   (return nil)))))

; can we send both y & z to v?  not just conj of both being legal
(defun prettify-free-bound-legal-p (y z v)
  (and (prettify-free-legal-p y v)
       (prettify-bound-legal-p z v)
       ; after z -> v, alpha will have this extra pair for prettify-free-legal-p to check, we do the check here
       (not (member z (get y 'free-must-avoid)))))

(defun prettify-bound-bound-legal-p (x z v)
  (and (prettify-bound-legal-p x v)
       (prettify-bound-legal-p z v)
       ; after z -> v, for each b in (get z 'bound-must-avoid), v will be put into (get b 'not-alpha-image).
       ; we must make sure x is not one such b.  That is, x must not be in (get z 'bound-must-avoid)
       (not (member x (get z 'bound-must-avoid)))
       ; after z -> v, alpha will have this extra pair for prettify-bound-legal-p to check, we do the check here
       (not (member z (get x 'bound-must-avoid)))))

; if y or z is sent to a pretty var and the other can be, do it
; if neither are sent anywhere yet, but can be sent to a common pretty var, do it.
; We have to be a bit careful about trying to send both the the same var,
; because both the subst's being legal independently does NOT imply they are
; legal together!
(defun prettify-identify-free-bound (y z)
  (declare (special theta alpha))
  (let ((yp (cdr (assoc y theta)))
	(zp (cdr (assoc z alpha))))
    (cond ((and zp yp) nil) ; already commited both
	  ((and yp (pretty-var-p yp) (prettify-bound-legal-p z yp))
	   (prettify-bound-rename z yp))
	  ((and zp (pretty-var-p zp) (prettify-free-legal-p y zp))
	   (prettify-free-rename y zp))
	  ((not (or zp yp))
	   (let ((yp1 (get-best-alt-free-name y)))
	     (if (and (pretty-var-p yp1) 
		      (prettify-free-bound-legal-p y z yp1))
		 (progn
		   (prettify-free-rename y yp1)
		   (prettify-bound-rename z yp1))
	       (let ((zp1 (get-best-alt-bound-name z)))
		 (when (and (pretty-var-p zp1)
			    (prettify-free-bound-legal-p y z zp1))
		   (prettify-free-rename y zp1)
		   (prettify-bound-rename z zp1)))))))))

(defun prettify-identify-bound-bound (x z)
  (declare (special alpha))
  (let ((xp (cdr (assoc x alpha)))
	(zp (cdr (assoc z alpha))))
    (cond ((and zp xp) nil) ; already commited both
	  ((and xp (pretty-var-p xp) (prettify-bound-legal-p z xp))
	   (prettify-bound-rename z xp))
	  ((and zp (pretty-var-p zp) (prettify-bound-legal-p x zp))
	   (prettify-bound-rename x zp))
	  ((not (or zp xp))
	   (let ((xp1 (get-best-alt-bound-name x)))
	     (if (and (pretty-var-p xp1) (prettify-bound-bound-legal-p x z xp1))
		 (progn
		   (prettify-bound-rename x xp1)
		   (prettify-bound-rename z xp1))
	       (let ((zp1 (get-best-alt-bound-name z)))
		 (when (and (pretty-var-p zp1) (prettify-bound-bound-legal-p x z zp1))
		   (prettify-bound-rename x zp1)
		   (prettify-bound-rename z zp1)))))))))
  
;;; if var is a superscript, find the lowest var of  a nameroot in its class
;;; which is not free in term
					; (this function is from the old prettify code, with extensions for proposition & relation vars)
(defun which-kind-of-var (var) 
  (let ((type (get var 'unabbreviated-type)))
    (do ((y type (car y))
	 (num-args 0 (1+ num-args)))
	((atom y)
	 (if (eq y 'O)
	     (cond ((= num-args 0)
		    proposition-var)
		   ((= num-args 1)
		    predicate-var)
		   (t relation-var))
	   (if (= num-args 0)
	       individual-var
	     function-var))))))

; var is "pretty" if it has no superscript
(defun pretty-var-p (var)
  (not (find #\^ (symbol-name var))))

					; based on prettify-term-aux in the old code
(defun get-best-alt-name (var legal-test)
  (let* ((type-string (type-to-string (get var 'unabbreviated-type)))
	 (name (symbol-name var))
	 (root (if (find #\^ name)
		   (subseq name 0 (position #\^ name :from-end t))
		 (if (find #\< name)
		     (subseq name 0 (position #\< name))
		   name)))
	 (facelist (if (or (string= root "w")
			   (string= root "h"))
		       (which-kind-of-var var) 
		     (list (intern-str root))))
	 (count 0))
    (do* ((face facelist (or (cdr face) (and (incf count) facelist)))
	  (newvar (intern-str (format nil "~A~A" (car face)
				      type-string))
		  (if (zerop count)
		      (intern-str (format nil "~A~A" (car face)
					  type-string))
		    (intern-str (format nil "~A^~A~A" (car face)
					count type-string)))))
	((funcall legal-test var newvar)
	 (putprop newvar (get var 'type) 'type)
	 (putprop newvar
		  (get var 'unabbreviated-type)
		  'unabbreviated-type)
	 (unless (zerop count)
	   (set-superscript-face (getnameroot newvar)))
	 newvar))))

(defun get-best-alt-free-name (y)
  (get-best-alt-name y #'prettify-free-legal-p))
    
(defun get-best-alt-bound-name (z)
  (get-best-alt-name z #'prettify-bound-legal-p))
    
; theta - renaming for free vars
; alpha - renaming for bound vars
(defun rename-all-vars-in-etree (theta alpha etree)
  (typecase 
   etree
   (leaf (setf (leaf-shallow etree) (rename-all-vars-in-wff theta alpha (leaf-shallow etree))))
   (rewrite
    (setf (rewrite-shallow etree) (rename-all-vars-in-wff theta alpha (rewrite-shallow etree)))
    (when (rewrite-ruleq-shallow etree)
      (setf (rewrite-ruleq-shallow etree) (rename-all-vars-in-wff theta alpha (rewrite-ruleq-shallow etree)))))
   (selection
    (setf (selection-shallow etree) (rename-all-vars-in-wff theta alpha (selection-shallow etree)))
    (setf (selection-terms etree) (list (rename-all-vars-in-wff theta alpha (car (selection-terms etree))))))
   (skolem
    (setf (skolem-shallow etree) (rename-all-vars-in-wff theta alpha (skolem-shallow etree)))
    (setf (skolem-terms etree) (list (rename-all-vars-in-wff theta alpha (car (skolem-terms etree))))))
   (expansion
    (setf (expansion-shallow etree) (rename-all-vars-in-wff theta alpha (expansion-shallow etree)))
    (setf (expansion-terms etree)
	  (mapcar #'(lambda (x)
		      (rename-all-vars-in-wff theta alpha x))
		  (expansion-terms etree))))
   (otherwise nil))
  (dolist (kid (etree-components etree))
	  (rename-all-vars-in-etree theta alpha kid)))

; returns T when the situation is [lambda x1 . . . lambda x2 . . . x1 . . .] => [lambda y . . . lambda y . . . y . . .]
; in such a case we have old = x1, new = y, gamma = (x2 . . . x1 . . .), newgamma = (y . . . y . . . )
; and so the new var occurs in gamma before the old var occurs in newgamma
(defun scope-problem-p (old new gamma newgamma)
  (let ((p (position old gamma))
	(n (position new newgamma)))
    (not (equal p n))))

; fails if variable capture occurs.
; this should not happen if we ensure:
;     (*) For any subwff Gamma|binder x . M
;         with z in Gamma and z (var other than x) free in M, alpha(x) is distinct from alpha(z).
;    (**) For any subwff Gamma|binder x . M
;         with y not in Gamma and y (var other than x) free in M, alpha(x) is distinct from theta(y).
; The construction of theta and alpha above should ensure these conditions hold.
(defun rename-all-vars-in-wff (theta alpha wff &optional gamma newgamma)
  (cond ((label-q wff) (apply-label wff (rename-all-vars-in-wff theta alpha wff gamma newgamma)))
	((lsymbol-q wff)
	 (cond ((logconst-q wff) wff)
	       ((or (propsym-q wff)
		    (pmpropsym-q wff))
		(let ((newvar
		       (if (member wff gamma)
			   (or (cdr (assoc wff alpha))
			       wff)
			 (or (cdr (assoc wff theta))
			     wff))))
		  (when (and (member newvar newgamma) ; will be bound in new fmla
			     (or (not (member wff gamma)) ; but wasn't bound in old fmla
				 (scope-problem-p wff newvar gamma newgamma)))
		    (throwfail "Prettify Failed: Variable Capture " wff " -> " newvar))
		  newvar))
	       (t wff)))
	((boundwff-q wff)
	 (let ((newbd (or (cdr (assoc (caar wff) alpha))
			  (caar wff))))
	   (acons newbd
		  (cdar wff)
		  (rename-all-vars-in-wff theta alpha (cdr wff)
					  (cons (caar wff) gamma)
					  (cons newbd newgamma)))))
	(t
	 (cons (rename-all-vars-in-wff theta alpha (car wff) gamma newgamma)
	       (rename-all-vars-in-wff theta alpha (cdr wff) gamma newgamma)))))

#|
Cleaning up the expansion tree:
This procedure comes after merging, because we assume that all
exp-vars and skolem-terms have been removed, leaving just ordinary wffs.

  1.  At each expansion term, normalize it and reduce superscripts on
      the bound variables, and make a new expansion 
      which is a "copy", 
    a.  remove unnecessary lambda-norm steps.
    b.  make the leaves the same name, so mating still holds
  2.  Remove original expansion.
In reality, we just create a whole new expansion tree, not sharing
with original tree at all.
|#

(defun cleanup-etree (etree)
  (let ((shallow (get-shallow etree)))
    (cleanup-all-expansions etree shallow nil
			    (case lambda-conv
			      (beta-only
			       (wffeq shallow
				      (lambda-norm shallow)))
			      ((beta-eta-separate beta-eta-together)
			       (wffeq shallow
				      (etanorm (lambda-norm shallow))))))))

(defun cleanup-all-expansions (etree shallow parent lambda-normal-p)
  (when (and merge-debug (not (wffeq-defeq (get-shallow etree) shallow)))
    (msgf "WARNING: etree " etree ": " ((get-shallow etree) . gwff) t 
	  "does not match shallow " (shallow . gwff) t
	  "in cleanup-all-expansions"))
  (etypecase etree
    ((or true false)
     (let ((newetree (copy-etree etree)))
       (setf (etree-parent newetree) parent)
       newetree))
    (empty-dup-info
     (let ((newetree (copy-empty-dup-info etree)))
       (setf (empty-dup-info-shallow newetree) shallow)
       (setf (etree-parent newetree) parent)
       newetree))
    (leaf 
     (let ((newetree (copy-leaf etree)))
       (setf (leaf-shallow newetree) shallow)
       (setf (etree-parent newetree) parent)
       newetree))
    (negation
     (let ((newetree (copy-negation etree)))
       (setf (etree-parent newetree) parent)
       (setf (etree-components newetree) 
	 (list (cleanup-all-expansions (car (etree-components etree))
				       (cdr shallow)
				       newetree lambda-normal-p)))
       newetree))
    ((or econjunction edisjunction implication)
     (let ((newetree (copy-etree etree)))
       (setf (etree-parent newetree) parent)
       (setf (etree-components newetree)
	 (list (cleanup-all-expansions (car (etree-components etree))
				       (glr shallow)
				       newetree
				       lambda-normal-p)
	       (cleanup-all-expansions (cadr (etree-components etree))
				       (grr shallow)
				       newetree
				       lambda-normal-p)))
       newetree))
    (skolem
     (let ((newetree (copy-skolem etree)))
       (setf (etree-parent newetree) parent)
       (setf (skolem-shallow newetree) shallow)
       (setf (etree-components newetree)
	 (list (cleanup-all-expansions
		(car (etree-components etree))
		(substitute-term-var (car (skolem-terms etree))
				     (caar shallow)
				     (cdr shallow))
		newetree
		lambda-normal-p)))
       newetree))
    (selection
     (let ((newetree (copy-selection etree)))
       (setf (etree-parent newetree) parent)
       (setf (selection-shallow newetree) shallow)
       (setf (etree-components newetree)
	 (list (cleanup-all-expansions
		(car (etree-components etree))
		(substitute-term-var (car (selection-terms etree))
				     (caar shallow)
				     (cdr shallow))
		newetree
		lambda-normal-p)))
       newetree))
    (expansion
     (let ((kids (etree-components etree))
	   (terms (expansion-terms etree))
	   (newetree (copy-expansion etree)))
       (setf (etree-parent newetree) parent)
       (setf (expansion-shallow newetree) shallow)
       (do ((kids kids (cdr kids))
	    (terms terms (cdr terms))
	    (new-kids nil)
	    (new-terms nil))
	   ((null kids)
	    (setf (etree-components newetree) (nreverse new-kids))
	    (setf (expansion-terms newetree) (nreverse new-terms)))
	 (multiple-value-bind (new-kid new-term)
	     (cleanup-expansion (car kids) (car terms) newetree
				shallow)
	   (push new-kid new-kids)
	   (push new-term new-terms)))
       newetree))
    (rewrite (cleanup-rewrite-node etree shallow parent lambda-normal-p))))

(defun cleanup-expansion (kid term parent shallow)
  (let* ((new-term (lnorm term))
	 (newshallow 
	  (substitute-l-term-var new-term (caar shallow) (cdr shallow)))
         (beta-normed-newshallow (lnorm-beta newshallow))
	 (lambda-normed-newshallow 
             (if (eq 'beta-only lambda-conv)
                 beta-normed-newshallow 
	       (etanorm beta-normed-newshallow))))
    (values 
     (cond ((wffeq newshallow lambda-normed-newshallow)
	    (cleanup-all-expansions kid newshallow parent t))
	   ((and (rewrite-p kid)
	         (or (eq 'lambda (rewrite-justification kid))
                     (and (eq 'beta-only lambda-conv)
                          (eq 'beta (rewrite-justification kid)))))
	    (cleanup-all-expansions kid newshallow parent nil))
           ((eq 'beta-eta-separate lambda-conv)
            (let ((new-rewrite-beta
                    (if (not (wffeq newshallow beta-normed-newshallow))
                        (make-rewrite :shallow newshallow
				      :parent parent
				      :justification 'beta
				      :junctive 'neutral)))
                  (new-rewrite-eta
                    (if (not (wffeq beta-normed-newshallow lambda-normed-newshallow))
                        (make-rewrite :shallow beta-normed-newshallow
				      :parent parent
				      :justification 'eta
				      :junctive 'neutral))))
                  (cond ((and new-rewrite-beta new-rewrite-eta)
                         (setf (etree-components new-rewrite-eta) (list kid))
                         (setf (etree-components new-rewrite-beta)
                                (list (cleanup-all-expansions new-rewrite-eta beta-normed-newshallow 
					                      new-rewrite-beta nil)))
                         new-rewrite-beta)
                        (new-rewrite-beta 
                         (setf (etree-components new-rewrite-beta)
                               (list (cleanup-all-expansions kid lambda-normed-newshallow 
					                     new-rewrite-beta T)))
                         new-rewrite-beta)
                        (new-rewrite-eta 
                         (setf (etree-components new-rewrite-eta)
                                (list (cleanup-all-expansions kid lambda-normed-newshallow 
					                      new-rewrite-eta T)))
                         new-rewrite-eta))))
	   (T
	    (let ((new-rewrite (make-rewrite :shallow newshallow
					     :parent parent
					     :justification 
                                              (if (eq 'beta-eta-together lambda-conv) 'lambda 'beta)
					     :junctive 'neutral)))
	      (setf (etree-components new-rewrite)
		(list (cleanup-all-expansions kid lambda-normed-newshallow 
					      new-rewrite
					      t)))
	      new-rewrite)))
     new-term)))

(defun cleanup-rewrite-node (etree shallow parent lambda-normal-p)
  (declare (special min-quant-etree))
  (cond 
   ((member (rewrite-justification etree) '(lambda beta eta ab))
    (if lambda-normal-p
	(cleanup-all-expansions (car (etree-components etree))
				shallow parent t)
      (let ((newetree (copy-rewrite etree))
	    (nf (lambda-norm shallow)))
	(setf (etree-parent newetree) parent)
	(setf (rewrite-shallow newetree) shallow)
	(setf (etree-components newetree)
	      (list (cleanup-all-expansions (car (etree-components etree))
					    (case lambda-conv
						  (beta-only nf)
						  (beta-eta-separate 
                                                   (if (eq (rewrite-justification etree) 'beta)
                                                       nf (etanorm shallow)))
                                                 (beta-eta-together (etanorm (lambda-norm shallow))))
					       newetree 
                                           (or (neq lambda-conv 'beta-eta-separate)
                                               (eq (rewrite-justification etree) 'eta)
                                               (eq nf (etanorm nf))))))
	    newetree)))
   ((and (member (rewrite-justification etree) '(subst= leibniz= ext= both=))
	 (equals-p shallow))
    (let* ((newetree (copy-rewrite etree))
	   (new-wff (let ((rewrite-equalities
			   (if (eq (rewrite-justification newetree) 'ext=)
			       'only-ext
			     (if (eq (rewrite-justification newetree) 'both=)
				 'core::both
			       (if (member (rewrite-justification newetree) 
					   '(subst= leibniz=))
				   'leibniz
				 rewrite-equalities)))))
		      (declare (special rewrite-equalities))
		      (expand-top= shallow)))
		(newshallow 
		 (if (or (eq (rewrite-justification etree) 'both=) ; cebrown
			 (eq (gar (gar new-wff)) 'equiv))
		     new-wff
		   (lcontr 
		    (cons (lcontr (gar new-wff)) (gdr new-wff))))))
	   (setf (etree-parent newetree) parent)
	   (setf (rewrite-shallow newetree) shallow)
	   (setf (etree-components newetree)
		 (list (cleanup-all-expansions (car (etree-components
						     etree))
					       newshallow
					       newetree
					       (wffeq (lambda-norm newshallow)
						      newshallow))))
	   (when (and (positive-p newetree)
		      (eq 'subst= (rewrite-justification newetree)))
	     (let ((exp-node (find-etree-node #'expansion-p newetree t)))
	       (when exp-node
		 (dolist (son (etree-components exp-node))
			 (let ((imp-node (find-etree-node #'implication-p son t)))
			   (when imp-node
			     (reduce-rewrites imp-node) 
			     (remove-double-negations-merge imp-node)
			     (setf (etree-parent imp-node) exp-node)
			     (update-global-lists son imp-node)
			     ;; added to make sure the equality rule can be applied
			     (check-shallow-formulas newetree exp-node imp-node)
			     ))))))
	   newetree))
   ((or (eq (rewrite-justification etree) 'equiv-implics)
	(eq (rewrite-justification etree) 'equiv-disjs)) ; equiv-disjs should already be
					; removed if USE-RULEP is NIL
    (let* ((newetree (copy-rewrite etree))
	   (left-arg (cdar shallow))
	   (rt-arg (cdr shallow))
	   (newshallow 
	    (if (eq 'equiv-implics (rewrite-justification etree))
		(acons 'and (acons 'implies left-arg rt-arg)
		       (acons 'implies  rt-arg left-arg))
	      (acons 'or (acons 'and  left-arg rt-arg)
		     (acons 'and (cons 'not left-arg) (cons 'not rt-arg))))))
      (setf (etree-parent newetree) parent)
      (setf (rewrite-shallow newetree) shallow)
      (setf (etree-components newetree)
	    (list (cleanup-all-expansions (car (etree-components
						etree))
					  newshallow
					  newetree
					  lambda-normal-p)))
      newetree))
   ((eq (rewrite-justification etree) 'refl=)
     (if (and (equals-p shallow)
	      (wffeq (cdar shallow) (cdr shallow))) ; A = A
	 (let ((newetree (copy-rewrite etree))
	       (newshallow 'truth))
	   (setf (etree-parent newetree) parent)
	   (setf (rewrite-shallow newetree) shallow)
	   (setf (etree-components newetree)
	     (list (cleanup-all-expansions (car (etree-components etree))
					   newshallow
					   newetree
					   lambda-normal-p)))
	   newetree)
					; otherwise, do something like what remove-leibniz does
       (let* ((list-of-rewrites nil))
	 (do ((wff shallow))
	     ((and (equals-p wff) (wffeq (cdar wff) (cdr wff)))
	      (push (cons wff 'REFL=) list-of-rewrites))
	   (if (and (equals-p wff) (wffeq-ab (cdar wff) (cdr wff)))
	       (progn
		 (unless (or (and (not list-of-rewrites) parent (rewrite-p parent)
				  (member (rewrite-justification parent) '(EQUIVWFFS LAMBDA AB)))
			     (and list-of-rewrites (member (cdar list-of-rewrites) '(EQUIVWFFS LAMBDA AB))))
		   (push (cons wff 'AB) list-of-rewrites))
		 (setq wff (cons (car wff) (cdar wff))))
	     (let ((wff* (lnorm wff)))
	       (if (wffeq wff* wff)
		   (if (contains-defn-not-equiv wff)
		       (progn
			 (unless (and (not list-of-rewrites) ; first new rewrite
				      parent (rewrite-p parent) (eq (rewrite-justification parent) 'EQUIVWFFS)) ; parent is already EQUIVWFFS, absorb together
			   (push (cons wff 'EQUIVWFFS) list-of-rewrites))
			 (setq wff (instantiate-all wff '(EQUIV))))
		     (throwfail "Merging Bug in Cleanup, Don't know how to handle REFL= node: " etree))
		 (progn
		   (unless (or (and (not list-of-rewrites)
				    parent (rewrite-p parent) 
				    (member (rewrite-justification parent) '(EQUIVWFFS LAMBDA)))
			       (and list-of-rewrites (member (cdar list-of-rewrites) '(EQUIVWFFS LAMBDA))))
		     (push (cons wff 'LAMBDA) list-of-rewrites))
		   (setq wff wff*))))))
	 (let* ((new-rewrites (mapcar #'(lambda (x) (make-rewrite :positive (etree-positive etree)
								  :shallow (car x)
								  :junctive 'neutral
								  :justification (cdr x)))
				      (reverse list-of-rewrites)))
		(new-rewrite (do ((new-rewrites (cdr new-rewrites) (cdr new-rewrites))
				  (rewrite (car new-rewrites) (car new-rewrites))
				  (previous parent rewrite)
				  (first (car new-rewrites)))
				 ((null rewrite) 
				  first)
			       (setf (etree-parent rewrite) previous)
			       (setf (etree-components rewrite)
				 (if (car new-rewrites)
				     (list (car new-rewrites))
				   (list (make-true :junctive 
						    (if (etree-positive etree) 'CON 'DIS)
						    :positive (etree-positive etree))))))))
	   (update-statuses new-rewrite)
	   new-rewrite))))
    ((eq (rewrite-justification etree) 'truthp)
         (let ((newetree (copy-rewrite etree))
	       (newshallow (acons 'OR (falsehood-elim (get-shallow etree)) 
                                      (cons 'NOT 'TRUTH))))
           (setf (etree-parent newetree) parent)
	   (setf (rewrite-shallow newetree) shallow)
	   (setf (etree-components newetree)
	         (list (cleanup-all-expansions (car (etree-components
					       etree))
					 newshallow
					 newetree
					 (wffeq (lambda-norm newshallow)
						newshallow))))
	   newetree))
    ((member (rewrite-justification etree) '(ruleq ruleq-univ))
       (let* ((newetree (copy-rewrite etree))
              (binder (if (positive-p etree) 'forall 'exists))
	      (newshallow (if (rewrite-ruleq-shallow etree)
			      (rewrite-ruleq-shallow etree)
			      (case (rewrite-justification etree)
				    (ruleq (min-quant-scope shallow))
				    (ruleq-univ shallow))))
;;;no difference at all because there are no quantifiers outside.
;;;The free variables in PARENT should not be counted.
              (ok-vars (if parent 
                           (set-difference (etree-free-vars newetree)
                                           (etree-free-vars parent) :test #'equal)
                           (etree-free-vars newetree))))
	 (setf (etree-parent newetree) parent)
	 (setf (rewrite-shallow newetree) shallow)
	 (setf (etree-components newetree)
	   (list (cleanup-all-expansions (car (etree-components etree))
                                         (dolist (var (nreverse ok-vars) newshallow)
					    (setq newshallow (acons var binder newshallow)))
					 newetree
					 (wffeq (lambda-norm newshallow)
						newshallow))))
	 newetree))
    ((eq (rewrite-justification etree) 'dual)
     (let* ((newdual (copy-rewrite etree)))
       (setf (etree-parent newdual) parent)
       (setf (rewrite-shallow newdual) shallow)
       (setf (etree-components newdual)
	     (list (cleanup-all-expansions (car (etree-components etree))
					   (if (etree-positive etree)
					       (acons 'AND shallow shallow)
					     (acons 'OR shallow shallow))
					   newdual
					   lambda-normal-p)))
       newdual))
    (t ; equivwffs, or Leibniz=, or Ext= (for an embedded =)
 ; major changes to this case - cebrown 7/01
     (let* ((newetree (copy-rewrite etree))
	    (old-sh (get-shallow (car (etree-components etree))))
	    (reds (common-defn-eq-refl=-lam-reduct shallow old-sh)))
       (unless reds
	 (throwfail "Could not clean up " etree))
       (if (cdar reds)
	   (do ((chain (car reds) (cddr chain))
		(etree2 newetree (let ((etree3 (copy-rewrite etree)))
				   (setf (rewrite-name etree3)
					 (intern-str (create-namestring rewrite-name)))
				   etree3))
		(parent2 parent etree2))
	       ((null (cdr chain))
		(setf (etree-components parent2)
		      (list (cleanup-all-expansions (car (etree-components etree))
						    (car chain) parent2
						    (wffeq (lnorm (car chain))
							   (car chain))))))
	       (setf (etree-parent etree2) parent2)
	       (setf (rewrite-justification etree2) 
		     (case (caadr chain)
			   ((DEFN BINDER) 'EQUIVWFFS)
			   (EXTENS 'EXT=)
			   (LEIBNIZ 'LEIBNIZ=)
			   (t (caadr chain))))
	       (setf (rewrite-shallow etree2) (car chain))
	       (unless (eq parent2 parent)
		 (setf (etree-components parent2)
		       (list etree2))))
	 (setq newetree (cleanup-all-expansions (car (etree-components etree)) shallow
						parent (wffeq (lnorm shallow) shallow))))
       newetree))))

