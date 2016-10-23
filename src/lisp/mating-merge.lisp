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

(deffile mating-merge
  (extension lisp)
  (part-of mating-transform)
  (mhelp "Contains functions for merging two expansion trees."))

(defmateop merge-tree
  (mate-alias merge-tree)
  (mate-applicable-p (lambda () (and (boundp 'current-eproof)
				     (eproof-p current-eproof)
				     (etree-p (eproof-etree current-eproof))
				     (boundp 'active-mating)
				     (mating-p active-mating)
				     (mating-completep active-mating)
				     (not (eproof-merged current-eproof)))))
  (mhelp "If the mating is complete, applies substitutions to the expansion 
tree, then applies Pfenning's MERGE algorithm, eliminating redundant 
expansion terms."))

;;; Added support for equality transformations DAN 29OCT88
;;; Added cleanup-etree stuff DAN 7JAN90

(defun merge-tree (&key (num-support-lines 0))
  (unwind-protect 
    (merge-tree-real :num-support-lines num-support-lines)
    (progn (breakcount 'merge) (display-time 'merge))))

;;;The following lists of vars will be used later in prettifying functions.
(defvar predicate-var '("P" "Q" "R" "p" "q" "r"))
(defvar proposition-var '("A" "B" "C" "D"))
(defvar relation-var '("R" "S" "B" "C" "D" "r" "s" "b" "c" "d"))
(defvar function-var '("f" "g" "h" "j" "F" "H" "G" "J"))
(defvar individual-var '("u" "v" "w" "x" "y" "z"))

(defun randomvars ()
   (setq predicate-var (randomlist predicate-var))
   (setq function-var (randomlist function-var))
   (setq individual-var (randomlist individual-var)))

(defun randomlist (list)
  (when list
     (let* ((len (length list))
            (ran (random len))
            (first (elt list ran)))
       (if (zerop ran) 
           (setq list (cdr list))
           (let ((nthcdr (nthcdr (1- ran) list)))
              (rplacd nthcdr (cddr nthcdr))))
       (cons first (randomlist list)))))

(defvar merge-debug nil) 
;;should be NIL, ETD for reduced information (ie the ETD command), or 
;;anything else for full information (ie the ETP command).

(defun merge-display (string &optional (etree (eproof-etree current-eproof)))
  (when (and merge-debug (query string nil))
	(if (eq merge-debug 'etd)
	    (display-etree-main etree)
	  (display-etree-all-main etree))))


(defun merge-option (string)
  (or (not merge-debug)
      (query string t)))

(defun merge-tree-real (&key (num-support-lines 0))
  (startcount 'merge)
;  (randomvars)  ; this was probably to prevent reusing the same var names all the time, but it makes bugs harder to reproduce.
					; We could reintroduce it with a flag to turn it on and off - cebrown 11/20/00
  (cr-eproof-jform) ;necessary for prune-unmated-rewrites MB Wed Feb 12 12:56:59 1997
;  (set-all-parents (eproof-etree current-eproof)) ; cebrown 9/7/01, commented because we should never need it, but if a bug crops up, we may find out we do need it.
  (merge-display "Welcome to the merging bug-o-rama! Show the etree?")
  (multiple-value-bind (etree new-conn-list)
      (etr-merge (eproof-etree current-eproof) active-mating)
      ;;;it is supposed to be a right place to put MIN-QUANT-ETREE-APP.
      ;;;Unfortunately, this doesn't. A great deal of care has to be taken.
    ;; make sure all remaining branches are visible
      (declare (special etree))  ; cebrown 4/5/99 (so remove-leibniz can modify etree)
    (update-statuses etree)
    (setq new-conn-list (delete-duplicates new-conn-list :test #'equal))
    (dolist (conn (eproof-dissolve current-eproof)) ; cebrown 10/14/01 - add these conns
	    (let ((l (find-etree-node-name (car conn)))
		  (r (find-etree-node-name (cdr conn))))
	      (when (and l r)
		(push (cons l r) new-conn-list))))
    ;; don't want any conns from nodes that no longer are in tree.
    (setq new-conn-list
      (delete-if #'(lambda (conn) 
		     (or (not (find-etree-node-name (etree-name (car conn))
						    etree t))
			 (not (find-etree-node-name (etree-name (cdr conn))
						    etree t))))
		 new-conn-list))
    ;; now try to get rid of unneeded branches
    (when (and (lazy2-used) *hacked-rewrites-list*)
	  (cr-eproof-jform)
	  (merge-display "About to modify dual rewrites. Show the etree?" etree)
	  (when (merge-option "Modify Dual Rewrites?")
	    (multiple-value-setq
		(etree new-conn-list)
		(modify-dual-rewrites etree new-conn-list)))
	  (update-statuses etree)
	  (setq new-conn-list
		(delete-if #'(lambda (conn) 
			       (or (not (find-etree-node-name (etree-name (car conn))
							      etree t))
				   (not (find-etree-node-name (etree-name (cdr conn))
							      etree t))))
			   new-conn-list))
	  (update-statuses etree))
    (merge-display "About to prune unmated branches. Show the etree?" etree)
    (when (merge-option "Prune unmated branches?")
      (multiple-value-setq
	  (etree new-conn-list)
	(prune-unmated-branches etree new-conn-list)))
    (update-statuses etree)
    (when (eproof-skolem-method current-eproof)
	  (merge-display "About to substitute skolem terms. Show the etree?" etree)
	  (subst-skol-terms etree)
	  (setf (eproof-skolem-method current-eproof) nil))
    (update-statuses etree)
    (when (and (implication-p etree)
	       (rewrite-p (car (etree-components etree)))
	       (eq (rewrite-justification (car (etree-components etree))) 'add-truth))
	  (setq etree (cadr (etree-components etree))))
    ;do this twice... we have a T and a F, possibly...
    (when (and (implication-p etree)
	       (rewrite-p (car (etree-components etree)))
	       (eq (rewrite-justification (car (etree-components etree))) 'add-truth))
	  (setq etree (cadr (etree-components etree))))
    (update-statuses etree)
    (when remove-leibniz
	  (merge-display "About to remove leibniz nodes. Show the etree?" etree)
	  (when (merge-option "Remove leibniz nodes?")
		(setq new-conn-list
		      (remove-leibniz-nodes new-conn-list etree))))
    (update-statuses etree)
    ;;;this is a hack: if (etree-leaf etree) is tree, then
    ;;;etree comes from NAT-ETRE. Hence this is no need of
    ;;;substitution. This should be done by introducing another
    ;;;slot for ETREE datastucture.
    (if (not (etree-leaf etree))
	(progn (merge-display "About to substitute for parameters. Show the etree?" etree)
	       (subst-vars-for-params etree))
      (setf (etree-leaf etree) nil))
    (merge-display "About to raise lambda nodes. Show the etree?" etree)
    (when (merge-option "Raise lambda nodes?")
      (multiple-value-setq
       (etree new-conn-list)
       (raise-lambda-nodes etree new-conn-list	; cebrown 11/3/00, now modifies the conn-list by moving conns from lambda rewrites
			   (eproof-lemmas current-eproof)
			   :num-support-lines num-support-lines ; cebrown 6/12/2004 - to prevent lambda's from being raised above implication and conjunction for support lines
			   )))
    (update-statuses etree)
    (merge-display "About to translate to propositional jform. Show the etree?" etree)
    (setf (eproof-allow-nonleaf-conns current-eproof) ; cebrown 11/3/00, records which nonleaves nodes are in the mating, to put them in the jform
      (mapcan #'(lambda (x)
		  (if (leaf-p (car x))
		      (if (leaf-p (cdr x))
			  nil
			(list (etree-name (cdr x))))
		    (if (leaf-p (cdr x))
			(list (etree-name (car x)))
		      (list (etree-name (car x)) (etree-name (cdr x))))))
	      new-conn-list))
    (setf (eproof-jform current-eproof) (etree-to-prop-jform etree))
    (let ((new-mating (init-mating)))
      (setf (mating-completep new-mating) t)
      (do ((jform (eproof-jform current-eproof))
	   (conn-list (cdr new-conn-list) (cdr conn-list))
	   (conn (car new-conn-list) (car conn-list))
	   (cgraph (cgraph)))
	  ((null conn) 
	   (push new-mating (mating-list)) 
	   (setf (mating-clist new-mating) new-conn-list)
	   (setf (eproof-merged current-eproof) t)
	   (setq active-mating new-mating)
	   etree)
	(let ((first (find-jform-name (etree-name (car conn)) jform))
	      (sec (find-jform-name (etree-name (cdr conn)) jform)))
	  (insert-in-cgraph (cons (etree-name (car conn)) 
				  (etree-name (cdr conn)))
			    (cons first sec) nil nil cgraph))))
    (merge-display "About to clean up etree. Show the etree?" etree)
    (when (merge-option "Clean up the etree?")
	  (setq etree (cleanup-etree etree)))
    (update-statuses etree)
    (merge-display "About to prettify etree. Show the etree?" etree)
    (when (merge-option "Prettify the etree?")
	  (setf (eproof-etree current-eproof)
 		(prettify-etree etree))) ; used to be done in cleanup-etree, cebrown 11/22/00
    (setq current-topnode (eproof-etree current-eproof))
    (setf (eproof-jform current-eproof) (etree-to-prop-jform current-topnode))
    (update-statuses current-topnode)
    (merge-display "Done. Show the etree?")
    current-topnode)
    (runcount 'merge)
    (get-possible-newabbs))


;;; In shallow-merge, assume that P and Q are expansion trees with
;;; identical shallow formulas.

(defun wffeq-ab1-lazy2 (wff1 wff2 varstack)
  (when (lsymbol-q wff1) (setq wff1 (or (cdr (assoc wff1 *instantiated-defs-list*)) wff1)))
  (when (lsymbol-q wff2) (setq wff2 (or (cdr (assoc wff2 *instantiated-defs-list*)) wff2)))
  (cond ((lsymbol-q wff1)
	 (cond ((not (lsymbol-q wff2)) nil)
	       ((or (logconst-q wff1) (logconst-q wff2)) (eq wff1 wff2))
	       ((and (propsym-q wff1) (propsym-q wff2))
		(true-correspondence wff1 wff2 varstack))
	       (t (eq wff1 wff2))))
	((lsymbol-q wff2) nil)
	((boundwff-q wff1)
	 (cond ((not (boundwff-q wff2)) nil)
	       ((and (eq (cdar wff1) (cdar wff2))
		     (type-equal (caar wff1) (caar wff2)))
		(wffeq-ab1-lazy2 (cdr wff1) (cdr wff2)
			   (acons (caar wff1) (caar wff2) varstack)))
	       (T nil)))
	((boundwff-q wff2) nil)
	(t (and (wffeq-ab1-lazy2 (car wff1) (car wff2) varstack)
		(wffeq-ab1-lazy2 (cdr wff1) (cdr wff2) varstack)))))

(defun fiddle-with-hrl (rew new)
  (setq *hacked-rewrites-list*
	(mapcar #'(lambda (x) (when (eq (car x) rew)
				    (setf (car x) new))
		    x) *hacked-rewrites-list*))
  (setq *hacked-rewrites-list* (remove-duplicates *hacked-rewrites-list* :test 'equal :key 'car)))

(defun fiddle-with-hrl2 (leaf new)
  (setq *hacked-rewrites-list*
	(mapcar #'(lambda (x) (when (eq (cddr x) leaf)
				    (setf (cddr x) new))
		    x) *hacked-rewrites-list*)))

(defun shallow-merge (P Q parent)
  "Carries out shallow merge, what is called \"m\" in Pfenning's thesis."
  (declare (special merge-theta merge-list merge-mating))
  (let ((R
	 (cond ((empty-dup-info-p P)
		(setf (etree-parent Q) parent) Q)
	       ((empty-dup-info-p Q)
		(setf (etree-parent P) parent) P)
	       ((leaf-p P)
		(if (leaf-p Q)
		    (if merge-theta
			(let ((newleaf
			       (make-leaf
				:shallow
				(simul-substitute-term-var merge-theta (get-shallow P))
				:positive (etree-positive P)
				:free-vars (etree-free-vars P)
				:components nil
				:parent parent
				:junctive nil)))
			  (fiddle-with-hrl2 P newleaf)
			  (fiddle-with-hrl2 Q newleaf)
			  newleaf)
		      (progn (setf (etree-parent P) parent) P))
		  (progn (setf (etree-parent Q) parent) Q)))
	       ((leaf-p Q)
		(setf (etree-parent P) parent) 
		P)
	       ((true-p P)
		(setf (etree-parent P) parent) 
		P)
	       ((false-p P)
		(setf (etree-parent P) parent) 
		P)
	       ((rewrite-p P) ; must be careful here
		(if (and (rewrite-p Q)
			 ;;; changed wffeq to wffeq-ab DAN 24OCT88
		         (wffeq-ab (get-shallow (car (etree-components P)))
				   (get-shallow (car (etree-components Q)))))
		    (let* ((new (make-rewrite
				 :positive (positive-p P)
				 :parent parent
				 :free-vars (etree-free-vars P)
				 :justification (rewrite-justification P)
				 :junctive (etree-junctive P)))
			   (newkid
			    (shallow-merge (car (etree-components P))
					   (car (etree-components Q)) new)))
		      (fiddle-with-hrl P new)
		      (fiddle-with-hrl Q new)
		      (setf (etree-components new) (list newkid))
					;(setf (etree-parent P) parent)
		      (setf (rewrite-shallow new)
			    (simul-substitute-term-var merge-theta
						       (rewrite-shallow P)))
		      new)
		  (let ((new (make-econjunction
		     :positive (positive-p P)
		     :parent parent
		     :free-vars (append (etree-free-vars P) (etree-free-vars Q))
		     :junctive (if (positive-p P) 'con 'dis)))) ;'dis 'con))))
		    (setf (etree-components new) (list P Q))
		    (setf (etree-parent P) new)
		    (setf (etree-parent Q) new)
		    new))
       		) 
	       ((rewrite-p Q) ; must be careful here
		;the case rewrite-p P has already happened
		  (let ((new (make-econjunction
		     :positive (positive-p Q)
		     :parent parent
		     :free-vars (append (etree-free-vars P) (etree-free-vars Q))
		     :junctive (if (positive-p Q) 'con 'dis)))) ;'dis 'con))))
		    (setf (etree-components new) (list P Q))
		    (setf (etree-parent P) new)
		    (setf (etree-parent Q) new)
		    new))
	       ((econjunction-p P)
		(let ((new
		       (make-econjunction 
			:positive (positive-p P)
			:parent parent
			:free-vars (etree-free-vars P)
			:junctive (etree-junctive P)
			)))
		  (setf (etree-components new)
			(list (shallow-merge (car (etree-components P))
					     (car (etree-components Q)) new)
			      (shallow-merge (cadr (etree-components P))
					     (cadr (etree-components Q)) new)))
		  new))
	       ((edisjunction-p P)
		(let ((new (make-edisjunction 
		 :positive (positive-p P)
		 :parent parent
		 :free-vars (etree-free-vars P)
		 :junctive (etree-junctive P)
		 )))
		  (setf (etree-components new)
			  (list (shallow-merge (car (etree-components P))
					       (car (etree-components Q)) new)
				(shallow-merge (cadr (etree-components P))
					       (cadr (etree-components Q)) new)))
		  new))
	       ((implication-p P)
		(let ((new (make-implication 
		 :parent parent
		 :positive (positive-p P)
		 :free-vars (etree-free-vars P)
		 :junctive (etree-junctive P)
		 )))
		  (setf (etree-components new)
			  (list (shallow-merge (car (etree-components P))
					       (car (etree-components Q)) new)
				(shallow-merge (cadr (etree-components P))
					       (cadr (etree-components Q)) new)))
		  new))

	       ((skolem-p P)
		 (let* ((new-skolem-term 
			(copy-skolem-term
			     (car (skolem-terms P))))
		       (new-parameter
			(make-skolem-fn 
			 (skolem-term-parameter new-skolem-term) nil)))
		  (push (cons (skolem-term-parameter new-skolem-term)
			      new-parameter)
			merge-theta)
		  (push (cons (skolem-term-parameter (car (skolem-terms Q)))
			      new-parameter)
			merge-theta)
		  (setf (skolem-term-parameter new-skolem-term)
			new-parameter)
		  (let ((new (make-skolem
		   :shallow
		   (simul-substitute-term-var merge-theta (skolem-shallow P))
		   :positive (etree-positive P)
		   :junctive (etree-junctive P)
		   :parent parent
		   :free-vars (etree-free-vars P)
		   :terms (list new-skolem-term))))
		    (setf (etree-components new)	
			  (list (shallow-merge (car (etree-components P))
					       (car (etree-components Q)) new)))
		  new)))
	       ((selection-p P)
		(let ((parameter (car (selection-terms P))))
		  (push (cons (car (selection-terms Q)) parameter) merge-theta)
		  (let ((new (make-selection
			      :shallow (selection-shallow P)
			      :positive (etree-positive P)
			      :junctive (etree-junctive P)
			      :parent parent
			      :free-vars (etree-free-vars P)
			      :terms (list parameter))))
		    (setf (etree-components new)
			  (list (shallow-merge (car (etree-components P))
					       (car (etree-components Q)) new)))
		    new)))
	       ((selection-p P)
		(let ((new-parameter
		       (make-skolem-fn (car (selection-terms P)) nil)))
		  (push (cons (car (selection-terms P)) new-parameter)
			merge-theta)
		  (push (cons (car (selection-terms Q)) new-parameter)
			merge-theta)
		  (let ((new (make-selection
		   :shallow
		   (simul-substitute-term-var merge-theta (selection-shallow P))
		   :positive (etree-positive P)
		   :junctive (etree-junctive P)
		   :parent parent
		   :free-vars (etree-free-vars P)
		   :terms (list new-parameter))))
		    (setf (etree-components new)
			  (list (shallow-merge (car (etree-components P))
					       (car (etree-components Q))new)))
		    new)))
	       ((expansion-p P)
		(let ((new-exp 
		       (make-expansion
			:shallow
			(simul-substitute-term-var merge-theta (expansion-shallow P))
			:positive (etree-positive P)
			:free-vars (etree-free-vars P)
			:terms (append (expansion-terms P) (expansion-terms Q))
			:parent parent
			:junctive (etree-junctive P)
			:components (append (etree-components P) (etree-components Q)))))
		  (push (list (copy-list (expansion-terms P)) (copy-list (expansion-terms Q)) new-exp)
			merge-list)
		  (dolist (S (etree-components new-exp))
			  (setf (etree-parent S) new-exp))
		  new-exp))
	       ((negation-p P)
		(let ((new (make-negation
		 :parent parent
		 :positive (positive-p P)
		 :free-vars (etree-free-vars P)
		 :junctive (etree-junctive P)
		 )))
		  (setf (etree-components new)
			(list (shallow-merge (car (etree-components P))
					     (car (etree-components Q)) new)))
		  new))
	       (t
		(throwfail "Error in shallow-merge: " (P . etree) t (Q . etree))))))
    (if (leaf-p R) (pushnew R (eproof-leaf-list current-eproof)))
    (dolist (x (list P Q) R) 
	    (unless (eq x R)
		    (setf (eproof-free-vars-in-etree current-eproof)
			  (subst R x (eproof-free-vars-in-etree current-eproof)))
		    (setq merge-mating
			  (nsubst R x merge-mating))
		    (if (leaf-p x)
			(setf (eproof-leaf-list current-eproof)
			      (delete x (eproof-leaf-list current-eproof))))))
    ))

(defun treemerge (P Q merge-mating)
  "Carries out treemerge algorithm as described in Pfenning's thesis.
Returns three values, the merged tree, the substitution merge-theta
of new parameters for old parameters and the altered mating."
  (declare (special merge-mating))
  (let ((merge-list nil)
	(merge-theta nil)
	(R nil)
	(*ignore-statuses* t))
    (declare (special merge-list merge-theta *ignore-statuses*))
    (setq R (shallow-merge P Q (etree-parent P)))
    (do* ((term-pair-exp (find-identical-terms)
			 (find-identical-terms)))
	 ((null term-pair-exp)
	  (values R merge-theta merge-mating))
	 (let* ((exp (caddr term-pair-exp))
		(pos1 (position (car term-pair-exp) (expansion-terms exp)))
		(pos2 (+ (1+ pos1)
			 (position (cadr term-pair-exp) 
				(nthcdr (1+ pos1) (expansion-terms exp)))))
		(P* (nth pos1 (etree-components exp)))
		(Q* (nth pos2 (etree-components exp))))
	   (setf (expansion-terms exp)
		 (cons (simul-substitute-term-var merge-theta 
						  (car term-pair-exp))
		       (nconc (subseq (expansion-terms exp) 0 pos1)
			      (subseq (expansion-terms exp) (1+ pos1) pos2)
			      (subseq (expansion-terms exp) (1+ pos2)))))
	   (setf (etree-components exp)
		 (cons (shallow-merge P* Q* exp)
		       (delete P* (delete Q* (etree-components exp)))))))))

(defun find-identical-terms ()
  "Uses the special variable merge-list which is a list of elements of form
({s1,s2,..,sq},{t1,...,tr},exp) where exp is the expansion node that
resulted from doing a shallow merge of two expansion nodes whose terms
were s1, s2, ... sq and t1, t2, ... tr respectively. 
The special var merge-theta is
a list of substitutions of new parameters for old parameters.  Checks to
see whether for some i, j, merge-theta(si) = merge-theta(tj), for some one 
of the elements of merge-list. If so, returns a list of (si, tj, exp), 
otherwise nil."
  (declare (special merge-list merge-theta))
  ;; Extremely inefficient but complete
  (dolist (triple merge-list nil)
    (let* ((old-s (car triple))
	   (old-t (cadr triple))
	   (new-s (mapcar #'(lambda (s) (simul-substitute-term-var
	 				 merge-theta s))
	 		  old-s))
           (new-t (mapcar #'(lambda (s) (simul-substitute-term-var
                                          merge-theta s))
			 old-t)))
      (do ((new-s* new-s
		   (if (cdr new-t*) new-s* (cdr new-s*)))
	   (new-t* new-t
		   (if (cdr new-t*) (cdr new-t*) new-t)))
	  ((or (null new-s*) (wffeq-ab (car new-s*) (car new-t*)))
	   (when new-s*
	     (let ((s* (nth (position (car new-s*) new-s) old-s))
		   (t* (nth (position (car new-t*) new-t) old-t)))
	     (setq merge-list
		   (nsubstitute-if
                    (list (delete s* old-s :count 1)
			  (delete t* old-t :count 1)
			  (caddr triple))
		    #'(lambda (x) (eq (caddr x) (caddr triple)))
		    merge-list))
	     (return-from find-identical-terms (list s* t* (caddr triple))))))))))


(defun merge-all (tree mating)
  "Takes an expansion tree and a mating, and descends into the tree.
At each expansion node, if two expansion terms are identical, their
corresponding trees are merged.  The resulting tree replaces the two
original ones, and the substitution returned is applied to the terms
and trees. The resulting tree and mating are returned."
  (do* ((nodes nil (append nodes (etree-components tree*)))
       (tree* tree (pop nodes))
       (theta* nil))
      ((null tree*)
       (when theta* (simul-substitute-in-etree-main theta* tree))
       (values tree mating))
     (when (expansion-p tree*)
       (do ((ident-pair (find-ident-pair tree*)
			(find-ident-pair tree*)))
	   ((null ident-pair))
	  ;; ident-pair is a pair of integers indicating the places of
	  ;; of two identical expansion terms in increasing order
	  (let* ((terms (expansion-terms tree*))
		 (kids (etree-components tree*))
		 (ident-term (nth (car ident-pair) terms))
		 (P (nth (car ident-pair) kids))
		 (Q (nth (cdr ident-pair) kids)))
	    (multiple-value-bind (R theta mating*)
	      (treemerge P Q mating)
	      (setq theta* (nconc theta theta*))
	      (setq mating mating*)
	      (setf (expansion-terms tree*)
		    (mapcar #'(lambda (x) 
				(if theta (simul-substitute-term-var theta x) x))
			    (cons ident-term
				  (append
				   (subseq terms 0 (car ident-pair))
				   (subseq terms (1+ (car ident-pair))
					   (cdr ident-pair))
				   (subseq terms (1+ (cdr ident-pair)))))))
	      (setf (etree-components tree*)
		    (cons R
			  (delete P
				  (delete Q kids))))
	      (when theta
		    (dolist (x (cdr (etree-components tree*)))
		      (simul-substitute-in-etree-main theta x)))))))))



(defun find-ident-pair (tree)
  "Given an expansion node, will check the expansion terms.  If any two
are identical, will return a cons of the integer positions of the first
such two terms in the node's terms list (in increasing order).  Otherwise
returns nil." 
  (let ((terms (expansion-terms tree)))
   (do ((terms1 terms (cdr terms1)))
       ((null terms1) nil)
       (do ((terms2 (cdr terms1) (cdr terms2))
	    (term (car terms1)))
	   ((or (null terms2) (wffeq-ab term (car terms2)))
	    (if terms2
		(return-from find-ident-pair
			     (cons (- (length terms) (length terms1))
				   (- (length terms) (length terms2))))))))))


(defun make-subst-list (mating)
  "Takes a completed mating and returns an alist which consists of the
substitutions for free variables which the mating requires."
  (let ((subst-stack
	  (find-substs-for-mating mating))
	(h-vars nil)
	(sub-list nil))
    (dolist (subst (reverse subst-stack)
		   ;; changed to ab-normalize each substitution DAN 24OCT88
		   ;; will avoid capturing any free variables
		   (ab-sub-list 
                    sub-list))
      (when (not (memq (car subst) h-vars))
	(push (cons (car subst) 
		    (lambda-reduce-subst (cdr subst) subst-stack))
	      sub-list))
      (setq subst (cdr subst))
      (when (subst-p subst)
	(if (subst-new-h-vars subst)
	    (dolist (sub (subst-new-h-vars subst))
	      (push (cdr sub) h-vars))
	    (dolist (var (subst-h-vars subst))
	      (push var h-vars)))))))

;;; Modified to remove pi and sigma from substitutions 12FEB91 DAN

(defun ab-sub-list (sub-list)
  (let ((new-list nil)
	(rename-all-bd-vars t)
	(binder-sigma-string (symbol-name binder-sigma))
	(binder-pi-string (symbol-name binder-pi)))
  (declare (special binder-sigma-string binder-pi-string))
    (dolist (sub sub-list)
      (let ((list (assoc (cdr sub) new-list :test #'wffeq)))
	(if list 
	    (rplacd list (cons (car sub) (cdr list)))
	  (push (list (cdr sub) (car sub)) new-list))))
    (dolist (sub new-list)
      (rplaca sub (ab-normalize 
		   (lambda-norm 
		    (replace-pi-and-sigma-in-wff (car sub))))))
    (setq sub-list nil)
    (dolist (list new-list sub-list)
      (dolist (var (cdr list))
	(push (cons var (car list)) sub-list)))))

;;; Returns T if node is not needed in the mating on etree


(defun unneeded-node-p (node mated-nodes)
  (let ((current-status 
	 (etree-status* node))
	(mnodes (reduce #'append (mapcar #'(lambda (y) (list (car y) (cdr y)))
					 mated-nodes))))
    (or (zerop current-status)
	(not (find-etree-node #'(lambda (x)
				  (or (member (etree-name* x) mnodes)
				      (and (true-p x) (not (etree-positive x)))
				      (and (false-p x) (etree-positive x))))
			      node))
	(and merge-minimize-mating
	     (prog2
		 (update-status nil node 0)
		 (spans (eproof-etree current-eproof) nil mated-nodes)
	       (update-status nil node current-status))))))
  
(defun prune-unmated-branches (etree mating)
  (do* ((nodes nil (append nodes (etree-components tree)))
        (tree etree (pop nodes))
	(mated-nodes 
	 (mapcar #'(lambda (x) 
		     (cons (etree-name* (car x))
			   (etree-name* (cdr x))))
		 mating)))
       ((null tree) (replace-non-leaf-leaves etree mating))
    (when (and (expansion-p tree) 
	       (> (length (etree-components tree)) 1))
      (do ((n (position-if
		#'(lambda (x) (unneeded-node-p x mated-nodes))
		(etree-components tree))
	      (position-if
		#'(lambda (x) (unneeded-node-p x mated-nodes))
		(etree-components tree))))
	  ((not n))
	  (setf (etree-components tree)
		(nconc (subseq (etree-components tree) 0 n)
		       (subseq (etree-components tree) (1+ n))))
	  (setf (expansion-terms tree)
		(nconc (subseq (expansion-terms tree) 0 n)
		       (subseq (expansion-terms tree) (1+ n))))
	  ))))

#+comment(defun prune-unmated-rewrites (etree mating)
 (setq *hacked-rewrites-list* (nreverse *hacked-rewrites-list*))
 (unless (or ms-jform (eproof-jform current-eproof)) 
	 (setf (eproof-jform current-eproof) (etree-to-jform (eproof-etree current-eproof))))
  (do ((hwl (cdr *hacked-rewrites-list*) (cdr hwl))
       (tree (car *hacked-rewrites-list*) (car hwl))
       (mated-nodes 
	(reduce #'append (mapcar #'(lambda (x) 
				     (list (etree-name* (car x))
					   (etree-name* (cdr x))))
				 mating))))
      ((null tree) (replace-non-leaf-leaves etree))
      (if (find-etree-node-name (etree-name tree) etree)
	;if it's not there, then we already threw it away after an earlier pass through this loop.
	  (let* ((leftrew (car (etree-components (car (etree-components tree)))))
		 (rightrew (cadr (etree-components (car (etree-components tree)))))
		 (defns (get-all-defns (rewrite-shallow tree)))
		 (rewrite-defns (fix-rewrite-defns defns))
		 (rewrite-defns (setdiff (append (cdr (assoc 'eager rewrite-defns))
						 (cdr (assoc 'lazy1 rewrite-defns))
						 (cdr (assoc 'dual rewrite-defns))
						 (cdr (assoc 'lazy2 rewrite-defns)))
					 '(equiv))))
	  ;leftrew should be a leaf, rightrew is the "real" rewrite of this definition.
	    (if (not (position-if #'(lambda (x) (memq x mated-nodes))
				  (mapcar #'etree-name
					  (find-etree-nodes #'etree-p rightrew nil t))))
					;then there are no mated nodes in that rewrite, so...
		(progn 
		  (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (eq x tree)) *hacked-rewrites-list*))
		  (setf (etree-components (etree-parent tree))
			(mapcar #'(lambda (x) (if (eq x tree) leftrew x)) (etree-components (etree-parent tree))))
		  (setf (etree-parent (etree-parent rightrew)) nil)
		  (setf (etree-parent leftrew) (etree-parent tree)))
	      (let* ((mclist (mapcar #'(lambda (x) (find-potential-connection 
						    (find-jform-name (princ-to-string (car x))
								     (or ms-jform (eproof-jform current-eproof)))
						    (list (find-jform-name (princ-to-string (cdr x))
									   (or ms-jform (eproof-jform current-eproof))))
						    (cgraph) nil))
				     mating))
		     (newclist (remove-rew-connections mclist 				    
						       (mapcar #'etree-name
							       (find-etree-nodes #'etree-p rightrew nil t)))))
		(if (not (find-next-openpath
			 (or ms-jform (etree-to-jform (eproof-etree current-eproof)))
			 nil (cgraph) newclist))
		    (progn
		      (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (eq x tree)) *hacked-rewrites-list*))
		      (setf (etree-components (etree-parent tree))
			    (mapcar #'(lambda (x) (if (eq x tree) leftrew x)) (etree-components (etree-parent tree))))
		      (setf (etree-parent (etree-parent rightrew)) nil)
		      (setf (etree-parent leftrew) (etree-parent tree)))
		  (if (unneeded-node-p leftrew mated-nodes)
	      ;then the leaf is not mated, or it is but the connection isn't necessary
	      ;so we shuffle the branches, and this node is no longer hacked.
		      (if (not (contains-some-defn (rewrite-shallow tree)))
		    ;then rightrew is an equality rewrite whose first node looks exactly like the current node
			  (progn
			    (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (eq x tree)) *hacked-rewrites-list*))
			    (setf (etree-components (etree-parent tree))
				  (mapcar #'(lambda (x) (if (eq x tree) rightrew x)) (etree-components (etree-parent tree))))
			    (setf (etree-parent (etree-parent leftrew)) nil)
			    (setf (etree-parent rightrew) (etree-parent tree)))
		  ;otherwise it's a definitional rewrite whose first node is the expanded defn.
			(progn
			  (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (eq x tree)) *hacked-rewrites-list*))
			  (setf (etree-components tree) (list rightrew))
			  (setf (etree-parent (etree-parent leftrew)) nil)
			  (setf (etree-parent rightrew) tree))))))))
	    ;otherwise we really do need the leaf -- do we need the "real" rewrite?
	;if we get here, it's because the current tree has already been thrown out
	(setq  *hacked-rewrites-list* (remove-if #'(lambda (x) (eq x tree)) *hacked-rewrites-list*)))))

(defun print-nodes-between (junct rewrite)
  (msg " " junct)
  (unless (eq junct rewrite) (print-nodes-between (etree-parent junct) rewrite)))

(defun rewrite-nodes-between (junct rewrite)
  (if (eq junct rewrite) 
      t
    (if (rewrite-p (etree-parent junct))
	(rewrite-nodes-between (etree-parent junct) rewrite)
      nil)))

(defun fix-shallow-chain (lowrew highrew func)
  (unless (eq lowrew highrew)
	  (setf (rewrite-shallow lowrew) (funcall func (rewrite-shallow lowrew)))
	  (fix-shallow-chain (etree-parent lowrew) highrew func)))

(defun replace-dual-gensym (gsym sh x)
  (when (rewrite-p x)
    (when (eq (rewrite-shallow x) gsym)
      (setf (rewrite-shallow x) sh))
    (replace-dual-gensym gsym sh (car (etree-components x))))
  (when (leaf-p x)
    (when (eq (leaf-shallow x) gsym)
      (setf (leaf-shallow x) sh))))

(defun string-rename (strname)
  (or (car (rassoc strname (mapcar #'(lambda (x) (cons (princ-to-string (car x)) (princ-to-string (cdr x)))) 
				   *renamed-leaves-list*) :test 'string=))
      strname))

(defun ancestor-rewrite (rew1 rew2)
  (let ((a (princ-to-string rew1))
	(b (princ-to-string rew2)))
    (or (< (length a) (length b))
	(and (= (length a) (length b))
	     (string< a b)))))

(defun get-junct (etree)
  (when etree
	(if (or (econjunction-p etree) (edisjunction-p etree))
	    etree
	  (get-junct (etree-parent etree)))))

(defun modify-dual-rewrites (etree mating)
  (let ((mated-nodes (mapcar #'(lambda (x)
				 (cons (etree-name* (car x))
				       (etree-name* (cdr x))))
			     mating)))
    (dolist (hr *hacked-rewrites-list*)
	    (let ((rewrite (car hr)))
	      (when (and (rewrite-p rewrite)
			 (find-etree-node-name (etree-name rewrite) etree))
		(let* ((junct (get-junct (etree-parent (cddr hr))))
		       (gsym (cadr hr))
		       (MERGE-MINIMIZE-MATING NIL)
		       (x (car (etree-components junct)))
		       (y (cadr (etree-components junct))))
		  (declare (special MERGE-MINIMIZE-MATING))
		  (setf (rewrite-justification rewrite) 'EQUIVWFFS)
		  (if (unneeded-node-p x mated-nodes)
		      (let ((jpar (etree-parent junct)))
			(setf (etree-components jpar) (list y))
			(setf (etree-parent y) jpar)
			(fix-shallow-chain jpar rewrite 'gdr))
		    (if (unneeded-node-p y mated-nodes)
			(let ((jpar (etree-parent junct))
			      (sh (rewrite-shallow rewrite)))
			  (replace-dual-gensym gsym sh x)
			  (setf (etree-components jpar) (list x))
			  (setf (etree-parent x) jpar)
			  (fix-shallow-chain jpar rewrite
					     #'(lambda (w)
						 (if (eq (cdar w) gsym)
						     sh
						   (cdar w)))))
		      (let ((dualrewrite (copy-rewrite rewrite))
			    (sh (rewrite-shallow rewrite))
			    (par (etree-parent rewrite)))
			(setf (etree-name dualrewrite)
			      (intern-str (create-namestring rewrite-name)))
			(if par
			    (setf (etree-components par)
				  (substitute dualrewrite rewrite
					      (etree-components par)))
			  (setq etree dualrewrite))
			(setf (rewrite-justification dualrewrite) 'DUAL)
			(setf (etree-components dualrewrite)
			      (list rewrite))
			(setf (etree-parent rewrite) dualrewrite)
			(setf (rewrite-shallow rewrite)
			      (if (econjunction-p junct)
				  (acons 'AND sh sh)
				(acons 'OR sh sh)))
			(update-status nil dualrewrite 1)
			(replace-dual-gensym gsym sh x)
			(fix-shallow-chain (etree-parent junct) rewrite
					   #'(lambda (w)
					       (if (and (or (and-p w) (or-p w))
							(eq (cdar w) gsym))
						   (acons (caar w) sh (cdr w))))))))))))
    (setq *hacked-rewrites-list* nil)
    (values etree mating)))

; unused as of 9/2001
(defun prune-unmated-rewrites (etree mating)
 (when *hacked-rewrites-list*
       (setq *hacked-rewrites-list* (sort (remove-if #'null *hacked-rewrites-list* :key 'cddr) 
					  'ancestor-rewrite :key 'car))
       (unless (or ms-jform (eproof-jform current-eproof)) 
	       (setf (eproof-jform current-eproof)
		     (etree-to-jform (eproof-etree current-eproof))))
       (do ((hwl (cdr *hacked-rewrites-list*) (cdr hwl))
	    (junct (get-junct (etree-parent (cddar *hacked-rewrites-list*)))
		   (and (not (null (car hwl))) (get-junct (etree-parent (cddar hwl)))))
	    (rewrite (caar *hacked-rewrites-list*) (caar hwl))
	    (mated-nodes 
	     (reduce #'append (mapcar #'(lambda (x) 
					  (list (etree-name* (car x))
						(etree-name* (cdr x))))
				      mating)))
	    (chain nil nil))
	   ((null rewrite) (replace-non-leaf-leaves etree mating))
	   (when (and (neq (etree-parent junct) rewrite) (rewrite-nodes-between junct rewrite))
		 (setq chain t))
	   (if (and (neq (etree-parent junct) rewrite) (not chain))
	       (progn (msgf "Can't fix gap between rewrite & junctive nodes: " t 
			    "NB: This can't happen. Translation will probably fail." t)
		      (print-nodes-between junct rewrite))
	     (progn
	       (when (eq mating-verbose 'max) (msgf t rewrite " (Chain:" chain ")" t))
	       (if (find-etree-node-name (etree-name rewrite) etree)
					;if it's not there, then we already threw it away after an earlier pass through this loop.
		   (let* ((leafrew (car (etree-components junct)))
			  (realrew (cadr (etree-components junct)))
			  (defns (get-all-defns (rewrite-shallow rewrite)))
			  (rewrite-defns (fix-rewrite-defns defns))
			  (rewrite-defns (setdiff (append (cdr (assoc 'eager rewrite-defns))
							  (cdr (assoc 'lazy1 rewrite-defns))
							  (cdr (assoc 'dual rewrite-defns))
							  (cdr (assoc 'lazy2 rewrite-defns)))
						  '(equiv))))
		     (if (not (position-if #'(lambda (x) (memq x mated-nodes))
					   (mapcar #'etree-name (find-etree-nodes #'etree-p realrew nil t))))
					;then there are no mated nodes in the real rewrite, so...
			 (progn 
			   (when (eq mating-verbose 'max) (msgf "case 1; real rewrite is irrelevant" t))
			   (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (eq (car x) rewrite)) *hacked-rewrites-list*))
			   (setf (etree-components (etree-parent rewrite))
				 (mapcar #'(lambda (x) (if (eq x rewrite) leafrew x)) (etree-components (etree-parent rewrite))))
			   (setf (etree-parent junct) nil)
			   (setf (etree-parent leafrew) (etree-parent rewrite)))
		       (let* ((mclist (mapcar #'(lambda (x) (find-potential-connection 
							     (find-jform-name (string-rename (princ-to-string (car x)))
									      (or ms-jform (eproof-jform current-eproof)))
							     (list (find-jform-name (string-rename (princ-to-string (cdr x)))
										    (or ms-jform (eproof-jform current-eproof))))
							     (cgraph) nil))
					      mating))
			      (newclist (remove-rew-connections mclist 				    
								(mapcar #'etree-name
									(find-etree-nodes #'etree-p realrew nil t)))))
			 (if (not (find-next-openpath
				   (or ms-jform (etree-to-jform (eproof-etree current-eproof)))
				   nil (cgraph) newclist))
			     (progn
			       (when (eq mating-verbose 'max) (msgf "case 2; real rewrite is irrelevant" t))
			       (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (eq (car x) rewrite)) *hacked-rewrites-list*))
			       (setf (etree-components (etree-parent rewrite))
				     (mapcar #'(lambda (x) (if (eq x rewrite) leafrew x)) (etree-components (etree-parent rewrite))))
			       (setf (etree-parent junct) nil)
			       (setf (etree-parent leafrew) (etree-parent rewrite)))
			   (if (or (not (memq (etree-name leafrew) mated-nodes))
				   (unneeded-node-p leafrew mated-nodes))
					;then the leaf is not mated, or it is but the connection isn't necessary
					;so we shuffle the branches, and this node is no longer hacked.
			       (if (not (contains-some-defn (rewrite-shallow rewrite)))
					;then realrew is an equality rewrite whose first node looks exactly like the current node
				   (progn
				     (when (eq mating-verbose 'max) (msgf "case 3; fake equality rewrite is irrelevant" t))
				     (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (eq (car x) rewrite)) 
									     *hacked-rewrites-list*))
				     (setf (etree-components (etree-parent rewrite))
					   (mapcar #'(lambda (x) (if (eq x rewrite) realrew x)) 
						   (etree-components (etree-parent rewrite))))
				     (setf (etree-parent junct) nil)
				     (setf (etree-parent realrew) (etree-parent rewrite)))
					;otherwise it's a definitional rewrite whose first node is the expanded defn.
				 (progn
				   (when (eq mating-verbose 'max) (msgf "case 4; fake rewrite is irrelevant" t))
				   (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (eq (car x) rewrite))
									   *hacked-rewrites-list*))
				   (if chain
				       (progn (fix-shallow-chain (etree-parent junct) rewrite 'gdr)
					      (setf (etree-components (etree-parent junct)) (list realrew))
					      (setf (etree-parent realrew) (etree-parent junct))
					      (setf (etree-parent junct) nil))
				     (progn (setf (etree-components rewrite) (list realrew))
					    (setf (etree-parent junct) nil)
					    (setf (etree-parent realrew) rewrite)))))
			     (when (eq mating-verbose 'max)
			       (msgf "case 5; we need both sides")))))))
					;if we get here, it's because the current tree has already been thrown out
		 (progn (when (eq mating-verbose 'max) (msgf "case 6; rewrite is already gone"))
			(setq  *hacked-rewrites-list* (remove-if #'(lambda (x) (eq (car x) rewrite)) *hacked-rewrites-list*)))))))))

(defun remove-rew-connections (oldclist nodes)
  (let ((newclist nil))
    (dolist (conn oldclist)
	    (let ((connection (car (gethash conn (connections-array)))))
	      (unless (or (memq (literal-name (car connection)) nodes)
			  (memq (literal-name (cdr connection)) nodes))
		      (push conn newclist))))
    (nreverse newclist)))

;;; If any terminal nodes of the tree are not leaves, make them leaves.

(defun replace-non-leaf-leaves (etree mating)
  (let ((leaves* (find-etree-nodes #'leaf-p* etree nil t)))
    (dolist (leaf* leaves* (values etree mating))
      (when (not (leaf-p leaf*))
	(let ((newleaf (make-leaf :shallow (get-shallow leaf*)
				  :positive (positive-p leaf*)
				  :parent (etree-parent leaf*)
				  :name (etree-name* leaf*)
				  :junctive nil)))
	  (setq mating 
	    (mapcar #'(lambda (x)
			(if (eq (car x) leaf*)
			    (cons newleaf (cdr x))
			  (if (eq (cdr x) leaf*)
			      (cons (car x) newleaf)
			    x)))
		    mating))
	  (update-global-lists leaf* newleaf))))))

(defun make-mating-lists (mating)
  "Given a mating, returns two
alists: the first a list of the mated pairs in the tree; and the
second a list of substitutions for variables which the mating requires."
  (values
    (mapcar #'(lambda (x)
		(let ((lit-pair (if (numberp x)
				    (car (gethash x (connections-array)))
				    x))) ; propositional case
		  (cons (if (etree-p (car lit-pair))
			    (car lit-pair)
			    (find-etree-node-name
			      (literal-name (car lit-pair)) 
			      (eproof-etree current-eproof)
			      t))
			(if (etree-p (cdr lit-pair))
			    (cdr lit-pair)
			    (find-etree-node-name
			      (literal-name (cdr lit-pair))
			      (eproof-etree current-eproof)
			      t)))))
	    (mating-clist mating))
    (make-subst-list mating)))

(defun prune-status-0 (etree)
  (when (expansion-p etree)
    (block out
      (loop
	 (let ((n (position-if #'zerop (etree-components etree)
			       :key #'etree-status*)))
	   (if n
	       (progn
		 (setf (etree-components etree)
		       (nconc (subseq (etree-components etree) 0 n)
			      (subseq (etree-components etree) (1+ n))))
		 (setf (expansion-terms etree)
		       (nconc (subseq (expansion-terms etree) 0 n)
			      (subseq (expansion-terms etree) (1+ n)))))
	       (return-from out nil))))))
    (mapc #'prune-status-0 (etree-components etree))
    etree)
    


(defun etr-merge (etree mating)
  "Given an etree and a mating (actual mating data structure) which proves
that etree, returns two results: the first is the etree after applying the 
substitutions dictated by the mating,
and merging duplicate expansion nodes; the second is an alist, in which each
pair is a mated pair in the new mating for the new etree."
 (let ((mating-list nil)
       (subst-list nil)
       (newtree etree))
    (multiple-value-setq (mating-list subst-list)
			 (make-mating-lists mating))
    (setq newtree (prune-status-0 newtree))
    (when subst-list
      (dolist (sub (remove-duplicates subst-list :key #'car))
        ; called with rename-all-bd-vars nil below, so don't change any
        ; identical substitutions DAN
        (substitute-in-etree (cdr sub) (car sub) newtree nil)))
    (setf (eproof-free-vars-in-etree current-eproof)
        (mapcar #'(lambda (x) 
                    (cons (exp-var-var (car x)) (cdr x)))
                (eproof-free-vars-in-etree current-eproof)))
    (strip-exp-vars-from-etree newtree)
    (merge-all newtree mating-list)))

(defun strip-exp-vars-from-etree (tree)
  (setf (etree-free-vars tree)
        (mapcar #'strip-exp-vars (etree-free-vars tree)))
  (apply-label tree (strip-exp-vars-from-etree tree))
  (mapc #'strip-exp-vars-from-etree (etree-components tree)))

(defun strip-sk-terms-from-etree (etree)
  (typecase etree
    (leaf (setf (leaf-shallow etree)
	    (strip-sk-terms (leaf-shallow etree))))
    ((or true false))
    (rewrite
     (setf (rewrite-shallow etree)
       (strip-sk-terms (rewrite-shallow etree))))
    (expansion
     (setf (expansion-shallow etree)
       (strip-sk-terms (expansion-shallow etree)))
     (setf (expansion-terms etree)
       (mapcar #'strip-sk-terms (expansion-terms etree))))
    (skolem
     (setf (skolem-shallow etree)
       (strip-sk-terms (skolem-shallow etree)))
     (setf (skolem-terms etree)
       (mapcar #'strip-sk-terms (skolem-terms etree))))
    (selection
     (setf (selection-shallow etree)
       (strip-sk-terms (selection-shallow etree)))
     (setf (selection-terms etree)
       (mapcar #'strip-sk-terms (selection-terms etree)))))
  (setf (etree-components etree)
    (mapcar #'strip-sk-terms-from-etree (etree-components etree)))
  etree)

(defun subst-skol-terms (etree)
  (let* ((skol-nodes (find-etree-nodes #'skolem-p etree nil t))
	 (skol-terms (mapcan 
		       #'(lambda (x)
			   (let ((sk (car (skolem-terms x))))
			     (if (skolem-term-p sk)
				 (list (cons (lambda-norm (skolem-term-term sk))
					     sk)))))
		       skol-nodes)))
    (subst-skol-terms-main skol-terms etree)
    etree))

(defun subst-skol-terms-main (skol-terms etree) 
  (flet ((nsub-sk-terms (wff) 
           (nsublis skol-terms wff :test #'wffeq-ab-beta))) ;changed MB 5/25/94 and again 6/1
    (typecase etree 
      (leaf (setf (leaf-shallow etree) (nsub-sk-terms (leaf-shallow etree))))
      ((or edisjunction econjunction implication negation) 
       (mapc #'(lambda (x) (subst-skol-terms-main skol-terms x))
             (etree-components etree)))
      (rewrite (setf (rewrite-shallow etree)
                     (nsub-sk-terms (rewrite-shallow etree)))
	       (if (rewrite-ruleq-shallow etree)
		   (setf (rewrite-ruleq-shallow etree)
			 (nsub-sk-terms (rewrite-ruleq-shallow etree))))
               (subst-skol-terms-main skol-terms 
                                      (car (etree-components etree))))
      (expansion (setf (expansion-shallow etree) 
                       (nsub-sk-terms (expansion-shallow etree)))
                 (setf (expansion-terms etree)
                       (mapcar #'(lambda (x) (nsub-sk-terms x))
                               (expansion-terms etree))) 
                 (mapc #'(lambda (x) (subst-skol-terms-main skol-terms x))
                       (etree-components etree)))
      (skolem 
       (setf (skolem-shallow etree) 
             (nsub-sk-terms (skolem-shallow etree)))
       (subst-skol-terms-main skol-terms (car (etree-components etree))))
      (otherwise 
       (mapc #'(lambda (x) (subst-skol-terms-main skol-terms x))
             (etree-components etree))))))

(defun subst-vars-for-params (etree)       
  (let ((skol-nodes (nreverse (find-etree-nodes #'(lambda (x) 
						    (or (skolem-p x) 
							(selection-p x)))
						etree nil t)))
	(exp-nodes (find-etree-nodes #'expansion-p etree nil t)))
    (let ((skol-terms 
	    (do ((nodes (cdr skol-nodes) (cdr nodes))
		 (node (car skol-nodes) (car nodes))
		 (exp-wffs (mapcar #'(lambda (x)
				       (copy-tree (expansion-shallow x)))
				   exp-nodes)
			   (mapcar #'(lambda (x)
				       (copy-tree (expansion-shallow x)))
				   exp-nodes))
		 (exp-terms-list
		   (mapcar #'(lambda (x)
			       (mapcar #'(lambda (y)
					   (copy-tree y))
				       (expansion-terms x)))
			   exp-nodes)
		   (mapcar #'(lambda (x)
			       (mapcar #'(lambda (y)
					   (copy-tree y))
				       (expansion-terms x)))
			   exp-nodes))
		 (vars nil)
		 (skol-terms nil)
		 (bad-nodes nil))
		((null node) skol-terms)
		(let ((var (bindvar (if (skolem-p node)
					(skolem-shallow node)
				      (selection-shallow node))))
		      (sk (car (if (skolem-p node)
				   (skolem-terms node)
				 (selection-terms node)))))
		  (if (or (member var vars :test #'wffeq)
			  (assoc var (eproof-skolem-constants current-eproof)
				 :test #'wffeq)
			  (not (free-for-exp sk var skol-terms
					     exp-terms-list exp-wffs)))
		      (progn
			(push node bad-nodes)
			(when (skolem-term-p sk)
			      (push (cons sk (skolem-term-parameter sk)) skol-terms)))
		    (progn 
		      (push var vars)
		      (push (cons sk var) skol-terms)))))))
      (simul-substitute-in-etree-main skol-terms etree)
      (when (skolem-term-p (car skol-terms))
	    (msg t t t "The following table shows that, in the current expansion tree, the skolem" t 
		 "constants on the left sides of arrows are replaced by those on the right sides." t t)
	    (dolist (pair skol-terms)
		    (msg ((skolem-term-term (car pair)) . gwff) " -------> " ((cdr pair) . gwff) t))
	    (terpri) (terpri)
	    ))
    etree))

(defun free-for-exp (sk var theta exp-terms-list exp-wffs)
  (setq exp-terms-list 
	(mapcar #'(lambda (y)
		    (mapcar #'(lambda (x) 
				(simul-substitute-term-var-etree
				    theta x))
			    y))
		exp-terms-list))
  (setq exp-wffs (mapcar #'(lambda (x) 
				   (simul-substitute-term-var-etree
				    theta x))
			       exp-wffs))
  (do ((exp-terms-list (cdr exp-terms-list) (cdr exp-terms-list))
       (exp-terms (car exp-terms-list) (car exp-terms-list))
       (exp-wffs (cdr exp-wffs) (cdr exp-wffs))
       (exp-wff (car exp-wffs) (car exp-wffs))
       (skvar (if (skolem-term-p sk)
		  (skolem-term-parameter sk)
		  sk)))
      ((null exp-wff) t)
    (let* ((new-terms (mapcar #'(lambda (x)
				 (if (not (free-for var skvar x))
				     (return-from free-for-exp nil)
				     (simul-substitute-term-var-etree 
				      (list (cons sk var)) x)))
			     exp-terms))
	   (new-wff (if (not (free-for var skvar exp-wff))
			(return-from free-for-exp nil)
			(simul-substitute-term-var-etree 
			 (list (cons sk var)) exp-wff)))
	   (bindvar (bindvar new-wff)))
      (dolist (new-term new-terms)
	(unless (free-for new-term bindvar (gdr new-wff))
	  (return-from free-for-exp nil))))))

(defun simul-free-for (theta wff)
  (dolist (sub theta t)
    (unless (free-for (cdr sub) (car sub) wff)
      (return nil))))

(defun insert-rewrite-ab-node (node)
  (let ((new-rewrite-node 
	 (make-rewrite :shallow (get-shallow node)
		       :justification 'ab
		       :parent (etree-parent node)
		       :positive (etree-positive node)
		       :free-vars (etree-free-vars node)
		       :components (list node)
		       :junctive 'neutral)))
    (setf (etree-components (etree-parent node))
	  (nsubst new-rewrite-node node (etree-components (etree-parent node))))
    (if (skolem-p node)
	(setf (skolem-shallow node)
	      (ab-change (skolem-shallow node) 
			 (car (skolem-terms node))))
	(setf (selection-shallow node)
	      (ab-change (selection-shallow node) 
			 (car (selection-terms node)))))
    (setf (etree-parent node) new-rewrite-node)))


;;; Makes sure that each node in etree is active

(defun update-statuses (etree)
  (dolist (kid (etree-components etree))
    (update-statuses kid))
  (unless (plusp (etree-status* etree))
    (update-status nil etree 1)))

(defun get-possible-newabbs ()
  (let ((posnodes (remove-if #'(lambda (y) (<= (length (find-etree-nodes #'leaf-p y)) 1))
			     (find-etree-nodes #'etree-positive (eproof-etree current-eproof))))
	(negnodes (remove-if #'(lambda (y) (<= (length (find-etree-nodes #'leaf-p y)) 1))
			     (find-etree-nodes #'(lambda (x) (not (etree-positive x))) (eproof-etree current-eproof))))
	returnlist)
    (dolist (p posnodes)
	    (dolist (n negnodes)
		    (when (not (standing-disjunctively p n))
			  (let ((pshal (get-shallow p))
				(nshal (get-shallow n)))
			    (when (or (wffeq-ab pshal nshal)
				      (and (= (length (find-etree-nodes #'leaf-p p)) (length (find-etree-nodes #'leaf-p n)))
					   (valid-p (cons (cons 'equiv pshal) nshal))))
				  (push pshal returnlist))))))
    (setq returnlist (discard-subfms returnlist))
    (when returnlist (msgf "Possible abbreviations in this theorem are:"))
    (dolist (r (mapcar 'ab-normalize 
		       (remove-duplicates (mapcar 'lambda-bind-free-vars returnlist) :test #'wffeq-ab)))
	    (msgf (r . gwff)))
    (msgf t)))

(defun standing-disjunctively (p n)
  (and (not (member n (find-etree-nodes #'(lambda (x) (declare (ignore x)) t) p)))
       (not (member p (find-etree-nodes #'(lambda (x) (declare (ignore x)) t) n)))
       (eq (etree-junctive (lowest-common-ancestor p n)) 'DIS)))

(defun lowest-common-ancestor (p n)
  (do ((plist (list p) (cons (etree-parent (car plist)) plist))
       (nlist (list n) (cons (etree-parent (car nlist)) nlist)))
      ((and (null (car plist)) (null (car nlist)))
       (dolist (pl (reverse plist))
	       (when (member pl nlist) (return pl))))
      (when (null (car plist)) (pop plist))
      (when (null (car nlist)) (pop nlist))))

(defun lambda-bind-free-vars (gwff)
  (let ((fvs (free-vars-of gwff)))
    (dolist (fv fvs gwff)
	    (setq gwff (cons (cons fv 'lambda) gwff)))))
	    
(defun discard-subfms (gwfflist)
  ;we want to be careful here, so given x and y we'll discard y only in the case
  ;where x is a quantified version of y.
  (let (returnlist)
    (do ((g gwfflist (cdr g)))
	((null g) returnlist)
	(when (dolist (g2 gwfflist t)
		      (when (more-quantifiers g2 (car g))
			    (return nil)))
	      (push (car g) returnlist)))))

(defun more-quantifiers (long short)
  (if (wffeq-ab long short) nil
    (when (boundwff-q long)
	  (do ((long2 long (cdr long2)))
	      ((or (wffeq-ab (lambda-bind-free-vars long2) (lambda-bind-free-vars short)) (not (boundwff-q long2)))
	       (wffeq-ab (lambda-bind-free-vars long2) (lambda-bind-free-vars short)))))))

; cebrown 9/7/01
(defun set-all-parents (etree &optional parent)
  (setf (etree-parent etree) parent)
  (dolist (e (etree-components etree))
	  (set-all-parents e etree)))
