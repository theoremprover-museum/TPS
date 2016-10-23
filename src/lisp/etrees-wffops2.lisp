;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :auto)

(part-of expansion-tree)

(deffile etrees-wffops2
  (part-of expansion-tree)
  (mhelp "Defines wffops used with expansion trees."))

(context expansion-trees)

(defun expand-top= (gwff &optional (pos t))
  (if (eq rewrite-equalities 'core::both)
      ; this setting is only partly implemented
      ; the idea is to expand A(oa)=B(oa) using both leibniz and ext= to get a conjunction/disjunction
      ; see wffabb2, etrees-wffops, etrees-wffops2
      ; translation into ND is not written yet.
      ; MB Mon May 12 14:50:16 1997
      (let* ((rewrite-equalities 'auto::leibniz)
	     (left (expand-top=-real gwff))
	     (rewrite-equalities 'auto::all)
	     (right (expand-top=-real gwff)))
	(if (and left right (not (wffeq-ab left right)))
	    (values (cons (cons (if pos 'and 'or) (lnorm left)) (lnorm right)) 'both=) ; cebrown -- changed both to both=
	  (expand-top=-real gwff)))
    (if (eq rewrite-equalities 'core::parity1)
	(if pos
	    (let ((rewrite-equalities 'leibniz))
	      (expand-top=-real gwff))
	  (let ((rewrite-equalities 'all))
	    (expand-top=-real gwff)))
      (expand-top=-real gwff))))

(defun expand-top=-real (gwff)
  "Expands top level equality. Returns NIL if the GWFF doesn't have = as the
       top level infix operator."
  (cond ((label-q gwff)
	 (apply-label gwff (expand-top= gwff)))
	((lsymbol-q gwff)
	 (if (equality-p gwff)
	     (if (memq rewrite-equalities (list 'auto::all 'auto::none 'auto::only-ext 'auto::lazy2 'dual))
		 (instantiate-equality-smart gwff)
	       (values (instantiate-equality-basic gwff) 'leibniz=))
	   nil))
	((boundwff-q gwff) nil)
	(t (let ((new-wff nil) (just nil))
	     (multiple-value-setq (new-wff just) (expand-top=-real (car gwff)))
	     (if new-wff 
		 (values (cons new-wff (cdr gwff)) just)
	         (values nil just))))))

(defun expand-first= (gwff)
  (multiple-value-bind (new just)
		       (expand-top=-real gwff)
		       (when (or new (not (consp gwff)))
			     (return-from expand-first= (values new just))))
  (multiple-value-bind (new just)
		       (expand-first= (car gwff))
		       (when new (return-from expand-first= (values (cons new (cdr gwff)) just))))
  (multiple-value-bind (new just)
		       (expand-first= (cdr gwff))
		       (when new (return-from expand-first= (values (cons (car gwff) new) just))))
  (values nil nil))


(defwffop select
  (argtypes anything)
  (argnames etree)
  (resulttype etree)
  (mainfns select%)
  (applicable-q 
   (lambda (label)
     (let ((label (if (etree-q label) label
		      (find-etree-node-name label))))
       (and (or (selection-p label) (leaf-p* label))
	    (if (leaf-p* label)
		(if (positive-p label) 
		    (e-bd-wff-p (get-shallow label))
		    (a-bd-wff-p (get-shallow label))))
	    (not (eproof-skolem-method current-eproof))))))
  (mhelp "SELECT for a given universal or existential quantifier."))


(defmateop sel
  (mate-alias select)
  (mate-result-> current-topnode)
  (matewff-argname etree))

(defun select% (leaf)
  (let ((oldnode (if (etree-q leaf) leaf
		     (find-etree-node-name leaf))))
    (deepen-leaf-node oldnode)))


(defwffop expand
  (argtypes gwff anything)
  (argnames term etree)
  (resulttype etree)
  (applicable-q 
   (lambda (term label)
     (let ((label (if (etree-q label) label
		      (find-etree-node-name label))))
       (and (or (expansion-p label) (leaf-p* label))
	    (if (leaf-p* label)
		(if (positive-p label) 
		    (a-bd-wff-p (get-shallow label))
		    (e-bd-wff-p (get-shallow label)))
		t)
	    (type-equal (bindvar (get-shallow label)) term)))))
  (mhelp "EXPAND a given universal or existential quantifier."))

(defun expand (term label)
  (let ((label (if (etree-q label) label
		   (find-etree-node-name label))))
    (if (and (leaf-p* label)
	     (not (expansion-p label)))
	(let ((newnode 
		(create-expansion-node 
		  (leaf-shallow label) 
		  (positive-p label)
		  (free-vars-of label)
		  (etree-parent label)
		  label)))
	  (add-expansion newnode term))
	(add-expansion label term))))

(defmateop exp
  (mate-alias expand)
  (mate-result-> current-topnode)
  (matewff-argname etree))


;;; Assume that substitute-in-etree figures which are skolem constants
;;; and which are variables in the new term 

(defun add-expansion (label term)
  (if (expansion-p label)
      (progn
        (duplicate-var label)
        (substitute-in-etree term (car (last (expansion-terms label))) label)
        label)
      (throwfail label " is not an expansion node.")))


(defwffop sel-exp-terms
  (argtypes gwff)
  (resulttype gwfflist)
  (argnames gwff)
  (applicable-q (lambda (gwff) (and (etree-q gwff) (memq (type-of gwff)
					  '(expansion selection skolem)))))
  (mhelp "Get the expansion terms of an expansion node or the
selected variable of a selection node."))

(defmateop terms
  (mate-alias sel-exp-terms)
  (matewff-argname gwff))

(defun sel-exp-terms (gwff)
  (cond ((label-q gwff) (apply-label gwff (sel-exp-terms gwff)))
	(t (throwfail "SEL-EXP-TERMS can only be applied to labels."))))

(context substitution)

(defwffop substitute-in-etree
  (argtypes gwff gwff gwff)
  (argnames term var etree)
  (mainfns substitute-in-etree)
  (applicable-q 
   (lambda (term var etree)
     (and (etree-q etree)
	  (is-variable var)
	  (type-equal term var)
	  (or (assoc var (eproof-free-vars-in-etree current-eproof))
              (assoc var (eproof-free-vars-in-etree current-eproof)
                     :key #'exp-var-var))
          )))
  (mhelp "Substitute a term for a variable throughout an expansion tree. 
Destructively alters the expansion tree."))




(defun substitute-in-etree (term var etree &optional
                                 (normalize-p t))
  (declare (ignore etree))
  (let* ((free-vars-in-term (free-vars-of term))
         (sub-list nil)
         (foo nil)
         (var (if (exp-var-p var) 
                  var
                  (car (assoc var (eproof-free-vars-in-etree current-eproof)
                              :key #'exp-var-var))))
         (exp (cdr (assoc var (eproof-free-vars-in-etree current-eproof)))))
    (dolist (new-var free-vars-in-term
                     (if sub-list 
                         (setq term 
                               (simul-substitute-term-var sub-list term))))
      (cond ((or (exp-var-p new-var) (skolem-term-p new-var)))
            ((setq foo 
                   (or 
                    (car (assoc new-var 
                                (eproof-free-vars-in-etree master-eproof)
                                :key #'exp-var-var))
                    (car (member new-var 
                                 (eproof-substitution-list master-eproof)
                                 :key #'exp-var-var))
                    (car 
                     (assoc new-var 
                            (eproof-skolem-constants master-eproof)
                            :key #'skolem-term-parameter))))
             (push (cons new-var foo) sub-list))
            ((assoc new-var (eproof-skolem-constants master-eproof)
                    :key #'(lambda (x) (head (skolem-term-term x)))))
            (t (push (cons new-var 
                           (make-exp-var :var new-var :subst new-var))
                     sub-list)
               (let ((pair (cons (cdar sub-list) exp)))
                 (push pair
                       (eproof-free-vars-in-etree master-eproof))
                 (pushnew pair
                          (eproof-free-vars-in-etree current-eproof))))))
    (let* ((rename-all-bd-vars t)
           (term (if normalize-p
                     (ab-normalize (lambda-norm term))
                     term)))
      (setf (exp-var-subst var) term)
      (push var (eproof-substitution-list master-eproof))
      (pushnew var (eproof-substitution-list current-eproof))
      (setf (eproof-free-vars-in-etree master-eproof)
            (delete var (eproof-free-vars-in-etree master-eproof)
                    :key #'car))
      (setf (eproof-free-vars-in-etree current-eproof)
            (delete var (eproof-free-vars-in-etree current-eproof)
                    :key #'car))
      (setf (expansion-prim-vars exp) 
            (delete var (expansion-prim-vars exp) :key #'car))
      (dolist (new-var (substitutable-vars-of term))
        (pushnew (list new-var) (expansion-prim-vars exp)
                 :key #'car))
      )))

(defun substitutable-vars-of (term)
  "Returns the actual exp-vars which have not been substituted for."
  (cond ((label-q term)
         (apply-label term (substitutable-vars-of term)))
        ((lsymbol-q term) nil)
        ((boundwff-q term)
         (substitutable-vars-of (gdr term)))
        (t
         (append (substitutable-vars-of (car term))
                 (substitutable-vars-of (cdr term))))))


(defun substitute-in-etree-main (term var etree)
  (if (etree-q etree)
      (apply-label etree (substitute-in-etree-main term var etree))
      (throwfail "Substitute-in-etree can only be applied to etrees.")))

(defun simul-substitute-in-etree (theta topnode)
  (when theta
    (setf (eproof-substitution-list current-eproof) 
	  (append theta (eproof-substitution-list* current-eproof)))
    (simul-substitute-in-etree-main theta topnode))
  topnode)



(defun simul-substitute-in-etree-main (theta etree)
  (if (etree-q etree)
      (apply-label etree (simul-substitute-in-etree-main theta etree))
      (throwfail "Substitute-in-etree can only be applied to etrees.")))

(defun substitute-term-var-etree (term var inwff)
  (simul-substitute-term-var-etree (acons var term nil) inwff))

(defun simul-substitute-term-var-etree (alist inwff)
  (or (simul-substitute-term-var-rec-etree alist inwff) inwff))

(defun simul-substitute-term-var-rec-etree (alist inwff)
  (cond ((label-q inwff)
	 (apply-label inwff (simul-substitute-term-var-rec-etree alist inwff)))
  	((or (lsymbol-q inwff) (skolem-term-p inwff))
	 (let ((new (assoc inwff alist)))
	   (if new (cdr new) nil)))
	((boundwff-q inwff)
	 (let* ((bdvar (bdvar inwff))
		(new (simul-substitute-term-var-rec-etree
		      (acons bdvar nil alist) (cdr inwff))))
	   (if new (cons (car inwff) new) nil)))
	(t (let ((left (or (simul-substitute-term-var-rec-etree alist (car inwff))
			   (car inwff)))
		 (rt (or (simul-substitute-term-var-rec-etree alist (cdr inwff))
			 (cdr inwff))))
	     (unless (and (eq left (car inwff)) (eq rt (cdr inwff)))
		     (cons left rt))))))

;;; Assume that wff is actually an equivalence

(defun equiv-to-implics (wff)
  (let ((left (glr wff))
	(right (grr wff)))
    (acons 'and (acons 'implies left right)
	   (acons 'implies right left))))

(defwffop duplicate-var
  (argtypes anything)
  (argnames gwff)
  (arghelp "Expansion node to duplicate in")
  (applicable-q
   (lambda (gwff)
     (let ((etree (if (etree-q gwff) gwff
		      (if (symbolp gwff) (find-etree-node-name gwff)))))
       (expansion-p etree))))
  (resulttype gwff)
  (wffop-type "O")
  (mhelp "Duplicate a variable at an expansion node."))

(context expansion-trees)
(defmateop dup-var
  (mate-alias duplicate-var)
  (matewff-argname gwff)
  (mate-result-> current-topnode))



(defun duplicate-var (exp)
  (let* ((exp (if (expansion-p exp) exp
		  (find-etree-node-name exp)))
	 (bdwff (get-shallow exp)))
    (signal-event 'dupe-var (bindvar bdwff))
    (really-duplicate-var exp bdwff)
    exp))


(defun really-duplicate-var (exp bdwff)
  (let* ((new-var* (funcall ren-var-fn (bindvar bdwff)))
         (new-var (make-exp-var :var new-var* :subst new-var*))
         (pair (cons new-var exp)))
    (push pair
	  (eproof-free-vars-in-etree current-eproof))
    (pushnew pair
             (eproof-free-vars-in-etree master-eproof))
    (setf (expansion-terms exp) 
	  (nconc (expansion-terms exp) (list new-var)))
    (setf (expansion-prim-vars exp)
	  (nconc (expansion-prim-vars exp) (list (list new-var))))
    (setf (etree-components exp)
	  (nconc (etree-components exp)
		 (list (create-leaf-node 
                        (substitute-term-var
                         new-var (bindvar bdwff) (gdr bdwff))
                        (positive-p exp) 
                        (cons new-var (etree-free-vars exp)) exp))))
    (update-status nil (car (last (etree-components exp)))
		   (etree-status* exp))
    (add-to-syms (car (last (etree-components exp)))
		 (car (etree-components exp)))))

;;;an etree version of jform-path-below
;;;this is for flag MAX-DUP-PATHS
(defun paths-below-etree (etree)
   (cond ((leaf-p* etree) 1)
         ((eq (etree-junctive* etree) 'dis)
          (let ((i 0))
               (dolist (etr (etree-components* etree) i)
                       (incf i (paths-below-etree etr)))))
          ((eq (etree-junctive* etree) 'con)
           (let ((i 1))
                (dolist (etr (etree-components* etree) i)
                        (setq i (* i (paths-below-etree etr))))))
          (t (paths-below-etree (car (etree-components* etree))))))


(defwffop duplicate-all-outer-vars
  (argtypes etree)
  (argnames gwff)
  (applicable-q (lambda (gwff) (declare (ignore gwff)) t))
  (resulttype etree)
  (wffop-type "O")
  (mhelp "Duplicate all outermost variables in an expansion tree."))

(defmateop dup-outer
  (mate-alias duplicate-all-outer-vars)
  (matewff-argname gwff)
  (mate-result-> current-topnode))

(defun duplicate-all-outer-vars (etree)
  (let ((newetree (duplicate-all-outer-vars* etree)))
    (if (eq etree (eproof-etree current-eproof))
	(setf (eproof-etree current-eproof) newetree))
    (if (eq etree (eproof-etree master-eproof))
	(setf (eproof-etree master-eproof) newetree))
    newetree))

(defun duplicate-all-outer-vars* (etree)
  (declare (special max-dup-paths))
  (cond ((leaf-p* etree) etree)
	((expansion-p etree)
	 (if (or (infinite-p max-dup-paths)
                 (<= (paths-below-etree etree) max-dup-paths))
             (duplicate-var etree)))
	(t (mapc #'duplicate-all-outer-vars*
		 (etree-components* etree))
	   etree)))

(defwffop duplicate-all-vars
  (argtypes etree)
  (argnames gwff)
  (applicable-q (lambda (gwff)(declare (ignore gwff)) t))
  (resulttype etree)
  (wffop-type "O")
  (mhelp "Duplicate all variables in an expansion tree."))

(defmateop dup-all
  (mate-alias duplicate-all-vars)
  (matewff-argname gwff)
  (mate-result-> current-topnode))

(defun duplicate-all-vars (etree)
  (let ((newetree (duplicate-all-vars* etree)))
    (if (eq etree (eproof-etree current-eproof))
	(setf (eproof-etree current-eproof) newetree))
    (if (eq etree (eproof-etree master-eproof))
	(setf (eproof-etree master-eproof) newetree))
    newetree))

(defun duplicate-all-vars* (etree)
  (declare (special max-dup-paths))
  (cond ((leaf-p* etree) etree)
	((expansion-p etree)
	 (mapc #'duplicate-all-vars* 
	       (etree-components* etree))
	 (if (or (infinite-p max-dup-paths)
                 (<= (paths-below-etree etree) max-dup-paths))
             (duplicate-var etree)))
	(t (mapc #'duplicate-all-vars*
		 (etree-components* etree))
	   etree)))


;*;Assumes that ETREE is an expansion node.


(defun apply-prim-subs (etree &optional (vars* nil))
  (setq *after-primsub* t)
  (let* ((vars (if vars* 
                   (mapcar #'list vars*)
                   (expansion-prim-vars etree)))
         (bdwff (get-shallow etree))
         (scope-bdwff (gdr bdwff))
         (vars-of-scope (substitutable-vars-of scope-bdwff)))
      (dolist (var vars)
	(setq var (car var))
	(unless (cdr (assoc var (expansion-prim-vars etree))) ; prim subs already
          (when (apply-prim-subst-for-var var)
	      (let* ((term (dolist (term (expansion-terms etree) nil)
			    (when (memq var (hvars term))
			      (return term))))
                     new-term
                     (new-kids nil)
                     (new-vars-of-term 
                      (when term 
                        (mapcar #'exp-var-var
                                (set-difference 
                                  (substitutable-vars-of term)
                                  vars-of-scope)))))
		(when term
		  (dolist (prim-sub (find-prim-substs var)
				    (push (cons var (nreverse new-kids))
					  (expansion-prim-vars etree)))
		    (setq new-term 
                          (substitute-term-var (subst-term prim-sub)
                                               (strip-exp-vars var)
                                               (strip-exp-vars term)))
                    (dolist (new-var new-vars-of-term)
                      (setq new-term
                            (substitute-term-var (funcall ren-var-fn new-var)
                                                 new-var
                                                 new-term)))
		    (duplicate-var etree)
		    (substitute-in-etree new-term 
					 (car (last (expansion-terms
						     etree)))
					 etree)
		    (push (car (last (etree-components etree)))
			  new-kids))))))))
  etree)



(defwffop apply-prim-subs
  (argtypes anything)
  (argnames gwff)
  (arghelp "Expansion node to apply primitive substitutions")
  (applicable-q
   (lambda (gwff)
     (let ((etree (if (etree-q gwff) gwff
		      (if (symbolp gwff) (find-etree-node-name gwff)))))
       (and (expansion-p etree)(not (leaf-p* etree))))))
  (resulttype gwff)
  (wffop-type "O")
  (mhelp "Apply primitive substitutions at an expansion node."))

(context expansion-trees)
(defmateop prim-sub
  (mate-alias apply-prim-subs)
  (matewff-argname gwff)
  (mate-result-> current-topnode))

(defwffop apply-prim-subs-outer
  (argtypes etree)
  (argnames gwff)
  (applicable-q (lambda (gwff)(declare (ignore gwff)) t))
  (resulttype etree)
  (wffop-type "O")
  (mhelp "Apply primitive substitutions at all outer expansion nodes."))

(defmateop prim-outer
  (mate-alias apply-prim-subs-outer)
  (matewff-argname gwff)
  (mate-result-> current-topnode))

(defun apply-prim-subs-outer (etree)
  (setq *after-primsub* t)
  (let ((newetree (apply-prim-subs-outer* etree)))
    (if (eq etree (eproof-etree current-eproof))
	(setf (eproof-etree current-eproof) newetree))
    (if (eq etree (eproof-etree master-eproof))
	(setf (eproof-etree master-eproof) newetree))
    newetree))

(defun apply-prim-subs-outer* (etree)
  (cond ((leaf-p* etree) etree)
	((expansion-p etree)
	 (apply-prim-subs etree))
	(t (mapc #'apply-prim-subs-outer*
		 (etree-components* etree))
	   etree)))

(defwffop apply-prim-subs-all
  (argtypes etree)
  (argnames gwff)
  (applicable-q (lambda (gwff)(declare (ignore gwff)) t))
  (resulttype etree)
  (wffop-type "O")
  (mhelp "Apply primitive substitutions at all outermost expansion nodes."))

(defmateop prim-all
  (mate-alias apply-prim-subs-all)
  (matewff-argname gwff)
  (mate-result-> current-topnode))

(defun apply-prim-subs-all (etree)
  (setq *after-primsub* t)
  (let ((newetree (apply-prim-subs-all* etree)))
    (if (eq etree (eproof-etree current-eproof))
	(setf (eproof-etree current-eproof) newetree))
    (if (eq etree (eproof-etree master-eproof))
	(setf (eproof-etree master-eproof) newetree))
    newetree))

(defun apply-prim-subs-all* (etree)
  (cond ((leaf-p* etree) etree)
	((expansion-p etree)
	 (mapc #'apply-prim-subs-all* 
	       (etree-components etree))
	 (apply-prim-subs etree))
	(t (mapc #'apply-prim-subs-all*
		 (etree-components etree))
	   etree)))


;;; find the expansion above node where var occurs, then apply the substitution
;;; beginning there

(defun apply-sub-to-etree (node term var)
  (multiple-value-bind (exp-node n)
    (do ((exp-node (etree-parent node) (etree-parent exp-node))
	 (n (position node (etree-components (etree-parent node)))
	    (position exp-node (etree-components (etree-parent exp-node)))))
	((and (expansion-p exp-node)
	      (wffeq var (nth n (expansion-terms exp-node))))
	 (values exp-node n)))
    (push (copy-etree-rec (nth n (etree-components exp-node)))
	  (etree-components exp-node))
    (push term (expansion-terms exp-node))
    (substitute-in-etree-main term var (car (etree-components exp-node)))
    (find-etree-node-name (concatenate 'string (etree-name node) "A")
			  (car (etree-components exp-node)))))

    
(defwffop prim-single
  (argtypes gwff gwff gwff)
  (argnames subst var etree)
  (resulttype gwff)
  (applicable-q 
   (lambda (term var etree)
     (and (etree-q etree)
	  (is-variable var)
	  (type-equal term var)
	  (or (assoc var (eproof-free-vars-in-etree current-eproof))
              (assoc var (eproof-free-vars-in-etree current-eproof)
                     :key #'exp-var-var))
          )))
  (mhelp "Applies a single primsub. These can be generated by using
the NAME-PRIM command. The command PRIM-SINGLE destructively alters
the etree and creates a new jform, and is basically equivalent to
SUB-ETREE followed by DP* and CJFORM. The variable must be specified
in full detail, with both superscript and type, as in the vpform 
(e.g. \"r^1(ob(ob))\")."))

(defun prim-single (subst var etree)
  (substitute-in-etree subst var etree)
  (setq *after-primsub* t)
  (deepen-to-literals etree)
  (setq *after-primsub* nil)
  (cr-eproof-jform))

(defmateop prim-single
  (mate-alias prim-single)
  (mate-result-> ignore)
  (matewff-argname etree))

(defmateop set-search-tree
    (mate-alias set-search-tree)
  (mate-result-> ignore)
  (mhelp "Set the current etree to be a tree generated and named by NAME-PRIM
when PRIMSUB-METHOD is PR00."))

(defwffop set-search-tree
  (argnames etree)
  (argtypes symbol)
  (arghelp "Which Etree or Eproof")
  (defaultfns (lambda (etree)
		(list etree)))
  (mainfns set-search-tree)
  (mhelp "Set the current etree to be a tree generated and named by NAME-PRIM
when PRIMSUB-METHOD is PR00."))

(defun set-search-tree (name)
  (declare (special *using-unifhash*))
  (unless (and (symbolp name)
	       (get name 'ftree))
    (throwfail name " is not the name of an instantiated tree."))
  (ftree-to-etree (get name 'ftree))
  (setf (eproof-dissolve current-eproof) (get name 'conns))
  (when first-order-mode-ms  ; may have been set to T if the instantiated dissolved jform is first order
    (setq *using-unifhash* nil))	; so the value for *using-unifhash* (used by MS98-1) set in init-everything may be inappropriate
					; (note FIRST-ORDER-MODE-MS T + *USING-UNIFHASH* T are incompatible; the dags will cause a bug)
					; - cebrown 11/15/00
  (cr-eproof-jform))
  
(defmateop name-prim
  (mate-alias name-primsubsts2)
  (mate-result-> ignore)
  (matewff-argname etree)
  (mhelp "If PRIMSUB-METHOD is something other than PR00,
NAME-PRIM lists all possible primitive substitutions for the current
shallow formula. See the flags PRIM-BDTYPES, MIN-PRIM-DEPTH, 
MAX-PRIM-DEPTH and PRIM-QUANTIFIER for information on how to change
which substitutions are generated.  One can use PRIM-SINGLE to
instantiate a set variable with one of the generated primsubs.

If PRIMSUB-METHOD is PR00, this creates a list of instantiated
etrees.  One can choose to do a mating search on one of these
using the mate operation SET-SEARCH-TREE."))

(defwffop name-primsubsts2
  (argtypes gwff)
  (argnames etree)
  (resulttype gwff)
  (mhelp "This is exactly the same function as name-primsubsts, but applies
to etrees rather than gwffs."))

;(defun name-primsubsts2 (etree)
;  (primlist (name-primsubsts (get-shallow etree))))
;old version MB Wed Apr 26 15:33:55 1995

(defun name-primsubsts2 (etree)
  (declare (special primsub-method ms98-rew-primsubs ms98-init ms98-max-prims
		    *pr00-setsubs*))
  (declare (special prim-bdtypes prim-bdtypes-auto prim-hashtable))
  (if (eq primsub-method 'pr00)
      (progn
	(generate-pr00-setsubs		; this actually works on the etree in the etree slot of current-eproof instead of the etree arg 
	 (if (and (eq default-ms 'ms98) ms98-rew-primsubs (boundp '*leibniz-var-list*))
	     (mapcar #'car *leibniz-var-list*)
	   nil)
	 (if (and (eq default-ms 'ms98) (eq ms98-init 2))
	     ms98-max-prims
	   0))
	(dolist (fc *pr00-setsubs*)
	  (let ((name (create-name 'ETREE)))
	    (putprop name (car fc) 'ftree)
	    (putprop name (cdr fc) 'conns)
	    (msgf t name t)
	    (msgf "Connections: " (cdr fc))
	    (msgf "(Normalized) Substitutions in Instantiated Expansion Tree:" t)
	    (dolist (sub (substitutions-in-ftree (car fc)))
	      (msgf (car sub) " --> " ((cdr sub) . gwff))))))
    (progn
      (case prim-bdtypes-auto
	((replace) (setq prim-bdtypes (find-prim-types (get-deep etree))))
	((replace-sub) (setq prim-bdtypes (find-prim-subtypes (get-deep etree))))
	((append) (setq prim-bdtypes (union prim-bdtypes (find-prim-types (get-deep etree)))))
	((append-sub) (setq prim-bdtypes 
                        (union prim-bdtypes (find-prim-subtypes (get-deep etree))))))
      (clrhash prim-hashtable)
      (primlist (apply #'append		; added apply #'append to fix a bug - cebrown 5/11/00
		       (mapcar #'dont-apply-prim-subs (find-ho-exps (find-exp-nodes etree))))))))

(defun dont-apply-prim-subs (etree &optional (vars* nil))
  (declare (special prim-prefix))
  (let* ((vars (if vars* 
                   (mapcar #'list vars*)
                   (expansion-prim-vars etree)))
	 (returning-list nil))
      (dolist (var vars)
	(setq var (car var))
	(unless (cdr (assoc var (expansion-prim-vars etree))) ; prim subs already
	  (when (apply-prim-subst-for-var var)
		      (setq returning-list (cons ; (append  - changed to cons to fix a bug - cebrown 5/11/00
					    (cons var 
						  (mapcar #'(lambda (prsub)
								(let ((name (create-name prim-prefix)))
								  (putprop name prsub 'subst)
								  (create-weak name (subst-term prsub))))
							  (find-prim-substs var)))
					    returning-list)))))
      returning-list))

;;; We don't want to use the built-in defined copy-etree that we get 
;;; from the defstruct.  Some lisps may not copy the whole structure,
;;; just the etree slots! (namely cmucl) DAN

(defun copy-etree (etree)
  (if (etree-p etree)
      (apply-label etree (copy-etree etree))
    (error "~S is not an etree" etree)))

(defun copy-etree-rec (etree &optional (change-names t))
  (let ((*oldnode-newnode-alist* nil))
    (declare (special *oldnode-newnode-alist*))
    (let ((newetree (copy-etree-rec-aux etree change-names)))
    (dolist (pair *oldnode-newnode-alist* newetree)
      (let* ((old (car pair))
	     (new (cdr pair))
	     (oldsyms (find-syms old)))
	(if oldsyms
	    (dolist (oldsym oldsyms)
	      (let ((newsym (cdr (assoc oldsym
					*oldnode-newnode-alist*))))
		(if (node-symmetry newsym)
		    (add-to-syms new newsym)
		  (progn 
		    (init-symmetry new master-eproof)
		    (add-to-syms newsym new)))))
	  (init-symmetry new master-eproof)))))))

(defun copy-etree-rec-aux (etree &optional (change-names t))
  (declare (special *oldnode-newnode-alist*))
  (let ((new-etree (copy-etree etree)))
    (when change-names
      (setf (etree-name new-etree) 
	(intern-str 
	 (concatenate 'string (symbol-name (etree-name etree)) "A"))))
    (typecase new-etree
      (empty-dup-info (setf (empty-dup-info-shallow new-etree)
			    (copy-tree (empty-dup-info-shallow new-etree))))
      (leaf (setf (leaf-shallow new-etree)
		  (copy-tree (leaf-shallow new-etree))))
      (expansion (setf (expansion-shallow new-etree)
		       (copy-tree (expansion-shallow new-etree)))
		 (setf (expansion-terms new-etree)
		       (copy-tree (expansion-terms new-etree))))
      (selection (setf (selection-shallow new-etree)
		       (copy-tree (selection-shallow new-etree)))
		 (setf (selection-terms new-etree)
		       (copy-tree (selection-terms new-etree))))
      (skolem (setf (skolem-shallow new-etree)
		       (copy-tree (skolem-shallow new-etree)))
		 (setf (skolem-terms new-etree)
		       (copy-tree (skolem-terms new-etree)))))
    (setf (etree-components new-etree)
      (mapcar #'(lambda (x) (let ((y (copy-etree-rec-aux x change-names)))
				  (setf (etree-parent y) new-etree)
				  y))
		  (etree-components new-etree)))
    (setq *oldnode-newnode-alist* (acons etree new-etree
					 *oldnode-newnode-alist*)) 
    new-etree))


(defun etree-components* (wff)
  (if (etree-q wff)
      (do ((kids (etree-components wff) (cdr kids))
	   (comps nil
		  (if (> (etree-status* (car kids)) 0)
		      (cons (car kids) comps)
		      comps)))
	  ((null kids) (nreverse comps)))
      (error "ETREE-COMPONENTS* can only be applied to etrees.")))

(defun leaf-p* (wff)
  (if (etree-q wff)
      (or (leaf-p wff) 
	  (if (and (not (true-p wff))
		   (not (false-p wff)))
	      (null (etree-components* wff))))
      (error "LEAF-P* can only be applied to etrees.")))

(defvar *ignore-statuses* nil)

(defun etree-status* (wff)
  (if (etree-q wff)
      (if *ignore-statuses* 
	  1
	  (current-status wff))
      (error "ETREE-STATUS* can only be applied to etrees.")))

;;; Assumptions: old (if not nil) and new are etrees. value if non-nil
;;; is an integer.

(defun update-status (old new &optional (value nil))
  (let ((value (or value (if old (current-status old) 1))))
    (when old
      (remhash (etree-name old) (eproof-statuses current-eproof))
      (remhash (etree-predecessor old) (eproof-statuses current-eproof)))
    (setf (gethash (etree-name new) (eproof-statuses current-eproof)) value)))

(defun current-status (etree)
  "Get status of etree in current eproof.  Returns 0 if not in status
hashtable." 
  (or (gethash (etree-name etree)
	       (eproof-statuses current-eproof))
      (gethash (etree-predecessor etree)
	       (eproof-statuses current-eproof)
	       0)))

;;; returns a list of substitutions in order of their application

(defun eproof-substitution-list* (eproof)
  (declare (ignore eproof))
  (reverse (eproof-substitution-list master-eproof)))



(defmateop sub-etree
  (mate-alias substitute-in-etree)
  (matewff-argname etree))




(defun higher-order-var (var)	       
  (or (eq 'O (get var 'unabbreviated-type))
      (not (atom (get var 'unabbreviated-type)))))
  
(defun wffeq-ab-etree (wff1 wff2)
  (wffeq-ab-etree1 wff1 wff2 nil))

(defun wffeq-ab-etree1 (wff1 wff2 varstack)
  (cond ((and (skolem-term-p wff1)
	      (skolem-term-p wff2))
	 (wffeq (skolem-term-parameter wff1)
		(skolem-term-parameter wff2)))
	((skolem-term-p wff1)
	 (wffeq-ab-etree1 (skolem-term-term wff1) wff2 varstack))
	((skolem-term-p wff2)
	 (wffeq-ab-etree1 wff1 (skolem-term-term wff2) varstack))
	((lsymbol-q wff1)
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
		(wffeq-ab-etree1 (cdr wff1) (cdr wff2)
				 (acons (caar wff1) (caar wff2) varstack)))
	       (T nil)))
	((boundwff-q wff2) nil)
	(t (and (wffeq-ab-etree1 (car wff1) (car wff2) varstack)
		(wffeq-ab-etree1 (cdr wff1) (cdr wff2) varstack)))))

