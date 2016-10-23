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
(context expansion-trees)

(deffile etrees-wffops
  (part-of expansion-tree)
  (mhelp "Defines wffops used with expansion trees."))

(defvar *instantiated-defs-list* nil)
(defvar *instantiated-eqs-list* nil)
(defvar *hacked-rewrites-list* nil)
(defvar *banned-conns-list* nil)
(defvar *ho-banned-conns-list* nil)
;will be of form (rewrite (gensym . leaf)), where rewrite is the fiddled rewrite, gensym is the
;symbol that was introduced, and leaf is the leaf where the gensym ended up...
(defvar *leibniz-var-list* nil)
(defvar *rew-unsubst-exps* nil)
(defvar *unsubst-exp-vars* nil)
(defvar *after-primsub* nil) ; a toggle that becomes T immediately after a primsub

(defflag add-truth
  (flagtype symbol)
  (default 'if-needed)
  (subjects etrees mating-search transmit)
  (mhelp "When set to IF-NEEDED, tests whether the etree has any path of 
length 1; if it does, then adds a conjunct TRUTH to the vpform.
When set to T, it will always add this conjunct.
When set to NIL, it will never add this conjunct.
(When TRUTHVALUES-HACK is NIL, it will also add a conjunct NOT FALSEHOOD)."))

(defmateop sub
  (mate-alias gwff-to-etree-sub)
  (mate-result-> current-topnode))

(defwffop gwff-to-etree-sub
  (argtypes gwff0-or-label-or-eproof yesno yesno)
  (argnames gwff skolemize deepen)
  (resulttype etree)
  (arghelp  "gwff" "Skolemize?" "Deepen?")
  (defaultfns (lambda (z y w)
		(list (if (eq z '$) 
			  (if current-eproof
			      (get-shallow (eproof-etree current-eproof))
			      '$)
			  z)
		      (if (eq y '$) skolem-default y)
		      (if (eq w '$) t w))))
  (applicable-q (lambda(y z w)(declare (ignore y z w)) t))
  (mhelp "Create an expansion tree from a gwff0."))

(defun gwff-to-etree-sub (gwff skolemize deepen)
  (let* ((hxsymbol (symbolp gwff))
         (truegwff (if hxsymbol 
                       (progn (setq dproof gwff) (get-gwff0 gwff))
                       gwff)))
     (prog1 (gwff-to-etree truegwff skolemize deepen)
;;;To change FIRST-ORDER-MODE-MS appropriately. (hx 8/1/93)
         (setq first-order-mode-ms
                          (first-order-problem-p
                          (mapcar #'(lambda (x) (cons (exp-var-var (car x)) (cdr x)))
                          (eproof-free-vars-in-etree current-eproof))))
         (if (memq default-ms '(ms88 ms89)) (initialize-mating-search)))))

(defun gwff-to-etree (gwff skolemize deepen)
  (cond ((label-q gwff)
	 (apply-label gwff (gwff-to-etree gwff skolemize deepen)))
	((eproof-p gwff)
         (setq last-eproof current-eproof current-eproof gwff
	       master-eproof gwff)
	 (eproof-etree current-eproof))
	(t (let ((newnode (gwff-to-etree1 skolemize gwff)))
	     (update-status nil newnode)
             (when truthvalues-hack
                 (setq newnode (add-falsehood newnode)))
	     (if deepen (progn (deepen-to-literals* newnode)
			       (setq *leibniz-var-list* (leibniz-quant *leibniz-var-list*))
			       (when (and (lazy2-used) *hacked-rewrites-list*)
				     (fiddle-with-def-leaves (eproof-etree current-eproof)))))
	     (when (or (eq add-truth t) (and (eq add-truth 'if-needed)
					     (truth-needed (etree-to-jform (eproof-etree current-eproof)))))
	        (setf (eproof-etree current-eproof) (add-trut (eproof-etree current-eproof) 'TRUTH))
		(unless truthvalues-hack
			;in which case all the FALSEHOODs have gone, so we don't need a NOT FALSEHOOD
			(setf (eproof-etree current-eproof) (add-trut (eproof-etree current-eproof) '(NOT . FALSEHOOD)))))
	     (setf (eproof-jform current-eproof)
		   (etree-to-jform (eproof-etree current-eproof)))
	     (eproof-etree current-eproof)))))

(defun truth-needed (jform)
  (case (jform-type jform)
	(literal t)
	(conjunction nil)
	(disjunction (dolist (d (disjunction-components jform) nil)
			     (when (truth-needed d) (return t))))
	(universal (truth-needed (universal-scope jform)))))

 ; the way this was written created an ill-constructed etree - though the
 ; final proof would work.  The problem was that etree is negative, but the 
 ; nodes created are positive.
 ; changed cebrown 11/26/01
(defun add-trut (etree wff)
  (let ((newnode
	 (make-rewrite
	  :positive nil ; changed cebrown 11/26/01
	  :shallow wff
	  :junctive 'neutral
	  :justification 'add-truth
	  :free-vars (etree-free-vars etree)
	  :predecessor nil
	  :parent nil)))
    (setf (etree-components newnode)
	  (list (create-leaf-node wff t (etree-free-vars etree) newnode)))
    (let ((parent (make-implication 
		   :positive nil ; changed cebrown 11/26/01
		   :free-vars (etree-free-vars etree)
		   :junctive 'con
		   :parent nil
		   :status 1
		   :predecessor nil
		   :components (list newnode etree))))
    (setf (etree-parent newnode) parent)
    (setf (etree-parent etree) parent)
    (update-statuses parent)
    parent)))

(defun lazy2-used ()
  (or (assoc 'lazy2 (cdr rewrite-defns))
      (assoc 'dual (cdr rewrite-defns))
      (eq (car rewrite-defns) 'lazy2)
      (eq (car rewrite-defns) 'dual)
      (eq rewrite-equalities 'dual)
      (eq rewrite-equalities 'lazy2)))

(defun gwff-to-etree1 (skolemize gwff)
  (declare (special lambda-conv))
  (initialize-etree-counters)
  (let* ((gwff  (dissolve-weak* gwff))
        (norm-wff nil)
        (skolem-terms nil)
	(list-of-rewrites nil))
    (multiple-value-setq (gwff skolem-terms)
      (skolemize-gwff gwff))
    (setq list-of-rewrites 
          (list (cons gwff 'topnode)))
    (case lambda-conv
       (beta-eta-together
        (setq norm-wff (lnorm gwff))
	(if (not (wffeq norm-wff gwff))
	    (push (cons norm-wff 'lambda) list-of-rewrites)))
       (beta-only
        (setq norm-wff (lnorm gwff))
        (if (not (wffeq norm-wff gwff))
	    (push (cons norm-wff 'beta) list-of-rewrites)))
       (beta-eta-separate 
        (setq norm-wff (lnorm-beta gwff))
        (if (not (wffeq norm-wff gwff))
	    (push (cons norm-wff 'beta) list-of-rewrites))
	(setq norm-wff (lnorm-eta norm-wff))
	(if (not (wffeq norm-wff gwff))
	    (push (cons norm-wff 'eta) list-of-rewrites))))
  (setq list-of-rewrites
        (rewrite-and-rectify-theorem norm-wff list-of-rewrites))
    (setq current-eproof 
	  (make-eproof
	   :free-vars-in-etree nil
	   :substitution-list nil
	   :leaf-list nil
	   :skolem-constants skolem-terms
	   :skolem-method (if skolemize skolem-default)
	   :skolem-node-list nil))
    (setf (symbol-value (eproof-name current-eproof)) current-eproof)
    (setq master-eproof current-eproof)
    (setf (eproof-etree current-eproof) 
	  (gwff-to-etree-main list-of-rewrites nil))
  (do* ((nodes (list (eproof-etree current-eproof))
	       (append nodes (etree-components node)))
	(node (pop nodes) (pop nodes)))
      ((null node))
    (init-symmetry node current-eproof))
  (eproof-etree current-eproof)))

(defun skolemize-gwff (gwff)
  (let ((free-vars (free-vars-of gwff))
	(skolem-consts nil))
    (setq skolem-consts
	  (mapcar #'(lambda (x) (cons 
                                 (make-skolem-term :term x :parameter x)
                                 0))
                  free-vars))
    (setq gwff
          (simul-substitute-term-var (pairlis free-vars
                                              (mapcar #'car
                                                      skolem-consts))
                                     gwff))
    (values gwff skolem-consts)))

;;; Assume that norm-wff is in lambda-normal form
;;; Returns one values, a list of conses of form
;;; (wff . justification), where each wff results from the
;;; one after it in the list by the justification, either 'lambda
;;; (lambda-normalization), 'equivwffs, 'equality or 'ab.


(defun rewrite-and-rectify-theorem (norm-wff list-of-rewrites)
  (let* ((gwff norm-wff)
	 (defns (get-all-defns norm-wff))
	 (rewrite-defns (fix-rewrite-defns defns)))
    (when truthvalues-hack
          (setq gwff (acons 'OR (falsehood-elim gwff) (cons 'NOT 'TRUTH)))
          (push (cons gwff 'TRUTHP) list-of-rewrites))
    (when min-quantifier-scope
      (let ((new-gwff nil)
	    (rename-all-bd-vars nil))
	(declare (special rename-all-bd-vars))
	(setq new-gwff (min-quant-scope gwff))
	(unless (eq gwff new-gwff)
	  (push (cons new-gwff 'RULEQ) list-of-rewrites)
	  (setq gwff new-gwff))))
    (let ((firstgwff gwff)
	  (rename-all-bd-vars t))
      (declare (special rename-all-bd-vars))
      (do ((most-recent-gwff firstgwff gwff)
	   (previous-gwff nil most-recent-gwff))
	  ((wffeq most-recent-gwff previous-gwff))
	  (when (cdr (assoc 'eager rewrite-defns))
		(let ((rewrite-defns (setdiff (cdr (assoc 'eager rewrite-defns)) '(equiv))))
		  (setq gwff
			(do ((gwff (instantiate-all gwff (setdiff defns rewrite-defns))
				   (instantiate-all gwff (setdiff defns rewrite-defns)))
			     (oldgwff nil gwff))
			    ((wffeq oldgwff gwff) gwff)))))
	  (unless (wffeq gwff most-recent-gwff)
		  (push (cons gwff 'equivwffs) list-of-rewrites))))
    list-of-rewrites))

(defun fix-rewrite-defns (defns)
  (let* ((default (car rewrite-defns))
	 (rest (cdr rewrite-defns))
	 (used (reduce #'append (mapcar #'cdr rest))))
    (cons (cons default (setdiff defns used))
	  (mapcar #'(lambda (x) (cons (car x) (intersection (cdr x) defns))) rest))))

(defun initialize-etree-counters ()
  (dolist (x (list leaf-name edisj-name econj-name imp-name
		   neg-name true-name false-name rewrite-name
		   expansion-name selection-name skolem-selection-name
		   mating-name))
    (reset-name-counter x)))

;;;The following two functions are used when TRUTH and/or FALSEHOOD is
;;;in the would-be-proved formulas.

(defun falsehood-elim (gwff)
  (cond ((and (symbolp gwff) (eq (get gwff 'flavor) 'weak))
         (falsehood-elim (get-weak gwff)))
        ((lsymbol-q gwff) 
         (if (and truthvalues-hack (eq gwff 'falsehood))
             (cons 'NOT 'TRUTH) gwff))
        ((boundwff-p gwff)
	 (cons (car gwff) (falsehood-elim (cdr gwff))))
        ((consp gwff)
         (cons (falsehood-elim (car gwff))
               (falsehood-elim (cdr gwff))))
        (T gwff)))

(defun not-truth-elim (gwff)
  (cond ((and (symbolp gwff) (eq (get gwff 'flavor) 'weak))
         (falsehood-elim (get-weak gwff)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-p gwff)
	 (cons (car gwff) (not-truth-elim (cdr gwff))))
        ((consp gwff)
         (if (and (eq (car gwff) 'NOT) (eq (cdr gwff) 'TRUTH)) 
             'FALSEHOOD 
             (cons (not-truth-elim (car gwff)) (not-truth-elim (cdr gwff)))))
        (T gwff)))

(defun add-falsehood (node)
  (let ((wff (get-shallow node))
	(free-vars (etree-free-vars node))
	(parent (etree-parent node))
	(positive (positive-p node))
	(newnode nil))
    (setq newnode
	  (let ((newwff (if positive (acons 'AND (falsehood-elim wff) 'TRUTH)
                            (acons 'OR (falsehood-elim wff) (cons 'NOT 'TRUTH)))))
	    (create-rewrite-node
		 wff positive newwff 'TRUTHP
		 free-vars parent node)))
   (update-global-lists node newnode)
    newnode))


;;; Assumes that list-of-rewrites always contains at least one pair
(defun gwff-to-etree-main (list-of-rewrites free-vars)
  (do* ((wff-just-pair (pop list-of-rewrites) 
		       (pop list-of-rewrites))
	(newnode (create-leaf-node (car wff-just-pair) nil free-vars nil)
		 (make-rewrite 
		 :positive nil
		 :shallow (car wff-just-pair)
		 :justification just
		 :junctive 'neutral
		 :free-vars free-vars
		 :components (list newnode)
		 :parent nil))
	(just (cdr wff-just-pair) (cdr wff-just-pair)))
       ((null list-of-rewrites)
	(unless (leaf-p newnode)
		(setf (etree-parent (car (etree-components newnode)))
		      newnode))
	newnode)
       (unless (leaf-p newnode)
	       (setf (etree-parent (car (etree-components newnode)))
		     newnode))))

;;; Does a depth-first search of etree until it finds a 
;;; node that satisfies predicate, else returns nil

(defun find-etree-node (predicate &optional
				  (etree (eproof-etree current-eproof))
				  (ignore-statuses nil))
  (if (funcall predicate etree) etree
      (dolist (etree (if ignore-statuses 
			 (etree-components etree)
			 (etree-components* etree)) nil)
	(let ((found (find-etree-node predicate etree ignore-statuses)))
	  (if found (return found))))))

;;; Does a depth-first-search of etree, returns a list of
;;; all nodes that satisfy predicate

(defun find-etree-nodes (predicate &optional
				   (etree (eproof-etree current-eproof))
				   (nodes nil)
				   (ignore-statuses nil))
  (dolist (son (if ignore-statuses 
		   (etree-components etree)
		 (etree-components* etree))
	    (if (funcall predicate etree) (cons etree nodes) nodes))
    (setq nodes (find-etree-nodes predicate son nodes ignore-statuses))))  

(defun find-etree-node-name (symbol &optional (etree
					       (eproof-etree current-eproof))
				    (ignore-statuses nil))
  (find-etree-node #'(lambda (x) (or (string= symbol (etree-name x))
				     (string= symbol (etree-predecessor x)))) 
		   etree
		   ignore-statuses))

(defwffop deepen=
  (argtypes etree)
  (argnames etree)
  (resulttype etree)
  (applicable-q (lambda (etree) (declare (ignore etree)) t))
  (mhelp "Deepen top level equality in the etree."))

(defmateop dp=
  (mate-alias deepen=)
  (mate-result-> current-topnode)
  (matewff-argname etree))

(defun deepen= (node)
  (let* ((wff (get-shallow node))
	 (new-wff nil)
	 (just nil))
    (multiple-value-setq (new-wff just)
       (expand-top= new-wff (positive-p node)))
    (if new-wff
	(let (newnode)
	  (setq newnode (create-rewrite-node
			  wff (positive-p node) new-wff just
			  (etree-free-vars node) (etree-parent node) node))
	  (update-global-lists node newnode)
	  newnode)
      node)))

(defmateop dp
  (mate-alias deepen-one)
  (matewff-argname leaf)
  (mate-result-> current-topnode))

(defwffop deepen-one
  (argtypes anything)
  (argnames leaf)
  (resulttype gwff0)
  (applicable-q
   (lambda (leaf)
     (or (leaf-p* leaf) (and (symbolp leaf) (find-etree-node-name leaf)))))
  (mhelp "Deepen a single leaf of an expansion tree."))

(defun deepen-one (leaf)
  (let ((oldnode (if (leaf-p* leaf) leaf (find-etree-node-name leaf))))
    (if oldnode (deepen-leaf-node oldnode)
	(throwfail leaf " node not found."))))

(defwffop deepen-etree
  (argtypes etree)
  (argnames etree)
  (resulttype etree)
  (applicable-q (lambda (etree) (declare (ignore etree)) t))
  (mhelp "Deepen every leaf node of an expansion tree."))

(defmateop dptree
  (mate-alias deepen-etree)
  (mate-result-> current-topnode)
  (matewff-argname etree))

(defun deepen-etree (etree)
  (if (leaf-p* etree) (deepen-leaf-node etree)
    (if (dual-p etree) (deepen-etree (cadr (etree-components* (car (etree-components* etree))))) ; cebrown - 8/7/00
      (dolist (son (etree-components* etree) etree)
	(deepen-etree son)))))


(defun deepen-leaf-node (node)
  (let ((result (deepen-leaf-node-real node)))
    (if (eproof-etree current-eproof) (relabel-etree (eproof-etree current-eproof)))
    result))

(defun deepen-leaf-node-real (node)
  (declare (special lambda-conv))
  (let* ((wff (get-shallow node))
	 (free-vars (etree-free-vars node))
	 (parent (etree-parent node))
	 (positive (positive-p node))
	 (rename-all-bd-vars t)
	 (new-wff nil)
	 (just nil)
	 (newnode nil)
	 (defns (get-all-defns wff))
	 (rewrite-defns (fix-rewrite-defns defns)))  ; notice we are changing rewrite-defns to be an alist of ([LAZY1|LAZY2|...] . <list of defns>)
    (declare (special rename-all-bd-vars))
    (setq newnode
	  (cond 
           ((let ((newwff (lambda-norm wff)))
               (case lambda-conv
                 ((beta-only beta-eta-separate)
	          (unless (wffeq newwff wff)
		     (create-rewrite-node wff positive newwff 'beta
				          free-vars parent node)))
                 (beta-eta-together 
                  (setq newwff (etanorm newwff))
                  (unless (wffeq newwff wff)
		     (create-rewrite-node wff positive newwff 'lambda
				          free-vars parent node))))))
            ((if (eq lambda-conv 'beta-eta-separate)
                 (let ((newwff (etanorm wff)))
             	     (unless (wffeq newwff wff)
		       (create-rewrite-node wff positive newwff 'eta
				          free-vars parent node)))))
            ((not-p wff)
	     (unless (and add-truth (wffeq wff '(NOT . FALSEHOOD)) (rewrite-p parent)
			  (eq (rewrite-justification parent) 'add-truth))
	     (create-negation-node wff positive free-vars parent node)))
	    ((and-p wff)
	     (create-econjunction-node wff positive free-vars parent node))
	    ((or-p wff)
	     (create-edisjunction-node wff positive free-vars parent node))
	    ((implies-p wff)
	     (create-implication-node wff positive free-vars parent node))
	    ((equiv-p wff)
	     (let ((newwff (rewrite-equiv wff positive)))
	       (create-rewrite-node
		 wff positive newwff 
                 (if (and-p newwff) 'equiv-implics 'equiv-disjs)
		 free-vars parent node)))
	    ;; push quantifiers across connectives if possible
	    ((and min-quantifier-scope
		  (or (a-bd-wff-p wff) (e-bd-wff-p wff))
		  (mqs-applicable wff))
	     (create-rewrite-node 
	       wff positive (min-quant-scope wff) 'ruleq free-vars parent node))
	    ((if positive (e-bd-wff-p wff) (a-bd-wff-p wff))
	     (if (eproof-skolem-method current-eproof)
		 (create-skolem-node wff positive free-vars parent node)
		 (create-selection-node wff positive free-vars parent node)))
	    ((if positive (a-bd-wff-p wff) (e-bd-wff-p wff))
	     (create-expansion-node wff positive free-vars parent node))
;;the chunk between ;;***'s below used to be here. THM250a provides a good reason for the move! MB Mon Jan 27 15:05:45 1997
;;for equalities: when we reach an equality WFF, push (SYM . WFF) onto instantiated-defs
;;and push (SYM2 . WFF) onto instantiated-eqs; then work with ((AND SYM) SYM2).
	    ((and (neq rewrite-equalities 'auto::none)
		  (equals-p wff)
		  (wffeq (gdr wff) (gdr (gar wff))))
	     (create-rewrite-node wff positive 'TRUTH 'refl=
				  free-vars parent node))
	    ((and (neq rewrite-equalities 'auto::none)
		  (equals-p wff)
		  (if (eq rewrite-equalities 'auto::only-ext)
		      (contains-ext= (gar (gar wff)))
		      t)
		  (if (or (eq rewrite-equalities 'auto::lazy2) (eq rewrite-equalities 'dual))
		      (let ((sym (gensym))
			    (sym2 (gensym))
			    (wff2 (expand-top= wff positive))) ; we could also have a "dual-parity" setting handled as a mixture if this and parity1 (see below) - cebrown 3/12/00
			(when wff2
			      (push (cons sym2 wff) *instantiated-eqs-list*)
			      (push (cons sym wff) *instantiated-defs-list*)
			      (setq new-wff (if positive 
						(cons (cons 'and sym) sym2)
					      (cons (cons 'or sym) sym2)))
			      (setq just nil))
			wff2)
		    (multiple-value-setq (new-wff just) (expand-top= wff positive))))
	     (let ((node (create-rewrite-node wff positive 
					      (if (or (not just) (eq just 'both=) (eq (gar (gar new-wff)) 'equiv)) ; both to both= cebrown
						  new-wff
						(lcontr 
						 (cons
						  (lcontr (gar new-wff))
						  (gdr new-wff))))
					      just 
					      free-vars parent node)))
	       (when (not just)
		     (push (cons node (cons (cdar new-wff) nil)) *hacked-rewrites-list*)
		     (setf (rewrite-justification node) (cdar new-wff)))
	       (when (eq just 'leibniz=) (push node *leibniz-var-list*))
	       node))
	    ((and (symbolp wff) (assoc wff *instantiated-eqs-list*))
	     (let ((sym wff))
	       (setq wff (cdr (assoc wff *instantiated-eqs-list*)))
	       (setq *instantiated-eqs-list* (remove-if #'(lambda (x) (eq (car x) sym)) *instantiated-eqs-list*))
	       (multiple-value-setq (new-wff just) (expand-top= wff positive))
	       (let ((node (create-rewrite-node wff positive 
				    (if (or (eq just 'both) (eq (gar (gar new-wff)) 'equiv))
					new-wff
				      (lcontr 
				       (cons
					(lcontr (gar new-wff))
					(gdr new-wff))))
				    just 
				    free-vars parent node)))
		 (when (eq just 'leibniz=) (push node *leibniz-var-list*))
		 node)))
;;***
	    ((and (or (cdr (assoc 'eager rewrite-defns))  ; note again that this is not the usual value of the flag rewrite-defns
		      (cdr (assoc 'lazy1 rewrite-defns)) ;  see the comment at the beginning of deepen-leaf-node-real (this function)
		      (cdr (assoc 'dual rewrite-defns)) ; where fix-rewrite-defns is called. - cebrown 10/5/00
		      (cdr (assoc 'lazy2 rewrite-defns)))
		  (let ((rewrite-defns (setdiff (append (cdr (assoc 'eager rewrite-defns))  ; and now we temporarily set rewrite-defns
							(cdr (assoc 'dual rewrite-defns))  ; to be a list of defns we should rewrite
							(cdr (assoc 'lazy1 rewrite-defns)) ; so that contains-some-defn will work
							(cdr (assoc 'lazy2 rewrite-defns)))
						'(equiv))))
		    (contains-some-defn wff)))
	     (let ((firstgwff wff)
		   (gwff wff))
	       (when (cdr (assoc 'eager rewrite-defns))
		     (let ((rewrite-defns (setdiff (cdr (assoc 'eager rewrite-defns)) '(equiv))))
		       (setq gwff
			     (do ((gwff (instantiate-all gwff (setdiff defns rewrite-defns)) ; send it a list of exceptions
					(instantiate-all gwff (setdiff defns rewrite-defns)))
				  (oldgwff nil gwff))
				 ((wffeq oldgwff gwff) gwff)))))
	       (when (and (wffeq gwff firstgwff) (cdr (assoc 'lazy1 rewrite-defns)))
		     (let ((rewrite-defns (setdiff (cdr (assoc 'lazy1 rewrite-defns)) '(equiv))))
		       (setq gwff (instantiate-1-from-list gwff rewrite-defns))))
	       (when (and (wffeq gwff firstgwff) (or (cdr (assoc 'lazy2 rewrite-defns))
						     (cdr (assoc 'dual rewrite-defns))))
		     (let ((rewrite-defns (setdiff (or (cdr (assoc 'lazy2 rewrite-defns))
						       (cdr (assoc 'dual rewrite-defns)))
						   '(equiv))))
		       (setq firstgwff 'lazy2)
		       (setq gwff (instantiate-1b-from-list gwff positive rewrite-defns))))
	       (let ((node (create-rewrite-node
			    wff positive gwff
			    (if (eq firstgwff 'lazy2) (cdar gwff) 'equivwffs) free-vars parent node)))
		 (when (eq firstgwff 'lazy2) 
		       (push (cons node (cons (cdar gwff) nil)) *hacked-rewrites-list*))
		 node)))
;;***
	    ((wffeq wff 'TRUTH)
	     (unless 
		 (or truthvalues-hack (and add-truth (rewrite-p parent) (eq (rewrite-justification parent) 'add-truth))) 
	       (create-true-node wff positive free-vars parent node)))
	    ((wffeq wff 'FALSEHOOD)
	     (unless (and add-truth (rewrite-p parent) (eq (rewrite-justification parent) 'add-truth))
		     (create-false-node wff positive free-vars parent node)))
	    (t nil)))
    (cond (newnode
	    (update-global-lists node newnode)
	    newnode)
	  (T node))))

(defun instantiate-1b (inwff pos)
  (let ((oneflag nil)
	(sym (gensym)))
    (declare (special oneflag))
    (push (cons sym inwff) *instantiated-defs-list*)
    (if pos (cons (cons 'and sym) (instantiate-definitions
				   inwff #'(lambda (abbsym chkarg)
					     (declare (ignore abbsym chkarg) (special oneflag))
					     (prog1 (not oneflag) (setq oneflag t)))
				   nil))
      (cons (cons 'or sym) (instantiate-definitions
			     inwff #'(lambda (abbsym chkarg)
				       (declare (ignore abbsym chkarg) (special oneflag))
				       (prog1 (not oneflag) (setq oneflag t)))
			     nil)))))

(defun instantiate-1c (inwff pos)
  (let ((oneflag nil))
    (declare (special oneflag))
    (let ((newwff (instantiate-definitions
		   inwff #'(lambda (abbsym chkarg)
			     (declare (ignore abbsym chkarg) (special oneflag))
			     (prog1 (not oneflag) (setq oneflag t)))
		   nil)))
      (if pos (cons (cons 'and inwff) newwff)
	(cons (cons 'or inwff) newwff)))))

(defun contains-defn-not-equiv (wff)
  (cond ((label-q wff) (apply-label wff (contains-defn-not-equiv wff)))
	((lsymbol-q wff)
	 (and (neq wff 'equiv) (contains-defn wff)))
	((boundwff-q wff)
	 (or (get (binding wff) 'defn) (contains-defn-not-equiv (gdr wff))))
	(t 
	 (or (contains-defn-not-equiv (gar wff))
	     (contains-defn-not-equiv (gdr wff))))))

(defun contains-some-defn (wff)
  (cond ((label-q wff) (apply-label wff (contains-some-defn wff)))
	((lsymbol-q wff)
	 (and (neq wff 'equiv) (contains-defn-1 wff)))
	((boundwff-q wff)
	 (or (member (get (binding wff) 'core::stands-for) rewrite-defns)
	     (member (binding wff) rewrite-defns) 
	     (contains-some-defn (gdr wff))))
	(t
	 (or (contains-some-defn (gar wff))
	     (contains-some-defn (gdr wff))))))

(defun contains-defn-1 (gwff)
  (cond ((label-q gwff) (apply-label gwff (contains-defn-1 gwff)))
	((lsymbol-q gwff)
	 (if (or (member gwff rewrite-defns)
		 (member (get gwff 'core::stands-for) rewrite-defns))
	     t nil))
	((boundwff-q gwff)
	 (if (or (member (get (binding gwff) 'core::stands-for) rewrite-defns)
		 (member (binding gwff) rewrite-defns) 
		 (contains-defn-1 (gdr gwff)))
	     t nil))
	(t (or (contains-defn-1 (gar gwff)) (contains-defn-1 (gdr gwff))))))

(defun instantiate-some (wff defns)
  (dolist (defn defns)
	  (setq wff (instantiate-defn defn wff)))
  wff)

(defun instantiate-1-from-list (inwff defns)
  (let ((oneflag nil))
    (declare (special oneflag))
    (instantiate-definitions
     inwff #'(lambda (abbsym chkarg)
	       (declare (ignore chkarg) (special oneflag))
	       (if (memq abbsym defns) 
		   (prog1 (not oneflag) (setq oneflag t))
		 nil))
     nil)))

(defun instantiate-1b-from-list (inwff pos defns)
  (let ((oneflag nil)
	(sym (gensym)))
    (declare (special oneflag))
    (push (cons sym inwff) *instantiated-defs-list*)
    (if pos (cons (cons 'and sym) (instantiate-definitions
				   inwff #'(lambda (abbsym chkarg)
					     (declare (ignore chkarg) (special oneflag))
					     (if (memq abbsym defns) 
						 (prog1 (not oneflag) (setq oneflag t))
					       nil))
				   nil))
      (cons (cons 'or sym) (instantiate-definitions
			     inwff #'(lambda (abbsym chkarg)
				       (declare (ignore chkarg) (special oneflag))
				       (if (memq abbsym defns) 
					   (prog1 (not oneflag) (setq oneflag t))
					 nil))
			     nil)))))

;;; Added symmetry as hash-table 10MAY91 DAN

(defstruct (symmetry-holder (:print-function print-symmetry-holder))
  (class nil :type list))

(defvar *print-symmetry-verbose* t)

(defun node-symmetry (node)
  (gethash node (eproof-symmetry master-eproof)))

(defun update-node-symmetry (node new-val)
  (setf (gethash node (eproof-symmetry master-eproof)) new-val))

(defsetf node-symmetry update-node-symmetry)

(defun print-symmetry-holder (o s d)
  (declare (ignore d))
  (if *print-symmetry-verbose*
      (prin1 (symmetry-holder-class o) s)
    (format s "(~A ... )" (symmetry-holder-class o))))

(defun update-global-lists (oldnode newnode)
  (let ((parentnode (etree-parent oldnode)))
    (unless (eq oldnode newnode)
      (setf (eproof-free-vars-in-etree master-eproof)
	    (subst newnode oldnode (eproof-free-vars-in-etree
				     master-eproof)))
      (if (eq current-topnode oldnode) (setq current-topnode newnode))
      (if (eq oldnode (eproof-etree master-eproof)) 
	  (setf (eproof-etree master-eproof) newnode))
      (if (eq oldnode (eproof-etree current-eproof)) 
	  (setf (eproof-etree current-eproof) newnode))
      (if (leaf-p oldnode)
	  (setf (eproof-leaf-list master-eproof)
		(delete oldnode (eproof-leaf-list master-eproof))))
      (if parentnode (setf (etree-components parentnode)
			   (subst newnode oldnode
				  (etree-components parentnode))))
      (if (skolem-p newnode)
	  (push newnode (eproof-skolem-node-list current-eproof)))
      (update-symmetry oldnode newnode))))

(defun init-symmetry (etree eproof)
  (unless (hash-table-p (eproof-symmetry eproof))
    (setf (eproof-symmetry eproof) (make-hash-table :test #'eq)))
  (setf (node-symmetry etree) 
    (make-symmetry-holder :class (list etree))))

(defun update-symmetry (oldnode newnode)
  (when (and oldnode newnode (not (eq oldnode newnode)))
    (let ((old-syms (or (node-symmetry oldnode)
			(make-symmetry-holder :class (list oldnode)))))
      (setf (symmetry-holder-class old-syms)
	(nsubst newnode oldnode (symmetry-holder-class old-syms)))
      (setf (node-symmetry newnode) old-syms)
      (let ((new-syms 
	     (remove-if #'dead-node-p
			(remove newnode
				(symmetry-holder-class old-syms)))))
      (if new-syms
	  (typecase newnode
	    ((or edisjunction econjunction implication negation skolem selection)
	     (let ((rsyms (remove-if-not #'rewrite-p new-syms))
                   (syms (remove-if-not 
			  #'(lambda (x)
			      (typecase newnode
				(edisjunction (edisjunction-p x))
				(econjunction (econjunction-p x))
				(implication (implication-p x))
				(negation (negation-p x))
				(skolem (skolem-p x))
				(selection (selection-p x))))
			  new-syms)))
;;;(partially) fix a bug in the higher-order case, where rewrite
;;;order could almost be random. So skipping over rewrite nodes
;;;is necessary, sometimes. Example: THM303
               (if (null syms)
                   (do ((hxrsyms (cdr rsyms) (cdr hxrsyms))
			(hxrsym (car rsyms) (car hxrsyms)))
                       ((null hxrsym))
                       (let ((subrsym (car (etree-components hxrsym))))
                          (if (rewrite-p subrsym)
                              (push subrsym hxrsyms)
                              (push subrsym syms)))))
	       (if syms
		   (dolist (sym syms)
		     (do ((newkids (etree-components newnode) (cdr newkids))
			  (oldkids (etree-components sym) (cdr oldkids)))
			 ((null newkids))
		       (add-to-syms (car newkids) (car oldkids))))
		 (dolist (kid (etree-components newnode))
		   (init-symmetry kid master-eproof)))))
	    (expansion
	     (dolist (sym new-syms)
	       (if (expansion-p sym) 
		   (add-to-syms (car (etree-components newnode))
				(car (etree-components sym))))))
	    (rewrite
	     (let ((syms (remove-if-not 
			  #'(lambda (x)
			      (and (rewrite-p x)
				   (eq (rewrite-justification x)
				       (rewrite-justification newnode))))
			  new-syms)))
	       (when (car (etree-components newnode))
		 (if syms 
		     (dolist (sym syms)
		       (add-to-syms (car (etree-components newnode))
				    (car (etree-components sym))))
;;;The following changes are used to handle option-sets.
		     (setf (node-symmetry (car (etree-components newnode))) 
                         (node-symmetry newnode)))))))
	(dolist (kid (etree-components newnode))
	  (init-symmetry kid master-eproof)))))))

(defun dead-node-p (node)
  (zerop (etree-status* node)))

(defun find-syms (node)
  (let ((sym (node-symmetry node)))
    (if sym
	(remove node
		(symmetry-holder-class sym)))))

(defun add-to-syms (newnode oldnode)
  (if (not (eq newnode oldnode))
      (let ((old-syms (or (node-symmetry oldnode)
			  (make-symmetry-holder :class (list oldnode)))))
	(when (and oldnode newnode)
	  (setf (symmetry-holder-class old-syms)
	    (pushnew newnode (symmetry-holder-class old-syms)))
	  (setf (node-symmetry newnode) old-syms)))
    (unless (node-symmetry newnode)
      (setf (node-symmetry newnode)
	(make-symmetry-holder :class (list newnode))))))

(defwffop deepen-to-literals
  (argtypes anything)
  (argnames gwff)
  (resulttype etree)
  (applicable-q (lambda (x) (or (gwff-q x)
				(and (symbolp x) (find-etree-node-name x)))))
  (mhelp "Iteratively deepen an expansion tree until all leaves are
literals."))

(defmateop dp*
  (mate-alias deepen-to-literals)
  (mate-result-> current-topnode)
  (matewff-argname gwff))

; cebrown
(defun dual-p (e)
  (and (rewrite-p e)
       (eq (rewrite-justification e) 'EQUIVWFFS)
       (let ((j (car (etree-components e))))
	 (and (etree-p j)
	      (or (econjunction-p j) (edisjunction-p j))
	      (let ((l (car (etree-components j))))
		(and (leaf-p l)
		     (wffeq-ab (rewrite-shallow e) (leaf-shallow l))))))))

;;; Fixed so that if gwff is a leaf, its replacement will be returned.
;;; DAN 22AUG88


(defun deepen-to-literals-main (gwff)
  (cond ((etree-q gwff)
	 (let* ((parent (etree-parent gwff))
		(n (if parent (position gwff (etree-components parent)))))
	   (mapc #'deepen-to-literals*
		 (find-etree-nodes #'(lambda (x) ; changed from leaf-p* to those leaves not first child of dual - cebrown 8/7/00
				       (and (leaf-p* x)
					    (if (etree-parent x)
						(if (etree-parent (etree-parent x))
						    (not (dual-p (etree-parent (etree-parent x))))
						  t)
					      t)))
				   gwff))
	   (if n 
	       (nth n (etree-components parent)) 
	       (eproof-etree current-eproof))))
	((symbolp gwff)
	 (let* ((node (find-etree-node-name gwff))
	        (parent (etree-parent node))
		(n (if parent (position node (etree-components parent)))))
	   (deepen-to-literals-main node)
	   (if n 
	       (nth n (etree-components parent)) 
	       (eproof-etree current-eproof))))
	(t (gwff-to-etree gwff skolem-default t))))

(defun deepen-to-literals (gwff)
  (when (and (eproof-lemmas current-eproof)
	     (eq gwff (eproof-etree current-eproof)))
    (setq gwff (cadr (etree-components (eproof-etree current-eproof))))) ; cebrown 10/20/01
  (when (and (lazy2-used) *hacked-rewrites-list*) (unfiddle-with-def-leaves (eproof-etree current-eproof)))
  (let ((result (deepen-to-literals-main gwff)))
    (relabel-etree (eproof-etree current-eproof))
    (when (and (lazy2-used) *hacked-rewrites-list*) (fiddle-with-def-leaves (eproof-etree current-eproof)))
    (setq *leibniz-var-list* (leibniz-quant *leibniz-var-list*) *after-primsub* nil)
    result))

(defun leibniz-quant (list)
  (let ((list1 (remove-if #'consp list))
	(list2 (remove-if-not #'consp list)))
    ;list2 has already been processed; list1 hasn't
  (remove-duplicates
   (append list2
	   (reduce #'append
		   (mapcar #'(lambda (x) 
		       (if (expansion-p (car (etree-components x)))
			   (mapcar #'(lambda (y) (cons (and (exp-var-p y) (exp-var-var y)) x))
				   ;above AND since it might have a primsub instead of a variable.
				   (expansion-terms (car (etree-components x))))
			 nil))
		   list1)))
   :key #'car))) 

(defun fiddle-with-def-leaves (etree)
  (when *hacked-rewrites-list*
	(setq *banned-conns-list* nil)
	(setq *ho-banned-conns-list* nil)
	(let ((leaves (remove-duplicates (append
					  (reduce 'append (mapcar #'(lambda (x) (find-etree-nodes 'leaf-p (car x)))
								  *hacked-rewrites-list*))
					  (find-etree-nodes 'leaf-p etree)))))
	  (dolist (leaf leaves)
		  (when (assoc (leaf-shallow leaf) *instantiated-defs-list*)
			(let ((real-gwff (cdr (assoc (leaf-shallow leaf) *instantiated-defs-list*)))
			      (associated-rewrite 
			       (car (remove-if-not #'(lambda (x) (eq (cadr x) (leaf-shallow leaf)))
						   *hacked-rewrites-list*))))
			  (setf *instantiated-defs-list* 
				(remove-if #'(lambda (x) (eq x (leaf-shallow leaf))) *instantiated-defs-list*))
			  (setf (cdr associated-rewrite)
				(cons (cadr associated-rewrite) leaf))
			  (setf (rewrite-justification (car associated-rewrite)) 'equivwffs)
			  (let ((rewrite-leaves (find-etree-nodes #'leaf-p* (cadr (etree-components (etree-parent leaf))))))
			    (dolist (bc rewrite-leaves)
				    (push (cons leaf bc) *banned-conns-list*))
			    (push (cons (leaf-name leaf) (mapcar 'leaf-name rewrite-leaves)) *ho-banned-conns-list*))
			  (setf (leaf-shallow leaf) real-gwff)))))
	(setq *unsubst-exp-vars* (remove-if-not 
				  #'(lambda (x) (and (exp-var-p x)
						     (eq (exp-var-var x) (exp-var-subst x))))
				  (mapcar 'car (eproof-free-vars-in-etree current-eproof))))
	(setq *rew-unsubst-exps* (remove-if-not #'
				  (lambda (x) (intersection (free-vars-of (leaf-shallow x))
							    (mapcar #'exp-var-var *unsubst-exp-vars*)))
				  (mapcar 'cddr *hacked-rewrites-list*)))
	(setq *rew-unsubst-exps* (mapcar 
				  #'(lambda (x) 
				      (cons (mapcar 'get-real-exp 
						    (intersection (free-vars-of (leaf-shallow x))
								  (mapcar #'exp-var-var *unsubst-exp-vars*)))
					    x)) *rew-unsubst-exps*))))

(defun get-real-exp (var)
  (car (remove-if-not #'(lambda (x) (eq var (exp-var-var x))) *unsubst-exp-vars*)))

(defun unfiddle-with-def-leaves2 (etree)
  (declare (ignore etree))
  (dolist (rew *hacked-rewrites-list*)
	  (when (and (cdr rew) (cddr rew))
		(let ((sym (cadr rew))
		      (leaf (cddr rew)))
		  (unless (assoc sym *instantiated-defs-list*)
			  (push (cons sym (leaf-shallow leaf)) *instantiated-defs-list*))
		  (setf (rewrite-justification (car rew)) sym)
		  (setf (leaf-shallow leaf) sym)))))

(defun unfiddle-with-def-leaves (etree)
  (if *after-primsub*
      (let* ((new-unsubst-exp-vars (mapcar 'car (remove-if-not 
						 #'(lambda (x) (and (exp-var-p (car x))
								    (eq (exp-var-var (car x)) (exp-var-subst (car x)))))
						 (eproof-free-vars-in-etree current-eproof))))
	     (substituted-for (setdiff *unsubst-exp-vars* new-unsubst-exp-vars)))
	(dolist (rew *hacked-rewrites-list*)
		(when (and (cdr rew) (cddr rew) 
			   (or (and (wffeq (cdr (assoc (cadr rew) *instantiated-defs-list*))
					   (leaf-shallow (cddr rew)))
				    (not (wffeq (lnorm (cdr (assoc (cadr rew) *instantiated-defs-list*)))
						(leaf-shallow (cddr rew)))))
			       (and (member (cddr rew) (mapcar 'cdr *rew-unsubst-exps*))
				    (intersection (car (rassoc (cddr rew) *rew-unsubst-exps*))
						  substituted-for))))
		      (setq *hacked-rewrites-list* 
			    (remove-if #'(lambda (x) (equal (car x) (car rew))) *hacked-rewrites-list*))
		      (setf (etree-components (car rew)) nil)
		      (deepen-to-literals-main (car rew)))))
    (dolist (rew *hacked-rewrites-list*)
	    (when (and (cdr rew) (cddr rew) 
		       (wffeq (cdr (assoc (cadr rew) *instantiated-defs-list*))
			      (leaf-shallow (cddr rew)))
		       (not (wffeq (lnorm (cdr (assoc (cadr rew) *instantiated-defs-list*)))
				   (leaf-shallow (cddr rew)))))
		  (setq *hacked-rewrites-list* (remove-if #'(lambda (x) (equal (car x) (car rew))) *hacked-rewrites-list*))
		  (setf (etree-components (car rew)) nil)
		  (deepen-to-literals-main (car rew)))))
  (unfiddle-with-def-leaves2 etree))
		
(defun expansion-above (node)
  (if (expansion-p node) node
    (if (etree-parent node) (expansion-above (etree-parent node))
      nil)))

(defun deepen-to-literals* (gwff)
  (let ((newnode (if (leaf-p* gwff) (deepen-leaf-node gwff) gwff)))
    (dolist (node (etree-components* newnode) newnode)
      (deepen-to-literals* node))))

(defun deepen-after-duplication (etree)
  (when (and (lazy2-used) *hacked-rewrites-list*) (unfiddle-with-def-leaves etree))
  (mapc #'deepen-to-literals*
	(remove-if-not #'(lambda (leaf) (expansion-p (etree-parent leaf)))
		       (eproof-leaf-list* current-eproof)))
  (when (and (lazy2-used) *hacked-rewrites-list*) (fiddle-with-def-leaves etree))
  (setq *leibniz-var-list* (leibniz-quant *leibniz-var-list*))
  etree)


(defun create-leaf-node (gwff positive free-vars parent)
  (let ((newnode
	 (make-leaf
	  :free-vars free-vars
	  :positive positive
	  :junctive nil
	  :parent parent
	  ;; lambda-norming causes problems when instantiating an equality
	  ;; creates new reducts 11APR89 DAN
	  :shallow ;(lambda-norm gwff)
	     gwff )))
    (push newnode (eproof-leaf-list master-eproof))
    newnode))

(defun create-edisjunction-node (gwff positive free-vars parent pred)
  (if (edisjunction-p pred)
      (progn
	(update-status nil
		       (car (etree-components pred))
		       (etree-status* pred))
	(update-status nil
		       (cadr (etree-components pred))
		       (etree-status* pred))
	pred)
      (let ((newnode
	      (make-edisjunction
		:positive positive
		:junctive (if positive 'dis 'con)
		:free-vars free-vars
		:parent parent
		:predecessor (etree-name pred))))
	(setf (etree-components newnode)
	      (list (create-leaf-node (glr gwff) positive  free-vars newnode)
		    (create-leaf-node (grr gwff) positive free-vars newnode)))
	(update-status pred newnode)
	(update-status nil
		       (car (etree-components newnode))
		       (etree-status* newnode))
	(update-status nil
		       (cadr (etree-components newnode))
		       (etree-status* newnode))
	newnode)))

(defun create-econjunction-node (gwff positive  free-vars parent pred)
  (if (econjunction-p pred)
      (progn
	(update-status nil
		       (car (etree-components pred))
		       (etree-status* pred))
	(update-status nil
		       (cadr (etree-components pred))
		       (etree-status* pred))
	pred)
      (let ((newnode
	      (make-econjunction 
		:positive positive
		:free-vars free-vars
		:junctive (if positive 'con 'dis)
		:parent parent
		:predecessor (etree-name pred))))
	(setf (etree-components newnode)
	      (list (create-leaf-node (glr gwff) positive free-vars newnode)
		    (create-leaf-node (grr gwff) positive free-vars newnode)))
	(update-status pred newnode)
	(update-status nil
		       (car (etree-components newnode))
		       (etree-status* newnode))
	(update-status nil
		       (cadr (etree-components newnode))
		       (etree-status* newnode))
	newnode)))

(defun create-implication-node (gwff positive free-vars parent pred)
  (if (implication-p pred)
      (progn
	(update-status nil
		       (car (etree-components pred))
		       (etree-status* pred))
	(update-status nil
		       (cadr (etree-components pred))
		       (etree-status* pred))
	pred)
      (let ((newnode
	      (make-implication
		:positive positive
		:junctive (if positive 'dis 'con)
		:free-vars free-vars
		:predecessor (etree-name pred)
		:parent parent)))
	(setf (etree-components newnode)
	      (list (create-leaf-node (glr gwff) 
				      (not positive) free-vars newnode)
		    (create-leaf-node (grr gwff) positive free-vars newnode)))
	(update-status pred  newnode)
	(update-status nil
		       (car (etree-components newnode))
		       (etree-status* newnode))
	(update-status nil
		       (cadr (etree-components newnode))
		       (etree-status* newnode))
	newnode)))

(defun create-negation-node (gwff positive free-vars parent pred)
  (if (negation-p pred)
      (progn
	(update-status nil (car (etree-components pred))
		       (etree-status* pred))
	pred)
      (let ((newnode
	      (make-negation 
		:positive positive
		:junctive 'neutral
		:free-vars free-vars
		:predecessor (etree-name pred)
		:parent parent)))
	(setf (etree-components newnode)
	      (list (create-leaf-node (gdr gwff) (not positive) free-vars
				      newnode)))
	(update-status pred newnode)
	(update-status nil (car (etree-components
				 newnode))
		       (etree-status* newnode))
	newnode)))

;;; Expansion node is initialized with one component, just the result
;;; of instantiating its quantifier with a new variable

(defun create-expansion-node (gwff positive free-vars parent pred)
  (if (expansion-p pred)
      (duplicate-var pred)
      (duplicate-var
	(let ((newnode
		(make-expansion
		     :positive positive
		     :junctive 'con
		     :shallow gwff
		     :free-vars free-vars
		     :predecessor (etree-name pred)
		     :parent parent)))
	  (update-status pred
			 newnode)
	  newnode))))



(defun create-skolem-node (gwff positive free-vars parent curnode)
  (if (skolem-p curnode)
      (progn
	(update-status nil (car (etree-components
				 curnode))
		       (etree-status* curnode))
	(push (cons (car (skolem-terms curnode))
		    (assoc (car (skolem-terms curnode))
			   (eproof-skolem-constants master-eproof)))
	      (eproof-skolem-constants current-eproof))
	curnode)
      (let* ((sk-vars  (cond ((eq (eproof-skolem-method current-eproof) 'sk1)
			      free-vars)
			     ((eq (eproof-skolem-method current-eproof) 'sk3)
			      (intersection (reduce 'append (mapcar 'free-vars-of free-vars))
					    (free-vars-of gwff)))))
	     (sk-vars (if (eq (eproof-skolem-method current-eproof) 'sk3)
			  (mapcar #'(lambda (x) 
				      (or (car (remove-if-not #'(lambda (y) (eq (exp-var-var y) x)) 
							      (mapcar 'car (eproof-free-vars-in-etree current-eproof))))
					  x)) sk-vars)
			sk-vars))
	     (bindvar (bindvar gwff))
	     (old-skolem 
               (find-if #'(lambda (x) 
                            (equal (get bindvar 'type) 
                                   (get (bindvar (get-shallow x)) 'type)))
;;;fix a widely spread bug in the higher-order case, where rewrite nodes
;;;may shadow skolem nodes, sometimes. So skipping over rewrite nodes
;;;is really necessary.
;;;Example: pred-ex1(after "prim-single pred-ex1sol" is done)
;;;EXample: x5310(after primsub is done.)
		  (let* ((nodes (find-syms curnode))
                         (snodes (remove-if-not #'skolem-p nodes)))
                     (or snodes 
                         (do ((hxrnodes (remove-if-not #'rewrite-p nodes) (cdr hxrnodes))
			      (hxrnode  (car (remove-if-not #'rewrite-p nodes)) (car hxrnodes)))
			     ((null hxrnode) (remove-if-not #'skolem-p snodes))
                           (let ((subnode (car (etree-components hxrnode))))
                             (if (rewrite-p subnode) 
                                 (push subnode hxrnodes)
                                 (push subnode snodes))))))))
	     (skolem-fn 
              (if old-skolem
                  (do* ((old-term (car (skolem-terms old-skolem)))
                        (sk-fn (if (skolem-term-p old-term)
                                   (head (skolem-term-term
                                          old-term))
                                   old-term))
                        (sk-vars sk-vars (cdr sk-vars)))
                      ((null sk-vars) sk-fn)
		      (setq sk-fn (cons sk-fn (car sk-vars))))
                  (make-skolem-fn bindvar sk-vars)))
	     (skolem-term-label
	       (if sk-vars
		   (make-skolem-term
		     :term skolem-fn
		     :parameter (make-skolem-fn bindvar nil))
                   (make-skolem-term
                    :term skolem-fn
                    :parameter skolem-fn)))
	     (newnode
	       (make-skolem
		 :positive positive
		 :junctive 'neutral
		 :shallow gwff
		 :free-vars free-vars
		 :parent parent
		 :predecessor (etree-name curnode)
		 :terms (list skolem-term-label) ;(list skolem-fn)
		 )))
	(setf (etree-components newnode)
	      (list (create-leaf-node 
		      (substitute-l-term-var skolem-term-label 
					     bindvar (gdr gwff))
		      positive free-vars newnode)))
	(update-status curnode newnode)
	(update-status nil (car (etree-components
				 newnode))
		       (etree-status* newnode))
	(push (cons			;*;(car (head-var-of skolem-fn))
		skolem-term-label (length sk-vars))
	      (eproof-skolem-constants current-eproof))
	newnode)))

(defun create-selection-node (gwff positive free-vars parent pred)
  (if (selection-p pred)
      (progn
	(update-status nil (car (etree-components pred)) 
		       (etree-status* pred))
	(push (cons (car (selection-terms pred)) 0)
	      (eproof-skolem-constants current-eproof))
	pred)
    (let* ((bindvar (bindvar gwff))
	   (sel-term* 
	    (make-skolem-fn bindvar nil))
	   (sel-term 
	    (make-skolem-term :term sel-term* :parameter sel-term*))
	   (newnode
	    (make-selection
	     :positive positive
	     :shallow gwff
	     :junctive 'neutral
	     :terms (list sel-term)
	     :parent parent)))
      (setf (etree-components newnode)
	    (list 
	     (create-leaf-node 
	      (substitute-l-term-var sel-term (bindvar gwff) (gdr gwff))
	      positive free-vars newnode)))
      (update-status pred newnode)
      (update-status nil (car (etree-components newnode))
		     (etree-status* newnode))
      (push (cons sel-term 0)
	    (eproof-skolem-constants current-eproof))
      newnode)))

(defun number-exps-above (node)
  (if (null node) 0
    (if (expansion-p node) (1+ (number-exps-above (etree-parent node)))
      (number-exps-above (etree-parent node)))))

(defun create-true-node (gwff positive free-vars parent pred)
  (declare (ignore gwff))
  (if (true-p pred)
      (progn
	(update-status nil pred)
	pred)
      (let ((newnode 
	      (make-true
		:positive positive
		:junctive (if positive 'con 'dis)
		:free-vars  free-vars
		:predecessor (etree-name pred)
		:parent parent
		:components nil)))
	(update-status pred newnode)
	newnode)))

(defun create-false-node (gwff positive free-vars parent pred)
  (declare (ignore gwff))
  (if (false-p pred)
      (progn
	(update-status nil pred)
	pred)
      (let ((newnode 
	      (make-false
		:positive positive
		:junctive (if positive 'dis 'con)
		:free-vars  free-vars
		:predecessor (etree-name pred)
		:parent parent
		:components nil)))
	(update-status pred newnode)
	newnode)))

(defun create-rewrite-node (gwff positive newwff justification free-vars
				 parent pred)
  (let ((kid nil))
  (cond ((and (rewrite-p pred)
	      justification (rewrite-justification pred)
	      (setq kid (find newwff (etree-components pred) 
		    :test #'wffeq-ab :key #'get-shallow)))
	 (update-status nil pred)
	 (update-status nil kid (etree-status* pred))
	 pred)
	((and (rewrite-p pred) (eq (rewrite-justification pred) justification)
	      justification)
	  (setq kid
		(create-leaf-node newwff positive free-vars pred))
	  (setf (etree-components pred)
		(nconc (etree-components pred) (list kid)))
	  (update-status nil pred)
	  (update-status nil kid (etree-status* pred))
	  pred)
	(t
	  (let ((newnode
		  (make-rewrite
		    :positive positive
		    :shallow gwff
		    :junctive 'neutral
		    :justification justification
		    :free-vars free-vars
		    :predecessor (etree-name pred)
		    :parent parent)))
	    (setq kid (create-leaf-node newwff positive free-vars newnode))
	    (setf (etree-components newnode)
		  (list kid))
	    (update-status pred newnode)
	    (update-status nil kid (etree-status* newnode))
	    newnode)))))



(defun get-deep (label) 
  (if (etree-q label)
      (apply-label label (get-deep label))
      (throwfail label " is not an etree.")))
      
(defun get-shallow (label)
  (if (etree-q label)
      (apply-label label (get-shallow label))
      (throwfail label " is not an etree.")))

(defun positive-p (label)
  (if (etree-q label)
      (etree-positive label)
      (throwfail "Positive-p can only be used on expansion trees.")))

(defun etree-junctive* (label)
  (if (leaf-p* label)
      nil
      (etree-junctive label)))

(defun etree-name* (etree)
  (if (leaf-p* etree) 
      (if (leaf-p etree) (etree-name etree) 
	  (or (etree-predecessor etree) (etree-name etree)))
      (etree-name etree)))

(defun eproof-leaf-list* (eproof)
  (if (eq eproof master-eproof)
      (eproof-leaf-list master-eproof)
      (find-etree-nodes #'leaf-p* (eproof-etree eproof))))

#|(defun free-vars-in-etree (eproof)
  (declare (ignore eproof))
  (mapcar #'car (eproof-free-vars-in-etree master-eproof)))|#

; note that this ignores its argument - we should stop passing it at all - cebrown
(defun free-vars-in-etree (eproof)
  (declare (ignore eproof))
  (remove-duplicates ; cebrown - removing duplicates, since evars occuring in multiple exp terms may be counted more than once
   (mapcar #'strip-exp-vars
	   (mapcar #'car (eproof-free-vars-in-etree master-eproof)))))
