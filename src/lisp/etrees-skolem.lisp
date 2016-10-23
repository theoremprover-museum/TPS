;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of mating) ;expansion-tree
(context expansion-trees)

(deffile etrees-skolem
  (part-of mating) ;expansion-tree
  (extension lisp)
  (mhelp "Contains code concerned with skolem terms and skolemizing
in etrees."))

(eval-when (compile load eval)
(defflag show-skolem
  (flagtype boolean)
  (default nil)
  (mhelp "When true, skolem terms are shown when a wff containing them
is printed, otherwise a parameter is printed instead."))
)

(eval-when (load compile eval)
(defflavor skolem-term
  (mhelp "A skolem-term label contains both a skolem term, which is a
skolem function applied to some free variables (if any), and a parameter,
which is a new constant.  Skolem-terms may be printed in either of the
two ways: the flag SHOW-SKOLEM controls how they are printed.")
  (structured t)
  (printfn print-skolem-term)
  (instance-attributes
   term
   parameter))
)

(defflavor skolem-term
  (printwff (lambda (wff bracket depth)
	      (printwff
	       (if show-skolem
		   (skolem-term-term wff) 
		   (skolem-term-parameter wff))
	       bracket depth)))
  (prt-symbol-p (lambda (gwff)
                  (or (not show-skolem)
                      (prt-symbol-p (skolem-term-term
                                     gwff)))))
  (prt-infix-op (lambda (gwff) (declare (ignore gwff)) nil))
  (prt-prefix-op (lambda (gwff) (declare (ignore gwff)) nil))
  (prt-associative-p (lambda (gwff) (declare (ignore gwff)) nil))
  ;; Misc. required operations follow
  (type (lambda (gwff) (type (skolem-term-parameter gwff))))
  (gwff-p (lambda (gwff)  (declare (ignore gwff)) T))
  (gwff-q (lambda (gwff)  (declare (ignore gwff)) T))
  (contains-defn-not-equiv (lambda (gwff) (declare (ignore gwff)) nil))
  (contains-defn (lambda (gwff) (declare (ignore gwff)) nil))
  (contains-some-defn (lambda (gwff) (declare (ignore gwff)) nil))
  (contains-defn-1 (lambda (gwff) (declare (ignore gwff)) nil))
  (get-all-defns-rec (lambda (gwff) (declare (ignore gwff)) nil))
  (legal-type-p1 (lambda (gwff)  (declare (ignore gwff)) T))
  (infix-p (lambda (label)  (declare (ignore label)) nil))
  (infix-op-p (lambda (label)  (declare (ignore label)) nil))
  (boundwff-p (lambda (label)  (declare (ignore label)) nil))
  (lambda-bd-p (lambda (label) (declare (ignore label)) nil))
  ;; The next gives the default for test operations.
  ;; Stepping operations follow
  (gar (lambda (gwff) (gar (skolem-term-term gwff))))
  (gdr (lambda (gwff) (gdr (skolem-term-term gwff))))
;;;The fix is motivated by thm140. While calling function
;;;subst-vars-for-params, some skolem-terms get replaced
;;;by the corresponding skolem parameters. This brings us
;;;a very obscure bug. 
  #+comment (free-in (lambda (var term) 
	                 (free-in var (skolem-term-term term))))
  (free-in (lambda (var term) 
	     (or (free-in var (skolem-term-term term))
                 (eq var (skolem-term-parameter term)))))
  #+comment (free-vars (lambda (gwff bindlist)
	       (free-vars (skolem-term-term gwff)
			  (cons (car (skolem-term-term gwff))
                                bindlist))))
  (free-vars (lambda (gwff bindlist)
	       (free-vars (skolem-term-term gwff)
			   bindlist)))
  (symbols (lambda (gwff bindlist)
	     (symbols (skolem-term-term gwff)
		      bindlist)))
  (contains-= (lambda (gwff) (declare (ignore gwff)) nil))
  (contains-ext= (lambda (gwff) (declare (ignore gwff)) nil))
  (core::contains-equality  (lambda (gwff complex) (declare (ignore gwff complex)) nil))
  (bdvar-free-in-term
    (lambda (binder-elt term stack)
      (bdvar-free-in-term binder-elt (skolem-term-term term) stack)))
  (lambda-norm (lambda (gwff)
		 (let ((newterm (lambda-norm (skolem-term-term gwff))))
		   (if newterm
		       (make-skolem-term :term newterm 
					 :parameter (skolem-term-parameter gwff))
		       gwff))
		 ))
  (lcontr (lambda (gwff) 
		 (let ((newterm (lcontr (skolem-term-term gwff))))
		   (if newterm
		       (make-skolem-term :term newterm 
					 :parameter (skolem-term-parameter gwff))
		       gwff))))
  (etanorm (lambda (gwff)
		 (let ((newterm (etanorm (skolem-term-term gwff))))
		   (if newterm
		       (make-skolem-term :term newterm 
					 :parameter (skolem-term-parameter gwff))
		       gwff))))
  (mqs-applicable (lambda (gwff) (mqs-applicable (skolem-term-term gwff))))
  (min-quant-scope (lambda (gwff) 
			    (let ((newgwff (skolem-term-term gwff)))
			      (if (mqs-applicable newgwff)
				  (setf (skolem-term-term gwff) (min-quant-scope newgwff))
				  #+comment
				  (make-skolem-term 
				    :term (min-quant-scope newgwff)
				    :parameter (skolem-term-parameter gwff))
				  gwff))))
  (ab-normalize-main (lambda (gwff) (declare (ignore gwff)) nil))
  (expand-all-equalities (lambda (gwff) (declare (ignore gwff)) nil))
  (instantiate-=  (lambda (gwff) (declare (ignore gwff)) nil))
  (expand-top= (lambda (gwff) (declare (ignore gwff)) nil))
  (intern-subst (lambda (gwff var) (declare (ignore var)) gwff))
  (core::subst-l-term-rec 
   (lambda (term var gwff)
     (let ((newwff (core::subst-l-term-rec term var (skolem-term-term gwff)))
	   (newparam (core::subst-l-term-rec term var (skolem-term-parameter gwff))))
       (when (or newwff newparam)
	 (let ((new-label (copy-skolem-term gwff)))
	   (setf (skolem-term-term new-label)
		 (or newwff (skolem-term-term new-label)))
	   (setf (skolem-term-parameter new-label)
		 (or newparam (skolem-term-parameter new-label)))
	   new-label)))))
  (core::subst-term-var-rec
   (lambda (term var gwff)
     (let ((newwff (core::subst-term-var-rec term var (skolem-term-term gwff)))
	   (newparam (core::subst-term-var-rec term var (skolem-term-parameter gwff))))
       (when (or newwff newparam)
	 (let ((new-label (copy-skolem-term gwff)))
	   (setf (skolem-term-term new-label)
		 (or newwff (skolem-term-term new-label)))
	   (setf (skolem-term-parameter new-label)
		 (or newparam (skolem-term-parameter new-label)))
	   new-label)))))
  (free-for (lambda (term x label) (declare (ignore term x label)) t))
  (simul-substitute-term-var-rec
   (lambda (theta label)
     (let ((newwff (simul-substitute-term-var-rec
		     theta (skolem-term-term label)))
	   (newparam
	    (simul-substitute-term-var-rec
		     theta (skolem-term-parameter label))))
       (when (or newwff newparam)
	   (let ((new-label (copy-skolem-term label)))
	     (setf (skolem-term-term new-label)
		   (or newwff (skolem-term-term new-label)))
	     (setf (skolem-term-parameter new-label)
		   (or newparam (skolem-term-parameter new-label)))
	     new-label)))))
  (simul-substitute-term-var-rec-etree
   (lambda (theta label)
     (or (cdr (assoc label theta :test #'sk-term-=))
	 (let ((newwff (simul-substitute-term-var-rec
			theta (skolem-term-term label)))
	       (newparam
		(simul-substitute-term-var-rec
		 theta (skolem-term-parameter label))))
	   (when (or newwff newparam)
	     (let ((new-label (copy-skolem-term label)))
	       (setf (skolem-term-term new-label)
		     (or newwff (skolem-term-term new-label)))
	       (setf (skolem-term-parameter new-label)
		     (or newparam (skolem-term-parameter new-label)))
	       new-label))))))
  ;; A skolem-term label will not contain any quantifiers
  (skolemize-normal-wff 
   (lambda (positive gwff bdvars skolem-terms)
     (declare (ignore positive bdvars))
     (values gwff skolem-terms)))
  (instantiate-definitions
   (lambda (inwff chkfn chkarg) (declare (ignore chkfn chkarg)) inwff))
;;Unification
  (hnf-rec
   (lambda (pos term args binder bdvars stack subst-stack)
     (hnf-rec pos (skolem-term-term term) args binder bdvars stack
	      subst-stack)))
  (fo-hnf
    (lambda (pos term args subst-stack)
      (fo-hnf pos (skolem-term-term term) args subst-stack)))
  (free-in-var-term
   (lambda (var term bdvars stack subst-stack rigid-path free-vars)
     (free-in-var-term var (skolem-term-term term) bdvars stack subst-stack
		       rigid-path free-vars)))
  (var-occurs-in-term-p
    (lambda (var term subst-stack)
      (var-occurs-in-term-p var (skolem-term-term term) subst-stack)))
;;new-ms
  (remove-sk-labels-wff
   (lambda (label) (remove-sk-labels-wff (skolem-term-term label))))
;;
  (core::hvars-rec
   (lambda (term) (core::hvars-rec (skolem-term-term term))))
  (core::head
    (lambda (term) (core::head (skolem-term-term term))))
  (wffeq 
   (lambda (wff1 wff2) (wffeq (skolem-term-term wff1) wff2)))
  (core::wffeq-ab11
   (lambda (wff1 wff2 stack)
     (core::wffeq-ab1 (skolem-term-term wff1) wff2 stack)))
  (core::wffeq-ab12
   (lambda (wff1 wff2 stack)
     (core::wffeq-ab1 wff1 (skolem-term-term wff2) stack)))
  (substitutable-vars-of 
   (lambda (label)
     (substitutable-vars-of (skolem-term-term label))))
  (and-p (lambda (gwff) (declare (ignore gwff)) nil))
  (or-p (lambda (gwff) (declare (ignore gwff)) nil))
  (implies-p (lambda (gwff) (declare (ignore gwff)) nil))
  (not-p (lambda (gwff) (declare (ignore gwff)) nil))
  (ae-bd-wff-p (lambda (gwff) (declare (ignore gwff)) nil))
  (a-bd-wff-p (lambda (gwff) (declare (ignore gwff)) nil))
  (e-bd-wff-p (lambda (gwff) (declare (ignore gwff)) nil))
  (equality-p (lambda (gwff) (declare (ignore gwff)) nil))
  (reduct-p (lambda (gwff) (declare (ignore gwff)) nil))
  (equiv-p (lambda (gwff) (declare (ignore gwff)) nil))
  (replace-pi-and-sigma-in-wff-rec 
     (lambda (x)
       (let* ((term (skolem-term-term x))
	      (newterm (replace-pi-and-sigma-in-wff-rec term)))
	 (when newterm
	   (progn (setf (skolem-term-term x) newterm) x)))))
  (wff-applic-p (lambda (x) (wff-applic-p (skolem-term-term x))))
  (dissolve-weak* (lambda (x) (dissolve-weak* (skolem-term-term x))))
)

(defun print-skolem-term (structure foo bar)
  (declare (ignore foo bar))
  (msg "[SK-TERM: " ((skolem-term-term structure) . gwff) "-"
	((skolem-term-parameter structure) . gwff) "]"))

(defun sk-term-= (x y)
  (if (skolem-term-p x)
      (and (skolem-term-p y) (eq (skolem-term-parameter x)
				 (skolem-term-parameter y)))
      (eq x y)))
