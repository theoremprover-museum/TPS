;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :AUTO)
(part-of ms90-3)

(deffile ms90-3-unif-fo
  (part-of ms90-3)
  (mhelp "First-order unification functions needed in the
implementation of Path-focused duplication."))

(defun pfd-occurs-in-term-p (var term env subst-stack)
  (if (lsymbol-q term)
      (let* ((varname (assoc term env));;term may be a constant.
	     (subst (cdr (assoc varname subst-stack))))	 
	(if subst (pfd-occurs-in-term-p var (car subst) (cdr subst)
					    subst-stack)
	    (eq varname var)))
      (or (pfd-occurs-in-term-p var (cdr term) env subst-stack)
	  (pfd-occurs-in-term-p var (car term) env subst-stack))))

;;;returns (values subst-stack unifiablep). However, if the pair of
;;;literals failed to unify because of occurs check, this will return
;;;(values t nil).

(defun pfd-fo-unify-conn (first env1 second env2)
  (let ((neg1 (not-p (jform-represents first)))
	(neg2 (not-p (jform-represents second))))
    (when (eq (if neg1 (jform-pos first) (not (jform-pos first)))
	      (if neg2 (not (jform-pos second)) (jform-pos second)))
      (pfd-fo-unify-rec
       (if neg1 (cdr (jform-represents first)) (jform-represents first))
       env1
       (if neg2 (cdr (jform-represents second)) (jform-represents second))
       env2 nil nil nil))))

(defun pfd-fo-unify (dpairs conns subst-stack)
  (runcount 'unification)
  (multiple-value-bind (a b)
		       (pfd-fo-unify-real dpairs conns subst-stack)
		       (breakcount 'unification)
		       (values a b)))

(defun pfd-fo-unify-real (dpairs conns subst-stack)
  (if dpairs
      (pfd-fo-unify-rec-real
       (caaar dpairs) (cdaar dpairs) (cadar dpairs) (cddar dpairs) (cdr dpairs)
       conns subst-stack)
      (if conns (pfd-fo-unify-rec1-real (caar conns) (cadar conns) (cddar conns)
				   (cdr conns) subst-stack)
	(if (and (null skolem-default) (cyclic-selection-terms subst-stack))
	    (values t nil)
	  (values subst-stack t)))))

(defun pfd-fo-unify-rec (term1 env1 term2 env2 dpairs conns subst-stack)
  (runcount 'unification)
  (multiple-value-bind (a b)
		       (pfd-fo-unify-rec-real term1 env1 term2 env2 dpairs conns subst-stack)
		       (breakcount 'unification)
		       (values a b)))

(defun pfd-fo-unify-rec-real (term1 env1 term2 env2 dpairs conns subst-stack)
  (if (lsymbol-q term1)
      (let ((var (assoc term1 env1)))
	(if var
	    (let ((subst (cdr (assoc var subst-stack))))
	      (if subst
		  (pfd-fo-unify-rec-real (car subst) (cdr subst) term2 env2
				    dpairs conns subst-stack)
		  ;;VAR is a variable. Check if it occurs free in term2.
		  (if (pfd-occurs-in-term-p var term2 env2 subst-stack)
		      ;;Does TERM2 reduce to VAR?
		      (if (simplify-term-p var term2 env2 subst-stack)
			  (pfd-fo-unify-real dpairs conns subst-stack)
			(values t nil))
		    (pfd-fo-unify-real
		       dpairs conns
		       (acons var (cons term2 env2) subst-stack)))))
	    ;;TERM1 is a constant.
	    (if (lsymbol-q term2)
		(let ((var (assoc term2 env2)))
		  (if var
		      (let ((subst (cdr (assoc var subst-stack))))
			(if subst
			    (pfd-fo-unify-rec-real (car subst) (cdr subst) term1
					      nil dpairs conns subst-stack)
			    ;;VAR is a variable.
			    (pfd-fo-unify-real
			     dpairs conns (acons var (cons term1 nil)
						 subst-stack))))
		      ;;TERM2 is a constant.
		      (if (eq term1 term2)
			  (pfd-fo-unify-real dpairs conns subst-stack)
			  nil)))
		nil)))
      (if (lsymbol-q term2)
	  (progn (psetq term2 term1 term1 term2 env2 env1 env1 env2)
		 (pfd-fo-unify-rec-real term1 env1 term2 env2 dpairs conns subst-stack))
	(pfd-fo-unify-rec-real
	 (car term1) env1 (car term2) env2
	 (acons (cons (cdr term1) env1) (cons (cdr term2) env2) dpairs)
	 conns subst-stack))))

(defun pfd-fo-unify-rec1 (var term2 env2 conns subst-stack)
  (runcount 'unification)
  (multiple-value-bind (a b)
		       (pfd-fo-unify-rec1-real var term2 env2 conns subst-stack)
		       (breakcount 'unification)
		       (values a b)))

(defun pfd-fo-unify-rec1-real (var term2 env2 conns subst-stack)
  (let ((subst (cdr (assoc var subst-stack))))
    (if subst
	(pfd-fo-unify-rec-real (car subst) (cdr subst) term2 env2 nil conns
			  subst-stack)
	(if (pfd-occurs-in-term-p var term2 env2 subst-stack)
	    ;;Does TERM2 reduce to VAR?
	    (if (simplify-term-p var term2 env2 subst-stack)
		(pfd-fo-unify-real nil conns subst-stack) nil)
	    (pfd-fo-unify-real
	     nil conns (acons var (cons term2 env2) subst-stack))))))

(defun simplify-term-p (term1 term env subst-stack)
  (if (lsymbol-q term)
      (let ((var (assoc term env)))
	(if var
	    (let ((subst (cdr (assoc var subst-stack))))
	      (if subst
		  (simplify-term-p term1 (car subst) (cdr subst) subst-stack)
		  (eq var term1)))
	    nil))
      nil))
