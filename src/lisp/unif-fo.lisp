;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;(part-of mating-search)
(part-of ms88)

(deffile unif-fo
;  (part-of mating-search)
  (part-of ms88)
  (mhelp "First-order unification."))

(defun var-occurs-in-terms-p (var terms subst-stack)
  (dolist (term terms nil)
    (when (var-occurs-in-term-p var term subst-stack)
      (return t))))

(defun var-occurs-in-term-p (var term subst-stack)
  (cond ((label-q term) (apply-label term (var-occurs-in-term-p
					    var term subst-stack)))
	((lsymbol-q term)
	 (let ((newterm (cdr (assoc term subst-stack :test #'eq))))
	   (if newterm (var-occurs-in-term-p var newterm subst-stack)
	       (eq term var))))
	(t (or (var-occurs-in-term-p var (cdr term) subst-stack)
	       (var-occurs-in-term-p var (car term) subst-stack)))))

(defun make-first-order-term (head args)
  (dolist (arg args head)
    (setq head (cons head arg))))

(defun fo-unify-conn (connection free-vars)
  (multiple-value-bind (head1 args1 pos1)
      (fo-hnf (not (jform-pos (car connection)))
	      (jform-represents (car connection)) nil nil)
    (multiple-value-bind (head2 args2 pos2)
	(fo-hnf (jform-pos (cdr connection))
		(jform-represents (cdr connection)) nil nil)
      (if (and (eq pos1 pos2) (eq head1 head2))
	  (fo-unify (pairlis args1 args2) nil free-vars)
	  'fail))))

(defun fo-unify (dpairs subst-stack free-vars)
  (runcount 'unification)
  (multiple-value-bind (a b)
		       (fo-unify-real dpairs subst-stack free-vars)
		       (breakcount 'unification)
		       (values a b)))

(defun fo-unify-real (dpairs subst-stack free-vars)
  (do ((dpairs (cdr dpairs) (cdr dpairs))
       (pair (car dpairs) (car dpairs))
       (new-dpairs nil)
       (fail nil))
      ((or (null pair) fail)
       (if fail 'fail (if (and (null skolem-default) (ms88-cyclic-selection-terms subst-stack))
			  (values 'fail nil)
			  (values 'success (nconc subst-stack new-dpairs)))))
    (multiple-value-bind (head1 args1)
	(fo-hnf nil (car pair) nil subst-stack)
      (multiple-value-bind (head2 args2)
	  (fo-hnf nil (cdr pair) nil subst-stack)
	(if (eq head1 head2)
	    (setq dpairs (pairlis args1 args2 dpairs))
	    (if (and args1 args2) (setq fail t)
		(if occurs-check
		    (let ((free1 (member head1 free-vars :test #'eq))
			  (free2 (member head2 free-vars :test #'eq)))
		      (when (and (not free1) free2)
			(psetq head1 head2 head2 head1)
			(setq args2 args1))
		      (if (or free1 free2)
			  (if (var-occurs-in-terms-p
				head1 args2 subst-stack)
			      (setq fail t)
			      (push
				(if (eq head1 (car pair)) pair
				    (cons head1 (make-first-order-term
						  head2 args2)))
				subst-stack))
			  (setq fail t)))
		    (if (or args1 args2)
			(push (cons (make-first-order-term head1 args1)
				    (make-first-order-term head2 args2))
			      new-dpairs)
			(push (cons head1 head2) subst-stack)))))))))

(defun fo-hnf (pos term args subst-stack)
  (cond ((label-q term)
	 (apply-label term (fo-hnf pos term args subst-stack)))
	((not-p term)
	 (fo-hnf (not pos) (cdr term) args subst-stack))
	((lsymbol-q term)
	 (let ((newterm (cdr (assoc term subst-stack))))
	   (if newterm
	       (fo-hnf pos newterm args subst-stack)
	       (values term args pos))))
	(t (fo-hnf pos (car term) (cons (cdr term) args) subst-stack))))

