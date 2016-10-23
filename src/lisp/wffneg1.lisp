;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-OPS1)

;;;
;;; File WFFNEG1
;;;
;;; operations to change the scope of negations.
;;;

(deffile wffneg1
  (part-of wff-ops1)
  (extension clisp)
  (mhelp "Contains operations changing scope of negations."))

(context neg-ops)

(defwffop pull-negation
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Pulls negations out one level."))

(defun pull-negation (gwff)
  (cond ((label-q gwff) (apply-label gwff (pull-negation gwff)))
	((lsymbol-q gwff) gwff)
	((boundwff-q gwff)
	 (case (binder gwff)
	   (forall (cons 'not (acons (caar gwff) 'exists
				     (negwff (cdr gwff)))))
	   (exists (cons 'not (acons (caar gwff) 'forall
				     (negwff (cdr gwff)))))
	   (t gwff)))
	((infix-p gwff)
	 (case (caar gwff)
	   (and (cons 'not (acons 'or (negwff (cdar gwff))
				  (negwff (cdr gwff)))))
	   (or (cons 'not (acons 'and (negwff (cdar gwff))
				 (negwff (cdr gwff)))))
	   (implies (cons 'not (acons 'and (cdar gwff)
				      (negwff (cdr gwff)))))
	   (t gwff)))
	(t (cons 'not (cons 'not gwff)))))

(defwffop negwff
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (mhelp "Negates current wff, erasing double negations."))

(defun negwff (gwff)
  (if (not-p gwff) (gdr gwff) (cons 'not gwff)))

(defwffop push-negation
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Negated Wff")
  (applicable-p not-p)
  (mhelp "Pushes negation through the outermost operator or quantifier."))

(defun push-negation (gwff)
  (let ((intwff (cdr gwff)))
    (cond ((label-q intwff) (apply-label gwff (push-negation gwff)))
          ((eq intwff 'TRUTH) 'FALSEHOOD)
          ((eq intwff 'FALSEHOOD) 'TRUTH)
	  ((lsymbol-q intwff) gwff)
	  ((boundwff-q intwff) 
	   (case (cdar intwff)
	     (forall (acons (caar intwff) 'exists (negwff (cdr intwff))))
	     (exists (acons (caar intwff) 'forall (negwff (cdr intwff))))
	     (t gwff)))
	  ((infix-p intwff)
	   (case (caar intwff)
	     (or (acons 'and (negwff (cdar intwff)) (negwff (cdr intwff))))
	     (and (acons 'or (negwff (cdar intwff)) (negwff (cdr intwff))))
	     (implies (acons 'and (cdar intwff) (negwff (cdr intwff))))
	     (equiv (acons 'or (acons 'and (cdar intwff) (negwff (cdr intwff)))
			   (acons 'and (negwff (cdar intwff)) (cdr intwff))))
	     (t gwff)))
	  ((eq (car intwff) 'not) (cdr intwff))
	  (t gwff))))

(defwffop neg-norm
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Return the negation normal form of the given wff."))

(defun neg-norm (gwff)
  (neg-norm-rec gwff t))

(defun neg-norm-rec (gwff pos)
  (cond ((label-q gwff) (apply-label gwff (neg-norm-rec gwff pos)))
	((eq gwff 'TRUTH) (if pos gwff 'FALSEHOOD)) ; cebrown 9/4/01
	((eq gwff 'FALSEHOOD) (if pos gwff 'TRUTH)) ; cebrown 9/4/01
	((lsymbol-q gwff) (if pos gwff (cons 'not gwff)))
	((not-p gwff)
	 (neg-norm-rec (cdr gwff) (not pos)))
	((ae-bd-wff-p gwff)
	 (let ((scope (neg-norm-rec (cdr gwff) pos)))
	   (if pos (cons (car gwff) scope)
	       (if (a-bd-wff-p gwff)
		   (acons (bdvar gwff) 'exists scope)
		   (acons (bdvar gwff) 'forall scope)))))
	((or (and-p gwff) (or-p gwff))
	 (let ((left (neg-norm-rec (cdar gwff) pos))
	       (right (neg-norm-rec (cdr gwff) pos)))
	   (acons (if pos (caar gwff) (if (eq (caar gwff) 'and) 'or 'and))
		  left right)))
	((implies-p gwff)
	 (let ((left (neg-norm-rec (cdar gwff) (not pos)))
	       (right (neg-norm-rec (cdr gwff) pos)))
	   (acons (if pos 'or 'and) left right)))
	(t (if pos gwff (cons 'not gwff)))))
