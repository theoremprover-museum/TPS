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
;;; File WFFEQU1
;;;
;;; Contains some test for equality between wffs.
;;;

(deffile wffequ1
  (part-of wff-ops1)
  (extension clisp)
  (mhelp "Contains tests for equality between wffs."))

(context wffequal)

(defwffop wffeq-ab
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (resulttype boolean)
;  (multiple-recursion t)
  (mhelp "Tests for equality modulo alphabetic change of bound variables."))

(defun wffeq-ab (wff1 wff2) (wffeq-ab1 wff1 wff2 nil))

(defwffrec wffeq-ab1
  (argnames wff1 wff2 varstack)
;  (multiple-recursion t)
)
;;; This function is symmetric in wff1, wff2.  Can just reverse order
;;; of arguments if wff2 is a label.  No need for multiple-recursion
;;; in definition above.

;;;The above is not true as the label may be embedded in wff1 or wff2.

(defun wffeq-ab1 (wff1 wff2 varstack)
  (cond ((label-q wff1)
	 (apply-label wff1 (wffeq-ab11 wff1 wff2 varstack)))
	((label-q wff2)
	 (apply-label wff2 (wffeq-ab12 wff1 wff2 varstack)))
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
		(wffeq-ab1 (cdr wff1) (cdr wff2)
			   (acons (caar wff1) (caar wff2) varstack)))
	       (T nil)))
	((boundwff-q wff2) nil)
	(t (and (wffeq-ab1 (car wff1) (car wff2) varstack)
		(wffeq-ab1 (cdr wff1) (cdr wff2) varstack)))))

;*;(defun true-correspondence (var1 var2 varstack)
;*;  (do ((varstack varstack (cdr varstack)))
;*;      ((null varstack) (eq var1 var2))
;*;    (cond ((eq (caar varstack) var1)
;*;	   (return (eq (cdar varstack) var2)))
;*;	  ((eq (cdar varstack) var2) (return nil)))))

#+comment(defun true-correspondence (var1 var2 varstack)
  (dolist (pair varstack (eq var1 var2))
    (cond ((eq (car pair) var1)
	   (return (eq (cdr pair) var2)))
	  ((eq (cdr pair) var2) (return nil)))))

(defun true-correspondence (var1 var2 varstack)
  (dolist (pair varstack (eq var1 var2))
    (if (eq (car pair) var1)
        (return (eq (cdr pair) var2))
        (if (eq (cdr pair) var2) (return nil)))))

(defwffop wffeq
  (argtypes gwff gwff)
  (resulttype boolean)
  (argnames wff1 wff2)
  (arghelp "wff1" "wff2")
;  (multiple-recursion t)
  (mhelp "Check whether two wffs are the same."))

(defun wffeq (wff1 wff2)
  (or (eq wff1 wff2)
  (cond ((label-q wff1) (apply-label wff1 (wffeq wff1 wff2)))
	((label-q wff2) (apply-label wff2 (wffeq wff2 wff1)))
	((lsymbol-q wff1) (eq wff1 wff2))
	((lsymbol-q wff2) nil)
	((boundwff-q wff1)
	 (if (boundwff-q wff2) (and (wffeq (car wff1) (car wff2))
				    (wffeq (cdr wff1) (cdr wff2))) 
	     nil))
	((boundwff-q wff2) nil)
	(t (and (wffeq (car wff1) (car wff2))
		(wffeq (cdr wff1) (cdr wff2)))))))

(defwffop not-wffeq
  (argtypes gwff gwff)
  (resulttype boolean)
  (argnames wff1 wff2)
  (arghelp "wff1" "wff2")
  (multiple-recursion t)
  (mhelp "Check, whether two wffs are not the same."))

(defun not-wffeq (wff1 wff2)
  (not (wffeq wff1 wff2)))

(defun wffmember (wff1 list)
  (dolist (wff2 list nil) (if (wffeq wff1 wff2) (return t))))

