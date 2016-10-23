;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WEAK-LABEL)

;;;
;;; File: WEAK
;;; Package: WEAK-LABEL
;;;
;;; Defines the WEAK flavor of labels.

(part-of weak-label)

(deffile weak
  (part-of weak-label)
  (extension lsp)
  (mhelp "Defines the WEAK label for wffs."))

(context weak-labels)

(defgwff-type weak-type
  (checkfn weak-ckfn)
  (getfn weak-getfn)
  (mhelp "weak label : the wff represented by a weak label."))

(defun weak-ckfn (xxx)
  (and (symbolp xxx) (get xxx 'flavor) (eq (get xxx 'flavor) 'weak)))

(defun weak-getfn (xxx)
  (get-weak xxx))

(defun get-weak (label) (get label 'represents))
(defun put-weak (label gwff) (putprop label gwff 'represents))

(defwffop create-weak
  (argtypes symbol gwff)
  (argnames label gwff)
  (applicable-p (lambda (label gwff)
		  (declare (ignore gwff))
		  (if (label-q label)
		      (progn (complain label " already represents "
				       ((get-weak label) . gwff))
			     nil)
		      t)))
  (resulttype gwff)
  (mhelp "Assigns a label to the edwff, but does not change the edwff. You can
use the label to refer to this wff later."))

(defedop cw
  (alias create-weak)
  (result-> ignore)
  (edwff-argname gwff))

(defun create-weak (label gwff)
  (putprop label 'weak 'flavor)
  (put-weak label gwff)
  label)

(defun redef-weak (label gwff)
  (put-weak label gwff))

(defwffop redef-weak
  (argtypes symbol gwff)
  (argnames label gwff)
  (resulttype gwff)
  (applicable-q (lambda (x y) (declare (ignore y)) (get-weak x)))
  (mhelp "Makes current edwff the new value of label (which must 
already exist)."))

(defedop rw
  (alias redef-weak)
  (result-> edwff)
  (edwff-argname gwff))

(defwffop delete-weak
  (argtypes symbol gwff)
  (argnames label gwff)
  (resulttype gwff)
  (applicable-q (lambda (label gwff) 
		  (declare (ignore gwff))
		  (and (symbolp label) (eq (get label 'flavor) 'weak))))
  (mhelp "Replace a weak label by the wff it represents."))

(defun delete-weak (label gwff)
  (cond ((label-q gwff) (apply-label gwff (delete-weak label gwff)))
	((lsymbol-q gwff) gwff)
	(t (cons (delete-weak label (car gwff))
		 (delete-weak label (cdr gwff))))))

(defedop delweak
  (alias delete-weak)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Replaces all occurrences of the label with the wff it represents
in the current wff."))


;*;(defun test-form (x testfn y z) (cond ((funcall testfn x y) z) (t x)))

(defun recover-place (cmdlis)
  (declare (special wffstack cmdstack edwff))
  (move-all-up)
  (setq wffstack (list edwff))
  (recover-place-rec cmdlis)
  (setq cmdstack cmdlis))

(defun recover-place-rec (cmdlis)
  (declare (special wffstack edwff))
  (cond ((null cmdlis) nil)
	((consp cmdlis) (recover-place-rec (cdr cmdlis)) 
			(recover-place-rec (car cmdlis)))
	(t (setq edwff (funcall (extract-gop cmdlis) edwff))
	   (setq wffstack (cons edwff wffstack)))))

(defun extract-gop  (repl-op)
  (get repl-op 'replaces))

;; Old definition of extract-gop
;;(defun extract-gop (repl-op)
;;  (readlist (list 'g (getchar repl-op 10) 'r)))
;;
;;e.g. the 10th char of replace-gar is "a".

(defwffop dissolve-weak
  (argtypes gwff)
  (argnames gwff)
  (resulttype gwff)
  (applicable-q (lambda (gwff) (and (symbolp gwff) (eq (get gwff 'flavor) 'weak))))
  (mhelp "Replace a top level occurrence of the label by the wff it represents."))

(defedop dw
  (alias dissolve-weak)
  (result-> edwff)
  (edwff-argname gwff))

(defun dissolve-weak (gwff)
  (cond ((and (symbolp gwff) (eq (get gwff 'flavor) 'weak))
	 (get-weak gwff))
	(t (throwfail gwff " is not a weak label."))))

(defedop dw*
  (alias dissolve-weak*)
  (result-> edwff)
  (edwff-argname gwff))

(defun dissolve-weak* (gwff)
  (cond ((label-q gwff)
	 (cond ((and (symbolp gwff) (eq (get gwff 'flavor) 'weak))
		(dissolve-weak* (get-weak gwff)))
	       (T (apply-label gwff (dissolve-weak* gwff)))))
	((lsymbol-q gwff) gwff)
	((boundwff-p gwff)
	 (cons (car gwff) (dissolve-weak* (cdr gwff))))
	(T (cons (dissolve-weak* (car gwff))
		 (dissolve-weak* (cdr gwff))))))

(defwffop dissolve-weak*
  (argtypes gwff)
  (argnames gwff)
  (resulttype gwff)
  (mhelp "Replace all labels in a wff by the wffs represented by them."))

(defedop name
  (alias create-weak)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Assign a label to the edwff, and replace the edwff with this label."))
