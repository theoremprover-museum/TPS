;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-OPS2)

;;;
;;; File WFFSUB2
;;;
;;; substitution operations on wffs.
;;;

(part-of wff-ops2)

(deffile wffsub2
  (part-of wff-ops2)
  (extension clisp)
  (mhelp "Contains substitution commands for wffs without lambda binders."))

(context substitution)

(defwffop substitute-l-term-var
  (argtypes gwff gvar gwff)
  (wffargtypes "A" "A" "B")
  (resulttype gwff)
  (wffop-type "B")
  (wffop-typelist "A" "B")
  (argnames term var inwff)
  (arghelp "term" "var" "inwff")
  (mhelp
   "Substitute a term for the free occurrences of variable in a gwff.
Bound variables may be renamed, using the function in the global
variable REN-VAR-FN."))


(defun substitute-l-term-var (term var inwff)
  (or (subst-l-term-rec (intern-subst term var) var inwff) inwff))

(defwffrec subst-l-term-rec
  (argnames term var inwff)
  (mhelp "Recursive part of SUBSTITUTE-L-TERM-VAR."))


(defun subst-l-term-rec (term var inwff)
  (cond ((label-q inwff)
	 (apply-label inwff (subst-l-term-rec term var inwff)))
  	((lsymbol-q inwff) (if (eq var inwff) term nil))
	((boundwff-q inwff)
	 (cond ((eq (bdvar inwff) var) nil)
	       ((free-in (bdvar inwff) term)
		(if (free-in var (cdr inwff))
		    (subst-l-term-rec term var (rename-bd-var inwff)) nil))
	       (t (let ((new-wff (subst-l-term-rec term var (cdr inwff))))
		    (if new-wff (cons (car inwff) new-wff) nil)))))
	(t (let ((left (or (subst-l-term-rec term var (car inwff))
			   (car inwff)))
		 (right (or (subst-l-term-rec term var (cdr inwff))
			    (cdr inwff))))
	     ;; Returns nil unless one of the two parts of the wff
	     ;; has been changed by the substitution
	     (unless (and (eq left (car inwff)) (eq right (cdr inwff)))
		     (cons left right))))))

(defwffop do-primsub
  (argtypes gwff gvar gwff)
  (wffargtypes "A" "B" "B")
  (wffop-type "A")
  (wffop-typelist "A" "B")
  (resulttype gwff)
  (argnames inwff var sub)
  (arghelp "Wff to change" "Variable to substitute for" "Substitution to make")
  (mhelp "Replaces a variable with a primitive substitution.
Differs from SUBST in that it will also replace quantified
variables, and their quantifiers, as necessary."))

(defun do-primsub (inwff var sub)
  (substitute-l-term-var sub var inwff)
  ;above takes care of all the free occurrences of var
  (do-primsub-rec inwff var sub))

(defwffrec do-primsub-rec
  (argnames inwff var sub)
  (mhelp ""))

(defun do-primsub-rec (inwff var sub)
  (cond ((label-q inwff)
	 (apply-label inwff (do-primsub-rec inwff var sub)))
	((lsymbol-q inwff) (if (eq var inwff) sub inwff))
	((boundwff-q inwff)
	 (cond ((eq (bdvar inwff) var)
		(let ((bd (binder inwff))
		      (fv (free-vars-of sub)))
		  (setq inwff (delete-binder inwff))
		  (setq inwff (do-primsub-rec inwff var sub))
		  (dolist (v fv)
			  (setq inwff (mbed-quant bd v inwff)))
		  inwff))
	       (t (cons (car inwff) (do-primsub-rec (cdr inwff) var sub)))))
	(t (cons (do-primsub-rec (car inwff) var sub)
		 (do-primsub-rec (cdr inwff) var sub)))))



