;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-EDITOR)

;;;
;;; File: EDSUB
;;;
;;; defines editor substitution operations.
;;;

(part-of wff-editor)

(deffile edsub
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Contains editor substitution operations."))

(context substitution)

(defwffop wff-identity
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "The identity function on gwff."))

(defun wff-identity (gwff) gwff)

(defedop ib
  (alias instantiate-binder)
  (result-> edwff)
  (edwff-argname bdwff))

(defwffop instantiate-binder
  (argtypes gwff gwff)
  (resulttype gwff)
  (argnames term bdwff)
  (arghelp "term" "bound wff")
  (applicable-p (lambda (term bdwff)
		  (and (ae-bd-wff-p bdwff) (type-equal (gar bdwff) term))))
  (mhelp
   "Instantiate a top-level universal or existential binder with a term."))

(defun instantiate-binder (term bdwff)
  (cond ((label-q bdwff)
	 (apply-label bdwff (instantiate-binder term bdwff)))
	((lsymbol-q bdwff)
	 (throwfail "Cannot instantiate " (bdwff . gwff)
		    ", a logical symbol."))
	((boundwff-q bdwff)
	 (cond ((ae-bd-wff-p bdwff)
		(substitute-l-term-var term (caar bdwff) (cdr bdwff)))
	       (t
		(throwfail "Instantiate only existential or universal quantifiers," t
			   "not " ((cdar bdwff) . fsym) "."))))
	(t (throwfail "Cannot instantiate an application."))))

(defedop subst
  (alias substitute-l-term-var)
  (result-> edwff)
  (edwff-argname inwff))

(defedop prim-subst
  (alias do-primsub)
  (result-> edwff)
  (mhelp "Replaces a variable with a primitive substitution.
Differs from SUBST in that it will also replace quantified
variables, and their quantifiers, as necessary.")
  (edwff-argname inwff))

(defedop ab
  (alias ab-change)
  (result-> edwff)
  (edwff-argname gwff))

(defedop sub   ;;previously \:
  (alias wff-identity)
  (result-> edwff)
  (mhelp "Replaces the current wff by the wff supplied."))

(defedop substyp
  (alias subst-1-type)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Substitutes a type for a type variable in edwff."))

(defedop rew-equiv
  (alias rewrite-all-equivalence)
  (edwff-argname gwff)
  (result-> edwff))
