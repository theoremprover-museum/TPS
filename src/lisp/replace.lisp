;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of REPLACE)

(deffile replace
  (part-of replace)
  (extension lisp)
  (mhelp "Functions for replacing one symbol or wff with another."))

;;; repsymbol is a replaceable symbol.
;;; (equiv-to (name wff) (name wff) ...)
;;; tells equivalent wffs with their names.

(eval-when (compile load eval)
(defcategory repsymbol
  (define defrepsymbol)
  (properties
   (equiv-to read-equiv-to)
   (mhelp single))
  (global-list global-repsymbollist)
  (mhelp-line "replaceable symbol")
  (mhelp-fn princ-rephelp))
)

(eval-when (compile load eval)
(defun read-equiv-to (prop-value name)
  (let ((global-type (get name 'type)))
    (declare (special global-type))
    (if global-type
	(do ((equiv-wffs prop-value (cdr equiv-wffs))
	     (internal-wffs
	      nil
	      (acons (caar equiv-wffs) (getrwff (cadar equiv-wffs))
		    internal-wffs)))
	    ((null equiv-wffs) internal-wffs))
	(tps-warning "Trying to define a REPSYMBOL which is not a logical symbol."))))
)

(eval-when (compile load eval)
(defun princ-rephelp (keyword category)
  (declare (ignore category))
  (msgf keyword " may be replaced by any of:" t)
  (dolist (equiv-wff (get keyword 'equiv-to))
    (msgf (car equiv-wff) (t 10) ((cdr equiv-wff) . gwff))))
)


;;; Next section is about replacing wffs by an equivalent wff.

(context substitution)

(defun get-equiv (rep-sym equiv-name)
  (cdr (assoc equiv-name (get rep-sym 'equiv-to))))

(defun get-pmequiv (rep-sym equiv-name)
  (let ((stands-for (get rep-sym 'stands-for)))
    (substitute-types
     (mapcar #'(lambda (tvar tconst) (cons tvar tconst))
	     (get stands-for 'typelist)
	     (get rep-sym 'polytypelist))
     (cdr (assoc equiv-name (get stands-for 'equiv-to))))))

(defwffrec replace-equiv-wff
  (argnames inwff chkfn equiv-name))

(defun replace-equiv-wff (inwff chkfn equiv-name)
  (cond ((label-q inwff)
	 (apply-label inwff (replace-equiv-wff inwff chkfn equiv-name)))
	((lsymbol-q inwff)
	 (cond ((or (logconst-q inwff) (propsym-q inwff))
		(if (funcall chkfn inwff)
		    (get-equiv inwff equiv-name) inwff))
	       ((or (pmpropsym-q inwff) (pmabbrev-q inwff))
		(if (funcall chkfn (get inwff 'stands-for))
		    (get-pmequiv inwff equiv-name) inwff))
	       ((abbrev-q inwff)
		(if (funcall chkfn inwff)
		    (get-equiv inwff equiv-name) inwff))))
	((boundwff-q inwff)
	 (cond
	  ;;Equiv-wffs for binders not yet allowed.
	  ;;((and (anyabbrev-q (binding inwff))
	  ;;(funcall chkfn (binding inwff)))
	  ;;(get-def-binder (binding inwff) (bindvar inwff) (gdr inwff)))
	  (t (bind-var-wff
	      (binding inwff) (bindvar inwff)
	      (replace-equiv-wff (gdr inwff) chkfn equiv-name)))))
	(t (let ((newcar (replace-equiv-wff (car inwff) chkfn equiv-name)))
	     (if (and (lambda-bd-p newcar)
		      (not (lambda-bd-p (car inwff))))
		 (lcontr (cons newcar
			       (replace-equiv-wff (cdr inwff)
						  chkfn equiv-name)))
		 (cons newcar
		       (replace-equiv-wff (cdr inwff)
					  chkfn equiv-name)))))))


(eval-when (compile load eval)
(defun repsym-p (obj) (and (symbolp obj) (get obj 'repsymbol)))
)

(defwffop replace-equiv-all
  (argtypes repsym symbol gwff)
  (resulttype gwff)
  (argnames rep-sym rep-by rep-in)
  (arghelp "Symbol to be Replaced" "Replace by (Name)" "Replace in")
  (applicable-p (lambda (rep-sym rep-by rep-in)
		  (declare (ignore rep-in))
		  (if (assoc rep-by (get rep-sym 'equiv-to)) t nil)))
  (mhelp "Replace a all occurrences of a symbol by a predefined equivalent wff."))

(defun replace-equiv-all (rep-sym rep-by rep-in)
  (replace-equiv-wff rep-in #'(lambda (symbol) (eq symbol rep-sym))
		     rep-by))

(defedop rpall
  (alias replace-equiv-all)
  (result-> edwff)
  (edwff-argname rep-in))


(defwffop replace-equiv
  (argtypes repsym symbol gwff)
  (resulttype gwff)
  (argnames rep-sym rep-by rep-in)
  (arghelp "Symbol to be Replaced" "Replace by (Name)" "Replace in")
  (applicable-p (lambda (rep-sym rep-by rep-in)
		  (declare (ignore rep-in))
		  (if (assoc rep-by (get rep-sym 'equiv-to)) t nil)))
  (mhelp "Replace one occurrence of a symbol (such as AND) by a predefined 
equivalent wff (such as [lambda p lambda q.~.p IMPLIES ~q]).  In this
example repsym is AND and rep-by is IMPLIES.  To see if a symbol can be
replaced by this command, enter HELP symbol; any such replacements will be
listed under the heading 'Replaceable Symbols'."))

(defun replace-equiv (rep-sym rep-by rep-in)
  (let ((oneflag nil))
    (declare (special oneflag))
    (replace-equiv-wff
     rep-in
     #'(lambda (symbol)
	 (declare (special oneflag))
	 (cond ((and (not oneflag) (eq symbol rep-sym))
		(setq oneflag t) t)
	       (t nil)))
     rep-by)))

(defedop rp
  (alias replace-equiv)
  (result-> edwff)
  (edwff-argname rep-in))
