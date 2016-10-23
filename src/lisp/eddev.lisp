;;; -*- Mode:LISP; Package:AUTO -*-
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
;;; File: EDDEV
;;;
;;; defines editor operations useful in connexion with development sequences.
;;;

(deffile eddev
  (part-of wff-editor)
  (extension clisp)
  (mhelp "Contains operations for quantifiers in the editor."))

(context develop-seqs)

(defedop db
  (alias delete-leftmost-binder)
  (result-> execute)
  (edwff-argname gwff))

(defwffop delete-leftmost-binder
  (argtypes gwff)
  (resulttype ed-command)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Delete the leftmost binder in a wff."))

(defwffop delete-binder
  (argtypes gwff)
  (resulttype gwff)
  (argnames bdwff)
  (arghelp "bound wff")
  (applicable-q ae-bd-wff-p)
  (applicable-q ae-bd-wff-p)
  (mhelp "Delete a top-level universal or existential binder."))

(defun delete-binder (bdwff)
  (cond ((label-q bdwff)
	 (apply-label bdwff (delete-binder bdwff)))
	((lsymbol-q bdwff)
	 (throwfail "Cannot delete binder from " (bdwff . gwff)
		    ", a logical symbol."))
	((boundwff-q bdwff)
	 (cdr bdwff))
	(t (throwfail "Cannot delete binder from an application."))))

(defedop op
  (alias openwffa)
  (result-> edwff)
  (edwff-argname gwff))

(defwffop openwffa
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Delete all accessible essentially universal quantifiers."))

(defun openwffa (gwff) (openwffa1 gwff nil))

(defwffrec openwffa1
  (argnames gwff existflag))

(defun openwffa1 (gwff existflag)
  (cond ((label-q gwff) (apply-label gwff (openwffa1 gwff existflag)))
	((lsymbol-q gwff) gwff)
	((boundwff-q gwff)
	 (if (essuniv (car gwff) existflag)
	     (openwffa1 (cdr gwff) existflag) gwff))
	((boolean-recursion openwffa1 gwff existflag))))

(defedop ep
  (alias openwffe)
  (result-> edwff)
  (edwff-argname gwff))

(defwffop openwffe
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Delete all accessible essentially existential quantifiers."))

(defun openwffe (gwff) (openwffe1 gwff nil))

(defwffrec openwffe1
  (argnames gwff existflag))

(defun openwffe1 (gwff existflag)
  (cond ((label-q gwff) (apply-label gwff (openwffe1 gwff existflag)))
	((lsymbol-q gwff) gwff)
	((boundwff-q gwff)
	 (if (essexist (car gwff) existflag)
	     (openwffe1 (cdr gwff) existflag) gwff))
	((boolean-recursion openwffe1 gwff existflag))))

(defun delete-leftmost-binder (gwff)
  (let ((bdwff-cmds (find-binder gwff)))
    (append (ldiff bdwff-cmds (member 'p bdwff-cmds))
	    `(sub (delete-binder edwff)))))

