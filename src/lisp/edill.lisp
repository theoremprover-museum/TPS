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
;;; File: EDILL
;;;
;;; defines editor operations dealing with ill-formed formulas (or possibly
;;; ill-formed formulas).
;;;

(part-of wff-editor)

(deffile edill
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Contains editing operations for ill-formed formulae."))

(context ill-formed)

(defwffop locateunwffs
  (argtypes gwff-ill)
  (resulttype msglistlist)
  (argnames unwff)
  (arghelp "unwff")
  (mhelp "Return a list of messages, each the describing the error
in a minimal ill-formed subparts of the argument."))

(defun locateunwffs (unwff)
  (cond ((label-q unwff) (apply-label unwff (locateunwffs unwff)))
	((lsymbol-p unwff) nil)
	((atom unwff) (list `((',unwff . gwff) "Not a legal logical symbol.")))
	((boundwff-q unwff)
	 (let ((cdrunwff (locateunwffs (cdr unwff))))
	   (cond ((null cdrunwff)
		  (cond ((legal-type-p unwff) nil)
			(t (list `((',unwff . gwff)
				    "Types required by binder does not match type of scope or bound variable.")))))
		 (t cdrunwff))))
	(t (let ((carunwff (locateunwffs (car unwff)))
		 (cdrunwff (locateunwffs (cdr unwff))))
	     (cond ((and (null carunwff) (null cdrunwff))
		    (cond ((legal-type-p unwff) nil)
			  (t (list `("Types "
				      (',(type (car unwff)) . typesym)
				      " of " (',(car unwff) . gwff)
				      " and "
				      (',(type (cdr unwff)) . typesym)
				      " of " (',(cdr unwff) . gwff)
				      " don't match.")))))
			   (t (nconc carunwff cdrunwff)))))))

(defwffop culprit-p
  (argtypes gwff-ill)
  (resulttype msglistlist)
  (argnames unwff)
  (arghelp "unwff")
  (mhelp "Test whether the unwff is a minimal ill-formed part."))


(defun culprit-p (unwff)
  (cond ((label-q unwff) (apply-label unwff (culprit-p unwff)))
	((lsymbol-p unwff) nil)
	((atom unwff) (list `((,unwff . gwff) "Not a legal logical symbol.")))
	((boundwff-q unwff)
	 (let ((cdrunwff (locateunwffs (cdr unwff))))
	   (cond ((null cdrunwff)
		  (cond ((legal-type-p unwff) nil)
			(t (list `((,unwff . gwff)
				    "Types required by binder does not match type of scope or bound variable.")))))
		 (t nil))))
	(t (let ((carunwff (locateunwffs (car unwff)))
		 (cdrunwff (locateunwffs (cdr unwff))))
	     (cond ((and (null carunwff) (null cdrunwff))
		    (cond ((legal-type-p unwff) nil)
			  (t (list `("Types " (,(type (car unwff)) . typesym) " of " (,(car unwff) . gwff)
					       " and " (,(type (cdr unwff)) . typesym) " of " (,(cdr unwff) . gwff)
					       " don't match.")))))
		   (t nil))))))

(defedop wffp
  (alias gwff-p)
  (result-> (lambda (yesno) 
	      (msgf "Expression is"
		   (cond ((not yesno) " not ")(t " "))
		   "a wff.")))
  (edwff-argname gwff))

(defedop ill
  (alias locateunwffs)
  (edwff-argname unwff))


(defedop edill
  (alias find-culprit)
  (result-> execute)
  (edwff-argname gwff))

(defwffop find-culprit
  (argtypes gwff-ill)
  (resulttype ed-command)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Find a minimal ill-formed subformula."))

(defun find-culprit (gwff) (edsearch gwff 'culprit-p))
