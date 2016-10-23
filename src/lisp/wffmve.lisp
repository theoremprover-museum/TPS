;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFFS)

;;;
;;; File: WFFMVE
;;;
;;; Defines wff moving operations like GAR, GDR, etc.
;;;

(part-of wffs)

(deffile wffmve
  (part-of wffs)
  (extension lsp)
  (mhelp "Contents allow movement within wffs."))

(context moving)

(defwffop gar
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (applicable-q (lambda (gwff) (or (label-q gwff) (not (lsymbol-q gwff)))))
  (applicable-p (lambda (gwff) (not (lsymbol-p gwff))))
  (mhelp "Extract the 'function' part of an application.
Returns the bound variable from a wff with top-level binder."))

(defun gar (gwff)
  (cond ((label-q gwff)
	 (apply-label gwff (gar gwff)))
	((lsymbol-q gwff) (throwfail 
			   "Cannot apply GAR to " (gwff . gwff) ", a logical symbol."))
	((boundwff-q gwff) (caar gwff))
	(t (car gwff))))

(defwffop replace-gar
  (argtypes gwff gwff)
  (resulttype gwff)
  (argnames gwff newgarwff)
  (arghelp "gwff" "New gar")
  (applicable-q (lambda (gwff) (or (label-q gwff) (not (lsymbol-q gwff)))))
  (applicable-p (lambda (gwff) (not (lsymbol-p gwff))))
  (replaces gar)
  (mhelp 
   "Replace the 'function' part of an application non-destructively."))

(defun replace-gar (gwff newgarwff)
  (cond ((label-q gwff)
	 (apply-label gwff (replace-gar gwff newgarwff)))
	((lsymbol-q gwff) (throwfail 
			   "Cannot replace GAR of " (gwff . gwff) ", a logical symbol."))
	((boundwff-q gwff) (cons (cons newgarwff (cdar gwff)) 
				 (cdr gwff)))
	(t (cons newgarwff (cdr gwff)))))


(defwffop gdr
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (applicable-q (lambda (gwff) (or (label-q gwff) (not (lsymbol-q gwff)))))
  (applicable-p (lambda (gwff) (not (lsymbol-p gwff))))
  (mhelp "Extract the 'argument' part of an application.
Returns the scope of the binder from a wff with top-level binder."))


(defun gdr (gwff)
  (cond ((label-q gwff) (apply-label gwff (gdr gwff)))
	((lsymbol-q gwff) (throwfail 
			   "Cannot apply GDR to " (gwff . gwff) ", a logical symbol."))
	((boundwff-q gwff) (cdr gwff))
	(t (cdr gwff))))

(defwffop replace-gdr
  (argtypes gwff gwff)
  (resulttype gwff)
  (argnames gwff newgdrwff)
  (arghelp "gwff" "New gdr")
  (applicable-q (lambda (gwff newgdrwff)
		  (declare (ignore newgdrwff))
		  (or (label-q gwff) (not (lsymbol-q gwff)))))
  (applicable-p (lambda (gwff newgdrwff)
		  (declare (ignore newgdrwff))
		  (not (lsymbol-p gwff))))
  (replaces gdr)
  (mhelp 
   "Replace the 'argument' part of an application non-destructively."))

(defun replace-gdr (gwff newgdrwff)
  (cond ((label-q gwff)
	 (apply-label gwff (replace-gdr gwff newgdrwff)))
	((lsymbol-q gwff) (throwfail 
			   "Cannot replace GDR of " (gwff . gwff) ", a logical symbol."))
	((boundwff-q gwff) (cons (car gwff) newgdrwff))
	(t (cons (car gwff) newgdrwff))))
