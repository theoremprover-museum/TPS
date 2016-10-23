;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of READ-RULES)

(deffile read-ruledefs
  (part-of read-rules)
  (extension lsp)
  (mhelp "Defines macros and functions necessary to digest rule definitions."))

(defun lvarconst-p (gwff)
  (or (gvar-p gwff) (logconst-p gwff)))

(defun read-lvarconsts (prop-value category)
  (declare (ignore category))
  (mapargtype 'lvarconst prop-value))

(defun read-typesyms (prop-value category)
  (declare (ignore category))
  (mapargtype 'typesym prop-value))

(defun read-lines (prop-value category)
  (declare (ignore category))
  (mapdigest #'read-linespec prop-value))

(defun read-linespec (linespec)
  (list (car linespec) (mapargtype 'wffset (cadr linespec))
	(mapargtype 'symbol (caddr linespec))
	(gettype 'gwff0 (cadddr linespec))
	(if (cddddr linespec)
	    (gettype 'justification (car (cddddr linespec)))
	    nil)))

(defun read-restrictions (prop-value category)
  (declare (ignore category))
  (mapdigest #'read-restriction prop-value))

(defun read-restriction (restr)
  (let ((wffop (car restr)))
    (cons wffop (mapargtypelist (get wffop 'argtypes) (cdr restr)))))

(defun read-items (prop-value category)
  (declare (ignore category))
  (mapdigest #'read-itemhelp prop-value))

(defun read-itemhelp (itemhelp)
  (let ((item (car itemhelp)))
    (cons (cond ((stringp item)
		 (gettype 'lvarconst item))
		(t item))
	  (cadr itemhelp))))

;;; Given a list of types, find-type-vars will return a list of
;;; type variables in those types.  E.G.
;;; (find-type-vars '((A . O) (A . (A . B)))) => (A B)
;;; if O is a type constant.

(defun find-type-vars (wfftypelist)
  (let ((wffop-types nil))
    (declare (special wffop-types))
    (do ((wffargtypes wfftypelist (cdr wffargtypes)))
	((null wffargtypes)
	 wffop-types)
      (rec-push-types (car wffargtypes)))))

(defun rec-push-types (typesym)
  (declare (special wffop-types))
  (cond ((symbolp typesym)
	 (when (and (not (get typesym 'typeconst))
		    (not (get typesym 'typeabbrev)) ; cebrown 6/4/99
		    (not (member typesym wffop-types)))
	       (push typesym wffop-types)))
	(t (rec-push-types (car typesym))
	   (rec-push-types (cdr typesym)))))
