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

(context maint::rules-object)

(deffile read-rdef-mac
  (part-of read-rules)
  (extension lsp)
  (mhelp "Defines macros necessary to digest rule definitions."))

(defflag treat-hlines-as-dlines
  (flagtype boolean)
  (default t)
  (subjects rules-object)
  (mhelp "If T, hlines may have multiple hypotheses and a justification,
if NIL, hlines can only have one hypothesis (itself) and `Hyps' as
justification."))

(defflag hline-justification
  (flagtype string)
  (default "Hyp")
  (subjects rules-object)
  (mhelp "The justification for hlines, if TREAT-HLINES-AS-DLINES is NIL."))


(defmacro mapdigest (digestfn external-list)
  `(do ((external-list ,external-list (cdr external-list))
	(internal-list nil (cons (funcall ,digestfn (car external-list))
				 internal-list)))
        ((null external-list) (nreverse internal-list))))

(defmacro mapargtype (argtype external-list)
  `(mapdigest #'(lambda (ext-arg) (gettype ,argtype ext-arg))
	      ,external-list))

(defmacro mapargtypelist (argtypelist external-list)
  `(mapcar #'gettype ,argtypelist ,external-list))

(context proof-outline)

(defun just-p (just)
  (and (consp just) (stringp (car just))
       (consp (cdr just)) (gwfflist-p (cadr just))
       (consp (cddr just)) (rlinelist-p (caddr just))))

(defun gwfflist-p (l) (declare (ignore l)) t)

(defun getjust (just)
  (mapargtypelist '(string gwfflist rlinelist) just))


(defun gettypedgwff (type gwff)
  (let ((global-type type))
    (declare (special global-type))
    (getwff-subtype #'gwff-p gwff)))

(defun rlinelist-p (l) (declare (ignore l)) t)
