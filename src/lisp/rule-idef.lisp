;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of RULES)

;;;
;;; File: Rule-IDef
;;; Package: Rules
;;;
;;; defines the category of intermediate rule definitions (IRULEDEF)
;;; and some functions on them.

(part-of rules)

(deffile rule-idef
  (part-of rules)
  (extension lsp)
  (mhelp "Defines the category of intermediate rule definitions (IRULEDEF)
and some functions on them."))

(defcategory iruledef
  (define defirule)
  (properties
   (lines read-lines)
   (restrictions read-restrictions)
   (priority single)
   (support-transformation multiple)
   (itemshelp read-items)
   (hyp-restrict single) ;currently only checks NIL or not-NIL - see rule-build-defaults.lisp
   (mhelp single))
  (global-list global-irulelist)
  (mhelp-line "intermediate rule definition")
  (mhelp-fn (declare (ignore category))
	    (feat-help iruledef 'iruledef
		       15 75 (get 'iruledef 'properties))))


