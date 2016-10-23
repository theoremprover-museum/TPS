;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFFMATCH)

;;;
;;; File: Match-Macros
;;; Package: Wffmatch
;;;

(part-of wffmatch)

(deffile match-macros
  (part-of wffmatch)
  (extension lsp)
  (mhelp "Defines macros and TPS objects to deal with matching."))

(context otl-object)

(defmacro wffset (meta-var gwff)
  `(push (cons ,meta-var ,gwff) wffbindings))

(defmacro wffeval (meta-var)
  `(cdr (assoc ,meta-var wffbindings)))

(defflag default-wffeq
  (flagtype symbol)
  (default wffeq-ab)
  (subjects outline)
  (mhelp "The name of the functions which checks for equality of wffs."))


(defmode rules
  (flag-settings
   (first-order-mode-parse nil)
   (make-wffops-labels t))
  (mhelp "Set flags so that the rules package can be run successfully."))


(defmacro same-match-p (meta-var new-match last-match)
  (declare (ignore meta-var))
  `(funcall default-wffeq ,new-match ,last-match))


;;; Changed name from MISMATCH since function of that name already
;;; exists in Common Lisp.  DAN 13Sep89

(defmacro mismatch% (expected found)
  `(throwfail "Wff Mismatch.  Expected " (,expected . gwff)
		    ", but found " (,found . gwff) ".  "))



