;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLSUGGEST)

;;;
;;; File: Otl-Sugg-Mac
;;;
;;; defines category of suggested rule.
;;;

(part-of otlsuggest)

(deffile otl-sugg-mac
  (part-of otlsuggest)
  (extension lsp)
  (mhelp "Defines category of suggested rule."))

(context suggestions)

(defcategory srule
  (define defsrule)
  (properties
   (matchfn singlefn)
   (match1fn singlefn)
   (shortfn singlefn)
   (priority single))
  (global-list global-srulelist)
  (mhelp-line "inference rule")
  (mhelp-fn princ-srulehelp))

(defun princ-srulehelp (srule category)
  (declare (ignore category))
  (msgf "Its priority is " (get srule 'priority)))
