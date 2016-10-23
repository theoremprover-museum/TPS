;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of JFORMS)

(deffile jforms-defns
  (part-of jforms)
  (extension clisp)
  (mhelp "Jform Macro file."))

(context jforms1)

(defflag lit-name
  (flagtype symbol)
  (default lit)
  (subjects jforms)
  (mhelp "Prefix for labels associated with literals."))


(defflag print-lit-name
  (flagtype boolean)			
  (default T)
  (subjects jforms)
  (mhelp "If the value of this flag is true, labels (instead
 of wffs associated with literal, or neg-literal) are printed inside
 the editor."))


(context jforms1)

(defgwff-type jforms-labels
  (checkfn jform-p)
  (getfn jform-to-gwff)
  (mhelp "Labels used in JFORMS."))
;is this ever used? MB Thu Apr  6 17:24:09 1995