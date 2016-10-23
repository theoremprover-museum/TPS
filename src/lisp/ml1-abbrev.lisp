;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of MATH-LOGIC-1-WFFS)

;;;
;;; File: ML1-Abbrev
;;;
;;; abbreviations for Mathematical Logic I
;;;

(part-of math-logic-1-wffs)

(deffile ml1-abbrev
  (part-of math-logic-1-wffs)
  (extension lsp)
  (mhelp "Abbreviations for Math Logic I."))

(context prim-obj)

(def-abbrev equiv
   (type "OOO")
   (printnotype t)
   (infix 2)
   (fo-single-symbol equiv)
   (defn "LAMBDA p(O) LAMBDA q(O). [p IMPLIES q] AND [q IMPLIES p]"))
