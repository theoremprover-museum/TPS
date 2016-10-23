;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of tpsdef)

(deffile contexts-ml
  (part-of tpsdef)
  (extension lsp)
  (mhelp "Defines contexts used in the ML package."))

(defcontext rules-1-misc
            (order 70)
            (mhelp " ")
            (short-id "Miscellaneous Rules"))

(defcontext rules-2-prop
            (order 71)
            (mhelp " ")
            (short-id "Propositional Rules"))

(defcontext rules-3-neg
            (order 72)
            (mhelp " ")
            (short-id "Negation Rules"))

(defcontext rules-4-quant
            (order 73)
            (mhelp " ")
            (short-id "Quantifier Rules"))

(defcontext rules-5-subst
            (order 74)
            (mhelp " ")
            (short-id "Substitution Rules"))

(defcontext rules-6-equality
            (order 75)
            (mhelp " ")
            (short-id "Equality Rules"))

(defcontext rules-7-defn
            (order 76)
            (mhelp " ")
            (short-id "Definition Rules"))

(defcontext rules-8-lambda
            (order 77)
            (mhelp "Having to do with lambda conversion rules.")
            (short-id "Lambda Conversion Rules"))

(defcontext book-theorems
            (order 89)
	    (mhelp "Book Theorems.")
	    (short-id "Book Theorems"))

(defcontext ml1-exercises
            (short-id "First-Order Logic")
            (order 91)
            (mhelp "Having to do with exercises for first order logic."))

(defcontext ml2-exercises
            (short-id "Higher-Order Logic")
            (order 92)
            (mhelp "Having to do with exercises for higher order logic."))

(defcontext abbrev-set-ops
            (short-id "Set Abbreviations")
            (order 111)
            (mhelp "Set-theoretic logical abbreviations."))

