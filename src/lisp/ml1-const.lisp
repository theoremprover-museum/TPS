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
;;; File: ML1-CONST
;;; Author: fp
;;;
;;; This file defines the categories TYPECONST, LOGCONST, and BINDER.
;;; I,O; AND,OR,IMPLIES,NOT; LAMBDA,FORALL,EXISTS are defined.
;;;

(part-of math-logic-1-wffs)

(deffile ml1-const
  (part-of math-logic-1-wffs)
  (extension lsp)
  (mhelp "Defines logical constants."))

(context prim-obj)

(def-typeconst o
  (mhelp "The type of truth values."))

(def-typeconst i
  (mhelp "The type of individuals."))

(def-logconst and
   (type "OOO")
   (printnotype t)
   (infix 5)
   (prt-associative t)
   (fo-single-symbol and))

(def-logconst or
   (type "OOO")
   (printnotype t)
   (infix 4)
   (prt-associative t)
   (fo-single-symbol or))

(def-logconst implies
   (type "OOO")
   (printnotype t)
   (infix 3)
   (fo-single-symbol implies))

(def-logconst not
   (type "OO")
   (printnotype t)
   (prefix 6)
   (fo-single-symbol not))

;;; We need to leave LAMBDA in here for the rule definitions,
;;; unless we reformulate them in terms of substitution instead
;;; of lambda-contraction.

(def-binder lambda
   (typelist ("A" "B"))
   (var-type "A")
   (scope-type "B")
   (wff-type "BA")
   (prefix 100)
   (fo-single-symbol lambda)
   (mhelp "Church's lambda binder."))

;;; forall and exists binds only individual variables in ML1

(def-binder forall
   (typelist ())
   (var-type "I")
   (scope-type "O")
   (wff-type "O")
   (prefix 100)
   (fo-single-symbol forall)
   (mhelp "Universal quantifier."))

(def-binder exists
   (typelist ())
   (var-type "I")
   (scope-type "O")
   (wff-type "O")
   (prefix 100)
   (fo-single-symbol exists)
