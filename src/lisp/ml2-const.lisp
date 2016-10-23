;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of MATH-LOGIC-2-WFFS)

(part-of math-logic-2-wffs)

;;; Binders except Lambda are in ML2-Abbrev.

(deffile ml2-const
  (part-of math-logic-2-wffs)
  (extension lsp)
  (mhelp "Defines logical constants."))

(context prim-obj)

(def-typeconst o
  (mhelp "The type of truth values."))

(def-typeconst i
  (mhelp "The type of individuals."))

(def-typeabbrev s
  (type-defn "O(OI)")
  (mhelp "The type of natural numbers."))

(def-binder lambda
   (typelist ("A" "B"))
   (var-type "A")
   (scope-type "B")
   (wff-type "BA")
   (prefix 100)
   (fo-single-symbol lambda)
   (mhelp "Church's lambda binder."))

;*;(def-pmpropsym Q
;*;  (type "OAA")
;*;  (typelist ("A"))
;*;  (printnotype t))

(def-pmpropsym =
  (type "OAA")
  (typelist ("A"))
  (printnotype t)
  (infix 7)
  (mhelp "Equality"))

(def-pmpropsym iota
  (type "A(OA)")
  (typelist ("A"))
  (printnotype t)
  (fo-single-symbol iota)
  (mhelp "Description operator"))

;;; There is a definition for the following propositional connectives
;;; in ML2-ABBREV, but those are made comments.

(def-logconst and
   (type "OOO")
   (printnotype t)
   (infix 5)
   (prt-associative t)
   (fo-single-symbol and)
   (mhelp "Denotes conjunction."))

(def-logconst or
   (type "OOO")
   (printnotype t)
   (infix 4)
   (prt-associative t)
   (fo-single-symbol or)
   (mhelp "Denotes (inclusive) disjunction."))

(def-logconst implies
   (type "OOO")
   (printnotype t)
   (infix 3)
   (fo-single-symbol implies)
   (mhelp "Denotes implication."))

(def-logconst not
   (type "OO")
   (printnotype t)
;   (prefix 6)
   ;;  increased to make higher than = DAN 19MAR89
   (prefix 8) 
   (fo-single-symbol not)
   (mhelp "Denotes negation."))


;;; Added following 9/5/87 DAN

(def-logconst falsehood
   (type "O")
   (printnotype t)
   (fo-single-symbol f)
   (mhelp "Denotes falsehood."))

(def-logconst truth
   (type "O")
   (printnotype t)
   (fo-single-symbol t)
   (mhelp "Denotes truth."))
