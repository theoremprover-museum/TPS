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

;;; In the following some definitions are made comments so as to
;;; avoid confusion in ETPS2.  Eventually, we would like to have
;;; the functionality.

(deffile ml2-abbrev
  (part-of math-logic-2-wffs)
  (extension lsp)
  (mhelp "Abbreviations for Math Logic II."))

(context abbrev-ops)

;;; From Section 61

;*;(def-abbrev =
;*;  (type "OAA")
;*;  (typelist ("A"))
;*;  (printnotype t)
;*;  (infix 7)
;*;  (defn "lambda A(A) lambda B(A). Q A B"))

;*;(def-abbrev T
;*;  (type "O")
;*;  (printnotype t)
;*;  (defn "[=] =(O(OOO)(OOO)) [=]"))

;*;(def-abbrev F
;*;  (type "O")
;*;  (printnotype t)
;*;  (defn "[lambda x(O) T] = [lambda x(O) x]"))

;*;(def-abbrev PI
;*;  (type "O(OA)")
;*;  (typelist ("A"))
;*;  (printnotype t)
;*;  (fo-single-symbol pi)
;*;  (face cappi)
;*;  (defn "[= [lambda x(A). T]]")
;*;  (mhelp "T iff the set is universal."))

(def-binder forall
   (typelist ("A"))
   (var-type "A")
   (scope-type "O")
   (wff-type "O")
   (prefix 100)
   (fo-single-symbol forall)
;*;   (def-var "x(A)")
;*;   (def-scope "A(O)")
;*;   (defn "PI [lambda x(A) A(O)]")
   (mhelp "Universal quantifier."))

;*;(def-abbrev and
;*;  (type "OOO")
;*;  (printnotype t)
;*;  (fo-single-symbol and)
;*;  (infix 5)
;*;  (defn
;*;    "lambda x(O) lambda y(O). [lambda g(OOO). g T T] = [lambda g(OOO). g x y]")
;*;  (mhelp "Conjunction."))

;*;(def-abbrev implies
;*;  (type "OOO")
;*;  (printnotype t)
;*;  (fo-single-symbol implies)
;*;  (infix 3)
;*;  (defn "lambda x(O) lambda y(O). x = [x and y]"))

;*;(def-abbrev not
;*;  (type "OO")
;*;  (printnotype t)
;*;  (fo-single-symbol not)
;*;  (prefix 6)
;*;  (defn "= F"))

;*;(def-abbrev or
;*;  (type "OOO")
;*;  (printnotype t)
;*;  (fo-single-symbol or)
;*;  (infix 4)
;*;  (defn "lambda x(O) lambda y(O). not .[not x] and [not y]")
;*;  (mhelp "Disjunction."))

(def-abbrev subset
   (type "O(OA)(OA)")
   (typelist ("A"))
   (printnotype t)
   (infix 8)
   (fo-single-symbol subset)
   (defn "lambda P(OA) lambda R(OA). forall x . P x implies R x"))

(def-abbrev equiv
  (type "OOO")
  (printnotype t)
  (fo-single-symbol equiv)
  (infix 2)
  (defn "[=(OOO)]"))

(def-binder exists
   (typelist ("A"))
   (var-type "A")
   (scope-type "O")
   (wff-type "O")
   (prefix 100)
   (fo-single-symbol exists)
;*;   (def-var "x(A)")
;*;   (def-scope "A(O)")
;*;   (defn "not forall x(A) not A(O)")
   (mhelp "Existential quantifier."))

;;; From Section 62

;*;(defcontext abbrev-set-ops
;*;  (short-id "Set Abbreviations")
;*;  (order 111)
;*;  (mhelp "Set-theoretic logical abbreviations."))

(context abbrev-set-ops)

(def-abbrev %
  (type "OA(OB)(AB)")
  (typelist ("A" "B"))
  (printnotype t)
  (fo-single-symbol t)
  (defn "lambda f(AB) lambda x(OB) lambda z(A) exists t(B).x t and z = f t"))


(def-abbrev complement
  (type "OA(OA)")
  (typelist ("A"))
  (printnotype t)
  (prefix 11)
  (face ~)
  (fo-single-symbol ~)
  (defn "lambda S(OA) lambda x(A) not. S x"))


(def-abbrev equivs
  (type "O(OA)(OA)")
  (typelist ("A"))
  (printnotype t)
  (infix 7)
  (face equiv sups)
  (fo-single-symbol equivs)
  (defn "lambda P(OA) lambda R(OA) forall x(A) [P x equiv R x]"))

(def-abbrev intersect
  (type "OA(OA)(OA)")
  (typelist ("A"))
  (printnotype t)
  (infix 10)
  (fo-single-symbol intersect)
  (defn "lambda P(OA) lambda R(OA) lambda x(A). P x and R x"))

(def-abbrev powerset
  (type "(O(OA))(OA)")
  (typelist ("A"))
  (printnotype nil)
  (fo-single-symbol powerset)
  (defn "lambda P(OA) lambda R(OA). R subset P"))

(def-abbrev setequiv
  (type "O(OA)(OA)")
  (typelist ("A"))
  (printnotype t)
  (infix 7)
  (face equiv sups)
  (fo-single-symbol setequiv)
  (defn "lambda P(OA) lambda R(OA). [P subset R] and [R subset P]"))

(def-abbrev setintersect
  (type "OA(O(OA))")
  (typelist ("A"))
  (printnotype t)
  (fo-single-symbol setintersect)
  (defn "lambda D(O(OA)) lambda x(A) forall S(OA). D S implies S x"))

(def-abbrev setunion
  (type "(OA)(O(OA))")
  (typelist ("A"))
  (printnotype t)
  (fo-single-symbol setunion)
  (defn "lambda D(O(OA)) lambda x(A) exists S(OA). D S and S x"))

(def-abbrev union
  (type "OA(OA)(OA)")
  (typelist ("A"))
  (printnotype t)
  (infix 9)
  (fo-single-symbol union)
  (defn "lambda P(OA) lambda R(OA) lambda z(A). P z or R z"))

;;; From Section 63

(context abbrev-ops)

;*;(def-abbrev sigma1
;*;  (typelist ("A"))
;*;  (type "O(OA)")
;*;  (face capsigma sub1)
;*;  (defn "lambda P(OA) exists y(A). P = [=(OAA) y]"))

;*;(def-binder exists1
;*;  (typelist ("A"))
;*;  (var-type "A")
;*;  (scope-type "O")
;*;  (wff-type "O")
;*;  (prefix 100)
;*;  (fo-single-symbol exists1)
;*;  (face exists sub1)
;*;  (def-var "x(A)")
;*;  (def-scope "A(O)")
;*;  (defn "sigma1(O(OA)) [lambda x(A) A(O)]"))

;;; Section 70

;*;(def-abbrev eqp
;*;  (typelist ("A" "B"))
;*;  (type "O(OA)(OB)")
;*;  (printnotype t)
;*;  (defn "lambda p(OB) lambda q(OA) exists s(AB).
;*;            forall x(B) [p x implies q. s x] and forall y(A).
;*;             q y implies exists1 x(B). p x and y = s x"))

(def-abbrev unitset
  (type "(OAA)")
  (typelist ("A"))
  (printnotype t)
  (fo-single-symbol unitset)
  (face scriptu)
  (defn "lambda x(A) lambda y(A). x = y"))












