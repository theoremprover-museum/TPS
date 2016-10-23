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

(deffile ml2-abbrev2
  (part-of math-logic-2-wffs)
  (extension lisp)
  (mhelp "Abbreviations for Math Logic II."))

(def-abbrev sigma1
  (typelist ("A"))
  (type "O(OA)")
  (face capsigma sup1)
  (fo-single-symbol sigma1)
  (defn "lambda P(OA) exists y(A). P = [=(OAA) y]"))

(def-binder exists1
  (typelist ("A"))
  (var-type "A")
  (scope-type "O")
  (wff-type "O")
  (prefix 100)
  (fo-single-symbol exists1)
  (face exists sub1)
  (def-var "x(A)")
  (def-scope "A(O)")
  (defn "sigma1(O(OA)) [lambda x(A) A(O)]"))

(def-binder that 
  (typelist ("C"))
  (var-type "C")
  (scope-type "O")
  (wff-type "C")
  (prefix 100)
  (fo-single-symbol that)
  (def-var "z(C)")
  (def-scope "A(O)")
  (defn "IOTA [lambda z(C) A(O)]")
  (mhelp "Description binder: Selects the unique term such that."))

;; Change this name?
(def-abbrev cond
  (typelist ("C"))
  (type "COCC")
  (printnotype t)
  (fo-single-symbol t)
  (defn "lambda x(C) lambda y(C) lambda p(O) that q(C).[p and .x = q] or [not p and .y = q"))

;;; Section 70

(def-abbrev eqp
  (typelist ("A" "B"))
  (type "O(OA)(OB)")
  (printnotype nil)
  (fo-single-symbol eqp)
  (defn "lambda p(OB) lambda q(OA) exists s(AB). forall x(B) [p x implies q. s x] and forall y(A). q y implies exists1 x(B). p x and y = s x"))

(def-abbrev nc
  (typelist ("B"))
  (type "O(O(OB))")
  (fo-single-symbol NC)
  (defn "lambda u(O(OB)) exists p(OB). u(O(OB)) = EQP(O(OB)(OB)) p"))

(def-abbrev zero
  (type "S")
  (fo-single-symbol zero)
  (defn "lambda p(OI) . not exists x p x"))

(def-abbrev succ
  (type "SS")
  (fo-single-symbol SUCC)
  (defn "lambda n(O(OI)) lambda p(OI) exists x(I). p x and n[lambda t(I). not [t = x] and p t]"))


(def-abbrev one
  (type "S")
  (fo-single-symbol one)
  (printnotype t)
  ;; (defn "lambda p(OI) . exists1 x p x")
  (defn "succ zero"))

(def-abbrev nat
  (type "OS")
  (printnotype t)
  (fo-single-symbol nat)
  (defn "lambda n(S) forall p(OS).[p zero and forall x(S). p x implies p [SUCC x]] implies p n"))

(def-binder foralln
  (var-type "S")
  (scope-type "O")
  (wff-type "O")
  (prefix 100)
  (fo-single-symbol foralln)
  (def-var "z(S)")
  (def-scope "A(O)")
  (defn "forall z(S). nat z implies A"))

(def-binder existsn
  (var-type "S")
  (scope-type "O")
  (wff-type "O")
  (prefix 100)
  (fo-single-symbol existsn)
  (def-var "z(S)")
  (def-scope "A(O)")
  (defn "exists z(S). nat z and A"))

(def-abbrev finite
  (type "O(OI)")
  (printnotype t)
  (fo-single-symbol finite)
  (defn "lambda p(OI) exists n(S). nat n and n p]"))

(def-abbrev <=
  (type "OSS")
  (infix 7)
  (printnotype t)
  (fo-single-symbol t)
  (defn "lambda x(S) lambda y(S) forall p(OS).[p x and forall z.p z implies p. succ z] implies p y"))

(def-abbrev mu
  (type "S(OS)")
  (printnotype t)
  (fo-single-symbol t)
  (defn "lambda p(OS) that x(S). nat x and p x and foralln y(S). p y implies x <= y"))

(def-binder mu-bind
  (var-type "S")
  (scope-type "O")
  (wff-type "O")
  (prefix 100)
  (fo-single-symbol mu-bind)
  (face mu)
  (def-var "z(S)")
  (def-scope "A(O)")
  (defn "mu(S(OS)) [lambda z(S) A]"))

(def-abbrev recursion
  (type "SSS(SSS)")
  (printnotype t)
  (fo-single-symbol t)
  (defn "lambda h(SSS) lambda g(S) lambda n(S) that m(S) forall w(OSS).
[w zero g and forall x(S) forall y(S). w x y implies w [succ x] [h x y]]
implies w n m"))
