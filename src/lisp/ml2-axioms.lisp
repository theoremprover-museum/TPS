;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

;;(part-of math-logic-2-wffs)

(deffile ml2-axioms
  (part-of math-logic-2-rules)
  (extension lisp)
  (mhelp "Axioms REFL=, SYM=, DESCR, EXT, etc."))

(defmacro defaxiom (&rest l)
  `(deftheorem ,@l (thm-type book)))

(context book-theorems)

(defaxiom refl=
  (assertion "A(A) = A(A)")
  (mhelp "Reflexivity of Equality."))

(defaxiom sym=
  (assertion "A(A) = B implies B = A")
  (mhelp "Symmetry of Equality."))

;(defaxiom descr
;  (assertion "IOTA [= Y(OA)] = Y")
;  (mhelp "Axiom of description at all types."))

(defaxiom descr
  (assertion "IOTA [= Y(A)] = Y")
  (mhelp "Axiom of description at all types."))

(defaxiom ext
  (assertion "forall x [f(AB) x = g x] implies f = g")
  (mhelp "Axiom of extensionality at all types."))

(defaxiom ext-leib
  (assertion "forall f(ab) forall g(ab) . forall x [f(AB) x = g x] implies . forall q(O(AB)) . q f IMPLIES q g")
  (mhelp "Extensional equality of f and g implies Leibniz equality of f and g."))

(defaxiom T5310
  (assertion
   "
FORALL z(A) [p(OA) z EQUIV y(A) = z] IMPLIES IOTA p = y
")
  (mhelp "Theorem about descriptions."))


(defaxiom T5310A
  (assertion
   "
FORALL z(A) [p(OA) z EQUIV z = y(A) ] IMPLIES IOTA p = y
")
  (mhelp "Theorem about descriptions."))

(defaxiom T5302
  (assertion
   "
[x(A) = y(A)] = .y = x
")
  (mhelp "Symmetry of Equality."))

