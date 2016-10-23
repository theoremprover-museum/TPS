;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of MATH-LOGIC-2-EXERCISES)

(deffile ml2-theorems
  (part-of math-logic-2-exercises)
  (extension lisp)
  (mhelp "Defines theorems x5200 to x6201."))

;;; First come the functions which check applications for legality

;*;(defun allow-all (theorem command)
;*;  t)

;*;(defun allow-all-help (theorem)
;*;  (msgf "All rules and commands are allowed."))

;*;(defcontext ml2-exercises
;*;  (short-id "Higher-Order Logic")
;*;  (order 92)
;*;  (mhelp "Having to do with exercises for higher order logic."))

(context ml2-exercises)

;;; Now come the exercises

(deftheorem X5200
  (assertion
"
x(OA) union y = setunion [lambda v.[v = x] or [v = y]]
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5201
  (assertion
"
x(OA) intersect y = setintersect [lambda v.[v = x] or
   [v = y]]
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5202
  (assertion
"
% f(AB) [x union y] = . [% f x] union [% f y]
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5203
  (assertion
"
% f(AB) [x intersect y] subset . [% f x] intersect [% f y]
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5204
  (assertion
"
% f(AB) [setunion w] = setunion .[% . % f] w
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5205
  (assertion
"
% f(AB) [setintersect w] subset . setintersect .[% . % f] w
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5206
  (assertion
   " % f(AB) [x union y] = . [% f x] union [% f y] ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (required-lemmas (x5200 x5204))
  (allowed-lemma-p only-required-lemmas))

;*;(deftheorem X5206
;*;  (assertion
;*;"
;*;     forall f(AB) forall w(O(OB))
;*;     [% f [setunion w] = setunion .[% . % f] w]
;*;  and forall f(OA(OB)) forall w(O(O(OB)))
;*;     [% f [setunion w] = setunion .[% . % f] w]
;*;  and forall x(OA) forall y(OA)
;*;   [x union y = setunion [lambda v(OA).[v = x] or [v = y]]]
;*;  and forall x(OB) forall y(OB)
;*;   [x union y = setunion [lambda v(OB).[v = x] or [v = y]]]
;*;implies  .% f(AB) [S(OB) union R(OB)] = . [% f S] union [% f R]
;*;  ")
;*;  (thm-type exercise)
;*;  (allowed-cmd-p allow-all)
;*;  (allowed-lemma-p allow-no-lemmas))

(deftheorem x5207
  (assertion
   "% f(AB) [x(OB) intersect y(OB)] subset .[% f x] intersect
        [% f y]")
  (thm-type exercise)
  (required-lemmas (x5201 x5205))
  (allowed-lemma-p only-required-lemmas))

;*;(deftheorem X5207
;*;  (assertion
;*;"
;*; forall f(AB) forall w(O(OB))
;*;     [% f [setintersect w] subset setintersect .[% . % f] w]
;*;  and forall f(OA(OB)) forall w(O(O(OB)))
;*;     [% f [setintersect w] subset setintersect .[% . % f] w]
;*;  and forall x(OA) forall y(OA)
;*;   [x intersect y = setintersect [lambda v(OA).[v = x] or [v = y]]]
;*;  and forall x(OB) forall y(OB)
;*;   [x intersect y = setintersect [lambda v(OB).[v = x] or [v = y]]]
;*;implies .% f(AB) [S(OB) intersect R(OB)] subset . [% f S] intersect [% f R]
;*;  ")
;*;  (thm-type exercise)
;*;  (allowed-cmd-p allow-all)
;*;  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5208
  (assertion
"
exists S(OI) forall x(I)[[[S x] or .P(OI) x] and .[NOT .S x] or .Q(OI) x] 
      equiv forall y(I) .[P y] or .Q y
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5209
  (assertion
"
powerset [D(OA) intersect E] = .[powerset D] intersect [powerset E]
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5210
  (assertion
"
[= x(A)] = . lambda z(A) exists y(A). [y = x] and. z = y
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5211
  (assertion
"
y(OA) = setunion. lambda z(OA) exists x(A). y x and .z = [= x]
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5212
  (assertion
"
[lambda z(A) exists x(B). g x and. z = f x] =. % f g
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5303
  (assertion
"
=(oaa) = lambda x(a) lambda y(a) forall p(oaa) . forall z(a) p z z implies p x y    
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5304
  (assertion
"
not exists g(OAA) forall f(OA) exists j(A) . g j = f
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5305
  (assertion
"
forall s not exists g forall f . f subset s implies exists j(A).
    s j and g j = f
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5308
  (assertion
"
EXISTS j(B(OB)) FORALL p(OB) [EXISTS x(B) p x IMPLIES p.j p] IMPLIES
.FORALL x(A) EXISTS y(B) r(OBA) x y EQUIV EXISTS f(BA) FORALL x r x.f x
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5309
  (assertion
"
~EXISTS h(I(OI)) FORALL p(OI) FORALL q(OI).h p = h q IMPLIES p = q
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X5310
  (assertion "FORALL r(OB(OB)) [FORALL x(OB) EXISTS y(B) [r x y] IMPLIES 
    EXISTS f(B(OB)) FORALL x(OB) .r x.f x] IMPLIES 
    EXISTS j(B(OB)) FORALL p(OB).EXISTS z(B) [p z] IMPLIES p.j p")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))


(deftheorem X5500
  (assertion
"
forall P(OB)[[exists x .P x] implies P .J P] implies
   forall f(AB) forall g(AB). f [J. lambda x(B). not. [f x] = .g x]
    = g [J . lambda x. not . [f x] = .g x] implies . f = g
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X6004
  (assertion
"
eqp [= x(B)] [= y(A)]
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X6101
  (assertion
"
one = sigma1
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X6104
  (assertion
"
 EXISTS i(O(AA)(AA)).FORALL g(AA) [i g [LAMBDA x(A) x] AND i g.LAMBDA x g.g x] AND FORALL f(AA) FORALL y(A).i [LAMBDA x y] f IMPLIES f y = y
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))


(deftheorem x6105
  (assertion
   "
FORALL n(O(OI)).NAT n IMPLIES FORALL q(OI).n q IMPLIES 
 EXISTS j(I(OI)) FORALL r(OI).r SUBSET q AND EXISTS x(I) r x IMPLIES r.j r
")
  (thm-type exercise)
  (mhelp "This is a lemma for X6106. 
You may need to ASSERT DESCR or T5310 or T5310A")
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))


(deftheorem x6106
  (assertion
   "
FINITE [LAMBDA x(I) TRUTH] IMPLIES 
 EXISTS j(I(OI)) FORALL r(OI).EXISTS x r x IMPLIES r.j r
")
  (thm-type exercise)
  (required-lemmas (x6105))
  (allowed-lemma-p only-required-lemmas))


(deftheorem X6201
  (assertion
"
exists r(OAA) forall x(A) forall y(A) forall z(A)
    [exists w(A) [r x w] and not [r x x] and
     .[r x y] implies .[r y z] implies .r x z]
   implies
    exists R(O(OA)(OA)) forall X(OA) forall Y(OA) forall Z(OA)
    [exists W(OA) [R X W] and not [R X X] and
     .[R X Y] implies .[R Y Z] implies .R X Z]
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x8030a
  (assertion
"
[g(OO) TRUTH AND g FALSEHOOD] = FORALL x(O) g x
  ")
  (thm-type exercise)
  (allowed-cmd-p allow-all)
  (allowed-lemma-p allow-no-lemmas))

