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

(deffile ml1-theorems
  (part-of math-logic-2-exercises)
  (extension lisp)
  (mhelp "Defines theorems with numbers X21nn."))

(setq base-type 'i)

;*;(defun allow-rulep (theorem command)
;*;  (or (not (member command '(advice go)))
;*;      (practice-p theorem)))

;*;(defun allow-rulep-help (theorem)
;*;  (if (practice-p theorem)
;*;      (msgf "All rules and commands are allowed.")
;*;      (msgf "You may not use ADVICE, but all other rules and commands are allowed.")))

;*;(defun allow-all (theorem command)
;*;  t)

;*;(defun allow-all-help (theorem)
;*;  (msgf "All rules and commands, including ADVICE, are allowed."))

;*;(defun disallow-rulep (theorem command)
;*;  (or (not (member command '(advice rulep)))
;*;      (and (equal command 'advice) (practice-p theorem))))

;*;(defun disallow-rulep-help (theorem)
;*;  (if (practice-p theorem)
;*;      (msgf "You may not use RULEP, but all other rules and commands are allowed.")
;*;      (msgf "You may not use RULEP or ADVICE, but all other rules and commands
;*;are allowed.")))

;*;(defcontext ml1-exercises
;*;  (short-id "First-Order Logic")
;*;  (order 91)
;*;  (mhelp "Having to do with exercises for first order logic."))

(context ml1-exercises)

(deftheorem X2106
  (assertion
   "
forall x [R x implies P x] and forall x [~Q x implies R x]
implies forall x.P x or Q x
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X2107
  (assertion
   "
R a b and forall x forall y [R x y implies.R y x and Q x y] and
forall u forall v [Q u v implies Q u u] implies.Q a a and Q b b
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X2108
  (assertion
   "
forall x exists y.P x implies P y
   ")
  (thm-type practice)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X2109
  (assertion
   "
exists x [p and Q x] equiv.p and exists x.Q x
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem X2110
  (assertion
   "
exists x R x and forall y [R y implies exists z Q y z] and
forall x forall y [Q x y implies Q x x] implies
exists x exists y.Q x y and R y
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2111
  (assertion
   "
forall x[exists y P x y implies forall y Q x y] and forall z exists y P z y
implies forall y forall x Q x y
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2112
  (assertion
   "
exists v forall x P x v and forall x[S x implies exists y Q y x] and
forall x forall y[P x y implies ~Q x y] implies exists u. ~S u
   ")
  (thm-type practice)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2113
  (assertion
   "
forall y exists w R y w and exists z forall x[P x implies ~R z x] implies
exists x ~P x
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2114
  (assertion
   "
forall x R x b and forall y [exists z R y z implies R a y] implies
exists u forall v R u v
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2115
  (assertion
   "
forall x [exists y P x y implies forall z P z z] and forall u exists v
[P u v  or. M u and Q.f u v ] and forall w[Q w implies ~ M.g w] implies
forall u exists v. P[g u]v and P u u
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2116
  (assertion
   "
forall x exists y[P x implies . R x[g.h y] and P y]
and forall w[P w implies . P[g w] and P[h w]] implies
forall x . P x implies exists y . R x y and P y
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2117
  (assertion
   "
forall u forall v[R u u equiv R u v ] and forall w forall z[R w w equiv R z w]
implies . exists x R x x implies forall y R y y
   ")
  (thm-type practice)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2118
  (assertion
   "
forall x [[p and Q x] or . ~p and R x] implies . forall x Q x or forall x R x
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2119
  (assertion
   "
exists y forall x . P y implies P x
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2120
  (assertion
   "
forall u forall v forall w [P u v  or P v w] implies exists x forall y.P x y
   ")
  (thm-type practice)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2121
  (assertion
   "
exists v forall y exists z. [P a y[h y] or P v y[f y]] implies P v y z
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2122
  (assertion
   "
[exists x R x x implies forall y R y y] implies exists u forall v.R u u implies R v v
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2123
  (assertion
   "
exists y[P y implies Q x] implies exists y. P y implies Q y
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2124
  (assertion
   "
exists x[P x implies Q x] equiv. forall x P x implies exists x Q x
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2125
  (assertion
   "
exists x forall y[P x equiv P y] equiv . exists x P x equiv forall y P y
   ")
  (thm-type practice)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2126
  (assertion
   "
forall x[P x equiv exists y P y] equiv . forall x P x equiv exists y P y
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2127
  (assertion
   "
exists x forall y[P y equiv P x] implies .forall x P x or forall x ~P x
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2128
  (assertion
   "
forall x [P x equiv forall y P y] equiv . exists x P x equiv forall y P y
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2129
  (assertion
   "
[exists x forall y[P x equiv P y] equiv . exists x Q x equiv forall y P y]
equiv . exists x forall y[Q x equiv Q y] equiv .
exists x P x equiv forall y Q y
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2130
  (assertion
   "
forall x P x implies .~exists y Q y or exists z. P z implies Q z
   ")
  (thm-type practice)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2131
  (assertion
   "
forall x P x implies exists y . forall x forall z Q x y z implies ~forall z
. P z and ~Q y y z
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2132
  (assertion
   "
forall w ~R w w implies exists x exists y. ~R x y and . Q y x implies 
forall z Q z z
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2133
  (assertion
   "
forall x[exists y Q x y implies P x] and forall v exists u Q u v  and 
forall w forall z[Q w z implies . Q z w or Q z z] implies forall z P z
   ")
  (thm-type practice)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2134
  (assertion
   "
forall z exists x[forall y P x y or Q x z] implies forall y exists x[P x y or Q x y]
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2135
  (assertion
   "
exists x forall y[P x and Q y implies . Q x or P y]
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2136
  (assertion
   "
exists x exists y forall u[P x y z implies P u x x]
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2137
  (assertion
   "
exists x forall y[P x implies . Q x or P y]
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(deftheorem x2138
  (assertion
   "
forall x exists y F x y and exists x forall e exists n forall w
[S n w implies D w x e] and
forall e exists d forall a forall b[D a b d implies forall y forall z.
F a y and F b z implies D y z e]
implies
exists y forall e exists m forall w. S m w implies forall z.F w z implies D z y e
   ")
  (thm-type exercise)
  (fol T)
  (allowed-cmd-p allow-rulep)
  (allowed-lemma-p allow-no-lemmas))

(defsavedwff x2200
  (represents "~ forall x R x u AND exists y . not forall z R y z IMPLIES Q u y"))

(defsavedwff x2201
  (represents "~ . exists x Q x y IMPLIES ~ forall z . forall u P u y z IMPLIES ~ exists v R v"))

(defsavedwff x2202
  (represents "forall x P x EQUIV exists y Q y"))

(defsavedwff x2203
  (represents "forall x ~ exists y forall u P x y IMPLIES ~ . exists z P x y implies forall y P u y"))

(defsavedwff x2204
  (represents "forall x [P x u or exists y . Q x y and ~ forall u R u z] implies forall z Q z u"))

(defsavedwff x2205
  (represents "forall w exists x [P w x IMPLIES forall y P y x] IMPLIES forall x Q x w"))

(defsavedwff x2206
  (represents "~ [[forall u P u z AND ~ exists x . Q u x or forall z ~ R x z] implies exists z Q u z]"))

(defsavedwff x2207 
  (represents "forall y [P y and exists y Q y] implies . forall x R x y implies exists z R z x"))

(defsavedwff x2208 
  (represents "forall x P x implies exists x . forall z Q x z implies exists y forall x R x y z"))

(defsavedwff x2209 
  (represents "forall z [ ~ exists x [P x and forall y R x y z] or forall z ~ . exists y Q y z implies P x]"))

(defsavedwff x2210
  (represents "forall u exists v ~ exists w exists x forall y forall z . [P w z or Q y] implies ~ R u x"))

(defsavedwff x2211 
  (represents "~[~ forall x [Q x or ~ forall y R y] or forall z Q z or ~ forall w R w]"))

(defsavedwff x2212
  (represents "forall x P x implies exists x . forall z Q x z implies exists y forall x R x y z"))

(defsavedwff x2213 
  (represents "forall z [~ exists x [P x and forall y R x y z] or forall z ~ . exists y Q y z implies P x]"))

(defsavedwff x2214
  (represents "forall x P x equiv exists y forall z Q x y z"))


