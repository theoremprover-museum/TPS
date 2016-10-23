;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of MATH-LOGIC-1-RULES)

(deffile ml1-prior
  (part-of math-logic-1-rules)
  (extension lisp)
  (mhelp "Defines priorities for hints given by ADVICE."))

(defmode math-logic-1-mode
  (flag-settings
   (first-order-mode-parse t)
   (type-iota-mode t)
   (first-order-print-mode t)
   (printtypes nil)
   (treat-hlines-as-dlines t)
   (default-wffeq wffeq))
  (mhelp "Mode to be used for Math Logic I."))


(defmacro defprior (rule priority)
  `(putprop ',rule ',priority 'priority))

(defmacro defhint (rule hint)
  `(putprop ',rule ',hint 'hint))

(defprior SAME 0)
(defhint SAME "A support line is the same as a planned line.")

(defprior CASES 1)
(defhint CASES "Some support line is a disjunction.")
(defprior ECONJ 1)
(defhint ECONJ "Some support line is a conjunction.")
(defprior RULEC 1)
(defhint RULEC "Some support line is existentially quantified.")
(defprior ICONJ 1)
(defhint ICONJ "Some planned line is a conjunction.")
(defprior UGEN 1)
(defhint UGEN "Some planned line is universally quantified.")
(defprior PULLNEG 1)
(defhint PULLNEG "Some planned line is negated.
   You could perhaps push in the negation.")
(defprior PUSHNEG 1)
(defhint PUSHNEG "Some support line is negated.
  You could perhaps push in the negation.")

(defprior EDEF 1)
(defhint EDEF "One of the support lines is a definition.")
(defprior IDEF 1)
(defhint IDEF "One of the planned lines is a definition.")

(defprior DEDUCT 2)
(defhint DEDUCT "Some planned line is an implication.")

(defprior EGEN 3)
(defhint EGEN "Some planned line is existentially quantified.
   Perhaps you should think about which term to use for existential
   generalization.")
(defprior UI 3)
(defhint UI "Some support line is universally quantified.
  Perhaps you should think about which term to use for universal
  instantiation.")

(defprior ABU 4)
(defhint ABU "Some planned line is universally quantified.
 If universal generalization is not applicable, perhaps you have to rename
 the bound variable.")
(defprior ABE 4)
(defhint ABE "Some support line is existentially quantified.
 If RuleC does not work, perhaps you have to rename the bound variable
 first.")

(defprior BACKCHAIN 4)
(defhint BACKCHAIN "Some support line is an implication.
  A likely possibility here would be to try to establish its antecedent and
  prove the planned line from its succedent (this method is called
  backchaining).")
(defprior INDIRECT 4)
(defhint INDIRECT "I can't see much beyond what I have already told you.
  Of course, one could always try an indirect proof.")
(defprior IDISJ-RIGHT 4)
(defhint IDISJ-RIGHT "There is a disjunction in the conclusion.
  I can't tell, but there is a chance you could prove the left disjunct.")
(defprior IDISJ-LEFT 4)
(defhint IDISJ-LEFT "There is a disjunction in the conclusion.
  I can't tell, but there is a chance you could prove the right disjunct.")

(defprior IMP-DISJ 5)
(defhint IMP-DISJ "Some support line is an implication.
  One possibility is to rewrite it as a disjunction (OR) and use the rule
  of cases.")

(defprior DISJ-IMP 8)
(defhint DISJ-IMP "Sometimes it shortens a proof to rewrite an disjunction
as an implication, but as a random attempt it's not very likely road
to success.")

(defprior LEMMA 9)
(defhint LEMMA "Using a lemma is mostly unnecessarily complicated for simple problems.
  But if you have a good idea what that lemma should be, go ahead.")

(defprior HYP 10)
(defhint HYP "One can always introduce a new hypothesis.
But it rarely turns out to be useful, because it's hard to get rid of it.")

;;; Because our MATCH functions aren't sophisticated enough,
;;; we leave the following without a priority for the moment.  They
;;; will never be suggested by ADVICE or GO.

(defprior MP nil)

(defprior AB* nil)
(defprior SUBST-TERM nil)
(defprior SUBST-WFF nil)

;*;(defcontext rules-1-misc
;*;  (short-id "Miscellaneous Rules"))

;*;(defcontext rules-2-prop
;*;  (short-id "Propositional Rules"))

;*;(defcontext rules-4-quant
;*;  (short-id "Quantifier Rules"))

;*;(defcontext rules-3-neg
;*;  (short-id "Negation Rules"))

;*;(defcontext rules-7-defn
;*;  (short-id "Definition Rules"))

;*;(defcontext rules-5-subst
;*;  (short-id "Substitution Rules"))

;;; Now comes a rule which was not formulated in the RULES package.

(context rules-1-misc)

(defmexpr assert
  (argtypes theorem line)
  (argnames theorem line)
  (arghelp "Name of Theorem" "Line with Theorem Instance")
  (mainfns assert-legal assert%)
  (mhelp "Use a theorem as a lemma in the current proof.
The help message for each exercise says what theorems can be asserted.
Find these using the PROBLEMS command.  See the \"Book Theorems\"
at the end of the problems list.  In proofs of theorems that are
not exercises, one can also assert a theorem from the library.

If the line already exists, ETPS will check whether it is a legal
instance of the theorem schema, otherwise it will prompt for the
metavariables in the theorem schema (usually x or P, Q, ...)."))
