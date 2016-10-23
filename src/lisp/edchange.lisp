;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
;;(part-of wff-ops1)

;;;
;;; File EDCHANGE
;;;
;;;operation to merge idempotent 
;;;

(deffile edchange
  (part-of wff-editor)
  (extension clisp)
  (mhelp "Contains editor operations to apply idempotent, commutative, 
associative laws, etc., to 'edwff'."))

(context changing)

(defedop mrg
  (alias merge-idempotent)
  (result-> edwff)
  (mhelp "Apply the following laws to a wff:
   A and A --> A
   A or A --> A
   A implies A --> TRUTH
   A and not A, not A and A --> FALSEHOOD
   A or not A, not A or A --> TRUTH
   A implies not A --> not A
   not A implies A --> A
   A equiv not A, not A equiv A --> FALSEHOOD.")
  (edwff-argname gwff))

(defedop cmrg
  (alias merge-constant)
  (result-> edwff)
  (mhelp "Delete the truth constants from a wff:
   A and TRUTH, TRUTH and A --> A
   A and FALSEHOOD, FALSEHOOD and A --> FALSEHOOD
   A or TRUTH, TRUTH or A --> TRUTH
   A or FALSEHOOD, FALSEHOOD or A --> A
   A implies TRUTH --> TRUTH
   TRUTH implies A --> A
   A implies FALSEHOOD --> not A
   FALSEHOOD implies A --> TRUTH
   A equiv TRUTH, TRUTH equiv A --> A
   A equiv FALSEHOOD, FALSEHOOD equiv A --> not A
   not TRUTH --> FALSEHOOD
   not FALSEHOOD --> TRUTH.")
  (edwff-argname gwff))

(defedop asrb
  (alias wff-absorb)
  (result-> edwff)
  (mhelp "Apply the following laws to a wff:
   A and (A or B), (A or B) and A --> A or B
   A and (B or A), (B or A) and A --> B or A
   A or (A and B), (A and B) or A --> A
   (B and A) or A, (B and A) or A --> A.")
  (edwff-argname gwff))

(defedop dist-exp
  (alias wff-dist-expand)
  (result-> edwff)
  (mhelp "Apply the distributivity laws to a wff in 
   the expanding direction:
   A and (B or C) --> (A and B) or (A and C)
   A or (B and C) --> (A or B) and (A or C)
   (B or C) and A --> (B and A) or (C and A)
   (B and C) or A --> (B or A) and (C or A).")
  (edwff-argname gwff))

(defedop dist-ctr
  (alias wff-dist-contract)
  (result-> edwff)
  (mhelp "Apply the distributivity laws to a wff in 
   the contracting direction:
  (A and B) or (A and C) --> A and (B or C)
  (A or B) and (A or C) --> A or (B and C)
  (B and A) or (C and A) --> (B or C) and A 
  (B or A) and (C or A) --> (B and C) or A.")
  (edwff-argname gwff))

(defedop assl
  (alias wff-associative-l)
  (result-> edwff)
  (mhelp "Apply the left associative law:
   A op (B op C) --> (A op B) op C.")
  (edwff-argname gwff))

(defedop assr
  (alias wff-associative-r)
  (result-> edwff)
  (mhelp "Apply the right associative law:
   (A op B) op C --> A op (B op C).")
  (edwff-argname gwff))

(defedop cmut
  (alias wff-commutative)
  (result-> edwff)
  (mhelp "Apply the commutative laws to a formula:
   A and B --> B and A
   A or B --> B or A
   A implies B --> not B implies not A
   A equiv B --> B equiv A.")
  (edwff-argname gwff))

(defedop pmut
  (alias wff-permute)
  (result-> edwff)
  (mhelp "Permute the two components of an infix operator:
   A op B --> B op A")
  (edwff-argname gwff))

(defedop dneg
  (alias wff-double-negation)
  (result-> edwff)
  (mhelp "Remove a double negation:
   not not A --> A.")
  (edwff-argname gwff))

(defedop subim
  (alias wff-sub-implies)
  (result-> edwff)
  (mhelp "Apply the following law to a formula:
   A implies B --> not A or B.")
  (edwff-argname gwff))

(defedop subeq
  (alias wff-sub-equiv)
  (result-> edwff)
  (mhelp "Apply the following law to a formula:
  A equiv B --> (A implies B) and (B implies A).")
  (edwff-argname gwff))

(defedop cntop
  (alias change-top)
  (result-> edwff)
  (mhelp "Change the top connective of a formula. For example,
\"cntop or\" will change \"A and B\" into \"A or B\";
\"cntop exists\" will change \"forall x P x\" into \"exists x P x\".")
  (edwff-argname gwff))

(defedop dl
  (alias delete-topconn-lscope)
  (result-> edwff)
  (edwff-argname gwff))

(defedop dr
  (alias delete-topconn-rscope)
  (result-> edwff)
  (edwff-argname gwff))

(context recursively-changing)

(defedop mrg*
  (alias merge-idempotent*)
  (result-> edwff)
  (mhelp "Recursively apply the following laws to a wff:
   A and A --> A
   A or A --> A
   A implies A --> TRUTH
   A equiv A --> TRUTH
   A and not A, not A and A --> FALSEHOOD
   A or not A, not A or A --> TRUTH
   A implies not A --> not A
   not A implies A --> A
   A equiv not A, not A equiv A --> FALSEHOOD.")
  (edwff-argname gwff))

(defedop cmrg*
  (alias merge-constant*)
  (result-> edwff)
  (mhelp "Recursively delete the truth constants in a wff:
   A and TRUTH, TRUTH and A --> A
   A and FALSEHOOD, FALSEHOOD and A --> FALSEHOOD
   A or TRUTH, TRUTH or A --> TRUTH
   A or FALSEHOOD, FALSEHOOD or A --> A
   A implies TRUTH --> TRUTH
   TRUTH implies A --> A
   A implies FALSEHOOD --> not A
   FALSEHOOD implies A --> TRUTH
   A equiv TRUTH, TRUTH equiv A --> A
   A equiv FALSEHOOD, FALSEHOOD equiv A --> not A
   not TRUTH --> FALSEHOOD
   not FALSEHOOD --> TRUTH.")
  (edwff-argname gwff))

(defedop asrb*
  (alias wff-absorb*)
  (result-> edwff)
  (mhelp "Recursively apply the following laws to a wff:
   A and (A or B), (A or B) and A --> A or B
   A and (B or A), (B or A) and A --> B or A
   A or (A and B), (A and B) or A --> A
   (B and A) or A, (B and A) or A --> A.")
  (edwff-argname gwff))

(defedop dist-exp*
  (alias wff-dist-expand*)
  (result-> edwff)
  (mhelp "Recursively apply the distributive laws to a wff in 
   the expanding direction:
   A and (B or C) --> (A and B) or (A and C)
   A or (B and C) --> (A or B) and (A or C)
   (B or C) and A --> (B and A) or (C and A)
   (B and C) or A --> (B or A) and (C or A).")
  (edwff-argname gwff))

(defedop dist-ctr*
  (alias wff-dist-contract*)
  (result-> edwff)
  (mhelp "Recursively apply the distributive laws to a wff in 
   the contracting direction:
  (A and B) or (A and C) --> A and (B or C)
  (A or B) and (A or C) --> A or (B and C)
  (B and A) or (C and A) --> (B or C) and A 
  (B or A) and (C or A) --> (B and C) or A.")
  (edwff-argname gwff))

(defedop assl*
  (alias wff-associative-l*)
  (result-> edwff)
  (mhelp "Recursively apply the left associative law:
   A op (B op C) --> (A op B) op C.")
  (edwff-argname gwff))

(defedop assr*
  (alias wff-associative-r*)
  (result-> edwff)
  (mhelp "Recursively apply the right associative law:
   (A op B) op C --> A op (B op C).")
  (edwff-argname gwff))

(defedop cmut*
  (alias wff-commutative*)
  (result-> edwff)
  (mhelp "Recursively apply the commutative laws to a formula:
   A and B --> B and A
   A or B --> B or A
   A implies B --> not B implies not A
   A equiv B --> B equiv A.")
  (edwff-argname gwff))

(defedop pmut*
  (alias wff-permute*)
  (result-> edwff)
  (mhelp "Recursively permute the two components of an infix operator:
   A op B --> B op A")
  (edwff-argname gwff))

(defedop dneg*
  (alias wff-double-negation*)
  (result-> edwff)
  (mhelp "Recursively remove double negations:
   not not A --> A.")
  (edwff-argname gwff))

(defedop subim*
  (alias wff-sub-implies*)
  (result-> edwff)
  (mhelp "Recursively apply the following law to a formula:
   A implies B  --> not A or B.")
  (edwff-argname gwff))

(defedop subeq*
  (alias wff-sub-equiv*)
  (result-> edwff)
  (mhelp "Recursively apply the following law to a formula:
   A equiv B --> (A implies B) and (B implies A).")
  (edwff-argname gwff))
