;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of LAMBDA-CALC)

;;;
;;; File: EDLMBD
;;;
;;; defines editor operations connected with the lambda calculus
;;; (like LNORM, RED etc.)
;;;

(part-of lambda-calc)

(deffile edlmbd
  (part-of lambda-calc)
  (extension lsp)
  (mhelp "Contains operations on typed lambda-calculus."))

;*;(defcontext lambda-op
;*;  (short-id "Lambda-Calculus")
;*;  (order 112)
;*;  (mhelp "TPS object dealing with operations in the lambda-calculus."))

(context lambda-op)

(defedop lnorm
  (alias lnorm)
  (result-> edwff)
  (edwff-argname gwff))

(defedop lnorm-beta
  (alias lnorm-beta)
  (result-> edwff)
  (edwff-argname gwff))

(defedop lnorm-eta
  (alias lnorm-eta)
  (result-> edwff)
  (edwff-argname gwff))

(defedop ulnorm
  (alias untyped-lambda-norm)
  (result-> edwff)
  (edwff-argname gwff))

(defedop red
  (alias lcontr)
  (result-> edwff)
  (edwff-argname reduct))

(defedop lexp
  (alias lexpd)
  (result-> edwff)
  (edwff-argname inwff))

(defedop etac
  (alias etacontr)
  (result-> edwff)
  (edwff-argname gwff))

(defedop etan
  (alias etanorm)
  (result-> edwff)
  (edwff-argname gwff))

(defedop etax
  (alias eta-exp)
  (result-> edwff)
  (edwff-argname gwff))

(defedop leta
  (alias long-eta)
  (result-> edwff)
  (edwff-argname gwff))

(defedop etab
  (alias eta-to-base)
  (result-> edwff)
  (edwff-argname gwff))

(defedop abnorm
  (alias ab-normalize)
  (result-> edwff)
  (edwff-argname gwff))

(context neg-ops)

(defedop push-neg
  (alias push-negation)
  (result-> edwff)
  (edwff-argname gwff))

(defedop pull-neg
  (alias pull-negation)
  (result-> edwff)
  (edwff-argname gwff))

(defedop nnf
  (alias neg-norm)
  (result-> edwff)
  (edwff-argname gwff))

(defedop neg
  (alias negwff)
  (result-> edwff)
  (edwff-argname gwff))
