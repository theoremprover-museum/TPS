;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-EDITOR)

;;;
;;; File: EDABB
;;;
;;; contains editor operations dealing with abbreviations (like INST, INST-1)
;;;

(part-of wff-editor)

(deffile edabb
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Contains editor operations for abbreviations."))

;*;(defcontext abbrev-ops
;*;  (short-id "Basic Abbreviations")
;*;  (order 110)
;*;  (mhelp "TPS objects having to do with logical abbreviations."))

(context abbrev-ops)

(defedop inst
  (alias instantiate-defn)
  (result-> edwff)
  (edwff-argname inwff))

(defedop install
  (alias instantiate-all)
  (result-> edwff)
  (edwff-argname inwff))

(defedop install-rec
  (alias instantiate-all-rec)
  (result-> edwff)
  (edwff-argname inwff))

(defedop abbr
  (alias abbr-list)
  (edwff-argname gwff))

(defedop lib-abbr
  (alias lib-abbr-list)
  (edwff-argname gwff))

(defedop new-defs
  (alias new-defs)
  (edwff-argname gwff))

(defedop constants
  (alias const-list)
  (edwff-argname gwff))

(defedop inst1
  (alias instantiate-1)
  (result-> edwff)
  (edwff-argname inwff))

(defedop expand=*
  (alias instantiate-equalities)
  (result-> edwff)
  (edwff-argname inwff))

(defedop expand=
  (alias instantiate-top-equality)
  (result-> edwff)
  (edwff-argname inwff))
