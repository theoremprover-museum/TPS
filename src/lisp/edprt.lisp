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
;;; File: EDPRT
;;;
;;; Defines the wff printing operations,
;;; as used in the editor.
;;;

(part-of wff-editor)

(deffile edprt
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Contains wff printing operations for editor."))

;*;(defcontext wff-printing
;*;  (short-id "Printing")
;*;  (order 31)
;*;  (mhelp "TPS objects which have to do with printing of wffs."))

(context wff-printing)

(defmacro defedprtop (op &rest props)
  `(defedop ,op
     ,@props
     (result-> ignore)
     (edwff-argname gwff)))

(push '(defedprtop . edop) global-definelist)

(defedprtop p (alias pw))

(defedprtop pp (alias ppw))

(defedprtop ps (alias pwscope))

(defedprtop pt (alias pwtypes))


