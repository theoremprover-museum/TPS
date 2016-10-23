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
;;; File: EDOPERA
;;;
;;; Miscellaneous editor operations.
;;;

(part-of wff-editor)

(deffile edopera
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Contains miscellaneous editor operations."))

(context ill-formed)

(defedop tp
  (alias type)
  (edwff-argname gwff))

(defedop dupw
  (alias dupwff)
  (result-> edwff)
  (edwff-argname gwff))
