;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of SKOLEMIZING)

;;;
;;; File: Wff-Skolem-Mac
;;;

;(part-of development-seqs)

(deffile wff-skolem-mac
;  (part-of development-seqs)
  (part-of skolemizing)
  (extension lsp)
  (mhelp "Flags and Macros for Skolemizing."))

;*;(defcontext skolems
;*;  (short-id "Skolemizing")
;*;  (order 115)
;*;  (mhelp "Having to do with Skolem funtions and Skolemizing."))

(context skolems)

(defflag name-skolem-fn
  (flagtype symbol)
  (default name-skolem-cap)
  (subjects wff-prims)
  (mhelp "Name of the functions which names a Skolem function."))

