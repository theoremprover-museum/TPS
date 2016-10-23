;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of LOGIC-SCRIBE)

(part-of logic-scribe)

(deffile ml1-scribe
  (part-of logic-scribe)
  (extension lsp)
  (mhelp "Defines SCRIBE style characters for ml1."))

(context misc-symbols)

(defscribefont and
  (dfont "and"))

(defscribefont assert
  (dfont "assert"))

(defscribefont equiv
  (dfont "equiv"))

(defscribefont implies
  (dfont "implies"))

(defscribefont neg
  (dfont "neg"))

(defscribefont not
  (dfont "not"))

(defscribefont forall
  (dfont "forall"))

(defscribefont exists
  (dfont "exists"))

(defscribefont or
  (dfont "or"))

(defscribefont !
  (dfont "assert"))
