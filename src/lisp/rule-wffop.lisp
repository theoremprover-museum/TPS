;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of RULES)

;;;
;;; File: Rule-Wffop
;;; Package: Rules
;;;
;;; defines some argument types and wffops useful for the rules package
;;;

(part-of rules)


(deffile rule-wffop
  (part-of rules)
  (extension lsp)
  (mhelp "Defines some argument types and wffops useful for the rules package"))

(context rules-object)

(defflag build-match
  (flagtype boolean)
  (default t)
  (subjects rules-pack)
  (mhelp "If T, <rule>-MATCH functions for use with SUGGEST will be built."))






