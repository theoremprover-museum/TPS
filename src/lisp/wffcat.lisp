;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFFS)

;;;
;;; File: WFFCAT
;;;
;;; Allows definiton of objects in the diffenrent wff categories
;;;

(part-of wffs)

(deffile wffcat
  (part-of wffs)
  (extension lsp)
  (mhelp "Defines categories of objects in wffs like binders, abbreviations
etc., without defining any objects in those categories."))


(context prim-obj)

(defcategory typeconst
  (define def-typeconst)
  (properties
   (defn typesym)
   (mhelp single))
  (global-list global-typeconstlist)
  (mhelp-line "typeconstant")
  (mhelp-fn princ-mhelp))

(defcategory typeabbrev
  (define def-typeabbrev)
  (properties 
   (type-defn typesym)
   (mhelp single))
  (global-list global-typeabbrevlist)
  (mhelp-line "type abbreviation")
  (mhelp-fn princ-mhelp))

(defcategory logconst
  (define def-logconst)
  (properties
   (type typesym)
   (mhelp single))
  (global-list global-logconstlist)
  (other-prop process-printing-property)
  (mhelp-line "logical constant")
  (mhelp-fn princ-mhelp-defn))

(defcategory pmpropsym
  (define def-pmpropsym)
  (properties
   (type typesym)
   (typelist typesymlist)
   (mhelp single))
  (other-prop process-printing-property)
  (global-list global-pmpropsymlist)
  (mhelp-line "polymorphic proper symbol")
  (mhelp-fn princ-mhelp))

(defcategory binder
  (define def-binder)
  (properties
   (typelist typesymlist)
   (var-type typesym)
   (scope-type typesym)
   (wff-type typesym)
   (def-var gvar)
   (def-scope gwff)
   (defn gwff)
   (mhelp single))
  (global-list global-binderlist)
  (other-prop process-printing-property)
  (mhelp-line "binder")
  (mhelp-fn princ-mhelp-defn))

(defcategory abbrev
  (define def-abbrev)
  (properties
   (type typesym)
   (typelist typesymlist)
   (defn gwff)
   (defn-fun single)
   (mhelp single))
  (other-prop process-printing-property)
  (global-list global-abbrevlist)
  (mhelp-line "logical abbreviation")
  (mhelp-fn princ-mhelp-defn))

(defun princ-mhelp-defn (defn category)
  (princ-mhelp defn category)
  (if (get defn 'defn) (msgf ((get defn 'defn) . gwff)))
  (if (get defn 'typelist) 
      (msgf "Type variables are: " ((get defn 'typelist) . typesymlist)))
  (if (get defn 'prefix) 
      (msgf "Prefix binding priority is " (get defn 'prefix)))
  (if (get defn 'infix)
      (msgf "Infix binding priority is " (get defn 'infix))))

