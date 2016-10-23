;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of semantics)

;;;
;;; File: semantics-macros - cebrown - 3/9/04
;;; Macros for semantics

(context subtoplevels)

(defvar *models-base-nbits* nil)
(defvar *models-var-interps* nil)
(defvar *models-binlr-tree* nil)

(deftoplevel models-top
  (top-prompt-fn models-top-prompt)
  (command-interpreter models-command-interpreter)
  (print-* models-print-*)
  (top-level-category modelscmd)
  (top-level-ctree models-command-ctree)
  (top-cmd-decode models-opdecode)
  (mhelp "The top level of MODELS."))

(eval-when (load compile eval)
(defcategory modelscmd
  (define defmodels)
  (properties
   (models-argtypes multiple)
   (models-argnames multiple)
   (models-arghelp multiple)
   (models-defaultfns multiplefns)
   (models-mainfns singlefn)
   (mhelp single))
  (global-list global-modelslist)
  (shadow t)
  (mhelp-line "models command")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command
	(format nil "@IndexOther(~A)" (symbol-name item))
	(get item 'models-argnames)
	(cdr (assoc 'modelscmd (get item 'mhelp))))))
  (mhelp-fn (modelscmd-mhelp modelscmd category))))

(context semantics)

(defflag MAX-DOMAIN-SIZE
  (flagtype integer+)
  (default 65536)
  (subjects semantic-bounds ms04-2)
  (mhelp "The maximum size of semantic domains TPS will consider. It does not
make sense to set this to any value other than a size such a domain
may have.  For example, the default value 2^16 is 65536.  Assuming
every base type is of size 2, the next reasonable value would be 2^32,
which is over 4 billion.  Consequently, the value of this flag should
not be changed until TPS is either considering models other than
standard models based on powers of 2 or computing power increases
tremendously."))

(defflag MAX-BINDER-COMPUTATION
  (flagtype integer+)
  (default 1048576)
  (subjects semantic-bounds ms04-2)
  (mhelp "The maximum number of elements TPS is willing to consider when
interpreting binders (quantifiers and lambdas) in a model.  This depends on the size of
domains and the nesting of binders in the formula."))
