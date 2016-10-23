;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-PARSE)

;;;
;;; File WFFINM
;;;
;;; defines flags etc having to do with wff parsing
;;;

(part-of wff-parse)

(deffile wffinm
  (part-of wff-print)
  (extension lsp)
  (mhelp "Contains flags and macros for wff parsing."))

(context wff-parsing)

(defflag first-order-mode-parse
  (flagtype boolean)
  (default nil)
  (subjects parsing)
  (mhelp "If T, every letter by itself is a symbol for the parser,
with the exception of keywords like FORALL, AND etc.,
which can be in mixed case.  If NIL, symbols must be separated by
spaces (or brackets, dots, etc.)."))

(defflag lowercaseraise
  (flagtype boolean)
  (default nil)
  (subjects parsing)
  (mhelp "If T, lower case characters will be raised to upper case, when read.
Has no effect in first-order mode."))


(defflag type-iota-mode
  (flagtype boolean)
  (default t)
  (subjects parsing)
  (mhelp "If T, type variables are always assumed to be iota."))

(defflag base-type
  (flagtype symbol)
;  (default nil)
  (default I)
  (subjects parsing)
  (mhelp "If not NIL, it should be the `default' type for individual
variables in a logic system.  Typically I (for iota)."))


(defmacro bind-priority (l)
  `(cond ((consp ,l)
	  (get (cdr ,l) 'prefix))
	 ((get ,l 'infix))
	 ((get ,l 'prefix))
	 (t (throwfail "Operator " ,l	;*;(kwote ,l) 
				 " w/o priority."))))
