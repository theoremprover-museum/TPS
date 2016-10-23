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
;;; File WFFmacros
;;; Package: WFFS
;;;
;;; macros for wffs
;;;

(deffile wffmacros
  (part-of wffs)
  (extension clisp)
  (mhelp "Contains macros for wffs."))

(defmacro bdvar (gwff) `(caar ,gwff))

(defmacro binder (gwff) `(cdar ,gwff))

(defmacro bdhead (gwff) `(car ,gwff))

(context flavor-obj)

(defflag make-wffops-labels
  (flagtype boolean)
  (default nil)
  (subjects parsing)
  (mhelp "If T, meta labels are created by the parser, if NIL, wffops are
evaluated at parse-time."))

(context wff-printing)

(defflag retain-initial-type
  (flagtype boolean)
  (default t)
  (subjects printing)
  (mhelp
 "If T, type property is inherited from the previous occurrence (if any)
of the logical symbols. Else, it is modified whenever the parser encounters
a fresh occurrence."))
