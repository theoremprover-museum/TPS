;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-OPS2)

;;;
;;; File WFFLMBD-macros
;;;
;;; lambda conversion operations on wffs.
;;;

(deffile wfflmbd-macros
  (part-of wff-ops2)
  (extension clisp)
  (mhelp "Contains macros for lambda operations."))

(context lambda-op)

(defmacro lambda-contr (reduct)
  "This macro assumes that REDUCT is of the form [lambda x. e1] e2"
  `(substitute-l-term-var
    (cdr ,reduct) (gar (car ,reduct)) (gdr (car ,reduct))))
