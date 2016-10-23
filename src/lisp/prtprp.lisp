;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-PRINT)

;;;
;;; File PRTPRP
;;; Author: fp
;;;

;;; This file defines the basic printing properties which are not
;;; specific to devices.  The category PRINTPROP is defined.
;;;

(part-of wff-print)

(deffile prtprp
  (part-of wff-print)
  (extension lsp)
  (mhelp "Defines basic printing properties and PRINTPROP category."))

(context wff-printing)


;;; Redefined 14FEB89 to fit changes in read-n-arg-fnspec. DAN
(eval-when (load compile eval)
(defcategory printprop
  (define defprintprop)
  (properties
   (printproptype single)
   (readfn singlefn)
   (mhelp single))
  (global-list global-printproplist)
  (mhelp-line "printing property")
  (mhelp-fn princ-mhelp))
)

(defun process-printing-property (prop-name prop-value log-symbol)
  (funcall (get prop-name 'readfn) prop-name prop-value log-symbol))

(defun printprop-default-readfn (prop-name printprop log-symbol)
  (declare (ignore log-symbol))
  (funcall (get (get prop-name 'printproptype) 'getfn)
	   (car printprop)))

(defprintprop printnotype
  (printproptype boolean)
  (readfn printprop-default-readfn)
  (mhelp "If T, types of the symbol will never be printed."))

(defprintprop prefix
  (printproptype integer+)
  (readfn printprop-default-readfn)
  (mhelp "The binding priority of a 'prefix operator'."))

(defprintprop infix
  (printproptype integer+)
  (readfn printprop-default-readfn)
  (mhelp "The binding priority of an infix operator."))

(defprintprop fo-single-symbol
  (printproptype boolean)
  (readfn printprop-default-readfn)
  (mhelp "If T, the symbol is special in first-order mode.
This will generally be the case for any new abbreviation."))

(defprintprop prt-associative
  (printproptype boolean)
  (readfn printprop-default-readfn)
  (mhelp "If T for an infix operator, it is assumed to be associative
for printing purposes."))

