;;; -*- Mode:LISP; Package:CORE -*-
;;; Last Modified: Thu Jun 16 06:10:16 1988
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-PRINT)

;;;;
;;;; File: PRTOP
;;;; Author: fp
;;;;
;;;; This file contains wff operations which are called inside the formula
;;;; printing routines.  They are not intended to be called by the user.
;;;;

(part-of wff-print)

(deffile prtop
  (part-of wff-print)
  (extension lsp)
  (mhelp "Contains wff operations for printing only."))

(context print-internals)

(defwffop prt-symbol-p
  (argtypes gwff)
  (argnames gwff)
  (resulttype boolean)
  (mhelp "Decides if a given wff is printed a symbol."))

(defun prt-symbol-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (prt-symbol-p gwff)))
	((lsymbol-q gwff) t)
	(t nil)))


(defwffop prt-aplicn-p
  (argtypes gwff)
  (argnames gwff)
  (resulttype boolean)
  (mhelp "Decides if a given wff is not printed as a symbol."))

(defun prt-aplicn-p (gwff) (not (prt-symbol-p gwff)))


(defwffop prt-infix-op
  (argtypes gwff)
  (argnames gwff)
  (resulttype anything)
  (mhelp "Returns NIL, if the argument is not an infix operator,
its binding priority otherwise."))

(defflag infix-notation
  (flagtype boolean)
  (default T)
  (subjects printing printing-tex)
  (mhelp "If T, infix notation can be used for connectives and
abbreviations which have an INFIX property. If NIL, infix
notation is disallowed. (Note: If you set this to NIL, 
library objects saved with infix notation will become 
unreadable.  Also, if you set this to NIL, you should
also set PPWFFLAG to NIL since pretty-printing will 
not work properly without using infix notation.)"))

(context otl-printing)
(defwffop prw
  (argtypes gwff)
  (argnames gwff)
  (resulttype anything)
  (mhelp "Print real wff. Turns off special characters
(including FACE definitions), infix notation, and dot 
notation, and then prints the wff."))

(defmexpr prw
  (argtypes gwff)
  (argnames gwff)
  (mhelp "Print real wff. Turns off special characters
(including FACE definitions), infix notation, and dot 
notation, and then prints the wff."))

(defun prw (gwff)
  (let ((infix-notation nil)
	(use-dot nil)
	(style 'generic))
    (pwff gwff)))

(context print-internals)

(defun prt-infix-op (gwff)
  (if infix-notation
      (cond ((label-q gwff) (apply-label gwff (prt-infix-op gwff)))
	    ((lsymbol-q gwff)
	     (cond ((or (logconst-q gwff) (propsym-q gwff))
		    (get gwff 'infix))
		   ((or (pmpropsym-q gwff) (pmabbrev-q gwff))
		    (or (get gwff 'infix) (get (get gwff 'stands-for) 'infix)))
		   ((abbrev-q gwff)
		    (get gwff 'infix))))
	    (t nil))
    nil))

(defwffop prt-prefix-op
  (argtypes gwff)
  (argnames gwff)
  (resulttype anything)
  (mhelp "Returns NIL, if the argument is not a declared prefix operator,
its binding priority otherwise."))

(defun prt-prefix-op (gwff)
  (cond ((label-q gwff) (apply-label gwff (prt-prefix-op gwff)))
	((lsymbol-q gwff)
	 (cond ((or (logconst-q gwff) (propsym-q gwff))
		(get gwff 'prefix))
	       ((or (pmpropsym-q gwff) (pmabbrev-q gwff))
		(or (get gwff 'prefix) (get (get gwff 'stands-for) 'prefix)))
	       ((abbrev-q gwff)
		(get gwff 'prefix))))
	(t nil)))

(defwffop prt-associative-p
  (argtypes gwff)
  (argnames gwff)
  (resulttype boolean)
  (mhelp
   "Returns T, if gwff prints as an associative operator, NIL otherwise."))

(defun prt-associative-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (prt-associative-p gwff)))
	((lsymbol-q gwff)
	 (cond ((or (logconst-q gwff) (propsym-q gwff))
		(get gwff 'prt-associative))
	       ((or (pmpropsym-q gwff) (pmabbrev-q gwff))
		(or (get gwff 'prt-associative)
		    (get (get gwff 'stands-for) 'prt-associative)))
	       ((abbrev-q gwff)
		(get gwff 'prt-associative))))
	(t nil)))


