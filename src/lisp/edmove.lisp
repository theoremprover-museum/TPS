;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-EDITOR)

;;;
;;; File: EDMOVE
;;; 
;;; Defines editor moving commands assuming WFFMVE has been read.
;;;

(part-of wff-editor)

(deffile edmove
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Defines editor moving operations from wff operations."))

(context moving)

(defedop \0
  (alias move-up)
  (mhelp "Move up one-level, i.e., undo the last L, R, D,
or A command. Note that 0 stands for the numeral zero."))

(defedop ^
  (alias move-all-up)
  (mhelp "Move upwards through enclosing wffs all the way to the top."))

;;;
;;; MOVE-ALL-UP moves from the current wff to the global wff.
;;; It may be thought of as a sequence of 0's, until it can no longer be
;;; applied.  It is invoked with the ^ command.
;;;

(defun move-all-up ()
  (declare (special edwff cmdstack wffstack))
  (do ((temp-wff edwff (funcall (pop cmdstack) (pop wffstack) temp-wff)))
      ((or (null cmdstack) (null wffstack))
       (setq edwff temp-wff))))

;;; UNDO-LAST moves up one step without making changes.

(defun undo-last ()
  (declare (special cmdstack wffstack))
  (if wffstack
      (progn (pop cmdstack)
	     (pop wffstack))
      (throwfail "You are at the top and cannot back up.")))

(defedop undo
  (alias undo-last)
  (result-> edwff)
  (mhelp "Moves up (like 0), but throws away any editing since your last
downward moving command (typically A,D,L,or R."))

(defun xtr-func ()
  (declare (special cmdstack wffstack))
  (setq cmdstack nil)
  (setq wffstack nil))

(defedop xtr
  (alias xtr-func)
  (mhelp "Makes the current edwff the top wff."))

(defedop a
  (alias gar)
  (result-> edwff)
  (move-fn replace-gar)
  (edwff-argname gwff)
  (mhelp "for an expression like @wt{P x y}, delete the rightmost element;
in this example the result will be to make @wt{Px} the current expression.
For a quantified expression, it will move to the quantified variable."))


(defedop d
  (alias gdr)
  (result-> edwff)
  (move-fn replace-gdr)
  (mhelp "for an expression like @wt{P x y}, move to the rightmost element;
in this example @wt{y}.  For a quantified expression it will move
to the scope of the quantifier.")
  (edwff-argname gwff))

(defwffop glr
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (applicable-q infix-p)
  (applicable-p infix-p)
  (mhelp "Extract the left-hand side of an infix operator."))

(defwffop replace-glr
  (argtypes gwff gwff)
  (resulttype gwff)
  (argnames gwff newglrwff)
  (arghelp "gwff" "New glr")
  (applicable-q infix-p)
  (applicable-p infix-p)
  (replaces glr)
  (mhelp 
   "Replace the left-hand side of an infix operator non-destructively."))

(defun replace-glr (gwff newglrwff)
  (cond ((infix-p gwff) (replace-gar gwff
				     (replace-gdr (gar gwff) newglrwff)))
	(t (throwfail (gwff . gwff) " is not an infix formula."))))

(defedop l
  (alias glr)
  (result-> edwff)
  (move-fn replace-glr)
  (mhelp "for an infix-operator, move to the left argument.")
  (edwff-argname gwff))


(defun glr (gwff)
  (cond ((infix-p gwff) (gdr (gar gwff)))
	(t (throwfail (gwff . gwff) " is not an infix formula."))))

(defwffop grr
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (applicable-q infix-p)
  (applicable-p infix-p)
  (mhelp "Extract the right-hand side of an infix operator."))

(defwffop replace-grr
  (argtypes gwff gwff)
  (resulttype gwff)
  (argnames gwff newgrrwff)
  (arghelp "gwff" "New grr")
  (applicable-q infix-p)
  (applicable-p infix-p)
  (replaces grr)
  (mhelp 
   "Replace the right-hand side of an infix operator non-destructively."))

(defun replace-grr (gwff newgrrwff)
  (cond ((infix-p gwff) (replace-gdr gwff newgrrwff))
	(t (throwfail (gwff . gwff) " is not an infix formula."))))

(defedop r
  (alias grr)
  (result-> edwff)
  (move-fn replace-grr)
  (mhelp "for an infix-operator, move to the right argument.")
  (edwff-argname gwff))

(defun grr (gwff)
  (cond ((infix-p gwff) (gdr gwff))
	(t (throwfail (gwff . gwff) " is not an infix formula."))))

(defedop fi
  (alias find-infix)
  (result-> execute)
  (mhelp "Find an infix operator.")
  (edwff-argname gwff))

(defwffop find-infix
  (argtypes gwff)
  (resulttype ed-command)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Find an infix operator."))

(defun find-infix (gwff) (edsearch gwff 'infix-p))

(defedop fb
  (alias find-binder)
  (result-> execute)
  (mhelp "Find the first binder (left to right)")
  (edwff-argname gwff))

(defwffop find-binder
  (argtypes gwff)
  (resulttype edcommand)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Find the first binder (left to right)"))

(defun find-binder (gwff) (edsearch gwff (function boundwff-p)))

