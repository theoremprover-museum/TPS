;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of wff-print)

(deffile prtcmd
  (part-of wff-print)
  (extension lsp)
  (mhelp "Contains printing commands and operations."))

(context wff-printing)

(defmacro defprtop (op &rest props)
  `(defwffop ,op
     (argtypes gwff)
     (argnames gwff)
     (arghelp "Print Wff")
     (resulttype ignore)
     (print-op t)
     ,@props))

(push '(defprtop . wffop) global-definelist)

(defprtop pw
  (mhelp "Print a wff using the global settings of all flags."))

(defun pw (gwff)
  (prtwff gwff (displaywff t)))

(defprtop pwscope
  (mhelp "Print a wff showing all brackets and dots."))

(defun pwscope (gwff)
  (prtwff gwff (displaywff t) (allscopeflag t)))

(defprtop pwtypes
  (mhelp "Print a wff showing types."))

(defun pwtypes (gwff)
  (prtwff gwff (displaywff t) (printtypes t)))

(defprtop ppw
  (mhelp "Pretty-print a wff."))

(defun ppw (gwff)
  (prtwff gwff (displaywff t) (ppwfflag t) (printdepth 0)))

; cebrown 5/10/02 - prints a table of symbols with reasonable tabs
(defun print-unordered-symbol-table (lst)
  (let ((short nil)
	(long nil)
	(cur 0))
    (do ((n 10 (+ n 10)))
	((null lst) (msg t))
	(dolist (a lst)
		(if (< (length (symbol-name a)) n)
		    (push a short)
		  (push a long)))
	(setq lst long)
	(setq long nil)
	(dolist (a short)
		(let* ((s (format nil "~d" a))
		       (l (length s)))
		  (if (< (+ cur l) RIGHTMARGIN)
		      (let ((tab (* (ceiling (/ (+ cur l) n)) n)))
			(msg s)
			(setq cur (+ cur l))
			(if (< tab RIGHTMARGIN)
			    (dotimes (i (- tab cur))
				     (incf cur)
				     (msg " "))
			  (progn
			    (msg t)
			    (setq cur 0))))
		    (if (= cur 0)
			(msg s t)
		      (let ((tab (* (ceiling (/ l n)) n)))
			(msg t s)
			(setq cur l)
			(if (< tab RIGHTMARGIN)
			    (dotimes (i (- tab cur))
				     (incf cur)
				     (msg " "))
			  (progn
			    (msg t)
			    (setq cur 0))))))))
	(setq short nil))))

