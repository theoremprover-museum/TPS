;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLNL)

(deffile otl-typ
  (part-of otlnl)
  (extension lsp)
  (mhelp "Defines argument types for the outline package."))

(context proof-outline)

;most of the type definitions have been moved to argtyp.lisp; these are just the functions 
;that they use.. MB Wed Apr  9 13:35:27 1997

(defgwff-type line-number
  (checkfn line-number-wfftype-checkfn)
  (getfn get-line-assertion)
  (mhelp "Number : the assertion of a line in the current outline."))

(defun line-number-wfftype-checkfn (line-number)
  (and (fixp line-number) (numalias line-number)))

(defun get-line-assertion (line-number)
  (get (numalias line-number) 'assertion))

(defun get-existingline (line)
  (cond ((and (integerp line) (> line 0) (numalias line))
	 (numalias line))
	(T (throwfail line " is not an existing line."))))

(defun get-pline (linenum)
  (cond ((planp (numalias linenum)) (numalias linenum))
	(T (throwfail linenum " is not a planned line."))))

(defun getocclist (L)
  (cond ((eq L 'all) L)
	((and (atom L) L)
	 (cond ((and (INTEGERP L) (> L 0))
		(list L))
	       (t (throwfail "Item is not a number."))))
	((occlistp L) L)
	(t (throwfail "Not a  valid list of occurrences."))))

(defun occlistp (l)
  (or (eq l 'all)
      (and (consp l) (forall nr l (and (integerp nr) (> nr 0))))))

(defun print-line-range (line-range)
  (format t "~D--~D" (car line-range) (cdr line-range)))

(defun get-line-range (arg)
  (cond ((and (integerp arg) (> arg 0)) (cons arg arg))
	((symbolp arg)
	 (let* ((name (symbol-name arg))
		(dash-pos (position #\- name))
		(dash-pos2 (position #\- name :start (1+ dash-pos)))
		(first-int nil)
		(second-int nil)
		(end nil))
	   (if (and dash-pos dash-pos2 (= (1+ dash-pos) dash-pos2))
	       (progn 
		 (multiple-value-setq (first-int end)
		   (parse-integer name :start 0 :end dash-pos
				  :junk-allowed t))
		 (when (/= end dash-pos)
		   (throwfail arg " does not represent a line range."))
		 (when (null first-int)
		   (setq first-int (linealias (car (proof-lines dproof)))))
		 (multiple-value-setq (second-int end)
		   (parse-integer name :start (1+ dash-pos2)
				  :junk-allowed t))
		 (unless (= end (length name))
		   (throwfail arg " does not represent a line range."))
		 (when (null second-int)
		   (setq second-int 
		     (linealias (car (last (proof-lines dproof))))))
		 (if (and first-int second-int (<= first-int second-int))
		     (cons first-int second-int)
		   (throwfail arg " does not represent a valid line range.")))
	     (throwfail arg " does not represent a line range."))))
	(t nil)))
	     
