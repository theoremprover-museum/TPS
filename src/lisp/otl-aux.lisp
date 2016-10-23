;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLRULES)

;;;
;;; File: OTL-Aux
;;;
;;; Auxiliary functions needed by the commands created by the rules package.

(part-of otlrules)

(deffile otl-aux
  (part-of otlrules)
  (extension lsp)
  (mhelp "Auxiliary functions needed by the commands created by the rules package."))

(defun specified-p (arg) (not (eq arg '$))) 

(defun existent-p (line) (numalias line))

(defun existent-label-p (line-label)
  (numalias (get line-label 'linenumber)))

(defun strong-hyp-p (hyps line dlineflag)
  (declare (ignore hyps))
  (and (specified-p line) (or (existent-p line) (not dlineflag))))

(defun hypsetdiff (hyplist singleton)
  (when (not (member singleton hyplist))
    (throwfail "Supposed hypothesis line " (singleton . line)
	       " does not include " (singleton . line) " in its hypotheses."))
  (remove singleton hyplist))

;;; The following functions on sets and lists were programmed si and
;;;  are from the file <sunil.tps>si-misc

;;;(set-eq set1 set2) - test whether set1 and set2 are equal (as sets).

(defun set-eq (set1 set2)
  (and (contained-p set1 set2) (contained-p set2 set1)))

;;;(ordered-join-h set list)  - delete every element in set which appears in
;;;list, return the union, making sure every element in list occurs in
;;;order and only once and after set. 

;*;(defun ordered-join-h (set list)
;*;  (cond ((null list) set)
;*;	(T
;*;	 (let ((join (reverse set)))
;*;	   (do ((element (car list) (car list))	
;*;		(list (cdr list) (cdr list)))
;*;	       ((null element))
;*;	     (cond ((member element join)
;*;		    (setq join (delete element join))))
;*;	     (push element join))
;*;	   (nreverse join)))))

(defun ordered-join-h (set list)
  (if list
      (let ((join (reverse set)))
	(dolist (element list)
	  (pushnew element join))
	(nreverse join))
      set))

;;;(contained-p set1 set2)  - return T if set1 is a subset of set2

;*;(defun contained-p (set1 set2)
;*;  (do ((element (car set1) (car set1))
;*;       (set1 (cdr set1) (cdr set1)))
;*;      ((not (and element (member element set2)))
;*;       (cond (element nil)(T T)))))

(defun contained-p (set1 set2)
  (dolist (element set1 T)
    (if (not (member element set2)) (return nil))))

;;;(join-h set-or-$ set)    - if set-or-$ is $, return set.  Otherwise
;;;return the union (as sets) of set-or-$ and set.  Order is irrelevant.

;*;(defun join-h (set-or-$ set)
;*;  (cond ((equal set-or-$ '$) set)
;*;	(T
;*;	 (let ((join set-or-$))
;*;	   (do ((element (car set) (car set))
;*;		(set (cdr set) (cdr set)))
;*;	       ((null element) join)
;*;	     (cond ((not (member element join))
;*;		    (push element join))))))))

(defun join-h (set-or-$ set)
  (if (equal set-or-$ '$) set
      (dolist (element (reverse set) set-or-$)
	(pushnew element set-or-$))))

;;;(meet-h set-or-$ set)    - if set-or-$ is $, return set.  Otherwise return
;;;the intersection (as sets) of set-or-$ and set.  Order is irrelevant.

;*;(defun meet-h (set-or-$ set)
;*;  (cond ((equal set-or-$ '$) set)
;*;	(T
;*;	 (let ((meet nil))
;*;	   (do ((element (car set) (car set))
;*;		(set (cdr set) (cdr set)))
;*;	       ((null element) meet)
;*;	     (cond ((member element set-or-$)
;*;		    (push element meet))))))))

(defun meet-h (set-or-$ set)
  (if (equal set-or-$ '$) set
      (let ((meet nil))
	(dolist (element set (nreverse meet))
	  (if (member element set-or-$)
	      (push element meet))))))

;;; Changed gentemp to gensym 9/5/87 DAN

(defun line-label-vec (n)
  (do ((n n (- n 1))
       (label-vec nil (cons (gensym "L") label-vec)))
      ((zerop n) label-vec)))

(defun subst-labels (alist linenumlist)
  (do ((linenumlist linenumlist (cdr linenumlist))
       (subst-list nil (cons (cond ((numalias (car linenumlist)))
				   ((cdr (assoc (car linenumlist) alist)))
				   (t (if (integerp (car linenumlist))
					  (throwfail t "Can't do that; there is no line " (car linenumlist))
					  (throwfail "The mapping from linenumbers to linelabels is broken."))))
			     subst-list)))
      ((null linenumlist) (nreverse subst-list))))

(defun hypnums (linenum)
  (mapcar #'linealias (get (numalias linenum) 'hypotheses)))


