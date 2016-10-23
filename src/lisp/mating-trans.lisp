;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)

(part-of mating-transform)


(deffile mating-trans
  (part-of mating-transform)
  (mhelp "Functions to check whether a mating is spanning."))

(defun old-spanning-clist-p (jform clist)
  (multiple-value-bind (path closed)
      (find-cheapest-spanning-clist-path jform nil clist)
    (do ()
	((not (and closed path)) (values (not path) path))
      (multiple-value-setq (path closed)
	  (find-alt-spanning-clist-path (car path) (cdr path) clist)))))

(defun spanning-clist-p (jform clist)
  (if *fps-succeeded*
      (multiple-value-bind (conns closed) 
			   (prop-msearch-silent jform)
			   (if (eq closed 'fail)
			       nil ;it can't be spanned
			     (if (mating-subset conns clist)
				 t ;conns is a subset of clist, so clist spans the jform
			       (old-spanning-clist-p jform clist))))
    (old-spanning-clist-p jform clist))) ;otherwise we use the old search.

(defun mating-subset (alist1 alist2)
  (if (and (null alist1) (null alist2)) t     ;we reached the end of both lists
    (if (null alist1) t
      (if (null alist2) nil  ;we reached the end of one list but not the other
      (mating-subset (filter-alist (car alist1) alist1) (filter-alist (car alist1) alist2))))))

#+comment(defun spanning-clist-p (jform clist)
  (multiple-value-bind (path closed)
      (find-cheapest-spanning-clist-path jform nil clist)
    (do ()
	((not (and closed path)) (values (not path) path))
      (multiple-value-setq (path closed)
	  (find-alt-spanning-clist-path (car path) (cdr path) clist)))))

(defun find-cheapest-spanning-clist-path (jform path clist)
  (case (jform-type jform)
    (literal
      (let ((jform-name (literal-name jform)))
	(dolist (literal path (cons jform path))
	  (when (or (member (cons jform-name (literal-name literal))
			    clist :test #'equal)
		    (member (cons (literal-name literal) jform-name)
			    clist :test #'equal))
	    (return (values (cons jform path) T))))))
    (disjunction
      (if (disjunction-components jform)
	  (find-cheapest-spanning-clist-path
	    (car (disjunction-components jform)) path clist)
;	  (values path t)
	  (values (cons jform path) t)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (multiple-value-bind (newpath closed)
		(find-cheapest-spanning-clist-path conj path clist)
	      (if closed (return (values newpath closed))
		  (setq path newpath))))
	  (cons (make-literal :represents t :parent jform :name (generate-a-fixed-name-for jform)) path)))))

(defun find-alt-spanning-clist-path (last-elt rem-path clist)
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (multiple-value-bind (alt-path closed)
	      (let ((disj (cadr (member last-elt (disjunction-components
						   parent)
					:test #'eq))))
		(when disj
		  (find-cheapest-spanning-clist-path disj rem-path clist)))
	    (if closed (values alt-path closed)
		(if alt-path (complete-alt-spanning-clist-path-and
			       parent alt-path clist)
		    (find-alt-spanning-clist-path parent rem-path clist)))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq (jform-represents last-elt) t))
	      (find-alt-spanning-clist-path parent rem-path clist)
	      (find-alt-spanning-clist-path
		(car rem-path) (cdr rem-path) clist)))))))

(defun complete-alt-spanning-clist-path-and (jform path clist)
  (let ((parent (jform-parent jform)))
    (if parent
	(dolist (conj (cdr (member jform (conjunction-components parent)
				   :test #'eq))
		      (complete-alt-spanning-clist-path-or parent path clist))
	  (multiple-value-bind (newpath closed)
	      (find-cheapest-spanning-clist-path conj path clist)
	    (if closed (return (values newpath closed))
		(setq path newpath))))
	path)))

(defun complete-alt-spanning-clist-path-or (jform path clist)
  (let ((parent (jform-parent jform)))
    (if parent (complete-alt-spanning-clist-path-and parent path clist)
	path)))
