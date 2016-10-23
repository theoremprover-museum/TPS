;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)

(deffile mating-paths
  (part-of ms88)
  (extension lisp)
  (mhelp "Functions for finding mating paths."))

(defun find-next-openpath (jform path cgraph clist &optional (pathp t))
  (multiple-value-bind (path closed)
      (if path (values path t)
	  (find-cheapest-mspath jform nil cgraph clist pathp))
    (do ()
	((or (not (and closed path)) (numberp closed))
	 (if pathp path (values path closed)))
      (multiple-value-setq (path closed)
	  (find-alt-mspath (car path) (cdr path) cgraph clist pathp)))))

(defun complete-mspath (last-elt path cgraph clist &optional (pathp t))
  (let ((parent (jform-parent last-elt)))
    (if parent
        (if (eq (jform-type parent) 'conjunction)
            (complete-alt-mspath-and parent last-elt path cgraph clist pathp)
            (complete-mspath parent path cgraph clist pathp))
        path)))

(defun find-cheapest-mspath (jform path cgraph clist pathp)
  (case (jform-type jform)
    (literal
      (if (open-mspath-p jform path cgraph clist)
	  (if pathp (cons jform path)
	      (let ((name (find-potential-connection jform path cgraph clist)))
		(if name (values (cons jform path) name)
		    (cons jform path))))
	  (values (cons jform path) t)))
    (disjunction
      (if (disjunction-components jform)
	  (find-cheapest-mspath
	    (car (disjunction-components jform)) path cgraph clist pathp)
	  (values (cons jform path) t)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (multiple-value-bind (newpath closed)
		(find-cheapest-mspath conj path cgraph clist pathp)
	      (if closed (return (values newpath closed))
		  (setq path newpath))))
	  (cons (make-literal :represents t :parent jform :name (generate-a-fixed-name-for jform)) path)))
    (universal 
     (find-cheapest-mspath
      (universal-scope jform) path cgraph clist pathp))))

(defun find-alt-mspath (last-elt rem-path cgraph clist pathp)
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (multiple-value-bind (alt-path closed)
	      (let ((disj (cadr (memq last-elt (disjunction-components
                                                parent)))))
		(when disj (find-cheapest-mspath
			     disj rem-path cgraph clist pathp)))
	    (if closed (values alt-path closed)
		(if alt-path (complete-mspath
			       parent alt-path cgraph clist pathp)
		    (find-alt-mspath
		      parent rem-path cgraph clist pathp)))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq (jform-represents last-elt) t))
	      (find-alt-mspath parent rem-path cgraph clist pathp)
	      (find-alt-mspath
		(car rem-path) (cdr rem-path) cgraph clist pathp)))
        (universal (find-alt-mspath parent rem-path cgraph clist pathp))))))

(defun complete-alt-mspath-and (parent jform path cgraph clist pathp)
  (dolist (conj (cdr (memq jform (conjunction-components parent)))
                (complete-mspath parent path cgraph clist pathp))
    (multiple-value-bind (newpath closed)
        (find-cheapest-mspath conj path cgraph clist pathp)
      (if closed (return (values newpath closed))
          (setq path newpath)))))
