;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;(part-of mating-search)
(part-of ms88)

(deffile mating-prop
;  (part-of mating-search)
  (part-of ms88)
  (mhelp "MS88 mating search for propositional cases."))

(defun find-open-msprop-push-path (jform clist)
  (declare (special clist))
  (let ((prop-classes nil))
    (declare (special prop-classes))
    (multiple-value-bind (path closed)
	(find-cheapest-msprop-push-path jform nil)
      (do ()
	  ((not (and closed path)) (or path clist))
	(multiple-value-setq (path closed)
	    (find-alt-msprop-push-path (car path) (cdr path)))))))

(defun find-cheapest-msprop-push-path (jform path)
  (declare (special prop-classes clist))
  (case (jform-type jform)
    (literal
      (let ((class (or (literal-prop-class jform)
		       (find-msprop-class jform))))
	(if (jform-pos jform)
	    (dolist (literal path (cons jform path))
	      (when (and (not (jform-pos literal))
			 (eq (literal-prop-class literal) class))
		(push (cons jform literal) clist)
		(return (values (cons jform path) T))))
	    (dolist (literal path (cons jform path))
	      (when (and (jform-pos literal)
			 (eq (literal-prop-class literal) class))
		(push (cons jform literal) clist)
		(return (values (cons jform path) T)))))))
    (disjunction
      (if (disjunction-components jform)
	  (find-cheapest-msprop-push-path
	    (car (disjunction-components jform)) path)
	  (values (cons jform path) t)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (multiple-value-bind (newpath closed)
		(find-cheapest-msprop-push-path conj path)
	      (if closed (return (values newpath closed))
		  (setq path newpath))))
	  (cons (make-literal :represents t :parent jform :name (generate-a-fixed-name-for jform)) path)))))

(defun find-alt-msprop-push-path (last-elt rem-path)
  (declare (special prop-classes clist))
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (multiple-value-bind (alt-path closed)
	      (let ((disj (cadr (member last-elt (disjunction-components
						   parent)
					:test #'eq))))
		(when disj
		  (find-cheapest-msprop-push-path disj rem-path)))
	    (if closed (values alt-path closed)
		(if alt-path (complete-alt-msprop-push-path-and
			       parent alt-path)
		    (find-alt-msprop-push-path parent rem-path)))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq (jform-represents last-elt) t))
	      (find-alt-msprop-push-path parent rem-path)
	      (find-alt-msprop-push-path (car rem-path) (cdr rem-path))))))))

(defun complete-alt-msprop-push-path-and (jform path)
  (declare (special prop-classes clist))
  (let ((parent (jform-parent jform)))
    (if parent
	(dolist (conj (cdr (member jform (conjunction-components parent)
				   :test #'eq))
		      (complete-alt-msprop-push-path-or parent path))
	  (multiple-value-bind (newpath closed)
	      (find-cheapest-msprop-push-path conj path)
	    (if closed (return (values newpath closed))
		(setq path newpath))))
	path)))

(defun complete-alt-msprop-push-path-or (jform path)
  (declare (special prop-classes clist))
  (let ((parent (jform-parent jform)))
    (if parent (complete-alt-msprop-push-path-and parent path)
	path)))

;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;

(defun find-open-msprop-pushnew-path (jform clist)
  (declare (special clist))
  (let ((prop-classes nil))
    (declare (special prop-classes))
    (multiple-value-bind (path closed)
	(find-cheapest-msprop-pushnew-path jform nil)
      (do ()
	  ((not (and closed path)) (or path clist))
	(multiple-value-setq (path closed)
	    (find-alt-msprop-pushnew-path (car path) (cdr path)))))))

(defun find-cheapest-msprop-pushnew-path (jform path)
  (declare (special prop-classes clist))
  (case (jform-type jform)
    (literal
      (let ((class (or (literal-prop-class jform)
		       (find-msprop-class jform))))
	(if (jform-pos jform)
	    (dolist (literal path (cons jform path))
	      (when (and (not (jform-pos literal))
			 (eq (literal-prop-class literal) class))
		(pushnew (cons jform literal) clist :test #'equal)
		(return (values (cons jform path) T))))
	    (dolist (literal path (cons jform path))
	      (when (and (jform-pos literal)
			 (eq (literal-prop-class literal) class))
		(pushnew (cons jform literal) clist :test #'equal)
		(return (values (cons jform path) T)))))))
    (disjunction
      (if (disjunction-components jform)
	  (find-cheapest-msprop-pushnew-path
	    (car (disjunction-components jform)) path)
	  (values (cons jform path) t)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (multiple-value-bind (newpath closed)
		(find-cheapest-msprop-pushnew-path conj path)
	      (if closed (return (values newpath closed))
		  (setq path newpath))))
      	  (cons (make-literal :represents t :parent jform :name (generate-a-fixed-name-for jform)) path)))))

(defun find-alt-msprop-pushnew-path (last-elt rem-path)
  (declare (special prop-classes clist))
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (multiple-value-bind (alt-path closed)
	      (let ((disj (cadr (member last-elt (disjunction-components
						   parent)
					:test #'eq))))
		(when disj
		  (find-cheapest-msprop-pushnew-path disj rem-path)))
	    (if closed (values alt-path closed)
		(if alt-path (complete-alt-msprop-pushnew-path-and
			       parent alt-path)
		    (find-alt-msprop-pushnew-path parent rem-path)))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq (jform-represents last-elt) t))
	      (find-alt-msprop-pushnew-path parent rem-path)
	      (find-alt-msprop-pushnew-path
		(car rem-path) (cdr rem-path))))))))

(defun complete-alt-msprop-pushnew-path-and (jform path)
  (declare (special prop-classes clist))
  (let ((parent (jform-parent jform)))
    (if parent
	(dolist (conj (cdr (member jform (conjunction-components parent)
				   :test #'eq))
		      (complete-alt-msprop-pushnew-path-or parent path))
	  (multiple-value-bind (newpath closed)
	      (find-cheapest-msprop-pushnew-path conj path)
	    (if closed (return (values newpath closed))
		(setq path newpath))))
	path)))

(defun complete-alt-msprop-pushnew-path-or (jform path)
  (declare (special prop-classes clist))
  (let ((parent (jform-parent jform)))
    (if parent (complete-alt-msprop-pushnew-path-and parent path)
	path)))

;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;;**;

(defun find-open-msprop-hashtable-path (jform cgraph max-cgraph-counter)
  (declare (special cgraph max-cgraph-counter))
  (let ((prop-classes nil))
    (declare (special prop-classes))
    (multiple-value-bind (path closed)
	(find-cheapest-msprop-hashtable-path jform nil)
      (do ()
	  ((not (and closed path)) path)
	(multiple-value-setq (path closed)
	    (find-alt-msprop-hashtable-path (car path) (cdr path)))))))

(defun find-cheapest-msprop-hashtable-path (jform path)
  (declare (special prop-classes cgraph max-cgraph-counter))
  (case (jform-type jform)
    (literal
      (let ((class (or (literal-prop-class jform)
		       (find-msprop-class jform))))
	(if (jform-pos jform)
	    (dolist (literal path (cons jform path))
	      (when (and (not (jform-pos literal))
			 (eq (literal-prop-class literal) class))
		(let ((conn (cons jform literal)))
		  (unless (gethash conn cgraph nil)
		    (setf (gethash conn cgraph) (incf max-cgraph-counter))))
		(return (values (cons jform path) T))))
	    (dolist (literal path (cons jform path))
	      (when (and (jform-pos literal)
			 (eq (literal-prop-class literal) class))
		(let ((conn (cons jform literal)))
		  (unless (gethash conn cgraph nil)
		    (setf (gethash conn cgraph) (incf max-cgraph-counter))))
		(return (values (cons jform path) T)))))))
    (disjunction
      (if (disjunction-components jform)
	  (find-cheapest-msprop-hashtable-path
	    (car (disjunction-components jform)) path)
	  (values (cons jform path) t)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (multiple-value-bind (newpath closed)
		(find-cheapest-msprop-hashtable-path conj path)
	      (if closed (return (values newpath closed))
		  (setq path newpath))))
      	  (cons (make-literal :represents t :parent jform :name (generate-a-fixed-name-for jform)) path)))))

(defun find-alt-msprop-hashtable-path (last-elt rem-path)
  (declare (special prop-classes cgraph max-cgraph-counter))
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (multiple-value-bind (alt-path closed)
	      (let ((disj (cadr (member last-elt (disjunction-components
						   parent)
					:test #'eq))))
		(when disj
		  (find-cheapest-msprop-hashtable-path disj rem-path)))
	    (if closed (values alt-path closed)
		(if alt-path (complete-alt-msprop-hashtable-path-and
			       parent alt-path)
		    (find-alt-msprop-hashtable-path parent rem-path)))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq (jform-represents last-elt) t))
	      (find-alt-msprop-hashtable-path parent rem-path)
	      (find-alt-msprop-hashtable-path
		(car rem-path) (cdr rem-path))))))))

(defun complete-alt-msprop-hashtable-path-and (jform path)
  (declare (special prop-classes cgraph max-cgraph-counter))
  (let ((parent (jform-parent jform)))
    (if parent
	(dolist (conj (cdr (member jform (conjunction-components parent)
				   :test #'eq))
		      (complete-alt-msprop-hashtable-path-or parent path))
	  (multiple-value-bind (newpath closed)
	      (find-cheapest-msprop-hashtable-path conj path)
	    (if closed (return (values newpath closed))
		(setq path newpath))))
	path)))

(defun complete-alt-msprop-hashtable-path-or (jform path)
  (declare (special prop-classes cgraph max-cgraph-counter))
  (let ((parent (jform-parent jform)))
    (if parent (complete-alt-msprop-hashtable-path-and parent path)
	path)))
