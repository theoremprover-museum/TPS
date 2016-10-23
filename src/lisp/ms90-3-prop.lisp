;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :auto)

(deffile ms90-3-prop
  (part-of ms90-3)
  (extension lisp)
  (mhelp "More functions for MS90-3."))

(defvar *cgraph*)
(defvar *cgraph2*)

;;;Apparently this is the culprit causing the compilation bug.
;;;Try to find a manual for lucid cl, then figure out the reason.
;#-lucid
;(proclaim '(inline prop-find-cheapest-path))

(defun prop-setup-cgraph (jform)
  (declare (special temp-list))
  (case (jform-type jform)
    (literal
     (let* ((wff (jform-represents jform))
            (pos (jform-pos jform))
            (l (assoc wff temp-list :test #'wffeq-ab)))
       (if l (if pos
                 (progn (setf (gethash jform *cgraph*) (reverse (cddr l)))
                        (push jform (cadr l)))
                 (progn (setf (gethash jform *cgraph*) (reverse (cadr l)))
                        (push jform (cddr l))))
           (if pos (push (list wff (list jform)) temp-list)
               (push (list wff nil jform) temp-list)))))
    (conjunction (mapc #'prop-setup-cgraph (conjunction-components jform)))
    (disjunction (mapc #'prop-setup-cgraph (disjunction-components jform)))
    (t (throwfail "No action specified for " (jform-type jform)))))


(eval-when (load compile eval)
  (defmacro prop-check-conn (first path)
    `(let ((already-mated (gethash ,first *cgraph2* nil)))
       (dolist (second already-mated)
         (when (memq second ,path)
           (setf (literal-prop-class second) t)
           (return-from prop-find-cheapest-path (values t (cons ,first ,path)))))
       (let ((poss-mates (gethash ,first *cgraph* nil)))
         (dolist (second poss-mates (values nil (cons ,first ,path)))
           (when (memq second ,path)
             (setf (literal-prop-class second) t
                   (gethash ,first *cgraph2*) (cons second already-mated)
                   (gethash ,first *cgraph*) (delete second poss-mates
                                                     :test #'eq))
             (return (values (cons ,first second) (cons ,first ,path)))))))))

(defun prop-find-cheapest-path (jform path)
  (declare (notinline prop-find-cheapest-path))
  (let ((type (jform-type jform)))
    (cond ((eq type 'literal) (prop-check-conn jform path))
	  ((eq type 'conjunction)
	   (if (conjunction-components jform)
	       (let (conn)
		 (dolist (conj (conjunction-components jform) (values nil path))
			 (multiple-value-setq (conn path)
					      (prop-find-cheapest-path conj path))
			 (when conn (return (values conn path)))))
	     (cons (make-literal :represents t :parent jform :name (generate-a-fixed-name-for jform)) path)))
	  ((eq type 'disjunction)
	   (if (disjunction-components jform)
	       (prop-find-cheapest-path (car (disjunction-components jform)) path)
	     (values t (cons jform path)))))))

(defun prop-complete-alt-path (jform path)
  (let ((parent (jform-parent jform)))
    (if parent
        (let (conn)
          (dolist (conj (cdr (memq jform (conjunction-components parent))))
            (multiple-value-setq (conn path)
              (prop-find-cheapest-path conj path))
            (when conn (return-from prop-complete-alt-path (values conn path))))
          (setq parent (jform-parent parent))
          (if parent (prop-complete-alt-path parent path)
              (values nil path)))
        (values nil path))))

(defun prop-find-alt-path (last-elt path touchedp conjuncts)
  (let ((parent (jform-parent last-elt)))
    (when parent
	  (when (eq (jform-type parent) 'disjunction)
		(let ((disj (and touchedp
				 (cadr (memq last-elt (disjunction-components parent)))))
		      conn)
		  (when disj
			(multiple-value-setq (conn path)
					     (prop-find-cheapest-path disj path))
			(return-from prop-find-alt-path
				     (if conn (values conn path)
				       (prop-complete-alt-path parent path)))))
		(unless (or touchedp
			    (eq last-elt (car (disjunction-components parent))))
			(setf (disjunction-components parent)
			      (cons last-elt
				    (delete last-elt (disjunction-components parent)))))
		(setq last-elt parent
		      parent (jform-parent parent))
		(unless parent (return-from prop-find-alt-path nil)))
	  (if (eq last-elt (car-not-empty (conjunction-components parent)))
	      (if (eq parent (car conjuncts))
		  (prop-find-alt-path parent path t (cdr conjuncts))
		(if (jform-parent parent)
		    (prop-find-alt-path parent path touchedp conjuncts) nil))
	    (prop-find-alt-path (car path) (cdr path)
				(when (literal-prop-class (car path))
				      (setf (literal-prop-class (car path)) nil) t)
				(if (and touchedp (jform-parent parent)
					 (not (eq parent (car conjuncts))))
				    (cons parent conjuncts) conjuncts))))))

(defun prop-msearch (jform)
  (let ((*cgraph2* (make-hash-table :test #'eq))
        (*cgraph* (make-hash-table :test #'eq))
        (temp-list nil))
    (declare (special *cgraph* *cgraph2* temp-list))
    (setq jform (cnvrt-prop-jform jform nil))
    (msgf "This is the transformed jform being verified:")
    (display-vp-diag jform)
    (prop-setup-cgraph jform)
    (multiple-value-bind (conn path)
	(prop-find-cheapest-path jform nil)
      (do ((mating (if (eq conn t) nil (list conn))
                   (if (eq conn t) mating (cons conn mating))))
	  ((not (and path conn))
           (if path
               (progn
                 (msg t "The formula is not provable as there's no"
                      " connection on the following path:" t (mapcar 'show-prop-name path))
                 'fail)
               (values (cdr mating) jform)))
        (multiple-value-setq (conn path)
          (prop-find-alt-path (car path)
			      (cdr path) t nil))))))






