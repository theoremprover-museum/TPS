;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of TPS2-RULEP)

(deffile newrulep-tsts
  (part-of tps2-rulep)
  (extension lisp)
  (mhelp "Functions testing for validity and satisfiability."))

(context rulep-test)

(defun valid-p (jform)
  (cond ((jform-p jform) (setq jform (jform-to-gwff jform))))
  (valid-p-main (local-gwff-to-jform jform nil)))

(defun local-gwff-to-jform (gwff pos)
  (let ((lit-name 'vpd-int-lit))
    (declare (special lit-name))
    (reset-name-counter lit-name)
    (gwff-to-prop-jform gwff pos)))

(defun init-prop (jform)
  (let ((classes nil))
    (declare (special classes))
    (init-prop-rec jform)))

;;;Since We have added more values to ORDER-COMPONENTS,
;;;The following function needs to be changed in a way
;;;I don't know yet. (HX June 14th, 1993)

(defun init-prop-rec (jform)
  (case (jform-type jform)
    (literal nil)
    (conjunction
      (mapc #'init-prop-rec (conjunction-components jform))
      (setf (jform-num-paths-below jform)
	    (or (apply-fn-with-key #'* (conjunction-components jform) 
				   #'jform-num-paths-below)
		1))
      (setf (conjunction-components jform)
            (funcall (get order-components 'tree-sorting) 
                     (conjunction-components jform))))
    (disjunction
      (mapc #'init-prop-rec (disjunction-components jform))
      (setf (jform-num-paths-below jform)
	    (or (apply-fn-with-key #'+ (disjunction-components jform) 
				   #'jform-num-paths-below)
		0))
      (setf (disjunction-components jform)
            (funcall (get order-components 'tree-sorting) 
                     (disjunction-components jform))))
    (t (throwfail
	 (jform-type jform) " is illegal in propositional wffs."))))


#+comment(defun valid-p-main (jform)
  (init-prop jform)
  (not (find-open-rulep-path jform)))

(defun valid-p-main (jform) 
  (if (and (fboundp '*fps-succeeded*) *fps-succeeded*)
      (multiple-value-bind (a b) (prop-msearch-silent jform)
			   (declare (ignore a))
			   (neq b 'fail))
    (progn
      (init-prop jform)
      (not (find-open-rulep-path jform)))))

(defun sat-p-main (jform)
  (init-prop jform)
  (find-open-rulep-path jform))

;(defun find-msprop-class (literal)
;  (declare (special prop-classes))
;  (let ((gwff (jform-represents literal)))
;    (dolist (cl prop-classes
;		(progn (push (cons gwff (gensym)) prop-classes)
;		       (setf (literal-prop-class literal) (cdar prop-classes))
;		       (cdar prop-classes)))
;      (when (funcall rulep-wffeq gwff (car cl))
;	(setf (literal-prop-class literal) (cdr cl))
;	(return (cdr cl))))))

(defun find-msprop-class (literal)
  (declare (special prop-classes))
  (let ((gwff (jform-represents literal)))
    ;; if the gwff has any initial negations, we remove them before testing
    ;; for the prop class, and update the pos slot of the literal 
    ;; appropriately.  Needed for mating-search, but not rulep. DAN 2MAR89
    (do ((gwff* gwff (gdr gwff*))
	 (pos (jform-pos literal) (not pos)))
	((not (not-p gwff*))
	 (setf (jform-pos literal) pos)
	 (setq gwff gwff*)))
    (dolist (cl prop-classes
		(progn (push (cons gwff (gensym)) prop-classes)
		       (setf (literal-prop-class literal) (cdar prop-classes))
		       (cdar prop-classes)))
      (when (funcall rulep-wffeq gwff (car cl))
	(setf (literal-prop-class literal) (cdr cl))
	(return (cdr cl))))))

(defun remove-msprop-class (jform)
  "Sets the PROP-CLASS attribute of LITERALS in JFORM to NIL."
  (case (jform-type jform)
    (literal (setf (literal-prop-class jform) nil))
    (disjunction (mapc #'remove-msprop-class (disjunction-components jform)))
    (conjunction (mapc #'remove-msprop-class (conjunction-components jform)))))

(defun find-open-rulep-path (jform)
  (if (and (conjunction-p jform) (not (conjunction-components jform))) t
      (let ((prop-classes nil))
	(declare (special prop-classes))
	(multiple-value-bind (path closed)
	    (find-cheapest-rulep-path jform nil)
	  (do ()
	      ((not (and closed path)) path)
	    (multiple-value-setq (path closed)
		(find-alt-rulep-path (car path) (cdr path))))))))

;(defun find-cheapest-rulep-path (jform path)
;  (declare (special prop-classes))
;  (case (jform-type jform)
;    (literal
;      (let ((class (or (literal-prop-class jform)
;		       (find-msprop-class jform))))
;	(if (jform-pos jform)
;	    (dolist (literal path (cons jform path))
;	      (when (and (not (jform-pos literal))
;			 (eq (literal-prop-class literal) class))
;		(return (values (cons jform path) T))))
;	    (dolist (literal path (cons jform path))
;	      (when (and (jform-pos literal)
;			 (eq (literal-prop-class literal) class))
;		(return (values (cons jform path) T)))))))
;    (disjunction
;      (if (disjunction-components jform)
;	  (find-cheapest-rulep-path (car (disjunction-components jform)) path)
;	  (values path t)))
;    (conjunction
;      (dolist (conj (conjunction-components jform) path)
;	(multiple-value-bind (newpath closed)
;	    (find-cheapest-rulep-path conj path)
;	  (if closed (return (values newpath closed))
;	      (setq path newpath)))))))

;;; Changed to have mating found put in variable *rulep-mate-list* as
;;; as side effect whenever that symbol is bound.  Used in translating
;;; nat deductions to eproofs. 01OCT88 DAN

(defun find-cheapest-rulep-path (jform path)
  (declare (special prop-classes *rulep-mate-list*))
  (case (jform-type jform)
    (literal
      (let ((class (or (literal-prop-class jform)
		       (find-msprop-class jform))))
	(if (jform-pos jform)
	    (dolist (literal path (cons jform path))
	      (when (and (not (jform-pos literal))
			 (eq (literal-prop-class literal) class))
		    (when (boundp '*rulep-mate-list*)
			  (push (cons (literal-name jform) 
				      (literal-name literal))
				*rulep-mate-list*))
		    (return (values (cons jform path) T))))
	    (dolist (literal path (cons jform path))
	      (when (and (jform-pos literal)
			 (eq (literal-prop-class literal) class))
		    (when (boundp '*rulep-mate-list*)
			  (push (cons (literal-name jform) 
				      (literal-name literal))
				*rulep-mate-list*))
		(return (values (cons jform path) T)))))))
    (disjunction
      (if (disjunction-components jform)
	  (find-cheapest-rulep-path (car (disjunction-components jform)) path)
	  (values (cons jform path) t)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (multiple-value-bind (newpath closed)
		(find-cheapest-rulep-path conj path)
	      (if closed (return (values newpath closed))
		(setq path newpath))))
	(let ((leaf-name (if (boundp 'leaf-name)
			     leaf-name
			   'LEAF)))	; added this so ETPS can use the function generate-a-fixed-name-for - cebrown 10/28/00
	  (declare (special leaf-name))
	  (cons (make-literal :represents t :parent jform :name (generate-a-fixed-name-for jform)) path))))))

					; moved this here from mating-paths.lisp so ETPS can use it.  - cebrown 10/28/00
(defun generate-a-fixed-name-for (jform)
  (declare (special leaf-name))
  ;;jform is an empty conjunction. We want to give it a fixed literal name, otherwise
  ;;we'll get a new name every time we look at it, and we won't realise we've seen it before.
  (do ((jf jform (jform-parent jf))
       (countlist (list 1)))
      ((null (jform-parent jf))
       (intern (apply #'concatenate (cons 'string (mapcar #'princ-to-string (cons leaf-name (cons T countlist)))))))
    (when (jform-parent jf)
      (case (jform-type (jform-parent jf))
	(conjunction (push (position jf (conjunction-components (jform-parent jf))) countlist))
	(disjunction (push (position jf (disjunction-components (jform-parent jf))) countlist))
	(t (push 1 countlist)))))) ; must be a universal node.
      

(defun find-alt-rulep-path (last-elt rem-path)
  (declare (special prop-classes))
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (multiple-value-bind (alt-path closed)
	      (let ((disj (cadr (member last-elt (disjunction-components
						   parent)
					:test #'eq))))
		(when disj
		  (find-cheapest-rulep-path disj rem-path)))
	    (if closed (values alt-path closed)
		(if alt-path (complete-alt-rulep-path-and parent alt-path)
		    (find-alt-rulep-path parent rem-path)))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq (jform-represents last-elt) t))
	      (find-alt-rulep-path parent rem-path)
	      (find-alt-rulep-path (car rem-path) (cdr rem-path))))))))

(defun complete-alt-rulep-path-and (jform path)
  (declare (special prop-classes))
  (let ((parent (jform-parent jform)))
    (if parent
	(dolist (conj (cdr (member jform (conjunction-components parent)
				   :test #'eq))
		      (complete-alt-rulep-path-or parent path))
	  (multiple-value-bind (newpath closed)
	      (find-cheapest-rulep-path conj path)
	    (if closed (return (values newpath closed))
		(setq path newpath))))
	path)))

(defun complete-alt-rulep-path-or (jform path)
  (declare (special prop-classes))
  (let ((parent (jform-parent jform)))
    (if parent (complete-alt-rulep-path-and parent path)
	path)))
