;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of JFORMS)

(deffile jforms
  (part-of jforms)
  (extension clisp)
  (mhelp "Jform-Wff conversion commands."))

(context jforms1)

(defwffop jform-to-gwff
  (argtypes jform)
  (argnames jform) 
  (resulttype gwff)
  (mhelp "Converts the given JFORM to GWFF. May not work with skolemized jforms."))

#+comment(defun jform-to-gwff (jform)
  (if (jform-pos jform) (jform-represents jform)
      (cons 'not (jform-represents jform))))


(defun jform-to-gwff (jform)
  (if (jform-represents jform)
      (if (jform-pos jform) (jform-represents jform) (cons 'not (jform-represents jform)))
;if jform-represents was filled in, we're OK. Otherwise we have to work.
  (case (jform-type jform)
	(conjunction (let ((components (conjunction-components jform))
			   (pos (jform-pos jform)))
		       (if (null components)
			   (if pos 'TRUTH '(cons 'NOT 'TRUTH))
			 (if pos (acons 'and (jform-to-gwff (car components))
					(jform-to-gwff-c 'and (cdr components)))
			   (cons 'not (acons 'and (jform-to-gwff (car components))
					     (jform-to-gwff-c 'and (cdr components))))))))
	(disjunction (let ((components (disjunction-components jform))
			   (pos (jform-pos jform)))
		       (if (null components)
			   (if pos 'FALSEHOOD (cons 'NOT 'FALSEHOOD))
			 (if pos (acons 'or (jform-to-gwff (car components))
					(jform-to-gwff-c 'or (cdr components)))
			   (cons 'not (acons 'or (jform-to-gwff (car components))
					     (jform-to-gwff-c 'or (cdr components))))))))
	(existential (if (jform-pos jform)
			 (append (mapcar #'(lambda (x) (cons x 'exists)) (existential-qvars jform))
				 (jform-to-gwff (existential-scope jform)))
		     (cons 'not 
			   (append (mapcar #'(lambda (x) (cons x 'exists)) (existential-qvars jform))
				   (jform-to-gwff (existential-scope jform))))))
	(universal (if (jform-pos jform)
		       (append (mapcar #'(lambda (x) (cons x 'forall)) (universal-qvars jform))
			       (jform-to-gwff (universal-scope jform)))
		     (cons 'not (append (mapcar #'(lambda (x) (cons x 'forall)) (universal-qvars jform))
					(jform-to-gwff (universal-scope jform))))))
	(literal  (if (jform-pos jform) (jform-represents jform)
		    (cons 'not (jform-represents jform)))))))

(defun jform-to-gwff-c (conn components)
  (if (cdr components) 
      (acons conn (jform-to-gwff (car components)) (jform-to-gwff-c conn (cdr components)))
    (jform-to-gwff (car components))))

(defwffop gwff-to-jform
  (argtypes gwff)
  (argnames gwff) 
  (resulttype jform)
  (applicable-p (lambda (gwff) (eq (type gwff) 'O)))
  (mhelp "Converts the given GWFF to JFORM."))

(defun gwff-to-jform (gwff &optional (pos t) (explore-quantifier T))
  (declare (special explore-quantifier))
  (car (gwff-to-jform-rec pos gwff nil)))

(defun gwff-to-jform-rec (pos gwff parent)
  (declare (special explore-quantifier))
  (cond ((label-q gwff)(apply-label gwff (gwff-to-jform-rec pos gwff parent)))
	((not-p gwff)
	 (gwff-to-jform-rec (not pos) (cdr gwff) parent))
	((if pos (and-p gwff) (or (or-p gwff) (implies-p gwff)))
	 (let ((sons (nconc (gwff-to-jform-rec
			      (if (or-p gwff) nil t) (glr gwff) 'and)
			    (gwff-to-jform-rec
			      (if (and-p gwff) t nil) (grr gwff) 'and))))
	   (if (eq parent 'and) sons
	       (let ((node (make-conjunction
			     :components sons :type 'conjunction
			     :represents gwff :pos pos)))
		 (dolist (son sons)
		   (setf (jform-parent son) node))
		 (list node)))))
	((or (if pos (or (or-p gwff) (implies-p gwff)) (and-p gwff))
	     (equiv-p gwff))
	 (let ((sons (nconc (gwff-to-jform-rec
			      (if (or (or-p gwff) (equiv-p gwff)) t nil)
			      (if (equiv-p gwff)
				  (if pos (acons 'and (glr gwff)
						 (grr gwff))
				      (acons 'and (glr gwff)
					     (cons 'not (grr gwff))))
				  (glr gwff))
			      'or)
			    (gwff-to-jform-rec
			      (if (and-p gwff) nil t)
			      (if (equiv-p gwff)
				  (if pos (acons 'and (cons 'not (glr gwff))
						 (cons 'not (grr gwff)))
				      (acons 'and (cons 'not (glr gwff))
					     (grr gwff)))
				  (grr gwff))
			      'or))))
	   (if (eq parent 'or) sons
	       (let ((node (make-disjunction
			     :components sons :type 'disjunction
			     :represents gwff :pos pos)))
		 (dolist (son sons)
		   (setf (jform-parent son) node))
		 (list node)))))
	((and explore-quantifier (ae-bd-wff-p gwff))
	 (let ((univ (if pos (a-bd-wff-p gwff) (e-bd-wff-p gwff))))
	   (multiple-value-bind (son qvars)
	       (gwff-to-jform-rec pos (cdr gwff)
				  (if univ 'univ 'exist))
	     (if (eq parent 'univ)
		 (if univ (values son (cons (bdvar gwff) qvars))
		     (let ((node (make-existential
				   :scope (car son) :pos pos
				   :represents gwff :type 'existential
				   :qvars (cons (bdvar gwff) qvars))))
		       (setf (jform-parent (car son)) node)
		       (list node)))
		 (if univ (let ((node (make-universal
					:scope (car son) :pos pos
					:represents gwff :type 'universal
					:qvars (cons (bdvar gwff) qvars))))
			    (setf (jform-parent (car son)) node) 
			    (list node))
		     (if (eq parent 'exist)
			 (values son (cons (bdvar gwff) qvars))
			 (let ((node (make-existential
				       :scope (car son) :pos pos
				       :represents gwff :type 'existential
				       :qvars (cons (bdvar gwff) qvars))))
			   (setf (jform-parent (car son)) node) 
			   (list node))))))))
	((or (and (eq gwff 'FALSEHOOD) pos)
	     (and (eq gwff 'TRUTH) (not pos)))
	 (list (make-disjunction :components nil :type 'disjunction
				 :represents gwff
				 :pos pos)))
	((or (and (eq gwff 'FALSEHOOD) (not pos))
	     (and (eq gwff 'TRUTH) pos))
	 (list (make-conjunction :components nil :type 'conjunction
				 :represents gwff
				 :pos pos)))
	(t (list (make-literal :type 'literal :represents gwff :pos pos)))))
	   

(defun get-jform (jform)
  (let ((global-type 'O))
    (declare (special global-type))
    (setq jform (funcall (get 'gwff 'getfn) jform))
    (gwff-to-jform jform)))

(defwffop gwff-to-prop-jform
  (argtypes gwff boolean)
  (argnames gwff pos) 
  (resulttype jform)
  (applicable-p (lambda (gwff) (eq (type gwff) 'O)))
  (mhelp "Converts the given GWFF (considered as a propositional GWFF) to JFORM."))

(defun gwff-to-prop-jform (gwff pos)
  (gwff-to-jform gwff pos nil))

