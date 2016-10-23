;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of SKOLEMIZING)

(deffile wff-skolem
  (part-of skolemizing)
  (extension clisp)
  (mhelp "Wffops and Edops for Skolemizing a la S1 and S3."))

(context skolems)

(defun make-skolem-fn (var rev-sk-args)
  (let ((true-args (reverse rev-sk-args))
	new-type)
    (setq new-type
	  (do ((true-args true-args (cdr true-args))
	       (new-type (type var) (cons new-type (type (car true-args)))))
	      ((null true-args) new-type)))
    (do ((rev-args rev-sk-args (cdr rev-args))
	 (skolem-term (funcall name-skolem-fn var new-type)
		      (cons skolem-term (car rev-args))))
	((null rev-args) skolem-term))))


(defwffop simul-substitute-l-term-var
  (argtypes subst-alist gwff)
  (resulttype gwff)
  (argnames alist inwff)
  (arghelp "substitution list" "inwff")
  (applicable-q (lambda (alist gwff)
		  (declare (ignore gwff))
		  (dolist (pair alist T)
		    (if (not (type-equal (car pair) (cdr pair)))
			(return nil)))))
  (mhelp
   "Simultaneously substitute terms for the free occurrences of variables."))

(defun simul-substitute-l-term-var (alist inwff)
  (or (simul-substitute-l-term-var-rec alist inwff) inwff))

(defun simul-substitute-l-term-var-rec (alist inwff)
  (cond ((label-q inwff)
	 (apply-label inwff (simul-substitute-l-term-var-rec alist inwff)))
  	((lsymbol-q inwff)
	 (let ((new (assoc inwff alist)))
	   (if new (cdr new) nil)))
	((boundwff-q inwff)
	 (let ((bdvar (bdvar inwff)))
	   (if (position-if #'(lambda (pair)
				(and (not (eq (car pair) bdvar))
				     (not (eq (car pair) (cdr pair)))
				     (free-in bdvar (cdr pair))
				     (free-in (car pair) (cdr inwff))))
			    alist)
	       (let ((inwff (rename-bd-var inwff)))
		 (or (simul-substitute-l-term-var-rec alist inwff) inwff))
	       (let ((new (simul-substitute-l-term-var-rec
			   (acons bdvar nil alist) (cdr inwff))))
		 (if new (cons (car inwff) new) nil)))))
	(t (let ((left (or (simul-substitute-l-term-var-rec alist (car inwff))
			   (car inwff)))
		 (rt (or (simul-substitute-l-term-var-rec alist (cdr inwff))
			 (cdr inwff))))
	     (unless (and (eq left (car inwff)) (eq rt (cdr inwff)))
		     (cons left rt))))))

(defun simul-substitute-term-var (alist inwff)
  (or (simul-substitute-term-var-rec alist inwff) inwff))

(defun simul-substitute-term-var-rec (alist inwff)
  (cond ((label-q inwff)
	 (apply-label inwff (simul-substitute-term-var-rec alist inwff)))
  	((lsymbol-q inwff)
	 (let ((new (assoc inwff alist)))
	   (if new (cdr new) nil)))
	((boundwff-q inwff)
	 (let* ((bdvar (bdvar inwff))
		(new (simul-substitute-term-var-rec
		      (acons bdvar nil alist) (cdr inwff))))
	   (if new (cons (car inwff) new) nil)))
	(t (let ((left (or (simul-substitute-term-var-rec alist (car inwff))
			   (car inwff)))
		 (rt (or (simul-substitute-term-var-rec alist (cdr inwff))
			 (cdr inwff))))
	     (unless (and (eq left (car inwff)) (eq rt (cdr inwff)))
		     (cons left rt))))))


(defwffop skolems1
  (argtypes gwff0 boolean)
  (argnames gwff univflag)
  (arghelp "Wff to be Skolemized" "If NIL, existential quantifiers will be replaced")
  (defaultfns (lambda (&rest rest)
		(mapcar #'(lambda (argdefault arg) 
			    (if (eq arg '$) argdefault arg))
			'($ NIL) rest)))
  (resulttype gwff0)
  (mhelp "Skolemize a wff using method S1. See page 127 of Andrews' book.
   If equivalences are present, you must eliminate them first by REW-EQUIV."))

(defedop sk1
  (alias skolems1)
  (edwff-argname gwff)
  (result-> edwff))

(defun skolems1 (gwff univflag)
  (skolems1-main gwff univflag nil nil))

(defun skolems1-main (gwff univflag govuniv exsubs)
  (cond ((label-q gwff)
	 (apply-label gwff (skolems1-main gwff univflag govuniv exsubs)))
	((lsymbol-q gwff)
	 (cond ((cdr (assoc gwff exsubs)))
	       (t gwff)))
	((boundwff-q gwff)
	 (cond ((essexist (car gwff) univflag)
		(skolems1-main (cdr gwff) univflag
			       govuniv
			       (acons (caar gwff)
				      (simul-substitute-l-term-var
				       exsubs (make-skolem-fn (caar gwff)
							      govuniv))
				      exsubs)))
	       ((essuniv (car gwff) univflag)
		;; This part is incorrect and does not handle capturing of
		;; variables free in the substitution term.
		(cons (car gwff)
		      (skolems1-main (cdr gwff) univflag
				     (cons (caar gwff) govuniv)
				     (acons (caar gwff) (caar gwff) exsubs))))
	       (t (simul-substitute-l-term-var exsubs gwff))))
	(t (boolean-recursion-escape
	    skolems1-main gwff univflag (govuniv exsubs)
	    (simul-substitute-l-term-var exsubs gwff)))))



(defwffop skolems3
  (argtypes gwff0 boolean)
  (argnames gwff univflag)
  (arghelp "Wff to be Skolemized" "If NIL, existential quantifiers will be replaced")
  (defaultfns (lambda (&rest rest)
		(mapcar #'(lambda (argdefault arg) 
			    (if (eq arg '$) argdefault arg))
			'($ NIL) rest)))
  (resulttype gwff0)
  (mhelp "Skolemize a wff using method S3.  At the moment it takes only
those free variables which are universally quantified somewhere before,
all other variables are considered to be constants.
    See page 127 of Andrews' book.
    If equivalences are present, you must eliminate them first by REW-EQUIV."))

(defedop sk3
  (alias skolems3)
  (edwff-argname gwff)
  (result-> edwff))


(defun skolems3 (gwff univflag)
  (skolems3-main gwff univflag nil))

(defun skolems3-main (gwff univflag govuniv)
  (cond ((label-q gwff)
	 (apply-label gwff (skolems3-main gwff univflag govuniv)))
	((lsymbol-q gwff) gwff)
	((boundwff-q gwff)
	 (cond ((essexist (car gwff) univflag)
		(skolems3-main (substitute-l-term-var
				(make-skolem-fn
				 (caar gwff)
				 (setintersect govuniv (free-vars-of gwff)))
				(caar gwff)
				(cdr gwff))
			       univflag govuniv))
	       ((essuniv (car gwff) univflag)
		(cons (car gwff)
		      (skolems3-main (cdr gwff) univflag
				     (push (caar gwff) govuniv))))
	       (t gwff)))
	(t (boolean-recursion-escape
	    skolems3-main gwff univflag (govuniv) gwff))))

(defun name-skolem-cap (var new-type)
  (let* ((nameroot (nameroot var))
	 (face (or (get var 'face) (get (intern nameroot) 'face))))
    (funcall ren-var-fn
	     (intern (string-capitalize (if face (string (car face)) nameroot)))
	     new-type)))

(context misc-edops)

(defedop clause-form
  (alias clause-form)
  (result-> edwff)
  (edwff-argname gwff))
  
(defwffop clause-form
  (argtypes gwff)
  (argnames gwff)
  (arghelp "gwff")
  (resulttype gwff)
  (mhelp "Converts the given wff to clause form, as if the resulting wff is to
    be given to a resolution theorem prover.  The gwff is skolemized,
    rectified, etc."))

(defun clause-form (gwff)
  (let* ((gwff (ab-normalize (skolems1 (cons 'not gwff) nil)))
	 (free-vars (free-vars-of gwff)))
    (setq gwff (conjunctive-normal-form (openwffa gwff)))
    (dolist (var free-vars gwff)
      (setq gwff (acons var 'exists gwff)))))
