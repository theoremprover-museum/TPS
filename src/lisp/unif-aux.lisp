;;; -*- Mode: Lisp -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile unif-aux
  (part-of unification)
  (extension lisp)
  (mhelp "Functions used by unification routines."))

(defun print-subst-stack (subst-stack)
  (vpwin-update-two subst-stack)
  (when (eq unif-counter-output 10) (print-unif-stats) (msg t))
  (msg T "Substitution Stack:" T)
  (let ((h-vars nil))
    (dolist (subst (reverse subst-stack))
      (when (not (memq (car subst) h-vars))
	(msg t ((car subst) . gwff) 3 "->" 3
	     ((lambda-reduce-subst (cdr subst) subst-stack) . gwff)))
      (setq subst (cdr subst))
      (when (subst-p subst)
	(if (subst-new-h-vars subst)
	    (dolist (sub (subst-new-h-vars subst))
	      (push (cdr sub) h-vars))
	    (dolist (var (subst-h-vars subst))
	      (push var h-vars)))))))

;;(defun find-head (term)
;;  (cond ((lsymbol-q term) term)
;;	((boundwff-q term) (find-head (cdr term)))
;;	(t (find-head (car term)))))

(defun eta-longform (head typevector)
  (do ((n (- (length typevector) 2) (1- n))
       (term head))
      ((minusp n) term)
    (setq term (acons (ren-var-uni-ho w-var-prefix (aref typevector n) n)
		      'lambda term))))

;;(defun find-subs-flexible-pairs (dpairs &optional (stack nil))
;;  (let ((basetype-vars nil))
;;    (dolist (pair dpairs stack)
;;      (let ((first (lambda-reduce (copy-uni-term (car pair)) stack))
;;	    (second (lambda-reduce (copy-uni-term (cdr pair)) stack)))
;;	(when (not (eq first second))
;;	  (if (not (lsymbol-q first))
;;	      (psetq first second second first))
;;	  (if (and (lsymbol-q first) (not (free-in first second)))
;;	      (if (rassoc first basetype-vars)
;;		  (let ((second (head second)))
;;		    (unless (rassoc second basetype-vars)
;;		      (push (cons second
;;				  (eta-longform first (listify-type
;;							(type second))))
;;			    stack)))
;;		(push (cons first second) stack))
;;	      (progn (setq first (head first)
;;			   second (head second))
;;		     (let* ((type1 (listify-type (type first)))
;;			    (resultype (aref type1 (1- (length type1))))
;;			    (head (cdr (assoc  resultype basetype-vars))))
;;		       (when (not head)
;;			 (setq head (ren-var-uni h-var-prefix resultype))
;;			 (push (cons resultype head) basetype-vars))
;;		       (when (not (rassoc first basetype-vars))
;;			 (push (cons first (eta-longform head type1)) stack))
;;		       (when (not (rassoc second basetype-vars))
;;			 (push (cons second
;;				     (eta-longform head (listify-type
;;							 (type second))))
;;			       stack))))))))))

(defun find-subs-flexible-pairs (dpairs subst-stack &optional (stack nil))
  (let ((basetype-vars nil))
    (dolist (pair dpairs stack)
      (let ((first (lambda-reduce (copy-uni-term (car pair)) stack))
	    (second (lambda-reduce (copy-uni-term (cdr pair)) stack)))
	(unless (eq first second)
	  (if (not (lsymbol-q first))
	      (psetq first second second first))
	  (if (and (lsymbol-q first)
		   (not (free-in-var-term first second nil nil subst-stack
					  nil nil)))
	      (if (rassoc first basetype-vars)
		  (let ((second (head second)))
		    (unless (rassoc second basetype-vars)
		      (push (cons second
				  (eta-longform first (listify-type
                                                       (type second))))
			    stack)))
		  (push (cons first second) stack))
	      (progn (setq first (head first)
			   second (head second))
		     (let* ((type1 (listify-type (type first)))
			    (resultype (aref type1 (1- (length type1))))
			    (head (cdr (assoc resultype basetype-vars))))
		       (unless head
			 (setq head (ren-var-uni-ho h-var-prefix resultype))
			 (push (cons resultype head) basetype-vars))
		       (unless (rassoc first basetype-vars)
			 (push (cons first (eta-longform head type1)) stack))
		       (unless (rassoc second basetype-vars)
			 (push (cons second
				     (eta-longform head (listify-type
                                                         (type second))))
			       stack))))))))))

(defun find-var-in-stack (var stack)
  (dolist (elt stack nil)
    (if (memq var elt) (return elt))))

(defun simplify-ffpairs (ffpairs)
  (let ((stack nil))
    (dolist (pair ffpairs)
      (let ((elt1 (find-var-in-stack (uni-term-head (car pair)) stack))
	    (elt2 (find-var-in-stack (uni-term-head (cdr pair)) stack)))
	(if elt1
	    (if elt2
		(unless (eq elt1 elt2)
		  (setq stack (delete elt1 (delete elt2 stack :test #'eq)
				      :test #'eq))
		  (push (nconc elt1 elt2) stack))
		(setq elt1 (nconc elt1 (list (uni-term-head (cdr pair))))))
	    (if elt2 (setq elt2 (nconc elt2 (list (uni-term-head (car pair)))))
		(push (list (uni-term-head (car pair))
			    (uni-term-head (cdr pair)))
		      stack)))))
    (let ((subst-stack nil))
      (dolist (elt stack)
	(let ((first (car elt)))
	  (dolist (var (cdr elt))
	    (push (cons var first) subst-stack))))
      subst-stack)))

(defun find-success-node-in-utree (utree)
  "Returns the first SUCCESS-NODE in left-to-right order in UTREE."
  (if (node-sons utree)
      (do ((sons (node-sons utree) (cdr sons))
	   (success-node nil (find-success-node-in-utree (car sons))))
	  ((or success-node (null sons)) success-node))
      (if (eq (node-terminal-flag utree) 'success)
	  utree nil)))
