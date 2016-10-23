;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :auto)
(part-of tactics)

(context tactics)
(deffile tacticals-macros
  (extension lisp)
  (part-of tactics)
  (mhelp "Functions used by tacticals."))

(defun make-validation (old-validation returned-validation)
  (if (and returned-validation
	   (not (equal returned-validation (list 'lambda (list 'x) 'x))))
      (let* ((old-validation (copy-tree old-validation))
	     (returned-validation (copy-tree returned-validation))
	     (replaced-var (caadr old-validation))
	     (new-vars (mapcar #'(lambda (x) (declare (ignore x)) (gensym "v"))
			       (cadr returned-validation))))
	(list 'lambda
	      (append new-vars (cdadr old-validation))
	      (subst (sublis (pairlis (cadr returned-validation) new-vars)
			     (caddr returned-validation))
		     replaced-var
		     (caddr old-validation))))
      old-validation))


;;; puts the first n variables in the lambda-list of validation
;;; at the rear of the lambda-list

(defun rotate-vars (n validation)
  (let* ((validation (copy-tree validation))
	 (first-vars (subseq (cadr validation) 0 n)))
    (list 'lambda
	  (append (nthcdr n (cadr validation)) first-vars)
	  (caddr validation))))
