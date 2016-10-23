;;; -*- Mode: Lisp -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile unif-lambda
  (part-of unification)
  (extension lisp)
  (mhelp "Unification functions."))

(defvar binder-pi 'cappi)
(defvar binder-sigma 'capsigma)
;;(putprop binder-pi t 'logconst)
;;(putprop binder-sigma t 'logconst)

(defun prefix-lambda-list (term list)
  (dolist (var list term)
    (setq term (acons var 'lambda term))))

(eval-when (load compile eval)
  (defmacro subst-term-stack (subst)
    `(if (consp (subst-type ,subst))
	 (cons (subst-type ,subst) (subst-new-h-vars ,subst))
	 (subst-new-h-vars ,subst))))

(defun hnf (term &optional (subst-stack nil))
  (unless (uni-term-p term)
    (setq term
	  (if (subst-p term)
	      (make-uni-term :head (subst-term term)
			     :stack (subst-term-stack term))
	      (make-uni-term :head term))))
  (multiple-value-bind (pos head args binder bdvars)
		       (hnf-rec (uni-term-pos term) (uni-term-head term)
				(uni-term-args term) (uni-term-binder term)
				(uni-term-bdvars term) (uni-term-stack term)
				subst-stack)
    (setf (uni-term-pos term) pos)
    (setf (uni-term-head term) head)
    (setf (uni-term-args term) args)
    (setf (uni-term-binder term) binder)
    (setf (uni-term-bdvars term) bdvars)
    (setf (uni-term-stack term) nil))
  term)
    
(defun hnf-rec (pos term args binder bdvars stack subst-stack)
  (cond ((label-q term)
	 (apply-label
	  term (hnf-rec pos term args binder bdvars stack subst-stack)))
	((lsymbol-q term)
	 (if (abbrev-q term)
	     (values pos term args binder bdvars) 
	   ;the line above is new.
	   ;the following was removed for the sake of BLEDSOE7A... MB Wed Jun 11 14:10:13 1997
	   ;it's possible that we need to keep it for SIGMA1 and PI1, or things like that...
	   ;(hnf-rec pos (get-defn term) args binder bdvars stack subst-stack)
	   (if (and (eq term 'not) ; cebrown -- added check for arguments since
                    args)             ; if there are no arguments,
	       (if reduce-double-neg  ; then this is an error.  In particular,
                                      ; (hnf-rec pos 'NOT nil . . . .)
                                      ; will return (values (not pos) nil nil . . . .)
                                      ; instead of (values pos 'NOT nil . . . .)
		   (hnf-rec (not pos) (caar args) (cdr args) binder bdvars
			    (cdar args) subst-stack)
		 (values pos term args binder bdvars))
	     (if (memq term binder) (values pos term args binder bdvars)
	       (let ((newterm (cdr (assoc term stack))))
		 (if newterm
		     (if (symbolp newterm)
			 (if (assoc newterm bdvars)
			     (values pos newterm args binder bdvars)
			   (hnf-rec pos newterm args binder bdvars nil
				    subst-stack))
		       (hnf-rec pos (car newterm) args binder bdvars
			        (cdr newterm) subst-stack))
		   (let ((subst (cdr (assoc term subst-stack))))
		     (if subst
			 (if (subst-p subst)
			     (hnf-rec
			      pos (subst-term subst) args binder
			      bdvars (subst-term-stack subst)
			      subst-stack)
			   (hnf-rec pos subst args binder bdvars nil
				    subst-stack))
		       (values pos term args binder bdvars)))))))))
	((boundwff-q term)
	 (cond ((a-bd-wff-p term)
		(values
		 pos (create-propsym
		      binder-pi (cons 'O (cons 'O (type (bdvar term)))))
		 (list (cons (acons (bdvar term) 'lambda (cdr term)) stack))
		 binder bdvars))
	       ((e-bd-wff-p term)
		(values
		 pos (create-propsym
		      binder-sigma (cons 'O (cons 'O (type (bdvar term)))))
		 (list (cons (acons (bdvar term) 'lambda (cdr term))
			     stack))
		 binder bdvars))
	       (t (if args
		      (hnf-rec pos (cdr term) (cdr args) binder bdvars
			       (acons (bdvar term) (car args) stack)
			       subst-stack)
		    (let ((label (gensym))
			  (bdvar (bdvar term)))
		      (hnf-rec pos (cdr term) nil (cons label binder)
			       (acons label bdvar bdvars)
			       (acons bdvar label stack)
			       subst-stack))))))
	(t (hnf-rec pos (car term) (acons (cdr term) stack args) binder bdvars
		    stack subst-stack))))

(defun rename-all-bd-variables (term &optional (first t))
  (declare (ignore first))
  (if (boundwff-q term)
      (let ((newterm (rename-bd-var term)))
	(cons (car newterm) (rename-all-bd-variables (cdr newterm) nil)))
    (if (consp term)
	(cons (rename-all-bd-variables (car term) nil)
	      (rename-all-bd-variables (cdr term) nil))
      term)))

(defun free-in-var-term (var term bdvars stack subst-stack rigid-path
			     free-vars)
  (cond ((label-q term)
	 (apply-label
	   term (free-in-var-term var term bdvars stack subst-stack
				  rigid-path free-vars)))
	((subst-p term)
	 (multiple-value-bind (free rigid-path)
	     (if (consp (subst-type term))
		 (free-in-var-term var (cdr (subst-type term)) nil nil
				   subst-stack rigid-path free-vars))
	   (if free (values free rigid-path)
	       (if (subst-new-h-vars term)
		   (dolist (pair (subst-new-h-vars term)
				 (values nil rigid-path))
		     (multiple-value-bind (free rigid-path)
			 (free-in-var-term var (cdr pair) nil nil subst-stack
					   rigid-path free-vars)
		       (if free (return (values free rigid-path)))))
		   (dolist (elt (subst-h-vars term) (values nil rigid-path))
		     (multiple-value-bind (free rigid-path)
			 (free-in-var-term var elt nil nil subst-stack
					   rigid-path free-vars)
		       (if free (return (values free rigid-path)))))))))
	((lsymbol-q term)
	 (let ((binding (cdr (assoc term stack))))
	   (if binding
	       (if (symbolp binding)
		   (let ((bdvar (cdr (assoc binding bdvars))))
		     (if bdvar (values nil rigid-path)
			 (free-in-var-term var binding nil nil subst-stack
					   rigid-path free-vars)))
		   (free-in-var-term var (car binding) bdvars (cdr binding)
				     subst-stack rigid-path free-vars))
	       (let ((binding (cdr (assoc term subst-stack))))
		 (if binding
		     (free-in-var-term var binding nil nil subst-stack
				       rigid-path free-vars)
		     (if (eq var term) (values t rigid-path)
			 (values nil (and rigid-path
					  (not (memq term free-vars)))))
		     )))))
	((boundwff-q term)
	 (if (eq var (bdvar term)) (values nil rigid-path)
	     (free-in-var-term var (cdr term) bdvars stack subst-stack
			       rigid-path free-vars)))
	(t (multiple-value-bind (free rigid-path)
	       (free-in-var-term var (car term) bdvars stack subst-stack
				 rigid-path free-vars)
	     (if free (values t rigid-path)
		 (free-in-var-term var (cdr term) bdvars stack subst-stack
				   rigid-path free-vars))))))

(defun free-in-var-arg (var arg binder bdvars subst-stack rigid-path free-vars)
  (multiple-value-bind (pos head args binder bdvars)
      (hnf-rec nil (car arg) nil binder bdvars (cdr arg) subst-stack)      
      (declare (ignore pos))
      (multiple-value-bind (free rigid-path)
        (free-in-var-term var head bdvars
			  nil subst-stack rigid-path free-vars)
        (if free (values t rigid-path)
            (dolist (hxarg args nil)
	      (multiple-value-bind (free rigid-path)
		(free-in-var-arg var hxarg binder bdvars 
				  subst-stack rigid-path free-vars)
		(if free (return (values t rigid-path)))))))))

(defun free-in-var-uniterm (var uniterm subst-stack free-vars)
  (let* ((hxuniterm (hnf uniterm subst-stack))
         (bdvars (uni-term-bdvars hxuniterm))
         (binder (uni-term-binder hxuniterm)))
    (multiple-value-bind (free rigid-path)
	(free-in-var-term var (uni-term-head hxuniterm) bdvars
			  (uni-term-stack hxuniterm) subst-stack t free-vars)
      (if free (values t rigid-path)
	  (dolist (arg (uni-term-args hxuniterm) nil)
	    (multiple-value-bind (free rigid-path)
		(free-in-var-arg var arg binder bdvars 
				 subst-stack rigid-path free-vars)
		(if free (return (values t rigid-path)))))))))



;*;(defun occurs-in-term-label (label term)
;*;  (cond ((label-q term) nil)
;*;	((symbolp term) (eq label term))
;*;	(t (or (occurs-in-term-label label (car term))
;*;	       (occurs-in-term-label label (cdr term))))))

;*;(defun lambda-reduce (term &optional (subst-stack nil))
;*;  (multiple-value-bind (term bdvars)
;*;		       (lambda-reduce-rec term subst-stack)
;*;    (let ((newnames nil))
;*;      (dolist (binding bdvars term)
;*;	(let ((label (car binding))
;*;	      (var (cdr binding)))
;*;	  (let ((new (cdr (assoc var newnames))))
;*;	    (if new (setq term (nsubst new label term :test #'eq))
;*;		(when (occurs-in-term-label label term)
;*;		  (if (free-in var term)
;*;		      (let ((new (funcall ren-var-fn var)))
;*;			(push (cons var new) newnames)
;*;			(setq term (nsubst new label term :test #'eq)))
;*;		      (progn
;*;		       (push (cons var var) newnames)
;*;		       (setq term (nsubst var label term :test #'eq)))
;*;		      )))))))))

(defun lambda-reduce (term &optional (subst-stack nil))
  (multiple-value-bind (term bdvars)
		       (lambda-reduce-rec term subst-stack)
    (let ((newnames nil))
      (dolist (binding bdvars term)
	(let ((label (car binding))
	      (var (cdr binding)))
	  (let ((new (cdr (assoc var newnames))))
	    (if new (setq term (nsubst new label term :test #'eq))
		(if (free-in var term)
		    (let ((new (funcall ren-var-fn var)))
		      (push (cons var new) newnames)
		      (setq term (nsubst new label term :test #'eq)))
		    (progn
		     (push (cons var var) newnames)
		     (setq term (nsubst var label term :test #'eq)))))))))))

(defun lambda-reduce-rec (term subst-stack)
  (let* ((hnf (hnf term subst-stack))
	 (term (uni-term-head hnf))
	 (bdvars (uni-term-bdvars hnf)))
    (dolist (arg (uni-term-args hnf)
		 (values (prefix-lambda-list
			  (if (uni-term-pos hnf) term (cons 'not term))
			  (uni-term-binder hnf))
			 bdvars))
      (multiple-value-bind (arg newbdvars)
          (lambda-reduce-rec
           (make-uni-term :head (car arg) :stack (cdr arg)
                          :bdvars bdvars)
           subst-stack)
	(setq term (cons term arg) bdvars newbdvars)))))

(defun lambda-reduce-subst (subst subst-stack)
  (if (subst-p subst)
      (lambda-reduce
	(make-uni-term :head (subst-term subst)
		       :stack (subst-term-stack subst))
	subst-stack)
      (lambda-reduce subst subst-stack)))
