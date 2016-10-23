;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-OPS2)

;;;
;;; File WFFLMBD2
;;;
;;; lambda conversion operations on wffs.
;;;

(deffile wfflmbd2
  (part-of wff-ops2)
  (extension clisp)
  (mhelp "Contains lambda operations."))

(context lambda-op)

;;This function replaces occurrences of a term
;;with a variable and returns the application of the modified wff,
;;with lambda binding for the variable, on the term.

(defwffop lexpd
  (argtypes gvar gwff gwff occ-list)
  (wffargtypes "A" "A" "B" NIL)
  (resulttype gwff)
  (wffop-type "B")
  (wffop-typelist "A" "B")
  (argnames var term inwff occurs)
  (arghelp "lambda variable" "term to be extracted" "contracted form"
	   "occurrences to be extracted")
  (applicable-p (lambda (var term inwff occurs)
		  (declare (ignore inwff occurs))
		  (type-equal term var)))
  (mhelp "Converts the wff into the application of a function to the term.
The function is formed by replacing given valid occurrences of a term
with the variable and binding the result."))

(defun lexpd (var term inwff occurs)
  (let ((occs 0) (val-occs 0))
    (declare (special occs troub val-occs ren-var-fn))
    (let* ((tfree (free-vars-of term))
	   (var1 (if (free-in var inwff)    
		     (if (member var tfree) var (funcall ren-var-fn var))
		     var)))
      (%catch% (cons (acons var1 'lambda
			    (lexpd-rec var1 term tfree inwff occurs nil))
		     term)
	       (fail (setq occs 0) (setq val-occs 0)
		     (let ((x (funcall ren-var-fn var)))
		       (cons (acons x 'lambda
				    (lexpd-rec x term tfree inwff occurs nil)) 
			     term)))))))

(defwffrec lexpd-rec 
  (argnames var term tfree inwff occurs prep-sw)
  (mhelp "Recursive part of lambda expansion."))

(defun lexpd-rec (var term tfree inwff occurs prep-sw)
  (declare (special occs val-occs)) 
  (cond ((exceeds occs occurs) inwff) 
	((label-q inwff)
	 (apply-label inwff (lexpd-rec var term tfree inwff occurs prep-sw)))
	((wffeq-ab term inwff) (setq occs (1+ occs))
	 (cond ((good-occ occs occurs) (setq val-occs (1+ val-occs)) var)
	       ((member var tfree) (throwfail nil))
	       (t inwff)))
	((lsymbol-q inwff) 
	 (if (and (not prep-sw) (eq inwff var)) (throwfail nil) inwff))
	((boundwff-q inwff)
	 (cond ((member (caar inwff) tfree) inwff)
	       ((and (null prep-sw) (equal var (caar inwff)))
		(let ((occs1 occs) 
		      (occs2 val-occs)
		      (new-wff (prepare-for var term tfree inwff occurs)))
		  (cond ((> val-occs occs2)  
			 (setq occs occs1) (setq val-occs occs2)
			 (lexpd-rec var term tfree new-wff occurs t))
			(t new-wff))))
	       (t (cons (car inwff) (lexpd-rec var term tfree (cdr inwff) 
					       occurs prep-sw)))))
	((infix-p inwff)
	 (if (wffeq-ab term (car inwff))
	     (progn
	     (setq occs (1+ occs))
	     (if (good-occ occs occurs)
		 (progn
		  (incf val-occs)
		  (cons var (lexpd-rec var term tfree (cdr inwff)
				       occurs prep-sw)))
		 (cons (car inwff)
		       (lexpd-rec var term tfree (cdr inwff) occurs prep-sw))))
	     (let ((x (lexpd-rec
		       var term tfree (gdr (car inwff)) occurs prep-sw)) 
		   (y (lexpd-rec
		       var term tfree (gar(car inwff)) occurs prep-sw)))
	       (cons (cons y x) 
		     (lexpd-rec var term tfree (cdr inwff) occurs prep-sw)))))
	(t (cons (lexpd-rec var term tfree (car inwff) occurs prep-sw)
		 (lexpd-rec var term tfree (cdr inwff) occurs prep-sw)))))

;;This function changes bound variables where needed
;;so that lexpd may replace var for term without care. cpk

(defwffrec prepare-for 
  (argnames var term inwff occurs)
  (mhelp 
   "Makes alphabetic change to avoid binding of variable replacing term."))

(defun prepare-for (var term tfree inwff occurs)
  (declare (special occs val-occs))
  (cond ((exceeds occs occurs) inwff)	
	((label-q inwff) (apply-label inwff (prepare-for var term tfree
							 inwff occurs)))
	((wffeq-ab term inwff)
	 (incf occs) 
	 (if (good-occ occs occurs) (incf val-occs))
	 inwff)
	((lsymbol-q inwff) inwff)
	((boundwff-q inwff) 
	 (if (member (caar inwff) tfree) inwff
	     (if (eq var (caar inwff)) 
		 (let ((pre-occs val-occs)
		       (new-wff (prepare-for var term tfree (cdr inwff)
					     occurs)))
		   (if (> val-occs pre-occs)
		       (rename-bd-var (cons (car inwff) new-wff))
		       (cons (car inwff) new-wff)))
		 (cons (car inwff) 
		       (prepare-for var term tfree (cdr inwff) occurs)))))
	((infix-p inwff)
	 (let ((x (prepare-for var term tfree (cdar inwff) occurs)) 
	       (y (prepare-for var term tfree (caar inwff) occurs)))
	   (cons (cons y x) (prepare-for var term tfree (cdr inwff) occurs))))
	(t (cons (prepare-for var term tfree (car inwff) occurs)
		 (prepare-for var term tfree (cdr inwff) occurs)))))

(defun exceeds (x lis)
  (cond ((eq lis t) nil)
	((null lis) t)
	((> x (car lis)) (exceeds x (cdr lis)))
	(t nil)))

(defwffop lcontr
  (argtypes gwff)
  (wffargtypes "A")
  (resulttype gwff)
  (wffop-type "A")
  (wffop-typelist "A")
  (argnames reduct)
  (arghelp "gwff (reduct)")
  (applicable-p reduct-p)
  (mhelp "Lambda-contract a top-level reduct.
Bound variables may be renamed using REN-VAR-FN"))

(defun lcontr (reduct)
  (cond ((label-q reduct) (apply-label reduct (lcontr reduct)))
	((lsymbol-q reduct)
	 (throwfail "Cannot Lambda-contract " (reduct . gwff)
		    ", a logical symbol."))
	((boundwff-q reduct)
	 (throwfail "Cannot Lambda-contract " (reduct . gwff)
		    ", a bound wff."))
	(t (if (lambda-bd-p (car reduct))
	       (substitute-l-term-var (cdr reduct) (gar (car reduct))
				      (gdr (car reduct)))
	       (throwfail "Top-level application " (reduct . gwff)
			  " is not of the form [LAMBDA x A]t.")))))

(defwffop lambda-norm
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Convert a wff into lambda-normal form."))

(defun lambda-norm (gwff)
  (cond ((label-q gwff) (apply-label gwff (lambda-norm gwff)))
	((lsymbol-q gwff) gwff)
	((boundwff-q gwff) (cons (car gwff) (lambda-norm (cdr gwff))))
	(t (let ((gwff (cons (lambda-norm (car gwff))
			     (lambda-norm (cdr gwff)))))
	     (if (lambda-bd-p (car gwff))
		 (lambda-norm (lambda-contr gwff)) gwff)))))

(defwffop untyped-lambda-norm
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Convert a untyped wff into lambda-normal form. Be aware of unterminated reduction 
in untyped lambda calculus."))

(defun untyped-lambda-norm (gwff) 
   (if untyped-lambda-calculus 
       (cond ((label-q gwff) (apply-label gwff (untyped-lambda-norm gwff)))
             (t (encode-untyped (lambda-norm (decode-typed gwff)))))
       (throwfail "Flag UNTYPED-LAMBDA-CALCULUS is not on.")))


(defwffop ab-normal-p
  (argtypes gwff)
  (argnames gwff)
  (resulttype boolean)
  (mhelp "Check whether the gwff is in alphabetic normal form. "))

(defun ab-normal-p (gwff)
  (let ((illegal-free nil)
	(illegal-bd nil))
    (declare (special illegal-free illegal-bd)) 
    (ab-normal-p1 gwff)))

(defun ab-normal-p1 (gwff)
  (declare (special illegal-free illegal-bd)) 
  (cond ((label-q gwff)
	 (apply-label gwff (ab-normal-p1 gwff)))
	((lsymbol-q gwff)
	 (cond ((logconst-q gwff) T)
	       ((propsym-q gwff)
		(if (member gwff illegal-free) nil
		    (progn (push gwff illegal-bd) T)))
	       (T T)))
	((boundwff-q gwff)
	 (let ((var (caar gwff)))
	   (if (member var illegal-bd) nil
	       (prog2
		(push var illegal-bd)
		(ab-normal-p1 (cdr gwff))
		(push var illegal-free)))))
	(T (and (ab-normal-p1 (car gwff))
		(ab-normal-p1 (cdr gwff))))))

(defwffop ab-normalize
  (argtypes gwff)
  (argnames gwff)
  (resulttype gwff)
  (mhelp "Convert the gwff to alphabetic normal form. "))

;;; Rewrote to minimize the number of variables that get renamed
;;; 10/3/87 DAN
;;; Also, now returns two values, first is the new wff in ab-normal form
;;; and second is an alist of the variable/new-variable pairs which
;;; were used in the renaming.

(defun ab-normalize (gwff)
  (let ((used-vars (free-vars-of gwff))
	(ren-vars nil))
    (declare (special ren-vars used-vars))
    (values (or (ab-normalize-main gwff) gwff) ren-vars)))


;;; When the flag rename-all-bd-vars is T, 
;;; all bound variables will be renamed DAN 2-22-88

(defun ab-normalize-main (gwff)
  (declare (special ren-vars used-vars))
  (cond ((label-q gwff) (apply-label gwff (ab-normalize-main gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff)
	 (if (or rename-all-bd-vars (memq (bdvar gwff) used-vars))
	     (let ((newvar (funcall ren-var-fn (bdvar gwff))))
	       (push newvar used-vars)
	       (push (cons (bdvar gwff) newvar) ren-vars)
	       (let ((newff (substitute-term-var
			     newvar (bdvar gwff) (cdr gwff))))
		 (acons newvar (binder gwff)
			(or (ab-normalize-main newff) newff))))
	     (progn
	      (push (bdvar gwff) used-vars)
	      (let ((newff (ab-normalize-main (cdr gwff))))
		(if newff (cons (bdhead gwff) newff) nil)))))
	(t  (let ((left (or (ab-normalize-main (car gwff))
			    (car gwff)))
		  (right (or (ab-normalize-main (cdr gwff))
			     (cdr gwff))))
	      ;; Returns nil unless one of the two parts of the wff
	      ;; has been changed
	      (unless (and (eq left (car gwff)) (eq right (cdr gwff)))
		      (cons left right))))))

(defwffop etanorm
  (argtypes gwff)
  (wffargtypes "A")
  (resulttype gwff)
  (wffop-type "A")
  (wffop-typelist "A")
  (applicable-p gwff-p)
  (argnames gwff)
  (arghelp "Wff to be eta-normalized.")
  (mhelp "Reduces [lambda x.fx] to f from inside out."))

(defun etanorm (gwff)
  (cond ((label-q gwff) (apply-label gwff (etanorm gwff)))
        ((lsymbol-q gwff) gwff)
	((boundwff-q gwff)
	 (let ((contrform (etanorm (gdr gwff))))
	   (if (eq (binding gwff) 'lambda)
	       (if (and (eta-applic-p contrform)
			(equal (bindvar gwff) (gdr contrform))
			(not (free-in (bindvar gwff) (gar contrform))))
		   (gar contrform) (cons (bindhead gwff) contrform))
	       (cons (bindhead gwff) contrform))))
	(t (cons (etanorm (gar gwff)) (etanorm (gdr gwff))))))

(defwffop etacontr
  (argtypes gwff)
  (wffargtypes "A")
  (resulttype gwff)
  (wffop-type "A")
  (wffop-typelist "A")
  (applicable-p gwff-p)
  (argnames gwff)
  (arghelp "Wff to be eta-contracted.")
  (mhelp "Reduces [lambda x.fx] to f at top."))

(defun etacontr (gwff)
  (cond ((lsymbol-p gwff) gwff)
	((boundwff-q gwff)
	 (if (eq (binding gwff) 'lambda)
	     (if (and (eta-applic-p (gdr gwff))   ; was (wff-applic-p contrform), which is surely wrong..? MB
		      (wffeq (bindvar gwff) (gdr (gdr gwff)))
		      (not (free-in (bindvar gwff) (gar (gdr gwff)))))
		 (gar (gdr gwff)) gwff)
	     gwff))
	(t gwff)))

(defun eta-applic-p (gwff)
  ;written Sun Sep 17 14:51:59 1995 because wff-applic-p was insufficient (it only tests for a cons,
  ;and that doesn't quite mean it's a function application - e.g.lambda y . x returns T from
  ;wff-applic-p.
  (and (wff-applic-p gwff)
       (not (boundwff-q gwff))))

(defun eta-exp (gwff)
  (cond ((label-q gwff) (apply-label gwff (eta-exp gwff)))
	((type-of-arg-1 gwff) 
	 (let ((var (fresh-var-1 (type-of-arg-1 gwff))))
	   (acons var 'lambda (apply-wff gwff var))))
	(t gwff)))

(defwffop eta-exp
  (argnames gwff)
  (argtypes gwff)
  (resulttype gwff)
  (applicable-q type-of-arg-1)
  (mhelp "Performs a one-step eta expansion."))

(defun eta-to-base (gwff)
  (if (label-q gwff) (apply-label gwff (eta-to-base gwff))
      (let ((varlis (mapcar #'fresh-var-1 (cdr (fntype-list gwff)))))
	(bind-var-wff-n 'lambda varlis (apply-wff-n gwff varlis)))))

(defwffop eta-to-base
  (argtypes gwff)
  (argnames gwff)
  (resulttype gwff)
  (applicable-q type-of-arg-1)
  (mhelp "Eta-expands until original wff is part of a wff of base type."))

(defun long-eta (gwff)
  (cond ((label-q gwff) (apply-label gwff (long-eta gwff)))
	((lsymbol-q gwff)
	 (let ((varlis (mapcar #'fresh-var-1 (cdr (fntype-list gwff)))))
	   (bind-var-wff-n 'lambda varlis
			   (apply-wff-n gwff (mapcar #'long-eta varlis)))))
	((boundwff-q gwff) (attach-head (bindhead gwff) (long-eta (gdr gwff))))
	(t (let ((varlis (mapcar #'fresh-var-1 (cdr (fntype-list gwff)))))
	     (bind-var-wff-n 'lambda varlis
			     (apply-wff-n (long-eta-feed gwff)
					  (mapcar #'long-eta varlis)))))))

; mkaminski 11/7/2005

;(defun long-eta-feed (gwff)
;  (if (lsymbol-q gwff)
;      gwff (apply-wff (long-eta-feed (gar gwff)) (long-eta (gdr gwff)))))

(defun long-eta-feed (gwff)
  (cond ((lsymbol-q gwff) gwff)
	((boundwff-q gwff) (apply-wff (car gwff) (long-eta (gdr gwff))))
	(t (apply-wff (long-eta-feed (gar gwff)) (long-eta (gdr gwff))))))

(defwffop long-eta
  (argtypes gwff)
  (argnames gwff)
  (resulttype gwff)
  (applicable-q gwff-p)
  (mhelp "Returns the long-eta normal form of wff."))

#+comment(defun rewrite-all-equivalence (gwff)
  (cond ((label-q gwff) (apply-label gwff (rewrite-all-equivalence gwff)))
	((lsymbol-q gwff) gwff)
	((not-p gwff) (cons 'not  (rewrite-all-equivalence (cdr gwff))))
	((infix-p gwff)
	 (let ((left (rewrite-all-equivalence (cdar gwff)))
	       (right (rewrite-all-equivalence (cdr gwff))))
	   (if (equiv-p gwff)
	       (acons 'and (acons 'implies left right)
		      (acons 'implies right left))
	       (acons (caar gwff) left right))))
	((boundwff-q gwff)
	 (cons (car gwff) (rewrite-all-equivalence (cdr gwff))))
	(t gwff)))

(defun rewrite-all-equivalence (gwff &optional (pos t) (done1 nil))
  (declare (special rewrite-equivs))
  (cond ((label-q gwff) (apply-label gwff (rewrite-all-equivalence gwff pos done1)))
	((lsymbol-q gwff) gwff)
	((not-p gwff) (cons 'not  (rewrite-all-equivalence (cdr gwff) (not pos) done1)))
	((infix-p gwff)
	 (let* ((left1 (rewrite-all-equivalence (cdar gwff) 
						(if (eq (caar gwff) 'implies) (not pos) pos) 
						(or done1 (equiv-p gwff))))
		(right1 (rewrite-all-equivalence (cdr gwff)
						 pos
						 (or done1 (equiv-p gwff))))
		(flag (member rewrite-equivs '(4 5) :test 'equal))
		(left2 (and (not flag) (rewrite-all-equivalence (cdar gwff) (not pos) t))) 
		(right2 (and (not flag) (rewrite-all-equivalence (cdr gwff) (not pos) t)))
		(rewrite-as-conj (and (equiv-p gwff)
				      (case rewrite-equivs
					(1 (not pos))
					(2 pos)
					(3 (if done1 pos (not pos)))
					(4 t)
					(5 nil)
					(t (not pos))))))
	   (if (equiv-p gwff)
	       (if rewrite-as-conj
		   (acons 'and (acons 'implies (if flag left1 left2) right1)
			  (acons 'implies (if flag right1 right2) left1))
		 (acons 'or (acons 'and left1 right1)
			(acons 'and (cons 'not (if flag left1 left2)) (cons 'not (if flag right1 right2)))))
	     (acons (caar gwff) left1 right1))))
	((boundwff-q gwff)
	 (cons (car gwff) (rewrite-all-equivalence (cdr gwff) pos done1)))
	(t gwff)))

(defwffop rewrite-all-equivalence
  (argtypes gwff)
  (argnames gwff)
  (resulttype gwff)
  (applicable-q gwff-p)
  (mhelp "Replaces all occurrences of the form `A EQUIV B'
according to the setting of the flag REWRITE-EQUIVS."))

(defwffop wffeq-ab-lambda
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (arghelp "old wff" "new wff")
  (wffargtypes "A" "A" )
  (resulttype boolean)
  (wffop-typelist "A" )
  (mhelp "Verifies that wff1 and wff2 are equal up to
lambda-normalization and alphabetic change of bound variables.
Uses both eta and beta rules (compare WFFEQ-AB-ETA and WFFEQ-AB-BETA)."))

(defun wffeq-ab-lambda (wff1 wff2)
  (wffeq-ab (etanorm (lambda-norm wff1)) (etanorm (lambda-norm wff2))))

(defwffop wffeq-ab-beta
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (arghelp "old wff" "new wff")
  (wffargtypes "A" "A" )
  (resulttype boolean)
  (wffop-typelist "A" )
  (mhelp "Verifies that wff1 and wff2 are equal up to
lambda-normalization with beta rule only, and alphabetic change of 
bound variables. (Compare WFFEQ-AB-ETA, WFFEQ-AB-LAMBDA.)"))

(defun wffeq-ab-beta (wff1 wff2)
  (wffeq-ab (lambda-norm wff1) (lambda-norm wff2)))

(defwffop wffeq-ab-eta
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (arghelp "old wff" "new wff")
  (wffargtypes "A" "A" )
  (resulttype boolean)
  (wffop-typelist "A" )
  (mhelp "Verifies that wff1 and wff2 are equal up to
lambda-normalization with eta rule only, and alphabetic change of 
bound variables. (Compare WFFEQ-AB-BETA, WFFEQ-AB-LAMBDA.)"))

(defun wffeq-ab-eta (wff1 wff2)
  (wffeq-ab (etanorm wff1) (etanorm wff2)))
  