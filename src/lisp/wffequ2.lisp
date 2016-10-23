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
;;; File WFFEQU2
;;;
;;; Contains some test for equality between wffs.
;;;

(deffile wffequ2
  (part-of wff-ops2)
  (extension clisp)
  (mhelp "Contains tests for equality between wffs."))

(context wffequal)

(defwffop wffeq-nnf
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (resulttype boolean)
  (multiple-recursion t)
  (mhelp "Test for equality modulo negation normal form."))

(defun wffeq-nnf (wff1 wff2) (wffeq-nnf1 wff1 wff2 T))

(defwffrec wffeq-nnf1
  (argnames wff1 wff2 parity)
  (multiple-recursion t))

(defun wffeq-nnf1 (wff1 wff2 parity)
  (cond ((label-q wff1) (apply-label wff1 (wffeq-nnf1 wff1 wff2 parity)))
	((label-q wff2) (apply-label wff2 (wffeq-nnf1 wff1 wff2 parity)))
	((not-p wff1)
	 (wffeq-nnf1 (cdr wff1) wff2 (not parity)))
	((not-p wff2)
	 (wffeq-nnf1 wff1 (cdr wff2) (not parity)))
	((lsymbol-q wff1) (and parity (eq wff1 wff2)))
	((lsymbol-q wff2) nil)
	((ae-bd-wff-p wff1)
	 (if (ae-bd-wff-p wff2)
	     (cond (parity
		    (and (eq (car wff1) (car wff2))
			 (wffeq-nnf1 (cdr wff1) (cdr wff2) T)))
		   ((not (eq (cdar wff1) (cdar wff2)))
		    (and (eq (caar wff1) (caar wff2))
			 (wffeq-nnf1 (cdr wff1) (cdr wff2) nil)))
		   (T nil))
	     nil))
	((ae-bd-wff-p wff2) nil)
	(T
	 (if parity
	     (cond ((or (and (or-p wff1) (or-p wff2))
			(and (and-p wff1) (and-p wff2))
			(and (implies-p wff1) (implies-p wff2)))
		    (and (wffeq-nnf1 (gdr (car wff1))
				     (gdr (car wff2)) T)
			 (wffeq-nnf1 (cdr wff1) (cdr wff2) T)))
		   ((or (and (or-p wff2) (implies-p wff1))
			(and (or-p wff1) (implies-p wff2)))
		    (and (wffeq-nnf1 (gdr (car wff1))
				     (gdr (car wff2)) nil)
			 (wffeq-nnf1 (cdr wff1) (cdr wff2) T)))
		   (T (wffeq wff1 wff2)))
	     (cond ((or (and (and-p wff1) (or-p wff2))
			(and (and-p wff2) (or-p wff1)))
		    (and (wffeq-nnf1 (gdr (car wff1))
				     (gdr (car wff2)) nil)
			 (wffeq-nnf1 (cdr wff1) (cdr wff2) nil)))
		   ((or (and (implies-p wff1) (and-p wff2))
			(and (implies-p wff2) (and-p wff1)))
		    (and (wffeq-nnf1
			  (gdr (car wff1)) (gdr (car wff2)) T)
			 (wffeq-nnf1 (cdr wff1) (cdr wff2) nil)))
		   (T nil))))))


(defwffop wffeq-lnorm
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (resulttype boolean)
  (multiple-recursion t)
  (mhelp "Test for equality modulo lambda conversions."))

(defun wffeq-lnorm (wff1 wff2) (wffeq-lnorm1 wff1 wff2 nil nil nil))

(defwffrec wffeq-lnorm1
  (argnames wff1 wff2 varstack argstack1 argstack2)
  (multiple-recursion t))

(defmacro collect1 ()
  `(progn (push (cdr carwff1) ast1)
	  (setq carwff1 (car carwff1))))

(defmacro collect2 ()
  `(progn (push (cdr carwff2) ast2)
	  (setq carwff2 (car carwff2))))

(defun wffeq-lnorm1 (wff1 wff2 varstack argstack1 argstack2)
  (cond ((label-q wff1)
	 (apply-label wff1 (wffeq-lnorm1 wff1 wff2 varstack argstack1 argstack2)))
	((label-q wff2)
	 (apply-label wff2 (wffeq-lnorm1 wff1 wff2 varstack argstack1 argstack2)))
	((lsymbol-q wff1)
	 (cond ((not (lsymbol-q wff2)) (wffeq-lnorm-lsym1 wff1 wff2 varstack))
	       ((or (logconst-q wff1) (logconst-q wff2)) (eq wff1 wff2))
	       ((and (propsym-q wff1) (propsym-q wff2))
		(true-correspondence wff1 wff2 varstack))
	       ;; Now pmabbrev-q or abbrev-q case left
	       (t (eq wff1 wff2))))
	((lsymbol-q wff2) (wffeq-lnorm-lsym2 wff1 wff2 varstack))
	((boundwff-q wff1)
	 (cond ((cond ((not (boundwff-q wff2)) (wffeq-lnorm-bdwff1 wff1 wff2 varstack))
		      (t (and (eq (cdar wff1) (cdar wff2))
			      (type-equal (caar wff1) (caar wff2))
			      (wffeq-lnorm1 (cdr wff1) (cdr wff2)
					    (cons (cons (caar wff1) (caar wff2)) varstack) argstack1 argstack2)))))
	       (t (cond ((and argstack1 (lambda-bd-p wff1))
			 (setq wff1 (lcontr (cons wff1 (pop argstack1))))
			 (wffeq-lnorm1 wff1 wff2 varstack argstack1 argstack2))
			((and argstack2 (lambda-bd-p wff2))
			 (setq wff2 (lcontr (cons wff2 (pop argstack2))))
			 (wffeq-lnorm1 wff1 wff2 varstack argstack1 argstack2))
			(t nil)))))
	((boundwff-q wff2)
	 (cond ((wffeq-lnorm-bdwff2 wff1 wff2 varstack) t)
	       (t (cond ((and argstack2 (lambda-bd-p wff2))
			 (setq wff2 (lcontr (cons wff2 (pop argstack2))))
			 (wffeq-lnorm1 wff1 wff2 varstack argstack1 argstack2))
			(t nil)))))
	(t (cond ((wffeq-lnorm1 (cdr wff1) (cdr wff2) varstack nil nil)
		  (wffeq-lnorm1 (car wff1) (car wff2) varstack
				(cons (cdr wff1) argstack1)
				(cons (cdr wff2) argstack2)))
		 (t (do ((ast1 (list (cdr wff1)))
			 (ast2 (list (cdr wff2)))
			 (carwff1 (car wff1))
			 (carwff2 (car wff2)))
			((and (null ast1) (null ast2))
			 (wffeq-lnorm1 carwff1 carwff2 varstack argstack1 argstack2))
		      (cond ((and ast1 (lambda-bd-p carwff1))
			     (setq carwff1 (lcontr (cons carwff1 (pop ast1)))))
			    ((and ast2 (lambda-bd-p carwff2))
			     (setq carwff2 (lcontr (cons carwff2 (pop ast2)))))
			    ((label-q carwff1) (throwfail "Illegal Label."))
			    ((label-q carwff2) (throwfail "Illegal Label."))
			    ((lsymbol-q carwff1)
			     (cond ((lsymbol-q carwff2) (return nil))
				   ((boundwff-q carwff2) (return nil))
				   (t (collect2))))
			    ((lsymbol-q carwff2)
			     (cond ((boundwff-q carwff2) (return nil))
				   (t (collect1))))
			    ((boundwff-q carwff1)
			     (cond ((boundwff-q carwff2) (return nil))
				   (t (collect2))))
			    ((boundwff-q carwff2)
			     (collect1))
			    (t (collect1)
			       (collect2)))))))))


(defmacro wffeq-lnorm-basis (wff testcall)
  `(do ((argstack nil)
	(lconv-wff ,wff))
       ((or (label-q lconv-wff) (lsymbol-q lconv-wff) (boundwff-q lconv-wff))
	(if argstack nil ,testcall))
     (if (lambda-bd-p (car lconv-wff))
	 (progn
	  (push (cdr lconv-wff) argstack)
	  (setq lconv-wff (car lconv-wff))
	  (do ()
	      ((not (lambda-bd-p lconv-wff)))
	    (if argstack
		(setq lconv-wff (lcontr (cons lconv-wff (pop argstack))))
		(return nil))))
	 (progn
	  (push (cdr lconv-wff) argstack)
	  (setq lconv-wff (car lconv-wff))))))

(defun wffeq-lnorm-lsym1 (lsym1 wff2 varstack)
  (wffeq-lnorm-basis wff2
		     (if (lsymbol-q lconv-wff)
			 (true-correspondence lsym1 lconv-wff varstack)
			 nil)))

(defun wffeq-lnorm-lsym2 (wff1 lsym2 varstack)
  (wffeq-lnorm-basis wff1
		     (if (lsymbol-q lconv-wff)
			 (true-correspondence lconv-wff lsym2 varstack)
			 nil)))

(defun wffeq-lnorm-bdwff1 (bdwff1 wff2 varstack)
  (wffeq-lnorm-basis wff2 (if (boundwff-q lconv-wff)
			      (wffeq-lnorm1 bdwff1 lconv-wff varstack nil nil)
			      nil)))

(defun wffeq-lnorm-bdwff2 (wff1 bdwff2 varstack)
  (wffeq-lnorm-basis wff1 (if (boundwff-q lconv-wff)
			      (wffeq-lnorm1 lconv-wff bdwff2 varstack nil nil)
			      nil)))


(defwffop wffeq-def
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (resulttype boolean)
  (multiple-recursion t)
  (mhelp "Tests for equality modulo definitions, lambda conversion and
alphabetic change of bound variables."))

(defun wffeq-def (wff1 wff2)
  (or (common-defn-lam-reduct-p wff1 wff2)
      (common-defn-lam-reduct-p (acons 'AND wff1 wff1) wff2)
      (common-defn-lam-reduct-p (acons 'OR wff1 wff1) wff2)
      (common-defn-lam-reduct-p wff1 (acons 'AND wff2 wff2))
      (common-defn-lam-reduct-p wff1 (acons 'OR wff2 wff2))))

(defwffop wffeq-defeq
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (resulttype boolean)
  (multiple-recursion t)
  (mhelp "Tests for equality modulo definitions, lambda conversion,
alphabetic change of bound variables and the definition of the symbol = ."))

(defun wffeq-defeq (wff1 wff2)
  (or (common-defn-eq-lam-reduct-p wff1 wff2)
      (common-defn-eq-lam-reduct-p (acons 'AND wff1 wff1) wff2)
      (common-defn-eq-lam-reduct-p (acons 'OR wff1 wff1) wff2)
      (common-defn-eq-lam-reduct-p wff1 (acons 'AND wff2 wff2))
      (common-defn-eq-lam-reduct-p wff1 (acons 'OR wff2 wff2))))

(defwffop inmost-gar 
  (argtypes gwff)
  (argnames wff)
  (applicable-p always-true)
  (resulttype gwff)
  (mhelp
   "Returns the head of a wff. This will be a logical symbol or a bound wff."))

(defun inmost-gar (wff)
  (cond ((label-q wff) (apply-label wff (inmost-gar wff)))
	((lsymbol-q wff) wff)
	((boundwff-q wff) wff)
	(t (inmost-gar (gar wff)))))

(defwffrec wffeq-def1
  (argnames wff1 wff2 varstack switch)
  (multiple-recursion t))

(defun wffeq-def-lsym-bind-chk (lsym bind varstack switch)
  (cond ((anyabbrev-q lsym)
	 (wffeq-def1 bind (get-defn-1 lsym) varstack (not switch)))
	((anyabbrev-q (binding bind))
	 (wffeq-def1 (get-def-binder1 bind) lsym varstack (not switch)))
	(t nil)))

;;; Depends crucially on INSTANTIATE-1 instantiating the inmost-gar
;;; if it is an abreviation. If the order in which INSTANTIATE-1 instantiates
;;; is changed, another instantiation function should be used.

(defun wffeq-def1 (wff1 wff2 varstack switch)
  (declare (special auto::min-quant-etree))
  (let ((lsym-1 (lsymbol-q wff1))
	(lsym-2 (lsymbol-q wff2))
	(bdwff-1 (boundwff-q wff1))
	(bdwff-2 (boundwff-q wff2)))
    (cond ((label-q wff1)
	   (apply-label wff1 (wffeq-def1 wff1 wff2 varstack switch)))
	  ((label-q wff2)
	   (apply-label wff2 (wffeq-def1 wff1 wff2 varstack switch)))
	  ((and lsym-1 lsym-2)
	   (cond ((not (type-equal wff1 wff2)) nil) ; Saves lots of work
;; One is a variable	
		 ((or (anypropsym-q wff1) (anypropsym-q wff2))
		  (if switch
		      (true-correspondence wff2 wff1 varstack)
		      (true-correspondence wff1 wff2 varstack)))
;; Else, we need not worry about ab-changes
		 ((equal wff1 wff2))
;; Not immediately equal, so we break apart defs, alternating between wffs
		 ((anyabbrev-q wff1) 
		  (wffeq-def1 wff2 (get-defn-1 wff1) varstack (not switch)))
		 ((anyabbrev-q wff2) 
		  (wffeq-def1 (get-defn-1 wff2) wff1 varstack (not switch)))
;; Else they can't be equal
		 (t nil)))
;; Check two boundwffs
	  ((and bdwff-1 bdwff-2)
	   (or (and (eq (binding wff1) (binding wff2))
		    (type-equal (bindvar wff1) (bindvar wff2))
		    (wffeq-def1
		     (gdr wff1) (gdr wff2)
		     (cons (if switch
			       (cons (bindvar wff2) (bindvar wff1))
			       (cons (bindvar wff1) (bindvar wff2)))
			   varstack) switch))
	       (and (anyabbrev-q (binding wff1))
		    (wffeq-def1 
		     wff2 (get-def-binder1 wff1) varstack (not switch)))
	       (and (anyabbrev-q (binding wff2))
		    (wffeq-def1 
		     (get-def-binder1 wff2) wff1 varstack (not switch)))))
;; If one LSYMBOL and one BDWFF, then inst LSYMBOL def first
	  ((and lsym-1 bdwff-2)
	   (wffeq-def-lsym-bind-chk wff1 wff2 varstack switch))
	  ((and bdwff-1 lsym-2)
	   (wffeq-def-lsym-bind-chk wff2 wff1 varstack (not switch)))
;;; Check for equality of GAR's and GDR's [GDR first since misalignment will
;;; often cause quick rejection from inconsistent types, and GDR types are
;;; shorter]
	  ((and (not bdwff-1) (not bdwff-2) (not lsym-1) (not lsym-2)
		(wffeq-def1 (gdr wff1) (gdr wff2) varstack switch)
		(wffeq-def1 (gar wff1) (gar wff2) varstack switch)))
;;; Check to see if another alignment of arguments brings equality.
;;; Occurrences of lambda terms at the head are the only way to re-align
;;; arguments. These may come directly or via definitions.
;;;
;;; The remaining cases:
;;;  1. an application and a bdwff
;;;  2. an application and a lsymbol
;;;  3. two applications
;;;
	  (t
	   (let* ((igar1 (inmost-gar wff1))
		  (igar2 (inmost-gar wff2))
		  (lambda-i-1 (lambda-bd-p igar1))
		  (bdwff-i-1 (boundwff-q igar1))
		  (anyabb-i-1 (anyabbrev-p igar1)))
;;; Lambda-reduce : Must be an application, i.e. beware of case 1 above
; 9/21/99 cebrown -- split the first case into two cases:  where the first
; contains a redex and where the second contains a redex.  Before this was
; one case that had an incorrect test inside the case to decide which side to
; reduce.  (This led to an infinite loop in a case like:
;            [lambda x(A) x]  =?  [[lambda y(AA) y] [lambda x(A) x]]
	     (cond ((and (not bdwff-1) lambda-i-1)
		    (wffeq-def1 (lambda-norm wff1) wff2 varstack switch))
		   ((and (not bdwff-2) (lambda-bd-p igar2))
		    (wffeq-def1 (lambda-norm wff2) wff1 varstack
				(not switch)))
;;; Check for functional abbreviation: symbol
		   ((or anyabb-i-1 (anyabbrev-p igar2))
		    (if (and (module-loaded-p 'auto-basic) auto::min-quant-etree)
			(if (not anyabb-i-1)
			    (wffeq-def1 (lnorm (instantiate-1 wff2)) wff1 varstack
					(not switch))
			  (wffeq-def1 (lnorm (instantiate-1 wff1)) wff2 varstack
				      switch))
		      (if (not anyabb-i-1)
			    (wffeq-def1 (instantiate-1 wff2) wff1 varstack
					(not switch))
			(wffeq-def1 (instantiate-1 wff1) wff2 varstack
				    switch))))
;;; Check for functional abbreviation: binder
		   ((or (and bdwff-i-1
			     (anyabbrev-q (binding igar1)))
			(and (boundwff-q igar2)
			     (anyabbrev-q (binding igar2))))
		    (if (and (module-loaded-p 'auto-basic) auto::min-quant-etree)
			(if (or (not bdwff-i-1)
				(not (anyabbrev-q (binding igar1))))
			    (wffeq-def1 (lnorm (instantiate-1 wff2)) wff1 varstack
					(not switch))
			  (wffeq-def1 (lnorm (instantiate-1 wff1)) wff2 varstack
				      switch))
		      (if (or (not bdwff-i-1)
			      (not (anyabbrev-q (binding igar1))))
			  (wffeq-def1 (instantiate-1 wff2) wff1 varstack
				      (not switch))
			(wffeq-def1 (instantiate-1 wff1) wff2 varstack
				    switch))))
;;; There is no way to re-align the arguments. The wffs are not equal.
		   (t nil)))))))

; a new way of checking for equality of wff's up to lam/defns/equality
; that gives the chain of rewrites.  This replaces wffeq-def1 above
; which has problems.  In particular, wffeq-def1 didn't work properly with eta:
; wffeq-def1 would return NIL with an input like "lambda x . P x" and "P".
; - cebrown 7/4/01
(defun common-defn-lam-reduct (wff1 wff2 &optional (lcontr-defs t))
  (let ((expand-equalities nil)
	(inst-all nil))
    (declare (special expand-equalities inst-all))
    (catch 'wffs-do-not-match
      (common-defn-lam-reduct-lam wff1 wff2 nil nil lcontr-defs))))

(defun common-defn-eq-lam-reduct (wff1 wff2 &optional (lcontr-defs t))
  (let ((expand-equalities t)
	(inst-all nil))
    (declare (special expand-equalities inst-all))
    (catch 'wffs-do-not-match
      (common-defn-lam-reduct-lam wff1 wff2 nil nil lcontr-defs))))

(defun common-defn-lam-reduct-p (wff1 wff2 &optional (lcontr-defs t))
  (let ((expand-equalities nil)
	(inst-all t))
    (declare (special expand-equalities inst-all))
    (catch 'wffs-do-not-match
      (common-defn-lam-reduct-lam wff1 wff2 nil nil lcontr-defs))))

(defun common-defn-eq-lam-reduct-p (wff1 wff2 &optional (lcontr-defs t))
  (let ((expand-equalities t)
	(inst-all t))
    (declare (special expand-equalities inst-all))
    (catch 'wffs-do-not-match
      (common-defn-lam-reduct-lam wff1 wff2 nil nil lcontr-defs))))

(defun common-defn-eq-refl=-lam-reduct (wff1 wff2 &optional (lcontr-defs t))
  (let ((expand-equalities 'refl)
	(inst-all nil))
    (declare (special expand-equalities inst-all))
    (catch 'wffs-do-not-match
      (common-defn-lam-reduct-lam wff1 wff2 nil nil lcontr-defs))))

(defun common-defn-lam-reduct-lam (wff1 wff2 blocked-pos1 blocked-pos2
					&optional (lcontr-defs t))
  (let* ((lwff1 (lnorm wff1))
	 (lwff2 (lnorm wff2))
	 (c (common-defn-lam-reduct-defn lwff1 lwff2
					 blocked-pos1 blocked-pos2 lcontr-defs)))
    (cons (if (wffeq lwff1 wff1)
	      (car c)
	    (cons wff1 (cons (list 'LAMBDA) (car c))))
	  (if (wffeq lwff2 wff2)
	      (cdr c)
	    (cons wff2 (cons (list 'LAMBDA) (cdr c)))))))

(defun common-defn-lam-reduct-defn (wff1 wff2 blocked-pos1 blocked-pos2
					 &optional (lcontr-defs t))
  (declare (special expand-equalities inst-all))
  (if (wffeq wff1 wff2)
      (cons (list wff1) (list wff2))
    (if (wffeq-ab wff1 wff2)
	(cons (list wff1 (list 'AB) wff2) (list wff2))
      (multiple-value-bind
       (h1 pos1 h2 pos2)
       (first-unblocked-pos wff1 wff2 blocked-pos1 blocked-pos2
			     (if expand-equalities
				 (if (eq expand-equalities 'refl)
				     #'(lambda (w)
					 (or (and (boundwff-p w)
						  (anyabbrev-q (binding w))) ; MUST use -q here
					     (and (anyabbrev-p w) (neq w 'EQUIV))
					     (equality-p w)
					     (and (equals-p w)
						  (wffeq (cdar w) (cdr w)))))
				   #'(lambda (w)
				       (or (and (boundwff-p w)
						(anyabbrev-q (binding w))) ; MUST use -q here
					   (and (anyabbrev-p w) (neq w 'EQUIV))
					   (equality-p w))))
			       #'(lambda (w)
				   (or (and (boundwff-p w)
					    (anyabbrev-q (binding w))) ; MUST use -q here
				       (and (anyabbrev-p w) (neq w 'EQUIV))))))
       (if h1
	   (or (and (not inst-all)
		    (catch 'wffs-do-not-match
		      (common-defn-lam-reduct-defn
		       wff1 wff2 (cons pos1 blocked-pos1) blocked-pos2 lcontr-defs)))
	       (if (equality-p h1)
		   (or (catch 'wffs-do-not-match
			 (let* ((dwff1 (instantiate-position wff1 pos1 'LEIBNIZ))
				(c (common-defn-lam-reduct-lam dwff1 wff2
							       blocked-pos1 blocked-pos2
							       lcontr-defs)))
			   (cons (cons wff1 (cons (cons 'LEIBNIZ pos1) (car c)))
				 (cdr c))))
		       (if (or (consp (cdr (unabbreviated-type h1)))
			       (eq (cdr (unabbreviated-type h1)) 'O))
			   (let* ((dwff1 (instantiate-position wff1 pos1 'EXTENS))
				  (c (common-defn-lam-reduct-lam
				      dwff1 wff2
				      blocked-pos1 blocked-pos2 lcontr-defs)))
			     (cons (cons wff1 (cons (cons 'EXTENS pos1) (car c)))
				   (cdr c)))
			 (throw 'wffs-do-not-match nil)))
		 (if (and (equals-p h1)
			  (wffeq (cdar h1) (cdr h1)))
		     (let* ((dwff1 (instantiate-position wff1 pos1 'REFL))
			    (c (common-defn-lam-reduct-defn dwff1 wff2
							    blocked-pos1 blocked-pos2
							    lcontr-defs)))
		       (cons (cons wff1 (cons (cons 'REFL pos1) (car c))) (cdr c)))
		   (if (boundwff-p h1) ; eg, EXISTS1
		       (let* ((dwff1 (instantiate-position wff1 pos1 'BINDER))
			      (c (common-defn-lam-reduct-lam dwff1 wff2
							     blocked-pos1 blocked-pos2
							     lcontr-defs)))
			 (cons (cons wff1 (cons (cons 'BINDER pos1) (car c))) (cdr c)))
		     (let* ((dwff1 (instantiate-position wff1 pos1 'DEFN))
			    (c (common-defn-lam-reduct-lam dwff1 wff2
							   blocked-pos1 blocked-pos2
							   lcontr-defs)))
		       (cons (cons wff1 (cons (cons 'DEFN pos1) (car c))) (cdr c)))))))
	 (if h2
	     (or (and (not inst-all)
		      (catch 'wffs-do-not-match
			(common-defn-lam-reduct-defn
			 wff1 wff2 pos1 (cons pos2 blocked-pos2)
			 lcontr-defs)))
		 (if (equality-p h2)
		     (or (catch 'wffs-do-not-match
			   (let* ((dwff2 (instantiate-position wff2 pos2 'LEIBNIZ lcontr-defs))
				  (c (common-defn-lam-reduct-lam
				      wff1 dwff2
				      blocked-pos1 blocked-pos2
				      lcontr-defs)))
			     (cons (car c) (cons wff2 (cons (cons 'LEIBNIZ pos2) 
							    (cdr c))))))
			 (if (or (consp (cdr (unabbreviated-type h2)))
				 (eq (cdr (unabbreviated-type h2)) 'O))
			     (let* ((dwff2 (instantiate-position wff2 pos2 'EXTENS lcontr-defs))
				    (c (common-defn-lam-reduct-lam
					wff1 dwff2
					blocked-pos1 blocked-pos2
					lcontr-defs)))
			       (cons (car c) (cons wff2 (cons (cons 'EXTENS pos2) (cdr c)))))
			   (throw 'wffs-do-not-match nil)))
		   (if (and (equals-p h2)
			    (wffeq (cdar h2) (cdr h2)))
		       (let* ((dwff2 (instantiate-position wff2 pos2 'REFL))
			      (c (common-defn-lam-reduct-defn wff1 dwff2
							      blocked-pos1 blocked-pos2
							      lcontr-defs)))
			 (cons (car c)
			       (cons wff2 (cons (cons 'REFL pos2) (cdr c)))))
		     (if (boundwff-p h2) ; eg, EXISTS1
			 (let* ((dwff2 (instantiate-position wff2 pos2 'BINDER))
				(c (common-defn-lam-reduct-lam wff1 dwff2
							       blocked-pos1 blocked-pos2
							       lcontr-defs)))
			   (cons (car c) 
				 (cons wff2 (cons (cons 'BINDER pos2) (cdr c)))))
		       (let* ((dwff2 (instantiate-position wff2 pos2 'DEFN lcontr-defs))
			      (c (common-defn-lam-reduct-lam wff1 dwff2
							     blocked-pos1 blocked-pos2
							     lcontr-defs)))
			 (cons (car c) (cons wff2 (cons (cons 'DEFN pos2) (cdr c)))))))))
	   (throw 'wffs-do-not-match nil)))))))

(defun instantiate-position (wff pos kind &optional (lcontr-defs t))
  (let ((wff2
	 (if pos
	     (if (car pos)
		 (cons (car wff) (instantiate-position (cdr wff) (cdr pos) kind))
	       (cons (instantiate-position (car wff) (cdr pos) kind) (cdr wff)))
	   (case kind
		 (REFL 'TRUTH)
		 (BINDER (get-def-binder1 wff))
		 (DEFN (get-defn-1 wff))
		 (EXTENS (if (eq (cdr (unabbreviated-type wff)) 'O)
			     'EQUIV
			   (instantiate-equality-extensional wff)))
		 (LEIBNIZ (instantiate-equality-basic wff))))))
    (if (or (not lcontr-defs) (member t pos))
	wff2
      (if (and (consp wff2) (lambda-bd-p (car wff2)))
	  (lcontr wff2)
	wff2))))

(defun first-unblocked-pos (wff1 wff2 blocked-pos1 blocked-pos2 pred &optional pos varstack)
  (if (and (funcall pred wff1)
	   (not (member pos blocked-pos1 :test #'equal)))
      (values wff1 pos nil nil)
    (if (and (funcall pred wff2)
	     (not (member pos blocked-pos2 :test #'equal)))
	(values nil nil wff2 pos)
      (if (boundwff-p wff1)
	  (if (boundwff-p wff2)
	      (if (and (equal (binder wff1) (binder wff2))
		       (type-equal (bindvar wff1) (bindvar wff2)))
		  (first-unblocked-pos
		   (cdr wff1) (cdr wff2) blocked-pos1 blocked-pos2 
		   pred
		   (append pos (list t))
		   (acons (bindvar wff1) (bindvar wff2) varstack))
		(first-unblocked-pos-2 wff1 blocked-pos1 wff2 blocked-pos2 pred pos pos))
	    (first-unblocked-pos-2 wff1 blocked-pos1 wff2 blocked-pos2 pred pos pos))
	(if (boundwff-p wff2)
	    (first-unblocked-pos-2 wff1 blocked-pos1 wff2 blocked-pos2 pred pos pos)
	  (if (consp wff1)
	      (if (consp wff2)
		  (multiple-value-bind
		   (h1 pos1 h2 pos2)
		   (first-unblocked-pos (car wff1) (car wff2)
					 blocked-pos1 blocked-pos2 pred
					 (append pos (list nil)) varstack)
		   (if (or h1 h2)
		       (values h1 pos1 h2 pos2)
		     (first-unblocked-pos (cdr wff1) (cdr wff2)
					   blocked-pos1 blocked-pos2 pred
					   (append pos (list t)) varstack)))
		(first-unblocked-pos-2 wff1 blocked-pos1 wff2 blocked-pos2 pred pos pos))
	    (if (consp wff2)
		(first-unblocked-pos-2 wff1 blocked-pos1 wff2 blocked-pos2 pred pos pos)
	      (if (true-correspondence wff1 wff2 varstack)
		  (values nil nil nil nil)
		(throw 'wffs-do-not-match nil)))))))))

(defun first-unblocked-pos-1 (wff blocked pred pos fst)
  (if (and (funcall pred wff)
	   (not (member pos blocked :test #'equal)))
      (if fst
	  (values wff pos nil nil)
	(values nil nil wff pos))
    (if (boundwff-p wff)
	(throw 'wffs-do-not-match nil)
      (if (consp wff)
	  (first-unblocked-pos-1 (car wff) blocked pred (append pos (list nil)) fst)
	(throw 'wffs-do-not-match nil)))))

(defun first-unblocked-pos-2 (wff1 blocked-pos1 wff2 blocked-pos2 pred pos1 pos2)
  (if (and (funcall pred wff1)
	   (not (member pos1 blocked-pos1 :test #'equal)))
      (values wff1 pos1 nil nil)
    (if (boundwff-p wff1)
	(first-unblocked-pos-1 wff2 blocked-pos2 pred pos2 nil)
      (if (consp wff1)
	  (first-unblocked-pos-2 (car wff1) blocked-pos1 wff2 blocked-pos2
				  pred (append pos1 (list nil)) pos2)
	(first-unblocked-pos-1 wff2 blocked-pos2 pred pos2 nil)))))

