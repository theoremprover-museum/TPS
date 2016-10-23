;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of EXT-DAGS)

;;;
;;; File: models  - cebrown - 3/9/04
;;; Code for standard models with base types a power of 2.

(deffile models
    (part-of semantics)
  (extension clisp)
  (mhelp "Top level for computing with standard models where the base types are powers of 2.
Usually the base types have 2 elements: 0 and 1.
In particular at type O, 0 means false (F) and 1 means true (T).
The cardinality of every type is a power of 2 and the elements
are 0,1,...,n where n is (2^k)-1 for some k.

Functions in a type (AB) are coded as integers.
Suppose the elements in type A are 0,...,n and in type B are 0,...,m.
Suppose f is a function from B to A.  Then f is determined by
its values f(0),...,f(m) and each value f(i) is between 0 and n.
The string 'f(m)...f(0)' represents a number between 0 and (n+1)^(m+1)
written in base n+1.  This number is the code for the function f in type (AB).
"))

(context subtoplevels)

(defun modelscmd-mhelp (keyword category)
  (princ-mhelp keyword category)
  (unless short-help 
    (msgf "The command format for " keyword " is:" -2 "<MODELS:>" keyword)
    (print-tps-format* keyword " "
		       (+ 5 (length (format nil "~A" keyword)))
		       (get keyword 'models-argnames)
		       (get keyword 'models-argtypes)
		       nil)))

(defun models ()
  (%catch% (modelstop)
	   (exit-inferior-top core::expand-catch-throw)))

(defun modelstop ()
  (let ((top-prompt-fn #'models-top-prompt)
	(command-interpreter #'models-command-interpreter)
	(print-* #'models-print-*)
	(top-level 'models-top))
    (declare (special top-prompt-fn command-interpreter print-* top-level command-ctree))
    (secondary-top)))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun models-top-prompt (id) (format nil "<MODELS:~A>" id))

(defun models-print-* (result) (fresh-line) (prin1 result))

(defun models-command-interpreter (cmd)
  (let ((carcmd (car cmd)))
    (setq *retrieve-stack* nil) ; just in case!
    (cond ((null cmd) nil)
	  ((and (null (cdr cmd)) (atom carcmd))
	   (cond 
	         ((integerp carcmd)
		  (throwfail "Unknown command."))
		 ((and (symbolp carcmd) (get carcmd 'modelscmd))
		  `(models-opdecode (quote ,cmd)))
		 ((and (symbolp carcmd) (get carcmd 'mexpr))
		  `(comdecode (quote ,cmd)))
		 ((and (symbolp carcmd) (get carcmd 'reviewcmd))
		  `(comdecode (quote ,cmd)))
		 ((and (symbolp carcmd) (get carcmd 'flag))
		  `(comdecode '(setflag ,@cmd)))
		 ((null expertflag)
		  (throwfail "Unknown Command or Flag."))
		 ((and (symbolp carcmd) (boundp carcmd)) carcmd)
		 ((and (symbolp carcmd) (fboundp carcmd)) cmd)
		 ((or (get carcmd 'mhelp) (get carcmd 'mhelp-fn))
		  (msg "Cannot evaluate that... calling HELP " carcmd t t)
		  `(comdecode '(help ,carcmd)))
		 (t  (throwfail ";" carcmd " - Unbound variable."))))
	  ((and expertflag (null (cdr cmd))) carcmd)
	  ((and (symbolp carcmd) (get carcmd 'modelscmd))
	   `(models-opdecode (quote ,cmd))) 
	  ((and (symbolp carcmd) (get carcmd 'mexpr))
	   `(comdecode (quote ,cmd)))
	  ((and (symbolp carcmd) (get carcmd 'reviewcmd))
	   `(comdecode (quote ,cmd)))
	  ((and (symbolp carcmd) (get carcmd 'flag))
	   `(comdecode '(set ,@cmd)))
	  ((null expertflag)
	   (throwfail "Unknown command."))
	  ((symbolp carcmd)
	   (if (fboundp carcmd) cmd
	       (throwfail ";" carcmd " - Undefined function.")))
	  (t cmd))))

(defun models-opdecode (command)
  (let ((keyword (car command))
	mainfn result)
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values keyword
		       (copy (cdr command))
		       (get keyword 'models-argtypes)
		       (mapcar #'(lambda (x) (declare (ignore x)) nil)
			       (get keyword 'models-argtypes))
		       nil
		       (get keyword 'models-defaultfns)
		       nil
		       (get keyword 'models-argnames)
		       (get keyword 'models-arghelp))
      (declare (ignore external-arglist))
      (setq mainfn (or (get keyword 'models-mainfns) keyword))
      (%catch% (setq result (apply mainfn internal-arglist))
	       (fail (complain f "Error from " mainfn ".  " 
                               core::expand-catch-throw)
		     (throwfail "Operation aborted.")))
      result)))

(defmexpr MODELS
  (argtypes )
  (defaultfns (lambda () nil))
  (mainfns models)
  (mhelp "Enter the MODELS top level for working with standard models
in which the base types (hence all types) are a power of 2."))

(defmodels leave
  (models-mainfns exit-models)
  (mhelp "Leave MODELS to the next enclosing top level."))

(defun exit-models () 
  (%throw% '|[Left MODELS.]| exit-inferior-top))

(context models)

(defmodels change-base-type
  (models-argtypes typesym posinteger)
  (models-argnames basetp num)
  (models-arghelp "Base Type" "Power of 2")
  (models-defaultfns (lambda (basetp num)
		     (list (if (eq basetp '$)
			       'CL-USER::I
			     basetp)
			   (if (eq num '$)
			       2
			     num)
			   )))
  (models-mainfns models-change-base-type)
  (mhelp "Change the number of elements in a base type.
This must be a power of 2."))

(defun models-change-base-type (basetp num)
  (let ((b (- (integer-length num) 1)))
    (unless (symbolp basetp)
      (throwfail "Cannot change the cardinality of the function type " (basetp . typesym)))
    (when (eq basetp 'O)
      (throwfail "Cannot change the cardinality of the type " (basetp . typesym) " of truth values"))
    (unless (= (ash 1 b) num)
      (throwfail num " is not a power of 2 - " (basetp . typesym) " unchanged."))
    (let ((a (assoc basetp *models-base-nbits*)))
      (when a
	(setq *models-base-nbits* (remove a *models-base-nbits*)))
      (unless (= b 1)
	(push (cons basetp b) *models-base-nbits*))
      nil)))

; Code for Standard 2 Elt model (and others where base type is a power of 2)
; nbits = log2 of size of codomain
; x = nat num in domain
; f = nat num in nbits
(defun models-apply (nbits f x)
  (let ((z (- nbits))
	(m (ash 1 nbits)))
    (dotimes (i x)
      (setq f (ash f z)))
    (mod f m)))

(defun tp2-nbits (tp)
  (declare (special *models-base-nbits*))
  (if (consp tp)
      (* (tp2-nbits (car tp)) (tp2-size (cdr tp)))
    (let ((b (assoc tp *models-base-nbits*)))
      (if (and b (integerp (cdr b)) (>= (cdr b) 0))
	  (cdr b)
	1))))

(defun tp2-size (tp)
  (let ((size (ash 1 (tp2-nbits tp))))
    (if (> size MAX-DOMAIN-SIZE)
	(throwfail "Size of domain " (tp . typesym) " is bigger than MAX-DOMAIN-SIZE")
      size)))

(defun models-const-fn (f nbitsc sized)
  (let* ((m (ash 1 nbitsc))
	 (k (mod f m)))
    (setq f (ash f (- nbitsc)))
    (do ((i 1 (1+ i)))
	((or (>= i sized) (null k))
	 k)
      (if (= k (mod f m))
	  (setq f (ash f (- nbitsc)))
	(setq k nil)))))

(defun assoc-fn-to-models-1 (dom nbits alist)
  (let ((j dom)
	(ret 0))
    (dotimes (i dom ret)
      (decf j)
      (let* ((a (assoc j alist :test #'equal))
	     (v (cdr a)))
	(unless (and (integerp v) (>= v 0))
	  (if a
	      (warning "Bad Value " v " For " j " Specified")
	    (warning "No Value For " j " Specified"))
	  (setq v 0))
	(setq ret (+ (ash ret nbits) v))))))

(defun assoc-fn-to-models (domtp codtp alist)
  (assoc-fn-to-models-1 (tp2-size domtp) (tp2-nbits codtp) alist))

(defun models-equals (tp)
  (let* ((n (tp2-size tp))
	 (j n)
	 (ret 0))
    (dotimes (i n ret)
      (decf j)
      (let ((v (ash 1 j)))
	(setq ret (+ (ash ret n) v))))))

(defun models-not-equals (tp)
  (let* ((n (tp2-size tp))
	 (j n)
	 (m (- (ash 1 n) 1))
	 (ret 0))
    (dotimes (i n ret)
      (decf j)
      (let ((v (- m (ash 1 j))))
	(setq ret (+ (ash ret n) v))))))

(defun models-pi (tp)
  (ash 1 (- (tp2-size (cons 'O tp)) 1)))

(defun models-sig (tp)
  (- (tp2-size (cons 'O (cons 'O tp))) 2))

(defun models-log (lc)
  (case lc
    (XOR 6)
    (AND 8)
    (EQUIV 9)
    (IMPLIES 11)
    (OR 14)
    (NOT 1)
    (TRUTH 1)
    (FALSEHOOD 0)))

(defmodels interpret
  (models-argtypes gwff)
  (models-argnames wff)
  (models-arghelp "Formula to Interpret")
  (models-defaultfns (lambda (wff)
		     (list wff)))
  (models-mainfns models-interpret)
  (mhelp "Interpret a formula in the current model.  The evaluation is lazy so
if a function is constant, the argument is not evaluated.  The flags
MAX-BINDER-COMPUTATION and MAX-DOMAIN-SIZE bound how complicated the
wff can be before interpret will fail.

SEE ALSO: ASSIGN-VAR, SHOW-ASSIGNMENTS, REMOVE-ALL-ASSIGNMENTS, UNASSIGN-VAR,
  MAX-BINDER-COMPUTATION, MAX-DOMAIN-SIZE"))

(defmodels probability
  (models-argtypes gwff0)
  (models-argnames wff)
  (models-arghelp "Formula to Interpret")
  (models-defaultfns (lambda (wff)
		     (list wff)))
  (models-mainfns models-probability)
  (mhelp "Computes the probability that a formula is true in the model.
Assigned variables are considered fixed.  All unassigned variables are
allowed to vary over the appropriate domains.  The probability is the
number of values for these unassigned variables for which the wff is true
over the total number of values for the unassigned variables.

SEE ALSO: COND-PROBABILITY, INTERPRET, MAX-BINDER-COMPUTATION, MAX-DOMAIN-SIZE"))

(defmodels cond-probability
  (models-argtypes gwff0 gwff0)
  (models-argnames wff1 wff2)
  (models-arghelp "Formula to Assume" "Formula to Show")
  (models-defaultfns (lambda (wff1 wff2)
		     (list wff1 wff2)))
  (models-mainfns models-cond-probability)
  (mhelp "Computes the conditional probability that a wff2 is true if a wff1 is
true in the model.  Assigned variables are considered fixed.  All
unassigned variables are allowed to vary over the appropriate domains.
The probability is the number of values for these unassigned variables
for which wff1 and wff2 are true over the number of values for which
wff1 is true.

SEE ALSO: PROBABILITY, INTERPRET, MAX-BINDER-COMPUTATION, MAX-DOMAIN-SIZE"))

(defmodels assign-var
  (models-argtypes gvar)
  (models-argnames v)
  (models-arghelp "Variable to Assign")
  (models-defaultfns (lambda (v)
		     (list v)))
  (models-mainfns models-assign-var)
  (mhelp "Assign a value to a variable in the current model.

SEE ALSO: REMOVE-ALL-ASSIGNMENTS, UNASSIGN-VAR, INTERPRET, SHOW-ASSIGNMENTS"))

(defmodels unassign-var
  (models-argtypes gvar)
  (models-argnames v)
  (models-arghelp "Variable to Remove Assignment")
  (models-defaultfns (lambda (v)
		     (list v)))
  (models-mainfns models-unassign-var)
  (mhelp "Remove an assignment for a variable in the current model.

SEE ALSO: REMOVE-ALL-ASSIGNMENTS, ASSIGN-VAR, INTERPRET, SHOW-ASSIGNMENTS"))

(defmodels remove-all-assignments
  (models-mainfns models-remove-all-assignments)
  (mhelp "Remove all assignments for variables in the current model.

SEE ALSO: UNASSIGN-VAR, ASSIGN-VAR, INTERPRET, SHOW-ASSIGNMENTS"))

(defmodels solve
  (models-argtypes gvarlist gvarlist gwff0)
  (models-argnames invars outvars wff)
  (models-arghelp "Input Variables" "Output Variables" "Proposition")
  (models-defaultfns (lambda (invars outvars wff)
		     (list (if (eq invars '$) nil invars)
			   (if (eq outvars '$) nil outvars)
			   wff)))
  (models-mainfns models-solve)
  (mhelp "Solve for values for the output variables for any values of the input
variables so that the given proposition is true.

If the domains involved are large, TPS will ask the user whether to
print the values to the screen or save them to a file.

TPS will always tell the user whether there are no solutions for any
inputs, solutions for some but not all inputs, solutions for all
inputs and whether there are unique solutions for some inputs."))

(defun models-interpret (wff)
  (let* ((nwff (lnorm wff))
	 (fvars (uninterp-syms-of nwff)))
    (dolist (v fvars)
      (msgf "Enter a Value for " (v . gwff))
      (models-assign-var v))
    (let ((val (models-interp nwff nil 1 t))
	  (utp (unabbreviated-type nwff)))
      (cond ((eq utp 'O)
	     (msgf "Return Value: " (elt2-name utp val) t)) ; T or F
	    ((consp utp)
	     (msgf "Return Value: " val t)
	     (msgf "Representing Value: " (elt2-name utp val) t)
	     (msgf "Recursive Representation: " (elt2-name utp val t) t))
	    (t
	     (msgf "Return Value: " val t))))))

(defun models-probability (wff)
  (let ((fvars (uninterp-syms-of wff)))
    (if fvars
	(progn
	  (msgf "Varying ")
	  (dolist (v fvars)
	    (msg (v . gwff) " "))
	  (msg t))
      (msgf "There are no unassigned variables.  Probability will be 0 or 1."))
    (let* ((models-prob-total-true 0)
	   (models-prob-total 0))
      (declare (special models-prob-total-true models-prob-total))
      (models-probability-1 wff fvars)
      (when (> models-prob-total 0) ; should always happen
	(msgf "Probability: " (* 1.0 (/ models-prob-total-true models-prob-total)))))))

(defun models-cond-probability (wff1 wff2)
  (let ((fvars (union (uninterp-syms-of wff1) (uninterp-syms-of wff2))))
    (if fvars
	(progn
	  (msgf "Varying ")
	  (dolist (v fvars)
	    (msg (v . gwff) " "))
	  (msg t))
      (msgf "There are no unassigned variables."))
    (let* ((models-prob-total-true 0)
	   (models-prob-total 0))
      (declare (special models-prob-total-true models-prob-total))
      (models-cond-probability-1 wff1 wff2 fvars)
      (if (> models-prob-total 0) ; should always happen
	  (msgf "Conditional Probability: " (* 1.0 (/ models-prob-total-true models-prob-total)))
	(msgf (wff1 . gwff) t " is never true")))))

(defun models-probability-1 (wff fvars &optional val (numtuples 1))
  (declare (special models-prob-total-true models-prob-total))
  (if fvars
      (let* ((v (car fvars))
	     (tp (unabbreviated-type v))
	     (n (tp2-size tp))
	     (numtuples2 (* n numtuples)))
	(when (> numtuples2 MAX-BINDER-COMPUTATION)
	  (throwfail "Too Many Possibilities to Solve - see the flag MAX-BINDER-COMPUTATION"))
	(dotimes (i n)
	  (models-probability-1 wff (cdr fvars) (acons v i val) numtuples2)))
    (progn
      (incf models-prob-total)
      (when (equal (models-interp wff val numtuples nil) 1)
	(incf models-prob-total-true)))))

(defun models-cond-probability-1 (wff1 wff2 fvars &optional val (numtuples 1))
  (declare (special models-prob-total-true models-prob-total))
  (if fvars
      (let* ((v (car fvars))
	     (tp (unabbreviated-type v))
	     (n (tp2-size tp))
	     (numtuples2 (* n numtuples)))
	(when (> numtuples2 MAX-BINDER-COMPUTATION)
	  (throwfail "Too Many Possibilities to Solve - see the flag MAX-BINDER-COMPUTATION"))
	(dotimes (i n)
	  (models-cond-probability-1 wff1 wff2 (cdr fvars) (acons v i val) numtuples2)))
    (progn
      (when (equal (models-interp wff1 val numtuples nil) 1)
	(incf models-prob-total)
	(when (equal (models-interp wff2 val numtuples nil) 1)
	  (incf models-prob-total-true))))))

(defun models-remove-all-assignments ()
  (declare (special *models-var-interps*))
  (setq *models-var-interps* nil))

(defun models-unassign-var (v)
  (models-unassign-var-1 v)
  (msgf (v . gwff) " now has no value."))

(defun models-unassign-var-1 (v)
  (declare (special *models-var-interps*))
  (let ((a (assoc v *models-var-interps*)))
    (setq *models-var-interps* (remove a *models-var-interps*))))

(defun models-assign-var (v)
  (declare (special *models-var-interps*))
  (models-unassign-var-1 v) ; remove any current assignment
  (let* ((tp (unabbreviated-type v))
	 (n (tp2-size tp))
	 (val 0)
	 (bad t))
    (loop while bad do
	  (prompt-read val nil
		       (msgf "Enter a value between 0 and " (- n 1))
		       'integer+
		       val
		       ((? (mhelp 'integer+)) (?? (msgf "Value for " (v . gwff)))))
	  (when (and (integerp val) (>= val 0) (< val n))
	    (setq bad nil)))
    (push (cons v val) *models-var-interps*)
    val))

(defun models-print-valuation (val)
  (when val
    (let ((i 10))
      (msg ((caar val) . gwff) " " (cdar val))
      (dolist (v (cdr val))
	(setq i (+ i 10))
	(if (> i rightmargin)
	    (progn
	      (msg t)
	      (setq i 10))
	  (msg ", "))
	(msg ((car v) . gwff) " " (cdr v))))
    (msg t)))

; gwff of some type
; val - assoc list of <propsym> . <int> where all propsyms (consts/vars) in gwff should be on val
; returns integer member of type of gwff
(defun models-interp (gwff &optional val (numelt-comps 1) qu-for-vals)
  (declare (special *models-var-interps*))
  (cond ((boundwff-p gwff)
	 (cond ((a-bd-wff-p gwff)
		(let* ((bv (bindvar gwff))
		       (tp (unabbreviated-type bv))
		       (n (tp2-size tp))
		       (fa nil)
		       (numelt-comps2 (* n numelt-comps)))
		  (when (> numelt-comps2 MAX-BINDER-COMPUTATION)
		    (throwfail "Too Many Binders - see the flag MAX-BINDER-COMPUTATION"))
		  (do ((x 0 (1+ x)))
		      ((or (= x n) fa)
		       (if fa
			   0
			 1))
		    (unless (equal (models-interp (cdr gwff) (acons bv x val) numelt-comps2 qu-for-vals) 1)
		      (setq fa t)))))
	       ((e-bd-wff-p gwff)
		(let* ((bv (bindvar gwff))
		       (tp (unabbreviated-type bv))
		       (n (tp2-size tp))
		       (tr nil)
		       (numelt-comps2 (* n numelt-comps)))
		  (when (> numelt-comps2 MAX-BINDER-COMPUTATION)
		    (throwfail "Too Many Quantifiers - see the flag MAX-BINDER-COMPUTATION"))
		  (do ((x 0 (1+ x)))
		      ((or (= x n) tr)
		       (if tr
			   1
			 0))
		    (unless (equal (models-interp (cdr gwff) (acons bv x val) numelt-comps2 qu-for-vals) 0)
		      (setq tr t)))))
	       ((lambda-bd-p gwff)
		(let* ((bv (bindvar gwff))
		       (domtp (unabbreviated-type bv))
		       (dom (tp2-size domtp))
		       (body (cdr gwff))
		       (codtp (unabbreviated-type body))
		       (nbits (tp2-nbits codtp))
		       (j dom)
		       (ret 0)
		       (numelt-comps2 (* dom numelt-comps)))
		  (when (> numelt-comps2 MAX-BINDER-COMPUTATION)
		    (throwfail "Too Many Binders - see the flag MAX-BINDER-COMPUTATION"))
		  (dotimes (i dom ret)
		    (decf j)
		    (let ((v (models-interp body (acons bv j val) numelt-comps2 qu-for-vals)))
		      (setq ret (+ (ash ret nbits) v))))))
	       ((anyabbrev-q (binder gwff))
		(models-interp (lnorm (instantiate-1 gwff)) val numelt-comps qu-for-vals))
	       (t
		(throwfail "Cannot interpret binder for " (gwff . gwff)))))
	((equals-p gwff)
	 (let ((x (models-interp (cdar gwff) val numelt-comps qu-for-vals))
	       (y (models-interp (cdr gwff) val numelt-comps qu-for-vals)))
	   (if (equal x y)
	       1
	     0)))
	((not-p gwff)
	 (let ((x (models-interp (cdr gwff) val numelt-comps qu-for-vals)))
	   (if (equal x 0)
	       1
	     0)))
	((or-p gwff)
	 (let ((x (models-interp (cdar gwff) val numelt-comps qu-for-vals))
	       (y (models-interp (cdr gwff) val numelt-comps qu-for-vals)))
	   (max x y)))
	((and-p gwff)
	 (let ((x (models-interp (cdar gwff) val numelt-comps qu-for-vals))
	       (y (models-interp (cdr gwff) val numelt-comps qu-for-vals)))
	   (min x y)))
	((implies-p gwff)
	 (let ((x (models-interp (cdar gwff) val numelt-comps qu-for-vals))
	       (y (models-interp (cdr gwff) val numelt-comps qu-for-vals)))
	   (if (and (equal x 1) (equal y 0))
	       0
	     1)))
	((equiv-p gwff)
	 (let ((x (models-interp (cdar gwff) val numelt-comps qu-for-vals))
	       (y (models-interp (cdr gwff) val numelt-comps qu-for-vals)))
	   (if (equal x y)
	       1
	     0)))
	((anyabbrev-p (head gwff))
	 (models-interp (lnorm (instantiate-1 gwff)) val numelt-comps qu-for-vals))
	((consp gwff) ; general application
	 (let* ((f (models-interp (car gwff) val numelt-comps qu-for-vals))
		(ftp (unabbreviated-type (car gwff)))
		(nbitsc (tp2-nbits (car ftp)))
		(sized (tp2-size (cdr ftp)))
		(k (models-const-fn f nbitsc sized))) ; check if f is a constant function (no need to interp argument)
	   (if k
	       k
	     (let ((x (models-interp (cdr gwff) val numelt-comps qu-for-vals)))
	       (models-apply nbitsc f x)))))
	((member gwff '(AND EQUIV IMPLIES OR NOT TRUTH FALSEHOOD))
	 (models-log gwff))
	((equality-p gwff)
	 (let ((tp (cdr (unabbreviated-type gwff))))
	   (models-equals tp)))
	(t
	 (let ((a (or (assoc gwff val)
		      (assoc gwff *models-var-interps*))))
	   (if a
	       (cdr a)
	     (if qu-for-vals
		 (progn
		   (msgf "Enter a Value for " (gwff . gwff))
		   (models-assign-var gwff))
	       (throwfail "Cannot interpret " (gwff . gwff))))))))

(defun models-images (vl subst &optional val)
  (if vl
      (let* ((v (car vl))
	     (tp (unabbreviated-type v))
	     (n (tp2-size tp))
	     (ret nil))
	(dotimes (i n ret)
	  (setq ret (append (models-images (cdr vl) subst (acons v i val))
			    ret))))
    (list
     (list val
	   (mapcar #'(lambda (x)
		       (cons (car x)
			     (models-interp (cdr x) val)))
		   subst)))))

; vals: list of n-tuples
; vl : list of m variables
; subst: subst of n terms using vars from vl for n variables
; return: list of m-tuples of values which map into vals
(defun models-preimages (vals vl subst)
  (let ((im (models-images vl subst))
	(ret nil))
    (dolist (im1 im ret)
      (when (member (mapcar #'cdr (cadr im1))
		    vals :test #'equal)
	(push (mapcar #'(lambda (var)
			  (let ((a (assoc var (car im1))))
			    (if a
				(cdr a)
			      '*))) ; unspecified
		      vl)
	      ret)))))


(defun models-solve (invars outvars wff)
  (let ((models-invals-with-no-soln nil)
	(models-invals-with-uniq-soln nil)
	(models-is-nonuniq-soln nil)
	(total-solns 0)
	(show-solns nil))
    (declare (special models-invals-with-no-soln models-invals-with-uniq-soln models-is-nonuniq-soln))
    (let ((solns (models-solve-1 invars outvars wff)))
      (msgf "Solving Complete")
      (if models-invals-with-no-soln
	  (progn
	    (msgf (length models-invals-with-no-soln) " inputs have no solution")
	    (when invars
	      (msgf "Sample: " t)
	      (models-print-valuation (car models-invals-with-no-soln))))
	(if models-is-nonuniq-soln
	    (msgf "Every input has a solution.")
	  (msgf "Every input has a unique solution.")))
      (when models-is-nonuniq-soln
	(when models-invals-with-uniq-soln
	  (msgf (length models-invals-with-uniq-soln) " inputs have a unique solution")
	  (when invars
	    (msgf "Sample: " t)
	    (models-print-valuation (car models-invals-with-uniq-soln)))))
      (dolist (soln solns)
	(setq total-solns (+ total-solns (length (cadr soln)))))
      (msgf "There are " total-solns " solutions." t)
      (when (< total-solns 50)
	(if (< total-solns 20)
	    (setq show-solns t)
	  (setq show-solns (query "Show Them?" t))))
      (when (and show-solns (or invars outvars))
	(dolist (soln solns)
	  (when invars
	    (msgf "Input:" t)
	    (models-print-valuation (mapcar #'cons invars (car soln))))
	  (when outvars
	    (msgf "Solutions:" t)
	    (dolist (outvals (cadr soln))
	      (models-print-valuation (mapcar #'cons outvars outvals))
	      (msgf "-----" t)))))
      (when (query "Save all information to a file?" nil)
	(let ((fname nil))
	  (prompt-read fname nil
		       (msgf "FNAME (FILESPEC): Name for output file ")
		       'filespec (concatenate 'string "models-solns" 
					      (case style (scribe ".mss") (tex ".tex") (t ".rec")))
		       ((? (mhelp 'filespec)) (?? (mhelp 'filespec))))
	  (reroute-output-append
	   fname *default-pathname-defaults*
	   (msgf t "===============================" t)
	   (stringdtl)
	   (msg t "Values")
	   (when outvars
	     (msg " For")
	     (dolist (v outvars)
	       (msg " " (v . gwff)))
	     (if (> (length outvars) 5) (msg t) (msg " "))
	     (when invars
	       (msg "In Terms")))
	   (when invars
	     (msg " Of")
	     (dolist (v invars)
	       (msg " " (v . gwff))))
	   (if (> (length invars) 5) (msg t) (msg " "))
	   (msg "Such That " t)
	   (pwff wff)
	   (msg t t "Is True." t "---------------" t)
	   (unless models-invals-with-no-soln
	     (if models-is-nonuniq-soln
		 (msgf "Every input has a solution." t)
	       (msgf "Every input has a unique solution." t)))
	   (dolist (soln solns)
	     (when invars
	       (msgf "Input:" t)
	       (models-print-valuation (mapcar #'cons invars (car soln))))
	     (when outvars
	       (msgf "Solutions:" t)
	       (dolist (outvals (cadr soln))
		 (models-print-valuation (mapcar #'cons outvars outvals))
		 (msgf "-----" t))))
	   (msgf "-----------------" t)
	   (when models-invals-with-no-soln
	     (msgf (length models-invals-with-no-soln) " inputs have no solution:")
	     (dolist (invals models-invals-with-no-soln)
	       (models-print-valuation invals)
	       (msgf "-----" t)))
	   (when (and models-invals-with-uniq-soln models-is-nonuniq-soln)
	     (msgf (length models-invals-with-uniq-soln) " inputs have a unique solution:")
	     (dolist (invals models-invals-with-uniq-soln)
	       (models-print-valuation invals)
	       (msgf "-----" t)))
	   (msg t)))))))

; if there is some valuation for invars such that there are no valuations for outvars making wff true,
; return (NO-SOLN <invars-val>)
; if a failure was thrown, return 'FAILED
; otherwise, return a big list of (<invars-val> <list of <outvars-val> making wff true>)
(defun find-model-solutions (invars outvars wff)
  (let ((models-invals-with-no-soln nil)
	(models-invals-with-uniq-soln nil)
	(models-is-nonuniq-soln nil)
	(total-solns 0)
	(show-solns nil))
    (declare (special models-invals-with-no-soln models-invals-with-uniq-soln models-is-nonuniq-soln))
    (let ((solns (catch 'fail
		   (catch 'no-soln
		     (models-solve-1 invars outvars wff nil 1 :throw-on-no-soln T)))))
      (if (and (consp solns) (not (stringp (car solns))))
	  solns
	'FAILED))))

(defun models-solve-1 (invars outvars wff &optional val (numelt-comps 1) &key (throw-on-no-soln nil))
  (declare (special models-invals-with-no-soln models-invals-with-uniq-soln models-is-nonuniq-soln))
  (if invars
      (let* ((v (car invars))
	     (tp (unabbreviated-type v))
	     (n (tp2-size tp))
	     (j n)
	     (numelt-comps2 (* n numelt-comps))
	     (solns nil))
	(when (> numelt-comps2 MAX-BINDER-COMPUTATION)
	  (throwfail "Too Many Possibilities to Solve - see the flag MAX-BINDER-COMPUTATION"))
	(dotimes (i n solns)
	  (let ((solns1 (models-solve-1 (cdr invars) outvars wff (acons (car invars) (decf j) val) numelt-comps2 :throw-on-no-soln throw-on-no-soln)))
	    (setq solns
		  (append
		   (mapcar #'(lambda (soln)
			       (cons (cons j (car soln)) (cdr soln)))
			   solns1)
		   solns)))))
    (let ((solns2 (models-solve-2 outvars wff val numelt-comps)))
      (if solns2
	  (if (cdr solns2)
	      (setq models-is-nonuniq-soln t)
	    (push val models-invals-with-uniq-soln))
	(unless solns2
	  (push val models-invals-with-no-soln)))
      (if solns2
	  (list (list nil solns2))
	(if throw-on-no-soln
	    (throw 'no-soln (cons 'NO-SOLN val))
	  nil)))))

(defun models-solve-2 (outvars wff &optional val (numelt-comps 1))
  (if outvars
      (let* ((v (car outvars))
	     (tp (unabbreviated-type v))
	     (n (tp2-size tp))
	     (j n)
	     (numelt-comps2 (* n numelt-comps))
	     (solns nil))
	(when (> numelt-comps2 MAX-BINDER-COMPUTATION)
	  (throwfail "Too Many Possibilities to Solve - see the flag MAX-BINDER-COMPUTATION"))
	(dotimes (i n solns)
	  (let ((solns1 (models-solve-2 (cdr outvars) wff (acons (car outvars) (decf j) val) numelt-comps2)))
	    (setq solns
		  (append
		   (mapcar #'(lambda (soln)
			       (cons j soln))
			   solns1)
		   solns)))))
    (let ((val (models-interp wff val numelt-comps)))
      (if (equal val 1)
	  (list nil) ; the empty tuple solves it
	nil)))) ; no soln

(context wff-printing)

(defmodels psize
  (models-argtypes typesym)
  (models-argnames tp)
  (models-arghelp "Type")
  (models-defaultfns (lambda (tp)
		     (list (if (eq tp '$)
			       'CL-USER::I
			     tp))))
  (models-mainfns models-psize)
  (mhelp "Print the size of the domain of the given type.
The elements of the type are 0, . . ., n-1 where n is the size."))

(defun models-psize (tp)
  (msgf "The size of " (tp . typesym) " is " (tp2-size tp)))

(defmodels pelt
  (models-argtypes typesym integer+)
  (models-argnames tp elt)
  (models-arghelp "Type" "Integer")
  (models-defaultfns (lambda (tp elt)
		     (list (if (eq tp '$)
			       'CL-USER::I
			     tp)
			   (if (eq elt '$)
			       0
			     elt))))
  (models-mainfns models-pelt)
  (mhelp "Print the integer in notation appropriate to the given type.
For example, elements of type (OA) are printed in set notation.
The empty set is called EMPTY and the universal set is called FULL.

Constant functions are denoted by Kc.

A few special cases are T and F at type O, NOT at type (OO),
the binary connectives AND, OR, IMPLIES, EQUIV and XOR at type (OOO),
PI and SIGMA at types of the form (O(OA)),
= at types of the form (OAA) and 
ID at types of the form (AA).

EMPTY at a type (OA) corresponds to the empty set.

FULL at a type (OA) corresponds to the set of all elements of type A.

PI at a type (O(OA)) corresponds to the singleton {FULL} where FULL 
corresponds to the set of all elements of type A.

SIGMA at a type (O(OA)) corresponds to the set containing all sets of
type A except EMPTY.

For elements of low types the command PELT-REC may also be helpful.

SEE ALSO: PELT-REC"))

(defmodels pelt-rec
  (models-argtypes typesym integer+)
  (models-argnames tp elt)
  (models-arghelp "Type" "Integer")
  (models-defaultfns (lambda (tp elt)
		     (list (if (eq tp '$)
			       'CL-USER::I
			     tp)
			   (if (eq elt '$)
			       0
			     elt))))
  (models-mainfns models-pelt-rec)
  (mhelp "Print the integer in notation appropriate to the given type.
For example, elements of type (OA) are printed in set notation.
The empty set is called EMPTY and the universal set is called FULL.

Constant functions are denoted by K(c).

A few special cases are T and F at type O, NOT at type (OO), 
the binary connectives AND, OR, IMPLIES, EQUIV and XOR at type (OOO),
PI and SIGMA at types of the form (O(OA)),
= at types of the form (OAA) and 
ID at types of the form (AA).

EMPTY at a type (OA) corresponds to the empty set.

FULL at a type (OA) corresponds to the set of all elements of type A.

PI at a type (O(OA)) corresponds to the singleton {FULL} where FULL 
corresponds to the set of all elements of type A.

SIGMA at a type (O(OA)) corresponds to the set containing all sets of
type A except EMPTY.

This command is recursive.  For low types this is helpful, but
the notation becomes unwieldy for higher types.  For higher types
the command PELT is more appropriate.

SEE ALSO: PELT"))

(defmodels pelts
  (models-argtypes typesym)
  (models-argnames tp)
  (models-arghelp "Type")
  (models-defaultfns (lambda (tp)
		     (list (if (eq tp '$)
			       'CL-USER::I
			     tp))))
  (models-mainfns models-pelts)
  (mhelp "Print all the elements of the given type as both integers
and the notation of PELT.

SEE ALSO: PELT"))

(defmodels pelts-rec
  (models-argtypes typesym)
  (models-argnames tp)
  (models-arghelp "Type")
  (models-defaultfns (lambda (tp)
		     (list (if (eq tp '$)
			       'CL-USER::I
			     tp))))
  (models-mainfns models-pelts-rec)
  (mhelp "Print all the elements of the given type as both integers
and the notation of PELT-REC.

SEE ALSO: PELT-REC"))

(defun models-pelts (tp)
  (let ((size (tp2-size tp)))
    (msgf "There are " size " elements in " (tp . typesym))
    (dotimes (i size)
      (msgf i "   " (elt2-name tp i nil)))))

(defun models-pelts-rec (tp)
  (let ((size (tp2-size tp)))
    (msgf "There are " size " elements in " (tp . typesym))
    (dotimes (i size)
      (msgf i "   " (elt2-name tp i t)))))

(defun models-pelt (tp elt)
  (let ((size (tp2-size tp)))
    (unless (< elt size)
      (throwfail "The largest integer in " tp " is " (- size 1)))
    (msgf (elt2-name tp elt nil))))

(defun models-pelt-rec (tp elt)
  (let ((size (tp2-size tp)))
    (unless (< elt size)
      (throwfail "The largest integer in " tp " is " (- size 1)))
    (msgf (elt2-name tp elt t))))

; given the type tp, print the elt x in D_tp
(defun elt2-name (tp x &optional rec)
  (let ((spname (elt2-special-name tp x)))
    (if spname
	spname
      (if (consp tp)
	  (if (eq (car tp) 'O)
	      (let ((n (tp2-size (cdr tp)))
		    (ret "{")
		    (full t)
		    (qp (and (consp (cdr tp)) (eq (cadr tp) 'O)))
		    (ns nil)
		    (s nil)
		    (fst t))
		(dotimes (i n)
		  (if (equal (models-apply 1 x i) 1)
		      (progn
			(push i s)
			(setq ret (format nil "~d~d~d"
					  ret
					  (if fst (progn (setq fst nil) "") ",")
					  (if rec
					      (elt2-name (cdr tp) i t)
					    i))))
		    (progn
		      (push i ns)
		      (setq full nil))))
		(if fst
		    "EMPTY"
		  (if full
		      "FULL"
		    (if (and qp (equal (list (- n 1)) s))
			(format nil "PI")
		      (if (and qp (equal (list 0) ns))
			  (format nil "SIGMA")
			(format nil "~d}" ret))))))
	    (let ((n (tp2-size (cdr tp)))
		  (nbits (tp2-nbits (car tp)))
		  (id (equal (car tp) (cdr tp)))
		  (equ (and (consp (car tp)) (eq (caar tp) 'O) (equal (cdar tp) (cdr tp))))
		  (const nil)
		  (ret "|")
		  (fst t))
	      (dotimes (i n)
		(let ((v (models-apply nbits x i)))
		  (when id
		    (unless (equal i v)
		      (setq id nil)))
		  (when equ
		    (unless (equal v (ash 1 i))
		      (setq equ nil)))
		  (if fst 
		      (setq const v)
		    (unless (equal const v)
		      (setq const nil)))
		  (setq ret (format nil "~d~d~d>~d"
				    ret
				    (if fst (progn (setq fst nil) "") ",")
				    (if rec
					(elt2-name (cdr tp) i t)
				      i)
				    (if rec
					(elt2-name (car tp) v t)
				      v)))))
	      (if const
		  (if rec
		      (if (consp (car tp))
			  (format nil "K(~d)" (elt2-name (car tp) const t))
			(format nil "K~d" (elt2-name (car tp) const t)))
		    (format nil "K~d" const))
		(if id
		    "ID"
		  (if equ
		      "="
		    (format nil "~d|" ret))))))
	(if (eq tp 'O)
	    (cond ((equal x 0) "F")
		  ((equal x 1) "T")
		  (t (format nil "~d" x)))
	  (format nil "~d" x))))))

(defun elt2-special-name (tp x)
  (cond ((equal tp '(O . O))
	 (cond ((equal x 0) "KF")
	       ((equal x 1) "NOT")
	       ((equal x 2) "ID")
	       ((equal x 3) "KT")))
	((equal tp '((O . O) . O))
	 (cond ((equal x 6) "XOR")
	       ((equal x 8) "AND")
	       ((equal x 9) "EQUIV")
	       ((equal x 11) "IMPLIES")
	       ((equal x 14) "OR")))))

(defmodels show-assignments
  (models-mainfns models-show-assignments)
  (mhelp "Show all currently assigned values.  To see the value of any
particular variable, use INTERPRET.  To assign a value or remove an
assignment, use ASSIGN-VAR or UNASSIGN-VAR.

SEE ALSO: ASSIGN-VAR, UNASSIGN-VAR, REMOVE-ALL-ASSIGNMENTS, INTERPRET"))

(defun models-show-assignments ()
  (dolist (va *models-var-interps*)
    (let ((tp (unabbreviated-type (car va))))
      (if (or (consp tp) (eq tp 'O))
	  (msgf ((car va) . gwff) " := " (cdr va) ", i.e. " (elt2-name (unabbreviated-type (car va)) (cdr va)))
	(msgf ((car va) . gwff) " := " (cdr va))))))

(context prim-obj)

; find symbols (variables, constants) that have no fixed interpretation
; (similar to free-vars-of in wffprim.lisp)
(defun uninterp-syms-of (inwff)
  (delete-duplicates (uninterpreted-syms inwff nil)))

(defwffrec uninterpreted-syms
  (argnames gwff bind-list)
  (mhelp "Finds uninterpreted symbols (variables and constants) of a wff."))

(defun uninterpreted-syms (gwff bind-list)
  (declare (special *models-var-interps*))
  (cond ((label-q gwff) (apply-label gwff (uninterpreted-syms gwff bind-list)))
	((lsymbol-q gwff)
	 (cond ((get gwff 'abbrev) (uninterpreted-syms (get gwff 'defn) bind-list))
               (t (unless (or (abbrev-p gwff) (pmabbrev-p gwff)
			      (equality-p gwff)
			      (member gwff '(AND EQUIV IMPLIES OR NOT TRUTH FALSEHOOD))
			      (member gwff bind-list)
			      (assoc gwff *models-var-interps*))
		    (list gwff)))))
	((boundwff-q gwff) (uninterpreted-syms (cdr gwff) (cons (caar gwff) bind-list)))
	(t (nconc (uninterpreted-syms (car gwff) bind-list) 
		  (uninterpreted-syms (cdr gwff) bind-list)))))

(context log-relns)

