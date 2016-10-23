;;; -*- Mode: Lisp -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile unif-simpl
  (part-of unification)
  (extension lisp)
  (mhelp "Unification functions."))

(defun ck-new-dpairs (dpairs free-vars fr-flag)
  (do ((dpairs dpairs (cdr dpairs))
       (new-dpairs nil))
      ((null dpairs) (values fr-flag (nreverse new-dpairs)))
    (let ((2nd (cdar dpairs))
	  (first (caar dpairs)))
      (unless (and (uni-term-pos 2nd)
		   (eq (uni-term-head first) (uni-term-head 2nd))
		   (not (or (uni-term-args first) (uni-term-args 2nd))))
	(push (car dpairs) new-dpairs)
	(if fr-flag nil (if (rigid-p 2nd free-vars) (setq fr-flag t)))))))

(defflag max-substs-var
  (flagtype null-or-integer)
  (default nil)
  (subjects unification important ms98-1 transmit)
  (relevant-kids (T '(max-substs-quick max-substs-proj max-substs-proj-total)))
  (mhelp "The maximum number of substitutions allowed for any given
free variable in a dpairset. This is cumulative (i.e. if an old 
variable f is replaced by h1, which is in turn replaced by h2,
that counts as two substitutions for f). Only projections or
imitations are counted; eliminating substitutions are not.
See also MAX-SUBSTS-PROJ and MAX-SUBSTS-PROJ-TOTAL.
This applies to higher-order unification (UN88 or UN90) only."))

(defflag max-substs-proj
  (flagtype null-or-integer)
  (default nil)
  (subjects unification transmit)
  (relevant-kids (T '(max-substs-var max-substs-quick max-substs-proj-total)))
  (mhelp "The total number of projection substitutions 
allowed for any given variable. See also MAX-SUBSTS-VAR
and MAX-SUBSTS-PROJ-TOTAL.
This applies to higher-order unification (UN88 or UN90) only."))

(defflag max-substs-proj-total
  (flagtype null-or-integer)
  (default nil)
  (subjects unification transmit)
  (relevant-kids (T '(max-substs-var max-substs-proj max-substs-quick)))
  (mhelp "The total number of projection substitutions 
allowed for any given dpairset. See also MAX-SUBSTS-VAR
and MAX-SUBSTS-PROJ.
This applies to higher-order unification (UN88 or UN90) only."))

(defflag max-substs-quick
  (flagtype null-or-integer)
  (default nil)
  (subjects unification important ms98-1 transmit)
  (relevant-kids (T '(max-substs-var max-substs-proj max-substs-proj-total)))
  (mhelp "When NIL, quick unification is governed by the MIN-QUICK-DEPTH
flag, and only minimal amounts of MAX-SUBSTS checking are done during 
quick unification.
When MIN-SUBSTS-QUICK is a positive integer, quick unification 
(i.e. partial unification of a possible connection) is considered as a 
special case of normal unification, with MAX-SUBSTS-VAR temporarily 
equal to the value of MAX-SUBSTS-QUICK.
When MIN-SUBSTS-QUICK is 0, quick unification goes down as far as it can 
until it is forced to either branch or violate MAX-SUBSTS-VAR. (This is 
almost equivalent to MAX-SUBSTS-QUICK NIL and MIN-QUICK-DEPTH 1.) 

Note: non-NIL values of MAX-SUBSTS-QUICK only take effect if MAX-SUBSTS-VAR
is also non-NIL. In this case, other flags will also be affected, as follows:
APPLY-MATCH will be ignored (the matching routine that is used will be a 
variant of APPLY-MATCH-ALL-FRDPAIRS)
COUNTSUBS-FIRST and STOP-AT-TSN will be T.
SUBSUMPTION-CHECK, UNIF-COUNTER and UNIF-TRIGGER will be NIL.
UNI-SEARCH-HEURISTIC will be BREADTH-FIRST.
MIN-QUICK-DEPTH and MAX-UTREE-DEPTH will be ignored."))

(defmode msv-off
  (flag-settings 
   (max-substs-var nil)
   (max-substs-proj-total nil)
   (max-substs-proj nil)
   (max-substs-quick nil)
   (apply-match 'apply-match-all-frdpairs))
  (mhelp "Turn off all of the MAX-SUBSTS-* routines."))

(defmode msv-on
  (flag-settings 
   (max-substs-var 5)
   (max-substs-proj-total nil)
   (max-substs-proj nil)
   (max-substs-quick 5)
   (apply-match 'apply-match-all-frdpairs)
   (max-utree-depth nil)
   (max-search-depth nil)
   (min-quick-depth nil))
  (mhelp "Turn on the MAX-SUBSTS-* routines and increase
the unification depths to infinity."))

(context unification)

(defreview unif-depths
  (mhelp "Turn off all the MAX-SUBSTS checking in unification,
and use only the flags MAX-SEARCH-DEPTH, MAX-UTREE-DEPTH
and MIN-QUICK-DEPTH."))

(defun unif-depths ()
  (msgf "Setting flags:" t)
  (set-flag 'max-substs-proj nil) (msg "MAX-SUBSTS-PROJ: NIL" t)
  (set-flag 'max-substs-proj-total nil) (msg "MAX-SUBSTS-PROJ-TOTAL: NIL" t)
  (set-flag 'max-substs-quick nil) (msg "MAX-SUBSTS-QUICK: NIL" t)
  (set-flag 'max-substs-var nil) (msg "MAX-SUBSTS-VAR: NIL" t)
  (set-flag 'apply-match 'apply-match-all-frdpairs)
  (msgf t "Now set the following flags to non-NIL values:" t)
  (update-flag 'max-search-depth)
  (update-flag 'max-utree-depth)
  (update-flag 'min-quick-depth))

(defreview unif-nodepths
  (mhelp "Turn off all the depth checking in unification,
and set the MAX-SUBSTS-VAR and MAX-SUBSTS-QUICK flags."))

(defun unif-nodepths ()
  (msgf "Setting flags:" t)
  (set-flag 'max-search-depth nil) (msg "MAX-SEARCH-DEPTH: NIL" t)
  (set-flag 'max-utree-depth nil) (msg "MAX-UTREE-DEPTH: NIL" t)
  (set-flag 'min-quick-depth nil) (msg "MIN-QUICK-DEPTH: NIL" t)
  (set-flag 'max-substs-proj nil) (set-flag 'max-substs-proj-total nil)
  (set-flag 'apply-match 'apply-match-all-frdpairs)
  (msgf t "Now set the following flags to non-NIL values:" t)
  (update-flag 'max-substs-var)
  (update-flag 'max-substs-quick))

(defun simpl-subst-check (substs)
  (if (or max-substs-proj max-substs-proj-total)
      (multiple-value-bind (var proj ptotal) 
			   (check-simpl-count substs t)
			   (if (and max-substs-var (> var max-substs-var))
			       t 
			     (if (and max-substs-proj (> proj max-substs-proj))
				 t
			       (if (and max-substs-proj-total (> ptotal max-substs-proj-total))
				   t
				 nil))))
    (multiple-value-bind (var proj ptotal) 
			 (check-simpl-count substs nil)
			 (declare (ignore proj ptotal))
			   (if (and max-substs-var (> var max-substs-var))
			       t nil))))
      
(defun check-simpl-count (substs &optional (pcount t))
  (let ((topvars '((0 . 0)))
	(hvars nil)
	(topvars-proj '((0 . 0)))
	(proj-total 0)
	(dummy nil))
    (dolist (sub (reverse substs))
	    (if (subst-p (cdr sub))
		(progn
		  (setq dummy (cdr (assoc (car sub) hvars)))
		  (if dummy
		      (progn 
			(incf (cdr (assoc dummy topvars)))
			(when (and pcount (integerp (subst-type (cdr sub))))
			      (incf (cdr (assoc dummy topvars-proj)))
			      (incf proj-total))
			(setq hvars (append (mapcar #'(lambda (x) (cons x dummy))
						    (mapcar #'(lambda (x) (or (cdr (assoc x (subst-new-h-vars (cdr sub))))
									      x)) (subst-h-vars (cdr sub)))) 
					    hvars)))
		    (progn
		      (setq topvars (cons (cons (car sub) 1) topvars))
		      (when pcount
			    (if (integerp (subst-type (cdr sub))) 
				(progn (incf proj-total) (setq topvars-proj (cons (cons (car sub) 1) topvars-proj)))
			      (setq topvars-proj (cons (cons (car sub) 0) topvars-proj))))
		      (setq hvars (append (mapcar #'(lambda (x) (cons x (car sub)))
						  (mapcar #'(lambda (x) (or (cdr (assoc x (subst-new-h-vars (cdr sub))))
									    x)) (subst-h-vars (cdr sub))))
					  hvars)))))))
    (values (reduce #'max (mapcar #'cdr topvars)) (reduce #'max (mapcar #'cdr topvars-proj)) proj-total)))
      
(defun simpl (dpairs subst-stack free-vars progress-count)
  (declare (special rigid-path-ck))
  (do ((dpairs (cdr dpairs) (cdr dpairs))
       (pair (car dpairs) (car dpairs))
       (new-dpairs nil)
       (elements-to-check nil))
      ((null pair)
       (let (fr-flag)
	 (multiple-value-setq (fr-flag dpairs)
	     (ck-new-dpairs elements-to-check free-vars
			    (if new-dpairs t nil)))
	 (setq dpairs (nconc new-dpairs dpairs))
	 (if (not (and fr-flag dpairs))
	     (if (and (not skolem-default) (ms88-cyclic-selection-terms subst-stack))
		 (values 'fail subst-stack)
	       (values 'success subst-stack dpairs free-vars))
	     (multiple-value-bind (dpairs stack)
		 (if rigid-path-ck
		     (simpl-modification dpairs free-vars subst-stack)
		     dpairs)
	       (if stack
		   (if (eq stack 'fail) (values 'fail subst-stack)
		     (simpl dpairs stack
			    (remove (caar stack) free-vars :test #'eq)
			    progress-count))
		   (values 'more subst-stack dpairs free-vars
			   progress-count))))))
    (let ((first (hnf (car pair) subst-stack))
	  (2nd (hnf (cdr pair) subst-stack)))
      (when (eq (uni-term-pos first) (uni-term-pos 2nd))
	(setf (uni-term-pos first) t)
	(setf (uni-term-pos 2nd) t))
      (when (not (uni-term-pos 2nd))
	(psetq first 2nd 2nd first))
      (if (uni-term-pos first)
	  (if (rigid-p first free-vars)
	      (if (rigid-p 2nd free-vars)
		  (multiple-value-bind
		      (fail-flag new-dpairs)
		      (step1-simpl
			(uni-term-head first) (uni-term-args first)
			(uni-term-binder first) (uni-term-bdvars first)
			(uni-term-head 2nd) (uni-term-args 2nd)
			(uni-term-binder 2nd) (uni-term-bdvars 2nd))
		    (if fail-flag (return-from simpl
				    (values 'fail subst-stack))
			(progn (setq dpairs (nconc new-dpairs dpairs))
			       (incf progress-count))))
		  (push (cons 2nd first) new-dpairs))
	      (push (cons first 2nd) elements-to-check))
	  (if (rigid-p 2nd free-vars)
	      (if (rigid-p first free-vars)
		  (return-from simpl (values 'fail subst-stack))
		  (progn
		    (setf (uni-term-pos first) t)
		    (setf (uni-term-pos 2nd) nil)
		    (push (cons first 2nd) new-dpairs)))
	      (push (cons 2nd first) new-dpairs))))))

(defun step1-simpl (head1 args1 binder1 bdvars1 head2 args2 binder2 bdvars2)
  (unless (= (length binder1) (length binder2))
    (if eta-rule
	(let ((num (- (length binder1) (length binder2))))
	  (if (plusp num)
	      (do ((binder1 binder1 (cdr binder1))
		   (new-vars nil)
		   (num num (1- num)))
		  ((zerop num)
		   (setq binder2 (nreconc (mapcar #'car new-vars) binder2))
		   (setq args2 (append args2 (mapcar #'(lambda (elt)
							 (list (car elt)))
						     new-vars)))
		   (setq bdvars2 (nreconc new-vars bdvars2)))
		(push (cons (gensym)
			    (funcall ren-var-fn
				     (cdr (assoc (car binder1) bdvars1))))
		      new-vars))
	      (do ((binder2 binder2 (cdr binder2))
		   (new-vars nil)
		   (num num (1+ num)))
		  ((zerop num)
		   (setq binder1 (nreconc (mapcar #'car new-vars) binder1))
		   (setq args1 (append args1 (mapcar #'(lambda (elt)
							 (list (car elt)))
						     new-vars)))
		   (setq bdvars1 (nreconc new-vars bdvars1)))
		(push (cons (gensym)
			    (funcall ren-var-fn
				     (cdr (assoc (car binder2) bdvars2))))
		      new-vars))))
	(return-from step1-simpl (values T nil))))
  (let ((bd1 (memq head1 binder1))
	(bd2 (memq head2 binder2)))
    (if (or bd1 bd2)
	(if (not (= (length bd1)(length bd2)))
	    (return-from step1-simpl (values T nil)))
	(if (not (wffeq head1 head2)) (return-from step1-simpl (values T nil)))))
  (values nil
	  (mapcar #'(lambda (elt1 elt2)
		      (step1-simpl-dpair
			(car elt1) binder1 (cdr elt1) bdvars1
			(car elt2) binder2 (cdr elt2) bdvars2))
		  args1 args2)))

(defun simpl-modification (dpairs free-vars subst-stack)
  (dolist (pair dpairs (values dpairs nil))
    (let ((first (car pair))
	  (second (cdr pair)))
      (let ((var (if (or (uni-term-args first) (uni-term-binder first))
		     nil (uni-term-head first))))
	(unless var
	  (setq var (if (or (uni-term-args second) (uni-term-binder second))
			nil (if (or (not (uni-term-pos second))
				    (not (memq (uni-term-head second)
					       free-vars)))
				nil (uni-term-head second)))
		second first))
	(when var
	  (multiple-value-bind (free rigid-p)
	      (free-in-var-uniterm var second subst-stack free-vars)
	    (if free
	      (when (and rigid-p (or (first-order-var var)
				     (not (uni-term-binder second))))
                (when (neq unify-verbose 'silent) 
		      (if (eq unify-verbose 'max)
			  (msg T "Rigid-path-check for" T var T second T "succeeded in ms88." T)
			(msg "R")))
		(return (values nil 'fail)))
	      (return (values (delete pair dpairs)
			      (acons var (rename-all-bd-variables (lambda-reduce second nil))
				     subst-stack))))))))))


(defun bdvar-free-in-term (binder-elt term stack)
  (cond ((label-q term)
	 (apply-label term (bdvar-free-in-term binder-elt term stack)))
	((lsymbol-q term)
	 (if (eq binder-elt term) t
	     (let ((binding (cdr (assoc term stack))))
	       (when binding
		 (if (symbolp binding) (eq binding binder-elt)
		     (bdvar-free-in-term binder-elt (car binding)
					 (cdr binding)))))))
	((boundwff-q term) (bdvar-free-in-term binder-elt (cdr term) stack))
	(t (or (bdvar-free-in-term binder-elt (car term) stack)
	       (bdvar-free-in-term binder-elt (cdr term) stack)))))

(defun step1-simpl-dpair (term1 binder1 stack1 bdvars1 term2 binder2 stack2
				bdvars2)
  "Returns a DPAIR after deleting vacuous quantifiers. Assumes that binder1
       and binder2 are of equal length."
  (do ((binder1 binder1 (cdr binder1))
       (binder2 binder2 (cdr binder2))
       (new-binder1 nil)
       (new-binder2 nil))
      ((null binder1)
       (if eta-rule
	   (cons (eta-reduce-uterm term1 (nreverse new-binder1) stack1 bdvars1)
		 (eta-reduce-uterm term2 (nreverse new-binder2) stack2
				   bdvars2))
	   (cons
	     (make-uni-term :head term1 :binder (nreverse new-binder1)
			    :stack stack1 :bdvars bdvars1)
	     (make-uni-term :head term2 :binder (nreverse new-binder2)
			    :stack stack2 :bdvars bdvars2))))
    (when (or (bdvar-free-in-term (car binder1) term1 stack1)
	      (bdvar-free-in-term (car binder2) term2 stack2))
      (push (car binder1) new-binder1)
      (push (car binder2) new-binder2))))

(defun eta-reduce-uterm (term binder stack bdvars)
  (if (and binder (consp term))
      (let ((bdvar (cdr (assoc (car binder) bdvars))))
	(if (and (eq bdvar (cdr term))
		 (bdvar-free-in-term (car binder) bdvar stack)
		 (not (free-in bdvar (car term))))
	    (eta-reduce-uterm (car term) (cdr binder) stack bdvars)
	    (make-uni-term :head term :binder binder :stack stack
			   :bdvars bdvars)))
      (make-uni-term :head term :binder binder :stack stack
		     :bdvars bdvars)))

