;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of ms90-3)

(deffile ms90-3-unif-simpl
  (part-of ms90-3)
  (mhelp "Implementation of Huet's Simpl routine for Path-focused
duplication."))

(defun simpl-head (term)
  (if (or (symbolp term) (numberp (cdr term))) term
      (if (lambda-bd-p term)
	  (simpl-head (cdr term))
	  (simpl-head (car term)))))

(defun ho-free-in (var term stack)
  (if (symbolp term) nil
      (if (numberp (cdr term))
          (let ((subst (cdr (assoc term stack))))
            (if subst (ho-free-in var subst stack)
                (eq var term)))
          (or (ho-free-in var (car term) stack)
              (ho-free-in var (cdr term) stack)))))

;;;returns nil for fail, 0 for success and 1 if there are frpairs.
;;;This is Sunil's implementation of the rigid-path check for 
;;;un90, the unification algorithm used by path-focused duplication
;;;See notes in /afs/cs/user/andrews/rigid-path.doc for relevant
;;;comments

(context unification)

(defflag rigid-path-ck
  (flagtype boolean)
  (default T)
  (subjects unification ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "If T, apply rigid-path checking when doing unification. If NIL, 
switch to original unification. Both UN90 and UN88 unification 
procedures are affected by the flag."))

(defmacro ho-simpl (node)
 `(if rigid-path-ck 
      (ho-simpl-rigid ,node)
      (ho-simpl-no-rigid ,node)))

(defun simpl-term-binder (term binder)
  (if (or (symbolp term) (numberp (cdr term))) (values term binder)
      (if (lambda-bd-p term)
          (simpl-term-binder (cdr term) (cons (caar term) binder))
          (values term binder))))

;;check that the initial binder is empty and var is of base type
;;before calling this function.

(defun rigid-path-p (var term)
  (let ((term (simpl-term-binder term nil)))
    (if (symbolp term) nil
      (if (numberp (cdr term))
	  (and (eq (car term) (car var))
	       (= (cdr term) (cdr var)))
	(let ((head (simpl-head term)))
	  (if (consp head) nil
	    (or (rigid-path-p var (car term))
		(rigid-path-p var (cdr term)))))))))

(defun ho-simpl-rigid (node)
  (do ((dpairs (copy-list (unode-dpairs node)) (cdr dpairs))
       (subst-stack (unode-substs node))
       (frpairs nil)
       (ffpairs nil)
       first second)
      ((null dpairs)
       (progn 
       (unless skolem-default
	       (when (cyclic-selection-terms (unode-substs node)) (return-from ho-simpl-rigid nil)))
       (prog1 (if frpairs 1 0)
	 (setf (unode-dpairs node) (nconc frpairs ffpairs)))))
      (setq first (ho-unif-lnorm nil (caar dpairs) nil subst-stack nil))
      (setq second (ho-unif-lnorm nil (cdar dpairs) nil subst-stack nil))
      (let ((rh1 (symbolp (simpl-head first)))
	    (rh2 (symbolp (simpl-head second))))
	(if (eq first second) nil
          (progn
            (when (or (and (consp first)
                           (numberp (cdr first))
                           (not (ho-free-in first second subst-stack)))
                      (when (and (consp second)
                                 (numberp (cdr second))
                                 (not (ho-free-in second first subst-stack)))
			    (psetq first second second first)
			    t))
		  (push (cons first second) (unode-substs node))
		  (setf (unode-dpairs node) (nconc (cdr dpairs) frpairs ffpairs))
		  (return-from ho-simpl-rigid (ho-simpl-rigid node)))
            (when (or (and (consp first) (numberp (cdr first)))
                      (when (and (consp second) (numberp (cdr second)))
			    (psetq first second second first)
			    (psetq rh1 rh2 rh2 rh1)
			    t))
		  (when (and (symbolp (type (car first)))
			     rh2)
			(multiple-value-bind (second binder)
					     (simpl-term-binder second nil)
					     (when (and (not binder)
							(rigid-path-p first second))
						   (case unify-verbose 
							 (max (msg T "Rigid path check succeeded" first second))
							 ((min med) (msg "R")))
						   (return-from ho-simpl-rigid nil)))))
            (if rh1
                (if rh2
                    (multiple-value-bind (fail new-dpairs)
					 (stepa-simpl first second )
					 (if fail (return-from ho-simpl-rigid nil)
					   (setq dpairs (nconc dpairs new-dpairs))))
		  (push (cons first second) frpairs))
	      (if rh2
		  (push (cons second first) frpairs)
		(push (cons first second) ffpairs))))))))



(defun ho-simpl-no-rigid (node)
  (do ((dpairs (copy-list (unode-dpairs node)) (cdr dpairs))
       (subst-stack (unode-substs node))
       (frpairs nil)
       (ffpairs nil)
       first second)
      ((null dpairs)
       (progn 
       (unless skolem-default
	       (when (cyclic-selection-terms (unode-substs node)) (return-from ho-simpl-no-rigid nil)))
       (prog1 (if frpairs 1 0)
         (setf (unode-dpairs node) (nconc frpairs ffpairs)))))
      (setq first (ho-unif-lnorm nil (caar dpairs) nil subst-stack nil))
      (setq second (ho-unif-lnorm nil (cdar dpairs) nil subst-stack nil))
      (let ((rh1 (symbolp (simpl-head first)))
	    (rh2 (symbolp (simpl-head second))))
	(if (eq first second) nil
          (progn
            (when (or (and (consp first)
                           (numberp (cdr first))
                           (not (ho-free-in first second subst-stack)))
                      (when (and (consp second)
                                 (numberp (cdr second))
                                 (not (ho-free-in second first subst-stack)))
			    (psetq first second second first)
			    t))
		  (push (cons first second) (unode-substs node))
		  (setf (unode-dpairs node) (nconc (cdr dpairs) frpairs ffpairs))
		  (return-from ho-simpl-no-rigid (ho-simpl-no-rigid node)))
            (if rh1
                (if rh2
                    (multiple-value-bind (fail new-dpairs)
					 (stepa-simpl first second )
					 (if fail (return-from ho-simpl-no-rigid nil)
					   (setq dpairs (nconc dpairs new-dpairs))))
		  (push (cons first second) frpairs))
	      (if rh2
		  (push (cons second first) frpairs)
		(push (cons first second) ffpairs))))))))

(defun fo-simpl (node)
  (let ((dpairs (unode-dpairs node))
	(subst-stack (unode-substs node)))
    (multiple-value-bind (subst-stack unifiable-p)
	(pfd-fo-unify nil dpairs subst-stack)
      (if unifiable-p
	  (progn (setf (unode-substs node) subst-stack)
		 0)
	  nil))))

(defun ck-binder (term1 term2 binder args1 args2)
  (if (lambda-bd-p term1)
      (if (lambda-bd-p term2)
          (ck-binder (cdr term1) (cdr term2)
                     (acons (caar term1) (caar term2) binder)
                     args1 args2)
          (let ((new-var (ren-var-uni-ho w-var-prefix (type (caar term1)))))
            (ck-binder (cdr term1) term2
                       (acons (caar term1) new-var binder)
                       nil (append args2 (list new-var)))))  ;NOT (cons new-var args2) !! MB Fri Sep  6 17:57:15 1996
      (if (lambda-bd-p term2)
          (let ((new-var (ren-var-uni-ho w-var-prefix (type (caar term2)))))
            (ck-binder term1 (cdr term2)
                       (acons new-var (caar term2) binder)
                       (append args1 (list new-var)) nil)) ;NOT (cons new-var args1) !! MB Fri Sep  6 17:57:15 1996
          (values term1 term2 binder args1 args2))))

(defun find-rhead (term args)
  (if (symbolp term) (values term args)
      (find-rhead (car term) (cons (cdr term) args))))

(defun stepa-simpl (term1 term2)
  (multiple-value-bind (term1 term2 binder args1 args2)
      (ck-binder term1 term2 nil nil nil)
    (multiple-value-bind (head1 args1)
        (find-rhead term1 args1)
      (multiple-value-bind (head2 args2)
          (find-rhead term2 args2)
        (when (dolist (bdvars binder t)
                (when (or (eq head1 (car bdvars)) (eq head2 (cdr bdvars)))
                  (if (and (eq head1 (car bdvars)) (eq head2 (cdr bdvars)))
                      (return nil)
                      (return-from stepa-simpl t))))
          (if (eq head1 head2) nil
              (if (or (eq head1 'not) (eq head2 'not))
                  (return-from stepa-simpl
                    (flip-negs head1 args1 head2 args2 binder))
                  (return-from stepa-simpl t))))
        (values nil (mapcar #'(lambda (elt1 elt2)
                                (prefix-l-both elt1 elt2 binder))
                            args1 args2))))))

(defun prefix-l-both (term1 term2 binder)
  (if binder (prefix-l-both (acons (caar binder) 'lambda term1)
                            (acons (cdar binder) 'lambda term2)
                            (cdr binder))
      (cons term1 term2)))

(defun ho-construct-term-1 (head args)
  (if args (ho-construct-term-1 (cons head (car args)) (cdr args))
      head))

(defun flip-negs (head1 args1 head2 args2 binder)
  (if (eq head1 'not)
      (if (rigidp (car args1) nil) t
          (values nil
                  (list (prefix-l-both
                         args1 (cons 'not (ho-construct-term-1 head2 args2))
                         binder))))
      (if (rigidp (car args2) nil) t
          (values nil
                  (list (prefix-l-both
                         (cons 'not (ho-construct-term-1 head1 args1)) args2
                         binder))))))


(defun ho-simpl-subst-check (substs)
  (if (or max-substs-proj max-substs-proj-total)
      (let ((count (ho-check-simpl-count substs t)))
	(if (and max-substs-var (> (car count) max-substs-var))
	    (progn (when (memq unify-verbose '(med max)) (msg "S")) t)
	  (if (and max-substs-proj (> (cadr count) max-substs-proj))
	      (progn (when (memq unify-verbose '(med max)) (msg "S")) t)
	    (if (and max-substs-proj-total (> (caddr count) max-substs-proj-total))
		(progn (when (memq unify-verbose '(med max)) (msg "S")) t)
	      nil))))
      (let ((count (ho-check-simpl-count substs nil)))
	(if (and max-substs-var (> (car count) max-substs-var))
	    (progn (when (memq unify-verbose '(med max)) (msg "S")) t) nil))))

(defun ho-check-simpl-count (substs &optional (pcount t))
  (let ((topvars '((0 . 0)))
	(hvars nil)
	(topvars-proj '((0 . 0)))
	(proj-total 0)
	(dummy nil))
    (dolist (sub (reverse substs))
	    (when (and (listp (cdr sub)) (listp (cadr sub)) (eq (cdadr sub) 'LAMBDA))
		  (setq dummy (cdr (assoc (car sub) hvars)))
		  (if dummy
		      (progn 
			(incf (cdr (assoc dummy topvars)))
			(when pcount
			      (unless (memq (ms90-3-head (cdr sub)) (ho-free-vars-of (cdr sub)))
				      (incf (cdr (assoc dummy topvars-proj)))
				      (incf proj-total)))
			(setq hvars (append (mapcar #'(lambda (x) (cons x dummy))
						    (ho-free-vars-of (cdr sub)))
					    hvars)))
		    (progn
		      (setq topvars (cons (cons (car sub) 1) topvars))
		      (when pcount
			    (if (memq (ms90-3-head (cdr sub)) (ho-free-vars-of (cdr sub)))
				(setq topvars-proj (cons (cons (car sub) 0) topvars-proj))
			      (progn (incf proj-total) (setq topvars-proj (cons (cons (car sub) 1) topvars-proj)))))
		      (setq hvars (append (mapcar #'(lambda (x) (cons x (car sub)))
						  (ho-free-vars-of (cdr sub)))
					  hvars))))))
    (list (reduce #'max (mapcar #'cdr topvars)) (reduce #'max (mapcar #'cdr topvars-proj)) proj-total)))

(defun ho-free-vars-of (gwff &optional (bind-list nil))
  (cond ((label-q gwff) (apply-label gwff (ho-free-vars-of gwff bind-list)))
	((lsymbol-q gwff)
	 (cond ((get gwff 'abbrev) (ho-free-vars-of (get gwff 'defn) bind-list))
               (t (if (and (not (logconst-q gwff)) (propsym-q gwff)
		           (not (member gwff bind-list)))
	              (list gwff)))))
	((and (consp gwff) (numberp (cdr gwff)))
	 (if (not (member gwff bind-list)) (list gwff) nil))
	((boundwff-q gwff) (ho-free-vars-of (cdr gwff) (cons (caar gwff) bind-list)))
	(t (nconc (ho-free-vars-of (car gwff) bind-list) 
		  (ho-free-vars-of (cdr gwff) bind-list)))))

(defun make-banned (evars)
  (let (retlist)
    (dolist (e evars retlist)
      (dolist (q (universal-qvars e))
	(setq retlist (all-banned-acons q (universal-selected e) retlist)))))) ; cebrown - 10/16/00 - fixed this to handle univ vars that occur more than once in the jform (due to occuring in multiple exp terms)
;	(push (cons q (universal-selected e)) retlist))) ; old way

(defun cyclic-selection-terms (substs)
  (let* ((evars (get-universals ms90-3-jform))
	 (gwff nil)
	 (all-banned (make-banned (mapcar 'cdr evars)))
	 (allb2 (reduce 'append (mapcar #'cdr all-banned)))
	 (all-subs nil)
	 (banned nil))
    (dolist (sub substs (get-real-cycles all-banned all-subs))
      (when (consp (car sub))
					; I commented out the following and added the (setq banned ...) line, because this appears to be recomputing what make-banned
					; already computed above, and it doesn't work when an evar has more than one universal node - cebrown 10/17/00
;		  (setq banned (cdar (remove-if-not #'(lambda (x) 
;							(and (eq (car x) (cdar sub))
;							     (memq (caar sub) (universal-qvars (cdr x)))))
;						    evars)))
;		  (unless banned 
;			  (setq banned (cdar (remove-if-not #'(lambda (x) 
;								(memq (caar sub) (universal-qvars (cdr x))))
;							    evars))))
;		  (when banned (setq banned (universal-selected banned)))
	(setq banned (cdr (assoc (caar sub) all-banned))) ; I added this to replace the code above - cebrown 10/17/00
		  (when banned
			(if first-order-mode-ms
			    (let ((newsubs (full-norm-fo substs)))
;			      (dolist (n newsubs) (msgf n))
			      (setq gwff (assoc (car sub) newsubs :test 'equal)))
			  (setq gwff (ho-unif-lnorm-full nil (car sub) nil substs nil)))
;			(msgf (car sub) "  " gwff)
			(when (occurs-in gwff banned) (return t)))
		  (push (cons (car sub) (all-occurs-in gwff allb2)) all-subs)))))

(defun full-norm-fo (substs)
  (setq substs (reverse substs))
  (let ((stack nil))
    (dolist (subst substs)
	    (push (cons (car subst)
			(expand-subst-fo (cadr subst) (cddr subst) substs))
		  stack))
    stack))

(defun expand-subst-fo (term env substs)
  (if (lsymbol-q term)
      (let ((var (car (assoc term env :test 'equal))))
	(if var 
	    (let ((sub  (cdr (assoc var substs :test 'equal))))
	      (if sub 
		  (expand-subst-fo (car sub) (cdr sub) substs)
		var))
            term))
      (cons (expand-subst-fo (car term) env substs)
            (expand-subst-fo (cdr term) env substs))))

(defun get-universals (jform &optional (count 1))
  (unless (or (null jform) (eq jform t))
  (case (jform-type jform)
	(universal (cons (cons count jform) (append (get-universals (universal-scope jform) count)
						    (get-universals (universal-dup jform) (1+ count)))))
	(disjunction (reduce 'append (mapcar #'(lambda (x) (get-universals x count))
					     (disjunction-components jform))))
	(conjunction (reduce 'append (mapcar #'(lambda (x) (get-universals x count))
					     (conjunction-components jform))))
	(literal nil))))

(defun ms88-cyclic-selection-terms (substs)
  (let* ((evars (remove-if-not #'exp-var-p (mapcar 'car (eproof-free-vars-in-etree current-eproof))))
	 (gwff nil)
;	 (all-banned (mapcar #'(lambda (x) (cons (exp-var-var x) (exp-var-selected x))) evars)) ; precomputing all-banned now, see eproof-all-banned slot - cebrown 8/1/00
	 (allb2 (reduce 'append (mapcar #'cdr (eproof-all-banned current-eproof))))
	 (all-subs nil)
	 (banned nil))
    (dolist (sub substs (get-real-cycles (eproof-all-banned current-eproof) all-subs))
	    (when (member (car sub) (mapcar #'exp-var-var evars))
		  (setq banned (exp-var-selected (car (remove-if-not #'(lambda (x) (eq (exp-var-var x) (car sub)))
								      evars))))
		  (setq gwff (lambda-reduce-subst (cdr sub) substs))
		  (when (occurs-in gwff banned) (return t))
		  (push (cons (car sub) (all-occurs-in gwff allb2)) all-subs)))))

(defvar *vars-checked*)

(defun get-real-cycles (all-banned all-subs)
  ;;all-banned is a list of elts (var const1 const2...) where each const is banned in subs for var
  ;;all-subs is a list of (var const1 const2) where each const occurs in the sub for var.
  (setq *vars-checked* nil)
  (let ((all-subs (append (eproof-inst-exp-vars-params current-eproof) all-subs))) ; cebrown - 7/25/00 - added eproof-inst-exp-vars-params, see also "fill-selected" in etrees-renumber.lisp
    (dolist (sub all-subs nil)
      (when (and (not (memq (car sub) *vars-checked*))
		 (find-cycle-beginning-at (list (car sub)) all-banned all-subs))
	(return t)))))

; added some comments - cebrown 7/25/00
(defun find-cycle-beginning-at (varlist all-banned all-subs)
  (let ((consts (cdr (assoc (car varlist) all-subs)))) ; all the sel vars (car varlist) does depend on
    (dolist (c consts nil)		; for each such sel var c
      (let ((newvars (find-cycle-fn c all-banned))) ; find evars that can't depend on c
	(when (intersection newvars varlist) (return-from find-cycle-beginning-at t)) ;found a cycle
	(dolist (v newvars)
	  (push v *vars-checked*)
	  (when (find-cycle-beginning-at (cons v varlist) all-banned all-subs)
	    (return-from find-cycle-beginning-at t)))))))

; returns list of all evars which cannot depend on sel var c
(defun find-cycle-fn (c all-banned)
  (if (null all-banned) nil
    (if (memq c (cdar all-banned)) (cons (caar all-banned) (find-cycle-fn c (cdr all-banned)))
      (find-cycle-fn c (cdr all-banned)))))

(defun occurs-in (gwff banned)
  (if (null gwff) nil
    (if (symbolp gwff) (member gwff banned)
      (if (consp gwff)
	  (or (occurs-in (car gwff) banned)
	      (occurs-in (cdr gwff) banned))
	nil))))

(defun all-occurs-in (gwff allbanned)
  (if (null gwff) nil
    (if (symbolp gwff) (if (member gwff allbanned) (list gwff) nil)
      (if (consp gwff)
	  (append (all-occurs-in (car gwff) allbanned)
		  (all-occurs-in (cdr gwff) allbanned))
	nil))))

(defun ho-unif-lnorm-full (neg gwff bdvars sstack bstack)
  (multiple-value-bind (gwff neg)
      (ho-unif-lnorm-full-rec neg gwff bdvars sstack bstack)
    (if neg (cons 'not gwff) gwff)))

(defun ho-unif-lnorm-full-rec (neg gwff bdvars sstack bstack)
  (cond ((symbolp gwff)
         (if (member gwff bdvars) (values gwff neg)
             (let ((term (assoc gwff bstack :test 'equal)))
               (if term 
		   (ho-unif-lnorm-full-rec neg (cdr term) nil sstack bstack)
		 (values gwff neg)))))
        ((numberp (cdr gwff))
         (let ((term (or (assoc gwff sstack :test 'equal) (assoc (car gwff) sstack :test 'equal))))
           (if term 
	       (ho-unif-lnorm-full-rec neg (cdr term) bdvars sstack bstack)
	     (values gwff neg))))
        ((not-p gwff)
         (ho-unif-lnorm-full-rec (not neg) (cdr gwff) bdvars sstack bstack))
        ((boundwff-q gwff)
         (cond ((if neg (e-bd-wff-p gwff) (a-bd-wff-p gwff))
                (values (cons (create-propsym
                               binder-pi
                               (cons 'O (cons 'O (type (bdvar gwff)))))
                              (acons (bdvar gwff) 'lambda
                                     (if neg (cons 'not (cdr gwff))
                                         (cdr gwff))))
                        nil))
               ((if neg (a-bd-wff-p gwff) (e-bd-wff-p gwff))
                (values (cons (create-propsym
                               binder-sigma
                               (cons 'O (cons 'O (type (bdvar gwff)))))
                              (acons (bdvar gwff) 'lambda
                                     (if neg (cons 'not (cdr gwff))
                                         (cdr gwff))))
                        nil))
               ((lambda-bd-p gwff)
                (cons (car gwff) (ho-unif-lnorm-full
                                  nil (cdr gwff) (cons (caar gwff) bdvars)
                                  sstack bstack)))
               (t (throwfail "HO-UNIF-LNORM-full-REC: Unknown bdwfftype in: "
                             (gwff . gwff)))))
        (t (if (lambda-bd-p (car gwff))
               (ho-unif-lnorm-full
                neg (cdar gwff) bdvars sstack
                (acons (caaar gwff)
                       (if bstack
                           (ho-unif-lnorm-full-rec nil (cdr gwff) bdvars sstack bstack)
                           (cdr gwff))
                       bstack))
               (let ((gwff (cons (ho-unif-lnorm-full-rec
                                  nil (car gwff) bdvars sstack bstack)
				 (ho-unif-lnorm-full-rec nil (cdr gwff) bdvars sstack
						 bstack))))
                 (if (lambda-bd-p (car gwff))
                     (ho-unif-lnorm-full neg (cdar gwff) bdvars sstack
                                        (acons (caaar gwff) (cdr gwff) bstack))
                     (values gwff neg)))))))

