;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of S-EQN)

(deffile s-eqn-rew
    (part-of S-EQN)
  (extension clisp)
  (mhelp "Additional rewriting facilities used by S-EQN."))

;(defun replace-rrule-uqvars- (inwff outwff before after types)
;  (declare (ignore inwff before))
;  (replace-rrule-uqvars-real outwff after types nil)
;  (substitute-types *type-subs*
;		    (foldl #'(lambda (sub wff)
;			       (substitute-lt-term-var (cdr sub) (car sub)
;						       wff))
;			   after *binding-subs*)))

;(defun replace-rrule-uqvars-real- (inwff before types bdsubs)
;  ;(format t "~&rrur: ~A, ~A, ~A, ~A" inwff before types bdsubs)
;  ;(format t "~&  type-subs: ~A" *type-subs*)
;  (cond ((lsymbol-q before)
;	 (let* ((newtypelist (suggest-types inwff before types))
;		(before-type-inst (substitute-types newtypelist before)))
;	   (delete-if #'(lambda (tau) (assoc (car tau) *type-subs*))
;		      newtypelist)
;	   (setq *type-subs* (nconc newtypelist *type-subs*))
;	   (cond ((assoc before bdsubs)
;		  (wffeq-ab1 before inwff bdsubs))
;		 ((assoc before *binding-subs*)
;		  (wffeq-ab (cdr (assoc before *binding-subs*)) inwff))
;		 (t (or ;(wffeq inwff before)
;		        (wffeq inwff before-type-inst)
;			(and (propsym-p before)
;			     (push (cons before inwff) *binding-subs*)))))))
;	((boundwff-q before)
;	 (and (boundwff-q inwff)
;	      (eq (binder inwff) (binder before))
;	      (replace-rrule-uqvars-real (gdr inwff) (gdr before) types
;					 (acons (bdvar before)
;						(bdvar inwff) bdsubs))))
;	((boundwff-q inwff) nil)
;	(t (and (consp inwff) (consp before)
;		(replace-rrule-uqvars-real (car inwff) (car before)
;					   types bdsubs)
;		(replace-rrule-uqvars-real (cdr inwff) (cdr before)
;					   types bdsubs)))))

(defun replace-rrule-uqvars (inwff outwff before after types)
  (declare (ignore inwff before))
  (let ((newwff (replace-rrule-uqvars-real outwff after types nil)))
    ;(format t "~&rruqv: ~A~&-> ~A~&; ~A, ~A" after newwff
	;    *type-subs* *binding-subs*)
    (if newwff (substitute-types *type-subs* newwff)
      (substitute-types *type-subs* after))))

(defun replace-rrule-uqvars-real (inwff before types bdsubs)
  ;(format t "~&rrur: ~A, ~A, ~A, ~A" inwff before types bdsubs)
  ;(format t "~&  type-subs: ~A" *type-subs*)
  (cond ((lsymbol-q before)
	 (let* ((newtypelist (suggest-types inwff before types))
		(before-type-inst (substitute-types newtypelist before)))
	   (delete-if #'(lambda (tau) (assoc (car tau) *type-subs*))
		      newtypelist)
	   (setq *type-subs* (nconc newtypelist *type-subs*))
	   (cond ((assoc before bdsubs)
		  (when (wffeq-ab1 before inwff bdsubs) inwff))
		 ((assoc before *binding-subs*)
		  (when (wffeq-ab (cdr (assoc before *binding-subs*)) inwff)
		    inwff))
		 (t (when (or ;(wffeq inwff before)
			   (wffeq inwff before-type-inst)
			   (and (propsym-p before)
				(not (anyabbrev-q before))
				(push (cons before inwff) *binding-subs*)))
		      inwff)))))
	((boundwff-q before)
	 (let ((body
	     (and (boundwff-q inwff)
		  (eq (binder inwff) (binder before))
		  (replace-rrule-uqvars-real (gdr inwff) (gdr before) types
					     (acons (bdvar before)
						    (bdvar inwff) bdsubs)))))
	   (when body (cons (car inwff) body))))
	((boundwff-q inwff) nil)
	(t (when (and (consp inwff) (consp before))
		(let ((hd (replace-rrule-uqvars-real (car inwff) (car before)
						     types bdsubs))
		      (tl (replace-rrule-uqvars-real (cdr inwff) (cdr before)
						     types bdsubs)))
		  (when (and hd tl) (cons hd tl)))))))

(defun apply-rrule-once-uqvars (inwff outwff before after func appfn types)
  (apply-rrule-once-uqvars1 inwff outwff before after func appfn types nil))

(defun apply-rrule-once-uqvars1 (inwff outwff before after
				 func appfn types bdvars)
  (setq *binding-subs* nil)
  (setq *type-subs* nil)
  (if *hopeless-fail* nil
    (cond
     ((lsymbol-q inwff)
      (if (rrule-instance-poly appfn inwff before after types)
	  (let ((newwff (if func
			    (funcall func
				     (replace-rrule-uqvars inwff outwff before
							   after types)
				     before after)
			  (replace-rrule-uqvars inwff outwff before
						after types))))
	    (if newwff
		(if (wffeq-ab1 newwff outwff bdvars)
		    t
		  (if (wffeq-ab1 inwff outwff bdvars) nil
		    (progn (setq *hopeless-fail* t) nil)));)
	      (if (wffeq-ab1 inwff outwff bdvars) nil
		(progn (setq *hopeless-fail* t) nil))))
	(if (wffeq-ab1 inwff outwff bdvars) nil
	  (progn (setq *hopeless-fail* t) nil))))
     ((boundwff-q inwff)
      (if (rrule-instance-poly appfn inwff before after types)
	  (let ((newwff (if func
			    (funcall func
				     (replace-rrule-uqvars inwff outwff before
							   after types)
				     before after)
			  (replace-rrule-uqvars inwff outwff before
						after types))))
	    ;(format t "~&arof: ~A, ~A -> ~A" inwff outwff newwff)
	    (if newwff
		(cond ((wffeq-ab1 newwff outwff bdvars) t)
		      ((boundwff-q outwff)
		       (apply-rrule-once-uqvars1 (gdr inwff) (gdr outwff)
						 before after func appfn types
						 (acons (bdvar inwff)
							(bdvar outwff)
							bdvars)))
		      (t (setq *hopeless-fail* t) nil));)
	      (if (boundwff-q outwff)
		  (apply-rrule-once-uqvars1 (gdr inwff) (gdr outwff) before
					    after func appfn types
					    (acons (bdvar inwff)
						   (bdvar outwff)
						   bdvars))
		(progn (setq *hopeless-fail* t) nil))))
	(if (boundwff-q outwff)
	    (apply-rrule-once-uqvars1 (gdr inwff) (gdr outwff) before after
				      func appfn types (acons (bdvar inwff)
							      (bdvar outwff)
							      bdvars))
	  (progn (setq *hopeless-fail* t) nil))))
     (t (if (rrule-instance-poly appfn inwff before after types)
	    (let ((newwff (if func
			      (funcall func
				       (replace-rrule-uqvars inwff outwff
							     before after
							     types)
				       before after)
			    (replace-rrule-uqvars inwff outwff before
						  after types))))
	      (if newwff
		  (if (wffeq-ab1 newwff outwff bdvars)
		      t
		    (and (consp outwff)
			 (or (and (apply-rrule-once-uqvars1
				      (car inwff) (car outwff)
				      before after func appfn
				      types bdvars)
				  (wffeq-ab1 (cdr inwff) (cdr outwff) bdvars))
			     (and (apply-rrule-once-uqvars1
				      (cdr inwff) (cdr outwff)
				      before after func appfn
				      types bdvars)
				  (wffeq-ab1 (car inwff) (car outwff)
					     bdvars)))));)
		(and (consp outwff)
		     (or (and (apply-rrule-once-uqvars1
                                  (car inwff) (car outwff)
			          before after func appfn
			          types bdvars)
			      (wffeq-ab1 (cdr inwff) (cdr outwff) bdvars))
			 (and (apply-rrule-once-uqvars1
			          (cdr inwff) (cdr outwff)
				  before after func appfn
				  types bdvars)
			      (wffeq-ab1 (car inwff) (car outwff) bdvars))))))
	  (and (consp outwff)
	       (or (and (apply-rrule-once-uqvars1 (car inwff) (car outwff)
						  before after func appfn
						  types bdvars)
			(wffeq-ab1 (cdr inwff) (cdr outwff) bdvars))
		   (and (apply-rrule-once-uqvars1 (cdr inwff) (cdr outwff)
						  before after func appfn
						  types bdvars)
			(wffeq-ab1 (car inwff) (car outwff) bdvars)))))))))

(defun instance-of-rewriting-uqvars (inwff outwff)
  (instance-of-rewriting-with
   (remove-if-not 'active-p global-rewrite-rule-list)
   inwff outwff))

(defun instance-of-rewriting-with (rules inwff outwff)
  (dolist (rule rules nil)
    (setq rule (instantiate-rrule rule))
    (when (and (rewrule-p (rrule-name rule))
	       (instance-of-ruleapp rule inwff outwff
				    (subtheory-containing (rrule-name rule))))
      (return rule))))

(defun theory-congruent-p (theory)
  (or (not theory) (get theory 'congruent)))

(defun instance-of-ruleapp (rule inwff outwff theory)
  (setq *hopeless-fail* nil)
  ;(format t "~&instance-of-ruleapp called with: ~A, ~A, ~A, ~A" rule inwff
	;  outwff theory)
  (and (not (wffeq-ab inwff outwff))
       (or (and (apply-rrule-once-if inwff outwff (get rule 'before)
				     (get rule 'after) (get rule 'rewfn)
				     (get rule 'appfn)
				     (get rule 'rtypelist)
				     (theory-congruent-p theory))
		(not *hopeless-fail*))
	   (and (when (get rule 'bidirectional)
		  (setq *hopeless-fail* nil)
		  (apply-rrule-once-if inwff outwff (get rule 'after)
				       (get rule 'before) (get rule 'rewfn)
				       (get rule 'appfn)
				       (get rule 'rtypelist)
				       (theory-congruent-p theory)))
		(not *hopeless-fail*)))))

(defun generate-rlist (wff rule dir theory)
  (labels ((assemble1 (wff rews)
		      (mapcar #'(lambda (rew) (acons wff (car rew) (cdr rew)))
			      rews))
	   (assemble2 (rews wff)
		      (mapcar #'(lambda (rew) (acons (car rew) wff (cdr rew)))
			      rews))
	   (rewrite (wff dir)
		    (let ((oneflag nil))
		      (declare (special oneflag))
		      (let ((rews
		    (apply-rrule-poly-flat
		     wff
		     (get rule (if dir 'before 'after))
		     (get rule (if dir 'after 'before))
		     (get rule 'rewfn) (get rule 'appfn) (get rule 'rtypelist)
		     #'(lambda (abbsym chkarg)
			 (declare (ignore abbsym chkarg)
				  (special oneflag))
			 (prog1 (not oneflag) (setq oneflag t)))
		     nil
		     ;(theory-congruent-p theory)
		     )))
			(cons rews
			      (delete-if
			       #'(lambda (x) (not (free-in x rews)))
			       (mapcar #'(lambda (x)
					   (substitute-types *type-subs*
						(apply-binding-subs x)))
				       (get rule 'variables)))))))
	   (gen-rewrite (wff dir)
			;(when (or dir (get rule 'bidirectional))
			(cons ;(rewrite wff t)
			      (rewrite wff dir)
			      (if (get rule 'bidirectional)
				  (list ;(rewrite wff nil)
				        (rewrite wff (not dir))) nil)));)
	   (rewrites (wff dir)
		     (if (theory-congruent-p theory)
			 (if (lsymbol-p wff) (gen-rewrite wff dir)
			   (if (boundwff-p wff)
			       (append (gen-rewrite wff dir)
				       (assemble1 (car wff) (rewrites (gdr wff) dir)))
			     (append (gen-rewrite wff dir)
				     (append (assemble2 (rewrites (gar wff) dir)
							(gdr wff))
					     (assemble1 (gar wff)
							(rewrites (gdr wff) dir))))))
		       (gen-rewrite wff dir))))
    (remove-if
     #'(lambda (rew)
	 (wffeq-ab wff (car rew)))
     (remove-duplicates (rewrites wff dir)
			:test #'(lambda (x y) (wffeq-ab (car x) (car y)))))))

(defun eta-appfn (wff &rest args)
  (declare (special s-eqn-eta-exp s-eqn-eta-initial-wff))
  (setq s-eqn-eta-initial-wff wff)
  (setq s-eqn-eta-exp  ; s-eqn-eta-exp is set to t iff we have no eta-redex
	(not (and (lambda-bd-p wff)
		  (wff-applic-p (gdr wff))
		  (propsym-p (gdr (gdr wff)))
		  (eq (bdvar wff) (gdr (gdr wff)))
		  (not (free-in (gdr (gdr wff)) (gar (gdr wff)))))))
  (if args
      (let ((after (cadr args)))
	(if (propsym-p after) (not s-eqn-eta-exp)
	  (consp (type wff))))
    (if s-eqn-eta-exp (consp (type wff)) t)))

(defun eta-rewfn (wff &rest args)
  (declare (special s-eqn-eta-exp s-eqn-eta-initial-wff))
  (if (and (legal-type-p wff)
	   (or (and (lambda-bd-p wff)    ; test for eta-expansion
		    (wff-applic-p (gdr wff))
		    (wffeq-ab (gar (gdr wff)) s-eqn-eta-initial-wff)
		    (eq (bdvar wff) (gdr (gdr wff)))
		    (not (free-in (bdvar wff) s-eqn-eta-initial-wff)))
	       (and (not s-eqn-eta-exp)  ; test for admissible eta-reduction
		    (wffeq-ab wff (gar (gdr s-eqn-eta-initial-wff))))))
      wff
    s-eqn-eta-initial-wff))

(defun beta-appfn (wff &rest args)
  (declare (special s-eqn-beta-initial-wff))
  (setq s-eqn-beta-initial-wff wff)
  (if (null args) t
    (let ((before (car args))
	  (after (cadr args)))
      (if (and (wff-applic-p before)
	       (propsym-p after))
	  (and (wff-applic-p wff)
	       (lambda-bd-p (gar wff)))
	t))))

(defun beta-rewfn (wff &rest args)
  (declare (special s-eqn-beta-initial-wff))
  (let ((before (if (null args) (cons '|h<BA>| '|u<A>|) (car args)))
	(after (if (null args) (create-propsym "v" 'B) (cadr args))))
    (if (and (wff-applic-p before)
	     (propsym-p after)
	     (wff-applic-p s-eqn-beta-initial-wff)
	     (lambda-bd-p (gar s-eqn-beta-initial-wff)))
	(substitute-l-term-var (gdr s-eqn-beta-initial-wff)
			       (bdvar (gar s-eqn-beta-initial-wff))
			       (gdr (gar s-eqn-beta-initial-wff)))
      (if (and (propsym-p before)
	       (wff-applic-p after)
	       (wff-applic-p wff)
	       (lambda-bd-p (gar wff))
	       (wffeq-ab s-eqn-beta-initial-wff
			 (substitute-l-term-var (gdr wff)
						(bdvar (gar wff))
						(gdr (gar wff)))))
	  wff
	s-eqn-beta-initial-wff))))

; The below appfn ensures that in subwffs of the form "lambda x.f x", x does
; not occur free in f.

(defun s-eqn-axiom-instance (inwff before types bdsubs)
  (cond ((lsymbol-q before)
	 (let* ((newtypelist (suggest-types inwff before types))
		(before-type-inst (substitute-types newtypelist before))
		(fvars-inwff (free-vars-of inwff))
		(bdvars (mapcar #'cdr bdsubs)))
	   (setq *type-subs* (nconc newtypelist *type-subs*))
	   (cond ((assoc before bdsubs)
		  (wffeq-ab1 before inwff bdsubs))
		 ((assoc before *binding-subs*)
		  (and (wffeq-ab (cdr (assoc before *binding-subs*)) inwff)
		       (foldl #'(lambda (x a) (and a (not (member x bdvars))))
			      t fvars-inwff)))
		 (t (or (wffeq inwff before-type-inst)
			(and (propsym-p before)
			     (type-equal inwff before-type-inst)
			     (foldl #'(lambda (x a)
					(and a (not (member x bdvars))))
				    t fvars-inwff)
			     (push (cons before inwff) *binding-subs*)))))))
	((boundwff-q before)
	 (and (boundwff-q inwff)
	      (eq (binder inwff) (binder before))
	      ;(push (cons (bdvar before) (bdvar inwff)) *binding-subs*)
	      (s-eqn-axiom-instance (gdr inwff) (gdr before) types
				    (acons (bdvar before)
					   (bdvar inwff) bdsubs))))
	((boundwff-q inwff) nil)
	(t (and (consp inwff) (consp before)
		(s-eqn-axiom-instance (car inwff) (car before) types bdsubs)
		(s-eqn-axiom-instance (cdr inwff) (cdr before) types bdsubs)))))

(defun s-eqn-axiom-appfn (wff before after types &rest args)
  (declare (ignore args after)
	   (special s-eqn-axiom-types s-eqn-axiom-initial-wff))
  (let* ((bind-subs *binding-subs*)
	 (type-subs *type-subs*)
	 (result (s-eqn-axiom-instance wff before types nil)))
    (setq s-eqn-axiom-types types)
    (setq s-eqn-axiom-initial-wff wff)
    (setq *binding-subs* bind-subs)
    (setq *type-subs* type-subs)
    result))

(defun s-eqn-axiom-rewfn (wff before after &rest args)
  (declare (ignore args before)
	   (special s-eqn-axiom-types s-eqn-axiom-initial-wff))
  (let* ((bind-subs *binding-subs*)
	 (type-subs *type-subs*)
	 (result (s-eqn-axiom-instance wff after s-eqn-axiom-types nil)))
    (setq *binding-subs* bind-subs)
    (setq *type-subs* type-subs)
    (if result wff s-eqn-axiom-initial-wff)))

(defun inference-scheme-appfn (wff &rest args)
  (declare (ignore args)
	   (special inference-initial-wff))
  (setq inference-initial-wff wff)
  t)

(defun make-inference-rewfn-from-rrule (rule cong)
  (declare (special inference-initial-wff))
  (lambda (wff before after &rest args)
    (declare (ignore before))
    (let* ((tsubs *type-subs*)
	   (bsubs *binding-subs*)
	   (res (if (and (consp wff) (consp (car wff))
			 (instance-of-ruleapp rule (cdar wff)
					      (cdr wff) (not cong)))
		    wff
		  inference-initial-wff)))
      (setq *type-subs* tsubs)
      (setq *binding-subs* bsubs)
      res)))

(defun inference-match-binders-rewfn (wff before after &rest args)
  (declare (ignore args)
	   (special inference-initial-wff))
  ;(format t "~&match-binders-rewfn: ~A, ~A~&vs. ~A, ~A" inference-initial-wff wff before after)
  (let ((bsubs nil))
    (labels ((match-rec (schema wff)
	       (cond ((lsymbol-q schema) t)
		     ((boundwff-q schema)
		      (and (if (assoc (bdvar schema) bsubs)
			       (eq (bdvar wff)
				   (cdr (assoc (bdvar schema) bsubs)))
			     (push (cons (bdvar schema) (bdvar wff)) bsubs))
			   (match-rec (cdr schema) (cdr wff))))
		     (t (and (match-rec (car schema) (car wff))
			     (match-rec (cdr schema) (cdr wff)))))))
      (if (and (match-rec before inference-initial-wff)
	       (match-rec after wff))
	  wff
	inference-initial-wff))))

(defun inference-beta-rewfn (&rest args)
  (let ((rule (gensym "rule")))
    (setf (get rule 'before) (getwff-subtype 'gwff-p "r(VU) s(U)"))
    (setf (get rule 'after) (getwff-subtype 'gwff-p "t(V)"))
    (setf (get rule 'derived-in) nil)
    (setf (get rule 'variables) (list (create-propsym "r" '(V . U))
				      (create-propsym "s" 'U)
				      (create-propsym "t" 'V)))
    (setf (get rule 'rtypelist) '(cl-user::U cl-user::V))
    (setf (get rule 'bidirectional) nil)
    (setf (get rule 'appfn) 'beta-appfn)
    (setf (get rule 'rewfn) 'beta-rewfn)
    (setf (get rule 'rewrite-rule) t)
    (apply (make-inference-rewfn-from-rrule rule nil) args)))

(defun inference-eta-rewfn (&rest args)
  (let ((rule (gensym "rule")))
    (setf (get rule 'before) (getwff-subtype 'gwff-p "lambda x(U).f(VU) x"))
    (setf (get rule 'after) (getwff-subtype 'gwff-p "f(VU)"))
    (setf (get rule 'derived-in) nil)
    (setf (get rule 'rtypelist) '(cl-user::U cl-user::V))
    (setf (get rule 'bidirectional) nil)
    (setf (get rule 'appfn) 'eta-appfn)
    (setf (get rule 'rewfn) 'eta-rewfn)
    (setf (get rule 'rewrite-rule) t)
    (apply (make-inference-rewfn-from-rrule rule nil) args)))

(defun de-bruijn-ize (wff)
  (labels ((subst (wff bdvars)
	     (cond (;(and
		    (propsym-p wff)
		    ;(not (anyabbrev-p wff)))
		    (let ((idx (search (list wff) bdvars)))
		      (if idx idx wff)))
		   ((boundwff-p wff)
		    (cons (cdar wff)
			  (subst (cdr wff) (cons (caar wff) bdvars))))
		   ((consp wff) (cons (subst (car wff) bdvars)
				      (subst (cdr wff) bdvars)))
		   (t wff))))
    (subst wff nil)))

(defun rewrite-search (src trg rules depth)
  (declare (special s-eqn-rule-instance))
  (let ((tab (make-hash-table :test #'equal
			      :size rewriting-auto-table-size))
	(bvars (remove-duplicates (nconc (free-vars-of src)
					 (free-vars-of trg))))
	(sterms (if rewriting-auto-substs rewriting-auto-substs
		  (labels ((get-sterms (wff)
			     (cond ((lsymbol-q wff) (list wff))
				   (t (nconc (get-sterms (gar wff))
					     (get-sterms (gdr wff)))))))
		    (get-sterms (cons src trg)))))
	(trg-key (de-bruijn-ize trg)))
    (labels ((gen-subslist (fvars)
	       (if fvars
		   (let ((rest (gen-subslist (cdr fvars))))
		     (foldl #'(lambda (subs rest)
				(nconc
				 (delete-if
				  #'null
				  (mapcar #'(lambda (s)
					      (if (type-equal (car fvars) s)
						  (acons (car fvars) s subs)
						subs))
					  sterms))
				 rest))
			    nil rest))
		 (list nil)))
	     (subst-fvars (wff bdvars)
	       (let* ((fvars (set-difference (free-vars-of wff)
					     (append bdvars bvars)
					     :test #'eq))
		      (subslist (gen-subslist fvars)))
		 (foldl #'(lambda (subs wffs)
			    (acons (foldl
				    #'(lambda (sub wff)
					(substitute-l-term-var (cdr sub)
							       (car sub) wff))
				    wff subs) subs wffs))
			nil subslist)))
	     (instantiate-wffs (wffs bv)
			       (foldl #'(lambda (wff wffs)
					  (let ((insts (subst-fvars wff bv)))
					    (if insts (nconc insts wffs)
					      (cons (list wff) wffs))))
				      nil wffs))
	     (wff-size (wff) (cond ((lsymbol-q wff) 1)
				   (t (+ (wff-size (car wff))
					 (wff-size (cdr wff))))))
	     (search-rew (src bdvars depth)
	       (cond
		((gethash trg-key tab) (list trg))
		((<= depth 0) nil)
		((<= rewriting-auto-table-size (hash-table-count tab))
		 "hash")
		((> (wff-size src) rewriting-auto-max-wff-size) nil)
		(t (dolist (rule rules)
		     (let* ((sth (subtheory-containing rule))
			    (rew-keys (mapcar
				       #'(lambda (rew)
					  (cons (de-bruijn-ize (car rew)) rew))
				       (instantiate-wffs
					(generate-rwfflist
					 src (instantiate-rrule rule) t sth)
					bdvars)))
			    (rews
			     (delete-if
			      #'(lambda (rewp)
				  (let ((d (gethash (car rewp) tab)))
				    (or (and d (>= d depth))
					(progn
					  (setf (gethash (car rewp) tab)
					        depth)
						;(- depth 1))
					  nil))))
			      rew-keys))
			    (result
			     (dolist (rew rews)
			       (uninstantiate-rrule s-eqn-rule-instance)
			       ;(setf (gethash (car rew) tab) depth)
			       (let ((result
				      (search-rew (cadr rew)
						  (nconc (mapcar #'cdr
								 (cddr rew))
							 bdvars)
						  (- depth 1))))
				 (cond ((stringp result) (return result))
				       (result
					(let ((res2 (cons src
							  (cons rule result))))
					  (return res2))))))))
		       (when result (return result))))))))
      (setf (gethash (de-bruijn-ize src) tab) depth)
      ;(search-rew src depth))))  ; that's dfs
      (dotimes (depth (+ (- depth rewriting-auto-min-depth) 1))
	(let ((result (search-rew src nil (+ depth rewriting-auto-min-depth))))
	  (when result (return result)))))))

(defun rewrite-search2 (src trg rules depth)
  (declare (special s-eqn-rule-instance))
  (let ((tab (make-hash-table :test #'equal
			      :size rewriting-auto-table-size))
	(rtab (make-hash-table :test #'equal
			       :size rewriting-auto-table-size))
	(bvars (remove-duplicates (nconc (free-vars-of src)
					 (free-vars-of trg))))
	(sterms (if rewriting-auto-substs rewriting-auto-substs
		  (labels ((get-sterms (wff)
			     (cond ((lsymbol-q wff) (list wff))
				   (t (nconc (get-sterms (gar wff))
					     (get-sterms (gdr wff)))))))
		    (get-sterms (cons src trg)))))
	(src-key (de-bruijn-ize src))
	(trg-key (de-bruijn-ize trg)))
    (labels ((gen-subslist (fvars)
	       (if fvars
		   (let ((rest (gen-subslist (cdr fvars))))
		     (foldl #'(lambda (subs rest)
				(nconc
				 (delete-if
				  #'null
				  (mapcar #'(lambda (s)
					      (if (type-equal (car fvars) s)
						  (acons (car fvars) s subs)
						subs))
					  sterms))
				 rest))
			    nil rest))
		 (list nil)))
	     (subst-fvars (wff bdvars)
	       (let* ((fvars (set-difference (free-vars-of wff)
					     (append bdvars bvars)
					     :test #'eq))
		      (subslist (gen-subslist fvars)))
		 (foldl #'(lambda (subs wffs)
			    (acons (foldl
				    #'(lambda (sub wff)
					(substitute-l-term-var (cdr sub)
							       (car sub) wff))
				    wff subs) subs wffs))
			nil subslist)))
	     (instantiate-wffs (wffs bv)
			       (foldl #'(lambda (wff wffs)
					  (let ((insts (subst-fvars wff bv)))
					    (if insts (nconc insts wffs)
					      (cons (list wff) wffs))))
				      nil wffs))
	     (wff-size (wff) (cond ((lsymbol-q wff) 1)
				   (t (+ (wff-size (car wff))
					 (wff-size (cdr wff))))))
	     (curkey (stack) (cadar stack))
	     (curdepth (stack) (caaar stack))
	     (pred (v) (cdddr v))
	     (pred-key (v) (caddr v))
	     (pred-rule (v) (cadr v))
	     (curval (stack) (caar stack))
	     (curwff (stack) (caddar stack))
	     (bdvars (stack) (cdddar stack))
	     (extract-path (wff v tab)
	       (if (pred-rule v)
		   (cons wff (cons (pred-rule v)
				   (extract-path (pred v)
						 (gethash (pred-key v) tab)
						 tab)))
		 (list wff)))
	     (search-rew (stack rstack tab rtab dir)
	       (cond
		((gethash (curkey stack) rtab)
		 (let ((path (nconc
			      (reverse
			       (extract-path (curwff stack)
					     (gethash (curkey stack) tab)
					     tab))
			      (cdr (extract-path
				    (curwff stack)
				    (gethash (curkey stack) rtab)
				    rtab)))))
		   (if dir path (reverse path))))
		((not (or stack rstack))
		 (if (and (<= rewriting-auto-table-size
			      (hash-table-count tab))
			  (<= rewriting-auto-table-size
			      (hash-table-count rtab)))
		     "hash"
		   nil))
		((null stack) (search-rew rstack stack rtab tab (not dir)))
		((or (<= (curdepth stack) 0)
		     (<= rewriting-auto-table-size (hash-table-count tab))
		     (> (wff-size (curwff stack)) rewriting-auto-max-wff-size))
		 (search-rew rstack (cdr stack) rtab tab (not dir)))
		(t (let* ((src (curwff stack))
			  (depth (curdepth stack))
			  (src-key (curkey stack))
			  (bdvars (bdvars stack))
			  (newstack
			   (foldl
			    #'(lambda
				(rule stack)
				(let*
				    ((sth (subtheory-containing rule))
				     (rew-keys
				      (mapcar
				       #'(lambda (rew)
					  (cons (de-bruijn-ize (car rew)) rew))
				       (instantiate-wffs
					(generate-rwfflist
					 src (instantiate-rrule rule) dir sth)
					bdvars)))
				     (rews
				      (delete-if
				       #'(lambda (rewp)
					   (let ((d (car (gethash (car rewp)
								  tab))))
					     (or (and d (>= d depth))
						 (progn
						   (setf (gethash (car rewp)
								  tab)
							 (cons depth
							       (cons
								rule
								(cons src-key
								      src))))
						   nil))))
				       rew-keys)))
				  (uninstantiate-rrule s-eqn-rule-instance)
				  (foldl
				   #'(lambda
				       (rew stack)
				       (acons (cons (- depth 1)
						    (cons rule
							  (cons src-key src)))
					      (cons (car rew)
						    (cons (cadr rew)
							  (nconc
							   (mapcar #'cdr
								   (cddr rew))
							   bdvars)))
					      stack))
				   stack rews)))
			    (cdr stack) rules)))
		     (search-rew rstack newstack rtab tab (not dir)))))))
      (setf (gethash src-key tab) (cons (+ depth 1) nil))
      (setf (gethash trg-key rtab) (cons (+ depth 1) nil))
      (dotimes (depth (+ (- depth rewriting-auto-min-depth) 1))
	(let ((result (search-rew
		       (list (cons (cons (+ depth rewriting-auto-min-depth)
					 nil)
				   (cons src-key
					 (cons src nil))))
		       (list (cons (cons (+ depth rewriting-auto-min-depth)
					 nil)
				   (cons trg-key
					 (cons trg nil))))
		       tab rtab t)))
	  (when result (return result)))))))

(defun rewrite-search3 (src trg rules depth)
  (declare (special s-eqn-rule-instance))
  (let ((tab (make-hash-table :test #'equal
			      :size rewriting-auto-table-size))
	(rtab (make-hash-table :test #'equal
			       :size rewriting-auto-table-size))
	(bvars (remove-duplicates (nconc (free-vars-of src)
					 (free-vars-of trg))))
	(sterms (if rewriting-auto-substs rewriting-auto-substs
		  (labels ((get-sterms (wff)
			     (cond ((lsymbol-q wff) (list wff))
				   (t (nconc (get-sterms (gar wff))
					     (get-sterms (gdr wff)))))))
		    (get-sterms (cons src trg)))))
	(src-key (de-bruijn-ize src))
	(trg-key (de-bruijn-ize trg)))
    (labels ((gen-subslist (fvars)
	       (if fvars
		   (let ((rest (gen-subslist (cdr fvars))))
		     (foldl #'(lambda (subs rest)
				(nconc
				 (delete-if
				  #'null
				  (mapcar #'(lambda (s)
					      (if (type-equal (car fvars) s)
						  (acons (car fvars) s subs)
						subs))
					  sterms))
				 rest))
			    nil rest))
		 (list nil)))
	     (subst-fvars (wff bdvars)
	       (let* ((fvars (set-difference (free-vars-of wff)
					     (append bdvars bvars)
					     :test #'eq))
		      (subslist (gen-subslist fvars)))
		 (foldl #'(lambda (subs wffs)
			    (acons (foldl
				    #'(lambda (sub wff)
					(substitute-l-term-var (cdr sub)
							       (car sub) wff))
				    wff subs) subs wffs))
			nil subslist)))
	     (instantiate-wffs (wffs bv)
			       (foldl #'(lambda (wff wffs)
					  (let ((insts (subst-fvars wff bv)))
					    (if insts (nconc insts wffs)
					      (cons (list wff) wffs))))
				      nil wffs))
	     (wff-size (wff) (cond ((lsymbol-q wff) 1)
				   (t (+ (wff-size (car wff))
					 (wff-size (cdr wff))))))
	     (curkey (stack) (cadar stack))
	     (curdepth (stack) (caaar stack))
	     (pred (v) (cdddr v))
	     (pred-key (v) (caddr v))
	     (pred-rule (v) (cadr v))
	     (curval (stack) (caar stack))
	     (curwff (stack) (caddar stack))
	     (bdvars (stack) (cdddar stack))
	     (extract-path (wff v tab)
	       (if (pred-rule v)
		   (cons wff (cons (pred-rule v)
				   (extract-path (pred v)
						 (gethash (pred-key v) tab)
						 tab)))
		 (list wff)))
	     (append-stack (new old order global)
	       (if order (if global (stable-sort (nconc new old) order)
			   (nconc (stable-sort new order) old))
		 (nconc new old)))
	     (search-rew (stack rstack tab rtab dir)
	       (cond
		((gethash (curkey stack) rtab)
		 (let ((path (nconc
			      (reverse
			       (extract-path (curwff stack)
					     (gethash (curkey stack) tab)
					     tab))
			      (cdr (extract-path
				    (curwff stack)
				    (gethash (curkey stack) rtab)
				    rtab)))))
		   (if dir path (reverse path))))
		((not (or stack rstack))
		 (if (and (<= rewriting-auto-table-size
			      (hash-table-count tab))
			  (<= rewriting-auto-table-size
			      (hash-table-count rtab)))
		     "hash"
		   nil))
		((null stack) (search-rew rstack stack rtab tab (not dir)))
		((or (<= (curdepth stack) 0)
		     (<= rewriting-auto-table-size (hash-table-count tab))
		     (> (wff-size (curwff stack)) rewriting-auto-max-wff-size))
		 (search-rew rstack (cdr stack) rtab tab (not dir)))
		(t (let* ((src (curwff stack))
			  (depth (curdepth stack))
			  (src-key (curkey stack))
			  (bdvars (bdvars stack))
			  (newstack
			   (append-stack
			   (foldl
			    #'(lambda
				(rule stack)
				(let*
				    ((sth (subtheory-containing rule))
				     (rew-keys
				      (mapcar
				       #'(lambda (rew)
					  (cons (de-bruijn-ize (car rew)) rew))
				       (instantiate-wffs
					(generate-rwfflist
					 src (instantiate-rrule rule) dir sth)
					bdvars)))
				     (rews
				      (delete-if
				       #'(lambda (rewp)
					   (let ((d (car (gethash (car rewp)
								  tab))))
					     (or (and d (>= d depth))
						 (progn
						   (setf (gethash (car rewp)
								  tab)
							 (cons depth
							       (cons
								rule
								(cons src-key
								      src))))
						   nil))))
				       rew-keys)))
				  (uninstantiate-rrule s-eqn-rule-instance)
				  (foldl
				   #'(lambda
				       (rew stack)
				       (acons (cons (- depth 1)
						    (cons rule
							  (cons src-key src)))
					      (cons (car rew)
						    (cons (cadr rew)
							  (append
							   (mapcar #'cdr
								   (cddr rew))
							   bdvars)))
					      stack))
				   stack rews)))
			    nil rules)
			   (cdr stack) #'(lambda (v1 v2)
					   (< (wff-size (caddr v1))
					      (wff-size (caddr v2))))
			   rewriting-auto-global-sort)))
		     (search-rew rstack newstack rtab tab (not dir)))))))
      (setf (gethash src-key tab) (cons (+ depth 1) nil))
      (setf (gethash trg-key rtab) (cons (+ depth 1) nil))
      (dotimes (depth (+ (- depth rewriting-auto-min-depth) 1))
	(let ((result (search-rew
		       (list (cons (cons (+ depth rewriting-auto-min-depth)
					 nil)
				   (cons src-key
					 (cons src nil))))
		       (list (cons (cons (+ depth rewriting-auto-min-depth)
					 nil)
				   (cons trg-key
					 (cons trg nil))))
		       tab rtab t)))
	  (when result (return result)))))))

(defun trules-used-by (theory)
  (if (car (get theory 'extends))
      (append
       (car (get theory 'rrules)) 
       (reduce
	'append
	(mapcar 'rrules-used-by
		(remove-if
		 #'(lambda (th) (and (get theory 'relation-sign)
				     (get th 'relation-sign)
				     (not (eq (get theory 'relation-sign)
					      (get th 'relation-sign)))))
		 (car (get theory 'extends))))))
    (car (get theory 'rrules))))

(defun conservative-subtheories (theory)
  (cons theory
	(remove-duplicates
	 (reduce #'append
		 (mapcar
		  #'conservative-subtheories
		  (remove-if #'(lambda (th)
				 (and (get theory 'relation-sign)
				      (get th 'relation-sign)
				      (not (eq (get theory 'relation-sign)
					       (get th 'relation-sign)))))
			     (car (get theory 'extends))))))))

(defun subtheories (theory)
  (cons theory
	(remove-duplicates
	 (reduce #'append
		 (mapcar #'subtheories (car (get theory 'extends)))))))

(defun non-conservative-subtheories (theory)
  (set-difference (subtheories theory) (conservative-subtheories theory)))

(defun subtheory-containing (rule &rest args)
  (let ((theory (if (null args) (get *current-eqnder* 'theory) (car args))))
    (when theory
      (if (member rule (car (get theory 'rrules))) theory
	(dolist (th (cdr (subtheories theory)))
	  (let ((sth (subtheory-containing rule th)))
	    (when sth (return sth))))))))

(defun apply-rrule-poly-flat (inwff before after func appfn types chkfn chkarg)
  (setq *binding-subs* nil)
  (setq *type-subs* nil)
  (setq *binding-subs-bdstack* nil)

  ;(format t "~&arpf called with ~A, ~A, ~A~&rip: ~A, chkfn: ~A~&rrp: ~A"
	;  inwff before after
	;  (rrule-instance-poly appfn inwff before after types)
	;  (funcall chkfn nil chkarg)
	;  (replace-rrule-poly inwff before after types))
  ;(setq *binding-subs* nil)
  ;(setq *type-subs* nil)
  ;(setq *binding-subs-bdstack* nil)

  ;(let ((res

  (if (rrule-instance-poly appfn inwff before after types)
      (if (funcall chkfn nil chkarg)
	  (if func (funcall func (replace-rrule-poly inwff before after types)
			    before after)
	    (replace-rrule-poly inwff before after types))
	inwff)
    inwff))

  ;)) (format t "~&arpf -> ~A" res) res))

(defun apply-rrule-once-flat (inwff outwff before after func appfn types)
  (setq *binding-subs* nil)
  (setq *type-subs* nil)
  (if (rrule-instance-poly appfn inwff before after types)
      (let ((newwff (if func
			(funcall func
				 (replace-rrule-uqvars inwff outwff before
						       after types)
				 before after)
		      (replace-rrule-uqvars inwff outwff before
					    after types))))
	(or (and newwff (wffeq-ab newwff outwff))
	    (progn (setq *hopeless-fail* t) nil)))
    (progn (setq *hopeless-fail* t) nil)))

(defun apply-rrule-poly-if (inwff before after func appfn types chkfn
			    chkarg cong)
  (if cong (apply-rrule-poly inwff before after func appfn types chkfn chkarg)
  (apply-rrule-poly-flat inwff before after func appfn types chkfn chkarg)))

(defun apply-rrule-once-if (inwff outwff before after func appfn types cong)
  (if cong (apply-rrule-once-uqvars inwff outwff before after func appfn types)
    (apply-rrule-once-flat inwff outwff before after func appfn types)))
