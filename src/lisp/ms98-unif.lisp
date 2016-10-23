;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(context ms98-1)

(deffile ms98-unif
  (part-of ms98)
  (extension lisp)
  (mhelp "The unification functions for MS98-1"))

(defun make-new-dpair (conn fvlist)
  (if (and first-order-mode-ms (not ms98-force-h-o))
      (fo-make-dpair conn fvlist)
  (ccs-initialize-conn (car conn) (cdr conn) fvlist)))

(defun fo-make-dpair (conn fvlist)
  (let ((neg1 (not-p (jform-represents (car conn))))
	(neg2 (not-p (jform-represents (cdr conn)))))
    (if (eq (if neg1 (jform-pos (car conn)) (not (jform-pos (car conn))))
	    (if neg2 (not (jform-pos (car conn))) (jform-pos (cdr conn))))
	(cons (cons (if neg1 (cdr (jform-represents (car conn)))
		      (jform-represents (car conn))) fvlist)
	      (cons (if neg2 (cdr (jform-represents (cdr conn)))
		      (jform-represents (cdr conn))) fvlist))
      (cons (cons 'truth fvlist) (cons 'falsehood fvlist)))))

(defun qum (dpairs mating &optional (touches nil) (key 0) (litkey 0))
  (runcount 'unification)
  (let ((return-value
	 (qum-real dpairs mating key litkey touches)))
    (breakcount 'unification)
    return-value))

(defun qum-real (dpairs mating key litkey touches &optional (force-now (or (not ff-delay) (= ms98-merge-dags 2))))
  (if (and first-order-mode-ms (not ms98-force-h-o))
      (multiple-value-bind (substs unifiablep) 
	  (ccs-pfd-fo-unify-real dpairs nil)
	(when unifiablep (if (= key -1) t 
			   (construct-component (full-normalize-fo substs)
						mating key litkey touches))))
    (if *ccs-rewrites* (qum-rewrites dpairs mating key litkey touches)
      (progn (setq dpairs (car dpairs))	;there's only one, we're using unifhash
	     (if (eq (car dpairs) t)
		 (if force-now
		     (let* ((d1 (simpl-full (list (cons (cons 'not (cadr dpairs)) (cddr dpairs))) nil))
			    (d2 (simpl-full (list (cons (cons 'not (cddr dpairs)) (cadr dpairs))) nil))
			    (nodes (mapcar #'qum-real-fn-1 
					   (remove-if #'qum-real-fn-2 (list d1 d2)))))
		       (when nodes (unify-unifhash nodes mating key litkey touches)))
		   (progn (msg "?") (construct-component 'delay mating key litkey touches)))
	       (let* ((d1 (simpl-full (list dpairs) nil (not force-now)))
		      (simpl-says-delay (and (listp d1) (eq (car d1) 'delay)))
		      (d1 (if simpl-says-delay (cdr d1) d1))
		      (nodes (unless (or (eq d1 'fail) simpl-says-delay)
			       (list (make-qnode :dpairs d1)))))
		 (if nodes 
		     (unify-unifhash nodes mating key litkey touches)
		   (if simpl-says-delay
		       (progn (msg "?") (construct-component 'delay mating key litkey touches)) nil))))))))

(defun qum-real-fn-1 (elt) (make-qnode :dpairs elt))
(defun qum-real-fn-2 (elt) (eq elt 'fail))

(defun qum-rewrites (dpairs mating key litkey touches)
  (unify-unifhash-rew (list (make-qnode :dpairs dpairs))
		      mating key litkey touches))

(defun lnorm-q (wff &optional (subst nil))
  (ho-unif-lnorm2 nil wff nil subst nil))

(defun process-further (list)
  (if (symbolp list) list
    (if (and (consp list) (numberp (cdr list)))
	(if (or (equal (car list) 'var) (h-var-check (car list))) '*** (car list))
      (let ((val (process-further (car list))))
	(if (equal val '***) '*** (cons val (process-further (cdr list))))))))

(defun process-further-fo (list)
  (if (symbolp list) (if (memq list (free-vars-in-etree current-eproof)) '*** list)
    (if (and (consp list) (numberp (cdr list)))
	(if (equal (car list) 'var) '*** (car list))
      (let ((val (process-further-fo (car list))))
	(if (equal val '***) '*** (cons val (process-further-fo (cdr list))))))))

(defun get-resulttype (fhead)
  (car (last (listify-vec (listify-type (type (car fhead)))))))

(defun choose-dpair (dpairs)
  (let ((min-subs 1000000)
	min-dpair)
    (dolist (d dpairs min-dpair)
	    (let ((subs1 (length (cdr (assoc (get-resulttype (find-fhead (car d))) *global-constlist*))))
		  (subs2 (length (cdr (assoc (get-resulttype (find-fhead (cdr d))) *global-constlist*)))))
	      (if (<= subs1 subs2)
		  (if (< subs1 min-subs)
		      (setq min-subs subs1 min-dpair d))
		(if (< subs2 min-subs)
		    (setq min-subs subs2 min-dpair (cons (cdr d) (car d))))))
	    (if (= min-subs 1) (return min-dpair)))))
		  
(defun ccs-match-i (rhead rterm-binder fhead)
  (let* ((type-fhead (listify-type (type (car fhead))))
         (p1 (1- (length type-fhead)))
         (w-vars (create-new-w-vars type-fhead p1)))
    (imitation-ho rhead w-vars type-fhead (listify-type (type rhead))
		  p1 rterm-binder fhead)))

(defun ccs-match-p (fhead)
  (let* ((type-fhead (listify-type (type (car fhead))))
         (p1 (1- (length type-fhead)))
         (w-vars (create-new-w-vars type-fhead p1)))
     (projections-ho w-vars type-fhead p1 fhead)))

(defun ccs-get-binder (term)
  (if (lambda-bd-p term)
      (cons (caar term) (ccs-get-binder (cdr term)))
    nil))

(defun make-new-dpairs (dpairs subst)
  (mapcar #'(lambda (x) (cons (ho-unif-lnorm2 nil (car x) nil subst nil)
			      (ho-unif-lnorm2 nil (cdr x) nil subst nil)))
	  dpairs))

(defun ho-unif-lnorm2 (neg gwff bdvars sstack bstack)
  (multiple-value-bind (gwff neg)
      (ho-unif-lnorm2-rec neg gwff bdvars sstack bstack)
    (if neg (cons 'not gwff) gwff)))

(defun ho-unif-lnorm2-rec (neg gwff bdvars sstack bstack)
  (cond ((symbolp gwff)
         (if (member gwff bdvars) (values gwff neg)
             (let ((term (assoc gwff bstack :test #'equal)))
               (if term 
		   (ho-unif-lnorm2-rec neg (cdr term) nil sstack bstack)
		 (values gwff neg)))))
        ((numberp (cdr gwff))
         (let ((term (or (assoc gwff sstack :test #'equal) (assoc (car gwff) sstack :test #'equal))))
           (if term 
	       (if (numberp (cdr term))
		   (ho-unif-lnorm2-rec neg (gethash (cdr term) *ccs-substs*) nil sstack bstack)
		 (ho-unif-lnorm2-rec neg (cdr term) bdvars sstack bstack))
	     (values gwff neg))))
        ((not-p gwff)
         (ho-unif-lnorm2-rec (not neg) (cdr gwff) bdvars sstack bstack))
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
                (cons (car gwff) (ho-unif-lnorm2
                                  nil (cdr gwff) (cons (caar gwff) bdvars)
                                  sstack bstack)))
               (t (throwfail "HO-UNIF-LNORM2-REC: Unknown bdwfftype in: "
                             (gwff . gwff)))))
        (t (if (lambda-bd-p (car gwff))
               (ho-unif-lnorm2
                neg (cdar gwff) bdvars sstack
                (acons (caaar gwff)
                       (if bstack
                           (ho-unif-lnorm2-rec nil (cdr gwff) bdvars sstack bstack)
                           (cdr gwff))
                       bstack))
               (let ((gwff (cons (ho-unif-lnorm2-rec
                                  nil (car gwff) bdvars sstack bstack)
				 (ho-unif-lnorm2-rec nil (cdr gwff) bdvars sstack
						 bstack))))
                 (if (lambda-bd-p (car gwff))
                     (ho-unif-lnorm2 neg (cdar gwff) bdvars sstack
                                        (acons (caaar gwff) (cdr gwff) bstack))
                     (values gwff neg)))))))

(defun find-consts ()
					;  (let ((consts (symbols-of (get-deep current-topnode))) ;(eproof-etree current-eproof))))
					; changed this to get symbols from literals of jform, so we can include nonleaves - cebrown 10/6/00
  (let ((consts (symbols-of-lits (ob-jform-to-literal-list ms90-3-jform)))
	(fvs (free-vars-in-etree current-eproof)))
    (setdiff consts fvs)))

(defun find-symbols ()
					;  (let ((consts (symbols-of (get-deep current-topnode))); (eproof-etree current-eproof))))
					; changed this to get symbols from literals of jform, so we can include nonleaves - cebrown 10/6/00
  (let ((consts (symbols-of-lits (ob-jform-to-literal-list ms90-3-jform)))
	(fvs (free-vars-in-etree current-eproof)))
    (append (setdiff consts fvs) (mapcar #'make-generic-var (remove-duplicates (mapcar #'type fvs))))))

(defun make-generic-var (type)
  (getrwff (concatenate 'string (princ-to-string (gensym)) 
			(core::type-to-string-2 type))))

(defun symbols-of-lits (lits)
  (delete-duplicates
   (apply #'nconc (mapcar #'(lambda (lit) (symbols (jform-represents lit) nil)) lits))))
  
(defun symbols-of (inwff) (delete-duplicates (symbols inwff nil)))

(defun symbols (gwff bind-list)
  (cond ((label-q gwff) (apply-label gwff (symbols gwff bind-list)))
	((lsymbol-q gwff)
	 (cond ((get gwff 'abbrev) (list gwff))
	       ((get gwff 'core::stands-for) (list gwff))
               (t (if (and ; (not (logconst-q gwff)) ; actually we want these if they occur inside a lit - 10/10/01
			   (propsym-q gwff)
		           (not (member gwff bind-list)))
	              (list gwff)))))
	((boundwff-q gwff) (symbols (cdr gwff) (cons (caar gwff) bind-list)))
	(t (nconc (symbols (car gwff) bind-list) 
		  (symbols (cdr gwff) bind-list)))))

(defun valid-proj-h-var (sub resulttype)
  ;we are one step from failing, and have not yet projected out an argument var.
  ;we check the h-vars to make sure that one of them *could* do this.
  (let ((hvarlist (remove-if-not #'ho-h-var-check (ho-free-vars-of sub))))
    (dolist (h hvarlist nil)
	    (when (and (equal (car (listify-vec (listify-type (type (car h))))) resulttype)
		       (member resulttype (cdr (listify-vec (listify-type (type (car h)))))))
		  (return t)))))

(defun ho-free-in-3 (var term)
  (cond ((symbolp term) (eq var term))
	((numberp (cdr term)) (eq var term))
	(t (or (ho-free-in-3 var (car term))
	       (ho-free-in-3 var (cdr term))))))

(defun ho-h-var-check (var)
  (when (consp var) (setq var (car var)))
  (let ((str (princ-to-string var))
	(val (elt (princ-to-string h-var-prefix) 0)))
    (and (eq (elt str 0) val)
	 (eq (elt str 1) #\^))))
    
(defun ccs-pfd-fo-unify-real (dpairs subst-stack)
  (if dpairs
      (ccs-pfd-fo-unify-rec-real
       (caaar dpairs) (cdaar dpairs) (cadar dpairs) (cddar dpairs) (cdr dpairs)
        subst-stack)
    (if (and (null skolem-default) (cyclic-selection-terms subst-stack))
	(values t nil)
      (values subst-stack t))))

(defun ccs-pfd-fo-unify-rec-real (term1 env1 term2 env2 dpairs subst-stack)
  (if (lsymbol-q term1)
      (let ((var (assoc term1 env1)))
	(if var
	    (let ((subst (cdr (assoc var subst-stack))))
	      (if subst
		  (ccs-pfd-fo-unify-rec-real (car subst) (cdr subst) term2 env2
				    dpairs subst-stack)
		  ;;VAR is a variable. Check if it occurs free in term2.
		  (if (pfd-occurs-in-term-p var term2 env2 subst-stack)
		      ;;Does TERM2 reduce to VAR?
		      (if (simplify-term-p var term2 env2 subst-stack)
			  (ccs-pfd-fo-unify-real dpairs subst-stack)
			  (values t nil))
		      (ccs-pfd-fo-unify-real
		       dpairs
		       (acons var (cons term2 env2) subst-stack)))))
	    ;;TERM1 is a constant.
	    (if (lsymbol-q term2)
		(let ((var (assoc term2 env2)))
		  (if var
		      (let ((subst (cdr (assoc var subst-stack))))
			(if subst
			    (ccs-pfd-fo-unify-rec-real (car subst) (cdr subst) term1
					      nil dpairs subst-stack)
			    ;;VAR is a variable.
			    (ccs-pfd-fo-unify-real
			     dpairs (acons var (cons term1 nil)
						 subst-stack))))
		      ;;TERM2 is a constant.
		      (if (eq term1 term2)
			  (ccs-pfd-fo-unify-real dpairs subst-stack)
			  nil)))
		nil)))
      (if (lsymbol-q term2)
	  (progn (psetq term1 term2 term2 term1 env1 env2 env2 env1)
		 (ccs-pfd-fo-unify-rec-real term1 env1 term2 env2 dpairs subst-stack))
	  (ccs-pfd-fo-unify-rec-real
	   (car term1) env1 (car term2) env2
	   (acons (cons (cdr term1) env1) (cons (cdr term2) env2) dpairs)
	   subst-stack))))

(defun ccs-initialize-conn (lit1 lit2 env)
  (if *ccs-rewrites* ;;then don't put a negation on either wff
      (cons (if (jform-pos lit1) (lnorm-q (ho-bind-vars (jform-represents lit1) env nil)) 
	      (lnorm-q (ho-bind-vars (cons 'not (jform-represents lit1)) env nil)))
	    (if (jform-pos lit2) (lnorm-q (ho-bind-vars (jform-represents lit2) env nil))
	      (lnorm-q (ho-bind-vars (cons 'not (jform-represents lit2)) env nil))))
    (multiple-value-bind (l1 l2 switch)
	(ccs-place-neg lit1 lit2 (mapcar #'car env))
      (if switch
	  (cons t 
		(cons (ho-unif-lnorm2 nil (ho-unif-lnorm nil (ho-bind-vars l1 env nil) nil nil nil) nil nil nil)
		      (ho-unif-lnorm2 nil (ho-unif-lnorm nil (ho-bind-vars l2 env nil) nil nil nil) nil nil nil)))
	(cons (ho-unif-lnorm2 nil (ho-unif-lnorm nil (ho-bind-vars l1 env nil) nil nil nil) nil nil nil)
	      (ho-unif-lnorm2 nil (ho-unif-lnorm nil (ho-bind-vars l2 env nil) nil nil nil) nil nil nil))))))


(defun ccs-place-neg (l1 l2 fvlist)
  (let ((n1 (not (jform-pos l1)))
	(n2 (not (jform-pos l2)))
	(wff1 (jform-represents l1))
	(wff2 (jform-represents l2)))
    (if (neq n1 n2) ;we have (A ~B)
	(values wff1 wff2 nil) ;so we unify <A B>
      (let ((r1 (or (ae-bd-wff-p wff1) ; quantifiers are rigid heads - cebrown 2/17/01
		    (not (memq (simpl-head wff1) fvlist))))
	    (r2 (or (ae-bd-wff-p wff2) ; quantifiers are rigid heads - cebrown 2/17/01
		    (not (memq (simpl-head wff2) fvlist)))))
	(if (and r1 r2 (not *ccs-rewrites*)) ;we have either (R1 R2) or (~R1 ~R2)
	    (values 'TRUTH 'FALSEHOOD nil) ;...which don't unify
	  (if r1 ;we have (R f) or (~R ~f)
	      (values (cons 'not wff1) wff2 nil) ;we return <~R f>
	    (if r2 ;we have (f R) or (~f ~R)
		(values wff1 (cons 'not wff2) nil) ;we return <f ~R>
	      ;otherwise we have (f1 f2) or (~f1 ~f2), and we want to allow both <f1 ~f2> and <~f1 f2>
	      (values wff1 wff2 t))))))))

(defun extract-switchable (node)
  (let ((dpair1 (ho-unif-lnorm2 nil (caar (unode-fo-mode node)) nil (unode-substs node) nil))
	(dpair2 (ho-unif-lnorm2 nil (cdar (unode-fo-mode node)) nil (unode-substs node) nil))
	new-dpair)
    (multiple-value-bind (rhead bdvars) (rigidp dpair1 nil)
			 (declare (ignore bdvars))
			 (when rhead (if (eq rhead 'not)
					 (setq new-dpair (cons (cdr dpair1) dpair2)) ;must be of type O
				       (setq new-dpair (cons (cons 'not dpair1) dpair2)))))
    (unless new-dpair (multiple-value-bind (rhead bdvars) (rigidp dpair2 nil)
					   (declare (ignore bdvars))
					   (when rhead (if (eq rhead 'not)
							   (setq new-dpair (cons (cdr dpair2) dpair1)) ;must be of type O
							 (setq new-dpair (cons (cons 'not dpair2) dpair1))))))
    (if new-dpair (progn (setf (unode-dpairs node) (cons new-dpair (unode-dpairs node)))
;			 (msgf "Oriented " new-dpair)
			 (pop (unode-fo-mode node))
			 (list node))
      (let ((node1 (copy-unode node))
	    (node2 (copy-unode node))
	    (msv1 (list (copy-alist (car (unode-msv node))) (copy-alist (cadr (unode-msv node)))))
	    (msv2 (list (copy-alist (car (unode-msv node))) (copy-alist (cadr (unode-msv node))))))
	(setf (unode-msv node1) msv1)
	(setf (unode-msv node2) msv2)
	(setf (unode-dpairs node1) (cons (cons (cons 'not dpair1) dpair2) (copy-list (unode-dpairs node1))))
	(setf (unode-dpairs node2) (cons (cons (cons 'not dpair2) dpair1) (copy-list (unode-dpairs node2))))
	(setf (unode-fo-mode node1) (copy-list (cdr (unode-fo-mode node1))))
	(setf (unode-fo-mode node2) (copy-list (cdr (unode-fo-mode node2))))
;	(msgf "Branched on " dpair1 " " dpair2)
	(list node1 node2)))))
	
(defun full-normalize-subs (substs)
  (let ((stack nil)
	(substs (reverse substs)))
    (dolist (subst substs)
	    (unless (= (cdar subst) -2)
		    (push (cons (car subst)
				(ho-unif-lnorm2 nil (cdr subst) nil substs nil))
			  stack)))
    (reverse stack)))

(defun full-normalize-fo (substs)
  (setq substs (mapcar #'strip-cdar-fn (nreverse substs)))
  (let ((stack nil))
    (dolist (subst substs)
	    (push (cons (car subst)
			(expand-subst-fo-2 (cadr subst) (cddr subst) substs))
		  stack))
    stack))

(defun strip-cdar-fn (elt) (cons (caar elt) (cdr elt)))

(defun expand-subst-fo-2 (term env substs)
  (if (lsymbol-q term)
      (let ((var (car (assoc term env))))
	(if var 
	    (let ((sub  (cdr (assoc var substs))))
	      (if sub 
		  (expand-subst-fo-2 (car sub) (cdr sub) substs)
		var))
            term))
      (cons (expand-subst-fo-2 (car term) env substs)
            (expand-subst-fo-2 (cdr term) env substs))))

(defun get-all-ccs-vars (&optional (xvars nil))
  (let ((vars (free-vars-in-etree current-eproof))
	retlist)
    (when *ccs-rewrites*
      (setq vars (remove-duplicates 
		  (append xvars
			  (intersection 
			   (reduce #'append (mapcar #'ccs-vars-fn-1 *live-leaves*))
			   vars)))))
    (dolist (v vars retlist)
	    (let* ((argtypes (cdr (reverse (listify-vec (listify-type (type v))))))
		   (arglist (mapcar #'ccs-vars-fn-2 argtypes)))
	      (push (reduce #'cons (cons (cons v -1) arglist)) retlist)))))

(defun ccs-vars-fn-1 (elt) (free-vars-of (get-shallow elt)))
(defun ccs-vars-fn-2 (elt) (getrwff (concatenate 'string (princ-to-string (gensym)) 
						 (core::type-to-string-2 elt))))
(defun satisfies-msv (wff)
  (if (and (listp wff) (listp (car wff)) (eq 'lambda (cdar wff)))
      (satisfies-msv (cdr wff))
    (let ((wfflist (wff-to-list wff)))
      (>= max-substs-var (- (length wfflist)
			    (* 2 (length (remove-if-not #'prune-lambda-fn wfflist))))))))

(defun prune-lambda-fn (elt) (eq elt 'lambda))
(defun uavp-fn-1 (x) (member (car (find-fhead x)) (mapcar #'car *leibniz-var-list*)))

(defun unify-all-var-pairs (&optional (xvars nil))
  (runcount 'unification)
  (let* ((vars (get-all-ccs-vars xvars))
	 (lvars (remove-if-not #'uavp-fn-1 vars))
	 (rvars (remove-if #'uavp-fn-1 vars))
	 (allb (reduce #'append (mapcar #'cdr *allb*))))
    (setq *sk-below-dups* (unless skolem-default (get-sk-below-dups))
	  *sk-below-primsubs* (unless skolem-default (get-sk-below-primsubs)))
    (setq *global-constlist* (parse-into-types (if ms98-unif-hack (cons 'NOT (find-consts))
						 (cons (getrwff "TRUTH") (cons 'NOT (find-consts))))))
    (setq *uavp-hash* (make-hash-table :test #'equal))
    (when lvars (setq unif-count (uavp lvars t unif-count allb)))
    (when rvars (setq *uavp-count* (uavp rvars nil unif-count allb)))
    (dolist (sk (reduce #'append (mapcar #'cdr (remove-if #'null *sk-below-dups* :key #'cdr))))
      (push (cons sk (sort (get-subs-involving sk) #'<)) *subs-involving*)))
  (breakcount 'unification))

(defun get-subs-involving (sk)
  (let (retlist)
    (maphash #'(lambda (key val) (when (memq sk (symbols-of val)) (push key retlist))) *ccs-substs*)
    retlist))

(defun squash-o-subterms (termlist)
  (let (retlist)
    (dolist (term termlist (if (memq 'TRUTH (assoc 'O *global-constlist*))
			       (mapcar #'ho-etanorm (reduce #'append retlist))
			     (mapcar #'ho-etanorm (remove-if #'contains-truth (reduce #'append retlist)))))
      (push (squash-o-terms term) retlist))))

(defun squash-o-terms (term &optional (vars nil))
  ;idea: subterms of type o containing no variables should be replaced by T or F.
  (if (and (eq (type term) 'O)
	   (no-vars-in term vars))
      (list 'TRUTH '(NOT . TRUTH))
    (cond ((lsymbol-q term) (list term))
	  ((boundwff-q term) 
	   (mapcar #'(lambda (x) (cons (car term) x)) (squash-o-terms (cdr term) (cons (caar term) vars))))
	  (t (let ((retlist1 (squash-o-terms (car term) vars))
		   (retlist2 (squash-o-terms (cdr term) vars))
		   retlist)
	       (dolist (r1 retlist1 retlist)
		 (dolist (r2 retlist2)
		   (push (cons r1 r2) retlist))))))))

(defun contains-truth (wff)
  (cond ((lsymbol-q wff) (eq wff 'TRUTH))
	((boundwff-q wff) (contains-truth (cdr wff)))
	(t (or (contains-truth (car wff)) (contains-truth (cdr wff))))))

(defun no-vars-in (term vars)
  (cond ((lsymbol-q term) (not (memq term vars)))
	((boundwff-q term) (no-vars-in (cdr term) (cons (caar term) vars)))
	(t (and (no-vars-in (car term) vars) (no-vars-in (cdr term) vars)))))

(defun etanorm-cdr-fn (elt) (ho-etanorm (cdr elt)))
(defun uavp-fn-2 (elt) (car (find-fhead elt)))
(defun uavp-fn-3 (elt) (type (uavp-fn-2 elt)))

(defun uavp (varlist leib count allb)
  (declare (special *full-unifhash* *ccs-substs*))
  (let* ((first (car varlist))
	 (first-tp (uavp-fn-3 first))
	 (skdup *sk-below-dups*)
	 (skprim *sk-below-primsubs*)
	 (unifs (remove-duplicates 
		 (remove-if-not #'satisfies-msv
				(if ms98-unif-hack2
				    (squash-o-subterms
				     (mapcar #'etanorm-cdr-fn
					     (unify-var-pair-real first leib)))
				  (mapcar #'etanorm-cdr-fn
					  (unify-var-pair-real first leib))))
		 :test #'wffeq-ab))
	 (others (remove-if #'(lambda (x) (equal (uavp-fn-3 x) first-tp)) varlist))
	 (together (mapcar #'uavp-fn-2
			   (remove-if-not #'(lambda (x) (equal (uavp-fn-3 x) first-tp)) varlist)))
	 (temp-count count))
    (when (and (remove-if #'null (mapcar #'cdr skdup)) skdup ms98-num-of-dups) 
      (setq unifs (prune-unifs unifs (remove-if #'null (mapcar #'cdr skdup)) ms98-num-of-dups)))
    (when (and (remove-if #'null (mapcar #'cdr skprim)) ms98-max-prims)
      (setq unifs (prune-unifs unifs (remove-if #'null (mapcar #'cdr skprim)) ms98-max-prims)))
    ;further pruning is possible by banning more unifiers for those subs having 
    (when ms98-verbose (dolist (s unifs)
			      (msgf (incf temp-count) " --> " (s . gwff))))
    (setq unifs (mapcar #'(lambda (x) (incf count) (cons count x)) unifs))
    (setf (gethash (cons (type (car (find-fhead first))) leib) *full-unifhash*) (mapcar #'car unifs))
    (dolist (u unifs)
	    (setf (gethash (car u) *ccs-substs*) (cdr u))
	    (when *skolem-matters* (setf (gethash (car u) *allsub-hash*) (remove-duplicates (all-occurs-in (cdr u) allb)))))
    (unless skolem-default
	    (dolist (elt together)
		    (let ((banned (exp-var-selected 
				   (car (remove-if-not #'(lambda (x) (eq (exp-var-var x) elt))
						       (mapcar #'car (eproof-free-vars-in-etree current-eproof)))))))
		      (push (cons elt 
				  (sort (mapcar #'car 
						(remove-if #'(lambda (x) (occurs-in (cdr x) banned)) unifs))
					#'<))
			    *selection-banned*)))
	    (dolist (elt together)
	      (let* ((dup (remove-if #'(lambda (x) (memq elt (car x))) skdup))
		     (prim (remove-if #'(lambda (x) (memq elt (car x))) skprim))
		     (dup2 (remove-if #'null dup :key #'cdr))
		     (prim2 (remove-if #'null prim :key #'cdr))
		     newbanned)
		(when (and ms98-num-of-dups (< (length dup) (length skdup))) ;then this is a duped var
		  (dolist (u unifs)
		    (unless (prune-unifs (list (cdr u)) dup2 (1- ms98-num-of-dups))
		      (push (car u) newbanned))))
		(when (and ms98-max-prims (< (length prim) (length skprim))) ;then this is a primvar
		  (dolist (u unifs)
		    (unless (prune-unifs (list (cdr u)) prim2 (1- ms98-max-prims))
		      (push (car u) newbanned))))
		(rplacd (assoc elt *selection-banned*)
			(sort (setdiff (cdr (assoc elt *selection-banned*)) newbanned) #'<))))
	    (when (= (length together) 1) ;there is only one var of this type
	      (setf (gethash (cons (type (car (find-fhead first))) leib) *full-unifhash*)
		    (cdr (assoc (car together) *selection-banned*))) ;;just the subs in *selection-banned*
	      (setq *selection-banned*
		    (remove-if #'(lambda (x) (eq (car x) (car together))) *selection-banned*)))
	    (when (and ms98-verbose (not others))
		  (msgf "Allowed substitutions: " (remove-if-not #'cdr *selection-banned*))))
    (msgf "There are " (length (gethash (cons (type (car (find-fhead first))) leib) *full-unifhash*))
			       " unifiers for each of " together ":" t)
    ;unifhash gets keys of the form (type . leib) for entries of the form (list of integers)
    ;ccs-substs gets integer keys for entries which are substitutions
    (when ms98-verbose (msg t t))
    (if others 
	(uavp others leib count allb)
      count)))

(defun unify-var-pair-real (var leib)
  (let ((root (make-qnode :dpairs (list (cons var var)) :other 0))
	(real-var (find-fhead var)))
    (clrhash *uavp-hash*)
    (do ((ignore-nodes nil)
	 (nodes nil (cdr nodes))
	 (son root (car nodes))
	 (simpl nil))
	((null son)
	 (if (null ignore-nodes) nil
	   (reduce #'append (mapcar #'qnode-substs ignore-nodes))))
	(setq simpl (ho-simpl-uavp son))
	(if (and simpl (or (not leib) (< (qnode-other son) (1- max-substs-var)) (ho-leibniz-vcheck son real-var)))
	    (if (zerop simpl)
		(if (null (qnode-dpairs son))
		    (progn
		      (setf (qnode-substs son)
			    (list (assoc real-var (full-normalize-subs (qnode-substs son))
					 :test #'equal)))
		      (when (or (not leib) (ho-leibniz-vcheck2 son))
			(push son ignore-nodes)))
		  (setq nodes (nconc (ccs-uavp-match-ff son) nodes)))
	      (setq nodes (nconc (ccs-uavp-match son) nodes)))
	  ))))

(defun projectible-type-check (list tp)
  (if (null list) t
    (if (equal (type (caar list)) tp) (projectible-type-check (cdr list) tp)
      nil)))

(defun plug-h-var-if-possible (node)
  (let* ((h-var (find-fhead (caar (qnode-dpairs node))))
	 (ok (ho-h-var-check h-var))
	 (lambdas (cdar (and ok (ccs-match-i nil nil h-var))))
	 subs sub)
    (when (and lambdas (projectible-type-check lambdas (get-resulttype h-var)))
      (dolist (l lambdas)
	(setq sub (copy-list (mapcar #'copy-list lambdas)))
	(rplacd (last sub) (car l))
	(push (cons h-var sub) subs))
      (create-successors-uavp subs node))))

(defun ccs-uavp-match-ff (unode)
  (if (< (qnode-other unode) max-substs-var)
      (let* ((dpair (choose-dpair (qnode-dpairs unode)))
	     (fhead1 (find-fhead (car dpair)))
	     (bdvars (ccs-get-binder (car dpair)))
	     (substitutions (gethash (cons (type (car fhead1)) (mapcar #'type bdvars)) *uavp-hash*)))
	(if substitutions
	    (setq substitutions 
		  (mapcar #'(lambda (x) (cons fhead1 (rename-h-vars-in x))) substitutions))
	  (progn (dolist (symbol (cdr (assoc (get-resulttype fhead1) *global-constlist*)))
		   (setq substitutions (nconc (ccs-match-i symbol bdvars fhead1) substitutions)))
		 (setq substitutions (nconc (ccs-match-p fhead1) substitutions))
		 (setf (gethash (cons (type (car fhead1)) (mapcar #'type bdvars)) *uavp-hash*)
		       (mapcar #'cdr substitutions))))
	(create-successors-uavp substitutions unode))
    (plug-h-var-if-possible unode)))

(defun ho-etanorm (gwff)
  (cond ((lsymbol-q gwff) gwff)
	((boundwff-q gwff)
	 (let ((contrform (ho-etanorm (gdr gwff))))
	   (if (eq (binding gwff) 'lambda)
	       (if (and (ho-eta-applic-p contrform)
			(equal (bindvar gwff) (gdr contrform))
			(not (ho-free-in-2 (bindvar gwff) (gar contrform))))
		   (gar contrform) (cons (bindhead gwff) contrform))
	       (cons (bindhead gwff) contrform))))
	(t (if (numberp (cdr gwff)) gwff
	     (cons (ho-etanorm (gar gwff)) (ho-etanorm (gdr gwff)))))))

(defun ho-eta-applic-p (gwff)
  (and (consp gwff)
       (not (numberp (cdr gwff)))
       (not (boundwff-q gwff))))

(defun rename-h-vars-in (gwff)
  (if (symbolp gwff)
      gwff
    (if (and (consp gwff) (eq (cdr gwff) -2))
	(cons (ren-var-uni-ho h-var-prefix (type (car gwff))) -2)
      (cons (rename-h-vars-in (car gwff)) (rename-h-vars-in (cdr gwff))))))

(defun ccs-uavp-match (unode)
  (if (< (qnode-other unode) max-substs-var)
      (let ((substitutions nil)
	    (min-subst-count most-positive-fixnum)
	    (min-dpair nil))
	(dolist (dpair (qnode-dpairs unode))
	  (multiple-value-bind (rhead bdvars) (rigidp (car dpair) nil)
	    (when rhead
	      (let* ((fhead (find-fhead (cdr dpair)))
		     (sub-count (ho-match-top-count rhead bdvars fhead)))
		(if (zerop sub-count)
		    (return-from ccs-uavp-match nil)
		  (if (= sub-count 1)
		      (progn (setq min-subst-count 1 min-dpair dpair)
			     (return t))
		    (if (< sub-count min-subst-count)
			(setq min-subst-count sub-count min-dpair dpair)
		      )))))))
	(when min-dpair
	  (multiple-value-bind (rhead bdvars)
	      (rigidp (car min-dpair) nil)
	    (setq substitutions (ho-match-top rhead bdvars 
					      (find-fhead (cdr min-dpair))))
	    (create-successors-uavp substitutions unode))))
    (plug-h-var-if-possible unode)))

(defun create-successors-uavp (substs node)
  (when substs
	(mapcar #'(lambda (subst)
		    (let ((dpairs (copy-list (qnode-dpairs node)))
			  (son (copy-qnode node)))
		      (setf (qnode-substs son) (cons subst (qnode-substs son)))
		      (setf (qnode-dpairs son) dpairs)
		      (unless (projection-of-a-w (cdr subst)) (incf (qnode-other son)))
		      son))
		substs)))

(defun projection-of-a-w (subst &optional (lambdas nil))
  (if (and (consp subst) (consp (car subst)))
      (if (eq (cdar subst) 'lambda)
	  (projection-of-a-w (cdr subst) (cons (caar subst) lambdas))
	nil)
    (if (or (consp subst) (null subst)) nil
      (member subst lambdas))))

(defun ho-simpl-uavp (node)
  (do ((dpairs (copy-list (qnode-dpairs node)) (cdr dpairs))
       (subst-stack (qnode-substs node))
       (frpairs nil)
       (ffpairs nil)
       first second)
      ((null dpairs)
       (prog1 (if frpairs 1 0)
         (setf (qnode-dpairs node) (make-new-dpairs (nconc frpairs ffpairs) (qnode-substs node)))))
      (setq first (ho-unif-lnorm2 nil (caar dpairs) nil subst-stack nil))
      (setq second (ho-unif-lnorm2 nil (cdar dpairs) nil subst-stack nil))
      (let ((rh1 (symbolp (simpl-head first)))
	    (rh2 (symbolp (simpl-head second))))
	(if rh1
	    (if rh2
		(multiple-value-bind (fail new-dpairs)
				     (stepa-simpl first second )
				     (if fail (return-from ho-simpl-uavp nil)
				       (setq dpairs (nconc dpairs new-dpairs))))
	      (push (cons first second) frpairs))
	  (if rh2
	      (push (cons second first) frpairs)
	    (push (cons first second) ffpairs))))))

(defun ho-leibniz-vcheck2 (node)
  (let ((sub (cdar (qnode-substs node))))
    (and (boundwff-q sub)
	 (lambda-bd-p sub)
	 (ho-free-in-3 (caar sub) (cdr sub)))))

(defun ho-leibniz-vcheck (node var)
  (let* ((lv (qnode-other node))
	 (sub (cdr (assoc var (full-normalize-subs (qnode-substs node)) :test #'equal)))
	 (rv t))
    (when (and sub
	       (>= lv (1- max-substs-var))
	       (boundwff-q sub)
	       (eq (cdar sub) 'lambda)
	       (not (memq (caar sub) (free-vars-of (process-further (cdr sub))))))
	  (if (= lv max-substs-var)
	      (progn ;(msgf "Rejected " var " --> " sub " because of L1.")
		     (setq rv nil))
	    (if (not (valid-proj-h-var (cdr sub) 
				       (car (listify-vec (listify-type (type (caar sub)))))))
		(progn ;(msgf "Rejected " var " --> " sub " because of L2.")
		       (setq rv nil)))))
    rv))

(defun create-successor-quick (subst dpairs substs)
  (unless (eq dpairs 'fail)
	  (make-qnode :dpairs dpairs :substs (cons subst substs))))

(defun create-successor-full (subst dpairs node)
  (unless (eq dpairs 'fail)
    (make-qnode :dpairs dpairs :substs (append subst (qnode-substs node)))))

(defun order-wrt-vars (list)
  (sort list #'order-var-fn-1))

(defun order-var-fn-1 (x y) (member (car y) (cdr (member (car x) *ordered-vars*))))

(defun simpl-full (dplist subst &optional(str nil) (flaglist nil))
  (when (null flaglist) (setq flaglist dplist))
  (do ((dpairs dplist (cdr dpairs))
       (flags flaglist (cdr flags))
       (frpairs nil)
       (skipped nil)
       (ffpairs nil)
       first second)
      ((null dpairs)
       (when str (when (or ffpairs skipped) ;(unless (or frpairs (null ffpairs)) 
		   (push 'delay frpairs)))
       (nconc frpairs skipped ffpairs))
    (if (or (caar flags) (cdar flags))
	(progn
	  (setq first (if (caar flags) (ho-unif-lnorm2 nil (caar dpairs) nil subst nil) (caar dpairs)))
	  (setq second (if (cdar flags) (ho-unif-lnorm2 nil (cdar dpairs) nil subst nil) (cdar dpairs)))
	  (unless (wffeq first second)
	    (if (symbolp (simpl-head first))
		(if (symbolp (simpl-head second))
		    (multiple-value-bind (fail new-dpairs)
			(stepa-simpl-full first second)
		      (if fail (return-from simpl-full 'fail)
			(setq dpairs (append dpairs new-dpairs)
			      flags (append flags new-dpairs))))
		  (push (cons first second) frpairs))
	      (if (symbolp (simpl-head second))
		  (push (cons second first) frpairs)
		(push (cons first second) ffpairs)))))
      (push (car dpairs) skipped))))

(defun stepa-simpl-full (term1 term2)
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
				 (return-from stepa-simpl-full t))))
		 (unless (eq head1 head2)
			 (return-from stepa-simpl-full t)))
	   (values nil (mapcar #'(lambda (elt1 elt2)
				   (prefix-l-both elt1 elt2 binder))
			       args1 args2))))))

(defun extract-switchable-full (node)
  (let ((dpair1 (ho-unif-lnorm2 nil (caar (unode-fo-mode node)) nil (unode-substs node) nil))
	(dpair2 (ho-unif-lnorm2 nil (cdar (unode-fo-mode node)) nil (unode-substs node) nil))
	new-dpair)
    (multiple-value-bind (rhead bdvars) (rigidp dpair1 nil)
			 (declare (ignore bdvars))
			 (when rhead (if (eq rhead 'not)
					 (setq new-dpair (cons (cdr dpair1) dpair2)) ;must be of type O
				       (setq new-dpair (cons (cons 'not dpair1) dpair2)))))
    (unless new-dpair (multiple-value-bind (rhead bdvars) (rigidp dpair2 nil)
					   (declare (ignore bdvars))
					   (when rhead (if (eq rhead 'not)
							   (setq new-dpair (cons (cdr dpair2) dpair1)) ;must be of type O
							 (setq new-dpair (cons (cons 'not dpair2) dpair1))))))
    (if new-dpair (progn (setf (unode-dpairs node) (simpl-full (cons new-dpair (unode-dpairs node))  (unode-substs node)))
;			 (msgf "Oriented " new-dpair)
			 (pop (unode-fo-mode node))
			 (if (eq (unode-dpairs node) 'fail) nil (list node)))
      (let ((node1 (copy-unode node))
	    (node2 (copy-unode node))
	    (substs1 (copy-list (unode-substs node)))
	    (substs2 (copy-list (unode-substs node))))
	(setf (unode-substs node1) substs1)
	(setf (unode-substs node2) substs2)
	(setf (unode-dpairs node1) (simpl-full (list (cons (cons 'not dpair1) dpair2)) (unode-substs node1)))
	(setf (unode-dpairs node2) (simpl-full (list (cons (cons 'not dpair2) dpair1)) (unode-substs node2)))
	(setf (unode-fo-mode node1) (copy-list (cdr (unode-fo-mode node1))))
	(setf (unode-fo-mode node2) (copy-list (cdr (unode-fo-mode node2))))
;	(msgf "Branched on " dpair1 " " dpair2)
	(remove-if #'extract-full-fn-1 (list node1 node2))))))
	
(defun extract-full-fn-1 (x) (eq (unode-dpairs x) 'fail))

(defun merge-utrees (comp1 comp2 mating key litkey &optional (ban-me nil) (touches nil))
  (declare (special *no-vars*))
  (if *no-vars*	
      (if (= key -1)
	  (progn
	    (when (and (member 'MATING ms98-trace)
		       (component-good comp1)
		       (component-good comp2))
	      (msgf "FAILED TO COMBINE GOOD COMPONENTS " (component-name comp1) " and " (component-name comp2))
	      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		(stringdtl)
		(msgf "FAILED TO COMBINE GOOD COMPONENTS " (component-name comp1) " and " (component-name comp2))))
	    t)
	(construct-component nil mating key litkey touches))
    (progn
      (runcount 'unification)
      (let ((newsubs (merge-subs (component-final-subs comp2)
				 (component-final-subs comp1) key touches nil ban-me)))
	(if (= key -1) (progn (breakcount 'unification)
			      (when (and (member 'MATING ms98-trace)
					 (component-good comp1)
					 (component-good comp2))
				(msgf "FAILED TO COMBINE GOOD COMPONENTS " (component-name comp1) " and " (component-name comp2))
				(with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
				  (stringdtl)
				  (msgf "FAILED TO COMBINE GOOD COMPONENTS " (component-name comp1) " and " (component-name comp2))))
			      newsubs)
	  (prog1 (if newsubs
		     (construct-component newsubs mating key litkey touches)
		   (progn
		     (when (and (member 'MATING ms98-trace)
				(component-good comp1)
				(component-good comp2))
		       (msgf "FAILED TO COMBINE GOOD COMPONENTS " (component-name comp1) " and " (component-name comp2))
		       (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
			 (stringdtl)
			 (msgf "FAILED TO COMBINE GOOD COMPONENTS " (component-name comp1) " and " (component-name comp2))))
		     nil))
	    (breakcount 'unification)))))))
	    
(defun merge-subs (sublist1 sublist2 key &optional (touches nil) (rct t) (ban-me nil))
  (if (or (eq sublist1 'fail) (eq sublist2 'fail)) nil
    (progn 
      (when rct (runcount 'unification))
      (let ((newsubs (if (= key -1) (dag-will-merge sublist1 sublist2)
		       (let ((result (merge-dags sublist1 sublist2 touches ban-me)))
			 (if (eq result 'fail) nil result)))))
	(when rct (breakcount 'unification))
	newsubs))))

(defun merge-subs-dummy (sublist1 sublist2)
  (dolist (s sublist1 nil)
	  (when (remove-if #'(lambda (x) (cannot-accept s x)) sublist2)
		(return t))))

(defun get-fail-subs (sublist1 sublist2)
  (let (failvlist)
    (dolist (s sublist1 failvlist)
	    (dolist (r sublist2)
		    (dolist (f (get-fails-from r s))
			    (pushnew f failvlist))))))

(defun cannot-accept (sub1 sub2)
  (when sub1
	(if (equal (or (car sub1) (car sub2)) (or (car sub2) (car sub1)))
	    ;(or nil nil) = nil ; (or nil x) = (or x nil) ; (or x y) <> (or y x)
	    (cannot-accept (cdr sub1) (cdr sub2))
	  t)))

(defun merge-ccs-lists2 (sub1 sub2)
  (when sub1
	(cons (or (car sub1) (car sub2)) (merge-ccs-lists2 (cdr sub1) (cdr sub2)))))

(defun get-fails-from (sub1 sub2 &optional (v *ordered-vars*))
  (when sub1
	(if (equal (or (car sub1) (car sub2)) (or (car sub2) (car sub1)))
	    (get-fails-from (cdr sub1) (cdr sub2) (cdr v))
	  (cons (car v) (get-fails-from (cdr sub1) (cdr sub2) (cdr v))))))

(defun merge-ccs-lists (a b &optional (v *ordered-vars*))
  (if a ; a and b are always the same length
      (if (equal (car a) (car b))
	  (cons (car a) (merge-ccs-lists (cdr a) (cdr b) (cdr v)))
	(if (and (car a) (car b))
	    (cons (car v) (merge-ccs-lists (cdr a) (cdr b) (cdr v)))
	  (cons (or (car a) (car b)) (merge-ccs-lists (cdr a) (cdr b) (cdr v)))))
    nil))
      
		      
(defun ccs-subsumed (comp1 comp2)
  ;every unifier of comp1 is also a unifier of comp2
  (let ((varlist1 (remove-duplicates (mapcar #'car (reduce #'append (component-final-subs comp1)))))
	(varlist2 (remove-duplicates (mapcar #'car (reduce #'append (component-final-subs comp2))))))
    (if (not (subsetp varlist2 varlist1 :test #'equal))
	nil
      (dolist (c (component-final-subs comp1) t)
	      (unless (find-same-sub-in c (component-final-subs comp2))
		      (return nil))))))

(defun find-same-sub-in (sub sublist)
  (if (null sublist) nil
    (if (set-difference (car sublist) sub :test #'equal)
	(find-same-sub-in sub (cdr sublist))
      t)))

(defun get-final-fn-1 (x) (eq (component-final-subs x) 'delay))

(defun get-final-subs-for (comp1 ccs-original touches)
  (declare (ignore touches))
  (unless (quick-check-subs (component-clist comp1))
    (setf (component-final-subs comp1) 'fail))
  (if (equal (component-final-subs comp1) 'delay)
      (let* ((sublist (remove-if-not #'(lambda (x) (member (car (component-clist x)) (component-clist comp1)
							   :test #'equal))
				     ccs-original))
	     (sublist1 (remove-if-not #'get-final-fn-1 sublist))
	     (sublist2 (remove-if #'get-final-fn-1 sublist))
	     (fail-right-now (dolist (s sublist2 nil)
			       (when (eq (component-final-subs s) 'fail) (return t))))
	     temp-touches newsub)
	(if fail-right-now (progn (setf (component-final-subs comp1) 'fail) nil)
	  (progn 
	    (unless sublist2 ;if they're all marked 'delay
	      (setq *allowed-subs-list* nil)
	      (setf (component-final-subs (car sublist1)) (qum-delay (car (component-clist (car sublist1)))
								     (component-touches (car sublist1))))
	      (when (eq 'fail (component-final-subs (car sublist1)))
		(setf (component-final-subs comp1) 'fail)
		(return-from get-final-subs-for nil))
	      (push (pop sublist1) sublist2))
					;now we know sublist2 has at least one element
	    (setq temp-touches (component-touches (car sublist2)))
	    (setq newsub (component-final-subs (pop sublist2)))
	    (runcount 'unification)
	    (dolist (c (sort sublist2 #'< :key #'(lambda (x) (cadr (dnode-keys (component-final-subs x)))))
		       (progn (dolist (d sublist1
					 (progn (breakcount 'unification)
						(setf (component-final-subs comp1) newsub) newsub))
				(collapse-dag newsub)
				(setq temp-touches (ordered-union (component-touches d) temp-touches))
				(setq newsub (merge-subs newsub 
							 (qum-delay (car (component-clist d)) (component-touches d))
							 0 temp-touches nil nil
							 ))
				(unless newsub (setf (component-final-subs comp1) 'fail) 
					(breakcount 'unification) (return nil)))))
	      (setq temp-touches (ordered-union (component-touches c) temp-touches))
	      (setq newsub (merge-subs newsub 
				       (component-final-subs c) 
				       0 temp-touches nil nil))
	      (unless newsub (setf (component-final-subs comp1) 'fail) (breakcount 'unification) (return nil))))))
    (progn 
      (when (and (listp (component-final-subs comp1)) (dnode-p (car (component-final-subs comp1))))
	(setf (component-final-subs comp1) (fill-in-delayed-subs-2 (component-final-subs comp1) (component-clist comp1))))
      (if (equal (component-final-subs comp1) 'fail)
	  nil
	(component-final-subs comp1)))))

(defun get-final-subs-make-comp (comp1 ccs-original previous-subs previous-clist previous-touched mating newkey newlitkey touches &optional (ban-me nil))
  ;;previous-subs is guaranteed not to be 'delay !!
  (when (or (eq previous-subs 'fail) (null (quick-check-subs mating)))
    (return-from get-final-subs-make-comp nil))
  (let* ((newsub previous-subs)
	 (newconns (remove-if #'(lambda (x) (member x previous-clist :test #'equal)) (component-clist comp1)))
	 (sublist (remove-if-not #'(lambda (x) (member (car (component-clist x)) newconns
						       :test #'equal))
				 ccs-original))
	 (sublist1 (remove-if-not #'get-final-fn-1 sublist))
	 (sublist2 (remove-if #'get-final-fn-1 sublist))
	 (fail-right-now (dolist (s sublist2 nil)
			   (when (eq (component-final-subs s) 'fail) (return t))))
	 (temp-touches previous-touched))
    (if fail-right-now nil
      (progn 
	(runcount 'unification)
	(dolist (c (sort sublist2 #'< :key #'(lambda (x) (cadr (dnode-keys (component-final-subs x)))))
		   (dolist (d sublist1
			      (prog1 (construct-component newsub mating newkey newlitkey touches)
				     (breakcount 'unification)))
		     (collapse-dag newsub)
		     (setq temp-touches (ordered-union (component-touches d) temp-touches))
		     (setq newsub (merge-subs newsub 
					      (qum-delay (car (component-clist d)) (component-touches d))
					      0 temp-touches nil ban-me))
		     (unless newsub 
		       (breakcount 'unification) (return nil))))
	  (setq temp-touches (ordered-union (component-touches c) temp-touches))
	  (setq newsub (merge-subs newsub 
				   (component-final-subs c) 
				   0 temp-touches nil ban-me))
	  (unless newsub (breakcount 'unification) (return nil)))))))

(defun same-sub (sub1 sub2)
  (if (null sub1) t
      (and (equal (car sub1) (car sub2)) (same-sub (cdr sub1) (cdr sub2)))))



(defun qum-delay (conn touches)
  (runcount 'unification)
  (let ((return-value
	 (or (qum-delay-real (copy-list (gethash conn dphash)) touches conn) 'fail)))
    (breakcount 'unification)
    return-value))

(defun qum-delay-real (dpairs touches &optional (conn nil))
  (if (eq (car dpairs) t)
      (let* ((d1 (simpl-full (list (cons (cons 'not (cadr dpairs)) (cddr dpairs))) nil))
	     (d2 (simpl-full (list (cons (cons 'not (cddr dpairs)) (cadr dpairs))) nil))
	     (nodes (mapcar #'qum-real-fn-1
			    (remove-if #'qum-real-fn-2 (list d1 d2)))))
	(when nodes (unify-unifhash-delay nodes conn touches)))
    (let* ((d1 (simpl-full (list dpairs) nil))
	   (nodes (unless (eq d1 'fail)
			  (list (make-qnode :dpairs d1)))))
      (when nodes 
	    (unify-unifhash-delay nodes conn touches)))))

(defun get-good-dpair (dpairs banned)
  (let ((fhead (car (find-fhead (cdar dpairs))))
	(limited *allowed-subs-list*)
	sub)
    (if (and (numberp (cddar dpairs)) ;i.e. flex is just a variable
	     (or (and (consp (caar dpairs))
		      (numberp (cdaar dpairs))) ;i.e. we have (var1 . var2)
		 (not (has-free-vars (caar dpairs))))) ;i.e. we have (var .term) where term contains no vars
	(if (and (consp (caar dpairs))
		 (numberp (cdaar dpairs))) ;if it's (var1 . var2), make the same sub for both
	    (values fhead (caaar dpairs) (get-correct-subs fhead (caaar dpairs) banned))
	  (progn (setq sub (remove-if-not #'(lambda (x) (wffeq-ab (gethash x *ccs-substs*) (caar dpairs)))
					  (get-correct-subs fhead nil banned)))
		 (values fhead nil sub)))
      (dolist (d (cdr dpairs) (values fhead nil (get-correct-subs fhead nil banned)))
	      (when (and (numberp (cddr d))
			 (or (and (consp (car d))
				  (numberp (cdar d)))
			     (not (has-free-vars (car d)))))
		    (setq fhead (cadr d))
		    (if (and (consp (car d))
			     (numberp (cdar d))) ;if it's (var1 . var2), make the same sub for both
			(return (values fhead (caar d)
					(get-correct-subs fhead (caar d) banned)))
		      (progn (setq sub (remove-if-not #'(lambda (x) (wffeq-ab (gethash x *ccs-substs*) (car d)))
						      (get-correct-subs fhead nil banned)))
			     (return (values fhead nil sub)))))
	      (when limited
		(if (assoc (car (find-fhead (cdar dpairs))) *allowed-subs-list*)
		    (setq fhead (car (find-fhead (cdar dpairs)))
			  limited nil)
		  (when (and (find-fhead2 (caar dpairs))
			     (assoc (car (find-fhead2 (caar dpairs))) *allowed-subs-list*))
		    (setq fhead (car (find-fhead2 (caar dpairs)))
			  limited nil)
		    )))))))

(defun get-correct-subs (fhead1 &optional (fhead2 nil) (banned nil))
  ;;find all the subs that can be applied to both fhead1 and fhead2 (if specified)
  ;;fheads 1 and 2 are guaranteed to be the same type if both are given.
  (let ((val (or (cdr (assoc fhead1 *selection-banned*)) ;if this is NIL then all subs are allowed
		 (gethash (cons (type fhead1) 
				(if (or (assoc fhead1 *leibniz-var-list* :test #'equal)
					(and fhead2 (assoc fhead2 *leibniz-var-list* :test #'equal)))
				    t nil))
			  *full-unifhash*)))
	(dummy (when fhead2 (cdr (assoc fhead2 *selection-banned*))))
	(limit1 (cdr (assoc fhead1 *allowed-subs-list*)))
	(limit2 (when fhead2 (cdr (assoc fhead2 *allowed-subs-list*)))))
    (when dummy (setq val (ordered-intersection val dummy)))
    (when limit1 (setq val (ordered-intersection limit1 val)))
    (when limit2 (setq val (ordered-intersection limit2 val)))
    (when banned (setq val (ordered-setdiff val banned)))
    val))

      
(defun has-free-vars (term)
  (cond ((symbolp term) nil)
	((numberp (cdr term)) t)
	(t (or (has-free-vars (car term))
	       (has-free-vars (cdr term))))))


(defun ccs-match-full-unif (qnode banned)
  (let (real-val newdpairs)
    (multiple-value-bind (fhead1 fhead2 val) ;real-val & newdpairs are always NIL
	(get-good-dpair (qnode-dpairs qnode) banned)
;      (msgf fhead1 " " fhead2 " " (length val))
      (if fhead2
	  (let ((flaglist (get-flags-for (qnode-dpairs qnode) (list fhead1 fhead2))))
	    (dolist (v val real-val)
	      (setq newdpairs (simpl-full (qnode-dpairs qnode) (list (cons fhead1 v) (cons fhead2 v)) nil flaglist))
	      (when (neq newdpairs 'fail) 
		(push (create-successor-full (list (cons fhead1 v) (cons fhead2 v)) newdpairs qnode)
		      real-val))))
	(let ((flaglist (get-flags-for (qnode-dpairs qnode) (list fhead1))))
	  (dolist (v val real-val)
	    (setq newdpairs (simpl-full (qnode-dpairs qnode) (list (cons fhead1 v)) nil flaglist))
	    (when (neq newdpairs 'fail) 
	      (push (create-successor-full (list (cons fhead1 v)) newdpairs qnode) real-val))))))))

(defun check-helper-fn-1 (elt) (gethash (cdr elt) *allsub-hash*))

(defun check-for-primsubs (subs untouched-prims prims consts)
  (let ((consts-in-subs (remove-duplicates 
			 (reduce #'append (mapcar #'check-helper-fn-1 subs)))))
    (do ((skprim consts (cdr skprim))
	 (primsleft untouched-prims (cdr primsleft)))
	((or (null skprim) (> prims ms98-max-prims)) 
	 (if (not (> prims ms98-max-prims)) t (progn (when ms98-verbose (msg "P")) nil)))
      (unless (or (null (car primsleft)) (null (cdar skprim))) ;touched this, or has no consts
	(when (intersection (cdar skprim) consts-in-subs) (incf prims))))))

(defun check-for-primsubs-2 (subs lookup)
  (check-for-primsubs subs (cdr lookup) (car lookup) *sk-below-primsubs*))

(defun unify-unifhash (nodes mating key litkey touched)
  (when ms98-verbose
    (msgf "UNIFY-UNIFHASH initial num of nodes: " (length nodes)))
  (setq *allowed-subs-list* nil)
  (let* ((untouched-prims (or (null *primsubs*)
			      (mapcar #'(lambda (x) (if (intersection x touched) nil x)) *primsubs*)))
	 (prims (or (null *primsubs*)
		    (- (length *prim-lookup*) (length (remove-if #'null untouched-prims)))))
	 (consts (or (null *primsubs*) *sk-below-primsubs*))
	 (touches (find-touches (list (caar mating) (cdar mating))))
	 (banned (when (and ms98-num-of-dups (= (ccs-count-dups-in touches) ms98-num-of-dups))
		   (ccs-banned-sk-consts touches)))
	 ignore-nodes)
    (do ((son (pop nodes) (pop nodes)))
	((null son)
	 (if (null ignore-nodes) nil
	   (if (= key -1) t
	     (construct-component (dagify-nodes (mapcar #'qnode-substs ignore-nodes)) mating key litkey touched))))
        (if (null (qnode-dpairs son))
	    (when (or (null *primsubs*) 
		      (check-for-primsubs (qnode-substs son) untouched-prims prims consts))
	      (push son ignore-nodes))
	  (setq nodes (nconc (ccs-match-full-unif son banned) nodes))))))

(defun unify-unifhash-delay (nodes conn touches)
  (declare (ignore conn))
  (let* ((untouched-prims (or (null *primsubs*)
			      (mapcar #'(lambda (x) (if (intersection x touches) nil x)) *prim-lookup*)))
	 (prims (or (null *primsubs*)
		    (- (length *prim-lookup*) (length (remove-if #'null untouched-prims)))))
	 (banned (when (and ms98-num-of-dups (= (ccs-count-dups-in touches) ms98-num-of-dups))
		   (ccs-banned-sk-consts touches)))
	 (consts (or (null *primsubs*) *sk-below-primsubs*))
	 ignore-nodes)
    (do ((son (pop nodes) (pop nodes)))
	((null son)
	 (if (null ignore-nodes) nil
	   (dagify-nodes (mapcar #'qnode-substs ignore-nodes))))
      (if (null (qnode-dpairs son))
	  (when (or (null *primsubs*) 
		    (check-for-primsubs (qnode-substs son) untouched-prims prims consts))
	    (push son ignore-nodes))
	(setq nodes (nconc (ccs-match-full-unif son banned) nodes))))))

(defun get-flags-for (dpairs varlist)
  (let (retlist) 
    (dolist (d dpairs (nreverse retlist))
      (push (cons (if (intersection (mapcar #'get-flags-fn-1
					    (ho-free-vars-of (car d))) varlist) t nil)
		  (if (intersection (mapcar #'get-flags-fn-1
					    (ho-free-vars-of (cdr d))) varlist) t nil))
	    retlist))))

(defun get-flags-fn-1 (x) (if (consp x) (car x) x))

(defun find-fhead2 (term)
  (when (consp term)
	(if (numberp (cdr term)) term
	  (if (and (listp term) (lambda-bd-p term))
	      (find-fhead2 (cdr term))
	    (find-fhead2 (car term))))))

(defun prune-unifs (unifs sklist max)
  ;;each unifier's constants may intersect at most max of the lists in sklist.
  (let (newunifs count occurs)
    (dolist (s unifs (nreverse newunifs))
      (setq count 0 occurs (symbols-of s))
      (dolist (sk sklist (push s newunifs))
	(when (intersection sk occurs)
	  (incf count)
	  (when (> count max) (return nil)))))))
      
(defun unify-unifhash-rew (nodes mating key litkey touches)
  ;;idea : apply all possible combinations of subs. lambda-normalize the resulting dpairs.
  ;;if they're equal up to some number of rewrites, accept them, o/w reject them.
  (setq *allowed-subs-list* nil)
  (let* ((varlist (get-vars-for (qnode-dpairs (car nodes)) (free-vars-in-etree current-eproof)))
	 (usc1 (append (unreachable-skolem-constants (caar mating)) (unreachable-skolem-constants (cdar mating))))
	 (banned (when (and ms98-num-of-dups (= (ccs-count-dups-in touches) ms98-num-of-dups))
		   (ccs-banned-sk-consts touches)))
	ignore-nodes)
    (do ((son (pop nodes) (pop nodes)))
	((null son)
	 (setq ignore-nodes (remove-if-not #'(lambda (x) (represents-a-rewrite x mating usc1)) ignore-nodes))
	 (if (null ignore-nodes) nil
	   (if (= key -1) t
	     (construct-component (dagify-nodes (mapcar #'qnode-substs ignore-nodes)) mating key litkey touches))))
      (multiple-value-bind (newnodes done)
	  (ccs-match-rew son varlist banned)
	(if done (setq ignore-nodes (nconc newnodes ignore-nodes))
	  (setq nodes (nconc newnodes nodes)))))))

(defun ccs-match-rew (qnode varlist banned)
  (let ((dpairs (qnode-dpairs qnode))
	real-val flex-var done)
    (setq flex-var (setdiff (remove-duplicates (get-vars-for dpairs varlist))
			    (mapcar #'car (qnode-substs qnode))))
    (setq done (not (cdr flex-var)))
    (if flex-var
	(progn (setq flex-var (car flex-var))
	       (dolist (v (get-correct-subs flex-var nil banned) (values real-val done))
		 (push (create-successor-rew (list (cons flex-var v)) dpairs qnode)
		       real-val)))
      (values (list qnode) done))))

(defun get-vars-for (dpairs varlist)
  (let (retlist) 
    (dolist (d dpairs retlist)
      (setq retlist (nconc (intersection (mapcar #'get-flags-fn-1
						 (ho-free-vars-of (car d))) varlist)
			   retlist))
      (setq retlist (nconc (intersection (mapcar #'get-flags-fn-1
						 (ho-free-vars-of (cdr d))) varlist)
			   retlist)))))

(defun create-successor-rew (subst dpairs node)
  (make-qnode :dpairs dpairs :substs (append subst (qnode-substs node))))

(defun lnorm-q2 (wff substs)
  (prune-numbers-from (lnorm-q wff substs)))

(defun prune-numbers-from (wff)
  (cond ((lsymbol-q wff) wff)
	((boundwff-q wff) (cons (prune-numbers-from (car wff)) (prune-numbers-from (cdr wff))))
	((consp wff) (if (numberp (cdr wff)) (car wff)
		       (cons (prune-numbers-from (car wff)) (prune-numbers-from (cdr wff)))))))

(defun unreachable-skolem-constants (lit)
  (when (null *usc-list*) (fill-usc-list))
  (cdr (assoc (literal-name lit) *usc-list*)))

(defun fill-usc-fn-1 (x) (or (skolem-p x) (selection-p x)))
(defun fill-usc-fn-2 (x) (head (skolem-term-parameter x)))

(defun fill-usc-list ()
  (let (result temp
	(sk-nodes (find-etree-nodes #'fill-usc-fn-1 current-topnode))
	(leaves *live-leaves*))
    (dolist (leaf leaves (setq *usc-list* result))
      (setq temp nil)
      (dolist (sk sk-nodes (push (cons (leaf-name leaf) (consider-global-consts (remove-duplicates temp))) result))
	(when (standing-disjunctively leaf sk)
	  (setq temp (append (mapcar #'fill-usc-fn-2
				     (if (skolem-p sk) (skolem-terms sk) (selection-terms sk))) temp)))))))

(defun consider-global-consts (constlist)
  (dolist (c *global-constlist* constlist)
    (unless (and (cdr c) (setdiff (cdr c) constlist)) ;if we have deleted all the consts at this result type...
      (setq constlist (delete (cadr c) constlist :test #'equal))))) ;...then put one of them back.
      
(defun path-up-from (leaf)
  (if (null leaf) nil
    (if (rtree-p (rtree-parent leaf))
	(cons (list (rtree-rule leaf) (rtree-right leaf) (rtree-gwff leaf)) (path-up-from (rtree-parent leaf)))
      nil)))

(defun match-modulo-neg (dtree1 dtree2 neg)
  (if neg
      (let ((negs1 (cdr (assoc 'NOT (dtree-sons dtree1))))
	    (negs2 (cdr (assoc 'NOT (dtree-sons dtree2))))
	    result)
	(when negs1 (setq result (match-mod-neg-real negs1 dtree2)))
	(or result
	    (and negs2 (match-mod-neg-real dtree1 negs2))))
    (match-mod-neg-real dtree1 dtree2)))

(defun match-mod-neg-real (dt1 dt2)
  (if (or (null dt1) (null dt2)) nil
    (if (and (null (dtree-sons dt1)) (null (dtree-sons dt2)))
	(cons (dtree-refers dt1) (dtree-refers dt2))
      (dolist (d1 (dtree-sons dt1))
	(let ((result (match-mod-neg-real (cdr d1) (cdr (assoc (car d1) (dtree-sons dt2))))))
	  (when result (return result)))))))


(defun get-final-subs-delay-comp (comp1 ccs-original previous-subs previous-clist previous-touched mating newkey newlitkey touches ban-me max-weight)
  ;;previous-subs is guaranteed not to be 'delay !!
  (when (or (eq previous-subs 'fail) 
	    (not (quick-check-subs (component-clist comp1)))) ;;rest of the mating is OK.
    (return-from get-final-subs-delay-comp nil))
  (let* ((newsub previous-subs)
	 (newconns (remove-if #'(lambda (x) (member x previous-clist :test #'equal)) (component-clist comp1)))
	 (sublist (remove-if-not #'(lambda (x) (member (car (component-clist x)) newconns
						       :test #'equal))
				 ccs-original))
	 (sublist1 (remove-if-not #'get-final-fn-1 sublist))
	 (sublist2 (remove-if #'get-final-fn-1 sublist))
	 (fail-right-now (dolist (s sublist2 nil)
			   (when (eq (component-final-subs s) 'fail) (return t))))
	 (temp-touches previous-touched))
    (if fail-right-now nil
      (if (or sublist1 ;we can't store 'delay in a list, so if that happened we're stumped
	      (null max-weight)
	      (let* ((lits (remove-duplicates (reduce #'append (mapcar #'(lambda (x) (list (car x) (cdr x))) mating))))
		     (weight (precompute-weight (find-blocks lits) (find-touches lits) mating)))
		(or (null weight) (>= weight max-weight))))
	  (progn (runcount 'unification)
		 (dolist (c (sort sublist2 #'< :key #'(lambda (x) (cadr (dnode-keys (component-final-subs x)))))
			    (dolist (d sublist1
				       (prog1 (construct-component newsub mating newkey newlitkey touches)
					 (breakcount 'unification)))
			      (collapse-dag newsub)
			      (setq temp-touches (ordered-union (component-touches d) temp-touches))
			      (setq newsub (merge-subs newsub 
						       (qum-delay (car (component-clist d)) 
								  (component-touches d))
						       0 temp-touches nil ban-me))
			      (unless newsub 
				(breakcount 'unification) (return nil))))
		   (setq temp-touches (ordered-union (component-touches c) temp-touches))
		   (setq newsub (merge-subs newsub 
					    (component-final-subs c) 
					    0 temp-touches nil ban-me))
		   (unless newsub (breakcount 'unification) (return nil))))
;;	(case ms98-merge-dags
;;	  (0 
	(construct-component 
	 (sort (cons newsub (mapcar #'component-final-subs sublist2))
	       #'< :key #'(lambda (x) (cadr (dnode-keys x))))
	 mating newkey newlitkey touches))
;;	  (1 (when (dolist (s sublist2 t)
;;		     (unless (dag-will-merge newsub (component-final-subs s))
;;		       (return nil)))
;;	       (construct-component 
;;		(sort (cons newsub (mapcar #'component-final-subs sublist2))
;;		      #'< :key #'(lambda (x) (cadr (dnode-keys x))))
;;		mating newkey newlitkey touches)))
;;	  (2 (when (and (dolist (s sublist2 t)
;;			  (unless (dag-will-merge newsub (component-final-subs s))
;;			    (return nil)))
;;			(daglist-will-merge (cons newsub (mapcar #'component-final-subs sublist2)) touches))
;;	       (construct-component 
;;		(sort (cons newsub (mapcar #'component-final-subs sublist2))
;;		      #'< :key #'(lambda (x) (cadr (dnode-keys x))))
;;		mating newkey newlitkey touches)))))
      )))

(defun fill-in-delayed-subs (sublist &optional (clist nil))
  (and (quick-check-subs clist)
       (let ((retval (car sublist)))
	 (dolist (s (cdr sublist) retval)
	   (setq retval (merge-subs retval s 0))
	   (unless retval (return nil))))))

(defun fill-in-delayed-subs-2 (sublist &optional (clist nil))
  (if (quick-check-subs clist)
      (let ((retval (car sublist)))
	(dolist (s (cdr sublist) retval)
	  (setq retval (merge-subs retval s 0))
	  (unless retval (return 'fail))))
    'fail))

(defun quick-check-subs (clist)
  (do ((c (mapcar #'(lambda (x) (gethash x primehash1)) clist) (cdr c)))
      ((null (cdr c)) t)
    (unless (dolist (d (cdr c) t)
	      (when (eq (gethash (* (car c) d) pairhash) 'fail)
		(return nil)))
      (return nil))))

(defun quick-check-subs-2 (numlist)
  (do ((c numlist (cdr c)))
      ((null (cdr c)) t)
    (unless (dolist (d (cdr c) t)
	      (when (eq (gethash (* (car c) d) pairhash) 'fail)
		(return nil)))
      (return nil))))
