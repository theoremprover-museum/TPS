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

(deffile ms90-3-unif-tree
  (part-of ms90-3)
  (mhelp "Implementation of Huet's unification algorithm for Path-focused
duplication."))


(defun ho-bind-vars (gwff free-vars bdvars)
  (cond ((symbolp gwff)
         (if (memq gwff bdvars) gwff
             (or (assoc gwff free-vars) gwff)))
        ((lambda-bd-p gwff)
         (cons (car gwff)
               (ho-bind-vars (cdr gwff) free-vars
                             (cons (caar gwff) bdvars))))
        (t (cons (ho-bind-vars (car gwff) free-vars bdvars)
                 (ho-bind-vars (cdr gwff) free-vars bdvars)))))

(defun ho-unif-lnorm (neg gwff bdvars sstack bstack)
  (multiple-value-bind (gwff neg)
      (ho-unif-lnorm-rec neg gwff bdvars sstack bstack)
    (if neg (cons 'not gwff) gwff)))


(defun ho-unif-lnorm-rec (neg gwff bdvars sstack bstack)
  (cond ((symbolp gwff)
         (if (memq gwff bdvars) (values gwff neg)
             (let ((term (assoc gwff bstack)))
               (if term (ho-unif-lnorm-rec neg (cdr term) nil sstack bstack) 
                   (values gwff neg)))))
        ((numberp (cdr gwff))
         (let ((term (assoc gwff sstack)))
           (if term (ho-unif-lnorm-rec neg (cdr term) bdvars sstack bstack)
               (values gwff neg))))
        ((not-p gwff)
         (ho-unif-lnorm-rec (not neg) (cdr gwff) bdvars sstack bstack))
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
                (cons (car gwff) (ho-unif-lnorm
                                  nil (cdr gwff) (cons (caar gwff) bdvars)
                                  sstack bstack)))
               (t (throwfail "HO-UNIF-LNORM-REC: Unknown bdwfftype in: "
                             (gwff . gwff)))))
        (t (if (lambda-bd-p (car gwff))
               (ho-unif-lnorm-rec
                neg (cdar gwff) bdvars sstack
                (acons (caaar gwff)
                       (if bstack
                           (ho-unif-lnorm nil (cdr gwff) bdvars nil bstack)
                           (cdr gwff))
                       bstack))
               (let ((gwff (cons (ho-unif-lnorm
                                  nil (car gwff) bdvars sstack bstack)
                                 (if bstack
                                     (ho-unif-lnorm nil (cdr gwff) bdvars nil
                                                    bstack)
                                     (cdr gwff)))))
                 (if (lambda-bd-p (car gwff))
                     (ho-unif-lnorm-rec neg (cdar gwff) bdvars sstack
                                        (acons (caaar gwff) (cdr gwff) bstack))
                     (values gwff neg)))))))

(defun rigidp (term vars)
  (if (symbolp term) (values term vars)
      (if (numberp (cdr term)) nil
          (if (lambda-bd-p term)
              (rigidp (cdr term) (cons (caar term) vars))
              (rigidp (car term) vars)))))

(defun create-successors (substs node)
  (mapcar #'(lambda (subst)
	      (let ((son (copy-unode node)))
		(push subst (unode-substs son))
		(setf (unode-sons son) nil)
		(incf (unode-depth son))
		son))
	  substs))

;; old code that generated substitutions instead of first counting them.

(context unification)

(defflag countsubs-first
  (flagtype boolean)
  (default nil)
  (subjects unification transmit)
  (mhelp "if NIL, the substitutions which MATCH generates for each dpair in the
unification process are generated and counted, and then MATCH is
actually applied to the variable for which this number is smallest; if
T, the substitutions are counted before they are generated, and only
those which will be applied are actually generated.
Applies to UN90 only."))


(defmacro ho-unif-match (unode)
  `(if COUNTSUBS-FIRST
       (ho-unif-match-count ,unode)
     (ho-unif-match-uncount ,unode)))

(defmacro ho-unif-match-substs (unode)
  `(if COUNTSUBS-FIRST
       (ho-unif-match-count-substs ,unode)
     (ho-unif-match-uncount-substs ,unode)))

(defun ho-unif-match-uncount (unode)
	   (let ((old-substs nil)
		 (frpair-ctr 0))
	     (dolist (dpair (unode-dpairs unode))
	       (multiple-value-bind (rhead bdvars)
		   (rigidp (car dpair) nil)
		 (when rhead
		   (let* ((fhead (find-fhead (cdr dpair)))
			  (substitutions (ho-match-top rhead bdvars fhead)))
		     (when (and (not substitutions) (eq rhead 'not))
		       (let ((head1 (ms90-3-head (car dpair))))
			 (when (and (consp head1) (numberp (cdr head1)))
			   (setq substitutions
				 (ho-match-top 'not bdvars head1)))))
		     (if substitutions
			 (if (= (length substitutions) 1)
			     (progn (setq old-substs substitutions)
				    (return t))
			     (if (or (not old-substs) (< (length substitutions)
							 (length old-substs)))
				 (setq old-substs substitutions)))
			 (unless (memq fhead imitation-h-var-list)
			   (return-from ho-unif-match-uncount nil))))
		   (when (> (incf frpair-ctr) num-frpairs)
		     (return t)))))
	     (create-successors old-substs unode)))

(defun ho-unif-match-count (unode)
  (let ((substitutions nil)
	(frpair-ctr 0)
	(min-subst-count most-positive-fixnum)
	(min-dpair nil))

    ;; compute the dpair that generates the fewest substitutions, 
    ;; while examining at most NUM-FRPAIRS dpairs

    (dolist (dpair (unode-dpairs unode))
      (multiple-value-bind (rhead bdvars)
          (rigidp (car dpair) nil)
        (when rhead
          (let* ((fhead (find-fhead (cdr dpair)))
                 (sub-count (ho-match-top-count rhead bdvars fhead)))
            (when (and (zerop sub-count) (eq rhead 'not))
              (let ((head1 (ms90-3-head (car dpair))))
                (when (and (consp head1) (numberp (cdr head1)))
                  (setq sub-count
                        (ho-match-top-count 'not bdvars head1)))))
            (if (zerop sub-count)
                (unless (memq fhead imitation-h-var-list)
                  (return-from ho-unif-match-count nil))
                (if (= sub-count 1)
                    (progn (setq min-subst-count 1
				 min-dpair dpair)
                           (return t))
                    (if (< sub-count min-subst-count)
			(setq min-subst-count sub-count
			      min-dpair dpair)))))
          (when (> (incf frpair-ctr) num-frpairs)
            (return t)))))

    ;; Generate the substitutions for the MIN-DPAIR selected above.

    (when min-dpair
      (multiple-value-bind (rhead bdvars)
	  (rigidp (car min-dpair) nil)
	(setq substitutions (ho-match-top rhead bdvars 
					  (find-fhead (cdr min-dpair))))
	(unless substitutions
	  ;; implicitly replace dpair (F1 ~F2) by (~F1 F2)
	  (let ((head1 (ms90-3-head (car min-dpair))))
	    (setq substitutions (ho-match-top 'not bdvars head1))))))

    (create-successors substitutions unode)))

(defun ho-unif-match-count-substs (unode)
  (let ((substitutions nil)
	(frpair-ctr 0)
	(min-subst-count most-positive-fixnum)
	(min-dpair nil)
	(curr-max 0))
    (multiple-value-bind (topvars topvars-proj hvars ptotal)
			 (ho-check-simpl-count-2 (unode-substs unode))
    (if (or (and max-substs-var (> (reduce 'max (mapcar 'cdr topvars)) max-substs-var))
	    (and max-substs-proj (> (reduce 'max (mapcar 'cdr topvars-proj)) max-substs-proj))
	    (and max-substs-proj-total (> ptotal max-substs-proj-total)))
	(return-from ho-unif-match-count-substs nil)
      (progn
	(dolist (dpair (unode-dpairs unode))
		(multiple-value-bind (rhead bdvars)
				     (rigidp (car dpair) nil)
        (when rhead
          (let* ((fhead (find-fhead (cdr dpair)))
		 (curr-max-temp (or (cdr (assoc (cdr (assoc fhead hvars)) topvars)) curr-max))
                 (sub-count (if (= curr-max-temp max-substs-var) 
				0 (ho-match-top-count rhead bdvars fhead))))
	    (when (= curr-max-temp max-substs-var) (return-from ho-unif-match-count-substs nil))
            (when (and (zerop sub-count) (eq rhead 'not))
		  (let ((head1 (ms90-3-head (car dpair))))
                (when (and (consp head1) (numberp (cdr head1)))
                  (setq sub-count
                        (ho-match-top-count 'not bdvars head1)))))
            (if (zerop sub-count)
                (unless (memq fhead imitation-h-var-list)
                  (return-from ho-unif-match-count-substs nil))
                (if (= sub-count 1)
                    (progn (setq min-subst-count 1
				 min-dpair dpair)
                           (return t))
                    (if (or (> curr-max-temp curr-max)
			    (and (= curr-max-temp curr-max) (< sub-count min-subst-count)))
			(setq min-subst-count sub-count
			      curr-max curr-max-temp
			      min-dpair dpair)))))
          (when (> (incf frpair-ctr) num-frpairs)
            (return t))))))))
    (when min-dpair
      (multiple-value-bind (rhead bdvars)
	  (rigidp (car min-dpair) nil)
	(setq substitutions (ho-match-top rhead bdvars 
					  (find-fhead (cdr min-dpair))))
	(unless substitutions
	  (let ((head1 (ms90-3-head (car min-dpair))))
	    (setq substitutions (ho-match-top 'not bdvars head1))))))
    (create-successors substitutions unode)))

(defun ho-unif-match-uncount-substs (unode)
	   (let ((old-substs nil)
		 (frpair-ctr 0)
		 (curr-max 0))
    (multiple-value-bind (topvars topvars-proj hvars ptotal)
			 (ho-check-simpl-count-2 (unode-substs unode))
    (if (or (and max-substs-var (> (reduce 'max (mapcar 'cdr topvars)) max-substs-var))
	    (and max-substs-proj (> (reduce 'max (mapcar 'cdr topvars-proj)) max-substs-proj))
	    (and max-substs-proj-total (> ptotal max-substs-proj-total)))
	(return-from ho-unif-match-uncount-substs nil)
      (progn
	     (dolist (dpair (unode-dpairs unode))
	       (multiple-value-bind (rhead bdvars)
		   (rigidp (car dpair) nil)
		 (when rhead
		   (let* ((fhead (find-fhead (cdr dpair)))
			  (curr-max-temp (or (cdr (assoc (cdr (assoc fhead hvars)) topvars)) curr-max))
			  (substitutions (if (= curr-max-temp max-substs-var) nil
					   (if (< curr-max-temp curr-max) 'dummy-looks-like-t
					     (ho-match-top rhead bdvars fhead)))))
		     (when (= curr-max-temp max-substs-var) (return-from ho-unif-match-uncount-substs nil))
		     (when (and (not substitutions) (eq rhead 'not))
		       (let ((head1 (ms90-3-head (car dpair))))
			 (when (and (consp head1) (numberp (cdr head1)))
			   (setq substitutions
				 (ho-match-top 'not bdvars head1)))))
		     (if (and (>= curr-max-temp curr-max) substitutions)
			 (if (= (length substitutions) 1)
			     (progn (setq old-substs substitutions)
				    (return t))
			     (if (or (not old-substs) 
				     (and (= curr-max-temp curr-max)
					  (< (length substitutions) (length old-substs)))
				     (> curr-max-temp curr-max))
				 (setq old-substs substitutions curr-max curr-max-temp)))
		       (unless (and (memq fhead imitation-h-var-list) (< curr-max-temp curr-max))
			   (return-from ho-unif-match-uncount-substs nil))))
		   (when (> (incf frpair-ctr) num-frpairs)
		     (return t))))))))
	     (create-successors old-substs unode)))

(defun ho-check-simpl-count-2 (substs)
  (let ((topvars '((0 . 0)))
	(hvars nil)
	(topvars-proj '((0 . 0)))
	(proj-total 0)
	(pcount (or max-substs-proj max-substs-proj-total))
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
    (values topvars topvars-proj hvars proj-total)))

(defun ho-unify (root &optional (depth (or max-search-depth
                                           most-positive-fixnum))
		      (start-time nil) (time-limit nil) (quickp nil))
  (runcount 'unification)
  (let ((return-value
	 (if (and max-substs-quick max-substs-var)
	     (ho-unify-msv root depth start-time time-limit quickp)
	   (if (eq uni-search-heuristic 'breadth-first)
	       (ho-unify-bf root depth start-time time-limit)
	     (if (eq uni-search-heuristic 'depth-first)
		 (ho-unify-df root depth start-time time-limit)
	       (if (eq uni-search-heuristic 'best-first)
		   (ho-unify-best root depth start-time time-limit)
		 (throwfail "Don't recognise UNI-SEARCH-HEURISTIC " uni-search-heuristic t)))))))
    (breakcount 'unification)
    return-value))

(defun ho-unify-df (root &optional (depth (or max-search-depth
                                           most-positive-fixnum))
		      (start-time nil) (time-limit nil))
  (if (if (unode-sons root)
          (dolist (son (copy-list (unode-sons root)) nil)
            (when (ho-unify-rec son root (- depth (unode-depth son)) start-time time-limit)
              (return t)))
          (ho-unify-rec root root depth start-time time-limit))
      (progn
        (setf (unode-sons root)
              (sort (unode-sons root) #'> :key #'unode-depth))
	(when subsumption-check
	      (do ((subsumption-nodes 'all-nodes)
		   (elt (car (unode-sons root)) (car oldelts))
		   (oldelts (remove-if #'(lambda (x) (option-> (unode-depth x) subsumption-depth))
				       (copy-list (cdr (unode-sons root)))) (cdr oldelts)))
		  ((null oldelts))
		  (if (ms90-3-subsumption-check elt oldelts start-time time-limit)
		      (setf (unode-sons root)
			    (remove-if #'(lambda (x) (eq x elt)) (unode-sons root))))))
	(progn (when (and (unode-sons root) (eq unif-trigger 'utree-end) (eq unif-counter-output 10))
					    (unif-stats3 root)) t))
      (progn (when (and (unode-sons root) (eq unif-trigger 'utree-end) (eq unif-counter-output 10))
					    (unif-stats3 root)) nil)))
      
(defun ho-unify-bf (root &optional (depth (or max-search-depth
                                           most-positive-fixnum))
		      (start-time nil) (time-limit nil))
  (do ((count depth (1- count))
       (flag nil))
      ((or flag (zerop count))
       (progn (when (and (unode-sons root) (eq unif-trigger 'utree-end) (eq unif-counter-output 10))
		    (unif-stats3 root)) flag))
      (if (if (unode-sons root)
	      (dolist (son (copy-list (unode-sons root)) nil)
		      (when (ho-unify-rec son root (if max-search-depth (min (- depth (unode-depth son)) 1) 1)
					  start-time time-limit)
			    (return t)))
	    (ho-unify-rec root root 1 start-time time-limit))
	  (progn
	    (setf (unode-sons root)
		  (sort (unode-sons root) #'< :key #'unode-depth))
	    (when subsumption-check
		  (do ((subsumption-nodes 'all-nodes)
		       (elt (car (unode-sons root)) (car oldelts))
		       (oldelts (remove-if #'(lambda (x) (option-> (unode-depth x) subsumption-depth))
					   (copy-list (cdr (unode-sons root)))) (cdr oldelts)))
		      ((null oldelts))
		      (if (ms90-3-subsumption-check elt oldelts start-time time-limit)
			  (setf (unode-sons root)
				(remove-if #'(lambda (x) (eq x elt)) (unode-sons root))))))
       (setq flag t))
	)))
      
(defun ho-unify-best (root &optional (depth (or max-search-depth
                                           most-positive-fixnum))
		      (start-time nil) (time-limit nil))
  (do ((count depth (1- count))
       (flag nil))
      ((or flag (zerop count)) flag)
      (if (if (unode-sons root)
	      (dolist (son (copy-list (unode-sons root)))
		      (when (ho-unify-rec son root (min (- depth (unode-depth son)) 1) start-time time-limit)
			    (return t)))
	    (ho-unify-rec root root 1 start-time time-limit))
	  (progn
	    (setf (unode-sons root)
		  (sort (unode-sons root) #'< :key #'unode-free-vars))
	    (when subsumption-check
		  (do ((subsumption-nodes 'all-nodes)
		       (elt (car (unode-sons root)) (car oldelts))
		       (oldelts (remove-if #'(lambda (x) (option-> (unode-depth x) subsumption-depth))
					   (copy-list (cdr (unode-sons root)))) (cdr oldelts)))
		      ((null oldelts))
		      (if (ms90-3-subsumption-check elt oldelts start-time time-limit)
			  (setf (unode-sons root)
				(remove-if #'(lambda (x) (eq x elt)) (unode-sons root))))))
        (setq flag t))
	)))
      
(defun unode-free-vars (node)
  (let ((dpairs (unode-dpairs node))
	(substs (unode-substs node))
	(count 0))
    (dolist (dpair dpairs)
	    (let ((first (ho-free-vars (ho-unif-lnorm nil (car dpair) nil substs nil)))
		  (second (ho-free-vars (ho-unif-lnorm nil (cdr dpair) nil substs nil))))
	    (setq count (+ count (+ (length first) (length second))))))
    count))
    
(defun ho-free-vars (gwff)
  (remove-duplicates (ho-free-vars2 gwff nil)))

(defun ho-free-vars2 (gwff bind-list)
  (cond ((label-q gwff) (apply-label gwff (ho-free-vars2 gwff bind-list)))
	((numberp gwff) nil)
	((lsymbol-q gwff)
	 (cond ((get gwff 'abbrev) (ho-free-vars2 (get gwff 'defn) bind-list))
               (t (if (and (not (logconst-q gwff)) (propsym-q gwff)
		           (not (member gwff bind-list)))
	              (list gwff)))))
	((boundwff-q gwff) (ho-free-vars2 (cdr gwff) (cons (caar gwff) bind-list)))
	(t (nconc (ho-free-vars2 (car gwff) bind-list) 
		  (ho-free-vars2 (cdr gwff) bind-list)))))

(defun ho-unify-msv (root &optional (depth (or max-search-depth most-positive-fixnum))
		      (start-time nil) (time-limit nil) (quickp nil))
  (do ((count depth (1- count))
       (success nil)
       (fail nil)
       (ignore-nodes nil)
       (new-nodes nil nil))
      ((or success fail (zerop count))
       (progn 
	 (setf (unode-sons root) (sort (nconc ignore-nodes (unode-sons root)) #'< :key #'unode-depth))
	 success))
      (unless (unode-sons root)
	      (setf (unode-sons root) (list (make-unode :dpairs (unode-dpairs root)
							:sons nil
							:depth (unode-depth root)
							:substs (unode-substs root)
							:fo-mode (unode-fo-mode root)))))
      (dolist (son (unode-sons root))
	      (let ((newsons nil)
		    (simpl (if first-order-mode-ms (fo-simpl son) (ho-simpl son))))
		(if success (when simpl (push son ignore-nodes))
		  (when (and (plusp depth) simpl (or (zerop simpl) (not leibniz-sub-check) 
						     (ho-leibniz-subs-ok son max-substs-var)))
			      (if (zerop simpl)
				  (progn (push son ignore-nodes)
					 (setq success t))
				(progn
				  (setq newsons (ho-unif-match-count-msq son quickp))
				  (when (eq newsons 48)
					(setq newsons nil success t)
					(push son ignore-nodes))))
			      (when newsons (setq new-nodes (nconc new-nodes newsons)))))))
      (setf (unode-sons root) new-nodes)
      (when (and time-limit
		 (>= (/ (- (get-net-internal-run-time) start-time) 
			internal-time-units-per-second)
		     time-limit))
	    (setq success nil fail t))
      (when (null new-nodes)
	    (if (null ignore-nodes)
		(setq fail t success nil)
	      (setq success t fail nil)))))

(defun ho-leibniz-subs-ok (node msv)
  (declare (ignore msv))
  (if *leibniz-var-list*
      (progn 
	(unless (unode-msv node) 
		(multiple-value-bind (topvars topvars-proj hvars ptotal)
				     (ho-check-simpl-count-2 (unode-substs node))
				     (setf (unode-msv node) (list topvars topvars-proj hvars ptotal))))
	(let ((lvl (remove-if #'null
			      (mapcar #'(lambda (x) (if (and (not (numberp (car x))) 
							     (memq (caar x) (mapcar #'car *leibniz-var-list*)))
							x nil))
				      (car (unode-msv node))))))
	  (dolist (lvar lvl t)
		  (let* ((leib (car lvar))
			 (subs-so-far (cdr lvar))
			 (lsub (and subs-so-far (ho-unif-lnorm nil leib nil (unode-substs node) nil))))
		    (when lsub
			  (unless (or (not (boundwff-q lsub))
				      (not (lambda-bd-p lsub))
				      (ho-free-in-2 (caar lsub) (cdr lsub)))
				  (when (memq unify-verbose '(med max)) (msg "u"))
				  (return nil)))))))
    t))

(defun ho-free-in-2 (var term)
  (cond ((symbolp term) (eq var term))
	((numberp (cdr term)) (eq var term))
	(t (or (ho-free-in-2 var (car term))
	       (ho-free-in-2 var (cdr term))))))

(defun ho-unif-match-count-msq (unode quickp)
  (let ((substitutions nil)
	(frpair-ctr 0)
	(min-subst-count most-positive-fixnum)
	(min-dpair nil)
	(flexhead nil)
	(pass2 nil)
	(branch (zerop max-substs-quick))
	(msv-diff (and quickp (< max-substs-quick max-substs-var))))
    (multiple-value-bind (topvars topvars-proj hvars ptotal)
			 (if (unode-msv unode)
			     (values (car (unode-msv unode)) (cadr (unode-msv unode))
				     (caddr (unode-msv unode)) (cadddr (unode-msv unode)))
			     (ho-check-simpl-count-2 (unode-substs unode)))
    (unless (unode-msv unode)
	    (setf (unode-msv unode) (list topvars topvars-proj hvars ptotal)))
    (if (or (and max-substs-proj (> (reduce 'max (mapcar 'cdr topvars-proj)) max-substs-proj))
	    (and max-substs-proj-total (> ptotal max-substs-proj-total)))
	(return-from ho-unif-match-count-msq nil)
      (progn
	(dolist (dpair (unode-dpairs unode))
		(multiple-value-bind (rhead bdvars) (rigidp (car dpair) nil)
        (when rhead
          (let* ((fhead (find-fhead (cdr dpair)))
                 (sub-count (ho-match-top-count rhead bdvars fhead))
		 (pass nil)
		 (dummy (cdr (assoc fhead hvars))))
	    (when (and dummy (>= (cdr (assoc dummy topvars)) (if (and msv-diff (not branch))
								 max-substs-quick max-substs-var)))
		  (if msv-diff (setq pass2 t pass t)
		    (return-from ho-unif-match-count-msq nil)))
	    (when (and (zerop sub-count) (eq rhead 'not))
		  (let ((head1 (ms90-3-head (car dpair))))
		    (when (and (consp head1) (numberp (cdr head1)))
			  (setq sub-count
				(ho-match-top-count 'not bdvars head1))
			  (setq dummy (cdr (assoc head1 hvars)))
			  (when (and dummy (>= (cdr (assoc dummy topvars)) 
					       (if (and msv-diff (not branch))
						   max-substs-quick max-substs-var)))
				(if msv-diff (setq pass2 t pass t)
				  (return-from ho-unif-match-count-msq nil))))))
	    (unless pass
		    (if (zerop sub-count)
			(unless (memq fhead imitation-h-var-list)
				(return-from ho-unif-match-count-msq nil))
		      (if (= sub-count 1)
			  (progn (setq min-subst-count 1 min-dpair dpair flexhead dummy)
				 (return t))
			(if (< sub-count min-subst-count)
			    (setq min-subst-count sub-count min-dpair dpair flexhead dummy))))
		    (when (> (incf frpair-ctr) num-frpairs)
			  (return t)))))))))
    (when (or (and pass2 (not min-dpair))
	      (and msv-diff branch (neq min-subst-count 1)))
	  (return-from ho-unif-match-count-msq 48))
    (when min-dpair
	  (multiple-value-bind (rhead bdvars)
			       (rigidp (car min-dpair) nil)
			       (setq substitutions (ho-match-top rhead bdvars 
								 (find-fhead (cdr min-dpair))))
			       (unless substitutions
				       (let ((head1 (ms90-3-head (car min-dpair))))
					 (setq substitutions (ho-match-top 'not bdvars head1))))))
    (create-successors-2 substitutions unode flexhead 
			 topvars topvars-proj hvars ptotal (or max-substs-proj max-substs-proj-total)))))

(defun create-successors-2 (substs node flexhead topvars topvars-proj hvars ptotal pcount)
  (unless (null substs)
	  (let ((realhead (caar substs)))
	    (if flexhead
		(incf (cdr (assoc flexhead topvars)))
	      (push (cons realhead 1) topvars))
	    (mapcar #'(lambda (subst)
			(let ((son (copy-unode node))
			      (topvars-c (copy-alist topvars))
			      (topvars-proj-c (and pcount (copy-alist topvars-proj)))
			      (proj-total ptotal)
			      (hvars-c (copy-alist hvars)))
			  (push subst (unode-substs son))
			  (setf (unode-sons son) nil)
			  (if flexhead (progn
					 (when pcount
					       (unless (memq (ms90-3-head (cdr subst)) (ho-free-vars-of (cdr subst)))
						       (incf (cdr (assoc flexhead topvars-proj-c)))
						       (incf proj-total)))
					 (setq hvars-c (nconc (mapcar #'(lambda (x) (cons x flexhead))
								      (ho-free-vars-of (cdr subst)))
							      hvars-c)))
			    (progn 
			      (when pcount
				    (if (memq (ms90-3-head (cdr subst)) (ho-free-vars-of (cdr subst)))
					(push (cons realhead 0) topvars-proj-c)
				      (progn (incf proj-total) 
					     (push (cons realhead 1) topvars-proj-c))))
			      (setq hvars-c (nconc (mapcar #'(lambda (x) (cons x realhead))
							   (ho-free-vars-of (cdr subst)))
						   hvars-c))))
			  (setf (unode-msv son) (list topvars-c topvars-proj-c hvars-c proj-total))
			  (incf (unode-depth son))
			  son))
		    substs))))

(defun ho-unify-rec (unode root depth &optional (start-time nil) (time-limit nil))
;  (if (and (eq unify-verbose 'MAX) (not (plusp depth)))
;      (msgf "MAX-SEARCH-DEPTH limit reached." t))
; the above is really annoying. MB Wed Sep  4 11:45:42 1996
  (if (plusp depth)
      (let ((simpl (if first-order-mode-ms (fo-simpl unode)
		       (ho-simpl unode))))
        (if simpl
            (if (zerop simpl)
                t
	      (let ((sons (if (eq apply-match 'apply-match-max-substs)
			      (ho-unif-match-substs unode)
			    (if (or max-substs-var max-substs-proj max-substs-proj-total) 
				(remove-if #'(lambda (x) (ho-simpl-subst-check (unode-substs x))) (ho-unif-match unode))
			      (ho-unif-match unode)))))
                  (setf (unode-sons root)
                        (append sons (delete
                                      unode (unode-sons root) :test #'eq)))
                  (dolist (new-unode sons nil)
			  (when (and time-limit
				     (>= (/ (- (get-net-internal-run-time) start-time) 
					    internal-time-units-per-second)
					 time-limit))
				(return nil))
			  (when (ho-unify-rec new-unode root (1- depth) start-time time-limit)
				(return t)))))
            (progn
              (setf (unode-sons root)
                    (delete unode (unode-sons root) :test #'eq))
              nil)))
      nil))

(defmexpr least-search-depth
   (mhelp "Print the least needed unification tree depth for the last proven 
higher-order theorem. Also suggest to lower flags MAX-SEARCH-DEPTH to the least
needed value if they are greater than it."))

(defun least-search-depth ()
  (if first-order-mode-ms
      (msgf "FIRST-ORDER-MODE-MS is T, so this command is irrelevant." t)
    (let ((max-substs-depth (find-max-substs)))
        (msg t "The least needed unification tree depth is " *least-search-depth* ".")
	(when max-substs-depth 
	      (msg t "The minimal usable value of MAX-SUBSTS-VAR is " (car max-substs-depth) ".")
	      (msg t "The minimal usable value of MAX-SUBSTS-PROJ is " (cadr max-substs-depth) ".")
	      (msg t "The minimal usable value of MAX-SUBSTS-PROJ-TOTAL is " (caddr max-substs-depth) "."))
        (when (and *least-search-depth* (or (null max-search-depth) (< *least-search-depth* max-search-depth)))
          (msg t "The value of flag MAX-SEARCH-DEPTH is " MAX-SEARCH-DEPTH ".")
          (let (change) 
             (prompt-read change nil
               (msgf "Set flag MAX-SEARCH-DEPTH to " *least-search-depth* "?")
	       'yesno 'yes 
               ((? (msgf "If YES, to lower MAX-SEARCH-DEPTH to the least needed value."))))
             (when change 
                   (set-flag 'max-search-depth *least-search-depth*))))
        (when (and *least-search-depth* (or (null max-utree-depth) (< *least-search-depth* max-utree-depth)))
          (msg t "The value of flag MAX-UTREE-DEPTH is " MAX-UTREE-DEPTH ".")
          (let (change) 
             (prompt-read change nil
               (msgf "Set flag MAX-UTREE-DEPTH to " *least-search-depth* "?")
	       'yesno 'yes 
               ((? (msgf "If YES, to lower MAX-UTREE-DEPTH to the least needed value."))))
             (when change 
                   (set-flag 'max-utree-depth *least-search-depth*))))
        (when (and (car max-substs-depth) (or (not max-substs-var) (< (car max-substs-depth) max-substs-var)))
          (msg t "The value of flag MAX-SUBSTS-VAR is " MAX-SUBSTS-VAR ".")
          (let (change) 
             (prompt-read change nil
               (msgf "Set flag MAX-SUBSTS-VAR to " (car max-substs-depth) "?")
	       'yesno 'yes 
               ((? (msgf "If YES, to set MAX-SUBSTS-VAR to the least needed value."))))
             (when change 
                   (set-flag 'max-substs-var (car max-substs-depth))))
          (let (change) 
             (prompt-read change nil
               (msgf "Set flag MAX-SUBSTS-QUICK to " (car max-substs-depth) "?")
	       'yesno 'yes 
               ((? (msgf "If YES, to set MAX-SUBSTS-QUICK to the least needed value."))))
             (when change 
                   (set-flag 'max-substs-quick (car max-substs-depth)))))
        (when (and (cadr max-substs-depth) (or (not max-substs-proj) (< (cadr max-substs-depth) max-substs-proj)))
          (msg t "The value of flag MAX-SUBSTS-PROJ is " MAX-SUBSTS-PROJ ".")
          (let (change) 
             (prompt-read change nil
               (msgf "Set flag MAX-SUBSTS-PROJ to " (cadr max-substs-depth) "?")
	       'yesno 'yes 
               ((? (msgf "If YES, to set MAX-SUBSTS-PROJ to the least needed value."))))
             (when change 
                   (set-flag 'max-substs-proj (cadr max-substs-depth)))))
        (when (and (caddr max-substs-depth) (or (not max-substs-proj-total) 
						(< (caddr max-substs-depth) max-substs-proj-total)))
          (msg t "The value of flag MAX-SUBSTS-PROJ-TOTAL is " MAX-SUBSTS-PROJ-TOTAL ".")
          (let (change) 
             (prompt-read change nil
               (msgf "Set flag MAX-SUBSTS-PROJ-TOTAL to " (caddr max-substs-depth) "?")
	       'yesno 'yes 
               ((? (msgf "If YES, to set MAX-SUBSTS-PROJ-TOTAL to the least needed value."))))
             (when change 
                   (set-flag 'max-substs-proj-total (caddr max-substs-depth))))))))

(defvar *max-substs-depth* nil)

(defun find-max-substs ()
  (let ((a (or (and current-eproof (node-p (mating-utree (car (eproof-mating-list current-eproof))))
		    (find-success-node-in-utree (mating-utree (car (eproof-mating-list current-eproof)))))
	       (and (node-p current-topnode) (find-success-node-in-utree current-topnode))
	       )))
    (if a (multiple-value-bind (x y z) (check-simpl-count (node-subst-stack a))
			       (list x y z)) (if *max-substs-depth* *max-substs-depth* nil))))
      
(defun find-success-node (root)
  (when (null root) (return-from find-success-node (make-unode :dpairs nil)))
  (when (and (not first-order-mode-ms)
	     max-substs-quick max-substs-var)
	(msg "C")
	(when (not (ho-unify-msv root)) (return-from find-success-node nil)))
  (let ((ctr 0) (utree-depth 0))
  (declare (special utree-depth))
  (setq *max-substs-depth* nil)
    (if first-order-mode-ms t
      (dolist (son (or (unode-sons root) (list root))
		     (progn
		       (when (eq mating-verbose 'max)
			     (stringdtl)
			     (msg "Returning from unification." t)
			     (finish-output))
		       nil))
	  (when (memq mating-verbose '(med max))
		(incf ctr)
		(msg "Root Node:" 3 "# of Children:" 3
		     (length (unode-sons root)) 3 "Current child:" 3 ctr)
		(stringdtl)
		(msg t)
		(finish-output))
	  (let ((val (find-success-node-rec
		      son (- (or max-search-depth most-positive-fixnum) (unode-depth son)))))
	    (when val
              (setq *least-search-depth* (1+ (unode-depth val))) ;(1+ utree-depth))
              (msgf "The unification tree depth is " *least-search-depth* ".")
	      (setq *max-substs-depth* (ho-check-simpl-count (unode-substs val) t))
              (msgf "The setting for MAX-SUBSTS-VAR is " (car *max-substs-depth*) ".")
              (msgf "The setting for MAX-SUBSTS-PROJ is " (cadr *max-substs-depth*) ".")
              (msgf "The setting for MAX-SUBSTS-PROJ-TOTAL is " (caddr *max-substs-depth*) ".")
	      (when (and (eq unif-trigger 'utree-end) (eq unif-counter-output 10))
		    (print-unif-stats-2))
;	      (when (unode-sons root)
	      (setf (unode-substs root)
		    (ms90-3-find-subs-flexible-pairs
		     (unode-dpairs val) (unode-substs val)))
	      (setf (unode-dpairs root) (unode-dpairs val))
	      (setf (unode-substs val) (unode-substs root))
					;)
	      (return t)))))))
  
(defun find-success-node-rec (unode depth)
  (declare (special utree-depth))
;  (if (and (eq unify-verbose 'MAX) (not (plusp depth))) 
;      (msgf "MAX-SEARCH-DEPTH limit reached. (Unnecessary nodes were generated.)" t))
;the above is really annoying. MB Wed Sep  4 11:46:32 1996
  (when (plusp depth)
     (if (< utree-depth (unode-depth unode))
         (setq utree-depth (unode-depth unode)))
     (let ((simpl (ho-simpl unode)))
        (if simpl
            (if (zerop simpl) unode
                (let ((sons (ho-unif-match unode)))
                  (dolist (new-unode sons nil)
                    (let ((val (find-success-node-rec
                                new-unode (1- depth))))
                      (when val (return val))))))
            nil))))

;;; In the function  ms90-3-find-subs-flexible-pairs, variables should
;;; always be of the form (var . number). This distinguishes the
;;; variables from the new constants that are introduced.

(defun ms90-3-find-subs-flexible-pairs (dpairs subst-stack)
  (let ((basetype-vars nil))
    (dolist (pair dpairs subst-stack)
      (let ((first (ho-unif-lnorm nil (car pair) nil subst-stack nil))
            (second (ho-unif-lnorm nil (cdr pair) nil subst-stack nil)))
	(unless (eq first second)
	  (when (or (lsymbol-q first) (not (numberp (cdr first))))
            (psetq first second second first))
	  (if (and (not (lsymbol-q first)) (numberp (cdr first))
                   (not (ho-free-in first second nil)))
	      (if (lsymbol-q first)
		  (let ((second (ms90-3-head second)))
		    (when (consp second)
		      (push (cons second
				  (eta-longform first (listify-type
                                                       (type second))))
			    subst-stack)))
		  (push (cons first second) subst-stack))
	      (progn (setq first (ms90-3-head first)
			   second (ms90-3-head second))
		     (let* ((type1 (listify-type
                                    (type (if (consp first) (car first)
                                              first))))
			    (resultype (aref type1 (1- (length type1))))
			    (head (cdr (assoc resultype basetype-vars))))
		       (unless head
			 (setq head (ren-var-uni-ho h-var-prefix resultype))
			 (push (cons resultype head) basetype-vars))
		       (when (consp first)
			 (push (cons first (eta-longform head type1))
                               subst-stack))
		       (when (consp second)
			 (push (cons second (eta-longform
                                             head (listify-type
                                                   (type (car second)))))
			       subst-stack))))))))))

(defun ms90-3-head (gwff)
  (cond ((label-q gwff)
	 (apply-label gwff (ms90-3-head gwff)))
	((or (lsymbol-q gwff) (numberp (cdr gwff))) gwff)
	((not-p gwff) (ms90-3-head (cdr gwff)))
	((boundwff-q gwff)
	 (ms90-3-head (cdr gwff)))
	(t (ms90-3-head (car gwff)))))

(defun ms90-3-genhash-sub (node)
  (let ((slot-val nil))
    (dolist (dpair (unode-dpairs node))
	    (let* ((wff1 (ho-unif-lnorm nil (car dpair) nil (unode-substs node) nil))
		   (no1 (ms90-3-unif-hash-fn wff1 2))
		   (wff2 (ho-unif-lnorm nil (cdr dpair) nil (unode-substs node) nil))
		   (no2 (ms90-3-unif-hash-fn wff2 2)))
	      (if (< no1 no2)
		  (push (cons (cons no1 no2) (cons wff1 wff2)) slot-val)
		(push (cons (cons no2 no1) (cons wff2 wff1)) slot-val))))
    (sort slot-val #'unif-lex-hash)))

(defun ms90-3-unif-hash-fn (gwff n)
  (cond ((numberp gwff) n)
	((abbrev-p gwff)
	 (ms90-3-unif-hash-fn (get gwff 'defn) (+ 2 (mod (* 19 n) 65536))))
	((boundwff-q gwff) (ms90-3-unif-hash-fn (cdr gwff) (+ 2 (mod (* (const-val (cdar gwff)) n) 65536))))
	((infix-p gwff) (ms90-3-unif-hash-fn (cdr gwff) (ms90-3-unif-hash-fn (cdar gwff) 
							       (+ 2 (mod (* (const-val (car gwff)) n)
									 65536)))))
	(t (if (consp gwff) 
	       (ms90-3-unif-hash-fn (cdr gwff) (ms90-3-unif-hash-fn (car gwff) (+ 2 (mod (* n n) 65536))))
	     n))))
; all those +2's are there so that we never end up with a 0 or 1...

(defun ms90-3-subsumption-check (next-node ck-subsumed &optional (start-time nil) (time-limit nil))
  (when (and subsumption-check ck-subsumed (option-> subsumption-depth (unode-depth next-node)))
	(let* ((sub-check (ms90-3-genhash-sub next-node))
	       (sub-output (mapcar #'car sub-check)))
	  (when (eq unify-verbose 'max) (msg t "SC" next-node ":" sub-output " compares to " 3))
	  (dolist (elt ck-subsumed)
		  (when (eq unify-verbose 'max) (msg elt " "))
		  (when (and start-time time-limit
			     (>= (/ (- (get-net-internal-run-time) start-time)
				    internal-time-units-per-second)
				 time-limit))
			(return-from ms90-3-subsumption-check nil))
		  (let ((nd-sub (ms90-3-genhash-sub elt)))
		    (if (unif-subsetp sub-check nd-sub)
			(if (ms90-3-canonical-form-check sub-check nd-sub t)
			    (progn
			      (case unify-verbose 
				    ((max med min) (msg "!"))
				    (t ))
			      (return-from ms90-3-subsumption-check t))
			  (progn 
			    (case unify-verbose 
				  ((max med min) (msg "?"))
				  (t ))))
		      (when (eq unify-verbose 'max) (msg " ")))))
	  (when (eq unify-verbose 'max) (msg "sc end" t)))))

(defun ms90-3-canonical-form-check (new-node-list old-node-list old-node)
  ;we need to a) enumerate all possible ways in which old-node can be considered
  ;              a subset of new-node,
  ;           b) produce canonical forms for each of these subsets of new-node.
  ;           c) check whether any of these is the same as the canonical form for old-node.
  (let ((subset-list (enumerate-unif-subsets old-node-list new-node-list)))
    (dolist (poss-subset subset-list nil)
	    ; now we have to work on the cdr of each element, renaming h variables.
	    (when (wff-canon (mapcar #'inc-numbers (car poss-subset)) (mapcar #'inc-numbers (cdr poss-subset)) nil)
		  (return old-node)))))

(defun inc-numbers (wff)
  ;the wff is in ms90-3 form -- i.e. variables of the form (x^2078 . n)
  ;we change this, ad hoc, to a variable x with superscript 2078 repeated n times, interspersed
  ;with 999's. This should distinguish things sufficiently.
  (cond ((abbrev-p wff)
	 (inc-numbers (get wff 'defn)))
	((boundwff-q wff) (cons (car wff) (inc-numbers (cdr wff))))
	((infix-p wff) (cons 
			 (cons (caar wff) (inc-numbers (cdar wff))) 
			 (inc-numbers (cdr wff))))
	(t (if (and (consp wff) (numberp (cdr wff)))
	       (let ((str (princ-to-string wff))
		     (numstr "") (tpstr "") (varstr "") (numstr2 ""))
		 (do ((i 0 (1+ i)))
		     ((= i (1- (length str))))
		     (if (eq (char str i) #\^)
			 (progn (setq varstr tpstr)
				(setq tpstr ""))
		       (if (eq (char str i) #\<)
			   (progn (setq numstr tpstr)
				  (setq tpstr ""))
			 (unless (eq (char str i) #\>)
				 (setq tpstr (concatenate 'string tpstr (princ-to-string (elt str i))))))))
		 ;this writes characters into tpstr, and copies the whole string into numstr or varstr when
		 ;appropriate. It works because vars are all xxx^nnnn<tttt>
		 (do ((j 1 (1+ j)))
		     ((= j (abs (cdr wff))))
		     (setq numstr2 (concatenate 'string numstr2 numstr "999")))
		 (intern (concatenate 'string varstr "^" numstr2 "<" tpstr ">")))
	       (if (consp wff) 
		   (cons (inc-numbers (car wff)) (inc-numbers (cdr wff)))
		 wff)))))

(defun unif-stats3 (root)
  (let ((usl nil)
	(leaves (stats-rec-2 root)))
    (push (/ (reduce '+ (mapcar #'unode-depth leaves)) (length leaves)) usl)
    (push (length leaves) usl)
    (push usl *unif-stats-store*)))

(defun stats-rec-2 (node)
  (let ((leaves nil))
    (if (unode-sons node)
	(progn (dolist (son (unode-sons node))
		       (setq leaves (append leaves (stats-rec-2 son))))
	       leaves)
      (list node))))

(defun print-unif-stats-2 ()
  (let* ((nodes (mapcar #'car *unif-stats-store*))
	 (dleaves (mapcar #'cadr *unif-stats-store*))
	 (c (length nodes)))
    (msgf t "Total number of trees considered: " c ", with " (reduce '+ nodes) " leaves." t)
    (msg "Average number of leaves per tree: " (format nil "~,F" (/ (reduce '+ nodes) c)) t)
    (msg "Average depth of leaves: " (format nil "~,F" (/ (reduce '+ dleaves) c)) t)
    (setq *unif-stats-store* nil)))

