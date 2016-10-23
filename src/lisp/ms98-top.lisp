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

(deffile ms98-top
  (part-of ms98)
  (extension lisp)
  (mhelp "The main functions for MS98-1"))

(defmateop ms98-1
  (mate-alias ccs-invoke)
  (mate-result-> ignore)
  (mhelp "Begin the MS98-1 mating search. 
See Matt Bishop's thesis for details."))

(defun ccs-invoke ()
  (let ((default-ms 'ms98-1))
    (matingsearch-controller)))

(defun init-everything ()
  #+(or sbcl cmu)(setq *daghash* (make-hash-table :test #'equal))
  #-(or sbcl cmu)(setq *daghash* (make-hash-table :test #'equalp)) ;;we really need equalp, but CMUCL doesn't like it. MB
  (setq *using-unifhash* (or (not first-order-mode-ms) ms98-force-h-o) *full-unifhash* (make-hash-table :test #'equal)
	*live-leaves* nil *openfrag-list* nil *dead-fragments* nil *subs-involving* nil *dmark* 0
	*dnode-count* 0 unif-count 0 *banned-list* nil *find-cycle-fn-list* nil
	*ordered-vars* nil *selection-banned* nil *length-list* nil *ccs-rewrites* nil *vaf* 0
	*vafindex* (make-hash-table :test #'equal) *primsubs* t *usc-list* nil
	*extra-sk-vars* nil *sk-below-dups* nil *sk-below-primsubs* nil
	*global-constlist* (parse-into-types (if ms98-unif-hack (find-consts) (cons (getrwff "TRUTH") (find-consts))))
	*allb* nil *successful-sub* nil *rewrite-hash* (make-hash-table :test #'equal)
	*ccs-substs* (make-hash-table :test #'equal) component-count 0 *global-sublist* nil lengths nil 
	*allsub-hash* (make-hash-table :test #'equal) *skolem-matters* (and (null skolem-default) (really-selected))
	*ms98-start-time* (get-net-internal-run-time)
	conn-hash (make-hash-table :test #'equal) *valid-hash* (make-hash-table :test #'equal)
	))

(defun ccs (&key (leib t))
  (declare (special ms90-3-jform component-count current-topnode global-sublist))
  ; mkaminski 10/1/2005
  (setq *active-external-rewrites* global-rewrite-rule-list)
  (setq *external-rewrites* (remove-if-not 'active-p global-rewrite-rule-list))
  (startcount 'mating)
  (setq *saved-first-order-mode-ms* first-order-mode-ms) ; cebrown 10/18/00
  (when (and (or (not first-order-mode-ms) ms98-force-h-o) (null max-substs-var))
    (msgf "You have to set MAX-SUBSTS-VAR.")
    (return-from ccs nil))
  (when (member 'MATING ms98-trace) ; cebrown
    (when (probe-file *ms98-trace-file*)
      (let ((fin (open *ms98-trace-file* :direction :input))
	    (f (open (concatenate 'string *ms98-trace-file* ".old") :direction :output :if-exists :append :if-does-not-exist :create)))
	(do ((l (read-line fin nil 'eof) (read-line fin nil 'eof)))
	    ((eq l 'eof))
	  (format f "~d~%" l))
	(close fin)
	(close f)
	(delete-file *ms98-trace-file*)))
    (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		    (format t "Beginning Search")
		    (stringdtl)))
  (setq *primsubs-for-ms98* nil)
  (setq *pr00-setsubs* nil)
  (setq *ccs-constrained-solns* nil)
  (setq *ccs-constrained-solns-prune* nil)
  (if (equal ms98-init 1) (ms98-dup)
    (if (equal ms98-init 2) (ms98-prim)
      (when (equal ms98-init 3) (ms98-make-primlist))))
  (when (and (or ms98-verbose options-verbose)
	     (not ms98-trace)
	     (eq PRIMSUB-METHOD 'PR00))
    (msgf "Generated " (length *pr00-setsubs*) " Set Substitutions"))
  (setq MEASUREMENTS NIL)
  (setq current-topnode (eproof-etree current-eproof))
  (when delay-setvars
    (setq ms98-current-ftree (etree-to-ftree current-topnode)))
  (setq ms90-3-jform (init-position (etree-to-jform (eproof-etree current-eproof)))) ; cebrown - 10/10/00 - added this so find-consts could use it
  (setq ms98-full-jform nil)
  (init-everything)
  (when *skolem-matters* 
    (setq *allb* (remove-if-not #'cdr 
				(mapcar #'helper-function-1
					(remove-if-not #'exp-var-p 
						       (mapcar #'car (eproof-free-vars-in-etree current-eproof)))))))
  (unless leib (setq *leibniz-var-list* nil))
  (deepen-to-literals (etree-name (eproof-etree current-eproof)))
  (setq current-topnode (eproof-etree current-eproof))
  (setq ms90-3-jform (init-position (etree-to-jform
				     (if (eproof-lemmas current-eproof)
					 (cadr (etree-components (eproof-etree current-eproof)))
				       (eproof-etree current-eproof)))))
  (pathnum-init-jform ms90-3-jform)
  (remove-sk-labels ms90-3-jform)
  (mark-ext-exp-nodes ms90-3-jform)
  (setren-counter h-var-prefix 0)
  (setren-counter w-var-prefix 0)
  (setq neg-h-var-list nil imitation-h-var-list nil)
  (update-measurement 'NUM-LITERALS (length (find-etree-nodes #'leaf-p current-topnode nil t)))
  (update-measurement 'NUM-VPATHS (number-of-vertical-paths-main ms90-3-jform))
  (update-measurement 'NUM-HPATHS (number-of-horizontal-paths-main ms90-3-jform))
  (let ((ms98-result
  (if (eq 'disjunction (jform-type ms90-3-jform))
      (let ((count 0)
	    connlist parent)
	(msgf "Splitting into " (length (disjunction-components ms90-3-jform)) " disjuncts" t)
	(setq *rewrite-hash* (make-hash-table :test #'equal) *ccs-rewrites* nil)
	(dolist (d (disjunction-components ms90-3-jform))
		(msgf "Disjunct #" (incf count) t)
		(let* ((etr1 (find-etree-node #'helper-function-2))
		       (etree (nth (1- count) (etree-components etr1)))
		       (exp-nodes (mapcar #'etree-name (find-outer-etree-nodes #'expansion-p etree)))
		       temp)
		  (do ((c 0 (1+ c)))
		      ((= c num-of-dups) (setq exp-nodes (reduce #'append temp)))
		      (push exp-nodes temp))
		  (setq ms90-3-jform d)
		  (setq current-topnode etree *openfrag-list* nil)
		  (setq parent (jform-parent d))
		  (setf (jform-parent d) nil)
		  (setq *global-constlist* (parse-into-types (if ms98-unif-hack (find-consts)
							       (cons (getrwff "TRUTH") (find-consts)))))
		  (when ms98-rewrites 
		    (get-ccs-rewrites))
		  (when (and (not *ccs-rewrites*) *using-unifhash*) (unify-all-var-pairs))
		  (setq *ordered-vars* (order-vars))
		  (setq *depth-hash* (make-array (list (length *ordered-vars*) *arbitrary-const*)
						 :element-type 'hash-table))
		  (when *ccs-rewrites* (maphash #'prune-rewrites-2 *full-unifhash*))
		  (ms98-print-primsubs etree)
		  (do ((clist (catch 'stop (ccs-real)) (catch 'stop (ccs-real))))
		      ((or (eq clist 'fail)
			   (and (eq clist 'next-constrained-soln)
				(not *ccs-constrained-solns*))
			   (and (eq clist 'duplicate) 
				(null exp-nodes))
			   (and (eq clist 'next-prim)
				(null *primsubs-for-ms98*)
				(null *pr00-setsubs*))
			   (and (consp clist) (consp (car clist))))
		       (setf (jform-parent d) parent)
		       (when (memq clist '(duplicate fail next-prim))
			 (msgf "Failed." t) (return-from ccs nil))
		       (msgf "Solved disjunct #" count t "Mating: " clist t t)
		       (if *ccs-rewrites* (push (expand-by-rewriting clist) connlist) 
			 (push clist connlist))
		       )
		      (when (eq clist 'next-constrained-soln)
			(msgf "********* RESUMING AFTER SOLVING SOME CONSTRAINTS IN DISJUNCT #" count t)
			(unless (ccs-next-constrained-soln-init)
			  (msgf "Failed." t) (return-from ccs nil)))
		      (when (eq clist 'delay-setvars)
			(setq ms98-setvars (setvars-in-jform ms90-3-jform))
			(msgf "********* DELAYING SETVARS " ms98-setvars " IN DISJUNCT #" count t)
			(setq ms98-full-jform ms90-3-jform *openfrag-list* nil)
			(setq ms90-3-jform (remove-flex-lits-from-jform
					    ms90-3-jform ms98-setvars)))
		      (when (eq clist 'duplicate)
			(msgf "********* DUPLICATING QUANTIFIER " (car exp-nodes) " IN DISJUNCT #" count t)
			(duplicate-all-outer-vars (find-etree-node-name (pop exp-nodes)))
			(setf (nth (1- count) (etree-components etr1)) (deepen-to-literals (etree-name etree)))
			(setq ms90-3-jform (init-position (etree-to-jform etree)))
			(pathnum-init-jform ms90-3-jform)
			(remove-sk-labels ms90-3-jform)
			(unless leib (setq *leibniz-var-list* nil))
			(mark-ext-exp-nodes ms90-3-jform)
			(when delay-setvars
			  (setq ms98-current-ftree (etree-to-ftree etree))
			  (setq ms98-full-jform ms90-3-jform)))
		      (when (eq clist 'next-prim)
			(init-everything)
			(msgf "********* CHANGING PRIMSUBS IN DISJUNCT #" count t)
			(hack-primsubs-about)
			(setf (eproof-free-vars-in-etree current-eproof)
			      (remove-if-not 
			       #'(lambda (x) (and 
					      (find-etree-node-name (etree-name (cdr x)) (eproof-etree current-eproof))
					      (intersection (free-vars-of (strip-exp-vars (get-deep (eproof-etree current-eproof))))
							    (free-vars-of (strip-exp-vars (car x))))))
			       (eproof-free-vars-in-etree current-eproof)))
			(ms98-print-primsubs etree)
			(setq ms90-3-jform (init-position (etree-to-jform etree)) 
			      *ms98-start-time* (get-net-internal-run-time))
			(pathnum-init-jform ms90-3-jform)
			(remove-sk-labels ms90-3-jform)
			(mark-ext-exp-nodes ms90-3-jform)
			(when delay-setvars
			  (setq ms98-full-jform ms90-3-jform))
			(setq *global-constlist* (parse-into-types (if ms98-unif-hack (find-consts)
								     (cons (getrwff "TRUTH") (find-consts)))))
			(setq *skolem-matters* (and (null skolem-default) (really-selected)))
			(when *skolem-matters* 
			  (setq *allb* 
				(remove-if-not #'cdr 
					       (mapcar #'helper-function-1
						       (remove-if-not #'exp-var-p 
								      (mapcar #'car 
									      (eproof-free-vars-in-etree current-eproof)))))))
			(unless leib (setq *leibniz-var-list* nil))
			(when ms98-rewrites 
			  (get-ccs-rewrites))
			(when (and (not *ccs-rewrites*) *using-unifhash*) (unify-all-var-pairs))
			(setq *ordered-vars* (order-vars))
			(setq *depth-hash* (make-array (list (length *ordered-vars*) *arbitrary-const*)
						       :element-type 'hash-table))
			(when *ccs-rewrites* (maphash #'prune-rewrites-2 *full-unifhash*))
			)		      
		      (unless (eq clist 'next-prim)
			(setq *ordered-vars* (order-vars))
			(setq *depth-hash* (make-array (list (length *ordered-vars*) *arbitrary-const*)
						       :element-type 'hash-table)))
		      )))
	(msgf "Success! The following is a complete mating:" t (reduce #'append connlist) t)
	(setq current-topnode (eproof-etree current-eproof))
	(finish-up-ccs (reduce #'append connlist)))
    (let ((exp-nodes (mapcar #'etree-name (find-outer-etree-nodes #'expansion-p)))
	  temp)
      (do ((c 0 (1+ c)))
	  ((= c num-of-dups) (setq exp-nodes (reduce #'append temp)))
	  (push exp-nodes temp))
      (when ms98-rewrites 
	(setq *rewrite-hash* (make-hash-table :test #'equal) *ccs-rewrites* nil)
	(get-ccs-rewrites))
      (when (and (not *ccs-rewrites*) *using-unifhash*) (unify-all-var-pairs))
      (setq *ordered-vars* (order-vars))
      (setq *depth-hash* (make-array (list (length *ordered-vars*) *arbitrary-const*)
				     :element-type 'hash-table))
      (when *ccs-rewrites* (maphash #'prune-rewrites-2 *full-unifhash*))
      (ms98-print-primsubs (eproof-etree current-eproof))
      (do ((clist (catch 'stop (ccs-real)) (catch 'stop (ccs-real))))
	  ((or (eq clist 'fail)
	       (and (eq clist 'next-constrained-soln)
		    (not *ccs-constrained-solns*))
	       (and (eq clist 'duplicate)
		    (null exp-nodes))
	       (and (eq clist 'next-prim)
		    (null *primsubs-for-ms98*)
		    (null *pr00-setsubs*))
	       (null clist) ; in case empty mating works - cebrown 10/22/01
	       (and (consp clist) (consp (car clist))))
	   (when (memq clist '(duplicate fail next-prim)) 
	     (msgf "Failed." t) (return-from ccs nil))
	   (when *ccs-rewrites* (setq clist (expand-by-rewriting clist)))
	   (msgf "Success! The following is a complete mating:" t clist t)
	   (finish-up-ccs clist))
	  (when (eq clist 'next-constrained-soln)
	    (msgf "********* RESUMING AFTER SOLVING SOME CONSTRAINTS " t)
	    (unless (ccs-next-constrained-soln-init)
	      (msgf "Failed." t) (return-from ccs nil)))
	  (when (eq clist 'delay-setvars)
	    (setq ms98-setvars (setvars-in-jform ms90-3-jform))
	    (msgf "********* DELAYING SETVARS " ms98-setvars t)
	    (setq ms98-full-jform ms90-3-jform *openfrag-list* nil)
	    (setq ms90-3-jform (remove-flex-lits-from-jform
				ms90-3-jform ms98-setvars)))
	  (when (eq clist 'duplicate)
		(msgf "********* DUPLICATING QUANTIFIER " (car exp-nodes) t)
		(when ms98-trace
		  (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		    (stringdtl)
		    (msgf "********* DUPLICATING QUANTIFIER " (car exp-nodes) t)))
		(duplicate-all-outer-vars (find-etree-node-name (pop exp-nodes)))
		(setf (eproof-etree current-eproof) (deepen-to-literals (etree-name (eproof-etree current-eproof))))
		(setq ms90-3-jform (init-position
				    (etree-to-jform
				     (if (eproof-lemmas current-eproof)
					 (cadr (etree-components (eproof-etree
								  current-eproof)))
				       (eproof-etree current-eproof)))))
		(pathnum-init-jform ms90-3-jform)
		(remove-sk-labels ms90-3-jform)
		(unless leib (setq *leibniz-var-list* nil))
		(mark-ext-exp-nodes ms90-3-jform)
		(when delay-setvars
		  (setq ms98-current-ftree (etree-to-ftree (eproof-etree current-eproof)))
		  (setq ms98-full-jform ms90-3-jform)))
	  (when (eq clist 'next-prim)
	    (msgf "********* CHANGING PRIMSUBS " t)
	    (when ms98-trace
	      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		(stringdtl)
		(msgf "********* CHANGING PRIMSUBS " t)))
	    (init-everything)
	    (hack-primsubs-about)
	    (setf (eproof-free-vars-in-etree current-eproof)
		  (remove-if-not 
		   #'(lambda (x) (and 
				  (find-etree-node-name (etree-name (cdr x)) (eproof-etree current-eproof))
				  (intersection (free-vars-of (strip-exp-vars (get-deep (eproof-etree current-eproof))))
						(free-vars-of (strip-exp-vars (car x))))))
		   (eproof-free-vars-in-etree current-eproof)))
	    (setq ms90-3-jform (init-position
				(etree-to-jform
				 (if (eproof-lemmas current-eproof)
				     (cadr (etree-components (eproof-etree current-eproof)))
				   (eproof-etree current-eproof))))
		  *ms98-start-time* (get-net-internal-run-time))
	    (ms98-print-primsubs (eproof-etree current-eproof))
	    (pathnum-init-jform ms90-3-jform)
	    (remove-sk-labels ms90-3-jform)
	    (setq *global-constlist* (parse-into-types (if ms98-unif-hack (find-consts)
							 (cons (getrwff "TRUTH") (find-consts)))))
	    (setq *skolem-matters* (and (null skolem-default) (really-selected)))
;	    (when *skolem-matters* 
;	      (setq *allb* 
;		    (remove-if-not #'cdr 
;				   (mapcar #'helper-function-1
;					   (remove-if-not #'exp-var-p 
;							  (mapcar #'car 
					;								  (eproof-free-vars-in-etree current-eproof)))))))
	    (when *skolem-matters*	; cebrown - 7/26/00 - changed this to set *allb* to include banned selected for setsubs too
	      (setq *allb* (eproof-all-banned current-eproof)))
	    (unless leib (setq *leibniz-var-list* nil))
	    (mark-ext-exp-nodes ms90-3-jform)
	    (when delay-setvars
	      (setq ms98-full-jform ms90-3-jform))
	    (when ms98-rewrites 
	      (get-ccs-rewrites))
	    (when (and (not *ccs-rewrites*) *using-unifhash*) (unify-all-var-pairs))
	    (setq *ordered-vars* (order-vars))
	    (setq *depth-hash* (make-array (list (length *ordered-vars*) *arbitrary-const*)
					   :element-type 'hash-table))
	    (when *ccs-rewrites* (maphash #'prune-rewrites-2 *full-unifhash*))
	    )		      
	  (unless (eq clist 'next-prim)
	    (setq *ordered-vars* (order-vars))
	    (setq *depth-hash* (make-array (list (length *ordered-vars*) *arbitrary-const*)
					   :element-type 'hash-table)))
	  )))))
    ; mkaminski 10/1/2005
    (unless ms98-pollute-global-rewrites
      (deactivate-rules global-rewrite-rule-list)
      (setq global-rewrite-rule-list *external-rewrites*)
      (activate-rules *active-external-rewrites*))
    ms98-result))

(defun ccs-real ()
  (declare (special *global-constlist* primehash1 primehash2 dphash disj-assoc pairhash))
  (unless (boundp 'given-clist)
    (setq given-clist nil))
  (when (and (disjunction-p ms90-3-jform) ; cebrown 10/9/01
	     (not (disjunction-components ms90-3-jform))
	     ms98-full-jform (neq ms90-3-jform ms98-full-jform))
    (ccs-solve-remaining-constraints-1 ms98-current-ftree
				       (eproof-dissolve current-eproof)
				       (eproof-lemmas current-eproof)
				       ms98-setvars
				       ms98-full-jform))
  (when ms98-trace  ; cebrown 2/21/00
    (let ((*ignore-statuses* T))
      (declare (special *ignore-statuses*))
      (when (and background-eproof
		 (eproof-p background-eproof)
		 (etree-p (eproof-etree background-eproof))
		 (wffeq-ab (get-shallow (eproof-etree background-eproof))
			   (get-shallow (eproof-etree current-eproof))))
	(msgf "Transferring mating information from background eproof"
	      background-eproof)
	(setq given-clist (map-eproof background-eproof))))
    (when active-mating
      (let ((conn-array (connections-array)))
	(dolist (conn (mating-clist active-mating))
		(let ((color (intern-str (create-namestring 'COLOR)))
		      (conn-nodes nil)
		      (p (gethash conn conn-array)))
		  (when p
		    (let* ((n1 (literal-name (caar p)))
			   (n2 (literal-name (cdar p)))
			   (e1 (find-etree-node-name n1))
			   (e2 (find-etree-node-name n2)))
		      (when (and e1 e2)
			(setq conn-nodes 
			      (append (find-etree-nodes #'(lambda (e)
							    (declare (ignore e))
							    t) e1)
				      (find-etree-nodes #'(lambda (e)
							    (declare (ignore e))
							    t) e2))))))
		  (setq given-clist (acons color conn-nodes given-clist))))))
    (msgf "COLORS:" t)
    (dolist (colored-nodes given-clist)
	    (msgf (car colored-nodes) " -")
	    (dolist (n (cdr colored-nodes))
		    (when (allow-node-in-jform n)
		      (msg " " n))))
    (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		    (msgf "COLORS:" t)
		    (dolist (colored-nodes given-clist)
			    (msgf (car colored-nodes) " -")
			    (dolist (n (cdr colored-nodes))
				    (when (allow-node-in-jform n)
				      (msg " " n))))))
;  (let ((colors nil)) ; 10/3/01, cebrown
;    (declare (special colors))
;    (color-expansion-tree current-topnode current-topnode)
  (when (eq query-user 'query-jforms)
    (display-vp-diag ms90-3-jform)
    (when (or *primsubs-for-ms98* *pr00-setsubs*) (ms98-print-primsubs current-topnode))
    (unless (query "Search on this jform?" 'yes)
      (if (and delay-setvars (setvars-in-jform ms90-3-jform))
	  (throw 'stop 'delay-setvars)
	(if *ccs-constrained-solns*
	    (throw 'stop 'next-constrained-soln)
	  (if (or *primsubs-for-ms98* *pr00-setsubs*)
	      (throw 'stop 'next-prim)
	    (throw 'stop 'duplicate))))))
  (setq primehash1 (make-hash-table :test #'equal) primehash2 (make-hash-table :test #'equal)
	dphash (make-hash-table :test #'equal)
	pairhash (make-hash-table :test #'equal) disj-assoc (fiddle-disjs (ccs-reorganize))
	*length-list* nil lengths nil dup-priority nil 
	*positive-leaves* (mapcar #'leaf-name (find-etree-nodes #'helper-function-3))
	*negative-leaves* (mapcar #'leaf-name (find-etree-nodes #'helper-function-4)))
  (let ((litlist (reverse (jform-to-literal-list ms90-3-jform nil)))
	(fvlist (append (mapcar #'helper-function-5 (free-vars-in-etree (eproof-etree current-eproof))) ; free-vars-in-etree doesn't use it's argument - cebrown - 9/27/00
			'((var . -1))))
	(dp2 (recursively-bunch (bunch-together disj-assoc)))
	(mate-ffpair t)
	(dup-arrangements nil)
	(ccs-components nil)
	(ccount 0))
    (setq *global-constlist* (parse-into-types (if ms98-unif-hack (find-consts) (cons (getrwff "TRUTH") (find-consts)))))
    (setq *temp-dups* (remove-if-not #'cdr dp2))
    (when *ccs-rewrites* (setq litlist (remove-if-not #'helper-function-6 litlist)))
    (setq dup-priority (get-priority disj-assoc (build-duplicate-reln (eproof-etree current-eproof))))
    (setq *single-lit-fragments* (mapcar #'car (remove-if-not #'helper-function-7 disj-assoc)))
    (setq *single-hpath-fragments* 
	  (mapcar #'car (remove-if-not #'helper-function-8 disj-assoc)))
    (setq *complete-weights* (cons 1 (mapcar #'helper-function-9 disj-assoc)))
    ;;temporary value for *complete-weights*, until we can compute the permanent value...
    (setq *prim-lookup* (mapcar #'helper-function-10 (get-prims-below disj-assoc))
	  *primsubs* (remove-if #'null *prim-lookup*))
    (setq fragjform (make-frag-jform ms90-3-jform))
    (setq dup-arrangements (darrange dup-priority))
    (setq *extra-sk-vars* (sk-extract-dups disj-assoc))
    (setq *ordered-vars* (order-vars-2 dup-arrangements))
    (setq litlist (sort litlist #'(lambda (x y) (< (cdaddr (assoc x dup-arrangements)) (cdaddr (assoc y dup-arrangements))))))
    (msgf "Checking " (/ (* (length litlist) (1- (length litlist))) 2) " potential connections." t)
    (do ((l (cdr litlist) (cdr l))
	 (curr (car litlist) (car l))
	 (anc nil nil))
	((null curr))
	(setq anc (find-junctive-ancestors curr))
	(dolist (lit2 l)
	  (if (and
	       (or (not (member 'MATING-FILTER ms98-trace)) ; cebrown 4/7/00
		   (colored-connection (cons lit2 curr) given-clist))
	       (plausible-conn lit2 curr *temp-dups*) (ccs-mateable lit2 anc))
	      (let ((x (assoc curr dup-arrangements))
		    (y (assoc lit2 dup-arrangements))
		    real-x real-y real-sub)
		(incf ccount)
		(when ms98-verbose (msgf curr " " lit2 ))
		(when (and *using-unifhash* (not *skolem-matters*) (< ms98-variable-order 9))
		  ;;x will be of the form (curr orig (conj . copy#) substitution to get here from copy#0)
		  (if (= (caaddr x) (caaddr y)) ;they're in the same original conjunct
		      (if (and (= (cdaddr x) 0) (= (cdaddr y) 0))
			  (setq real-x nil real-y nil real-sub nil) ;we haven't done this yet.
			(if (= (cdaddr x) (cdaddr y)) ;if they're in the same original & duplicate conjuncts..
			    (setq real-x (cadr x) real-y (cadr y)
				  real-sub (get-sub-for-dup (append (cdddr x) (cdddr y)))) ;move it all back to 0
					;otherwise they're in different duplicate conjuncts but the same original
			  (if (and (= (cdaddr x) 0) (= (cdaddr y) 1)) ;this is the first time
			      (setq real-x nil real-y nil real-sub nil)
			    (progn (setq real-x (cadr x) ;move x back to 0
					 real-y ;find copy 1 of y
					 (car (remove-if-not #'(lambda (p) (and (eq (cadr p) (cadr y))
										(= (cdaddr p) 1)))
							     dup-arrangements)))
				   (setq real-sub (get-sub-for-dup (append (cdddr x) 
									   (compose-ccs-subs (cdddr real-y)
											     (cdddr y)))))
				   (setq real-y (car real-y))))))
					;otherwise they're in different originals
		    (if (and (= (cdaddr x) 0) (= (cdaddr y) 0))
			(setq real-x nil real-y nil real-sub nil) ;we haven't done this yet.
		      (setq real-x (cadr x) real-y (cadr y)
			    real-sub (get-sub-for-dup (append (cdddr x) (cdddr y)))))))
		(when *skolem-matters* ;;then we're stuffed; do everything separately.
		  (setq real-x nil))
		(if real-x 
		    (let ((realcomponent 
			   (car (remove-if-not #'(lambda (x) (or (and (eq (caar (component-clist x)) real-x)
								      (eq (cdar (component-clist x)) real-y))
								 (and (eq (caar (component-clist x)) real-y)
								      (eq (cdar (component-clist x)) real-x))))
					       ccs-components))))
		      (if ms98-verbose
			  (msg " is a copy of " real-x " " real-y " " real-sub (if realcomponent " (OK)" " (fail)"))
			(msg "*"))
		      (when realcomponent
			(let ((sfc (sub-for-component realcomponent real-sub curr lit2 disj-assoc)))
			  (when (and sfc
				     (or (not (member 'MATING-FILTER ms98-trace))
					 (component-good sfc)))
			    (push sfc
				  ccs-components)))))
		  (progn
		    (when ms98-trace
		      (stringdtl)
		      (msgf "Trying to Unify " curr " with " lit2)
		      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
				      (msgf "---------BEGIN---------")
				      (stringdtl)
				      (msgf "Trying to Unify " curr " with " lit2)))
		  (let ((newcomponent (qum (list (make-new-dpair (cons curr lit2) fvlist))
					   (list (cons curr lit2)) (find-touches (list curr lit2)) 0 0)))
		    (when ms98-trace
		      (stringdtl)
		      (msgf "Unification finished:" curr " with " lit2)
		      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
			(stringdtl)
			(msgf "Unification finished:" curr " with " lit2)
			(msgf "----------END-----------" t)))
		    (if ms98-verbose
			(msg " is new " (if newcomponent "(OK)" "(fail)")) (msg "+"))
		    (when newcomponent 
		      (push newcomponent ccs-components))))))
		(msg "-"))))
    (dolist (c ccs-components) (setf (gethash (car (component-clist c)) dphash)
				     (make-new-dpair (car (component-clist c)) fvlist))
	    (setf (gethash (car (component-clist c)) conn-hash) c))
    (mapcar #'(lambda (x) (setf (component-ddf x) (does-dups-first-2 (component-blocks x) (component-clist x))))
	    ccs-components)
    (let ((plist (make-primes (max (length litlist) (length ccs-components)))))
      (dolist (l (mapcar #'cons plist litlist))
	      (setf (gethash (cdr l) primehash2) (car l)))
      (dolist (l (mapcar #'cons plist ccs-components))
	      (setf (component-key (cdr l)) (car l))
	      (setf (component-litkey (cdr l)) (make-litkey (component-lits (cdr l))))
	      (when (and (consp (component-openpath (cdr l)))
			 (not (eq (car (component-openpath (cdr l))) 'OR)))
		(setf (component-openpath (cdr l)) (make-litkey-by-name (component-openpath (cdr l)))))
	      (setf (gethash (car (component-clist (cdr l))) primehash1) (car l))
	      (insert-in-cgraph-2 (car l) (car (component-clist (cdr l))))))
    (msgf "There are " (length ccs-components) " acceptable connections (out of " ccount ")." t)
    (update-measurement 'ACCEPTABLE-CONNECTIONS ccount)
    (when (member 'MATING ms98-trace)
      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
	(stringdtl)
	(msgf "There are " (length ccs-components) " acceptable connections (out of " ccount ")." t)))
    (push (cons 'fail (length ccs-components)) *length-list*)
    (dolist (c (remove-if #'helper-function-12 ccs-components))
	    (when (ccs-complete-p (component-clist c) c) 
	      (when *ccs-rewrites* (setq *successful-sub* 
					 (append (extract-sub-from (component-final-subs c)) *successful-sub*)))
	      (if (or (not ms98-full-jform) (eq ms98-full-jform ms90-3-jform))
		  (throw 'stop (component-clist c))
		(ccs-solve-remaining-constraints c))))
    (when (null ccs-components) 
      (if (and delay-setvars (setvars-in-jform ms90-3-jform))
	  (throw 'stop 'delay-setvars)
	(if *ccs-constrained-solns*
	    (throw 'stop 'next-constrained-soln)
	  (if (= ms98-init 3)
	      (throw 'stop 'next-prim)
	    (throw 'stop 'duplicate)))))
    (when (and max-search-limit
	       (>= (/ (- (get-net-internal-run-time) *ms98-start-time*) 
		      internal-time-units-per-second)
		   max-search-limit))
      (cond ((= ms98-init 3) (throw 'stop 'next-prim))
	    ((and delay-setvars (setvars-in-jform ms90-3-jform))
	     (throw 'stop 'delay-setvars))
	    (*ccs-constrained-solns*
	     (throw 'stop 'next-constrained-soln))))
    (case DEFAULT-MS
      (MS98-1
       (ccs2 ccs-components nil)))))

(defun ccs2 (ccs-components ccs-components2 &optional (ccs-orig2 nil))
  (declare (special *global-constlist* primehash1 primehash2 pairhash dphash disj-assoc))
  (let (returnlist temp-qc dp2)
    (declare (special returnlist))
    (setq ccs-original ccs-components)
    (when ms98-verbose (msgf "Components: " t)
	  (dolist (c ccs-components) (msg (parse-clist (component-clist c)) " "))
	  (msgf "-----------" t))
    (setq dp2 (recursively-bunch (bunch-together disj-assoc)))
    (setq ccs-components (remove-if #'(lambda (x) (too-many-dups (component-touches x) dp2)) ccs-components))
    (setq ccs-original ccs-components)
    (dolist (d disj-assoc)		;d is (n (literals) width fragment)
	  (let ((mlist (enumerate-all-matings (cadddr d) ccs-components)))
	    (when (or ms98-num-of-dups ms98-max-prims)
	      (setq mlist (remove-if #'(lambda (x) (too-many-dups x dp2))
				     mlist :key #'component-touches)))
	    (when (numberp max-mates)
	      (setq mlist (remove-if #'helper-function-13 mlist)))
	    (push (cons (car d) (length mlist)) *length-list*)
	    ;;note: calculate length *before* doing subsumptions, o/w we may get "0 mates" and discard fragment!
	    (msgf "Fragment " (car d) ":" (cadddr d) " has " (length mlist) " atomic (initial) components." t)
	    (when (member 'MATING ms98-trace)
	      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		(msgf "-BEGIN- Fragment " (car d) ":" (cadddr d) " has " (length mlist) " atomic (initial) components. -END-" t)))
	    (setq mlist (remove-if #'helper-function-14 mlist)) ;;discard anything which is obviously subsumed.
	    (setq temp-qc (append mlist temp-qc))
	    (when ms98-low-memory
	      (incf *dmark*)
	      (mapcar #'mark-dag ccs-original)
	      (mapcar #'mark-dag temp-qc)
	      (mapcar #'mark-dag ccs-components)
	      (mapcar #'mark-dag ccs-components2)
	      (mapcar #'mark-dag ccs-orig2)
	      (prune-unmarked-array))
	    (when (and max-search-limit
		       (>= (/ (- (get-net-internal-run-time) *ms98-start-time*) 
			      internal-time-units-per-second)
			   max-search-limit))
	    (cond ((= ms98-init 3) (throw 'stop 'next-prim))
		  ((and delay-setvars (setvars-in-jform ms90-3-jform))
		   (throw 'stop 'delay-setvars))
		  (*ccs-constrained-solns*
		   (throw 'stop 'next-constrained-soln))))
	    ))
    (dolist (d (mapcar #'car disj-assoc)) ;;for each fragment...
      (push (cons d (ccs-length-measure (remove-if-not #'(lambda (x) (memq d (component-blocks x))) temp-qc)))
	    lengths)) ;;..find all the blocking components and measure them.
    (do ((did-something (rassoc 0 lengths) (rassoc 0 lengths))
	 (fragments (mapcar #'car lengths) (mapcar #'car lengths)))
	((null did-something))
      (progn
	(dolist (l lengths)
	  (when (eq (cdr l) 0) ;;if nothing blocks this component, then nothing should touch it either
	    (setq fragments (delete (car l) fragments))
	    (setq temp-qc (remove-if #'(lambda (x) (memq (car l) (component-touches x))) temp-qc))))
	(setq lengths nil)
	(dolist (d fragments) ;;for each fragment...
	  (push (cons d (ccs-length-measure (remove-if-not #'(lambda (x) (memq d (component-blocks x))) temp-qc)))
		lengths)))) ;;..find all the blocking components and measure them.
  (setq lengths (ccs-length-measure-2 (mapcar #'car lengths) temp-qc))
  (setq lengths (sort (remove-if #'helper-function-15 dup-priority)
		      #'helper-function-16))
  (setq dup-priority dp2)
  (setq *temp-dups* (remove-if-not #'cdr dup-priority))
  (do ((reall (mapcar #'helper-function-10 lengths) (remove-if #'null (mapcar #'cdr reall)))
       temp)
      ((null reall) (setq lengths (reduce #'append (nreverse temp))))
    (setq temp (push (mapcar #'car reall) temp)))
  (msgf "Lengths : " lengths t)
  (setq ccs-components (remove-if-not #'helper-function-17 temp-qc))
  (setq ccs-original (remove-if-not #'helper-function-17 ccs-original))
  (do ((tempccs nil)
       (templ nil))
      ((or tempccs (null lengths))
       (setq lengths (cons  (car lengths) (append (nreverse templ) (cdr lengths))))
       (setq ccs-components2 (setdiff ccs-components tempccs))
       (setq ccs-components (sort tempccs #'> :key #'component-measure)))
    (setq tempccs (remove-if-not #'helper-function-18 ccs-components))
    (unless tempccs (msgf "All components for fragment " (car lengths) " do dups first."))
    (when *using-unifhash* (mapcar #'(lambda (x) (get-final-subs-for x ccs-original (component-touches x))) tempccs))
    (setq tempccs (remove-if #'helper-function-19 tempccs))
    (unless tempccs (msgf "All components for fragment " (car lengths) " that don't do dups first, don't unify."))
    (unless tempccs (push (car lengths) templ) (setq lengths (cdr lengths))
	    (msgf "Lengths : " lengths t)
	    ))
  (when (null ccs-components)
      (if (and delay-setvars (setvars-in-jform ms90-3-jform))
	  (throw 'stop 'delay-setvars)
	(if *ccs-constrained-solns*
	    (throw 'stop 'next-constrained-soln)
	  (if (= ms98-init 3)
	      (throw 'stop 'next-prim)
	    (throw 'stop 'duplicate)))))
  (let* ((len (component-measure (car ccs-components)))
	 (ccs1 (remove-if-not #'(lambda (x) (= len (component-measure x))) ccs-components))
	 (ccs2 (remove-if #'(lambda (x) (= len (component-measure x))) ccs-components)))
    (setq ccs-components ccs1)
    (setq ccs-orig2 (cons (length ccs-components2) (arrange-components (append ccs-components ccs-components2)
								       (reduce #'max lengths))))
    (setq ccs-components2 ccs2))
  (setq *complete-weights* (mapcar #'length (cdr ccs-orig2)) *comps-tried* (make-hash-table :test #'eql))
  (setq ccso (mapcar #'helper-function-20 ccs-original))
  (msgf "Now merging " (length ccs-components) " current and " 
	(car ccs-orig2) " atomic components, with " (length ccs-components2) " stored."  t)
  (when ms98-trace
    (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		    (msgf "---------BEGIN---------")
		    (stringdtl)
      (msgf "Now merging " (length ccs-components) " current and " 
	    (car ccs-orig2) " atomic components, with " (length ccs-components2) " stored."  t)
      (msgf "Current Components: " (mapcar #'(lambda (c) (component-good-name c)) ccs-components))
      (msgf "Stored Components: " (mapcar #'(lambda (c) (component-good-name c)) ccs-components2)) 
      (print-component-info ccs-components)
      (msgf "---------END---------" t)))
  (ccs-larger ccs-components ccs-components2 lengths (cdr ccs-orig2) (car ccs-orig2))))

(defun arrange-components (complist maxcomp)
  (do ((newlist (list nil)) ;;0th fragment does not exist & so has no components blocking it!
       (count 1 (1+ count)))
      ((> count maxcomp) (nreverse newlist))
    (push (remove-duplicates (remove-if-not #'(lambda (x) (member count (component-blocks x) :test #'eql)) complist) :key #'component-key) newlist)))

(defun ccs-larger (ccs-components ccs-components2 lengths ccs-orig2 minor)
  (declare (special *global-constlist* primehash1 primehash2 dphash disj-assoc))
  (when ms98-low-memory (incf *dmark*)
	(mapcar #'mark-dag ccs-components)
	(mapcar #'mark-dag ccs-original)
	(dolist (c ccs-orig2) (mapcar #'mark-dag c))
	(mapcar #'mark-dag ccs-components2)
	(prune-unmarked-array))
  (let (temp-qc temp-qc1 failed key litkey touches clist touches-single gcd comp2list blocking will-never-work
		max-weight-so-far mm-toobig-list mating lits ctried mm-mark *ban-me* newtouches)
    (do ((l (cdr ccs-components) (cdr l))
	 (comp1 (car ccs-components) (car l)))
	((null comp1))
      (handle-user-interrupt nil nil nil nil (cons t (component-clist comp1)))
      (setq key (component-key comp1) clist (component-clist comp1)
	    touches (ordered-setdiff (component-touches comp1) (component-blocks comp1))
	    litkey (component-litkey comp1) temp-qc1 nil failed nil
	    ctried (component-tried comp1) mm-mark nil)
      (when (or (member 'ALL-COMPONENTS ms98-trace)
		(and (component-good comp1)
		     (member 'MATING ms98-trace)))
	(with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
	  (msgf "-----------BEGIN----------")
	  (if (component-good comp1)
	      (msgf "Considering good component: " (component-name comp1))
	    (msgf "Considering component: " (component-name comp1)))
	  (msgf "------------END-----------")))
	(when (and (null touches)
		   (eq (car (component-openpath comp1)) 'OR)
		   (> (length (component-openpath comp1)) 2)) ;;i.e. we now have to block multiple fragments.
	  (let ((newcomp (copy-component comp1))
		(firsttouched (fewest-extensions (cdr (component-openpath comp1)) lengths)))
	    (when ms98-trace
	      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		(msgf "------------BEGIN--------")
		(msgf "Nothing touched but not blocked.  With firsttouched = " firsttouched ".")
		(msgf "openpath = " (component-openpath comp1))
		(msgf "------------END----------" t)))
	    (if firsttouched
		(progn
		  (setf (component-openpath newcomp) (cons 'OR (delete firsttouched (cdr (copy-list (component-openpath comp1))))))
		  (setf (component-measure newcomp) -9999999) ;;push it to the back of the waiting list
		  (setf (component-original newcomp) (insert-sorted firsttouched (component-original newcomp)))
		  (push newcomp ccs-components2) 
		  ;;i.e. make a new component requiring one less fragment, that we can
		  ;;come back to if this fails
		  (setf (component-openpath comp1) nil)
		  (setf (component-touches comp1) (insert-sorted firsttouched (component-touches comp1)))
		  (setq touches (list firsttouched))
		  (setq blocking t))
	      (setq blocking 'fail))))
	(if (null touches) 
	    (setq touches (component-blocks comp1) touches-single nil)
	  (setq touches-single (fewest-extensions touches lengths) touches nil))
        (when ms98-trace
	  (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
			  (msgf "-BEGIN- blocking = " blocking t 
				"touches-single = " touches-single t
				"touches = " touches t "-END-" t))
	  (msgf "-BEGIN- blocking = " blocking t 
		"touches-single = " touches-single t
		"touches = " touches t "-END-" t))
	(if (eq blocking 'fail) 
	    (setq comp2list nil)
	  (progn
	    (if blocking (setq blocking nil) (setq blocking touches-single))
	    (setq comp2list (if touches-single ;;we have a unique disjunct to search for
				(nth touches-single ccs-orig2)
			      (if (eq (car (component-openpath comp1)) 'OR)
				  (let (newlist)
				    (dolist (c (cdr (component-openpath comp1)) newlist)
				      (setq newlist (append (nth c ccs-orig2) newlist))))
				(progn (setq *one* (car (component-openpath comp1))
					     *two* (cdr (component-openpath comp1))
					     *touches* touches)
				       (funcall (fiddle-comp2list) ccso)))))
	    (setq *one* (component-original comp1) *two* (component-tried comp1))
	    (if *one*
		(setq comp2list (remove-if #'comp2list-pruner-1 comp2list))
	      (setq comp2list (remove-if #'comp2list-pruner-2 comp2list)))
	    ))
	(when ms98-verbose 
	  (msgf " (blocks " (component-blocks comp1) " and touches " (component-touches comp1) " ; " 
		(length comp2list) " to try")
	  (when ms98-trace
	    (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
	      (msgf "-BEGIN- (blocks " (component-blocks comp1) " and touches " (component-touches comp1) " ; " 
		    (length comp2list) " to try -END-" t))))
        (dolist (comp2 comp2list)
	  (when (and max-search-limit
		     (>= (/ (- (get-net-internal-run-time) *ms98-start-time*) 
			    internal-time-units-per-second)
			 max-search-limit))
	    (cond ((= ms98-init 3) (throw 'stop 'next-prim))
		  ((and delay-setvars (setvars-in-jform ms90-3-jform))
		   (throw 'stop 'delay-setvars))
		  (*ccs-constrained-solns*
		   (throw 'stop 'next-constrained-soln))))
	  (setq will-never-work nil newtouches (ordered-union (component-touches comp1) 
							      (component-touches comp2)))
	  (if (and (progn (setq gcd (gcd key (component-key comp2)))
			  (setq will-never-work 
				(or (too-many-dups newtouches)
				    (= gcd (component-key comp2))))
			  (not will-never-work))
		   (or (not blocking) ;we aren't blocking a fragment
		       (memq blocking *single-lit-fragments*) ;we are blocking a fragment, but it has only 1 literal
		       (and (not (eq hpath-threshold 1)) ;blocking won't work if we can have A AND B in a fragment...
			    (or (not (memq blocking *single-hpath-fragments*))
				(neq gcd 1)
				(and (> max-mates 1)
				     (excess-maxmates-ok comp1 (component-clist comp2)))))
			    ;;we are blocking a multiple-literal single-hpath fragment *and* either have >1 conn in 
			    ;;common or don't touch anything new with the extra mates
		       (and (eq hpath-threshold 1)
			    (or (neq gcd 1)
				(and (> max-mates 1)
				     (excess-maxmates-ok comp1 (component-clist comp2))))))
		       ;;we are blocking a multiple-literal fragment *and* (as above)
		   (not (does-dups-first-2b (component-touches comp1) (component-clist comp2)))
		   ;;above: starting with the comps already touched, add conns one by one & fail if we add a dup first.
		   (progn (setq *vaf* (+ (gethash (component-name comp2) *vafindex*)
					 (gethash (component-name comp1) *vafindex*)))
			  (setq will-never-work ;which must be NIL, or we wouldn't have got this far... 
				(not (compatible-pairs clist (component-clist comp2) (component-final-subs comp1))))
			  (not will-never-work))
		   )
	      (progn 
		(setq mating (remove-duplicates (append (component-clist comp2) clist) :test #'equal))
		(when (and (member 'MATING ms98-trace)
			   (colored-mating mating given-clist))
		  (msgf "Trying to combine two components " (component-name comp1) " and " (component-name comp2) " to get connections:" t mating t)
		  (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		    (msgf "---------BEGIN---------")
		    (stringdtl)
		    (msgf "Trying to combine two components " (component-name comp1) " and " (component-name comp2) " to get connections:" t mating t)
		    (msgf "---------END---------" t)))
		(setq lits (reduce #'append (mapcar #'helper-function-21 mating)))
		(setq *ban-me* (when (and *skolem-matters* ms98-num-of-dups
					  (not (= (ccs-count-dups-in (component-touches comp1)) ms98-num-of-dups))
					  (= (ccs-count-dups-in newtouches) ms98-num-of-dups))
				 (ccs-banned-sk-consts newtouches)))
		(if (and maximize-first *using-unifhash*
			 (neq max-mates 1)
			 (violates-max-mates-1 lits (reduce #'append (mapcar #'helper-function-21 clist))))
		    (if (or (not (numberp max-mates))
			    (not (violates-max-mates lits)))
			(progn (setq mm-mark t)
			       (push (list comp1 comp2 mating lits gcd) mm-toobig-list))
		      (setq failed (cons (component-name comp2) failed)))
		  (progn 
		    (setq ctried (insert-sorted (component-name comp2) ctried))
		    (if (= gcd key) ;comp2 subsumes comp1
			(push comp2 temp-qc1)
		      (let* ((fail-now (or (and (numberp max-mates) (violates-max-mates lits))
					   (and ms98-minimality-check (violates-minimality mating))
					   ))
			     (newkey (unless fail-now (/ (* key (component-key comp2)) gcd)))
			     (newlitkey (unless fail-now (/ (* litkey (component-litkey comp2))
							    (gcd litkey (component-litkey comp2)))))
			     (already-tried (unless fail-now (already-tried newkey)))
			     (result (unless (or fail-now already-tried)
				       (if *using-unifhash*
					   (if (equal (component-final-subs comp2) 'delay)
					       (get-final-subs-delay-comp comp2 
									 ccs-original (component-final-subs comp1)
									 (component-clist comp1) (component-touches comp1)
									 mating newkey newlitkey newtouches
									 *ban-me* max-weight-so-far)
					     (merge-utrees comp1 comp2 mating newkey newlitkey *ban-me* newtouches))
					 (qum (mapcar #'helper-function-22 mating)
					      mating newtouches newkey newlitkey)))))
			(when (and ms98-verbose already-tried) (msg "&"))
			(when (and (not already-tried) (not fail-now))
			  (setf (gethash newkey *comps-tried*) (if result t 'fail))
			  (when ms98-low-memory (incf *dmark*)
				(mapcar #'mark-dag ccs-components)
				(mapcar #'mark-dag ccs-original)
				(dolist (c ccs-orig2) (mapcar #'mark-dag c))
				(mapcar #'mark-dag ccs-components2)
				(mapcar #'mark-dag temp-qc1)
				(mapcar #'mark-dag temp-qc)
				(mark-dag result)
				(prune-unmarked-array)))
			(if result (progn (setf (component-tried result)
						(ordered-union *two* (component-tried comp2)))
					  (if max-weight-so-far 
					      (setq max-weight-so-far (max max-weight-so-far (component-measure result)))
					    (setq max-weight-so-far (component-measure result)))
					  (push result temp-qc1))
			  (progn (when ms98-verbose
				   (if fail-now (msgf (parse-clist mating) " violated max-mates.")
				     (if already-tried (msgf (parse-clist mating) " seen before.")
				       (msgf (parse-clist mating) " isn't unifiable."))))
				 (setq failed (cons (component-name comp2) failed)))
			  ))))))
	    (when will-never-work (setq failed (cons (component-name comp2) failed)))
	    ))
	(setq temp-qc1 (remove-if #'helper-function-23 temp-qc1))
	(when mm-mark 
	  ;;if mm-mark is t, we've only done some of the extensions of this fragment
	  (setf (component-tried comp1) (ordered-union failed ctried)))
	(dolist (c temp-qc1) (setf (component-tried c) (ordered-union failed (component-tried c))))
	(when ms98-low-memory (incf *dmark*)
	      (mapcar #'mark-dag ccs-original)
	      (mapcar #'mark-dag ccs-components)
	      (dolist (c ccs-orig2) (mapcar #'mark-dag c))
	      (mapcar #'mark-dag ccs-components2)
	      (mapcar #'mark-dag temp-qc1)
	      (mapcar #'mark-dag temp-qc)
	      (prune-unmarked-array))
	(setq temp-qc (append temp-qc1 temp-qc)))
    (when mm-toobig-list
      (if temp-qc 
	  ;;then we found something, so we'll do no more at the minute.
	  ;;put all the unfinished components onto the pending list
	  (dolist (elt (remove-duplicates (mapcar #'car mm-toobig-list) :test #'equal)) (push elt ccs-components2))
	(do (elt comp2 comp1 lits mating gcd)
	    ((null mm-toobig-list)
	     (setq temp-qc (remove-if #'helper-function-23 temp-qc)))
	  (setq elt (pop mm-toobig-list))
	  (setq comp2 (cadr elt) mating (caddr elt) lits (cadddr elt) gcd (car (last elt)) comp1 (car elt))
	  (if (= gcd (component-key comp1)) ;comp2 subsumes comp1
	      (push comp2 temp-qc)
	    (let* ((fail-now (or (and (numberp max-mates) (violates-max-mates lits))
				 (and ms98-minimality-check (violates-minimality mating))))
		   (newkey (unless fail-now (/ (* (component-key comp1) (component-key comp2)) gcd)))
		   (newlitkey (unless fail-now (/ (* (component-litkey comp1) (component-litkey comp2))
						  (gcd (component-litkey comp1) (component-litkey comp2)))))
		   (already-tried (unless fail-now (already-tried newkey)))
		   (result (unless (or fail-now already-tried)
			     (if *using-unifhash*
				 (if (equal (component-final-subs comp2) 'delay)
				     (get-final-subs-make-comp comp2 
							       ccs-original (component-final-subs comp1)
							       (component-clist comp1) (component-touches comp1)
							       mating newkey newlitkey *ban-me* newtouches)
				   (merge-utrees comp1 comp2 mating newkey newlitkey *ban-me* newtouches))
			       (qum (mapcar #'helper-function-22 mating)
				    mating newtouches newkey newlitkey)))))
	      (when (and ms98-verbose already-tried) (msg "&"))
	      (when (and (not already-tried) (not fail-now))
		(setf (gethash newkey *comps-tried*) (if result t 'fail)))
	      (if result (progn (setf (component-tried result)
				      (ordered-union (component-tried comp1) (component-tried comp2)))
				(push result temp-qc))
		(when ms98-verbose
		  (if fail-now (msgf (parse-clist mating) " violated max-mates.")
		    (msgf (parse-clist mating) " isn't unifiable.")))
		))))))
    (msgf "Merging produced " (length temp-qc) " acceptable components." t)
    (when ms98-trace
      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		    (msgf "---------BEGIN---------")
	(stringdtl)
	(msgf "Merging produced " (length temp-qc) " acceptable components." t)
		    (msgf "---------END---------" t)))
    (if temp-qc
	(progn (setq ccs-components (sort temp-qc #'> :key #'component-measure))
	       (let ((len (component-measure (car ccs-components)))
		     (count 0)
		     ccs1 ccs2)
		 (dolist (c ccs-components)
		   (if (and (= len (component-measure c))
			    (or (null ms98-max-components) (< count ms98-max-components)))
		       (progn (incf count) (push c ccs1))
		     (push c ccs2)))
		 (setq ccs-components ccs1)
		 (setq ccs-components2 (append ccs2 ccs-components2))))
      (let ((len (car lengths))
	    ccs1 ccs2)
	(dolist (c ccs-components2)
	  (if (and (memq len (component-blocks c))
		   (not (component-ddf c)))
	      (push c ccs1) (push c ccs2)))
	(if ccs1 (progn (msgf "Discarding current components and looking for stored components blocking " len "." t)
			(setq ccs-components ccs1)
			(let ((len (reduce #'max (mapcar #'component-measure ccs-components)))
			      (count 0)
			      ccs1 ccs2a)
			  (dolist (c ccs-components)
			    (if (and (= len (component-measure c))
				     (or (null ms98-max-components) (< count ms98-max-components)))
				(progn (incf count) (push c ccs1))
			      (push c ccs2a)))
			  (setq ccs-components ccs1)
			  (setq ccs-components2 (append ccs2a ccs2))))
	  (progn (setq ccs-orig2 (remove-duplicates (reduce #'append ccs-orig2)))
		 (do ()
		     ((acceptable-disjunct (car lengths) ccs-orig2))
		   (when (null lengths)
		     (if (and delay-setvars (setvars-in-jform ms90-3-jform))
			 (throw 'stop 'delay-setvars)
		       (if *ccs-constrained-solns*
			   (throw 'stop 'next-constrained-soln)
			 (if (= ms98-init 3)
			     (throw 'stop 'next-prim)
			   (throw 'stop 'duplicate)))))
		   (setq ccs-orig2 (remove-if #'helper-function-24 ccs-orig2))
		   (setq ccs-original (remove-if #'helper-function-24 ccs-original))
		   (setq ccso (mapcar #'helper-function-20 ccs-original))
		   (msgf "Failed on fragment " (pop lengths) t))
		 (let ((len (car lengths))
		       ccs1 ccs2)
		   (dolist (c ccs-orig2)
		     (if (and (memq len (component-blocks c))
			      (not (component-ddf c)))
			 (push c ccs1) (push c ccs2)))
		   (msgf "Discarding current components and looking for stored components blocking " len "." t)
		   (setq ccs-components ccs1)
		   (let ((len (reduce #'max (mapcar #'component-measure ccs-components)))
			 (count 0)
			 ccs1 ccs2a)
		     (when *using-unifhash* 
		       (dolist (x ccs-components)
			 (get-final-subs-for x ccs-original (component-touches x))))
		     (setq ccs-components (remove-if #'helper-function-19 ccs-components))
		     (dolist (c ccs-components)
		       (if (and (= len (component-measure c))
				(or (null ms98-max-components) (< count ms98-max-components)))
			   (progn (incf count) (push c ccs1))
			 (push c ccs2a)))
		     (setq ccs-components ccs1 ccs-components2 ccs2a minor (length ccs2)
			   ccs-orig2 (arrange-components ccs2 (reduce #'max lengths))))))))))
  (when *using-unifhash* 
    (dolist (c ccs-components)
      (when (and (listp (component-final-subs c))
		 (dnode-p (car (component-final-subs c))))
	(setf (component-final-subs c) (fill-in-delayed-subs-2 (component-final-subs c) (component-clist c)))
	(when ms98-low-memory (incf *dmark*)
	      (mapcar #'mark-dag ccs-original)
	      (mapcar #'mark-dag ccs-components)
	      (dolist (c ccs-orig2) (mapcar #'mark-dag c))
	      (mapcar #'mark-dag ccs-components2)
	      (prune-unmarked-array))))
    (setq ccs-components (remove-if #'helper-function-19 ccs-components)))
  (when ms98-low-memory
    (setq ccs-components (delete-if-not #'(lambda (x) (quick-check-subs (component-clist x))) ccs-components))
    (setq ccs-components2 (delete-if-not #'(lambda (x) (quick-check-subs (component-clist x))) ccs-components2)))
  (msgf "Now merging " (length ccs-components) " current and " 
	minor " atomic (initial) components, with " (length ccs-components2) " stored." t)
  (when ms98-trace
    (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		    (msgf "------------BEGIN---------")
      (stringdtl)
      (msgf "Now merging " (length ccs-components) " current and " 
	    minor " atomic components, with " (length ccs-components2) " stored."  t)
      (msgf "Current Components: " (mapcar #'(lambda (c) (component-good-name c)) ccs-components))
      (msgf "Stored Components: " (mapcar #'(lambda (c) (component-good-name c)) ccs-components2)) 
      (print-component-info ccs-components)))
  (ccs-larger ccs-components ccs-components2 lengths ccs-orig2 minor))

(defun already-tried (newkey)
  (gethash newkey *comps-tried*)) ;we've already done this component.

(defun acceptable-disjunct (d comps)
  (dolist (c comps nil)
	  (when (and (member d (component-blocks c))
		     (not (component-ddf c))) ;(does-dups-first-2 (component-blocks c) (component-clist c))))
	    (return t))))

(defun initial-segment-p (list1 list2)
  (if (null list1) t
    (if (null list2) nil
      (and (eq (car list1) (car list2))
	   (initial-segment-p (cdr list1) (cdr list2))))))

(defun compatible-pairs (mating1 mating2 subs-for-mating1)
  (let ((real-mating2 (remove-if #'(lambda (x) (member x mating1 :test #'equalp)) mating2))
	will-return-nil
	pairstack key)
    (and (dolist (m1 mating1 t)
	   (unless (dolist (m2 real-mating2 t)
		     ;;only count *new* instances of valid-addition-2 towards the weight.
		     (unless (valid-addition-2 m1 m2)
		       (return nil)))
	     (return nil)))
	 (dolist (m2 real-mating2
		     (dolist (p pairstack t)
		       (when (eq 'fail (apply #'check-pair p)) (return nil))))
	   (when *using-unifhash*
	     (if (and (neq 'fail subs-for-mating1)
		      (neq 'fail (component-final-subs (gethash m2 conn-hash)))
		      (dag-will-merge subs-for-mating1 (component-final-subs (gethash m2 conn-hash))))
		 (dolist (m1 mating1)
		   (setf (gethash (* (gethash m1 primehash1) (gethash m2 primehash1)) pairhash) t))
	       (setq will-return-nil t)))
	   (unless (dolist (m1 mating1 t)
		     (setq key (* (gethash m1 primehash1) (gethash m2 primehash1)))
		     (if (gethash key pairhash)
			 (when (eq (gethash key pairhash) 'fail) (return nil))
		       (when (or (null ms98-low-memory)
				 (e-ordered-intersection (component-touches (gethash m1 conn-hash))
							 (component-touches (gethash m2 conn-hash))))
			 (push (list m1 m2 key) pairstack))))
	     (return nil))
	   (when will-return-nil (return nil))))))

(defun fewest-extensions (touches lengths)
  (if (null touches) (if hpath-threshold nil 
		       (if (and delay-setvars (setvars-in-jform ms90-3-jform))
			   (throw 'stop 'delay-setvars)
			 (if *ccs-constrained-solns*
			     (throw 'stop 'next-constrained-soln)
			   (if (= ms98-init 3)
			       (throw 'stop 'next-prim)
			     (throw 'stop 'duplicate)))))
    (if (null (cdr touches)) (car touches)
      (dolist (l lengths (if hpath-threshold nil
			   (if (and delay-setvars (setvars-in-jform ms90-3-jform))
			       (throw 'stop 'delay-setvars)
			     (if *ccs-constrained-solns*
				 (throw 'stop 'next-constrained-soln)
			       (if (= ms98-init 3)
				   (throw 'stop 'next-prim)
				 (throw 'stop 'duplicate))))))
	      (when (member l touches) (return l))))))

(defun make-litkey (litlist)
  (declare (special primehash2))
  (reduce #'*-or-nil (mapcar #'helper-function-25 litlist)))

(defun *-or-nil (x y)
  (when (and x y) (* x y)))

(defun get-ccs-key (mating)
  (declare (special primehash1))
  (let ((key 1))
    (dolist (m mating)
	    (setq key (* key (gethash m primehash1))))
    key))

(defun parse-clist (list)
  (conc-stringlist (mapcar #'remove-leaf list)))

(defun remove-leaf (x)
  (concatenate 'string "(" (string-left-trim "LEAF" (princ-to-string (car x)))
	       " . " (string-left-trim "LEAF" (princ-to-string (cdr x)))
	       ") "))

(defun make-pairs (list)
  (do ((l list (cdr l))
       return)
      ((null (cdr l)) return)
      (dolist (k (cdr l))
	      (push (cons (car l) k) return))))

(defun find-touches (lits)
  (declare (special disj-assoc))
  (let (return)
    (dolist (d disj-assoc (sort return #'<))
	    (when (intersection (cadr d) lits)
		  (setq return (cons (car d) return))))))

(defun construct-component (root mating key litkey &optional (touches nil))
  (declare (special disj-assoc))
  (let* ((urun unif-running)
	 (dummy (when unif-running (breakcount 'unification)))
	 (lits (reduce #'append (mapcar #'helper-function-21 mating)))
	 (name (incf component-count))
	 (comp 
	  (make-component :clist mating 
			  :lits lits
			  :name name
			  :tried (list name)
			  :key key
			  :litkey litkey
			  :touches (or touches (find-touches lits))
			  :blocks (find-blocks lits)
			  :final-subs root
			  :ddf nil)))
    (declare (ignore dummy))
    (when (and (member 'MATING ms98-trace)
	       (colored-mating mating given-clist))
      (setf (component-good comp) t)
      (msgf "Component " name " is good:")  ; this should say "connection" if we are in the initial stage of finding potential connections
      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		      (msgf "------------BEGIN---------")
	(stringdtl)
	(msgf "Component " name " is good:")
	(msgf "Connections: " mating t)))
    (setf (component-measure comp) (ccs-measure comp (length disj-assoc)))
    (when ms98-verbose (msgf (parse-clist (component-clist comp)) " is OK"))
    (when (and (null (e-ordered-setdiff (component-touches comp) (component-blocks comp)))
	       (ccs-complete-p (component-clist comp) comp)
	       (if (eq root 'FAIL) ; cebrown 9/23/00, added this case to return nil when unification failed.
		   (return-from construct-component nil) ; previously if the proposed mating was complete, but unif failed (eg, cyclic check), 
					; ms98 was tricked into believing it had succeeded
	       (if (and (listp root) (dnode-p (car root)))
		   (progn (runcount 'unification)
			  (setf (component-final-subs comp) (fill-in-delayed-subs root (component-clist comp)))
			  (breakcount 'unification)
			  (unless (component-final-subs comp)
			    (return-from construct-component nil))
			  t)
		 t)))
      (when *ccs-rewrites* (setq *successful-sub* (append (extract-sub-from (component-final-subs comp))
							  *successful-sub*)))
;      (breakcount 'unification)
      (if (or (not ms98-full-jform) (eq ms98-full-jform ms90-3-jform)) ; cebrown 10/6/01
	  (throw 'stop (component-clist comp))
	(progn
	  (ccs-solve-remaining-constraints comp)
	  (setq comp nil))))
    (when urun (runcount 'unification))
    comp))

(defun colored-connection (conn given-clist)
  (let ((n1 (literal-name (car conn)))
	(n2 (literal-name (cdr conn))))
    (dolist (cl given-clist nil)
	    (when (and (find-if #'(lambda (x) (eq (etree-name x) n1)) (cdr cl))
		       (find-if #'(lambda (x) (eq (etree-name x) n2)) (cdr cl)))
	      (return (car cl))))))
				

(defun colored-mating (conns given-clist)
  (dolist (conn conns t)
	  (unless (colored-connection conn given-clist)
	    (return nil))))

					; cebrown, for MS98-TRACE 2/21/00, unused since I started using "colors" 9/2001
(defun subclist (clist1 clist2)
  (dolist (cl clist1 t)
    (unless (member cl clist2 :test #'equal-conn)
      (return nil))))

					; cebrown, for MS98-TRACE 2/21/00
(defun equal-conn (conn1 conn2)
  (or (and (eq (literal-name-or-symbol (car conn1))
	       (literal-name-or-symbol (car conn2)))
	   (eq (literal-name-or-symbol (cdr conn1))
	       (literal-name-or-symbol (cdr conn2))))
      (and (eq (literal-name-or-symbol (cdr conn1))
	       (literal-name-or-symbol (car conn2)))
	   (eq (literal-name-or-symbol (car conn1))
	       (literal-name-or-symbol (cdr conn2))))))

(defun literal-name-or-symbol (l)
  (cond ((symbolp l)
	 l)
	((literal-p l)
	 (literal-name l))
	(t (throwfail l " should be a literal or a symbol."))))

(defun make-primes (n)
  (do ((c 2 (1+ c))
       (plist nil)
       (count 0))
      ((= count n) plist)
      (when (prime-p c plist) (incf count) (push c plist))))

(defun prime-p (n plist)
  (dolist (p plist t)
	  (when (zerop (mod n p)) (return nil))))
	
(defun go-below-disjunct (l)
  (if (jform-parent l)
      (if (eq (jform-type (jform-parent l)) 'disjunction) 
	  (if (or (no-higher-disjunct (jform-parent l))
		  (cadr (member l (disjunction-components (jform-parent l)))))
	      l ;then there's another component to this disjunct, or there is no higher disjunct
	    (go-below-disjunct (jform-parent l)))
	(go-below-disjunct (jform-parent l)))
    nil))

(defun finish-up-ccs (clist)
  (let* ((dissolve-conns (eproof-dissolve current-eproof)))
    (setq first-order-mode-ms *saved-first-order-mode-ms*)
					; some of the dissolved literals may involve lambda normalization,
					; though the jform which the automatic mating search worked on was 1st order,
					; so that TPS may have set first-order-mode-ms to T.
    (ccs-etree-cleanup clist dissolve-conns) ; call this before we mess with the clist
    (setq clist 
      (append (mapcar #'(lambda (x)
			  (cons (etree-name ; this may be a nonleaf deepened from a leaf
				 (find-etree-node-name (car x)))
				(etree-name
				 (find-etree-node-name (cdr x)))))
		      dissolve-conns)
	      (mapcar #'(lambda (x)
			  (cons (literal-name (car x))
				(literal-name (cdr x))))
		      clist)))
    (setf (eproof-allow-nonleaf-conns current-eproof) ; cebrown - 10/2/00
      (append (eproof-allow-nonleaf-conns current-eproof)
	      (mapcar #'car clist)
	      (mapcar #'cdr clist)))
    (setf (eproof-dissolve current-eproof) nil)
    (setq *global-rewrite-dtree* nil)
    (clrhash (cgraph))
    (clrhash (connections-array))
    (gc)
    (when ms98-verbose ; cebrown
      (msgf "Creating final jform with eproof allow-nonleaf-conns = " (eproof-allow-nonleaf-conns current-eproof)))
    (cr-eproof-jform)
    (when ms98-verbose
      (msgf "Final Jform")
      (display-vp-diag (eproof-jform current-eproof)))
    (setq active-mating			; cebrown 9/28/00.  This is necessary when the complete mating has no conns (e.g., "a = a")
      (init-mating))
    (setf (mating-clist active-mating) nil)
    (when (let ((count 0)
		(max-substs-var nil)
		(max-substs-quick nil)
		(MAX-MATES :INFINITE) ; cebrown 28/10/01
		(min-quick-depth 1)
		(max-search-depth nil)
		(max-utree-depth nil))
	    (dolist (d clist t)
	      (incf count)
	      (add-conn (car d) (cdr d))
	      (unless (= (length (mating-clist active-mating)) count) ;if it wasn't added...
		(return nil))))
      (if (or first-order-mode-ms (null clist))
	  (setf (mating-completep active-mating) t)
	(let ((max-substs-var (max 10 (or max-substs-var 0)))
	      (max-substs-quick (max 10 (or max-substs-quick 0)))
	      (MAX-MATES :INFINITE) ; cebrown 28/10/01
	      (root0 (pull-up-leaves ; unify-msv (apparently) assumes the utree is of depth <= 1, so I wrote this function - cebrown
		      (add-hidden-subs (find-real-root (mating-utree active-mating))))))
	  (multiple-value-bind
	      (flag root subst-hash-table success-nodes)
	      (unify-msv root0 (make-hash-table))
	    (setf (mating-utree active-mating) root)
	    (setf (mating-subst-hash-table active-mating)
	      subst-hash-table)
	    (setf (mating-success-nodes active-mating) success-nodes)
	    (when (eq flag 'success)
	      (setf (mating-completep active-mating) t))))))
    ;;we already checked completeness of this set of conns,
    ;;so as long as they were all added we're OK.
    (runcount 'mating)
    (breakcount 'mating)
    (runcount 'mating-ctr)
    (breakcount 'mating-ctr)
    (display-time 'mating) 
    (runcount 'mating)
    (if (mating-completep active-mating) 
	t
      (msgf "Something went wrong... that's not a complete mating." t))))

(defun add-hidden-subs (node) ;this is new, and probably unnecessary. MB Fri May 29 16:29:02 1998
  (dolist (e (eproof-free-vars-in-etree current-eproof) node)
    (unless (equal (exp-var-var (car e)) (exp-var-subst (car e)))
      (push (cons (exp-var-var (car e)) (exp-var-subst (car e))) (node-subst-stack node))
      (setf (exp-var-subst (car e)) (exp-var-var (car e))))))

(defun find-real-root (utree)
  (if (= (length (node-sons utree)) 1) (find-real-root (car (node-sons utree))) utree))

; unify-msv (apparently) assumes the utree is of depth <= 1, so I wrote this function - cebrown
; It takes a unification tree with node root and returns a unification tree of depth <= 1
; with node root and where the sons of root are all the leaves of the given tree.
; That is, this flattens the tree, forgetting all intermediate nodes.
(defun pull-up-leaves (root)
  (setf (node-sons root)
	(pull-up-leaves-rec (node-sons root) root)) ; what I did before would turn a root with no sons into a root with itself as a son
					; fixed this - 5/15/00 - cebrown
  root)

(defun pull-up-leaves-rec (nodes root)
  (if nodes
      (let ((node (car nodes)))
	(if (node-sons node)
	    (pull-up-leaves-rec (append (node-sons node) (cdr nodes)) root)
	  (progn
	    (setf (node-parent node) root)
	    (cons node
		  (pull-up-leaves-rec (cdr nodes) root)))))
    nil))

(defun parse-into-types (symbollist)
  (parse-fully (mapcar #'helper-function-29 symbollist)))

(defun parse-fully (alist)
  (if (null alist) nil
    (let* ((first (caar alist))
	   (list1 (remove-if-not #'(lambda (x) (equal (car x) first)) alist))
	   (list2 (remove-if #'(lambda (x) (equal (car x) first)) alist))
	   (needs-dummy (remove-if-not #'helper-function-30 list1)))
      (when (and (not ms98-unif-hack) (null needs-dummy)) ;there is no constant whose type is this resulttype...
	    (let ((const (intern-str (format nil "a~A" (type-to-string first)))))
	      (setf (get const 'type) first)
	      (push (cons first const) list1)))
      (cons (cons first (mapcar #'cdr list1)) (parse-fully list2)))))

(defun find-outer-etree-nodes (function &optional (etree (eproof-etree current-eproof)))
  (let ((nodes (find-etree-nodes function etree))
	newnodes)
    (dolist (n nodes)
	    (unless (if (etree-parent n) (remove-if-not function (etree-ancestor-list n)) nil)
		    (push n newnodes)))
    newnodes))

(defun get-sub-for-dup (sub)
  ;just the remove-duplicates of the two subs; complain if there's a clash.
  ;sub is of the form (vi . wi), and we will apply it to rename vi in component-final-subs to wi
  (remove-duplicates sub :test #'equal)
  )

(defun compose-ccs-subs (sub1 sub2)
  ;sub1 gets from copy 0 to copy 1, and sub1 gets from copy 0 to copy k (k <> 1)
  ;sub1 (v0 . v1) , sub2 (v0 . vk)
  ;return a sub that gets from copy 1 to copy k
  (let (newsub)
    (dolist (s sub2 newsub)
	    (when (assoc (car s) sub1)
		  (push (cons (cdr (assoc (car s) sub1)) (cdr s)) newsub)))))

(defun sub-for-component (comp2 real-sub real-x real-y disj-assoc)
  (let* ((name (incf component-count))
	 (lits (list real-x real-y))
	 (comp (make-component :clist (list (cons real-x real-y))
			       :lits lits
			       :name name
			       :tried (list name)
			       :key 0
			       :litkey 0
			       :touches (find-touches lits)
			       :blocks (find-blocks lits)
			       :final-subs (apply-ccs-real-sub (component-final-subs comp2) real-sub)
			       :ddf nil)))
    (when (and (member 'MATING ms98-trace)
	       (colored-connection (cons real-x real-y) given-clist))
      (setf (component-good comp) t)
      (msgf "Component " name " is good:")
      (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
		      (msgf "------------BEGIN---------")
	(stringdtl)
	(msgf "Component " name " is good:")
	(msgf "Connections: " real-x " . " real-y t)))	; also print to file
    (setf (component-measure comp) (ccs-measure comp (length disj-assoc)))
    (when ms98-verbose (msgf (component-clist comp) " is OK"))
    (when (and (null (e-ordered-setdiff (component-touches comp) (component-blocks comp)))
	       (ccs-complete-p (component-clist comp) comp))
      (when *ccs-rewrites* (setq *successful-sub* (append (extract-sub-from (component-final-subs comp))
							  *successful-sub*)))
      (if (or (not ms98-full-jform) (eq ms98-full-jform ms90-3-jform)) ; cebrown 10/4/01
	  (throw 'stop (component-clist comp))
	(progn
	  (ccs-solve-remaining-constraints comp)
	  (setq comp nil))))
    comp))

(defun apply-ccs-real-sub (oldsub real-sub)
  ;;idea: real-sub is a dotted list of form ((old-var . new-var)...)
  ;;we un-dagify oldsub, then apply real-sub, then dagify again.
  (if (eq oldsub 'delay) 'delay
    (if (or (not first-order-mode-ms) ms98-force-h-o)
	(apply-sub-to-dag oldsub real-sub)
      (apply-ccs-real-sub-2 oldsub real-sub))))

(defun apply-ccs-real-sub-2 (final-subs real-sub)
  (let (newsub
	newsubs)
    (dolist (f final-subs newsubs)
	    (setq newsub (remove-if #'helper-function-31 (mapcar #'cons *ordered-vars* f) :key #'cdr))
	    (setq newsub (mapcar #'(lambda (x) (if (assoc (car x) real-sub)
						   (cons (cdr (assoc (car x) real-sub)) (cdr x))
						 x)) newsub))
	    (if (and first-order-mode-ms (not ms98-force-h-o))
		(push (mapcar #'(lambda (x) (cdr (assoc x newsub))) *ordered-vars*) newsubs)
	      (push (mapcar #'(lambda (x) (assoc x newsub)) *ordered-vars*) newsubs)))))

(defun check-pair (conn1 conn2 key)
  (declare (special pairhash))
  (or (gethash key pairhash)
      (if (if *using-unifhash*
	      (let ((comp1 (gethash conn1 conn-hash))
		    (comp2 (gethash conn2 conn-hash)))
		(merge-utrees comp1 comp2 nil -1 -1 nil 
			      (find-touches (list (car conn1) (cdr conn1) (car conn2) (cdr conn2)))))
	    (qum (mapcar #'helper-function-22 (list conn1 conn2))
		 nil (find-touches (list (car conn1) (cdr conn1) (car conn2) (cdr conn2))) -1 -1))
	  (progn (setf (gethash key pairhash) t)
		 t)
	(progn (setf (gethash key pairhash) 'fail)
	       'fail))))

(defun violates-max-mates (litlist)
  (do ((lit (car litlist) (car lits))
       (lits (cdr litlist) (cdr lits)))
      ((null lits) nil)
    (when (>= ;because we already have one mate, namely lit.
	   (length (remove-if-not #'(lambda (x) (eq x lit)) lits)) ;number of *other* mates for this lit.
	   max-mates)
      (return t))))

(defun violates-max-mates-1 (litlist1 litlist2)
  ;;litlist1 is the new mating, litlist2 the old mating
  (do ((lit (car litlist1) (car lits))
       (lits (cdr litlist1) (cdr lits)))
      ((null lits) nil)
    (when (> (1+ (length (remove-if-not #'(lambda (x) (eq x lit)) lits))) ; #copies of lit in the new mating
	     (length (or (remove-if-not #'(lambda (x) (eq x lit)) litlist2) '(1)))) 
					; #copies of lit in the old mating, or 1 if it didn't occur at all
      (return t))))

(defun really-selected ()
  (let ((evars (remove-if-not #'exp-var-p (mapcar #'car (eproof-free-vars-in-etree current-eproof)))))
    (dolist (e evars nil)
      (when (exp-var-selected e) (return t)))))

(defun fiddle-comp2list ()
  (if hpath-threshold
      (if (not (zerop *two*)) #'mapcan-func-1 #'mapcan-func-2)
    (if (not (zerop *two*)) #'mapcan-func-3 #'mapcan-func-4)))

(defun mapcan-func-1 (y) (mapcan #'fiddle-func-1 y))
(defun mapcan-func-2 (y) (mapcan #'fiddle-func-2 y))
(defun mapcan-func-3 (y) (mapcan #'fiddle-func-3 y))
(defun mapcan-func-4 (y) (mapcan #'fiddle-func-4 y))

(defun fiddle-func-1 (x) (and (or (and (zerop (mod *one* (cadr x)))
				       (or (zerop (mod *one* (caddr x)))
					   (zerop (mod *two* (caddr x)))))
				  (and (zerop (mod *one* (caddr x)))
				       (or (zerop (mod *one* (cadr x)))
					   (zerop (mod *two* (cadr x))))))
			      (copy-list (cdddr x))))
			 ;;i.e. both literals of this connection lie on the next-openpath of comp1
			 ;;and at least one lies in the relevant open part.

(defun fiddle-func-2 (x) (and (zerop (mod *one* (cadr x)))
			      (zerop (mod *one* (caddr x)))
			      (copy-list (cdddr x))))

(defun fiddle-func-3 (x) (and (intersection (car x) *touches*)
			       (or (and (zerop (mod *one* (cadr x)))
					(or (zerop (mod *one* (caddr x)))
					    (zerop (mod *two* (caddr x)))))
				   (and (zerop (mod *one* (caddr x)))
					(or (zerop (mod *one* (cadr x)))
					    (zerop (mod *two* (cadr x))))))
			       (copy-list (cadddr x))))

(defun fiddle-func-4 (x) (and (intersection (car x) *touches*)
			      (zerop (mod *one* (cadr x)))
			      (zerop (mod *one* (caddr x)))
			      (copy-list (cadddr x))))

(defun helper-function-1 (x)  (cons (exp-var-var x) (exp-var-selected x)))
(defun helper-function-2 (x) (eq (etree-junctive x) 'dis))
(defun helper-function-3 (x) (and (etree-positive x) (leaf-p x)))
(defun helper-function-4 (x) (and (not (etree-positive x)) (leaf-p x)))
(defun helper-function-5 (x) (cons x -1))
(defun helper-function-6 (x) (memq (literal-name x) (mapcar #'leaf-name *live-leaves*)))
(defun helper-function-7 (x) (= (caddr x) 1))
(defun helper-function-8 (x) (= (number-of-horizontal-paths-main (cadddr x)) 1))
(defun helper-function-9 (x) (declare (ignore x)) 1)
(defun helper-function-10 (x) (sort x #'<))
(defun helper-function-12 (x) (or (null (component-blocks x))
				  (e-ordered-setdiff (component-touches x) (component-blocks x))))
(defun helper-function-13 (x) (violates-max-mates (component-lits x)))
(defun helper-function-14 (x) (and (= (length (component-clist x)) 1)
				   (e-ordered-setdiff (component-touches x) (component-blocks x))))
(defun helper-function-15 (x) (null (assoc (car x) lengths)))
(defun helper-function-16 (x y) (or (< (cdr (assoc (car x) lengths))
				       (cdr (assoc (car y) lengths)))))
(defun helper-function-17 (x) (subsetp (component-touches x) lengths))
(defun helper-function-18 (x) (and (not (component-ddf x))
				   (memq (car lengths) (component-blocks x))))
(defun helper-function-19 (x) (equal (component-final-subs x) 'fail))
(defun helper-function-20 (x) (list (component-touches x)
				    (gethash (caar (component-clist x)) primehash2)
				    (gethash (cdar (component-clist x)) primehash2)
				    x))
(defun helper-function-21 (x) (list (car x) (cdr x)))
(defun helper-function-22 (x) (copy-list (gethash x dphash)))
(defun helper-function-23 (x) (does-dups-first (component-blocks x) (component-touches x)))
(defun helper-function-24 (x) (memq (car lengths) (component-touches x)))
(defun helper-function-25 (x) (gethash x primehash2))
(defun helper-function-26 (x) (cons (component-blocks x) (component-touches x))) 
(defun helper-function-27 (x) (fewest-extensions (setdiff (cdr x) (car x)) (mapcar #'car lengths)))
(defun helper-function-28 (x) (if x (cdr (assoc x lengths)) 1000))
(defun helper-function-29 (x) (cons (car (last (listify-vec (listify-type (type x))))) x))
(defun helper-function-30 (x) (equal (type (cdr x)) (car x)))
(defun helper-function-31 (x) (or (null x) (zerop x))) 

(defun comp2list-pruner-1 (x) (or (e-ordered-intersection *one* (component-touches x))
				  (ordered-memq (component-name x) *two*)))
(defun comp2list-pruner-2 (x) (ordered-memq (component-name x) *two*))

(defun ms98-print-primsubs (etree)
  (msgf "Substitutions in this jform:" t)
  (let (subs-exist)
    (dolist (var (reduce #'append (mapcar #'etree-components (find-etree-nodes #'expansion-p etree))))
      (let ((exp-var (nth (position var (etree-components (etree-parent
							   var)))
			  (expansion-terms (etree-parent var)))))
	(if (not (eq (exp-var-var exp-var) (exp-var-subst exp-var)))
	    (progn
	      (msg ((exp-var-var exp-var) . gwff) t "  -->  "
		   ((exp-var-subst exp-var) . gwff) t)
	      (setq subs-exist t)))))
    (if (not subs-exist) (msg "None." t))
    subs-exist))

(defun print-component-info (ccs-components)
  (msgf "The current list has " (length ccs-components) " components.")
  (let ((good-conn-count (mapcan #'(lambda (x) (and (component-good x) (list x)))
				 ccs-components))
	(count-info nil))
    (dolist (g good-conn-count)
      (let* ((numconns (length (component-clist g))) 
	     (a (assoc numconns count-info)))
	(if a
	    (setq count-info (acons numconns (+ (cdr a) 1) (remove a count-info)))
	  (setq count-info (acons numconns 1 count-info))))
      (msgf "The number of good ones: " (length good-conn-count) ".")
      (msgf "The number of connections and number of components with that many connections is " t count-info))))

(defun component-good-name (c)
  (if (component-good c)
      (format nil "*~d*" (component-name c))
    (format nil "~d" (component-name c))))
      
(defun update-measurement (prop val)
  (let ((a (assoc prop MEASUREMENTS)))
    (if a
	(setq MEASUREMENTS (acons prop val (remove a MEASUREMENTS)))
      (setq MEASUREMENTS (acons prop val MEASUREMENTS)))))

; c - component
(defun ccs-solve-remaining-constraints (c)
  (declare (special ms98-setvars))
  (let* ((sub (if c (get-component-sub c) nil))
	 (clist (append (eproof-dissolve current-eproof)
			(mapcar #'(lambda (x)
			    (cons (literal-name (car x))
				  (literal-name (cdr x))))
			(if c (component-clist c) nil))))
	 (lemmas (eproof-lemmas current-eproof))
	 (f (simul-substitute-in-ftree sub ms98-current-ftree)))
    (setq clist (ftree-mating-image clist ms98-current-ftree f))
    (if (intersection ms98-setvars (mapcar #'car sub)) ; instantiated a setvar during mating of the rigid part
	(new-ccs-constrained-soln (make-constraint-set
				   :ftree f :clist clist :lemmas lemmas
				   :kind '(SOLVED)))
      (ccs-solve-remaining-constraints-1
       f clist lemmas
       (remove-if #'(lambda (x) (assoc x sub)) ms98-setvars)
       (ccs-dissolve-clist clist (ftree-to-jform f))))))

(defun ccs-solve-remaining-constraints-1 (f clist lemmas setvars j)
  (let ((cs (catch 'complete-constraints
	      (generate-constraint-sets-for-vars
	       f clist lemmas setvars (ftree-all-banned f) j))))
    (if (constraint-set-p cs) ; if it's not a list of constraint sets,
					; it must be a complete one
					; we can solve to finish the problem
	(ccs-solve-final-constraints cs)
      (new-ccs-constrained-solns cs))))

; low numbers are considered first.
; extend this later to use a flag to try other measures.
; I expect this to always return a positive real
(defun constrained-soln-measure (f clist cs lemmas)
  (declare (ignore f clist lemmas))
  (let ((constrs (constraint-set-constrs cs)))
    (apply #'+ (mapcar #'length constrs))))

; this is to ensure fairness - every positive real gets reduced until
; it must be considered before new constrained solns.
(defun constraint-reduce-measure (m)
  (/ m 2))

(defun get-component-sub (c)
  (let ((sub nil))
    (dolist (s (extract-sub-from (component-final-subs c)))
	    (when (consp s)
	      (if (integerp (cdr s))
		  (push (cons (car s) (gethash (cdr s) *ccs-substs*))
			sub))))
    sub))

(defun ccs-solve-final-constraints (cset)
  (multiple-value-bind
   (f2 clist2 lemmas2)
   (catch 'cant-solve
     (ftree-solve-constraint-set cset))
   (if f2
       (progn
	 (ftree-to-etree f2)
	 (setf (eproof-lemmas current-eproof) lemmas2)
	 (setf (eproof-dissolve current-eproof)
	       (append clist2 (eproof-dissolve current-eproof)))
	 (throw 'stop nil))
     (progn
       (msgf "Could not solve final constraint set:")
       (print-constraint-set cset)
       (throwfail "Possibly a bug in set constraint search?")))))

(defun new-ccs-constrained-solns (cll)
  (dolist (cl2 cll)
	  (new-ccs-constrained-soln cl2)))

(defun new-ccs-constrained-soln (cl2)
  (declare (special *ccs-constrained-solns* *ccs-constrained-solns-prune*))
  (let ((ll (constraint-identifier cl2)))
    (unless (member ll *ccs-constrained-solns-prune* :test #'equal)
      (push ll *ccs-constrained-solns-prune*)
      (if *ccs-constrained-solns*
	  (nconc *ccs-constrained-solns* (list cl2))
	(setq *ccs-constrained-solns* (list cl2))))))

;      (setq *ccs-constrained-solns*
;	    (merge 'list (list cl2)
;		   *ccs-constrained-solns*
;		   #'< :key #'(lambda (l) (nth 4 l))))

(defun constraint-identifier (cl)
  (let (ret)
    (case (car (constraint-set-kind cl))
	  ((EMPTY FULL MAX MIN)
	   (dolist (constr (constraint-set-constrs cl) (sort ret #'string<))
		   (let ((l nil)
			 (r nil))
		     (dolist (lit constr)
			     (if (jform-pos lit)
				 (push (format nil "~d" (literal-represents lit)) l)
			       (push (format nil "~d" (literal-represents lit)) r)))
		     (setq l (sort l #'string<))
		     (setq r (sort r #'string<))
		     (let ((str ""))
		       (dolist (s r)
			       (setq str (format nil "~d,~d" s str)))
		       (setq str (format nil "==>~d" str))
		       (dolist (s l)
			       (setq str (format nil "~d,~d" s str)))
		       (push str ret)))))
	  (t (format nil "~d-~d" (car (constraint-set-kind cl)) (intern (gensym)))))))
  
(defun ccs-next-constrained-soln-init ()
  (declare (special ms98-current-ftree))
  (when *ccs-constrained-solns*
    (let ((cs (pop *ccs-constrained-solns*)))
      (if (equal (car (constraint-set-kind cs)) 'SOLVED)
	  (ccs-next-solved-constrained-soln-init cs)
	(let ((new-csets (process-constraint-set cs)))
;    (dolist (cl0 *ccs-constrained-solns*)
;	    (rplaca (cddddr cl0) (constraint-reduce-measure (nth 4 cl0))))
	  (if (and (equal (length new-csets) 1)
		   (equal (car (constraint-set-kind (car new-csets))) 'SOLVED))
	      (ccs-next-solved-constrained-soln-init (car new-csets))
	    (progn
	      (new-ccs-constrained-solns new-csets)
	      (ccs-next-constrained-soln-init))))))))

(defun ccs-next-solved-constrained-soln-init (cs)
  (let ((f (constraint-set-ftree cs))
	(clist (constraint-set-clist cs))
	(lemmas (constraint-set-lemmas cs)))
    (setq ms98-current-ftree f)
    (ftree-to-etree ms98-current-ftree)
    (setf (eproof-lemmas current-eproof) lemmas)
    (setf (eproof-dissolve current-eproof) clist)
    (setf (eproof-free-vars-in-etree current-eproof)
	  (remove-if-not
	   #'(lambda (x) (and 
			  (find-etree-node-name (etree-name (cdr x)) (eproof-etree current-eproof))
			  (intersection (free-vars-of (strip-exp-vars (get-deep (eproof-etree current-eproof))))
					(free-vars-of (strip-exp-vars (car x))))))
	   (eproof-free-vars-in-etree current-eproof)))
    (setq ms90-3-jform (init-position
			(etree-to-jform
			 (if (eproof-lemmas current-eproof)
			     (cadr (etree-components (eproof-etree current-eproof)))
			   (eproof-etree current-eproof))))
	  *ms98-start-time* (get-net-internal-run-time))
    (pathnum-init-jform ms90-3-jform)
    (remove-sk-labels ms90-3-jform)
    (setq *global-constlist* (parse-into-types (if ms98-unif-hack (find-consts)
						 (cons (getrwff "TRUTH") (find-consts)))))
    (setq *skolem-matters* (and (null skolem-default) (really-selected)))
    (when *skolem-matters*	; cebrown - 7/26/00 - changed this to set *allb* to include banned selected for setsubs too
      (setq *allb* (eproof-all-banned current-eproof)))
    (mark-ext-exp-nodes ms90-3-jform)
    (setq ms98-full-jform ms90-3-jform)
    (when ms98-rewrites 
      (get-ccs-rewrites))
    (setq *using-unifhash* (or (not first-order-mode-ms) ms98-force-h-o))
    (when (and (not *ccs-rewrites*) *using-unifhash*) (unify-all-var-pairs))
    (setq *ordered-vars* (order-vars))
    (setq *depth-hash* (make-array (list (length *ordered-vars*) *arbitrary-const*)
				   :element-type 'hash-table))
    (when *ccs-rewrites* (maphash #'prune-rewrites-2 *full-unifhash*))
    (setq *ordered-vars* (order-vars))
    (setq *depth-hash* (make-array (list (length *ordered-vars*) *arbitrary-const*)
				   :element-type 'hash-table))
    (when *ccs-rewrites* (maphash #'prune-rewrites-2 *full-unifhash*))
    t))
