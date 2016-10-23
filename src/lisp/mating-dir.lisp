;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package "AUTO")
(part-of ms88)

(deffile mating-dir
  (part-of ms88)
  (extension lisp)
  (mhelp "Functions to direct the mating search package.
    Applies to MS88.  In this version of the file, after backtracking
    TPS continues working on the same path, which prevents floundering."))

(defvar *doing-prenex* nil)

(defun initialize-mating-search (&optional (reinit nil))
  (signal-event 'stop-time)
  (unless (or (mating-list) reinit)
    (setren-counter w-var-prefix 1)
    (setq active-mating nil)
    (setq duplicated nil)
    (setf (max-cgraph-counter) 0)
    (setf (incomp-clists) nil)
    (setf (cgraph) (make-hash-table :test #'equal))
    (setf (bktrack-limit) initial-bktrack-limit)
    (setf (connections-array) (make-hash-table :test #'eql))
    (setf (incomp-clists-wrt-etree) nil)
    (setf (max-incomp-clists-wrt-etree) nil)
    (setf (mating-list) nil))
  (push (init-mating) (mating-list))
  t)

(defun ms-director (&optional (etree (eproof-etree current-eproof))
			      (allow-duplications t))
  (if (free-vars-in-etree current-eproof)
      (ms etree allow-duplications)
      (ms-propositional etree)))

(defun ms-propositional (etree)
    (unwind-protect  
      (ms-propositional-real etree)
      (progn (breakcount 'mating) (display-time 'mating))))

(defun ms-propositional-real (etree)
  (setq active-mating (car (mating-list)))
  (let ((jform (etree-to-jform etree)))
  (funcall (get order-components 'init-jform-mspath) jform)          
;;(init-jform-mspath jform) ;;;The change is for order-components HX 1993-6-5
    (when (or (memq mating-verbose '(max med)) (eq query-user 'query-jforms))
      (msg T "Displaying VP diagram ..." t)
      (when (eq style 'concept-s) (tyol 85) (tyol 106 32))
      (display-vp-diag jform))
    (signal-event 'start-time)
    (let ((result nil))
      (case prop-strategy
	(hash-table
	  (setq result
		(find-open-msprop-hashtable-path
		  jform (cgraph) (max-cgraph-counter)))
	  (signal-event 'stop-time)
	  (unless result
	    (maphash #'(lambda (key val)
			 (push val result)
			 (setf (gethash val (connections-array)) (list key)))
		     (cgraph))
	    (setf (mating-clist active-mating) result)
	    (setq result nil)))
	(pushnew
	  (setq result (find-open-msprop-pushnew-path jform nil))
	  (signal-event 'stop-time)
	  (when (consp (car result))
	    (setf (mating-clist active-mating) result
		  result nil)))
	(t				;allow-duplicates
	  (setq result (find-open-msprop-push-path jform nil))
	  (signal-event 'stop-time)
	  (when (consp (car result))
	    (setf (mating-clist active-mating) result)
	    (setq result nil))))
      (runcount 'mating)
      (if result
	  (progn (msg T "The given formula is not valid as there's no"
		      " connection on the following path:" t (l result))
		 nil)
	  (progn (msg t "Eureka!  Proof complete.")
		 (vpwin-update-five active-mating)
		 (setf (mating-completep active-mating) t)
		 active-mating)))))


;;;As Frank pointed out there's a problem with the fo-unification algorithm in
;;;case we are unifying terms with bound variables. For example, consider the 
;;;problem "exists y. P[lambda x. x] implies P[lambda x. y]" 
;;;Hence, I am taking out the check for first-order problem at this time. We 
;;;will thus always be using the higher-order algorithm from now on (until
;;;the fo algorithm handles this case) SI 18OCT88

(defun ms (etree allow-duplications  &optional (time-limit nil))
  (unwind-protect  
      (ms-real etree allow-duplications time-limit)
      (progn (breakcount 'mating) (display-time 'mating))))

(defun ms-real (etree allow-duplications time-limit)
  (declare (special allow-duplications))
  (let* ((ms-jform (etree-to-jform etree))
	 (mate-ffpair-etree (or mate-ffpair first-order-mode-ms))
	 (mate-ffpair mate-ffpair-etree)
	 (next-open-path nil)
         (search-complete-paths nil)
	 (local-interrupt-enable interrupt-enable))
    (when (eq etree (eproof-etree current-eproof))
      (setf (eproof-jform current-eproof) ms-jform))
    (do ((next-action (funcall ms-dir nil nil)
		      (funcall ms-dir next-action result))
	 (result)
	 (connection)
	 (dup-needed nil)
	 (start-time (get-net-internal-run-time)))
	(nil)
      (signal-event 'stop-time)
      (finish-output)
      (when (and *test-top* (>= (truncate (/ (- (get-net-internal-run-time) start-time) internal-time-units-per-second))
				time-limit))
		 ;(>= (+ *test-total-time* (truncate (/ (- (get-net-internal-run-time) start-time) internal-time-units-per-second)))
		 ;    (+ time-limit test-initial-time-limit)))
	    (return-from ms-real (values 'test-fail nil)))
      (when (and interrupt-enable local-interrupt-enable)
	(handle-user-interrupt next-action connection result))
      (signal-event 'start-time)
      (if monitorflag (funcall (symbol-function *current-monitorfn*) 'new-action nil))
;      (msgf next-action)
      (case next-action
	(next-mating
	  (if (or (mating-list) (not mate-ffpair-etree))
	      (progn
		(unless (or (mating-list) mate-ffpair-etree)
		  (setq mate-ffpair-etree t)
		  (initialize-mating-search t))
		(signal-event 'stop-time)
		(let ((old-mating active-mating)
		      mating-list)
		  (multiple-value-setq (active-mating mating-list result)
		      (find-active-mating (mating-list)))
                  ;; result should be nil, if mating was not changed.
		  (setq result (or result (neq old-mating active-mating)))
		  (signal-event 'start-time)
		  (setf (mating-list) mating-list)
		  (when result
		    (when old-mating
		      (setf (mating-last-path old-mating) next-open-path)
		      (when (mating-clist active-mating)
			(setq next-open-path
			      (mating-last-path active-mating)
			      connection nil))
		      (signal-event 'mating-changed))
		    (setq result (if (mating-completep active-mating)
				     'complete
				     (if (mating-clist active-mating)
					 (progn
					   (setq local-interrupt-enable nil)
					   'ck-conns)
					 (if next-open-path
					     (progn
					       (setf (mating-first-path
						       active-mating)
						     next-open-path)
					       'next-conn)
					     (if search-complete-paths
						 'next-open-path
						 'find-next-conn))))
			  mate-ffpair mate-ffpair-etree))))
	      (setq result 'ck-duplicate)))
	(find-next-conn
	  (multiple-value-setq (next-open-path connection)
	      (find-next-openpath ms-jform next-open-path (cgraph)
				  (mating-clist active-mating) nil))
	  (setq result
		(if (and connection (numberp connection))
		    'add-connection
		    (if next-open-path 'remove-incomp-wrt-etree
			(if (mating-first-path active-mating)
			    (progn
			      (setq next-open-path nil)
			      (setf (mating-first-path active-mating) nil)
			      'first-path)
			    (progn (setf (mating-completep active-mating) t)
				   'complete))))))
	(next-open-path
	  (setq next-open-path
		(find-next-openpath ms-jform next-open-path (cgraph)
				    (mating-clist active-mating)))
	  (setq result
		(if next-open-path 'path
		    (if (mating-first-path active-mating)
			(progn
			  (setq next-open-path nil)
			  (setf (mating-first-path active-mating) nil)
			  'first-path)
			(progn (setf (mating-completep active-mating) t)
			       'complete)))))
	(complete-path
         (setq result
               (if next-open-path
                   (multiple-value-bind (path conn)
                       (complete-mspath (car next-open-path) next-open-path
                                        (cgraph) (mating-clist active-mating)
                                        search-complete-paths)
                     ;;(if path;;(eq path next-open-path)
                       ;;  (progn
                         ;;  (setq next-open-path path)
                           ;;(if search-complete-paths 'path
                             ;;  (if (setq connection conn)
                               ;;    (if (numberp conn) 'add-conn
                                 ;;      'find-next-conn)
                                   ;;'remove-incomp-wrt-etree)))
                     ;;'remove-incomp-wrt-etree)
                     (if (eq path next-open-path)
                         'remove-incomp-wrt-etree
                         (progn
                           (setq next-open-path path)
                           (if search-complete-paths 'path
                               (if (setq connection conn)
                                   (if (numberp conn) 'add-conn
                                       'find-next-conn)
                                   'remove-incomp-wrt-etree)))))
                   (if search-complete-paths 'next-path 'find-next-conn))))
	(next-connection
	  (multiple-value-bind (next-conn path closed)
	      (locate-connection-on-mspath
		next-open-path (cgraph) (mating-clist active-mating))
	    (if next-conn (setq result 'conn
				connection next-conn
				next-open-path path)
		(if closed (setq result
				 (if search-complete-paths
				     'next-open-path 'find-next-conn)
				 next-open-path path)
		    (setq result nil)))))
	(alt-connection
	  (setq connection
		(find-potential-connection
		  (car next-open-path) (cdr next-open-path)
		  (cgraph) (mating-clist active-mating)))
	  (if connection (setq result 'conn)
	      (setq result nil)))
	(add-connection
	  (signal-event 'added-conn connection)
	  (setq dup-needed nil)
	  (add-connection connection active-mating)
	  (if monitorflag (funcall (symbol-function *current-monitorfn*) 'added-conn (list (cons 'npfd t))))
;;;The next line is added to speed up the proof search (May, 30, 93 HX)
          (setf (mating-last-path active-mating) next-open-path)
	  (if first-order-mode-ms
	      (multiple-value-bind (flag substs)
		  (fo-compatible-p connection (mating-utree active-mating))
		(setq result flag)
		(if (eq flag 'fail)
		    (progn (setq local-interrupt-enable nil)
			   (signal-event 'incomp-mating))
		    (setf (mating-utree active-mating) substs)))
	      (multiple-value-bind (flag root subst-hash-table success-nodes)
		  (unifiable-p active-mating T)
		(setf (mating-utree active-mating) root)
		(setf (mating-subst-hash-table active-mating)
		      subst-hash-table)
		(setf (mating-success-nodes active-mating) success-nodes)
		(setq result flag)
		(when (eq flag 'fail)
		  ;;(modify-utree connection active-mating)
		  (setq local-interrupt-enable nil)
		  (signal-event 'incomp-mating)))))
	;;(remove-connection
	;;(signal-event 'removed-conn connection)
	;;(remove-connection connection active-mating))
	(remove-last-connection
	  (setq result (remove-last-connection active-mating))
	  (setq connection
		(if (and first-order-mode-ms connection (= connection result))
		    nil result))
	  (signal-event 'removed-conn result)
	  (unless (occurs-on-path-p result next-open-path)
;;; The next line is changed to speed up the proof search (May, 30, 93 HX)
;;;	    (setf (mating-first-path active-mating) next-open-path)))
	    (setf (mating-last-path active-mating) next-open-path)))
	(find-connection
	  (multiple-value-setq (connection next-open-path)
	      (locate-connection-on-mspath
		next-open-path (cgraph) (mating-clist active-mating)))
	  (unless (mating-clist active-mating)
	    (setf (mating-first-path active-mating) next-open-path))
	  (if connection (setq result t)
	      (setq result nil
		    next-open-path nil)))
	  
	(unifiable-p
	  (case mating-verbose 
              ((min med)
	       (msg T "Trying to unify mating:")
	       (msg (mating-clist active-mating)))
	      (max (msg T "Trying to unify mating:")
                   (print-clist (mating-clist active-mating) (connections-array))))
	  (if first-order-mode-ms
	      (if occurs-check
		  (progn
		    (setq result 'success)
		    (print-subst-stack (mating-utree active-mating)))
		  (let ((occurs-check t))
		    (declare (special occurs-check))
		    (multiple-value-bind (flag substs)
			(first-order-unification
			  (mating-utree active-mating))
		      (setq result flag)
		      (if (eq flag 'fail)
			  (setf (mating-utree active-mating) nil)
			  (progn
			    (setf (mating-utree active-mating) substs)
			    (print-subst-stack
			      (mating-utree active-mating)))))))
	      (multiple-value-bind
		  (flag root subst-hash-table success-nodes)
		  (unifiable-p active-mating)
		(setf (mating-utree active-mating) root)
		(setf (mating-subst-hash-table active-mating)
		      subst-hash-table)
		(setf (mating-success-nodes active-mating) success-nodes)
		(setq result flag)
		(if (eq flag 'fail)
		    (case unify-verbose 
			  (max (msg "  Not unifiable. Backtracking."))
			  ((med min) (msg "F"))
			  (t ))
		    (if (eq flag 'success)
			(msg "  Unifiable.")
			(msg T "No terminal success node at current depth."
			     ))))))
	(store-incomp-clist
	  (setf (incomp-clists)
		(store-incomp-clist (mating-clist active-mating)
				    (incomp-clists))))
	(modify-utree
         (modify-utree connection active-mating)
;;; The next line is changed to speed up the proof search (May 30, 93 HX)
;;;      (setq next-open-path (mating-first-path active-mating))
         (setq next-open-path (mating-last-path active-mating))
         (when (memq mating-verbose '(max med min))
               (msg t "PATH:" next-open-path))
         (setq local-interrupt-enable interrupt-enable))
	(modify-utree1
         (modify-utree connection active-mating)
         (setq next-open-path (mating-first-path active-mating))
         (when (memq mating-verbose '(max med min))
                (msg t "PATH:" next-open-path))
         (setq local-interrupt-enable interrupt-enable)
         (if next-open-path
             (setq result nil)
             (setq result (if search-complete-paths
                              'next-open-path 'find-next-conn))))
	(remove-incomp-mating
	  (setq connection nil)
	  (setq local-interrupt-enable interrupt-enable)
	  (do ((conns-removed -1 (1+ conns-removed)))
	      ((progn (signal-event 'mate-subsumed-test)
		      (not (subsumed-p (mating-clist active-mating)
				       (incomp-clists-wrt-etree))))
	       (when (and (plusp conns-removed) (mating-clist active-mating))
		 (setf (mating-utree active-mating) nil)
		 (setf (mating-first-path active-mating)
		       (mating-last-path active-mating))))
	    (signal-event 'mate-subsumed-true)
	    (setq connection (remove-last-connection active-mating))
	    (signal-event 'removed-conn connection))
	  (if (mating-clist active-mating)
	      (setq result t)
;;;I added 
;;;(setq next-open-path (mating-first-path active-mating))
;;;to rectify the next-path when mating-clis of active-mating is empty.
	      (progn (setq next-open-path (mating-first-path active-mating))
                     (setf (mating-list)
			   (delete active-mating (mating-list)))
		     (setq result nil))))
	(remove-incomp-wrt-etree
	  (if (mating-clist active-mating)
	      (if mate-ffpair
		  (let ((clist (copy (mating-clist active-mating))))
		    (push clist (max-incomp-clists-wrt-etree))
		    (setf (incomp-clists-wrt-etree)
			  (store-incomp-clist clist
					      (incomp-clists-wrt-etree)))
		    (setq connection nil))
		  (setq mate-ffpair t
			result 'alt-connection))
	      (progn
		(setf (mating-list) (delete active-mating (mating-list)))
		(setq result 'next-mating))))
        (init-path (funcall (get order-components 'init-jform-mspath) ms-jform))
;;;	(init-path (init-jform-mspath ms-jform)) ;;; The change is for order-components HX 1993-6-5
	(init-after-dup
	  (initialize-ms-after-dup)
	  (setq next-open-path nil)
	  (setq active-mating nil)) 
	(skolemize)
	(create-new-jform
	  (unless ms-jform
	    (setq ms-jform (etree-to-jform etree))
	    (when (eq (eproof-etree current-eproof) etree)
	      (setf (eproof-jform current-eproof) ms-jform))))
	(dup-all
	  (signal-event 'dupe)
	  (setq mate-ffpair-etree t
		duplicated t
		dup-needed nil
		etree (duplicate-all-vars etree)))
	(dup-outer
	  (signal-event 'dupe)
	  (setq mate-ffpair-etree t
		duplicated t
		dup-needed nil
		etree (duplicate-all-outer-vars etree)))
	(deepen
	  (when (memq mating-verbose '(max med min))
                (msg T "Deepening expansion tree."))
	  (setq etree (deepen-to-literals etree)
		ms-jform nil))
	(deepen-leaves
	  (when (memq mating-verbose '(max med min))
                (msg T "Deepening expansion tree."))
	  (setq etree (deepen-after-duplication etree)
		ms-jform nil))
	(init-ms (initialize-mating-search))
	(print-connection (print-connection connection (connections-array)))
	(print-clist (print-clist (mating-clist active-mating)
				  (connections-array)))
	(vp (when (or (memq mating-verbose '(max med)) (memq query-user '(query-jforms show-jforms)))
	      (msg T "Displaying VP diagram...." t)
	      (when (eq style 'concept-s) (tyol 85) (tyol 106 32))
	      (display-vp-diag ms-jform)
	      (setq result 'next-mating)
	      (if (eq query-user 'query-jforms) 
		  (let (discard)
 		    (progn (prompt-read discard nil (msgf "Search on this vpform ? ") 'yesno nil nil)
			 (if (not discard) (setq result 'ignore-this-one)))))
	      (if (eq query-user 'show-jforms) (setq result 'ignore-this-one))))
	(ck-duplicate
	  (if (or dup-needed (and duplicated (not new-mating-after-dup)))
	      (setq result t)
	      (progn (setq result nil
			   dup-needed t
			   next-open-path nil)
		     (push (init-mating) (mating-list)))))
	(success
	  (signal-event 'stop-time)
	  (when (eq etree (eproof-etree current-eproof))
		(runcount 'mating)
		(msg t "Eureka!  Proof complete.")
		(vpwin-update-five active-mating)))
	(post-processing
	  (when (eq etree (eproof-etree current-eproof))
	    (apply-substs-ms)
	    (when (memq mating-verbose '(max med)) (display-vp-diag ms-jform)))
          (runcount 'mating)
	  (return active-mating))
	(xx-abort
	  (signal-event 'stop-time)
	  (when allow-duplications
	    (msg t "Aborting proof search."))
	  (return nil))
	(T
	  (signal-event 'stop-time)
	  (throwfail "No action specified for " next-action))))))

(defun handle-user-interrupt (next-action connection result &optional (mtree nil) (mating nil))
  (declare (special *executing* restore-work-infile restore-file-p
		    next-action connection result))
  (when (or *executing* *using-interface*) (msg "."))
  (unless (or *executing* *using-interface*)
  (let ((in-stream (if (and (boundp 'restore-file-p)
			    restore-file-p)
		       restore-work-infile *standard-input*)))
    (if (listen in-stream)
	(progn 
	  (unless (and (boundp 'restore-file-p)
		       restore-file-p)
	    (msgf "Interrupting mating search."))
	  (let ((interrupt-char
		  (peek-char nil in-stream nil nil)))
	    (if (char= interrupt-char control-g-char)
		(progn
		  (msgf "Aborting mating search." t t)
		  (read-char nil in-stream nil nil)
		  (read-char nil in-stream nil nil)
		  ;these two lines discard the ^G+return
		  (throw-^g))
		(case interrupt-char
		  (#\newline 
		   (read-char in-stream)
		   (msgf "Entering new mating-search top-level." t
			 "Type LEAVE at top level to return to search." t)
		   (let ((hrl *hacked-rewrites-list*)
			 (bcl *banned-conns-list*)
			 (ho-bcl *ho-banned-conns-list*)
			 (idl *instantiated-defs-list*)
			 (iel *instantiated-eqs-list*))
		   (if mtree (mtree-mating current-eproof nil nil nil)
		       (mate-wff current-eproof nil nil nil))
		   (setq *hacked-rewrites-list* hrl *banned-conns-list* bcl
			 *ho-banned-conns-list* ho-bcl *instantiated-defs-list* idl
			 *instantiated-eqs-list* iel))
		   )
		  ((#\t #\T)
		   (read-char in-stream) (read-char in-stream)
		   (breakcount 'mating-ctr) (display-time 'mating-ctr) (runcount 'mating-ctr))
		  ((#\m #\M)
		   (read-char in-stream) (read-char in-stream)
		   (if mtree (mst-show-mating)
		     (if mating (if (and (listp mating) (eq t (car mating)))
				    (msg (parse-clist mating))
				  (ms90-3-print-mating mating))
		       (show-mating))))
		  ((#\q #\Q)
		   (read-char in-stream) (read-char in-stream)
		   (when *doing-prenex* (throw 'next 'fail)))
		  (#\. 
		   (read-line in-stream);;says skip if reading from file
		   )
		  (t 
		    (msgf "Ignoring extraneous interrupt.")
		    (read-char in-stream)
		    )))))
	(progn
	  (when restore-file-p (throw 'core::restore-file-end t))
	  (msg ".")
	  ;(when saving-work-p
	  ;  (write-line "." save-work-output-stream))
	  ;no idea what this line was for. MB Thu Aug 24 14:29:45 1995
	  )))))

(defun quasi-tps1 (pre-action result)
  (declare (special allow-duplications))
  (case pre-action
    (next-mating
      (case result
	((nil) 'alt-connection)
	(next-conn
	  ;;(if search-complete-paths 'next-open-path 'find-connection)
	  'next-connection)
	(complete 'unifiable-p)
	(ck-conns 'remove-incomp-mating)
	(next-open-path 'next-open-path)
	(find-next-conn 'find-next-conn)
	(ck-duplicate 'ck-duplicate)))
    (next-open-path
      (case result
	(path 'next-connection)
	(first-path 'next-open-path)
	(complete 'unifiable-p)))
    (complete-path
      (case result
	(path 'next-connection)
	(next-path 'next-open-path)
	(add-conn 'add-connection)
	(remove-incomp-wrt-etree 'remove-incomp-wrt-etree)
	(find-next-conn 'find-next-conn)))
    (find-next-conn
      (case result
	(add-connection 'add-connection)
	(remove-incomp-wrt-etree 'remove-incomp-wrt-etree)
	(first-path 'find-next-conn)
	(complete 'unifiable-p)))
    (next-connection
      (case result
	(next-open-path 'next-open-path)
	(find-next-conn 'find-next-conn)
	(conn 'add-connection)
	(t 'complete-path)))
    (alt-connection
      (if result 'add-connection 'complete-path))
    (add-connection 
      (if (eq result 'fail) 'store-incomp-clist
	  (if search-complete-paths 'next-open-path 'find-next-conn)))
    (unifiable-p
      (case result
	(fail 'store-incomp-clist)
	(success 'success)
	(more (if ;interrupt-enable
                  (eq query-user T) ; Changed this 22MAY90 DAN ; changed again 7/23/93 MB
		  (if (query "Continue with increased unification depth?" '$)
		      (if (mating-utree-depth active-mating)
			  (if max-utree-depth
			      (progn (incf (mating-utree-depth active-mating) 3) 'unifiable-p)
			    (progn (setf (mating-utree-depth active-mating) nil) 'unifiable-p)) 
					;the line above shouldn't really happen, but we can recover if it does.
			'store-incomp-clist) ;this also shouldn't happen.
		      (progn
			(setf (incomp-clists)
			      (store-incomp-clist (mating-clist active-mating)
						  (incomp-clists)))
			(setf (mating-bktrack active-mating)
			      (if (integerp (bktrack-limit)) (1+ (bktrack-limit)) (bktrack-limit))) 
			'next-mating))
		  (progn
		    (when (mating-utree-depth active-mating) 
			  (if max-utree-depth 
			      (incf (mating-utree-depth active-mating) 3)
			    (setf (mating-utree-depth active-mating) nil)))
		    (setf (mating-bktrack active-mating) (if (integerp (bktrack-limit)) (1+ (bktrack-limit)) (bktrack-limit)))
		    (setf (incomp-clists-wrt-etree)
			  (store-incomp-clist (mating-clist active-mating)
					      (incomp-clists-wrt-etree)))
		    'next-mating)))))
    (store-incomp-clist 'remove-last-connection)
    (remove-last-connection  ;;;remove-connection
     'modify-utree)
    (remove-incomp-mating
      (if result 'modify-utree1 'next-mating))
    (modify-utree1
      (case result
	(next-open-path 'next-open-path)
	(find-next-conn 'find-next-conn)
	(t 'next-connection)))
    (modify-utree 'next-mating)
    ;; If interrupts disabled, will automatically duplicate.
    (remove-incomp-wrt-etree
      (if (eq result 'alt-connection) 'alt-connection
	  (if (eq result 'next-mating) 'next-mating
	      'remove-incomp-mating ;;'remove-last-connection
	      )))
    ;;((nil) 'init-ms)
    (init-path 'vp)
    ((dup-all dup-outer) 'deepen-leaves)    
    ((deepen deepen-leaves) 'init-after-dup)
    ((nil init-after-dup) 'create-new-jform)
    (create-new-jform 'init-path)
    (vp (if (eq result 'ignore-this-one)
	    duplication-strategy
	  'next-mating))
    (ck-duplicate
      (if result
	  (if allow-duplications
	      (if ;interrupt-enable 
                  (eq query-user T) ;; Changed this 22MAY90 DAN ; and again 7/23/93 MB
		  (if (query "Duplication required. Continue?" '$)
		      duplication-strategy 'xx-abort)
		  duplication-strategy)
	      'xx-abort)
	  'next-mating))
    (print-clist 'success)
    (success 'post-processing)
    (T (throwfail "No next action specified for " pre-action))))

