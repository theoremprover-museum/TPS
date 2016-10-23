(in-package :auto)

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
	 (start-time (tps-get-internal-run-time)))
	(nil)
      (signal-event 'stop-time)
      (finish-output)
      (when (and *test-top* 
		 (>= (+ *test-total-time* (truncate (/ (- (get-net-internal-run-time) start-time) internal-time-units-per-second)))
		     (+ time-limit test-initial-time-limit)))
	    (return-from ms-real (values 'test-fail nil)))
      (when (and interrupt-enable local-interrupt-enable)
	(handle-user-interrupt next-action connection result))
      (signal-event 'start-time)
      (if monitorflag (funcall (symbol-function *current-monitorfn*) 'new-action nil))
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
            (progn (runcount 'mating)
	           (msg t "Eureka!  Proof complete.")
		   (vpwin-update-five active-mating))))
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