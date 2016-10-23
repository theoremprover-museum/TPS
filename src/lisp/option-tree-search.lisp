;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :auto)
(part-of ms89)

;;; Written by Dan Nesmith

(deffile option-tree-search
  (part-of ms89)
  (extension lisp)
  (mhelp "Contains code implementing search procedure for option trees.
    Applies to MS89.  In this version of the file, after backtracking
    TPS continues working on the same path, which prevents floundering."))


;; choose lowest positively-ranked eproof
(defun choose-lowest-eproof (option-tree)
  (do* ((best nil)
        (queue nil
	       (append queue (option-tree-children current)
		       (if (option-tree-back current)
			   (list (option-tree-back current)))))
	(current option-tree
		 (pop queue)))
;	(current-ranking (option-tree-ranking current)
;			 (if current (option-tree-ranking current))))
      ((null current) best)
    (when (and (> (option-tree-ranking current) 0)
               (if (and max-search-limit (> max-search-limit
                                            (option-tree-tried
                                             current)))
                   t
                   (not max-search-limit))
	       (if best 
		   (< (option-tree-ranking current) 
		      (option-tree-ranking best))
		   t))
	(setq best current))))

(defun adjust-option-tree-ranking (last-eproof-attempted give-up)
  (when give-up (msg "Giving up on this option." t))
  (setf (option-tree-ranking last-eproof-attempted)
	(if give-up 0 (* 2 (option-tree-ranking
                            last-eproof-attempted))))
  (when give-up
    (do ((option-tree* (option-tree-parent last-eproof-attempted)
		       (option-tree-parent option-tree*)))
	((null option-tree*))
      (setf (option-tree-ranking option-tree*) 0))))

;;;Here is the main function in this file:
;;;Since auto-search calls msearch, this is unnecessary.
#+comment
(defun auto-search (eproof)
  (unwind-protect  
      (auto-search-real eproof)
      (progn (breakcount 'mating) (display-time 'mating))))


(defun auto-search (eproof)
  (let ((*option-tree-ms* default-ms))
  (set-up-option-tree-root eproof)
  (case prim-bdtypes-auto
	((replace) (setq prim-bdtypes 
                         (find-prim-types (get-deep (eproof-etree eproof)))))
	((replace-sub) (setq prim-bdtypes (find-prim-subtypes (get-deep (eproof-etree
                                                 eproof)))))
	((append) (setq prim-bdtypes 
                        (union prim-bdtypes (find-prim-types (get-deep (eproof-etree eproof))))))
	((append-sub) (setq prim-bdtypes 
                        (union prim-bdtypes (find-prim-subtypes (get-deep (eproof-etree eproof)))))))
  (clrhash prim-hashtable)
  (do ((current-option-tree 
	 (choose-lowest-eproof option-tree-root)
	 (if (memq flag '(xx-abort succeed user-abort))
	     current-option-tree
	     (choose-lowest-eproof option-tree-root)))
       (give-up nil)
       (flag nil))
      ((or (null current-option-tree) 
	   (eq flag 'xx-abort) 
	   (eq flag 'succeed)
	   (eq flag 'user-abort)
	   (eq flag 'test-fail))
       (finish-up-search current-option-tree flag master-eproof 
			 option-tree-root))
    (let ((current-eproof (option-tree-eproof current-option-tree)))
      (msgf "Choosing eproof:" current-option-tree ": Ranking: "
	    (option-tree-ranking current-option-tree)t)
      (print-option-tree-subs current-option-tree)
      (multiple-value-setq (flag give-up)
	(catch 'option-tree-search-kill 
	  (limited-mating-search current-option-tree)))
      (when (eq flag 'fail) 
	(msgf "Failed to find proof." t)
	(adjust-option-tree-ranking current-option-tree give-up)
	(setq flag (decide-to-continue current-option-tree
				       option-tree-root give-up flag))
	(unless (or (eq flag 'test-fail) (eq flag 'user-abort))
	  (when (decide-to-update-option-tree option-tree-root flag)
            (msgf "Computing new options to try." t)
            (update-option-tree option-tree-root))))))))
      
(defun find-prim-types (wff)
  (let ((prim-type-list nil))
    (declare (special prim-type-list))
    (find-prim-types-aux wff)
    (delete 'nil (delete 'O (delete-duplicates prim-type-list)))))

(defun find-prim-types-aux (wff)
  (cond ((symbolp wff)
         (find-prim-types-atom (get wff 'type)))
        ((exp-var-p wff)
         (find-prim-types-aux (exp-var-subst wff)))
        ((skolem-term-p wff)
         (find-prim-types-aux (skolem-term-term wff)))
        (t (find-prim-types-aux (car wff))
           (find-prim-types-aux (cdr wff)))))

(defun find-prim-types-atom (tp)
  (declare (special prim-type-list))
  (if (atom tp)
      (push tp prim-type-list)
      (progn (find-prim-types-atom (car tp))
             (find-prim-types-atom (cdr tp)))))

(defun find-prim-subtypes (wff)
  (let ((prim-type-list nil))
    (declare (special prim-type-list))
    (find-prim-subtypes-aux wff)
    (delete 'nil (remove-if #'(lambda (x) (and (listp x) 
					       (or (and (consp (last x)) (eq (cdr (last x)) 'O))
						   (eq (last x) 'O))))
				; not sure that (last x) can be anything but a consp, but it can't hurt...
				(delete 'O (remove-duplicates prim-type-list :test 'equal-type-p))))))

(defun find-prim-subtypes-aux (wff)
  (cond ((symbolp wff)
         (find-prim-subtypes-atom (get wff 'type)))
        ((exp-var-p wff)
         (find-prim-subtypes-aux (exp-var-subst wff)))
        ((skolem-term-p wff)
         (find-prim-subtypes-aux (skolem-term-term wff)))
        (t (find-prim-subtypes-aux (car wff))
           (find-prim-subtypes-aux (cdr wff)))))

(defun find-prim-subtypes-atom (tp)
  (declare (special prim-type-list))
  (if (atom tp)
      (push tp prim-type-list)
      (progn (push (car tp) prim-type-list)
	     (push (cdr tp) prim-type-list)
	     (find-prim-subtypes-atom (car tp))
             (find-prim-subtypes-atom (cdr tp)))))

       
#+comment(defun get-initial-expansions (etree)
  (typecase etree
    (expansion 
     (mapcar #'(lambda (x) (cons x (expansions-below x)))
	     (etree-components etree)))
    (otherwise
     (mapcan #'get-initial-expansions (etree-components etree)))))

(defun get-initial-expansions (etree)
  (let* ((exps (find-exp-nodes etree))
	 (lowest-exps
	  (remove-if #'(lambda (y)
			 (find-if #'(lambda (x) (find-etree-node 
						 #'expansion-p x))
				  (etree-components y)))
		     exps)))
    (mapcan #'(lambda (exp)
		(mapcar #'expansions-above (etree-components exp)))
	    lowest-exps)))



(defun set-up-option-tree-root (eproof)
  (setq master-eproof eproof)
  (setq option-tree-root
	  (make-option-tree :eproof (copy-eproof* master-eproof) 
			    :free-vars 
			    (get-initial-expansions 
			     (eproof-etree eproof))))
  (setf (symbol-value (option-tree-name option-tree-root))
	option-tree-root)
  (init-option-tree option-tree-root))

    
(defun finish-up-search (current--option-tree flag master--eproof 
			 master--eproof-root)
  (declare (ignore master--eproof))
  (if current--option-tree
    (case flag
      (xx-abort (msgf "Mating search aborted." t) nil)
      (user-abort (msgf "Mating search aborted at user request." t) nil)
      (fail (msgf "All current search possibilities have been exhausted." 
		  t ) nil)
      (test-fail (msgf "Search aborted by test-top." t) nil)
      (succeed ;;;(runcount 'mating) hx: Since auto-search call msearch, this is unnecessary.
	       (msgf "An expansion proof has been found." t)
	       (setq current-eproof (option-tree-eproof current--option-tree))
	       (option-tree-name master--eproof-root)))
    (progn
      (msgf "All options have been exhausted.  Try increasing the value of
the flag MAX-SEARCH-LIMIT.") nil)))


(defun decide-to-continue (current-option-tree option-tree-root
						give-up flag)
  (declare (ignore flag give-up) (special current-option-tree option-tree-root))
  (if ;interrupt-enable
      (eq query-user T) ; Changed 22MAY90 DAN and again 7/23/93 MB
      (query "Continue mating search?" 'yes)
      nil))

(defun decide-to-update-option-tree (option-tree-root flag)
  (declare (ignore flag))
  (let ((percent-tried (find-percent-option-trees-changed option-tree-root)))
    (if (>= percent-tried 0.99)
	(if (not ;interrupt-enable
               (eq query-user T) ; Changed 22MAY90 DAN and again 7/23/93 MB
               ) 
	    t
	    (query "More than 99% of the available choices have been
tried.  Do you want to add more expansions (if possible)?" 'yes))
	nil)))

(defun find-percent-option-trees-changed (option-tree-root)
  (do* ((node option-tree-root (car nodes))
	(nodes (if node 
		   (append (option-tree-children node)
			   (if (option-tree-back node)
			       (list (option-tree-back
				       node)))))
	       (if node 
		   (append (cdr nodes)
			   (option-tree-children node)
			   (if (option-tree-back node)
			       (list (option-tree-back
				       node))))
		   (cdr nodes)))
	(num-tried (cond ((null node) 0)
                         ((zerop (option-tree-ranking node)) 
                          1)
                         ((zerop (option-tree-tried node)) 0)
                         (max-search-limit 
                          (if (>= (option-tree-tried node)
                                  max-search-limit)
                              1 0))
                         (t 1))
                   (cond ((null node) num-tried)
                         ((zerop (option-tree-ranking node)) 
                          (1+ num-tried))
                         ((zerop (option-tree-tried node)) num-tried)
                         (max-search-limit 
                          (if (>= (option-tree-tried node)
                                  max-search-limit)
                              (1+ num-tried)
                              num-tried))
                         (t (1+ num-tried))))
	(total 1 (if node (1+ total) total)))
      ((null node) (/ num-tried total))))


(defun limited-mating-search (current-option-tree)
  (let* ((current-eproof (option-tree-eproof current-option-tree))
	 (first-order-mode-ms 
	  (not (find-if #'higher-order-var
			(free-vars-in-etree
			 (eproof-etree current-eproof)))))
        (max-time (or search-time-limit 
		    (if first-order-mode-ms
			8 
			30))))
    (setq *test-total-time* (+ *test-total-time* max-time))
    (incf (option-tree-tried current-option-tree) max-time)
    (case *option-tree-ms*
      ((ms89 ms88)
       (limited-ms (eproof-etree current-eproof) max-time))
      ((ms90-3 ms90-9)
       (ms90-3-controller (eproof-etree current-eproof) max-time))
      ((ms92-9 ms93-1)
       (ms92-9-controller (eproof-etree current-eproof) max-time)))))

(defun limited-ms (etree time-limit)
  (unwind-protect  
      (limited-ms-real etree time-limit) 
      (progn (breakcount 'mating) (display-time 'mating))))

(defun limited-ms-real (etree time-limit)
  (let* ((dummy-var (when (lazy2-used) (fiddle-with-def-leaves etree)))
	 (ms-jform (etree-to-jform etree))
	 (mate-ffpair-etree (or mate-ffpair first-order-mode-ms))
	 (mate-ffpair mate-ffpair-etree)
	 (next-open-path nil)
	 (local-interrupt-enable t)
	 (allow-duplications nil)
	 (search-complete-paths nil)
	 (start-time (get-net-internal-run-time)))
    (declare (special allow-duplications) (ignore dummy-var))
    (when (and active-mating
	       (not (null (mating-clist active-mating)))
	       (null (mating-utree active-mating)))
	  (multiple-value-bind (r s)
		(initialize-utree 
		 (clist-to-dpairs (mating-clist active-mating))
		 (free-vars-in-etree current-eproof))
		(setf (mating-utree active-mating) r)
		(setf (mating-subst-hash-table active-mating) s)))
    (do ((next-action (funcall ms-dir nil nil)
		      (funcall ms-dir next-action result))
	 (result)
	 (connection)
	 (dup-needed nil))
	(nil)
      (signal-event 'stop-time)
      (finish-output)
      (when (and *test-top* 
		 (>= (+ *test-total-time* (truncate (/ (- (get-net-internal-run-time) start-time) internal-time-units-per-second)))
		     (+ time-limit test-initial-time-limit)))
	    (return-from limited-ms-real (values 'test-fail nil)))
      (when (and local-interrupt-enable (not (eq result 'fail)))
        (when (and (not (eq result 'success))
                   (>= (/ (- (get-net-internal-run-time) start-time)
                          internal-time-units-per-second)
                       time-limit))
	(return-from limited-ms-real (values 'fail nil)))
        (when interrupt-enable 
          (handle-user-interrupt next-action connection result)))
      (signal-event 'start-time)
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
		  (setq result (or result (neq old-mating active-mating)))
		  (signal-event 'start-time)
		  (setf (mating-list) mating-list)
		  (when result
		    (when old-mating
		      (setf (mating-last-path old-mating) next-open-path)
		      (when (mating-clist active-mating)
			    (when (not (mating-utree active-mating))
				  (multiple-value-bind (r s)
						       (initialize-utree 
							(clist-to-dpairs (mating-clist active-mating))
							(free-vars-in-etree current-eproof))
						       (setf (mating-utree active-mating) r)
						       (setf (mating-subst-hash-table active-mating) s)))
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
			   (if path;;(eq path next-open-path)
			       (progn
				 (setq next-open-path path)
				 (if search-complete-paths 'path
				   (if (setq connection conn)
				       (if (numberp conn) 'add-conn
					 'find-next-conn)
				     'remove-incomp-wrt-etree)))
			     'remove-incomp-wrt-etree))
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
;;;added into ms89 as well ... MB 11/22/93
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
		  (unifiable-p active-mating t start-time time-limit)
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
;;; added into ms89 by MB 11/22/93
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
		  (unifiable-p active-mating nil start-time time-limit)
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
;;; added into MS89 by MB 22/11/93
;;;      (setq next-open-path (mating-first-path active-mating))
         (setq next-open-path (mating-last-path active-mating))
         (setq local-interrupt-enable interrupt-enable))
	(modify-utree1
         (modify-utree connection active-mating)
         (setq next-open-path (mating-first-path active-mating))
         (setq local-interrupt-enable interrupt-enable)
         (if next-open-path
             (setq result nil)
             (setq result (if search-complete-paths
                              'next-open-path 'find-next-conn))))
	(remove-incomp-mating
	  (setq connection nil)
	  (setq local-interrupt-enable t)
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
;;; added into MS89 by MB 22/11/93
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
	(init-path (funcall (get order-components 'init-jform-mspath)
                            ms-jform))
	(init-after-dup
	  (initialize-ms-after-dup)
	  (setq next-open-path nil)
	  (setq active-mating nil)) 
	(skolemize)
	(create-new-jform
	  (unless ms-jform
	    (setq ms-jform (etree-to-jform etree))
	    (if (and (eq default-ms 'ms88) *test-top*) (when (eq (eproof-etree current-eproof) etree)
							     (setf (eproof-jform current-eproof) ms-jform)))
;the two lines above were commented out before...
            ))
	(dup-all (if (and (eq default-ms 'ms88) *test-top*) (progn (signal-event 'dupe)
								  (setq mate-ffpair-etree t
									duplicated t
									dup-needed nil
									etree (duplicate-all-vars etree)))
		   (return (values 'fail t))))
	(dup-outer (if (and (eq default-ms 'ms88) *test-top*) (progn (signal-event 'dupe)
								     (setq mate-ffpair-etree t
									   duplicated t
									   dup-needed nil
									   etree (duplicate-all-outer-vars etree)))
		     (return (values 'fail t))))
;everything before the (return...) was previously commented out of both of the above...
	(deepen
	  (msg T "Deepening expansion tree.")
	  (setq etree (deepen-to-literals etree)
		ms-jform nil))
	(deepen-leaves
	  (msg T "Deepening expansion tree.")
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
		  (progn (let (discard) (prompt-read discard nil (msgf "Search on this vpform ? ") 'yesno nil nil)
			 (if (not discard) (return-from limited-ms-real (values 'fail nil))))))
	      (if (eq query-user 'show-jforms) (return-from limited-ms-real (values 'fail nil)))))
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
	    (when (neq mating-verbose 'min) (display-vp-diag ms-jform)))
	  (return (values 'succeed nil)))
	(xx-abort
	  (signal-event 'stop-time)
	  (when allow-duplications
	    (msg t "Aborting proof search."))
	  (return (values 'fail t)))
	(T
	  (signal-event 'stop-time)
	  (throwfail "No action specified for " next-action))))))



