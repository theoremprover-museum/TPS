;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of ms90-3)

(deffile ms90-3-top
  (part-of ms90-3)
  (mhelp "Main functions implementing Path-focused duplication.
Detailed description in the file."))

;;;1. assumption: jform has no existential variables.
;;;2. active-path is the portion of path in active-jform. open-path is 
;;;   (append open-path active-path)
;;;3. To simplify the task of path-enumerator in deciding whether it
;;;   has reached the top of a duplicated jform, we'll copy the
;;;   topmost universal node and set it's parent to NIL. This copy
;;;   will be saved in the DUP attribute of the universal node, and
;;;   used instead.
;;;4. Each dpair in unif-problem is of the form (var . (term . env)) 
;;;   var is of the form (var . index)
;;;5. DUP-RECORD contains the following types of elements:
;;;    a) jform --- adding a duplication
;;;    b) (var . index) adding an expansion variable to the active-path
;;;    c) (var1 ... varN) Removing a sequence of variables from the active-path
;;;    d) ((lit1 . lit2) . subst-stack) adding a connection. subst-stack is
;;;    the solution to the unification problem (unify lit1 lit2)
;;;    e) ((((open-path . active-path) . active-jform) . env) . dup-jform)
;;;       After removing DUP-JFORM the others reflect the current
;;;       state. ENV is the environment for free variables in DUP-JFORM.
;;;       Meaningful only for jforms which are nested in other
;;;       universal jforms.
;;;    f) (disj-jform . bkup-disj) to indicate that at this stage the
;;;    path-enumerator backed up over the disjunction DISJ-JFORM.
;;;    Useful only while restoring path during backtracking.
;;;    g) (pre-conn . conn) to indicate that an earlier connection
;;;    closed the path at this stage. Used only when restoring an
;;;    earlier path in backtracking.
;;;6. In a connection (lit1 . lit2) lit1 always occurs later than lit2 on a
;;;   path.
;;;7. For convenience in testing, env-stack is initialized to (var . -1).
;;;8. If there are no connections in a duplicated jform J, then make
;;;enough copies of J so that further duplications of J at this stage
;;;are not possible.

(defvar *ms90-3-cgraph* (make-hash-table :test #'equal))
(defvar dup-record)

(defun msearch (jform time-limit)
   (unwind-protect
     (catch 'in-test-top-for-bktrk (msearch-real jform time-limit))
     (progn (breakcount 'mating) (display-time 'mating))))

(defun msearch-real (jform time-limit)
  (declare (special dup-immediately *temp-num-of-dups*))
  (case (jform-type jform)
    (disjunction
     (unless (disjunction-components jform)
       (runcount 'mating)
       (return-from msearch-real (values (list nil) nil (make-node)))))
    (conjunction
     (unless (conjunction-components jform)
       (runcount 'mating)
       (return-from msearch-real (values nil nil nil)))))
  (let ((active-jform jform)
	(state-stack nil)
	(open-path nil)
	dup-p)
    (set-max-mates active-jform)
    (mark-ext-exp-nodes active-jform)
    (funcall (get order-components 'sort-ms90-3-jform) 
             active-jform #'number-of-vertical-paths-main t)
    (setren-counter h-var-prefix 0)
    (setren-counter w-var-prefix 0)
    (setq neg-h-var-list nil)
    (setq imitation-h-var-list nil)
    (if first-order-mode-ms
        (msg t "Running in First-order mode." t)
        (msg t "Running in Higher-order mode." t))
    (clrhash *ms90-3-cgraph*)
    (when (eq style 'concept-s) (tyol 85) (tyol 106 32))
    (when (neq mating-verbose 'silent)
          (msg T "Displaying VP diagram ..." t)
          (display-vp-diag ms90-3-jform))
    (finish-output)
    (case query-user 
     (show-jforms
        (if (memq default-ms '(ms93-1 ms90-3 ms90-9 ms91-7))
 	    (return-from msearch-real (values nil nil)))
        (if (eq default-ms 'ms92-9)
            (throw 'duplicate-everything-now (values nil 'bigger nil))))
     (query-jforms
        (when (memq default-ms '(ms93-1 ms90-3 ms90-9 ms91-7))
           (let (discard)
	      (prompt-read discard nil 
                           (msgf "Search on this vpform ? ") 'yesno nil nil)
              (if (not discard) (return-from msearch-real (values nil nil)))))
        (when (eq default-ms 'ms92-9)
	   (let (discard)
              (prompt-read discard nil 
                           (msgf "Search on this vpform ? ") 'yesno nil nil)
              (if (not discard)
                  (throw 'duplicate-everything-now (values nil 'bigger nil)))))))
    (multiple-value-bind
	(conn-added-p active-path unif-prob env-stack dup-record exp-index
		      mating touchedp)
	(find-cheapest-path
	 active-jform nil nil nil default-env-stack nil 1 nil (get-net-internal-run-time) time-limit)
      (setq dup-p (not conn-added-p))
      (do ((temp-start-time (get-net-internal-run-time))
	   (out-of-time-p nil
			  (if time-limit
			      (>= (/ (- (get-net-internal-run-time) temp-start-time) 
                                     internal-time-units-per-second)
				  time-limit)))
	   (test-out-of-time-p nil
			       (and *test-top* 
				    (>= (+ *test-total-time* 
                                           (truncate (/ (- (get-net-internal-run-time) 
                                                           temp-start-time) 
                                                        internal-time-units-per-second)))
					(+ time-limit test-initial-time-limit))))
	   (dummy-variable (when interrupt-enable (handle-user-interrupt nil nil nil nil mating))
			   (when interrupt-enable (handle-user-interrupt nil nil nil nil mating)))
	   (success-p (not (or active-path state-stack))
		      (not (or active-path state-stack)))
	   (print-mating-ctr 0 (1+ print-mating-ctr))
           (alt-conn-p nil nil))
	  ((or success-p out-of-time-p test-out-of-time-p)
	   (if success-p
	       (progn
                 (setq mating (transform-mating mating))
                 (setq dup-record (transform-dup-record dup-record))
		 (runcount 'mating);;;hx....
                 (when (neq mating-verbose 'silent)
		     (msg t "Proof found.")
		     (msg -2)
		     (vpwin-update-six mating)
		     (do ((mating mating (cddr mating)))
		         ((null mating) (msg t))
		         (msg t (caar mating) 3 (cadr mating)))
		     (finish-output))
		 (values (or dup-record (list nil)) mating (or unif-prob (make-node))))
               (if test-out-of-time-p (values 'test-fail nil) (values nil nil))))
	  (declare (ignore dummy-variable))
	  (when (> print-mating-ctr print-mating-counter)
	  (setq print-mating-ctr 0)
          (stringdtl)
	  (ms90-3-print-mating mating))
	(finish-output)

        (if active-path
	    (progn
	      (if dup-p (setq dup-p nil)
		(multiple-value-setq
		      (conn-added-p active-path unif-prob env-stack dup-record
				    exp-index mating touchedp)
		    (find-alt-path-top
		     (car active-path) (cdr active-path) open-path unif-prob
		     env-stack dup-record exp-index mating conn-added-p
		     nil temp-start-time time-limit)))
              (unless (or active-path state-stack)
                (when (neq mating-verbose 'silent)
		      (msg t "Complete Mating: ")
                      (ms90-3-print-mating mating))
		(when *temp-num-of-dups* (setq num-of-dups *temp-num-of-dups*))
		(when (memq mating-verbose '(med max))
		      (msg t "Calling Unification:")
		      (stringdtl)
		      (finish-output))
                (setq touchedp (find-success-node unif-prob))
                (setq alt-conn-p (not touchedp)))
	      (unless (if active-path conn-added-p touchedp)
;;;active-path is complete or empty. active-path will be empty if a duplicated
;;;jform has no connections. In this case env-stack will represent the env for
;;;active-jform. in this case there must be a proof at a lower 
;;;depth, for example, one in which this duplicated jform does not occur. if
;;;active-path is empty then backtrack, else try to duplicate.
		(let ((dup-jform nil))
		  (when (and active-path (not (eq active-path 'alt-conn)))
		    (multiple-value-setq
			(dup-jform conn-added-p active-path open-path
				   state-stack unif-prob env-stack dup-record
				   exp-index mating)
		      (check-duplication
		       active-jform (if (consp (car active-path)) active-path
					(cons env-stack active-path))
		       open-path state-stack unif-prob dup-record exp-index
		       nil mating (if (= ms90-3-dup-strategy 1) nil
                                      (if state-stack active-jform nil)) temp-start-time time-limit)))
		  (when dup-jform (setq active-jform dup-jform))
		  (unless conn-added-p
		    (multiple-value-setq
			(conn-added-p active-jform active-path open-path
				      state-stack unif-prob env-stack
				      dup-record exp-index mating)
		      (backtrack-top
		       active-jform open-path state-stack unif-prob env-stack
		       dup-record exp-index mating
                       (or active-path alt-conn-p) temp-start-time time-limit))
		    (unless conn-added-p
		      ;;active-path may no longer be complete as it may
		      ;;have changed during backtracking. It will
		      ;;be complete though, if it ends with an env-stack.
		      (unless (consp (car active-path))
			(multiple-value-setq
			    (conn-added-p active-path unif-prob env-stack
					  dup-record exp-index mating)
			  (complete-alt-path
			     (car active-path) active-path open-path unif-prob
			     env-stack dup-record exp-index mating temp-start-time time-limit)))
		      ;;active-path is now complete. On the next iteration
		      ;;try to duplicate. 
		      (setq dup-p (not conn-added-p)))))))
	    (let ((pre-state (pop state-stack))
                  (aj active-jform))
	      ;;remove duplication
	      (push (acons pre-state env-stack active-jform) dup-record)
	      ;;restore the values prior to this duplication
	      (setq active-jform (cdr pre-state)
		    open-path (caar pre-state)
		    active-path (cdar pre-state)
		    env-stack (car active-path)
		    active-path (cdr active-path)
		    conn-added-p  (lit-in-mating-p (car active-path) 
                                                   (cdr active-path) mating))
              (when (eq mating-verbose 'max)
		    (let ((*print-pretty* nil))
		      (msg t "All Extensions of path" t (l active-path)
			   t "in jform " aj t "are closed.")
		      (msg t "Mating: " mating t)))))))))

(defun check-duplication (active-jform active-path open-path state-stack
				       unif-prob dup-record exp-index pre-dup
				       mating &optional (next-dup nil)
				       (start-time nil) (time-limit nil))
  (multiple-value-bind (dup-jform env)
      (find-next-dup
       active-path open-path pre-dup state-stack mating next-dup)
    (if dup-jform
	(let ((new-open-path (append active-path open-path)))
          (when (eq mating-verbose 'max)
		(let ((*print-pretty* nil))
		  (msg t "Duplicating:" t 5 dup-jform t "on path" t 5
		       (l new-open-path))))
	  (multiple-value-bind
	      (conn-added-p path unif-prob env-stack dup-record exp-index
			    mating)
              (find-cheapest-path
               dup-jform nil new-open-path unif-prob env
               (cons dup-jform dup-record) exp-index mating start-time time-limit)
            (when (and (not conn-added-p) (eq mating-verbose 'max))
              (msg t "But no connection was added."))
            (values dup-jform conn-added-p path new-open-path
                    (cons (acons open-path active-path active-jform)
                          state-stack)
                    unif-prob env-stack dup-record exp-index mating)))
	(values nil nil active-path open-path state-stack unif-prob
		default-env-stack dup-record exp-index mating))))

(defun duplicate-jform (jform parent &optional (renamings nil))
  (let (new-jform)
    (case (jform-type jform)
      (universal
       (setq new-jform (copy-universal jform))
       (unless skolem-default 			    
	       (do ((bottom jform (universal-dup bottom)))
		   ((memq (universal-dup bottom) '(t nil))
		    (setq new-jform (copy-universal bottom))))
	       (setf (universal-selected new-jform)
		     (mapcar #'(lambda (x) (if (assoc x renamings) (cdr (assoc x renamings))
					     (progn (push (cons x (ren-var-xa x)) renamings)
						    (cdr (assoc x renamings)))))
			     (universal-selected jform))))
       (setf (universal-dup new-jform) t)
       (setf (universal-prnt new-jform) (jform-parent jform))
       (setf (universal-scope new-jform)
             (duplicate-jform (universal-scope jform) new-jform renamings)))
      (disjunction
       (setq new-jform (copy-disjunction jform))
       (setf (disjunction-components new-jform)
             (mapcar #'(lambda (jform) (duplicate-jform jform new-jform renamings))
                     (disjunction-components jform))))
      (conjunction
       (setq new-jform (copy-conjunction jform))
       (setf (conjunction-components new-jform)
             (mapcar #'(lambda (jform) (duplicate-jform jform new-jform renamings))
                     (conjunction-components jform))))
      (literal (setq new-jform (copy-literal jform))
	       (unless skolem-default (setf (literal-represents new-jform) 
					    (rename-using renamings (literal-represents new-jform))))
               #+comment(if (infinite-p max-mates)
			    (setf (jform-num-paths-below new-jform) 1000000) ; a million literals should be OK..
			  (setf (jform-num-paths-below new-jform) max-mates))
	       ;the above is false -- it allows max-mates to be exceeded.
	       ;we change this to a pointer to the old jform, and use the new functions 
	       ;{decrease,increase,find}-paths-below to chase down the list of pointers.
	       (setf (jform-num-paths-below new-jform) jform)))
    (setf (jform-parent new-jform) parent)
    new-jform))

(defun rename-using (renamings wff)
  (if (null wff) nil
    (if (symbolp wff) (or (cdr (assoc wff renamings)) wff)
      (cons (rename-using renamings (car wff))
	    (rename-using renamings (cdr wff))))))

(defflag max-dup-paths
  (flagtype integer+-or-infinity)
  (default infinity)
  (subjects mating-search ms90-3 ms90-9 ms91-7 ms91-6 ms89 ms88 ms92-9 ms93-1 transmit)
  (mhelp "Any universal jform which has more than MAX-DUP-PATHS paths below it cannot get
duplicated during search process."))

(defun find-outmost-univ (univ)
  (let ((son univ))
    (do ((parent (jform-parent son) (jform-parent parent)))
        ((not (universal-p parent)) son)
      (setq son parent))))

(defun find-inner-univ (subpath env qvars)
  (let ((last-univ (car subpath))
        (last-env env)
        (last-elt nil))
    (dolist (elt (cdr subpath) (throwfail "No quantifier!"))
      (if (jform-p elt)
          (if (not (universal-p elt)) 
	      (return (cons last-env last-univ))
              (if (eq qvars (universal-qvars elt))
                  (return (cons last-env last-univ))
                  (setq last-env last-elt last-univ elt)))
          (setq last-elt elt)))))

;;;This is a clumsy way to define find-next-dup. A better way
;;;is to use the change-fn slot of flag DUPLICATION-STRATEGY-PFD
;;;to define find-next-dup whenever the flag is re-set.
(defun find-next-dup
  (active-path open-path pre-dup state-stack mating next-dup)
  (case duplication-strategy-pfd
    (dup-inner (find-next-dup-inner
	         active-path open-path pre-dup state-stack mating next-dup))
    (dup-outer (find-next-dup-outer
		  active-path open-path pre-dup state-stack mating next-dup))
    (otherwise (throwfail 
		 "Flag duplication-strategy is set wrongly. Check its help message."))))

(defflag total-num-of-dups
  (flagtype null-or-posinteger)
  (default nil)
  (subjects important mating-search ms90-3 transmit)
  (irrelevancy-preconditions 
   (duplication-strategy-pfd (neq duplication-strategy-pfd 'dup-outer)))
  (relevancy-preconditions 
   (duplication-strategy-pfd (eq duplication-strategy-pfd 'dup-outer)))
  (mhelp "Max number of duplications allowed at any time 
during a search using path-focused duplication. Compare
NUM-OF-DUPS. This flag will be ignored if set to NIL.
THE IMPLEMENTATION OF THIS IS BUGGY; setting it to NIL is safest."))

(defvar *temp-num-of-dups* nil)

(defun count-dups-in (dup-record)
  (length (remove-if-not #'(lambda (x) (or (and (jform-p x) (eq (jform-type x) 'universal))
					   (and (listp x) (not (numberp (cdr x)))
						(represents-expansion-node x))))
			 dup-record)))

(defun find-next-dup-outer
  (active-path open-path pre-dup state-stack mating next-dup)
  (when total-num-of-dups
	(if (>= (count-dups-in dup-record)
		total-num-of-dups)
	    (progn (unless *temp-num-of-dups* (setq *temp-num-of-dups* num-of-dups) 
			   (when (memq mating-verbose '(med max)) (msg "T(")))
		   (setq num-of-dups 0))
	  (when *temp-num-of-dups* (setq num-of-dups *temp-num-of-dups*) 
		(when (memq mating-verbose '(med max)) (msg ")"))
		(setq *temp-num-of-dups* nil))))
  (when (and (> num-of-dups 0)
	     (if mating (< (length state-stack) num-of-dups)
	       (not state-stack)))
    (if next-dup
	(values next-dup (or (cadr (memq next-dup active-path))
			     (cadr (memq next-dup open-path))))
	(do ((path (reverse (or open-path active-path)) (cdr path))
	     (elt nil (car path))
	     (qvars (and pre-dup (universal-qvars pre-dup))))
	    ((or (null path)
		 (and (null pre-dup) (car path) (not (consp (car path)))
		      (eq (jform-type (car path)) 'universal)
                      (universal-dup-t (car path))))
	     (if (and path (not (eq (universal-dup (car path)) t))
		      (<= (universal-dup-mark (car path)) num-of-dups))
		 (let* ((dup-jform (car path))
                        (duptimes (universal-dup-mark dup-jform)))
		   (if (null skolem-default)
		       (do ((jf dup-jform (universal-dup jf))
			    (count 0 (1+ count)))
			   ((or (= count duptimes) (memq (universal-dup jf) '(t nil)))
			    (when (eq (universal-dup jf) t) 
				  (setf (universal-dup jf) nil))
			    (unless (universal-dup jf)
				    (let ((new-jform (duplicate-jform jf nil)))
				      (setf (universal-dup jf) new-jform)
				      (setf (universal-dup new-jform) t)
				      (setf (universal-adam new-jform) jf))) 
			    (setf (universal-dup-mark dup-jform) (1+ duptimes))  ;;;add the mark here
			    (values (universal-dup jf) elt)))
		     (progn (unless (universal-dup dup-jform)
				    (let ((new-jform (duplicate-jform dup-jform nil)))
				      (setf (universal-dup dup-jform) new-jform)
				      (setf (universal-dup new-jform) t)
				      (setf (universal-adam new-jform) dup-jform))) 
			    (setf (universal-dup-mark dup-jform) (1+ duptimes))  ;;;add the mark here
			    (values (universal-dup dup-jform) elt))))
		 nil))
	  (when (and pre-dup (car path) (not (consp (car path)))
		     (eq (jform-type (car path)) 'universal)
		     (eq (universal-qvars (car path)) qvars))
	    (setq pre-dup nil))))))

(defun find-next-dup-inner
  (active-path open-path pre-dup state-stack mating next-dup)
  (when total-num-of-dups
	(if (>= (count-dups-in dup-record)
		total-num-of-dups)
	    (progn (unless *temp-num-of-dups* (setq *temp-num-of-dups* num-of-dups) 
			   (when (memq mating-verbose '(med max)) (msg "T(")))
		   (setq num-of-dups 0))
	  (when *temp-num-of-dups* (setq num-of-dups *temp-num-of-dups*) 
		(when (memq mating-verbose '(med max)) (msg ")"))
		(setq *temp-num-of-dups* nil))))
  (when (and (> num-of-dups 0)
	     (if mating (< (length state-stack) num-of-dups)
	       (not state-stack)))
    (if next-dup
	(values next-dup (or (cadr (memq next-dup active-path))
			     (cadr (memq next-dup open-path))))
	(do ((path (reverse (or open-path active-path)) (cdr path))
	     (elt nil (car path))
             (qvars (and pre-dup (universal-qvars pre-dup)))
	     (out-qvars (and pre-dup (universal-out-qvars pre-dup))))
	    ((or (null path)
                 (and (universal-p (car path))
                      (universal-dup-t (car path))
                      (or (null pre-dup) 
                          (and (neq qvars out-qvars)
                               (eq (universal-qvars (car path)) out-qvars)))))
             (when path
               (let* ((pair (find-inner-univ path elt qvars))
                      (dup-jform (cdr pair))
                      (duptimes (universal-dup-mark dup-jform)))
		 (when (<= (universal-dup-mark (car path)) num-of-dups)
		   (if (null skolem-default)
		       (do ((jf dup-jform (universal-dup jf))
			    (count 0 (1+ count)))
			   ((or (= count duptimes) (memq (universal-dup jf) '(t nil)))
			    (when (eq (universal-dup jf) t) 
				  (setf (universal-dup jf) nil))
			    (unless (universal-dup jf)
				    (let ((new-jform (duplicate-jform jf nil)))
				      (setf (universal-dup jf) new-jform)
				      (setf (universal-dup new-jform) t)
				      (setf (universal-out-qvars new-jform) (universal-qvars (car path)))
				      (setf (universal-adam new-jform) jf))) 
			    (setf (universal-dup-mark dup-jform) (1+ duptimes))  ;;;add the mark here
			    (values (universal-dup jf) (car pair))))
		     (progn (unless (eq (universal-dup dup-jform) t)
				    (unless (universal-dup dup-jform)
					    (let ((new-jform (duplicate-jform dup-jform nil)))
					      (setf (universal-dup dup-jform) new-jform)
					      (setf (universal-dup new-jform) t)
					      (setf (universal-out-qvars new-jform) (universal-qvars (car path)))
					      (setf (universal-adam new-jform) dup-jform))) 
				    (setf (universal-dup-mark dup-jform) (1+ duptimes))  ;;;add the mark here
				    (values (universal-dup dup-jform) (car pair)))))))))
	  (when (and pre-dup 
                     (eq qvars out-qvars)
		     (universal-p (car path))
                     (eq (universal-qvars (car path)) out-qvars))
	    (setq pre-dup nil))))))

;;;If UNIF-SUCC-P is NIL, then the unification problem must be
;;;ununifiable with regard to the current unification bounds. Hence,
;;;it is pointless to use the current mating. In other words,
;;;backtracking should remove a connection, instead of trying to
;;;duplicate quantifiers, which, at best, is futile, and at worst
;;;might lead to a bug since there was no distinction between an
;;;empty unification problem and an ununifiable problem. This is
;;;the reason to make the distinction. A simple observation is that
;;;this gives a quicker search in any cases.

(defun backtrack-top (active-jform open-path state-stack unif-prob
                                   env-stack dup-record exp-index
                                   mating alt-conn-p &optional (start-time nil) (time-limit nil))
  (multiple-value-bind
      (unif-succ-p conn-added-p active-jform active-path open-path
                   state-stack unif-prob env-stack dup-record
                   exp-index mating)
      (backtrack active-jform open-path state-stack unif-prob env-stack
                 dup-record exp-index mating alt-conn-p start-time time-limit)
    (if unif-succ-p
        (values conn-added-p active-jform active-path open-path
                state-stack unif-prob env-stack dup-record exp-index
                mating)
        (backtrack-top active-jform open-path state-stack 'ununifiable env-stack
                       dup-record exp-index mating alt-conn-p start-time time-limit))))

;;;The following function returned NIL when (ho-unify unode)
;;;failed. Later on this may cause a confusion between an
;;;empty unification problem and and ununifiable unification
;;;problem. Thm300a-bug in my library is a simple example.
;;;Here calling the latter "ununifiable", I can make a
;;;distinguishment between these two to fix thm300a-bug.
;;;(HX AUG 19th,94)
(defun backtrack (active-jform open-path state-stack unif-prob env-stack
			       dup-record exp-index mating alt-conn-p &optional (start-time nil) (time-limit nil))
  (if dup-record
      (do ((dup-record dup-record (cdr dup-record)))
        ((or (null dup-record)
             (and (consp mating)
                  (eq (car dup-record) (car mating))))
         (if (null dup-record) 
             (return (values nil nil active-jform nil open-path state-stack
                             nil env-stack dup-record exp-index mating)))
         (let ((conn (car mating))
              (indices (cadr mating))) 
           (when (eq mating-verbose 'max)
             (msg t "Removing connection: " conn 3 indices))
           (increase-paths-below (car-conn conn))
           (increase-paths-below (cdr-conn conn))
	   (setq dup-record (cdr dup-record)
		 mating (cddr mating)
		 unif-prob (restore-unif-prob mating start-time time-limit))
           (if (neq unif-prob 'ununifiable)
               (let ((active-path (conn-path conn))
                     (env-stack (conn-old-env conn)))
                 (if alt-conn-p
                     (multiple-value-bind
                         (conn-added-p unif-prob dup-record mating)
                         (find-alt-conn (car-conn conn) (cdr-conn conn)
                                        (cdr indices) active-path
                                        open-path unif-prob env-stack 
                                        dup-record mating start-time time-limit)
                       (values t conn-added-p active-jform active-path
                               open-path state-stack unif-prob
                               env-stack dup-record exp-index mating))
                     (values t nil active-jform active-path open-path
                             state-stack unif-prob env-stack dup-record
                             exp-index mating)))
                 (values nil nil active-jform nil open-path state-stack
                         'ununifiable env-stack dup-record exp-index mating))))
	(let ((elt (car dup-record)))
	  (if (consp elt)
	      (when (jform-p (cdr elt))
		    ;;(structurep (cdr elt))
		    ;;else ELT represents addition or removal of expansion
		    ;;variable by the path enumerator, backing up over a
		    ;;disjunction, or adding an earlier connection.
		    ;;
		    ;;duplication was removed at this stage. add it to the
		    ;;state-stack and update active-jform, env-stack and
		    ;;open-path.
		    (setq active-jform (cdr elt)
			  env-stack (cdar elt)
			  state-stack (cons (caar elt) state-stack)
			  open-path (append (cdaaar elt) (caaaar elt)))
		    (when (eq mating-verbose 'max)
			  (let ((*print-pretty* nil))
			  (msg t "Restoring an earlier duplication" t 5
			       active-jform t "on path " (l open-path)))))
	    ;;adding a duplication. Remove this duplication and find an 
	    ;;alternate one. If none exists, then remove this duplication
	    ;;and continue backtracking. 
	    (let ((state (pop state-stack)))
	      (when (eq mating-verbose 'max)
		    (let ((*print-pretty* nil))
		    (msg t "Removing duplication: " t 5 active-jform)))
	      (let* ((adam (universal-adam active-jform))
		     (duptimes (universal-dup-mark adam)))
		(setf (universal-dup-mark adam) (1- duptimes)))  ;;;remove the mark here.
	      (setq active-jform (cdr state)
		    open-path (caar state))
	      (multiple-value-bind
	         (dup-jform conn-added-p active-path open-path state-stack 
			    unif-prob env-stack dup-record exp-index mating)
		 (if (neq unif-prob 'ununifiable)
		     (check-duplication active-jform (cdar state) open-path state-stack
					unif-prob (cdr dup-record) exp-index elt mating nil start-time time-limit))
		 (if dup-jform
		     (return (values t conn-added-p dup-jform
				     active-path open-path state-stack
				     unif-prob env-stack dup-record
				     exp-index mating))
		     (setq env-stack
			   (cadr (memq active-jform (cdar state))))))))))
      ;; This throwfail is extremely annoying in ms90-9, where you can
      ;; have more than one proof going at a time.
      #+comment(throwfail "No proof at current depth (" num-of-dups
                          "). Try with a higher bound on NUM-OF-DUPS.")
      (no-backtrack-handler)
      ))

(defun no-backtrack-handler ()
   (cond ((and *option-tree-ms* (not (eq default-ms 'ms93-1)))  ; need such a flag
          (msgf "Try to set NUM-OF-DUPS to a value greater than " 
                num-of-dups 
                ", and search again;")
          (msgf "Or try to set MAX-MATES to a value greater than " max-mates ", and search again;")
          (if (not first-order-mode-ms)
              (msgf "Or try to reset MAX-SEARCH-DEPTH to larger value and search again."))
          (throw 'option-tree-search-kill (values 'fail nil)))
         (*no-internal-duplications* (throw 'duplicate-everything-now (values nil 'bigger nil)))
         (*test-top* (throw 'in-test-top-for-bktrk (values 'nil nil)))
         (t (throwfail
              t "Try to set NUM-OF-DUPS to a value greater than " num-of-dups ", and search again;"
              t "Or try to set MAX-MATES to a value greater than " max-mates ", and search again;"
              t (if (not first-order-mode-ms)
                    "Or try to reset MAX-SEARCH-DEPTH to a larger value and search again.")))))


(defun restore-unif-prob (mating &optional (start-time nil) (time-limit nil))
  (setren-counter h-var-prefix 0)
  (setren-counter w-var-prefix 0)
  (setq neg-h-var-list nil)
  (setq imitation-h-var-list nil)
  (do ((mating mating (cddr mating))
       (unif-prob nil (append (conn-env (car mating)) unif-prob)))
      ((null mating)
       (if first-order-mode-ms
           (make-unode :dpairs nil :substs (pfd-fo-unify nil unif-prob nil)
		       :fo-mode t)
           (let ((unode (make-unode :dpairs unif-prob)))
             (if (ho-unify unode (or max-search-depth most-positive-fixnum) start-time time-limit)
		 unode 'ununifiable))))))

(defun find-path-elts (dup-record path-elts)
  (do ((dup-record dup-record (cdr dup-record)))
      ((not dup-record) (values nil path-elts))
    (let ((elt (car dup-record)))
      (if (consp elt)
	  (if (or (numberp (cdr elt)) (eq (cdr elt) bkup-disj))
	      (push elt path-elts)
	      ;;If QVARS, then locate place in dup-record where QVARS are
	      ;;added. Update dup-record. Careful as there may be more
	      ;;additions and removals of the same variable within the scope
	      ;;of this addition.
	      (if (symbolp (car elt))
		  (if (eq (car elt) pre-conn) (push (cdr elt) path-elts)
		      (setq dup-record
			    (locate-exp-add (cdr dup-record) (1- (length elt)))
			    path-elts (cons elt path-elts)))
		  ;;jform was removed at this stage.
		  (if ;(structurep (cdr elt))
		      (jform-p (cdr elt))
		      (return (values (cdaaar elt) path-elts))
		      ;;ELT is a connection.
		      (push (car elt) path-elts))))
	  (return (values nil path-elts))))))

(defun locate-exp-add (dup-record counter)
  (do ((dup-record dup-record (cdr dup-record)))
      (nil)
    (let ((elt (car dup-record)))
      (if (and (consp elt) (symbolp (car elt))
               (not (eq (car elt) pre-conn)))
	  (if (numberp (cdr elt))
	      (if (zerop counter) (return dup-record) (decf counter))
	      (incf counter (length elt)))))))

(defun mark-ext-exp-nodes (jform)
  (case (jform-type jform)
    (literal nil)
    (conjunction (mapc #'mark-ext-exp-nodes (conjunction-components jform)))
    (disjunction (mapc #'mark-ext-exp-nodes (disjunction-components jform)))
    (universal (if (or (infinite-p max-dup-paths)
                       (<= (find-paths-below jform) max-dup-paths))
                   (setf (universal-dup-t jform) t)
                   (case duplication-strategy
		     (dup-outer nil)
		     (dup-inner (mark-ext-exp-nodes (universal-scope jform))))))))

;;;sort top level conjunctions in decreasing order according to number
;;;of paths.

(defun sort-ms90-3-jform (jform key set-progress-ctr-p)
  (case (jform-type jform)
    (literal nil)
    (conjunction
     (when set-progress-ctr-p
       (let ((ctr (length (conjunction-components jform))))
         (dolist (conj (conjunction-components jform))
           (setf (jform-progress conj) (decf ctr)))))
     (setf (conjunction-components jform)
           (sort (conjunction-components jform) #'> :key key)))
    (disjunction
     (dolist (disj (disjunction-components jform))
       (sort-ms90-3-jform disj key set-progress-ctr-p)))
    (universal (sort-ms90-3-jform (universal-scope jform) key
                                  set-progress-ctr-p))))

(defun set-max-mates (jform)
  (case (jform-type jform)
    (literal (if (infinite-p max-mates)
		 (setf (jform-num-paths-below jform) 1000000) ;a million mates should be sufficient...
	       (setf (jform-num-paths-below jform) max-mates)))
    (conjunction (mapc #'set-max-mates (conjunction-components jform)))
    (disjunction (mapc #'set-max-mates (disjunction-components jform)))
    ((universal existential) (set-max-mates (universal-scope jform)))))

(defun transform-mating (mating)
  (let (new-mating)
    (dolist (elt mating (nreverse new-mating))
      (if (conn-p elt) 
          (push (acons (conn-lit1 elt) (conn-lit2 elt) (conn-env elt)) new-mating)
          (push elt new-mating)))))

(defun transform-dup-record (dup-record)
   (let (new-dup-record)
     (dolist (elt dup-record (nreverse new-dup-record))
       (if (conn-p elt) 
           (push (acons (conn-lit1 elt) (conn-lit2 elt) (conn-env elt)) new-dup-record)
           (push elt new-dup-record)))))

(defun ms90-3-print-mating (mating)
   (do ((mating mating (cddr mating)))
       ((null mating) (terpri))
     (msg t (car mating) 3 (cadr mating))))

(defun increase-paths-below (jform)
  (if (literal-p (jform-num-paths-below jform))
      (increase-paths-below (jform-num-paths-below jform))
    (incf (jform-num-paths-below jform))))

(defun decrease-paths-below (jform)
  (if (literal-p (jform-num-paths-below jform))
      (decrease-paths-below (jform-num-paths-below jform))
    (decf (jform-num-paths-below jform))))

(defun find-paths-below (jform)
  (if (literal-p (jform-num-paths-below jform))
      (find-paths-below (jform-num-paths-below jform))
    (jform-num-paths-below jform)))


