;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(context ms91)

(deffile ms91-search
 (part-of ms91)
 (mhelp "Functions dealing with overall structure of MS91-6 and MS91-7
mating-search procedures."))


(defun option-search (eproof)
  (let ((*option-tree-ms* default-ms))
  (init-option-search eproof)
  (setq *option-set-weight-limit*
    (if (and *option-list* (option-> weight-a-coefficient 0))
	(option-min *option-list*)
      0))
  (setq *absolute-max-weight-d* (compute-weight-d *option-list*))
  (setq *option-search-markers* nil)
  (setq *primsubs-remaining* nil)
  (setq *min-weight-d-on-this-round* infinity)
  (case prim-bdtypes-auto
	((replace) (setq prim-bdtypes (find-prim-types (get-deep (eproof-etree
                                                 eproof)))))
	((replace-sub) (setq prim-bdtypes (find-prim-subtypes (get-deep (eproof-etree
                                                 eproof)))))
	((append) (setq prim-bdtypes (union prim-bdtypes (find-prim-types (get-deep (eproof-etree eproof))))))
	((append-sub) (setq prim-bdtypes 
                        (union prim-bdtypes (find-prim-subtypes (get-deep (eproof-etree eproof)))))))
  (clrhash prim-hashtable)
  (when ms91-interleave 
	(setq new-option-set-limit 
	      (min (* ms91-interleave
		      (length (mapcar #'dont-apply-prim-subs (find-ho-exps (find-exp-nodes (eproof-etree eproof)))))) ; this is always the same as the number of ho-exp's, is this what was intended? strange.  I suspect the intention was (apply #'append (apply #'append (mapcar . . . ))) in order to get some count of the number of primsubs generated.  I will not change it for now, though. - cebrown 5/11/00
		   new-option-set-limit)))
  (do ((first-time t nil)
       (initial-vpaths 0)
       (new-option-sets t)
       (current-option-set
	 (choose-lowest-option-set)
	 (if (memq flag '(xx-abort succeed user-abort no-options))
	     (progn (if options-verbose 
			(msg "Due to message : " flag ", using current option set rather 
than choosing a new one.")) 
		    current-option-set)
	     (choose-lowest-option-set)))                                
       (give-up nil nil)
       (flag nil))
      ((or (eq flag 'xx-abort) 
	   (eq flag 'succeed)
	   (eq flag 'no-options)
	   (eq flag 'user-abort)
	   (eq flag 'fail-inf)
	   (eq new-option-sets 'fail)
	   (eq flag 'test-fail))
       (when (eq new-option-sets 'fail) (finish-up-option-search nil 'fail-inf))
       (finish-up-option-search current-option-set flag))
      (progn (if (eq new-option-sets 'fail) (finish-up-option-search nil 'fail-inf))
	     (if (null current-option-set) 
	(if (or first-time (decide-to-update-option-sets))
	    (setq new-option-sets (update-option-sets new-option-sets))
	  (setq flag 'no-options))
      (let ((current-eproof (option-set-eproof current-option-set)))
	(when (zerop initial-vpaths)
	      (setq initial-vpaths (number-of-vertical-paths-main (etree-to-jform (eproof-etree current-eproof)))))
	(msgf "Choosing eproof:" current-option-set ": Weight: "
	      (option-set-weight-d current-option-set) t)
	(incf (option-set-times current-option-set))
	(print-option-set-subs current-option-set)
	(multiple-value-setq (flag give-up)
	  (catch 'option-tree-search-kill 
	    (limited-mating-search-option current-option-set initial-vpaths)))
	(when (eq flag 'fail) 
	  (msgf "Failed to find proof." t)
	  (adjust-option-set-ranking current-option-set give-up)
	  (setq flag (decide-to-continue-option))
	  (unless (or (eq flag 'test-fail) (eq flag 'user-abort))
		  (when (decide-to-update-option-sets)
			(setq new-option-sets (update-option-sets new-option-sets)))))))))))

;;; Choose lowest weighted option-set so far.
;;; If there are none, or all have exceeded max-search-limit, return nil.
;;; If two have equal weight, pick one with lowest time-used.

(defun choose-lowest-option-set ()
  (if (and options-verbose *option-set-list*) (msg "We have a list of option sets, and will choose the lowest-weighted." t)
    (if options-verbose (msg "We have no list of option sets to choose from." t)))
  (when *option-set-list*
    (let ((best nil)
	  (best-weight nil)
	  (best-time-used nil))
      (dolist (o-set *option-set-list* best)
	(let ((o-set-weight (option-set-weight-d o-set))
	      (o-set-time-used (option-set-time-used o-set)))
	  (if options-verbose (msg "Considering: a set of weight " o-set-weight " with time used " o-set-time-used t))
	  (unless (or (infinite-p o-set-weight)
		    (and max-search-limit
			 (<= max-search-limit o-set-time-used)))
	    (when (or (null best)
		      (< o-set-weight best-weight)
		      (and (= o-set-weight best-weight)
			   (< o-set-time-used best-time-used)))
	      (setq best o-set
		    best-weight o-set-weight
		    best-time-used o-set-time-used))))))))



(defun print-option-set-subs (option-set)
  (progn
  (vpwin-update-three option-set)
  (let ((vars (remove-duplicates 
	       (flatten (mapcar #'option-nodes
				(option-set-options option-set)))))
	(subs-exist nil))
    (msg "Considering option set " (option-set-label option-set) " for the " (option-set-times option-set) (ordinal-print (option-set-times option-set)) "time." t) 
    (if monitorflag (funcall (symbol-function *current-monitorfn*) 'new-oset (list (cons 'oset (princ-to-string (option-set-label option-set))))))
    (msg "Substitutions: " t)
    (dolist (var vars)
      (let* ((p (position var (etree-components (etree-parent var))))
	     (exp-var (when p (nth p (expansion-terms (etree-parent var))))))
	(when (and (exp-var-p exp-var)
		   (not (eq (exp-var-var exp-var) (exp-var-subst exp-var))))
	  (msg ((exp-var-var exp-var) . gwff) "  -->  "
	       ((exp-var-subst exp-var) . gwff) t)
	  (setq subs-exist t))))
    (if (not subs-exist) (msg "None." t)))))
		
(defun ordinal-print (n)
  (if (or (> (mod n 10) 3) (= (mod n 10) 0)) "th "
    (if (= (mod (/ (- n (mod n 10)) 10) 10) 1) "th "
      (if (= (mod n 10) 3) "rd "
	(if (= (mod n 10) 2) "nd "
	  "st ")))))

(defun limited-mating-search-option (current-option-set &optional (initial-vpaths 1))
  (let ((current-eproof (option-set-eproof current-option-set)))
    (deepen-to-literals (eproof-etree current-eproof))
    (let* ((first-order-mode-ms 
	    (not (find-if #'higher-order-var
			  (free-vars-in-etree
			   (eproof-etree current-eproof)))))
	   (n (number-of-vertical-paths-main (etree-to-jform (eproof-etree current-eproof))))
	   (*leibniz-var-list* (leibniz-quant *leibniz-var-list*))
	   (max-time-1 (or search-time-limit (if first-order-mode-ms 8 30)))
	   (max-time (or (if (and ms91-time-by-vpaths search-time-limit)
			     (/ (* search-time-limit n) initial-vpaths)
			   search-time-limit)
			 max-time-1))
	   (time-now (get-net-internal-run-time)))
      (if ms91-time-by-vpaths
	  (msgf "Time scaling by a factor of " (format nil "~,F" (/ n initial-vpaths)) " : new time limit is " 
		(format nil "~,F" max-time) t) 
	(if (and search-time-limit (not (zerop n)))
	    (msgf "Using MS91-TIME-BY-VPATHS, you could set SEARCH-TIME-LIMIT to "
		  (format nil "~,F" (* search-time-limit (/ initial-vpaths n))) " for this oset." )))
      (setq *test-total-time* (+ *test-total-time* max-time))
      (incf (option-set-time-used current-option-set) max-time-1) ;this fools max-search-limit into working...
      (multiple-value-bind (a b)
			   (case *option-tree-ms*
				 ((ms88 ms91-6)
				  (limited-ms (eproof-etree current-eproof) max-time))
				 ((ms90-3 ms91-7)
				  (ms90-3-controller (eproof-etree current-eproof) max-time)))
			   (unless (eq a 'test-fail)
				   (setq *test-total-time*
					 (+ (truncate (1+ (/ (- (get-net-internal-run-time) time-now) 
							     internal-time-units-per-second)))
					    (- *test-total-time* max-time))))
			   (values a b)))))

(defun adjust-option-set-ranking (last-option-set-attempted give-up)
  (if give-up (msg "Giving up on this option-set." t) 
    (msg "Increasing weighting on this option set; new weight is "))
  (setf (option-set-weight-d last-option-set-attempted)
	(if give-up infinity 
	  (progn (let ((wt (reconsider-weight (option-set-weight-d last-option-set-attempted))))
		   (msg wt t)
		   wt))))
  (when (infinite-p (option-set-weight-d last-option-set-attempted))
	(setf (eproof-incomp-clists current-eproof) nil)
	(setf (eproof-incomp-clists master-eproof) nil)
	(setf (eproof-etree (option-set-eproof last-option-set-attempted)) nil)
	(setf (cgraph) (make-hash-table :test #'equal))
	(setf (connections-array) (make-hash-table :test #'eql))
	(setf (eproof-cgraph (option-set-eproof last-option-set-attempted)) (make-hash-table :test #'equal))
	(setf (eproof-connections-array (option-set-eproof last-option-set-attempted)) (make-hash-table :test #'eql))
	(setf (eproof-jform (option-set-eproof last-option-set-attempted)) nil)
	(setf (eproof-skolem-node-list (option-set-eproof last-option-set-attempted)) nil)
	(setf (eproof-skolem-constants (option-set-eproof last-option-set-attempted)) nil)
	(setf (eproof-substitution-list (option-set-eproof last-option-set-attempted)) nil)
	(dolist (mat (eproof-mating-list (option-set-eproof last-option-set-attempted)))
		(destroy-utree (mating-utree mat))
		(setf (mating-clist mat) nil)
		(do-symbols (sym1 (find-package "CL-USER"))
			    (when (and (boundp sym1) (eq (symbol-value sym1) mat))
				  (setf (symbol-value sym1) nil) (unintern sym1))))
	(setf (eproof-mating-list (option-set-eproof last-option-set-attempted)) nil)
	(do-symbols (sym1 (find-package "CL-USER"))
		    (when (and (boundp sym1) (eq (symbol-value sym1) (option-set-eproof last-option-set-attempted)))
			  (setf (symbol-value sym1) nil) (unintern sym1)))
	))
	
(defun destroy-utree (utree)
  (when (node-p utree)
	(dolist (u (node-sons utree)) (destroy-utree u))
	(setf (node-sons utree) nil)
	(setf (node-parent utree) nil)))

(defun decide-to-continue-option ()
  (if (eq query-user T) ; changed MB 7/23/93 
      (query "Continue mating search?" 'yes)
      nil))

(defun decide-to-update-option-sets ()
    (let ((percent-tried (find-percent-option-sets-changed)))
      (if (and (< 0.99 percent-tried) *option-list*)
	  (if (eq query-user T) ; changed MB 7/23/93
	      (query "More than 99% of the available choices have been tried.  
Do you want to add more option-sets (if possible)?" 'yes)
	    t)
	nil)))

(defun find-percent-option-sets-changed ()
  (fudge-time-used *option-set-list*)
  (if *option-set-list*
      (/ (count-if-not #'zerop *option-set-list* :key #'option-set-time-used)
	 (length *option-set-list*))
    1.0))

(defun fudge-time-used (list)
  (map 'list #'(lambda (x) (if (infinite-p (option-set-weight-d x)) (progn (setf (option-set-time-used x) 1000000) x) x)) list))

(defun finish-up-option-search (current-option-set flag)
  (case flag
      (xx-abort (msgf "Mating search aborted." t) 
	     (report-option-search current-option-set flag) nil)
      (no-options (msgf "Ran out of options." t) 
		  (report-option-search current-option-set flag) nil)
      (user-abort (msgf "Mating search aborted at user request." t) 
		  (report-option-search current-option-set flag) nil)
      (fail (msgf "All current search possibilities have been exhausted." 
		  t ) (report-option-search current-option-set flag) nil)
      (fail-inf (msgf "All option sets now have weight infinity, and no new options" t 
		      "can be generated. Halting search." t)
		(report-option-search current-option-set flag) nil)
      (test-fail (msgf "Search aborted by test-top." t) nil)
      (succeed (msgf "An expansion proof has been found." t)
	       (report-option-search current-option-set flag)
	       (setq current-eproof (option-set-eproof current-option-set)))))

(defun report-option-search (option-set flag)
  (declare (ignore option-set flag))
  (let ((num-option-sets (length *option-set-list*))
	(num-option-sets-searched
	 (count-if-not #'zerop *option-set-list* :key #'option-set-time-used))
	(num-discarded
	 (count-if #'(lambda (x) (infinite-p x))
		   *option-set-list* :key #'option-set-weight-d)))
    (msgf "Total number of options created: " *length-of-option-list* t
	  "Total number of option-sets created: " num-option-sets t
	  "Number of option-sets actually searched: " num-option-sets-searched t
	  "Number of option-sets discarded: " num-discarded t)))

;;; Here we want to create some more option sets, possibly creating
;;; more options first.  This returns non-NIL if any new option-sets were
;;; created, NIL if none were created.

(defun update-option-sets (last-call-resulted-in-something)
  (msgf "Computing new option-sets to try." t)
;;;useless (hx (1/2/93)  (unless (eq mating-verbose 'min) (runcount))
  (let ((cur-o-list-length *length-of-option-list*)
	(cur-o-set-list-length (length *option-set-list*))
	(result
	 (cond 
	  ;; if there are no options, then this must be a problem which has
	  ;; no expansion nodes.  Just create and return a new option-set which
	  ;; has no nodes.
	  ((and (null *option-list*) (null *option-set-list*))
	   (if options-verbose (msg "This problem has no expansion nodes.
Returning an option set with no nodes." t))
	   (make-new-option-set nil))
	  ;; here we have no options, but there is an option-set.  That is, we
	  ;; must have already been through the case above.  Don't do anything.
	  ((null *option-list*) 
	   (if options-verbose (msg "There are no options, but there is an option set.
So we have already returned an option set with no nodes. Returning nothing." t))
	   nil)
	  ;; Should we add new options, by prim subs or duplication?  
	  ;; If so, do that before creating the new option-sets.
	  ((or (null last-call-resulted-in-something) (decide-to-add-options))
	   (if options-verbose (msg "Decided to add options, and to create new option-sets." t))
	   (let ((lgth (length *option-list*)))
	     (make-new-options (eproof-etree master-eproof))
	     (if (and (eq lgth (length *option-list*)) ; if we created nothing new
		      (stuck-checker))                 ; and everything has infinite time used
		 'fail                                 ; then it's dead.
	       (make-new-option-sets))))
	  ;; Just create the new option-sets from existing options
	  (t
	   (if options-verbose (msg "Creating new option sets from existing options." t))
	   (make-new-option-sets)))))
    (unless (eq mating-verbose 'min) 
      (msgf "Number of new options in master etree: " 
	    (- *length-of-option-list* cur-o-list-length)
	    t
	    "Number of new option-sets created: "
	    (- (length *option-set-list*) cur-o-set-list-length) t));;;useless(hx 1/2/93)(runcount))
    result))

;;; Returns T if every non-infinite option in *option-list* is used in some
;;; option-set in *option-set-list*, and if the number of
;;; option-sets is more than 3/4 of all possible ones.
;;; (and also if there are primsubs left over)

(defun decide-to-add-options ()
  (let ((truth-value (decide-to-add-options-real)))
    (if (and options-verbose truth-value)
	(msg "Time to create some new options." t)
      (if options-verbose (msg "Don't need any new options yet; we'll just make more option sets." t)))
    truth-value))

(defun add-options-original (number)
  (and 
   (every #'(lambda (option) 
	      (or (or (infinite-p (option-weight-a option))
		      (and (infinite-p penalty-for-ordinary-dup)
			   (implic-node (option-nodes option))))
		  (some #'(lambda (option-set) 
			    (member option (option-set-options option-set)))
			*option-set-list*)))
	  *option-list*)
   (>= (/ (length *option-set-list*) (number-of-possible-option-sets)) 
       (+ 0.0 (/ number 100)))))

(defun number-of-possible-option-sets ()
  (if (infinite-p penalty-for-ordinary-dup)
      (expt 2 (count-if-not #'implic-node *option-list* :key #'option-nodes))
  (expt 2 (length *option-list*))))

(defun implic-node (list)
  (implication-p (car list)))

(defun decide-to-add-options-real ()
  (if (and ms91-interleave *primsubs-remaining*) t
  (if (<= (length *option-set-list*) 1) (add-options-original 75)
    ;the first and second time don't count... 
    ;(1st is just the original vpform, 2nd is the first lot of primsubs)
    (if (infinite-p options-generate-arg) nil
      (let ((value (funcall (symbol-function options-generate-fn) options-generate-arg)))
	(if value (funcall (symbol-function options-generate-update)))
	value)))))

(defun add-options-weight (weight)
      (if (< *option-set-weight-limit* weight) t nil))

(defun add-options-subs (number)
  (let ((oset (choose-lowest-option-set)))
    (if oset
	(if (< number (1- (simplest-weight-b-fn (option-set-options oset)))) t nil)
      nil)))

(defun add-options-count (number)
  (let ((number-tried (count-if-not #'zerop *option-set-list* :key #'option-set-time-used)))
    (if (< number number-tried) t nil)))

;;; Using existing *option-list*, construct new option-sets.
;;; Return T if new option-sets are created, otherwise NIL.
;;; Uses flag new-option-set-limit to limit the number 
;;; of option-sets created in any iteration.

(defflag new-option-set-limit 
  (flagtype posinteger)
  (default 20) ; was 5
  (mhelp "The maximum number of new option-sets that can be created at
any one time. See MS91-INTERLEAVE.")
  (subjects ms91-6 ms91-7 transmit))


(defun make-new-option-sets ()
  (if options-verbose (msg "Creating new option sets." t))
  (if (and options-verbose (option-> *option-set-weight-limit* *absolute-max-weight-d*))
      (msg "Option set weight limit has exceeded the absolute maximum of " *absolute-max-weight-d* t "Not creating any more option sets."))
  (unless (option-> *option-set-weight-limit* *absolute-max-weight-d*)
  (do* ((next-option-set (get-next-option-set)
			 (get-next-option-set))
	(created-option-set-count
	 (if next-option-set 1 0) 
	 (if next-option-set 
	     (1+ created-option-set-count)
	   created-option-set-count)))
      ((or (null next-option-set) 
	   (= created-option-set-count 
	       new-option-set-limit))
       (if (zerop created-option-set-count)
	   (progn (if options-verbose (msg "Created no new option sets." t))
		  (if (and (decide-to-update-option-sets)
			   (decide-to-add-options)) 
		      (progn (msg "Adding new options" t)
			     (make-new-options (eproof-etree master-eproof))))
		  (if (infinite-p *min-weight-d-on-this-round*)
		      (progn (if options-verbose (msg "Increasing weight limit by 1." t))
			     (setq *option-set-weight-limit* (1+ *option-set-weight-limit* )))
		    (progn (if options-verbose (msg "Setting weight limit to " *min-weight-d-on-this-round* "." t))
			   (setq *option-set-weight-limit* *min-weight-d-on-this-round*)))
		  (setq *min-weight-d-on-this-round* infinity)
		  (make-new-option-sets))
	 t)))))


(defmateop ms91-6
  (mate-result-> ignore)
  (mate-alias ms91-6-real) ;was auto-search-ms91-6
  (mhelp "Begin mating search MS91-6 on the current expansion proof.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS88 is used.  The flags
MAX-SEARCH-LIMIT and SEARCH-TIME-LIMIT are used to control the amount
of time spent on each jform.

The order in which the possible jforms are considered depends on a 
number of flags. Firstly, the primitive substitutions which are generated 
are determined by the values of MAX-PRIM-DEPTH, MIN-PRIM-DEPTH,
PRIM-QUANTIFIER and NEG-PRIM-SUB. If DUP-ALLOWED is T, then additional
options are generated corresponding to duplicated quantifiers. These
options are then combined into sets; because there can be many such sets,
the flag NEW-OPTION-SET-LIMIT controls how many are generated at once.
Each set is given a weighting (see flags WEIGHT-x-COEFFICIENT and 
WEIGHT-x-FN, for x = A,B,C), and the lowest-weighted set is chosen
for searching. If the weight of the lowest-weighted set is too large,
TPS may generate more sets; the interpretation of \"too large\" is given
by MS91-WEIGHT-LIMIT-RANGE. If the search fails, it will be discarded;
if it runs out of time then it will be re-weighted to be continued
later (see RECONSIDER-FN)."))

(defun auto-search-ms91-6 (&rest ignore)
  (declare (ignore ignore))
  (let ((default-ms 'ms91-6))
    (option-search current-eproof)))

(defun ms91-6-real ()
  (let ((default-ms 'ms91-6))
    (matingsearch-controller)))

(defun ms91-7-real ()
  (let ((default-ms 'ms91-7))
    (matingsearch-controller)))

(defmateop ms91-7
  (mate-result-> ignore)
  (mate-alias ms91-7-real); was auto-search-ms91-7
  (mhelp "Begin mating search MS91-7 on the current expansion proof.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS90-3 is used.  The flags
MAX-SEARCH-LIMIT and SEARCH-TIME-LIMIT are used to control the amount
of time spent on each jform.

The order in which the possible jforms are considered depends on a 
number of flags. Firstly, the primitive substitutions which are generated 
are determined by the values of MAX-PRIM-DEPTH, MIN-PRIM-DEPTH,
PRIM-QUANTIFIER and NEG-PRIM-SUB. If DUP-ALLOWED is T, then additional
options are generated corresponding to duplicated quantifiers. These
options are then combined into sets; because there can be many such sets,
the flag NEW-OPTION-SET-LIMIT controls how many are generated at once.
Each set is given a weighting (see flags WEIGHT-x-COEFFICIENT and 
WEIGHT-x-FN, for x = A,B,C), and the lowest-weighted set is chosen
for searching. If the weight of the lowest-weighted set is too large,
TPS may generate more sets; the interpretation of \"too large\" is given
by MS91-WEIGHT-LIMIT-RANGE. If the search fails, it will be discarded;
if it runs out of time then it will be re-weighted to be continued
later (see RECONSIDER-FN)."))

(defun auto-search-ms91-7 (&rest ignore)
  (declare (ignore ignore))
  (let ((default-ms 'ms91-7))
    (option-search current-eproof)))

;;;;;;; code for search-order begins here MB 2/2/94

(defmexpr search-order
  (argtypes integer+ yesno yesno)
  (argnames num vpf verb)
  (arghelp "Number of option sets to show." "Show vpforms too?" "Verbose output?")
  (defaultfns (lambda (num vpf verb) (list (if (eq num '$) 25 num) (if (eq vpf '$) nil vpf) 
					   (if (eq verb '$) nil verb))))
  (mainfns search-order-ms91)
  (mhelp "Generates the first n option sets that will be searched under 
the current flag settings (assuming that the first (n-1) searches fail
because they run out of time rather than for any other reason).
This will show the names and weights of the option sets, the 
primitive substitutions and duplications. 
Note : \"Ordinary\" duplications are duplications that have not 
had a primsub applied to them. So, for example, \"X has 2 primsubs 
plus 3 ordinary duplications\" means that the vpform now contains 
five copies of the relevant quantifier, two of which have had 
primsubs applied to them."))

(defun search-order-ms91 (n vpf verb)
  (if (planp (pline-default '$))
      (progn (let ((goal (pline-default '$))
		   (support nil))
      (let* (skolem-terms imp-node supp-nodes left-node right-node)
        (setq skolem-terms 
	  (delete-duplicates
	   (mapcar #'(lambda (x) (cons (make-skolem-term :term x
                                                         :parameter x) 0))
                   (free-vars-in-lines 
                    (cons goal 
                          (apply #'append
                                 (mapcar #'(lambda (x) (line-hypotheses x))
                                         support)))))))
        (setq current-eproof
          (make-eproof
	   :free-vars-in-etree nil
	   :substitution-list nil
	   :leaf-list nil
	   :skolem-constants skolem-terms
	   :skolem-method skolem-default
	   :skolem-node-list nil))
        (setq master-eproof current-eproof)
        (setq imp-node 
          (make-implication :junctive 'con :free-vars nil
                            :parent nil :positive nil))
        (setq supp-nodes
          (mapcar #'(lambda (x)
                      (deepen-to-literals*
                       (let ((node (make-universalized-node x)))
			 (init-symmetry node current-eproof)
			 node)))
                  support))
        (setq left-node
          (make-etree-conjunction supp-nodes imp-node))
        (setq right-node
	  (let ((leaf (make-leaf :shallow (line-assertion goal)
                                 :junctive nil
                                 :positive nil
                                 :free-vars nil)))
	    (update-status nil leaf 1)
	    #+comment(push (cons (etree-name leaf) 1)
		  (eproof-statuses current-eproof))
	    (init-symmetry leaf current-eproof)
	    (deepen-to-literals* (if truthvalues-hack (add-falsehood leaf) leaf))))
        (setq first-order-mode-ms
          (first-order-problem-p
           (mapcar #'(lambda (x) (cons (exp-var-var (car x)) (cdr x)))
                   (eproof-free-vars-in-etree current-eproof))))
        (setq active-mating nil)
        (do ((supp-nodes supp-nodes (cdr supp-nodes))
             (support support (cdr support)))
            ((null support))
          (setf (line-node (car support)) (car supp-nodes)))
	(init-symmetry right-node current-eproof)
        (setf (line-node goal) right-node)
        (if left-node
            (progn
	      (init-symmetry left-node current-eproof)
              (setf (etree-parent left-node) imp-node)
              (setf (etree-parent right-node) imp-node)
              (setf (etree-components imp-node)
		(list left-node right-node))
              (setf (eproof-etree current-eproof) imp-node))
          (setf (eproof-etree current-eproof) right-node))
	(when (or (eq add-truth t) (and (eq add-truth 'if-needed) 
					(or (update-statuses (eproof-etree current-eproof)) t)
					(truth-needed (etree-to-jform (eproof-etree current-eproof)))))
	      (setf (eproof-etree current-eproof) (add-trut (eproof-etree current-eproof) 'TRUTH))
	      (unless truthvalues-hack
		      ;in which case all the FALSEHOODs have gone, so we don't need a NOT FALSEHOOD
		      (setf (eproof-etree current-eproof) (add-trut (eproof-etree current-eproof) '(NOT . FALSEHOOD)))))
        (initialize-mating-search)
	(when (lazy2-used) (fiddle-with-def-leaves (eproof-etree current-eproof)))
        (update-statuses (eproof-etree current-eproof))))))
  (if (not current-eproof) (msg "No current eproof in memory, and/or no current planned line." t "SEARCH-ORDER aborted." t)
    (progn
    (let ((*option-tree-ms* default-ms)
	  (options-verbose nil)
	  (mating-verbose 'silent)
	  (unify-verbose 'silent)
	  (query-user nil)
	  (counter 0)
	  (eproof current-eproof))
      (init-option-search eproof)
      (setq *option-set-weight-limit*
	    (if (and *option-list* (option-> weight-a-coefficient 0))
		(option-min *option-list*)
	      0))
      (setq *absolute-max-weight-d* (compute-weight-d *option-list*))
      (setq *primsubs-remaining* nil)
      (setq *option-search-markers* nil)
      (setq *min-weight-d-on-this-round* infinity)
      (case prim-bdtypes-auto
	((replace) (setq prim-bdtypes (find-prim-types (get-deep (eproof-etree
                                                 eproof)))))
	((replace-sub) (setq prim-bdtypes (find-prim-subtypes (get-deep (eproof-etree
                                                 eproof)))))
	((append) (setq prim-bdtypes (union prim-bdtypes (find-prim-types (get-deep (eproof-etree eproof))))))
	((append-sub) (setq prim-bdtypes 
                        (union prim-bdtypes (find-prim-subtypes (get-deep (eproof-etree eproof)))))))
      (clrhash prim-hashtable)
      (do ((first-time t nil)
	   (new-option-sets t)
	   (current-option-set
	    (choose-lowest-option-set)
	    (if (memq flag '(xx-abort succeed user-abort no-options))
		current-option-set
	      (choose-lowest-option-set)))
	   (flag nil))
	  ((= counter n))
	  (progn (if (eq new-option-sets 'fail) (finish-up-option-search nil 'fail-inf))
	  (if (null current-option-set) 
	      (if (or first-time (decide-to-update-option-sets))
		  (setq new-option-sets (update-osets-2 new-option-sets verb))
		(setq flag 'no-options))
	    (let ((current-eproof (option-set-eproof current-option-set)))
	      (incf counter)
	      (msgf t "*** " counter (ordinal-print counter) "option set *** : Weight " 
		    (option-set-weight-d current-option-set) " : Name " (option-set-label current-option-set) t)
	      (incf (option-set-times current-option-set))
	      (incf (option-set-time-used current-option-set) 30)
	      (print-option-set-s-d current-option-set)
	      (if vpf (display-vp-diag (init-position (etree-to-jform (eproof-etree current-eproof)))))
	      (adjust-oset-2 current-option-set nil)
	      (when (decide-to-update-option-sets)
		    (setq new-option-sets (update-osets-2 new-option-sets verb)))))))))))
  
(defun adjust-oset-2 (last-option-set-attempted give-up)
  (setf (option-set-weight-d last-option-set-attempted)
	(if give-up infinity 
	  (progn (let ((wt (reconsider-weight (option-set-weight-d last-option-set-attempted))))
		   wt)))))

(defun print-option-set-s-d (option-set)
  (progn
  (let ((vars (remove-duplicates 
	       (flatten (mapcar #'option-nodes
				(option-set-options option-set)))))
	(subs-exist 0))
    (if (< 1 (option-set-times option-set)) 
	(msg "Considering this set for the " (option-set-times option-set) (ordinal-print (option-set-times option-set)) "time:" t))
      (dolist (var vars)
	      (let ((exp-var (nth (position var (etree-components (etree-parent
								   var)))
				  (expansion-terms (etree-parent var)))))
		(if (not (eq (exp-var-var exp-var) (exp-var-subst exp-var)))
		    (progn (if (= 0 subs-exist) (msgf "Substitutions :" t))
		      (msg ((exp-var-var exp-var) . gwff) "  -->  "
			   ((exp-var-subst exp-var) . gwff) t)
		      (setq subs-exist (1+ subs-exist))))))
      (if (= 0 subs-exist) (msg "No substitutions." t))
      (print-dups-2 option-set subs-exist))))


(defun print-dups-2 (option-set subs-exist)
  (let* ((option-list (option-set-options option-set))
	 (exps 
	  (let ((exps nil))
	    (dolist (option option-list exps)
	      (dolist (node (option-nodes option))
		(pushnew node exps)))))
	 (exps-by-parent
	  (let ((exps-by-parent nil))
	    (dolist (exp exps exps-by-parent)
	      (let ((sublist (assoc (etree-parent exp) exps-by-parent)))
		(if sublist
		    (rplacd sublist (cons exp (cdr sublist)))
		  (push (list (etree-parent exp) exp) exps-by-parent))))))
	 (total 0))
    (setq total (compute-emptydups exps-by-parent))
    (if (and (= subs-exist 0) (= total 0)) (msg "No duplications." t))))

(defun compute-emptydups (exp-lists)
  (let ((total 0))
    (dolist (list exp-lists total)
      (let* ((expansions (cdr list))
	     (num-primsubs (count-if-not #'simple-dupe-p expansions)))
	(unless (and (= (length expansions) 1) (= num-primsubs 0))
		(progn (if (= (length expansions) num-primsubs)
			   (msg "One variable has " (length expansions) (if (= (length expansions) 1) " primsub." " primsubs.") t)
			 (if (> num-primsubs 0)
			     (msg "One variable has " num-primsubs (if (= num-primsubs 1) " primsub" " primsubs") " and " 
				  (- (length expansions) num-primsubs) " ordinary " (if (= (- (length expansions) num-primsubs) 1) "duplication." "duplications.") t)
			   (msg "One variable has " (1- (length expansions)) " ordinary" (if (= (length expansions) 2) " duplication." " duplications.") t)))))
	(setq total (- (+ total (1- (length expansions))) num-primsubs))))))


(defun update-osets-2 (last-call-resulted-in-something verb)
  (if verb (msgf "Updating option sets."))
  (let ((result
	 (cond 
	  ((and (null *option-list*) (null *option-set-list*))
	   (if verb (msgf "Problem has no expansion nodes; returning empty option set." t))
	   (make-new-option-set nil))
	  ((null *option-list*) 
	   (if verb (msgf "There are no options but there is an option set." t 
"Problem must have no expansion nodes." t))
	   nil)
	  ((or (null last-call-resulted-in-something) (decide-to-add-options))
	   (if verb (msgf "Decided to create new options and then make more option sets" t))
	   (let ((lgth (length *option-list*)))
	     (make-new-options (eproof-etree master-eproof))
	     (if (and (eq lgth (length *option-list*)) ; if we created nothing new
		      (stuck-checker))                 ; and everything has infinite time used
		 'fail                                 ; then it's dead.
	       (make-new-option-sets))))
	  (t (if verb (msgf "Making new option sets from the existing options" t))
	   (make-new-option-sets)))))
    result))

(defun stuck-checker ()
  (< 0 (count-if-not #'infp *option-set-list* :key #'option-set-time-used)))

(defun infp (x)
  (infinite-p x))
