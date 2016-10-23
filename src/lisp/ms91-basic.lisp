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

(deffile ms91-basic
  (part-of ms91)
  (mhelp "Basic data structures used in ms91-6 and ms91-7 search procedures."))

;;; The list of options at any time. Each option consists of one expansion
;;; term and those necessary for it to make sense.
(defvar *option-list* nil)

;;; The length of *option-list* at any time. Saves recomputing it.
(defvar *length-of-option-list* 0)

;;; The list of option-sets which have been created from options.
(defvar *option-set-list* nil)

(defstruct (option-set (:print-function print-option-set))
  (options nil) ; the individual options comprising this option-set
  (weight-d 1)  ; the weight of this option-set
  (eproof nil)  ; the (possibly incomplete) eproof which these options make up
  (time-used 0) ; time used in mating-search so far on this option-set
  (label nil)   ; the name of the option set
  (times 0)     ; the number of times it's been used
)

(defun print-option-set (obj stream depth)
  (declare (ignore depth))
  (format stream "[O-SET ~S ~D]" (option-set-options obj)
	  (option-set-weight-d obj))) 

(defstruct (option (:print-function print-option))
  nodes ; list of the children of expansion nodes choosing this option entails
  weight-a ; the individual weight for this option
)

(defun print-option (obj stream depth)
  (declare (ignore depth))
  (format stream "[OPT ~S ~D]" (option-nodes obj)
	  (option-weight-a obj))) 

;;; *EXPANSION-LEVEL* is an integer that says when a particular option
;;; was introduced.  It starts at 1, and we increment it every time we
;;; make new options.

(defvar *expansion-level* 1 
"Incremented each time we add new options to the master-eproof.  Gives
a rough estimate of how late in process an individual option was added.")

(defvar *absolute-max-weight-d* 1
"This is computed by taking (compute-weight-d *option-list*). Gives an
upper bound (if possible) on the largest option-set we can construct
at this time.")

(defvar *expansion-to-option-hash-table* nil
"A hash table that given an expansion (child of an expansion node),
gives the options that include that child. In particular, expansion
terms farther down in the expansion tree are options which include
the given option")

(defvar *minimal-option-hash-table* nil
"A hash table, that given an expansion (child of an expansion node),
returns the minimal option that includes that child, i.e., that child
and the requisite simple duplications that occur above and below it.")


(defvar *weight-d-hash-table* nil
"A hash table, that given a list of options, will return the value of
weight-d of that set, if it has already been computed.  Used to store
weight-d values as they are computed.")

(defun init-option-search (eproof)
  (declare (special *option-search-markers* *option-set-weight-limit*))
  (setq master-eproof eproof
	*option-list* nil 
	*option-search-markers* nil
	*option-set-weight-limit* 0
	*length-of-option-list* 0
	*option-set-list* nil
	*expansion-level* 1
	*expansion-to-option-hash-table* (make-hash-table :test #'eq)
	*minimal-option-hash-table* (make-hash-table :test #'eq)
	*weight-d-hash-table* (make-hash-table :test #'equal))
    (let ((initial-exps (get-initial-expansions* (eproof-etree master-eproof))))
      (mapc #'expansion-terms-to-option initial-exps))
    *option-list*)



;;; want to return an option-set that includes all the options
;;; necessary to make a good expansion tree from the given options.
;;; Some additional options may be required.

(defun options-to-option-set (options)
  (let ((required-options (copy-list (remove-duplicates options))))
    (flet ((kids-in-required-options (exp-node)
	     (remove-if-not 
	      #'(lambda (x)
		  (let ((options-in (expansion-to-options x)))
		    (find-if #'(lambda (y) 
				 (memq y required-options))
			     options-in)))
	      (etree-components exp-node))))
      (make-new-option-set
       (do ((nodes nil (cdr nodes))
	    (node (eproof-etree master-eproof) (car nodes)))
	   ((null node) required-options)
	 (if (expansion-p node)
	     (let ((kids (kids-in-required-options node)))
	       (if kids
		   (setq nodes (append nodes kids))             ; trivial expansions
		 (progn
		   (setq nodes (append nodes 
				      (list 
				       (car (etree-components node)))))
		   (push (minimal-option (car (etree-components node))) 
			 required-options))))
	   (setq nodes (append nodes (etree-components node)))))))))

;this is exactly the same as the above, but uses make-new-option-set-2

(defun options-to-option-set-2 (options)
  (let ((required-options (copy-list (remove-duplicates options))))
    (flet ((kids-in-required-options (exp-node)
	     (remove-if-not 
	      #'(lambda (x)
		  (let ((options-in (expansion-to-options x)))
		    (find-if #'(lambda (y) 
				 (memq y required-options))
			     options-in)))
	      (etree-components exp-node))))
      (make-new-option-set-2
       (do ((nodes nil (cdr nodes))
	    (node (eproof-etree master-eproof) (car nodes)))
	   ((null node) (remove-if 'null required-options))  
					;the remove-if 'null is needed since the changed to make-higher-order-options
	 (if (expansion-p node)
	     (let ((kids (kids-in-required-options node)))
	       (if kids
		   (setq nodes (append nodes kids))             ; trivial expansions
		 (progn
		   (setq nodes (append nodes 
				      (list 
				       (car (etree-components node)))))
		   (push (minimal-option (car (etree-components node))) 
			 required-options))))
	   (setq nodes (append nodes (etree-components node)))))))))
	 


(defun make-new-options (etree)
  (declare (special *primsubs-remaining* *option-set-weight-limit* *option-search-markers*))
  (let ((*ignore-first-order-dups* 
	 (eq *option-tree-ms* 'ms91-7))
	 (renumber-leaves nil))
    (setq current-eproof master-eproof)
    (unless *primsubs-remaining* (setq *expansion-level* *option-set-weight-limit*))
    (let ((exp-nodes (sort-by-name (find-exp-nodes etree))))
      (make-higher-order-options exp-nodes)
      (make-first-order-options exp-nodes))
    (setq *absolute-max-weight-d* (compute-weight-d *option-list*))
    (setq *option-search-markers* nil)))

(defun sort-by-name (expnodes)
  (sort expnodes
	#'(lambda (x y) (string< (string-left-trim (princ-to-string expansion-name) 
						   (princ-to-string x))
				 (string-left-trim (princ-to-string expansion-name) 
						   (princ-to-string y))))))

(defun expansion-to-options (exp)
  (gethash exp *expansion-to-option-hash-table*))

(defun add-option-to-hash-table (node opt)
  (pushnew opt (gethash node *expansion-to-option-hash-table*)))

(defsetf expansion-to-options add-option-to-hash-table)


(defun minimal-option (exp)
  (gethash exp *minimal-option-hash-table*))

(defun add-option-to-minimal-option-table (node opt)
  (setf (gethash node *minimal-option-hash-table*)
    opt))

(defsetf minimal-option add-option-to-minimal-option-table)

(defun set-equal (a b)
  (and (every #'(lambda (x) (member x b)) a)
       (every #'(lambda (y) (member y a)) b)))

(defun make-new-option-set (options)
  (unless (some #'(lambda (option-set) 
		    (set-equal options (option-set-options option-set)))
		*option-set-list*)
    (let ((new-option-set
	   (make-option-set :options options
			    :eproof (options-to-eproof options)
			    :time-used 0
			    :label (concatenate 'string "oset-" (princ-to-string (length *option-set-list*)))
			    :times 0)))
      (setf (option-set-weight-d new-option-set) 
	(compute-weight-d options))
      (push new-option-set *option-set-list*)
      (reorder-option-set-list)
      new-option-set)))

;this returns the existing option set if there is one.

(defun make-new-option-set-2 (options)
  (if (some #'(lambda (option-set) 
		    (set-equal options (option-set-options option-set)))
		*option-set-list*)
      (find-if #'(lambda (option-set) 
		   (set-equal options (option-set-options option-set)))
		   *option-set-list*)
    (let ((new-option-set
	   (make-option-set :options options
			    :eproof (options-to-eproof options)
			    :time-used 0
			    :label (concatenate 'string "oset-" (princ-to-string (length *option-set-list*)))
			    :times 0)))
      (setf (option-set-weight-d new-option-set) 
	(compute-weight-d options))
      (push new-option-set *option-set-list*)
      (reorder-option-set-list)
      new-option-set)))

(defun options-to-eproof (options)
  (let* ((new-eproof (copy-eproof* master-eproof))
	 (current-eproof new-eproof))
    (update-statuses-in-eproof
     (mapcar #'option-nodes options))
    new-eproof))

(defun make-higher-order-options (exp-nodes)
  (declare (special ms91-interleave))
  (let* ((current-eproof master-eproof)
	 (ho-exps (sort-by-name (find-ho-exps exp-nodes)))
	 (new-expansions (if ms91-interleave
			     (mapcan #'nreverse 
				     ;(reverse 
				      (make-higher-order-expansions** ho-exps));)
			   (mapcan
			  #'(lambda (x) (nreverse (make-higher-order-expansions* x))) ho-exps))))
    (mapcar #'expansion-terms-to-option
	    (nreverse new-expansions))))

#+comment(defun make-higher-order-options (exp-nodes)
  (let* ((current-eproof master-eproof)
	 (ho-exps (find-ho-exps exp-nodes))
	 (new-expansions (mapcan #'make-higher-order-expansions* ho-exps)))
    (mapcar #'expansion-terms-to-option
	    new-expansions)))
;;above is the old version of m-h-o-o; it's exactly the same except it applies primsubs
;;from the top of the etree down rather than from the bottom up. It's much slower,
;;but there may have been a reason for it, so I've left it here in case we need it...
;; MB Thu May 15 17:31:44 1997

;;; returns a list of lists.  Each list's first elt is the expansion
;;; that was added by prim-sub, remainder is the expansion terms above
;;; and below it in the tree.

(defflag ms91-interleave
  (flagtype null-or-posinteger)
  (default 5)
  (subjects ms91-6 ms91-7 primsubs transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms91-6)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms91-6)))
  (mhelp "In MS91-*, primitive substitutions are generated by NAME-PRIM,
and they are applied to the master eproof before the search mechanism 
chooses particular parts of that eproof (and hence particular 
substitutions) to try and prove.

If MS91-INTERLEAVE is NIL, all of the substitutions generated by NAME-PRIM
are applied at once, and then the search mechanism chooses among them, probably
in the order in which they were generated. The process of applying them to
the eproof can take a very long time.

If MS91-INTERLEAVE is an integer n, we take n primsubs at a time for each 
variable which has primsubs, and apply only those to the eproof. Once we
have searched through those (to be specific, once we decide to generate new
options), we take the next n primsubs for each variable and apply them, 
and so on. This is much quicker, and has the advantage of not having to 
work through every primsub for the first variable before starting work on 
the next variable.

If MS91-INTERLEAVE is non-NIL, and NEW-OPTION-SET-LIMIT is greater than
MS91-INTERLEAVE * (# of vars that have primsubs), then TPS will reduce 
NEW-OPTION-SET-LIMIT. This ensures that single substitutions are 
generated before multiple substitutions."))

(defvar *primsubs-remaining* nil)

(defun make-higher-order-expansions** (exp-nodes)
  (let ((lenlist (mapcar #'(lambda (x) (length (etree-components x))) exp-nodes))
	(returnlist nil))
    (apply-prim-subs-partial exp-nodes)
    (do ((lens lenlist (cdr lens))
	 (exps exp-nodes (cdr exps)))
	((or (null lens) (null exps)) (reverse returnlist))
	(let ((len (car lens))
	      (exp-node (car exps))
	      (new-exps nil))
	  (setq new-exps
		(subseq (etree-components exp-node) len))
	  (mapc #'deepen-one new-exps)
	  (setq new-exps
		(subseq (etree-components exp-node) len))
	  (push (mapcar #'cons new-exps
			(mapcar #'(lambda (x) 
				    (nconc (expansions-above x) (expansions-below x)))
				new-exps))
		returnlist)))
    returnlist))

(defun make-higher-order-expansions* (exp-node)
  (let ((len (length (etree-components exp-node)))
	(new-exps nil))
    (apply-prim-subs exp-node)
    (setq new-exps
	  (subseq (etree-components exp-node) len))
    (mapc #'deepen-one new-exps) 
    ;was deepen-to-literals*, but that takes forever if there are lots of primsubs
    ;so we deepen one step here, and finish the job in limited-mating-search-option.
    ;this also requires a small change to options-to-option-set-2. MB Fri May 16 17:00:18 1997
    (setq new-exps
	  (subseq (etree-components exp-node) len))
    (mapcar #'cons new-exps
	    (mapcar #'(lambda (x) 
			(nconc (expansions-above x) (expansions-below x)))
		    new-exps))))


(defun apply-prim-subs-partial (etree-nodes &optional (finished-old nil))
  (setq *after-primsub* t)
  (let ((var-finished t)
	(c ms91-interleave))
  (dolist (etree etree-nodes 
		 (if (and var-finished (not finished-old)) ; then we never did anything, so try with the new vars
		     (progn (setq *primsubs-remaining* nil)
			    (apply-prim-subs-partial etree-nodes t))
		   etree-nodes))
	  (let* ((vars (expansion-prim-vars etree))
		 (bdwff (get-shallow etree))
		 (scope-bdwff (gdr bdwff))
		 (vars-of-scope (substitutable-vars-of scope-bdwff))
		 l)
      (dolist (var vars)
	(setq var (car var))
	(setq l (length (cdr (assoc var (expansion-prim-vars etree))))) ; l = # of prim subs already
	(when (or (and finished-old (eq l 0))
		  (and (not finished-old) (neq l 0)))
	      ;apply primsubs to new vars only if we're done with the old ones.
          (when (apply-prim-subst-for-var var)
	      (let* ((term (dolist (term (expansion-terms etree) nil)
			    (when (memq var (hvars term))
			      (return term))))
                     new-term
                     (new-kids nil)
                     (new-vars-of-term 
                      (when term (mapcar #'exp-var-var (set-difference 
							(substitutable-vars-of term) vars-of-scope)))))
		(when term
		  (dolist (prim-sub (do ((list (find-prim-substs var) (cdr list))
					 (count 0 (1+ count)))
					((= count l) (do ((list1 list (cdr list1))
							  (list2 nil (cons (car list1) list2))
							  (count 0 (1+ count)))
							 ((or (null list1) (= count c))
							  (progn 
							  (setq var-finished (and var-finished 
										  (null list2)))
							  (unless (null list1)
							      (setq *primsubs-remaining* t))
							  (reverse list2))))))
				    (if (assoc var (expansion-prim-vars etree))
					(setf (cdr (assoc var (expansion-prim-vars etree)))
					      (append (cdr (assoc var (expansion-prim-vars etree)))
						      (nreverse new-kids)))
				      (push (cons var (nreverse new-kids))
					    (expansion-prim-vars etree))))
		    (setq new-term 
                          (substitute-term-var (subst-term prim-sub)
                                               (strip-exp-vars var)
                                               (strip-exp-vars term)))
                    (dolist (new-var new-vars-of-term)
                      (setq new-term
                            (substitute-term-var (funcall ren-var-fn new-var)
                                                 new-var
                                                 new-term)))
		    (duplicate-var etree)
		    (substitute-in-etree new-term 
					 (car (last (expansion-terms
						     etree)))
					 etree)
		    (push (car (last (etree-components etree)))
			  new-kids)))))))))))

(defun make-first-order-options (exp-nodes)
  (let* ((current-eproof master-eproof)
	 (new-bottom-exp-nodes 
	  (add-new-expansions-to-master-tree* exp-nodes)))
    (mapcar #'expansion-terms-to-option
	    new-bottom-exp-nodes)))

;;; Like make-higher-order-expansions*, but just dupes variable
(defun add-new-expansions-to-master-tree* (exp-nodes)
  (declare (special penalty-for-ordinary-dup))
  (let ((duped-exps nil))
    (dolist (exp exp-nodes)
      ;; this unless exists for this reason.  If we are using
      ;; path-focused-duplication (*ignore-first-order-dups*), then
      ;; the only reason we are doing the first-order-duplications is
      ;; if we can later apply prim-subs to them.  So see if prim-subs
      ;; can indeed be applied now, and avoid generating them if possible.
      (unless (or (infinite-p penalty-for-ordinary-dup) 
		  (and *ignore-first-order-dups* 
		       (not (apply-prim-subst-for-var (car (sel-exp-terms exp))))))
	(push exp duped-exps)
	(duplicate-var exp)
	(deepen-to-literals (car (last (etree-components exp))))))
    (mapcar #'cons (mapcar #'(lambda (x) (car (last (etree-components x))))
			   duped-exps)
	    (mapcar #'(lambda (x) 
			(nconc (expansions-above 
				(car (last (etree-components x))))
			       (expansions-below
				(car (last (etree-components x))))))
		    duped-exps))))
  
(defun get-initial-expansions* (etree)
  (let* ((exps (find-exp-nodes etree))
	 (lowest-exps
	  (remove-if #'(lambda (y)
			 (find-if #'(lambda (x) (find-etree-node 
						 #'expansion-p x))
				  (etree-components y)))
		     exps)))
    (mapcan #'(lambda (exp)
		(mapcar #'cons (etree-components exp)
			(mapcar #'expansions-above (etree-components exp))))
	    lowest-exps)))

;;; The argument exps below is always the type of list created by  get-initial-expansions*
;;; and is determined by its first element

(defun expansion-terms-to-option (exps)
  (let* ((new-exp (car exps))
	 (nodes (cdr exps))
	 (new-opt (make-option :nodes nodes)))
    (setf (option-weight-a new-opt) (compute-weight-a new-opt))
    (mapc #'(lambda (exp) (setf (expansion-to-options exp) new-opt))
	  nodes)
    (setf (minimal-option new-exp) new-opt)
    (dolist (exp nodes)
      (unless (minimal-option exp)
	(setf (minimal-option exp) new-opt)))
    (add-to-option-list new-opt)
    new-opt))


(defun add-to-option-list (opt)
  (push opt *option-list*)
  (incf *length-of-option-list*)
  (reorder-option-list))

(defun reorder-option-set-list ()
  (setq *option-set-list*
	(reorder-list *option-set-list* (symbol-function 'option-set-weight-d))))

(defun reorder-option-list ()
    (setq *option-list*
	  (reorder-list *option-list* (symbol-function 'option-weight-a))))


;;; Assume that when this function is called, the cdr of the list is
;;; ordered, and only the first element is out of place.  Will put the
;;; new element at the end of all options of the same weight.

(defun reorder-list (list weight-fn)
  (let* ((new-elt (car list))
	 (old-elts (cdr list))
	 (new-elt-weight (funcall weight-fn new-elt))
	 (higher-elt 
	  (position-if #'(lambda (x) (option-> (funcall weight-fn x)
					new-elt-weight))
		       old-elts)))
    (if higher-elt
	(nconc (subseq old-elts 0 higher-elt) 
		 (list new-elt)
		 (subseq old-elts higher-elt))
      (nconc old-elts (list new-elt)))))

(defmode ms91-simplest
  (flag-settings
   (ms91-weight-limit-range 1)
   (weight-a-coefficient 0)
   (weight-b-coefficient 1)
   (weight-c-coefficient 0)
   (weight-b-fn simplest-weight-b-fn)
   (reconsider-fn inf-weight)
   (new-option-set-limit 5)
   (options-generate-fn add-options-original)
   (options-generate-arg 75)
   (options-generate-update ident-arg))
  (mhelp "Generates option sets in the simplest possible order, in 
batches of five. Does not set the PRIMSUBS flags."))

(defmode ms91-original
  (flag-settings
   (ms91-weight-limit-range 3)
   (new-option-set-limit 5)
   (weight-a-coefficient 1)
   (weight-b-coefficient 1)
   (weight-c-coefficient 1)
   (weight-a-fn expansion-level-weight-a)
   (weight-b-fn simple-weight-b-fn)
   (weight-c-fn option-set-num-leaves)
   (reconsider-fn inf-weight)
   (penalty-for-each-primsub 3)
   (penalty-for-multiple-primsubs 5)
   (penalty-for-multiple-subs 5)
   (options-generate-fn add-options-original)
   (options-generate-arg 75)
   (options-generate-update ident-arg))
  (mhelp "The original flag settings. Does not set the PRIMSUBS flags."))

(defmode ms91-nodups
  (flag-settings
   (ms91-weight-limit-range infinity)
   (new-option-set-limit 1)
   (weight-a-coefficient 0)
   (weight-b-coefficient 1)
   (weight-c-coefficient 0)
   (weight-b-fn all-penalties-fn)
   (reconsider-fn inf-weight)
   (penalty-for-each-primsub 3)
   (penalty-for-multiple-primsubs 5)
   (penalty-for-multiple-subs 5)
   (penalty-for-ordinary-dup infinity)
   (options-generate-fn add-options-original)
   (options-generate-arg 75)
   (options-generate-update ident-arg))
  (mhelp "Generates one new option set at a time and accepts it, irrespective
of its weight. Does not generate option sets with ordinary duplications (i.e.
duplications not used by a primsub). Does not set the PRIMSUBS flags."))

(defmode ms91-deep
  (flag-settings
   (ms91-weight-limit-range infinity)
   (new-option-set-limit 1)
   (weight-a-coefficient 0)
   (weight-b-coefficient 1)
   (weight-c-coefficient 0)
   (weight-b-fn all-penalties-fn)
   (reconsider-fn inf-weight)
   (penalty-for-each-primsub 3)
   (penalty-for-multiple-primsubs 5)
   (penalty-for-multiple-subs infinity)
   (penalty-for-ordinary-dup infinity)
   (options-generate-fn add-options-original)
   (options-generate-arg 75)
   (options-generate-update ident-arg))
  (mhelp "Generates one new option set at a time and accepts it, 
irrespective of its weight. Does not generate option sets with ordinary 
duplications (i.e. duplications not used by a primsub), nor sets with 
multiple primsubs for the same variable; will instead generate recursive 
substitutions (i.e. will substitute for the expansion variables introduced
by the first lot of substitutions). Does not set the PRIMSUBS flags."))
  






