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

(deffile ms91-weights
 (part-of ms91)
 (mhelp "Functions and flags for computing weights in MS91-6 and MS91-7
mating-search procedures."))

;;; COMPUTING WEIGHTS
				  
(defflag weight-a-coefficient
  (flagtype integer+)
  (default 0) ;was 1
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "Coefficient to be used in multiplying weight-a of options in
the option-set of which we are computing weight-d. See WEIGHT-A-FN.
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set."))

(defflag weight-b-coefficient
  (flagtype integer+)
  (default 1)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp 
"Coefficient to be used in multiplying weight-b of option/option-subset
pairs for the option-set of which we are computing weight-d. See
WEIGHT-B-FN. The total weight of a set of options is the weight-a of 
each option plus the weight-b of the set plus the weight-c of the set."))

(defflag weight-c-coefficient
  (flagtype integer+)
  (default 0) ;was 1
  (subjects ms91-6 ms91-7 transmit)
  (mhelp 
"Coefficient to be used in multiplying weight-c of options in
the option-set of which we are computing weight-d. See WEIGHT-C-FN.
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set."))

(defflag penalty-for-multiple-primsubs
  (flagtype integer+-or-infinity)
  (default 5)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "Used in computing weight-b in MS91 search procedures.  Should be 
a nonnegative integer or the symbol INFINITY, and will be the amount 
of penalty given for using more than one primitive substitution for a single
variable. See WEIGHT-B-FN."))

(defflag penalty-for-multiple-subs
  (flagtype integer+-or-infinity)
  (default 5)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "Used in computing weight-b in MS91 search procedures.  Should be 
a nonnegative integer or the symbol INFINITY, and will be the amount 
of penalty given for using more than one substitution for a single
variable. See WEIGHT-B-FN."))

(defflag penalty-for-each-primsub
  (flagtype integer+-or-infinity)
  (default 3)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "Used in computing weight-b in MS91 search procedures.  Should be 
a nonnegative integer or the symbol INFINITY, and will be the amount 
of penalty given for using each primitive substitution. See WEIGHT-B-FN."))

(defflag penalty-for-ordinary-dup
  (flagtype integer+-or-infinity)
  (default infinity)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "Used in computing weight-b in MS91 search procedures.  Should be 
a nonnegative integer or the symbol INFINITY, and will be the amount 
of penalty given for each duplicate copy of a quantifier which is not 
used by a primitive substitution. See WEIGHT-B-FN."))

(defflag options-verbose
  (flagtype boolean)
  (default nil)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "If T, will output extra information about the options being 
considered."))

(defflag options-generate-fn
  (flagtype symbol)
  (default add-options-original)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "This is the function for deciding when to add new options
to the list from which option sets are generated. This is only called 
when new option sets are being generated, so if you are generating large
numbers of options sets at a time then you might not see an effect until
some time after your given criterion is satisfied. (Check the value of
NEW-OPTION-SETS-LIMIT if this seems to be the case.) The argument for 
this function is in the flag OPTIONS-GENERATE-ARG, and the function
to update that argument is in the flag OPTIONS-GENERATE-UPDATE.
The options are:
* ADD-OPTIONS-ORIGINAL generates new options when over 
  OPTIONS-GENERATE-ARG percent of the possible option sets have been 
  used, and each option appears in at least one option set.
* ADD-OPTIONS-COUNT generates new options when more than 
  OPTIONS-GENERATE-ARG different option sets have been tried.
* ADD-OPTIONS-WEIGHT generates new options when the lower end of the
  acceptable weight bracket for a new option set exceeds 
  OPTIONS-GENERATE-ARG.
* ADD-OPTIONS-SUBS generates new options when the number of 
  substitutions and duplications in the next option set (i.e.
  its SIMPLEST-WEIGHT-B) exceeds OPTIONS-GENERATE-ARG.
If OPTIONS-GENERATE-ARG is INFINITY, no new options are ever generated."))

(definfo add-options-original
  (mhelp "A flag setting for OPTIONS-GENERATE-FN.
Generate new options when over OPTIONS-GENERATE-ARG percent of 
the possible option sets have been used, and each option appears 
in at least one option set."))

(definfo add-options-weight
  (mhelp "A flag setting for OPTIONS-GENERATE-FN.
Generate new options when the lower end of the acceptable weight 
bracket for a new option set exceeds OPTIONS-GENERATE-ARG."))

(definfo add-options-count
  (mhelp "A flag setting for OPTIONS-GENERATE-FN.
Generate new options when more than OPTIONS-GENERATE-ARG 
different option sets have been tried."))

(definfo add-options-subs
  (mhelp "A flag setting for OPTIONS-GENERATE-FN.
Generate new options when the number of substitutions and 
duplications in the next option set (i.e. its SIMPLEST-WEIGHT-B)
exceeds OPTIONS-GENERATE-ARG."))

(defflag options-generate-arg
  (flagtype integer+-or-infinity)
  (default 75)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "The argument used by the function given in the flag 
OPTIONS-GENERATE-FN. If this argument is INFINITY then new options will
never be generated. See the help message for OPTIONS-GENERATE-FN."))

(defflag options-generate-update
  (flagtype symbol)
  (default ident-arg)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "The function used to update the value of the flag
OPTIONS-GENERATE-ARG. Current possibilities are:
* IDENT-ARG leaves the value unchanged.
* DOUBLE-ARG doubles the value.
* SQUARE-ARG squares the value.
* INF-ARG makes the value INFINITY.
Note that a value of INFINITY means that new options will never be 
generated."))

(definfo ident-arg
  (mhelp "A flag setting for OPTIONS-GENERATE-UPDATE.
Each time options are updated, leave the value of 
OPTIONS-GENERATE-ARG unchanged."))

(definfo double-arg
  (mhelp "A flag setting for OPTIONS-GENERATE-UPDATE.
Each time options are updated, double the value of 
OPTIONS-GENERATE-ARG."))

(definfo square-arg
  (mhelp "A flag setting for OPTIONS-GENERATE-UPDATE.
Each time options are updated, square the value of 
OPTIONS-GENERATE-ARG."))

(definfo inf-arg
  (mhelp "A flag setting for OPTIONS-GENERATE-UPDATE.
Each time options are update, make the value of 
OPTIONS-GENERATE-ARG infinity."))

(defun compute-weight-d (options)
  (let ((weight (compute-weight-d-real options)))
    (if options-verbose (msg "  Total weight : " weight t))
    weight))

(defun compute-weight-d-real (options)
  (if (and options-verbose (gethash options *weight-d-hash-table*)) 
      (msg "Getting previously computed weight."))
  (or (gethash options *weight-d-hash-table*)
      (let* ((weight-b-val (if (zerop weight-b-coefficient)
			       0
			     (compute-weight-b options)))
	     (weight-a-val (if (zerop weight-a-coefficient)
			       0
			     (unless (infinite-p weight-b-val)
				     (sum-weight-a options))))
	     (weight-c-val (if (zerop weight-c-coefficient)
			       0
			     (unless (or (infinite-p weight-b-val)
					 (infinite-p weight-a-val))
			       (compute-weight-c options)))))
	(if options-verbose (msg "Calculating weights for option " options "." t "A: " 
weight-a-val " B: " weight-b-val " C: " weight-c-val))
	(if (or (infinite-p weight-a-val)
		(infinite-p weight-b-val)
		(infinite-p weight-c-val))
	    infinity
	  (setf (gethash (copy-list options) *weight-d-hash-table*)
	    (+ (* weight-a-coefficient weight-a-val)
	       (* weight-b-coefficient weight-b-val)
	       (* weight-c-coefficient weight-c-val)))))))

;;;; COMPUTE WEIGHT A

(defun sum-weight-a (options)
  (let ((sum 0))
    (dolist (option options sum)
      (let ((weight (option-weight-a option)))
	(when (infinite-p weight)
	  (return-from sum-weight-a :infinite))
	(incf sum weight)))))

(defflag weight-a-fn
   (flagtype symbol)
   (default expansion-level-weight-a)
   (subjects ms91-6 ms91-7 transmit)
   (mhelp "A function that should take an option as argument and
return a value to be used as its weight-a. Currently, the only 
such predefined function is EXPANSION-LEVEL-WEIGHT-A, which returns
the expansion level of the option to be used as a weight.
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set."))

(definfo expansion-level-weight-a
  (mhelp "A setting for the flag WEIGHT-A-FN.
Returns the expansion level of the option to be used as a weight.
The expansion level is (roughly) the number of times that NAME-PRIM
had to be called in order to generate this option -- usually 1."))

(defun compute-weight-a (option)
  (funcall (symbol-function weight-a-fn) option))

(defun expansion-level-weight-a (option)
  (declare (ignore option))
  *expansion-level*)

;;;; COMPUTE WEIGHT B

(defflag weight-b-fn
   (flagtype symbol)
   (default simplest-weight-b-fn) ;was simple-...
   (subjects ms91-6 ms91-7 transmit)
   (mhelp "A function that should take an option set and return a value 
to be used as its weight-b. Currently, the only such predefined functions are: 
* SIMPLE-WEIGHT-B-FN, which returns the sum of the penalties for the primsubs, 
  multiple subs and duplications used in the option set (see the flags 
  PENALTY-FOR-EACH-PRIMSUB, PENALTY-FOR-MULTIPLE-PRIMSUBS and  
  PENALTY-FOR-MULTIPLE-SUBS for more information),
* ALL-PENALTIES-FN which is much the same as SIMPLE-WEIGHT-B-FN but also adds
  a penalty for extra duplications given by the PENALTY-FOR-ORDINARY-DUP 
  flag, and 
* SIMPLEST-WEIGHT-B-FN, which returns 1 for the original option set and adds 
  1 for each primsub or duplication (the idea is to set the coefficients of 
  weight-a and weight-c to zero while using SIMPLEST-WEIGHT-B-FN).
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set."))

(definfo simple-weight-b-fn
  (mhelp "A setting for WEIGHT-B-FN.
Returns the sum of the penalties for the primsubs, multiple subs 
and duplications used in the option set (see the flags 
PENALTY-FOR-EACH-PRIMSUB, PENALTY-FOR-MULTIPLE-PRIMSUBS and  
PENALTY-FOR-MULTIPLE-SUBS for more information)"))

(definfo all-penalties-fn
  (mhelp "A setting for WEIGHT-B-FN.
Much the same as SIMPLE-WEIGHT-B-FN but also adds a penalty for 
extra duplications given by the PENALTY-FOR-ORDINARY-DUP flag."))

(definfo simplest-weight-b-fn
  (mhelp "A setting for WEIGHT-B-FN.
Returns 1 for the original option set and adds 
1 for each primsub or duplication (the idea is to set the coefficients of 
weight-a and weight-c to zero while using SIMPLEST-WEIGHT-B-FN)."))

(defun compute-weight-b (options)
  (funcall (symbol-function weight-b-fn) options))

(defun simple-dupe-p (etree-node)
" Assume that etree-node is a child of an expansion node. Return T if
its expansion term is just a variable."
    (setq cl-user::etree-node99 etree-node)
  (let* ((parent (etree-parent etree-node))
	 (term (nth (position etree-node (etree-components parent))
		    (expansion-terms parent))))
    (wffeq (exp-var-subst term) (exp-var-var term))))

(defun simple-weight-b-fn (option-list)
  " Computes penalties for 3 things.  First, calls 
COMPUTE-PENALTY-FOR-LACK-OF-EARLIER-COPY, which computes the penalty
for using an expansion which is simple duplication without using all earlier
simple duplications for the same variable.
 Next, it calls COMPUTE-PENALTY-FOR-PRIMSUBS, which computes the penalty
for each primsub, as well as using multiple primsubs for a single
variable.
 Next, it calls COMPUTE-PENALTY-FOR-MULTIPLE-SUBS, which computes the
penalty for using multiple substitutions for the same variable,
whether simple or primitive.  
 The penalties are added together and returned."
  (let* ((exps 
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
    (setq total (compute-penalty-for-lack-of-earlier-copy exps-by-parent))
    (when (infinite-p total) (return-from simple-weight-b-fn total))
    (setq total
      (option-+ total (compute-penalty-for-primsubs exps-by-parent)))
    (when (infinite-p total) (return-from simple-weight-b-fn total))
    (setq total
      (option-+ total (compute-penalty-for-multiple-subs exps-by-parent)))
    (when (infinite-p total) (return-from simple-weight-b-fn total))
    total))

(defun weight-b-a (options)
  (let ((max 0))
    (dolist (option options max)
      (let ((weight (option-weight-a option)))
	(when (infinite-p weight)
	  (return :infinite))
	(if (> weight max) (setq max weight))))))

(defun all-penalties-fn (option-list)
  (let* ((exps 
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
    (setq total (compute-penalty-for-ordinary-dups exps-by-parent))
    (when (infinite-p total) (return-from all-penalties-fn total))
    (setq total 
	  (option-+ total (compute-penalty-for-lack-of-earlier-copy exps-by-parent)))
    (when (infinite-p total) (return-from all-penalties-fn total))
    (setq total
      (option-+ total (compute-penalty-for-primsubs exps-by-parent)))
    (when (infinite-p total) (return-from all-penalties-fn total))
    (setq total
      (option-+ total (compute-penalty-for-multiple-subs exps-by-parent)))
    (when (infinite-p total) (return-from all-penalties-fn total))
    total))

(defun simplest-weight-b-fn (option-list)
  "Gives a weight of 1 to the original option set, and then adds 1 for each 
primsub, multiple sub or duplication."
  (let* ((exps 
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
	 (total 1))
    (setq total
      (option-+ total (compute-simplest-penalty-for-primsubs exps-by-parent)))
    (setq total
      (option-+ total (compute-simplest-penalty-for-multiple-subs exps-by-parent)))
    total))

(defun compute-penalty-for-ordinary-dups (exp-lists)
  (let ((total 0))
    (dolist (list exp-lists total)
      (let* ((expansions (cdr list))
	     (num-primsubs (count-if-not #'simple-dupe-p expansions)))
	(unless (and (= (length expansions) 1) (= num-primsubs 0))
		(unless (= (length expansions) num-primsubs)
		(if (> num-primsubs 0)
			; then we have (- (length expansions) num-primsubs) ord dups.
		    (if (or (infinite-p penalty-for-ordinary-dup)
			    (infinite-p total))
			(setq total infinity)
		      (setq total (+ total (* penalty-for-ordinary-dup (- (length expansions) num-primsubs)))))
		;o/w we have (1- (length expansions)) ord dups.
		  (if (or (infinite-p penalty-for-ordinary-dup)
			  (infinite-p total))
		      (setq total infinity)
		    (setq total (+ total (* penalty-for-ordinary-dup (1- (length expansions)))))))))
	(when (infinite-p total)
	      (return-from compute-penalty-for-ordinary-dups total))))))

(defun compute-penalty-for-lack-of-earlier-copy (exp-lists)
  " Each member of exp-lists is a list, with an expansion node at the head
and the remainder being some of its expansions.  Return INFINITY
if any of the simple expansions appears without all earlier simple 
expansions also appearing, else return 0."
  (dolist (list exp-lists 0)
    (let* ((expansion-node (car list))
	   (all-expansions (etree-components expansion-node))
	   (expansions (cdr list)))
      (when 
	  (some #'(lambda (expansion)
		    (and (simple-dupe-p expansion)
			 (some #'(lambda (other-expansion)
				   (and (not (member other-expansion
						     expansions))
					(simple-dupe-p other-expansion)))
			       (subseq all-expansions 0 
				       (position expansion all-expansions)))))
		expansions)
	(return-from compute-penalty-for-lack-of-earlier-copy infinity)))))


(defun compute-penalty-for-primsubs (exp-lists)
  " Each member of exp-lists is a list, with an expansion node at the head
and the remainder being some of its expansions.  For each such
list, add value of PENALTY-FOR-MULTIPLE-PRIMSUBS for each primsub after the
first one, and add value of PENALTY-FOR-EACH-PRIMSUB for each primsub."
  (let ((total 0))
    (dolist (list exp-lists total)
      (let* ((expansions (cdr list))
	     (num-primsubs (count-if-not #'simple-dupe-p expansions)))
	(dotimes (i num-primsubs)
	  (when (> i 0)
	    (setq total (option-+ total
				  penalty-for-multiple-primsubs)))
	  (setq total (option-+ total penalty-for-each-primsub))
	  (when (infinite-p total) 
	    (return-from compute-penalty-for-primsubs total)))))))

(defun compute-simplest-penalty-for-primsubs (exp-lists)
  " Each member of exp-lists is a list, with an expansion node at the head
and the remainder being some of its expansions.  For each such
list, add 1 for each primsub."
  (let ((total 0))
    (dolist (list exp-lists total)
      (let* ((expansions (cdr list))
	     (num-primsubs (count-if-not #'simple-dupe-p expansions)))
	  (setq total (option-+ total num-primsubs))))))

;;; each member of exp-lists is a list, with an expansion node at the head
;;; and the remainder being some of its expansions.  For each such
;;; list, add penalty-for-multiple-subs for each sub after the
;;; first one. 
	
(defun compute-penalty-for-multiple-subs (exp-lists)
" Each member of exp-lists is a list, with an expansion node at the head
and the remainder being some of its expansions.  For each such
list, add value of PENALTY-FOR-MULTIPLE-SUBS for each sub after the
first one. "
  (let ((total 0))
    (dolist (list exp-lists total)
      (let ((expansions (cdr list)))
	(dotimes (i (1- (length expansions))
		   total)
	  (setq total (option-+ total penalty-for-multiple-subs))
	  (when (infinite-p total) 
	    (return-from compute-penalty-for-multiple-subs
	      total)))))))

(defun compute-simplest-penalty-for-multiple-subs (exp-lists)
" Each member of exp-lists is a list, with an expansion node at the head
and the remainder being some of its expansions.  For each such
list, add 1 for each sub after the first one. "
  (let ((total 0))
    (dolist (list exp-lists total)
      (let ((expansions (cdr list)))
	(dotimes (i (1- (length expansions))
		   total)
	  (setq total (option-+ total 1)))))))

;;;; COMPUTE WEIGHT C


(defflag weight-c-fn
   (flagtype symbol)
;; (default option-set-num-vpaths)
   (default option-set-num-leaves)
   (subjects ms91-6 ms91-7 transmit)
   (mhelp "A function that should take an list of options as argument and
return a value to be used as its weight-c. Currently, the only such 
predefined functions are OPTION-SET-NUM-VPATHS, which returns the number of
vertical paths through the relevant etree, and OPTION-SET-NUM-LEAVES, which 
returns the number of leaves in the relevant etree.
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set."))

(definfo option-set-num-vpaths
  (mhelp "A flag setting for WEIGHT-C-FN.
Returns the number of vertical paths through the relevant etree."))

(definfo option-set-num-leaves
  (mhelp "A flag setting for WEIGHT-C-FN.
Returns the number of leaves in the relevant etree."))

(defun compute-weight-c (options)
  (funcall (symbol-function weight-c-fn) options))

(defun option-set-num-leaves (options)
  (options-number-of-leaves
   (eproof-etree current-eproof)
   (remove-duplicates (apply #'append (mapcar #'option-nodes options)))))

(defun options-number-of-leaves (etree included-nodes)
  (if (expansion-p etree)
      (let ((included-kids (remove-if-not #'(lambda (x) 
					      (memq x included-nodes))
					  (etree-components etree))))
	(if included-kids
	    (case (etree-junctive etree)
	      (con (do ((nodes (cdr included-kids) (cdr nodes))
			(node (car included-kids) (car nodes))
			(sum 0 (+ sum (options-number-of-leaves 
					 node
					 included-nodes))))
		       ((null node) sum)))
	      (dis (do ((nodes (cdr included-kids) (cdr nodes))
			(node (car included-kids) (car nodes))
			(sum 0 (+ sum (options-number-of-leaves 
				       node
				       included-nodes))))   
		       ((null node) sum))))
	  1))
    (ecase (etree-junctive etree)
      (con (do ((nodes (cdr (etree-components etree)) (cdr nodes))
		(node (car (etree-components etree)) (car nodes))
		(sum 1 (+ sum (options-number-of-leaves 
				 node
				 included-nodes))))
	       ((null node) sum)))
      (dis (do ((nodes (cdr (etree-components etree)) (cdr nodes))
		(node (car (etree-components etree)) (car nodes))
		(sum 1 (+ sum (options-number-of-leaves 
			       node
			       included-nodes))))   
	       ((null node) sum)))
      (neutral (options-number-of-leaves (car (etree-components etree))
						 included-nodes))
      ((nil) 0))))

(defun option-set-num-vpaths (options)
  (options-number-of-vertical-paths
   (eproof-etree current-eproof)
   (remove-duplicates (apply #'append (mapcar #'option-nodes options)))))

(defun options-number-of-vertical-paths (etree included-nodes)
  (if (expansion-p etree)
      (let ((included-kids (remove-if-not #'(lambda (x) 
					      (memq x included-nodes))
					  (etree-components etree))))
	(if included-kids
	    (case (etree-junctive etree)
	      (con (do ((nodes (cdr included-kids) (cdr nodes))
			(node (car included-kids) (car nodes))
			(prod 1 (* prod (options-number-of-vertical-paths 
					 node
					 included-nodes))))
		       ((null node) prod)))
	      (dis (do ((nodes (cdr included-kids) (cdr nodes))
			(node (car included-kids) (car nodes))
			(sum 0 (+ sum (options-number-of-vertical-paths 
				       node
				       included-nodes))))   
		       ((null node) sum))))
	  1))
    (ecase (etree-junctive etree)
      (con (do ((nodes (cdr (etree-components etree)) (cdr nodes))
		(node (car (etree-components etree)) (car nodes))
		(prod 1 (* prod (options-number-of-vertical-paths 
				 node
				 included-nodes))))
	       ((null node) prod)))
      (dis (do ((nodes (cdr (etree-components etree)) (cdr nodes))
		(node (car (etree-components etree)) (car nodes))
		(sum 0 (+ sum (options-number-of-vertical-paths 
			       node
			       included-nodes))))   
	       ((null node) sum)))
      (neutral (options-number-of-vertical-paths (car (etree-components etree))
						 included-nodes))
      ((nil) 1))))

(defflag reconsider-fn
   (flagtype symbol)
   (default double-weight)
   (subjects ms91-6 ms91-7 transmit)
   (mhelp "A function that should take a weight as argument and
return a value to be used as a new weight after the associated option set
runs out of time. Currently, the predefined functions are INF-WEIGHT 
SQUARE-WEIGHT, DOUBLE-WEIGHT and INCREMENT-WEIGHT (which, respectively,
make reconsidering an old option set impossible, very unlikely,
quite likely and probable). INCREMENT-WEIGHT actually adds 10 to the weight 
of an option set, as adding 1 is insignificant under most circumstances."))

(definfo inf-weight
  (mhelp "A flag setting for RECONSIDER-FN.
When an option set runs out of time, reset its weight to INFINITE
(and hence prevent its ever being reconsidered)."))

(definfo square-weight
  (mhelp "A flag setting for RECONSIDER-FN.
When an option set runs out of time, square its weight."))

(definfo double-weight
  (mhelp "A flag setting for RECONSIDER-FN.
When an option set runs out of time, double its weight."))

(definfo increment-weight
  (mhelp "A flag setting for RECONSIDER-FN.
When an option set runs out of time, add 10 to its weight."))

(defun reconsider-weight (weight)
  (funcall (symbol-function reconsider-fn) weight))

(defun inf-weight (weight) 
  (declare (ignore weight)) 
  infinity)

(defun square-weight (weight) (* weight weight))

(defun double-weight (weight) (* 2 weight))

(defun increment-weight (weight) (+ 10 weight)) ;; 10 is ad hoc, but 1 is too small

;;;; WEIGHT AUXILIARY FUNCTIONS

;;; Determine if wt1 > wt2, but allow for possibility that either is
;;; infinite.

(defun option-> (wt1 wt2)
  (cond ((infinite-p wt2) nil)
	((infinite-p wt1) t)
	(t (> wt1 wt2))))

;;; Determine if wt1 >= wt2, but allow for possibility that either is
;;; infinite.

(defun option->= (wt1 wt2)
  (cond ((infinite-p wt1) t)
	((infinite-p wt2) nil)
	(t (>= wt1 wt2))))

;;; Determine if wt1 = wt2, but allow for possibility that either is
;;; infinite.

(defun option-= (wt1 wt2)
  (cond ((infinite-p wt1) (infinite-p wt2))
	((infinite-p wt2) nil)
	(t (= wt1 wt2))))

;;; Add wt1 & wt2, but allow for possibility that either is
;;; infinite.

(defun option-+ (wt1 wt2)
  (cond ((infinite-p wt1) wt1)
	((infinite-p wt2) wt2)
	(t (+ wt1 wt2))))

;;; Find the minimum weight-a of some list of options.  Don't forget that
;;; they could be infinite.

(defun option-min (option-list)
  (let ((min infinity))
    (dolist (option option-list min)
      (when (option-> min (option-weight-a option))
	(setq min (option-weight-a option)))))) 


;;; Find the max weight-a of the list of options, but only of the non-infinite
;;; ones.
;;; never actually used in the code - MB

(defun option-non-inf-max (option-list)
  (let ((max 0))
    (dolist (option option-list max)
      (let ((wt (option-weight-a option)))
	(when (and (not (infinite-p wt))
		   (option-> wt max))
	  (setq max wt))))))


(defun square-arg ()
  (setq options-generate-arg (* options-generate-arg options-generate-arg)))

(defun double-arg ()
  (setq options-generate-arg (+ options-generate-arg options-generate-arg)))

(defun ident-arg ())

(defun inf-arg ()
  (setq options-generate-arg infinity))
