;;; -*- Mode: Lisp -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile unif-subs
  (part-of unification)
  (extension lisp)
  (mhelp "Unification functions."))

(defun reduce-dpairlist (dpairlist)
  (do ((dpairs dpairlist (cdr dpairs)))
      ((null (cdr dpairs)) dpairlist)
    (let ((first (car dpairs)))
      (dolist (second (cdr dpairs))
	(when (and (eq (car first) (car second))
		   (eq (cdr first) (cdr first)))
	  (setq dpairlist (delete second dpairlist)))))))
	    
(defun find-unif-node (node name)
  (if (string= (node-print-name node) name) node
      (if (node-sons node)
	  (dolist (son (node-sons node) nil)
	    (let ((found (find-unif-node son name)))
	      (if found (return found))))
	  nil)))
      
(defflag subsumption-nodes
  (flagtype symbol)
  (default 'lp-nodes)
  (subjects unification transmit)
  (relevancy-preconditions
   (subsumption-check (and subsumption-check (not (and max-substs-quick max-substs-var))))
   (max-substs-var (and subsumption-check (not (and max-substs-quick max-substs-var))))
   (max-substs-quick (and subsumption-check (not (and max-substs-quick max-substs-var)))))
  (irrelevancy-preconditions
   (subsumption-check (not subsumption-check)))
  (mhelp "When SUBSUMPTION-CHECK is T, this flag determines
which other nodes should be examined to see if they subsume the
new node being considered. The values are as follows, arranged
in order with the quickest first:
PATH-NODES checks only those nodes on the path from the root to
  the new node.
LEAF-NODES checks only the leaf nodes in the tree.
LP-NODES checks leaf nodes and those on the path to the
  new node.
ALL-NODES checks every node in the tree.
Some nodes will always be excluded from subsumption checking,
regardless of the value of this flag. In particular, two nodes
representing different sets of connections will not be 
compared. 
This flag only applies to the UN88 procedure; in UN90, if
subsumption-checking is used at all, it is implicitly
set to ALL-NODES.")) 

(definfo path-nodes
  (mhelp "A setting for SUBSUMPTION-NODES.
Checks only those nodes in the unification tree on the path 
from the root to the new node."))

(definfo leaf-nodes
  (mhelp "A setting for SUBSUMPTION-NODES.
Checks only those nodes in the unification tree which are leaves."))

(definfo all-nodes
  (mhelp "A setting for SUBSUMPTION-NODES.
Checks all nodes in the unification tree."))

(defflag subsumption-depth
  (flagtype integer+-or-infinity)
  (default :infinite)
  (subjects unification transmit)
  (relevancy-preconditions
   (subsumption-check (and subsumption-check (eq default-ms 'ms89)
			   (not (and max-substs-quick max-substs-var))))
   (default-ms (and subsumption-check (eq default-ms 'ms89)
		    (not (and max-substs-quick max-substs-var))))
   (max-substs-var (and subsumption-check (eq default-ms 'ms89)
			(not (and max-substs-quick max-substs-var))))
   (max-substs-quick (and subsumption-check (eq default-ms 'ms89)
			  (not (and max-substs-quick max-substs-var)))))
  (irrelevancy-preconditions
   (subsumption-check (not subsumption-check))
   (default-ms (not (eq default-ms 'ms89)))
   (max-substs-var max-substs-var)
   (max-substs-quick max-substs-quick))
  (mhelp "Subsumption checking takes a lot of time, compared 
to unification, which means that checking a new node may
take more time than it could possibly save, particularly
if the node is almost at the maximum depth for the 
unification tree.
In the unification tree, new nodes at depth SUBSUMPTION-DEPTH
or deeper will not be subsumption-checked; other new nodes
will be. Having SUBSUMPTION-DEPTH INFINITY means that all new 
nodes are subsumption-checked; SUBSUMPTION-DEPTH 0 is just
a slower way of turning subsumption-checking off altogether.
(You should use SUBSUMPTION-CHECK NIL to do that!)
This flag only applies when SUBSUMPTION-CHECK is T.
See also SUBSUMPTION-NODES."))

(defun leaf-node-p (root node)
  (not (node-sons (find-unification-node-rec node root))))

(defun path-node-p (node-name node)
  (descendentp node node-name) )

(defun lp-node-p (root node node-name)
  (or (leaf-node-p root node) (path-node-p node-name node)))

(defun dont-consider (node node-name root)
;node is the name of the old node. node-name is the name of the new node.
  (case subsumption-nodes
	(all-nodes nil)
	(leaf-nodes (not (leaf-node-p root node)))
	(path-nodes (not (path-node-p node-name node)))
	(lp-nodes (not (lp-node-p root node node-name)))))

(defun unif-subsetp (new-node old-node)
  (if (and (null old-node) (null new-node)) t
    (if (or (null new-node) (null old-node)) nil 
    (let ((n1 (car new-node))
	  (o1 (car old-node)))
;want to check if every element of n1 occurs in o1. Both are in lex. order.
;can't use regular subsetp command because we need to consider them as multisets.
      (if (or (> (caar n1) (caar o1))
	      (and (= (caar n1) (caar o1))
		   (> (cdar n1) (cdar o1))))
	  ;then n1 > o1, and we discard the first elt of o1
	  (unif-subsetp new-node (cdr old-node))
	(if (or (< (caar n1) (caar o1))
		(and (= (caar n1) (caar o1))
		     (< (cdar n1) (cdar o1))))
	    ;then n1 < o1, and we can fail
	    nil
	  ;otherwise n1=o1
	  (unif-subsetp (cdr new-node) (cdr old-node))))))))

(defun different-connections (node1 node2)
  (set-exclusive-or (connections-down-to-here node1) (connections-down-to-here node2)))

(defun connections-down-to-here (node)
  (if (node-parent node) 
      (append (node-connections node) (connections-down-to-here (node-parent node)))
    nil))

(defun subsumption-check (root next-node ck-subsumed subst-hashtable &optional (start-time nil) (time-limit nil))
  (when (and subsumption-check ck-subsumed (option-> subsumption-depth (node-depth next-node)))
	(let* ((node-name (node-print-name next-node))
	       (sub-check (or (node-sub-check next-node) (fill-sub-check-slot next-node)))
	       (sub-output (mapcar #'car sub-check)))
	  (when (eq unify-verbose 'max) (msg t "SC" node-name ":" sub-output " compares to " 3))
	  (dolist (elt ck-subsumed)
		  (dolist (name (cddr elt))
			  (unless (or (string= name node-name) 
				      (dont-consider name node-name root)
				      (different-connections next-node (find-unification-node-rec name root)))
				  ;i.e. we don't look for subsumptions of a node by itself,
				  ;we exclude everything that SUBSUMPTION-NODES says we should,
				  ;and if the old node represents a different set of connections we ignore it.
				  (when (eq unify-verbose 'max) (msg name " "))
				  (when (and start-time time-limit
					     (>= (/ (- (get-net-internal-run-time) start-time)
						    internal-time-units-per-second)
						 time-limit))
					(return-from subsumption-check nil))
				  (let* ((nd (find-unification-node-rec name root))
					 (nd-sub (or (node-sub-check nd) (fill-sub-check-slot nd))))
				    (if (unif-subsetp sub-check nd-sub)
					(if (canonical-form-check sub-check nd-sub nd)
					    (progn
					      (propagate-failure-downwards next-node subst-hashtable)
					      (case unify-verbose 
						    (max (msg "MATCH! [" name " subsumes " node-name "]"))
						    (med (msg " [" name " subsumes " node-name "]")) 
						    (min (msg "!"))
						    (t ))
					      (return-from subsumption-check t))
					  (progn 
					    (case unify-verbose 
						  (max (msg "MATCH? "))
						  ((med min) (msg "?"))
						  (t ))))
				      (when (eq unify-verbose 'max) (msg " ")))))))
	  (when (eq unify-verbose 'max) (msg "sc end" t)))))

(defun propagate-failure-downwards (next-node subst-hashtable)
  (when (node-parent next-node) (setf (node-sons (node-parent next-node))
				    (remove-if #'(lambda (x) (eq x next-node))
					       (node-sons (node-parent next-node)))))
  (maphash #'(lambda (x y) (trim-subst-hashtable x y next-node
						 subst-hashtable))
	   subst-hashtable)
  (dolist (node (node-sons next-node)) (propagate-failure-downwards node subst-hashtable)))

(defun trim-subst-hashtable (key entry node subst-hashtable)
  (if (member (node-print-name node) (assoc 'nodelist entry) :test 'string=)
      (let ((l nil))
	(dolist (elt (gethash key subst-hashtable))
		(if (eq (car elt) 'nodelist)
		    (push 
		     (remove-if #'(lambda (x) (string= x (node-print-name node))) elt)
		     l)
		  (push elt l)))
	(setf (gethash key subst-hashtable) (nreverse l)))))
      
(defun canonical-form-check (new-node-list old-node-list old-node)
  ;we need to a) enumerate all possible ways in which old-node can be considered
  ;              a subset of new-node,
  ;           b) produce canonical forms for each of these subsets of new-node.
  ;           c) check whether any of these is the same as the canonical form for old-node.
  (let ((subset-list (enumerate-unif-subsets old-node-list new-node-list)))
    (dolist (poss-subset subset-list nil)
	    ; now we have to work on the cdr of each element, renaming h variables.
	    (when (wff-canon (car poss-subset) (cdr poss-subset) nil) 
		  (return old-node)))))

(defun wff-canon (wfflist1 wfflist2 subs &optional (renamed-1 nil) (renamed-2 nil))
  (if (null wfflist1) (cons renamed-1 renamed-2)
    ;we know both lists are the same length
    (let ((first (car wfflist1))
	  (second (car wfflist2))
	  (firstwff (cdar wfflist1))
	  (secondwff (cdar wfflist2)))
      (multiple-value-bind (renamable new1 new2 subs)
			   (wff-rename firstwff secondwff subs)
			   (if renamable 
			       (wff-canon (cdr wfflist1) (cdr wfflist2) subs
					  (cons (cons (car first) new1) renamed-1) 
					  (cons (cons (car second) new2) renamed-2))
			     nil)))))

(defun wff-rename (wff1 wff2 subs)
  ;we will use the fact that free-vars-of always returns the free variables 
  ;in the same order to generate a list of substitutions. Then we apply the
  ;substitutions to the formulas, and finally we check whether they are wffeq-ab to each other.
  (let ((fv1 (free-vars-of wff1))
	(fv2 (free-vars-of wff2)))
    (if (eq (length fv1) (length fv2))
	(let ((new-sublist (remove-if #'(lambda (x) (eq (car x) (cdr x))) 
				      (append (mapcar 'cons fv1 fv2) subs))))
	  (do* ((wff-new1 wff1 (substitute-l-term-var (cdr sub) (car sub) wff-new1))
		(wff-new2 wff2 (substitute-l-term-var (cdr sub) (car sub) wff-new2))
		(subs new-sublist (mapcar #'(lambda (x) 
					      (cons (if (eq (car x) (car sub)) (cdr sub) (car x))
						    (if (eq (cdr x) (car sub)) (cdr sub) (cdr x))))
					  (cdr subs)))
		(sub (car subs) (car subs)))
	       ((or (null sub) 
		    (not (or (h-var-check (car sub)) (h-var-check (cdr sub)))))
		(if (null sub)
		    (if (wffeq-ab wff-new1 wff-new2) 
			(values t wff-new1 wff-new2 new-sublist) 
		      (values nil))
		  ; o/w neither is an h-variable, and we have already removed all
		  ; identity substitutions, so we can't do this
		  (values nil)))
	       (unless (h-var-check (car sub)) (setq sub (cons (cdr sub) (car sub)))))))))

(defun h-var-check (var)
  (let ((str (princ-to-string var))
	(val (elt (princ-to-string h-var-prefix) 0)))
    (and (eq (elt str 0) val)
	 (eq (elt str 1) #\^))))
    
(defvar subset-arrangements-list nil)

(defun enumerate-unif-subsets (old-list new-list)
  (declare (special subset-arrangements-list))
  (setq subset-arrangements-list nil)
  (enumerate-unif-subsets-real old-list new-list)
  (rearrange-unif-sl subset-arrangements-list))

(defun rearrange-unif-sl (sl)
  ;sl is currently (((o1 . n1) (o2 . n2) ...) ((o1 . n2) (o2 . n1)...)) etc
  ; we want (((o1 o2) . (n1 n2)) ((o1 o2) . (n2 n1))) etc
  (if (null sl) nil
    (let* ((carlist (car sl))
	   (caarlist (mapcar 'car carlist))
	   (cadrlist (mapcar 'cdr carlist)))
      (cons (cons caarlist cadrlist) (rearrange-unif-sl (cdr sl))))))

(defun enumerate-unif-subsets-real (old-list new-list)
  ;lists are arranged in lexicographic order by hash function.
  ;every element of old-list occurs in new-list at least once.
  (declare (special subset-arrangements-list))
  (let* ((old-first (car old-list))
	 (new-first (car new-list))
	 (old-hash1 (caar old-first))
	 (old-hash2 (cdar old-first))
	 (new-hash1 (caar new-first))
	 (new-hash2 (cdar new-first)))
    (if (or (null old-first) (null new-first))
	(list nil)
      (if (or (> old-hash1 new-hash1)
	      (and (= old-hash1 new-hash1)
		   (> old-hash2 new-hash2)))
	  ;then the first element of new-list is irrelevant
	  (enumerate-unif-subsets-real old-list (cdr new-list))
	;otherwise the first element of old-list *can't* be irrelevant,
	;so we must have a match. Now we need to work out how many matches we have...
	(let* ((old-matches (remove-if-not 
			     #'(lambda (y) (and (= old-hash1 (caar y)) (= old-hash2 (cdar y))))
			     old-list))
	       (old-nonmatches (remove-if
				#'(lambda (y) (and (= old-hash1 (caar y)) (= old-hash2 (cdar y))))
				old-list))
	       (new-matches (remove-if-not 
			     #'(lambda (y) (and (= new-hash1 (caar y)) (= new-hash2 (cdar y))))
			     new-list))
	       (new-nonmatches (remove-if
				#'(lambda (y) (and (= new-hash1 (caar y)) (= new-hash2 (cdar y))))
				new-list)))
	  (if (= new-hash1 new-hash2)
	      (dolist (om old-matches)
		      (dolist (nm new-matches)
			      (let ((rest-of-list (enumerate-unif-subsets-real old-nonmatches new-nonmatches))
				    (nm2 (cons (cons (cdar nm) (caar nm)) (cons (cddr nm) (cadr nm)))))
				;nm2 is a reversed version of nm.
				(setq subset-arrangements-list
				      (append
				       (mapcar #'(lambda (x) (cons (cons om nm) x)) rest-of-list)
				       (mapcar #'(lambda (x) (cons (cons om nm2) x)) rest-of-list))))))
	    (dolist (om old-matches)
		    (dolist (nm new-matches)
			    (setq subset-arrangements-list 
				  (mapcar #'(lambda (x) (cons (cons om nm) x)) 
					  (enumerate-unif-subsets-real old-nonmatches new-nonmatches))))))
	  subset-arrangements-list)))))

(defun princ-unif-node-dpairs (node)
  (msg (node-print-name node) (node-dpairs node) t)
  (mapcar #'princ-unif-node-dpairs (node-sons node))
  nil)

(defun princ-unif-node-names (node)
  (msg (node-print-name node) t)
  (mapcar #'princ-unif-node-names (node-sons node))
  nil)

(defun fill-sub-check-slot (node)
  (let ((slot-val nil))
    (dolist (dpair (node-dpairs node))
	    (let* ((wff1 (lambda-reduce (car dpair)))
		   (no1 (unif-hash-fn wff1 2))
		   (wff2 (lambda-reduce (cdr dpair)))
		   (no2 (unif-hash-fn wff2 2)))
	      (if (< no1 no2)
		  (push (cons (cons no1 no2) (cons wff1 wff2)) slot-val)
		(push (cons (cons no2 no1) (cons wff2 wff1)) slot-val))))
    (setf (node-sub-check node) (sort slot-val #'unif-lex-hash)))
  (node-sub-check node))
		  
(defun unif-lex-hash (a b)
  (or (< (caar a) (caar b))
      (and (= (caar a) (caar b))
	   (< (cdar a) (cdar b)))))

(defun unif-hash-fn (gwff n)
  (cond ((abbrev-p gwff)
	 (unif-hash-fn (get gwff 'defn) (+ 2 (mod (* 19 n) 65536))))
	((boundwff-q gwff) (unif-hash-fn (cdr gwff) (+ 2 (mod (* (const-val (cdar gwff)) n) 65536))))
	((infix-p gwff) (unif-hash-fn (cdr gwff) (unif-hash-fn (cdar gwff) 
							       (+ 2 (mod (* (const-val (caar gwff)) n)
									 65536)))))
	(t (if (consp gwff) 
	       (unif-hash-fn (cdr gwff) (unif-hash-fn (car gwff) (+ 2 (mod (* n n) 65536))))
	     n))))
; all those +2's are there so that we never end up with a 0 or 1...

(defun const-val (sym)
  (case sym 
	(and 3)
	(or 5)
	(not 7)
	(exists 11)
	(forall 13)
	(t 17)))
