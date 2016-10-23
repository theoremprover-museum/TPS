;;; -*- Mode: Lisp -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of UNIFICATION-interface)
(context unification)
;;;
;;; File: UNIF-user
;;; Package: UNIFICATION-interface
;;;
;;; Contents defines interface functions and commands for higher-order
;;; unification
;;;

(deffile unif-user
  (part-of unification-interface)
  (extension lisp)
  (mhelp "Contents define unification top-level."))

(context mating-search)

(defmateop unify
  (mate-result-> ignore)
  (mate-alias call-unify))

(defwffop call-unify  
  (applicable-q
    (lambda () (and (not first-order-mode-ms) active-mating)))
  (resulttype ignore)
  (mhelp "Call unification in interactive mode for active mating. The unification
tree associated with the active-mating is passed on to the unification
top-level. Any changes made to this tree are destructive. Applicable only for
a higher-order unification problem. Uses MS88-style unification."))

(defun call-unify ()
  (unless (node-p (mating-utree active-mating))
	  (multiple-value-bind
	   (root-node subst-hash-table)
	   (initialize-utree
	    (mating-utree active-mating)
	    (free-vars-in-etree current-eproof))
	   (setf (mating-utree active-mating) root-node)
	   (setf (mating-subst-hash-table active-mating)
		 subst-hash-table)))
  (let ((root-node (mating-utree active-mating))
	(subst-hash-table (mating-subst-hash-table active-mating))
	(displaywff t)
	(printdepth printdepth)
	(ppwfflag ppwfflag)
	(nodestack nil)
	(prev-node nil)
	(current-topnode nil)
	(printtypes printtypes)
	)
    (declare (special displaywff printdepth ppwfflag  root-node nodestack
		      printtypes current-topnode subst-hash-table prev-node))
    (%catch% (progn (setq current-topnode root-node prev-node root-node)
		    (uniftop)
		    (throwfail "Unification aborted."))
	     (exit-inferior-top))))

(context subtoplevels)

(defmexpr unify
  (mainfns enter-uniftop)
  (mhelp "Enter the unification top-level. The user can define disagreement sets
using the command ADD-DPAIR available in the unification top-level. If you
are entering from the MATE top level, the unification tree associated with 
the active-mating is passed on to the unification top-level.
Any changes made to this tree are destructive. Applicable only for
a higher-order unification problem. Uses MS88-style unification."))

(defun enter-uniftop ()
  (let ((displaywff t)
	(printdepth printdepth)
	(ppwfflag ppwfflag)
	(nodestack nil)
	(prev-node nil)
	(current-topnode nil)
	(printtypes printtypes)
	root-node subst-hash-table)
    (declare (special displaywff printdepth ppwfflag  root-node nodestack
		      printtypes current-topnode subst-hash-table prev-node))
    (%catch% (progn (setq current-topnode nil prev-node nil)
		    (uniftop)
		    (throwfail "Unification aborted."))
	     (exit-inferior-top))))

(context unification)

(defunifop ^
  (unif-applicablep (lambda () (and current-topnode
				    (node-parent current-topnode))))
  (unif-mainfns goto-parentnode)
  (move-command t)
  (mhelp "Go to the parent node of the current-topnode.
(i.e. move up one level in the tree)."))

(defun goto-parentnode ()
  (declare (special nodestack))
  (push current-topnode nodestack)
  (setq current-topnode (node-parent current-topnode)))

(defun goto-rootnode ()
  (declare (special nodestack))
  (push current-topnode nodestack)
  (setq current-topnode root-node))

(defunifop ^^
  (unif-applicablep (lambda () current-topnode))
  (unif-mainfns goto-rootnode)
  (move-command t)
  (mhelp "Go to the root node in the unification tree 
(the node with name \"0\")."))

(defunifop p
  (unif-argnames name)
  (unif-argtypes string)
  (unif-arghelp "Name of node in the unification tree")
  (unif-defaultfns (lambda (name)
		     (when current-topnode
		       (if (eq name '$)
			   (setq name (node-print-name current-topnode))))
		     (list name)))
  (unif-applicablep (lambda (name) (declare (ignore name)) current-topnode))
  (print-command t)
  (unif-mainfns show-unode)
  (mhelp "Displays the current unification node; show its name,
measure, number of descendants, substitutions added and free
variables. Does not display the disagreement pairs (use PP or
PP* for that), or the cumulative substitutions from this node 
to the root (use SUBST-STACK for that)."))

(defun show-unode (name)
  (let ((node (if (string= name (node-print-name current-topnode))
		  current-topnode (find-unification-node name nil))))
    (msg t "Name: " (node-print-name node))
    ;(let ((subst (car (node-subst-stack node))))
    (if (node-parent node)
	(dolist (subst (reverse (setdiff (node-subst-stack node) (node-subst-stack (node-parent node)))))
					;might be several subs at this node...
		(when subst
		      (msg t "Substitution:" 3 (find-type subst) " " ((car subst) . gwff) " . "
			   ((lambda-reduce-subst (cdr subst) nil) . gwff))))
      (dolist (subst (reverse (node-subst-stack node)))
	      (when subst
		    (msg t "Substitution:" 3 (find-type subst) " " ((car subst) . gwff) " . "
			 ((lambda-reduce-subst (cdr subst) nil) . gwff)))))
    (if (node-terminal-flag node) (msg " " (node-terminal-flag node))
	(progn (msg t "Measure: " (node-measure node))
	       (unless (node-sons node) (msg 3 "LEAF"))))
    (if (node-sons node)
	(msg t "Number of descendents: "  (length (node-sons node))))
    (when (node-connections node)
	(msg t "Connections added: ")
	(dolist (connection (node-connections node))
	  (print-connection connection (connections-array))))
    (when (node-free-vars node)
      (msg t "Free variables:")
      (dolist (var (node-free-vars node))
	(msg 2 (var . gwff))))
    (if (node-subsumed node)
	(msg t "Node subsumed " (node-subsumed node)))
    (multiple-value-bind (tv tv-p hv pt)
			 (check-simpl-count-2 (node-subst-stack node))
			 (declare (ignore hv))
			 (msgf "Subs so far: " (remove-if #'(lambda (x) (integerp (car x))) tv))
			 (msgf "Subs so far would require: M-S-VAR " (reduce 'max (mapcar 'cdr tv)) ", M-S-PROJ " (reduce 'max (mapcar 'cdr tv-p)) ", M-S-PROJ-TOTAL " pt "."t))
))

(defunifop pp
  (unif-applicablep (lambda () current-topnode))
  (unif-mainfns show-dpairs)
  (print-command t)
  (mhelp "Displays the disagreement pairs at the current node. See also PP*. 
More information about the current node is given by the command P."))

(defunifop pp*
  (unif-applicablep (lambda () current-topnode))
  (unif-mainfns show-dpairs-2)
  (print-command t)
  (mhelp "Displays the disagreement pairs at the current-topnode, including the order of
each pair and other information. See also PP. The other information displayed
includes (for each wff, each disagreement pair and the whole set of 
disagreement pairs):
1) the order (e.g. \"x(i)\" is first order, and so on).
2) whether it is monadic (all function constants are unary).
3) whether it is linear (all free vars occur once only).
4) whether it is a matching problem (one side of a dpair has no free vars).
5) whether it is a relaxed pattern (all free vars have only bound vars as
   arguments).
6) whether it is a pattern (all free vars have distinct bound vars as 
   arguments).
7) whether a disagreement pair is variable-disjoint (the free vars on the
   left are disjoint from those on the right).
8) whether the set of disagreement pairs can be partitioned into sets in 
   which each free var in the whole problem occurs in at most one set.
9) whether there are any free vars that occur only once, or not at all, in 
   the whole problem.
These conditions all appear in the literature on higher-order unification; 
see, for example, Prehofer's paper in CADE '94.

More information about the current node is given by the command P."))

(defun show-dpairs (&optional (current-topnode current-topnode)
			      (tab "") (file nil))
  (msgf "Properties: (" (show-dpairs-3 current-topnode) ")")
  (do ((dpairs (node-dpairs current-topnode) (cdr dpairs))
       (counter 1 (1+ counter)))
      ((null dpairs))
    (if file
	(msg "\\\\" t counter "&"  ((lambda-reduce (caar dpairs)) .  gwff)
	     " & " ((lambda-reduce (cdar dpairs)) .  gwff))
	(msg t tab counter 3 ((lambda-reduce (caar dpairs)) .  gwff) 2 "." 2
	     ((lambda-reduce (cdar dpairs)) .  gwff)))))

(defunifop eproof-utree
  (unif-mainfns eproof-utree)
  (mhelp "Create a new utree whose root has all the dpairs
associated with the current mating. (The existing utree may
have some of the dpairs added lower down the tree; this will
bring them all to the top). See also NAME-DPAIR.")) 

(defun eproof-utree ()
  (if (and current-eproof (eproof-mating-list current-eproof))
  (let ((free-vars (free-vars-in-etree current-eproof))
	(dpairs (clist-to-dpairs (mating-clist (car (eproof-mating-list current-eproof))))))
    (multiple-value-setq (root-node subst-hash-table)
			 (initialize-utree dpairs free-vars))
    (setq current-topnode root-node)
    (setq prev-node root-node))
  (throwfail "There is no mating attached to the current eproof.")))

(defunifop name-dpair
  (unif-argnames name)
  (unif-argtypes symbol)
  (unif-arghelp "Name of dpairset")
  (unif-applicablep (lambda (name) (declare (ignore name)) current-topnode))
  (unif-mainfns name-dpair)
  (mhelp "Give a name to the dpairset associated with the current topnode.
This is most useful when UNIFY has been issued from the MATE top
level, and you want to name the current dpair so that you can save
it in the library. See also EPROOF-UTREE."))

(defvar named-fv-list nil)

(defun name-dpair (name)
  (if (memq name dpairlist)
      (complain "A dpairset by this name already exists.")
    (progn (push name dpairlist)
	   (set name nil)
	   (dolist (dpairs (node-dpairs current-topnode))
		   (set name (acons (lambda-reduce (car dpairs)) (lambda-reduce (cdr dpairs)) (eval name))))
	   (msg "Done. Free variables should be set to: " (node-free-vars current-topnode) ".")
	   (setq named-fv-list (acons name (node-free-vars current-topnode) named-fv-list)))))

(defunifop stats
  (print-command t)
  (unif-mainfns unif-stats)
  (mhelp "Statistics about the current unification tree."))

(defvar *unif-stats-store* nil)

(defun unif-stats ()
  (when current-topnode
	(goto-rootnode)
	(multiple-value-bind (branching leaves nodecount)
			     (stats-rec current-topnode nil nil 0)
			     (msg "Total number of nodes: " nodecount "." t)
			     (msg "Average branching at a non-leaf node: "
				  (format nil "~,F" (/ (reduce '+ branching) (length branching))) t)
			     (msg "Number of leaves: " (length leaves) " at average depth "
				  (format nil "~,F" (/ (reduce '+ (mapcar #'node-depth leaves)) (length leaves)))
				  ", max depth " (reduce 'max (mapcar #'node-depth leaves))
				  " and min depth " (reduce 'min (mapcar #'node-depth leaves)) "." t)
			     (let ((fails (mapcar #'node-depth 
						  (remove-if-not #'(lambda (x) (eq (node-terminal-flag x) 'fail)) leaves)))
				   (wins (mapcar #'node-depth
						 (remove-if-not #'(lambda (x) (and (node-terminal-flag x) 
										   (not (eq (node-terminal-flag x) 'fail))))
							leaves)))
				   (others (mapcar #'node-depth (remove-if #'node-terminal-flag leaves))))
			     (msg "Number of failure nodes: " (length fails))
			     (if (zerop (length fails)) (msg "." t) (progn
				     (if (= 1 (length fails))
					 (msg " at depth " (car fails) "." t)
				       (msg " at average depth "
					    (format nil "~,F" (/ (reduce '+ fails) (length fails)))
					    ", max depth " (reduce 'max fails) " and min depth "
					    (reduce 'min fails) "." t))))
			     (msg "Number of success nodes: " (length wins))
			     (if (zerop (length wins)) (msg "." t) (progn
				     (if (= 1 (length wins))
					 (msg " at depth " (car wins) "." t)
				       (msg " at average depth "
					    (format nil "~,F" (/ (reduce '+ wins) (length wins)))
					    ", max depth " (reduce 'max wins) " and min depth "
					    (reduce 'min wins) "." t))))
			     (msg "Number of other leaves: " (length others))
			     (if (zerop (length others)) (msg "." t) (progn
				     (if (= 1 (length others))
					 (msg " at depth " (car others) "." t)
				       (msg " at average depth "
					    (format nil "~,F" (/ (reduce '+ others) (length others)))
					    ", max depth " (reduce 'max others) " and min depth "
					    (reduce 'min others) "." t))))))))

(defun unif-stats2 ()
  (when current-topnode
	(goto-rootnode)
	(let ((usl nil))
	(multiple-value-bind (branching leaves nodecount)
			     (stats-rec current-topnode nil nil 0)
			     (push nodecount usl)
			     (push (/ (reduce '+ branching) (length branching)) usl)
			     (push (length leaves) usl)
			     (push (/ (reduce '+ (mapcar #'node-depth leaves)) (length leaves)) usl)
			     (let ((fails (remove-if-not #'(lambda (x) (eq (node-terminal-flag x) 'fail)) leaves))
				   (wins (remove-if-not #'(lambda (x) (and (node-terminal-flag x) 
									   (not (eq (node-terminal-flag x) 'fail))))
							leaves))
				   (others (remove-if #'node-terminal-flag leaves)))
			     (push (length fails) usl)
			     (push (length wins) usl)
			     (push (length others) usl)
			     (push (reverse usl) *unif-stats-store*))))))

(defun print-unif-stats ()
  (let* ((nodes (mapcar #'car *unif-stats-store*))
	 (branching (mapcar #'cadr *unif-stats-store*))
	 (leaves (mapcar #'caddr *unif-stats-store*))
	 (dleaves (mapcar #'cadddr *unif-stats-store*))
	 (uss (mapcar #'cddddr *unif-stats-store*))
	 (fails (mapcar #'car uss))
	 (wins (mapcar #'cadr uss))
	 (others (mapcar #'caddr uss))
	 (c (length nodes)))
    (msgf t "Total number of trees considered: " c ", totalling " (reduce '+ nodes) " nodes." t)
    (msg "Average number of nodes per tree: " (format nil "~,F" (/ (reduce '+ nodes) c)) t)
    (msg "Average branching at a non-leaf node: " (format nil "~,F" (/ (reduce '+ branching) c)) t)
    (msg "Average number of leaves: " (format nil "~,F" (/ (reduce '+ leaves) c)) t)
    (msg "Average depth of leaves: " (format nil "~,F" (/ (reduce '+ dleaves) c)) t)
    (msg "Average number of failure nodes: " (format nil "~,F" (/ (reduce '+ fails) c)) t)
    (msg "Average number of success nodes: " (format nil "~,F" (/ (reduce '+ wins) c)) t)
    (msg "Average number of other leaf nodes: " (format nil "~,F" (/ (reduce '+ others) c)) t)
    (setq *unif-stats-store* nil)))

(defun stats-rec (node branching leaves nodecount)
  (if (node-sons node)
      (progn (setq branching (cons (length (node-sons node)) branching))
	     (incf nodecount) 
	     (dolist (son (node-sons node))
		     (multiple-value-bind (b l n) 
					  (stats-rec son branching leaves nodecount)
					  (setq branching b leaves l nodecount n)))
	     (values branching leaves nodecount))
    (values branching (cons node leaves) (1+ nodecount))))

(defunifop utree
  (unif-argnames name filename verbose)
  (unif-argtypes string filespec boolean)
  (print-command t)
  (unif-arghelp "Name of node in the unification tree" "Filename" "Extra output?")
  (unif-defaultfns (lambda (name filename verbose)
		     (when current-topnode
		       (if (eq name '$)
			   (setq name (node-print-name current-topnode)))
		       (if (eq filename '$)
			   (setq filename "TTY:"))
		       (if (eq verbose '$) (setq verbose t)))
		     (list name filename verbose)))
  (unif-applicablep (lambda (name filename verbose) (declare (ignore name filename verbose))
		      current-topnode))
  (unif-mainfns print-utree)
  (mhelp "Displays the unification tree and the associated substitutions at
each node which is below the specified node. Display is in a flat
format; UTREE* prints the same information in a tree format."))

(defunifop utree*
  (unif-argnames name print-subs)
  (unif-argtypes string boolean)
  (print-command t)
  (unif-arghelp "Name of node in the unification tree" "Print substitution at node?")
  (unif-defaultfns (lambda (name print-subs)
		     (progn
		     (when current-topnode
		       (if (eq name '$)
			   (setq name (node-print-name current-topnode))))
		     (if (eq print-subs '$) (setq print-subs nil))
		     (list name print-subs))))
  (unif-applicablep (lambda (name print-subs) (declare (ignore name print-subs))
		      current-topnode))
  (unif-mainfns tp-utree)
  (mhelp "Displays the unification tree and the associated substitutions at
each node which is below the specified node. Display is in a tree
format; UTREE prints the same information in a flat format. Display 
shows nodes as numbers, followed by I for imitation, P for projection,
~ for negation, A for administrative (e.g. anything generated by SIMPL).
Optionally shows the most recent substitution on the subst-stack at 
each node."))

(defun print-utree (name filename verbose)
  (let ((utree (find-unification-node name nil)))
    (when utree
      (if (terminalp filename) (print-utree-rec utree verbose)
	  (print-utree-tex utree filename verbose)))))

(defun print-utree-rec (utree verbose)
  (let ((subst (car (node-subst-stack utree))))
    (msg t (node-print-name utree))
    (if verbose (msg "  (" (show-dpairs-3 utree) ")"))
    (when subst
      (msg 3 (find-type subst) " " ((car subst) . gwff) " . "
	   ((lambda-reduce-subst (cdr subst) nil) . gwff))))
  (if (node-sons utree) (mapc #'(lambda (x) (print-utree-rec x verbose)) (node-sons utree))
      (if (and (node-terminal-flag utree) (not verbose)) (msg 3 (node-terminal-flag utree)))))

(defun print-utree-tex (utree filename verbose)
  (let ((style 'tex)
	(in-tex-math-mode t)
	(num-of-lines 1)
	(printtypes nil))
    (declare (special num-of-lines))
    (with-open-file (*standard-output*
		      (merge-pathnames filename "utree.tex")
		      :direction :output
		      :if-does-not-exist :create
		      :if-exists :append)
      (if verbose
	  (msg t "$\\begin{array}{llll}"
	       t "Node Name & Description & Variable & Substitution")
	(msg t "$\\begin{array}{llll}" t
	     "Node Name & Variable & Substitution & Terminal"))
      (print-utree-tex-rec utree verbose)
      (msg t "\\end{array}$" -2))))

(defun print-utree-tex-rec (utree verbose)
  (declare (special num-of-lines))
  (let ((subst (car (node-subst-stack utree))))
    (incf num-of-lines)
    (when (> num-of-lines pagelength)
      (setq num-of-lines 1)
      (if verbose
	  (msg t "\\end{array}$" -2 "$\\begin{array}{llll}"
	       t "Node Name & Description & Variable & Substitution")
	  (msg t "\\end{array}$" -2 "$\\begin{array}{llll}"
	   t "Node Name & Variable & Substitution & Terminal")))
    (msg "\\\\" t (node-print-name utree))
    (if verbose (msg "&  (" (show-dpairs-3 utree) ")"))
    (when subst
      (msg "&" (find-type subst) " " ((car subst) . gwff) " & "
	   ((lambda-reduce-subst (cdr subst) nil) . gwff))))
  (if (node-sons utree)
      (mapc #'(lambda (x) (print-utree-tex-rec x verbose)) (node-sons utree))
      (if (and (node-terminal-flag utree) (not verbose)) (msg " & " (node-terminal-flag utree)))))

(defunifop pall
  (unif-argnames name filename verbose)
  (unif-argtypes string filespec boolean)
  (unif-arghelp "Name of node in the unification tree" "Filename" "Extra details?")
  (unif-defaultfns (lambda (name filename verbose)
		     (when current-topnode
		       (if (eq name '$)
			   (setq name (node-print-name current-topnode)))
		       (if (eq filename '$)
			   (setq filename "TTY:"))
		       (if (eq verbose '$)
			   (setq verbose t)))
		     (list name filename verbose)))
  (unif-applicablep (lambda (name filename verbose) (declare (ignore name filename verbose))
		      current-topnode))
  (unif-mainfns print-dpairs)
  (print-command t)
  (mhelp "Displays all the disagreement pairs at every node below the
given node. (Similar to PP, but for the entire tree below the current
node.)"))

(defun print-dpairs (name filename verbose)
  (let ((utree (find-unification-node name nil)))
    (when utree
      (if (terminalp filename) (print-dpairs-rec utree verbose)
	  (print-dpairs-tex utree filename verbose)))))

(defun print-dpairs-rec (utree verbose)
  (msg t t (node-print-name utree))
  (if (and (node-terminal-flag utree) (not (node-sons utree))) 
      (msg 3 (node-terminal-flag utree))
      (if (node-sons utree) 
	  (if verbose (msg 3 (show-dpairs-3 utree))) (msg 3 "LEAF")))
  (when verbose (msgf "  Substs: ")
	(if (node-parent utree)
	    (dolist (subst (reverse (setdiff (node-subst-stack utree) (node-subst-stack (node-parent utree)))))
		    (when subst
			  (msg (find-type subst) " " ((car subst) . gwff) " . "
			       ((lambda-reduce-subst (cdr subst) nil) . gwff) " ")))
	  (dolist (subst (reverse (node-subst-stack utree)))
		  (when subst
			(msg (find-type subst) " " ((car subst) . gwff) " . "
			     ((lambda-reduce-subst (cdr subst) nil) . gwff) " ")))))
  (show-dpairs utree "    ")
  (when (node-sons utree) (mapc #'(lambda (x) (print-dpairs-rec x verbose)) (node-sons utree))))

(defun print-dpairs-tex (utree filename verbose)
  (let ((style 'tex)
	(in-tex-math-mode t)
	(num-of-lines 2)
	(printtypes nil))
    (declare (special num-of-lines))
    (with-open-file (*standard-output* (merge-pathnames filename "utree.tex")
				       :direction :output
				       :if-does-not-exist :create
				       :if-exists :append)
      (if verbose
	  (msg t "$\\begin{array}{lll}" t "Node Name & Description \\\\" t
	       "Number & First Element & Second Element")
	  (msg t "$\\begin{array}{lll}" t "Node Name & Terminal Flag \\\\" t
	   "Number & First Element & Second Element"))
      (print-dpairs-tex-rec utree verbose)
      (msg t "\\end{array}$" -2))))

(defun print-dpairs-tex-rec (utree verbose)
  (declare (special num-of-lines))
  (incf num-of-lines)
  (when (> num-of-lines pagelength)
    (setq num-of-lines 1)
    (msg t "\\end{array}$" -2 "$\\begin{array}{lll}" t
	 "Node Name & First Element & Second Element"))
  (msg "\\\\" t (node-print-name utree))
  (if (and (node-terminal-flag utree) (not (node-sons utree))) 
      (msg "&" (node-terminal-flag utree))
      (if (node-sons utree) 
	  (if verbose (msg "&" (show-dpairs-3 utree))) (msg "& LEAF")))
;  (if (node-terminal-flag utree) (msg  "&" (node-terminal-flag utree))
;      (if (node-sons utree) nil (msg  "&")))
  (incf num-of-lines (length (node-dpairs utree)))
  (show-dpairs utree "    " t)
  (when (node-sons utree) (mapc #'(lambda (x) (print-dpairs-tex-rec x verbose)) (node-sons utree))))

(defunifop goto
  (unif-argnames name)
  (unif-argtypes string)
  (unif-arghelp "Name of node in the unification tree.")
  (unif-applicablep (lambda (name) (declare (ignore name)) current-topnode))
  (unif-mainfns find-unification-node)
  (mhelp "Go to the specified node in the unification tree."))

(defun find-unification-node (name &optional (goto t))
  (declare (special nodestack))
  (let ((utree (find-unification-node-rec name current-topnode)))
    (unless utree
      (setq utree (find-unification-node-rec name root-node)))
    (when (and goto utree)
      (push current-topnode nodestack)
      (setq current-topnode utree))
    (unless utree
      (complain name " ----- no such node in the unification tree."))
    utree))

(defun find-unification-node-rec (name utree)
  (if (string= name (node-print-name utree)) utree
      (dolist (utree (node-sons utree) nil)
	(let ((found (find-unification-node-rec name utree)))
	  (if found (return found))))))

(defunifop nth-son
  (unif-argnames n)
  (unif-argtypes integer+)
  (unif-arghelp "Descendant")
  (move-command t)
  (unif-applicablep (lambda (n) (declare (ignore n)) current-topnode))
  (mhelp "Go to the nth descendant of the current-topnode. Instead of
using this command, you can simply type n on the unification top level to
go to the nth descendant. It has no effect if the current-topnode has
no descendents."))

(defun nth-son (n)
  (declare (special nodestack))
  (when (and current-topnode
	     (node-sons current-topnode)
	     (not (> n (length (node-sons current-topnode)))))
    (if (cdr (node-sons current-topnode))
	(let ((string2 (format nil "~A-~A"
			       (node-print-name current-topnode) n)))
	  (dolist (son (node-sons current-topnode))
	    (when (string= (node-print-name son) string2)
	      (push current-topnode nodestack)
	      (setq current-topnode son))))
	(when (< n 2)
	  (push current-topnode nodestack)
	  (setq current-topnode (car (node-sons current-topnode))))))
  (when current-topnode (node-print-name current-topnode)))

(defunifop simplify
  (unif-applicablep (lambda () (and current-topnode
				    (not (node-terminal-flag current-topnode))
				    (not (node-sons current-topnode)))))
  (unif-mainfns uniftop-simpl)
  (mhelp "A call to TPS's version of Huet's SIMPL algorithm. Dpairs in the 
current topnode are replaced by the dpairs returned by the call. It will 
also find substitutions of the form (var . term) provided `var' does not
occur in `term'. This command will alter the unification tree."))

(defun uniftop-simpl ()
  (multiple-value-bind (flag subst-stack dpairs)
      (simpl (node-dpairs current-topnode) (node-subst-stack current-topnode)
	     (node-free-vars current-topnode) 0)
    (setf (node-dpairs current-topnode) dpairs)
    (setf (node-subst-stack current-topnode) subst-stack)
    (unless (eq flag 'more)
      (setf (node-terminal-flag current-topnode) flag)
      (setf (node-measure current-topnode) TN-measure)
      (funcall update-measure current-topnode)
      (when (eq flag 'success)
	(setf (node-subst-stack current-topnode)
	      (nconc (find-subs-flexible-pairs dpairs subst-stack)
		     (node-subst-stack current-topnode)))))
    (msg flag)))

(defunifop subst-stack
  (unif-argnames filename)
  (unif-argtypes filespec)
  (unif-arghelp "Filename")
  (unif-defaultfns (lambda (filename)
		     (if (and current-topnode (eq filename '$))
			 (list "TTY:") (list filename))))
  (unif-applicablep (lambda (filename) (declare (ignore filename))
		      current-topnode))
  (unif-mainfns display-subst-stack)
  (print-command t)
  (mhelp "Displays the substitution stack for the current topnode.
See also P, PP, PP* for other information about the current node."))

(defun display-subst-stack (filename)
  (if (terminalp filename)
      (dolist (subst (node-subst-stack current-topnode))
	(msg t (find-type subst) " " ((car subst) . gwff) 3)
	(if (subst-p (cdr subst))
	    (msg ((lambda-reduce-subst (cdr subst) nil) . gwff))
	    (msg ((cdr subst) . gwff))))
      (let ((style 'tex)
	    (in-tex-math-mode t)
	    (num-of-lines 1)
	    (printtypes nil))
	(with-open-file (*standard-output* (merge-pathnames filename
							    "utree.tex")
					   :direction :output
					   :if-does-not-exist :create
					   :if-exists :append)
	  (msg "Substitution Stack for node: "
	       (node-print-name current-topnode)
	       -2 "$\\begin{array}{ll}" t "Variable & Substitution")
	  (dolist (subst (node-subst-stack current-topnode))
	    (incf num-of-lines)
	    (when (> num-of-lines pagelength)
	      (setq num-of-lines 1)
	      (msg t "\\end{array}$" t "$\\begin{array}{ll}" t
		   "Variable & Substitution"))
	    (msg "\\\\" t ((car subst) . gwff)" & ")
	    (if (subst-p (cdr subst))
		(msg ((lambda-reduce-subst (cdr subst) nil) . gwff))
		(msg ((cdr subst) . gwff))))
	  (msg t "\\end{array}$" -2)))))

(defunifop apply-subst
  (unif-argnames var term)
  (unif-argtypes gvar gwff)
  (unif-arghelp "Variable" "Term to substitute")
  (unif-applicablep (lambda (var term) (declare (ignore var term))
		      current-topnode))
  (unif-mainfns ck-and-apply-subst)
  (mhelp "Apply a substitution, suggested by the user, to the current topnode.
Modifies the unification tree."))

(defun ck-and-apply-subst (var term)
  (if (not (free-in-var-term var term nil nil (node-subst-stack current-topnode)
			nil nil))
      (push (cons var term) (node-subst-stack current-topnode))
      (complain f "Can't apply suggested substitution because of "
		"occurs check.")))

(defunifop \0
  (unif-applicablep (lambda ()
		      (declare (special nodestack))
		      (and current-topnode nodestack)))
  (unif-mainfns pop-unifnodestack)
  (move-command t)
  (mhelp "Replace the current topnode with the node on top of the nodestack.
Generally, typing an integer n will go to the nth son of the current
node. Compare the command NTH-SON."))

(defun pop-unifnodestack ()
  (declare (special nodestack))
  (setq current-topnode (pop nodestack)))

(defunifop go
  (unif-applicablep (lambda () current-topnode))
  (unif-mainfns go-unify)
  (mhelp "Call unification in automatic mode. Will search for unifiers
only below the current-topnode."))

(defun go-unify ()
  (timing-sethash 'unification 'sofar nil)
  (timing-sethash 'diy 'proof-count -999)
  (runcount 'unification)
  (let ((uflag (unify current-topnode subst-hash-table)))
    (breakcount 'unification)
    (display-time 'unification)
    (msg t uflag)))

(defunifop match
  (unif-applicablep (lambda ()
		      (and current-topnode
			   (not (node-terminal-flag current-topnode))
			   (not (node-sons current-topnode)))))
  (unif-mainfns uniftop-match)
  (mhelp "This command is applicable only if current-topnode is a non-terminal
leaf node. Calls TPS's version of Huet's MATCH algorithm to find substitutions
at the current topnode. The pair selected by MATCH is determined by the value 
of the flag APPLY-MATCH."))

(defun uniftop-match ()
  (let ((ck-subsumed nil))
    (declare (special ck-subsumed))
    (multiple-value-bind (var substitutions flag)
	(funcall apply-match current-topnode subst-hash-table
		 (node-depth current-topnode) nil)
      ;;FLAG is T if RHEAD is NOT.
      (subsumption-check root-node current-topnode ck-subsumed subst-hash-table)
      (uniftop-match-descendents var substitutions flag current-topnode))))

(defun uniftop-match-descendents (var substitutions not-flag node)
  (let* ((subst-stack (node-subst-stack node))
	 (sons (mapcar #'(lambda (substitution)
			   (create-successor
			     (acons var substitution subst-stack)
			     not-flag node))
		       substitutions)))
    (setq sons (delete-if #'null sons))
    (if sons
	(progn
	  (assign-print-name sons node)
	  (setf (node-sons node) sons)
	  (funcall assign-measure sons
		   (node-measure node) (length sons)
		   (node-depth node))
	  (msg (length sons)))
	(progn
	  (setf (node-terminal-flag node) 'fail)
	  (setf (node-measure node) TN-measure)
	  (msg "FAIL")))
    (funcall update-measure node)))

(defunifop match-pair
  (unif-argnames n)
  (unif-argtypes posinteger)
  (unif-arghelp "A reference to the dpair")
  (unif-applicablep (lambda (n)  (declare (ignore n)) current-topnode))
  (unif-mainfns uniftop-match-pair)
  (mhelp "This command is applicable only if current-topnode is a non-terminal
leaf node. Calls TPS's version of Huet's MATCH algorithm to find 
substitutions at the current topnode. n refers to the nth dpair, 
and this must be a flexible-rigid dpair."))

(defun uniftop-match-pair (n)
  (unless (or (node-terminal-flag current-topnode)
	      (node-sons current-topnode))
    (let ((dpair (nth (1- n) (node-dpairs current-topnode)))
	  (name (node-print-name current-topnode))
	  (free-vars (node-free-vars current-topnode))
	  (ck-subsumed nil))
      (declare (special ck-subsumed)) 
      (multiple-value-bind (frflag fhead p1 n1 rhead p2 rigid-term-binder
				   bdvars bdargs)
	  (frpair-p (car dpair) (cdr dpair) free-vars)
	(when frflag
	  (multiple-value-bind (substitutions hash-entry)
	      (match-top fhead (type fhead) p1 n1 rhead p2 (length rigid-term-binder)
			 rigid-term-binder bdvars subst-hash-table name bdargs)
	    (subsumption-check root-node current-topnode ck-subsumed subst-hash-table)
	    (when substitutions
	      (if (caar hash-entry) (push name (car hash-entry))
		  (rplaca (car hash-entry) name))
	      (uniftop-match-descendents fhead substitutions (eq rhead 'not)
					 current-topnode))))))))

(context unification-dpairs)

;;;input dpairs

(defunifop add-dpair
  (unif-argnames name elt1 elt2)
  (unif-argtypes symbol gwff gwff)
  (unif-arghelp "Name of set containing dpair" "First element"
		"Second Element")
  (unif-defaultfns (lambda (name elt1 elt2)
		     (if (and dpairlist (eq name '$))
			 (setq name (car dpairlist)))
		     (list name elt1 elt2)))
  (unif-applicablep (lambda (name elt1 elt2)
		      (declare (ignore name))
		      (if (and (equal (type elt1) (type elt2))) t
			  (progn (complain "The two elements do not "
					   "have the same type.") nil))))
  (mhelp "If the disagreement set already exists, insert a disagreement pair at
the front. Else create a new disagreement set consisting of this dpair only."))

(defun add-dpair (name elt1 elt2)
  (unless (memq name dpairlist)
    (push name dpairlist)
    (set name nil))
  (set name (acons elt1 elt2 (eval name))))

(defunifop rm-dpair
  (unif-argnames name elt1 elt2)
  (unif-argtypes symbol gwff gwff)
  (unif-arghelp "Name of set containing dpair" "First element"
		"Second Element")
  (unif-defaultfns (lambda (name elt1 elt2)
		     (if (eq name '$)
			 (setq name (or (car dpairlist) '$))
			 (if (eq elt1 '$)
			     (setq elt1 (caar (eval name)))
			     (if (eq elt2 '$)
				 (setq elt2 (cdr (assoc elt1 (eval name)
							:test #'equal))))))
		     (list name elt1 elt2)))
  (mhelp "Remove a disagreement pair from a disagreement set."))

(defun rm-dpair (name elt1 elt2)
  (set name (delete (cons elt1 elt2) (eval name) :test #'equal)))

(defunifop show-dpairset
  (unif-argnames name)
  (unif-argtypes symbol)
  (unif-arghelp "Name of disagreement set.")
  (unif-defaultfns (lambda (name) (if (and dpairlist (eq name '$))
				      (list (car dpairlist)) (list name))))
  (unif-applicablep (lambda (name) (boundp name)))
  (print-command t)
  (mhelp "Show a disagreement set."))

(defun show-dpairset (name)
  (dolist (dpair (eval name))
    (msg t ((car dpair) . gwff) 2 "." 2 ((cdr dpair) . gwff))))

(defunifop unif-problem
  (unif-argnames name free-vars)
  (unif-argtypes symbol gvarlist)
  (unif-arghelp "Name of disagreement set" "List of free variables.")
  (unif-defaultfns (lambda (name free-vars)
		     (declare (special show-all-libobjects))
		     (if (and dpairlist (eq name '$))
			 (setq name (car dpairlist)))
		     (unless (or (eq name '$) (memq name dpairlist))
			     (let (again) 
			       (prompt-read again nil (msgf name " is not a dpairset. Search for it in library?") 
					  'yesno 'yes nil))
			     (if (core::locate-item name :type 'dpairset)
				 (progn 
				   (core::retrieve-libobject name :type 'dpairset :multiple show-all-libobjects)
				   (msgf "Found a dpairset" t))
			       (msgf "Didn't find a dpairset" t)))
		     (if (and (memq name dpairlist) (eq free-vars '$))
			 (setq free-vars (if (assoc name named-fv-list) (cdr (assoc name named-fv-list)) 
					   (parse-vars (eval name)))))
		     (list name free-vars)))
  (unif-applicablep (lambda (name free-vars)
		      (declare (ignore free-vars))
		      (dolist (dpair (eval name) t)
			(unless (and (gwff-p (car dpair))
				     (gwff-p (cdr dpair)))
			  (complain dpair " is not a dpair.")
			  (return nil)))))
  (mhelp "Set up a new unification problem. `Name', the first argument to this 
command must already represent a disagreement set. Use the command ADD-DPAIR
to create this set. This is in some ways the inverse of the NAME-DPAIR 
command."))

(defun parse-vars (name)
  (let ((fv nil))
    (dolist (dpair name)
	    (setq fv (union (union fv (free-vars-of (car dpair))) (free-vars-of (cdr dpair)))))
    (setq fv (remove-if #'(lambda (x) (string= (string-upcase (princ-to-string x))
					       (princ-to-string x))) fv))
    fv))

(defun unif-problem (name free-vars)
  (multiple-value-setq (root-node subst-hash-table)
      (initialize-utree (eval name) free-vars))
  (setq current-topnode root-node)
  (setq prev-node root-node))

(defunifop add-dpairs-to-node
  (unif-argnames name free-vars)
  (unif-argtypes symbol gvarlist)
  (unif-arghelp "Name of disagreement set" "List of free variables" )
  (unif-applicablep (lambda (name free-vars)
		      (declare (ignore free-vars))
		      (and current-topnode
			   (dolist (dpair (eval name) t)
			     (unless (and (gwff-p (car dpair))
					  (gwff-p (cdr dpair))
					  (equal (type (car dpair))
						 (type (cdr dpair))))
			       (complain dpair " is not a dpair.")
			       (return nil)))
			   (not (eq (node-terminal-flag current-topnode)
				    'fail))
			   (not (node-sons current-topnode)))))
  (mhelp "Add new dpairs to the disagreement set at the CURRENT-TOPNODE. Applicable 
only if CURRENT-TOPNODE is a non failure leaf node. `Name', the first argument 
to this command must already represent a disagreement set. Use the command 
ADD-DPAIR,etc., to create this set."))
    

(defun add-dpairs-to-node (name free-vars)
  (add-dpairs-to-node-main name free-vars current-topnode)
  (when (node-parent current-topnode)
    (funcall update-measure-int-node (node-parent current-topnode))))

(defun add-dpairs-to-node-main (name free-vars node)
  (setf (node-old-dpairs node) (node-dpairs node))
  (setf (node-dpairs node) (append (eval name) (node-dpairs node)))
  (setf (node-free-vars node) (append free-vars (node-free-vars node)))
  (setf (node-terminal-flag node) nil)
  (let ((parent (node-parent node)))
    (if parent
	(funcall assign-measure (list node)
		 (node-measure parent) (length (node-sons parent))
		 (node-depth parent))
	(setf (node-measure node) initial-measure-root))))

(defunifop add-dpairs-to-utree
  (unif-argnames name free-vars)
  (unif-argtypes symbol gvarlist)
  (unif-arghelp "Name of disagreement set" "List of free variables" )
  (unif-applicablep (lambda (name free-vars)
		      (declare (ignore free-vars))
		      (and current-topnode
			   (dolist (dpair (eval name) t)
			     (unless (and (gwff-p (car dpair))
					  (gwff-p (cdr dpair))
					  (equal (type (car dpair))
						 (type (cdr dpair))))
			       (complain dpair " is not a dpair.")
			       (return nil))))))
  (mhelp "Add new dpairs at all non failure leaf nodes."))


(defun add-dpairs-to-utree (name free-vars)
  (add-dpairs-to-utree-rec name root-node free-vars 0))

(defun add-dpairs-to-utree-rec (name utree free-vars depth)
  (if (node-sons utree)
      (dolist (son (node-sons utree) (funcall update-measure-int-node utree))
	(add-dpairs-to-utree-rec name son free-vars (1+ depth)))
      (unless (eq (node-terminal-flag utree) 'fail)
	(add-dpairs-to-node-main name free-vars utree)))
  utree)


(defun show-dpairs-2 (&optional (current-topnode current-topnode)
			      (tab ""))
  (do ((dpairs (node-dpairs current-topnode) (cdr dpairs))
       (fv-list (node-free-vars current-topnode))
       (counter 1 (1+ counter))
       (order -999)
       (big-fvlist nil)
       (match t)
       (linear t)
       (vdj t)
       (monadic t)
       (pattern t)
       (regular t)
       (relaxed-pattern t)
       (final-list nil))
      ((null dpairs) (unless (= order -999)
			     (if (= order -1) (msgf "The whole dpair is polymorphic." t)
			       (progn (msgf t "The whole thing is " order (ordinal-print order) "order." t)
				      (if match (msgf "The whole thing is a matching problem."))
				      (if vdj (msgf "All pairs are variable-disjoint."))
				      (if monadic (msgf "The whole thing is monadic."))
				      (if linear (msgf "The whole thing is linear."))
				      (if regular (msgf "The whole thing is regular."))
				      (if pattern (msgf "The whole thing is a pattern.")
					(if relaxed-pattern (msgf "The whole thing is a relaxed pattern.")))
				      (isolation-check big-fvlist fv-list)
				      (msgf "Partitioning by free variables into independent sets : ")
				      (divide-and-conquer final-list)))))
      (when (eq counter 1) 
	    (msg t t "Free variables in this problem are : " ((car fv-list) . gwff))
	    (dolist (fv (cdr fv-list)) (msg ", " (fv . gwff) ))
	    (msg "." t))
      (msg t t tab counter 3 ((lambda-reduce (caar dpairs)) .  gwff) 2 "." 2
	   ((lambda-reduce (cdar dpairs)) .  gwff))
					;functions to look at the dpair:
      (let* ((dpair (cons (lambda-reduce (instantiate-all (lambda-reduce (caar dpairs)) nil)) 
			  (lambda-reduce (instantiate-all (lambda-reduce (cdar dpairs)) nil))))
	     (bd-vars-l (bd-vars-in-gwff (car dpair)))
	     (bd-vars-r (bd-vars-in-gwff (cdr dpair)))
	     (vars-l (union bd-vars-l (free-vars-of (car dpair))))
	     (vars-r (union bd-vars-r (free-vars-of (cdr dpair))))
	     (vars-in-dpair (union vars-l vars-r))
	     (left-o1 (max-order vars-l))
	     (right-o1 (max-order vars-r))
	     (left-o2 (max-order bd-vars-l))
	     (right-o2 (max-order bd-vars-r))
	     (left-rank (reduce #'max (mapcar #'(lambda (x) (find-rank (get x 'type))) vars-l)))
	     (right-rank (reduce #'max (mapcar #'(lambda (x) (find-rank (get x 'type))) vars-r)))
	     (left-reg (not (memq 'nil (mapcar #'(lambda (x) (find-reg (get x 'type))) vars-l))))
	     (right-reg (not (memq 'nil (mapcar #'(lambda (x) (find-reg (get x 'type))) vars-r))))
	     (mon-l (monadic-check (set-difference (set-difference vars-l fv-list) bd-vars-l)))
	     (mon-r (monadic-check (set-difference (set-difference vars-r fv-list) bd-vars-r)))
	     (lin-l (is-linear (car dpair) fv-list))
	     (lin-r (is-linear (cdr dpair) fv-list))
	     (relp-l (is-a-relaxed-pattern (car dpair) fv-list nil))
	     (relp-r (is-a-relaxed-pattern (cdr dpair) fv-list nil))
	     (patt-l (and relp-l (is-a-pattern (car dpair) fv-list nil)))
	     (patt-r (and relp-r (is-a-pattern (cdr dpair) fv-list nil)))
	     (order-l (if (= (min left-o1 left-o2) -1) -1
			    (if (= left-o2 0) left-o1
			      (if (= left-o1 left-o2)
				  (* 2 (1- left-o1)) (1- (* 2 (1- left-o1)))))))
	     (order-r (if (= (min right-o1 right-o2) -1) -1
			    (if (= right-o2 0) right-o1
			      (if (= right-o1 right-o2)
				  (* 2 (1- right-o1)) (1- (* 2 (1- right-o1)))))))
	     (order-dpair (max order-l order-r)))
	(msg t tab 5 "The left hand side is ")
	(if (= order-l -1) (msg "polymorphic.") (msg order-l (ordinal-print order-l) "order"))
	(if mon-l (msg " monadic")) (if lin-l (msg " linear"))
	(if patt-l (msg " pattern") (if relp-l (msg " relaxed pattern")))
	(if left-reg (msg " regular")) (msg " with rank " left-rank) 
	(msg ".")
	(msg t tab 5 "The right hand side is ")
	(if (= order-r -1) (msg "polymorphic.") (msg order-r (ordinal-print order-r) "order"))
	(if mon-r (msg " monadic")) (if lin-r (msg " linear"))
	(if patt-r (msg " pattern") (if relp-r (msg " relaxed pattern")))
	(if right-reg (msg " regular")) (msg " with rank " right-rank) 
	(msg ".")
	(msg t tab 5 "This dpair as a whole is ")
	(if (= order-dpair -1) (msg "polymorphic.") (msg order-dpair (ordinal-print order-dpair) "order."))
	(if (or (= order-dpair -1) (> order-dpair order)) (setq order order-dpair))
	(if (or (null (intersection vars-l fv-list)) (null (intersection vars-r fv-list)))
	    (msg t tab 5 "This dpair is a matching problem (i.e. one side has no free vars).") 
	  (progn (setq match nil)
		 (if (intersection (intersection fv-list vars-l) (intersection fv-list vars-r))
		     (setq vdj nil)
		   (msg t tab 5 "This dpair is variable-disjoint (i.e. no variable occurs on both sides)."))))
	(if (and mon-l mon-r)
	    (msg t tab 5 "This dpair is monadic (i.e. all function constants are unary).") (setq monadic nil))
	(if (and lin-l lin-r)
	    (msg t tab 5 "This dpair is linear (i.e. all free vars occur once only).") (setq linear nil))
	(if (and left-reg right-reg)
	    (msg t tab 5 "This dpair is regular (see e.g. Zaionc 1987).") (setq regular nil))
	(if (and relp-l relp-r)
	    (if (and patt-l patt-r)
		(msg t tab 5 "This dpair is a pattern. (free vars have distinct bound vars as args).")
	      (progn (msg t tab 5 "This dpair is a relaxed pattern. (free vars have bound vars as args).")
		     (setq pattern nil)))
	  (progn (setq relaxed-pattern nil) (setq pattern nil)))
	(setq final-list (acons counter (intersection vars-in-dpair fv-list) final-list))
	(setq big-fvlist (append (free-vars (cdr dpair) nil) (append (free-vars (car dpair) nil) big-fvlist)))
	)))

(defun show-dpairs-3 (&optional (current-topnode current-topnode))
  (do ((dpairs (node-dpairs current-topnode) (cdr dpairs))
       (fv-list (node-free-vars current-topnode))
       (counter 1 (1+ counter))
       (order -999)
       (big-fvlist nil)
       (match t)
       (linear t)
       (monadic t)
       (vdj t)
       (pattern t)
       (regular t)
       (relaxed-pattern t)
       (returning-string ""))
      ((null dpairs) (if (= order -999) (setq returning-string (princ-to-string (node-terminal-flag current-topnode)))
			     (if (= order -1) (setq returning-string "POLY")
			       (progn (setq returning-string (concatenate 'string (princ-to-string order) (ordinal-print order) returning-string))
				      (if match (setq returning-string (concatenate 'string "MATCH " returning-string)))
				      (if monadic (setq returning-string (concatenate 'string "MON " returning-string)))
				      (if vdj (setq returning-string (concatenate 'string "VDJ " returning-string)))
				      (if linear (setq returning-string (concatenate 'string "LIN " returning-string)))
				      (if regular (setq returning-string (concatenate 'string "REG " returning-string)))
				      (if pattern (setq returning-string (concatenate 'string "PAT " returning-string))
					(if relaxed-pattern (setq returning-string (concatenate 'string "REL-PAT " returning-string))))))) returning-string)
      (let* ((dpair (cons (lambda-reduce (instantiate-all (lambda-reduce (caar dpairs)) nil)) 
			  (lambda-reduce (instantiate-all (lambda-reduce (cdar dpairs)) nil))))
	     (bd-vars-l (bd-vars-in-gwff (car dpair)))
	     (bd-vars-r (bd-vars-in-gwff (cdr dpair)))
	     (vars-l (union bd-vars-l (free-vars-of (car dpair))))
	     (vars-r (union bd-vars-r (free-vars-of (cdr dpair))))
	     (left-o1 (max-order vars-l))
	     (right-o1 (max-order vars-r))
	     (left-o2 (max-order bd-vars-l))
	     (right-o2 (max-order bd-vars-r))
	     (left-reg (not (memq 'nil (mapcar #'(lambda (x) (find-reg (get x 'type))) vars-l))))
	     (right-reg (not (memq 'nil (mapcar #'(lambda (x) (find-reg (get x 'type))) vars-r))))
	     (mon-l (monadic-check (set-difference (set-difference vars-l fv-list) bd-vars-l)))
	     (mon-r (monadic-check (set-difference (set-difference vars-r fv-list) bd-vars-r)))
	     (lin-l (is-linear (car dpair) fv-list))
	     (lin-r (is-linear (cdr dpair) fv-list))
	     (relp-l (is-a-relaxed-pattern (car dpair) fv-list nil))
	     (relp-r (is-a-relaxed-pattern (cdr dpair) fv-list nil))
	     (patt-l (and relp-l (is-a-pattern (car dpair) fv-list nil)))
	     (patt-r (and relp-r (is-a-pattern (cdr dpair) fv-list nil)))
	     (order-l (if (= (min left-o1 left-o2) -1) -1
			    (if (= left-o2 0) left-o1
			      (if (= left-o1 left-o2)
				  (* 2 (1- left-o1)) (1- (* 2 (1- left-o1)))))))
	     (order-r (if (= (min right-o1 right-o2) -1) -1
			    (if (= right-o2 0) right-o1
			      (if (= right-o1 right-o2)
				  (* 2 (1- right-o1)) (1- (* 2 (1- right-o1)))))))
	     (order-dpair (max order-l order-r)))
	(if (or (= order-dpair -1) (> order-dpair order)) (setq order order-dpair))
	(if (not (or (null (intersection vars-l fv-list)) (null (intersection vars-r fv-list))))
	    (progn (setq match nil)
		   (if (intersection (intersection fv-list vars-l) (intersection fv-list vars-r))
		       (setq vdj nil))))
	(if (not (and mon-l mon-r))
	    (setq monadic nil))
	(if (not (and lin-l lin-r))
	    (setq linear nil))
	(if (not (and left-reg right-reg))
	    (setq regular nil))
	(if (and relp-l relp-r)
	    (if (and patt-l patt-r)
		nil
	      (setq pattern nil))
	  (progn (setq relaxed-pattern nil) (setq pattern nil)))
	(setq big-fvlist (append (free-vars (cdr dpair) nil) (append (free-vars (car dpair) nil) big-fvlist)))
	)))


(defun divide-and-conquer (alist)
  (let ((vars (cdar alist))
	(taken (list (car alist)))
	(left (cdr alist))
	(left1 nil) (taken1 nil) (vars1 nil))
    (multiple-value-setq (vars1 taken1 left1) (dandc vars taken left))
    (do ()
	((eq (length left1) (length left)))
	(unless (null vars1) (setq vars vars1) (setq taken taken1) (setq left left1))
	(multiple-value-setq (vars1 taken1 left1) (dandc vars taken left)))
    (msg (mapcar 'car taken1) " ")
    (if left1 (divide-and-conquer left1))))

(defun isolation-check (big-fvlist fv-list)
  (let ((never nil)
	(once nil))
    (dolist (fv fv-list)
	    (let ((occs (remove-if-not #'(lambda (x) (eq x fv)) big-fvlist)))
	      (if (= 0 (length occs)) (setq never (cons fv never))
		(if (= 1 (length occs)) (setq once (cons fv once))))))
    (if never (progn (msgf "These free variables never occur at all:")
		     (msg " " ((car never) . gwff))
		     (dolist (fv (cdr never)) (msg ", " (fv . gwff) ))
		     (msg "." t)))
    (if once (progn (msgf "These free variables are isolated (occur exactly once): ")
		    (msg ((car once) . gwff))
		    (dolist (fv (cdr once)) (msg ", " (fv . gwff) ))
		    (msg "." t)))
    (if (= (length fv-list) (+ (length never) (length once)))
	(msg t "The whole problem is isolated (every free var occurs at most once)."))))

(defun is-linear (gwff fv-list)
  (let* ((g-fv (free-vars gwff nil))
	 (g-fv (remove-if-not #'(lambda (x) (memq x fv-list)) g-fv))
	 (g-fv2 (remove-duplicates g-fv)))
    (= (length g-fv) (length g-fv2))))

(defun is-a-pattern (gwff fv-list bv-list) 
  (cond ((label-q gwff) (apply-label gwff (is-a-pattern gwff fv-list bv-list)))
	((abbrev-p gwff)
	 (is-a-pattern (get gwff 'defn) fv-list bv-list))
	((boundwff-q gwff) (is-a-pattern (cdr gwff) fv-list (cons (bdvar gwff) bv-list)))
	((infix-p gwff) (and (is-a-pattern (cdar gwff) fv-list bv-list) 
			     (is-a-pattern (cdr gwff) fv-list bv-list)))
	(t ;this is the important case!
	 (if (memq (head gwff) fv-list)
	     ;if it's [f ...] and f is free 
	     (if (consp gwff)
		 ;then if it's [f a b c]
		 (if (memq (lambda-reduce (cdr gwff)) bv-list)
		     ;and if c is bound, remove c from the bv-list
		     (is-a-pattern (car gwff) fv-list (remove-if #'(lambda (x) (eq x (lambda-reduce (cdr gwff)))) bv-list))
		   ;remains to check [f a b], otherwise fail
		   nil)
	       t)
	   (if (consp gwff)
	       ;if it's [f a b c] and f isn't free
	       (and (is-a-pattern (cdr gwff) fv-list bv-list)
		    ;check c is a pattern
		    (is-a-pattern (car gwff) fv-list bv-list))
	     ;check [f a b] is a pattern, otherwise it was just f, and we succeed.
	     t)))))

(defun is-a-relaxed-pattern (gwff fv-list bv-list) 
  (cond ((label-q gwff) (apply-label gwff (is-a-relaxed-pattern gwff fv-list bv-list)))
	((abbrev-p gwff)
	 (is-a-relaxed-pattern (get gwff 'defn) fv-list bv-list))
	((boundwff-q gwff) (is-a-relaxed-pattern (cdr gwff) fv-list (cons (bdvar gwff) bv-list)))
	((infix-p gwff) (and (is-a-relaxed-pattern (cdar gwff) fv-list bv-list) 
			     (is-a-relaxed-pattern (cdr gwff) fv-list bv-list)))
	(t ;this is the important case!
	 (if (memq (head gwff) fv-list)
	     ;if it's [f ...] and f is free 
	     (if (consp gwff)
		 ;then if it's [f a b c]
		 (if (memq (lambda-reduce (cdr gwff)) bv-list)
		     ;and if c is bound
		     (is-a-relaxed-pattern (car gwff) fv-list bv-list)
		   ;remains to check [f a b], otherwise fail
		   nil)
	       t)
	   (if (consp gwff)
	       ;if it's [f a b c] and f isn't free
	       (and (is-a-relaxed-pattern (cdr gwff) fv-list bv-list)
		    ;check c is a pattern
		    (is-a-relaxed-pattern (car gwff) fv-list bv-list))
	     ;check [f a b] is a pattern, otherwise it was just f, and we succeed.
	     t)))))
  
(defun dandc (vv tt ll)
  (let ((v1 vv)
	(t1 tt)
	(l1 nil))
    (dolist (l ll)
	    (if (intersection (cdr l) v1)
		(progn (setq v1 (union (cdr l) v1))
		       (setq t1 (cons l t1)))
		(setq l1 (cons l l1))))
    (values v1 t1 l1)))

(defun monadic-check (var-list)
;var-list is a list of constants
  (let ((result t))
    (dolist (var var-list)
	    (unless (or (symbolp (type var)) (symbolp (car (type var))))
		    (setq result nil)))
    result))

(defun bd-vars-in-gwff (gwff)
  (cond ((label-q gwff) (apply-label gwff (bd-vars-in-gwff gwff)))
	((lsymbol-q gwff)
	 (if (get gwff 'abbrev) (bd-vars-in-gwff (get gwff 'defn))))
	((boundwff-q gwff) (cons (bdvar gwff) (bd-vars-in-gwff (cdr gwff))))
	(t (nconc (bd-vars-in-gwff (car gwff)) (bd-vars-in-gwff (cdr gwff))))))

(defvar polytype-alert nil)

(defun find-rank (type)
  (cond ((typeabbrev-p type) (find-rank (get type 'type-defn)))
	((symbolp type) 0)
	((consp type) 
	 (if (consp (car type)) 
	     (find-rank (cons (caar type) (cons (cdar type) (cdr type))))
	      ;because types have to be of the form t1->(t2->...->(tn->A)) for A of base type.
	   (1+ (find-rank-2 (cdr type)))))))

(defun find-rank-2 (typelist &optional (result 0))
  (if (null typelist) result
    (if (symbolp typelist) 0
      (if (listp (cdr typelist))
	  (find-rank-2 (cdr typelist) (max result (find-rank (car typelist))))
	(max result (1+ (find-rank (car typelist))))))))
;the last line works because then the type is A . B with B a symbol (& thus of rank 0)

(defun find-reg (type)
  (if (> (find-rank type) 3) nil
    (find-reg-2 type)))

(defun find-reg-2 (type)
  (cond ((typeabbrev-p type) (find-reg-2 (get type 'type-defn)))
	((symbolp type) 0)
	((consp type) 
	 (if (consp (car type)) 
	     (setq type (cons (cdar type) (cdr type)))
	   (setq type (cdr type)))
	 (check-reg-args type))))

(defun check-reg-args (type)
  (if (null type) t
    (if (symbolp type) t
      (if (and (listp (cdr type)) (< (find-arg (car type)) 2))
	  (check-reg-args (cdr type))
	(if (< (find-arg (car type)) 2) t nil)))))

(defun find-arg (type)
  (cond ((typeabbrev-p type) (find-reg-2 (get type 'type-defn)))
	((symbolp type) 0)
	((consp type) 
	 (if (consp (car type)) 
	     (setq type (cons (cdar type) (cdr type)))
	   (setq type (cdr type)))
	 (find-no-args type))))

(defun find-no-args (arglist)
  (if (null arglist) 0
    (if (symbolp arglist) 1
      (if (listp (cdr arglist)) (1+ (find-no-args (cdr arglist))) 1))))

(defun max-order (varlist)
  (if (null varlist) 0
    (progn
      (setq polytype-alert nil)
      (let ((olist (mapcar #'(lambda(x) (type-order (type x)))
			   varlist)))
	(if polytype-alert (progn (setq polytype-alert nil) -1) (apply 'max olist)))) ))

(defun type-order (type)
  (cond ;((eq type 'core::o) 1)
	;((eq type 'cl-user::i) 1)
        ; by the time we get to unification, we don't do any type unification.
        ; so we may as well declare that alpha (etc) are base types, and use the following 2 lines instead:
        ((typeabbrev-p type) (type-order (get type 'type-defn)))
        ((symbolp type) 1)
	((consp type) (max (type-order (car type)) (1+ (type-order (cdr type)))))
	(t (setq polytype-alert t) 0)))

(defun tp-utree (name ps)
  (let ((node (find-unification-node name nil)))
    (when node (tr-print-utree node ps))))

(defun tr-print-utree (node ps)
  (let* ((center (round (/ (+ leftmargin rightmargin) 2)))
	 (label-width (length (princ-to-string (node-print-name node))))
	 (old-tabstops nil))
    (msg T T (t (- center (round (/ label-width 2)))) (node-print-name node) t)
    (setq old-tabstops (list (cons center "|")))
    (do ((plist (node-sons node) (rec-utree-sons plist))
	 (old-plist nil plist)
	 (useless nil (tabstops-broken old-tabstops)))
	((or (equal plist old-plist) useless) (if useless (msgf "...tree continues, but no room left to print it..." t)))
	(if old-plist (setq old-tabstops (rec-utree-print leftmargin rightmargin old-plist ps))))))

(defun tabstops-broken (tabstops)
  (null (remove-if #'(lambda (x) (string= (char (cdr x) 0) "*")) tabstops)))

(defun rec-utree-print (l r plist ps)
  (let ((tabstops (closeness-filter2 (mapcar #'(lambda (x) (cons (round (car x)) (cdr x)))
					    (sort (calc-tabstops l r plist) 
						  #'(lambda (x y) (< (car x) (car y))))))))
    (do* ((i 0 (1+ i))
	  (oldshape " " (or newshape " "))
	  (newshape (cdr (assoc i tabstops)) (cdr (assoc i tabstops))))
	 ((> i (caar (last tabstops))))
	 (if newshape (progn 
			(if (> (length newshape) 1) (setq newshape (char newshape 1)))
			(if (string= newshape "\\") (msg "\\")
			  (if (string= newshape "/") (msg "/")
			    (msg newshape))) 
			(if (or (string= newshape "/") (string= newshape "+"))
			    (setq newshape "-")
			  (setq newshape " ")))
	   (progn (if (string= oldshape "-") (setq newshape "-"))
		  (msg oldshape))))
    (msg t)
    (mapcar #'(lambda (x y) (p-utree-elt (cons (car x) "|") (node-print-name y))) tabstops (remove-if #'null (flatten-mstlist plist)))
    (msg t)
    (mapcar #'(lambda (x y) (p-utree-type x (node-subst-stack y) (node-parent y))) tabstops (remove-if #'null (flatten-mstlist plist)))
    (msg t)
    (if ps
	(mapcar #'(lambda (x y) (p-utree-dp x (node-subst-stack y) tabstops)) 
		tabstops (remove-if #'null (flatten-mstlist plist))))
    (dolist (tb tabstops) (msg (t (car tb)) "|"))
    (unless (null (remove-if #'null (mapcar #'node-terminal-flag (remove-if #'null (flatten-mstlist plist)))))
	    (msg t)
	    (mapcar #'(lambda (x y) (p-utree-elt x (node-terminal-flag y) t)) tabstops (remove-if #'null (flatten-mstlist plist))))
    (msg t)
    tabstops))

(defun p-utree-elt (tb l &optional (ms nil))
  (if (string= (char (cdr tb) 0) "*") 
      (if (and ms l) (let ((str (princ-to-string l)))
		       (case (length str)
			     (0 (msg (t (car tb)) "|"))
			     (1 (msg (t (car tb)) str))
			     (2 (msg (t (1- (car tb))) str))
			     (t (msg (t (1- (car tb))) (elt str 0) (elt str 1) (elt str 2))))) 
	(if l (msg (t (car tb)) "*") (msg (t (car tb)) "|")))
    (if (null l) 
	(msg (t (car tb)) "|")
      (if ms
	  (msg (t (- (car tb) (round (/ (length (princ-to-string l)) 2)))) l )
	(msg (t (car tb)) (elt (princ-to-string l) (1- (length (princ-to-string l)))))))))

#+comment(defun p-utree-type (tb l &optional (ms nil))
  (let* ((typ (find-type l)))
	  (if (consp typ) (msg (t (car tb)) "I")
	    (if (null typ) (msg (t (car tb)) "~")
	      (if (integerp typ) (msg (t (car tb)) "P")
		(msg (t (car tb)) (princ-to-string typ)))))))

(defun p-utree-type (tb l p)
  (progn
    (when p (setq l (setdiff l (node-subst-stack p))))
    (setq l (reduce #'(lambda (x y) (concatenate 'string x y)) (mapcar #'find-type l)))
    (msg (t (1+ (- (car tb) (round (/ (length l) 2))))) l)))
 
(defun find-type (elt)
  (if (not (subst-p (cdr elt))) "[A]"
    (if (consp (subst-type (cdr elt))) "[I]"
      (if (null (subst-type (cdr elt))) "[~]"
	(if (integerp (subst-type (cdr elt))) "[P]"
	  (concatenate 'string "[" (princ-to-string (subst-type (cdr elt))) "]"))))))

(defun p-utree-dp (tb l tbs)
  (let ((wherearewe 0))
    (dolist (tab tbs)
	    (if (and (eq (car tb) (car tab)) (eq (cdr tb) (cdr tab)))
		(setq wherearewe (p-utree-dp-real tb l wherearewe))
	      (if (string= (char (cdr tab) 0) "*") 
		  (progn (spaces (- (car tab) wherearewe)) 
			 (setq wherearewe (1+ (car tab)))
			 (msg "*"))
		(progn (spaces (- (car tab) wherearewe))
		       (setq wherearewe (1+ (car tab))) 
		       (msg "|")))))
  (msg t)))

(defun p-utree-dp-real (tb l2 wherearewe)
  (if (string= (char (cdr tb) 0) "*") (progn (spaces (- (car tb) wherearewe)) (msg "*") (setq wherearewe (1+ (car tb))))
    (let* ((l (car l2))
	   (left (lambda-reduce (car l)))
	   (right (lambda-reduce-subst (cdr l) l2))
	   (lstring (super-hack (with-output-to-string (*standard-output*) (msg (left . gwff)))))
	   (rstring (super-hack (with-output-to-string (*standard-output*) (msg (right . gwff)))))
	   (ll (+ (+ 5 (find-rough-length left)) (find-rough-length right))))
      (if (null (and left right))
	  (progn
	    (spaces (- (car tb) wherearewe))
	    (setq wherearewe (1+ (car tb)))
	    (msg "|"))
	(if (> (+ (car tb) (round (/ ll 2))) rightmargin)
	    (progn
	      (spaces (- (car tb) wherearewe))
	      (setq wherearewe (1+ (car tb)))
	      (msg "*"))
	  (progn
	    (spaces (- (- (car tb) (round (/ ll 2))) wherearewe))
	    (setq wherearewe (+ (car tb) (- ll (round (/ ll 2)))))
	    (msg "(" lstring " . " rstring ")"))))
      wherearewe)))

(defun super-hack (string)
  (let ((strlist nil)
	(strlist2 nil)
	(changing-flag nil))
    (do ((i 0 (1+ i)))
	((= i (length string)) (setq strlist (reverse strlist)))
	(push (princ-to-string (elt string i)) strlist))
    (dolist (char strlist)
	    (cond ((string= char "^") 
		   (setq changing-flag t) 
		   (setq strlist2 (append strlist2 (mapcar #'(lambda (x) (princ-to-string x))
							   `(,escape-char #\[ #\5 #\m)))))
		  ((or (not changing-flag)
		       (and changing-flag (string>= char "0") (string<= char "9")))
		   (setq strlist2 (append strlist2 (list char))))
		  (changing-flag
		   (setq changing-flag nil)
		   (setq strlist2 (append strlist2  (mapcar #'(lambda (x) (princ-to-string x))
							   `(,escape-char #\[ #\0 #\m))))
		   (setq strlist2 (append strlist2 (list char))))))
    (reduce #'(lambda (x y) (concatenate 'string x y)) 
	    (append strlist2  (mapcar #'(lambda (x) (princ-to-string x))
				      `(,escape-char #\[ #\0 #\m))))))

(defun find-rough-length (gwff)
  (let ((string (super-hack (with-output-to-string (*standard-output*) (msg (gwff . gwff))))))
    (do ((i 0 (1+ i))
	 (count 0))
	((>= i (length string)) count)
	(if (= (char-int (elt string i)) 27) (setq i (+ 3 i))
	  (if (and (> (char-int (elt string i)) 31)
		   (< (char-int (elt string i)) 128))
	      (setq count (1+ count)))
	  ))))

(defun rec-utree-sons (plist)
  (if (null plist) nil
    (if (listp (car plist)) (cons (rec-utree-sons (car plist)) (rec-utree-sons (cdr plist)))
      (cons (node-sons (car plist)) (rec-utree-sons (cdr plist))))))

(defunifop prune
  (unif-applicablep (lambda () current-topnode))
  (unif-mainfns prune-tree)
  (move-command t)
  (mhelp "Prune all the branches which have either reached the maximum
allowed depth, or which end only in failure nodes."))

(defun prune-tree (&optional (node current-topnode))
  (if (null (node-sons node))
      (if (or (eq (node-terminal-flag node) 'fail) (and max-search-depth (>= (node-depth node) max-search-depth)))
	  (progn (when (node-parent node)
		       (setf (node-sons (node-parent node)) (remove-if #'(lambda (x) (eq x node)) (node-sons (node-parent node))))
		       (when (null (node-sons (node-parent node))) (setf (node-terminal-flag (node-parent node)) 'fail)))
		 (setf (node-parent node) nil)
		 (setf (node-sons node) nil)
		 t)
	nil)
    (if (memq nil (mapcar #'prune-tree (node-sons node)))
	nil
      (progn (when (node-parent node)
		   (setf (node-sons (node-parent node))
			 (remove-if #'(lambda (x) (eq x node)) (node-sons (node-parent node))))
		   (when (null (node-sons (node-parent node))) (setf (node-terminal-flag (node-parent node)) 'fail)))
	     (setf (node-parent node) nil)
	     (setf (node-sons node) nil)
	     t))))
      
(defunifop find-nesting
  (unif-applicablep (lambda () current-topnode))
  (unif-mainfns find-nesting)
  (move-command t)
  (mhelp "Find the values for MAX-SUBSTS-* implied by
the current node."))

(defun find-nesting (&optional (node current-topnode))
  (let ((substs (node-subst-stack node)))
    (multiple-value-bind (a b c) (check-simpl-count substs)
			 (msgf "At this node, we have:" t 
			       "SUBSTS-VAR :" a t "SUBSTS-PROJ : " b t "SUBSTS-PROJ-TOTAL : " c t))))

(context unification)
