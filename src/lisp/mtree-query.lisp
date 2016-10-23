;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile mtree-query
  (part-of mst)
  (extension lsp)
  (mhelp "Defines automatic search functions for matingstree."))

(context mtree-auto)

(defflag mt94-12-trigger
  (flagtype integer+-or-infinity)
  (default :infinite)
  (subjects mtree-top etrees transmit)
  (mhelp "If the current obligation contains fewer than MT94-12-TRIGGER 
literals, MT94-12 will behave in the same way as MT94-11
If it contains MT94-12-TRIGGER or more, MT94-12 will choose a literal
with as few mates as possible. There are two extrema: infinity 
means that the least branch will only be chosen if the obligation
is as big as the initial obligation; 0 means that the least branch 
will always be chosen."))

(defflag mtree-stop-immediately
  (flagtype boolean)
  (default t)
  (subjects mtree-top etrees transmit)
  (mhelp "If T, will stop an automatic search as soon as a closed 
node is found. If NIL, will continue to generate whatever level of
the tree it was working on, and will check for closed nodes when
it finishes."))

(defflag mtree-filter-dups
  (flagtype boolean)
  (default t)
  (subjects mtree-top etrees transmit)
  (mhelp "If T, will not add the same link to a mating twice on
the same branch of a matingstree during automatic search. If NIL,
will add it as many times as it wants to."))

(defun cycle-lits-in-ob (oblitlist c ob)
  (if (null oblitlist)  nil
    (let ((literal (car oblitlist)))
	    (when (memq mating-verbose '(med max)) 
		  (msgf "QRY returned the following list of possible connections:" t))
	    (let ((lit-list (if (memq mating-verbose '(med max))
				(query-ob literal ob)
			      (query-ob-quiet literal ob)))
		  (returning-flag nil))
	      (when (memq mating-verbose '(med max)) 
		    (msg t "Creating new MSTREE nodes:" t))
	      (setq returning-flag (cycle-adding-lits literal lit-list c ob))
	      ;will be T if we created a closed node and mtree-stop-immediately is T, NIL o/w
	      (if returning-flag t (cycle-lits-in-ob (cdr oblitlist) c ob))))))

(defun cycle-adding-lits (literal lit-list cur-mt-node ob)
  (if (null lit-list) nil
    (let ((lit (car lit-list))
	  (dummy-var (when interrupt-enable (handle-user-interrupt nil nil nil t))))
      (declare (ignore dummy-var))
      (setq current-matingstree (mst-goto cur-mt-node current-matingstree))
      (eval (mtree-command-interpreter (list 'add-conn (literal-name literal) (obligation-name ob) (literal-name lit) '$)))
      (if (and mtree-stop-immediately (not (get-open-obs)))
	  t
	(cycle-adding-lits literal (cdr lit-list) cur-mt-node ob)))))

(defun cycle-expanding-nodes (nodelist)
  (if (null nodelist) nil
    (let ((leaf (car nodelist))
	  (dummy-var (when interrupt-enable (handle-user-interrupt nil nil nil t)))
	  (returning-flag nil))
      (declare (ignore dummy-var))
      (when (memq mating-verbose '(med max)) 
	    (msgf "***Working on matingstree " (matingstree-name leaf) ":"))
      (setq current-matingstree (mst-goto (matingstree-name leaf) current-matingstree))
      (setq returning-flag (add-all-ob 'default))
      (if returning-flag t (cycle-expanding-nodes (cdr nodelist))))))

(defun cycle-expanding-nodes-lb (nodelist)
  (if (null nodelist) nil
    (let ((leaf (car nodelist))
	  (dummy-var (when interrupt-enable (handle-user-interrupt nil nil nil t)))
	  (returning-flag nil))
      (declare (ignore dummy-var))
      (when (memq mating-verbose '(med max)) 
	    (msgf "***Working on matingstree " (matingstree-name leaf) ":"))
      (setq current-matingstree (mst-goto (matingstree-name leaf) current-matingstree))
      (setq returning-flag (add-all-ob-lb 'default))
      (if returning-flag t (cycle-expanding-nodes-lb (cdr nodelist))))))

(defun eligible-conjunction-list (jform)
   (let ((parent (jform-parent jform)))
      (and parent
          (if (eq (jform-type parent) 'conjunction)
              (cons (cons jform parent) (eligible-conjunction-list parent))
              (eligible-conjunction-list parent)))))

;;;if a conjunction or a disjunction is empty, the following function
;;;may behave weirdly.
(defun ob-jform-to-literal-list (jform)
   (case (jform-type jform)
      (conjunction (apply #'nconc (mapcar #'ob-jform-to-literal-list 
                                          (conjunction-components jform))))
      (disjunction (apply #'nconc (mapcar #'ob-jform-to-literal-list 
                                          (disjunction-components jform))))
      (universal (ob-jform-to-literal-list (universal-scope jform)))
      (literal (list jform))))


;;;--------------------------------------------------------

(defun ineligible-disjunction-list (ob)
   (let (disj-list)
     (do ((newob ob)
          (oldob (obligation-last ob)))
         ((null oldob) disj-list)
         (if (eq (obligation-type oldob) 'disjunction)
             (do ((oldoldob (obligation-last oldob)))
                 ((or (null oldoldob)
                      (neq (obligation-type oldoldob) 'disjunction))
                  (if oldoldob (push (cons (obligation-jform oldob)
                                        (obligation-jform newob))
                                  disj-list)))
                 (setq oldob oldoldob)))
          (progn (setq newob oldob) 
                 (setq oldob (obligation-last oldob))))))
     
(defun break-down-conjunction-list (conjlist ineligible)
  (remove-duplicates (break-down-conjunction-list-real conjlist ineligible)))

(defun break-down-conjunction-list-real (conjlist ineligible)
  (and conjlist
   (let* ((head (car conjlist))
          (components (conjunction-components (cdr head)))
          (literal-list (jform-to-literal-list (car components) ineligible)))
     (dolist (com (cdr components))
             (nconc literal-list (jform-to-literal-list com ineligible)))
     (nconc literal-list (break-down-conjunction-list (cdr conjlist) ineligible)))))

(defun jform-to-literal-list (jform ineligible)
  (labels ((ftemp (fjform) 
     (case (jform-type fjform)
       (conjunction (apply #'nconc (mapcar #'ftemp 
                                          (conjunction-components fjform))))
       (disjunction (let ((hxpair (assoc fjform ineligible)))
                      (if hxpair                           
                          (ftemp (cdr hxpair))
                          (apply #'nconc 
                            (mapcar #'ftemp (disjunction-components fjform))))))
       (universal (ftemp (universal-scope fjform)))
       (literal (list fjform)))))
    (ftemp jform)))
  

(defun query-ob (literal ob)
  (declare (special cl-user::considered-conn-enabled))
   (let ((literal-list (break-down-conjunction-list 
                           (eligible-conjunction-list literal)
                           (ineligible-disjunction-list ob)))
	 (mated-list (mb-mated-to literal))
	 (cl-user::considered-conn-enabled nil))
     (declare (special cl-user::considered-conn-enabled))
     (when mated-list (msgf "Already mated to : ") (print mated-list) (msgf "Eligible mates : "))
     (setq literal-list (delete-if-not #'(lambda (x) (occurs-in-obtree-path (literal-name x) ob)) 
				       literal-list))
     (setq literal-list (delete-if-not #'(lambda (x) (mb-stupid-mates literal x (eproof-jform current-eproof)))
				       literal-list))
     (if mtree-filter-dups (setq literal-list (delete-if #'(lambda (x) (memq x mated-list)) literal-list)))
     (print (delete-if-not #'(lambda (x) (eligible-literal (literal-name literal) ob 
							   (literal-name x) 
							   (occurs-in-obtree-path (literal-name x) ob)))
                    literal-list))))

(defun query-ob-quiet (literal ob)
  (declare (special cl-user::considered-conn-enabled))
   (let ((literal-list (break-down-conjunction-list 
                           (eligible-conjunction-list literal)
                           (ineligible-disjunction-list ob)))
	 (mated-list (mb-mated-to literal))
	 (cl-user::considered-conn-enabled nil))
     (declare (special cl-user::considered-conn-enabled))
     (setq literal-list (delete-if-not #'(lambda (x) (occurs-in-obtree-path (literal-name x) ob)) 
				       literal-list))
     (setq literal-list (delete-if-not #'(lambda (x) (mb-stupid-mates literal x (eproof-jform current-eproof)))
				       literal-list))
     (if mtree-filter-dups (setq literal-list (delete-if #'(lambda (x) (memq x mated-list)) literal-list)))
     (delete-if-not #'(lambda (x) (eligible-literal (literal-name literal) ob 
						    (literal-name x) 
						    (occurs-in-obtree-path (literal-name x) ob)))
                    literal-list)))

(defun mb-mated-to (literal)
 (let ((active-mating (mapcar #'(lambda (x) (car (gethash x (connections-array)))) 
			      (mating-clist (matingstree-mating current-matingstree))))
       (returning-list nil))
   (dolist (conn active-mating)
	   (if (eq (literal-name literal) (literal-name (car conn)))
	       (setq returning-list (cons (cdr conn) returning-list))
	     (if (eq (literal-name literal) (literal-name (cdr conn)))
		 (setq returning-list (cons (car conn) returning-list)))))
   returning-list))

(defun mb-stupid-mates (lit1 lit2 jform)
  (case (jform-type jform)
	(disjunction (let ((component nil))
		       (dolist (comp (disjunction-components jform))
			       (if (and (occurs-in-jform (literal-name lit1) comp)
					(occurs-in-jform (literal-name lit2) comp))
				   (setq component comp)))
		       (if component (mb-stupid-mates lit1 lit2 component) nil)))
	;return nil if they occur in different parts of a disjunction, o/w recurse.
	(conjunction (let ((component nil))
		       (dolist (comp (conjunction-components jform))
			       (if (and (occurs-in-jform (literal-name lit1) comp)
					(occurs-in-jform (literal-name lit2) comp))
				   (setq component comp)))
		       (if component (mb-stupid-mates lit1 lit2 component) t)))
	;return t if they occur in different parts of a conjunction, o/w recurse.
	(universal (mb-stupid-mates lit1 lit2 (universal-scope jform)))
	;if it's a universal quantifier, skip it.
	(literal t)
	))

(defmtreeop qry
  (mtree-alias query-ob)
  (mtree-args 2)
  (mhelp "Output a list of literals which can be mated with a given literal."))

(defwffop query-ob
  (argnames literal obligation)
  (argtypes leaftype symbol-or-integer)
  (defaultfns (lambda (x y) 
                 (let* ((jform (or ms-jform (eproof-jform current-eproof)))
			(x1 (if (eq x '$) x
                                (intern (complete-leaf-name x leaf-name))))
                        (y1 (if (eq y '$)
                                (and (neq '$ x) 
                                     (or (car (occurs-in-obtree x1 current-obligation)) 
                                     (throwfail "Sorry, no such a leaf " x1 " 
                                                in unfulfiled obligations.")))
                                (let ((y2 (if (obligation-p y) y 
                                              (obt-goto y current-obligation))))
                                  (if (obligation-next y2)
                                      (throwfail "Node " y2 " is not a leaf 
                                                 in the obligation tree.") y2))))
			(x1 (if (eq x '$) x
                                (find-jform-name (complete-leaf-name x leaf-name) jform))))
                     (list x1 y1))))
  (mhelp "Output a list of literals which can be mated with a given literal.")
  (resulttype matingstree))


(defmtreeop add-all-lit
  (mtree-alias add-all-lit)
  (mtree-args 2)
  (mhelp "Attempt to mate a literal with all potential mates on 
the current path."))

(defmtreeop add-all-ob
  (mtree-alias add-all-ob)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Attempt to mate all literals in an obligation 
with all potential mates on the current path."))

(defwffop add-all-lit
  (argnames literal obligation)
  (argtypes symbol-or-integer symbol-or-integer)
  (defaultfns (lambda (x y) 
                 (let* ((jform (or ms-jform (eproof-jform current-eproof)))
			(x1 (if (eq x '$) x
                                (intern (complete-leaf-name x leaf-name))))
                        (y1 (if (eq y '$)
                                (and (neq '$ x) 
                                     (or (car (occurs-in-obtree x1 current-obligation)) 
                                     (throwfail "Sorry, no such a leaf " x1 " 
                                                in unfulfiled obligations.")))
                                (let ((y2 (if (obligation-p y) y 
                                              (obt-goto y current-obligation))))
                                  (if (obligation-next y2)
                                      (throwfail "Node " y2 " is not a leaf 
                                                 in the obligation tree.") y2))))
			(x1 (if (eq x '$) x
                                (find-jform-name (complete-leaf-name x leaf-name) jform))))
                     (list x1 y1))))
  (mhelp "Attempt to mate a literal with all potential mates on 
the current path.")
  (resulttype matingstree))

(defwffop add-all-ob
  (argnames obligation)
  (argtypes symbol-or-integer)
  (mhelp "Attempt to mate all literals in an obligation with 
all potential mates on the current path.")
  (resulttype obligation))


(defun add-all-lit (literal ob)
  (when (memq mating-verbose '(med max)) 
	(msgf "QRY returned the following list of possible connections:" t))
  (let ((cur-mt-node (matingstree-name current-matingstree))
	(lit-list (if (memq mating-verbose '(med max))
		      (query-ob literal ob)
		    (query-ob-quiet literal ob))))
    (when (memq mating-verbose '(med max)) 
	  (msg t "Creating new MSTREE nodes:" t))
    (if (cycle-adding-lits literal lit-list cur-mt-node ob)
	t
      (if (and (eq (matingstree-name current-matingstree) cur-mt-node)
	       (matingstree-sons current-matingstree))
	  (progn
	  (setq current-matingstree (mst-go-down 0 current-matingstree))
	  nil)
	nil))))

(defun add-all-ob (name)
  (add-all-ob-real
   (if (eq name 'default)
       (if (get-open-obs)
	   (obt-goto (get-default-ob) current-obligation)
	 (throwfail t "No open obligations in this obligation tree" t t))
     (obt-goto name current-obligation))))


(defun add-all-ob-real (ob)
  (let ((lits-in-ob (find-all-leaves-in-jform (obligation-jform ob)))
	(cur-mt-node (matingstree-name current-matingstree)))
    (if (cycle-lits-in-ob lits-in-ob cur-mt-node ob)
	t
      (progn (setf (matingstree-dead current-matingstree) 'all-conns-added)
					;leave a mark to show that this node has had all possible connections added
	     (setq current-matingstree (mst-goto cur-mt-node current-matingstree))
	     (if (and (eq (matingstree-name current-matingstree) cur-mt-node)
		      (matingstree-sons current-matingstree))
		 (setq current-matingstree (mst-go-down 0 current-matingstree))
	       (progn 
		 (setf (matingstree-dead current-matingstree) t)
		 (when (memq mating-verbose '(med max)) 
		       (msg "Current matingstree is now officially dead. 
Killing off this branch and moving up."))
		 (do ((mst (matingstree-parent current-matingstree) (matingstree-parent mst)))
		     ((or (null mst)
			  (neq (matingstree-dead mst) 'all-conns-added)
			  (not (all-dead (matingstree-sons mst))))
					;we've either reached the top, or a node which we don't know for sure
					;has had all possible connections added and below which all nodes are dead
		      (if (null mst) 
			  (setq current-matingstree (mst-goto 'mstroot current-matingstree))
			(setq current-matingstree (mst-goto (matingstree-name mst) current-matingstree))))
		     (progn (setf (matingstree-dead mst) t) (mst-prune mst)))))
	     nil))))
;in order to get here, we had to have open obligations

(defun all-dead (mtrees)
  (if (null mtrees) t
    (and (eq t (matingstree-dead (car mtrees))) (all-dead (cdr mtrees)))))

(defun find-all-leaves-in-jform (jform &optional (l nil))
  (let ((l (case (jform-type jform)
		 (universal
		  (find-all-leaves-in-jform (universal-scope jform) l))
		 (disjunction
		  (reduce #'append (mapcar #'(lambda (jform) (find-all-leaves-in-jform jform l))
					   (disjunction-components jform))))
		 (conjunction
		  (reduce #'append (mapcar #'(lambda (jform) (find-all-leaves-in-jform jform l))
					   (conjunction-components jform))))
		 (literal (list jform)))))
  (remove-if #'null (remove-duplicates l))))

(defmtreeop expand-leaves
  (mtree-alias expand-mst-leaves)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Apply ADD-ALL-OB to all live leaves of the current matingstree
that lie below the given node (or the current node, if no node is given).
WARNING: Potential combinatorial explosion!"))

(defwffop expand-mst-leaves
  (argnames mtree)
  (argtypes symbol-or-integer)
  (mhelp "Apply ADD-ALL-OB to all live leaves of the current matingstree
that lie below the given node (or the current node, if no node is given).
WARNING: Potential combinatorial explosion!")
  (resulttype obligation))


(defun expand-mst-leaves (name)
  (let ((cur-mt-node (matingstree-name current-matingstree))
	(leaflist (get-live-leaves-main (if (eq name 'default)
				       current-matingstree
				     (mst-goto name current-matingstree)))))
    (if (cycle-expanding-nodes leaflist)
	(progn
	  (setq current-matingstree (mst-goto cur-mt-node current-matingstree))
	  t)
      (progn 
      (setq current-matingstree (mst-goto cur-mt-node current-matingstree))
      nil))))

(defmtreeop MT94-11
  (mtree-alias mst-basic-search)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Apply EXPAND-LEAVES repeatedly to all live leaves of the current 
matingstree that lie below the given node (or the current node, if 
no node is given), until a closed leaf is generated.
WARNING: Potential combinatorial explosion!"))

(defwffop mst-basic-search
  (argnames mtree)
  (argtypes symbol-or-integer)
  (mhelp "Apply EXPAND-LEAVES repeatedly to all live leaves of the current 
matingstree that lie below the given node (or the current node, if 
no node is given), until a closed leaf is generated.
WARNING: Potential combinatorial explosion!")
  (resulttype obligation))

(defun mst-basic-search (name)
   (startcount 'mating-ctr)
   (unwind-protect  
      (mst-basic-search-real name)
     (runcount 'mating)
     (breakcount 'mating)
     (display-time 'mating)
     (breakcount 'mating-ctr)))


(defun mst-basic-search-real (name)
  (let* ((doneflag (expand-mst-leaves name))
	 (ll (get-live-leaves-main 
	      (if (eq name 'default)
		  (mst-goto 'mstroot current-matingstree)
		(mst-goto name current-matingstree))))
	 (llc (remove-if #'(lambda (x) (get-open-obs (matingstree-obligation x))) ll)))
    (if (or llc doneflag) (msgf t t t "END OF SEARCH (SUCCESS)" t "Have found closed leaves: " llc t)
      (if ll (mst-basic-search-real name)
	(msgf t t t "END OF SEARCH (FAILURE)" t "All leaves below starting point are now dead." t)))))

(defmtreeop mt94-12
  (mtree-alias mst-lb-search)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Least Branching Search: In each leaf node, take the current
obligation and find a literal that can be mated, but with as few 
mates as possible. Add all of these mates as sons to this node.
Repeat until a closed leaf is generated.
This search is probably not complete."))

(defwffop mst-lb-search
  (argnames mtree)
  (argtypes symbol-or-integer)
  (mhelp "Least Branching Search: In each leaf node, take the current
obligation and find a literal that can be mated, but with as few 
mates as possible. Add all of these mates as sons to this node.
Repeat until a closed leaf is generated.
This search is probably not complete.")
  (resulttype obligation))

(defun mst-lb-search (name)
   (startcount 'mating-ctr)
   (unwind-protect  
      (mst-lb-search-real name)
     (runcount 'mating)
     (breakcount 'mating)
     (display-time 'mating)
     (breakcount 'mating-ctr)))


(defun mst-lb-search-real (name)
  (multiple-value-bind (ret-val1 ret-val2) (expand-mst-lb-leaves name)
  (let* ((ll (get-live-leaves-main 
	      (if (eq name 'default)
		  (mst-goto 'mstroot current-matingstree)
		(mst-goto name current-matingstree))))
	 (llc (remove-if #'(lambda (x) (get-open-obs (matingstree-obligation x))) ll)))
    (if (or llc ret-val1) (msgf t t t "END OF SEARCH (SUCCESS)" t "Have found closed leaves: " llc t)
      (if (and ll ret-val2) (mst-lb-search-real name)
	(msgf t t t "END OF SEARCH (FAILURE)" t "No more leaves have possible new connections." t 
	      "Try adding a connection to a node higher in the tree." t))))))

(defun expand-mst-lb-leaves (name)
  (let ((cur-mt-node (matingstree-name current-matingstree))
	(leaflist (get-live-leaves-main (if (eq name 'default)
				       current-matingstree
				     (mst-goto name current-matingstree))))
	(ret-var nil))
    (setq ret-var (cycle-expanding-nodes-lb leaflist))
    (setq current-matingstree (mst-goto cur-mt-node current-matingstree))
    (values ret-var (remove-if 'null (mapcar 'matingstree-sons leaflist)))))


(defun add-all-ob-lb (name)
  (let ((argument (if (eq name 'default)
		      (if (get-open-obs)
			  (obt-goto (get-default-ob) current-obligation)
			nil)
		    (obt-goto name current-obligation))))
    (when argument 
	  (let ((count (length (find-all-leaves-in-jform (obligation-jform argument)))))
	    (if (eq mt94-12-trigger :infinite)
		(if (>= count (length *initial-lits*))
		    (add-all-ob-lb-real argument)
		  (add-all-ob-real argument))
	      (if (>= count mt94-12-trigger)
		  (add-all-ob-lb-real argument)
		(add-all-ob-real argument)))))))

(defun add-all-ob-lb-real (ob)
  (let ((lits-in-ob (find-all-leaves-in-jform (obligation-jform ob)))
	(cur-mt-node (matingstree-name current-matingstree))
	(working-lit nil)
	(working-litlist nil))
    (setf (matingstree-dead current-matingstree) 'lb-conn-added)
    (dolist (literal lits-in-ob)
	    (when (= (length working-litlist) 1) (return t))
	    (let ((lit-list (if (memq mating-verbose '(med max))
				(query-ob literal ob)
			      (query-ob-quiet literal ob))))
	      (when (or (and working-lit (> (length lit-list) 0) 
			     (< (length lit-list) (length working-litlist)))
			(and (not working-lit) (> (length lit-list) 0)))
		    (progn
		    (setq working-lit literal)
		    (setq working-litlist lit-list)))))
    (if working-lit
	  (if (cycle-adding-lits working-lit working-litlist cur-mt-node ob)
	      t
	    (progn
	      (setq current-matingstree (mst-goto cur-mt-node current-matingstree))
	      nil))
      (progn
	(setq current-matingstree (mst-goto cur-mt-node current-matingstree))
	nil))
    ))

(defmtreeop mt95-1
  (mtree-alias mst-fewest-ob-search)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Fewest Obligations Search: Choose the matingstree node (from the 
entire tree, not just the tree below the current node) with the 
fewest open obligations. Go to that node and do one step of MT94-12
(i.e. choose the literal with the fewest number of mates, and generate
all of the associated branches of the mtree).
Repeat until a closed leaf is generated.
This search is probably not complete.")
)

(defwffop mst-fewest-ob-search
  (argnames mtree)
  (argtypes symbol-or-integer)
  (mhelp "Fewest Obligations Search: Choose the matingstree node (from the 
entire tree, not just the tree below the current node) with the 
fewest open obligations. Go to that node and do one step of MT94-12
(i.e. choose the literal with the fewest number of mates, and generate
all of the associated branches of the mtree).
Repeat until a closed leaf is generated.
This search is probably not complete.")
  (resulttype obligation))

(defun mst-fewest-ob-search (name)
   (startcount 'mating-ctr)
   (unwind-protect  
      (mst-fewest-ob-search-real name)
     (runcount 'mating)
     (breakcount 'mating)
     (display-time 'mating)
     (breakcount 'mating-ctr)))


(defun mst-fewest-ob-search-real (name)
  (multiple-value-bind (ret-val1 ret-val2) (expand-mst-fewest-ob-leaves name)
  (let* ((ll (get-live-leaves-main 
	      (if (eq name 'default)
		  (mst-goto 'mstroot current-matingstree)
		(mst-goto name current-matingstree))))
	 (llc (remove-if #'(lambda (x) (get-open-obs (matingstree-obligation x))) ll)))
    (if (or llc ret-val1) (msgf t t t "END OF SEARCH (SUCCESS)" t "Have found closed leaves: " llc t)
      (if (and ll ret-val2) (mst-fewest-ob-search-real name)
	(msgf t t t "END OF SEARCH (FAILURE)" t "No more leaves have possible new connections." t 
	      "Try adding a connection to a node higher in the tree." t))))))

(defun expand-mst-fewest-ob-leaves (name)
  (let ((cur-mt-node (matingstree-name current-matingstree))
	(leaflist (get-live-leaves-main (if (eq name 'default)
				       current-matingstree
				     (mst-goto name current-matingstree))))
	(ret-var nil))
    (setq leaflist (leaf-with-fewest-obs leaflist))
    (setq ret-var (cycle-expanding-nodes-lb leaflist))
    (setq current-matingstree (mst-goto cur-mt-node current-matingstree))
    (values ret-var (not (null leaflist)))))
; we've only failed completely if there aren't any more open leaves...
;    (values ret-var (remove-if 'null (mapcar 'matingstree-sons leaflist)))))

(defun leaf-with-fewest-obs (leaflist &optional (leaf nil) (open-obs 0) (leaf-height 0))
  (if (or (null leaflist) (= open-obs 1)) leaf
    (if (null leaf) (leaf-with-fewest-obs (cdr leaflist) (list (car leaflist)) (open-obs-in-leaf (car leaflist)) (find-leaf-height (car leaflist)))
      (let ((open-obs-1 (open-obs-in-leaf (car leaflist)))
	    (leaf-height-1 (find-leaf-height (car leaflist))))
	(if (or (< open-obs-1 open-obs)
		(and (= open-obs-1 open-obs) (< leaf-height-1 leaf-height))) 
	    (leaf-with-fewest-obs (cdr leaflist) (list (car leaflist)) open-obs-1 leaf-height-1)
	  (leaf-with-fewest-obs (cdr leaflist) leaf open-obs leaf-height))))))

(defun find-leaf-height (leaf)
  (if (matingstree-parent leaf) (1+ (find-leaf-height (matingstree-parent leaf)))
    0))

(defun open-obs-in-leaf (leaf)
  (let ((cur-mt-node (matingstree-name current-matingstree))) 
    (setq current-matingstree (mst-goto (matingstree-name leaf) current-matingstree))
    (let ((l (length (get-open-leaf-obs))))
      (setq current-matingstree (mst-goto cur-mt-node current-matingstree))
      l)))

(defmtreeop go
  (mtree-alias go-default-ms)
  (mhelp "Call the matingstree procedure given in DEFAULT-MS."))

(defun go-default-ms ()
  (if (memq default-ms '(mt94-11 mt94-12 mt95-1)) 
      (eval (mtree-command-interpreter (list default-ms (matingstree-name current-matingstree))))
    (throwfail t default-ms " is not the value of a matingstree procedure." t 
	       "Only MT94-11, MT94-12 and MT95-1 are allowed." t)))

