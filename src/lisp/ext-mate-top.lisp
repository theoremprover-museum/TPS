;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of EXT-DAGS)

;;;
;;; File: EXT-MATE-TOP  - cebrown - 5/29/03
;;; Top level for the extensional expansion dags in Chad E. Brown's thesis.

(deffile ext-mate-top
    (part-of EXT-DAGS)
  (extension clisp)
  (mhelp "Top Level for Extensional Expansion Dags.  See Chad E. Brown's thesis."))

(context subtoplevels)

(defflag EXT-MATE-RECOMPUTE-JFORMS
  (default T)
  (flagtype boolean)
  (subjects ext-search)
  (mhelp "If T, JForms are eagerly recomputed after modifications are made to
extensional expansion dags in the EXT-MATE top level.  Otherwise, the
user must use the command CJFORM to update the JForm.  Even if the
value is T, CJFORM is useful for obtaining special JForms where
Flex-Flex or Flexible nodes are left out."))

(defun extmatecmd-mhelp (keyword category)
  (princ-mhelp keyword category)
  (unless short-help 
    (when *doing-html* (msg " fnord "))
    (msgf "The command format for " keyword " is:" -2 "<EXT-MATE>" keyword)
    (print-tps-format* keyword " "
		       (+ 5 (length (format nil "~A" keyword)))
		       (get keyword 'extmate-argnames)
		       (get keyword 'extmate-argtypes)
		       nil)))

(defun ext-mate (gwff)
  (cond ((ext-exp-open-dag-p gwff)
	 (setq *current-edag* gwff))
	((ext-exp-dag-p gwff)
	 (setq *current-edag* (eed-to-eeod gwff))
	 (let ((name (intern-str (create-namestring 'EDAG))))
	   (setf (get name 'ext-exp-open-dag) *current-edag*)
	   (push name *eeod-list*)))
	((and (symbolp gwff)
	      (get gwff 'ext-exp-open-dag))
	 (setq *current-edag*
	       (get gwff 'ext-exp-open-dag)))
	(t
	 (unless (gwff-q gwff)
	   (setq gwff (getrwff gwff)))
	 (let ((name (intern-str (create-namestring 'EDAG))))
	   (dolist (x (list leaf-name econj-name edisj-name imp-name
			    true-name false-name neg-name
			    selection-name expansion-name
			    rewrite-name 'EQNGOAL 'EQN 'DEC 'ATOM
			    'FLEX))
	     (reset-name-counter x))
	   (create-ext-exp-open-dag gwff)
	   (setf (get name 'ext-exp-open-dag) *current-edag*)
	   (push name *eeod-list*))))
  (unless (ext-exp-open-dag-p *current-edag*)
    (throwfail "Not an extensional expansion dag."))
  (setq *individual-types* (find-prim-types (ext-exp-open-dag-shallow *current-edag*)))
  (setq *merged-edag* nil)
  (setq *current-edag-jform*
	(eeod-to-jform *current-edag*
		       :posflex t :negflex t :flexflex t))
  (let ((nodestack nil))
    (declare (special nodestack))
    (%catch% (extmatetop)
	     (exit-inferior-top
	      (progn
		core::expand-catch-throw)))))

(defun extmatetop () 
  (let ((top-prompt-fn #'ext-mate-top-prompt)
	(command-interpreter #'ext-mate-command-interpreter)
	(print-* #'ext-mate-print-*)
	(top-level 'ext-mate-top))
    (declare (special top-prompt-fn command-interpreter print-* top-level command-ctree))
    (secondary-top)))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun ext-mate-top-prompt (id) (format nil "<EXT-MATE~A>" id))

(defun ext-mate-print-* (result) (fresh-line) (prin1 result))

(defun ext-mate-command-interpreter (cmd)
  (let ((carcmd (car cmd)))
    (setq core::*retrieve-stack* nil) ; just in case! (of something)
    (cond ((null cmd) nil)
	  ((and (null (cdr cmd)) (atom carcmd))
	   (cond ((integerp carcmd)
		  (if (zerop carcmd)
		      '(ext-mate-0)
		    (list 'ext-mate-successor carcmd)))
		 ((and (symbolp carcmd) (get carcmd 'extmatecmd))
		  `(ext-mate-opdecode (quote ,cmd)))
		 ((and (symbolp carcmd) (get carcmd 'mexpr))
		  `(comdecode (quote ,cmd)))
		 ((and (symbolp carcmd) (get carcmd 'reviewcmd))
		  `(comdecode (quote ,cmd)))
		 ((and (symbolp carcmd) (get carcmd 'flag))
		  `(comdecode '(setflag ,@cmd)))
		 ((null expertflag)
		  (throwfail "Unknown Command or Flag."))
		 ((and (symbolp carcmd) (boundp carcmd)) carcmd)
		 ((and (symbolp carcmd) (fboundp carcmd)) cmd)
		 ((or (get carcmd 'mhelp) (get carcmd 'mhelp-fn))
		  (msg "Cannot evaluate that... calling HELP " carcmd t t)
		  `(comdecode '(help ,carcmd)))
		 (t  (throwfail ";" carcmd " - Unbound variable."))))
	  ((and expertflag (null (cdr cmd))) carcmd)
	  ((and (symbolp carcmd) (get carcmd 'extmatecmd))
	   `(ext-mate-opdecode (quote ,cmd))) 
	  ((and (symbolp carcmd) (get carcmd 'mexpr))
	   `(comdecode (quote ,cmd)))
	  ((and (symbolp carcmd) (get carcmd 'reviewcmd))
	   `(comdecode (quote ,cmd)))
	  ((and (symbolp carcmd) (get carcmd 'flag))
	   `(comdecode '(set ,@cmd)))
	  ((null expertflag)
	   (throwfail "Unknown command."))
	  ((symbolp carcmd)
	   (if (fboundp carcmd) cmd
	       (throwfail ";" carcmd " - Undefined function.")))
	  (t cmd))))

(defun ext-mate-opdecode (command)
  (let ((keyword (car command))
	mainfn result)
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values keyword
		       (copy (cdr command))
		       (get keyword 'extmate-argtypes)
		       (mapcar #'(lambda (x) (declare (ignore x)) nil)
			       (get keyword 'extmate-argtypes))
		       nil
		       (get keyword 'extmate-defaultfns)
		       nil
		       (get keyword 'extmate-argnames)
		       (get keyword 'extmate-arghelp))
      (declare (ignore external-arglist))
      (setq mainfn (or (get keyword 'extmate-mainfns) keyword))
      (%catch% (setq result (apply mainfn internal-arglist))
	       (fail (complain f "Error from " mainfn ".  " 
                               core::expand-catch-throw)
		     (throwfail "Operation aborted.")))
      result)))

(defmexpr ext-mate
  (argtypes gwff0-or-label-or-edag)
  (defaultfns (lambda (z)
		(list (if (eq z '$)
			  (cond ((ext-exp-open-dag-p *current-edag*)
				 *current-edag*)
				((ext-exp-dag-p *current-edag*)
				 *current-edag*)
				(t '$))
			z))))
  (mainfns ext-mate)
  (mhelp "Enter the EXT-MATE top level for building and manipulating
extensional expansion dags (see Chad E. Brown's thesis)."))

(context subtoplevels)

(defextmate leave
  (extmate-mainfns exit-ext-mate)
  (mhelp "Leave EXT-MATE to the next enclosing top level."))

(defun exit-ext-mate () 
  (%throw% '|[Left EXT-MATE.]| exit-inferior-top))

(defun get-gwff0-or-edag (gwff)
  (cond ((ext-exp-open-dag-p gwff) gwff)
	((and (symbolp gwff) (get gwff 'ext-exp-open-dag))
	 (get gwff 'ext-exp-open-dag))
	((and (symbolp gwff) (boundp gwff)
	      (or (ext-exp-open-dag-p (symbol-value gwff))
		  (ext-exp-dag-p (symbol-value gwff))))
	 (symbol-value gwff))
	(t (let ((global-type 'O))
	     (declare (special global-type))
	     (setq *last-gwff-name* gwff)
	     (getwff-subtype 'gwff-p gwff)))))

(context wff-printing)
; PRINTING: etd etp p pdeep pp ppdeep ppf psh ptree ptree* ptree-file 

(defextmate etp
  (extmate-mainfns ext-mate-etp)
  (mhelp "Show the current the extensional expansion dag, printing all shallow formulas"))

(defextmate etd
  (extmate-mainfns ext-mate-etd)
  (mhelp "Show the current the extensional expansion dag, only printing some shallow formulas"))

(defextmate p
  (extmate-mainfns ext-mate-p)
  (mhelp "Print the current extensional expansion dag node."))

(defextmate pdeep
  (extmate-mainfns ext-mate-pdeep)
  (mhelp "Print the deep formula of an extensional expansion dag node."))

(defextmate ppdeep
  (extmate-mainfns ext-mate-ppdeep)
  (mhelp "Pretty-print the deep formula of an extensional expansion dag node."))

(defextmate pp
  (extmate-mainfns ext-mate-pp)
  (mhelp "Print an extensional expansion dag with node-names."))

(defextmate ppf
  (extmate-mainfns ext-mate-ppf)
  (mhelp "Prints information about the current extensional expansion dag."))

(defextmate psh
  (extmate-mainfns ext-mate-psh)
  (mhelp "Print the shallow formula of an extensional expansion dag."))

(defun ext-mate-ppf ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (msgf "EDag : " (ext-exp-open-dag-name *current-edag*))
    (let ((vars (eeod-exp-vars *current-edag*)))
      (when vars
	(terpri) (princ "Free Variables: ") (terpri)
	(format t "~5TNode~20TVar~%")
	(dolist (v vars)
	  (dolist (arc (get v 'ext-exp-var-arcs))
	    (format t "~5T~d~20T"
		    (ext-exp-open-dag-name (ext-exp-open-arc-parent arc)))
	    (printwffhere v)
	    (terpri)))))
    (let ((atoms (eeod-get-nodes
		  #'(lambda (x)
		      (eq (ext-exp-open-dag-kind x) 'ATOM))
		  *current-edag*))
	  (flexs (eeod-get-nodes
		  #'(lambda (x)
		      (eq (ext-exp-open-dag-kind x) 'FLEX))
		  *current-edag*))
	  (pos-eqns (eeod-get-nodes
		     #'(lambda (x)
			 (and (eq (ext-exp-open-dag-kind x) 'EQN)
			      (ext-exp-open-dag-positive x)))
		     *current-edag*))
	  (eqngoals (eeod-get-nodes
		     #'(lambda (x)
			 (eq (ext-exp-open-dag-kind x) 'EQNGOAL))
		     *current-edag*)))
      (when atoms
	(terpri) (princ "Atoms:   ") (terpri)
	(print-unordered-symbol-table
	 (mapcar #'(lambda (x)
		     (ext-exp-open-dag-name x))
		 atoms)))
      (when flexs
	(terpri) (princ "Flexible:   ") (terpri)
	(print-unordered-symbol-table
	 (mapcar #'(lambda (x)
		     (ext-exp-open-dag-name x))
		 flexs)))
      (when pos-eqns
	(terpri) (princ "Positive Equations:   ") (terpri)
	(print-unordered-symbol-table
	 (mapcar #'(lambda (x)
		     (ext-exp-open-dag-name x))
		 pos-eqns)))
      (when eqngoals
	(terpri) (princ "Equation Goals:   ") (terpri)
	(print-unordered-symbol-table
	 (mapcar #'(lambda (x)
		     (ext-exp-open-dag-name x))
		 eqngoals))))
    (ext-mate-show-mating)))

(defun ext-mate-p ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (msgf (ext-exp-open-dag-name *current-edag*))))

(defun ext-mate-pdeep ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (msgf ((ext-exp-open-dag-deep *current-edag*) . gwff))
    nil))

(defun ext-mate-ppdeep ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (prtwff (ext-exp-open-dag-deep *current-edag*)
	    (displaywff t) (ppwfflag t) (printdepth 0)
	    (print-deep t) (print-nodenames nil))
    nil))

(defun ext-mate-pp ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (msgf (ext-exp-open-dag-name *current-edag*))
    (msgf "Deep formula: "
	  ((ext-exp-open-dag-deep *current-edag*) . gwff))
    (if (ext-exp-open-dag-positive *current-edag*)
	(msgf "Node is positive.")
      (msgf "Node is negative."))
    (msgf "Components: ")
    (dolist (arc (ext-exp-open-dag-arcs *current-edag*))
      (princ (ext-exp-open-dag-name (ext-exp-open-arc-node arc)))
      (princ " "))
    nil))

(defun ext-mate-psh ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (msg ((ext-exp-open-dag-shallow *current-edag*) . gwff))
    nil))

(defun ext-mate-etp ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (let ((printed-nodes nil)
	  (print-shallows t))
      (declare (special printed-nodes print-shallows))
      (print-ext-exp-open-dag-verbose-rec *current-edag*))
    nil))

(defun ext-mate-etd ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (let ((printed-nodes nil)
	  (print-shallows nil))
      (declare (special printed-nodes print-shallows))
      (print-ext-exp-open-dag-verbose-rec *current-edag*))
    nil))

(defextmate show-mating
  (extmate-mainfns ext-mate-show-mating)
  (mhelp "Show the current mating in the extensional expansion dag"))

(defun ext-mate-show-mating ()
  (declare (special *current-edag*))
  (ext-mate-show-mating-1 *current-edag*))

(defun ext-mate-show-mating-1 (edag)
  (when (ext-exp-open-dag-p edag)
    (let ((mates nil)
	  (eunifs nil)
	  (node-assoc nil))
      (dolist (node (eeod-get-nodes
		     #'(lambda (x)
			 (or (member (ext-exp-open-dag-kind x) '(ATOM EQNGOAL))
			     (and (eq (ext-exp-open-dag-kind x) 'EQN) (ext-exp-open-dag-positive x))))
		     edag))
	(dolist (arc (ext-exp-open-dag-arcs node))
	  (let* ((node0 (ext-exp-open-arc-node arc))
		 (a (assoc node0 node-assoc)))
	    (if a
		(case (ext-exp-open-arc-kind arc)
		  (MATE (push (cons node (cadr a)) mates))
		  ((EUNIF1 EUNIF2) (push (cons node (cadr a)) eunifs)))
	      (push (list node0 node) node-assoc)))))
      (when mates
	(msgf "Mates : " mates t))
      (when eunifs
	(msgf "EUnifs : " eunifs t))
      (unless (or mates eunifs)
	(msgf "Currently, there are no connections." t)))))

(context ext-search)

(defextmate complete-p
  (extmate-mainfns ext-mate-complete-p)
  (mhelp "Indicate if the current extensional expansion dag is complete,
and print an open path if it is not complete."))

(defun ext-mate-complete-p ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (multiple-value-bind
	(compl open-path)
	(ext-exp-open-dag-complete *current-edag*)
      (if compl
	  (msgf "Extensional Expansion Dag is Complete.")
	(msgf "Extensional Expansion Dag is NOT Complete." t
	      "Open Path:" t
	      open-path)))))

(context moving)

(defextmate \0
  (extmate-mainfns ext-mate-0)
  (mhelp "Move back to previous node, e.g., undo the last L or R
command. Note that 0 stands for the numeral zero."))

(defun ext-mate-0 ()
  (declare (special nodestack *current-edag*))
  (if (and nodestack (ext-exp-open-dag-p (car nodestack)))
      (setq *current-edag* (pop nodestack))
    (throwfail "No more nodes on the stack to undo.")))

(defextmate d
  (extmate-mainfns ext-mate-d)
  (mhelp "Move down one node in extensional expansion dag
(to leftmost node if more than one successor)."))

(defun ext-mate-d ()
  (declare (special nodestack *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (if (ext-exp-open-dag-arcs *current-edag*)
	(progn
	  (push *current-edag* nodestack)
	  (setq *current-edag* (ext-exp-open-arc-node
				(car (ext-exp-open-dag-arcs *current-edag*))))
	  *current-edag*)
      (throwfail *current-edag* " has no child."))))

(defun ext-mate-successor (n)
  (declare (special nodestack *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (let ((arc (nth (1- n) (ext-exp-open-dag-arcs *current-edag*))))
      (if arc
	  (progn
	    (push *current-edag* nodestack)
	    (setq *current-edag* (ext-exp-open-arc-node arc))
	    *current-edag*)
	(throwfail *current-edag* " has fewer than " n " children.")))))

(defextmate l
  (extmate-mainfns ext-mate-l)
  (mhelp "For an infix edag node, move to the left argument."))

(defextmate r
  (extmate-mainfns ext-mate-r)
  (mhelp "For an infix edag node, move to the right argument."))

(defun ext-mate-l ()
  (declare (special nodestack *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (if (member (ext-exp-open-dag-kind *current-edag*)
		'(CON DIS IMP))
	(progn
	  (push *current-edag* nodestack)
	  (setq *current-edag* (ext-exp-open-arc-node
				(car (ext-exp-open-dag-arcs *current-edag*))))
	  *current-edag*)
      (throwfail *current-edag* " is not an infix node."))))

(defun ext-mate-r ()
  (declare (special nodestack *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (if (member (ext-exp-open-dag-kind *current-edag*)
		'(CON DIS IMP))
	(progn
	  (push *current-edag* nodestack)
	  (setq *current-edag* (ext-exp-open-arc-node
				(cadr (ext-exp-open-dag-arcs *current-edag*))))
	  *current-edag*)
      (throwfail *current-edag* " is not an infix node."))))

(defextmate fb
  (extmate-mainfns ext-mate-fb)
  (mhelp "Move down to the first expansion or selection node
(those whose shallow formulas start with a binder)."))

(defun ext-mate-fb ()
  (declare (special nodestack *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (let ((nodes (eeod-get-nodes #'(lambda (x)
				     (member (ext-exp-open-dag-kind x)
					     '(SEL EXP)))
				 *current-edag*)))
      (if nodes
	  (progn
	    (push *current-edag* nodestack)
	    (setq *current-edag* (car nodes))
	    *current-edag*)
	(throwfail "Could not find a binder below " *current-edag*)))))

(defextmate fi
  (extmate-mainfns ext-mate-fi)
  (mhelp "Move down to the first infix node."))

(defun ext-mate-fi ()
  (declare (special nodestack *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (let ((nodes (eeod-get-nodes #'(lambda (x)
				     (member (ext-exp-open-dag-kind x)
					     '(CON DIS IMP)))
				 *current-edag*)))
      (if nodes
	  (progn
	    (push *current-edag* nodestack)
	    (setq *current-edag* (car nodes))
	    *current-edag*)
	(throwfail "Could not find a binder below " *current-edag*)))))

(defextmate up
  (extmate-mainfns ext-mate-up)
  (mhelp "Move up one node in the edag."))

(defextmate ^
  (extmate-mainfns ext-mate-all-up)
  (mhelp "Move up to the root of the edag."))

(defun ext-mate-up ()
  (declare (special nodestack *current-edag*))
  (when (and (ext-exp-open-dag-p *current-edag*)
	     (ext-exp-open-dag-parent-arcs *current-edag*))
    (push *current-edag* nodestack)
    (setq *current-edag* (ext-exp-open-arc-parent
			  (car (ext-exp-open-dag-parent-arcs *current-edag*))))
    *current-edag*))

(defun ext-mate-all-up ()
  (declare (special nodestack *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (do ((node *current-edag*
	       (ext-exp-open-arc-parent
		(car (ext-exp-open-dag-parent-arcs node)))))
	((null (ext-exp-open-dag-parent-arcs node))
	 (push *current-edag* nodestack)
	 (setq *current-edag* node)
	 *current-edag*))))

(defextmate goto
  (extmate-mainfns ext-mate-goto)
  (extmate-argnames node)
  (extmate-argtypes symbol)
  (extmate-arghelp "Name of a node below the current node.")
  (mhelp "Go to a node in the extensional expansion dag."))

(defun ext-mate-goto (node)
  (let ((node1 (eeod-find-node node *current-edag*)))
    (if node1
	(progn
	  (push *current-edag* nodestack)
	  (setq *current-edag* node1)
	  *current-edag*)
      (throwfail "Could not find " node " below " *current-edag*))))

(context ext-exp-dags)

(defextmate add-conn
  (extmate-argnames first second)
  (extmate-argtypes symbol symbol)
  (extmate-arghelp "Name of first node" "Name of second node")
  (extmate-mainfns ext-mate-add-conn)
  (mhelp "Add a connection between two atoms or equations in the edag."))

(defextmate add-conn*
  (extmate-mainfns ext-mate-add-conn*)
  (mhelp "Repeatedly call add-conn"))

(defextmate rem-conn
  (extmate-argnames first second)
  (extmate-argtypes symbol symbol)
  (extmate-arghelp "Name of first node" "Name of second node")
  (extmate-mainfns ext-mate-rem-conn)
  (mhelp "Remove a connection between two atoms or equations in the edag."))

(defextmate rem-conn*
  (extmate-mainfns ext-mate-rem-conn*)
  (mhelp "Repeatedly call rem-conn"))

(defun ext-mate-add-conn (first second)
  (let ((node1 (eeod-find-node first *current-edag*))
	(node2 (eeod-find-node second *current-edag*)))
    (unless node1
      (throwfail "Could not find " first))
    (unless node2
      (throwfail "Could not find " second))
    (unless (member (ext-exp-open-dag-kind node1) '(FLEX ATOM EQNGOAL EQN))
      (throwfail "Cannot connect to "
		 (format nil "~d" (ext-exp-open-dag-kind node1))
		 " node"))
    (unless (member (ext-exp-open-dag-kind node2) '(FLEX ATOM EQNGOAL EQN))
      (throwfail "Cannot connect to "
		 (format nil "~d" (ext-exp-open-dag-kind node2))
		  " node"))
    (cond ((eq (ext-exp-open-dag-kind node1) 'FLEX)
	   (if (eq (ext-exp-open-dag-kind node2) 'FLEX)
	       (throwfail "Cannot mate two flexible nodes." t
			  "Instantiate either "
			  ((head (ext-exp-open-dag-shallow node1)) . gwff)
			  " or  "
			  ((head (ext-exp-open-dag-shallow node2)) . gwff)
			  " first"))
	   (let* ((sh1 (ext-exp-open-dag-shallow node1))
		  (ev (head sh1))
		  (sh2 (ext-exp-open-dag-shallow node2))
		  (h (head sh2)))
	     (when (equal (ext-exp-open-dag-positive node1)
			  (ext-exp-open-dag-positive node2))
	       (let ((evars (eeod-subst-imit ev 'NOT)))
		 (msgf "Instantiating " (ev . gwff) " with "
		       ((get ev 'ext-exp-var-subst) . gwff)
		       " and deepening" t)
		 (let ((neg (get (ext-exp-open-dag-name node1)
				 'ext-exp-open-dag)))
		   (unless (and (ext-exp-open-dag-p neg)
				(eq (ext-exp-open-dag-kind neg) 'NEG)
				(ext-exp-open-dag-arcs neg)
				(ext-exp-open-arc-p (car (ext-exp-open-dag-arcs neg))))
		     (throwfail "Problem deepening negation"))
		   (setq node1 (ext-exp-open-arc-node
				(car (ext-exp-open-dag-arcs neg))))
		   (setq ev (car evars)))))
	     (eeod-subst-imit ev h)
	     (msgf "Instantiating " (ev . gwff) " with "
		   ((get ev 'ext-exp-var-subst) . gwff))
	     (setq node1 (get (ext-exp-open-dag-name node1)
			      'ext-exp-open-dag))
	     (ext-mate-add-conn-1 node1 node2)))
	  ((eq (ext-exp-open-dag-kind node2) 'FLEX)
	   (let* ((sh1 (ext-exp-open-dag-shallow node1))
		  (h (head sh1))
		  (sh2 (ext-exp-open-dag-shallow node2))
		  (ev (head sh2)))
	     (when (equal (ext-exp-open-dag-positive node1)
			  (ext-exp-open-dag-positive node2))
	       (let ((evars (eeod-subst-imit ev 'NOT)))
		 (msgf "Instantiating " (ev . gwff) " with "
		       ((get ev 'ext-exp-var-subst) . gwff)
		       " and deepening" t)
		 (let ((neg (get (ext-exp-open-dag-name node2)
				 'ext-exp-open-dag)))
		   (unless (and (ext-exp-open-dag-p neg)
				(eq (ext-exp-open-dag-kind neg) 'NEG)
				(ext-exp-open-dag-arcs neg)
				(ext-exp-open-arc-p (car (ext-exp-open-dag-arcs neg))))
		     (throwfail "Problem deepening negation"))
		   (setq node2 (ext-exp-open-arc-node
				(car (ext-exp-open-dag-arcs neg))))
		   (setq ev (car evars)))))
	     (eeod-subst-imit ev h)
	     (msgf "Instantiating " (ev . gwff) " with "
		   ((get ev 'ext-exp-var-subst) . gwff))
	     (setq node2 (get (ext-exp-open-dag-name node2)
			      'ext-exp-open-dag))
	     (ext-mate-add-conn-1 node1 node2)))
	  (t (ext-mate-add-conn-1 node1 node2)))
    nil))

(defun ext-mate-add-conn-1 (node1 node2)
  (unless (and (ext-exp-open-dag-p node1)
	       (ext-exp-open-dag-p node2))
    (throwfail "Problem adding connection."))
  (let ((first (ext-exp-open-dag-name node1))
	(second (ext-exp-open-dag-name node2)))
    (when (and (eq (ext-exp-open-dag-kind node1) 'EQN)
	       (not (ext-exp-open-dag-positive node1)))
      (let* ((arcs (ext-exp-open-dag-arcs node1))
	     (arc1 (find-if #'(lambda (arc)
				(eq (ext-exp-open-dag-kind
				     (ext-exp-open-arc-node arc))
				    'EQNGOAL))
			    arcs)))
	(unless arc1
	  (throwfail "Cannot connect to negative equation node " first))
	(setq node1 (ext-exp-open-arc-node arc1))
	(msgf "Connecting to equation goal " (ext-exp-open-dag-name node1) t
	      " instead of negative equation " first t)))
    (when (and (eq (ext-exp-open-dag-kind node2) 'EQN)
	       (not (ext-exp-open-dag-positive node2)))
      (let* ((arcs (ext-exp-open-dag-arcs node2))
	     (arc2 (find-if #'(lambda (arc)
				(eq (ext-exp-open-dag-kind
				     (ext-exp-open-arc-node arc))
				    'EQNGOAL))
			    arcs)))
	(unless arc2
	  (throwfail "Cannot connect to negative equation node " second))
	(setq node2 (ext-exp-open-arc-node arc2))
	(msgf "Connecting to equation goal " (ext-exp-open-dag-name node2) t
	      " instead of negative equation " second t)))
    (unless (and (ext-exp-open-dag-p node1)
		 (ext-exp-open-dag-p node2))
      (throwfail "Problem adding connection."))
    (cond ((equal (ext-exp-open-dag-positive node1)
		  (ext-exp-open-dag-positive node2))
	   (throwfail
	    "Cannot connect nodes " (ext-exp-open-dag-name node1) " and "
	    (ext-exp-open-dag-name node2) t
	    "since they have the same polarity."))
	  ((eq (ext-exp-open-dag-kind node1) 'ATOM)
	   (if (eq (ext-exp-open-dag-kind node2) 'ATOM)
	       (let ((mate
		      (if (ext-exp-open-dag-positive node1)
			  (eeod-mate node1 node2)
			(eeod-mate node2 node1))))
		 (when (ext-exp-open-dag-p mate)
		   (ext-mate-recompute-jforms)
		   (ext-mate-print-new-info mate)))
	     (throwfail "Cannot connect atom " (ext-exp-open-dag-name node1)
			" with "
			(format nil "~d" (ext-exp-open-dag-kind node2))
			(ext-exp-open-dag-name node2))))
	  ((eq (ext-exp-open-dag-kind node1) 'EQN)
	   (if (eq (ext-exp-open-dag-kind node2) 'EQNGOAL)
	       (progn
		 (if (query "Reverse Equation (EUNIF2)?" nil)
		     (eeod-eunif2 node1 node2)
		   (eeod-eunif1 node1 node2))
		 (ext-mate-recompute-jforms))
	     (throwfail "Cannot connect eqn " (ext-exp-open-dag-name node1)
			" with "
			(format nil "~d" (ext-exp-open-dag-kind node2))
			(ext-exp-open-dag-name node2))))
	  ((eq (ext-exp-open-dag-kind node1) 'EQNGOAL)
	   (if (eq (ext-exp-open-dag-kind node2) 'EQN)
	       (progn
		 (if (query "Reverse Equation (EUNIF2)?" nil)
		     (eeod-eunif2 node2 node1)
		   (eeod-eunif1 node2 node1))
		 (ext-mate-recompute-jforms))
	     (throwfail "Cannot connect eqn " (ext-exp-open-dag-name node1)
			" with "
			(format nil "~d" (ext-exp-open-dag-kind node2))
			(ext-exp-open-dag-name node2))))
	  (t
	   (throwfail "Cannot connect " (ext-exp-open-dag-name node1)
		      " with " (ext-exp-open-dag-name node2))))))

(defun ext-mate-print-new-info (node)
  (let ((atoms (eeod-get-nodes
		#'(lambda (x)
		    (eq (ext-exp-open-dag-kind x) 'ATOM))
		node))
	(flexs (eeod-get-nodes
		#'(lambda (x)
		    (eq (ext-exp-open-dag-kind x) 'FLEX))
		node))
	(pos-eqns (eeod-get-nodes
		   #'(lambda (x)
		       (and (eq (ext-exp-open-dag-kind x) 'EQN)
			    (ext-exp-open-dag-positive x)))
		   node))
	(eqngoals (eeod-get-nodes
		   #'(lambda (x)
		       (eq (ext-exp-open-dag-kind x) 'EQNGOAL))
		   node)))
    (when atoms
      (terpri) (princ "New Atoms:   ") (terpri)
      (dolist (n atoms)
	(msgf (ext-exp-open-dag-name n) " : "
	      ((ext-exp-open-dag-shallow n) . gwff) t)))
    (when flexs
      (terpri) (princ "New Flexible:   ") (terpri)
      (dolist (n flexs)
	(msgf (ext-exp-open-dag-name n) " : "
	      ((ext-exp-open-dag-shallow n) . gwff) t)))
    (when pos-eqns
      (terpri) (princ "New Positive Equations:   ") (terpri)
      (dolist (n pos-eqns)
	(msgf (ext-exp-open-dag-name n) " : "
	      ((ext-exp-open-dag-shallow n) . gwff) t)))
    (when eqngoals
      (terpri) (princ "New Equation Goals:   ") (terpri)
      (dolist (n eqngoals)
	(msgf (ext-exp-open-dag-name n) " : "
	      ((ext-exp-open-dag-shallow n) . gwff) t)))))

(defun ext-mate-rem-conn (first second)
  (let ((node1 (eeod-find-node first *current-edag*))
	(node2 (eeod-find-node second *current-edag*)))
    (unless node1
      (throwfail "Could not find " first))
    (unless node2
      (throwfail "Could not find " second))
    (unless (member (ext-exp-open-dag-kind node1) '(ATOM EQNGOAL EQN))
      (throwfail "No connection to " first))
    (unless (member (ext-exp-open-dag-kind node2) '(ATOM EQNGOAL EQN))
      (throwfail "No connection to " second))
    (when (and (eq (ext-exp-open-dag-kind node1) 'EQN)
	       (not (ext-exp-open-dag-positive node1)))
      (let* ((arcs (ext-exp-open-dag-arcs node1))
	     (arc1 (find-if #'(lambda (arc)
				(eq (ext-exp-open-dag-kind
				     (ext-exp-open-arc-node arc))
				    'EQNGOAL))
			    arcs)))
	(unless arc1
	  (throwfail "Problem with negative equation " first))
	(setq node1 (ext-exp-open-arc-node arc1))))
    (when (and (eq (ext-exp-open-dag-kind node2) 'EQN)
	       (not (ext-exp-open-dag-positive node2)))
      (let* ((arcs (ext-exp-open-dag-arcs node2))
	     (arc2 (find-if #'(lambda (arc)
				(eq (ext-exp-open-dag-kind
				     (ext-exp-open-arc-node arc))
				    'EQNGOAL))
			    arcs)))
	(unless arc2
	  (throwfail "Problem with negative equation " second))
	(setq node2 (ext-exp-open-arc-node arc2))))
    (cond ((and (eq (ext-exp-open-dag-kind node1) 'ATOM)
		(eq (ext-exp-open-dag-kind node2) 'ATOM))
	   (let* ((arcs1 (ext-exp-open-dag-arcs node1))
		  (arcs2 (ext-exp-open-dag-arcs node2))
		  (kids1 (mapcar #'(lambda (arc)
				     (ext-exp-open-arc-node arc))
				 arcs1))
		  (kids2 (mapcar #'(lambda (arc)
				     (ext-exp-open-arc-node arc))
				 arcs2))
		  (kid (car (intersection kids1 kids2))))
	     (if kid
		 (ext-mate-rem-conn-1 node1 node2 kid)
	       (msgf "No Connection between " first " and " second t))))
	  ((or (and (eq (ext-exp-open-dag-kind node1) 'EQN)
		    (eq (ext-exp-open-dag-kind node2) 'EQNGOAL))
	       (and (eq (ext-exp-open-dag-kind node1) 'EQNGOAL)
		    (eq (ext-exp-open-dag-kind node2) 'EQN)))
	   (let* ((arcs1 (ext-exp-open-dag-arcs node1))
		  (arcs2 (ext-exp-open-dag-arcs node2))
		  (arcs1-1 (remove-if-not #'(lambda (arc)
					      (eq (ext-exp-open-arc-kind arc)
						  'EUNIF1))
					  arcs1))
		  (arcs1-2 (remove-if-not #'(lambda (arc)
					      (eq (ext-exp-open-arc-kind arc)
						  'EUNIF2))
					  arcs1))
		  (arcs2-1 (remove-if-not #'(lambda (arc)
					      (eq (ext-exp-open-arc-kind arc)
						  'EUNIF1))
					  arcs2))
		  (arcs2-2 (remove-if-not #'(lambda (arc)
					      (eq (ext-exp-open-arc-kind arc)
						  'EUNIF2))
					  arcs2))
		  (kids1-1 (mapcar #'(lambda (arc)
				       (ext-exp-open-arc-node arc))
				   arcs1-1))
		  (kids1-2 (mapcar #'(lambda (arc)
				       (ext-exp-open-arc-node arc))
				   arcs1-2))
		  (kids2-1 (mapcar #'(lambda (arc)
				       (ext-exp-open-arc-node arc))
				   arcs2-1))
		  (kids2-2 (mapcar #'(lambda (arc)
				       (ext-exp-open-arc-node arc))
				   arcs2-2))
		  (eunif1 (car (intersection kids1-1 kids2-1)))
		  (eunif2 (car (intersection kids1-2 kids2-2))))
	     (if eunif1
		 (if eunif2
		     (progn
		       (when (query "Delete EUnif1" t)
			 (ext-mate-rem-conn-1 node1 node2 eunif1))
		       (when (query "Delete EUnif2" t)
			 (ext-mate-rem-conn-1 node1 node2 eunif2)))
		   (ext-mate-rem-conn-1 node1 node2 eunif1))
	       (if eunif2
		   (ext-mate-rem-conn-1 node1 node2 eunif2)
		 (msgf "No Connection between " first " and " second t)))))
	  (t
	   (msgf "No Connection between " first " and " second t)))
    nil))

(defun ext-mate-rem-conn-1 (node1 node2 kid)
  (setf (ext-exp-open-dag-arcs node1)
	(remove-if #'(lambda (arc)
		       (eq (ext-exp-open-arc-node arc) kid))
		   (ext-exp-open-dag-arcs node1)))
  (setf (ext-exp-open-dag-arcs node2)
	(remove-if #'(lambda (arc)
		       (eq (ext-exp-open-arc-node arc) kid))
		   (ext-exp-open-dag-arcs node2)))
  (ext-mate-recompute-jforms))

(defun ext-mate-add-conn* ()
  (eval (ext-mate-command-interpreter  '(add-conn)))
  (msg t)
  (let (again) 
    (let ((EXT-MATE-RECOMPUTE-JFORMS NIL))
      (declare (special EXT-MATE-RECOMPUTE-JFORMS))
      (prompt-read again nil (msgf "Add another connection? ") 'yesno 'yes nil)
      (do () ((not again))
	(msg t)
	(eval (ext-mate-command-interpreter '(add-conn)))
	(msg t t)
	(prompt-read again nil (msgf "Add another connection? ") 'yesno 'yes nil)))
    (ext-mate-recompute-jforms)))

(defun ext-mate-rem-conn* ()
  (eval (ext-mate-command-interpreter  '(rem-conn)))
  (msg t)
  (let (again) 
    (let ((EXT-MATE-RECOMPUTE-JFORMS NIL))
      (declare (special EXT-MATE-RECOMPUTE-JFORMS))
      (prompt-read again nil (msgf "Remove another connection? ") 'yesno 'yes nil)
      (do () ((not again))
	(msg t)
	(eval (ext-mate-command-interpreter '(rem-conn)))
	(msg t t)
	(prompt-read again nil (msgf "Remove another connection? ") 'yesno 'yes nil)))
    (ext-mate-recompute-jforms)))

(defun ext-mate-recompute-jforms ()
  (when EXT-MATE-RECOMPUTE-JFORMS
    (msgf "Recomputing JForm")
    (ext-mate-cjform t t t)))

(context jforms1)

(defextmate cjform
  (extmate-argnames posflex negflex flexflex)
  (extmate-argtypes boolean boolean boolean)
  (extmate-arghelp "Include Positive Flex Lits"
		   "Include Negative Flex Lits"
		   "Include FlexFlex Lits")
  (extmate-defaultfns (lambda (x y z)
			(list (if (eq x '$) t x)
			      (if (eq y '$) t y)
			      (if (eq z '$) t z))))
  (extmate-mainfns ext-mate-cjform)
  (mhelp "Create (or update) for the edag.  You can choose to
leave out positive and/or negative flexible literals.
You can also choose to leave out flex/flex equation goals."))

(defextmate vp
  (extmate-mainfns ext-mate-vp)
  (mhelp "Print the jform for the edag as a VP diagram."))

(defextmate vpd
  (extmate-mainfns ext-mate-vpd)
  (mhelp "Save the jform for the edag as a VP diagram in a file
The variables VPD-FILENAME, VPD-STYLE, VPD-PTYPES, VPD-BRIEF and VPD-VPFPAGE
control this."))

(defextmate num-hpaths
  (extmate-mainfns ext-mate-num-hpaths)
  (mhelp "Print the number of horizontal paths in the jform for the edag."))

(defextmate num-vpaths
  (extmate-mainfns ext-mate-num-vpaths)
  (mhelp "Print the number of vertical paths in the jform for the edag."))

(defun ext-mate-cjform (posflex negflex flexflex)
  (when (ext-exp-open-dag-p *current-edag*)
    (setq *current-edag-jform*
	  (eeod-to-jform *current-edag*
			 :posflex posflex :negflex negflex
			 :flexflex flexflex)))
  nil)

(defun ext-mate-num-hpaths ()
  (when *current-edag-jform*
    (number-of-horizontal-paths *current-edag-jform*)))

(defun ext-mate-num-vpaths ()
  (when *current-edag-jform*
    (number-of-vertical-paths *current-edag-jform*)))

(defun ext-mate-vp ()
  (when *current-edag-jform*
    (display-vp-diag *current-edag-jform*))
  nil)

(defun ext-mate-vpd ()
  (when *current-edag-jform*
    (display-vpd *current-edag-jform*))
  nil)

(context etr-nat)

(defextmate merge-tree
  (extmate-mainfns ext-mate-merge-tree)
  (mhelp "Merge a complete edag."))

(defun ext-mate-merge-tree ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (ext-exp-open-dag-complete *current-edag*)
      (throwfail "EDag is not complete."))
    (eeod-solve-flex-flex)
    (setq *merged-edag* (eeod-to-eed-node *current-edag*
					  (ext-exp-open-dag-shallow
					   *current-edag*))))
  nil)

(defextmate etree-nat
  (extmate-argtypes symbol line tactic-exp tactic-mode)
  (extmate-argnames prefix num tac mode)
  (extmate-arghelp "Name of the Proof" "Line Number for Theorem" "Tactic to be Used" "Tactic Mode")
  (extmate-defaultfns (lambda (x y z w)
			(list
			 (if (eq '$ x) dproof x)
			 (if (eq '$ y) 100 y)
			 (if (eq '$ z) default-tactic z)
			 (if (eq '$ w) 'auto w))))
  (extmate-mainfns ext-mate-etree-nat)
  (mhelp "Translate a complete edag proof into natural deduction."))

(defun ext-mate-etree-nat (prefix num tac mode)
  (declare (special *merged-edag* *current-edag*))
  (unless (ext-exp-dag-p *merged-edag*)
    (when (ext-exp-open-dag-p *current-edag*)
      (unless (ext-exp-open-dag-complete *current-edag*)
	(throwfail "EDag is not complete."))
      (when (query "Merge the EDag?" t)
	(eeod-solve-flex-flex)
	(setq *merged-edag* (eeod-to-eed-node *current-edag*
					      (ext-exp-open-dag-shallow
					       *current-edag*))))))
  (unless (ext-exp-dag-p *merged-edag*)
    (throwfail "Must have a complete, merged EDag to translate."))
  (unwind-protect
   (ext-mate-etree-nat-real prefix num tac mode *merged-edag*)
   (progn (breakcount 'eproof) (display-time 'eproof))))

(defun ext-mate-etree-nat-real (prefix num tac mode eed)
  (startcount 'eproof)
  (when (and (not tac) (neq query-user T))
    (setq tac default-tactic))
  (unless tac
    (prompt-read tac nil (msgf "What tactic should be used for translation?")
		 'tactic-exp default-tactic nil))
  (when (and (not mode) (neq query-user T))
    (setq mode 'auto))
  (unless mode
    (prompt-read mode nil (msgf "Tactic mode?")
		 'tactic-mode 'auto ((? (mhelp 'tactic-mode)))))
  (prove2 (ext-exp-dag-shallow eed) prefix num)
  (let* ((plans (proof-plans dproof))
	 (line (caar plans)))
    (ext-exp-pf-to-nd eed line)))

(context ext-search)

(defextmate ms03-lift
  (extmate-mainfns ms03-lift)
  (mhelp "Use lifting to guide the search for a proof using diy
with default-ms MS03-7.  If successful, values are suggested for many
relevant flags in the subject MS03-7.

Setting QUERY-USER to T allows the user more control over lifting.

See Also: LIST MS03-7"))

(defextmate ms04-lift
  (extmate-mainfns ms04-lift)
  (mhelp "Use lifting to guide the search for a proof using diy
with default-ms MS04-2.  If successful, values are suggested for many
relevant flags in the subject MS04-2.

Setting QUERY-USER to T allows the user more control over lifting.

See Also: LIST MS04-2"))

(defun ms03-lift ()
  (declare (special *merged-edag* *current-edag*))
  (unless (ext-exp-dag-p *merged-edag*)
    (when (ext-exp-open-dag-p *current-edag*)
      (unless (ext-exp-open-dag-complete *current-edag*)
	(throwfail "EDag is not complete."))
      (when (query "Merge the EDag?" t)
	(eeod-solve-flex-flex)
	(setq *merged-edag* (eeod-to-eed-node *current-edag*
					      (ext-exp-open-dag-shallow
					       *current-edag*))))))
  (unless (ext-exp-dag-p *merged-edag*)
    (throwfail "Must have a complete, merged EDag to lift."))
  (lift-eed-to-saturation *merged-edag*))

(defun ms04-lift ()
  (declare (special *merged-edag* *current-edag*))
  (unless (ext-exp-dag-p *merged-edag*)
    (when (ext-exp-open-dag-p *current-edag*)
      (unless (ext-exp-open-dag-complete *current-edag*)
	(throwfail "EDag is not complete."))
      (when (query "Merge the EDag?" t)
	(eeod-solve-flex-flex)
	(setq *merged-edag* (eeod-to-eed-node *current-edag*
					      (ext-exp-open-dag-shallow
					       *current-edag*))))))
  (unless (ext-exp-dag-p *merged-edag*)
    (throwfail "Must have a complete, merged EDag to lift."))
  (lift-eed-to-ms04 *merged-edag*))

(context ext-exp-dags)

(defextmate dup-var
  (extmate-argtypes gvar)
  (extmate-argnames evar)
  (extmate-arghelp "Expansion Variable")
  (extmate-defaultfns (lambda (evar)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (eeod-exp-vars *current-edag*)))
				    (if evars
					(car evars)
				      evar))
				evar))))
  (extmate-mainfns extmate-dup-var)
  (mhelp "Duplicate an expansion var in an expansion dag."))

(defextmate dup-node
  (extmate-argtypes symbol)
  (extmate-argnames expnode)
  (extmate-arghelp "Name of Expansion Node")
  (extmate-defaultfns (lambda (expnode)
			(list (if (and (eq expnode '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((exps (eeod-get-nodes
					       #'(lambda (x) (eq (ext-exp-open-dag-kind x) 'EXP)) *current-edag*)))
				    (if exps
					(ext-exp-open-dag-name (car exps))
				      expnode))
				expnode))))
  (extmate-mainfns extmate-dup-node)
  (mhelp "Create a new expansion arc from an expansion node in an expansion dag."))

(defextmate imitate
  (extmate-argtypes gvar lvarconst)
  (extmate-argnames evar head)
  (extmate-arghelp "Expansion Var" "Head for Imitation")
  (extmate-defaultfns (lambda (evar head)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (eeod-exp-vars *current-edag*)))
				    (if evars
					(car evars)
				      evar))
				evar)
			      head)))
  (extmate-mainfns extmate-imitate)
  (mhelp "Substitute a general imitation term for the original var."))

(defextmate dup-and-imitate
  (extmate-argtypes gvar lvarconst)
  (extmate-argnames evar head)
  (extmate-arghelp "Expansion Var" "Head for Imitation")
  (extmate-defaultfns (lambda (evar head)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (eeod-exp-vars *current-edag*)))
				    (if evars
					(car evars)
				      evar))
				evar)
			      head)))
  (extmate-mainfns extmate-dup-and-imitate)
  (mhelp "Duplicate an expansion var and substitute a general imitation term for the original var."))

(defextmate expand-imitate
  (extmate-argtypes gvar lvarconst)
  (extmate-argnames evar head)
  (extmate-arghelp "Expansion Var" "Head for Imitation")
  (extmate-defaultfns (lambda (evar head)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (eeod-exp-vars *current-edag*)))
				    (if evars
					(car evars)
				      evar))
				evar)
			      head)))
  (extmate-mainfns extmate-expand-imitate)
  (mhelp "Given an expansion variable x(A) and a head H (appropriate
for an imitation term of type A), let H' be the general imitation term
for H of type x.  For every expansion arc with expansion term t
containing the given expansion variable x, add a new expansion arc
using expansion term [H'/x]t."))

(defextmate project
  (extmate-argtypes gvar integer+)
  (extmate-argnames evar argnum)
  (extmate-arghelp "Expansion Var" "Number Indicating Argument to Project")
  (extmate-defaultfns (lambda (evar argnum)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'ext-possible-var-projections
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq argnum '$) evar (not (eq evar '$)) (symbolp evar))
				  (let ((p (ext-possible-var-projections evar)))
				    (if p
					(car p)
				      0))
				argnum))))
  (extmate-mainfns extmate-project)
  (mhelp "Substitute a general projection term for the original var."))

(defextmate dup-and-project
  (extmate-argtypes gvar integer+)
  (extmate-argnames evar argnum)
  (extmate-arghelp "Expansion Var" "Number Indicating Argument to Project")
  (extmate-defaultfns (lambda (evar argnum)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'ext-possible-var-projections
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq argnum '$) evar (not (eq evar '$)) (symbolp evar))
				  (let ((p (ext-possible-var-projections evar)))
				    (if p
					(car p)
				      0))
				argnum))))
  (extmate-mainfns extmate-dup-and-project)
  (mhelp "Duplicate an expansion var and substitute a general projection term for the original var."))

(defextmate expand-project
  (extmate-argtypes gvar integer+)
  (extmate-argnames evar argnum)
  (extmate-arghelp "Expansion Var" "Number Indicating Argument to Project")
  (extmate-defaultfns (lambda (evar argnum)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'ext-possible-var-projections
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq argnum '$) evar (not (eq evar '$)) (symbolp evar))
				  (let ((p (ext-possible-var-projections evar)))
				    (if p
					(car p)
				      0))
				argnum))))
  (extmate-mainfns extmate-expand-project)
  (mhelp "Given an expansion variable x(A) and integer i (appropriate
for a projection term of type A), let p be the i'th projection term
for type A.  For every expansion arc with expansion term t containing
the given expansion variable x, add a new expansion arc using
expansion term [p/x]t."))

(defextmate subst-forall
  (extmate-argtypes gvar typesym)
  (extmate-argnames evar tp)
  (extmate-arghelp "Expansion Var" "Type for Quantified Variable")
  (extmate-defaultfns (lambda (evar tp)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'(lambda (ev)
						    (eq (inmost-car (unabbreviated-type ev)) 'O))
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq tp '$) *individual-types*)
				  (car *individual-types*)
				tp))))
  (extmate-mainfns extmate-subst-forall)
  (mhelp "Substitute a primsub with a universal quantifier for the original var."))

(defextmate dup-and-subst-forall
  (extmate-argtypes gvar typesym)
  (extmate-argnames evar tp)
  (extmate-arghelp "Expansion Var" "Type for Quantified Variable")
  (extmate-defaultfns (lambda (evar tp)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'(lambda (ev)
						    (eq (inmost-car (unabbreviated-type ev)) 'O))
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq tp '$) *individual-types*)
				  (car *individual-types*)
				tp))))
  (extmate-mainfns extmate-dup-and-subst-forall)
  (mhelp "Duplicate an expansion var and substitute a primsub with a universal quantifier for the original var."))

(defextmate expand-forall
  (extmate-argtypes gvar typesym)
  (extmate-argnames evar tp)
  (extmate-arghelp "Expansion Var" "Type for Quantified Variable")
  (extmate-defaultfns (lambda (evar tp)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'(lambda (ev)
						    (eq (inmost-car (unabbreviated-type ev)) 'O))
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq tp '$) *individual-types*)
				  (car *individual-types*)
				tp))))
  (extmate-mainfns extmate-expand-forall)
  (mhelp "Given an expansion variable x(A) and a variable y of type B,
let p be the primsub using forall of type B.  For every expansion arc
with expansion term t containing the given expansion variable x, add a
new expansion arc using expansion term [p/x]t."))

(defextmate expand-exists
  (extmate-argtypes gvar typesym)
  (extmate-argnames evar tp)
  (extmate-arghelp "Expansion Var" "Type for Quantified Variable")
  (extmate-defaultfns (lambda (evar tp)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'(lambda (ev)
						    (eq (inmost-car (unabbreviated-type ev)) 'O))
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq tp '$) *individual-types*)
				  (car *individual-types*)
				tp))))
  (extmate-mainfns extmate-expand-exists)
  (mhelp "Given an expansion variable x(A) and a variable y of type B,
let p be the primsub using forall of type B.  For every expansion arc
with expansion term t containing the given expansion variable x, add a
new expansion arc using expansion term [p/x]t."))

(defextmate subst-exists
  (extmate-argtypes gvar typesym)
  (extmate-argnames evar tp)
  (extmate-arghelp "Expansion Var" "Type for Quantified Variable")
  (extmate-defaultfns (lambda (evar tp)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'(lambda (ev)
						    (eq (inmost-car (unabbreviated-type ev)) 'O))
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq tp '$) *individual-types*)
				  (car *individual-types*)
				tp))))
  (extmate-mainfns extmate-subst-exists)
  (mhelp "Substitute a primsub with an existential quantifier for the original var."))

(defextmate dup-and-subst-exists
  (extmate-argtypes gvar typesym)
  (extmate-argnames evar tp)
  (extmate-arghelp "Expansion Var" "Type for Quantified Variable")
  (extmate-defaultfns (lambda (evar tp)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (remove-if-not
						#'(lambda (ev)
						    (eq (inmost-car (unabbreviated-type ev)) 'O))
						(eeod-exp-vars *current-edag*))))
				    (if evars
					(car evars)
				      evar))
				evar)
			      (if (and (eq tp '$) *individual-types*)
				  (car *individual-types*)
				tp))))
  (extmate-mainfns extmate-dup-and-subst-exists)
  (mhelp "Duplicate an expansion var and substitute a primsub with an existential quantifier for the original var."))

(defextmate subst
  (extmate-argtypes gvar gwff)
  (extmate-argnames evar wff)
  (extmate-arghelp "Expansion Var" "Term to Substitute")
  (extmate-defaultfns (lambda (evar wff)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (eeod-exp-vars *current-edag*)))
				    (if evars
					(car evars)
				      evar))
				evar)
			      wff)))
  (extmate-mainfns extmate-subst)
  (mhelp "Substitute a term for an expansion var in an expansion dag."))

(defextmate expand-subst
  (extmate-argtypes gvar gwff)
  (extmate-argnames evar wff)
  (extmate-arghelp "Expansion Var" "Term to Substitute")
  (extmate-defaultfns (lambda (evar wff)
			(list (if (and (eq evar '$) (ext-exp-open-dag-p *current-edag*))
				  (let ((evars (eeod-exp-vars *current-edag*)))
				    (if evars
					(car evars)
				      evar))
				evar)
			      wff)))
  (extmate-mainfns extmate-subst)
  (mhelp "Given an expansion variable x(A) and a wff W(A), for every
expansion arc with expansion term t containing the given expansion
variable x, add a new expansion arc using expansion term [W/x]t.  (The
free variables of W are not considered new expansion variables.)"))

(context wff-printing)
(defextmate show-exp-terms
  (extmate-mainfns extmate-show-exp-terms)
  (mhelp "Show expansion terms in expansion dag."))

(defextmate show-exp-vars
  (extmate-mainfns extmate-show-exp-vars)
  (mhelp "Show expansion vars in expansion dag."))

(defextmate show-sel-vars
  (extmate-mainfns extmate-show-sel-vars)
  (mhelp "Show selection vars in expansion dag."))

(defun extmate-dup-var (evar)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (eeod-duplicate-expvar evar)
    (ext-mate-recompute-jforms)))

(defun extmate-dup-node (expnode)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (let ((e (eeod-get-nodes #'(lambda (x) (eq (ext-exp-open-dag-name x) expnode)) *current-edag* nil)))
      (unless (and (ext-exp-open-dag-p e)
		   (eq (ext-exp-open-dag-kind e) 'EXP))
	(throwfail "Cannot find expansion node " expnode))
      (eeod-duplicate-expnode e)
      (ext-mate-recompute-jforms))))

(defun extmate-dup-and-imitate (evar head)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (when (member head (get evar 'banned-sel-vars))
      (throwfail "Sel Var " (head . gwff) " is banned for Exp Var " (evar . gwff)))
    (when (member head (get evar 'banned-imitations))
      (throwfail "Head " (head . gwff) " is banned for Exp Var " (evar . gwff) " (already used)"))
    (eeod-duplicate-and-imitate evar head)
    (ext-mate-recompute-jforms)))

(defun extmate-dup-and-project (evar argnum)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (when (member argnum (get evar 'banned-projs) :test #'equal)
      (throwfail "Projection " argnum " is banned for Exp Var " (evar . gwff) " (already used)"))
    (unless (member argnum (ext-possible-projections (unabbreviated-type evar)) :test #'equal)
      (throwfail "Projection " argnum " is impossible for type of Exp Var " (evar . gwff)))
    (eeod-duplicate-and-project evar argnum)
    (ext-mate-recompute-jforms)))

(defun extmate-dup-and-subst-forall (evar tp)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (eeod-duplicate-and-subst-forall evar tp)
    (ext-mate-recompute-jforms)))

(defun extmate-dup-and-subst-exists (evar tp)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (eeod-duplicate-and-subst-exist evar tp)
    (ext-mate-recompute-jforms)))

(defun extmate-imitate (evar head)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (when (member head (get evar 'banned-sel-vars))
      (throwfail "Sel Var " (head . gwff) " is banned for Exp Var " (evar . gwff)))
    (when (member head (get evar 'banned-imitations))
      (throwfail "Head " (head . gwff) " is banned for Exp Var " (evar . gwff) " (already used)"))
    (eeod-subst-imit evar head)
    (ext-mate-recompute-jforms)))

(defun extmate-project (evar argnum)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (when (member argnum (get evar 'banned-projs) :test #'equal)
      (throwfail "Projection " argnum " is banned for Exp Var " (evar . gwff) " (already used)"))
    (unless (member argnum (ext-possible-projections (unabbreviated-type evar)) :test #'equal)
      (throwfail "Projection " argnum " is impossible for type of Exp Var " (evar . gwff)))
    (eeod-subst-proj evar argnum)
    (ext-mate-recompute-jforms)))

(defun extmate-subst-forall (evar tp)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (eeod-subst-forall evar tp)
    (ext-mate-recompute-jforms)))

(defun extmate-subst-exists (evar tp)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (eeod-subst-exists evar tp)
    (ext-mate-recompute-jforms)))

(defun eeod-expand-var (ev wff evl)
  (runcount 'unification)
  (dolist (arc (get ev 'ext-exp-var-arcs))
    (let* ((expnode (ext-exp-open-arc-parent arc))
	   (kid (ext-exp-open-arc-node arc))
	   (trm1 (ext-exp-open-arc-exp-term arc))
	   (evars1 (remove-if #'(lambda (x)
				  (or (not (ext-exp-var-p x))
				      (eq x ev)))
			      (free-vars-of trm1)))
	   (wff1 (ext-exp-open-dag-shallow kid))
	   (wff2 (substitute-l-term-var wff ev wff1))
	   (newarc (make-ext-exp-open-arc :parent expnode :kind 'EXP
					  :exp-term (etanorm (lambda-norm (substitute-l-term-var wff ev trm1)))))
	   (newkid
	    (create-simple-ext-exp-open-dag wff2 (ext-exp-open-dag-positive expnode) (list newarc)))) ; don't start with any exp arcs or dec arcs
      (push newarc (ext-exp-open-dag-arcs expnode))
      (setf (ext-exp-open-arc-node newarc) newkid)
      (dolist (ev0 (append evars1 evl))
	(push newarc (get ev0 'ext-exp-var-arcs)))
      (ext-mate-recompute-jforms)))
  (breakcount 'unification))

(defun extmate-expand-imitate (evar head)
  (multiple-value-bind
      (wff evars)
      (create-imit-subst evar head)
    (eeod-expand-var evar wff evars)))

(defun extmate-expand-project (evar argnum)
  (multiple-value-bind
      (wff evars)
      (create-proj-subst evar argnum)
    (eeod-expand-var evar wff evars)))

(defun extmate-expand-forall (evar tp)
  (multiple-value-bind
      (wff evars)
      (create-quant-subst evar tp 'FORALL)
    (eeod-expand-var evar wff evars)))

(defun extmate-expand-exists (evar tp)
  (multiple-value-bind
      (wff evars)
      (create-quant-subst evar tp 'EXISTS)
    (eeod-expand-var evar wff evars)))

(defun extmate-expand-subst (evar wff)
  (eeod-expand-var evar wff))

(defun extmate-subst (evar wff)
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (unless (member evar (eeod-exp-vars *current-edag*))
      (throwfail (evar . gwff) " is not an expansion var in edag."))
    (eeod-subst-deepen (acons evar wff nil) *current-edag*)
    (ext-mate-recompute-jforms)))

(defun extmate-show-exp-terms ()
  (declare (special *current-edag*))
  (extmate-show-exp-terms-1 *current-edag*))

(defun extmate-show-exp-terms-1 (edag)
  (when (ext-exp-open-dag-p edag)
    (dolist (exp (eeod-get-nodes #'(lambda (x) (eq (ext-exp-open-dag-kind x) 'EXP)) edag))
      (let ((arcs (ext-exp-open-dag-arcs exp)))
	(when arcs
	  (msgf (ext-exp-open-dag-name exp) " :")
	  (dolist (arc arcs)
	    (when (ext-exp-open-arc-exp-term arc)
	      (msgf ((ext-exp-open-arc-exp-term arc) . gwff)))))))))

(defun extmate-show-exp-vars ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (let ((evars (eeod-exp-vars *current-edag*)))
      (if evars
	  (progn
	    (msgf "Expansion Vars :")
	    (dolist (ev evars)
	      (msg " " (ev . gwff))))
	(msgf "No Expansion Vars.")))))

(defun extmate-show-sel-vars ()
  (declare (special *current-edag*))
  (when (ext-exp-open-dag-p *current-edag*)
    (let ((svars (eeod-sel-vars *current-edag*)))
      (if svars
	  (progn
	    (msgf "Selected Vars :")
	    (dolist (sv svars)
	      (msg " " (sv . gwff))))
	(msgf "No Selected Vars.")))))

