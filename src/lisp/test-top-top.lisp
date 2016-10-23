;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;;;
;;; File TEST-TOP-TOP
;;;
;;; defines main fns of the test-top level.
;;;

(deffile test-top-top
  (part-of mating)
  (extension lisp)
  (mhelp "Defines top-level for test-top."))

(context subtoplevels)

(deftoplevel test-top
  (top-prompt-fn test-top-prompt)
  (command-interpreter test-command-interpreter)
  (print-* test-print-*)
  (top-level-category testcmd)
  (top-level-ctree test-command-ctree)
  (top-cmd-decode test-opdecode)
  (mhelp "The TEST-TOP top level."))

(eval-when (load compile eval)
(defcategory testcmd
  (define deftest)
  (properties
    (test-argtypes multiple)
    (test-argnames multiple)
    (test-arghelp multiple)
    (test-defaultfns multiplefns)
    (test-mainfns singlefn)
    (mhelp single))
  (global-list global-testlist)
  (shadow t)
  (mhelp-line "test-top command")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command
	(format nil "@IndexOther(~A)" (symbol-name item))
	(get item 'test-argnames)
	(cdr (assoc 'testcmd (get item 'mhelp))))))
  (mhelp-fn (testcmd-mhelp testcmd category)))
)

(defun testcmd-mhelp (keyword category)
  (declare (special short-help))
  (princ-mhelp keyword category)
  (unless short-help 
    (when *doing-html* (msg " fnord "))
    (msgf "The command format for " keyword " is:" -2 "<TEST>" keyword)
	  (print-tps-format* keyword " "
			     (+ 5 (length (format nil "~A" keyword)))
			     (get keyword 'test-argnames)
			     (get keyword 'test-argtypes)
			     nil)))

(defun testtop ()
  (let ((top-prompt-fn #'test-top-prompt)
	(command-interpreter #'test-command-interpreter)
	(print-* #'test-print-*)
	(top-level 'test-top)
	;;(command-ctree test-command-ctree)
	)
    (declare (special top-prompt-fn command-interpreter print-* top-level
		      command-ctree))
    (secondary-top)))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun test-top-prompt (id) (format nil "<test~A>" id))

(defun test-print-* (result) (fresh-line) (prin1 result))

(defun test-command-interpreter (cmd)
  (declare (special expertflag))
  (setq *test-top* t)
  (cond ((null cmd) nil)
	((and (null (cdr cmd)) (atom (car cmd)))
	 (cond
	  ((get (car cmd) 'testcmd)
	   `(test-opdecode (quote ,cmd)))
	  ((get (car cmd) 'mateop)
	   `(mate-opdecode (quote ,cmd)))
	  ((get (car cmd) 'reviewcmd)
	   `(comdecode (quote ,cmd)))
	  ((get (car cmd) 'mexpr)
	   `(comdecode (quote ,cmd)))
	  ((get (car cmd) 'flag)
	   `(comdecode '(setflag ,@cmd)))
	  ((null expertflag)
	   (throwfail "Unknown Command or Flag."))
	  ((boundp (car cmd)) (car cmd))
	  ((or (get (car cmd) 'mhelp) (get (car cmd) 'mhelp-fn))
	   (msg "Cannot evaluate that... calling HELP " (car cmd) t t)
	   `(comdecode '(help ,(car cmd))))
	  ((not (fboundp (car cmd)))
	   (throwfail ";" (car cmd) " - Unbound variable."))
	  (t cmd)))
	((and expertflag (null (cdr cmd))) (car cmd))
	((get (car cmd) 'testcmd)
	 `(test-opdecode (quote ,cmd)))
	((get (car cmd) 'reviewcmd)
	 `(comdecode (quote ,cmd)))
	((get (car cmd) 'mexpr) `(comdecode (quote ,cmd)))
	((get (car cmd) 'flag)
	 (if (cdr cmd) `(comdecode '(set ,@cmd))
	   `(comdecode '(setflag ,@cmd))))
	((null expertflag)
	 (throwfail "Unknown command."))
	(t cmd)))

(defun test-opdecode (command)
  (let ((keyword (car command))
	mainfn result)
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values keyword
		       (copy (cdr command))
		       (get keyword 'test-argtypes)
		       (mapcar #'(lambda (x) (declare (ignore x)) nil)
			       (get keyword 'test-argtypes))
		       nil
		       (get keyword 'test-defaultfns)
		       nil
		       (get keyword 'test-argnames)
		       (get keyword 'test-arghelp))
      (declare (ignore external-arglist))
      (setq mainfn (or (get keyword 'test-mainfns) keyword))
      (%catch% (setq result (apply mainfn internal-arglist))
	       (fail (complain f "Error from " mainfn ".  " 
                               core::expand-catch-throw)
		     (throwfail "Operation aborted.")))
      result)))


(defmexpr uniform-search
  (argtypes gwff0-or-label-or-eproof yesno symbol symbol yesno)
  (argnames gwff window mode slist modify)
  (arghelp "Gwff or Eproof" "Open Vpform Window?" "Mode" "Searchlist" "Automatically modify searchlist?")
  (defaultfns (lambda (z s m l q)
		(list (if (eq z '$) 
			  (cond ((eproof-p current-eproof) current-eproof)
				((eproof-p last-eproof) last-eproof)
				(t '$))
			  z)
		      (if (eq s '$) nil s)
		      (if (eq m '$) 'cl-user::uniform-search-mode m)
		      (if (eq l '$) 'cl-user::uniform-search-2 l) 
		      (if (eq q '$) t q)
		      )))
  (mainfns test-wff-unif)
  (mhelp "Enter the test top level to search for any mode that will prove
a given theorem. The mode provided by the user should list flag settings
that are not to be varied, and the searchlist provided by the user should
list all of the flags to be varied. The default settings for the mode and
searchlist are UNIFORM-SEARCH-MODE and UNIFORM-SEARCH-2.
If you opt for the searchlist to be automatically modified, TPS will inspect
the given wff to check whether it is first order, whether it contains any 
definitions, whether it contains any equalities (and if so whether the LEIBNIZ
and ALL instantiations are different), and whether it has any possible primitive
substitutions, and will then remove or modify any unnecessary flags from the 
searchlist (respectively, unification bounds will be deleted, REWRITE-DEFNS will
be deleted, REWRITE-EQUALITIES will be deleted or modified, and DEFAULT-MS will
be changed to a search without option sets).
Also, if you opt for the searchlist to be modified and there is a proof of this
theorem in memory, AUTO-SUGGEST will be run and you will be asked whether to 
modify the searchlist using the results it provides.

After entering the test top level with this command, type GO ! to start 
searching for a successful mode."))

(defun test-wff-unif (z s m l q)
  (declare (special current-sl))
  (when (tps-mode-p m) (mode m))
  (if (known-searchlist-p l) 
      (new-searchlist l)
    (core::test-fetch l 'slist))
  (assert-defaults current-sl)
  (test-wff z t t s q))

(defun test-wff-unif-partial (z s m l q)
  (declare (special current-sl))
  (when (tps-mode-p m) (mode m))
  (if (known-searchlist-p l) 
      (new-searchlist l)
    (core::test-fetch l 'slist))
  (assert-defaults current-sl)
  (maint::test-partial z t s q))

(defmexpr test
  (argtypes gwff0-or-label-or-eproof yesno yesno yesno)
  (argnames gwff deepen reinit window)
  (arghelp "Gwff or Eproof" "Deepen?" "Reinitialize Variable Names?" "Open Vpform Window?")
  (defaultfns (lambda (z w r s)
		(list (if (eq z '$) 
			  (cond ((eproof-p current-eproof) current-eproof)
				((eproof-p last-eproof) last-eproof)
				(t '$))
			  z)
		      (if (eq w '$) t w) (if (eq r '$) t r) (if (eq s '$) nil s))))
  (mainfns test-wff)
  (mhelp "Enter the test top level. In this top level, the user can search
for an optimal mode in which to prove a particular theorem, by defining
a list of flags to be varied and then running matingsearch repeatedly 
with different flag settings.
It only works if the value of the flag DEFAULT-MS is one of thse:
MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, MS93-1, MS98-1, 
MS03-7 and MS04-2."))

(defun test-wff-prefix (gwff deepen window &optional (uniform nil))
  (declare (special printtypes *hacked-rewrites-list* *banned-conns-list* *ho-banned-conns-list*
		    *leibniz-var-list* *instantiated-defs-list* *instantiated-eqs-list* prev-node
		    *test-gwff-name* current-sl *test-gwff*))
  (if window (open-vpwin-auto))
  (setq *hacked-rewrites-list* nil)
  (setq *banned-conns-list* nil)
  (setq *ho-banned-conns-list* nil)
  (setq *leibniz-var-list* nil)
  (setq *first-equiv* nil)
  (setq *instantiated-defs-list* nil)
  (setq *instantiated-eqs-list* nil)
  (when (and (listp gwff) (eq (car gwff) 'reinit-unif-l))
	(reinit-unif-l (cadr gwff) (caddr gwff))
	(setq gwff current-eproof))
  (let* ((hxsymbol (symbolp gwff))
	 (truegwff (if hxsymbol 
		       (progn (setq dproof gwff) (get-gwff0 gwff))
		     gwff)))
    (setq current-topnode
	  (if (eproof-p truegwff) 
	      (progn (setq current-eproof truegwff) 
		     (eproof-etree truegwff))
	    (gwff-to-etree truegwff t deepen)))
    (setq active-mating 
	  (if (eproof-p truegwff) (car (eproof-mating-list truegwff)) nil))
    (setq prev-node current-topnode)
    (setq printtypes (if first-order-mode-parse nil printtypes))
    (unless (eproof-p gwff) 
	    (initialize-mating-search))
    (update-statuses (eproof-etree current-eproof))
    (unless (eproof-merged current-eproof)
	    (setq first-order-mode-ms
		  (first-order-problem-p
		   (mapcar #'(lambda (x) (cons (exp-var-var (car x)) (cdr x)))
			   (eproof-free-vars-in-etree current-eproof)))))
    (when uniform
	  (let* ((gwff (get-gwff0 *test-gwff-name*))
		 (p (not (reduce #'(lambda (x y) (or x y)) 
				 (mapcar #'(lambda (x) (apply-prim-subst-for-var (car x)))
					 (eproof-free-vars-in-etree current-eproof)))))
		 (f first-order-mode-ms)
		 (d (not (contains-defn-not-equiv gwff)))
		 (e (not (contains-equality-eventually gwff)))
		 ;e will be T if the formula contains no equalities
		 (l (and (not e) (contains-equality-smart gwff)))
		 ;f will be T if the formula contains equalities but LEIBNIZ and ALL give the same result.
		 (name (copy-list (searchlist-list current-sl)))
		 (func (searchlist-function current-sl))
		 (*suggest-list* nil)
		 (a (use-nd-proof *test-gwff-name* *test-gwff*))
		 newname)
	    (declare (special *suggest-list*))
	    (when (or p f d e l a) 
		  (setq newname (make-symbol (concatenate 'string (princ-to-string (searchlist-name current-sl))
							  "-" (when p "P") (when d "D") (when f "F")
							  (when e "E") (when l "L") (when a "A")))))
	    (when newname
		  (new-searchlist newname) ;if it exists, use it, otherwise create it
		  (setf (searchlist-list current-sl)
			(discard-if-useless name p f d e l a))
		  (setf (searchlist-function current-sl) func)
		  (msgf t "Modified searchlist " newname " is as follows:" t)
		  (print-searchlist current-sl nil nil)
		  (assert-defaults current-sl)
		  (test-wff-prefix gwff deepen window nil)))
	    )))

(defun use-nd-proof (name gwff)
  (declare (special core::*dproof-list*))
  (let* ((gwff (if (listp gwff) gwff (get-gwff0 gwff)))
	 (possible (car (remove-if-not #'(lambda (x) (and (not (proof-plans x))
							  (or (eq name x) (wffeq-ab (get x 'assertion) gwff))))
				       core::*dproof-list*)))
	 (dproof (or possible dproof)))
    (when possible
	  (msgf "There is a proof of this theorem in memory." t
		"Running AUTO-SUGGEST..." t)
	  (auto-suggest nil)
	  (msgf t)
	  (query "Use these results to modify the searchlist?" t))))

(defun contains-equality-eventually (gwff)
  (let ((rewrite-defns '(eager)))
    (do ((gwff (instantiate-1-notequiv gwff) (instantiate-1-notequiv gwff))
	 (oldgwff gwff gwff))
	((wffeq-ab gwff oldgwff) (core::contains-= gwff)))))

(defun contains-equality-smart (gwff)
  (let ((rewrite-defns '(eager)))
    (do ((gwff (instantiate-1-notequiv gwff) (instantiate-1-notequiv gwff))
	 (oldgwff gwff gwff))
	((wffeq-ab gwff oldgwff) 
	 (let ((gwff1 (installeq gwff 'LEIBNIZ))
	       (gwff2 (installeq gwff 'ALL)))
	   (wffeq-ab gwff1 gwff2))))))

(defun instantiate-1-notequiv (inwff)
  (let ((oneflag nil))
    (declare (special oneflag))
    (instantiate-definitions
     inwff #'(lambda (abbsym chkarg)
	       (declare (special oneflag))
	       (when (not (memq abbsym chkarg))
		     (prog1 (not oneflag) (setq oneflag t))))
     '(EQUIV))))
	   
(defun installeq (gwff flagval)
  (let ((rewrite-equalities flagval))
    (do ((gwff (instantiate-equalities gwff) (instantiate-equalities gwff))
	 (oldgwff gwff gwff))
	((wffeq-ab gwff oldgwff) gwff))))

(defun discard-if-useless (list p f d e l a)
  (declare (special *suggest-list* max-search-limit search-time-limit))
  ; caar is value for msv
  ; cadar is T if there are primsubs, NIL o/w
  ; cadr is MAX-MATES
  ; caddr is rewrite-defns
  ; cadddr is NUM-OF-DUPS
  (if (null list) nil
    (cond ((and (or p (and a (not (cadar *suggest-list*))))
		(eq (search-item-flag (car list)) 'DEFAULT-MS))
	   (let ((c (if (memq (search-item-current (car list)) '(MS90-9 MS91-7 MS92-9 MS93-1))
			'ms90-3 'ms88))
		 (d2 (if (memq (search-item-default (car list)) '(MS90-9 MS91-7 MS92-9 MS93-1))
			'ms90-3 'ms88))
		 (r (remove-duplicates
		     (mapcar #'(lambda (x) (if (memq x '(MS90-3 MS90-9 MS91-7 MS92-9 MS93-1)) 'ms90-3 'ms88))
			     (search-item-range (car list))))))
	   (if (= 1 (length r))
	       (progn (setq default-ms (search-item-current (car list)))
		      (discard-if-useless (cdr list) p f d e l a))
	     (cons (make-search-item :flag 'default-ms :current c :range r :default d2)
		   (discard-if-useless (cdr list) p f d e l a)))))
	  ((and p (memq (search-item-flag (car list)) '(MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT)))
	   (setq max-search-limit (search-item-current (car list)))
	   (setq search-time-limit (search-item-current (car list)))
	   (discard-if-useless (cdr list) p f d e l a))
	  ((and f (memq (search-item-flag (car list)) '(MAX-SEARCH-DEPTH MAX-UTREE-DEPTH MAX-SUBSTS-VAR MAX-SUBSTS-QUICK
									 MAX-SUBSTS-PROJ MAX-SUBSTS-PROJ-TOTAL
									 COUNTSUBS-FIRST MIN-QUICK-DEPTH IMITATION-FIRST)))
	   (discard-if-useless (cdr list) p f d e l a))
	  ((and a (memq (search-item-flag (car list)) '(MAX-SUBSTS-VAR MAX-SUBSTS-QUICK)))
	   (cons (make-search-item :flag (search-item-flag (car list))
				   :current (max (search-item-current (car list)) (caar *suggest-list*))
				   :default (max (search-item-default (car list)) (caar *suggest-list*))
				   :range (let ((newr (remove-if #'(lambda (x) (< x (caar *suggest-list*)))
								 (search-item-range (car list))))
						(min (apply 'min (search-item-range (car list)))))
					    (when (< (length newr) 3)
						  (setq newr 
						      (mapcar #'(lambda (x) 
								  (+ x (max 0 
									    (- (caar *suggest-list*) min))))
							      (search-item-range (car list)))))
					    newr))
		 (discard-if-useless (cdr list) p f d e l a)))
	  ((and a (eq (search-item-flag (car list)) 'MAX-MATES))
	   (cons (make-search-item :flag 'MAX-MATES
				   :current (max (search-item-current (car list)) (cadr *suggest-list*))
				   :default (max (search-item-default (car list)) (cadr *suggest-list*))
				   :range (let ((newr (remove-if #'(lambda (x) (< x (cadr *suggest-list*)))
								 (search-item-range (car list))))
						(min (apply 'min (search-item-range (car list)))))
					    (when (< (length newr) 3)
						  (setq newr 
						      (mapcar #'(lambda (x) 
								  (+ x (max 0 
									    (- (cadr *suggest-list*) min))))
							      (search-item-range (car list)))))
					    newr))
		 (discard-if-useless (cdr list) p f d e l a)))
	  ((and a (eq (search-item-flag (car list)) 'NUM-OF-DUPS))
	   (cons (make-search-item :flag 'NUM-OF-DUPS
				   :current (max (search-item-current (car list)) (cadddr *suggest-list*))
				   :default (max (search-item-default (car list)) (cadddr *suggest-list*))
				   :range (let ((newr (remove-if #'(lambda (x) (< x (cadddr *suggest-list*)))
								 (search-item-range (car list))))
						(min (apply 'min (search-item-range (car list)))))
					    (when (< (length newr) 3)
						  (setq newr 
						      (mapcar #'(lambda (x) 
								  (+ x (max 0 
									    (- (cadddr *suggest-list*) min))))
							      (search-item-range (car list)))))
					    newr))
		 (discard-if-useless (cdr list) p f d e l a)))
	  ((and d (eq (search-item-flag (car list)) 'REWRITE-DEFNS))
	   (discard-if-useless (cdr list) p f d e l a))
	  ((and a (eq (search-item-flag (car list)) 'REWRITE-DEFNS))
	   (setq rewrite-defns
		 (if (caaddr *suggest-list*)
		     (if (cadr (caddr *suggest-list*))
			 (list 'EAGER (cons 'DUAL (caaddr *suggest-list*)) (cons 'NONE (cadr (caddr *suggest-list*))))
		       (list 'EAGER (cons 'DUAL (caaddr *suggest-list*))))
		   (if (cadr (caddr *suggest-list*))
		       (list 'EAGER (cons 'NONE (cadr (caddr *suggest-list*))))
		     (list 'EAGER))))
	   (discard-if-useless (cdr list) p f d e l a))
	  ((and e (eq (search-item-flag (car list)) 'REWRITE-EQUALITIES))
	   (discard-if-useless (cdr list) p f d e l a))
	  ((and l (eq (search-item-flag (car list)) 'REWRITE-EQUALITIES))
	   (let ((c (if (eq (search-item-current (car list)) 'LEIBNIZ)
			'ALL (search-item-current (car list))))
		 (d2 (if (eq (search-item-default (car list)) 'LEIBNIZ)
			'ALL (search-item-default (car list))))
		 (r (remove-duplicates
		     (mapcar #'(lambda (x) (if (eq x 'leibniz) 'all x))
			     (search-item-range (car list))))))
	   (if (= 1 (length r))
	       (progn (setq rewrite-equalities (search-item-current (car list)))
		      (discard-if-useless (cdr list) p f d e l a))
	     (cons (make-search-item :flag 'rewrite-equalities :current c :range r :default d2)
		   (discard-if-useless (cdr list) p f d e l a)))))
	  (t (cons (car list) (discard-if-useless (cdr list) p f d e l a))))))
	   
(defun test-wff (gwff deepen reinit window &optional (uniform nil))
  (setq *test-gwff-name* *last-gwff-name*)
  (setq *test-mode-name* (make-symbol (concatenate 'string (princ-to-string *test-gwff-name*)
						   "-TEST-MODE")))
  (setq *test-gwff* gwff)
  (let ((displaywff t)
	(ppwfflag nil)
	(prev-node nil)
	(nodestack nil)
	(cmdstack nil)
	(current-topnode nil)
	(strong-defaults 'strong-mate-defaults)
	(printtypes printtypes))
    (declare (special displaywff printdepth ppwfflag  prev-node nodestack
		      cmdstack strong-defaults  printtypes current-eproof
		      current-topnode))
    (if reinit (expunge-vars))
    (%catch% (progn (test-wff-prefix gwff deepen window uniform)
		    (setq *test-top* t)
                    (testtop) 
                    (setq last-eproof current-eproof)
                    (setq *test-top* nil)
		    (throwfail "Test-mating-search aborted."))
	(exit-inferior-top (setq last-eproof current-eproof) last-eproof))))

;(defmexpr test
;  (mainfns test)
;  (mhelp "Enter the test-top top level."))

(context subtoplevels)

(deftest leave
  (test-mainfns exit-testtop)
  (mhelp "Leave TEST-TOP to the next enclosing top level."))

(defun exit-testtop ()
 (when (and (boundp 'current-eproof)
	    (eproof-p current-eproof)
	    (etree-p (eproof-etree current-eproof))
	    (boundp 'active-mating)
	    (mating-p active-mating)
	    (mating-completep active-mating)
	    (not (eproof-merged current-eproof))
            (query "Merge the expansion tree?" t))
   (msgf "Merging the expansion tree.  Please stand by." t)
   (merge-tree)) 
 (setq *test-top* nil)
 (%throw% '|[Left TEST-TOP.]| exit-inferior-top))

(defun get-useful-range (line)
  (let* ((p (linealias line))
	 (last (do ((n p (1- n)))
		   ((or (not (numalias n)) (= n 1)) n)))
	 (first (do ((n (1- last) (1- n)))
		    ((or (numalias n) (= n 0)) (1+ n)))))
    (cons first last)))

(defmexpr uniform-search-l
  (argtypes pline existing-linelist line-range yesno symbol symbol yesno)
  (argnames goal support line-range window mode slist modify)
  (arghelp "Planned Line" "Support Lines" "Line range for new lines?" 
	   "Open Vpform Window?" "Mode" "Searchlist" "Automatically modify searchlist?")
  (defaultfns (lambda (p s r w m l q)
		(list (pline-default p)
		      (if (and (not (eq p '$)) (eq s '$))
			  (cdr (assoc p (proof-plans dproof)))
			  s) 
		      (if (and (eq r '$) (not (eq p '$))) (get-useful-range p) r)
		      (if (eq w '$) nil w)
		      (if (eq m '$) 'cl-user::uniform-search-mode m)
		      (if (eq l '$) 'cl-user::uniform-search-2 l) 
		      (if (eq q '$) t q)
		      )))
  (mainfns test-wff-unif-lemma)
  (mhelp "Enter the test top level to search for any mode that will prove
a given lemma. (Compare DIY-L) The mode provided by the user should list flag 
settings that are not to be varied, and the searchlist provided by the user should
list all of the flags to be varied. The default settings for the mode and
searchlist are UNIFORM-SEARCH-MODE and UNIFORM-SEARCH-2.
If you opt for the searchlist to be automatically modified, TPS will inspect
the given wff to check whether it is first order, whether it contains any 
definitions, whether it contains any equalities (and if so whether the LEIBNIZ
and ALL instantiations are different), and whether it has any possible primitive
substitutions, and will then remove or modify any unnecessary flags from the 
searchlist (respectively, unification bounds will be deleted, REWRITE-DEFNS will
be deleted, REWRITE-EQUALITIES will be deleted or modified, and DEFAULT-MS will
be changed to a search without option sets).
After entering the test top level with this command, type GO ! to start 
searching for a successful mode."))

(defun test-wff-unif-lemma (p s range w m l q)
  (declare (special current-sl use-diy))
  (when (tps-mode-p m) (mode m))
  (if (known-searchlist-p l) 
      (new-searchlist l)
    (core::test-fetch l 'slist))
  (assert-defaults current-sl)
  (let ((z (make-me-a-gwff p s))
	(pre-linelist (get dproof 'lines))
	(lr1 (car range))
	(lr2 (cdr range))
	(post-linelist nil) (introduced-lines nil) 
	(lines-before nil) (lines-after nil) (lines-in nil)
	(last-number nil))
    (if (or (> lr2 (linealias p))
	    (and (not (null s))
		 (< lr1 (reduce #'min (mapcar #'linealias s)))))
	(throwfail "The range should lie between the supports and the planned line."))
    (maint::test-partial z t w q)
    (let ((left-node (reinit-unif-l p s)))
      (unwind-protect (setq *test-gwff* (list 'reinit-unif-l p s))
	(setq *test-top* nil))
	(go-test *test-mode-name* nil)
	(when active-mating
	  (merge-tree)
          (dolist (hxsupport s)
                  (let ((newnode (update-line-node hxsupport current-topnode)))
                    (if newnode (setf (line-node hxsupport) newnode))))
          (if left-node 
	      (setf (eproof-etree current-eproof) 
		    (cadr (etree-components (eproof-etree
					     current-eproof)))))
          (if use-diy (diy-justify p s)
	    (etree-nat dproof (linealias p) nil nil))))
    (setq post-linelist (get dproof 'lines))
    (setq last-number (get (car (last post-linelist)) 'linenumber))
    (setq introduced-lines (setdiff post-linelist pre-linelist))
    ;note: setdiff is a TPS function; it is guaranteed to keep the lines in order...
    (setq lines-before (remove-if #'(lambda (x) (> (get x 'linenumber) lr1)) introduced-lines))
    (setq lines-after (remove-if #'(lambda (x) (< (get x 'linenumber) lr2)) introduced-lines))
    (setq lines-in (remove-if #'(lambda (x) (< (get x 'linenumber) lr2)) 
			      (remove-if #'(lambda (x) (> (get x 'linenumber) lr1)) post-linelist)))
    (let ((stream-store nil)
	  (newf (new-tmp-filename)))
      (progn (setq stream-store *standard-output*)
	     (setq *standard-output* (open newf :direction :output :if-exists :append :if-does-not-exist :create)))
      (unwind-protect
	  (progn
	    (do* ((lines lines-in (cdr lines))
		  (line (car lines) (car lines))
		  (number (1+ last-number) (1+ number)))
		 ((null line))
		 (move line number))
	    (if (numalias lr1) 
		(make-room-after (numalias lr1) (length introduced-lines))
	      (if (car lines-before) 
		  (progn (move (car lines-before) lr1)
			 (make-room-after (numalias lr1) (1- (length introduced-lines)))
			 (setq lines-before (cdr lines-before)))))
	    (do* ((lines (append (append lines-before lines-in) lines-after) (cdr lines))
		  (line (car lines) (car lines))
		  (number (1+ lr1) (1+ number)))
		 ((null line))
		 (move line number)))
	(progn (close *standard-output*)
	       (delete-file newf)
	       (setq *standard-output* stream-store))))))


(defun make-me-a-gwff (plan support)
  (let* ((conjunc (when support (reduce-conjunc support)))
	 (wff (if conjunc (cons (cons 'IMPLIES conjunc) (line-assertion plan))
		(line-assertion plan)))
	 (symbol (intern (concatenate 'string (princ-to-string dproof) "-LEMMA") (find-package 'auto))))
    (create-weak symbol wff)
    symbol))

(defun reduce-conjunc (supplist)
  (if (cdr supplist)
      (cons (cons 'AND (reduce-conjunc (cdr supplist))) (line-assertion (car supplist)))
    (line-assertion (car supplist))))


(defun reinit-unif-l (p s)
  (declare (special add-truth *leibniz-var-list*))
  (let* (skolem-terms imp-node supp-nodes left-node right-node)
    (setq skolem-terms (delete-duplicates
			(mapcar #'(lambda (x) (cons (make-skolem-term :term x :parameter x) 0))
				(free-vars-in-lines (cons p 
			     (apply #'append (mapcar #'(lambda (x) (line-hypotheses x)) s)))))))
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
                  s))
        (setq left-node
          (make-etree-conjunction supp-nodes imp-node))
        (setq right-node
	  (let ((leaf (make-leaf :shallow (line-assertion p)
                                 :junctive nil
                                 :positive nil
                                 :free-vars nil)))
	    (update-status nil leaf 1)
	    (init-symmetry leaf current-eproof)
	    (deepen-to-literals* (if truthvalues-hack (add-falsehood leaf) leaf))))
        (setq first-order-mode-ms
          (first-order-problem-p
           (mapcar #'(lambda (x) (cons (exp-var-var (car x)) (cdr x)))
                   (eproof-free-vars-in-etree current-eproof))))
        (setq active-mating nil)
        (do ((supp-nodes supp-nodes (cdr supp-nodes))
             (support s (cdr support)))
            ((null support))
          (setf (line-node (car support)) (car supp-nodes)))
	(init-symmetry right-node current-eproof)
        (setf (line-node p) right-node)
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
	(setq *leibniz-var-list* (leibniz-quant *leibniz-var-list*))
	(when (lazy2-used) (fiddle-with-def-leaves (eproof-etree current-eproof)))
        (update-statuses (eproof-etree current-eproof))
	left-node))
	
