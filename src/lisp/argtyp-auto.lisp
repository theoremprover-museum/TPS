;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of auto-basic)

(deffile argtyp-auto
  (part-of auto-basic)
  (mhelp "Contains the definitions of types used in AUTO."))

(context expansion-trees)

(deftype% etree
  (getfn get-etree)
  (testfn etree-p)
  (printfn princ)
  (mhelp "An expansion tree or a gwff."))

(deftype% boolean-or-abbrevlist
  (getfn testfn)
  (testfn (or (eq boolean-or-abbrevlist T)
	      (eq boolean-or-abbrevlist nil)
	      (and (listp boolean-or-abbrevlist)
		   (reduce #'(lambda (x y) (and x y)) (mapcar #'abbrev-q boolean-or-abbrevlist)))))
  (printfn princ)
  (mhelp "T, NIL or a list of abbreviations."))

(deftype% rewrite-defns-list
  (getfn (if (memq rewrite-defns-list (list 'none 'eager 'lazy1 'lazy2 'dual))
	     (list rewrite-defns-list)
	   (if (and (listp rewrite-defns-list)
		    (and (memq (car rewrite-defns-list) (list 'none 'eager 'lazy1 'lazy2 'dual))
			 (dolist (odefs (cdr rewrite-defns-list) rewrite-defns-list)
				 (memq (car odefs) '(none eager lazy1 lazy2 dual))
				 (when (remove-if #'symbolp (cdr odefs)) (return nil)))))
	       rewrite-defns-list
	     nil)))
  (testfn (and (listp rewrite-defns-list)
	       (and (memq (car rewrite-defns-list) (list 'none 'eager 'lazy1 'lazy2 'dual))
		    (dolist (odefs (cdr rewrite-defns-list) rewrite-defns-list)
			    (memq (car odefs) '(none eager lazy1 lazy2))
			    (when (remove-if #'symbolp (cdr odefs)) (return nil))))))
  (printfn princ)
  (mhelp "A list whose first element is one of NONE, EAGER, LAZY1 and DUAL,
and whose other (optional) elements are lists whose first element is
one of these four options and whose other elements are the names of 
definitions.
The first element is the default behaviour for rewriting definitions,
and the other lists are lists of exceptions to this default, with a
different behaviour specified.
NONE:  do not rewrite this definition at all.
EAGER: rewrite all of these definitions, in one big step, as soon 
       as possible.
LAZY1: rewrite these, one step at a time, when there are no more
       EAGER rewrites to do.
DUAL: as LAZY1, but rewrite these abbreviations A to a conjunction of
       A and A, and then deepen only one of these conjuncts. (e.g.
       TRANSITIVE p becomes 
       TRANSITIVE p AND FORALL x y z . [pxy AND pyz] IMPLIES pxz
LAZY2: synonym for DUAL.

For example: the value
(EAGER)
would be interpreted as \"Rewrite every definition in one step.\"

(DUAL (EAGER TRANSITIVE) (NONE INJECTIVE SURJECTIVE))
would be interpreted as \"Rewrite TRANSITIVE whenever it appears.
Don't ever rewrite INJECTIVE or SURJECTIVE. Rewrite every other 
definition in the DUAL way.\""))

(context mating-search)
(deftype% querytype
  (getfn testfn)
  (testfn (lambda (x) (case x ((query-jforms show-jforms query-slists t nil) t))))
  (printfn princ)
  (mhelp "Should be one of T, NIL, QUERY-SLISTS, SHOW-JFORMS or QUERY-JFORMS.
Used in the flag QUERY-USER."))

(deftype% leaftype
  (testfn (lambda (leaftype) (declare (special leaf-name *renamed-leaves-list*))
	    (or (symbolp leaftype) (integerp leaftype) 
		(if (realp leaftype)
		    (car (rassoc (conc-names leaf-name (princ-to-string leaftype))
				 *renamed-leaves-list*))))))
  (getfn (lambda (leaftype) (declare (special leaf-name *renamed-leaves-list*))
	   (if (integerp leaftype) leaftype
	     (if (symbolp leaftype) (or (car (rassoc leaftype *renamed-leaves-list*))
					leaftype)
	       (if (realp leaftype)
		   (car (rassoc (conc-names leaf-name (princ-to-string leaftype))
				*renamed-leaves-list*)))))))
  (printfn princ)
  (mhelp "The type of leaf names; i.e. symbol, integer or a restricted set of reals."))

(deftype% searchtype
  (getfn (lambda (x) (case x ((ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 mt94-11 mt94-12 mt95-1 ms98-1 ms03-7 ms04-2) x)
                             (t (complain x " is not of type SEARCHTYPE.")
                                (throwfail)))))
  (testfn (lambda (x) (case x ((ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 mt94-11 mt94-12 mt95-1 ms98-1 ms03-7 ms04-2) t)
			      (t nil))))
  (printfn princ)
  (mhelp "Should be one of MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, MS93-1, MT94-11, MT94-12, MT95-1, MS98-1, MS03-7, MS04-2"))

(deftype% nat-etree-version-type
  (getfn (lambda (x) (case x ((old hx ceb) x)
			   (t (complain x " is not of type NAT-ETREE-VERSION-TYPE")
			      (throwfail)))))
  (testfn (lambda (x) (case x ((old hx ceb) t)
			    (t nil))))
  (printfn princ)
  (mhelp "Should be one of OLD, HX, CEB"))

(deftype% mate-command
  (getfn testfn)
  (testfn consp)
  (printfn princ)
  (mhelp "A list with mating-search commands."))

(deftype% gwff0
  (testfn (lambda (gwff0)
	    (and (gwff-p gwff0) (string= (princ-to-string (type gwff0)) "O"))))
                                        ; removed this because it doesn't let you test non-symbols (e.g. can't test edwff)
					;was (eq (get gwff0 'type) 'O))))
  (getfn get-gwff0)
  (printfn printwffhere)
  (mhelp ("A reference to a wff of type O."
	  (e (category-mhelp-list 'getgwfftype)))))

(deftype% eproof
  (getfn (lambda (epf)
	   (cond ((eproof-p epf) epf)
		 ((and (symbolp epf) (boundp epf) (eproof-p (symbol-value epf)))
		  (symbol-value epf))
		 ((and (symbolp epf) (get epf 'eproof))
		  (get epf 'eproof))
		 (t epf))))
  (testfn getfn)
  (printfn (lambda (epf)
	     (if (eproof-p epf)
		 (print-eproof epf *standard-output* 3)
	       (msgf epf))))
  (mhelp "An Expansion Proof"))

(deftype% gwff0-or-eproof
  (getfn get-gwff0-or-eproof)
  (testfn getfn)
  (printfn (lambda (it)
	     (cond ((eproof-p it) (print-eproof it *standard-output* 3))
		   ((symbolp it) (msg it))
		   (t (printwffhere it)))))
  (mhelp "Either a gwff of type O, CURRENT-EPROOF, LAST-EPROOF, an eproof, 
or a symbol which names an eproof."))

(deftype% gwff0-or-label-or-eproof
  (testfn get-gwff0-or-eproof)
  (getfn (lambda (gwff0) (let ((wff-or-label-or-eproof (get-gwff0-or-eproof gwff0)))
                              (if (symbolp gwff0) 
                                  (if (or (and (boundp gwff0) (eproof-p (symbol-value gwff0)))
					  (eproof-p wff-or-label-or-eproof))
                                      wff-or-label-or-eproof
                                      gwff0)
                                  wff-or-label-or-eproof))))
  (printfn (lambda (it)
	     (cond ((eproof-p it) (print-eproof it *standard-output* 3))
		   ((symbolp it) (msg it))
		   (t (printwffhere it)))))
  (mhelp "Either a gwff of type O, CURRENT-EPROOF, LAST-EPROOF, an eproof, 
or a symbol which names an eproof. If it is a symbol representing a gwff,
getfn returns the symbol instead of the gwff. Checking type gwff0-or-label
for more details."))

(deftype% gwff0-or-label-or-edag
  (testfn get-gwff0-or-edag)
  (getfn (lambda (gwff0) (let ((wff-or-label-or-edag (get-gwff0-or-edag gwff0)))
			   (if (symbolp gwff0) 
			       (if (or (and (boundp gwff0)
					    (or (ext-exp-open-dag-p (symbol-value gwff0))
						(ext-exp-dag-p (symbol-value gwff0))))
				       (or (ext-exp-open-dag-p wff-or-label-or-edag)
					   (ext-exp-dag-p wff-or-label-or-edag)))
				   wff-or-label-or-edag
				 gwff0)
			     wff-or-label-or-edag))))
  (printfn (lambda (it)
	     (cond ((ext-exp-open-dag-p it) (print-ext-exp-open-dag it *standard-output* 3))
		   ((ext-exp-dag-p it) (print-ext-exp-dag it *standard-output* 3))
		   ((symbolp it) (msg it))
		   (t (printwffhere it)))))
  (mhelp "Either a gwff of type O, an extensional expansion dag,
or a symbol which names an extensional expansion dag. If it is a symbol 
representing a gwff, getfn returns the symbol instead of the gwff. 
Checking type gwff0-or-label for more details."))

(context mating-search)

(deftype% matingpair
  (getfn testfn)
  (testfn (lambda (elt) (and (consp elt) (symbolp (car elt)) (symbolp (cdr elt)))))
  (printfn  princ)
  (mhelp "A mating connection in the form (LEAFn . LEAFm). Actually, any dotted pair 
of symbols will do; it is up to the user to ensure that it's really a connection."))

(deflisttype matingpairlist matingpair)

(deftype% mt-subsumption
  (getfn (lambda (x) (case x ((subset-conns same-conns same-tag T NIL) x) 
                             (t (complain x " is not of type MT-SUBSUMPTION.")
                                (throwfail)))))
  (testfn (lambda (x) (case x ((subset-conns same-conns same-tag T NIL) t)
			      (t nil))))
  (printfn princ)
  (mhelp "Should be one of NIL, SUBSET-CONNS, SAME-CONNS, SAME-TAG, T. 
See the flag MT-SUBSUMPTION-CHECK."))

(context mtree-print)

(deftype% obdefault
  (getfn (lambda (x) (case x ((deepest highest h-smallest d-smallest) x)
                             (t (complain x " is not of type OBDEFAULT.")
                                (throwfail)))))
  (testfn (lambda (x) (case x ((deepest highest h-smallest d-smallest) t)
			      (t nil))))
  (printfn princ)
  (mhelp "Should be one of DEEPEST, HIGHEST, D-SMALLEST or H-SMALLEST.
Used by the flag DEFAULT-OB."))

(deftype% matingstree
  (getfn (lambda (x) x))
  (testfn matingstree-p)
  (printfn princ)
  (mhelp "An expansion tree or a gwff."))

(context unification)
(deftype% verbose
  (getfn (lambda (x) (case x ((silent min med max T NIL) x) 
                             (t (complain x " is not of type VERBOSE.")
                                (throwfail)))))
  (testfn (lambda (x) (case x ((silent min med max T NIL) t)
			      (t nil))))
  (printfn princ)
  (mhelp "Should be one of SILENT=NIL, MIN, MED, or MAX=T, used in the flag MATING-VERBOSE, UNIFY-VERBOSE."))

(context tactics)

(deftype% tactic-exp
  (getfn (lambda (tac-exp)
	   (if (tactic-p tac-exp)
	       tac-exp
	       (progn (complain tac-exp " is not of type " 'tactic-exp)
		      (throwfail)))))
  (testfn tactic-p)
  (printfn princ)
  (mhelp ("Either the name of a tactic or a compound tactic expression. 
Currently defined tactics are:
" (e (prinlc (remove-if-not #'symbolp *global-tacticlist*))) ".")))

(deftype% tactic-use
  (getfn (lambda (tactic-use)
	   (if (tactic-use-p tactic-use)
	       tactic-use
	       (progn (complain tactic-use " is not of type " 'tactic-use)
		    (throwfail)))))
  (testfn tactic-use-p)
  (printfn princ)
  (mhelp ("The use to which a tactic will be put.  Allowable values are:
" (e (prinlc *tactic-use-list*)) ".")))

(deftype% tactic-mode
  (getfn (lambda (tactic-mode)
	   (if (tactic-mode-p tactic-mode)
	       tactic-mode
	       (progn (complain tactic-mode " is not of type " 'tactic-mode)
		    (throwfail)))))
  (testfn tactic-mode-p)
  (printfn princ)
  (mhelp ("The mode in which a tactic will be used.  Allowable values are:
" (e (prinlc *tactic-mode-list*)) ".")))

(context test-searchlists)
(deflisttype anything-list anything)

(context skolems)

(defconstype subst-pair gvar gwff
  (mhelp "Means substitute gwff for gvar."))

(deflisttype subst-alist subst-pair
  (mhelp "List of (gvar . gwff) pairs."))

; cebrown 4/26/00
(defconstype symbol-data-pair symbol anything
	     (mhelp "A (SYMBOL . <data>) pair"))

(deflisttype symbol-data-list symbol-data-pair
  (mhelp "List of (SYMBOL . <anything>) pairs."))

; mkaminski 8/11/05
(deftype% rel-or-label
  (testfn (lambda (eqn)
            (and (gwff-p eqn) (eq (get eqn 'type) 'O)
		 (equals-p eqn))))
  (getfn (lambda (eqn) (let ((tmp-wff (get-gwff0 eqn)))
			 (let ((eqn-or-label
				(if ;(equals-p tmp-wff)
				    (and (wff-applic-p tmp-wff)
					 (wff-applic-p (gar tmp-wff))
					 (eq (if ;*active-rewrite-theory*
						 rewriting-relation-symbol
						 rewriting-relation-symbol
					       (if (get
						    *active-rewrite-theory*
						    'relation-sign)
						   (get
						    *active-rewrite-theory*
						    'relation-sign)
						 '=))
					     (find-symbol
					      (nameroot (gar (gar tmp-wff))))))
				    tmp-wff
				  (acons (inherit-abbrev '=
							 '((O . O) . O) '(O))
					 tmp-wff 'TRUTH)))
			       (eqn (if (or (equals-p tmp-wff) (symbolp eqn))
					eqn
				      (format nil "~A ~A" eqn "= TRUTH"))))
			   (setq *last-gwff-typed* eqn)
			   (if (label-p eqn) eqn eqn-or-label)))))
  (printfn printwffhere)
  (mhelp "A reference to a relation. If the relation is a label
the getfn will give the label name instead of the wff represented
by the label."))
;          (e (category-mhelp-list 'getgwfftype)))))

(deftype% line-ge-2
  (testfn (lambda (line) (and (integerp line) (> line 1))))  
  (getfn testfn)
  (short-prompt T)
  (printfn princ)
  (mhelp "A line number >=2."))

(deftype% gwff-or-selection
  (testfn gwff-or-selection-p)
  (getfn get-gwff-or-selection)
	 ;(lambda (wff-or-sel)
	 ;  (if (gwff-or-selection-p wff-or-sel)
	 ;      (if (integerp wff-or-sel) wff-or-sel
	 ;	 (getwff-subtype 'gwff-p wff-or-sel))
	 ;    (throwfail wff-or-sel " does not refer to a wff."))))
  (printfn (lambda (wff-or-sel) (if (integerp wff-or-sel) (princ wff-or-sel)
				  (printwffhere wff-or-sel))))
  (mhelp "A selection from a number of given wffs or a reference to a wff."))
;	  (e (category-mhelp-list 'getgwfftype)))))

(deftype% gwff-or-nil
  (testfn (lambda (x) (or (null x) (gwff-p x))))
  (getfn (lambda (gwff)
	   (if gwff (getwff-subtype 'gwff-p gwff)
	     gwff)))
  (printfn (lambda (gwff) (if gwff (printwffhere gwff) (princ gwff))))
  (mhelp "A reference to a wff or NIL."))
;	  (e (category-mhelp-list 'getgwfftype)))))

(deftype% gwff-or-label
  (testfn gwff-p)
  (getfn (lambda (gwff) (let ((wff-or-label ;(get-gwff0 gwff)))
			       (getwff-subtype 'gwff-p gwff)))
			  (setq *last-gwff-typed* gwff)
			  (if (label-p gwff) gwff wff-or-label))))
  (printfn printwffhere)
  (mhelp "A reference to a wff. If the gwff is a label the getfn will
give the label name instead of the wff represented by the label."))

(deftype% auto-searchtype
  (getfn (lambda (x) (case x
		       ((simple bidir bidir-sorted) x)
		       (t (throwfail x " is not of type AUTO-SEARCHTYPE")))))
  (testfn (lambda (x) (case x
			((simple bidir bidir-sorted) t)
			(t nil))))
  (printfn princ)
  (mhelp "Should be one of SIMPLE, BIDIR, BIDIR-SORTED."))
