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
;;; File: EXT-EXP-DAG-MACROS  - cebrown - 7/13/03
;;; This contains the definitions of structures, etc, which define macros
;;; for the ext-dags module.

(context ext-seq)

(defstruct (ext-seq
	    (:print-function print-ext-seq))
  (wffs nil) ; the members of the sequent (order is not important)
  (princ-wffs nil)
  (pos-rule nil)
  (kind nil)				; TRUE, NEG, DIS, FORALL, INTERNALIZE, LAMBDA,
					; INITEQ, DEC, EUNIF1, EUNIF2, EQO, EQFUNC, EXTO, EXTFUNC,
					; REW, CUT, CONTR
					; INIT, REFL
  (rew-just nil :type symbol)
  (exp-term nil)
  (sel-var nil)
  (prems nil))

(context ext-exp-dags)

(defstruct (ext-exp-dag
	    (:print-function print-ext-exp-dag))
  (name 'a :type symbol)
  (shallow nil)
  (positive t)
  (kind nil) ; TRUE FALSE NEG DIS CON IMP EXP SEL REW ATOM EQN DEC EQNGOAL LEAF
  (rew-just nil :type symbol) ; LAMBDA EQUIVWFFS EQUIV-IMPLICS EQUIV-DISJS EXT=
  (arcs nil :type list))

(defstruct (ext-exp-arc
	    (:print-function print-ext-exp-arc))
  (node nil) ; target ext-exp-dag
  (kind nil) ; NIL EXP SEL MATE EQNDEC EQNGOAL DEC EUNIF1 EUNIF2
  (dec-index nil)
  (exp-term nil)
  (sel-var nil))

(defvar ext-exp-dag-verbose nil)
(defvar ext-exp-dag-debug nil)

(defvar *eeod-stamp* 0)

(defstruct (ext-exp-open-dag
	    (:print-function print-ext-exp-open-dag))
  (name 'a :type symbol)
  (shallow nil)
  (positive t)
  (kind nil) ; TRUE FALSE NEG DIS CON IMP EXP SEL REW FLEX ATOM EQN DEC EQNGOAL
  (rew-just nil :type symbol) ; EQUIV-IMPLICS EQUIV-DISJS EXT=
  (parent-arcs nil :type list) ; WARNING: MS03-7 keeps up with parents, MS04-2 does NOT
  (arcs nil :type list)
  (stamp (incf *eeod-stamp*) :type number))

(defstruct (ext-exp-open-arc
	    (:print-function print-ext-exp-open-arc))
  (parent nil)
  (node nil) ; target ext-exp-dag
  (kind nil) ; NIL EXP SEL MATE EQNDEC EQNGOAL DEC EUNIF1 EUNIF2
  (dec-index nil)
  (exp-term nil)
  (sel-var nil))

(context subtoplevels)

(defvar *extseqder-list* nil)

(deftoplevel ext-seq-top
  (top-prompt-fn ext-seq-top-prompt)
  (command-interpreter ext-seq-command-interpreter)
  (print-* ext-seq-print-*)
  (top-level-category extseqcmd)
  (top-level-ctree ext-seq-command-ctree)
  (top-cmd-decode ext-seq-opdecode)
  (mhelp "The top level for building and manipulating Extensional Sequent Derivations."))

(eval-when (load compile eval)
(defcategory extseqcmd
  (define defextseq)
  (properties
    (extseq-argtypes multiple)
    (extseq-argnames multiple)
    (extseq-arghelp multiple)
    (extseq-defaultfns multiplefns)
    (extseq-mainfns singlefn)
    (mhelp single))
  (global-list global-extseqlist)
  (shadow t)
  (mhelp-line "extensional sequent command")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command
	(format nil "@IndexOther(~A)" (symbol-name item))
	(get item 'extseq-argnames)
	(cdr (assoc 'extseqcmd (get item 'mhelp))))))
  (mhelp-fn (extseqcmd-mhelp extseqcmd category))))

(defvar *ext-seq-allowed-mexprs* '(HELP EXIT ? ?? EXT-MATE)) ; get BEGIN-PRFW and END-PRFW to work w/ ext-seq top level

(defun es-find-all-gaps ()
  (let ((dproof (get *current-seqder* 'ext-seq-der)))
    (declare (special dproof))
    (find-all-gaps)))

(defvar *eeod-list* nil)

(deftoplevel ext-mate-top
  (top-prompt-fn ext-mate-top-prompt)
  (command-interpreter ext-mate-command-interpreter)
  (print-* ext-mate-print-*)
  (top-level-category extmatecmd)
  (top-level-ctree ext-mate-command-ctree)
  (top-cmd-decode ext-mate-opdecode)
  (mhelp "The top level for building and manipulating Extensional Expansion Dags."))

(eval-when (load compile eval)
(defcategory extmatecmd
  (define defextmate)
  (properties
    (extmate-argtypes multiple)
    (extmate-argnames multiple)
    (extmate-arghelp multiple)
    (extmate-defaultfns multiplefns)
    (extmate-mainfns singlefn)
    (mhelp single))
  (global-list global-extmatelist)
  (shadow t)
  (mhelp-line "extensional expansion dag command")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command
	(format nil "@IndexOther(~A)" (symbol-name item))
	(get item 'extmate-argnames)
	(cdr (assoc 'extmatecmd (get item 'mhelp))))))
  (mhelp-fn (extmatecmd-mhelp extmatecmd category))))

(context ext-seq-files)

(defmacro defsavedextseq (name date (assertion assert)
			       (nextplan-no num) 
			       (plans plan-list)
			       (lines &rest line-list)
			       code
			       &optional (abbrev-list nil) (comment '(COMMENT "")) (locked '(LOCKED nil))
			       (gwff-name nil) ; cebrown 4/12/01
			       )
  "Reads in a representation of an extensional sequent derivation and reconstructs it.
CODE is used to test whether the proof has been altered since it was written."
  (declare (ignore assertion nextplan-no plans lines))
  (let ((prfname (gensym)))
  `(let ((,prfname ',name)
	 (prfname2 (gensym))
	 (lastline nil))
     (block defsavedextseq
     (in-mode re-read
       (let ((first-order-mode-parse nil))
     (when (and (not expertflag) (get ,prfname 'ext-seq-der))
       (unless (query (conc-strings "An extensional derivation by the name of " (string ,prfname)
				    " already exists.  Shall I overwrite it?") t)
	 (return-from defsavedextseq nil)))
     (when (and expertflag (get ,prfname 'ext-seq-der))
	   (unless (query (conc-strings "An extensional derivation by the name of " (string ,prfname)
					" already exists. Shall I overwrite it?") t)
		   (if (query "Shall I restore the proof under some other name?" t)
		       (setq ,prfname (core::prompt-prfname))
		     (return-from defsavedextseq nil))))
     (when (and (not expertflag)
		(/= ,code (mod (core::easy-code-list (list ,prfname (status-userid) ',date ',assert
							   ',num ',plan-list
							   ',line-list))
			       *modulo*)))
       (throwfail "I can't understand this proof."))
     ;; don't want students to overwrite the assertion property of a theorem
     ;; since that is checked when DONE is issued.
     ,(dolist (abbr abbrev-list) (eval abbr)) ;; cebrown 3/11/2001 - changed this to define the abbreviations before trying to parse wffs
     (setf (get prfname2 'comment) (cadr ',comment))
     (setf (nextplan-no prfname2) ,num)
     (let ((linealiases nil)
	   (proof-lines nil))
       (dolist (line ',line-list)
	 (let ((linename (gensym "L"))
	       (linenumber (car line))
	       (assertions (mapcar #'(lambda (x)
				       (if (stringp x)
					   (gettype 'gwff x)
					 x))
				   (cadr line)))
	       (just-rule (third line))
	       (just-terms (fourth line))
	       (just-lines (fifth line))
	       (princ-wffs (mapcar #'(lambda (x)
				       (if (stringp x)
					   (gettype 'gwff x)
					 x))
				   (sixth line)))
	       (line-comment (seventh line))
	       (linelock (member (car line) (cadr ',locked))))
	   (setq lastline linename)
	   (push (cons linenumber linename) 
		 linealiases)
	   (push linename proof-lines)
	   (setf (line-linenumber linename) linenumber)
	   (setf (line-justification linename) (make-list 3))
	   (setf (line-just-rule linename) just-rule)
	   (setf (line-just-terms linename) 
		 (mapcar #'(lambda (x) (if (stringp x) (gettype 'gwff x) x))
			 just-terms))
	   (setf (get linename 'comment) line-comment)
	   (setf (get linename 'locked) (if linelock t nil))
	   (setf (line-just-lines linename) 
		 (mapcar #'(lambda (x) 
			     (cdr (assoc x linealiases)))
			 just-lines))
	   (let ((es (build-line-ext-seq linename princ-wffs assertions)))
	     (setf (line-assertion linename) (ext-seq-wffs-to-fake-wff es))
	     (setf (get linename 'ext-seq) es))))
       (setf (proof-assertion prfname2) (line-assertion lastline))
       (setf (get prfname2 'ext-seq) (get lastline 'ext-seq))
       (setq *current-seqder* ,prfname)
       (setf (get *current-seqder* 'ext-seq-der) prfname2)
       (setq *extseqder-list* (adjoin *current-seqder* *extseqder-list*))
       (setf (proof-plans prfname2)
	     (mapcar 
	       #'(lambda (y) 
		   (mapcar #'(lambda (x)
			       (cdr (assoc x linealiases)))
			   y))
	       ',plan-list))
       (setf (proof-linealiases prfname2)
	     (nreverse linealiases))
       (setf (proof-lines prfname2)
	 (nreverse proof-lines))
       (setf (get prfname2 'gwff-name) ',gwff-name)
       (dolist (line (proof-lines prfname2))
	       (when (get line 'comment) (setf (get line 'comment) (core::parse-comment (get line 'comment)))))
       (when (get prfname2 'comment) (setf (get prfname2 'comment) (core::parse-comment (get prfname2 'comment))))
       (es-find-all-gaps) 
       (format t "Derivation ~A restored." ,prfname) ,prfname)))))))

(context ext-search)

(defvar *individual-types* nil)
(defvar *ext-rigid-jform* nil)
(defvar *ext-jform* nil)
(defvar *ext-full-jform* nil)
(defvar *edag-lift-info* nil)
(defvar *current-edag* nil)
(defvar *current-edag-lemmas* nil)
(defvar *current-edag-lemma-ftree-pfs* nil)
(defvar *merged-edag* nil)
(defvar *current-edag-jform* nil)

(defstruct (ms04-search
	    (:print-function ms04-print-search-state))
  (name "SEARCH")
  (weight 0)
  (edag nil)
  (banned nil) ; banning relation is local
  (dupd-exps nil)  ; list of names of exp nodes, corresponds to the script D in search states in Chad E. Brown's thesis.
  (cnf-evars nil) ; only used if MS04-PRENEX-PRIMSUBS is T
  (clause-evars nil) ; only used if MS04-PRENEX-PRIMSUBS is T
  (lit-evars nil) ; only used if MS04-PRENEX-PRIMSUBS is T
  (almost-atomic-evars nil) ; corresponds to the script S in search states in Chad E. Brown's thesis.
  (primsub-quant-evar nil) ; if we're going to primsub with a quantifier, this will be (<BINDER> <evar> <num>)
  (num-foralls 0) ; for counting primsubs
  (num-exists 0)
  (num-ands 0)
  (num-ors 0)
  (num-nots 0) ; only used if ms04-prenex-primsubs is NIL
  (num-projs 0)
  (num-equals 0)
  (num-not-projs 0) ; only used if ms04-prenex-primsubs is T
  (num-not-equals 0) ; only used if ms04-prenex-primsubs is T
  (num-dups 0)
  (num-rigid-mates 0)
  (num-eunif1s 0)
  (num-eunif2s 0)
  (num-flexrigid-mates 0)
  (num-flexrigid-neg-mates 0)
  (num-flexrigid-proj-mates 0)
  (num-flexrigid-neg-proj-mates 0)
  (num-flex-eunifs 0)
  (num-uimits 0)
  (num-uprojs 0)
  (num-delayed-conns 0)
  (delayed nil)
  (unif-dpairs nil)
  (unif-subst nil)
  (unif-banned nil)
  (set-constrs nil)
  (lemmas nil)
  (lemma-ftree-pfs nil)
  (aux-info nil))

(defvar *extra-log-consts* nil)
(defvar *ms04-semantic-hash-table* nil)
(defvar *ms04-search-done* nil)
(defvar *ms04-search-bound* 0)
(defvar *ms04-bound-reached* nil)
(defvar *ms04-MAX-PRIMSUB-EXISTS* 0)
(defvar *ms04-MAX-PRIMSUB-FORALL* 0)
(defvar *ms04-MAX-PRIMSUB-NOT* 0)
(defvar *ms04-MAX-PRIMSUB-OR* 0)
(defvar *ms04-MAX-PRIMSUB-AND* 0)
(defvar *ms04-MAX-PRIMSUB-NOT-EQUALS* 0)
(defvar *ms04-MAX-PRIMSUB-EQUALS* 0)
(defvar *ms04-MAX-PRIMSUB-NOT-PROJ* 0)
(defvar *ms04-MAX-PRIMSUB-PROJ* 0)
(defvar *ms04-MAX-PROJS* 0)
(defvar *ms04-MAX-IMITS* 0)
(defvar *ms04-MAX-FLEXRIGID-PROJ-MATES* 0)
(defvar *ms04-MAX-FLEXRIGID-MATES* 0)
(defvar *ms04-MAX-EUNIF2S* 0)
(defvar *ms04-MAX-EUNIF1S* 0)
(defvar *ms04-MAX-RIGID-MATES* 0)
(defvar *ms04-MAX-FLEX-EUNIFS* 0)
(defvar *ms04-MAX-FLEXRIGID-NEG-PROJ-MATES* 0)
(defvar *ms04-MAX-FLEXRIGID-NEG-MATES* 0)
(defvar *ms04-MAX-DUPS* 0)
(defvar *ms04-MAX-DELAYED-CONNS* 0)



