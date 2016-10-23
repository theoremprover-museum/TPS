;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of S-EQN)

(deftoplevel s-eqn-top
  (top-prompt-fn s-eqn-top-prompt)
  (command-interpreter s-eqn-command-interpreter)
  (print-* s-eqn-print-*)
  (top-level-category seqncmd)
  (top-level-ctree s-eqn-command-ctree)
  (top-cmd-decode s-eqn-opdecode)
  (mhelp "The REWRITING top level."))

(eval-when (load compile eval)
(defcategory seqncmd
  (define defseqn)
  (properties
    (s-eqn-argtypes multiple)
    (s-eqn-argnames multiple)
    (s-eqn-arghelp multiple)
    (s-eqn-defaultfns multiplefns)
    (s-eqn-mainfns singlefn)
    (s-eqn-closefns singlefn) ; 9/10/05
    (mhelp single))
  (global-list global-seqnlist)
  (shadow t)
  (mhelp-line "rewriting command")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command
	(format nil "@IndexOther(~A)" (symbol-name item))
	(get item 's-eqn-argnames)
	(cdr (assoc 'seqncmd (get item 'mhelp))))))
  (mhelp-fn (seqncmd-mhelp seqncmd category))))

(context s-eqn-printing)

(defflag verbose-rewrite-justification
  (flagtype boolean)
  (default t)
  (subjects s-eqn)
  (mhelp "When set to T, justification of lines obtained by rewriting
in the REWRITING top level will indicate the rewriting theory used to
obtain the transformation."))

(defflag rewriting-relation-symbol
  (flagtype symbol)
  (default '=)
  (subjects s-eqn)
  (mhelp "Contains the symbol that is printed between lines obtained
by rewriting from immediately preceding lines."))

(context s-eqn-rules)

(defflag app*-rewrite-depth
  (flagtype null-or-posinteger)
  (default 50)
  (subjects s-eqn)
  (mhelp "The maximal rewrite depth of an app* application."))

(defflag rewriting-auto-table-size
  (flagtype posinteger)
  (default 10000)
  (subjects s-eqn)
  (mhelp "The maximal size of a search table used by AUTO. Note that while the
SIMPLE search procedure uses only one table of that size, BIDIR and
BIDIR-SORTED use two."))

(defflag rewriting-auto-depth
  (flagtype posinteger)
  (default 5)
  (subjects s-eqn)
  (mhelp "The maximal depth of a search tree when applying AUTO. For the
SIMPLE search procedure, the number corresponds to the maximal rewrite
depth, whereas for BIDIR and BIDIR-SORTED the maximal search depth is
twice the specified number."))

(defflag rewriting-auto-min-depth
  (flagtype integer+)
  (default 0)
  (subjects s-eqn)
  (mhelp "The minimal depth of a search tree needed by AUTO to find a
derivation. The value should be less or equal to that of REWRITING-AUTO-DEPTH,
otherwise no search will be performed."))

(defflag rewriting-auto-max-wff-size
  (flagtype posinteger)
  (default 15)
  (subjects s-eqn)
  (mhelp "The maximal size of a wff to be rewritten when applying AUTO."))

(defflag rewriting-auto-substs
  (flagtype gwfflist)
  (default nil)
  (subjects s-eqn)
  (mhelp "List of terms to substitute for any free variables which may be
introduced during rewriting by AUTO. If NIL, the list will be generated
automatically from atomic subwffs of the source and the target wff."))

(defflag rewriting-auto-search-type
  (flagtype auto-searchtype)
  (default 'bidir-sorted)
  (subjects s-eqn)
  (mhelp "The search procedure to use with AUTO. Currently defined are SIMPLE,
BIDIR and BIDIR-SORTED. BIDIR-SORTED will try to rewrite shorter wffs first.
When this is not needed, use BIDIR. The precise behaviour of BIDIR-SORTED
depends on the flag REWRITING-AUTO-GLOBAL-SORT."))

(defflag rewriting-auto-global-sort
  (flagtype boolean)
  (default nil)
  (subjects s-eqn)
  (mhelp "When NIL, BIDIR-SORTED will choose the next wff to be rewritten from
the successors of the current wff. When T, it will choose the next wff from
all unexplored wffs obtained so far from the initial or the target wff,
respectively. See the flag REWRITING-AUTO-SEARCH-TYPE."))

(defvar *s-eqn-allowed-mexprs*
  '(HELP EXIT ? ?? LIB USE-THEORY MAKE-THEORY LIST-RRULES DEACTIVATE-THEORY
    ACTIVE-THEORY DEACTIVATE-RULES DELETE-RRULE MAKE-ABBREV-RRULE
    MAKE-INVERSE-RRULE PERMUTE-RRULES ACTIVATE-RULES REVIEW))
(defvar *seqnder-list* nil)
(defvar *current-eqnder* nil)

(defvar *s-eqn-dproof-backup* nil)

(defmacro ln-exists (lnum)
  `(assoc ,lnum (proof-linealiases (get *current-eqnder* 's-eqn-der))))

(defmacro ln-exists-top (lnum)
  `(assoc ,lnum (proof-linealiases *s-eqn-dproof-backup*)))

(defmacro linenum-line (lnum)
  `(cdr (ln-exists ,lnum)))

(defmacro linenum-line-top (lnum)
  `(cdr (ln-exists-top ,lnum)))

(defmacro linenum-assertion (lnum)
  `(get (linenum-line ,lnum) 'assertion))

(defmacro linenum-assertion-top (lnum)
  `(get (linenum-line-top ,lnum) 'assertion))

(defmacro linenum-hyps-top (lnum)
  `(mapcar #'(lambda (l) (line-linenumber l))
	   (get (linenum-line-top ,lnum) 'hypotheses)))

(defun strongly-specified-p (arg) (and (specified-p arg) (not (integerp arg))))

(defmacro defsavedrew (name date (assertion assert)
			    (nextplan-no num) 
			    (plans plan-list)
			    (lines &rest line-list)
			    code
			    &optional (abbrev-list nil) (comment '(COMMENT "")) (locked '(LOCKED nil))
			    (gwff-name nil) ; cebrown 4/12/01
			    )
  "Reads in a representation of a rewriting proof and reconstructs it.
CODE is used to test whether the proof has been altered since it was written."
  (declare (ignore assertion nextplan-no plans lines);)
	   (special defsavedrew-target))
  (let ((prfname (gensym))
	(prfname2 (gensym)))
  `(let ((,prfname ',(gensym))
	 (,prfname2 ',gwff-name))
     (block defsavedrew
     (in-mode re-read
       (let ((first-order-mode-parse nil))
     (when (and (not expertflag) (get ,prfname2 's-eqn-der))
		;(proof-lines ,prfname))
       (unless (query (conc-strings "A proof by the name of " (string ,prfname2)
				    " already exists.  Shall I overwrite it?") t)
	 (return-from defsavedrew nil)))
     (when (and expertflag (get ,prfname2 's-eqn-der))
                ;(proof-lines ,prfname))
	   (unless (query (conc-strings "A proof by the name of " (string ,prfname2)
					" already exists. Shall I overwrite it?") t)
		   (if (query "Shall I restore the proof under some other name?" t)
		       (setq ,prfname2 (prompt-prfname))
		     (return-from defsavedrew nil))))
     (when (and (not expertflag)
		(/= ,code (mod (easy-code-list (list ,prfname (status-userid) ',date ',assert
					   ',num ',plan-list
					   ',line-list))
			       *modulo*)))
       (throwfail "I can't understand this proof."))
     ,(dolist (abbr abbrev-list) (eval abbr))
     (setf (get ,prfname 'comment) (cadr ',comment))
     (when (or expertflag (not (proof-assertion ,prfname)))
       (setf (proof-assertion ,prfname)
	     ',(let ((dummy nil))
		 (declare (special defsavedrew-target))
		 (if (and (stringp assert)
			  (not (equal assert "NIL")))
		     (progn
		       (setq defsavedrew-target t)
		       (gettype 'gwff assert))
		   (setq defsavedrew-target nil)))))
     (if (stringp (proof-assertion ,prfname))
	 (setf (proof-assertion ,prfname) 
	       (gettype 'gwff (proof-assertion ,prfname))))
     (setf (nextplan-no ,prfname) ,num)
     (let ((linealiases nil)
	   (proof-lines nil))
       (dolist (line ',line-list)
	 (let ((linename (gensym "L"))
	       (linenumber (car line))
	       (hyps (cadr line))
	       (assertion (third line))
	       (just-rule (fourth line))
	       (just-terms (fifth line))
	       (just-lines (sixth line))
	       (line-comment (seventh line))
	       (linelock (member (car line) (cadr ',locked))))
	   (push (cons linenumber linename) 
		 linealiases)
	   (push linename proof-lines)
	   (setf (line-linenumber linename) linenumber)
	   (setf (line-hypotheses linename) 
		 (mapcar #'(lambda (x) 
			     (cdr (assoc x linealiases)))
			 hyps))
	   (setf (line-assertion linename) 
		 (if (stringp assertion) (gettype 'gwff assertion)
		   (restoreproof-gwff assertion)))
	   (setf (line-justification linename) (make-list 3))
	   (setf (line-just-rule linename) just-rule)
	   (setf (line-just-terms linename) 
		 (mapcar #'(lambda (x) (if (stringp x) (gettype 'gwff x) (restoreproof-gwff x)))
			 just-terms))
	   (setf (get linename 'comment) line-comment)
	   (setf (get linename 'locked) (if linelock t nil))
	   (setf (line-just-lines linename) 
		 (mapcar #'(lambda (x) 
			     (cdr (assoc x linealiases)))
			 just-lines))))
       (setf (get ,prfname2 's-eqn-der) ,prfname)
       (setq *current-eqnder* ,prfname2)
       (setq *seqnder-list* (adjoin ,prfname2 *seqnder-list*))
       (when ,name
	 (setf (get ,prfname2 'theory) ,name))
       ;(setf (get ,prfname 'gwff-name) ',gwff-name)
       (setf (proof-plans ,prfname) 
	     (mapcar 
	       #'(lambda (y) 
		   (mapcar #'(lambda (x)
			       (cdr (assoc x linealiases)))
			   y))
	       ',plan-list))
       (setf (proof-linealiases ,prfname)
	     (nreverse linealiases))
       (setf (proof-lines ,prfname)
	 (nreverse proof-lines))
       (let ((dummy nil))
	 (declare (special defsavedrew-target))
	 (when defsavedrew-target
	   (setf (get ,prfname2 'target-line) (car (last proof-lines)))))
       (dolist (line (proof-lines ,prfname)) 
	       (when (get line 'comment) (setf (get line 'comment) (parse-comment (get line 'comment)))))
       (when (get dproof 'comment) (setf (get dproof 'comment) (parse-comment (get dproof 'comment))))
       ;(find-all-gaps)
       (let ((dproof (get *current-eqnder* 's-eqn-der)))
	 (declare (special dproof))
	 (find-all-gaps))
       (format t "Proof ~A restored." ,prfname2)
       (unless (get ,name 'theory)
	 (let ((load-th nil))
	   (prompt-read
	    load-th nil
	    (msgf "The proof was obtained in theory " ,name "." t
		  "Should I load it from the library?")
	    'yesno t
	    ((? (msgf "y or yes for YES, n or no for NO."))
	     (abort (throwfail "Aborting by user request."))))
	   (when load-th
	     (retrieve-libobject-real ,name 'theory))))
       ,prfname)))))))
