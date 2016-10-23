;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(context ms88)

(deffile mating-top
  (part-of mating)
  (extension lsp)
  (mhelp "Contents define mating-search top-level and MATE command."))

(context mating-search)

(deftoplevel mate-top
  (top-prompt-fn mate-top-prompt)
  (command-interpreter mate-command-interpreter)
  (print-* mate-print-*)
  (top-level-category mateop)
  (top-level-ctree mate-command-ctree)
  (top-cmd-decode mate-opdecode)
  (mhelp "The top level of mating search."))


;;;
;;; The category mateop is really the category of mating-search commands.  Any
;;; wffop may also be called directly, with CURRENT-TOPNODE standing for 
;;; the current top node of the expansion tree.
;;;

(eval-when (compile load eval)
(defcategory mateop
  (define defmateop)
  (properties
   (mate-alias single)
   (mate-result-> singlefn)
   (matewff-argname single)
   (mate-defaultfns multiplefns)
   (mate-applicable-p singlefn)
   (mate-move-fn singlefn)
   (mhelp single))
  (global-list global-mateoplist)
  (shadow t)
  (mhelp-line "mating-search command")
  (scribe-one-fn 
   (lambda (item)
      (maint::scribe-doc-command 
       (format nil "@IndexOther(~A)" (symbol-name item))
       (remove (get item 'matewff-argname) 
	       (get (get item 'mate-alias) 'argnames))
       (or (cdr (assoc 'mateop (get item 'mhelp)))
	   (cdr (assoc 'wffop 
		       (get (get item 'mate-alias)
			    'mhelp)))))))
  (mhelp-fn core::mateop-mhelp)))



;;; Parameters and flags follow.
;;;

(context scribe-record)

(defflag printmateflag
  (flagtype boolean)
  (default nil)
  (subjects mating-search printing)
  (mhelp
   "If T, mating-search operations are recorded into open transcript files.
Not currently implemented."))

(defflag printmateflag-slides
  (flagtype boolean)
  (default nil)
  (subjects mating-search printing)
  (mhelp
   "If T, mating-search operations are recorded in slides style. This flag has 
no effect unless PRINTMATEFLAG is T. (In fact, it has no effect even if 
PRINTMATEFLAG is T, since it hasn't been implemented.)"))

(defflag printmatefile
  (flagtype filespec)
  (default "mate.mss")
  (subjects mating-search printing)
  (mhelp "The name of the file in which mateops are recorded. This has not 
yet been implemented, although one can record remarks (only) into the 
file."))

(defmode scribe-matewff
  (flag-settings
   (allscopeflag nil)
   (atomvalflag nil)
   (displaywff t)
   (first-order-print-mode nil)
   (flushleftflag nil)
   (leftmargin 0)
   (localleftflag nil)
   (ppwfflag t)
   (printdepth 0)
   (printtypes t)
   (rightmargin 70)
   (scope nil)
   (style scribe))
  (mhelp "Mode used for writing formulas from mating search."))

(defflag printmateops
  (flagtype anything)
  (default always-true)
  (subjects mating-search printing)
  (mhelp "The function or name of the function which test whether the
result of a particular mateop should be written to a file. This has not
been implemented."))

;;;
;;;

(defvar mate-written-p nil)


;;; MATE-LEVEL keeps track of the number of recursive calls to mating-search.

(defvar mate-level 0)

;;; MATE-COMMAND-CTREE contains the command-ctree for mating-search commands

(defvar mate-command-ctree nil)

(defun initialize-mate-ctree ()
  (initialize-top-level-ctree 'mate-top))

(context mating-search)
;;;
;;; wff-mate is the main function to be called.  It returns the eproof
;;; associated with the wff, 
;;; or NIL if the mating-search was aborted with EXIT or QUIT.
;;;

(defvar last-eproof nil)

(defvar *last-gwff-name* nil)

(defvar *eproof-list* nil)

(defun get-gwff0-or-eproof (gwff)
  (cond ((eproof-p gwff) gwff)
	((and (symbolp gwff) (get gwff 'EPROOF)) ; 9/13/01
	 (get gwff 'EPROOF)) ; 9/13/01
	((eq gwff 'current-eproof) 
	 (if (eproof-p current-eproof) current-eproof
	     (throwfail "CURRENT-EPROOF not an eproof.")))
	((eq gwff 'last-eproof) 
	 (if (eproof-p last-eproof) last-eproof
	     (throwfail "LAST-EPROOF not an eproof.")))
	((and (symbolp gwff) (boundp gwff) (eproof-p (symbol-value gwff)))
	 (symbol-value gwff))
	(t (let ((global-type 'O))
	     (declare (special global-type))
	     (setq *last-gwff-name* gwff)
	     (getwff-subtype 'gwff-p gwff)))))

(defmexpr eprooflist
  (argtypes yesno)
  (argnames complete)
  (arghelp "Only Show Complete Eproofs?")
  (defaultfns (lambda (x) (list (if (eq x '$) t x))))
  (mhelp "Print a list of all expansion proofs currently in memory."))

(defun eprooflist (complete)
  (dolist (e (reverse *eproof-list*))
	  (let ((epf (get e 'eproof))
		(*ignore-statuses* t))
	    (declare (special *ignore-statuses*))
	    (if (eproof-p epf)
		(let ((g (get-shallow (eproof-etree epf)))
		      (m (find-if #'(lambda (am)
				      (and (mating-p am)
					   (mating-completep am)))
				  (eproof-mating-list epf))))
		  (when (or (not complete) m)
		    (msgf e ": " (g . gwff))))
	      (unless complete
		(msgf e))))))

(defmexpr set-eproof
  (argtypes eproof)
  (argnames epf)
  (arghelp "Eproof")
  (defaultfns (lambda (epf)
		(list (if (eq epf '$)
			  current-eproof
			epf))))
  (mhelp "Set the current expansion proof.

To see a list of expansion proofs in memory, use EPROOFLIST"))
  
(defun set-eproof (epf)
  (if (eproof-p epf)
      (setq current-eproof epf master-eproof epf)
    (throwfail epf " is not an expansion proof")))

(context subtoplevels)

;;; Removed ability to not skolemize; we don't handle selection trees 
;;; properly in the unification procedure. DAN 17DEC89

(defmexpr mate
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
  (mainfns mate-wff)
  (mhelp "Begin an expansion proof for a gwff."))

(context mating-search)


;;; See file ETREES-WFFOPS for gwff-to-etree
(defvar nodestack nil)

(defun mate-wff-prefix (gwff deepen window)
  (declare (special printtypes *hacked-rewrites-list* *banned-conns-list* *ho-banned-conns-list*
		    *leibniz-var-list* *instantiated-defs-list* *instantiated-eqs-list* prev-node))
     (if window (open-vpwin-auto))
     (setq *hacked-rewrites-list* nil)
     (setq *banned-conns-list* nil)
     (setq *ho-banned-conns-list* nil)
     (setq *leibniz-var-list* nil)
     (setq *first-equiv* t)
     (setq *instantiated-defs-list* nil)
     (setq *instantiated-eqs-list* nil)
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
;;;I think there is a bug in first-order-problem-p when current-eproof
;;;is merged. The following should be considered as a temporary solution
        (unless (eproof-merged current-eproof)
           (setq first-order-mode-ms
              (first-order-problem-p
                  (mapcar #'(lambda (x) (cons (exp-var-var (car x)) (cdr x)))
                      (eproof-free-vars-in-etree current-eproof)))))))

(defun mate-wff (gwff deepen reinit window)
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
    (%catch% (progn (mate-wff-prefix gwff deepen window)
                    (matetop) 
                    (setq last-eproof current-eproof)
                    (throwfail "Mating-search aborted."))
	(exit-inferior-top (setq last-eproof current-eproof) last-eproof))))
				
(defun strong-mate-defaults (keyword alias)
  (let ((matewff-argname (get keyword 'matewff-argname)))
    (do ((argnames (getkey keyword alias 'argnames) (cdr argnames))
	 (strong-defaults
	  nil
	  (cons (if (eq (car argnames) matewff-argname) (cons t current-topnode) nil)
		strong-defaults)))
	((null argnames) (nreverse strong-defaults)))))


(defgwff-type current-eproof-type
  (checkfn current-eproof-ckfn)
  (getfn current-eproof-getfn)
  (mhelp "current-eproof : The mating-search name for the eproof being worked on."))

(defun current-eproof-ckfn (xxx)
  (eq xxx 'current-eproof))

(defun current-eproof-getfn (xxx)
  (declare (ignore xxx))
  (if (and (boundp 'current-eproof) current-eproof) current-eproof
    (throwfail "Can't use current-eproof outside mating-search. ")))

(defgwff-type last-eproof-type
  (checkfn last-eproof-ckfn)
  (getfn last-eproof-getfn)
  (mhelp "last-eproof : The name for the last expansion proof when outside mating search."))

(defun last-eproof-ckfn (xxx)
  (eq xxx 'last-eproof))

(defun last-eproof-getfn (xxx)
  (declare (ignore xxx))
  (if (and (boundp 'last-eproof) last-eproof) last-eproof
    (throwfail "You haven't used mating-search yet!")))


;;;
;;; MATETOP is the mating-search top-level, locally binding all the important special
;;; variables.
;;;

(defun matetop ()
  (let ((top-prompt-fn #'mate-top-prompt)
	(mate-level (+ mate-level 1))
	(mate-written-p nil)
	(printmateflag (if (= mate-level 0) printmateflag nil))
	(command-interpreter #'mate-command-interpreter)
	(print-* #'mate-print-*)
	(top-level 'mate-top)
	(command-ctree mate-command-ctree))
    (declare (special top-prompt-fn mate-level command-interpreter
		      print-* top-level command-ctree mate-written-p
		      printmateflag
		      active-mating next-action connection result))
    (secondary-top)))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun mate-top-prompt (id)
  (declare (special mate-level mate-written-p))
  (let ((x (cond ((= mate-level 1) "") (T mate-level))))
    (format nil "<~[+~;-~;~;~]~:[~A:~;~A~]Mate~A>"
	    (cond (mate-written-p 0) (printmateflag 1) (t 2))
	    (= mate-level 1) x id)))

;;;
;;; MATE-COMMAND-INTERPRETER returns a form (PROGN form1 ... formn),
;;; where form1 ... formn are all forms which can be evaluated directly
;;; by LISP.
;;;
(defvar *mate-temp* nil)

(defun mateop-interpreter (command)
   (declare (special rest))
   (let* ((alias (get command 'mate-alias))
	  (result (get command 'mate-result->))
	  (resulttype (getkey command alias 'resulttype)))
		   (cond ((get command 'mate-move-fn)
			  `(%catch%
			    (progn
			      (setq *mate-temp* 
				(funcall ',alias current-topnode))
			      (push current-topnode nodestack)
			      (setq current-topnode *mate-temp*))
			    (fail (complain core::expand-catch-throw t)
				  (complain "Operation " ',command
					    " not performed."))))
			 ((and (get command 'mateop)
			       (getkey command alias 'print-op))
			  `(funcall ',alias current-topnode))
			 (t (let* ((no-args (length (getkey command
							    alias
							    'argtypes)))
				   (more-p (> (length rest) no-args))
				   (new-rest (if more-p (nthcdr no-args rest)
						 nil))
				   (args (if more-p (ldiff rest new-rest)
					     rest)))
			      (setq rest new-rest)
			      `(%catch%
				(progn
				  (setq *mate-temp*
				    (mate-opdecode ',(cons command args)))
				  ,(cond ((eq result 'current-topnode)
					  `(setq current-topnode *mate-temp*)
					  )
					 ((or (eq result
						  'execute)
					      (eq 'mate-command
						  resulttype))
					  `(eval (mate-command-interpreter 
						  *mate-temp*)))
					 ((eq result 'ignore)
					  'nil)
					 (result 
					  `(funcall ',result
						    *mate-temp*))
					 ((get
					   resulttype
					   'printfn)
					  `(funcall
					    ',(get resulttype
						   'printfn)
					    *mate-temp*))))
				(fail (complain core::expand-catch-throw t)
				      (complain "Operation not performed."))))))))

(defun mate-command-interpreter (cmd)
  (declare (special displaywff printdepth ppwfflag  prev-node nodestack
		    cmdstack strong-defaults  printtypes current-eproof
		    current-topnode))
  (do ((cmdlist nil (cons nextcmd cmdlist))
       (cmd cmd rest)
       (command (car cmd) (car rest))
       (rest (cdr cmd) (cdr rest))
       (nextcmd nil))
       ((null cmd)
	`(progn (setq mate-written-p nil) ,@(nreverse cmdlist)))
    (declare (special rest))
    (setq nextcmd
	  (cond ((integerp command)
		 (if (zerop command)
		     '(negate-last-move current-topnode)
		     `(move-to-successor ,command)))
		((consp command) (consp-interpreter command))
		((and (symbolp command)
		      (or (get command 'mateop) (get command 'wffop)))
                 (mateop-interpreter command))
		((and (symbolp command)
		      (or (get command 'mexpr) (get command 'reviewcmd) (get command 'monitorfn)))
                 (mexpr-interpreter command))
		((and (symbolp command) (get command 'flag))
                 (flag-interpreter command))
                (t (misc-interpreter command))))))

;;;
;;; MATE-PRINT-* prints the current topnode, if it is not the same as before
;;; the last command.
;;;

(defun mate-print-* (result)
  (declare (special prev-node)
	   (ignore result))
  (when (not (eq current-topnode prev-node))
	(print-etree current-topnode *standard-output* nil)
	(setq prev-node current-topnode)))

;;;
;;; Following are some EDOPS which allow one to exit or move backwards
;;; through the edited formulas.
;;;

(context scribe-record)

(defmateop o
  (mate-alias invert-printmateflag)
  (mhelp "Invert PRINTMATEFLAG, that is switch automatic recording of mating-search
into a file either on or off. This has not actually been implemented!"))

(defun invert-printmateflag ()
  (setq printmateflag (not printmateflag)))

(defmateop rem
  (mate-alias remark-printmatefile)
  (mate-result-> ignore)
  (mhelp "Write a remark into the PRINTMATEFILE."))

(defwffop remark-printmatefile
  (argnames rm)
  (argtypes string)
  (arghelp "Remark")
  (mhelp "Write a remark into the PRINTMATEFILE."))


(context subtoplevels)

(defmateop leave
  (mate-alias exit-ms)
  (mhelp "Exit mating-search.  If the current expansion proof is
complete, the user will be prompted as to whether to apply MERGE-TREE 
before exiting."))

(context mating-search)

(defmateop noop
  (mate-alias ednoop)
  (mhelp "Do nothing. (TPS uses this internally.)"))

(defmateop go
  (mate-alias matingsearch-controller)
  (mhelp "Start mating search using default mating search (controlled
by flag DEFAULT-MS)."))

(defflag default-ms
  (flagtype searchtype)
  (default ms90-3)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms98-1 ms03-7 ms04-2 mtree-top important transmit)
  (change-fn (lambda (flag value pvalue)
	       (declare (ignore flag))
	       (when (neq value pvalue) (update-otherdefs value))))
  (relevant-kids ((eq default-ms 'ms98-1) '(BREAK-AT-QUANTIFIERS DEFAULT-MS FIRST-ORDER-MODE-MS MAX-MATES MAX-SUBSTS-QUICK 
					    MAX-SUBSTS-VAR MERGE-MINIMIZE-MATING MIN-QUANTIFIER-SCOPE 
					    MS98-FIRST-FRAGMENT MS98-FRAGMENT-ORDER MS98-INIT MS98-MEASURE 
					    MS98-NUM-OF-DUPS MS98-REWRITE-DEPTH MS98-REWRITE-SIZE 
					    MS98-REWRITE-UNIF MS98-REWRITES MS98-VERBOSE NUM-OF-DUPS REWRITE-DEFNS 
					    REWRITE-EQUALITIES REWRITE-EQUIVS SKOLEM-DEFAULT
					    FF-DELAY HPATH-THRESHOLD MAXIMIZE-FIRST 
					    MS98-FORCE-H-O MS98-LOW-MEMORY MS98-MAX-COMPONENTS MS98-MERGE-DAGS MS98-MINIMALITY-CHECK 
					    MS98-REWRITE-MODEL MS98-REWRITE-PRUNE MS98-TRACE MS98-UNIF-HACK 
					    MS98-UNIF-HACK2 MS98-VALID-PAIR MS98-VARIABLE-ORDER)))
  (irrelevant-kids ((neq default-ms 'ms98-1) '(ms98-first-fragment ms98-init ms98-fragment-order
					       ms98-measure ms98-num-of-dups ms98-rewrite-depth
					       ms98-rewrite-size ms98-rewrite-unif ms98-rewrites ms98-verbose
					       ms98-force-h-o ms98-low-memory
					       ms98-max-components ms98-merge-dags ms98-minimality-check
					       ms98-rewrite-model ms98-rewrite-prune ms98-trace ms98-unif-hack ms98-unif-hack2
					       ff-delay ms98-valid-pair ms98-variable-order hpath-threshold maximize-first)))
  (mhelp "The default mating search procedure to be used when either the DIY command or the
mate level GO command is invoked. This will be changed if you set the 
DEFAULT-MATE and DEFAULT-EXPAND flags (they may also change DEFAULT-MS 
to NIL, if you pick a non-existent combination -- see the help messages
for those flags). Conversely, setting DEFAULT-MS will set the values 
of DEFAULT-MATE and DEFAULT-EXPAND, as follows:
(Notice that for otree and oset searches, the actual primsubs generated will
depend on the setting of PRIMSUB-METHOD.)
        DEFAULT-EXPAND:        |  NONE   |  OTREE  |  OSET  |
       ========================+=========+=========+========+
DEFAULT-MATE:          NPFD    |  MS88   |   MS89  | MS91-6 |
                 --------------+---------+---------+--------+
                      NPFD-1   | MS92-9  |  MS93-1 |  N/A   |
                 --------------+---------+---------+--------+
                        PFD    | MS90-3  |  MS90-9 | MS91-7 |
                 --------------+---------+---------+--------+
                      MTREE    | MT94-11 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-1   | MT94-12 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-2   | MT95-1  |   N/A   |  N/A   |
                 --------------+---------+---------+--------+

(Setting DEFAULT-MS to MS98-1, MS03-7 or MS04-2 will also set both DEFAULT-EXPAND and DEFAULT-MATE
to MS98-1, MS03-7 or MS04-2, since those procedures don't really fit into the above table.)
Possible values are MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, 
MS93-1, MT94-11, MT94-12, MT95-1, MS98-1, MS03-7 and MS04-2."))

(defflag default-expand
  (flagtype symbol)
  (default otree)
  (change-fn (lambda (flag value pvalue)
	       (declare (special default-mate))
	       (if (memq value '(none otree oset ms98-1 ms03-7 ms04-2))
		   (when (neq value pvalue) (update-defms value default-mate))
		 (progn
		   (msgf value " is an illegal value for DEFAULT-EXPAND.")
		   (set flag pvalue)))))
 (subjects mating-search  ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 mtree-top transmit)
 (mhelp "Used with DEFAULT-MATE to determine a setting for DEFAULT-MS.
Combinations marked N/A will result in DEFAULT-MS being set to NIL.
Notice that for otree and oset searches, the actual primsubs generated will
depend on the setting of PRIMSUB-METHOD.
Takes values: none, ms98-1, ms03-7, ms04-2, otree and oset.
The values MS98-1, MS03-7 and MS04-2 are exceptional settings used for both this flag and 
DEFAULT-MATE to denote the MS98-1, MS03-7 and MS04-2 procedures.
Changes DEFAULT-MS as follows:
        DEFAULT-EXPAND:        |  NONE   |  OTREE  |  OSET  |
       ========================+=========+=========+========+
DEFAULT-MATE:          NPFD    |  MS88   |   MS89  | MS91-6 |
                 --------------+---------+---------+--------+
                      NPFD-1   | MS92-9  |  MS93-1 |  N/A   |
                 --------------+---------+---------+--------+
                        PFD    | MS90-3  |  MS90-9 | MS91-7 |
                 --------------+---------+---------+--------+
                      MTREE    | MT94-11 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-1   | MT94-12 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-2   | MT95-1  |   N/A   |  N/A   |
                 --------------+---------+---------+--------+"))

(definfo otree
  (mhelp "A setting for DEFAULT-EXPAND.
Use a mating search that has option trees. (MS89, MS93-1 or MS90-9)"))

(definfo oset
  (mhelp "A setting for DEFAULT-EXPAND.
Use a mating search that has option sets. (MS91-6 or MS91-7)"))

(defflag default-mate
  (flagtype symbol)
  (default pfd)
  (change-fn (lambda (flag value pvalue)
	       (if (memq value '(npfd npfd-1 pfd mtree mtree-1 mtree-2 ms98-1 ms03-7 ms04-2))
		   (when (neq value pvalue) (update-defms default-expand value))
		 (progn
		 (msgf value " is an illegal value for DEFAULT-MATE.")
		 (set flag pvalue)))))
 (subjects mating-search  ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 mtree-top transmit)
 (mhelp "Used with DEFAULT-EXPAND to determine a setting for DEFAULT-MS.
Combinations marked N/A will result in DEFAULT-MS being set to NIL.
(Notice that for otree and oset searches, the actual primsubs generated will
depend on the setting of PRIMSUB-METHOD.)
Takes values: ms98-1, ms03-7, ms04-2, npfd, npfd-1, pfd, mtree, mtree-1 and mtree-2.
The values MS98-1, MS03-7 and MS04-2 are exceptional settings used for both this flag and 
DEFAULT-EXPAND to denote the MS98-1, MS03-7 and MS04-2 procedures.
Changes DEFAULT-MS as follows:
        DEFAULT-EXPAND:        |  NONE   |  OTREE  |  OSET  |
       ========================+=========+=========+========+
DEFAULT-MATE:          NPFD    |  MS88   |   MS89  | MS91-6 |
                 --------------+---------+---------+--------+
                      NPFD-1   | MS92-9  |  MS93-1 |  N/A   |
                 --------------+---------+---------+--------+
                        PFD    | MS90-3  |  MS90-9 | MS91-7 |
                 --------------+---------+---------+--------+
                      MTREE    | MT94-11 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-1   | MT94-12 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-2   | MT95-1  |   N/A   |  N/A   |
                 --------------+---------+---------+--------+"))

(definfo npfd
  (mhelp "A setting for DEFAULT-MATE.
Use a non-path-focused procedure (MS88, MS89 or MS91-6)."))

(definfo ms98-1
  (mhelp "A setting for DEFAULT-MATE and DEFAULT-EXPAND.
Use the MS98-1 procedure."))

(definfo npfd-1 
  (mhelp "A setting for DEFAULT-MATE.
Use a non-path-focused version of a path-focused procedure 
(MS92-9 or MS93-1)"))

(definfo pfd 
  (mhelp "A setting for DEFAULT-MATE.
Use a path-focused procedure (MS90-3, MS90-9 or MS91-7)"))

(definfo mtree
  (mhelp "A setting for DEFAULT-MATE.
Use the matingstree procedure MT94-11."))

(definfo mtree-1
  (mhelp "A setting for DEFAULT-MATE.
Use the matingstree procedure MT94-12."))

(definfo mtree-2
  (mhelp "A setting for DEFAULT-MATE.
Use the matingstree procedure MT95-1."))

(defun update-defms (expand mate)
  (setq default-ms
	(case expand
	      (none (case mate
			  (npfd 'ms88)
			  (npfd-1 'ms92-9)
			  (pfd 'ms90-3)
			  (mtree 'mt94-11)
			  (mtree-1 'mt94-12)
			  (mtree-2 'mt95-1)))
	      (otree (case mate
			   (npfd 'ms89)
			   (npfd-1 'ms93-1)
			   (pfd 'ms90-9)
			   (t nil)))
	      (oset (case mate
			  (npfd 'ms91-6)
			  (pfd 'ms91-7)
			  (t nil)))
	      (ms98-1 'ms98-1)
	      (ms03-7 'ms03-7)
	      (ms04-2 'ms04-2))))

(defun update-otherdefs (ms)
  (setq default-expand
	(case ms
	      ((ms88 ms92-9 ms90-3 mt94-11 mt94-12 mt95-1) 'none)
	      ((ms89 ms93-1 ms90-9) 'otree)
	      ((ms91-6 ms91-7) 'oset)
	      ((ms98-1) 'ms98-1)
	      ((ms03-7) 'ms03-7)
	      ((ms04-2) 'ms04-2)))
  (setq default-mate
	(case ms
	      ((ms88 ms89 ms91-6) 'npfd)
	      ((ms92-9 ms93-1) 'npfd-1)
	      ((ms90-3 ms90-9 ms91-7) 'pfd)
	      ((mt94-11) 'mtree)
	      ((mt94-12) 'mtree-1)
	      ((mt95-1) 'mtree-2)
	      ((ms98-1) 'ms98-1)
	      ((ms03-7) 'ms03-7)
	      ((ms04-2) 'ms04-2))))

(defun matingsearch-controller (&rest ignore)
   (declare (ignore ignore) (special *unif-stats-store* temp-num-of-dups ms88-unif-counter))
   (startcount 'mating-ctr) ;hx: April. 18, 1993
   (setq *unif-stats-store* nil)
   (setq temp-num-of-dups nil)
   (setq ms88-unif-counter 0)
   (unwind-protect  
      (matingsearch-controller-real)
      (breakcount 'mating-ctr)))

(defflag use-fast-prop-search
  (flagtype boolean)
  (default t)
  (subjects mating-search transmit)
  (mhelp "If T, will attempt to use the path-focused fast propositional
theorem prover on all problems, before switching to the usual default
mating-search if this fails. If NIL, will use the default mating-search 
only."))

(defvar *fps-succeeded* nil)
;use this to tell spanning-clist-p whether a fast prop. search was successful

;;;It is unnecessary to apply MS90-9 to first-order problems.
(defun matingsearch-controller-real (&rest ignore)
  (declare (ignore ignore) (special num-of-splitting))
  (setq num-of-splitting 0);In order to cope with the splitting while recording time
  (setq *fps-succeeded* nil)
  (prog1
      (if (and use-fast-prop-search (conjunction-p (eproof-jform current-eproof)))
	  (if (eq (mate-prop-msearch-silent) 'fail)
	      (if first-order-mode-ms 
		  (case default-ms
			((ms88 ms89 ms91-6) (ms88-controller))
			((ms90-3 ms90-9 ms91-7 ms92-9 ms93-1)
			 (if (ms90-3-controller) (expand-etree)))
			(ms98-1 (ccs))
			((ms03-7 ms04-2) (msgf default-ms " is intended for extensional mating search only.
See the EXT-MATE top level."))
			(otherwise (msgf "DEFAULT-MS must take as values only mating search procedures.
Possible values are MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, MS93-1, MS98-1, MS03-7 and MS04-2.")))
		(case default-ms
		      (ms88 (ms88-controller))
		      (ms89 (auto-search-ms89))
		      (ms90-3 (if (ms90-3-controller) (expand-etree)))
		      (ms90-9 (if (auto-search-ms90-9) (expand-etree)))
		      (ms91-6 (auto-search-ms91-6))
		      (ms91-7 (if (auto-search-ms91-7) (expand-etree)))
		      (ms92-9 (if (ms92-9-controller) (expand-etree)))
		      (ms93-1 (if (auto-search-ms93-1) (expand-etree)))
		      (ms98-1 (ccs))
		      ((ms03-7 ms04-2) (msgf default-ms " is intended for extensional mating search only.
See the EXT-MATE top level."))
		      (otherwise (msgf "DEFAULT-MS must take as values only mating search procedures.
Possible values are MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, MS93-1, MS98-1, MS03-7 and MS04-2."))))
	    (progn (runcount 'mating) (msgf "Fast propositional search succeeded." t) (setq *fps-succeeded* t) t))
					;if not using fast-prop-search
	(if first-order-mode-ms 
	      (case default-ms
		    ((ms88 ms89 ms91-6) (ms88-controller))
		    ((ms90-3 ms90-9 ms91-7 ms92-9 ms93-1)
		     (if (ms90-3-controller) (expand-etree)))
		    (ms98-1 (ccs))
		    ((ms03-7 ms04-2) (msgf default-ms " is intended for extensional mating search only.
See the EXT-MATE top level."))
		    (otherwise (msgf "DEFAULT-MS must take as values only mating search procedures.
Possible values are MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, MS93-1, MS98-1, MS03-7 and MS04-2.")))
	    (case default-ms
		  (ms88 (ms88-controller))
		  (ms89 (auto-search-ms89))
		  (ms90-3 (if (ms90-3-controller) (expand-etree)))
		  (ms90-9 (if (auto-search-ms90-9) (expand-etree)))
		  (ms91-6 (auto-search-ms91-6))
		  (ms91-7 (if (auto-search-ms91-7) (expand-etree)))
		  (ms92-9 (if (ms92-9-controller) (expand-etree)))
		  (ms93-1 (if (auto-search-ms93-1) (expand-etree)))
		  ((ms03-7 ms04-2) (msgf default-ms " is intended for extensional mating search only.
See the EXT-MATE top level."))
		  (ms98-1 (ccs))
		  (otherwise (msgf "DEFAULT-MS must take as values only mating search procedures.
Possible values are MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, MS93-1, MS98-1, MS03-7 and MS04-2.")))))
	(runcount 'mating-ctr)))


;;;
;;; NEGATE-LAST-MOVE undoes the last moving 
;;; command like A, D, L, R.
;;;

(defun negate-last-move (current)
  (declare (special nodestack))
  (if nodestack
      (progn
       (setq current-topnode (pop nodestack))
       (push current nodestack))
      (throwfail "No previous nodes.")))

(defun exit-ms ()
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
  (%throw% '|[Normal Exit of Mating-Search.]| exit-inferior-top))

;;;
;;; ABORT-MS quits the mating-search top-level, signalling an error.  
;;;

(defun abort-ms ()
  (%throw% '|[Mating-search aborted.]| quit-inferior-top))



(defun remark-printmatefile (rm)
  (declare (special scribe-preamble))
  (when (not printmateflag)
    (ttymsg "Not recording at the moment.  Writing remark anyway."))
  (reroute-output-append printmatefile (make-pathname% :name "mate" :type "mss")
    (if (not existent-file)
	(if printmateflag-slides (msg slides-preamble t)
	    (msg scribe-preamble t))) ; was printmatefile-preamble
    (msg T rm T))
  (when (and (boundp printvpdflag) printvpdflag)
    (reroute-output-append vpd-filename (make-pathname% :type "vpf")
      (msg T rm T))))




(defun mate-opdecode (command)
  (declare (special strong-defaults))
  (let ((keyword (car command))
	(alias (get (car command) 'mate-alias))
	 appfn mainfn result)
    (multiple-value-bind
     (internal-arglist external-arglist)
     (prompt-values keyword
		    (copy (cdr command))
		    (getkey keyword alias 'argtypes)
		    (or (getkey keyword alias 'wffargtypes)
			(mapcar #'(lambda (x) (declare (ignore x)) nil)
				(getkey keyword alias 'argtypes)))
		    (getkey keyword alias 'wffop-typelist)
		    (getkey keyword alias 'defaultfns)
		    (funcall strong-defaults keyword alias)
		    (get-mate-argnames keyword)
		    (getkey keyword alias 'arghelp))
     (declare (ignore external-arglist))
    (setq appfn (or (get keyword 'mate-applicable-p)
		    (getkey keyword alias 'applicable-p)
		    (getkey keyword alias 'applicable-q)))
    (if (and appfn (not (apply appfn internal-arglist)))
	(throwfail keyword " not applicable."))
    (setq mainfn (cond (alias) (t keyword)))
    (%catch% (setq result (apply mainfn internal-arglist))
	     (fail (complain f "Error from " mainfn ".  " 
                             core::expand-catch-throw)
		   (throwfail "Operation aborted.")))
    result)))


(defun get-mate-argnames (fn-name)
  (cond ((get fn-name 'argnames))
	((get fn-name 'mate-alias) (get (get fn-name 'mate-alias) 'argnames))
	(t nil)))

