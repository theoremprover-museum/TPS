;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of TPS-HELP)

(deffile mhelp
  (part-of tps-help)
  (extension lsp)
  (mhelp "Defines general TPS help facilities: HELP and ENVIRONMENT."))

(context help-obj)

(defvar *doing-html* nil)

;;; Keeps track of whether all symbols will be shown or only accessible
;;; ones.
(eval-when (compile load eval)
(defflag show-all-packages
  (flagtype boolean)
  (default nil)
  (subjects maintain)
  (mhelp "Determines whether ENVIRONMENT will show symbols in all packages
or merely accessible symbols."))
)

(defmexpr ?
  (mainfns top-help)
  (dont-restore T)
  (mhelp "Type ? to obtain a list of possible options."))

(defmexpr ??
  (mainfns help-help)
  (dont-restore T)
  (mhelp "Type ?? to get general help on TPS, command completion
and history substitution."))

(defmexpr help*
  (argtypes help*-list)
  (argnames keywords)
  (arghelp "List of TPS objects")
  (dont-restore T)
  (mainfns mhelp*)
  (mhelp
   "Give information about each of a list of TPS objects.
This is equivalent to doing HELP on each of them.
The amount of help given for inference rules may be changed by setting the
flag SHORT-HELP."))

(defun mhelp* (objectlist)
  (dolist (object objectlist)
	  (msgf)
	  (mhelp object)
	  (msgf "------" t)))

(defmexpr help-group
  (argtypes symbol-or-integer-list)
  (argnames keywords)
  (arghelp "TPS objects")
  (dont-restore T)
  (mainfns mhelp-group)
  (mhelp
   "Give information about a group of TPS objects; specifically,
given the name of a category, a context, or a top level, 
list the help messages for every object in that class.
If given a list of names, it will list the help messages
for all the objects that fall into the intersection of these
classes (e.g. HELP-GROUP (MEXPR REWRITING) will show all the 
top-level commands in the context REWRITING).
NOTE: Remember that the name of a context is not necessarily
the name that prints on the screen; do HELP CONTEXT
to show their real names."))

(defun mhelp-group (keywordlist)
  (let ((gdefl (remove-duplicates global-definelist :test #'(lambda (x y) (equal (cdr x) (cdr y)))))
	(done T))
    (setq gdefl (mapcar #'(lambda (x) (remove-duplicates (eval (get (cdr x) 'global-list)) :test #'equal)) gdefl))
    (setq gdefl (reduce 'append gdefl))
    (setq gdefl (remove-duplicates (remove-if #'listp gdefl)))
    (dolist (keyword keywordlist)
	    (when (eq keyword 'ed) (setq keyword 'ed-top))
	    (when (eq keyword 'lib) (setq keyword 'library-top))
	    (when (eq keyword 'mtree) (setq keyword 'mtree-top))
	    (when (eq keyword 'review) (setq keyword 'review-top))
	    (when (eq keyword 'test) (setq keyword 'test-top))
	    (when (eq keyword 'mate) (setq keyword 'mate-top))
	    (when (eq keyword 'unify) (setq keyword 'unif-top))
	    (when (eq keyword 'grader) (setq keyword 'grade-top))
	    (when (eq keyword 'cmd) (setq keyword 'cmd-top))
	    (when (eq keyword 'top) (setq keyword 'cmd-top))
;the above are hacks to change the mexpr for a top level into the name of the top level itself.
	    (when (member keyword global-toplevellist)
		  (setq keyword (get keyword 'top-level-category)))
	    (when (member keyword global-contextlist)
		  (setq gdefl (remove-if-not #'(lambda (x) (right-context keyword x)) gdefl))
		  (setq done nil))
	    (when (member keyword global-categorylist)
		  (let ((foo (remove-if #'listp (eval (get keyword 'global-list)))))
		    (setq gdefl (remove-if-not #'(lambda (x) (memq x foo)) gdefl)))
		  (setq done NIL)))
    (setq gdefl (sort (remove-duplicates gdefl) #'(lambda (x y) (string< (princ-to-string x) (princ-to-string y)))))
    (mhelp* gdefl)
    (if done (msgf "Don't know how to do HELP-GROUP for " keywordlist "." t
		   "Try using HELP instead." t))))

(defun right-context (context object)
  (let ((clist (get object 'core::contexts)))
    (setq clist (mapcar #'cdr clist))
    (memq context clist)))

(defmexpr help
  (argtypes symbol-or-integer)
  (argnames keyword)
  (arghelp "TPS object")
  (defaultfns (lambda (x) (list (if (eq x '$) 'HELP x))))
  (dont-restore T)
  (mainfns mhelp)
  (mhelp
   "Give information about a TPS object like a command or argument type.
The amount of help given for inference rules may be changed by setting the
flag SHORT-HELP.

Online help can be found at the web site:
 
http://gtps.math.cmu.edu/tps.html

Typing \"?\" will show you all available commands at this level.

The web site includes online documentation as well
as postscript manuals."))

(defun mhelp (keyword &optional (key-cats nil))
  (declare (special short-help auto::mate-level auto::mtree-level rightmargin))
  (if (integerp keyword) 
      (if (zerop keyword) (setq keyword '|0|) (setq keyword 'weeble-ftang-banana)))
;couldn't find the unif/mateop 0 otherwise. OK, so it's a hack. I admit it... MB 4/94
  (dolist (cat global-categorylist)
    (if (and (eq t (get keyword cat)) (neq cat 'context))
	(push cat key-cats)))
  (if (member keyword global-contextlist) (push 'context key-cats))
  (if (member keyword global-categorylist) (push 'category key-cats))
  (if (eq keyword 'category) (push 'basic-object key-cats))
  (setq key-cats (setdiff key-cats '(MENU MENUITEM)))
  (if key-cats
      (progn
	(when *using-interface* (change-color 'blue))
	(if (and (boundp 'short-help) short-help (member 'srule key-cats))
	    (help-rule keyword)
	  (progn
	    (if (and (member 'srule key-cats) (not (member 'rulehelp key-cats)))
		(if (get-rule-help keyword (find-rule-file keyword))
		    (push 'rulehelp key-cats))) ; get irule defn...
            #|(msgf keyword " is a ")
	   (do ((cats key-cats (cdr cats)))
	       ((null cats))
	     (msg (cat-hack (car cats) 'mhelp-line))
	     (if (cdr cats) (msg ", ") (msg ".")))
           |#
            ;; The following does a little more smart formatting of
            ;; multiple arguments, and also decides whether to put an
            ;; "an" or an "a" before a word depending on whether it
            ;; begins with a vowel or not.  Fails on words like
            ;; "unification", which actually begins with a consonant
            ;; sound. 
            ;; Output should be like this "foo is an A, a B, and a C." 
            ;; DAN 11MAY90
	    (format t "~A is~?.~%" 
		    keyword
		    "~#[~; ~1{~A ~A~}~; ~1{~A ~A~} and ~1{~A ~A~}~:;~@{~#[~; and~] ~1{~A ~A~}~^,~}~]"
		    (mapcar #'(lambda (x)
				(let* ((str (string x))
				       (len (length str))
				       (ch (if (> len 0) 
					       (char str 0))))
				  (if ch
				      (if (member ch 
						  '(#\a #\A #\e #\E #\i #\I #\o #\O #\u #\U))
					  (list "an" str)
					(list "a" str))
				    (list "" ""))))
			    (mapcar #'(lambda (x) (cat-hack x 'mhelp-line))
				    key-cats)))
	    
	    (do ((cats key-cats (cdr cats)))
		((null cats))
		(when (cdr key-cats)
		  (if alpha-lower-flag
		      (let* ((string (string-upcase (cat-hack (car cats) 'mhelp-line)))
			     (sample-prompt (let ((ed-level 1)
						  (oldml (when (find-package 'auto) auto::mate-level))
						  (oldmtl (when (find-package 'auto) auto::mtree-level))
						  (foo nil))
					      (declare (special ed-level))
					      (setq foo (dolist (topl (remove-if-not 'symbolp global-toplevellist) "")
								(when (find-package 'auto)
								  (setq auto::mate-level 1 auto::mtree-level 1))
								(when (eq (get topl 'top-level-category) (car cats))
								  (return (funcall (get topl 'top-prompt-fn) "n")))))
					      (when (find-package 'auto)
						(setq auto::mate-level oldml auto::mtree-level oldmtl))
					      foo))
			     (string (concatenate 'string  string 
						  (if (string= sample-prompt "") "" " ")
						  (princ-to-string sample-prompt)))
			     (l (length string))
			     (gap (- (- rightmargin l) 2)) ;we ignore leftmargin, since HELP does.
			     (ldash (if (oddp gap) (floor (/ gap 2)) (/ gap 2)))
			     (rdash (if (oddp gap) (1+ ldash) ldash)))
			(progn
			  (when *using-interface* (change-color 'green))
			  (msgf t (if (> ldash 0) (make-string ldash :initial-element #\-) "----------")
				string
				(if (> rdash 0) (make-string rdash :initial-element #\-) "----------"))
			  (when *using-interface* (change-color 'blue))))
		    (progn (msgf (t 5) "-----")
			   (msgf "As a " (cat-hack (car cats) 'mhelp-line) ":" t))))
		(funcall (cat-hack (car cats) 'mhelp-fn) keyword (car cats)))))
	(when *using-interface* (change-color 'black)))
    (progn
      (when *using-interface* (change-color 'red))
      (msgf (if (eq keyword 'weeble-ftang-banana) "That" keyword)
	    " is not recognized as a TPS object.")
      (when *using-interface* (change-color 'black)))))

(defun mhelp-for-cat (keyword category)
  (funcall (cat-hack category 'mhelp-fn) keyword category))

(defun cat-hack (category property)
  (case category
    (category (case property
		(mhelp-line "category. Use ENVIRONMENT to examine it further")
		(mhelp-fn 'cat-help-fn)
		(t (get 'category property))))
    (basic-object (case property
		    (mhelp-line "basic object. Use ENVIRONMENT to examine it further")
		    (mhelp-fn 'cat-help-fn)))
    (t (get category property))))

(defun cat-help-fn (keyword category)
  (declare (ignore category))
  (let ((mhelp (get keyword 'cat-help))
	(helpline (if (eq keyword 'category) "category" (get keyword 'mhelp-line)))
	(global (if (eq keyword 'category) global-categorylist (get keyword 'global-list))))
    (if (not mhelp) "Sorry. Help is not available."
	(if (consp mhelp) (eval (cons 'msgf mhelp))
	    (msg f mhelp)))
    (if (eq keyword 'utility)
	(msg "UTILITIES are lisp functions and not TPS commands, and so cannot be
used from the TPS prompt unless you have EXPERTFLAG set to T."))
    (if global (progn (msgf "Defined as a " helpline " are: ") 
		      (if (eq keyword 'category) 
			  (msgf (if alpha-lower-flag 
				    (mapcar #'string-downcase global-categorylist) global-categorylist))
			(category-items-short-help keyword))))))

(defun categ-mhelp (tps-obj category)
  (let ((mhelp (get tps-obj 'mhelp)))
    (if (and mhelp (not (consp mhelp)))
	(progn (complain f "The HELP for " tps-obj " has been clobbered."
			 t "Please notify your friendly TPS maintainer.")
	       '"*****
 This HELP was clobbered by some TPS2 Bug, possibly because a category
 and another TPS-object had the same name.
*****")
	(cdr (assoc category (get tps-obj 'mhelp))))))

(defun princ-mhelp (keyword category)
  (let ((mhelp (or (categ-mhelp keyword category)
		   (when (eq category 'edop)
		     (categ-mhelp (get keyword 'alias) 'wffop)))))
    (if mhelp (if (consp mhelp) (eval (cons 'msgf mhelp)) (msg f mhelp))
	(msgf "No more help available.  Sorry."))))

(defun princ-short-mhelp (keyword category)
  (let ((mhelp (or (categ-mhelp keyword category)
		   (when (eq category 'edop)
		     (categ-mhelp (get keyword 'alias) 'wffop)))))
    (if mhelp (if (consp mhelp) (eval (cons 'msg mhelp)) (princ mhelp))
	(msg "No help available.  Sorry."))))

(defun category-mhelp (keyword)
  (princ-mhelp keyword 'category)
  (category-mhelp-list keyword))

(defun category-mhelp-list (keyword)
  (when (get keyword 'global-list)
	(msgf "Currently any of: " t)
	(category-items-short-help keyword)))

(defun command-mhelp (keyword prompt category)
  (declare (special short-help))
  (princ-mhelp keyword category)
  (unless short-help 
	  (msgf "The command format for " keyword " is:" -2)
	  (when *doing-html* (msg " fnord "))
	  (msg prompt keyword)
	  (print-tps-format keyword '" "
			    (1+ (length (format nil "~A~A" prompt keyword))))
	  (princ-arghelps keyword)))

(defun operation-mhelp (keyword category)
  (declare (special short-help))
  (princ-mhelp keyword category)
  (unless short-help
	  (when *doing-html* (msg " fnord "))
	  (msgf "The calling scheme for " keyword " is:" -2 "(" keyword)
	  (print-tps-format keyword '")" (+ 2 (length (format nil "~A" keyword))))
	  (msg t "The result is of type " (get keyword 'resulttype) "." t)
	  (princ-arghelps keyword)))

(defun edop-mhelp (keyword category)
  (declare (special short-help))
  (princ-mhelp keyword category)
  (cond ((get keyword 'move-fn)
	 (msgf keyword " is a editor moving command."))
	((getkey keyword (get keyword 'alias) 'print-op)
	 (msgf keyword " is a printing command."))
	(t (unless short-help 		   
	     (when *doing-html* (msg " fnord "))
	     (msgf "The command format for " keyword " is:" -2 "<Ed>" keyword)
	     (print-tps-format keyword '" "
			       (+ 4 (length (format nil "~A" keyword))))
	     (if (eq (get keyword 'result->) 'edwff)
		 (msgf "The result replaces the current wff."))))))

(defun mateop-mhelp (keyword category)
  (declare (special short-help))
  (if (and (null (cdr (assoc category (get keyword 'mhelp))))
	   (get keyword 'auto::mate-alias))
	(princ-mhelp (get keyword 'auto::mate-alias) 'wffop)
	(princ-mhelp keyword category))
  (cond ((get keyword 'auto::mate-move-fn)
	 (msgf keyword " is a expansion-tree moving command."))
	((getkey keyword (get keyword 'auto::mate-alias) 'print-op)
	 (msgf keyword " is a printing command."))
	(t (unless short-help 
		   (when *doing-html* (msg " fnord "))
		   (msgf "The command format for " keyword " is:" -2 "<Mate>" keyword)
		   (print-tps-format* keyword " "
				      (+ 6 (length (format nil "~A" keyword)))
				      (if (get keyword 'auto::mate-alias)
					  (get (get keyword 'auto::mate-alias)
					       'argnames)
					(get keyword 'auto::mate-argnames))
				      (if (get keyword 'auto::mate-alias)
					  (get (get keyword 'auto::mate-alias)
					       'argtypes)
					(get keyword 'auto::mate-argtypes))
				      (if (get keyword 'auto::matewff-argname)
					  (list (get keyword 'auto::matewff-argname))
					nil))
		   (if (eq (get keyword 'auto::mate-result->) 'auto::current-topnode)
		       (msgf "The result replaces the current top node."))))))

(defun mtreeop-mhelp (keyword category)
  (declare (special short-help))
  (if (and (null (cdr (assoc category (get keyword 'mhelp))))
	   (get keyword 'auto::mtree-alias))
	(princ-mhelp (get keyword 'auto::mtree-alias) 'wffop)
	(princ-mhelp keyword category))
  (unless short-help
    (when *doing-html* (msg " fnord "))		
    (msgf "The command format for " keyword " is:" -2 "<Mtree>" keyword)
	  (let* ((alias (get keyword 'auto::mtree-alias))
		 (move (get keyword 'auto::mtree-move))
		 (argnames (and alias (get alias 'argnames)))
		 (argtypes (and alias (get alias 'argtypes)))
		 (length (and move (1- (length argnames)))))
	    (print-tps-format* keyword " "
			       (+ 9 (length (format nil "~A" keyword)))
			       (if move (subseq argnames 0 length) argnames)
			       (if move (subseq argtypes 0 length) argtypes)
			       nil))))

(defun princ-arghelps (keyword)
  (when (get keyword 'arghelp)
    (msg t "The arguments have the following meaning:")
    (do ((argnames (get keyword 'argnames) (cdr argnames))
	 (arghelps (get keyword 'arghelp) (cdr arghelps)))
	((null argnames))
      (msg t (string (car argnames)) " : " (car arghelps)))
    (msg t)))

;;Modifying PRINT-TPS-FORMAT to take another argument to eliminate call
;;to (CURPOS). The old fn (from TPS2) CEILING, now %CEILING% (this
;;should eventually be deleted), and fn CEILING compatible with the one
;;in CLISP are defined in file TML:MAC-CLISP.LSP. SI (11-24-85)

(eval-when (load compile eval)
(defmacro symbol-length (symbol)
  `(length (string ,symbol)))
)

(defun print-tps-format (keyword end-line position)
  (declare (special argnumbers))
  (prog (prtlengthpairs tabs argnames argtypes alias)
    (setq alias (get keyword 'alias))
    (setq argtypes (getkey keyword alias 'argtypes))
    (setq argnames
	  (cond ((getkey keyword alias 'argnames))
		(t (ldiff argnumbers (nthcdr (length argtypes) argnumbers)))))
    (setq prtlengthpairs
	  (mapcar #'(lambda (argtype argname)
		      (cons (if (eq argname (getkey keyword alias
						    'edwff-argname))
				(+ (symbol-length argtype) 4)
			      (if alpha-lower-flag (symbol-length argtype) (+ (symbol-length argtype) 2)))
			    (if (eq argname (getkey keyword alias
						    'edwff-argname))
				(+ 4 (symbol-length argname))
				(symbol-length argname))))
		  argtypes argnames))    
    (setq tabs (cons (1+ position) (copy prtlengthpairs)))
    (mapl #'(lambda (lengthpairstail)
	      (if (cdr lengthpairstail)
		  (rplaca (cdr lengthpairstail)
			  (+ 1 (car lengthpairstail)
			     (max (caadr lengthpairstail)
				  (cdadr lengthpairstail))))))
	  tabs)
    (mapc #'(lambda (argname tabpos lengthpair)
	      (msg (t tabpos))
	      (if (< (cdr lengthpair) (car lengthpair))
		  (spaces (ceiling (- (car lengthpair) (cdr lengthpair))
				   2)))
	      (if (eq argname (getkey keyword alias 'edwff-argname))
		  (msg "<<" (string argname) ">>")
		  (princ (string argname))))
	  argnames tabs prtlengthpairs)
    (msg end-line t)
    (when argnames 
	  (mapc #'(lambda (argtype argname tabpos lengthpair)
	      (msg (t tabpos))
	      (if (< (car lengthpair) (cdr lengthpair))
		  (spaces (ceiling (- (cdr lengthpair) (car lengthpair)) 2)))
	      (if (eq argname (getkey keyword alias 'edwff-argname))
		  (princ "<<") (unless alpha-lower-flag (princ "\"")))
	      (if alpha-lower-flag (princ (string-downcase (string argtype))) (princ (string argtype)))
	      (if (eq argname (getkey keyword alias 'edwff-argname))
		  (princ ">>") (unless alpha-lower-flag (princ "\""))))
	  argtypes argnames tabs prtlengthpairs)
	  (msg t))
    (msg t))
  (when *doing-html* (msg " FNORD "))
  )

(defun print-tps-format* (keyword end-line position argnames argtypes spec-list)
;; Spec-list contains a list of any arguments which should be printed with
;; brackets surrounding them.  
  (declare (special argnumbers) (ignore keyword))
  (let* ((prtlengthpairs
	   (mapcar #'(lambda (argtype argname)
		      (cons (if (memq argname spec-list)
				(+ (symbol-length argtype) 4)
				(if alpha-lower-flag (symbol-length argtype) (+ (symbol-length argtype) 2)))
			    (if (memq argname spec-list)
				(+ 4 (symbol-length argname))
				(symbol-length argname))))
		  argtypes argnames))
	 (tabs (cons (1+ position) (copy prtlengthpairs))))
    (mapl #'(lambda (lengthpairstail)
	      (if (cdr lengthpairstail)
		  (rplaca (cdr lengthpairstail)
			  (+ 1 (car lengthpairstail)
			     (max (caadr lengthpairstail)
				  (cdadr lengthpairstail))))))
	  tabs)
    (mapc #'(lambda (argname tabpos lengthpair)
	      (msg (t tabpos))
	      (if (< (cdr lengthpair) (car lengthpair))
		  (spaces (ceiling (- (car lengthpair) (cdr lengthpair))
				   2)))
	      (if (memq argname spec-list)
		  (msg "<<" (string argname) ">>")
		  (princ (string argname))))
	  argnames tabs prtlengthpairs)
    (msg end-line t)
    (when argnames 
	  (mapc #'(lambda (argtype argname tabpos lengthpair)
	      (msg (t tabpos))
	      (if (< (car lengthpair) (cdr lengthpair))
		  (spaces (ceiling (- (cdr lengthpair) (car lengthpair)) 2)))
	      (if (memq argname spec-list)
		  (princ "<<") (unless alpha-lower-flag (princ "\"")))
	      (if alpha-lower-flag (string-downcase (princ (string argtype))) (princ (string argtype)))
	      (if (memq argname spec-list)
		  (princ ">>") (unless alpha-lower-flag (princ "\""))))
	  argtypes argnames tabs prtlengthpairs)
	  (msg t))
    (msg t))
  (when *doing-html* (msg " FNORD "))
  )


(defun category-short-help ()
  (dolist (cat global-categorylist (msg t))
    (if (symbolp cat) (msg (if alpha-lower-flag (string-downcase cat) cat) " ")
	(msg t (get (car cat) 'short-id) ":"))))

(defun category-help ()
  (dolist (cat global-categorylist (msg t))
    (msg t cat (t 12) " - " (get cat 'mhelp-line))))

(defun category-items-short-help (category)
  (declare (special leftmargin))
  (pcall begin-environment "description, spread 0.3")
  (let ((newlist (eval (get category 'global-list))))
  (when alpha-lower-flag
	(setq newlist nil)
	(let ((templist nil))
	  (dolist (cat (reverse (eval (get category 'global-list))))
		  (if (symbolp cat)
		      (push cat templist)
		    (progn (push cat templist)
			   (push templist newlist)
			   (setq templist nil))))
	  (setq newlist (apply 'append (sort newlist #'string< 
					     :key #'(lambda (x) (princ-to-string (get (caar x) 'short-id))))))))
  (do ((cat-items newlist (cdr cat-items)))
      ((null cat-items) (msg t))
    (if (symbolp (car cat-items))
	(when (or (and show-all-packages expertflag)
		  (accessible-p (car cat-items)))
	  (pcall margin-correct (if alpha-lower-flag 
				    (string-downcase (princ-to-string (car cat-items)))
				  (car cat-items))))
	(if (or (and show-all-packages expertflag)
		(accessible-p (caar cat-items)))
	    ;; It is considered inconsistent if an TPS object is accessible,
	    ;; but not its context.  The check below will still
	    ;; print a context, even if there is no accessible object
	    ;; in the context.  This must be fixed later.
	    (progn (pcall print-nextpar)
		   (msg (if alpha-lower-flag 
			    (string-upcase (get (caar cat-items) 'short-id))
			  (get (caar cat-items) 'short-id)) ":")
		   (setq curpos (+ leftmargin (1+ (length (string-upcase (get (caar cat-items) 'short-id))))))
		   (pcall print-tab))
	    ;; Now we know the context is not accessible.  Skip everything
	    ;; in it.
	    (do () ((or (null (cdr cat-items))
			(not (symbolp (car (cdr cat-items))))))
	      (setq cat-items (cdr cat-items))))
	))
  (pcall end-environment "description")))

(defun category-items-help (category)
  (let ((newlist (remove-if-not #'symbolp (eval (get category 'global-list))))
	(skipping nil))
    (when alpha-lower-flag
	  (setq newlist nil)
	  (dolist (cat (eval (get category 'global-list)))
		  (if (symbolp cat)
		      (unless skipping (push cat newlist))
		    (if (or (and show-all-packages expertflag)
			    (accessible-p (car cat)))
			(setq skipping nil)
		      (setq skipping t))))
	  (setq newlist (sort newlist #'string< 
			      :key #'princ-to-string)))
    (do ((cat-items newlist (cdr cat-items)))
	((null cat-items) (msg t))
	(when (or (and show-all-packages expertflag)
		  (accessible-p (car cat-items)))
	      (msg t (car cat-items) (t 12) " - ")
	      (princ-short-mhelp (car cat-items) category)))))

;;;
;;; TOP-HELP is called from TPSTTYSCAN on encountering a ? before
;;; the first character is typed on a top-level with command completion.
;;; 
;;; Things are different in TPS3.  On a ? as a command, it merely calls
;;; this function normally.

(defun top-help ()
  (declare (special top-level))
  (reorganize-global-list (get top-level 'top-level-category))
  (category-items-short-help (get top-level 'top-level-category)))

;;;
;;; FEAT-HELP is currently only used by packages.  It lists the
;;; properties of KEYWORD which are in FEATURE-LIST, indenting
;;; The properties between LEFT and RIGHT.   ck.
;;;

(defun feat-help (keyword category left right feature-list)
  (princ-mhelp keyword category)
  (msg -2)
  (dolist (fel feature-list)
    (feat-help1 fel (get keyword fel) left right #'princ)))

(defun feat-help1 (fel feat left right printfn)
  (when feat
    (msg fel (t left))
    (if (listp feat)
	(do ((feats feat (cdr feats))
	     (where left))
	    ((not (consp feats))
	     (when feats (msg " . ") (funcall printfn feats))
	     (msg -2))
	  (let ((foot (flatc (car feats))))
	    (cond ((> (+ where foot) right) 
		   (msg (t left)) (setq where (+ left foot 5)))
		  (t (setq where (+ where foot 5))))
	    (funcall printfn (car feats)) (princ "     ")))
	(progn
	 (funcall printfn feat)
	 (msg -2)))))

(defun help-help ()
  (msgf
"Help commands:
? shows the commands available in the current top level.
?? shows this message.
HELP gets help about a particular object or command in TPS.
LIST shows all of the flags available in TPS.
ENVIRONMENT shows all of the objects available in TPS.
ABBREVIATIONS shows all of the abbreviations in TPS.
LIST-RULES shows all of the natural deduction rules in TPS.
PROBLEMS shows all of the exercises in TPS.
PROOFLIST shows all of the completed and partial proofs in TPS.
SEARCH and KEY look for given strings in the names of objects in TPS.

Command completion (see also the flag COMPLETION-OPTIONS) :
@ after a partial command name means \"complete this to a command name 
  or flag name\".
& joins together two commands on the same line.

History substitution (see also the flag HISTORY-SIZE) :

To select a particular line:
!n     is expanded to \"the contents of line n\".
!-n    is expanded to \"the contents of the line n lines above this one\" .
!!     is expanded to \"the previous line\" (i.e. !-1).
!str   is expanded to \"the most recent line that began with str\".
!?str? is expanded to \"the most recent line that contained str\".

To select a word (token) from a particular line, use one of the 
above followed by : followed by one of the following:
0   means \"the first word\".
n   means \"the nth word\" (i.e. the (n-1)st argument).
^   means \"the first argument\" (i.e. 1).
$   means \"the last argument\".
%   means \"the word matched by the immediately preceding ?str? search\".
x-y means \"words x through y inclusive\".
-y  means \"0-y\".
*   means \"^-$\" (or nothing, if there are no arguments on that line).
x*  means \"x-$\".
x-  means \"x* except for word $\".

Arguments to a command which can be accepted either on the command line
or at the argument prompt are:
$     means \"accept the default for this argument\".
$flag expands to \"the current value of the given flag\".
!     means \"accept the default for this and all subsequent arguments\".

Arguments to a command which can only be accepted at the argument prompt are:
?  means \"give me help about this argument type\".
?? means \"give me help about the current command\".
(at the top level, ? and ?? have different meanings -- see above).

For example:
ETREE-NAT !   (call etree-nat, accepting all defaults)
ETREE-NAT $ $ $ INTERACTIVE   (accepting all defaults but the last)
UNIF@ !  (call some command beginning with UNIF, accepting all defaults;
          this will only work if COMPLETION-OPTIONS is T)
HELP ETREE-NAT & HELP NAT-ETREE (the two commands will be run consecutively)
? & ABBREV@ ! (call ?, then call ABBREVIATIONS, accepting the defaults)
HELP $DEFAULT-MS (get help on the current mating search)
!!:0 (repeat the first word on the last line)

Manuals for TPS and ETPS, in Postscript or HTML form, are available at
http://gtps.math.cmu.edu/tps.html"))

(defmexpr oops
  (argtypes integer+ anything)
  (argnames position replacement)
  (arghelp "Number of word to replace (starting from 0)" "Replacement")
  (defaultfns (lambda (x y) (list (if (eq x '$) 0 x) y)))
  (mhelp "Replace the word at a given position in the previous line
with another word. Positions start from 0, and the substituted-for
command will be entered into the command history list, so for example:
<9>HELP GR-FILENAMES
<10>OOPS 0 LIST      (calls LIST GR-FILENAMES instead)
<11>OOPS 1 GR-MISC   (calls LIST GR-MISC)
"))

(defun oops (pos rep)
  (when (and history-size (= history-size 1)) (throwfail "OOPS can't work if HISTORY-SIZE = 1."))
  (when (stringp rep) (setq rep (concatenate 'string "\"" rep "\"")))
  (let ((expr  (if (= pos 0) (apply 'append (list (history-substitute-top (princ-to-string rep))
						  (history-substitute-top "!-2:1*")))
		 (if (<= (length (history-substitute-top "!-2")) (1+ pos))
		     (apply 'append 
			    (list (history-substitute-top (concatenate 'string "!-2:-" (princ-to-string (1- pos))))
				  (history-substitute-top (princ-to-string rep))))
		   (apply 'append
			  (list (history-substitute-top (concatenate 'string "!-2:-" (princ-to-string (1- pos))))
				(history-substitute-top (princ-to-string rep))
				(history-substitute-top (concatenate 'string "!-2:" (princ-to-string (1+ pos)) "-$"))))))))
    (format t "~{~A ~}~%" expr)
    (pop *command-history*)
    (push (cons (1+ (caar *command-history*)) expr) *command-history*)
    (eval (funcall (get top-level 'command-interpreter) (lisp-ize expr)))))

