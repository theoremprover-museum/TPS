;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of REVIEW-FLAGS)

;;;
;;; File REVIEW
;;;
;;; defines REVIEW, FLAGS etc.
;;;

(deffile review
  (part-of review-flags)
  (extension clisp)
  (mhelp "Defines top-level for reviewing flags."))

(context subtoplevels)

(deftoplevel review-top
  (top-prompt-fn review-top-prompt)
  (command-interpreter review-command-interpreter)
  (print-* review-print-*)
  (top-level-category reviewcmd)
  (top-level-ctree review-command-ctree)
  (top-cmd-decode comdecode)
  (mhelp "The top level of REVIEW."))

(defmexpr review
  (mhelp "Enter REVIEW to examine and change flags or parameters."))

(eval-when (load compile eval)
(defcategory reviewcmd
  (define defreview)
  (properties
   (argtypes multiple)
   (argnames multiple)
   (arghelp multiple)
   (defaultfns multiplefns)
   (mainfns multiplefns)
   (closefns multiplefns)
   (mhelp single))
  (global-list global-reviewlist)
  (shadow t)
  (mhelp-line "review command")
  (scribe-one-fn
   (lambda (item)
     (maint::scribe-doc-command
       (format nil "@IndexReviewcmd(~A)" (symbol-name item))
       (get item 'argnames)
       (cdr (assoc 'reviewcmd (get item 'mhelp))))))
  (mhelp-fn (command-mhelp reviewcmd '"<Rn>" category)))
)

(context flag-modes)

(defflag suppress-flags
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If T, will suppress the printing of any flags in SUPPRESS-FLAGS-LIST
by the HELP MODE, COMPARE-MODES, LIST, DESCRIBE*, UPDATE and CHANGED-FLAGS
commands."))

(defflag suppress-flags-list
  (flagtype tpsflaglist)
  (default nil)
  (subjects printing)
  (mhelp "If SUPPRESS-FLAGS is T, these flags will not be printed.
SUPPRESS-FLAGS-LIST itself is always suppressed, because it's very large."))

(defvar *suppressed-count* 0)

(defun suppressed-p (flag)
  (when (or (eq flag 'suppress-flags-list)
	    (and suppress-flags (member flag suppress-flags-list)))
    (incf *suppressed-count*) t))

(defun suppressed-message ()
  (when (and suppress-flags (> *suppressed-count* 1)) ;>1 so we don't count suppressed-flags-list
    (msgf *suppressed-count* " flags were suppressed." t)))

(defun describe-mode (mode category)
  (princ-mhelp mode category)
  (setq *suppressed-count* 0)
  (msgf "Flags are set as follows:")
  (msgf (t 3) "Flag" (t 28) "Value in Mode" (t 50) "Current Value")
  (dolist (flag (get mode 'flag-settings) (suppressed-message))
    (if (and (get (car flag) 'flag) (not (suppressed-p (car flag))))
        (msgf (t 3) (car flag) (t 28) (cadr flag) (t 50) (eval (car flag))))))

(defun describe-modes-gwffs (gml category)
  (princ-mhelp gml category)
  (setq *suppressed-count* 0)
  (msgf t "Modes:" t)
  (print-unordered-symbol-table (get gml 'modes-gwffs-modes))
  (msgf t "Gwffs:" t)
  (print-unordered-symbol-table (get gml 'modes-gwffs-gwffs)))

(defreview compare-modes
  (argtypes tps-mode tps-mode)
  (argnames mode1 mode2)
  (mhelp "Compare two different modes; print a list of the values on 
which they differ."))

(defun compare-modes (mode1 mode2)
  (setq *suppressed-count* 0)
  (let ((fl1 (get mode1 'flag-settings))
	(fl2 (get mode2 'flag-settings)))
  (msgf t t (t 3) "Flag" (t 28) "Value in " mode1 (t 50) "Value in " mode2 t "Defined in both, but differently : " t)
  (dolist (flag (intersection fl2 fl1 :test #'(lambda (a b) (and (equal (car a) (car b))
								 (not (equal (cadr a) (cadr b)))))))
	  (if (and (get (car flag) 'flag) (not (suppressed-p (car flag)))) 
	      (msgf (t 3) (car flag) (t 28) (cadr (assoc (car flag) fl1)) 
					   (t 50) (cadr (assoc (car flag) fl2)))))
  (msgf t "Defined in " mode1 " but not in " mode2 " : " t)
  (dolist (flag (set-difference fl1 fl2 :test #'(lambda (a b) (equal (car a) (car b)))))
	  (if (and (get (car flag) 'flag) (not (suppressed-p (car flag)))) 
	      (msgf (t 3) (car flag) (t 28) (cadr flag))))
  (msgf t "Defined in " mode2 " but not in " mode1 " : " t)
  (dolist (flag (set-difference fl2 fl1 :test #'(lambda (a b) (equal (car a) (car b)))) (suppressed-message))
	  (if (and (get (car flag) 'flag) (not (suppressed-p (car flag)))) 
	      (msgf (t 3) (car flag) (t 50) (cadr flag))))))

;;;
;;; Global parameters or flags (later simply called flags) are defined
;;; with the Defflag command which defines MHELP, SUBJECTS, DEFAULT
;;; properties of the flag.  Flags are also added to GLOBAL-FLAGLIST,
;;; which is in turn used by REVIEW to collect all flags concerning
;;; a given subjects.  Subjects are collected into GLOBAL-SUBJECTLIST.
;;;

;;;
;;; Following are argument types used for REVIEW.
;;;

(context flag-review)

(defun tpsflag-p (tpsflag)
  (if (get tpsflag 'flag) t nil))

;;; REVIEW-COMMAND-CTREE contains the command-ctree for REVIEW commands

(defvar review-command-ctree nil)

(defun review ()
  (%catch% (reviewtop)
	 (exit-inferior-top core::expand-catch-throw)))

(defun reviewtop ()
  (let ((top-prompt-fn #'review-top-prompt)
	(command-interpreter #'review-command-interpreter)
	(print-* #'review-print-*)
	(top-level 'review-top)
	(command-ctree review-command-ctree))
    (declare (special top-prompt-fn command-interpreter print-* top-level
		      command-ctree))
    (secondary-top)))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun review-top-prompt (id) (format nil "<R~A>" id))

(defun review-print-* (result) (fresh-line) (prin1 result))

(defun review-command-interpreter (cmd)
  (declare (special expertflag))
  (cond ((null cmd) nil)
	((and (null (cdr cmd)) (atom (car cmd)))
	 (cond
	  ((eq (car cmd) 'leave) (exit-review))
	  ;the line above is necessary because both review commands and grader commands
	  ;have the property MAINFN, and the latter overwrites the former in the case of LEAVE.
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
	((get (car cmd) 'reviewcmd)
	 `(comdecode (quote ,cmd)))
	((get (car cmd) 'mexpr) `(comdecode (quote ,cmd)))
	((get (car cmd) 'flag)
	 (if (cdr cmd) `(comdecode '(set ,@cmd))
	     `(comdecode '(setflag ,@cmd))))
	((null expertflag)
	 (throwfail "Unknown command."))
	(t cmd)))

;;;
;;; Following are the definitions of the REVIEW commands as currently
;;; available.
;;;

(defreview subjects
  (argtypes yesno)
  (argnames show-help)
  (arghelp "Show Help Messages?")
  (defaultfns (lambda (show-help) (list (if (eq show-help '$) nil show-help))))
  (mhelp "Print a list of currently defined subjects for REVIEW."))

(defun subjects (&optional show-help)
  (msgf)
  (dolist (subj global-subjectlist (msgf))
    (if (symbolp subj)
	(if show-help
	    (let ((mh (assoc 'review-subject (get subj 'mhelp))))
	      (if mh
		  (msg subj " : " (cdr mh) t)
		(msg subj t)))
	  (msg subj " ")))))

(defreview list
  (argnames subjectlist)
  (argtypes subjectlist)
  (mainfns list-subjects)
  (mhelp "List all flags in the given subjects with their current value."))

(defun list-subjects (subjectlist)
  (dolist (subject subjectlist)
    (when (accessible-p subject)
      (msgf "Subject: " subject t)
      (list-flags subject))))

(defun list-flags (subject)
  (setq *suppressed-count* 0)
  (let ((subject-flags 
	 (sort (copy-list
		(remove-if-not 
		 #'(lambda (flag) (and (symbolp flag)
				       (member subject 
					       (get flag 'subjects))
				       (accessible-p flag)
				       (not (suppressed-p flag))))
			       global-flaglist))
	       #'string<)))
    (dolist (flag subject-flags (suppressed-message))
      (list-flag flag))))

(defun list-flag (flag)
  (msg (t 2) flag ":" (t 25))
  (funcall (get (get flag 'flagtype) 'printfn) (eval flag))
  (msg t))

(defreview update
  (argnames subjectlist)
  (argtypes subjectlist)
  (mhelp "Update all the flags concerning the given subjects. ! will
leave the remaining flags unchanged."))

(defun update (subjectlist)
  (dolist (subject subjectlist)
    (msgf "Subject: " subject t)
    (update-flags subject)))

(defun subject-flags (subject)
  (sort (copy-list
	 (remove-if-not 
	  #'(lambda (flag) (and (symbolp flag)
				(member subject 
					(get flag 'subjects))
				(not (suppressed-p flag))))
	  global-flaglist))
	#'string<))

					; the following code about relevance and irrelevance was written August 2000 - cebrown
					; see the Programmer's Guide, Chapter on Flags
(defun relevant-flags (flag)
  (let (ret)
    (dolist (o (get flag 'outgoing-relevant) ret)
      (when (eval (car o))
	(setq ret (union (cdr o) ret))))))

(defun flag-irrelevant (flag)
  (if (member flag *never-irrelevant*)
      nil
    (reachably-irrelevant flag)))

(defun reachably-irrelevant (flag &optional checked important-flags)
  (if (member flag checked)
      nil
    (if (member flag *never-irrelevant*)
	important-flags			; this will be nil if no pred on the path was true, otherwise it is a list of flags for which the
					; setting made the pred true
      (dolist (p (get flag 'incoming-irrelevant) nil)
	(let ((z (reachably-irrelevant (car p) (cons flag checked)
				       (if (eval (cadr p))
					   (cons (car p) important-flags)
					 important-flags))))
	  (when z
	    (return z)))))))

(defun build-relevance-graph ()
  (dolist (f global-flaglist)
    (when (symbolp f)
      (dolist (p (get f 'relevancy-preconditions))
	(putprop (car p)
		 (cons (list (cadr p) f)
		       (get (car p) 'outgoing-relevant))
		 'outgoing-relevant))
      (dolist (k (get f 'relevant-kids))
	(putprop f (cons (cons (car k)
			       (eval (cadr k)))
			 (get f 'outgoing-relevant))
		 'outgoing-relevant)))))

(defun build-irrelevance-graph ()
  (dolist (f global-flaglist)
    (when (symbolp f)
      (putprop f (append 
		  (get f 'irrelevancy-preconditions)
		  (get f 'incoming-irrelevant))
	       'incoming-irrelevant)
      (dolist (k (get f 'irrelevant-kids))
	(dolist (f2 (eval (cadr k)))
	  (putprop f2 (cons (list f (car k)) 
			    (get f2 'incoming-irrelevant))
		   'incoming-irrelevant)))))
  (dolist (f global-flaglist)
    (when (symbolp f)
      (unless (get f 'incoming-irrelevant)
	(push f *never-irrelevant*)))))

(defreview update-relevant
    (argtypes tpsflag)
  (argnames flag)
  (arghelp "Flag to Update First")
  (defaultfns (lambda (flag)
		(list (if (eq flag '$) 'AUTO::DEFAULT-MS flag))))
  (mhelp "Update a flag and flags that are known to be relevant to the value given.
For example,

update-relevant DEFAULT-MS

will allow the user to first set DEFAULT-MS.  If the user sets DEFAULT-MS to MS98-1,
then TPS will ask the user to set flags relevant to MS98-1.

When update-relevant is called, the user is given the option of using the current
flag relevancy information in memory, loading flag relevancy information saved to
a file using SAVE-FLAG-RELEVANCY, or rebuilding flag relevancy information from
the Lisp source files."))

(defun update-relevant (flag)
  (choose-relevancy-data)
  (let ((updated nil))
    (declare (special updated))
    (update-relevant-rec flag)
    (when (and (cdr updated) ; if there are at least 2 flags set . . .
	       (query "Do you want to define a mode with these settings?" t))
      (let ((symb (intern (gensym "MODE"))))
	(do ((name nil))
	    ((not (or (null name) (memq name global-modelist)))
	     (eval `(defmode ,name ,(cons 'flag-settings (core::flagsort
							  (mapcar #'(lambda (m)
								      (list m (eval m)))
								  updated)))
		      (mhelp "Mode created by UPDATE-RELEVANT.")))
	     (reorganize)
	     (when (and (module-loaded-p 'library) default-lib-dir
			(query "Would you like to save this mode in the library?" t))
	       (setf (get name 'SAVE-THIS-MODE) T)
	       (insert-libobject name 'MODE)
	       (setf (get name 'SAVE-THIS-MODE) NIL)))
	  (prompt-read name nil (msgf "Name for mode? ") 'symbol symb
		       ((? (msgf "Provide a name for the new mode."))
			(?? (mhelp 'tps-mode))))
	  (when (and (memq name global-modelist)
		     (query "That mode already exists. Overwrite it?" t))
	    (dolist (prop (plist name))
	      (remprop name prop))
	    (setq global-modelist (delete name global-modelist))))))))
	


(defun update-relevant-rec (flag)
  (declare (special updated *flag-rel-hash*))
  (unless (member flag updated)
    (push flag updated)
    (unless (flag-irrelevant flag) ; this uses the irrelevance info built into TPS through defflag
      (update-flag flag)
      (dolist (f (if (hash-table-p *flag-rel-hash*)
		     (relevant-flags-from-hash flag) ; cebrown 5/18/02
		   (relevant-flags flag)))
	      (update-relevant-rec f)))))

(defflag suppress-irrelevance-warnings
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If SUPPRESS-IRRELEVANCE-WARNINGS is T, TPS does not warn when the user
sets a flag that has no effect given the current settings of other flags."))

(defun update-flags (subject)
  (setq *suppressed-count* 0)
  (let ((subject-flags (subject-flags subject)))
    (dolist (flag subject-flags (suppressed-message))
      (update-flag flag))))

(defun update-flag (flag)
  (unless *relevance-graphs-built* ; the first time we update a flag, build the relevance and irrelevance graphs - cebrown 8/30/00
    (build-relevance-graph)
    (build-irrelevance-graph)
    (setq *relevance-graphs-built* t))
  (do ((flagvalue nil)
       (ext-var nil))
      (nil)
    (let ((irr-info (flag-irrelevant flag))) ; cebrown - 8/30/00
      (when (and irr-info (not suppress-irrelevance-warnings))
	(if (cdr irr-info)
	    (complain "WARNING: The settings of the flags " irr-info " make the value of the flag " flag " irrelevant.")
	  (complain "WARNING: The setting of the flag " (car irr-info) " makes the value of the flag " flag " irrelevant."))))
;    (%catch% 
    (progn
      (prompt-read flagvalue ext-var
		   (msgf flag) (get flag 'flagtype) (eval flag)
		   ((? (mhelp flag))
		    (?? (mhelp (get flag 'flagtype)))))
      (when (eq ext-var '!)
	(return t))
      (let ((current-value (eval flag)))
	(when (get flag 'pre-change-fn) ; cebrown - 10/29/00 (see EXPERTFLAG for an example with a PRE-CHANGE-FN)
	  (funcall (get flag 'pre-change-fn) flag flagvalue current-value)) ; cebrown - 10/29/00
	(set flag flagvalue)
	(if (get flag 'change-fn)
	    (funcall (get flag 'change-fn) flag flagvalue current-value))
	(if (get flag 'synonymlist)
	    (update-all-in-list (get flag 'synonymlist) (eval flag)))
	(unless (or (eq ext-var '!)
		    (eq current-value flagvalue)
		    (eq flag 'last-mode-name))
	  (setq last-mode-name (concatenate 'string last-mode-name
					    (if *mode-altered* ", and " ", but with ")
					    (string-upcase (princ-to-string flag))
					    " set to " (princ-to-string flagvalue)))
	  (setq *mode-altered* t)))
      (return nil))))
;             (fail (complain f "Could not set flag.  "
;                             core::expand-catch-throw)))))


(defutil update-flag
  (form-type function)
  (keywords input initialization)
  (mhelp "(UPDATE-FLAG flag)
is used to give the user a chance to change a flag or parameter.
The user will be prompted for a new value of flag, the default being
its current value.  This is useful in initialization dialogues.  For example:
(update-flag 'style)
will prompt the user for a style.  It the user simply types return, it will
be unchanged."))

(defreview setflag
  (argnames flag)
  (argtypes tpsflag)
  (mainfns update-flag)
  (mhelp "Set the value of a flag after examining it."))

(defreview set
  (argnames flag flag-value)
  (argtypes tpsflag anything)
  (mainfns set-flag)
  (mhelp "Directly set the value of a flag."))
	
(defreview setflags1
  (argtypes fv-list)
  (argnames fvlist)
  (arghelp "List of flag names & values.")
  (mhelp "Simultaneously sets multiple flags of the form ((FLAG1 . VALUE1)
(FLAG2 . VALUE2)...) (the dots may be omitted); intended for use
when cutting and pasting records from library or bug files. The opening
and closing parentheses must be supplied."))

(defun setflags1 (fvlist)
  (dolist (fv fvlist)
	  (set-flag (car fv) (cdr fv))))

(defreview setflags2
  (argtypes string)
  (argnames whole)
  (mhelp "Simultaneously sets multiple flags of the form \"FLAG1: VALUE1
FLAG2: VALUE2 ...\". Intended for use when cutting and pasting records 
from library or bug files. User must provide double quotes before and 
after pasting the record, and each flag and value pair should be 
separated by a newline. Flag-names containing double quotes must be 
set separately. This command cannot handle such cases.")
  (arghelp "List of flag names & values.")
  (mainfns com2))

(defun com2 (whole)
  (let ((line "")(fn "")
	rest fandv fv)
  (if (string= whole "") nil 
    (progn 
      (do ((i 0 (+ i 1)))
	  ((or (< (length whole) (1+ i)) (equal (char whole i) #\Newline)))
	  (setq line (concatenate 'string line (princ-to-string (char whole i)))))
      (setq fandv (string-left-trim '(#\Space) line))
      (do ((j 0 (+ j 1)))
	  ((string= (princ-to-string (char fandv j)) ":"))      			(setq fn (concatenate 'string fn (princ-to-string (char fandv j)))))
      (setq rest  (string-right-trim '(#\Space) (string-left-trim fn fandv)))
      (setq fv (string-left-trim  '(#\: #\Space) rest))
      (let ((A (find-symbol fn (find-package 'user)))
	    (B (find-symbol fv (find-package 'user))))
	(when A (set-flag A B)))
      (setq whole (string-left-trim line whole))
      (setq whole (string-left-trim '(#\Newline) whole))
      (com2 whole)))))

;;;
;;; KEY is a new REVIEW command which searches for a given string
;;; in all help strings for flags of the given subjects.
;;;

(defreview key
  (argnames phrase subjectlist search-names)
  (argtypes string subjectlist boolean)
  (arghelp "String to search for" "List of subjects, or ALL" "Search names of flags only?")
  (defaultfns (lambda (phrase subjectlist search-names)
		(list phrase (if (eq subjectlist '$) 
				 (remove-if-not #'symbolp global-subjectlist) subjectlist)
		      (if (eq search-names '$) nil search-names))))
  (mainfns review-keys)
  (mhelp "Look for a key phrase in the help strings (or just the names) 
of flags of given subjects. See also SEARCH, at the main top level."))

(defun review-keys (phrase subjectlist names)
    (dolist (flag global-flaglist)
	    (if (not (listp flag))
	  (let ((flagtext (if (listp (cdar (get flag 'mhelp))) 
			      (string-downcase (tacktogether (mapcar #'(lambda (x) (string (eval x))) (cdar (get flag 'mhelp)))))
			    (string-downcase (cdar (get flag 'mhelp)))))
		(searchtext (string-downcase phrase))
		(flagname (string-downcase (princ-to-string flag)))
		(slist (get flag 'subjects))
		(offset (length phrase))
		(found nil))
	    (if names (setq flagtext flagname) 
	      (setq flagtext (concatenate 'string flagname " " flagtext)))
	    (if (not (null (intersection slist subjectlist)))
		(do ((counter 0 (1+ counter)))
		    ((or found (>= (+ counter offset) (1+ (length flagtext)))))
		    (setq found (string= flagtext searchtext :start1 counter :end1 (+ offset counter)))
		    (if found (msg flag t))))))))

(defun tacktogether (list)
  (if (null list) ""
  (concatenate 'string (car list) (tacktogether (cdr list)))))

(defreview describe*
  (argnames subjectlist)
  (argtypes subjectlist)
  (mainfns list-subjects*)
  (mhelp "List all flags under the subjects requested, along with their descriptions."))

(defun list-subjects* (subjectlist)
  (dolist (subject subjectlist)
    (when (accessible-p subject)
      (msgf "Subject: " subject t)
      (list-flags* subject))))

(defun list-flags* (subject)
  (setq *suppressed-count* 0)
  (let ((subject-flags 
	 (sort (copy-list
		(remove-if-not 
		 #'(lambda (flag) (and (symbolp flag)
				       (member subject 
					       (get flag 'subjects))
				       (accessible-p flag)
				       (not (suppressed-p flag))))
			       global-flaglist))
	       #'string<)))
    (dolist (flag subject-flags (suppressed-message))
	    (msgf (string-upcase (princ-to-string flag)) ":" t)
	    (describe-flag flag 'flag)
	    (msgf "-----" t))))

(defreview describe
  (argnames flag)
  (argtypes tpsflag)
  (mainfns (lambda (flag) (describe-flag flag 'flag)))
  (mhelp "Describe a flag."))

(defun describe-flag (flag category)
  (princ-mhelp flag category)
  (msgf flag " takes values of type " (get flag 'flagtype) ".")
  (msgf "It belongs to subjects " (l (get flag 'subjects)) ".")
  (msgf "Its default value is ") (funcall (get (get flag 'flagtype) 'printfn) (get flag 'default))
  (msgf "Its current value is ")
  (funcall (get (get flag 'flagtype) 'printfn) (eval flag))
  (msg "."))

(defreview changed-flags
  (argnames omit)
  (argtypes subjectlist)
  (arghelp "Subjects to OMIT from the listing")
  (defaultfns (lambda (s) (list (if (eq s '$) '(EVENTS GR-FILENAMES MAINTAIN SYSTEM) s))))
  (mhelp "List all those flags whose current value is not
the default value."))

(defun changed-flags (subjects)
  (setq *suppressed-count* 0)
  (let ((flaglist nil))
    (dolist (flag global-flaglist)
	    (when (and (symbolp flag)
		       (not (intersection subjects (get flag 'subjects)))
		       (not (equal (get flag 'default) (eval flag)))
		       (not (suppressed-p flag)))
		  (push flag flaglist)))
    (when flaglist (msgf "FLAG" (t 30) "DEFAULT" (t 50) "CURRENT" t))
    (dolist (flag (sort flaglist #'string< :key #'princ-to-string) (suppressed-message))
	    (msgf flag (t 30))
	    (funcall (get (get flag 'flagtype) 'printfn) (get flag 'default))
	    (msg (t 50))
	    (funcall (get (get flag 'flagtype) 'printfn) (eval flag)))))

(context flag-modes)

(defreview mode
  (argtypes tps-mode)
  (argnames mode)
  (mhelp "Set a group of flags by switching to a mode."))

(defreview copy-mode
  (argtypes tps-mode symbol)
  (argnames oldname newname)
  (arghelp "Existing mode" "Name for new mode")
  (mhelp "Make a copy of a mode, with a new name. To delete the
old mode from memory, use DESTROY."))

(defun copy-mode (name newname)
  (unless (tps-mode-p name)
	  (throwfail "There is no mode called " name " in memory." t))
  (let ((newobject nil)
	(newprops nil))
    (do ((pl (symbol-plist name) (cddr pl)))
	((null pl))
	(if (eq (car pl) 'flag-settings)
	    (push (cons (car pl) (cadr pl)) newobject) 
	  (if (eq (car pl) 'mhelp)
	      (push (cons (car pl) (list (cdr (assoc 'flag-mode (cadr pl))))) newobject)
	    (push (cons (car pl) (cadr pl)) newprops))))
    (eval (nconc (list 'defmode newname)
		 (nreverse newobject)))
    (dolist (prop newprops)
	    (setf (get newname (car prop)) (cdr prop)))
    (reorganize)))

(defreview add-flag-to-mode
  (argtypes tps-mode tpsflag)
  (argnames mode flag)
  (arghelp "Mode" "New flag")
  (mhelp "Add a flag to a mode. The flag will be added with its
current setting. If the flag is already present, its value in the
mode will be changed to its current setting."))

(defun add-flag-to-mode (mode flag)
  (let ((exists (assoc flag (get mode 'flag-settings))))
    (if exists 
	(progn 
	  (msgf "Changing the setting of the flag " flag " in mode " mode " from " (cadr exists) " to " (eval flag) t)
	  (setf (cdr exists) (list (eval flag))))
      (progn 
	(msgf "Adding " flag "  " (eval flag) " to the mode " mode t)
	(setf (get mode 'flag-settings) (flagsort (cons (list flag (eval flag)) (get mode 'flag-settings))))))))

(defreview remove-flag-from-mode
  (argtypes tps-mode tpsflag)
  (argnames mode flag)
  (arghelp "Mode" "Flag to remove")
  (mhelp "Delete a flag from a mode. If the flag is not present
in the mode, this command will do nothing."))

(defun remove-flag-from-mode (mode flag)
  (let ((exists (assoc flag (get mode 'flag-settings))))
    (if exists 
	(progn 
	  (msgf "Removing the flag " flag " from mode " mode t)
	  (setf (get mode 'flag-settings)
		(delete flag (get mode 'flag-settings) :key 'car)))
      (msgf flag " is not in  " mode "!" t))))

(context subtoplevels)

(defreview leave
  (mainfns exit-review)
  (mhelp "Leave REVIEW to the next enclosing top level."))

(defun exit-review () (%throw% '|[Left REVIEW.]| exit-inferior-top))

