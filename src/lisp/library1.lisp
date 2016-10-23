;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
;;;
;;; File LIBRARY
;;;
;;; defines LIBRARY, FLAGS etc.
;;;

(deffile library1
  (part-of library)
  (extension lisp)
  (mhelp "Defines top-level for library."))

(context subtoplevels)

(deftoplevel library-top
  (top-prompt-fn library-top-prompt)
  (command-interpreter library-command-interpreter)
  (print-* library-print-*)
  (top-level-category librarycmd)
  (top-level-ctree library-command-ctree)
  (top-cmd-decode lib-opdecode)
  (mhelp "The top level of LIBRARY."))

(eval-when (load compile eval)
(defcategory librarycmd
  (define deflibrary)
  (properties
    (lib-argtypes multiple)
    (lib-argnames multiple)
    (lib-arghelp multiple)
    (lib-defaultfns multiplefns)
    (lib-mainfns singlefn)
    (mhelp single))
  (global-list global-librarylist)
  (shadow t)
  (mhelp-line "library command")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command
	(format nil "@IndexOther(~A)" (symbol-name item))
	(get item 'lib-argnames)
	(cdr (assoc 'librarycmd (get item 'mhelp))))))
  (mhelp-fn (librarycmd-mhelp librarycmd category))))

(defvar *lib-masterindex* (make-hash-table))

(defun librarycmd-mhelp (keyword category)
  (princ-mhelp keyword category)
  (unless short-help 
    (when *doing-html* (msg " fnord "))
    (msgf "The command format for " keyword " is:" -2 "<LIB>" keyword)
    (print-tps-format* keyword " "
		       (+ 5 (length (format nil "~A" keyword)))
		       (get keyword 'lib-argnames)
		       (get keyword 'lib-argtypes)
		       nil)))

(defun library ()
  (%catch% (librarytop)
	   (exit-inferior-top core::expand-catch-throw)))

(defun librarytop () 
  (let ((top-prompt-fn #'library-top-prompt)
	(command-interpreter #'library-command-interpreter)
	(print-* #'library-print-*)
	(top-level 'library-top))
    (declare (special top-prompt-fn command-interpreter print-* top-level
		      command-ctree lib-object))
    (secondary-top)))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun library-top-prompt (id) (format nil "<lib~A>" id))

(defun library-print-* (result) (fresh-line) (prin1 result))

(defun library-command-interpreter (cmd)
  (let ((carcmd (car cmd)))
    (setq *retrieve-stack* nil) ; just in case!
    (cond ((null cmd) nil)
	  ((and (null (cdr cmd)) (atom carcmd))
	   (cond 
	         ((integerp carcmd)
		  (throwfail "Unknown command."))
		 ((and (symbolp carcmd) (get carcmd 'librarycmd))
		  `(lib-opdecode (quote ,cmd)))
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
	  ((and (symbolp carcmd) (get carcmd 'librarycmd))
	   `(lib-opdecode (quote ,cmd))) 
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

(defun lib-opdecode (command)
  (let ((keyword (car command))
	mainfn result)
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values keyword
		       (copy (cdr command))
		       (get keyword 'lib-argtypes)
		       (mapcar #'(lambda (x) (declare (ignore x)) nil)
			       (get keyword 'lib-argtypes))
		       nil
		       (get keyword 'lib-defaultfns)
		       nil
		       (get keyword 'lib-argnames)
		       (get keyword 'lib-arghelp))
      (declare (ignore external-arglist))
      (setq mainfn (or (get keyword 'lib-mainfns) keyword))
      (%catch% (setq result (apply mainfn internal-arglist))
	       (fail (complain f "Error from " mainfn ".  " 
                               core::expand-catch-throw)
		     (throwfail "Operation aborted.")))
      result)))

(defmexpr lib
  (mainfns library)
  (mhelp "Enter the library top-level.

See Also: UNIXLIB (an alternative library top level)"))

(context subtoplevels)

(deflibrary leave
  (lib-mainfns exit-library)
  (mhelp "Leave LIBRARY to the next enclosing top level."))

(defun exit-library () (%throw% '|[Left LIBRARY.]| exit-inferior-top))

(context lib-display)

(deflibrary show
  (lib-argnames name type)
  (lib-argtypes symbol lib-argtype-or-nil)
  (lib-arghelp "Name of object" "argtype or nil")
  (lib-defaultfns (lambda (name type)
		    (list name (if (eq type '$) nil type))))
  (lib-mainfns show-libobject)
  (mhelp "Display a library object.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(defun show-libobject (name type)
  (let ((item (retrieve-item name :type type :multiple show-all-libobjects)))
    (when item 
	  (write-libitem item)
	  (msg t "Stored in file " (pfile (namestring (libitem-file item)))))))

(deflibrary key
  (lib-argnames string backup)
  (lib-argtypes string yesno)
  (lib-arghelp "String to search for" "Search backup directories?")
  (lib-defaultfns (lambda (string backup) (list (if (eq string '$) "" string) (if (eq backup '$) t backup))))
  (lib-mainfns lib-key)
  (mhelp "Search for a string in the names of all library objects. If
the given string is also a keyword (see SHOW-KEYWORDS), then the keywords
for each library object will also be searched. This command does not search
the help messages of library objects."))

(defun lib-key (string backup)
  (let ((objlist nil)
	(keyword (or (car (keyword-p (find-symbol (string-upcase string))))
		     (car (keyword-p (find-symbol string)))
		     (car (keyword-p (find-symbol (string-downcase string)))))))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val) (setq objlist (cons (list key elt) objlist)))))
	     *lib-masterindex*)
    (unless backup (let ((dirs (mapcar 'directory-namestring backup-lib-dir)))
		     (setq objlist (remove-if #'(lambda (x) 
						  (member (directory-namestring (cdadr x)) dirs :test 'string=))
					      objlist))))
    (setq objlist (remove-duplicates (sort objlist #'string-lessp :key #'car)))
    (dolist (object objlist)
	    (let ((objtext (string-downcase (princ-to-string (car object))))
		  (searchtext (string-downcase string))
		  (offset (length string))
		  (found nil))
	      (or (and keyword
		       (is-a-keyword keyword
				     (retrieve-item (car object) :type (caadr object) :multiple nil :writeable t
						    :preferred-dir (directory-namestring (cdadr object)) :fail-gently t))
		       (not (msg (car object) 2 (caadr object) 2 (pfile (namestring (cdadr object))) t)))
		  ;;msg always returns NIL
		  (do ((counter 0 (1+ counter)))
		      ((or found (>= (+ counter offset) (1+ (length objtext)))))
		    (setq found (string= objtext searchtext :start1 counter :end1 (+ offset counter)))
		    (if found (msg (car object) 2 (caadr object) 2 (pfile (namestring (cdadr object))) t))))))))

(defun is-a-keyword (keyword libitem)
  (and libitem (member keyword (libitem-keywords libitem))))

(deflibrary search
  (lib-argnames type stringlist backup)
  (lib-argtypes lib-argtype-or-nil stringlist yesno)
  (lib-arghelp "Argtype or NIL"
	       "List of strings to search for" "Search backup dirs as well?")
  (lib-defaultfns (lambda (type stringlist backup) 
		    (list (if (eq type '$) nil type)
			  (if (eq stringlist '$) '("") stringlist) 
			  (if (eq backup '$) 'yes backup))))
  (lib-mainfns lib-search)
  (mhelp "Search the entire library, including all comments, for any one
of a given list of strings, and return the names of all objects which
contain such a string. This is useful for finding out, for example,
which gwffs can be proven using either MS88 or MS89.
WARNING: THIS COMMAND IS SLOW, AND CAN USE A LOT OF MEMORY.
You might want to think about using the Unix \"grep\" command instead."))

(deflibrary search2
  (lib-argnames type stringlist backup)
  (lib-argtypes lib-argtype-or-nil stringlistlist yesno)
  (lib-arghelp "Argtype or NIL"
	       "List of strings to search for" "Search backup dirs as well?")
  (lib-defaultfns (lambda (type stringlist backup) 
		    (list (if (eq type '$) nil type)
			  (if (eq stringlist '$) '(("")) stringlist) 
			  (if (eq backup '$) 'yes backup))))
  (lib-mainfns lib-search-2)
  (mhelp "Search the entire library, including all comments, for a 
given combination of strings. See also SEARCH.
The syntax for the given list is essentially conjunctive normal
form -- it should be a list of conjuncts, each of which is a list of 
disjuncts.
For example:
((MS88) (THM)) finds everything containing THM and MS88
((MS88 THM)) finds everything containing THM or MS88
((MS88 MS89) (THM EXERCISE)) finds everything containing (MS88 or MS89)
                             and (THM or EXERCISE).
WARNING: THIS COMMAND IS SLOW, AND CAN USE A LOT OF MEMORY.
You might want to think about using the Unix \"grep\" command instead."))

(defun lib-search-2 (a b c)
  (lib-search a nil c b))

(defun lib-search (type stringlist backup &optional (and-list nil))
  (let ((filelist nil)
	(found-list nil)
	(old-foundlist nil)
	(file-foundlist nil)
	(stringl nil)
	(first-time t))
    (msg t t "Examining files... please be patient." t)
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val) (setq filelist (cons (namestring (cdr elt)) filelist)))))
	     *lib-masterindex*)
    (setq filelist (remove-duplicates filelist :test #'string=))
    (unless backup (let ((dirs (mapcar 'directory-namestring backup-lib-dir)))
		     (setq filelist (remove-if #'(lambda (x) 
						   (member (directory-namestring x) dirs :test 'string=))
					       filelist))))
    (if (and (null stringlist) (not (null and-list)))
	(setq stringl and-list)
      (setq stringl (list stringlist)))
    (dolist (string stringl)
       (dolist (filename filelist)
	  (when (probe-file filename)
		(let ((copy-file (new-tmp-filename)))
		  (copy-file filename copy-file)
		  (with-open-file (*input-stream* copy-file :direction :input)
		     (do ((item (read *input-stream* nil control-d-char)
				(read *input-stream* nil control-d-char)))
			 ((eq item control-d-char))
			 (let ((object (cadr (memq :name item)))
			       (tp (cadr (memq :type item)))
			       (file filename)
			       (objtext (string-downcase (princ-to-string item))))
			   (do* ((strings string (cdr strings))
				 (searchtext (string-downcase (car strings)) (string-downcase (car strings)))
				 (offset (length searchtext) (length searchtext))
				 (found nil))
				((or (and type (neq tp type)) found (null strings)))
				(do ((counter 0 (1+ counter)))
				    ((or found (>= (+ counter offset) (1+ (length objtext)))))
				    (setq found (string= objtext searchtext :start1 counter :end1 (+ offset counter)))
				    (when found 
					  (push filename file-foundlist) 
					  (push (cons (list object (cons tp file)) searchtext) found-list)))))))
		  (delete-file copy-file))))
       (if first-time 
	   (setq found-list 
		 (remove-duplicates found-list
				    :key #'caar :test #'(lambda (x y) 
							  (string= (princ-to-string x) (princ-to-string y))))
		 first-time nil)
	 (setq found-list 
	       (remove-duplicates (intersection found-list old-foundlist :key #'caar) 
				  :key #'caar :test #'(lambda (x y) (string= (princ-to-string x) (princ-to-string y))))))
	    (setq filelist file-foundlist) ;no need to do it *all* again
	    (setq old-foundlist found-list)
	    (setq found-list nil))
    (msgf t t "The following items contain strings satisfying: ")
    (readable-boolean stringl) (msg t)
    (dolist (object old-foundlist)
	    (msg (caar object) 2 (caadar object) 2 (pfile (namestring (cdadar object))))
	    (if (null and-list) 
		(msg " : found " (cdr object) "." t)
	      (msg t)))))

(defun readable-boolean (stringlist)
  (let ((conjunct (car stringlist))
	(rest (cdr stringlist)))
    (if (null conjunct) nil 
      (progn (msg "(")
	     (do ((elt (car conjunct) (car conjunct))
		  (conjunct (cdr conjunct) (cdr conjunct)))
		 ((null conjunct) (msg elt ")") (when rest (msg " AND ") (readable-boolean rest)))
		 (msg elt " OR " ))))))

(deflibrary show-wff
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Name of object")
  (lib-defaultfns (lambda (name) (list name)))
  (lib-mainfns show-libobject-wff)
  (mhelp "Display the wff of a gwff in the library.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(defun show-libobject-wff (name)
  (let ((item (or (retrieve-item name :type 'gwff :multiple show-all-libobjects)
		  (retrieve-item name :type 'abbr :multiple show-all-libobjects))))
    (when item 
	  (write-description-only item)
	  (msg t "Stored in file " (pfile (namestring (libitem-file item)))))))

(deflibrary show-timing
  (lib-argnames name screen)
  (lib-argtypes symbol yesno)
  (lib-arghelp "Name of object" "Output to File?")
  (lib-defaultfns (lambda (name file)
		    (list name (if (eq file '$) nil file))))
  (lib-mainfns show-libobject-time)
  (mhelp "Display the timing information of a gwff in the library.
NOTE: Will only display timing information that has been recorded 
in standard DATEREC format.
If you opt for output to go to a file as well as to the screen, 
the format of the file will be SCRIBE or TEX if this is the current
value of the STYLE flag, and GENERIC otherwise."))

(defun show-libobject-time (name file)
  (let ((fname nil)
	(silent nil)
	(itemlist (locate-item name :type 'gwff :multiple t))
	(count 0)
	(number nil))
  (when file
	(prompt-read fname nil
		     (msgf "FNAME (FILESPEC): Name for output file ")
		     'filespec (concatenate 'string "timing-" (string-downcase (princ-to-string name)) (case style (scribe ".mss") (tex ".tex") (t ".rec"))) ((? (mhelp 'filespec)) (?? (mhelp 'filespec))))
	(prompt-read silent nil
		     (msgf "SILENT (YESNO): Output without printing to screen? ")
		     'yesno 'no  ((? (mhelp 'boolean)) (?? (mhelp 'boolean)))))
  (when (cdr itemlist)
        (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-msg))
	(complain (length itemlist) " items called " name " found. They are as follows:" t)
        (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	(dolist (elt itemlist)
		(incf count) 
		(complain count ") " (car elt) " in file " (pfile (namestring (cdr elt)))))
	(incf count) 
	(complain count ") ALL OF THESE.")
	(incf count) 
	(complain count ") None of these.")
	(setq number (get-a-number count))
	(if (<= number (length itemlist))
	    (setq itemlist (list (nth (1- number) itemlist)))
	  (unless (= number (1+ (length itemlist)))
		  (setq itemlist nil))))
  (when itemlist
	(unless silent (main-show-time-fn name itemlist))
	(if fname (progn
		    (case style
			  (scribe (reroute-output-append fname *default-pathname-defaults* (msg scribe-preamble)
							 (msg "@Begin{Verbatim}")
							 (main-show-time-fn name itemlist) (msg scribe-postamble)))
			  ((tex tex-1)
			   (reroute-output-append fname *default-pathname-defaults* 
						  (if latex-emulation (msgf latex-preamble t "{\\obeylines\\tt " t)
						    (progn (princ "\\input ") (princ tpstex) (terpri)
							   (msg tex-preamble) (princ "{\\obeylines\\tt ")))
						  (main-show-time-fn name itemlist t)
						  (msg "}") ;closes obeylines
						  (if latex-emulation (msgf latex-postamble)
						    (msg tex-postamble))))
			  (t (let ((style 'generic))
			       (reroute-output-append fname *default-pathname-defaults* 
						      (main-show-time-fn name itemlist))))))))))

(defun main-show-time-fn (name itemlist &optional tex)
  (dolist (i itemlist)
	  (let ((item (retrieve-item name :type 'gwff :multiple nil :preferred-dir (cdr i))))
	    (write-libitem-time item style *standard-output* tex)
	    (msg t "Stored in file " (pfile (namestring (libitem-file item))) t t))))

(deflibrary show-help
  (lib-argnames name type)
  (lib-argtypes symbol lib-argtype-or-nil)
  (lib-arghelp "Name of object" "argtype or nil")
  (lib-defaultfns (lambda (name type)
		    (list name (if (eq type '$) nil type))))
  (lib-mainfns show-libobject-help)
  (mhelp "Display the help message associated with a library object.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(defun show-libobject-help (name type)
  (let ((item (retrieve-item name :type type)))
    (when item 
	  (write-mhelp-only item)
	  (msg t "Stored in file " (pfile (namestring (libitem-file item)))))))

(deflibrary show-wff&help
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Name of object")
  (lib-defaultfns (lambda (name)
		    (list name)))
  (lib-mainfns show-libobject-wff-and-help)
  (mhelp "Display the wff of a gwff in the library, with the associated
help message, keywords and provability status."))

(deflibrary show*-wff
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Name of object")
  (lib-defaultfns (lambda (name)
		    (list name)))
  (lib-mainfns show-libobject-wff-and-help-rec)
  (mhelp "Display the wff of a gwff in the library, with the associated
help message, keywords and provability status.
Also shows any needed objects, such as the definition and
help for abbrevations used in the gwff."))

(defun show-libobject-wff-and-help (name)
  (let ((item (or (retrieve-item name :type 'gwff :fail-gently t :multiple show-all-libobjects)
		  (retrieve-item name :type 'abbr :fail-gently t :multiple show-all-libobjects))))
    (when item 
	  (write-description-only item) 
	  (write-mhelp-only item)
	  (msg t "Stored in file " (pfile (namestring (libitem-file item)))))))

(defun show-libobject-wff-and-help-rec (name)
  (let ((shown-already nil))
    (declare (special shown-already))
    (show-libobject-wff-and-help-rec-1 name)))

(defun show-libobject-wff-and-help-rec-1 (name)
  (declare (special shown-already))
  (unless (member name shown-already)
    (let ((item (or (retrieve-item name :type 'gwff :fail-gently t :multiple (if shown-already nil show-all-libobjects))
		    (retrieve-item name :type 'abbr :fail-gently t :multiple (if shown-already nil show-all-libobjects)))))
      (when item 
	(msgf t name " (" (libitem-type item) "):" t)
	(funcall (get-lib-printfn (libitem-type item))
		 (libitem-description item) style)
	(unless (equal (libitem-mhelp item) "")
	  (msg t)
	  (prin1 (libitem-mhelp item)))
	(push name shown-already)
	(dolist (n (libitem-needed-objects item))
	  (show-libobject-wff-and-help-rec-1 n))))))

(deflibrary show-all-wffs
  (lib-argnames backup filter)
  (lib-argtypes yesno keyword-list)
  (lib-arghelp "Show wffs in backup directories?" "Properties? (RETURN to show all wffs)")
  (lib-defaultfns (lambda (backup filter) (list (if (eq backup '$) nil backup) 
						(if (eq filter '$) '() filter))))
  (lib-mainfns show-all-wffs-anywhere)
  (mhelp "Show all wffs in all files in DEFAULT-LIB-DIR (and optionally BACKUP-LIB-DIR).
As a filter, you can select any known keywords; only the wffs which 
satisfy all of the given keywords will be shown. See SHOW-KEYWORDS 
for a list of keywords."))

(deflibrary scribe-all-wffs
  (lib-argnames backup filter fname verbosity)
  (lib-argtypes yesno keyword-list filespec symbol)
  (lib-arghelp "Show wffs in backup directories?" "Properties? (RETURN to show all wffs)"
	       "MSS file to write" "Verbosity (MIN/MED/MAX)")
  (lib-defaultfns (lambda (backup filter fname verbosity) 
		    (list (if (eq backup '$) nil backup)
			  (if (eq filter '$) '() filter)
			  fname (if (eq verbosity '$) 'MIN verbosity))))
  (lib-mainfns scribe-all-wffs-anywhere)
  (mhelp "Write all wffs in all files in DEFAULT-LIB-DIR (and optionally BACKUP-LIB-DIR)
to an mss file.
The three verbosity settings are: MIN, which just shows the names of the 
objects, MED, which shows the help messages, keywords, provability and wffs 
as well, and MAX, which shows everything. 
As a filter, you can select any known keywords; only the wffs which 
satisfy all of the given keywords will be shown. See SHOW-KEYWORDS 
for a list of keywords."))

(deflibrary tex-all-wffs
  (lib-argnames backup filter fname verbosity)
  (lib-argtypes yesno keyword-list filespec symbol)
  (lib-arghelp "Show wffs in backup directories?" "Properties? (RETURN to show all wffs)"
	       "TeX file to write" "Verbosity (MIN/MED/MAX)")
  (lib-defaultfns (lambda (backup filter fname verbosity) 
		    (list (if (eq backup '$) nil backup) 
			  (if (eq filter '$) '() filter)
			  fname (if (eq verbosity '$) 'MIN verbosity))))
  (lib-mainfns tex-all-wffs-anywhere)
  (mhelp "Write all wffs in all files in DEFAULT-LIB-DIR (and optionally BACKUP-LIB-DIR)
to a TeX file. 
The three verbosity settings are: MIN, which just shows the names of the 
objects, MED, which shows the help messages, provability and wffs as well, and MAX, 
which shows everything.
As a filter, you can select any known keywords; only the wffs which 
satisfy all of the given keywords will be shown. See SHOW-KEYWORDS 
for a list of keywords."))

(defun scribe-all-wffs-anywhere (backup filter fname verbosity)
  (write-all-wff-anywhere backup filter fname verbosity t))

(defun tex-all-wffs-anywhere (backup filter fname verbosity)
  (write-all-wff-anywhere backup filter fname verbosity nil))

(defun write-all-wff-anywhere (backup filter fname verbosity scribe)
  (let ((style (if scribe 'scribe 'tex-1))
	(RIGHTMARGIN PAGEWIDTH))
  (reroute-output-append fname
     (namestring (make-pathname% :name fname :type (if scribe "mss" "tex")))
     (if scribe 
	 (if use-internal-print-mode
	     (in-mode scribe-otl
		      (msgf scribe-preamble)
		      (format T "~%@PageHeading(Immediate, Left <~A>~
				     ,Center <@value(Page)>, Right <~A>)~
				     ~%@PageFooting(Immediate, Right <~A>)~
				     ~%@Begin(Verbatim, LineWidth ~D)~%"
			      filter (status-userid) (stringdt nil) scribe-line-width))
	   (progn (msgf scribe-preamble)
		  (format T "~%@PageHeading(Immediate, Left <~A>~
				     ,Center <@value(Page)>, Right <~A>)~
				     ~%@PageFooting(Immediate, Right <~A>)~
				     ~%@Begin(Verbatim, LineWidth ~D)~%"
			  filter (status-userid) (stringdt nil) scribe-line-width)))
     (if latex-emulation (msgf latex-preamble t "\\markhack{{\\bf " filter "}\\hfill " (stringdt nil)
			       "\\hfill{\\bf " (status-userid) "}}" t "{\\obeylines\\tt " t)
     (if use-internal-print-mode
	 (in-mode tex-1-otl
		  (progn (princ "\\input ") (princ tpstex) (terpri)
			 (princ "\\headline={\\noindent{\\bf ") 
			 (princ filter)
			 (princ "}\\hfil\\folio\\hfil{\\bf ")      
			 (princ (status-userid))
			 (princ "}}") (princ "\\vskip36pt ") 
			 (princ "\\footline={\\hfil ") (princ (stringdt nil)) (princ "}")    
			 (msgf tex-preamble)
			 (princ "{\\obeylines\\tt ")))
       (progn (princ "\\input ") (princ tpstex) (terpri)
	      (princ "\\headline={\\noindent{\\bf ") 
	      (princ filter)
	      (princ "}\\hfil\\folio\\hfil{\\bf ")      
	      (princ (status-userid))
	      (princ "}}") (princ "\\vskip36pt ") 
	      (princ "\\footline={\\hfil ") (princ (stringdt nil)) (princ "}")    
	      (msgf tex-preamble)
	      (princ "{\\obeylines\\tt ")))))
;;do the real work here
     (let ((libwfflist nil)
	   (dirs (mapcar 'directory-namestring backup-lib-dir)))
       (maphash #'(lambda (key val)
		    (when key (dolist (elt val)
				      (when (eq 'gwff (car elt))
					    (if backup
						(setq libwfflist (cons key libwfflist))
					      (when (not (member (directory-namestring (cdr elt)) dirs :test 'string=))
						    (setq libwfflist (cons key libwfflist))))))))
		*lib-masterindex*)
       (setq libwfflist (sort (remove-duplicates libwfflist :test 'string=) #'string-lessp))
       (dolist (elt libwfflist)
	       (let ((item (retrieve-item elt :type 'gwff :multiple nil :fail-gently t)))
		 (when (and item (keyword-filter item filter))
		   (cond ((eq verbosity 'MAX) 
			  (if scribe
			      (write-libitem item)
			    (write-libitem item style *standard-output* t))
			  (if scribe (msg t t) (msg t t "\\vskip6pt \\hrule\\vskip6pt " t t)))
			 ((eq verbosity 'MIN)
			  (princ (libitem-name item))
			  (terpri))
			 (t 
			  (msgf (libitem-name item) t)
			  (write-brief-d-and-m item style *standard-output* (not scribe))
			  (if scribe (msg t t) (msg t t "\\vskip6pt \\hrule\\vskip6pt " t t)))))))
       (msg t t))
     (if scribe (msgf scribe-postamble) 
       (progn (msg "}") ;closes obeylines
	      (if latex-emulation (msgf "\\vfill" t latex-postamble t)
		(msgf "\\vfill" t tex-postamble)))))))

(defun keyword-filter (item filter)
  (if (libitem-keywords item)		; if there are keywords associated with the item, use these to test
      (dolist (f filter t)
	(unless (member f (libitem-keywords item))
	  (return nil)))
					; if there are no keywords associated with the item, test keywords with a test fn, if such a test fn exists
    (dolist (f filter t)
      (unless (funcall (get-real-filter f) item)
	(return nil)))))

(defun first-order-check (truegwff)
  (setq auto::current-topnode
	(auto::gwff-to-etree truegwff t t))
  (setq auto::active-mating 
	(if (auto::eproof-p truegwff) (car (auto::eproof-mating-list truegwff)) nil))
  (setq auto::prev-node auto::current-topnode)
  (auto::initialize-mating-search)
  (auto::update-statuses (auto::eproof-etree auto::current-eproof))
  (auto::first-order-problem-p
   (mapcar #'(lambda (x) (cons (auto::exp-var-var (car x)) (cdr x)))
	   (auto::eproof-free-vars-in-etree auto::current-eproof))))

(defun proven (x) 
  (member (libitem-provability x)
	  '("Successful mode for automatic proof found automatically."
	    "Automatic expansion proof and translation to natural deduction"
	    "Automatic expansion proof"
	    "Semi-automatic expansion proof in MATE top level"
	    "Interactive expansion proof in MATE top level" 
	    "Semi-automatic natural deduction proof"
	    "Interactive natural deduction proof")
	  :test 'string=))

(defun unproven (x) 
  (not (proven x)))

(defun true-fo-check (gwff)
  (cond ((lsymbol-q gwff)
	 (if (equality-p gwff)
	     (let ((tp (type gwff)))
	       (and (lsymbol-p (cdar tp)) (lsymbol-p (cdr tp))))
	   t))
	((boundwff-q gwff)
	 (if (memq (cdar gwff) '(FORALL EXISTS))
	     (let ((tp (type (caar gwff))))
	       (and (symbolp tp) (true-fo-check (cdr gwff))))
	   nil))
	(t (and (true-fo-check (cdr gwff)) (true-fo-check (car gwff))))))

(defun get-real-filter (filter)
  (case filter 
	(SK-FIRST-ORDER (if (module-loaded-p 'auto-basic) 
			 #'(lambda (x) (first-order-check (libitem-description x)))
		       (progn (msgf "You need the AUTO module for this. Showing ALL wffs instead")
			      #'(lambda (x) x))))
	(SK-HIGHER-ORDER (if (module-loaded-p 'auto-basic) 
			  #'(lambda (x) (not (first-order-check (libitem-description x))))
			(progn (msgf "You need the AUTO module for this. Showing ALL wffs instead")
			       #'(lambda (x) x))))
	(FIRST-ORDER #'(lambda (x) (true-fo-check (eliminate-defns (libitem-description x)))))
	(HIGHER-ORDER #'(lambda (x) (not (true-fo-check (eliminate-defns (libitem-description x))))))
	(PROVEN 'proven)
	(UNPROVEN 'unproven)
	(AUTO-PROOF #'(lambda (x) 
		  (member (libitem-provability x)
			  '("Successful mode for automatic proof found automatically."
			    "Automatic expansion proof and translation to natural deduction"
			    "Automatic expansion proof"
			    "Semi-automatic expansion proof in MATE top level")
			  :test 'string=)))
	(WITH-DEFN #'(lambda (x) (auto::contains-defn-not-equiv (libitem-description x))))
	(WITH-EQUALITY (if (module-loaded-p 'auto-basic) 
		      #'(lambda (x) 
			  (auto::contains-equality-eventually (libitem-description x)))
		    (progn (msgf "You need the AUTO module for this. Showing ALL wffs instead")
			   #'(lambda (x) x))))
	(t #'(lambda (x) x))))

(defun show-all-wffs-anywhere (backup filter)
  (let ((libwfflist nil)
	(dirs (mapcar 'directory-namestring backup-lib-dir)))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val)
				   (when (eq 'gwff (car elt))
					 (if backup
					     (setq libwfflist (cons key libwfflist))
					   (when (not (member (directory-namestring (cdr elt)) dirs :test 'string=))
						 (setq libwfflist (cons key libwfflist))))))))
	     *lib-masterindex*)
  (setq libwfflist (sort (remove-duplicates libwfflist :test 'string=) #'string-lessp))
  (dolist (elt libwfflist)
	  (let ((item (retrieve-item elt :type 'gwff :multiple nil :fail-gently t)))
	    (when (and item (keyword-filter item filter))
	      (princ ">>>")
	      (msg elt t t)
	      (write-brief-description item)
	      (msg t t))))
  (msg t t)))


(deflibrary show-wffs-in-file
  (lib-argnames file)
  (lib-argtypes filespec)
  (lib-arghelp "Name of file")
  (mhelp "Lists the wffs in a file."))

(defun show-wffs-in-file (fname)
  (let ((filename (complete-lib-filename fname)))
    (if filename
	(with-open-file (*input-stream* filename :direction :input)
			(do ((item (read *input-stream* nil control-d-char)
				   (read *input-stream* nil control-d-char)))
			    ((eq item control-d-char))
			    (when (eq (cadr (memq :type item)) 'gwff)
				  (princ ">>>")
				  (msg (cadr (memq :name item)) t)
				  (let ((object (retrieve-item (cadr (memq :name item)) :type 'gwff 
							       :multiple nil
							       :preferred-dir (directory-namestring filename))))
				    (when object (write-brief-description object)))
				  (msg t t))))
      (complain t "I can't locate file: " (filename . filespec)))))

(deflibrary show-objects-in-file
  (lib-argnames file types)
  (lib-argtypes filespec lib-argtype-list)
  (lib-arghelp "Name of file" "Types of objects to show")
  (lib-defaultfns (lambda (x y) 
		    (list x 
			  (if (eq y '$)
			      '(abbr dpairset gwff lib-const mode mode1 slist)
			    y))))
  (mhelp "Lists all the objects of the given type (or types) in a file.

If more than one file of the given name is found in the library directories
in DEFAULT-LIB-DIR and BACKUP-LIB-DIR, the user is prompted to choose one."))

(defun show-objects-in-file (fname types)
  (let ((filename (complete-lib-filename-choose fname))
	(load-warn-p nil))
    (when filename
	  (with-open-file (*input-stream* filename :direction :input)
			  (do ((item (read *input-stream* nil control-d-char)
				     (read *input-stream* nil control-d-char)))
			      ((eq item control-d-char))
			      (when (memq (cadr (memq :type item)) types)
				    (princ ">>>")
				    (msg (cadr (memq :name item)) "  : " (cadr (memq :type item)) t)
				    (let ((object (retrieve-item 
						   (cadr (memq :name item)) 
						   :type (cadr (memq :type item))
						   :multiple nil
						   :preferred-dir (directory-namestring filename))))
				      (when object (write-brief-d-and-m object)))
				    (msg t t)))))))

(deflibrary list-of-libobjects
  (lib-argnames type backup)
  (lib-argtypes lib-argtype-or-nil yesno)
  (lib-arghelp "Types of objects or NIL for all objects." "Include backup directories?")
  (lib-defaultfns (lambda (obj backup) (list (if (eq obj '$) nil obj) (if (eq backup '$) nil backup))))
  (lib-mainfns list-of-libobjects)
  (mhelp "List all objects or all objects of specified TYPE."))

(defun list-of-libobjects (type backup)
  (let ((objlist nil))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val) (setq objlist (cons (list key elt) objlist)))))
	     *lib-masterindex*)
    (unless backup (let ((dirs (mapcar 'directory-namestring backup-lib-dir)))
		     (setq objlist (remove-if #'(lambda (x) 
						  (member (directory-namestring (cdadr x)) dirs :test 'string=))
					      objlist))))
    (setq objlist (sort objlist #'string-lessp :key #'car))
    (if type (dolist (elt objlist) (when (eq type (caadr elt)) (msg (car elt) (t 15) (pfile (namestring (cdadr elt))) t)))
      (dolist (elt objlist) (msg (car elt) (t 15) (caadr elt) (t 21) (pfile (namestring (cdadr elt))) t)))))

(deflibrary libfiles
  (lib-argnames)
  (lib-argtypes)
  (lib-arghelp)
  (lib-defaultfns (lambda () nil))
  (lib-mainfns libfiles)
  (mhelp "Lists all library files in the current default directories,
or in a single chosen directory."))

(defun libfiles ()
  (let ((dirs nil)
	(filelist nil)
	(count 0))
    (complain "List Files In:")
    (complain "1) All Directories in DEFAULT-LIB-DIR")
    (complain "2) All Directories in BACKUP-LIB-DIR")
    (complain "3) All Directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR")
    (complain "4) A Single Library Directory")
    (complain "5) DO NOTHING")
    (setq count (get-a-number 5))
    (cond ((= count 1) (setq dirs default-lib-dir))
	  ((= count 2) (setq dirs backup-lib-dir))
	  ((= count 3) (setq dirs (append default-lib-dir backup-lib-dir)))
	  ((= count 4) (let ((dir (choose-library-directory :backups t)))
			 (when dir
			       (setq dirs (list dir))))))
    (when dirs
	  (maphash #'(lambda (key val)
		       (when key
			     (dolist (elt val)
				     (if (and (member (directory-namestring (cdr elt)) dirs
						      :test #'string=)
					      (not (member (namestring (cdr elt)) filelist
							   :test #'string=)))
					 (setq filelist (cons (namestring (cdr elt)) filelist))))))
		   *lib-masterindex*)
	  (setq filelist (sort filelist #'string-lessp))
	  (dolist (elt filelist) (msg (pfile elt) t)))))

(deflibrary libobjects-in-file
  (lib-argnames file)
  (lib-argtypes filespec)
  (lib-arghelp "Name of file")
  (mhelp "Lists the contents of a file.

If more than one file of the given name is found in the library directories
in DEFAULT-LIB-DIR and BACKUP-LIB-DIR, the user is prompted to choose one."))

(defun  libobjects-in-file (fname &optional (typelist nil))
  (let ((filename (complete-lib-filename-choose fname))
	(load-warn-p nil))
    (msgf "Objects in " (pfile (namestring filename)) " are:" t)
    (with-open-file (*input-stream* filename :direction :input)
		    (do ((item (read *input-stream* nil control-d-char)
			       (read *input-stream* nil control-d-char)))
			((eq item control-d-char))
			(when (or (null typelist) (memq (cadr (memq :type item)) typelist))
			      (msg t (cadr (memq :name item)) 5 (cadr (memq :type item))))))))

(deflibrary scribelibfile
  (lib-argtypes filespeclist filespeclist symbol)
  (lib-argnames filenamesin filenamesout verbosity)
  (lib-arghelp "Library files to read" "MSS files to write" "Verbosity (MIN/MED/MAX)")
  (lib-defaultfns (lambda (filenamesin filenamesout verbosity)
		    (list filenamesin (if (eq filenamesout '$) filenamesin filenamesout) (if (eq verbosity '$) 'MIN verbosity))))
  (mhelp "Print the specified library files into MSS files.
The three verbosity settings are: MIN, which just shows the names of the 
objects, MED, which shows the help messages, keywords, provability and wffs 
as well, and MAX, which shows everything. It can take a list of filenames and 
a corresponding list of output files; if the latter list is too long it will 
be truncated, and if it is too short then the last filename given will be used 
for all the remaining output (so you can write a group of library files to a 
single output file by only supplying one output filename).  
After leaving TPS, run the .mss files through Scribe and print the resulting
files.

Some files in the list of library files may be ambiguous, in the 
sense that more than one file of the given name exists in the 
library directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR.  
In this case, the user is prompted to disambiguate each ambiguous
filename from first to last."))



(deflibrary scribelibdir
  (lib-argtypes dirspec lib-argtype-list filespec symbol yesno)
  (lib-argnames directory types filename verbosity eject)
  (lib-arghelp "Library directory to read" "Types of objects to show (RETURN for all)"
	       "MSS file to write" "Verbosity (MIN/MED/MAX)" "New page for each file?")
  (lib-defaultfns (lambda (directory types filename verbosity eject)
		    (list (if (eq directory '$)
			      (or (car default-lib-dir) default-lib-dir) directory)
			  (if (eq types '$) nil types)
			  filename (if (eq verbosity '$) 'MIN verbosity)
			  (if (eq eject '$) t eject))))
  (mhelp "Print all the library files in a given directory into 
MSS files. See SCRIBELIBFILE for details."))

(deflibrary texlibdir
  (lib-argtypes dirspec lib-argtype-list filespec symbol yesno)
  (lib-argnames directory types filename verbosity eject)
  (lib-arghelp "Library directory to read" "Types of objects to show (RETURN for all)"
	       "TEX file to write" "Verbosity (MIN/MED/MAX)" "New page for each file?")
  (lib-defaultfns (lambda (directory types filename verbosity eject)
		    (list (if (eq directory '$)
			      (or (car default-lib-dir) default-lib-dir) directory)
			  (if (eq types '$) nil types)
			  filename (if (eq verbosity '$) 'MIN verbosity)
			  (if (eq eject '$) t eject))))
  (mhelp "Print all the library files in a given directory into 
TEX files. See TEXLIBFILE for details."))

(defun scribelibdir (dir typ fil ver eject)
  (writelibdir dir typ fil ver eject t))

(defun texlibdir (dir typ fil ver eject)
  (writelibdir dir typ fil ver eject nil))

(defun writelibdir (dir types fil ver eject scribe)
  (let ((filelist nil))
    (maphash #'(lambda (key val)
		 (when key
		       (dolist (elt val)
			       (if types 
				   (when (memq (car elt) types)
					 (setq filelist (cons (namestring (cdr elt)) filelist)))
				 (setq filelist (cons (namestring (cdr elt)) filelist))))))
	     *lib-masterindex*)
    (when (null types) (setq types (remove-if #'listp global-libobject-list)))
    (setq filelist (remove-duplicates filelist :test 'string=))
    (setq filelist (remove-if-not #'(lambda (x) 
				      (string= (directory-namestring x) dir))
				  filelist))
    (setq filelist (sort filelist #'string-lessp))
    (if scribe 
	(scribelibfile filelist (list fil) ver types eject dir)
      (texlibfile filelist (list fil) ver types eject dir))))

(defun scribelibfile (fnames fnamesout verbosity &optional (types nil) (eject nil) (title nil))
  (let ((full-names (mapcar #'complete-lib-filename-choose fnames))
	(load-warn-p nil))
    (do* ((fname (car fnames) (car fnames))
	  (fnames (cdr fnames) (cdr fnames))
	  (filenames full-names (cdr filenames))
	  (filename (car filenames) (car filenames))
	  (continuing nil (not (car fnamesout)))
	  (first t nil)
	  (fnameout (if (null fnamesout) fname (car fnamesout)) (if (null fnamesout) fnameout (car fnamesout)))
	  (fnamesout (cdr fnamesout) (cdr fnamesout)))
	 ((null fname))
	 (scribelibfile-real fname filename fnameout 
			     verbosity (not continuing) (or (car fnamesout) (not (car fnames))) fnames 
			     types title first eject))))

(defun scribelibfile-real (fname filename fnameout verbosity write-preamble write-postamble fnames 
				 &optional (types nil) (title nil) (first nil) (eject nil))
  (reroute-output-append fnameout
     (namestring (make-pathname% :name fnameout :type "mss"))
     (if use-internal-print-mode
	 (in-mode scribe-otl
		  (when write-preamble 
			(msgf scribe-preamble)
			(format T "~%@PageHeading(Immediate, Left <~A>~
				     ,Center <@value(Page)>, Right <~A>)~
				     ~%@PageFooting(Immediate, Right <~A>)~
				     ~%@Begin(Verbatim, LineWidth ~D)~%"
				(or (and (not eject) title)
				    (and eject (file-namestring fname))
				    (if write-postamble fname
				      (string-right-trim ")" 
					 (string-left-trim "(" 
					   (princ-to-string (cons fname fnames))))))
				(status-userid) (stringdt nil)
				scribe-line-width))
		  (when (and types first (not eject) (not write-postamble)) 
			(msgf t "--------next file: " fname "--------" t))
		  (cond ((eq verbosity 'MAX)(maximum-output filename types))
			((eq verbosity 'MIN)(libobjects-in-file filename types))
			(t (medium-output filename types)))
		  (if write-postamble (msgf scribe-postamble) 
		    (if eject
			(progn (msgf "@end(Verbatim)" t "@newpage" t)
			       (format T "~%@PageHeading(Immediate, Left <~A>~
				     ,Center <@value(Page)>, Right <~A>)~
				     ~%@PageFooting(Immediate, Right <~A>)~
				     ~%@Begin(Verbatim, LineWidth ~D)~%"
				       (file-namestring (car fnames))
				       (status-userid) (stringdt nil)
				       scribe-line-width))
		    (msgf t "--------next file: " (car fnames) "--------" t))))
       (let ((style 'scribe))
	 (when write-preamble 
	       (msgf scribe-preamble)
	       (format T "~%@PageHeading(Immediate, Left <~A>~
			  ,Center <@value(Page)>, Right <~A>)~
			  ~%@PageFooting(Immediate, Right <~A>)~
			  ~%@Begin(Verbatim, LineWidth ~D)~%"
		       (or (and (not eject) title)
			   (and eject (file-namestring fname))
			   (if write-postamble fname
			     (string-right-trim ")" 
			       	(string-left-trim "(" 
				   (princ-to-string (cons fname fnames))))))
		       (status-userid) (stringdt nil)
		       scribe-line-width))
		  (when (and types (not eject)
			     first (not write-postamble)) (msgf t "--------next file: " fname "--------" t))
	 (cond ((eq verbosity 'MAX)(maximum-output fname types))
	       ((eq verbosity 'MIN)(libobjects-in-file filename))
	       (t (medium-output fname types)))
	 (if write-postamble (msgf scribe-postamble) 
	   (if eject
	       (progn (msgf "@end(Verbatim)" t "@newpage" t)
		      (format T "~%@PageHeading(Immediate, Left <~A>~
				     ,Center <@value(Page)>, Right <~A>)~
				     ~%@PageFooting(Immediate, Right <~A>)~
				     ~%@Begin(Verbatim, LineWidth ~D)~%"
			      (file-namestring (car fnames))
			      (status-userid) (stringdt nil)
			      scribe-line-width))
	     (msgf t "--------next file: " (car fnames) "--------" t)))))))

(deflibrary texlibfile
  (lib-argtypes filespeclist filespeclist symbol)
  (lib-argnames filenamesin filenamesout verbosity)
  (lib-arghelp "Library files to read" "TeX files to write" "Verbosity (MIN/MED/MAX)")
  (lib-defaultfns (lambda (filenamesin filenamesout verbosity)
		    (list filenamesin (if (eq filenamesout '$) filenamesin filenamesout) (if (eq verbosity '$) 'MIN verbosity))))
  (mhelp "Print the specified library files into TeX files.
The three verbosity settings are: MIN, which just shows the names of the 
objects, MED, which shows the help messages, keywords, provability and wffs 
as well, and MAX, which shows everything. It can take a list of filenames 
and a corresponding list of output files; if the latter list is too long it 
will be truncated, and if it is too short then the last filename given will 
be used for all the remaining output (so you can write a group of library 
files to a single output file by only supplying one output filename).  
After leaving TPS, run the .tex files through TeX and print the resulting
files."))

(defun texlibfile (fnames fnamesout verbosity &optional (types nil) (eject nil) (title nil))
    (when (not (probe-file tpstex))
	  (msgf "Please input the correct full pathname of the file tps.tex.")
	  (update-flag 'tpstex))
    (when (not (probe-file tpstex)) (throwfail "The file tps.tex does not exist."))  
    (let ((full-names (mapcar #'complete-lib-filename-choose fnames))
	  (load-warn-p nil))
      (do* ((fname (car fnames) (car fnames))
	    (fnames (cdr fnames) (cdr fnames))
	    (filenames full-names (cdr filenames))
	    (filename (car filenames) (car filenames))
	    (continuing nil (not (car fnamesout)))
	    (first t nil)
	    (fnameout (if (null fnamesout) fname (car fnamesout)) (if (null fnamesout) fnameout (car fnamesout)))
	    (fnamesout (cdr fnamesout) (cdr fnamesout)))
	   ((null fname))
	   (texlibfile-real fname filename 
			    fnameout verbosity (not continuing) (or (car fnamesout) (not (car fnames))) fnames types
			    title first eject))))

(defun texlibfile-real (fname filename fnameout verbosity write-preamble write-postamble fnames 
			      &optional (types nil) (title nil) (first nil) (eject nil))
  (reroute-output-append fnameout
     (namestring (make-pathname% :name fnameout :type "tex"))
     (if use-internal-print-mode
	 (in-mode tex-1-otl
		  (when write-preamble
			(if latex-emulation 
			    (msgf latex-preamble t "\\markhack{{\\bf " 
				  (or (and (not eject) title)
				       (and eject (file-namestring fname))
				       (if write-postamble fname
					 (string-right-trim ")" 
							    (string-left-trim "(" 
									      (princ-to-string (cons fname fnames))))))
				  "}\\hfill " (stringdt nil)
			       "\\hfill{\\bf " (status-userid) "}}" t "{\\obeylines\\tt" t)
			  (progn
			    (princ "\\input ") (princ tpstex) (terpri)
			    (princ "\\headline={\\noindent{\\bf ") 
			    (princ (or (and (not eject) title)
				       (and eject (file-namestring fname))
				       (if write-postamble fname
					 (string-right-trim ")" 
							    (string-left-trim "(" 
									      (princ-to-string (cons fname fnames)))))))
			    (princ "}\\hfil\\folio\\hfil{\\bf ")
			    (princ (status-userid))
			    (princ "}}") (princ "\\vskip36pt ") 
			    (princ "\\footline={\\hfil ") (princ (stringdt nil)) (princ "}")
			    (msgf tex-preamble)
			    (princ "{\\obeylines\\tt "))))
		  (when (and types (not eject) first (not write-postamble))
			  (msg "\\vskip36pt" t "========next file: " fname "========" t "\\vskip36pt"))
		  (cond ((eq verbosity 'MAX) (maximum-output filename types t))
			((eq verbosity 'MIN)
			 (let ((strout ""))
			   (let ((*standard-output* (make-string-output-stream)))
			     (libobjects-in-file filename)
			     (setq strout (get-output-stream-string *standard-output*)))
			   (princ-tex-verbatim strout)))
			(t (medium-output filename types t)))
		  (if write-postamble (progn (msg t "\\vfill ") 
					     (msg "}") ;closes obeylines
					     (if latex-emulation (msgf latex-postamble) (msgf tex-postamble)))
		    (if eject 
			(progn 
			  (msg t "\\vfill")
			  (princ "\\eject")
			  (if latex-emulation
			      (msgf "\\markhack{{\\bf " (file-namestring (car fnames))
				    "}\\hfill " (stringdt nil)
				    "\\hfill{\\bf " (status-userid) "}}" t)
			    (progn
			      (princ "\\headline={\\noindent{\\bf ")
			      (princ (file-namestring (car fnames)))
			      (princ "}\\hfil\\folio\\hfil{\\bf ")      
			      (princ (status-userid))
			      (princ "}}"))))
		      (msg "\\vskip36pt" t "========next file: " (car fnames) "========" t "\\vskip36pt"))))
       (let ((style 'tex-1))
	 (when write-preamble
	       (if latex-emulation 
		   (msgf latex-preamble t "\\markhack{{\\bf " 
			 (or (and (not eject) title)
			     (and eject (file-namestring fname))
			     (if write-postamble fname
			       (string-right-trim ")" 
						  (string-left-trim "(" 
								    (princ-to-string (cons fname fnames))))))
			 "}\\hfill " (stringdt nil)
			 "\\hfill{\\bf " (status-userid) "}}" t "{\\obeylines\\tt " t)
		 (progn
		   (princ "\\input ") (princ tpstex) (terpri)
		   (princ "\\headline={\\noindent{\\bf ") 
		   (princ (or (and (not eject) title)
			      (and eject (file-namestring fname))
			      (if write-postamble fname
				(string-right-trim ")" 
						   (string-left-trim "(" 
								     (princ-to-string (cons fname fnames)))))))
		   (princ "}\\hfil\\folio\\hfil{\\bf ")      
		   (princ (status-userid))
		   (princ "}}") (princ "\\vskip36pt ") 
		   (princ "\\footline={\\hfil ") (princ (stringdt nil)) (princ "}")    
		   (msgf tex-preamble)
		   (princ "{\\obeylines\\tt "))))
	 (when (and types (not eject) first (not write-postamble)) 
	   (msg "\\vskip36pt" t "========next file: " fname "========" t "\\vskip36pt"))
	 (cond ((eq verbosity 'MAX) (maximum-output filename types t))
	       ((eq verbosity 'MIN)
		(let ((strout ""))
		  (let ((*standard-output* (make-string-output-stream)))
		    (libobjects-in-file filename)
		    (setq strout (get-output-stream-string *standard-output*)))
		  (princ-tex-verbatim strout)))
	       (t (medium-output filename types t)))
	 (if write-postamble (progn (msg t "\\vfill ") 
				    (msg "}") ;closes obeylines
				    (if latex-emulation (msgf latex-postamble) (msgf tex-postamble)))
	   (if eject 
	       (progn
		 (msg t "\\vfill")
		 (princ "\\eject")
		 (if latex-emulation
		     (msgf "\\markhack{{\\bf " (file-namestring (car fnames))
			   "}\\hfill " (stringdt nil)
			   "\\hfill{\\bf " (status-userid) "}}" t)
		   (progn
		     (princ "\\headline={\\noindent{\\bf ") 
		     (princ (file-namestring (car fnames)))
		     (princ "}\\hfil\\folio\\hfil{\\bf ")      
		     (princ (status-userid))
		     (princ "}}"))))
	     (msg "\\vskip36pt" t "========next file: " (car fnames) "========" t "\\vskip36pt"))))
       )))

(defun maximum-output (fname &optional (types nil) (tex nil))
  (let ((liblist nil))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val)
				   (when (string-equal (namestring fname) (namestring (cdr elt)))
					 (setq liblist (cons (list key (car elt)) liblist))))))
	     *lib-masterindex*)
    (setq liblist (sort liblist #'string-lessp :key #'car))
    (when types (setq liblist (remove-if-not #'(lambda (x) (memq (cadr x) types)) liblist)))
    (dolist (elt liblist)
	    (if tex (princ "\\par{\\vskip6pt}") (terpri))
	    (let ((item (retrieve-item (car elt) :type (cadr elt) :multiple nil :preferred-dir (directory-namestring fname))))
	      (when item
		(write-libitem item style *standard-output* tex)))
	    (if tex (princ "\\par{\\vskip6pt}") (terpri)))))

(defun medium-output (fname &optional (types nil) (tex nil))
  (let ((libwfflist nil))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val)
				   (when (string-equal (namestring fname) (namestring (cdr elt)))
					 (when (or (null types)
						   (and types (memq (car elt) types)))
					       (if (or (eq 'mode1 (car elt))
						       (eq 'mode (car elt)))
						   (msg key 2 (car elt) t)
						 (setq libwfflist (cons (list key (car elt)) libwfflist))))))))
	     *lib-masterindex*)
    (setq libwfflist (sort libwfflist #'string-lessp :key #'car))
    (dolist (elt libwfflist)
	    (if tex (princ "\\par{\\vskip6pt}") (terpri))
	    (let ((item (retrieve-item (car elt) :type (cadr elt) :multiple nil :preferred-dir (directory-namestring fname))))
	      (when item (progn (msg (car elt) t) 
				(if tex (princ "\\par{\\vskip6pt}") (terpri))
				(write-brief-d-and-m item style *standard-output* tex))))
	    (if tex (princ "\\par{\\vskip6pt}") (terpri)))))

(defmexpr displayfile
  (argtypes filespec yesno)
  (argnames filename bigwin)
  (arghelp "File to be shown" "Big Window?")
  (defaultfns (lambda (x y)
		(list x (if (eq y '$) nil y))))
  (mhelp "Open a (big) window in which the contents of the given file will be displayed.
Once the end of the file is reached, a message will be printed and
some additional blank lines will be added. Once the end of the
blank lines is reached, the window will vanish."))

(defun displayfile (infilename bigwin)
 (if (not (probe-file infilename)) (throwfail "The file given doesn't exist."))
 (let ((outfilename (concatenate 'string "/tmp/foo" (princ-to-string (get-universal-time)))))
   (with-open-file (outstream outfilename :direction :output)
      (with-open-file (instream infilename :direction :input)
        (do ((hxstring (read-line instream nil nil) (read-line instream  nil nil)))
            ((not hxstring) nil)
            (write-line hxstring outstream)))
;;;I don't use (terpri) because 'more' command may ignore blank lines.
      (dotimes (i 10 nil) (write-line " "  outstream)) 
      (write-line "-------------------- END of FILE --------------------" outstream)
      (dotimes (i 40 nil) (write-line " "  outstream)))
     (if bigwin
         (core::setup-big-xterm-window "TpsMore" outfilename "80x25+0+0" "more -Cf")
         (core::setup-xterm-window "TpsMore" outfilename "80x50+0+0" "more -Cf"))))
