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
;;; File UNIX-LIBRARY
;;;
;;; defines UNIX-LIBRARY, FLAGS etc.
;;;

(deffile unix-library1
  (part-of library)
  (extension lisp)
  (mhelp "Defines top-level for unix-style library interface."))

(context subtoplevels)

(defvar *unix-libclass-scheme-changed* nil)

(deftoplevel unix-library-top
  (top-prompt-fn unix-library-top-prompt)
  (command-interpreter unix-library-command-interpreter)
  (print-* library-print-*)
  (top-level-category unix-librarycmd)
  (top-level-ctree unix-library-command-ctree)
  (top-cmd-decode unix-lib-opdecode)
  (mhelp "The top level of for accessing the TPS Library using a Unix-style Interface.

The value of the flag CLASS-SCHEME determines what classification scheme
is used to determine the virtual directory structure.

If the flag UNIXLIB-SHOWPATH is T, the prompt will be
<<CLASSSCHEME>:<PATH TO CLASS><num>>

If the flag UNIXLIB-SHOWPATH is NIL, the prompt will be
<LIB:<CLASS><num>>

See Also: UNIXLIB, PSCHEMES, CLASS-SCHEME, UNIXLIB-SHOWPATH, CD, LS, PWD,
LN, RM, MKDIR, FETCH, SHOW, PINTERSECT, PINTERSECT*"))

(eval-when (load compile eval)
(defcategory unix-librarycmd
  (define defunixlibrary)
  (properties
    (ulib-argtypes multiple)
    (ulib-argnames multiple)
    (ulib-arghelp multiple)
    (ulib-defaultfns multiplefns)
    (ulib-mainfns singlefn)
    (mhelp single))
  (global-list global-unix-librarylist)
  (shadow t)
  (mhelp-line "library command using a unix style interface")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command
	(format nil "@IndexOther(~A)" (symbol-name item))
	(get item 'lib-argnames)
	(cdr (assoc 'unixlibrarycmd (get item 'mhelp))))))
  (mhelp-fn (unix-librarycmd-mhelp unix-librarycmd category))))

(defun unix-librarycmd-mhelp (keyword category)
  (princ-mhelp keyword category)
  (unless short-help 
    (when *doing-html* (msg " fnord "))
    (msgf "The command format for " keyword " is:" -2 "<LIB>" keyword)
    (print-tps-format* keyword " "
		       (+ 5 (length (format nil "~A" keyword)))
		       (get keyword 'ulib-argnames)
		       (get keyword 'ulib-argtypes)
		       nil)))

(defun unix-library ()
  (declare (special global-class-scheme-list *unix-libclass-scheme-changed*))
  (let ((sl (remove-if-not #'symbolp global-class-scheme-list)))
    (unless sl
      (throwfail "There are no class schemes currently in memory.
Try fetching one from the library."))
    (when (and sl (not (cdr sl))
	       (not class-scheme))
      (set-flag 'class-scheme (car sl))
      (setq *current-class* (get class-scheme 'libclass)))
    (let ((cs nil))
      (loop until (and class-scheme (libclass-p (get class-scheme 'libclass))
		       (libclass-p *current-class*)) do
	    (prompt-read cs nil 
			 (msgf "Classification Scheme")
			 'symbol (car sl)
			 ((? (msgf "Classification Scheme - one of " sl))))
	    (when (libclass-p (get cs 'libclass))
	      (set-flag 'class-scheme cs)
	      (setq *current-class* (get cs 'libclass))))))
  (setq *unix-libclass-scheme-changed* nil)
  (%catch% (unix-librarytop)
	   (exit-inferior-top core::expand-catch-throw)))

(defun unix-librarytop () 
  (let ((top-prompt-fn #'unix-library-top-prompt)
	(command-interpreter #'unix-library-command-interpreter)
	(print-* #'library-print-*)
	(top-level 'unix-library-top))
    (declare (special top-prompt-fn command-interpreter print-* top-level
		      command-ctree lib-object))
    (secondary-top)))

(defflag unixlib-showpath
  (flagtype boolean)
  (default t)
  (mhelp "If T, print the current class as a directory in the prompt
in the Unix Style Library Top Level.

If the value is T, the prompt will be
<<CLASSSCHEME>:<PATH TO CLASS><num>>

If the value is NIL, the prompt will be
<LIB:<CLASS><num>>

See Also: UNIXLIB, PSCHEMES, CLASS-SCHEME, CD, LS, PWD, LN, RM,
MKDIR, FETCH, SHOW"))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun unix-library-top-prompt (id)
  (if (libclass-p *current-class*)
      (if unixlib-showpath
	  (format nil "<~d:~d:~A>" class-scheme (class-fullpath *current-class*) id)
	(format nil "<LIB:~d:~A>" (libclass-name *current-class*) id))
    (format nil "<LIB:CLASS:~A>" id)))

(defun unix-library-command-interpreter (cmd)
  (let ((carcmd (car cmd)))
    (setq *retrieve-stack* nil) ; just in case!
    (cond ((null cmd) nil)
	  ((and (null (cdr cmd)) (atom carcmd))
	   (cond 
	         ((integerp carcmd)
		  (throwfail "Unknown command."))
		 ((and (symbolp carcmd) (get carcmd 'unix-librarycmd))
		  `(unix-lib-opdecode (quote ,cmd)))
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
	  ((and (symbolp carcmd) (get carcmd 'unix-librarycmd))
	   `(unix-lib-opdecode (quote ,cmd))) 
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

(defun unix-lib-opdecode (command)
  (let ((keyword (car command))
	mainfn result)
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values keyword
		       (copy (cdr command))
		       (get keyword 'ulib-argtypes)
		       (mapcar #'(lambda (x) (declare (ignore x)) nil)
			       (get keyword 'ulib-argtypes))
		       nil
		       (get keyword 'ulib-defaultfns)
		       nil
		       (get keyword 'ulib-argnames)
		       (get keyword 'ulib-arghelp))
      (declare (ignore external-arglist))
      (setq mainfn (or (get keyword 'ulib-mainfns) keyword))
      (%catch% (setq result (apply mainfn internal-arglist))
	       (fail (complain f "Error from " mainfn ".  " 
                               core::expand-catch-throw)
		     (throwfail "Operation aborted.")))
      result)))

(defmexpr unixlib
  (mainfns unix-library)
  (mhelp "Enter the library top-level with a unix style interface.

The value of the flag CLASS-SCHEME determines what classification scheme
is used to determine the virtual directory structure.

If the flag UNIXLIB-SHOWPATH is T, the prompt will be
<<CLASSSCHEME>:<PATH TO CLASS><num>>

If the flag UNIXLIB-SHOWPATH is NIL, the prompt will be
<LIB:<CLASS><num>>

See Also: LIB, PSCHEMES, CLASS-SCHEME, UNIXLIB-SHOWPATH, CD, LS, PWD, LN, RM,
MKDIR, FETCH, SHOW"))

(context subtoplevels)


(defunixlibrary leave
  (ulib-mainfns exit-unix-library)
  (mhelp "Leave the Unix-style LIBRARY to the next enclosing top level."))

(defun exit-unix-library ()
  (when (and *unix-libclass-scheme-changed*
	     (query (format nil "Save changes to ~d?" class-scheme) t))
    (insert-libobject class-scheme 'CLASS-SCHEME))
  (%throw% '|[Left UNIX STYLE LIBRARY.]| exit-inferior-top))

(context lib-class)

(defunixlibrary cl-user::ls ; cannot export ls from core . . .
  (ulib-mainfns unixlib-ls)
  (mhelp "List the Library Items and the Subclasses in the current class.

See Also: UNIXLIB"))

(defunixlibrary cl-user::cd ; cannot export cd from core . . .
  (ulib-argtypes anything)
  (ulib-argnames class)
  (ulib-arghelp "Subclass of Current Class, or ., or .., or Full Path")
  (ulib-defaultfns (lambda (x)
		    (list (if (eq x '$)
			      "."
			    x))))
  (ulib-mainfns unixlib-cd)
  (mhelp "Change to a different Library Class.

See Also: UNIXLIB"))

(defun unixlib-ls ()
  (let ((str ""))
    (dolist (l (libclass-kids *current-class*))
	    (let ((str2 (format nil "~d/" (libclass-name l))))
	      (if (>= (+ (length str) (length str2)) rightmargin)
		  (if (= (length str) 0)
		      (msgf str2)
		    (progn
		      (msgf str)
		      (setq str str2)))
		(if (= (length str) 0)
		    (setq str str2)
		  (setq str (format nil "~d ~d" str str2))))))
    (dolist (i (libclass-libitems *current-class*))
	    (let ((str2 (format nil "~d" i)))
	      (if (>= (+ (length str) (length str2)) rightmargin)
		  (if (= (length str) 0)
		      (msgf str2)
		    (progn
		      (msgf str)
		      (setq str str2)))
		(if (= (length str) 0)
		    (setq str str2)
		  (setq str (format nil "~d ~d" str str2))))))
    (when (> (length str) 0)
      (msgf str))))

(defun unixlib-cd (class)
  (let ((cl (locate-unixlibclass class)))
    (when cl
      (setq *current-class* cl))))

(defun locate-unixlibclass (class)
  (let ((classstr (string-upcase (format nil "~d" class)))
	(ret *current-class*)
	(par nil))
    (cond ((or (string= classstr "")
	       (string= classstr ".")))
	  ((string= classstr "..")
	   (if (libclass-parents *current-class*)
	       (setq ret (car (libclass-parents *current-class*)))
	     (complain "Already at the root.")))
	  (t
	   (let ((cl *current-class*)
		 (str1 "")
		 (str2 ""))
	     (dotimes (i (length classstr))
		      (case (aref classstr i)
			    ((#\space #\newline #\return)
			     (let* ((name (intern-str str2))
				    (cl2 (member name (libclass-kids cl) :key #'libclass-name)))
			       (unless cl2
				 (throwfail str1 " does not exist."))
			       (setq par cl)
			       (setq ret (car cl2)) (return nil)))
			    (#\/ 
			     (if (= i 0)
				 (progn
				   (setq str1 "/")
				   (setq cl (get class-scheme 'libclass)))
			       (unless (string= str2 ".")
				 (setq str1 (format nil "~d/" str1))
				 (if (string= str2 "..")
				     (if (libclass-parents ret)
					 (progn
					   (setq cl (car (libclass-parents cl)))
					   (setq str2 ""))
				       (throwfail "Path goes beneath the root."))
				   (when (and (> (length str2) 0)
					      (not (all-dots str2)))
				     (let* ((name (intern-str str2))
					    (cl2 (member name (libclass-kids cl)
							 :key #'libclass-name)))
				       (unless cl2
					 (throwfail str1 " does not exist."))
				       (setq str2 "")
				       (setq par cl)
				       (setq cl (car cl2))))))))
			    (t
			     (setq str1 (format nil "~d~d" str1 (aref classstr i)))
			     (setq str2 (format nil "~d~d" str2 (aref classstr i))))))
	     (if (or (string= str2 "") (string= str2 "."))
		 (setq ret cl)
	       (if (string= str2 "..")
		   (if (libclass-parents cl)
		       (setq ret (car (libclass-parents cl)))
		     (throwfail "Path goes beneath the root."))
		 (let* ((name (intern-str str2))
			(cl2 (member name (libclass-kids cl) :key #'libclass-name)))
		   (unless cl2
		     (throwfail str1 " does not exist."))
		   (setq str1 (format nil "~d/" str1))
		   (setq par cl)
		   (setq ret (car cl2))))))))
    (values ret par)))

(defun locate-unixlibclass-or-item (class)
  (let ((classstr (string-upcase (format nil "~d" class)))
	(ret *current-class*)
	(par nil))
    (cond ((or (string= classstr "")
	       (string= classstr ".")))
	  ((string= classstr "..")
	   (if (libclass-parents *current-class*)
	       (setq ret (car (libclass-parents *current-class*)))
	     (complain "Already at the root.")))
	  (t
	   (let ((cl *current-class*)
		 (str1 "")
		 (str2 ""))
	     (dotimes (i (length classstr))
		      (case (aref classstr i)
			    ((#\space #\newline #\return)
			     (let* ((name (intern-str str2))
				    (cl2 (member name (libclass-kids cl) :key #'libclass-name)))
			       (unless cl2
				 (throwfail str1 " does not exist."))
			       (setq par cl)
			       (setq ret (car cl2)) (return nil)))
			    (#\/ 
			     (if (= i 0)
				 (progn
				   (setq str1 "/")
				   (setq cl (get class-scheme 'libclass)))
			       (unless (string= str2 ".")
				 (setq str1 (format nil "~d/" str1))
				 (if (string= str2 "..")
				     (if (libclass-parents ret)
					 (progn
					   (setq cl (car (libclass-parents cl)))
					   (setq str2 ""))
				       (throwfail "Path goes beneath the root."))
				   (when (and (> (length str2) 0)
					      (not (all-dots str2)))
				     (let* ((name (intern-str str2))
					    (cl2 (member name (libclass-kids cl)
							 :key #'libclass-name)))
				       (unless cl2
					 (throwfail str1 " does not exist."))
				       (setq str2 "")
				       (setq par cl)
				       (setq cl (car cl2))))))))
			    (t
			     (setq str1 (format nil "~d~d" str1 (aref classstr i)))
			     (setq str2 (format nil "~d~d" str2 (aref classstr i))))))
	     (if (or (string= str2 "") (string= str2 "."))
		 (setq ret cl)
	       (if (string= str2 "..")
		   (if (libclass-parents cl)
		       (setq ret (car (libclass-parents cl)))
		     (throwfail "Path goes beneath the root."))
		 (let* ((name (intern-str str2))
			(cl2 (member name (libclass-kids cl) :key #'libclass-name)))
		   (setq str1 (format nil "~d/" str1))
		   (if cl2
		       (progn
			 (setq par cl)
			 (setq ret (car cl2)))
		     (if (member name (libclass-libitems cl))
			 (progn
			   (setq par *current-class*)
			   (setq par cl)
			   (setq ret name))
		       (throwfail str1 " does not exist.")))))))))
    (values ret par)))

(defun unixlib-pwd ()
  (let ((str (class-fullpath *current-class*)))
    (if (string= str "")
	(msgf "/")
      (msgf str)))
  nil)

; return a string like "/top/next/current"
(defun class-fullpath (cl)
  (if (libclass-parents cl)
      (format nil "~d/~d"
	      (class-fullpath (car (libclass-parents cl)))
	      (libclass-name cl))
    ""))

(defmenuitem MV
  (display-name "MV")
  (placement 4.1)
  (command "MV")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem RENAME-CLASS
  (display-name "RENAME-CLASS")
  (placement 4.2)
  (command "RENAME-CLASS")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defmenuitem COPY-CLASS-SCHEME
  (display-name "COPY-CLASS-SCHEME")
  (placement 4.3)
  (command "COPY-CLASS-SCHEME")
  (parent UNIXLIB-CLASS)
  (mhelp ""))

(defunixlibrary mv
  (ulib-argnames name newname)
  (ulib-argtypes anything anything)
  (ulib-arghelp "Name of a Class" "New Class Location")
  (ulib-mainfns unixlib-mv)
  (mhelp "Move an item or class to another class.

See Also:  UNIXLIB, CP, RM, CD, MKDIR"))

(defunixlibrary rename-class
  (ulib-argnames name newname)
  (ulib-argtypes anything anything)
  (ulib-arghelp "Name of a Class" "New Name for Class")
  (ulib-mainfns unixlib-rename-class)
  (mhelp "Renames a class.

NOTE:  This changes the name of the class everywhere in the structure.

See Also:  UNIXLIB, CP, RM, CD, MKDIR"))

(defunixlibrary COPY-CLASS-SCHEME
  (ulib-argnames name newname help)
  (ulib-argtypes symbol symbol string)
  (ulib-arghelp "Existing Classification Scheme" "New Name" "Help String")
  (ulib-defaultfns (lambda (name newname help)
		     (list (if (eq name '$)
			       (or CLASS-SCHEME 'LIBDIR)
			     name)
			   newname
			   (if (eq help '$)
			       ""
			     help))))
  (ulib-mainfns unixlib-copy-class-scheme)
  (mhelp "Copy an existing classification scheme to a new name so the new scheme can be modified and saved
in the library under this new name without changing the old classification scheme."))

(defunixlibrary cl-user::rm ; rm cannot be exported
  (ulib-argnames name)
  (ulib-argtypes anything)
  (ulib-arghelp "Name of a Class or an Item")
  (ulib-mainfns unixlib-rm)
  (mhelp "Remove a Classification of a Class or Item.  This 
DOES NOT remove a library item from the library.  It only
removes the classification.

If a class has more than one parent and the class is removed
from its primary parent, a secondary parent becomes the primary
parent.  For example, if C has two parents A and B with A as the
primary parent.  Here, the full path to C is

/A/C

After performing

cd /A
rm C

the full path to C will be

/B/C

See Also: UNIXLIB, LN, MKDIR"))

(defunixlibrary ln
  (ulib-argnames from to)
  (ulib-argtypes anything anything)
  (ulib-arghelp "Link From" "Link To")
  (ulib-mainfns unixlib-ln)
  (mhelp "Classify a Class by essentially creating
a Link to another place it is classified.

Example:

ln /B/C /A

makes the class C a child of A.

Unlike the Unix ln command, the link must have the same name
as the target class.  Also, users are not allowed to create
a link that results in a cycle in the heirarchy.

See Also: UNIXLIB, CLASSIFY-CLASS, CP"))

(defunixlibrary cp
  (ulib-argnames from to)
  (ulib-argtypes anything anything)
  (ulib-arghelp "Copy From" "Copy To Class")
  (ulib-mainfns unixlib-cp)
  (mhelp "Copy a Library Item to another Class.
If the user specifies a Class to copy from,
then all the library items in that class are copied.

See Also: UNIXLIB, CLASSIFY-CLASS, LN"))

(defunixlibrary classify-item
  (ulib-argnames itemname)
  (ulib-argtypes symbol)
  (ulib-arghelp "Library Item")
  (ulib-mainfns unixlib-classify-item)
  (mhelp "Classify a Library Item in the Current Library Class.

See Also: UNIXLIB"))

(defunixlibrary mkdir
  (ulib-argnames class)
  (ulib-argtypes symbol)
  (ulib-arghelp "Class to Create")
  (ulib-mainfns unixlib-mkdir)
  (mhelp "Creates a new Library Class classified under
the Current Library Class.

See Also: UNIXLIB, CREATE-LIBCLASS"))
  
(defun unixlib-mv (from to)
  (multiple-value-bind
      (fromcl fromclpar)
      (locate-unixlibclass-or-item from)
    (let ((tocl (locate-unixlibclass to)))
      (unless (libclass-p tocl)
	(throwfail "Cannot find " to))
      (unless fromcl
	(throwfail "Cannot find " from))
      (unless fromclpar
	(throwfail "Could not determine parent of " from))
      (if (libclass-p fromcl)
	  (if (find-libsubclasses (libclass-name tocl) fromcl)
	      (throwfail (libclass-name fromcl) " is an ancestor of " (libclass-name tocl)
			 ", cannot create a classification loop")
	    (progn
	      (setq *unix-libclass-scheme-changed* t)
	      (setf (libclass-kids fromclpar) (remove fromcl (libclass-kids fromclpar)))
	      (setf (libclass-parents fromcl) (remove fromclpar (libclass-parents fromcl)))
	      (setf (libclass-kids tocl) (union (libclass-kids tocl) (list fromcl)))
	      (setf (libclass-parents fromcl) (union (libclass-parents fromcl) (list tocl)))))
	(let ((classified nil))
	  (declare (special classified))
	  (setq *unix-libclass-scheme-changed* t)
	  (setf (libclass-libitems fromclpar)
		(remove fromcl (libclass-libitems fromclpar)))
	  (classify-item-2 fromcl (libclass-name tocl) tocl)))
      *current-class*)))

(defun unixlib-rm (name)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (when (member name (libclass-libitems *current-class*))
    (setq *unix-libclass-scheme-changed* t)
    (setf (libclass-libitems *current-class*)
	  (remove name (libclass-libitems *current-class*))))
  (let ((a (member name (libclass-kids *current-class*) :key #'libclass-name)))
    (when a
      (setq *unix-libclass-scheme-changed* t)
      (setf (libclass-kids *current-class*)
	    (remove (car a) (libclass-kids *current-class*)))
      (setf (libclass-parents (car a))
	    (remove *current-class* (libclass-parents (car a))))))
  nil)

(defun unixlib-ln (from to)
  (let ((fromcl (locate-unixlibclass from))
	(tocl (locate-unixlibclass to)))
    (unless (libclass-p tocl)
      (throwfail "Cannot find " to))
    (unless (libclass-p fromcl)
      (throwfail "Cannot find " from))
    (progn
      (when (member (libclass-name fromcl) (libclass-kids tocl) :key #'libclass-name)
	(throwfail to " already has a child named " (libclass-name fromcl)))
      (let ((cll (find-libsubclasses (libclass-name tocl) fromcl)))
	(when (member tocl cll)
	  (throwfail from " is an ancestor of " to ", cannot create a classification loop"))
	(setf (libclass-parents fromcl)
	      (append (libclass-parents fromcl) (list tocl)))
	(setf (libclass-kids tocl)
	      (append (libclass-kids tocl) (list fromcl)))))
    (setq *unix-libclass-scheme-changed* t)))

(defun unixlib-cp (from to)
  (let ((fromcl (locate-unixlibclass-or-item from))
	(tocl (locate-unixlibclass to)))
    (unless (libclass-p tocl)
      (throwfail "Cannot find " to))
    (unless fromcl
      (throwfail "Cannot find " from))
    (if (libclass-p fromcl)
	(dolist (a (libclass-libitems fromcl))
	  (setf (libclass-libitems tocl)
		(adjoin a (libclass-libitems tocl))))
      (setf (libclass-libitems tocl)
	    (adjoin fromcl (libclass-libitems tocl))))
    (setq *unix-libclass-scheme-changed* t)))

(defun unixlib-classify-item (itemname)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (let ((classified nil))
    (declare (special classified))
    (classify-item-1 itemname
		     (libclass-name *current-class*)
		     *current-class*))
  (setq *unix-libclass-scheme-changed* t))

(defun unixlib-mkdir (class)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (let* ((new (make-libclass :name class :libitems nil
			     :parents (list *current-class*))))
    (push new (libclass-kids *current-class*))
    (setq *unix-libclass-scheme-changed* t)))

(context lib-reading)

(defunixlibrary destroy
  (ulib-argnames name)
  (ulib-argtypes symbol)
  (ulib-arghelp "Name of object")
  (ulib-defaultfns (lambda (name) (list name)))
  (ulib-mainfns destroy-libobject)
  (mhelp "Remove a library object from TPS (the object will remain
stored in the library)."))

(defunixlibrary fetch
  (ulib-argnames name)
  (ulib-argtypes symbol)
  (ulib-arghelp "Name of object")
  (ulib-mainfns unixlib-retrieve-libobject-real)
  (mhelp "Make a library object from the current class available in TPS.
Will create a new TPS object if EXPERTFLAG is set to T, otherwise
will create a weak label for the new library object.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate.

See Also: CD, LS, PWD, LN, RM, MKDIR, SHOW"))

(defun unixlib-retrieve-libobject-real (name)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (unless (member name (libclass-libitems *current-class*))
    (throwfail name " is not in " (class-fullpath *current-class*)))
  (unless (or (member name core-abbrevlist)
	      (member name core-binderabbrevlist)
	      (member name core-theoremlist)
	      (member name core-constlist))
    (retrieve-libobject name :type nil :fail-gently t)))

(context lib-display)

(defunixlibrary show
  (ulib-argnames name)
  (ulib-argtypes symbol)
  (ulib-arghelp "Name of object")
  (ulib-mainfns unixlib-show-libobject)
  (mhelp "Display a library object.

If more than one library object of this name is stored in
the class and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(defunixlibrary pwd
  (ulib-mainfns unixlib-pwd)
  (mhelp "Print the path to the current class.

See Also: UNIXLIB"))

(defunixlibrary pup
  (ulib-mainfns unixlib-pup)
  (mhelp "Print all the paths up to the current class."))

(defunixlibrary pdown
  (ulib-mainfns unixlib-pdown)
  (mhelp "Print all the subpaths of the current class."))

(defunixlibrary locate
  (ulib-argnames name)
  (ulib-argtypes symbol)
  (ulib-arghelp "Name of class or library object")
  (ulib-mainfns unixlib-locate)
  (mhelp "Locate a class or classified item in the classification scheme."))

(defmenuitem UNIXLIB-PUP
  (display-name "PUP")
  (placement 9)
  (command "PUP")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenuitem UNIXLIB-PDOWN
  (display-name "PDOWN")
  (placement 10)
  (command "PDOWN")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenu UNIXLIB-SEARCH
  (display-name "Search")
  (placement 3)
  (parent UNIX-STYLE-LIB)
  (mhelp ""))

(defmenuitem UNIXLIB-LOCATE
  (display-name "LOCATE")
  (placement 1)
  (parent UNIXLIB-SEARCH)
  (mhelp ""))

(defmenuitem LS
  (display-name "LS")
  (placement 1.2)
  (command "LS")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defmenuitem LS-ITEMS*
  (display-name "LS-ITEMS*")
  (placement 1.5)
  (command "LS-ITEMS*")
  (parent UNIXLIB-DISPLAY)
  (mhelp ""))

(defun unixlib-show-libobject (name)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (unless (member name (libclass-libitems *current-class*))
    (throwfail name " is not in " (class-fullpath *current-class*)))
  (cond ((member name core-abbrevlist)
	 (msgf t name " is an abbrev defined in the TPS core"))
	((member name core-binderabbrevlist)
	 (msgf t name " is a binder abbrev defined in the TPS core"))
	((member name core-theoremlist)
	 (msgf t name " is a theorem defined in the TPS core"))
	((member name core-constlist)
	 (msgf t name " is a constant defined in the TPS core"))
	(t
	 (let ((item (retrieve-item name)))
	   (when item 
	     (write-libitem item)
	     (msg t "Stored in file " (pfile (namestring (libitem-file item)))))))))

(defunixlibrary show-wff
  (ulib-argnames name)
  (ulib-argtypes symbol)
  (ulib-arghelp "Name of object")
  (ulib-defaultfns (lambda (name) (list name)))
  (ulib-mainfns unixlib-show-libobject-wff)
  (mhelp "Display the wff of a gwff in the library.

If more than one library object of this name is stored in
the class and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(defun unixlib-show-libobject-wff (name)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (unless (member name (libclass-libitems *current-class*))
    (throwfail name " is not in " (class-fullpath *current-class*)))
  (cond ((member name core-abbrevlist)
	 (msgf t name " is an abbrev defined in the TPS core")
	 (pwff (get name 'defn)))
	((member name core-binderabbrevlist)
	 (msgf t name " is a binder abbrev defined in the TPS core")
	 (pwff (get name 'defn)))
	((member name core-theoremlist)
	 (msgf t name " is a theorem defined in the TPS core")
	 (if (get name 'defn)
	     (pwff (get name 'defn))
	   (if (stringp (get name 'assertion))
	       (msgf (get name 'assertion))
	     (if (gwff-q (get name 'assertion))
		 (pwff (get name 'assertion))
	       nil))))
	((member name core-constlist)
	 (msgf t name " is a constant defined in the TPS core"))
	(t
	 (let ((item (retrieve-item name)))
	   (when item 
	     (write-description-only item)
	     (msg t "Stored in file " (pfile (namestring (libitem-file item)))))))))

(defunixlibrary show-help
  (ulib-argnames name type)
  (ulib-argtypes symbol lib-argtype-or-nil)
  (ulib-arghelp "Name of object" "argtype or nil")
  (ulib-defaultfns (lambda (name type)
		    (list name (if (eq type '$) nil type))))
  (ulib-mainfns unixlib-show-libobject-help)
  (mhelp "Display the help message associated with a library object.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(defun unixlib-show-libobject-help (name type)
  (declare (ignore type))
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (unless (member name (libclass-libitems *current-class*))
    (throwfail name " is not in " (class-fullpath *current-class*)))
  (cond ((member name core-abbrevlist)
	 (mhelp name))
	((member name core-binderabbrevlist)
	 (mhelp name))
	((member name core-theoremlist)
	 (mhelp name))
	((member name core-constlist)
	 (mhelp name))
	(t
	 (let ((item (retrieve-item name)))
	   (when item 
	     (write-mhelp-only item)
	     (msg t "Stored in file " (pfile (namestring (libitem-file item)))))))))

(defunixlibrary show-wff&help
  (ulib-argnames name)
  (ulib-argtypes symbol)
  (ulib-arghelp "Name of object")
  (ulib-defaultfns (lambda (name)
		    (list name)))
  (ulib-mainfns unixlib-show-libobject-wff-and-help)
  (mhelp "Display the wff of a gwff in the library, with the associated
help message, keywords and provability status."))

(defun unixlib-show-libobject-wff-and-help (name)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (unless (member name (libclass-libitems *current-class*))
    (throwfail name " is not in " (class-fullpath *current-class*)))
  (cond ((member name core-abbrevlist)
	 (msgf t name " is an abbrev defined in the TPS core")
	 (mhelp name))
	((member name core-binderabbrevlist)
	 (msgf t name " is a binder abbrev defined in the TPS core")
	 (mhelp name))
	((member name core-theoremlist)
	 (msgf t name " is a theorem defined in the TPS core")
	 (mhelp name))
	((member name core-constlist)
	 (msgf t name " is a constant defined in the TPS core")
	 (mhelp name))
	(t
	 (let ((item (retrieve-item name)))
	   (when item 
	     (write-description-only item) 
	     (write-mhelp-only item)
	     (msg t "Stored in file " (pfile (namestring (libitem-file item)))))))))

(defunixlibrary show-all-wffs
  (ulib-argnames filter)
  (ulib-argtypes keyword-list)
  (ulib-arghelp "Properties? (RETURN to show all wffs)")
  (ulib-defaultfns (lambda (filter) (list (if (eq filter '$) '() filter))))
  (ulib-mainfns unixlib-show-all-wffs-anywhere)
  (mhelp "Show all wffs in all files in the current library class.
As a filter, you can select any known keywords; only the wffs which 
satisfy all of the given keywords will be shown. See SHOW-KEYWORDS 
for a list of keywords."))

(defun unixlib-show-all-wffs-anywhere (filter)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (let ((already-done nil))
    (dolist (name (libclass-libitems *current-class*))
      (when (member name core-abbrevlist)
	(msgf t ">>> An abbrev defined in the TPS core")
	(pwff (get name 'defn)))
      (when (member name core-binderabbrevlist)
	(msgf t ">>> A binder abbrev defined in the TPS core")
	(pwff (get name 'defn)))
      (when (member name core-theoremlist)
	(msgf t ">>> A theorem defined in the TPS core")
	(if (get name 'defn)
	    (pwff (get name 'defn))
	  (if (stringp (get name 'assertion))
	      (msgf (get name 'assertion))
	    (if (gwff-q (get name 'assertion))
		(pwff (get name 'assertion))
	      nil))))
      (when (member name core-constlist)
	(msgf t ">>> A constant defined in the TPS core") t)
      (dolist (ch (gethash name *lib-masterindex*))
	(let ((item (retrieve-item name :type (car ch)
				   :preferred-dir (directory-namestring (cdr ch))
				   :multiple nil :fail-gently t)))
	  (unless (member (cons item ch) already-done :test #'equal)
	    (when (and item (keyword-filter item filter))
	      (princ ">>>")
	      (msg name t t)
	      (write-brief-description item)
	      (msg t t))))))))

(defunixlibrary pintersect
  (ulib-argnames classnames)
  (ulib-argtypes anylist)
  (ulib-arghelp "A list of classes given by paths")
  (ulib-mainfns unixlib-pintersect)
  (mhelp "Print the objects that are classified in all the specified classes.

See Also: pintersect*"))

(defunixlibrary pintersect*
  (ulib-argnames classnames)
  (ulib-argtypes anylist)
  (ulib-arghelp "A list of classes given by paths")
  (ulib-mainfns unixlib-pintersect*)
  (mhelp "Finds and prints the name of all the objects which, for each
specified class, are classified in the class or a 'subclass'.

If CLASS-DIRECTION is set to DOWN, 'subclass' means a descendant class.

If CLASS-DIRECTION is set to UP, 'subclass' means a ancestor class.

See Also: pintersect"))

(defun unixlib-pintersect (classnames)
  (let ((classes nil))
    (dolist (p classnames)
	    (let ((cl (locate-unixlibclass p)))
	      (when (libclass-p cl)
		(push cl classes))))
    (when classes
      (let ((items (libclass-libitems (car classes))))
	(dolist (cl (cdr classes))
		(setq items
		      (intersection items (libclass-libitems cl))))
	(if items
	    (progn
	      (msgf "Items Classified under all of " classnames ":" t)
	      (print-libclass-libitems items))
	  (msgf "There are no items classified under all of " classnames "." t))))
    nil))

(defun unixlib-pintersect* (classnames)
  (let ((classes nil))
    (dolist (p classnames)
	    (let ((cl (locate-unixlibclass p)))
	      (when (libclass-p cl)
		(push cl classes))))
    (when classes
      (let ((items (libclass-libitems* (car classes))))
	(dolist (cl (cdr classes))
		(setq items
		      (intersection items (libclass-libitems* cl))))
	(if items
	    (progn
	      (msgf "Items Classified under all of " classnames ":" t)
	      (print-libclass-libitems items))
	  (msgf "There are no items classified under all of " classnames "." t))))
    nil))

(defunixlibrary generate-class-scheme
  (ulib-argnames name help)
  (ulib-argtypes symbol string)
  (ulib-arghelp "Name" "Help Message")
  (ulib-mainfns generate-class-scheme)
  (mhelp "Generate a classification scheme for all abbreviations,
constants, and gwffs.  TPS does some of the work, and prompts the
user to interactively make other choices.

NOTE:  It is best to run this with a fresh core image.  Otherwise,
TPS may confuse items previously fetched from the library with
objects defined in the core TPS image.

See Also:  FIND-GENERATED-CLASS, MV, RENAME-CLASS, IMPORT-CLASS"))

(defunixlibrary import-class
  (ulib-argnames name)
  (ulib-argtypes anything)
  (ulib-arghelp "Name of a Class")
  (ulib-mainfns unixlib-import-class)
  (mhelp "Move the items and subclasses of a class to its parent
and delete the class."))

(defunixlibrary find-generated-class
  (ulib-mainfns find-generated-class)
  (mhelp "Find a class with an automatically generated name, i.e.,
one with prefix 'GEN.' and cd to its parent.  This command is
important when GENERATE-CLASS-SCHEME has been used to automatically
generate a class-scheme and one wants to rename and manipulate
these classes.

See Also:  GENERATE-CLASS-SCHEME, CD, MV, RENAME-CLASS, IMPORT-CLASS"))

(defunixlibrary ls-items*
  (ulib-mainfns unixlib-ls-items*)
  (mhelp "List all accessable Library Items accessable from the current class.

See Also: UNIXLIB, CLASS-DIRECTION, LS"))

(defun unixlib-ls-items* ()
  (let ((items (libclass-libitems* *current-class*)))
    (print-unordered-symbol-table items)))

(defun find-generated-class ()
  (unless (libclass-p *current-class*)
    (setq *current-class* (get class-scheme 'libclass)))
  (let ((gen-cl (find-generated-class-rec
		 (list *current-class* (get class-scheme 'libclass)))))
    (if gen-cl    
	(progn
	  (msgf "Generated Class: " (libclass-name gen-cl))
	  (if (libclass-parents gen-cl)
	      (setq *current-class* (car (libclass-parents gen-cl)))
	    (setq *current-class* gen-cl)))
      (throwfail "There are no remaining classes with automatically generated names"))))

(defun find-generated-class-rec (cll &optional already)
  (if cll
      (if (member (car cll) already)
	  (find-generated-class-rec (cdr cll) already)
	(let ((name (format nil "~d" (libclass-name (car cll)))))
	  (if (and (> (length name) 3)
		   (string-equal name "GEN." :start1 0 :end1 4))
	      (car cll)
	    (find-generated-class-rec 
	     (append (libclass-kids (car cll)) (cdr cll))
	     (cons (car cll) already)))))
    nil))

(defun unixlib-rename-class (name newname)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (let ((a (member name (libclass-kids *current-class*) :key #'libclass-name)))
    (if a
	(setf (libclass-name (car a)) newname)
      (throwfail "Could not find " name " in current class."))))

(defun unixlib-copy-class-scheme (name newname help)
  (let ((cl (get name 'libclass))
	(fake (gensym)))
    (unless (libclass-p cl)
      (let ((y (car (remove-if-not #'(lambda (x) (eq (car x) 'libclass))
				   (gethash name *lib-masterindex*)))))
	(when (and y (query (format nil "Retrieve ~d from the library?" name) t))
	  (core::retrieve-libobject name :type 'libclass :multiple t)
	  (setq cl (get name 'libclass)))))
    (unless (libclass-p cl)
      (throwfail "Unknown Classification Scheme " name))
    (setf (get fake 'libclass) (copy-libclass-rec cl))
    (eval (list 'def-class-scheme newname
		(list 'class-direction (get name 'class-direction))
		(list 'libclass fake)
		(list 'mhelp help)))))

(defun copy-libclass-rec (cl)
  (let ((cassoc nil)
	(cll (list cl)))
    (loop while cll do
	  (let* ((cl1 (car cll))
		 (clp (assoc cl1 cassoc)))
	    (if clp
		(pop cll)
	      (let ((cl2 (copy-libclass cl1)))
		(push (cons cl1 cl2) cassoc)
		(setq cll (append (libclass-kids cl1) (cdr cll)))))))
    (dolist (clp cassoc)
      (let ((cl1 (car clp))
	    (cl2 (cdr clp)))
	(setf (libclass-kids cl2)
	      (mapcar #'(lambda (x) (cdr (assoc x cassoc)))
		      (libclass-kids cl1)))
	(setf (libclass-parents cl2)
	      (mapcar #'(lambda (x) (cdr (assoc x cassoc)))
		      (libclass-parents cl1)))))
    (cdr (assoc cl cassoc))))

(defun unixlib-import-class (name)
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (let ((a0 (member name (libclass-kids *current-class*) :key #'libclass-name)))
    (when a0
      (let ((a (car a0)))
	(setq *unix-libclass-scheme-changed* t)
	(setf (libclass-kids *current-class*)
	      (remove a (libclass-kids *current-class*)))
	(setf (libclass-parents a)
	      (remove *current-class* (libclass-parents a)))
	(setf (libclass-libitems *current-class*)
	      (union (libclass-libitems a) (libclass-libitems *current-class*)))
	(dolist (k (libclass-kids a))
	  (setf (libclass-kids *current-class*)
		(adjoin k (libclass-kids *current-class*)))
	  (setf (libclass-parents k)
		(adjoin *current-class*
			(remove a (libclass-parents k)))))))))

(defun unixlib-pup ()
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (unixlib-pup-rec (list *current-class*)))

(defun unixlib-pup-rec (cll &optional already)
  (if cll
      (let ((cl1 (car cll)))
	(if (member cl1 already)
	    (unixlib-pup-rec (cdr cll) already)
	  (let ((pars (libclass-parents cl1)))
	    (msgf (class-fullpath cl1) t)
	    (when (libclass-libitems cl1)
	      (print-unordered-symbol-table
	       (libclass-libitems cl1)))
	    (unixlib-pup-rec (append pars (cdr cll))
			     (cons cl1 already)))))
    nil))

(defun unixlib-pdown ()
  (unless (libclass-p *current-class*)
    (throwfail "No Current Class?"))
  (unixlib-pdown-rec (list *current-class*)))

(defun unixlib-pdown-rec (cll &optional already)
  (if cll
      (let ((cl1 (car cll)))
	(if (member cl1 already)
	    (unixlib-pdown-rec (cdr cll) already)
	  (let ((kids (libclass-kids cl1)))
	    (msgf (class-fullpath cl1) t)
	    (when (libclass-libitems cl1)
	      (print-unordered-symbol-table
	       (libclass-libitems cl1)))
	    (dolist (k kids)
	      (unless (eq (car (libclass-parents k)) cl1)
		(msgf (format nil "~d/~d" 
			      (class-fullpath cl1)
			      (libclass-name k))
		      " -> " (class-fullpath k) t)))
	    (unixlib-pdown-rec (append kids (cdr cll))
			       (cons cl1 already)))))
    nil))

(defun unixlib-locate (name)
  (let* ((cl (get CLASS-SCHEME 'libclass))
	 (namecll (unixlib-locate-rec name (list cl)))
	 (already nil))
    (if namecll
	(progn
	  (if (cdr namecll)
	      (msgf "Found " (length namecll) " Occurrences:" t)
	    (msgf "Single Occurrence:" t))
	  (dolist (namecl namecll)
	    (when (string-equal name (libclass-name namecl))
	      (msgf (class-fullpath namecl) "/" t))
	    (when (member name (libclass-libitems namecl))
	      (msgf (class-fullpath namecl) "/" name))
	    (do ((anc namecl (car (libclass-parents anc))))
		((null anc))
	      (dolist (p (cdr (libclass-parents anc)))
		(unless (member p already)
		  (msgf (class-fullpath p) "/" (libclass-name anc) 
			" -> " (class-fullpath anc) t)
		  (push p already))))))
      (msgf "Could Not Locate " name t))))

(defun unixlib-locate-rec (name cll &optional already)
  (if cll
      (let ((cl1 (car cll)))
	(if (member cl1 already)
	    (unixlib-locate-rec name (cdr cll) already)
	  (let ((r (unixlib-locate-rec name (append (libclass-kids cl1) (cdr cll))
				       (cons cl1 already))))
	    (if (or (string-equal name (libclass-name cl1)) (member name (libclass-libitems cl1)))
		(cons cl1 r)
	      r))))
    nil))

