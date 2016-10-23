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

(deffile library2
  (part-of library)
  (extension lisp)
  (mhelp "Defines top-level for library."))

;;; ************ context : reading *****************

(context lib-reading)

(deflibrary restore-masterindex
  (lib-mainfns  restore-lib-hashtable)
  (mhelp "Restore library master index. Normally this need not be done by the
user as it is done automatically when TPS is first entered. However, if
the contents of the library may have been changed from outside of TPS
(e.g. by a text editor) since TPS was started, then this command will
re-initialize the library index."))

(deflibrary destroy
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Name of object")
  (lib-defaultfns (lambda (name) (list name)))
  (lib-mainfns destroy-libobject)
  (mhelp "Remove a library object from TPS (the object will remain
stored in the library)."))

(defun destroy-libobject (name)
  (declare (special global-theory-list global-rewrite-rule-list))
  (unless (object-present name nil)
	  (throwfail "This object is not currently loaded in TPS."))
  (dolist (prop (plist name))
	  (remprop name prop))
  (setq global-abbrevlist (delete name global-abbrevlist))
  (when (boundp 'global-theory-list) (setq global-theory-list (delete name global-theory-list)))
  (setq global-modelist (delete name global-modelist))
  (when (boundp 'global-rewrite-rule-list) (setq global-rewrite-rule-list (delete name global-rewrite-rule-list)))
  (setq global-logconstlist (delete name global-logconstlist)) 
  (setq global-pmpropsymlist (delete name global-pmpropsymlist))
  (when (find-package 'auto)
	(setq auto::*global-searchlist* (delete name auto::*global-searchlist*)))
  (msgf "Destroyed " name t))


(deflibrary fetch
  (lib-argnames name type)
  (lib-argtypes symbol lib-argtype-or-nil)
  (lib-arghelp "Name of object" "argtype or nil")
  (lib-defaultfns (lambda (name type)
		    (list name (if (eq type '$) nil type))))
  (lib-mainfns retrieve-libobject-real)
  (mhelp "Make a library object available in TPS.
Will create a new TPS object if EXPERTFLAG is set to T, otherwise
will create a weak label for the new library object.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(defun retrieve-libobject-real (name type)
  (if (object-present name type)
      (when (query "An object with this name has already been loaded. Reload it?" nil)
	    (when (retrieve-libobject name :type type :multiple t)
		  (msgf name t)))
; cebrown: changed :multiple t to :multiple show-all-libobjects
; so that if show-all-libobjects is NIL, then fetch retrieves first found
; instead of failing.  7/26/99
    (when (retrieve-libobject name :type type :multiple show-all-libobjects) 
	  (msgf name t))))

(defun retrieve-libobject (name &key (type nil) (multiple nil) (preferred-dir nil) (writeable nil) (fail-gently nil))
  (let ((item (retrieve-item name :type type :multiple multiple :preferred-dir preferred-dir :writeable writeable
			     :fail-gently fail-gently)))
    (when item 
	  (funcall (make-lib-tpsobject (or type (libitem-type item))) item)
	  (when expertflag ; cebrown 4/2/02 - trying to fix a problem with expertflag nil
	    (remprop name 'flavor))
;the line above was inserted because we were having problems with weak labels that
;were also abbreviations (a lot of wffops would just pass them to apply-label, which would
;barf, rather than expanding them). So now if you load a library object, it automatically kills
;off the weak label.
	  T)))

(deflibrary retrieve-file
  (lib-argnames file)
  (lib-argtypes filespec)
  (lib-arghelp "File to retrieve")
  (mhelp "Make all objects in a library file available in TPS. Objects in a
file are retrieved in the same order as they are stored in the file.

If more than one file of the given name is found in the library directories
in DEFAULT-LIB-DIR and BACKUP-LIB-DIR, the user is prompted to choose one."))

(defun retrieve-file (fname)
  (let ((filename (complete-lib-filename-choose fname))
	(load-warn-p nil))
    (when filename
	  (let ((copy-file (new-tmp-filename))
		(input-file (pathname-name filename)))
	    (copy-file filename copy-file)
	    (with-open-file (*input-stream* copy-file :direction :input)
			    (msg t "Retrieving: ")
			    (do ((item (read *input-stream* nil control-d-char)
				       (read *input-stream* nil control-d-char)))
				((eq item control-d-char))
				(let ((name (cadr (memq :name item)))
				      (type (cadr (memq :type item))))
				  (let ((libitem (parse-libitem item input-file)))
				    (msg t name 5 type)
				    (define-permanent-objects item)
				    (funcall (make-lib-tpsobject type) libitem)))))
	    (delete-file copy-file)))))


(deflibrary find-provable
  (lib-argnames backup)
  (lib-argtypes yesno)
  (lib-arghelp "Search backup directories?")
  (lib-defaultfns (lambda (backup) (list (if (eq backup '$) nil backup))))
  (mhelp "Look for gwffs with a certain provability status."))

(defun find-provable (backup)
  (let ((pr (get-provability-list nil)))
    (when pr
	  (lib-search 'gwff pr backup))))

;;; ************ context : writing *****************

(context lib-writing)

(deflibrary change-provability
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Gwff to be changed")
  (mhelp "Change the PROVABILITY attribute of a stored gwff.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(defun change-provability (name)
  (let* ((found (locate-item name :type 'gwff :writeable t))
	 (count 0) provability proof-date)
    (unless found (throwfail "No such gwff exists in your library." t))
    (if (cdr found) ; cebrown: changed so that user is not prompted if only one file is found
	(progn
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-msg))
	  (complain (length found) " items called " name " found. They are as follows:" t)
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	  (dolist (elt found)
		  (incf count) 
		  (complain count ") MODIFY " (car elt) " in file " (pfile (namestring (cdr elt)))))
	  (incf count) 
	  (complain count ") DO NOTHING.")
	  (setq count (1- (get-a-number count)))
	  (setq found (nth count found)))
      (setq found (car found)))
    (when found
	  (let ((old-value (retrieve-item name :type 'gwff :writeable t :multiple nil 
					 :preferred-dir (cdr found))))
	    (when (libitem-provability old-value)
		  (if (libitem-proof-date old-value)
		      (msgf "Since " (libitem-proof-date old-value) ", " name " has had provability:" t)
		    (msgf name " has provability:" t))
		  (msgf "\"" (libitem-provability old-value) "\"." t))
	    (msgf t "Choose a new value from the following list:" t)
	    (setq provability (get-provability (libitem-provability old-value)))
	    (if (not (string= (princ-to-string provability)
			      (princ-to-string (libitem-provability old-value))))
		(setq proof-date (stringdt nil))
	      (setq proof-date (libitem-proof-date old-value)))
	    (let ((item (make-libitem
			 :name name :type 'gwff :description (libitem-description old-value)
			 :mhelp (libitem-mhelp old-value) :context (libitem-context old-value)
			 :provability provability
			 :keywords (libitem-keywords old-value)
			 :proof-date proof-date
			 :other-attributes (libitem-other-attributes old-value)
			 :needed-objects (libitem-needed-objects old-value)
			 :other-remarks (libitem-other-remarks old-value)
			 :file (namestring (cdr found)))))
	      (setf (libitem-keywords item) (get-keywords-for item (libitem-keywords item)))
	      (update-libitem item))))))

(defun get-provability (prov)
  (let* ((prov (1+ (do ((count 0 (1+ count)))
		       ((or (= count (1- (length provability-list)))
			    (string= (princ-to-string prov) (princ-to-string (elt provability-list count)))) count))))
	 (count 0))
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
    (dolist (pr provability-list)
	    (incf count)
	    (if pr (complain count ") " pr)
	      (complain count ") NIL (nothing recorded)")))
    (setq count (1- (get-a-number count prov)))
    (nth count provability-list)))

(defun get-provability-list (prov)
  (let* ((prov (1+ (do ((count 0 (1+ count)))
		       ((or (= count (1- (length provability-list)))
			    (string= (princ-to-string prov) (princ-to-string (elt provability-list count)))) count))))
	 (count 0))
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
    (dolist (pr provability-list)
	    (incf count)
	    (if pr (complain count ") " pr)
	      (complain count ") NIL (nothing recorded)")))
    (setq count (mapcar #'(lambda (x) (1- x)) (get-a-number-list count (list prov))))
    (mapcar #'(lambda (x) (or (nth x provability-list) "provability NIL")) count)))
  
(deflibrary insert
  (lib-argnames name type)
  (lib-argtypes symbol lib-argtype)
  (lib-arghelp "Name of object to be created" "Type of object")
  (lib-defaultfns (lambda (name type) (list name type)))
  (lib-mainfns insert-libobject)
  (mhelp "Insert an item in the library. 
The INSERT command can be used to create a new library object or
to modify existing entries in the library.  If SHOW-ALL-LIBOBJECTS
is set to T, the user is prompted to indicate which existing
library object to modify or which library directory into which
the new object should be inserted. If AUTO-KEYWORDS is set to T, 
executing INSERT-LIBOBJECT requires expanding all definitions, 
which can take an enormous amount of time when definitions are 
deeply nested.

All the items will be replaced by whatever you write
(or kept the same if you use the default) except for \"additional
remarks\"; what you specify here will be added to whatever is already
there. If you don't want to add additional remarks, respond with
<space><return>. Use your favorite editor to make any changes within
the existing comment."))


(defun insert-libobject (name type &optional (comment nil))
; the optional arguments are used only by test-insert, in test-top-lib.lisp
  (msgf "Checking for existing objects..." t)
  (let* ((found (locate-item name :type type :writeable t
			     :multiple t)) ; ignore show-all-libobjects -- cebrown 7/26/99
	 (otherfound (unless found
		       (locate-item name :type type :writeable nil
				    :multiple t))) ; ignore show-all-libobjects -- cebrown 7/26/99
	 (count 0)
	 (allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
	 directory needed-objects modify old-value help file provability proof-date keywords)
    (when (null allowable-dirlist) (throwfail "You need to set DEFAULT-LIB-DIR"))
    (when (cdr (remove-duplicates (mapcar #'car 
					  (remove-if
					   #'(lambda (x) (and type (eq (car x) type)))
					   (gethash name *lib-masterindex*)))))
      (msgf "WARNING:  There are conflicting library objects with name " name))
    (when otherfound
      (if (cdr otherfound)
	  (complain "There are " (length otherfound) " items " name " in the backup library." t)
	(complain "There is an item " name " in the backup library." t))
      (msg "It is strongly recommended that you copy ")
      (if (cdr otherfound)
	  (msg "one of these")
	(msg "it"))
      (msg " into your library directory," t)
      (msg "then modify associated information as you insert this item.")
      (when (query "Copy A Libobject?" t)
	(let ((fname (file-namestring (cdar otherfound))))
	  (prompt-read fname nil (msg "Please input a filename for this record.") 'filespec fname
		       ((? (mhelp 'filespec)) (?? (mhelp 'filespec))))
	  (copy-libobject name type fname t)
	  (setq found (locate-item name :type type :writeable t
				   :multiple t)))))
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-msg))
    (when found
      (complain (length found) " writeable items called " name " found. They are as follows:" t)
      (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
      (dolist (elt found)
	(incf count) 
	(complain count ") MODIFY " (car elt) " in file " (pfile (namestring (cdr elt))))
	(setq allowable-dirlist (delete (directory-namestring (cdr elt)) allowable-dirlist :test 'string=)))
      (unless show-all-libobjects
	(throwfail t "Please either specify the type of the item or set SHOW-ALL-LIBOBJECTS to T."))) ; cebrown: changed NIL to T.
    (unless found
      (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options)))
    (when allowable-dirlist
      (dolist (elt allowable-dirlist)
	(incf count)
	(complain count ") INSERT into directory " (pfile elt))))
    (incf count) 
    (complain count ") DO NOTHING.")
    (setq count (1- (get-a-number count)))
    (if (nth count found)
	(setq found (nth count found))
      (let ((found-l (length found))) ; cebrown 7/22/99
	(setq found nil directory (nth (- count found-l) allowable-dirlist))))
    (unless (and (null found) (null directory))
      (when found 
	(msgf "The INSERT command can be used to modify existing entries in" t
	      "the library. All the items will be replaced by whatever you write" t
	      "(or kept the same if you use the default) except for \"additional" t
	      "remarks\"; what you specify here will be added to whatever is already" t
	      "there. If you don't want to add additional remarks, respond with" t
	      "<space><return>." t)
	(setq modify t)
	(setq old-value (retrieve-item name :type type :writeable t :multiple nil 
				       :preferred-dir (cdr found))))
      (prompt-read help nil (msgf "HELP (STRING): Help for object") 'string
		   (if old-value (libitem-mhelp old-value) 
		     (if (and (fboundp 'rewrule-p) (rewrule-p name)) 
			 (or (cdr (assoc 'rewrite-rule (get name 'mhelp))) "")
		       (if (and (fboundp 'theory-p) (theory-p name))
			   (or (cdr (assoc 'theory (get name 'mhelp))) "")
			 (if (eq type 'class-scheme)
			     (or (cdr (assoc 'class-scheme (get name 'mhelp))) "")
			   ""))))
		   ((? (mhelp 'string)) (?? (mhelp 'string))))
      (prompt-read needed-objects nil 
		   (if (or (eq type 'theory)
			   (and old-value (eq (libitem-type old-value) 'theory)))
		       (msgf "OBJECTS (LIST): Names of the subtheories, axioms, rewrite rules," t 
			     "   and other defns in this theory")
		     (msgf "NEEDED OBJECTS (LIST): Other library objects"))
		   'symbollist (if old-value 
				   (libitem-needed-objects old-value) 
				 (if (and (eq type 'theory) (fboundp 'theory-p) (theory-p name))
				     (reduce 'append 
					     (append (get name 'extends) (get name 'gwffs) (get name 'rrules)
						     (get name 'other-stuff)))
				   (if (and (fboundp name) 
					    (or (and (get 'exercise 'testfn) 
						     (funcall (get 'exercise 'testfn) name)
						     (get name 'assertion))
						(gwff-q name))) ; avoiding asking to search for it in the lib
				       (or (new-defs name) nil) nil)))
		   ((? (msgf " List of objects needed to define the current object."))
		    (?? (mhelp 'symbollist))))
      (let ((load-warn-p nil))
	(dolist (needed needed-objects) 
	  (retrieve-libobject needed :multiple nil
			      :preferred-dir (and found (directory-namestring (cdr found))))))
      (let ((filename (or (if (and old-value (libitem-file old-value))
			      (file-namestring (libitem-file old-value))
			    (concatenate 'string (string-downcase (symbol-name type)) 
					 "." default-libfile-type)))))
	(prompt-read file nil (msgf "FILE (FILESPEC): File to be saved in ")
		     'string filename ((? (mhelp 'string)) (?? (mhelp 'string))))
	(setq file (namestring (merge-pathnames (pathname-name file)
						(concatenate 'string 
							     (or (and directory (namestring directory))
								 (and found (directory-namestring (cdr found))))
							     (pathname-name filename) "."
							     default-libfile-type)))))
      (unless comment (prompt-read comment nil (msgf "COMMENTS (STRING): Additional Remarks")
				   'string (if old-value (libitem-other-remarks old-value) "")
				   ((? (when old-value (msg "Comment will be appended to the existing comment." t))
				       (mhelp 'string))
				    (?? (mhelp 'string)))))
      (multiple-value-bind (description context other-attributes more-comments) ; only modes return more-comments
	  (funcall (get type 'lib-promptfn)
		   name type help file modify comment old-value)
	(unless (eq description control-d-char)
	  (when old-value (setq keywords (libitem-keywords old-value)))
	  (let ((item (make-libitem
		       :name name :type type :description description
		       :mhelp help :context context
		       :keywords keywords
		       :provability provability
		       :proof-date proof-date
		       :other-attributes other-attributes
		       :needed-objects needed-objects
		       :other-remarks 
		       (if (and old-value
				(not (string= (libitem-other-remarks old-value)
					      "")))
			   (concatenate 'string
					(libitem-other-remarks old-value)
					(string #\newline) comment (string #\newline) 
					more-comments)
			 (concatenate 'string comment (string #\newline) more-comments))
		       :file (namestring (pathname file)))))
	    (if auto-keywords
		(setf (libitem-keywords item) (input-keywords item))
	      ())
	    (if modify 
		(update-libitem item)
		(store-item item))))))))

(defflag auto-keywords
  (flagtype boolean)
  (default nil)
  (subjects library)
  (mhelp "If T, keywords will automatically be generated and attached to the library object.
However, setting auto-keywords to T requires expanding all definitions, 
which can take an enormous amount of time when definitions are deeply nested."))  

(defun insert-libobject-auto (name type content filename needed-objects &optional (comment nil) (help ""))
  (let* ((file (namestring (merge-pathnames (pathname-name filename)
					    (concatenate 'string
							 (namestring  auto-lib-dir)
							 (pathname-name "foo")
							 "." default-libfile-type))))
	 description
	 other-attributes
	 keywords
	 )
    (let ((load-warn-p nil))
      (dolist (needed needed-objects) 
	(retrieve-libobject needed :multiple nil
			    :preferred-dir (directory-namestring file))))
    (setq description (gettype 'gwff content))
    (setq other-attributes (with-open-stream (string-stream (make-string-output-stream))
			     (type-to-lib-stream
			      (type description) string-stream)
			     (list (list 'type (get-output-stream-string
						string-stream)))))
    (when (eq type 'abbr)
      (setq description
	    (with-output-to-string (*standard-output*)
	      (in-mode re-read (let ((style 'generic))
				 (msg (description . gwff)))))))
    (let ((item (make-libitem
		:name name :type type :description description
		:mhelp help :other-attributes other-attributes
		:keywords keywords
		:needed-objects needed-objects
		:other-remarks comment
		:file (namestring (pathname file)))))
      (if auto-keywords
	  (setf (libitem-keywords item) (input-keywords-2 item))
	())
      (store-item item))))

(defun const-insert (name type) 
  (push (cons (concatenate 'string " " name " ")
	      (string-upcase (concatenate 'string " " name "-" *insert-suffix*
					  "(" type ") ")))
	*type-list*))

(defun def-insert (name content)
  (let ((new-name (string-upcase (concatenate 'string " " name "-" *insert-suffix* " ")))
	(symbol-name (intern (string-upcase (concatenate 'string name "-" *insert-suffix*))))
	(new-content (concatenate 'string " " (remove #\Linefeed content) " ")))
    (insert-libobject-auto symbol-name
			   'abbr
			   (string-upcase (replace-all-in-list (replace-all-in-list new-content *def-list*)
							       *type-list*))
			   *destination*
			   (needed-objects new-content *def-list*))
    (push (cons (concatenate 'string " " name " ") new-name) *def-list*)))

(defun axiom-insert (content)
  (if (equal "" *needed-axioms*)
      (setf *needed-axioms* (concatenate 'string " [ " (remove #\Linefeed content) " ] "))
      (setf *needed-axioms* (concatenate 'string " [[" *needed-axioms* "] AND [" 
					 (remove #\Linefeed content) "]] " ))))

(defun thm-insert (name content)
  (let ((new-name (concatenate 'string name "-" *insert-suffix*))
	(symbol-name (intern (string-upcase (concatenate 'string name "-" *insert-suffix*))))
	(new-content (if (equal *needed-axioms* "")
			 (concatenate 'string " " (remove #\Linefeed  content) " ")
			 (concatenate 'string " " *needed-axioms* " IMPLIES [ " (remove #\Linefeed content) "] "))))
    (format t "Theorem ~A inserted as ~A.~%" *thm-name* new-name)
    (insert-libobject-auto symbol-name
			   'gwff
			   (string-upcase (replace-all-in-list (replace-all-in-list new-content *def-list*)
							       *type-list*))
			   *destination*
			   (needed-objects new-content *def-list*)
			   nil ;(concatenate 'string "Theorem " *thm-name* " from TPTP Library.")
			   (get-help-insert *source-fname*))))

(defun get-help-insert (fname)
  (let ((comment nil)
	(flag nil))
    (with-open-file (str fname :direction :input)
      (do ((line (read-line str nil 'eof)
		 (read-line str nil 'eof)))
	  ((not (or (equal line "") (eql (char line 0) #\;))))
	(if (or (equal line "") (not (eql (char line 1) #\-)))
	    (setf comment (concatenate 'string 
				       comment
				       (if (equal line "")
					   line
					   (subseq line 2))
				       "
"))
	    ())))
    comment))


(deflibrary insert-tptp
  (lib-argnames source-file destination-file suffix)
  (lib-argtypes string string string)
  (lib-arghelp "Source file (.tps file)" "Destination file (in the library)" "Suffix to be added to the new abbreviations/theorems")
  (lib-defaultfns (lambda (source-fname destination-fname suffix) (list source-fname destination-fname suffix)))
  (lib-mainfns insert-libobject-tptp)
  (mhelp "Insert a TPTP Problem into the library. 
The INSERT-TPTP command can be used to create a new library file 
containing abbreviations and theorems from a TPTP formatted file 
(.tps file). The destination directory is set according to the flag
AUTO-LIB-DIR. It will not overwrite or delete any existing items: 
new items need to have different names, hence the suffix.
If AUTO-KEYWORDS is set to T, executing INSERT-TPTP requires 
expanding all definitions, which can take an enormous amount of time 
when definitions are deeply nested."))

(defun insert-libobject-tptp (source-fname destination-fname suffix)
  (progn
    (defparameter *destination* destination-fname)
    (defparameter *type-list* ())
    (defparameter *def-list* ())
    (defparameter *insert-suffix* suffix)
    (defparameter *thm-name* (pathname-name source-fname))
    (defparameter *needed-axioms* "")
    (defparameter *source-fname* source-fname)
    (load (pathname source-fname))))

(deflibrary insert-tptp*
  (lib-argnames source-directory)
  (lib-argtypes string)
  (lib-arghelp "Source directory")
  (lib-defaultfns (lambda (source-directory) (list source-directory)))
  (lib-mainfns insert-libobject-tptp*)
  (mhelp "For each TPTP Problem in the source directory, insert 
a new file into the library. The INSERT-TPTP* command can be used 
to create new library files containing abbreviations and theorems 
from a directory of TPTP formatted files (.tps file). The destination
directory is set accordig to the flag AUTO-LIB-DIR. It will not 
overwrite or delete any existing items: new items need to have 
different names.
If AUTO-KEYWORDS is set to T, executing INSERT-TPTP* requires 
expanding all definitions, which can take an enormous amount of time 
when definitions are deeply nested."))

(defun insert-libobject-tptp* (source-dir)
  (let ((paths (directory (concatenate 'string source-dir "/**/*.tps"))))
    (dolist (source-fname paths)
      (let* ((suffix (remove #\^ (pathname-name source-fname)))
	     (destination-fname (make-pathname :name suffix
					       :type "lib")))
	(insert-libobject-tptp source-fname destination-fname suffix)))))

(defun needed-objects (content items)
  (needed-objects-aux content items ()))
    

(defun needed-objects-aux (content items acc)
  (if items
      (if (search (caar items) content)
	  (needed-objects-aux content (cdr items) (cons (intern (string-upcase (string-trim " " (cdar items)))) 
							acc))
	  (needed-objects-aux content (cdr items) acc))
      acc))

(defun replace-all-in-list (string replacements)
  (if replacements
      (replace-all-in-list (replace-all string (caar replacements) (cdar replacements))
			   (cdr replacements))
      string))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
	 do (write-string string out
			  :start old-pos
			  :end (or pos (length string)))
	 when pos do (write-string replacement out)
       while pos)))

(defflag auto-lib-dir
  (flagtype dirspec)
  (default "")
  (subjects library)
  (pre-change-fn (lambda (flag value old-value)
		   (declare (ignore flag))
		   (unless (equal value old-value)
		     (when (and *running-remotely* (not *expert-running-remotely*))
		       (throwfail "Sorry.  You are not allowed to change this flag when running remotely.")))))
  (change-fn (lambda (flag value old-value)
	       (declare (ignore flag))
	       (unless (equal value old-value)
		 (restore-lib-hashtable))))
  (mhelp "A writeable directory containing 
library files, used for automatic library insertion.
See the LIBRARY command INSERT-TPTP and INSERT-TPTP*"))  

(deflibrary delete
  (lib-argnames names type)
  (lib-argtypes symbollist lib-argtype-or-nil)
  (lib-arghelp "Name(s) of object(s)" "Type of object(s)")
  (lib-defaultfns (lambda (names type)
		    (list names (if (eq type '$) nil type))))
  (lib-mainfns delete-libobject-list)
  (mhelp "Delete an object from the library.

If more than one library object of this name is stored in
the library, the user is prompted to disambiguate."))

(defun delete-libobject-list (names type)
  (dolist (name names)
	  (delete-libobject name type)))

(deflibrary move-libobject
  (lib-argnames name type filename)
  (lib-argtypes symbollist lib-argtype-or-nil filespec)
  (lib-arghelp "List of names of objects" "argtype or nil" "File to move object(s) into")
  (lib-defaultfns (lambda (name type filename)
		    (list name (if (eq type '$) nil type) filename)))
  (lib-mainfns move-libobject)
  (mhelp "Move an object from one library file to another.
This command will also move a list of objects (either all of the same type, or
all of type NIL), into a single named file."))

(defun move-libobject (name type fname)
  (dolist (elt name) (movlib elt type fname)))

(defun movlib (name type fname)
  (let* ((load-warn-p nil)
	 (libitem (retrieve-item name :type type :writeable t))
	 (found (locate-item name :type type :writeable t :multiple t))
	 (count 0)
	 (allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
	 (fname (concatenate 'string (namestring (pathname-name fname)) "." default-libfile-type)))
    (when libitem
    (when (null allowable-dirlist) (throwfail t "You need to set DEFAULT-LIB-DIR"))
    (dolist (elt found)
	    (setq allowable-dirlist (delete (directory-namestring (cdr elt)) allowable-dirlist :test 'string=)))
    (setq allowable-dirlist (cons (directory-namestring (libitem-file libitem)) allowable-dirlist))
    (setq allowable-dirlist 
	  (mapcar #'(lambda (x) (concatenate 'string x fname)) allowable-dirlist))
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
    (when allowable-dirlist
	  (dolist (elt allowable-dirlist)
		  (incf count)
		  (complain count ") INSERT into "
			    (if (probe-file elt)
				"old" "new") " file " (pfile elt))))
    (incf count) 
    (complain count ") DO NOTHING.")
    (setq count (1- (get-a-number count)))
    (setq fname (nth count allowable-dirlist))
    (when (and libitem fname)
	  (funcall (make-lib-tpsobject (or type (libitem-type libitem))) libitem)
	  (let ((item (make-libitem
			    :name (libitem-name libitem)
			    :type (libitem-type libitem)
			    :description (libitem-description libitem)
			    :provability (libitem-provability libitem)
			    :proof-date (libitem-proof-date libitem)
			    :mhelp (libitem-mhelp libitem)
			    :keywords (libitem-keywords libitem)
			    :context (libitem-context libitem)
			    :other-attributes (libitem-other-attributes libitem)
			    :needed-objects (libitem-needed-objects libitem)
			    :other-remarks (libitem-other-remarks libitem)
			    :file fname)))
	    (delete-libobject name (libitem-type libitem))
	    (store-item item))))))

; added omit-other-remarks -- cebrown 8/4/99
(deflibrary copy-libobject
  (lib-argnames name type filename omit-other-remarks)
  (lib-argtypes symbol lib-argtype-or-nil filespec yesno)
  (lib-arghelp "Name of object" "argtype or nil" "File to insert object into"
	       "Omit other-remarks property of object?")
  (lib-defaultfns (lambda (name type filename omit-other-remarks)
		    (list name (if (eq type '$) nil type) filename
			  (if (eq omit-other-remarks '$)
			      t
			    omit-other-remarks))))
  (lib-mainfns copy-libobject)
  (mhelp "Copy an object from some specified directory to the default directory.
Does not copy the library entries of needed objects.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate."))

(deflibrary rename-object
  (lib-argnames name type newname)
  (lib-argtypes symbol lib-argtype-or-nil symbol)
  (lib-arghelp "Name of object" "argtype or nil" "New name for object")
  (lib-defaultfns (lambda (name type newname)
		    (list name (if (eq type '$) nil type) newname)))
  (lib-mainfns rename-libobject)
  (mhelp "Change the name of a library object. Does not move the object or 
alter it in any other way."))

(defun rename-libobject (name type newname)
  (let ((item (retrieve-item name :type type :writeable t)))
    (when item
	  (delete-libobject name type t (directory-namestring (libitem-file item)))
	  (setf (libitem-name item) newname)
	  (store-item item))))

(defun copy-libobject (name type fname &optional 
			    (omit-other-remarks nil) ; cebrown 8/4/99
			    (full-olddir nil) (full-newname nil))
  (let* ((load-warn-p nil)
	 (libitem (retrieve-item name :type type :multiple (if full-olddir nil show-all-libobjects)
				 :preferred-dir full-olddir))
	 (found (locate-item name :type type :writeable t :multiple t))
	 (banned-dirlist (mapcar 'directory-namestring (mapcar 'cdr found)))
	 (count 0)
	 (allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
	 (allowable-dirlist (remove-if #'(lambda (x) (member x banned-dirlist :test 'string=)) allowable-dirlist))
	 (fname (or full-newname (concatenate 'string (namestring (pathname-name fname)) "." default-libfile-type))))
    (unless full-newname
	    (when (null allowable-dirlist) (throwfail t "All the directories in DEFAULT-LIB-DIR already contain such an object."))
	    (dolist (elt found)
		    (setq allowable-dirlist (delete (directory-namestring (cdr elt)) allowable-dirlist :test 'string=)))
	    (setq allowable-dirlist 
		  (mapcar #'(lambda (x) (concatenate 'string x fname)) allowable-dirlist))
	    (unless allowable-dirlist 
		    (throwfail t "All the directories in DEFAULT-LIB-DIR already contain such an object."))
	    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
	    (when allowable-dirlist
		  (dolist (elt allowable-dirlist)
			  (incf count)
			  (complain count ") INSERT into "
				    (if (probe-file elt)
					"old" "new") " file " (pfile elt))))
	    (incf count) 
	    (complain count ") DO NOTHING.")
	    (setq count (1- (get-a-number count)))
	    (setq fname (nth count allowable-dirlist)))
    (when (and libitem fname)
	  (if (member (directory-namestring fname) banned-dirlist :test 'string=)
	      (complain "Can't copy this; there is already an object " name (when type " of type ")
			(when type type)
			t "in directory " (directory-namestring full-newname) t)
	    (progn (funcall (make-lib-tpsobject (or type (libitem-type libitem))) libitem)
		   (let ((item (make-libitem
				:name (libitem-name libitem)
				:type (libitem-type libitem)
				:description (libitem-description libitem)
				:provability (libitem-provability libitem)
				:keywords (libitem-keywords libitem)
				:proof-date (libitem-proof-date libitem)
				:mhelp (libitem-mhelp libitem)
				:context (libitem-context libitem)
				:other-attributes (libitem-other-attributes libitem)
				:needed-objects (libitem-needed-objects libitem)
				:other-remarks (if omit-other-remarks  ; cebrown 8/2/99
						   "
"
						 (libitem-other-remarks libitem))
				:file fname)))
		     (store-item item)))))))

(defun movlib2 (name type full-olddir full-newname)
  (let* ((load-warn-p nil)
	 (libitem (retrieve-item name :type type :multiple nil :preferred-dir full-olddir)))
    (when libitem
	  (delete-item-from-masterindex name (cons type (pathname (libitem-file libitem))))
	  (delete-item-from-file name (pathname (libitem-file libitem)) type t)
	  (funcall (make-lib-tpsobject (or type (libitem-type libitem))) libitem)
	  (let ((item (make-libitem
		       :name (libitem-name libitem)
		       :type (libitem-type libitem)
		       :description (libitem-description libitem)
		       :provability (libitem-provability libitem)
		       :keywords (libitem-keywords libitem)
		       :proof-date (libitem-proof-date libitem)
		       :mhelp (libitem-mhelp libitem)
		       :context (libitem-context libitem)
		       :other-attributes (libitem-other-attributes libitem)
		       :needed-objects (libitem-needed-objects libitem)
		       :other-remarks (libitem-other-remarks libitem)
		       :file full-newname)))
	    (store-item item)))))

(deflibrary reformat
  (lib-argnames file)
  (lib-argtypes filespec)
  (lib-arghelp "Name of file")
  (lib-mainfns reformat-libfile)
  (mhelp "Reformat the specified file. Will attempt to load all the objects 
in a given file and then to rewrite that file in the standard library format.
This can be useful if you manually edit your library files a lot and they've 
started to look a little disorganized.
To reformat all files in your directories, use SPRING-CLEAN."))

(defun reformat-libfile (filename)
  (let* ((load-warn-p nil)
	 (filename (complete-lib-filename-choose filename default-libfile-type t))
	 (input-file (namestring filename))
	 (new-file (make-pathname% :directory (directory-namestring filename)
				   :name (pathname-name (new-filename))))
	 (copy-file (new-tmp-filename)))
    (copy-file filename copy-file)
    (with-open-file (outstream new-file :direction :output)
       (with-open-file (*input-stream* copy-file :direction :input)
	  (do ((item (read *input-stream* nil control-d-char)
		     (read *input-stream* nil control-d-char))
	       (first t nil))
	      ((eq item control-d-char))
	      (let ((name (cadr (memq :name item))))
		(msg (if first "" ", ") name)
		(write-libitem (parse-libitem item input-file)
			       'write-to-file outstream)))))
    (rename-file new-file filename)
    (delete-file copy-file)))

(deflibrary sort
  (lib-argnames file head)
  (lib-argtypes filespec symbollist)
  (lib-arghelp "Name of file" "Objects to move to beginning of file")
  (lib-defaultfns (lambda (file head) (list file (if (eq head '$) nil head))))
  (lib-mainfns sort-libfile)
  (mhelp "Sort the specified file into alphabetical order, except for the
given list of objects which are put at the head of the file (if they were 
originally in the file). This command reads in the entire file and then
rewrites it; it will incidentally also catch any parsing errors."))

(defun sort-libfile (filename head)
  (let* ((load-warn-p nil)
	 (filename (complete-lib-filename-choose filename default-libfile-type t))
	 (copy-file (new-tmp-filename))
	 (namelist nil)
	 (namelist-head nil))
    (copy-file filename copy-file)
    (with-open-file 
       (*input-stream* copy-file :direction :input)
       (do ((item (read *input-stream* nil control-d-char)
		  (read *input-stream* nil control-d-char)))
	   ((eq item control-d-char))
	   (setq namelist (cons (cons (cadr (memq :name item)) (cadr (memq :type item))) namelist))))
    (setq namelist-head (remove-if-not #'(lambda (x) (memq x head)) namelist :key #'car))
    (setq namelist (set-difference namelist namelist-head))
    (setq namelist (append namelist-head (sort namelist #'string-lessp :key #'car)))
    (let ((itemlist (mapcar #'(lambda (x) 
			       (retrieve-item (car x) :type (cdr x) :multiple nil 
					  :preferred-dir (directory-namestring filename)))
			   namelist))
	  (itemlist2 nil))
      (dolist (libitem (reverse itemlist))
	      (when libitem
		    (funcall (make-lib-tpsobject (libitem-type libitem)) libitem)
		    (push (make-libitem
			   :name (libitem-name libitem)
			   :type (libitem-type libitem)
			   :description (libitem-description libitem)
			   :provability (libitem-provability libitem)
			   :keywords (libitem-keywords libitem)
			   :proof-date (libitem-proof-date libitem)
			   :mhelp (libitem-mhelp libitem)
			   :context (libitem-context libitem)
			   :other-attributes (libitem-other-attributes libitem)
			   :needed-objects (libitem-needed-objects libitem)
			   :other-remarks (libitem-other-remarks libitem)
			   :file (namestring filename)) 
			  itemlist2)))
      (delete-file filename)
      (dolist (item itemlist) (delete-item-from-masterindex (libitem-name item) (cons (libitem-type item) filename)))
      (dolist (item itemlist2) (store-item item)))
    (delete-file copy-file)))

(defun daterec (name type comment &optional (stats-p t) (prompts t))
  (let ((found (locate-item name :type type :writeable t :multiple nil))
	(flags1 (with-output-to-string (*standard-output*)
		 (stringdt)))
	(answer nil)
        (flags2 (with-output-to-string (*standard-output*)
                 (auto::display-time-in-daterec)
		 (dolist (flag recordflags)
		   (prin1 (cons flag (eval flag)))
		   (terpri))
                 (terpri)
                 (when (memq auto::default-ms '(ms88 ms89))
                   (auto::show-mating-stats)))))
    (if found 
	(let ((old-value (retrieve-item name :type type :writeable t :multiple nil)))
	  (setf (libitem-other-remarks old-value)
		(concatenate 'string
			     (libitem-other-remarks old-value)
			     (string #\newline)
                             flags1
			     (unless (string= comment "") (string #\newline))
			     comment flags2))
	  (setf (libitem-file old-value) (namestring (libitem-file old-value)))
	  (when (and stats-p prompts)
		(msgf "Current provability is: " (libitem-provability old-value) t)
		(when (query "Change provability status?" nil)
		      (let ((newprov (get-provability (libitem-provability old-value)))
			    (newdate (stringdt nil)))
			(unless (string= (princ-to-string newprov) (princ-to-string (libitem-provability old-value)))
				(setf (libitem-provability old-value) newprov)
				(setf (libitem-keywords old-value) (get-keywords-for old-value (libitem-keywords old-value)))
				(setf (libitem-proof-date old-value) newdate))))
		(msgf "Current keywords are: " (libitem-keywords old-value) t)
		(when (query "Change keywords?" nil)
		      (let ((newkeys (input-keywords old-value)))
			(setf (libitem-keywords old-value) newkeys)))
		(when (query "Add a new mode to the list of best modes for this theorem?" nil)
		  (prompt-read answer nil
			       (msg "Mode name to enter? ") 'symbol *lastmode* nil)
		  (add-bestmode-2 name answer (output-today) (get-rough-time-estimate) "" nil
				  (query "Should this mode be used for testing automatic search?" t))))
	  (update-libitem old-value))
        (if (and stats-p DEFAULT-LIB-DIR)
	    (let ((found (locate-item name :type type :writeable nil :multiple nil)))
	      (when (remove-if #'(lambda (x) (eq (car x) type)) (gethash name *lib-masterindex*))
		(msgf "WARNING:  There are conflicting library objects with name " name))
	      (if found
		  (let ((old-value (retrieve-item name :type type :writeable nil :multiple nil)))
		    (when (libitem-p old-value)
		      (prompt-read answer nil
				   (msg "Copy a record of " name " into your library?") 'yesno 'yes
				   ((? (mhelp 'yesno)) (?? (mhelp 'yesno))))
		      (when answer
			(prompt-read answer nil (msg "Please input a filename for this record.") 'filespec
				     (file-namestring (cdar found))
				     ((? (mhelp 'filespec)) (?? (mhelp 'filespec))))
			(let* ((count 0)
			       (allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
			       directory)
			  (unless allowable-dirlist
			    (throwfail "You need to set DEFAULT-LIB-DIR"))
			  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
			  (dolist (elt allowable-dirlist)
			    (incf count)
			    (complain count ") INSERT into directory " (pfile elt)))
			  (incf count) 
			  (complain count ") DO NOTHING.")
			  (setq count (1- (get-a-number count)))
			  (setq directory (nth count allowable-dirlist))
			  (when directory
			    (msgf "Input a provability status for the new record:")
			    (let* ((newprov (get-provability nil))
				   (newdate (stringdt nil))
				   (newitem (copy-libitem old-value)))
			      (setf (libitem-other-remarks newitem)
				    (concatenate 'string flags1 
						 (unless (string= comment "") 
						   (string #\newline))
						 comment flags2))
			      (setf (libitem-file newitem)
				    (namestring (merge-pathnames (pathname-name answer)
								 (concatenate 'string 
									      (namestring directory)
									      (pathname-name answer) "."
									      default-libfile-type))))
			      (setf (libitem-provability newitem) newprov)
			      (setf (libitem-proof-date newitem) newdate)
			      (store-item newitem)))))))
		(progn (prompt-read answer nil 
				    (msg "Record for " name " does not exist in the library." t
					 "Would you like to create a new record for it?") 'yesno 'yes 
					 ((? (mhelp 'yesno)) (?? (mhelp 'yesno))))
		       (when answer
			 (prompt-read answer nil (msg "Please input a filename for this record.") 'filespec "thm.lib"
				      ((? (mhelp 'filespec)) (?? (mhelp 'filespec))))
			 (let* ((count 0)
				(allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
				directory)
			   (unless allowable-dirlist
			     (throwfail "You need to set DEFAULT-LIB-DIR"))
			   (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
			   (dolist (elt allowable-dirlist)
			     (incf count)
			     (complain count ") INSERT into directory " (pfile elt)))
			   (incf count) 
			   (complain count ") DO NOTHING.")
			   (setq count (1- (get-a-number count)))
			   (setq directory (nth count allowable-dirlist))
			   (when directory
			     (msgf "Input a provability status for the new record:")
			     (let* ((newprov (get-provability nil))
				    (newdate (stringdt nil))
				    (newitem 
				     (when auto::current-eproof				  
				       (make-libitem
					:name name :type type
					; cebrown - 5/2/02 changed the way the
					; description is computed since the
					; etree may have lemmas
					:description 
					(if (auto::eproof-lemmas auto::current-eproof)
					    (cddr
					     (auto::get-shallow 
					      (auto::eproof-etree auto::current-eproof)))
					  (auto::get-shallow 
					   (auto::eproof-etree auto::current-eproof)))
					:mhelp "" :context 'unclassified
					:provability newprov
					:keywords nil
					:proof-date newdate
					:other-attributes nil
					:needed-objects
					(remove-duplicates
					 (remove-if-not
					  #'(lambda (b)
					      (let ((a (gethash b *lib-masterindex*)))
						(and a (or (assoc 'gwff a) (assoc 'abbr a) (assoc 'lib-const a)))))
					  (mapcar #'(lambda (a) (or (get a 'stands-for) a))
						  (append
						   (lib-abbr-list (auto::get-shallow 
								   (auto::eproof-etree auto::current-eproof)))
						   (const-list-real (auto::get-shallow 
								     (auto::eproof-etree auto::current-eproof)))))))
					:other-remarks (concatenate 'string flags1 
								    (unless (string= comment "") 
								      (string #\newline))
								    comment flags2)
					:file nil))))
			       (when newitem
				 (setf (libitem-file newitem)
				       (namestring (merge-pathnames (pathname-name answer)
								    (concatenate 'string 
										 (namestring directory)
										 (pathname-name answer) "."
										 default-libfile-type))))
				 (setf (libitem-keywords newitem) (input-keywords newitem)))
			       (if newitem (store-item newitem)
				 (msg "Sorry; nothing can be recorded unless it has been involved" T
				      "with the auto part of the system.")))))))))
	  (msg "Record for " name " does not exist in the library.")))))

(defun get-rough-time-estimate ()
  (if (auto::whether-called 'auto::diy)
      (multiple-value-bind (vh vt) 
			   (auto::diffcount 'auto::diy)
			   (declare (ignore vt))
	(floor (- (/ (or (cadr vh) 0) internal-time-units-per-second)
		  (/ (or (caddr vh) 0) internal-time-units-per-second))))
    (if (auto::whether-called 'mating)
	(multiple-value-bind (vh vt) 
			     (auto::diffcount 'mating)
			     (declare (ignore vt))
	  (floor (- (/ (or (cadr vh) 0) internal-time-units-per-second)
		    (/ (or (caddr vh) 0) internal-time-units-per-second))))
      0)))

(deflibrary reindex
  (lib-argnames file reformat)
  (lib-argtypes filespec yesno)
  (lib-arghelp "Name of file" "Reformat?")
  (lib-defaultfns (lambda (x y) (list x (if (eq y '$) 'Y y))))
  (lib-mainfns reindex-libfile)
  (mhelp "Reindex and reformat the specified file --- i.e. reconstruct the 
entries in the library master index relating to the objects in a 
particular file (you should only need this if you've been manually editing 
the libindex.rec file and have accidentally lost some entries...), and then
attempt to load and rewrite the file.
To reindex all files in your directories, use SPRING-CLEAN.
If you get an error because of parsing problems, try again but answer
no to \"Reformat?\" (it is not possible to format a file without 
parsing it)."))

(defun reindex-libfile (fname parsing &optional (full-fname nil) (restore-after t))
  (let* ((filename (or full-fname (complete-lib-filename-choose fname default-libfile-type t)))
	 (load-warn-p nil)
	 (input-file (pathname-name fname))
	 (dir (directory-namestring filename))
	 (wrote-something nil)
	 (new-file (make-pathname% :directory (directory-namestring filename)
				   :name (pathname-name (new-filename))))
	 (copy-file (new-tmp-filename)))
    (copy-file filename copy-file)
    (with-open-file (outstream new-file :direction :output)
       (with-open-file (*input-stream* copy-file :direction :input)
	  (do ((item (read *input-stream* nil control-d-char)
		     (read *input-stream* nil control-d-char)))
	      ((eq item control-d-char))
	      (if (and (listp item) (memq :name item)) ;otherwise it's not a proper lib file
		  (let ((name (cadr (memq :name item)))
			(type (cadr (memq :type item))))
		    (unless (string= dir 
				     (cdr (locate-item name :type type :writeable t :multiple nil :preferred-dir dir)))
			    ;if there's already an item called this, then pass.
			    (let ((reroute-close-message nil))
			      (reroute-output-append
			       (make-pathname% :directory dir
					       :name (pathname-name lib-masterindex-file)
					       :type default-libindex-type)
			       *default-pathname-defaults*
			       (msg "(" name " " type " . ") 
			       (prin1 (file-namestring fname))
			       (msg ")" t))))
			  (setq wrote-something T)
			  (when parsing (write-libitem (parse-libitem item input-file t nil) 
						       'write-to-file outstream)))))))
	  (if parsing (rename-file new-file filename) (delete-file new-file))
	  (unless wrote-something (delete-file filename))
	  (delete-file copy-file)
	  (when restore-after (restore-lib-hashtable t))))

(deflibrary spring-clean
  (lib-argnames reindex reformat sort delete)
  (lib-argtypes yesno yesno yesno yesno)
  (lib-arghelp "Reindex all files?" "Reformat?" "Sort all files?" "Delete all non-library files?")
  (lib-defaultfns (lambda (reindex reformat sort delete)
		    (list (if (eq reindex '$) 'Y reindex)
			  (if (eq reformat '$) 'Y reformat)
			  (if (eq sort '$) 'Y sort)
			  (if (eq delete '$) nil delete))))
  (lib-mainfns spring-clean)
  (mhelp "Will do its best to reindex, reformat and/or sort every file in the 
default library directory. If your files are a real mess, you might consider
using emacs to get rid of the worst of the problems before using SPRING-CLEAN.
It will also delete any file in the directory that doesn't belong there
Generally this means everything except .lib and libindex.rec files; you will be 
asked for confirmation before each file is deleted.
If you get an error because of parsing problems, try again but answer
no to \"Reformat?\" and \"Sort?\" (it is not possible to reformat or sort 
a file that cannot be parsed). Better yet, delete the unparseable entry and
try again."))

(defun spring-clean (r p s d)
  (if (or r p s d)
      (let ((msg (concatenate 'string 
			      "This command has the potential to cause serious havoc with your library
files. You are strongly recommended to BACK UP ALL THE DIRECTORIES 
"
			      (reduce #'(lambda (x y) (concatenate 'string x "
" y)) (mapcar 'princ-to-string default-lib-dir))
			      "
BEFORE PROCEEDING.
Do you want to proceed with spring-cleaning?" )))
	(if (query msg nil)
	    (let ((files (reduce 'append (mapcar 'directory% default-lib-dir)))
		  (load-warn-p nil))
	      (clrhash *lib-masterindex*)
	      (restore-lib-hashtable t)
	      (when d 
		    (dolist (file (remove-if #'valid-file-p files)) 
			    (when (and (null (car (directory% (concatenate 'string (namestring file) '"/"))))
					;don't want to delete subdirectories
				       (query (concatenate 'string "Shall I delete " (namestring file) "?") nil))
				  (msgf "Deleting " (namestring file) t) 
				  (delete-file file))))
	      (setq files (remove-if-not #'library-file-p files))
	      (when r 
		    (dolist (dir default-lib-dir)
			    (when (probe-file (concatenate 'string dir lib-masterindex-file))
				  (delete-file (concatenate 'string dir lib-masterindex-file))))
		    (dolist (file files)
			    (msgf "Reindexing " (namestring file) t)
			    (reindex-libfile (file-namestring file) nil file nil)))
	      (setq files (remove-if-not #'library-file-p 
					 (reduce 'append (mapcar 'directory% default-lib-dir))))
					;on the first pass, we just build the libindex.rec without parsing 
					;since some of the needed-objects may not yet be in libindex.rec...
	      (restore-lib-hashtable t)
	      (when p 
		    (dolist (dir default-lib-dir)
			    (when (probe-file (concatenate 'string dir lib-masterindex-file))
				  (delete-file (concatenate 'string dir lib-masterindex-file))))
		    (dolist (file files)
			    (msgf "Reformatting " (namestring file) t)
			    (reindex-libfile (file-namestring file) t file nil)))
	      (restore-lib-hashtable t)
	      (setq files (remove-if-not #'library-file-p 
					 (reduce 'append (mapcar 'directory% default-lib-dir))))
					;possible that reindexing deleted some empty files; we don't want to sort those...
	      (when s (dolist (file files)
			      (msgf "Sorting " (namestring file) t)
			      (sort-libfile file nil))))))
	(msgf "This command does nothing unless you say \"Yes\" to one of the options!" t)))
  
(defun library-file-p (file)
  (let ((type (string-downcase (pathname-type file))))
    (string= type (string-downcase DEFAULT-LIBFILE-TYPE))))

(defun valid-file-p (file)
  (let ((type (string-downcase (pathname-type file))))
    (or (string= type (string-downcase DEFAULT-LIBFILE-TYPE))
	(string= type (string-downcase DEFAULT-LIBINDEX-TYPE))
	(string= (string-downcase (file-namestring file)) (string-downcase lib-masterindex-file))
	(string= (string-downcase (file-namestring file)) (string-downcase lib-keyword-file))
	(string= (string-downcase (file-namestring file)) (string-downcase lib-bestmode-file))
	(string= (string-downcase (file-namestring file)) "bestmodes.bk1")
	(string= (string-downcase (file-namestring file)) "bestmodes.bk2")
	(string= (string-downcase (file-namestring file)) "bestmodes.bk3")
	(string= (string-downcase (file-namestring file)) "bestmodes.bk4")
	(string= (string-downcase (file-namestring file)) "bestmodes.bk5"))))



(deflibrary fix-modes
  (mhelp "Change all references to obsolete flags into the 
appropriate new flag setting, for every mode in your library
directory. You only need to do this once.
You will be prompted before anything is changed, and
you should probably keep a backup copy of your old library in
case disaster strikes!
THE CODE FOR THIS COMMAND SHOULD BE REWRITTEN FOR EACH 
RELEVANT CHANGE TO THE TPS FLAGS.
At the minute, it's set up to remove references to 
REWRITE-DEFNS-EAGER, REWRITE-EQUAL-EXT and REWRITE-ONLY-EXT,
which have been removed, and to reset REWRITE-DEFNS
and REWRITE-EQUALITIES to appropriate values.
It also puts LAST-MODE-NAME at the head of all settings
for RECORDFLAGS."))

(defun fix-modes ()
  ;at this point, we shuld fix the old settings of rewrite-defns and rewrite-defns-eager.
  (let ((objlist nil)
	(writeable (mapcar 'directory-namestring default-lib-dir)))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val) (setq objlist (cons (list key elt) objlist)))))
	     *lib-masterindex*)
    (setq objlist (remove-if-not #'(lambda (x) (member (directory-namestring (cdadr x)) writeable :test 'string=)) objlist))
    (setq objlist (remove-if-not #'(lambda (x) (memq (caadr x) '(mode1 mode))) objlist))
    (setq objlist (sort objlist #'string-lessp :key #'car))
    (setq objlist (mapcar #'(lambda (x) (retrieve-item (car x) :type (caadr x) :multiple nil :writeable t
						       :preferred-dir (directory-namestring (cdadr x)))) objlist))
    ;now we have a list of libitems.
    (setq objlist (remove-if-not #'(lambda (x) (or (assoc 'rewrite-defns-eager (libitem-description x))
						   (memq 'rewrite-defns-eager 
							 (cadr (assoc 'recordflags (libitem-description x))))
						   (memq (cdr (assoc 'auto::rewrite-defns (libitem-description x)))
							 '((t) (nil)))))
				 objlist))
    (when objlist
	  (msgf "The following modes have references to REWRITE-DEFNS-EAGER, 
or treat REWRITE-DEFNS as boolean." t)
	  (dolist (obj objlist) (msg (libitem-name obj) "  "))
	  (when (query "Do you want to change them?" nil)
		(dolist (libitem objlist)
			(let* ((desc (libitem-description libitem))
			       (rde (cadr (assoc 'rewrite-defns-eager desc)))
			       (rd (cadr (assoc 'auto::rewrite-defns desc)))
			       (newrd (if (memq rd '(t nil))
					  (if rd
					      (if rde (list 'auto::eager)
						(list 'auto::lazy1))
					    (list 'auto::none))
					rd)))
			  (when (assoc 'auto::rewrite-defns desc)
				(setf (cdr (assoc 'auto::rewrite-defns desc)) (list newrd)))
			  (when (assoc 'recordflags desc)
				(setf (cdr (assoc 'recordflags desc))
				      (list (remove-if #'(lambda (x) (eq x 'rewrite-defns-eager))
						       (cadr (assoc 'auto::recordflags desc))))))
			  (setf (libitem-description libitem)
				(remove-if #'(lambda (x) (eq (car x) 'rewrite-defns-eager)) desc))
			  (update-libitem libitem))))))
  ;now we go after REWRITE-EQUALITIES / REWRITE-EQUAL-EXT / REWRITE-ONLY-EXT
  (let ((objlist nil)
	(writeable (mapcar 'directory-namestring default-lib-dir)))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val) (setq objlist (cons (list key elt) objlist)))))
	     *lib-masterindex*)
    (setq objlist (remove-if-not #'(lambda (x) (member (directory-namestring (cdadr x)) writeable :test 'string=)) objlist))
    (setq objlist (remove-if-not #'(lambda (x) (memq (caadr x) '(mode1 mode))) objlist))
    (setq objlist (sort objlist #'string-lessp :key #'car))
    (setq objlist (mapcar #'(lambda (x) (retrieve-item (car x) :type (caadr x) :multiple nil :writeable t
						       :preferred-dir (directory-namestring (cdadr x)))) objlist))
    ;now we have a list of libitems.
    (setq objlist (remove-if-not #'(lambda (x) (or (assoc 'rewrite-equal-ext (libitem-description x))
						   (assoc 'rewrite-only-ext (libitem-description x))
						   (memq 'rewrite-equal-ext
							 (cadr (assoc 'recordflags (libitem-description x))))
						   (memq 'rewrite-only-ext
							 (cadr (assoc 'recordflags (libitem-description x))))
						   (memq (cdr (assoc 'rewrite-equalities (libitem-description x)))
							 '((t) (nil)))))
				 objlist))
    (when objlist
	  (msgf "The following modes have references to REWRITE-EQUAL-EXT or
REWRITE-ONLY-EXT, or treat REWRITE-EQUALITIES as boolean." t)
	  (dolist (obj objlist) (msg (libitem-name obj) "  "))
	  (when (query "Do you want to change them?" nil)
		(dolist (libitem objlist)
			(let* ((desc (libitem-description libitem))
			       (ree (cadr (assoc 'rewrite-equal-ext desc)))
			       (roe (cadr (assoc 'rewrite-only-ext desc)))
			       (re (cadr (assoc 'rewrite-equalities desc)))
			       (newre (if (memq re '(t nil))
					  (if re
					      (if roe 'auto::only-ext
						(if ree 'auto::all 'auto::leibniz))
					    'auto::none)
					re)))
			  (when (assoc 'rewrite-equalities desc)
				(setf (cdr (assoc 'rewrite-equalities desc)) (list newre)))
			  (when (assoc 'recordflags desc)
				(setf (cdr (assoc 'recordflags desc))
				      (list (remove-if #'(lambda (x) (memq x '(rewrite-equal-ext rewrite-only-ext)))
						       (cadr (assoc 'auto::recordflags desc))))))
			  (setf (libitem-description libitem)
				(remove-if #'(lambda (x) (memq (car x) '(rewrite-equal-ext rewrite-only-ext))) desc))
			  (update-libitem libitem))))))
  (let ((objlist nil)
	(writeable (mapcar 'directory-namestring default-lib-dir)))
    (maphash #'(lambda (key val)
		 (when key (dolist (elt val) (setq objlist (cons (list key elt) objlist)))))
	     *lib-masterindex*)
    (setq objlist (remove-if-not #'(lambda (x) (member (directory-namestring (cdadr x)) writeable :test 'string=)) objlist))
    (setq objlist (remove-if-not #'(lambda (x) (memq (caadr x) '(mode1 mode))) objlist))
    (setq objlist (sort objlist #'string-lessp :key #'car))
    (setq objlist (mapcar #'(lambda (x) (retrieve-item (car x) :type (caadr x) :multiple nil :writeable t
						       :preferred-dir (directory-namestring (cdadr x)))) objlist))
    ;now we have a list of libitems.
    (setq objlist (remove-if-not #'(lambda (x) (and (assoc 'recordflags (libitem-description x))
						    (neq (caadr (assoc 'recordflags (libitem-description x))) 
							 'LAST-MODE-NAME)))
				 objlist))
    (when objlist
	  (msgf "About to add LAST-MODE-NAME to RECORDFLAGS in the following modes." t)
	  (dolist (obj objlist) (msg (libitem-name obj) "  "))
	  (when (query "Do you want to change them?" nil)
		(dolist (libitem objlist)
			(let* ((desc (libitem-description libitem))
			       (rec (cadr (assoc 'recordflags desc)))
			       (newrec (cons 'LAST-MODE-NAME
					     (delete 'LAST-MODE-NAME rec))))
			  (setf (cdr (assoc 'recordflags desc)) (list newrec))
			  (update-libitem libitem)))))))

(deflibrary check-needed-objects
  (lib-mainfns check-needed-objects)
  (mhelp "Checks for library objects which are not stored
in the chosen directory, but are needed by some object in
that directory."))

(defun check-needed-objects ()
  (let* ((fromdir (progn
		(msg "Choose Directory to Check for Needed Objects:" t)
		(choose-library-directory :backups t))))
    (when fromdir
	  (let ((extra-needed-objects nil)
		(local-objects nil)
		(local-types nil))
	    (maphash #'(lambda (name value)
			 (dolist (info value)
				 (when (string= (directory-namestring (cdr info)) 
						(namestring fromdir))
				       (let* ((load-warn-p nil)
					      (type (car info)))
					 (setq local-objects (cons name local-objects))
					 (setq local-types (cons type local-types))))))
		     *lib-masterindex*)
	    (mapcar #'(lambda (name type)
			(let ((libitem (retrieve-item name :type type :multiple nil
						      :preferred-dir fromdir)))
			  (dolist (needed (libitem-needed-objects libitem))
				  (unless (member needed local-objects)
					  (setq extra-needed-objects
						(adjoin needed
							extra-needed-objects))))))
		    local-objects local-types)
	    (if extra-needed-objects
		(progn
		  (msg "There are extra needed objects for the library directory" t fromdir t)
		  (if (cdr extra-needed-objects)
		      (msg "At least the following objects are needed:" t extra-needed-objects)
		    (msg "At least the following object is needed:" t (car extra-needed-objects))))
	      (msg "All needed objects are already in the library directory" t fromdir t))))))

(deflibrary import-needed-objects
  (lib-argnames omit-other-remarks)
  (lib-argtypes yesno)
  (lib-arghelp "Omit other-remarks property of objects?")
  (lib-defaultfns (lambda (omit-other-remarks)
		    (list (if (eq omit-other-remarks '$)
			      t
			    omit-other-remarks))))
  (lib-mainfns import-needed-objects)
  (mhelp "Copies library objects which are not stored
in the chosen directory, but are needed by some object in
that directory, into the directory.  If there is a choice
of objects to import, and SHOW-ALL-LIBOBJECTS is set to T,
then the user is prompted to choose one."))

(defun import-needed-objects (omit-other-remarks)
  (let* ((fromdir (progn
		    (msg "Choose Directory to Check for Needed Objects:" t)
		    (choose-library-directory))))
    (import-needed-objects-1 omit-other-remarks fromdir)))

(defun import-needed-objects-1 (omit-other-remarks fromdir)
    (when fromdir
	  (let ((extra-needed-objects nil)
		(local-objects nil)
		(local-types nil))
	    (maphash #'(lambda (name value)
			 (dolist (info value)
				 (when (string= (directory-namestring (cdr info)) 
						(namestring fromdir))
				       (let* ((load-warn-p nil)
					      (type (car info)))
					 (setq local-objects (cons name local-objects))
					 (setq local-types (cons type local-types))))))
		     *lib-masterindex*)
	    (mapcar #'(lambda (name type)
			(let ((libitem (retrieve-item name :type type :multiple nil
						      :preferred-dir fromdir)))
			  (dolist (needed (libitem-needed-objects libitem))
				  (unless (member needed local-objects)
					  (setq extra-needed-objects
						(adjoin needed
							extra-needed-objects))))))
		    local-objects local-types)
	    (if extra-needed-objects
		(let ((extra-filename nil))
		  (prompt-read extra-filename nil
				 (msgf "Name of file for extra needed objects? ")
				 'filespec
				 "extra" ((? (mhelp 'filespec))))
		    (let* ((extra-file (namestring
					(make-pathname% :directory fromdir
							:name (file-namestring
							       (pathname-name extra-filename))
							:type default-libfile-type))))
		      (do nil
			  ((not extra-needed-objects))  ; all needed objects have been retrieved already
			  (let* ((name (car extra-needed-objects)) ; but we need the libitem to see dependency
				 (libitem ; so we can be sure we save all the extra needed objects
				  (or (retrieve-item name :type 'abbr :fail-gently t)
				      (retrieve-item name :type 'lib-const :fail-gently t)
				      (retrieve-item name :type 'gwff :fail-gently t)
				      (retrieve-item name :type 'rrule :fail-gently t)
				      (retrieve-item name :type 'theory :fail-gently t)))
				 (fname (when libitem
					      (file-namestring (pathname-name (pathname (princ-to-string
											 (libitem-file libitem))))))))
			    (if libitem 
				(progn
				  (copy-libobject name (libitem-type libitem) fname omit-other-remarks
						  fromdir extra-file)
				  (setq extra-needed-objects
					(remove name extra-needed-objects))
				  (dolist (needed (libitem-needed-objects libitem))
					  (unless 
					   (or (member needed extra-needed-objects)
					       (member
						fromdir
						(gethash needed *lib-masterindex*)
						:test #'(lambda (x y)
							  (equal x (directory-namestring
								    (cdr y))))))
					   (setq extra-needed-objects
						 (cons needed extra-needed-objects)))))
			      (setq extra-needed-objects (cdr extra-needed-objects)))))))
	      (msg "All needed objects are already in the library directory" t fromdir t)))))

;;; ************** context : structure ***************
(context lib-structure)

; commands  for creating and using directories and subdirectories
(deflibrary create-lib-dir
  (lib-argnames directory)
  (lib-argtypes dirspec)
  (lib-arghelp "Library Directory to Create")
  (lib-defaultfns (lambda (directory) (list directory)))
  (lib-mainfns create-lib-dir)
  (mhelp "Create a directory to store files containing library items.
This will not only create the directory, but create a file libindex.rec
so that TPS will recognize the directory as a library directory.
This command can be executed for the latter purpose even if the
directory already exists. 
This command will automatically add the directory to DEFAULT-LIB-DIR in
the current session of TPS."))

(deflibrary create-lib-subdir
  (lib-argnames subdir)
  (lib-argtypes dirspec)
  (lib-arghelp "Library Subdirectory to Create")
  (lib-defaultfns (lambda (subdir)
		    (list subdir)))
  (lib-mainfns create-lib-subdir)
  (mhelp "Creates a subdirectory of a current library directory
in DEFAULT-LIB-DIR to store files containing library items.
This will not only create the directory, but also creates a
LIB-MASTERINDEX-FILE so that TPS will recognize the directory as a 
library directory.  This command will also add the subdirectory to
DEFAULT-LIB-DIR.  TPS automatically looks for subdirectories when setting
DEFAULT-LIB-DIR, so there is no need to add the subdirectory to
the DEFAULT-LIB-DIR setting in the tps3.ini file."))

(deflibrary delete-lib-dir
  (lib-argnames )
  (lib-argtypes )
  (lib-arghelp )
  (lib-defaultfns (lambda () nil))
  (lib-mainfns delete-lib-dir)
  (mhelp "Deletes a library directory and removes it from DEFAULT-LIB-DIR.
The command will fail if the directory contains any library objects
(i.e., if the index file is not empty)."))

(defun create-lib-dir (directory)
  (unless (member directory DEFAULT-LIB-DIR :test #'equal)
	  (when (and (not #-:clisp (probe-file (format nil "~d/" directory))
			  #+:clisp (ext:probe-directory
				    (format nil "~d/" directory)))
		    (probe-file (string-right-trim "/" directory)))
	    (throwfail t "Cannot create directory " directory
		       t " since a file by this name exists."))
	  (call-system (format nil "mkdir ~d" directory))
	  (do nil
	      #-:clisp ((probe-file directory))
	      ; mkaminski 10/19/2005 -- probe-file doesn't work on directories
              ; for clisp
	      #+:clisp ((ext:probe-directory directory))
	      )
	  (close
	   (open (format nil "~d/~d" directory lib-masterindex-file)
		 :direction :output
		 :if-does-not-exist :create)) ; the point is only to create the file
	  (setq DEFAULT-LIB-DIR (append DEFAULT-LIB-DIR (list directory)))
	  (restore-lib-hashtable t)))

(defun create-lib-subdir (subdir)
  (let ((count 0)
	(allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
	directory)
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
    (dolist (elt allowable-dirlist)
	    (incf count)
	    (complain count ") CREATE Subdirectory of " (pfile elt)))
    (incf count)
    (complain count ") DO NOTHING.")
    (setq count (1- (get-a-number count)))
    (setq directory (nth count allowable-dirlist))
    (when directory
	  (create-lib-dir (format nil "~d~d" directory subdir)))))

(defun delete-lib-dir ()
  (let ((directory (choose-library-directory)))
    (when directory
	  (when (probe-file (concatenate 'string directory lib-masterindex-file))
		(let ((eof (gensym))
		      (s (open (concatenate 'string directory lib-masterindex-file)
			       :direction :input)))
		  (if (eq (read s nil eof) eof)
		      (progn
			(close s)
			(delete-file (concatenate 'string directory lib-masterindex-file))
			#+:clisp (delete-file (concatenate 'string directory "keywords.rec")))
		    (progn
		      (close s)
		      (complain "There are library objects stored in " t directory t)
		      (if (query "Shall I delete them?" nil)
			  (let ((delete-filelist nil))
			    (maphash #'(lambda (key value)
					 (declare (ignore key))
					 (dolist (info value)
						 (when (string= (directory-namestring (cdr info)) directory)
						       (setq delete-filelist 
							     (adjoin (cdr info) delete-filelist
								     :test #'(lambda (x y)
									       (string=
										(namestring x)
										(namestring y))))))))
				     *lib-masterindex*)
			    (dolist (a delete-filelist)
				    (delete-libfile-2 a :delete-from-index-file nil))
			    (delete-file (concatenate 'string directory lib-masterindex-file)))
			(throwfail "Library Directory not deleted."))))))
	  (let ((files (directory% directory))
		(failure nil))
	    (dolist (file (cdr files))
		    (if #-:clisp (probe-file (format nil "~d/" file))
		        #+:clisp (ext:probe-directory (format nil "~d/" file))
			(progn
			  (setq failure t)
			  (complain "Cannot delete subdirectory " file))
		      (when (and (probe-file file)
				 (query (concatenate 'string "Shall I delete " (namestring file) "?") t))
			    (msgf "Deleting " (namestring file) t)
			    (delete-file file))))
	    (unless failure
	      (call-system (format nil "rmdir ~d" directory)))
	    (setq default-lib-dir (remove directory default-lib-dir
					  :test #'equal))
	    (restore-lib-hashtable)))))

; If we're given an existing directory which is not in the lists
; DEFAULT-LIB-DIR and BACKUP-LIB-DIR, we should update the hash table
; to recognize entries in this directory (by calling restore-hash-table
; tricking it into believing the new dir is in DEFAULT-LIB-DIR).
; Unfortunately, this takes a while to execute, but we need to do it
; to make other commands behave properly.  - cebrown 8/5/99
(defun choose-library-directory (&key (new nil) (backups nil) (dirlist default-lib-dir))
  (let ((count 0)
	(allowable-dirlist (if (eq new t)
			       nil
			     (mapcar 'directory-namestring dirlist)))
	directory)
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
    (dolist (elt allowable-dirlist)
	    (incf count)
	    (complain count ") " (pfile elt)))
    (incf count)
    (when (and backups (not (eq new t)))
	  (complain count ") A Directory from BACKUP-LIB-DIR")
	  (incf count))
    (if new
	(complain count ") A Subdirectory of a Library Directory")
      (complain count ") An Existing Subdirectory of a Library Directory"))
    (incf count)
    (if new
	(if (eq new 'maybe)
	    (complain count ") Another Library Directory")
	  (complain count ") A New Library Directory"))
      (complain count ") Another Existing Library Directory"))
    (incf count)
    (complain count ") DO NOTHING.")
    (setq count (1- (get-a-number count)))
    (let ((l (length allowable-dirlist)))
      (cond ((< count l)
	     (setq directory (nth count allowable-dirlist)))
	    ((and backups (= count l))
	     (setq directory
		   (choose-library-directory :backups nil :dirlist backup-lib-dir)))
	    (backups
	     (setq count (- count (+ l 1))))
	    (t (setq count (- count l))))
      (or directory
	  (cond ((= count 0)
		 (choose-library-subdirectory :new new :backups backups :dirlist dirlist))
		((= count 1)
		 (do nil
		     (directory directory)
		     (prompt-read directory nil (msgf "Please Enter the Full Path")
				  'dirspec '$ ((? (mhelp 'dirspec))))
		     (if (probe-file (make-pathname% :directory directory
						     :name lib-masterindex-file))
			 (if (eq new t)
			     (progn
			       (complain "There is already a library directory " directory)
			       (setq directory nil))
			   (unless (member directory (append default-lib-dir
							     backup-lib-dir)
					   :test #'string=)
				   (let ((real-dld default-lib-dir))
				     (setq default-lib-dir (cons directory default-lib-dir))
				     (restore-lib-hashtable)
				     (setq default-lib-dir real-dld))))
		       (unless new
			       (complain "There is no library directory " directory)
			       (setq directory nil)))))
		(t nil))))))

(defun choose-library-subdirectory (&key (new nil) (backups nil) (dirlist default-lib-dir))
  (let ((count 0)
	(allowable-dirlist (mapcar 'directory-namestring dirlist))
	directory)
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
    (dolist (elt allowable-dirlist)
	    (incf count)
	    (complain count ") Subdirectory of " (pfile elt)))
    (incf count)
    (when backups
	  (complain count ") Subdirectory of a Directory from BACKUP-LIB-DIR")
	  (incf count))
    (if new
	(complain count ") Subdirectory of Another Library Directory")
      (complain count ") Subdirectory of Another Existing Library Directory"))
    (incf count)
    (complain count ") DO NOTHING.")
    (setq count (1- (get-a-number count)))
    (let ((l (length allowable-dirlist)))
      (cond ((< count l)
	     (setq directory (nth count allowable-dirlist)))
	    ((and backups (= count l))
	     (setq directory
		   (choose-library-directory :backups nil :dirlist backup-lib-dir)))
	    ((or (and backups (= count (+ l 1)))
		 (= count l))
	     (do nil
		 (directory)
		 (prompt-read directory nil (msgf "Please Enter the Full Path")
			      'dirspec '$ ((? (mhelp 'dirspec))))
		 (unless (or new
			     (probe-file (make-pathname% :directory directory
							 :name lib-masterindex-file)))
			 (complain "There is no library directory " directory)
			 (setq directory nil)))))
      (if directory
	  (let (subdir
		dir)
	    (do nil
		(dir dir)
		(prompt-read subdir nil (msgf "Subdirectory Name")
			     'dirspec '$ ((? "Name of the Subdirectory (not the Full Path)")))
		(setq dir (format nil "~d~d" directory subdir))
		(if (probe-file (make-pathname% :directory dir
						:name lib-masterindex-file))
		    (if (eq new t)
			(progn
			  (complain "There is already a library directory " dir)
			  (setq dir nil))
		      (unless (member dir (append default-lib-dir
						  backup-lib-dir)
				      :test #'string=)
			      (let ((real-dld default-lib-dir))
				(setq default-lib-dir (cons dir default-lib-dir))
				(restore-lib-hashtable)
				(setq default-lib-dir real-dld))))
		  (unless new
			  (complain "There is no library directory " dir)
			  (setq dir nil)))))
	nil))))

(deflibrary copy-libdir
  (lib-argnames omit-other-remarks)
  (lib-argtypes yesno)
  (lib-arghelp "Omit other-remarks property of objects?")
  (lib-defaultfns (lambda (omit-other-remarks)
		    (list (if (eq omit-other-remarks '$)
			      t
			    omit-other-remarks))))
  (lib-mainfns copy-libdir)
  (mhelp "COPY-LIBDIR can be used to copy a library directory into a
new library directory which TPS will automatically create,
or it can be used to copy the contents of a library directory into
an existing library directory.  If COPY-LIBDIR is copying into
an existing directory, and an object of the same name and type
exists in both the source and destination directory, the original
object remains in the destination directory instead of being overwritten.
The user has the option of omitting the other-remarks property
of the library objects.  If any needed-objects are left over,
the user is given the option of copying these extra needed-objects
into a new library file in the destination library directory.

COPY-LIBDIR will also copy the bestmodes and keywords files, if they
exist.  If the target directory already has a bestmodes or keywords
file, then the corresponding files will be merged."))

(defun copy-libdir (omit-other-remarks)
  (let* ((fromdir (progn
		    (msg "Choose Directory to Copy from:" t)
		    (choose-library-directory :backups t)))
	 (todir (when fromdir
		      (msg "Choose Directory to Copy to:" t)
		      (choose-library-directory :new 'maybe))))
    (copy-libdir-work fromdir todir omit-other-remarks)))

(defun copy-libdir-work (fromdir todir omit-other-remarks &optional (ignore-extras nil))
    (when (and fromdir todir)
	  (when (equal fromdir todir)
		(throwfail "Directories " fromdir t "and " todir t "are the same"))
	  (unless (probe-file (format nil "~d/~d" todir lib-masterindex-file))
		  (create-lib-dir todir))
	  (let ((from-index-file (make-pathname% :directory fromdir
						 :name (file-namestring
							(pathname-name
							 lib-masterindex-file))
						 :type default-libindex-type))
		(extra-needed-objects nil))
; I'm reading the index file in the source directory.
; An alternative would be to go through (via maphash) *lib-masterindex*
; looking for objects in the directory, but the problem is that *lib-masterindex*
; is changing in the body of the loop, while the index file (in the source dir)
; is constant.
	    (with-open-file (*input-stream* from-index-file :direction :input)
			    (do ((itemi nil (read *input-stream* nil control-d-char)))
				((eq itemi control-d-char))
			      (when itemi
				(format t "checking item ~d~%" itemi)
				      (let* ((load-warn-p nil)
					     (name (car itemi))
					     (type (cadr itemi))
					     (fname (cddr itemi))
					     (libitem (retrieve-item name :type type :multiple nil
								     :preferred-dir fromdir :fail-gently t)))
					(copy-libobject name type fname omit-other-remarks
							fromdir
							(namestring
							 (make-pathname% :directory todir
									 :name 
									 (file-namestring
									  (pathname-name
									   fname))
									 :type 
									 default-libfile-type)))
					(setq extra-needed-objects
					      (remove name extra-needed-objects))
					(dolist (needed (libitem-needed-objects libitem))
						(unless 
						 (or (member needed extra-needed-objects)
						     (member
						      todir
						      (gethash needed *lib-masterindex*)
						      :test #'(lambda (x y)
								(equal x (directory-namestring
									  (cdr y))))))
						 (setq extra-needed-objects
						       (cons needed
							     extra-needed-objects))))))))
	    (when (and extra-needed-objects
		       (not ignore-extras)
		       (query "Do you want to copy the extra needed objects into a file?" t))
		  (let ((extra-filename nil))
		    (prompt-read extra-filename nil
				 (msgf "Name of file for extra needed objects? ")
				 'filespec
				 "extra" ((? (mhelp 'filespec))))
		    (let* ((extra-file (namestring
					(make-pathname% :directory todir
							:name (file-namestring
							       (pathname-name extra-filename))
							:type default-libfile-type))))
		      (do nil
			  ((not extra-needed-objects))  ; all needed objects have been retrieved already
			  (let* ((name (car extra-needed-objects)) ; but we need the libitem to see dependency
				 (libitem ; so we can be sure we save all the extra needed objects
				  (or (retrieve-item name :type 'abbr :fail-gently t)
				      (retrieve-item name :type 'lib-const :fail-gently t)
				      (retrieve-item name :type 'gwff :fail-gently t)
				      (retrieve-item name :type 'rrule :fail-gently t)
				      (retrieve-item name :type 'theory :fail-gently t)))
				 (fname (when libitem
					      (file-namestring (pathname-name (pathname (princ-to-string
											 (libitem-file libitem))))))))
			    (if libitem 
				(progn
				  (copy-libobject name (libitem-type libitem) fname omit-other-remarks
						  fromdir extra-file)
				  (setq extra-needed-objects
					(remove name extra-needed-objects))
				  (dolist (needed (libitem-needed-objects libitem))
					  (unless 
					   (or (member needed extra-needed-objects)
					       (member
						todir
						(gethash needed *lib-masterindex*)
						:test #'(lambda (x y)
							  (equal x (directory-namestring
								    (cdr y))))))
					   (setq extra-needed-objects
						 (cons needed extra-needed-objects)))))
			      (setq extra-needed-objects (cdr extra-needed-objects)))))))))
; Copy the bestmodes and keywords files (if they exist):
; There's a lot of trickery going on here.  The idea is that I temporarily
; change default-lib-dir and backup-lib-dir and restore the index hashtable
; (which also updates the globals lib-keyword-list and lib-bestmodes)
; so that TPS knows about bestmodes and keywords only from fromdir and todir
; (and, in the case of bestmodes TPS believes the bestmodes are "mine" --
; so they should be written).  Then I change default-lib-dir to be just todir
; and update the bestmodes and keywords files.  Finally, I change everything
; back to normal and restore the index 
	  (let ((real-dld default-lib-dir)
		(real-bld backup-lib-dir)
		(real-adsub add-subdirectories))
; Trick the code to load only those in fromdir and todir as "mine"
; and printing them all into todir
	    (setq add-subdirectories nil)
	    (setq default-lib-dir (list todir fromdir)) ; both, so that data will merge, with todir first so that bestmodes output goes into todir
	    (setq backup-lib-dir nil)
	    (restore-lib-hashtable t) ; this will set lib-keyword-file & lib-bestmodes to be merged data
	    (let ((q (make-pathname% :directory todir
				     :name (namestring (pathname-name lib-keyword-file))
				     :type default-libindex-type)))
	      (when (probe-file q)
		    (delete-file q))
	      (reroute-output q *default-pathname-defaults* (msg lib-keyword-list t)))
	    (output-bestmodes-to-file) ; 4/2/01 - cebrown
; And now, back to normal . . .
	    (setq add-subdirectories real-adsub)
	    (setq default-lib-dir real-dld)
	    (setq backup-lib-dir real-bld)
	    (restore-lib-hashtable t))))

(deflibrary update-libdir
  (lib-argnames omit-other-remarks directory)
  (lib-argtypes yesno dirspec)
  (lib-arghelp "Omit other-remarks property of objects?" "Common Destination Library Directory")
  (lib-defaultfns (lambda (omit-other-remarks directory)
		    (list (if (eq omit-other-remarks '$)
			      t
			    omit-other-remarks)
			  directory)))
  (lib-mainfns update-libdir)
  (mhelp "UPDATE-LIBDIR can be used to update a (common) library directory
by copying any object from a directory DEFAULT-LIB-DIR or BACKUP-LIB-DIR
into the (common) library directory, if it is not already there.
Before updating from a library directory, the user is asked whether
to update from this directory.  This is so one can choose a collection
of library directories to combine into the common destination directory.

This has the same effect of 

1. calling COPY-LIBDIR with copying from each (chosen) directory in
DEFAULT-LIB-DIR and BACKUP-LIB-DIR into the (common) destination
library directory.

2. Calling IMPORT-NEEDED-OBJECTS to ensure all needed-objects
are also put into the destination directory.

If one wants to get the latest version of all library items,
specify the complete pathname of a nonexistent directory when 
TPS prompts for a destination directory."))

(defun update-libdir (omit-other-remarks directory)
;  (if (probe-file (make-pathname% :directory directory
;				  :name (namestring (pathname-name lib-masterindex-file))
;				  :type default-libindex-type))
      (dolist (fromdir (append default-lib-dir backup-lib-dir))
	(when (query (format nil "Update From ~d?" fromdir) t)
	  (copy-libdir-work fromdir directory omit-other-remarks t)))
      (import-needed-objects-1 omit-other-remarks directory))
;    (throwfail "There is no library directory " directory)))

(deflibrary delete-libfile
  (lib-argnames filename)
  (lib-argtypes filespec)
  (lib-arghelp "Library File to Delete")
  (lib-defaultfns (lambda (filename) (list filename)))
  (lib-mainfns delete-libfile)
  (mhelp "Delete a Library File"))

(defun delete-libfile (filename)
  (let ((fullname (complete-lib-filename-choose filename default-libfile-type t)))
    (when fullname
	  (delete-libfile-2 fullname))))

(defun delete-libfile-2 (fullname &key (delete-from-index-file t))
  (let ((delete-list nil))
    (maphash #'(lambda (key value)
		 (dolist (info value)
			 (when (string= (namestring (cdr info)) (namestring fullname))
			       (setq delete-list (cons (cons key info) delete-list)))))
	     *lib-masterindex*)
    (dolist (a delete-list)
	    (delete-item-from-masterindex (car a) (cdr a)
					  :delete-from-index-file 
					  delete-from-index-file))
    (delete-file fullname)))

(deflibrary rename-libfile
  (lib-argnames oldfile newfile)
  (lib-argtypes filespec filespec)
  (lib-arghelp "Library File to Rename" "New Name for File")
  (lib-defaultfns (lambda (oldfile newfile) (list oldfile newfile)))
  (lib-mainfns rename-libfile)
  (mhelp "Rename a Library File (within the same library directory)"))

(defun rename-libfile (oldfile newfile)
  (let* ((full-oldfile (complete-lib-filename-choose oldfile default-libfile-type t))
	 (directory (directory-namestring full-oldfile))
	 (full-newfile (namestring
			(make-pathname% :directory directory
					:name (file-namestring
					       (pathname-name newfile))
					:type default-libfile-type))))
    (move-libfile-2 full-oldfile full-newfile)
    (msg "File renamed." t)))

(deflibrary rename-libdir
  (lib-mainfns rename-libdir)
  (mhelp "Rename a Library Directory"))

(defun rename-libdir ()
  (let* ((fromdir (progn
		    (msg "Choose Directory to Rename:" t)
		    (choose-library-directory)))
	 (todir (when fromdir
		      (msg "Choose New Directory Name:" t)
		      (choose-library-directory :new t))))
    (when (and fromdir todir)
          (call-system (format nil "mv ~d ~d" fromdir todir))
	  (when (member fromdir default-lib-dir :test #'string=)
		(setq default-lib-dir (cons todir
					    (remove fromdir default-lib-dir :test #'string=))))
	  (restore-lib-hashtable))))

(deflibrary copy-libfile
  (lib-argnames oldfile newfile omit-other-remarks)
  (lib-argtypes filespec filespec yesno)
  (lib-arghelp "Existing file to copy FROM" "New file to copy TO"
	       "Omit other-remarks property of objects?")
  (lib-defaultfns (lambda (oldfile newfile omit-other-remarks)
		    (list oldfile newfile
			  (if (eq omit-other-remarks '$)
			      t
			    omit-other-remarks))))
  (mhelp "Copy a file of library objects. The source file will
be found among the directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR
(the user will be prompted if more than one such file exists, and also
if there is a choice of directories for the new file).
Needed objects are not copied."))

(defun copy-libfile (oldfname newfname &optional (omit-other-remarks nil))
  (let ((load-warn-p nil)
	(oldfilename (complete-lib-filename-choose oldfname))
	(allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
	(count 0)
	(newfname (concatenate 'string (namestring (pathname-name newfname)) "." default-libfile-type))
	(objlist nil))
    (setq allowable-dirlist (delete (directory-namestring oldfilename) allowable-dirlist :test 'string=))
    (setq allowable-dirlist (mapcar #'(lambda (x) (concatenate 'string x newfname)) allowable-dirlist))
    (setq allowable-dirlist (remove-if #'probe-file allowable-dirlist))
    (unless allowable-dirlist (throwfail t "All the directories in DEFAULT-LIB-DIR already contain a file " newfname t))
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
    (when allowable-dirlist
	  (dolist (elt allowable-dirlist)
		  (incf count)
		  (complain count ") Make new file " (pfile elt))))
    (incf count)
    (complain count ") DO NOTHING.")
    (setq count (1- (get-a-number count)))
    (setq newfname (nth count allowable-dirlist))
    (when newfname
	  (with-open-file (*input-stream* oldfilename :direction :input)
			  (do ((item (read *input-stream* nil control-d-char)
				     (read *input-stream* nil control-d-char)))
			      ((eq item control-d-char))
			      (setq objlist (cons (list (cadr (memq :name item)) 
							(cadr (memq :type item))) objlist))))
	  (setq objlist (reverse objlist)) ; keep the file in the original order
	  (dolist (elt objlist)
		  (msg "Copying " (car elt) " of type " (cadr elt) t)
		  (copy-libobject (car elt) (cadr elt) newfname omit-other-remarks (directory-namestring oldfilename) newfname))
	  (msg "File copied." t))))

(deflibrary move-libfile
  (lib-argnames oldfile newfile)
  (lib-argtypes filespec filespec)
  (lib-arghelp "Existing file to move" "New file to create")
  (mhelp "Move a file of library objects. The source file will
be found among the directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR
(the user will be prompted if more than one such file exists, and also
if there is a choice of directories for the new file).
Needed objects are not moved."))

(defun move-libfile (oldfname newfname)
  (let ((load-warn-p nil)
	(oldfilename (complete-lib-filename-choose oldfname default-libfile-type t))
	(allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
	(count 0)
	(newfname (concatenate 'string (namestring (pathname-name newfname)) "." default-libfile-type)))
    (setq allowable-dirlist (delete (directory-namestring oldfilename) allowable-dirlist :test 'string=))
    (setq allowable-dirlist (mapcar #'(lambda (x) (concatenate 'string x newfname)) allowable-dirlist))
    (setq allowable-dirlist (remove-if #'probe-file allowable-dirlist))
    (unless allowable-dirlist (throwfail t "All the directories in DEFAULT-LIB-DIR already contain a file " newfname t))
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-options))
    (when allowable-dirlist
	  (dolist (elt allowable-dirlist)
		  (incf count)
		  (complain count ") Make new file " (pfile elt))))
    (incf count)
    (complain count ") DO NOTHING.")
    (setq count (1- (get-a-number count)))
    (setq newfname (nth count allowable-dirlist))
    (when newfname
	  (move-libfile-2 oldfilename newfname)
	  (msg "File moved." t))))

; cebrown - 8/3/99 - separated this into a new function called by both
; move-libfile and rename-libfile
(defun move-libfile-2 (oldfilename newfname)
  (let ((objlist nil))
    (with-open-file (*input-stream* oldfilename :direction :input)
		    (do ((item (read *input-stream* nil control-d-char)
			       (read *input-stream* nil control-d-char)))
			((eq item control-d-char))
			(setq objlist (cons (list (cadr (memq :name item)) 
						  (cadr (memq :type item))) objlist))))
    (setq objlist (reverse objlist)) ; keep the file in the original order
    (dolist (elt objlist)
	    (msg "Moving " (car elt) " of type " (cadr elt) t)
	    (movlib2 (car elt) (cadr elt) (directory-namestring oldfilename) newfname))))

