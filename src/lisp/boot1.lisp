;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :core)
;;; Next line is commented out, since PART-OF is not yet defined.
;;;(part-of BOOTSTRAP)

(deffile boot1
  (part-of BOOTSTRAP)
  (extension lisp)
  (mhelp "Defines modules, message handling and various other stuff."))

;;; Changed tps-packages to modules everywhere. DAN 9MAR91

;;;

;;;Try to eliminate some tps-warnings, but this seems unnecessary.

(eval-when (load compile eval)
(defcategory module
  (define defmodule)
  (properties
   (needed-modules multiple)
   (lisp-pack single)
   (macro-files read-multiple-and-export)
   (files read-multiple-and-export)
   (mhelp single))
  (global-list global-modulelist)
  (mhelp-line "module")
  (mhelp-fn module-help))
)

  
(eval-when (compile load eval)
(defmacro defunobsolete (old use-instead)
`(defun ,old (&rest args)
   (tps-warning (format nil "Calling ~A which is obsolete. Use ~A ~
instead.~%" ',old ',use-instead))
   (apply (symbol-function ',use-instead) args)))
)

(eval-when (compile load eval)
  (unless (and (find-package :common-lisp) 
	       (find-symbol "DEFPACKAGE" (find-package :common-lisp)))
    (defmacro defpackage (&rest args)
      (tps-warning "Calling DEFPACKAGE which is obsolete. Use DEFMODULE instead.")
      `(defmodule ,@args))))

(defun module-package (module)
  (and (symbolp module) (module-p module)
       (find-package (module-lisp-pack module))))

(defunobsolete find-pack-package module-package)

(defun module-p (symbol) (get symbol 'module))
(defunobsolete package-p module-p)

(defun module-loaded-p (module) (get module 'loaded-p))
(eval-when (load compile eval)
  (defsetf module-loaded-p (module) (new-val)
     `(setf (get ,module 'loaded-p) ,new-val)))

(defunobsolete loaded-package-p module-loaded-p)

(defun module-needed-modules (module)
  (get module 'needed-modules))
(defun module-macro-files (module)
  (get module 'macro-files))
(defun module-files (module)
  (get module 'files))
(defun module-mhelp (module)
  (get module 'mhelp))
(defun module-lisp-pack (module)
  (get module 'lisp-pack))

(defsetf module-needed-modules (module) (new-val)
  `(setf (get ,module 'needed-modules) ,new-val))
(defsetf module-macro-files (module) (new-val)
  `(setf (get ,module 'macro-files) ,new-val))
(defsetf module-files (module) (new-val)
  `(setf (get ,module 'files) ,new-val))
(defsetf module-mhelp (module) (new-val)
  `(setf (get ,module 'mhelp) ,new-val))
(defsetf module-lisp-pack (module) (new-val)
  `(setf (get ,module 'lisp-pack) ,new-val))


(defun sys-load-module (module)
  (dolist (np (module-needed-modules module))
    (if (module-p np)
	(load-module np)
	(throwfail np " is not a known module, but required for "
		   module ".")))
  (let ((current-context 'unclassified)
  	(*package*
	  (let ((fpack (module-package module)))
	    (unless fpack
	      (tps-warning "Lisp package for TPS module " module " not found." t
		       "Unless every file starts with (in-package :...) it will be loaded into "
		       (package-name *package*) "."))
	    (if fpack fpack *package*))))
    (declare (special current-context *package*))	
    (msgf "--- Loading " module
	  " into Lisp package " (package-name *package*) " ---")
    (dolist (pf (append (module-macro-files module) (module-files module)))
      (tload pf))
    (setf (module-loaded-p module) t)))

(defunobsolete sys-load-package sys-load-module)

(defun load-macro-files (module)
  (let ((current-context 'unclassified)
  	(*package*
	  (let ((fpack (module-package module)))
	    (unless fpack
	      (tps-warning "Lisp package for TPS module " module " not found." t
		       "Unless every file starts with (in-package :...) it will be loaded into "
		       (package-name *package*) "."))
	    (if fpack fpack *package*))))
    (dolist (filespec (nreverse (needed-macro-files (list module))))
      (tload filespec))))


;;; defines functions and macros dealing with TPS packages relations
;;; between them.
;;;
;;; For the benefit of the compiler, a record of all loaded packages
;;; and the necessary packages for a given file is kept.  Every file
;;; must be marked with the packages it needs.
;;;
;;; Every package has one or more files defining macros for that
;;; package, which includes definition of flags (since DEFFLAG is a macro).
;;;
;;; IMPORTANT: No file may be part of more than one package!

(defcategory lisp-pack
  (define def-lisp-package1)
  (properties
   (needed-lisp-packages multiple)
   (mhelp single))
  (global-list global-lisp-packagelist)
  (mhelp-line "Lisp package")
  (mhelp-fn princ-mhelp))

(push (cons 'def-lisp-package 'lisp-pack) global-definelist)

(defmacro def-lisp-package (name &rest props)
  `(progn (def-lisp-package1 ,name ,@props)
	  (when (not (find-package ',name))
	    (make-package ',name))
	  (when (get ',name 'needed-lisp-packages)
	    (use-package (get ',name 'needed-lisp-packages)
			 (find-package ',name)))))




;;; (LOAD-MODULE module) loads the given module and checks whether
;;; all necessary modules are also loaded.  If not, they will be
;;; loaded, too.


(defun load-modules (modulelist &optional (force-load nil))
  (dolist (module modulelist (reorganize))
    (load-module module force-load)))

;(defunobsolete load-packages load-modules)


(defun load-module (module &optional (force-load nil))
  (if (and (not force-load) (module-loaded-p module))
      (msgf "Module " module " already loaded.")
      (sys-load-module module)))

(defunobsolete load-package load-module)

(defun sys-load (modulelist)
  (dolist (module modulelist (reorganize))
    (sys-load-module module)))

(defun load-all-modules ()
  (dolist (module (if (eq (car global-modulelist) 'maclisp-extensions)
		       global-modulelist
		       (reverse global-modulelist)))
    (when (symbolp module) (load-module module))))

;(defunobsolete load-all-packages load-all-modules)

(defun load-all-modules-except (exceptionlist)
  (dolist (module (if (eq (car global-modulelist) 'maclisp-extensions)
		       global-modulelist
		       (reverse global-modulelist)))
    (when (and (symbolp module)
	       (not (member module exceptionlist)))
      (load-module module))))

;(defunobsolete load-all-packages-except load-all-modules-except)

;;; For the moment we ignore the PART-OF line at the begining
;;; of a file (it doesn't appear consistently).

(defmacro part-of (package) (declare (ignore package)) `nil)

(defun needed-macro-files (module-list)
  (do ((module-list module-list (cdr module-list))
       (needed-files nil (append (needed-mfs (car module-list))
				 needed-files)))
      ((null module-list) needed-files)))

(defun needed-mfs (module)
  (unless (module-p module)
    (error "~%Cannot compile file, since module ~S is not known."  module)
    ;;(^G)
    )
  (if (module-loaded-p module) nil
      (let ((mfs (module-macro-files module)))
	(prog1 (revappend mfs
			  (needed-macro-files (module-needed-modules module)))
	  (setf (module-loaded-p module) t)))))

;;The module help function

(defun module-help (keyword category)
  (msg t "It is " (if (module-loaded-p keyword) "" "not ") "loaded. ")
  (feat-help keyword category 20 70 '(needed-modules macro-files files)))

(defunobsolete package-help module-help)


(defun organize-all ()
  (organize-cat-contxt)
  (dolist (category global-categorylist)
    (organize-global-list category)))

(defun reorganize ()
  (organize-cat-contxt)
  (dolist (category global-categorylist)
    (when (symbolp (car (eval (get category 'global-list))))
	  (organize-global-list category))))

(defun unique-sort (lis sort-fn)
  (do ((sort-lis (sort lis sort-fn) (cdr sort-lis))
       (new-lis nil (if (and (cdr sort-lis)
			     (equal (car sort-lis) (cadr sort-lis)))
			new-lis (cons (car sort-lis) new-lis))))
      ((null sort-lis) (nreverse new-lis))))


;;; Used in ordering contexts 8/30/87 DAN

(defun context< (context1 context2)
  (cond ((not (numberp (get context1 'order))) nil)
	((not (numberp (get context2 'order))) t)
	(t (< (get context1 'order) (get context2 'order)))))

;;; Changed to reorganize contexts based on their order properties 8/29/87 DAN

(defun organize-cat-contxt ()
  (setq global-categorylist (unique-sort global-categorylist #'alphalessp))
  (setq global-contextlist
	(unique-sort (remove-if-not #'symbolp global-contextlist) #'context<))
  (dolist (context global-contextlist)
    (push (cons 'context context) (get context 'contexts))))

(eval-when (load compile eval)
(defmacro categ-context (tps-obj category)
  `(cdr (assoc ,category (get ,tps-obj 'contexts)))))

(defun organize-global-list (category)
  (let ((glist-name (get category 'global-list)))
    (do ((glist (sort (eval glist-name)
		      #'(lambda (it1 it2)
			  (if (symbolp it1)
			      (if (symbolp it2)
				  (let ((c1 (categ-context it1 category))
					(c2 (categ-context it2 category)))
				    (or ;(alphalessp c1 c2)
				        (context< c1 c2) ; 8/30/87 DAN
					(and (eq c1 c2)
					     (alphalessp it1 it2))))
				  nil)
			      t)))
		(cdr glist))
	 (new-glist nil)
	 (c-context nil))
	((null glist) (set glist-name (nreverse new-glist)))
      (let ((item (car glist))
	    (nitem (cadr glist)))
	(when (symbolp item)
	      (when (not (eq c-context (categ-context item category)))
		    (setq c-context (categ-context item category))
		    (push (list c-context) new-glist))
	      (if (not (eq item nitem)) (push item new-glist)))))))

;;; Next comes the category of files

(defun mhelp-file (file category)
  (princ-mhelp file category)
  (let ((part-of (get file 'part-of))
	(truename (or (locate-file file source-path nil)
		      (string file))))
    ;;was just "file" above, but needs to be a string for namestring... MB
    (if part-of
	(msgf (namestring truename) " is part of module " part-of ".")
	(msgf (namestring truename) " is not part of any module."))))



;;; Added condition that list be non-nil, otherwise infinite loop results
;;; if predicate is true of nil DAN
(defun split-list (list predicate)
  "split-list takes a list and a predicate on the elements of the list.
  It returns two values: the first is the (reverse) contiguous segment
  of the list in which each element satisfies predicate, the second is
  the tail of the list (eq)."
  (do ((list list (cdr list))
       (prefix nil (cons (car list) prefix)))
      ((or (null list) (not (funcall predicate (car list))))
       (values prefix list))))

(defun reorganize-global-list (category)
  (let ((glist-name (get category 'global-list))
	(last-place (list (list nil))))
    (multiple-value-bind (new-elts rest-list)
	(split-list (eval glist-name) #'symbolp)
      ;; Now we insert the new elements, one by one, without
      ;; first sorting them - even though that could be done easily.
      (setq new-elts (delete-duplicates new-elts))
      (if rest-list
	  (dolist (new-elt new-elts (set glist-name rest-list))
	    (let* ((context (categ-context new-elt category))
		   (context-place (if (and (consp (car last-place))
					   (eq context (caar last-place)))
				      last-place
				      (insert-context context rest-list))))
	      ;; (obj-place ..)
	      (if (cdr context-place)
		  (insert-object new-elt (cdr context-place))
		  (setf (cdr context-place) (list new-elt)))
	      (setq last-place context-place)))
	  (organize-global-list category)))))

(eval-when (load compile eval)
(defmacro destructive-push (new-elt list)
  `(progn (setf (cdr ,list) (cons (car ,list) (cdr ,list))
		(car ,list) ,new-elt)
	  ,list)))

(defun insert-context (context rest-list)
  (do ((rest-list rest-list (cdr rest-list))
       (head))
      ((null (cdr rest-list))
       (setf (cdr rest-list) (list (list context)))
       (cdr rest-list))
    (setq head (car rest-list))
    (when (and (not (symbolp head))
	       (string<= context (car head)))
      (if (eq context (car head))
	  (return rest-list)
	  (return (destructive-push (list context) rest-list))))))

(defun insert-object (object rest-list)
  (do* ((rest-list rest-list (cdr rest-list))
	(head (car rest-list) (car rest-list)))
       ((null (cdr rest-list))
	(setf (cdr rest-list) (list object)))
    (cond ((eq head object) (return rest-list))
	  ((or (not (symbolp head)) (string< object head)) 
	   (return (destructive-push object rest-list))))))


(context basics)

(defutil for-each
  (form-type macro)
  (keywords iteration)
  (mhelp "(FOR-EACH mapfn varlist list1 ... listn form1 ...)
is an iteration macro which applied mapfn (if omitted MAP) to
list1 ... listn, binding in turn each variable in varlist, then
executing form1 ...  It is roughly equivalent to
(mapfn #'(lambda varlist form1 ...) list1 ... listn)"))


(defutil set-of
  (form-type macro)
  (keywords iteration)
  (mhelp "(SET-OF var list form1 ... formn)
will take list and build a new list, in which every element which does not
satisfy form1 ... formn will be deleted.  E. g.
(SET-OF X '(0 1 -1 2 1 -2) (> X 0)) -> (0 1 2 1)"))


(defutil copy
  (form-type macro)
  (keywords list)
  (mhelp "(COPY sexpr) will recursively copy the whole sexpr.  For something
less dramatic see also COPY-LIST."))


(defutil %catch%
  (form-type macro)
  (keywords control)
  (mhelp "This is the old UCI-Lisp CATCH.
See the UCI-Lisp Manual for documentation."))


(defutil %throw%
  (form-type macro)
  (keywords control)
  (mhelp "This is the old UCI-Lisp THROW.
See the UCI-Lisp Manual for documentation."))


(defutil throwfail
  (form-type macro)
  (keywords control)
  (mhelp "THROWFAIL is the canonical way of signalling errors in TPS.
The format is (THROWFAIL msg1 ... msgn) where msg1 ... msgn are
instructions for MSG.  See there."))



(defutil msg
  (form-type macro)
  (keywords output)
  (mhelp "MSG is the canonical way of producing text output, error or warning
messages etc.  It has the general form (MSG item1 ... itemn) where each item
can be one of the following forms:
T -> (TERPRI)
F -> (FRESH-LINE) ((TERPRI), but only of not at beginning of line)
THROW -> print (again using MSG) value of most recent THROW, usually THROWFAIL
(T n) -> (TAB n)
(TX n) -> (TABX n) (tabs without using <tab> characters)
(E form) -> evaluates form without printing the result
(L list) -> (PRINLC list) (print list without outermost parens)
(form . argtype) -> calls the printfn for argtype on form.  This is extremely
                    useful for wffs, lines, type symbols etc.
n, n>0 -> (SPACES n)
n, n<0 -> -n times (TERPRI)
otherwise -> (PRINC otherwise)
"))


(defutil msgf
  (form-type macro)
  (keywords output)
  (mhelp "(MSGF ...) expands to (MSG F ...).  It does a (FRESH-LINE) and then
calls MSG on the arguments."))

(defutil ttymsg
  (form-type macro)
  (keywords output)
  (mhelp "(TTYMSG msg1 ... msgn) will call (MSG msg1 ... msgn) after
making sure that the messages go to the terminal only."))

(defutil complain
  (form-type macro)
  (keywords output)
  (mhelp "COMPLAIN is the canonical way of announcing an error by the user.
(COMPLAIN msg1 ... msgn) will ring a bell at the terminal, and then call
(MSG msg1 ... msgn) after making sure that the messages go to the terminal 
only. "))

(defutil tps-warning
  (form-type macro)
  (keywords output)
  (mhelp "WARNING is the canonical way of warning the user.
(WARNING msg1 ... msgn) will call (MSG T \"Warning:  \" msg1 ... msgn) after
making sure that the messages go to the terminal only."))


(eval-when (eval compile load)
(defvar compiled-dir nil)
(defvar *source-files-only* nil)
(defvar *only-sources* nil)
(defvar *qload-compile* nil)
(defvar expertflag nil)
)
;; Looks at every directory in path, from left to right.

#+tops-20
(defun locate-file (filespec path source) 
  (do ((path path (cdr path))
       (probed-compiled-file
	nil
	(and (not source)
	     (probe-file
	      (merge-pathnames
	       filespec
	       (make-pathname% :directory (car path)
			       :type compiled-extension)))))
       (probed-source-file
	nil
	(probe-file
	 (merge-pathnames
	  filespec
	  (make-pathname% :directory (car path)
			  :type source-extension)))))
      ((or (null path)
	   probed-compiled-file probed-source-file)
       (cond ((and probed-compiled-file probed-source-file)
	      ;; Now we compare the times that the files were created.
	      ;;Load the uncompiled file if it was created before the
	      ;;corresponding compiled file, otherwise the LAP file.
	      (if (> (file-write-date probed-compiled-file)
		     (file-write-date probed-source-file))
		  probed-compiled-file  probed-source-file))
	     ((and probed-compiled-file (not source)) probed-compiled-file)
	     (probed-source-file probed-source-file)
	     (T nil)))))

(defvar *found-compiled-file* nil)

#-tops20
(defun locate-file (filespec path source) 
  (when (symbolp filespec)
    (setq filespec (string-downcase (symbol-name filespec))))
  (flet ((find-file (search-path file-type)
	   (dolist (dir search-path nil)
	     (let ((true-name
		    (probe-file
		     (merge-pathnames 
		      filespec
		      (if file-type
			  (make-pathname% :directory dir :type file-type)
			(make-pathname% :directory dir))))))
	       (when true-name (return true-name))))))
    (let* ((probed-source-file
	    (or (find-file path nil) (find-file path source-extension)))
	   (probed-compiled-file
	    (if source
		nil
	      (find-file 
	       (if probed-source-file
		   (list (directory-namestring probed-source-file)
			 compiled-dir)
		 (list compiled-dir))
	       compiled-extension))))
      (cond ((and probed-compiled-file probed-source-file)
	     (if (> (file-write-date probed-compiled-file)
		    (file-write-date probed-source-file))
		 (progn (setq *found-compiled-file* t)
			probed-compiled-file)
	       (progn (setq *found-compiled-file* nil)
		      probed-source-file)))
	    (probed-compiled-file
	     (setq *found-compiled-file* t)
	     probed-compiled-file)
	    (t 
	     (setq *found-compiled-file* nil)
	     probed-source-file)))))

(defvar smart-load nil)
(defvar building-finish-time nil)

(defun locate-tps-file (filespec source)
  ;; Looks at connected directory, home directory, then the source-path.
  (let ((true-name 
	 (locate-file filespec
		      `(nil		; Looks in the connected dir.
			;; take true name first, to remove any 
			;; logical pathname stuff
			;; unbelievably, the old cmu common lisp's truename 
			;; will error when given a directory.
			,(directory-namestring 
			  (#+(and :cmu (not :new-compiler)) identity
			   #-(and :cmu (not :new-compiler)) truename 
			   (user-homedir-pathname)))
					; Looks in the home directory.
			,@source-path)	; Looks in the predefined path.
		      source)))
    (cond ((not true-name)
 	   (format *error-output* "~&File ~A not found."
		   (namestring filespec))
	   (finish-output *error-output*))
	  ((and smart-load
		(> building-finish-time (file-write-date true-name)))
           (setq true-name nil)
 	   #+TPS
	   (format *error-output* 
		   "~&File ~A has not been changed since building TPS3."
		   (namestring filespec))
 	   #+ETPS
	   (format *error-output* 
		   "~&File ~A has not been changed since building ETPS."
		   (namestring filespec))
	   (finish-output *error-output*)))
    true-name))

(defun find-lisp-package (filename)
  (and (symbolp filename) 
       (module-package (get filename 'part-of))))

;;; This is always bound in qload depending on its arguments and expertflag

(defvar *allow-compile-source* t)

;;; If this is T, then whenever a source file is found, 
;;; and *allow-compile-source* is T, it will be compiled first without
;;; asking the user.
;;; E.g., set this to T in tps-build.lisp 

(defvar *always-compile-source-if-newer* nil)

(defun tload (filespec &key (verbose T) (print nil) (source nil))
  (let* ((source (if (or *only-sources*
		       (member filespec *source-files-only*
			       :test #'string-equal))
		   T 
		  source))
	 (true-name (locate-tps-file filespec source)))
    (cond ((not true-name))
	  (*found-compiled-file*
	   (load true-name :verbose verbose :print print))
	  ((and *allow-compile-source*
		(stringp (pathname-type true-name))
		(string-equal (pathname-type true-name) source-extension)
		(or *always-compile-source-if-newer*
		    (y-or-n-p "Compile file ~A?>" (namestring true-name))))
	   (cload-aux true-name verbose print))
	  (t (load true-name :verbose verbose :print print)))))

; was the following, but the new cmulisp objects... MB 1/17/94				   
;		    (y-or-n-p (format nil "Compile file ~A?>"
;				      (namestring true-name)))))


(defun qload (filespec &key (verbose T) (print nil) (source nil) (compile t))
  (when (symbolp filespec)
	(setq filespec (string-downcase (symbol-name filespec))))
  (let ((load-warn-p nil)
	(*allow-compile-source* (and expertflag compile)))
    (if (and *allow-compile-source* (not source))
	(let ((filetype (pathname-type filespec)))
	  (when filetype
	     (setq source 
		   (string-equal (string-downcase filetype) source-extension))
	     (setq *allow-compile-source* (not source)))))
    (tload filespec :verbose verbose :print print :source source)))


(defun cload (filespec &key (verbose T) (print nil))
  (let ((true-name (locate-tps-file filespec t)))
    (when true-name
      (cload-aux true-name verbose print nil))))


(defun cload-aux (true-name verbose print 
                            &optional (load-warn-p load-warn-p))
  (let ((output-file (make-pathname% :type compiled-extension
                                     :name (pathname-name true-name)
                                     :directory compiled-dir)))
    #+tops-20(compile-file true-name)
    #+:cmu(compile-file true-name :output-file (namestring output-file) 
                        :error-file nil)
    #-(or tops-20 :cmu)(compile-file true-name :output-file output-file)
    (load output-file :verbose verbose :print print)))

#+comment
(defun cload-module (module)
  (setq *qload-compile* nil)
  (dolist (np (module-needed-modules module))
    (if (module-p np)
	(load-module np)
	(throwfail np " is not a known module, but required for "
		   module ".")))
  (let ((current-context 'unclassified)
  	(*package*
	  (let ((fpack (module-package module)))
	    (unless fpack
	      (tps-warning "Lisp package for TPS module " module " not found." t
		       "Unless every file starts with (in-package :...) it will be loaded into "
		       (package-name *package*) "."))
	    (if fpack fpack *package*))))
    (declare (special current-context *package*))	
    (msgf "--- Compiling & Loading " module
	  " into Lisp package " (package-name *package*) " ---")
    (dolist (pf (append (module-macro-files module)
			(module-files module)))
      (if (member pf *source-files-only* :test #'string-equal)
	  (tload pf)
	  (cload pf)))
    (setf (module-loaded-p module) t)))
;(defunobsolete cload-package cload-module)

;;;CLOAD-MODULE in too conservative! Makefile is supposed to compile files
;;;only when it is necessary, but CLOAD-MODULE does not check for this at
;;;all. A very annoying feature of CLOAD-MODULE is that you have to start
;;;from the very beginning even when you have almost finished compiling all
;;;files but failed at a very last step, e.g., did not save the core image.
;;;The following CLOAD-MODULE can tackle this problem, but it is not highly
;;;trustworthy in general. There are some good examples of "true Makefile"
;;;which we could learn from. (HX March 30, 1994)

(defun cload-module (module)
  (let ((*always-compile-source-if-newer* T))
    (dolist (np (module-needed-modules module))
       (if (module-p np)
	   (load-module np)
	   (throwfail np " is not a known module, but required for "module ".")))
    (let ((current-context 'unclassified)
	  (*package* 
	   (let ((fpack (module-package module)))
	    (unless fpack
	      (tps-warning "Lisp package for TPS module " module " not found." t
			   "Unless every file starts with (in-package :...) it will be loaded into "(package-name *package*) "."))
	    (if fpack fpack *package*))))
      (declare (special current-context *package*))	
      (msgf "--- Compiling & Loading " module
	    " into Lisp package " (package-name *package*) " ---")
      (dolist (pf (append (module-macro-files module) (module-files module)))
	      (let ((*allow-compile-source*
		       (not (member pf *source-files-only* :test #'string-equal))))
		(tload pf)))
      (setf (module-loaded-p module) t))))
;(defunobsolete cload-package cload-module)

(defun cload-modules (modulelist)
  (dolist (module modulelist (reorganize))
    (cload-module module)))
;(defunobsolete cload-packages cload-modules)

;;; This is a counter, incremented each time user enters a top-level command.

(defvar *command-history-counter* 0)

(defvar auto-doc-flag t)

(context flags)
(defcategory review-subject
  (define defsubject)
  (properties (mhelp single))
  (global-list global-subjectlist)
  (mhelp-line "subject")
  (mhelp-fn princ-mhelp))

