;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
;;; Next line is commented out, since PART-OF is not yet defined.
;;;(part-of BOOTSTRAP)

;;; Taken from Tops20:
#+tops-20
(defmacro make-pathname% (&rest rest) `(make-pathname ,@rest))


;;; The value of the :directory keyword argument should always be a
;;; string -- see the file macsys for definition of the DIRSPEC argtype.
;;; Some Lisps, notably CMU, use the device slot of the pathname to
;;; determine whether the pathname is absolute or relative.  In
;;; addition, each Lisp has a different way of representing the
;;; directory argument slot, some as strings or vectors, others as
;;; lists.  This way of doing things should allow the user to just
;;; specify a directory as a string, and not worry about the Lisp they
;;; are using.  DAN 17MAY89

#-tops-20
(defun make-pathname% (&rest rest &key (directory nil)
				       (defaults
					*default-pathname-defaults*
					defaults-p) &allow-other-keys)
  (let ((dirpathname 
	  (if directory (pathname directory)))
	(other-keys 
	 (let ((tmp
		(if directory
		    (nconc (subseq rest 0 (position :directory rest))
			   (subseq rest (+ 2 (position :directory
						       rest))))
		  rest)))
	   (if defaults-p
	       (nconc (subseq tmp 0 (position :defaults tmp))
		      (subseq tmp (+ 2 (position :defaults
						 tmp))))
	     tmp))))
    (if dirpathname
	 (apply #'make-pathname 
		(append other-keys
			(list :directory (pathname-directory dirpathname)
			      :device (pathname-device dirpathname)
			      :defaults (if defaults-p defaults 
					  *default-pathname-defaults*))))
	 (apply #'make-pathname 
		(append other-keys
			(list :defaults (if defaults-p defaults 
					  *default-pathname-defaults*)))))))

;;; In Lucid 4.0, ACONS generates a run-time error when its third argument
;;; is not a list, so we redefine ACONS here to do what is expected.

#+:lcl4.0
(defun acons (x y z)
  "(ACONS X Y Z) is equivalent to (CONS (CONS X Y) Z)."
  (cons (cons x y) z))

(defmacro defconstnt (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))


(defun accessible-p (symbol &optional (package *package*))
  "Returns NIL, :internal, :external, or :inherited like
 INTERN or FIND-SYMBOL.  It looks for a symbol with the print-name of symbol."
  (multiple-value-bind (found-symbol status)
		       (find-symbol (symbol-name symbol) package)
    (and (eq found-symbol symbol) status)))

(defun shadow-copy (symbols package)
  "Works like SHADOW, but requires the package and will copy property
  list, function, and value associated with symbol."
  (shadow symbols package)
  (dolist (sym symbols)
    (let ((new-sym (intern (symbol-name sym) package)))
      (setf (symbol-plist new-sym) (copy-list (symbol-plist sym)))
      (when (fboundp sym)
	(setf (symbol-function new-sym) (symbol-function sym)))
      (when (boundp sym)
	(setf (symbol-value new-sym) (symbol-value sym)))))
  t)

;;;  contains some functions, mostly macros, which emulate some MacLisp
;;; functions in Common Lisp.  All functions in here are independent of
;;; the particular implementation of Common Lisp.

(eval-when (compile load eval)
(defun compile-warn-obsolete (fun)
  (format *error-output* "~&WARNING: ~S is an obsolete function or macro."
	  fun))
)

#-(or ibcl kcl :allegro :excl)
(defmacro putprop (symbol value property)
  `(setf (get ,symbol ,property) ,value))

;;For some reason the next thing is predifned by the following Lisps

#-(or tops-20 lucid :cmu kcl ibcl :allegro :excl)
(defmacro memq (elt list) `(member ,elt ,list :test #'eq))

#+:allegro-v4.3
(defmacro memq (elt list) `(member ,elt ,list :test #'eq))

#+(and allegro-version>= (version>= 5 0))
(defun memq (elt list) (member elt list :test #'eq))
;;; memq must be exported in core.exp

; ceb 5/26/2004 - clisp complains about common-lisp package being locked unless we unlock it
; NOTE: clisp means Gnu Common Lisp (http://clisp.cons.org/)
#+:clisp
(setf (ext:package-lock :common-lisp) nil)

; mkaminski 9/13/2005 - clisp already has PRIORITY and KILL in cl-user
#+:clisp
(progn
  (shadowing-import 'maint:priority :cl-user)
  (shadowing-import 'auto:kill :cl-user))

;;; Changed THROW to EXPAND-CATCH-THROW to get a more unique name for
;;; the variable which will be bound to the result of form. DAN 13SEP89

(defmacro %catch% (form &rest tagcases)
  "Emulates the UCI-Lisp catch special form.  This was called CATCH in
   our version of MacLisp.  An important detail is that we have to locally
   bind the variable EXPAND-CATCH-THROW in the tagcases.  If tagcases is a
   symbol, it simply returns the value of the throw."
  (cond ((symbolp (car tagcases)) `(catch ',(car tagcases) ,form))
	((cdr tagcases)
	 (let ((xxx `(%catch% ,form ,@(cdr tagcases)))
	       (tagcases (car tagcases)))
	   `(expand-%catch% ,xxx ,tagcases)))
	(t `(expand-%catch% ,form ,(car tagcases)))))


;;; Got tired of compiler always complaining "THROW bound but not
;;; referenced."  So modified to add an IGNORE declaration if necessary.
;;; Now instead of using the variable THROW to hold the result of
;;; evaluating FORM, use EXPAND-CATCH-THROW to get a more unique value.
;;; FOUND-EXPAND-CATCH-THROW-INSIDE checks the tagcases to make sure that 
;;; the variable EXPAND-CATCH-THROW is not being referenced.  DAN 13SEP89

(defmacro expand-%catch% (form tagcases)
  (let ((tag (car tagcases))
	(tagforms (cdr tagcases))
	(result-var (gensym))
	(special-value (gensym)))
    `(let* ((,result-var ',special-value)
	    (expand-catch-throw
	     (catch ',tag
	       (setq ,result-var 
		     (multiple-value-list ,form)))))
       ,@(unless (found-expand-catch-throw-inside tagforms)
           '((declare (ignore expand-catch-throw))))
       (if (eq  ,result-var ',special-value)
	   (progn ,@tagforms)
	 (values-list ,result-var)))))


(defun found-expand-catch-throw-inside (forms)
  (not (tree-equal forms (subst nil 'expand-catch-throw forms))))

(defmacro plist (symbol) `(symbol-plist ,symbol))

(defmacro fixp (num) `(integerp ,num))

(defmacro pairp (object) `(consp ,object))

(defun status-date ()
  (multiple-value-bind (second minute hour date month year
			       day-of-week daylight-saving-time-p time-zone)
		       (get-decoded-time)
    (declare (ignore second minute hour day-of-week
		     daylight-saving-time-p time-zone))
;    (list (- year 1900) month date))) ; remember in the year 3000 to change this to 3000
    (list (- year 2000) month date)))

(defun status-daytime ()
  (multiple-value-bind (second minute hour date month year
			       day-of-week daylight-saving-time-p time-zone)
		       (get-decoded-time)
    (declare (ignore date month year day-of-week daylight-saving-time-p
		     time-zone))
    (list hour minute second)))


(defmacro conc-strings (&rest strings)
  `(concatenate 'string ,@strings))

(eval-when (load compile eval)
(defun conc-stringlist (stringlist)
  (apply #'concatenate 'string stringlist))
)


;;; Redefined to allow optional package specification -- DAN 2-5-88
(eval-when (load compile eval)
(defun conc-names (name &rest more-names)
  "Will make strings of its arguments, concatenate them, and intern
a symbol whose name is the concatenation.  The package in which the
symbol will be interned can be specified by using a :package keyword
argument, e.g., (conc-names 'foo 'bar :package (find-package \"USER\")).
The default package is the current package."
  (let ((pack (cadr (member ':package more-names :test #'eq))))
    (intern (conc-stringlist
	      (mapcar #'(lambda (symbol)
			  (if (symbolp symbol) (symbol-name symbol)
			    (string symbol)))
		      (if pack
			  ;; Remove the keyword and its follower
			  ;; from the list
			  (let ((l (cons name more-names)))
			    (delete ':package (delete pack l)))
			(cons name more-names))))
	    (or pack *package*))))
)

(defun collapse-face (facelist)
  (apply #'conc-names facelist))

(defmacro prepend (&rest names)
  `(conc-names ,@names))


(defun dsubst (to from in)
  (cond ((null in) nil)
	((atom in)
	 (if (eq from in) to in))
	((eq from (car in))
	 (cond ((eq from (cdr in))
		(rplacd in to))
	       (t (dsubst to from (cdr in))
		  in))
	 (rplaca in to))
	((eq from (cdr in))
	 (dsubst to from (car in))
	 (rplacd in to))
	(t (dsubst to from (car in))
	   (dsubst to from (cdr in))
	   in)))


(eval-when
  (compile load eval)
  (defun expand-fe (form)
    (cons (cond ((eq (car form) 'map)
		 ;; MAP is special, because it's called MAPL in Common Lisp.
		 (prog1 'mapl (setq form (cdr form))))
		((memq (car form) '(mapc mapcan mapcar mapcon
					 mapconc maplist))
		 (prog1 (car form) (setq form (cdr form))))
		(t 'mapc))
	  (let ((vars (car form))
		body)
	    (cond ((atom vars) (setq vars (list vars))))
	    (setq body (nthcdr (1- (length vars)) (cddr form)))
	    `(#'(lambda ,vars ,@body) ,@(ldiff (cdr form) body)))))
  )

(eval-when (compile load eval)
(defun expand-ex (fn form)
  (let ((vars (cond ((atom (car form)) (list (car form)))(T (car form))))
	(predicate (car (last form)))
	(args (butlast (cdr form))))
    `(,fn #'(lambda ,vars ,predicate) ,@args)))
)

(eval-when (compile load eval)
(defun expand-set-of (form)
  (let ((vars (cond ((atom (car form)) (list (car form))) (T (car form))))
	body)
    (setq form (cdr form))
    (setq body (nthcdr (1- (length vars)) (cdr form)))
    `(mapcan #'(lambda ,vars (cond (,(car body) (list ,(car vars)))))
	     ,@(ldiff form body))))
)

(defmacro for-each (&rest l) (expand-fe l)) 

(defmacro exists (&rest l) (expand-ex 'some l)) 

(defmacro forall (&rest l) (expand-ex 'every l)) 

(defmacro set-of (&rest l) (expand-set-of l)) 

(defmacro copy (l) `(copy-tree ,l))

;; This function starts counting from 0.

(defun tab (location) (format t "~vT" location))

(defmacro tabx (location) `(tab ,location))

;;; TABX is like TAB, but does not put out any <Tab> (^I) characters.
;;; This is useful for Scribe output.

(defmacro spaces (n) `(dotimes (i ,n (values)) (princ " ")))

(defmacro neq (first second) `(not (eq ,first ,second)))

(defmacro %throw% (&rest l)
  `(throw ',(cadr l) ,(car l)))

(defmacro throwfail (&rest l)
  `(throw 'fail
	  (list ,@(mapcar #'(lambda (item)
			      (cond ((stringp item) `',item)
				    ((and (consp item) (atom (cdr item))
					   (cdr item))
				     `(cons (list 'quote ,(car item)) 
					    ',(cdr item))) 
				    ((eq item t))
				    (t `(list 'quote ,item))))
			  l))))

;;; Changed THROW to EXPAND-CATCH-THROW everywhere here. DAN 13SEP89

(defmacro throwfail-on (&rest l)
  (let* ((throw-tail (member 'expand-catch-throw l))
	 (before-throw (ldiff l throw-tail)))
    `(eval (cons 'throwfail (append ',before-throw
				    (if (consp expand-catch-throw) 
                                        expand-catch-throw 
                                        (list expand-catch-throw))
				    ',(cdr throw-tail))))))

(defun prinlc (list) (format T "~{~S ~}" list))

(defmacro flatc (sexpr)
  `(length (format nil "~A" ,sexpr))) ;;`(length (string ,sexpr))

(deftype tps-string-char () 'character)

(defmacro tps-ascii (int) `(code-char ,int))

;;; Preserves order of elements in s1 9/14/87 DAN

(defun setdiff (s1 s2)
  (dolist (elt s2 s1)
    (setq s1 (remove elt s1))))

(defmacro setintersect (s1 s2) `(intersection ,s1 ,s2))

(defmacro max-with-key (list key)
  `(when ,list
	 (do ((l (cdr ,list) (cdr l))
	      (max* (funcall ,key (car ,list))
		    (max max* (funcall ,key (car l)))))
	     ((null l) max*))))

(defmacro apply-fn-with-key (fn list key)
  `(when ,list
     (do ((l (cdr ,list) (cdr l))
	  (elt (funcall ,key (car ,list))
	       (funcall ,fn elt (funcall ,key (car l)))))
	 ((null l) elt))))

(defun alphalessp (name1 name2)
  (string< name1 name2))

;;; tyo does not allow an optional output-stream for now.
;;; This is done for efficiencies sake and also to catch
;;; occasions where we used IMAGE-FILE-OBJECT.

(defun tyo (int) (write-char (code-char int)))

(eval-when (load compile eval)
(defmacro msg (&rest mssg)
  (do ((mssg-tail  mssg (cdr mssg-tail))
       (forms (list 'progn))
       (item))
      ((atom mssg-tail) (nreverse (cons '(finish-output) forms)))
    (setq item (car mssg-tail))
    (push (cond ((integerp item)
		 (cond ((> item 0) `(spaces ,item))
		       ((< item 0)			
			`(do ((n ,item (1+ n)))
			     ((zerop n))
			   (terpri)))))
		((eq item 't) `(terpri))
		((eq item 'f) `(fresh-line))
		((eq item 'expand-catch-throw) 
                 `(eval (cons 'msg expand-catch-throw)))
		((atom item) `(princ ,item))
		((eq (car item) 't) `(tab ,(cadr item)))
		((eq (car item) 'tx) `(tabx ,(cadr item)))
		((eq (car item) 'e) (cadr item))
		((eq (car item) 'l) `(prinlc ,(cadr item)))
		((and (atom (cdr item)) (cdr item))
		 (cond ((get (cdr item) 'printfn)
			`(,(get (cdr item) 'printfn) ,(car item)))
		       (t
			(error "Illegal argument type in MSG instructions"))))
		(t `(princ ,item)))
	  forms)))
)

;;; Changed the following not to rebind *standard-output*.  Always
;;; want such error messages to go to current value of
;;; *standard-output*. DAN 11-4-89

(defmacro ttymsg (&rest mssg)
  `(msg ,@mssg))

(defmacro complain (&rest l)
  `(msg F ,@l))

(defmacro tps-warning (&rest l) `(complain t "WARNING:  " ,@l))

(defmacro msgf (&rest mssg)
  `(msg f ,@mssg))



;;;
;;; File: Categ
;;; Author: fp
;;;
;;; This file allows to define categories of commands or definitions.

;;;
;;; GLOBAL-CATEGORYLIST is the global list of all categories, in the reverse
;;; order they were defined.
;;;

(eval-when (load compile eval)
(defvar global-categorylist nil)
;(defvar global-contextlist nil)
(defvar current-context nil)

(defvar load-warn-p nil) ;was T ... MB Wed Apr  9 15:44:05 1997

(defvar global-definelist
  '((defun . function)
    (defmacro . macro)
    (defvar . variable)
    (defparameter . parameter)
    (defconstnt . constant)
    (defcategory . category)
    ))
)
(eval-when (compile load eval)
(defun prop-error (prop-name prop-value symbol)
  (complain "Error while defining " symbol ":" t prop-name
	    " (suggested value: " prop-value
			 ") is not a legal attribute.")))

(eval-when (compile load eval)
(defun define-properties (name proppairlist category)
  (declare (special global-contextlist))
    (if (get name category)
	(when load-warn-p
	  (complain "WARNING: Redefining " name ", a "
		    (get category 'mhelp-line) "."))
	(let ((cats (remove-if-not #'(lambda (cat)
				       (and (get name cat) (neq cat 'context)))
				       global-categorylist)))
	  (if (member name global-contextlist) (push 'context cats))
	  (push category cats)
	  (when (and (cdr cats) (forall cat cats (get cat 'shadow)) load-warn-p)
	    (complain f "WARNING: Simultaneously defining " name
		      " as " (l cats) "."))))
    (putprop name 't category)
    (let ((context (assoc category (get name 'contexts))))
      (if context
	  (setf (cdr context) current-context)
	(push (cons category current-context) (get name 'contexts))))
    (let ((category-read-fns (get category 'category-read-fns)))
    (dolist (property proppairlist name)
      (let ((prop-name (car property))
	    (prop-value (cdr property)))
	(if (eq prop-name 'mhelp)
	    (read-mhelp name prop-value category)
	    (let ((digest-fn (cdr (assoc prop-name category-read-fns))))
	      (cond ((null digest-fn)
		     (putprop name (funcall (get category 'other-prop)
					    prop-name prop-value name)
			      prop-name))
		    ((or (and (symbolp digest-fn) (not (fboundp digest-fn)))
			 (and (not (symbolp digest-fn))
			      (not (functionp digest-fn))))
		     (complain "ERROR: Digest function " digest-fn
			       " for category " category " property " prop-name
			       " is not a legal function."))
		    (t (putprop name (funcall digest-fn prop-value name)
				prop-name))))))))))


;;; Specify properties of the category that should be read as functions by
;;; singlefn and multiplefns.  This will ensure that they get compiled
;;; when the file is compiled.  This is done by redefining the define
;;; macro for the category to check for these guys. 
;;; This will necessitate some changes in the current category definitions.
;;; 27MAR91 DAN

(defmacro defcategory (category &rest forms)
 (let* ((define (cadr (assoc 'define forms)))
	(properties (cdr (assoc 'properties forms)))
	(other-prop-fn-name (conc-names category "--OTHER-PROP"))
	(other-prop (read-n-arg-fnspec
		      (or (cdr (assoc 'other-prop forms))
			(list 'prop-error))
		      (list 'prop-name 'prop-value category)
		      other-prop-fn-name))
	(global-list (cadr (assoc 'global-list forms)))
	(shadow (cadr (assoc 'shadow forms)))
	(mhelp-line (cadr (assoc 'mhelp-line forms)))
	(mhelp-fn-name (conc-names category "--MHELP-FN"))	
	(mhelp-fn (read-n-arg-fnspec (cdr (assoc 'mhelp-fn forms))
				     (list category 'category)
				     (conc-names category 
						 "--MHELP-FN")))
	(cat-help (cdr (assoc 'cat-help forms)))
	;; scribe-one-fn should be a function of one arg, an item of
	;; the category, and should format that item's help for scribe,
	;; suitable for facilities guide
	(scribe-one-fn-name (conc-names category "--SCRIBE-ONE-FN"))
	(scribe-one-fn (read-n-arg-fnspec (cdr (assoc 'scribe-one-fn forms))
					  (list category 'category)
					  (conc-names category 
						      "--SCRIBE-ONE-FN")))
	(putprops `((pushnew ',category global-categorylist)
		    (putprop ',category ',define 'define)
		    (putprop ',category ',(mapcar #'car properties)
			     'properties)
		    (putprop ',category
			     ',(if (or (compiled-function-p other-prop)
				       (symbolp other-prop))
				   other-prop
				   other-prop-fn-name)
			     'other-prop)
		    (putprop ',category ',global-list 'global-list)
		    (putprop ',category ',mhelp-line 'mhelp-line)
		    (putprop ',category ',shadow 'shadow)
		    (putprop ',category
			     ',(if (or (compiled-function-p mhelp-fn)
				       (symbolp mhelp-fn))
				   mhelp-fn mhelp-fn-name)
			     'mhelp-fn)
		    (putprop ',category
			     ',(if (or (compiled-function-p scribe-one-fn)
				       (symbolp scribe-one-fn))
				   scribe-one-fn
				   scribe-one-fn-name)
			     'scribe-one-fn)
		    (putprop ',category ',cat-help 'cat-help)))
	(read-fns nil)
	(defun-list (do* ((x 
			    (delete-if #'null 
				       (list scribe-one-fn 
					     mhelp-fn other-prop))
			     (cdr x))
			  (y (car x) (car x))
			  (defun-list nil))
			 ((null y) defun-list)
			 (unless (or (compiled-function-p y) 
				     (symbolp y))
			   (push y defun-list)))))
      (dolist (property properties)
	(let* ((prop-name (car property))
	       (car-read-fn (cadr property))
	       (read-fn-name (conc-names category "--" prop-name))
	       (read-fn
		 (if (atom car-read-fn)
		     (cond ((eq car-read-fn 'single) 'read-single)
			   ((eq car-read-fn 'multiple) 'read-multiple)
			   ((get car-read-fn 'argtype)
			    `(defun ,read-fn-name (,prop-name ,category)
			       (%catch%
				 (,(get car-read-fn 'getfn)
				  (car ,prop-name))
				 (fail (complain f "Error in reading "
						 ,category ", a "
						 ,mhelp-line
						 ":" t expand-catch-throw)
				       nil))))
			   (t car-read-fn))
		     (read-n-arg-fnspec
		       (cdr property)
		       (list prop-name category)
		       read-fn-name))))
	  (unless (or (compiled-function-p read-fn)
		      (symbolp read-fn))
	    (push read-fn defun-list))
	  (push
	   (cons prop-name 
		 (if (or (compiled-function-p read-fn)
			       (symbolp read-fn))
			   read-fn
			   read-fn-name))
	   read-fns)))
      `(progn
	,@(if global-list `((defvar ,global-list nil)) nil)
	(pushnew (cons ',define ',category) global-definelist)
	(defmacro ,define (tps-object &rest prop-list)
	  (multiple-value-bind (fn-props non-fn-props)
	      (do ((prop-list (cdr prop-list) (cdr prop-list))
		   (fn-props nil)
		   (non-fn-props nil)
		   (prop (car prop-list) (car prop-list)))
		  ((null prop) (values fn-props non-fn-props))
		(if (member (cdr (assoc (car prop)
					(get ',category 'category-read-fns)))
			    '(multiplefns singlefn))
		    (push prop fn-props)
		  (push prop non-fn-props)))
	    (let ((fn-prop-defs 
		   (define-fn-props tps-object fn-props ',category)))
	  `(progn
	     (pushnew ',tps-object ,',global-list)
	     ,@fn-prop-defs
	     (define-properties ',tps-object
				',non-fn-props ',',category)))))
	,@defun-list
	,@putprops
	(setf (get ',category 'category-read-fns) ',read-fns)
	',category)))


(eval-when (eval load compile)
(defun define-fn-props (object props category)
  (let ((defun-list nil)
	(putprops nil))
    (dolist (prop props)
      (multiple-value-bind (new-defuns new-putprop)
	(read-fn-prop object prop category)
      (setq defun-list (append defun-list new-defuns))
      (push new-putprop putprops)))
    (append defun-list putprops)))


(defun read-fn-prop (object prop category)
  (let ((new-defuns nil)
	(new-putprop nil))
    (case (cdr (assoc (car prop) (get category 'category-read-fns)))
      (singlefn
       (let* ((fn-name 
	      (conc-names object "_" category "_" (car prop)))
	      (new-defun
	       (list (read-n-arg-fnspec (cdr prop) nil fn-name))))
	 (unless (symbolp (car new-defun))
	   (setq new-defuns new-defun))
	 (setq new-putprop `(setf (get ',object ',(car prop))
		  ',(if (symbolp (car new-defun)) (car new-defun)
		     fn-name)))))
      (multiplefns
       (let ((n 0) (putprop-list nil))
	 (dolist (defn (cdr prop)
		   (setq new-putprop 
		     `(setf (get ',object ',(car prop))
			',(nreverse putprop-list))))
	   (incf n)
	   (let* ((new-defn-name 
		   (conc-names object "_" category "_" (car prop) 
			       (princ-to-string n)))
		  (new-defun
		   (read-n-arg-fnspec (list defn) nil new-defn-name)))
	     (if (not (symbolp new-defun))
		 (progn 
		   (push new-defun new-defuns)
		   (push new-defn-name putprop-list))
	       (push new-defun putprop-list))))))
      (otherwise (error "Bad val in READ-FN-PROP")))
    (values new-defuns new-putprop)))
)

(eval-when (load compile eval)
(defun read-mhelp (symbol prop-value category)
  (let ((current-mhelp (assoc category (get symbol 'mhelp))))
    (if (consp current-mhelp)
	(setf (cdr current-mhelp) (car prop-value))
      (push (cons category (car prop-value)) (get symbol 'mhelp)))))


(defun read-single (prop-value category)
  (declare (ignore category))
  (car prop-value))



(defun read-multiple (prop-value category)
    (declare (ignore category))
    prop-value)

(defun read-multiple-and-export (prop-value category)
  (declare (ignore category))
  (if (listp prop-value)
      (dolist (whatever prop-value)
	      (if (not (find-symbol (symbol-name whatever)))
		  (export whatever *package*)))
    (if (not (find-symbol (symbol-name prop-value)))
	(export prop-value *package*)))
  prop-value)
)


;;; Changed fun below to check for a function explicitly with functionp
;;; fp. 86/6/8.
;;; Function should return one of two things: a symbol, or a function
;;; which can be used in a funcall, i.e.,
;;; (funcall (read-n-arg-fnspec ...) ...) should work.


;;; Redefined 14FEB89.  Will either return a symbol, if fnspec already
;;; exists, else a defun form.  FN-NAME should be a symbol.
;;; DAN
(eval-when (load compile eval)
(defun read-n-arg-fnspec (fnspec lambda-varlist &optional fn-name)
  (when fnspec
    (let ((fn (cond ((symbolp (car fnspec))
		     (car fnspec))
		    ((and (listp (car fnspec))
			  (eq (caar fnspec) 'lambda))
		     (car fnspec))
		    ;; a function, but not a lambda-exp or symbol
		    ((functionp (car fnspec))
		     (car fnspec))
		    (t
		      (cons 'lambda (cons lambda-varlist fnspec))))))
      (if (or (symbolp fn) (compiled-function-p fn))
	  fn
	  `(defun ,fn-name ,@(cdr fn))))))



(defun get-fnspec (fnspec lambda-var &optional fn-name)
  (read-n-arg-fnspec fnspec (list lambda-var) fn-name))


(defun getargnames (fn-name)
  (cond ((get fn-name 'argnames))
	((get fn-name 'alias) (get (get fn-name 'alias) 'argnames))
	(t nil)))
)

;;; The help message for CATEGORY.

(putprop 'category "Categories are classes of TPS objects." 'cat-help)

;;;
;;; The definition of the category COMMAND follows.  For historical reasons
;;; the definition function is called DEFMEXPR.
;;;

(defcategory mexpr
  (define defmexpr)
  (properties
   (argtypes multiple)
   (wffargtypes (declare (ignore mexpr))
		(funcall (get 'typesymlist-nil 'getfn) wffargtypes))
   (wffop-typelist (declare (ignore mexpr))
		   (funcall (get 'typesymlist 'getfn) wffop-typelist))
   (argnames multiple)
   (arghelp multiple)
   (defaultfns multiplefns)
   (mainfns multiplefns)
   (enterfns multiplefns)
   (closefns multiplefns)
   (print-command single)
   (dont-restore single)
   (mhelp single))
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command 
       (format nil "@IndexMexpr(~A)" (symbol-name item))
       (get item 'argnames)
       (cdr (assoc 'mexpr (get item 'mhelp))))))
  (global-list global-mexprlist)
  (shadow t)
  (mhelp-line "top-level command")
  (mhelp-fn (command-mhelp mexpr '"<n>" category)))

;;;
;;; The category of top-level is also defined here.
;;;

(defcategory toplevel
  (define deftoplevel)
  (properties
   (top-prompt-fn singlefn)
   (command-interpreter singlefn)
   (print-* singlefn)
   (top-level-category singlefn)
   (top-level-ctree singlefn)
   (top-cmd-interpret multiplefns)
   (top-cmd-decode singlefn)
   (mhelp single))
  (global-list global-toplevellist)
  (mhelp-line "top level")
  (mhelp-fn princ-mhelp))


;;; 
;;; The category of utilities provides organization and accessibility
;;; for general-purpose functions and macros.  cpk
;;;

(defcategory utility
  (define defutil)
  (properties
   (form-type single)
   (keywords multiple)
   (mhelp single))
  (global-list global-utilist)
  (mhelp-line "utility")
  (mhelp-fn princ-mhelp))

;info is used to provide help messages for flag settings and other junk. MB

(defcategory info
  (define definfo)
  (properties
   (mhelp single))
  (global-list global-infolist)
  (mhelp-line "flag setting or other piece of information")
  (mhelp-fn princ-mhelp))

(setq current-context 'unclassified)

(defun context-p (context)
  (and (symbolp context) (get context 'context)))


(defmacro context (context)
  ;; The test COULD be done at compile time, right now
  ;; is done at load time.
  `(progn (when (not (context-p ',context))
	    (complain "TPS-WARNING: " ',context " is not a defined context."))
	  (setq current-context ',context)))

;;; Added order property 8/29/87 DAN


(defcategory context
  (define defcontext)
  (properties
   (short-id single)
   (order single)
   (mhelp single))
  (global-list global-contextlist)
  (mhelp-line "context")
  (mhelp-fn princ-mhelp))

; cebrown 5/3/02 - created new categories for menus - these are used for the Java interface
(defcategory menu
  (define defmenu)
  (properties
   (display-name single)
   (placement single)
   (parent single) ; the menu on which this should appear as a sub menu
					; this may also be the name of a top level
					; indicating the menu should only show
					; when in that top level
   (remote-expert single) ; if this is T, only show this menu option for remote experts
					; (or when running locally)
   (mhelp single))
  (global-list global-menulist)
  (mhelp-line "Menu for the User Interface")
  (mhelp-fn princ-mhelp))

(defcategory menuitem
  (define defmenuitem)
  (properties
   (display-name single)
   (command single)
   (hotkey single) ; NIL or a character
   (placement single)
   (parent single) ; the menu on which the item should appear
   (remote-expert single) ; if this is T, only show this menu option for remote experts
					; (or when running locally)
   (etps single)
   (mhelp single))
  (global-list global-menuitemlist)
  (mhelp-line "Menu Item for the User Interface")
  (mhelp-fn princ-mhelp))

(defcategory tps-file
  (define deffile1)
  (properties
   (tps-import multiple)
   (tps-export multiple)
   (extension single)
   (part-of single)
   (mhelp single))
  (global-list global-filelist)
  (mhelp-line "file")
  (mhelp-fn mhelp-file)
  (cat-help "The category of TPS files."))

;;; Redefining DEFFILE in its final form.

(push (cons 'deffile 'tps-file) global-definelist)

(defmacro deffile (file-name &rest props)
  `(let ((current-context ',(case (cadr (assoc 'extension props))
			      (lsp 'lisp-source)
			      (lisp 'lisp-source)
			      (clisp 'lisp-source)
			      ((pcl dcl) 'command-declaration)
			      ((lib mss) 'documentation)
			      (ctl 'batch)
			      (ind 'indirect)
			      (exe 'core-image)
			      ((news note) 'system-news)
			      (t 'lisp-source))))
     (if (not (find-symbol (symbol-name ',file-name)))
	 (export ',file-name *package*))
     (deffile1 ,file-name ,@props)))

(defvar *user* (find-package "CL-USER"))

(deffile boot0
  (part-of BOOTSTRAP)
  (extension lisp)
  (mhelp "Defines categories, mexprs and various other essential stuff."))

(defun new-filename ()
  (symbol-name (gensym)))

(defun new-tmp-filename ()
  (concatenate 'string "/tmp/" (new-filename)))

(defun always-true (&rest ignore)
  "Always returns T. Takes any number of args."
  (declare (ignore ignore))
  t)

(defun always-nil (&rest ignore)
  "Always returns NIL.  Takes any number of args."
  (declare (ignore ignore))
  nil)

;;; Following is to allow functions to be defined to auto load when
;;; used.  DAN 27SEP89

(defmacro defautoloadfn (fn-name filename)
  "Define a function to be auto-loading. FN-NAME should be the
function's name; FILENAME (a string) is the name of the file to be loaded when
FN-NAME is first called."
  `(defun ,fn-name (&rest args)
     (qload ,filename :verbose expertflag)
     (apply #',fn-name args)))


#+(and :excl (not :allegro)) 
; excl doesn't expand this properly when it has declarations DAN
(defmacro lisp:with-open-file (open-form &rest forms)
  (let ((decls (if (eq 'declare (caar forms)) (car forms))))
  `(let ((,(car open-form) (open ,@(cdr open-form))))
     ,decls
     (unwind-protect
       (let nil
         ,@(if decls (cdr forms) forms))
       (when (streamp ,(car open-form)) (close ,(car open-form)))))))


(defmacro gaps nil
  `(get dproof 'gaps))

(defmacro nextplan-no (proof)
  `(get ,proof 'nextplan-no))


;;; Following macros added 7/19/87 DAN
(defmacro line-hypotheses (line)
  `(get ,line 'hypotheses))

(defmacro line-justification (line)
  `(get ,line 'justification))

(defmacro line-just-rule (line)
  `(car (get ,line 'justification)))

(defmacro line-just-terms (line)
  `(cadr (get ,line 'justification)))

(defmacro line-just-lines (line)
  `(caddr (get ,line 'justification)))

(defmacro line-assertion (line)
  `(get ,line 'assertion))

(defmacro line-support (line)
  `(get ,line 'support))

(defmacro line-linenumber (label)
  `(get ,label 'linenumber))

(defmacro proof-lines (proof)
  `(get ,proof 'lines))

(defmacro proof-plans (proof)
  `(get ,proof 'plans))

(defmacro proof-linealiases (proof)
  `(get ,proof 'linealiases))

(defmacro proof-key (proof)
  `(get ,proof 'key))

(defmacro proof-assertion (proof)
  `(get ,proof 'assertion))

(defmacro proof-nodealiases (proof)
  `(get ,proof 'nodealiases))

(defmacro line-mating (label)
  `(get ,label 'mating))

(defmacro line-node (label)
  `(get ,label 'node))

;copied temporarily into boot0.lisp to stop cmucl from complaining:
;Warning: Defining LINE-HYPOTHESES to be a macro when it was assumed to be a function.
;Warning: Defining LINE-ASSERTION to be a macro when it was assumed to be a function.
;Warning: Defining PROOF-PLANS to be a macro when it was assumed to be a function
;when it reaches otl-macros.lisp
;
;should eventually work out where these ought to be defined... MB Mon Jul  7 17:05:37 1997
