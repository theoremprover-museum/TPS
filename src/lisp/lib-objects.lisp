;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(context  miscellaneous)

;;;
;;; File: LIB-objects
;;; Package: library
;;;

(deffile lib-objects
  (part-of library)
  (extension lisp)
  (mhelp "Functions to handle various TYPES of objects to be stored
    in the library."))

;;GWFF
(deflibobject gwff
  (lib-promptfn lib-promptfn-gwff)
  (lib-descr-readfn libparse-gwff)
  (lib-tpsobject get-lib-cw)
  (lib-printfn libprint-gwff)
  (mhelp "Gwff"))

(defun lib-promptfn-gwff (name type help file modify comment old-value)
  (let* ((label nil)
	 (gwff (if old-value (libitem-description old-value)
		   (if (label-q name)
		       (%catch%
                        (progn
                          (setq label (apply-label
                                       name
                                       (lib-promptfn-gwff
                                        name type help file modify comment
                                        old-value)))
                          name)
                        (fail '$))
		       '$)))
	 description)
    (prompt-read description nil (msgf "Gwff to save? ") 'gwff gwff
		 ((? (msgf "the wff to be saved."))
		  (?? (mhelp 'gwff))))
    (values (or label description) 'unclassified nil)))

(defun libprint-gwff (gwff libstyle)
  (if (eq libstyle 'write-to-file)
	(in-mode re-read (msg (gwff . gwff)))
    (if (eq style 'tex-1)
	(let ((displaywff t) ; highly misleading - the wff's are not actually put in display more
	      (atomvalflag t)
	      (ppwfflag t)
	      (infix-notation t)) ; added cebrown 3/12/03 since ppwfflag t only makes since with infix-notation t
	  (pwff-lib gwff))
      (msg (gwff . gwff)))))

(defun libparse-gwff (gwff)
  (gettype 'gwff gwff))

(defun get-lib-cw (libitem)
  (let ((name (libitem-name libitem)))
    (when (and (label-q name) (get-weak name) load-warn-p)
	  (complain "Redefining weak label: " name))
    (create-weak name (libitem-description libitem))))

;;ABBR

(deflibobject abbr
  (lib-promptfn lib-promptfn-abbr)
  (lib-tpsobject get-lib-abbr)
  (lib-printfn libprint-abbr)
  (mhelp "Saving abbreviations. Abbreviations should be closed wffs. "))

(defun libprint-abbr (abbr libstyle)
  (let ((gwff (getwff-subtype 'gwff-p abbr)))
    (if (eq libstyle 'write-to-file)
	(in-mode re-read (msg (gwff . gwff)))
      (msg (gwff . gwff)))))

(defun get-lib-abbr (libitem)
  (let ((abbrev-object nil))
    (push (list 'defn (libitem-description libitem)) abbrev-object)
    (push (list 'mhelp (libitem-mhelp libitem)) abbrev-object)
    (mapcar #'(lambda (prop)
		(if (consp prop)
		    (push prop abbrev-object)
		    (when load-warn-p (complain "Ignoring attribute: " prop))))
	    (libitem-other-attributes libitem))
    (eval (nconc (list 'def-abbrev (libitem-name libitem))
		 (nreverse abbrev-object)))
    (libitem-name libitem))) ;added to prevent it from returning stuff like #<Synonym Stream to SYSTEM:*STDOUT*>

(defun lib-promptfn-abbr (name type help file modify comment old-value)
  (declare (ignore name type help file modify comment))
  (let ((gwff (if old-value (libparse-gwff (libitem-description old-value))
		  '$))
	(description control-d-char)
	(other-attributes nil)
	(foo nil)
	ignore)
    (prompt-read description nil (msgf "Definition to save? ") 'gwff gwff
		 ((? (msgf "the definition to be saved."))
		  (?? (mhelp 'gwff))))
    (unless (eq description control-d-char)
      (let ((free-vars (free-vars-of description)))
	(when free-vars
	  (complain "The following variables are free in the abbreviation:"
		    t)
	  (dolist (var free-vars)
	    (msg (var . gwff) 2))))
      (with-open-stream (string-stream (make-string-output-stream))
			(type-to-lib-stream
			  (type description) string-stream)
			(push (list 'type (get-output-stream-string
					    string-stream))
			      other-attributes))
      (setq description
	    (with-output-to-string
	      (*standard-output*)
	      (in-mode re-read (let ((style 'generic))
				 (msg (description . gwff))))))) 
    (let ((typelist (if old-value
			(mapcar #'get-stringtype
				(cadr (assoc 'typelist
					     (libitem-other-attributes
					       old-value))))
			nil)))
      (prompt-read ignore foo (msgf "Typelist") 'typesymlist typelist
		   ((? (msgf "List of type variables, eg., (\"A\" \"B\") ")
		       )
		    (?? (mhelp 'typesymlist))))
      (if (eq ignore typelist)
	  (setq foo (if typelist
			(cadr (assoc 'typelist
				     (libitem-other-attributes old-value)))
			nil))))
    (when foo
      (push (list 'typelist (if (consp foo) foo (list foo)))
	    other-attributes))
    (prompt-read foo nil (msgf "Infix priority") 'null-or-posinteger
		 (if old-value (cadr (assoc 'infix (libitem-other-attributes
						     old-value)))
		     nil)
		 ((? (msgf "Some positive integer n if an infix abbreviation else NIL"))
		  (?? (mhelp 'null-or-posinteger))))
    (when foo (push (list 'infix foo) other-attributes))
    (prompt-read foo nil (msgf "printnotype") 'boolean
		 (if old-value (cadr (assoc 'printnotype
					    (libitem-other-attributes
					      old-value)))
		     (not printtypes))
		 ((? (msgf "If T types are not printed."))
		  (?? (mhelp 'boolean))))
    (when foo (push (list 'printnotype foo) other-attributes))
    (prompt-read foo nil (msgf "fo-single-symbol") 'symbol
		 (if old-value (cadr (assoc 'fo-single-symbol
					    (libitem-other-attributes
					      old-value)))
		     t)
		 ((? (msgf "If non NIL, TPS will accept the abbreviation in uppercase," t
			   "lowercase,  or any combination of these. Otherwise it has" t
			   "to be specified as an uppercase symbol only." t))
		  (?? (mhelp 'symbol))))
    (when foo
      (push (list 'fo-single-symbol foo) other-attributes))
    (prompt-read foo nil (msgf "Face") 'symbollist
		 (if old-value (cdr (assoc 'face (libitem-other-attributes
						   old-value)))
		     nil)
		 ((? (msgf "If non NIL, then used by printing to decide how to print the" t
			   "abbreviation. Should be a list of symbols, each of which is either a" t
			   "printable character such as X, %, + or even | | for an empty space," t
			   "or the name of a special character." t
			   "In styles which do not have a given special character," t
			   "the name of the character will be printed instead." t
			   "The special characters available in style XTERM are:" t
			   (mapcar 'car xterm-characters) t
			   "To see the special characters available in other styles, type ??" t))
		  (?? (msgf "In style GENERIC, there are no special characters." t t
			    "In style CONCEPT, there are: " (remove-if 'listp global-cfontlist) t t
			    "In style SCRIBE, there are: " (remove-if 'listp global-scribecharlist) t t
			    "In style TEX, there are: " (remove-if 'listp global-texcharlist) t))))
    (when foo
      (push (cons 'face foo) other-attributes))
    (values description 'unclassified other-attributes)))

;;;DPAIRS
 
(deflibobject dpairset
  (lib-promptfn lib-promptfn-dpairset)
  (lib-descr-readfn libparse-dpairset)
  (lib-tpsobject lib-tpsobject-dpairset)
  (lib-printfn libprint-dpairset)
  (mhelp "Set of disagreement pairs."))

(defun libprint-dpairset (dpairset libstyle)
  (if (eq libstyle 'write-to-file)
      (in-mode re-read
	       (msg "(")
	       (dolist (dpair dpairset (msg ")"))
		 (msg t "(" ((car dpair) . gwff) 2 "." 2
		      ((cdr dpair) . gwff) ")" )))
      (dolist (dpair dpairset)
	(msg t ((car dpair) . gwff) 2 "." 2 ((cdr dpair) . gwff)))))

(defun libparse-dpairset (dpairset)
  (let ((dpairs nil))
    (dolist (dpair dpairset (nreverse dpairs))
      (let ((first (gettype 'gwff (car dpair)))
	    (2nd (gettype 'gwff (cdr dpair))))
	(push (cons first 2nd) dpairs)))))

(defun lib-tpsobject-dpairset (libitem)
  (let ((name (libitem-name libitem)))
    (when (boundp 'auto:dpairlist)
      (delete name auto:dpairlist)
      (push name auto:dpairlist)
      (set name (libitem-description libitem))
      (when (assoc 'variable-list (libitem-other-attributes libitem))
	    (setq auto::named-fv-list 
		  (cons (cons name (cadr (assoc 'variable-list (libitem-other-attributes libitem))))
			(delete-if #'(lambda (x) (eq (car x) name)) auto::named-fv-list)))))))

(defun lib-promptfn-dpairset (name type help file modify comment old-value)
  (declare (ignore type help file modify comment old-value))
  (let ((description nil)
	(other-attributes nil)
	(foo nil))
    (prompt-read description nil (msgf "Name of disagreement set to save?")
		 'symbol (if (and (boundp 'auto:dpairlist) auto:dpairlist)
			     (car auto:dpairlist) '$)
		 ((? (msgf "The name of disagreement set to be saved. "))
		  (?? (when (and (boundp 'auto:dpairlist) auto:dpairlist)
			(msgf "Possibly one of:")
			(dolist (name auto:dpairlist)
			  (msg 2 name))))))
    (prompt-read foo nil (msgf "Free variables") 'gvarlist
		 (if (assoc name auto::named-fv-list) (cdr (assoc name auto::named-fv-list)) nil)
		 ((? (msgf "A list of variables, each in quotation marks."))
		  (?? (mhelp 'gvarlist))))
    (when foo
      (push (list 'variable-list foo) other-attributes))
    (values (eval description) 'unclassified other-attributes)))

;;MODE

(deflibobject mode
  (lib-promptfn lib-promptfn-mode)
  (lib-tpsobject get-lib-mode)
  (lib-printfn libprint-mode1)
  (mhelp "Define a new mode, and save it in the library. Note that you will
    have to explicitly set the all the flag settings that you want to save
    even if the mode already exists in the library. Also see MODE1."))

(defun lib-promptfn-mode (name type help file modify comment old-value)
  (declare (ignore type help file modify comment old-value))
  (when (memq name global-modelist)
    (unless (get name 'SAVE-THIS-MODE)
      (msgf "This mode is already defined in TPS. You can either define a " t
	    "different mode for the library, or save the mode that exists in TPS." t))
    (when (or (get name 'SAVE-THIS-MODE) (query "Do you want to save the mode that is already defined?" t))
      (return-from lib-promptfn-mode
	(values (get name 'flag-settings) 'unclassified nil ""))))
  (let (subjectlist)
    (prompt-read subjectlist nil (msgf "Subjects to be included in mode?")
                 'subjectlist '$
                 ((? (msgf (e (category-mhelp-list 'review-subject))))
		  (?? (mhelp 'subjectlist))))
    (values (flagsort (nreverse (get-mode-flags subjectlist))) 
	    'unclassified nil 
	    (concatenate 'string "Subjects included in mode were: " (princ-to-string subjectlist)))))

(defun get-mode-flags (subjectlist)
  (write-char control-g-char)
  (msgf "<Return> selects the default value of the flag,"
	t "<!> means that the flag value should not be affected by the mode,"
	t "<?> prints the help message for the flag.")
  (let ((defined-flags nil))
    (dolist (subject subjectlist (nreverse defined-flags))
      (msgf "Subject: " subject t)
      (setq defined-flags (define-mode-flags subject defined-flags)))))

;;;the next 3 functions used to be in file MODSAV. -si 27 nov 88

(defun get-lib-mode (libitem)
  (let ((mode-object nil))
    (mapcar #'(lambda (prop)
		(if (and (consp prop) (get (car prop) 'flag))
		    (push prop mode-object)
		    (when load-warn-p (complain "Ignoring attribute: " prop))))
	    (libitem-description libitem))
    (push 'flag-settings mode-object)
    (setq mode-object (list mode-object))
    (push (list 'mhelp (libitem-mhelp libitem)) mode-object)
    (eval (nconc (list 'defmode (libitem-name libitem))
		 (nreverse mode-object)))))

(defun define-mode-flags (subject defined-flags)
  (dolist (flag global-flaglist defined-flags)
    (if (and (symbolp flag) (member subject (get flag 'subjects)))
	(setq defined-flags (define-mode-flag flag defined-flags)))))

(defun define-mode-flag (flag defined-flags)
  (do ((flagvalue nil) (external-value nil) (dont-save nil)) (nil)
    (%catch% (progn
	       (prompt-read flagvalue external-value
			    (msgf flag) (get flag 'flagtype) (eval flag)
			    ((? (mhelp flag)) (?? (mhelp 'flag)) 
			     (! (progn (princ "Flag not saved.")(setq dont-save t)(return '$)))))
		     (if (eq external-value '$) (setq external-value (eval flag)))
	       (unless dont-save
		 (push (list flag external-value) defined-flags))
	       (return defined-flags))
	     (fail (complain "Could not read value of flag.  " 
                             core::expand-catch-throw)))))

(defun flagsort (list)
  (sort list #'(lambda (x y) 
		 (string-lessp (string-upcase (princ-to-string x)) 
			       (string-upcase (princ-to-string y))))))

;;MODE1

(deflibobject mode1
  (lib-promptfn lib-promptfn-mode1)
  (lib-tpsobject get-lib-mode)
  (lib-printfn libprint-mode1)
  (mhelp "Define a new mode, and save it in the library. All the current flag
    settings for the subjects that you specify will be saved. Also see MODE."))

(defun lib-promptfn-mode1 (name type help file modify comment old-value)
  (declare (ignore type help file modify comment old-value))
  (let ((default (list (if (string= (princ-to-string auto::default-ms) "MS" :end1 2)
			   auto::default-ms
			 'auto::mtree-top)
		       'auto::mating-search 'auto::unification 'auto::primsubs 'auto::important))
	(subjectlist))
    (when (eq auto::default-ms 'ms98-1) (push 'auto::ms98-minor default))
    (when (memq name global-modelist)
	  (msgf "This mode is already defined in TPS. You can either define a " t
		"different mode for the library, or save the mode that exists in TPS." t)
	  (when (query "Do you want to save the mode that is already defined?" t)
		(return-from lib-promptfn-mode1
			     (values (get name 'flag-settings) 'unclassified nil
				     (concatenate 'string "Subjects included in mode1 were: " 
						  (princ-to-string subjectlist))))))
    (prompt-read subjectlist nil (msgf "Subjects to be included in mode?")
                 'subjectlist default 
                 ((? (msgf (e (category-mhelp-list 'review-subject))))
		  (?? (mhelp 'subjectlist))))
    (let ((write-flags nil))
      (dolist (subject (if (equal subjectlist '(all))
                           (remove-if-not #'symbolp global-subjectlist)
                           subjectlist))
        (dolist (flag global-flaglist)
          (when (and (symbolp flag) (not (get flag 'synonym))
                     (member subject (get flag 'subjects)))
            (pushnew flag write-flags))))
      (values (flagsort write-flags) 'unclassified nil (concatenate 'string "Subjects included in mode1 were: " (princ-to-string subjectlist))))))

;;;Now we don't need to making distinguish between 
;;;writing to file and to tty
(defun libprint-mode1 (mode libstyle)
    (declare (ignore libstyle))
    (progn 
      (msg "(")
	(dolist (flag mode)
	  (msg t (t 3))
	  (if (symbolp flag)
	      (progn (msg "(") (prin1 flag) (msg " ")
		     (let ((flag-value (eval flag)))
		       (in-mode re-read
				(funcall (get (get flag 'flagtype) 'printfn)
					 flag-value)))
		     (msg ") "))
	      (prin1 flag)))
	(msg ") ")))


;;CONST

(deflibobject lib-const
  (lib-promptfn lib-promptfn-const)
  (lib-tpsobject get-lib-const)
  (mhelp "Constants and Polymorphic Proper Symbols. 
These are like abbreviations, but will never be expanded
by TPS and hence have no definition."))

(defun get-lib-const (libitem)
  (let ((const-object nil))
    (push (list 'mhelp (libitem-mhelp libitem)) const-object)
    (mapcar #'(lambda (prop)
		(if (consp prop)
		    (push prop const-object)
		    (when load-warn-p (complain "Ignoring attribute: " prop))))
	    (libitem-other-attributes libitem))
    (if (assoc 'typelist const-object)
	(eval (nconc (list 'def-pmpropsym (libitem-name libitem))
		     (nreverse const-object)))
      (eval (nconc (list 'def-logconst (libitem-name libitem))
		   (nreverse const-object))))
    (libitem-name libitem))) ;added to prevent it from returning stuff like #<Synonym Stream to SYSTEM:*STDOUT*>

(defun lib-promptfn-const (name libtype help file modify comment old-value)
  (declare (ignore libtype help file modify comment))
  (let ((description nil)
	(other-attributes nil)
	(foo nil))
    (let ((type1 (if old-value
			(mapcar #'get-stringtype
				(cadr (assoc 'type
					     (libitem-other-attributes
					       old-value))))
			(if (or (logconst-p name) (pmprsym-p name)) (get name 'type) nil))))
      (prompt-read foo nil (msgf "Type") 'typesym type1
		   ((? (msgf "The type of the constant."))
		    (?? (mhelp 'typesym)))))
    (when foo
      (with-open-stream (string-stream (make-string-output-stream))
			(type-to-lib-stream
			  foo string-stream)
			(push (list 'type (get-output-stream-string
					    string-stream))
			      other-attributes)))
    (let ((typelist (if old-value
			(mapcar #'get-stringtype
				(cadr (assoc 'typelist
					     (libitem-other-attributes
					       old-value))))
			(if (pmprsym-p name) (get name 'typelist) nil))))
      (prompt-read foo nil (msgf "Typelist") 'typesymlist typelist
		   ((? (msgf "The type list, if the symbol is polymorphic."))
		    (?? (mhelp 'typesymlist)))))
    (when foo
      (with-open-stream (string-stream (make-string-output-stream))
			(dolist (f foo) (type-to-lib-stream
					 f string-stream)
			(push (list 'typelist (get-output-stream-string
					       string-stream))
			      other-attributes))))
    (prompt-read foo nil (msgf "Infix priority") 'null-or-posinteger
		 (if old-value (cadr (assoc 'infix (libitem-other-attributes
						     old-value)))
		     (if (or (logconst-p name) (pmprsym-p name)) (get name 'infix) nil))
		 ((? (msgf "Some positive integer n if an infix abbreviation else NIL"))
		  (?? (mhelp 'null-or-posinteger))))
    (when foo (push (list 'infix foo) other-attributes))
    (prompt-read foo nil (msgf "printnotype") 'boolean
		 (if old-value (cadr (assoc 'printnotype
					    (libitem-other-attributes
					      old-value)))
		     (if (or (logconst-p name) (pmprsym-p name)) (get name 'printnotype) (not printtypes)))
		 ((? (msgf "If T types are not printed."))
		  (?? (mhelp 'boolean))))
    (when foo (push (list 'printnotype foo) other-attributes))
    (prompt-read foo nil (msgf "fo-single-symbol") 'symbol
		 (if old-value (cadr (assoc 'fo-single-symbol
					    (libitem-other-attributes
					      old-value)))
		     (if (or (logconst-p name) (pmprsym-p name)) (get name 'fo-single-symbol) nil))
		 ((? (msgf "If non NIL, TPS will accept the abbreviation in uppercase," t
			   "lowercase,  or any combination of these. Otherwise it has" t
			   "to be specified as an uppercase symbol only." t))
		  (?? (mhelp 'symbol))))
    (when foo
      (push (list 'fo-single-symbol foo) other-attributes))
    (prompt-read foo nil (msgf "Face") 'symbollist
		 (if old-value (cdr (assoc 'face (libitem-other-attributes
						   old-value)))
		     (if (or (logconst-p name) (pmprsym-p name)) (get name 'face) nil))
		 ((? (msgf "If non NIL, then used by printing to decide how to print the" t
			   "abbreviation. Should be a list of symbols, each of which is either a" t
			   "printable character such as X, %, + or even | | for an empty space," t
			   "or the name of a special character." t
			   "In styles which do not have a given special character," t
			   "the name of the character will be printed instead." t
			   "The special characters available in style XTERM are:" t
			   (mapcar 'car xterm-characters) t
			   "To see the special characters available in other styles, type ??" t))
		  (?? (msgf "In style GENERIC, there are no special characters." t t
			    "In style CONCEPT, there are: " (remove-if 'listp global-cfontlist) t t
			    "In style SCRIBE, there are: " (remove-if 'listp global-scribecharlist) t t
			    "In style TEX, there are: " (remove-if 'listp global-texcharlist) t t))))
    (when foo
      (push (cons 'face (if (listp foo) foo (list foo))) other-attributes))
    (values description 'unclassified other-attributes)))


(defvar *libtex-start*)

(defun pwff-lib (wff)
  (declare (special atomposlist atomvallist markatom untyped-lambda-calculus))
  (let ((wff (if untyped-lambda-calculus (decode-typed wff) 
	       (if elim-defns (eliminate-defns wff) wff)))
        (*print-case* :upcase)
	(ppvirtflag nil)
	(ppwfflength 0)
	(pc " ")
	(hatomalist nil))
    (declare (special ppvirtflag ppwfflength pc hatomalist))
    (when atomvalflag
      (setq atomposlist nil)
      (setq atomvallist nil)
      (setq markatom nil))
    (setq *libtex-start* t curpos rightmargin)
    (msgf "}\\vskip 1em\\hskip 2em\\vbox{\\indent")
;    (pcall text-prefix)
    (if ppwfflag
	(let (pplist lastprefindt)
	  (declare (special lastprefindt))
	  (setq ppvirtflag t)
	  (setq pplist (if scope (printwffscope-lib-tex wff nil 1)
			   (printwffplain-lib-tex wff t 1))
		ppvirtflag nil)
	  (setq lastprefindt
		(if (singleaplicn (caar pplist)) leftmargin curpos))
	  (printpplist-lib-tex pplist leftmargin))
      ;(if atomvalflag (atomvalterpri-tex))
      (if scope (printwffscope-lib-tex wff nil 1) (printwffplain-lib-tex wff t 1)))
    (pcall text-postfix)
    (msg "\\lastformula\\judgelink{}}\\filbreak{\\obeylines\\tt")
    (msgf "\\vskip 1em")
    hatomalist))

(defun atomvalterpri-tex ()
  (declare (special curpos))
  (setq curpos 0)
  (if *libtex-start*
      (setq *libtex-start* nil)
    (progn (princ "$}\\middleformula") (terpri)))
;  (princ "\\linenumbox{}\\emptyhypbox\\partformula{$")
  (princ "\\partformula{$"))


(defun printwffscope-lib-tex (wff brackets depth)
  (declare (special hatomalist))
  (let ((*print-case* :upcase))
    (cond ((prt-symbol-p wff)
	   (printwffplain-lib-tex wff brackets depth))
	  (ppvirtflag
	   (let ((beforelength ppwfflength)
		 (space-p (pcall print-space-p "[" pc))
		 (dot-flag (and brackets use-dot (null allscopeflag)))
		 aplicnlist ppwffdepth)
	     (setq ppwfflist nil)
	     (setq pc (if dot-flag "." "["))
	     (incf ppwfflength (1+ (if space-p 1 0)))
	     (setq aplicnlist (printwff-lib-tex wff t depth))
	     (incf ppwfflength (if dot-flag 0 1))
	     (setq ppwffdepth (if (genchar (car aplicnlist))
				  (- ppwfflength beforelength)
				(+ (aplicnlistdepth aplicnlist)
				   (if dot-flag 0 1))))
	     (acons aplicnlist
		    (cons ppwffdepth
			  (if dot-flag (if space-p 'space-dot 'dot)
			    (if space-p 'space-brackets 'brackets)))
		    (- ppwfflength beforelength))))
	  (t (if (and brackets use-dot (null allscopeflag))
		 (pp-symbol-space ".")
	       (pp-symbol-space "["))
	     (printwff-lib-tex wff t depth)
	     (if (not (and brackets use-dot (null allscopeflag)))
		 (pp-symbol-space "]"))))))

(defun printwffplain-lib-tex (wff brackets depth)
  (declare (special hatomalist))
  (cond ((and (prt-aplicn-p wff)
	      (or (prt-infix-op (gar wff)) (prt-infix-op (gdr wff))))
	 (printwffscope-lib-tex wff nil depth))
	(ppvirtflag (let ((beforelength ppwfflength)
			  aplicnlist ppwffdepth)
		      (setq ppwfflist nil)
		      (setq aplicnlist (printwff-lib-tex wff brackets depth))
		      (setq ppwffdepth
			    (if (genchar (car aplicnlist))
				(- ppwfflength beforelength)
				(aplicnlistdepth aplicnlist)))
		      (acons aplicnlist (list ppwffdepth)
			     (- ppwfflength beforelength))))
	(t (printwff-lib-tex wff brackets depth))))

(defun printwff-lib-tex  (wff brackets depth)
  (declare (special hatomalist printtypes))
  (let ((*print-case* :upcase))
    (cond ((label-q wff)
	   (apply-label wff (printwff-lib-tex wff brackets depth)))
	  ((lsymbol-q wff)
	   (if printtypes
	       (let* ((nr (getnameroot wff))
		      (typ (cdr (assoc nr hatomalist))))
		 (if (and (not printtypes-all) typ (eq typ wff))
		     (pp-symbol-space nr)
		   (progn (push (cons nr wff) hatomalist)
			  (pp-lsymbol-space wff))))
	     (pp-symbol-space (getnameroot wff))))
	  ((= depth printdepth) (pp-symbol-space '&))
	  ((and (prt-aplicn-p (car wff)) (prt-infix-op (gar (car wff))))
	   (prog (plainflag slevelflag prior inf left-arg left conn right)
	     (setq inf (gar (car wff)))
	     (setq prior (prt-infix-op inf))
	     (setq left-arg (gdr (car wff)))
	     (setq plainflag (need-no-brackets prior left-arg))
	     (setq slevelflag
	       (and plainflag (prt-aplicn-p left-arg)
		    (prt-aplicn-p (gar left-arg))
		    (prt-infix-op (gar (gar left-arg)))
		    (= prior (prt-infix-op (gar (gar left-arg))))
		    (prt-associative-p (gar (gar left-arg)))))
	     (setq left
	       (funcall
		(cond (slevelflag #'printwff)
		      (plainflag #'printwffplain-lib-tex)
		      (t #'printwffscope-lib-tex))
		left-arg nil (if slevelflag depth (1+ depth))))
	     (setq conn (printwffplain-lib-tex inf nil depth))
	     (setq plainflag (need-no-brackets-on-right prior (cdr wff)))
	     (setq right
	       (funcall (if plainflag #'printwffplain-lib-tex #'printwffscope-lib-tex)
			(cdr wff) brackets (1+ depth)))
	     (return (if ppvirtflag
			 (nconc (if slevelflag left
				   (list (cons '((nil 0) . 0) left)))
				(list (cons conn right)))
		       nil))))
	  ((boundwff-q wff)
	   (if ppvirtflag
	       (let ((beforelength ppwfflength)
		     binderpatom binderpgl)
		 (pp-symbol-space (cdar wff))
		 (setq binderpatom (printwff-lib-tex (caar wff) brackets depth))
		 (setq binderpgl
		   (acons binderpatom (list (- ppwfflength beforelength))
			  (- ppwfflength beforelength)))
		 (list (cons binderpgl
			     (funcall
			      (if (or (and (prt-aplicn-p (cdr wff))
					   (boundwff-q (cdr wff))
					   (null allscopeflag))
				      (and (get (cdar wff) 'prefix)
					   (need-no-brackets
					    (get (cdar wff) 'prefix)
					    (cdr wff))))
				  #'printwffplain-lib-tex #'printwffscope-lib-tex)
			      (cdr wff) brackets (1+ depth)))))
	     (progn
	       (pp-symbol-space (cdar wff))
	       (printwff-lib-tex (caar wff) brackets depth)
	       (funcall
		(if (or (and (prt-aplicn-p (cdr wff)) (boundwff-q (cdr wff))
			     (null allscopeflag))
			(and (get (cdar wff) 'prefix)
			     (need-no-brackets (get (cdar wff) 'prefix)
					       (cdr wff))))
		    #'printwffplain-lib-tex #'printwffscope-lib-tex)
		(cdr wff) brackets (1+ depth)))))
	  (t (list
	      (cons (funcall
		     (if (or
			  (and
			   (prt-aplicn-p (car wff))
			   (or (boundwff-q (car wff))
			       (and (prt-aplicn-p (gar (car wff)))
				    (prt-infix-op (gar (gar (car wff)))))))
			  allscopeflag)
			 #'printwffscope-lib-tex #'printwffplain-lib-tex)
		     (car wff) nil (1+ depth))
		    (funcall
		     (if (and (null allscopeflag)
			      (prt-symbol-p (car wff))
			      (prt-prefix-op (car wff))
			      (need-no-brackets (prt-prefix-op (car wff))
						(cdr wff) t))
			 #'printwffplain-lib-tex #'printwffscope-lib-tex)
		     (cdr wff) brackets (1+ depth))))))))


(defun printpplist-lib-tex (pplist indent)
  (declare (special lastprefindt markatom))
  (cond
   ((null (caar pplist)) nil)
   ((and atomvalflag (eq (caaar pplist) 'markatom))
    (rplaca (car pplist) (cdaar pplist))
    (setq markatom t)
    (printpplist-lib-tex pplist indent))
   ((< (+ indent (cdr pplist)) rightmargin)
    (indentation-lib-tex indent)
    (setq curpos (+ curpos (cdr pplist)))
    (sprintpplist pplist))
   ((genchar (caaar pplist))
    (when (not (< (+ curpos (cdr pplist)) rightmargin))
      (indentation-lib-tex (min (+ 2 lastprefindt)
			(- rightmargin (cdr pplist)))))
    (setq curpos (+ curpos (cdr pplist)))
    (sprintpplist pplist))
   (t (prog (newindent rspace)
	(when (cdaar pplist)
	  (setq rspace (1+ (- rightmargin (+ (cadar pplist) indent))))
	  (when (or (< rspace 0) (> (- indent lastprefindt) 30))
	    (cond ((< (+ (- indent lastprefindt 2) rspace) 0)
		   (setq indent (min indent (+ lastprefindt 2)))
		   (setq newindent (+ 2 indent))
		   (go ind))
		  (t (setq indent (+ lastprefindt 2))))))
	(setq newindent (+ indent (or (max-with-key (caar pplist) #'cdar) 0)))
       ind (when (<= indent rightmargin) (indentation-lib-tex indent))
	(case (cddar pplist)
	  (brackets
	   (princ "[")
	   (incf curpos)
	   (incf indent)
	   (incf newindent))
	  (dot
	   (incf curpos)
	   (princ ".")
	   (incf indent)
	   (incf newindent))
	  (space-brackets
	   (incf curpos 2)
	   (princ " [")
	   (incf indent 2)
	   (incf newindent 2))
	  (space-dot
	   (incf curpos 2)
	   (princ " .")
	   (incf indent 2)
	   (incf newindent 2)))
	(printpplist-lib-tex (caaaar pplist) indent)
	(when (cdaar pplist)
	      (setq lastprefindt
		    (if (and localleftflag (zerop (cdr (caaaar pplist))))
			(min curpos newindent)
			newindent)))
	(printpplist-lib-tex (cdaaar pplist)
		     (if (and localleftflag (zerop (cdr (caaaar pplist))))
			 (min curpos newindent)
			 newindent))
	(dolist (aplicn (cdaar pplist))
	  (if (and fillineflag
		   (< (+ curpos (cdar aplicn) (cddr aplicn)) rightmargin))
	      (progn
	       (printpplist-lib-tex (car aplicn) curpos)
	       (printpplist-lib-tex (cdr aplicn) curpos))
	      (progn
	       (atomvalterpri-tex)
	       (printpplist-lib-tex (car aplicn) indent)
	       (setq lastprefindt
		     (if (< (1+ (- rightmargin curpos)) (cadadr aplicn))
			 curpos newindent))
	       (printpplist-lib-tex (cdr aplicn) lastprefindt)
)))
	(when (memq (cddar pplist) '(brackets space-brackets))
	      (setq curpos (1+ curpos))
	      (princ "]"))
	(return nil)))))

(defun indentation-lib-tex (indent)
  (when (< indent curpos)
	 (atomvalterpri-tex))
  ;;(setq curpos (+ curpos indent))
  (pcall print-indent indent))

;;; CLASS-SCHEME - cebrown - 11/9/01
(deflibobject class-scheme
  (lib-promptfn lib-promptfn-class-scheme)
  (lib-tpsobject get-class-scheme)
  (lib-printfn libprint-class-scheme)
  (mhelp "Classification Scheme for a library.
A classification scheme is a way of organizing library items into a tree 
(actually a directed acyclic graph) of classes.  Each class can have 
classes as children.  Each class has associated libitems.

To see what classification schemes are available call:
LIST-OF-LIBOBJECTS CLASS-SCHEME
from the lib top level.

See Also: CREATE-CLASS-SCHEME, PSCHEMES, PCLASS-SCHEME-TREE, 
PCLASS-TREE, CREATE-LIBCLASS, CLASSIFY-CLASS, CLASSIFY-ITEM, 
FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(defun libprint-class-scheme (cl libstyle)
  (declare (ignore libstyle))
  (pprint cl))

(defun lib-promptfn-class-scheme (name type help file modify comment old-value)
  (declare (ignore type help file modify comment old-value))
  (unless (and (get name 'libclass) (libclass-p (get name 'libclass)))
    (throwfail name " is not a Classification Scheme in memory."))
  (values (class-scheme-description (get name 'libclass))
	  'unclassified nil ""))

(defun get-class-scheme (libitem)
  (let ((class-scheme-name (libitem-name libitem))
	(descr (libitem-description libitem)))
    (mapcar #'(lambda (prop)
		(if (and (consp prop) (symbolp (car prop)))
		    (setf (get class-scheme-name (car prop)) (cdr prop))
		  (when load-warn-p (complain "Ignoring attribute: " prop))))
	    (libitem-other-attributes libitem))
    (eval (list 'def-class-scheme class-scheme-name
		(list 'class-direction (car descr))
		(list 'libclass (build-class-scheme (cdr descr)))
		(list 'mhelp (libitem-mhelp libitem))))))

; returns a libclass or throws failure if given a bad description of a libclass dag
(defun build-class-scheme (descr)
  (let ((already-done nil))
    (declare (special already-done))
    (build-class-scheme-rec descr)))

(defun build-class-scheme-rec (descr)
  (declare (special already-done))
  (if (symbolp descr)
      (let ((a (assoc descr already-done)))
	(if a
	    (cdr a)
	  (throwfail "Problem Reading Classification Scheme")))
    (if (and (consp descr) (consp (cdr descr)) (consp (cddr descr))
	     (symbolp (car descr)))
	(let ((cl (make-libclass :name (car descr)
				 :libitems (cadr descr)
				 :kids nil))
	      (kids nil))
	  (push (cons (car descr) cl) already-done)
	  (do ((dkids (caddr descr) (cdr dkids)))
	      ((not (consp dkids))
	       (when dkids
		 (throwfail "Problem Reading Classification Scheme")))
	    (let ((kcl (build-class-scheme-rec (car dkids))))
	      (push cl (libclass-parents kcl))
	      (push kcl kids)))
	  (setf (libclass-kids cl) (reverse kids))
	  cl)
      (throwfail "Problem Reading Classification Scheme"))))

(defun class-scheme-description (cl)
  (let ((already-done nil))
    (declare (special already-done))
    (cons class-direction
	  (class-scheme-description-rec cl))))

(defun class-scheme-description-rec (cl)
  (declare (special already-done))
  (if (member (libclass-name cl) already-done)
      (libclass-name cl)
    (let ((l))
      (push (libclass-name cl) already-done)
      (dolist (cl1 (libclass-kids cl))
	(push (class-scheme-description-rec cl1) l))
      (list (libclass-name cl)
	    (libclass-libitems cl)
	    (reverse l)))))

;;; MODES-GWFFS - cebrown 4/5/03 changed from GOODMODE-LIST - cebrown - 1/28/03 
(deflibobject MODES-GWFFS
  (lib-promptfn lib-promptfn-modes-gwffs)
  (lib-tpsobject get-modes-gwffs)
  (lib-printfn libprint-modes-gwffs)
  (mhelp "A list of 'good' modes.  Generally, this should be a list of
modes which can be used to prove many theorems automatically.
We usually want a list of goodmodes to be 'complete' in the following sense:
For any theorem that has a bestmode, there is some goodmode that proves the
theorem.

SEE ALSO: GOODMODES, TEST-INIT, ADD-GOODMODES, REMOVE-GOODMODES"))

(defun lib-promptfn-modes-gwffs (name type help file modify comment old-value)
  (declare (ignore type help file modify comment old-value))
  (unless (symbolp name)
    (throwfail "The name of a modes-gwffs should be a symbol."))
  (let (modes-gwffs-modes
	modes-gwffs-gwffs)
    (prompt-read modes-gwffs-gwffs nil (msgf "List of Modes") 'symbollist
		 (get name 'modes-gwffs-modes)
		 ((? (msgf "A list of modes." t))
		  (?? (msgf "Usually this is a list of goodmodes.  For each theorem that has a bestmode, there" t
			    "should be a mode on this list that proves that theorem." t))))
    (prompt-read modes-gwffs-gwffs nil (msgf "List of Thms") 'symbollist
		 (get name 'modes-gwffs-gwffs)
		 ((? (msgf "A list of gwffs." t))
		  (?? (msgf "Usually this is a list of theorems known to be provable using a mode" t
			    "on the list of modes given above." t))))
    (values (list modes-gwffs-modes modes-gwffs-gwffs)
	    'unclassified nil 
	    (format nil "Modes included are:~%~S~2%Gwffs included are:~%~S~%"
		    modes-gwffs-modes
		    modes-gwffs-gwffs))))

(defun get-modes-gwffs (libitem)
  (let ((modes-gwffs-modes (car (libitem-description libitem)))
	(modes-gwffs-gwffs (cadr (libitem-description libitem)))
	(name (libitem-name libitem)))
    (eval (list 'def-modes-gwffs name
		(cons 'modes-gwffs-modes modes-gwffs-modes)
		(cons 'modes-gwffs-gwffs modes-gwffs-gwffs)
		(list 'mhelp (libitem-mhelp libitem))))))

(defun libprint-modes-gwffs (g libstyle)
  (declare (ignore libstyle))
  (pprint g))
