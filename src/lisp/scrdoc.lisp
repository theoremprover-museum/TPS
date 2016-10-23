;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :maint)
(part-of AUTO-DOC)

;;;
;;; File SCRDOC
;;; Module: AUTO-DOC

;;;
;;; defines functions necessary to produce Scribe-able documentation
;;; automatically from  TPS2.
;;;

(deffile scrdoc
  (part-of auto-doc)
  (extension clisp)
  (mhelp "Allows generation of SCRIBE-able documentation."))

(context coll-help)

(defmexpr scribe-doc
  (argtypes tpscatlist contextlist filespec)
  (argnames category-list context-list filename)
  (defaultfns (lambda (category-list context-list filename)
		(list category-list context-list
		      (if (eq filename '$)
			  (make-pathname% :name "facilities" :type "mss")
			  filename)))) 
  (mainfns scribe-cats-mexpr)
  (mhelp "Produce Scribe documentation about the specified categories."))

(defun scribe-cats-mexpr (category-list context-list filename)
  (scribe-cats category-list context-list filename t))

(defmexpr quick-ref
  (argtypes filespec)
  (argnames filename)
  (defaultfns (lambda (filename)
		(list (if (eq filename '$)
			  (make-pathname% :name "quick-ref-auto" :type "mss")
			   filename)))) ;;;(hx: I change '(list filename)' into 'filename' (30/10/92).
  (mainfns quick-ref-mexpr)
  (mhelp "Produce a quick reference to the rules available in TPS."))

(defvar global-srulelist)

(defun quick-ref-mexpr (filename)
  (quick-ref filename t))

(defun quick-ref (filename &optional (mexpr nil))
  (declare (special scribe-preamble))
  (if mexpr
  (reroute-output filename (make-pathname% :name "quick-ref-auto" :type "mss")
		  (msg scribe-preamble t)
		  (msg "@Begin(Format)" t "@TabSet(1.2inch,1.5in)")
		  (dolist (srule global-srulelist (msg t "@End(Format)" t "@TabClear"))
			  (msg T)
			  (if (atom srule)
			      (msg srule "@\\@\\"
	       (cdar (get srule 'mhelp)))
	  (msg t "@b(@ @ @ @ " (get (car srule) 'short-id) ")"))))
  (reroute-output filename (make-pathname% :name "quick-ref-auto" :type "mss")
    (msg "@Begin(Format)" t "@TabSet(1.2inch,1.5in)")
    (dolist (srule global-srulelist (msg t "@End(Format)" t "@TabClear"))
      (msg T)
      (if (atom srule)
	  (msg "@indexsrule(" srule ")@\\@pageref(" srule ")@\\"
	       (cdar (get srule 'mhelp)))
	  (msg t "@b(@ @ @ @ " (get (car srule) 'short-id) ")"))))))

(defmacro msgt (&rest l)
  `(msg "@t{" ,@l "}"))

(defmacro msgi (&rest l)
  `(msg "@i{" ,@l "}"))

(defun scribe-cats (category-list context-list filename &optional (mexpr nil))
  (if mexpr
      (reroute-output filename (make-pathname% :name "facilities" :type "mss")
		      (msg "@Make(Manual)" t "@modify(description, spread .3)" t
			   "@String(DoubleSided=\"No\")" t "@Style(DoubleSided=No)" t
			   "@String(KsetSize=\"10\")" t "@String(DocName=\"Scribe-doc output\")" t 
			   "@Use(Database = \"" sys-dir "doc/lib\")" t "@LibraryFile(TPSDocuments)" t)
		      (dolist (cat category-list)
			      (scribe-cat cat context-list)))
  (reroute-output filename (make-pathname% :name "facilities" :type "mss")
    (dolist (cat category-list)
      (scribe-cat cat context-list)))))

(defun scribe-cat (category context-list)
  (msgf "@ChapterPh(" (cat-plural (get category 'mhelp-line)) ")" t)
  (cond ((equal category 'context)
	 (scribe-context context-list))
	((equal category '%theorem%)
	 (msgf)
	 (scribe-cat-long category context-list)
	 (msgf))
	(t
	 (scribe-cat-short category context-list)
	 (msgf)
	 (scribe-cat-long category context-list)
	 (msgf))))

(defun scribe-context (context-list)
  (when (get 'context 'cat-help) (msg (get 'context 'cat-help) t))
  (msg "The internal name of this category is @t(CONTEXT)." t
       (an-a (get 'context 'mhelp-line) t) " can be defined using " t
       (get 'context 'define)
       "." t "Allowable properties are: ")
  (do ((props (get 'context 'properties) (cdr props)))
      ((null props))
    (msgt (car props))
    (if (cdr props) (msg ", ")))
  (if (eq (get 'context 'other-prop) 'prop-error)
      (msg "." t)
      (msg ", and more." t))
  (msgf "@Begin(Description, Spread 0.5)")
  (dolist (item (eval (get 'context 'global-list)))
    (when (and (symbolp item) (member item context-list))
      (msgf item "@\\ is for " (string-downcase (get item 'short-id)) ".")
      (princ-mhelp item 'context)
      (msg -2)))
  (msgf "@End(Description)"))

(defun scribe-cat-short (category context-list)
  (declare (ignore context-list))
  (if (get category 'cat-help) (msg (get category 'cat-help) t))
  (msg "The internal name of this category is " t category "." t
       (an-a (get category 'mhelp-line) t) " can be defined using "
       (if (string= category "TACTIC") 'DEFTACTIC (get category 'define)) ;o/w tactic shows up as "defnonsense" !!
       "." t "Allowable properties are: ")
  (do ((props (get category 'properties) (cdr props)))
      ((null props))
    (msgt (car props))
    (if (cdr props) (msg ", ")))
  (if (eq (get category 'other-prop) 'prop-error)
      (msg "." t)
      (msg ", and more." t))  )

(defun next-context (lis)
  (if (or (null lis) (consp (car lis))) lis (next-context (cdr lis))))

(defun scribe-cat-long (category context-list)
  (reorganize-global-list category)
  (do ((cat-items (eval (get category 'global-list))
		  (if skip (next-context (cdr cat-items)) (cdr cat-items)))
       (skip nil nil))
      ((null cat-items) (msgf))
    (if (symbolp (car cat-items))
	(progn
	  ;; changed to show all files, no need to export their names DAN
	 (when (or show-all-packages (eq category 'tps-file) 
		   (accessible-p (car cat-items)))
	   (scribe-doc-item (car cat-items) category))
	 (if (or (not (cdr cat-items))
		 (not (symbolp (cadr cat-items))))
	     (msg "@End(Description)" t) (msgf t)))
	(if (and (or show-all-packages (accessible-p (caar cat-items)))
		 (member (caar cat-items) context-list))
	    (progn
	     (msgf t "@Section(" (get (caar cat-items) 'short-id) ")" -2)
	     (case category
	       ((scribe-char concept-char tex-char)
		(msg "@Begin(Description, ScriptPush No, Spread 0)"
		     t "@TabSet(+40pts)"))
	       ((abbrev logconst binder pmpropsym repsymbol)
		(msg "@Begin(Description)" t "@TabSet(+40pts, +80pts)"))
	       (t (msg "@Begin(Description)"))))
	    (setq skip t)))))

(defun index-item (item category)
  ;;;(msgf "@Index" (or (get category 'core::index-name) category)
  ;;;"(" item ")")
  (declare (ignore category))
					;  (msgf "@IndexCategory(" item ")")
  (msgf "@IndexOther(" item ")");; changed 8/22/87 DAN
  )


(defun scribe-doc-item (item category)
  (if (get category 'scribe-one-fn)
      (funcall (get category 'scribe-one-fn) item)
      (case category
	(toplevel (scribe-doc-toplevel item))
	(flag (scribe-doc-flag item category))
	((abbrev logconst binder) (scribe-doc-log item category))
	(repsymbol (scribe-doc-repsym item category))
	((scribe-char concept-char tex-char) (scribe-doc-char item category))
	(tps-file (scribe-doc-file item category))
	(module (scribe-doc-feature item category))
	(flag-mode (scribe-doc-mode item category))
	(srule (scribe-doc-rule item category))
	(%theorem% (scribe-doc-theorem item category))
	(review-subject (scribe-doc-subject item category))
	(t (scribe-doc-other item category)))))

(defun scribe-doc-subject (item category)
  (msgf)
  (index-item item category)
  (msg "@\\")
  (in-auto-doc-mode (princ-mhelp item category))
  (msgf "@begin(format)")
  (do ((subject-flags (mapcar #'string-downcase
			      (sort (copy-list
				     (remove-if-not 
				      #'(lambda (flag) (and (symbolp flag)
							    (member item
								    (get flag 'subjects))
							    (accessible-p flag)))
				      global-flaglist))
				    #'string<))))
      ((null subject-flags) (msgf "@end(format)"))
    (msgf (pop subject-flags))
    (when (car subject-flags) (msg "@=" (pop subject-flags)))
    (when (car subject-flags) (msg "@>" (pop subject-flags)))
    ))

(defun scribe-doc-toplevel (item)
  ;;(msgf "@T{")
  (index-item item 'toplevel)
  ;;(msgf "}@\\")
  (msgf "@\\")
  (princ-mhelp item 'toplevel)
  (msgf "@\\Its prompt is " (funcall (get item 'top-prompt-fn) "@i(n)")
	 " and it takes " (get (get item 'top-level-category) 'mhelp-line)
	  "s as input. "))

;;; index should be a string which gives the indexing, e.g. "@IndexMexpr(FOO)",
;;; for the command being scribed, args is the list of argnames for the item, 
;;; and description should be a description of what the command does.

(defun scribe-doc-command (index args description)
  (msgf index)
  (dolist (arg args)
      (msg " ")
      (msgi (string-downcase (string arg))))
    (msg "@\\")
  (if description
      (if (consp description)
	  (eval (cons 'msgf description))
	  (msgf description))
      (msgf "No further help available.  Sorry.")))


(defun scribe-doc-flag (item category)
  (msgf "@IndexFlag(" item ")@\\")
  (princ-mhelp item category)
  (msgf "It takes values of type " (get item 'flagtype)
       " and belongs to subjects ")
  (mapl #'(lambda (subj) (msgt (car subj))
		  (if (cdr subj) (msg ", ") (msg ".  ")))
	(get item 'subjects))
  (msg "The default value is @T{")
  (funcall (get (get item 'flagtype) 'printfn) (get item 'default))
  (msg "}." t))

(defun scribe-doc-mode (item category)
  (msgf)
  (index-item item category)
  (msg "@\\")
  (princ-mhelp item category)
  (msg " The settings of the flags are:" t
       "@Begin(Description, Spread 0, leftmargin +19, indent -19)")
  (dolist (setting (get item 'flag-settings))
    (msgf (car setting)  " @\\"  (cadr setting) -2))
  (msgf "@End(Description)" t))

(defun scribe-doc-log (item category)
  (msgf)
  (index-item item category)
  (msg "@\\")
  (when (or (get item 'dfont) (get item 'face))
	(in-auto-doc-mode
	 (msgt (item . fsym))))
  (msg "@\\")
  (cond ((get item 'infix) (msg (get item 'infix) " (Infix)"))
	((get item 'prefix) (msg (get item 'prefix) " (Prefix)")))
  (msg "@\\")
  (when (get item 'defn)
	(in-auto-doc-mode
	 (msg "@t{")
	 (setcurpos 0)
	 (prtwff (get item 'defn))
	 (msg "}.  ")))
  (when (get item 'mhelp)
	(when (get item 'defn) (msgf "@*@\\@\\@\\"))
	(princ-mhelp item category))
  (msgf))

(defun scribe-doc-repsym (item category)
  (msgf)
  (index-item item category)
  (msg "@\\@begin(verbatim)")
  (in-auto-doc-mode
   (princ-rephelp item category))
  (msg "@end(verbatim)")
  (msgf))


(defun scribe-doc-char (item category)
  (msgf)
  (index-item item category)
  (msg "@\\")
  (when (get item 'dfont)
	(in-auto-doc-mode
		 (msgt (item . fsym))))
  (when (get item 'mhelp) (msg "@\\") (princ-mhelp item category))
  (msgf))

(defun scribe-doc-file (item category)
  (msgf "@IndexFile(" item ")@\\")
  (if (get item 'part-of)
      (msg " Part of the " (get item 'part-of) " module. " f)
      (msg " Not part of any module. " f))
  (princ-mhelp item category))

(defun scribe-doc-feature (item category)
  (msgf)
  (index-item item category)
  (msg " @\\")
  (princ-mhelp item category)
  (msgf "It consists of:" T)
  (scribe-feat-list item (case category
			   (module '(macro-files files needed-modules)))))
		
(defun scribe-feat-list (item proplist)
  (msgf "@Begin(Description, Spread 0)" t)
  (dolist (prop proplist)
    (when (get item prop)
      (msg (string-downcase (string prop)) ": @\\")
      (dolist (elt (get item prop)) (msgt elt 3))
      (msg -2)))
  (msg "@End(Description)" t))

(defun scribe-doc-other (item category)
  (msgf)
  (index-item item category)
  (msg "@\\")
  (in-auto-doc-mode (princ-mhelp item category)))

(defun scribe-doc-rule (rule category)
  (declare (ignore category))
  (msg t "@IndexRule(" rule ")@label(" rule ")@\\")
  (in-auto-doc-mode (princ-mhelp rule 'mexpr))
  (msg  t "@\\")
  (msg "@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)")
  (%catch% (in-auto-doc-mode
	    (let ((auto-doc-flag T))
	      (help-rule rule)))
	   (fail (msg "Some error has occurred in TPS.")))
  (msg "@End(Verbatim)" t))

(defun scribe-doc-theorem (theorem category)
  ;(msgf theorem)
  (msg t "@IndexOther(" theorem ")")
  (when (and (assoc category (get theorem 'mhelp))
	     (not (string= (cdr (assoc category (get theorem 'mhelp))) "")))
    (msg "@ @ @ @ @ (" (cdr (assoc category (get theorem 'mhelp))) ")"))
  (msg "@\\")				
  (in-auto-doc-mode
   (msg "@t{")
   (setcurpos 0)
   (if (get theorem 'fol) ;;If we are printing an exercise from first order
       ;; logic, we don't want to print types.
       (let ((printtypes nil))
	 (prtwff (funcall (get 'theorem-type 'getfn) theorem)))
       (prtwff (funcall (get 'theorem-type 'getfn) theorem)))
   (msg "}"))
  )


;;; The section deals with listing the result of Help in a non-scribe
;;; format, rather than producing nice documents.

(defmexpr help-list
  (argtypes tpscat filespec) ;was "symbol" rather than "tpscat"
  (argnames category filename)
  (arghelp "Category to list" "Filename to list into")
  (defaultfns (lambda (category filename)
	       (list
                 (if (eq category '$) 'mexpr category)
		 (if (eq filename '$)
		     (make-pathname% :name (string-downcase (symbol-name category)) :type "help")
		      filename))))
  (mainfns mhelp-list)
  (mhelp "List all help available for objects of the given category
into a file."))

(defun mhelp-list (category filename)
  (reorganize-global-list category)
  (reroute-output filename  (make-pathname% :name category :type "help")
    (do ((cat-items (eval (get category 'global-list)) 
		    (if skip (next-context (cdr cat-items)) (cdr cat-items)))
	 (skip nil nil))
	((null cat-items))
      (if (symbolp (car cat-items))
	  (when (or show-all-packages (accessible-p (car cat-items)))
	    (funcall (cat-hack category 'mhelp-fn) (car cat-items) category)
	    (msg t "______________________________" t)) 
	  (if (or show-all-packages (accessible-p (caar cat-items)))
	      (msg t "=============================="
		   t (get (caar cat-items) 'short-id) ":"
		   t (e (princ-mhelp (caar cat-items) 'context))
		   t)
	      (setq skip t))))))


;;;
;;; Following are commands to produce documentation for special characters
;;; for various device styles.
;;;
;;; This does not yet distinguish between accessible and inaccessible symbols!
;;;

(defmexpr chardoc
  (argtypes symbol dev-stylelist filespec)
  (argnames output-style styles filename)
  (arghelp "The style of the output file, TEX or SCRIBE?"
           "The styles for which to list the special characters."
	   "File to list into")
  (defaultfns (lambda (output-style styles filename)
                (setq output-style (if (eq output-style' $) 'tex output-style))
		(setq styles (if (eq styles '$)
				 (remove-if-not
				  #'(lambda (style)
				      (and (symbolp style)
					   (get style 'char-cat)))
				  global-stylelist)
				 styles))
		(setq filename
		      (if (eq filename '$)
			  (make-pathname% :name "chars"
                              :type (cond ((eq output-style 'tex) "tex")
                                          ((eq output-style 'scribe) "mss")
                                          (t (throwfail "Sorry, cannot output in this style."))))
			  filename))
		(list output-style styles filename)))
 (mhelp "List the special characters of certain output styles in a TeX or Scribe
file. The output file can be processed by TeX or Scribe and will have 
multicolumn format."))


(defun chardoc (output-style styles filename)
  (let ((ext (cond ((eq output-style 'tex) "tex")
                   ((eq output-style 'scribe) "mss")
                   (t (throwfail "Sorry, cannot output in this style.")))))
       (reroute-output filename (make-pathname% :name "chars" :type ext)
                      (chardoc-styles output-style styles))))



(defun chardoc-styles (output-style styles)
  (cond ((eq output-style 'tex) (texchardoc-styles styles))
        ((eq output-style 'scribe) (scribechardoc-styles styles))
        (t (throwfail "Sorry, cannot output in this style."))))


(defconstnt charheading-scribe "
@Comment<
	This file contains the Character Set Guide for TPS3.
	>

@Comment<
	Editions:
	First: Being prepared
	>

@Make(Manual)

@Comment<Modify this for double-sided/single-sided versions>
@String(DoubleSided=\"No\")
@Style(DoubleSided=No)

@String(KsetSize=\"10\")
@String(DocName=\"TPS3 Character Sets\")

@Use(Database = \"/usr/theorem/tps/doc/lib\")
@LibraryFile(TPSDocuments)

@Comment<
	This is the title page information.
	>

@blankspace(2.25inches)

@MajorHeading{TPS3 Character Sets}
@style(date=\"1952 March 8\")
@Heading(@value(date)

Peter Andrews
Sunil Issar
Carl Klapper
Dan Nesmith
Frank Pfenning)

@BlankSpace(1.5inches)
@Center(Working Edition)

@BlankSpace(1inch)
");;; used to produce an independent chars.mss


(defmacro tex-char (ch)
  `(cond ((char= ,ch '#\\) (princ "$\\backslash$"))
                 ((char= ,ch '#\_) (princ "\\_"))
                 ((char= ,ch '#\$) (princ "\\$"))
                 ((char= ,ch '#\%) (princ "\\%"))
                 ((char= ,ch '#\>) (princ "$>$"))
                 ((char= ,ch '#\<) (princ "$<$"))
                 ((char= ,ch '#\&) (princ "\\&"))
                 ((char= ,ch '#\^) (princ "\\char94 "))
                 ((char= ,ch '#\#) (princ "\\#")) 
                 ((char= ,ch '#\-) (princ "$-$"))
                 ((char= ,ch '#\{) (princ "$\\{$"))
                 ((char= ,ch '#\}) (princ "$\\}$"))
                 (t (princ ,ch))))

(defun tex-writeout (string)
  (dotimes (i (length string)  nil)
     (tex-char (char string i))))


(defun texchardoc (styles filename)
  (reroute-output filename (make-pathname% :name "chars" :type "tex")
    (texchardoc-styles styles)))

(defun texchardoc-styles (styles)
  (declare (special core::tpstex))
  (setq styles (remove-if-not 
		#'(lambda (style) (get style 'char-cat)) 
		styles))
  (let ((char-cats (mapcar #'(lambda (style) (get style 'char-cat)) styles))
	sp-chars)
    (msg t "\\input " core::tpstex t "\\frontpageofchardoc" t)
    (msg t "\\noindent Listed below is the help for special characters in the styles ")
    (do ((styles styles (cdr styles)))
	((null styles) (msg "." t))
	(msg (car styles))
	(unless (null (cdr styles)) (msg ", ")))
    (msg t "\\vskip20pt")
    (setq sp-chars (apply #'append
			  (mapcar #'(lambda (char-cat)
				      (eval (get char-cat 'global-list)))
				  char-cats)))
    (setq sp-chars (unique-sort (remove-if-not #'symbolp sp-chars) #'alphalessp))
    (msg t "\\settabs " (+ (length char-cats) 2) " \\columns" t)
    (msg "\\+Name&\\hskip20pt Look in {\\TeX}")
    (dolist (style* styles) (msg "&\\hskip5pt  ") (msg style*))
    (msg "&\\cr" t "\\+&&&&&\\cr")
    (dolist (sp-char sp-chars)
	    (msg t) (msg "\\+") (tex-writeout (symbol-name sp-char)) (msg "&\\hskip20pt ")
	    (if (get sp-char 'tex-char)
		(msg "$\\" (get sp-char 'texname) "$")
	      (msg "[N/A]"))
	    (dolist (char-cat char-cats)
		    (msg "&\\hskip5pt ")
		    (if (get sp-char char-cat)
			(case char-cat
			      (scribe-char (msg  "@") (tex-writeout (get sp-char 'dfont)))
			      (tex-char (msg "$\\backslash$") (tex-writeout (get sp-char 'texname)))
			      (concept-char
			       (let ((chrset (car (get sp-char 'cfont)))
				     (chrval (cdr (get sp-char 'cfont))))
				 (case chrset
				       (0)
				       (1 (msg "f12 "))
				       (2 (msg "f13 "))
				       (3 (msg "f14 ")))
				 (cond ((< chrval 32) (msg "f10 ")
					(msg (e (if (= chrval 0)
						    (msg "@")
						  (tex-char (code-char (+ chrval 64)))))))
				       ((= chrval 127) (msg "\\char94 Q") (msg "$\\langle$Rubout$\\rangle$"))
				       (t (msg (e (if (= chrval 64)
						      (msg "@")
						    (tex-char (code-char chrval))))))))))
		      (msg "[N/A]")))
	    (msg "&\\cr")))
  (msg t "\\vfill\\eject\\end"))


(defun scribechardoc-styles (styles)
  (setq styles (remove-if-not #'(lambda (style) (get style 'char-cat))
			      styles))
  (let ((char-cats (mapcar #'(lambda (style) (get style 'char-cat)) styles))
	sp-chars)
    ;;Now char-cats is a list of the categories with special characters
    ;;corresponding to the styles in styles.
    (msg charheading-scribe)
    (msg t "@ChapterPh(Help for Special Characters)" t)
    (msg t "Listed below is the help for special characters in the styles ")
    (do ((styles styles (cdr styles)))
	((null styles) (msg "." t))
      (msgt (car styles))
      (when (not (null (cdr styles))) (msg ", ")))
    ;;For now we don't use contexts, but that should change soon.
    (setq sp-chars (apply #'append
			  (mapcar #'(lambda (char-cat)
				      (eval (get char-cat 'global-list)))
				  char-cats)))
    (setq sp-chars (unique-sort (remove-if-not #'symbolp sp-chars)
				#'alphalessp))
    (msg t "@Begin(Format)" t "@TabDivide(" (+ (length char-cats) 2) ")" t)
    (msg "Name@\\Look in Scribe")
    (dolist (style* styles) (msg "@\\") (msgt style*))
    (msg t)
    (dolist (sp-char sp-chars)
      (msg t) (msgt sp-char) (msg "@\\")
      (if (get sp-char 'scribe-char)
	  (msg "@" (get sp-char 'dfont))
	  (msg "[N/A]"))
      (dolist (char-cat char-cats)
	(msg "@\\")
	(if (get sp-char char-cat)
	    (case char-cat
	      (scribe-char (msg  "@t[@@" (get sp-char 'dfont) "]"))
	      (tex-char (msgt "\\" (get sp-char 'texname)))
	      (concept-char
	       (let ((chrset (car (get sp-char 'cfont)))
		     (chrval (cdr (get sp-char 'cfont))))
		 (case chrset
		   (0)
		   (1 (msg "f12 "))
		   (2 (msg "f13 "))
		   (3 (msg "f14 ")))
		 (cond ((< chrval 32) (msg "f10 ")
				      (msgt (e (if (= chrval 0)
						   (msg "@@")
						   (tyo (+ chrval 64))))))
		       ((= chrval 127) (msgt "^Q") (msg "<Rubout>"))
		       (t (msgt (e (if (= chrval 64)
				       (msg "@@")
				       (tyo chrval)))))))))
	    (msg "[N/A]")))))
  (msg t "@End(Format)"))

;;;--------------------------------------------------------
;;The following functions are called by others. So I keep them here
;;Some day, they will be moved to an appropriate place.

(defun stringparse (string)
  (do* ((i (length string) (1- i)) (charlist nil (cons (schar string i) charlist)))
       ((eq i 0) charlist)))

(defun list2string (list)
 (if list (concatenate 'string (string (car list)) (list2string (cdr list))) ""))

(defun string-nthcdr (nth string)
   (remove-if #'(lambda (x) (declare (ignore x)) T) string :end nth))
