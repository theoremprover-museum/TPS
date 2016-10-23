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
;;; File LATEXDOC
;;; Module: AUTO-DOC

;;;
;;; defines functions necessary to produce Latex-able documentation
;;; automatically from  TPS2.
;;;

(deffile latexdoc
  (part-of auto-doc)
  (extension clisp)
  (mhelp "Allows generation of LaTeX-able documentation."))

(context coll-help)

(defmexpr latex-doc
  (argtypes tpscatlist contextlist filespec)
  (argnames category-list context-list filename)
  (defaultfns (lambda (category-list context-list filename)
		(list category-list context-list
		      (if (eq filename '$)
			  (make-pathname% :name "facilities" :type "tex")
			  filename)))) 
  (mainfns latex-cats-mexpr)
  (mhelp "Produce Latex documentation about the specified categories."))

(defun latex-cats-mexpr (category-list context-list filename)
  (latex-cats category-list context-list filename t))

(defmexpr latex-quick-ref
  (argtypes filespec)
  (argnames filename)
  (defaultfns (lambda (filename)
		(list (if (eq filename '$)
			  (make-pathname% :name "quick-ref-auto" :type "tex")
			   filename))))
  (mainfns latex-quick-ref-mexpr)
  (mhelp "Produce a quick Latex reference to the rules available in TPS."))

;(defvar global-srulelist) ;Already defined in scrdoc.lisp

(defun latex-quick-ref-mexpr (filename)
  (latex-quick-ref filename t))

(defun latex-quick-ref (filename &optional (mexpr nil))
  (declare (special latex-preamble))
  ;;To be adapted (scribe -> latex)
  (if mexpr
  (reroute-output filename (make-pathname% :name "quick-ref-auto" :type "tex")
		  (msg latex-preamble t)
		  (msg "\\begin{description}" t)
		  (dolist (srule global-srulelist (msg t "\\end{description}" t "\\end{document}"))
			  (msg T)
			  (if (atom srule)
			      (msg "\\item[\\texttt{" srule "}] "
	       (cdar (get srule 'mhelp)))
	  (msg t "\\item[{\\bf " (get (car srule) 'short-id) "}]"))))
  (reroute-output filename (make-pathname% :name "quick-ref-auto" :type "tex")
    (msg "\\begin{description}" t)
    (dolist (srule global-srulelist (msg t "\\end{description}" t "\\end{document}"))
      (msg T)
      (if (atom srule)
	  (msg "\\index{" srule "} \\ref{" srule "} "
	       (cdar (get srule 'mhelp)))
	  (msg t "{\\bf " (get (car srule) 'short-id) "}"))))))

(defmacro latex-msgt (&rest l)
  `(msg "\\texttt{" ,@l "}"))

(defmacro latex-msgi (&rest l)
  `(msg "\\textit{" ,@l "}"))

(defun latex-cats (category-list context-list filename &optional (mexpr nil))
  (if mexpr
      (reroute-output filename (make-pathname% :name "facilities" :type "tex")
		      ;(msg "\\documentclass[a4paper]{report}" t
			;   "\\usepackage{makeidx}" t
			;   "\\usepackage{listings}" t
			;   "\\usepackage{alltt}" t
			;   "\\usepackage[T1]{fontenc}" t
			;   "\\lstset{breaklines=true,languague=Lisp" t
			;   "\\makeindex" t)
		      (dolist (cat category-list)
			      (latex-cat cat context-list)))
  (reroute-output filename (make-pathname% :name "facilities" :type "tex")
    (dolist (cat category-list)
      (latex-cat cat context-list)))))

(defun latex-cat (category context-list)
  (msgf "\\chapter{" (cat-plural (get category 'mhelp-line)) "}" t)
  (cond ((equal category 'context)
	 (latex-context context-list))
	((equal category '%theorem%)
	 (msgf)
	 (latex-cat-long category context-list)
	 (msgf))
	(t
	 (latex-cat-short category context-list)
	 (msgf)
	 (latex-cat-long category context-list)
	 (msgf))))

(defun latex-context (context-list)
  (when (get 'context 'cat-help) (msg (get 'context 'cat-help) t))
  (msg "The internal name of this category is \\texttt{CONTEXT}." t
       (an-a (get 'context 'mhelp-line) t) " can be defined using " t
       (get 'context 'define)
       "." t "Allowable properties are: ")
  (do ((props (get 'context 'properties) (cdr props)))
      ((null props))
    (latex-msgt (car props))
    (if (cdr props) (msg ", ")))
  (if (eq (get 'context 'other-prop) 'prop-error)
      (msg "." t)
      (msg ", and more." t))
  (msgf "\\begin{description}")
  (dolist (item (eval (get 'context 'global-list)))
    (when (and (symbolp item) (member item context-list))
      (msgf "\\item[" item "] is for " (string-downcase (get item 'short-id)) ".")
      (princ-mhelp item 'context)
      (msg -2)))
  (msgf "\\end{description}"))

(defun latex-cat-short (category context-list)
  (declare (ignore context-list))
  (if (get category 'cat-help) (msg (get category 'cat-help) t))
  (msg "The internal name of this category is " t category ".\\\\" t
       (an-a (get category 'mhelp-line) t) " can be defined using "
       (if (string= category "TACTIC") 'DEFTACTIC (get category 'define)) ;o/w tactic shows up as "defnonsense" !!
       "." t "Allowable properties are: ")
  (do ((props (get category 'properties) (cdr props)))
      ((null props))
    (latex-msgt (car props))
    (if (cdr props) (msg ", ")))
  (if (eq (get category 'other-prop) 'prop-error)
      (msg "." t)
      (msg ", and more." t))  )

(defun latex-next-context (lis)
  (if (or (null lis) (consp (car lis))) lis (latex-next-context (cdr lis))))

(defun latex-cat-long (category context-list)
  (reorganize-global-list category)
  (do ((cat-items (eval (get category 'global-list))
		  (if skip (latex-next-context (cdr cat-items)) (cdr cat-items)))
       (skip nil nil))
      ((null cat-items) (msgf))
    (if (symbolp (car cat-items))
	(progn
	  ;; changed to show all files, no need to export their names DAN
	 (when (or show-all-packages (eq category 'tps-file) 
		   (accessible-p (car cat-items)))
	   (latex-doc-item (car cat-items) category))
	 (if (or (not (cdr cat-items))
		 (not (symbolp (cadr cat-items))))
	     (msg t "\\item" t "\\end{description}" t) (msgf t)))
	(if (and (or show-all-packages (accessible-p (caar cat-items)))
		 (member (caar cat-items) context-list))
	    (progn
	     (msgf t "\\section{" (get (caar cat-items) 'short-id) "}" -2)
	     (case category
	       ((scribe-char concept-char tex-char)
		(msg "\\begin{description} "
		     t ))
	       ((abbrev logconst binder pmpropsym repsymbol)
		(msg "\\begin{description} " t ))
	       (t (msg "\\begin{description} " t))))
	    (setq skip t)))))

(defun latex-doc-item (item category)
  (case category
    (toplevel (latex-doc-toplevel item category))
    (flag (latex-doc-flag item category))
    ((abbrev logconst binder) (latex-doc-log item category))
    (repsymbol (latex-doc-repsym item category))
    ((scribe-char concept-char tex-char) (latex-doc-char item category))
    (tps-file (latex-doc-file item category))
    (module (latex-doc-feature item category))
    (flag-mode (latex-doc-mode item category))
    (srule (latex-doc-rule item category))
    (%theorem% (latex-doc-theorem item category))
    (review-subject (latex-doc-subject item category))
    (mexpr (latex-doc-mexpr item))
    (auto:tactic (latex-doc-tactic item category)) 
    (t (latex-doc-other item category))))

(defun latex-doc-index (item category)
  (let ((cleanitem (latex-doc-sanitize-help (format nil "~A" item))))
    (case category
      (auto:tactic (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf Tactic}} "))
      (toplevel (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf Command}} "))
      (flag (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf Flag}} "))
      ((scribe-char concept-char tex-char) (msg "\\item[" cleanitem "] \\index{" cleanitem 
						"@" cleanitem ", {\\bf Character}} "))
      (tps-file (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf File}} "))
      (module (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf Module}} "))
      (flag-mode (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf Flag-Mode}} "))
      (srule (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf Srule}} "))
      (mexpr (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf MExpr}} "))
      (%theorem% (msg "\\item[" cleanitem "] \\index{" cleanitem "@" cleanitem ", {\\bf Theorem}} "))
      (t (msg "\\item[" cleanitem "] \\index{" cleanitem "} ")))))

(defun latex-doc-mexpr (item)
  (latex-doc-command (latex-doc-sanitize-help (format nil "~A" (symbol-name item)))
		     (get item 'argnames)
		     (cdr (assoc 'mexpr (get item 'mhelp)))))

(defun latex-doc-tactic (item category)
  (latex-doc-index item category)
  (msg "Defined for the following uses:" t)
  (msg "\\begin{description}" t)
  (dolist (prop (mapcar #'(lambda (use) (cons use (get item use)))
			auto::*tactic-use-list*))
    (when (cadr prop)
      (msg  "\\item["(car prop) ":] ")
      (if (auto::compound-tac-p (cadr prop))
	  (progn 
	    (pprint (cadr prop))
	    (msg t))
	  (msg " is a primitive tactic."))
      (when (caddr prop)
	(msg (caddr prop)))
      (msg -2)))
  (msg "\\end{description}" t))

(defun latex-doc-subject (item category)
  (latex-doc-index item category)
  (in-auto-doc-mode (princ-mhelp item category))
  (msgf t "\\begin{tabular}{l l}")
  (do ((subject-flags (mapcar #'string-downcase
			      (sort (copy-list
				     (remove-if-not 
				      #'(lambda (flag) (and (symbolp flag)
							    (member item
								    (get flag 'subjects))
							    (accessible-p flag)))
				      global-flaglist))
				    #'string<))))
      ((null subject-flags) (msgf "\\end{tabular}"))
    (msgf (pop subject-flags))
    ;(when (car subject-flags) (msg "&" (pop subject-flags)))
    (when (car subject-flags) (msg "&" (pop subject-flags) "\\\\"))
    ))

(defun latex-doc-toplevel (item category)
  (latex-doc-index item category)
  (princ-mhelp-latex item 'toplevel)
  (msgf "Its prompt is " (funcall (get item 'top-prompt-fn) "\\textit{n}")
	 " and it takes " (get (get item 'top-level-category) 'mhelp-line)
	  "s as input. "))

;;; index should be a string which gives the indexing, e.g. "@IndexMexpr(FOO)",
;;; for the command being scribed, args is the list of argnames for the item, 
;;; and description should be a description of what the command does.

(defun latex-doc-command (index args description)
  (msgf "\\item[\\parbox{\\textwidth}{" index )
  (dolist (arg args)
      (msg " ")
      (msg "\\textit{" (string-downcase (string arg)) "}"))
  (msg "}] \\index{" index "@" index ", {\\bf MExpr}} " t)
  (if description
      (if (consp description)
	  (eval (cons 'msgf description))
	  (msgf (latex-doc-sanitize-help (format nil "~A" description))))
	  ;(msgf description))
      (msgf "No further help available.  Sorry.")))

(defun latex-doc-flag (item category)
  (latex-doc-index item category)
  (princ-mhelp-latex item category)
  (msgf "It takes values of type " (get item 'flagtype)
       " and belongs to subjects ")
  (mapl #'(lambda (subj) (latex-msgt (car subj))
		  (if (cdr subj) (msg ", ") (msg ".  ")))
	(get item 'subjects))
  (msg "The default value is: \\begin{lstlisting}" t)
  (funcall (get (get item 'flagtype) 'printfn) (get item 'default))
  (msg t "\\end{lstlisting}" t))

(defun latex-doc-mode (item category)
  (latex-doc-index item category)
  (princ-mhelp item category)
  (msg " The settings of the flags are:" t
       "\\begin{description}")
  (dolist (setting (get item 'flag-settings))
    (msgf "\\item[" (car setting)  "] "  (cadr setting) -2))
  (msgf "\\end{description}" t))

(defun latex-doc-log (item category)
  (latex-doc-index item category)
  (msg "\\begin{tabular}{l l l l}" t)
  (when (or (get item 'dfont) (get item 'face))
	(in-auto-doc-mode
	 (let ((style 'tex))
	   (msg "\\texttt{\$" (item . fsym) "\$}"))))
  (msg "&")
  (cond ((get item 'infix) (msg (get item 'infix) " (Infix)"))
	((get item 'prefix) (msg (get item 'prefix) " (Prefix)")))
  (msg "&")
  (when (get item 'defn)
	(in-auto-doc-mode
	 (let ((style 'tex))
	   (msg "\\texttt{")
	   (setcurpos 0)
	   (prtwff (get item 'defn))
           (msg "}. \\\\ "))))
  (when (get item 'mhelp)
	(when (get item 'defn) (msgf " & & & "))
	(princ-mhelp item category))
  (msgf "\\end{tabular}"))

(defun latex-doc-repsym (item category)
  (latex-doc-index item category)
  (msg "\\texttt{")
  (in-auto-doc-mode
   (let ((style 'tex))
     (princ-rephelp item category)))
  (msg "}")
  (msgf))


(defun latex-doc-char (item category)
  (latex-doc-index item category)
  (when (get item 'dfont)
	(in-auto-doc-mode
	 (let ((style 'tex))
		 (latex-msgt "$"(item . fsym) "$"))))
  (when (get item 'mhelp) (msg t) (princ-mhelp-latex item category))
  (msgf))

(defun latex-doc-file (item category)
  (latex-doc-index item category)
  (if (get item 'part-of)
      (msg " Part of the " (get item 'part-of) " module. " f)
      (msg " Not part of any module. " f))
  (princ-mhelp-latex item category))

(defun latex-doc-feature (item category)
  (latex-doc-index item category)
  (princ-mhelp-latex item category)
  (msgf "It consists of:" T)
  (latex-feat-list item (case category
			   (module '(macro-files files needed-modules)))))
		
(defun latex-feat-list (item proplist)
  (latex-doc-index item proplist)
  (msg "\\begin{description}" t)
  (dolist (prop proplist)
    (when (get item prop)
      (msg "\\item[" (string-downcase (string prop)) ":] ")
      (dolist (elt (get item prop)) (latex-msgt elt 3))
      (msg -2)))
  (msg "\\end{description}" t))

(defun latex-doc-other (item category)
  (latex-doc-index item category)
  (in-auto-doc-mode 
   (let ((style 'tex))
     (princ-mhelp-latex item category))))

(defun latex-doc-rule (rule category)
  (declare (ignore category))
  (msg t "\\item[\\parbox{\\textwidth}{" rule "}] \\index{" rule "@" rule ", {\\bf Rule}}\\label{" rule "}")
  (in-auto-doc-mode (princ-mhelp rule 'mexpr))
  (msg "\\\\" t)
  (msg "\\texttt{ \\setubspaces{ ")
  (%catch% (in-auto-doc-mode
	    (let ((auto-doc-flag T)(style 'tex))
	      (help-rule rule)))
	   (fail (msg "Some error has occurred in TPS.")))
  (msg "}}" t))

(defun latex-doc-theorem (theorem category)
  (msg t "\\item[\\parbox{\\textwidth}{" theorem "\\index{" theorem "@" theorem ", {\\bf Theorem}} ")
  (when (and (assoc category (get theorem 'mhelp))
	     (not (string= (cdr (assoc category (get theorem 'mhelp))) "")))
    (msg (cdr (assoc category (get theorem 'mhelp))) ))
  (msg "}] ")				
  (in-auto-doc-mode
   (msg "\\texttt{")
   (setcurpos 0)
   (if (get theorem 'fol) ;;If we are printing an exercise from first order
       ;; logic, we don't want to print types.
       (let ((printtypes nil))
	 (prtwff (funcall (get 'theorem-type 'getfn) theorem) (style 'tex) ))
       (prtwff (funcall (get 'theorem-type 'getfn) theorem) (style 'tex)))
   (msg "}"))
  )

(defun latex-replace-all (string part replacement &key (test #'char=))
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

(defun latex-doc-sanitize-help (description)
  (latex-replace-all 
   (latex-replace-all 
    (latex-replace-all
     (latex-replace-all
      (latex-replace-all
       (latex-replace-all
	description
        "\\" "\\textbackslash ")
       "$" "\\$ ")
      "%" "\\% ")
     "^" "\\textasciicircum ")
    "#" "\\# ")  
   "&" "\\& ")
 )

(defun princ-mhelp-latex (keyword category)
  (let ((mhelp (or (categ-mhelp keyword category)
		   (when (eq category 'edop)
		     (categ-mhelp (get keyword 'alias) 'wffop)))))
    (if mhelp 
	(if (consp mhelp) 
	    (msg f (latex-doc-sanitize-help (with-output-to-string (*standard-output*)
					      (eval (cons 'msgf mhelp)))))
	    (msg  f (latex-doc-sanitize-help (format nil "~A" mhelp))))
	(msgf "No more help available.  Sorry."))))


