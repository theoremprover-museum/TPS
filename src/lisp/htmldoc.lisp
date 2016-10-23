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

(deffile htmldoc
  (part-of auto-doc)
  (extension clisp)
  (mhelp "Allows generation of HTML documentation."))

(context coll-help)

(defmexpr html-doc
  (argtypes dirspec)
  (argnames directory)
  (mainfns html-everything)
  (mhelp "Produce HTML documentation in the specified directory.
This requires an empty directory and a lot of disk space, and
will take quite some time to produce."))

(defun html-everything (directory)
  (let ((show-all-packages T)
	(core::*doing-html* t)
	(open-ul nil)
	(filestack nil)
	(alpha-lower-flag nil)
	(short-help nil)
	(style 'generic)
	(rightmargin 10000)
	(*print-pretty* t)) ; added because of bad pprint function
  (declare (special short-help))
  (dolist (rule (remove-if #'listp global-srulelist))
	  (core::get-rule-help rule (core::find-rule-file rule)))
  (setf (get 'iruledef 'global-list) 'core::global-hrulelist)
  (reorganize)
  (make-htaccess directory)
  (make-html-header directory)
  (reroute-output (make-pathname% :name "categories" :type "html" :directory directory)
		  *default-pathname-defaults*
		  #+TPS(msgf "<html><head><title>TPS Documentation : Category Index</title></head>")
		  #-TPS(msgf "<html><head><title>ETPS Documentation : Category Index</title></head>")
		  #+TPS(msgf "<body bgcolor=\"#FFFFFF\"><center><h1>Categories of TPS objects</h1></center><ul>")
		  #-TPS(msgf "<body bgcolor=\"#FFFFFF\"><center><h1>Categories of ETPS objects</h1></center><ul>")
		  (dolist (cat global-categorylist)
		    (unless (or (listp cat) (string= cat "RULEHELP") (null (eval (get cat 'global-list))))
		      (msgf "<li><a href=\"" (strhtml cat) ".html\">" (string-downcase cat) "</a> : "
			    (get cat 'mhelp-line))))
		  (msgf "</ul><p><hr><br>")
		  (msgf "&copy; 2000, Carnegie Mellon University.<p>")
		  (msgf "<a href=\"http://gtps.math.cmu.edu/tps.html\">TPS homepage</a><p></body></html>")
     )
  (dolist (cat global-categorylist)
    (unless (or (null (eval (get cat 'global-list))) (string= cat "RULEHELP"))
      (reroute-output (make-pathname% :name (strhtml cat) :type "html" :directory directory)
		      *default-pathname-defaults*
		      #+TPS(msgf "<html><head><title>TPS Documentation : " cat "</title></head>")
		      #-TPS(msgf "<html><head><title>ETPS Documentation : " cat "</title></head>")
		      (msgf "<body bgcolor=\"#FFFFFF\"><h1>" cat " : " (get cat 'mhelp-line) "</h1>")
		      (dolist (elt (eval (get cat 'global-list)))
			(if (listp elt)
			    (progn
			      (when open-ul (msgf "</ul>")) (setq open-ul t)
			      (msgf "<h3>" (get (car elt) 'short-id) "</h3> <ul>"))
			  (msgf "<li><a href=\"" (strhtml elt) ".html#" (string-downcase cat) "\">"
				(string-downcase elt) "</a>")))
		      (msgf "</ul><p><hr><br>")
		      (msgf "&copy; 2000, Carnegie Mellon University.<p>")
		      #+TPS(msgf "<a href=\"index.html\">TPS documentation homepage</a><p>")
		      #-TPS(msgf "<a href=\"index.html\">ETPS documentation homepage</a><p>")
		      (msgf "<a href=\"http://gtps.math.cmu.edu/tps.html\">TPS homepage</a><p></body></html>"))))
  (dolist (cat global-categorylist)
    (unless (string= cat "RULEHELP")
    (if (string= cat "TACTIC") (setq short-help t) (setq short-help nil))
    ;;because some tactics are recursive!
    (dolist (elt (eval (get cat 'global-list)))
      (unless (listp elt)
	(let ((file (make-pathname% :name (strhtml elt) :type "html" :directory directory)))
	  (pushnew file filestack)
	  (if (probe-file file)
	      (reroute-output-append file *default-pathname-defaults*
				     (msgf "<p><hr><p>")
				     (msgf "<a name=\"" (string-downcase cat) "\"><h1>" elt " : " cat "</h1></a><tt>")
				     (mhelp-html elt (list cat))
				     #+TPS(msgf "</tt><p><a href=\"index.html\">TPS documentation homepage</a><p>")
				     #-TPS(msgf "</tt><p><a href=\"index.html\">ETPS documentation homepage</a><p>")
				     )
	    (reroute-output file *default-pathname-defaults*
			    #+TPS(msgf "<html><head><title>TPS Documentation : " elt "</title></head>")
			    #-TPS(msgf "<html><head><title>ETPS Documentation : " elt "</title></head>")
			    (msgf "<body bgcolor=\"#FFFFFF\">")
			    (msgf "<a name=\"" (string-downcase cat) "\"><h1>" elt " : " cat "</h1></a><tt>")
			    (mhelp-html elt (list cat))
			    #+TPS(msgf "</tt><p><a href=\"index.html\">TPS documentation homepage</a><p>")
			    #-TPS(msgf "</tt><p><a href=\"index.html\">ETPS documentation homepage</a><p>")
			    )
	    ))))))
  (setf (get 'iruledef 'global-list) 'global-irulelist)
  (dolist (file (remove-duplicates filestack :key 'namestring :test 'string= ))
	  (reroute-output-append file *default-pathname-defaults*
				 (msgf "<p><hr><p></body></html>")
				 (msgf "&copy; 1988-99, Carnegie Mellon University.<p>")
				 (msgf "<a href=\"http://gtps.math.cmu.edu/tps.html\">TPS homepage</a><p></body></html>")))))


;;we never actually use the following, but to HTMLify an arbitrary string,
;;call print-html "arbitrary string" "/home/theorem/project/doc/htmldoc/"
;;where the second argument is a prefix to be added to all embedded links
;;which should be the URL of the home directory of the TPS documentation.
;;(e.g. if prefix is "/users/foo/", then (e.g.) "The flag NUM-OF-DUPS" 
;;will become "The flag <a href="/users/foo/num-of-dups.html">NUM-OF-DUPS</a>"

(defun print-html (string prefix &optional (ignore-tags nil))
  (unless (string= string "") (mhelp-html nil nil string prefix ignore-tags)))

(defun mhelp-html (keyword key-cats &optional (string "") (prefix "") (ignore-tags nil))
  (when (and (string= string "") (eq (car key-cats) 'iruledef)) (setq key-cats '(rulehelp)))
  (let ((string (if (string= string "") (with-output-to-string (*standard-output*) (mhelp-html-2 keyword key-cats))
		  string))
	(l 0)
	(bot1 (char-int #\a))
	(top1 (char-int #\z))
	(bot2 (char-int #\A))
	(top2 (char-int #\Z))
	(star (char-int #\*))
	(eqchr (char-int #\=))
	(hat (char-int #\^))
	(percent (char-int #\%))
	(dash (char-int #\-))
	(fdslash (char-int #\/))
	(plus (char-int #\+))
	(num0 (char-int #\0))
	(num9 (char-int #\9))
	(gt (char-int #\>))
	(lt (char-int #\<))
	(backslash (char-int #\\))
	(inside-link nil)
	(stringlist nil))
    (setq l (length string))
    (do* ((i 0 (1+ i))
	  (elt (when (< i l) (char-int (aref string i))) (when (< i l) (char-int (aref string i)))))
	((= i l) (progn 
		   (unless (null stringlist)
		     (if inside-link  ;; we really shouldn't finish inside a link, but you never know.
			 (msg (apply 'concatenate (cons 'string (nreverse stringlist))))
		       (print-htmlified 
			(apply 'concatenate
			       (cons 'string (nreverse stringlist))) prefix)))
		   (msg t t)))
      (if (or (and (>= elt bot1) (<= elt top1))
	      (and (>= elt bot2) (<= elt top2))
	      (and (>= elt num0) (<= elt num9))
	      (= elt dash) (= elt plus) (= elt fdslash) (= elt percent) (= elt star) (= elt hat) (= elt eqchr))
	  (setq stringlist (cons (string (aref string i)) stringlist)) ;; character to check
	(progn
	  (unless (null stringlist) 
	    (if inside-link
	      (msg (apply 'concatenate (cons 'string (nreverse stringlist))))
	      (print-htmlified (apply 'concatenate
				      (cons 'string (nreverse stringlist))) prefix)))
	  (setq stringlist nil)
	  (if (< elt 32) (msg "<br>")
	    (if (and ignore-tags 
		     (or (and (= elt lt) (< (1+ i) l) (neq (char-int (aref string (1+ i))) 32))
			 ;;a < whose next character isn't a space
			 (and (= elt gt) (> i 0) (neq (char-int (aref string (1- i))) 32))))
			 ;;or a > whose previous character wasn't a space.
		(if (and (eql inside-link 1) ;we previously hit an <a> link
			 (= (char-int (aref string (1+ i))) backslash) ;;next char is a backslash
			 (< (+ 2 i) l) ;;there's a char after that
			 (= (char-int (aref string (+ 2 i))) (char-int #\a))) ;;which is an a
		    (progn (msg (aref string i))
			   (setq inside-link nil))
		  (if (eq inside-link nil) ;;we're not inside a link
		      (if (= elt lt) ;; but now we are
			  (progn (msg (aref string i)) 
				 (if (and (= (char-int (aref string (1+ i))) (char-int #\a))) ;;this is an <a> link
				     (setq inside-link 1)
				   (setq inside-link t)))
			(msg "&gt;")) ;;otherwise it really was a > sign.
		    (if (eq inside-link t) ;;we were inside a (non-<a>) link
			(if (= elt gt) ;;but now we're not
			    (progn (msg (aref string i)) (setq inside-link nil))
			  (msg "&lt;"))
		      (msg (aref string i)))))
	      (if (= elt lt) (msg "&lt;")
		(if (= elt gt) (msg "&gt;")
		  (msg (aref string i)))))))))))

(defun print-htmlified (string &optional (prefix ""))
  (if (string= string "fnord") (msg "<pre>")
    (if (string= string "FNORD") (msg "</pre>")
      (let ((real-symbol (find-symbol-real (string-upcase string) (string= string (string-upcase string)))))
	(if real-symbol
	    (msg "<a href=\"" prefix (strhtml string) ".html\">" 
		 string "</a>")
	  (msg string))))))

(defutil print-html
  (form-type function)
  (keywords printing html)
  (mhelp "PRINT-HTML outputs help messages in HTML format, with links to the
other help messages in TPS. It takes three arguments:
  PRINT-HTML \"arbitrary string\" \"/home/theorem/tps/doc/htmldoc/\" ignore-tags
where the first argument is any string and the second argument is a prefix 
which should be a string containing the URL of the home directory of the 
TPS documentation. The third argument is optional, defaulting to NIL; 
if it's set to T, then PRINT-HTML will attempt to preserve existing HTML tags 
in the input string while still producing correct HTML output; if NIL, it won't 
try to do this. Output is produced on the screen, using the MSG command; it's 
up to the user to redirect it to a file (see the help messages for 
REROUTE-OUTPUT and REROUTE-OUTPUT-APPEND) or a string (using the lisp 
function (with-output-to-string (*standard-output*) <form>)).

For example:
PRINT-HTML \"The flag NUM-OF-DUPS\" \"/users/foo\"
 will return 
The flag <a href=\"/users/foo/num-of-dups.html\">NUM-OF-DUPS</a>

The URL prefix should usually be the localised version given 
above, but if you're running this on a system outside CMU and you want to 
link to the documentation at CMU, use the prefix 
\"http://gtps.math.cmu.edu/htmldoc/\" instead."))

; The line above used to read:
; 
; The URL prefix should usually be the localised (\"/afs/cs...\") version given 

(defun find-symbol-real (string already-upcase)
  (if (member string '("AND" "OR" "THEN" "REPEAT" "DO" "NOT" "THAT" "USE" "TOP" "ALL" "LIST" "SET" "NAME" 
		       "SAME" "UP" "ONE" "SIMILAR" "ASK" "SHOW" "FORGET" "GO" "P" "S" "ALWAYS" "NEVER")
	      :test 'string=)
      already-upcase ;the elements of the list above must be upper-case to be indexed
    (if (member string '("A" "T" "0") :test 'string=)
	nil
      ;; one-character objects are 0, A, D, !, I, =, %, ?, L, O, P, ^, R, S, T. 
      ;; Of these, only A, T and 0 occur often enough to be a nuisance.
      (or (and (find-symbol string)
	       (get (find-symbol string) 'core::contexts))
	  (and (find-symbol string (find-package 'auto))
	       (get (find-symbol string (find-package 'auto)) 'core::contexts))
	  (and (find-symbol string (find-package 'core))
	       (get (find-symbol string (find-package 'core)) 'core::contexts))
	  (and (find-symbol string (find-package 'maint))
	       (get (find-symbol string (find-package 'maint)) 'core::contexts))
	  (and (find-symbol string (find-package 'ml))
	       (get (find-symbol string (find-package 'ml)) 'core::contexts))
	  (and (find-symbol string (find-package 'teacher))
	       (get (find-symbol string (find-package 'teacher)) 'core::contexts))))))

(defun mhelp-html-2 (keyword key-cats)
  (declare (special short-help auto::mate-level auto::mtree-level rightmargin))
  (if (integerp keyword) 
      (if (zerop keyword) (setq keyword '|0|) (setq keyword 'weeble-ftang-banana)))
;couldn't find the unif/mateop 0 otherwise. OK, so it's a hack. I admit it... MB 4/94
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
	    (msgf "As a " (cat-hack (car cats) 'mhelp-line) ":" t))
      (funcall (cat-hack (car cats) 'mhelp-fn) keyword (car cats))))

(defun strhtml (symbol-or-string)
  (let ((firstval (string-downcase symbol-or-string))
	(in-front 0)
	(bot (char-int #\a))
	(top (char-int #\z))
	(dash (char-int #\-))
	(num0 (char-int #\0))
	(num9 (char-int #\9)))
    (do* ((i 0 (1+ i))
	  (elt (char-int (aref firstval i)) (when (< i (length firstval)) (char-int (aref firstval i)))))
	((= i (length firstval)) (if (zerop in-front) firstval 
				   (concatenate 'string (princ-to-string in-front) "~" firstval)))
      (when (or (< elt bot) (> elt top))
	(unless (or (= elt dash) (and (>= elt num0) (<= elt num9)))
	  (setq in-front (+ (* in-front 10) (mod elt 10)))
	  (setf (aref firstval i) (code-char (+ bot (mod elt 26)))))))))
      
#+TPS(defun make-html-header (directory)
  (reroute-output (make-pathname% :name "index" :type "html" :directory directory)
		  *default-pathname-defaults*
		  (msgf "<html><head><title>TPS Documentation : Main Index</title></head>")
		  (msgf "<body bgcolor=\"#FFFFFF\"><center><h1>TPS ONLINE DOCUMENTATION</h1></center><ul>")
		  (msgf "<li><a href=\"mexpr.html\">The Main Top-Level Commands</a>.")
		  (msgf "<li><a href=\"iruledef.html\">Natural Deduction Rules</a>.")
		  (msgf "<li><a href=\"tactic.html\">Tactics</a> and <a href=\"tactical.html\">Tacticals</a>.")
		  (msgf "<li><a href=\"mateop.html\">The Matings Top Level</a>.")
		  (msgf "<li><a href=\"unifop.html\">The Unification Top Level</a>.")
		  (msgf "<li><a href=\"testcmd.html\">The Test Top Level</a>.")
		  (msgf "<li><a href=\"mtreeop.html\">The Matingstree Top Level</a>.")
		  (msgf "<li><a href=\"edop.html\">The Formula Editor</a>.")
		  (msgf "<li><a href=\"librarycmd.html\">The Library</a>.")
		  (msgf "<li><a href=\"reviewcmd.html\">The Review Top Level</a>.")
		  (msgf "<li><a href=\"flag.html\">Flags</a> and <a href=\"flag-mode.html\">Modes</a>.")
		  (msgf "<li><a href=\"wffop.html\">Wffops</a> and <a href=\"7~wffrecl.html\">Recursive Wffops</a>.")
		  (msgf "<li><a href=\"argtype.html\">Argument Types</a>.")
		  (msgf "<li><a href=\"gexpr.html\">The Grader Top Level</a>.")
		  (msgf "<li><a href=\"77~ltheoreml.html\">Theorems</a>, <a href=\"savedwff.html\">Saved Wffs</a> and <a href=\"abbrev.html\">Abbreviations</a>.")
		  (msgf "<li><a href=\"typeabbrev.html\">Type abbreviations</a> and <a href=\"typeconst.html\">Type constants</a>.")
		  (msgf "<li><a href=\"binder.html\">Binders</a>, <a href=\"logconst.html\">Constants</a>, <a href=\"pmpropsym.html\">Polymorphic Symbols</a> and <a href=\"repsymbol.html\">Replaceable Symbols</a>.")
		  (msgf "<li><a href=\"getgwfftype.html\">Wff types</a> and <a href=\"libobject.html\">Library object types</a>.")
		  (msgf "<li><a href=\"lisp-pack.html\">Packages</a>, <a href=\"module.html\">Modules</a> and <a href=\"tps-file.html\">Files</a>.")
		  (msgf "<li><a href=\"review-subject.html\">Subjects</a>, <a href=\"context.html\">Contexts</a> and <a href=\"categories.html\">Categories</a>.")
		  (msgf "<li>Special characters for <a href=\"concept-char.html\">Concept</a>, <a href=\"scribe-char.html\">Scribe</a> and <a href=\"tex-char.html\">TeX</a>.")
		  (msgf "<li><a href=\"utility.html\">Utilities</a> and <a href=\"info.html\">Information</a>.")
		  (msgf "<li><a href=\"toplevel.html\">Other Top Levels</a>.")
		  (msgf "<li><a href=\"device-style.html\">Device Styles</a>.")
		  (msgf "<li><a href=\"flavor.html\">Flavors</a>.")
		  (msgf "<li><a href=\"printprop.html\">Printing Properties</a>.")
		  (msgf "<li><a href=\"monitorfn.html\">Monitor Functions</a>.")
		  (msgf "<li><a href=\"ordercomponents.html\">Settings for Order-Components</a>.")
		  (msgf "<li><a href=\"event.html\">Events</a>.")
		  (msgf "</ul><p><hr><br>")
		  (msgf "<p><a href=\"http://gtps.math.cmu.edu/tps-mans.html\">TPS Manuals in Postscript form.</a><p><hr><br>")
		  (msgf "&copy; 1988-99, Carnegie Mellon University.<p>")
		  (msgf "This page was automatically created by TPS on ") (stringdtl) (msgf "<p>")
		  (msgf "<a href=\"http://gtps.math.cmu.edu/tps.html\">TPS homepage</a><p></body></html>")
     ))

#-TPS(defun make-html-header (directory)
  (reroute-output (make-pathname% :name "index" :type "html" :directory directory)
		  *default-pathname-defaults*
		  (msgf "<html><head><title>ETPS Documentation : Main Index</title></head>")
		  (msgf "<body bgcolor=\"#FFFFFF\"><center><h1>ETPS ONLINE DOCUMENTATION</h1></center>")
		  (msgf "<h2>BASIC FEATURES OF ETPS</h2></center><ul>")
		  (msgf "<li><a href=\"mexpr.html\">The Main Top-Level Commands</a>.")
		  (msgf "<li><a href=\"srule.html\">Natural Deduction Rules for constructing proofs</a>.")
		  (msgf "<li><a href=\"edop.html\">The Formula Editor</a>.")
		  (msgf "<li><a href=\"reviewcmd.html\">The Review Top Level</a>.")
		  (msgf "<li><a href=\"flag.html\">Flags</a> and <a href=\"flag-mode.html\">Modes</a>.")
		  (msgf "<li><a href=\"77~ltheoreml.html\">Theorems</a>, <a href=\"savedwff.html\">Saved Wffs</a> and <a href=\"abbrev.html\">Abbreviations</a>.")
		  (msgf "<li><a href=\"typeabbrev.html\">Type abbreviations</a> and <a href=\"typeconst.html\">Type constants</a>.")
		  (msgf "<li><a href=\"binder.html\">Binders</a>, <a href=\"logconst.html\">Constants</a>, <a href=\"pmpropsym.html\">Polymorphic Symbols</a> and <a href=\"repsymbol.html\">Replaceable Symbols</a>.")
		  (msgf "<li><a href=\"toplevel.html\">Other Top Levels</a>.")
		  (msgf "</ul><p><hr><br>")
		  (msgf "<h2>ADVANCED FEATURES OF ETPS</h2></center><p>Students using ETPS will probably not need any of the following:<ul>")
		  (msgf "<li><a href=\"wffop.html\">Wffops</a> and <a href=\"7~wffrecl.html\">Recursive Wffops</a>.")
		  (msgf "<li><a href=\"argtype.html\">Argument Types</a>.")
		  (msgf "<li><a href=\"gexpr.html\">The Grader Top Level</a>.")
		  (msgf "<li><a href=\"getgwfftype.html\">Wff types</a>.")
		  (msgf "<li><a href=\"lisp-pack.html\">Packages</a>, <a href=\"module.html\">Modules</a> and <a href=\"tps-file.html\">Files</a>.")
		  (msgf "<li><a href=\"review-subject.html\">Subjects</a>, <a href=\"context.html\">Contexts</a> and <a href=\"categories.html\">Categories</a>.")
		  (msgf "<li>Special characters for <a href=\"concept-char.html\">Concept</a>, <a href=\"scribe-char.html\">Scribe</a> and <a href=\"tex-char.html\">TeX</a>.")
		  (msgf "<li><a href=\"utility.html\">Utilities</a> and <a href=\"info.html\">Information</a>.")
		  (msgf "<li><a href=\"device-style.html\">Device Styles</a>.")
		  (msgf "<li><a href=\"flavor.html\">Flavors</a>.")
		  (msgf "<li><a href=\"printprop.html\">Printing Properties</a>.")
		  (msgf "<li><a href=\"event.html\">Events</a>.")
		  (msgf "</ul><p><hr><br>")
		  (msgf "<p><a href=\"http://gtps.math.cmu.edu/tps-mans.html\">TPS Manuals in Postscript form.</a><p><hr><br>")
		  (msgf "&copy; 1988-99, Carnegie Mellon University.<p>")
		  (msgf "This page was automatically created by ETPS on ") (stringdtl) (msgf "<p>")
		  (msgf "<a href=\"http://gtps.math.cmu.edu/tps.html\">TPS homepage</a><p></body></html>")
     ))

(defun make-htaccess (directory)
  (reroute-output (make-pathname% :name ".htaccess" :directory directory)
		  *default-pathname-defaults*
		  (msgf "<Limit GET>" t "order deny,allow" t "allow from all" t "</Limit>" t)))