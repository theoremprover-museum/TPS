;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of OMDOC)
(context coll-help)

					; include preamble and <assertion . . .>

					; date, and lots of information about how, when, who generated it

;;;
;;; File: omdoc
;;; Package: omdoc
;;;
;;; defines an output format for formulas that may be printed by
;;; Omdoc.
;;;

(deffile omdoc
  (part-of omdoc)
  (extension lsp)
  (mhelp "Functions for OMDoc output."))

					; an alist associating gwff's in the library with their names
					; This is used to find which gwff a proof proves.
(defvar *global-gwff-alist* nil)

(defvar needed nil)

(defflag omdoc-rights
  (flagtype string)
  (default "The formalization can be freely distributed, maintaining reference to the TPS source.")
  (subjects maintain)
  (mhelp "The rights listed in metadata of TPS omdoc files."))

(defflag omdoc-catalogue
  (flagtype string)
;  (default "http://www.mathweb.org/omdoc/examples/omstd/catalogue.omdoc")
  (default "../logics/catalogue.omdoc")
  (subjects maintain)
  (mhelp "The omdoc catalogue location."))

(defflag omdoc-type
  (flagtype string)
  (default "Dataset")
  (subjects maintain)
  (mhelp "The type listed in metadata of TPS omdoc files."))

(defflag omdoc-source
  (flagtype string)
  (default "The TPS library: http://gtps.math.cmu.edu/tps.html")
  (subjects maintain)
  (mhelp "The source listed in metadata of TPS omdoc files."))

(defflag omdoc-aut-creator
  (flagtype string)
  (default "The TPS Project")
  (subjects maintain)
  (mhelp "The aut creator listed in metadata of TPS omdoc files."))

(defflag omdoc-trc-creator
  (flagtype string)
  (default "")
  (subjects maintain)
  (mhelp "The trc creator listed in metadata of TPS omdoc files.
If this is the empty string, the userid is used."))

(defmexpr omdoc-assertion
    (argnames wff wff-name filename)
  (argtypes gwff symbol filespec)
  (arghelp "Wff to Print" "Name of Wff" "Output File")
  (defaultfns (lambda (wff wff-name filename)
		(list wff 
		      (if (and (symbolp wff)
			       (eq wff-name '$))
			  wff
			wff-name)
		      (if (eq filename '$)
			  (namestring
			   (make-pathname%
			    :name (string-downcase (symbol-name wff-name))
			    :type "omdoc"))
			filename))))
  (print-command t)
  (mhelp "Print a wff in OMDoc notation."))

(defmexpr omdoc-proof
    (argtypes filespec)
  (argnames filename)
  (defaultfns (lambda (filename)
		(list
		 (if (eq filename '$)
		     (namestring
		      (make-pathname%
		       :name (string-downcase (symbol-name dproof))
		       :type "omdoc"))
		   filename))))
  (print-command t)
  (mhelp "Print the current proof into an OMDoc file
in OMDoc notation."))

(defmexpr omdoc-lib
    (print-command t)
  (mhelp "Print the library into OMDoc files
in OMDoc notation."))

(defmexpr omdoc-class-scheme
  (argnames name)
  (argtypes symbol)
  (arghelp "Classification Scheme")
  (defaultfns (lambda (name) 
		(declare (special class-scheme))
		(list (if (and (eq name '$) class-scheme)
			  class-scheme name))))
  (mainfns omdoc-class-scheme)
  (mhelp "Print the library into OMDoc files using the given
Classification Scheme to collect library items into theories."))

(defun omdoc-assertion (wff wff-name filename)
  (reroute-output 
   filename
   (namestring (make-pathname%
		:name (string-downcase (symbol-name wff-name))
		:type "omdoc"))
   (let* ((title (format nil "~d" wff-name)))
     (omdoc-preamble title)
     (omdoc-assertion-2 wff title)
     (omdoc-postamble))))

(defun omdoc-assertion-2 (wff title &optional (theory "tps"))
  (format t "<assertion type=\"theorem\" id=\"~d\" theory=\"~d\">~%" title theory)
  (format t "<CMP> ")
;  (if cmp
;      (print cmp)
  (pwff wff)
  (format t "</CMP>~%")
  (format t "<FMP>~%<OMOBJ>~%")
  (princ (omdocwff-string wff))
  (format t "~%</OMOBJ>~%</FMP>~%</assertion>~%"))

(defun build-global-gwff-alist ()
  (declare (special core::*lib-masterindex*))
  (maphash #'(lambda (key value)
	       (let ((a (assoc 'gwff value)))
		 (when a
		   (let ((item (core::retrieve-item key :multiple nil :type 'gwff :preferred-dir (directory-namestring (cdr a)))))
		     (when item
		       (push (cons (core::libitem-description item) key) *global-gwff-alist*))))))
	   core::*lib-masterindex*))

(defun find-gwff-in-library (name gwff)
  (let ((item (core::retrieve-item name :multiple nil :type 'gwff :fail-gently t)))
    (if (and item (wffeq gwff (core::libitem-description item)))
	name
      (progn
	(unless *global-gwff-alist*
	  (build-global-gwff-alist))
	(cdr (assoc gwff *global-gwff-alist* :test #'wffeq))))))

(defun omdoc-proof (filename)
  (reroute-output 
   filename
   (namestring (make-pathname%
		:name (string-downcase (symbol-name dproof))
		:type "omdoc"))
   (let* ((title (format nil "~d" dproof))
	  (assertion (get dproof 'represents))
	  (gwff-name 
	   (find-gwff-in-library dproof assertion))) ; this is terrible, but the only way to find the gwff the proof proves, if it is in the library at all.
     (omdoc-preamble title)
     (unless gwff-name			; if it is not one in the current library
       (omdoc-assertion-2 assertion (format nil "gwff.~d" dproof))
       (setq gwff-name dproof))
     (format t "<proof id=\"~d.proof\" for=\"gwff.~d\" theory=\"tps\">~%" title gwff-name)
     (let ((last-line (last (proof-lines dproof))))
       (dolist (l (proof-lines dproof))
	 (if (eq l last-line)
	     (format t "<conclude id=\"gwff.~d\"><CMP>~%We conclude by applying ~d</CMP></conclude>"
		     gwff-name (natural-language-justification title (line-justification l)))
	   (omdoc-proof-line title l))))
     (format t "</proof>~%")
     (omdoc-postamble))))

(defun omdoc-lib ()
  (declare (special core::*lib-masterindex*))
  (let ((already-done nil))
    (declare (special already-done))
    (let ((theories-file (open "THEORIES.TXT" :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (declare (special theories-file))
;    (format theories-file "mltt.omdoc~%") ; fake small imports for when we're testing
;    (format theories-file "BASE.omdoc~%")
;    (format theories-file "quant1.omdoc~%")
;    (format theories-file "fns1.omdoc~%")
      (omdoc-tps-core)
      (maphash #'(lambda (key value)
		   (omdoc-lib-item key value))
	       core::*lib-masterindex*)
      (close theories-file)
      (copy-file "THEORIES.TXT" "ALL_THEORIES.TXT"))))

(defun omdoc-class-scheme (name)
  (let ((current-class (get name 'class-scheme))
	(already-done nil)
	(catalogue-file (open "tps.catalogue.omdoc" :direction :output
			      :if-exists :supersede :if-does-not-exist :create))
	(theories-file (open "THEORIES.TXT" :direction :output
			     :if-exists :supersede :if-does-not-exist :create)))
    (declare (special current-class already-done catalogue-file theories-file))
    (setq needed nil)
    (let ((*standard-output* catalogue-file))
      (omdoc-catalogue-preamble "tps.catalogue" "The TPS Library Theories"))
    (format catalogue-file "<catalogue>~%")
    (omdoc-catalogue-and-theories (list current-class))
    (format catalogue-file "</catalogue>~%")
    (let ((*standard-output* catalogue-file))
      (omdoc-catalogue-postamble))
    (close catalogue-file)
    (close theories-file)
    (omdoc-class-rec current-class)))

(defun omdoc-tps-core ()
  (declare (special core-abbrevlist core-theoremlist))
  (dolist (a core-abbrevlist)
    (when (symbolp a)
      (omdoc-abbr a " - built in abbrev")))
  (dolist (a core-theoremlist)
    (when (symbolp a)
      (omdoc-gwff a " - built in gwff"))))

(defun omdoc-lib-item (name &optional treat-as-abbr)
  (declare (special already-done theories-file))
  (unless (assoc name already-done)
    (let ((item (or (core::retrieve-item name :type 'abbr :multiple nil :fail-gently t)
		    (core::retrieve-item name :type 'gwff :multiple nil :fail-gently t)
		    (core::retrieve-item name :type 'lib-const :multiple nil :fail-gently t))))
      (when item
	(if (eq (core::libitem-type item) 'LIB-CONST)
	  (progn
	    (funcall (core::make-lib-tpsobject 'LIB-CONST) item)
	    (remprop name 'flavor)
	    (when (core::libitem-needed-objects item)
	      (throwfail "Why does the lib const " name " depend on anything?"))
	    (let* ((theory-name (lib-const-omdoc-theory-name name))
		   (omdoc-file (make-pathname% :name theory-name :type "omdoc")))
	      (push (cons (intern name) theory-name) already-done)
	      (format theories-file "~D.omdoc~%" theory-name)
	      (reroute-output
	       omdoc-file
	       (namestring omdoc-file)
	       (omdoc-preamble theory-name (format nil "From to TPS library file:~%~d~%"
						   (core::libitem-file item)))
	       (format t "<theory id=\"~d\">~%" theory-name)
	       (format t "<symbol id=\"const.~d\">~%<CMP>~%" (omdoc-name name))
	       (format t "~D" (mhelp-omdoc-string name))
	       (format t "</CMP>")
	       (format t "<type system=\"simpletypes\"><OMOBJ>~d</OMOBJ></type></symbol></theory>" (omdoc-type-string-1 (get name 'type)))
	       (omdoc-postamble))))
	  (let ((needed (core::libitem-needed-objects item)))
	    (dolist (e needed)
	      (omdoc-lib-item e t))
	    (if (or (eq (core::libitem-type item) 'ABBR)
		    (and treat-as-abbr
			 (eq (core::libitem-type item) 'GWFF)))
		(omdoc-abbr name (format nil " library file:~%~d" (core::libitem-file item)))
	      (if (eq (core::libitem-type item) 'GWFF)
		  (omdoc-gwff name (format nil " library file:~%~d" (core::libitem-file item)))))))))))

(defun omdoc-abbr (name &optional from-info)
  (declare (special already-done theories-file))
  (let* ((theory-name (abbr-omdoc-theory-name name))
	 (omdoc-file (make-pathname% :name theory-name :type "omdoc"))
	 (omdoc-str (open omdoc-file :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (push (cons (intern name) theory-name) already-done)
    (format theories-file "~D.omdoc~%" theory-name)
    (let ((*standard-output* omdoc-str))
      (declare (special *standard-output*))
      (omdoc-preamble theory-name (format nil "From to TPS~d~% " from-info)))
    (format omdoc-str "<theory id=\"~d\">~%" theory-name)
    (let ((needed nil))
      (declare (special needed))
      (let ((str (omdocwff-string (get name 'defn))))
	(dolist (n (remove-duplicates needed))
	  (let ((a (assoc (intern n) already-done)))
	    (unless a
	      (throwfail "Processing: " name ".  Do not know " n))
	    (format omdoc-str "<imports id=\"~d.import.~d\" from=\"~d\" type=\"global\"/>"
		    theory-name (omdoc-name n) (cdr a))))
	(format omdoc-str "<symbol id=\"abbr.~d\">~%" (omdoc-name name))
	(format omdoc-str "<CMP>~%")
	(format omdoc-str "~D" (mhelp-omdoc-string name))
	(format omdoc-str "</CMP>")
	(format omdoc-str "<type system=\"simpletypes\"><OMOBJ>~d</OMOBJ></type></symbol>"
		(omdoc-type-string-1 (get name 'type)))
	(format omdoc-str "<definition for=\"abbr.~d\" id=\"abbr.~d.def\" type=\"simple\">~%"
		(omdoc-name name) (omdoc-name name))
	(format omdoc-str "<FMP><OMOBJ>~d</OMOBJ></FMP></definition>~%" str)
	(format omdoc-str "~%</theory>~%")
	(let ((*standard-output* omdoc-str))
	  (declare (special *standard-output*))
	  (omdoc-postamble))))))

(defun omdoc-gwff (name &optional from-info)
  (declare (special already-done theories-file))
  (let* ((theory-name (gwff-omdoc-theory-name name))
	 (omdoc-file (make-pathname% :name theory-name :type "omdoc")))
    (push (cons (intern name) theory-name) already-done)
    (format theories-file "~D.omdoc~%" theory-name)
    (reroute-output
     omdoc-file
     (namestring omdoc-file)
     (omdoc-preamble theory-name (format nil "From to TPS~d~%" from-info))
     (format t "<theory id=\"~d\">~%" theory-name)
     (let ((needed nil))
       (declare (special needed))
       (let ((str (omdocwff-string (gwff-defn name))))
	 (dolist (n (remove-duplicates needed))
	   (let ((a (assoc (intern n) already-done)))
	     (unless a
	       (throwfail "Processing: " name ".  Do not know " a))
	     (format t "<imports id=\"~d.import.~d\" from=\"~d\" type=\"global\"/>"
		     theory-name (omdoc-name n)
		     (cdr (assoc n already-done)))))
	 (format t "<assertion type=\"theorem\" id=\"gwff.~d\">~%<CMP>~%" name)
	 (format t "~D" (mhelp-omdoc-string name))
	 (format t "</CMP><FMP><OMOBJ>~d</OMOBJ></FMP></assertion>" str)
	 (format t "~%</theory>~%")
	 (omdoc-postamble))))))


(defun gwff-defn (name)
  (getrwff name))
;  (or (get name 'defn)
;      (get name 'represents)
;      (get name 'assertion)))

(defun abbr-omdoc-theory-name (name)
  (declare (special current-class))
  (if (and (boundp 'current-class) (core::libclass-p current-class))
      (let ((cl (find-class-with-item name current-class)))
	(if cl
	    (format nil "tps.~d" (core::libclass-name cl))
	  (progn
	    (msgf "Could not access ABBR " name " from class "
		  (core::libclass-name current-class) t)
	    (throwfail "Class Problem"))))
    (format nil "tps.abbr.~d" (omdoc-name name))))

(defun lib-const-omdoc-theory-name (name)
  (declare (special current-class))
  (if (and (boundp 'current-class) (core::libclass-p current-class))
      (let ((cl (find-class-with-item name current-class)))
	(if cl
	    (format nil "tps.~d" (core::libclass-name cl))
	  (progn
	    (msgf "Could not access LIB-CONST " name " from class "
		  (core::libclass-name current-class) t)
	    (throwfail "Class Problem"))))
    (format nil "tps.const.~d" (omdoc-name name))))

(defun gwff-omdoc-theory-name (name)
  (declare (special current-class))
  (if (and (boundp 'current-class) (core::libclass-p current-class))
      (let ((cl (find-class-with-item name current-class)))
	(if cl
	    (format nil "tps.~d" (core::libclass-name cl))
	  (progn
	    (msgf "Could not access GWFF " name " from class "
		  (core::libclass-name current-class))
	    (throwfail "Class Problem"))))
    (format nil "tps.gwff.~d" (omdoc-name name))))

(defun omdoc-name (name)
  (let ((name-str (format nil "~d" name))
	(str ""))
    (dotimes (i (length (format nil "~d" name-str)))
	     (setq str (format nil "~d~d"
			       str
			       (omdoc-char (aref name-str i)))))
    str))

(defun omdoc-char (c)
  (if (and (< (char-code c) 27) (not (member c '(#\return #\newline))))
      ""
    (case c
      (#\< "&lt;")
      (#\> "&gt;")
      (#\& "&amp;")
      (t (format nil "~d" c)))))
;	(#\* "star")
;	(#\\ "backslash")
;	(#\/ "slash")
;	(#\$ "dollar")
;	(#\& "and")
;	(#\^ "carrot")
;	(#\# "hash")
;	(#\% "in-image")
;	(#\! "bang")
;	(#\+ "plus")
;	(#\< "less")
;	(#\> "greater")
;	(t (format nil "~d" c))

(defun omdoc-proof-line (title l)
  (format t "<derive id=\"~d.proof.~d\">~%" title (linealias l))
  (format t "<CMP> ")
  (format t "Applying ~d, we have~%"
	  (natural-language-justification title (line-justification l)))
  (pwff (line-assertion l))
  (format t "</CMP>~%")
					; method?
  (let ((prems (caddr (line-justification l))))
    (when (member l (line-hypotheses l)) ; The line is a hyp
      (pushnew l prems))
    (dolist (p prems)
      (format t "<premise xref=\"~d.~d\"/>" title (linealias p)))
    (format t "<FMP>~%")
    (dolist (h (set-difference (line-hypotheses l) prems))
      (format t "<assumption id=\"~d.~d\"/>" title (linealias h)))
    (format t "<conclusion id=\"~d.~d\"><OMOBJ>~%" title (linealias l))
    (princ (omdocwff-string (line-assertion l)))
    (format t "</OMOBJ></conclusion></FMP>~%")
    (format t "~%</derive>~%")))

(defun natural-language-justification (title just)
  (let ((rule (car just))
	(trms (cadr just))
	(prems (caddr just)) ret)
    (setq ret (format nil "rule ~d" rule))
    (if prems
	(if (cdr prems)
	    (progn
	      (setq ret (format nil "~d to premisses: " ret))
	      (do ((prs prems (cdr prs)))
		  ((not (cdr prs))
		   (setq ret (format nil "~d & <premise xref=\"~d.~d\"/>" ret title (linealias (car prs)))))
		(setq ret (format nil "~d <premise xref=\"~d.~d\"/>," ret title (linealias (car prs))))))
	  (setq ret (format nil "~d to premiss <premise xref=\"~d.~d\"/>" ret title (linealias (car prems))))))
    (if trms
	(if (cdr trms)
	    (progn
	      (setq ret (format nil "~d (using" ret))
	      (do ((trms2 trms (cdr trms2)))
		  ((not (cdr trms2)) (setq ret (format nil "~d & <OMOBJ>~d</OMOBJ>)/>" ret (omdocwff-string (car trms2)))))
		(setq ret (format nil "~d <OMOBJ>~d</OMOBJ>," ret (omdocwff-string (car trms2))))))
	  (setq ret (format nil "~d (using <OMOBJ>~d</OMOBJ>)" ret (omdocwff-string (car trms))))))
    ret))

(defun omdocwff-string (wff &optional (args ""))
  (let ((omdoc-typevars nil))
    (declare (special omdoc-typevars))
    (let ((str (omdocwff-string-rec wff args)))
      (if omdoc-typevars
	  (let ((tstr ""))
	    (dolist (v omdoc-typevars)
	      (setq tstr (format nil "~d <OMV name=\"~d\"/>" tstr v)))
	    (format nil "<OMBIND><OMS cd=\"simpletypes\" name=\"all-types\"/><OMBVAR>~d</OMBVAR>~d</OMBIND>"
		    tstr str))
	str))))

(defun omdocwff-string-rec (wff &optional (args ""))
  (declare (special needed needed-local current-class))
  (cond ((label-q wff) (apply-label wff (omdocwff-string-rec wff args)))
	((lsymbol-q wff)
	 (let ((f nil))
	   (setq f (cond ((member wff '(AND OR NOT IMPLIES))
			 (format nil "<OMS cd=\"pl0\" name=\"~d\"/>"
				 (string-downcase (format nil "~d" wff))))
			((eq wff 'EQUIV)
			 (format nil "<OMS cd=\"pl0\" name=\"equivalent\"/>"))
			((eq wff 'TRUTH)
			 (format nil "<OMS cd=\"truthval\" name=\"true\"/>"))
			((eq wff 'FALSEHOOD)
			 (format nil "<OMS cd=\"truthval\" name=\"false\"/>"))
			((equality-p wff)
			 (format nil "<OMS cd=\"sthol\" name=\"eq\"/>")) ; fix this cd
			((eq (getnameroot wff) 'IOTA)
			 (format nil "<OMS cd=\"sthold\" name=\"descr-op\"/>")) ; fix this cd
			((or (propsym-q wff)
			     (pmpropsym-q wff))
			 (format nil "<OMV name=\"~d\"/>~%"
				 (omdoc-name (getnameroot wff))))
			((and (boundp 'current-class)
			      (core::libclass-p current-class))
			 (let* ((nr (getnameroot wff))
				(nrs (intern nr)))
			   (let ((cl (find-class-with-item nrs current-class)))
			     (if cl
				 (progn
				   (push nrs needed)
				   (when (and (eq cl current-class) (boundp 'needed-local))
				     (push nrs needed-local))
				   (format nil "<OMS cd=\"tps.~d\" name=\"tps.obj.~d\"/>~%"
					   (core::libclass-name cl)
					   (omdoc-name nr)))
			       (progn
				 (complain "Had trouble referencing " nrs " from class " current-class t
					   "Treating it like a variable" t)
				 (format nil "<OMV name=\"~d\"/>~%"
					 (omdoc-name nr)))))))
			((or (pmabbrev-q wff)
			     (abbrev-q wff))
			 (push (intern (getnameroot wff)) needed)
			 (format nil "<OMS cd=\"~d\" name=\"abbr.~d\"/>~%"
				 (abbr-omdoc-theory-name (getnameroot wff))
				 (omdoc-name (getnameroot wff))))
			(t ; lib constant hopefully
			 (push (intern (getnameroot wff)) needed)
			 (format nil "<OMS cd=\"~d\" name=\"const.~d\"/>~%"
				 (lib-const-omdoc-theory-name (getnameroot wff))
				 (omdoc-name (getnameroot wff))))))
	   (if (string= args "")
	       f
	     (format nil "<OMA>~d ~d</OMA>" f args))))
	((boundwff-q wff)
	 (let ((bvars ""))
	   (let ((f nil))
	     (setq f
		   (if (member (binder wff) '(lambda exists forall))
		       (do ((wff1 wff (cdr wff1)))
			    ((or (not (boundwff-q wff1))
				 (neq (binder wff) (binder wff1)))
			     (cond ((member (binder wff) '(exists forall))
				    (format nil "<OMBIND><OMS cd=\"sthol\" name=\"~d\"/>~% <OMBVAR>~d~% </OMBVAR>~% ~d</OMBIND>" 
					    (string-downcase (format nil "~d" (binder wff)))
					    bvars (omdocwff-string-rec wff1 "")))
				   ((eq (binder wff) 'lambda)
				    (format nil "<OMBIND><OMS cd=\"lambda-calc\" name=\"lambda\"/>~% <OMBVAR>~d~% </OMBVAR>~% ~d</OMBIND>" 
					    bvars (omdocwff-string-rec wff1 "")))
				   (t (throwfail "Don't know binder: " (binder wff)))))
			  (setq bvars (format nil "~d~%   ~d" bvars (omdoc-typedvar-string
								     (caar wff1)))))
		     (omdocwff-string-rec (get-def-binder (binder wff) (bindvar wff) (cdr wff)))))
	     (if (string= args "")
		 f
	       (format nil "<OMA>~d ~d</OMA>" f args)))))
	(t
	 (omdocwff-string-rec (car wff)
			  (format nil
				  "~d ~d" (omdocwff-string-rec (cdr wff) "")
				  args)))))

(defun omdoc-typedvar-string (x)
  (format nil "<OMATTR>~%~d<OMV name=\"~d\"/>~%</OMATTR>~%"
	  (omdoc-type-string (type x))
	  (omdoc-name (getnameroot x))))

(defun omdoc-type-string (y)
  (format nil "  <OMATP>~%   <OMS cd=\"simpletypes\" name=\"type\"/>~%~d  </OMATP>~%"
	  (omdoc-type-string-rec y)))

(defun omdoc-type-string-1 (y)
  (let ((omdoc-typevars nil))
    (declare (special omdoc-typevars))
    (omdoc-type-string-rec y)))

(defun omdoc-type-string-rec (y)
  (declare (special omdoc-typevars))
  (if (consp y)
      (format nil "  <OMA>~%   <OMS cd=\"simpletypes\" name=\"funtype\"/>~%~d~d   </OMA>~%"
	      (omdoc-type-string-rec (cdr y))
	      (omdoc-type-string-rec (car y)))
    (if (typeabbrev-p y)
	(omdoc-type-string-rec (get-type-def y))
      (case y
	(O (format nil "    <OMS cd=\"truthval\" name=\"bool\"/>~%"))
	(cl-user::I (format nil "    <OMS cd=\"ind\" name=\"ind\"/>~%"))
	(t (when (boundp 'omdoc-typevars)
	     (setq omdoc-typevars (adjoin y omdoc-typevars)))
	   (format nil "    <OMV name=\"~d\"/>~%" y))))))

(defun omdoc-preamble (title &optional other-info)
  (format t "<?xml version=\"1.0\" encoding=\"utf-8\" ?>~%<!DOCTYPE omdoc SYSTEM \"omdoc.dtd\" []>~2%")
  (format t "<\!-- Document Automatically Generated by TPS3 by ~d -->~%" (status-userid))
  (format t "<\!-- on ")
  (stringdt) ; date and time
  (format t " -->~%")
  (when other-info
    (format t "<!-- ~d -->~%" other-info))
  (format t "<omdoc id=\"~d\" catalogue=\"tps.catalogue.omdoc\">~%<metadata>~%" title)
  (format t "<Title>~d</Title>~%<Date>" title)
  (multiple-value-bind
      (second minute hour date month year day-of-week daylight-saving-time-p time-zone)
      (get-decoded-time)
    (declare (ignore day-of-week daylight-saving-time-p time-zone))
    (format t "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
	    year month date hour minute second))
  (format t "</Date>~%")
  (format t "<Creator role=\"aut\">~d</Creator>~%" omdoc-aut-creator)
  (format t "<Creator role=\"trc\">~d</Creator>~%"
	  (if (string-equal omdoc-trc-creator "")
	      (status-userid)
	    omdoc-trc-creator))
  (format t "<Format>application/omdoc+xml</Format>~%")
  (format t "<Type>~d</Type>~%" omdoc-type)
  (format t "<Source>~d</Source>~%" omdoc-source)
  (format t "<Rights>~d</Rights>~%" omdoc-rights)
  (format t "</metadata>~%"))

(defun omdoc-catalogue-preamble (id title &optional other-info)
  (format t "<?xml version=\"1.0\" encoding=\"utf-8\" ?>~%<!DOCTYPE omdoc SYSTEM \"omdoc.dtd\" []>~2%")
  (format t "<\!-- Document Automatically Generated by TPS3 by ~d -->~%" (status-userid))
  (format t "<\!-- on ")
  (stringdt) ; date and time
  (format t " -->~%")
  (when other-info
    (format t "<!-- ~d -->~%" other-info))
  (format catalogue-file "<omdoc id=\"tps.catalogue\" catalogue=\"~d\">~%" omdoc-catalogue)
  (format catalogue-file "<metadata>~%")
  (format t "<Title>~d</Title>~%<Date>" title)
  (multiple-value-bind
      (second minute hour date month year day-of-week daylight-saving-time-p time-zone)
      (get-decoded-time)
    (declare (ignore day-of-week daylight-saving-time-p time-zone))
    (format t "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
	    year month date hour minute second))
  (format t "</Date>~%")
  (format t "<Creator role=\"aut\">~d</Creator>~%" omdoc-aut-creator)
  (format t "<Creator role=\"trc\">~d</Creator>~%"
	  (if (string-equal omdoc-trc-creator "")
	      (status-userid)
	    omdoc-trc-creator))
  (format t "<Format>application/omdoc+xml</Format>~%")
  (format t "<Type>~d</Type>~%" omdoc-type)
  (format t "<Source>~d</Source>~%" omdoc-source)
  (format t "<Rights>~d</Rights>~%" omdoc-rights)
  (format t "</metadata>~%"))

(defun omdoc-postamble ()
  (format t "</omdoc>~%"))

(defun omdoc-catalogue-postamble ()
  (format t "<omtext id=\"t\">~%")
  (format t "<CMP> The information in these OMDoc files will be kept up to date by TPS.</CMP>~%")
  (format t "</omtext>~%")
  (format t "</omdoc>~%"))

(defun mhelp-omdoc-string (name &optional (typ 'ABBR))
  (let ((str (make-string-output-stream)))
    (let ((*standard-output* str)
	  (STYLE 'GENERIC))
      (declare (special *standard-output* STYLE))
      (case typ
	(ABBR (princ-mhelp-defn name 'abbrev))
	(GWFF (theorem-help name '%theorem%))
	(t (mhelp name))))
    (let ((mh (omdoc-name (get-output-stream-string str))))
      (close str)
      mh)))

(defun omdoc-catalogue-and-theories (cll &optional already)
  (declare (special catalogue-file theories-file))
  (when cll
    (let ((cl (car cll)))
      (if (member cl already)
	  (omdoc-catalogue-and-theories (cdr cll) already)
	(if (set-difference (core::libclass-parents cl) already)
	    (omdoc-catalogue-and-theories (append (core::libclass-parents cl) cll) already)
	  (progn
	    (let ((theory-name (format nil "tps.~d" (omdoc-name (core::libclass-name cl)))))
	      (format theories-file "~D.omdoc~%" theory-name)
	      (format catalogue-file "<loc theory=\"~d\" omdoc=\"~d.omdoc\"/>~%" theory-name theory-name))
	    (omdoc-catalogue-and-theories (append (cdr cll) (core::libclass-kids cl))
					  (cons cl already))))))))

(defun omdoc-class-rec (cl)
  (declare (special already-done))
  (unless (member cl already-done)
    (push cl already-done)
    (dolist (cl0 (core::libclass-kids cl))
      (omdoc-class-rec cl0))
    (omdoc-class cl)))

(defun omdoc-class (cl)
  (declare (special current-class))
  (setq current-class cl)
  (let* ((theory-name (format nil "tps.~d" (omdoc-name (core::libclass-name cl))))
	 (omdoc-file (make-pathname% :name theory-name :type "omdoc"))
	 (omdoc-str (open omdoc-file :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (let ((*standard-output* omdoc-str))
      (declare (special *standard-output*))
      (omdoc-preamble theory-name)
      (format t "<theory id=\"~d\">~%" theory-name)
      (omdoc-class-imports theory-name cl))
    (omdoc-class-libitems omdoc-str theory-name (core::libclass-libitems cl))
    (format omdoc-str "</theory>~%" theory-name)
    (let ((*standard-output* omdoc-str))
      (declare (special *standard-output*))
      (omdoc-postamble))
    (close omdoc-str)))

(defun omdoc-class-libitems (omdoc-str theory-name items)
  (omdoc-class-libitems-rec omdoc-str theory-name items items nil nil))

(defun omdoc-class-libitems-rec (omdoc-str theory-name items all-items already already-symbs)
  (declare (special needed core::*lib-masterindex*))
  (when items
    (let ((name (car items)))
      (if (member name already-symbs)
	  (omdoc-class-libitems-rec omdoc-str theory-name (cdr items) all-items already already-symbs)
	(let ((oname (omdoc-name name))
	      (i (gethash name core::*lib-masterindex*)))
					; I'm not saving defined binders.  I'm expanding them instead. - cebrown 6/2002
	  (when (member name core-abbrevlist)
	    (push (cons 'abbr :internal-abbrev) i))
	  (when (member name core-theoremlist)
	    (push (cons 'gwff :internal-theorem) i))
	  (when (member name core-constlist)
	    (push (cons 'lib-const :internal-const) i))
	  (let ((labbr (assoc 'ABBR i))
		(lgwff (assoc 'GWFF i))
		(lconst (assoc 'LIB-CONST i))
		(needed-local nil)
		(needed-fst nil)
		(symb-p nil))
	    (declare (special needed-local))
	    (cond (lgwff
		   (%catch%
		    (let ((file (cdr lgwff)))
		      (unless (eq file :internal-theorem)
			(core::retrieve-libobject name :type 'GWFF :multiple nil
						  :preferred-dir (directory-namestring file)
						  :fail-gently nil))
		      (let* ((wff1 (or (get name 'defn) (get name 'represents) (get name 'assertion)))
			     (wff (or (if (stringp wff1) (getrwff wff1) wff1)
				      (progn
					(msgf  "Could not find defn of " name t)
					(throwfail "Problem with " name))))
			     (str (omdocwff-string wff))
			     (hstr (mhelp-omdoc-string name))
			     (tstr (omdoc-type-string-1 'O)))
			(dolist (n needed-local)
			  (when (member n all-items)
			    (unless (member n already-symbs)
			      (push n needed-fst))))
			(unless needed-fst
			  (when (member name needed)
			    (setq symb-p t)
			    (format omdoc-str "<symbol id=\"tps.obj.~d\">~%" oname)
			    (format omdoc-str "<CMP>~%")
			    (format omdoc-str "~D" hstr)
			    (format omdoc-str "~%</CMP>~%")
			    (format omdoc-str "<type system=\"simpletypes\"><OMOBJ>~d</OMOBJ></type></symbol>~%" tstr)
			    (format omdoc-str "<definition for=\"tps.obj.~d\" id=\"abbr.~d.def\" type=\"simple\">~%" oname oname)
			    (format omdoc-str "<FMP><OMOBJ>~d</OMOBJ></FMP></definition>~%" str))
			  (unless (member name already) ; already declared as an assertion earlier, but not as a symbol until now
			    (format omdoc-str "<assertion id=\"gwff.~d\" type=\"conjecture\">~%" oname)
			    (format omdoc-str "<CMP>~%")
			    (format omdoc-str "~D" hstr)
			    (format omdoc-str "~%</CMP>~%")
			    (format omdoc-str "<FMP><OMOBJ>~d</OMOBJ></FMP></assertion>~%" str))))
		      (core::destroy-all-extras))
		    (fail (msgf "Library Problem with GWFF " name t))))
		  (labbr
		   (%catch%
		    (let ((file (cdr labbr)))
		      (unless (eq file :internal-abbrev)
			(core::retrieve-libobject name :type 'ABBR :multiple nil
						  :preferred-dir (directory-namestring file)
						  :fail-gently nil))
		      (let ((str (omdocwff-string (get name 'defn)))
			    (hstr (mhelp-omdoc-string name))
			    (tstr (omdoc-type-string-1 (get name 'type))))
			(dolist (n needed-local)
			  (when (member n all-items)
			    (unless (member n already-symbs)
			      (push n needed-fst))))
			(unless needed-fst
			  (setq symb-p t)
			  (format omdoc-str "<symbol id=\"tps.obj.~d\">~%" oname)
			  (format omdoc-str "<CMP>~%")
			  (format omdoc-str "~D" hstr)
			  (format omdoc-str "~%</CMP>~%")
			  (format omdoc-str "<type system=\"simpletypes\"><OMOBJ>~d</OMOBJ></type></symbol>~%" tstr)
			  (format omdoc-str "<definition for=\"tps.obj.~d\" id=\"abbr.~d.def\" type=\"simple\">~%"
				  oname oname)
			  (format omdoc-str "<FMP><OMOBJ>~d</OMOBJ></FMP></definition>~%" str)))
		      (core::destroy-all-extras))
		    (fail (msgf "Library Problem with ABBR " name t))))
		  (lconst
		   (%catch%
		    (let ((file (cdr lconst)))
		      (unless (eq file :internal-const)
			(core::retrieve-libobject name :type 'LIB-CONST :multiple nil
						  :preferred-dir (directory-namestring file)
						  :fail-gently nil))
		      (let ((hstr (mhelp-omdoc-string name))
			    (tstr (omdoc-type-string-rec (get name 'type))))
			(unless needed-fst
			  (setq symb-p t)
			  (format omdoc-str "<symbol id=\"tps.obj.~d\">~%" oname)
			  (format omdoc-str "<CMP>~%")
			  (format omdoc-str "~D" hstr)
			  (format omdoc-str "~%</CMP>~%")
			  (format omdoc-str "<type system=\"simpletypes\"><OMOBJ>~d</OMOBJ></type></symbol>~%" tstr)))
		      (core::destroy-all-extras))
		    (fail (msgf "Library Problem with LIB-CONST " name t)))))
	    (if needed-fst
		(if (member name needed-fst)
		    (progn
		      (msgf "Library Problem with " name t "Recursively defined?" t)
		      (omdoc-class-libitems-rec omdoc-str theory-name (cdr items) all-items (cons name already) already-symbs))
		  (omdoc-class-libitems-rec omdoc-str theory-name
					    (append needed-fst (set-difference items needed-fst))
					    all-items already already-symbs))
	      (omdoc-class-libitems-rec omdoc-str theory-name (cdr items) all-items (cons name already)
					(if symb-p (cons name already-symbs) already-symbs)))))))))

; assuming importing in omdoc is transitive
(defun omdoc-class-imports (thname cl)
  (declare (special CLASS-DIRECTION))
  (let ((cll (if (eq CLASS-DIRECTION 'DOWN)
		 (core::libclass-kids cl)
	       (core::libclass-parents cl))))
    (dolist (cl0 cll)
      (format t "<imports id=\"~d.import.~d\" from=\"tps.~d\" type=\"global\"/>~%"
	      thname (core::libclass-name cl0) (core::libclass-name cl0)))))

(defun find-class-with-item (name cl)
  (declare (special CLASS-DIRECTION))
  (if (member name (core::libclass-libitems cl))
      cl
    (if (eq CLASS-DIRECTION 'DOWN)
	(find-class-with-item-l name (core::libclass-kids cl))
      (find-class-with-item-l name (core::libclass-parents cl)))))
	  
(defun find-class-with-item-l (name cll)
  (if cll
      (or (find-class-with-item name (car cll))
	  (find-class-with-item-l name (cdr cll)))
    nil))

