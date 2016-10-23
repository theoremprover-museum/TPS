;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of tpsdef)

(deffile argtyp
  (part-of tpsdef)
  (extension lsp)
  (mhelp "Contains the definitions of the ARGTYPE category plus 
some common argument types which don't belong anywhere else."))


(context basics)

(eval-when (load compile eval)
(defcategory argtype
  (define deftype%%)
  (properties
    (testfn (get-fnspec testfn argtype))
    (getfn (get-fnspec getfn argtype))
    (printfn (get-fnspec printfn argtype))
   (short-prompt single)
   (mhelp single))
  (global-list global-argtypelist)
  (mhelp-line "argument type")
  (mhelp-fn princ-mhelp))
)

(push (cons 'deftype% 'argtype) global-definelist)

(eval-when (load compile eval)
(defmacro deftype% (name &rest props)
  (let* ((testfn-form (cdr (assoc 'testfn props)))
	 (getfn-form (cdr (assoc 'getfn props)))
	 (printfn-form (cdr (assoc 'printfn props)))
	 (testfn-name (conc-names name "--ARGTYPE-TESTFN"))
	 (getfn-name (conc-names name "--ARGTYPE-GETFN"))
	 (printfn-name (conc-names name "--ARGTYPE-PRINTFN"))
	 (other-props (remove 'testfn
			      (remove 'getfn
				      (remove 'printfn props
					      :key #'car)
				      :key #'car)
			      :key #'car))
	 (testfn 
	  (get-fnspec testfn-form name testfn-name))
	 (getfn (if (equal getfn-form '(testfn))
		    `(defun ,getfn-name (,name)
		       (cond ((funcall (get ',name 'testfn) ,name) 
			      ,name)
			     (t (complain ,name " is not of type "
					  ',name ".")
				(throwfail))))
		  (get-fnspec getfn-form name getfn-name)))
	 (printfn (if printfn-form 
		      (get-fnspec printfn-form name printfn-name)
		    'princ)))
    (setq other-props
      (append other-props
	      (list 
	       (cons 'testfn 
		     (if (listp testfn) (list testfn-name) testfn-form))
	       (cons 'getfn (if (listp getfn) (list getfn-name) getfn-form))
	       (cons 'printfn (if (listp printfn) 
				  (list printfn-name) printfn-form)))))
  `(eval-when (load compile eval)
     ,(if (listp testfn) testfn)
     ,(if (listp getfn) getfn)
     ,(if (listp printfn) printfn)
     (deftype%% ,name ,@other-props))))
)


;;;
;;; DEFLISTTYPE is a macro that expands into a deftype%.
;;;
;;; (DEFLISTTYPE list-type single-type rest-props)
;;; defines the type of lists of type single-type.  rest-props can be used
;;; to override any inheritance of properties from the single-type, typically
;;; used for the MHELP property.
;;;
;;; For an alternative form, see the DEFUTIL

(push '(deflisttype . argtype) global-definelist)

;;; Changed both deflisttype & defconstype to be more robust. cpk
;;; [Singleton lists for LISTTYPE, rejecting non-pairs for CONSTYPE]


;;; redefined way that catchget is given functional
;;; value in hope that compiler will like it better 8/5/87 DAN

;*;I just can't understand the purpose of the 2 nested multiple binds
;*; in the GETFN in this macro. I am modifying that portion. SI 6 AUG 87

(defmacro deflisttype (listtype singletype &rest restprops)
  (let ((other-keys (assoc 'other-keys restprops)))
    (when other-keys
      (setq restprops (remove other-keys restprops))
      (setq other-keys (cdr other-keys)))
    `(deftype% ,listtype
       (getfn
	 (lambda (,listtype &optional (rest nil))
	   (cond ,@other-keys
		 ((and (listp ,listtype) (not (cdr (last ,listtype))))
		  (let ((getfn* (get ',singletype 'getfn)))
		    (mapcar #'(lambda (elt) (funcall getfn* elt))
			    ,listtype)))
		 ((atom ,listtype)
		  (let ((getfn* (get ',singletype 'getfn)))
		    (mapcar #'(lambda (elt) (funcall getfn* elt))
			    (cons ,listtype rest))))
		 (t (throwfail ,listtype
			       " is neither a singleton nor a list.")))))
       (testfn (lambda (,listtype
			  &optional (testfn* (get ',singletype 'testfn)))
		 (position-if-not  testfn* ,listtype)))
       (printfn (lambda (,listtype
			   &optional (printfn* (get ',singletype 'printfn)))
		  (princ "(")
		  (mapl #'(lambda (x) (funcall printfn* (car x))
			    (if (cdr x) (princ " ")))
			,listtype)
		  (princ ")")))
       ,@restprops)))

(defutil deflisttype
  (form-type macro)
  (keywords define)
  (mhelp ("DEFLISTTYPE is a macro that expands into a deftype%."
	 (e (pcall print-nextpar))
	 (e (pcall print-tab))
	 "(DEFLISTTYPE list-type single-type rest-props) defines the type of
lists with elements of type single-type.  rest-props can be used to
override any inherited attributes from the single-type, typically
used for the MHELP property."
	 (e (pcall print-nextpar))
	 (e (pcall print-tab))
	 "In an alternative form, one can write (DEFLISTTYPE list-type
single-type (OTHER-KEYS (test form ...) (test form ...) ...) rest-props)
where form ... is executed if the corresponding test is non-NIL.
The variable list-type will hold the typed expression.  If none of
the tests is true, the usual will be done.")))

(push '(defconstype . argtype) global-definelist)

(defmacro defconstype (constype singletype1 singletype2 &rest restprops)
  (let ((getfn1 (get singletype1 'getfn))
	(getfn2 (get singletype2 'getfn))
	(other-keys (assoc 'other-keys restprops)))
    (when other-keys
      (setq restprops (remove other-keys restprops))
      (setq other-keys (cdr other-keys)))
    `(deftype% ,constype
       (getfn (lambda (,constype)
		(cond ,@other-keys
		      (t 
			(if (not (and (listp ,constype)
				      (listp (cdr ,constype))))
			    (throwfail "Not a pair.")
			    (cons (%catch%
				    (funcall #',getfn1 (car ,constype))
				    (fail (throwfail;;-on SI
					    "CAR is not of type "
					    ',singletype1 ".")))
				  (%catch%
				    (funcall #',getfn2 (cadr ,constype))
				    (fail (throwfail
					    "CDR is not of type "
					    ',singletype2 ".")))))))))
       (testfn (lambda (,constype)
		 (and (funcall #',(get singletype1 'testfn) (car ,constype))
		      (funcall #',(get singletype2 'testfn)
			       (cadr ,constype)))))
       (printfn (lambda (,constype)
		  (msg "(")
		  (funcall #',(get singletype1 'printfn) (car ,constype))
		  (msg "   ")
		  (funcall #',(get singletype2 'printfn) (cdr ,constype))
		  (msg ")")))
       ,@restprops)))

(defutil defconstype
  (form-type macro)
  (keywords define)
  (mhelp "Like DEFLISTTYPE, but for cons-cells rather than lists. "
	 "Takes pairs, without the dot, and converts them into cons-cells."))

;;;
;;; Following are some common argument types.
;;;

(deftype% anything
  (getfn (lambda (anything) anything))
  (testfn (lambda (anything) (declare (ignore anything)) t))
  (printfn princ)
  (mhelp "Any legal LISP object."))

(deftype% symbol
  (getfn testfn)
  (testfn symbolp)
  (printfn princ)
  (mhelp "Any legal LISP symbol (must be able to have property list)."))

(deftype% symbol-or-integer
  (getfn testfn)
  (testfn (or (symbolp symbol-or-integer) (integerp symbol-or-integer)))
  (printfn princ)
  (mhelp "Any legal LISP symbol (must be able to have property list) or integer."))

(deftype% string
  (getfn testfn)
  (testfn stringp)
  (printfn prin1)
  (mhelp "A string enclosed in double-quotes."))

(deftype% boolean
  (getfn (cond (boolean t) (t nil)))
  (testfn (or (eq boolean t) (eq boolean nil)))
  (printfn (if boolean (princ t) (princ nil)))
  (mhelp "A Boolean value (NIL for false, T for true)."))

(deftype% integer+
  (getfn testfn)
  (testfn (and (integerp integer+) (> integer+ -1)))
  (printfn princ)
  (mhelp "A nonnegative integer."))

(deftype% posnumber
  (getfn testfn)
  (testfn (and (numberp posnumber) (> posnumber 0)))
  (printfn princ)
  (mhelp "A positive number of any kind."))

(deftype% posinteger
  (getfn testfn)
  (testfn (and (integerp posinteger) (> posinteger 0)))
  (printfn princ)
  (mhelp "A positive integer."))

(deflisttype posintegerlist posinteger)

(deftype% yesno
  (getfn (cond ((not (or (stringp yesno) (symbolp yesno)))
		(throwfail "Not yes-or-no answer!"))
	       ((member yesno '(y yes t) :test #'string-equal) t)
	       ((member yesno '(n no nil) :test #'string-equal) nil)
	       (t (throwfail "Not yes-or-no answer!"))))
  (testfn (and (or (stringp yesno) (symbolp yesno))
	       (member yesno '(y yes t n no nil) :test #'string-equal)))
  (printfn (cond (yesno (princ "Yes"))
		 (t (princ "No"))))
  (mhelp "y or yes for YES, n or no for NO."))

(deftype% updown
  (getfn (cond ((not (or (stringp updown) (symbolp updown)))
		(throwfail "Not up-or-down!"))
	       ((member updown '(u up) :test #'string-equal) 'UP)
	       ((member updown '(d down dn) :test #'string-equal) 'DOWN)
	       (t (throwfail "Not up-or-down!"))))
  (testfn (and (or (stringp updown) (symbolp updown))
	       (member updown '(u up d dn down) :test #'string-equal)))
  (printfn (cond ((eq updown 'UP) (princ "Up"))
		 (t (princ "Down"))))
  (mhelp "u or up for Up, d or down for Down."))

(deftype% null-or-posinteger
  (getfn testfn)
  (testfn (or (null null-or-posinteger)
	      (and (integerp null-or-posinteger) (plusp null-or-posinteger))))
  (printfn princ)
  (mhelp "NIL or a positive integer."))

(deftype% null-or-integer
  (getfn testfn)
  (testfn (or (null null-or-integer)
	      (and (integerp null-or-integer) (or (plusp null-or-integer) (zerop null-or-integer)))))
  (printfn princ)
  (mhelp "NIL or a nonnegative integer."))

(eval-when (load eval compile)
(defconstnt infinity :infinite)

(defmacro infinite-p (thing)
  `(eq ,thing infinity))

(deftype% integer+-or-infinity
  (getfn get-integer+-or-infinity)
  (testfn get-integer+-or-infinity)
  (printfn (lambda (x) (if (infinite-p x) (princ "INFINITY") (princ x))))
  (mhelp "A nonnegative integer or the symbol INFINITY."))

(defun get-integer+-or-infinity (thing)  
  (cond ((typep thing '(integer 0)) thing)
        ((and (symbolp thing)
	      (or (string-equal "INFINITY" thing)
		  (string-equal "INFINITE" thing))) infinity)
        (t (complain thing " is not of type INTEGER+-OR-INFINITY.")
           (throwfail))))

(deftype% posinteger-or-infinity
  (getfn get-posinteger-or-infinity)
  (testfn get-posinteger-or-infinity)
  (printfn (lambda (x) (if (infinite-p x) (princ "INFINITY") (princ x))))
  (mhelp "A positive integer or the symbol INFINITY."))

(defun get-posinteger-or-infinity (thing)  
  (cond ((and 
	  (typep thing '(integer 0))
	  (neq thing 0))
	 thing)
        ((and (symbolp thing)
	      (or (string-equal "INFINITY" thing)
		  (string-equal "INFINITE" thing))) infinity)
        (t (complain thing " is not of type POSINTEGER-OR-INFINITY.")
           (throwfail)))))

(context rewriting)

(deftype% rrule
  (testfn rewrule-p)
  (getfn get-rewrule)
  (printfn princ)
  (mhelp ("A rewrite rule." (e (category-mhelp-list 'rewrite-rule)))))

(deflisttype rrulelist rrule (mhelp "A list of rewrite rules."))

(deftype% theory
  (testfn theory-p)
  (getfn get-theory)
  (printfn princ)
  (mhelp ("A theory." (e (category-mhelp-list 'theory)))))

(context editor-obj)

(deftype% ed-command
  (getfn testfn)
  (testfn consp)
  (printfn princ)
  (mhelp "A list with editor commands.
This is mainly useful as resulttype for editor operations like EDSEARCH."))

(deftype% msglist
  (getfn testfn)
  (testfn (lambda (msglist) (declare (ignore msglist)) t))
  (printfn (eval (cons 'msg msglist)))
  (mhelp "A list with message instructions a la UCI-Lisp's MSG function.
In addition it may contain pairs (item . argtype)"))

(deflisttype msglistlist msglist
  (mhelp "A list of message lists (see argument type MSGLIST)."))

(context flags)

(deflisttype anylist anything
  (mhelp ("A list.")))

(deflisttype symbollist symbol
  (mhelp ("A list of symbols.")))

(deftype% subject
  (testfn subject-p)
  (getfn testfn)
  (printfn princ)
  (mhelp ("A subject in REVIEW." (e (category-mhelp-list 'review-subject)))))

(deflisttype subjectlist subject
  (other-keys ((eq subjectlist 'all)
	       (remove-if-not #'symbolp global-subjectlist)))
  (mhelp ("A list of subjects in REVIEW or ALL for all subjects."
	  (e (category-mhelp-list 'review-subject)))))

(deftype% tps-mode
  (testfn tps-mode-p)
  (getfn testfn)
  (printfn princ)
  (mhelp ("A TPS mode in REVIEW. If it is not loaded, search for it in library."  
          (e (category-mhelp-list 'flag-mode)))))

(deftype% dev-style
  (testfn (lambda (dev-style) 
	    (declare (special global-stylelist))
	    (member dev-style global-stylelist)))
  (getfn testfn)
  (printfn princ)
  (mhelp ("This specifies the style for the output file."
	  (e (category-mhelp-list 'device-style)))))

(deftype% modes-gwffs
  (testfn (lambda (m)
	    (declare (special global-modes-gwffs))
	    (member m global-modes-gwffs)))
  (getfn testfn)
  (printfn princ)
  (mhelp ("A symbol naming a pair of MODES and GWFFS where MODES is a list of modes and GWFFS is a list of GWFFS")))

(context library)

(deftype% lib-argtype
  (getfn testfn)
  (testfn (lambda (elt)
	    (declare (special global-libobject-list))
	    (memq elt global-libobject-list)))
  (printfn princ)
  (mhelp ("Type of object that can be stored in the library."
	  (e (category-mhelp-list 'libobject)))))

(deftype% lib-argtype-or-nil
  (getfn testfn)
  (testfn (lambda (elt)
	    (declare (special global-libobject-list))
	    (or (null elt) (memq elt global-libobject-list))))
  (printfn princ)
  (mhelp ("NIL or Type of object that can be stored in the library."
	  (e (category-mhelp-list 'libobject)))))

					; this was originally a type which hardwired keywords into the code.
					; it has been replaced by keyword-prop & keyword-list below. - cebrown 4/18/01
(deftype% gwff-prop
  (getfn (lambda (x) (case x ((ALL FIRST-ORDER HIGHER-ORDER SK-FIRST-ORDER SK-HIGHER-ORDER 
                                   WITH-EQUALITY WITH-DEFN PROVEN UNPROVEN AUTO-PROOF) x) 
                             (t (complain x " is not a known gwff property.")
                                (throwfail)))))
  (testfn (lambda (x) (case x ((ALL FIRST-ORDER HIGHER-ORDER SK-FIRST-ORDER SK-HIGHER-ORDER 
                                   WITH-EQUALITY WITH-DEFN PROVEN UNPROVEN AUTO-PROOF) t)
                              (t nil))))
  (printfn princ)
  (mhelp "One of the following properties of gwffs:
ALL : true for all gwffs
FIRST-ORDER : true for first-order gwffs
SK-FIRST-ORDER : true for gwffs that are first-order after skolemizing.
HIGHER-ORDER : true for non-first-order gwffs
SK-HIGHER-ORDER : true for gwffs that are non-first-order after skolemizing.
WITH-EQUALITY : true of gwffs that contain an equality
WITH-DEFN : true of gwffs that contain a definition
PROVEN : true of gwffs that have been marked as proven in the library
UNPROVEN : true of all gwffs that aren't PROVEN
AUTO-PROOF : true of all gwffs with automatic or semi-automatic proofs.

For the first- and higher-order checks, equalities are rewritten as specified
by the flag REWRITE-EQUALITIES; if any equalities remain in the gwff after 
rewriting, these are considered first-order if they are equalities between
base types."))

(deflisttype gwff-prop-list gwff-prop
  (mhelp "A list of some of the following properties of gwffs:
ALL : true for all gwffs
FIRST-ORDER : true for first-order gwffs
SK-FIRST-ORDER : true for gwffs that are first-order after skolemizing.
HIGHER-ORDER : true for non-first-order gwffs
SK-HIGHER-ORDER : true for gwffs that are non-first-order after skolemizing.
WITH-EQUALITY : true of gwffs that contain an equality
WITH-DEFN : true of gwffs that contain a definition
PROVEN : true of gwffs that have been marked as proven in the library
UNPROVEN : true of all gwffs that aren't PROVEN
AUTO-PROOF : true of all gwffs with automatic or semi-automatic proofs.

For the first- and higher-order checks, equalities are rewritten as specified
by the flag REWRITE-EQUALITIES; if any equalities remain in the gwff after 
rewriting, these are considered first-order if they are equalities between
base types."))

					;  cebrown 4/18/01
(deftype% keyword-prop
    (getfn (lambda (x) 
	     (if (keyword-p x)
		 x
	       (progn
		 (complain x " is not a known keyword.")
		 (throwfail)))))
  (testfn (lambda (x) (keyword-p x)))
  (printfn princ)
  (mhelp "A keyword used to signify that a gwff has a certain property.
Use show-keywords to see a list of known keywords."))

(deflisttype keyword-list keyword-prop
  (mhelp "A list of keywords.  Use show-keywords to see a list of known keywords."))

(deflisttype tpsflaglist tpsflag)
(deflisttype stringlist string (mhelp "A list of strings."))
(deflisttype stringlistlist stringlist (mhelp "A list of lists of strings."))
(deflisttype lib-argtype-list lib-argtype
  (mhelp "A list of lib-argtypes; see the help message for LIB-ARGTYPE."))
(deflisttype filespeclist filespec)

(context lisp-packages)

(deftype% lisp-package
  (testfn lisp-package-p)
  (getfn get-lisp-package-name)
  (printfn print-lisp-package-name)
  (mhelp ("A LISP package known to TPS.  Currently any of: "
	  (e (category-items-short-help 'lisp-pack)))))

(deflisttype lisp-package-list lisp-package
  (mhelp "A list of Lisp packages known to TPS."))

(context otl-entering)

(deftype% filespec
  (getfn getfilespec)
  (testfn filespecp)
  (printfn fileprinc)
  (mhelp "The name of a file as a string or TTY for terminal. "))

(deftype% dirspec
  (getfn getdirspec)
  (testfn dirspecp)
  (printfn prin1)
  (mhelp "The name of a file directory, written as a string (delimited
by double-quotes)."))

(deflisttype dirspeclist dirspec)

(context wfftyp-obj)

(deftype% gwffalist
  (testfn (forall x gwffalist (and (label-q (car x))
				   (eq (get (car x) 'flavor) 'meta-var)
				   (gwff-p (cdr x))
				   (type-equal (car x) (cdr x)))))
  (getfn (mapcar #'getsubstpair gwffalist))
  (printfn (mapcar #'princsubstpair gwffalist))
  (mhelp "A list of substitutions for meta-variables."))

(context help-obj)
(deflisttype help*-list symbol-or-integer
  (mhelp "A list of names of TPS objects. Only used by HELP*"))

(deflisttype symbol-or-integer-list symbol-or-integer)

(context suggestions)

(deftype% go-instruct
  (testfn go-instruct-p)
  (getfn testfn)
  (printfn princ)
  (mhelp "A list of GO instructions ((priority action) ...),
where each ACTION is either DO, ASK, SHOW or FORGET."))

(deftype% exec-form
  (testfn exec-form-p)
  (getfn get-exec-form)
  (printfn princ)
  (mhelp "A list of GO instructions ((priority action) ...),
where each ACTION is either DO, ASK, SHOW or FORGET."))

(context otl-printing)
(deftype% print-function
  (getfn testfn)
  (testfn (lambda (x) (case x ((^p pall pstatus ^pn prfw-^p prfw-^pn prfw-pall) x))))
  (printfn princ)
  (mhelp "Should be one of PALL, ^P, ^PN, PSTATUS, PRFW-^P, PRFW-^PN or PRFW-PALL."))

(definfo prfw-^p
  (mhelp "An option for ETREE-NAT-VERBOSE.
After each tactic during ETREE-NAT, in the proofwindow \"Current Subproof\",
print the current plan-support pair in the proof."))

(definfo prfw-^pn
  (mhelp "An option for ETREE-NAT-VERBOSE.
After each tactic during ETREE-NAT, in the proofwindow \"Current Subproof 
and Line Numbers\", print the current plan-support pair in the proof, and
also print just the line numbers of the other lines in the proof."))

(definfo prfw-pall
  (mhelp "An option for ETREE-NAT-VERBOSE.
After each tactic during ETREE-NAT, in the proofwindow \"Complete Proof\",
print the current proof."))

(deflisttype print-function-list print-function
  (mhelp "A list of elements of type print-function, which is to say
a list containing some or all (or none!) of 
PALL, ^P, ^PN, PSTATUS, PRFW-^P, PRFW-^PN and PRFW-PALL."))

(context proof-outline)
(deftype% line
  (testfn (lambda (line) (and (integerp line) (> line 0))))  
  (getfn testfn)
  (short-prompt T)
  (printfn princ)
  (mhelp "A line number."))

(deflisttype linelist line
  (mhelp  "A list of lines.  Examples: (1 3 4), (), (25)"))

(deftype% existing-line
  (testfn (lambda (line) (and (integerp line) (> line 0) (numalias line))))
  (getfn get-existingline)
  (short-prompt T)
  (printfn (lambda (line) (princ (linealias line))))
  (mhelp "Line number of an existing line."))

(deflisttype existing-linelist existing-line
  (mhelp  "A list of existing lines."))

(deftype% pline
  (testfn planp)
  (getfn get-pline)
  (printfn (lambda (pline) (princ (linealias pline))))
  (short-prompt T)
  (mhelp "Line number of an existing planned line."))

(deftype% occlist
  (getfn getocclist)
  (testfn occlistp)
  (printfn princ)
  (mhelp "	A list of occurrences (counted left-to-right) of a subwff in a wff.
	ALL refers to all occurrences of the subwff."))

(deftype% line-range
  (testfn get-line-range)  
  (getfn get-line-range)
  (short-prompt T)
  (printfn print-line-range)
  (mhelp "A range of lines from M through N, written M--N, where M and N 
are positive integers and M <= N.  As shortcuts, one may write M, which 
represents the range M--M;  M--, which stands for the range from line M 
through the last line of the current proof; and --N, which represents the 
range from the first line of the proof through line N.  Hence -- 
represents the range consisting of every line in the proof."))

(deflisttype line-range-list line-range
  (mhelp  "A list of line ranges. See the help message for LINE-RANGE
for examples."))

(context wff-printing)

(deftype% ignore
  (getfn testfn)
  (testfn (lambda (ignore)
	    (declare (ignore ignore))
	    (throwfail
	     "The argument type IGNORE is only for results of wffops.")))
  (printfn (lambda (ignore) (declare (ignore ignore)) nil))
  (mhelp "Used as RESULTTYPE for wff printing operations."))

(deftype% indentation
  (getfn (lambda (x) (case x ((min vary fix compress) x) 
                             (t (complain x " is not of type INDENTATION.")
                                (throwfail)))))
  (testfn (lambda (x) (case x ((min vary fix compress ) T))))
  (printfn princ)
  (mhelp "Should be one of MIN, VARY, COMPRESS or FIX. Used by the flag 
TURNSTILE-INDENT-AUTO."))

(context wfftyp-obj)

(deftype% wffset
  (testfn symbolp)
  (getfn testfn)
  (printfn (lambda (wffset) (declare (special style)) (pcall print-symbol wffset)))
  (mhelp "A symbol standing for a set of wffs in a hypothesis."))

(context proof-outline)

(deftype% justification
  (testfn just-p)
  (getfn getjust)
  (printfn princ)			;PRINC for now
  (mhelp "The justification of a line in a proof in the form (string wfflist linelist)."))

(deftype% rline
  (testfn symbolp)
  (getfn testfn)
  (printfn princ)
  (mhelp "Dummy line definition for the rules packages."))

(deflisttype rlinelist rline
  (mhelp "A list of dummy lines for the rules package."))

(deftype% lvarconst
  (testfn lvarconst-p)
  (getfn (getwff-subtype #'lvarconst-p lvarconst))
  (printfn printwffhere)
  (mhelp "A logical variable or constant, not a polymorphic symbol or abbreviation."))

(context substitution)

(deftype% repsym
  (testfn repsym-p)
  (getfn testfn)
  (printfn princ)
  (mhelp "A replaceable symbol."))

(context flag-review)

(deftype% tpsflag
  (testfn tpsflag-p)
  (getfn testfn)
  (printfn princ)
  (mhelp ("A global flag or parameter." (e (category-mhelp-list 'flag)))))

(defconstype flag-and-val tpsflag anything
 (mhelp "Type for dotted pair of flag name & value."))

(deflisttype fv-list flag-and-val
  (mhelp "A list of dotted pairs of flags and values."))

(context tps-theorems)

(deftype% theorem
  (testfn theorem-p)
  (getfn (lambda (gwff) (if (theorem-p gwff) gwff 
			  (if (and (symbolp gwff) (module-loaded-p 'library)
				   (let (again)
				     (declare (special again *using-interface*))
				     (prompt-read again nil (msgf gwff " is not a known gwff. Search for it in library?") 
						  'yesno 'yes nil))
				   (go-fetch-gwff gwff)
				   (theorem-p gwff))
			      gwff
			    (throwfail gwff " is not a known theorem.")))))
  ;was originally just (getfn testfn)
  (printfn princ)
  (mhelp "A theorem.  Exercises and practice exercises are theorems."))

(deflisttype theoremlist theorem (mhelp "A list of theorems."))

(deftype% lib-theorem
  (testfn library-theorem-p)
  (getfn testfn)
  (printfn princ)
  (mhelp "A theorem loaded from a library."))

(deftype% book-theorem
  (testfn book-theorem-p)
  (getfn testfn)
  (printfn princ)
  (mhelp "A theorem proven in the book."))

(deftype% exercise
  (testfn (lambda (exercise)
	    (and (symbolp exercise)
		 (get exercise '%theorem%)
		 (eq (get exercise 'thm-type) 'exercise))))
  (getfn testfn)
  (printfn princ)
  (mhelp "An exercise which may be assigned."))

(deftype% practice
  (testfn practice-p)
  (getfn testfn)
  (printfn princ)
  (mhelp "An unscored practice exercise."))

(deftype% test-problem
  (testfn (lambda (test-problem)
	    (and (symbolp test-problem)
		 (get test-problem '%theorem%)
		 (eq (get test-problem 'thm-type) 'test-problem))))
  (getfn testfn)
  (printfn princ)
  (mhelp "A potential test problem."))

(context abbrev-ops)

(deftype% rewrite-defns
  (testfn (memq rewrite-defns (list 'none 'all 'only-ext 'leibniz 'lazy2 'both 'dual 'parity1)))
  (getfn testfn)
  (printfn princ)
  (mhelp "One of the following:
NONE: do not rewrite equalities
ONLY-EXT: rewrite only those equalities that can be rewritten using
          extensionality.
LEIBNIZ: rewrite all equalities using the Leibniz definition.
ALL: rewrite all equalities, using extensionality where possible
     and the Leibniz definition otherwise.
DUAL: As in the flag REWRITE-DEFNS.
PARITY1: Uses the parity to determine whether equalities should be
    rewritten as the setting LEIBNIZ or as the setting ALL.  For example,
    using PARITY1 when trying to prove the wff 
              A(OI) = B(OI) implies C
    the equality is expaned using Leibniz, and when trying to prove the wff
              D implies A(OI) = B(OI)
    the equality is expanded using extensionality.  The heuristic
    is that we often use the substitutivity property when we use an equation
    and use extensionality to show an equation."))

(context saving-wffs)
(deftype% weak-label
  (testfn weak-label-p)
  (getfn testfn)
  (printfn princ)
  (mhelp "A weak label."))

(deflisttype weak-label-list weak-label
  (mhelp "A list of weak labels."))

(context wfftyp-obj)

(deftype% gwff
  (getfn (lambda (gwff) (getwff-subtype 'gwff-p gwff)))
  (testfn gwff-p)
  (printfn printwffhere)
  (mhelp ("A reference to a wff."
	  (e (category-mhelp-list 'getgwfftype)))))

(deftype% gwff-ill
  (getfn get-only-rwff)
  (testfn (lambda (gwff-ill) (declare (ignore gwff-ill)) t))
  (printfn printwffhere)
  (mhelp "A reference to a well- or ill-formed formula."))

(deftype% gwff0
  (testfn (lambda (gwff0)
	    (and (gwff-p gwff0) (eq (type gwff0) 'O))))
  (getfn get-gwff0)
  (printfn printwffhere)
  (mhelp ("A reference to a wff of type O."
	  (e (category-mhelp-list 'getgwfftype)))))

(defvar *last-gwff-typed* nil)
;this variable is solely for the benefit of PROVE, which can't otherwise get a default name for the proof.

(deftype% gwff0-or-label
  (testfn (lambda (gwff0)
            (and (gwff-p gwff0) (eq (get gwff0 'type) 'O))))
  (getfn (lambda (gwff0) (let ((wff-or-label (get-gwff0 gwff0)))
			   (setq *last-gwff-typed* gwff0)
			   (if (label-p gwff0) gwff0 wff-or-label))))
  (printfn printwffhere)
  (mhelp ("A reference to a wff of type O. If the gwff0 is a label
the getfn will give the label name instead of the wff represented
by the label."
          (e (category-mhelp-list 'getgwfftype)))))

(deftype% gvar
  (getfn (getwff-subtype 'gvar-p gvar))
  (testfn gvar-p)
  (printfn printwffhere)
  (mhelp ("A gwff which must be a logical variable."
	  (e (category-mhelp-list 'getgwfftype)))))

(deflisttype gvarlist gvar (mhelp ("A list of variables."
				    (e (category-mhelp-list 'getgwfftype)))))

(deflisttype occ-list posinteger
  (other-keys ((eq occ-list 'all) t))
  (mhelp "A list of positive integers or ALL."))

(deftype% typesym
  (getfn get-stringtype)
  (testfn typesym-p)
  (printfn printype)
  (mhelp "The string representation of a type."))

(defconstype typesym-cons typesym typesym
  (mhelp "A cons-cell of type symbols."))

(deflisttype typealist typesym-cons
  (mhelp "An a-list of type symbols."))

(deflisttype typesymlist typesym
  (mhelp "A list of string representations of types."))

(deftype% typesym-nil
  (getfn (lambda (typesym-nil)
	   (if typesym-nil (get-stringtype typesym-nil) nil)))
  (testfn (lambda (typesym-nil) (or (null typesym-nil)
				    (typesym-p typesym-nil))))
  (printfn (lambda (typesym-nil)
	     (if typesym-nil (printype typesym-nil) (princ typesym-nil))))
  (mhelp "The string representation of a type or NIL."))

(deflisttype typesymlist-nil typesym-nil
  (mhelp "A list of type symbols or NIL."))

;tyep fsym moved back into gensty.lisp

(deflisttype gwfflist gwff
  (mhelp "A list of GWFFs, used for lists of expansions terms."))

(defconstype gwffpair gwff gwff
  (mhelp "A pair of GWFFs. In unification, a disagreement pair."))

(deflisttype gwffpairlist gwffpair
  (mhelp "A list of GWFFPAIRs. In unification, a disagreement set."))

(defvar global-ordercomlist)

(deftype% ordercom
  (testfn (lambda (ordercom) (or (eq ordercom t) (member ordercom global-ordercomlist))))
  (getfn testfn)
  (printfn princ)
  (mhelp ("This specifies the value of order-components for mating search."
	  (e (category-mhelp-list 'ordercomponents)))))

(context auto::jforms1)

(deftype% jform
  (getfn auto::get-jform)
  (testfn auto::jform-p)
  (printfn princ)
  (mhelp "A jform or a gwff."))

(deftype% vpformat
   (getfn (lambda (vpformat)
	    (let ((result (member vpformat '(A L LT T B BT NIL)
				  :test #'string-equal)))
	      (if result (car result) (throwfail vpformat
				 " is not of type vpformat.")))))
   (testfn (lambda (vpformat)
	     (member vpformat '(A L LT T B BT NIL) :test #'string-equal)))
   (printfn princ)
   (mhelp "T   = no atom values will show in VP diagram
A   = atom values but no labels will appear in VP diagram
NIL = atom values and labels will show in VP diagram
LT  = atom values and  labels and a legend will show in VP diagram 
L   = labels but no atom values will show in VP diagram,
      and a legend will show both
B   = boxed labels and atoms will show in the VP diagram.
BT  = boxed labels will show in the diagram, and the atom values
      will be listed below.
B and BT only work in TeX format (i.e. with the VPT command)."))

(deftype% vpstyle
  (getfn testfn)
  (testfn (lambda (vpstyle)
	    (memq vpstyle '(concept concept-s sail scribe
				    scribe-slides generic))))
  (printfn princ)
  (mhelp "Styles supported for vertical path diagrams.
Currently any of CONCEPT, CONCEPT-S, SAIL, SCRIBE, SCRIBE-SLIDES, GENERIC.
(Use the VPT command to print in TEX style.)
The linelength associated with various SCRIBE fonts is: (8 99) (10 79) 
 (12 65) (14 56) (18 43).
The linelength associated with various SAIL fonts is: 	(4L 301) (4P 216)
(5L 240) (5P 172) (6L 199) (6P 143) (7L 171) (7P 123) (8L 149) (8P 107)
(9L 133) (9P 95) (10L 120) (10P 86) (12L 99) (12P 71) (14L 85) (14P 61)
(18L 66) (18P 47).  "))

(context style)
(deftype% fontsizestring
  (getfn testfn)
  (testfn stringp)
  (printfn prin1)
  (mhelp "A string describing the fontsize for an interface:
The empty string \"\" means normal sized fonts.
The string \"-big\" means big fonts.
The string \"-x2\" or \"-x4\" means normal sized fonts times 2 or 4.
The string \"-big -x2\" or \"-big -x4\" means big fonts times 2 or 4."))

