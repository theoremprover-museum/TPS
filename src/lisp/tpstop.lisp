;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of TPSDEF)

;;;
;;; File: TPSTOP
;;;
;;; defines the command decoder for all command top levels like REVIEW,
;;; or the absolute top level.


;;;Note that function PROMPT-VALUES now returns its values as
;;;internal-arglist, external-arglist. Previously, it returned a list in
;;;the opposite order.  SI.

(deffile tpstop
  (part-of tpsdef)
  (extension lsp)
  (mhelp "Defines the command decoder for all command top levels like REVIEW, 
or the absolute top level."))

(context subtoplevels)

(defvar max-prompt-col 60)

(defvar max-short-prompt 75)


(defmacro pcall (fn &rest args)
  `(let ((pcall-fn (get style ',fn)))
     (if pcall-fn
	 (funcall pcall-fn ,@args)
	 (throwfail "Operation " ',fn " is illegal for style "
		    style "."))))

(defvar ordinals
  '("first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eigth"
	   "ninth" "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
	   "fifteenth"))

(defvar argnumbers
  '(%1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15))

;;;
;;; PROMPT-READ is a macro with the following arguments
;;; (PROMPT-READ internal-var external-var
;;;              initial-message-form argument-type default-value
;;;              ((response form ...) (response form ...)))
;;;

(defmacro prompt-read (internal-var external-var initial-message
				    argument-type default-value
				    special-response-list)
  (declare (special *using-interface* *simple-interface-prompts*))
  `(if (and *using-interface* (not *simple-interface-prompts*) (not *executing*))
       (setq ,internal-var
	     (do ((response nil))
		 (nil)
	       (let ((prompt-sym (intern (gensym "PROMPT"))))
		 (start-prompt-msg)
		 ,initial-message
		 (start-prompt-name)
		 (write-string (format nil "~d" prompt-sym))
		 (start-prompt-argtyp)
		 (write-string (format nil "~d" ,argument-type))
		 (start-prompt-help)
		 (let ((*using-interface* NIL) ; set to nil so the help is just text
		       (style 'GENERIC))
		   (declare (special *using-interface* style))
		   (dolist (sr ',special-response-list) ; help info
		     (when (or (eq (car sr) '?) (eq (car sr) '??))
		       (terpri)
		       (case (car sr)
			 ,@special-response-list))))
		 (start-prompt-default)
		 (unless (equal ,default-value '$) 
		   (funcall (get ,argument-type 'printfn)
			    ,default-value))
		 (end-prompt)
		 (setq response (let ((command-completion nil))
				  (declare (special command-completion))
				  (linereadpp " " nil nil t nil t prompt-sym))))
	       (cond ((null response)
		      (cond ((equal ,default-value '$)
			     (complain f "There is no default for this argument."))
			    (t ,@(if external-var `((setq ,external-var '$)) nil)
			       (return ,default-value))))
		     (t (if (null (cdr response))
			    (setq response (car response)))
			,@(if external-var `((setq ,external-var response)) nil)
			(if (symbolp response)
			    ;; added check for ABORT to allow user to quit
			    ;; in middle of a command without going into
			    ;; the debugger, especially commands like
			    ;; use-tactic and go.  13FEB91 DAN
			    (if (string-equal response "ABORT")
				(throwfail "Aborting by user request.")
			      (if (string-equal response "PAUSE")
				  (pause)
				(if (string-equal response "PUSH")
				    (top)
				  (case response
				    ,@special-response-list
				    (t
				     (%catch% (return 
					       (gettype ,argument-type response))
					      (fail 
					       (complain f expand-catch-throw))))))))
			  (%catch% (return (gettype ,argument-type response))
				   (fail (complain f
						   expand-catch-throw))))))))
     (setq ,internal-var
	   (do ((response nil))
	       (nil)
	     ,initial-message
	     (msg " [")
	     (if (equal ,default-value '$) 
		 (msg "No Default")
	       (funcall (get ,argument-type 'printfn)
			,default-value))
	     (msg "]")
	     (setq response (let ((command-completion nil))
			      (declare (special command-completion))
			      (if (and *using-interface* (not *executing*))
				  (linereadpp '> nil nil t nil t 'COMMAND)
				(linereadpp '>))))
	     (cond ((null response)
		    (cond ((equal ,default-value '$)
			   (complain f "There is no default for this argument."))
			  (t ,@(if external-var `((setq ,external-var '$)) nil)
			     (return ,default-value))))
		   (t (if (null (cdr response))
			  (setq response (car response)))
		      ,@(if external-var `((setq ,external-var response)) nil)
		      (if (symbolp response)
			  ;; added check for ABORT to allow user to quit
			  ;; in middle of a command without going into
			  ;; the debugger, especially commands like
			  ;; use-tactic and go.  13FEB91 DAN
			  (if (string-equal response "ABORT")
			      (throwfail "Aborting by user request.")
			    (if (string-equal response "PAUSE")
				(pause)
			      (if (string-equal response "PUSH")
				  (top)
				(case response
				  ,@special-response-list
				  (t
				   (%catch% (return 
					     (gettype ,argument-type response))
					    (fail 
					     (complain f expand-catch-throw))))))))
			(%catch% (return (gettype ,argument-type response))
				 (fail (complain f
						 expand-catch-throw))))))))))


(defutil prompt-read
  (form-type macro)
  (keywords input)
  (mhelp ("PROMPT-READ is the canonical way of doing input in TPS.
It provides argument type checking, a default mechanism and options
which allow ? and ?? help and arbitrarily many other special responses.
Its form is"
	  (e (pcall begin-environment "LispCode"))"
 (PROMPT-READ internal-var external-var
              initial-message-form argument-type default-value
              ((response form ...) (response form ...)))"
	  (e (pcall end-environment "LispCode"))
	  (e (pcall print-tab))
	  (e (pcall begin-environment "description"))"
internal-var will hold the internal representation of the user's response
after the input.

external-var will hold the external representation of what the user typed.
If external-var = NIL, the external form of the input is thrown away.

initial-message-form is evaluated and should somehow output the initial
part of the prompt.

argument-type is the type of the object that the user is supposed to input.
Common here is 'YESNO

default-value is the internal representation of the default for the input.
A default-value of $ means that there is no default.

((response form ...) (response form ...)) are forms to handle special responses
like ?, ?? or perhaps <Esc>.  response is either a single symbol or a list
of symbols and form ... are evaluated in case one of the corresponding
    responses has been typed.  A common use is"
t
(e (pcall print-tab))
(e (pcall begin-environment "LispCode"))"
 ((? (msgf \"Please decide whether you want to see any more news.\"))
  (?? (mhelp 'yesno)))"
(e (pcall end-environment "LispCode"))
(e (pcall end-environment "Description"))
t
(e (pcall print-tab))
"Here is a complete example of a use of PROMPT-READ within an initialization
dialogue:"
t
(e (pcall print-tab))
(e (pcall begin-environment "LispCode"))
"
  (let (ldefp)
    (prompt-read
      ldefp nil
      (msgf \"Load private definitions? \")
      'yesno 'nil
      ((? (msgf \"Load PPS:DEFS and PPS:MODES.INI ?\"))
       (?? (mhelp 'yesno)))))
    
    (when ldefp (lload \"pps:defs\") (lload \"pps:modes.ini\")))"
(e (pcall end-environment "LispCode")))))

(defmacro prompt-read-return-vals (&rest args)
  "Same as prompt-read, but doesn't take the first two arguments.
Instead, instead of setting variables, returns the INTERNAL and
EXTERNAL versions of the user's input.  Uses prompt-read."
  (let ((var1 (gensym))
	(var2 (gensym)))
  `(let ((,var1 nil)
	 (,var2 nil))
     (prompt-read ,var1 ,var2 ,@args)
     (values ,var1 ,var2))))

(defmacro query (message default)
  `(let ((var nil))
       (prompt-read var nil
	   (msgf ,message) 'yesno ,default ((? (mhelp 'yesno))))
     var))

(defutil query
  (form-type macro)
  (keywords input)
  (mhelp ("QUERY is the canonical way of obtaining a yes-no response
from the user. It calls PROMPT-READ with appropriate arguments. The only
difference between these two macros is that prompt-read sets a variable,
while query just returns T or nil. Its form is"
	  (e (pcall begin-environment "LispCode"))"
       (query initial-message-form default-value)"
	  (e (pcall end-environment "LispCode"))
	  (e (pcall print-tab))
          (e (pcall begin-environment "description"))"
initial-message-form is evaluated and should somehow output the initial
part of the prompt.

default-value is the internal representation of the default for the input.
A default-value of $ means that there is no default."

    (e (pcall end-environment "Description")))))

;;; DPROOF, the name of the current proof.   ***** means no
;;; proof in progress.
(defvar dproof '*****)

(defun comdecode (command)
  (comdecode-real command))

(defun comdecode-real (command)
  (declare (special restore-file-p execprint))
  ;; For deciding whether to execute commands with DONT-RESTORE = T
  ;; or PRINT-COMMAND = T.
  (let ((keyword (car command)))
;;  finalresult [not used?], lastmainfn [no longer used]
;; err-free result [bd in RUN-COMMAND]
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values keyword (copy (cdr command)) (get keyword 'argtypes)
		       (or (get keyword 'wffargtypes)
			   (mapcar #'(lambda (x) (declare (ignore x)) nil)
				   (get keyword 'argtypes)))
		       (get keyword 'wffop-typelist)
		       (get keyword 'defaultfns)
		       (mapcar #'(lambda (x) (declare (ignore x)) nil)
			       (get keyword 'argtypes))
		       (getargnames keyword) (get keyword 'arghelp))
      (declare (ignore external-arglist))
      (cond ((and restore-file-p (get keyword 'dont-restore))
	     (when (fboundp 'signal-event)
	       (signal-event 'command keyword 'dr))
	     (ttymsg f "; Not restoring " keyword ".")
	     t)
	    ((and restore-file-p (not execprint)
		  (get keyword 'print-command))
	     (when (fboundp 'signal-event)
	       (signal-event 'command keyword 'dp))
	     (ttymsg f "; Not restoring " keyword ", a print command.")
	     t)
	    ((and (boundp 'dproof) (get dproof 'allowed-cmd-p)
		  (not (funcall (get dproof 'allowed-cmd-p) dproof keyword)))
	     (when (fboundp 'signal-event)
	       (signal-event 'command keyword 'ir))
	     (complain f keyword " is not a legal rule or command for this exercise.")
	     nil)
	    (t
	     ;;(commandsanspp (cons (car command) internal-arglist))
	     (run-command keyword internal-arglist))))))

(defun run-command (keyword internal-arglist)
  (if (get keyword 'window-op) (protect-concept))
  (let ((result nil)
	(mainfns (or (get keyword 'mainfns) (list keyword)))
	(err-free t))
    (dolist (mainfn mainfns)
      (%catch% (setq result (apply mainfn internal-arglist))
	       (fail (when (get keyword 'window-op)
		       (unprotect-concept))
		     (when (and (fboundp 'signal-event)
				(event-p 'rule-error)
				(get keyword 'srule))
		       (signal-event 'rule-error
				     keyword mainfn expand-catch-throw))
		     (complain f "Error from " mainfn ".  " expand-catch-throw)
		     (setq err-free nil)
		     (return))))
    (if err-free
	(progn
	  (dolist (enterfn (get keyword 'enterfns))
	    (%catch% (apply enterfn result)
		     (fail
		       (complain f "Error from " enterfn
				 ".  " expand-catch-throw)
		       (when (and (fboundp 'signal-event)
				  (event-p 'rule-error)
				  (get keyword 'srule))
			 (signal-event 'rule-error keyword enterfn 
                                       expand-catch-throw))
		       (setq err-free nil)
		       (return))))
	  (if (get keyword 'window-op) (unprotect-concept))
	  (if err-free
	      (progn
		(dolist (closefn (get keyword 'closefns))
		  (%catch%
		    (funcall closefn result)
		    (fail (complain f "Result of " (last mainfns)
				    " could not be processed by"
				    closefn "." t expand-catch-throw)
			  (setq err-free nil)
			  (return))))
		(if err-free
		    (progn
		      (when (fboundp 'signal-event)
			(signal-event 'command keyword 'ok))
		      t)
		    (progn
		      (when (fboundp 'signal-event)
			(signal-event 'command keyword 'ec))
		      nil)))
	      (progn
		(when (fboundp 'signal-event)
		  (signal-event 'command keyword 'ee))
		nil)))
	 (progn
	 (when (fboundp 'signal-event)
	   (signal-event 'command keyword 'em))
	 nil))))


(defmacro getkey (primary secondary prop)
  `(or (get ,primary ,prop) (get ,secondary ,prop)))

;;See CATCH

(defun opdecode (command)
  (declare (special strong-defaults))
  (let ((keyword (car command))
	(alias (get (car command) 'alias))
	 appfn mainfn result)
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values keyword
		       (copy (cdr command))
		       (getkey keyword alias 'argtypes)
		       (or (getkey keyword alias 'wffargtypes)
			   (mapcar #'(lambda (x) (declare (ignore x)) nil)
				   (getkey keyword alias 'argtypes)))
		       (getkey keyword alias 'wffop-typelist)
		       (getkey keyword alias 'defaultfns)
		       (funcall strong-defaults keyword alias)
		       (getargnames keyword)
		       (getkey keyword alias 'arghelp))
      (declare (ignore external-arglist))
      (setq appfn (or (getkey keyword alias 'applicable-q)
		      (getkey keyword alias 'applicable-p)))
      (if (and appfn (not (apply appfn internal-arglist)))
	  (throwfail keyword " not applicable."))
      (setq mainfn (cond (alias) (t keyword)))
      (%catch% (setq result (apply mainfn internal-arglist))
	       (fail (complain f "Error from " mainfn ".  " expand-catch-throw)
		     (throwfail "Operation aborted.")))
      result)))

(defun strong-ed-defaults (keyword alias)
  (declare (special edwff))
  (let ((edwff-argname (get keyword 'edwff-argname)))
    (do ((argnames (getkey keyword alias 'argnames) (cdr argnames))
	 (argtypes (getkey keyword alias 'argtypes) (cdr argtypes))
	 (wffargtypes (getkey keyword alias 'wffargtypes) (cdr wffargtypes))
	 (polytypes (getkey keyword alias 'wffop-typelist))
	 (strong-defaults
	  nil
	  (cons (if (eq (car argnames) edwff-argname) 
		    (progn 
		      (when (and (memq (car argtypes) '(gwff gwff0)) ;do this one first, because jforms are also gwff0's
				 (boundp 'auto::jform-p)
				 (fboundp 'auto::jform-p)
				 (auto::jform-p edwff))
			(setq edwff (auto::jform-to-gwff edwff)))
		      (if (and (funcall (get (car argtypes) 'testfn) edwff)
			       (or (not wffargtypes)
				   (might-be-polytype (princ-to-string (car wffargtypes)) (mapcar 'princ-to-string polytypes))
					;this is a non-exact check, but it'll do. more exact checking can be done by the wffop.
				   (eq (type edwff) (car wffargtypes))))
			  (cons t edwff)
			(if (and (equal (car argtypes) 'jform)
				 (boundp 'auto::jform-p)
				 (fboundp 'auto::jform-p)
				 (gwff-p edwff))
			    (cons t (auto::gwff-to-jform edwff))
			  (throwfail t "The current edwff is of the wrong type; TPS expected a " (car argtypes) "." t))))
		  nil)
		strong-defaults)))
	((null argnames) (nreverse strong-defaults)))))

(defun might-be-polytype (wat pol)
  (do ((polytype pol (cdr pol))
       (return nil))
      ((or return (null polytype)) return)
      (setq return
       (do ((i 0 (1+ i))
	    (sub-return nil))
	   ((or sub-return (<= (length wat) i)) sub-return)
	   (setq sub-return (eq (char (car polytype) 0) (char wat i)))))))
 
;this is the old version, which didn't check the argtype of the edwff against the expected type    
#+comment(defun strong-ed-defaults (keyword alias)
  (declare (special edwff))
  (let ((edwff-argname (get keyword 'edwff-argname)))
    (do ((argnames (getkey keyword alias 'argnames) (cdr argnames))
	 (strong-defaults
	  nil
	  (cons (if (eq (car argnames) edwff-argname) (cons t edwff) nil)
		strong-defaults)))
	((null argnames) (nreverse strong-defaults)))))
      
(defun initial-polytypevarlist (typevarlist)
  (mapcar #'(lambda (tvar) (cons tvar (next-temp-tvar))) typevarlist))

(defun prompt-values (keyword arglist argtypes wffargtypes typelist
			      defaultfns strong-defaultlist argnames arghelp)
  (declare (special strong-defaultlist *using-interface*))
  (let* ((external-arglist (fill-args arglist argtypes '*))
	 (polytypevarlist (initial-polytypevarlist typelist))
	 (internal-arglist (get-args external-arglist argtypes wffargtypes
				     polytypevarlist)))
    (do ()
	((and (not (memq '$ internal-arglist))
	      (not (memq '? internal-arglist)))
	 (values internal-arglist external-arglist))
      (do ((argnames argnames (cdr argnames))
	   (arghelp arghelp (cdr arghelp))
	   ;;(ordtail ordinals (cdr ordtail))
	   (argtypetail argtypes (cdr argtypetail))
	   (wffargtypetail wffargtypes (cdr wffargtypetail))
	   (external-argtail external-arglist (cdr external-argtail))
	   (internal-argtail internal-arglist (cdr internal-argtail))
	   (arg-pos 0 (+ arg-pos 1)))
	  ((null external-argtail))
	(let ((external-arg (car external-argtail))
	      (internal-arg (car internal-argtail)))
	  (when
	   (or (eq external-arg '*) (eq internal-arg '?))
	   (let ((default
		   (if (memq '? (cdr internal-argtail)) '$
		       (if (eq internal-arg '?)
			   (let ((clean-internal
				  (subst '$ '? internal-arglist)))
			     (nth arg-pos
				  (get-defaults clean-internal defaultfns
						wffargtypes polytypevarlist)))
			   (nth arg-pos
				(get-defaults internal-arglist defaultfns
					      wffargtypes polytypevarlist))))))
	     (if (nth arg-pos strong-defaultlist)
		 (progn
		  (when (eq default '$)
			(if (consp (nth arg-pos strong-defaultlist))
			    (setq default
				  (cdr (nth arg-pos strong-defaultlist)))
			    (complain f "Error in TPS: Strong default for "
				      (car argnames)
				      " not supplied by a defaultspec.")))
		  (setq external-arg '$)
		  (setq internal-arg default))
		 (progn
		  (when (and (eq internal-arg '?) (not (eq external-arg '$)))
			(complain f "Illegal argument value: " external-arg "." t))
		  (let ((global-type (if (car wffargtypetail)
					 (sublis polytypevarlist
						 (car wffargtypetail))
					 nil)))
		    (declare (special global-type))
		    (prompt-read
		     internal-arg external-arg
		     (msgf (if argnames (symbol-name (car argnames)) "") " "
			   "(" (car argtypetail) ")"
			   ": " (if arghelp (car arghelp) ""))
		     (car argtypetail)
		     default
		     ((! (use-defaults external-argtail)
			 (setq external-arg '$)
			 (setq internal-arg '$)
			 (return '$))
		      (? (mhelp-for-cat (car argtypetail) 'argtype))
		      (?? (mhelp keyword))))
		    (when (and (not (equal external-arg '$)) global-type)
			  (matchtwo global-type (type internal-arg))))))
	     (rplaca external-argtail external-arg)
	     (rplaca internal-argtail internal-arg)))
	  (when (and (eq external-arg '$)
		     (eq internal-arg '$)
		     (nth arg-pos strong-defaultlist))
		(rplaca internal-argtail
			(if (consp (nth arg-pos strong-defaultlist))
			    (cdr (nth arg-pos strong-defaultlist))
			    (complain f "Error in TPS: Strong default for "
				      (car argnames)
				      " not supplied by a defaultspec."))))))
      (setq internal-arglist (fill-in-defaults internal-arglist defaultfns
					       wffargtypes polytypevarlist))
      (cond ((memq '$ internal-arglist)
	     (complain f "Some defaults could not be determined.")
	     (setq internal-arglist
		   (sublis '(($ . ?) (* . ?)) internal-arglist)))
	    ((or (memq '? internal-arglist) (memq '* internal-arglist))
	     (complain f "? or * : Illegal argument for commands.")
	     (setq internal-arglist
		   (sublis '(($ . ?) (* . ?)) internal-arglist)))))))


(defun fill-args (arglist argtypes filler)
  ;; First check for an exclamation mark.  If found, try to use
  ;; defaults for all other arguments without prompting.
  (let ((last (last arglist)))
    (when (not (null (cdr last)))
	  (throwfail "Illegal use of period." t
		     "Perhaps you need to enclose a filename in doublequotes."))
    (when (equal (car last) '!)
	  (setq arglist (ldiff arglist last))
	  (setq filler '$)))
  (let ((al-length (length arglist))
	(at-length (length argtypes)))
    (if (> al-length at-length)
	(if (= at-length 0) (throwfail "No arguments expected, but " al-length
				       " supplied.")
	    (let ((tail (nthcdr (- at-length 1) arglist)))
	      (append (ldiff arglist tail) (list tail))))
	(do ((al-length al-length (+ al-length 1))
	     (fillers nil (cons filler fillers)))
	    ((= al-length at-length)
	     (append arglist fillers))))))

(defun get-args (arglist argtypes wffargtypes polytypevarlist)
  (declare (special polytypevarlist))
  (mapcar #'(lambda (arg argtype wffargtype)
	      (declare (special polytypevarlist))
	      (let ((global-type (if wffargtype
				     (sublis polytypevarlist wffargtype)
				     nil)))
		(declare (special global-type))
		(%catch% (gettype argtype arg)
			 (fail (complain f "Illegal argument detected." t
					 expand-catch-throw)
			       '?))))
	  arglist argtypes wffargtypes))

(defun use-defaults (external-args)
  (do ((ea external-args (cdr ea)))
      ((null ea))
    (if (eq (car ea) '*) (rplaca ea '$))))

(defun fill-in-defaults (internal-arglist defaultfns wffargtypes
					  polytypevarlist)
  (do ((internal-arglist internal-arglist
			 (get-defaults internal-arglist defaultfns
				       wffargtypes polytypevarlist))
       (prev-int-arglist nil internal-arglist))
      ((if prev-int-arglist
	   (or (not (memq '$ internal-arglist))
	       (same-dollars internal-arglist
			     prev-int-arglist))
	   (null internal-arglist))
       internal-arglist)))

(defun same-dollars (list prev-list)
  (do ((list list (cdr list))
       (prev-list prev-list (cdr prev-list)))
      ((null list) t)
    (if (and (eq (car prev-list) '$) (not (eq (car list) '$)))
	(return nil))))


;;; (GETTYPE argument-type external-form)
;;; converts external-form to internal representation, assuming it's of type
;;; argument-type.  The external form should not prompt the user (i.e.
;;; not be RDC or such).


(defun gettype (argtype arg)
  (cond ((memq arg '($ *)) '$)
	((and (consp arg) (eq (car arg) 'quote))
	 ;; This is new to prevent some arguments from being
	 ;; interpreted: quote them.
	 (cadr arg))
	((and (fboundp 'signal-event) (event-p 'input-error))
	 (%catch% (funcall (get argtype 'getfn) arg)
		  (fail (signal-event 'input-error argtype arg)
			(throwfail-on expand-catch-throw))))
	(t (funcall (get argtype 'getfn) arg))))

(defun get-defaults (internal-args defaultfns wffargtypes polytypevarlist)
  (declare (special strong-defaultlist))
  (let ((prev-strong-defaultlist strong-defaultlist))
    (do ((defaultspecs defaultfns (cdr defaultspecs))
	 (internal-defaults
	  internal-args
	  (%catch% (apply-defaultspec (car defaultspecs) internal-defaults)
		   ;; Changed this to always throw.  fpf.
		   ;;(fail (complain f "Inconsistent arguments." t throw)
		   ;;internal-defaults)
		   (fail (throwfail-on "Inconsistent arguments." t 
                                       expand-catch-throw)))))
	((null defaultspecs)
	 (match-strong-types prev-strong-defaultlist strong-defaultlist
			internal-defaults wffargtypes polytypevarlist)
	 internal-defaults))))

(defun apply-defaultspec (dspec args)
  (cond ((or (symbolp dspec) (eq (car dspec) 'lambda))
	 (apply dspec args))
	(t (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
		   dspec args))))


(defun match-strong-types (prev-strong-defaultlist strong-defaultlist
			   internal-defaults wffargtypes polytypevarlist)
  (do ((psd prev-strong-defaultlist (cdr psd))
       (sd strong-defaultlist (cdr sd))
       (idd internal-defaults (cdr idd))
       (wat wffargtypes (cdr wat)))
      ((null psd))
    (when (and (not (car psd)) (car sd) (car wat))
	  (%catch% (matchtwo (sublis polytypevarlist (car wat))
			     (type (car idd)))
		   (fail ;;Changed below to throw stronger
		    ;;(complain f "Typing of wff arguments inconsistent"
		    ;;" with strong defaults."  t throw)
		    ;; Here we make sure that the ``strong'' default is
		    ;;made into a regular weak default.
		    (rplaca sd nil)
		    (throwfail-on "Typing of wff arguments inconsistent with strong defaults." t 
                                  expand-catch-throw)
		    )))))
