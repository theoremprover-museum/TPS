;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of S-EQN)

(deffile s-eqn-top
  (part-of S-EQN)
  (extension clisp)
  (mhelp "Commands for the REWRITING top-level."))

(context subtoplevels)

(defun seqncmd-mhelp (keyword category)
  (princ-mhelp keyword category)
  (unless short-help 
    (when *doing-html* (msg " fnord "))
    (msgf "The command format for " keyword " is:" -2 "<REWRITING>" keyword)
    (print-tps-format* keyword " "
		       (+ 5 (length (format nil "~A" keyword)))
		       (get keyword 's-eqn-argnames)
		       (get keyword 's-eqn-argtypes)
		       nil)))

(defun s-eqn ()
  (let ((old-print-line-gen (get 'generic 'print-line))
	(old-print-line-xterm (get 'xterm 'print-line))
	(old-print-line-tex (get 'tex 'print-line))
	(old-print-line-tex-1 (get 'tex-1 'print-line)))
    (setf (get 'generic 'print-line) #'print-s-eqn-line-generic)
    (setf (get 'xterm 'print-line) #'print-s-eqn-line-generic)
    (setf (get 'tex 'print-line) #'print-s-eqn-line-tex)
    (setf (get 'tex-1 'print-line) #'print-s-eqn-line-tex)
    (setq *s-eqn-dproof-backup* dproof)
    (%catch% (seqntop)
	     (exit-inferior-top
	      (progn
		(setf (get 'generic 'print-line) old-print-line-gen)
		(setf (get 'xterm 'print-line) old-print-line-xterm)
		(setf (get 'tex 'print-line) old-print-line-tex)
		(setf (get 'tex-1 'print-line) old-print-line-tex-1)
		core::expand-catch-throw)))))

(defun seqntop () 
  (let ((top-prompt-fn #'s-eqn-top-prompt)
	(command-interpreter #'s-eqn-command-interpreter)
	(print-* #'s-eqn-print-*)
	(top-level 's-eqn-top))
    (declare (special top-prompt-fn command-interpreter print-* top-level command-ctree))
    (secondary-top)))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun s-eqn-top-prompt (id) (format nil "<REWRITING~A>" id))

(defun s-eqn-print-* (result) (declare (ignore result))
  (fresh-line)); (prin1 result))	; looks better without

(defun s-eqn-command-interpreter (cmd)
  (let ((carcmd (car cmd)))
    (setq core::*retrieve-stack* nil)
    (cond ((null cmd) nil)
	  ((and (null (cdr cmd)) (atom carcmd))
	   (cond 
	         ((integerp carcmd)
		  (throwfail "Unknown command."))
		 ((and (symbolp carcmd) (get carcmd 'seqncmd))
		  (if (eq carcmd 'end-prfw)
		      (progn
			(complain
			 "Proofwindows are not enabled. Use LEAVE to leave.")
			nil)
		    `(s-eqn-opdecode (quote ,cmd))))
		 ((and (symbolp carcmd) (get carcmd 'mexpr))
		  (if (member carcmd *s-eqn-allowed-mexprs*)
		      `(comdecode (quote ,cmd))
		    (throwfail "Cannot apply " carcmd " in REWRITING top level.")))
		 ((and (symbolp carcmd) (get carcmd 'reviewcmd))
		  `(comdecode (quote ,cmd)))
		 ((and (symbolp carcmd) (get carcmd 'flag))
		  `(comdecode '(setflag ,@cmd)))
		 ((and (symbolp carcmd) (get carcmd 'rewrite-rule))
		  `(s-eqn-opdecode '(app ,@cmd)))
		 ((null expertflag)
		  (throwfail "Unknown Command or Flag."))
		 ((and (symbolp carcmd) (boundp carcmd)) carcmd)
		 ((and (symbolp carcmd) (fboundp carcmd)) cmd)
		 ((or (get carcmd 'mhelp) (get carcmd 'mhelp-fn))
		  (msg "Cannot evaluate that... calling HELP " carcmd t t)
		  `(comdecode '(help ,carcmd)))
		 (t  (throwfail ";" carcmd " - Unbound variable."))))
	  ((and expertflag (null (cdr cmd))) carcmd)
	  ((and (symbolp carcmd) (get carcmd 'seqncmd))
	   `(s-eqn-opdecode (quote ,cmd))) 
	  ((and (symbolp carcmd) (get carcmd 'mexpr))
	   (if (member carcmd *s-eqn-allowed-mexprs*)
	       `(comdecode (quote ,cmd))
	     (throwfail "Cannot apply " carcmd " in REWRITING top level.")))
	  ((and (symbolp carcmd) (get carcmd 'reviewcmd))
	   `(comdecode (quote ,cmd)))
	  ((and (symbolp carcmd) (get carcmd 'flag))
	   `(comdecode '(set ,@cmd)))
	  ((null expertflag)
	   (throwfail "Unknown command."))
	  ((symbolp carcmd)
	   (if (fboundp carcmd) cmd
	       (throwfail ";" carcmd " - Undefined function.")))
	  (t cmd))))

(defun s-eqn-opdecode (command)
  (let ((keyword (car command))
	(dproof (get *current-eqnder* 's-eqn-der))
	mainfn closefn result)
    (declare (special dproof))
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values-dynamic-help keyword
		       (copy (cdr command))
		       (get keyword 's-eqn-argtypes)
		       (mapcar #'(lambda (x) (declare (ignore x)) nil)
			       (get keyword 's-eqn-argtypes))
		       nil
		       (get keyword 's-eqn-defaultfns)
		       nil
		       (get keyword 's-eqn-argnames)
		       (get keyword 's-eqn-arghelp))
      (declare (ignore external-arglist))
      (setq mainfn (or (get keyword 's-eqn-mainfns) keyword))
      (setq closefn (get keyword 's-eqn-closefns))
      (%catch% (setq result (apply mainfn internal-arglist))
;	       (fail (complain f "Error from " mainfn ".  " 
;                               core::expand-catch-throw)
;		     (throwfail "Operation aborted.")))
	       (fail (if (and (consp core::expand-catch-throw)
			      (equal (car core::expand-catch-throw)
				     "Aborting by user request."))
			 (complain f core::expand-catch-throw)
		       (complain f "Error from " mainfn ".  " 
				 core::expand-catch-throw))
		     (throwfail "Operation aborted.")))
      (%catch% (setq result (if closefn (apply closefn (list result)) result))
	       (fail (complain f "Error from " mainfn ".  " 
                               core::expand-catch-throw)
		     (throwfail "Operation aborted.")))
      result)))

; mkaminski -- 10/8/2005
; Needed a way to change help messages while arguments are processed.
; Apart from strings, prompt-values-dynamic-help accepts symbols as elements
; of arghelp. The actual help messages are extracted using
; (get <message-symbol> 'message) and (get <message-symbol> 'printfn)
(defun prompt-values-dynamic-help (keyword arglist argtypes wffargtypes
				   typelist defaultfns strong-defaultlist
				   argnames arghelp)
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
		     (if (and (symbolp (car arghelp))
			      (get (symbol-value (car arghelp)) 'printfn))
			 (funcall (get (symbol-value (car arghelp)) 'printfn)
				  (if argnames (symbol-name (car argnames)) "")
				  " "
				  "(" (car argtypetail) ")"
				  ": " (if arghelp
					   (if (symbolp (car arghelp))
					       (get (symbol-value
						     (car arghelp)) 'message)
					     (car arghelp))
					 ""))
		       (msgf (if argnames (symbol-name (car argnames)) "") " "
			     "(" (car argtypetail) ")"
			     ": " (if arghelp
				      (if (symbolp (car arghelp))
					  (get (symbol-value (car arghelp))
					       'message)
					(car arghelp))
				    "")))
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

(defmexpr rewriting
  (mainfns s-eqn)
  (mhelp "Enter the REWRITING top level."))

(context subtoplevels)

(defseqn leave
  (s-eqn-mainfns exit-s-eqn)
  (mhelp "Leave the REWRITING top level."))

(defun exit-s-eqn ()
  ;(if (and *current-eqnder* (get *current-eqnder* 'executing-rewrite))
  ;    (complain "A REWRITE command is in progress. Use OK or ABORT to leave" t
  ;              "the REWRITING top level.")
  (%throw% '|[Left REWRITING.]| exit-inferior-top));)

(context s-eqn-printing)

(defseqn pall
  (s-eqn-mainfns s-eqn-pall)
  (mhelp "Print all the lines in the current derivation."))

(defun is-seqnder-p (a)
  (and a (atom a) (get a 's-eqn-der) (atom (get a 's-eqn-der))))

(defun s-eqn-pall ()
  (if (is-seqnder-p *current-eqnder*)
      (pall)
    (throwfail "There is no current rewriting derivation." t "Use PROVE to start one.")))

(defun get-symbol-tex-face (sym)
  (let ((texname (get sym 'texname))
	(face (get sym 'face)))
    (cond (texname (format nil "\\~A" texname))
	  (face (get-symbol-tex-face (car face)))
	  (t (symbol-name sym)))))

(defun get-s-eqn-relation-symbol (style just-rule)
  (let* ((cur-theory (get *current-eqnder* 'theory))
	 (just (find-symbol (string-upcase
			     (string-right-trim "*+" just-rule))))
	 (rewriting-relation-symbol
	  (cond ((not cur-theory) rewriting-relation-symbol)
		((member just
			 (append (trules-used-by cur-theory)
				 (conservative-subtheories cur-theory)))
		 (if (get *current-eqnder* 'relation-sign)
		     (get *current-eqnder* 'relation-sign)
		   rewriting-relation-symbol))
		(t (dolist (th (non-conservative-subtheories cur-theory))
		     (when (or (eq just th)
			       (member just (trules-used-by th)))
		       (return (if (get th 'relation-sign)
				   (get th 'relation-sign)
				 rewriting-relation-symbol))))))))
    (if (or (eq rewriting-relation-symbol '=)
	    (and (string= just-rule (string-right-trim "*" just-rule))
		 (not (member just-rule (list "Same as") ;"Beta" "Eta")
			      :test #'equal))))
	(if (member style (list 'tex 'tex-1))
	    (get-symbol-tex-face rewriting-relation-symbol)
	  rewriting-relation-symbol)
      (if (member style (list 'tex 'tex-1))
	  ;(format nil "~A=" (symbol-name rewriting-relation-symbol))
	  "="
	;(intern (format nil "~A=" (symbol-name rewriting-relation-symbol)))
	'=
	))))

(defun print-s-eqn-line-generic (line)
  (let ((hatomalist nil) (curpos 0) justificationlength)
    (declare (special hatomalist))
    (msg "(" (linealias line) ") ") 
    (if (eq turnstile-indent-auto 'compress) 
	(setq curpos (+ curpos 3 (length (princ-to-string (linealias line)))))
      (progn (spaces (- 3 (length (princ-to-string (linealias line)))))
	     (setq curpos 6)))
    (if (eq turnstile-indent-auto 'fix) 
	(progn (if (> curpos turnstile-indent) (progn (msg t) (setq curpos -1)))
	       (indentation turnstile-indent) (setq curpos (1- turnstile-indent)))
      (if (eq turnstile-indent-auto 'min) 
	  (progn (let ((point (1+ (core::figure-out-indent)))) (indentation point) (setq curpos (1- point))))))
    (if (and (line-just-lines line)
	     (eq (cdr (foldl #'(lambda (cur prev) (if (car prev) prev
						    (if (eq cur line)
							(cons t (cdr prev))
						      (cons nil cur))))
			     (cons nil nil)
			     (proof-lines (get *current-eqnder* 's-eqn-der))))
		 (car (line-just-lines line))))
	(pcall print-symbol (get-s-eqn-relation-symbol 'generic
						       (line-just-rule line)))
      (princ " "))
    (if (eq turnstile-indent-auto 'compress) (setq curpos (1+ curpos)) (setq curpos (+ 2 curpos)))
    (cond ((not (fixp rightmargin))
	   (setq rightmargin (linewidth))))
    (if (eq turnstile-indent-auto 'compress)
	(setq hatomalist 
	      (prtwff (get line 'assertion)
		      (leftmargin (+ 1 curpos))))
      (setq hatomalist 
	    (prtwff (get line 'assertion)
		    (leftmargin (+ 2 curpos)))))
    (setq justificationlength (justlength (get line 'justification)))
    ;;Call this function after wff has been printed, because the
    ;;variable HATOMALIST is set when the wff is being printed. SI.
    (when (> (+ curpos justificationlength) rightmargin)
      (setq curpos 0)
      (terpri))
    (indentation (1+ (- rightmargin justificationlength)))
    (princjustification (get line 'justification))
    (setq curpos rightmargin)
    (when (and print-comments (get line 'comment) (listp (get line 'comment))) ;if it's a string, it's ""
	  (msg t "   ")
	  (eval (get line 'comment))
	  (msg t)
	  (setq curpos 0))))

(defun print-s-eqn-line-tex (line)
  (if (and (not ppwfflag) (not displaywff))
      (print-line-tex-plain line)
    (let* ((hatomalist nil) (curpos 0) 
	   (turnstile-indent
	    (if (eq turnstile-indent-auto 'MIN)
		(1+ (figure-out-indent))
	      turnstile-indent)))
      (declare (special hatomalist turnstile-indent))
      (let ((line-num (format nil "(~S) " (linealias line))))
	(setq curpos (length line-num))
	(princ "\\vbox{\\indent") (terpri)
	(progn (princ line-num)))
      (pcall print-indent 6)
      (if (and (line-just-lines line)
	       (eq (cdr (foldl #'(lambda (cur prev) (if (car prev) prev
						      (if (eq cur line)
							  (cons t (cdr prev))
							(cons nil cur))))
			       (cons nil nil)
			       (proof-lines (get *current-eqnder*
						 's-eqn-der))))
		   (car (line-just-lines line))))
	  (progn
	    (princ "\\ ")
	    (princ ;(symbol-name rewriting-relation-symbol)))
	     (get-s-eqn-relation-symbol 'tex (line-just-rule line))))
	(princ "\\ ~"))
      (setq curpos (1+ curpos))
      (cond ((not (fixp rightmargin))
	     (setq rightmargin (linewidth))))  ;;(linelength nil)
      (setq hatomalist 
	    (prtwff (get line 'assertion)
		    (leftmargin (+ 2 curpos))))
      (unless ppwfflag
	(unless auto-doc-flag (terpri))
	(msg "\\hfill ")
	(let ((justlen (justlength (get line 'justification))))
	  (if (> (+ curpos justlen 10) rightmargin)
	      (progn
		(princ "\\ceblinejust{")
		(princjustification (get line 'justification))
		(princ "}"))
	    (progn
	      (princ "\\hfill{")
	      (princjustification (get line 'justification))
	      (princ "}"))))
	(if (or (eq style 'tex-1) latex-emulation)
	    (msg "}\\filbreak " t)
	  (msg "}\\cr " t))
	(setq curpos rightmargin)
	(return-from print-s-eqn-line-tex rightmargin))
      (progn ;(setq curpos 0) 
	     (unless displaywff
	       (princ "\\lastformula"))
	     (let ((justlen (justlength (get line 'justification))))
	       (if (> (+ curpos justlen 10) rightmargin)
		   (progn
		     (princ "\\ceblinejust{")
		     (princjustification (get line 'justification))
		     (princ "}"))
		 (progn
		   (princ "\\hfill{")
		   (princjustification (get line 'justification)) 
		   (princ "}")))))
      (if (or (eq style 'tex-1) latex-emulation)
	  (progn (terpri) (princ "}\\filbreak"))
	(progn (terpri) (princ "}\\cr")))
      (setq curpos rightmargin)
      (when (and print-comments (get line 'comment)
		 (listp (get line 'comment))) ;if it's a string, it's ""
	(msg "\\vbox{\\indent ~ ~ {\\it ")
	(eval (tex-fudge-comment (get line 'comment)))
	(if (or (eq style 'tex-1) latex-emulation)
	    (msg "}}\\filbreak")
	  (msg "}}\\cr"))
	(setq curpos 0)))))

(defun se-numalias (num)
  (cdr (assoc num (proof-linealiases (get *current-eqnder* 's-eqn-der)))))

(defun se-nextplan ()
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof))
    (nextplan)))

(defun se-introduce-gap (line num)
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof))
    (introduce-gap line num)))

(defun se-find-all-gaps ()
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof))
    (find-all-gaps)))

(context s-eqn-entering)

(defseqn prove
  (s-eqn-argtypes rel-or-label symbol line-ge-2)
  (s-eqn-argnames relation prefix num)
  (s-eqn-arghelp "Prove Relation" "Name of Derivation" "Line Number for target wff (>=2)")
  (s-eqn-defaultfns
    (lambda (peqn &rest rest)
      (when (and *active-rewrite-theory*
		 (get *active-rewrite-theory* 'relation-sign))
	(setq rewriting-relation-symbol
	      (get *active-rewrite-theory* 'relation-sign)))
      (cons peqn
	    (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
		    (list (if (label-p peqn) peqn
			    (if (and (boundp '*last-gwff-typed*) 
				     *last-gwff-typed* (symbolp *last-gwff-typed*)) *last-gwff-typed*
			      '$)) 100)
		    rest))))
  (s-eqn-mainfns s-eqn-prove)
  (s-eqn-closefns %prtlines)
  (mhelp "Prove a relation by rewriting."))

(defun s-eqn-prove (eqn prefix num)
  (s-eqn-prove-generic nil t eqn prefix num))

(defseqn prove-in
  (s-eqn-argtypes theory rel-or-label symbol line-ge-2)
  (s-eqn-argnames theory relation prefix num)
  (s-eqn-arghelp "Rewrite Theory" "Prove Relation" "Name of Derivation"
		 "Line Number for target wff (>=2)")
  (s-eqn-defaultfns
    (lambda (theory peqn &rest rest)
      (when (and (specified-p theory) (get theory 'relation-sign))
	(setq rewriting-relation-symbol (get theory 'relation-sign)))
      (cons theory
	    (cons peqn
	    (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
		    (list (if (label-p peqn) peqn
			    (if (and (boundp '*last-gwff-typed*) 
				     *last-gwff-typed* (symbolp *last-gwff-typed*)) *last-gwff-typed*
			      '$)) 100)
		    rest)))))
  (s-eqn-mainfns s-eqn-prove-in)
  (s-eqn-closefns %prtlines)
  (mhelp "Prove a relation by rewriting using a particular theory."))

(defun s-eqn-prove-in (theory eqn prefix num)
  (let ((old-theory *active-rewrite-theory*))
    (use-theory theory)
    (let ((result (s-eqn-prove-generic nil t eqn prefix num)))
      (if old-theory (use-theory old-theory))
      result)))

(defun s-eqn-prove-generic (rewrite proof eqn prefix num)
  (let ((s-eqn-der (gensym))
	(src-line (gentemp))
	(trg-line (gentemp)))
    (setq *current-eqnder* prefix)
    (setf (get prefix 's-eqn-der) s-eqn-der)
    (setf (get prefix 'last-plan) nil)
    (setf (get prefix 'theory) *active-rewrite-theory*)
    (when *active-rewrite-theory*
      (setq rewriting-relation-symbol
	    (if (get *active-rewrite-theory* 'relation-sign)
		(get *active-rewrite-theory* 'relation-sign)
	      '=)))
    (if proof (setf (get prefix 'target-line) trg-line))
    (setf (get prefix 'executing-rewrite) rewrite)
    ;(setf (get s-eqn-der 'gwff-name) 
    ;  (if (symbolp *last-gwff-typed*)
    ;	  *last-gwff-typed*
    ;	nil))
    (setf (get s-eqn-der 'gwff-name) prefix)
    (setq *seqnder-list* (adjoin prefix *seqnder-list*))
    (if proof (setf (proof-assertion s-eqn-der) eqn))
    (setf (nextplan-no s-eqn-der) 1)
    (if proof
	(progn
	  (setf (proof-linealiases s-eqn-der)
		(list (cons 1 src-line) (cons num trg-line)))
	  (setf (proof-plans s-eqn-der) (list (list trg-line)))
	  (setf (proof-lines s-eqn-der) (list src-line trg-line))
	  (setf (line-assertion src-line) (gdr (gar eqn))))
      (progn
	(setf (proof-linealiases s-eqn-der) (list (cons 1 src-line)))
	(setf (proof-plans s-eqn-der) nil)
	(setf (proof-lines s-eqn-der) (list src-line))
	(setf (line-assertion src-line) eqn)))
    (setf (line-justification src-line) '("" nil nil))
    (setf (line-linenumber src-line) 1)
    (when proof
      (setf (line-assertion trg-line) (gdr eqn))
      (setf (line-justification trg-line) (se-nextplan))
      (setf (line-linenumber trg-line) num))
    (se-find-all-gaps)
    (if proof (list src-line trg-line)
      (list src-line))))

(defseqn derive
  (s-eqn-argtypes gwff-or-label symbol)
  (s-eqn-argnames wff prefix)
  (s-eqn-arghelp "Source Wff" "Name of Derivation")
  (s-eqn-defaultfns
    (lambda (peqn &rest rest)
      (cons peqn
	    (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
		    (list (if (label-p peqn) peqn
			    (if (and (boundp '*last-gwff-typed*) 
				     *last-gwff-typed* (symbolp *last-gwff-typed*)) *last-gwff-typed*
			      '$)))
		    rest))))
  (s-eqn-mainfns s-eqn-derive)
  (s-eqn-closefns %prtlines)
  (mhelp "Begin a rewrite derivation without a fixed target wff."))

(defun s-eqn-derive (wff prefix)
  (s-eqn-prove-generic nil nil wff prefix 100))  ; 100 is not used

(defseqn derive-in
  (s-eqn-argtypes theory gwff-or-label symbol)
  (s-eqn-argnames theory wff prefix)
  (s-eqn-arghelp "Rewrite Theory" "Source Wff" "Name of Derivation")
  (s-eqn-defaultfns
    (lambda (theory peqn &rest rest)
      (cons theory
	    (cons peqn
		  (mapcar #'(lambda (argdefault arg)
			      (if (eq arg '$) argdefault arg))
			  (list (if (label-p peqn) peqn
				  (if (and (boundp '*last-gwff-typed*) 
					   *last-gwff-typed* (symbolp *last-gwff-typed*)) *last-gwff-typed*
				    '$)))
			  rest)))))
  (s-eqn-mainfns s-eqn-derive-in)
  (s-eqn-closefns %prtlines)
  (mhelp "Start a derivation by rewriting using a particular theory."))

(defun s-eqn-derive-in (theory wff prefix)
  (let ((old-theory *active-rewrite-theory*))
    (use-theory theory)
    (let ((result
	   (s-eqn-prove-generic nil nil wff prefix 100)))  ; 100 is not used
      (if old-theory (use-theory old-theory))
      result)))

(context s-eqn-rules)

(defconstnt s-eqn-app-help-msg-a (gensym "help-message"))
;(setf (get s-eqn-app-help-msg-a 'message) "Wff before rewriting")

(defconstnt s-eqn-app-help-msg-b (gensym "help-message"))
;(setf (get s-eqn-app-help-msg-b 'message) "Wff after rewriting")

(defun get-fplans ()
  (labels ((search-fplans (lines referenced fplans)
	    (if (null lines) fplans
	      (search-fplans (cdr lines)
			     (append (line-just-lines (car lines)) referenced)
			     (if (member (car lines) referenced :test #'equal)
				 fplans
			       (cons (car lines) fplans))))))
    (search-fplans (reverse (mapcar #'cdr (proof-linealiases
					   (get *current-eqnder* 's-eqn-der))))
		   nil nil)))

(defun get-next-fplan ()
  (let* ((fplans (get-fplans))
	 (last-fplan (get *current-eqnder* 'last-plan)))
    (if fplans
	(if (and last-fplan (member last-fplan fplans))
	    (line-linenumber last-fplan)
	  (line-linenumber (car fplans))))))

(defun trim-plans (plans curline topdown)
  (delete-if #'(lambda (line)
		 (or (and topdown (<= (line-linenumber line) curline))
		     (and (not topdown)
			  (>= (line-linenumber line) curline))))
	     (if topdown plans
	       (sort plans #'(lambda (x y)
			       (> (line-linenumber x)
				  (line-linenumber y)))))))

(defun find-rewrite-instance (curline rule inwff topdown)
  (let* ((eqn-der (get *current-eqnder* 's-eqn-der))
	 (plans (trim-plans
		 (if topdown (mapcar #'car (proof-plans eqn-der))
		   (get-fplans))
		 curline topdown)))
    (dolist (plan plans nil)
      (if (instance-of-ruleapp rule
			       (if topdown inwff (line-assertion plan))
			       (if topdown (line-assertion plan) inwff)
			       (subtheory-containing (rrule-name rule)))
	  (return (line-linenumber plan))))))

(defun find-wff-instance (curline wff topdown)
  (find-simple-instance curline #'(lambda (wff2) (wffeq-ab wff wff2)) topdown))

(defun find-possible-instance (curline topdown)
  (find-simple-instance curline #'(lambda (x) t) topdown))

(defun find-simple-instance (curline pred topdown)
  (let* ((eqn-der (get *current-eqnder* 's-eqn-der))
	 (plans (trim-plans
		 (if topdown (mapcar #'car (proof-plans eqn-der))
		   (get-fplans))
		 curline topdown)))
    (dolist (plan plans nil)
      (if (apply pred (line-assertion plan) nil)
	  (return (line-linenumber plan))))))

(defun find-any-wff-instance (curline wff topdown)
  (let ((wff-instance (find-wff-instance curline wff topdown)))
    (if wff-instance wff-instance
      (let ((plans (trim-plans
		    (mapcar #'cdr (proof-linealiases
				   (get *current-eqnder* 's-eqn-der)))
		    curline topdown)))
	(dolist (plan plans nil)
	  (if (wffeq-ab wff (line-assertion plan))
	      (return (line-linenumber plan))))))))

(defseqn app
  (s-eqn-argtypes rrule line line gwff-or-selection gwff-or-selection)
  (s-eqn-argnames rule p1 p2 a b)
  (s-eqn-arghelp "Rule to apply" "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 s-eqn-app-help-msg-a s-eqn-app-help-msg-b)
  (s-eqn-defaultfns s-eqn-app-defaults)
  (s-eqn-mainfns s-eqn-app)
  (s-eqn-closefns %prtlines)
  (mhelp "Apply a rewrite rule."))

(defun gwff-or-selection-p (wff-or-sel)
  (declare (special s-eqn-selection-range))
  (or (gwff-p wff-or-sel)
      (and (integerp wff-or-sel) (> wff-or-sel 0)
	   (<= wff-or-sel s-eqn-selection-range))))

(defun get-gwff-or-selection (wff-or-sel)
  (declare (special s-eqn-selection-range))
  (if (and (integerp wff-or-sel) (> wff-or-sel 0)
	   (<= wff-or-sel s-eqn-selection-range)) wff-or-sel
    (getwff-subtype 'gwff-p wff-or-sel)))

(defvar *s-eqn-rule-prototype-set* nil)

(defun instantiate-rrule (rule)
  (declare (special s-eqn-rule-prototype s-eqn-rule-instance))
  (if (and *s-eqn-rule-prototype-set* (not (eq rule s-eqn-rule-instance))
	   (not (eq rule s-eqn-rule-prototype)) (specified-p rule))
      (setq *s-eqn-rule-prototype-set* nil))
  (unless (or *s-eqn-rule-prototype-set*
	      (not (specified-p rule)))
    (setq *s-eqn-rule-prototype-set* t)
    (setq s-eqn-rule-prototype rule)
    (setq rule (gensym "rule"))
    (setq s-eqn-rule-instance rule)
    (let* ((typesubs (mapcar #'(lambda (x) (cons x (gensym "type")))
			     (get s-eqn-rule-prototype 'rtypelist)))
	   (typelist (mapcar #'cdr typesubs)))
      (setf (get rule 'before)
	    (substitute-types typesubs (get s-eqn-rule-prototype 'before)))
      (setf (get rule 'after)
	    (substitute-types typesubs (get s-eqn-rule-prototype 'after)))
      (setf (get rule 'bidirectional)
	    (get s-eqn-rule-prototype 'bidirectional))
      (setf (get rule 'variables)
	    (mapcar #'(lambda (x) (substitute-types typesubs x))
		    (get s-eqn-rule-prototype 'variables)))
      (setf (get rule 'rewfn) (get s-eqn-rule-prototype 'rewfn))
      (setf (get rule 'appfn) (get s-eqn-rule-prototype 'appfn))
      (setf (get rule 'mhelp) (get s-eqn-rule-prototype 'mhelp))
      (setf (get rule 'active) (get s-eqn-rule-prototype 'active))
      ;(setf (get rule 'typesubs) typesubs)
      (setf (get rule 'rtypelist) typelist))
    (let* ((fvars (remove-duplicates
		   (append (free-vars-of (get rule 'before))
			   (free-vars-of (get rule 'after)))))
	   (subs (mapcar #'(lambda (x) (cons x ;(funcall ren-var-fn x)
					     (fresh-var (type x)
							(getnameroot x))))
			 fvars)))
      (setf (get rule 'before)
	    (foldl #'(lambda (sub wff)
		       (substitute-term-var (cdr sub) (car sub) wff))
		   (get rule 'before) subs))
      (setf (get rule 'after)
	    (foldl #'(lambda (sub wff)
		       (substitute-term-var (cdr sub) (car sub) wff))
		   (get rule 'after) subs))
      (setf (get rule 'variables)
	    (foldl #'(lambda (sub vars)
		       (mapcar #'(lambda (x)
				   (substitute-term-var (cdr sub) (car sub) x))
			       vars))
		   (get rule 'variables) subs)))
    ;(if (active-p s-eqn-rule-prototype)
	;(setq global-rewrite-rule-list (cons rule global-rewrite-rule-list)))
    )
  (if (specified-p rule) s-eqn-rule-instance '$))

(defun rrule-name (instance)
  (declare (special s-eqn-rule-prototype s-eqn-rule-instance))
  (if (and *s-eqn-rule-prototype-set*
	   (eq instance s-eqn-rule-instance))
      s-eqn-rule-prototype
    instance))

(defun uninstantiate-rrule (instance)
  (declare (special s-eqn-rule-prototype s-eqn-rule-instance))
  (when (and *s-eqn-rule-prototype-set*
	     (eq instance s-eqn-rule-instance))
    (if (active-p instance) (setq global-rewrite-rule-list
				  (delete instance global-rewrite-rule-list)))
    (setq *s-eqn-rule-prototype-set* nil)
    (setq s-eqn-rule-instance nil))
  s-eqn-rule-prototype)

(defun s-eqn-empty-printfn (a1 a2 a3 a4 a5 a6 a7)
  (declare (special s-eqn-selection-range))
  (setq s-eqn-selection-range 0)
  (msgf a1 a2 a3 a4 a5 a6 a7))

(defun s-eqn-gen-app-help-printfn (wff dir rule)
  (declare (special s-eqn-selection-range))
  (labels ((help-print (n rews)
		       (if (null rews) (format t "~%")
			 (progn
			   (format t " ~A)  " n)
			   ;(printwffhere (car rews))
			   (s-eqn-print-wff-schema (car rews)
						   (rrule-name rule))
			   (format t "~&")
			   (help-print (+ 1 n) (cdr rews))))))
    #'(lambda (a1 a2 a3 a4 a5 a6 a7)
	(let ((rews (generate-rwfflist
		     wff rule dir
		     (subtheory-containing (rrule-name rule)))))
	  (setq s-eqn-selection-range (length rews))
	  (msgf a1 a2 a3 a4 a5 a6 a7)
	  (unless (= 0 (length rews))
	    (format t "~%")
	    (help-print 1 rews))))))

(defun s-eqn-app-defaults (rule p1 p2 a b)
  (declare (special strong-defaultlist s-eqn-selection-range))
  (unless (or (not (specified-p rule))
	      (not (get *current-eqnder* 'theory))
	      (member (rrule-name rule)
		      ;(car (get (get *current-eqnder* 'theory) 'rrules))))
		      (rrules-used-by (get *current-eqnder* 'theory))))
    (throwfail (rrule-name rule) " not a member of "
	       (get *current-eqnder* 'theory)))
  (setq rule (instantiate-rrule rule))
  (unless (is-seqnder-p *current-eqnder*)
    (throwfail "There is no current rewriting derivation." t
	       "Use PROVE to start one."))
  (labels ((gen-help-printfn (wff dir)
			     (s-eqn-gen-app-help-printfn wff dir rule)))
    (let ((topdown t))
      (if (not (specified-p p1))
	  (progn
	    (setq p1 (get-next-fplan))
	    (unless p1
	      (setq topdown nil)
	      (setq-destruct ((p1 'ss))
			     (line-no-defaults-from
			      (eval-destruct ((p1 'ss))))))
	    (let ((lnum p1))
	      ;(setq p1 (if (or (eq lnum '$) topdown) p1 (- p1 1)))))))
	      (setq p1 (if (or (eq lnum '$) topdown) p1
			 (let ((ri (if (specified-p rule)
				       (find-rewrite-instance
					p1 rule (linenum-assertion p1)
					topdown))))
			   (if ri ri (- p1 1)))))))))
    ;(setq p2 (if (specified-p p2) p2 (+ p1 1)))
    (setq p2 (if (specified-p p2) p2
	       (let ((ri (if (and (specified-p rule) (specified-p p1)
				  (ln-exists p1))
			     (find-rewrite-instance
			      p1 rule (linenum-assertion p1) t))))
		 (if ri ri (+ p1 1)))))
    (setq a (if (not (specified-p p1)) '$
	      (if (or (specified-p a) (not (ln-exists p1)))
		  a (linenum-assertion p1))))
    (setq b (if (not (specified-p p2)) '$
	      (if (or (specified-p b) (not (ln-exists p2)))
		  b (linenum-assertion p2))))
    (setf (get s-eqn-app-help-msg-a 'message) "Wff before rewriting")
    (setf (get s-eqn-app-help-msg-b 'message) "Wff after rewriting")
    (if (and (not (strongly-specified-p a)) (strongly-specified-p b)
	     (specified-p rule))
	(progn
	  (setq a (if (< 0 (length (generate-rwfflist
				    b rule nil (subtheory-containing
						(rrule-name rule)))))
		      (if (not (specified-p a)) 1 a) '$))
	  (setf (get s-eqn-app-help-msg-a 'printfn) (gen-help-printfn b nil)))
      (setf (get s-eqn-app-help-msg-a 'printfn) #'s-eqn-empty-printfn))
    (if (and (not (strongly-specified-p b)) (strongly-specified-p a)
	     (specified-p rule))
	(progn
	  (setq b (if (< 0 (length (generate-rwfflist
				    a rule t (subtheory-containing
					      (rrule-name rule)))))
		      (if (not (specified-p b)) 1 b) '$))
	  (setf (get s-eqn-app-help-msg-b 'printfn) (gen-help-printfn a t)))
      (setf (get s-eqn-app-help-msg-b 'printfn) #'s-eqn-empty-printfn))
    (setq strong-defaultlist
	  (append '(nil nil nil) (mapcar #'strongly-specified-p (list a b))))
    (list rule p1 p2 a b)))

(defun generate-rwfflist (wff rule dir theory)
  (mapcar #'car (generate-rlist wff rule dir theory)))

(defun map-wff-vars (fun wff boundvars)
  (cond ((propsym-p wff) (apply fun (list wff boundvars)))
	((boundwff-q wff)
	 (cons (car wff)
	       (map-wff-vars fun (cdr wff) (cons (caar wff) boundvars))))
	((consp wff) (cons (map-wff-vars fun (car wff) boundvars)
			   (map-wff-vars fun (cdr wff) boundvars)))
	(t wff)))

(defun map-wff-free-vars (fun wff)
  (map-wff-vars #'(lambda (var boundvars)
		    (if (member var boundvars :test #'equal) var
		      (apply fun var))) wff nil))

(defun foldl (fun init list)
  (if (null list) init
    (foldl fun (apply fun (list (car list) init)) (cdr list))))

(defun apply-binding-subs (x)
  (foldl #'(lambda (sub x)
	     (if (propsym-p (cdr sub))
		 (substitute-l-term-var (cdr sub) (car sub) x)
	       x))
	 x *binding-subs*))

(defun type-instance (type schema ptypes)
  (let ((typesubs nil))
    (labels ((descend-type (type schema)
	       (cond ((symbolp schema)
		      (or (eq type schema)
			  (and (member schema ptypes)
			       (if (assoc schema typesubs)
				   (equal type (assoc schema typesubs))
				 (setq typesubs
				       (acons schema type typesubs))))))
		     ((symbolp type) nil)
		     (t (and (descend-type (car type) (car schema))
			     (descend-type (cdr type) (cdr schema)))))))
      (descend-type type schema))))

(defun s-eqn-prep-wff-schema (wff rule)
  (let ((typesubs nil))
    (labels ((descend-type (type typelist)
			   (when typelist
			     (cond ((symbolp type)
				    (if (and (equal "type"
						    (string-right-trim
						     "0123456789"
						     (symbol-name type)))
					     (not (assoc type typesubs)))
					(progn
					  (setq typesubs
						(acons type (car typelist)
						       typesubs))
					  (cdr typelist))
				      typelist))
				   ((consp type)
				    (descend-type (cdr type)
						  (descend-type (car type)
								typelist))))))
	     (mk-subs (wff typelist)
		      (cond ((lsymbol-q wff)
			     (descend-type (type wff) typelist))
			    (t (mk-subs (gdr wff)
					(mk-subs (gar wff) typelist))))))
      (mk-subs wff (get rule 'rtypelist))
      (substitute-types typesubs wff))))

(defun s-eqn-mk-wff-typesubs (wff rule)
  (let ((typesubs nil))
    (labels ((descend-type (type typelist)
			   (when typelist
			     (cond ((symbolp type)
				    (if (and (equal "type"
						    (string-right-trim
						     "0123456789"
						     (symbol-name type)))
					     (not (assoc type typesubs)))
					(progn
					  (setq typesubs
						(acons type (car typelist)
						       typesubs))
					  (cdr typelist))
				      typelist))
				   ((consp type)
				    (descend-type (cdr type)
						  (descend-type (car type)
								typelist))))))
	     (mk-subs (wff typelist)
		      (cond ((lsymbol-q wff)
			     (descend-type (type wff) typelist))
			    (t (mk-subs (gdr wff)
					(mk-subs (gar wff) typelist))))))
      (mk-subs wff (get rule 'rtypelist))
      typesubs)))

(defun s-eqn-print-wff-schema (wff rule)
  (printwffhere (s-eqn-prep-wff-schema wff rule)))

(defun mk-prompt-type-subs (wff fvars bvars rule)
  (let ((typesubs (s-eqn-mk-wff-typesubs wff rule)))
    (values (substitute-types typesubs wff)
	    (mapcar #'(lambda (x) (substitute-types typesubs x)) fvars)
	    (mapcar #'(lambda (x) (substitute-types typesubs x)) bvars)
	    (mapcar #'(lambda (p) (cons (cdr p) (car p))) typesubs))))

(defun prompt-subs (wff fvars bvars rule)
  (multiple-value-bind (wff fvars bvars revsubs)
      (mk-prompt-type-subs wff fvars bvars rule)
    (labels ((read-loop (var)
	       (let ((term nil))
		     ;(varinst (s-eqn-prep-wff-schema var rule)))
		 (prompt-read
		  term nil
		  (progn (msgf "(GWFF): Substitution term for ")
			 (printwffhere var)
			 ;(s-eqn-print-wff-schema var rule)
			 )
		'gwff var
		((? (progn
		      (msgf "You may wish to instantiate the free variable ")
		      (printwffhere var)
		      ;(s-eqn-print-wff-schema var rule)
		      (msgf "with a wff of the same type.")))
		 (?? (mhelp 'gwff))
		 (abort (throwfail
			 "Aborting by user request."))))
		 (if (equal (type term) (type var))
		     ;(type-instance (unabbreviated-type term)
			;	    (unabbreviated-type var)
			;	    (get rule 'rtypelist))
		     term
		   (progn
		     (msgf "Term does not have the required type: " (type var))
		     (read-loop var))))))
      ;(substitute-types revsubs
      (foldl #'(lambda (var wff)
		 (if (member var bvars) wff
		   (substitute-l-term-var (read-loop var) var wff)))
	     wff fvars))));)

(defun s-eqn-app (rule l1 l2 a b)
  (let* ((rule (instantiate-rrule rule))
	 ;(a1 a)
	 ;(b1 b)
	 (rew-a (if (integerp a)
		    (nth (- a 1) (generate-rlist b rule nil
						 (subtheory-containing
						  (rrule-name rule))))
		  (list a)))
	 (rew-b (if (integerp b)
		    (nth (- b 1) (generate-rlist a rule t
						 (subtheory-containing
						  (rrule-name rule))))
		  (list b)))
	 (a (if (integerp a) (prompt-subs (car rew-a) (cdr rew-a)
					  (free-vars-of (car rew-b))
					  (rrule-name rule))
	      a))
	 (b (if (integerp b) (prompt-subs (car rew-b) (cdr rew-b)
					  (free-vars-of (car rew-a))
					  (rrule-name rule))
	      ;(if (integerp a1)
		;  (substitute-types
		;   (s-eqn-mk-wff-typesubs a (rrule-name rule)) b)
	      b)));)
    ;(when (integerp b1)
    ;  (setq a (substitute-types
	;       (s-eqn-mk-wff-typesubs b (rrule-name rule)) a)))
    ;(princ a)
    ;(format t "~%")
    ;(princ b)
    (unless ;(or
	(instance-of-ruleapp rule a b
			     (subtheory-containing (rrule-name rule)))
      ;(instance-of-ruleapp (rrule-name rule) a b
      ;(subtheory-containing (rrule-name rule))))
      (throwfail (rrule-name rule) " not applicable."))
    (let ((new-lines (s-eqn-modify-plan (string-capitalize
					 (princ-to-string (rrule-name rule)))
					l1 l2 a b)))
      (uninstantiate-rrule rule)
      new-lines)))

(defun s-eqn-modify-plan (rule l1 l2 a b)
  (let ((topdown (ln-exists l1))
	(bottomup (ln-exists l2))
	(eqn-der (get *current-eqnder* 's-eqn-der)))
    (unless (>= l2 l1)
      (throwfail "Line " l1 " which justifies line " l2 " occurs after line "
		 l2 "."))
    (unless (< l1 l2)
      (throwfail "Line " l1 " needs to be justified by a preceding line."))
    (unless topdown
      (let ((new-line (gentemp)))
	(setf (line-assertion new-line) a)
	(setf (line-justification new-line) (se-nextplan))
	(setf (line-linenumber new-line) l1)
	(setf (proof-lines eqn-der)
	      (sort (cons new-line (proof-lines eqn-der))
		    #'(lambda (x y) (< (line-linenumber x)
				       (line-linenumber y)))))
	(setf (proof-plans eqn-der)
	      (cons (list new-line) (proof-plans eqn-der)))
	(setf (proof-linealiases eqn-der)
	      (sort (cons (cons l1 new-line) (proof-linealiases eqn-der))
		    #'(lambda (x y) (< (car x) (car y)))))))
    (if (ln-exists l2)
	(progn
	  (setf (proof-plans eqn-der)
		(remove (list (linenum-line l2)) (proof-plans eqn-der)
			:test #'equal))
	  (setf (line-justification (linenum-line l2))
		`(,rule nil ,(list (linenum-line l1)))))
      (let ((new-line (gentemp)))
	(setf (line-assertion new-line) b)
	(setf (line-justification new-line)
	      `(,rule nil ,(list (linenum-line l1))))
	(setf (line-linenumber new-line) l2)
	(setf (proof-lines eqn-der)
	      (sort (cons new-line (proof-lines eqn-der))
		    #'(lambda (x y) (< (line-linenumber x)
				       (line-linenumber y)))))
	(setf (proof-linealiases eqn-der)
	      (sort (cons (cons l2 new-line) (proof-linealiases eqn-der))
		    #'(lambda (x y) (< (car x) (car y)))))))
    (se-find-all-gaps)
    (setf (get *current-eqnder* 'last-plan)
	  (if (eq (linenum-line l2) (get *current-eqnder* 'target-line)) nil
	    (linenum-line l2)))
    (if (and (not topdown) (not bottomup))
	(list (linenum-line l1) (linenum-line l2))
      (list (linenum-line (if topdown l2 l1))))))

(defseqn any
  (s-eqn-argtypes line line gwff-or-selection gwff-or-selection)
  (s-eqn-argnames p1 p2 a b)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 s-eqn-app-help-msg-a s-eqn-app-help-msg-b)
  (s-eqn-defaultfns s-eqn-any-defaults)
  (s-eqn-mainfns s-eqn-any)
  (s-eqn-closefns %prtlines)
  (mhelp "Try to apply any active rewrite rule from the current theory and all
its subtheories. If there is no current theory, all active rewrite rules
will be tried."))

(defun generate-rlist-any (wff rules dir)
  (foldl #'(lambda (rule rwffs)
	     (setq rule (instantiate-rrule rule))
	     (append (mapcar #'(lambda (rew) (cons (rrule-name rule) rew))
			     (generate-rlist wff rule dir
					     (subtheory-containing
					      (rrule-name rule))))
		     rwffs))
	 nil rules))

(defun generate-rwfflist-any (wff rules dir)
  (foldl #'(lambda (rule rwffs)
	     (setq rule (instantiate-rrule rule))
	     (append (mapcar #'(lambda (wff) (cons (rrule-name rule) wff))
			     (generate-rwfflist wff rule dir
						(subtheory-containing
						 (rrule-name rule))))
		     rwffs))
	 nil rules))

(defun s-eqn-gen-any-help-printfn (wff dir rules)
  (declare (special s-eqn-selection-range))
  (labels ((help-print (n rews)
		       (if (null rews) (format t "~%")
			 (progn
			   (format t " ~A)  " n)
			   (s-eqn-print-wff-schema (cdar rews) (caar rews))
			   ;(printwffhere (cdar rews))
			   (format t "  (~A)~&" (caar rews))
			   (help-print (+ 1 n) (cdr rews))))))
    #'(lambda (a1 a2 a3 a4 a5 a6 a7)
	(let ((rews (generate-rwfflist-any wff rules dir)))
	  (setq s-eqn-selection-range (length rews))
	  (msgf a1 a2 a3 a4 a5 a6 a7)
	  (unless (= 0 (length rews))
	    (format t "~%")
	    (help-print 1 rews))))))

(defun s-eqn-any-defaults (p1 p2 a b)
  (declare (special strong-defaultlist s-eqn-selection-range))
  (let ((rules (remove-if
		#'(lambda (rule)
		    (and (get *current-eqnder* 'theory)
			 (not (member rule
				      ;(car (get (get *current-eqnder* 'theory)
					;	'rrules))))))
				      (rrules-used-by
				       (get *current-eqnder* 'theory))))))
		(remove-if-not 'active-p global-rewrite-rule-list))))
    (unless rules
      (if (get *current-eqnder* 'theory)
	  (throwfail "No rules from " (get *current-eqnder* 'theory)
		     " are active.")
	(throwfail "No rewrite rules are active. Activate some rules before
trying to apply ANY.")))
  (unless (is-seqnder-p *current-eqnder*)
    (throwfail "There is no current rewriting derivation." t
	       "Use PROVE to start one."))
  (labels ((gen-help-printfn (wff dir)
			     (s-eqn-gen-any-help-printfn wff dir rules)))
    (let ((topdown t))
      (if (not (specified-p p1))
	  (progn
	    (setq p1 (get-next-fplan))
	    (unless p1
	      (setq topdown nil)
	      (setq-destruct ((p1 'ss))
			     (line-no-defaults-from
			      (eval-destruct ((p1 'ss))))))
	    (let ((lnum p1))
	      (setq p1 (if (or (eq lnum '$) topdown) p1 (- p1 1)))))))
    (setq p2 (if (specified-p p2) p2 (+ p1 1)))
    (setq a (if (not (specified-p p1)) '$
	      (if (or (specified-p a) (not (ln-exists p1)))
		  a (linenum-assertion p1))))
    (setq b (if (not (specified-p p2)) '$
	      (if (or (specified-p b) (not (ln-exists p2)))
		  b (linenum-assertion p2))))
    (setf (get s-eqn-app-help-msg-a 'message) "Wff before rewriting")
    (setf (get s-eqn-app-help-msg-b 'message) "Wff after rewriting")
    (if (and (not (strongly-specified-p a)) (strongly-specified-p b))
	(progn
	  (setq a (if (< 0 (length (generate-rwfflist-any b rules nil)))
		      (if (not (specified-p a)) 1 a) '$))
	  (setf (get s-eqn-app-help-msg-a 'printfn) (gen-help-printfn b nil)))
      (setf (get s-eqn-app-help-msg-a 'printfn) #'s-eqn-empty-printfn))
    (if (and (not (strongly-specified-p b)) (strongly-specified-p a))
	(progn
	  (setq b (if (< 0 (length (generate-rwfflist-any a rules t)))
		      (if (not (specified-p b)) 1 b) '$))
	  (setf (get s-eqn-app-help-msg-b 'printfn) (gen-help-printfn a t)))
      (setf (get s-eqn-app-help-msg-b 'printfn) #'s-eqn-empty-printfn))
    (setq strong-defaultlist
	  (append '(nil nil) (mapcar #'strongly-specified-p (list a b))))
    (list p1 p2 a b))))

(defun s-eqn-any (l1 l2 a b)
  (let* ((rules (remove-if
		 #'(lambda (rule)
		     (and (get *current-eqnder* 'theory)
			  (not (member rule
				       ;(car (get (get *current-eqnder* 'theory)
					;	 'rrules))))))
				       (rrules-used-by
					(get *current-eqnder* 'theory))))))
		 (remove-if-not 'active-p global-rewrite-rule-list)))
	 (rew-a (if (integerp a) (nth (- a 1) (generate-rlist-any b rules nil))
		  (list nil a)))
	 (rew-b (if (integerp b) (nth (- b 1) (generate-rlist-any a rules t))
		  (list nil b)))
	 (rule (cond ((integerp a) (car rew-a))
		     ((integerp b) (car rew-b))
		     (t nil)))
	 (a (if (integerp a) (prompt-subs (cadr rew-a) (cddr rew-a)
					  (free-vars-of (cadr rew-b))
					  (car rew-a))
	      a))
	 (b (if (integerp b) (prompt-subs (cadr rew-b) (cddr rew-b)
					  (free-vars-of (cadr rew-a))
					  (car rew-b))
	      b)))
    (cond (rule (setq rule (instantiate-rrule rule))
		(unless (instance-of-ruleapp rule a b
					     (subtheory-containing
					      (rrule-name rule)))
		  (throwfail (rrule-name rule) " not applicable.")))
	  (t (setq rule (instance-of-rewriting-with rules a b))
	     (unless rule (throwfail "No rules applicable."))))
    (let ((new-lines (s-eqn-modify-plan (string-capitalize
					 (princ-to-string (rrule-name rule)))
					l1 l2 a b)))
      (uninstantiate-rrule rule)
      new-lines)))

(context s-eqn-rearrange)

(defseqn delete
  (s-eqn-argtypes existing-linelist)
  (s-eqn-argnames del-lines)
  (s-eqn-arghelp "delete lines")
  (s-eqn-mainfns s-eqn-delete)
  (mhelp "      Delete lines from the proof outline."))

(defun s-eqn-delete (&rest args)
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof))
    (cond ((member (linenum-line 1) (car args))
	   (throwfail "Line 1 cannot be deleted"))
	  ((member (get *current-eqnder* 'target-line) (car args))
	   (throwfail "Line " (line-linenumber
			       (get *current-eqnder* 'target-line))
		      " cannot be deleted")))
    (apply #'dellines args)))

(context subtoplevels)

(defmexpr rewrite
  (argtypes line line gwff-or-nil gwff-or-nil linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a b p2-hyps p1-hyps)
  (arghelp "Line after rewriting (higher-numbered)" "Line before rewriting (lower-numbered)" "Wff before rewriting" "Wff after rewriting" "Hypotheses" "Hypotheses")
  (defaultfns s-eqn-rewrite-defaults)
  (mainfns s-eqn-rewrite)
  (mhelp "Rewrite a line of the current natural deduction proof in the
REWRITING top level. When finished rewriting, use OK to leave the REWRITING top
level, modifying the main proof accordingly."))

(defun s-eqn-rewrite-defaults (p2 p1 a b p2-hyps p1-hyps)
  (let ((defaults (use-rrules-defaults p2 p1 a b p2-hyps p1-hyps)))
    (if (not (specified-p (nth 2 defaults)))
	(setf (nth 2 defaults) nil))
    (if (not (specified-p (nth 3 defaults)))
	(setf (nth 3 defaults) nil))
    defaults))

(defun s-eqn-rewrite (p2 p1 a b p2-hyps p1-hyps)
  (setq rewriting-relation-symbol '=)
  (cond ((and a b)
	 (s-eqn-prove-generic (list p2 p1 a b p2-hyps p1-hyps)
			      t (acons (inherit-abbrev
					'= '((O . O) . O) '(O))
				       a b) (gensym "s-eqn-der") 100))
	(a (s-eqn-prove-generic (list p2 p1 a b p2-hyps p1-hyps)
				nil a (gensym "s-eqn-der") nil))
	(b (s-eqn-prove-generic (list p2 p1 a b p2-hyps p1-hyps)
				nil b (gensym "s-eqn-der") nil)))
  (s-eqn))

(defmexpr rewrite-in
  (argtypes theory line line gwff-or-nil gwff-or-nil linelist linelist)
  (wffargtypes nil nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames theory p2 p1 a b p2-hyps p1-hyps)
  (arghelp "Rewrite Theory" "Line after rewriting (higher-numbered)" "Line before rewriting (lower-numbered)" "Wff before rewriting" "Wff after rewriting" "Hypotheses" "Hypotheses")
  (defaultfns s-eqn-rewrite-in-defaults)
  (mainfns s-eqn-rewrite-in)
  (mhelp "Rewrite a line in the REWRITING top level using a particular
theory."))

(defun s-eqn-rewrite-in-defaults (theory p2 p1 a b p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((defaults (use-rrules-defaults p2 p1 a b p2-hyps p1-hyps)))
    (if (not (specified-p (nth 2 defaults)))
	(setf (nth 2 defaults) nil))
    (if (not (specified-p (nth 3 defaults)))
	(setf (nth 3 defaults) nil))
    (if strong-defaultlist
	(setq strong-defaultlist (cons nil strong-defaultlist)))
    (cons theory defaults)))

(defun s-eqn-rewrite-in (theory p2 p1 a b p2-hyps p1-hyps)
  (let ((old-theory *active-rewrite-theory*))
    (use-theory theory)
    (cond ((and a b)
	   (s-eqn-prove-generic (list p2 p1 a b p2-hyps p1-hyps)
				t (acons (inherit-abbrev
					  '= '((O . O) . O) '(O))
					 a b) (gensym "s-eqn-der") 100))
	  (a (s-eqn-prove-generic (list p2 p1 a b p2-hyps p1-hyps)
				  nil a (gensym "s-eqn-der") nil))
	  (b (s-eqn-prove-generic (list p2 p1 a b p2-hyps p1-hyps)
				  nil b (gensym "s-eqn-der") nil)))
    (if old-theory (use-theory old-theory))
    (s-eqn)))

(defseqn ok
  (s-eqn-argnames p2 p1 a b p2-hyps p1-hyps num)
  (s-eqn-argtypes line line gwff-or-nil gwff-or-nil linelist linelist
		  existing-line)
  (s-eqn-arghelp "Line after rewriting (higher-numbered)"
		 "Line before rewriting (lower-numbered)"
		 "Wff before rewriting" "Wff after rewriting"
		 "Hypotheses" "Hypotheses" "Target Line")
  (s-eqn-defaultfns s-eqn-ok-defaults)
  (s-eqn-mainfns s-eqn-ok)
  (mhelp "Leave the REWRITING top level, completing a REWRITE command."))

(defun s-eqn-ok-defaults (p2 p1 a b p2-hyps p1-hyps line)
  (declare (ignore line)
	   (special strong-defaultlist))
  (setq strong-defaultlist '(nil nil nil nil nil nil nil))
  (labels ((ok-defaults (p2 p1 a b p2-hyps p1-hyps)
	     (if (and (or (not (ln-exists-top p2))
			  (and (ln-exists-top p2)
			       (wffeq-ab (linenum-assertion-top p2) b)
			       (equal (linenum-hyps-top p2) p2-hyps)))
		      (specified-p b)
		      (specified-p p2-hyps))
		 (progn
		   (setf (nth 0 strong-defaultlist) t)
		   (setf (nth 3 strong-defaultlist) t)
		   (setf (nth 4 strong-defaultlist) t))
	       (progn
		 (setf (nth 0 strong-defaultlist) nil)
		 (setf (nth 3 strong-defaultlist) nil)
		 (setf (nth 4 strong-defaultlist) nil)))
	     (setf (nth 1 strong-defaultlist) nil)
	     (setf (nth 2 strong-defaultlist) nil)
	     (setf (nth 5 strong-defaultlist) nil)
	     (setf (nth 6 strong-defaultlist) nil)
	     (when (and (or (not (ln-exists-top p1))
			    (and (ln-exists-top p1)
				 (wffeq-ab (linenum-assertion-top p1) a)
				 (equal (linenum-hyps-top p1) p1-hyps)))
			(specified-p a)
			(specified-p p1-hyps))
	       (setf (nth 1 strong-defaultlist) t)
	       (setf (nth 2 strong-defaultlist) t)
	       (setf (nth 5 strong-defaultlist) t))
	     (list p2 p1 a b p2-hyps p1-hyps
		   (if (get *current-eqnder* 'target-line)
		       (progn
			 (setf (nth 6 strong-defaultlist) t)
			 (get *current-eqnder* 'target-line))
		     (linenum-line (get-next-fplan))))))
    (if (and *current-eqnder* (get *current-eqnder* 'executing-rewrite))
	(let ((new-vals (apply #'ok-defaults
			       (get *current-eqnder* 'executing-rewrite)))
	      (eqnder (get *current-eqnder* 'executing-rewrite)))
	  (if (or (not (specified-p p2)) (= p2 (nth 0 new-vals)))
	      (setf (nth 0 eqnder) (nth 0 new-vals))
	    (setf (nth 0 eqnder) p2))
	  (if (or (not (specified-p b)) (wffeq-ab b (nth 3 new-vals)))
	      (progn
		(setf (nth 3 eqnder) (nth 3 new-vals))
		(setf (nth 4 eqnder) (nth 4 new-vals)))
	    (progn
	      (setf (nth 3 eqnder) b)
	      (setf (nth 4 eqnder) p2-hyps)))
	  (if (or (not (specified-p p1)) (= p1 (nth 1 new-vals)))
	      (setf (nth 1 eqnder) (nth 1 new-vals))
	    (setf (nth 1 eqnder) p1))
	  (if (or (not (specified-p a)) (wffeq-ab a (nth 2 new-vals)))
	      (progn
		(setf (nth 2 eqnder) (nth 2 new-vals))
		(setf (nth 5 eqnder) (nth 5 new-vals)))
	    (progn
	      (setf (nth 2 eqnder) a)
	      (setf (nth 5 eqnder) p1-hyps)))
	  (append eqnder (list (nth 6 new-vals))))
      (throwfail "No REWRITE command in progress."))))

(defun s-eqn-done (line)
  (s-eqn-done-generic (linenum-line 1) line))

(defun s-eqn-done-generic (first-line line)
  (labels ((search (jlist line)
		   (cond ((= (line-linenumber line)
			     (line-linenumber first-line) ;1
			     ) (cond (jlist)
				     (t)))
			 ((null (line-just-lines line)) nil)
			 (t (search (cons (line-just-rule line) jlist)
				    (car (line-just-lines line)))))))
    (search nil line)))

(defun s-eqn-strict-p (jlist)
  (and
   jlist
   (or (not (get *current-eqnder* 'theory))
       (get (get *current-eqnder* 'theory) 'reflexive)
       (when (consp jlist)
	 (remove-if
	  #'(lambda (just)
	      (not (and (string= just (string-right-trim "*" just))
			(member (find-symbol (string-upcase
					      (string-right-trim "+" just)))
				(cons 'any
				      (cons
				       (get *current-eqnder* 'theory)
				       (trules-used-by
					(get *current-eqnder* 'theory))))))))
	  jlist)))))

(defun s-eqn-find-unjust-line (line)
  (labels ((search (line)
		   (cond ((= (line-linenumber line) 1) nil)
			 ((null (line-just-lines line)) line)
			 (t (search (car (line-just-lines line)))))))
    (search line)))

(defun s-eqn-update-plan (default-exist default-new)
  (let ((dproof *s-eqn-dproof-backup*))
    (declare (special dproof))
    (update-plan default-exist default-new)))

(defun s-eqn-ok-enter (p2 p1 a b p2-hyps p1-hyps)
  (s-eqn-update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

(defun s-eqn-ok-build (p2 p1 a b p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (use-rrules-mv1 use-rrules-mv0))
      (unquoted maint::wffarg (a b)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (use-rrules-p2 use-rrules-p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (p2 p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((list (if (and verbose-rewrite-justification
		       (get *current-eqnder* 'theory))
		  (format nil "Rewrite(~A)" (get *current-eqnder* 'theory))
		"Rewrite")
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list p1)))
        (nextplan)))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a b p2-hyps p1-hyps))))

(defun s-eqn-ok (p2 p1 a b p2-hyps p1-hyps line)
  (if (s-eqn-strict-p (s-eqn-done line))
      (let ((args (get *current-eqnder* 'executing-rewrite)))
	(let ((p1-ok1 (or (not (ln-exists-top p1))
			  (wffeq-ab (linenum-assertion 1)
				    (linenum-assertion-top p1))))
	      (p2-ok1 (or (not (ln-exists-top p2))
			  (wffeq-ab (line-assertion line)
				    (linenum-assertion-top p2))))
	      (p1-ok2 (or (not (ln-exists-top p1))
			  (wffeq-ab (line-assertion line)
				    (linenum-assertion-top p1))))
	      (p2-ok2 (or (not (ln-exists-top p2))
			  (wffeq-ab (linenum-assertion 1)
				    (linenum-assertion-top p2))))
	      (a-ok1 (or (not a) (wffeq-ab a (linenum-assertion 1))))
	      (a-ok2 (or (not a) (wffeq-ab a (line-assertion line))))
	      (b-ok1 (or (not b) (wffeq-ab b (line-assertion line))))
	      (b-ok2 (or (not b) (wffeq-ab b (linenum-assertion 1)))))
	  (unless (and (wffeq-ab a (nth 2 args))
		       (wffeq-ab b (nth 3 args))
		       (or (and a-ok1 b-ok1)
			   (and a-ok2 b-ok2))
		       (or (and p1-ok1 p2-ok1)
			   (and p1-ok2 p2-ok2))
		       (equal p1-hyps p2-hyps))
	    (throwfail "Supplied arguments inconsistent with current proof
outline.")))
	(if (not (nth 2 args))
	    (setf (nth 2 args) (line-assertion line)))
	(if (not (nth 3 args))
	    (setf (nth 3 args) (line-assertion line)))
	(let ((args (apply #'s-eqn-ok-build args)))
	  (apply #'s-eqn-ok-enter args))
	(%throw% '|[Left REWRITING.]| exit-inferior-top))
    (complain "The planned assertion has not yet been proved.
Use the DONE command for more details.")))

(defseqn assert-top
  (s-eqn-argtypes existing-line line)
  (s-eqn-argnames line p1)
  (s-eqn-arghelp "Target Line of Derivation"
		 "Line of current ND proof with asserted relation")
  (s-eqn-defaultfns s-eqn-assert-top-defaults)
  (s-eqn-mainfns s-eqn-assert-top)
  (mhelp "Leave the REWRITING top-level, inserting the obtained relation as a
lemma into the current natural deduction proof."))

(defun s-eqn-assert-top-defaults (line p1)
  (declare (special strong-defaultlist))
  (when (and (get *current-eqnder* 'target-line) (not (specified-p line)))
    (setq line (get *current-eqnder* 'target-line))
    (setq strong-defaultlist '(t nil)))
  (when (and (specified-p line) (not (specified-p p1)))
    (let ((dproof *s-eqn-dproof-backup*))
      (declare (special dproof))
      (setq-destruct ((p1 'ss)) (line-no-defaults-from
				 (eval-destruct ((p1 'ss)))))
      ;(let ((ri (find-wff-instance 0 line t)))
      ;  (if ri (setq p1 ri)))
      ))
  (list line p1))

(defun s-eqn-assert-top (line p1)
  (if (s-eqn-strict-p (s-eqn-done line))
      (let ((thm (gensym "thm")))
	(putprop thm
		 (if (get *current-eqnder* 'assertion)
		     (get *current-eqnder* 'assertion)
		   (acons (inherit-abbrev
			   (let ((th (get *current-eqnder* 'theory)))
			     (if th (if (get th 'relation-sign)
					(get th 'relation-sign)
				      '=)
			       rewriting-relation-symbol))
			   (acons 'O (type (linenum-assertion 1))
				  (type line))
			   (remove-duplicates
			    (list (type line)
				  (type (linenum-assertion 1)))))
			  (linenum-assertion 1)
			  (line-assertion line)))
		 'assertion)
	(putprop thm 'book 'thm-type)
	(putprop thm t '%THEOREM%)
	(let ((dproof *s-eqn-dproof-backup*))
	  (declare (special dproof))
	  (assert2-generic thm p1 "Asserted wff"
			   (list (if (and verbose-rewrite-justification
					  (get *current-eqnder* 'theory))
				     (conc-strings
				      "Rewrite("
				      (princ-to-string
				       (get *current-eqnder* 'theory))
				      ")")
				   "Rewrite") nil nil)))
	(%throw% '|[Left REWRITING.]| exit-inferior-top))
    (complain "The planned assertion has not yet been proved.
Use the DONE command for more details.")))

;(defseqn abort
;  (s-eqn-mainfns (lambda ()
;		   (if (and *current-eqnder* (get *current-eqnder*
;						  'executing-rewrite))
;		       (%throw% '|[Left REWRITING.]| exit-inferior-top)
;		     (complain "No REWRITE command in progress.
;Use LEAVE to leave the REWRITING top level."))))
;  (mhelp "Leave the REWRITING top level, aborting a REWRITE command."))

(defgwff-type rewriting-line-ref
  (checkfn rew-lineref-p)
  (getfn rew-lineref-getfn))

(defun rew-lineref-getfn (r)
  (if (and (consp r) (consp (cdr r)) (eq (car r) 'rew)
	   *current-eqnder* (get *current-eqnder* 's-eqn-der)
	   (assoc (cadr r)
		  (proof-linealiases (get *current-eqnder* 's-eqn-der))
		  :test #'equal))
      (line-assertion
       (cdr (assoc (cadr r)
		   (proof-linealiases (get *current-eqnder* 's-eqn-der))
		   :test #'equal)))
    (throwfail "Line reference badly formed or refers to a non-existing line.")))

(defun rew-lineref-p (r)
  (and (consp r) (eq (car r) 'rew)))

(defgwff-type dproof-line-ref
  (checkfn dproof-lineref-p)
  (getfn dproof-lineref-getfn))

(defun dproof-lineref-getfn (r)
  (if (and (consp r) (consp (cdr r)) (eq (car r) 'top) *s-eqn-dproof-backup*
	   (assoc (cadr r) (proof-linealiases *s-eqn-dproof-backup*)
		  :test #'equal))
      (line-assertion
       (cdr (assoc (cadr r) (proof-linealiases *s-eqn-dproof-backup*)
		   :test #'equal)))
    (throwfail "Line reference badly formed or refers to a non-existing line.")))

(defun dproof-lineref-p (r)
  (and (consp r) (eq (car r) 'top)))

(context s-eqn-theories)

(defseqn current-theory
  (s-eqn-mainfns
   (lambda ()
     (princ (get *current-eqnder* 'theory))))
  (mhelp "    Show the theory associated with current rewrite derivation."))

(context s-eqn-lambda)

(defun s-eqn-nf-defaults (fun p1 p2 a)
  (declare (special strong-defaultlist))
  (unless (is-seqnder-p *current-eqnder*)
    (throwfail "There is no current rewriting derivation." t
	       "Use PROVE to start one."))
  (let ((topdown t))
    (unless (specified-p p1)
      (setq p1 (get-next-fplan))
      (unless p1
	(setq topdown nil)
	(setq-destruct ((p1 'ss))
		       (line-no-defaults-from
			(eval-destruct ((p1 'ss))))))
      (let ((lnum p1))
	(setq p1 (if (or (eq lnum '$) topdown) p1 (- p1 1))))))
  (setq p2 (if (specified-p p2) p2
	     (let ((ri (if (and (specified-p p1) (ln-exists p1))
			   (find-wff-instance
			    p1 (apply fun (linenum-assertion p1) nil) t))))
	       (if ri ri (+ p1 1)))))
  (setq a (if (not (specified-p p1)) '$
	    (if (or (specified-p a) (not (ln-exists p1)))
		a (linenum-assertion p1))))
  (setq strong-defaultlist
	(append `(nil nil ,(strongly-specified-p a))))
  (list p1 p2 a))

(defun s-eqn-nf (fun cmdname just p1 p2 a)
  (if (and (ln-exists p2)
	   (not (wffeq-ab (linenum-assertion p2) (apply fun a nil))))
      (throwfail cmdname " not applicable.
Line " p2 " is not a " (string-downcase (princ-to-string cmdname))
                 " of line " p1))
  (s-eqn-modify-plan just p1 p2 a (apply fun a nil)))

(defseqn beta-nf
  (s-eqn-argtypes line line gwff)
  (s-eqn-argnames p1 p2 a)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Beta Redex")
  (s-eqn-defaultfns s-eqn-beta-nf-defaults)
  (s-eqn-mainfns s-eqn-beta-nf)
  (s-eqn-closefns %prtlines)
  (mhelp "Beta-normalize a line."))

(defun s-eqn-beta-nf-defaults (p1 p2 a)
  (s-eqn-nf-defaults #'lambda-norm p1 p2 a))

(defun s-eqn-beta-nf (p1 p2 a)
  (s-eqn-nf #'lambda-norm "BETA-NF" "Beta*" p1 p2 a))

(defseqn eta-nf
  (s-eqn-argtypes line line gwff)
  (s-eqn-argnames p1 p2 a)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Eta Redex")
  (s-eqn-defaultfns s-eqn-eta-nf-defaults)
  (s-eqn-mainfns s-eqn-eta-nf)
  (s-eqn-closefns %prtlines)
  (mhelp "Eta-normalize a line."))

(defun s-eqn-eta-nf-defaults (p1 p2 a)
  (s-eqn-nf-defaults #'etanorm p1 p2 a))

(defun s-eqn-eta-nf (p1 p2 a)
  (s-eqn-nf #'etanorm "ETA-NF" "Eta*" p1 p2 a))

(defseqn lambda-nf
  (s-eqn-argtypes line line gwff)
  (s-eqn-argnames p1 p2 a)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Lambda Redex")
  (s-eqn-defaultfns s-eqn-lambda-nf-defaults)
  (s-eqn-mainfns s-eqn-lambda-nf)
  (s-eqn-closefns %prtlines)
  (mhelp "Lambda-normalize a line."))

(defun s-eqn-lambda-nf-defaults (p1 p2 a)
  (s-eqn-nf-defaults #'(lambda (x) (etanorm (lambda-norm x))) p1 p2 a))

(defun s-eqn-lambda-nf (p1 p2 a)
  (s-eqn-nf #'(lambda (x) (etanorm (lambda-norm x)))
	    "LAMBDA-NF" "{Beta,Eta}*" p1 p2 a))

(defseqn long-eta-nf
  (s-eqn-argtypes line line gwff)
  (s-eqn-argnames p1 p2 a)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff to be normalized")
  (s-eqn-defaultfns s-eqn-long-eta-nf-defaults)
  (s-eqn-mainfns s-eqn-long-eta-nf)
  (s-eqn-closefns %prtlines)
  (mhelp "Compute the long-eta normal form of a line."))

(defun s-eqn-long-eta-nf-defaults (p1 p2 a)
  (s-eqn-nf-defaults #'long-eta p1 p2 a))

(defun s-eqn-long-eta-nf (p1 p2 a)
  (s-eqn-nf #'long-eta "LONG-ETA-NF" "Eta*" p1 p2 a))

(defun s-eqn-eq-defaults (fun p1 p2 a b)
  (declare (special strong-defaultlist))
  (unless (is-seqnder-p *current-eqnder*)
    (throwfail "There is no current rewriting derivation." t
	       "Use PROVE to start one."))
  (let ((topdown t))
    (unless (specified-p p1)
      (setq p1 (get-next-fplan))
      (unless p1
	(setq topdown nil)
	(setq-destruct ((p1 'ss))
		       (line-no-defaults-from
			(eval-destruct ((p1 'ss))))))
      (let ((lnum p1))
	(setq p1 (if (or (eq lnum '$) topdown) p1 (- p1 1))))))
  (setq a (if (not (specified-p p1)) '$
	    (if (or (specified-p a) (not (ln-exists p1)))
		a (linenum-assertion p1))))
  (setq p2 (if (specified-p p2) p2
	     (let ((ri (if ;(and (specified-p p1) (ln-exists p1))
			   (specified-p a)
			   (find-simple-instance
			    p1 #'(lambda (wff2)
				   (apply fun a (list wff2)))
			    t))))
	       (if ri ri (+ p1 1)))))
  (setq b (if (not (specified-p p2)) '$
	    (if (or (specified-p b) (not (ln-exists p2)))
		b (linenum-assertion p2))))
  (setq strong-defaultlist
	(append `(nil nil ,(specified-p a) ,(specified-p b))))
  (list p1 p2 a b))

(defun s-eqn-eq (fun cmdname rel-name just p1 p2 a b)
  (unless (apply fun a (list b))
    (throwfail cmdname " not applicable.
Line " p2 " is not " rel-name
               " to line " p1))
  (s-eqn-modify-plan just p1 p2 a b))

(defseqn beta-eq
  (s-eqn-argtypes line line gwff gwff)
  (s-eqn-argnames p1 p2 a b)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewiting")
  (s-eqn-defaultfns s-eqn-beta-eq-defaults)
  (s-eqn-mainfns s-eqn-beta-eq)
  (s-eqn-closefns %prtlines)
  (mhelp "Assert that two lines are beta-equivalent."))

(defun s-eqn-beta-eq-defaults (p1 p2 a b)
  (s-eqn-eq-defaults #'wffeq-ab-beta p1 p2 a b))

(defun s-eqn-beta-eq (p1 p2 a b)
  (s-eqn-eq #'wffeq-ab-beta "BETA-EQ" "beta-equivalent" "Beta*" p1 p2 a b))

(defseqn eta-eq
  (s-eqn-argtypes line line gwff gwff)
  (s-eqn-argnames p1 p2 a b)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewiting")
  (s-eqn-defaultfns s-eqn-eta-eq-defaults)
  (s-eqn-mainfns s-eqn-eta-eq)
  (s-eqn-closefns %prtlines)
  (mhelp "Assert that two lines are eta-equivalent."))

(defun s-eqn-eta-eq-defaults (p1 p2 a b)
  (s-eqn-eq-defaults #'wffeq-ab-eta p1 p2 a b))

(defun s-eqn-eta-eq (p1 p2 a b)
  (s-eqn-eq #'wffeq-ab-eta "ETA-EQ" "eta-equivalent" "Eta*" p1 p2 a b))

(defseqn lambda-eq
  (s-eqn-argtypes line line gwff gwff)
  (s-eqn-argnames p1 p2 a b)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewiting")
  (s-eqn-defaultfns s-eqn-lambda-eq-defaults)
  (s-eqn-mainfns s-eqn-lambda-eq)
  (s-eqn-closefns %prtlines)
  (mhelp "Assert that two lines are lambda-equivalent."))

(defun s-eqn-lambda-eq-defaults (p1 p2 a b)
  (s-eqn-eq-defaults #'wffeq-ab-lambda p1 p2 a b))

(defun s-eqn-lambda-eq (p1 p2 a b)
  (s-eqn-eq #'wffeq-ab-lambda "LAMBDA-EQ" "lambda-equivalent"
	    "{Beta,Eta}*" p1 p2 a b))

(context s-eqn-rules)

(defseqn app*
  (s-eqn-argtypes rrule line line gwff-or-nil gwff-or-nil)
  (s-eqn-argnames rule p1 p2 a b)
  (s-eqn-arghelp "Rule to apply" "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewriting")
  (s-eqn-defaultfns s-eqn-app*-defaults)
  (s-eqn-mainfns s-eqn-app*)
  (s-eqn-closefns %prtlines)
  (mhelp "Justify a line by a sequence of applications of a rewrite rule in
the forward direction, starting from a preceding line. In most cases, this
command will apply a rewrite rule in the forward direction as often as
possible or until a specified target wff is obtained. If the wff after
rewriting is specified but the one before rewriting is set to NIL, the rewrite
rule will be applied in the backward direction, starting from the target
formula. CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL."))

(defun s-eqn-app*-defaults (rule p1 p2 a b)
  (declare (special strong-defaultlist))
  (unless (or (not (specified-p rule))
	      (not (get *current-eqnder* 'theory))
	      (member (rrule-name rule)
		      ;(car (get (get *current-eqnder* 'theory) 'rrules))))
		      (rrules-used-by (get *current-eqnder* 'theory))))
    (throwfail (rrule-name rule) " not a member of "
	       (get *current-eqnder* 'theory)))
  (setq rule (instantiate-rrule rule))
  (unless (is-seqnder-p *current-eqnder*)
    (throwfail "There is no current rewriting derivation." t
	       "Use PROVE to start one."))
  (let ((topdown t))
    (if (not (specified-p p1))
	(progn
	  (setq p1 (get-next-fplan))
	  (unless p1
	    (setq topdown nil)
	    (setq-destruct ((p1 'ss))
			   (line-no-defaults-from
			    (eval-destruct ((p1 'ss))))))
	  (let ((lnum p1))
	    (setq p1 (if (or (eq lnum '$) topdown) p1
		       (let ((ri (if (specified-p rule)
				     (find-rewrite-instance
				      p1 rule (linenum-assertion p1)
				      topdown))))
			 (if ri ri (- p1 1)))))))))
  (setq p2 (if (specified-p p2) p2
	     (let ((ri (if (and (specified-p rule) (specified-p p1)
				(ln-exists p1))
			   (find-rewrite-instance
			    p1 rule (linenum-assertion p1) t))))
	       (if ri ri (+ p1 1)))))
  (unless (specified-p a)
    (setq a (if (or (not (specified-p p1)) (not (ln-exists p1)))
		nil
	      (linenum-assertion p1))))
  (unless (specified-p b)
    (setq b (if (or (not (specified-p p2)) (not (ln-exists p2)))
		nil
	      (linenum-assertion p2))))
  (setq strong-defaultlist `(nil nil nil ,a ,b))
  (list rule p1 p2 a b))

(defun s-eqn-app* (rule p1 p2 a b)
  (s-eqn-app*-generic rule t p1 p2 a b))

(defun s-eqn-app*-generic (rule direction p1 p2 a b)
  (unless (or direction (get rule 'bidirectional))
    (throwfail (rrule-name rule) " is not bidirectional"))
  (let* ((rule (instantiate-rrule rule))
	 (before (if direction (get rule 'before) (get rule 'after)))
	 (after (if direction (get rule 'after) (get rule 'before)))
	 (subtheory (subtheory-containing (rrule-name rule))))
    (labels ((app-loop (depth last a b)
	       (let ((oneflag nil))
		 (declare (special oneflag))
		 (if depth (setq depth (- depth 1)))
		 (cond ((equal depth -1) nil)
		       ((not b)
			(when (or (not last)
				  (instance-of-ruleapp rule last a subtheory)
				  (wffeq-ab last a))
			  (if (wffeq-ab last a) last
			    (app-loop depth a
				      (apply-rrule-poly-if
				       a
				       before
				       after
				       (get rule 'rewfn) (get rule 'appfn)
				       (get rule 'rtypelist)
				       #'(lambda (abbsym chkarg)
					   (declare (ignore abbsym chkarg)
						    (special oneflag))
					   (prog1 (not oneflag)
					     (setq oneflag t)))
				       nil
				       (theory-congruent-p subtheory))
				      b))))
		       ((not a)
			(when (or (not last) (instance-of-ruleapp
					      rule b last subtheory)
				  (wffeq-ab last b))
			  (if (wffeq-ab last b) last
			    (app-loop depth b a
				      (apply-rrule-poly-if
				       b
				       after
				       before
				       (get rule 'rewfn) (get rule 'appfn)
				       (get rule 'rtypelist)
				       #'(lambda (abbsym chkarg)
					   (declare (ignore abbsym chkarg)
						    (special oneflag))
					   (prog1 (not oneflag)
					     (setq oneflag t)))
				       nil
				       (theory-congruent-p subtheory))))))
		       ((wffeq-ab last a) nil)
		       ((wffeq-ab a b)
			(when (or (not last)
				  (instance-of-ruleapp rule last a subtheory))
			  b))
		       (t (when (or (not last)
				    (instance-of-ruleapp rule last a
							 subtheory))
			    (app-loop depth a
				      (apply-rrule-poly-if
				       a
				       before
				       after
				       (get rule 'rewfn) (get rule 'appfn)
				       (get rule 'rtypelist)
				       #'(lambda (abbsym chkarg)
					   (declare (ignore abbsym chkarg)
						    (special oneflag))
					   (prog1 (not oneflag)
					     (setq oneflag t)))
				       nil
				       (theory-congruent-p subtheory))
				      b)))))))
      (when (or a b)
	(let ((nf (app-loop app*-rewrite-depth nil a b)))
	  (unless nf (throwfail
		      "Found no justification by repeated application of "
		      (rrule-name rule) "."))
	  (if (not a) (setq a nf)
	    (if (not b) (setq b nf)))
	  (let ((new-lines (s-eqn-modify-plan (string-capitalize
					       (format nil "~A~A"
						       (rrule-name rule)
						       (if (wffeq-ab a b) "*"
							 "+")))
					      p1 p2 a b)))
	    (uninstantiate-rrule rule)
	    new-lines))))))

(defseqn unapp*
  (s-eqn-argtypes rrule line line gwff-or-nil gwff-or-nil)
  (s-eqn-argnames rule p1 p2 a b)
  (s-eqn-arghelp "Rule to apply" "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewriting")
  (s-eqn-defaultfns s-eqn-app*-defaults)
  (s-eqn-mainfns s-eqn-unapp*)
  (s-eqn-closefns %prtlines)
  (mhelp "Justify a line by a sequence of applications of a rewrite rule in
the backward direction, starting from a preceding line. In most cases, this
command will apply a rewrite rule in the backward direction as often as
possible or until a specified target wff is obtained. If the wff after
rewriting is specified but the one before rewriting is set to NIL, the rewrite
rule will be applied in the forward direction, starting from the target
formula. CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL."))

(defun s-eqn-unapp* (rule p1 p2 a b)
  (s-eqn-app*-generic rule nil p1 p2 a b))

(defseqn any*
  (s-eqn-argtypes line line gwff-or-nil gwff-or-nil)
  (s-eqn-argnames p1 p2 a b)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewriting")
  (s-eqn-defaultfns s-eqn-any*-defaults)
  (s-eqn-mainfns s-eqn-any*)
  (s-eqn-closefns %prtlines)
  (mhelp "Justify a line by a sequence of applications of any active
rewrite rules from the current theory in the forward direction, starting
from a preceding line. In most cases, this command will apply rewrite
rules in the forward direction as often as possible or until a specified
target wff is obtained. If the wff after rewriting is specified but the
one before rewriting is set to NIL, rewrite rules will be applied in the
backward direction, starting from the target formula.
CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL."))

(defseqn unany*
  (s-eqn-argtypes line line gwff-or-nil gwff-or-nil)
  (s-eqn-argnames p1 p2 a b)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewriting")
  (s-eqn-defaultfns s-eqn-any*-defaults)
  (s-eqn-mainfns s-eqn-unany*)
  (s-eqn-closefns %prtlines)
  (mhelp "Justify a line by a sequence of applications of any active
rewrite rules from the current theory in the backward direction, starting
from a preceding line. In most cases, this command will apply rewrite
rules in the backward direction as often as possible or until a specified
target wff is obtained. If the wff after rewriting is specified but the
one before rewriting is set to NIL, rewrite rules will be applied in the
forward direction, starting from the target formula.
CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL."))

(defseqn any*-in
  (s-eqn-argtypes theory line line gwff-or-nil gwff-or-nil)
  (s-eqn-argnames theory p1 p2 a b)
  (s-eqn-arghelp "Rewrite subtheory" "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewriting")
  (s-eqn-defaultfns s-eqn-any*-in-defaults)
  (s-eqn-mainfns s-eqn-any*-in)
  (s-eqn-closefns %prtlines)
  (mhelp "Justify a line by a sequence of applications of any active
rewrite rules from the specified subtheory of the current theory in the
forward direction, starting from a preceding line. In most cases, this
command will apply rewrite rules in the forward direction as often as
possible or until a specified target wff is obtained. If the wff after
rewriting is specified but the one before rewriting is set to NIL,
rewrite rules will be applied in the backward direction, starting from
the target formula.
CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL."))

(defseqn unany*-in
  (s-eqn-argtypes theory line line gwff-or-nil gwff-or-nil)
  (s-eqn-argnames theory p1 p2 a b)
  (s-eqn-arghelp "Rewrite subtheory" "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewriting")
  (s-eqn-defaultfns s-eqn-any*-in-defaults)
  (s-eqn-mainfns s-eqn-unany*-in)
  (s-eqn-closefns %prtlines)
  (mhelp "Justify a line by a sequence of applications of any active
rewrite rules from the specified subtheory of the current theory in the
backward direction, starting from a preceding line. In most cases, this
command will apply rewrite rules in the backward direction as often as
possible or until a specified target wff is obtained. If the wff after
rewriting is specified but the one before rewriting is set to NIL,
rewrite rules will be applied in the forward direction, starting from
the target formula.
CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL."))

(defun s-eqn-any*-defaults (p1 p2 a b)
  (s-eqn-any*-defaults-generic nil (get *current-eqnder* 'theory) p1 p2 a b))

(defun s-eqn-any*-in-defaults (theory p1 p2 a b)
  (s-eqn-any*-defaults-generic t theory p1 p2 a b))

(defun s-eqn-any*-defaults-generic (in-version theory p1 p2 a b)
  (declare (special strong-defaultlist))
  (unless (specified-p theory)
    (when (get *current-eqnder* 'theory)
      (setq theory (get *current-eqnder* 'theory))))
  (unless (or (not (specified-p theory))
	      (not (get *current-eqnder* 'theory))
	      (member theory (subtheories (get *current-eqnder* 'theory))))
    (throwfail theory " is not a subtheory of "
	       (get *current-eqnder* 'theory) "."))
  (let ((rules (remove-if
		#'(lambda (rule)
		    (and theory (specified-p theory)
			 (not (member rule (trules-used-by theory)))))
		(remove-if-not 'active-p global-rewrite-rule-list))))
    (unless rules
      (if (get *current-eqnder* 'theory)
	  (throwfail "No applicable rules from " theory
		     " are active.")
	(throwfail "No rewrite rules are active. Activate some rules before" t
		   "trying to apply ANY*(-IN)/UNANY*(-IN).")))
    (unless (is-seqnder-p *current-eqnder*)
      (throwfail "There is no current rewriting derivation." t
		 "Use PROVE to start one."))
    (let ((topdown t))
      (if (not (specified-p p1))
	  (progn
	    (setq p1 (get-next-fplan))
	    (unless p1
	      (setq topdown nil)
	      (setq-destruct ((p1 'ss))
			     (line-no-defaults-from
			      (eval-destruct ((p1 'ss))))))
	    (let ((lnum p1))
	      (setq p1 (if (or (eq lnum '$) topdown) p1 (- p1 1)))))))
		;       (let ((ri (if (specified-p rule)
		;		     (find-rewrite-instance
		;		      p1 rule (linenum-assertion p1)
		;		      topdown))))
		;	 (if ri ri (- p1 1)))))))))
    (setq p2 (if (specified-p p2) p2 (+ p1 1)))
	     ;(let ((ri (if (and (specified-p rule) (specified-p p1)
		;		(ln-exists p1))
		;	   (find-rewrite-instance
		;	    p1 rule (linenum-assertion p1) t))))
	       ;(if ri ri (+ p1 1)))))
    (unless (specified-p a)
      (setq a (if (or (not (specified-p p1)) (not (ln-exists p1)))
		  nil
		(linenum-assertion p1))))
    (unless (specified-p b)
      (setq b (if (or (not (specified-p p2)) (not (ln-exists p2)))
		  nil
		(linenum-assertion p2))))
    (cond (in-version (setq strong-defaultlist `(nil nil nil ,a ,b))
		      (list theory p1 p2 a b))
	  (t (setq strong-defaultlist `(nil nil ,a ,b))
	     (list p1 p2 a b)))))

(defun s-eqn-any* (p1 p2 a b)
  (s-eqn-any*-generic (get *current-eqnder* 'theory) t p1 p2 a b))

(defun s-eqn-unany* (p1 p2 a b)
  (s-eqn-any*-generic (get *current-eqnder* 'theory) nil p1 p2 a b))

(defun s-eqn-any*-in (theory p1 p2 a b)
  (s-eqn-any*-generic theory t p1 p2 a b))

(defun s-eqn-unany*-in (theory p1 p2 a b)
  (s-eqn-any*-generic theory nil p1 p2 a b))

(defun s-eqn-any*-generic (theory direction p1 p2 a b)
  (labels ((app-loop (depth last a b rule)
             (let* ((rule (instantiate-rrule rule))
		    (before (if direction (get rule 'before)
			      (get rule 'after)))
		    (after (if direction (get rule 'after)
			     (get rule 'before)))
		    (subtheory (subtheory-containing (rrule-name rule)
						     theory)))
	       (let ((oneflag nil))
		 (declare (special oneflag))
		 (if depth (setq depth (- depth 1)))
		 (cond ((equal depth -1) nil)
		       ((not b)
			(when (or (not last)
				  (instance-of-ruleapp rule last a subtheory)
				  (wffeq-ab last a))
			  (if (wffeq-ab last a) last
			    (app-loop depth a
				      (apply-rrule-poly-if
				       a
				       before
				       after
				       (get rule 'rewfn) (get rule 'appfn)
				       (get rule 'rtypelist)
				       #'(lambda (abbsym chkarg)
					   (declare (ignore abbsym chkarg)
						    (special oneflag))
					   (prog1 (not oneflag)
					     (setq oneflag t)))
				       nil
				       (theory-congruent-p subtheory))
				      b rule))))
		       ((not a)
			(when (or (not last)
				  (instance-of-ruleapp rule b last subtheory)
				  (wffeq-ab last b))
			  (if (wffeq-ab last b) last
			    (app-loop depth b a
				      (apply-rrule-poly-if
				       b
				       after
				       before
				       (get rule 'rewfn) (get rule 'appfn)
				       (get rule 'rtypelist)
				       #'(lambda (abbsym chkarg)
					   (declare (ignore abbsym chkarg)
						    (special oneflag))
					   (prog1 (not oneflag)
					     (setq oneflag t)))
				       nil
				       (theory-congruent-p subtheory))
				      rule))))
		       ((wffeq-ab last a) a)
		       ((wffeq-ab a b)
			(when (or (not last)
				  (instance-of-ruleapp rule last a subtheory))
			  b))
		       (t (when (or (not last)
				    (instance-of-ruleapp rule last a
							 subtheory))
			    (app-loop depth a
				      (apply-rrule-poly-if
				       a
				       before
				       after
				       (get rule 'rewfn) (get rule 'appfn)
				       (get rule 'rtypelist)
				       #'(lambda (abbsym chkarg)
					   (declare (ignore abbsym chkarg)
						    (special oneflag))
					   (prog1 (not oneflag)
					     (setq oneflag t)))
				       nil
				       (theory-congruent-p subtheory))
				      b rule)))))))
	   (any-loop (depth a b rules)
		     (if (null rules) (list a b (if depth (- depth 1) depth))
		       (let ((nf (app-loop depth nil a b (car rules))))
			 (uninstantiate-rrule (car rules))
			 (if nf
			     (if (not a) (any-loop ;(if depth (- depth 1)
					           depth ;)
						   a nf (cdr rules))
			       (any-loop ;(if depth (- depth 1)
				         depth ;)
					 nf b (cdr rules)))
			   (any-loop depth a b (cdr rules))))))
	   (main-loop (depth last-a last-b rules)
	     (let ((res (any-loop depth last-a last-b rules)))
	       (cond ((and a b (wffeq-ab (car res) b)) t)
		     ((and (wffeq-ab last-a (car res))
			   (wffeq-ab last-b (cadr res)))
		      (cond ((not a) (setq a (cadr res)))
			    ((not b) (setq b (car res)))
			    (t (throwfail "Found no justification."))))
		     ((equal (caddr res) 0)
		      (throwfail "Maximum rewrite depth reached.
Found no justification."))
		     (t (main-loop (caddr res) (car res) (cadr res) rules))))))
    (when (or a b)
      (let ((rules (remove-if
		    #'(lambda (rule)
			(or (not (or direction (get rule 'bidirectional)))
			    (and theory
				 (not (member rule (trules-used-by theory))))))
		    (remove-if-not 'active-p global-rewrite-rule-list))))
	(main-loop app*-rewrite-depth a b rules)
	(let ((new-lines (s-eqn-modify-plan
			  (string-capitalize
			   (format nil "~A~A"
				   (if theory theory 'any)
				   (if (wffeq-ab a b) "*" "+")))
			  p1 p2 a b)))
	  new-lines)))))

(defseqn same
  (s-eqn-argtypes line line gwff gwff)
  (s-eqn-argnames p1 p2 a b)
  (s-eqn-arghelp "Lower-numbered line" "Higher-numbered line" "Wff on line P1"
		 "Wff on line P2")
  (s-eqn-defaultfns s-eqn-same-defaults)
  (s-eqn-mainfns s-eqn-same)
  (s-eqn-closefns %prtlines)
  (mhelp "Use reflexivity of equality. The wffs A and B need to be identical
up to alphabetic change of bound variables."))

(defun s-eqn-same-defaults (p1 p2 a b)
  (declare (special strong-defaultlist))
  (unless (is-seqnder-p *current-eqnder*)
    (throwfail "There is no current rewriting derivation." t
	       "Use PROVE to start one."))
  (let ((topdown t))
    (unless (specified-p p1)
      (setq p1 (get-next-fplan))
      (unless p1
	(setq topdown nil)
	(setq-destruct ((p1 'ss))
		       (line-no-defaults-from
			(eval-destruct ((p1 'ss))))))
      (let ((lnum p1))
	(setq p1 (if (or (eq lnum '$) topdown) p1 (- p1 1))))))
  (setq p2 (if (specified-p p2) p2
	     (let ((ri (if (and (specified-p p1) (ln-exists p1))
			   (find-wff-instance
			    p1 (linenum-assertion p1) t))))
	       (if ri ri (+ p1 1)))))
  (unless (specified-p a)
    (setq a (if (or (not (specified-p p1)) (not (ln-exists p1)))
		(if (and (specified-p p2) (ln-exists p2))
		    (linenum-assertion p2)
		  a)
	      (linenum-assertion p1))))
  (unless (specified-p b)
    (setq b (if (or (not (specified-p p2)) (not (ln-exists p2)))
		(if (and (specified-p p1) (ln-exists p1))
		    (linenum-assertion p1)
		  b)
	      (linenum-assertion p2))))
  (setq strong-defaultlist `(nil nil ,(ln-exists p1) ,(ln-exists p2)))
  (list p1 p2 a b))

(defun s-eqn-same (p1 p2 a b)
  (if (not (wffeq-ab a b))
      (throwfail "SAME not applicable. Line " p2
		 " is not identical to line " p1)
    (s-eqn-modify-plan "Same as" p1 p2 a b)))

(defseqn auto
  (s-eqn-argtypes line line gwff gwff)
  (s-eqn-argnames p1 p2 a b)
  (s-eqn-arghelp "Line before rewriting  (lower-numbered)"
		 "Line after rewriting  (higher-numbered)"
		 "Wff before rewriting" "Wff after rewriting")
  (s-eqn-defaultfns s-eqn-auto-defaults)
  (s-eqn-mainfns s-eqn-auto)
  ;(s-eqn-closefns %prtlines)
  (mhelp "Search for a rewrite sequence between two lines using any active
rewrite rules from the current theory. The exact behaviour is affected by
following flags: REWRITING-AUTO-DEPTH, REWRITING-AUTO-TABLE-SIZE,
REWRITING-AUTO-MAX-WFF-SIZE, REWRITING-AUTO-SUBSTS"))

(defun s-eqn-auto-defaults (p1 p2 a b)
  (s-eqn-auto-defaults-generic nil (get *current-eqnder* 'theory) p1 p2 a b))

(defun s-eqn-auto-defaults-generic (in-version theory p1 p2 a b)
  (declare (special strong-defaultlist))
  (unless (specified-p theory)
    (when (get *current-eqnder* 'theory)
      (setq theory (get *current-eqnder* 'theory))))
  (unless (or (not (specified-p theory))
	      (not (get *current-eqnder* 'theory))
	      (member theory (subtheories (get *current-eqnder* 'theory))))
    (throwfail theory " is not a subtheory of "
	       (get *current-eqnder* 'theory) "."))
  (let ((rules (remove-if
		#'(lambda (rule)
		    (and theory (specified-p theory)
			 (not (member rule (rrules-used-by theory)))))
		(remove-if-not 'active-p global-rewrite-rule-list))))
    (unless rules
      (if (get *current-eqnder* 'theory)
	  (throwfail "No rules from " theory
		     " are active.")
	(throwfail "No rewrite rules are active. Activate some rules before" t
		   "trying to apply AUTO.")))
    (unless (is-seqnder-p *current-eqnder*)
      (throwfail "There is no current rewriting derivation." t
		 "Use PROVE to start one."))
    (let ((topdown t))
      (if (not (specified-p p1))
	  (progn
	    (setq p1 (get-next-fplan))
	    (unless p1
	      (setq topdown nil)
	      (setq-destruct ((p1 'ss))
			     (line-no-defaults-from
			      (eval-destruct ((p1 'ss))))))
	    (let ((lnum p1))
	      (setq p1 (if (or (eq lnum '$) topdown) p1 (- p1 1)))))))
    (unless (specified-p p2)
      (setq p2 ;(if (specified-p p2) p2 (+ p1 1)))
	    (let ((ri (if (and (specified-p p1) (ln-exists p1))
			  (find-possible-instance p1 t))))
	      (if ri ri (+ p1 1)))))
    (unless (specified-p a)
      (setq a (if (or (not (specified-p p1)) (not (ln-exists p1)))
		  a
		(linenum-assertion p1))))
    (unless (specified-p b)
      (setq b (if (or (not (specified-p p2)) (not (ln-exists p2)))
		  b
		(linenum-assertion p2))))
    (cond (in-version (setq strong-defaultlist `(nil nil nil
						 ,(specified-p a)
						 ,(specified-p b)))
		      (list theory p1 p2 a b))
	  (t (setq strong-defaultlist `(nil nil
					,(specified-p a) ,(specified-p b)))
	     (list p1 p2 a b)))))

;(defun s-eqn-auto2 (p1 p2 a b)
;  (let ((rules (remove-if
;		#'(lambda (rule)
;		    (and (get *current-eqnder* 'theory)
;			 (not (member rule
;				      (rrules-used-by
;				       (get *current-eqnder*
;					    'theory))))))
;		(remove-if-not 'active-p global-rewrite-rule-list))))
;    (mapcar #'(lambda (x) (format t "~&~A" x))
;	    (rewrite-search2 a b rules rewriting-auto-depth))))

(defun s-eqn-auto (p1 p2 a b)
  (let ((rules (remove-if
		#'(lambda (rule)
		    (and (get *current-eqnder* 'theory)
			 (not (member rule
				      (rrules-used-by
				       (get *current-eqnder*
					    'theory))))))
		(remove-if-not 'active-p global-rewrite-rule-list))))
    ;(mapcar #'(lambda (x) (format t "~&~A" x))
	;    (rewrite-search a b rules rewriting-auto-depth))
    (when (= 1 p2)
      (throwfail "Line " p1
		 " needs to be justified by a preceding line."))
    (format t "~&Search in progress. Please wait...~%")
    (time
    (let ((path (case rewriting-auto-search-type
		  ((simple) (rewrite-search a b rules rewriting-auto-depth))
		  ((bidir) (rewrite-search2 a b rules rewriting-auto-depth))
		  ((bidir-sorted)
		   (rewrite-search3 a b rules rewriting-auto-depth)))))
      (cond ((null path)
	     (complain "Found no derivation."))
	    ((equal path "hash")
	     (complain "The maximal search table size has been reached." t
		       "Search aborted."))
	    ((= (length path) 1) (s-eqn-modify-plan "Same as" p1 p2 a b))
	    (t (labels ((modify-plan (p1 a path)
			  (if (<= (length path) 2)
			      (s-eqn-modify-plan (string-capitalize
						  (princ-to-string (car path)))
						 p1 p2 a b)
			    (progn
			      (when (ln-exists (+ p1 1))
				(se-introduce-gap (linenum-line (+ p1 1)) 1)
				(setq p2 (+ p2 1)))
			      (if (= (+ p1 1) p2) (setq p2 (+ p2 1)))
			      (s-eqn-modify-plan (string-capitalize
						  (princ-to-string (car path)))
						 p1 (+ p1 1) a (cadr path))
			      (modify-plan (+ p1 1)
					   (cadr path) (cddr path))))))
		 (modify-plan p1 a (cdr path)))
	       (msgf "Success.")))))))

(context s-eqn-rearrange)

(defseqn connect
  (s-eqn-argtypes existing-line existing-line)
  (s-eqn-argnames p1 p2)
  (s-eqn-arghelp "Lower-numbered line" "Higher-numbered line")
  (s-eqn-defaultfns s-eqn-connect-defaults)
  (s-eqn-mainfns s-eqn-connect)
  (mhelp "Given two identical lines, delete the lower one, rearranging the
derivation appropriately. With symmetric relations, the command will also
rearrange the lines from which the higher-numbered line was obtained to
follow from the lower-numbered line."))

(defun s-eqn-connect-defaults (p1 p2)
  (unless (is-seqnder-p *current-eqnder*)
    (throwfail "There is no current rewriting derivation." t
	       "Use PROVE to start one."))
  (let ((topdown t))
    (unless (specified-p p1)
      (setq p1 (let ((fplan (get-next-fplan)))
		 (when fplan (linenum-line fplan))))
      (unless p1
	(setq topdown nil)
	(setq-destruct ((p1 'ss))
		       (line-no-defaults-from
			(eval-destruct ((p1 'ss)))))
	(setq p1 (if (ln-exists p1) (linenum-line p1) '$)))
      ;(let ((lnum p1))
	;(setq p1 (if (or (eq lnum '$) topdown) p1 (- p1 1))))
      ))
  (setq p2 (if (specified-p p2) p2
	     (let ((ri (if (specified-p p1)
			   (linenum-line
			    (find-any-wff-instance
			     (line-linenumber p1) (line-assertion p1) t)))))
	       (if ri ri '$))))
  (list p1 p2))

(defun lines-justified-by (line)
  (foldl #'(lambda (l ls)
	     (if (member line (line-just-lines l)) (cons l ls) ls))
	 nil (proof-lines (get *current-eqnder* 's-eqn-der))))

(defun s-eqn-justifying-lines (line)
  (let ((jlines (line-just-lines line)))
    (if jlines (append jlines
		       (apply #'concatenate 'list
			      (mapcar #'s-eqn-justifying-lines
				      jlines))))))

(defun justifying-subproof (line)
  (sort (cons line (s-eqn-justifying-lines line))
	#'(lambda (l1 l2) (< (line-linenumber l1) (line-linenumber l2)))))

(defun reverse-rewrite-sequence (lines)
  (let* ((eqnder (get *current-eqnder* 's-eqn-der))
	 (lnums (mapcar #'(lambda (l) (line-linenumber l)) lines))
	 (rev-lnalist (mapcar #'(lambda (n l)
				  (setf (line-linenumber l) n)
				  (cons n l))
			      lnums (reverse lines))))
    (foldl #'(lambda (l prev)
	       (setf (line-just-lines l) (list prev)) l)
	   (car (last lines))
	   (mapcar #'(lambda (l just)
		       (setf (line-justification l) just)
		       l)
		   (cdr (reverse lines))
		   (reverse (cdr (mapcar #'(lambda (l)
					     (line-justification l))
					 lines)))))
    (setf (proof-linealiases eqnder)
	  (mapcar #'(lambda (p)
		      (let ((new (assoc (car p) rev-lnalist :test #'equal)))
			(if new new p)))
		  (proof-linealiases eqnder)))
    (setf (proof-lines eqnder)
	  (sort (proof-lines eqnder)
		#'(lambda (l1 l2) (< (line-linenumber l1)
				     (line-linenumber l2)))))))

(defun s-eqn-connect (p1 p2)
  (unless (wffeq-ab (line-assertion p1) (line-assertion p2))
    (throwfail "CONNECT not applicable.
Line " (line-linenumber p1)
	       " is not identical to line " (line-linenumber p2) "."))
  (unless (< (line-linenumber p1) (line-linenumber p2))
    (throwfail "Line " (line-linenumber p1) " does not precede line "
	       (line-linenumber p2) "."))
  (when (eq p2 (get *current-eqnder* 'target-line))
    (throwfail "CONNECT not applicable since line " (line-linenumber p2)
	       " cannot be deleted."))
  (let ((p2-subproof (justifying-subproof p2)))
    (when (and (not (member p1 p2-subproof))
	       (< (line-linenumber (car p2-subproof))
		  (line-linenumber p1)))
      (throwfail "Line " (line-linenumber p1)
		 " does not precede the derivation of line "
		 (line-linenumber p2) "."))
    (unless (member p1 p2-subproof)
      (cond ((test-bidirectionality (car p2-subproof) p2)
	     (reverse-rewrite-sequence p2-subproof)
	     (setf (proof-plans (get *current-eqnder* 's-eqn-der))
		   (remove-if #'(lambda (plan)
				  (and (member (car plan) p2-subproof)
				       (not (eq (car plan)
						(get *current-eqnder*
						     'target-line)))))
			      (proof-plans (get *current-eqnder*
						's-eqn-der)))))
	    ((not (eq p2 (get *current-eqnder* 'target-line)))
	     (setf (proof-plans (get *current-eqnder* 's-eqn-der))
		   (remove p2 (proof-plans (get *current-eqnder*
						's-eqn-der)))))))
    (mapcar #'(lambda (l) (setf (line-just-lines l)
				(cons p1 (remove p2 (line-just-lines l)))))
	    (remove p2 (lines-justified-by p2)))
    (se-find-all-gaps)
    (s-eqn-delete (list p2))))

(context s-eqn-entering)

(defseqn done
  (s-eqn-argtypes existing-line)
  (s-eqn-argnames p1)
  (s-eqn-arghelp "Target line")
  (s-eqn-defaultfns s-eqn-done-defaults)
  (s-eqn-mainfns (lambda (p1)
		   (let ((done (s-eqn-done p1)))
		     (cond ((s-eqn-strict-p done) (msg "Derivation complete."))
			   (done (msg "The derivation does not seem to make any rewrite steps.
The relation to prove, however, is not reflexive."))
			   (t (complain "Line "
					(line-linenumber
					 (s-eqn-find-unjust-line p1))
					" still not justified."))))))
  (mhelp "Check whether the current derivation is complete. For rewriting
proofs, DONE checks whether the target line was obtained from the initial
line. In case of derivations without a target line, DONE prompts for a
line which is to be regarded as the target."))

(defun s-eqn-done-defaults (p1)
  (declare (special strong-defaultlist))
  (let ((target (get *current-eqnder* 'target-line))
	(next-fplan (get-next-fplan)))
    (setq p1 (if target (progn (setq strong-defaultlist '(t)) target)
	       (if next-fplan next-fplan '$))))
  (list p1))

(context s-eqn-theories)

(defseqn derive-rrule
  (s-eqn-argtypes existing-line existing-line symbol string typesymlist yesno)
  (s-eqn-argnames left right name help typelist bidir)
  (s-eqn-arghelp "Line with left-hand side" "Line with right-hand side"
		 "Name of Rule" "Help message" "List of polymorphic types"
		 "Make rule bidirectional?")
  (s-eqn-defaultfns s-eqn-derive-rrule-defaults)
  (s-eqn-mainfns s-eqn-derive-rrule)
  (mhelp "Create a derived rewrite rule from two provably related lines.
If the relation was proven using bidirectional rules only, the derived rule
may be made bidirectional."))

(defun test-bidirectionality (src trg)
  (let ((result
	 (when (and (<= (line-linenumber src) (line-linenumber trg))
		    (or (symbolp (line-just-rule trg))
			(member (line-just-rule trg)
				(list "Same as" "Beta*" "Eta*" "{Beta,Eta}*")
				:test #'equal)
			(and (member (string-right-trim "*+"
							(line-just-rule trg))
				     (cons "Any"
					   ;(string-capitalize
					   ; (princ-to-string
					   ;  (get *current-eqnder* 'theory)))
					   (mapcar
					    #'(lambda (th)
						(string-capitalize
						 (princ-to-string th)))
					    (subtheories
					     (get *current-eqnder* 'theory))))
				     :test #'equal)
			     (null (remove-if
				    #'(lambda (rr)
					(get rr 'bidirectional))
				    ;(if (get *current-eqnder* 'theory)
					;(rrules-used-by
					; (get *current-eqnder* 'theory))
				      ;global-rewrite-rule-list))))
				    (let ((th (find-symbol
					       (string-upcase
						(string-right-trim
						 "*+"
						 (line-just-rule trg))))))
				      (if (eq th 'any)
					  global-rewrite-rule-list
					(trules-used-by th))))))
			(let ((rule (find-symbol (string-upcase
						  (string-right-trim
						   "*+"
						   (line-just-rule trg))))))
			  (and (get rule 'rewrite-rule)
			       (get rule 'bidirectional)))))
	   (unless (eq src trg)
	     (when (car (line-just-lines trg))
	       (test-bidirectionality src (car (line-just-lines trg))))))))
    (or result (eq src trg))))

(defun s-eqn-get-subwffs (wff)
  (cond ((boundwff-p wff)
	 (remove-duplicates (cons (bdvar wff) (s-eqn-get-subwffs (gdr wff)))))
	((consp wff)
	 (remove-duplicates (append (s-eqn-get-subwffs (gar wff))
				    (s-eqn-get-subwffs (gdr wff)))))
	(t (list wff))))

(defun s-eqn-derive-rrule-defaults (left right name help typelist bidir)
  (declare (special strong-defaultlist))
  (unless (is-seqnder-p *current-eqnder*)
    (throwfail "There is no current rewriting derivation." t
	       "Use PROVE to start one."))
  (unless (or (specified-p left) (not (ln-exists 1)))
    (setq left (linenum-line 1)))
  (unless (specified-p right)
    (setq right (cond ((get *current-eqnder* 'target-line))
		      ((get *current-eqnder* 'last-plan))
		      (t '$))))
  (unless (specified-p help) (setq help ""))
  (unless (specified-p typelist)
    (when (and (specified-p left) (specified-p right))
      (setq typelist (find-type-vars
		      (mapcar #'type
			      (s-eqn-get-subwffs
			       (cons (line-assertion left)
				     (line-assertion right))))))))
  (unless (specified-p bidir)
    (cond ((if (<= (line-linenumber left) (line-linenumber right))
	       (test-bidirectionality left right)
	     (test-bidirectionality right left))
	   (setq bidir t)
	   (setq strong-defaultlist `(nil nil nil nil ,(not typelist) nil)))
	  (t (setq bidir nil)
	     (setq strong-defaultlist `(nil nil nil nil ,(not typelist) t)))))
  (list left right name help typelist bidir))

(defun s-eqn-derive-rrule (left right name help typelist bidir)
  (cond ((<= (line-linenumber left) (line-linenumber right))
	 (unless (s-eqn-strict-p (s-eqn-done-generic left right))
	   (throwfail "The asserted relation between line "
		      (line-linenumber left)
		      " and line " (line-linenumber right)
		      " has not yet been established."))
	 (when (and bidir (not (test-bidirectionality left right)))
	   (throwfail "Rule cannot be made bidirectional because its derivation
is not symmetric.")))
	(t (unless (s-eqn-strict-p (s-eqn-done-generic right left))
	     (throwfail "The asserted relation between line "
			(line-linenumber left)
			" and line " (line-linenumber right)
			" has not yet been established."))
	   (unless (test-bidirectionality right left)
	     (throwfail "Rule cannot be made applicable in the bottom-up
direction because the justifying derivation is not symmetric."))))
  (cond ((get name 'rewrite-rule)
	 (unless (and (wffeq-ab (get name 'before) (line-assertion left))
		      (wffeq-ab (get name 'after) (line-assertion right)))
	   (throwfail "The existing rewrite rule "
		      name
		      " is not alpha-equivalent
to the derived relation."))
	 (unless (eq (get name 'bidirectional) bidir)
	   (if bidir
	       (throwfail
		name " cannot be made bidirectional because it is already
loaded as a directed rule.")
	     (throwfail name " is already present as a bidirectional rule.
Therefore it cannot be added as a directed rule.")))
	 (unless (or (not (get *current-eqnder* 'theory))
		     (eq (get name 'appfn)
			 (get (get *current-eqnder* 'theory) 'derived-appfn)))
	   (throwfail
	    "The applicability test (appfn) for existing rule " name t
	    "differs from the corresponding test for rules derived in "
	    (get *current-eqnder* 'theory)))
	 (unless (or (not (get *current-eqnder* 'theory))
		     (eq (get name 'rewfn)
			 (get (get *current-eqnder* 'theory) 'derived-rewfn)))
	   (throwfail
	    "The auxiliary rewrite procedure (rewfn) for existing rule " name t
	    "differs from the corresponding procedure for rules derived in "
	    (get *current-eqnder* 'theory)))
	 (msg name " is already present. The list of theories in which the rule
is derivable is the only attribute which will be updated."))
	((create-rewrite-rule name (line-assertion left)
			      (line-assertion right) ;nil
			      (get (get *current-eqnder* 'theory)
				   'derived-rewfn)
			      nil bidir ;nil
			      (get (get *current-eqnder* 'theory)
				   'derived-appfn)
			      help)
	 (putprop name (remove-duplicates
			(append (free-vars-of (line-assertion left))
				(free-vars-of (line-assertion right))))
		  'variables)
	 (putprop name typelist 'rtypelist)))
  (let ((theory (get *current-eqnder* 'theory)))
    (when theory
      (putprop theory (remove-duplicates
		       (list (cons name (car (get theory 'rrules)))))
	       'rrules)
      (setf (get name 'derived-in) (remove-duplicates
				    (cons theory (get name 'derived-in)))))))

(defseqn save-rrule
  (s-eqn-argnames name)
  (s-eqn-argtypes symbol)
  (s-eqn-arghelp "Name of Rule")
  (s-eqn-mainfns save-rrule)
  (mhelp "Save a rewrite rule into the library."))

(defseqn make-rrule
  (s-eqn-argtypes symbol gwff gwff typesymlist yesno symbol symbol gvarlist
		  string)
  (s-eqn-argnames name gwff1 gwff2 types bidir appfn rewfn vars mhelp)
  (s-eqn-arghelp "Name of Rule" "Left-hand side" "Right-hand side"
		 "Polymorphic type vars" "Bidirectional rule?"
		 "Applicability test" "Rewriting function"
		 "Variables" "Help message")
  (s-eqn-defaultfns s-eqn-make-rrule-defaults)
  (s-eqn-mainfns s-eqn-make-rrule)
  (mhelp "Create a new rewrite rule with the given left and right
sides in memory."))

(defun s-eqn-make-rrule-defaults (n l r ty b a f v h)
  (declare (special strong-defaultlist))
  (unless (or (eq l '$) (eq r '$) (equal (type l) (type r)))
    (throwfail "The two sides are of different types."))
  (let ((tyvars (if (and (eq ty '$) (specified-p l) (specified-p r))
		    (find-type-vars (mapcar #'type
					    (s-eqn-get-subwffs (cons l r))))
		  ty))
	(vars (if (and (eq v '$) (specified-p l) (specified-p r))
		  (set-exclusive-or (free-vars-of l) (free-vars-of r))
		v)))
    (setq strong-defaultlist `(nil nil nil
			       ,(and (specified-p tyvars) (not tyvars))
			       nil nil nil
			       ,(and (specified-p vars) (not vars))
			       nil))
    (list n l r tyvars
	  (if (eq b '$) t b)
	  (if (eq a '$) (get *active-rewrite-theory* 'derived-appfn) a)
	  (if (eq f '$) (get *active-rewrite-theory* 'derived-rewfn) f)
	  vars (if (eq h '$) "" h))))

(defun s-eqn-make-rrule (name gwff1 gwff2 types bidir appfn func vars mhelp)
  (let ((rew (list (list 'mhelp mhelp) (list 'before gwff1)
		   (list 'bidirectional bidir)  (list 'after gwff2)
		   (list 'active t) (list 'rewfn func) (list 'appfn appfn)
		   (list 'rtypelist types) (list 'variables vars)
		   (list 'derived-in nil)))
	(redefine-rrule t))
    (when (rewrule-p name)
      (prompt-read
       redefine-rrule nil
       (msgf "The rule " name
	     " already exists in memory. " t
	     "Do you want to replace it by the new definition?")
       'yesno t
       ((? (msgf "T to replace the old definition of the rewrite rule")))))
    (when redefine-rrule
      (eval (nconc (list 'defrewrule name) rew))
      gwff1)))

(context s-eqn-printing)

(defseqn texproof
  (s-eqn-argtypes filespec yesno)
  (s-eqn-argnames filename timing)
  (s-eqn-arghelp "filename" "Recording time?")
  (s-eqn-defaultfns (lambda (filename timing)
                (list
                  (if (eq filename '$)
                      (namestring
                        (make-pathname%
                          :name (string-downcase
				 (symbol-name *current-eqnder*))
                          :type "tex"))
                    filename)
                  (if (eq timing '$) nil timing))))
  (s-eqn-mainfns (lambda (filename timing)
		   (let ((dproof (get *current-eqnder* 's-eqn-der)))
		     (declare (special dproof))
		     (texproof-generic filename timing *current-eqnder*))))
  (mhelp "Print the current proof into a tex file.
After leaving tps, run this .tex file through tex and print the resulting
file.

Many flags affect the output of texproof.
See: USE-INTERNAL-PRINT-MODE, TURNSTILE-INDENT-AUTO, TURNSTILE-INDENT,
LATEX-EMULATION, TEX-MIMIC-SCRIBE, PPWFFLAG, DISPLAYWFF, INFIX-NOTATION,
PAGELENGTH, PAGEWIDTH, TEX-BREAK-BEFORE-SYMBOLS, LOCALLEFTFLAG, SCOPE,
ALLSCOPEFLAG, USE-DOT, FIRST-ORDER-PRINT-MODE, FILLINEFLAG, ATOMVALFLAG."))

(context s-eqn-entering)

(defseqn saveproof
  (s-eqn-argtypes filespec)
  (s-eqn-argnames savefile)
  (s-eqn-arghelp "File in which to save proof")
  (s-eqn-defaultfns (lambda (savefile)
		      (list (if (eq savefile '$)
				(namestring
				 (make-pathname% :name (string-downcase
							(symbol-name
							 *current-eqnder*))
						 :type "rew"
						 :version :newest))
			      savefile))))
  (s-eqn-mainfns s-eqn-saveproof)
  (mhelp "Saves the current rewriting proof to the specified file in
a form in which it can be restored.  Use RESTOREPROOF to restore the proof.
Overwrites the file if it already exists."))

(defun s-eqn-saveproof (savefile)
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof))
    (when (pathnamep savefile) (setq savefile (namestring savefile)))
    (setq savefile
	  (merge-pathnames savefile
			   (make-pathname% :name (string dproof) 
					   :type "rew"
					   :version :newest)))
    (with-open-file (*standard-output* savefile
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)
      (write-saved-proof-real dproof "defsavedrew" ;*current-eqnder*))
			      `',(get *current-eqnder* 'theory)))
    (format t "File ~A written." (namestring savefile))))

(defseqn restoreproof
  (s-eqn-argtypes filespec)
  (s-eqn-argnames savefile)
  (s-eqn-arghelp "File in which proof resides")
  (s-eqn-defaultfns (lambda (savefile)
		      (list (if (eq savefile '$)
				(namestring
				 (make-pathname% :name (string-downcase
							(symbol-name
							 *current-eqnder*))
						 :type "rew"
						 :version :newest))
			      savefile))))
  (s-eqn-mainfns s-eqn-restoreproof)
  (mhelp "Reads a rewriting proof from a file created by SAVEPROOF
and makes it the current proof.  A security feature prevents the 
restoration of saved proofs which have been altered in any way.
Retrieve any definitions which are used in the proof and stored in the
library before restoring the proof. If you don't specify a directory,
it will first try your home directory and then all the directories 
listed in SOURCE-PATH."))

(defun s-eqn-restoreproof (file)
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof))
    (s-eqn-restoreproof-real file (cons nil source-path))))

(defun s-eqn-restoreproof-real (sav source-path)
  (let ((exp nil)
	(savefile nil))
    (setq savefile
	  (merge-pathnames sav
			   (make-pathname% :directory (car source-path)
					   :name (string dproof) 
					   :type "rew"
					   :version :newest)))
    (if (probe-file savefile)
	(progn 
	  (with-open-file (in savefile :direction :input
			      :if-does-not-exist :error)
			  (setq exp (read in)))
	  (if (and (consp exp) (eq (car exp) 'defsavedrew))
	      (eval exp)
	    (throwfail "File does not contain a saved proof.")))
      (if (cdr source-path) 
	  (s-eqn-restoreproof-real sav (cdr source-path))
	(throwfail "File " sav " does not exist; check SOURCE-PATH.")))))

(context s-eqn-rearrange)

(defseqn squeeze
  (s-eqn-mainfns (lambda ()
		   (let ((dproof (get *current-eqnder* 's-eqn-der)))
		     (declare (special dproof))
		     (remove-unnecessary-gaps))))
  (mhelp "Removes unnecessary gaps from the derivation."))

(defseqn cleanup
  (s-eqn-mainfns (lambda ()
		   (setf (line-justification (linenum-line 1))
			 '("Assert TRUTH" nil nil))
		   (let ((dproof (get *current-eqnder* 's-eqn-der)))
		     (declare (special dproof))
		     (cleanup))
		   (setf (line-justification (linenum-line 1))
			 '("" nil nil))))
  (mhelp "Deletes unnecessary lines from a derivation."))

(defseqn introduce-gap
  (s-eqn-argnames line num)
  (s-eqn-argtypes existing-line posinteger)
  (s-eqn-arghelp "Line where gap is to be introduced" "Increment")
  (s-eqn-defaultfns (lambda (&rest rest)
		      (mapcar #'(lambda (argdefault arg)
				  (if (eq arg '$) argdefault arg))
			      '($ 10) rest)))
  (s-eqn-mainfns (lambda (line num)
		   (let ((dproof (get *current-eqnder* 's-eqn-der)))
		     (declare (special dproof))
		     (when (= 1 (line-linenumber line))
		       (throwfail t "You are not allowed to move the initial line of the derivation."))
		     (introduce-gap line num))))
  (mhelp "Introduce a gap in an existing derivation."))

(defseqn move
  (s-eqn-argtypes existing-line line)
  (s-eqn-argnames old-line new-line)
  (s-eqn-arghelp "Present line number" "New line number")
  (s-eqn-mainfns (lambda (old-line new-line)
		   (let ((dproof (get *current-eqnder* 's-eqn-der)))
		     (declare (special dproof))
		     (when (= 1 (line-linenumber old-line))
		       (throwfail t "You are not allowed to move the initial line of the derivation."))
		     (move old-line new-line))))
  (mhelp "Renumber one particular line."))

(context s-eqn-entering)

(defseqn prooflist
  (s-eqn-mainfns s-eqn-prooflist)
  (mhelp "Print a list of all rewrite derivations currently in memory.
For proofs, the corresponding proof assertions are printed. For general
derivations, the corresponding initial lines are printed."))

(defun s-eqn-prooflist ()
  (dolist (x *seqnder-list*)
    (msgf x " : " ((if (proof-assertion (get x 's-eqn-der))
		       (proof-assertion (get x 's-eqn-der))
		     (line-assertion (car (proof-lines (get x 's-eqn-der)))))
		   . gwff)
	  (if (proof-assertion (get x 's-eqn-der))
	      "  (proof)" "  (derivation)"))))

(defseqn reconsider
  (s-eqn-argtypes symbol)
  (s-eqn-argnames prefix)
  (s-eqn-arghelp "Name of Derivation")
  (s-eqn-defaultfns
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '($) rest)))
  (s-eqn-mainfns (lambda (prefix)
		   (if (get prefix 's-eqn-der)
		       (progn
			 (setq *current-eqnder* prefix)
			 (setq rewriting-relation-symbol
			       (if (get (get prefix 'theory) 'relation-sign)
				   (get (get prefix 'theory) 'relation-sign)
				 '=)))
		     (throwfail t prefix
				" is not a known rewrite derivation."))))
  (mhelp ("Reconsider a derivation. The following derivations are in memory:"
	  t (l *seqnder-list*) t
	  "For more details, use the PROOFLIST command." t)))
