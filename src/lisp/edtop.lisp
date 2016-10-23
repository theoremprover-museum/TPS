;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-EDITOR)

;;;
;;; File: EDTOP
;;; Package: WFF-EDITOR
;;;
;;; defines the editor top-level as well as the ED command.
;;;

(part-of wff-editor)

(deffile edtop
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Contents define editor top-level and ED command."))

(context subtoplevels)

(deftoplevel ed-top
  (top-prompt-fn ed-top-prompt)
  (command-interpreter ed-command-interpreter)
  (print-* ed-print-*)
  (top-level-category edop)
  (top-level-ctree ed-command-ctree)
  (top-cmd-decode opdecode)
  (mhelp "The top level of the formula editor."))

;;;
;;; Only one MEXPR to invoke the editor.
;;;

(defmexpr ed
  (argnames edwff)
  (argtypes gwff)
  (defaultfns (lambda (gwff)
		(declare (special last-edwff))
		(if (and (eq gwff '$) last-edwff)
		    (list last-edwff) (list gwff))))
  (mainfns wff-ed)
  (mhelp "Enter the editor on a given wff.
Editor windows may be initialized, depending the values of the flags 
EDWIN-TOP, EDWIN-CURRENT, EDWIN-VPFORM. The flags BLANK-LINES-INSERTED and 
CHARSIZE determine the layout of these windows. The flags 
EDWIN-{CURRENT,TOP,VPFORM}-WIDTH and EDWIN-{CURRENT,TOP,VPFORM}-HEIGHT 
determine the intial size of these windows; they may be resized after they are
opened in the usual way.
WARNING: Since editing is non-destructive, nothing is done with the result
of the editing process!"))

(context editor-obj)

;;;
;;; The catgory edop is really the category of editor commands.  Any
;;; wffop may also be called directly, with EDWFF standing for the current
;;; wff in the editor.
;;;

(eval-when (load compile eval)
(defcategory edop
  (define defedop)
  (properties
   (alias single)
   (result-> singlefn)
   (edwff-argname single)
   (defaultfns multiplefns)
   (move-fn singlefn)
   (mhelp single))
  (global-list global-edoplist)
  (shadow t)
  (mhelp-line "editor command")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command 
       (format nil "@IndexEdop(~A)" (symbol-name item))
       (remove (get item 'edwff-argname) 
	       (get (get item 'alias) 'argnames))
       (or (cdr (assoc 'edop (get item 'mhelp)))
	   (cdr (assoc 'wffop (get (get item 'alias) 'mhelp)))))))
  (mhelp-fn edop-mhelp))
)


;;;
;;; Parameters and flags follow.
;;;
(defflag edppwfflag
  (flagtype boolean)
  (default nil)
  (subjects editor printing)
  (mhelp "If T, wffs are always pretty-printed in the formula editor."))

(defflag edprintdepth
  (flagtype integer+)
  (default 24)
  (subjects editor printing)
  (mhelp "The depth to which wffs are printed in the formula editor."))

(defflag edwin-top
  (flagtype boolean)
  (default t)
  (subjects editor printing)
  (mhelp "If T, the Top Edwff window is opened to display the entire
   wff being edited when the editor is started."))

(defflag edwin-current
  (flagtype boolean)
  (default t)
  (subjects editor printing)
  (mhelp "If T, the Current Edwff window is opened to display the current
   wff being edited when the editor is started."))

(defflag edwin-vpform
  (flagtype boolean)
  (default nil)
  (subjects editor printing)
  (mhelp "If T, the Current Vpform window is opened 
   to display the vpform of the current wff being edited 
   when the editor is started. This flag is ignored in ETPS, where
   the Vpform window is never opened."))

(defflag edwin-top-width
  (flagtype posinteger)
  (default 80)
  (subjects editor window-props)
  (mhelp "Controls the initial width of the Top Edwff window."))

(defflag edwin-current-width
  (flagtype posinteger)
  (default 80)
  (subjects editor window-props)
  (mhelp "Controls the initial width of the Current Edwff window."))

(defflag edwin-vpform-width
  (flagtype posinteger)
  (default 60)
  (subjects editor window-props)
  (mhelp "Controls the initial width of the Current Vpform window."))

(defflag edwin-top-height
  (flagtype posinteger)
  (default 3)
  (subjects editor window-props)
  (mhelp "Controls the initial height of the Top Edwff window."))

(defflag edwin-current-height
  (flagtype posinteger)
  (default 3)
  (subjects editor window-props)
  (mhelp "Controls the initial height of the Current Edwff window."))

(defflag edwin-vpform-height
  (flagtype posinteger)
  (default 30)
  (subjects editor window-props)
  (mhelp "Controls the initial height of the Current Vpform window."))

(context scribe-record)

(defflag printedtflag
  (flagtype boolean)
  (default nil)
  (subjects editor printing)
  (mhelp
   "If T, editor operations are recorded into open transcript files."))

(defflag printedtflag-slides
  (flagtype boolean)
  (default nil)
  (subjects editor printing)
  (mhelp
   "If T, editor operations are recorded in slides style. This flag has 
no effect unless PRINTEDTFLAG is T."))

(defflag printedtfile
  (flagtype filespec)
  (default "edt.mss")
  (subjects editor printing)
  (mhelp "The name of the file in which wffs are recorded."))

;(defvar printedtfile-preamble "@Use(Database 'TPSDoc:')
;@Libraryfile(TPSDocuments)
;")

(defvar *edwin-top-window* nil)
(defvar *edwin-current-window* nil)
(defvar *edwin-vpform-window* nil)
(defvar *edwin-top-process* nil)
(defvar *edwin-current-process* nil)
(defvar *edwin-vpform-process* nil)
(defvar *edwins-opened* nil)

(eval-when (load eval compile)
(defmode scribe-edwff
  (flag-settings
   (allscopeflag nil)
   (atomvalflag nil)
   (displaywff t)
   (first-order-print-mode nil)
   (flushleftflag nil)
   (leftmargin 0)
   (localleftflag nil)
   (ppwfflag t)
   (printdepth 0)
   (printtypes t)
   (rightmargin 70)
   (scope nil)
   (style scribe))
  (mhelp "Mode used for writing formulas from the editor."))
)

(eval-when (load eval compile)
(defflag printedtops
  (flagtype anything)
  (default always-true)
  (subjects editor printing)
  (mhelp "The function or name of the function which test whether the
result of a particular edop should be written to a file."))
)
;;;
;;;

(defvar edt-written-p nil)

;;(defvar printvpdflag nil) Already defined in AUTO 9/17/87 DAN

;*;(defmacro wrteds (&rest l)
;*;  `(wrts (if printedtflag (cons nil selectchannels) (list nil))
;*;	 ,@l))

(context editor-obj)

;;; ED-LEVEL keeps track of the number of recursive calls to the editor.

(defvar ed-level 0)

;;; ED-COMMAND-CTREE contains the command-ctree for editor commands

(defvar ed-command-ctree nil)

(defun initialize-ed-ctree ()
  (initialize-top-level-ctree 'ed-top))


;;;
;;; wff-ED is the main function to be called.  It returns the edited wff,
;;; or NIL if the editing was aborted with EXIT or QUIT.
;;;

(defvar last-edwff nil)
(defvar edwff)
(defvar cmdstack)
(defvar wffstack)
(defvar temp)

;;; Added strong-defaults to make opdecode more versatile 10/12/87 DAN

(defun wff-ed (edwff)
  (let ((displaywff t)
	(printdepth edprintdepth)
	(ppwfflag edppwfflag)
	(edwff edwff) ;; Why do we need this binding? (SI) 85-12-6
	(prev-wff edwff)
	(wffstack nil)
	(cmdstack nil)
	(strong-defaults 'strong-ed-defaults))
    (declare (special displaywff printdepth ppwfflag  prev-wff
		      wffstack cmdstack strong-defaults)) ;; last-edwff edwff
    (%catch% (progn (edtop) (setq last-edwff edwff)
		    (throwfail "Editor aborted: no wff supplied."))
	     (exit-inferior-top (setq last-edwff edwff)
				last-edwff))))

(defgwff-type edwff-type
  (checkfn edwff-ckfn)
  (getfn edwff-getfn)
  (mhelp "edwff : The editor's name for the wff being edited."))

(defun edwff-ckfn (xxx)
  (eq xxx 'edwff))

(defun edwff-getfn (xxx)
  (declare (ignore xxx) (special edwff))
  (cond ((and (boundp 'edwff) edwff) edwff)
	(T (throwfail "Can't use edwff outside the editor. "))))

(defgwff-type last-edwff-type
  (checkfn last-edwff-ckfn)
  (getfn last-edwff-getfn)
  (mhelp "last-edwff : The name for the last edited wff when outside the editor."))

(defun last-edwff-ckfn (xxx)
  (eq xxx 'last-edwff))

(defun last-edwff-getfn (xxx)
  (declare (ignore xxx) (special last-edwff))
  (cond ((and (boundp 'last-edwff) last-edwff) last-edwff)
	(t (throwfail "You haven't used the editor yet!"))))


(defgwff-type edit-wff
  (checkfn edit-wff-p)
  (getfn get-edit-wff)
  (mhelp "A specification of the form (ED gwff) to edit gwff."))

(defun edit-wff-p (xxx)
  (and (consp xxx) (eq (car xxx) 'ed)))

(defun get-edit-wff (xxx)
  (wff-ed (getrwff (cadr xxx))))

;;;
;;; EDTOP is the editor top-level, locally binding all the important special
;;; variables.
;;;

(defun edtop ()
  (when (null *edwins-opened*) ; only one set of windows at a time!
    (setq *edwin-top-window* (setup-edwin-top-window))
    (setq *edwin-current-window* (setup-edwin-current-window))
    (setq *edwin-vpform-window* (setup-edwin-vpform-window)))
  (unwind-protect
      (let ((top-prompt-fn #'ed-top-prompt)
	    (ed-level (+ ed-level 1))
	    (edt-written-p nil)
	    (printedtflag (if (= ed-level 0) printedtflag nil))
	    (command-interpreter #'ed-command-interpreter)
	    (print-* #'ed-print-*)
	    (top-level 'ed-top)
	    (command-ctree ed-command-ctree))
	(declare (special top-prompt-fn ed-level command-interpreter
			  print-* top-level command-ctree edt-written-p
			  printedtflag))
	(secondary-top))))

(defun kill-edwin-windows ()
  (declare (special ed-level *using-interface*))
  (when (= ed-level 1)         ; we only close the windows if there are no more formulae being edited.
      (when (streamp *edwin-top-window*) 
	(if *using-interface*
	    (progn
	      (close-window *edwin-top-window*)
	      (setq *edwin-top-window* nil))
	  (progn (close *edwin-top-window*)  
		 (if (probe-file (pathname *edwin-top-window*)) (delete-file (pathname *edwin-top-window*)))
		 (setq *edwin-top-window* (kill-xterm-window *edwin-top-process*)))))
      (when (streamp *edwin-current-window*) 
	(if *using-interface*
	    (progn
	      (close-window *edwin-current-window*)
	      (setq *edwin-current-window* nil))
	  (progn (close *edwin-current-window*)  
		 (if (probe-file (pathname *edwin-current-window*))  (delete-file (pathname *edwin-current-window*)))
		 (setq *edwin-current-window* (kill-xterm-window *edwin-current-process*)))))
      (when (streamp *edwin-vpform-window*) 
	(if *using-interface*
	    (progn
	      (close-window *edwin-vpform-window*)
	      (setq *edwin-vpform-window* nil))
	  (progn (close *edwin-vpform-window*)  
		 (if (probe-file (pathname *edwin-vpform-window*))  (delete-file (pathname *edwin-vpform-window*)))
		 (setq *edwin-vpform-window* (kill-xterm-window *edwin-vpform-process*)))))
      (setq *edwins-opened* nil))
  nil)

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun ed-top-prompt (id)
  (declare (special ed-level edt-written-p))
  (edwin-update ) ; put here rather than in edtop so that windows are updated when moving within
		  ; recursively nested ED top levels.
  (let ((x (cond ((= ed-level 1) "") (T ed-level))))
    (format nil "<~[+~;-~;~;~]~:[~A:~;~A~]Ed~A>"
	    (cond (edt-written-p 0) (printedtflag 1) (t 2))
	    (= ed-level 1) x id)))

(eval-when (load compile eval)
(defmacro multiple-prin1 (cmd)
  `(dolist (elt (multiple-value-list ,cmd))
     (fresh-line) (prin1 elt)))
)
;;;
;;; ED-COMMAND-INTERPRETER returns a form (PROGN form1 ... formn),
;;; where form1 ... formn are all forms which can be evaluated directly
;;; by LISP.
;;;

(defun ed-command-interpreter (cmd)
  (do ((cmdlist nil (cons nextcmd cmdlist))
       (cmd cmd rest)
       (command (car cmd) (car rest))
       (rest (cdr cmd) (cdr rest))
       (nextcmd nil))
      ((null cmd)
       `(progn (setq edt-written-p nil) ,@(nreverse cmdlist)))
    (declare (special rest))
    (setq nextcmd
	  (cond ((and (integerp command) (zerop command))
		 `(move-up))
		((consp command) (consp-interpreter command))
		((and (symbolp command)
                       (or (get command 'edop) (get command 'wffop)))
		 (cond ((get command 'move-fn)
			`(%catch%
			  (progn
			   (setq temp
				 (funcall ',(get command 'alias) edwff))
			   (push edwff wffstack)
			   (push ',(get command 'move-fn) cmdstack)
			   (setq edwff temp))
				  (fail (complain expand-catch-throw t)
					(complain "Operation " ',command
						  " not performed."))))
		       ((and (get command 'edop)
			     (getkey command (get command 'alias) 'print-op))
			`(funcall ',(get command 'alias) edwff))
		       (t (let* ((no-args (length (getkey command
							  (get command 'alias)
							  'argtypes)))
				 (more-p (> (length rest) no-args))
				 (new-rest (if more-p (nthcdr no-args rest)
					       nil))
				 (args (if more-p (ldiff rest new-rest)
					   rest)))
			    (setq rest new-rest)
			    `(%catch%
			      (progn
			       (setq temp
				     (opdecode ',(cons command args)))
			       ,(cond ((eq (get command 'result->) 'edwff)
				       `(progn
					 (setq edwff temp)
					 (write-edwff edwff ',command)))
				      ((or (eq (get command 'result->)
					       'execute)
					   (eq 'ed-command
					       (getkey command
						       (get command 'alias)
						       'resulttype)))
				       `(eval (ed-command-interpreter temp)))
				      ((eq (get command 'result->) 'ignore)
				       `nil)
				      ((get command 'result->) 
				       `(funcall ',(get command 'result->)
						 temp))
				      ((get
					(getkey command (get command 'alias)
						'resulttype)
					'printfn)
				       `(funcall
					 ',(get (getkey command
							(get command 'alias)
							'resulttype)
						'printfn)
					 temp))))
			      (fail (complain expand-catch-throw t)
				    (complain "Operation not performed.")))))))
		((and (symbolp command)
                      (or (get command 'mexpr) (get command 'reviewcmd)))
                 (mexpr-interpreter command))
		((and (symbolp command) (get command 'flag))
                      (flag-interpreter command))
                (t (misc-interpreter command))))))

#+comment
(defun ed-command-interpreter (cmd)
  (do ((cmdlist nil (cons nextcmd cmdlist))
       (cmd cmd rest)
       (command (car cmd) (car rest))
       (rest (cdr cmd) (cdr rest))
       (nextcmd nil))
      ((null cmd)
       `(progn (setq edt-written-p nil) ,@(nreverse cmdlist)))
    (setq nextcmd
	  (cond ((and (integerp command) (zerop command))
		 `(move-up))
		((consp command)
		 (let ((carcmd (car command)))
		   (cond ((null expertflag)
			  (throwfail "Unknown Command or Flag."))
			 ((and (symbolp carcmd)
			       (not (fboundp carcmd)))
			  (setq rest nil)
			  `(complain ',carcmd ": undefined function.  "))
			 (t `(multiple-prin1 ,command)))))
		((not (symbolp command))
		 (if expertflag
		     `(multiple-prin1 ,command)
		   (throwfail "Unknown Command or Flag.")))
		((or (get command 'edop) (get command 'wffop))
		 (cond ((get command 'move-fn)
			`(%catch%
			  (progn
			   (setq temp
				 (funcall ',(get command 'alias) edwff))
			   (push edwff wffstack)
			   (push ',(get command 'move-fn) cmdstack)
			   (setq edwff temp))
				  (fail (complain expand-catch-throw t)
					(complain "Operation " ',command
						  " not performed."))))
		       ((and (get command 'edop)
			     (getkey command (get command 'alias) 'print-op))
			`(funcall ',(get command 'alias) edwff))
		       (t (let* ((no-args (length (getkey command
							  (get command 'alias)
							  'argtypes)))
				 (more-p (> (length rest) no-args))
				 (new-rest (if more-p (nthcdr no-args rest)
					       nil))
				 (args (if more-p (ldiff rest new-rest)
					   rest)))
			    (setq rest new-rest)
			    `(%catch%
			      (progn
			       (setq temp
				     (opdecode ',(cons command args)))
			       ,(cond ((eq (get command 'result->) 'edwff)
				       `(progn
					 (setq edwff temp)
					 (write-edwff edwff ',command)))
				      ((or (eq (get command 'result->)
					       'execute)
					   (eq 'ed-command
					       (getkey command
						       (get command 'alias)
						       'resulttype)))
				       `(eval (ed-command-interpreter temp)))
				      ((eq (get command 'result->) 'ignore)
				       `nil)
				      ((get command 'result->) 
				       `(funcall ',(get command 'result->)
						 temp))
				      ((get
					(getkey command (get command 'alias)
						'resulttype)
					'printfn)
				       `(funcall
					 ',(get (getkey command
							(get command 'alias)
							'resulttype)
						'printfn)
					 temp))))
			      (fail (complain expand-catch-throw t)
				    (complain "Operation not performed.")))))))
		((or (get command 'mexpr) (get command 'reviewcmd))
		 (let* ((no-args (length (get command 'argtypes)))
			(more-p (> (length rest) no-args))
			(new-rest (if more-p (nthcdr no-args rest)
				      nil))
			(args (if more-p (ldiff rest new-rest)
				  rest)))
		   (setq rest new-rest)
		   `(%catch% (comdecode ',(cons command args))
			     (fail (complain "Error in Top-Level command "
					     ',command "."  
                                             expand-catch-throw)))))
		((get command 'flag)
		 (let* ((more-p (> (length rest) 1))
			(new-rest (if more-p (nthcdr 1 rest)
				      nil))
			(args (if more-p (ldiff rest new-rest)
				  rest)))
		   (setq rest new-rest)
		   `(%catch% ,(if args `(comdecode '(set ,command ,@args))
				  `(comdecode '(setflag ,command)))
			     (fail (complain "Error while stting flag "
					     ',command "."  
                                             expand-catch-throw)))))
		((null expertflag)
		 (throwfail "Unknown Command or Flag."))
		((and (boundp command) (null rest))
		 `(prin1 ,command))
		(t
		 (setq rest nil)
		 (cond ((not (fboundp command))
			`(complain ',command
				   ": undefined variable or function.  "))
		       (t `(multiple-prin1 ,cmd))))))))

;;;
;;; ED-PRINT-* print the current wff, if it is not the same as before
;;; the last command.
;;;

(defun ed-print-* (result)
  (declare (special edwff prev-wff last-edwff)
	   (ignore result))
  (when (not (eq edwff prev-wff))
	(edwin-update )
	(prtwff edwff (displaywff nil) (leftmargin (curpos)))
	(setq last-edwff edwff)
	(setq prev-wff edwff)))



(defun edwin-update ()
  (declare (special temp cmdstack wffstack *using-interface*))
	  (when (and (streamp *edwin-current-window*) (open-stream-p *edwin-current-window*))
		(let ((*standard-output* *edwin-current-window*)
		      (style (if use-window-style window-style style)))
		  (if *using-interface*
		      (clear-window *edwin-current-window*)
		    (dotimes (i blank-lines-inserted) (terpri)))
		  (prtwff edwff (displaywff nil) (leftmargin (curpos))) (terpri)
		  (finish-output *standard-output*)))
	  (when (and (streamp *edwin-top-window*) (open-stream-p *edwin-top-window*))
		(let ((*standard-output* *edwin-top-window*)
		      (style (if use-window-style window-style style)))
		  (if *using-interface*
		      (clear-window *edwin-top-window*)
		    (dotimes (i blank-lines-inserted) (terpri)))
		  (prtwff (edwin-find-top edwff cmdstack wffstack) (displaywff nil) 
			                                        (leftmargin (curpos))) (terpri)
		  (finish-output *standard-output*)))
	  (when (and (streamp *edwin-vpform-window*) (open-stream-p *edwin-vpform-window*))
		(let ((*standard-output* *edwin-vpform-window*)
		      (style (if use-window-style window-style style)))
		  (if *using-interface*
		      (clear-window *edwin-vpform-window*)
		    (progn
		      (dotimes (i blank-lines-inserted) (terpri))
		      (dotimes (i blank-lines-inserted) (terpri))))  ;; this window is tall, so we do this twice
		  (auto:display-vp-diag edwff) (terpri)
		  (finish-output *standard-output*)))
	  t)


(defun edwin-find-top (edwffa cmdstacka wffstacka)
  (declare (special tempwff))
  (do ((tempwff edwffa (funcall (pop cmdstacka) (pop wffstacka) tempwff)))
      ((or (null cmdstacka) (null wffstacka))
       tempwff)))
;;;
;;; Following are some EDOPS which allow one to exit or move backwards
;;; through the edited formulas.
;;;

(context scribe-record)

(defedop o
  (alias invert-printedtflag)
  (mhelp "Invert PRINTEDTFLAG, that is switch automatic recording of wffs
in a file either on or off.  When switching on, the current wff will be
written to the PRINTEDTFILE. Notice that the resulting file will be in 
Scribe format; if you want something you can reload into TPS, then use
the SAVE command."))

(defun invert-printedtflag ()
  (declare (special edwff))
  (when (setq printedtflag (not printedtflag))
	(write-edwff edwff nil)))

(defedop rem
  (alias remark-printedtfile)
  (result-> ignore))

(defwffop remark-printedtfile
  (argnames rm)
  (argtypes string)
  (arghelp "Remark")
  (mhelp "Write a remark into the PRINTEDTFILE."))


(context subtoplevels)

(defedop ok
  (alias move-exit)
  (mhelp "Exit the editor with all the changes in place."))

(defedop leave
  (alias move-exit)
  (mhelp "Exit the editor with all the changes in place."))

(defedop noop
  (alias ednoop)
  (mhelp "Do nothing."))

(defun ednoop () t)


;;;
;;; MOVE-UP undoes the last moving command like A, D, L, R, or SHALLOW, DEEP,
;;; making the changes in the enclosing wff.
;;;

(defun move-up ()
  (declare (special edwff cmdstack wffstack))
  (if (or (null cmdstack) (null wffstack))
      (throwfail "You are at the top.")
      (setq edwff (funcall (pop cmdstack) (pop wffstack) edwff))))

;;;
;;; MOVE-EXIT is achieved by the OK command and moves up first, then
;;; returns from the editor top-level.
;;;


(defun move-exit ()
  (kill-edwin-windows)
  (%throw% '|[Normal Exit of Editor.]| exit-inferior-top))

;;;
;;; ABORT-EXIT quits the editor top-level, signalling an error.  Usually
;;; this should be done with a throw of 'QUIT-INFERIOR-TOP label, but here
;;; it is FAIL, since the editor is often used in command arguments.
;;;

(defun abort-exit ()
  (kill-edwin-windows)
  (%throw% '|[Editor aborted.]| quit-inferior-top))

(defwffrec edsearch
  (argnames gwff condfn))

(defun edsearch (gwff condfn)
  (cond ((funcall condfn gwff) (list 'noop))
	((label-q gwff) (apply-label gwff (edsearch gwff condfn)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff)
	 (let ((resx (edsearch (cdr gwff) condfn)))
	   (if resx (cons 'd resx) nil)))
	(t (let ((resx (edsearch (car gwff) condfn)))
	     (if resx (cons 'a resx)
		 (progn
		  (setq resx (edsearch (cdr gwff) condfn))
		  (if resx (cons 'd resx) nil)))))))

;;; Rest of file used to be in EDTOP-3  10/12/87 DAN

;;; printvpdflag may not be bound if vpforms not loaded 9/17/87 DAN

(defvar auto:printvpdflag)
(defvar auto:vpd-filename)

(defun remark-printedtfile (rm)
  (declare (special scribe-preamble))
  (when (not printedtflag)
    (ttymsg "Not recording at the moment.  Writing remark anyway."))
  (reroute-output-append printedtfile (make-pathname% :name "edt" :type "mss")
    (if (not existent-file)
	(if printedtflag-slides (msg slides-preamble t)
	    (msg scribe-preamble t))) ; was printedtfile-preamble
    (msg T rm T))
  (when (and (boundp 'auto:printvpdflag) auto:printvpdflag)
    (reroute-output-append auto:vpd-filename (make-pathname% :type "vpf")
      (msg T rm T))))

(defun write-edwff (edwff edop)
  (declare (special edt-written-p scribe-preamble))
  (let ((reroute-close-message nil))
    (declare (special reroute-close-message))
    (when (and printedtflag (or (null edop) (funcall printedtops edop)))
      (in-mode scribe-edwff
	(let ((default (make-pathname% :name "edt" :type "mss"))
	      (rightmargin (if printedtflag-slides slides-width
			       rightmargin)))
	  (reroute-output-append printedtfile default
	    (if (not existent-file)
		(if printedtflag-slides (msg slides-preamble)
		    (msg scribe-preamble))) ; was printedtfile-preamble, but this doesn't work any more MB94.
	    (msg t) (prtwff edwff) (msg t))))
      (if (and (boundp auto:printvpdflag)
	       auto:printvpdflag)
	  (auto:display-vpd edwff))
      (setq edt-written-p t))))


(defun setup-edwin-top-window ()
  (declare (special *using-interface*))
  (if edwin-top
      (if *using-interface*
	  (open-window-with-socket "EDWIN-TOP" "Editor Window Top"
				   edwin-top-width edwin-top-height
				   (equal charsize 'MAX))
	(if (equal charsize 'MAX)
	    (let* ((outfilename (concatenate 'string "/tmp/foo" 
					     (princ-to-string (tps-get-internal-run-time))))
		   (outstream (open outfilename :direction :output 
				    :if-exists :overwrite 
				    :if-does-not-exist :create)))
	      (setq *edwin-top-process* 
		    (setup-big-xterm-window "Top Edwff" outfilename 
					    (concatenate 'string (princ-to-string edwin-top-width) "x" 
							 (princ-to-string edwin-top-height) "+0+0")))
	      (setq *edwins-opened* t)
	      outstream)
	  (let* ((outfilename (concatenate 'string "/tmp/foo" 
					   (princ-to-string (tps-get-internal-run-time))))
		 (outstream (open outfilename :direction :output 
				  :if-exists :overwrite 
				  :if-does-not-exist :create)))
	    (setq *edwin-top-process* 
		  (setup-xterm-window "Top Edwff" outfilename 
				      (concatenate 'string (princ-to-string edwin-top-width) "x" 
						   (princ-to-string edwin-top-height) "+0+0")))
	    (setq *edwins-opened* t)
	    outstream)))
    nil))
  
(defun setup-edwin-current-window ()
  (declare (special *using-interface*))
  (if edwin-current
      (if *using-interface*
	  (open-window-with-socket "EDWIN-CURRENT" "Editor Window Current"
				   edwin-current-width edwin-current-height
				   (equal charsize 'MAX))
	(if (equal charsize 'MAX)
	    (let* ((outfilename (concatenate 'string "/tmp/foo" 
					     (princ-to-string (* 3 (get-universal-time)))))
		   (outstream (open outfilename :direction :output 
				    :if-exists :overwrite 
				    :if-does-not-exist :create)))
	      (setq *edwin-current-process* 
		    (setup-big-xterm-window "Current Edwff" outfilename 
					    (concatenate 'string (princ-to-string edwin-current-width) "x"
							 (princ-to-string edwin-current-height) "+0+100")))
	      (setq *edwins-opened* t)
	      outstream)
	  (let* ((outfilename (concatenate 'string "/tmp/foo" 
					   (princ-to-string (* 3 (get-universal-time)))))
		 (outstream (open outfilename :direction :output 
				  :if-exists :overwrite 
				  :if-does-not-exist :create)))
	    (setq *edwin-current-process* 
		  (setup-xterm-window "Current Edwff" outfilename 
				      (concatenate 'string (princ-to-string edwin-current-width) "x"
						   (princ-to-string edwin-current-height) "+0+100")))
	    (setq *edwins-opened* t)
	    outstream)))
    nil))

(defun setup-edwin-vpform-window ()
  (declare (special *using-interface*))
  (if (and edwin-vpform (member (find-package 'AUTO) (package-use-list *package*))) ; ie if AUTO is loaded...
      (if *using-interface*
	  (open-window-with-socket "EDWIN-VPFORM" "Editor Window VPForm"
				   edwin-vpform-width edwin-vpform-height
				   (equal charsize 'MAX))
	(if (equal charsize 'MAX)
	    (let* ((outfilename (concatenate 'string "/tmp/foo" 
					     (princ-to-string (* 5 (tps-get-internal-run-time)))))
		   (outstream (open outfilename :direction :output 
				    :if-exists :overwrite 
				    :if-does-not-exist :create)))
	      (setq *edwin-vpform-process* 
		    (setup-big-xterm-window "Current Vpform" outfilename 
					    (concatenate 'string (princ-to-string edwin-vpform-width) "x" 
							 (princ-to-string edwin-vpform-height) "+0+200")))
	      (setq *edwins-opened* t)
	      outstream)
	  (let* ((outfilename (concatenate 'string "/tmp/foo" 
					   (princ-to-string (* 5 (tps-get-internal-run-time)))))
		 (outstream (open outfilename :direction :output 
				  :if-exists :overwrite 
				  :if-does-not-exist :create)))
	    (setq *edwin-vpform-process* 
		  (setup-xterm-window "Current Vpform" outfilename 
				      (concatenate 'string (princ-to-string edwin-vpform-width) "x" 
						   (princ-to-string edwin-vpform-height) "+0+200")))
	    (setq *edwins-opened* t)
	    outstream)))
    nil))








