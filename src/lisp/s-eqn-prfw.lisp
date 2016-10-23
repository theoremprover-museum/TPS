;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of S-EQN)


(deffile s-prfw
  (part-of S-EQN)
  (extension lisp)
  (mhelp "Proofwindow support for the REWRITING toplevel."))

(context subtoplevels)

(deftoplevel s-prfw-top
  (top-prompt-fn s-prfw-prompt)
  (command-interpreter interpret-s-prfw)
  (print-* print-*-s-eqn-prwf)
  (top-level-category seqncmd)
  (top-cmd-decode s-prfw-opdecode)
  (mhelp "The REWRITING top level, supplemented by proofwindow output."))

(defun s-prfw-prompt (id)
  (format nil "<REW-PRFW~A>" id))

; Commands/Rules listed here cause the prfw to be updated
; when they are executed/applied
(defvar *s-prfw-iftrue-update-commands*
  (list 'app 'pall 'prove 'delete 'introduce-gap 'prove-in 'derive 'derive-in
	'move 'squeeze 'pstatus 'beta-nf 'app* 'unapp* 'cleanup 'same 'any
	'connect 'beta-eq 'eta-nf 'eta-eq 'lambda-nf 'lambda-eq 'long-eta-nf
	'restoreproof 'reconsider 'any* 'unany* 'auto 'any*-in 'unany*-in)
  "Commands after which the proofwindows should be updated, if the result returned is T")

;(defvar *prfw-pall-window* nil) ; will be the name of the stream for pall
;(defvar *prfw-^p-window* nil)
;(defvar *prfw-^pn-window* nil)
;(defvar *prfw-pall-process* nil) ; the number of the xterm process
;(defvar *prfw-^p-process* nil)
;(defvar *prfw-^pn-process* nil)
;(defvar *big-proofwindow* nil) ; size of proofwindow

;(defflag proofw-all
;  (flagtype boolean)
;  (default t)
;  (subjects otl-vars printing window-props)
;  (mhelp "If T, entire proof so far is printed in the Complete Proof 
;window, if this window exists."))

;(defflag proofw-active
;  (flagtype boolean)
;  (default t)
;  (subjects otl-vars printing window-props)
;  (mhelp "If T, active lines of the current proof are printed in the 
;Current Subproof window, if this window exists."))

;(defflag proofw-active+nos
;  (flagtype boolean)
;  (default t)
;  (subjects otl-vars printing window-props)
;  (mhelp "If T, active lines of the current proof are printed in the 
;Current Subproof & Line Numbers window, if this window exists."))

;(defflag proofw-active-height
;  (flagtype posinteger)
;  (default 24)
;  (subjects otl-vars window-props)
;  (mhelp "Controls the initial height of the Current Subproof window."))

;(defflag proofw-all-height
;  (flagtype posinteger)
;  (default 24)
;  (subjects otl-vars window-props)
;  (mhelp "Controls the initial height of the Complete Proof window."))

;(defflag proofw-active-width
;  (flagtype posinteger)
;  (default 80)
;  (subjects otl-vars window-props)
;  (mhelp "Controls the initial width of the Current Subproof window."))

;(defflag proofw-all-width
;  (flagtype posinteger)
;  (default 80)
;  (subjects otl-vars window-props)
;  (mhelp "Controls the initial width of the Complete Proof window."))

;(defflag proofw-active+nos-height
;  (flagtype posinteger)
;  (default 24)
;  (subjects otl-vars window-props)
;  (mhelp "Controls the initial height of the Current Subproof & Line Numbers
;window."))

;(defflag proofw-active+nos-width
;  (flagtype posinteger)
;  (default 80)
;  (subjects otl-vars window-props)
;  (mhelp "Controls the initial width of the Current Subproof & Line Numbers
;window."))

(defseqn begin-prfw
  (s-eqn-argtypes)
  (s-eqn-mainfns begin-s-prfw)
  (s-eqn-argnames )
  (mhelp "Begin proofwindow top level.
Open Current Subproof, Current Subproof & Line Numbers, and Complete Proof
windows with text size determined by the value of the flag CHARSIZE.
Printing in various windows can be modified by changing the flags
PROOFW-ALL, BLANK-LINES-INSERTED and PRINTLINEFLAG.
The initial size of the windows can be modified with the flags
PROOFW-ALL-HEIGHT and PROOFW-ALL-WIDTH; after the windows are open, they can
simply be resized as normal. PSTATUS will update the proofwindows manually
if necessary. 
Close the proofwindows with END-PRFW."))

(defseqn end-prfw
  (s-eqn-argtypes)
  (s-eqn-argnames )
  (s-eqn-mainfns end-s-prfw)
  (mhelp "End REW-PRFW top level; close all open proofwindows."))

;;;The following function is used to get rid the annoying "NIL" and "T",
;;;which always follow a command otherwise.
(defun print-*-s-prfw (result) (declare (ignore result)))

(defun general-begin-prfw ()
;;; set up *prfw-pall-window* and *prfw-^p-window* as streams to be
;;; printed to, then begin a new top level
  (declare (special printlineflag *using-interface*))
  (unless (or *prfw-pall-window* *prfw-^p-window* *prfw-^pn-window*)  ; prfw already started.
    (if proofw-all (setq *prfw-pall-window* (setup-prfw-pall-window)) (setq *prfw-pall-window* nil))
;    (if proofw-active (setq *prfw-^p-window* (setup-prfw-^p-window)) (setq *prfw-^p-window* nil))
    (setq *prfw-^p-window* nil)  ; since REWRITING has no ^p
;    (if proofw-active+nos (setq *prfw-^pn-window* (setup-prfw-^pn-window)) (setq *prfw-^pn-window* nil))
    (setq *prfw-^pn-window* nil)  ; since REWRITING has no ^pn
    (setq printlineflag nil)
    (unwind-protect 
	(let ((command-interpreter #'interpret-s-prfw)
              (print-* #'print-*-s-prfw)
              (top-prompt-fn #'s-prfw-prompt))
	  (%catch% (progn (secondary-top)
			  (throwfail "REW-PRFW aborted."))
		   (exit-inferior-top
		    (if (eq core::expand-catch-throw '|[Left REWRITING.]|)
			(%throw% '|[Left REWRITING.]| exit-inferior-top)))))
      (if *using-interface*
	  (progn
	    (close-window *prfw-pall-window*)
	    (close-window *prfw-^p-window*)
	    (close-window *prfw-^pn-window*)
	    (setq *prfw-pall-window* nil *prfw-^p-window* nil *prfw-^pn-window* nil))
	(progn
	  (when (streamp *prfw-pall-window*)
	    (close *prfw-pall-window*) 
	    (if (probe-file (pathname *prfw-pall-window*)) (delete-file (pathname *prfw-pall-window*))))
	  (when (streamp *prfw-^p-window*)
	    (close *prfw-^p-window*)  
	    (if (probe-file (pathname *prfw-^p-window*)) (delete-file (pathname *prfw-^p-window*))))
	  (when (streamp *prfw-^pn-window*)
	    (close *prfw-^pn-window*)  
	    (if (probe-file (pathname *prfw-^pn-window*)) (delete-file (pathname *prfw-^pn-window*))))
	  (if *prfw-pall-window* (setq *prfw-pall-window* (kill-xterm-window *prfw-pall-process* )))
	  (if *prfw-^pn-window* (setq *prfw-^pn-window* (kill-xterm-window *prfw-^pn-process* )))
	  (if *prfw-^p-window* (setq *prfw-^p-window* (kill-xterm-window *prfw-^p-process* ))))))))

(defun begin-s-prfw ()
  (progn (if (equal charsize 'MAX)
	     (setq *big-proofwindow* T)
	   (setq *big-proofwindow* NIL))
	 (general-begin-prfw)))

;;; this is arcane and silly, and is only written like this for historical reasons!
;;; there are much more efficient ways to do this, but efficiency isn't an issue here...

;(defun setup-prfw-^p-window ()
;  (declare (special *using-interface*))
;  "Make a new window to which we can output ^P as a Lisp stream."
;  (if *using-interface*
;      (open-window-with-socket "prfw^p" "Current Subproof"
;			       proofw-active-width proofw-active-height
;			       *big-proofwindow*)
;    (let* ((outfilename (concatenate 'string "/tmp/foo" 
;				     (princ-to-string (* 3 (tps-get-internal-run-time)))))
;	   (outstream (open outfilename :direction :output 
;			    :if-exists :overwrite 
;			    :if-does-not-exist :create)))
;      (if *big-proofwindow*
;	  (setq *prfw-^p-process* 
;		(setup-big-xterm-window "Current Subproof" outfilename 
;					(concatenate 'string (princ-to-string proofw-active-width) "x" 
;						     (princ-to-string proofw-active-height) "+0-0")))
;	(setq *prfw-^p-process* 
;	      (setup-xterm-window "Current Subproof" outfilename 
;				  (concatenate 'string (princ-to-string proofw-active-width) "x" 
;					       (princ-to-string proofw-active-height) "+0-0"))))
;      outstream)))

;(defun setup-prfw-^pn-window ()
;  (declare (special *using-interface*))
;  "Make a new window to which we can output ^PN as a Lisp stream."
;  (if *using-interface*
;      (open-window-with-socket "prfw^pn" "Current Subproof & Line Numbers"
;			       proofw-active-width proofw-active-height
;			       *big-proofwindow*)
;    (let* ((outfilename (concatenate 'string "/tmp/foo" 
;				     (princ-to-string (* 5 (tps-get-internal-run-time)))))
;	   (outstream (open outfilename :direction :output 
;			    :if-exists :overwrite 
;			    :if-does-not-exist :create)))
;      (if *big-proofwindow*
;	  (setq *prfw-^pn-process* 
;		(setup-big-xterm-window "Current Subproof & Line Numbers" outfilename 
;					(concatenate 'string (princ-to-string proofw-active-width) "x" 
;						     (princ-to-string proofw-active-height) "-0+0")))
;	(setq *prfw-^pn-process* 
;	      (setup-xterm-window "Current Subproof & Line Numbers" outfilename 
;				  (concatenate 'string (princ-to-string proofw-active-width) "x" 
;					       (princ-to-string proofw-active-height) "-0+0"))))
;      outstream)))

;(defun setup-prfw-pall-window ()
;  (declare (special *using-interface*))
;  "Make a new window to which we can output PALL as a Lisp stream."
;  (if *using-interface*
;      (open-window-with-socket "prfw-pall" "Complete Proof"
;			       proofw-all-width proofw-all-height
;			       *big-proofwindow*)
;    (let* ((outfilename (concatenate 'string "/tmp/foo" 
;				     (princ-to-string (get-universal-time))))
;	   (outstream (open outfilename :direction :output 
;			    :if-exists :overwrite 
;			    :if-does-not-exist :create)))
;      (if *big-proofwindow*
;	  (setq *prfw-pall-process* 
;		(setup-big-xterm-window "Complete Proof" outfilename 
;					(concatenate 'string (princ-to-string proofw-all-width) "x" 
;						     (princ-to-string proofw-all-height) "+0+0")))
;	(setq *prfw-pall-process* 
;	      (setup-xterm-window "Complete Proof" outfilename 
;				  (concatenate 'string (princ-to-string proofw-all-width) "x" 
;					       (princ-to-string proofw-all-height) "+0+0"))))
;      outstream)))

(defun end-s-prfw ()
  (declare (special printlineflag))
  (progn (setq printlineflag T)
	 (%throw% '|[Left REW-PRFW.]| exit-inferior-top)))


(defun s-prfw-opdecode (command)
  "Use comdecode on the command, and then update the prfw windows if
desired."
  (declare (special global-srulelist))
  (let ((command-name (car command))
	(result (s-eqn-opdecode command)))
    (when (or (member command-name *s-prfw-iftrue-update-commands*) (member command-name global-srulelist))
; this way we don't have to remember to add new rules to *s-prfw-iftrue-update-commands* ... MB 8/23/93
	  (s-prfw-pall)
	  (s-prfw-^p)
	  (s-prfw-^pn)
	  result)))

;;; the flag blank-lines-inserted is also used by the edwindows, and is defined in wffout.lisp

(defun interpret-s-prfw (cmd)
  "Same as s-eqn-command-interpreter, but uses s-prfw-opdecode instead of
  s-eqn-opdecode."
  (let ((carcmd (car cmd)))
    (setq core::*retrieve-stack* nil)
    (cond ((null cmd) nil)
	  ((and (null (cdr cmd)) (atom carcmd))
	   (cond 
	         ((integerp carcmd)
		  (throwfail "Unknown command."))
		 ((and (symbolp carcmd) (get carcmd 'seqncmd))
		  (if (eq carcmd 'leave)
		      (progn (complain "Use END-PRFW to leave.") nil)
		    `(s-prfw-opdecode (quote ,cmd))))
		 ((and (symbolp carcmd) (get carcmd 'mexpr))
		  (if (member carcmd *s-eqn-allowed-mexprs*)
		      `(prfw-comdecode (quote ,cmd))
		    (throwfail "Cannot apply " carcmd " in REW-PRFW top level.")))
		 ((and (symbolp carcmd) (get carcmd 'reviewcmd))
		  `(prfw-comdecode (quote ,cmd)))
		 ((and (symbolp carcmd) (get carcmd 'flag))
		  `(prfw-comdecode '(setflag ,@cmd)))
		 ((and (symbolp carcmd) (get carcmd 'rewrite-rule))
		  `(s-prfw-opdecode '(app ,@cmd)))
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
	   `(s-prfw-opdecode (quote ,cmd))) 
	  ((and (symbolp carcmd) (get carcmd 'mexpr))
	   (if (member carcmd *s-eqn-allowed-mexprs*)
	       `(prfw-comdecode (quote ,cmd))
	     (throwfail "Cannot apply " carcmd " in REW-PRFW top level.")))
	  ((and (symbolp carcmd) (get carcmd 'reviewcmd))
	   `(prfw-comdecode (quote ,cmd)))
	  ((and (symbolp carcmd) (get carcmd 'flag))
	   `(prfw-comdecode '(set ,@cmd)))
	  ((null expertflag)
	   (throwfail "Unknown command."))
	  ((symbolp carcmd)
	   (if (fboundp carcmd) cmd
	       (throwfail ";" carcmd " - Undefined function.")))
	  (t cmd))))

(defun s-prfw-pall ()
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof *using-interface*))
    (when (and proofw-all (streamp *prfw-pall-window*) (open-stream-p *prfw-pall-window*))
      (let ((*standard-output* *prfw-pall-window*)
	    (style (if use-window-style window-style style)))
	(if *using-interface*
	    (clear-window *prfw-pall-window*)
	  (dotimes (i blank-lines-inserted) (terpri)))
	(prtlines (get dproof 'lines)) (terpri)
	(when (and print-comments (get dproof 'comment) (listp (get dproof 'comment)))
	  (eval (get dproof 'comment)))
	(finish-output *standard-output*)))))

(defun s-prfw-^p ()
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof *using-interface*))
    (when (and proofw-active (streamp *prfw-^p-window*) (open-stream-p *prfw-^p-window*))
      (let ((*standard-output* *prfw-^p-window*)
	    (style (if use-window-style window-style style)))
	(if *using-interface*
	    (clear-window *prfw-^p-window*)
	  (dotimes (i blank-lines-inserted) (terpri)))
	(prtactive)
	(finish-output *standard-output*)))))

(defun s-prfw-^pn ()
  (let ((dproof (get *current-eqnder* 's-eqn-der)))
    (declare (special dproof *using-interface*))
    (when (and proofw-active+nos (streamp *prfw-^pn-window*) (open-stream-p *prfw-^pn-window*))
      (let ((*standard-output* *prfw-^pn-window*)
	    (style (if use-window-style window-style style)))
	(if *using-interface*
	    (clear-window *prfw-^pn-window*)
	  (dotimes (i blank-lines-inserted) (terpri)))
	(prtactivenos)
	(finish-output *standard-output*)))))
