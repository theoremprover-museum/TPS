;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of BARE)

;;;
;;; File: TOP
;;; Author: fp
;;;
;;; This file contains functions which let one define a top level and
;;; defines the default top-level when starting TPS.
;;;
;;;


(deffile top
  (part-of bare)
  (extension lsp)
  (mhelp "Defines default top-level."))


(context subtoplevels)

(defvar restore-file-p nil)

(defvar absolute-top-p t)



;;;
;;; Now follow a number of variables which may be, but need not be 
;;; locally bound be a LET in the function enclosing the secondary
;;; top level.
;;;


(defvar top-prompt-fn 'top-prompt)	; The primary prompt function

(defvar command-interpreter 'interpret-tps-top) ; Command interpreter

(defvar print-* 'print-*-tps-top)	; Function printing the result

(deftoplevel cmd-top
  (top-prompt-fn top-prompt)
  (command-interpreter interpret-tps-top)
  (print-* print-*-tps-top)
  (top-level-category mexpr)
  ;;(top-level-ctree command-ctree)
  (top-cmd-decode comdecode)
  (mhelp "The initial command top level of TPS."))

;;;
;;; The following are specially bound for every top-level itself.
;;; The user has no choice here.
;;;


(defvar top-level 'cmd-top)		; CMD-TOP is the current top-level

;;;
;;; SECONDARY-TOP is to be called when a secondary top level is desired.
;;; Notice that SECONDARY-TOP will return if the result of an evaluation
;;; is QUIT-THIS-TOP-LEVEL.
;;;

(defun secondary-top ()
  (let ((absolute-top-p nil)
	(* nil)
	+ -)
    (%catch% (%top-catch% (loop (tps-top-level)))
	     (quit-inferior-top (%throw% expand-catch-throw 
                                         abort-inferior-top)))))


(defun absolute-top-top ()  ;;never returns
  (let ((absolute-top-p T)
	(* nil)
	+ -)
    (loop (%top-catch% (tps-top-level)))
    ))

;;;
;;; The function TOP creates a new secondary top level.
;;; This section may be seen as an example of how to define a top-level.
;;;

(defvar top-level-level 0)

(defun top () (%catch% (tps-sub-top) (exit-inferior-top expand-catch-throw)))

(defun tps-sub-top ()
  (let ((top-prompt-fn #'sec-tps-top-prompt)
	(command-interpreter #'interpret-tps-top)
	(print-* #'print-*-tps-top)
	(top-level-level (+ top-level-level 1))
	(top-level 'cmd-top)
	)
    (secondary-top)))

;(defun exit-top () (%throw% 'quit-this-top-level quit-inferior-top)) ;loses all information on return!
(defun exit-top () (%throw% nil exit-inferior-top))

(defun sec-tps-top-prompt (id)
  (format nil "<~A:~A>" top-level-level id))

;;;
;;; TPS-TOP-LEVEL is the function that is set to the absolute top level,
;;; but it is also used by any other top levels.
;;; THROWS with the label QUIT-INFERIOR-TOP are cought by SECONDARY-TOP
;;; which passes it on to be caught by the enclosing top-level as a THROW
;;; with label ABORT-INFERIOR-TOP.

(defvar curr-cmd 'none)
(defvar beep-flag T)



;;; This is used to keep commands that have already been issued at a
;;; previous top level, then use them when recursing into a new top
;;; level.  For example, user might say "<0> mate foo ! & go & leave ".


(defvar *top-level-commands-in-queue* nil)

(defun tps-top-level ()
  (declare (special *using-interface* *executing*))
  (flet ((read-user-input ()
	   (setq beep-flag T)
	   (do ((tps-input
		 (%catch% (progn (fresh-line)
				 (when *using-interface* 
				   (command-finished)
				   (change-color 'black))
				 (linereadpp (funcall top-prompt-fn
						      *command-history-counter*)
					     'keep-history
					     'do-history-subs 
					     'do-alias-subs 
					     'do-flag-subs
					     'do-command-completion
					     (if (or (and *using-interface* (not *executing*))
						     (get 'COMMAND 'RESPONSE))
						 'COMMAND
					       *standard-input*)))
			  (fail (complain f "TPS error while reading."
					  t expand-catch-throw)
				(when (fboundp 'signal-event)
				  (signal-event 'input-error 'on-top
						'read-throw))		   
				nil))
		 (%catch% (progn (fresh-line)
				 (linereadpp (funcall top-prompt-fn
						      *command-history-counter*)
					     'keep-history
					     'do-history-subs 
					     'do-alias-subs 
					     'do-flag-subs
					     'do-command-completion
					     (if (or (and *using-interface* (not *executing*))
						     (get 'COMMAND 'RESPONSE))
						 'COMMAND
					       *standard-input*)))
			  (fail (complain f "TPS error while reading."
					  t expand-catch-throw)
				(when (fboundp 'signal-event)
				  (signal-event 'input-error 'on-top
						'read-throw))		   
				nil))))
	       (tps-input tps-input))))
  (do* ((commands
	 *top-level-commands-in-queue*
	 *top-level-commands-in-queue*)
	(end-command
	 (position '& commands) 
	 (position '& commands))
	(next-command 
	 (subseq commands 0 end-command)
	 (subseq commands 0 end-command))
	(tps- 
	 (if next-command
	     (%catch% (progn (setq *top-level-commands-in-queue*
			       (if end-command
				   (subseq *top-level-commands-in-queue* 
					   (1+ end-command))
				 nil))
			     (when *using-interface* (terpri))
			     (funcall command-interpreter next-command))
		      (fail (when (fboundp 'signal-event)
			      (signal-event 'input-error 'cmd next-command))
			    (complain f expand-catch-throw) nil)))
	 (if next-command
	     (%catch% (progn (setq *top-level-commands-in-queue*
			       (if end-command
				   (subseq *top-level-commands-in-queue* 
					   (1+ end-command))
				 nil))
			     (when *using-interface* (terpri))
			     (funcall command-interpreter next-command))
		      (fail (when (fboundp 'signal-event)
			      (signal-event 'input-error 'cmd next-command))
			    (complain f expand-catch-throw) nil))))
	(tps* nil))
      (nil)
    (when tps-
      (setq tps* '("[Command aborted.]"))
      (%catch% (%top-catch% 
		(prog1 (setq tps*
			     (multiple-value-list (eval tps-)))
		  (terpri)
		  (dolist (elt tps*) (funcall print-* elt))
		  (terpri))
		(when (fboundp 'signal-event)
		  (signal-event 'command curr-cmd '^g)) tps*)
	       (fail (when (fboundp 'signal-event)
			   (signal-event 'command curr-cmd 'fl))
		     (complain f expand-catch-throw) tps*)
	       (abort-inferior-top (complain f expand-catch-throw) tps*)))
    (unless *top-level-commands-in-queue* 
	    (setq *top-level-commands-in-queue*
		  (read-user-input))))))


;;;
;;; The rest of this file defines the functions called by the
;;; absolute top level.
;;;

;;;
;;; PRINT-*-TPS-TOP prints the result for the normal TPS top level
;;;

(defun print-*-tps-top (result)
  (cond ((not expertflag)
	 (cond ((string= result "[Command aborted.]")  (princ result))))))
;;; I removed the following part from the old version of print-*-tps-top
;;;in order to get rid of annoying "NIL" and "T" following every command
;;;issued in the tps toplevel. (HX 8/4/93)
;;;(t (prin1 result))))

;;;
;;; INTERPRET-TPS-TOP is the command interpreter for the absolute top
;;; level.
;;;
;;; A command interpreter is a function which takes the command issued
;;; by the user and returns a (Lisp) form that is to be evaluated instead.
;;; If NIL is returned, nothing is done.
;;;
;;; Cleaned up this function to keep from getting Lisp errors from
;;; using get on non-symbols, etc.  10/16/87 DAN

(defun interpret-tps-top (cmd)
  (declare (special *running-remotely* *expert-running-remotely*))
  (setq curr-cmd 'none)
  (cond ((null cmd) nil)
	;; Input of form '(atom)
	((and (null (cdr cmd)) (atom (car cmd)))
	 (cond ;; Have to make sure we have a symbol before
	       ;; doing these gets
	       ((symbolp (car cmd))
		(cond 
		 ((get (car cmd) 'mexpr)
		  (setq curr-cmd (car cmd))
		  `(comdecode ',cmd))	       
		 ((get (car cmd) 'reviewcmd)
		  (if (eq (car cmd) 'leave)
		      (progn (complain "Use EXIT to leave.") nil)
		      `(comdecode ',cmd)))
		 ((get (car cmd) 'auto::monitorfn)
		  (setq curr-cmd (car cmd)) `(comdecode ',cmd))
		 ((get (car cmd) 'flag)
		  `(comdecode '(setflag ,@cmd)))
		 ((null expertflag)
		  (throwfail "Unknown Command." ))
		 ((or (get (car cmd) 'mhelp) (get (car cmd) 'mhelp-fn))
		  (msg "Cannot evaluate that... calling HELP " (car cmd) t t)
		  `(comdecode '(help ,(car cmd))))
		 ((boundp (car cmd)) `(multiple-prin1 ,(car cmd)))
		 ((not (fboundp (car cmd)))
		  (throwfail ";" (car cmd) " - Unbound variable." ))))
	       ;; Not a symbol 
	       ((null expertflag)
		(throwfail "Unknown Command." ))
	       (t `(multiple-prin1 ,(car cmd)))))
	((and expertflag (null (cdr cmd)) (or (not *running-remotely*) *expert-running-remotely*))
	 (cond ((not (and (symbolp (caar cmd)) (fboundp (caar cmd))))
		(throwfail ";" (car cmd) " - Undefined function." ))
	       (t `(multiple-prin1 ,(car cmd)))))
	((not (symbolp (car cmd)))
	 (if (and *running-remotely* (not *expert-running-remotely*))
	     (throwfail "Cannot remotely evaluate Lisp form." )
	   (throwfail "Illegal input - must be EXPERT to evaluate Lisp form." )))
	((get (car cmd) 'mexpr)
	 (setq curr-cmd (car cmd))
	 `(comdecode ',cmd))
	((get (car cmd) 'reviewcmd)
	 `(comdecode  ',cmd))
	((get (car cmd) 'auto::monitorfn)
	 (setq curr-cmd (car cmd)) `(comdecode ',cmd))
	((get (car cmd) 'flag)
	 (if (cdr cmd) `(comdecode '(set ,@cmd))
	     `(comdecode '(setflag ,@cmd))))
	((null expertflag)
	 (throwfail "Unknown command." t))
	((not (fboundp (car cmd)))
	 (throwfail ";" (car cmd) " - Undefined function." ))
	(t `(multiple-prin1 ,cmd))))

;;;The following function can be used when you
;;;would like to write a new, somehow powerful,
;;;toplevel such as mate toplevel and editor toplevel.

(defun consp-interpreter (command)
  (declare (special rest))
  (let ((carcmd (car command)))
       (cond ((null expertflag) (throwfail "Unknown command for flag."))
             ((and (symbolp carcmd) (not (fboundp carcmd)))
              (setq rest nil)
              `(complain ',carcmd ": undefined function. "))
              (t `(multiple-prin1 ,command)))))


(defun flag-interpreter (command)
   (declare (special rest))
   (let* ((more-p (> (length rest) 1))
          (new-rest (if more-p (nthcdr 1 rest) nil))
          (args (if more-p (ldiff rest new-rest) rest)))
         (setq rest new-rest)
         `(%catch% ,(if args `(comdecode '(set ,command ,@args))
                             `(comdecode '(setflag ,command)))
                    (fail (complain "Error while setting flag " 
				    ',command "." core::expand-catch-throw)))))

(defun mexpr-interpreter (command)
  (declare (special rest))
  (let* ((no-args (length (get command 'argtypes)))
         (more-p (> (length rest) no-args))
         (new-rest (if more-p (nthcdr no-args rest) nil))
         (args (if more-p (ldiff rest new-rest) rest)))
        (setq rest new-rest)
        `(%catch% (comdecode ',(cons command args))
                  (fail (complain "Error in Top-Level command " ',command "."
                                  expand-catch-throw)))))

(defun misc-interpreter (command)
   (declare (special rest))
   (cond ((null expertflag) (throwfail "Unknown command or flag."))
         ((and (if (symbolp command) (boundp command)) (null rest))
          `(prin1 ,command))
         ((symbolp command)
          (cond ((not (fboundp command))
                 `(complain ',command ": undefined variable or function. "))
                (t (let ((hxrest rest))
                         (setq rest nil)
                        `(multiple-prin1 ,(cons command hxrest))))))
         (t (setq rest nil) `(multiple-prin1 ,command))))

;;;
;;; TOP-PROMPT and TOP-PROMPT2 are the functions which return the primary
;;; and secondary prompt for the absolute top level.  Each top level
;;; should locally bind TOP-PROMPT-FN and TOP-PROMPT2-FN to functions
;;; which result in their own prompts, so that different top levels
;;; can be distinguished.
;;;

(defun top-prompt (id) (format nil "<~A>" id))

(defun prompt2 (top-prompt-fn id)
  (format nil "<~A>"  (funcall top-prompt-fn id)))

(context subtoplevels) 

(defmexpr push
  (mainfns top)
  (mhelp "Start a new top level. This command is almost useless,
except from within a prompt (e.g. one can type PUSH in the middle 
of converting an etree to a ND proof interactively, call SCRIBEPROOF,
and then type POP to return to the conversion)."))

(defmexpr pop
  (mainfns exit-top-2)
  (mhelp "Return from a top level started with PUSH."))

(defun exit-top-2 ()
  (if (> top-level-level 0) (exit-top) (msg "You are already at the topmost level!" t)))

(context save-work-obj)

(defflag save-work-p
  (flagtype boolean)
  (default t)
  (subjects saving-work)
  (mhelp "If T, work is saved automatically."))

(defflag save-work-on-start-up
  (flagtype boolean)
  (default nil)
  (subjects saving-work)
  (mhelp "If T, work is saved automatically whenever TPS3 is started. "))
