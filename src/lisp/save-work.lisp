;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of SAVE-TPS-WORK)

;;;
;;; File SAVE-WORK
;;;
;;; defines commands, flags, etc. dealing with saving and restoring
;;; of work.
;;;

(part-of save-tps-work)

(deffile save-work
  (part-of save-tps-work)
  (extension lsp)
  (mhelp "Contains commands for saving and restoring work."))

(context save-work-obj)

(defvar *executing* nil)
(defvar pause-quietly nil)

(defflag save-interval
  (flagtype integer+)
  (default 5)
  (subjects saving-work)
  (mhelp "Interval of file-write of saved commands."))

(defmexpr save-work
  (argtypes filespec)
  (argnames savefile)
  (arghelp "SAVE-WORK file")
  (defaultfns (lambda (savefile)
		(list (if (eq savefile '$)
			  (namestring
			   (make-pathname% :name "work" :type "work"
					   :version :newest))
			  savefile))))
  (mainfns open-save-file)
  (dont-restore t)
  (mhelp
   "Start saving commands in the specified file. These commands can be
executed subsequently by using EXECUTE-FILE or RESTORE-WORK. If you are 
creating a work file for a demonstration, and need it to pause at certain
points as it is reloaded by TPS, then see the help message for EXECUTE-FILE
for more information on how to do this."))

(defvar last-restored-file nil)
(defvar save-work-output-stream nil)

(defvar *standard-input*-save nil)

(defvar restore-work-echo-stream nil)


(defun open-save-file (fname)
  (if saving-work-p (throwfail "Saving in progress. Command ignored.")
      (progn
	(setq fname
	      (merge-pathnames fname
			       (make-pathname% :name "work" :type "work"
					       :version :newest))
	      save-work-output-stream (open fname :direction :output
                                            :if-exists :supersede
					    :if-does-not-exist :create))
	(format save-work-output-stream "~%")
	(close save-work-output-stream)
	(setq save-work-output-stream (open fname :direction :output
					    :if-exists :append)
	      save-work-file (namestring (pathname save-work-output-stream)))
	(format save-work-output-stream ";;~A saving into file ~A on ~%;;~A~%"
		(status-userid)		; DAN 10/31/87
		save-work-file (stringdt nil))
	(setq save-work-echo-stream
	      (make-echo-stream *standard-input* save-work-output-stream)
	      *standard-input*-save *standard-input*
	      *standard-input* save-work-echo-stream
	      save-work-p T
	      saving-work-p T))))


(defmexpr stop-save
  (dont-restore t)
  (mhelp "Stop saving commands in a SAVE-WORK file."))

(defun stop-save ()
  (if saving-work-p
      (progn
	(fresh-line save-work-output-stream)
	(setq *standard-input* *standard-input*-save)
	(close save-work-output-stream)
	;;(close save-work-echo-stream)
	(format *standard-output* "~&File ~A written." save-work-file)
	(setq save-work-p nil)
	(setq saving-work-p nil))
      (throwfail "No saving in progress.")))

(defmexpr finish-save
  (dont-restore t)
  (mhelp "Finishing saving work in a file.
The difference between STOP-SAVE and FINISH-SAVE is: 
the former is temporary because you can use RESUME-SAVE
to resume saving work into the same file; the latter closes 
the output stream, so you can not save work into the same 
file after executing it."))

(defun finish-save ()
  (when save-work-p 
     (when (streamp save-work-output-stream) 
	   (fresh-line save-work-output-stream)
	   (close save-work-output-stream)
	   (setq save-work-output-stream nil))
     (setq *standard-input* *standard-input*-save)
     (setq saving-work-p NIL)
     (format *standard-output* "~&File ~A written." save-work-file)))


(defmexpr execute-file
  (argtypes filespec yesno filespec yesno)
  (argnames comfil execprint outfil stepping)
  (arghelp "SAVE-WORK file" "Execute Print-Commands?"
	   "Output file (\"NUL:\" to discard)" "Single Step Operation?")
  (defaultfns
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '($ nil "TTY:" nil) rest))
   (lambda (comfil %2 %3 %4)
      (if (eq comfil '$)
	  (setq comfil
		(namestring (make-pathname% :name "work" :type "work"))))
      (list comfil %2 %3 %4)))
  (mainfns restore-work)
  (dont-restore t)
  (mhelp "Execute commands from a SAVE-WORK file.
Call this from the main top level or the proofwindows top level of TPS.
Note that this will not save subsequent commands in the same file,
which distinguishes it from RESTORE-WORK.
In the cases where EXECUTE-FILE doesn't work, one can usually just 
load the .work file into an editor and then cut and paste it, whole, 
into the TPS window.
Single-stepping only works between commands on the main top level;
it will not stop at prompts which are internal to a command, nor between
commands on a different top level. To force a work-file to stop in
such a place, use the PAUSE command when creating the work file.
If you are single-stepping through a file, you can 
abort at any time by typing ^G<RETURN>."))

(defun restore-work (comfil execprint outfil &optional (stepping nil))
  (declare (special execprint stepping intermediary *executing* pause-quietly))
  (let ((filename
	  (merge-pathnames comfil (make-pathname% :name "work" :type "work")))
	(outflag (string-equal outfil "TTY:")))
    (unless (probe-file filename)
      (throwfail "File " comfil " does not exist."))
    (if stepping (setq pause-quietly (query "Suppress \"Press RETURN\" messages between steps?" t))
      (setq pause-quietly nil))
    (when pause-quietly (msgf "Whenever the output pauses, press RETURN to continue, or ^G RETURN to abort." t
			      "Press RETURN now to start executing the file...")
	  (pause))
    (with-open-file (restore-work-infile filename :direction :input)
      (declare (special restore-work-infile))
      (let ((save-work-p nil)
	    (saving-work-p nil)
	    (restore-file-p t)	    
	    (outfile
             (cond (outflag *standard-output*)
                   ;; following will make a bit-sink
                   ((string-equal outfil "NUL:")
                    (make-broadcast-stream))
                   (t (open   
                       (merge-pathnames outfil (make-pathname%
                                                :name "work" :type "work"))
                       :direction :output))))
	    (printlineflag execprint))
	(declare (special printlineflag restore-file-p))
	;; added pathname below since we can't take truename of
	;; of a stream, only of a pathname DAN 11/7/87
	(setq last-restored-file
	      (namestring (truename (pathname restore-work-infile)))
	      restore-work-echo-stream
	      (make-echo-stream restore-work-infile outfile))
	(setq *executing* t)
	(if stepping
	    (let ((command-interpreter #'interpret-step-top))
	      (unwind-protect (do ((*standard-input* restore-work-echo-stream)
				   (*standard-output* outfile))
				  ((catch 'stepping-aborted (%catch% (%top-catch% (progn (secondary-top)
						   (throwfail "Execute-file aborted."))
					   (exit-inferior-top))
				  (restore-file-end t)))))))
	  (unwind-protect (do ((*standard-input* restore-work-echo-stream)
			       (*standard-output* outfile))
			      ((%catch% (%top-catch%
					 (progn (top) T)
					 nil)
					(restore-file-end t))))))
	(setq *executing* nil pause-quietly nil)
	(unless outflag (close outfile))))))

(defmexpr restore-work
  (argtypes filespec yesno filespec)
  (argnames comfil execprint outfil)
  (arghelp "SAVE-WORK file" "Execute Print-Commands?"
	   "Output file (\"NUL:\" to discard)")
  (defaultfns 
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '($ nil "TTY:") rest))
    (lambda (comfil %2 %3)
      (if (eq comfil '$)
	  (setq comfil
		(namestring (make-pathname% :name "work" :type "work"))))
      (list comfil %2 %3)))
  (dont-restore t)
  (mainfns applicable-p-restore-work restore-work continue-save-after-restore)
  (mhelp "Execute commands from a SAVE-WORK file and continue
to save in that file. See EXECUTE-FILE for more information."))

(defun applicable-p-restore-work (&rest rest)
  (declare (ignore rest))
  (if saving-work-p
      (throwfail T "Saving in progress!!! Perhaps, you want to use the command EXECUTE-FILE.
 Please use the command STOP-SAVE first if you wish to use RESTORE-WORK. ")))

(defun continue-save-after-restore (comfil execprint outfil)
  (declare (ignore comfil execprint outfil))
  (setq saving-work-p t
	save-work-file last-restored-file
	save-work-output-stream
	(open save-work-file :direction :output :if-exists :append))
  (format save-work-output-stream "~%;;~A resuming ~A on ~%;;~A ~%"
	  (status-userid) save-work-file
	  (stringdt nil))
  (setq save-work-echo-stream
	(make-echo-stream *standard-input* save-work-output-stream)
	*standard-input*-save *standard-input*
	*standard-input* save-work-echo-stream))

(defmexpr resume-save
  (dont-restore t)
  (mainfns applicable-p-resume-save resume-save)
  (mhelp "Use this command to resume saving commands into the most recent
save-work file.  Unlike RESTORE-WORK, this command doesn't
execute commands from the file, but simply appends subsequent commands to
the file.  You can not use this command if you are already saving work.
Also, you may run into trouble if you forgot to save some commands. "))

;;Note that this command is not necessary if you left TPS by using the EXIT
;;command and returned using REENTER; in this case TPS will automatically 
;;resume saving (if you were saving before you exited).  If, however, you
;;left TPS by using the EXIT command and returned by using CONTINUE, you
;;must use this command to resume saving work."


(defun applicable-p-resume-save ()
  (if saving-work-p
      (throwfail T "Saving in progress!!! Perhaps, you want to use the command EXECUTE-FILE.
 Please use the command STOP-SAVE, if you wish to use RESTORE-WORK. ")
      (if save-work-file
	  (msg T "Resuming SAVE-WORK in file " (save-work-file . filespec))
	  (throwfail "You haven't saved any work previously!!! 
I think that you want to use the command SAVE-WORK."))))

(defun resume-save ()
  (setq saving-work-p t
	save-work-output-stream (open save-work-file :direction :output
				      :if-exists :append))
  (format save-work-output-stream "~%;;~A resuming ~A on ~%;;~A ~%"
	  (status-userid)
	  save-work-file
	  (stringdt nil))
  (setq save-work-echo-stream
	(make-echo-stream *standard-input* save-work-output-stream)
	*standard-input*-save *standard-input*
	*standard-input* save-work-echo-stream))


(defun save-command (&rest args)
  (format save-work-output-stream "~{~A~}" args))

(defmexpr script
  (argtypes filespec yesno)
  (argnames scriptfile if-exists-append)
  (defaultfns (lambda (file append-p)
		(if (eq file '$)
		    (setq file (namestring (make-pathname% :name "script"))))
		(if (eq append-p '$) (setq append-p t))
		(list file append-p)))
  (arghelp "File in which to save transcript"
	   "If yes, appends to file if it exists")
  (mhelp "Saves a transcript of session to a file. If the current setting
of STYLE is SCRIBE or TEX, an appropriate header will be output to the 
script file (unless the file already exists).
**NOTE** If you start SCRIPT from a PUSHed top level, be sure to do 
UNSCRIPT before you POP that top level, or your transcript may be lost.
The same also applies to starting SCRIPT from subtoplevels such as MATE;
you can enter further subtoplevels like LIB and ED from the MATE top level,
and SCRIPT will carry on recording, but before leaving the MATE top level 
you should type UNSCRIPT or your work will be lost.
"))

(defmexpr unscript
  (argtypes )
  (mhelp "Closes the most recent file opened with the SCRIPT command."))


(defun script (pathname append-if-exists)
  (declare (special scribe-preamble latex-preamble tpstex vpdtex))
  (let ((dummy (probe-file pathname)))
  (with-open-file (f pathname :direction :output  
                     :if-exists (if append-if-exists :append :supersede)
		     :if-does-not-exist :create)
    (catch 'script-file-punt
	  (let ((*standard-input*
		  (make-echo-stream *standard-input* f))
		(*standard-output*
		  (make-broadcast-stream *standard-output* f))
		(script-files (cons pathname script-files)))
	    (if (and (not dummy) (eq style 'scribe)) (msg scribe-preamble t "@begin{verbatim}" t))
	    (when (and (not dummy) (memq style '(tex tex-1))) 
		  (if latex-emulation (msgf latex-preamble)
		    (progn 
		      (msgf "\\magnification=\\magstep1")
		      (when (probe-file tpstex) (msgf "\\input " tpstex))
		      (when (probe-file vpdtex) (msgf "\\input " vpdtex)))))
; this is vpform-tex-preamble, which may not be present
	    (msgf "Opening script file: " pathname t)
	    (if (eq top-level 'cmd-top)
		(absolute-top-top)
	      (secondary-top)))))))


(defun unscript ()
  (declare (special latex-postamble))
  (if script-files
      (progn 
	(msgf "Closing script file: " (namestring (car script-files)))
	(if (memq style '(tex tex-1)) (if latex-emulation (msgf latex-postamble) (msg -2 "\\end" t)))
	(if (eq style 'scribe) (msg t "@end(verbatim)" t))
	(throw 'script-file-punt nil))
    (msgf "No script file in use." t)))


(defmexpr save-flags-and-work
  (argtypes filespec)
  (argnames savefile)
  (arghelp "SAVE-WORK file")
  (defaultfns (lambda (savefile)
		(list (if (eq savefile '$)
			  (namestring
			   (make-pathname% :name "work" :type "work"
					   :version :newest))
			  savefile))))
  (mainfns open-save-file output-all-flags)
  (dont-restore t)
  (mhelp
   "Start saving commands in the specified file, first storing all
flag settings."))


(defun output-all-flags (ignore)
  (declare (ignore ignore))
  (when (and saving-work-p save-work-output-stream)
;    (format save-work-output-stream "~%") ;"(progn ~%")
    (dolist (flag global-flaglist)
      (if (and (symbolp flag)
               (not (compiled-function-p (symbol-value flag))))
          (format save-work-output-stream
                  (conc-strings "(setq " (symbol-name flag)
                                " (quote ~S))~%")
                  (symbol-value flag))))))
;    (format save-work-output-stream "~%")))  ;")~%")))
;used to export as a (progn ...); now exports as a lot of setq's   MB 4/7/94

;(deftoplevel prfw-top
;  (top-prompt-fn prfw-tps-top-prompt)
;  (command-interpreter interpret-prfw-top)
;  (print-* print-*-tps-top)
;  (top-level-category mexpr)
;  ;;(top-level-ctree command-ctree)
;  (top-cmd-decode prfw-comdecode)
;  (mhelp "The command top level of TPS, supplemented by proofwindow output."))


(defun step-comdecode (command)
  "Use comdecode on the command, update proofwindows, and then wait for a RETURN."
  (let ((command-name (car command))
	(result (comdecode command)))
    (when (and result (memq command-name *prfw-iftrue-update-commands*))
	  (prfw-pall)
	  (prfw-^pn)
	  (prfw-^p))
    (pause)
    result))

(defmexpr pause
  (mhelp "Force a work file to stop and query the user. PAUSE, like ABORT, is valid
both as a top-level command and as a response to a prompt; it prints the 
message \"Press RETURN, or Ctrl-G RETURN to abort.\", waits for such a response 
from the user, and then repeats the original prompt.
This command is of no use unless a work file is being created; see EXECUTE-FILE 
for more details."))

(defun pause ()
  (unless pause-quietly
	  (terpri)
	  (terpri)
	  (princ "Press RETURN, or ^G RETURN to abort.")
	  (terpri))
  (force-output)
  (when (eq (char-int (read-char *query-io*)) 7) (throw 'stepping-aborted t)))

(defun interpret-step-top (cmd)
  "Same as interpret-tps-top, but uses step-comdecode instead of comdecode."
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
		  `(step-comdecode ',cmd))	       
		 ((get (car cmd) 'reviewcmd)
		  (if (eq (car cmd) 'leave)
		      (progn (complain "Use EXIT to leave.") nil)
		      `(step-comdecode ',cmd)))
		 ((get (car cmd) 'flag)
		  `(step-comdecode '(setflag ,@cmd)))
		 ((null expertflag)
		  (throwfail "Unknown Command." ))
		 ((boundp (car cmd)) (car cmd))
		 ((or (get (car cmd) 'mhelp) (get (car cmd) 'mhelp-fn))
		  (msg "Cannot evaluate that... calling HELP " (car cmd) t t)
		  `(step-comdecode '(help ,(car cmd))))
		 ((not (fboundp (car cmd)))
		  (throwfail ";" (car cmd) " - Unbound variable." ))))
	       ;; Not a symbol 
	       ((null expertflag)
		(throwfail "Unknown Command." ))
	       (t (car cmd))))
	((and expertflag (null (cdr cmd)))
	 (cond ((not (and (symbolp (caar cmd)) (fboundp (caar cmd))))
		(throwfail ";" (car cmd) " - Undefined function." ))
	       (t (car cmd))))
	((not (symbolp (car cmd)))
	 (throwfail "Illegal input - must be EXPERT to evaluate Lisp form." ))
	((get (car cmd) 'mexpr)
	 (setq curr-cmd (car cmd))
	 `(step-comdecode ',cmd))
	((get (car cmd) 'reviewcmd)
	 `(step-comdecode  ',cmd))
	((get (car cmd) 'flag)
	 (if (cdr cmd) `(step-comdecode '(set ,@cmd))
	     `(step-comdecode '(setflag ,@cmd))))
	((null expertflag)
	 (throwfail "Unknown command." t))
	((not (fboundp (car cmd)))
	 (throwfail ";" (car cmd) " - Undefined function." ))
	(t cmd)))

