;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of ETPS-EVENTS)

;;;
;;; File: ETPS-Events
;;;

(part-of etps-events)

(deffile etps-events
  (part-of etps-events)
  (extension lsp)
  (mhelp "Defines common events in ETPS.
They can be disabled or enabled in some common init file."))

(context tps-events)

(defflag error-file
  (flagtype filespec)
  (default "etps3.error")
  (subjects events)
  (mhelp "The file recording the events of errors."))

(defevent error
  (event-args error-args)
  (template ((or *remote-userid* (status-userid))
	     error-args))
  (template-names (userid error-args))
  (write-when immediate)
  (write-file error-file)    ; a global variable, eg
			     ; `((tpsrec: *) etps error)
  (signal-hook count-errors) ; count errors to avoid infinite loops
  (mhelp "The event of a Lisp Error."))

(defvar max-err-count 20)

(defvar err-count 0)

(defun count-errors (&rest error-args)
  (declare (special max-err-count err-count)
	   (ignore error-args))
  (if (> err-count max-err-count)
      (format *error-output* "You produced a lot of Common Lisp errors.
Perhaps you better interrupt with ^C and start over.")
      (incf err-count)))


(defvar computed-code 0)

;;; dt is used to freeze the daytime upon invocation of done-exc so that
;;; the code is computed correctly.

(defvar dt '(0 0 0)) 

(defflag USER-PASSWD-FILE
  (flagtype filespec)
  (default "user-passwd")
  (subjects events)
  (mhelp "The file recording user id's and passwords for a class
using ETPS over the web."))

(defflag score-file
  (flagtype filespec)
  (default "etps3.scores")
  (subjects events)
  (mhelp "The file recording completed exercises."))

(defevent done-exc
  (event-args numberoflines)
  (template ((or *remote-userid* (status-userid))
	     dproof numberoflines computed-code
	     (status-date) dt))
  (template-names (userid dproof numberoflines computed-code date daytime))
  (signal-hook done-exc-hook)
  (write-when immediate)
  (write-file score-file)
  (mhelp "The event of completing an exercise."))

(defun done-exc-hook (numberoflines)
  ;; The done-exc-hook will compute the code written to the file.
  ;; Freeze the time of day right now.
  (declare (special numberoflines))
  ;; because of the (eval `(list ..)) below.
  (setq dt (status-daytime))
  (setq computed-code 0)
  (setq computed-code (code-list (eval `(list ,@(get 'done-exc 'template))))))

(defflag proof-file
  (flagtype filespec)
  (default "etps3.proof")
  (subjects events)
  (mhelp "The file recording started and completed proofs."))

(defevent proof-action
  (event-args kind)
  (template ((or *remote-userid* (status-userid))
	     kind dproof (status-date) (status-daytime)))
  (template-names (userid kind dproof date daytime))
  (write-when immediate)
  (write-file proof-file)
  (mhelp "The event of completing any proof."))


(defflag advice-file
  (flagtype filespec)
  (default "etps3.advice")
  (subjects events)
  (mhelp "The file recording advice."))

(defevent advice-asked
  (event-args hint-p)
  (template ((or *remote-userid* (status-userid))
	     dproof hint-p))
  (template-names (userid dproof hint-p))
  (write-when 1)
  (write-file advice-file)
  (mhelp "Event of user asking for advice."))


;;;
;;; The final section is more for statistical purposes: Use of rules,
;;; number and kind of mistakes are recorded.
;;;

(defflag command-file
  (flagtype filespec)
  (default "etps3.command")
  (subjects events)
  (mhelp "The file recording commands."))

(defevent command
  (event-args command completion-status)
  (template (command completion-status))
  (template-names (command completion-status))
  (write-when 3)
  (write-file command-file)
  (mhelp "Event of user issuing a command."))

(defflag input-error-file
  (flagtype filespec)
  (default "etps3.ierror")
  (subjects events)
  (mhelp "The file recording illegal inputs caught by TPS."))

(defevent input-error
  (event-args argtype input)
  (template (argtype input))
  (template-names (argtype input))
  (write-when 3)
  (write-file input-error-file)
  (mhelp "Event of illegal input caught by TPS."))


(defflag rule-error-file
  (flagtype filespec)
  (default "etps3.rerror")
  (subjects events)
  (mhelp "The file recording illegal rules caught by TPS."))

(defevent rule-error
  (event-args rule mainfn complaint)
  (template (rule mainfn complaint))
  (template-names (rule mainfn complaint))
  (write-when 3)
  (write-file rule-error-file)
  (mhelp "Event of illegal rule applications caught by TPS."))

;;; Added 18OCT89 DAN.  Uses global variable SCORE-FILE, which should
;;; be set in the .SYS file. 

(context otl-entering)

(defmexpr summary
  (mhelp "Tells the user what exercises have been completed."))


(defun summary ()
  (summarize-score-file score-file))

(defun summarize-score-file (score-file)
  (let ((user (or *remote-userid* (status-userid))))
    (unless (probe-file score-file)
      (throwfail "Unable to find score file: " score-file "."))
    (with-open-file (scores-input score-file :direction :input)
      (let ((scores nil))
        (do ((line (read scores-input nil nil)
                   (read scores-input nil nil)))
                  ((null line))
          (when (eq (car line) user)
            (push line scores)))
        (if (null scores)
            (msgf "I have no record of any completed exercises." t)
            (progn
              (msgf "You have completed the following exercises:")
              (format t "~&  Exercise       Date       Time")
              (dolist (score (nreverse scores))
                (format t "~&  ~8@A ~? ~?" (second score) 
                        "  ~2,'0D-~2,'0D-~2,'0D" (nconc (cdr (fifth score))
                                                        (list (car 
                                                               (fifth score))))
                        "  ~2,'0D:~2,'0D:~2,'0D" (sixth score)))
              (msg t)))))))

