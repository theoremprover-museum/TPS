

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic reimplementation of the suggestion mechanism
;; using concurrent lisp facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The Process Module
;;
;;                            (sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Module is mainly a wrapper for some multiprocessing functions
;; (Thanx to the INKA people and especially (serge@dfki.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :AUTO)


; (mod~defmod PROC
;             :uses (socket)
;             :documentation "Wrapping functions for multi-processing functionality of ALLEGRO and LUCID."
;             :exports (
; 		      proc~activate
; 		      proc~actual-process
; 		      proc~add-arrest-reason
; 		      proc~all-processes
; 		      proc~allow-schedule
; 		      proc~arrest-reasons
; 		      proc~create
; 		      proc~deactivate
; 		      proc~interrupt
; 		      proc~is-active
; 		      proc~is-running
; 		      proc~kill
; 		      proc~make-process-lock
; 		      proc~new-priority
; 		      proc~name
; 		      proc~p
; 		      proc~property-list
; 		      proc~priority
; 		      proc~quit
; 		      proc~resume-hook
; 		      proc~revoke-arrest-reason
; 		      proc~sleep
; 		      proc~socket-wait-for-input-available
; 		      proc~wait
; 		      proc~wait-with-timeout
; 		      proc~with-process-lock
; 		      proc~without-scheduling
; 		      proc~whostate
; 		      ))



(defun proc~all-processes ()
;  (declare (edited  "09-MAR-2000")
;	   (authors sorge ?)
;	   (input   "None.")
;	   (effect  "Lists all active processes.")
;	   (value   "None."))

  #+ALLEGRO        mp:*all-processes* 
  #+LUCID          lcl:*all-processes*
  )

(defun proc~actual-process ()
;  (declare (edited  "10-MAR-2000")
;	   (authors Sorge ?)
;	   (input   "None.")
;	   (effect  "Lists process which is currently running.")
;	   (value   "None."))

  #+ALLEGRO        mp:*current-process* 
  #+LUCID          lcl:*current-process*
  )

(defmacro proc~without-scheduling (&body body)

  #+ALLEGRO     `(excl:without-interrupts ,@body)
  #+LUCID       `(with-scheduling-inhibited ,@body)
  )

(defmacro proc~allow-schedule (&optional process)

  ;;; Edited  : 12. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   :

  #+ALLEGRO `(mp:process-allow-schedule ,process)
  #+LUCID   `(process-allow-schedule ,process)
  )


(defmacro proc~quit ()

  #+ALLEGRO     `(exit)
  #+LUCID       `(quit)
  )


(defmacro proc~create (&key name (priority 0) function args wait-function wait-args)
  #+ALLEGRO (declare (ignore wait-function wait-args))
;  (declare (edited  "09-MAR-2000")
;	   (authors sorge)
;	   (input   "name:          name of process"
;		    "function:      the initial function which starts the process"
;		    "args:          list of arguments to the initial function"
;		    "wait-function: the wait function is used by the scheduler to test whether"
;		    "the process should run"
;		    "wait-args:     list of arguments to the wait function")
;	   (effect  "A process is created.")
;	   (value   "A process."))

  #+ALLEGRO `(mp:process-run-function (list :name ,name :priority ,priority) ,function ,@args)
  #+LUCID `(append
	    (list 'make-process
		  :name ,name :function ,function :args ,args
		  :priority ,priority)
	    (cond ((neq ,wait-function nil) (list :wait-function ,wait-function)))
	    (cond ((neq ,wait-args nil) (list :wait-args ,wait-args))))
  )

		
(defun proc~is-active (process)
;  (declare (edited  "09-MAR-2000")
;	   (authors sorge ?)
;	   (input   "A process.")
;	   (effect  "Test the state of the process."
;		    "A process is active, if it is either running or waiting to run."
;		    "A process is inactive, if it is a process that is alive but that cannot be run."
;		    "A process is dead, if it has been killed or if it has run to completion.")
;	   (value   "True, if the process is active, otherwise Nil."))

  #+ALLEGRO  (if (mp:process-active-p process) t nil)
  #+LUCID    (if (lcl:process-active-p process) t nil)
  )

(defun proc~is-running (process)
;  (declare (edited  "09-MAR-2000")
;	   (authors sorge ?)
;	   (input   "A process.")
;	   (effect  "Test the state of the process."
;		    "A process is active, if it is either running or waiting to run."
;		    "A process is inactive, if it is a process that is alive but that cannot be run."
;		    "A process is dead, if it has been killed or if it has run to completion.")
;	   (value   "True, if the process is active, otherwise NIL."))

   
  #+ALLEGRO  (if (mp:process-runnable-p process) t nil)
  #+LUCID    (if (lcl:process-runnable-p process) t nil)
  )


(defmacro proc~sleep (seconds &optional whostate)
;  (declare (edited  "09-MAR-2000")
;	   (authors sorge ?)
;	   (input   "seconds: the time the process is put to sleep"
;		    "whostate: specifies a string that will be displayed as the state of the"
;		    "process")
;	   (effect  "Suspends the actual process for the specified number of seconds")
;	   (value   "NIL."))

  #+ALLEGRO `(mp:process-sleep ,seconds ,whostate)
  #+LUCID   `(lcl:process-sleep ,seconds ,whostate)
  )

(defmacro proc~wait (whostate function &rest args)
;  (declare (edited  "09-MAR-2000")
;	   (authors sorge ?)
;	   (input   "stream-or-fd: a stream or a file-descriptor"
;		    "whostate: specifies a string that will be displayed as the state of the"
;		    "process"
;		    "function: the wait function is used by the scheduler to test whether the"
;		    "process should run.")
;	   (effect  "Suspends the process until function returns a non-nil value when"
;		    "applied to args.")
;	   (value   "The non-nil value."))

  
  
  #+ALLEGRO `(mp:process-wait ,whostate ,function ,@args)
  #+LUCID   `(lcl:process-wait ,whostate ,function ,@args)
  )

(defmacro proc~socket-wait-for-input-available (socketname whostate)

  ;;; Edited  : 12. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  #+(not (and allegro-version>= (version>= 5 0)))
  `(mp:wait-for-input-available (socket~fd ,socketname) :whostate ,whostate)
  #+(and allegro-version>= (version>= 5 0))
  `(mp:wait-for-input-available (socket~find-socket ,socketname) :whostate ,whostate)
  #+LUCID   `(lcl:process-wait ,whostate #'(lambda () (socket~receives? ,socketname)))
  )

(defmacro proc~wait-with-timeout (whostate seconds function &rest args)
;  (declare (edited  "10-MAR-2000")
;	   (authors sorge ?)
;	   (input   "seconds: the number of seconds to wait"
;		    "whostate: specifies a string that will be displayed as the state of the process"
;		    "function: the wait function is used by the scheduler to test whether the process should run"
;		    "args:     list of arguments to the wait function")
;	   (effect  "Suspends the process until the function argument returns a non-nil value or the time is out.")
;	   (value   "The non-nil value or Nil, if timeout."))

 
  #+ALLEGRO `(mp:process-wait-with-timeout ,whostate ,seconds ,function ,@args)
  #+LUCID   `(lcl:process-wait-with-timeout ,whostate ,seconds ,function ,@args)
  )


(defun proc~activate (process)
;  (declare (edited  "10-MAR-2000")
;	   (authors Sorge ?)
;	   (input   "A process.")
;	   (effect  "A inactive process is activated.")
;	   (value   "The activated process."))

  #+ALLEGRO (mp:process-enable process)
  #+LUCID   (lcl:activate-process process)
  )


(defun proc~deactivate (process)
;  (declare (edited  "10-MAR-2000")
;	   (authors Sorge ?)
;	   (input   "A process.")
;	   (effect  "A active process is deactivated.")
;	   (value   "The deactivated process."))

 
  #+ALLEGRO  (mp:process-disable process)
  #+LUCID    (deactivate-process process)
  )


(defmacro proc~interrupt (process function &rest args)
;  (declare (edited  "10-MAR-2000")
;	   (authors Sorge ?)
;	   (input   "process:  a process"
;		    "function: a function"
;		    "args:     the arguments of function")
;	   (effect  "The execution of process is interrupted and the specified function argument is invoked."
;		    "The process resumes its previous computation when the interruption returns.")
;	   (value   "The interrupted process"))

   #+ALLEGRO  `(mp:process-interrupt ,process ,function ,@args)
  #+LUCID    `(interrupt-process ,process ,function ,@args)
  )


(defun proc~kill (process)
;  (declare (edited  "10-MAR-2000")
;	   (authors Sorge)
;	   (input   "A process.")
;	   (effect  "The process is killed. The process cannot be restarted.")
;	   (value   "The killed process"))

  
  #+ALLEGRO   (mp:process-kill process)
  #+LUCID     (kill-process process)
  )


(defmacro proc~with-process-lock (lock &body body)

  ;;; Edited  : 10. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  #+ALLEGRO   `(mp:with-process-lock ,lock :norecursive t (progn ,@body))
  #+LUCID     `(with-process-lock ,lock (progn ,@body))
  )


(defun proc~new-priority (process priority)
;  (declare (edited  "26-NOV-1999")
;	   (authors Sorge)
;	   (input   "A process and an integer.")
;	   (effect  "Sets the PROCESS priority to the PRIORITY.")
;	   (value   "Undefined."))
  #+ALLEGRO   (setf (mp:process-priority process) priority))

(defun proc~priority (process)
  (mp:process-priority process))

(defun proc~whostate (process)
  (mp:process-whostate process))

(defsetf proc~resume-hook (process) (function)
  `(setf (mp:process-resume-hook ,process) ,function))

(defun proc~resume-hook (process)
  (mp:process-resume-hook process))

(defun proc~name (process)
  (mp:process-name process))

(defun proc~property-list (process)
  (mp:process-property-list process))

(defsetf proc~property-list (process) (value)
  `(setf (mp:process-property-list ,process) ,value))

(defun proc~arrest-reasons (process)
  (mp:process-arrest-reasons process))

(defun proc~add-arrest-reason (process reason)
  (mp:process-add-arrest-reason process reason))

(defun proc~revoke-arrest-reason (process reason)
  (mp:process-revoke-arrest-reason process reason))

(defun proc~make-process-lock ()
  (mp:make-process-lock))

(defun proc~p (obj)
  (mp:process-p obj))
