;;; -*- syntax: common-lisp; package: TPS; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     TPS Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The new TPS architecture using LISP multiprocessing
;; =====================================================
;;
;; TPS consists of four independent processes:
;; - The TPS core process that executes commands
;; - The TPS command interpreter which accepts command input
;;   from the regular ascii interface
;; - The MathWeb listener which handles the incoming commands from MathWeb clients
;; - And the service listener for communication with Mathweb services
;;
;;  The TPS core process coordination between the other processes.
;;  It especially maintains a blackboard with requests for command
;;  executions and performs these sequentially.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :AUTO)

;;; The following functions are internal in other modules and should not be used:
;;; (tps=output tps=str2msg tps=write2loui)

(eval-when (load compile eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   The class of core blackboards 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defclass tpr+core-blackboard () 
 ((command-queue :accessor tpr~bb-command-queue
		  :initarg :command-queue
		  :initform nil
		  :documentation "A priority queue of command execution requests."))
  (:documentation "The class of blackboards the TPS core process handles commands with."))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Global Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mathweb-service-mode* nil "Indicates whether TPS is a MathWeb service or runs alone.")

(defvar tpr*active nil "TPS multiprocessing active or not.")
(defvar tpr*on-loui T "TPS is connected to the LOUI interface or not.")
(defvar tpr*not-killed nil "TPS multiprocessing has been intentionally killed or not.")

(defvar tpr*core-blackboard nil "The anchor for the core blackboard maintaining the commands.")

(defvar tpr*lisp-listener nil "Accesses the lisp listener.")
(defvar tpr*tps-core-process nil "Accesses the core process.")
(defvar tpr*mathweb-listener nil "Accesses the loui socket listener.")
(defvar tpr*service-listener nil "Accesses the mathweb socket listener.")

(defvar tpr*quit nil "Quitting multiprocessing.")
(defvar tpr*reset nil "Reseting multiprocessing.")

(defvar tpr*interrupt-command :interrupt
  "The command used for interrupting arbitary computations.")

(defvar tpr*halt-lisp-listener '(:lisp :exit :exit-lisp :break)
  "A list of commands where the ascii-interface should wait for it to finish (must not include interrupt).")
(defvar tpr*halt-mathweb-listener
  '(:socketread-problem :socketread-pds :socketread-resolution-proof :socketload-file :receive-atp-out)
  "A list of commands where the mathweb-listener should wait for it to finish.")

(defvar tpr*halt-service-listener '(:plan :nsp
					  :s
					  :search
					  :show-constraints
					  :read-problem :execute-theory-log :execute-theory-log*
					  :solve-analogous-to-proof :solve-analogous-to-node)
  "A list of commands where the mathweb-listener should wait for it to finish.")
(defvar tpr*runtime-object nil "The last runtime-object saved.")

(defvar tpr*lock (proc~make-process-lock) "A lock for protecting operations of the core process")

(defvar tpr*listener-names '(:inout :service)
  "The names of the currently used listeners: :inout is for external calls from other programs.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  The socket listener processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;; Querying and such

(defmacro tpr~mathweb-listener-p (process)
  `(and tpr*mathweb-listener (eq ,process tpr*mathweb-listener)))

(defmacro tpr~service-listener-p (process)
  `(and tpr*service-listener (eq ,process tpr*service-listener)))

(defun tpr~socket2process (socketname)
  (cond ((string-equal socketname :inout) tpr*mathweb-listener)
	((string-equal socketname :service) tpr*service-listener)
	(t (tps-warning "Unknown socket:" socketname))))

(defun tpr~process2socket (process)
  (cond ((tpr~mathweb-listener-p process) :inout)
	((tpr~mathweb-listener-p process) :http)
	((tpr~service-listener-p process) :service)))

;;; Handling and maintaining
(defun tpr=reset-socket-listeners ()
;  (declare (edited  "13-DEC-1999")
;	   (authors Sorge)
;	   (input   "None.")
;	   (effect  "Resets the socket listeners.")
;	   (value   "Undefined."))
  (when tpr*mathweb-listener
    (tpr~listener-stop tpr*mathweb-listener)
    (setf tpr*mathweb-listener (tpr=listener-create :inout)))
  (when tpr*service-listener
    (tpr~listener-stop tpr*service-listener)
    (setf tpr*service-listener (tpr=listener-create :service))))

(defun tpr~listener-stop (listener)
;  (declare (edited  "13-DEC-1999")
;	   (authors Sorge)
;	   (input   "None.")
;	   (effect  "Stops the socket listeners.")
;	   (value   "Undefined."))
  (when (proc~p listener) (proc~kill listener)))

(defun tpr=listener-create (socketname &optional (func 'tpr=listener-function))
;  (declare (edited  "13-DEC-1999")
;	   (authors Sorge)
;	   (input   "A socketname.")
;	   (effect  "Creates a process listening on the socket specified by socketname.")
;	   (value   "The listener process."))
  (proc~create :name (format nil "Listener (~A)" socketname)
	       :function func
	       :priority 200
	       :args (socketname)))

(defun tpr=listener-function (socketname)
;  (declare (edited  "13-DEC-1999")
;	   (authors Sorge)
;	   (input   "A socketname.")
;	   (effect  "Loops for input available at the given socket and passes it on.")
;	   (value   "Undefined."))
  (loop
   (with-simple-restart
    (abort "Return to TPS level")
    (unless (socket~active? socketname)
      (return (tps-warning "Socket " socketname " is no longer active!")))
    (handler-case
     (progn
       (proc~socket-wait-for-input-available socketname
					     (format nil "Waiting for input on socket ~A" socketname))
       (let* ((input_str (socket~read socketname))
	      (input (read-from-string input_str))
	      (process (tpr~socket2process socketname)))
	 (declare (ignore process))
	 ;; do something useful here with the string!
	 ;;==========================================
	 (format t "~%Input on :inout socket: ~% ~A" input)
	 (enqueue-external-request input)
	 ))
     (simple-error (c)
		   (tps-warning c))
     (error (c) 
	    (tps-warning c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A little mechanism to abort arbitrary executions. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define-condition tpr+abort (sys+abort)
;   ((return-object nil)
;    (caller tpr*lisp-listener))
;   #'(lambda (cond stream)
;       (format stream "Execution aborted, the runtime-object ~A was saved." (tpr+abort-return-object cond))))

(defun tpr~signal-interrupt (&optional (object nil))
;  (declare (edited  "10-DEC-1999")
;	   (authors Sorge)
;	   (input   "A runtime-object.")
;	   (effect  "Aborts the current execution if this was signalled by the user.")
;	   (value   "Undefined."))
  (tpr~release-listener)
  (tpr=remove-special-commands tpr*halt-lisp-listener)
  (when (tpr=remove-special-commands tpr*halt-mathweb-listener)
    (socket~read)
    (tpr~release-listener tpr*mathweb-listener))
  (tpr=remove-special-commands tpr*halt-service-listener)
  (let* ((command (tpr=find-interrupt-on-board tpr*core-blackboard))
	 (caller (third command)))
    (when command
      (signal (make-condition 'tpr+abort
				  :return-object object
				  :caller caller
				  )))))

(defun tpr=remove-special-commands (&optional (coms tpr*halt-lisp-listener)
					      (bb tpr*core-blackboard))
  (let* ((change nil)
	 (new-queue
	  (remove-if #'(lambda (x)
			 (let ((command (car x)))
			   (and (com~command-p command)
				(find (keim~name command) coms :test #'string-equal)
				(setf change t))))
		     (tpr~bb-command-queue bb))))
    (when change
      (setf (tpr~bb-command-queue bb) new-queue))
    change))

(defun tpr=find-interrupt-on-board (&optional (bb tpr*core-blackboard))
  (let* ((queue (tpr~bb-command-queue bb))
	 (command (find-if #'(lambda (x)
			       (let ((com (car x)))
				 (and (com~command-p com)
				      (string-equal (keim~name com) tpr*interrupt-command))))
			   queue)))
    (when command
      (setf (tpr~bb-command-queue bb) nil)  ;;(remove command queue))
      command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; OUTPUT Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output redirection for the standard-output stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)

(defmacro tpr=with-output-to-loui (body)
  `(if tpr*on-loui
       (let ((*standard-output* (make-string-output-stream))
	 ;;;(*trace-output* (make-string-output-stream))
	     )
	 ,body
	 (let ((strings1 (atptop~divide-string
			  (get-output-stream-string *standard-output*)
			  #\NEWLINE :handle-break-char 'pre))
	   ;;;(str2 (get-output-stream-string *trace-output*))
	       )
	   (dolist (str1 strings1)
	     (unless (equal str1 "") (socket~write (concatenate 'string "browseOutput(" (write-to-string str1) ")"))))
       ;;;(unless (equal str2 "") (socket~write (concatenate 'string "browseTrace(" (write-to-string str2) ")")))
	   ))
     ,body))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions adapted from the TPS output module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)

(defmacro tps=output (stream)
;  (declare (edited  "03-MAR-1998")
;	   (authors Sorge Konrad)
;	   (input   "A function specifying the interface-stream.")
;	   (effect  "Prints arguments to the specified stream of the TPS interface.")
;	   (value   "NIL"))
  `(let ((interface (if (and (boundp 'comint*current-comint) comint*current-comint)
			(comint~interface comint*current-comint)
		      (asi~create))))
     (,stream interface (apply #'format nil args))
     nil))

(defmacro tps=write2loui (label)
  `(let ((str (format nil "~A~%" (apply #'format nil args))))
     (socket~write  (tps=str2msg ,label str) :inout)
  nil)))

(defun tps=str2msg (label str)
  (concatenate 'string  label "(" (write-to-string str) ")"))

(defun tps~output (&rest args)
  (format t "ERROR: ~A" args)
  )
;   (cond ((tpr~mathweb-listener-p tpr*calling-listener)
; 	 (tps=write2loui "browseOutput"))
; 	(t (tps=output inter~output-object))))

;(defun tps~message (&rest args)
;  (format t "MESSAGE: ~A" args)
;  )
;   (cond ((tpr~mathweb-listener-p tpr*calling-listener)
; 	 (tps=write2loui "browseMessage"))
; 	(t (tps=output inter~print-message))))
  
;(defun tps~warn (&rest args)
;  (format t "WARNING: ~A" args)
;  )
;   (cond ((tpr~mathweb-listener-p tpr*calling-listener)
; 	 (tps=write2loui "browseWarning"))
; 	(t (tps=output inter~print-warning))))
		         
;(defun tps~error (&rest args)
;  (format t "ERROR: ~A" args)
;  )
;   (cond ((tpr~mathweb-listener-p tpr*calling-listener)
; 	 (tps=write2loui "browseError"))
; 	(t
; 	 (let ((interface (if (and (boundp 'comint*current-comint) comint*current-comint)
; 			      (comint~interface comint*current-comint)
; 			    (asi~create))))
;     (cond ((typep (car args) 'condition)
; 	   (inter~print-error interface (car args)))
; 	  ((= (length args) 1) 
; 	   (inter~print-error interface (string (car args))))
; 	  (T (inter~print-error interface (apply #'format nil args))))
; 	   nil))))

(defun tps~trace (&rest args)
  (format t "TRACE: ~A" args)
  )
;   (cond ((tpr~mathweb-listener-p tpr*calling-listener)
; 	 (tps=write2loui "browseTrace"))
; 	(t (tps=output inter~print-trace))))

  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locking the Listeners
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tpr~arrest-listener (&optional (listener tpr*lisp-listener))
  (when (proc~p listener)
    (proc~add-arrest-reason listener :send-command)))

(defun tpr~release-listener (&optional (listener tpr*lisp-listener))
  (when (and (proc~p listener) (proc~arrest-reasons listener))
    (proc~revoke-arrest-reason listener :send-command)))


; (defgeneric tpr~arrest-service (command)
;   (:method ((command com+command))
; 	   (tpr~arrest-service (keim~name command)))
;   (:method ((command string))
; 	   (when (and tpr*service-listener
; 		      (proc~is-active tpr*service-listener)
; 		      (find command tpr*halt-service-listener :test #'string-equal))
; 	     (tpr~arrest-listener tpr*service-listener)))
;   (:method ((command symbol))
; 	   (when (and tpr*service-listener
; 		      (proc~is-active tpr*service-listener)
; 		      (find command tpr*halt-service-listener :test #'string-equal))
; 	     (tpr~arrest-listener tpr*service-listener))))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; Handling the core blackboard
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defun tpr~create-core-blackboard (commands)
;   (make-instance 'tpr+core-blackboard :command-queue commands))

; (defun tpr~make-core-blackboard ()
;   (setf tpr*core-blackboard (tpr~create-core-blackboard nil)))

; (defun tpr~reset-core-blackboard ()
;   (declare (edited  "13-DEC-1999")
; 	   (authors Sorge)
; 	   (input   "None.")
; 	   (effect  "Resets the core blackboard, i.e. removes all its command entries.")
; 	   (value   "Undefined."))
;   (proc~with-process-lock (tpr*lock)
; 			  (setf (tpr~bb-command-queue tpr*core-blackboard) nil)))

; (defun tpr~enqueue-command (command &optional (bb tpr*core-blackboard))
;   (declare (edited  "08-DEC-1999")
; 	   (authors Sorge)
; 	   (input   "A list representing a normalized command (see tpr~normalize-command) and the core blackboard.")
; 	   (effect  "Enqueues the command in the blackboard priority queue of commands.")
; 	   (value   "Undefined."))
;   (when bb
;     (proc~with-process-lock (tpr*lock)
; 			    (setf (tpr~bb-command-queue bb)
; 				  (append (tpr~bb-command-queue bb) (list command))))))

; (defgeneric tpr~normalize-command (command &key (args nil) (status nil) (process nil))
;   (declare (edited  "08-DEC-1999")
; 	   (authors Sorge)
; 	   (input   "A command, a list of arguments and a symbol and a process name.")
; 	   (effect  "None.")
; 	   (value   "The normalized representation for the core blackboard:"
; 		    "(command args calling-process status)."
; 		    "If status is NIL the command can be executed, if T it still waits for"
; 		    "some arguments to be completed."))
;   (:method ((command string) &key (args nil) (status nil) (process nil))
; 	   (tpr~normalize-command (bb~find-command command) :args args :status status :process process))
;   (:method ((command symbol) &key (args nil) (status nil) (process nil))
; 	   (tpr~normalize-command (bb~find-command command) :args args :status status :process process))
;   (:method ((command com+command) &key (args nil) (status nil) (process nil))
; 	   (list command args process status)))

; (defun tpr=command-executable-p (command)
;   (declare (edited  "08-DEC-1999")
; 	   (authors Sorge)
; 	   (input   "A core bb command entry.")
; 	   (effect  "None.")
; 	   (value   "T if the command can be executed right away."))
;   (not (fourth command)))

; (defun tpr~pop-command (&optional (bb tpr*core-blackboard))
;   (declare (edited  "08-DEC-1999")
; 	   (authors Sorge)
; 	   (input   "Returns the first executable command on the blackboard, if there is one.")
; 	   (effect  "Can remove a command entry from BB.")
; 	   (value   "An executable command or NIL."))
;   (when bb
;     (proc~with-process-lock (tpr*lock)
; 			    (let* ((queue (tpr~bb-command-queue bb))
; 				   (command (find-if #'tpr=command-executable-p queue)))
; 			      (setf (tpr~bb-command-queue bb) (remove command queue))
; 			      command))))
  

; (defun tpr~execute-command (command)
;   (let* ((com (first command))
; 	 (args (second command))
; 	 (caller (third command))
; 	 (tpr*calling-listener caller))
;     (tpr~arrest-service com)
;     (unwind-protect
; 	(cond ((and (symbolp caller)(string<
; 				     (string-downcase "httpclient")
; 				     (string-downcase (string caller)))) ;;this is ugly, todo MP
; 	       (handler-case
; 		(let ((result (list (apply (com~function com) args))))
; 		  (http~send-response caller                       
; 				      (rpc~compose-methodresponse result) 
; 				      :type "text/xml")                   
; 		  (socket~close caller)                            
; 		  (socket~delete caller))
; 		(error (c)
; 		       (progn (http~send-response caller
; 						  (rpc~compose-methodresponse (list "error") :fault T)
; 						  :type "text/xml")
; 			      (socket~close caller)
; 			      (socket~delete caller)
; 			      (tps~error "~A" c)))))
; 	      ((and (tpr~lisp-listener-p caller)
; 		    (or (find (keim~name com) tpr*halt-lisp-listener :test #'string-equal)
; 			(comint~debugger-on-break comint*current-comint)))
; 	       (unwind-protect
; 		   (apply (com~function com) args)
; 		 (setf tpr*active nil)
; 		 (tpr~release-listener))
; 	       (setf tpr*active t))
; 	      ((tpr~lisp-listener-p caller)
; 	       (tpr~release-listener)
; 	       (apply (com~function com) args))
; 	      ((and (tpr~mathweb-listener-p caller)
; 		    (comint~debugger-on-break comint*current-comint))
; 	       (tpr~arrest-listener)
; 	       (tpr=with-output-to-loui
; 		(unwind-protect
; 		    (apply (com~function com) args)
; 		  (setf tpr*active nil)
; 		  (tpr~release-listener)
; 		  (tpr~release-listener tpr*mathweb-listener))))
; 	      ((tpr~mathweb-listener-p caller)
; 	       (tpr=with-output-to-loui
; 		(unwind-protect
; 		    (apply (com~function com) args)
; 		  (tpr~release-listener tpr*mathweb-listener))))
; 	      (t (apply (com~function com) args)))
;       (tpr~release-listener tpr*service-listener))))

; ;;   (simple-error (c) (tps~error "Command could not be applied:~%        ~A" c))
; ;;   (error (c) (tps~error "Command could not be applied:~%    ~A " c))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Querying the Mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tpr~active-p () tpr*active)

(defun tpr~runtime-object () tpr*runtime-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Handling the core process for command execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun tpr~reset ()
  (setf tpr*reset (not tpr*reset)))

(defun tpr~quit ()
  (setf tpr*active nil)
  (setf tpr*not-killed nil)
  (setf tpr*quit t))

(defun tpr~start ()
  (setf tpr*quit nil)
  (setf tpr*not-killed t)
  (setf tpr*active t)
  (tpr~prepare-shutdown))

; (defun tpr=core-process-run-function ()
;   (let ((reset tpr*reset)
; 	command)
;     (in-package :tps)
;     (loop
;      (with-simple-restart
;       (abort "Return to TPS top level")
;       (if (comint~debugger-on-break comint*current-comint)
; 	  (handler-case
; 	   (progn
; 	     (proc~wait "TPS core process waits for applicable commands."
; 			#'(lambda () (or (not (equal reset tpr*reset))
; 					 tpr*quit
; 					 (setf command (tpr~pop-command)))))
; 	     (cond (tpr*quit
; 		    (return (tps~message "TPS core process is quitting job!")))
; 		   ((not (equal reset tpr*reset))
; 		    (tps~warn "TPS core process resets.")
; 		    (tpr~reset-core-blackboard)
; 		    (setf reset tpr*reset))
; 		   (t (when tpr*mathweb-listener
; 			(socket~write (format nil "indicateBusy(\"~:(~A~)\")" (keim~name (car command))) :inout))
; 		      (unwind-protect
; 			  (tpr~execute-command command)
; 			(when tpr*mathweb-listener (socket~write "indicateIdle" :inout))))))
; 	   (tpr+abort (c) (progn
; 			    (tpr~release-listener)
; 			    (setf tpr*runtime-object (tpr+abort-return-object c))
; 			    (let ((tpr*calling-listener (tpr+abort-caller c)))
; 			      (tps~error "Execution aborted, the runtime-object ~A was saved." (tpr~runtime-object))))))
; 	(handler-case
; 	 (progn
; 	   (proc~wait "TPS core process waits for applicable commands."
; 		      #'(lambda () (or (not (equal reset tpr*reset))
; 				       tpr*quit
; 				       (setf command (tpr~pop-command)))))
; 	   (cond (tpr*quit
; 		  (return (tps~message "TPS core process is quitting job!")))
; 		 ((not (equal reset tpr*reset))
; 		  (tps~warn "TPS core process resets.")
; 		  (tpr~reset-core-blackboard)
; 		  (setf reset tpr*reset))
; 		 (t (when tpr*mathweb-listener
; 		      (socket~write  (format nil "indicateBusy(\"~:(~A~)\")" (keim~name (car command))) :inout))
; 		      (unwind-protect
; 			  (tpr~execute-command command)
; 			(when tpr*mathweb-listener (socket~write "indicateIdle" :inout))))))
; 	 (tpr+abort (c) (progn
; 			  (tpr~release-listener)
; 			  (setf tpr*runtime-object (tpr+abort-return-object c))
; 			  (let ((tpr*calling-listener (tpr+abort-caller c)))
; 			    (tps~error "Execution aborted, the runtime-object ~A was saved." (tpr~runtime-object)))))
; 	 (simple-error (c) (tps~error "~A" c)) 
; 	 (error (c) (tps~error "~A" c))   

; 	 ))))))
;;;))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Initialization of the mechanism 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun tpr~initialize-listeners ()
  (when (tpr~active-p)
    (mapcar #'(lambda (socket)
		(format t "Socket ~A active? : ~A" socket (socket~active? socket))
		(when (socket~active? socket)
		  (tpr=initialize-listener socket)))
	    tpr*listener-names)))

(defgeneric tpr=initialize-listener (socket)
;  (declare (edited  "20-DEC-1999")
;	   (authors Sorge)
;	   (input   "A symbol specifying a socket.")
;	   (effect  "Initializes a listener for that socket.")
;	   (value   "The process running the socket-listener."))
  (:method (socket)
	   (declare (ignore socket)))
  (:method ((socket symbol))
	   t)
  (:method ((socket (eql :inout)))
	   (if (and tpr*mathweb-listener (proc~is-active tpr*mathweb-listener))
	       tpr*mathweb-listener
	     (let ((new-proc (tpr=listener-create socket)))
	       (setf tpr*mathweb-listener new-proc)
	       new-proc)))
  )	   

(defun tpr~initialize-tps ()
  (cond ((and tpr*tps-core-process (proc~is-active tpr*tps-core-process))
	 (tpr~start))
	((and tpr*tps-core-process (not (proc~is-active tpr*tps-core-process)))
	 (tpr~start)
	 (setf tpr*tps-core-process (proc~create :name "TPS-CORE-PROCESS"
						   :priority 100
						   :function #'tpr=core-process-run-function)))
	(t
	 (tpr~make-core-blackboard)
	 (setf tpr*lisp-listener (proc~actual-process))
	 (proc~new-priority tpr*lisp-listener 200)
	 (tpr~start)
	 (setf tpr*tps-core-process (proc~create :name "TPS-CORE-PROCESS"
						   :priority 100
						   :function #'tpr=core-process-run-function)))))

(defun tpr~reset-tps-processes ()
  (msgf "Reseting TPS's processes!!!")
  (tpr=reset-socket-listeners)
;  (tpr~reset-core-blackboard)
;  (tpr~initialize-tps)
  )
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redefining some Mixin Functions. Should be done more cleanly one day
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some modifications to Allegro CL exit functionality

#+ALLEGRO
(eval-when (load compile eval)
  (defun tpr~prepare-shutdown ()
    (setq sys:*exit-cleanup-forms* (list '(format t "Exiting TPS!!!~%")
					 '(format t "Shutting down MathWeb sockets ~%")
					 '(auto::socket~close  :service)
					 '(auto::socket~delete :service)
					 '(auto::socket~close  :inout)
					 '(auto::socket~delete :inout)))
    (tpl::add-new-command "exit" 1 #'(lambda () (excl:exit 0 :no-unwind t)) "Exiting TPS's multiprocessing.")
  ))

; cebrown 8/26/01
; This starts a listener that can be connected to briefly to
; start a tps -service or tps -lservice on the same machine as
; the listener - the idea is to leave this running continually
; on each machine on an agreed upon port.
(defun tps-remote (port)
  (socket~reset)
  (socket~define :listener-pass)
  (socket~bind port :listener-pass)
  (socket~define :listener)
  (socket~define :local-pass)
  (socket~bind-new-port :local-pass)
  (socket~define :local)
  (loop while t do
	(socket~accept :listener-pass :listener)
	(let* ((input_str (socket~read :listener))
	       (input (read-from-string input_str)))
	  (when (and (listp input) (= (length input) 3))
	    #+(and allegro (not mswindows))
	    (excl:run-shell-command (format nil "xtps -- -service ~A ~A ~A"
					    (car input) (cadr input) (caddr input))
				    :wait nil)
	    #+:mswindows
	    (excl:run-shell-command (format nil "C:\\TPS\\tps3 -- -service ~A ~A ~A"
					    (car input) (cadr input) (caddr input))
				    :wait nil)
	    )
	  (when (string-equal input "LOCAL-SERVICE")
	    #+(and allegro (not mswindows))
	    (excl:run-shell-command (format nil "xtps -- -lservice ~A"
					    (socket~port :local-pass))
				    :wait nil)
	    
	    #+:mswindows
	    (excl:run-shell-command (format nil "C:\\TPS\\tps3 -- -lservice ~A"
					    (socket~port :local-pass))
				    :wait nil)
	    (socket~accept :local-pass :local)
	    (socket~write (socket~read :local) :listener)
	    (socket~close :local))
	  (socket~close :listener))))
