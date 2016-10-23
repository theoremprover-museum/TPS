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

(deffile tps3-error
  (part-of etps-events)
  (extension lisp)
  (mhelp "Error-handling routines for various implementations of lisp."))

(defvar *trap-errors* nil)

(eval-when (load compile eval)
(defmacro deftrap (function lambda-list message-form)
  `(progn
    (eval-when (load eval)
      (unless (get ',function 'original-function)
	(setf (get ',function 'original-function)
	      (symbol-function ',function))))
    (defun ,function ,lambda-list
      (declare (special *trap-errors*))
      (if *trap-errors*
	  (progn ,message-form
		 (throw-^G))
	  (apply (get ',function 'original-function)
		 ,@(remove '&rest lambda-list))))))
)

;;; In ibcl and CMU CL, error's first arg need not be a string, could be condition,
;;; symbol, etc.  Part of the new standard for error handling. DAN
#-(or ibcl :sbcl :cmu :allegro :lcl3.0)
(deftrap error (format-string &rest args)
  (progn (format *error-output* "~%Common Lisp error trapped.~%")
	 (let ((error-string (apply #'format nil format-string args)))
	   (princ error-string *error-output*)
	   (signal-event 'error error-string))))

;#+(or ibcl :cmu)
;(deftrap error ( &rest args)
;  (progn (format *error-output* "~%Common Lisp error trapped.~%")
;	 (signal-event 'error "Common Lisp error trapped.")))

#+(or ibcl :sbcl :cmu :lcl3.0)
(deftrap error ( first-arg &rest args)
  (progn (format *error-output* "~%Common Lisp error trapped.~%")
         (let ((error-string
                (cond ((stringp first-arg)
                       (apply #'format nil first-arg args))
                      ((symbolp first-arg)
                       (symbol-name first-arg))
                      (t ""))))
           (princ error-string *error-output*)
           (signal-event 'error "Common Lisp error trapped."))))


#-(or ibcl :sbcl :cmu :allegro :lcl3.0)
(deftrap cerror (continue-format-string error-format-string &rest args)
  (progn (format *error-output* "~%Common Lisp correctable error trapped.~%")
	 (let ((error-string (apply #'format nil error-format-string args)))
	   (princ error-string *error-output*)
	   (signal-event 'error error-string))))

#+(or ibcl :sbcl :cmu :lcl3.0)
(deftrap cerror (continue-format-string datum &rest args)
  (progn (format *error-output* "~%Common Lisp correctable error trapped.~%")
	 (let ((error-string (apply #'format nil continue-format-string args)))
	   (princ error-string *error-output*)
	   (signal-event 'error error-string))))


#+tops20
(deftrap lisp::internal-break-loop (&rest args)
  (progn (format *error-output* "~%Not entering break loop.~%")
	 (signal-event 'error "Break Loop.  Probably unbound variable or undefined function.")))

#+kcl
(deftrap si::universal-error-handler (&rest args)
  (progn (format *error-output* "~%Not entering break loop.~%")
	 (signal-event 'error "Break Loop.  Probably unbound variable or undefined function.")))

#+(or :cmu)
(deftrap debug::debug-loop (&rest args)
  (progn (format *error-output* "~%Not entering break loop.~%")
	 (signal-event 'error "Break Loop.  Probably unbound variable or undefined function.")))

#+(or :sbcl)
(deftrap sb-debug::invoke-debugger (&rest args)
  (progn (format *error-output* "~%Not entering break loop.~%")
	 (signal-event 'error "Break Loop.  Probably unbound variable or undefined function.")))

#+ibcl
(deftrap debugger::debugger-command-level (&rest args)
  (progn (format *error-output* "~%Not entering break loop.~%")
	 (signal-event 'error "Break Loop.  Probably unbound variable or undefined function.")))

;#+ibcl
;(deftrap debugger::debug (&rest args)
;  (progn (format *error-output* "~%Not entering break loop.~%")
;	 (signal-event 'error "Break Loop.  Probably unbound variable or undefined function.")))

#+ibcl
(deftrap debugger::debug (first-arg &rest args)
  (progn (let ((error-string (cond ((stringp first-arg)
                                    (apply #'format nil first-arg args))
                                   ((symbolp first-arg)
                                    (symbol-name first-arg))
                                   (t ""))))
           (princ error-string *error-output*)
;           (format *error-output* "~%Not entering break loop.~%")
	 (signal-event 'error "Break Loop.  Probably unbound variable or undefined function."))))


;;; This is called by hitting a control-c.  Don't want to signal an error.
#+ibcl
(deftrap cond::invoke-debugger (&rest args)
  (progn (format *error-output* "~%Caught a CTRL-C.  Aborting.~%")
;	 (signal-event 'error "Break Loop.  Probably unbound variable or undefined function.")
))


;#+ibcl
;(deftrap si::universal-error-handler (error-name correctable function-name continue-format-string error-format-string &rest args)
;  (progn (format *error-output* "~%Not entering break loop.~%")
;	 (let ((error-string 
;		 (if correctable 
;		     (apply #'format nil continue-format-string args))))
;	 (signal-event 'error 
;		       (or error-string 
;			   "Break Loop.  Probably unbound variable or undefined function.")))))

;;;Gives a better error message than the above.
#+ibcl
(deftrap si::universal-error-handler (error-name correctable function-name continue-format-string error-format-string &rest args)
  (progn 
	 (let ((error-string 
		 (if correctable 
		     (apply #'format nil continue-format-string args)
                     (apply #'format nil
                            (conc-strings "Error in ~
                                           function: ~A. " 
                                          error-format-string)
                            function-name args))))
           (princ error-string *error-output*)
           ;(format *error-output* "~%Not entering break loop.~%")
           (signal-event 'error 
		       (or error-string 
			   "Break Loop.  Probably unbound variable or undefined function.")))))


#+:allegro
(excl:advise error :around check-for-trapped-errors nil
  (if *trap-errors* 
      (progn 
	(terpri *error-output*)
	(format *error-output* "~%Common Lisp error trapped.~%")
	(signal-event 'error "Common Lisp error trapped.")
	(throw-^g))
      :do-it))

#+:allegro
(excl:advise cerror :around check-for-trapped-errors nil
  (if *trap-errors* 
      (progn 
	(terpri *error-output*)
	(format *error-output* "~%Common Lisp cerror trapped.~%")
	(signal-event 'error "Common Lisp cerror trapped.")
	(throw-^g))
      :do-it))

;allegro version used to be as follows, which crashed on ^C...
#+comment
(excl:advise cerror :around check-for-trapped-errors nil
  (if *trap-errors* 
      (let ((error-string (apply #'format nil (cadr excl:arglist)
				 (cddr excl:arglist))))
	(terpri *error-output*)
	(format *error-output* error-string)				 
	(format *error-output* "~%Common Lisp cerror trapped.~%")
	(signal-event 'error "Common Lisp cerror trapped.")
	(throw-^g))
      :do-it))



