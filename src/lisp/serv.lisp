;;; -*- Mode: KEIM; Base: 10; Syntax: Common-lisp; Package: OMEGA -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
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

(in-package :auto)

; (mod~defmod serv
; 	    :uses (mod proc socket)
; 	    :documentation "An interface to the distributed services."
; 	    :exports  (serv~available
;                        serv~enter
;                        serv~apply
;                        serv~leave
;                        serv~leave-all
;                        serv~restart
		       
;                        serv~default-timeout-available
;                        serv~default-timeout-enter
;                        serv~default-timeout-apply
;                        serv~default-timeout-leave
;                        serv~set-default-timeout-available
;                        serv~set-default-timeout-enter
;                        serv~set-default-timeout-apply
;                        serv~set-default-timeout-leave
                       
;                        serv~ok?
;                        serv~error?
;                        serv~error-number
;                        serv~error-message
;                        serv~last-sent
;                        serv~interface-error?
;                        serv~service-error?
;                        serv~service-error-number
                    
;                        serv~signal-errors-on  ; this is the default
;                        serv~signal-errors-off 
                       
;                        serv~signal-service-errors-on    
;                        serv~signal-service-errors-off ; this is the default
                       
;                        serv~debug-on   ; for testing purposes only
;                        serv~debug-off
                       
;                        serv~browse-on  ; should only be used
;                        serv~browse-off ; during development of services
;                        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;    errors    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar serv*error '(0 "Dummy-Answer-String" "noMethodHasBeenSentYet"))
(defvar serv*signal-errors T)            
(defvar serv*signal-service-errors nil)

(defvar serv*default-timeout-available nil)
(defvar serv*default-timeout-enter     nil)
(defvar serv*default-timeout-apply     nil)
(defvar serv*default-timeout-leave     nil)

(defun serv~default-timeout-available () serv*default-timeout-available)
(defun serv~default-timeout-enter     () serv*default-timeout-enter)
(defun serv~default-timeout-apply     () serv*default-timeout-apply)
(defun serv~default-timeout-leave     () serv*default-timeout-leave)

(defun serv~set-default-timeout-available (seconds)
  (setq serv*default-timeout-available seconds))
(defun serv~set-default-timeout-enter (seconds)
  (setq serv*default-timeout-enter seconds))
(defun serv~set-default-timeout-apply (seconds)
  (setq serv*default-timeout-apply seconds))
(defun serv-set-default-timeout-leave (seconds)
  (setq serv*default-timeout-leave seconds))
  
;;; interface-errors are signaled iff serv*signal-errors is not nil.
;;; service-errors   are signaled iff serv*signal-errors is not nil and
;;;                                   serv*signal-service-errors is not nil.


(defun serv~error-number ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "None.")
;           (value   "The error code of the last service-operation. "
;                    "If the operation went allright, the error code is 0."))
  (cadr serv*error))

(defun serv~error-message ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "None.")
;           (value   "The error message of the last service-operation. "
;                    "If the operation went allright, the error message is "
;                    "the answer string."))
  (caddr serv*error))

(defun serv~last-sent ()
;  (declare (edited  "22-JUN-1999")
;           (authors afranke)
;           (input   "None.")
;           (effect  "None.")
;           (value   "A string containing the Oz-method that was sent during "
;                    "the last service-operation. "
;                    "If the result of that service-operation was an error, "
;                    "this is the method that caused it. "))
  (car serv*error))

(defun serv~ok? ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "None.")
;           (value   "T iff the last service-operation went allright."))
  (zerop (serv~error-number)))

(defun serv~error? ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "None.")
;           (value   "T iff the last service-operation returned an error. "))
  (not (zerop (serv~error-number))))

(defun serv~interface-error? ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "None.")
;           (value   "T iff the last service-operation returned an "
;                    "interface error."))
  (< (serv~error-number) 100))

(defun serv~service-error? ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "None.")
;           (value   "T iff the last service-operation returned a "
;                    "service error."))
  (>= (serv~error-number) 100))

(defun serv~service-error-number ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "None.")
;           (value   "The original service-error-number. "))
  (- (serv~error-number) 100))

(defun serv~signal-errors-on ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "serv*signal-errors is set to T.")
;           (value   "T."))
  (setq serv*signal-errors T)
  T)

(defun serv~signal-errors-off ()
  (declare (special serv*signal-interface-errors))
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "serv*signal-errors is set to nil.")
;           (value   "T."))
  (setq serv*signal-interface-errors nil)
  T)

(defun serv~signal-service-errors-on ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "serv*signal-service-errors is set to T.")
;           (value   "T."))
  (setq serv*signal-service-errors T)
  T)

(defun serv~signal-service-errors-off ()
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "serv*signal-service-errors is set to nil.")
;           (value   "T."))
  (setq serv*signal-service-errors nil)
  T)

(defun serv=signal-error (function-name)
;  (declare (edited  "22-JUN-1999")
;           (authors afranke)
;           (input   "None.")
;           (effect  "Signals an error.")
;           (value   "Nil, if continued."))
  (cerror "let ~A return nil."
          "Function ~A did not operate successfully.~%~A: ~A~%The request was:~% ~A"
          function-name
          (serv~error-number) (serv~error-message) (serv~last-sent)))

(defun serv=signal-service-error (function-name service method)
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None.")
;           (effect  "Signals an error.")
;           (value   "Nil, if continued."))
  (cerror "let ~A return nil."
          "~*Service '~A' could not apply method '~A' successfully.~%~A: ~A"
          function-name service method
          (serv~service-error-number) (serv~error-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;  general operation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv=operation (message)
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "A message (string).")
;           (effect  "Send the message to the :service-socket, read the "
;                    "answer list '(err-number err-message) from the "
;                    ":service-socket and assign it to serv*error.")
;           (value   "The err-message if the err-number is 0, otherwise nil."))
  (socket~write message :service)
  (proc~socket-wait-for-input-available :service "Waiting for the service socket.")
  (setq serv*error (cons message (read-from-string (socket~read :service))))
  (if (serv~ok?) (serv~error-message)
    nil))

;;; Some error codes:
;;; 
;;;   0 <This is not an error code. Everything went allright.> 
;;;   1 Compiler cannot read the message.    
;;;   2 Message must be a record.               
;;;   3 Unknown Command.                       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;  common options  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv=mkopt-at-url (url)
  (cond ((null url) "")
        (T (format nil " atURL:~S" url))))

(defun serv=mkopt-timeout (seconds)
  (cond ((null seconds) "")
        (T (format nil " timeout:~A" seconds))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;   available    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~available (service
                       &key
                       ((:signal-errors signal-errors) serv*signal-errors)
                       ((:at url))
                       ((:timeout seconds) serv*default-timeout-available))
;  (declare (edited  "22-JUN-1999")
;           (authors afranke)
;           (input   "A string naming the requested service. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "none.")
;           (value   "T iff the service is available."))
  (serv=operation
   (format nil "available('~A' ~A ~A)" service
           (serv=mkopt-at-url url) (serv=mkopt-timeout seconds)))
  (cond ((serv~ok?) (read-from-string (serv~error-message)))
        (signal-errors (serv=signal-error "serv~available"))
        (T nil)))

;;; Some error codes:
;;;
;;;  50 Error using command 'available'.
;;;  51 Timeout.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   enter    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~enter (service
                   &key
                   ((:signal-errors signal-errors) serv*signal-errors)
                   ((:at url))
                   ((:timeout seconds) serv*default-timeout-enter))
;  (declare (edited  "22-JUN-1999")
;           (authors afranke)
;           (input   "A string naming the requested service. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "Provides the service, if possible. "
;                    "Eventually an error is signaled.")
;           (value   "T iff the operation was successful."))
  (serv=operation
   (format nil "enter('~A' ~A ~A)" service
           (serv=mkopt-at-url url) (serv=mkopt-timeout seconds)))
  (cond ((serv~ok?) T)
        ((= (serv~error-number) 11) T)
        (signal-errors (serv=signal-error "serv~enter"))
        (T nil)))

;;; Some error codes:
;;;
;;;  10 Error using command 'enter'.
;;;  11 Service already open.                  
;;;  12 Time out while creating ServiceObject. 
;;;  13 Service unknown.                        
;;;  14 Time out while entering service.        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   leave    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~leave (service
                   &key
                   ((:signal-errors signal-errors) serv*signal-errors)
                   ((:timeout seconds) serv*default-timeout-leave))
;  (declare (edited  "22-JUN-1999")
;           (authors afranke)
;           (input   "A string naming the service. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "Closes the service, if possible. "
;                    "Eventually an error is signaled.")
;           (value   "T iff the operation was successful."))
  (serv=operation
   (format nil "leave('~A' ~A)" service
           (serv=mkopt-timeout seconds)))
  (cond ((serv~ok?) T)
        ((= (serv~error-number) 21) T)
        (signal-errors (serv=signal-error "serv~leave"))
        (T nil)))

;;; Some error codes:
;;;
;;;  20 Error using command 'leave'.
;;;  21 Service not open.                       
;;;  22 Time out while leaving service.           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;   restart   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~restart (service
                     &key
                     ((:signal-errors signal-errors) serv*signal-errors)
                     ((:timeout secs)))
;  (declare (edited  "22-JUN-1999")
;           (authors afranke)
;           (input   "A string naming the service. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "Restarts the service, if possible. "
;                    "Eventually an error is signaled.")
;           (value   "T iff the operation was successful."))
  (let ((tleave (cond ((null secs) serv*default-timeout-leave) (T secs)))
        (tenter (cond ((null secs) serv*default-timeout-enter) (T secs))))
    (serv~leave service :signal-errors signal-errors :timeout tleave)
    (serv~enter service :signal-errors signal-errors :timeout tenter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   leave-all   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~leave-all (&key
                       ((:signal-errors signal-errors) serv*signal-errors)
                       ((:timeout seconds) serv*default-timeout-leave))
;  (declare (edited  "22-JUN-1998")
;           (authors afranke)
;           (input   "None. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "Closes all services, if possible. "
;                    "Eventually an error is signaled.")
;           (value   "T iff the operation was successful."))
  (serv=operation
   (format nil "close(~A)" (serv=mkopt-timeout seconds)))
  (cond ((serv~ok?) T)
        (signal-errors (serv=signal-error "serv~leave-all"))
        (T nil)))

;;; Some error codes:
;;;
;;;  30 Error using simple command.              
;;;  40 Time out while leaving service.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   apply    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~apply (service method
                           &key
                           ((:signal-errors signal-errors) serv*signal-errors)
                           ((:signal-service-errors signal-service-errors)
                            serv*signal-service-errors)
                           ((:timeout seconds) serv*default-timeout-apply))
;  (declare (edited  "22-JUN-1999")
;           (authors afranke)
;           (input   "Two strings. The first is the service-name, the second "
;                    "describes the Oz-method that will be sent to it. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "Performs the service-operation, if possible. "
;                    "Eventually an error is signaled.")
;           (value   "The answer string if the operation was successful, "
;                    "otherwise nil."))
  (serv=operation
   (format nil "applyMethod('~A' ~A ~A)" service method
           (serv=mkopt-timeout seconds)))
  (cond ((serv~ok?) (serv~error-message))
        (signal-errors
         (cond ((serv~interface-error?)
                (serv=signal-error "serv~apply"))
               (signal-service-errors
                (serv=signal-service-error "serv~apply" service method))))
        (T nil)))

;;; Some error codes:
;;;
;;;  90 Error using command 'applyMethod'.
;;;  91 Service not open.                        
;;;  92 Error while executing 'applyMethod'.    
;;;  93 Time out while applying method.          
;;;  94 Result is not a det value.                
;;;  95 Result is not a VirtualString.           
;;;  96 User defined error has illegal structure.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   debug-on    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~debug-on (&key ((:signal-errors signal-errors)
                            serv*signal-errors))
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "Switch on debugging. "
;                    "Eventually an error is signaled.")
;           (value   "T iff the operation was successful."))
  (serv=operation "debugOn")
  (cond ((serv~ok?) T)
        (signal-errors (serv=signal-error "serv~debug-on"))
        (T nil)))

;;; Some error codes:
;;;
;;;  30 Error using simple command.              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   debug-off   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~debug-off (&key ((:signal-errors signal-errors)
                             serv*signal-errors))
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "Switch off debugging. "
;                    "Eventually an error is signaled.")
;           (value   "T iff the operation was successful."))
  (serv=operation "debugOff")
  (cond ((serv~ok?) T)
        (signal-errors (serv=signal-error "serv~debug-off"))
        (T nil)))

;;; Some error codes:
;;;
;;;  30 Error using simple command.              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   browse-on   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~browse-on (&key ((:signal-errors signal-errors)
                             serv*signal-errors))
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "If a result is not a VirtualString, "
;                    "it will be displayed in the Browser Window. "
;                    "Eventually an error is signaled.")
;           (value   "T iff the operation was successful."))
  (serv=operation "browseResultOn")
  (cond ((serv~ok?) T)
        (signal-errors (serv=signal-error "serv~browse-on"))
        (T nil)))

;;; Some error codes:
;;;
;;;  30 Error using simple command.              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;   browse-off  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun serv~browse-off (&key ((:signal-errors signal-errors)
                              serv*signal-errors))
;  (declare (edited  "02-SEP-1998")
;           (authors afranke)
;           (input   "None. "
;                    "An optional key argument indicates if errors will be "
;                    "signaled.")
;           (effect  "If a future result is not a VirtualString "
;                    "it will produce the error-code 95 (see apply). "
;                    "Eventually an error is signaled.")
;           (value   "T iff the operation was successful."))
  (serv=operation "browseResultOff")
  (cond ((serv~ok?) T)
        (signal-errors (serv=signal-error "serv~browse-off"))
        (T nil)))

;;; Some error codes:
;;;
;;;  30 Error using simple command.              
















