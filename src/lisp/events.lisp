;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of EVENTS)

;;;
;;; File : Events
;;; 

(deffile events
  (part-of events)
  (extension clisp)
  (mhelp "Defines functions handling events. Events currently only work
for the non path-focused duplication procedures ms88, ms89 and ms91-6."))

(context tps-events)
			       
;;;(defun signal-event (event &rest event-args)
;;;  (if (or (not (event-p event)) (not (get event 'event-fn)))
;;;      ; Event undefined or not initialized - return NIL: unsuccessful.
;;;      nil
;;;      (apply (get event 'event-fn) event-args)))

(defun signal-event (event &rest event-args)
  (when (and  events-enabled (event-p event) (get event 'event-fn))
    ;; Event defined and initialized
    (apply (get event 'event-fn) event-args)))

;; The next function is invoked at the end of every input.

;;;(defun count-events ()
;;;  (declare (special input-counter))
;;;  (setq input-counter (+ input-counter 1))
;;;  (when (= 0 (rem input-counter event-cycle))
;;;	(do ((events global-eventlist (cdr events))
;;;	     (quot (floor input-counter event-cycle))
;;;	     (event))
;;;	    ((null events))
;;;	  (setq event (car events))
;;;	  (when (and (symbolp event)
;;;		     (eval (get event 'event-list))
;;;		     (fixp (get event 'write-when))
;;;		     (= 0 (rem quot (get event 'write-when))))
;;;		     ;; Notice that it will write, event if the event
;;;		     ;; is disabled.  Disabling just prevents recording
;;;		     ;; of new events - the events that already occurred
;;;		     ;; are still recorded when their time comes.
;;;		(hook-write-events event)))))


(defun count-events ()
  (declare (special input-counter))
  (incf input-counter)
  (when (zerop (rem input-counter event-cycle))
    (let ((quot (floor input-counter event-cycle)))
      (dolist (event global-eventlist)
	(when (and (symbolp event)
		   (eval (get event 'event-list))
		   (fixp (get event 'write-when))
		   (zerop (rem quot (get event 'write-when))))
	  ;; Notice that it will write, event if the event
	  ;; is disabled.  Disabling just prevents recording
	  ;; of new events - the events that already occurred
	  ;; are still recorded when their time comes.
	  (hook-write-events event))))))

;;;(defun write-all-events ()
;;;  (do ((events global-eventlist (cdr events))
;;;       (event))
;;;      ((null events))
;;;    (setq event (car events))
;;;    (when (and (symbolp event)
;;;	       (eval (get event 'event-list)))
;;;	  (hook-write-events event))))

(defun write-all-events ()
  (dolist (event global-eventlist)
    (when (and (symbolp event)
	       (eval (get event 'event-list)))
      (hook-write-events event))))

(defun hook-write-events (event)
  (%catch% (progn
	    (when (get event 'write-hook)
	      (funcall (get event 'write-hook)
		       (eval (get event 'write-file))
		       (eval (get event 'event-list))))
	    (write-events (eval (get event 'write-file))
			  (eval (get event 'event-list)))
	    (set (get event 'event-list) ())
	    t)
	   (fail nil)))

(defun write-events (event-file event-list)
  (let ((reroute-close-message (not quiet-events)))
    (declare (special reroute-close-message))
    (reroute-output-append event-file *default-pathname-defaults*
      (mapcar #'(lambda (rec) (prin1 rec) (terpri))
	      (reverse event-list)))))

;; There used to be an ERRSET around the reroute-output-append
