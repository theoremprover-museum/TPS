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
;;; File : Events-Mac
;;; 

(part-of events)

(deffile events-mac
  (part-of events)
  (extension lsp)
  (mhelp "Defines category of EVENT and some flags etc."))

(context tps-events)

;*;    A category of EVENT.  Every event has a few properties:
;*;    MHELP - the obvious
;*;    EVENT-ARGS - list of arguments passed on by SIGNAL for any event
;*;	of this kind.
;*;    TEMPLATE - constructs the list to be written.  Contrary to what
;*;	we had before, we will not assume that every event is
;*;	time-stamped or has the user-id.  The template
;*;        must only contain globally evaluable forms and the arguments
;*;	of the particular event signalled.
;*;    WRITE-WHEN - one of IMMEDIATE, NEVER, or an integer n, which means
;*;    	write after an implementation depended period of `n'.
;*;     At the moment this will write, whenever #-of-inputs = n * event-cycle,
;*;     where event-cycle is a global variable, say 5.
;*;    WRITE-FILE - the filename of the file for the message to be
;*;        appended to.
;*;    SIGNAL-HOOK - an optional function to be called whenever the
;*;	the event is signalled.  This should NOT to the writing of
;*;	the information, but may be used to do something else.
;*;    WRITE-HOOK - an optional function to be called whenever a number
;*;	(>0) of events are written.
;*;
;*;    A macro or function SIGNAL-EVENT, whose first argument is the
;*;    kind of even to be signalled, the rest of the arguments are the
;*;    event-args for this particular event.  SIGNAL-EVENT will return
;*;    T or NIL, depending on whether the action to be taken in case of
;*;    the even was successful or not.  It is the caller's reponsibility
;*;    to act accordingly.  E.g. if (SIGNAL-EVENT COSTLY-ADVICE 'X2106)
;*;    returns NIL, the advice should not be given (of course at the moment
;*;    we don't charge for advice).
;*;
;*;  The MACRO (INIT-EVENT event) or (INIT-EVENTS) must be called before
;*;  events occur.  One may locally bind the variable <event>-ENABLED
;*;  to NIL to disable an event, which is then always considered to
;*;  be successful (eventually, these will be made flags with defflag).
;*;  (setq error-enabled nil)
;*;  in your .INI file will make sure that no MacLisp error will be recorded.
;*;  For a maintainer, this is probably a good idea.

(defflag quiet-events
  (flagtype boolean)
  (default t)
  (subjects events)
  (mhelp "If T, no message will be given when events are written."))

(defflag event-cycle
  (flagtype integer+)
  (default 5)
  (subjects events)
  (mhelp "The indivisible unit in number of inputs.
When WRITE-WHEN for an EVENT is `n', the event info will be written
every n * event-cycle inputs.  n=0 means don't write."))

(defflag events-enabled
  (flagtype boolean)
  (default t)
  (subjects events)
  (mhelp "If nil, all events are disabled."))

(defvar input-counter 0)

(defcategory event
  (define defevent)
  (properties
   (event-args multiple)
   (template single)
   (template-names single)
   (write-when single)
   (write-file single)
   (signal-hook singlefn)
   (write-hook singlefn)
   (mhelp single))
  (global-list global-eventlist)
  (mhelp-line "event")
  (mhelp-fn princ-event-help))

(defun princ-event-help (event category)
  (feat-help event category 20 78
	     '(event-args template template-names
			  write-when write-file write-hook)))

(defun event-p (event) (and (symbolp event) (get event 'event)))

(defmacro init-event (event)
  (init-event-expand event))

(defmacro init-events ()
  (do ((events global-eventlist (cdr events))
       (init-events-forms
	 nil
	 (if (symbolp (car events))
	     (cons (init-event-expand (car events)) init-events-forms)
	     init-events-forms)))
      ((null events) `(progn ,@init-events-forms))))

(defun init-event-expand (event)
  (unless (event-p event)
    (throwfail event " cannot be initialized: not recognized."))
  (let ((event-list-name (prepend event '-list))
	(event-fn-name (prepend event '-fn))
	(event-enabled
	  (prepend event '-enabled :package (find-package "CL-USER")))
	event-fn-defn
	(event-args (get event 'event-args))
	(template (get event 'template))
	(write-when (get event 'write-when))
	(write-file (get event 'write-file))
	(signal-hook (get event 'signal-hook))
	;;; added following so that -ENABLED flags will be defined in
	;;; the same context in which the event is defined DAN 8AUG89
	(event-context (cdr (assoc 'event (get event
					       'core::contexts))))
	(curr-context current-context)
	(write-hook (get event 'write-hook)))
    (setq event-fn-defn
	  `(defun ,event-fn-name ,event-args
	     (declare (special ,event-enabled ,event-list-name))
	     (if ,event-enabled
		 (%catch%
		   (progn
		     ,@(if signal-hook `((funcall #',signal-hook ,@event-args))
			   ())
		     ,@(cond ((equal write-when 'never) ())
			     ((equal write-when 'immediate)
			      `((push (list ,@template) ,event-list-name)
				,@(if write-hook
				      `((funcall #',write-hook ,write-file
						 ,event-list-name))
				      ())
				(write-events ,write-file ,event-list-name)
				(setq ,event-list-name '())))
			     ((not (fixp write-when))
			      ;; in this case we supplied a fn to decide
			      ;; whether to write.
			      `((push (list ,@template) ,event-list-name)
				(when (funcall #',write-when ,event-list-name)
				  ,@(if write-hook
					`((funcall #',write-hook ,write-file
						   ,event-list-name))
					())
				  (write-events ,write-file ,event-list-name)
				  (setq ,event-list-name '()))))
			     (t ;; so nothing if Fixnum: this is done after n
			       ;; inputs, not when the event is actually
			       ;;signalled
			       `((push (list ,@template) ,event-list-name)))
			     )
		     t)			; return T if no THROWFAIL inside.
		   (fail nil))
		 t)))			; return NIL if unsuccessful.
    `(progn ;; added so that -ENABLED flags will be defined in the
	    ;; same context as the event itself DAN 8AUG89
       (context ,event-context)
       (putprop ',event ',event-fn-name 'event-fn)
       (defvar ,event-list-name ())
       (setq ,event-list-name ())	;ignore previous events after INIT
       (putprop ',event ',event-list-name 'event-list)
       (defflag ,event-enabled
	 (flagtype boolean)
	 (default t)		;enable event, if not already disabled.
	 (subjects events)
	 (mhelp ("If NIL, recording events of type "
		 ',event " is disabled.")))
       ,event-fn-defn
       (compile ',event-fn-name)
       ',event
       (when ',curr-context
	 (context ,curr-context)))))

(defun disable-events ()
  ;;disable all events except DONE-EXC
  (dolist (event global-eventlist)
    (when (and (symbolp event) (neq event 'done-exc))
      (set (prepend event '-enabled  :package (find-package "CL-USER")) nil))))

(defmexpr disable-events
  (mhelp "Disable recording of TPS events. You will need to start a new session
of TPS to enable recording of events after they have been disabled."))

