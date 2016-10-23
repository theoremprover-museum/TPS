;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :auto)
(part-of tactics)

(context tactics)

(deffile tacticals
  (extension lisp)
  (part-of tactics)
  (mhelp "Defines standard tacticals."))

(deftactical call
  (defn call-tactical-fn)
  (mhelp "(CALL command) will execute command as if it were entered at the
top level by the user.  CALL is used only for side effects, the goal is
always returned."))

(defun call-tactical-fn (goal command)
  (tactic-output (conc-strings "Calling " (princ-to-string command))
		 t)
  (progn (%catch% (comdecode command) (fail nil))
	 (values (if goal (list goal))
		 (conc-strings "Called " (princ-to-string command))
		 nil
		 (list 'lambda (list 'x) 'x))))

(deftactical orelse
 (defn orelse-tactical-fn)
 (mhelp "Given a list of tactics, ORELSE will apply the first one which succeeds."))

(defun orelse-tactical-fn (goal tac-list)
  (do ((tac-list tac-list (cdr tac-list))
       (newgoals (if goal (list goal)))
       (msg nil)
       (validation (list 'lambda (list 'x) 'x))
       (token 'fail))
      ((or (null tac-list)
	   (not (member token (list nil 'fail))))
       (values newgoals msg token validation))
    (multiple-value-setq (newgoals msg token validation)
      (apply-tactic (car tac-list) :goal goal))))


(eval-when (eval load compile)
(deftactical idtac
  (defn idtac-tactical-fn)
  (mhelp "Tactical which always succeeds, returns its goal unchanged."))
)

(defun idtac-tactical-fn  (goal tac-list)
  (declare (ignore tac-list))
  (values (if goal (list goal)) "IDTAC" 'succeed (list 'lambda (list 'x) 'x)))

(deftactical failtac
  (defn failtac-tactical-fn)
  (mhelp "Tactical which always fails, returns its goal unchanged."))

(defun failtac-tactical-fn (goal tac-list)
  (declare (ignore tac-list))
  (values (if goal (list goal)) "FAILTAC" 'fail (list 'lambda (list 'x) 'x)))


(deftactical repeat
  (defn repeat-tactical-fn)
  (mhelp "(REPEAT tactic) will apply tactic repeatedly until it fails on
every subgoal which has been created."))

(defun repeat-tactical-fn (goal tac-list)
  (do ((goal goal (car pending))
       (pending nil (cdr pending))
       (tac (car tac-list))
       (no-good nil)
       (msg "Complete.")
       (token nil)
       (validation (list 'lambda (list 'x) 'x))
       (returned-validation nil)
       (newgoals nil))
      ((or (null goal) (eq token 'xx-abort))
       (if (eq token 'xx-abort)
	   (values (append pending no-good) msg 'xx-abort validation)
	   (values no-good msg (if no-good 'succeed 'complete) validation)
	   ))
    (multiple-value-setq (newgoals msg token returned-validation)
      (apply-tactic tac :goal goal))
    (setq validation (make-validation validation returned-validation))
    (if (member token (list 'fail 'xx-abort))
	(progn (setq no-good (append no-good (list goal)))
	       (rplaca (cdr validation)
		       (append (cdadr validation) 
			       (list (caadr validation)))))
	(setq pending (sort-tac-goals (nconc newgoals pending) tacuse)))
    ))

(deftactical then
  (defn then-tactical-fn)
  (mhelp "(THEN tactic1 tactic2) will first apply tactic1; if it fails
 then failure is returned, otherwise tactic2 is applied to each resulting
 goal.  If tactic2 fails on any of these goals, then failure is returned,
 otherwise the new goals obtained from the calls to tactic2 are returned."))

(defun then-tactical-fn (goal tac-list)
  ; tac-list should contain only two elements
  (if goal
      (let* ((old (save-object goal))
	     (new (copy-object goal))
	     (newgoal (get-new-goal goal new))
	     (newgoals nil)
	     (msg nil)
	     (validation nil)
	     (validation* nil)
	     (token nil)
	     (tac1 (car tac-list))
	     (tac2 (cadr tac-list)))
	(multiple-value-setq (newgoals msg token validation) 
	  (%catch% 
	   (%top-catch% (apply-tactic tac1 :goal newgoal)
			(values (list newgoal) "Aborted." 'xx-abort
				(list 'lambda (list 'x) 'x)))
	   (fail (values (list newgoal) "Aborted." 'xx-abort
			 (list 'lambda (list 'x) 'x)))
	   ))
	(if (member token (list 'fail 'xx-abort))
	    (progn (restore-object old newgoal) 
		   (values (list goal) msg token 
			   (list 'lambda (list 'x) 'x)))
	    (do* ((goals newgoals (cdr goals))
		  (newgoals* nil)
		  (goal* (car newgoals) (car goals)))
		((null goals)
		 (update-object new old) 
		 (values (update-goals newgoals* new)
			 msg 'succeed validation))
	      (multiple-value-setq (newgoals msg token validation*) 
		(%catch%
		 (%top-catch% (apply-tactic tac2 :goal goal*)
			      (values (list goal*) "Aborted."
				      'xx-abort nil))
		 (fail (values (list goal) "Aborted" 'xx-abort
			       nil))
		 ))
	      (if (member token (list 'fail 'xx-abort))
		  (progn (restore-object old goal)
			 (return (values (list goal) msg token
					 (list 'lambda (list 'x) 'x))))
		  (progn 
		    (setq newgoals* (append newgoals* 
					    newgoals))
		    (setq validation 
			  (rotate-vars (length (cadr validation*))
				       (make-validation
					validation validation*))))

		  ))))
      (values nil "Complete." 'complete (list 'lambda (list 'x) 'x))))


;;; Extremely inefficient implementation, tac1 will 
;;; be tried twice if it fails

(deftactical then*
  (defn 
    (tac-lambda (tac1 tac2)
      (then tac1 (then (orelse tac2 (idtac)) (idtac)))))
  (mhelp "(THEN* tactic1 tactic2) will first apply tactic1; if it fails
 then failure is returned, otherwise tactic2 is applied to each resulting
 goal.  If tactic2 fails on any of these goals, then the new goals obtained
 as a result of applying tactic1 are returned, otherwise the new goals
 obtained as the result of applying both tactic1 and tactic2 are returned."))

(deftactical then**
  (defn then**-tactical-fn)
  (mhelp "(THEN** tactic1 tactic2) will first apply tactic1 to the current
goal.  If it does not fail, tactic2 will be applied to the goals which are
produced by tactic1, and success will be returned along with any new goals
produced.  If tactic1 fails, failure will be returned.  Differs from THEN
and THEN* in that the current goal will never be copied."))

(defun then**-tactical-fn (goal tac-list)
  (let* ((tac1 (car tac-list))
	 (tac2 (cadr tac-list))
	 (newgoals nil)
	 (validation (list 'lambda (list 'x) 'x))
	 (returned-validation nil)
	 (token nil)
	 (msg "Complete."))
    (multiple-value-setq (newgoals msg token returned-validation)
      (apply-tactic tac1 :goal goal))
    (cond ((memq token (list 'fail 'xx-abort))
	   (values (list goal) msg token validation))
	  ((null newgoals)
	   (values nil msg token (make-validation validation
						  returned-validation)))
	  (t
	   (setq newgoals (sort-tac-goals newgoals tacuse))
	   (do ((goals (cdr newgoals) (cdr goals))
		(goal (car newgoals) (car goals))
		(return-goals nil))
	       ((or (null goal) (eq token 'xx-abort))
		(if (eq token 'xx-abort)
		    (values (append goals return-goals)
			    msg 'xx-abort validation)
		    (values return-goals msg 'succeed validation)
		    ))
	     (multiple-value-setq (newgoals msg token returned-validation)
	       (apply-tactic tac2 :goal goal))
	     (setq validation (make-validation validation returned-validation))
	     (setq return-goals (append newgoals return-goals)))))))

(deftactical ifthen
  (defn ifthen-tactical-fn) 
  (mhelp "(IFTHEN test tactic1 [tactic2]) will first evaluate test, which may
be either a tactical or (if user is an expert) an arbitrary LISP expression.
If test is a tactical and does not fail, or is a LISP expression which does
not evaluate to nil, then tactic1 will be executed and IFTHEN will return its
results.  If test fails or is nil, then tactic2  (if present) will be executed
and its results returned by IFTHEN.  Tactic2 is optional; if not specified,
and test fails, IFTHEN will return failure."))

(defun ifthen-tactical-fn (goal tac-list)
  (let* ((test (car tac-list))
	 (then-tac (cadr tac-list))
	 (else-tac (caddr tac-list))
	 (result (if (tactic-p test)
		     (multiple-value-bind (newgoals msg token validation)
			 (apply-tactic test :goal goal)
		       (declare (ignore msg validation newgoals))
		       (if (eq token 'xx-abort)
			   'xx-abort
			   (not (eq token 'fail))))
		     (if expertflag (eval test)))))
    (if (eq result 'xx-abort)
	(values (if goal (list goal)) "Aborted." 'xx-abort 
		(list 'lambda (list 'x) 'x))
	(if result
	    (progn
;	      (tactic-output
;		(conc-strings "Ifthen test "
;			(if (symbolp test) (symbol-name test)
;			    (princ-to-string test))
;			" succeeded. ")
;		t)
	      (apply-tactic then-tac :goal goal))
	    (progn
;	      (tactic-output
;		(conc-strings "Ifthen test "
;			(if (symbolp test) (symbol-name test)
;			    (princ-to-string test))
;			" failed. ")
;		nil)
	      (if else-tac
		  (apply-tactic else-tac :goal goal)
		  (values (if goal (list goal))
			  (conc-strings "Ifthen test " 
					(if (symbolp test)
					    (symbol-name test)
					    (princ-to-string test)) 
					" failed. ")
			  'fail (list 'lambda (list 'x) 'x)))
	      )))))



(deftactical sequence
  (defn sequence-tactical-fn) 
  (mhelp "(SEQUENCE TAC1 ... TACn) applies tactics TAC1, ..., TACn in order,
regardless of their success or failure. "))

(defun sequence-tactical-fn (goal tac-list)
  (do ((tac-list tac-list (cdr tac-list))
       (goal goal (car pending))
       (pending nil (cdr pending))
       (newgoals* nil)
       (validation (list 'lambda (list 'x) 'x)
		   (rotate-vars (length (cadr validation*))
				(make-validation validation validation*)))
       (validation* nil)
       (msg nil)
       (token nil))
      ((or (null tac-list)
	   (eq token 'xx-abort))
       (if goal
	   (values (cons goal pending) msg token validation)
	   (values pending msg token validation)))
    (multiple-value-setq (newgoals* msg token validation*) 
      (apply-tactic (car tac-list) :goal goal))
    (setq pending (append pending newgoals*))))

(deftactical try
  (defn 
    (tac-lambda (tactic)
      (then tactic (failtac))))
  (mhelp "(TRY tactic) will use tactic on the current object.  If any goals
remain after tactic finishes, then the original object will be restored,
otherwise the work done by tactic will be kept."))

(deftactical compose
  (defn compose-tactical-fn) 
  (mhelp "(COMPOSE tac1 tac2 ... tacn) will apply its argument tactics
in order, composing their results until one of them fails."))

(defun compose-tactical-fn (goal tac-list)
  (let ((first-tac (car tac-list))
	(rest (cdr tac-list)))
    (apply-tactic (if (null first-tac)
		      (list 'idtac)
		      `(then* ,first-tac (compose ,@rest)))
		  :goal goal)))

(deftactical no-goal
  (defn no-goal-tactical-fn)
  (mhelp "(NO-GOAL) succeeds iff the goal with which it is invoked is nil."))

(defun no-goal-tactical-fn (goal tac-list)
  (declare (ignore tac-list))
  (if (null goal)
      (values nil "No goal." 'succeed (list 'lambda (list 'x) 'x))
    (values (list goal) "Goal exists." 'fail (list 'lambda (list 'x) 'x))))

(defflag default-tactic
  (flagtype tactic-exp)
  (default (idtac))
  (subjects tactics transmit)
  (mhelp "The default tactic for ETREE-NAT and USE-TACTIC.
See the help messages for these commands for more information."))
