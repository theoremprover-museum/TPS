;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLGO)

;;;
;;; File: Otl-Go
;;;
;;; defines categories etc. to allow automatic suggestion of inference
;;; rules without the benefit of an expansion tree.
;;;

(part-of otlgo)

(deffile otl-go
  (part-of otlgo)
  (extension lsp)
  (mhelp "Defines categories etc. to allow automatic suggestion of inference
rules without the benefit of an expansion tree."))

(context suggestions)

(defmexpr go
  (mainfns go-wild)
  (mhelp "Start producing and applying suggestions until no more are found.
Suggestions are treated according to their priority and the state of
the global parameter GO-INSTRUCTIONS."))

(defun go-wild ()
  (declare (special dproof))
  (cond ((proof-lines dproof)
	 (let ((sorted-rulelist
		 (sort-rules (set-of srule global-srulelist
				     (and (symbolp srule)
					  (get srule 'priority))))))
	   (do ((plans (get dproof 'plans) (get dproof 'plans))
		(dont-consider nil)
		(found-something))
	       ((null plans) (msgf "PROOF COMPLETE."))
	     (setq found-something
		   (do ((plans plans (cdr plans))
			(plan)
			(done-something))
		       ((null plans) (msgf "Stopping GO.") nil)
		     (setq plan (car plans))
		     (when (not (member (car plan) dont-consider))
		       (msgf "Considering planned line "
			     ((car plan) . existing-line) ".")
		       ;; The next line is so ^P works correctly.
		       (subproof (car plan))
		       (setq done-something (go-main plan sorted-rulelist))
		       (if (not done-something)
			   (push (caar plans) dont-consider)
			   (return t)))))
	     (when found-something (prfw-pall) (prfw-^p) (prfw-^pn))
	     (when (not found-something) (return nil)))))
	(t (complain "No proof in progress."))))

(defun go-main (plan sorted-rulelist)
  (do ((ssrules sorted-rulelist (cdr ssrules))
       (suggestions) (action))
      ((null ssrules) nil)
    (setq action (find-action (caar ssrules) go-instructions))
    (case action
      (do (setq suggestions (find-suggests plan (cdar ssrules)))
	  (when suggestions
		(if (or resolve-conflict
			(not (multiple-suggestions-p suggestions)))
		    (let ((donep (apply-rule suggestions)))
		      (if donep 
			  (return t)
			(return nil))) ; rule failed (eg, SAME when hyps aren't legal) cebrown 6/17/01
		    (let ((donep (ask-apply-rule suggestions)))
		      (when donep (return t))))))
      (ask (setq suggestions (find-suggests plan (cdar ssrules)))
	   (when suggestions
		 (let ((donep (ask-apply-rule suggestions)))
		   (when donep (return t)))))
      (show (setq suggestions (find-suggests plan (cdar ssrules)))
	    (msgf ;;"Planned line " ((car plan) . existing-line)
		  "Showing suggestions of priority " (caar ssrules) ":")
	    (show-suggestions suggestions))
      (forget (return nil))
      (t (throwfail "Illegal action in GO-INSTRUCTIONS: " action)))))

(definfo do
  (mhelp "An action for GO-INSTRUCTIONS.
Generate a list of suggestions for the next step of GO,
and do whatever seems most likely to work."))

(definfo ask
  (mhelp "An action for GO-INSTRUCTIONS.
Ask for input from the user for the next step of GO."))

(definfo show
  (mhelp "An action for GO-INSTRUCTIONS.
Show the suggestions for the next step of GO."))

(definfo forget
  (mhelp "An action for GO-INSTRUCTIONS.
Do nothing."))

(defun find-action (priority go-instructions)
  (if (null priority) 'forget
      (do ((goi go-instructions (cdr goi)))
	  ((null goi) 'forget)
	(when (not (> priority (caar goi)))
	      (return (cadar goi))))))

(defun apply-rule (suggestions)
  (let ((exec-form (first-rule suggestions)))
    (msgf "Executing -> " (l exec-form))
    (%catch% (comdecode-rule exec-form)
	     (fail (complain core::expand-catch-throw t 
                             "Execution of rule aborted.") nil))))

(defun comdecode-rule (exec-form)
  (let ((true-args 
	 (if (and quietly-use-defaults (get (car exec-form) 'srule))
	     (fill-args-$ exec-form)
	     exec-form)))
    (if (not (comdecode true-args))
	;; Some Error during the execution of the rule.
	;; in this case we do a THROWFAIL
	(throwfail "Error while executing a command inside GO.  "
		   "Aborting GO.")
	t)))

(defun fill-args-$ (exec-form)
  (cons (car exec-form)
	(fill-args (cdr exec-form) (get (car exec-form) 'argtypes) '$)))

(defun first-rule (suggestions)
  (let ((curr-rule (caar suggestions))
	(plist (cadar suggestions))
	(slist (caddar suggestions)))
    (if plist (setq plist (list (linealias (car plist)))))
    (if slist (setq slist (list (linealias (car slist)))))
    `(,curr-rule ,@plist ,@slist)))

(defun multiple-suggestions-p (suggestions)
  (or (cdr suggestions) (cdr (caddar suggestions))))

(defun ask-apply-rule (suggestions)
  (show-suggestions suggestions)
  (let ((exec-form nil))
    (%catch% (progn
	      (prompt-read exec-form nil
			   (msgf "Command") 'exec-form (first-rule suggestions)
			   ((? (msgf "One of the rule applications listed above,"
				     t "any command, or MORE to see more suggestions."))
			    (more (msgf "No suggestion executed.")
				  (%throw% 'nothing-done nothing-done))))
	      (comdecode-rule exec-form))
	     (nothing-done nil)
	     (fail (throwfail-on core::expand-catch-throw 
                                 t "Execution of rule aborted.") t))))

(defmexpr suggest
  (argtypes pline)
  (argnames pline)
  (arghelp "Planned line to get a suggestion for")
  (defaultfns (lambda (pline)
		(when (eq pline '$) (setq pline (pline-default '$)))
		(list pline)))
  (mhelp "Suggest some applicable inference rule for proving a planned line."))

(defun suggest (pline)
  (declare (special dproof max-suggest-priority))
  (let ((plan-support (assoc pline (get dproof 'plans)))
	(sorted-srulelist (sort-rules global-srulelist))
	(suggestions nil))
    (do ((ssrules sorted-srulelist (cdr ssrules)))
	((null ssrules))
      (setq suggestions (find-suggests plan-support (cdar ssrules)))
      (when suggestions
	    (if (integerp (caar ssrules))
		(msgf "Priority " (caar ssrules) ":")
		(msgf "No Priority:")) 
	    (show-suggestions suggestions)))))

