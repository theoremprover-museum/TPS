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

(deffile otl-go-mac
  (part-of otlgo)
  (extension lsp)
  (mhelp "Defines flags etc for GO."))

(context suggestions)

(eval-when (load compile eval)
(defun go-instruct-p (go-instruct)
  (and (consp go-instruct)
       (forall pair go-instruct (and (pairp pair)
				     (integerp (car pair))
				     (pairp (cdr pair))
				     (member (cadr pair)
					     '(do ask show forget))))))
)

(defflag go-instructions
  (flagtype go-instruct)
  (subjects suggests)
  (default ((0 do) (5 ask) (9 show) (100 forget)))
  (mhelp "A list of instructions for GO to decide what to do with suggestions.
It is a list of pairs (priority action), action being among DO, ASK, SHOW, 
FORGET.
The default setting ((0 DO) (5 ASK) (9 SHOW) (100 FORGET)) means
do suggestions of priority 0, ask me about doing suggestions of
priority 5 or less, otherwise just show me suggestions of priority
9 or less and then quit."))

(defflag resolve-conflict
  (flagtype boolean)
  (subjects suggests)
  (default t)
  (mhelp "If T, always the first of several suggestions is chosen,
if NIL, the user will be asked."))

(defflag quietly-use-defaults
  (flagtype boolean)
  (subjects suggests)
  (default t)
  (mhelp "If T, GO will fill in arguments with their defaults without
asking for confirmation.  If NIL, the command will be executed like
any other command issued at the top level."))

(defun get-exec-form (xxx)
  (cond ((symbolp xxx) (list xxx))
	((pairp xxx)
	 (cond ((not (symbolp (car xxx)))
		(throwfail (car xxx) " is not a symbol."))
	       ((not (get (car xxx) 'mexpr))
		(throwfail (car xxx) " is not a known command."))
	       (t xxx)))
	(t (throwfail xxx " is not a list."))))

(defun exec-form-p (exec-form)
  (and (pairp exec-form) (symbolp (car exec-form))
       (get (car exec-form) 'mexpr)))
