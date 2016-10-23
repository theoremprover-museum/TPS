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

(deffile tactics-aux
  (extension lisp)
  (part-of tactics)
  (mhelp "Auxiliary tactics."))

(context aux-tactics)

(deftactic finished-p
 (nat-ded 
  (lambda (goal)
    (declare (ignore goal))
    (if (proof-plans dproof)
	(progn
	 (tactic-output "Proof not complete." nil)
	 (values nil "Proof not complete." 'fail))
	(progn
	 (tactic-output "Proof complete." t)
	 (values nil "Proof complete." 'succeed))))
  "Returns success if current proof has no remaining planned lines."))

(deftactic make-nice
  (nat-ded
   (sequence (call cleanup) (call squeeze) (call pall))
   "Cleans up a completed proof."))

(defvar *lines-were-moved* nil)

(deftactic make-room
  (nat-ded
   (lambda (goal)
     (make-room-before goal 4)
     (values (list goal) " " nil))
;     (let ((plan-num (linealias goal))
;	   (closest (linealias 
;		     (do ((l (proof-lines dproof) (cdr l)))
;			 ((or (null l) (eq (cadr l) goal))
;			  (car l))))))
;       (if (and (numberp plan-num) (numberp closest)
;		(< (- plan-num closest) 5))
;	   (apply-tactic 
;	    `(call introduce-gap ,plan-num 
;		   ,(- 5 (- plan-num closest))) :goal goal)
;	   (values (list goal) " " nil))))
   "Ensures that there is room for at least four new lines before
the planned line." ))
     
(defun make-room-after (line num)
  (let* ((cur-num (linealias line))
	 (bad-lines (do ((bad nil)
			 (checknums
			  (let ((checknums nil))
			    (dotimes (i (1- num) (reverse checknums))
			      (push (+ i cur-num 2) checknums)))
			  (cdr checknums))
			 (checknum (1+ cur-num) (car checknums))
			 (newnum (+ 1 num cur-num)))
			((null checknum) bad)
		      (when (numalias checknum) 
			(push (cons checknum newnum)
			      bad)
			(setq checknums (nconc checknums (list newnum)))
			(incf newnum)))))
    (dolist (bad-line bad-lines)
      (funcall #'comdecode
	       (list 'move (car bad-line) (cdr bad-line))))
    (if (> (length bad-lines) 0) (progn (print-routines) (setq *lines-were-moved* nil)))))


(defun make-room-before (line num)
  (setq *lines-were-moved* nil)
  (let* ((cur-num (linealias line))
	 (bad-lines (do ((bad nil)
			 (checknums
			  (let ((checknums nil))
			    (dotimes (i (1- num) (nreverse checknums))
			      (push (- cur-num i 2) checknums)))
			  (cdr checknums))
			 (checknum (1- cur-num) (car checknums))
			 (newnum (- cur-num 1 num)))
			((null checknum) bad)
		      (when (numalias checknum) 
			(push (cons checknum newnum)
			      bad)
			(setq checknums (nconc checknums (list newnum)))
			(decf newnum)))))
    (when (or (and bad-lines (< (cdar bad-lines) 1))
	      (< (- cur-num num) 1))
      (let ((inc (- 1 (or (cdar bad-lines) (- cur-num num)))))
	(unless (eq tactic-verbose 'min) (msgf "Calling INTRODUCE-GAP " cur-num " " inc "."))
	(setq *lines-were-moved* T)
	(funcall #'comdecode 
		 (list 'introduce-gap cur-num inc))
	(setq bad-lines
	      (mapcar #'(lambda (x) (cons (car x) (+ (cdr x) inc)))
		      bad-lines))))
    (dolist (bad-line bad-lines)
      (when (not (= (car bad-line) (cdr bad-line)))
	    (setq *lines-were-moved* T)
	    (funcall #'comdecode
		 (list 'move (car bad-line) (cdr bad-line)))))
    (if *lines-were-moved* (progn (print-routines) (setq *lines-were-moved* nil)))))


(deftactic show-plans
  (etree-nat
   (lambda (pline)
     (declare (ignore pline))
     (msg t "Proof plans:" )
     (mapc #'(lambda (x) (msg (x . existing-linelist)))
	   (proof-plans dproof))
     (msg t))
   "Shows current plan support structure for all planned lines.")
  (nat-ded
   (lambda (pline)
     (declare (ignore pline))
     (msg t "Proof plans:" )
     (mapc #'(lambda (x) (msg (x . existing-linelist)))
	   (proof-plans dproof))
     (msg t))
   "Shows current plan support structure for all planned lines."))

(deftactic show-current-plan
  (etree-nat
   (lambda (pline)
     (msg t "Current planned line: " (pline . existing-line) t ))
   "Shows the current planned line.")
  (nat-ded
   (lambda (pline)
     (msg t "Current planned line: " (pline . existing-line) t ))
   "Shows the current planned line."))
	
(defflag use-rulep
  (flagtype boolean)
  (default t)
  (subjects mating-search etr-nat tactics ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "When true, indicates that RuleP should be used when possible
in translating from expansion proof to natural deduction proof."))


(deftactic use-rulep-tac
  (etree-nat
   (lambda (pline)
     (if use-rulep
	 (values (list pline) "RuleP allowed." 'succeed)
	 (values (list pline) "RuleP not allowed." 'fail)))
   "Returns success if value of the flag USE-RULEP is T.")
  (nat-ded
   (lambda (pline)
     (if use-rulep
	 (values (list pline) "RuleP allowed." 'succeed)
	 (values (list pline) "RuleP not allowed." 'fail)))
   "Returns success if value of the flag USE-RULEP is T."))

(defflag use-symsimp
  (flagtype boolean)
  (default t)
  (subjects mating-search etr-nat tactics ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "When true, indicates that symmetric simplification should be 
used when possible in translating from expansion proof to natural deduction 
proof.  Consult Pfenning's thesis for a description of symmetric
simplification."))


(deftactic use-symsimp-tac
  (etree-nat
   (lambda (pline)
     (if use-symsimp
	 (values (list pline) "Symmetric simplification allowed." 'succeed)
	 (values (list pline) "Symmetric simplification not allowed." 'fail)))
   "Returns success if value of the flag USE-SYMSIMP is T."))
