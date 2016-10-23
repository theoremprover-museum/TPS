;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of EVENT-SIGNAL)

;;;
;;; File: Event-Signal-Utils    (formerly HACK)
;;;
;;; for coding messages for completed exercises.

(part-of event-signal)

(deffile event-signal-utils
  (part-of event-signal)
  (extension lsp)
  (mhelp "Defines the function which assigns a code for exercises completed
by the students."))

;;; Important in the next function is that the value returned is
;;; a true integer, not an approximation, if called with integers.

(defmacro ^ (base power) `(expt ,base ,power))

(defvar *modulo* (- (^ 2 17) 1))

(defun mark-list (list)
  (append list (list (code-list list))))

;;;Changed error to throwfail 8/9/87 DAN 
(defun code-list (object)
  (typecase object
    (integer object)
    ((or symbol string)
     (reduce #'(lambda (n char) (rem (^ (+ n (char-int char)) 2) *modulo*))
	     (string object)
	     :initial-value 0))
    (list (reduce #'(lambda (n elm) (rem (^ (+ n (code-list elm)) 2) *modulo*))
		  object :initial-value 0))
    (t (throwfail (format nil "Cannot code object ~S." object))
;;;    (error "Cannot code object ~S." object)
       )))
