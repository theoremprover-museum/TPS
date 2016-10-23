;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of AUTO-DOC)

;;;
;;; File: PLURALS
;;; Author: cpk
;;;
;;; Various language hacks to give the automatic documentation
;;; a better command of English. We expect that most of these
;;; will be obviated by Common Lisp facilities.
;;; 

(part-of auto-doc)

(deffile plurals
  (part-of auto-doc)
  (extension lsp)
  (mhelp "A file of language hacks in lieu of Common Lisp."))

(defun cat-plural (id)
  (let ((length (1- (length id)))
	(id (string-capitalize  id)))
    (if (equal (char id length) #\y)
	(concatenate 'string (subseq id 0 length) "ies")
	(concatenate 'string id
		     (if (equal (char id length) #\s) "es" "s")))))
  
(defun vowel (sym)
  (member sym '(#\a #\A #\e #\E #\i #\I #\o #\O #\u #\U)))

(defun an-a (word cap-sw)
  (concatenate 'string (if cap-sw "A" "a")
	       (if (vowel (char word 0)) "n " " ") word))
