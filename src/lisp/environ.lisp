;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of ENVIRONMENT)

;;;
;;; File: Environ
;;; Author: fp
;;;
;;; This file contains ENVIRONMENT, and other miscellaneous
;;; documentation features.
;;;

(part-of environment)

(deffile environ
  (part-of environment)
  (extension lsp)
  (mhelp "Defines the ENVIRONMENT help facility."))

(context help-obj)

;;;
;;; The ENVIRONMENT feature follows:
;;;


(defmexpr environment
  (mhelp "Helps to find out about TPS' current environment, i.e.
categories of TPS objects, commands, argument types, logical constants, etc."))


(defun environment ()
  (msgf "Currently defined are the following categories of TPS objects:" t)
  (dolist (cat (if alpha-lower-flag (mapcar #'string-downcase 
					    (if show-all-packages
						global-categorylist
					      (remove-if-not #'accessible-p global-categorylist)))
		 (if show-all-packages
		     global-categorylist
		   (remove-if-not #'accessible-p global-categorylist))))
	  (msg cat " "))
  (do ((category nil))
      (nil)
    (msgf t "Type a category, LEAVE, ?, ?? for more information on categories.")
    (prompt-read category nil
		 (msgf 'category) 'symbol 'leave
		 ((? (category-short-help))
		  (?? (category-help))))
    (if (eq category 'leave) (return nil))
    (when (member category global-categorylist)
	  ;;(msgf "A " (get category 'mhelp-line)
	  ;; " may have the following attributes:" t (l (get category 'properties)))
	  (msgf "Defined as a " (get category 'mhelp-line) " are:" t)
	  (category-items-short-help category)
	  (do* ((cat-item nil)
		(str (get category 'mhelp-line) (get category 'mhelp-line))
		(article (if (and (> (length str) 0)
				  (member (char str 0) '(#\a #\A #\e #\E #\i #\I #\o #\O #\u #\U)))
			     "an " "a ")
			 (if (and (> (length str) 0)
				  (member (char str 0) '(#\a #\A #\e #\E #\i #\I #\o #\O #\u #\U)))
			     "an " "a ")))
		(nil)
	    (msgf t "Type " article str
		 ", LEAVE, ?, or ?? for more information on "
		 (get category 'mhelp-line) "s.")
	    (prompt-read cat-item nil
			 (msgf category) 'symbol 'leave
			 ((? (category-items-short-help category))
			  (?? (category-items-help category))))
	    (if (eq cat-item 'leave) (return nil))
	    (when (get cat-item category)
		  (funcall (get category 'mhelp-fn) cat-item category))))))
