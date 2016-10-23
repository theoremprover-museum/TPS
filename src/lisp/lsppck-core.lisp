;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of BARE)

(deffile lsppck-core
  (part-of BARE)
  (extension lisp)
  (mhelp "Functions in the CORE package to do with lisp packages."))

(context lisp-packages)

(defun lisp-package-p (pack)
  (and (symbolp pack) (get pack 'lisp-pack)))

(defun get-lisp-package-name (pack)
  (if (or (not (symbolp pack)) (not (lisp-package-p pack)))
      (throwfail "Not a package.")
      (find-package (string pack))))

(defun print-lisp-package-name (pack)
  (princ (package-name pack)))

(defun use-one (lisp-package)
  (use-package (list lisp-package)))

(defmexpr unuse
  (argnames lisp-package)
  (argtypes lisp-package)
  (arghelp "LISP Package")
  (mhelp "Make a Lisp package inaccessible."))

(defun unuse (lisp-package)
  (unuse-package (list lisp-package)))

(defun pack-status ()
  (let ((used-packs (package-use-list *package*))
	(known-packs (list *package*)))
    (msgf "You are currently in package " (*package* . lisp-package) ".")
    (msgf "You are USE'ing packages "
	  (used-packs . lisp-package-list) ".")
    (msgf "Other Lisp packages known to TPS are")
    (dolist (pack global-lisp-packagelist)
      (when (and (symbolp pack) (not (member (find-package pack) used-packs)))
	(push (find-package pack) known-packs)
	(msg " " pack)))
    (msg ".")
    (msgf "Lisp also knows about "
	  ((set-difference 
	    (set-difference (list-all-packages)
			   used-packs)
	    known-packs) . lisp-package-list) ".")))


