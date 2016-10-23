;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :maint)
(part-of AUTO-DOC)

;;;
;;; File: Collect-Help-3
;;;
;;; looks through a list of modules and writes the help-string in
;;; them into file(s), sorted alphabetically.
;;;

(part-of auto-doc)

(deffile collect-help
  (part-of auto-doc)
  (extension clisp)
  (mhelp "Looks through a list of modules and writes the help-string in
them into file(s), sorted alphabetically."))

(context coll-help)

(defmexpr collect-help
  (argtypes modulelist tpscatlist filespec) ;was originally "anything" rather than "tpscatlist"
  (argnames modules categories filename)
  (arghelp "Modules to collect help for"
	   "Categories to collect the help of"
	   "File to write help to")
  (defaultfns (lambda (modules categories filename)
		(when (eq modules '$)
		      (setq modules (loaded-modules)))
		(when (eq categories '$)
		      (setq categories global-categorylist))
		(when (eq filename '$)
		      (setq filename (make-pathname% :name core-name
						    :type "help")))
		(list modules categories filename)))
  (mhelp "Collect help for the specified modules into a file. 
Prints out a # every time it finds a help message, and a * every time it 
finds a TPS object with no help message.")) 

(defun collect-help (modules categories filename)
  (let ((define-list
	  ;;(set-of pair global-definelist (member (cdr pair) categories))
	  (remove-if-not #'(lambda (pair) (member (cdr pair) categories))
			 global-definelist))
	(keys nil))
    (declare (special keys))
    (dolist (module modules (write-help-file keys filename))
      (msgf "Module " module ": ")
      (dolist (macro-file  (get module 'macro-files))
	(read-help-from-file  macro-file define-list))
      (dolist (file (get module 'files))
	(read-help-from-file file define-list)))))

(defun read-help-from-file (file define-list)
  (declare (special keys))
  (msg " " file)
  (setq file (locate-tps-file file t))
  (if (not file) (msg "-Not Found-" t)
      (with-open-file (sourcef file :direction :input); not necessary, and makes cmucl choke
		                                      ; :element-type :default)
		      (do ((form (read sourcef nil :$$eof$$ t)
				 (read sourcef nil :$$eof$$ t)))
			  ((eq form :$$eof$$))
			(when (and (consp form) (assoc (car form) define-list))
			      (let ((mhelp (cadr (assoc 'mhelp (delete-if-not #'consp (cdr form))))))
				(cond ((not mhelp) (msg "*"))
				      (t (msg "#")
					 (push (list (cadr form)
						     (cdr (assoc (car form)
								 define-list))
						     mhelp)
					       keys)))))))))

#+comment(defun read-help-from-file (file define-list)
  (declare (special keys))
  (msg " " file)
  (setq file (locate-tps-file file t))
  (if (not file) (msg "-Not Found-" t)
      (with-open-file (sourcef file :direction :input :element-type :default)
		      (do ((form (read sourcef :$$eof$$)
				 (read sourcef :$$eof$$)))
			  ((eq form :$$eof$$))
			(when (and (consp form) (assoc (car form) define-list))
			      (let ((mhelp (cadr (assoc 'mhelp (cdr form)))))
				(cond ((not mhelp) (msg "*"))
				      (t (msg "#")
					 (push (list (cadr form)
						     (cdr (assoc (car form)
								 define-list))
						     mhelp)
					       keys)))))))))

(defun write-help-file (keys filename)
  (msgf "Sorting the " (length keys) " help strings ..." T
	"Writing ...")
  (reroute-output filename (make-pathname% :name core-name :type "help")
    (dolist (key (sort keys #'alphalessp :key #'car) (msg T))
      (terpri)
      (print key))))
