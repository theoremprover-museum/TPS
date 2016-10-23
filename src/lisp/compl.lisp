;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :maint)
(part-of MAINTAIN)

(deffile compl
  (part-of MAINTAIN)
  (extension lisp)
  (mhelp "Functions to do with compiling and loading code."))

(defmexpr cload
  (argtypes filespec)
  (argnames file)
  (arghelp "File to be Compiled and Loaded")
  (mhelp "Compile and load a file."))

;;; Function CLOAD is in file BOOT1

(defmexpr cload-modules
  (argtypes modulelist)
  (argnames modules)
  (arghelp "List of modules to compile and load")
  (mhelp "Compile and Load a list of modules."))

;;; Function CLOAD-MODULES is in file BOOT1.

(defmexpr compl
  (argtypes filespeclist)
  (argnames filespeclist)
  (arghelp "Files to be compiled.")
  (mhelp "Compile 1 or more files."))

(defun compl (list)
  (dolist (filespec list)
    (let ((true-name 
	   (locate-tps-file filespec t)))
      (if true-name
	  (compile-file true-name)
	  (throwfail "File " (filespec . filespec) " not found.")))))


(defun compile-list (&optional (directory-list source-path)
			       &key (source-only nil))
  (let ((compl-list nil))
    (dolist (directory directory-list)
      (let ((pathname (make-pathname% :directory directory)))
	(dolist (source-file (directory pathname))
	  (when (string= (pathname-type source-file) source-extension)
	    (let ((newest-file
		   (locate-file (pathname-name source-file)
				directory-list source-only)))
	      (when (string= (pathname-type newest-file) source-extension)
		(push (namestring (truename newest-file)) compl-list)))))))
    (nreverse compl-list)))

(defmexpr compile-list
  (argtypes dirspeclist boolean)
  (argnames directory-list source-only)
  (arghelp "Directories to be searched." "T to consider all files.")
  (defaultfns (lambda (directory-list source-only)
		(list (if (eq directory-list '$) source-path
			  directory-list)
		      (if (eq source-only '$) nil source-only))))
  (mainfns (lambda (directory-list source-only)
	     (compile-list directory-list :source-only source-only)))
  (closefns princ)
  (mhelp "Returns a list of files that need to be compiled."))
