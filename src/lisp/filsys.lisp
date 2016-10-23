;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of FILE-OPS)

;;;
;;; File: FILSYS
;;; Package: FILE-OPS
;;;
;;; defines some utilities with respect to files, like OPENRECORD.
;;; 

(part-of file-ops)

(deffile filsys
  (part-of file-ops)
  (extension clisp)
  (mhelp "Defines file utitlities."))


(context file-operations)

(defmexpr openrecord
  (argtypes filespec)
  (argnames filespec)
  (arghelp "name of record file")
  (defaultfns (lambda (filespec)
		(cond ((eq filespec '$)
		       (setq filespec (make-pathname% :name "record"
						     :type "record"))))
		(list filespec)))
  (mhelp "Open a record file."))

;;This function needs to be checked and, perhaps, should be deleted in CLISP.

(defun openrecord (filespec)
  (declare (special recfile))
  (cond ((and (boundp 'recfile) (streamp recfile))
	 (throwfail "A record file " (recfile . filespec) " is already open.")))
  (setq recfile
	(open
	 (merge-pathnames filespec
			  (make-pathname% :name "record" :type "record"))
	 :direction "out" )) ;CMUCL doesn't like this-- :element-type :default))
  (push recfile outfiles)
  (push recfile echofiles)
  (push recfile msgfiles)
  ;;(setq ^R t)
  )

(defmexpr closerecord
  (mhelp "Close any possibly open record files."))

(defun closerecord ()
  (declare (special recfile))
  (cond ((or (not (boundp 'recfile)) (null recfile) (not (streamp recfile)))
	 (throwfail "No record file is currently open.")))
  (setq outfiles (remove recfile outfiles))
  (setq echofiles (remove recfile echofiles))
  (setq msgfiles (remove recfile msgfiles))
  (close recfile)
  (setq recfile nil)
  ;;(setq ^R nil)
  )
