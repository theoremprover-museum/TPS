;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :maint)
(part-of tps-modules)

(deffile pck
  (part-of tps-modules)
  (extension clisp)
  (mhelp "Contains commands for loading modules."))

(context modules-in-tps)


;;; (MODULES (p1 p2 ...)) loads modules p1 p2 ..., if they are not
;;; loaded already.

(defmexpr modules
  (argtypes modulelist)
  (argnames modulelist)
  (mhelp "Load the specified modules."))

(defun modules (modulelist)
  (dolist (module modulelist (reorganize))
    (load-module module)))
					
(defun clean-global-list (lis warnlis)
  (if (consp (car lis))
      (let ((truwarn ;;(set-of warns warnlis (not (member warns lis)))
	     (remove-if #'(lambda (warns) (member warns lis)) warnlis)))
	(if truwarn 
	    (progn
	     (complain (l truwarn)
		       (if (cddr truwarn) "do not have valid contexts."
			   "does not have a valid context."))
	     (append truwarn lis))
	    lis))
      (clean-global-list
       (cdr lis) (if (member (car lis) warnlis)
		     warnlis (cons (car lis) warnlis)))))

(defun loaded-mods ()
  (msg (l (loaded-modules))))

(defun unloaded-mods ()
  (msg (l (unloaded-modules))))

(defmexpr unloaded-mods
  (mhelp "Returns list of unloaded modules."))

(defmexpr loaded-mods
  (mhelp "Returns list of loaded modules."))

(defun unloaded-modules ()
  (setq global-modulelist (clean-global-list global-modulelist nil))
  (sort (remove-if #'(lambda (mod) (or (consp mod) (module-loaded-p mod)))
		   global-modulelist)
	#'string<))


(defun loaded-modules ()
  (setq global-modulelist (clean-global-list global-modulelist nil))
  (sort (remove-if #'(lambda (mod)
		       (or (consp mod) (not (module-loaded-p mod))))
		   global-modulelist)
	#'string<))
