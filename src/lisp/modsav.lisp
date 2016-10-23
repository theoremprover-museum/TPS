;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :core)
(part-of SAVING-MODES)

;;;
;;; File: Modsav
;;; Package: Saving-Modes
;;;
;;; defines DEFINE-MODE which can be used to define and then save
;;; a mode (collection of flags with their settings).
;;;
;;; DEFINE-MODE will append the mode definition to a file on your
;;; directory (by default: MODES.INI) to which you may refer inside
;;; your TPS2.INI file.

(deffile modsav
  (part-of saving-modes)
  (extension clisp)
  (mhelp "Allows definition of modes of flag settings."))

(context flag-modes)

(defreview define-mode
  (argnames mode-name help-string subjectlist filename)
  (argtypes symbol string subjectlist filespec)
  (arghelp "Name for New Mode" "Help for New Mode"
	  "Subjects to be Included in Mode"
	  "File to Append Mode Definition to")
  (defaultfns (lambda (%1 %2 %3 filename)
		(when (eq filename '$)
		      (setq filename
			    (make-pathname% :name "modes" :type "ini")))
		(list %1 %2 %3 filename)))
  (mhelp "Define a mode by interactively supplying the flag settings. This 
command is now obsolete and will be phased out in a future release of TPS.
Use the library command INSERT to save modes."))

(defun define-mode (mode-name help-string subjectlist filename)
  (when (equal subjectlist '(all))
    (setq subjectlist (remove-if-not #'symbolp global-subjectlist)))
  (write-mode mode-name help-string (get-mode-flags subjectlist) filename))

(defun write-mode (mode-name help-string write-flags filename)
  (reroute-output-append filename (make-pathname% :name "modes" :type "ini")
    (msg t "(DEFMODE " mode-name
	 t (t 2) "(FLAG-SETTINGS")
    (dolist (flag write-flags)
      (msg t (t 3))
      (if (symbolp flag)
	  (progn (msg "(") (prin1 flag) (msg " ")
		 (let ((flag-value (eval flag)))
		   ;; We need to bind the flag-value, because the
		   ;; following in-mode will locally change the value
		   ;; of some flags.
		   ;;(in-mode re-read)  ;Probably not defined by the 
					; time this file is loaded.
		   (funcall (get (get flag 'flagtype) 'printfn)
			    flag-value))
		 (msg ")"))
	  (prin1 flag)))
    (msg ")" t (t 2) "(MHELP ")
    (prin1 help-string) (msg "))" -2)))


;;; The next section defines SAVE-FLAGS, which is like DEFINE-MODE,
;;; but saves all flag setting of the given subjects automatically.


(defreview save-flags
  (argnames mode-name help-string subjectlist filename)
  (argtypes symbol string subjectlist filespec)
  (arghelp "Name for New Mode" "Help for New Mode"
	  "Subjects to be Included in Mode"
	  "File to Append Mode Definition to")
  (defaultfns (lambda (%1 %2 subjectlist filename)
		(when (eq filename '$)
		  (setq filename
			(make-pathname% :name "modes" :type "ini")))
		(when (eq subjectlist '$)
		  (setq subjectlist '(all)))
		(list %1 %2 subjectlist filename)))
  (mhelp "Define a mode by the current flag settings. This command is now
obsolete and will be phased out in a future release of TPS.Use the library 
command INSERT to save modes."))

(defun save-flags (mode-name help-string subjectlist filename)
  (when (equal subjectlist '(all))
    (setq subjectlist (remove-if-not #'symbolp global-subjectlist)))
  (write-mode mode-name help-string
	      (let ((write-flags nil))
		(dolist (subject subjectlist write-flags)
		  (dolist (flag global-flaglist)
		    (when (and (symbolp flag)
			       (member subject (get flag 'subjects)))
		      (pushnew flag write-flags)))))
	      filename))

