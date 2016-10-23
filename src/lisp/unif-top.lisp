;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of UNIFICATION-interface)
(context unification)
;;;
;;; File: UNIF-TOP
;;; Package: UNIFICATION-interface
;;;
;;; Contents define unification top-level
;;;

(deffile unif-top
  (part-of unification-interface)
  (extension lisp)
  (mhelp "Contents define unification top-level."))

(context unification)

;;;
;;; Parameters and flags follow.
;;;

(context unification)

(defvar root-node)
(defvar subst-hash-table)
(defvar prev-node)
(defvar dpairlist nil)

;;;
;;; UNIFTOP is the unification top-level, locally binding all the important 
;;; special variables.
;;;

(defun uniftop ()
  (let ((top-prompt-fn #'unif-top-prompt)
	;;(unif-level (+ unif-level 1))
	(command-interpreter #'unif-command-interpreter)
	(print-* #'unif-print-*)
	;;(command-ctree unif-command-ctree)
	(top-level 'unif-top)
	(unification-toplevel t))
    (declare (special top-prompt-fn  print-* top-level command-interpreter
		      ;;command-ctree unif-level
		      ))
    (secondary-top)))

;;;
;;; The following are the primary and secondary prompts.
;;;

(defun unif-top-prompt (id)
  (format nil "<Unif~A>" id))

;;;
;;; UNIF-COMMAND-INTERPRETER returns a LISP form
;;;


(defun unif-command-interpreter (cmd)
  (cond	((and (null (cdr cmd)) (atom (car cmd)))
	 (setq cmd (car cmd))
	 (cond ((integerp cmd)
		(if (minusp cmd) nil `(nth-son ,cmd)))
	       ((symbolp cmd)
		(cond ((get cmd 'unifop)
		       `(unif-opdecode (list ',cmd)))
		      ((get cmd 'mexpr)
		       `(comdecode (list ',cmd)))
		      ((get cmd 'reviewcmd)
		       `(comdecode  (list ',cmd)))
		      ((get cmd 'flag)
		       `(comdecode '(setflag ,cmd)))
		      ((boundp cmd) `(multiple-prin1 ,cmd))
		      ((null expertflag) (throwfail "Unknown Command."))
		      ((or (get cmd 'mhelp) (get cmd 'mhelp-fn))
		       (msg "Cannot evaluate that... calling HELP " cmd t t)
		       `(comdecode '(help ,cmd)))
		      ((fboundp cmd) `(multiple-prin1 (list ,cmd)))
		      (t (throwfail ";" cmd " - Unbound variable."))))
	       (t `(multiple-prin1 (list ,cmd)))))
	((and expertflag (null (cdr cmd)))
	 (cond ((not (and (symbolp (caar cmd)) (fboundp (caar cmd))))
		(throwfail ";" (car cmd) " - Undefined function."))
	       (t `(multiple-prin1 ,(car cmd)))))
	((not (symbolp (car cmd)))
	 (throwfail "Illegal input - must be EXPERT to evaluate Lisp form."))
	((get (car cmd) 'unifop)
	 `(unif-opdecode ',cmd))
	((get (car cmd) 'mexpr)
	 `(comdecode ',cmd))
	((get (car cmd) 'reviewcmd)
	 `(comdecode  ',cmd))
	((get (car cmd) 'flag)
	 (if (cdr cmd) `(comdecode '(set ,@cmd))
	     `(unif-opdecode '(setflag ,@cmd))))
	((null expertflag)
	 (throwfail "Unknown command."))
	((not (fboundp (car cmd)))
	 (throwfail ";" (car cmd) " - Undefined function."))
	(t  `(multiple-prin1 ,cmd))))

;;;
;;; UNIF-PRINT-* prints the current topnode, if it is not the same as before
;;; the last command.
;;;

(defun unif-print-* (result)
  (declare (special prev-node)
	   (ignore result))
  (unless (eq current-topnode prev-node)
    (show-unode (node-print-name current-topnode))
    (setq prev-node current-topnode)))

(context subtoplevels)

(defunifop leave
  (unif-mainfns leave-unif)
  (mhelp "Exit unification."))

(defun leave-unif ()
  (%throw% "Normal exit from unification toplevel." exit-inferior-top))

(context unification)

(defun unif-opdecode (command)
  (let ((keyword (car command)) mainfn result)
    (multiple-value-bind
	(internal-arglist external-arglist)
	(prompt-values keyword
		       (copy (cdr command))
		       (get keyword 'unif-argtypes)
		       (mapcar #'(lambda (x) (declare (ignore x)) nil)
			       (get keyword 'unif-argtypes))
		       nil (get keyword 'unif-defaultfns) nil
		       (get keyword 'unif-argnames)
		       (get keyword 'unif-arghelp))
      (declare (ignore external-arglist))
      (let ((appfn (get keyword 'unif-applicablep)))
	(when (and appfn (not (apply appfn internal-arglist)))
	  (throwfail keyword " not applicable.")))
      (setq mainfn (or (get keyword 'unif-mainfns) keyword))
      (%catch% (setq result (apply mainfn internal-arglist))
	       (fail (complain f "Error from " mainfn ".  " 
                               core::expand-catch-throw)
		     (throwfail "Operation aborted.")))
      result)))
