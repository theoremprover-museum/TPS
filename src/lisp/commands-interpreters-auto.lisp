;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :auto)

(deffile commands-interpreters-auto
  (part-of BARE)
  (extension lisp)
  (mhelp "Functions in the AUTO package to do with command interpreters."))

(defun mateop-interpreter (command)
   (declare (special rest))
   (let* ((alias (get command 'mate-alias))
		        (result (get command 'mate-result->))
			(resulttype (getkey command alias 'resulttype)))
		   (cond ((get command 'mate-move-fn)
			  `(%catch%
			    (progn
			      (setq *mate-temp* 
				(funcall ',alias current-topnode))
			      (push current-topnode nodestack)
			      (setq current-topnode *mate-temp*))
			    (fail (complain core::expand-catch-throw t)
				  (complain "Operation " ',command
					    " not performed."))))
			 ((and (get command 'mateop)
			       (getkey command alias 'print-op))
			  `(funcall ',alias current-topnode))
			 (t (let* ((no-args (length (getkey command
							    alias
							    'argtypes)))
				   (more-p (> (length rest) no-args))
				   (new-rest (if more-p (nthcdr no-args rest)
						 nil))
				   (args (if more-p (ldiff rest new-rest)
					     rest)))
			      (setq rest new-rest)
			      `(%catch%
				(progn
				  (setq *mate-temp*
				    (mate-opdecode ',(cons command args)))
				  ,(cond ((eq result 'current-topnode)
					  `(setq current-topnode *mate-temp*)
					  )
					 ((or (eq result
						  'execute)
					      (eq 'mate-command
						  resulttype))
					  `(eval (mate-command-interpreter 
						  *mate-temp*)))
					 ((eq result 'ignore)
					  'nil)
					 (result 
					  `(funcall ',result
						    *mate-temp*))
					 ((get
					   resulttype
					   'printfn)
					  `(funcall
					    ',(get resulttype
						   'printfn)
					    *mate-temp*))))
				(fail (complain core::expand-catch-throw t)
				      (complain "Operation not performed."))))))))

(defun mate-command-interpreter (cmd)
  (declare (special displaywff printdepth ppwfflag  prev-node nodestack
		    cmdstack strong-defaults  printtypes current-eproof
		    current-topnode))
  (do ((cmdlist nil (cons nextcmd cmdlist))
       (cmd cmd rest)
       (command (car cmd) (car rest))
       (rest (cdr cmd) (cdr rest))
       (nextcmd nil))
       ((null cmd)
	`(progn (setq mate-written-p nil) ,@(nreverse cmdlist)))
    (declare (special rest))
    (setq nextcmd
	  (cond ((integerp command)
		 (if (zerop command)
		     '(negate-last-move current-topnode)
		     `(move-to-successor ,command)))
		((consp command) (consp-interpreter command))
		((and (symbolp command)
		      (or (get command 'mateop) (get command 'wffop)))
                 (mateop-interpreter command))
		((and (symbolp command)
		      (or (get command 'mexpr) (get command 'reviewcmd) (get command 'monitorfn)))
                 (mexpr-interpreter command))
		((and (symbolp command) (get command 'flag))
                 (flag-interpreter command))
                (t (misc-interpreter command))))))
