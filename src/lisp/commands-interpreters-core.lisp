;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :core)

(deffile commands-interpreters-core
  (part-of BARE)
  (extension lisp)
  (mhelp "Functions in the CORE package to do with command interpreters."))

(defun consp-interpreter (command)
  (declare (special rest))
  (let ((carcmd (car command)))
       (cond ((null expertflag) (throwfail "Unknown command for flag."))
             ((and (symbolp carcmd) (not (fboundp carcmd)))
              (setq rest nil)
              `(complain ',carcmd ": undefined function. "))
              (t `(multiple-prin1 ,command)))))


(defun flag-interpreter (command)
   (declare (special rest))
   (let* ((more-p (> (length rest) 1))
          (new-rest (if more-p (nthcdr 1 rest) nil))
          (args (if more-p (ldiff rest new-rest) rest)))
         (setq rest new-rest)
         `(%catch% ,(if args `(comdecode '(set ,command ,@args))
                             `(comdecode '(setflag ,command)))
                    (fail (complain "Error while setting flag " 
				    ',command "." core::expand-catch-throw)))))

(defun mexpr-interpreter (command)
  (declare (special rest))
  (let* ((no-args (length (get command 'argtypes)))
         (more-p (> (length rest) no-args))
         (new-rest (if more-p (nthcdr no-args rest) nil))
         (args (if more-p (ldiff rest new-rest) rest)))
        (setq rest new-rest)
        `(%catch% (comdecode ',(cons command args))
                  (fail (complain "Error in Top-Level command " ',command "."
                                  expand-catch-throw)))))

(defun misc-interpreter (command)
   (declare (special rest))
   (cond ((null expertflag) (throwfail "Unknown command or flag."))
         ((and (if (symbolp command) (boundp command)) (null rest))
          `(prin1 ,command))
         ((symbolp command)
          (cond ((or (get command 'mhelp) (get command 'mhelp-fn))
		 (msg "Cannot evaluate that... calling HELP " command t t)
		 `(comdecode '(help ,command)))
		((not (fboundp command))
                 `(complain ',command ": undefined variable or function. "))
                (t (let ((hxrest rest))
                         (setq rest nil)
                        `(multiple-prin1 ,(cons command hxrest))))))
         (t (setq rest nil) `(multiple-prin1 ,command))))

                  

(defun ed-command-interpreter (cmd)
  (do ((cmdlist nil (cons nextcmd cmdlist))
       (cmd cmd rest)
       (command (car cmd) (car rest))
       (rest (cdr cmd) (cdr rest))
       (nextcmd nil))
      ((null cmd)
       `(progn (setq edt-written-p nil) ,@(nreverse cmdlist)))
    (declare (special rest))
    (setq nextcmd
	  (cond ((and (integerp command) (zerop command))
		 `(move-up))
		((consp command) (consp-interpreter command))
		((and (symbolp command)
                       (or (get command 'edop) (get command 'wffop)))
		 (cond ((get command 'move-fn)
			`(%catch%
			  (progn
			   (setq temp
				 (funcall ',(get command 'alias) edwff))
			   (push edwff wffstack)
			   (push ',(get command 'move-fn) cmdstack)
			   (setq edwff temp))
				  (fail (complain expand-catch-throw t)
					(complain "Operation " ',command
						  " not performed."))))
		       ((and (get command 'edop)
			     (getkey command (get command 'alias) 'print-op))
			`(funcall ',(get command 'alias) edwff))
		       (t (let* ((no-args (length (getkey command
							  (get command 'alias)
							  'argtypes)))
				 (more-p (> (length rest) no-args))
				 (new-rest (if more-p (nthcdr no-args rest)
					       nil))
				 (args (if more-p (ldiff rest new-rest)
					   rest)))
			    (setq rest new-rest)
			    `(%catch%
			      (progn
			       (setq temp
				     (opdecode ',(cons command args)))
			       ,(cond ((eq (get command 'result->) 'edwff)
				       `(progn
					 (setq edwff temp)
					 (write-edwff edwff ',command)))
				      ((or (eq (get command 'result->)
					       'execute)
					   (eq 'ed-command
					       (getkey command
						       (get command 'alias)
						       'resulttype)))
				       `(eval (ed-command-interpreter temp)))
				      ((eq (get command 'result->) 'ignore)
				       `nil)
				      ((get command 'result->) 
				       `(funcall ',(get command 'result->)
						 temp))
				      ((get
					(getkey command (get command 'alias)
						'resulttype)
					'printfn)
				       `(funcall
					 ',(get (getkey command
							(get command 'alias)
							'resulttype)
						'printfn)
					 temp))))
			      (fail (complain expand-catch-throw t)
				    (complain "Operation not performed.")))))))
		((and (symbolp command)
                      (or (get command 'mexpr) (get command 'reviewcmd)))
                 (mexpr-interpreter command))
		((and (symbolp command) (get command 'flag))
                      (flag-interpreter command))
                (t (misc-interpreter command))))))
