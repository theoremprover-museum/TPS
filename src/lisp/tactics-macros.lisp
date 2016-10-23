;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of tactics)

(context tactics)
(deffile tactics-macros
  (extension lisp)
  (part-of tactics)
  (mhelp "Functions and macros needed by tactics and tacticals."))

(eval-when (load compile eval)
(defvar *tactic-use-list* '(nat-ded etree-nat mate-srch ext-seq))
)

(eval-when (compile load eval)
(defvar *tactic-mode-list* '(auto interactive))
)
(defvar *global-tacticlist*)

(eval
 `(defcategory tactic
    (define defnonsense) ;for deftactic, see below.
    (properties
     ,@(mapcar #'(lambda(x) (list x 'multiple)) *tactic-use-list*)
     )
    (global-list *global-tacticlist*)
    (scribe-one-fn scribe-one-tactic)
    (mhelp-line "tactic")
    (mhelp-fn tactic-mhelp)))


;;; Added this code so that primitive tactics get compiled when
;;; defined.  Also changed prim-tac-p below. DAN 16FEB91
(defmacro deftactic (name &rest rest)
  (let ((defn-list
	 (mapcan #'(lambda (x)
		     (let* ((use (car x))
			    (defn (cadr x))
			    (help (caddr x))
			    (fn (if (and (listp defn)
					 (eq 'lambda (car defn)))
				    (intern (concatenate 'string
					      (symbol-name name)
					      "--" (symbol-name use)
					      "--FN")))))
		       (list `(setf (get ',name ',use)
				',(list (or fn defn) help))
			     (if fn
				 `(defun ,fn ,@(cdr defn))))))
		 rest)))
    `(progn ,@defn-list (setf (get ',name 'tactic) t) 
	    (pushnew (cons 'tactic current-context) (get ',name 'core::contexts))
	    (pushnew ',name *global-tacticlist*))))

(defvar *global-tacticallist*)
(defcategory tactical
  (define deftactical)
  (properties
   (defn read-tactical-defn)
   (mhelp single))
  (global-list *global-tacticallist*)
  (mhelp-line "tactical")
  (mhelp-fn princ-mhelp))

(defun read-tactical-defn (tacl-defn name)
  (let ((tacl-defn (car tacl-defn)))
    (if (or (prim-tacl-p tacl-defn)
	    (compound-tacl-p tacl-defn))
	tacl-defn
      (complain "Defn ~S of tactical ~A is not valid." tacl-defn name))))

(defflag tactic-verbose
  (flagtype symbol)
  (default med)
  (subjects tactics transmit)
  (mhelp "Determines which of the three levels of verbosity will be used:
MAX -- prints the message returned by each tactic called, even if it fails.
MED -- prints messages only when tactic succeeds.
MIN -- prints nothing."))


(defun tactic-output (msg success)
  (case tactic-verbose
     (max (msgf msg t))
     (med (if success (msgf msg t)))
     (min )))

(eval-when (compile load eval)
(defun tactic-use-p (x)
  (member x *tactic-use-list*))
)

(eval-when (compile load eval)
(defun tactic-mode-p (x)
  (member x *tactic-mode-list*))
)

(eval-when (compile load eval)
(defflag tacmode 
  (flagtype tactic-mode)
  (default interactive)
  (subjects tactics transmit)
  (mhelp "The default mode for tactics. " ))
)

(definfo auto
  (mhelp "A flag setting for TACMODE.
Apply tactics in automatic mode (i.e. without user input)."))

(definfo interactive
  (mhelp "A flag setting for TACMODE.
Apply tactics in interactive mode (i.e. prompting the user 
before each application)."))

(definfo nat-ded
  (mhelp "A flag setting for TACUSE.
Use tactics in natural deduction style (i.e. apply them to the
lines of the current dproof)."))

(definfo etree-nat
  (mhelp "A flag setting for TACUSE.
Use tactics in etree-nat translation style (i.e. apply them to the
current eproof to create lines of a natural deduction proof)."))

(definfo mate-srch
  (mhelp "A flag setting for TACUSE.
Unused setting. Eventually, copy and save eproofs with this tactic use."))

(eval-when (compile load eval)
(defflag tacuse
  (flagtype tactic-use)
  (default nat-ded)
  (subjects tactics transmit)
  (mhelp "The default use for tactics. "))
)

(defun prim-tac-p (tactic-defn)
  (or (symbolp tactic-defn)
      (and (listp tactic-defn) (eq (car tactic-defn) 'lambda))))

(defun compound-tac-p (tactic-defn)
  (and (listp tactic-defn) (tactical-p (car tactic-defn))))

(defun prim-tacl-p (tacl-defn)
  (symbolp tacl-defn))

(defun compound-tacl-p (tacl-defn)
  (and (listp tacl-defn) (eq (car tacl-defn) 'tac-lambda)))


(defun get-tac-goal (use)
  (case use
    ((etree-nat nat-ded) (caar (proof-plans dproof)))
    (ext-seq (caar (proof-plans (get *current-seqder* 'ext-seq-der))))
    (otherwise nil)))               ; obviously needs to be completed

(defun get-tac-defn (tacname use)
  (car (get tacname use)))

(defun get-tacl-defn (taclname)
  (get taclname 'defn))

(defun sort-tac-goals (goals use)
  (case use
    ((nat-ded etree-nat)
     (sort (remove-if #'null goals) #'< :key #'linealias))
    ;there should never be a NIL in the goals list, but if there is then throw it out.
    ;MB Tue Sep  3 14:51:23 1996
    (otherwise goals)))



(eval-when (load compile eval)
(defun tactic-p (tactic-exp)
  (and tactic-exp
       (or (member tactic-exp *global-tacticlist*)
	   (and (listp tactic-exp) (or (tactic-p (car tactic-exp))
				       (tactical-p (car tactic-exp)))))))
)

(eval-when (load compile eval)
(defun tactical-p (taclname)
  (member taclname *global-tacticallist*))
)

(defun expand-tacl-defn (tactic-list tacl-defn)
  (let ((alist (mapcar #'cons (cadr tacl-defn) tactic-list)))
    (sublis alist (caddr tacl-defn))))



(defun apply-tactic (tac &key ((:use tacuse) tacuse) ((:mode tacmode) tacmode)
                              ((:goal goal) nil goalp))
   (if (not (tactic-p tac))
      (progn
       (tactic-output (conc-strings (if (symbolp tac)
					(symbol-name tac)
				      (princ-to-string tac))
				    " is not a tactic.  Aborted.")
		      t)
       (values (if goal (list goal))
	       (conc-strings
		(if (symbolp tac)
		    (symbol-name tac)
		    (princ-to-string tac))
		" is not a tactic.  Aborted.") 'xx-abort))
      (progn
       (when (and (not goalp)
		  (or (not (listp tac))
		      (not (member :goal tac))))
	     (setq goal (get-tac-goal tacuse)))
       (cond ((atom tac)
	      (let ((tac-defn (get-tac-defn tac tacuse)))
	       (cond ((prim-tac-p tac-defn)
		      (if (null goal)
			  (values goal "Goals exhausted." 'complete)
			(funcall tac-defn goal)))
		     ((compound-tac-p tac-defn)
		      (apply-tactical goal tac-defn))
		     (t 
		      (tactic-output
			 (conc-strings "Tactic " (symbol-name tac)
					      " not defined for use "
					     (symbol-name tacuse) 
					     ". Aborted.")
			 t)
		      (values (list goal)
			      (conc-strings "Tactic " (symbol-name tac)
					     " not defined for use "
					     (symbol-name tacuse) 
					     ". Aborted.")
			      'xx-abort)))))


	    ((tactic-p (car tac))
	     (if (member :goal tac)
		 (apply #'apply-tactic tac)
	       (apply #'apply-tactic (append tac (list :goal goal)))))
	    ((tactical-p (car tac))
	     (apply-tactical goal tac))))))

;;; Assumed that we know that (car tac-defn) is a tactical

(defun apply-tactical (goal tac-defn)
  (let ((tacl-defn (get-tacl-defn (car tac-defn))))
    (cond ((prim-tacl-p tacl-defn)
	   (funcall tacl-defn goal (cdr tac-defn)))
	  ((compound-tacl-p tacl-defn)
	   (apply-tactical goal (expand-tacl-defn (cdr tac-defn) tacl-defn)))
	  (t
	   (values nil (concatenate 'string "Tactical "
				    (princ-to-string (car tac-defn))
				    " improperly defined.")
		   'xx-abort)))))





(defmexpr use-tactic
  (argtypes tactic-exp tactic-use tactic-mode)
  (argnames tac tac-use tac-mode)
  (arghelp "Tactic name/expression" "Use" "Mode")
  (defaultfns 
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '($ $ $) rest))
   (lambda (%1 %2 %3)
     (declare (special default-tactic))
     (list (if (eq %1 '$) default-tactic %1)
	   (if (eq %2 '$) tacuse %2) (if (eq %3 '$) tacmode %3))))
  (mhelp "Use a tactic on the current goal. The default tactic
is given by the flag DEFAULT-TACTIC."))

(defun use-tactic (tac tac-use tac-mode)
  (apply-tactic tac :use tac-use :mode tac-mode 
		:goal (get-tac-goal tac-use))
  (print-routines))

(defmexpr echo
  (argtypes string)
  (argnames echothing)
  (arghelp "String to echo")
  (defaultfns 
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '("") rest))
   (lambda (%1) (list %1)))
  (mhelp "Echo a string."))

(defun echo (str)
  (msg str))

(defun save-object (goal)
  (case tacuse
    ((etree-nat nat-ded) (save-proof goal))
    ((mate-srch)
     (save-eproof goal))
))



(defun save-proof (goal)
  (declare (ignore goal))
  dproof)

(defun save-eproof (goal)
  goal)

(defun copy-object (goal)
  (case tacuse
    ((etree-nat nat-ded) (copy-proof goal))
    ((mate-srch) (copy-exp-proof goal))))

;;; do we need to have expansion proof already defined? Yes, put in sep. file.
(defun copy-exp-proof (goal)
  (let ((newproof (copy-eproof goal)))
    (setf (eproof-mating-list newproof) nil)))



;;; The KEY property stores an alist used in
;;; subbing the new symbols for the old ones.

(defun copy-proof (goal)
  (declare (ignore goal))
  (let ((newproof (gensym "PROOF"))
	(key (mapcar #'(lambda (x) (cons x (gensym "LINE")))
		     (proof-lines dproof))))
    (setf (proof-plans newproof) (sublis key (proof-plans dproof)))
    (setf (proof-key newproof) key)
    (setf (proof-linealiases newproof) (sublis key (proof-linealiases dproof)))
    (setf (proof-lines newproof) (sublis key (proof-lines dproof)))
    (setf (proof-nodealiases newproof) (sublis key (proof-nodealiases dproof)))
    (setf (proof-assertion newproof) (proof-assertion dproof))
    (setf (nextplan-no newproof) (nextplan-no dproof))
    (dolist (x key) (setf (plist (cdr x))
			  (nsublis key (copy-tree (plist (car x))))))
    (setq dproof newproof)
    (find-all-gaps) ;;; fill in gaps property
    newproof))

(defun restore-object (old goal)
  (case tacuse
    ((etree-nat nat-ded) (restore-proof old goal))))


(defun restore-proof (oldproof goal)
  (declare (ignore goal))
  (setq dproof oldproof))

(defun update-object (new old)
  (case tacuse
    ((etree-nat nat-ded) (update-proof new old))))


(defun update-proof (newproof oldproof)
  (let ((revkey (mapcar #'(lambda (x) (cons (cdr x) (car x))) 
			(proof-key newproof))))
    (setf (proof-plans oldproof)
	  (nsublis revkey (proof-plans newproof)))
    (dolist (x (proof-lines newproof))
      (setf (plist (if (cdr (assoc x revkey)) (cdr (assoc x revkey)) x))
		(nsublis revkey (plist x))))
    (setf (proof-lines oldproof) (nsublis revkey (proof-lines newproof)))
    (setf (proof-linealiases oldproof)
	  (nsublis revkey (proof-linealiases newproof)))
    (setf (proof-nodealiases oldproof) (nsublis revkey
					       (proof-nodealiases newproof)))
    (setf (nextplan-no oldproof) (nextplan-no dproof))
    (setq dproof oldproof)
    (setf (gaps) nil)
    (find-all-gaps)))

(defun get-new-goal (goal new-object)
  (case tacuse
    ((etree-nat nat-ded) (get-new-proof-goal goal new-object))))

(defun get-new-proof-goal (pline newproof)
  (cdr (assoc pline (proof-key newproof))))

(defun update-goals (goals new-object)
  (case tacuse
    ((etree-nat nat-ded) (update-proof-goals goals new-object))))

(defun update-proof-goals (plines newproof)
  (let ((revkey (mapcar #'(lambda (x) (cons (cdr x) (car x))) 
			(proof-key newproof))))
    (nsublis revkey plines)))

(defun still-goal (goal)
  (case tacuse
    ((etree-nat nat-ded) (still-proof-goal goal))))

(defun still-proof-goal (line)
  (planp line))

(defun tactic-mhelp (tactic category)
  (declare (ignore category))
  (dolist (prop (mapcar #'(lambda (use) (cons use (get tactic use)))
			*tactic-use-list*))
    (when (cadr prop)
      (msgf "As defined for use " (car prop) ":")
      (if (compound-tac-p (cadr prop))
	  (pprint (cadr prop))
	  (msg " is a primitive tactic."))
      (if (and (compound-tac-p (cadr prop)) (not short-help))
	  (progn (msgf "the above expands into primitive tactics as follows:")
		 (pprint (tactic-mhelp-expand (car prop) (cadr prop)))))
      (when (caddr prop)
	(msgf (caddr prop)))
;the following two lines are new...
;discovered when writing SEARCH that some tactics have mhelp even though they shouldn't... MB 6/2/93
      (when (get tactic 'mhelp) 
	    (msg t (core::tacktogether (get tactic 'mhelp)))))))

(defun tactic-mhelp-expand (use tac)
  (let ((list nil))
    (dolist (tactic tac)
	    (setq list (append list (list (single-tactic-expand tactic :use use)))))
    list))

(defun single-tactic-expand (tac &key ((:use tacuse) tacuse))
   (if (not (tactic-p tac))
      (if (listp tac) (tactic-mhelp-expand tacuse tac) tac)
     (cond ((atom tac)
	    (let ((tac-defn (get-tac-defn tac tacuse)))
	      (cond ((prim-tac-p tac-defn) tac)
		    ((compound-tac-p tac-defn) (tactic-mhelp-expand tacuse tac-defn)))))
	   (t (tactic-mhelp-expand tacuse tac)))))

(defun scribe-one-tactic (tactic)
  (msgf)
  (maint::index-item tactic 'tactic)
  (msg " @\\ Defined for the following uses:" t)
  (msg "@begin(description,spread 0)" t)
  (dolist (prop (mapcar #'(lambda (use) (cons use (get tactic use)))
			*tactic-use-list*))
    (when (cadr prop)
      (msgf  (car prop) ": ")
      (if (compound-tac-p (cadr prop))
	  (progn 
	    (msgf "@begin(flushleft, font smallbodyfont,facecode f, size -2)")
	    (pprint (cadr prop))
	    (msgf "@end(flushleft)" t))
	  (msg " is a primitive tactic."))
      (when (caddr prop)
	(msgf (caddr prop)))
      (msg -2)))
  (msg "@end(description)" t))


