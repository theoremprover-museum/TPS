;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of RULES)

;;;
;;; File: Rule-Build-Default
;;; Package: Rules
;;;
;;; defines the function which build the definition of the function
;;; <rule>-defaults.
;;;

(part-of rules)

(deffile rule-build-default
  (part-of rules)
  (extension lsp)
  (mhelp "Defines the functions which build the definition of the function
<rule>-defaults."))

(defun build-rule-defaults (rule)
  (let ((propsymalist (get rule 'propsymalist))
	(lineargnames (get rule 'lineargnames))
	(wffargnames (get rule 'wffargnames))
	(hypargnames (get rule 'hypargnames))
	(argnames (get rule 'argnames))
	(support-transformation (get rule 'support-transformation))
	(unique-linenames (get rule 'unique-linenames))
	(rule-default-fn-name (prepend rule '-defaults))
	(rule-hyp-default-fn-name (prepend rule '-hyp-defaults))
	(multi-default-lines (get rule 'multi-default-lines))
	function-defn)
    (let ((meta-var-unique-names (mapcar #'cdr propsymalist)))
      (setq function-defn
	    `(defun ,rule-default-fn-name ,argnames
	       (declare (special strong-defaultlist))
	       (let ((wffbindings nil) strong-hypdefaults)
		 (declare (special wffbindings strong-hypdefaults))
		 (macro-do ((quoted metavar ,meta-var-unique-names)
			    (unquoted wffarg ,wffargnames))
		   (when (specified-p wffarg)
			 (push (cons metavar wffarg) wffbindings)))
		 (macro-do ((quoted unique-line ,unique-linenames)
			    (unquoted linearg ,lineargnames))
		   (if (and (not (eq linearg '$))
			    (existent-p linearg))
		       ;; here we could go a step further:
		       ;; could catch an error or even fill in
		       ;; the meta-assertion in the macro-do.
		       (match-bind (get unique-line 'meta-assertion)
				   (get (numalias linearg) 'assertion))))
		 (macro-do ((quoted metalabel ,(set-of mv meta-var-unique-names
						 (eq (get mv 'flavor) 'meta))))
		   (let ((wffval (wffeval metalabel)))
		     (when (not wffval)
			   (%catch% (meta-subst metalabel)
				  (fail nil)))))
		 (macro-do ((quoted metavar ,meta-var-unique-names)
			    (unquoted wffarg ,wffargnames))
		   (let ((wffval (wffeval metavar)))
		     (when wffval (setq wffarg wffval))))
		 ;;So far we have established the strong defaults for the
		 ;; wff arguments for the specified lines.
		 ;; Next comes figuring out strong defaults for the
		 ;; hypotheses, but only if all line numbers have been
		 ;; determined!
		 ;; ---- Hypotheses defaults here ----
		 (when (member '$ (list ,@hypargnames))
		       (if (not (member '$ (list ,@lineargnames)))
			   (setq-destruct ,hypargnames
					  (,rule-hyp-default-fn-name
					   ,@argnames))
			   (setq strong-hypdefaults
				 (mapcar #'specified-p (list ,@hypargnames)))))
		 ;;Next comes figuring out defaults for the lines.
		 ;;First for the lines which are supposed to exist, by default
		 (setq-destruct
		  ,(car support-transformation)
		  (line-no-defaults-from (eval-destruct
					  ,(car support-transformation))))
		 (when (not (member '$ (list
					,@(apply #'append
						 (car
						  support-transformation)))))
		       (,@(if multi-default-lines
			      `(setq-destruct-multi ,multi-default-lines)
			      `(setq-destruct))
			,(cadr support-transformation)
			(line-no-defaults-to
			 (eval-destruct ,(car support-transformation))
			 (eval-destruct ,(cadr support-transformation)))))
		 ;; Now we tell COMDECODE which are the strong defaults.
		 (setq strong-defaultlist
		       (append ',(mapcar #'(lambda (x)
					     (declare (ignore x)) nil)
					 lineargnames)
			       (mapcar #'specified-p (list ,@wffargnames))
			       strong-hypdefaults))
		 ;; Finally we must return the list of all arguments, with
		 ;; defaults filled in as far as they could be determined.
		 (list ,@argnames))))
    (putprop rule function-defn 'default-fn-defn)
    t)))


(defun build-rule-hyp-defaults (rule)
  (let ((lineargnames (get rule 'lineargnames))
	(wffargnames (get rule 'wffargnames))
	(hypargnames (get rule 'hypargnames))
	(argnames (get rule 'argnames))
	(m-line-args (get rule 'm-line-args))
	(hyp-restrict (get rule 'hyp-restrict))
	(rule-hyp-default-fn-name (prepend rule '-hyp-defaults))
	(dlines (get rule 'dlines))
	(hlines (get rule 'hlines))
	(plines (get rule 'plines))
	hyp-function-defn
	(forms nil)
	(subforms nil))
    (if hyp-restrict
    (push `(macro-do ((unquoted linearg ,lineargnames)
		      (unquoted hyparg ,hypargnames))
	     (when (existent-p linearg)
		   (if (specified-p hyparg)
		       (when (not (contained-p hyparg (hypnums linearg)))
			     (throwfail "Hypothesis specified for line "
					(linearg . line) " are not the same as the one in the proof."))
		       (setq hyparg (hypnums linearg)))))
	  forms)
    (push `(macro-do ((unquoted linearg ,lineargnames)
		      (unquoted hyparg ,hypargnames))
	     (when (existent-p linearg)
		   (if (specified-p hyparg)
		       (when (not (set-eq hyparg (hypnums linearg)))
			     (throwfail "Hypothesis specified for line "
					(linearg . line) " are not the same as the one in the proof."))
		       (setq hyparg (hypnums linearg)))))
	  forms))
    (do ((m-line-args m-line-args (cdr m-line-args))
	 (hypargnames hypargnames (cdr hypargnames))
	 (m-line-arg nil) 
	 (linearg nil) 
	 (hyparg nil))
	((null m-line-args))
      (setq m-line-arg (car m-line-args))
      (setq linearg (caar m-line-args))
      (setq hyparg (car hypargnames))
      (cond ((member linearg dlines)
	     (if (not (null (cadr m-line-arg)))
		 (push 
		  `(when (specified-p ,hyparg)
			 (setq hlower
			       (join-h hlower (set-difference ,hyparg
						       (list ,@(caddr m-line-arg))))))
		  forms)
		 (if hyp-restrict
		     (push 
		      `(when (specified-p ,hyparg)
			     (when (not (contained-p ,hyparg
						(list ,@(caddr m-line-arg))))
				   (throwfail "Illegal set of hypotheses for line "
					      (,linearg . line) ".")))
		      forms)
		   (push 
		    `(when (specified-p ,hyparg)
			   (when (not (set-eq ,hyparg
					      (list ,@(caddr m-line-arg))))
				 (throwfail "Illegal set of hypotheses for line "
					    (,linearg . line) ".")))
		    forms))))
	    ((member linearg hlines)
	     (if (not (null (cadr m-line-arg)))
		 (if hyp-restrict
		     (push 
		      `(when (specified-p ,hyparg)
			     ,(if treat-hlines-as-dlines
				  `(setq hlower
					 (join-h hlower (set-difference ,hyparg
									(list ,@(caddr m-line-arg)))))
				`(when (not (contained-p ,hyparg
							 (list ,linearg)))
				       (throwfail "Illegal hypotheses for hyp line " (,linearg . line) "."))))
		      forms)
		   (push 
		    `(when (specified-p ,hyparg)
			   ,(if treat-hlines-as-dlines
				`(setq hlower
				       (join-h hlower (set-difference ,hyparg
								      (list ,@(caddr m-line-arg)))))
			      `(when (not (set-eq ,hyparg
						  (list ,linearg)))
				     (throwfail "Illegal hypotheses for hyp line " (,linearg . line) "."))))
		  forms))
	       (if hyp-restrict
		 (push 
		  `(when (specified-p ,hyparg)
			 ,(if treat-hlines-as-dlines
			      `(when (not (contained-p ,hyparg
						(list ,@(caddr m-line-arg))))
				   (throwfail "Illegal hypotheses for hyp line " (,linearg . line) "."))
			      `(when (not (contained-p ,hyparg
						(list ,linearg)))
				   (throwfail "Illegal hypotheses for hyp line " (,linearg . line) "."))))
		  forms)
		 (push 
		  `(when (specified-p ,hyparg)
			 ,(if treat-hlines-as-dlines
			      `(when (not (set-eq ,hyparg
						(list ,@(caddr m-line-arg))))
				   (throwfail "Illegal hypotheses for hyp line " (,linearg . line) "."))
			      `(when (not (set-eq ,hyparg
						(list ,linearg)))
				   (throwfail "Illegal hypotheses for hyp line " (,linearg . line) "."))))
		  forms))))
	    ((member linearg plines)
	     (if (not (null (cadr m-line-arg)))
		 (push 
		  `(when (specified-p ,hyparg)
			 (setq hupper
			       (meet-h hupper ,hyparg)))
		  forms)
	       (if hyp-restrict
		 (push 
		  `(when (specified-p ,hyparg)
			 (when (not (contained-p ,hyparg
					    (list ,@(caddr m-line-arg))))
			       (throwfail "Illegal hypotheses for line "
					  (,linearg . line) ".")))
		  forms)
		 (push 
		  `(when (specified-p ,hyparg)
			 (when (not (set-eq ,hyparg
					    (list ,@(caddr m-line-arg))))
			       (throwfail "Illegal hypotheses for line "
					  (,linearg . line) ".")))
		  forms))))))
    ;; Now some intermediate stuff before filling in the defaults
    (if (not treat-hlines-as-dlines)
	(push `(when auto-generate-hyps
		     (if (not (eq hupper '$))
			 (setq strong-hypdefaults
			       (mapcar #'(lambda (x)
					   (declare (ignore x))
					   t)
				       (list ,@hypargnames)))
			 ;;; The following is necessary, since sometimes
			 ;;; When the planned line is a variable 'pp it
			 ;;; does not know which lines to prove the new planned
			 ;;; line from, if dlines and hlines are treated
			 ;;; differently.
			 (setq strong-hypdefaults
			       (mapcar #'strong-hyp-p (list ,@hypargnames)
				       (list ,@lineargnames)
				       ',(mapcar #'(lambda (l)
						     (if (member l dlines) t nil))
						 lineargnames)))))
	      subforms))
    (push `(cond ((eq hlower '$) (setq hlower hupper))
		 ((eq hupper '$) (setq hupper hlower))
		 ((not (contained-p hlower hupper))
		  (throwfail "Illegal extra hypotheses in conclusion: "
			     ((set-difference hlower hupper) . linelist) ".")))
	  subforms)
    (push `(when (not auto-generate-hyps)
		 (setq strong-hypdefaults (mapcar #'specified-p
						  (list ,@hypargnames))))
	  subforms)
    ;; Now we fill in the hypotheses
    (do ((m-line-args m-line-args (cdr m-line-args))
	 (hypargnames hypargnames (cdr hypargnames))
	 (m-line-arg nil)
	 (linearg nil)
	 (hyparg nil))
	((null m-line-args))
      (setq m-line-arg (car m-line-args))
      (setq linearg (caar m-line-args))
      (setq hyparg (car hypargnames))
      (cond ((member linearg dlines)
	     (if (not (null (cadr m-line-arg)))
		 (push 
		  `(when (not (specified-p ,hyparg))
			 (setq ,hyparg
			       (ordered-join-h hupper (list ,@(caddr m-line-arg)))))
		  subforms)
		 (push 
		  `(when (not (specified-p ,hyparg))
			 (setq ,hyparg (list ,@(caddr m-line-arg))))
		  subforms)))
	    ((member linearg hlines)
	     (if (not (null (cadr m-line-arg)))
		 (push 
		  `(when (not (specified-p ,hyparg))
			 ,(if treat-hlines-as-dlines
			      `(setq ,hyparg
				   (ordered-join-h hupper (list ,@(caddr m-line-arg))))
			      `(setq ,hyparg (list ,linearg))))
		  subforms)
		 (push `(setq ,hyparg (list ,linearg)) subforms)))
	    ((member linearg plines)
	     (if (not (null (cadr m-line-arg)))
		 (push 
		  `(when (not (specified-p ,hyparg))
			 (setq ,hyparg
			       (ordered-join-h hlower (list ,@(caddr m-line-arg)))))
		  subforms)
		 (push 
		  `(when (not (specified-p ,hyparg))
			 (setq ,hyparg (list ,@(caddr m-line-arg))))
		  subforms)))))
    (if treat-hlines-as-dlines
	(push `(when auto-generate-hyps
		     (setq strong-hypdefaults (mapcar #'specified-p
						      (list ,@hypargnames))))
	      subforms))
    (push `(if (and (eq hlower '$) (eq hupper '$))
	       (setq strong-hypdefaults
		     (mapcar #'specified-p (list ,@hypargnames)))
	       (progn
		,@(nreverse subforms)))
	  forms)
    (setq hyp-function-defn
	  `(defun ,rule-hyp-default-fn-name ,argnames
	     (declare (special strong-hypdefaults)
		      (ignore ,@wffargnames))
	     (let ((hupper '$) (hlower '$))
	       ,@(nreverse forms)
	       (list ,@hypargnames))))
    (putprop rule hyp-function-defn 'hyp-default-fn-defn)
    t))
