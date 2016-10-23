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
;;; File: Rule-Build-Check
;;; Package: Rules
;;;
;;; defines the functions which build the definition of the functions
;;; <rule>-legal-hyps and <rule>-legal-wffs.
;;;

(part-of rules)

(deffile rule-build-check
  (part-of rules)
  (extension lsp)
  (mhelp "Defines the functions which build the definition of the functions
<rule>-legal-hyps and <rule>-legal-wffs."))

(defun build-rule-check (rule)
  (let ((argnames (get rule 'argnames))
	(rule-wff-check-fn-name (prepend rule '-legal-wffs))
	(rule-hyp-check-fn-name (prepend rule '-legal-hyps))
	(rule-check-fn-name (prepend rule '-legal))
	function-defn)
    (setq function-defn
	  `(defun ,rule-check-fn-name ,argnames
	     (let ((rule-hupper nil) (rule-hlower nil))
	       (declare (special rule-hupper rule-hlower))
	       (setq core::check-hyps-again nil)
	       (,rule-hyp-check-fn-name ,@argnames)
	       (,rule-wff-check-fn-name ,@argnames)
	       t)))
    (putprop rule function-defn 'check-fn-defn)
    t))

(defun build-rule-hyp-checks (rule)
  (let ((lineargnames (get rule 'lineargnames))
	(wffargnames (get rule 'wffargnames))
	(hypargnames (get rule 'hypargnames))
	(argnames (get rule 'argnames))
	(hyp-restrict (get rule 'hyp-restrict))
	(m-line-args (get rule 'm-line-args))
	(rule-hyp-check-fn-name (prepend rule '-legal-hyps))
	(dlines (get rule 'dlines))
	(hlines (get rule 'hlines))
	(plines (get rule 'plines))
	hyp-function-defn
	(forms nil)
	(subforms nil))
    (if hyp-restrict
    (push
     `(macro-do ((unquoted linearg ,lineargnames)
		 (unquoted hyparg ,hypargnames))
        (when (and (existent-p linearg)
		   (not (contained-p hyparg (hypnums linearg))))
	      (throwfail "Hypothesis specified for line "
			 (linearg . line) " are not the same as the ones in the proof.")))
     forms)
    (push
     `(macro-do ((unquoted linearg ,lineargnames)
		 (unquoted hyparg ,hypargnames))
        (when (and (existent-p linearg)
		   (not (set-eq hyparg (hypnums linearg))))
	      (throwfail "Hypothesis specified for line "
			 (linearg . line) " are not the same as the ones in the proof.")))
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
		  `(setq hlower
			 (join-h hlower (set-difference ,hyparg
						 (list ,@(caddr m-line-arg)))))
		  forms)
	       (if hyp-restrict
		 (push 
		  `(when (not (contained-p ,hyparg
				      (list ,@(caddr m-line-arg))))
			 (throwfail "Illegal set of hypotheses for line "
				    (,linearg . line) "."))
		  forms)
		 (push 
		  `(when (not (set-eq ,hyparg
				      (list ,@(caddr m-line-arg))))
			 (throwfail "Illegal set of hypotheses for line "
				    (,linearg . line) "."))
		  forms))))
	    ((member linearg hlines)
	     (if (not (null (cadr m-line-arg)))
		 (if hyp-restrict
		 (push 
		  (if treat-hlines-as-dlines
		      `(setq hlower
			     (join-h hlower (hypsetdiff ,hyparg
							,@(caddr m-line-arg))))
		      `(when (not (contained-p ,hyparg
					  (list ,linearg)))
			     (throwfail "Illegal hypotheses for hyp line " (,linearg . line) ".")))
		  forms)
		 (push 
		  (if treat-hlines-as-dlines
		      `(setq hlower
			     (join-h hlower (hypsetdiff ,hyparg
							,@(caddr m-line-arg))))
		      `(when (not (set-eq ,hyparg
					  (list ,linearg)))
			     (throwfail "Illegal hypotheses for hyp line " (,linearg . line) ".")))
		  forms))
	     (if hyp-restrict
		 (push 
		  (if treat-hlines-as-dlines
		      `(when (not (contained-p ,hyparg
					  (list ,@(caddr m-line-arg))))
			     (throwfail "Illegal hypotheses for hyp line " (,linearg . line) "."))
		       `(when (not (contained-p ,hyparg
					  (list ,linearg)))
			     (throwfail "Illegal hypotheses for hyp line " (,linearg . line) ".")))
		  forms)
		 (push 
		  (if treat-hlines-as-dlines
		      `(when (not (set-eq ,hyparg
					  (list ,@(caddr m-line-arg))))
			     (throwfail "Illegal hypotheses for hyp line " (,linearg . line) "."))
		       `(when (not (set-eq ,hyparg
					  (list ,linearg)))
			     (throwfail "Illegal hypotheses for hyp line " (,linearg . line) ".")))
		  forms))))
	    ((member linearg plines)
	     (if (not (null (cadr m-line-arg)))
		 (push 
		  `(setq hupper (meet-h hupper ,hyparg))
		  forms)
	       (if hyp-restrict
		 (push 
		  `(when (not (contained-p ,hyparg
				      (list ,@(caddr m-line-arg))))
			 (throwfail "Illegal hypotheses for line "
				    (,linearg . line) "."))
		  forms)
		 (push 
		  `(when (not (set-eq ,hyparg
				      (list ,@(caddr m-line-arg))))
			 (throwfail "Illegal hypotheses for line "
				    (,linearg . line) "."))
		  forms))))))
    ;; Now some intermediate stuff before filling in the defaults
    (push `(cond ((eq hlower '$) (setq hlower hupper))
		 ((eq hupper '$) (setq hupper hlower))
		 ((not (contained-p hlower hupper))
		  (throwfail "Illegal extra hypotheses in conclusion: "
			     ((set-difference hlower hupper) . linelist) ".")))
	  subforms)
    ;; Here we must push the restrictions on the set of hypotheses onto
    ;; the SUBFORMS
    (push `(if (and (eq hlower '$) (eq hupper '$))
	       t
	       (progn ,@(nreverse subforms)))
	  forms)
    (setq hyp-function-defn
	  `(defun ,rule-hyp-check-fn-name ,argnames
	     (declare (special rule-hupper rule-hlower)
		      (ignore ,@wffargnames))
	     (let ((hupper '$) (hlower '$))
	       ,@(nreverse forms)
	       (setq rule-hupper hupper)
	       (setq rule-hlower hlower)
	       t)))
    (putprop rule hyp-function-defn 'hyp-check-fn-defn)
    t))

;;; Next comes the function which build the functions which tests for
;;; validity of restrictions.  Note that it does not check whether
;;; APPLICABLE-P for the restriction is satisfied, but rather simply
;;; calls the function.

;;; NOTE:
;;; Temporary Restriction: All arguments to a restriction must be wffs!

(defun build-rule-wff-checks (rule)
  (let  ((propsymalist (get rule 'propsymalist))
	 (lineargnames (get rule 'lineargnames))
	 (wffargnames (get rule 'wffargnames))
	 (hypargnames (get rule 'hypargnames))
	 (argnames (get rule 'argnames))
	 (unique-linenames (get rule 'unique-linenames))
	 (unique-restrictions (get rule 'unique-restrictions))
	 (wff-check-fn-name (prepend rule '-legal-wffs))
	 function-defn)
    (let ((meta-var-unique-names (mapcar #'cdr propsymalist)))
      (setq function-defn
	    `(defun ,wff-check-fn-name ,argnames
	       (declare (special rule-hlower rule-hupper)
			(ignore ,@hypargnames))
	       (let ((wffbindings nil))
		 (declare (special wffbindings))
		 ;; The next MACRO-DO establishes the wff-bindings.
		 (macro-do ((quoted metavar ,meta-var-unique-names)
			    (unquoted wffarg ,wffargnames))
		   (push (cons metavar wffarg) wffbindings))
		 ;; Now check for consistency by evaluating all Meta-Labels
		 ;; which come from WFFOP's
		 (macro-do ((quoted metalabel
				    ,(set-of mv meta-var-unique-names
				       (eq (get mv 'flavor) 'meta))))
		   (when (not (same-match-p metalabel (wffeval metalabel)
					    (meta-label-eval metalabel)))
			 (mismatch% (wffeval metalabel)
				   (meta-label-eval metalabel))))
		 ;; After the wffbindings have been established check for
		 ;; consistency with existing lines.
		 (macro-do ((quoted unique-line ,unique-linenames)
			    (unquoted linearg ,lineargnames))
		   (when (existent-p linearg)
			 (match-bind (get unique-line 'meta-assertion)
				     (get (numalias linearg) 'assertion))))
		 ;; Now we substitute into the restrictions.
		 ;;  --- For now: all args are gwffs! ---
		 (macro-do ((quoted restr ,unique-restrictions))
		    (let ((rstr (get restr 'restr-call)))
		      (when
		       (not (apply (car rstr)
				   (mapcar #'meta-subst (cdr rstr))))
		       (throwfail "Rule not applicable.  " t
				  "Restrictions " (car rstr)
				  " not satisfied.  "))))))))
    (putprop rule function-defn 'wff-check-fn)
    t))
