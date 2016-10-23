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
;;; File: Rule-Build-Match
;;; Package: Rules
;;;
;;; defines the function which build the definition of the function
;;; <rule>-match.
;;;

(part-of rules)

(deffile rule-build-match
  (part-of rules)
  (extension lsp)
  (mhelp "Defines the functions which build the definition of the function
<rule>-match"))

(defun build-rule-match (rule)
  (let (;;(propsymalist (get rule 'propsymalist))
	(lineargnames (get rule 'lineargnames))
	;;(wffargnames (get rule 'wffargnames))
	;;(hypargnames (get rule 'hypargnames))
	;;(argnames (get rule 'argnames))
	(support-transformation (get rule 'support-transformation))
	(unique-linenames (get rule 'unique-linenames))
	(unique-restrictions (get rule 'unique-restrictions))
	(rule-match-fn-name (prepend rule '-match))
	;;(lines (get rule 'lines))
	function-defn)
    (let* (;;(meta-var-unique-names (mapcar #'cdr propsymalist))
	   (default-exists (caar support-transformation))
	   (line-assoc (mapcar #'cons lineargnames unique-linenames))
	   (default-planned (if (symbolp (car default-exists))
				(cdr (assoc (car default-exists) line-assoc))
				nil))
	   (default-support (if (symbolp (cadr default-exists))
				(cdr (assoc (cadr default-exists) line-assoc))
				nil)))
      (setq function-defn
	    `(defun ,rule-match-fn-name (plan-support)
	       (let ((wffbindings nil) matched-plan matched-support)
		 (declare (special wffbindings))
		 ;; For now we consider only the case of one pair on the lhs
		 ;; of the support-transformation.
		 (setq
		  matched-plan
		  ,(if (not default-planned) nil
		      `(progn
			(%catch% (match-bind
				(get ',default-planned 'meta-assertion)
				(get (car plan-support) 'assertion))
			       (fail (throwfail
				      "Planned lines did not match.")))
			(list (car plan-support)))))
		 ;; and another restriction: at most one support line
		 ;; on the lhs of the support-transformation.
		 (setq
		  matched-support
		 ,(if (not default-support)
		      `(macro-do ((quoted restr ,unique-restrictions))
			 (let ((rstr (get restr 'restr-call)))
			   (when
			    ;; The following is tricky:
			    ;; If we cannot find out whether a
			    ;; restriction is satisfied, it's OK.
			    ;; Otherwise, throwfail.
			    (%catch% (not (apply (car rstr)
					       (mapcar #'meta-subst (cdr rstr))))
				   (fail nil))
			    (throwfail "Some restriction not satisfied."))))
		      `(do ((supps (cdr plan-support) (cdr supps))
			    (legal-supports nil))
			   ((null supps)
			    (if (null legal-supports)
				(throwfail "No support line matched.")
				(nreverse legal-supports)))
			 ;; The following is also tricky: always restore
			 ;; wffbindings when looking for next support match.
			 (let ((wffbindings wffbindings))
			   (declare (special wffbindings))
			   (%catch% (progn
				   (match-bind
				    (get ',default-support
					 'meta-assertion)
				    (get (car supps) 'assertion))
				   (macro-do ((quoted restr ,unique-restrictions))
				     (let ((rstr (get restr 'restr-call)))
				       (when
					;; The following is tricky:
					;; If we cannot find out whether a
					;; restriction is satisfied, it's OK.
					;; Otherwise, throwfail.
					(%catch% (not (apply (car rstr)
							   (mapcar #'meta-subst (cdr rstr))))
					       (fail nil))
					(throwfail nil))))
				   (push (car supps) legal-supports))
				  (fail nil))))))
		 ;; Now we return the first n arguments of the function
		 ;; as filled in above, but using linelabels instead of numbers
		 (list matched-plan matched-support))))
      (putprop rule function-defn 'match-fn-defn)
      t)))
