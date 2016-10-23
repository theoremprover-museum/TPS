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
;;; File: Rule-BB
;;;
;;; defines functions which builds the function which actually constructs
;;; the lines for the outline before they are inserted.
;;;

(part-of rules)

(deffile rule-bb
  (part-of rules)
  (extension lsp)
  (mhelp "Defines functions which build the function which actually construct
the lines for the outline before they are inserted."))

(defun build-rule-build (rule)
  (let ((propsymalist (get rule 'propsymalist))
	(lineargnames (get rule 'lineargnames))
	(u-lineargnames (get rule 'unique-linenames))
	(mlines (get rule 'm-line-args))
	(hlines (get rule 'hlines))
	(wffargnames (get rule 'wffargnames))
	(hypargnames (get rule 'hypargnames))
	(argnames (get rule 'argnames))
	(rule-build-fn-name (prepend rule '-build))
	function-defn)
    (let ((meta-var-unique-names (mapcar #'cdr propsymalist))
	  (justifications (mapcar #'(lambda (mline)
				      (if (and (not treat-hlines-as-dlines)
					       (member (car mline) hlines))
					  '(hline-justification () ())
					  (nth 4 mline)))
				  mlines)))
      (setq justifications
	    (mapcar #'(lambda (just)
			(if (null just) `(nextplan)
			    `(list ,(car just)
				   (mapcar #'meta-subst ',(cadr just))
				   (subst-labels num-alist
						 (list ,@(caddr just))))))
		    justifications))
      (setq function-defn
	    `(defun ,rule-build-fn-name ,argnames
	       (let ((new-line-labels (line-label-vec ,(length lineargnames)))
		     (wffbindings nil)
		     (num-alist nil))
		 (declare (special wffbindings))
		 ;; The next MACRO-DO establishes the wff-bindings.
		 (macro-do ((quoted metavar ,meta-var-unique-names)
			    (unquoted wffarg ,wffargnames))
		   (push (cons metavar wffarg) wffbindings))
		 ;; Now we must create some line-labels and set their
		 ;; properties
		 (macro-do ((quoted u-line ,u-lineargnames)
			    (local line-label new-line-labels))
		   (putprop line-label
			    (meta-subst (get u-line 'meta-assertion))
			    'assertion))
		 ;; Next we put the line number property on the lines
		 ;; Note that for now we create all lines, even existent ones.
		 (macro-do ((unquoted line-arg ,lineargnames)
			    (local line-label new-line-labels))
		   (putprop line-label line-arg 'linenumber)
		   (push (cons line-arg line-label) num-alist))
		 ;; The justification properties must be taken
		 ;; care of here.
		 (macro-do ((unquoted just ,justifications)
			    (local line-label new-line-labels))
		    (putprop line-label just 'justification))
		 ;; Now we replace line numbers by labels in the hypotheses.
		 (macro-do ((unquoted hyp-arg ,hypargnames))
		   (setq hyp-arg (subst-labels num-alist hyp-arg)))
		 ;; And now the hypothesis properties.
		 (macro-do ((unquoted hyp-arg ,hypargnames)
			    (local line-label new-line-labels))
		   (putprop line-label hyp-arg 'hypotheses))
		 ;; Now we return a list, the line labels replacing
		 ;; the line numbers.
		 (append new-line-labels (list ,@wffargnames ,@hypargnames)))))
      (putprop rule function-defn 'rule-build-fn))))

;;; The next function actually enters the lines into the proof outline -
;;; using the function update-plan

#+comment(defun build-rule-enter (rule)
  (let ((argnames (get rule 'argnames))
	(wffargnames (get rule 'wffargnames))
	(hypargnames (get rule 'hypargnames))
	(rule-enter-fn-name (prepend rule '-enter))
	function-defn)
    (setq function-defn
	  `(defun ,rule-enter-fn-name ,argnames
	     (declare (ignore ,@(append wffargnames hypargnames)))
	     (update-plan
	      (eval-destruct ,(car (get rule 'support-transformation)))
	      (eval-destruct ,(cadr (get rule 'support-transformation))))))
    (putprop rule function-defn 'rule-enter-fn)))

(defun build-rule-enter (rule)
  (let ((argnames (get rule 'argnames))
	(rule-enter-fn-name (prepend rule '-enter))
	(legal-fn (cons (prepend rule '-legal) (get rule 'argnames)))
	function-defn)
    (setq function-defn
	  `(defun ,rule-enter-fn-name ,argnames
	     (declare (special core::check-hyps-again))
	     (when core::check-hyps-again ,legal-fn)
	     (update-plan
	      (eval-destruct ,(car (get rule 'support-transformation)))
	      (eval-destruct ,(cadr (get rule 'support-transformation))))))
    (putprop rule function-defn 'rule-enter-fn)))
