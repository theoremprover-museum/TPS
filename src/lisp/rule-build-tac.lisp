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
;;; File: Rule-Build-Tac
;;; Package: Rules
;;;
;;; defines the function which build the definition of the function
;;; <rule>-match1.
;;;

(part-of rules)

(deffile rule-build-tac
  (part-of rules)
  (extension lsp)
  (mhelp "Defines the functions which build the definition of the function
<rule>-match"))



;*;;;; Fixed bug (see below) 7/21/87 DAN
;*;(defun build-rule-match1 (rule)
;*;  (let (;;(propsymalist (get rule 'propsymalist))
;*;	(lineargnames (get rule 'lineargnames))
;*;	;;(wffargnames (get rule 'wffargnames))
;*;	;;(hypargnames (get rule 'hypargnames))
;*;	;;(argnames (get rule 'argnames))
;*;	(support-transformation (get rule 'support-transformation))
;*;	(unique-linenames (get rule 'unique-linenames))
;*;	(unique-restrictions (get rule 'unique-restrictions))
;*;	(rule-match-fn-name (prepend rule '-match1))
;*;	;;(lines (get rule 'lines))
;*;	function-defn)
;*;    (let* (;;(meta-var-unique-names (mapcar #'cdr propsymalist))
;*;	   (default-exists (caar support-transformation))
;*;	   (line-assoc (mapcar #'cons lineargnames unique-linenames))
;*;	   (default-planned (if (symbolp (car default-exists))
;*;				(cdr (assoc (car default-exists) line-assoc))
;*;				nil))
;*;	   (default-support (if (symbolp (cadr default-exists))
;*;				(cdr (assoc (cadr default-exists) line-assoc))
;*;				nil)))
;*;      (setq function-defn
;*;	    `(defun ,rule-match-fn-name
;*;	       (,@(if default-planned `(pline) nil)
;*;		,@(if default-support `(sline) nil))
;*;	       (let ((wffbindings nil))
;*;		 (declare (special wffbindings))
;*;		 ;; For now we consider only the case of one pair on the lhs
;*;		 ;; of the support-transformation.
;*;		 (and 
;*;		  ,(if (not default-planned) `t
;*;		       `(%catch% (match-bind
;*;				  (get ',default-planned 'meta-assertion)
;*;				  (get pline 'assertion))
;*;				 (fail nil)))
;*;		  ,(if (not default-support) `t
;*;		       `(%catch% (match-bind
;*;				  (get ',default-support 'meta-assertion)
;*;				  (get sline 'assertion))
;*;				 (fail nil)))
;*;		  ,@(mapcar
;*;		     #'(lambda (restr)
;*;			 `(let ((rstr (get ',restr 'restr-call)))
;*;			     ;; The following is tricky:
;*;			     ;; If we cannot find out whether a
;*;			     ;; restriction is satisfied, it's OK.
;*;			     ;; Otherwise, throwfail.
;*;;*;			     (%catch% (apply (car rstr)
;*;;*;					     (mapcar #'meta-subst (cdr rstr))))
;*;;*;			     (fail t)))
;*;			     ;; A misplaced paren caused the above to be wrong
;*;			     ;; corrected 7/21/87 DAN
;*;			     (%catch% (apply (car rstr)
;*;					     (mapcar #'meta-subst (cdr rstr)))
;*;				      (fail t))))
;*;		     unique-restrictions)))))
;*;      (putprop rule function-defn 'match1-fn-defn)
;*;      t)))

;;; Fixed bug (see below) 7/21/87 DAN
;;; Fixed another bug (see below) 7/22/87 DAN
(defun build-rule-match1 (rule)
  (let (;;(propsymalist (get rule 'propsymalist))
	(lineargnames (get rule 'lineargnames))
	;;(wffargnames (get rule 'wffargnames))
	;;(hypargnames (get rule 'hypargnames))
	;;(argnames (get rule 'argnames))
	(support-transformation (get rule 'support-transformation))
	(unique-linenames (get rule 'unique-linenames))
	(unique-restrictions (get rule 'unique-restrictions))
	(rule-match-fn-name (prepend rule '-match1))
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
	    `(defun ,rule-match-fn-name
	       (,@(if default-planned `(pline) nil)
		,@(if default-support `(sline) nil))
	       (let ((wffbindings nil))
		 (declare (special wffbindings))
		 ;; For now we consider only the case of one pair on the lhs
		 ;; of the support-transformation.
		 (and 
		  ;;; It is possible for match-bind to return nil even though
		  ;;; it hasn't failed.  In order to determine just when it
		  ;;; has really failed, change below was made to the value
		  ;;; returned by the %catch% in case of a throwfail.
		  ;;; 7/22/87 DAN
		  ,(if (not default-planned) `t
		       `(not (eq 'failed
				 (%catch% (match-bind
					   (get ',default-planned 'meta-assertion)
					   (get pline 'assertion))
					  (fail 'failed)))))
		  ,(if (not default-support) `t
		       `(not (eq 'failed
				 (%catch% (match-bind
					   (get ',default-support 'meta-assertion)
					   (get sline 'assertion))
					  (fail 'failed)))))
		  ,@(mapcar
		     #'(lambda (restr)
			 `(let ((rstr (get ',restr 'restr-call)))
			     ;; The following is tricky:
			     ;; If we cannot find out whether a
			     ;; restriction is satisfied, it's OK.
			     ;; Otherwise, throwfail.
;*;			     (%catch% (apply (car rstr)
;*;					     (mapcar #'meta-subst (cdr rstr))))
;*;			     (fail t)))
			     ;; A misplaced paren caused the above to be wrong
			     ;; corrected 7/21/87 DAN
			     (%catch% (apply (car rstr)
					     (mapcar #'meta-subst (cdr rstr)))
				      (fail t))))
		     unique-restrictions)))))
      (putprop rule function-defn 'match1-fn-defn)
      t)))




(defun build-rule-short (rule)
  (let (;(lineargnames (get rule 'lineargnames))
	(argnames (get rule 'argnames))
	(rule-short-fn-name (prepend rule '-short))
	(ex-lineargnames (get rule 'ex-lineargnames))
	(new-wff-arglist (get rule 'new-wff-arglist))
	function-defn)
    (let* (;(line-arg-forms (mapcar #'(lambda (line) `(linealias ,line))
	   ;ex-lineargnames))
	   ;(wff-arg-forms (mapcar #'(lambda (wff) `'',wff) new-wff-arglist))
	   (short-argnames (append ex-lineargnames new-wff-arglist))
	   ;;long-argnames
	   )
      (setq function-defn
	    (append (list 'defun) (list rule-short-fn-name) (list short-argnames)
		    (list (list 'funcall '#'comdecode 
				(append (list 'append) (list (list 'list (list 'quote rule))) 
					(mapcar #'(lambda (arg)
						    (cond ((member arg ex-lineargnames)
							   (list 'list (append (list 'linealias) (list arg))))
							  ((member arg new-wff-arglist) 
							   (list 'append (list 'list (list 'append (list 'list ''quote) (list 'list arg) ''nil))))
							  (t (list 'list (quote '$)))))
						argnames) 'nil)))))
      (putprop rule function-defn 'short-fn-defn)
      t)))

; function-defn used to be setq'd to the following:
;
;	    `(defun ,rule-short-fn-name ,short-argnames
;	       (funcall
;		#'comdecode
;		`(,',rule
;		  ,,@(mapcar
;		     #'(lambda (arg)
;			 (cond ((member arg ex-lineargnames)
;				`(linealias ,arg))
;			       ((member arg new-wff-arglist) ``',,arg)
;			       (t `'$)))
;			    argnames)))))
;
; producing output like this (if you were lucky):
;   (defun egen-short (p2 |t|)
;        (funcall #'comdecode `(egen ,(linealias p2) $ ',|t| $ $ $ $ $)))
;
; the problem is that the above backquoted form only has to be evaluated to 
; something EQUAL to the definition in CLtL2, so Allegro 4.1 was producing 
; things like:
;   (defun egen-short (p2 |t|)
;        (funcall #'comdecode (excl::bq-list 'egen (linealias p2) '$ maint::arg '$...)))
; ...and this wasn't working.




