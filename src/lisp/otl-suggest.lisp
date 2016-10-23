;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLSUGGEST)

;;;
;;; File: Otl-Suggest
;;;
;;; defines categories etc. to allow automatic suggestion of inference
;;; rules without the benefit of an expansion tree.
;;;

(part-of otlsuggest)

(deffile otl-suggest
  (part-of otlsuggest)
  (extension lsp)
  (mhelp "Defines categories etc. to allow automatic suggestion of inference
rules without the benefit of an expansion tree."))

(context suggestions)

(defun show-suggestions (suggestions)
  (do ((suggs suggestions (cdr suggs))
       (curr-rule) (curr-plan))
      ((null suggs))
    (setq curr-rule (caar suggs))
    (setq curr-plan (caadar suggs))
    (if (null (caddar suggs))
	(msgf (t 2) curr-rule " " (curr-plan . existing-line))
	(do ((lines (caddar suggs) (cdr lines)))
	    ((null lines))
	  (msgf (t 2) curr-rule " ")
	  (when curr-plan (msg (curr-plan . existing-line) " "))
	  (msg ((car lines) . existing-line))))))

;;; (FIND-SUGGESTS plan-support srulelist)
;;; will return a list ((rule planlist supportlist) ...) where rule is
;;; the name of the suggested rule, planlist is either NIL (no planned
;;; line in the arguments of rule) or (linelab), where linelab is the
;;; label of the planned line, and supportlst is either NIL (no
;;; support line in the arguments of rule) or (linelab1 ... linelabn),
;;; each linelab being a possible choice for the supporting line in the
;;; rule definition.  FIND-SUGGEST does not work for rules with more than
;;; one planned line or support line on the lhs of the support-transformation.

(defun find-suggests (plan-support srulelist)
  (do ((srules srulelist (cdr srules))
       (suggestions nil))
      ((null srules)
       (setq suggestions (parse-suggestions (reverse suggestions)))
       (setq suggestions (remove-if #'not-really-allowed suggestions))
       (nreverse suggestions))
    ;; Since we don't want to do the work of figuring out HLOWER and HUPPER
    ;; for the few rules where they are actually needed,  we bind them
    ;; to NIL.  Then every rule which checks a condition on them will
    ;; be accepted as T.  Eventually, we should carry HUPPER and HLOWER
    ;; along with the plan-support structure.
    (let ((rule-hupper nil) (rule-hlower nil))
      (declare (special rule-hupper rule-hlower))
      (when (symbolp (car srules))
	    (%catch% (push
		    (cons (car srules)
			  (funcall (get (car srules) 'matchfn) plan-support))
		    suggestions)
		   (fail nil))))))

(defun parse-suggestions (suggestions)
  ;;as this doesn't work for anything with >1 support line, we split anything with multiple supports
  (let ((retlist))
    (dolist (s suggestions retlist)
      (if (> (length (caddr s)) 1)
	  (dolist (elt (caddr s))
	    (push (list (car s) (cadr s) (list elt)) retlist))
	(push s retlist)))))

(defun not-really-allowed (suggestion)
  ;suggestion will be of the form RULE PLANLIST SUPPORTLIST
  ;we try to get rid of ill-typed suggestions for EXT=0 or EXT=.
  ;this is somewhat ad hoc.
  ;this function should also checks for applicability of LAMBDA, EQUIV-EQ-CONTR, EQUIV-EQ-EXPD
  ;and anything else where the MATCH function may not be strong enough.
  (or (and (string= (car suggestion) "EXT=")
	   ;so it *is* an equality
	   (caadr suggestion) 
	   (not (consp (type (cdar (get (caadr suggestion) 'assertion))))))
      (and (string= (car suggestion) "EXT=0")
	   ;so it *is* an equality
	   (caadr suggestion) 
	   (not (equal 'o (type (cdar (get (caadr suggestion) 'assertion))))))
      (and (string= (car suggestion) "EQUIV-EQ-CONTR")
	   ;may not be an equality at all
	   (caadr suggestion) 
	   (not (contains-= (get (caadr suggestion) 'assertion))))
      (and (string= (car suggestion) "EQUIV-EQ-EXPD")
	   ;may not be an equality at all
	   (caaddr suggestion) ;this will be a *support* line
	   (not (contains-= (get (caaddr suggestion) 'assertion))))
      (and (string= (car suggestion) "LCONTR*")
	   (caaddr suggestion)
	   (wffeq-ab (lambda-norm (get (caaddr suggestion) 'assertion)) (get (caaddr suggestion) 'assertion)))
      (and (string= (car suggestion) "LEXPD*")
	   (caadr suggestion)
	   (wffeq-ab (lambda-norm (get (caadr suggestion) 'assertion))  (get (caadr suggestion) 'assertion)))
      (and (string= (car suggestion) "IDEF")
	   (caadr suggestion)
	   (module-loaded-p 'expansion-tree)
	   (not (auto::contains-defn-not-equiv (get (caadr suggestion) 'assertion))))
      (and (string= (car suggestion) "EDEF")
	   (caaddr suggestion) ;this will be a *support* line
	   (module-loaded-p 'expansion-tree)
	   (not (auto::contains-defn-not-equiv (get (caaddr suggestion) 'assertion))))
))
      
(defun rule-compfn (sr1 sr2)
  (let ((srp1 (get sr1 'priority))
	(srp2 (get sr2 'priority)))
    (cond ((null srp1) nil)
	  ((or (null srp2) (< srp1 srp2))))))

(defun sort-rules (srulelist)
  (do ((srules (sort (set-of srule srulelist
		       (symbolp srule))
		     #'rule-compfn)
	       (cdr srules))
       (srule-alist nil))
      ((null srules)
       (nreverse (mapcar #'(lambda (al) (cons (get (car al) 'priority)
					      (nreverse al)))
			 srule-alist)))
    (if (or (null srule-alist)
	    (not (equal (get (caar srule-alist) 'priority)
			(get (car srules) 'priority))))
	(push (list (car srules)) srule-alist)
	(push (car srules) (car srule-alist)))))
