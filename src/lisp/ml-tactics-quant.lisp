;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of ml-tactics)

(context quant-tactics)

(deffile ml-tactics-quant
  (extension lisp)
  (part-of ml-tactics)
  (mhelp "Defines tactics for quantifier rules."))

(deftactic ugen-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'ugen-match1 (list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply UGEN to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied UGEN."))
	     (if (ugen-short pline)
		 (progn
		  (tactic-output msg t)
		  (values (set-difference (mapcar #'car (proof-plans dproof))
					  oldplans)
			  msg 'succeed))
		 (progn 
		  (tactic-output "UGEN failed." nil)
		  (values (list pline) "UGEN failed." 'fail))))
	   (let ((msg (if matched-plans
			  "Not applying UGEN."
			  "Can't apply UGEN.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))
   "Applies UGEN if planned line is universally quantified."))


(defun get-wff (wfftyp prompt default)
  (do ((term (prompt-read-return-vals (msgf prompt) wfftyp
			  default ((? (mhelp 'gwff))))
	     (prompt-read-return-vals (msgf prompt) wfftyp
			  default ((? (mhelp 'gwff))))))
      ((type-equal term default) term)
    (msgf (term . gwff) " is not of the correct type." t)
    ))

(deftactic ui-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not #'ui-match1 supports))
	    (matched-support (car matched-supports))
	    (term (if matched-support
		      (bindvar (line-assertion matched-support))))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil "Apply UI to line ~D?"
					  (linealias x)) t))
			    matched-supports))
	     (if matched-support
		 (setq term (get-wff 'gwff "Substitution term" term))))
       (if matched-support
	   (let ((msg "Applied UI."))
	     (tactic-output msg t)
	     (ui-short matched-support term)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-supports
			  "Not applying UI."
			  "Can't apply UI.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "If a support line is universally quantified, will instantiate it.  In
interactive mode will ask for a term, otherwise will use the bound
variable itself."))

(deftactic rulec-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	    (matched-supports (remove-if-not
			       #'(lambda (x) (rulec-match1 pline x))
			       supports))
	    (matched-support (car matched-supports))
	    (instvar (if matched-support
			 (let ((x (bindvar (line-assertion matched-support))) ; cebrown - 5/25/00 - we decided the default should be the bound var, if it is legal
			       (rule-hupper (line-hypotheses matched-support)))
			   (if (and (not (member x (free-in-hyps)))
				    (not (free-in x (line-assertion pline))))
			       x
			     (funcall ren-var-fn x)))))
;	    (instvar (if matched-support   ; the old version always renamed the var - cebrown - 5/25/00
;		      (funcall ren-var-fn
;			       (bindvar (line-assertion matched-support)))))
	    (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
       (when (eq tacmode 'interactive)
	     (setq matched-support
		   (find-if #'(lambda (x)
				(query (format nil  "Instantiate line ~D to prove line ~D by RULEC?"
					  (linealias x) (linealias pline)) t))
			    matched-supports))
	     (if matched-support
		 (setq instvar (get-wff 'gvar "Instantiation variable" instvar))))
       (if matched-support
	   (let ((msg "Applied RULEC."))
	     (if (rulec-short pline matched-support instvar)
		 (progn 
		  (tactic-output msg t)
		  (values (set-difference (mapcar #'car (proof-plans dproof))
					 oldplans)
			 msg 'succeed))
		 (progn 
		  (tactic-output "RULEC failed." nil) 
		  (values (list pline) "RULEC failed." 'fail))))
	   (let ((msg (if matched-supports
			  "Not applying RULEC."
			  "Can't apply RULEC.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail)))
))
   "If a support line is existentially quantified, will apply RULEC
with a brand new variable."))

(deftactic egen-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'egen-match1 (list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply EGEN to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	   (term (if matched-plan 
		     (if (eq tacmode 'interactive)
			 (get-wff 'gwff "Term to generalize upon?" 
				   (bindvar (line-assertion matched-plan)))
			 (bindvar (line-assertion matched-plan)))))
	   (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied EGEN."))
	     (tactic-output msg t)
	     (egen-short pline term)
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	   (let ((msg (if matched-plans
			  "Not applying EGEN."
			  "Can't apply EGEN.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))
   "If the planned line is existentially quantified, will apply EGEN,
prompting for the term if in interactive mode."))

(deftactic abu-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'ml::abu-match1 (list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply ABU to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	   (newvar (if matched-plan 
		     (if (eq tacmode 'interactive)
			 (get-wff 'gvar "New variable?" 
				   (funcall ren-var-fn
					    (bindvar (line-assertion matched-plan))))
			 (funcall ren-var-fn (bindvar (line-assertion matched-plan))))))
	   (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied ABU."))
	     (if (ml::abu-short pline newvar)
		 (progn 
		  (tactic-output msg t)
		  (values (set-difference (mapcar #'car (proof-plans dproof))
					  oldplans)
			  msg 'succeed))
		 (progn
		  (tactic-output "ABU failed" nil)
		  (values (list pline) "ABU failed." 'fail))))
	   (let ((msg (if matched-plans
			  "Not applying ABU."
			  "Can't apply ABU.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))
   "If planned line is universally quantified, will apply ABU, prompting
for a variable if in interactive mode."))


(deftactic quantificational
  (nat-ded
   (orelse ugen-tac (then abu-tac ugen-tac) egen-tac ui-tac)))



(deftactic idef-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'idef-match1 (list pline)))
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply IDEF to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied IDEF."))
	     (if (idef-short pline)
		 (progn
		  (tactic-output msg t)
		  (values (set-difference (mapcar #'car (proof-plans dproof))
					  oldplans)
			  msg 'succeed))
		 (progn 
		  (tactic-output "IDEF failed." nil)
		  (values (list pline) "IDEF failed." 'fail))))
	   (let ((msg (if matched-plans
			  "Not applying IDEF."
			  "Can't apply IDEF.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
)))
   "Applies IDEF if planned line contains a definition."))

(deftactic edef-tac
  (nat-ded
    (lambda (pline)
      (let* ((supports (cdr (assoc pline (proof-plans dproof)))) 
	     (matched-supports (remove-if-not #'edef-match1 
					      supports))
	     (matched-support (car matched-supports))
	     (oldplans (remove pline (mapcar #'car (proof-plans dproof)))))
	(when (eq tacmode 'interactive)
	  (setq matched-support
		(find-if #'(lambda (x)
			     (query (format nil  "Apply EDEF to line ~D?"
					    (linealias x)) t))
			 matched-supports)))
	  (if matched-support
	      (let ((msg "Applied EDEF."))
		(if (edef-short matched-support)
		    (progn 
		      (tactic-output msg t)
		      (values (set-difference (mapcar #'car (proof-plans dproof))
					      oldplans)
			      msg 'succeed))
		    (progn 
		      (tactic-output "EDEF failed." nil) 
		      (values (list pline) "EDEF failed." 'fail))))
	      (let ((msg (if matched-supports
			     "Not applying EDEF."
			     "Can't apply EDEF.")))
		(tactic-output msg nil)
		(values (list pline) msg 'fail)))
	  ))
      "Applies EDEF if a support line contains a definition."
      ))

(context equality-tactics)

(deftactic subst=l-tac
  (nat-ded
   (lambda (pline)
     (let ((supports (cdr (assoc pline (proof-plans dproof))))
	   (pline-assertion (line-assertion pline)))
       (multiple-value-bind (support-equality other-support)
	   (do ((supps (cdr supports) (cdr supps))
		(supp (car supports) (car supps)))
	       ((null supp))
	     (let ((supp-assertion (line-assertion supp))
		   (other-supp nil))
	       (when (equals-p supp-assertion)
		 (setq other-supp
		   (find-if #'(lambda (x) 
				(r-prime-restr (glr supp-assertion)
					       (line-assertion x)
					       (grr supp-assertion)
					       pline-assertion))
			    (remove supp supports))))
	       (when other-supp (return (values supp other-supp)))))
	 (if support-equality
	     (let ((msg "Applied SUBST=L."))
	       (tactic-output msg t)
	       (comdecode (list 'subst=l (linealias support-equality)
				(linealias pline)
				(linealias other-support) 
				'$
				'$ '$ '$ '$ '$ '$))
	       (values nil
		       msg
		       'succeed))
	   (let ((msg "Can't apply SUBST=L."))
	     (tactic-output msg nil)
	     (values (list pline)
		     msg
		     'fail))
	   ))))
   "Applies SUBST=L if planned line follows by this rule from a support line."
))

(deftactic subst=r-tac
  (nat-ded
   (lambda (pline)
     (let ((supports (cdr (assoc pline (proof-plans dproof))))
	   (pline-assertion (line-assertion pline)))
       (multiple-value-bind (support-equality other-support)
	   (do ((supps (cdr supports) (cdr supps))
		(supp (car supports) (car supps)))
	       ((null supp))
	     (let ((supp-assertion (line-assertion supp))
		   (other-supp nil))
	       (when (equals-p supp-assertion)
		 (setq other-supp
		   (find-if #'(lambda (x) 
				(r-prime-restr (grr supp-assertion)
					       (line-assertion x)
					       (glr supp-assertion)
					       pline-assertion))
			    (remove supp supports))))
	       (when other-supp (return (values supp other-supp)))))
	 (if support-equality
	     (let ((msg "Applied SUBST=R."))
	       (tactic-output msg t)
	       (comdecode (list 'subst=r (linealias support-equality)
				(linealias pline)
				(linealias other-support) 
				'$
				'$ '$ '$ '$ '$ '$))
	       (values nil
		       msg
		       'succeed))
	   (let ((msg "Can't apply SUBST=R."))
	     (tactic-output msg nil)
	     (values (list pline)
		     msg
		     'fail))
	   ))))
   "Applies SUBST=R if planned line follows by this rule from a support line."
))

(deftactic refl=-tac
  (nat-ded
   (lambda (pline)
     (let* ((assertion (line-assertion pline))
	    (matched-pline
	     (if (and (equals-p assertion)
		      (wffeq (glr assertion) (grr assertion)))
		 pline)))
       (if matched-pline
	   (let ((msg "Applied REFL=."))
	     (tactic-output msg t)
	     (comdecode (list 'assert 'refl= (linealias pline)))
	     (values nil msg 'succeed))
	 (let ((msg "Can't apply REFL=."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))
   "Applies rule for reflexivity of equality if planned line is of
form a=a."))

(deftactic sym=-tac
  (nat-ded
   (lambda (pline)
     (let* ((assertion (line-assertion pline))
	    (matched-support
	     (when (equals-p assertion)
	       (let ((left (glr assertion))
		     (right (grr assertion)))
	       (find-if #'(lambda (x)
			    (let ((assert (line-assertion x)))
			      (and (equals-p assert)
				   (wffeq left (grr assert))
				   (wffeq right (glr assert)))))
			(cdr (assoc pline (proof-plans dproof))))))))
       (if matched-support
	   (let ((msg "Applied SYM=."))
	     (tactic-output msg t)
	     (comdecode (list 'sym= (linealias pline)
			      (linealias matched-support)
			      '$ '$ '$ '$))
	     (values nil msg 'succeed))
	 (let ((msg "Can't apply SYM=."))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))
   "Applies symmetry of equality if planned line follows by that rule from
some support line."))
		      

(context lambda-tactics)

(defun lexpd*-matches (line)
  (not (equal (get line 'assertion) (lnorm (get line 'assertion)))))

(defun lcontr*-matches (line)
  (not (equal (get line 'assertion) (lnorm (get line 'assertion)))))

(defun lexpd*-beta-matches (line)
  (not (equal (get line 'assertion) (lnorm-beta (get line 'assertion)))))

(defun lcontr*-beta-matches (line)
  (not (equal (get line 'assertion) (lnorm-beta (get line 'assertion)))))

(defun lexpd*-eta-matches (line)
  (not (equal (get line 'assertion) (lnorm-eta (get line 'assertion)))))

(defun lcontr*-eta-matches (line)
  (not (equal (get line 'assertion) (lnorm-eta (get line 'assertion)))))

(deftactic lexpd*-vary-tac
  (nat-ded
   (orelse
   (ifthen beta-eta-together-tac lexpd*-tac)
   (ifthen beta-only-tac lexpd*-beta-tac)
   (ifthen beta-eta-separate-tac (orelse lexpd*-beta-tac lexpd*-eta-tac)))
   "Decides which sort of lambda expansion to do, based
on the setting of LAMBDA-CONV.")
  (etree-nat
   (orelse
   (ifthen beta-eta-together-tac lexpd*-tac)
   (ifthen beta-only-tac lexpd*-beta-tac)
   (ifthen beta-eta-separate-tac (orelse lexpd*-beta-tac lexpd*-eta-tac)))
   "Decides which sort of lambda expansion to do, based
on the setting of LAMBDA-CONV."))


(deftactic lcontr*-vary-tac
  (nat-ded
   (orelse
   (ifthen beta-eta-together-tac lcontr*-tac)
   (ifthen beta-only-tac lcontr*-beta-tac)
   (ifthen beta-eta-separate-tac (orelse lcontr*-beta-tac lcontr*-eta-tac)))
   "Decides which sort of lambda contraction to do, based
on the setting of LAMBDA-CONV.")
  (etree-nat
   (orelse
   (ifthen beta-eta-together-tac lcontr*-tac)
   (ifthen beta-only-tac lcontr*-beta-tac)
   (ifthen beta-eta-separate-tac (orelse lcontr*-beta-tac lcontr*-eta-tac)))
   "Decides which sort of lambda contraction to do, based
on the setting of LAMBDA-CONV."))


(deftactic beta-eta-together-tac
  (etree-nat
   (lambda (pline)
     (if (eq lambda-conv 'BETA-ETA-TOGETHER) 
	 (values (list pline) "Beta and Eta together" 'succeed)
       (values (list pline) "Not Beta and Eta together" 'fail)))
   "Returns success if LAMBDA-CONV is BETA-ETA-TOGETHER.")
  (nat-ded
   (lambda (pline)
     (if (eq lambda-conv 'BETA-ETA-TOGETHER) 
	 (values (list pline) "Beta and Eta together" 'succeed)
       (values (list pline) "Not Beta and Eta together" 'fail)))
   "Returns success if LAMBDA-CONV is BETA-ETA-TOGETHER."))

(deftactic beta-eta-separate-tac
  (etree-nat
   (lambda (pline)
     (if (eq lambda-conv 'BETA-ETA-SEPARATE) 
	 (values (list pline) "Beta and Eta separately" 'succeed)
       (values (list pline) "Not Beta and Eta separately" 'fail)))
   "Returns success if LAMBDA-CONV is BETA-ETA-SEPARATE.")
  (nat-ded
   (lambda (pline)
     (if (eq lambda-conv 'BETA-ETA-SEPARATE) 
	 (values (list pline) "Beta and Eta separately" 'succeed)
       (values (list pline) "Not Beta and Eta separately" 'fail)))
   "Returns success if LAMBDA-CONV is BETA-ETA-SEPARATE."))

(deftactic beta-only-tac
  (etree-nat
   (lambda (pline)
     (if (eq lambda-conv 'BETA-ONLY) 
	 (values (list pline) "Beta only" 'succeed)
       (values (list pline) "Not Beta only" 'fail)))
   "Returns success if LAMBDA-CONV is BETA-ONLY.")
  (nat-ded
   (lambda (pline)
     (if (eq lambda-conv 'BETA-ONLY) 
	 (values (list pline) "Beta only" 'succeed)
       (values (list pline) "Not Beta only" 'fail)))
   "Returns success if LAMBDA-CONV is BETA-ONLY."))


(deftactic lexpd*-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'lexpd*-matches (list pline))) ;anything will do
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply LEXPD* to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied LEXPD*."))
	     (tactic-output msg t)
	     (comdecode (list 'lexpd* (linealias pline) '$ '$ '$ '$ '$)) 
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	 (let ((msg "Can't apply LEXPD*."))
		  (tactic-output msg nil)
		  (values (list pline) msg 'fail)))))
   "Applies LEXPD*, if that will change the planned line."))

(deftactic lcontr*-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	    (matched-supports (remove-if-not #'lcontr*-matches supports))
	   (matched-support
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply LCONTR* to line ~D?"
						 (linealias x)) t))
			 matched-supports)
		(car matched-supports)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-support
	   (let ((msg "Applied LCONTR*."))
	     (tactic-output msg t)
	     (comdecode (list 'lcontr* (linealias matched-support) '$ '$ '$ '$ '$)) 
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	 (let ((msg "Can't apply LCONTR*."))
		  (tactic-output msg nil)
		  (values (list pline) msg 'fail)))))
   "Applies LCONTR*, if that will change the support line."))

(deftactic lexpd*-beta-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'lexpd*-beta-matches (list pline))) ;anything will do
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply LEXPD*-BETA to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied LEXPD*-BETA."))
	     (tactic-output msg t)
	     (comdecode (list 'lexpd*-beta (linealias pline) '$ '$ '$ '$ '$)) 
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	 (let ((msg "Can't apply LEXPD*-BETA."))
		  (tactic-output msg nil)
		  (values (list pline) msg 'fail)))))
   "Applies LEXPD*-BETA, if that will change the planned line."))

(deftactic lcontr*-beta-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	    (matched-supports (remove-if-not #'lcontr*-beta-matches supports))
	   (matched-support
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply LCONTR*-BETA to line ~D?"
						 (linealias x)) t))
			 matched-supports)
		(car matched-supports)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-support
	   (let ((msg "Applied LCONTR*-BETA."))
	     (tactic-output msg t)
	     (comdecode (list 'lcontr*-beta (linealias matched-support) '$ '$ '$ '$ '$)) 
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	 (let ((msg "Can't apply LCONTR*-BETA."))
		  (tactic-output msg nil)
		  (values (list pline) msg 'fail)))))
   "Applies LCONTR*-BETA, if that will change the support line."))

(deftactic lexpd*-eta-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'lexpd*-eta-matches (list pline))) ;anything will do
	   (matched-plan
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply LEXPD*-ETA to line ~D?"
						 (linealias x)) t))
			 matched-plans)
		(car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied LEXPD*-ETA."))
	     (tactic-output msg t)
	     (comdecode (list 'lexpd*-eta (linealias pline) '$ '$ '$ '$ '$)) 
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	 (let ((msg "Can't apply LEXPD*-ETA."))
		  (tactic-output msg nil)
		  (values (list pline) msg 'fail)))))
   "Applies LEXPD*-ETA, if that will change the planned line."))

(deftactic lcontr*-eta-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	    (matched-supports (remove-if-not #'lcontr*-eta-matches supports))
	   (matched-support
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply LCONTR*-ETA to line ~D?"
						 (linealias x)) t))
			 matched-supports)
		(car matched-supports)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-support
	   (let ((msg "Applied LCONTR*-ETA."))
	     (tactic-output msg t)
	     (comdecode (list 'lcontr*-eta (linealias matched-support) '$ '$ '$ '$ '$)) 
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	 (let ((msg "Can't apply LCONTR*-ETA."))
		  (tactic-output msg nil)
		  (values (list pline) msg 'fail)))))
   "Applies LCONTR*-ETA, if that will change the support line."))

(deftactic ext=-tac 
  (nat-ded
   (lambda (pline)
      (let* ((matched-plans (remove-if-not 
                               #'(lambda (x) (core::real-ext=-p (line-assertion x) 'compose))
                               (list pline)))
             (matched-plan
                (if (eq tacmode 'interactive)
                    (find-if #'(lambda (x) 
			         (query 
			            (format nil 
				       "Apply EXT= to line ~D?"
				       (linealias x)) t))
			  matched-plans)
                     (car matched-plans)))
              (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
    (if matched-plan                 
       (let ((msg "Applied EXT=."))
         (if (%catch% (comdecode (list 'ext= (linealias pline) '!))
		    (fail nil))
                (progn (tactic-output msg t)
                       (values (set-difference (mapcar #'car (proof-plans dproof))
					       oldplans)
                               msg 'succeed))
                (progn 
		  (tactic-output "EXT= failed." nil)
		  (values (list pline) "EXT= failed." 'fail)))))))
  "Applies EXT= if planned line is appropriate."))


(deftactic ext=0-tac 
  (nat-ded
   (lambda (pline)
      (let* ((matched-plans (remove-if-not 
                               #'(lambda (x) (core::real-ext=-p (line-assertion x) 'O))
                               (list pline)))
             (matched-plan
                (if (eq tacmode 'interactive)
                    (find-if #'(lambda (x) 
			         (query 
			            (format nil 
				       "Apply EXT=0 to line ~D?"
				       (linealias x)) t))
			  matched-plans)
                     (car matched-plans)))
              (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
    (if matched-plan                 
       (let ((msg "Applied EXT=0."))
         (if (%catch% (comdecode (list 'ext=0 (linealias pline) '!))
		    (fail nil))
                (progn (tactic-output msg t)
                       (values (set-difference (mapcar #'car (proof-plans dproof))
					       oldplans)
                               msg 'succeed))
                (progn 
		  (tactic-output "EXT= failed." nil)
		  (values (list pline) "EXT=0 failed." 'fail)))))))
  "Applies EXT=0 if planned line is appropriate."))


#+comment(deftactic equiv-eq-tac 
  (nat-ded
   (lambda (pline)
      (let* ((matched-plans (remove-if-not 
                                #'(lambda (x) (core::contains-ext=i (line-assertion x)))
                                (assoc pline (proof-plans dproof))))
             (matched-plan
                (if (eq tacmode 'interactive)
                    (find-if #'(lambda (x) 
			         (query 
			            (format nil 
				       "Apply EQUIV-EQ to line ~D?"
				       (linealias x)) t))
			  matched-plans)
                     (car matched-plans)))
              (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
    (if matched-plan                 
       (let ((msg "Applied EQUIV-EQ."))
         (if (%catch% (comdecode (list 'EQUIV-EQ (linealias matched-plan) '!))
		    (fail nil))
                (progn (tactic-output msg t)
                       (values (set-difference (mapcar #'car (proof-plans dproof))
					       oldplans)
                               msg 'succeed))
                (progn 
		  (tactic-output "EQUIV-EQ failed." nil)
		  (values (list pline) "EQUIV-EQ failed." 'fail)))))))
  "Applies EQUIV-EQ if planned line is appropriate."))

(deftactic equiv-eq-contr-tac 
  (nat-ded
   (lambda (pline)
      (let* ((matched-plans (remove-if-not 
                                #'(lambda (x) (core::contains-= (line-assertion x)))
                                (list pline)))
             (matched-plan
                (if (eq tacmode 'interactive)
                    (find-if #'(lambda (x) 
			         (query 
			            (format nil 
				       "Apply EQUIV-EQ-CONTR to line ~D?"
				       (linealias x)) t))
			  matched-plans)
                     (car matched-plans)))
              (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
    (if matched-plan                 
       (let ((msg "Applied EQUIV-EQ-CONTR."))
         (tactic-output msg t)
	 (comdecode (list 'EQUIV-EQ-CONTR (linealias matched-plan) '!))
	 (values (set-difference (mapcar #'car (proof-plans dproof))
				 oldplans)
		 msg 'succeed))
      (progn 
	(tactic-output "EQUIV-EQ-CONTR failed." nil)
	(values (list pline) "EQUIV-EQ-CONTR failed." 'fail)))))
  "Applies EQUIV-EQ-CONTR if planned line is appropriate."))

(deftactic equiv-eq-expd-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	    (matched-supports 
	     (remove-if-not #'(lambda (x) (core::contains-= (line-assertion x))) supports))
	   (matched-support
	    (if (eq tacmode 'interactive)
		(find-if #'(lambda (x)
			     (query (format nil "Apply EQUIV-EQ-EXPD to line ~D?"
						 (linealias x)) t))
			 matched-supports)
		(car matched-supports)))
	    (oldplans (delete pline (mapcar #'car (proof-plans dproof)))))
       (if matched-support
	   (let ((msg "Applied EQUIV-EQ-EXPD."))
	     (tactic-output msg t)
	     (comdecode (list 'equiv-eq-expd (linealias matched-support) '$ '$ '$ '$ '$)) 
	     (values (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)
		     msg 'succeed))
	 (let ((msg "Can't apply EQUIV-EQ-EXPD."))
		  (tactic-output msg nil)
		  (values (list pline) msg 'fail)))))
   "Applies EQUIV-EQ-EXPD, if that will change the support line."))
