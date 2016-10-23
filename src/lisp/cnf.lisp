;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of LAMBDA-CALC)

;;;
;;; File: cnf
;;;
;;; defines editor operation CNF
;;;

(part-of lambda-calc)

(deffile cnf
  (part-of lambda-calc)
  (extension clisp)
  (mhelp 
 "Contains functions required to find conjunctive normal form of a wff."))

(context misc-edops)

(defedop cnf
  (alias conjunctive-normal-form)
  (result-> edwff)
  (edwff-argname gwff))

(defwffop conjunctive-normal-form
  (argtypes gwff)
  (argnames gwff)
  (resulttype gwff)
  (mhelp "Find the conjunctive normal form of a wff."))

;(defun CONJUNCTIVE-NORMAL-FORM (gwff)
;  (CONJUNCTIVE-NORMAL-FORM1 gwff nil))

(defun conjunctive-normal-form (gwff)
  (nest-inner 
    (nest-left (conjunctive-normal-form1 gwff nil) 'and)'and))

;;; nest-left takes an expression which is already in conjunctive
;;; or disjunctive normal form
;;; and rewrites it so that the conjunctions (disjunctions) are nested to the 
;;;left. for example, if we are dealing with a formula in cnf, 
;;; the resulting formula will either be a clause, or will be a conjunction
;;; whose right conjunct is a clause, and whose left conjunct is either a
;;; clause or is a conjunction which is also nested to the left.
;;; the analogous case will hold for disjunctive normal formulas.
;;; here the variable op indicates which form we are dealing with:
;;; and for cnf formulas, or for dnf formulas.
;;; comments below refer to a formula in cnf, replace "conjunct"
;;; by "disjunct" and "and" by "or" for dnf.

(defun nest-left (gwff op)
  ;; if expression is a conjunction, then first nest the ands of
  ;; each of its conjuncts.
  (cond ((not (infix-p gwff))
	 gwff)
	((eq (caar gwff) op)
         (setq gwff (acons op (nest-left (cdar gwff) op)
			   (nest-left (cdr gwff) op)))
	 ;; if the resulting second conjunct is also a conjunction,
	 ;; then conjoin its first conjunct with the first conjunct
	 ;; of the higher-level expression. then nest that conjunction,
	 ;; and make it the first conjunct of a conjunction whose
	 ;; second conjunct is the second conjunct of the original
	 ;; second conjunct.
	 ;; e.g., ((and.a).((and.b).c)) becomes
	 ;;       ((and.((and.a).b)).c)
         (if (and (infix-p (cdr gwff)) (eq (caadr gwff) op)) 
	     (acons op (nest-left (acons op (cdar gwff) (cdadr gwff)) op)
		    (cddr gwff))
	     ;; otherwise, the expression is now nested, so return it.
	     gwff))
	;; if not a conjunction, just return the expression.
        (t gwff)))

(defun nest-inner (gwff op)
  (let ((neg-op (if (eq op 'or) 'and 'or)))
    (cond ((not (infix-p gwff)) gwff)
	  ((eq (caar gwff) op)
	   (acons op (nest-inner (cdar gwff) op) (nest-inner (cdr gwff) op)))
	  (t (nest-left gwff neg-op)))))

(defun conjunctive-normal-form1 (gwff neg-flag)
  (cond ((label-q gwff)
	 (apply-label gwff (conjunctive-normal-form1 gwff neg-flag)))
	((or (lsymbol-q gwff) (boundwff-q gwff))
	 (if neg-flag (cons 'not gwff) gwff))
	((not-p gwff) (conjunctive-normal-form1 (cdr gwff) (not neg-flag)))
	((infix-p gwff)
	 (case (caar gwff)
	   (and (let ((left-conjunct
			(conjunctive-normal-form1 (cdar gwff) neg-flag))
		      (right-conjunct
			(conjunctive-normal-form1 (cdr gwff) neg-flag)))
		  (if neg-flag
		      (conjunctive-normal-form1
			(acons 'or left-conjunct right-conjunct) nil)
		      (acons 'and left-conjunct right-conjunct))))
	   (or (let ((left-disjunct
		       (conjunctive-normal-form1 (cdar gwff) neg-flag))
		     (right-disjunct
		       (conjunctive-normal-form1 (cdr gwff) neg-flag)))
		 (if neg-flag
		     (acons 'and left-disjunct right-disjunct)
		     (if (and-p left-disjunct)
			 (acons 'and (conjunctive-normal-form1
				       (acons 'or (cdar left-disjunct)
					      right-disjunct) nil)
				(conjunctive-normal-form1
				  (acons 'or (cdr left-disjunct)
					 right-disjunct) nil))
			 (if (and-p right-disjunct)
			     (acons 'and
				    (conjunctive-normal-form1
				      (acons 'or left-disjunct
					     (cdar right-disjunct))
				      nil)
				    (conjunctive-normal-form1
				      (acons 'or left-disjunct
					     (cdr right-disjunct))
				      nil))
			     (acons 'or left-disjunct right-disjunct))))))
	   (implies (if neg-flag
			(acons 'and (conjunctive-normal-form1 (cdar gwff) nil)
			       (conjunctive-normal-form1 (cdr gwff) T))
			(conjunctive-normal-form1
			  (acons 'or (cons 'not (cdar gwff)) (cdr gwff)) nil)))
	   (equiv (let ((left-arg (cdar gwff))
			(rt-arg (cdr gwff)))
		    (if neg-flag
			(acons 'and
			       (conjunctive-normal-form1
				 (acons 'or (cons 'not left-arg)
					(cons 'not rt-arg)) nil)
			       (conjunctive-normal-form1
				 (acons 'or left-arg rt-arg) nil))
			(acons 'and
			       (conjunctive-normal-form1
				 (acons 'or (cons 'not left-arg)
					rt-arg) nil)
			       (conjunctive-normal-form1
				 (acons 'or left-arg
					(cons 'not rt-arg)) nil)))))
	   (T gwff)))
	(T (if neg-flag (cons 'not gwff) gwff))))

(defun hvars (gwff)
  (delete-duplicates (hvars-rec gwff)))

(defun hvars-rec (gwff)
  (cond ((label-q gwff)
	 (apply-label gwff (hvars-rec gwff)))
	((lsymbol-q gwff)
	 (if (propsym-p gwff) (list gwff) nil))
	((not-p gwff) (hvars-rec (cdr gwff)))
	((infix-p gwff)
	 (nconc (hvars-rec (glr gwff)) (hvars-rec (grr gwff))))
	((boundwff-q gwff)
	 (hvars-rec (cdr gwff)))
	(t (hvars-rec (car gwff)))))

(defedop hvars
  (alias hvars)
  (result->)
  (edwff-argname gwff))

(defwffop hvars
  (argtypes gwff)
  (argnames gwff)
  (resulttype gvarlist)
  (mhelp "Find all head variables of a wff."))

(defun head (gwff)
  (cond ((label-q gwff)
	 (apply-label gwff (head gwff)))
	((lsymbol-q gwff) gwff)
	((not-p gwff) (head (cdr gwff)))
	((boundwff-q gwff)
	 (head (cdr gwff)))
	(t (head (car gwff)))))

(defedop head
  (alias head)
  (result->)
  (edwff-argname gwff))

(defwffop head
  (argtypes gwff)
  (argnames gwff)
  (resulttype gvar)
  (mhelp "Find the head of a gwff."))

(defedop min-scope
  (alias min-quant-scope)
  (result-> edwff)
  (edwff-argname gwff))

(defwffop min-quant-scope
  (argtypes gwff)
  (argnames gwff)
  (resulttype gvar)
  (mhelp "Minimize the scope of quantifiers in a gwff. Deletes vacuous
quantifiers. During proof transformation, the gap between a formula
and its min-quant-scope version is filled by RULEQ."))

(defun rewrite-equiv-old (wff pos)
  (let ((left-arg (cdar wff))
	(rt-arg (cdr wff)))
    (if pos (acons 'or (acons 'and  left-arg rt-arg)
		   (acons 'and (cons 'not left-arg) (cons 'not rt-arg)))
	(acons 'and (acons 'implies left-arg rt-arg)
	       (acons 'implies  rt-arg left-arg)))))

(defflag rewrite-equivs
  (flagtype posinteger)
  (default 1)
  (subjects mating-search important ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms98-1 transmit)
  (mhelp "This chooses one of the two ways of constructing an etree
from an equivalence A EQUIV B:
1 chooses the option with the fewest vertical paths
   (positive: A AND B OR ~A AND ~B
    negative: A IMPLIES B AND B IMPLIES A)
2 chooses the option with the fewest horizontal paths
   (negative: A AND B OR ~A AND ~B
    positive: A IMPLIES B AND B IMPLIES A)
3 behaves as for 2 except for the first equivalence it finds, 
  when it behaves as for 1. (This means that a gwff which is a 
  quantified equivalence will produce an etree which can be split.)
4 always chooses A IMPLIES B AND B IMPLIES A
5 always chooses A AND B OR ~A AND ~B
Any other setting will behave like 1.

This does not work with MIN-QUANTIFIER-SCOPE T; in that case, 
etrees will be constructed as in case 1, regardless of the setting
of this flag."))

(defvar *first-equiv*)
(defvar *first-equiv2*)

(defun rewrite-equiv (wff pos)
  (declare (special auto:min-quantifier-scope))
  (let ((left-arg (cdar wff))
	(rt-arg (cdr wff))
	(rewrite-as-conj (if (and (boundp auto::min-quantifier-scope)
				  auto::min-quantifier-scope)
			     (not pos)
			   (case rewrite-equivs
			     (1 (not pos))
			     (2 pos)
			     (3 (if *first-equiv* (not pos) pos))
			     (4 t)
			     (5 nil)
			     (t (not pos))))))
    (setq *first-equiv* nil *first-equiv2* t)
    (if rewrite-as-conj
	(acons 'and (acons 'implies left-arg rt-arg)
	       (acons 'implies  rt-arg left-arg))
      (acons 'or (acons 'and  left-arg rt-arg)
	     (acons 'and (cons 'not left-arg) (cons 'not rt-arg)))
      )))

(defun cohead (head)
   (case (cdr head)
      (exists (cons (car head) 'forall))
      (forall (cons (car head) 'exists))
      (t head)))

(defun one-step-mqs-bd (head scope)
  (let ((gwff (cons head scope))
	(infix (infix-p scope)))
   (case (cdr head)
     ((forall exists) 

      (if (and infix (eq (caar scope) 'equiv))
	  (if (eq (cdr head) 'forall)
	      (setq scope (rewrite-equiv scope nil))
	    (setq scope (rewrite-equiv scope t))))

      (if (and infix (memq (caar scope) '(and or implies)))
          (let ((left (free-in (car head) (cdar scope)))
                (right (free-in (car head) (cdr scope))))
            (case (caar scope)
              (and (cond ((and left right) 
			  (if (eq 'forall (cdr head))
			      (acons (caar scope) 
				     (one-step-mqs-bd head (cdar scope)) 
                                     (one-step-mqs-bd head (cdr scope)))
                              gwff))
                         (left (acons (caar scope) 
				      (one-step-mqs-bd head (cdar scope)) 
				      (cdr scope)))
                         (right (acons (caar scope) 
				       (cdar scope) 
				       (one-step-mqs-bd head (cdr scope))))
                         (t scope)))
              (or (cond ((and left right) 
			  (if (eq 'exists (cdr head))
			      (acons (caar scope) 
				     (one-step-mqs-bd head (cdar scope)) 
				     (one-step-mqs-bd head (cdr scope)))
                              gwff))
                         (left (acons (caar scope) 
				      (one-step-mqs-bd head (cdar scope))
				      (cdr scope)))
                         (right (acons (caar scope) 
				       (cdar scope) 
				       (one-step-mqs-bd head (cdr scope))))
                         (t scope)))
              (implies (cond ((and left right) 
			  (if (eq 'exists (cdr head))
			      (acons (caar scope) 
				     (one-step-mqs-bd (cohead head) (cdar scope)) 
				     (one-step-mqs-bd head (cdr scope)))
                              gwff))
                          (left (acons (caar scope)
				       (one-step-mqs-bd (cohead head) (cdar scope)) 
				       (cdr scope)))
                          (right (acons (caar scope) 
					(cdar scope) 
					(one-step-mqs-bd head (cdr scope))))
                          (t scope)))))
           (if (not-p scope) (cons (car scope) (one-step-mqs-bd (cohead head) (cdr scope)))
               (if (not (free-in (car head) scope)) scope gwff))))
     (t gwff))))

(defun one-step-mqs-infix (gwff)
  (let ((infix (caar gwff))
        (lwff (cdar gwff))
        (rwff (cdr gwff)))
   (case infix
      (and gwff)
      (or (if (not-p lwff) 
              (one-step-mqs-infix (acons 'implies (cdr lwff) rwff))
              (if (not-p rwff)
                  (one-step-mqs-infix (acons 'implies (cdr rwff) lwff))
                  gwff)))
      (implies (if (and (not-p lwff) (not-p rwff)) 
                   (one-step-mqs-infix (acons infix (cdr rwff) (cdr lwff)))
                   gwff))
      (t gwff))))
             

(defun min-quant-scope (gwff)
   (cond ((label-q gwff) 
          (apply-label gwff (min-quant-scope gwff)))
         ((lsymbol-p gwff) gwff)
         ((boundwff-p gwff)
 	  (one-step-mqs-bd (car gwff) (min-quant-scope (cdr gwff))))
         ((infix-p gwff)
          (one-step-mqs-infix 
              (acons (caar gwff) (min-quant-scope (cdar gwff)) (min-quant-scope (cdr gwff)))))
         ((null gwff) (throwfail "Sorry, NIL is not a gwff."))
         (t (cons (min-quant-scope (car gwff)) (min-quant-scope (cdr gwff))))))
 
(defun min-quant-scope-applicable (head scope)
  (if (eq (caar scope) 'equiv) T
    (let ((left (free-in (car head) (cdar scope)))
          (right (free-in (car head) (cdr scope))))
      (if (and left right)
          (if (and (eq (cdr head) 'forall))
              (if (eq (caar scope) 'and) T nil)
              (if (memq (caar scope) '(or implies)) T nil)) T))))

(defun min-quant-infix-applicable (infix left right)
  (case infix
    (and nil)
    (or (or (not-p left) (not-p right)))
    (implies (and (not-p left) (not-p right)))))

(defun mqs-applicable (gwff)
   (cond ((label-q gwff) (apply-label gwff (mqs-applicable gwff)))
	 ((lsymbol-p gwff) nil)
	 ((boundwff-p gwff) 
	  (or (and (memq (cdar gwff) '(forall exists))
		   (infix-p (cdr gwff))
		   (memq (caadr gwff) '(and or implies equiv))
	           (min-quant-scope-applicable (car gwff) (cdr gwff)))
	      (mqs-applicable (cdr gwff))))
	 ((infix-p gwff) 
	  (or (min-quant-infix-applicable (caar gwff) (cdar gwff) (cdr gwff))
	      (mqs-applicable (cdar gwff)) 
	      (mqs-applicable (cdr gwff))))
	 (t (or (mqs-applicable (car gwff)) (mqs-applicable (cdr gwff))))))

(defedop subformulas
  (alias find-subformulas)
  (result-> ignore)
  (edwff-argname gwff))

(defwffop find-subformulas
  (argtypes gwff typesym)
  (argnames gwff type)
  (resulttype gwff)
  (mhelp "Find all subformulas of a given type in a wff."))

(defun find-subformulas (gwff type)
  (let ((outlist nil))
    (declare (special outlist))
    (find-subformulas-real gwff type)
    (msgf t)
    (setq outlist (remove-duplicates outlist :test 'wffeq-ab))
    (dolist (out outlist) (msgf (out . gwff))))
  gwff)

(defun find-subformulas-real (gwff type)
  (declare (special outlist))
  (when (equal type (type gwff)) (push gwff outlist))
  (cond ((weak-label-p gwff) (find-subformulas-real (get gwff 'represents) type))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) 
	 (find-subformulas-real (bindvar gwff) type)
	 (find-subformulas-real (cdr gwff) type))
	(t (find-subformulas-real (gar gwff) type)
	   (find-subformulas-real (gdr gwff) type))))

(defun make-subformula-list (gwff)
  (let ((subfm-list nil))
    (declare (special subfm-list))
    (msl-real gwff)
    (when (fboundp 'auto::simul-substitute-l-term-var)
	  (setq subfm-list (remove-duplicates subfm-list :test #'same-subfmla)))
    subfm-list))

(defun msl-real (gwff)
  (declare (special subfm-list))
  (unless (gvar-p gwff)
	  (push (cons (type gwff) (cons (free-vars-of gwff) gwff)) subfm-list))
  (cond ((weak-label-p gwff) (msl-real (get gwff 'represents)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) 
	 (msl-real (bindvar gwff))
	 (msl-real (cdr gwff)))
	(t (msl-real (gar gwff))
	   (msl-real (gdr gwff)))))

(defun same-subfmla (x y)
  (let ((typex (car x))
	(typey (car y))
	(fvx (cadr x))
	(fvy (cadr y))
	(fmlx (cddr x))
	(fmly (cddr y)))
    (and (equal typex typey)
	 (= (length fvx) (length fvy))
	 (wffeq-ab (auto::simul-substitute-l-term-var (mapcar 'cons fvx fvy) fmlx) fmly))))


