;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WEAK-LABEL)

;;;
;;; File: Weak-Mac
;;;
;;; Macros for weak labels
;;;

(deffile weak-mac
  (part-of weak-label)
  (extension clisp)
  (mhelp "Flags and labels of weak flavor."))

(context weak-labels)

(defflag print-weak
  (flagtype boolean)
  (default t)
  (subjects printing)
  (mhelp "If T, weak labels are printed, otherwise they wff the represent
will be printed."))

;;;***********************************************************************
;;;Operations for weak lables in AUTO package should be specified in *****
;;;the file WEAK-MAC-AUTO.clisp                                      *****
;;;***********************************************************************

(defflavor weak
  (mhelp "A weak label stands for another wff, but dissolves under most
operations like substitution etc.")
  (wffeq (lambda (wff1 wff2)
	   (wffeq (get-weak wff1) wff2)))
  (wffeq-ab11 (lambda (wff1 wff2 varstack)
	       (wffeq-ab1 (get-weak wff1) wff2 varstack)))
  (wffeq-ab12 (lambda (wff1 wff2 varstack)
	       (wffeq-ab1 wff1 (get-weak wff2) varstack)))
  (subst-term-var-rec
   (lambda (term var inwff) (subst-term-var-rec term var (get-weak inwff))))
  (subst-l-term-rec
   (lambda (term var inwff) (subst-l-term-rec term var (get-weak inwff))))
;  (substitute-l-term-var-old
;   (lambda (term var inwff)
;     (substitute-l-term-var-old term var (get-weak inwff))))
  (free-for
   (lambda (term var inwff) (free-for term var (get-weak inwff))))
  (lambda-norm
   (lambda (gwff) (lambda-norm (get-weak gwff))))
;;; the three following may help in keeping other wffops off this list. cpk
  (binding (lambda (gwff) (binding (get-weak gwff))))
  (bindvar (lambda (gwff) (bindvar (get-weak gwff))))
  (bindhead (lambda (gwff) (bindhead (get-weak gwff))))
;;; 
  (neg-norm-rec (lambda (gwff pos) (neg-norm-rec (get-weak gwff) pos)))
  (ab-normal-p1 (lambda (inwff) (ab-normal-p1 (get-weak inwff)))) ;-si.
  (ab-normalize-main
   (lambda (gwff) (ab-normalize-main (get-weak gwff))))
  (conjunctive-normal-form1
   (lambda (gwff neg-flag)
     (conjunctive-normal-form1 (get-weak gwff) neg-flag)))
  ;;
  (lexpd-rec
   (lambda (var term tfree inwff occurs prep-sw)
     (declare (special occs val-occs)
	      (ignore prep-sw))
     (cond ((eq term inwff) 
	    (setq occs (1+ occs))
	    (cond ((good-occ occs occurs) 
		   (setq val-occs (1+ val-occs)) var)
		  ((member var tfree) (throwfail nil))
		  (t inwff)))
	   ((free-in var (get-weak inwff)) (throwfail nil))
	   (t inwff))))
  (prepare-for
   (lambda (var term tfree inwff occurs)
     (declare (special occs val-occs)
	      (ignore tfree var))
     (cond ((eq term inwff)
	    (setq occs (1+ occs))
	    (and (good-occ occs occurs)
		 (setq val-occs (1+ val-occs)))
	    inwff)
	   (t inwff))))
  (free-in (lambda (var inwff) (free-in var (get-weak inwff))))
  (free-vars (lambda (gwff bind-list) (free-vars (get-weak gwff) bind-list)))
  (pull-negation (lambda (gwff) (pull-negation (get-weak gwff))))
  (push-negation (lambda (gwff) (push-negation (get-weak gwff))))
  (delete-weak (lambda (label gwff) (cond ((eq label gwff) (get-weak gwff))
					  (t gwff))))
  ;; The next section is required for printing purposes
  (printwff (lambda (wff brackets depth)
	      (if print-weak (pp-symbol-space wff)
		  (printwff (get-weak wff) brackets depth))))
  (prt-symbol-p (lambda (gwff) (if print-weak t
				   (prt-symbol-p (get-weak gwff)))))
  (prt-infix-op (lambda (gwff) (if print-weak nil
		    (prt-infix-op (get-weak gwff)))))
  (prt-prefix-op (lambda (gwff) (if print-weak nil
		     (prt-prefix-op (get-weak gwff)))))
  (prt-associative-p (lambda (gwff) (if print-weak nil
			 (prt-associative-p (get-weak gwff)))))
  ;; Misc. required operations follow
  (type (lambda (gwff) (type (get-weak gwff))))
  (gwff-p (lambda (gwff) (gwff-p (get-weak gwff))))
  (gwff-q (lambda (gwff) (declare (ignore gwff)) t))
  (legal-type-p1 (lambda (gwff) (legal-type-p1 (get-weak gwff))))
  ;; The next gives the default for test operations.  
  ;; See macro apply-label in flavoring.lisp
  (default-boolean-result (lambda (gwff) (get-weak gwff)))
  ;; Stepping operations follow
  (gar (lambda (gwff) (gar (get-weak gwff))))
  (gdr (lambda (gwff) (gdr (get-weak gwff))))
  (glr (lambda (gwff) (glr (get-weak gwff))))
  (grr (lambda (gwff) (grr (get-weak gwff))))
  (replace-gar
   (lambda (gwff newgarwff) (replace-gar (get-weak gwff) newgarwff)))
  (replace-gdr
   (lambda (gwff newgdrwff) (replace-gdr (get-weak gwff) newgdrwff)))
  (lib-promptfn-gwff
    (lambda (name type help file modify comment old-value)
      (declare (ignore type help file modify comment old-value))
      (get-weak name)))
  (substitute-term-term
   (lambda (term1 term2 wff)
     (substitute-term-term term1 term2 (get-weak wff))))
  (wff-length
   (lambda (wff)
     (wff-length (get-weak wff))))
  (subst-occs***
   (lambda (wff1 wff2 eq-lists pvs)
     (subst-occs*** 
      (if (and (label-q wff1) (symbolp wff1) (eq 'weak (get wff1 'flavor)))
	  (get-weak wff1)
	wff1)
      (if (and (label-q wff2) (symbolp wff2) (eq 'weak (get wff2 'flavor)))
	  (get-weak wff2)
	wff2)
      eq-lists pvs)))
  (wff-applic-p (lambda (gwff) (wff-applic-p (get-weak gwff))))
  (boundwff-p (lambda (gwff) (boundwff-p (get-weak gwff))))
  (boundwff-q (lambda (gwff) (boundwff-q (get-weak gwff))))
  (reduct-p (lambda (gwff) (reduct-p (get-weak gwff))))
  (infix-p (lambda (gwff) (infix-p (get-weak gwff))))
  (infix-op-p (lambda (gwff) (infix-op-p (get-weak gwff))))
  (implies-p (lambda (gwff) (implies-p (get-weak gwff))))
  (equiv-p (lambda (gwff) (equiv-p (get-weak gwff))))
  (and-p (lambda (gwff) (and-p (get-weak gwff))))
  (or-p (lambda (gwff) (or-p (get-weak gwff))))
  (not-p (lambda (gwff) (not-p (get-weak gwff))))
  (ae-bd-wff-p (lambda (gwff) (ae-bd-wff-p (get-weak gwff))))
  (lambda-bd-p (lambda (gwff) (lambda-bd-p (get-weak gwff))))
  (a-bd-wff-p (lambda (gwff) (a-bd-wff-p (get-weak gwff))))
  (e-bd-wff-p (lambda (gwff) (e-bd-wff-p (get-weak gwff))))
  (equals-p (lambda (gwff) (equals-p (get-weak gwff))))
  (equality-p (lambda (gwff) (equality-p (get-weak gwff))))
  (contains-= (lambda (gwff) (contains-= (get-weak gwff))))
  (contains-ext= (lambda (gwff) (contains-ext= (get-weak gwff))))
  (real-ext=-p (lambda (gwff complex) (real-ext=-p (get-weak gwff) complex)))
)
