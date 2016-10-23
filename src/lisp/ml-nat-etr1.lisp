;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ml)
(part-of ml-etr-tactics)

(context etr-nat)

(deffile ml-nat-etr1
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Functions and macros needed for translating from natural deduction
proofs to expansion proofs."))

;;; What we mean by intro or elim here is not quite introduction rule
;;; or elimination rule.  What we really want is for intro to mean
;;; that the etree node for this line is the parent (in some way) of
;;; the etree nodes for the lines that justify it; a line
;;; justified by iconj is a good example.  For elim, we want it to
;;; mean that the node for this line would be the child of its
;;; justifying line.  Rules like neg-elim and indirect do not really fit in 
;;; here, and we make them fail both tests.

(defun justified-by-intro (line)
  (let ((just-rule (line-just-rule line))
	(just-lines (line-just-lines line)))
    (or (and (string-equal just-rule "Conj") (= 2 (length just-lines))) ;ICONJ
	(string-equal just-rule "Deduct") ; DEDUCT
	(string-equal just-rule "UGen") ; UGEN
	(string-equal just-rule "EGen") ; EGEN
	(string-equal just-rule "AB") ; ABE, ABU, AB*
	(and (string-equal just-rule "Defn") ; IDEF 
	     (contains-defn (line-assertion line))
	     (wffeq-ab (line-assertion (car just-lines))
		       (inst-def (line-assertion line))))
	(string-equal just-rule "EquivWffs")
	(string-equal just-rule "RuleQ")
	(string-equal just-rule "RuleC")
	(and (string-equal just-rule "Equality") ; leibniz=
	     (equals-p (line-assertion line)))
	(and (string-equal just-rule "Lambda") ; LEXPD*
	     (wffeq-ab (line-assertion (car just-lines))
		       (lnorm (line-assertion line))))
	(and (string-equal just-rule "Ext=") ; EXT= and EXT=0
	     (equals-p (line-assertion line)))
	(string-equal just-rule "Sym=") ; SYM=
	(string-equal just-rule "ImpEquiv") ; IMPLICS-EQUIV
	(string-equal just-rule "Disj-Imp") ; DISJ-IMP
	(string-equal just-rule "Imp-Disj") ; DISJ-IMP
	(string-equal just-rule "NegIntro") ; INEG
;	(string-equal just-rule "Absurd") ; ABSURD
;	(string-equal just-rule "NegElim") ; ENEG
	(and (string-equal just-rule "Neg") ; PULLNEG
	     (not-p (line-assertion line)))
)))

(defun justified-by-elim (line)
  (let ((just-rule (line-just-rule line))
	(just-lines (line-just-lines line)))
    (or (string-equal just-rule "Hyp") ; HYP, DEDUCT
	(and (string-equal just-rule "Conj") ; ECONJ
	     (= 1 (length just-lines)))
	(string-equal just-rule "EquivImp") ; EQUIV-IMPLICS
	(string-equal just-rule "MP") ; MP 
	(string-equal just-rule "UI") ; UI
	(string-equal just-rule "Choose") ; RULEC
	(string-equal just-rule "EquivConj") ; equivalence rewritten to disjunction
	(and (string-equal just-rule "Ext=")
	     (equals-p (line-assertion (car just-lines))))
	(and (string-equal just-rule "Equality")
	     (equals-p (line-assertion (car just-lines))))
	(and (string-equal just-rule "Defn") ; EDEF
	     (contains-defn (line-assertion (car just-lines)))
	     (wffeq-ab (line-assertion line)
		       (inst-def (line-assertion (car just-lines)))))
	(and (string-equal just-rule "Lambda") ; LCONTR*
	     (wffeq-ab (lnorm (line-assertion (car just-lines)))
		       (line-assertion line)))
	(string-equal just-rule "Subst") ; SUBSTITUTE
	(string-equal just-rule "Case 1");CASES
	(string-equal just-rule "Case 2")
	(and (string-equal just-rule "Neg") ; PUSHNEG
	     (not (not-p (line-assertion line)))))))



(defun same-ify ()
  (with-input-from-string (*standard-input* (string #\Newline))
			  (cleanup))
  (negelim-ify dproof)
  (do ((lines-to-check nil (cdr lines-to-check))
       (line (car (last (proof-lines dproof))) (car lines-to-check)))
      ((null line))
      (when (or (justified-by-intro line)
		(string-equal (line-just-rule line) "Cases")
		;(string-equal (line-just-rule line) "Indirect")
		)
	(dolist (just-line 
		  (if (or (string-equal (line-just-rule line) "Cases")
			  (string-equal (line-just-rule line) "RuleC"))
		      (cdr (line-just-lines line))
		    (line-just-lines line)))
		(if (justified-by-elim just-line)
		    (make-same-line just-line line)
		  (setq lines-to-check 
			(nconc lines-to-check (list just-line))))))
      )
  (same-ify2)
  (same-ify3))

;;; Looks for places where Same is used to justify an existentially or
;;; universally quantified line, and instead inserts a trivial 
;;; ui or rulec 

(defun same-ify2 ()
  (do ((lines-to-check (reverse (proof-lines dproof)) (cdr lines-to-check)))
      ((null (car lines-to-check)))
    (let* ((line (car lines-to-check))
	   (rule (line-just-rule line))
	   (top-line (car (line-just-lines line)))
	   (assertion (line-assertion line)))
      (when (and (string-equal rule "Same as") (ae-bd-wff-p assertion))
	(if (eq (cdar assertion) 'forall)
	    ;; insert a UI and UGen
	    (progn 
	      (make-room-after top-line 1)
	      (comdecode (list 'plan (linealias line) '!))
	      (comdecode (list 'ui (linealias top-line) (1+ (linealias top-line))
			 (list 'quote (bindvar assertion)) '$ '$ '$ '$ '$))
	      (make-room-before line 1)
	      (comdecode (list 'same (1- (linealias line)) 
			 (1+ (linealias top-line)) '!))
	      (comdecode (list 'ugen line (1- (linealias line)) '!)))
	    ;; insert a RULEC and EGen
	    (progn 
	      (make-room-after top-line 2)
	      (make-room-before line 1 )
	      (comdecode (list 'plan (linealias line) '!))
	      (comdecode (list 'rulec (linealias line)
			 (linealias top-line) (1- (linealias line))
			 (1+ (linealias top-line)) 
			 (list 'quote (bindvar assertion))
			 (list 'quote (gdr assertion)) 
			 (list 'quote (gdr assertion)) '!))
	      (setq line (numalias (1- (linealias line))))
	      (make-room-before line 1)
	      (comdecode (list 'same (1- (linealias line)) 
			 (1+ (linealias top-line)) '!))
	      (comdecode (list 'egen (linealias line) (1- (linealias line))
			 (list 'quote (bindvar assertion)) '!)))
	    )))))

;;; Looks for uses of the Indirect1 rule, and applies econj to the
;;; conjunction, thus allowing use of xlate-indirect2

(defun same-ify3 ()
  (dolist (line (proof-lines dproof))
    (when (and (string-equal "Indirect" (line-just-rule line))
	       (= 1 (length (line-just-lines line)))
	       (not (wffeq 'falsehood (line-assertion 
					(car (line-just-lines line))))))
      (let ((top-line (car (line-just-lines line))))
	(make-room-after top-line 2)
	(comdecode (list 'econj (linealias top-line) 
			 (+ 2 (linealias top-line))
			 (1+ (linealias top-line)) '!))
	(setf (line-just-lines line)
	      (list (numalias (1+ (linealias top-line)))
		    (numalias (+ 2 (linealias top-line)))))))))

;;; just-line is justified by an elimination rule, and is used to
;;; justify line

(defun make-same-line (just-line line)
  (let ((num (linealias just-line))
	(new-line nil))
    (make-room-after just-line 1)
    (comdecode `(same ,(1+ num) ,num $ $ $))
    (setq new-line (numalias (1+ num)))
    (setf (line-just-lines line)
	  (nsubstitute new-line just-line (line-just-lines line)))))

(defun nat-xlate (line)
  (declare (special line-node-list mate-list))
  (let ((just-rule (line-just-rule line))
	(just-terms (line-just-terms line))
	(just-lines (line-just-lines line))
	(assertion (line-assertion line)))
  (cond ((string-equal just-rule "Same as") ;done maybe
	 (xlate-same line just-lines just-terms assertion))
	((string-equal just-rule "Deduct")  ;done
	 (xlate-deduct line just-lines just-terms assertion))
	((string-equal just-rule "Cases") ;done
	 (xlate-cases line just-lines just-terms assertion))	
	((and (string-equal just-rule "Conj") ;done
	      (= 1 (length just-lines)))
	 (xlate-econj line just-lines just-terms assertion))
	((or (string-equal just-rule "Case 1") ;done
	     (string-equal just-rule "Case 2"))
	 (xlate-case line just-lines just-terms assertion))
	((and (string-equal just-rule "Conj") ;done
	      (= 2 (length just-lines)))
	 (xlate-iconj line just-lines just-terms assertion))	
	((string-equal just-rule "UGen") ; done
	 (xlate-ugen line just-lines just-terms assertion))
	((string-equal just-rule "UI")  ; done
	 (xlate-ui line just-lines just-terms assertion))
	((string-equal just-rule "EGen") ; done
	 (xlate-egen line just-lines just-terms assertion))
	((string-equal just-rule "RuleC") ; done
	 (xlate-rulec line just-lines just-terms assertion))
	((string-equal just-rule "MP")    ; done maybe
	 (xlate-mp line just-lines just-terms assertion))
	((or (string-equal just-rule "EquivImp") ;done 
	     (string-equal just-rule "ImpEquiv")
	     (string-equal just-rule "Disj-Imp")
	     (string-equal just-rule "Imp-Disj"))
	 (xlate-rewrite line just-lines  assertion 'auto::equiv-implics))
	((string-equal just-rule "Lambda") ; done
	 (xlate-rewrite line just-lines assertion 'lambda))
	((string-equal just-rule "EquivConj") ; done
	 (xlate-rewrite line just-lines assertion 'auto::equiv-disjs))
	((string-equal just-rule "Ext=") ; done
	 (xlate-rewrite line just-lines assertion 'auto::ext=))
	((string-equal just-rule "Equality") ; done
	 (xlate-rewrite line just-lines assertion 'auto::leibniz=))
;	((string-equal just-rule "Sym=")
;	 (xlate-rewrite line just-lines assertion 'auto::equality))
	((string-equal just-rule "AB") ; done
	 (xlate-rewrite line just-lines assertion 'auto::ab))
	((or (string-equal just-rule "Defn") ;done
	     (string-equal just-rule "EquivWffs"))
	 (xlate-rewrite line just-lines assertion 'auto::equivwffs))
	((string-equal just-rule "RuleQ") ;done
	 (xlate-rewrite line just-lines assertion 'auto::ruleq))
	((string-equal just-rule "Subst")
	 (xlate-subst line just-lines just-terms assertion))
	((and (string-equal just-rule "Indirect")
	      (= 2 (length just-lines)))
	 (xlate-indirect2 line just-lines))
	((and (string-equal just-rule "Indirect")
	      (wffeq (line-assertion (car just-lines)) 'falsehood))
	 (xlate-indirect line just-lines))
	((string-equal just-rule "NegElim")
	 (xlate-eneg line just-lines))
	((string-equal just-rule "NegIntro")
	 (xlate-ineg line just-lines))
	((string-equal just-rule "Absurd")
	 (xlate-absurd line just-lines))
;	((string-equal just-rule "Assume negation")
;	 (xlate-assume-negation line just-lines just-terms assertion))
	((and (string-equal just-rule "Neg") ; PULLNEG
	      (not-p assertion))
	 (xlate-pullneg line just-lines assertion))
	((and (string-equal just-rule "Neg") ; PUSHNEG
	      (not (not-p assertion)))
	 (xlate-pushneg line just-lines assertion))
	((and (string-equal just-rule "Neg") 
	      (or
		(and (not-p assertion) (boundwff-q (gdr assertion)))
		(and (not-p (line-assertion (car just-lines)))
		     (boundwff-q assertion))))
	 (xlate-rewrite line just-lines assertion 'ruleq))
;	((string-equal just-rule "Neg") 
;	 (xlate-rewrite line just-lines assertion 'equiv-implics))
;	((string-equal just-rule "Subst=")
;	 (xlate-subst= line just-lines just-terms assertion))
	((string-equal just-rule "RuleP")
	 (xlate-rulep line just-lines))
	((string-equal just-rule "Hyp")
	 nil)
	(t (throwfail "Don't know how to translate line " 
		      (line . existing-line) ".")))))


;(defun cl-user::show-vp ()
;  (display-vp-diag (etree-to-jform (eproof-etree current-eproof))))

(defun use-of-indirect2-p (line)
  (and (string= "Indirect" (line-just-rule line))
       (= (length (line-just-lines line)) 2)))


(defun use-of-indirect1-p (line)
  (and (string= "Indirect" (line-just-rule line))
       (= (length (line-just-lines line)) 1)
       (and-p (line-assertion (car (line-just-lines line))))))

(defun negelim-ify (proof)
  (let ((lines (proof-lines proof)))
    (dolist (line (reverse lines))
      (cond ((use-of-indirect2-p line)
	     (negelim-indirect2 line))
	    ((use-of-indirect1-p line)
	     (negelim-indirect1 line))
	    (t nil)))))

(defun negelim-indirect2 (line)
  (let ((orig-just-lines (line-just-lines line)))
    (make-room-before line 2)
    (comdecode (list 'plan (linealias line)))
    (comdecode 
     (list 'indirect (linealias line)  
	   (1- (linealias line)) 
	   (or (linealias (car 
			   (set-difference 
			    (apply #'append
				   (mapcar #'(lambda (x) (line-hypotheses x))
					   orig-just-lines))
			    (line-hypotheses line))))
	       (- (linealias line) 2))
	   '$ '$  '$ '$))
    (comdecode
     (list 'eneg
	   (1- (linealias line)) 
	   (linealias (cadr orig-just-lines))
	   (linealias (car orig-just-lines)) 
	   (list 'quote (line-assertion (car orig-just-lines)))
	   '$ '$ '$))

    ))

(defun negelim-indirect1 (line)
  (let ((orig-just-line (car (line-just-lines line))))
    (comdecode (list 'plan (linealias line)))
    (make-room-after orig-just-line 2)
    (comdecode
     (list 'econj (linealias orig-just-line)
	   (1+ (linealias orig-just-line))
	   (+ 2 (linealias orig-just-line)) '!))
    (make-room-before line 2)
    (comdecode 
     (list 'indirect (linealias line)  
	   (1- (linealias line)) 
	   (or (linealias (car 
			   (set-difference 
			    (line-hypotheses orig-just-line)
			    (line-hypotheses line))))
	       (- (linealias line) 2))
	   '$ '$  '$ '$))
    (comdecode
     (list 'eneg
	   (1- (linealias line)) 
	   (1+ (linealias orig-just-line))
	   (+ 2 (linealias orig-just-line))
	   (list 'quote (line-assertion 
			 (numalias (+ 2 (linealias orig-just-line)))))
	   '$ '$ '$))
    ))
