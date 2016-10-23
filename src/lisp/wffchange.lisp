;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of wff-ops1)

;;;
;;; File WFFCHANGE
;;;
;;;operation to merge idempotent 
;;;

(deffile wffchange
  (part-of wff-ops1)
  (extension clisp)
  (mhelp "Contains operation to apply idempotent, commutative, 
associative laws, etc., to 'edwff'."))

(defun inf-go-on(gwff rec fun)
       (if rec (acons (caar gwff) (funcall fun (cdar gwff) rec) (funcall fun (cdr gwff) rec))
               (throwfail "The command can not be applied to this part of the formula."))) 

(defun pre-go-on(gwff rec fun)
       (if rec (cons (car gwff) (funcall fun (cdr gwff) rec))
               (throwfail "The command can not be applied to this part of the formula."))) 

;;;-------------
;;;The following function is used to make command really recursive.

(defun go-on (rest)
  (declare (special srec))
  (if (eq srec 0) (setq srec 1))
  rest)

;;;-------------

(defun complain-unless-gwff (gwff rec)
  (if (not (gwff-p gwff)) 
      (throwfail "The formula is ill-formed.")
      (if (not rec) (throwfail "The command cannot be applied to this wff.") gwff)))


(context changing)

(defwffop merge-idempotent
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Merges idempotent component(s) of a formula."))

(context recursively-changing)

(defwffop merge-idempotent*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively merges idempotent component(s) of a formula."))

(defun merge-idempotent (gwff) 
       (repeat-real-merge-idempotent gwff -1))

(defun merge-idempotent* (gwff) 
       (repeat-real-merge-idempotent gwff 0))

(defun repeat-real-merge-idempotent (gwff srec)
   (declare (special srec))
   (if (< srec 0)
       (real-merge-idempotent gwff nil)
       (let ((hxgwff (real-merge-idempotent gwff t)))
            (if (eq srec 1) (repeat-real-merge-idempotent hxgwff 0) hxgwff))))

(defun real-merge-idempotent (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-merge-idempotent gwff rec)))
        ((lsymbol-q gwff) gwff)   
        ((boundwff-q gwff) (cons (car gwff) (merge-idempotent (cdr gwff))))
        ((not-p gwff) (if (not-p (gdr gwff))
                          (cddr gwff) 
                          (pre-go-on gwff rec #'real-merge-idempotent)))
        ((infix-p gwff) 
         (cond ((equal (cdar gwff) (cdr gwff)) (case (caar gwff)
                                                     (and (go-on (cdr gwff)))
                                                     (or  (go-on (cdr gwff)))
                                                     (implies (go-on 'TRUTH))
						     (equiv (go-on 'TRUTH))))
               ((equal (cons 'not (cdar gwff)) (cdr gwff)) (case (caar gwff)
                                                                 (and (go-on 'FALSEHOOD))
                                                                 (or  (go-on 'TRUTH))
                                                                 (implies (go-on (cdr gwff)))
								 (equiv (go-on 'FALSEHOOD))))
               ((equal (cons 'not (cdr gwff)) (cdar gwff)) (case (caar gwff)
                                                                 (and (go-on 'FALSEHOOD))
                                                                 (or  (go-on 'TRUTH))
                                                                 (implies (go-on (cdr gwff)))
								 (equiv (go-on 'FALSEHOOD))))
               (t (inf-go-on gwff rec #'real-merge-idempotent))))
         (t (complain-unless-gwff gwff rec))))

;;;----------------------------------

(context changing)

(defwffop merge-constant
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Remove constant truth values TRUTH and FALSEHOOD in a wff."))

(context recursively-changing)

(defwffop merge-constant*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively remove truth constants TRUTH and FALSEHOOD in a wff."))

(defun merge-constant (gwff)
       (repeat-real-merge-constant gwff -1))

(defun merge-constant* (gwff)
       (repeat-real-merge-constant gwff 0))
            
(defun real-merge-constant (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-merge-constant gwff rec)))
        ((lsymbol-q gwff) gwff)   
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-merge-constant))
        ((not-p gwff) (cond ((equal(gdr gwff) 'FALSEHOOD) 'TRUTH)
                            ((equal(gdr gwff) 'TRUTH) 'FALSEHOOD)
                            (t (pre-go-on gwff rec #'real-merge-constant))))
        ((infix-p gwff) 
         (case (caar gwff)
           (and (cond ((equal (cdar gwff) 'TRUTH) (go-on (gdr gwff)))     
                      ((equal (cdar gwff) 'FALSEHOOD) (go-on 'FALSEHOOD))
                      ((equal (gdr gwff) 'TRUTH) (go-on  (cdar gwff)))
                      ((equal (gdr gwff) 'FALSEHOOD) (go-on 'FALSEHOOD))
                      (t (inf-go-on gwff rec #'real-merge-constant))))
           (or  (cond ((equal (cdar gwff) 'FALSEHOOD) (go-on (gdr gwff)))      
                      ((equal (cdar gwff) 'TRUTH) (go-on 'TRUTH))
                      ((equal (gdr gwff) 'FALSEHOOD) (go-on (cdar gwff)))
                      ((equal (gdr gwff) 'TRUTH) (go-on  'TRUTH))
                      (t (inf-go-on gwff rec #'real-merge-constant))))
           (implies (cond ((equal (cdar gwff) 'FALSEHOOD) (go-on 'TRUTH))
                          ((equal (cdar gwff) 'TRUTH) (go-on (gdr gwff)))
                          ((equal (gdr gwff) 'TRUTH) (go-on 'TRUTH))
                          ((equal (gdr gwff) 'FALSEHOOD) (go-on (cons 'not (cdar gwff))))
                          (t (inf-go-on gwff rec #'real-merge-constant))))
           (equiv (cond ((equal (cdar gwff) 'FALSEHOOD) (go-on (cons 'not (cdr gwff))))
			((equal (cdar gwff) 'TRUTH) (go-on (gdr gwff)))
			((equal (gdr gwff) 'TRUTH) (go-on (gdr gwff)))
			((equal (gdr gwff) 'FALSEHOOD) (go-on (cons 'not (cdar gwff))))
			(t (inf-go-on gwff rec #'real-merge-constant))))
	   (t (complain-unless-gwff gwff rec))))
         (t (complain-unless-gwff gwff rec))))

(defun repeat-real-merge-constant (gwff srec)
   (declare (special srec))
   (if (< srec 0) (real-merge-constant gwff nil)
       (let ((hxgwff (real-merge-constant gwff t)))
            (if (eq srec 1) (repeat-real-merge-constant hxgwff 0) hxgwff))))

 


;;;
;;;---------------operation to apply distribute laws---------------
;;;
 
(context changing)

(defwffop wff-absorb
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply absorption laws to a formula."))

(context recursively-changing)

(defwffop wff-absorb*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply absorption laws to a formula."))

(defun wff-absorb (gwff)
       (repeat-real-wff-absorb gwff -1))

(defun wff-absorb* (gwff)
       (repeat-real-wff-absorb gwff 0))

(defun repeat-real-wff-absorb (gwff srec)
       (declare (special srec)) 
       (if (< srec 0) 
           (real-wff-absorb gwff nil)
           (let ((hxgwff (real-wff-absorb gwff t)))
                (if (eq srec 1) (repeat-real-wff-absorb hxgwff 0) hxgwff))))

(defun real-wff-absorb (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-absorb gwff rec)))
        ((lsymbol-q gwff) gwff) 
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-absorb))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-absorb))
        ((infix-p gwff)
         (case (caar gwff) 
               (implies (inf-go-on gwff rec #'real-wff-absorb))
               (and (cond ((or-p (cdar gwff)) (if (or (equal (cdadar gwff) (cdr gwff)) (equal (cddar gwff) (cdr gwff)))
                                                  (go-on (cdr gwff))
                                                  (cond ((or-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                                               (go-on (cdar gwff)) 
                                                                               (inf-go-on gwff rec #'real-wff-absorb)))
                                                        ((and-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                                                (go-on (cdr gwff))            
                                                                                (inf-go-on gwff rec #'real-wff-absorb)))
                                                       (t (inf-go-on gwff rec #'real-wff-absorb)))))
                          ((and-p (cdar gwff)) (if (or (equal (cdadar gwff) (cdr gwff)) (equal (cddar gwff) (cdr gwff)))
                                                   (go-on (cdar gwff))
                                                   (cond ((or-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                                            (go-on (cdar gwff)) 
                                                                            (inf-go-on gwff rec #'real-wff-absorb)))
                                                       ((and-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                                            (go-on (cdr gwff))           
                                                                            (inf-go-on gwff rec #'real-wff-absorb)))
                                                       (t (inf-go-on gwff rec #'real-wff-absorb)))))
                          ((or-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                 (go-on (cdar gwff))
                                                 (inf-go-on gwff rec #'real-wff-absorb)))
                          ((and-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                 (go-on (cdr gwff))           
                                                 (inf-go-on gwff rec #'real-wff-absorb)))
                          (t (pre-go-on gwff rec #'real-wff-absorb))))
                 (or (cond ((or-p (cdar gwff)) (if (or (equal (cdadar gwff) (cdr gwff)) (equal (cddar gwff) (cdr gwff)))
                                                 (go-on (cdar gwff))
                                                 (cond ((or-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                                           (go-on (cdr gwff)) 
                                                                           (inf-go-on gwff rec #'real-wff-absorb)))
                                                       ((and-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                                           (go-on (cdar gwff))            
                                                                           (inf-go-on gwff rec #'real-wff-absorb)))
                                                       (t (inf-go-on gwff rec #'real-wff-absorb)))))
                          ((and-p (cdar gwff)) (if (or (equal (cdadar gwff) (cdr gwff)) (equal (cddar gwff) (cdr gwff)))
                                                   (go-on (cdr gwff))           
                                                   (cond ((or-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                                                (go-on (cdr gwff)) 
                                                                                (inf-go-on gwff rec #'real-wff-absorb)))
                                                         ((and-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                                                 (go-on (cdar gwff))            
                                                                                 (inf-go-on gwff rec #'real-wff-absorb)))
                                                         (t (inf-go-on gwff rec #'real-wff-absorb)))))
                          ((or-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                 (go-on (cdr gwff))
                                                 (inf-go-on gwff rec #'real-wff-absorb)))
                          ((and-p (cdr gwff)) (if (or (equal (cdar gwff) (cdadr gwff)) (equal (cdar gwff) (cddr gwff)))
                                                  (go-on (cdar gwff))            
                                                  (inf-go-on gwff rec #'real-wff-absorb)))
                          (t (inf-go-on gwff rec #'real-wff-absorb))))
                   (t (complain-unless-gwff gwff rec))))        
        (t (complain-unless-gwff gwff rec))))

;;;
;;;
;;;operation to apply distribute laws
;;;
 
(context changing)

(defwffop wff-dist-expand
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply distributivity laws to a formula in the expanding direction:
   A and (B or C) --> (A and B) or (A and C)
   A or (B and C) --> (A or B) and (A or C)
   (B or C) and A --> (B and A) or (C and A)
   (B and C) or A --> (B or A) and (C or A)."))

(context recursively-changing)

(defwffop wff-dist-expand*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively apply distributivity laws to a formula in 
the expanding direction:
   A and (B or C) --> (A and B) or (A and C)
   A or (B and C) --> (A or B) and (A or C)
   (B or C) and A --> (B and A) or (C and A)
   (B and C) or A --> (B or A) and (C or A)."))

(context changing)

(defwffop wff-dist-contract
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply distributivity laws to a formula in the contracting direction:
   (A and B) or (A and C) --> A and (B or C)
   (A or B) and (A or C) --> A or (B and C)
   (B and A) or (C and A) --> (B or C) and A 
   (B or A) and (C or A) --> (B and C) or A."))

(context recursively-changing)

(defwffop wff-dist-contract*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively apply distributivity laws to a formula in 
the contracting direction:
   (A and B) or (A and C) --> A and (B or C)
   (A or B) and (A or C) --> A or (B and C)
   (B and A) or (C and A) --> (B or C) and A 
   (B or A) and (C or A) --> (B and C) or A."))

(defun wff-dist-expand (gwff)
       (real-wff-dist-expand gwff nil))

(defun wff-dist-expand* (gwff)
       (real-wff-dist-expand gwff t))

(defun real-wff-dist-expand (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-dist-expand gwff rec)))
        ((lsymbol-q gwff) gwff) 
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-dist-expand))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-dist-expand))
        ((infix-p gwff)
         (case (caar gwff)
               (implies (inf-go-on gwff rec #'real-wff-dist-expand))
               (and (cond ((or-p (cdr gwff)) (acons 'or (acons 'and (cdar gwff) (cdadr gwff)) (acons 'and (cdar gwff) (cddr gwff)))) 
                          ((or-p (cdar gwff)) (acons 'or (acons 'and (cdadar gwff) (cdr gwff)) (acons 'and (cddar gwff) (cdr gwff))))
                          (t (inf-go-on gwff rec #'real-wff-dist-expand))))
               (or  (cond ((and-p (cdr gwff)) (acons 'and (acons 'or (cdar gwff) (cdadr gwff)) (acons 'or (cdar gwff) (cddr gwff)))) 
                          ((and-p (cdar gwff)) (acons 'and (acons 'or (cdadar gwff) (cdr gwff)) (acons 'or (cddar gwff) (cdr gwff))))
                          (t (inf-go-on gwff rec #'real-wff-dist-expand))))))
        (t (complain-unless-gwff gwff rec))))

(defun wff-dist-contract (gwff)
       (real-wff-dist-contract gwff nil))

(defun wff-dist-contract* (gwff)
       (real-wff-dist-contract gwff t))

(defun real-wff-dist-contract (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-dist-contract gwff rec)))
        ((lsymbol-q gwff) gwff) 
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-dist-contract))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-dist-contract))
        ((infix-p gwff)
         (case (caar gwff)
               (implies (inf-go-on gwff rec #'real-wff-dist-contract))
               (and (cond ((and (or-p (cdr gwff)) (or-p (cdar gwff)))
                            (cond ((equal (cdadar gwff) (cdadr gwff)) (acons 'or (cdadr gwff) (acons 'and (cddar gwff) (cddr gwff))))
                                  ((equal (cddar gwff) (cdadr gwff)) (acons 'or (cddar gwff) (acons 'and (cdadar gwff) (cddr gwff))))
                                  ((equal (cdadar gwff) (cddr gwff)) (acons 'or (cddr gwff) (acons 'and (cddar gwff) (cdadr gwff))))
                                  ((equal (cddar gwff) (cddr gwff)) (acons 'or (cddr gwff) (acons 'and (cdadar gwff) (cdadr gwff))))
                                  (t (inf-go-on gwff rec #'real-wff-dist-contract))))
                            (t (inf-go-on gwff rec #'real-wff-dist-contract))))
               (or (cond ((and (and-p (cdr gwff)) (and-p (cdar gwff)))
                               (cond ((equal (cdadar gwff) (cdadr gwff)) (acons 'and (cdadr gwff) (acons 'or (cddar gwff) (cddr gwff))))
                                     ((equal (cddar gwff) (cdadr gwff)) (acons 'and (cddar gwff) (acons 'or (cdadar gwff) (cddr gwff))))
                                     ((equal (cdadar gwff) (cddr gwff)) (acons 'and (cddr gwff) (acons 'or (cddar gwff) (cdadr gwff))))
                                     ((equal (cddar gwff) (cddr gwff)) (acons 'and (cddr gwff) (acons 'or (cdadar gwff) (cdadr gwff))))
                                     (t (inf-go-on gwff rec #'real-wff-dist-contract))))
                           (t (inf-go-on gwff rec #'real-wff-dist-contract))))
               (t (inf-go-on gwff rec #'real-wff-dist-contract))))
         (t (complain-unless-gwff gwff rec))))


;;;
;;;
;;;operation for ASSOCIATIVE LAWS
;;;

(context changing)

(defwffop wff-associative-l
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply the left associative law to a formula:
   A op (B op C) --> (A op B) op C."))

(context recursively-changing)

(defwffop wff-associative-l*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively apply the left associative law to a formula:
   A op (B op C) --> (A op B) op C."))

(context changing)

(defwffop wff-associative-r
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply the right associative law to a formula:
   (A op B) op C --> A op (B op C)."))

(context recursively-changing)

(defwffop wff-associative-r*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively apply the right associative law to a formula:
   (A op B) op C --> A op (B op C)."))

(defun wff-associative-l (gwff)
    (real-wff-associative-l gwff nil))

(defwffop assoc-l
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively apply the left associative law to a formula.
Used in the rule ASSOC."))

(defun assoc-l (gwff)
  (let ((wff (wff-associative-l* gwff)))
    (if (wffeq wff gwff) wff (assoc-l wff))))

(defun wff-associative-l* (gwff)
       (real-wff-associative-l gwff t))

(defun real-wff-associative-l (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-associative-l gwff rec)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-associative-l))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-associative-l))
        ((infix-p gwff)
         (let ((hxinf (caar gwff)))
              (if (and (infix-p (cdr gwff)) (equal hxinf (caadr gwff)))
                  (acons hxinf (acons hxinf (cdar gwff) (cdadr gwff)) (cddr gwff))
                  (inf-go-on gwff rec #'real-wff-associative-l))))
        (t (complain-unless-gwff gwff rec))))

(defun wff-associative-r-rewfn (gwff &rest args) ; mkaminski 11/13/2005
  (declare (ignore args))
  (wff-associative-r gwff))

(defun wff-associative-r (gwff)
    (real-wff-associative-r gwff nil))

(defun wff-associative-r*-rewfn (gwff &rest args) ; mkaminski 11/13/2005
  (declare (ignore args))
  (wff-associative-r* gwff))

(defun wff-associative-r* (gwff)
       (real-wff-associative-r gwff t))

(defun real-wff-associative-r (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-associative-r gwff rec)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-associative-r))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-associative-r))
        ((infix-p gwff)
         (let ((hxinf (caar gwff)))
              (if (and (infix-p (cdar gwff)) (equal hxinf (caadar gwff)))
		  (acons hxinf  (cdadar gwff) (acons hxinf (cddar gwff) (cdr gwff)))
                  (inf-go-on gwff rec #'real-wff-associative-l))))
        (t (complain-unless-gwff gwff rec))))

;;;
;;;
;;;operation to apply commutative law 
;;;

(context changing)

(defwffop wff-commutative
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply commutativity laws to a formula:
   A and B --> B and A
   A or B --> B or A
   A implies B --> not B implies not A
   A equiv B --> B equiv A."))

(context recursively-changing)

(defwffop wff-commutative*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively apply commutativity laws to a formula:
   A and B --> B and A
   A or B --> B or A
   A implies B --> not B implies not A
   A equiv B --> B equiv A."))

(defun wff-commutative (gwff)
       (real-wff-commutative gwff nil))

(defun wff-commutative* (gwff)
       (real-wff-commutative gwff t))

(defun real-wff-commutative (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-commutative gwff rec)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-commutative))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-commutative))
        ((infix-p gwff)
          (case (caar gwff)
            (and (acons 'and (cdr gwff) (cdar gwff)))
            (or (acons 'or (cdr gwff) (cdar gwff)))
            (equiv (acons 'equiv (cdr gwff) (cdar gwff)))
            (implies (acons 'implies 
                            (if (not-p (cdr gwff)) (cddr gwff) (cons 'not (cdr gwff)))
                            (if (not-p (cdar gwff)) (cddar gwff) (cons 'not (cdar gwff)))))         
            (t (complain-unless-gwff gwff rec))))
        (t (complain-unless-gwff gwff rec))))

(context changing)

(defwffop wff-permute
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Permute the two components of an infix operator:
   A op B --> B op A."))

(context recursively-changing)

(defwffop wff-permute*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively permute the two components of an infix operator:
   A op B --> B op A"))

(defun wff-permute (gwff)
       (real-wff-permute gwff nil))

(defun wff-permute* (gwff)
       (real-wff-permute gwff t))

(defun real-wff-permute (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-permute gwff rec)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-permute))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-permute))
        ((infix-p gwff)
          (acons (caar gwff) (cdr gwff) (cdar gwff)))
        (t (complain-unless-gwff gwff rec))))

(context changing)

(defwffop wff-double-negation
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Remove a double negation:
   not not A --> A."))

(context recursively-changing)

(defwffop wff-double-negation*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively remove double negations:
   not not A --> A."))

(defun wff-double-negation (gwff)
       (real-wff-double-negation gwff nil))

(defun wff-double-negation* (gwff)
       (real-wff-double-negation gwff t))

(defun real-wff-double-negation (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-double-negation gwff rec)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-double-negation))
        ((infix-p gwff) (inf-go-on gwff rec #'real-wff-double-negation))
        ((not-p gwff)
            (if (eq (cadr gwff) 'not) 
		(cddr gwff)
                (pre-go-on gwff rec #'real-wff-double-negation)))
        (t (complain-unless-gwff gwff rec))))

(context changing)

(defwffop wff-sub-implies
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply the following law to a formula:
   A implies B --> not A or B."))

(context recursively-changing)

(defwffop wff-sub-implies*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively apply the following law to a formula:
   A implies B --> not A or B."))

(defun wff-sub-implies (gwff)
       (repeat-real-wff-sub-implies gwff -1))

(defun wff-sub-implies* (gwff)
       (repeat-real-wff-sub-implies gwff 0))

(defun repeat-real-wff-sub-implies (gwff srec)
  (declare (special srec))
  (if (< srec 0) (real-wff-sub-implies gwff nil)
      (let ((hxgwff (real-wff-sub-implies gwff t)))
           (if (eq srec 1) (repeat-real-wff-sub-implies hxgwff 0) hxgwff))))

(defun real-wff-sub-implies (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-sub-implies gwff rec)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-sub-implies))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-sub-implies))
        ((infix-p gwff)
          (case (caar gwff)
            ((and or) (inf-go-on gwff rec #'real-wff-sub-implies))
            (implies (go-on (acons 'or (cons 'not (cdar gwff)) (cdr gwff))))
            (t (complain-unless-gwff gwff rec))))            
        (t (complain-unless-gwff gwff rec))))

;;;----------------------------
;;;use 'and' and 'implies' to replace 'eqiuiv'

(context changing)

(defwffop wff-sub-equiv
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Apply following law to a formula:
   A equiv B --> (A implies B) and (B implies A)."))

(context recursively-changing)

(defwffop wff-sub-equiv*
  (argtypes gwff)
  (wffargtypes "O")
  (resulttype gwff)
  (wffop-type "O")
  (wffop-typelist)
  (argnames gwff)
  (arghelp "Wff")
  (mhelp "Recursively apply the following law to a formula:
   A equiv B --> (A implies B) and (B implies A)."))
  
(defun wff-sub-equiv (gwff)
       (repeat-real-wff-sub-equiv gwff -1))

(defun wff-sub-equiv* (gwff)
       (repeat-real-wff-sub-equiv gwff 0))

(defun repeat-real-wff-sub-equiv (gwff srec)
  (declare (special srec))
  (if (< srec 0) (real-wff-sub-equiv gwff nil)
      (let ((hxgwff (real-wff-sub-equiv gwff t)))
           (if (eq srec 1) (repeat-real-wff-sub-equiv hxgwff 0) hxgwff))))

(defun real-wff-sub-equiv (gwff rec)
  (cond ((label-q gwff) (apply-label gwff (real-wff-sub-equiv gwff rec)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff) (pre-go-on gwff rec #'real-wff-sub-equiv))
        ((not-p gwff) (pre-go-on gwff rec #'real-wff-sub-equiv))
        ((infix-p gwff)
          (case (caar gwff)
            ((and or implies) (inf-go-on gwff rec #'real-wff-sub-equiv))
            (equiv (go-on (acons 'and (acons 'implies (cdar gwff) (cdr gwff)) 
				 (acons 'implies (cdr gwff) (cdar gwff)))))
            (t (complain-unless-gwff gwff rec))))            
        (t (complain-unless-gwff gwff rec))))


;;;
;;;
;;; Change the top connective of a formula.
;;;

(context changing)

(defwffop change-top
  (argtypes symbol gwff)
  (resulttype gwff)
  (argnames conn gwff)
  (arghelp "Connective or Quantifier" "Wff")
  (mhelp "Change the top connective of a formula. For example,
\"cntop or\" will change \"A and B\" into \"A or B\";
\"cntop exists\" will change \"forall x P x\" into \"exists x P x\"."))

(defun change-top (connective gwff)
  (cond ((label-q gwff) (apply-label gwff (change-top connective gwff)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff) (if (or (equal (get connective 'typelist) (get (cdar gwff) 'typelist))) 
                               (acons (caar gwff) connective (cdr gwff))
                               (prog2 (terpri) (throwfail "Cannot substitute " (connective . gwff) 
                                " for the binder " ((cdar gwff) . gwff) " of " (gwff . gwff) "."))))
        ((infix-p gwff) (if (equal (get connective 'type) (get (caar gwff) 'type)) 
                            (acons connective (cdar gwff) (cdr gwff))
                            (prog2 (terpri) (throwfail t "Cannot substitute " (connective . gwff) 
                             " for the connective " ((caar gwff) . gwff) " in " (gwff . gwff) "," t "because it has the wrong type."))))
        (t (prog2 (terpri) (throwfail "Please use 'sub' command to change the formula.")))))


(defwffop delete-topconn-rscope
  (argtypes gwff)
  (resulttype  gwff)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp  "Delete the topmost binary connective and its right scope"))

(defun delete-topconn-rscope (gwff)
  (cond ((label-q gwff) (apply-label gwff (delete-topconn-rscope gwff)))
        ((lsymbol-q gwff) gwff)
	((boundwff-q gwff)  (prog2 (terpri) (throwfail t "Cannot perform operation- no top connective")))
	((infix-p gwff) (cdar gwff))))

(defwffop delete-topconn-lscope
  (argtypes gwff)
  (resulttype  gwff)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp  "Delete the topmost binary connective and its left scope"))

(defun delete-topconn-lscope (gwff)
  (cond ((label-q gwff) (apply-label gwff (delete-topconn-lscope gwff)))
        ((lsymbol-q gwff) gwff)
	((boundwff-q gwff)  (prog2 (terpri) (throwfail t "Cannot perform operation- no top connective")))
	((infix-p gwff) (cdr gwff))))
