;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-OPS1)

;;;
;;; File WFFSUB1
;;;
;;; substitution operations on wffs.
;;;

(deffile wffsub1
  (part-of wff-ops1)
  (extension clisp)
  (mhelp "Contains substitution commands for wffs without lambda binders."))

(context substitution)

(defwffop substitute-term-var
  (argtypes gwff gvar gwff)
  (wffargtypes "A" "A" "B")
  (resulttype gwff)
  (wffop-type "B")
  (wffop-typelist "A" "B")
  (argnames term var inwff)
  (arghelp "term" "var" "inwff")
  (applicable-p (lambda (term var inwff) (free-for term var inwff)))
  (mhelp
   "Substitute a term for the free occurrences of variable in a gwff."))


(defun substitute-term-var (term var inwff)
  "This function should be used with extreme caution. There's an underlying
  assumption that TERM is free for VAR in INWFF (which is true if TERM is
  a new variable)."
  (or (subst-term-var-rec (intern-subst term var) var inwff)
      inwff))

(defun subst-term-var-rec (term var inwff)
  (cond ((label-q inwff)
	 (apply-label inwff (subst-term-var-rec term var inwff)))
	((lsymbol-q inwff) (if (eq var inwff) term nil))
	((boundwff-q inwff)
	 (if (eq (caar inwff) var) nil
	     (let ((new-wff (subst-term-var-rec term var (cdr inwff))))
	       (if new-wff (cons (car inwff) new-wff) nil))))
	(t (let ((left (or (subst-term-var-rec term var (car inwff))
			   (car inwff)))
		 (right (or (subst-term-var-rec term var (cdr inwff))
			    (cdr inwff))))
	     (unless (and (eq left (car inwff)) (eq right (cdr inwff)))
		     (cons left right))))))

(context wfftst-obj)

(defwffop subst-some-occurrences
  (argtypes gwff gwff gwff gwff)
  (argnames term1 wff1 term2 wff2)
  (arghelp "old term" "old wff" "new term" "new wff")
  (wffargtypes "A" "B" "A" "B")
  (resulttype boolean)
  (wffop-typelist "A" "B")
  (mhelp "Checks to see if wff2 is the result of replacing some
occurrences of term1 in wff1 with term2."))

;;; WFFEQ (i.e. strict) used

(defun subst-some-occurrences (term1 wff1 term2 wff2)
  (s-s-o-rec term1 (free-vars-of term1) wff1  
	     term2 (free-vars-of term2) wff2 nil))

(defwffrec s-s-o-rec
  (argnames t1 fv1 w1 t2 fv2 w2 bv)
  (mhelp "Recursive part of SUBST-SOME-OCCURRENCES."))

(defun s-s-o-rec (t1 fv1 w1 t2 fv2 w2 bv)
  (cond ((label-q w1) (apply-label w1 (s-s-o-rec t1 fv1 w1 t2 fv2 w2 bv)))
	((label-q w2) (apply-label w2 (s-s-o-rec t1 fv1 w1 t2 fv2 w2 bv)))
	((wffeq w1 t1) (or (wffeq w2 t1)
			   (and (not (setintersect fv1 bv))
				(not (setintersect fv2 bv))
				(wffeq w2 t2))))
	((lsymbol-q w1) (wffeq w1 w2))
	((lsymbol-q w2) nil)
	((boundwff-q w1) (and (boundwff-q w2)
			      (equal (binding w1) (binding w2))
			      (equal (bindvar w1) (bindvar w2))
			      (s-s-o-rec t1 fv1 (gdr w1) t2 fv2 (gdr w2)
				    (cons (bindvar w1) bv))))
	(t (and (s-s-o-rec t1 fv1 (gar w1) t2 fv2 (gar w2) bv)
		(s-s-o-rec t1 fv1 (gdr w1) t2 fv2 (gdr w2) bv)))))

;;; To implement Rule R'. In this case, PVS has:
;;;  (intersection (free-vars-of hyps) (free-vars-of "A=B"))

(defwffop subst-occs
  (argtypes gwff gwff gwff gwff gvarlist)
  (argnames term1 wff1 term2 wff2 pvs)
  (arghelp "old term" "old wff" "new term" "new wff"
	   "variables to be free in occurrence")
  (wffargtypes "A" "B" "A" "B" nil)
  (resulttype boolean)
  (wffop-typelist "A" "B")
  (mhelp "Checks to see if wff2 is the result of replacing some
occurrences of term1 in wff1 with term2. The pvs must not be
bound at such occurrences of term1."))

;;; WFFEQ (i.e. strict) used

(defun subst-occs (term1 wff1 term2 wff2 pvs)
  (cond ((label-q wff1) (apply-label wff1
				     (subst-occs term1 wff1 term2 wff2 pvs)))
	((label-q wff2) (apply-label wff2
				     (subst-occs term1 wff1 term2 wff2 pvs)))
	((wffeq wff1 term1) (or (wffeq wff2 term1) (wffeq wff2 term2)))
	((lsymbol-q wff1) (wffeq wff1 wff2))
	((lsymbol-q wff2) nil)
	((boundwff-q wff1)
	 (and (boundwff-q wff2)
	      (equal (binding wff1) (binding wff2))
	      (equal (bindvar wff1) (bindvar wff2))
	      (if (wffmember (bindvar wff1) pvs)
		  (wffeq (gdr wff1) (gdr wff2))
		  (subst-occs term1 (gdr wff1) term2 (gdr wff2) pvs))))
	(t (and (subst-occs term1 (gar wff1) term2 (gar wff2) pvs)
		(subst-occs term1 (gdr wff1) term2 (gdr wff2) pvs)))))

(defwffop r-prime-restr
  (argtypes gwff gwff gwff gwff)
  (argnames term1 wff1 term2 wff2)
  (arghelp "old term" "old wff" "new term" "new wff")
  (wffargtypes "A" "B" "A" "B" nil)
  (resulttype boolean)
  (wffop-typelist "A" "B")
  (mhelp "Verifies that wff2 follows from wff1 by Rule R' using equality term1=term2."))

(defun r-prime-restr (term1 wff1 term2 wff2)
  (subst-occs
   term1 wff1 term2 wff2
   (intersection (union (free-vars-of term1) (free-vars-of term2))
		 (free-in-hyps))))

;;; Following is similar to R-PRIME-RESTR, but the order of the
;;; equality doesn't matter, and wff2 may follow from Wff1 by iterated
;;; uses of Rule R'.  E.g., this will succeed for wff1=P x,
;;; wff2=P[f[fx]], with the equality x = fx.   An upper bound on the
;;; iteration is the length of the formulas wff1 and wff2.  See below.
;;; Added 6MAR91. DAN

(defwffop same-modulo-equality
  (argtypes gwff gwff gwff gwff)
  (argnames wff1 wff2 term1 term2)
  (arghelp "old wff" "new wff" "first term" "equal term")
  (wffargtypes "B" "B" "A" "A" nil)
  (resulttype boolean)
  (wffop-typelist "A" "B")
  (mhelp "Verifies that wff2 follows from wff1 by Rule R' (possibly
iterated) using equality term1=term2."))

(defun same-modulo-equality (wff1 wff2 term1 term2)
  (let* ((len-limit (max (wff-length wff1) (wff-length wff2)))
	 (eq-list (generate-eq-list term1 term2 len-limit)))
    (subst-occs***
     wff1 wff2
     (list eq-list)
     (intersection (union (free-vars-of term1) (free-vars-of term2))
		   (free-in-hyps)))))

;;; We want to see if wff1 and wff2 are equal. PVS is a list of
;;; variables; if one of these variables is bound in wff1 or wff2,
;;; then we can't make any substitution occurrence in its scope.
;;; EQ-LISTS is a list of lists.  Each list in EQ-LISTS is a list of
;;; terms, each of which are equal to each other, and can be
;;; substituted for each other.  So if wff1 and wff2 are both in some
;;; member of EQ-LISTS, then we succeed.

(defun subst-occs*** (wff1 wff2 eq-lists pvs)
  (or (wffeq wff1 wff2)
      (cond
       ((label-q wff1) (apply-label wff1 (subst-occs*** wff1 wff2
							eq-lists
							pvs)))
       ((label-q wff2) (apply-label wff2 (subst-occs*** wff1 wff2
							eq-lists pvs)))
       ((some #'(lambda (x) (and (member wff1 x :test #'wffeq)
				 (member wff2 x :test #'wffeq)))
	      eq-lists))
       ((or (lsymbol-q wff1) (lsymbol-q wff2)) nil)
       ((boundwff-q wff1)
	(and (boundwff-q wff2)
	     (equal (binding wff1) (binding wff2))
	     (equal (bindvar wff1) (bindvar wff2))
	     (if (wffmember (bindvar wff1) pvs)
		 (wffeq (gdr wff1) (gdr wff2))
	       (subst-occs*** (gdr wff1) (gdr wff2) eq-lists pvs))))
       (t (and (subst-occs*** (gar wff1) (gar wff2) eq-lists pvs)
	       (subst-occs*** (gdr wff1) (gdr wff2) eq-lists pvs))))))

(defun wff-length (wff)
  "Return the length (number of symbols) of WFF.  Doesn't include
brackets."
  (cond 
   ((label-q wff) (apply-label wff (wff-length wff)))
   ((lsymbol-q wff) 1)
   ((boundwff-q wff) 
    (+ 2 (wff-length (gdr wff))))
   (t
    (+ (wff-length (gar wff))
       (wff-length (gdr wff))))))

(defun substitute-term-term (term1 term2 wff)
  "Return all possible ways of substituting or not substituting term1
for term2 in wff.  That is, if term1 and term2 are equal, then what
are the wffs equal to wff? Assumes that term1 and term2 are not labels."
  (cond 
   ((label-q wff) (apply-label wff (substitute-term-term term1 term2 wff)))
   ((wffeq wff term2) 
    (list wff term1))
   ((lsymbol-q wff) (list wff))
   ((boundwff-q wff)
    (if (member (caar wff) (free-vars-of term1))
	(list wff) ; can't substitute into scope that would capture a variable
      (let ((sub-gdr (substitute-term-term term1 term2 (gdr wff))))
	(mapcar #'(lambda (x) (cons (car wff) x))
		sub-gdr))))
   (t 
    (let ((sub-car (substitute-term-term term1 term2 (car wff)))
	  (sub-cdr (substitute-term-term term1 term2 (cdr wff))))
      (mapcan #'(lambda (x) 
		  (mapcar #'(lambda (y) (cons x y)) sub-cdr))
	      sub-car)))))
	  
;;; This is the tricky one.  term1 and term2 are equal.  What we want
;;; to do is generate as many terms as we can from them that are also
;;; equal. E.g., if term1 is x and term2 is [f x], we can generate
;;; [f [f x]], [f. f. f x], etc.  If there are no instances of term1
;;; in term2 or vice versa, then we should just end up with (term1 term2).
;;; LEN-LIMIT is the longest term we allow.

(defun generate-eq-list (term1 term2 len-limit)
  (labels ((get-all-conses  (list)
	     (when (cdr list)
	       (nconc (mapcar #'(lambda (x) (cons (car list) x))
			      (cdr list))
		      (get-all-conses (cdr list)))))
	   (apply-equalities (assoc-list wff)
	     (if (null assoc-list) 
		 nil
	       (delete-if 
		#'(lambda (x) (> (wff-length x) len-limit))
		(nconc
		 (cdr (substitute-term-term 
		       (caar assoc-list) (cdar assoc-list) wff))
		 (cdr (substitute-term-term 
		       (cdar assoc-list) (caar assoc-list) wff))
		 (apply-equalities (cdr assoc-list) wff))))))
  (do* ((old-pairs nil (nconc old-pairs new-pairs))
	(total-terms 
	 nil
	 (delete-duplicates (nconc total-terms new-terms)
			    :test #'wffeq))
	(new-terms 
	 (list term1 term2)
	 (set-difference
	  (delete-duplicates
	  (mapcan #'(lambda (x) (apply-equalities old-pairs x))
		  new-terms)
	  :test #'wffeq)
	  total-terms :test #'wffeq))
	(new-pairs 
	 (list (cons term1 term2))
	 (nconc 
	  (mapcan #'(lambda (x) (mapcar #'(lambda (y) (cons x y))
					new-terms))
		  total-terms)
	  (get-all-conses new-terms))))
      ((null new-terms) 
       total-terms))))

; posn ("position") is a list of T's and NIL's where NIL indicates a "car"
; and T indicates a "cdr".  This indicates the position of the subwff.
(defun replace-wff-posn (wff posn newsub)
  (if posn
      (if (car posn)
	  (cons (car wff) (replace-wff-posn (cdr wff) (cdr posn) newsub))
	(cons (replace-wff-posn (car wff) (cdr posn) newsub) (cdr wff)))
    newsub))

(defun get-wff-posn (wff posn)
  (if posn
      (if (car posn)
	  (get-wff-posn (cdr wff) (cdr posn))
	(get-wff-posn (car wff) (cdr posn)))
    wff))
