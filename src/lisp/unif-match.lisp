;;; -*- Mode: Lisp -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile unif-match
  (part-of unification)
  (extension lisp)
  (mhelp "Unification functions."))

(defun listify-type (type)
  (do ((typelist type (car typelist))
       (counter 1 (1+ counter)))
      ((atom typelist)
       (let ((type-vector (make-array counter)))
	 (declare (simple-vector type-vector))
	 (decf counter)
	 (dotimes (i counter
		     (progn
		      (setf (svref type-vector counter) type)
		      type-vector))
	   (setf (svref type-vector i) (cdr type))
	   (setq type (car type)))))
    (declare (fixnum counter))))

(defun prefix-lambda (term vector k)
  (do ((k (1- k) (1- k)))
      ((minusp k) term)
    (setq term (acons (aref vector k) 'lambda term))))

(defun construct-type (type vector k)
  (do ((k (1- k) (1- k)))
      ((minusp k) type)
    (setq type (cons type (aref vector k)))))

(defun construct-term (term vector k)
  (dotimes (i k term)
    (setq term (cons term (aref vector i)))))

(defun construct-E-i (term h-vars type w-vars type-fhead k)
  (let* ((type (construct-type type type-fhead k))
	 (head (ren-var-uni-ho h-var-prefix type)))
    (values (cons term (construct-term head w-vars k))
	    (cons head h-vars))))

(defun construct-E-j (term h-vars type w-vars type-fhead k 
			    v-vars v-vars-typevector k1)
  (let* ((type (construct-type 
		(construct-type type v-vars-typevector k1)
		type-fhead k))
	 (head (ren-var-uni-ho h-var-prefix type)))
    (values (cons term (construct-term 
			(construct-term head w-vars k)
			v-vars k1))
	    (cons head h-vars))))


(defun construct-projection-1 (head w-i-typevector m w-vars type-fhead k j)
  (let ((h-vars nil)
	(term head))
    (dotimes (i m (make-subst :term (prefix-lambda term w-vars k)
			      :h-vars (nreverse h-vars) :type j))
      (multiple-value-setq (term h-vars)
			   (construct-E-i term h-vars (aref w-i-typevector i)
					  w-vars type-fhead k)))))

(defun construct-projection-2 (head w-i-typevector m w-vars type-fhead p1
				    v-vars v-vars-typevector k j)
  (let ((h-vars nil)
	(term head))
    (dotimes (i m
		(make-subst
		 :term (prefix-lambda (prefix-lambda term v-vars k)
				      w-vars p1) 
		 :h-vars (nreverse h-vars) :type j))
      (multiple-value-setq (term h-vars)
			   (construct-E-j term h-vars (aref w-i-typevector i)
					  w-vars type-fhead p1 v-vars 
					  v-vars-typevector k)))))

(defun projections (w-vars type-fhead v-vars v-vars-typevector p1 n)
  (let ((projections nil)
	(projections1-flag
	 (not (or (zerop n) ;;(= p1 q1)
		  (= p1 (1- (length type-fhead))))))
	(k (1- (length type-fhead))))
    (dotimes (i p1 (nreverse projections))
      (let ((w-i-typevector (listify-type (aref type-fhead i))))
	(do ((m (1- (length w-i-typevector)) (1- m))
	     (k k (1- k))
	     (flag nil t))
	    ((or (minusp m) (minusp k)
		 (not (equal (aref w-i-typevector m) (aref type-fhead k))))
	     (when flag
	       (incf m)
	       (incf k)
	       (let ((w-i (aref w-vars i)))
		 (do ((m1 m (1+ m1))
		      (k k (1+ k)))
		     ((> k p1) (setq m m1))  ;;ck this
		   (push (construct-projection-1
			  w-i w-i-typevector m1 w-vars type-fhead k i)
			 projections))
		 (when projections1-flag
		   (do ((m m (1+ m))
			(k 1 (1+ k)))
		       ((> k n))
		     (push (construct-projection-2
			    w-i w-i-typevector m w-vars type-fhead p1 v-vars
			    v-vars-typevector k i)
			   projections)))))))))))

(defun projections-eta (w-vars type-fhead p1 &optional (fhead nil))
  (declare (special imitation-h-var-list))
  (if (and fhead (memq fhead imitation-h-var-list)) nil
      (let ((projections nil)
            (resultype (aref type-fhead (1- (length type-fhead)))))
        (dotimes (i p1 projections)
          (let ((type-w-i (listify-type (aref type-fhead i))))
            (if (eq (aref type-w-i (1- (length type-w-i))) resultype)
                (push (construct-projection-1
                       (aref w-vars i) type-w-i (1- (length type-w-i)) w-vars
                       type-fhead p1 i)
                      projections)))))))

(defun find-min-legit-k (k type-fhead type-rhead p1 p2)
  (do ((j (1- p1) (1- j))
       (p2-p1+j (1- p2) (1- p2-p1+j)))
      ((or (< j k)
	   (not (equal (aref type-fhead j) (aref type-rhead p2-p1+j)))) 
       ;;(if (< j k) k j)
       (1+ j))))

;*;In the functions CONSTRUCT-IMITATION-TERM if SUBST (head -> rhead) is a
;*; CONS, then the substitution SUBST must be applied. If we don't want this
;*;susbtitution (eg, when finding primitive substitutions which contain
;*;logical constants), SUBST will be NIL.

(defun construct-imitation-term (subst head w-vars type-fhead type-rhead k
				       p2-p1+k)
  (let ((h-vars nil)
	(term head))
    (dotimes (counter p2-p1+k (make-subst
			       :term (prefix-lambda term w-vars k)
			       :h-vars (nreverse h-vars) :type subst))
      (multiple-value-setq (term h-vars)
			   (construct-E-i
			    term h-vars (aref type-rhead counter) w-vars
			    type-fhead k)))))

(defun construct-imitation-term-1 (subst head w-vars v-vars type-fhead
					 type-rhead v-vars-typevector p1 p2 n)
  (let ((term head)
	(h-vars nil))
    (dotimes (i p2
		(make-subst
		 :term (prefix-lambda (prefix-lambda term v-vars n) w-vars p1)
		 :h-vars (nreverse h-vars) :type subst))
      (multiple-value-setq (term h-vars)
			   (construct-E-j
			    term h-vars (aref type-rhead i) w-vars type-fhead
			    p1 v-vars v-vars-typevector n)))))

(defun construct-sk-imitation-term (subst head w-vars type-fhead type-rhead k p2-p1+k nan)
  (let ((h-vars nil)
	(term head))
    (dotimes (counter p2-p1+k (make-subst :term (prefix-lambda term w-vars k)
			                          :h-vars (nreverse h-vars) :type subst))
      (if (< counter nan)
          (let ((h-var (ren-var-uni-ho h-var-prefix (aref type-rhead counter))))
	       (push h-var h-vars)
	       (setq term (cons term h-var)))
          (multiple-value-setq (term h-vars)
               (construct-E-i term h-vars (aref type-rhead counter) w-vars type-fhead k))))))

;the following fn appears twice in this file; eliminating the first one. MB Thu Jun  1 12:41:16 1995
#+comment(defun imitation-eta (rhead w-vars type-fhead type-rhead p1 rterm-binder 
                      &optional (fhead nil))
  (declare (ignore fhead))
  (unless (memq rhead rterm-binder)
    (let ((head (ren-var-uni-ho h-var-prefix (type rhead)))
          (nan (sk-const-p rhead)))
      (if nan
	  (list (construct-sk-imitation-term
                 (cons head rhead) head w-vars type-fhead type-rhead p1
                 (1- (length type-rhead)) nan))
          (if (eq rhead 'not)
              (list (construct-imitation-term (cons head rhead) head w-vars
                                              type-fhead type-rhead p1
                                              (1- (length type-rhead)))
                    (construct-dneg-imitation-term w-vars type-fhead p1))
              (list (construct-imitation-term
                     (cons head rhead) head w-vars type-fhead type-rhead p1
                     (1- (length type-rhead)))))))))

(defun imitation (rhead w-vars v-vars type-fhead type-rhead 
			v-vars-typevector p1 p2 n n1 rterm-binder)
  (if (memq rhead rterm-binder)
      (when (> (length (memq rhead rterm-binder)) n1)
	(list (construct-imitation-term-1
		nil rhead w-vars v-vars type-fhead type-rhead
		v-vars-typevector p1 p2 n)))
      (let* ((head (ren-var-uni-ho h-var-prefix (type rhead)))
	     (subst (cons head rhead))
             (nan (sk-const-p rhead)))
	(if nan
	    (if (zerop n)
		(let ((k (find-min-legit-k (max 0 (- p1 p2)) type-fhead
					   type-rhead p1 p2)))
		  (do ((k k (1+ k))
		       (p2-p1+k (+ (- p2 p1) k) (1+ p2-p1+k))
		       (substitutions nil
				      (cons (construct-sk-imitation-term
					      subst head w-vars type-fhead
					      type-rhead k p2-p1+k nan)
					    substitutions)))
		      ((> k p1) (nreverse substitutions))))
		(let ((subst (construct-sk-imitation-term
			       subst head w-vars type-fhead type-rhead 0 p2 nan)))
		  (setf (subst-term subst)
			(prefix-lambda (prefix-lambda (subst-term subst)
						      v-vars n)
				       w-vars p1))
		  (list subst)))
	    (if (zerop n)
		(let ((k (find-min-legit-k (max 0 (- p1 p2)) type-fhead
					   type-rhead p1 p2)))
		  (do ((k k (1+ k))
		       (p2-p1+k (+ (- p2 p1) k) (1+ p2-p1+k))
		       (substitutions nil
				      (cons (construct-imitation-term
					      subst head w-vars type-fhead
					      type-rhead k p2-p1+k)
					    substitutions)))
		      ((> k p1) (nreverse substitutions))))
		(list (construct-imitation-term-1
			subst head w-vars v-vars type-fhead type-rhead
			v-vars-typevector p1 p2 n)))))))

(defun construct-dneg-imitation-term (w-vars type-fhead k rhead)
  (declare (special imitation-h-var-list))
  (let* ((type (construct-type 'O type-fhead k))
         (head (ren-var-uni-ho h-var-prefix type))
         (term (construct-term head w-vars k)))
    (push head imitation-h-var-list)
    (push (cons head rhead) non-dneggable-vars)
    (make-subst :term (prefix-lambda term w-vars k) :h-vars (list head)
                :type nil)))

;;;The old version of imitation-eta below had the effect of not letting
;;;flexible-flexible pairs get mated in both directions. Delete the old
;;;version after the new code has been tested.

#+comment(defun imitation-eta (rhead w-vars type-fhead type-rhead p1 rterm-binder)
  (unless (memq rhead rterm-binder)
    (let ((head (ren-var-uni h-var-prefix (type rhead))))
      (if (sk-const-p rhead)
	  (list (construct-sk-imitation-term
		  (cons head rhead) head w-vars type-rhead p1
		  (1- (length type-rhead))))
	  (list (construct-imitation-term (cons head rhead) head w-vars
					  type-fhead type-rhead p1
					  (1- (length type-rhead))))))))

;;; I think handling of skolem constants is not right, that is, the
;;; substitution generated is incorrect. eliminating this test for the
;;; time being. -si 11 dec 91.

(defun imitation-eta (rhead w-vars type-fhead type-rhead p1 rterm-binder 
                      &optional (fhead nil) (bdargs nil))
;  (declare (ignore fhead))
  (unless (memq rhead rterm-binder)
    (let ((head (ren-var-uni-ho h-var-prefix (type rhead)))
          (nan (sk-const-p rhead)))
      (if nan
	  (list (construct-sk-imitation-term
                 (cons head rhead) head w-vars type-fhead type-rhead p1
                 (1- (length type-rhead)) nan))
          (if (and (eq rhead 'not) (or (not reduce-double-neg)
				       (not (dneg-refuse-test bdargs fhead)))) 
					;bdargs is head of rterm if arg is rigid.
		     (let ((imit (construct-imitation-term (cons head rhead) head w-vars
						     type-fhead type-rhead p1
						     (1- (length type-rhead)))))
		       (if (memq (lnorm (subst-term imit)) (subst-h-vars imit))
			   (list (construct-dneg-imitation-term w-vars type-fhead p1 (cdr bdargs)))
			 (list imit (construct-dneg-imitation-term w-vars type-fhead p1 (cdr bdargs)))))
	    (let ((imit (construct-imitation-term
			 (cons head rhead) head w-vars type-fhead type-rhead p1
			 (1- (length type-rhead)))))
	      (if (memq (lnorm (subst-term imit)) (subst-h-vars imit))
		  nil (list imit))))))))

(defflag unif-counter
  (flagtype integer+)
  (default 0)
  (subjects unification transmit)
  (mhelp "If this flag is non-zero, PP* will be called to 
print out information about the current unification tree at 
regular intervals. This flag determines the length of
the intervals, measured by the number of calls to the
unification procedure. The amount of information is
determined by the setting of UNIF-COUNTER-OUTPUT.
If the flag is set to 0, this feature will be turned off.
This flag only applies in UN88 unification."))

(defflag unif-counter-output
  (flagtype integer+)
  (default 0)
  (subjects unification transmit)
  (mhelp "See UNIF-COUNTER and UNIF-TRIGGER for the use of this flag.
Settings are:
0: Print the entire tree in flat format with details. (PALL)
1: Print the entire tree in flat format without details. (PALL)
2: Print the tree in tree format with subs. (UTREE*)
3: Print the tree in tree format without subs. (UTREE*)
4: Print just the subs and details in flat format. (UTREE)
5: Print just the subs in flat format. (UTREE)
6: Print full details of the last node. (P and PP*)
7: Print some details of the last node. (P and PP)
8: Print the last node and its properties only.
9: Print the statistics for the tree so far. (STATS)
10: Print the average values for STATS, after a mating is found.
This flag only applies in UN88 unification."))

(defflag unif-trigger
  (flagtype symbol)
  (default NIL)
  (subjects unification transmit)
  (irrelevancy-preconditions
   (max-substs-var (and max-substs-var max-substs-quick))
   (max-substs-quick (and max-substs-var max-substs-quick)))
  (relevancy-preconditions
   (max-substs-var (not (and max-substs-var max-substs-quick)))
   (max-substs-quick (not (and max-substs-var max-substs-quick))))
  (mhelp "If this flag is non-NIL, PP* will be called to 
print out information about the current unification tree 
after certain events (compare UNIF-COUNTER).
Settings are:
NIL: Print nothing.
UTREE-END: Printout whenever a tree has come to an end
  (either failure or success; NB UNIF-COUNTER-OUTPUT 6 or 7
   will not work with this setting.)
UTREE-END1: As UTREE-END, but also gives output when quick
  unification ends a tree without completing it.
UTREE-BEGIN: Printout the root node when unification is
  first called.
PROPS-CHANGE: Printout whenever the properties of a node 
  are different from those of its parent. (Best used with
  UNIF-COUNTER-OUTPUT 6 or 7.) 
The amount of information is determined by the setting of 
UNIF-COUNTER-OUTPUT. If the flag is set to NIL, this 
feature will be turned off.
This flag only applies in UN88 unification."))


(defflag dneg-imitation
  (flagtype symbol)
  (default 'const-flex)
  (subjects unification transmit)
  (mhelp "Determine when to produce imitation terms that contain
double negations. Only applies in UN88 when REDUCE-DOUBLE-NEG is T
(in UN88 otherwise, it is implicitly set to ALWAYS; in UN90 it is
implicitly set to CONST-FLEX).
When TPS mates two flexible literals f and g, it adds (f . ~g) as
a dpair. Because it may really have needed (g . ~f), we allow 
imitation terms to contain double negations even if REDUCE-DOUBLE-NEG
is T. The options are as follows:
ALWAYS always allows double negations to be used.
CONST forbids them for dpairs of the form (f . ~G), where G is a
  constant, but allows them otherwise.
FLEX forbids them for (f . ~g) if g was created by a double negation
  in the first place (this prevents endless cycles), but allows them
  otherwise.
CONST-FLEX forbids them in the two cases for CONST and FLEX, but allows
  them otherwise.
NEVER forbids them outright."))

(definfo always
  (mhelp "A setting for DNEG-IMITATION.
Always allow double negations to be used as imitation terms."))

(definfo const
  (mhelp "A setting for DNEG-IMITATION.
Forbid double negations to be used as imitation terms for dpairs 
of the form (f . ~G), where G is a constant, but allows them otherwise."))

(definfo flex
  (mhelp "A setting for DNEG-IMITATION.
Forbid double negations to be used as imitation terms for dpairs 
of the form (f . ~g) if g was created by a double negation
in the first place (this prevents endless cycles), but allow them
otherwise."))

(definfo const-flex
  (mhelp "A setting for DNEG-IMITATION.
Forbid double negations to be used as imitation terms 
in the two cases CONST and FLEX (see help messages for these
cases), but allow them otherwise."))

(definfo never
  (mhelp "A setting for DNEG-IMITATION.
Forbid double negations to be used as imitation terms, ever."))

(defun dneg-refuse-test (rigid-args fhead)
   (or (eq dneg-imitation 'never) ; don't do dnegs if it's ALWAYS
       (and (car rigid-args) (memq dneg-imitation '(const const-flex)))
					;or if we have a constant and we've banned that.
       (and (not (car rigid-args)) (memq dneg-imitation '(flex const-flex))
	    ;(or (memq (cdr rigid-args) non-dneggable-vars) (memq fhead non-dneggable-vars))) 
	    (check-dneg-vars (cdr rigid-args) fhead non-dneggable-vars))
					;or it's a bad f-f pair and we've banned that
       ))

(defun check-dneg-vars (a b vlist)
  (if (null vlist) nil
    (let ((a1 (caar vlist))
	  (b1 (cdar vlist)))
      (or (and (eq a a1) (eq b b1))
	  (and (eq b a1) (eq a b1))
	  (check-dneg-vars a b (cdr vlist))))))

#+comment(defun imitation-eta (rhead w-vars type-fhead type-rhead p1 rterm-binder
                            &optional (fhead nil))
  (unless (memq rhead rterm-binder)
    (let ((head (ren-var-uni-ho h-var-prefix (type rhead))))
      (if (eq rhead 'not)
          (if (and fhead (memq fhead imitation-h-var-list)) nil
              (list (construct-imitation-term (cons head rhead) head w-vars
                                              type-fhead type-rhead p1
                                              (1- (length type-rhead)))
                    (construct-dneg-imitation-term w-vars type-fhead p1)))
          (list (construct-imitation-term
                 (cons head rhead) head w-vars type-fhead type-rhead p1
                 (1- (length type-rhead))))))))

#+comment(defun create-new-w-vars (w-vars-typevector p1)
  (declare (fixnum p1) (simple-vector w-vars-typevector))
  (let ((w-var-vector (make-array p1)))
    (declare (simple-vector w-var-vector))
    (dotimes (i p1 w-var-vector)
      (setf (svref w-var-vector i)
	    (ren-var-uni-ho w-var-prefix (svref w-vars-typevector i) i)))))
;already in ms90-3-unif-match

(defun unif-match (fhead type-fhead p1 n1 rhead p2 n2 rterm-binder bdvars bdargs)
  (if eta-rule
      (progn (setq p1 (1- (length type-fhead)))
	     (let* ((w-vars (create-new-w-vars type-fhead p1))
		    (imitations (imitation-eta
                                 rhead w-vars type-fhead
                                 (listify-type (type rhead)) p1 rterm-binder
                                 fhead bdargs))
		    (projections (projections-eta
                                  w-vars type-fhead p1 fhead)))
	       (if imitation-first
		   (nconc imitations projections)
		   (nconc projections imitations))))
      (unless (> n1 n2)
	(let ((w-vars (create-new-w-vars type-fhead p1))
	      (n (- n2 n1)))
	  (declare (fixnum n))
	  (let ((v-vars (make-array n))
		(v-vars-typevector (make-array n)))
	    (declare (simple-vector v-vars v-vars-typevector))
	    (dotimes (i n);;ck the order here!
	      (let ((pair (assoc (elt rterm-binder i) bdvars)))
		;;(if (eq (car pair) rhead)
		;;  (setq rhead (cdr pair)))
		(setf (svref v-vars i) (cdr pair))
		(setf (svref v-vars-typevector i) (type (cdr pair)))))
	    (let ((imitations (imitation rhead w-vars v-vars type-fhead
					 (listify-type (type rhead))
					 v-vars-typevector p1 p2 n n1
					 rterm-binder))
		  (projections (projections w-vars type-fhead v-vars
					    v-vars-typevector p1 n)))
	      (if imitation-first
		  (nconc imitations projections)
		  (nconc projections imitations))))))))



