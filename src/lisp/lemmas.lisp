;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

; cebrown 10/9/01

(part-of etr-nat)
(context mating-search)

(deffile lemmas
  (part-of etr-nat)
  (extension lisp)
  (mhelp "Functions for dealing with lemmas."))

(defflag ASSERT-LEMMAS
  (flagtype boolean)
  (default T)
  (subjects etr-nat transmit)
  (mhelp "If this is set to T, Lemmas are justified in the natural deduction 
proofs using an Assert.  The Assert gives the name of the proof of the Lemma.

Lemmas may be introduced in the following circumstances:

. when extensionality is used (USE-EXT-LEMMAS must be set to T)
. when set variables are solved instantiated using constraints
  (DELAY-SETVARS must be set to T)

If lemmas L1, . . ., Ln are used to prove A, then the full proof
consists of proofs of each of the Li and a proof of A using the
lemmas Li.  In other words, it is a proof of

[L1 and . . . and Ln] and [[L1 and . . . and Ln] implies A]"))


(defflag USE-EXT-LEMMAS
  (flagtype boolean)
  (default nil)
  (subjects mating-search transmit)
  (mhelp "If this is set to T, then diy finds all positive and
negative literals which have a proper subterm of propositional,
set, or relation types.  For example, the jform may have a positive
literal P X(OA) and a negative literal P Y(OA).  For each pair
of subterms such as X and Y, extensionality lemmas of the form

    forall x [X x EQUIV Y x] implies X = Y

are added to the expansion tree before beginning mating search.
Note that the type A is determined by the types of the
subterms X and Y.

See Also:  ADD-EXT-LEMMAS"))

(defmateop add-ext-lemmas
  (mate-alias extensionality-etree-lemmas)
  (mhelp "Automatically add extensionality lemmas to the expansion tree.

See Also:  USE-EXT-LEMMAS"))


; input: e - etree
; return: etree of the form (ext1 => . . . (extn => e) . . .)
; where each exti is an etree with shallow [[A = B] implies [all q [q A] implies [q B]]]
; where A and B have the same prop/set/reln type, A occurs as a proper subterm
; of a positive literal and B occurs as a proper subterm of a negative literal
(defun extensionality-etree-lemmas ()
  (let* ((f (etree-to-ftree (eproof-etree current-eproof)))
	 (al (set-subterms f t))
	 (bl (set-subterms f nil))
	 (clist (eproof-dissolve current-eproof))
	 (lemmas (eproof-lemmas current-eproof)))
    (declare (special clist lemmas))
    (dolist (a al)
	    (dolist (b bl)
		    (when (and (type-equal a b) ; same type,
			       (not (wffeq-ab a b))) ; but not syntactically equal
		      (setq f
			    (introduce-extensionality-etree-lemma f a b)))))
    (ftree-to-etree f)
    (setf (eproof-lemmas current-eproof) lemmas)
    (setf (eproof-dissolve current-eproof) clist)
    (cr-eproof-jform)))

(defun introduce-extensionality-etree-lemma (f a b)
  (declare (special clist lemmas))
  (let ((eqn (mbed=left a b))
	(REWRITE-EQUALITIES 'ALL))
    (declare (special REWRITE-EQUALITIES))
    (multiple-value-bind
     (fnegconclem negequivleaf)
     (extensionality-ftree-lemma-pf-1 eqn)
     (multiple-value-bind
      (fposhyplem posequivleaf)
      (extensionality-ftree-lemma-pf-2 fnegconclem)
      (push (cons (ftree-name posequivleaf) (ftree-name negequivleaf))
	    clist)
      (let* ((fneglem (make-ftree-imp fposhyplem fnegconclem))
	     (lem (ftree-shallow fneglem))
	     (q (fresh-var (cons 'O (type (cddr lem))) #\q))
	     (fposlem (make-ftree-imp
		       (gwff-to-ftree (cdar lem) nil)
		       (make-ftree-rew
			(cdr lem) 'LEIBNIZ=
			(gwff-to-ftree
			 (acons q 'forall
				(acons 'IMPLIES
				       (cons q (cdadr lem))
				       (cons q (cddr lem)))) t)))))
	(if lemmas
	    (let* ((oldneglems (car (ftree-components f)))
		   (imp (cadr (ftree-components f)))
		   (oldposlems (car (ftree-components imp)))
		   (main (cadr (ftree-components imp))))
	      (setq f (make-ftree-con
		       (make-ftree-con fneglem oldneglems)
		       (make-ftree-imp (make-ftree-con fposlem oldposlems)
				       main))))
	  (setq f (make-ftree-con fneglem (make-ftree-imp fposlem f))))
	(push (list (intern (create-namestring 'EXT-LEMMA-))) lemmas)
	(setq first-order-mode-ms nil) ; can't be first order now
	(setf (eproof-dissolve current-eproof)
	      (append clist (eproof-dissolve current-eproof)))
	f)))))

(defun extensionality-ftree-lemma-pf-1 (wff)
  (if (equals-p wff)
      (if (eq (type (cdr wff)) 'O)
	  (let* ((e (acons 'EQUIV (cdar wff) (cdr wff)))
		 (l (make-ftree-leaf e nil)))
	    (values (make-ftree-rew wff 'EXT= l) l))
	(multiple-value-bind
	 (f l)
	 (extensionality-ftree-lemma-pf-1
	  (expand-all-equalities wff))
	 (values (make-ftree-rew wff 'EXT= f) l)))
    (if (a-bd-wff-p wff)
	(let* ((x (bindvar wff))
	       (a (fresh-var (type x) (getnameroot x))))
	  (multiple-value-bind
	   (f l)
	   (extensionality-ftree-lemma-pf-1
	    (sublis (acons x a nil) (cdr wff)))
	   (values (make-ftree-sel wff a f) l)))
      (multiple-value-bind
       (f l)
       (extensionality-ftree-lemma-pf-1 (lnorm wff))
       (values (make-ftree-rew wff 'LAMBDA f) l)))))

(defun extensionality-ftree-lemma-pf-2 (f)
  (case (ftree-kind f)
	(REW (extensionality-ftree-lemma-pf-2 (car (ftree-components f))))
	(SEL (multiple-value-bind
	      (g l)
	      (extensionality-ftree-lemma-pf-2 (car (ftree-components f)))
	      (let ((x (bindvar (ftree-shallow f)))
		    (a (ftree-sel-var f)))
		(values (make-ftree-exp
			 (acons x 'forall 
				(sublis (acons a x nil)
					(ftree-shallow g)))
			 (list a)
			 (list g)) l))))
	(LEAF (let ((l (make-ftree-leaf (ftree-shallow f) t)))
		(values l l)))
	(t (throwfail "trouble forming ext lemma")))) ; should never happen

(defun set-subterms (f pos)
  (remove-duplicates (set-subterms-1 f pos)
		     :test #'wffeq-ab))

(defun set-subterms-1 (f pos)
  (let ((ret nil))
    (if (and (eq (ftree-kind f) 'LEAF) (eq (ftree-positive f) pos))
	(setq ret (set-subterms-2 (ftree-shallow f)))
      (dolist (k (ftree-components f))
	      (setq ret (append (set-subterms-1 k pos) ret))))
    ret))

(defun set-subterms-2 (wff &optional bdvars)
  (if (boundwff-p wff)
      (set-subterms-2 (cdr wff) (cons (bindvar wff) bdvars))
    (if (consp wff)
	(append (set-subterms-2 (car wff) bdvars)
		(set-subterms-3 (cdr wff) bdvars))
      nil)))

(defun set-subterms-3 (wff bdvars)
  (do ((y (type wff) (car y)))
      ((not (consp y))
       (if (eq y 'O)
	   (cons (lnorm (bind-var-wff-n 'lambda bdvars wff))
		 (set-subterms-2 wff bdvars))
	 (set-subterms-2 wff bdvars)))))

; 2/21/02
(defmexpr deassert-lemmas 
  (argtypes symbol)
  (argnames prefix)
  (arghelp "Name of the Proof")
  (defaultfns (lambda (prefix) (list (if (eq prefix '$) 
					 (or dproof '$)
				       prefix))))
  (mhelp "Combine a collection of natural deduction proofs where
some lines contain ASSERT justifications where the asserted line
has a natural deduction proof into a single natural deduction proof."))

; returns t if proof changed
(defun deassert-lemmas (prefix)
  (let ((assert-lemma-lines nil))
    (dolist (x (get prefix 'lines))
	    (let ((str (car (line-justification x))))
	      (when (and (stringp str)
			 (string= str "Assert: " :end1 (min 8 (length str))))
		(let ((lemname (read-from-string (string-left-trim "Assert: " str))))
		  (when (and (get lemname 'lines)
			     (wffeq (get lemname 'assertion) (line-assertion x)))
		    (setq dproof lemname)
		    (let ((lemcopy (copy-proof lemname)))
		      (push (cons x lemcopy) assert-lemma-lines)))))))
    (dolist (llem assert-lemma-lines)
	    (deassert-lemmas (cdr llem))
	    (setq dproof prefix)
	    (let* ((lastlemline (car (last (get (cdr llem) 'lines))))
		   (lastlemlinenum (linealias lastlemline))
		   (fstpfline (car (get prefix 'lines))))
	      (introduce-gap fstpfline lastlemlinenum)
	      (setf (get prefix 'lines)
		    (append (get (cdr llem) 'lines)
			    (get prefix 'lines)))
	      (setf (get prefix 'linealiases)
		    (append (get (cdr llem) 'linealiases)
			    (get prefix 'linealiases)))
	      (plan (car llem))
	      (ml::same-short (car llem) lastlemline)))
    (if assert-lemma-lines t nil)))

	      
									
