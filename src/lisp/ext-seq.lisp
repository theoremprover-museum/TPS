;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of EXT-DAGS)

;;;
;;; File: EXT-SEQ  - cebrown - 3/15/03
;;; This corresponds to the extensional sequent calculi in Chad E. Brown's thesis.

(deffile ext-seq
    (part-of EXT-DAGS)
  (extension clisp)
  (mhelp "Extensional Sequent Calculus.  See Chad E. Brown's thesis."))

(context ext-seq)

(defun wffeq-ab-multiset (gam1 gam2)
  (if gam1
      (let* ((g1 (car gam1))
	     (gam3 (remove-if #'(lambda (x) (wffeq-ab x g1)) gam2 :count 1)))
	(if (< (length gam3) (length gam2))
	    (wffeq-ab-multiset (cdr gam1) gam3)
	  nil))
    (not gam2)))

(defun multiset-extract-wffs (gam &rest wffs)
  (multiset-extract-wffs-1 gam wffs))

(defun multiset-extract-wffs-1 (gam wffs)
  (let ((n (length gam)))
    (dolist (wff wffs gam)
      (setq gam (remove-if #'(lambda (x) (wffeq-ab x wff)) gam :count 1))
      (unless (< (length gam) n)
	(throwfail "Could not find " (wff . gwff) " in multiset"))
      (setq n (length gam)))))

(defun negate-wffs (gam)
  (mapcar #'(lambda (x) (cons 'NOT x)) gam))

(defun make-ext-seq-true (&optional gam)
  (make-ext-seq :kind 'TRUE :pos-rule t :wffs (cons 'TRUTH gam)
		:princ-wffs '(TRUTH)))

(defun make-ext-seq-neg (wff fs)
  (let* ((wffs (ext-seq-wffs fs))
	 (gam (multiset-extract-wffs wffs wff))
	 (pwff (cons 'NOT (cons 'NOT wff))))
    (make-ext-seq :kind 'NEG :pos-rule nil
		  :princ-wffs (list pwff)
		  :wffs (cons pwff gam)
		  :prems (list fs))))

(defun make-ext-seq-dis-pos (wff1 wff2 fs)
  (let* ((wffs (ext-seq-wffs fs))
	 (gam (multiset-extract-wffs wffs wff1 wff2))
	 (pwff (acons 'OR wff1 wff2)))
    (make-ext-seq :kind 'DIS :pos-rule t
		  :princ-wffs (list pwff)
		  :wffs (cons pwff gam)
		  :prems (list fs))))

(defun make-ext-seq-dis-neg (wff1 wff2 fs1 fs2)
  (let* ((wffs1 (ext-seq-wffs fs1))
	 (wffs2 (ext-seq-wffs fs2))
	 (gam1 (multiset-extract-wffs wffs1 (cons 'NOT wff1)))
	 (gam2 (multiset-extract-wffs wffs2 (cons 'NOT wff2)))
	 (pwff (cons 'NOT (acons 'OR wff1 wff2))))
    (unless (wffeq-ab-multiset gam1 gam2)
      (throwfail "Gamma's for Neg Disj do not match"))
    (make-ext-seq :kind 'DIS :pos-rule nil
		  :princ-wffs (list pwff)
		  :wffs (cons pwff gam1)
		  :prems (list fs1 fs2))))

(defun make-ext-seq-forall-pos (wff a fs1)
  (let* ((wffs (ext-seq-wffs fs1))
	 (wff2 (substitute-l-term-var a (bindvar wff) (cdr wff)))
	 (gam (multiset-extract-wffs wffs wff2)))
    (make-ext-seq :kind 'FORALL :pos-rule t
		  :princ-wffs (list wff)
		  :wffs (cons wff gam)
		  :sel-var a
		  :prems (list fs1))))

(defun make-ext-seq-forall-neg (wff trm fs1)
  (let* ((wffs (ext-seq-wffs fs1))
	 (wff2 (substitute-l-term-var trm (bindvar wff) (cdr wff)))
	 (gam (multiset-extract-wffs wffs (cons 'NOT wff2))))
    (make-ext-seq :kind 'FORALL :pos-rule nil
		  :princ-wffs (list (cons 'NOT wff))
		  :wffs (cons (cons 'NOT wff) gam)
		  :exp-term trm
		  :prems (list fs1))))

(defun make-ext-seq-internalize (wff0 fs pos)
  (let* ((wffs (ext-seq-wffs fs))
	 (pwff (if pos wff0 (cons 'NOT wff0)))
	 (wff (externalize-wff1 wff0))
	 (gam (multiset-extract-wffs wffs (if pos wff (cons 'NOT wff)))))
    (if (wffeq wff0 wff)
	fs
      (make-ext-seq :wffs (cons pwff gam)
		    :princ-wffs (list pwff)
		    :pos-rule pos
		    :kind 'INTERNALIZE
		    :prems (list fs)))))

(defun loghead (wff)
  (cond ((member wff '(AND OR NOT IMPLIES EQUIV FALSEHOOD TRUTH))
	 wff)
	((boundwff-p wff)
	 (case (binder wff)
	   (EXISTS 'EXISTS)
	   (FORALL 'FORALL)
	   (t nil)))
	((consp wff)
	 (loghead (car wff)))
	(t nil)))

(defun externalize-wff1 (wff)
  (let ((loghead (loghead wff)))
    (case loghead
      (FALSEHOOD (cons 'NOT 'TRUTH))
      (AND
       (let ((l (cdar wff))
	     (r (cdr wff)))
	 (cons 'NOT (acons 'OR (cons 'NOT l) (cons 'NOT r)))))
      (IMPLIES
       (let ((l (cdar wff))
	     (r (cdr wff)))
	 (acons 'OR (cons 'NOT l) r)))
      (EQUIV
       (let ((l (cdar wff))
	     (r (cdr wff)))
	 (externalize-wff1
	  (acons 'AND
		 (acons 'OR (cons 'NOT l) r)
		 (acons 'OR l (cons 'NOT r))))))
      (EXISTS
       (let ((b (cdr wff))
	     (x (bindvar wff)))
	 (cons 'NOT (acons x 'FORALL (cons 'NOT b)))))
      (t wff))))

(defun make-ext-seq-lambda-1 (wff nwff fs)
  (if (wffeq-ab wff nwff)
      fs
    (let* ((wffs (ext-seq-wffs fs))
	   (gam (multiset-extract-wffs wffs nwff)))
      (make-ext-seq :kind 'LAMBDA 
		    :princ-wffs (list wff)
		    :wffs (cons wff gam)
		    :prems (list fs)))))

(defun make-ext-seq-lambda (wff fs)
  (make-ext-seq-lambda-1 wff (etanorm (lambda-norm wff)) fs))

(defun make-ext-seq-init (wff &optional gam)
  (make-ext-seq :princ-wffs (list (cons 'NOT wff) wff)
		:wffs (cons (cons 'NOT wff) (cons wff gam)) :kind 'INIT))

(defun make-ext-seq-init-ab (wff1 wff2 &optional gam)
  (unless (wffeq-ab wff1 wff2)
    (throwfail "Incorrect Application of Init (AB)" (wff1 . gwff) t (wff2 . gwff)))
  (make-ext-seq :princ-wffs (list (cons 'NOT wff1) wff2)
		:wffs (cons (cons 'NOT wff1) (cons wff2 gam)) :kind 'INIT))

(defun make-ext-seq-init-eq (wff1 wff2 gam &rest fsl)
  (let ((bwff wff1)
	(awff wff2)
	(eqwff nil)
	(gam0 nil)
	(pwff1 (cons 'NOT wff1)))
    (dolist (fs (reverse fsl)) ; just for sanity checking - loop can be commented
      (unless (and (consp awff) (consp bwff))
	(throwfail "Problem with InitEq"))
      (let ((tp (unabbreviated-type (cdr awff))))
	(setq eqwff (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))
			   (cdr awff) (cdr bwff))))
      (setq gam0 (multiset-extract-wffs (ext-seq-wffs fs) eqwff))
      (unless (wffeq-ab-multiset gam gam0)
	(throwfail "Gamma Mismatch for Init Eq"))
      (setq awff (car awff) bwff (car bwff)))
    (unless (equal awff bwff)
      (throwfail "Different Heads in Init Eq"))
    (make-ext-seq :wffs (cons pwff1 (cons wff2 gam)) :kind 'INITEQ
		  :princ-wffs (list pwff1 wff2)
		  :prems fsl)))

(defun make-ext-seq-refl (wff &optional gam)
  (let* ((tp (unabbreviated-type wff))
	 (e (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)))
	 (pwff (acons e wff wff)))
    (make-ext-seq :wffs (cons pwff gam)
		  :pos-rule t
		  :princ-wffs (list pwff)
		  :kind 'REFL)))

(defun make-ext-seq-refl-ab (wff &optional gam)
  (unless (and (equals-p wff) (wffeq-ab (cdar wff) (cdr wff)))
    (throwfail "Incorrect application of Refl (AB)" (wff . gwff)))
  (make-ext-seq :wffs (cons wff gam)
		:pos-rule t
		:princ-wffs (list wff)
		:kind 'REFL))

(defun make-ext-seq-dec (wff gam &rest fsl)
  (let ((awff (cdar wff))
	(bwff (cdr wff))
	(eqwff nil)
	(gam0 nil))
    (dolist (fs (reverse fsl)) ; just for sanity checking - loop can be commented
      (unless (and (consp awff) (consp bwff))
	(throwfail "Problem with Dec"))
      (let ((tp (unabbreviated-type (cdr awff))))
	(setq eqwff (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))
			   (cdr awff) (cdr bwff))))
      (setq gam0 (multiset-extract-wffs (ext-seq-wffs fs) eqwff))
      (unless (wffeq-ab-multiset gam gam0)
	(throwfail "Gamma Mismatch for Dec"))
      (setq awff (car awff) bwff (car bwff)))
    (unless (equal awff bwff)
      (throwfail "Different Heads in Dec"))
    (make-ext-seq :wffs (cons wff gam)
		  :princ-wffs (list wff)
		  :pos-rule t
		  :kind 'DEC
		  :prems fsl)))

(defun make-ext-seq-eunif (wff1 wff2 fs1 fs2 rev)
  (unless (and (equals-p wff1)
	       (equals-p wff2)
	       (not (consp (type (cdr wff1))))
	       (not (consp (type (cdr wff2))))
	       (not (eq (type (cdr wff1)) 'O))
	       (not (eq (type (cdr wff2)) 'O))
	       (eq (type (cdr wff1)) (type (cdr wff2))))
    (throwfail "Invalid Application of EUnif"))
  (let* ((wffs1 (ext-seq-wffs fs1))
	 (wffs2 (ext-seq-wffs fs2))
	 (pwff1 (cons 'NOT wff1))
	 (awff (cdar wff1))
	 (bwff (cdr wff1))
	 (cwff (cdar wff2))
	 (dwff (cdr wff2))
	 (eqi (caar wff1))
	 (gam1 (multiset-extract-wffs wffs1
				      (if rev
					  (acons eqi awff dwff)
					(acons eqi awff cwff))))
	 (gam2 (multiset-extract-wffs wffs2
				      (if rev
					  (acons eqi bwff cwff)
					(acons eqi bwff dwff)))))
    (unless (wffeq-ab-multiset gam1 gam2)
      (throwfail "Gamma Mismatch in EUnif"))
    (if rev
	(make-ext-seq :wffs (cons pwff1 (cons wff2 gam1))
		      :princ-wffs (list pwff1 wff2)
		      :kind 'EUNIF2
		      :prems (list fs1 fs2))
      (make-ext-seq :wffs (cons pwff1 (cons wff2 gam1))
		    :princ-wffs (list pwff1 wff2)
		    :kind 'EUNIF1
		    :prems (list fs1 fs2)))))

(defun make-ext-seq-eunif1 (wff1 wff2 fs1 fs2)
  (make-ext-seq-eunif wff1 wff2 fs1 fs2 nil))

(defun make-ext-seq-eunif2 (wff1 wff2 fs1 fs2)
  (make-ext-seq-eunif wff1 wff2 fs1 fs2 t))

(defun make-ext-seq-eqo (wff fs1 fs2)
  (unless (and (equals-p wff) (eq (unabbreviated-type (cdr wff)) 'O))
    (throwfail "Invalid Application of EQO"))
  (let* ((wffs1 (ext-seq-wffs fs1))
	 (wffs2 (ext-seq-wffs fs2))
	 (awff (cdar wff))
	 (bwff (cdr wff))
	 (pwff (cons 'NOT wff))
	 (gam1 (multiset-extract-wffs wffs1 awff bwff))
	 (gam2 (multiset-extract-wffs wffs2 (cons 'NOT awff) (cons 'NOT bwff))))
    (unless (wffeq-ab-multiset gam1 gam2)
      (throwfail "Gamma Mismatch in EQO"))
    (make-ext-seq :wffs (cons pwff gam1)
		  :princ-wffs (list pwff)
		  :pos-rule nil
		  :kind 'EQO
		  :prems (list fs1 fs2))))

 (defun make-ext-seq-eqfunc (wff trm fs)
   (let ((tp (unabbreviated-type (cdr wff))))
     (unless (and (equals-p wff) (consp tp) (equal (cdr tp) (unabbreviated-type trm)))
       (throwfail "Invalid application of EqFunc"))
     (let* ((wffs (ext-seq-wffs fs))
	    (wff2 (acons (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))
			 (cons (cdar wff) trm)
			 (cons (cdr wff) trm)))
	    (pwff (cons 'NOT wff))
	    (gam (multiset-extract-wffs wffs (cons 'NOT wff2))))
       (make-ext-seq :kind 'EQFUNC :pos-rule nil
		     :princ-wffs (list pwff)
		     :wffs (cons pwff gam)
		     :exp-term trm
		     :prems (list fs)))))

(defun make-ext-seq-exto (wff fs1 fs2)
  (unless (and (equals-p wff) (eq (unabbreviated-type (cdr wff)) 'O))
    (throwfail "Invalid Application of EXTO"))
  (let* ((wffs1 (ext-seq-wffs fs1))
	 (wffs2 (ext-seq-wffs fs2))
	 (awff (cdar wff))
	 (bwff (cdr wff))
	 (gam1 (multiset-extract-wffs wffs1 (cons 'NOT awff) bwff))
	 (gam2 (multiset-extract-wffs wffs2 awff (cons 'NOT bwff))))
    (unless (wffeq-ab-multiset gam1 gam2)
      (throwfail "Gamma Mismatch in EXTO"))
    (make-ext-seq :wffs (cons wff gam1)
		  :princ-wffs (list wff)
		  :pos-rule t
		  :kind 'EXTO
		  :prems (list fs1 fs2))))

(defun make-ext-seq-extfunc (wff a fs)
  (let ((tp (unabbreviated-type (cdr wff))))
    (unless (and (equals-p wff) (consp tp) (equal (cdr tp) (unabbreviated-type a)))
      (throwfail "Invalid application of ExtFunc"))
    (let* ((wffs (ext-seq-wffs fs))
	   (wff2 (acons (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))
			(cons (cdar wff) a)
			(cons (cdr wff) a)))
	   (gam (multiset-extract-wffs wffs wff2)))
      (make-ext-seq :kind 'EXTFUNC :pos-rule t
		    :princ-wffs (list wff)
		    :wffs (cons wff gam)
		    :sel-var a
		    :prems (list fs)))))

(defun make-ext-seq-rew (wff1 wff2 rew-just pos fs)
  (let* ((wffs (ext-seq-wffs fs))
	 (wff3 (if pos wff1 (cons 'NOT wff1)))
	 (wff4 (if pos wff2 (cons 'NOT wff2)))
	 (gam (multiset-extract-wffs wffs wff4)))
    (make-ext-seq :kind 'REW :rew-just rew-just
		  :princ-wffs (list wff3)
		  :pos-rule pos
		  :wffs (cons wff3 gam)
		  :prems (list fs))))

(defun make-ext-seq-cut (wff fs1 fs2)
  (let* ((wffs1 (ext-seq-wffs fs1))
	 (wffs2 (ext-seq-wffs fs2))
	 (gam1 (multiset-extract-wffs wffs1 wff))
	 (gam2 (multiset-extract-wffs wffs2 (cons 'NOT wff))))
    (unless (wffeq-ab-multiset gam1 gam2)
      (throwfail "Gamma Mismatch in Cut"))
    (make-ext-seq :kind 'CUT :wffs gam1
		  :prems (list fs1 fs2))))

(defun make-ext-seq-contr (wff fs)
  (let* ((wffs (ext-seq-wffs fs))
	 (gam (multiset-extract-wffs wffs wff wff)))
    (make-ext-seq :kind 'CONTR :princ-wffs (list wff) :wffs (cons wff gam) :prems (list fs))))

; derived rules
(defun make-ext-seq-false (&optional gam)
  (make-ext-seq-internalize
   'FALSEHOOD
   (make-ext-seq-neg 'TRUTH (make-ext-seq-true gam))
   nil))

(defun make-ext-seq-con-pos (wff1 wff2 fs1 fs2)
  (let* ((fs3 (make-ext-seq-neg wff1 fs1))
	 (fs4 (make-ext-seq-neg wff2 fs2))
	 (fs5 (make-ext-seq-dis-neg (cons 'NOT wff1) (cons 'NOT wff2) fs3 fs4)))
    (make-ext-seq-internalize
     (acons 'AND wff1 wff2) fs5 t)))

(defun make-ext-seq-con-neg (wff1 wff2 fs)
  (let* ((wff3 (cons 'NOT wff1))
	 (wff4 (cons 'NOT wff2))
	 (fs2 (make-ext-seq-neg (acons 'OR wff3 wff4)
				(make-ext-seq-dis-pos wff3 wff4 fs))))
    (make-ext-seq-internalize
     (acons 'AND wff1 wff2) fs2 nil)))

(defun make-ext-seq-imp-neg (wff1 wff2 fs1 fs2)
  (let* ((fs3 (make-ext-seq-neg wff1 fs1))
	 (fs4 (make-ext-seq-dis-neg (cons 'NOT wff1) wff2 fs3 fs2)))
    (make-ext-seq-internalize
     (acons 'IMPLIES wff1 wff2) fs4 nil)))

(defun make-ext-seq-imp-pos (wff1 wff2 fs)
  (let ((fs2 (make-ext-seq-dis-pos (cons 'NOT wff1) wff2 fs)))
    (make-ext-seq-internalize
     (acons 'IMPLIES wff1 wff2) fs2 t)))

(defun make-ext-seq-equiv-pos (wff1 wff2 fs1 fs2)
  (let* ((fs3 (make-ext-seq-dis-pos (cons 'NOT wff1) wff2 fs1))
	 (fs4 (make-ext-seq-dis-pos wff1 (cons 'NOT wff2) fs2))
	 (wff3 (acons 'OR (cons 'NOT wff1) wff2))
	 (wff4 (acons 'OR wff1 (cons 'NOT wff2)))
	 (fs5 (make-ext-seq-neg wff3 fs3))
	 (fs6 (make-ext-seq-neg wff4 fs4))
	 (fs7 (make-ext-seq-dis-neg (cons 'NOT wff3) (cons 'NOT wff4) fs5 fs6)))
    (make-ext-seq-internalize (acons 'EQUIV wff1 wff2) fs7 t)))

(defun make-ext-seq-equiv-neg (wff1 wff2 fs1 fs2)
  (let* ((gam (multiset-extract-wffs (ext-seq-wffs fs2) wff1 wff2))
	 (fs3 (make-ext-seq-dis-neg wff1 (cons 'NOT wff2) fs1
				    (make-ext-seq-init (cons 'NOT wff2) gam)))
	 (fs4 (make-ext-seq-neg wff2 fs2))
	 (fs5 (make-ext-seq-dis-neg wff1 (cons 'NOT wff2) (make-ext-seq-init wff1 gam) fs4))
	 (fs6 (make-ext-seq-neg wff1 fs5))
	 (fs7 (make-ext-seq-dis-neg (cons 'NOT wff1) wff2 fs6 fs3))
	 (wff3 (acons 'OR (cons 'NOT wff1) wff2))
	 (wff4 (acons 'OR wff1 (cons 'NOT wff2)))
	 (fs8 (make-ext-seq-dis-pos (cons 'NOT wff3) (cons 'NOT wff4) fs7))
	 (fs9 (make-ext-seq-neg (acons 'OR (cons 'NOT wff3) (cons 'not wff4)) fs8)))
    (make-ext-seq-internalize (acons 'EQUIV wff1 wff2) fs9 nil)))

(defun make-ext-seq-exists-pos (wff trm fs1)
  (let* ((b (cdr wff))
	 (x (bindvar wff))
	 (wff2 (substitute-l-term-var trm x b)))
    (make-ext-seq-internalize
     wff
     (make-ext-seq-forall-neg
      (acons x 'FORALL (cons 'NOT b))
      trm 
      (make-ext-seq-neg wff2 fs1))
     t)))

(defun make-ext-seq-exists-neg (wff a fs1)
  (let* ((b (cdr wff))
	 (x (bindvar wff))
	 (wff3 (acons x 'FORALL (cons 'NOT b))))
    (make-ext-seq-internalize
     wff
     (make-ext-seq-neg
      wff3
      (make-ext-seq-forall-pos
       wff3 a fs1))
     nil)))

(defun make-ext-seq-sym-ax-1 (tp pos neg &optional gam)
  (cond ((consp tp)
	 (let ((a (fresh-var-1 (cdr tp)))
	       (eqb (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))))
	   (make-ext-seq-extfunc
	    pos a
	    (make-ext-seq-eqfunc
	     neg a
	     (make-ext-seq-sym-ax-1
	      (car tp)
	      (acons eqb (cons (cdar pos) a) (cons (cdr pos) a))
	      (acons eqb (cons (cdar neg) a) (cons (cdr neg) a))
	      gam)))))
	((eq tp 'O)
	 (let ((a1 (cdar pos))
	       (b1 (cdr pos))
	       (b2 (cdar neg))
	       (a2 (cdr neg)))
	   (make-ext-seq-exto
	    pos
	    (make-ext-seq-eqo
	     neg
	     (make-ext-seq-init-ab a1 a2 (cons b1 (cons b2 gam)))
	     (make-ext-seq-init-ab b2 b1 (cons (cons 'NOT a1) (cons (cons 'NOT a2) gam))))
	    (make-ext-seq-eqo
	     neg
	     (make-ext-seq-init-ab b1 b2 (cons a1 (cons a2 gam)))
	     (make-ext-seq-init-ab a2 a1 (cons (cons 'NOT b1) (cons (cons 'NOT b2) gam)))))))
	(t
	 (make-ext-seq-eunif2
	  neg pos
	  (make-ext-seq-refl-ab (acons (caar neg) (cdar neg) (cdr pos)) gam)
	  (make-ext-seq-refl-ab (acons (caar neg) (cdr neg) (cdar pos)) gam)))))

(defun make-ext-seq-sym-ax (neg &optional gam)
  (let* ((pos (acons (caar neg) (cdr neg) (cdar neg))))
    (make-ext-seq-sym-ax-1 (unabbreviated-type (cdr neg))
			   pos neg gam)))
	

; admissible rules
(defun make-ext-seq-weaken (fs delta)
  (let ((dfrees nil))
    (dolist (d delta)
      (setq dfrees (append dfrees (free-vars-of d))))
    (setq dfrees (remove-duplicates dfrees))
    (make-ext-seq-weaken-rec fs delta dfrees nil)))

(defun make-ext-seq-weaken-rec (fs delta dfrees &optional theta)
  (let ((es (copy-ext-seq fs))
	(a (ext-seq-sel-var fs)))
    (setf (ext-seq-wffs es)
	  (append (mapcar #'(lambda (x)
			      (simul-substitute-l-term-var theta x))
			  (ext-seq-wffs es))
		  delta))
    (when (ext-seq-exp-term es)
      (setf (ext-seq-exp-term es)
	    (simul-substitute-l-term-var theta (ext-seq-exp-term es))))
    (when (and a (member a dfrees))
      (let ((b (fresh-var (type a) (getnameroot a))))
	(setq theta (acons a b theta))
	(setf (ext-seq-sel-var es) b)))
    (setf (ext-seq-prems es)
	  (mapcar #'(lambda (x)
		      (make-ext-seq-weaken-rec x delta dfrees theta))
		  (ext-seq-prems es)))
    es))

(defun ext-seq-general-eunif1 (tp lft1 rght1 lft2 rght2 &optional gam)
  (let* ((q (inherit-abbrev '= (acons 'O tp tp) (list tp)))
	 (wff1 (acons q lft1 rght1))
	 (wff2 (acons q lft2 rght2)))
    (if (consp tp)
	(let* ((tp1 (car tp))
	       (tp2 (cdr tp))
	       (x (fresh-var-1 tp2)))
	(make-ext-seq-extfunc
	 wff1 x
	 (make-ext-seq-eqfunc
	  wff1 x
	  (ext-seq-general-eunif1 tp1 (cons lft1 x) (cons rght1 x)
				  (cons lft2 x) (cons rght2 x) gam))))
      (if (eq tp 'O)
	  (make-ext-seq-eqo
	   wff1
	   (make-ext-seq-exto wff2
			      (ext-seq-expand-init lft2 lft1 (cons rght1 (cons rght2 gam)))
			      (ext-seq-expand-init rght2 rght1 (cons lft1 (cons lft2 gam))))
	   (make-ext-seq-exto wff2
			      (ext-seq-expand-init rght1 rght2 (cons (cons 'NOT lft1) (cons (cons 'NOT lft2) gam)))
			      (ext-seq-expand-init lft1 lft2 (cons (cons 'NOT rght1) (cons (cons 'NOT rght2) gam)))))
	(make-ext-seq-eunif1
	 wff1 wff2
	 (ext-seq-expand-refl lft1 lft2 gam)
	 (ext-seq-expand-refl rght1 rght2 gam))))))

(defun ext-seq-expand-refl-4 (h args1 args2 wff1 wff2 &optional gam esl)
  (cond ((and (consp args1) (consp args2))
	 (let ((es (ext-seq-expand-refl (cdr args1) (cdr args2) gam)))
	   (ext-seq-expand-refl-4 h (car args1) (car args2) wff1 wff2 gam (cons es esl))))
	((and (eq h args1) (eq h args2))
	 (let ((tp (type wff1)))
	   (apply #'make-ext-seq-dec (append
				      (list (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))
						   wff1 wff2) gam)
				      esl))))
	(t (throwfail "Mismatch " (wff1 . gwff) " with " (wff2 . gwff) " in refl"))))

; wff1 and wff2 are a base type other than O and are beta-eta normal
(defun ext-seq-expand-refl-3 (tp wff1 wff2 &optional gam)
  (if (head-abbrev-p wff1)
      (ext-seq-expand-refl-2 tp (instantiate-1 wff1) wff2 gam)
    (if (head-abbrev-p wff2)
	(ext-seq-expand-refl-2 tp wff1 (instantiate-1 wff2) gam)
      (let ((h1 (head wff1))
	    (h2 (head wff2)))
	(if (eq h1 h2)
	    (ext-seq-expand-refl-4 h1 wff1 wff2 wff1 wff2 gam nil)
	  (throwfail "Head Mismatch " h1 " and " h2 " in refl."))))))
	  
; wff1 and wff2 are a base type other than O
(defun ext-seq-expand-refl-2 (tp wff1 wff2 &optional gam)
  (let ((nwff1 (etanorm (lambda-norm wff1)))
	(nwff2 (etanorm (lambda-norm wff2))))
    (make-ext-seq-lambda-1
     (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)) wff1 wff2)
     (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)) nwff1 nwff2)
     (ext-seq-expand-refl-3 tp nwff1 nwff2 gam))))

(defun ext-seq-expand-refl-1 (tp wff1 wff2 &optional gam)
  (cond ((consp tp)
	 (let ((a (fresh-var-1 (cdr tp))))
	   (make-ext-seq-extfunc
	    (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)) wff1 wff2)
	    a
	    (ext-seq-expand-refl-1
	     (car tp) (cons wff1 a) (cons wff2 a) gam))))
	((eq tp 'O)
	 (let ((es1 (ext-seq-expand-init wff1 wff2 gam))
	       (es2 (ext-seq-expand-init wff2 wff1 gam)))
	   (make-ext-seq-exto
	    (acons (inherit-abbrev '= '((O . O) . O) (list 'O)) wff1 wff2)
	    es1 es2)))
	(t
	 (ext-seq-expand-refl-2 tp wff1 wff2 gam))))
    
(defun ext-seq-expand-refl (wff1 wff2 &optional gam)
  (ext-seq-expand-refl-1 (unabbreviated-type wff1) wff1 wff2 gam))

; h must be a parameter/variable
(defun ext-seq-expand-init-5 (h args1 args2 wff1 wff2 &optional gam esl)
  (cond ((and (consp args1) (consp args2))
	 (let ((es (ext-seq-expand-refl (cdr args1) (cdr args2) gam)))
	   (ext-seq-expand-init-5 h (car args1) (car args2) wff1 wff2 gam (cons es esl))))
	((and (eq h args1) (eq h args2))
	 (apply #'make-ext-seq-init-eq (append (list wff1 wff2 gam) esl)))
	(t (throwfail "Mismatch " (wff1 . gwff) " with " (wff2 . gwff) " in init"))))
  

; wff1 and wff2 are beta-eta-normal with common nonabbrev head (or binder) h
(defun ext-seq-expand-init-4 (h wff1 wff2 &optional gam)
  (case h
    ((EXISTS AND IMPLIES EQUIV FALSEHOOD)
     (let* ((ewff1 (externalize-wff1 wff1))
	    (ewff2 (externalize-wff1 wff2))
	    (k (loghead ewff1)))
       (make-ext-seq-internalize
	wff1
	(make-ext-seq-internalize
	 wff2
	 (ext-seq-expand-init-4 k ewff1 ewff2 gam)
	 t) nil)))
    (TRUTH
     (make-ext-seq-true (cons (cons 'NOT wff1) gam)))
    (NOT
     (make-ext-seq-neg
      (cdr wff1)
      (ext-seq-expand-init (cdr wff2) (cdr wff1) gam)))
    (OR
     (let ((es1 (ext-seq-expand-init (cdar wff1) (cdar wff2) (cons (cdr wff1) gam)))
	   (es2 (ext-seq-expand-init (cdr wff1) (cdr wff2) (cons (cdar wff1) gam))))
       (make-ext-seq-dis-pos
	(cdar wff2) (cdr wff2)
	(make-ext-seq-dis-neg
	 (cdar wff1) (cdr wff1)
	 es1 es2))))
    (FORALL
     (let* ((x (bindvar wff1))
	    (tp (unabbreviated-type x))
	    (y (fresh-var tp (getnameroot x)))
	    (wff1a (substitute-l-term-var y x (cdr wff1)))
	    (wff2a (substitute-l-term-var y (bindvar wff2) (cdr wff2))))
       (make-ext-seq-forall-pos
	wff2 y
	(make-ext-seq-forall-neg
	 wff1 y
	 (ext-seq-expand-init wff1a wff2a gam)))))
    (t
     (if (equality-p h)
	 (ext-seq-general-eunif1
	  (cdr (unabbreviated-type h))
	  (cdar wff1) (cdr wff1)
	  (cdar wff2) (cdr wff2) gam)
       (ext-seq-expand-init-5 h wff1 wff2 wff1 wff2 gam nil)))))

(defun ext-seq-expand-init-3 (h1 wff1 wff2 &optional gam)
  (if (atom-head-abbrev-p wff2)
      (let ((wff3 (instantiate-head-abbrev wff2)))
	(make-ext-seq-rew
	 wff2 wff3 'EQUIVWFFS t
	 (ext-seq-expand-init-2 h1 wff1 wff3 gam)))
    (if (boundwff-p wff2)
	(if (eq (binder wff2) h1)
	    (ext-seq-expand-init-4 h1 wff1 wff2 gam)
	  (throwfail "binders " h1 " and " (binder wff2) " in init do not match"))
      (let ((h (or (loghead wff2) (head wff2))))
	(if (eq h1 h)
	    (ext-seq-expand-init-4 h1 wff1 wff2 gam)
	  (throwfail "heads " h1 " and " h " in init do not match"))))))

(defun ext-seq-expand-init-2 (h1 wff1 wff2 &optional gam)
  (let ((nwff2 (etanorm (lambda-norm wff2))))
    (make-ext-seq-lambda-1
     wff2
     nwff2
     (ext-seq-expand-init-3 h1 wff1 nwff2 gam))))

(defun ext-seq-expand-init-1 (wff1 wff2 &optional gam)
  (if (atom-head-abbrev-p wff1)
      (let ((wff3 (instantiate-head-abbrev wff1)))
	(make-ext-seq-rew
	 wff1 wff3 'EQUIVWFFS nil
	 (ext-seq-expand-init wff3 wff2 gam)))
    (if (boundwff-p wff1)
	(ext-seq-expand-init-2 (binder wff1) wff1 wff2 gam)
      (let ((h (or (loghead wff1) (head wff1))))
	(ext-seq-expand-init-2 h wff1 wff2 gam)))))

(defun ext-seq-expand-init (wff1 wff2 &optional gam)
  (let ((nwff1 (etanorm (lambda-norm wff1))))
    (make-ext-seq-lambda-1
     (cons 'NOT wff1)
     (cons 'NOT nwff1)
     (ext-seq-expand-init-1 nwff1 wff2 gam))))

(defun ext-seq-expand-init-refl (es)
  (case (ext-seq-kind es)
    (INIT 
     (let* ((wffs (ext-seq-wffs es))
	    (princ-wffs (ext-seq-princ-wffs es))
	    (wff1 (car princ-wffs))
	    (wff2 (cadr princ-wffs))
	    (gam (multiset-extract-wffs wffs wff1 wff2)))
       (ext-seq-expand-init (cdr wff1) wff2 gam)))
    (REFL
     (let* ((wffs (ext-seq-wffs es))
	    (princ-wffs (ext-seq-princ-wffs es))
	    (wff (car princ-wffs))
	    (gam (multiset-extract-wffs wffs wff)))
       (ext-seq-expand-refl (cdar wff) (cdr wff) gam)))
    (t
     (let ((es2 (copy-ext-seq es)))
       (setf (ext-seq-prems es2)
	     (mapcar #'ext-seq-expand-init-refl (ext-seq-prems es)))
       es2))))

; returns "gam, ~eqwff, leibwff"
(defun ext-seq-leibniz-neg (eqwff leibwff &optional gam)
  (let* ((q (bindvar leibwff))
	 (tp (unabbreviated-type q))
	 (q2 (fresh-var tp (getnameroot q)))
	 (wff2 (substitute-l-term-var q2 q (cdr leibwff)))
	 (wff3 (cdar wff2))
	 (wff4 (cdr wff2)))
    (unless (and (equals-p eqwff) (a-bd-wff-p leibwff) (implies-p wff2)
		 (wffeq-ab (cdar eqwff) (cdr wff3)) (wffeq-ab (cdr eqwff) (cdr wff4)))
      (throwfail "Bad call to ext-seq-leibniz-neg"))
    (make-ext-seq-forall-pos
     leibwff q2
     (make-ext-seq-imp-pos
      wff3 wff4
      (make-ext-seq-init-eq
       wff3 wff4 (cons (cons 'NOT eqwff) gam)
       (make-ext-seq-sym-ax eqwff gam))))))

; returns "gam, eqwff, ~leibwff"
(defun ext-seq-leibniz-pos (eqwff leibwff &optional gam)
  (let* ((lft (cdar eqwff))
	 (q (bindvar leibwff))
	 (tp (unabbreviated-type q))
	 (tp2 (cdr tp))
	 (x (fresh-var-1 tp2))
	 (qe (inherit-abbrev '= (cons tp tp2) (list tp2)))
	 (trm (acons x 'LAMBDA (acons qe lft x)))
	 (wff2 (substitute-l-term-var trm q (cdr leibwff)))
	 (wff3 (etanorm (lambda-norm wff2)))
	 (wff4 (cdar wff3))
	 (wff5 (cdr wff3)))
    (make-ext-seq-forall-neg
     leibwff trm
     (make-ext-seq-lambda-1
      (cons 'NOT wff2) (cons 'NOT wff3)
      (make-ext-seq-imp-neg
       wff4 wff5
       (make-ext-seq-refl lft (cons eqwff gam))
       (make-ext-seq-init eqwff gam))))))

(defun ext-seq-wff-assoc (wff wffassoc)
  (let ((a (assoc wff wffassoc :test #'wffeq-ab)))
    (unless a
      (throwfail "equivwffs assoc problem"))
    (values (cdr a)
	    (remove a wffassoc :count 1))))

(defun ext-seq-gamma-assoc (gam wffassoc)
  (let ((gamret nil))
    (dolist (w gam)
      (let ((a (assoc w wffassoc :test #'wffeq-ab)))
	(unless a
	  (throwfail "equivwffs gamma assoc problem"))
	(push (cdr a) gamret)
	(setq wffassoc (remove a wffassoc :count 1))))
    gamret))

(defun ext-seq-equivwffs-reduced (wff)
  (let ((nwff (etanorm (lambda-norm wff))))
    (if (wffeq-ab wff nwff)
	(let ((wff1 (if (not-p wff) (cdr wff) wff)))
	  (if (atom-head-abbrev-p wff)
	      (let ((wff2 (instantiate-head-abbrev wff1)))
		(list 'EQUIVWFFS wff2 (not (not-p wff))))
	    nil))
      (list 'LAMBDA nwff))))

(defun ext-seq-equivwffs (wffassoc es)
  (let ((princ-wffs (ext-seq-princ-wffs es))
	(wffs (ext-seq-wffs es))
	(k (ext-seq-kind es))
	(pos-rule (ext-seq-pos-rule es))
	(prems (ext-seq-prems es)))
    (if princ-wffs
	(let ((wff1 (car princ-wffs)))
	  (multiple-value-bind
	      (wff1r wffassoc1)
	      (ext-seq-wff-assoc wff1 wffassoc)
	    (let ((wff1rr (ext-seq-equivwffs-reduced wff1r)))
	      (if wff1rr
		  (let ((es2 (ext-seq-equivwffs (acons wff1 (cadr wff1rr) wffassoc1) es))) 
		    (if (eq (car wff1rr) 'LAMBDA)
			(make-ext-seq-lambda-1 wff1r (cadr wff1rr) es2)
		      (make-ext-seq-rew (if (caddr wff1rr)
					    wff1r
					  (cdr wff1r))
					(cadr wff1rr) 'EQUIVWFFS (caddr wff1rr) es2)))
		(if (cdr princ-wffs)
		    (let ((wff2 (cadr princ-wffs)))
		      (multiple-value-bind
			  (wff2r wffassoc2)
			  (ext-seq-wff-assoc wff2 wffassoc1)
			(let ((wff2rr (ext-seq-equivwffs-reduced wff2r)))
			  (if wff2rr
			      (let ((es2 (ext-seq-equivwffs
					  (acons wff2 (cadr wff2rr) (acons wff1 wff1r wffassoc2))
					  es)))
				(if (eq (car wff1rr) 'LAMBDA)
				    (make-ext-seq-lambda-1 wff2r (cadr wff2rr) es2)
				  (make-ext-seq-rew (if (caddr wff2rr)
							wff2r
						      (cdr wff2r))
						    (cadr wff2rr) 'EQUIVWFFS (caddr wff2rr) es2)))
			    (case k
			      (INITEQ
			       (let* ((bargs (args (cdr wff1)))
				      (aargs (args wff2))
				      (bargsr (args (cdr wff1r)))
				      (aargsr (args wff2r))
				      (gam (multiset-extract-wffs wffs wff1 wff2))
				      (gam2 (ext-seq-gamma-assoc gam wffassoc))
				      (esl nil))
				 (do ((prems0 prems (cdr prems0))
				      (bargs0 bargs (cdr bargs0))
				      (aargs0 aargs (cdr aargs0))
				      (bargs0r bargsr (cdr bargs0r))
				      (aargs0r aargsr (cdr aargs0r)))
				     ((null prems0)
				      (when (or bargs0 aargs0 bargs0r aargs0r)
					(throwfail "argument problem with INITEQ and equivwffs")))
				   (unless (and bargs0 aargs0 bargs0r aargs0r)
				     (throwfail "argument problem with INITEQ and equivwffs"))
				   (let* ((tp (unabbreviated-type (car bargs0)))
					  (q (inherit-abbrev '= (acons 'O tp tp) (list tp)))
					  (eqwff (acons q (car aargs0) (car bargs0)))
					  (eqwffr (acons q (car aargs0r) (car bargs0r))))
				     (push (ext-seq-equivwffs 
					    (acons eqwff eqwffr wffassoc2)
					    (car prems0))
					   esl)))
				 (apply #'make-ext-seq-init-eq
					(append (list (cdr wff1r) wff2r gam2)
						(reverse esl)))))
			      ((EUNIF1 EUNIF2)
			       (let* ((lft1 (cdadr wff1))
				      (rght1 (cddr wff1))
				      (lft2 (cdar wff2))
				      (rght2 (cdr wff2))
				      (lft1r (cdadr wff1r))
				      (rght1r (cddr wff1r))
				      (lft2r (cdar wff2r))
				      (rght2r (cdr wff2r))
				      (q (caar wff2))
				      (eqwff1 (if (eq k 'EUNIF1)
						  (acons q lft1 lft2)
						(acons q lft1 rght2)))
				      (eqwff2 (if (eq k 'EUNIF1)
						  (acons q rght1 rght2)
						(acons q rght1 lft2)))
				      (eqwff1r (if (eq k 'EUNIF1)
						   (acons q lft1r lft2r)
						 (acons q lft1r rght2r)))
				      (eqwff2r (if (eq k 'EUNIF1)
						   (acons q rght1r rght2r)
						 (acons q rght1r lft2r))))
				 (make-ext-seq-eunif
				  wff1r wff2r
				  (ext-seq-equivwffs
				   (acons eqwff1 eqwff1r wffassoc2)
				   (car prems))
				  (ext-seq-equivwffs
				   (acons eqwff2 eqwff2r wffassoc2)
				   (cadr prems))
				  (eq k 'EUNIF2))))
			      (INIT
			       (if (and (not-p wff1r) (wffeq-ab (cdr wff1r) wff2r))
				   (let* ((gam (multiset-extract-wffs wffs wff1 wff2))
					  (gam2 (ext-seq-gamma-assoc gam wffassoc)))
				     (make-ext-seq-init-ab (cdr wff1r) wff2r gam2))
				 (ext-seq-equivwffs wffassoc (ext-seq-expand-init-refl es))))
			      (t (throwfail "Rule " k " should not have more than one princ wff")))))))
		  (case k
		    ((LAMBDA REW)
		     (let* ((gam (multiset-extract-wffs wffs wff1))
			    (prem1 (car prems))
			    (pwffs (ext-seq-wffs prem1))
			    (nwff1 (car (multiset-extract-wffs-1 pwffs gam))))
		       (ext-seq-equivwffs (acons nwff1 wff1r wffassoc1) (car prems))))
		    (TRUE
		     (let* ((gam (multiset-extract-wffs wffs 'TRUTH))
			    (gam2 (ext-seq-gamma-assoc gam wffassoc)))
		       (make-ext-seq-true gam2)))
		    (NEG
		     (if (and (not-p wff1r) (not-p (cdr wff1r)))
			 (make-ext-seq-neg (cddr wff1r)
					   (ext-seq-equivwffs
					    (acons (cddr wff1) (cddr wff1r) wffassoc1)
					    (car prems)))
		       (throwfail "Formula mismatch in ext-seq-equivwffs" (wff1 . gwff) t (wff1r . gwff))))
		    (DIS
		     (if pos-rule
			 (make-ext-seq-dis-pos (cdar wff1r) (cdr wff1r)
					       (ext-seq-equivwffs
						(acons (cdar wff1) (cdar wff1r)
						       (acons (cdr wff1) (cdr wff1r)
							      wffassoc1))
						(car prems)))
		       (make-ext-seq-dis-neg (cdadr wff1r) (cddr wff1r)
					     (ext-seq-equivwffs
					      (acons (cons 'NOT (cdadr wff1)) (cons 'NOT (cdadr wff1r))
						     wffassoc1)
					      (car prems))
					     (ext-seq-equivwffs
					      (acons (cons 'NOT (cddr wff1)) (cons 'NOT (cddr wff1r))
						     wffassoc1)
					      (cadr prems)))))
		    (FORALL
		     (if pos-rule
			 (let ((sv (ext-seq-sel-var es))
			       (x1 (bindvar wff1))
			       (x2 (bindvar wff1r)))
			   (make-ext-seq-forall-pos wff1r sv
						    (ext-seq-equivwffs
						     (acons (substitute-l-term-var sv x1 (cdr wff1))
							    (substitute-l-term-var sv x2 (cdr wff1r))
							    wffassoc1)
						     (car prems))))
		       (let* ((trm (ext-seq-exp-term es))
			      (x1 (bindvar (cdr wff1)))
			      (x2 (bindvar (cdr wff1r)))
			      (instwff1 (substitute-l-term-var trm x2 (cddr wff1r)))
			      (es2 (ext-seq-equivwffs
				    (acons (cons 'NOT (substitute-l-term-var trm x1 (cddr wff1)))
					   (cons 'NOT instwff1) wffassoc1)
				    (car prems))))
			 (make-ext-seq-forall-neg (cdr wff1r) trm es2))))
		    (INTERNALIZE
		     (if pos-rule
			 (let ((ewff1 (externalize-wff1 wff1))
			       (ewff1r (externalize-wff1 wff1r)))
			   (make-ext-seq-internalize
			    wff1r (ext-seq-equivwffs
				   (acons ewff1 ewff1r wffassoc1)
				   (car prems)) t))
		       (let ((ewff1 (externalize-wff1 (cdr wff1)))
			     (ewff1r (externalize-wff1 (cdr wff1r))))
			 (make-ext-seq-internalize
			  (cdr wff1r)
			  (ext-seq-equivwffs
			   (acons (cons 'NOT ewff1) (cons 'NOT ewff1r) wffassoc1)
			   (car prems)) nil))))
		    (DEC
		     (let* ((lft (cdar wff1))
			    (rght (cdr wff1))
			    (lftr (cdar wff1r))
			    (rghtr (cdr wff1r))
			    (aargs (args lft))
			    (bargs (args rght))
			    (aargsr (args lftr))
			    (bargsr (args rghtr))
			    (gam (multiset-extract-wffs wffs wff1))
			    (gam2 (ext-seq-gamma-assoc gam wffassoc))
			    (esl nil))
		       (do ((prems0 prems (cdr prems0))
			    (bargs0 bargs (cdr bargs0))
			    (aargs0 aargs (cdr aargs0))
			    (bargs0r bargsr (cdr bargs0r))
			    (aargs0r aargsr (cdr aargs0r)))
			   ((null prems0)
			    (when (or bargs0 aargs0 bargs0r aargs0r)
			      (throwfail "argument problem with DEC and equivwffs")))
			 (unless (and bargs0 aargs0 bargs0r aargs0r)
			   (throwfail "argument problem with DEC and equivwffs"))
			 (let* ((tp (unabbreviated-type (car bargs0)))
				(q (inherit-abbrev '= (acons 'O tp tp) (list tp)))
				(eqwff (acons q (car aargs0) (car bargs0)))
				(eqwffr (acons q (car aargs0r) (car bargs0r))))
			   (push (ext-seq-equivwffs 
				  (acons eqwff eqwffr wffassoc1)
				  (car prems0))
				 esl)))
		       (apply #'make-ext-seq-dec
			      (cons wff1r (cons gam2 (reverse esl))))))
		    (EQO
		     (let ((awff (cdadr wff1))
			   (bwff (cddr wff1))
			   (awffr (cdadr wff1r))
			   (bwffr (cddr wff1r)))
		       (make-ext-seq-eqo
			(cdr wff1r)
			(ext-seq-equivwffs
			 (acons awff awffr (acons bwff bwffr wffassoc1))
			 (car prems))
			(ext-seq-equivwffs
			 (acons (cons 'NOT awff) (cons 'NOT awffr)
				(acons (cons 'NOT bwff) (cons 'NOT bwffr) wffassoc1))
			 (cadr prems)))))
		    (EXTO
		     (let ((awff (cdar wff1))
			   (bwff (cdr wff1))
			   (awffr (cdar wff1r))
			   (bwffr (cdr wff1r)))
		       (make-ext-seq-exto
			wff1r
			(ext-seq-equivwffs
			 (acons (cons 'NOT awff) (cons 'NOT awffr)
				(acons bwff bwffr wffassoc1))
			 (car prems))
			(ext-seq-equivwffs
			 (acons (cons 'NOT bwff) (cons 'NOT bwffr)
				(acons awff awffr wffassoc1))
			 (cadr prems)))))
		    (EQFUNC
		     (let* ((exptrm (ext-seq-exp-term es))
			    (lft (cdar wff1))
			    (rght (cdr wff1))
			    (lftr (cdar wff1r))
			    (rghtr (cdr wff1r))
			    (tp (unabbreviated-type lft))
			    (tp2 (car tp))
			    (q (inherit-abbrev '= (acons 'O tp2 tp2) (list tp2)))
			    (wff2 (acons q (cons lft exptrm) (cons rght exptrm)))
			    (wff2r (acons q (cons lftr exptrm) (cons rghtr exptrm))))
		       (make-ext-seq-eqfunc
			wff1r exptrm (ext-seq-equivwffs (acons wff2 wff2r wffassoc1) (car prems)))))
		    (EXTFUNC
		     (let* ((sv (ext-seq-sel-var es))
			    (lft (cdar wff1))
			    (rght (cdr wff1))
			    (lftr (cdar wff1r))
			    (rghtr (cdr wff1r))
			    (tp (unabbreviated-type lft))
			    (tp2 (car tp))
			    (q (inherit-abbrev '= (acons 'O tp2 tp2) (list tp2)))
			    (wff2 (acons q (cons lft sv) (cons rght sv)))
			    (wff2r (acons q (cons lftr sv) (cons rghtr sv))))
		       (make-ext-seq-extfunc
			wff1r sv (ext-seq-equivwffs (acons wff2 wff2r wffassoc1) (car prems)))))
		    (CONTR
		     (make-ext-seq-contr
		      wff1r (ext-seq-equivwffs (acons wff1 wff1r wffassoc) (car prems))))
		    (REFL
		     (if (and (equals-p wff1r) (wffeq-ab (cdar wff1r) (cdr wff1r)))
			 (let* ((gam (multiset-extract-wffs wffs wff1))
				(gam2 (ext-seq-gamma-assoc gam wffassoc)))
			   (make-ext-seq-refl-ab wff1r gam2))
		       (ext-seq-equivwffs wffassoc (ext-seq-expand-init-refl es))))
		    (t (throwfail "Rule " k " should not have one princ wff"))))))))
      (case k
	(CUT
	 (let* ((prem1 (car prems))
		(cutwff (car (multiset-extract-wffs-1 (ext-seq-prems prem1) wffs)))
		(ncutwff (cons 'NOT cutwff)))
	   (make-ext-seq-cut cutwff
			     (ext-seq-equivwffs (acons cutwff cutwff wffassoc) prem1)
			     (ext-seq-equivwffs (acons ncutwff ncutwff wffassoc) prem1))))
	(t (throwfail "Rule " k " should have princ wff"))))))

; inversion principles
; input: derivation of Gamma
; output: derivation of nf(Gamma) where every lambda rule is applied just before an ALL-, EQFUNC or EXTFUNC
(defun ext-seq-eager-lambdas (fs)
  (let* ((prems (ext-seq-prems fs))
	 (prems2 (mapcar #'ext-seq-eager-lambdas prems))
	 (k (ext-seq-kind fs)))
    (if (eq k 'LAMBDA)
	(car prems2)
      (if (and (eq k 'FORALL) (not (ext-seq-pos-rule fs)))
	  (let* ((etrm (ext-seq-exp-term fs))
		 (wff (etanorm (lambda-norm (cdar (ext-seq-princ-wffs fs)))))
		 (wff2 (substitute-l-term-var etrm (bindvar wff) (cdr wff))))
	    (make-ext-seq-forall-neg
	     wff etrm
	     (make-ext-seq-lambda (cons 'NOT wff2) (car prems2))))
	(if (eq k 'EQFUNC)
	    (let* ((etrm (ext-seq-exp-term fs))
		   (wff (etanorm (lambda-norm (cdar (ext-seq-princ-wffs fs)))))
		   (tp (unabbreviated-type (cdr wff)))
		   (wff2 (acons (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))
				(cons (cdar wff) etrm)
				(cons (cdr wff) etrm))))
	      (make-ext-seq-eqfunc
	       wff etrm
	       (make-ext-seq-lambda (cons 'NOT wff2) (car prems2))))
	  (if (eq k 'EXTFUNC)
	      (let* ((sv (ext-seq-sel-var fs))
		     (wff (etanorm (lambda-norm (car (ext-seq-princ-wffs fs)))))
		     (tp (unabbreviated-type (cdr wff)))
		     (wff2 (acons (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))
				  (cons (cdar wff) sv)
				  (cons (cdr wff) sv))))
		(make-ext-seq-extfunc
		 wff sv
		 (make-ext-seq-lambda wff2 (car prems2))))
	    (let ((es (copy-ext-seq fs)))
	      (setf (ext-seq-princ-wffs es)
		    (mapcar #'(lambda (x)
				(etanorm (lambda-norm x)))
			    (ext-seq-princ-wffs es)))
	      (setf (ext-seq-wffs es)
		    (mapcar #'(lambda (x)
				(etanorm (lambda-norm x)))
			    (ext-seq-wffs es)))
	      (setf (ext-seq-prems es) prems2)
	      es)))))))

(defun ext-seq-invert-lambda-1 (wff nwff fs &optional (count 1))
  (if (wffeq-ab wff nwff)
      fs
    (let* ((es (ext-seq-eager-lambdas fs))
	   (wffs (ext-seq-wffs fs))
	   (gam (remove-if #'(lambda (x) (wffeq-ab x wff)) wffs :count count)))
      (dolist (a gam)
	(setq es (make-ext-seq-lambda a es)))
      es)))
  
(defun ext-seq-invert-lambda (wff fs &optional (count 1))
  (ext-seq-invert-lambda-1 wff (etanorm (lambda-norm wff)) fs count))

(defun ext-seq-general-inversion (pwff premwffs ki fs pos premi &optional (count 1))
  (let* ((nwff (etanorm (lambda-norm pwff)))
	 (npremwffs (mapcar #'(lambda (x) (etanorm (lambda-norm x))) premwffs))
	 (nfs (ext-seq-invert-lambda-1 pwff nwff fs count))
;	 (es (ext-seq-general-inversion-1 nwff npremwffs ki fs pos premi count)))
	 (es (ext-seq-general-inversion-1 nwff npremwffs ki nfs pos premi count)))
    (dolist (a (mapcar #'cons premwffs npremwffs))
      (unless (wffeq-ab (car a) (cdr a))
	(dotimes (i count)
	  (setq es (make-ext-seq-lambda-1 (car a) (cdr a) es)))))
    es))
    
(defun ext-seq-general-inversion-1 (pwff premwffs ki fs pos premi &optional (count 1))
  (if (> count 0)
      (let ((prwffs (ext-seq-princ-wffs fs))
	    (posr (ext-seq-pos-rule fs))
	    (k (ext-seq-kind fs))
	    (prems (ext-seq-prems fs)))
	(cond ((and (eq k ki)
		    (equal posr pos)
		    (wffeq-ab (car prwffs) pwff))
	       (ext-seq-general-inversion-1
		pwff premwffs ki (nth premi prems) pos premi (- count 1)))
	      ((and (eq k 'CONTR)
		    (equal posr pos)
		    (wffeq-ab (car prwffs) pwff))
	       (ext-seq-general-inversion-1
		pwff premwffs ki (car prems) pos premi (1+ count)))
	      ((and (eq k 'INIT) ; expand the init away and then try again
		    (or (wffeq-ab (car prwffs) pwff)
			(wffeq-ab (cadr prwffs) pwff)))
	       (ext-seq-general-inversion-1
		pwff premwffs ki
		(ext-seq-expand-init (cdar prwffs) (cadr prwffs)
				     (multiset-extract-wffs (ext-seq-wffs fs) (car prwffs) (cadr prwffs)))
		pos premi count))
	      (t
	       (let* ((prems2 (mapcar #'(lambda (x)
					  (ext-seq-general-inversion-1
					   pwff premwffs ki x pos premi count))
				      prems))
		      (es (copy-ext-seq fs))
		      (wffs (ext-seq-wffs fs))
		      (gam (remove-if #'(lambda (x) (wffeq-ab x pwff)) wffs :count count))
		      (wffs2 (append gam (apply #'append (make-list count :initial-element premwffs)))))
		 (setf (ext-seq-wffs es) wffs2)
		 (setf (ext-seq-prems es) prems2)
		 es))))
    fs))

(defun ext-seq-invert-internalize (wff0 fs pos &optional (count 1))
  (let* ((pwff (if pos wff0 (cons 'NOT wff0)))
	 (wff (externalize-wff1 wff0))
	 (ewff (if pos wff (cons 'NOT wff))))
    (if (wffeq-ab wff0 wff)
	fs
      (ext-seq-general-inversion pwff (list ewff) 'INTERNALIZE fs pos 0 count))))

(defun ext-seq-invert-truth-pos (fs &optional (count 1))
  (ext-seq-general-inversion 'TRUTH nil 'TRUE fs t 0 count))

(defun ext-seq-invert-truth-neg (fs &optional (count 1))
					; this call to general-inversion is a little misleading since
					; the TRUE rule never occurs negatively.
					; the effect is just to remove ~TRUTH as a side formula to every rule
  (ext-seq-general-inversion '(NOT . TRUTH) nil 'TRUE fs nil 0 count))

(defun ext-seq-invert-dis-pos (dis fs &optional (count 1))
  (ext-seq-general-inversion dis (list (cdar dis) (cdr dis)) 'DIS fs t 0 count))

(defun ext-seq-invert-dis-neg (ndis fs &optional (count 1))
  (list (ext-seq-invert-dis-neg-1 ndis fs count)
	(ext-seq-invert-dis-neg-2 ndis fs count)))

(defun ext-seq-invert-dis-neg-1 (ndis fs &optional (count 1))
  (ext-seq-general-inversion ndis (list (cons 'NOT (cdadr ndis))) 'DIS fs nil 0 count))

(defun ext-seq-invert-dis-neg-2 (ndis fs &optional (count 1))
  (ext-seq-general-inversion ndis (list (cons 'NOT (cddr ndis))) 'DIS fs nil 1 count))

(defun ext-seq-invert-neg (nnwff fs &optional (count 1))
  (ext-seq-general-inversion nnwff (list (cddr nnwff)) 'NEG fs nil 0 count))

(defun ext-seq-forall-to-funceq (es eqwff extwff pos)
  (if pos
      (ext-seq-forall-to-funceq-1 es (unabbreviated-type (cdr eqwff)) (bindvar extwff)
				  eqwff extwff pos)
    (ext-seq-forall-to-funceq-1 es (unabbreviated-type (cdr eqwff)) (bindvar extwff)
				(cons 'NOT eqwff) (cons 'NOT extwff) pos)))

(defun ext-seq-forall-to-funceq-1 (es tp x eqwff extwff pos &optional (count 1))
  (if (> count 0)
      (let ((princ-wffs (ext-seq-princ-wffs es))
	    (wffs (ext-seq-wffs es))
	    (k (ext-seq-kind es))
	    (prems (ext-seq-prems es)))
	(if (member extwff princ-wffs :test #'wffeq-ab)
	    (cond ((eq k 'FORALL)
		   (if pos
		       (let* ((sv (ext-seq-sel-var es))
			      (es2 (ext-seq-forall-to-funceq-1 (car prems) tp x eqwff extwff pos (- count 1))))
			 (make-ext-seq-extfunc
			  eqwff sv
			  (make-ext-seq-lambda
			   (acons (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))
				  (cons (cdar eqwff) sv) (cons (cdr eqwff) sv))
			   es2)))
		     (let* ((exp (ext-seq-exp-term es))
			    (es2 (ext-seq-forall-to-funceq-1 (car prems) tp x eqwff extwff pos (- count 1))))
		       (make-ext-seq-eqfunc
			(cdr eqwff) exp
			(make-ext-seq-lambda
			 (cons 'NOT (acons (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))
					   (cons (cdadr eqwff) exp) (cons (cddr eqwff) exp)))
			 es2)))))
		  ((eq k 'CONTR)
		   (make-ext-seq-contr eqwff
				       (ext-seq-forall-to-funceq-1 (car prems) tp x
								   eqwff extwff pos
								   (+ count 1))))
		  ((eq k 'LAMBDA)
		   (let* ((es2 (ext-seq-forall-to-funceq-1 (car prems) tp x
							   eqwff extwff pos
							   (- count 1)))
			  (eqnwff (etanorm (lambda-norm eqwff)))
			  (es3 (ext-seq-forall-to-funceq-1 es2 tp x eqnwff
							   (etanorm (lambda-norm extwff))
							   pos 1)))
		     (make-ext-seq-lambda-1 eqwff eqnwff es3)))
		  ((eq k 'REW)
		   (let* ((es2 (ext-seq-forall-to-funceq-1 (car prems) tp x eqwff extwff pos
							   (- count 1)))
			  (gam (multiset-extract-wffs wffs (car princ-wffs)))
			  (wffs2 (ext-seq-prems es))
			  (wffs3 (multiset-extract-wffs wffs2 gam))
			  (extwff3 (car wffs3))
			  (body (if pos (cdr extwff3) (cddr extwff3)))
			  (eqnwff3 (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))
					  (acons x 'LAMBDA (cdar body))
					  (acons x 'LAMBDA (cdr body))))
			  (eqnwff4 (if pos eqnwff3 (cons 'NOT eqnwff3)))
			  (es3 (ext-seq-forall-to-funceq-1 es2 tp x eqnwff4 extwff3 pos 1)))
		     (make-ext-seq-rew (if pos eqwff (cdr eqwff)) eqnwff3
				       'EQUIVWFFS pos es3)))
		  ((member k '(INIT REFL))
		   (ext-seq-forall-to-funceq-1
		    (ext-seq-expand-init-refl es)
		    tp x eqwff extwff pos count))
		  ((eq k 'CUT)
		   (throwfail "cannot change forall to func ext in the cut rule"))
		  (t (throwfail "bad princ wffs " princ-wffs " for rule " k)))
	  (let* ((prems2 (mapcar #'(lambda (y)
				     (ext-seq-forall-to-funceq-1
				      y tp x eqwff extwff pos count))
				 prems))
		 (gam (remove-if #'(lambda (y) (wffeq-ab y extwff)) wffs))
		 (wffs2 (append gam (make-list count :initial-element eqwff)))
		 (es2 (copy-ext-seq es)))
	    (setf (ext-seq-wffs es2) wffs2)
	    (setf (ext-seq-prems es2) prems2)
	    es2)))
    es))

; translation from ftree-seq to ext-seq
; delta - extra part of multiset
(defun ftree-seq-to-ext-seq (fs &optional delta)
  (let ((pos-wffs (ftree-seq-pos-wffs fs))
	(neg-wffs (ftree-seq-neg-wffs fs))
	(prem1 (ftree-seq-prem1 fs))
	(prem2 (ftree-seq-prem2 fs))
	(pos-rule (ftree-seq-pos-rule fs))
	(k (ftree-seq-kind fs)))
    (case k
      (INIT
       (make-ext-seq-init (car pos-wffs) delta))
      (CUT
       (let ((cutwff (car (ftree-seq-neg-wffs prem1)))
	     (gam1 (append (negate-wffs (ftree-seq-pos-wffs prem1))
			   (cdr (ftree-seq-neg-wffs prem1))))
	     (gam2 (append (negate-wffs (cdr (ftree-seq-pos-wffs prem2)))
			   (ftree-seq-neg-wffs prem2))))
	 (make-ext-seq-cut cutwff
			   (ftree-seq-to-ext-seq prem1 (append gam2 delta))
			   (ftree-seq-to-ext-seq prem2 (append gam1 delta)))))
      (MERGE
       (let ((mergewff (if pos-rule (cons 'NOT (car pos-wffs)) (car neg-wffs))))
	 (make-ext-seq-contr mergewff
			     (ftree-seq-to-ext-seq prem1 delta))))
      (FOCUS
       (ftree-seq-to-ext-seq prem1 delta))
      (WEAKEN
       (let ((weakenwff (if pos-rule (cons 'NOT (car pos-wffs)) (car neg-wffs))))
	 (ftree-seq-to-ext-seq prem1 (cons weakenwff delta))))
      (FALSE
       (make-ext-seq-false delta))
      (TRUE 
       (make-ext-seq-true delta))
      (DIS 
       (if pos-rule
	   (let ((gam1 (append (negate-wffs (cdr (ftree-seq-pos-wffs prem1)))
			       (ftree-seq-neg-wffs prem1)))
		 (gam2 (append (negate-wffs (cdr (ftree-seq-pos-wffs prem2)))
			       (ftree-seq-neg-wffs prem2)))
		 (dwff (car pos-wffs)))
	     (make-ext-seq-dis-neg
	      (cdar dwff) (cdr dwff)
	      (ftree-seq-to-ext-seq prem1 (append gam2 delta))
	      (ftree-seq-to-ext-seq prem2 (append gam1 delta))))
	 (let ((dwff (car neg-wffs)))
	   (make-ext-seq-dis-pos
	    (cdar dwff) (cdr dwff)
	    (ftree-seq-to-ext-seq prem1 delta)))))
      (CON
       (if pos-rule
	   (let ((cwff (car pos-wffs)))
	     (make-ext-seq-con-neg
	      (cdar cwff) (cdr cwff)
	      (ftree-seq-to-ext-seq prem1 delta)))
	 (let ((gam1 (append (negate-wffs (ftree-seq-pos-wffs prem1))
			     (cdr (ftree-seq-neg-wffs prem1))))
	       (gam2 (append (negate-wffs (ftree-seq-pos-wffs prem2))
			     (cdr (ftree-seq-neg-wffs prem2))))
	       (cwff (car neg-wffs)))
	   (make-ext-seq-con-pos
	    (cdar cwff) (cdr cwff)
	    (ftree-seq-to-ext-seq prem1 (append gam2 delta))
	    (ftree-seq-to-ext-seq prem2 (append gam1 delta))))))
      (IMP
       (if pos-rule
	   (let ((iwff (car pos-wffs))
		 (gam1 (append (negate-wffs (ftree-seq-pos-wffs prem1))
			       (cdr (ftree-seq-neg-wffs prem1))))
		 (gam2 (append (negate-wffs (cdr (ftree-seq-pos-wffs prem2)))
			       (ftree-seq-neg-wffs prem2))))
	     (make-ext-seq-imp-neg
	      (cdar iwff) (cdr iwff)
	      (ftree-seq-to-ext-seq prem1 (append gam2 delta))
	      (ftree-seq-to-ext-seq prem2 (append gam1 delta))))
	 (let ((iwff (car neg-wffs)))
	   (make-ext-seq-imp-pos
	    (cdar iwff) (cdr iwff)
	    (ftree-seq-to-ext-seq prem1 delta)))))
      (NEG
       (if pos-rule
	   (let ((pwff (car pos-wffs)))
	     (make-ext-seq-neg
	      (cdr pwff)
	      (ftree-seq-to-ext-seq prem1 delta)))
	 (ftree-seq-to-ext-seq prem1 delta)))
      (EXP
       (if pos-rule
	   (make-ext-seq-forall-neg (car pos-wffs)
				    (ftree-seq-exp-term fs)
				    (ftree-seq-to-ext-seq prem1 delta))
	 (make-ext-seq-exists-pos (car neg-wffs)
				  (ftree-seq-exp-term fs)
				  (ftree-seq-to-ext-seq prem1 delta))))
      (SEL
       (if pos-rule
	   (make-ext-seq-exists-neg (car pos-wffs)
				    (ftree-seq-sel-var fs)
				    (ftree-seq-to-ext-seq prem1 delta))
	 (make-ext-seq-forall-pos (car neg-wffs)
				  (ftree-seq-sel-var fs)
				  (ftree-seq-to-ext-seq prem1 delta))))
      (REW
       (let* ((wff1 (if pos-rule (car pos-wffs) (car neg-wffs)))
	      (rj (ftree-seq-rew-just fs))
;	      (es2 (unless (and (eq rj 'REFL=) (not pos-rule))
;		     (ftree-seq-to-ext-seq prem1 delta)))
	      (prem-pos-wffs (ftree-seq-pos-wffs prem1))
	      (prem-neg-wffs (ftree-seq-neg-wffs prem1))
	      (wff2 (if pos-rule (car prem-pos-wffs) (car prem-neg-wffs)))
	      (otherwffs (append (mapcar #'(lambda (x) (cons 'NOT x))
					 (if pos-rule (cdr pos-wffs) pos-wffs))
				 (if pos-rule neg-wffs (cdr neg-wffs)))))
	 (cond ((eq rj 'EQUIV-IMPLICS)
		(if pos-rule
		    (let* ((lwff (cdar wff1))
			   (rwff (cdr wff1))
			   (impwff1 (acons 'IMPLIES lwff rwff))
			   (impwff2 (acons 'IMPLIES rwff lwff))
			   (andwff (acons 'AND impwff1 impwff2))
			   (ornimpwff (acons 'OR (cons 'NOT impwff1) (cons 'NOT impwff2)))
			   (orwff1 (acons 'OR (cons 'NOT lwff) rwff))
			   (orwff2 (acons 'OR (cons 'NOT rwff) lwff))
			   (orwff3 (acons 'OR lwff (cons 'NOT rwff)))
			   (es2 (ftree-seq-to-ext-seq prem1 delta))
			   (es-eand (ext-seq-invert-internalize andwff es2 nil))
			   (es-or (ext-seq-invert-neg (cons 'NOT (cons 'NOT ornimpwff)) es-eand))
			   (es-dor (ext-seq-invert-dis-pos ornimpwff es-or))
			   (es-or1 (ext-seq-invert-internalize impwff1 es-dor nil))
			   (es-or2 (ext-seq-invert-internalize impwff2 es-or1 nil))
			   (es-or21 (ext-seq-invert-dis-neg-1 (cons 'NOT orwff2) es-or2))
			   (es-or22 (ext-seq-invert-dis-neg-2 (cons 'NOT orwff2) es-or2))
			   (es-or3 (make-ext-seq-dis-neg (cdar orwff3) (cdr orwff3) es-or22 es-or21))
			   (es-or4 (make-ext-seq-dis-pos (cons 'NOT orwff1) (cons 'NOT orwff3) es-or3))
			   (es-neg (make-ext-seq-neg (acons 'OR (cons 'NOT orwff1) (cons 'NOT orwff3)) es-or4)))
		      (make-ext-seq-internalize wff1 es-neg nil))
		  (let* ((lwff (cdar wff1))
			 (rwff (cdr wff1))
			 (impwff1 (acons 'IMPLIES lwff rwff))
			 (impwff2 (acons 'IMPLIES rwff lwff))
			 (orwff1 (acons 'OR (cons 'NOT lwff) rwff))
			 (orwff2 (acons 'OR (cons 'NOT rwff) lwff))
			 (orwff3 (acons 'OR lwff (cons 'NOT rwff)))
			 (andwff (acons 'AND impwff1 impwff2))
			 (nonwff (cons 'NOT (acons 'OR (cons 'NOT impwff1) (cons 'NOT impwff2))))
			 (es2 (ftree-seq-to-ext-seq prem1 delta))
			 (es-or (ext-seq-invert-internalize andwff es2 t))
			 (es-nimp1 (ext-seq-invert-dis-neg-1 nonwff es-or))
			 (es-nimp2 (ext-seq-invert-dis-neg-2 nonwff es-or))
			 (es-imp1 (ext-seq-invert-neg (cons 'NOT (cons 'NOT impwff1)) es-nimp1))
			 (es-imp2 (ext-seq-invert-neg (cons 'NOT (cons 'NOT impwff2)) es-nimp2))
			 (es-or1 (ext-seq-invert-internalize impwff1 es-imp1 t))
			 (es-or2 (ext-seq-invert-internalize impwff2 es-imp2 t))
			 (es-dor2 (ext-seq-invert-dis-pos orwff2 es-or2))
			 (es-or3 (make-ext-seq-dis-pos (cdar orwff3) (cdr orwff3) es-dor2))
			 (es2 (make-ext-seq-neg orwff1 es-or1))
			 (es3 (make-ext-seq-neg orwff3 es-or3))
			 (es4 (make-ext-seq-dis-neg (cons 'NOT orwff1) (cons 'NOT orwff3) es2 es3)))
		    (make-ext-seq-internalize wff1 es4 t))))
	       ((and (member rj '(LAMBDA BETA ETA))
		     (wffeq-ab (etanorm (lambda-norm wff1)) wff2))
		(let ((es2 (ftree-seq-to-ext-seq prem1 delta)))
		  (if pos-rule
		      (make-ext-seq-lambda-1 (cons 'NOT wff1) (cons 'NOT wff2) es2)
		    (make-ext-seq-lambda-1 wff1 wff2 es2))))
	       ((eq rj 'EXT=)
		(unless (equals-p wff1)
		  (throwfail "Bad ftree-seq EXT= rewrite" fs " : " (wff1 . gwff)))
		(let* ((nwff2 (etanorm (lambda-norm wff2)))
		       (nwff1 (etanorm (lambda-norm wff1)))
		       (es2 (ftree-seq-to-ext-seq prem1 delta))
		       (lft (cdar nwff1))
;		       (rght (cdr nwff1))
		       (tp (unabbreviated-type lft)))
		  (if (consp tp)
		      (make-ext-seq-lambda-1
		       wff1 nwff1
		       (ext-seq-forall-to-funceq
			(ext-seq-invert-lambda-1 wff2 nwff2 es2)
			nwff1 nwff2 (not pos-rule)))
		    (if (eq tp 'O)
			(if pos-rule
			    (let* ((es3 (ext-seq-invert-internalize nwff2 es2 nil))
				   (nwff3 (externalize-wff1 nwff2))
				   (es4 (ext-seq-invert-neg (cons 'NOT nwff3) es3))
				   (es5 (ext-seq-invert-neg (cdr nwff3) es4))
				   (ndis1 (cdadr nwff3))
				   (ndis2 (cddr nwff3))
				   (es6 (ext-seq-invert-dis-neg-1 ndis1 es5))
				   (es7 (ext-seq-invert-dis-neg-2 ndis2 es6))
				   (es8 (ext-seq-invert-neg (cons 'NOT (cdadr ndis1)) es7))
				   (es9 (ext-seq-invert-neg (cons 'NOT (cddr ndis2)) es8))
				   (es10 (ext-seq-invert-dis-neg-2 ndis1 es5))
				   (es11 (ext-seq-invert-dis-neg-1 ndis2 es10)))
			      (make-ext-seq-eqo wff1 es9 es11))
			  (let* ((es3 (ext-seq-invert-internalize nwff2 es2 t))
				 (nwff3 (externalize-wff1 nwff2))
				 (dis1 (cdadr nwff3))
				 (dis2 (cddr nwff3))
				 (es4 (ext-seq-invert-dis-neg-1 nwff3 es3))
				 (es5 (ext-seq-invert-dis-neg-2 nwff3 es3))
				 (es6 (ext-seq-invert-neg (cons 'NOT dis1) es4))
				 (es7 (ext-seq-invert-neg (cons 'NOT dis2) es5))
				 (es8 (ext-seq-invert-dis-pos (cdr dis1) es6))
				 (es9 (ext-seq-invert-dis-pos (cdr dis2) es7)))
			    (make-ext-seq-lambda-1
			     wff1 nwff1
			     (make-ext-seq-exto nwff1 es8 es9))))
		      (throwfail "Bad ftree-seq EXT= rewrite" fs " : " (wff1 . gwff))))))
	       ((eq rj 'LEIBNIZ=)
		(unless (equals-p wff1)
		  (throwfail "Bad ftree-seq EXT= rewrite" fs " : " (wff1 . gwff)))
		(if pos-rule
		    (let* ((es2 (ftree-seq-to-ext-seq prem1 (cons (cons 'NOT wff1) delta)))
			   (nwff1 (etanorm (lambda-norm wff1)))
			   (nwff2 (etanorm (lambda-norm wff2)))
			   (es21 (ext-seq-invert-lambda-1 wff2 nwff2 es2))
			   (es3 (ext-seq-leibniz-neg nwff1 nwff2 (append delta otherwffs))))
		      (make-ext-seq-lambda-1
		       (cons 'NOT wff1) (cons 'NOT nwff1)
		       (make-ext-seq-cut nwff2 es3 es21)))
		  (let* ((es2 (ftree-seq-to-ext-seq prem1 (cons wff1 delta)))
			 (nwff1 (etanorm (lambda-norm wff1)))
			 (nwff2 (etanorm (lambda-norm wff2)))
			 (es21 (ext-seq-invert-lambda-1 wff2 nwff2 es2))
			 (es3 (ext-seq-leibniz-pos nwff1 nwff2 (append delta otherwffs))))
		    (make-ext-seq-lambda-1
		     wff1 nwff1
		     (make-ext-seq-cut nwff2 es21 es3)))))
	       ((eq rj 'REFL=)
		(unless (and (wffeq wff2 'TRUTH) (equals-p wff1)
			     (wffeq-ab (cdar wff1) (cdr wff1)))
		  (throwfail "Bad ftree-seq REFL= rewrite" fs " : " (wff1 . gwff) " to " (wff2 . gwff)))
		(if pos-rule
		    (let* ((es2 (ftree-seq-to-ext-seq prem1 (cons (cons 'NOT wff1) delta))))
		      (ext-seq-invert-truth-neg es2))
		  (make-ext-seq-refl-ab wff1 (append delta otherwffs))))
	       (t
		(let* ((es2 (ftree-seq-to-ext-seq prem1 delta)))
		  (ext-seq-equivwffs (acons (if pos-wffs (cons 'NOT wff2) wff2)
					    (if pos-wffs (cons 'NOT wff1) wff1)
					    (mapcar #'(lambda (x)
							(cons x x))
						    (append otherwffs delta)))
				     es2))))))
      (t (throwfail "Unrecognized ftree-seq rule " k " in " fs)))))

; unfinished
; converting to ND proof
;(defun ext-seq-to-proof-rec (es lines conc)
;  (unless (eq conc (caar (proof-plans dproof)))
;    (subproof conc))
;  (let ((ln (linealias conc))
;	(princ-wffs (ext-seq-princ-wffs es))
;	(pos-rule (ext-seq-pos-rule es)))
;    (case (ext-seq-kind es)
;      (INIT
;       (let* ((nwff (car princ-wffs))
;	      (pwff (cadr princ-wffs))
;	      (nl (assoc nwff neg-lines))
;	      (pl (assoc pwff neg-lines))
;	      )
;	 (if (eq (cdr nl) 'CONC)
;	     ()
;	   (if (eq (cdr pl) 'CONC)
;	       ()
;	     (let ((nline (cdr nl))
;		   (pline (cdr pl)))
;	       ())))))
;	 ))
;      (TRUE
;       ))))

;(defun ext-seq-to-proof-indirect (es lines conc)
;  )

; unfinished
;(defun ext-seq-to-ftree-seq (es)
;  (ext-seq-to-ftree-seq-1 es nil (mapcar #'(lambda (x) (cons x x)) (ext-seq-wffs es))))

; *-wffs-a controls the order of the output and the sides of the sequents
;(defun ext-seq-to-ftree-seq-1 (es pos-wffs-a neg-wffs-a)
;  (let ((princ-wffs (ext-seq-princ-wffs es))
;	(prems (ext-seq-prems es))
;	(pos-rule (ext-seq-pos-rule es))
;	(k (ext-seq-kind es)))
;    (ftree-seq-order-by
;     (case k
;       (TRUE
;	(let ((a (assoc 'TRUTH neg-wffs-a)))
;	  (unless a
;	    (throwfail "TRUTH rule problem translating from ext-seq to ftree-seq" es))
;	  (ftree-seq-weaken-by *ftree-seq-true* pos-wffs (remove 'TRUTH neg-wffs :count 1))))
;       (NEG
;	(if (member (car princ-wffs) neg-wffs :test #'wffeq-ab)
;	    (let ((pos-wffs2 (cons (cdar princ-wffs) pos-wffs))
;		  (neg-wffs2 (multiset-extract-wffs neg-wffs (car princ-wffs))))
;	      (make-ftree-seq-neg-neg
;	       (ext-seq-to-ftree-seq-1 (car prems) pos-wffs2 neg-wffs2)))
;	  (let ((pos-wffs2 (multiset-extract-wffs pos-wffs (cdar princ-wffs)))
;		(neg-wffs2 (cons (cdar princ-wffs) neg-wffs)))
;	    (make-ftree-seq-neg-pos
;	     (ext-seq-to-ftree-seq-1 (car prems) pos-wffs2 neg-wffs2)))))
;       (DIS
;	(if pos-rule
;	    (let ((neg-wffs2 (cons (cdaar princ-wffs)
;				   (cons (cdar princ-wffs)
;					 (multiset-extract-wffs pos-wffs (car princ-wffs))))))
;;	      (make-ftree-seq-dis-neg
;;	       (ext-seq-to-ftree-seq-1 
;;		))
;	)))
;       (FORALL )
;       (INTERNALIZE )
;       (LAMBDA )
;       (INITEQ )
;       (DEC )
;       (EUNIF1 )
;       (EUNIF2 )
;       (EQO )
;       (EQFUNC )
;       (EXTO )
;       (EXTFUNC )
;       (REW )
;       (CUT )
;       (CONTR )
;       (INIT )
;       (REFL ))
;     (mapcar #'cdr pos-wffs-a)
;     (mapcar #'cdr neg-wffs-a))))
