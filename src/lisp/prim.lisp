;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of PRIMITIVE-SUBST)

;;;
;;; File: PRIM
;;; Functions for generating primitive substitutions.

(deffile prim
  (part-of PRIMITIVE-SUBST)
  (extension clisp)
  (mhelp "Basic primitive-substitution functions."))

(context primsubs)


(defvar prim-logconst '(and or))
(defvar prim-binders '(exists forall))
(defvar prim-hashtable (make-hash-table :test #'equal))

(defflag pr97c-prenex
  (flagtype boolean)
  (default t)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 important transmit)
  (mhelp "If T, PR97C generates substitutions in prenex normal form. If NIL, it doesn't."))

(defflag pr97c-max-abbrevs
  (flagtype posinteger)
  (default 1)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 important transmit)
  (mhelp "The maximum number of abbreviations that may appear in a PR97C primsub."))

(defflag min-prim-depth
  (flagtype posinteger)
  (default 1)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 important transmit)
  (relevancy-preconditions
   (primsub-method (not (member primsub-method '(pr89 pr00)))))
  (irrelevancy-preconditions
   (primsub-method (member primsub-method '(pr89 pr00))))
  (mhelp "Minimum depth at which primsubs with quantifiers are generated.
The types of the quantified variables range over the values in PRIM-BDTYPES.
With PRIMSUB-METHOD PR89 : 
 This flag is ignored. Primsubs of the form \"exists x . literal\" and 
 \"forall x . literal\" will be generated.
With PRIMSUB-METHOD PR93 :
 At depth 1, a single quantifier is introduced, as in PR89. 
 At depth N>1, we have (N-1) quantifiers ranging over a formula
 containing (N-1) conjunctions {disjunctions} of (N-2) 
 disjunctions {conjunctions}.
With PRIMSUB-METHOD PR95 :
 At depth 1, as in PR89.
 At depth N>1, we have (N-1) quantifiers ranging over a formula
 with between MIN-PRIM-LITS and MAX-PRIM-LITS literals, with
 all combinations of connectives between them.
With PRIMSUB-METHOD PR97 :
 At depth N>0, we have (N-1) quantifiers ranging over each 
 subformula taken from the etree which contains between 
 MIN-PRIM-LITS and MAX-PRIM-LITS literals. You can see these
 subformulas by doing NAME-PRIM from the MATE top level.
With PRIMSUB-METHOD PR97A :
 As in PR97, but all substitutions are in negation normal form.
With PRIMSUB-METHOD PR97B :
 The substitutions from PR97A and PR95 are interleaved. The order
 is determined firstly by the number of literals, then by the number of
 quantifiers, and lastly with PR97 substs taking precedence over PR95.
With PRIMSUB-METHOD PR97C :
 If set to N, the number of quantifiers in any primsub will be >= N-1.
With PRIMSUB-METHOD PR00  :
 The value is ignored."))

(defflag min-prim-lits
  (flagtype posinteger)
  (default 2)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 important transmit)
  (mhelp "Minimum no. of literals allowed in a primsub.
Does not apply for PRIMSUB-METHOD PR89 or PR93. 
See the help message for MIN-PRIM-DEPTH, which explains how primsubs
are generated."))

(defflag max-prim-lits
  (flagtype posinteger)
  (default 4)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 important transmit)
  (mhelp "Maximum no. of literals allowed in a primsub.
Does not apply for PRIMSUB-METHOD PR89 or PR93. 
See the help message for MIN-PRIM-DEPTH, which explains how primsubs
are generated."))

(defflag primsub-method
  (flagtype symbol)
  (default 'pr93)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (relevant-kids ((eq primsub-method 'pr97c) '(pr97c-prenex pr97c-max-abbrevs))
		 ((eq primsub-method 'PR00) '(pr00-num-iterations
					      pr00-max-substs-var pr00-require-arg-deps))
		 (t '(max-prim-depth max-prim-lits min-prim-depth min-prim-lits ms91-interleave
		      neg-prim-sub prim-quantifier primsub-var-select prim-bdtypes-auto)))
  (irrelevant-kids ((neq primsub-method 'pr97c) '(pr97c-prenex pr97c-max-abbrevs))
		   ((eq primsub-method 'PR00) '(max-prim-depth min-prim-depth))
		   ((neq primsub-method 'PR00) '(pr00-approximate-solns pr00-num-partial-solns pr00-num-iterations
						 pr00-max-substs-var pr00-require-arg-deps)))
  (subjects primsubs ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 important transmit)
  (mhelp "Takes one of the values PR89, PR93, PR95, PR97, PR97A, PR97B.
This determines how primsubs will be generated, in 
conjunction with MAX-PRIM-DEPTH, MIN-PRIM-DEPTH, 
MAX-PRIM-LITS and MIN-PRIM-LITS.
With PRIMSUB-METHOD PR89 : 
 Primsubs of the form \"exists x . literal\" and 
 \"forall x . literal\" will be generated.
With PRIMSUB-METHOD PR93 :
 For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
  At depth 1, a single quantifier is introduced, as in PR89. 
  At depth N>1, we have (N-1) quantifiers ranging over a formula
  containing (N-1) conjunctions {disjunctions} of (N-2) 
  disjunctions {conjunctions}.
With PRIMSUB-METHOD PR95 :
 For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
  At depth 1, as in PR89.
  At depth N>1, we have (N-1) quantifiers ranging over a formula
  with between MIN-PRIM-LITS and MAX-PRIM-LITS literals, with
  all combinations of connectives between them.
With PRIMSUB-METHOD PR97 :
 For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
  At depth N>0, we have (N-1) quantifiers ranging over each 
  subformula taken from the etree which contains between 
  MIN-PRIM-LITS and MAX-PRIM-LITS literals. You can see these
 subformulas by doing NAME-PRIM from the MATE top level. (Note:
 both the instantiated and uninstantiated versions of each 
 definition are used.)
With PRIMSUB-METHOD PR97A :
 As in PR97, but all substitutions are in negation normal form.
With PRIMSUB-METHOD PR97B :
 The substitutions from PR97A and PR95 are interleaved. The order
 is determined firstly by the number of literals, then by the number of
 quantifiers, and lastly with PR97 substs taking precedence over PR95.
With PRIMSUB-METHOD PR97C :
 Using the connectives AND and OR, and the quantifiers EXISTS and
 FORALL (ranging over variables of types PRIM-BDTYPES), and also using
 any abbreviations or equalities that occur in the gwff to be proven, 
 primsubs are built up using the bounds given by MIN- and MAX-PRIM-LITS
 and MIN- and MAX-PRIM-DEPTH. See also PR97C-PRENEX and PR97C-MAX-ABBREVS.
With PRIMSUB-METHOD PR00  :
 This uses higher order unification to determine set substitutions
 that solve part of the mating search in advance.  PR00 only works with 
 DEFAULT-MS MS98-1 and SKOLEM-DEFAULT NIL. PR00 can be controlled using the flags
 PR00-MAX-SUBSTS-VAR, PR00-REQUIRE-ARG-DEPS, PR00-NUM-ITERATIONS."))

(defflag max-prim-depth
  (flagtype posinteger)
  (default 1)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 important transmit)
  (mhelp "Maximum depth to which primsubs with quantifiers are generated.
The types of the quantified variables range over the values in PRIM-BDTYPES.
With PRIMSUB-METHOD PR89 : 
 This flag is ignored. Primsubs of the form \"exists x . literal\" and 
 \"forall x . literal\" will be generated.
With PRIMSUB-METHOD PR93 :
 At depth 1, a single quantifier is introduced, as in PR89. 
 At depth N>1, we have (N-1) quantifiers ranging over a formula
 containing (N-1) conjunctions {disjunctions} of (N-2) 
 disjunctions {conjunctions}.
With PRIMSUB-METHOD PR95 :
 At depth 1, as in PR89.
 At depth N>1, we have (N-1) quantifiers ranging over a formula
 with between MIN-PRIM-LITS and MAX-PRIM-LITS literals, with
 all combinations of connectives between them.
With PRIMSUB-METHOD PR97 :
 At depth N>0, we have (N-1) quantifiers ranging over each 
 subformula taken from the etree which contains between 
 MIN-PRIM-LITS and MAX-PRIM-LITS literals. You can see these
 subformulas by doing ETP from the MATE top level.
With PRIMSUB-METHOD PR97A :
 As in PR97, but all substitutions are in negation normal form.
With PRIMSUB-METHOD PR97B :
 The substitutions from PR97A and PR95 are interleaved. The order
 is determined firstly by the number of literals, then by the number of
 quantifiers, and lastly with PR97 substs taking precedence over PR95.
With PRIMSUB-METHOD PR97C :
 If set to N, all primsubs will have < N quantifiers.
With PRIMSUB-METHOD PR00  :
 This is ignored."))

(defun ini-prim-hashtable ()
  (clrhash prim-hashtable))

(defun find-prim-substs (var &optional (h-var-prefix nil))
  (prim-substs var h-var-prefix))

(defun prim-substs (var &optional (h-var-prefix nil))
  (unless (eq primsub-method 'pr00)	; pr00 is handled completely differently, if this is called with pr00, just return nil
					; pr00 builds instantiated & deepened etrees (actually ftrees) with assumed connections.
					; these are put into the global *pr00-setsubs* (see pr00.lisp)
					; this is done in a pre-processing stage by the relevant search procedure (for now, just ms98-1)
					; or by name-prim if we call name-prim from mate.
    (let* ((type-var (type var))
	   (substs (gethash type-var prim-hashtable))
	   (h-var-prefix (or h-var-prefix (string-downcase (nameroot var))))
	   (alpha-length 
	    ;; changed to conform with superscripted vars.
	    ;; see wffprim.lisp DAN 31OCT90
	    (1- (last-^-with-int h-var-prefix))
	    #+old(position-if-not
		  #'(lambda (x)
		      (member (string x) '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
			      :test #'string=))
		  h-var-prefix :from-end t)))
      (setq h-var-prefix (intern (subseq h-var-prefix 0 (1+ alpha-length))))
      (if substs (mapcar #'rename-prim-subst substs)
	(let* ((typevector-var (listify-type type-var))
	       (length (1- (length typevector-var)))
	       (resulttype (aref typevector-var length))
	       (w-vars (create-new-w-vars typevector-var length))
	       (prim-binders (if prim-quantifier '(exists forall) nil))
	       (max-prim-depth (if (memq primsub-method '(pr97 pr97a pr97b pr97c)) (1+ max-prim-depth) max-prim-depth)))
	  (do ((i 0 (1+ i)))
	      ((= i length))
	    (rd-string-bound (princ-to-string (aref w-vars i))))
	  (setq substs
	    (cond ((eq primsub-method 'pr97c)
		   (make-deep-subfm-pr97c resulttype w-vars h-var-prefix))
		  (t
		   (nconc
		    (prim-const resulttype prim-logconst w-vars length
				typevector-var)
		    (prim-projections w-vars length typevector-var)
		    (if (or (eq primsub-method 'pr89)
			    (and (not (memq primsub-method '(pr97 pr97a pr97b pr97c))) (< min-prim-depth 2) (> max-prim-depth 0)))
			(prim-binder resulttype prim-binders w-vars length typevector-var h-var-prefix))
		    (if (neq primsub-method 'pr89)
			(do ((count (if (< min-prim-depth 2) 2 min-prim-depth) (1+ count))
			     (bds nil (nconc bds (make-deep-prims resulttype prim-binders w-vars length typevector-var h-var-prefix count))))
			    ((> count max-prim-depth) bds)))))))
	  (unless (member primsub-method '(pr97c))
	    (let ((blanklist nil))
	      (declare (special blanklist))
	      (setq substs (reverse (remove-duplicates (reverse substs)
						       :test 'wffeq-ab 
						       :key #'(lambda (x) (blank-out-parts-of (subst-term x))))))))
	  (setf (gethash type-var prim-hashtable) substs))))))


(defun rename-prim-subst (subst)
  (let ((new-h-vars
	 (mapcar #'(lambda (h-var) (ren-var-uni-ho h-var-prefix (type h-var)))
		 (subst-h-vars subst))))
    (dolist (v new-h-vars) (rd-string-bound (princ-to-string v)))
    (dolist (v (subst-h-vars subst)) (rd-string-bound (princ-to-string v)))
    (if new-h-vars
	(make-subst
	 :term
	 (if (cdr new-h-vars)
	     (simul-substitute-term-var
	      (mapcar #'cons (subst-h-vars subst) new-h-vars)
	      (subst-term subst))
	     (substitute-term-var (car new-h-vars) (car (subst-h-vars subst))
				  (subst-term subst)))
	 :h-vars new-h-vars :type (subst-type subst))
	subst)))

(defun prim-const (resulttype logconsts w-vars length type-var)
  (declare (special neg-prim-sub))
  (let ((substs nil))
    (when neg-prim-sub
	  (setq logconsts (cons 'not logconsts)))
    (dolist (logconst logconsts (nreverse substs))
      (let ((type (listify-type (type logconst))))
	(if (eq (aref type (1- (length type))) resulttype)
	    (push
	     (construct-imitation-term "PrimConst" logconst w-vars type-var
				       type length (1- (length type)))
	     substs))))))

(defun prim-projections (w-vars length type-var)
  (projections-eta w-vars type-var length))

(defun prim-binder (resulttype binders w-vars length type-var prim-var-prefix)
  (declare (special prim-bdtypes))
  (when (and (eq resulttype 'o) (not (null prim-bdtypes)))
	(let ((substs nil))
	  (dolist (type prim-bdtypes (nreverse substs))
		  (let ((bd-var (ren-var-uni-ho w-var-prefix type))
			(type (construct-type (cons 'o type) type-var length)))
		    (rd-string-bound (princ-to-string bd-var))
		    (dolist (binder binders)
			    (let ((head (ren-var-uni-ho prim-var-prefix type)))
			      (rd-string-bound (princ-to-string head))
			      (push 
			       (make-subst
				:term (prefix-lambda
				       (acons bd-var binder
					      (cons (construct-term
						     head w-vars length) bd-var))
				       w-vars length)
				:h-vars (list head) :type (if prim-quantifier "PrimQuant" "GenSub"))
			       substs))))))))

(defun make-deep-prims (resulttype binders w-vars length type-var prim-var-prefix depth)
  (case primsub-method
	(pr95
	 (make-deep-prims-new resulttype binders w-vars length type-var prim-var-prefix depth))
	(pr93
	 (make-deep-prims-old resulttype binders w-vars length type-var prim-var-prefix depth))
	(pr97
	 (make-deep-subfm-prims resulttype binders w-vars length type-var prim-var-prefix depth))
	(pr97a
	 (make-deep-subfm-prims resulttype binders w-vars length type-var prim-var-prefix depth t))
	(pr97b
	 (let* ((max-prim-depth (1- max-prim-depth))
		(substs (mapcar #'(lambda (x) (label-subst x 1))
				(make-deep-prims-new resulttype binders w-vars length type-var prim-var-prefix depth)))
		(max-prim-depth (1+ max-prim-depth))
		(substs (append substs 
				(mapcar #'(lambda (x) (label-subst x 0))
					(make-deep-subfm-prims resulttype binders w-vars 
							       length type-var prim-var-prefix depth t))))
		(blanklist nil))
	   (declare (special blanklist))
	   (mapcar 'cdr (sort (remove-duplicates substs :test 'wffeq-ab 
						 :key #'(lambda (x) (blank-out-parts-of (subst-term (cdr x)))))
			      'lex3 :key 'car))))
	(t
	 (throwfail "I don't know what PRIMSUB-METHOD " primsub-method " is." t))))

(defun lex3 (l1 l2)
  (or (< (car l1) (car l2))
      (and (= (car l1) (car l2))
	   (< (cadr l1) (cadr l2)))
      (and (= (car l1) (car l2))
	   (= (cadr l1) (cadr l2))
	   (< (caddr l1) (caddr l2)))))

(defun label-subst (subst int)
  (let (blanklist)
  (declare (special blanklist))
  (let ((gwff (blank-out-parts-of (subst-term subst)))
	(blankcount 0) (dummycount 0))
    (declare (special blankcount dummycount))
    (count-blanks gwff blanklist)
    (cons (list blankcount dummycount int) subst))))

(defun not-useless (gwff)
  (cond ((weak-label-p gwff) (not-useless (get gwff 'represents)))
	((lsymbol-q gwff) (and (neq gwff 'NOT) (or (logconst-q gwff)
						   (and (get gwff 'core::stands-for)
							(not-useless (get gwff 'core::stands-for)))
						   (and (abbrev-q gwff)
							(not-useless (get gwff 'defn)))
						   (pmpropsym-q gwff))))
	((boundwff-q gwff) (if (eq (binder gwff) 'lambda)
			       (not-useless (cdr gwff))
			     t))
	(t (or (not-useless (gar gwff))
	       (not-useless (gdr gwff))))))

(defun restyp (type)
  (if (symbolp type) (eq type 'O)
    (restyp (car type))))

(defun get-useful-subformulas (gwff &optional (nnf nil))
  (let ((subfms (mapcar #'(lambda (x) (cons (car x) (cddr x)))
			(remove-if-not #'(lambda (x) (and (not-useless (cddr x)) (restyp (car x))))
				       (if (etree-p gwff) (subfms-of-etree gwff)
					 (core::make-subformula-list gwff)))))
	(blanklist nil)
	newsubfms)
    (declare (special blanklist))
    (dolist (subfm subfms)
	    (let ((newfm (lnorm (blank-out-parts-of (if nnf (neg-norm (cdr subfm)) (cdr subfm)))))
		  (blankcount 0)
		  (dummycount 0))
	      (declare (special blanklist blankcount dummycount))
	      (count-blanks newfm blanklist)
	      (push (cons (cons blankcount (car subfm)) newfm) newsubfms)))
    (setq subfms (remove-duplicates newsubfms :key 'cdr :test 'wffeq-ab))
    (setq newsubfms nil)
    (dolist (subfm subfms)
	    (if (> (caar subfm) 2)
		(push (mapcar #'(lambda (x) (cons (cons (count-blanks2 x blanklist) (cdar subfm)) x))
			      (amalgamate-blanks (cdr subfm)))
		      newsubfms)
	      (push (list subfm) newsubfms)))
    (setq newsubfms (remove-if-not #'(lambda (x) (and (>= (caar x) min-prim-lits) (<= (caar x) max-prim-lits)))
				   (apply 'append newsubfms)))
    (setq newsubfms (remove-duplicates newsubfms :key 'cdr :test 'wffeq-ab))
    (values blanklist (sort newsubfms #'(lambda (x y) (or (< (car x) (car y))
							  (and (= (car x) (car y))
							       (< (cdr x) (cdr y)))))
			    :key #'(lambda (x) (count-blanks2 (cdr x) blanklist t))))))

(defun count-blanks2 (gwff blanks &optional (quant nil))
  (let ((blankcount 0) (dummycount 0))
  (declare (special blankcount dummycount))
  (count-blanks gwff blanks)
  (if quant (cons dummycount blankcount) blankcount)))

(defun amalgamate-blanks (gwff)
  (declare (special blanklist))
  (let ((newfms (amalg-blanks-real gwff)))
    (remove-if-not #'not-useless newfms)))

(defun amalg-blanks-real (gwff)
  (declare (special blanklist))
  (cond ((lsymbol-q gwff) (if (not-useless gwff)
			      (list gwff) (list (make-blank (type gwff)))))
	((boundwff-q gwff) (cons (make-blank (type gwff))
				 (if (> (count-blanks2 (cdr gwff) blanklist) 1)
				     (cons
				      (cons (car gwff) (make-blank (type (cdr gwff))))
				      (mapcar #'(lambda (x) (cons (car gwff) x))
					      (amalg-blanks-real (cdr gwff))))
				   (mapcar #'(lambda (x) (cons (car gwff) x))
					   (amalg-blanks-real (cdr gwff))))))
	((infix-p gwff)
	 (cons (make-blank (type gwff))
	       (append (mapcar #'(lambda (x) (cons (cons (caar gwff) x) (cdr gwff)))
			       (amalg-blanks-real (cdar gwff)))
		       (mapcar #'(lambda (x) (cons (car gwff) x)) (amalg-blanks-real (cdr gwff))))))
	(t
	 (if (> (count-blanks2 gwff blanklist) 1)
	     (cons (make-blank (type gwff))
		   (append (mapcar #'(lambda (x) (cons x (cdr gwff))) (amalg-blanks-real (car gwff)))
			   (mapcar #'(lambda (x) (cons (car gwff) x)) (amalg-blanks-real (cdr gwff)))))
	   (append (mapcar #'(lambda (x) (cons x (cdr gwff))) (amalg-blanks-real (car gwff)))
		   (mapcar #'(lambda (x) (cons (car gwff) x)) (amalg-blanks-real (cdr gwff))))))))

(defun subfms-of-etree (etree)
  (let ((subfmlist nil))
    (declare (special subfmlist))
    (subfms-of-etree-real etree)
    (setq subfmlist (remove-duplicates subfmlist :test #'core::same-subfmla))
    subfmlist))

(defun suppress-details (gwff)
  (cond ((skolem-term-p gwff) (suppress-details (skolem-term-term gwff)))
	((exp-var-p gwff) (suppress-details (exp-var-subst gwff)))
	((lsymbol-q gwff) gwff)
	(t (cons (suppress-details (car gwff)) (suppress-details (cdr gwff))))))

(defun subfms-of-etree-real (etree)
  (declare (special subfmlist))
  (let ((newitem (suppress-details (get-shallow etree))))
    (push (cons 'O (cons (free-vars-of newitem) newitem)) subfmlist)
    (when (rewrite-p etree)
	  (setq subfmlist (append (core::make-subformula-list newitem) subfmlist))))
  (if (etree-components etree)
      (dolist (son (etree-components etree))
	      (subfms-of-etree-real son))
    (let ((newlist (core::make-subformula-list (suppress-details (get-shallow etree)))))
      ;bug: things like A(I) =(OII) B(I) always become A(O) =(OOO) B(O)
      (setq subfmlist (append newlist subfmlist)))))

(defun count-blanks (gwff blanks)
  ;always returns NIL
  (declare (special blankcount dummycount))
  (cond ((weak-label-p gwff) (count-blanks (get gwff 'represents) blanks))
	((lsymbol-q gwff) (when (assoc gwff blanks) (incf blankcount)) nil)
	((boundwff-q gwff) (incf dummycount) (count-blanks (cdr gwff) blanks))
	(t (count-blanks (car gwff) blanks) (count-blanks (cdr gwff) blanks))))

(defun blank-out-parts-of (gwff)
  (declare (special blanklist))
  (cond ((weak-label-p gwff) (blank-out-parts-of (get gwff 'represents)))
	((lsymbol-q gwff) (if (not-useless gwff)
			      gwff (make-blank (type gwff))))
	((boundwff-q gwff) ;(if (not-useless gwff)
			       (cons (car gwff) ;this leaves a vacuous bound variable we have to deal with...
				     (if (not-useless (cdr gwff)) 
					 (blank-out-parts-of (cdr gwff))
				       (make-blank (type (cdr gwff))))))
			    ; (make-blank (type gwff))))
	(t (if (and (neq (car gwff) 'not) (not-useless gwff))
	       (cons (if (not-useless (car gwff)) 
			 (blank-out-parts-of (car gwff))
		       (make-blank (type (car gwff))))
		     (if (not-useless (cdr gwff)) 
			 (blank-out-parts-of (cdr gwff))
		       (make-blank (type (cdr gwff)))))
	     (make-blank (type gwff))))))

(defun make-blank (type)
  (declare (special blanklist))
  (if (rassoc type blanklist :test 'equal)
      (car (rassoc type blanklist :test 'equal))
    (let ((newblank (getrwff (concatenate 'string (princ-to-string (gensym)) (core::type-to-string-2 type)))))
      (push (cons newblank type) blanklist)
      newblank)))

(defun type-to-string-3 (tp)
  (with-open-stream (string-stream (make-string-output-stream))
    (type-to-lib-stream tp string-stream)
    (get-output-stream-string string-stream)))

(defun vector-final-seg (v1 v2)
  (do ((a1 (1- (length v1)) (1- a1))
       (a2 (1- (length v2)) (1- a2)))
      ((or (= a1 -1)
	   (neq (aref v1 a1) (aref v2 a2)))
       (= a1 -1))))

(defun drop-w (type-base type-var w-vars)
  ;from a basic formula of type TYPE-BASE, we abstract w-vars until we reach something of
  ;type TYPE-VAR
  (if (or (> (length type-base) (length type-var))
	  (not (vector-final-seg type-base type-var)))
	  'fail
    (if (= (length type-base) (length type-var))
	nil
      (do ((ar 0 (1+ ar)))
	  ((or (= ar (- (length type-var) (length type-base)))
	       (not (equal (type (aref w-vars ar)) (aref type-var ar))))
	   (if (= ar (- (length type-var) (length type-base)))
	       (make-array (- (length type-var) (length type-base))
			   :fill-pointer t
			   :initial-contents (do ((c (1- (- (length type-var) (length type-base))) (1- c))
						  (list nil))
						 ((= c -1) list)
						 (push (aref w-vars c) list)))
	       'fail))))))

(defun make-deep-subfm-prims (resulttype binders w-vars length type-var prim-var-prefix depth &optional (nnf nil))
  (declare (special prim-bdtypes) (ignore length))
  (when (and (eq resulttype 'o) (not (null prim-bdtypes)))
	(multiple-value-bind (blanks subfms) 
			     (get-useful-subformulas (eproof-etree current-eproof) nnf)
			     ;newsubfms has the form of a list of (n . m) . gwff
			     ;where n is the number of blanks to be filled
			     ; and m is the number of (currently) vacuous quantifiers to be fixed
			     ; and gwff is a blanked-out formula.
	  (let ((substs-list nil))
	  (dolist (typelist (if (= 2 depth) (list nil) 
			      (binderlistlist prim-bdtypes (1- (1- depth)))) 
			    (nreverse substs-list))
		  (let* ((bd-var-list (if (= 2 depth) nil (make-n-new-heads w-var-prefix typelist)))
			 (hvarlist nil))
		    (declare (special hvarlist))
		    (dolist (b bd-var-list) (rd-string-bound (princ-to-string b)))
		    ;at this point, bd-var-list is a list of w-vars to be bound by the 
		    ;list of foralls and exists is binderlist
		    ;w-vars is a list of variables to be lambda-bound
		    (dolist (binderlist (if (= 2 depth) (list nil) (binderlistlist binders (1- (1- depth)))))
		    (dolist (subfm subfms)
			    (when (or (null binderlist) (eq (cdar subfm) 'O))
				  ;if there are going to be quantifiers, we want fmlas of type O
				  ;otherwise we'll fix the w-vars to allow other types...
			    (let ((hvarlist nil)
				  (w-vars (if binderlist w-vars (drop-w (listify-type (cdar subfm)) type-var w-vars))))
			      (unless (eq w-vars 'fail)
			    (setq subfm (fill-in-blanks (cdr subfm) blanks 
							(if w-vars (append bd-var-list (listify-vec w-vars))
							  bd-var-list)
							prim-var-prefix))
			    (push (make-subst
				   :term (if w-vars
					     (prefix-lambda
					      (append (if binderlist 
							  (mapcar #'cons bd-var-list binderlist) nil)
						      subfm)
					      w-vars (length w-vars))
					   (eta-to-base (append (if binderlist 
								    (mapcar #'cons bd-var-list binderlist) nil)
								subfm)))
				   :h-vars hvarlist 
				   :type (concatenate 'string "SubFmSub" (princ-to-string (1- depth))))
				  substs-list))))))))))))

(defun make-deep-subfm-pr97c (resulttype w-vars prim-var-prefix)
  (declare (special prim-bdtypes))
  (when (eq resulttype 'o)
	(multiple-value-bind (blanks subfms) 
			     (get-useful-subformulas-pr97c (eproof-etree current-eproof))
			     ;subfms has the form of a list of (n m a) . gwff
			     ;where n is the number of blanks to be filled
			     ; and m is the number of (currently) vacuous quantifiers to be fixed
			     ; and a is the # of abbrevs
			     ; and gwff is a blanked-out formula.
			     (let ((substs-list nil)
				   (hvarlist nil))
			       (declare (special hvarlist))
					;w-vars is a list of variables to be lambda-bound
			       (dolist (subfm subfms (nreverse substs-list))
				       (let ((hvarlist nil)
					     (quant (cadar subfm)))
					 (setq subfm (fill-in-blanks (cdr subfm) blanks 
								     (if w-vars (listify-vec w-vars) nil)
								     prim-var-prefix))
					 (push (make-subst
						:term (if w-vars
							  (prefix-lambda subfm
									 w-vars (length w-vars))
							(eta-to-base subfm))
						:h-vars hvarlist 
						:type (concatenate 'string "SubFmSub" (princ-to-string quant)))
					       substs-list)))))))

(defun listify-vec (vector)
  (do ((list nil)
       (count (length vector) (1- count)))
      ((zerop count) list)
      (push (aref vector (1- count)) list)))

(defun fill-in-blanks (gwff blanks bdvars pvp)
  (declare (special hvarlist))
  (cond ((weak-label-p gwff) (fill-in-blanks (get gwff 'represents) blanks bdvars pvp))
	((lsymbol-q gwff) 
	 (if (assoc gwff blanks)
	     (let* ((hvar (car (make-n-new-heads pvp (list (car (nary-type (cons (cdr (assoc gwff blanks))
										 (mapcar 'type bdvars))))))))
		    (term hvar))
	       (rd-string-bound (princ-to-string hvar))
	       (push hvar hvarlist)
	       (dolist (var (reverse bdvars))
		       (setq term (cons term var)))
	       term)
	   gwff))
	((boundwff-q gwff) (let ((new-w (car (make-n-new-heads w-var-prefix (list (type (caar gwff)))))))
			     (rd-string-bound (princ-to-string new-w))
			     (cons (cons new-w (binder gwff)) (fill-in-blanks (cdr gwff) blanks (cons new-w bdvars) pvp))))
	(t (cons (fill-in-blanks (car gwff) blanks bdvars pvp) (fill-in-blanks (cdr gwff) blanks bdvars pvp)))))

(defun fudge-cons (list)
  (if (null (cdr list)) (car list)
    (cons (car list) (fudge-cons (cdr list)))))

(defun generate-new-terms (hv-list bv-list wv-list length)
  (let* ((termlist 
	  (mapcar #'construct-term hv-list (make-list (length hv-list) :initial-element wv-list) 
		  (make-list (length hv-list) :initial-element length)))
	 (complete-termlist (mapcar #'(lambda (x y) (construct-term x y (length y))) termlist (make-list (length termlist) :initial-element (reverse bv-list)))))
    (cdr complete-termlist)))

(defun make-deep-prims-new (resulttype binders w-vars length type-var prim-var-prefix depth)
  (declare (special prim-bdtypes))
  (when (and (eq resulttype 'o) (not (null prim-bdtypes)))
	(let ((substs nil))
	  (dolist (typelist (binderlistlist prim-bdtypes (1- depth)) (nreverse substs))
		  (let* ((bd-var-list (make-n-new-heads w-var-prefix typelist))
			 (bd-var-vec (make-array (list (1- depth)) :element-type 'symbol 
						 :initial-contents bd-var-list))
			 (type (construct-type (car (nary-type (cons 'o typelist))) type-var length)))
		    (dolist (binderlist (binderlistlist binders (1- depth)))
			    (do ((i min-prim-lits (1+ i)))
				((eq i (1+ max-prim-lits)) substs)
				(do ((j 0 (1+ j)))
				    ((eq j (^ 2 (1- i))))
				    (let* ((headvarlist (make-n-new-heads prim-var-prefix 
									  (make-list i :initial-element type)))
					   (nflist (generate-new-nflist j i headvarlist bd-var-vec w-vars length)))
				      (dolist (hvar headvarlist) (rd-string-bound (princ-to-string hvar)))
				      (do ((i 0 (1+ i))) ((= i (length bd-var-vec))) 
					  (rd-string-bound (princ-to-string (aref bd-var-vec i))))
				      (do ((i 0 (1+ i))) ((= i (length w-vars))) 
					  (rd-string-bound (princ-to-string (aref w-vars i))))
				      (push
				       (make-subst
					:term (prefix-lambda
					       (append (mapcar #'cons bd-var-list binderlist) (car nflist))
					       w-vars length)
					:h-vars headvarlist :type (concatenate 'string "GenSub" (princ-to-string depth)))
				       substs))))))))))

(defun generate-new-nflist (j i hv-list bv-list wv-list length)
  (let* ((termlist 
	  (mapcar #'construct-term hv-list (make-list (length hv-list) :initial-element wv-list) 
		  (make-list (length hv-list) :initial-element length)))
	 (complete-termlist (mapcar #'(lambda (x y) (construct-term x y (length y))) termlist (make-list (length termlist) :initial-element (reverse bv-list))))
	 (empty-list (list (car complete-termlist)))
	 (complete-termlist (cdr complete-termlist)))
;now we have a list of all the required terms, and we just have to join them together correctly...
    (do ((k (^ 2 (- i 2)) (/ k 2))
	 (j1 j))
	((< k 1) empty-list)
	(if (>= j1 k)
	    (progn (setq j1 (- j1 k))
		   (setq empty-list (list (cons (cons 'and (car empty-list)) (car complete-termlist))))
		   (setq complete-termlist (cdr complete-termlist)))
	  (progn (setq empty-list (list (cons (cons 'or (car empty-list)) (car complete-termlist))))
		 (setq complete-termlist (cdr complete-termlist)))))))
	  

(defun make-deep-prims-old (resulttype binders w-vars length type-var prim-var-prefix depth)
  (declare (special prim-bdtypes))
  (when (and (eq resulttype 'o) (not (null prim-bdtypes)))
	(let ((substs nil))
	  (dolist (typelist (binderlistlist prim-bdtypes (1- depth)) (nreverse substs))
		  (let* ((bd-var-list (make-n-new-heads w-var-prefix typelist))
			 (bd-var-vec (make-array (list (1- depth)) :element-type 'symbol 
						 :initial-contents bd-var-list))
			 (type (construct-type (car (nary-type (cons 'o typelist))) type-var length)))
		    (dolist (binderlist (binderlistlist binders (1- depth)))
			    (dolist (logconst (list 'and 'or))
			    (let* ((headvarlist (make-n-new-heads prim-var-prefix (make-list (* depth (1- depth)) :initial-element type)))
				   (nflist (generate-nflist depth headvarlist bd-var-vec w-vars logconst length)))
			      (push
			       (make-subst
				:term (prefix-lambda
				       (append (mapcar #'cons bd-var-list binderlist) (car nflist))
				       w-vars length)
				:h-vars headvarlist :type (concatenate 'string "GenSub" (princ-to-string depth)))
			       substs)))))))))


(defun nary-type (typelist)
  (if (= 1 (length typelist)) typelist
    (nary-type (cons (cons (car typelist) (cadr typelist)) (cddr typelist)))))

(defun generate-nflist (depth hv-list bv-list wv-list logconst length)
  (let* ((termlist 
	  (mapcar #'construct-term hv-list (make-list (length hv-list) :initial-element wv-list) 
		  (make-list (length hv-list) :initial-element length)))
	 (complete-termlist (mapcar #'(lambda (x y) (construct-term x y (length y))) termlist (make-list (length termlist) :initial-element (reverse bv-list)))))
;now we have a list of all the required terms, and we just have to join them together correctly...
  (if (equal logconst 'and)
      (outer-clumps (inner-clumps complete-termlist 'and (1- depth)) 'or)
    (outer-clumps (inner-clumps complete-termlist 'or (1- depth)) 'and))))

(defun outer-clumps (tlist logconst)
  (if (= (length tlist) 1) tlist
  (outer-clumps (cons (append (list (cons logconst (car tlist))) (cadr tlist)) (cddr tlist)) logconst)))

(defun inner-clumps (tlist logconst n)
  (if (eq (length tlist) n) (outer-clumps tlist logconst)
    (append (outer-clumps (butlast tlist (- (length tlist) n)) logconst) 
	    (inner-clumps (lastn tlist (- (length tlist) n)) logconst n))))

(defun lastn (list n) ; this is the lisp function LAST, with the optional argument that ACL4.1 forgot about
  (if (<= (length list) n) list
    (lastn (cdr list) n)))

(defun binderlistlist (binders n)
  (flatten-bindlist (binderlistlist1 binders n)))

;generates a list of all possible lists of n binders
;we also use it to generate lists of n types chosen from prim-bdtypes..

(defun binderlistlist1 (binders n)
	   (if (= n 1) (mapcar #'(lambda (x) (list (list x))) binders)
	     (mapcar #'(lambda (x) (flatten-bindlist (addbinders binders x))) (binderlistlist1 binders (1- n)))))

(defun flatten-bindlist (ls)
  (if (null (car ls)) nil
  (append (car ls) (flatten-bindlist (cdr ls)))))

(defun addbinders (binds bindlist)
	   (mapcar #'(lambda (x) (mapcar #'(lambda (y) (cons y x)) binds)) bindlist))

(defun make-n-new-heads (prim-var-prefix typelist)
  (mapcar #'(lambda (x) (ren-var-uni-ho prim-var-prefix x)) typelist))


(definfo pr89
 (mhelp "A flag setting for PRIMSUB-METHOD.
Only primsubs of the form \"exists x . literal\" and 
 \"forall x . literal\" will be generated."))

(definfo pr93
  (mhelp "A flag setting for PRIMSUB-METHOD.
For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
At depth 1, a single quantifier is introduced, as in PR89. 
At depth N>1, we have (N-1) quantifiers ranging over a formula
containing (N-1) conjunctions {disjunctions} of (N-2) 
disjunctions {conjunctions}."))

(definfo pr95
  (mhelp "A flag setting for PRIMSUB-METHOD.
For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
At depth 1, as in PR89.
At depth N>1, we have (N-1) quantifiers ranging over a formula
with between MIN-PRIM-LITS and MAX-PRIM-LITS literals, with
all combinations of connectives between them."))

(definfo pr97
  (mhelp "A flag setting for PRIMSUB-METHOD.
For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
At depth N>0, we have (N-1) quantifiers ranging over each 
subformula taken from the etree which contains between 
MIN-PRIM-LITS and MAX-PRIM-LITS literals. You can see these
subformulas by doing NAME-PRIM from the MATE top level. (Note:
both the instantiated and uninstantiated versions of each 
definition are used.)"))

(definfo pr97A
  (mhelp "A flag setting for PRIMSUB-METHOD.
Exactly as for PR97, but all substitutions are put into 
negation normal form."))

(definfo pr97B
  (mhelp "A flag setting for PRIMSUB-METHOD.
The substitutions from PR97A and PR95 are interleaved. The order
is determined firstly by the number of literals, then by the number of
quantifiers, and lastly with PR97 substs taking precedence over PR95."))

(definfo pr97C
  (mhelp "A flag setting for PRIMSUB-METHOD.
Using the connectives AND and OR, and the quantifiers EXISTS and
FORALL (ranging over variables of types PRIM-BDTYPES), and also using
any abbreviations or equalities that occur in the gwff to be proven, 
primsubs are built up using the bounds given by MIN- and MAX-PRIM-LITS
and MIN- and MAX-PRIM-DEPTH. See also PR97C-PRENEX and PR97C-MAX-ABBREVS."))

(definfo pr00
    (mhelp "A flag setting for PRIMSUB-METHOD.
This uses higher order unification to determine set substitutions
that solve part of the mating search in advance.
PR00 only works with 

DEFAULT-MS MS98-1

and

SKOLEM-DEFAULT NIL.

PR00 can be controlled using the flags
PR00-MAX-SUBSTS-VAR, PR00-REQUIRE-ARG-DEPS, PR00-NUM-ITERATIONS."))

(defun get-useful-subformulas-pr97c (gwff)
  (let ((subfms (mapcar #'(lambda (x) (cons (car x) (cddr x)))
			(subfms-of-etree-97c gwff)))
	(blanklist nil)
	newsubfms)
    (declare (special blanklist))
    (dolist (subfm subfms)
	    (let ((newfm (lnorm (blank-out-parts2 (cdr subfm))))
		  (blankcount 0)
		  (dummycount 0)
		  (abbrevcount 0))
	      (declare (special blanklist blankcount dummycount abbrevcount))
	      (count-blanks3 newfm blanklist)
	      (push (cons (list (max blankcount 1) dummycount abbrevcount) newfm) newsubfms)))
    (setq newsubfms (remove-duplicates newsubfms :key 'cdr :test 'wffeq-ab))
    (setq newsubfms (remove-if #'(lambda (x) (member x '(TRUTH FALSEHOOD (NOT . TRUTH) (NOT . FALSEHOOD)) :test 'equal))
			       newsubfms :key 'cdr))
    (setq newsubfms (make-combinations-of newsubfms blanklist))
    (values blanklist (sort newsubfms #'smaller-sub))))


(defun smaller-sub (x y) 
  (or (< (cadar x) (cadar y)) ;fewer quants
      (and (= (cadar x) (cadar y)) ;same # quants 
	   (or (< (caar x) (caar y)) ;but fewer lits
	       (and (= (caar x) (caar y)) ;same #lits
		    (or (< (caddar x) (caddar y)) ;but fewer abbrevs
			(and (= (caddar x) (caddar y))
			     (< (real-length (cdr x)) (real-length (cdr y))))))))))

(defun real-length (gwff)
  (length (princ-to-string
	   (do ((gwff (instantiate-all gwff nil)
		      (instantiate-all gwff nil))
		(oldgwff nil gwff))
	       ((wffeq oldgwff gwff) gwff)))))

(defun make-combinations-of (subfms blanklist)
  (declare (special blanklist))
  ;each subfm is of the form ((#blanks . #quants) fmla)
  ;we make all combinations from min-prim-lits to max-prim-lits, with <= max-prim-depth quantifiers,
  ;of the formulas in subfms, using the connectives 'and 'or and the quantifiers 'exists 'forall
  ;ranging over the types in prim-bdtypes.
  (unless (member (make-blank 'O) subfms :test 'equal :key 'cdr)
	  (push (cons (list 1 0 0) (make-blank 'O)) subfms))
  (setq subfms (mapcar #'(lambda (x) (cons (cons (if (zerop (caar x)) 1 (caar x)) (cdar x)) (cdr x))) subfms))
  (setq subfms (remove-if #'(lambda (x) (or (> (caar x) max-prim-lits)
					    (> (caddar x) pr97c-max-abbrevs)
					    (> (cadar x) (1- max-prim-depth))))
			  subfms))
  (do ((newsubfms subfms (append temp-subfms newsubfms))
       (units subfms)
       (this-time subfms temp-subfms)
       (first-time t nil)
       (temp-subfms subfms))
      ((null temp-subfms) (setq subfms (remove-if #'(lambda (x) (or (< (caar x) min-prim-lits)
								    (> (caar x) max-prim-lits)))
						  newsubfms)))
      (setq temp-subfms (if pr97c-prenex (join-everything units this-time first-time)
			  (append (quantify-everything newsubfms) (join-everything units this-time first-time)))))
;      (setq temp-subfms (remove-if #'(lambda (x) (member (cdr x) newsubfms :test 'wffeq-ab :key 'cdr)) temp-subfms)))
  (setq subfms (remove-duplicates subfms :test 'same-wff))
  (if pr97c-prenex
      (do ((newsubfms subfms (append temp-subfms newsubfms))
	   (this-time subfms temp-subfms)
	   (temp-subfms subfms))
	  ((null temp-subfms) (remove-if #'(lambda (x) (or (and (not (zerop (cadar x)))
								(< (cadar x) (1- min-prim-depth)))
							   (> (cadar x) (1- max-prim-depth))
							   (and (= (caar x) 1)
								(not (boundwff-q (cdr x)))
								(assoc (head (cdr x)) blanklist))))
					 newsubfms))
	  (setq temp-subfms (quantify-everything this-time)))
    (remove-if #'(lambda (x) 
		   (and (not (zerop (cadar x))) (< (cadar x) (1- min-prim-depth)))
		   (> (cadar x) (1- max-prim-depth))
		   (and (= (caar x) 1)
			(not (boundwff-q (cdr x)))
			(assoc (head (cdr x)) blanklist)))
	       subfms)))

(defun quantify-everything (subfms)
  (declare (special prim-bdtypes))
  (let ((returnlist nil))
    (dolist (s subfms returnlist)
	    (when (< (cadar s) (1- (1- max-prim-depth))) ;we want up to (1- m-p-d) quantifiers
		  (dolist (tp prim-bdtypes)
			  (push (cons (list (caar s) (1+ (cadar s)) (caddar s))
				      (cons (cons (make-blank tp) 'forall) (cdr s)))
				returnlist)
			  (push (cons (list (caar s) (1+ (cadar s)) (caddar s))
				      (cons (cons (make-blank tp) 'exists) (cdr s)))
				returnlist))))))

(defun join-everything (units subfms first-time)
  ;if first-time is T , units= subfms
  (if first-time
      (do ((subs subfms (cdr subs))
	   (lits (caaar subfms) (caaar subs))
	   (quants (cadaar subfms) (cadaar subs))
	   (abbrevs (cadr (cdaar subfms)) (cadr (cdaar subs)))
	   (sub (cdar subfms) (cdar subs))
	   (newsubs nil))
	  ((null sub) newsubs)
	  (when (and (< lits max-prim-lits) (<= quants (1- max-prim-depth)) (<= abbrevs pr97c-max-abbrevs))
		(dolist (s subs)
			(when (and (<= (+ lits (caar s)) max-prim-lits)
				   (<= (+ abbrevs (caddar s)) pr97c-max-abbrevs)
				   (< (+ quants (cadar s)) (1- max-prim-depth)))
			      (push (cons (list (+ lits (caar s)) (+ quants (cadar s)) (+ abbrevs (caddar s)))
					  (cons (cons 'and (cdr s)) sub))
				    newsubs)
			      (push (cons (list (+ lits (caar s)) (+ quants (cadar s)) (+ abbrevs (caddar s)))
					  (cons (cons 'or (cdr s)) sub))
				    newsubs)))))
    (do ((subs subfms (cdr subs))
	 (lits (caaar subfms) (caaar subs))
	 (quants (cadaar subfms) (cadaar subs))
	 (abbrevs (cadr (cdaar subfms)) (cadr (cdaar subs)))
	 (sub (cdar subfms) (cdar subs))
	 (newsubs nil))
	((null sub) newsubs)
	(when (and (< lits max-prim-lits) (<= quants (1- max-prim-depth)) (<= abbrevs pr97c-max-abbrevs))
	      (dolist (s units)
		      (when (and (<= (+ lits (caar s)) max-prim-lits)
				 (<= (+ abbrevs (caddar s)) pr97c-max-abbrevs)
				 (< (+ quants (cadar s)) (1- max-prim-depth)))
			    (push (cons (list (+ lits (caar s)) (+ quants (cadar s)) (+ abbrevs (caddar s)))
					(cons (cons 'and (cdr s)) sub))
				  newsubs)
			    (push (cons (list (+ lits (caar s)) (+ quants (cadar s)) (+ abbrevs (caddar s)))
					(cons (cons 'or (cdr s)) sub))
				  newsubs)))))))
	      
(defun subfms-of-etree-97c (etree)
  (reverse (reduce 'append 
		   (mapcar #'(lambda (x) (make-subformula-list-97c (suppress-details (get-shallow x))))
			   (append (find-etree-nodes 'leaf-p etree)
				   (find-etree-nodes #'(lambda (x) (and (rewrite-p x)
									(memq (rewrite-justification x)
									      '(LEIBNIZ= EQUIVWFFS))))
						     etree))))))

(defun make-subformula-list-97c (gwff)
  (unless (equiv-p (head gwff))
	  (if (and (gvar-p (head gwff)) (not (equality-p (head gwff))))
	      (list (cons 'O (cons (free-vars-of gwff) gwff)))
	    (list (cons 'O (cons (free-vars-of gwff) gwff)) (cons 'O (cons (free-vars-of gwff) (negwff gwff)))))))

(defun blank-out-parts2 (gwff)
  (declare (special blanklist))
  (cond ((weak-label-p gwff) (blank-out-parts2 (get gwff 'represents)))
	((lsymbol-q gwff) (if (not-useless gwff)
			      gwff (make-blank (type gwff))))
	((boundwff-q gwff) (cons (car gwff) ;this leaves a vacuous bound variable we have to deal with...
				 (if (not-useless (cdr gwff)) 
				     (blank-out-parts2 (cdr gwff))
				   (make-blank (type (cdr gwff))))))
	(t (if (not-useless gwff)
	       (cons (if (eq (car gwff) 'not)
			 'not
		       (if (not-useless (car gwff)) 
			   (blank-out-parts2 (car gwff))
			 (make-blank (type (car gwff)))))
		     (if (not-useless (cdr gwff)) 
			 (blank-out-parts2 (cdr gwff))
		       (make-blank (type (cdr gwff)))))
	     (make-blank (type gwff))))))

(defun count-blanks3 (gwff blanks)
  ;always returns NIL
  (declare (special blankcount dummycount abbrevcount))
  (cond ((weak-label-p gwff) (incf abbrevcount) (count-blanks3 (get gwff 'represents) blanks))
	((lsymbol-q gwff) (if (assoc gwff blanks) (incf blankcount)
			    (if (memq gwff '(not and or)) nil
			      (progn (incf abbrevcount) nil))))
	((boundwff-q gwff) (incf dummycount) (count-blanks3 (cdr gwff) blanks))
	(t (count-blanks3 (car gwff) blanks) (count-blanks3 (cdr gwff) blanks))))

(defun same-wff (x y)
  (and (= (caar x) (caar y))
       (= (cadar x) (cadar y))
       (= (caddar x) (caddar y))
       (same-wff-real (cdr x) (cdr y) (caar x))))

(defun same-wff-real (x y lits)
  (declare (special blanklist))
  (if (boundwff-q x)
      (if (and (boundwff-q y)
	       (eq (cdar x) (cdar y))
	       (eq (type (caar x)) (type (caar y))))
	  (same-wff-real (cdr x) (cdr y) lits)
	nil)
    (if (boundwff-q y)
	nil
      (let ((newblist (let (symlist)
			(dolist (b blanklist symlist)
				(do ((c 0 (1+ c))
				     tempsymlist)
				    ((= c lits) (push (cons tempsymlist (cdr b)) symlist))
				    (push (getrwff (concatenate 'string (princ-to-string (gensym)) 
								(core::type-to-string-2 (cdr b)))) tempsymlist)))))
	    blist)
	(declare (special blist))
	(let ((fmla (progn (setq blist (copy-list (mapcar 'copy-list newblist)))
			   (replace-blanks-in x)))
	      (fmlb (progn (setq blist newblist)
			   (replace-blanks-in y))))
	  (valid-p (cons (cons 'equiv fmla) fmlb)))))))

(defun replace-blanks-in (gwff)
  (declare (special blanklist blist))
  (cond ((symbolp gwff) (if (assoc gwff blanklist :test 'equal)
			    (dolist (b blist)
				    (when (equal (type gwff) (cdr b))
					  (return (pop (car b)))))
			  gwff))
	(t (cons (replace-blanks-in (car gwff))
		 (replace-blanks-in (cdr gwff))))))

