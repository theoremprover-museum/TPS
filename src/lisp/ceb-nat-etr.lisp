;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of etr-nat)

(context etr-nat)

; The following is code to convert natural deduction proofs
; into expansion proofs.  I got the idea from Frank's ATP notes.
; The procedure is as follows:


; 1. Annotate the lines of the natural deduction proof, as
;    in Frank Pfenning's ATP notes:
;      EXTRACTION (down arrow)
;      NORMAL (double up arrow)
;    by setting these properties of lines

; 2. If there are no backwards coercions (no extraction line depends on a normal line),
;    then the proof is "normal",
;    and we can construct a cut-free sequent calculus derivation.

; put this in a file ceb-annotate.lisp 
; we probably don't really need to do the annotation, instead
; incorporating it implicitly in the translation, but for now
; I will do it.

; A flag to determine which version of nat-etree to use

(defflag NAT-ETREE-VERSION
  (flagtype nat-etree-version-type)
  (default ceb)
  (subjects etr-nat)
  (mhelp "Determines which version of NAT-ETREE to use:
OLD -- The original version.
HX  -- Hongwei Xi's version which is intended to work on
       any natural deduction proof, normal or not.  This
       version has problems, but might work.
CEB -- Which is intended to only work on normal proofs,
       and should in principle always work on normal proofs."))
;       If NAT-ETREE-CUT-ELIM is set to T, then it attempts
;       to translate any proof by eliminate cuts when necessary."))

; perhaps someday
;(defflag NAT-ETREE-CUT-ELIM
;    (flagtype boolean)
;  (default nil)
;  (subjects etr-nat)
;  (mhelp "When set to T, NAT-ETREE (version CEB) attempts to
;eliminate cuts which correspond to parts of the natural deduction
;proof that are not normal.  When set to NIL, NAT-ETREE (version CEB)
;will fail if the proof is not normal and indicate to the user
;the first inference it found which is not normal."))

; mexpr's moved from hx-natree-top.lisp and modified to make use
; of the above flag.
(defmexpr pfnat
  (ARGTYPES symbol)
  (ARGNAMES proof)
  (ARGHELP "Proof to be mapped")
  (MAINFNS (lambda (proofname) 
	     (when (proof-plans proofname)
	       (throwfail "Proof " proofname " is not complete."))
	     (case NAT-ETREE-VERSION
		   (HX (setq current-natree (proof-to-natree proofname)))
		   (CEB (setq current-natree (ceb-proof-to-natree proofname)))
		   (t (throwfail "NAT-ETREE-VERSION " NAT-ETREE-VERSION " does not support pfnat.")))))
  (defaultfns (lambda (line) (if (eq line '$) (list dproof) (list line))))
  (MHELP "To generate a NATREE from given proof and store it in CURRENT-NATREE. This may
evolve into a command for rearranging natural deduction style proofs."))

(defun ceb-nat-etree (proofname)
  (declare (special background-eproof))
  (reconsider proofname)
  (let* ((newpfname (let ((pfname- (intern-str (format nil "~d-" proofname))))
		      (do ((pfname1 (intern-str (create-namestring pfname-)) (intern-str (create-namestring pfname-))))
			  ((not (proof-lines pfname1)) pfname1))))
	 (changed (eliminate-all-rulep-apps newpfname))) ; this changes the dproof to the new proof name
					; cebrown 6/30/01, eliminate RuleQ
    (when (eliminate-all-ruleq-apps)
      (setq changed t))
					; cebrown 8/22/99, copied the following code from auto-suggest to remove subst= lines
    (let ((subst=-lines (remove-if-not #'(lambda (x) (member (car (line-justification x)) 
							     '("Subst=" "Sub=" "Assert SYM=" "Sym=")
							     :test 'string=))
				       (get dproof 'lines)))
	  (bad-assert-lines (remove-if-not #'(lambda (x) (and (string= (car (line-justification x)) "Assert" 
								       :end1 (min 6 (length (car (line-justification x)))))
							      (not (member (car (line-justification x)) 
									   '("Assert REFL=" "Assert SYM=")
									   :test 'string=))))
					   (get dproof 'lines))))
      (when subst=-lines
      (msgf "Removing lines in this proof are justified by SUBST= or SYM=" t 
	    "NAT-ETREE will not work properly until they are removed." t t)
      (setq changed t)
      (remove-subst=-lines subst=-lines))
    (when bad-assert-lines
      (msgf "There are some lines justified by Assert" t
	    "The assertions will be replaced by hypotheses discharged at the end.")
      (setq changed t)
      (dolist (a bad-assert-lines) (make-assert-a-hyp a)))
    (dolist (l (proof-lines dproof))
	    (when (string= (car (line-justification l)) "Subst")
	      (let ((var (cadadr (line-justification l)))
		    (trm (caadr (line-justification l)))
		    (support (caaddr (line-justification l)))
		    (i (linealias l)))
		(setq changed t)
		(plan l)
		(introduce-gap l 1)
		(comdecode (list 'cl-user::ugen i (linealias support)
				 '$ (list 'quote var) '$ '$))
		(comdecode (list 'cl-user::ui i (+ i 1)
				 (list 'quote trm) '$ '$ '$ '$ '$)))))
    (eliminate-all-rulep-apps)		; remove-subst= may introduce rulep's
    (when (expand-cases) ; makes any application of CASES binary (see rules CASES3, CASES4)
      (setq changed t))
    ; finally, if we started with a proof with undischarged hyps, discharge them now
    (do* ((conc (car (last (proof-lines dproof))) (car (last (proof-lines dproof))))
	  (hyps (line-hypotheses conc) (line-hypotheses conc)))
	 ((null hyps))
	 (comdecode (list 'cl-user::deduct (+ (linealias conc) 1) (linealias conc)
			  (linealias (car hyps)) '$ '$ '$ '$ '$)))
    (setq current-natree (ceb-proof-to-natree dproof))
    (multiple-value-bind
	(f ftree-clist)
	(natree-to-ftree-main)
      (ftree-to-etree f)
    (setf (etree-leaf current-topnode) T ;;;tell merge-tree-real that this etree does need substitutions.
	  (eproof-etree current-eproof) current-topnode)
    (update-statuses current-topnode)
    (let ((ALLOW-NONLEAF-CONNS 
	   (apply #'append
		  (mapcar #'(lambda (x)
			      (list (ftree-name (car x))
				    (ftree-name (cdr x))))
			  ftree-clist)))
	  (MAX-MATES :INFINITE))
      (declare (special ALLOW-NONLEAF-CONNS MAX-MATES))
      (start-new-mating)
      (setf (eproof-jform current-eproof)
	(etree-to-jform (eproof-etree current-eproof)))
      (dolist (conn ftree-clist)
	(add-conn (ftree-name (car conn))
		  (ftree-name (cdr conn))))
      (setq background-eproof current-eproof)
      (msgf "If you want to translate the expansion proof back to a natural deduction proof,
you must first merge the etree.  If you want to use the expansion proof to determine
flag settings for automatic search, you should not merge the etree.")
      (if (query "Merge The Etree?" t)
	(progn
;	  (let ((*ignore-statuses* nil))
;	    (update-statuses (eproof-etree current-eproof)))
	  (setf (eproof-leaf-list current-eproof)
	    (find-etree-nodes #'leaf-p (eproof-etree current-eproof)))
;	  (mate-prop-msearch)
	  (let ((*ignore-statuses* nil)
		(ca (eproof-connections-array current-eproof)))
	    (merge-tree))
	  (msgf "Proof has been translated -- use ETREE-NAT to reverse the
process." t))
	(msgf "The expansion proof " background-eproof " can be used to trace MS98-1 search procedure."))
    (if changed
	(msgf "The current natural deduction proof " dproof " is a modified version
of the original natural deduction proof.

Use RECONSIDER " proofname " to return to the original proof.")
      (setq dproof proofname)))))))

(defmexpr nat-etree
 (argtypes symbol)
 (argnames prefix)
 (arghelp "Name of the Proof")
 (MAINFNS (lambda (proofname)
	    (when (proof-plans proofname)
	      (throwfail "Proof " proofname " is not complete."))
	    (case NAT-ETREE-VERSION
		  (OLD (nat-etree-old proofname))
		  (HX (hx-nat-etree proofname))
		  (CEB (ceb-nat-etree proofname))
		  (t (throwfail "Please Set NAT-ETREE-VERSION")))))
 (defaultfns (lambda (x) (list (if (eq '$ x) dproof x))))
 (mhelp "Translates a natural deduction proof, (which must be the current dproof
-- use RECONSIDER to return to an old proof in memory), into an 
expansion proof. This will not work on all proofs: in particular,
proofs containing ASSERT of anything but REFL= and SYM=, proofs 
using rewrite rules and proofs containing SUBST= or SUB= cannot be
translated at present.  

There are several versions of nat-etree.  Set the flag
NAT-ETREE-VERSION to determine which version to use.

In all but the OLD version, the user is given the option of
removing lines justified by SUBST=, SUB=, or SYM= and replacing the justification
with a subproof.  This permanently modifies the proof.  (AUTO-SUGGEST
also gives such an option.)"))

; first a new version of proof-to-natree

; Made several changes to this function to make it compatible with
; my nat-etree code.  One change is to have the eigenvariable condition
; be global (since selected variables must be globally distinct in the etree).
; So, the variable in RuleC and UGen will be different from the one
; in the original ND proof (which only needed to satisfy a local eigenvariable
; condition). - cebrown 2/00.  
(defun ceb-proof-to-natree (proofname)
  (when (proof-plans proofname) (throwfail "A complete proof with no plan lines is needed."))
  (proof-line-to-natree proofname (car (last (proof-lines proofname)))))

; I was creating DAG's to prevent recomputation
; BUT there is a problem -- we cannot reuse UGen and RuleC derivations if we want to
; maintain a global eigenvariable condition.  We could still reuse some derivations,
; but just to be safe I am now building an actual tree, and not reusing any derivations.
; Note that in the worst case, this could make the tree exponential in the size of the number of lines,
; but this should not happen in ordinary proofs. - cebrown 2/8/00

; I thought it would be okay to use DAG's even with UGen and RuleC,
; because each line should correspond to a unique place in the
; expansion tree (except possibly leaves).  But in fact a single UI may
; correspond to several expansions (which have accidently been identified)
; since the eigenvariables may be the same.  So, what I've
; done is associate lines and ab's (var automorphisms) with natree nodes - cebrown 3/4/00

					; Updated this to create an annotated natree. - cebrown 7/29/00

; Convert line (after subst ab) to a natree justification.
; The purpose of ab is to move variables so that UGen and
; RuleC satisfy a global eigenvariable condition.
(defun proof-line-to-natree (proofname line &optional ab (annotation 'normal))
  (let* ((just (line-justification line))
	 (nd-rule (intern (string-upcase (first just))
			  (find-package "AUTO")))
	 (natree-rule
					; cebrown 2/16/01 -- decided to leave NEG as NEG and handle this like SAME in nat-etree below
	  (cond ((member nd-rule '(ASSOC EQUIVCONJ
					 IMP-DISJ-L IMP-DISJ-R IMP-DISJ
					 DISJ-IMP-L DISJ-IMP-R DISJ-IMP))
		 'RULEP)
		((eq nd-rule 'REFL=)
		 (if (third just)
		     'REFL=
		   '|ASSERT REFL=|))
		(t nd-rule)))
	 (number (line-linenumber line)))
    (let* ((na 
	    (case natree-rule
		  ((EQUIV-EQ EQUALITY DEFN EQUIVWFFS LAMBDA LAMBDA= |BETA RULE| |ETA RULE| EXT= REFL=)
		   (let ((supp (car (third (line-justification line)))))
		     (proof-line-to-natree-rewrites
		      proofname number
		      (proof-line-to-natree proofname supp ab 'extraction)
		      (simul-substitute-l-term-var ab (line-assertion supp))
		      (simul-substitute-l-term-var ab (line-assertion line))
		      annotation)))
		  (RULEC
		   (let* ((choose (first (set-difference
					  (line-hypotheses (second (third just)))
					  (line-hypotheses line))))
			  (x (if choose
				 (car (second (line-justification choose)))))
			  (a (if choose (fresh-var (type x) (getnameroot x)))))
		     (if choose
			 (let ((na1 (proof-line-to-natree proofname (car (third just)) ab 'extraction))
			       (na2 (proof-line-to-natree proofname (cadr (third just)) (acons x a ab) 'normal))) ; x cannot appear free in this hyp
			   (create-natree proofname number 'RULEC 
					  (simul-substitute-l-term-var ab (line-assertion line)) ; x cannot appear free in line assertion
					  'NORMAL (list na1 na2) (list a)
					  (union (remove-if #'(lambda (n)
								(equal (line-linenumber choose) (natree-number n)))
							    (natree-hypo na2))
						 (natree-hypo na1))))
		       (proof-line-to-natree (cadr (third just)) ab 'normal))))
		  (UGEN
		   (let* ((x (car (second just)))
			  (a (fresh-var (type x) (getnameroot x))))
		     (create-natree proofname number 'UGEN
				    (simul-substitute-l-term-var ab (line-assertion line)) ; x cannot appear free in assertion
				    'NORMAL
				    (list (proof-line-to-natree proofname (car (third just)) (acons x a ab) 'normal))
				    (list a))))
		  (MP
		   (create-natree proofname number 'MP
				  (simul-substitute-l-term-var ab (line-assertion line))
				  'EXTRACTION
				  (list
				   (proof-line-to-natree proofname (car (third just)) ab
							 'NORMAL)
				   (proof-line-to-natree proofname (cadr (third just)) ab
							 'EXTRACTION))))
		  (NEGELIM
		   (create-natree proofname number 'NEGELIM
				  (simul-substitute-l-term-var ab (line-assertion line))
				  'NORMAL
				  (list (proof-line-to-natree proofname (car (third just)) ab
							      'EXTRACTION)
					(proof-line-to-natree proofname (cadr (third just)) ab
							      'NORMAL))))
		  (CASES ; modified this to only handle 2 cases, expand-cases should be called before calling this function
		   (let* ((jor (car (third just)))
			  (j1 (cadr (third just)))
			  (j2 (caddr (third just)))
			  (wff (simul-substitute-l-term-var ab (line-assertion line)))
			  (case1 (first (setdiff (line-hypotheses j1)
						 (line-hypotheses line))))
			  (case2 (first (setdiff (line-hypotheses j2)
						 (line-hypotheses line)))))
		     (if case1
			 (if case2
			     (let* ((na-or (proof-line-to-natree proofname jor ab 'EXTRACTION))
				    (na1 (proof-line-to-natree proofname j1 ab 'NORMAL))
				    (na2 (proof-line-to-natree proofname j2 ab 'NORMAL))
				    (case1num (line-linenumber case1))
				    (case2num (line-linenumber case2)))
			       (create-natree proofname number 'CASES wff
					      'NORMAL (list na-or na1 na2) nil
					      (union (natree-hypo na-or)
						     (union
						      (remove-if #'(lambda (n)
								     (equal case1num (natree-number n)))
								 (natree-hypo na1))
						      (remove-if #'(lambda (n)
								     (equal case2num (natree-number n)))
								 (natree-hypo na2))))))
					;  if case2 pf doesn't depend on case2 hyp
			   (proof-line-to-natree proofname j2 ab 'NORMAL))
					; if case1 pf doesn't depend on case1 hyp
		       (proof-line-to-natree proofname j1 ab 'NORMAL))))
		  ((ABSURD RULEP RULEQ TRUTH)
		   (create-natree proofname number natree-rule
				  (simul-substitute-l-term-var ab (line-assertion line))
				  'NORMAL
				  (mapcar #'(lambda (l)
					      (proof-line-to-natree 
					       proofname l ab 'EXTRACTION))
					  (third just))))
		  ((SUBST= SUB=) ; see the comments below in annotate-pf for a discussion about annotating Subst=
		   (let ((na1 (proof-line-to-natree proofname (car (third just)) ab 'NORMAL))
			 (na2 (proof-line-to-natree proofname (cadr (third just)) ab 'EXTRACTION)))
		     (create-natree proofname number 'SUBST= 
				    (simul-substitute-l-term-var ab (line-assertion line))
				    'EXTRACTION (list na1 na2))))
					; most rules have the same annotations on prems as on concl,
					; some exceptions are : RULEC, MP, NEGELIM, CASES, EQUIV-EQ, EQUALITY, ABSURD, RULEP, RULEQ (& SUBST=) - handled above
		  ((SAME AB NEG |SAME AS|)
		   (let ((na1 (proof-line-to-natree proofname (car (third just))
						    ab annotation)))
		     (create-natree proofname number natree-rule
				    (simul-substitute-l-term-var ab (line-assertion line))
				    annotation (list na1))))
		  (CONJ
		   (if (cdr (third just)) ; ICONJ
		       (let ((na1 (proof-line-to-natree proofname (car (third just))
							ab 'normal))
			     (na2 (proof-line-to-natree proofname (cadr (third just))
							ab 'normal)))
			 (create-natree proofname number natree-rule
					(simul-substitute-l-term-var ab (line-assertion line))
					'NORMAL (list na1 na2)))
					; else, ECONJ
		     (let ((na1 (proof-line-to-natree proofname (car (third just))
						      ab 'extraction)))
		       (create-natree proofname number natree-rule
				      (simul-substitute-l-term-var ab (line-assertion line))
				      'EXTRACTION (list na1)))))
		  (INDIRECT ; this could mean 3 things:
		   (let* ((P2 (car (third just)))
			  (H1 (first (setdiff (line-hypotheses (car (third just)))
					      (line-hypotheses line))))
			  (P2-wff (line-assertion P2)))
		     (cond ((eq P2-wff 'FALSEHOOD)
					; (H1)  H,H1   !~A(O)                           Assume negation
					; (P2)  H,H1   !FALSEHOOD                                          
					;*(P3)  H      !A(O)                              Indirect: P2
			    (let ((na1 (proof-line-to-natree proofname P2 ab 'NORMAL)))
			      (create-natree proofname number 'INDIRECT
					     (simul-substitute-l-term-var ab (line-assertion line))
					     'NORMAL (list na1) nil
					     (if H1
						 (let ((hnum (line-linenumber H1)))
						   (remove-if #'(lambda (n)
								  (equal hnum (natree-number n)))
							      (natree-hypo na1)))
					       (natree-hypo na1)))))
			   ((null (cdr (third just)))
					; (H1)  H,H1   !~A(O)                           Assume negation
					; (P2)  H,H1   !B(O) AND ~B                                          
					;*(P3)  H      !A(O)                              Indirect: P2
			    (let* ((naP2 (proof-line-to-natree proofname P2 ab 'EXTRACTION))
				   (BandNotB (natree-assertion naP2))
				   (final-hyps
				    (if H1
					(let ((hnum (line-linenumber H1)))
					  (remove-if #'(lambda (n)
							 (equal hnum (natree-number n)))
						     (natree-hypo naP2)))
				      (natree-hypo naP2)))
				   (naB (create-natree
					 proofname number 'COERCION
					 (cdar BandNotB)
					 'NORMAL
					 (list
					  (create-natree
					   proofname number 'CONJ
					   (cdar BandNotB)
					   'EXTRACTION (list naP2)))))
				   (naNotB (create-natree
					    proofname number 'CONJ
					    (cdr BandNotB)
					    'EXTRACTION (list naP2))))
			      (create-natree
			       proofname number 'INDIRECT
			       (simul-substitute-l-term-var ab (line-assertion line))
			       'NORMAL
			       (list (create-natree
				      proofname number 'NEGELIM 'FALSEHOOD 'NORMAL
				      (list naNotB naB)))
			       nil
			       final-hyps)))
			   (t 
; (H1)  H,H1   !~A(O)                           Assume negation
; (P2)  H,H1   !B(O)                                          
; (P3)  H,H1   !~B(O)                                          
;*(P4)  H      !A(O)                           Indirect: P2 P3
			    (let* ((P3 (cadr (third just)))
				   (naP2 (proof-line-to-natree proofname P2 ab 'NORMAL))
				   (naP3 (proof-line-to-natree proofname P3 ab 'EXTRACTION))
				   (final-hyps
				    (if H1
					(let ((hnum (line-linenumber H1)))
					  (remove-if #'(lambda (n)
							 (equal hnum (natree-number n)))
						     (append (natree-hypo naP2)
							     (natree-hypo naP3))))
				      (append (natree-hypo naP2)
					      (natree-hypo naP3)))))
			      (create-natree
			       proofname number 'INDIRECT
			       (simul-substitute-l-term-var ab (line-assertion line))
			       'NORMAL
			       (list 
				(create-natree
				 proofname number 'NEGELIM 'FALSEHOOD 'NORMAL
				 (list naP3 naP2)
				 nil
				 final-hyps))
			       nil
			       final-hyps))))))
		  ((DEDUCT NEGINTRO)
		   (let ((na1 (proof-line-to-natree proofname (car (third just))
						    ab 'NORMAL))
			 (hyp (first (setdiff (line-hypotheses (car (third just)))
					      (line-hypotheses line)))))
		     (create-natree proofname number natree-rule
				    (simul-substitute-l-term-var ab (line-assertion line))
				    'NORMAL (list na1) nil
				    (if hyp
					(let ((hnum (line-linenumber hyp)))
					  (remove-if  #'(lambda (n)
							  (equal hnum (natree-number n)))
						      (natree-hypo na1)))
				      (natree-hypo na1)))))
		  (EGEN
		   (let ((na1 (proof-line-to-natree 
			       proofname (car (third just)) ab 'NORMAL)))
		     (create-natree proofname number natree-rule
				    (simul-substitute-l-term-var ab (line-assertion line))
				    'NORMAL (list na1)
				    (list (simul-substitute-l-term-var ab (car (second just)))))))
		  ((|ASSERT REFL=| IDISJ-R IDISJ-L IMPEQUIV
		    NNF-EXPAND TRUTH TRUTHP)
		   (let ((nal (mapcar #'(lambda (l)
					  (proof-line-to-natree 
					   proofname l ab 'NORMAL))
				      (third just))))
		     (create-natree proofname number natree-rule
				    (simul-substitute-l-term-var ab (line-assertion line))
				    'NORMAL nal)))
		  ((CHOOSE HYP |ASSUME NEGATION| |CASE 1| |CASE 2| |CASE 3| |CASE 4|)
		   (let ((na0
			  (create-natree proofname number natree-rule
					 (simul-substitute-l-term-var ab (line-assertion line))
					 'EXTRACTION nil)))
		     (setf (natree-hypo na0) (list na0))
		     na0))
		  (UI
		   (let ((na1 (proof-line-to-natree proofname (car (third just))
						    ab 'EXTRACTION)))
		     (create-natree proofname number 'UI
				    (simul-substitute-l-term-var ab (line-assertion line))
				    'EXTRACTION (list na1)
				    (list (simul-substitute-l-term-var ab (car (second just)))))))
		  ((EQUIVIMP NNF)
		   (let ((na1 (proof-line-to-natree proofname (car (third just))
						    ab 'EXTRACTION)))
		     (create-natree proofname number natree-rule
				    (simul-substitute-l-term-var ab (line-assertion line))
				    'EXTRACTION (list na1))))
		  (t (throwfail "Unexpected case " natree-rule)))))
      (proof-line-to-natree-annotation annotation proofname na))))
  
(defun proof-line-to-natree-annotation (annotation proofname na)
  (if (eq annotation (natree-annotation na))
      na
    (if (eq annotation 'NORMAL)
	(if (eq (natree-justification na) 'BACKWARDS-COERCION)
	    (car (natree-support na))
	  (create-natree proofname (natree-number na) 'COERCION
			 (natree-assertion na) 'NORMAL (list na)))
      (if (eq (natree-justification na) 'COERCION)
	  (car (natree-support na))
	(create-natree proofname (natree-number na) 'BACKWARDS-COERCION
		       (natree-assertion na) 'EXTRACTION (list na))))))

(defun create-natree (proofname number just wff ann supp
				&optional jterm (hypo 'SUPP))
  (when (eq hypo 'SUPP)
    (if supp
	(if (cdr supp)
	    (setq hypo (remove-duplicates (apply #'append (mapcar #'(lambda (n) (natree-hypo n)) supp))))
	  (setq hypo (natree-hypo (car supp))))
      (setq hypo nil)))
  (make-natree :name (intern (create-namestring-with-hyphen proofname)
			     (find-package "CL-USER"))
	       :number number
	       :hypo hypo
	       :justification just
	       :jterm jterm
	       :annotation ann
	       :assertion wff
	       :support supp))

					; wff1 & wff2 should be equal up to lambda conv, defn expansion, and expanding
					; equality using Leibniz or Ext
					; 8/30/01 - added binder defns - cebrown
(defun proof-line-to-natree-rewrites (proofname number na wff1 wff2 annotation)
  (let ((c (common-defn-eq-refl=-lam-reduct wff1 wff2 nil)))
    (if c
	(let ((rews1 (car c))
	      (rews2 (cdr c)))
	  (proof-line-to-natree-annotation
	   annotation proofname
	   (proof-line-to-natree-rewrites-1 proofname number na rews1 rews2)))
      (let ((c2 (common-defn-eq-refl=-lam-reduct (acons 'AND wff1 wff1) wff2)))
	(if c2
	    (let ((rews1 (car c2))
		  (rews2 (cdr c2)))
	      (proof-line-to-natree-annotation
	       annotation proofname
	       (proof-line-to-natree-rewrites-1
		proofname number
		(create-natree proofname number 'DUAL-AND
			       (acons 'AND wff1 wff1) 'EXTRACTION (list na))
		rews1 rews2)))
	  (let ((c3 (common-defn-eq-refl=-lam-reduct wff1 (acons 'OR wff2 wff2))))
	    (if c3
		(let* ((rews1 (car c3))
		       (rews2 (cdr c3))
		       (na2 (proof-line-to-natree-rewrites-1
			     proofname number na rews1 rews2)))
		  (proof-line-to-natree-annotation
		   annotation proofname
		   (create-natree
		    proofname number 'DUAL-OR wff2 'NORMAL (list na2))))
	      (throwfail "Could not make wffs " t (wff1 . gwff) t " and " t (wff2 . gwff) t " up to lambda/defns/equality"))))))))

					; na is an extraction natree with assertion wff1
					; return a normal natree with assertion wff2
					; rews1 is (wff1 JUST wff11 JUST . . . wff3)
					; rews2 is (wff2 JUST wff22 JUST . . . wff3)
(defun proof-line-to-natree-rewrites-1 (proofname number na rews1 rews2)
  (if (cdr rews2)
      (create-natree proofname number
		      (case (caadr rews2)
			    (LEIBNIZ 'LEIBNIZ=)
			    (EXTENS 'EXT=)
			    (DEFN 'DEFN)
			    (BINDER 'BINDER-DEFN)
			    (REFL 'REFL=)
			    (t (caadr rews2)))
		      (car rews2) 'NORMAL
		      (list (proof-line-to-natree-rewrites-1
			     proofname number na rews1 (cddr rews2))))
    (proof-line-to-natree-rewrites-2
     proofname number na rews1)))

(defun proof-line-to-natree-rewrites-2 (proofname number na rews1)
  (if (cdr rews1)
      (proof-line-to-natree-rewrites-2
       proofname number
       (create-natree proofname number 
		      (case (caadr rews1)
			    (LEIBNIZ 'LEIBNIZ=)
			    (EXTENS 'EXT=)
			    (DEFN 'DEFN)
			    (BINDER 'BINDER-DEFN)
			    (REFL 'REFL=)
			    (t (caadr rews1)))
		      (caddr rews1) 'EXTRACTION (list na))
       (cddr rews1))
    (proof-line-to-natree-annotation 'NORMAL proofname na)))

					; update this to handle defns differently?
					; At each step, we can check that there is a complete mating.
(defun annotate-pf (&optional (pf dproof))
  (let ((lines (proof-lines pf)))
    (dolist (line lines)
	    (setf (get line 'extraction) nil)
	    (setf (get line 'normal) nil)
	    (setf (get line 'coercion) nil)
	    (setf (get line 'bcoercion) nil))
    (dolist (line lines)
	    (let* ((j (line-justification line))
		   (rule (car j))
		   (deps (caddr j)))
	      (cond ((member rule '("Hyp" "Choose" "Case 1" "Case 2" "Case 3" "Case 4" "Assume negation")
			     :test #'equal)
		     (set-extraction-line line))
		    ((member rule '("UI") :test #'equal)
		     (mapc #'set-extraction-line (cons line deps)))
		    ((member rule '("AB" "Same as"))
		     (propagate-annotation (car deps) line))
		    ((equal rule "Eta rule")
		     (cond ((eta-normal-p (get (car deps) 'assertion))
			    (set-normal-line line)
			    (set-normal-line (car deps)))
			   ((eta-normal-p (get line 'assertion))
			    (set-extraction-line line)
			    (set-extraction-line (car deps)))
			   (t 
			    (set-normal-line line)
			    (set-extraction-line (car deps)))))
		    ((equal rule "Beta rule")
		     (cond ((beta-normal-p (get (car deps) 'assertion))
			    (set-normal-line line)
			    (set-normal-line (car deps)))
			   ((beta-normal-p (get line 'assertion))
			    (set-extraction-line line)
			    (set-extraction-line (car deps)))
			   (t 
			    (set-normal-line line)
			    (set-extraction-line (car deps)))))
		    ((member rule '("Lambda" "Lambda=") :test #'equal)
		     (cond ((beta-eta-normal-p (get (car deps) 'assertion))
			    (set-normal-line line)
			    (set-normal-line (car deps)))
			   ((beta-eta-normal-p (get line 'assertion))
			    (set-extraction-line line)
			    (set-extraction-line (car deps)))
			   (t 
			    (set-normal-line line)
			    (set-extraction-line (car deps)))))
		    ((member rule '("Defn" "EquivWffs") :test #'equal)
		     (cond ((wffeq-ab (instantiate-1 (get line 'assertion)) (get (car deps) 'assertion))
			    (set-extraction-line line)
			    (set-extraction-line (car deps)))
			   ((wffeq-ab (instantiate-1 (get (car deps) 'assertion)) (get line 'assertion))
			    (set-normal-line line)
			    (set-normal-line (car deps)))
			   (t
			    (set-normal-line line)
			    (set-extraction-line (car deps)))))
		    ((member rule '("Equality" "Equiv-eq") :test #'equal)
		     (let ((pwff (line-assertion line))
			   (swff (line-assertion (car deps))))
		       (if (wffeq-ab pwff (instantiate-top-equality swff))
			   (progn
			     (set-extraction-line line)
			     (set-extraction-line (car deps)))
			 (if (wffeq-ab swff (instantiate-top-equality pwff))
			     (progn
			       (set-normal-line line)
			       (set-normal-line (car deps)))
			   (progn
			     (set-normal-line line)
			     (set-extraction-line (car deps)))))))
		    ((equal rule "ImpEquiv")
		     (set-normal-line line)
		     (set-normal-line (car deps)))
		    ((equal rule "EquivImp")
		     (set-extraction-line line)
		     (set-extraction-line (car deps)))
		    ((member rule '("RuleP" "RuleQ" "Sym=" "Assoc" "Neg" "Imp-Disj" "Imp-Disj-L" "Imp-Disj-R" "Disj-Imp" "Disj-Imp-L" "Disj-Imp-R") :test #'equal) ; RuleP (and others) treated as n-ary rule with all premisses extractions, and conclusion a normal
		     (set-normal-line line)
		     (mapc #'set-extraction-line deps))
		    ((equal rule "MP")
		     (set-extraction-line line)
		     (set-extraction-line (cadr deps))
		     (set-normal-line (car deps)))
		    ((equal rule "RuleC") ; two possible annotations (all down, or as in Frank's Notes)
		     (set-extraction-line (car deps))
		     (propagate-annotation (cadr deps) line))
		    ((member rule '("Deduct" "UGen" "EGen" "Indirect" "NegIntro") :test #'equal)
		     (mapc #'set-normal-line (cons line deps)))
		    ((equal rule "NegElim")
		     (set-extraction-line (car deps))
		     (set-normal-line (cadr deps))
		     (set-normal-line line))
		    ((and
		      (> (length rule) 5)
		      (string= rule "Assert" :end1 6)) ; asserts are normal (ie, we worked back to them and stopped)
		     (set-normal-line line))
		    ((member rule '("Subst=" "Sub=") :test #'equal) ; here I am annotating Subst='s, although nat-etree removes subst='s, replacing them with Leibniz Equality.
					; I annotate Subst='s in the hope that we will handle equality better in the future.
		     
					; Two possibilities are:
					; (1) annotate the rule like a Leibniz derivation would be annotated (see UI & MP)
					;     the advantage here is that normalization can treat Subst= like UI + MP
					;
					;       P[s] up (as prem of MP)   s = t down (this would be followed by a UI, down)
					;       ------------------------------------- Subst=
					;            P[t] down (as concl of MP)
		     
					; (2) annotate the rule so that in an etree we are trying to connect P[s] to P[t]
					;     and we solve the equation s = t on the side, which is probably the right
					;     way to handle automatic search - especially if we want to build in prop extensionality
					;
					;       P[s] down        s = t up
					;       ------------------------- Subst=
					;                P[t] up
					;  
					;     A problem with using (2) is that there doesn't seem to be a normal pf of
					;     "a = b implies f a = f b".  We would probably need a separate Subst= rule:
					;
					;              s = t down
					;             -----------
					;             f[s] = f[t] up
					;
					;     to handle such a situation (corresponds to 'mating' two eqns)
		     
					; (1), using this for now:
 		     (set-extraction-line line) 
		     (set-normal-line (car deps)) 
		     (set-extraction-line (cadr deps)))
					; (2), would like to use this, but probably would need another version of Subst= rule
					;		     (set-normal-line line)
					;		     (set-extraction-line (car deps))
					;                    (set-normal-line (cadr deps)))
		    ((equal rule "Ext=") ; one direction is extraction, the other is normal
		     (if (equals-p (get line 'assertion))
			 (mapc #'set-normal-line (cons line deps))
		       (mapc #'set-extraction-line (cons line deps))))
		    ((equal rule "Absurd")
		     (set-normal-line line)
		     (set-extraction-line (car deps)))
		    ((equal rule "Cases")
		     (set-extraction-line (car deps))
		     (mapc #'set-normal-line (cons line (cdr deps))))
		    ((equal rule "Conj") ; TPS names both econj and iconj "Conj", 
					; but we can distinguish based on the number of premisses
		     (if (cdr deps)
			 (mapc #'set-normal-line (cons line deps))
		       (mapc #'set-extraction-line (cons line deps))))
		    ((member rule '("Idisj-R" "Idisj-L") :test #'equal)
		     (mapc #'set-normal-line (cons line deps)))
		    (t (throwfail "Cannot Annotate Proofs Using Rule " rule)))))))

(defun propagate-annotation (line1 line2)
  (setf (get line2 'normal) (get line1 'normal))
  (setf (get line2 'extraction) (get line1 'extraction)))

(defun set-extraction-line (line)
  (when (and (get line 'normal)
	     (not (get line 'coercion)))
    (setf (get line 'bcoercion) t))
  (setf (get line 'extraction) t))

(defun set-normal-line (line)
  (when (and (get line 'extraction)
	     (not (get line 'bcoercion)))
    (setf (get line 'coercion) t))
  (setf (get line 'normal) t))

(defun print-annotations (&optional (pf dproof))
  (annotate-pf pf)
  (dolist (line (proof-lines pf))
	  (format t "(~d) " (linealias line))
	  (if (get line 'BCOERCION)
	      (format t "Backwards Coercion (CUT)~%")
	    (if (get line 'COERCION)
		(format t "Coercion~%")
	      (if (get line 'EXTRACTION)
		  (format t "Extraction~%")
		(if (get line 'NORMAL)
		    (format t "Normal~%")
		  (format t "Not Annotated?~%")))))))


					; it is ESSENTIAL that the following two lists do not intersect.
					;  - cebrown - 7/27/00
(defun extraction-justification-p (j)
  (member j '(UI CHOOSE HYP |ASSUME NEGATION| |CASE 1| |CASE 2| |CASE 3| |CASE 4| MP ASSOC EQUIVIMP NNF)))

(defun normal-justification-p (j)
  (member j '(UGEN EGEN RULEC DEDUCT RULEP RULEQ ABSURD CASES INDIRECT NEGINTRO NEGELIM
	      |ASSERT REFL=| IDISJ-R IDISJ-L IMPEQUIV TRUTHP NNF-EXPAND TRUTH)))

(defun check-mating-exists (hyps node)
  (if hyps
      (let ((imp (if (natree-dup-info-pos (car hyps))
		     (make-implication
		      :positive nil
		      :junctive 'con
		      :components (list (natree-dup-info-pos (car hyps)) node))
		   node)))
	(setf (etree-parent node) imp)
	(check-mating-exists (cdr hyps) imp)
	(setf (etree-parent node) nil))
    (let* ((jform (etree-to-jform node))
	   (default-ms 'ms90-3)
	   (max-mates 100000)
	   (order-components t))
      (initialize-mating-search)
      (when (eq (prop-msearch jform) 'fail)
	(throwfail "Translation Failure")))))

					; some functions which I can't find elsewhere in the code,
					; though it's almost beyond belief they aren't there
; move these into wfftst.lisp
(defun eta-normal-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (eta-normal-p gwff)))
	((lsymbol-q gwff) t)
	((boundwff-q gwff)
	 (if (and (eq (binding gwff) 'lambda)
		  (core::eta-applic-p (gdr gwff))
		  (equal (bindvar gwff) (gdr (gdr gwff)))
		  (not (free-in (bindvar gwff) (gar (gdr gwff)))))
	     nil
	   (eta-normal-p (gdr gwff))))
	(t (and (eta-normal-p (gar gwff))
		(eta-normal-p (gdr gwff))))))

(defun beta-normal-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (beta-normal-p gwff)))
	((lsymbol-q gwff) t)
	((boundwff-q gwff)
	 (beta-normal-p (gdr gwff)))
	((lambda-bd-p (gar gwff))
	 nil)
	(t
	 (and (beta-normal-p (gar gwff))
	      (beta-normal-p (gdr gwff))))))

(defun beta-eta-normal-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (beta-eta-normal-p gwff)))
	((lsymbol-q gwff) t)
	((boundwff-q gwff)
	 (if (and (eq (binding gwff) 'lambda)
		  (core::eta-applic-p (gdr gwff))
		  (equal (bindvar gwff) (gdr (gdr gwff)))
		  (not (free-in (bindvar gwff) (gar (gdr gwff)))))
	     nil
	   (beta-eta-normal-p (gdr gwff))))
	((lambda-bd-p (gar gwff))
	 nil)
	(t (and (beta-eta-normal-p (gar gwff))
		(beta-eta-normal-p (gdr gwff))))))

(context search-analysis)

(defvar background-eproof nil)

(defmexpr set-background-eproof
  (argtypes eproof)
  (argnames epr)
  (arghelp "Eproof")
  (defaultfns (lambda (z)
		(list (if (eq z '$)
			  (cond ((eproof-p current-eproof) current-eproof)
				((eproof-p last-eproof) last-eproof)
				(t '$))
			z))))
  (mhelp "Sets the background eproof to be used by MS98-TRACE.
These are automatically set when nat-etree is run."))

(defun set-background-eproof (epr)
  (setq background-eproof epr))

(defun map-eproof (epr)
  (let* ((epr2 current-eproof)
	 (node1 (when epr (eproof-etree epr)))
	 (node2 (when epr2 (eproof-etree current-eproof)))
	 (m (car (eproof-mating-list epr)))
	 (ca (eproof-connections-array epr))
	 (conns (when (mating-p m) (mating-clist m)))
	 (retval nil)
	 (*ignore-statuses* T))
    (declare (special *ignore-statuses*))
    (set-all-parents node1)
    (unless (and node1 node2
		 (wffeq (get-shallow node1) (get-shallow node2)))
      (throwfail "Cannot build corresponding mating, as background eproof and current-eproof do not have the same shallow formulas"))
    (dolist (p conns)
	    (let* ((color (intern-str (create-namestring 'COLOR)))
		   (p1 (gethash p ca)))
	      (when (and (consp p1) (consp (car p1)))
		(let* ((l1 (caar p1))
		       (l2 (cdar p1))
		       (n1 (cond ((literal-p l1) (literal-name l1))
				 ((etree-p l1) (etree-name l1))
				 (t l1)))
		       (n2 (cond ((literal-p l2) (literal-name l2))
				 ((etree-p l2) (etree-name l2))
				 (t l2)))
		       (e1 (find-etree-node-name n1 node1))
		       (e2 (find-etree-node-name n2 node1)))
		  (when (and e1 e2)
		    (setq retval
			  (acons color (remove-duplicates
					(append (map-to-corresponding-nodes
						 e1 node1 node2)
						(map-to-corresponding-nodes
						 e2 node1 node2)))
				 retval)))))))
					; might be TRUTH's in the etree,
					; eg, from Refl= rewrites in node1,
					; these may correspond to connections in node2
    (dolist (e (find-etree-nodes #'(lambda (x)
				     (or (and (true-p x)
					      (not (etree-positive x)))
					 (and (false-p x)
					      (etree-positive x))))
				 node1))
	    (let ((color (intern-str (create-namestring 'COLOR))))
	      (setq retval (acons color
				  (remove-duplicates
				   (map-to-corresponding-nodes e node1 node2))
				  retval))))
    retval))

(defun map-to-corresponding-nodes (e node1 node2)
  (declare (ignore node1))
  (do ((node e (etree-parent node))
       (anc-list nil (cons node anc-list)))
      ((null node)
       (map-to-corresponding-nodes-1 anc-list node2))))

(defun map-to-corresponding-nodes-1 (anc-list node2)
  (if (rewrite-p node2)
      (cons node2 (map-to-corresponding-nodes-1
		   anc-list (car (etree-components node2))))
    (map-to-corresponding-nodes-2 anc-list node2)))

(defun map-to-corresponding-nodes-2 (anc-list node2)
  (if anc-list
      (let ((node1 (car anc-list)))
	(if (cdr anc-list)
	    (if (rewrite-p node1)
		(map-to-corresponding-nodes-2 (cdr anc-list) node2)
	      (if (or (eq (type-of node1) (type-of node2))
		      (and (or (selection-p node1)
			       (skolem-p node1))
			   (or (selection-p node2)
			       (skolem-p node2))))
		  (if (expansion-p node1)
		      (apply #'append
			     (mapcar #'(lambda (n2)
					 (map-to-corresponding-nodes-1
					  (cdr anc-list) n2))
				     (etree-components node2)))
		    (let* ((kid (cadr anc-list))
			   (n (position kid (etree-components node1))))
		      (if n
			  (cons node2
				(map-to-corresponding-nodes-1
				 (cdr anc-list)
				 (nth n (etree-components node2))))
			(find-etree-nodes #'(lambda (e)
					      (declare (ignore e))
					      T) node2))))
		(find-etree-nodes #'(lambda (e)
				      (declare (ignore e))
				      T) node2)))
	  (find-etree-nodes #'(lambda (e)
				(declare (ignore e))
				T) node2)))
    nil))

(defmexpr eliminate-conj*-rulep-apps
    (argtypes symbol)
  (argnames pfname)
  (arghelp "Name for new proof")
  (defaultfns (lambda (pfname)
		(list (if (eq pfname '$)
			  (if dproof
			      (let ((dproof- (intern-str (format nil "~d-" dproof))))
				(do ((pfname1 (intern-str (create-namestring dproof-)) (intern-str (create-namestring dproof-))))
				    ((not (proof-lines pfname1)) pfname1)))
			    '$)
			pfname))))
  (mhelp "Expands applications of RuleP in the current natural deduction proof
when they can be replaced by a sequence of IConj or EConj applications.

This reverses the effect of the ICONJ* and ECONJ* tactics which are often
used when translating from an expansion proof to a natural deduction proof.

SEE ALSO: ELIMINATE-ALL-RULEP-APPS, ELIMINATE-RULEP-LINE"))

					; destructively modifies a natural deduction proof
					; by checking if applications of RuleP are actually ECONJ* or ICONJ* and
					; if so, expands them into ECONJ & ICONJ applications
(defun eliminate-conj*-rulep-apps (newpf)
  (create-subproof (mapcar #'(lambda (x)
			       (cons x x))
			   (list (linealias (car (proof-lines dproof)))
				 (linealias (car (last (proof-lines dproof))))))
		   newpf)
  (dolist (line (proof-lines dproof))
    (when (string= (car (line-justification line)) "RuleP")
      (let* ((concl (line-assertion line))
	     (supps (third (line-justification line)))
	     (prems (mapcar #'(lambda (x)
				(line-assertion x)) supps)))
	(cond ((member concl prems :test #'wffeq-ab) ; just in case SAME will do
	       (dolist (supp supps)
		 (when (wffeq-ab concl (line-assertion supp))
		   (plan line)
		   (ml::same-short line supp)
		   (return t))))
	      ((iconj*-rulep-app-p concl prems)
	       (plan line)
	       (eliminate-iconj*-rulep-app (mapcar #'cons prems supps) line concl))
	      ((econj*-rulep-app-p concl prems)
	       (plan line)
	       (eliminate-econj*-rulep-app (car supps) (car prems) line concl))))))
  (cleanup)
  (remove-unnecessary-gaps))

(defun eliminate-iconj*-rulep-app (sassoc pline pwff)
  (let ((a (assoc pwff sassoc :test #'wffeq-ab)))
    (if a
	(ml::same-short pline (cdr a))
      (let ((pln (linealias pline)))
	(introduce-gap pline 2)
	(comdecode (list 'cl-user::iconj (+ pln 2) (+ pln 1) pln '$ '$ '$ '$ '$))
	(let ((lpline (numalias pln))
	      (rpline (numalias (+ pln 1))))
	  (eliminate-iconj*-rulep-app sassoc lpline (cdar pwff))
	  (eliminate-iconj*-rulep-app sassoc rpline (cdr pwff)))))))

(defun eliminate-econj*-rulep-app (sline swff pline pwff)
  (if (wffeq-ab swff pwff)
      (ml::same-short pline sline)
    (let ((sln (linealias sline))
	  (pln (linealias pline)))
      (introduce-gap pline 2)
      (comdecode (list 'cl-user::econj sln (+ pln 1) pln
		       '$ '$ '$ '$ '$))
      (if (member pwff (list-of-conjuncts (cdar swff)) :test #'wffeq-ab)
	  (eliminate-econj*-rulep-app (numalias pln) (cdar swff) pline pwff)
	(eliminate-econj*-rulep-app (numalias (+ pln 1)) (cdr swff) pline pwff)))))

(defun iconj*-rulep-app-p (concl prems)
  (premisses-cover-conjuncts-p prems concl))

(defun premisses-cover-conjuncts-p (prems concl)
  (or (member concl prems :test #'wffeq-ab)
      (and (and-p concl)
	   (premisses-cover-conjuncts-p prems (cdar concl))
	   (premisses-cover-conjuncts-p prems (cdr concl)))))

(defun econj*-rulep-app-p (concl prems)
  (and (= (length prems) 1)
       (member concl (list-of-conjuncts (car prems))
	       :test #'wffeq-ab)))

(defun list-of-conjuncts (wff)
  (if (and-p wff)
      (cons wff				; note that I'm including the conjunction as a conjunct
	    (append (list-of-conjuncts (cdar wff))
		    (list-of-conjuncts (cdr wff))))
    (list wff)))

  
(defmexpr eliminate-all-rulep-apps
    (argtypes symbol)
  (argnames pfname)
  (arghelp "Name for new proof")
  (defaultfns (lambda (pfname)
		(list (if (eq pfname '$)
			  (if dproof
			      (let ((dproof- (intern-str (format nil "~d-" dproof))))
				(do ((pfname1 (intern-str (create-namestring dproof-)) (intern-str (create-namestring dproof-))))
				    ((not (proof-lines pfname1)) pfname1)))
			    '$)
			pfname))))
  (mhelp "Expands applications of RuleP in the current natural deduction proof
into more primitive rules.  This works by calling fast propositional search with
the current flag settings except USE-RULEP is set to NIL.
BASIC-PROP-TAC is used to translate to natural deduction.

This command also eliminates other 'fancy' propositional justifications:
Assoc (Assoc-Left), EquivConj (in favor of EquivImplics),
Imp-Disj-L, Imp-Disj-R, Imp-Disj, Disj-Imp-L, Disj-Imp-R, and Disj-Imp.

See Also: 
ELIMINATE-RULEP-LINE - which eliminates a particular application of RuleP.
ELIMINATE-CONJ*-RULEP-APPS - which does not depend on automatic search."))


(defmexpr eliminate-rulep-line
    (argtypes existing-line)
  (argnames line)
  (arghelp "Line Justified by RuleP or other Prop Rule")
  (mhelp "Expands an application of RuleP in the current natural deduction proof
into more primitive rules.  This works by calling fast propositional search with
the current flag settings except USE-RULEP is set to NIL.
BASIC-PROP-TAC is used to translate to natural deduction.

This command can also eliminate other 'fancy' propositional justifications:
Assoc (Assoc-Left), EquivConj (in favor of EquivImplics),
Imp-Disj-L, Imp-Disj-R, Imp-Disj, Disj-Imp-L, Disj-Imp-R, and Disj-Imp.

SEE ALSO: ELIMINATE-ALL-RULEP-APPS, ELIMINATE-CONJ*-RULEP-APPS"))


					; destructively modifies a natural deduction proof
(defun eliminate-all-rulep-apps (&optional newpf)
  (when newpf
    (create-subproof (mapcar #'(lambda (x)
				 (cons x x))
			     (list (linealias (car (proof-lines dproof)))
				   (linealias (car (last (proof-lines dproof))))))
		     newpf))
  (let ((rulep-like-justifications 
	 '("RuleP" "Assoc" "EquivConj" "Imp-Disj-L" "Imp-Disj-R" "Imp-Disj"
	   "Disj-Imp-L" "Disj-Imp-R" "Disj-Imp"))
	(changed nil))
    (dolist (line (proof-lines dproof))
      (when (member (car (line-justification line)) 
		    rulep-like-justifications
		    :test #'string=)
	(setq changed t)
	(eliminate-rulep-line line)))
    (when changed
      (cleanup)
      (remove-unnecessary-gaps))
    changed))

					; destructively modifies a natural deduction proof
(defun eliminate-all-ruleq-apps ()
  (let ((changed nil))
    (dolist (line (proof-lines dproof))
	    (when (string= (car (line-justification line)) "RuleQ")
	      (setq changed t)
	      (if (ugen*-inference-p line)
		  (let ((supp (car (third (line-justification line))))
			(pline line))
		    (plan pline)
		    (unless (subsetp (line-hypotheses line) (line-hypotheses supp))
		      (introduce-gap pline 1)
		      (comdecode (list 'cl-user::same (linealias pline) (- (linealias pline) 1)
				       '$ '$ (list 'quote
						   (mapcar #'(lambda (x)
							       (linealias x))
							   (line-hypotheses supp)))))
		      (setq pline (numalias (- (linealias pline) 1))))
		    (fill-in-ugen*-gap supp pline (line-assertion supp)))
		(eliminate-ruleq-line line))))
    (when changed
      (cleanup)
      (remove-unnecessary-gaps))
    changed))

(defun fill-in-ugen*-gap (supp pline swff)
  (if (wffeq swff (line-assertion pline))
      (comdecode (list 'cl-user::same (linealias pline) (linealias supp) '$ '$ '$))
    (progn
      (introduce-gap pline 1)
      (comdecode (list 'cl-user::ugen (linealias pline) (- (linealias pline) 1) '$ '$ '$ '$))
      (fill-in-ugen*-gap supp (numalias (- (linealias pline) 1)) swff))))

(defun ugen*-inference-p (line)
  (let ((supp (third (line-justification line))))
    (if (and (= (length supp) 1)
	     (subsetp (line-hypotheses (car supp)) (line-hypotheses line)))
	(let ((swff (line-assertion (car supp)))
	      (pwff (line-assertion line))
	      (hvars (free-vars-in-lines (line-hypotheses (car supp)))))
	  (ugen*-inference-p-1 swff pwff hvars))
      nil)))

(defun ugen*-inference-p-1 (swff pwff hvars)
  (or (wffeq swff pwff)
      (and (a-bd-wff-p pwff)
	   (not (member (bindvar pwff) hvars))
	   (ugen*-inference-p-1 swff (cdr pwff) hvars))))
		
					; This depends on the gap being filled by automatic search
(defun eliminate-ruleq-line (line0)
  (let ((line (create-lambda-normal-gap line0)))
    (dolist (supp (line-support line))
	    (let ((ok-vars (delete-duplicates
			    (nset-difference (free-vars-of (line-assertion supp))
					     (free-vars-in-lines (line-hypotheses supp)))))
		  (ln (linealias supp))
		  (pln (linealias line)))
	      (dolist (u ok-vars)
		      (introduce-gap (numalias pln) 1)
		      (comdecode (list 'cl-user::lemma (+ pln 1) pln
				       (list 'quote
					     (acons u 'FORALL
						    (line-assertion
						     (numalias ln))))
				       '$ '$ '$))
		      (comdecode (list 'cl-user::ugen pln ln '$ '$ '$ '$))
		      (unsponsor (numalias (+ pln 1)) (list (numalias ln)))
		      (setq ln pln)
		      (setq pln (+ 1 pln)))))
    (introduce-gap line 100)
    (diy-real line (line-support line) t)))

(defun create-lambda-normal-gap (line)
  (let ((supps (third (line-justification line)))
	(supps2 nil))
					; first make sure all the lines are lambda normal, or we may have
					; trouble with etree-nat after merging
    (dolist (l supps)
	    (let* ((wff (line-assertion l))
		   (bwff (lambda-norm wff))
		   (ewff (lnorm-eta bwff))
		   (s1 (not (wffeq-ab wff bwff)))
		   (s2 (not (wffeq-ab bwff ewff))))
	      (cond ((and s1 (member lambda-conv '(beta-only beta-eta-separate)))
		     (introduce-gap line 1)
		     (comdecode (list 'cl-user::lcontr*-beta
				      (linealias l) (- (linealias line) 1)
				      '$ '$ '$ '$))
		     (when (and s2 (eq lambda-conv 'beta-eta-separate))
		       (introduce-gap line 1)
		       (comdecode (list 'cl-user::lcontr*-eta
					(- (linealias line) 2) (- (linealias line) 1)
					'$ '$ '$ '$)))
		     (push (numalias (- (linealias line) 1)) supps2))
		    ((and s2 (eq lambda-conv 'beta-eta-separate)) ; eta
		     (introduce-gap line 1)
		     (comdecode (list 'cl-user::lcontr*-eta
				      (linealias l) (- (linealias line) 1)
				      '$ '$ '$ '$))
		     (push (numalias (- (linealias line) 1)) supps2))
		    ((or s1 s2) ; lambda
		     (introduce-gap line 1)
		     (comdecode (list 'cl-user::lcontr*
				      (linealias l) (- (linealias line) 1)
				      '$ '$ '$ '$))
		     (push (numalias (- (linealias line) 1)) supps2))
		    (t (push l supps2)))))
    (plan line)
    (let* ((wff (line-assertion line))
	   (bwff (lambda-norm wff))
	   (ewff (lnorm-eta bwff))
	   (s1 (not (wffeq-ab wff bwff)))
	   (s2 (not (wffeq-ab bwff ewff))))
      (cond ((and s1 (member lambda-conv '(beta-only beta-eta-separate)))
	     (introduce-gap line 1)
	     (comdecode (list 'cl-user::lexpd*-beta
			      (linealias line) (- (linealias line) 1)
			      '$ '$ '$ '$))
	     (setq line (numalias (- (linealias line) 1)))
	     (when (and s2 (eq lambda-conv 'beta-eta-separate))
	       (introduce-gap line 1)
	       (comdecode (list 'cl-user::lexpd*-eta
				(linealias line) (- (linealias line) 1)
				'$ '$ '$ '$))
	       (setq line (numalias (- (linealias line) 1)))))
	    ((and s2 (eq lambda-conv 'beta-eta-separate)) ; eta
	     (introduce-gap line 1)
	     (comdecode (list 'cl-user::lexpd*-eta
			      (linealias line) (- (linealias line) 1)
			      '$ '$ '$ '$))
	     (setq line (numalias (- (linealias line) 1))))
	    ((or s1 s2) ; lambda
	     (introduce-gap line 1)
	     (comdecode (list 'cl-user::lexpd*
			      (linealias line) (- (linealias line) 1)
			      '$ '$ '$ '$))
	     (setq line (numalias (- (linealias line) 1))))))
    (unsponsor line (line-support line))
    (sponsor line supps2) ; destructively modifies supps!
    line))

					; This code closely resembles diy, but uses fast prop search
					; I did it this (not very pretty) way because etree-nat needs
					; there to be actual etrees associated with line numbers -
					; otherwise I could just build the jform & get the mating with fast prop search
					; - cebrown 2/17/01
(defun eliminate-rulep-line (line)
  (fill-in-propositional-gap (create-lambda-normal-gap line)))

(defun fill-in-propositional-gap (line)
    (let* ((support (line-support line))
	   (cwff (line-assertion line))
	   supp-nodes
	   left-node right-node
	   (REWRITE-EQUIVS 4)		; always want to use defn of EQUIV in terms of AND & IMPLIES - just to make things easier
					; when we normalize - note that BASIC-PROP-TAC cannot handle EQUIV in any other way
	   (REWRITE-EQUALITIES 'NONE)
	   (REWRITE-DEFNS '(NONE))
	   (REMOVE-LEIBNIZ NIL)
	   (FIRST-ORDER-MODE-MS nil)
	   (ALLOW-NONLEAF-CONNS '(ALL))) ; so we can mate non-literals
      (declare (special ALLOW-NONLEAF-CONNS FIRST-ORDER-MODE-MS REWRITE-EQUIVS
			REWRITE-EQUALITIES REWRITE-DEFNS REMOVE-LEIBNIZ))
      (let* ((f (gwff-to-rulep-ftree cwff nil))
	     (fh (mapcar #'(lambda (l)
			     (gwff-to-rulep-ftree (line-assertion l) t))
			 support))
	     (fc (make-ftree-conjunction fh))
	     (imp (if fc
		      (make-ftree-imp fc f)
		    f)))
	(ftree-to-etree imp)
	(setq supp-nodes
	      (mapcar #'(lambda (g)
			  (find-etree-node 
			   #'(lambda (n)
			       (eq (etree-name n) (ftree-name g)))))
		      fh))
      (setq left-node
	    (if fh
		(car (etree-components current-topnode))
	      nil))
      (setq right-node (if fh
			   (cadr (etree-components current-topnode))
			 current-topnode))
      (do ((supp-nodes supp-nodes (cdr supp-nodes))
	   (support support (cdr support)))
	  ((null support))
	(setf (line-node (car support)) (car supp-nodes)))
      (init-symmetry imp current-eproof)
      (setf (line-node line) right-node)
      (relabel-etree (eproof-etree current-eproof))
      (update-statuses (eproof-etree current-eproof))
      (let* ((conjs
	      (cons (etree-to-rulep-jform right-node)
		    (mapcar #'(lambda (n)
				(etree-to-rulep-jform n))
			    supp-nodes)))
	     (j (make-conjunction :components conjs
				  :type 'conjunction)))
	(dolist (conj conjs)
	  (setf (jform-parent conj) j))
	(setq j (normalize-jform j))
	(multiple-value-bind
	    (conns closed)
	    (prop-msearch-silent j)
	  (setq conns (remove t conns))	; special case when there are empty disjunctions 5/4/01
	  (if (eq closed 'fail)
	      (throwfail "Propositional Search Failed - Invalid Application of RuleP at line " (linealias line) "?")
	    (let ((MAX-MATES :INFINITE))
	      (declare (special MAX-MATES))
	      (start-new-mating)
	      (setf (eproof-jform current-eproof)
		(etree-to-jform (eproof-etree current-eproof)))
	      (dolist (conn conns)
		(add-conn (princ-to-string (car conn)) (princ-to-string (cdr conn))))
	      (setf (mating-completep active-mating) t)
	      (let ((*fps-succeeded* t))
		(declare (special *fps-succeeded*))
		(merge-tree)
		(dolist (hxsupport support)
		  (let ((newnode (update-line-node hxsupport current-topnode)))
		    (if newnode (setf (line-node hxsupport) newnode))))
		(if left-node
		    (setf (eproof-etree current-eproof)
		      (cadr (etree-components (eproof-etree current-eproof)))))
		(let ((*fps-succeeded* nil))
		  (declare (special *fps-succeeded*))
		  (etree-nat dproof (linealias line) 'ML::BASIC-PROP-TAC nil))))))))
   (cleanup)
   (remove-unnecessary-gaps)))


					; for converting natrees to ND proofs - in case I ever need it
(defun ceb-natree-to-proof (na)
  (prove1 (natree-assertion na) dproof 1)
  (ceb-natree-to-proof-rec na (numalias 1))
  (cleanup)
  (remove-unnecessary-gaps))

(defun ceb-natree-to-proof-rec (na line &optional hyps)
  (let ((ln (linealias line))
	(supp1 (car (natree-support na)))
	(supp2 (cadr (natree-support na)))
	(supp3 (caddr (natree-support na)))
	(wff (natree-assertion na)))
    (case (natree-justification na)
      (conj (if (eq (natree-annotation na) 'extraction) ; ECONJ
		(let ((conj (natree-assertion supp1)))
		  (introduce-gap line 2)
		  (comdecode (list 'cl-user::lemma (+ ln 2) ln
				   (list 'quote conj)
				   '$ '$ '$))
		  (if (wffeq wff (cdar conj)) ; first conjunct
		      (comdecode (list 'cl-user::econj ln (+ ln 1) (+ ln 2)
				       '$ '$ '$ '$ '$))
		    (comdecode (list 'cl-user::econj ln (+ ln 2) (+ ln 1) ; else second conjunct
				     '$ '$ '$ '$ '$)))
		  (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
	      (progn ; else ICONJ
		(introduce-gap line 2)
		(comdecode (list 'cl-user::iconj (+ ln 2) (+ ln 1) ln
				 '$ '$ '$ '$ '$))
		(let ((line1 (numalias ln))
		      (line2 (numalias (+ ln 1)))) ; line numbers may change in rec call because of introduce-gap
		  (ceb-natree-to-proof-rec supp1 line1 hyps)
		  (ceb-natree-to-proof-rec supp2 line2 hyps)))))
      (deduct 
       (let ((hyp (car (setdiff (natree-hypo supp1) (natree-hypo na)))))
	 (introduce-gap line 2)
	 (comdecode (list 'cl-user::deduct (+ ln 2) (+ ln 1) ln
			  '$ '$ '$ '$ '$))
	 (ceb-natree-to-proof-rec supp1 (numalias (+ ln 1))
				  (if hyp
				      (acons hyp (numalias ln) hyps)
				    hyps))))
      ((hyp choose |CASE 1| |CASE 2| |CASE 3| |CASE 4| |ASSUME NEGATION|)
       (let ((a (assoc na hyps)))
	 (unless a
	   (throwfail "Could not find hyp for natree " na))
	 (comdecode (list 'cl-user::same ln (linealias (cdr a)) '$ '$ '$))))
      (rulec 
       (let ((exwff (natree-assertion supp1))
	     (hyp (car (setdiff (natree-hypo supp2) (natree-hypo na))))
	     (y (car (natree-jterm na))))
	 (introduce-gap line 3)
	 (comdecode (list 'cl-user::lemma (+ ln 3) ln (list 'quote exwff) '$ '$ '$))
	 (comdecode (list 'cl-user::rulec (+ ln 3) ln (+ ln 2) (+ ln 1)
			  (list 'quote y)
			  '$ '$ '$ '$ '$ '$))
	 (let ((line2 (numalias (+ ln 2)))
	       (line1 (numalias ln))
	       (hline (numalias (+ ln 1))))
	   (ceb-natree-to-proof-rec supp1 line1 hyps)
	   (ceb-natree-to-proof-rec supp2 line2
				    (if hyp
					(acons hyp hline hyps)
				      hyps)))))
      ((SAME |SAME AS|)
       (introduce-gap line 1)
       (comdecode (list 'cl-user::same (+ ln 1) ln '$ '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (EGEN
       (introduce-gap line 1)
       (let ((swff (natree-assertion supp1)))
	 (comdecode (list 'cl-user::lemma (+ ln 1) ln 
			  (list 'quote swff) '$ '$ '$))
	 (comdecode (list 'cl-user::egen (+ ln 1) ln
			  (list 'quote (car (natree-jterm na)))
			  '$ '$ '$ '$ '$))
	 (ceb-natree-to-proof-rec supp1 (numalias ln) hyps)))
      (UI
       (let ((swff (natree-assertion supp1)))
	 (introduce-gap line 1)
	 (comdecode (list 'cl-user::ui ln (+ ln 1)
			  (list 'quote (car (natree-jterm na)))
			  (list 'quote (cdr swff))
			  (list 'quote (bindvar swff))
			  '$ '$ '$))
	 (ceb-natree-to-proof-rec supp1 (numalias ln) hyps)))
      (UGEN
       (let ((x (car (natree-jterm na))))
	 (if (eq x (bindvar wff))	; may have been renamed to get a global eigenvariable cond to hold
	     (progn
	       (introduce-gap line 1)
	       (comdecode (list 'cl-user::ugen (+ ln 1) ln '$ '$ '$ '$)))
	   (progn
	     (introduce-gap line 2)
	     (comdecode (list 'cl-user::ab* (+ ln 1) (+ ln 2) '$
			      (list 'quote (ab-change wff x)) '$ '$))
	     (comdecode (list 'cl-user::ugen (+ ln 1) ln '$ '$ '$ '$))))
	 (ceb-natree-to-proof-rec supp1 (numalias ln) hyps)))
      (MP
       (introduce-gap line 2)
       (comdecode (list 'cl-user::mp (+ ln 1) (+ ln 2) ln
			'$ (list 'quote (natree-assertion supp1)) '$ '$ '$))
       (let ((line1 (numalias ln))
	     (line2 (numalias (+ ln 1))))
	 (ceb-natree-to-proof-rec supp1 line1 hyps)
	 (ceb-natree-to-proof-rec supp2 line2 hyps)))
      (AB (introduce-gap line 1)
	  (comdecode (list 'cl-user::ab* ln (+ ln 1) '$
			   (list 'quote (natree-assertion supp1)) '$ '$))
	  (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      ((LAMBDA LAMBDA=)
       (introduce-gap line 1)
       (comdecode (list 'cl-user::lambda* ln (+ ln 1) '$
			(list 'quote (natree-assertion supp1)) '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (|ETA RULE|
       (introduce-gap line 1)
       (comdecode (list 'cl-user::eta* ln (+ ln 1) '$
			(list 'quote (natree-assertion supp1)) '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (|BETA RULE|
       (introduce-gap line 1)
       (comdecode (list 'cl-user::beta* ln (+ ln 1) '$
			(list 'quote (natree-assertion supp1)) '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (NEGELIM
       (introduce-gap line 2)
       (comdecode (list 'cl-user::eneg (+ ln 2) ln (+ ln 1)
			(list 'quote (natree-assertion supp2))
			'$ '$ '$))
       (let ((line1 (numalias ln))
	     (line2 (numalias (+ ln 1))))
	 (ceb-natree-to-proof-rec supp1 line1 hyps)
	 (ceb-natree-to-proof-rec supp2 line2 hyps)))
      (NEGINTRO
       (let ((hyp (car (setdiff (natree-hypo supp1) (natree-hypo na)))))
	 (introduce-gap line 2)
	 (comdecode (list 'cl-user::ineg (+ ln 2) (+ ln 1) ln
			  '$ '$ '$))
	 (let ((hline (numalias ln)))
	   (ceb-natree-to-proof-rec supp1 (numalias (+ ln 1)) (if hyp
								  (acons hyp hline hyps)
								hyps)))))
      (INDIRECT
       (let ((hyp (car (setdiff (natree-hypo supp1) (natree-hypo na)))))
	 (introduce-gap line 2)
	 (comdecode (list 'cl-user::indirect (+ ln 2) (+ ln 1) ln '$ '$ '$ '$))
	 (ceb-natree-to-proof-rec supp1 (numalias (+ ln 1))
				  (if hyp
				      (acons hyp (numalias ln) hyps)
				    hyps))))
      (ABSURD
       (introduce-gap line 1)
       (comdecode (list 'cl-user::absurd (+ ln 1) ln '$ '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (CASES
       (let ((supps (natree-support na))
	     (disj (natree-assertion supp1)))
	 (cond ((= (length supps) 3)
		(introduce-gap line 5)
		(comdecode (list 'cl-user::cases (+ ln 5) ln (+ ln 4) (+ ln 3) (+ ln 2) (+ ln 1)
				 (list 'quote (cdr disj))
				 (list 'quote (cdar disj)) '$ '$ '$ '$ '$ '$ '$))
		(let ((hline1 (numalias (+ ln 1)))
		      (hyp1 (car (setdiff (natree-hypo supp2) (natree-hypo na))))
		      (hline2 (numalias (+ ln 3)))
		      (hyp2 (car (setdiff (natree-hypo supp3) (natree-hypo na))))
		      (dline (numalias ln))
		      (cline1 (numalias (+ ln 2)))
		      (cline2 (numalias (+ ln 4))))
		  (ceb-natree-to-proof-rec supp1 dline hyps)
		  (ceb-natree-to-proof-rec supp2 cline1
					   (if hyp1
					       (acons hyp1 hline1 hyps)
					     hyps))
		  (ceb-natree-to-proof-rec supp3 cline2
					   (if hyp2
					       (acons hyp2 hline2 hyps)
					     hyps))))
	       ((= (length supps) 4)
		(introduce-gap line 7)
		(comdecode (list 'cl-user::cases3 (+ ln 7) ln (+ ln 6) (+ ln 5)(+ ln 4) (+ ln 3) (+ ln 2) (+ ln 1)
				 (list 'quote (cdr disj))
				 (list 'quote (cddar disj))
				 (list 'quote (cdadar disj))
				 '$ '$ '$ '$ '$ '$ '$ '$ '$))
		(let* ((supp4 (nth 3 supps))
		       (hline1 (numalias (+ ln 1)))
		       (hyp1 (car (setdiff (natree-hypo supp2) (natree-hypo na))))
		       (hline2 (numalias (+ ln 3)))
		       (hyp2 (car (setdiff (natree-hypo supp3) (natree-hypo na))))
		       (hline3 (numalias (+ ln 5)))
		       (hyp3 (car (setdiff (natree-hypo supp4) (natree-hypo na))))
		       (dline (numalias ln))
		       (cline1 (numalias (+ ln 2)))
		       (cline2 (numalias (+ ln 4)))
		       (cline3 (numalias (+ ln 6))))
		  (ceb-natree-to-proof-rec supp1 dline hyps)
		  (ceb-natree-to-proof-rec supp2 cline1
					   (if hyp1
					       (acons hyp1 hline1 hyps)
					     hyps))
		  (ceb-natree-to-proof-rec supp3 cline2
					   (if hyp2
					       (acons hyp2 hline2 hyps)
					     hyps))
		  (ceb-natree-to-proof-rec supp4 cline3
					   (if hyp3
					       (acons hyp3 hline3 hyps)
					     hyps))))
	       ((= (length supps) 5)
		(introduce-gap line 9)
		(comdecode (list 'cl-user::cases4 (+ ln 9) ln (+ ln 8) (+ ln 7) (+ ln 6) (+ ln 5)(+ ln 4) (+ ln 3) (+ ln 2) (+ ln 1)
				 (list 'quote (cdr disj))
				 (list 'quote (cddar disj))
				 (list 'quote (cdr (cdadar disj)))
				 (list 'quote (cdar (cdadar disj)))
				 '$ '$ '$ '$ '$ '$ '$ '$ '$ '$ '$))
		(let* ((supp4 (nth 3 supps))
		       (supp5 (nth 4 supps))
		       (hline1 (numalias (+ ln 1)))
		       (hyp1 (car (setdiff (natree-hypo supp2) (natree-hypo na))))
		       (hline2 (numalias (+ ln 3)))
		       (hyp2 (car (setdiff (natree-hypo supp3) (natree-hypo na))))
		       (hline3 (numalias (+ ln 5)))
		       (hyp3 (car (setdiff (natree-hypo supp4) (natree-hypo na))))
		       (hline4 (numalias (+ ln 7)))
		       (hyp4 (car (setdiff (natree-hypo supp5) (natree-hypo na))))
		       (dline (numalias ln))
		       (cline1 (numalias (+ ln 2)))
		       (cline2 (numalias (+ ln 4)))
		       (cline3 (numalias (+ ln 6)))
		       (cline4 (numalias (+ ln 8))))
		  (ceb-natree-to-proof-rec supp1 dline hyps)
		  (ceb-natree-to-proof-rec supp2 cline1
					   (if hyp1
					       (acons hyp1 hline1 hyps)
					     hyps))
		  (ceb-natree-to-proof-rec supp3 cline2
					   (if hyp2
					       (acons hyp2 hline2 hyps)
					     hyps))
		  (ceb-natree-to-proof-rec supp4 cline3
					   (if hyp3
					       (acons hyp3 hline3 hyps)
					     hyps))
		  (ceb-natree-to-proof-rec supp5 cline4
					   (if hyp4
					       (acons hyp4 hline4 hyps)
					     hyps))))
	       ((< (length supps) 3)
		(throwfail "Cases does not have enough support lines: " na))
	       (t (throwfail "Cannot handle more than 4 cases at once: " na)))))
      (IDISJ-L
       (introduce-gap line 1)
       (comdecode (list 'cl-user::idisj-left (+ ln 1) ln '$ '$ '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (IDISJ-R
       (introduce-gap line 1)
       (comdecode (list 'cl-user::idisj-right (+ ln 1) ln '$ '$ '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      ((RULEP TRUTHP)
       (let* ((supps (natree-support na))
	      (n (length supps))
	      (lines nil)
	      (linenums nil))
	 (introduce-gap line n)
	 (do ((supps2 supps (cdr supps2))
	      (i 0 (+ i 1)))
	     ((not supps2))
	   (push (+ ln i) linenums)
	   (comdecode (list 'cl-user::lemma (+ ln n) (+ ln i)
			    (list 'quote (natree-assertion (car supps2)))
			    '$ '$ '$))
	   (push (cons (car supps2) (numalias (+ ln i))) lines))
	 (comdecode (list 'cl-user::rulep (+ ln n) (reverse linenums)))
	 (dolist (na-line lines)
	   (ceb-natree-to-proof-rec (car na-line) (cdr na-line) hyps))))
					; RuleQ ?
      (EXT=
       (let ((swff (natree-assertion supp1)))
	 (if (boundwff-p swff)		; functional ext
	     (progn
	       (introduce-gap line 1)
	       (comdecode (list 'cl-user::ext= (+ ln 1) ln (list 'quote (bindvar swff))
				'$ '$ '$ '$))
	       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
	   (progn			; prop ext
	     (introduce-gap line 1)
	     (comdecode (list 'cl-user::ext=0 (+ ln 1) ln
			      '$ '$ '$ '$))
	     (ceb-natree-to-proof-rec supp1 (numalias ln) hyps)))))
      (IMPEQUIV
       (introduce-gap line 1)
       (comdecode (list 'cl-user::implics-equiv (+ ln 1) ln
			'$ '$ '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (EQUIVIMP
       (introduce-gap line 1)
       (comdecode (list 'cl-user::equiv-implics ln (+ ln 1) '$ '$ '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      ((SUBST= SUB=)
       (let ((eqn (natree-assertion supp2)))
	 (introduce-gap line 2)
	 (comdecode (list 'cl-user::subst= (+ ln 1) (+ ln 2) ln
			  (list 'quote (natree-assertion supp1)) '$
			  (list 'quote (cdr eqn)) (list 'quote (cdar eqn))
			  '$ '$ '$))
	 (let ((line1 (numalias ln))
	       (line2 (numalias (+ ln 1))))
	   (ceb-natree-to-proof-rec supp1 line1 hyps)
	   (ceb-natree-to-proof-rec supp2 line2 hyps))))
      ((DEFN EQUIVWFFS REFL=)
       (introduce-gap line 1)
       (comdecode (list 'cl-user::equiv-wffs ln (+ ln 1) '$
			(list 'quote (natree-assertion supp1)) '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      ((EQUIV-EQ EQUALITY)
       (introduce-gap line 1)
       (comdecode (list 'cl-user::equiv-eq ln (+ ln 1) '$
			(list 'quote (natree-assertion supp1)) '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      ((|ASSERT REFL=|)
       (comdecode (list 'cl-user::assert (list 'quote 'REFL=) ln)))
      (NEG
       (introduce-gap line 1)
       (if (eq (natree-annotation na) 'EXTRACTION)
	   (comdecode (list 'cl-user::pushneg ln (+ ln 1) 
			    (list 'quote (cdr (natree-assertion supp1))) '$ '$ '$))
	 (comdecode (list 'cl-user::pullneg (+ ln 1) ln
			  '$ '$ '$ '$)))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (NNF
       (introduce-gap line 1)
       (comdecode (list 'cl-user::nnf ln (+ ln 1) 
			(list 'quote (natree-assertion supp1)) '$ '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      (NNF-EXPAND
       (introduce-gap line 1)
       (comdecode (list 'cl-user::nnf-expand (+ ln 1) ln '$ '$ '$ '$))
       (ceb-natree-to-proof-rec supp1 (numalias ln) hyps))
      ((coercion backwards-coercion)
       (ceb-natree-to-proof-rec supp1 line hyps))
      (otherwise (throwfail "Cannot handle rule " (natree-justification na))))))


(defun expand-cases ()
  (let ((changed nil))
    (dolist (line (proof-lines dproof))
	    (when (and (string= (car (line-justification line)) "Cases")
		       (> (length (caddr (line-justification line))) 3))
	      (let ((disj (car (third (line-justification line))))
		    (cases (cdr (third (line-justification line)))))
		(setq changed t)
		(plan line)
		(sponsor line (list disj))
		(fill-in-cases-gap disj cases line))))
    (cleanup)
    (remove-unnecessary-gaps)
    changed))

(defun fill-in-cases-gap (disj cases concl)
  (let ((ch-assoc nil))
  (dolist (case cases) ; first make sure every case really uses the case
	  (let ((h (car (setdiff (line-hypotheses case) (line-hypotheses concl)))))
	    (unless h
	      (comdecode (list 'cl-user::same (linealias concl) (linealias case)))
	      (return-from fill-in-cases-gap t))
	    (setq ch-assoc ; ordered by line #'s of case hyps
		  (merge 'list (list (cons h case)) ch-assoc #'<
			 :key #'(lambda (x)
				  (linealias (car x)))))))
  (let* ((n (length cases)))
    (dolist (ch ch-assoc)
	    (plan (car ch)))
    (cond ((= n 4)
	   (introduce-gap (caar ch-assoc) 2)
	   (introduce-gap (car (nth 3 ch-assoc)) 1)
	   (introduce-gap (car (nth 2 ch-assoc)) 1)
	   (comdecode (list 'cl-user::cases (linealias concl)
			    (linealias disj)
			    (linealias (cdr (nth 3 ch-assoc)))
			    (linealias (car (nth 3 ch-assoc)))
			    (- (linealias (car (nth 3 ch-assoc))) 1)
			    (- (linealias (caar ch-assoc)) 2)
			    '$ '$ '$ '$))
	   (dolist (l (proof-lines dproof))
		   (when (member (car (nth 2 ch-assoc)) (line-hypotheses l))
		     (push (numalias (- (linealias (caar ch-assoc)) 2))
			   (line-hypotheses l))))
	   (comdecode (list 'cl-user::cases
			    (- (linealias (car (nth 3 ch-assoc))) 1)
			    (- (linealias (caar ch-assoc)) 2)
			    (linealias (cdr (nth 2 ch-assoc)))
			    (linealias (car (nth 2 ch-assoc)))
			    (- (linealias (car (nth 2 ch-assoc))) 1)
			    (- (linealias (caar ch-assoc)) 1)
			    '$ '$ '$ '$))
	   (dolist (l (proof-lines dproof))
		   (when (or (member (caar ch-assoc) (line-hypotheses l))
			     (member (caadr ch-assoc) (line-hypotheses l)))
		     (push (numalias (- (linealias (caar ch-assoc)) 2))
			   (line-hypotheses l))
		     (push (numalias (- (linealias (caar ch-assoc)) 1))
			   (line-hypotheses l))))
	   (comdecode (list 'cl-user::cases
			    (- (linealias (car (nth 2 ch-assoc))) 1)
			    (- (linealias (caar ch-assoc)) 1)
			    (linealias (cdr (nth 1 ch-assoc)))
			    (linealias (car (nth 1 ch-assoc)))
			    (linealias (cdar ch-assoc))
			    (linealias (caar ch-assoc))
			    '$ '$ '$ '$)))
	  ((= n 3)
	   (introduce-gap (caar ch-assoc) 1)
	   (introduce-gap (car (nth 2 ch-assoc)) 1)
	   (comdecode (list 'cl-user::cases (linealias concl)
			    (linealias disj)
			    (linealias (cdr (nth 2 ch-assoc)))
			    (linealias (car (nth 2 ch-assoc)))
			    (- (linealias (car (nth 2 ch-assoc))) 1)
			    (- (linealias (caar ch-assoc)) 1)
			    '$ '$ '$ '$))
	   (dolist (l (proof-lines dproof))
		   (when (member (car (nth 1 ch-assoc)) (line-hypotheses l))
		     (push (numalias (- (linealias (caar ch-assoc)) 1))
			   (line-hypotheses l))))
	   (comdecode (list 'cl-user::cases
			    (- (linealias (car (nth 2 ch-assoc))) 1)
			    (- (linealias (caar ch-assoc)) 1)
			    (linealias (cdr (nth 1 ch-assoc)))
			    (linealias (car (nth 1 ch-assoc)))
			    (linealias (cdar ch-assoc))
			    (linealias (caar ch-assoc))
			    '$ '$ '$ '$)))
	  ((= n 2)
	   (comdecode (list 'cl-user::cases
			    concl
			    (linealias (cdr (nth 1 ch-assoc)))
			    (linealias (car (nth 1 ch-assoc)))
			    (linealias (cdar ch-assoc))
			    (linealias (caar ch-assoc))
			    '$ '$ '$ '$)))
	  (t (throwfail "Cases rule should have 2, 3, or 4 cases, not " n " cases."))))))

