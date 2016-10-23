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
; into sequent calculus derivations, then to ftrees representations of exp proofs
; The ideas are in Frank's ATP class notes.
; The procedure is as follows:

; 1. Annotate the lines of the natural deduction proof, as
;    in Frank Pfenning's ATP notes:
;      EXTRACTION (down arrow)
;      NORMAL (double up arrow)
;    by setting these properties of lines

; 2. If there are no backwards coercions (no extraction line depends on a normal line),
;    then the proof is "normal",
;    and we can construct a cut-free sequent calculus derivation.
;    otherwise we use a CUT node, which we might be able to eliminate before
;    converting to an ftree
(defun natree-to-ftree-main (&optional (natree current-natree))
  (multiple-value-bind
   (pos neg ftree-clist)
   (cutfree-ftree-seq-to-ftrees
    (new-ftree-seq-der
     (intern-str (format nil "~d-CUTFREE-SEQ" (natree-name natree)))
     (ftree-seq-cut-elim
      (ftree-seq-weaken-early ; this may eliminate some unnecessary cuts
       (new-ftree-seq-der
	(intern-str (format nil "~d-SEQ" (natree-name natree)))
	(natree-to-ftree-seq-normal natree))))))
   (declare (ignore pos))
   (values (car neg) ftree-clist)))

; given a normal deduction of line: Gamma |- C,
; return a sequent derivation C* of Gamma' => C
; where each wff in Gamma' is in Gamma (but may occur several times or not at all)
(defun natree-to-ftree-seq-normal (&optional (natree current-natree))
  (declare (special ftree-clist natree-debug))
  (when natree-debug
    (msgf "I am working on normal: (" (natree-number natree) ") " (natree-justification natree))
    (msgf "The assertion is " ((natree-assertion natree) . gwff)))
  (unless (eq (natree-annotation natree) 'normal)
    (throwfail "An extraction natree was expected to be normal. " natree))
  (let ((node nil))
    (if (eq (natree-justification natree) 'COERCION)
	(setq node (natree-to-ftree-seq-coercion (car (natree-support natree))))
      (let* ((support (natree-support natree))
	     (assertion (natree-assertion natree)))
	(setq node
	  (case (natree-justification natree)
	    (TRUTH			; truth introduction
	     *ftree-seq-true*)
	    (UGEN
	     (let* ((node1 (natree-to-ftree-seq-normal (first support))))
	       (make-ftree-seq-sel-neg assertion
				       (car (natree-jterm natree))
				       node1)))
	    (RULEC
	     (let* ((premiss (first support))
		    (qwff (natree-assertion premiss))
		    (spremiss (second support))
		    (snode (natree-to-ftree-seq-normal spremiss)))
	       (natree-to-ftree-seq-extraction
		premiss (make-ftree-seq-sel-pos
			 qwff (car (natree-jterm natree))
			 (ftree-seq-merge-focus-all-pos
			  (substitute-l-term-var 
			   (car (natree-jterm natree))
			   (bindvar qwff)
			   (cdr qwff))
			  snode)))))
	    (EGEN
	     (let* ((node1 (natree-to-ftree-seq-normal (first support))))
	       (make-ftree-seq-exp-neg assertion (car (natree-jterm natree)) node1)))
	    (IDISJ-R
	     (let* ((disj assertion)
		    (ldisj (cdar disj))
		    (node1 (natree-to-ftree-seq-normal (first support))))
	       (make-ftree-seq-dis-neg
		(make-ftree-seq-weaken ldisj node1 nil))))
	    (IDISJ-L
	     (let* ((disj assertion)
		    (rdisj (cdr disj))
		    (node1 (natree-to-ftree-seq-normal (first support))))
	       (make-ftree-seq-dis-neg
		(make-ftree-seq-focus
		 1 (make-ftree-seq-weaken rdisj node1 nil) nil))))
	    ((|SAME AS| AB)
	     (natree-to-ftree-seq-normal (first support)))
	    (ABSURD
	     (let* ((premiss (first support)))
	       (natree-to-ftree-seq-extraction
		premiss (make-ftree-seq-weaken assertion *ftree-seq-false* nil))))
	    (CASES ; assume there are only 2 cases - see the function expand-cases to get rid of 3 or 4 cases
	     (let* ((disj (natree-assertion (first support)))
		    (disj1 (cdar disj))
		    (disj2 (cdr disj))
		    (node1 (natree-to-ftree-seq-normal (second support)))
		    (node2 (natree-to-ftree-seq-normal (third support)))
		    (dnode (make-ftree-seq-merge-neg
			    (make-ftree-seq-dis-pos
			     (ftree-seq-merge-focus-all-pos disj1 node1)
			     (ftree-seq-merge-focus-all-pos disj2 node2)))))
	       (natree-to-ftree-seq-extraction
		(first support) dnode)))
	    (CONJ
					; ICONJ
	     (let* ((nodel (natree-to-ftree-seq-normal (first support)))
		    (noder (natree-to-ftree-seq-normal (second support))))
	       (make-ftree-seq-con-neg nodel noder)))
	    (DEDUCT
	     (let* ((premiss (first support))
		    (node1 (natree-to-ftree-seq-normal premiss)))
	       (make-ftree-seq-imp-neg
		(ftree-seq-merge-focus-all-pos (cdar assertion) node1))))
	    (INDIRECT			; treat this similar to negation introduction 
	     (let* ((premiss (first support))
		    (node1 (natree-to-ftree-seq-normal premiss))
		    (node2 (ftree-seq-remove-neg-false node1 (list 0)))
		    (not-A (cons 'NOT assertion))
		    (posl nil))
	       (do ((i 0 (+ i 1))
		    (pos-wffs (ftree-seq-pos-wffs node2) (cdr pos-wffs)))
		   ((null pos-wffs))
		   (when (wffeq-ab (car pos-wffs) not-A)
		     (push i posl)))
	       (ftree-seq-merge-focus-all-neg
		assertion
		(ftree-seq-pos-not-A-to-neg-A ; this is where classical uses multiple wffs in RHS
		 node2
		 (reverse posl)))))
	    (NEGINTRO
	     (let* ((premiss (first support))
		    (node1 (natree-to-ftree-seq-normal premiss)))
	       (make-ftree-seq-neg-neg 
		(ftree-seq-merge-focus-all-pos
		 (cdr assertion)
		 (ftree-seq-remove-neg-false node1 (list 0))))))
	    (NEGELIM
	     (let* ((premiss1 (first support)) ; ~ A (extracted)
		    (premiss2 (second support)) ; A (normal)
		    (node2 (natree-to-ftree-seq-normal premiss2))
		    (node1 (make-ftree-seq-neg-pos node2))
		    (nodeC (make-ftree-seq-weaken assertion node1 nil)))
	       (natree-to-ftree-seq-extraction premiss1 nodeC)))
	    (|ASSERT REFL=| ; handling this as Leibniz instead of directly as a rewrite
	     (let ((REWRITE-EQUALITIES 'LEIBNIZ))
	       (declare (special REWRITE-EQUALITIES))
	       (let* ((qwff (instantiate-top-equality assertion))
		      (lwff (lnorm qwff))
		      (q (bindvar lwff))
		      (qsel (fresh-var (type q) (getnameroot q))))
		 (make-ftree-seq-rew-neg
		  assertion 'LEIBNIZ=
		  (make-ftree-seq-rew-neg
		   qwff 'LAMBDA
		   (make-ftree-seq-sel-neg
		    lwff qsel
		    (make-ftree-seq-imp-neg
		     (make-ftree-seq-init (cons qsel (cdr assertion))))))))))
	    (DUAL-OR
	     (let ((fs (natree-to-ftree-seq-normal (first support)))
		   (ini (make-ftree-seq-init assertion)))
					;             init -------  ------ init
					;                   A -> A  A -> A
					;                  ---------------- or+
					;                   A or A -> A, A
					;                  ---------------- merge
					; fs::G -> A or A   A or A -> A
					; ------------------------------- cut
					;         G -> A
	       (make-ftree-seq-cut
		fs (make-ftree-seq-merge-neg
		    (make-ftree-seq-dis-pos ini ini)))))
	    ((EXT=			; one direction is extraction, the other is normal
	      |ETA RULE| |BETA RULE| LAMBDA LAMBDA= 
	      DEFN BINDER-DEFN EQUIVWFFS LEIBNIZ= EQUIV-EQ REFL=
	      IMPEQUIV)
	     (make-ftree-seq-rew-neg assertion 
				     (rename-justification (natree-justification natree))
				     (natree-to-ftree-seq-normal (first support))))
	    (TRUTHP			; see ml-etr-tactics-main.lisp for this justification.  It's handled ad hoc here just as it is there.
	     (make-ftree-seq-rew-neg assertion 'TRUTHP
				     (natree-to-ftree-seq-normal (first support))))
	    ((NEG NNF-EXPAND)
	     (ftree-seq-nnf-neg
	      assertion
	      (natree-to-ftree-seq-normal (first support))))
	    ((RULEP RULEQ)
	     (throwfail "Lines with justification " (natree-justification natree) " should have been removed."))
	    ((SUB-EQUIV REWRITE REWRITES)
	     (throwfail "Cannot handle ND proofs with " (natree-justification natree)))
	    ((SYM= |ASSERT SYM=| SUBST= SUB= ASSOC
	      IMP-DISJ-L IMP-DISJ-R IMP-DISJ
	      DISJ-IMP-L DISJ-IMP-R DISJ-IMP)
	     (throwfail "A Natree Should Not Have Justification " (natree-justification natree)
			"There must be a bug in proof-to-natree."))))
	(when natree-debug
					; double check that the seq calc derivation is well formed
	  (unless (wffeq-ab (neg-norm (natree-assertion natree))
			    (neg-norm (car (ftree-seq-neg-wffs node))))
	    (throwfail "The assertion " ((natree-assertion natree) . gwff) t
		       " does not match formula in concl of " t node))
	  (msgf "Double Checking Normal Output (" (natree-number natree) ")")
	  (check-ftree-seq node)
	  )
	(if node
	    node
	  (throwfail "Cannot translate line " (natree-number natree)))))))
	
					; where extractions meet normal (or, working backwards, normal meets extractions)    
(defun natree-to-ftree-seq-coercion (natree)
  (declare (special ftree-clist))
  (let* ((assertion (natree-assertion natree)))
    (natree-to-ftree-seq-extraction
     natree (make-ftree-seq-init assertion))))

; given an extraction derivation of line: Gamma |- A 
; and a sequent derivation C* of A, Gamma' => C
; return a sequent derivation C** of Gamma'' => C
; where each wff in Gamma'/Gamma'' is in Gamma (but may occur several times or not at all)
(defun natree-to-ftree-seq-extraction (natree fs)
  (declare (special natree-debug))
					; Comment the next two lines unless debugging
  (when natree-debug
    (msgf "I am working on extraction: (" (natree-number natree) ") " (natree-justification natree))
    (msgf "The assertion is " ((natree-assertion natree) . gwff))
    (unless (wffeq-ab (natree-assertion natree)
		      (car (ftree-seq-pos-wffs fs)))
      (throwfail "This assertion does not match first of seq " fs))
    (msgf "Double Checking Extraction Input")
    (check-ftree-seq fs)
    (unless (eq (natree-annotation natree) 'EXTRACTION)
      (throwfail "A normal natree was expected to be an extraction. " natree)))
  (let ((der
	 (if (eq (natree-justification natree) 'BACKWARDS-COERCION) ; cut!
	     (let* ((node1 (natree-to-ftree-seq-normal (car (natree-support natree)))))
	       (make-ftree-seq-cut node1 fs))
	   (let ((support (natree-support natree))
		 (assertion (natree-assertion natree)))
	     (ecase (natree-justification natree)
		    (UI
		     (let* ((premiss (first support))
			    (node (make-ftree-seq-exp-pos
				   (natree-assertion premiss)
				   (car (natree-jterm natree))
				   fs)))
		       (natree-to-ftree-seq-extraction premiss node)))
		    ((CHOOSE HYP |CASE 1| |CASE 2| |CASE 3| |CASE 4| |ASSUME NEGATION|)
		     ; no need to do anything here, since we are allowing multiple copies
		     fs)
		    ((NEG NNF)
		     (natree-to-ftree-seq-extraction 
		      (first support)
		      (ftree-seq-nnf-pos (natree-assertion (car support)) fs)))
		    ((|SAME AS| AB) ; we don't explicitly represent alpha conversions
		     (natree-to-ftree-seq-extraction (first support) fs))
		    ((EXT= |ETA RULE| |BETA RULE| LAMBDA LAMBDA= LEIBNIZ=
			   DEFN BINDER-DEFN EQUIVWFFS EQUIV-EQ EQUALITY EQUIVIMP REFL=)
		     (natree-to-ftree-seq-extraction
		      (first support)
		      (make-ftree-seq-rew-pos (natree-assertion (first support))
					      (rename-justification
					       (natree-justification natree))
					      fs)))
		    (DUAL-AND
					; ------  ------ init
					; A -> A  A -> A
					; --------------- and-
					; A, A -> A and A
					; --------------- merge+
					;    A -> A and A       fs::A and A, G -> C
					; ------------------------------- cut
					;             A, G -> C
		     (let ((ini (make-ftree-seq-init (natree-assertion (first support)))))
		       (make-ftree-seq-cut
			(make-ftree-seq-merge-pos
			 (make-ftree-seq-con-neg ini ini))
			fs)))
		    (CONJ ; ECONJ
		     (let* ((conj (natree-assertion (car support)))
			    (conj1 (cdar conj))
			    (conj2 (cdr conj)))
		       (if (wffeq assertion conj1)
			   (natree-to-ftree-seq-extraction 
			    (car support)
			    (make-ftree-seq-con-pos
			     (make-ftree-seq-focus
			      1 (make-ftree-seq-weaken conj2 fs t) t)))
			 (natree-to-ftree-seq-extraction
			  (car support)
			  (make-ftree-seq-con-pos
			   (make-ftree-seq-weaken conj1 fs t))))))
		    (MP
		     (natree-to-ftree-seq-extraction
		      (second support)		; B => A (extracted)
		      (make-ftree-seq-imp-pos
		       (natree-to-ftree-seq-normal (first support)) ; B (normal)
		       fs)))
		    ((SUB-EQUIV REWRITE REWRITES)
		     (throwfail "Cannot handle ND proofs with " (natree-justification natree)))
		    ((SYM= |ASSERT SYM=| SUBST= SUB= ASSOC
			   IMP-DISJ-L IMP-DISJ-R IMP-DISJ
			   DISJ-IMP-L DISJ-IMP-R DISJ-IMP)
		     (throwfail "A Natree Should Not Have Justification " (natree-justification natree)
				"There must be a bug in proof-to-natree.")))))))
    (when natree-debug
      (msgf "Double Checking Extraction Output (" (natree-number natree)  ")")
      (check-ftree-seq der))
    der))

; cebrown 2/21/02
(defmexpr normalize-proof
  (argtypes symbol)
  (argnames prefix)
  (arghelp "Name of the Proof")
  (defaultfns (lambda (prefix) (list (if (eq prefix '$) 
					 (or dproof '$)
				       prefix))))
  (mhelp "Normalize a natural deduction proof.  The actual procedure
uses DEASSERT-LEMMAS to combine asserted lemmas into one big natural
deduction proof.  This is the converted into a sequent calculus
derivations with cuts.  A cut elimination (which may not terminate
in principle) creates a cut-free proof which is translated back
to a normal natural deduction proof.

To suppress excessive output, try setting the following flags
NATREE-DEBUG, ETREE-NAT-VERBOSE and PRINTLINEFLAG
to NIL and TACTIC-VERBOSE to MIN."))

; much of this looks the same as the beginning of ceb-nat-etr
(defun normalize-proof (proofname)
  (reconsider proofname)
  (let* ((newpfname (let ((pfname- (intern-str (format nil "~d-" proofname))))
		      (do ((pfname1 (intern-str (create-namestring pfname-)) (intern-str (create-namestring pfname-))))
			  ((not (proof-lines pfname1)) pfname1)))))
    (eliminate-all-rulep-apps newpfname) ; this changes the dproof to the new proof name
    (deassert-lemmas proofname) ; in case some of the cuts have been separated into several
					; ND proofs connected by Assert's
    (eliminate-all-ruleq-apps)
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
	(remove-subst=-lines subst=-lines))
      (when bad-assert-lines
	(msgf "There are some lines justified by Assert" t
	      "The assertions will be replaced by hypotheses discharged at the end.")
	(dolist (a bad-assert-lines) (make-assert-a-hyp a)))
      (dolist (l (proof-lines dproof))
	      (when (string= (car (line-justification l)) "Subst")
		(let ((var (cadadr (line-justification l)))
		      (trm (caadr (line-justification l)))
		      (support (caaddr (line-justification l)))
		      (i (linealias l)))
		  (plan l)
		  (introduce-gap l 1)
		  (comdecode (list 'cl-user::ugen i (linealias support)
				   '$ (list 'quote var) '$ '$))
		  (comdecode (list 'cl-user::ui i (+ i 1)
				   (list 'quote trm) '$ '$ '$ '$ '$)))))
      (eliminate-all-rulep-apps)		; remove-subst= may introduce rulep's
      (expand-cases) ; makes any application of CASES binary (see rules CASES3, CASES4)
    ; finally, if we started with a proof with undischarged hyps, discharge them now
    (do* ((conc (car (last (proof-lines dproof))) (car (last (proof-lines dproof))))
	  (hyps (line-hypotheses conc) (line-hypotheses conc)))
	 ((null hyps))
	 (comdecode (list 'cl-user::deduct (+ (linealias conc) 1) (linealias conc)
			  (linealias (car hyps)) '$ '$ '$ '$ '$)))
    (setq current-natree (ceb-proof-to-natree dproof))
    (let ((fsname (intern-str (format nil "~d-CUTFREE-SEQ" (natree-name current-natree))))
	  (newpfname2 (intern-str (format nil "~d-NORMAL" newpfname))))
      (new-ftree-seq-der
       fsname
       (ftree-seq-raise-lambdas ; guarantees exp terms are lambda-normal
	(ftree-seq-cut-elim
	 (ftree-seq-weaken-early ; this may eliminate some unnecessary cuts
	  (new-ftree-seq-der
	   (intern-str (format nil "~d-SEQ" (natree-name current-natree)))
	   (natree-to-ftree-seq-normal current-natree))))))
      (seq-to-nat fsname newpfname2)
      (pall)))))



  