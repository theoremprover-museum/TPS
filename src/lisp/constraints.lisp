;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(context primsubs)

(deffile constraints
  (part-of PRIMITIVE-SUBST)
  (extension lisp)
  (mhelp "Functions for dealing with set constraints."))

(defstruct constraint-set
  (ftree nil)
  (clist nil)
  (lemmas nil)
  (kind nil) ; (MAX|MIN|UNIF -vars-)
  (constrs nil) ; set constraints
  (dpairs nil) ; unification constraints - not used yet
  (eqn-constrs nil) ; may use narrowing later . . .
  (weight 0))

(defflag DELAY-SETVARS
  (default NIL)
  (flagtype boolean)
  (subjects primsubs transmit)
  (relevant-kids (DELAY-SETVARS '(MAX-CONSTRAINT-SIZE
				  MAX-NUM-CONSTRAINTS WHICH-CONSTRAINTS
				  BAD-VAR-CONNECTED-PRUNE
				  INCLUDE-INDUCTION-PRINCIPLE
				  INCLUDE-COINDUCTION-PRINCIPLE)))
  (mhelp "If T, first solve the rigid part of the jform,
then try to solve the flexible parts using setvar constraints."))

(defflag BAD-VAR-CONNECTED-PRUNE
  (flagtype boolean)
  (default t)
  (subjects mating-search important primsubs transmit)
  (mhelp "When generating set constraints, prune those which do not
have bad variables (selected variables the set variable cannot depend upon)
shared between the literals in the constraints.  For example,
if p cannot depend on x or y, the constraints

   p 0 -> A x
   p x -> A 0
   p x -> A x, B y
   p y -> A x, B y

would be pruned while the constraints

   p x -> A x
   p x -> A x y, B y

would not be pruned."))

  
(defflag max-constraint-size
  (flagtype integer+-or-infinity)
  (default 3)
  (subjects mating-search important primsubs transmit)
  (relevancy-preconditions
   (default-ms (or (and (eq default-ms 'ms98-1) delay-setvars)
		   (and (eq default-ms 'ms03-7) ms03-use-set-constraints)
		   (and (eq default-ms 'ms04-2) ms04-use-set-constraints)))
   (delay-setvars (and (eq default-ms 'ms98-1) delay-setvars))
   (ms03-use-set-constraints (and (eq default-ms 'ms03-7) ms03-use-set-constraints))
   (ms04-use-set-constraints (and (eq default-ms 'ms04-2) ms04-use-set-constraints)))
  (irrelevancy-preconditions
   (default-ms (not (member default-ms '(ms98-1 ms03-7 ms04-2))))
   (delay-setvars (and (eq default-ms 'ms98-1) (not delay-setvars)))
   (ms03-use-set-constraints (and (eq default-ms 'ms03-7) (not ms03-use-set-constraints)))
   (ms04-use-set-constraints (and (eq default-ms 'ms04-2) (not ms04-use-set-constraints))))
  (mhelp "Maximum number of literals allowed in a single constraint"))

(defflag max-num-constraints
  (flagtype integer+-or-infinity)
  (default 2)
  (subjects mating-search important primsubs transmit)
  (mhelp "Maximum number of combined constraints in each constraint set."))

(defflag which-constraints
  (flagtype symbollist)
  (default '(MAX MIN))
  (subjects mating-search important primsubs transmit)
  (relevancy-preconditions
   (default-ms (or (and (eq default-ms 'ms98-1) delay-setvars)
		   (and (eq default-ms 'ms03-7) ms03-use-set-constraints)
		   (and (eq default-ms 'ms04-2) ms04-use-set-constraints)))
   (delay-setvars (and (eq default-ms 'ms98-1) delay-setvars))
   (ms03-use-set-constraints (and (eq default-ms 'ms03-7) ms03-use-set-constraints))
   (ms04-use-set-constraints (and (eq default-ms 'ms04-2) ms04-use-set-constraints)))
  (irrelevancy-preconditions
   (default-ms (not (member default-ms '(ms98-1 ms03-7 ms04-2))))
   (delay-setvars (and (eq default-ms 'ms98-1) (not delay-setvars)))
   (ms03-use-set-constraints (and (eq default-ms 'ms03-7) (not ms03-use-set-constraints)))
   (ms04-use-set-constraints (and (eq default-ms 'ms04-2) (not ms04-use-set-constraints))))
  (mhelp "Which kinds of set constraints should be generated and solved.

. MAX:  Constraints for p of the form Psi | p t ==> Gamma(p)
        solved using maximal solution.
. MIN:  Constraints for p of the form Psi | Gamma(p) ==> p t
        solved using minimal solution.
. PR00: Generates instantiated ftrees and connections by mating
        nonleaves.
"))

(defflag include-induction-principle
  (flagtype boolean)
  (default t)
  (subjects mating-search primsubs important transmit)
  (mhelp "When solving closure set-variable constraints we
include in the lemma a higher-order statement that we have
the least solution.

For example, suppose we want a set N such that

N 0
and
forall n [N n implies [N [S n]]]

If include-induction-principle is set to T, then the lemma
will include a conjunct of the form

forall p . p 0 and [forall n [p n implies [p [S n]]]]
       implies forall x [N x implies p x]."))


(defflag include-coinduction-principle
  (flagtype boolean)
  (default t)
  (subjects mating-search primsubs important transmit)
  (mhelp "When solving co-closure set-variable constraints we
include in the lemma a higher-order statement that we have
the greatest solution.

For example, suppose we want a set N such that

~X 0
and
forall z [X [f z] implies [X z]]

If include-coinduction-principle is set to T, then the lemma
will include a conjunct of the form

forall p . ~[p 0] and [forall z [p [f z] implies [p z]]]
       implies forall x [p x implies N x]."))

(defun constraint-next-openpath (jform constrs)
  (multiple-value-bind
   (path closed)
   (constraint-find-cheapest-mspath jform nil constrs)
   (do ()
       ((not (and closed path))
	path)
       (multiple-value-setq
	(path closed)
	(constraint-find-alt-mspath (car path) (cdr path) constrs)))))

(defun constraint-complete-mspath (last-elt path constrs)
  (let ((parent (jform-parent last-elt)))
    (if parent
        (if (eq (jform-type parent) 'conjunction)
            (constraint-complete-alt-mspath-and parent last-elt path constrs)
            (constraint-complete-mspath parent path constrs))
        path)))

(defun constraint-find-cheapest-mspath (jform path constrs)
  (case (jform-type jform)
    (literal
      (if (constraint-open-mspath-p jform path constrs)
	  (cons jform path)
	(values (cons jform path) t)))
    (disjunction
      (if (disjunction-components jform)
	  (constraint-find-cheapest-mspath
	    (car (disjunction-components jform)) path constrs)
	  (values (cons jform path) t)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (multiple-value-bind (newpath closed)
		(constraint-find-cheapest-mspath conj path constrs)
	      (if closed (return (values newpath closed))
		  (setq path newpath))))
	  (cons 'stop path)))
    (universal 
     (constraint-find-cheapest-mspath
      (universal-scope jform) path constrs))))

(defun constraint-find-alt-mspath (last-elt rem-path constrs)
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (multiple-value-bind (alt-path closed)
	      (let ((disj (cadr (memq last-elt (disjunction-components
                                                parent)))))
		(when disj (constraint-find-cheapest-mspath
			     disj rem-path constrs)))
	    (if closed (values alt-path closed)
		(if alt-path (constraint-complete-mspath
			       parent alt-path constrs)
		    (constraint-find-alt-mspath
		      parent rem-path constrs)))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq last-elt 'stop))
	      (constraint-find-alt-mspath parent rem-path constrs)
	      (constraint-find-alt-mspath
		(car rem-path) (cdr rem-path) constrs)))
        (universal (constraint-find-alt-mspath parent rem-path constrs))))))

(defun constraint-complete-alt-mspath-and (parent jform path constrs)
  (dolist (conj (cdr (memq jform (conjunction-components parent)))
                (constraint-complete-mspath parent path constrs))
    (multiple-value-bind (newpath closed)
        (constraint-find-cheapest-mspath conj path constrs)
      (if closed (return (values newpath closed))
          (setq path newpath)))))

(defun constraint-open-mspath-p (literal path constrs)
  "Returns NIL if adding LITERAL to PATH closes the PATH, else returns T."
  (let ((name (literal-name literal))
	(newpath (mapcar #'literal-name path)))
    (dolist (c constrs t)
	    (let ((m (member name c :test #'(lambda (x y) (eq x (literal-name y))))))
	      (when (and m (subsetp (remove (car m) c) newpath))
		(return nil))))))

(defun generate-constraint-sets-for-vars (f clist lemmas
					    vl allb j &key (throw-on-complete-set t))
  (declare (special options-verbose))
  (when options-verbose
    (msgf "Finding constraints for " vl " in jform:" t)
    (display-vp-diag j))
  (let ((vposlits (find-literals-in-jform
		   #'(lambda (lit)
		       (let ((h (head (literal-represents lit))))
			 (and (jform-pos lit)
			      (member h vl)
			      (not (find-if #'(lambda (w)
						(free-in h w))
					    (args (literal-represents lit)))))))
		   j))
	(vneglits (find-literals-in-jform
		   #'(lambda (lit)
		       (let ((h (head (literal-represents lit))))
			 (and (not (jform-pos lit))
			      (member h vl)
			      (not (find-if #'(lambda (w)
						(free-in h w))
					    (args (literal-represents lit)))))))
		   j))
	(csets nil))
    (when options-verbose
      (msgf "Pos Flex Lits: " vposlits t)
      (msgf "Neg Flex Lits: " vneglits t))
    (when (member 'PR00 WHICH-CONSTRAINTS)
      (setq *pr00-setsubs* nil)
      (let ((ALLOW-NONLEAF-CONNS '(ALL))
	    (DISSOLVE nil)
	    (MAX-SUBSTS-VAR PR00-MAX-SUBSTS-VAR)
	    (exceptions nil))
	(declare (special ALLOW-NONLEAF-CONNS DISSOLVE MAX-SUBSTS-VAR exceptions))
	(generate-pr00-setsubs-rec f clist 1)
	(dolist (fc *pr00-setsubs*)
		(push (make-constraint-set :kind '(SOLVED)
					   :ftree (car fc) :clist (cdr fc)
					   :lemmas lemmas)
		      csets))))
    (when (member 'MAX WHICH-CONSTRAINTS)
      (dolist (vposlit vposlits)
	      (when options-verbose
		(msgf "Generating Constraints with mainlit " vposlit))
	      (let* ((v (head (literal-represents vposlit)))
		     (new-max-constrs (generate-max-constraints-for-vlit vposlit))
		     (new-max-csets nil)
		     (banned (cdr (assoc v allb)))
		     (bannedoccurs
		      (intersection 
		       (free-vars-of (strip-exp-vars (literal-represents vposlit)))
		       banned)))
		(if (distinct-vars-from (args (strip-exp-vars (literal-represents vposlit)))
					bannedoccurs)
		    (let ((cs (make-constraint-set :ftree f :clist clist :lemmas lemmas
						   :kind (list 'EMPTY v)
						   :constrs (list (list vposlit)))))
		      (when (and throw-on-complete-set
				 (not (constraint-next-openpath j (list (list vposlit)))))
			(throw 'complete-constraints cs))
		      (push cs csets))
		  (push nil new-max-constrs))
		(when BAD-VAR-CONNECTED-PRUNE
		  (setq new-max-constrs
			(remove-if-not #'(lambda (constr)
					   (lits-sel-conn-p constr bannedoccurs banned))
				       new-max-constrs)))
		(setq new-max-csets
		      (mapcar #'(lambda (constr)
				  (make-constraint-set
				   :ftree f :clist clist :lemmas lemmas
				   :kind (list 'MAX v)
				   :constrs (list (cons vposlit constr))))
			      new-max-constrs))
		(dolist (new-cset new-max-csets)
			(when throw-on-complete-set
			  (unless (constraint-next-openpath
				   j (constraint-set-constrs new-cset))
			    (throw 'complete-constraints new-cset)))
			(let ((combined (combine-constraint-sets (list new-cset) csets)))
			  (when throw-on-complete-set
			    (dolist (cset combined)
				    (unless (constraint-next-openpath j (constraint-set-constrs cset))
				      (throw 'complete-constraints cset))))
			  (setq csets (cons new-cset (append combined csets))))))))
    (when (member 'MIN WHICH-CONSTRAINTS)
      (dolist (vneglit vneglits)
	      (when options-verbose
		(msgf "Generating Constraints with mainlit " vneglit))
	      (let* ((v (head (literal-represents vneglit)))
		     (new-min-constrs (generate-min-constraints-for-vlit vneglit))
		     (new-min-csets nil)
		     (banned (cdr (assoc v allb)))
		     (bannedoccurs
		      (intersection 
		       (free-vars-of (strip-exp-vars (literal-represents vneglit)))
		       banned)))
		(if (distinct-vars-from (args (strip-exp-vars (literal-represents vneglit)))
					bannedoccurs)
		    (let ((cs (make-constraint-set :ftree f :clist clist :lemmas lemmas
						   :kind (list 'FULL v)
						   :constrs (list (list vneglit)))))
		      (when (and throw-on-complete-set
				 (not (constraint-next-openpath j (list (list vneglit)))))
			(throw 'complete-constraints cs))
		      (push cs csets))
		  (push nil new-min-constrs))
		(when BAD-VAR-CONNECTED-PRUNE
		  (setq new-min-constrs
			(remove-if-not #'(lambda (constr)
					   (lits-sel-conn-p constr bannedoccurs banned))
				       new-min-constrs)))
		(setq new-min-csets
		      (mapcar #'(lambda (constr)
				  (make-constraint-set
				   :ftree f :clist clist :lemmas lemmas
				   :kind (list 'MIN v)
				   :constrs (list (cons vneglit constr))))
			      new-min-constrs))
		(dolist (new-cset new-min-csets)
			(when throw-on-complete-set
			  (unless (constraint-next-openpath j (constraint-set-constrs new-cset))
			    (throw 'complete-constraints new-cset)))
			(let ((combined (combine-constraint-sets (list new-cset) csets)))
			  (when throw-on-complete-set
			    (dolist (cset combined)
				    (unless (constraint-next-openpath j (constraint-set-constrs cset))
				      (throw 'complete-constraints cset))))
			  (setq csets (cons new-cset (append combined csets))))))))
    (setq csets (remove-if #'prune-constraint-set-p csets))
    (when options-verbose
      (when csets
	(msgf "Generated Constraints:" t))
      (dolist (cset csets)
	      (msgf "PATHS: " (constraint-set-constrs cset) t)))
    csets))

(defun distinct-vars-from (args vars)
  (if args
      (if (member (car args) vars)
	  (distinct-vars-from (cdr args) (remove (car args) vars))
	nil)
    t))

; make sure there's no proper subset lits' of lits where
; (bannedvars(lits')) is disjoint from (bannedvars(lits - lits') U seloccurs)
(defun lits-sel-conn-p (lits bannedoccurs banned)
  (lits-sel-conn-p-1 (mapcar #'(lambda (lit)
				 (intersection
				  (free-vars-of (strip-exp-vars (literal-represents lit)))
				  banned)) lits)
		     bannedoccurs))

(defun lits-sel-conn-p-1 (bvl bannedoccurs)
  (if bvl
      (let ((x (find-if #'(lambda (y) (intersection bannedoccurs y)) bvl)))
	(if x
	    (lits-sel-conn-p-1 (remove x bvl) (union bannedoccurs x))
	  nil))
    t))

(defun generate-min-constraints-for-vlit (lit)
  (let ((constrs (list nil))
	(v (head (literal-represents lit))))
    (do ((j (jform-parent lit) (jform-parent j))
	 (lastj lit j))
	((null j))
	(when (conjunction-p j)
	  (let ((subpaths (generate-subpaths-of-conjuncts
			   #'(lambda (lit)
			       (or (jform-pos lit)
				   (not (eq (head (literal-represents lit)) v))))
			   (remove lastj (conjunction-components j)))))
	    (dolist (sp1 constrs)
		    (let ((n (length sp1)))
		      (dolist (sp2 subpaths)
			      (unless (and (integerp max-constraint-size)
					   (> (+ n (length sp2)) max-constraint-size))
				(push (append sp1 sp2) constrs))))))))
    (remove nil constrs)))

(defun generate-max-constraints-for-vlit (lit)
  (let ((constrs (list nil))
	(v (head (literal-represents lit))))
    (do ((j (jform-parent lit) (jform-parent j))
	 (lastj lit j))
	((null j))
	(when (conjunction-p j)
	  (let ((subpaths (generate-subpaths-of-conjuncts
			   #'(lambda (lit)
			       (or (not (jform-pos lit))
				   (not (eq (head (literal-represents lit)) v))))
			   (remove lastj (conjunction-components j)))))
	    (dolist (sp1 constrs)
		    (let ((n (length sp1)))
		      (dolist (sp2 subpaths)
			      (unless (and (integerp max-constraint-size)
					   (> (+ n (length sp2)) max-constraint-size))
				(push (append sp1 sp2) constrs))))))))
    (remove nil constrs)))

(defun generate-subpaths-of-conjuncts (legallit jl)
  (let ((subpaths nil))
    (dolist (j jl)
	    (let* ((subpaths2 (generate-subpaths legallit j))
		   (subpaths3 (append subpaths subpaths2)))
	      (dolist (subpath1 subpaths)
		      (let ((n (length subpath1)))
			(dolist (subpath2 subpaths2)
				(unless (and (integerp max-constraint-size)
					     (> (+ n (length subpath2)) max-constraint-size))
				  (push (append subpath1 subpath2) subpaths3)))))
	      (setq subpaths subpaths3)))
    subpaths))

(defun generate-subpaths (legallit j)
  (typecase j
	    (conjunction (generate-subpaths-of-conjuncts
			  legallit (conjunction-components j)))
	    (disjunction 
	     (apply #'append
		    (mapcar #'(lambda (x)
				(generate-subpaths legallit x))
			    (disjunction-components j))))
	    (universal (generate-subpaths legallit (universal-scope j)))
	    (existential (generate-subpaths legallit (existential-scope j)))
	    (literal
	     (if (funcall legallit j)
		 (list (list j))
	       nil))))

(defun combine-constraint-sets (csets1 csets2)
  (let ((csets nil))
    (dolist (cset1 csets1)
	    (dolist (cset2 csets2)
		    (let ((cset (combine-constraints cset1 cset2)))
		      (when cset
			(push cset csets)))))
    csets))

(defun combine-constraints (cset1 cset2)
  (when (compatible-constraints cset1 cset2)
    (let ((constrs nil)
	  (contr1 nil)
	  (contr2 nil))
      (dolist (constr (constraint-set-constrs cset1))
	      (unless (member constr (constraint-set-constrs cset2)
			      :test #'(lambda (x y)
					(subsetp y x)))
		(push constr constrs)
		(setq contr1 t)))
      (dolist (constr (constraint-set-constrs cset2))
	      (unless (member constr (constraint-set-constrs cset1)
			      :test #'(lambda (x y)
					(subsetp y x)))
		(push constr constrs)
		(setq contr2 t)))
      (when (and contr1 contr2 ; both contribute constraints
		 (not (and (integerp max-num-constraints)
			   (> (length constrs) max-num-constraints))))
	(let ((newcs (copy-constraint-set cset1)))
	  (setf (constraint-set-kind newcs)
		(cons (car (constraint-set-kind cset1))
		      (union (cdr (constraint-set-kind cset1))
			     (cdr (constraint-set-kind cset2)))))
	  (setf (constraint-set-constrs newcs) constrs)
	  newcs)))))

(defun compatible-constraints (cset1 cset2)
  (and (eq (car (constraint-set-kind cset1)) (car (constraint-set-kind cset2)))
       (member (car (constraint-set-kind cset1)) '(MAX MIN))))

; examples of constraint sets we prune are those
; which are supposed to be max constraints for v,
; but the full set works - or min constraints for v
; where the empty set works.
(defun prune-constraint-set-p (cs)
  (let ((k (constraint-set-kind cs)))
    (or (and (eq (car k) 'MAX)
	     (prune-max-constr-p (cdr k) (constraint-set-constrs cs)))
	(and (eq (car k) 'MIN)
	     (prune-min-constr-p (cdr k) (constraint-set-constrs cs))))))

(defun prune-max-constr-p (vl constrs)
  (dolist (path constrs t)
	  (let ((v (head (literal-represents (car path)))))
	    (unless (find-if #'(lambda (lit)
				 (and (not (jform-pos lit))
				      (eq (head (literal-represents lit)) v)))
			     (cdr path))
	      (setq vl (remove v vl))
	      (unless vl
		(return-from prune-max-constr-p nil))))))
	  
(defun prune-min-constr-p (vl constrs)
  (dolist (path constrs t)
	  (let ((v (head (literal-represents (car path)))))
	    (unless (find-if #'(lambda (lit)
				 (and (jform-pos lit)
				      (eq (head (literal-represents lit)) v)))
			     (cdr path))
	      (setq vl (remove v vl))
	      (unless vl
		(return-from prune-min-constr-p nil))))))

(defun ftree-solve-constraint-set (cs)
  (let ((new-csets (process-constraint-set cs)))
    (do ((cs2 new-csets (cdr cs2)))
	((or (null cs2)
	     (eq (car (constraint-set-kind (car cs2))) 'SOLVED))
	 (if cs2
	     (values (constraint-set-ftree (car cs2))
		     (constraint-set-clist (car cs2))
		     (constraint-set-lemmas (car cs2)))
	   (throw 'cant-solve nil))))))

; performs operations on cs and returns a list of new cs's -
; some of these may be solved - eg, MAX/MIN constraints may return
; a list with one solved constraint
; (unless some abstracting of bad variables is included)
(defun process-constraint-set (cs)
  (declare (special options-verbose))
  (let ((k (constraint-set-kind cs))
	(f (constraint-set-ftree cs))
	(clist (constraint-set-clist cs))
	(lemmas (constraint-set-lemmas cs))
	(new-csets nil))
    (when query-user
      (print-constraint-set cs)
      (unless (query (format nil "Process ~d constraints?" k) t)
	(return-from process-constraint-set nil)))
    (case (car k)
	  ((EMPTY FULL) ; in these cases we just instantiate with KTRUE or KFALSE
	   (multiple-value-bind
	    (f2 clist2)
	    (make-ftree-setvar-inst (cadr k) f clist (car k))
	    (let ((new-cset (make-constraint-set
			     :ftree f2 :clist clist2 :lemmas lemmas
			     :kind '(SOLVED))))
	      (push new-cset new-csets))))
	  ((MAX MIN)
	   (let* ((constrs (constraint-set-constrs cs)))
	     (if (> (length (cdr k)) 1) ; more than 1 setvar - mutual rec defn
		 (msgf "Sorry, can't solve set constraints with several vars yet")
	       (let* ((v (cadr k))
		      (vsel (fresh-var (unabbreviated-type v) (getnameroot v)))
		      (f3 (substitute-in-ftree vsel v f))
		      (fassoc (ftree-assoc f f3))
		      (clist3 nil)
		      (lemmas3 lemmas)
		      (rec-flag nil)
		      (paths nil)
		      (substitutable-vars (substitutable-vars-of-ftree-rec f))
		      (misc-vars (append (selected-vars-of-ftree f) substitutable-vars))
		      (misc-occurs nil)
		      (banned-occurs nil)
		      (banned (cdr (assoc v (ftree-all-banned f)))))
		 (when options-verbose
		   (msgf "Solving " k " Constraint for: " v t))
		 (dolist (constr constrs)
			 (let ((fconstr (mapcar #'(lambda (lit)
						    (cdr (assoc (literal-name lit) fassoc
								:test #'(lambda (x y)
									  (eq x (ftree-name y))))))
						constr))
			       (path nil)
			       (banned-occs nil))
			   (when (eq (head (literal-represents (car constr))) v)
			     (let ((v3 (head (ftree-shallow (car fconstr)))))
			       (dolist (lit fconstr)
				       (let* ((free-vars (free-vars-of (ftree-shallow lit))))
					 (push lit path)
					 (unless (eq lit (car fconstr))
					   (when (member v3 free-vars)
					     (setq rec-flag t)))
					 (dolist (x free-vars)
						 (if (member x banned)
						     (setq banned-occs (adjoin x banned-occs))
						   (if (member x misc-vars)
						       (setq misc-occurs (adjoin x misc-occurs))))))))
			     (when options-verbose
			       (print-set-constraint constr))
			     (push banned-occs banned-occurs)
			     (push (reverse path) paths))))
		 (setq clist3 (ftree-mating-image clist f f3))
					; negf is the proof of the lemma,
					; posf is the version of the lemma we can use
		 (multiple-value-bind
		  (negf posf clist1)
		  (make-ftree-setvar-soln vsel (car k) paths banned-occurs misc-occurs substitutable-vars rec-flag)
		  (when options-verbose
		    (msgf "Lemma: " ((ftree-shallow posf) . gwff) t))
		  (setq clist3 (append clist1 clist3))
		  (setq f3 (if lemmas3
			       (make-ftree-con
				(make-ftree-con
				 negf (car (ftree-components f3)))
				(make-ftree-imp
				 (make-ftree-con
				  posf (car (ftree-components
					     (cadr (ftree-components f3)))))
				 (cadr (ftree-components
					(cadr (ftree-components f3))))))
			     (make-ftree-con negf (make-ftree-imp posf f3))))
		  (if rec-flag
		      (push (list (intern (create-namestring (car k)))
				  (list (intern (create-namestring 'KNASTER-TARSKI-FPTHM))))
			    lemmas3)
		    (push (list (intern (create-namestring (car k)))) lemmas3))
		  (let ((new-cset (make-constraint-set
				   :ftree f3 :clist clist3 :lemmas lemmas3
				   :kind '(SOLVED))))
		    (push new-cset new-csets)))))))
	  (t
	   (msgf "Cannot handle " (car k) " constraints.")))
    new-csets))
		
; creates a lemma
(defun make-ftree-setvar-soln (vsel kind paths banned-occurs misc-occurs substitutable-vars rec-flag)
  (let ((clist nil))
    (declare (special clist))
    (case kind
	  (MIN
	   (let* ((inv-princ (make-min-inv-princ vsel paths banned-occurs))
		  (posf (make-min-setvar-lemma-posf
			 inv-princ vsel paths banned-occurs misc-occurs substitutable-vars))
		  (negf 
		   (if rec-flag
		       (make-clos-setvar-lemma-negf
			(ftree-shallow posf) vsel paths banned-occurs misc-occurs)
		     (make-min-setvar-lemma-negf
		      (ftree-shallow posf) vsel misc-occurs inv-princ
		      paths banned-occurs))))
	     (values negf posf clist)))
	  (MAX
	   (let* ((inv-princ (make-max-inv-princ vsel paths banned-occurs))
		  (posf (make-max-setvar-lemma-posf
			 inv-princ vsel paths banned-occurs misc-occurs substitutable-vars))
		  (negf 
		   (if rec-flag
		       (make-coclos-setvar-lemma-negf
			(ftree-shallow posf) vsel paths banned-occurs misc-occurs)
		     (make-max-setvar-lemma-negf
		      (ftree-shallow posf) vsel misc-occurs inv-princ
		      paths banned-occurs))))
	     (values negf posf clist)))
	  (t (throwfail "Cannot solve " kind " constraints")))))

; instantiates
(defun make-ftree-setvar-inst (v f clist kind)
  (case kind
	(EMPTY
	 (do ((y (unabbreviated-type v) (car y))
	      (zl nil (cons (fresh-var-1 (cdr y)) zl)))
	     ((atom y)
	      (values (substitute-in-ftree (bind-var-wff-n 'lambda zl 'FALSEHOOD) v f)
		      clist))))
	(FULL
	 (do ((y (unabbreviated-type v) (car y))
	      (zl nil (cons (fresh-var-1 (cdr y)) zl)))
	     ((atom y)
	      (values (substitute-in-ftree (bind-var-wff-n 'lambda zl 'TRUTH) v f)
		      clist))))
	(t (throwfail "Cannot instantiate " v " using " kind " constraints"))))

(defun make-min-inv-princ (vsel paths banned-occurs)
  (let ((zl nil)
	(vzl vsel))
    (do ((y (unabbreviated-type vsel) (car y)))
	((not (consp y)))
	(push (fresh-var-1 (cdr y)) zl)
	(setq vzl (cons vzl (car zl))))
    (bind-var-wff-n 'forall zl
		    (acons 'IMPLIES vzl
			   (make-min-inv-princ-2 vsel zl vzl paths banned-occurs)))))

(defun make-min-inv-princ-2 (vsel zl vzl paths banned-occurs)
  (if paths
      (let ((wff2 (make-min-inv-princ-2 vsel zl vzl (cdr paths) (cdr banned-occurs)))
	    (wff1 (make-min-inv-princ-3 vsel zl vzl (ftree-shallow (caar paths))
					(cdar paths) (car banned-occurs))))
	(if wff2
	    (acons 'OR wff1 wff2)
	  wff1))
    nil))

(defun make-min-inv-princ-3 (vsel zl vzl main path banned-occs &optional aassoc eqns)
  (if (and (consp main) (consp vzl))
      (let ((m (member (cdr main) banned-occs)))
	(if m
	    (make-min-inv-princ-3 vsel zl (car vzl) (car main) path
				     (remove (car m) banned-occs)
				     (acons (cdr main) (cdr vzl) aassoc) eqns)
	  (make-min-inv-princ-3 vsel zl (car vzl) (car main) path banned-occs aassoc
				   (cons (mbed=left (cdr vzl) (cdr main)) eqns))))
    (make-min-inv-princ-4 vsel zl eqns path banned-occs aassoc)))

(defun make-min-inv-princ-4 (vsel zl eqns path banned-occs aassoc)
  (if banned-occs
      (let* ((w (car banned-occs))
	     (w2 (fresh-var (unabbreviated-type w) (getnameroot w))))
	(acons w2 'exists
	       (make-min-inv-princ-4 vsel zl eqns path (cdr banned-occs)
					(acons w w2 aassoc))))
    (make-min-inv-princ-5 vsel zl eqns path aassoc)))

(defun make-min-inv-princ-5 (vsel zl eqns path aassoc)
  (if eqns
      (let ((wff (make-min-inv-princ-5 vsel zl (cdr eqns) path aassoc)))
	(if wff
	    (acons 'AND (simul-substitute-l-term-var aassoc (car eqns)) wff)
	  (simul-substitute-l-term-var aassoc (car eqns))))
    (make-min-inv-princ-6 vsel zl path aassoc)))

(defun make-min-inv-princ-6 (vsel zl path aassoc)
  (if path
      (let* ((wff (make-min-inv-princ-6 vsel zl (cdr path) aassoc))
	     (wff2 (simul-substitute-l-term-var aassoc (ftree-shallow (car path))))
	     (wff3 (if (ftree-positive (car path)) wff2 (cons 'NOT wff2)))
	     (wff4 (if (find-if #'(lambda (arg)
				    (free-in vsel arg))
				(args wff2))
		       (let ((w (fresh-var (unabbreviated-type vsel) '\w)))
			 (acons w 'exists
				(acons 'AND
				       (bind-var-wff-n 'forall zl
						       (acons 'IMPLIES
							      (apply-wff-n w zl)
							      (apply-wff-n vsel zl)))
				       (substitute-l-term-var w vsel wff3))))
		     wff3)))
	(if wff
	    (acons 'AND wff4 wff)
	  wff4))
    nil))

(defun make-clos-setvar-lemma-negf (lemma vsel paths banned-occurs misc-occurs)
  (declare (special clist))
  (multiple-value-bind
   (ktnegf ktclist)
   (if include-induction-principle
       (make-knaster-tarski-leastfp-lemma (unabbreviated-type vsel))
     (make-knaster-tarski-lemma (unabbreviated-type vsel)))
   (setq clist (append ktclist clist))
   (let (expf)
     (declare (special expf))
     (let ((negf (make-clos-setvar-lemma-negf-0
		  lemma (ftree-shallow ktnegf) vsel paths
		  banned-occurs misc-occurs)))
       (make-ftree-con ktnegf (make-ftree-imp expf negf))))))

(defun make-clos-setvar-lemma-negf-0 (lemma ktlfp vsel paths banned-occurs misc-occurs)
  (declare (special expf))
  (if misc-occurs
      (let* ((a (bindvar lemma))
	     (b (fresh-var (unabbreviated-type a) (getnameroot a))))
	(make-ftree-sel
	 lemma b
	 (make-clos-setvar-lemma-negf-0 (substitute-l-term-var b a (cdr lemma))
					ktlfp vsel paths banned-occurs (cdr misc-occurs))))
    (let* ((inv-princ (if include-induction-principle
			  (substitute-l-term-var vsel (bindvar lemma) (cdaddr lemma))
			(substitute-l-term-var vsel (bindvar lemma) (cddr lemma))))
	   (monfn (acons vsel 'lambda (make-mon-setfn-from-rside inv-princ)))
	   (wff2 (substitute-l-term-var monfn (bindvar ktlfp) (cdr ktlfp)))
	   (wff3 (lnorm wff2))
	   (monnegf (mon-fn-negf (cdar wff3)))
	   (eu (cdr wff3))
	   (usel (fresh-var (unabbreviated-type (bindvar eu)) (getnameroot (bindvar eu))))
	   (wff4 (substitute-l-term-var usel (bindvar eu) (cdr eu)))
	   (uKu (if include-induction-principle
		    (cddar wff4)
		  (cdr wff4)))
	   (Kuu (if include-induction-principle
		    (cdadar wff4)
		  (cdar wff4)))
	   (lfwff (when include-induction-principle (cdr wff4)))
	   (lff nil)
	   (uKuf (make-ftree-leaf uKu t))
	   (Kuuf (make-ftree-leaf Kuu t))
	   (lemma2 (substitute-l-term-var usel (bindvar lemma) (cdr lemma)))
	   (lemma3 (if include-induction-principle
		       (cdadr lemma2)
		     (cdr lemma2)))
	   (l (make-ftree-leaf lemma3 nil)))
      (declare (special Kuuf lff))
      (unless (wffeq-ab lemma3 uKu)
	(msgf "lemma3 " (lemma3 . gwff) t
	      "uKu " (uKu . gwff) t "do not match")
	(break))
      (push-onto-clist l uKuf)
      (let* ((f (make-clos-setvar-lemma-negf-1 usel Kuu (cdar lemma2) paths))
	     (indf (when include-induction-principle
		     (make-clos-setvar-ind-negf lfwff (cddr lemma2) paths banned-occurs)))
	     (impf (make-ftree-imp monnegf (make-ftree-sel eu usel
							   (if include-induction-principle
							       (make-ftree-con
								(make-ftree-con Kuuf uKuf)
								lff)
							     (make-ftree-con Kuuf uKuf))))))
	(setq expf (make-ftree-exp ktlfp
				   (list monfn)
				   (list (if (wffeq wff2 wff3)
					     impf
					   (make-ftree-rew wff2 'LAMBDA impf)))))
	(make-ftree-exp lemma (list usel)
			(if include-induction-principle
			    (list (make-ftree-con f (make-ftree-con l indf)))
			  (list (make-ftree-con f l))))))))

; returns f-(wff)
(defun make-clos-setvar-lemma-negf-1 (usel Kuu wff paths &optional (n 0))
  (if (and-p wff)
      (make-ftree-con
       (make-clos-setvar-lemma-negf-2 usel Kuu (cdar wff) nil paths n)
       (make-clos-setvar-lemma-negf-1 usel Kuu (cdr wff) paths (1+ n)))
    (make-clos-setvar-lemma-negf-2 usel Kuu wff nil paths n)))

; returns f-(wff)
(defun make-clos-setvar-lemma-negf-2 (usel Kuu wff bl paths n)
  (declare (special Kuuf))
  (if (a-bd-wff-p wff)
      (let* ((z (bindvar wff))
	     (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	(make-ftree-sel wff z2
			(make-clos-setvar-lemma-negf-2
			 usel Kuu
			 (substitute-l-term-var z2 z (cdr wff))
			 (cons z2 bl) paths n)))
    (if (implies-p wff)
	(let ((args2 (args (cdr wff))))
	  (multiple-value-bind
	   (f1 f2 f3)
	   (make-clos-setvar-lemma-negf-3 usel Kuu args2 (cdar wff) bl
					  n paths args2)
	   (setq Kuuf (constr-merge-ftrees f1 Kuuf))
	   (make-ftree-imp f2 (make-clos-setvar-lemma-negf-4 f3 (cdr wff)))))
      (let ((args2 (args wff)))
	(multiple-value-bind
	 (f1 f2 f3)
	 (make-clos-setvar-lemma-negf-3 usel Kuu args2 nil bl
					n paths args2)
	 (declare (ignore f2))
	 (setq Kuuf (constr-merge-ftrees f1 Kuuf))
	 (make-clos-setvar-lemma-negf-4 f3 wff))))))

; returns f+(Kuu) f+(hwff) f+(conc(Kuu))
(defun make-clos-setvar-lemma-negf-3 (usel Kuu args hwff bl n paths args2)
  (if args
      (multiple-value-bind
       (f1 f2 f3)
       (make-clos-setvar-lemma-negf-3
	usel (substitute-l-term-var (car args) (bindvar Kuu) (cdr Kuu))
	(cdr args) hwff bl n paths args2)
       (values (make-ftree-exp Kuu (list (car args)) (list f1)) f2 f3))
    (let ((f3 (make-ftree-leaf (cdr Kuu) t)))
      (multiple-value-bind
       (f1 f2)
       (make-clos-setvar-lemma-negf-5 usel (cdar Kuu) hwff bl n paths args2)
       (values (make-ftree-imp f1 f3) f2 f3)))))

; returns f-(cwff)
(defun make-clos-setvar-lemma-negf-4 (f3 cwff)
  (let ((cf (make-ftree-leaf cwff nil)))
    (push-onto-clist f3 cf)
    cf))

; returns f-(wff) f+(hwff)
(defun make-clos-setvar-lemma-negf-5 (usel wff hwff bl n paths args2)
  (if (> n 0)
      (progn
	(unless paths
	  (break) (throwfail "Problem creating closure setvar constraint proof"))
	(unless (or-p wff)
	  (break) (throwfail "Expected " (wff . gwff) " to be a disjunction"))
	(multiple-value-bind
	 (f1 f2)
	 (make-clos-setvar-lemma-negf-5
	  usel (cdr wff) hwff bl (- n 1) (cdr paths) args2)
	 (values (make-ftree-dis (make-ftree-leaf (cdar wff) nil) f1) f2)))
    (if (cdr paths)
	(progn
	  (unless (or-p wff)
	    (break) (throwfail "Expected " (wff . gwff) " to be a disjunction"))
	  (multiple-value-bind
	   (f1 f2)
	   (make-clos-setvar-lemma-negf-6 usel (cdar wff) hwff (cdar paths) 
					  (reverse bl) args2 bl)
	   (values (make-ftree-dis f1 (make-ftree-leaf (cdr wff) nil)) f2)))
      (make-clos-setvar-lemma-negf-6 usel wff hwff (cdar paths) (reverse bl) args2 bl))))

 ; make trees corresponding to wff and hwff, with mates
 ; wff becomes a negative ftree, hwff becomes a positive ftree
 ; returns f-(wff) f+(hwff)|NIL
(defun make-clos-setvar-lemma-negf-6 (usel wff hwff path bl args2 banned-occs)
  (if bl
      (if (member (car bl) args2)
	  (make-clos-setvar-lemma-negf-6 usel wff hwff path (cdr bl)
					 (remove (car bl) args2 :count 1) banned-occs)
	(multiple-value-bind
	 (f1 f2)
	 (make-clos-setvar-lemma-negf-6
	  usel (substitute-l-term-var (car bl) (bindvar wff) (cdr wff))
	  hwff path (cdr bl) args2 banned-occs)
	 (values (make-ftree-exp wff (list (car bl)) (list f1)) f2)))
    (make-clos-setvar-lemma-negf-7 usel wff hwff path args2)))
;				   (setdiff args2 banned-occs)

; returns f-(wff) f+(hwff)|NIL
(defun make-clos-setvar-lemma-negf-7 (usel wff hwff path args2)
  (if args2
      (if (or path (cdr args2)) ; cebrown 6/03/2002
	  (progn
	    (unless (and (and-p wff) (equals-p (cdar wff))
			 (wffeq-ab (cdadar wff) (cddar wff)))
	      (break) (throwfail "Expected " (wff . gwff) " to be a conjunction [[s = s] AND A]"))
	    (let* ((refl (cdar wff))
		   (lft (cdar refl))
		   (rght (cdr refl))
		   (reflf (if (wffeq lft rght)
			      (make-ftree-rew refl 'REFL= (make-ftree-true nil))
			    (make-ftree-rew refl 'AB
					    (make-ftree-rew (cons (car refl) lft) 'REFL=
							    (make-ftree-true nil))))))
	      (multiple-value-bind
	       (f1 f2)
	       (make-clos-setvar-lemma-negf-7 usel (cdr wff) hwff path (cdr args2))
	       (values (make-ftree-con reflf f1) f2))))
	(progn
	  (when hwff
	    (break) (throwfail "Problem constructing closure setvar lemma"))
	  (unless (and (equals-p wff) (wffeq-ab (cdar wff) (cdr wff)))
	    (break) (throwfail "Expected " (wff . gwff) " to be [s = s]"))
	  (let* ((lft (cdar wff))
		 (rght (cdr wff)))
	    (values 
	     (if (wffeq lft rght)
		 (make-ftree-rew wff 'REFL= (make-ftree-true nil))
	       (make-ftree-rew wff 'AB
			       (make-ftree-rew (cons (car wff) lft) 'REFL=
					       (make-ftree-true nil))))
	     nil))))
    (make-clos-setvar-lemma-negf-8 usel wff hwff path)))

; returns f-(wff) f+(hwff)
(defun make-clos-setvar-lemma-negf-8 (usel wff hwff path)
  (if (cdr path)
      (progn
	(unless (and-p wff)
	  (break) (throwfail "Expected " (wff . gwff) " to be a conjunction"))
	(unless (and-p hwff)
	  (break) (throwfail "Expected " (hwff . gwff) " to be a conjunction"))
	(multiple-value-bind
	 (f1l f2l)
	 (make-clos-setvar-lemma-negf-9 usel (cdar wff) (cdar hwff))
	 (multiple-value-bind
	  (f1r f2r)
	  (make-clos-setvar-lemma-negf-8 usel (cdr wff) (cdr hwff) (cdr path))
	  (values (make-ftree-con f1l f1r) (make-ftree-con f2l f2r)))))
    (make-clos-setvar-lemma-negf-9 usel wff hwff)))

; returns f-(wff1) f+(wff2)
(defun make-clos-setvar-lemma-negf-9 (usel wff1 wff2)
  (if (wffeq-ab wff1 wff2)
      (let ((f1 (make-ftree-leaf wff1 nil))
	    (f2 (make-ftree-leaf wff2 t)))
	(push-onto-clist f1 f2)
	(values f1 f2))
    (progn
      (unless (e-bd-wff-p wff1)
	(break) (throwfail "Expect " (wff1 . gwff) " to be an existential wff"))
      (let* ((wff3 (substitute-l-term-var usel (bindvar wff1) (cdr wff1)))
	     (f1c (make-ftree-leaf (cdr wff3) nil))
	     (f2 (make-ftree-leaf wff2 t)))
	(unless (wffeq-ab (cdr wff3) wff2)
	  (break) (throwfail "Expected " (wff2 . gwff) t " to be the same as " ((cdr wff3) . gwff)))
	(push-onto-clist f2 f1c)
	(values (make-ftree-exp wff1 (list usel)
				(list (make-ftree-con
				       (make-forall-a-imp-a-negf (cdar wff3))
				       f1c)))
		f2)))))

(defun make-clos-setvar-ind-negf (lfwff indwff paths banned-occurs)
  (declare (special lff))
  (let* ((p (fresh-var (unabbreviated-type (bindvar lfwff)) '\p))
	 (lfwff1 (substitute-l-term-var p (bindvar lfwff) (cdr lfwff)))
	 (indwff1 (substitute-l-term-var p (bindvar indwff) (cdr indwff)))
	 (lfpre (cdar lfwff1))
	 (lfconcf (make-ftree-leaf (cdr lfwff1) t))
	 (indhyp (cdar indwff1))
	 (indconcf (make-ftree-leaf (cdr indwff1) nil)))
    (push-onto-clist lfconcf indconcf)
    (multiple-value-bind
     (indhypf lfhypf)
     (make-clos-setvar-ind-negf-1 indhyp lfpre paths banned-occurs)
     (setq lff (make-ftree-exp lfwff (list p) (list (make-ftree-imp lfhypf lfconcf))))
     (make-ftree-sel indwff p (make-ftree-imp indhypf indconcf)))))

; returns two values: a positive ftree for wff1, a negative ftree for wff2
(defun make-clos-setvar-ind-negf-1 (wff1 wff2 paths banned-occurs)
  (if (a-bd-wff-p wff2)
      (let* ((z (bindvar wff2))
	     (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	(multiple-value-bind
	 (pf nf)
	 (make-clos-setvar-ind-negf-1 wff1 
				      (substitute-l-term-var z2 z (cdr wff2))
				      paths banned-occurs)
	 (values pf (make-ftree-sel wff2 z2 nf))))
    (let ((leaf (make-ftree-leaf (cdr wff2) nil)))
      (multiple-value-bind
       (pf1 pf2)
       (make-clos-setvar-ind-negf-2 wff1 (cdar wff2) leaf paths banned-occurs)
       (values pf1 (make-ftree-imp pf2 leaf))))))

(defun make-clos-setvar-ind-negf-2 (wff1 wff2 leaf paths banned-occurs)
  (if paths
      (let ((aassoc (construct-arg-assoc (ftree-shallow (caar paths)) (ftree-shallow leaf)))
	    (psi (car banned-occurs))
	    (psi0 nil)
	    (psi-z-assoc nil))
	(dolist (w psi)
		(let ((a (assoc w aassoc)))
		  (if a
		      (push a psi-z-assoc)
		    (push w psi0))))
	(setq psi0 (reverse psi0)) ; cebrown 6/4/02
	(if (cdr paths)
	    (progn
	      (unless (and-p wff1)
		(throwfail "Expected " (wff1 . gwff) " to be a conjunction"))
	      (unless (or-p wff2)
		(throwfail "Expected " (wff2 . gwff) " to be a disjunction"))
	      (multiple-value-bind
	       (pfC1 pfD1)
	       (make-clos-setvar-ind-negf-3
		psi0 (cdar wff1) (cdar wff2) leaf (cdar paths) psi psi-z-assoc)
	       (multiple-value-bind
		(pfC2 pfD2)
		(make-clos-setvar-ind-negf-2
		 (cdr wff1) (cdr wff2) leaf (cdr paths) (cdr banned-occurs))
		(values (make-ftree-con pfC1 pfC2) (make-ftree-dis pfD1 pfD2)))))
	  (make-clos-setvar-ind-negf-3
	   psi0 wff1 wff2 leaf (cdar paths) psi psi-z-assoc)))
    (throwfail "Problem Creating Induction Principle")))

(defun make-clos-setvar-ind-negf-3 (psi0 wff1 wff2 leaf gamma psi psi-z-assoc
					 &optional psi-w-assoc)
  (if psi0
      (let* ((w (car psi0))
	     (w2 (fresh-var (unabbreviated-type w) (getnameroot w))))
	(unless (and (e-bd-wff-p wff2)
		     (equal (unabbreviated-type (bindvar wff2))
			    (unabbreviated-type w)))
	  (throwfail "Problem Proving Induction Principle"))
	(multiple-value-bind
	 (pfC1 pfD1)
	 (make-clos-setvar-ind-negf-3
	  (cdr psi0) wff1 (substitute-l-term-var w2 (bindvar wff2) (cdr wff2))
	  leaf gamma psi psi-z-assoc
	  (acons w w2 psi-w-assoc))
	 (values pfC1 (make-ftree-sel wff2 w2 pfD1))))
    (make-clos-setvar-ind-negf-4 psi wff1 wff2 leaf
				 gamma psi-z-assoc psi-w-assoc)))

(defun make-clos-setvar-ind-negf-4 (psi wff1 wff2 leaf gamma
					psi-z-assoc psi-w-assoc)
  (if psi
      (let* ((w (car psi))
	     (wz (assoc w psi-z-assoc))
	     (ww (assoc w psi-w-assoc))
	     (exptrm nil))
	(if wz
	    (if ww
		(throwfail "Problem Proving Induction Principle - w/z variable conflict")
	      (setq exptrm (cdr wz)))
	  (if ww
	      (setq exptrm (cdr ww))
	    (throwfail "Problem Proving Induction Principle - " (w . gwff))))
	(unless (a-bd-wff-p wff1)
	  (throwfail "Problem Proving Induction Principle"))
	(multiple-value-bind
	 (pfC pfD)
	 (make-clos-setvar-ind-negf-4 
	  (cdr psi) (substitute-l-term-var exptrm (bindvar wff1) (cdr wff1))
	  wff2 leaf gamma psi-z-assoc psi-w-assoc)
	 (values (make-ftree-exp wff1 (list exptrm) (list pfC)) pfD)))
    (if gamma
	(progn
	  (unless (implies-p wff1)
	    (throwfail "Problem Proving Induction Principle"))
	  (let ((hyp (cdar wff1))
		(conc (cdr wff1)))
	    (make-clos-setvar-ind-negf-5 (head conc) hyp conc wff2 leaf
					 (args conc) (args (ftree-shallow leaf))
					 gamma)))
	(make-clos-setvar-ind-negf-5 (head wff1) nil wff1 wff2 leaf
				     (args wff1) (args (ftree-shallow leaf))
				     gamma))))

(defun make-clos-setvar-ind-negf-5 (pargs hyp conc wff2 leaf args1 args2 gamma)
  (declare (special clist))
  (if args1
      (if args2
	  (let ((arg1 (car args1))
		(arg2 (car args2)))
	    (if (wffeq arg1 arg2)
		(make-clos-setvar-ind-negf-5
		 (cons pargs arg1)
		 hyp conc wff2 leaf (cdr args1) (cdr args2)
		 gamma)
	      (progn
		(unless (or (and (equals-p wff2)
				 (wffeq-ab (cdar wff2) arg2)
				 (wffeq-ab (cdr wff2) arg1))
			    (and (and-p wff2) (equals-p (cdar wff2))
				 (wffeq-ab (cdadar wff2) arg2)
				 (wffeq-ab (cddar wff2) arg1)))
		  (throwfail "Problem Proving Induction Principle"))
		(let ((eqn (if (and-p wff2) (cdar wff2) wff2)))
		  (multiple-value-bind
		   (leaf2 eqnf new-conns)
		   (make-ftree-subst (apply-wff-n (cons pargs arg1)
						  (reverse (cdr args2)))
				     eqn leaf)
		   (setq clist (append new-conns clist))
		   (multiple-value-bind
		    (pfC pfD)
		    (make-clos-setvar-ind-negf-5
		     (cons pargs arg1) hyp conc
		     (if (and-p wff2) (cdr wff2) nil)
		     leaf2 (cdr args1) (cdr args2) gamma)
		    (values pfC (if pfD (make-ftree-con eqnf pfD) eqnf))))))))
	(throwfail "Problem Proving Induction Principle"))
    (if args2
	(throwfail "Problem Proving Induction Principle")
      (let ((leaf2 (make-ftree-leaf conc t)))
	(unless (wffeq-ab conc (ftree-shallow leaf))
	  (throwfail "Problem Proving Induction Principle"))
	(push-onto-clist leaf leaf2)
	(if gamma
	    (progn
	      (unless (and hyp wff2 (wffeq-ab hyp wff2))
		(throwfail "Problem Proving Induction Principle"))
	      (let ((nfC (make-ftree-leaf hyp nil))
		    (pfD (make-ftree-leaf wff2 t)))
		(push-onto-clist nfC pfD)
		(values (make-ftree-imp nfC leaf2) pfD)))
	  (progn
	    (when (or hyp wff2)
	      (throwfail "Problem Proving Induction Principle"))
	    (values leaf2 nil)))))))

(defun make-min-setvar-lemma-negf (lemma vsel misc-occurs inv-princ paths banned-occurs)
  (if misc-occurs
      (let* ((a (bindvar lemma))
	     (b (fresh-var (unabbreviated-type a) (getnameroot a))))
	(make-ftree-sel
	 lemma b
	 (make-min-setvar-lemma-negf (substitute-l-term-var b a (cdr lemma))
				     vsel (cdr misc-occurs)
				     (substitute-l-term-var b (car misc-occurs)
							    inv-princ)
				     paths banned-occurs)))
    (let* ((soln (min-setvar-soln-from-inv-princ inv-princ))
	   (i (substitute-l-term-var soln (bindvar lemma) (cdr lemma)))
	   (l (lnorm i)))
      (if (wffeq i l)
	  (make-ftree-exp
	   lemma (list soln) (list (make-min-setvar-lemma-negf-1 l paths banned-occurs)))
	(make-ftree-exp 
	 lemma (list soln)
	 (list (make-ftree-rew
		i 'LAMBDA
		(make-min-setvar-lemma-negf-1 l paths banned-occurs))))))))

(defun min-setvar-soln-from-inv-princ (inv-princ)
  (if (a-bd-wff-p inv-princ)
      (acons (bindvar inv-princ) 'LAMBDA
	     (min-setvar-soln-from-inv-princ (cdr inv-princ)))
    (if (implies-p inv-princ)
	(cdr inv-princ)
      (throwfail "Problem with Inversion Principle solving Minimal Constraint"))))

(defun make-min-setvar-lemma-negf-1 (wff paths banned-occurs)
  (make-ftree-con
   (make-min-setvar-lemma-negf-3 (cdar wff) paths banned-occurs)
   (if INCLUDE-INDUCTION-PRINCIPLE
       (make-ftree-con
	(make-min-setvar-lemma-negf-2 (cdadr wff))
	(make-min-setvar-ind-negf (cddr wff) paths banned-occurs))
     (make-min-setvar-lemma-negf-2 (cdr wff)))))

(defun make-min-setvar-lemma-negf-2 (wff)
  (if (a-bd-wff-p wff)
      (let* ((z (bindvar wff))
	     (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	(make-ftree-sel wff z2 (make-min-setvar-lemma-negf-2
				(substitute-l-term-var z2 z (cdr wff)))))
    (if (and (implies-p wff)
	     (wffeq-ab (cdar wff) (cdr wff)))
	(let ((l (make-ftree-leaf (cdar wff) t))
	      (r (make-ftree-leaf (cdr wff) nil)))
	  (push-onto-clist l r)
	  (make-ftree-imp l r))
      (throwfail "Problem Generating Min Solution"))))

(defun make-min-setvar-lemma-negf-3 (wff paths banned-occurs &optional (n 0))
  (if paths
      (if (cdr paths)
	  (progn
	    (unless (and-p wff)
	      (throwfail "Expected " (wff . gwff) " to be a conjunction"))
	    (make-ftree-con
	     (make-min-setvar-lemma-negf-4 (car banned-occurs) (cdar wff) 
					   (args (ftree-shallow (caar paths)))
					   (cdar paths) n)
	     (make-min-setvar-lemma-negf-3 (cdr wff) (cdr paths) (cdr banned-occurs) (1+ n))))
	(make-min-setvar-lemma-negf-4 (car banned-occurs) wff
				      (args (ftree-shallow (caar paths))) (cdar paths) n))
    (throwfail "Problem Proving Min Soln")))

(defun make-min-setvar-lemma-negf-4 (psi wff args gamma n &optional (k 0) bl)
  (if psi
      (progn
	(unless (a-bd-wff-p wff)
	  (throwfail "Expected Univ: " (wff . gwff)))
	(let* ((z (bindvar wff))
	       (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	  (if (member (car psi) args)
	      (incf k)
	    (push z2 bl))
	  (make-ftree-sel wff z2
			  (make-min-setvar-lemma-negf-4
			   (cdr psi) (substitute-l-term-var z2 z (cdr wff))
			   args gamma n k bl))))
    (if (implies-p wff)
	(let ((l (make-ftree-leaf (cdar wff) t)))
	  (make-ftree-imp
	   l (make-min-setvar-lemma-negf-5 n (reverse bl) 
					   (- (length args) k)
					   (cdr wff) gamma l)))
      (make-min-setvar-lemma-negf-5 n (reverse bl)
				    (- (length args) k)
				    wff gamma nil))))

(defun make-min-setvar-lemma-negf-5 (n bl m wff gamma l)
  (if (> n 0)
      (if (or-p wff)
	  (make-ftree-dis (make-ftree-leaf (cdar wff) nil)
			  (make-min-setvar-lemma-negf-5 (- n 1) bl m (cdr wff) gamma l))
	(throwfail "Problem Constructing Minimal Solution"))
    (if (or-p wff)
	(make-ftree-dis
	 (make-min-setvar-lemma-negf-6 bl m (cdar wff) gamma l)
	 (make-ftree-leaf (cdr wff) nil))
      (make-min-setvar-lemma-negf-6 bl m wff gamma l))))

(defun make-min-setvar-lemma-negf-6 (bl m wff gamma l)
  (if bl
      (make-ftree-exp wff (list (car bl))
		      (list
		       (make-min-setvar-lemma-negf-6
			(cdr bl) m (substitute-l-term-var (car bl) (bindvar wff) (cdr wff))
			gamma l)))
    (make-min-setvar-lemma-negf-7 m wff gamma l)))

(defun make-min-setvar-lemma-negf-7 (m wff gamma l)
  (if (> m 0)
      (if (or gamma (> m 1))
	  (let ((eqn (cdar wff)))
	    (make-ftree-con (make-ftree-rew eqn 'REFL= (make-ftree-true nil))
			    (make-min-setvar-lemma-negf-7 (- m 1) (cdr wff) gamma l)))
	(make-ftree-rew wff 'REFL= (make-ftree-true nil)))
    (if l
	(if (wffeq-ab (ftree-shallow l) wff)
	    (let ((r (make-ftree-leaf wff nil)))
	      (push-onto-clist l r)
	      r)
	  (throwfail "Expected " (wff . gwff) t " to equal " ((ftree-shallow l) . gwff)))
      (throwfail "Problem Proving Min Soln"))))

(defun make-min-setvar-ind-negf (wff paths banned-occurs)
  (let* ((p (bindvar wff))
	 (p2 (fresh-var (unabbreviated-type p) (getnameroot p)))
	 (body (substitute-l-term-var p2 p (cdr wff))))
    (multiple-value-bind
     (hypf concf)
     (make-clos-setvar-ind-negf-1 (cdar body) (cdr body) paths banned-occurs)
     (make-ftree-sel wff p2 (make-ftree-imp hypf concf)))))

(defun make-min-setvar-lemma-posf (inv-princ vsel paths banned-occurs misc-occurs
					     substitutable-vars)
  (if misc-occurs
      (let* ((a (car misc-occurs))
	     (b (fresh-var (unabbreviated-type a) (getnameroot a)))
	     (f (make-min-setvar-lemma-posf
		 inv-princ vsel paths banned-occurs (cdr misc-occurs)
		 substitutable-vars))
	     (expf (make-ftree-exp
		    (acons b 'forall (substitute-l-term-var b a (ftree-shallow f)))
		    (list a)
		    (list f))))
	(when (member a substitutable-vars)
	  (push (cons a a) (ftree-exp-subs expf)))
	expf)
    (let ((f (make-min-setvar-lemma-posf-1 inv-princ vsel paths banned-occurs))
	  (v2 (fresh-var (unabbreviated-type vsel) (getnameroot vsel))))
      (make-ftree-sel (acons v2 'exists 
			     (substitute-l-term-var v2 vsel (ftree-shallow f)))
		      vsel f))))

(defun make-min-setvar-lemma-posf-1 (inv-princ vsel paths banned-occurs)
  (declare (special NUM-OF-DUPS))
  (let* ((f1 (make-min-setvar-lemma-posf-2 paths banned-occurs vsel))
	 (f2 (if (consp (unabbreviated-type vsel))
		 (gwff-to-ftree-dup inv-princ t NUM-OF-DUPS)
	       (gwff-to-ftree inv-princ t))))
    (if include-induction-principle
	(let* ((p (fresh-var (unabbreviated-type vsel) '\p))
	       (indwff 
		(acons p 'FORALL
		       (acons 'IMPLIES
			      (substitute-l-term-var p vsel (ftree-shallow f1))
			      (make-subreln-wff vsel p)))))
	  (make-ftree-con f1 (make-ftree-con f2 (gwff-to-ftree-dup indwff t NUM-OF-DUPS))))
      (make-ftree-con f1 f2))))

(defun make-min-setvar-lemma-posf-2 (paths banned-occurs vsel)
  (if paths
      (if (cdr paths)
	  (make-ftree-con (make-min-setvar-lemma-posf-3
			   (car paths) (car banned-occurs) vsel)
			  (make-min-setvar-lemma-posf-2
			   (cdr paths) (cdr banned-occurs) vsel))
	(make-min-setvar-lemma-posf-3 (car paths) (car banned-occurs) vsel))
    (throwfail "Problem constructing Min Solution")))

(defun make-min-setvar-lemma-posf-3 (path banned-occs vsel)
  (declare (special NUM-OF-DUPS))
  (if banned-occs
      ; duplicate
      (let* ((kidsl nil)
	     (expvl nil)
	     (w (car banned-occs))
	     (w3 (fresh-var (unabbreviated-type w) (getnameroot w)))
	     (f (make-min-setvar-lemma-posf-4 path (cdr banned-occs) vsel)))
	(do ((w2 (fresh-var (unabbreviated-type w) (getnameroot w))
		 (fresh-var (unabbreviated-type w) (getnameroot w)))
	     (i 0 (1+ i)))
	    ((> i num-of-dups)
	     (push w2 expvl)
	     (push (gwff-to-ftree (substitute-l-term-var w2 w (ftree-shallow f)) t) kidsl)))
	(let ((exp (make-ftree-exp (acons w3 'forall
					  (substitute-l-term-var w3 w (ftree-shallow f)))
				   (cons w expvl)
				   (cons f kidsl))))
	  (setf (ftree-exp-subs exp)
		(append (mapcar #'(lambda (x)
				    (cons x x))
				expvl)
			(ftree-exp-subs exp)))
	  exp))
    (let ((l (make-ftree-leaf (ftree-shallow (car path))
			      (not (ftree-positive (car path))))))
      (push-onto-clist l (car path))
      (if (cdr path)
	  (make-ftree-imp
	   (make-min-setvar-lemma-posf-5 (cdr path) vsel)
	   l)
	l))))

(defun make-min-setvar-lemma-posf-4 (path banned-occs vsel)
  (if banned-occs
      (let* ((f (make-min-setvar-lemma-posf-4 path (cdr banned-occs) vsel))
	     (w (car banned-occs))
	     (w3 (fresh-var (unabbreviated-type w) (getnameroot w))))
	(make-ftree-exp 
	 (acons w3 'forall (substitute-l-term-var w3 w (ftree-shallow f)))
	 (list (car banned-occs))
	 (list f)))
    (let ((l (make-ftree-leaf (ftree-shallow (car path)) t)))
      (push-onto-clist l (car path))
      (if (cdr path)
	  (make-ftree-imp
	   (make-min-setvar-lemma-posf-5 (cdr path) vsel)
	   l)
	l))))

(defun make-min-setvar-lemma-posf-5 (lits vsel)
  (if lits
      (if (cdr lits)
	  (make-ftree-con
	   (make-min-setvar-lemma-posf-6 (car lits) vsel)
	   (make-min-setvar-lemma-posf-5 (cdr lits) vsel))
	(make-min-setvar-lemma-posf-6 (car lits) vsel))
    (throwfail "Problem constructing Min Solution")))

(defun make-min-setvar-lemma-posf-6 (lit vsel)
  (let* ((l (make-ftree-leaf (ftree-shallow lit) (not (ftree-positive lit))))
	 (f (if (ftree-positive lit) l (make-ftree-neg l))))
    (push-onto-clist l lit)
    (if (find-if #'(lambda (arg)
		     (free-in vsel arg))
		 (args (ftree-shallow lit)))
	(let* ((w (fresh-var (unabbreviated-type vsel) '\w))
	       (zl nil))
	  (do ((y (unabbreviated-type w) (car y)))
	      ((atom y))
	      (push (fresh-var (cdr y) '\z) zl))
	  (setq zl (reverse zl))
	  (let* ((wff2 (bind-var-wff-n 'forall zl
				       (acons 'IMPLIES
					      (apply-wff-n w zl)
					      (apply-wff-n vsel zl))))
		 (wff3 (bind-var-wff-n 'forall zl
				       (acons 'IMPLIES
					      (apply-wff-n vsel zl)
					      (apply-wff-n vsel zl))))
		 (wff (acons w 'exists
			     (acons 'AND wff2
				    (substitute-l-term-var w vsel (ftree-shallow f))))))
	    (make-ftree-exp
	     wff (list vsel) (list (make-ftree-con
				    (make-forall-a-imp-a-negf wff3)
				    f)))))
      f)))

; max code
(defun make-max-inv-princ (vsel paths banned-occurs)
  (let ((zl nil)
	(vzl vsel))
    (do ((y (unabbreviated-type vsel) (car y)))
	((not (consp y)))
	(push (fresh-var-1 (cdr y)) zl)
	(setq vzl (cons vzl (car zl))))
    (bind-var-wff-n 'forall zl
		    (make-max-inv-princ-1 vsel zl vzl paths banned-occurs))))

(defun make-max-inv-princ-1 (vsel zl vzl paths banned-occurs)
  (let ((wff (make-max-inv-princ-2 vsel zl vzl paths banned-occurs)))
    (acons 'IMPLIES wff vzl)))

(defun make-max-inv-princ-2 (vsel zl vzl paths banned-occurs)
  (if paths
      (let ((wff2 (make-max-inv-princ-2 vsel zl vzl (cdr paths) (cdr banned-occurs)))
	    (wff1 (make-max-inv-princ-3 vsel zl vzl (ftree-shallow (caar paths))
					   (cdar paths) (car banned-occurs))))
	(if wff2
	    (acons 'AND wff1 wff2)
	  wff1))
    nil))

(defun make-max-inv-princ-3 (vsel zl vzl main path banned-occs &optional aassoc eqns)
  (if (and (consp main) (consp vzl))
      (let ((m (member (cdr main) banned-occs)))
	(if m
	    (make-max-inv-princ-3 vsel zl (car vzl) (car main) path
				     (remove (car m) banned-occs)
				     (acons (cdr main) (cdr vzl) aassoc) eqns)
	  (make-max-inv-princ-3 vsel zl (car vzl) (car main) path banned-occs aassoc
				   (cons (mbed=left (cdr vzl) (cdr main)) eqns))))
    (make-max-inv-princ-4 vsel zl eqns path banned-occs aassoc)))

(defun make-max-inv-princ-4 (vsel zl eqns path banned-occs aassoc)
  (if banned-occs
      (let* ((w (car banned-occs))
	     (w2 (fresh-var (unabbreviated-type w) (getnameroot w))))
	(acons w2 'forall
	       (make-max-inv-princ-4 vsel zl eqns path (cdr banned-occs)
					(acons w w2 aassoc))))
    (make-max-inv-princ-5 vsel zl eqns path aassoc)))

(defun make-max-inv-princ-5 (vsel zl eqns path aassoc)
  (if eqns
      (let ((wff (make-max-inv-princ-5 vsel zl (cdr eqns) path aassoc)))
	(if wff
	    (acons 'OR (cons 'NOT (simul-substitute-l-term-var aassoc (car eqns))) wff)
	  (cons 'NOT (simul-substitute-l-term-var aassoc (car eqns)))))
    (make-max-inv-princ-6 vsel zl path aassoc)))

(defun make-max-inv-princ-6 (vsel zl path aassoc)
  (if path
      (let* ((wff (make-max-inv-princ-6 vsel zl (cdr path) aassoc))
	     (wff2 (simul-substitute-l-term-var aassoc (ftree-shallow (car path))))
	     (wff3 (if (ftree-positive (car path)) (cons 'NOT wff2) wff2))
	     (wff4 (if (find-if #'(lambda (arg)
				    (free-in vsel arg))
				(args wff2))
		       (let ((w (fresh-var (unabbreviated-type vsel) '\w)))
			 (acons w 'forall
				(acons 'IMPLIES
				       (bind-var-wff-n 'forall zl
						       (acons 'IMPLIES
							      (apply-wff-n vsel zl)
							      (apply-wff-n w zl)))
				       (substitute-l-term-var w vsel wff3))))
		     wff3)))
	(if wff
	    (acons 'OR wff4 wff)
	  wff4))
    nil))

(defun make-coclos-setvar-lemma-negf (lemma vsel paths banned-occurs misc-occurs)
  (declare (special clist))
  (multiple-value-bind
   (ktnegf ktclist)
   (if include-coinduction-principle
       (make-knaster-tarski-gfp-lemma (unabbreviated-type vsel))
     (make-knaster-tarski-lemma (unabbreviated-type vsel)))
   (setq clist (append ktclist clist))
   (let (expf)
     (declare (special expf))
     (let ((negf (make-coclos-setvar-lemma-negf-0
		  lemma (ftree-shallow ktnegf) vsel paths
		  banned-occurs misc-occurs)))
       (make-ftree-con ktnegf (make-ftree-imp expf negf))))))

(defun make-coclos-setvar-lemma-negf-0 (lemma ktgfp vsel paths banned-occurs misc-occurs)
  (declare (special expf))
  (if misc-occurs
      (let* ((a (bindvar lemma))
	     (b (fresh-var (unabbreviated-type a) (getnameroot a))))
	(make-ftree-sel
	 lemma b
	 (make-coclos-setvar-lemma-negf-0 (substitute-l-term-var b a (cdr lemma))
					  ktgfp vsel paths banned-occurs (cdr misc-occurs))))
    (let* ((inv-princ (if include-coinduction-principle
			  (substitute-l-term-var vsel (bindvar lemma) (cdaddr lemma))
			(substitute-l-term-var vsel (bindvar lemma) (cddr lemma))))
	   (monfn (acons vsel
			 'lambda (make-mon-setfn-from-lside inv-princ)))
	   (wff2 (substitute-l-term-var monfn (bindvar ktgfp) (cdr ktgfp)))
	   (wff3 (lnorm wff2))
	   (monnegf (mon-fn-negf (cdar wff3)))
	   (eu (cdr wff3))
	   (usel (fresh-var (unabbreviated-type (bindvar eu)) (getnameroot (bindvar eu))))
	   (wff4 (substitute-l-term-var usel (bindvar eu) (cdr eu)))
	   (uKu (if include-coinduction-principle
		    (cddar wff4)
		  (cdr wff4)))
	   (Kuu (if include-coinduction-principle
		    (cdadar wff4)
		  (cdar wff4)))
	   (gfwff (when include-coinduction-principle (cdr wff4)))
	   (gff nil)
	   (uKuf (make-ftree-leaf uKu t))
	   (Kuuf (make-ftree-leaf Kuu t))
	   (lemma2 (substitute-l-term-var usel (bindvar lemma) (cdr lemma)))
	   (lemma3 (if include-coinduction-principle
		       (cdadr lemma2)
		     (cdr lemma2)))
	   (l (make-ftree-leaf lemma3 nil)))
      (declare (special uKuf gff))
      (unless (wffeq-ab lemma3 Kuu)
	(msgf "lemma3 " (lemma3 . gwff) t
	      "uKu " (uKu . gwff) t "do not match")
	(break))
      (push-onto-clist l Kuuf)
      (let* ((f (make-coclos-setvar-lemma-negf-1 usel uKu (cdar lemma2) paths))
	     (indf (when include-coinduction-principle
		     (make-coclos-setvar-ind-negf gfwff (cddr lemma2) paths banned-occurs)))
	     (impf (make-ftree-imp monnegf (make-ftree-sel eu usel
							   (if include-coinduction-principle
							       (make-ftree-con
								(make-ftree-con Kuuf uKuf)
								gff)
							     (make-ftree-con Kuuf uKuf))))))
	(setq expf (make-ftree-exp ktgfp
				   (list monfn)
				   (list (if (wffeq wff2 wff3)
					     impf
					   (make-ftree-rew wff2 'LAMBDA impf)))))
	(make-ftree-exp lemma (list usel)
			(if include-coinduction-principle
			    (list (make-ftree-con f (make-ftree-con l indf)))
			  (list (make-ftree-con f l))))))))

; returns f-(wff)
(defun make-coclos-setvar-lemma-negf-1 (usel uKu wff paths &optional (n 0))
  (if (and-p wff)
      (make-ftree-con
       (make-coclos-setvar-lemma-negf-2 usel uKu (cdar wff) nil paths n)
       (make-coclos-setvar-lemma-negf-1 usel uKu (cdr wff) paths (1+ n)))
    (make-coclos-setvar-lemma-negf-2 usel uKu wff nil paths n)))

; returns f-(wff)
(defun make-coclos-setvar-lemma-negf-2 (usel uKu wff bl paths n)
  (declare (special uKuf))
  (if (a-bd-wff-p wff)
      (let* ((z (bindvar wff))
	     (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	(make-ftree-sel wff z2
			(make-coclos-setvar-lemma-negf-2
			 usel uKu
			 (substitute-l-term-var z2 z (cdr wff))
			 (cons z2 bl) paths n)))
    (if (implies-p wff)
	(let ((args2 (args (cdar wff))))
	  (multiple-value-bind
	   (f1 f2 f3)
	   (make-coclos-setvar-lemma-negf-3 usel uKu args2 (cdr wff) bl
					    n paths args2)
	   (setq uKuf (constr-merge-ftrees f1 uKuf))
	   (make-ftree-imp (make-coclos-setvar-lemma-negf-4 f3 (cdar wff)) f2)))
      (if (not-p wff)
	  (let ((args2 (args (cdr wff))))
	    (multiple-value-bind
	     (f1 f2 f3)
	     (make-coclos-setvar-lemma-negf-3 usel uKu args2 nil bl
					      n paths args2)
	     (declare (ignore f2))
	     (setq uKuf (constr-merge-ftrees f1 uKuf))
	     (make-ftree-neg (make-coclos-setvar-lemma-negf-4 f3 (cdr wff)))))
	(throwfail "problem creating coclosure setvar soln")))))

; return f+(uKu) f-(hwff) f-(hyp(uKu))
(defun make-coclos-setvar-lemma-negf-3 (usel uKu args hwff bl n paths args2)
  (if args
      (multiple-value-bind
       (f1 f2 f3)
       (make-coclos-setvar-lemma-negf-3
	usel (substitute-l-term-var (car args) (bindvar uKu) (cdr uKu))
	(cdr args) hwff bl n paths args2)
       (values (make-ftree-exp uKu (list (car args)) (list f1)) f2 f3))
    (let ((f3 (make-ftree-leaf (cdar uKu) nil)))
      (multiple-value-bind
       (f1 f2)
       (make-coclos-setvar-lemma-negf-5 usel (cdr uKu) hwff bl n paths args2)
       (values (make-ftree-imp f3 f1) f2 f3)))))

; f+(cwff)
(defun make-coclos-setvar-lemma-negf-4 (f3 cwff)
  (let ((cf (make-ftree-leaf cwff t)))
    (push-onto-clist f3 cf)
    cf))

; returns f+(wff) f-(hwff)
(defun make-coclos-setvar-lemma-negf-5 (usel wff hwff bl n paths args2)
  (if (> n 0)
      (progn
	(unless paths
	  (break) (throwfail "Problem creating coclosure setvar constraint proof"))
	(unless (and-p wff)
	  (break) (throwfail "Expected " (wff . gwff) " to be a conjunction"))
	(multiple-value-bind
	 (f1 f2)
	 (make-coclos-setvar-lemma-negf-5
	  usel (cdr wff) hwff bl (- n 1) (cdr paths) args2)
	 (values (make-ftree-con (make-ftree-leaf (cdar wff) t) f1) f2)))
    (if (cdr paths)
	(progn
	  (unless (and-p wff)
	    (break) (throwfail "Expected " (wff . gwff) " to be a conjunction"))
	  (multiple-value-bind
	   (f1 f2)
	   (make-coclos-setvar-lemma-negf-6 usel (cdar wff) hwff (cdar paths) (reverse bl) args2 bl)
	   (values (make-ftree-con f1 (make-ftree-leaf (cdr wff) t)) f2)))
      (make-coclos-setvar-lemma-negf-6 usel wff hwff (cdar paths) (reverse bl) args2 bl))))

; f+(wff) f-(hwff)|NIL
(defun make-coclos-setvar-lemma-negf-6 (usel wff hwff path bl args2 banned-occs)
  (if bl
      (if (member (car bl) args2)
	  (make-coclos-setvar-lemma-negf-6 usel wff hwff path (cdr bl) args2 banned-occs)
	(multiple-value-bind
	 (f1 f2)
	 (make-coclos-setvar-lemma-negf-6
	  usel (substitute-l-term-var (car bl) (bindvar wff) (cdr wff))
	  hwff path (cdr bl) args2 banned-occs)
	 (values (make-ftree-exp wff (list (car bl)) (list f1)) f2)))
    (make-coclos-setvar-lemma-negf-7 usel wff hwff path (setdiff args2 banned-occs))))

; f+(wff) f-(hwff)|NIL
(defun make-coclos-setvar-lemma-negf-7 (usel wff hwff path args2)
  (if args2
      (if (or path (cdr args2)) ; cebrown 6/03/2002
	  (progn
	    (unless (and (or-p wff) (not-p (cdar wff)) (equals-p (cddar wff))
			 (wffeq-ab (cdar (cddar wff)) (cdddar wff)))
	      (break) (throwfail "Expected " (wff . gwff) " to be a disjunction [~[s = s] OR A]"))
	    (let* ((refl (cddar wff))
		   (lft (cdar refl))
		   (rght (cdr refl))
		   (reflf (make-ftree-neg
			   (if (wffeq lft rght)
			       (make-ftree-rew refl 'REFL= (make-ftree-true nil))
			     (make-ftree-rew refl 'AB
					     (make-ftree-rew (cons (car refl) lft) 'REFL=
							     (make-ftree-true nil)))))))
	      (multiple-value-bind
	       (f1 f2)
	       (make-coclos-setvar-lemma-negf-7 usel (cdr wff) hwff path (cdr args2))
	       (values (make-ftree-dis reflf f1) f2))))
	(progn
	  (when hwff
	    (break) (throwfail "Problem constructing closure setvar lemma"))
	  (unless (and (not-p wff) (equals-p (cdr wff)) (wffeq-ab (cdadr wff) (cddr wff)))
	    (break) (throwfail "Expected " (wff . gwff) " to be ~[s = s]"))
	  (let* ((lft (cdadr wff))
		 (rght (cddr wff)))
	    (values 
	     (make-ftree-neg
	      (if (wffeq lft rght)
		  (make-ftree-rew (cdr wff) 'REFL= (make-ftree-true nil))
		(make-ftree-rew (cdr wff) 'AB
				(make-ftree-rew (cons (car wff) lft) 'REFL=
						(make-ftree-true nil)))))))))
    (make-coclos-setvar-lemma-negf-8 usel wff hwff path)))

; f+(wff) f-(hwff)|NIL
(defun make-coclos-setvar-lemma-negf-8 (usel wff hwff path)
  (if (cdr path)
      (progn
	(unless (or-p wff)
	  (break) (throwfail "Expected " (wff . gwff) " to be a conjunction"))
	(unless (or-p hwff)
	  (break) (throwfail "Expected " (hwff . gwff) " to be a conjunction"))
	(multiple-value-bind
	 (f1l f2l)
	 (make-coclos-setvar-lemma-negf-9 usel (cdar wff) (cdar hwff))
	 (multiple-value-bind
	  (f1r f2r)
	  (make-coclos-setvar-lemma-negf-8 usel (cdr wff) (cdr hwff) (cdr path))
	  (values (make-ftree-dis f1l f1r) (make-ftree-dis f2l f2r)))))
    (make-coclos-setvar-lemma-negf-9 usel wff hwff)))

; f+(wff1) f-(wff2)|NIL
(defun make-coclos-setvar-lemma-negf-9 (usel wff1 wff2)
  (if (wffeq-ab wff1 wff2)
      (let ((f1 (make-ftree-leaf wff1 t))
	    (f2 (make-ftree-leaf wff2 nil)))
	(push-onto-clist f1 f2)
	(values f1 f2))
    (progn
      (unless (a-bd-wff-p wff1)
	(break) (throwfail "Expect " (wff1 . gwff) " to be a universal wff"))
      (let* ((wff3 (substitute-l-term-var usel (bindvar wff1) (cdr wff1)))
	     (f1c (make-ftree-leaf (cdr wff3) t))
	     (f2 (make-ftree-leaf wff2 nil)))
	(unless (wffeq-ab (cdr wff3) wff2)
	  (break) (throwfail "Expected " (wff2 . gwff) t " to be the same as " ((cdr wff3) . gwff)))
	(push-onto-clist f2 f1c)
	(values (make-ftree-exp wff1 (list usel)
				(list (make-ftree-imp
				       (make-forall-a-imp-a-negf (cdar wff3))
				       f1c)))
		f2)))))

(defun make-coclos-setvar-ind-negf (gfwff indwff paths banned-occurs)
  (declare (special gff))
  (let* ((p (fresh-var (unabbreviated-type (bindvar gfwff)) '\p))
	 (gfwff1 (substitute-l-term-var p (bindvar gfwff) (cdr gfwff)))
	 (indwff1 (substitute-l-term-var p (bindvar indwff) (cdr indwff)))
	 (gfpost (cdar gfwff1))
	 (gfconcf (make-ftree-leaf (cdr gfwff1) t))
	 (indhyp (cdar indwff1))
	 (indconcf (make-ftree-leaf (cdr indwff1) nil)))
    (push-onto-clist gfconcf indconcf)
    (multiple-value-bind
     (indhypf gfhypf)
     (make-coclos-setvar-ind-negf-1 indhyp gfpost paths banned-occurs)
     (setq gff (make-ftree-exp gfwff (list p) (list (make-ftree-imp gfhypf gfconcf))))
     (make-ftree-sel indwff p (make-ftree-imp indhypf indconcf)))))

; returns two values: a positive ftree for wff1, a negative ftree for wff2
(defun make-coclos-setvar-ind-negf-1 (wff1 wff2 paths banned-occurs)
  (if (a-bd-wff-p wff2)
      (let* ((z (bindvar wff2))
	     (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	(multiple-value-bind
	 (pf nf)
	 (make-coclos-setvar-ind-negf-1 wff1 
					(substitute-l-term-var z2 z (cdr wff2))
					paths banned-occurs)
	 (values pf (make-ftree-sel wff2 z2 nf))))
    (let ((leaf (make-ftree-leaf (cdar wff2) t)))
      (multiple-value-bind
       (pf1 nf2)
       (make-coclos-setvar-ind-negf-2 wff1 (cdr wff2) leaf paths banned-occurs)
       (values pf1 (make-ftree-imp leaf nf2))))))

; return pos ftree for wff1 and neg ftree for wff2
(defun make-coclos-setvar-ind-negf-2 (wff1 wff2 leaf paths banned-occurs)
  (if paths
      (let ((aassoc (construct-arg-assoc (ftree-shallow (caar paths)) (ftree-shallow leaf)))
	    (psi (car banned-occurs))
	    (psi0 nil)
	    (psi-z-assoc nil))
	(dolist (w psi)
		(let ((a (assoc w aassoc)))
		  (if a
		      (push a psi-z-assoc)
		    (push w psi0))))
	(setq psi0 (reverse psi0)) ; cebrown 6/4/02
	(if (cdr paths)
	    (progn
	      (unless (and-p wff1)
		(throwfail "Expected " (wff1 . gwff) " to be a conjunction"))
	      (unless (and-p wff2)
		(throwfail "Expected " (wff2 . gwff) " to be a disjunction"))
	      (multiple-value-bind
	       (pfC1 nfD1)
	       (make-coclos-setvar-ind-negf-3
		psi0 (cdar wff1) (cdar wff2) leaf (cdar paths) psi psi-z-assoc)
	       (multiple-value-bind
		(pfC2 nfD2)
		(make-coclos-setvar-ind-negf-2
		 (cdr wff1) (cdr wff2) leaf (cdr paths) (cdr banned-occurs))
		(values (make-ftree-con pfC1 pfC2) (make-ftree-con nfD1 nfD2)))))
	  (make-coclos-setvar-ind-negf-3
	   psi0 wff1 wff2 leaf (cdar paths) psi psi-z-assoc)))
    (throwfail "Problem Creating Induction Principle")))

; returns pos ftree for wff1 and neg ftree for wff2
(defun make-coclos-setvar-ind-negf-3 (psi0 wff1 wff2 leaf gamma psi psi-z-assoc
					 &optional psi-w-assoc)
  (if psi0
      (let* ((w (car psi0))
	     (w2 (fresh-var (unabbreviated-type w) (getnameroot w))))
	(unless (and (a-bd-wff-p wff2)
		     (equal (unabbreviated-type (bindvar wff2))
			    (unabbreviated-type w)))
	  (throwfail "Problem Proving Co-Induction Principle"))
	(multiple-value-bind
	 (pfC1 nfD1)
	 (make-coclos-setvar-ind-negf-3
	  (cdr psi0) wff1 (substitute-l-term-var w2 (bindvar wff2) (cdr wff2))
	  leaf gamma psi psi-z-assoc
	  (acons w w2 psi-w-assoc))
	 (values pfC1 (make-ftree-sel wff2 w2 nfD1))))
    (make-coclos-setvar-ind-negf-4 psi wff1 wff2 leaf
				   gamma psi-z-assoc psi-w-assoc)))

; returns pos ftree wff1, neg ftree wff2
(defun make-coclos-setvar-ind-negf-4 (psi wff1 wff2 leaf gamma psi-z-assoc psi-w-assoc)
  (if psi
      (let* ((w (car psi))
	     (wz (assoc w psi-z-assoc))
	     (ww (assoc w psi-w-assoc))
	     (exptrm nil))
	(if wz
	    (if ww
		(throwfail "Problem Proving Co-Induction Principle - w/z variable conflict")
	      (setq exptrm (cdr wz)))
	  (if ww
	      (setq exptrm (cdr ww))
	    (throwfail "Problem Proving Co-Induction Principle - " (w . gwff))))
	(unless (a-bd-wff-p wff1)
	  (throwfail "Problem Proving Co-Induction Principle"))
	(multiple-value-bind
	 (pfC nfD)
	 (make-coclos-setvar-ind-negf-4 
	  (cdr psi) (substitute-l-term-var exptrm (bindvar wff1) (cdr wff1))
	  wff2 leaf gamma psi-z-assoc psi-w-assoc)
	 (values (make-ftree-exp wff1 (list exptrm) (list pfC)) nfD)))
    (if gamma
	(progn
	  (unless (implies-p wff1)
	    (throwfail "Problem Proving Co-Induction Principle"))
	  (let ((hyp (cdar wff1))
		(conc (cdr wff1)))
	    (make-coclos-setvar-ind-negf-5 (head hyp) conc hyp wff2 leaf
					   (args hyp) (args (ftree-shallow leaf))
					   gamma)))
      (progn
	(unless (not-p wff1)
	  (throwfail "Problem Proving Co-Induction Principle"))
	(make-coclos-setvar-ind-negf-5 (head (cdr wff1)) nil (cdr wff1) wff2 leaf
				       (args (cdr wff1)) (args (ftree-shallow leaf))
				       gamma)))))

(defun make-coclos-setvar-ind-negf-5 (pargs wff0 wff1 wff2 leaf args1 args2 gamma)
  (declare (special clist))
  (if args1
      (if args2
	  (let ((arg1 (car args1))
		(arg2 (car args2)))
	    (if (wffeq arg1 arg2)
		(make-coclos-setvar-ind-negf-5
		 (cons pargs arg1)
		 wff0 wff1 wff2 leaf (cdr args1) (cdr args2)
		 gamma)
	      (progn
		(unless (or (and (not-p wff2)
				 (equals-p (cdr wff2))
				 (wffeq-ab (cdadr wff2) arg2)
				 (wffeq-ab (cddr wff2) arg1))
			    (and (or-p wff2) 
				 (not-p (cdar wff2))
				 (equals-p (cddar wff2))
				 (wffeq-ab (cdar (cddar wff2)) arg2)
				 (wffeq-ab (cdddar wff2) arg1)))
		  (throwfail "Problem Proving Co-Induction Principle"))
		(let ((eqn (if (or-p wff2) (cddar wff2) (cdr wff2))))
		  (multiple-value-bind
		   (leaf2 eqnf new-conns)
		   (make-ftree-subst (apply-wff-n (cons pargs arg1)
						  (reverse (cdr args2)))
				     eqn leaf)
		   (setq clist (append new-conns clist))
		   (multiple-value-bind
		    (pfC nfD)
		    (make-coclos-setvar-ind-negf-5
		     (cons pargs arg1) wff0 wff1
		     (if (or-p wff2) (cdr wff2) nil)
		     leaf2 (cdr args1) (cdr args2) gamma)
		    (values pfC (if nfD
				    (make-ftree-dis (make-ftree-neg eqnf) nfD)
				  (make-ftree-neg eqnf)))))))))
	(throwfail "Problem Proving Co-Induction Principle"))
    (if args2
	(throwfail "Problem Proving Co-Induction Principle")
      (let ((leaf2 (make-ftree-leaf wff1 nil)))
	(unless (wffeq-ab wff1 (ftree-shallow leaf))
	  (throwfail "Problem Proving Co-Induction Principle"))
	(push-onto-clist leaf leaf2)
	(if gamma
	    (progn
	      (unless (and wff0 wff2 (wffeq-ab wff0 wff2))
		(throwfail "Problem Proving Co-Induction Principle"))
	      (let ((pfC (make-ftree-leaf wff0 t))
		    (nfD (make-ftree-leaf wff2 nil)))
		(push-onto-clist pfC nfD)
		(values (make-ftree-imp leaf2 pfC) nfD)))
	  (progn
	    (when (or wff0 wff2)
	      (throwfail "Problem Proving Co-Induction Principle"))
	    (values (make-ftree-neg leaf2) nil)))))))


; returns f-(wff)
(defun make-forall-a-imp-a-negf (wff)
  (if (a-bd-wff-p wff)
      (let* ((z (bindvar wff))
	     (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	(make-ftree-sel wff z2 (make-forall-a-imp-a-negf
				(substitute-l-term-var z2 z (cdr wff)))))
    (if (implies-p wff)
	(if (wffeq-ab (cdar wff) (cdr wff))
	    (let ((f1 (make-ftree-leaf (cdar wff) t))
		  (f2 (make-ftree-leaf (cdr wff) nil)))
	      (push-onto-clist f1 f2)
	      (make-ftree-imp f1 f2))
	  (progn (break) (throwfail "Expected " (wff . gwff) t " to have the form [A implies A]")))
      (progn (break) (throwfail "Expected " (wff . gwff) t " to have the form [A implies A]")))))

(defun make-subreln-wff (u v)
  (let ((bl nil)
	(wff1 u)
	(wff2 v))
    (do ((y (unabbreviated-type u) (car y))
	 (z (unabbreviated-type v) (car z)))
	((or (atom y) (atom z))
	 (unless (and (eq y 'O) (eq z 'O))
	   (throwfail "make-subreln-wff was called without relns of the same type"))
	 (bind-var-wff-n 'forall bl
			 (acons 'IMPLIES wff1 wff2)))
	(unless (equal (cdr y) (cdr z))
	  (throwfail "make-subreln-wff was called without relns of the same type"))
	(let ((b (fresh-var-1 (cdr y))))
	  (push b bl)
	  (setq wff1 (cons wff1 b))
	  (setq wff2 (cons wff2 b))))))

(defun make-max-setvar-lemma-negf (lemma vsel misc-occurs inv-princ paths banned-occurs)
  (if misc-occurs
      (let* ((a (bindvar lemma))
	     (b (fresh-var (unabbreviated-type a) (getnameroot a))))
	(make-ftree-sel
	 lemma b
	 (make-max-setvar-lemma-negf (substitute-l-term-var b a (cdr lemma))
				     vsel (cdr misc-occurs)
				     (substitute-l-term-var b (car misc-occurs)
							    inv-princ)
				     paths banned-occurs)))
    (let* ((soln (max-setvar-soln-from-inv-princ inv-princ))
	   (i (substitute-l-term-var soln (bindvar lemma) (cdr lemma)))
	   (l (lnorm i)))
      (if (wffeq i l)
	  (make-ftree-exp lemma (list soln) (list (make-max-setvar-lemma-negf-1
						   l paths banned-occurs)))
	(make-ftree-exp 
	 lemma (list soln)
	 (list (make-ftree-rew
		i 'LAMBDA
		(make-max-setvar-lemma-negf-1 l paths banned-occurs))))))))

(defun max-setvar-soln-from-inv-princ (inv-princ)
  (if (a-bd-wff-p inv-princ)
      (acons (bindvar inv-princ) 'LAMBDA
	     (max-setvar-soln-from-inv-princ (cdr inv-princ)))
    (if (implies-p inv-princ)
	(cdar inv-princ)
      (throwfail "Problem with Inversion Principle solving Maximal Constraint"))))

(defun make-max-setvar-lemma-negf-1 (wff paths banned-occurs)
  (make-ftree-con
   (make-max-setvar-lemma-negf-3 (cdar wff) paths banned-occurs)
   (if INCLUDE-COINDUCTION-PRINCIPLE
       (make-ftree-con
	(make-max-setvar-lemma-negf-2 (cdadr wff))
	(make-max-setvar-ind-negf (cddr wff) paths banned-occurs))
     (make-max-setvar-lemma-negf-2 (cdr wff)))))

(defun make-max-setvar-lemma-negf-2 (wff)
  (if (a-bd-wff-p wff)
      (let* ((z (bindvar wff))
	     (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	(make-ftree-sel wff z2
			(make-max-setvar-lemma-negf-2
			 (substitute-l-term-var z2 z (cdr wff)))))
    (if (and (implies-p wff)
	     (wffeq-ab (cdar wff) (cdr wff)))
	(let ((l (make-ftree-leaf (cdar wff) t))
	      (r (make-ftree-leaf (cdr wff) nil)))
	  (push-onto-clist l r)
	  (make-ftree-imp l r))
      (throwfail "Problem Generating Max Solution"))))

(defun make-max-setvar-lemma-negf-3 (wff paths banned-occurs &optional (n 0))
  (if paths
      (if (cdr paths)
	  (progn
	    (unless (and-p wff)
	      (throwfail "Expected " (wff . gwff) " to be a conjunction"))
	    (make-ftree-con
	     (make-max-setvar-lemma-negf-4 (car banned-occurs) (cdar wff)
					   (args (ftree-shallow (caar paths)))
					   (cdar paths) n)
	     (make-max-setvar-lemma-negf-3 (cdr wff) (cdr paths) (cdr banned-occurs) (1+ n))))
	(make-max-setvar-lemma-negf-4 (car banned-occurs) wff
				      (args (ftree-shallow (caar paths)))
				      (cdar paths) n))))

(defun make-max-setvar-lemma-negf-4 (psi wff args gamma n &optional (k 0) bl)
  (if psi
      (progn
	(unless (a-bd-wff-p wff)
	  (throwfail "Expected Univ: " (wff . gwff)))
	(let* ((z (bindvar wff))
	       (z2 (fresh-var (unabbreviated-type z) (getnameroot z))))
	  (if (member (car psi) args)
	      (incf k)
	    (push z2 bl))
	  (make-ftree-sel wff z2
			  (make-max-setvar-lemma-negf-4
			   (cdr psi)
			   (substitute-l-term-var z2 z (cdr wff))
			   args gamma n k bl))))
    (if (implies-p wff)
	(let ((l (make-ftree-leaf (cdr wff) nil)))
	  (make-ftree-imp 
	   (make-max-setvar-lemma-negf-5 n (reverse bl) (- (length args) k) (cdar wff) gamma l)
	   l))
      (if (not-p wff)
	  (make-ftree-neg (make-max-setvar-lemma-negf-5 n (reverse bl) (- (length args) k)
							(cdr wff) gamma nil))
	(throwfail "Problem proving Max Soln")))))

(defun make-max-setvar-lemma-negf-5 (n bl m wff gamma l)
  (if (> n 0)
      (if (and-p wff)
	  (make-ftree-con (make-ftree-leaf (cdar wff) t)
			  (make-max-setvar-lemma-negf-5 (- n 1) bl m (cdr wff) gamma l))
	(throwfail "Problem Constructing Maximal Solution"))
    (if (and-p wff)
	(make-ftree-con
	 (make-max-setvar-lemma-negf-6 bl m (cdar wff) gamma l)
	 (make-ftree-leaf (cdr wff) t))
      (make-max-setvar-lemma-negf-6 bl m wff gamma l))))

(defun make-max-setvar-lemma-negf-6 (bl m wff gamma l)
  (if bl
      (make-ftree-exp wff (list (car bl))
		      (list
		       (make-max-setvar-lemma-negf-6
			(cdr bl) m (substitute-l-term-var (car bl) (bindvar wff) (cdr wff))
			gamma l)))
    (make-max-setvar-lemma-negf-7 m wff gamma l)))

(defun make-max-setvar-lemma-negf-7 (m wff gamma l)
  (if (> m 0)
      (if (or gamma (> m 1))
	  (progn
	    (unless (and (or-p wff) (not-p (cdar wff)))
	      (throwfail "Expected " (wff . gwff) t " to be of the form [~[A=A] OR B]"))
	    (let ((eqn (cddar wff)))
	      (unless (and (equals-p eqn) (wffeq-ab (cdar eqn) (cdr eqn)))
		(throwfail "Expected " (wff . gwff) t " to be of the form [~[A=A] OR B]"))
	      (make-ftree-dis (make-ftree-neg (make-ftree-rew eqn 'REFL=
							      (make-ftree-true nil)))
			      (make-max-setvar-lemma-negf-7 (- m 1) (cdr wff) gamma l))))
	(progn
	  (unless (and (not-p wff) (equals-p (cdr wff))
		       (wffeq-ab (cdadr wff) (cddr wff)))
	    (throwfail "Expected " (wff . gwff) t " to be of the form ~[A=A]"))
	  (make-ftree-neg (make-ftree-rew (cdr wff) 'REFL= (make-ftree-true nil)))))
    (if l
	(if (wffeq-ab (ftree-shallow l) wff)
	    (let ((r (make-ftree-leaf wff t)))
	      (push-onto-clist l r)
	      r)
	  (throwfail "Expected " (wff . gwff) t " to equal " ((ftree-shallow l) . gwff)))
      (throwfail "Problem proving Max Soln"))))

(defun make-max-setvar-ind-negf (wff paths banned-occurs)
  (let* ((p (bindvar wff))
	 (p2 (fresh-var (unabbreviated-type p) (getnameroot p)))
	 (body (substitute-l-term-var p2 p (cdr wff))))
    (multiple-value-bind
     (hypf concf)
     (make-coclos-setvar-ind-negf-1 (cdar body) (cdr body) paths banned-occurs)
     (make-ftree-sel wff p2 (make-ftree-imp hypf concf)))))

(defun make-max-setvar-lemma-posf (inv-princ vsel paths banned-occurs misc-occurs
					     substitutable-vars)
  (if misc-occurs
      (let* ((a (car misc-occurs))
	     (b (fresh-var (unabbreviated-type a) (getnameroot a)))
	     (f (make-max-setvar-lemma-posf
		 inv-princ vsel paths banned-occurs (cdr misc-occurs)
		 substitutable-vars))
	     (expf (make-ftree-exp
		    (acons b 'forall (substitute-l-term-var b a (ftree-shallow f)))
		    (list a)
		    (list f))))
	(when (member a substitutable-vars)
	  (push (cons a a) (ftree-exp-subs expf)))
	expf)
    (let ((f (make-max-setvar-lemma-posf-1 inv-princ vsel paths banned-occurs))
	  (v2 (fresh-var (unabbreviated-type vsel) (getnameroot vsel))))
      (make-ftree-sel (acons v2 'exists 
			     (substitute-l-term-var v2 vsel (ftree-shallow f)))
		      vsel f))))

(defun make-max-setvar-lemma-posf-1 (inv-princ vsel paths banned-occurs)
  (declare (special NUM-OF-DUPS))
  (let* ((f1 (make-max-setvar-lemma-posf-2 paths banned-occurs vsel))
	 (f2 (if (consp (unabbreviated-type vsel))
		 (gwff-to-ftree-dup inv-princ t NUM-OF-DUPS)
	       (gwff-to-ftree inv-princ t))))
    (if include-coinduction-principle
	(let* ((p (fresh-var (unabbreviated-type vsel) '\p))
	       (indwff
		(acons p 'FORALL
		       (acons 'IMPLIES
			      (substitute-l-term-var p vsel (ftree-shallow f1))
			      (make-subreln-wff p vsel)))))
	  (make-ftree-con f1 (make-ftree-con f2 (gwff-to-ftree-dup indwff t NUM-OF-DUPS))))
      (make-ftree-con f1 f2))))

(defun make-max-setvar-lemma-posf-2 (paths banned-occurs vsel)
  (if paths
      (if (cdr paths)
	  (make-ftree-con (make-max-setvar-lemma-posf-3
			   (car paths) (car banned-occurs) vsel)
			  (make-max-setvar-lemma-posf-2
			   (cdr paths) (cdr banned-occurs) vsel))
	(make-max-setvar-lemma-posf-3 (car paths) (car banned-occurs) vsel))
    (throwfail "Problem constructing Max Solution")))

(defun make-max-setvar-lemma-posf-3 (path banned-occs vsel)
  (declare (special NUM-OF-DUPS))
  (if banned-occs
      ; duplicate
      (let* ((kidsl nil)
	     (expvl nil)
	     (w (car banned-occs))
	     (w3 (fresh-var (unabbreviated-type w) (getnameroot w)))
	     (f (make-max-setvar-lemma-posf-4 path (cdr banned-occs) vsel)))
	(do ((w2 (fresh-var (unabbreviated-type w) (getnameroot w))
		 (fresh-var (unabbreviated-type w) (getnameroot w)))
	     (i 0 (1+ i)))
	    ((> i num-of-dups)
	     (push w2 expvl)
	     (push (gwff-to-ftree (substitute-l-term-var w2 w (ftree-shallow f)) t) kidsl)))
	(let ((exp (make-ftree-exp (acons w3 'forall
					  (substitute-l-term-var w3 w (ftree-shallow f)))
				   (cons w expvl)
				   (cons f kidsl))))
	  (setf (ftree-exp-subs exp)
		(append (mapcar #'(lambda (x)
				    (cons x x))
				expvl)
			(ftree-exp-subs exp)))
	  exp))
    (let ((l (make-ftree-leaf (ftree-shallow (car path))
			      (not (ftree-positive (car path))))))
      (push-onto-clist l (car path))
      (if (cdr path)
	  (make-ftree-imp l (make-max-setvar-lemma-posf-5 (cdr path) vsel))
	(make-ftree-neg l)))))

(defun make-max-setvar-lemma-posf-4 (path banned-occs vsel)
  (if banned-occs
      (let* ((f (make-max-setvar-lemma-posf-4 path (cdr banned-occs) vsel))
	     (w (car banned-occs))
	     (w3 (fresh-var (unabbreviated-type w) (getnameroot w))))
	(make-ftree-exp 
	 (acons w3 'forall (substitute-l-term-var w3 w (ftree-shallow f)))
	 (list (car banned-occs))
	 (list f)))
    (let ((l (make-ftree-leaf (ftree-shallow (car path)) nil)))
      (push-onto-clist l (car path))
      (if (cdr path)
	  (make-ftree-imp l (make-max-setvar-lemma-posf-5 (cdr path) vsel))
	(make-ftree-neg l)))))

(defun make-max-setvar-lemma-posf-5 (lits vsel)
  (if lits
      (if (cdr lits)
	  (make-ftree-dis
	   (make-max-setvar-lemma-posf-6 (car lits) vsel)
	   (make-max-setvar-lemma-posf-5 (cdr lits) vsel))
	(make-max-setvar-lemma-posf-6 (car lits) vsel))
    (throwfail "Problem constructing Max Solution")))

(defun make-max-setvar-lemma-posf-6 (lit vsel)
  (let* ((l (make-ftree-leaf (ftree-shallow lit) (not (ftree-positive lit))))
	 (f (if (ftree-positive lit) (make-ftree-neg l) l)))
    (push-onto-clist l lit)
    (if (find-if #'(lambda (arg)
		     (free-in vsel arg))
		 (args (ftree-shallow lit)))
	(let* ((w (fresh-var (unabbreviated-type vsel) '\w))
	       (zl nil))
	  (do ((y (unabbreviated-type w) (car y)))
	      ((atom y))
	      (push (fresh-var (cdr y) '\z) zl))
	  (setq zl (reverse zl))
	  (let* ((wff2 (bind-var-wff-n 'forall zl
				       (acons 'IMPLIES
					      (apply-wff-n vsel zl)
					      (apply-wff-n w zl))))
		 (wff3 (bind-var-wff-n 'forall zl
				       (acons 'IMPLIES
					      (apply-wff-n vsel zl)
					      (apply-wff-n vsel zl))))
		 (wff (acons w 'forall (acons 'IMPLIES wff2
					      (substitute-l-term-var
					       w vsel (ftree-shallow f))))))
	    (make-ftree-exp
	     wff (list vsel) (list (make-ftree-imp
				    (make-forall-a-imp-a-negf wff3)
				    f)))))
      f)))

; tp should be the type of a relation.
(defun make-knaster-tarski-lemma (tp)
  (let* ((monop (fresh-var (cons tp tp) '\K))
	 (u (fresh-var tp '\u))
	 (v (fresh-var tp '\v))
	 (monopuzl (cons monop u))
	 (monopvzl (cons monop v))
	 (uzl u)
	 (vzl v)
	 (ktclist nil)
	 (special-nodes nil)
	 (special-svars nil)
	 zl)
    (declare (special ktclist special-nodes special-svars))
    (do ((tp2 tp (car tp2)))
	((not (consp tp2)))
	(let ((z (fresh-var (cdr tp2) '\z)))
	  (push z zl)
	  (setq uzl (cons uzl z))
	  (setq vzl (cons vzl z))
	  (setq monopuzl (cons monopuzl z))
	  (setq monopvzl (cons monopvzl z))))
    (let* ((thm (acons monop 'forall
		       (acons 'IMPLIES
			      (acons u 'forall
				     (acons v 'forall
					    (acons 'IMPLIES
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES uzl vzl))
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES monopuzl
									  monopvzl)))))
			      (acons u 'exists
				     (acons 'AND
					    (bind-var-wff-n 'forall zl
							    (acons 'IMPLIES monopuzl uzl))
					    (bind-var-wff-n 'forall zl
							    (acons 'IMPLIES uzl monopuzl)))))))
	   (ktnegf (make-knaster-tarski-negf thm zl)))
      (values ktnegf ktclist))))
  
(defun make-knaster-tarski-negf (thm zl)
  (let ((mopsel (fresh-var (unabbreviated-type (bindvar thm)) (getnameroot (bindvar thm)))))
    (make-ftree-sel thm mopsel
		    (make-knaster-tarski-negf-1
		     mopsel (substitute-l-term-var mopsel (bindvar thm) (cdr thm)) zl))))

(defun make-knaster-tarski-negf-1 (mop thm zl)
  (let* ((monhyp (cdar thm))
	 (lfpconc (cdr thm))
	 (u (bindvar lfpconc))
	 (w (fresh-var (unabbreviated-type u) '\w))
	 (soln (bind-var-wff-n 'lambda zl
			       (acons w 'forall
				      (acons 'IMPLIES
					     (bind-var-wff-n 'forall zl
							     (acons 'IMPLIES
								    (apply-wff-n
								     (cons mop w) zl)
								    (apply-wff-n w zl)))
					     (apply-wff-n w zl)))))
	 (lfpconc2 (substitute-l-term-var soln (bindvar lfpconc) (cdr lfpconc)))
	 (fconc (make-ftree-exp
		 lfpconc
		 (list soln)
		 (list (make-ftree-rew 
			lfpconc2 'LAMBDA
			(make-knaster-tarski-negf-2
			 soln (lnorm lfpconc2) mop (reverse zl))))))
	 (fmon (make-knaster-tarski-leastfp-negf-11 mop soln monhyp zl)))
    (make-ftree-imp fmon fconc)))

; conj70		
(defun make-knaster-tarski-negf-2 (soln wff mop zl)
  (make-ftree-con
   (make-knaster-tarski-leastfp-negf-3 soln (cdar wff) mop zl)
   (make-knaster-tarski-leastfp-negf-6 soln (cdr wff) mop zl)))

; tp should be the type of a relation.
(defun make-knaster-tarski-leastfp-lemma (tp)
  (let* ((monop (fresh-var (cons tp tp) '\K))
	 (u (fresh-var tp '\u))
	 (v (fresh-var tp '\v))
	 (monopuzl (cons monop u))
	 (monopvzl (cons monop v))
	 (uzl u)
	 (vzl v)
	 (ktclist nil)
	 (special-nodes nil)
	 (special-svars nil)
	 zl)
    (declare (special ktclist special-nodes special-svars))
    (do ((tp2 tp (car tp2)))
	((not (consp tp2)))
	(let ((z (fresh-var (cdr tp2) '\z)))
	  (push z zl)
	  (setq uzl (cons uzl z))
	  (setq vzl (cons vzl z))
	  (setq monopuzl (cons monopuzl z))
	  (setq monopvzl (cons monopvzl z))))
    (let* ((thm (acons monop 'forall
		       (acons 'IMPLIES
			      (acons u 'forall
				     (acons v 'forall
					    (acons 'IMPLIES
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES uzl vzl))
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES monopuzl
									  monopvzl)))))
			      (acons u 'exists
				     (acons 'AND
					    (acons 'AND
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES monopuzl uzl))
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES uzl monopuzl)))
					    (acons v 'forall
						   (acons 'IMPLIES
							  (bind-var-wff-n 'forall zl
									  (acons 'IMPLIES monopvzl vzl))
							  (bind-var-wff-n 'forall zl
									  (acons 'IMPLIES uzl vzl)))))))))
	   (ktnegf (make-knaster-tarski-leastfp-negf thm zl)))
      (values ktnegf ktclist))))
  
(defun make-knaster-tarski-leastfp-negf (thm zl)
  (let ((mopsel (fresh-var (unabbreviated-type (bindvar thm)) (getnameroot (bindvar thm)))))
    (make-ftree-sel thm mopsel
		    (make-knaster-tarski-leastfp-negf-1
		     mopsel (substitute-l-term-var mopsel (bindvar thm) (cdr thm)) zl))))

(defun make-knaster-tarski-leastfp-negf-1 (mop thm zl)
  (let* ((monhyp (cdar thm))
	 (lfpconc (cdr thm))
	 (u (bindvar lfpconc))
	 (w (fresh-var (unabbreviated-type u) '\w))
	 (soln (bind-var-wff-n 'lambda zl
			       (acons w 'forall
				      (acons 'IMPLIES
					     (bind-var-wff-n 'forall zl
							     (acons 'IMPLIES
								    (apply-wff-n
								     (cons mop w) zl)
								    (apply-wff-n w zl)))
					     (apply-wff-n w zl)))))
	 (lfpconc2 (substitute-l-term-var soln (bindvar lfpconc) (cdr lfpconc)))
	 (fconc (make-ftree-exp
		 lfpconc
		 (list soln)
		 (list (make-ftree-rew 
			lfpconc2 'LAMBDA
			(make-knaster-tarski-leastfp-negf-2
			 soln (lnorm lfpconc2) mop zl)))))
	 (fmon (make-knaster-tarski-leastfp-negf-11 mop soln monhyp (reverse zl))))
    (make-ftree-imp fmon fconc)))

; conj70		
(defun make-knaster-tarski-leastfp-negf-2 (soln wff mop zl)
  (make-ftree-con
   (make-ftree-con
    (make-knaster-tarski-leastfp-negf-3 soln (cdar (cdar wff)) mop zl)
    (make-knaster-tarski-leastfp-negf-6 soln (cdr (cdar wff)) mop zl))
   (make-knaster-tarski-leastfp-negf-8 (cdr wff) zl)))

; SEL5
(defun make-knaster-tarski-leastfp-negf-3 (soln wff mop zl &optional zl2)
  (declare (special special-nodes special-svars))
  (if zl
      (let ((z2 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z2
			(make-knaster-tarski-leastfp-negf-3
			 soln (substitute-l-term-var z2 (bindvar wff) (cdr wff)) mop (cdr zl)
			 (cons z2 zl2))))
    (let* ((l (make-ftree-leaf (cdar wff) t))
	   (c (cdr wff))
	   (w (bindvar c))
	   (w2 (fresh-var (unabbreviated-type w) (getnameroot w)))
	   (f2 (make-ftree-sel c w2
			       (make-knaster-tarski-leastfp-negf-4
				mop (substitute-l-term-var w2 w (cdr c))
				zl2))))
      (push (cons 'w2 w2) special-svars)
      (push (cons 'zl2 zl2) special-svars)
      (push (cons 'Klfpz+ l) special-nodes)
      (make-ftree-imp l f2))))

; IMP28
(defun make-knaster-tarski-leastfp-negf-4 (mop wff zl)
  (let ((l (make-ftree-leaf (cdr wff) nil)))
    (make-ftree-imp
     (make-knaster-tarski-leastfp-negf-5 l mop (cdar wff) (reverse zl))
     l)))

(defun make-knaster-tarski-leastfp-negf-5 (l mop wff zl)
  (declare (special ktclist special-nodes))
  (if zl
      (let ((f (make-ftree-exp wff
			       (list (car zl))
			       (list (make-knaster-tarski-leastfp-negf-5
				      l mop (substitute-l-term-var (car zl) (bindvar wff) (cdr wff))
				      (cdr zl))))))
	(push (cons 'Kwsubw f) special-nodes)
	f)
    (let ((hyp (make-ftree-leaf (cdar wff) nil))
	  (conc (make-ftree-leaf (cdr wff) t)))
      (push (cons (ftree-name conc) (ftree-name l)) ktclist) 
      (push (cons 'Kwz- hyp) special-nodes)
      (make-ftree-imp hyp conc))))

; SEL9      
(defun make-knaster-tarski-leastfp-negf-6 (soln wff mop zl &optional zl3)
  (declare (special special-svars special-nodes))
  (if zl
      (let ((z3 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z3
			(make-knaster-tarski-leastfp-negf-6
			 soln (substitute-l-term-var z3 (bindvar wff) (cdr wff))
			 mop (cdr zl) (cons z3 zl3))))
    (let* ((l (make-ftree-leaf (cdr wff) nil))
	   (f (make-knaster-tarski-leastfp-negf-7 l soln (cdar wff) mop)))
      (make-ftree-imp f l))))

; exp13
(defun make-knaster-tarski-leastfp-negf-7 (l soln wff mop)
  (declare (special special-nodes ktclist)) 
  (let* ((moplfp (cons mop soln))
	 (wff2 (substitute-l-term-var moplfp (bindvar wff) (cdr wff))) ; doesn't lead to beta-reducts
	 (l1 (make-ftree-leaf (cdar wff2) nil)) ; this is a complex wff, but we don't need to deepen
	 (l2 (make-ftree-leaf (cdr wff2) t)))
    (push (cons 'KKlfpKlfp- l1) special-nodes)
    (push (cons (ftree-name l) (ftree-name l2)) ktclist) 
    (make-ftree-exp
     wff (list moplfp)
     (list (make-ftree-imp l1 l2)))))

; sel11
(defun make-knaster-tarski-leastfp-negf-8 (wff zl)
  (declare (special special-svars))
  (let ((v2 (fresh-var (unabbreviated-type (bindvar wff)) '\v)))
    (make-ftree-sel wff v2
		    (make-knaster-tarski-leastfp-negf-9
		     (substitute-l-term-var v2 (bindvar wff) (cdr wff))
		     v2 zl))))

; imp91
(defun make-knaster-tarski-leastfp-negf-9 (wff v2 zl)
  (declare (special ktclist special-nodes))
  (let* ((l1 (make-ftree-leaf (cdar wff) t)) ; don't need to deepen
	 (f (make-knaster-tarski-leastfp-negf-10 l1 (cdr wff) v2 zl))
	 (l40 (cdr (assoc 'Kvsubv- special-nodes))))
    (push (cons (ftree-name l1) (ftree-name l40)) ktclist) 
    (make-ftree-imp l1 f)))

; SEL10
(defun make-knaster-tarski-leastfp-negf-10 (l1 wff v2 zl)
  (declare (special ktclist special-nodes))
  (if zl
      (let ((z4 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z4
			(make-knaster-tarski-leastfp-negf-10
			 l1 (substitute-l-term-var z4 (bindvar wff) (cdr wff))
			 v2 (cdr zl))))
    (let* ((l3 (make-ftree-leaf (cdr wff) nil))
	   (hyp (substitute-l-term-var v2 (bindvar (cdar wff)) (cddar wff)))
	   (l4 (make-ftree-leaf (cdar hyp) nil))
	   (l5 (make-ftree-leaf (cdr hyp) t)))
      (push (cons (ftree-name l3) (ftree-name l5)) ktclist) 
      (push (cons 'Kvsubv- l4) special-nodes)
      (make-ftree-imp
       (make-ftree-exp (cdar wff) (list v2)
		       (list (make-ftree-imp l4 l5))) l3))))

; EXP19     
(defun make-knaster-tarski-leastfp-negf-11 (mop soln monhyp zl)
  (let* ((f1 (make-knaster-tarski-leastfp-negf-12
	      soln (substitute-l-term-var (cons mop soln) (bindvar monhyp) (cdr monhyp)) zl))
	 (wff2 (substitute-l-term-var soln (bindvar monhyp) (cdr monhyp)))
	 (wff3 (lnorm wff2))
	 (f2 (make-knaster-tarski-leastfp-negf-16 wff3 zl)))
    (make-ftree-exp monhyp (list (cons mop soln) soln)
		    (list f1
			  (if (wffeq wff2 wff3)
			      f2
			    (make-ftree-rew wff2 'lambda f2))))))

; imp67
(defun make-knaster-tarski-leastfp-negf-12 (soln wff zl)
  (declare (special special-nodes ktclist)) 
  (let* ((wff2 (substitute-l-term-var soln (bindvar wff) (cdr wff)))
	 (wff3 (lnorm wff2))
	 (l (make-ftree-leaf (cdr wff3) t)) ; don't need to deepen
	 (l35 (cdr (assoc 'KKlfpKlfp- special-nodes))))
    (push (cons (ftree-name l) (ftree-name l35)) ktclist) 
    (make-ftree-exp wff (list soln)
		    (list (make-ftree-rew wff2 'LAMBDA
					  (make-ftree-imp
					   (make-knaster-tarski-leastfp-negf-13
					    (cdar wff3) zl)
					   l))))))

; SEL8
(defun make-knaster-tarski-leastfp-negf-13 (wff zl &optional zl5)
  (declare (special special-nodes special-svars))
  (if zl
      (let ((z5 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z5
			(make-knaster-tarski-leastfp-negf-13
			 (substitute-l-term-var z5 (bindvar wff) (cdr wff))
			 (cdr zl) (cons z5 zl5))))
    (let ((l (make-ftree-leaf (cdar wff) t))
	  (w3 (fresh-var (unabbreviated-type (bindvar (cdr wff))) '\w)))
      (push (cons 'zl5 zl5) special-svars)
      (push (cons 'w3 w3) special-svars)
      (push (cons 'Klfpsubw+ l) special-nodes)
      (make-ftree-imp l (make-ftree-sel (cdr wff) w3
					(make-knaster-tarski-leastfp-negf-14
					 (substitute-l-term-var w3 (bindvar (cdr wff))
								(cddr wff))
					 zl5))))))

; IMP59
(defun make-knaster-tarski-leastfp-negf-14 (wff zl5)
  (declare (special special-nodes))
  (let* ((l5 (make-ftree-leaf (cdr wff) nil))
	 (e (make-knaster-tarski-leastfp-negf-15 l5 (cdar wff) (reverse zl5))))
    (push (cons 'Kwsubw2 e) special-nodes)
    (make-ftree-imp e l5)))

; exp10
(defun make-knaster-tarski-leastfp-negf-15 (l5 wff zl5)
  (declare (special ktclist special-nodes))
  (if zl5
      (make-ftree-exp wff (list (car zl5))
		      (list (make-knaster-tarski-leastfp-negf-15
			     l5
			     (substitute-l-term-var (car zl5) (bindvar wff) (cdr wff))
			     (cdr zl5))))
    (let ((l1 (make-ftree-leaf (cdar wff) nil))
	  (l2 (make-ftree-leaf (cdr wff) t)))
      (push (cons 'Kw- l1) special-nodes)
      (push (cons (ftree-name l2) (ftree-name l5)) ktclist) 
      (make-ftree-imp l1 l2))))

; EXP18
(defun make-knaster-tarski-leastfp-negf-16 (wff zl)
  (declare (special special-svars))
  (let ((w2 (cdr (assoc 'w2 special-svars)))
	(w3 (cdr (assoc 'w3 special-svars))))
    (make-ftree-exp wff
		    (list w3 w2)
		    (list (make-knaster-tarski-leastfp-negf-17
			   (substitute-l-term-var w3 (bindvar wff) (cdr wff)) zl)
			  (make-knaster-tarski-leastfp-negf-20
			   (substitute-l-term-var w2 (bindvar wff) (cdr wff)) zl)))))

; imp48
(defun make-knaster-tarski-leastfp-negf-17 (wff zl)
  (declare (special special-svars))
  (make-ftree-imp (make-knaster-tarski-leastfp-negf-18 (cdar wff) zl)
		  (make-knaster-tarski-leastfp-negf-19 
		   (cdr wff) (reverse (cdr (assoc 'zl5 special-svars))))))

; sel6
(defun make-knaster-tarski-leastfp-negf-18 (wff zl)
  (declare (special special-svars special-nodes ktclist)) 
  (if zl
      (let ((z6 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z6
			(make-knaster-tarski-leastfp-negf-18
			 (substitute-l-term-var z6 (bindvar wff) (cdr wff))
			 (cdr zl))))
    (let* ((l28 (make-ftree-leaf (cdr wff) nil))
	   (w3 (cdr (assoc 'w3 special-svars)))
	   (hyp (substitute-l-term-var w3 (bindvar (cdar wff)) (cddar wff)))
	   (l26 (make-ftree-leaf (cdar hyp) nil))
	   (l27 (make-ftree-leaf (cdr hyp) t))
	   (Kwsubw2 (cdr (assoc 'Kwsubw2 special-nodes))))
      (push (cons (ftree-name l26) (ftree-name Kwsubw2)) ktclist) 
      (push (cons (ftree-name l27) (ftree-name l28)) ktclist) 
      (make-ftree-imp (make-ftree-exp (cdar wff) (list w3)
				      (list (make-ftree-imp l26 l27)))
		      l28))))

; exp7
(defun make-knaster-tarski-leastfp-negf-19 (wff zl5)
  (declare (special special-nodes ktclist)) 
  (if zl5
      (make-ftree-exp wff (list (car zl5))
		      (list
		       (make-knaster-tarski-leastfp-negf-19
			(substitute-l-term-var (car zl5) (bindvar wff) (cdr wff))
			(cdr zl5))))
    (let ((l30 (make-ftree-leaf (cdar wff) nil))
	  (l31 (make-ftree-leaf (cdr wff) t))
	  (l29 (cdr (assoc 'Klfpsubw+ special-nodes)))
	  (l32 (cdr (assoc 'Kw- special-nodes))))
      (push (cons (ftree-name l29) (ftree-name l30)) ktclist) 
      (push (cons (ftree-name l31) (ftree-name l32)) ktclist) 
      (make-ftree-imp l30 l31))))

; imp17
(defun make-knaster-tarski-leastfp-negf-20 (wff zl)
  (declare (special special-svars))
  (make-ftree-imp (make-knaster-tarski-leastfp-negf-21 (cdar wff) zl)
		  (make-knaster-tarski-leastfp-negf-22
		   (cdr wff) (reverse (cdr (assoc 'zl2 special-svars))))))

; sel3
(defun make-knaster-tarski-leastfp-negf-21 (wff zl)
  (declare (special special-svars special-nodes ktclist)) 
  (if zl
      (let ((z7 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel
	 wff z7
	 (make-knaster-tarski-leastfp-negf-21
	  (substitute-l-term-var z7 (bindvar wff) (cdr wff))
	  (cdr zl))))
    (let* ((l18 (make-ftree-leaf (cdr wff) nil))
	   (w2 (cdr (assoc 'w2 special-svars)))
	   (hyp (substitute-l-term-var w2 (bindvar (cdar wff)) (cddar wff)))
	   (l16 (make-ftree-leaf (cdar hyp) nil))
	   (l17 (make-ftree-leaf (cdr hyp) t))
	   (e (cdr (assoc 'Kwsubw special-nodes))))
      (push (cons (ftree-name e) (ftree-name l16)) ktclist) 
      (push (cons (ftree-name l17) (ftree-name l18)) ktclist) 
      (make-ftree-imp
       (make-ftree-exp (cdar wff) (list w2) (list (make-ftree-imp l16 l17)))
       l18))))

(defun make-knaster-tarski-leastfp-negf-22 (wff zl2)
  (declare (special special-nodes ktclist)) 
  (if zl2
      (make-ftree-exp wff (list (car zl2))
		      (list (make-knaster-tarski-leastfp-negf-22
			     (substitute-l-term-var (car zl2) (bindvar wff) (cdr wff))
			     (cdr zl2))))
    (let ((l20 (make-ftree-leaf (cdar wff) nil))
	  (l21 (make-ftree-leaf (cdr wff) t))
	  (l19 (cdr (assoc 'Klfpz+ special-nodes)))
	  (l22 (cdr (assoc 'Kwz- special-nodes))))
      (push (cons (ftree-name l19) (ftree-name l20)) ktclist) 
      (push (cons (ftree-name l21) (ftree-name l22)) ktclist) 
      (make-ftree-imp l20 l21))))

(defun make-mon-setfn-from-rside (wff)
  (if (a-bd-wff-p wff)
      (acons (bindvar wff) 'lambda
	     (make-mon-setfn-from-rside (cdr wff)))
    (if (implies-p wff) (cdr wff) wff)))

(defun make-mon-setfn-from-lside (wff)
  (if (a-bd-wff-p wff)
      (acons (bindvar wff) 'lambda
	     (make-mon-setfn-from-lside (cdr wff)))
    (if (implies-p wff) (cdar wff) wff)))

; tp should be the type of a relation.
(defun make-knaster-tarski-gfp-lemma (tp)
  (let* ((monop (fresh-var (cons tp tp) '\K))
	 (u (fresh-var tp '\u))
	 (v (fresh-var tp '\v))
	 (monopuzl (cons monop u))
	 (monopvzl (cons monop v))
	 (uzl u)
	 (vzl v)
	 (ktclist nil)
	 (special-nodes nil)
	 (special-svars nil)
	 zl)
    (declare (special ktclist special-nodes special-svars))
    (do ((tp2 tp (car tp2)))
	((not (consp tp2)))
	(let ((z (fresh-var (cdr tp2) '\z)))
	  (push z zl)
	  (setq uzl (cons uzl z))
	  (setq vzl (cons vzl z))
	  (setq monopuzl (cons monopuzl z))
	  (setq monopvzl (cons monopvzl z))))
    (let* ((thm (acons monop 'forall
		       (acons 'IMPLIES
			      (acons u 'forall
				     (acons v 'forall
					    (acons 'IMPLIES
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES uzl vzl))
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES monopuzl
									  monopvzl)))))
			      (acons u 'exists
				     (acons 'AND
					    (acons 'AND
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES monopuzl uzl))
						   (bind-var-wff-n 'forall zl
								   (acons 'IMPLIES uzl monopuzl)))
					    (acons v 'forall
						   (acons 'IMPLIES
							  (bind-var-wff-n 'forall zl
									  (acons 'IMPLIES vzl monopvzl))
							  (bind-var-wff-n 'forall zl
									  (acons 'IMPLIES vzl uzl)))))))))
	   (ktnegf (make-knaster-tarski-gfp-negf thm zl)))
      (values ktnegf ktclist)))) 
  
(defun make-knaster-tarski-gfp-negf (thm zl)
  (let ((mopsel (fresh-var (unabbreviated-type (bindvar thm)) (getnameroot (bindvar thm)))))
    (make-ftree-sel thm mopsel
		    (make-knaster-tarski-gfp-negf-1
		     mopsel (substitute-l-term-var mopsel (bindvar thm) (cdr thm)) zl))))

(defun make-knaster-tarski-gfp-negf-1 (mop thm zl)
  (let* ((monhyp (cdar thm))
	 (gfpconc (cdr thm))
	 (u (bindvar gfpconc))
	 (w (fresh-var (unabbreviated-type u) '\w))
	 (soln (bind-var-wff-n 'lambda zl
			       (acons w 'exists
				      (acons 'AND
					     (bind-var-wff-n 'forall zl
							     (acons 'IMPLIES
								    (apply-wff-n w zl)
								    (apply-wff-n
								     (cons mop w) zl)))
					     (apply-wff-n w zl)))))
	 (gfpconc2 (substitute-l-term-var soln (bindvar gfpconc) (cdr gfpconc)))
	 (fconc (make-ftree-exp
		 gfpconc (list soln)
		 (list (make-ftree-rew 
			gfpconc2 'LAMBDA
			(make-knaster-tarski-gfp-negf-2
			 soln (lnorm gfpconc2) mop zl)))))
	 (fmon (make-knaster-tarski-gfp-negf-11 mop soln monhyp zl)))
    (make-ftree-imp fmon fconc)))

; conj70		
(defun make-knaster-tarski-gfp-negf-2 (soln wff mop zl)
  (make-ftree-con
   (make-ftree-con
    (make-knaster-tarski-gfp-negf-6 soln (cdar (cdar wff)) mop zl)
    (make-knaster-tarski-gfp-negf-3 soln (cdr (cdar wff)) mop zl))
   (make-knaster-tarski-gfp-negf-8 (cdr wff) zl)))

; SEL27
(defun make-knaster-tarski-gfp-negf-3 (soln wff mop zl &optional zl2)
  (declare (special special-nodes special-svars))
  (if zl
      (let ((z2 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z2
			(make-knaster-tarski-gfp-negf-3
			 soln (substitute-l-term-var z2 (bindvar wff) (cdr wff)) mop (cdr zl)
			 (cons z2 zl2))))
    (let* ((l (make-ftree-leaf (cdr wff) nil)) ; L68
	   (c (cdar wff))
	   (w (bindvar c))
	   (w2 (fresh-var (unabbreviated-type w) (getnameroot w))) ; w^20
	   (f2 (make-ftree-sel c w2
			       (make-knaster-tarski-gfp-negf-4
				mop (substitute-l-term-var w2 w (cdr c))
				zl2))))
      (push (cons 'w2 w2) special-svars) ; w^20
      (push (cons 'zl2 zl2) special-svars)
      (push (cons 'Kgfpz- l) special-nodes)
      (make-ftree-imp f2 l))))

; CONJ128
(defun make-knaster-tarski-gfp-negf-4 (mop wff zl)
  (let ((l (make-ftree-leaf (cdr wff) t))) ; L66
    (make-ftree-con
     (make-knaster-tarski-gfp-negf-5 l mop (cdar wff) (reverse zl))
     l)))

; EXP29
(defun make-knaster-tarski-gfp-negf-5 (l mop wff zl)
  (declare (special ktclist special-nodes))
  (if zl
      (let ((f (make-ftree-exp wff
			       (list (car zl))
			       (list (make-knaster-tarski-gfp-negf-5
				      l mop (substitute-l-term-var (car zl) (bindvar wff) (cdr wff))
				      (cdr zl))))))
	(push (cons 'wsubKw f) special-nodes)
	f)
    (let ((hyp (make-ftree-leaf (cdar wff) nil))
	  (conc (make-ftree-leaf (cdr wff) t)))
      (push (cons (ftree-name hyp) (ftree-name l)) ktclist) 
      (push (cons 'Kwz+ conc) special-nodes)
      (make-ftree-imp hyp conc))))

; SEL24
(defun make-knaster-tarski-gfp-negf-6 (soln wff mop zl &optional zl3)
  (declare (special special-svars special-nodes))
  (if zl
      (let ((z3 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z3
			(make-knaster-tarski-gfp-negf-6
			 soln (substitute-l-term-var z3 (bindvar wff) (cdr wff))
			 mop (cdr zl) (cons z3 zl3))))
    (let* ((l (make-ftree-leaf (cdar wff) t)) ; L56
	   (f (make-knaster-tarski-gfp-negf-7 l soln (cdr wff) mop)))
      (make-ftree-imp l f))))

; exp28
(defun make-knaster-tarski-gfp-negf-7 (l soln wff mop)
  (declare (special special-nodes ktclist)) 
  (let* ((mopgfp (cons mop soln))
	 (wff2 (substitute-l-term-var mopgfp (bindvar wff) (cdr wff))) ; doesn't lead to beta-reducts
	 (l1 (make-ftree-leaf (cdar wff2) nil)) ; this is a complex wff, but we don't need to deepen ; L55
	 (l2 (make-ftree-leaf (cdr wff2) nil))) ; L57
    (push (cons 'KKgfpKgfp- l1) special-nodes)
    (push (cons (ftree-name l) (ftree-name l2)) ktclist) 
    (make-ftree-exp
     wff (list mopgfp)
     (list (make-ftree-con l1 l2)))))

; sel29
(defun make-knaster-tarski-gfp-negf-8 (wff zl)
  (declare (special special-svars))
  (let ((v2 (fresh-var (unabbreviated-type (bindvar wff)) '\v)))
    (make-ftree-sel wff v2
		    (make-knaster-tarski-gfp-negf-9
		     (substitute-l-term-var v2 (bindvar wff) (cdr wff))
		     v2 zl))))

; imp146
(defun make-knaster-tarski-gfp-negf-9 (wff v2 zl)
  (declare (special ktclist special-nodes))
  (let* ((l2 (make-ftree-leaf (cdar wff) t)) ; don't need to deepen
	 (f (make-knaster-tarski-gfp-negf-10 l2 (cdr wff) v2 zl))
	 (l40 (cdr (assoc 'Kvsubv- special-nodes))))
    (push (cons (ftree-name l2) (ftree-name l40)) ktclist) 
    (make-ftree-imp l2 f)))

; SEL28
(defun make-knaster-tarski-gfp-negf-10 (l1 wff v2 zl)
  (declare (special ktclist special-nodes))
  (if zl
      (let ((z4 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z4
			(make-knaster-tarski-gfp-negf-10
			 l1 (substitute-l-term-var z4 (bindvar wff) (cdr wff))
			 v2 (cdr zl))))
    (let* ((l3 (make-ftree-leaf (cdar wff) t)) ; L72
	   (conc (substitute-l-term-var v2 (bindvar (cdr wff)) (cddr wff)))
	   (l4 (make-ftree-leaf (cdar conc) nil)) ; L70
	   (l5 (make-ftree-leaf (cdr conc) nil))) ; L73
      (push (cons (ftree-name l3) (ftree-name l5)) ktclist) 
      (push (cons 'Kvsubv- l4) special-nodes)
      (make-ftree-imp
       l3 (make-ftree-exp (cdr wff) (list v2)
			  (list (make-ftree-con l4 l5)))))))

; EXP36
(defun make-knaster-tarski-gfp-negf-11 (mop soln monhyp zl)
  (declare (special special-svars))
  (let* ((wff2 (substitute-l-term-var soln (bindvar monhyp) (cdr monhyp)))
	 (wff3 (lnorm wff2))
	 (wff4 (substitute-l-term-var (cons mop soln) (bindvar wff3) (cdr wff3)))
	 (f1 (make-ftree-exp wff3 (list (cons mop soln))
			     (list (make-knaster-tarski-gfp-negf-12 wff4 zl))))
	 (w2 (cdr (assoc 'w2 special-svars)))
	 (wff21 (substitute-l-term-var w2 (bindvar monhyp) (cdr monhyp)))
	 (wff22 (substitute-l-term-var soln (bindvar wff21) (cdr wff21)))
	 (wff23 (lnorm wff22))
	 (f23 (make-knaster-tarski-gfp-negf-20 wff23 zl))
	 (f22 (if (wffeq wff22 wff23)
		  f23
		(make-ftree-rew wff22 'LAMBDA f23)))
	 (f2 (make-ftree-exp wff21 (list soln) (list f22)))
	 (w3 (cdr (assoc 'w3 special-svars)))
	 (wff31 (substitute-l-term-var w3 (bindvar monhyp) (cdr monhyp)))
	 (wff32 (substitute-l-term-var soln (bindvar wff31) (cdr wff31)))
	 (wff33 (lnorm wff32))
	 (f33 (make-knaster-tarski-gfp-negf-17 wff33 zl))
	 (f32 (if (wffeq wff32 wff33)
		  f33
		(make-ftree-rew wff32 'LAMBDA f33)))
	 (f3 (make-ftree-exp wff31 (list soln) (list f32))))
    (make-ftree-exp monhyp (list soln w2 w3)
		    (list (if (wffeq wff2 wff3)
			      f1
			    (make-ftree-rew wff2 'lambda f1))
			  f2 f3))))

; IMP92
(defun make-knaster-tarski-gfp-negf-12 (wff zl)
  (declare (special special-nodes ktclist)) 
  (let* ((l (make-ftree-leaf (cdr wff) t)) ; don't need to deepen, L54
	 (l35 (cdr (assoc 'KKgfpKgfp- special-nodes))))
    (push (cons (ftree-name l) (ftree-name l35)) ktclist) 
    (make-ftree-imp
     (make-knaster-tarski-gfp-negf-13 (cdar wff) zl)
     l)))

; SEL23
(defun make-knaster-tarski-gfp-negf-13 (wff zl &optional zl5)
  (declare (special special-nodes special-svars))
  (if zl
      (let ((z5 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z5
			(make-knaster-tarski-gfp-negf-13
			 (substitute-l-term-var z5 (bindvar wff) (cdr wff))
			 (cdr zl) (cons z5 zl5))))
    (let ((l (make-ftree-leaf (cdr wff) nil)) ; L53
	  (w3 (fresh-var (unabbreviated-type (bindvar (cdar wff))) '\w))) ; w^19
      (push (cons 'zl5 zl5) special-svars)
      (push (cons 'w3 w3) special-svars)
      (push (cons 'Kgfpz2- l) special-nodes)
      (make-ftree-imp (make-ftree-sel (cdar wff) w3
				      (make-knaster-tarski-gfp-negf-14
				       (substitute-l-term-var w3 (bindvar (cdar wff))
							      (cddar wff))
				       zl5))
		      l))))

; CONJ66
(defun make-knaster-tarski-gfp-negf-14 (wff zl5)
  (declare (special special-nodes))
  (let* ((l5 (make-ftree-leaf (cdr wff) T)) ; L51
	 (e (make-knaster-tarski-gfp-negf-15 l5 (cdar wff) (reverse zl5))))
    (push (cons 'w2subKw e) special-nodes)
    (make-ftree-con e l5)))

; EXP22
(defun make-knaster-tarski-gfp-negf-15 (l5 wff zl5)
  (declare (special ktclist special-nodes))
  (if zl5
      (make-ftree-exp wff (list (car zl5))
		      (list (make-knaster-tarski-gfp-negf-15
			     l5
			     (substitute-l-term-var (car zl5) (bindvar wff) (cdr wff))
			     (cdr zl5))))
    (let ((l1 (make-ftree-leaf (cdar wff) nil)) ; L47
	  (l2 (make-ftree-leaf (cdr wff) t))) ; L49
      (push (cons 'Kw+ l2) special-nodes)
      (push (cons (ftree-name l1) (ftree-name l5)) ktclist) 
      (make-ftree-imp l1 l2))))

; IMP79
(defun make-knaster-tarski-gfp-negf-17 (wff zl)
  (declare (special special-svars))
  (make-ftree-imp (make-knaster-tarski-gfp-negf-18 (cdar wff) zl)
		  (make-knaster-tarski-gfp-negf-19 
		   (cdr wff) (reverse (cdr (assoc 'zl5 special-svars))))))

; SEL21
(defun make-knaster-tarski-gfp-negf-18 (wff zl)
  (declare (special special-svars special-nodes ktclist)) 
  (if zl
      (let ((z6 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel wff z6
			(make-knaster-tarski-gfp-negf-18
			 (substitute-l-term-var z6 (bindvar wff) (cdr wff))
			 (cdr zl))))
    (let* ((l28 (make-ftree-leaf (cdar wff) T)) ; L45
	   (w3 (cdr (assoc 'w3 special-svars)))
	   (conc (substitute-l-term-var w3 (bindvar (cdr wff)) (cddr wff)))
	   (l26 (make-ftree-leaf (cdar conc) NIL)) ; SEL32
	   (l27 (make-ftree-leaf (cdr conc) NIL)) ; L76.1
	   (w2subKw (cdr (assoc 'w2subKw special-nodes))))
      (push (cons (ftree-name l26) (ftree-name w2subKw)) ktclist) 
      (push (cons (ftree-name l27) (ftree-name l28)) ktclist) 
      (make-ftree-imp l28
		      (make-ftree-exp (cdr wff) (list w3)
				      (list (make-ftree-con l26 l27)))))))

; EXP23
(defun make-knaster-tarski-gfp-negf-19 (wff zl5)
  (declare (special special-nodes ktclist)) 
  (if zl5
      (make-ftree-exp wff (list (car zl5))
		      (list
		       (make-knaster-tarski-gfp-negf-19
			(substitute-l-term-var (car zl5) (bindvar wff) (cdr wff))
			(cdr zl5))))
    (let ((l30 (make-ftree-leaf (cdar wff) nil)) ; L50
	  (l31 (make-ftree-leaf (cdr wff) t)) ; L52
	  (l29 (cdr (assoc 'Kgfpz2- special-nodes)))
	  (l32 (cdr (assoc 'Kw+ special-nodes))))
      (push (cons (ftree-name l29) (ftree-name l31)) ktclist) 
      (push (cons (ftree-name l30) (ftree-name l32)) ktclist) 
      (make-ftree-imp l30 l31))))

; IMP123
(defun make-knaster-tarski-gfp-negf-20 (wff zl)
  (declare (special special-svars))
  (make-ftree-imp (make-knaster-tarski-gfp-negf-21 (cdar wff) zl)
		  (make-knaster-tarski-gfp-negf-22
		   (cdr wff) (reverse (cdr (assoc 'zl2 special-svars))))))

; SEL25
(defun make-knaster-tarski-gfp-negf-21 (wff zl)
  (declare (special special-svars special-nodes ktclist)) 
  (if zl
      (let ((z7 (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel
	 wff z7
	 (make-knaster-tarski-gfp-negf-21
	  (substitute-l-term-var z7 (bindvar wff) (cdr wff))
	  (cdr zl))))
    (let* ((l18 (make-ftree-leaf (cdar wff) T)) ; L45.1
	   (w2 (cdr (assoc 'w2 special-svars)))
	   (conc (substitute-l-term-var w2 (bindvar (cdr wff)) (cddr wff)))
	   (l16 (make-ftree-leaf (cdar conc) NIL)) ; SEL31
	   (l17 (make-ftree-leaf (cdr conc) NIL)) ; L76
	   (e (cdr (assoc 'wsubKw special-nodes))))
      (push (cons (ftree-name e) (ftree-name l16)) ktclist) 
      (push (cons (ftree-name l17) (ftree-name l18)) ktclist) 
      (make-ftree-imp
       l18
       (make-ftree-exp (cdr wff) (list w2) (list (make-ftree-con l16 l17)))))))

; EXP30
(defun make-knaster-tarski-gfp-negf-22 (wff zl2)
  (declare (special special-nodes ktclist)) 
  (if zl2
      (make-ftree-exp wff (list (car zl2))
		      (list (make-knaster-tarski-gfp-negf-22
			     (substitute-l-term-var (car zl2) (bindvar wff) (cdr wff))
			     (cdr zl2))))
    (let ((l20 (make-ftree-leaf (cdar wff) nil)) ; L50.1
	  (l21 (make-ftree-leaf (cdr wff) t)) ; L52.1
	  (l19 (cdr (assoc 'Kgfpz- special-nodes)))
	  (l22 (cdr (assoc 'Kwz+ special-nodes))))
      (push (cons (ftree-name l22) (ftree-name l20)) ktclist) 
      (push (cons (ftree-name l21) (ftree-name l19)) ktclist) 
      (make-ftree-imp l20 l21))))

; pf that the monotone function is monotone
(defun mon-fn-negf (wff)
  (let* ((u (fresh-var (unabbreviated-type (bindvar wff)) '\u))
	 (v (fresh-var (unabbreviated-type (bindvar wff)) '\v))
	 (wff2 (substitute-l-term-var u (bindvar wff) (cdr wff)))
	 (wff3 (substitute-l-term-var v (bindvar wff2) (cdr wff2)))
	 (usubv-uses nil))
    (declare (special usubv-uses))
    (let ((f (mon-fn-negf-1 u v (cdr wff3) (cdar wff3))))
      (make-ftree-sel
       wff u (make-ftree-sel
	      wff2 v
	      (make-ftree-imp
	       (make-ftree-exp (cdar wff3) 
			       (mapcar #'car usubv-uses)
			       (mapcar #'cdr usubv-uses))
	       f))))))

(defun mon-fn-negf-1 (u v wff usubv)
  (if (a-bd-wff-p wff)
      (let ((z (fresh-var (unabbreviated-type (bindvar wff)) '\z)))
	(make-ftree-sel
	 wff z
	 (mon-fn-negf-1
	  u v (substitute-l-term-var z (bindvar wff) (cdr wff)) usubv)))
    (multiple-value-bind
     (uf vf)
     (mon-fn-negf-2 u v (cdar wff) (cdr wff) usubv)
     (make-ftree-imp uf vf))))

(defun mon-fn-negf-2 (u v poswff negwff usubv)
  (declare (special usubv-uses))
  (if (wffeq-ab poswff negwff)
      (let ((uf (make-ftree-leaf poswff t))
	    (vf (make-ftree-leaf negwff nil)))
	(push-onto-clist uf vf)
	(values uf vf))
    (cond ((or (and (or-p poswff) (or-p negwff))
	       (and (and-p poswff) (and-p negwff)))
	   (multiple-value-bind
	    (uf1 vf1)
	    (mon-fn-negf-2 u v (cdar poswff) (cdar negwff) usubv)
	    (multiple-value-bind
	     (uf2 vf2)
	     (mon-fn-negf-2 u v (cdr poswff) (cdr negwff) usubv)
	     (if (or-p poswff)
		 (values (make-ftree-dis uf1 uf2) (make-ftree-dis vf1 vf2))
	       (values (make-ftree-con uf1 uf2) (make-ftree-con vf1 vf2))))))
	  ((and (not-p poswff) (not-p negwff))
	   (multiple-value-bind
	    (vf1 uf1)
	    (mon-fn-negf-2 u v (cdr negwff) (cdr poswff) usubv) ; switch the order
	    (values (make-ftree-neg uf1) (make-ftree-neg vf1))))
	  ((and (implies-p poswff) (implies-p negwff))
	   (multiple-value-bind
	    (vf1 uf1)
	    (mon-fn-negf-2 u v (cdar negwff) (cdar poswff) usubv) ; switch the order
	    (multiple-value-bind
	     (uf2 vf2)
	     (mon-fn-negf-2 u v (cdr poswff) (cdr negwff) usubv)
	     (values (make-ftree-imp uf1 uf2) (make-ftree-imp vf1 vf2)))))
	  ((and (e-bd-wff-p poswff) (e-bd-wff-p negwff))
	   (let* ((x (fresh-var (unabbreviated-type (bindvar poswff)) (getnameroot (bindvar poswff)))))
	     (multiple-value-bind
	      (uf vf)
	      (mon-fn-negf-2
	       u v
	       (substitute-l-term-var x (bindvar poswff) (cdr poswff))
	       (substitute-l-term-var x (bindvar negwff) (cdr negwff))
	       usubv)
	      (values (make-ftree-sel poswff x uf)
		      (make-ftree-exp negwff (list x) (list vf))))))
	  ((and (a-bd-wff-p poswff) (a-bd-wff-p negwff))
	   (let* ((x (fresh-var (unabbreviated-type (bindvar poswff)) (getnameroot (bindvar poswff)))))
	     (multiple-value-bind
	      (uf vf)
	      (mon-fn-negf-2
	       u v
	       (substitute-l-term-var x (bindvar poswff) (cdr poswff))
	       (substitute-l-term-var x (bindvar negwff) (cdr negwff))
	       usubv)
	      (values (make-ftree-exp poswff (list x) (list uf))
		      (make-ftree-sel negwff x vf)))))
	  ((and (eq u (head poswff))
		(eq v (head negwff)))
	   (let* ((args1 (args poswff))
		  (args2 (args negwff))
		  (l1 (make-ftree-leaf poswff nil))
		  (l2 (make-ftree-leaf negwff t))
		  (l3 (make-ftree-leaf poswff t))
		  (l4 (make-ftree-leaf negwff nil)))
	     (push-onto-clist l1 l3)
	     (push-onto-clist l2 l4)
	     (mapc #'(lambda (x y)
		       (unless (wffeq-ab x y)
			 (throwfail "Args " x " and " y " should match")))
		   args1 args2)
	     (let ((f (mon-fn-negf-3
		       (cdr args1) l1 l2
		       (substitute-l-term-var (car args1) (bindvar usubv) (cdr usubv)))))
	       (push (cons (car args1) f) usubv-uses)
	       (values l3 l4))))
	  (t (throwfail "Problem Showing Monotonicity in Knaster Tarski Lemma")))))

(defun mon-fn-negf-3 (args l1 l2 usubv)
  (if args
      (make-ftree-exp usubv (list (car args))
		      (list (mon-fn-negf-3
			     (cdr args) l1 l2
			     (substitute-l-term-var (car args) (bindvar usubv) (cdr usubv)))))
    (make-ftree-imp l1 l2)))

(defun constr-merge-ftrees (f1 f2)
  (declare (special clist))
  (let* ((m1 (list 'MERGE (list f1 f2)))
	 (merge-result nil))
    (declare (special merge-result))
    (dolist (k (append (ftree-components f1) (ftree-components f2)))
	    (setq merge-result (append (ftree-assoc k k) merge-result)))
    (setq merge-result (acons f1 m1 (acons f2 m1 merge-result)))
    (merge-ftree-real)
    (setq clist (mapcar #'(lambda (conn)
			    (let ((a (assoc (car conn) merge-result :key #'ftree-name))
				  (b (assoc (cdr conn) merge-result :key #'ftree-name)))
			      (cons (if a
					(ftree-name (cdr a))
				      (car conn))
				    (if b
					(ftree-name (cdr b))
				      (cdr conn)))))
			clist))
    (cdr (assoc f1 merge-result))))

; given two wff's of the same type, [f a1 . . . an] and [g b1 . . . bn],
; return ((a1 . b1) . . . (an . bn))
(defun construct-arg-assoc (wff1 wff2)
  (if (and (consp wff1) (not (boundwff-p wff1)) (consp wff2) (not (boundwff-p wff2)))
      (acons (cdr wff1) (cdr wff2) (construct-arg-assoc (car wff1) (car wff2)))
    nil))

(defun push-onto-clist (f1 f2)
  (declare (special clist))
  (push (cons (ftree-name f1) (ftree-name f2)) clist))

(defun print-constraint-set (cset)
  (msgf (constraint-set-kind cset))
  (when (member (car (constraint-set-kind cset)) '(EMPTY FULL MAX MIN))
    (dolist (constr (constraint-set-constrs cset))
	    (msgf ". ")
	    (print-set-constraint constr)))
  (when (eq (car (constraint-set-kind cset)) 'UNIF)
    (display-vp-diag (make-conjunction :components (car (constraint-set-constrs cset))))))

(defun print-set-constraint (constr)
  (if (jform-pos (car constr))
      (progn
	(msgf ((literal-represents (car constr)) . gwff) " ==> ")
	(when (cdr constr)
	  (if (jform-pos (cadr constr))
	      (msg ((cons 'NOT (literal-represents (cadr constr)))
		    . gwff))
	    (msg ((literal-represents (cadr constr)) . gwff))))
	(dolist (lit (cddr constr))
		(msg " , ")
		(if (jform-pos lit)
		    (msg ((cons 'NOT (literal-represents lit))
			  . gwff))
		  (msg ((literal-represents lit) . gwff))))
	(msg t))
    (progn
      (when (cdr constr)
	(if (jform-pos (cadr constr))
	    (msgf ((literal-represents (cadr constr)) . gwff))
	  (msgf ((cons 'NOT (literal-represents (cadr constr))) .
		 gwff)))
	(dolist (lit (cddr constr))
		(msg " , ")
		(if (jform-pos lit)
		    (msg ((literal-represents lit) . gwff))
		  (msg ((cons 'NOT (literal-represents lit))
			. gwff)))))
      (msg " ==> " ((literal-represents (car constr)) . gwff) t))))

; temporary, for testing	
(defun knaster-tarski-nd (tp &optional (w 0))
  (declare (special MS98-VERBOSE DEFAULT-TACTIC *global-rewrite-dtree*))
  (multiple-value-bind
   (negf clist)
   (if (equal w 0)
       (make-knaster-tarski-leastfp-lemma tp)
     (if (equal w 1)
	 (make-knaster-tarski-gfp-lemma tp)
       (make-knaster-tarski-lemma tp)))
   (check-ftree negf)
   (ftree-to-etree negf)
    (setf (eproof-allow-nonleaf-conns current-eproof) ; cebrown - 10/2/00
      (append (eproof-allow-nonleaf-conns current-eproof)
	      (mapcar #'car clist)
	      (mapcar #'cdr clist)))
    (setf (eproof-dissolve current-eproof) nil)
    (setq *global-rewrite-dtree* nil)
    (clrhash (cgraph))
    (clrhash (connections-array))
    (gc)
    (when ms98-verbose ; cebrown
      (msgf "Creating final jform with eproof allow-nonleaf-conns = " (eproof-allow-nonleaf-conns current-eproof)))
    (cr-eproof-jform)
    (when ms98-verbose
      (msgf "Final Jform")
      (display-vp-diag (eproof-jform current-eproof)))
   (setq active-mating (init-mating))
    (setf (mating-clist active-mating) nil)
    (when (let ((count 0)
		(max-substs-var nil)
		(max-substs-quick nil)
		(max-mates :INFINITE) ; cebrown 28/10/01
		(min-quick-depth 1)
		(max-search-depth nil)
		(max-utree-depth nil))
	    (dolist (d clist t)
	      (incf count)
	      (add-conn (car d) (cdr d))
	      (unless (= (length (mating-clist active-mating)) count) ;if it wasn't added...
		(return nil))))
      (setf (mating-completep active-mating) t)
      (merge-tree)
      (prove1 (ftree-shallow negf) 'KNASTER-TARSKI 100)
      (setf (get (caar (proof-plans dproof)) 'node) current-topnode)
      (etree-nat 'KNASTER-TARSKI 100 default-tactic 'auto))))

(defun check-ftree (f)
  (ftree-to-etree f)
  (check-etree-structure-break current-topnode "problem in ftree")
  (check-ftree-rec f))

(defun check-ftree-rec (f)
;  (msgf "f " (ftree-name f) ": " ((ftree-shallow f) . gwff) t)
;  (msgf (ftree-components f) t)
  (unless (check-wff (ftree-shallow f))
    (msgf "f " f)
    (break))
  (dolist (f1 (ftree-components f))
	  (check-ftree-rec f1)))
   
(defun check-wff (wff)
  (if (boundwff-p wff)
      (if (lambda-bd-p wff)
	  (check-wff (cdr wff))
	(and (eq (unabbreviated-type (cdr wff)) 'O)
	     (check-wff (cdr wff))))
    (if (consp wff)
	(let ((f (unabbreviated-type (car wff)))
	      (a (unabbreviated-type (cdr wff))))
	  (and (consp f) (equal (cdr f) a)
	       (check-wff (car wff))
	       (check-wff (cdr wff))))
      t)))

