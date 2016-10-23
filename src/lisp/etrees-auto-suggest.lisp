;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of etr-nat)

(context search-suggestions)

(defmateop etr-info
  (mate-alias etr-info)
  (mate-result-> ignore)
  (mhelp "Print information about the expansion tree"))

(defun etr-info ()
  (when (etree-p current-topnode)
    (let* ((exp-nodes (find-exp-nodes current-topnode)))
      (when exp-nodes
	(msgf "Expansion Terms:")
	(dolist (e exp-nodes)
		(dolist (trm (expansion-terms e))
			(msgf (trm . gwff))))))))

(defmexpr etr-auto-suggest
  (mhelp "Given an eproof, suggest flag settings for 
an automatic proof of the same theorem.  Such an eproof may
be the result of translating a natural deduction proof using
nat-etree.

This will show all of the instantiations (and primitive substitutions)
that are necessary for the proof, and suggest settings for 
NUM-OF-DUPS, MS98-NUM-OF-DUPS, and MAX-MATES."))

; and eventually should suggest for 
; MAX-PRIM-DEPTH, MAX-PRIM-LITS and REWRITE-DEFNS

(defun etr-auto-suggest (&optional (define-mode t))
  (declare (special *etr-suggest-list* MS98-DUP-BELOW-PRIMSUBS))
  (unless current-eproof (throwfail "There is no current eproof."))
  (let* ((etree (eproof-etree current-eproof))
	 (exp-nodes (find-exp-nodes etree))
	 (leaf-nodes (find-etree-nodes #'leaf-p etree nil t))
	 (equivwffs-nodes (find-etree-nodes 
			   #'(lambda (n)
			       (and (rewrite-p n)
				    (member (rewrite-justification n) '(equiv-eq equivwffs))))
			   etree nil t))
; NUM-OF-DUPS
	 (num-of-dups-sugg (compute-num-of-dups exp-nodes))
	 (ms98-num-of-dups-sugg 
	  (max 1 (apply #'+
			(or (mapcar #'(lambda (e)
					(- (length (etree-components e)) 1))
				    exp-nodes)
			    '(0)))))
	 (none-defns nil)		; defns which appear uninstantiated in the head of a leaf
	 (eager-defns nil)              ; defns which are instantiated
	 (rewrite-defns-sugg nil)
	 (rewrite-equalities-sugg nil)
	 (max-substs-var-sugg 1)
	 (ms98-init-sugg 1)
	 (ms98-dup-below-primsubs-sugg nil)
	 (ms98-primsub-count-sugg nil)
	 (max-prim-lits-sugg nil) ; may need separate suggestions depending on primsub-method suggestion.
	 (min-prim-lits-sugg nil)
	 (max-prim-depth-sugg nil)
	 (min-prim-depth-sugg nil)
	 (prim-bd-types-sugg nil)
	 (pr97c-max-abbrevs-sugg 0)
	 (num-primsubs 0)
	 (primsub-method-sugg 'pr95))	; for now, choose between pr95 and pr97c
    (dolist (l leaf-nodes)
      (let ((head (head (leaf-shallow l))))
	(when (or (abbrev-p head)
		  (pmabbrev-p head))
	  (setq none-defns (adjoin (get head 'stands-for) none-defns)))))
;    (dolist (r equivwffs-nodes)   I can't figure out what to do here to figure out which abbrevs were expanded - cebrown 3/3/00
;      (let* ((s1 (get-shallow r))
;	     (s2 (get-shallow (car (etree-components r)))))
;	
;	))
    (dolist (e exp-nodes)
	    (do ((terms (expansion-terms e) (cdr terms))
		 (kids (etree-components e) (cdr kids)))
		((or (not terms) (not kids)))
		(let ((term (car terms))
		      (kid (car kids)))
		  (if (gensub-p term) ; then analyze the gensub
		      (let* ((pnf-term (prenex-gensub (strip-exp-vars term))) ; strip-exp-vars to get a usual wff
			     (msvp (max-substs-var-in-gensub pnf-term))
			     (depth (gensub-depth pnf-term))
			     (lits (gensub-num-lits pnf-term))
			     (bd-types (gensub-bd-types pnf-term))
			     (abbrnum (gensub-num-abbr-heads pnf-term)))
			(setq ms98-init-sugg 3) ; For now I'm deciding between 1 and 3.  Might consider 2 later.
			(unless ms98-dup-below-primsubs-sugg
			  (when (> (compute-num-of-dups (find-exp-nodes kid)) 0)
			    (setq ms98-dup-below-primsubs-sugg t)))
			(when (> abbrnum 0)
			  (setq pr97c-max-abbrevs-sugg (max pr97c-max-abbrevs-sugg abbrnum))
			  (setq primsub-method-sugg 'pr97c))
			(setq num-primsubs (+ 1 num-primsubs))
			(setq min-prim-lits-sugg 
			      (if min-prim-lits-sugg 
				  (min min-prim-lits-sugg lits)
				lits))
			(setq max-prim-lits-sugg 
			      (if max-prim-lits-sugg 
				  (max max-prim-lits-sugg lits)
				lits))
			(setq min-prim-depth-sugg 
			      (if min-prim-depth-sugg 
				  (min min-prim-depth-sugg depth)
				depth))
			(setq max-prim-depth-sugg 
			      (if max-prim-depth-sugg 
				  (max max-prim-depth-sugg depth)
				depth))
			(setq max-substs-var-sugg
			      (max max-substs-var-sugg msvp)))
		      ; otherwise check setting for max-substs-var
		    (setq max-substs-var-sugg
			  (max max-substs-var-sugg
			       (ho-unif-depth-of-term term)))))))
; REWRITE-DEFNS ?
; REWRITE-EQUALITIES ?
; MAX-MATES, we need the mating (not just the etree) to determine this one.
    (unless (and active-mating 
		 (mating-completep active-mating))
      (mate-prop-msearch))
    (let ((conn-list (make-mating-lists active-mating))
	  (occ-count nil))
      (dolist (conn conn-list)
	      (when (consp conn)
		(let ((a1 (assoc (car conn) occ-count))
		      (a2 (assoc (cdr conn) occ-count)))
		  (if a1
		      (setq occ-count (acons (car conn) (1+ (cdr a1))
					     (remove a1 occ-count)))
		    (setq occ-count (acons (car conn) 1 occ-count)))
		  (if a2
		      (setq occ-count (acons (cdr conn) (1+ (cdr a2))
					     (remove a2 occ-count)))
		    (setq occ-count (acons (cdr conn) 1 occ-count))))))
      (let ((max-mates-sugg (apply #'max (mapcar #'cdr occ-count)))
	    (mode-flags nil))
	(declare (special mode-flags))
	(msgf "MS98-INIT suggestion: " ms98-init-sugg t)
	(msgf "MAX-SUBSTS-VAR should be " max-substs-var-sugg t)
	(msgf "NUM-OF-DUPS should be " num-of-dups-sugg t)
	(msgf "MS98-NUM-OF-DUPS should be " ms98-num-of-dups-sugg t)
	(msgf "MAX-MATES should be " max-mates-sugg t)
	(when (> num-primsubs 0)
	  (msgf "PRIMSUB-METHOD suggestion: " primsub-method-sugg)
	  (msgf "MAX-PRIM-DEPTH should be: " max-prim-depth)
	  (msgf "MAX-PRIM-LITS should be: " max-prim-lits)
	  (msgf "MIN-PRIM-DEPTH should be: " min-prim-depth)
	  (msgf "MIN-PRIM-LITS should be: " min-prim-lits)
	  (msgf "MS98-DUP-BELOW-PRIMSUBS should be: " ms98-dup-below-primsubs)
	  (push (list 'primsub-method primsub-method-sugg) mode-flags)
	  (push (list 'max-prim-depth max-prim-depth-sugg) mode-flags)
	  (push (list 'max-prim-lits max-prim-lits-sugg) mode-flags)
	  (push (list 'min-prim-depth min-prim-depth-sugg) mode-flags)
	  (push (list 'min-prim-lits min-prim-lits-sugg) mode-flags)
	  (push (list 'ms98-dup-below-primsubs ms98-dup-below-primsubs) mode-flags))
	(push (list 'num-of-dups num-of-dups-sugg) mode-flags)
	(push (list 'ms98-num-of-dups ms98-num-of-dups-sugg) mode-flags)
	(push (list 'max-mates max-mates-sugg) mode-flags)
	(push (list 'default-ms 'MS98-1) mode-flags) ; Matt's procedure
; Matt's Thesis (Appendix A, pp. 124-125) claims it is better to
; use selection nodes rather than skolemization for component search.
; So, skolem-default should be set to NIL.
	(push (list 'skolem-default NIL) mode-flags) ; Matt's procedure
; for now make something up for the other major values:
	(push (list 'rewrite-defns '(EAGER)) mode-flags)
	(push (list 'rewrite-equalities 'ALL) mode-flags)
	(push (list 'remove-leibniz 'T) mode-flags)
	(push (list 'max-substs-var max-substs-var-sugg) mode-flags)
        (push (list 'ms98-init ms98-init-sugg) mode-flags) ; THIS ONE IS VERY IMPORTANT, see Matt's Thesis, p. 118
           ; I should set it to 1, 2, or 3 depending on the primsubs I need.
	(when (and define-mode (query "Do you want to define a mode with these settings?" t))
	  (let ((symb (intern (concatenate 'string "MODE-" (princ-to-string dproof)
					   "-SUGGEST"))))
	    (do ((name nil))
		((not (or (null name) (memq name global-modelist)))
		 (eval `(defmode ,name ,(cons 'flag-settings (core::flagsort mode-flags))
			  (mhelp "Mode created by AUTO-SUGGEST.")))
		 (reorganize))
		(prompt-read name nil (msgf "Name for mode? ") 'symbol symb
			     ((? (msgf "Provide a name for the new mode."))
			      (?? (mhelp 'tps-mode))))
		(when (and (memq name global-modelist)
			   (query "That mode already exists. Overwrite it?" t))
		      (dolist (prop (plist name))
			      (remprop name prop))
		      (setq global-modelist (delete name global-modelist))))))))))

(defun prenex-gensub (wff)
  (if (lambda-bd-p wff)
      (cons (car wff)
	    (prenex-gensub (cdr wff)))
    (prenex-normal-form wff)))

; I can't find a function to convert a wff to prenex normal form.
; Hard to believe.  But at least I'm reusing ab-normalize, neg-norm,
; and rewrite-all-equivalence
(defun prenex-normal-form (wff)
  (multiple-value-bind
   (matrix prefix)
   (nnf-ab-to-prenex-normal-form 
    (neg-norm (ab-normalize (rewrite-all-equivalence wff))))
   (dolist (binder (reverse prefix) matrix)
	   (setq matrix (cons binder matrix)))))

; assumes all quantifiers are positive subformulas,
; and the only binary connectives are AND and OR
; (as a consequence of nnf, but that's all I'm using)
; and all bound variables are distinct (ab-normalize
; took care of renaming)
(defun nnf-ab-to-prenex-normal-form (wff)
  (cond ((label-q wff) (values wff nil))
	((lsymbol-q wff) (values wff nil))
	((boundwff-q wff)
	 (if (lambda-bd-p wff) ; this really shouldn't happen
	     (values wff nil)
	   (multiple-value-bind
	    (p binders)
	    (nnf-ab-to-prenex-normal-form (gdr wff))
	    (values p (cons (car wff) binders)))))
	((or (and-p wff) (or-p wff))
	 (multiple-value-bind
	  (wff1 binders1)
	  (nnf-ab-to-prenex-normal-form (cdar wff))
	  (multiple-value-bind
	   (wff2 binders2)
	   (nnf-ab-to-prenex-normal-form (cdr wff))
	   (values (cons (cons (caar wff) wff1) wff2)
		   (append binders1 binders2)))))
	(t (values wff nil)))) ; there can be no more binders, by assumption on the form of the wff

(defun strip-lambda-prefix (term)
  (if (lambda-bd-p term)
      (strip-lambda-prefix (gdr term))
    term))

(defun strip-binding-prefix (term)
  (if (boundwff-p term)
      (strip-binding-prefix (cdr term))
    term))

(defun wff-applic-head (term)
  (if (and (not (boundwff-p term))
	   (wff-applic-p term))
      (wff-applic-head (gar term))
    term))

(defun gensub-p (term)
  (let* ((body (strip-lambda-prefix term))
	 (head (wff-applic-head term)))
    (or (a-bd-wff-p body)
	(e-bd-wff-p body)
	(and-p body)
	(or-p body)
	(implies-p body)
	(equiv-p body)
	(equals-p body)
	(abbrev-p head)
	(pmabbrev-p head))))

(defun max-substs-var-in-gensub (gensub)
  (max-substs-var-in-gensub-1
   (strip-lambda-prefix gensub)))

(defun max-substs-var-in-gensub-1 (body)
  (cond ((or (and-p body) (or-p body) (implies-p body) (equiv-p body))
	 (max (max-substs-var-in-gensub (gdr (gar body)))
	      (max-substs-var-in-gensub (gdr body))))
	((or (a-bd-wff-p body) (e-bd-wff-p body))
	 (max-substs-var-in-gensub (gdr body)))
	((equals-p body)
	 (max (max-substs-var-in-gensub (gdr (gar body)))
	      (max-substs-var-in-gensub (gdr body))))
	(t (ho-unif-depth-of-term body))))

(defun gensub-depth (gensub)
  (let ((body (strip-lambda-prefix gensub)))
    (if (and (boundwff-p body)
	     (not (or (boundwff-p (cdr body))
		      (and-p (cdr body))
		      (or-p (cdr body)))))
	1 ; sort of a special case (do a help on primsub-method)  ; actually I should fix this -- it's different for 95 vs 97C
      (+ 1 (prefix-length body)))))

(defun prefix-length (wff)
  (if (boundwff-p wff)
      (+ 1 (prefix-length (cdr wff)))
    0))

(defun gensub-num-lits (gensub)
  (let ((matrix (strip-binding-prefix gensub)))
    (gensub-num-lits-1 matrix)))

(defun gensub-num-lits-1 (matrix)
  (if (or (and-p matrix)
	  (or-p matrix))
      (+ (gensub-num-lits-1 (cdar matrix))
	 (gensub-num-lits-1 (cdr matrix)))
    1))

(defun gensub-bd-types (gensub)
  (let ((body (strip-lambda-prefix gensub)))
    (cond ((or (and-p gensub) (or-p gensub) (implies-p gensub) (equiv-p gensub))
	   (union (gensub-bd-types (gdr (gar gensub)))
		  (gensub-bd-types (gdr gensub))
		  :test #'equal))
	  ((or (a-bd-wff-p body) (e-bd-wff-p body))
	   (adjoin (with-output-to-string (*standard-output*) (pp-typesym (type (caar gensub))))
		   (gensub-bd-types (gdr gensub))
		   :test #'equal))
	  (t nil))))

(defun gensub-num-abbr-heads (gensub)
  (gensub-num-abbr-heads-1 (strip-lambda-prefix gensub)))

(defun gensub-num-abbr-heads-1 (body)
  (cond ((or (and-p body) (or-p body) (implies-p body) (equiv-p body))
	 (+ (gensub-num-abbr-heads-1 (gdr (gar body)))
	    (gensub-num-abbr-heads-1 (gdr body))))
	((or (a-bd-wff-p body) (e-bd-wff-p body))
	 (gensub-num-abbr-heads-1 (gdr body)))
	((equals-p body) 0)
	(t (let ((head (wff-applic-head body)))
	     (if (or (abbrev-p head)
		     (pmabbrev-p head))
		 1
	       0)))))

(defun compute-num-of-dups (exp-nodes)
  (- (apply #'max
	    (or
	     (mapcar #'(lambda (e)
			 (length (etree-components e)))
		     exp-nodes)
	     '(1)))
     1)) ; subtract 1 because we start with one copy in advance

; I should prove that this really gives the number of
; imitations/projections needed to generate the term
; (modulo etanorm -- see Matt's Thesis App. A on MAX-SUBSTS-VAR)
;     This doesn't work quite right yet.  I believe it overestimates.
(defun ho-unif-depth-of-term (term)
  (if (lsymbol-p (etanorm term))
      1
    (let ((body (strip-lambda-prefix term)))
      (cond ((lsymbol-p body) 1)
	    ((boundwff-p body) (+ 1 (ho-unif-depth-of-term (gdr body)))) ; this is a primsub actually, I suppose -- unless TPS's unification handles formulas (which it really should) (?)
	    ((wff-applic-p body) (+ (ho-unif-depth-of-term (gar body))
				    (ho-unif-depth-of-term (gdr body))))
	    (t 1))))) ; ???
