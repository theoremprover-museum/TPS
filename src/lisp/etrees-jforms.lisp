;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of expansion-tree)

(deffile etrees-jforms
  (part-of expansion-tree)
  (extension clisp)
  (mhelp "Etree to Jform conversion commands."))

(context jforms1)


;;; If a literal is returned, i.e., there's only one leaf in the tree,
;;; then make it the only child of a conjunction.  This avoids problems later
;;; with trying to check the parent of the literal (in complete-mspath).
;;; DAN 18AUG88

					; Regarding DAN's comment above, I'm doing this in final-jform now. (called from etree-to-jform & etree-to-prop-jform)
(defun final-jform (jform)
  (if (literal-p jform)
      (make-conjunction :components (list jform))
    jform))

(defun etree-to-jform (etree)
  (declare (special dissolve))
  (let ((jform (normalize-jform (etree-to-jform-2 etree)))
	(diss (append dissolve
		      (if (eproof-p current-eproof)
			  (eproof-dissolve current-eproof)
			nil))))
    (dolist (d diss (final-jform jform))
      (setq jform (normalize-jform (dissolve-jform d jform))))))
;      (display-vp-diag jform))))
  
(defun etree-to-jform-2 (etree)
  (let ((jform (car (etree-to-jform-rec etree nil))))
    (if jform
	(if (literal-p jform)
	    (let ((and-form (make-conjunction :components (list jform))))
	      (setf (jform-parent jform) and-form)
	      (setf (jform-progress and-form) (jform-progress jform))
	      and-form)
	    jform)
	;;this case will arise only if the status of all nodes in ETREE is
	;;not positive.
      (throwfail "No active node in the current expansion tree."))))

(defun etree-to-prop-jform (etree)
  (declare (special dissolve))
  (let ((jform (normalize-jform (etree-to-prop-jform-2 etree)))
	(diss (append dissolve
		      (if (eproof-p current-eproof)
			  (eproof-dissolve current-eproof)
			nil))))
    (dolist (d diss (final-jform jform))
      (setq jform (normalize-jform (dissolve-jform d jform))))))

(defun etree-to-prop-jform-2 (etree)
  (let ((jform (car (etree-to-prop-jform-rec etree nil))))
    (if jform
        (if (eq jform '&)
            (make-disjunction :components nil
                              :represents
                              (if (wffeq (get-shallow etree)
                                         'FALSEHOOD)
                                  'FALSEHOOD
                                  (cons 'not 'TRUTH))
                              :progress (etree-status* etree))
            (if (eq jform '$)
                (make-conjunction :components nil
                                      :represents
                                      (if (wffeq (get-shallow etree) 'TRUTH)
                                          'TRUTH
                                          (cons 'not 'FALSEHOOD))
                                      :progress (etree-status* etree))
                (if (literal-p jform)
                    (let ((and-form (make-conjunction :components (list jform))))
                      (setf (jform-parent jform) and-form)
                      (setf (jform-progress and-form) (jform-progress jform))
                      and-form)
                    jform)))
	;;this case will arise only if the status of all nodes in ETREE is
	;;not positive.
      (throwfail "No active node in the current expansion tree."))))

(defflag ALLOW-NONLEAF-CONNS
    (flagtype symbollist)
  (default nil)
  (subjects mating-search transmit)
  (mhelp "The value of this flag is a list of symbols.
If ALL is in the list, then the jform contains literals for each node
(except LAMBDA rewrites).

If REWRITES is in the list, then the jform contains literals
for each rewrite node (except LAMBDA's).

If the name of an etree node is in the list, then the jform contains literals
for the specified node.

NOTE:  This flag affects the way jforms are generated.  Consequently,
different search procedures may (or may not) be affected by it."))

(defflag DISSOLVE
    (flagtype matingpairlist)
  (default nil)
  (subjects mating-search transmit)
  (mhelp "DISSOLVE is set to a list of connections which are used to perform dissolution
when forming the jform from the etree.  If the list of connections is NIL the jform
is constructed as usual.
(See Murray, Rosenthal, Dissolution: Making Paths Vanish, JACM 40, 3, July 1993, pp. 504-535)"))

(defun lambda-rewrite-p (etree)
  (and (rewrite-p etree)
       (member (rewrite-justification etree) '(LAMBDA BETA ETA))))

(defun flexible-p (w)
  (if (exp-var-p w)
      (if (eq (exp-var-var w) (exp-var-subst w))
	  t
	(flexible-p (exp-var-subst w)))
    (if (and (consp w) (not (boundwff-p w)))
	(flexible-p (car w))
      nil)))

(defun allow-node-in-jform (etree)
  (or (leaf-p* etree)
      (member (etree-name etree) ALLOW-NONLEAF-CONNS)
      (and (eproof-p current-eproof)
	   (member (etree-name etree) (eproof-allow-nonleaf-conns current-eproof)))
      (and (not (lambda-rewrite-p etree))
	   (not (negation-p etree)) ; cebrown 2/17/01
	   (member 'ALL ALLOW-NONLEAF-CONNS))
      (and (rewrite-p etree)
	   (not (lambda-rewrite-p etree))
	   (member 'REWRITES ALLOW-NONLEAF-CONNS))
      (let ((diss (append dissolve
			  (if (eproof-p current-eproof)
			      (eproof-dissolve current-eproof)
			    nil))))
	(dolist (d diss nil)
		(when (or (eq (etree-name etree) (car d))
			  (eq (etree-name etree) (cdr d)))
		  (return t))))))
  
					; make-nnf-literal, cebrown - 2/17/00 -
					; works with nnf's - 
					; chooses whether to make the lit pos or neg based on
					; whether nnf or wff starts with an EXISTS/OR or FORALL/AND
					; this is so we can mate these nodes later

					; made a flag MATE-UP-TO-NNF
					; to control when NNF's are
					; used and when original wff's are used.
					; PR00 gives cleaner setsubs sometimes when
					; original wff's are used (eg, X5310) - cebrown 7/4/01
(defflag MATE-UP-TO-NNF
  (flagtype boolean)
  (default t)
  (subjects mating-search transmit)
  (mhelp "If MATE-UP-TO-NNF is T, then literals represent the
negation normal form of formulas or their negation.
This allows connections between formulas that are
only equal up to negation normal form."))

(defun make-nnf-literal (name wff pos progr)
  (if MATE-UP-TO-NNF
      (let ((nnf (neg-norm (lnorm wff))))
	(cond ((or (a-bd-wff-p nnf)
		   (and-p nnf))
	       (setq nnf (neg-norm (cons 'NOT nnf)))
	       (setq pos (not pos)))
	      ((not-p nnf)
	       (setq nnf (cdr nnf))
	       (setq pos (not pos))))
	(if (and (not add-truth) (not truthvalues-hack))
	    (cond ((or (and pos (eq nnf 'TRUTH))
		       (and (not pos) (eq nnf 'FALSEHOOD)))
		   (make-conjunction :components nil))
		  ((or (and (not pos) (eq nnf 'TRUTH))
		       (and pos (eq nnf 'FALSEHOOD)))
		   (make-disjunction :components nil))
		  (t
		   (make-literal :name name :type 'literal :represents nnf :pos pos
				 :progress progr)))
	  (if (equal wff '(NOT . FALSEHOOD))
	      (make-literal :name name :type 'literal :represents 'FALSEHOOD 
			    :pos (not pos))
	    (make-literal :name name :type 'literal :represents nnf :pos pos
			  :progress progr))))
    (make-literal :name name :type 'literal :represents wff :pos pos
		  :progress progr)))
  
(defun etree-to-prop-jform-rec (etree junctive)
  (let ((j (etree-to-prop-jform-rec-2 etree 
				      (if (ALLOW-NODE-IN-JFORM etree)
					  'CON
					junctive))))
    (if (and (ALLOW-NODE-IN-JFORM etree) (not (leaf-p* etree)))
	(let ((lit (make-nnf-literal (etree-name* etree)
					   (strip-exp-vars (get-shallow etree))
					   (positive-p etree)
					   (etree-status* etree))))
	  (when (memq '& j)		; false
	    (return-from etree-to-prop-jform-rec (list '&)))
	  (setq j (delete '$ j))
	  (if (eq junctive 'CON)
	      (cons lit j)
	    (if j
		(let ((node (make-conjunction :components (cons lit j))))
		  (setf (jform-parent lit) node)
		  (dolist (son j)
		    (setf (jform-parent son) node))
		  (setf (jform-progress node)
		    (max (jform-progress lit) (jform-progress (car j))))
		  (list node))
	      (list lit))))
      j)))

(defun etree-to-prop-jform-rec-2 (etree junctive)
  (when (plusp (etree-status* etree))
    (let ((lit (make-nnf-literal (etree-name* etree) 
				 (strip-exp-vars (get-shallow etree))
				 (positive-p etree)
				 (etree-status* etree))))
    (if (etree-junctive* etree)
	(cond 
         ((cdr (etree-components* etree))
          (let* ((flavor (etree-junctive* etree))
                 (successors (mapcan #'(lambda (etree)
                                         (etree-to-prop-jform-rec
                                          etree (or flavor junctive)))
                                     (etree-components* etree))))
	    (unless successors
              (return-from etree-to-prop-jform-rec-2 nil))
            (when successors
              (case flavor
                (con (setq successors (delete '$ successors))
                     (unless successors
		       (return-from etree-to-prop-jform-rec-2 (list '$)))
                     (when (memq '& successors)
                       (return-from etree-to-prop-jform-rec-2 (list '&))))
                (dis (setq successors (delete '& successors))
                     (unless successors
                       (return-from etree-to-prop-jform-rec-2 (list '&)))
                     (when (memq '$ successors)
                       (return-from etree-to-prop-jform-rec-2 (list '$))))))
            (if (or (and (not (ALLOW-NODE-IN-JFORM etree)) (eq junctive flavor))
		    (and (eq junctive flavor) (eq junctive 'CON))) 
		successors
                (let ((node (if (eq flavor 'dis)
                                (make-disjunction :components successors)
                                (make-conjunction :components successors))))
                  (dolist (son successors)
                    (setf (jform-parent son) node))
                  (setf (jform-progress node)
                        (max (etree-status* etree)
                             (apply-fn-with-key #'max successors
                                                #'jform-progress)))
                  (list node)))))
         ((etree-components* etree)
          (etree-to-prop-jform-rec (car (etree-components* etree)) junctive))
         (t (case (etree-junctive* etree)
              (con (list '$));;$ = T
              (dis (list '&)))));;& = F
	(list lit)))))

(defun exp-node-to-jform (etree junctive)
  (labels ((add-vars (son bdvars)
                 (if (consp son)
                     (cons (car son) (append bdvars (cdr son)))
                     (cons son bdvars)))
           (make-son (comp exp-term)
                     (let ((jforms (etree-to-jform-rec comp 'univ))
                           (bdvars 
                            (mapcar #'strip-exp-vars
                                    (substitutable-vars-of exp-term))))
                       (mapcar #'(lambda (x) (add-vars x bdvars))
                               jforms))))
  (let ((sons (mapcan #'make-son
                      (etree-components* etree) (sel-exp-terms etree))))
    (if (eq junctive 'univ)
        sons
        (if (cdr sons)
            (if (eq junctive 'con)
                (mapcar #'(lambda (son)
                            (if (cdr son)
                                (let ((node (make-universal :scope (car son) 
							    :selected (reduce 'append 
										(mapcar #'exp-var-selected
											(expansion-terms etree)))
                                                            :progress (etree-status* etree)
                                                            :qvars (cdr son))))
                                   (setf (jform-parent (car son)) node) node)
                                (car son)))
                        sons)
                (let ((node
                       (make-conjunction
                        :components
                        (mapcar #'(lambda (son)
                                    (if (cdr son)
                                        (let ((node (make-universal 
						     :scope (car son)
						     :selected (reduce 'append 
								       (mapcar #'exp-var-selected
									       (remove-if-not #'(lambda (x) (memq (exp-var-var x) (cdr son)))
										(expansion-terms etree))))
						     :progress (etree-status* etree)
						     :qvars (cdr son))))
                                           (setf (jform-parent (car son)) node) node)
                                        (car son)))
                                 sons))))
                  (dolist (child (conjunction-components node))
                    (setf (jform-parent child) node))
                  (setf (jform-progress node)
                        (max (etree-status* etree)
                             (apply-fn-with-key
                              #'max (conjunction-components node)
                              #'jform-progress)))
                  (list node)))
            (if (cdar sons)
                (let ((node  (make-universal :scope (caar sons)
					     :selected (reduce 'append 
								 (mapcar #'exp-var-selected
									 (expansion-terms etree)))
					     :progress (etree-status* etree)
					     :qvars (cdar sons))))
                   (setf (jform-parent (caar sons)) node) (list node))
	      (car sons)))))))

;replaced by the version in hx-natree-aux.lisp MB Thu Jul  3 12:31:56 1997
#+comment(defun etree-to-jform-rec (etree junctive)
  (when (plusp (etree-status* etree))
    (if (and (expansion-p etree) (not (leaf-p* etree)))
        (exp-node-to-jform etree junctive)
         (if (etree-junctive* etree)
            (cond 
             ((cdr (etree-components* etree))
              (let* ((flavor (etree-junctive* etree))
                     (successors (mapcan #'(lambda (etree)
                                             (etree-to-jform-rec etree flavor))
                                         (etree-components* etree))))
                (when successors
                  (if (eq junctive flavor) successors
                      (let ((node (if (eq flavor 'dis)
                                      (make-disjunction :components successors)
                                      (make-conjunction :components successors))))
                        (dolist (son successors)
                          (setf (jform-parent son) node))
                        (setf (jform-progress node)
                              (max (etree-status* etree)
                                   (apply-fn-with-key #'max successors
                                                      #'jform-progress)))
                        (list node))))))
             ((etree-components* etree)
              (etree-to-jform-rec (car (etree-components* etree)) junctive))
             (t (list 
                 (case (etree-junctive* etree)
                   (con (make-conjunction :components nil
                                          :represents
                                          (if (wffeq (get-shallow etree) 'TRUTH)
                                              'TRUTH
                                              (cons 'not 'FALSEHOOD))
                                          :progress (etree-status* etree)))
                   (dis (make-disjunction :components nil
                                          :represents
                                          (if (wffeq (get-shallow etree)
                                                     'FALSEHOOD)
                                              'FALSEHOOD
                                              (cons 'not 'TRUTH))
                                          :progress (etree-status* etree)))))))
            (list (make-nnf-literal (etree-name* etree)
				    (strip-exp-vars (get-shallow etree))
				    (positive-p etree)
				    (etree-status* etree)))))))

					; the following etree-to-rulep-jform is used by the RuleP tactic (ml-etr-tactics-book.lisp)
					; it builds a JForm ignoring nodes beneath EXP/SEL nodes.  This is accomplished by
					; treating EXP/SEL nodes as True (so they cannot be of any help finding a mating)
					; Also, we do not treat formulas up to NNF, since NNF may involve quantifiers
(defun make-rulep-literal (name wff pos progr)
  (when (not-p wff)
    (setq wff (cdr wff))
    (setq pos (not pos)))
  (cond ((or (and pos (eq wff 'TRUTH))
	     (and (not pos) (eq wff 'FALSEHOOD)))
	 (make-conjunction :components nil))
	((or (and (not pos) (eq wff 'TRUTH))
	     (and pos (eq wff 'FALSEHOOD)))
	 (make-disjunction :components nil))
	(t
	 (make-literal :name name :type 'literal :represents wff :pos pos
		       :progress progr))))

(defun etree-to-rulep-jform (etree)
  (final-jform (normalize-jform (etree-to-rulep-jform-rec etree))))

(defun etree-to-rulep-jform-rec (etree)
  (if (ALLOW-NODE-IN-JFORM etree)
      (let ((lit (make-rulep-literal (etree-name* etree)
				     (strip-exp-vars (get-shallow etree))
				     (positive-p etree)
				     (etree-status* etree))))
	(make-conjunction :components
			  (list lit (etree-to-rulep-jform-rec-2 etree))))
    (etree-to-rulep-jform-rec-2 etree)))

(defun etree-to-rulep-jform-rec-2 (etree)
  (cond ((or (econjunction-p etree)	; only allow propositional nodes
	     (edisjunction-p etree)
	     (true-p etree)
	     (false-p etree)
	     (implication-p etree)
	     (and (rewrite-p etree)
		  (member (rewrite-justification etree) '(EQUIV-DISJS EQUIV-IMPLICS))))
	 (if (eq (etree-junctive etree) 'con)
	     (make-conjunction :components
			       (mapcar #'etree-to-rulep-jform-rec (etree-components etree)))
	   (make-disjunction :components
			     (mapcar #'etree-to-rulep-jform-rec (etree-components etree)))))
	((negation-p etree)
	 (etree-to-rulep-jform-rec (car (etree-components etree))))
	(t				; otherwise it is not propositional
	 (if (ALLOW-NODE-IN-JFORM etree)
	     (make-conjunction :components nil) ; then it's already been created above, so just return true
	   (make-rulep-literal (etree-name* etree) ; otherwise create a literal for this node
			       (strip-exp-vars (get-shallow etree))
			       (positive-p etree)
			       (etree-status* etree))))))

					; dissolution code - cebrown 9/1/00
					;(see Murray Rosenthal, Dissolution: Making Paths Vanish, JACM 40, 3, July 1993, pp. 504 - 535

(defun find-all-leaf-names-in-jform (jform)
  (mapcar #'literal-name (find-all-leaves-in-jform jform)))

					; destructively changes the jform to dissolve the connection conn, cebrown 9/28/00
					; note that after dissolve a conn, there may be multiple copies of lits in jform
(defun dissolve-jform (conn jform)
  (when mating-verbose (msgf "Dissolving conn: " conn))
  (let ((a (catch 'false (dissolve-jform-rec conn jform))))
    (if (eq a 'false)
	(make-disjunction)
      a)))

(defun dissolve-jform-rec (conn jform)
;  (display-vp-diag jform)
  (case (jform-type jform)
    (disjunction 
     (dolist (j (disjunction-components jform) jform)
       (let ((l (find-all-leaf-names-in-jform j)))
	 (when (and (member (car conn) l)
		    (member (cdr conn) l))
	   (let ((a (catch 'false (dissolve-jform-rec conn j))))
	     (when (eq a 'false)
	       (setf (disjunction-components jform)
		 (remove j (disjunction-components jform)))
	       (unless (disjunction-components jform)
		 (throw 'false 'false))))))))
    (conjunction 
     (let ((first-conjs nil)
	   (second-conjs nil)
	   (kids nil))
     (dolist (j (conjunction-components jform))
       (let ((l (find-all-leaf-names-in-jform j)))
	 (cond ((and (member (car conn) l)
		     (member (cdr conn) l))
		(push (dissolve-jform-rec conn j) kids))
	       ((member (car conn) l)
		(push j first-conjs))
	       ((member (cdr conn) l)
		(push j second-conjs))
	       (t
		(push j kids)))))
     (if (and first-conjs second-conjs)
	 (progn
       (when (cdr first-conjs)
	 (throwfail "Two copies of the literal " (car conn) " share a vertical path."))
       (when (cdr second-conjs)
	 (throwfail "Two copies of the literal " (cdr conn) " share a vertical path."))
       (let* ((ccj1 (dissolve-cc (car conn) (car first-conjs)))
	      (ccj2 (dissolve-cc (cdr conn) (car second-conjs)))
	      (cpej2 (if (and ccj1 ccj2) (dissolve-cpe (cdr conn) (car second-conjs)))))
;	 (msgf "ccj1 " ccj1)
;	 (msgf "ccj2 " ccj2)
;	 (msgf "cpej2 " cpej2)
	 (cond ((and ccj1 ccj2)
		(let* ((conj1 (make-conjunction :components (list (copy-jform (car first-conjs)) (car ccj2))))
		       (conj2 (make-conjunction :components (list (car ccj1) cpej2)))
		       (disj (make-disjunction :components (list conj1 conj2))))
		  (setf (jform-parent conj1) disj)
		  (setf (jform-parent conj2) disj)
		  (setf (jform-parent disj) jform)
		  (setf (conjunction-components jform)
		    (cons disj (set-difference (conjunction-components jform)
					       (list (car first-conjs) (car second-conjs)))))
		  jform))
	       (ccj1
		(let ((conj (make-conjunction :components (list (car ccj1) (copy-jform (car second-conjs))))))
		  (setf (jform-parent conj) jform)
		  (setf (conjunction-components jform)
		    (cons conj (set-difference (conjunction-components jform)
					       (list (car first-conjs) (car second-conjs)))))
		  jform))
	       (ccj2
		(let ((conj (make-conjunction :components (list (car first-conjs) (car ccj2)))))
		  (setf (jform-parent conj) jform)
		  (setf (conjunction-components jform)
		    (cons conj (set-difference (conjunction-components jform)
					       (list (car first-conjs) (car second-conjs)))))
		  jform))
	       (t 
		(throw 'false 'false)))))
       (progn
	 (setf (conjunction-components jform) kids)
	 jform))))
    (existential
     (setf (existential-scope jform) (dissolve-jform-rec conn (existential-scope jform)))
     jform)
    (universal
     (setf (universal-scope jform) (dissolve-jform-rec conn (universal-scope jform)))
     jform)
    (t jform)))

(defun dissolve-cpe (lit jform &optional (parent nil))
  (let ((ret nil))
    (case (jform-type jform)
      (disjunction
       (setq ret (make-disjunction :parent parent))
       (dolist (j (disjunction-components jform) ret)
	 (when (member lit (find-all-leaf-names-in-jform j))
	   (setf (disjunction-components ret)
	     (cons (dissolve-cpe lit j ret)
		   (disjunction-components ret))))))
      (conjunction
       (setq ret (make-conjunction :parent parent))
       (dolist (j (conjunction-components jform) ret)
	 (setf (conjunction-components ret)
	   (if (member lit (find-all-leaf-names-in-jform j))
	       (cons (dissolve-cpe lit j ret)
		     (conjunction-components ret))
	     (cons (copy-jform j) (conjunction-components ret)))))) ; really should reset parent of (copy-jform j)
      (literal
       (setq ret (copy-jform jform))
       (setf (jform-parent ret) parent)
       ret)
      (universal
       (setq ret (copy-jform jform))
       (setf (jform-parent ret) parent)
       (setf (universal-scope ret) (dissolve-cpe lit (universal-scope jform) ret))
       ret)
      (existential
       (setq ret (copy-jform jform))
       (setf (jform-parent ret) parent)
       (setf (existential-scope ret) (dissolve-cpe lit (existential-scope jform) ret))
       ret))))
      
(defun dissolve-cc (lit jform &optional (parent nil))
  (let ((ret nil))
    (case (jform-type jform)
      (disjunction
       (setq ret (make-disjunction :parent parent))
       (dolist (j (disjunction-components jform) (list ret))
	 (setf (disjunction-components ret)
	   (if (member lit (find-all-leaf-names-in-jform j))
	       (append (dissolve-cc lit j ret)
		       (disjunction-components ret))
	     (cons (copy-jform j) (disjunction-components ret)))))) ; really should reset parent of (copy-jform j)
      (conjunction
       (setq ret (make-conjunction :parent parent))
       (dolist (j (conjunction-components jform) (list ret))
	 (if (member lit (find-all-leaf-names-in-jform j))
	     (let ((c (dissolve-cc lit j ret)))
	       (if c
		   (setf (conjunction-components ret)
		     (append c (conjunction-components ret)))
		 (return nil)))
	   (setf (conjunction-components ret)
	     (cons (copy-jform j) (conjunction-components ret))))))
      (literal
       (if (eq (literal-name jform) lit)
	   nil
	 (list (copy-jform jform))))
      (universal
       (setq ret (copy-jform jform))
       (setf (jform-parent ret) parent)
       (let ((c (dissolve-cc lit (universal-scope jform) ret)))
	 (if c
	     (progn
	       (setf (universal-scope ret) (car c))
	       (list ret))
	   nil)))
      (existential
       (setq ret (copy-jform jform))
       (setf (jform-parent ret) parent)
       (let ((c (dissolve-cc lit (existential-scope jform) ret)))
	 (if c
	     (progn
	       (setf (existential-scope ret) (car c))
	       (list ret))
	   nil))))))

; returns normalized jform (no disj's as kids of disj's, etc) (also, no empty disj's or empty conj's, except possibly at the root)
(defun normalize-jform (jform)
  (case (jform-type jform)
    (disjunction 
     (let ((disjuncts (normalize-jform-dis jform)))
       (if (= (length disjuncts) 1) ; in particular, empty conj (TRUE) gets passed along
	   (car disjuncts)
	 (let ((j (make-disjunction :components disjuncts))) ; in particular, empty disj (FALSE) gets passed along
	   (dolist (d disjuncts j)
	     (setf (jform-parent d) j))))))
    (conjunction 
     (let ((conjuncts (normalize-jform-con jform)))
       (if (= (length conjuncts) 1) ; in particular, empty disj (FALSE) gets passed along
	   (car conjuncts)
	 (let ((j (make-conjunction :components conjuncts))) ; in particular, empty conj (TRUE) gets passed along
	   (dolist (c conjuncts j)
	     (setf (jform-parent c) j))))))
    (universal 
     (let* ((c (copy-jform jform))
	    (sc (normalize-jform (universal-scope c))))
       (if (or (and (conjunction-p sc)
		    (not (conjunction-components sc)))
	       (and (disjunction-p sc)
		    (not (disjunction-components sc))))
	   sc
	 (progn
	   (setf (universal-scope c) sc)
	   (setf (jform-parent sc) c)
	   c))))
    (existential
     (let* ((c (copy-jform jform))
	    (sc (normalize-jform (existential-scope c))))
       (if (or (and (conjunction-p sc)
		    (not (conjunction-components sc)))
	       (and (disjunction-p sc)
		    (not (disjunction-components sc))))
	   sc
	 (progn
	   (setf (existential-scope c) sc)
	   (setf (jform-parent sc) c)
	   c))))
    (t (copy-jform jform))))

					; returns a list of normalized jforms (none of which are disjunctions), intended as the disjunction
					; an empty conjunction (TRUE) can only be on the list if it is all of the list
(defun normalize-jform-dis (jform)
  (let* ((disjuncts
	 (if (disjunction-p jform)
	     (apply #'append
		    (mapcar #'normalize-jform-dis
			    (disjunction-components jform)))
	   (let ((j (normalize-jform jform)))
	     (if (disjunction-p j)
		 (disjunction-components j)
	       (list j)))))
	 (tr (find-if #'(lambda (x)
			  (and (conjunction-p x)
			       (not (conjunction-components x))))
		      disjuncts)))
    (if tr
	(list tr)
      disjuncts)))

					; returns a list of normalized jforms (none of which are conjunctions), intended as the conjunction
					; an empty disjunction (FALSE) can only be on the list if it is all of the list
(defun normalize-jform-con (jform)
  (let* ((conjuncts
	 (if (conjunction-p jform)
	     (apply #'append
		    (mapcar #'normalize-jform-con
			    (conjunction-components jform)))
	   (let ((j (normalize-jform jform)))
	     (if (conjunction-p j)
		 (conjunction-components j)
	       (list j)))))
	 (fa (find-if #'(lambda (x)
			  (and (disjunction-p x)
			       (not (disjunction-components x))))
		      conjuncts)))
    (if fa
	(list fa)
      conjuncts)))


(defun strip-exp-vars (gwff)
  "Replaces the exp-vars in gwff by their subst slots."
  (cond ((exp-var-p gwff) (strip-exp-vars (exp-var-subst gwff)))
        ((skolem-term-p gwff) 
         (strip-exp-vars (skolem-term-term gwff)))
        ((lsymbol-q gwff) gwff)
        ((boundwff-q gwff)
         (cons (car gwff) (strip-exp-vars (cdr gwff))))
        (t
         (cons (strip-exp-vars (car gwff))
               (strip-exp-vars (cdr gwff))))))

(defmateop cw
  (mate-result-> ignore)
  (matewff-argname gwff)
  (mate-alias cw-jform)
  (mhelp ""))

(defmateop cws
  (mate-result-> ignore)
  (matewff-argname gwff)
  (mate-alias cw-shallow)
  (mhelp ""))

(defmateop cwd
  (mate-result-> ignore)
  (matewff-argname gwff)
  (mate-alias cw-deep)
  (mhelp ""))

(defwffop cw-shallow
  (argtypes symbol gwff)
  (argnames label gwff)
  (applicable-p (lambda (label gwff)
		  (declare (ignore gwff))
		  (if (label-q label)
		      (progn (complain label " already represents "
				       ((get-weak label) . gwff))
			     nil)
		      t)))
  (resulttype gwff)
  (mhelp "Create a weak label from the shallow formula of an etree."))

(defun cw-shallow (label gwff)
  (putprop label 'weak 'flavor)
  (put-weak label (strip-sk-terms (strip-exp-vars (get-shallow gwff))))
  label)

(defwffop cw-deep
  (argtypes symbol gwff)
  (argnames label gwff)
  (applicable-p (lambda (label gwff)
		  (declare (ignore gwff))
		  (if (label-q label)
		      (progn (complain label " already represents "
				       ((get-weak label) . gwff))
			     nil)
		      t)))
  (resulttype gwff)
  (mhelp "Create a weak label from the deep formula of an etree."))

(defun cw-deep (label gwff)
  (putprop label 'weak 'flavor)
  (put-weak label (strip-sk-terms (strip-exp-vars (get-deep gwff))))
  label)

(defwffop cw-jform
  (argtypes symbol gwff)
  (argnames label gwff)
  (applicable-p (lambda (label gwff)
		  (declare (ignore gwff))
		  (if (label-q label)
		      (progn (complain label " already represents "
				       ((get-weak label) . gwff))
			     nil)
		      t)))
  (resulttype gwff)
  (mhelp "Create a weak label from the current jform representation
of an etree."))

(defun cw-jform (label gwff)
  (declare (ignore gwff))
  (cr-eproof-jform)
  (putprop label 'weak 'flavor)
  (put-weak label (jform-to-gwff (eproof-jform current-eproof)))
  label)

(defmateop cjform
  (mate-alias cr-eproof-jform)
  (mate-result-> ignore))

(defwffop cr-eproof-jform
  (applicable-q (lambda () (boundp 'current-eproof)))
  (mhelp "Create a new jform for the expansion tree associated with the
current mating-search top-level. You need to use this command only if
you modify the expansion tree interactively and you are constructing a
mating interactively."))

(defun cr-eproof-jform ()
  (setf (eproof-jform current-eproof)
	(etree-to-jform (eproof-etree current-eproof))))

(defmateop num-vpaths
  (mate-alias number-of-vertical-paths)
  (matewff-argname gwff)
  (mate-result-> princ))

(defmateop num-hpaths
  (mate-alias number-of-horizontal-paths)
  (matewff-argname gwff)
  (mate-result-> princ))

(defmateop vpd
  (mate-alias display-vpd)
  (mate-result-> ignore)
  (matewff-argname jform))

(defmateop vp
  (mate-alias display-vp-diag)
  (mate-result-> ignore)
  (matewff-argname jform))

(defmateop vpt
  (mate-alias vp-tex)
  (mate-result-> ignore)
  (matewff-argname jform))

(defmateop vpetree
  (mate-alias display-vp-etree)
  (mate-result-> ignore)
  (matewff-argname jform))

(defwffop display-vp-etree
  (argtypes)
  (argnames)
  (applicable-q (lambda () (eproof-jform current-eproof)))
  (resulttype ignore)
  (mhelp "Display the VP diagram of the ETREE as used in mating-search."))

(defun display-vp-etree ()
  (display-vp-diag (eproof-jform current-eproof)))

(defun find-jform-name (name jform)
   (case (jform-type jform)
    (literal
     (if (string= (literal-name jform) name) jform nil))
    (conjunction
     (dolist (jform (conjunction-components jform) nil)
       (let ((literal (find-jform-name name jform)))
         (if literal (return literal)))))
    (disjunction
     (dolist (jform (disjunction-components jform) nil)
       (let ((literal (find-jform-name name jform)))
         (if literal (return literal)))))
    (universal (find-jform-name name (universal-scope jform)))
    (t (throwfail (jform-type jform) " is illegal in jforms."))))


(defun apply-substs-to-jform (jform subst-stack)
  (case (jform-type jform)
    (literal
     (setf (jform-represents jform)
           (lambda-reduce (if (jform-pos jform) (jform-represents jform)
                              (cons 'not (jform-represents jform)))
                          subst-stack))
     (setf (jform-pos jform) t))
    (conjunction
     (dolist (jform (conjunction-components jform))
       (apply-substs-to-jform jform subst-stack)))
    (disjunction
     (dolist (jform (disjunction-components jform))
       (apply-substs-to-jform jform subst-stack)))
    (universal (apply-substs-to-jform (universal-scope jform) subst-stack))
    (t (throwfail (jform-type jform) " is illegal in jforms."))))

(context expansion-trees)

(defwffop modify-status
  (argtypes integer+ anything)
  (argnames status etree)
  (resulttype ignore)
  (mhelp "Set the status of the current-topnode to the specified
value. If the status of a node is not positive, it is ignored during
mating search."))

(defmateop mod-status
  (mate-alias modify-status)
  (mate-result-> ignore)
  (matewff-argname etree))

(defun modify-status (status label)
  (let* ((label (if (etree-q label) label
		    (find-etree-node-name label))))
    (update-status nil label status)
    ))

(defun find-literals-in-jform (pred j)
  (typecase j
	    (conjunction (apply #'append
				(mapcar #'(lambda (x)
					    (find-literals-in-jform pred x))
					(conjunction-components j))))
	    (disjunction (apply #'append
				(mapcar #'(lambda (x)
					    (find-literals-in-jform pred x))
					(disjunction-components j))))
	    (universal (find-literals-in-jform pred (universal-scope j)))
	    (existential (find-literals-in-jform pred (existential-scope j)))
	    (literal (if (funcall pred j) (list j) nil))))

(defun find-literal-in-jform (pred j)
  (typecase j
	    (conjunction (find-literal-in-jforms pred (conjunction-components j)))
	    (disjunction (find-literal-in-jforms pred (disjunction-components j)))
	    (universal (find-literal-in-jform pred (universal-scope j)))
	    (existential (find-literal-in-jform pred (existential-scope j)))
	    (literal (if (funcall pred j) j nil))))

(defun find-literal-in-jforms (pred jl)
  (if jl
      (or (find-literal-in-jform pred (car jl))
	  (find-literal-in-jforms pred (cdr jl)))
    nil))

(defun setvars-in-jform (j &optional evars)
  (typecase j
	    (universal
	     (setvars-in-jform (universal-scope j)
			       (append evars (universal-qvars j))))
	    (existential
	     (setvars-in-jform (existential-scope j) evars))
	    (conjunction
	     (let ((setvars nil))
	       (dolist (j1 (conjunction-components j) setvars)
		       (setq setvars (union setvars
					    (setvars-in-jform j1 evars))))))
	    (disjunction
	     (let ((setvars nil))
	       (dolist (j1 (disjunction-components j) setvars)
		       (setq setvars (union setvars
					    (setvars-in-jform j1 evars))))))
	    (literal
	     (let ((h (head (jform-represents j))))
	       (if (member h evars)
		   (list h)
		 nil)))))
