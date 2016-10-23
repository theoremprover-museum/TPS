;;; -*- Mode: Lisp -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile unif-tree
  (part-of unification)
  (extension lisp)
  (mhelp "Unification functions."))

;;;The substitution hash-table stores substitutions only for rigid terms with
;;constant heads.
;;;Each entry in the substitution hash-table is a list of elements. one of
;;;the elements is of the form (nodelist name1 name2 ... namei)
;;;The remaining elements are of the form:
;;;((name1 name2 ... namei . rhead) substs)

(context unification)

(eval-when (load compile eval)
  (defmacro imitation-p (subst)
    `(consp (subst-type ,subst))))

(defun neg-head (subst)
  (do ((term (subst-term subst) (cdr term)))
      ((symbolp (car term))
       (let ((head (head (cdr term))))
	 (or (cdr (assoc head (subst-new-h-vars subst))) head)))))

(defun create-successor (subst-stack notp parent)
  (declare (special imitation-h-var-list))
  (let* ((subst (cdar subst-stack))
	 (imitation-flag (and notp (imitation-p subst)))
	 (free-vars (remove (caar subst-stack) (node-free-vars parent))))
    (unless (and imitation-flag
		 (memq (caar subst-stack) (node-neg-vars parent)))
      (make-node :subst-stack subst-stack :parent parent
		 :depth (1+ (node-depth parent))
		 :neg-vars (if imitation-flag
			       (cons (neg-head subst) (node-neg-vars parent))
			       (node-neg-vars parent))
		 :free-vars (if (subst-new-h-vars subst)
				(nconc (mapcar #'cdr (subst-new-h-vars subst))
				       free-vars)
				(append (subst-h-vars subst) free-vars))
		 :dpairs (mapcar #'(lambda (pair)
				     (cons (copy-uni-term (car pair))
					   (copy-uni-term (cdr pair))))
				 (node-dpairs parent))
                 :only-imitation imitation-h-var-list))))

(defun assign-print-name (nodes parent)
  (let ((name (node-print-name parent)))
    (if (cdr nodes)
	(do ((i 1 (1+ i))
	     (nodes nodes (cdr nodes)))
	    ((null nodes))
	    (setf (node-print-name (car nodes)) (format nil "~A-~A" name i)))
      (setf (node-print-name (car nodes)) (format nil "~A-0" name)))))

(defun frpair-p (first 2nd free-vars)
  "Assumes that the first term in DPAIR is flexible."
  (if (uni-term-pos 2nd)
      (if (rigid-p 2nd free-vars)
	  (values T (uni-term-head first) (length (uni-term-args first))
		  (length (uni-term-binder first)) (uni-term-head 2nd)
		  (length (uni-term-args 2nd)) (uni-term-binder 2nd)
		  (uni-term-bdvars 2nd)))
      (values T (uni-term-head first) (length (uni-term-args first))
	      (length (uni-term-binder first)) 'not 1
	      (uni-term-binder 2nd) (uni-term-bdvars 2nd) 
	      (if (memq (uni-term-head 2nd) free-vars)
		  (cons nil (uni-term-head 2nd))
		(cons T (uni-term-head 2nd))))))

(defun apply-match-max-substs (node subst-hash-table depth quickp)
  "This function applies MATCH to whichever head variable is the 
most likely to cause a failure by MAX-SUBSTS-VAR."
  (declare (special ck-subsumed))
  (let ((name (node-print-name node))
	(free-vars (node-free-vars node))
	(hash-entry nil)
	(subst nil)
	(fhead nil)
	(rhead nil)
	(curr-max 0)
	(curr-max-temp 0))
    (multiple-value-bind (topvars topvars-proj hvars ptotal)
			 (check-simpl-count-2 (node-subst-stack node))
    (if (and (neq quickp t) 
	     ;if quickp is T then we don't return a max-substs fail. If it's 99, we're
	     ;doing a quick search but MAX-SUBSTS failures are allowed. If it's NIL, we're on full search.
	     (or (and max-substs-var (> (reduce 'max (mapcar 'cdr topvars)) max-substs-var))
		 (and max-substs-proj (> (reduce 'max (mapcar 'cdr topvars-proj)) max-substs-proj))
		 (and max-substs-proj-total (> ptotal max-substs-proj-total))))
	(values nil nil 'fail)
      (progn
	(when (dolist (dpair (node-dpairs node) T)
	   (multiple-value-bind (frflag flexible-head p1 n1 rigid-head p2
					rigid-term-binder bdvars bdargs)
	     (frpair-p (car dpair) (cdr dpair) free-vars)
	     (when (neq quickp t)
		;if quickp is T then we don't return a max-substs fail. If it's 99, we're
	        ;doing a quick search but MAX-SUBSTS failures are allowed. If it's NIL, we're on full search.
		   (setq curr-max-temp (or (cdr (assoc (cdr (assoc flexible-head hvars)) topvars)) 0));curr-max))
		   (when (= curr-max-temp max-substs-var) (values nil nil 'fail)))
	     (when (and frflag 
			(>= curr-max-temp curr-max))
		   (multiple-value-bind (substitutions entry)
					(match-top
					 flexible-head (type flexible-head) p1 n1
					 rigid-head p2 (length rigid-term-binder)
					 rigid-term-binder bdvars subst-hash-table name bdargs)
		     (if substitutions
		      (cond ((not subst)
			     (setq hash-entry entry
				   subst substitutions
				   fhead flexible-head
				   rhead rigid-head
				   curr-max curr-max-temp)
			     (unless (cdr substitutions) (return t)))
			    ((or (> curr-max-temp curr-max)
				 (and (= curr-max-temp curr-max) (< (length substitutions) (length subst))))
			     (setq hash-entry entry
				   subst substitutions
				   fhead flexible-head
				   rhead rigid-head
				   curr-max curr-max-temp)
			     (unless (cdr substitutions) (return t))))
		       (return nil))))))
	      (unless (and quickp (and min-quick-depth (> depth min-quick-depth)) (cdr subst))
		      (if (caar hash-entry) (push name (car hash-entry))
			(if hash-entry (rplaca (car hash-entry) name))))
	      (values fhead subst (eq rhead 'not))))))))

(defun apply-match-all-frdpairs-msv (node subst-hash-table depth quickp)
  "APPLY-MATCH-ALL-FRDPAIRS with an additional MAX-SUBSTS-VAR checker"
  (declare (special ck-subsumed))
  (let ((name (node-print-name node))
	(free-vars (node-free-vars node))
	(hash-entry nil)
	(subst nil)
	(fhead nil)
	(rhead nil))
    (multiple-value-bind (topvars topvars-proj hvars ptotal)
			 (check-simpl-count-2 (node-subst-stack node))
    (if (and (neq quickp t) 
	     ;if quickp is T then we don't return a max-substs fail. If it's 99, we're
	     ;doing a quick search but MAX-SUBSTS failures are allowed. If it's NIL, we're on full search.
	     (or (and max-substs-var (> (reduce 'max (mapcar 'cdr topvars)) max-substs-var))
		 (and max-substs-proj (> (reduce 'max (mapcar 'cdr topvars-proj)) max-substs-proj))
		 (and max-substs-proj-total (> ptotal max-substs-proj-total))))
	(values nil nil 'fail)
      (progn
	(when (dolist (dpair (node-dpairs node) T)
	   (multiple-value-bind (frflag flexible-head p1 n1 rigid-head p2
					rigid-term-binder bdvars bdargs)
	     (frpair-p (car dpair) (cdr dpair) free-vars)
	     (when (neq quickp t)
		;if quickp is T then we don't return a max-substs fail. If it's 99, we're
	        ;doing a quick search but MAX-SUBSTS failures are allowed. If it's NIL, we're on full search.
		   (when (or (= max-substs-var (or (cdr (assoc (cdr (assoc flexible-head hvars)) topvars)) 0))
			     (= max-substs-proj (or (cdr (assoc (cdr (assoc flexible-head hvars)) topvars-proj)) 0)))
			 (values nil nil 'fail)))
	     (when frflag 
		   (multiple-value-bind (substitutions entry)
					(match-top
					 flexible-head (type flexible-head) p1 n1
					 rigid-head p2 (length rigid-term-binder)
					 rigid-term-binder bdvars subst-hash-table name bdargs)
		     (if substitutions
		      (cond ((not subst)
			     (setq hash-entry entry
				   subst substitutions
				   fhead flexible-head
				   rhead rigid-head)
			     (unless (cdr substitutions) (return t)))
			    ((< (length substitutions) (length subst))
			     (setq hash-entry entry
				   subst substitutions
				   fhead flexible-head
				   rhead rigid-head)
			     (unless (cdr substitutions) (return t))))
		       (return nil))))))
	      (unless (and quickp (and min-quick-depth (> depth min-quick-depth)) (cdr subst))
		      (if (caar hash-entry) (push name (car hash-entry))
			(if hash-entry (rplaca (car hash-entry) name))))
	      (values fhead subst (eq rhead 'not))))))))

(defun apply-match-min-substs (node subst-hash-table depth quickp)
  "This function applies MATCH to whichever head variable is the 
least likely to cause a failure by MAX-SUBSTS-VAR."
  (declare (special ck-subsumed))
  (let ((name (node-print-name node))
	(free-vars (node-free-vars node))
	(hash-entry nil)
	(subst nil)
	(fhead nil)
	(rhead nil)
	(curr-min 10000)
	(curr-min-temp 10000))
    (multiple-value-bind (topvars topvars-proj hvars ptotal)
			  (check-simpl-count-2 (node-subst-stack node))
    (if (or (and max-substs-var (> (reduce 'max (mapcar 'cdr topvars)) max-substs-var))
	    (and max-substs-proj (> (reduce 'max (mapcar 'cdr topvars-proj)) max-substs-proj))
	    (and max-substs-proj-total (> ptotal max-substs-proj-total)))
	(values nil nil nil)
      (progn
	(when (dolist (dpair (node-dpairs node) T)
	   (multiple-value-bind (frflag flexible-head p1 n1 rigid-head p2
					rigid-term-binder bdvars bdargs)
	     (frpair-p (car dpair) (cdr dpair) free-vars)
	     (setq curr-min-temp (or (cdr (assoc (cdr (assoc flexible-head hvars)) topvars)) 0))
	     (when (= curr-min-temp max-substs-var) (return nil))
	     (when (and frflag 
			(<= curr-min-temp curr-min))
		   (multiple-value-bind (substitutions entry)
					(match-top
					 flexible-head (type flexible-head) p1 n1
					 rigid-head p2 (length rigid-term-binder)
					 rigid-term-binder bdvars subst-hash-table name bdargs)
		     (if substitutions
		      (cond ((not subst)
			     (setq hash-entry entry
				   subst substitutions
				   fhead flexible-head
				   rhead rigid-head
				   curr-min curr-min-temp)
			     (unless (cdr substitutions) (return t)))
			    ((or (< curr-min-temp curr-min)
				 (and (= curr-min-temp curr-min) (< (length substitutions) (length subst))))
			     (setq hash-entry entry
				   subst substitutions
				   fhead flexible-head
				   rhead rigid-head
				   curr-min curr-min-temp)
			     (unless (cdr substitutions) (return t))))
		       (return nil))))))
      (unless (and quickp (and min-quick-depth (> depth min-quick-depth)) (cdr subst))
	(if (caar hash-entry) (push name (car hash-entry))
	    (if hash-entry (rplaca (car hash-entry) name))))
      (values fhead subst (eq rhead 'not))))))))

(defun check-simpl-count-2 (substs)
  (let ((topvars '((0 . 0)))
	(hvars nil)
	(topvars-proj '((0 . 0)))
	(proj-total 0)
	(pcount (or max-substs-proj max-substs-proj-total))
	(dummy nil))
    (dolist (sub (reverse substs))
	    (if (subst-p (cdr sub))
		(progn
		  (setq dummy (cdr (assoc (car sub) hvars)))
		  (if dummy
		      (progn 
			(incf (cdr (assoc dummy topvars)))
			(when (and (integerp (subst-type (cdr sub))) pcount)
			      (incf (cdr (assoc dummy topvars-proj)))
			      (incf proj-total))
			(setq hvars (append (mapcar #'(lambda (x) (cons x dummy))
						    (mapcar #'(lambda (x) (or (cdr (assoc x (subst-new-h-vars (cdr sub))))
									      x)) (subst-h-vars (cdr sub)))) 
					    hvars)))
		    (progn
		      (setq topvars (cons (cons (car sub) 1) topvars))
		      (when pcount
			    (if (integerp (subst-type (cdr sub))) 
				(progn (incf proj-total) (setq topvars-proj (cons (cons (car sub) 1) topvars-proj)))
			      (setq topvars-proj (cons (cons (car sub) 0) topvars-proj))))
		      (setq hvars (append (mapcar #'(lambda (x) (cons x (car sub)))
						  (mapcar #'(lambda (x) (or (cdr (assoc x (subst-new-h-vars (cdr sub))))
									    x)) (subst-h-vars (cdr sub))))
					  hvars)))))))
    (values topvars topvars-proj hvars proj-total)))

(defun apply-match-all-frdpairs (node subst-hash-table depth quickp)
  "This function applies MATCH to all f-r dpairs, eventually picking out
   the one with the fewest substitutions. However, if there's
   a unique substitution for some dpair, it stops there."
  (declare (special ck-subsumed))
  (let ((name (node-print-name node))
	(free-vars (node-free-vars node))
	(hash-entry nil)
	(subst nil)
	(fhead nil)
	(rhead nil))
    (when (dolist (dpair (node-dpairs node) T)
	    (multiple-value-bind (frflag flexible-head p1 n1 rigid-head p2
					 rigid-term-binder bdvars bdargs)
		(frpair-p (car dpair) (cdr dpair) free-vars)
	      (when frflag
		(multiple-value-bind (substitutions entry)
		    (match-top
                     flexible-head (type flexible-head) p1 n1
                     rigid-head p2 (length rigid-term-binder)
                     rigid-term-binder bdvars subst-hash-table name bdargs)
		  (if substitutions
		      (cond ((not subst)
			     (setq hash-entry entry
				   subst substitutions
				   fhead flexible-head
				   rhead rigid-head)
			     (unless (cdr substitutions) (return t)))
			    ((< (length substitutions) (length subst))
			     (setq hash-entry entry
				   subst substitutions
				   fhead flexible-head
				   rhead rigid-head)
			     (unless (cdr substitutions) (return t))))
		      (return nil))))))
	  ;;; (cdr subst) means |substs| > 1
	  (unless (and quickp (and min-quick-depth (> depth min-quick-depth)) (cdr subst))
		  (if (caar hash-entry) (push name (car hash-entry))
		    (if hash-entry (rplaca (car hash-entry) name))))
	  (values fhead subst (eq rhead 'not)))))

(defun apply-match-most-consts (node subst-hash-table depth quickp)
  "This function applies MATCH to all dpairs, eventually picking out
   the one with the most constant symbols. However, if there's
   a unique substitution for some dpair, it stops there."
  (declare (special ck-subsumed))
  (let ((name (node-print-name node))
	(free-vars (node-free-vars node))
	(hash-entry nil)
	(consts 1000000)
	(consts2 nil)
	(subst nil)
	(fhead nil)
	(rhead nil))
    (when (dolist (dpair (node-dpairs node) T)
	    (multiple-value-bind (frflag flexible-head p1 n1 rigid-head p2
					 rigid-term-binder bdvars bdargs)
		(frpair-p (car dpair) (cdr dpair) free-vars)
	      (when frflag
		 (setq consts2 (free-vars2 (cons (uni-term-args (car dpair)) (uni-term-args (cdr dpair))) free-vars))
		 (when (or (not subst) (<= consts consts2))
		       (multiple-value-bind (substitutions entry)
				      (match-top
				       flexible-head (type flexible-head) p1 n1
				       rigid-head p2 (length rigid-term-binder)
				       rigid-term-binder bdvars subst-hash-table name bdargs)
			    (if substitutions
			    (cond ((not subst) ;this is the first dpair
				   (setq hash-entry entry
					 subst substitutions
					 consts consts2
					 fhead flexible-head
					 rhead rigid-head)
				   (unless (cdr substitutions) (return t)))
				  ((or (< consts consts2)
				       (< (length substitutions) (length subst)))
				   (setq hash-entry entry
					 subst substitutions
					 consts consts2
					 fhead flexible-head
					 rhead rigid-head)
				   (unless (cdr substitutions) (return t)))
				  )
		      (return nil)))))))
	  ;;; (cdr subst) means |substs| > 1
      (unless (and quickp (and min-quick-depth (> depth min-quick-depth)) (cdr subst))
	(if (caar hash-entry) (push name (car hash-entry))
	    (if hash-entry (rplaca (car hash-entry) name))))
      (values fhead subst (eq rhead 'not)))))

(defflag leibniz-sub-check
  (flagtype boolean)
  (default nil)
  (subjects unification transmit)
  (relevancy-preconditions
   (max-substs-var (and max-substs-var max-substs-quick))
   (max-substs-quick (and max-substs-var max-substs-quick)))
  (irrelevancy-preconditions
   (max-substs-var (not (and max-substs-var max-substs-quick)))
   (max-substs-quick (not (and max-substs-var max-substs-quick))))
  (mhelp "When T, check substitutions which are made for Leibniz
variables, to ensure that they are relevant in their first argument.
When NIL, don't do this."))

(defun apply-match-all-frdpairs-msq (node subst-hash-table quickp)
  "This function applies MATCH to all f-r dpairs, eventually picking out
   the one with the fewest substitutions. However, if there's
   a unique substitution for some dpair, it stops there."
  (declare (special ck-subsumed))
  (let ((name (node-print-name node))
	(free-vars (node-free-vars node))
	(hash-entry nil)
	(subst nil)
	(fhead nil)
	(rhead nil)
	(mff nil)
	(mff2 nil)
	(branch (zerop max-substs-quick))
	(dummy nil))
    (multiple-value-bind (topvars hvars)
			 (if (node-msv node)
			     (values (car (node-msv node))
				     (caddr (node-msv node)))
			   (multiple-value-bind (topvars topvars-proj hvars ptotal)
			       (check-simpl-count-2 (node-subst-stack node))
			     (setf (node-msv node) (list topvars topvars-proj hvars ptotal))
			     (values (car (node-msv node))
				     (caddr (node-msv node)))))
      (dolist (dpair (node-dpairs node))
	      (setq mff2 nil)
	      (multiple-value-bind (frflag flexible-head p1 n1 rigid-head p2
					   rigid-term-binder bdvars bdargs)
		(frpair-p (car dpair) (cdr dpair) free-vars)
		(setq dummy (cdr (assoc flexible-head hvars)))
		(when frflag
		      ; if MSQ is 0, MSV will always be the "real" value
		      ; otherwise if MSQ>0, MSV will be (if quickp MSQ MSV)
		      (when (and dummy max-substs-var (<= max-substs-var (cdr (assoc dummy topvars))))
			    ;we already have enough substitutions for this variable...
			    (if quickp
				(setq mff t mff2 t) ;so find another variable if we're doing quick unif 
			      (return-from apply-match-all-frdpairs-msq (values nil nil nil t))))
		      (when (or (null mff2) branch) ;i.e. if MSQ=0 or we don't have enough subs., then carry on.
			(multiple-value-bind (substitutions entry)
			  (match-top flexible-head (type flexible-head) p1 n1
				     rigid-head p2 (length rigid-term-binder)
				     rigid-term-binder bdvars subst-hash-table name bdargs)
			  (if substitutions
			      (cond ((not subst)
				     (setq hash-entry entry   subst substitutions
					   fhead flexible-head  rhead rigid-head)
				     (unless (cdr substitutions) (return t)))
				    ((< (length substitutions) (length subst))
				     (setq hash-entry entry
					   subst substitutions
					   fhead flexible-head
					   rhead rigid-head)
				     (unless (cdr substitutions) (return t))))
			    (return nil)))))))
	  ;;; (cdr subst) means |substs| > 1
      (if subst
	  (progn
	    (if (caar hash-entry) (push name (car hash-entry))
	      (if hash-entry (rplaca (car hash-entry) name)))
	    (if (and (cdr subst) quickp branch) ;if MSQ=0 and we got >1 subst
		(values nil nil nil t)
	      (values fhead subst (eq rhead 'not))))
	(values nil nil nil mff)))))


(defun initialize-utree (dpairs &optional (free-vars nil) (subst-stack nil))
  (setren-counter h-var-prefix initial-value-h-var-counter)
  (setren-counter eta-var-prefix initial-value-eta-var-counter)
  (setq non-dneggable-vars nil)
  (values (make-node :parent nil :dpairs dpairs :print-name "0"
		     :measure initial-measure-root :depth 0
		     :subst-stack subst-stack :free-vars free-vars)
	  (make-hash-table :test #'equal)))

(defvar assign-measure 'assign-measure-breadth-first)
(defvar update-measure-int-node 'update-measure-breadth-first-int-node)
(defvar update-measure 'update-measure-breadth-first)
(defvar find-next-node 'find-next-node-breadth-first)
(defvar tn-measure very-small-number)

(defflag apply-match
  (flagtype symbol)
  (default apply-match-all-frdpairs)
  (subjects unification transmit)
  (irrelevancy-preconditions
   (max-substs-var (and max-substs-var max-substs-quick))
   (max-substs-quick (and max-substs-var max-substs-quick))
   (default-ms (not (member default-ms '(MS88 MS89 MS91-6))))
   (ms-split (not ms-split)))
  (relevancy-preconditions
   (max-substs-var (and (not (and max-substs-var max-substs-quick))
			ms-split (member default-ms '(MS88 MS89 MS91-6))))
   (max-substs-quick (and (not (and max-substs-var max-substs-quick))
			ms-split (member default-ms '(MS88 MS89 MS91-6))))
   (ms-split (and (not (and max-substs-var max-substs-quick))
			ms-split (member default-ms '(MS88 MS89 MS91-6))))
   (default-ms (and (not (and max-substs-var max-substs-quick))
		    ms-split (member default-ms '(MS88 MS89 MS91-6)))))
  (mhelp "Heuristic to decide the pair that should be given to match.
UN88 procedures:
APPLY-MATCH-ALL-FRDPAIRS applies match to all flexible-rigid pairs 
   and chooses whichever will have fewest substitutions.
APPLY-MATCH-ALL-FRDPAIRS-MSV does the same, but also checks for
   MAX-SUBSTS-VAR violations at the same time.
APPLY-MATCH-MAX-SUBSTS applies match to whichever flexible-rigid
   pair is closest to exceeding the bound in MAX-SUBSTS-VAR.
   If it finds one with a unique substitution, it uses that.
APPLY-MATCH-MIN-SUBSTS is like the above, but chooses the pair
   which is farthest from the MAX-SUBSTS-VAR bound.
APPLY-MATCH-MOST-CONSTS applies match to whichever flex-rigid
   pair contains the most constant symbols.
(The last two of these are all but useless; both of the SUBSTS
versions will be disastrous if MAX-SUBSTS-VAR is NIL...)

UN90 procedures:
This flag is almost always ignored (the default behaviour is
much like APPLY-MATCH-ALL-FRDPAIRS, but see NUM-FRPAIRS and
COUNTSUBS-FIRST for more details). The exception is if it is
APPLY-MATCH-MAX-SUBSTS, in which case it will go for whichever
pair is closest to exceeding the MAX-SUBSTS-VAR bound (but will
still use NUM-FRPAIRS and COUNTSUBS-FIRST)."))

(definfo apply-match-all-frdpairs
  (mhelp "A setting for APPLY-MATCH.
In unification search, applies match to all flexible-rigid pairs 
and chooses whichever will have fewest substitutions."))

(definfo apply-match-all-frdpairs-msv
  (mhelp "A setting for APPLY-MATCH.
As for APPLY-MATCH-ALL-FRDPAIRS, but also checks for
MAX-SUBSTS-VAR violations at the same time. This is obsolete,
and is ignored by path-focused procedures."))

(definfo apply-match-max-substs
  (mhelp "A setting for APPLY-MATCH.
In unification search, applies match to whichever flexible-rigid
pair is closest to exceeding the bound in MAX-SUBSTS-VAR.
If it finds one with a unique substitution, it uses that."))

(definfo apply-match-min-substs
  (mhelp "A setting for APPLY-MATCH.
The opposite of APPLY-MATCH-MAX-SUBSTS: chooses the pair
which is farthest from the MAX-SUBSTS-VAR bound. This only
works for non-path-focused procedures, and should be deleted
someday because it's useless."))

(definfo apply-match-most-consts
  (mhelp "A setting for APPLY-MATCH.
In unification search, applies match to whichever flex-rigid
pair contains the most constant symbols. This only
works for non-path-focused procedures, and should be deleted
someday because it's useless."))

(defflag uni-search-heuristic
  (flagtype symbol)
  (default breadth-first)
  (change-fn change-search-heuristic)
  (subjects unification transmit)
  (mhelp "Search strategy used to select the next node in the unification tree.
BREADTH-FIRST and DEPTH-FIRST are exactly as they sound;
BEST-FIRST takes whichever leaf node has the fewest free
variables (and is not already terminal).
All of these options work for UN90 (ms90-*, ms91-7, ms92-*);
BREADTH-FIRST and BEST-FIRST are the only options for UN88 (ms88, ms89, 
ms91-6, mtree)."))

(definfo breadth-first
  (mhelp "A setting for UNI-SEARCH-HEURISTIC.
Search the unification tree breadth-first."))

(definfo depth-first
  (mhelp "A setting for UNI-SEARCH-HEURISTIC.
Search the unification tree depth-first, for path-focused procedures.
(There is no reason for this, and you should avoid doing it.)"))

(definfo best-first
  (mhelp "A setting for UNI-SEARCH-HEURISTIC.
Search the unification tree best-first (take whichever leaf node
has the fewest free variables). BREADTH-FIRST is faster than this."))

(defvar *least-search-depth* most-positive-fixnum)

(defun unify (root-node subst-hash-table &optional (quickp nil) (start-time nil) (time-limit nil))
  (runcount 'unification)
  (multiple-value-bind (a b c d)
		       (if (and max-substs-var max-substs-quick) 
			   (unify-msv root-node subst-hash-table quickp start-time time-limit)
			 (unify-old root-node subst-hash-table quickp start-time time-limit))
		       (breakcount 'unification)
		       (values a b c d)))

(defun unify-old (root-node subst-hash-table &optional (quickp nil) (start-time nil) (time-limit nil))
  "If QUICKP is true or 99, generates tree till it branches at a depth greater than
       MIN-QUICK-DEPTH. (99 means MAX-SUBSTS failures are allowed, T means they aren't.)"
  (declare (special ms88-unif-counter))
  (when (eq unif-trigger 'utree-begin) 
	(unif-extra-output root-node root-node))
  (multiple-value-bind (next-node depth)
      (funcall find-next-node root-node 1)
    (do ((next-node next-node)
	 (unifiable-p nil)
	 (depth depth)
	 (ck-subsumed nil nil))
	((or (null next-node) 
	     (and max-utree-depth (> depth max-utree-depth))
	     (and start-time time-limit
		  (>= (/ (- (get-net-internal-run-time) start-time)
			 internal-time-units-per-second)
		      time-limit)))
	 (progn
	 (when (eq unif-trigger 'utree-end1) (unif-extra-output (if next-node next-node root-node) root-node))
	 (if (or (null next-node) (and max-utree-depth (> depth max-utree-depth)))
	     (progn
	       (if (and (eq unify-verbose 'max) 
			(not (null next-node)) (and max-utree-depth (> depth max-utree-depth))) 
		   (msg "u")) ; (msgf "Current utree depth of " depth " is over MAX-UTREE-DEPTH." t))
	       (if unifiable-p (progn (when (eq unif-trigger 'utree-end)
					    (unif-extra-output (if next-node next-node root-node) root-node))
				      (values 'success root-node subst-hash-table
					      unifiable-p))
		 (if next-node (values 'more root-node subst-hash-table)
		   (progn (when (eq unif-trigger 'utree-end)
				(unif-extra-output root-node root-node))
			  (values 'fail)))))
	   (progn (when (eq unif-trigger 'utree-end)
			(unif-extra-output next-node root-node))
	   		 (return-from unify-old (values 'fail))))))
      (declare (special ck-subsumed))
      (when (memq unify-verbose '(med max)) (msg 2 (node-print-name next-node)))
      (multiple-value-bind
       (terminal-flag subst-stack dpairs free-vars progress-count)
       (simpl (node-dpairs next-node)
	      (node-subst-stack next-node) (node-free-vars next-node) 0)
       (declare (ignore progress-count))
       (when (and (not (memq apply-match '(apply-match-max-substs apply-match-all-frdpairs-msv)))
		  (or max-substs-var max-substs-proj max-substs-proj-total) 
		  (neq quickp t)
		  (simpl-subst-check subst-stack)) 
	     (setq terminal-flag 'fail)
	     (when (memq unify-verbose '(med max)) (msg "S")))
       (when (and (neq terminal-flag 'more)
		  (memq apply-match '(apply-match-max-substs apply-match-all-frdpairs-msv))
		  (neq quickp t)
		  (simpl-subst-check subst-stack)) 
	     (setq terminal-flag 'fail)
	     (when (memq unify-verbose '(med max)) (msg "S")))
       (setf (node-subst-stack next-node) subst-stack)
       (setf (node-free-vars next-node) free-vars)
       (setf (node-dpairs next-node) dpairs)
       (incf ms88-unif-counter)
       (if (eq terminal-flag 'more)
	   (multiple-value-bind (var substitutions flag)
				(funcall apply-match next-node subst-hash-table depth quickp)
				;;FLAG is T if RHEAD is NOT.
				(when (eq flag 'fail) ;then we called APPLY-MATCH-MAX-SUBSTS and we have a MAX-SUBSTS fail.
				      (setq terminal-flag 'fail)
				      (setf (node-terminal-flag next-node) terminal-flag)
				      (setf (node-measure next-node) TN-measure)
				      (funcall update-measure next-node)
				      (when (memq unify-verbose '(med max)) (msg "S"))
				      (setq flag nil))
				(unless (eq terminal-flag 'fail)
				(if (and quickp substitutions (cdr substitutions)
					 (> depth min-quick-depth))
				    (return (values T root-node subst-hash-table))
				  (let ((sons (mapcar #'(lambda (substitution)
							  (create-successor
							   (acons var substitution
								  subst-stack)
							   flag next-node))
						      substitutions)))
				    (setq sons (delete-if #'null sons))
				    (if sons
					(progn
					  (assign-print-name sons next-node)
					  (setf (node-sons next-node) sons)
					  (funcall assign-measure sons
						   (node-measure next-node) (length sons)
						   depth))
				      (progn
					(setf (node-terminal-flag next-node) 'fail)
					(setf (node-measure next-node) TN-measure)))
				    (funcall update-measure next-node)))))
	 (if (and quickp (eq terminal-flag 'success))
	     (progn (when (eq unif-trigger 'utree-end1)
			  (unif-extra-output next-node root-node))
		    (return-from
		     unify-old (values 'success root-node subst-hash-table
				   (list next-node))))
	   (progn (setf (node-terminal-flag next-node) terminal-flag)
		  (setf (node-measure next-node) TN-measure)
		  (funcall update-measure next-node)
		  (when (eq terminal-flag 'success)
			(unless quickp
				(setf (node-subst-stack next-node)
				      (nconc (find-subs-flexible-pairs
					      dpairs subst-stack)
					     (node-subst-stack next-node)))
				(setq *least-search-depth* depth)
				(when (or (eq unif-trigger 'utree-end) (eq unif-trigger 'utree-end1))
				      (unif-extra-output next-node root-node))
				(print-subst-stack (node-subst-stack next-node)))
			(if stop-at-TSN
			    (return-from
			     unify-old
			     (values 'success root-node
				     subst-hash-table (list next-node)))
			  (push next-node unifiable-p)))))))
      (when (and (< 0 unif-counter) (= 0 (mod ms88-unif-counter unif-counter)))
	    (unif-extra-output next-node root-node))
      (when (and (eq unif-trigger 'props-change) next-node (node-parent next-node)
		 (not (string= (show-dpairs-3 next-node) (show-dpairs-3 (node-parent next-node)))))
	    (unif-extra-output (node-parent next-node) root-node "----Previous unif node----")
	    (unif-extra-output next-node root-node "----Current unif node----" "----End of unif nodes----" ))
      (dolist (node (node-sons next-node))
	      (subsumption-check root-node node ck-subsumed subst-hash-table start-time time-limit))
      (multiple-value-setq (next-node depth)
	  (funcall find-next-node root-node 1)))))

;;;;;;***************BEGIN MATCH BOOKKEEPING***************

(eval-when (load compile eval)
	   (defmacro descendentp (name1 name2)
	     "Return T if name2 is a descendent of name1."
	     `(and (>= (length ,name2) (length ,name1))
		   (string= ,name1 ,name2 :end2 (length ,name1)))))

(defun match-top (fhead type-fhead p1 n1 rhead p2 n2 rterm-binder
                        bdvars hash-table name bdargs)
  (declare (special ck-subsumed))
  (let* ((key (list type-fhead (type rhead) p1 n1 p2 n2))
	 (subs (gethash key hash-table))
	 (rename nil))
    (if (memq rhead rterm-binder) (setq key nil))
    (when key
      (if subs (let ((nodelist (assoc 'nodelist subs)))
		 (unless (string= (car (last nodelist)) name)
		   (setq nodelist (nconc nodelist (list name)))
		   (push (cons name nodelist) ck-subsumed)))
	  (push (list 'nodelist name) (gethash key hash-table))))
    ;here we need to add something like: if it's a negation, and we've already negated it,
    ;then throw it away.... We also need to remove the current implementation of DNEG-IMITATION,
    ;which is incorrect.
    (if subs
	(let ((new-subs (dolist (sub subs nil)
			  (when (and (neq (car sub) 'nodelist)
				     (eq rhead (cdr (last (car sub)))))
			    (when (do ((names (car sub) (cdr names)))
				      ((atom names) T)
				    (when (descendentp (car names) name)
				      (return nil)))
			      (return sub))
			    (setq rename sub)))))
	  (if new-subs (values (cdr new-subs) new-subs)
	      (let* ((substs (if rename (match-rename (cdr rename))
				 (rename-head (match-rename (cdar subs))
					      rhead)))
		     (entry (acons nil rhead substs)))
		(push entry (gethash key hash-table))
		(values substs entry))))
	(let* ((substs (unif-match fhead (listify-type type-fhead) p1 n1
				   rhead p2 n2 rterm-binder bdvars bdargs))
	       (entry (when key (acons nil rhead substs))))
	  (push entry (gethash key hash-table))
	  (values substs entry)))))

(defun rename-head (subs head)
  (mapcar #'(lambda (sub)
	      (when (consp (subst-type sub))
		(unless (subst-h-vars sub)
		  (setq sub (copy-subst sub)))
		(setf (subst-type sub) (cons (car (subst-type sub)) head)))
	      sub)
	  subs))

(defun match-rename (subs)
  (let ((new-subs nil)
	(h-vars nil)
	new)
    (dolist (sub subs (nreverse new-subs))
      (multiple-value-setq (new h-vars)
			   (rename-h-vars sub h-vars))
      (push new new-subs))))

(defun rename-h-vars (subst new-h-vars)
  (when (subst-h-vars subst)
    (setq subst (copy-subst subst))
    (setf (subst-new-h-vars subst) nil)
    (dolist (h-var (subst-h-vars subst))
      (let ((found (assoc h-var new-h-vars)))
	(if found (push found (subst-new-h-vars subst))
	    (progn
	     (push (cons h-var (ren-var-uni-ho h-var-prefix (type h-var)))
		   new-h-vars)
	     (push (car new-h-vars) (subst-new-h-vars subst)))))))
  (values subst new-h-vars))

;;;;;;*************** END MATCH BOOKKEEPING ***************

(defflag pruning
  (subjects unification transmit)
  (flagtype boolean)
  (default nil)
  (mhelp "If T, the unification routine will prune the tree as it goes.
Only works for BREADTH-FIRST and BEST-FIRST unification, and
only then in MS88."))

;;;;;;***************BEGIN BREADTH FIRST***************

(defun update-measure-breadth-first (node)
  (if node
      (let ((measure (if (node-sons node)
			 (let ((sons (stable-sort (node-sons node) #'<
						  :key #'node-measure)))
			   (setf (node-sons node) sons)
			   (node-measure (car (last sons))))
			 (node-measure node))))
	(cond ((= measure TN-measure)
	       (unless (node-terminal-flag node)
		       (setf (node-terminal-flag node) T))
	       (if pruning (setf (node-sons node) nil))
	       (setf (node-measure node) measure)
	       (update-measure-breadth-first (node-parent node)))
	      ((< (node-measure node) measure)
	       (setf (node-measure node) measure)
	       (update-measure-breadth-first (node-parent node)))))))

(defun update-measure-breadth-first-int-node (node)
  (let ((measure (let ((sons (stable-sort (node-sons node) #'<
					  :key #'node-measure)))
		   (setf (node-sons node) sons)
		   (node-measure (car (last sons))))))
    (cond ((= measure TN-measure)
	   (unless (node-terminal-flag node)
	     (setf (node-terminal-flag node) T))
	   (if pruning (setf (node-sons node) nil))
	   (setf (node-measure node) measure))
	  ((< (node-measure node) measure)
	   (setf (node-measure node) measure)))))

(defun find-next-node-breadth-first (node depth)
  (declare (special imitation-h-var-list))
  (if (and (memq unify-verbose '(med max)) (and max-search-depth (> depth max-search-depth))) 
      (msg "s"));(msgf "Current utree depth is " depth "...which is over MAX-SEARCH-DEPTH." t))
  (if (and (not (and max-search-depth (> depth max-search-depth)))
	   (> (node-measure node) TN-measure))
      (let ((sons (node-sons node)))
	(if sons (dolist (son sons nil)
		   (multiple-value-bind (next-node depth)
                       (find-next-node-breadth-first
                        son (1+ depth))
		     (if next-node (return (values next-node depth)))))
            (progn
              (setq imitation-h-var-list (node-only-imitation node))
              (values node depth))))))

(defun assign-measure-breadth-first (sons measure number-of-sons depth)
  (declare (ignore number-of-sons depth))
  (dolist (son sons)
    (setf (node-measure son) (1+ measure))))

;;;;;;***************END BREADTH FIRST***************

;;;;;;***************BEGIN BEST FIRST***************

(defun update-measure-best-first (node)
  (if node
      (let ((measure (if (node-sons node)
			 (let ((sons (sort (node-sons node) #'<
					   :key #'node-measure)))
			   (setf (node-sons node) sons)
			   (node-measure (car sons)))
		       (node-measure node)))
	    (omeasure (node-measure node))
	    (pmeasure (if (node-parent node) (node-measure (node-parent node)) very-small-number)))
	(cond ((= measure TN-measure)
	       (unless (node-terminal-flag node)
		 (setf (node-terminal-flag node) T)
		 (if pruning (setf (node-sons node) nil)))
	       (setf (node-measure node) measure)
	       (update-measure-best-first (node-parent node)))
	      ((neq (node-measure node) measure)
	       (setf (node-measure node) measure)
	       (when (or (< measure pmeasure) (= omeasure pmeasure))
		     (update-measure-best-first (node-parent node))))))))

(defun update-measure-best-first-int-node (node)
  (let ((measure (let ((sons (sort (node-sons node) #'<
				   :key #'node-measure)))
		   (setf (node-sons node) sons)
		   (node-measure (car sons)))))
    (cond ((= measure TN-measure)
	   (unless (node-terminal-flag node)
		   (setf (node-terminal-flag node) T)
		   (if pruning (setf (node-sons node) nil)))
	   (setf (node-measure node) measure))
	  ((neq (node-measure node) measure)
	   (setf (node-measure node) measure)))))

(defun find-next-node-best-first (node depth)
  (declare (special imitation-h-var-list))
  (if (and (memq unify-verbose '(med max)) (and max-search-depth (> depth max-search-depth))) 
      (msg "s"))
  (if (and (not (and max-search-depth (> depth max-search-depth)))
	   (< (node-measure node) TN-measure))
      (let ((son (car (node-sons node))))
	(if son (multiple-value-bind (next-node depth)
				      (find-next-node-best-first
				       son (1+ depth))
				      (if next-node (values next-node depth)))
            (progn
              (setq imitation-h-var-list (node-only-imitation node))
              (values node depth))))))

(defun assign-measure-best-first (sons measure number-of-sons depth)
  (declare (ignore number-of-sons measure))
  (dolist (son sons)
    (setf (node-measure son) (if (and max-search-depth (>= depth max-search-depth))
				 TN-measure (length (node-free-vars son))))))

;;;;;;***************END BREADTH FIRST***************



(defun change-search-heuristic (flag-name new-value old-value)
  (declare (ignore flag-name old-value))
  (cond ((eq new-value 'breadth-first)
	 (setq TN-measure very-small-number
	       assign-measure 'assign-measure-breadth-first
	       update-measure 'update-measure-breadth-first
	       update-measure-int-node 'update-measure-breadth-first-int-node
	       find-next-node 'find-next-node-breadth-first))
	((eq new-value 'best-first)
	 (setq TN-measure very-large-number
	       assign-measure 'assign-measure-best-first
	       update-measure 'update-measure-best-first
	       update-measure-int-node 'update-measure-best-first-int-node
	       find-next-node 'find-next-node-best-first))
	((eq new-value 'depth-first)
	 ;the following applies to ms88 only, which has no depth-first search.
	 (msgf "Note: DEPTH-FIRST only works for path-focused procedures." t)
	 (setq TN-measure very-small-number
	       assign-measure 'assign-measure-breadth-first
	       update-measure 'update-measure-breadth-first
	       update-measure-int-node 'update-measure-breadth-first-int-node
	       find-next-node 'find-next-node-breadth-first))
	(T (complain "Unknown search heuristic. No action taken."))))

(defun free-vars2 (gwff bind-list)
  (cond ((lsymbol-q gwff)
	 (if (and (not (logconst-q gwff)) (propsym-q gwff)
		  (not (member gwff bind-list)))
	     1 0))
	((boundwff-q gwff) (free-vars2 (cdr gwff) (cons (caar gwff) bind-list)))
	(t (+ (free-vars2 (car gwff) bind-list) 
	      (free-vars2 (cdr gwff) bind-list)))))

(defun unif-extra-output (next root &optional (start-msg nil) (end-msg nil))
  (declare (special root-node current-topnode))
  (let ((root-node (or root (and (fboundp 'root-node) root-node)))
	(current-topnode (or root (and (fboundp 'current-topnode) current-topnode))))
    (setq next (or next root-node))
    (case unif-counter-output
	  (0 (msgf t (or start-msg "----Current unif tree----"))
	     (when root (print-dpairs (node-print-name root) "TTY:" t))
	     (msgf (or end-msg "----End of unif tree----") t))
	  (1 (msgf t (or start-msg "----Current unif tree----"))
	     (when root (print-dpairs (node-print-name root) "TTY:" nil))
	     (msgf (or end-msg "----End of unif tree----") t))
	  (2 (msgf t (or start-msg "----Current unif tree----"))
	     (when root (tp-utree (node-print-name root) t))
	     (msgf (or end-msg "----End of unif tree----") t))
	  (3 (msgf t (or start-msg "----Current unif tree----"))
	     (when root (tp-utree (node-print-name root) nil))
	     (msgf (or end-msg "----End of unif tree----") t))
	  (4 (msgf t (or start-msg "----Current unif tree----"))
	     (when root (print-utree (node-print-name root) "TTY:" t))
	     (msgf (or end-msg "----End of unif tree----") t))
	  (5 (msgf t (or start-msg "----Current unif tree----"))
	     (when root (print-utree (node-print-name root) "TTY:" nil))
	     (msgf (or end-msg "----End of unif tree----") t))
	  (6 (msgf t (or start-msg "----Current unif node----"))
	     (when next (show-unode (node-print-name next))
		   (show-dpairs-2 next))
	     (msgf (or end-msg "----End of unif node----") t))
	  (7 (msgf t (or start-msg "----Current unif node----"))
	     (when next (show-unode (node-print-name next))
		   (show-dpairs next))
	     (msgf (or end-msg "----End of unif node----") t))
	  (8 (msgf (or start-msg ""))
	     (when next
		   (msgf (node-print-name next) " : " 
			 (find-type (car (node-subst-stack next))) " : " (show-dpairs-3 next) t))
	     (msgf (or end-msg "")))
	  (9 (msgf (or start-msg "----") t)
	     (unif-stats)
	     (msgf (or end-msg "")))
	  (10 (unif-stats2)))))


(defun unify-msv (root subst-hash-table &optional (quickp nil) (start-time nil) (time-limit nil))
  (declare (special imitation-h-var-list))
;  (format t "UNIFY-MSV: imitation-h-var-list = ~d " imitation-h-var-list) ; cebrown
  (do ((success nil)
       (return-node nil)
       (ck-subsumed nil nil)
       (ignore-nodes nil)
       (msv-diff (and quickp (> max-substs-var max-substs-quick)))
       (max-substs-var (if (and quickp (not (zerop max-substs-quick)))
			   (min max-substs-quick max-substs-var) max-substs-var))
       ;(msv-real max-substs-var)
       (new-nodes nil nil))
      (success
       (progn 
	 (setf (node-sons root) (stable-sort (nconc ignore-nodes (node-sons root)) #'< :key #'node-depth))
	 (values success root subst-hash-table return-node)))
      (declare (special ck-subsumed))
      (unless (node-msv root)
	      (multiple-value-bind (topvars topvars-proj hvars ptotal)
				   (check-simpl-count-2 
				    (node-subst-stack root))
				   (setf (node-msv root) (list topvars topvars-proj hvars ptotal))))
      (unless (node-sons root)
	      (setf (node-sons root) (list (make-node :subst-stack (copy-list (node-subst-stack root))
						:depth 1
						:dpairs (mapcar #'copy-dpair (node-dpairs root))
						:print-name "0"
						:neg-vars (copy-list (node-neg-vars root))
						:free-vars (copy-list (node-free-vars root))
						:parent root
						:sons nil
						:msv (copy-list (node-msv root))
						:measure 1
						:only-imitation imitation-h-var-list))))
      (dolist (son (node-sons root))
	      (when (memq unify-verbose '(med max)) (msg 2 (node-print-name son)))
	      (let ((newsons nil))
		(if success (push son ignore-nodes)
		  (when (and (or (null max-search-depth) (and max-search-depth (<= (node-depth son) max-search-depth)))
			     (or (not leibniz-sub-check) (leibniz-subs-ok son))
			     (not (eq (node-terminal-flag son) 'fail))) ; cebrown 10/30/00 - so we don't follow a path that has already failed:
					; this was happening with MS98 on EXT1 (use mode MODE-EXT1-BUG)
			(setq imitation-h-var-list (node-only-imitation son))
			(multiple-value-bind
			 (terminal-flag subst-stack dpairs free-vars progress-count)
			 (simpl (node-dpairs son) (node-subst-stack son) (node-free-vars son) 0)
			 (declare (ignore progress-count))
			 (unless (or (eq terminal-flag 'fail)
				     (and (or (not msv-diff) max-substs-proj max-substs-proj-total)
					  (or (and max-substs-proj 
						   (cadr (node-msv son)) ; in case this is nil - cebrown 10/22/00 (example: EXT1 w/ ms98-1)
						   (< max-substs-proj 
						      (reduce #'max (mapcar #'cdr (cadr (node-msv son))))))
					      (and max-substs-proj-total 
						   (integerp (cadddr (node-msv son))) ; cebrown 10/22/00
						   (< max-substs-proj-total 
						      (cadddr (node-msv son)))))))
			   (setf (node-subst-stack son) subst-stack)
			   (setf (node-free-vars son) free-vars)
			   (setf (node-dpairs son) dpairs)
			   (if (eq terminal-flag 'more)
			       (multiple-value-bind 
				(var substitutions flag mff)
				(apply-match-all-frdpairs-msq son subst-hash-table msv-diff)
				(if mff
				    (when msv-diff
					(push son ignore-nodes) (setq success 'more))
				  (progn 
				    (setq newsons 
					  (delete-if #'null
						     (mapcar #'(lambda (substitution)
								 (create-successor-2 (acons var substitution subst-stack)
										     flag son))
							     substitutions)))
				    (dolist (ns newsons)
					    (setf (node-parent ns) root))
				    (when newsons (assign-print-name newsons son))
				    (setq new-nodes (nconc new-nodes newsons)))))
			     (if (eq terminal-flag 'success)
				 (progn (unless quickp 
						(setf (node-subst-stack son)
						      (nconc (find-subs-flexible-pairs
							      dpairs subst-stack)
							     (node-subst-stack son)))
						(setq *least-search-depth* (node-depth son))
						(print-subst-stack (node-subst-stack son)))
					(setq success 'success return-node (list son))
					(push son ignore-nodes)
					#+comment(unless quickp (return t)))
			       (throwfail "Huh?" t)))))))))
      (setf (node-sons root) new-nodes)
      (when (and time-limit
		 (>= (/ (- (get-net-internal-run-time) start-time) 
			internal-time-units-per-second)
		     time-limit))
	    (setq success 'more))
      (when (and (not success) (null new-nodes))
	    (if (null ignore-nodes)
		(setq success 'fail)
	      (if quickp (setq success t) (setq success 'more))))))

(defun leibniz-subs-ok (node)
  (dolist (lvar *leibniz-var-list* t)
	  (let* ((leib (car lvar))
		 (subs-so-far (cdr (assoc leib (car (node-msv node)))))
		 (lsub (and subs-so-far (lambda-reduce leib (node-subst-stack node)))))
	    (when lsub ;if it's NIL, we haven't substituted for this leibniz var at all
		  (unless (or (not (boundwff-q lsub))
			      (not (eq (cdar lsub) 'lambda))
			      (free-in (caar lsub) (cdr lsub)))
			  (when (memq unify-verbose '(med max)) (msg "L"))
			  (return nil))))))

(defun create-successor-2 (subst-stack notp parent)
  (declare (special imitation-h-var-list))
  (let* ((subst (cdar subst-stack))
	 (var (caar subst-stack))
	 (pcount (or max-substs-proj max-substs-proj-total))
	 (imitation-flag (and notp (imitation-p subst)))
	 (free-vars (remove (caar subst-stack) (node-free-vars parent)))
	 (topvars-c (copy-alist (car (node-msv parent))))
	 (topvars-proj-c (copy-alist (cadr (node-msv parent))))
	 (hvars-c (copy-alist (caddr (node-msv parent))))
	 (proj-total (cadddr (node-msv parent)))
	 (dummy (cdr (assoc var hvars-c))))
    (unless (and imitation-flag
		 (memq (caar subst-stack) (node-neg-vars parent)))
      (make-node :subst-stack subst-stack :parent parent
		 :depth (1+ (node-depth parent))
		 :neg-vars (if imitation-flag
			       (cons (neg-head subst) (node-neg-vars parent))
			       (node-neg-vars parent))
		 :free-vars (if (subst-new-h-vars subst)
				(nconc (mapcar #'cdr (subst-new-h-vars subst))
				       free-vars)
				(append (subst-h-vars subst) free-vars))
		 :dpairs (mapcar #'(lambda (pair)
				     (cons (copy-uni-term (car pair))
					   (copy-uni-term (cdr pair))))
				 (node-dpairs parent))
		 :msv  (if dummy
			   (progn (incf (cdr (assoc dummy topvars-c)))
				  (when (and pcount imitation-flag)
					(incf (cdr (assoc dummy topvars-proj-c)))
					(incf proj-total))
				  (setq hvars-c (nconc (mapcar #'(lambda (x) (cons x dummy))
					        (mapcar #'(lambda (x) (or (cdr (assoc x (subst-new-h-vars subst)))
									  x)) (subst-h-vars subst))) 
						       hvars-c))
				  (list topvars-c topvars-proj-c hvars-c proj-total))
			 (progn
			   (push (cons var 1) topvars-c)
			   (when pcount
				 (if imitation-flag
				     (progn (incf proj-total) 
					    (push (cons var 1) topvars-proj-c))
				   (push (cons var 0) topvars-proj-c)))
			   (setq hvars-c (nconc (mapcar #'(lambda (x) (cons x var))
					  (mapcar #'(lambda (x) (or (cdr (assoc x (subst-new-h-vars subst)))
								    x)) (subst-h-vars subst)))
						hvars-c))
			   (list topvars-c topvars-proj-c hvars-c proj-total)))
                 :only-imitation imitation-h-var-list))))
