;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(context ms98-1)

(deffile ms98-jform
  (part-of ms98)
  (extension lisp)
  (mhelp "Miscellaneous functions to handle jforms for MS98-1"))

(defun find-comps-touching (lits comps)
  (remove-if-not #'(lambda (x) (intersection (component-lits x) lits)) comps))

(defun fiddle-disjs (disj-list)
  (let ((count 0))
    (mapcar #'(lambda (x) (incf count) (cons count x)) (nreverse disj-list))))

(defun get-priority (dlist dup-reln)
  (if (cdr dlist)
      (let* ((elt (ubind (car (last (car dlist)))
			 (jform-to-gwff (car (last (car dlist)))))) ;elt is the literal we want to find copies of
	     (nl (remove-if-not #'(lambda (x) (wffeq-ab x elt)) dlist 
				:key #'(lambda (x) (ubind (car (last x)) (jform-to-gwff (car (last x)))))))
	     (newlist2 (remove-if #'(lambda (x) (wffeq-ab x elt)) dlist 
				  :key #'(lambda (x) (ubind (car (last x)) (jform-to-gwff (car (last x)))))))
	     (newlist (remove-if-not #'(lambda (x) (dup-rel-jforms dup-reln (car (last x)) (car (last (car dlist))))) nl))
	     (newlist3 (remove-if #'(lambda (x) (dup-rel-jforms dup-reln (car (last x)) (car (last (car dlist))))) nl)))
	(cons (sort (mapcar #'car newlist) #'<) (get-priority (append newlist3 newlist2) dup-reln)))
   (if (car dlist) (list (list (caar dlist))) nil)))

; cebrown 10/13/01
(defun dup-rel-jforms (dup-reln jf1 jf2)
  (cond ((eq jf1 jf2) t)
	((and (literal-p jf1) (literal-p jf2)
	      (let ((l1 (literal-name jf1))
		    (l2 (literal-name jf2)))
		(find-if #'(lambda (x)
			     (and (member l1 x)
				  (member l2 x)))
			 dup-reln)))
	 t)
	((and (conjunction-p jf1) (conjunction-p jf2))
	 (dup-rel-jforms-2 dup-reln
			   (conjunction-components jf1)
			   (conjunction-components jf2)))
	((and (disjunction-p jf1) (disjunction-p jf2))
	 (dup-rel-jforms-2 dup-reln
			   (disjunction-components jf1)
			   (disjunction-components jf2)))
	((and (universal-p jf1) (universal-p jf2))
	 (dup-rel-jforms dup-reln (universal-scope jf1) (universal-scope jf2)))
	((and (existential-p jf1) (existential-p jf2))
	 (dup-rel-jforms dup-reln (existential-scope jf1) (existential-scope jf2)))
	(t nil)))

; cebrown 10/13/01
(defun dup-rel-jforms-2 (dup-reln jl1 jl2)
  (if jl1
      (if jl2
	  (and (dup-rel-jforms dup-reln (car jl1) (car jl2))
	       (dup-rel-jforms-2 dup-reln (cdr jl1) (cdr jl2)))
	nil)
    (if jl2
	nil
      t)))

; cebrown 10/13/01
(defun build-duplicate-reln (e &optional el)
  (let* ((el1 (remove-if-not #'(lambda (x) (eq (type-of e) (type-of x)))
			     el))
	 (el2 (remove-if #'(lambda (x) (eq (type-of e) (type-of x)))
			 el))
	 (el0 (remove-duplicates (cons e el1))) ; remove duplicates - cebrown 5/29/02
	 (ret nil))
    (when el2
      (setq ret (build-duplicate-reln (car el2) (cdr el2))))
    (let ((rel-nodes (remove nil (mapcar #'(lambda (e0)
					     (if (allow-node-in-jform e0)
						 (etree-name e0)
					       nil))
					 el0))))
      (when rel-nodes (push rel-nodes ret)))
    (if (expansion-p e)
	(let ((allkids (apply #'append
			      (mapcar #'etree-components el0)))) ; el0 instead of el1 - cebrown 5/29/02
	  (when allkids
	    (setq ret (append ret
			      (build-duplicate-reln (car allkids) (cdr allkids))))))
      (if (etree-components e)
	  (if (cdr (etree-components e))
	      (setq ret
		    (append ret
			    (build-duplicate-reln (car (etree-components e))
						  (mapcar #'(lambda (x)
							      (car (etree-components x)))
							  el1))
			    (build-duplicate-reln (cadr (etree-components e))
						  (mapcar #'(lambda (x)
							      (cadr (etree-components x)))
							  el1))))
	    (setq ret
		  (append ret
			  (build-duplicate-reln (car (etree-components e))
						(mapcar #'(lambda (x)
							    (car (etree-components x)))
							el0)))))))
    ret))

(defun ubind (jform gwff)
  (let ((vars (upward-collectvars jform))
	(outwff gwff))
    (dolist (v vars outwff)
      (push (cons v 'forall) outwff))))

(defun upward-collectvars (jform)
  (if (null jform) nil
    (if (eq (jform-type jform) 'universal)
	(append (universal-qvars jform) (upward-collectvars (jform-parent jform)))
      (upward-collectvars (jform-parent jform)))))

(defun darrange (dup-priority)
  (declare (special disj-assoc))
  (let (returnlist
	litpairs
	count) 
    ;this will be a list of (lit2  lit1 (d . k) (v1 . w1) (v2 . w2)...(vn . wn))
    ;in which lit2 is the kth copy of lit1 (from conjunct d) with variables v1..vn replaced by w1..wn
    (dolist (dups dup-priority)
	    (setq count -1)
	    (dolist (d2 dups)
		    (incf count)
					;at this point, d2 is a later copy of (car dups), or the same thing
		    (setq litpairs (mapcar #'cons (cadr (assoc (car dups) disj-assoc)) (cadr (assoc d2 disj-assoc))))
	      (dolist (lp litpairs)
		(push (cons (cdr lp) 
			    (cons (car lp)
				  (cons (cons (car dups) count)
					(get-varchanges (jform-represents (car lp)) 
							(jform-represents (cdr lp)))))) returnlist))))
    returnlist))

(defun get-varchanges (wff1 wff2)
  (cond ((equal wff1 wff2) nil)
	((lsymbol-q wff1) (list (cons wff1 wff2)))
	(t (append (get-varchanges (gar wff1) (gar wff2)) (get-varchanges (gdr wff1) (gdr wff2))))))

(defun insert-in-cgraph-2 (counter key)
  (let ((key (cons (etree-name (find-etree-node-name (literal-name (car key))))
		   (etree-name (find-etree-node-name (literal-name (cdr key)))))))
  (setf (max-cgraph-counter) (1+ counter))
  (setf (gethash key (cgraph)) counter)
  (setf (gethash counter (connections-array))
	(cons key t))
  (when (or reduce-double-neg (and first-order-mode-ms (not ms98-force-h-o)))
	(setf (gethash (cons (cdr key) (car key)) (cgraph)) counter))))

(defun ccs-reorganize ()
  ;we want a list of elements like ((literals) width disjunction)
  (let (returnlist)
    (dolist (d (break-into-disjuncts ms90-3-jform) returnlist)
	    (push (list (jform-to-literal-list d nil) (disjunction-width d nil) d) returnlist))))

(defun disjunction-width (jform lits)
  (case (jform-type jform)
	(universal (disjunction-width (universal-scope jform) lits))
	(conjunction (if (conjunction-components jform)
			 (apply #'min (mapcar #'(lambda (x) (disjunction-width x lits)) (conjunction-components jform)))
		       1))
	(disjunction (if (disjunction-components jform)
			 (apply #'+ (mapcar #'(lambda (x) (disjunction-width x lits)) (disjunction-components jform)))
		       1))
	(literal (if (member jform lits) 0 1))
	(t (throwfail "DISJUNCTION-WIDTH found a jform of unknown type " (jform-type jform)))))

(defun break-into-disjuncts (jform)
  (break-into-disjuncts-real jform))

(defun break-into-disjuncts-real (jform)
  (case (jform-type jform)
	(universal (break-into-disjuncts-real (universal-scope jform)))
	(conjunction (if hpath-threshold ;if it's NIL, then break
			 (if (and (or (not (contains-univ jform))
				      (not-going-to-duplicate-anyway))
				  (<= (number-of-horizontal-paths-main jform) hpath-threshold))
			     (list jform)
			   (let (ucomps nonucomps retlist 1comps)
			     (dolist (d (conjunction-components jform))
			       (if (and (contains-univ d) (not (not-going-to-duplicate-anyway)))
				   (push d ucomps) 
				 (push d nonucomps)))
			     (when (> hpath-threshold 1) ;we can add a few nonucomps to the 1comps
			       (let ((count 0) newcount)
				 (dolist (c nonucomps)
				   (setq newcount (+ (number-of-horizontal-paths-main c) count))
				   (when (<= newcount hpath-threshold)
				     (push c 1comps) (setq count newcount)
				     (setq nonucomps (delete c nonucomps)))
				   (when (< hpath-threshold (1+ count))
				     (return t)))))
			     (setq ucomps (nreverse ucomps) nonucomps (nreverse nonucomps) 1comps (nreverse 1comps))
			     (if ucomps
				 (progn
				   (setq retlist (reduce #'append (mapcar #'break-into-disjuncts-real ucomps)))
				   (when nonucomps
				     (if (> (length nonucomps) 1)
					 (setq retlist 
					       (append retlist 
						       (break-into-disjuncts-real 
							(make-conjunction :components nonucomps 
									  :parent jform))))
				       (setq retlist (append retlist (break-into-disjuncts-real (car nonucomps))))))
				   (when 1comps 
				     (if (> (length 1comps) 1)
					 (push (make-conjunction :components 1comps :parent jform)
					       retlist)
				       (setq retlist (append 1comps retlist))))
				   retlist)
					;o/w no univs below here
			       (if 1comps
				   (progn
				     (when nonucomps
				       (if (> (length nonucomps) 1)
					   (setq retlist 
						 (append retlist 
							 (break-into-disjuncts-real 
							  (make-conjunction :components nonucomps 
									    :parent jform))))
					 (setq retlist (append retlist (break-into-disjuncts-real (car nonucomps))))))
				     (if (> (length 1comps) 1)
					 (push (make-conjunction :components 1comps :parent jform) retlist)
				       (setq retlist (cons (car 1comps) retlist))))
			       ;o/w it's all nonucomps, so behave as normal
				 (if (> (number-of-horizontal-paths-main jform) hpath-threshold)
				     (reduce #'append (mapcar #'break-into-disjuncts-real (conjunction-components jform)))
				   (list jform))))))
		       (reduce #'append (mapcar #'break-into-disjuncts-real (conjunction-components jform)))))
	(disjunction (if hpath-threshold
			 (if (and (or (not (contains-univ jform))
				      (not-going-to-duplicate-anyway))
				  (<= (number-of-horizontal-paths-main jform) hpath-threshold))
			     (list jform)
			   (let (ucomps nonucomps 1comps retlist)
			     (dolist (d (disjunction-components jform))
			       (if (and (contains-univ d) (not (not-going-to-duplicate-anyway)))
				   (push d ucomps) 
				 (if (= (number-of-horizontal-paths-main d) 1)
				     (push d 1comps)
				   (push d nonucomps))))
			     (when (> hpath-threshold 1) ;we can add a few nonucomps to the 1comps
			       (let ((count 1) newcount)
				 (dolist (c nonucomps)
				   (setq newcount (* (number-of-horizontal-paths-main c) count))
				   (when (<= newcount hpath-threshold)
				     (push c 1comps) (setq count newcount)
				     (setq nonucomps (delete c nonucomps)))
				   (when (< 1 (/ hpath-threshold count))
				     (return t)))))
			     (if ucomps
				 (progn
				   (setq retlist (reduce #'append (mapcar #'break-into-disjuncts-real ucomps)))
				   (when nonucomps
				     (if (> (length nonucomps) 1)
					 (setq retlist 
					       (append retlist 
						       (break-into-disjuncts-real 
							(make-disjunction :components nonucomps 
									  :parent jform))))
				       (setq retlist (append retlist (break-into-disjuncts-real (car nonucomps))))))
				   (when 1comps 
				     (if (> (length 1comps) 1)
					 (push (make-disjunction :components 1comps :parent jform)
					       retlist)
				       (setq retlist (append 1comps retlist))))
				   retlist)
					;o/w no univs below here
			       (if 1comps
				   (progn
				     (when nonucomps
				       (if (> (length nonucomps) 1)
					   (setq retlist 
						 (append retlist 
							 (break-into-disjuncts-real 
							  (make-disjunction :components nonucomps 
									  :parent jform))))
					 (setq retlist (append retlist (break-into-disjuncts-real (car nonucomps))))))
				     (if (> (length 1comps) 1)
					 (push (make-disjunction :components 1comps :parent jform) retlist)
				       (setq retlist (cons (car 1comps) retlist))))
			       ;o/w it's all nonucomps, so behave as normal
				 (if (> (number-of-horizontal-paths-main jform) hpath-threshold)
				     (reduce #'append (mapcar #'break-into-disjuncts-real (disjunction-components jform)))
				   (list jform))))))
		       (list jform)))
	(t (list jform))))

(defun not-going-to-duplicate-anyway ()
  (and (not break-at-quantifiers)
       (or *using-unifhash*
	   (eq num-of-dups 0)
	   (neq ms98-init 0))))

(defun contains-univ (jform)
  (case (jform-type jform)
    (universal t)
    (conjunction (dolist (c (conjunction-components jform) nil)
		   (when (contains-univ c) (return t))))
    (disjunction (dolist (c (disjunction-components jform) nil)
		   (when (contains-univ c) (return t))))
    (literal nil)))

(defun find-junctive-ancestors (jform)
  (if (jform-parent jform)
      (if (memq (jform-type (jform-parent jform)) '(disjunction conjunction))
	  (cons (jform-parent jform) (find-junctive-ancestors (jform-parent jform)))
	(find-junctive-ancestors (jform-parent jform)))
    nil))

(defun plausible-conn (lit1 lit2 dp2)
  (not (too-many-dups (find-touches (list lit1 lit2)) dp2)))

(defun ccs-mateable (jform anc)
  (or (null jform)
      (if (memq jform anc)
	  (eq (jform-type jform) 'conjunction)
	(ccs-mateable (jform-parent jform) anc))))

(defun find-blocks (lits)
  (declare (special disj-assoc))
  (let (return)
    (dolist (d disj-assoc (sort return #'<))
	    (when (and (intersection (cadr d) lits)
		       (zerop (disjunction-width (cadddr d) lits)))
		  (setq return (cons (car d) return))))))

(defun find-free-vars-in (mating)
  (let (fv2)
    (dolist (m mating)
	    (push (free-vars-of (jform-represents (car m))) fv2)
	    (push (free-vars-of (jform-represents (cdr m))) fv2))
    (remove-duplicates (intersection (free-vars-in-etree current-eproof) (reduce #'append fv2)))))

(defun no-higher-disjunct (l)
  (if (jform-parent l)
      (if (eq (jform-type (jform-parent l)) 'disjunction)
	  nil ;there's another disjunct above l
	(no-higher-disjunct (jform-parent l)))
    t))

(defun next-disjunct (l)
  (when l (cadr (member l (disjunction-components (jform-parent l))))))

(defun get-leftmost (l)
  (when l
	(case (jform-type l)
	      (literal (list l))
	      (disjunction (get-leftmost (car (disjunction-components l))))
	      (conjunction (reduce #'append (mapcar #'get-leftmost (conjunction-components l))))
	      (universal (get-leftmost (universal-scope l))))))

(defun etree-ancestor-list (n)
  (if (etree-parent n)
      (cons (etree-parent n) (etree-ancestor-list (etree-parent n)))
    (list n)))

(defun same-ccs-connlist (l1 l2)
  (and (null (setdiff l1 l2))
       (null (setdiff l2 l1))))
	      
(defun valid-addition-2 (c1 c2)
  (let* ((key (* (gethash c1 primehash1) 
		 (gethash c2 primehash1)))
	 (*vaf*-old *vaf*)
	 (result (gethash key *valid-hash*)))
    (if result
	(progn (when (cdr result) (incf *vaf*))
	       (if (eq (car result) 'fail) nil t))
      (progn (setq result (valid-addition-2-real c1 c2))
	     (setf (gethash key *valid-hash*) (cons (or result 'fail) (> *vaf* *vaf*-old)))
	     result))))

(defun valid-addition-2-real (c1 c2)
  (declare (special primehash1))
  ;suppose c1 is (A B) and c2 is (X Y).
  ;if A and X stand disjunctively and so do B and Y, then 
  ;;EITHER we *must* also have (A Y) (B X), 
  ;;OR there are U1 U2 U3 U4 such that we have (X U1) (Y U2) (A U3) (B U4)
  ;;in either case, we need max-mates >= 2
  ;similarly, if A and X stand conjunctively, and so do B and Y, we don't need them both.
  (let ((A (car c1))
	(B (cdr c1))
	(X (car c2))
	(Y (cdr c2))
	(compat-check (list c1 c2)))
    (if (and (stands-disjunctively A X)
	     (not (stands-disjunctively A Y))
	     (stands-disjunctively B Y)
	     (not (stands-disjunctively B X)))
	(progn (incf *vaf*)
	(if (or (eq max-mates 1) (= ms98-valid-pair 0))
	    nil
	  (if (= ms98-valid-pair 1)
	      t
	    (if (and (or (and (gethash (cons A Y) primehash1) (push (cons A Y) compat-check) t)
			 (and (gethash (cons Y A) primehash1) (push (cons Y A) compat-check) t))
		     (or (and (gethash (cons B X) primehash1) (push (cons B X) compat-check) t)
			 (and (gethash (cons X B) primehash1) (push (cons X B) compat-check) t)))
		(or (= ms98-valid-pair 2)
		    (compat-unify-check compat-check) ;;AY and BX are possible & correct
		    (get-new-conns-for (list A B X Y) (list c1 c2))) ;; or find new conns
	      (get-new-conns-for (list A B X Y) (list c1 c2)))))) ;;AX and BY are not possible; find new conns.
      (if (and (stands-disjunctively A Y)
	       (not (stands-disjunctively A Y))
	       (stands-disjunctively B X)
	       (not (stands-disjunctively B Y)))
	  (progn (incf *vaf*)
	  (if (or (eq max-mates 1) (= ms98-valid-pair 0))
	      nil
	    (if (= ms98-valid-pair 1)
		t
	      (if (and (or (and (gethash (cons A X) primehash1) (push (cons A X) compat-check) t)
			   (and (gethash (cons X A) primehash1) (push (cons X A) compat-check) t))
		       (or (and (gethash (cons B Y) primehash1) (push (cons B Y) compat-check) t)
			   (and (gethash (cons Y B) primehash1) (push (cons Y B) compat-check) t)))
		  (or (= ms98-valid-pair 2)
		      (compat-unify-check compat-check) ;;AX and BY are possible & correct
		      (get-new-conns-for (list A B X Y) (list c1 c2))) ;; or find new conns
		(get-new-conns-for (list A B X Y) (list c1 c2)))))) ;;AX and BY are not possible; find new conns.
	(if (or (and (stands-conjunctively A X)
		     (stands-conjunctively B Y))
		(and (stands-conjunctively A Y)
		     (stands-conjunctively B X)))
	    nil
	  t)))))

(defun compat-unify-check (connlist)
  (if *using-unifhash*
      (let (clist)
	(dolist (c connlist)
	  (push (car (remove-if-not #'(lambda (x) (equalp (car (component-clist x)) c)) ccs-original)) clist))
	(if (= ms98-valid-pair 3)
	    (pairwise-check-dags clist)
	  (progn 
	    (setq clist (remove-if-not #'dnode-p (mapcar #'component-final-subs clist)))
	    (neq 'fail (reduce #'merge-dags clist)))))
    t))

(defun pairwise-check-dags (clist)
  (do ((cl clist (cdr cl))
       key)
      ((null (cdr cl)) t)
    (unless (dolist (c (cdr cl) t)
	      (setq key (* (gethash (component-key (car cl)) primehash1) (gethash (component-key c) primehash1)))
	      (if (gethash key pairhash)
		  (when (eq (gethash key pairhash) 'fail) (return nil))
		(if (dag-will-merge (component-final-subs (car cl)) (component-final-subs c))
		    (when (and (dnode-p (component-final-subs (car cl)))
			       (dnode-p (component-final-subs c)))
		      (setf (gethash key pairhash) t))
		  (progn (setf (gethash key pairhash) 'fail)
			 (return nil)))))
      (return nil))))

(defun get-new-conns-for (litlist connlist)
  (setq litlist (mapcar #'(lambda (y) (remove-if
				       #'(lambda (x) (or (member (car (component-clist x)) connlist :test #'equalp) 
							 ;;don't want any of connlist
							 (and (neq (caar (component-clist x)) y)
							      (neq (cdar (component-clist x)) y))))
				       ccs-original))
			litlist))
  (setq connlist (mapcar #'(lambda (y) (car 
					(remove-if-not
					 #'(lambda (x) (equalp (car (component-clist x)) y))
					 ccs-original)))
			 connlist))
  ;litlist is now a list of lists of connections
  (if (memq nil litlist)
      nil
    (if *using-unifhash*
	(if (= ms98-valid-pair 3)
	    (progn (setq litlist (mapcar #'(lambda (x) (append connlist x)) (get-all-ccs-permutations-2 litlist)))
		   (dolist (l litlist nil)
		     (when (pairwise-check-dags l) (return t))))
	  (progn (setq litlist (get-all-ccs-permutations-2 litlist))
		 (setq connlist (mapcar #'component-final-subs connlist))
		 (setq connlist (if (dnode-p (car connlist))
				    (if (dnode-p (cadr connlist))
					(merge-dags (car connlist) (cadr connlist))
				      (car connlist))
				  (if (dnode-p (cadr connlist))
				      (cadr connlist)
				    nil)))
		 (dolist (possible litlist nil)
		   (let ((temp-connl connlist))
		     (when (dolist (l possible nil)
			     (when (dnode-p (component-final-subs l))
			       (if (dnode-p temp-connl)
				   (setq temp-connl (merge-dags temp-connl (component-final-subs l)))
				 (setq temp-connl (component-final-subs l)))
			       (when (eq temp-connl 'fail) (return t))))
		       (return t))))))
      t)))

(defun get-all-ccs-permutations-2 (list)
  (if (cdr list)
      (let (newlist)
	(dolist (c (car list) (reduce #'append newlist))
		(push (mapcar #'(lambda (x) (cons c x)) (get-all-ccs-permutations-2 (cdr list))) newlist)))
    (mapcar #'list (car list))))

(defun stands-disjunctively (p n)
  (eq (jform-type (lowest-common-jf-ancestor p n)) 'disjunction))

(defun lowest-common-jf-ancestor (p n)
  (do ((plist (list p) (cons (jform-parent (car plist)) plist))
       (nlist (list n) (cons (jform-parent (car nlist)) nlist)))
      ((and (null (car plist)) (null (car nlist)))
       (dolist (pl (reverse plist))
	       (when (member pl nlist) (return pl))))
      (when (null (car plist)) (pop plist))
      (when (null (car nlist)) (pop nlist))))

(defun stands-conjunctively (A B)
  (intersection (ancestors-until A 'disjunction) (ancestors-until B 'disjunction)))
  
(defun ancestors-until (lit fail-cond)
  (if (jform-parent lit)
      (unless (eq (jform-type lit) fail-cond)
	      (cons lit (ancestors-until (jform-parent lit) fail-cond)))
    (unless (eq (jform-type lit) fail-cond)
	    (list lit))))

(defun order-vars ()
  (free-vars-in-etree current-eproof))

(defun free-vars-grouped (etree)
  (when etree
    (typecase etree
      (expansion (let ((retlist (mapcar #'reverse (get-grouped-lists-below etree))))
		   (append (mapcar #'reverse (mapcar #'cdr retlist))
			   (reduce #'append (mapcar #'free-vars-grouped (remove-if #'null (mapcar #'car retlist)))))))
      (t (reduce #'append (mapcar #'free-vars-grouped (etree-components etree)))))))

(defun get-grouped-lists-below (etree)
  (if (not (etree-p etree)) (list (list nil))
    (if (and (expansion-p etree)
	     (eq (length (etree-components etree)) (length (expansion-terms etree))))
	(let (retlist)
	  (do ((pv (mapcar #'free-vars-of (expansion-terms etree)) (cdr pv))
	       (comp (etree-components etree) (cdr comp)))
	      ((null pv) retlist)
	    (dolist (x (get-grouped-lists-below (car comp)))
	      (push (append (car pv) x) retlist))))
      (if (and (not (expansion-p etree))
	       (eq (length (etree-components etree)) 1))
	  (get-grouped-lists-below (car (etree-components etree)))
	(list (list etree))))))

(defun free-vars-all ()
  (let (retlist)
    (dolist (l (free-vars-grouped (eproof-etree master-eproof)) retlist)
      (push (mapcar #'strip-exp-vars l) retlist))))

(defun get-live-vars ()
  (let ((leaves (mapcar #'leaf-name (or *live-leaves*
					(find-etree-nodes #'leaf-p* current-topnode))))
	(lits (jform-to-literal-list ms90-3-jform nil))
	vars)
    (dolist (l lits (remove-duplicates vars))
      (when (memq (literal-name l) leaves) (setq vars (append (free-vars-of (jform-represents l)) vars))))))

(defvar *no-vars*)

(defun order-vars-2 (da)
  (let ((live-vars (get-live-vars))
	var-eq-classes)
    (case (mod (floor (/ ms98-variable-order 10)) 10)
      (1 (setq var-eq-classes (mapcar #'list (free-vars-in-etree current-eproof))))
      ((2 4) (setq var-eq-classes (free-vars-all)))
      (t (let ((newlist (remove-if #'null (mapcar #'cdddr (remove-if-not #'(lambda (x) (neq x 0)) da :key #'cdaddr))))
	       (ovars nil)
	       (seen *ordered-vars*))
	   (dolist (n newlist) ; n is a list of old.new pairs
	     (dolist (v n) ;v is a pair old . new 
	       (if (assoc (car v) ovars)
		   (pushnew (cdr v) (cdr (assoc (car v) ovars)))
		 (setq ovars (cons (list (car v) (cdr v)) ovars)))
	       (setq seen (delete (car v) (delete (cdr v) seen)))))
	   (setq var-eq-classes (append (mapcar #'list seen) ovars)))))
    (setq var-eq-classes (remove-if #'null (mapcar #'(lambda (x) (remove-if-not #'(lambda (y) (memq y live-vars)) x))
						   var-eq-classes)))
    (setq var-eq-classes
	  (case (mod ms98-variable-order 10)
	    (1
	     (sort var-eq-classes
		   #'(lambda (x y)
		       (< (reduce #'*
				  (mapcar #'(lambda (z)
					      (length (gethash (cons (type z) 
								     (memq z *leibniz-var-list*)) *full-unifhash*)))
					  x))
			  (reduce #'*
				  (mapcar #'(lambda (z)
					      (length (gethash (cons (type z) 
								     (memq z *leibniz-var-list*)) *full-unifhash*)))
					  y))))))
	    (2 (sort var-eq-classes #'(lambda (x y) (> (length x) (length y)))))
	    (3 (sort var-eq-classes #'(lambda (x y) (< (length x) (length y)))))
	    (t 
	     (sort var-eq-classes
		   #'(lambda (x y)
		       (> (reduce #'*
				  (mapcar #'(lambda (z)
					      (length (gethash (cons (type z)
								     (memq z *leibniz-var-list*)) *full-unifhash*)))
					  x))
			  (reduce #'*
				  (mapcar #'(lambda (z)
					      (length (gethash (cons (type z)
								     (memq z *leibniz-var-list*)) *full-unifhash*)))
					  y))))))))
    (when (memq (mod (floor (/ ms98-variable-order 10)) 10) '(3 4))
      (setq var-eq-classes (mapcar #'list (reduce #'append var-eq-classes))))
    (when (eq (floor (/ ms98-variable-order 100)) 1)
      (let ((allb-vars (mapcar #'car *allb*))
	    sub1 sub2)
	(dolist (v var-eq-classes (setq var-eq-classes (nconc (nreverse sub1) (nreverse sub2))))
	  (if (intersection v allb-vars) (push v sub2) (push v sub1)))))
    (setq *veq-hash* (make-hash-table :test 'equal))
    (do ((count 0 (1+ count))
	 (vars var-eq-classes (cdr vars)))
	((null vars))
      (setf (gethash count *veq-hash*) (car vars)))
    (msgf "Variable ordering is: " t var-eq-classes t)
    (setq *no-vars* (null var-eq-classes))
    (setq *depth-hash* (adjust-array *depth-hash* (list (length var-eq-classes) *arbitrary-const*)))
    ))

(defun occurs-with (vars litlist)
  (dolist (l litlist nil)
    (when (subsetp vars (free-vars-of (jform-represents l))) (return t))))

(defun too-many-dups-clist (clist)
  (let ((lits (and (numberp max-mates)
		   (reduce #'append (mapcar #'component-lits clist))))
	(touches (reduce #'ordered-union (mapcar #'component-touches clist))))
    (or (too-many-dups touches)
	(and (numberp max-mates)
	     (violates-max-mates lits)))))

(defun enumerate-all-matings (jform comps)
  (declare (special primehash2 disj-assoc primehash1 pairhash))
  (let ((hpathlist (enumerate-horizontal-paths jform))
	temp-hpath
	complist)
    (dolist (hpath hpathlist complist)
	    (setq temp-hpath (mapcar #'(lambda (x) ;x is an elt of an hpath
					 (mapcar #'list (remove-if-not #'(lambda (z) ;z is any component 
									  (member x (component-lits z)))
								      comps))) hpath))
	    ;returns (((c1)(c2)(c3)) ((c4)(c5)) ...) where c1 c2 c3 are conns for the first elt of the hpath.
	    (setq temp-hpath (get-all-ccs-permutations-of temp-hpath))
	    (setq temp-hpath (remove-if #'too-many-dups-clist temp-hpath))
	    (msg "Checking " (length temp-hpath) " possible atomic components ")
	    (dolist (mating temp-hpath)
	      (setq *vaf* 0)
	      (do ((conn (cadr mating) (car clist))
		   (clist (cddr mating) (cdr clist))
		   (mating-sofar (list (car mating)) (cons conn mating-sofar))
		   (key nil)
		   (pairstack nil nil)
		   (flag nil)
		   (newcomp (quick-check-subs-2 (mapcar #'component-key mating)))) 
		  ;;t)) returns t unless there is a known incompatible pair.
		  ((or (null conn) (null newcomp))
		   (when (eq newcomp t) (setq newcomp (component-final-subs (car mating))
					      flag t))
		   (unless (or *using-unifhash*
			       (qum (mapcar #'(lambda (x) (copy-list (gethash x dphash))) 
					    (reverse (reduce #'append (mapcar #'component-clist mating))))
				    nil nil -1 -1)) ; added extra nil to fix bug - cebrown 2/12/01
		     (setq flag nil newcomp nil))
		   (when (and newcomp *using-unifhash* (> (length mating-sofar) 2) (= ms98-merge-dags 1))
		     (unless (daglist-will-merge
			      (mapcar #'component-final-subs mating-sofar)
			      (when (and *skolem-matters* *primsubs*) 
				(find-touches (reduce #'append (mapcar #'component-lits mating)))))
		       (setq newcomp nil flag nil)))
		   (when (and newcomp *using-unifhash* (= ms98-merge-dags 2))
		     (let ((touches (when (and *skolem-matters* *primsubs*) 
				      (find-touches (reduce #'append (mapcar #'component-lits mating)))))
			   (temp (component-final-subs (car mating-sofar))))
		       (dolist (c (mapcar #'component-final-subs (cdr mating-sofar))
				  (setq newcomp temp))
			 (setq temp (merge-subs temp c 0 touches t))
			 (unless temp (setq newcomp nil flag nil) (return nil)))))
		   (when (and ms98-low-memory (> ms98-merge-dags 0))
		     (incf *dmark*)
		     (mapcar #'mark-dag ccs-original)
		     (mapcar #'mark-dag complist) (mark-dag-real newcomp) (mapcar #'mark-dag comps)
		     (prune-unmarked-array))
		   (when (or ms98-verbose (> ms98-merge-dags 0)) 
		     (if newcomp (msg "+") (msg "-")))
		   (when (or newcomp flag)
		     (let* ((lits (reduce #'append (mapcar #'component-lits mating)))
			    (ncomp (make-component :clist (reverse (reduce #'append (mapcar #'component-clist mating)))
						   :lits lits
						   :name (incf component-count)
						   :tried (list component-count)
						   :key (reduce #'* (mapcar #'component-key mating))
						   :litkey (make-litkey lits)
						   :touches (find-touches lits)
						   :blocks (find-blocks lits)
						   :final-subs newcomp
						   :ddf nil)))
		       (when (and (member 'MATING ms98-trace)
				  (colored-mating (component-clist ncomp) given-clist))
			 (setf (component-good ncomp) t)
			 (msgf "Component " (component-name ncomp) " is good:")
			 (with-open-file (*standard-output* *ms98-trace-file* :direction :output :if-exists :append :if-does-not-exist :create)
					 (stringdtl)
					 (msgf "Component " (component-name ncomp) " is good:")
					 (msgf "Connections: " (component-clist ncomp) t)))	; also print to file
		       (setf (component-measure ncomp) (ccs-measure ncomp (length disj-assoc)))
		       (when ms98-verbose (msgf (component-clist ncomp) " is OK"))
		       (when (and (null (e-ordered-setdiff (component-touches ncomp) (component-blocks ncomp)))
				  (ccs-complete-p (component-clist ncomp) ncomp) ;check completeness *before* unifying...
				  (or (not *using-unifhash*)
				      (= ms98-merge-dags 2)
				      (get-final-subs-for ncomp comps (component-touches ncomp))))
			 (when *ccs-rewrites*
			   (setq *successful-sub* (append (extract-sub-from (component-final-subs ncomp))
							  *successful-sub*)))
			 (if (or (not ms98-full-jform) (eq ms98-full-jform ms90-3-jform)) ; cebrown 11/27/01
			     (throw 'stop (component-clist ncomp))
			   (progn
			     (ccs-solve-remaining-constraints ncomp)
			     (setq ncomp nil))))
			     
;		       (when (or (not (member 'MATING-FILTER ms98-trace)) ; this didn't work like I expected - cebrown
;				 (component-good ncomp)))       ; but maybe I don't need to worry since only good components are generated at step 1
		       (when ncomp
			 (push ncomp complist)))))
		  (if (dolist (c mating-sofar (dolist (p pairstack t)
					      (when (eq 'fail (apply #'check-pair p))
						(return nil))))
		      (unless (valid-addition-2 (car (component-clist c)) (car (component-clist conn)))
			(return nil))
		      (setq key (* (gethash (car (component-clist c)) primehash1) (component-key conn)))
		      (if (gethash key pairhash)
			  (when (eq (gethash key pairhash) 'fail) (return nil))
			(push (list (car (component-clist c)) (car (component-clist conn)) key) pairstack)))
		    (setq newcomp 'delay)
		  (setq newcomp nil)))))))

(defun enumerate-horizontal-paths (jform)
  (case (type-of jform)
    (literal (list (list jform)))
    ((universal existential)
     (enumerate-horizontal-paths (universal-scope jform)))
    (disjunction
     (if (disjunction-components jform)
         (get-all-ccs-permutations-of 
	  (mapcar #'enumerate-horizontal-paths (disjunction-components jform)))
       nil))
    (conjunction
     (if (conjunction-components jform)
	 (reduce #'append (mapcar #'enumerate-horizontal-paths (conjunction-components jform)))
       nil))))

(defun get-all-ccs-permutations-of (list)
  (if (cdr list)
      (let (newlist)
	(dolist (c (car list) (reduce #'append newlist))
		(push (mapcar #'(lambda (x) (append c x)) (get-all-ccs-permutations-of (cdr list))) newlist)))
    (car list)))

(defun bunch-together (da)
  (bunch-together-real da))

(defun bunch-together-real (da) 
  ;;da elts are (n (literals) width fragment) where n is the number in dp, and (literals) is the list of literals.
  ;;We return a list whose elements are lists (a V n1 n2 n3...) meaning that fragment a can only be
  ;;touched if one of the fragments n1 n2.. has been, and this means duplicating V.
  ;;This is done so that , given (A OR B) AND (A' OR B'), we can use B' once A is touched
  ;;(o/w we can only use B' once B is touched, which is incorrect).
  ;;
  ;;The rule is as follows: for each fragment, find the deepest expansion node covering the whole 
  ;;fragment below one of its children. If this child is a primsub, return (a).
  ;;If this is not the first non-primsub child, then we return (a n1 n2 n3...)
  ;;where n1 n2... are the fragments below the previous non-primsub child.
  ;;If it *is* the first non-primsub child, proceed to the next-highest expansion and repeat.
  ;;If we run out of expansions, return (a).
  (let ((fraglist (mapcar #'(lambda (x) (list (car x) (mapcar #'find-etree-node-name (mapcar #'literal-name (cadr x)))))
			  da))
	;;list of (n (leaves))
	retlist)
    (dolist (frag fraglist)
      (do ((node (find-expansion-covering (cadr frag)) (if node (ascend-to-exp-node (etree-parent node)) nil))
	   (frags (list (car frag))))
	  ((null node) (push frags retlist))
	(do ((children (reverse (etree-components (etree-parent node))) (cdr children))
	     (subs (reverse (expansion-terms (etree-parent node))) (cdr subs))
	     (stop nil)
	     (found nil))
	    ((or stop (null children)) (when stop (setq node nil) (setq frags (append frags stop))))
	  (if found
	      (when (not-primsub (car subs))
		(setq stop (cons (exp-var-var (car subs))
				 (fragments-below (car children) fraglist)))) ;found an earlier non-primsub
	    (if (eq (car children) node)
		(if (not-primsub (car subs))
		    (setq found t) ;this is OK, now look for the next earliest non-primsub
		  (setq children nil node nil))))))) ;this is a primsub, stop here.
    retlist))

(defun recursively-bunch (dp)
  ;;dp is a list of elts which are either (n) or (n var n1 n2... nk)
  ;;return a list of elts (n) or (n (varlist) n1 n2... nk)
  (let (retlist)
    (dolist (d dp (mapcar #'(lambda (x) (if (cdr x) (cons (car x) (cons (cadr x) (sort (cddr x) #'<))) x)) retlist))
      (if (null (cdr d)) (push d retlist)
	(let ((possible-dups (recurse-dp d dp)))
	  (do ((pd (cdr possible-dups) (cdr pd))
	       (chosen (car possible-dups)))
	      ((null pd) (push (cons (car d) (cons chosen (cddr d))) retlist))
	    (when (< (length (car possible-dups)) (length chosen))
	      (setq chosen (car possible-dups)))))))))

(defun recurse-dp (d dp)
  (if (null (cdr d)) (list nil)
  (mapcar #'(lambda (x) (cons (cadr d) x)) ;stick the current var on the front of all the lists...
	  (reduce #'append
		  (mapcar #'(lambda (y) (recurse-dp (assoc y dp) dp)) (cddr d)))))) ;...generated by recursing on each elt of the cddr

(defun not-primsub (sub)
  (eq (exp-var-var sub) (exp-var-subst sub)))

(defun fragments-below (node fraglist)
  (let ((leaves (find-etree-nodes #'leaf-p node))
	frags) ;leaves below node
    (dolist (f fraglist frags)
      (when (intersection (cadr f) leaves) (push (car f) frags)))))

(defun find-expansion-covering (leaflist)
  (setq leaflist (remove-duplicates (mapcar #'ascend-to-exp-node leaflist)))
  (if (memq nil leaflist) 
      nil
    (find-compatible-expansion leaflist)))

(defun find-compatible-expansion (exps)
  (if (null (cdr exps)) 
      (car exps)
    (let ((maxlist (list (car exps))))
      (dolist (e (cdr exps))
	(dolist (m maxlist)
	  (if (ebelow e m) (return t) ;e is below m
	    (when (ebelow m e) (progn (setq maxlist (cons e (delete m maxlist))) (return t)))))) ;m is below e
      (if (null (cdr maxlist)) (car maxlist) ;one of the exps was above all the others
	;;otherwise they're still incompatible
	(progn (setq maxlist (mapcar #'ascend-to-exp-node (mapcar #'etree-parent maxlist)))
	       (if (memq nil maxlist)
		   nil
		 (find-compatible-expansion maxlist)))))))

(defun ebelow (e m)
  (memq e (find-etree-nodes #'(lambda (x) (declare (ignore x)) t) m)))
    
(defun ascend-to-exp-node (node)
  (if node
      (if (expansion-p (etree-parent node)) node
	(ascend-to-exp-node (etree-parent node)))
    nil))

(defun does-dups-first (numlist &optional (numlist2 nil))
  ;;checks for two things: using a duplication before using the original, 
  ;;and using more than ms98-num-of-dups duplications in total.
  ;;if numlist is component-blocks, numlist2 contains component-touches
  ;;both numlist and numlist1 are guaranteed sorted least-first.
  (or (dolist (dups dup-priority nil)
	(when (and (cdr dups)
		   (ordered-memq (car dups) numlist)
		   (not (e-ordered-intersection (cddr dups) numlist)))
	  (return t)))
      (and ms98-num-of-dups
	   (> (ccs-count-dups-in (or numlist2 numlist)) ms98-num-of-dups))
      (and ms98-max-prims
	   (> (ccs-count-prims-in numlist) ms98-max-prims))))

(defun does-dups-first-2b (numlist connlist)
  ;;similar to does-dups-first, but adds elts from connlist one by one and fails if we add a dup first.
  ;;numlist is guaranteed sorted least-first.
  ;;tdups stores the list of dups as yet untouched.
  (let (tdups newconn temp)
    (or (dolist (dups *temp-dups* nil)
	  (if (ordered-memq (car dups) numlist)
	      (unless (e-ordered-intersection (cddr dups) numlist) ;the cddr of elts of temp-dups is sorted.
		(return t))
	    (push dups temp))) ;check that the initial part is OK; we've already done too-many-dups
	(dolist (c connlist nil)
	  (setq newconn (gethash c conn-hash))
	  (setq tdups temp) (setq temp nil)
	  (setq numlist (ordered-union-ck (component-touches newconn) numlist))
	  (when *sortedvar*
	    (when (dolist (dups tdups nil)
		    (if (ordered-memq (car dups) (component-touches newconn))
			(unless (e-ordered-intersection (cddr dups) numlist)
			  (return t))
		      (push dups temp)))
	      (return t)))))))

(defun does-dups-first-2 (numlist connlist)
  ;;numlist is guaranteed sorted least-first.
  (or (does-dups-first numlist) ;check that the initial part is OK
      (dolist (c connlist nil)
	(setq numlist (ordered-union-ck (component-touches (gethash c conn-hash)) numlist))
	(when *sortedvar*
	  (when (does-dups-first numlist) (return t))))))

(defun does-dups-first-2-init (numlist numlist2)
  ;;numlists are guaranteed sorted least-first.
  (or (does-dups-first numlist) ;check that the initial part is OK
      (dolist (c numlist2 nil)
	(setq numlist (ordered-union-ck (list c) numlist))
	(when *sortedvar*
	  (when (does-dups-first numlist) (return t))))))

(defun too-many-dups (numlist &optional (dp nil)) ;;dp is really supposed to be dup-priority
  ;;numlist guaranteed ordered
  (or (and ms98-num-of-dups
	   (> (ccs-count-dups-in numlist dp) ms98-num-of-dups))
      (and ms98-max-prims
	   (> (ccs-count-prims-in numlist) ms98-max-prims))))

(defun ccs-count-dups-in (numlist &optional (dp nil)) ;;dp is really supposed to be dup-priority
  (let (vdup)
    (dolist (dup dp (length (remove-duplicates (reduce #'append vdup))))
      (when (and (cdr dup)
		 (ordered-memq (car dup) numlist))
	(push (cadr dup) vdup)))
    (dolist (dup *temp-dups* (length (remove-duplicates (reduce #'append vdup))))
      (when (ordered-memq (car dup) numlist)
	(push (cadr dup) vdup)))))

(defun ccs-count-prims-in (numlist)
  (let ((count 0))
    (dolist (p *prim-lookup* count)
      (when (e-ordered-intersection p numlist) (incf count)))))

(defun ccs-banned-sk-consts (numlist)
  ;requires that ms98-num-of-dups be at the limit (e.g. that the component has 2 dups already if 
  ;ms98-num-of-dups is 2).
  ;;numlist guaranteed ordered
  (when *skolem-matters*
    (let ((retval (or (lookup-banned-list numlist (length numlist))
		      (store-banned-list numlist (ccs-banned-2 numlist)))))
      (if (eq retval 'fail) nil 
	;;      (progn (msgf numlist "-->" retval t) 
	retval))))

(defun ccs-banned-2 (numlist)
  (let ((skconsts (remove-if #'null (get-sk-below-dups) :key #'cdr))
	vdup disallow)
    (dolist (dup *extra-sk-vars* (setq vdup (remove-duplicates (reduce #'append vdup))))
      (when (ordered-memq (car dup) numlist)
	(push (cdr dup) vdup)))
    (dolist (s skconsts (sort (remove-duplicates 
			       (reduce #'append
				       (mapcar #'(lambda (x) (cdr (assoc x *subs-involving*)))
					       (remove-duplicates (reduce #'append disallow)))))
			      #'<))
      (unless (intersection (car s) vdup) (push (cdr s) disallow)))))
  
(defun store-banned-list (tl result)
  (setq result (or result 'fail))
  (push (cons (cons (length tl) tl) result) *banned-list*)
  result)

(defun lookup-banned-list (tl len)
  (dolist (o *banned-list* nil)
    (when (and (= (caar o) len)
	       (proper-tlist-check tl (cdar o)))
      (return (cdr o)))))

(defun sk-extract-dups (da)
  ;da is disj-assoc
  (let ((carlist (mapcar #'car da))
	(varlist (mapcar #'upward-collectvars (mapcar #'cadddr da)))
	(important-vars (reduce #'append (mapcar #'car (remove-if #'null (get-sk-below-dups) :key #'cdr))))
	temp)
    (setq varlist (mapcar #'(lambda (x) (intersection x important-vars)) varlist))
    (setq temp (mapcar #'cons carlist varlist))
    (remove-if #'null temp :key #'cdr)))
    
(defun excess-maxmates-ok (comp new-clist)
  (let ((real-clist (remove-if #'(lambda (x) (member x (component-clist comp))) new-clist))
	(lits (component-lits comp))
	(touched (component-touches comp)))
    (dolist (r real-clist t)
      (when (and (or (member (car r) lits) (member (cdr r) lits)) ;this is the cause of MM>1
		 (ordered-setdiff (find-touches (list (car r) (cdr r))) touched)) ;;yet it touches a new fragment
	(return nil)))))

; cebrown 10/4/01
(defun remove-flex-lits-from-jform (j setvars)
  (normalize-jform (remove-flex-lits-from-jform-rec j setvars)))
   

(defun remove-flex-lits-from-jform-rec (j setvars)
  (typecase j
	(disjunction
	 (let* ((kids1 (disjunction-components j))
		(kids2 (remove 'forget
			       (mapcar #'(lambda (kid)
					   (remove-flex-lits-from-jform-rec kid setvars))
				       kids1))))
	   (if (equal kids1 kids2)
	       j
	     (let ((j2 (copy-jform j)))
	       (setf (disjunction-components j2) kids2)
	       j2))))
	(conjunction
	 (let* ((kids1 (conjunction-components j))
		(kids2 (remove 'forget 
			       (mapcar #'(lambda (kid)
					   (remove-flex-lits-from-jform-rec kid setvars))
				       kids1))))
	   (if (equal kids1 kids2)
	       j
	     (let ((j2 (copy-jform j)))
	       (setf (conjunction-components j2) kids2)
	       j2))))
	(universal
	 (let ((kid (remove-flex-lits-from-jform-rec (universal-scope j) setvars)))
	   (if (equal kid (universal-scope j))
	       j
	     (if (eq kid 'forget)
		 'forget
	       (let ((j2 (copy-jform j)))
		 (setf (universal-scope j2) kid)
		 j2)))))
	(existential
	 (let ((kid (remove-flex-lits-from-jform-rec (existential-scope j) setvars)))
	   (if (equal kid (existential-scope j))
	       j
	     (if (eq kid 'forget)
		 'forget
	       (let ((j2 (copy-jform j)))
		 (setf (existential-scope j2) kid)
		 j2)))))
	(literal
	 (if (member (head (literal-represents j)) setvars)
	     (make-disjunction :components nil)
	   j))))

(defun dissolve-flex-lits-from-jform (j setvars)
  (normalize-jform
   (dissolve-flex-lits-from-jform-rec j setvars)))

(defun dissolve-flex-lits-from-jform-rec (j setvars)
  (typecase j
	(disjunction
	 (let* ((kids1 (disjunction-components j))
		(kids2 (mapcar #'(lambda (kid)
				   (dissolve-flex-lits-from-jform-rec kid setvars))
			       kids1)))
	   (if (equal kids1 kids2)
	       j
	     (let ((j2 (copy-jform j)))
	       (setf (disjunction-components j2) kids2)
	       j2))))
	(conjunction
	 (let* ((kids1 (conjunction-components j))
		(kids2 (mapcar #'(lambda (kid)
				   (dissolve-flex-lits-from-jform-rec kid setvars))
			       kids1)))
	   (if (equal kids1 kids2)
	       j
	     (let ((j2 (copy-jform j)))
	       (setf (conjunction-components j2) kids2)
	       j2))))
	(universal
	 (let ((kid (dissolve-flex-lits-from-jform-rec (universal-scope j) setvars)))
	   (if (equal kid (universal-scope j))
	       j
	     (let ((j2 (copy-jform j)))
	       (setf (universal-scope j2) kid)
	       j2))))
	(existential
	 (let ((kid (dissolve-flex-lits-from-jform-rec (existential-scope j) setvars)))
	   (if (equal kid (existential-scope j))
	       j
	     (let ((j2 (copy-jform j)))
	       (setf (existential-scope j2) kid)
	       j2))))
	(literal
	 (if (member (head (literal-represents j)) setvars)
	     (make-conjunction :components nil)
	   j))))

(defun ccs-dissolve-clist (clist j)
  (do ((j2 j (normalize-jform
	      (dissolve-jform (cons (caar clist2) (cdar clist2)) j2)))
       (clist2 clist (cdr clist2)))
      ((null clist2) j2)))
