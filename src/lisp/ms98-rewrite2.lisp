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

(deffile ms98-rewrite2
  (part-of ms98)
  (extension lisp)
  (mhelp "More functions that implement rewriting of equalities in MS98-1"))

(defun represents-a-rewrite (node mating banned)
  (let ((temp *active-rewrites*)
	ar sublist)
    (setq ar (append *global-rewrites* 
		     (intersection (cdr (assoc (literal-name (caar mating)) *local-rewrites* :test 'equal))
				   (cdr (assoc (literal-name (cdar mating)) *local-rewrites* :test 'equal)))))
    (setq *active-rewrites* nil sublist (qnode-substs node))
    (dolist (a ar)
      (unless (or (not (compatible-sublists-2 (gethash a *rrule-sub-list*) sublist))
		  (member a *fo-delete*))
	(push a *active-rewrites*)))
    (if *active-rewrites*
	(progn
	  (when (or (setdiff temp *active-rewrites*) (setdiff *active-rewrites* temp))
	    (do-parity-check))
	  (when (and ms98-rewrite-model
		     (or *global-rewrite-model* ;;there is a global interpretation
			 (and (null *global-rewrites*) *active-rewrites*))) ;;or there are no global rewrites...
	    (if (null (setdiff *active-rewrites* *global-rewrites*))
		(setq *rewrite-model* *global-rewrite-model*)
	      (get-and-test-model-auto)))
	  (set-global-rewrite-dtree)
	  (now-do-rewriting (caar (qnode-dpairs node)) (cdar (qnode-dpairs node)) 
			    (copy-list (qnode-substs node)) mating banned))
      (let ((wff1 (lnorm-q2 (caar (qnode-dpairs node)) (copy-list (qnode-substs node))))
	    (wff2 (lnorm-q2 (cdar (qnode-dpairs node)) (copy-list (qnode-substs node)))))
	(sort-of-simpl wff1 wff2)))))

(defun now-do-rewriting (gwff1 gwff2 subs mating &optional (banned t))
  (when (eq banned t)
    (setq banned (append (unreachable-skolem-constants (caar mating)) (unreachable-skolem-constants (cdar mating)))))
  (let* ((wff1 (lnorm-q2 gwff1 subs))
	 (wff2 (lnorm-q2 gwff2 subs))
	 (wff1neg (not-p wff1))
	 (wf1 (if wff1neg (cdr wff1) wff1))
	 (wff2neg (not-p wff2))
	 (wf2 (if wff2neg (cdr wff2) wff2)))
    (if (and (or (and wff1neg (not wff2neg)) (and wff2neg (not wff1neg))) ;different signs
	     (equal (head wf1) (head wf2)) ;same head variable
	     (not (exists-top-level-rewrite wf1 (free-vars-in-etree current-eproof)))
	     (not (exists-top-level-rewrite wf2 (free-vars-in-etree current-eproof)))) ;and no top-level rewrites
	(unless (or (and banned (or (contains-banned-const wff1 banned)
				    (contains-banned-const wff2 banned)))
		    (parity-check-fails wff1 wff2)
		    (and *rewrite-model* (not (verify-by-model wff1 wff2))))
	  (rewrite-by-args wf1 wff1neg wf2 banned subs mating))
      (let ((gotcha (generate-two-rewrite-trees wff1 wff2 ms98-rewrite-depth banned)))
	(store-global-rewrite-dtree)
	(if gotcha
	    (let ((path-up-from-1 (if (eq (cdar gotcha) t) (path-up-from (caar gotcha))
				    (path-up-from (nth (cdar gotcha) (rtree-sons (caar gotcha))))))
		  (path-up-from-2 (if (eq (cddr gotcha) t) (path-up-from (cadr gotcha))
				    (path-up-from (nth (cddr gotcha) (rtree-sons (cadr gotcha)))))))
	      (setf (gethash (car mating) *rewrite-hash*) 
		    (cons (cons subs
				(append (nreverse path-up-from-1)
					(mapcar #'(lambda (x) (list (car x) (not (cadr x))
								    (caddr x)))
						path-up-from-2)))
			  (gethash (car mating) *rewrite-hash*)))
	      t)
	  nil)))))

(defun delay-rewriting (mating subs rewrites)
  (setf (gethash (car mating) *rewrite-hash*) 
	(cons (cons subs (cons 'delay rewrites))
	      (gethash (car mating) *rewrite-hash*)))
  t)

(defun rewrite-by-args (wff1 wff1neg wff2 banned subs mating)
  (let ((h (head wff1)) ;which is also (head wff2)
	(a1list (args wff1))
	(a2list (args wff2))
	rlist newrlist) ;rlist will hold lists of rewrites for each arg
    (when (dolist (a (mapcar 'cons a1list a2list) t)
	    (when (or (parity-check-fails (car a) (cdr a))
		      (not-remotely-plausible-arg (car a) (cdr a))
		      (and *rewrite-model* (not (verify-by-model-arg (car a) (cdr a)))))
	      (return nil))
	    (let ((gotcha (generate-two-rewrite-trees (car a) (cdr a) ms98-rewrite-depth banned nil)))
	      (store-global-rewrite-dtree)
	      (unless gotcha (return nil)) ;if we can't rewrite one of the arguments, we fail
	      (let ((path-up-from-1 (if (eq (cdar gotcha) t) (path-up-from (caar gotcha))
				      (path-up-from (nth (cdar gotcha) (rtree-sons (caar gotcha))))))
		    (path-up-from-2 (if (eq (cddr gotcha) t) (path-up-from (cadr gotcha))
				      (path-up-from (nth (cddr gotcha) (rtree-sons (cadr gotcha)))))))
		(setq path-up-from-2 (process-path path-up-from-2 (cdr a)))
		(push (cons (nreverse path-up-from-1)
			    (mapcar #'(lambda (x) (list (car x) (not (cadr x))
							(caddr x)))
				    path-up-from-2))
		      ;;..which mess is the list of rewrites that turns argn of wff1 into argn of wff2...
		      rlist)
		)))
      (setq rlist (nreverse rlist)) ;rlist now starts with rewrites for arg1.
      (do ((rewrites rlist (cdr rewrites))
	   (leftargs nil (append leftargs (list (car targetlist))))
	   (rightargs a1list (cdr rightargs))
	   (targetlist a2list (cdr targetlist))
	   (currentgwff wff1))
	  ((null rightargs))
	(dolist (r (caar rewrites)) ;list of rewrites for the current arg
	  (let ((newwff (reduce 'cons (cons h (append leftargs (list (caddr r)) (cdr rightargs))))))
	    (when wff1neg (setq newwff (cons 'not newwff)))
	    (setq currentgwff newwff)
	    (push (list (car r) (cadr r) newwff) newrlist)))
	(setq wff1neg (not wff1neg))
	(if (not-p currentgwff) (setq currentgwff (cdr currentgwff)) (setq currentgwff (cons 'not currentgwff)))
	(dolist (r (cdar rewrites)) ;list of rewrites for the current arg
	  (let ((newwff (reduce 'cons (cons h (append leftargs (list (caddr r)) (cdr rightargs))))))
	    (when wff1neg (setq newwff (cons 'not newwff)))
	    (setq currentgwff newwff)
	    (push (list (car r) (cadr r) newwff) newrlist)))
	)
      (setf (gethash (car mating) *rewrite-hash*) 
	    (cons (cons subs
			(nreverse newrlist))
		  (gethash (car mating) *rewrite-hash*)))
      t)))
      

(defun process-path (path wff)
  (do ((rewrites path (cdr rewrites))
       newpath)
      ((null rewrites) (nreverse newpath))
    (let ((newwff (if (cdr rewrites) (caddr (cadr rewrites)) wff)))
      (push (list (caar rewrites) (cadar rewrites) newwff) newpath))))

(defun get-local-leaves (node)
  (let ((path1 (follow-branch-to-disjunct node))
	local-list)
    (dolist (l (find-etree-nodes #'leaf-p current-topnode) local-list)
      (when (intersection (follow-branch-to-top l) path1 :test 'equal) ;*not* follow-branch-to-disjunct
	(push l local-list)))))

(defun follow-branch-to-top (node)
  (if (etree-parent node)
      (cons node (follow-branch-to-top (etree-parent node)))
    (list node)))

(defun follow-branch-to-disjunct (node)
  (if (etree-parent node)
      (if (equal (etree-junctive (etree-parent node)) 'DIS)
	  (list node)
	(cons node (follow-branch-to-disjunct (etree-parent node))))
    (list node)))

(defun compatible-sublists-2 (list1 list2)
  (dolist (l list1 t)
    (when (and (assoc (car l) list2 :test 'equal)
	       (neq (cdr (assoc (car l) list2 :test 'equal)) (cdr l)))
      (return nil))))
      
(defun set-global-rewrite-dtree ()
  (let ((rewlist (setdiff *active-rewrites* *global-rewrites*)))
    (dolist (elt *rewrite-dtree-list* (setq *global-rewrite-dtree* (make-dtree :rewrites t)))
      (when (and (null (setdiff rewlist (car elt)))
		 (null (setdiff (car elt) rewlist)))
	(setq *global-rewrite-dtree* (cdr elt))
	(return t)))))

(defun store-global-rewrite-dtree ()
  (let ((rewlist (setdiff *active-rewrites* *global-rewrites*))
	newlist did-something)
    (dolist (elt *rewrite-dtree-list* 
		 (progn (when (not did-something) (push (cons rewlist *global-rewrite-dtree*) newlist))
			(setq *rewrite-dtree-list* newlist)))
      (if (and (null (setdiff rewlist (car elt)))
	       (null (setdiff (car elt) rewlist)))
	  (progn (push (cons rewlist *global-rewrite-dtree*) newlist)
		 (setq did-something t))
	(push elt newlist)))))
  
	
(defun not-remotely-plausible-arg (gwff1 gwff2)
  (let ((wff1 (if (not-p gwff1) (cdr gwff1) gwff1))
	(wff2 (if (not-p gwff2) (cdr gwff2) gwff2))
	(v (free-vars-in-etree current-eproof))
	(both-neg (and (not-p gwff1) (not-p gwff2)))
	(both-pos (and (not (not-p gwff1)) (not (not-p gwff2)))))
    (if (or both-pos both-neg)
	(not (or (equal (head wff1) (head wff2)) ;have same head, or the head can be rewritten
		 (exists-top-level-rewrite wff1 v)
		 (exists-top-level-rewrite wff2 v)))
      (or (not (exists-neg-rewrite))
	  (not (or (exists-top-level-rewrite gwff1 v) (exists-top-level-rewrite gwff2 v)
		   (exists-top-level-rewrite wff1 v) (exists-top-level-rewrite wff2 v)))))))

(defun verify-by-model-arg (wff1 wff2)
  (let ((result (if (and (not (not-p wff1)) (not (not-p wff2)))
		    (replace-with-interps wff1 wff2)
		  (if (and (not-p wff1) (not-p wff2))
		      (replace-with-interps (cdr wff1) (cdr wff2))
		    nil))))
    result))

