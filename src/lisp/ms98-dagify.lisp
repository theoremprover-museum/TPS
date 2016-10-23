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

(deffile ms98-dagify
  (part-of ms98)
  (extension lisp)
  (mhelp "The functions to handle directed acyclic graphs for MS98-1"))

(defun make-dnode1 (sons depth &optional (merge (list nil)))
  (let* ((l (length sons))
	 (key2 (branchbelow sons))
	 (key3 (sumbelow sons))
	 (key1 (mod (+ l key2 key3) *arbitrary-const*))
	 newnode)
    (setq sons (sort sons #'lex-sublist :key #'car))
    (unless (hash-table-p (aref *depth-hash* depth key1))
      (setf (aref *depth-hash* depth key1) #+(or sbcl cmu)(make-hash-table :test #'equal) ;cmucl doesn't like equalp. MB
	                                   #-(or sbcl cmu)(make-hash-table :test #'equalp)))
    (dolist (n (gethash (cons key2 key3) (aref *depth-hash* depth key1))
	       (progn (setq newnode 
			    (make-dnode :sons sons :merge merge
					:keys (cons
					       (if (get-collect-sons (mapcar #'car sons))
						   (- (incf *dnode-count*))
						 (incf *dnode-count*))
					       (cons key2 key3))))
		      (push newnode (gethash (cons key2 key3) (aref *depth-hash* depth key1)))
		      newnode))
      (when (dag-isom-flat2 sons n l key2 key3)
	(setq newnode n)
	(return newnode)))))

(defun get-collect-sons (list)
  (if (get-collect-sons-real list) t nil))

(defun get-collect-sons-real (list) 
  ;;this function checks whether we might need to call combine-identical-sons later on...
  (if (null (car list)) nil
    (or (let ((foo (remove-duplicates (mapcar 'car list))))
	  (and (memq 0 foo)
	       (cdr foo)))
	(get-collect-sons-real (mapcar 'cdr list)))))

(defun key2-fn (elt)
  (reduce #'+ (car elt)))

(defun dnode-key-cdr (elt)
  (cddr (dnode-keys (cdr elt))))

(defun dnode-key-car (elt)
  (cadr (dnode-keys (cdr elt))))

(defun sumbelow (sons)
  (+ (reduce #'+ (mapcar #'key2-fn sons))
     (if (dnode-p (cdar sons))
	 (reduce #'+ (mapcar #'dnode-key-cdr sons))
       0)))

(defun branchbelow (sons)
  (+ (length sons)
     (if (dnode-p (cdar sons))
	 (reduce #'+ (mapcar #'dnode-key-car sons))
       0)))

(defun mark-dag (component)
  (if (listp component) 
      (if (dnode-p (car component))
	  (mapcar #'mark-dag-real component)
	(when (or (component-p (car component))
		  (listp (car component)))
	  (mapcar #'mark-dag component)))
    (mark-dag-real (component-final-subs component))))

(defun dname (dag) (abs (car (dnode-keys dag))))
(defun dcollect (dag) (< (car (dnode-keys dag)) 0))
(defun dvars-d (d) (gethash d *veq-hash*))

(defun mark-dag-real (dag)
  (when (and (dnode-p dag)
	     (not (eql (dnode-merge dag) *dmark*)))
    (setf (dnode-merge dag) *dmark*) 
    (mapcar #'mark-dag-real (mapcar #'cdr (dnode-sons dag)))))

(defun prune-unmarked-array ()
  (clrhash *daghash*)
  (do ((a 0 (1+ a))
       elt) ;  foo)
      ((= a (array-dimension *depth-hash* 0)))
    (do ((b 0 (1+ b)))
	((= b *arbitrary-const*))
      (setq elt (aref *depth-hash* a b))
      (when (hash-table-p elt)
	#+comment(maphash #'(lambda (key val)
		     (progn (setq foo (delete-if-not #'markp val))
			    (dolist (v foo)
			      (setf (dnode-merge v) (list nil)))
			    (if foo (setf (gethash key elt) foo) (remhash key elt))))
			  elt)
	(maphash #'(lambda (key val)
		     (progn (setf (gethash key elt) (delete-if-not #'markp val))
			    (unless (gethash key elt) (remhash key elt))))
		     elt)
	)))
  )

(defun showme-unmarked-array ()
  (clrhash *daghash*)
  (do ((a 0 (1+ a))
       elt)
      ((= a (array-dimension *depth-hash* 0)))
    (do ((b 0 (1+ b)))
	((= b *arbitrary-const*))
      (setq elt (aref *depth-hash* a b))
      (if (hash-table-p elt)
	  (maphash #'(lambda (key val)
		       (declare (ignore key))
		       (msgf (remove-if #'listp (mapcar #'dnode-merge val))))
		   elt)
	(when elt (msg elt))
	)))
  )

(defun markp (val) 
  (if (equal (dnode-merge val) *dmark*)
      (progn (setf (dnode-merge val) (list nil)) t)
    (progn (setf (dnode-merge val) (list nil)) nil)))

(defun dagify-nodes (substlist)
  (if *skolem-matters* 
      (dagify-nodes-2 substlist 0)
    (dagify-nodes-1 substlist 0)))

(defun dagify-nodes-1 (substlist depth)
  (if (eq substlist '(delay)) 'delay
    (if (null (dvars-d depth)) nil ;we're done
      (let ((v (dvars-d depth)) ;a list of variables
	    olist sons)
	(dolist (s substlist)
	  (setq olist (insert-new-subnode (remove-if #'(lambda (x) (member (car x) v)) s)
					  (mapcar #'(lambda (v1) (or (cdr (assoc v1 s)) 0)) v) olist)))
	(dolist (o olist
		   (make-dnode1 sons depth))
	  (push (cons (car o)
		      (dagify-nodes-1 (cdr o) (1+ depth)))
		sons))))))

(defun insert-new-subnode (subs sublist insertlist)
  (if (null insertlist) (list (cons sublist (list subs)))
    (case (sublist-order (caar insertlist) sublist)
      (0 ;same sublist
       (cons
	(cons (caar insertlist) (cons subs (cdar insertlist))) ;add the subs at this point
	(cdr insertlist))) ;and then tack on the rest of the list
      (1 ;caar insertlist is smaller
       (cons (car insertlist)
	     (insert-new-subnode subs sublist (cdr insertlist))))
      (2 ;sublist is smaller
       (cons (cons sublist (list subs))
	     insertlist)))))

(defun sublist-order (list1 list2)
  ;return 0 if the same, 1 if list1 is smaller, 2 if list2 is smaller
  ;lists are guaranteed same length
  (if (null list1) 0
    (if (= (car list1) (car list2)) (sublist-order (cdr list1) (cdr list2))
      (if (< (car list1) (car list2)) 1 2))))

(defun dagify-nodes-2 (substlist depth &optional (subsofar nil))
  (if (eq substlist '(delay)) 'delay
    (if (null (dvars-d depth)) nil ;we're done
      (let* ((v (dvars-d depth)) ;a list of variables
	     (check (dolist (v1 v nil) (when (assoc v1 *allb*) (return t))))
	     cktemp olist failflag sons)
	(dolist (s substlist)
	  (setq olist (insert-new-subnode (remove-if #'(lambda (x) (member (car x) v)) s)
					  (mapcar #'(lambda (v1) (or (cdr (assoc v1 s)) 0)) v)
					  olist)))
	(dolist (o olist
		   (progn
		     (when (rassoc 'fail sons) (setq failflag t))
		     (setq sons (remove-if #'failprune sons))
		     (if (and failflag (null sons)) 'fail
		       (make-dnode1 sons depth))))
	  (setq cktemp (and check 
			    (get-check-allb v (car o))))
	  (if (and cktemp (cyclic-selection-dag (append cktemp subsofar)))
	      (push (cons nil 'fail) sons)
	    (push (cons (car o)
			(dagify-nodes-2 (cdr o)
					(1+ depth)
					(if cktemp (append cktemp subsofar) subsofar)))
		  sons)))))))

(defun failprune (elt)
  (eq (cdr elt) 'fail))

(defun get-check-allb (vars subs)
  (if (null vars) nil
    (if (and (> (car subs) 0)
	     (assoc (car vars) *allb*)
	     (gethash (car subs) *allsub-hash*))
	(cons (cons (car vars) (gethash (car subs) *allsub-hash*))
	      (get-check-allb (cdr vars) (cdr subs)))
      (get-check-allb (cdr vars) (cdr subs)))))

(defun cyclic-selection-dag (all-subs)
  ;;*allb* is a list of elts (var const1 const2...) where each const is banned in subs for var
  ;;all-subs is a list of (var const1 const2) where each const occurs in the sub for var.
  (setq *dag-vars-checked* nil)
  (let ((all-subs (append (eproof-inst-exp-vars-params current-eproof) all-subs))) ; cebrown 7/26/00
    (dolist (sub all-subs nil)
      (when (and (not (memq (car sub) *dag-vars-checked*))
		 (dag-find-cycle-beginning-at (list (car sub)) all-subs))
	(return t)))))

(defun dag-find-cycle-beginning-at (varlist all-subs)
  (dolist (c (cdr (assoc (car varlist) all-subs)) nil)
    (dolist (v (dagassoc c))
      (when (memq v varlist) (return-from dag-find-cycle-beginning-at t)) ;found a cycle
      (push v *dag-vars-checked*)
      (when (dag-find-cycle-beginning-at (cons v varlist) all-subs)
	(return-from dag-find-cycle-beginning-at t)))))

(defun dagassoc (c)
  (if (assoc c *find-cycle-fn-list*)
      (cdr (assoc c *find-cycle-fn-list*))
    (let ((result (dag-find-cycle-fn c *allb*)))
      (push (cons c result) *find-cycle-fn-list*)
      result)))

(defun dag-find-cycle-fn (c all-banned)
  (if (null all-banned) nil
    (if (memq c (cdar all-banned)) (cons (caar all-banned) (dag-find-cycle-fn c (cdr all-banned)))
      (dag-find-cycle-fn c (cdr all-banned)))))

(defun same-sublist (alist blist)
  (if (and (null blist) (null alist)) t ;we know they're the same length
    (and (eq (car alist) (car blist))
	 (same-sublist (cdr alist) (cdr blist)))))

(defun dag-isom-flat2 (sons dag2 l b s)
  (and (= (cadr (dnode-keys dag2)) b)
       (= (cddr (dnode-keys dag2)) s)
       (= (length (dnode-sons dag2)) l)
       (do ((sons1 sons (cdr sons1))
	    (sons2 (dnode-sons dag2) (cdr sons2)))
	   ((null sons1) t)
	 (unless (and (eq (cdar sons1) (cdar sons2))
		      (same-sublist (caar sons1) (caar sons2))) ;caar and cdar both the same
	   (return nil)))))

(defun daghash-get (name1 name2)
  (if (< name1 name2)
      (gethash (cons name1 name2) *daghash*)
    (gethash (cons name2 name1) *daghash*)))

(defun nullist (list)
  (or (null list)
      (and (eq (car list) 0)
	   (nullist (cdr list)))))

(defun merge-dags (dag1 dag2 &optional (touched nil) (ban-me nil))
  (setq *banme* ban-me)
  (when ms98-low-memory (clrhash *daghash*))
  (if (or *skolem-matters* *primsubs*)
      (progn (setq *lookup* (get-lookup-list touched))
	     (merge-dags-2 dag1 dag2 0 nil))
    (merge-dags-1 dag1 dag2 0)))

(defun get-lookup-list (touched)
  (if *prim-lookup*
      (let* ((untouched-prims (mapcar #'(lambda (x) (if (e-ordered-intersection x touched) nil x)) *prim-lookup*))
	     (prims (- (length *prim-lookup*) (length (remove-if #'null untouched-prims)))))
	(cons prims untouched-prims))
    (cons 0 nil)))

(defun merge-dags-1 (dag1 dag2 depth)
  (if (or (eq dag1 'fail) (eq dag2 'fail))
      'fail
    (if (eq dag1 dag2) dag1
      (if (null dag2) 
	  'fail
	(or (daghash-get (dname dag1) (dname dag2))
	    (let ((sons1 (if *banme* (remove-if #'e-intersection-banme (dnode-sons dag1))
			   (dnode-sons dag1)))
		  (sons2 (if *banme* (remove-if #'e-intersection-banme (dnode-sons dag2))
			   (dnode-sons dag2)))
		  sons son-temp merge)
	      (dolist (son1 sons1)
		(dolist (son2 sons2)
		  (when (and (> (caar son1) 0) (> (caar son2) (caar son1))) (return nil))
		  (setq merge (merge-if-compatible (car son1) (car son2)))
		  (when (and merge 
			     (dag-will-merge-1 (cdr son1) (cdr son2)))
		    (setq son-temp (merge-dags-1 (cdr son1) (cdr son2) (1+ depth)))
		    (unless (eq son-temp 'fail)
		      (push (cons merge son-temp) sons)))))
	      (when (and sons (or (dcollect dag1) (dcollect dag2)))
		(setq sons (combine-identical-sons sons depth)))
	      (setq sons (if sons 
			     (make-dnode1 sons depth)
			   'fail))
	      (unless (and *banme* (not ms98-low-memory))
		;;then store it, o/w don't
		(let ((name1 (dname dag1))
		      (name2 (dname dag2)))
		  (if (< name1 name2)
		      (setf (gethash (cons name1 name2) *daghash*) sons)
		    (setf (gethash (cons name2 name1) *daghash*) sons))))
	      sons))))))

(defun merge-dags-2 (dag1 dag2 depth subsofar &optional (vars nil))
  (when (null vars) (setq vars (dvars-d depth)))
  (if (or (eq dag1 'fail) (eq dag2 'fail))
      'fail
    (if (eq dag1 dag2) dag1
      (if (or (null dag2) (null dag1))
	  'fail
	(or (daghash-get (dname dag1) (dname dag2))
	    (let ((check (dolist (v vars nil) (when (assoc v *allb*) (return t))))
		  (sons1 (if *banme* (remove-if #'e-intersection-banme (dnode-sons dag1))
			   (dnode-sons dag1)))
		  (sons2 (if *banme* (remove-if #'e-intersection-banme (dnode-sons dag2))
			   (dnode-sons dag2)))
		  merge cktemp sons son-temp)
	      (dolist (son1 sons1)
		(dolist (son2 sons2)
		  (when (and (> (caar son1) 0) (> (caar son2) (caar son1))) (return nil))
		  ;;since sons are lex-sorted, the case above means no further elt of sons2 can be compatible w/ son1
		  (setq merge (merge-if-compatible (car son1) (car son2)))
		  (when merge
		    (setq cktemp (and check 
				      (get-check-allb vars merge)))
		    (when (and 
			   (or (null cktemp) (not (cyclic-selection-dag (append cktemp subsofar))))
			   (or (not (and *primsubs* (primcheck-sublists (car son1) (car son2))))
			       (check-for-primsubs-2 subsofar *lookup*))
			   (dag-will-merge-2 (cdr son1) (cdr son2) (append cktemp subsofar) (1+ depth)))
					;don't need to check, or c-s-d is NIL
					;and don't need primsub-check, or primsub-check succeeds
					;and what follows will merge.
		      (setq son-temp (merge-dags-2 (cdr son1) (cdr son2) (1+ depth) 
						   (if cktemp (append cktemp subsofar) subsofar)
						   (dvars-d (1+ depth))))
		      (unless (eq son-temp 'fail)
			(push (cons merge son-temp) sons))))))
	      (when (and sons (or (dcollect dag1) (dcollect dag2)))
		(setq sons (combine-identical-sons sons depth)))
	      (setq sons (if sons (make-dnode1 sons depth)
			   'fail))
	      (unless (or subsofar (and *banme* (not ms98-low-memory)))
		;;then store it, o/w don't
		(let ((name1 (dname dag1))
		      (name2 (dname dag2)))
		  (if (< name1 name2)
		      (setf (gethash (cons name1 name2) *daghash*) sons)
		    (setf (gethash (cons name2 name1) *daghash*) sons))))
	      sons))))))

(defun merge-if-compatible (list1 list2)
  (do ((l1 (cdr list1) (cdr l1))
       (l2 (cdr list2) (cdr l2))
       (e1 (car list1) (car l1))
       (e2 (car list2) (car l2))
       temp)
      ((null e1) (nreverse temp))
    (if (zerop e1) (push e2 temp)
      (if (zerop e2) (push e1 temp)
	(if (= e1 e2) (push e1 temp)
	  (return nil))))))

(defun combine-identical-real (sonlist depth)
  (do ((sons (cdr sonlist) (cdr sons))
       (son1 (caar sonlist) (caar sons))
       (son2 (cdar sonlist) (cdar sons))
       (temp nil nil)
       newsonlist)
      ((null son1) newsonlist)
    (if (car sons)
	(do ((s (pop sons) (pop sons)))
	    ((or (null sons) (not (same-sublist son1 (car s))))
	     (progn
	       (if (same-sublist son1 (car s)) ;then we're here because we're at the end of the list
		   (pushnew (cdr s) temp)
		 (push s sons)) ;otherwise this is just an ordinary failure.
	       (when temp
		 (setq son2 (if (null son2) nil
			      (make-dnode1 (combine-identical-sons 
					    (append (dnode-sons son2) (reduce #'append (mapcar #'dnode-sons temp)))
					    (1+ depth))
					   (1+ depth)
					   (get-mergelist (mapcar #'dnode-merge (cons son2 temp)))
					   ))))
	       (push (cons son1 son2) newsonlist)))
	  (pushnew (cdr s) temp))
      (push (cons son1 son2) newsonlist))))

(defun get-mergelist (mergelist)
  (cons (reduce #'reverse-ordered-union (mapcar #'car mergelist))
	(reduce #'reverse-ordered-union (mapcar #'cdr mergelist))))

(defun lex-sublist (x y)
  (or (< (car x) (car y))
      (and (= (car x) (car y))
	   (cdr x)
	   (lex-sublist (cdr x) (cdr y)))))

(defun combine-identical-sons (sonlist depth)
  (combine-identical-real (sort (copy-list sonlist) #'lex-sublist :key #'car) depth))

(defun dag-will-merge (dag1 dag2)
  (if *skolem-matters*
      (dag-will-merge-2 dag1 dag2 nil 0)
    (dag-will-merge-1 dag1 dag2)))

(defun known-mergeable (dag1 dag2)
  (let ((name1 (dname dag1))
	(name2 (dname dag2)))
    (if (> name1 name2)
	(reverse-ordered-memq name1 (car (dnode-merge dag2)))
      ;they can't be =, so > name1 name2
      (reverse-ordered-memq name2 (car (dnode-merge dag1))))))

(defun known-not-mergeable (dag1 dag2)
  (let ((name1 (dname dag1))
	(name2 (dname dag2)))
    (if (> name1 name2)
	(reverse-ordered-memq name1 (cdr (dnode-merge dag2)))
      ;they can't be =, so > name1 name2
      (reverse-ordered-memq name2 (cdr (dnode-merge dag1))))))

(defun dag-will-merge-1 (dag1 dag2)
  (if (or (eq dag1 dag2) (eq dag1 'delay) (eq dag2 'delay)) t
    (if (known-mergeable dag1 dag2)
	t
      (if (known-not-mergeable dag1 dag2)
	  nil
	(dolist (sons1 (dnode-sons dag1) 
		       (progn (pair-wont-merge dag1 dag2)
			      nil))
	  (when (dolist (sons2 (dnode-sons dag2) nil)
		  (when (and (> (caar sons1) 0) (> (caar sons2) (caar sons1))) (return nil))
		  (when (and (compatible-sublists (car sons1) (car sons2))
			     (dag-will-merge-1 (cdr sons1) (cdr sons2)))
		    (return t)))
	    (progn (pair-will-merge dag1 dag2)
	      (return t))))))))

(defun dag-will-merge-2 (dag1 dag2 &optional (subsofar nil) (depth 0) (vars nil))
  (when (null vars) (setq vars (dvars-d depth)))
  (if (or (eq dag1 dag2) (eq dag1 'delay) (eq dag2 'delay)) t
    (if (and (null subsofar)
	     (known-mergeable dag1 dag2))
	t
      (if (known-not-mergeable dag1 dag2)
	  nil
	(let ((check (dolist (v vars nil) (when (assoc v *allb*) (return t))))
	      cktemp merge)
	  (dolist (sons1 (dnode-sons dag1)
			 (progn (when (null subsofar)
				  (pair-wont-merge dag1 dag2))
				nil))
	    (when (dolist (sons2 (dnode-sons dag2) nil)
		    (when (and (> (caar sons1) 0) (> (caar sons2) (caar sons1))) (return nil))
		    (setq merge (merge-if-compatible (car sons1) (car sons2)))
		    (when (and merge
			       (progn 
				 (setq cktemp (and check (get-check-allb vars merge)))
				 (when (or (null cktemp) (not (cyclic-selection-dag (append cktemp subsofar))))
				   (dag-will-merge-2 (cdr sons1) (cdr sons2)
						     (if cktemp (append cktemp subsofar) subsofar) (1+ depth)
						     (dvars-d (1+ depth))))))
		      (return t)))
	      (progn (pair-will-merge dag1 dag2) ;subsofar can only restrict the allowable merges
		     (return t)))))))))

(defun compatible-sublists (list1 list2)
  (and (or (zerop (car list1))
	   (zerop (car list2))
	   (eq (car list1) (car list2)))
       (or (null (cdr list1)) ;both lists are the same length
	   (compatible-sublists (cdr list1) (cdr list2)))))

(defun primcheck-sublists (list1 list2)
  (or (and (zerop (car list1)) (not (zerop (car list2))))
      (and (zerop (car list2)) (not (zerop (car list1))))
      (and (cdr list1)
	   (primcheck-sublists (cdr list1) (cdr list2)))))

(defun collapse-dag (dag)
  (setq *allowed-subs-list* 
	(remove-if #'collapse-fn-2 (collapse-dag-real (list dag)))))

(defun collapse-fn-2 (elt)
  (eq (cadr elt) 0))

(defun collapse-dag-real (daglist &optional (depth 0))
  (if (null (car daglist)) nil
    (let ((sonlist (reduce #'append (mapcar #'dnode-sons daglist))))
      (do ((subs (mapcar #'car sonlist) (mapcar #'cdr subs))
	   (vlist (dvars-d depth) (cdr vlist))
	   sublist)
	  ((null vlist)
	   (append sublist
		   (collapse-dag-real (remove-duplicates (mapcar #'cdr sonlist)) (1+ depth))))
	(push (cons (car vlist) (sort (copy-list (remove-duplicates (mapcar #'car subs))) #'<)) sublist)))))

(defun apply-sub-to-dag (dag sub &optional (depth 0))
  (if (null dag) nil
    (if (intersection (mapcar #'car sub) (dvars-d depth))
	(let ((newdags (mapcar #'(lambda (x) (cons (rearrange-sublist (car x) (dvars-d depth) sub)
						   (apply-sub-to-dag (cdr x) sub (1+ depth)))) (dnode-sons dag))))
	  (make-dnode1 newdags depth))
      (let ((newdags (mapcar #'(lambda (x) (cons (car x)
						 (apply-sub-to-dag (cdr x) sub (1+ depth)))) (dnode-sons dag))))
	(if (same-sublist (mapcar #'cdr newdags) (mapcar #'cdr (dnode-sons dag))) ;nothing changed
	    dag
	  (make-dnode1 newdags depth))))))

(defun rearrange-sublist (numlist oldvars sub)
  (let* ((realsub (mapcar #'cons oldvars numlist))
	 (newsub (mapcar #'(lambda (x) (cons (or (cdr (assoc (car x) sub)) ;the var was renamed
						 (and (not (rassoc (car x) sub)) (car x))) ;the var was ignored
					     (cdr x))) realsub)))
    (mapcar #'cdr (mapcar #'(lambda (x) (or (assoc x newsub) '(0 . 0))) oldvars))))

(defun count-subs-in-dag (dag)
  (if (listp dag) ;we know it's not h.o.
      (reduce #'* (mapcar #'count-subs-in-dag dag))
    (if (dnode-p dag) 
	(cadr (dnode-keys dag))
      1)))

(defun extract-sub-from (dag)
  (remove-if #'extract-sub-fn (extract-sub-from-real dag 0)))

(defun extract-sub-fn (elt) (equal (cdr elt) 0))

(defun extract-sub-from-real (dag depth)
  (if (null dag) (list nil)
    (append (mapcar #'cons (dvars-d depth) (caar (dnode-sons dag)))
	    (extract-sub-from-real (cdar (dnode-sons dag)) (1+ depth)))))

(defun daglist-will-merge (daglist &optional (touches))
  (if (memq 'delay daglist) t
    (if *skolem-matters*
	(if *primsubs*
	    (progn (setq *lookup* (get-lookup-list touches))
		   (daglist-will-merge-2 daglist))
	  (daglist-will-merge-2 daglist))
      (ndags-1 daglist))))

(defun merge-fn-1 (elt) (length (dnode-sons elt)))

(defun daglist-will-merge-2 (daglist &optional (subsofar nil) (primcheck nil) (depth 0) (vars nil))
  (when (null vars) (setq vars (dvars-d depth)))
  (setq daglist (parse-daglist daglist))
  (if (null (cdr daglist)) t
    (let ((check (dolist (v vars nil)
		   (when (assoc v *allb*) (return t)))))
      (dolist (sons (dnode-sons (car daglist)) nil)
	(when (daglist-recursive-merge-2 (car sons) (list (cdr sons)) (cdr daglist) vars
					 subsofar check primcheck depth)
	  (return t))))))

(defun daglist-recursive-merge-2 (elt next daglist vars subsofar check primcheck depth)
  (let (cktemp merge)
  (dolist (sons (dnode-sons (car daglist)) nil)
    (setq primcheck (or primcheck (and *primsubs* (primcheck-sublists elt (car sons)))))
    (setq merge (merge-if-compatible (car sons) elt))
    (when (and merge
	       (if (and (cdr daglist) 
			(dolist (n next t) (unless (dag-will-merge-2 (cdr sons) n subsofar (1+ depth)) (return nil))))
		   (daglist-recursive-merge-2 merge
					      (cons (cdr sons) next) (cdr daglist)
					      vars subsofar check primcheck (1+ depth))
		 (progn (setq cktemp (and check (get-check-allb vars merge)))
			(when cktemp (setq subsofar (append cktemp subsofar)))
			(and (not (or (and cktemp (cyclic-selection-dag subsofar))
				      (and primcheck (not (check-for-primsubs-2 subsofar *lookup*)))))
			     (dolist (n next t) (unless (dag-will-merge-2 (cdr sons) n subsofar (1+ depth)) (return nil)))
			     (daglist-will-merge-2 (cons (cdr sons) next) subsofar primcheck (1+ depth))))))
      (return t)))))

(defun parse-daglist (daglist)
  (setq daglist (remove-duplicates daglist))
  (if (null (cdr daglist)) daglist   
    (if (or (all-zeros (mapcar #'dnode-sons daglist))
	    (all-same (mapcar #'dnode-sons daglist)))
	(parse-daglist (mapcar #'parse-dlist-1 daglist))
      daglist)))

(defun parse-dlist-1 (elt) (cdar (dnode-sons elt)))

(defun all-zeros (sonlist)
  (let ((subs-used (remove-duplicates (reduce #'append (mapcar #'car (reduce #'append sonlist))))))
    (and (zerop (car subs-used)) (null (cdr subs-used)))))

(defun all-same (sonlist)
  (let ((lengths (remove-duplicates (mapcar #'length sonlist))))
    (when (equal lengths '(1))
      (setq lengths (mapcar #'car (reduce #'append sonlist)))
      (iterate-same lengths))))

(defun iterate-same (listoflists)
  (or (null (car listoflists))
      (and (eq (length (remove-duplicates (mapcar #'car listoflists))) 1)
	   (iterate-same (mapcar #'cdr listoflists)))))

(defun ndags-1 (daglist)
  (setq daglist (parse-daglist daglist))
  (if (null (cdr daglist)) t
    (ndags-recurse daglist)))

(defun ndags-recurse (daglist &optional (sonlist nil) (lower-daglist nil))
  (if (cdr daglist)
      (let (merge)
	(dolist (sons (dnode-sons (car daglist)) nil)
	  (setq merge (if sonlist (merge-if-compatible (car sons) sonlist) (car sons)))
	  (when (and merge
		     (dolist (n lower-daglist t) (unless (dag-will-merge-1 (cdr sons) n) (return nil)))
		     (ndags-recurse (cdr daglist) merge
				    (cons (cdr sons) lower-daglist)))
	    (return t))))
    (dolist (sons (dnode-sons (car daglist)) nil)
      (when (and (compatible-sublists (car sons) sonlist)
		 (dolist (n lower-daglist t) (unless (dag-will-merge-1 (cdr sons) n) (return nil)))
		 (ndags-1 (cons (cdr sons) lower-daglist)))
	(return t)))))

(defun n-compatible-sublists (sublists)
  (and (null (cdr (remove-duplicates (remove-if #'zerop (mapcar #'car sublists)))))
       (or (null (cdar sublists)) ;both lists are the same length
	   (n-compatible-sublists (mapcar #'cdr sublists)))))

(defun pair-will-merge (dag1 dag2)
  (if (< (dname dag1) (dname dag2))
      (rplaca (dnode-merge dag1) (insert-reverse-sorted (dname dag2) (car (dnode-merge dag1))))
    (rplaca (dnode-merge dag2) (insert-reverse-sorted (dname dag1) (car (dnode-merge dag2))))))

(defun pair-wont-merge (dag1 dag2)
  (if (< (dname dag1) (dname dag2))
      (rplacd (dnode-merge dag1) (insert-reverse-sorted (dname dag2) (cdr (dnode-merge dag1))))
    (rplacd (dnode-merge dag2) (insert-reverse-sorted (dname dag1) (cdr (dnode-merge dag2))))))
