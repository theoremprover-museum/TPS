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

(deffile ms98-weights
  (part-of ms98)
  (extension lisp)
  (mhelp "The functions to handle weightings and numbered lists for MS98-1"))

(defun e-intersection-banme (elt)
  (e-ordered-intersection (sort (copy-list (car elt)) #'<) *banme*))


(defun insert-reverse-sorted (elt list)
  (if (null list) (list elt)
    (if (= (car list) elt) list
      (if (> (car list) elt) (cons (car list) (insert-reverse-sorted elt (cdr list)))
	(cons elt list)))))

(defun reverse-ordered-intersection (list1 list2)
  (if (or (null list1) (null list2)) nil
    (if (= (car list1) (car list2)) 
	(cons (car list1) (reverse-ordered-intersection (cdr list1) (cdr list2)))
      (if (< (car list1) (car list2))
	  (reverse-ordered-intersection list1 (cdr list2))
	(reverse-ordered-intersection (cdr list1) list2)))))

(defun reverse-ordered-memq (elt list)
  (if (or (null list) (> elt (car list))) nil
    (if (< elt (car list)) (reverse-ordered-memq elt (cdr list))
      t)))

(defun reverse-ordered-union (list1 list2)
  (if (null list1) list2
    (if (null list2) list1
      (if (= (car list1) (car list2)) 
	  (cons (car list1) (reverse-ordered-union (cdr list1) (cdr list2)))
	(if (< (car list1) (car list2))
	    (cons (car list2) (reverse-ordered-union list1 (cdr list2)))
	  (cons (car list1) (reverse-ordered-union (cdr list1) list2)))))))

(defun e-ordered-setdiff (list1 list2)
  (if (or (null list1) (null list2)) list1
    (if (= (car list1) (car list2))
	(e-ordered-setdiff (cdr list1) (cdr list2))
      (if (< (car list1) (car list2)) t
	(e-ordered-setdiff list1 (cdr list2))))))

(defun ordered-setdiff (list1 list2)
  (if (or (null list1) (null list2)) list1
    (if (= (car list1) (car list2))
	(ordered-setdiff (cdr list1) (cdr list2))
      (if (< (car list1) (car list2))
	  (cons (car list1) (ordered-setdiff (cdr list1) list2))
	(ordered-setdiff list1 (cdr list2))))))

(defun e-ordered-intersection (list1 list2) ;;ordered-intersection is nonempty.
  (if (or (null list1) (null list2)) nil
    (if (= (car list1) (car list2)) t
      (if (> (car list1) (car list2))
	  (e-ordered-intersection list1 (cdr list2))
	(e-ordered-intersection (cdr list1) list2)))))

(defun ordered-intersection (list1 list2)
  (if (or (null list1) (null list2)) nil
    (if (= (car list1) (car list2)) 
	(cons (car list1) (ordered-intersection (cdr list1) (cdr list2)))
      (if (> (car list1) (car list2))
	  (ordered-intersection list1 (cdr list2))
	(ordered-intersection (cdr list1) list2)))))
      
(defun ordered-union (list1 list2)
  (if (null list1) list2
    (if (null list2) list1
      (if (= (car list1) (car list2)) 
	  (cons (car list1) (ordered-union (cdr list1) (cdr list2)))
	(if (> (car list1) (car list2))
	    (cons (car list2) (ordered-union list1 (cdr list2)))
	  (cons (car list1) (ordered-union (cdr list1) list2)))))))

(defun insert-sorted (elt list)
  (if (null list) (list elt)
    (if (= (car list) elt) list
      (if (< (car list) elt) (cons (car list) (insert-sorted elt (cdr list)))
	(cons elt list)))))

(defun ccs-measure (comp nd)
  (setf (gethash (component-name comp) *vafindex*) *vaf*)
  (when ms98-verbose (msgf (parse-clist (component-clist comp)) "  "))
  (let ((weight
	 (case ms98-measure
	   (0 (- (ccs-measure-3 comp) *vaf*))
	   (1 (ccs-measure-1 comp nd))
	   (2 (ccs-measure-2 comp nd))
	   (3 (ccs-measure-3 comp))
	   (4 (ccs-measure-4 comp nd))
	   (5 (ccs-measure-5 comp))
	   (6 (ccs-measure-6 comp))
	   (7 (ccs-measure-7 comp))
	   (8 (ccs-measure-8 comp))
	   (9 (ccs-measure-9 comp))
	   (10 (- (ccs-measure-9 comp) *vaf*))
	   (11 (- (ccs-measure-6 comp) *vaf*))
	   (12 (- (ccs-measure-8 comp) *vaf*))
	   (13 (- (ccs-measure-13 (component-clist comp)) *vaf*))
	   (14 (- (ccs-measure-7 comp) *vaf*))
	   (15 (- (ccs-measure-15 comp) *vaf*))
	   (t -9999999))))
    (when ms98-verbose (msg weight t))
    weight))

(defun precompute-weight (blocks touches conns)
  (case ms98-measure
    (0 (- (length blocks) *vaf*))
    (1 (precompute-1 blocks touches conns (length disj-assoc)))
    ((2 4) (if blocks nil -9999999))
    (3 (length blocks))
    (5 (precompute-5 blocks touches))
    (6 (precompute-6 blocks touches conns))
    (7 (precompute-7 conns))
    (8 (precompute-8 (length blocks) (length touches)))
    (9 (precompute-9 blocks touches conns))
    (10 (- (precompute-9 blocks touches conns) *vaf*))
    (11 (- (precompute-6 blocks touches conns) *vaf*))
    (12 (- (precompute-8 (length blocks) (length touches)) *vaf*))
    (13 (- (ccs-measure-13 conns) *vaf*))
    (14 (- (precompute-7 conns) *vaf*))
    (15 (- (precompute-15 blocks touches conns) *vaf*))
    (t -9999999)))

(defun precompute-15 (b to c)
  (+ (/ (precompute-6 b to c) 4)
     (* (precompute-7 c) 50)))

(defun ccs-measure-15 (comp)
  (+ (/ (ccs-measure-6 comp) 4)
     (* (ccs-measure-7 comp) 50)))

(defun ccs-measure-1 (comp nd)
  (if (component-blocks comp)
      ;idea: we want a high measure for a component that blocks a lot and touches at least something,
      ; but not very much more...
      (let ((touches (- (length (component-touches comp)) (length (component-blocks comp))))
	    (blocks (length (component-blocks comp)))
	    (conns (length (component-clist comp))))
	(- (- (* 100 blocks) (* 25 (if (zerop touches) (- nd blocks) touches))) (* 5 conns)))
    -9999999))

(defun precompute-1 (b tt c n)
  (if b (let ((tl (- (length tt) (length b)))
	      (bl (length b))
	      (cl (length c)))
	  (- (- (* 100 bl) (* 25 (if (zerop tl) (- n bl) tl)) (* 5 cl))))
    -9999999))

(defun ccs-measure-2 (comp nd)
  (if (component-blocks comp)
      ;idea: we want a high measure for a component that blocks a lot and touches at least something,
      ; but not very much more...
      (let ((touches (- (length (component-touches comp)) (length (component-blocks comp))))
	    (blocks (length (component-blocks comp)))
	    (conns (length (component-clist comp))))
	(- (- (- (* 100 blocks) 
		 (* 25 (if (zerop touches) (- nd blocks) touches)))
	      (* 5 conns))
	   (make-subcount (component-final-subs comp))))
    -9999999))

(defun ccs-measure-3 (comp)
  (length (component-blocks comp)))

(defun ccs-measure-4 (comp nd)
  (if (component-blocks comp)
      ;idea: we want a high measure for a component that blocks a lot and touches at least something,
      ; but not very much more...
      (let* ((touches (- (length (component-touches comp)) (length (component-blocks comp))))
	     (blocks (length (component-blocks comp)))
	     (conns (length (component-clist comp)))
	     (to-consider (if lengths 
			      (if (zerop touches)
				  (cdr (assoc 'fail *length-list*))
				(cdr (assoc (fewest-extensions (component-touches comp) lengths) *length-list*)))
			    0)))
	(- (- (- (* 100 blocks) 
		 (* 25 (if (zerop touches) (- nd blocks) touches)))
	      (* 5 conns))
	   (+ to-consider (make-subcount (component-final-subs comp)))))
    -9999999))

(defun ccs-measure-5 (comp)
  (if (component-blocks comp)
      ;idea: we want a high measure for a component that blocks a lot and touches at least something,
      ; but not very much more...
      (let* ((touches (- (length (component-touches comp)) (length (component-blocks comp))))
	     (to-consider (if lengths 
			      (if (zerop touches)
				  (cdr (assoc 'fail *length-list*))
				(cdr (assoc (fewest-extensions (ordered-setdiff (component-touches comp) 
									(component-blocks comp))
							       lengths) *length-list*)))
			    0)))
	(- 0 
	   to-consider))
    -9999999))

(defun precompute-5 (b tt)
  (if b (let* ((tl (- (length tt) (length b)))
	       (tc (if lengths (if (zerop tl)
				   (cdr (assoc 'fail *length-list*))
				 (cdr (assoc (fewest-extensions (ordered-setdiff tt b) lengths) *length-list*)))
		     0)))
	  (- 0 tc))
    -9999999))

(defun ccs-measure-6 (comp)
  (+ (* 100 (/ (length (component-blocks comp)) (length (component-clist comp))))
     (* 100 (/ (length (component-blocks comp)) (length (component-touches comp))))))

(defun precompute-6 (b tt c)
  (+ (* 100 (/ (length b) (length c)))
     (* 100 (/ (length b) (length tt)))))

(defun ccs-measure-7 (comp)
  (let ((count 0))
  (dolist (c (component-clist comp) (/ count (length (component-clist comp))))
    (let ((n1 (literal-name (car c)))
	  (n2 (literal-name (cdr c))))
      (when (or (and (member n1 *positive-leaves*) (member n2 *negative-leaves*))
		(and (member n2 *positive-leaves*) (member n1 *negative-leaves*)))
	(incf count))))))

(defun precompute-7 (conns)
  (let ((count 0))
  (dolist (c conns (abs count))
    (let ((n1 (literal-name (car c)))
	  (n2 (literal-name (cdr c))))
    (if (or (and (memq n1 *positive-leaves*) (memq n2 *negative-leaves*))
	    (and (memq n2 *positive-leaves*) (memq n1 *negative-leaves*)))
	(incf count)
      (decf count))))))

(defun ccs-measure-8 (comp)
  (if (= (length (component-blocks comp)) (length (component-touches comp)))
      (* 100 (/ (length (component-blocks comp)) (1+ (length (component-blocks comp)))))
    (* 100 (/ (length (component-blocks comp)) (length (component-touches comp))))))

(defun precompute-8 (b tt)
  (if (= b tt)
      (* 100 (/ b (1+ b)))
    (* 100 (/ b tt))))

(defun ccs-measure-9 (comp)
  (+ (- 100 (* 10 (- (* 2 (length (component-clist comp))) (length (remove-duplicates (component-lits comp))))))
     (- (+ (length (component-clist comp)) (length (component-blocks comp)))
	(length (component-touches comp)))))

(defun precompute-9 (b tt c)
  (let ((lits (remove-duplicates (reduce 'append (mapcar #'(lambda (x) (list (car x) (cdr x))) c)))))
    (+ (- 100 (* 10 (- (* 2 (length c)) (length lits))))
       (- (+ (length c) (length b))
	  (length tt)))))

(defun ccs-measure-13 (clist)
  (let ((lits (remove-duplicates (reduce #'append (mapcar #'(lambda (x) (list (car x) (cdr x))) clist)))))
    (* 100 
       (/ (length lits) ;;actual no. of lits
	  (* 2 (length clist)))))) ;;expected no of lits (if MM were 1)
	
(defun ccs-length-measure (list-of-comps)
  (length list-of-comps))

(defun last-non-dup (len)
  (dolist (l len)
    (when (assoc l dup-priority)
      (return l))))

(defun ccs-length-measure-2 (len comps)
;  (msgf t t len t t disj-assoc t t dup-priority)
  (let ((chosen-elt (if (eq ms98-first-fragment t)
			(last-non-dup len)
		      (if (integerp ms98-first-fragment)
			  (let ((literal-name (concatenate 'string (princ-to-string leaf-name) 
							   (princ-to-string ms98-first-fragment)))
				literal)
			    (setq literal
				  (dolist (l (jform-to-literal-list ms90-3-jform nil) nil)
				    (when (string= (literal-name l) literal-name)
				      (return l))))
			    (if literal
				(dolist (d disj-assoc (last-non-dup len))
				  (when (and (memq literal (cadr d)) ;literal is in this fragment
					     (assoc (car d) dup-priority)) ; this fragment is an original copy
				    (return (car d))))
			      (last-non-dup len)))
			nil))))
  (case ms98-fragment-order
    (0 (ccs-length-measure-a len comps chosen-elt))
    (1 (ccs-length-measure-b len comps chosen-elt))
    (2 (ccs-length-measure-c len comps chosen-elt))
    (3 (ccs-length-measure-d len chosen-elt))
    (t (ccs-length-measure-b len comps chosen-elt)))))

(defun ccs-length-measure-a (len comps chosen-elt)
  (let (retlist)
    (dolist (l len retlist)
      (if (eq l chosen-elt)
	  (push (cons l -9999) retlist)
	(progn 
	  (push (cons l (length (remove-if-not #'(lambda (y) (memq l (component-blocks y))) comps)))
		retlist)
	  (when (and (not ms98-base-prim)
		     (memq l (reduce #'append *prim-lookup*)))
	    (rplacd (car retlist) (* 100000 (cdar retlist)))))))))

(defun ccs-length-measure-b (len comps chosen-elt)
  ;;look ahead 2 steps to see how many components we might generate...
  (let (retlist)
    (dolist (l len retlist)
      (let ((touchl (mapcar #'helper-function-26
			    (remove-if-not #'(lambda (y) (memq l (component-blocks y))) comps))))
	(setq touchl (mapcar #'helper-function-27 touchl))
	;;for each component blocking l, find the next component to be added
	(if (eq l chosen-elt)
	    (push (cons l -9999) retlist)
	  (progn
	    (push (cons l (reduce #'+ (mapcar #'helper-function-28 touchl))) retlist)
	    (when (and (not ms98-base-prim)
		       (memq l (reduce #'append *prim-lookup*)))
	      (rplacd (car retlist) (* 100000 (cdar retlist))))))))))

(defun ccs-length-measure-c (len comps chosen-elt)
  ;;look ahead 2 steps to see how many components we might generate...
  (let (retlist)
    (dolist (l len retlist)
      (let ((touchl (mapcar #'helper-function-26
			    (remove-if-not #'(lambda (y) (memq l (component-blocks y))) comps))))
	(setq touchl (mapcar #'helper-function-27 touchl))
	;;for each component blocking l, find the next component to be added
	(if (eq l chosen-elt)
	    (push (cons l -9999) retlist)
	  (progn
	    (push (cons l (reduce #'+ (mapcar #'helper-function-28 touchl))) retlist)
	    (if (and (not ms98-base-prim)
		     (memq l (reduce #'append *prim-lookup*)))
		(rplacd (car retlist) (* 100000 (cdar retlist)))
	      (when (null (intersection (free-vars-of (jform-to-gwff (caddr (cdr (assoc l disj-assoc))))) 
					(free-vars-in-etree current-topnode)))
		(rplacd (car retlist) (ceiling (cdar retlist) 1000))))))))))
	 
(defun ccs-length-measure-d (len chosen-elt)
  (let (retlist)
    (dolist (l len retlist)
      (if (eq l chosen-elt)
	  (push (cons l -9999) retlist)
	(progn
	  (push (cons l (- 1000 l)) retlist) ;because we count components from the top down.
	  (when (and (not ms98-base-prim)
		     (memq l (reduce #'append *prim-lookup*)))
	    (rplacd (car retlist) (* 100000 (cdar retlist)))))))))
	 
(defun ordered-memq (elt list)
  (if (or (null list) (< elt (car list))) nil
    (if (> elt (car list)) (ordered-memq elt (cdr list))
      t)))

(defun ordered-union-ck (list1 list2)
  (setq *sortedvar* nil) ;will become T if any elements of list1 are added that weren't already there...
  (ordered-union-2 list1 list2))

(defun ordered-union-2 (list1 list2)
  (if (null list1) list2
    (if (null list2) (progn (setq *sortedvar* t) ;because list1 isn't nil
			    list1)
      (if (= (car list1) (car list2)) 
	  (cons (car list1) (ordered-union-2 (cdr list1) (cdr list2)))
	(if (> (car list1) (car list2))
	    (cons (car list2) (ordered-union-2 list1 (cdr list2)))
	  (progn (setq *sortedvar* t) ;car list1 is about to be added.
		 (cons (car list1) (ordered-union-2 (cdr list1) list2))))))))
      
(defun make-subcount (root)
  (if (or (not (listp root))
	  (and (listp root) (dnode-p (car root))))
      (count-subs-in-dag root)
    (length root)))
