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

(deffile symsimp2
  (part-of etr-nat)
  (mhelp "Defines additional functions used for symmetric simplification."))


(defun single-mating-change (supports disj a b M)
  (let* ((nec nil)
	 (temp-flag t)
	 (old-M M)
	 (N nil)
	 (M* nil)
	 (etree (make-etree* (cons disj supports) (list a b)))
	 (conc (cadr (etree-components etree)))
	 (arb-pair nil)
	 (c (car (etree-components disj)))
	 (not-c (cadr (etree-components disj)))
	 (etree1 (make-etree* (cons c supports) (list a)))
	 (etree2 (make-etree* (cons not-c supports) (list b)))
	 (jform1 (etree-to-prop-jform etree1))
	 (jform2 (etree-to-prop-jform etree2))
	 (prop-classes nil))
    (declare (special prop-classes))
    (loop 
      (unless temp-flag
	(return-from single-mating-change 
		     (values 
		       (or (set-difference old-M M 
					   :test #'equal)
			   (set-difference M old-M 
				       :test #'equal))
		       M)))
      ;; don't want C, not C to become unnecessary to the proof
      (let ((conns-below-c 
	      (remove-if-not #'(lambda (x) 
				 (or (find-etree-node-name (car x) c t)
				     (find-etree-node-name (cdr x) c t)))
			     M))
	    (conns-below-not-c (remove-if-not #'(lambda (x) 
				 (or (find-etree-node-name (car x) not-c t)
				     (find-etree-node-name (cdr x) not-c t)))
			     M)))
	(when (and (= (length conns-below-c) 1)
		   (= (length conns-below-not-c) 1))
	  (return-from single-mating-change 
		     (values 
		       (or (set-difference old-M M 
					   :test #'equal)
			   (set-difference M old-M 
				       :test #'equal))
		       M)))
	(when (= (length conns-below-c) 1)
	  (pushnew (car conns-below-c) nec :test #'equal))
	(when (= (length conns-below-not-c) 1)
	  (pushnew (car conns-below-not-c) nec :test #'equal)))
      
      ;; step 2 of Pfenning's algorithm

      (when (or (null arb-pair) (contains-elt-in arb-pair c))
      (do* ((path (sym-find-cheapest-path jform1 nil)
		  (sym-find-alt-path (car path) (cdr path)))
	   (conns (find-connections-closing path M)
		  (find-connections-closing path M))
	   (empty-disj-path (contains-empty-disj path)))
	  ((null path))
	(unless (or empty-disj-path
		    (> (length conns) 1)
		    (not (contains-elt-in (car conns) disj)))
	  (unless (or (member (car conns) nec :test #'equal)
		       (remove-if #'(lambda (x) 
                                     (or (contains-elt-in x c)
                                         (contains-elt-in x not-c)))
                                 (separation-preserving-pairs 
                                  path (car conns) supports c a not-c b)))
	     (push (car conns) nec)))))

      (when (or (null arb-pair) (contains-elt-in arb-pair not-c))
      (do* ((path (sym-find-cheapest-path jform2 nil)
		  (sym-find-alt-path (car path) (cdr path)))
	   (conns (find-connections-closing path M)
		  (find-connections-closing path M))
	   (empty-disj-path (contains-empty-disj path)))
	  ((null path))
	(unless (or empty-disj-path
		    (> (length conns) 1)
		    (not (contains-elt-in (car conns) disj)))
	  (unless (or (member (car conns) nec :test #'equal)
                      (remove-if #'(lambda (x)
                                     (or (contains-elt-in x c)
                                         (contains-elt-in x not-c)))
                                 (separation-preserving-pairs 
                                  path (car conns) supports c a not-c b)))
	     (push (car conns) nec)))))

      ;; step 3 of Pfenning's algorithm
      (setq arb-pair
	    (car (let ((pairs (sort-possible-pairs M nec disj c not-c conc)))
;		   (when cl-user::*debug*
;		     (msgf "Poss-pairs:" pairs))
		   pairs)))

;      (when cl-user::*debug*
;	(msgf "2 " "NEC: " nec t "ARB-PAIR: " arb-pair t "M: " m t "N: " N t))
      
      (if arb-pair
	  ;; step 4 of Pfenning's algorithm
	  (progn
	    (setq temp-flag t)
	    (setq M* (remove arb-pair M :test #'equal) 
		  N M*)
	    (do* ((jform (if (contains-elt-in arb-pair c) jform1 jform2))
		  (path (sym-find-cheapest-path jform nil)
			  (sym-find-alt-path (car path) (cdr path)))
		    (conc* (if (contains-elt-in arb-pair c) a b))
		    (lemma (if (contains-elt-in arb-pair c) c not-c))
		    (conns (find-connections-closing path N)
			   (find-connections-closing path N)))
		
		  ((null path))
		;; don't want to make a mating in which disj is unnec.
		(unless conns
		  (let ((newconn 
			  (choose-path-closer conc* lemma 
					    (separation-preserving-pairs 
					      path arb-pair 
					      supports c a not-c b))))
		  (push newconn N)
		  (push newconn nec))))
	    (setq M N))
	  (setq temp-flag nil)))))

;;; We know that conns will consist of possible connections that will
;;; close the path that was opened by removing arb-pair.  But we don't
;;; want to choose a connection that would add more connections involving
;;; the lemma (c or not-c) which is involved in arb-pair -- that might
;;; be counterproductive.  Nor do we want to choose a connection that
;;; may make the lemma unnecessary after single-deletion.  So we
;;; prefer connections that do not involve the lemma, and prefer
;;; connections that do not involve the particular conclusion 
;;; for which the lemma is to be used.


(defun choose-path-closer (conc lemma conns)
  (car
    (stable-sort
      (stable-sort
	(remove-if #'(lambda (x) (contains-elt-in x lemma))
                   (copy-alist conns))
	#'(lambda (x y)
	    (and (not (contains-elt-in x conc))
		 (contains-elt-in y conc))))
      #'(lambda (x y)
	  (and (not (and (find-etree-node-name (car x) lemma)
			 (find-etree-node-name (cdr x) lemma)))
	       (and (find-etree-node-name (car y) lemma)
		    (find-etree-node-name (cdr y) lemma)))))
    ))



;;; here want to make a good decision about which pair should be
;;; removed from the mating
;;; prefer connections in c to not-c
;;; prefer connections in which the literal in c is negative,
;;; and those in which the literal in not-c is positive.

(defun sort-possible-pairs (M nec disj c not-c conn)
  (declare (ignore conn))
  (let ((poss (copy-alist
		(remove-if-not #'(lambda (x) (contains-elt-in x disj))
			       (set-difference M nec :test #'equal)))))
    (stable-sort 
      (sort poss
	    #'(lambda (x y)
		(and (contains-elt-in x c)
		     (contains-elt-in y not-c))))
      #'(lambda (x y) 
	  (let ((x-node (or (find-etree-node-name (car x) disj t)
			    (find-etree-node-name (cdr x) disj t)))
		(y-node (or (find-etree-node-name (car y) disj t)
			    (find-etree-node-name (cdr y) disj t))))
	    (or (and (find-etree-node-name (etree-name x-node) c t)
		     (find-etree-node-name (etree-name y-node) c t)
		     (not (positive-p x-node))
		     (positive-p y-node))
		(and (find-etree-node-name (etree-name x-node) not-c t)
		     (find-etree-node-name (etree-name y-node) not-c t)
		     (positive-p x-node)
		     (not (positive-p y-node)))))))))


(defun make-etree* (supports conclusions)
  (let* ((topnode (make-implication :positive nil :junctive 'con))
	 (left (cond ((cdr supports)
		      (make-econjunction :positive t :junctive 'con
					 :components supports))
		     (supports (car supports))
		     (t (make-true :positive t :junctive 'CON)))) ; cebrown 11/19/00 - added :junctive 'CON
	 (right (cond ((cdr conclusions)
		       (make-edisjunction :positive nil :junctive 'con
					  :components conclusions))
		      (conclusions (car conclusions))
		      (t (make-false :positive nil :junctive 'CON))))) ; cebrown 11/19/00 - added :junctive 'CON
    (setf (etree-components topnode)
	  (list left right))
    topnode))

(defun contains-empty-disj (path)
  (member-if #'(lambda (x) (and (disjunction-p x)
				(null (disjunction-components x))))
	     path))

(defun find-connections-closing (path conn-list)
  (let ((temp nil))
    (dolist (conn conn-list (nreverse temp))
      (when (and (member (car conn) path :key #'literal-name :test #'string=)
		 (member (cdr conn) path :key #'literal-name :test #'string=))
	(push conn temp)))))

(defun contains-elt-in (conn etree)
  (or (find-etree-node-name (car conn) etree t)
      (find-etree-node-name (cdr conn) etree t)))


(defun separation-preserving-pairs (path conn supports c a not-c b)
  (let ((conn-list nil)
	(a-supports (list* a c supports))
	(b-supports (list* b not-c supports)))
    (setq path
	  (if (contains-elt-in conn c)
	      (remove-if-not #'(lambda (x) (is-below a-supports x)) path)
	      (remove-if-not #'(lambda (x) (is-below b-supports x)) path)))
    (remove-if
      #'(lambda (x) (or (and (string-equal (car x) (car conn))
			     (string-equal (cdr x) (cdr conn)))
			(and (string-equal (car x) (cdr conn))
			     (string-equal (cdr x) (car conn)))))
      (dolist (jform1 path conn-list)
	(dolist (jform2 (cdr (memq jform1 path)))
	  (when (complementary-jform jform1 jform2)
	    (push (cons (literal-name jform1)
			(literal-name jform2))
		  conn-list))))
      )))


(defun complementary-jform (jform1 jform2)
  (declare (special prop-classes))
  (when (and (literal-p jform1) (literal-p jform2))
    (let ((class1 (or (literal-prop-class jform1)
		      (find-msprop-class jform1)))
	  (class2 (or (literal-prop-class jform2)
		      (find-msprop-class jform2))))
      (if (jform-pos jform1)
	  (and (not (jform-pos jform2)) 
	       (eq class1 class2))
	  (and (jform-pos jform2)
	       (eq class1 class2))))))

(defun is-below (list-of-etrees jform)
  (let ((name (literal-name jform)))
    (find-if #'(lambda (x) (find-etree-node-name name x t))
	     list-of-etrees)))

(defun sym-find-cheapest-path (jform path)
  (case (jform-type jform)
    ((literal universal existential)
     (cons jform path))
    (disjunction
      (if (disjunction-components jform)
	  (sym-find-cheapest-path (car (disjunction-components jform)) path)
	  (cons jform path)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (setq path (sym-find-cheapest-path conj path)))
	  (cons (make-literal :represents t :parent jform :name (generate-a-fixed-name-for jform)) path))
	  )))

(defun sym-find-alt-path (last-elt rem-path)
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (let ((alt-path
		  (let ((disj (cadr (member last-elt (disjunction-components
						       parent)
					    :test #'eq))))
		    (when disj
		      (sym-find-cheapest-path disj rem-path)))))
	    (if alt-path (sym-complete-alt-path-and parent alt-path)
		(sym-find-alt-path parent rem-path))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq (jform-represents last-elt) t))
	      (sym-find-alt-path parent rem-path)
	      (sym-find-alt-path (car rem-path) (cdr rem-path))))))))

(defun sym-complete-alt-path-and (jform path)
  (let ((parent (jform-parent jform)))
    (if parent
	(dolist (conj (cdr (member jform (conjunction-components parent)
				   :test #'eq))
		      (sym-complete-alt-path-or parent path))
	  (setq path (sym-find-cheapest-path conj path)))
	path)))

(defun sym-complete-alt-path-or (jform path)
  (let ((parent (jform-parent jform)))
    (if parent (sym-complete-alt-path-and parent path)
	path)))


;; want to find the shortest subpath closed by conn-list
;; then get alternate path from that

(defun sym-find-shortest-alt-path (path conn-list)
  (do ((path path (cdr path)))
      ((find-if #'(lambda (x) 
		      (let ((name (literal-name (car path))))
			(or (string= name (car x))
			    (string= name (cdr x)))))
		  (find-connections-closing path conn-list))
       (sym-find-alt-path (car path) (cdr path)))))
    

