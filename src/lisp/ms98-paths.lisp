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

(deffile ms98-paths
  (part-of ms98)
  (extension lisp)
  (mhelp "Functions that implement the completeness checker in MS98-1"))

(defun ccs-complete-p (clist comp)
  (declare (special ms90-3-jform))
  (let ((openfrag (get-shortest-open-fragments fragjform (component-touches comp))))
    (if (neq openfrag 'fail)
	(progn ;(msgf "Component " (parse-clist clist) " closing " (component-touches comp) " has openfrag " openfrag)
	       (setf (component-openpath comp) (cons 'OR openfrag))
	       nil)
      (let* ((oldjform (copy-jform ms90-3-jform))
	     (get-rid-of (ccs-etree-deletable clist))
	     (get-rid-of-2 (frag-to-lit (unused-fragments-of fragjform (component-touches comp))))
	     (newjform (edit-jform oldjform (append get-rid-of get-rid-of-2)))
	     (extendpath (set-difference (get-leftpath ms90-3-jform) (get-leftpath newjform) :key #'literal-name))
	     (openpath (ccs-find-next-openpath
			newjform (mapcar #'complete-fn-1 clist))))
;	(when (null get-rid-of) (msgf ">>" get-rid-of-2 "  " extendpath))
	(when (null openpath) (return-from ccs-complete-p t))
	(setq openpath (trim-path openpath) extendpath (trim-path extendpath))
	(setq openpath (cons openpath extendpath)) ;(append openpath extendpath)))
;	(msgf (component-touches comp) t fragjform t clist t (component-clist comp) t openpath t t t )
	(setf (component-openpath comp) (or (make-litkey-by-name openpath) openpath))
	nil))))

(defun complete-fn-1 (x) (cons (literal-name (car x)) (literal-name (cdr x))))

(defun frag-to-lit (fraglist)
  (let (result)
    (dolist (f fraglist (mapcar #'literal-name result))
      (setq result (append (cadr (assoc f disj-assoc)) result)))))

(defun unused-fragments-of (listjform touches)
  (if (listp listjform)
      (if (eq (car listjform) 'AND)
	  (reduce #'append (mapcar #'(lambda (x) (unused-fragments-of x touches)) (cdr listjform)))
	;;otherwise (eq (car listjform) 'OR)
	(let ((result (touched-literals-of listjform)))
	  (if (intersection result touches) nil result)))
    (if (memq listjform touches) nil (list listjform))))
	
(defun touched-literals-of (listjform)
  (if (listp listjform)
      (reduce #'append (mapcar #'touched-literals-of (cdr listjform)))
    (list listjform)))
  
(defun trim-path (litlist)
  (if *live-leaves* (trim-path-real litlist)
    litlist))

(defun trim-path-real (litlist)
  (if (null litlist) nil
    (if (member (literal-name (car litlist)) (mapcar #'leaf-name *live-leaves*))
	(cons (car litlist) (trim-path-real (cdr litlist)))
      (trim-path-real (cdr litlist)))))

(defun get-leftpath (jform)
  (if (null jform) nil
    (case (jform-type jform)
      (universal (get-leftpath (universal-scope jform)))
      (conjunction (reduce #'append (mapcar #'get-leftpath (conjunction-components jform))))
      (disjunction (get-leftpath (car (disjunction-components jform))))
      (literal (list jform)))))

(defun ccs-find-next-openpath (jform clist)
  (multiple-value-bind (path closed)
      (ccs-find-cheapest-mspath jform nil clist)
    (do ()
	((or (not (and closed path)) (numberp closed))
	 path)
      (multiple-value-setq (path closed)
	(ccs-find-alt-mspath (car path) (cdr path) clist)))))

(defun ccs-complete-mspath (last-elt path clist)
  (let ((parent (jform-parent last-elt)))
    (if parent
        (if (eq (jform-type parent) 'conjunction)
            (ccs-complete-alt-mspath-and parent last-elt path clist)
            (ccs-complete-mspath parent path clist))
        path)))

(defun ccs-find-cheapest-mspath (jform path clist)
  (case (jform-type jform)
    (literal
      (if (ccs-open-mspath-p jform path clist)
	  (cons jform path)
	(values (cons jform path) t)))
    (disjunction
      (if (disjunction-components jform)
	  (ccs-find-cheapest-mspath
	    (car (disjunction-components jform)) path clist)
	  (values (cons jform path) t)))
    (conjunction
      (if (conjunction-components jform)
	  (dolist (conj (conjunction-components jform) path)
	    (multiple-value-bind (newpath closed)
		(ccs-find-cheapest-mspath conj path clist)
	      (if closed (return (values newpath closed))
		  (setq path newpath))))
	  (cons 'stop path)))
    (universal 
     (ccs-find-cheapest-mspath
      (universal-scope jform) path clist))))

(defun ccs-find-alt-mspath (last-elt rem-path clist)
  (let ((parent (jform-parent last-elt)))
    (when parent
      (case (jform-type parent)
	(disjunction
	  (multiple-value-bind (alt-path closed)
	      (let ((disj (cadr (memq last-elt (disjunction-components
                                                parent)))))
		(when disj (ccs-find-cheapest-mspath
			     disj rem-path clist)))
	    (if closed (values alt-path closed)
		(if alt-path (ccs-complete-mspath
			       parent alt-path clist)
		    (ccs-find-alt-mspath
		      parent rem-path clist)))))
	(conjunction
	  (if (or (eq (car (conjunction-components parent)) last-elt)
		  (eq last-elt 'stop))
	      (ccs-find-alt-mspath parent rem-path clist)
	      (ccs-find-alt-mspath
		(car rem-path) (cdr rem-path) clist)))
        (universal (ccs-find-alt-mspath parent rem-path clist))))))

(defun ccs-complete-alt-mspath-and (parent jform path clist)
  (dolist (conj (cdr (memq jform (conjunction-components parent)))
                (ccs-complete-mspath parent path clist))
    (multiple-value-bind (newpath closed)
        (ccs-find-cheapest-mspath conj path clist)
      (if closed (return (values newpath closed))
          (setq path newpath)))))

(defun ccs-open-mspath-p (literal path clist)
  "Returns NIL if adding LITERAL to PATH closes the PATH, else returns T."
  (let ((name (literal-name literal))
	(newpath (mapcar #'literal-name path)))
    (dolist (c clist t)
	    (if (or (and (eq (car c) name) (memq (cdr c) newpath))
		    (and (eq (cdr c) name) (memq (car c) newpath)))
		(return nil)))))

(defun make-litkey-by-name (pair)
  (let ((result (cons (make-litkey-by-name-real (car pair)) (make-litkey-by-name-real (cdr pair)))))
    (if (car result) result nil)))

(defun make-litkey-by-name-real (litlist)
  (declare (special primehash2))
  (let ((reallits (jform-to-literal-list ms90-3-jform nil))
	(litnames (mapcar #'literal-name litlist))
	newlits)
    (dolist (r reallits)
      (when (memq (literal-name r) litnames) (push r newlits)))
    (if (null newlits) 0
      (reduce #'*-or-nil (mapcar #'litkey-fn-1 newlits)))))

(defun litkey-fn-1 (x) (gethash x primehash2))

(defun cleanup-ccs-fn-1 (x) (list (literal-name (car x)) (literal-name (cdr x))))

; modified this to deal with nonleaf conns and dissolved conns - cebrown 11/16/01
(defun ccs-etree-cleanup (connlist dissolve-conns)
  (let ((litlist (append (reduce #'append (mapcar #'cleanup-ccs-fn-1 connlist))
			 (mapcar #'car dissolve-conns)
			 (mapcar #'cdr dissolve-conns))))
    (ccs-etree-cleanup-real (mapcar #'find-etree-node-name litlist))))

; modified this to deal with nonleaf conns and dissolved conns - cebrown 11/16/01
(defun ccs-etree-cleanup-real (leaves)
  (let ((expnodes (find-etree-nodes #'expansion-p)))
    (dolist (exp expnodes)
      (do ((subnodes (cdr (etree-components exp)) (cdr subnodes))
	   (terms (cdr (expansion-terms exp)) (cdr terms)))
	  ((null subnodes))
	(unless (find-etree-node
		 #'(lambda (e) (member e leaves))
		 (car subnodes))
	  (msgf "deleting subnode " (car subnodes) t) ; delete me
	  (setf (etree-components exp) (delete (car subnodes) (etree-components exp)))
	  (setf (expansion-terms exp) (delete (car terms) (expansion-terms exp))))))))

(defun ccs-etree-deletable (connlist)
  (let ((litlist (reduce #'append (mapcar #'cleanup-ccs-fn-1 connlist))))
    (ccs-etree-deletable-real (mapcar #'find-etree-node-name litlist))))

(defun ccs-etree-deletable-real (leaves)
  (let ((expnodes (find-etree-nodes #'expansion-p))
	retlist)
    (dolist (exp expnodes retlist)
      (do ((subnodes (if (disjunct-above exp) (cdr (etree-components exp)) (etree-components exp)) (cdr subnodes))
	   (terms (if (disjunct-above exp) (cdr (expansion-terms exp)) (expansion-terms exp)) (cdr terms))
	   leaves2)
	  ((null subnodes))
	(setq leaves2 (find-etree-nodes #'leaf-p (car subnodes)))
	(unless (intersection leaves2 leaves)
	  (setq retlist (append (mapcar #'leaf-name leaves2) retlist)))))))

(defun disjunct-above (node)
  (if node
      (if (and (or (edisjunction-p node) (econjunction-p node) (implication-p node))
	       (eq (etree-junctive node) 'dis))
	  t
	(disjunct-above (etree-parent node)))
    nil))
;;the point of the above is that if FORALL x A is a major conjunct which isn't touched, we can ignore any copy of it.
;;However, if it occurs in the context B OR FORALL x A, then we mustn't ignore the first copy; there may
;;be a path through it when all paths through B are blocked.

(defun edit-jform (jform lits)
  (let ((result (edit-jform-real jform lits)))
    (unless (null result)
      (setf (jform-parent result) nil)
      result)))

(defun edit-jform-real (jform lits)
  (case (jform-type jform)
    (universal (if (subsetp (mapcar #'literal-name (jform-to-literal-list jform nil)) lits :test #'string=)
		   nil
		 (let ((newj (copy-jform jform)))
		   (setf (universal-scope newj) (edit-jform-real (universal-scope newj) lits))
		   (setf (jform-parent (universal-scope newj)) newj)
		   (if (universal-scope newj) newj nil))))
    (conjunction (if (subsetp (lits-contained jform) lits :test #'string=)
		     nil
		   (let ((newj (copy-jform jform)))
		     (setf (conjunction-components newj)
			   (remove-if #'null 
				      (mapcar #'(lambda (x) (edit-jform-real x lits)) (conjunction-components newj))))
		     (dolist (c (conjunction-components newj)) (setf (jform-parent c) newj))
		     (if (> (length (conjunction-components newj)) 1) newj 
		       (if (conjunction-components newj) (car (conjunction-components newj)) nil)))))
    (disjunction (if (subsetp (lits-contained jform) lits :test #'string=)
		   nil
		   (let ((newj (copy-jform jform)))
		     (setf (disjunction-components newj)
			   (remove-if #'null 
				      (mapcar #'(lambda (x) (edit-jform-real x lits)) (disjunction-components newj))))
		     (dolist (c (disjunction-components newj)) (setf (jform-parent c) newj))
		     (if (> (length (disjunction-components newj)) 1) newj 
		       (if (disjunction-components newj) (car (disjunction-components newj))
			 nil)))))
    (literal (if (member (literal-name jform) lits :test #'string=)
		   nil
	       (copy-jform jform)))))

(defun lits-contained (jform)
  (mapcar #'literal-name (jform-to-literal-list jform nil)))

(defun make-frag-jform (jform)
  (case (jform-type jform)
    (universal (make-frag-jform (universal-scope jform)))
    (conjunction (let ((comps (remove-duplicates (mapcar #'make-frag-jform (conjunction-components jform))))
		       newcomps)
		   (if (> (length comps) 1)
		       (dolist (c comps (cons 'AND newcomps))
			 (if (and (listp c) (eq (car c) 'AND)) (setq newcomps (append newcomps (cdr c)))
			   (setq newcomps (append newcomps (list c)))))
		     (if (= (length comps) 1) (car comps) nil))))
    (disjunction (let ((comps (remove-duplicates (mapcar #'make-frag-jform (disjunction-components jform))))
		       newcomps)
		   (if (> (length comps) 1)
		       (dolist (c comps (cons 'OR newcomps))
			 (if (and (listp c) (eq (car c) 'OR)) (setq newcomps (append newcomps (cdr c)))
			   (setq newcomps (append newcomps (list c)))))
		     (if (= (length comps) 1) (car comps) nil))))
    (literal (dolist (d disj-assoc)
	       (when (member jform (cadr d))
		 (return (car d)))))))

(defun get-shortest-open-fragments (fake-jform tlist)
  (let ((sorted (sort (copy-list tlist) #'<)))
    (or (lookup-openfrag-list sorted (length sorted))
	(store-openfrag-list sorted (g-s-o-f fake-jform sorted)))))

(defun g-s-o-f (fake-jform tlist)
  (let ((result (getshortopenfrag fake-jform tlist)))
;    (msgf fake-jform t tlist "   --> " result t t)
    result))
    
(defun gsof-helper-fn-1 (x) (eq (car x) nil))

(defun getshortopenfrag (fake-jform tlist &optional (top t) (touchedp nil))
  (if (listp fake-jform)
      (if (eq (car fake-jform) 'AND)
	  (if top
	      (let ((newand 
		     (remove-if #'(lambda (x) 
				    (let ((flat (flatten-fake-jform x)))
				    (or (not (e-ordered-intersection flat tlist)) ;not touched
					(not (e-ordered-setdiff flat tlist))))) ;completely touched,
				(cdr fake-jform))))
		(if (> (length newand) 1)
		    (reduce #'append (mapcar #'(lambda (x) (getshortopenfrag x tlist nil t)) newand))
		  (if (null newand) 
		      nil
		    (getshortopenfrag (car newand) tlist nil t))))
	    (let ((newand (mapcar #'(lambda (x) 
				      (let ((flat (ordered-intersection (flatten-fake-jform x) tlist)))
					(cons flat
					      (getshortopenfrag x tlist nil flat))))
				  (cdr fake-jform)))
		  foo)
	      (setq foo (remove-if #'gsof-helper-fn-1 newand)) ;something was touched
	      (if foo (progn (setq foo nil)
			     (dolist (n newand)
			       (unless (or (and (eq (car n) t) (eq (cdr n) nil)) ;touched & no open paths
					   (and (eq (car n) nil))) ;untouched but open paths
				 (push (cdr n) foo))))
		(if touchedp
		    (setq foo (mapcar #'cdr newand))
		    (setq foo nil) ))
	      (reduce #'append foo)))
	;;o/w (eq (car fake-jform) 'OR)
	(get-shortest-list (remove-if #'null (mapcar #'(lambda (x) 
							 (if (or (not touchedp)
								 (e-ordered-intersection (flatten-fake-jform x) tlist))
							     (getshortopenfrag x tlist nil touchedp)
							     (get-left-fragpath x)))
						     (cdr fake-jform))))
	)
    (if (or (member fake-jform tlist) 
	    (member fake-jform *dead-fragments*))
	nil (list fake-jform))))
	      
(defun get-left-fragpath (list)
  (if (listp list)
      (if (eq (car list) 'OR)
	  (get-left-fragpath (cadr list))
	(if (eq (car list) 'AND)
	    (reduce #'append (mapcar #'get-left-fragpath (cdr list)))
	  list))
    (list list)))

(defun store-openfrag-list (tl result)
  (setq result (or result 'fail))
  (push (cons (cons (length tl) tl) result) *openfrag-list*)
  result)

(defun lookup-openfrag-list (tl len)
  (dolist (o *openfrag-list* nil)
    (when (and (= (caar o) len)
	       (proper-tlist-check tl (cdar o)))
      (return (cdr o)))))

(defun proper-tlist-check (tlist1 tlist2)
  ;;we know they're the same length
  (if (null tlist1)
      t
    (if (equal (car tlist1) (car tlist2))
	(proper-tlist-check (cdr tlist1) (cdr tlist2))
      nil)))

(defun get-shortest-list (l)
  (do ((l1 (cdr l) (cdr l1))
       (current (car l))
       (lgh (ccs-list-length (car l))))
      ((null l1) current)
    (when (< (ccs-list-length (car l1)) lgh) (setq current (car l1)
						   lgh (ccs-list-length (car l1))))))

(defun ccs-list-length (list)
  (if (null list) 0
    (+ (or (nth (car list) *complete-weights*) 0) (ccs-list-length (cdr list)))))

(defun flatten-fake-jform (fake-jform)
  (sort (flatten-fake-jform-2 fake-jform) #'<))

(defun flatten-fake-jform-2 (fake-jform)
  (if (listp fake-jform)
      (reduce #'append (mapcar #'flatten-fake-jform-2 (cdr fake-jform)))
    (list fake-jform)))
