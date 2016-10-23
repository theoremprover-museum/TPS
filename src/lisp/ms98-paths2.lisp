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

(deffile ms98-paths2
  (part-of ms98)
  (extension lisp)
  (mhelp "Functions that implement the minimality checker in MS98-1"))

(context ms88) ;same context as complete-p, which seems reasonable.

(defmateop minimal-p
  (mate-result-> ignore)
  (mate-alias minimal-p))

(defwffop minimal-p
  (argnames)
  (argtypes)
  (applicable-q (lambda ()  (if active-mating t
			      (complain "No mating in progress."))))
  (resulttype ignore)
  (mhelp "A mating M is non-minimal if it contains some connection 
c such that M-{c} spans exactly the same vertical paths as M.
MINIMAL-P will find such a connection if it exists; otherwise
it will report that the mating is minimal."))

(defun minimal-p ()
  (setq ms90-3-jform (init-position (etree-to-jform (eproof-etree current-eproof))))
  (pathnum-init-jform ms90-3-jform)
  (remove-sk-labels ms90-3-jform)
  (mark-ext-exp-nodes ms90-3-jform)
  (setq disj-assoc (fiddle-disjs (ccs-reorganize)))
  (setq fragjform (make-frag-jform ms90-3-jform))
  (if (eq (car fragjform) 'OR)
      (progn
	(dolist (fj (copy-list (cdr fragjform)) (msgf "Mating is minimal."))
	  (setq fragjform fj)
	  (let ((foo (internal-minimal-fn)))
	    (when foo 
	      (msgf "Mating is not minimal; connection " foo 
		    " is unnecessary.")
	      (return nil))))
	(setq fragjform (make-frag-jform ms90-3-jform)))
    (let ((foo (internal-minimal-fn)))
      (if foo 
	  (msgf "Mating is not minimal; connection " foo 
		" is unnecessary.")
	(msgf "Mating is minimal.")))))
	    
(defun internal-minimal-fn ()
  (let ((mating (mapcar #'(lambda (x) (car (gethash x (connections-array))))
			(mating-clist active-mating)))
	(lits (flatten-fake-jform fragjform)))
    (setq mating 
	  (remove-if #'(lambda (x) (setdiff (find-touches-2 (list (car x) (cdr x))) lits)) mating))
    (violates-minimality mating nil)))

(defun find-touches-2 (lits)
  (declare (special disj-assoc))
  (let ((lits (mapcar #'literal-name lits))
	return)
    (dolist (d disj-assoc (sort return #'<))
	    (when (intersection (mapcar #'literal-name (cadr d)) lits)
		  (setq return (cons (car d) return))))))

(defun turn-into-literal-pair (c)
  (let ((conn (car (gethash c (connections-array))))
	(litlist (jform-to-literal-list ms90-3-jform nil)))
    (cons (dolist (l litlist)
	    (when (eq (literal-name l) (leaf-name (car conn)))
	      (return l)))
	  (dolist (l litlist)
	    (when (eq (literal-name l) (leaf-name (cdr conn)))
	      (return l))))))

;(defvar matings-checked 0)
;(defvar matings-rejected 0)
;;the two vars above, and the two commented lines in the function below, 
;;can be used to estimate the proportion of non-minimal matings being found...

(defun violates-minimality (mating &optional (output t))
;  (incf matings-checked)
  (do ((this-conn (car mating) (car conns))
       (conns (cdr mating) (cdr conns))
       (done-conns nil (cons this-conn done-conns)))
      ((null this-conn) nil)
    (unless (ccs-minimal-p (append done-conns conns) 
			   (sort (find-touches-2 (reduce #'append (mapcar #'(lambda (x) (list (car x) (cdr x)))
						       (append done-conns conns)))) #'<)
			   (sort (find-touches-2 (list (car this-conn) (cdr this-conn))) #'<)
			   this-conn)
      (when output (msg "!"))
;      (incf matings-rejected)
      (return this-conn))))

(defun ccs-minimal-p (clist oldtouch newtouch conn)
  (declare (special ms90-3-jform))
  ;;returns T if clist does not already block all the paths through conn.
  (let* ((keep-these-fragments (ordered-union (hack-to-ribbons fragjform oldtouch newtouch) newtouch))
	 (delete-these-lits (setdiff (frag-to-lit newtouch) (list (car conn) (cdr conn))))
	 (delete-these-fragments (ordered-setdiff (flatten-fake-jform fragjform) keep-these-fragments))
	 (oldjform (copy-jform ms90-3-jform))
	 (newjform (edit-jform oldjform (append delete-these-lits (frag-to-lit delete-these-fragments)))))
    (or (null newjform)
	(not (null (ccs-find-next-openpath newjform (mapcar #'complete-fn-1 clist)))))))

(defun hack-to-ribbons (fake-jform oldtouch newtouch)
  (if (eq (car fake-jform) 'AND)
      (let (conjuncts ft)
	(dolist (x (cdr fake-jform) 
		   (sort (remove-duplicates (reduce #'append (mapcar #'flatten-fake-jform conjuncts))) #'<))
	  (setq ft (flatten-fake-jform x))
	  (when (e-ordered-intersection ft oldtouch) ;something in this outer conjunct was touched
	    (if (e-ordered-intersection ft newtouch) ;and one of the new fragments is in here
		(let ((frag (collapse-to-newtouch x newtouch)))
		  (unless (exists-untouched-vpath frag oldtouch)
		    (push frag conjuncts)))
;;		(push (collapse-to-newtouch x newtouch) conjuncts)
	      ;;o/w this is touched but none of the new fragments is here
	      (unless (exists-untouched-vpath x oldtouch) ;in which case there's always an open path in here
		(push x conjuncts))))))
;;    (if (eq (car fake-jform) 'OR)
;;	(sort (reduce #'append (mapcar #'(lambda (x) (hack-to-ribbons x oldtouch newtouch)) (cdr fake-jform))) #'<)
;; fake-jform should always be a conjunction.
      (throwfail "Huh?")))
;;)

#+comment(defun collapse-to-newtouch (fake-jform newtouch)
  (if (listp fake-jform)
      (if (eq (car fake-jform) 'AND)
	  (let (newconj)
	    (dolist (c (cdr fake-jform)
		       (reduce #'append newconj))
	      (if (e-ordered-intersection (flatten-fake-jform c) newtouch)
		  (push (collapse-to-newtouch c newtouch) newconj)
		(push (flatten-fake-jform c) newconj))))
	(let (newor)
	  (dolist (x (cdr fake-jform) 
		     (reduce #'append newor))
	    (when (e-ordered-intersection (flatten-fake-jform x) newtouch)
	      (push (collapse-to-newtouch x newtouch) newor)))))
    (if (ordered-memq fake-jform newtouch) (list fake-jform) nil)))

(defun collapse-to-newtouch (fake-jform newtouch)
  (if (listp fake-jform)
      (if (eq (car fake-jform) 'AND)
	  (let (newconj)
	    (dolist (c (cdr fake-jform)
		       (progn (setq newconj (remove-if #'null newconj))
			      (if (> (length newconj) 1)
				  (cons 'AND newconj)
				(car newconj))))
	      (if (e-ordered-intersection (flatten-fake-jform c) newtouch)
		  (push (collapse-to-newtouch c newtouch) newconj)
		(push c newconj))))
	(let (newor)
	  (dolist (x (cdr fake-jform) 
		     (progn (setq newor (remove-if #'null newor))
			      (if (> (length newor) 1)
				  (cons 'OR newor)
				(car newor))))
	    (when (e-ordered-intersection (flatten-fake-jform x) newtouch)
	      (push (collapse-to-newtouch x newtouch) newor)))))
    (if (ordered-memq fake-jform newtouch) fake-jform nil)))

(defun exists-untouched-vpath (fake-jform touched)
  (if (listp fake-jform)
      (if (eq (car fake-jform) 'AND)
	  (dolist (x (cdr fake-jform) t)
	    (unless (exists-untouched-vpath x touched) (return nil)))
	(dolist (x (cdr fake-jform) nil)
	  (when (exists-untouched-vpath x touched) (return t))))
    (if (ordered-memq fake-jform touched) nil t)))
