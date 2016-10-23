;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)

;;;
;;; File: FLAVORING
;;; Package: WFFS
;;;
;;; defines the necessary macros and functions dealing with flavors
;;; of labels.
;;;
;;; Modified with new hash-table implementation of flavors 9/26/87 DAN

(deffile etrees-renumber
  (part-of expansion-tree)
  (extension clisp)
  (mhelp "Defines renumber-leaves and associated functions."))

(defvar *renamed-leaves-list* nil)

(defflag renumber-leaves
  (flagtype boolean)
  (default T)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		       (setq *renamed-leaves-list* nil)
		       (if (and newvalue (eproof-etree current-eproof))
			   (relabel-etree (eproof-etree current-eproof))))))
  (subjects jforms)
  (mhelp "If this flag is T, copies of leafN will be numbered 
leafN.1, leafN.2, etc. If the flag is NIL, they will be given
the next available number, as determined by an internal counter."))

(defun relabel-etree (etree)
  (unless skolem-default
	  (fill-selecteds etree))
  (when renumber-leaves
	(setq *renamed-leaves-list* nil)
	(relabel-etree-main etree)))

(defun relabel-etree-main (etree)
  (if (expansion-p etree)
      (rewrite-trees (etree-components etree))
    (dolist (l (etree-components etree)) (relabel-etree-main l))))

(defun rewrite-trees (etreelist)
;;rewrite a list of etrees that are supposedly copies of each other
  (unless (null etreelist)
  (let* ((next-etreelist (mapcar 'etree-components etreelist))
	 (net-lengths (remove-duplicates (mapcar 'length next-etreelist)))
	 (min-length (reduce 'min net-lengths))
	 (max-length (reduce 'max net-lengths)))
    (unless (null next-etreelist)
	    (if (neq min-length max-length)
					;then the trees are not all the same length, in which case...
		(if (expansion-p (car etreelist))
					;then it's OK, we just have etree nodes with different nos. of exps...
		    (progn (rename-given-leaves (remove-if-not 'leaf-p (reduce 'append next-etreelist)))
		    (rewrite-trees (reduce 'append next-etreelist)))
					;otherwise it's not OK; we must have a primsub in here somewhere...
		  (progn 
		    (setq next-etreelist (remove-if #'(lambda (x) (or (null x) (subs-here (etree-free-vars (car x)))))
					;(> min-length (length x))))
					;removed the above hack because it breaks if you substitute and *then* duplicate
					;MB Sun Mar 12 17:07:56 1995
						    next-etreelist))
		    (dolist (trees (shuffle-etrlists next-etreelist))
			    (rewrite-trees (remove-if 'null trees)))))
					;otherwise all things are the same length, so we recombine them...
	      (dolist (trees (shuffle-etrlists (remove-if 'null next-etreelist)))
		      (if (leaf-p (car trees)) (rename-given-leaves trees)
			(rewrite-trees (remove-if 'null trees)))))))))

(defun subs-here (efv-list)
  (if (null efv-list) nil
    (if (and (exp-var-p (car efv-list)) (neq (exp-var-var (car efv-list)) (exp-var-subst (car efv-list))))
	t
      (subs-here (cdr efv-list)))))

(defun shuffle-etrlists (l)
  (if (null (car l)) nil
    (cons (mapcar 'car l) (shuffle-etrlists (mapcar 'cdr l)))))

(defun rename-given-leaves (leaves)
  (let* ((leaves (sort leaves #'leaf-lessthan))
	 (leafa (car leaves))
	 (leaves (cdr leaves)))
    (do ((i 1 (1+ i))
	 (l leaves (cdr l)))
	((null l))
	(push (cons (etree-name (car l)) (conc-names (etree-name leafa) "." (princ-to-string i)))
	      *renamed-leaves-list*))))

(defun leaf-lessthan (a b)
  (let ((sa (princ-to-string (etree-name a)))
	(sb (princ-to-string (etree-name b))))
    (if (neq (length sa) (length sb))
	(< (length sa) (length sb))
      (if (string< sa sb) t nil))))

;show-prop-name applies to literals
(defun show-prop-name (lit)
  (when (literal-p lit)
  (or (cdr (assoc (literal-name lit) *renamed-leaves-list*))
      (literal-name lit))))

;show-other-name applies to literal names
(defun show-other-name (litname)
  (or (cdr (assoc litname *renamed-leaves-list*))
      litname))

(defun show-internal-name (litname)
  (or (car (rassoc litname *renamed-leaves-list*))
      litname))

(defun all-banned-acons (eh sel-below banned-alist)
  (let ((a (assoc eh banned-alist)))
    (if a
	(acons eh
	       (union sel-below (cdr a))
	       (remove a banned-alist))
      (acons eh sel-below banned-alist))))

(defun all-banned-append (ab1 ab2)
  (if ab1
      (all-banned-acons (caar ab1) (cdar ab1) 
			(all-banned-append (cdr ab1) ab2))
    ab2))

					; see the Programmer's Guide, section on "Checking Acyclicity of the Dependency Relation"
(defun fill-selecteds (etree)
  (setf (eproof-all-banned current-eproof) nil)  ; B(q) for q in Sigma_Q cup V_Q
  (setf (eproof-inst-exp-vars-params current-eproof) nil) ; S(t) for t in Theta_Q
  (let ((banned-alist nil)		; exp vars v associated with sel vars y dominated by some exp term t w/ v free in t
	(ievs nil)) ; instantiated evars (Sigma_Q) associated with sel vars beneath
  (dolist (exp (find-etree-nodes #'expansion-p etree))
    (do ((comp (expansion-components exp) (cdr comp))
	 (term (expansion-terms exp) (cdr term))
	 (pvs (reduce 'append (expansion-prim-vars exp))))
	((null comp))
      (when (exp-var-p (car term))
	(let ((sel-below (mapcar #'skolem-term-term 
				 (reduce 'append
					 (mapcar #'selection-terms (find-etree-nodes #'selection-p (car comp)))))))
	  (when sel-below
	    (let ((fvlist (free-vars-of (exp-var-subst (car term)))))
	      (when (neq (exp-var-var (car term)) (exp-var-subst (car term)))
		(push (cons (exp-var-var (car term)) fvlist) ievs)
		(setf (exp-var-selected (car term)) sel-below)
		(push (cons (exp-var-var (car term)) (exp-var-selected (car term)))
		      (eproof-all-banned current-eproof)))
	      (setq banned-alist (all-banned-acons (car term) sel-below banned-alist))
	      (dolist (h fvlist)
		(let ((eh (find-if #'(lambda (x) (and (exp-var-p x) (eq (exp-var-var x) h)))
				   pvs)))
		  (when eh
		    (setq banned-alist
		      (all-banned-acons eh sel-below banned-alist)))))))))))
  (dolist (a banned-alist)
    (setf (exp-var-selected (car a)) (cdr a))
    (push (cons (exp-var-var (car a)) (cdr a)) (eproof-all-banned current-eproof)))
  (let ((allb2 (apply #'append (mapcar #'cdr (eproof-all-banned current-eproof)))))
    (dolist (ssd ievs)
      (let ((i (intersection (cdr ssd) allb2)))
	(when i
	  (push (cons (car ssd) i) (eproof-inst-exp-vars-params current-eproof))))))))
	    
	      
