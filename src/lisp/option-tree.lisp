;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :auto)
(part-of ms89)

;;; Written by Dan Nesmith

(deffile option-tree
  (part-of ms89)
  (extension lisp)
  (mhelp "Contains code implementing option trees."))

(defun update-option-tree (option-tree)
  (let ((exp-nodes (reverse (find-etree-nodes #'expansion-p)))
	(renumber-leaves nil))
    (update-option-tree-higher-order option-tree exp-nodes)
    (let ((*ignore-first-order-dups* 
	   (memq *option-tree-ms* '(ms90-3 ms90-9))))
      (update-option-tree-first-order option-tree exp-nodes))
    (setup-vars option-tree)))

;;; First order (just duplications)

(defun update-option-tree-first-order (option-tree exp-nodes)
  (let ((current-eproof master-eproof)
	(new-bottom-exp-nodes nil))
    (setq new-bottom-exp-nodes
	  (add-new-expansions-to-master-tree exp-nodes))
    (deepen-all-leaf-nodes option-tree new-bottom-exp-nodes)
    (deepen-all-new-leaf-nodes-ordered option-tree
				       new-bottom-exp-nodes)
    option-tree))

#+comment(defun add-new-expansions-to-master-tree (exp-nodes)
  (dolist (exp exp-nodes)
    (duplicate-var exp)
    (deepen-to-literals (car (last (etree-components exp)))))
  #+comment(mapcar #'(lambda (x) 
	      (cons (car (last (etree-components x)))
		    (expansions-below
		      (car (last (etree-components x))))))
	  exp-nodes)
  (mapcar #'(lambda (x) 
	      (nconc (expansions-above 
		      (car (last (etree-components x))))
		     (expansions-below
		      (car (last (etree-components x))))))
	  exp-nodes))
(defun add-new-expansions-to-master-tree (exp-nodes)
  (let ((duped-exps nil))
    (dolist (exp exp-nodes)
      ;; this unless exists for this reason.  If we are using
      ;; path-focused-duplication (*ignore-first-order-dups*), then
      ;; the only reason we are doing the first-order-duplications is
      ;; if we can later apply prim-subs to them.  So see if prim-subs
      ;; can indeed be applied now, and avoid generating them if possible.
      (unless (and *ignore-first-order-dups* 
		   (not (apply-prim-subst-for-var (car (sel-exp-terms exp)))))
	(push exp duped-exps)
	(duplicate-var exp)
	(deepen-to-literals (car (last (etree-components exp))))))
    (mapcar #'(lambda (x) 
		(nconc (expansions-above 
			(car (last (etree-components x))))
		       (expansions-below
			(car (last (etree-components x))))))
	    duped-exps)))
  
(defun deepen-all-leaf-nodes (tree bottom-nodes)
  (do ((tree tree (car trees))
       (trees nil
	      (cdr trees)))
      ((null tree))
    (if (option-tree-children tree)
	(setq trees (append trees (option-tree-children tree)))
	(progn
          (setf (option-tree-children tree)
                (mapcar #'(lambda (x) 
                            (make-new-option-tree-node tree x))
                        bottom-nodes))
          (mapc #'init-option-tree (option-tree-children tree))))))
	
(defun deepen-all-new-leaf-nodes-ordered (tree bottom-nodes)
  (do* ((leaves (option-tree-leaves tree)
		(append leaves (option-tree-children leaf)))
	(leaf (pop leaves) (pop leaves))
	(free-vars (option-tree-free-vars leaf)
		   (if leaf (option-tree-free-vars leaf)))
	(other-vars-list 
	  (cdr (member (car free-vars) bottom-nodes :test #'equal))
	  (cdr (member (car free-vars) bottom-nodes :test #'equal))))
      ((null leaf) tree)
    (when other-vars-list
      (deepen-all-new-leaf-nodes-ordered-aux leaf other-vars-list))))

(defun deepen-all-new-leaf-nodes-ordered-aux (leaf other-vars)
  (let ((new-kids (mapcar #'(lambda (x) (make-new-option-tree-node
					  leaf x))
			  other-vars)))
    (mapc #'init-option-tree new-kids)
    (setf (option-tree-children leaf) new-kids)))


;;; Higher order, i.e., primitive substitutions

(defun update-option-tree-higher-order (option-tree exp-nodes)
  (let* ((current-eproof master-eproof)
	 (ho-exps (find-ho-exps exp-nodes))
	 (first-born (get-first-born ho-exps)))
    (do ((exps (cdr ho-exps) (cdr exps))
	 (exp (car ho-exps) (car exps))
	 (first-born first-born (cdr first-born))
	 (new-exps nil))
	((null exp) option-tree)
      (setq new-exps
	    (make-higher-order-expansions exp))
      (add-ho-exps-to-option-tree new-exps option-tree (car first-born)))))

(defun make-higher-order-expansions (exp-node)
  (let ((len (length (etree-components exp-node)))
	(new-exps nil))
    (apply-prim-subs exp-node)
    (setq new-exps
	  (subseq (etree-components exp-node) len))
    (mapc #'deepen-to-literals* new-exps)
    (setq new-exps
	  (subseq (etree-components exp-node) len))
    #+comment(mapcar #'(lambda (x) (cons x (expansions-below x)))
	    new-exps)
    (mapcar #'(lambda (x) (nconc (expansions-above x) (expansions-below x)))
	    new-exps)))


(defun most-used-parent (nodes)
  (let ((parents (mapcar #'(lambda (x) (etree-parent (car x)))
			 nodes)))
    (do* ((len 0 (length parents))
	  (cur-parent nil (car parents))
	  (parents parents
		   (delete cur-parent parents))
	  (diff 0 (- len (length parents)))
	  (max-so-far 0 (if (> diff max-so-far)
			    diff
			  max-so-far)))
	((null parents) max-so-far))))
	

#+comment(defun add-ho-exps-to-option-tree (new-exps option-tree first-born)
  (when (null new-exps)
    (return-from add-ho-exps-to-option-tree nil))
  (let* ((first-born-exps 
	  (cons first-born (expansions-below first-born)))
	 (first-born-nodes (find-option-tree-nodes
			     first-born-exps
			     option-tree)))
    (dolist (first-born-node first-born-nodes)
      (dolist (new-exp new-exps)
	(let ((parent (etree-parent (car new-exp)))
	      (last-node nil)
	      (new-nodes nil))
	  (do ((node first-born-node
		     (option-tree-back node))
	       (last nil node))
	      ((null node)(setq last-node last))
	    (let ((total-nodes
		   (append (option-tree-free-vars node)
			   (list new-exp))))
	      (when 

		  (setq new-nodes (cons (make-new-option-tree-node 
					 node 
					 total-nodes) 
					new-nodes))
		(setf (option-tree-parent (car new-nodes)) nil)
		(setf (option-tree-children (car new-nodes)) 	 
		  (mapcar
		   #'(lambda (x) (copy-option-tree* x (car new-nodes)))
		   (option-tree-children node))))))
	  (push (make-new-option-tree-node* first-born-node (list new-exp))
		new-nodes)
	  (setf (option-tree-children (car new-nodes)) 	 
	    (mapcar
	     #'(lambda (x) (copy-option-tree* x (car new-nodes)))
	     (option-tree-children first-born-node)))
      ;;; place each new node in the axis option-tree 
	  (dolist (node new-nodes)
	    (setf (option-tree-back last-node) node)
	    (setf (option-tree-forward node) last-node)
	    (setq last-node node))
	  (dolist (node new-nodes)
	    (init-option-tree-and-kids node)))))))

(defvar *subset-limit* 1)

(defun make-all-non-empty-subsets (n set)
  (if (null set)
      nil
    (let ((n-1-sets (make-all-non-empty-subsets n (cdr set))))
      (append n-1-sets 
	      (mapcar #'(lambda (x) (cons (car set)
					  x))
		      (remove-if #'(lambda (y) (>= (length y) n))
				 n-1-sets))
	      (list (list (car set)))))))

(defun find-option-tree-first-born-nodes (free-vars option-tree same-level)
  (let* ((me-found 
	  (subsetp free-vars (flatten (option-tree-free-vars option-tree))))
	 (kids-found (mapcan #'(lambda (x) (find-option-tree-first-born-nodes
					    free-vars x nil))
			     (option-tree-children option-tree)))
	 (back-found (if (option-tree-back option-tree)
			 (find-option-tree-first-born-nodes free-vars
						 (option-tree-back
						  option-tree)
						 (or me-found same-level)))))
    
    (if (and me-found (not same-level))
	(cons option-tree (append kids-found back-found))
      (append kids-found back-found))))

(defun duplicate-parent-found (old-opt new-opt parent)
  (let ((pos (mismatch old-opt new-opt)))
    (eq parent (etree-parent (nth pos old-opt)))))

;;;The following function is modified to update 
;;;the SKOLEM-CONSTANTS slot of the eproof attached to option-tree
;;;(HX Aug, 29, 93)
(defun add-ho-exps-to-option-tree (new-exps option-tree first-born)
  (when (null new-exps)
    (return-from add-ho-exps-to-option-tree nil))
  (let* ((parent (etree-parent first-born))
	 (new-exps 
	  (make-all-non-empty-subsets *subset-limit* new-exps))
	 (first-born-exps 
	  (cons first-born (expansions-below first-born)))
	 (first-born-nodes #+comment(find-option-tree-nodes
			     first-born-exps
			     option-tree)
			   (find-option-tree-first-born-nodes 
			    first-born-exps option-tree nil)))
    (dolist (first-born-node first-born-nodes)
      (let ((new-nodes nil)
	    (last-node 
	     (do ((node first-born-node (or (option-tree-back
					     node)
					    node)))
		 ((not (option-tree-back node)) node))))
	(dolist (new-exp new-exps)
	  (let ()
	    (do ((node first-born-node
		       (option-tree-back node))
		 (last nil node))
		((null node)(setq last-node last))
	      (let* ((non-sib-nodes 
		      (remove-if #'(lambda (x) 
				     (duplicate-parent-found x (car new-exp)
							     parent))
				 (option-tree-free-vars node)))
		     (total-nodes
		      (append non-sib-nodes new-exp))
                     (new-node (make-new-option-tree-node node total-nodes)))
                (setf (eproof-skolem-constants (option-tree-eproof new-node))
                      (eproof-skolem-constants current-eproof))
		(setq new-nodes (cons new-node new-nodes))
		(setf (option-tree-parent (car new-nodes)) nil)
		(setf (option-tree-children (car new-nodes)) 	 
		  (mapcar
		   #'(lambda (x) (copy-option-tree* x (car new-nodes)))
		   (option-tree-children node)))
		#+comment(push (make-new-option-tree-node* first-born-node 
							   new-exp)
			       new-nodes)
		(setf (option-tree-children (car new-nodes)) 	 
		  (mapcar
		   #'(lambda (x) (copy-option-tree* x (car new-nodes)))
		   (option-tree-children first-born-node)))))))
      ;;; place each new node in the axis option-tree 
	(dolist (node new-nodes)
	  (setf (option-tree-back last-node) node)
	  (setf (option-tree-forward node) last-node)
	  (setq last-node node))
	(dolist (node new-nodes)
	  (init-option-tree-and-kids node))))))

