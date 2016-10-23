;;; -*- Mode:LISP; Package:AUTO -*-
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

(deffile option-tree-aux
  (part-of ms89)
  (extension lisp)
  (mhelp "Contains auxiliary code for dealing with option trees."))


(defun print-option-tree (tree *standard-output* level)
  (declare (ignore level))
  (princ "[") (princ (option-tree-name tree)) (princ " : ") (princ (option-tree-free-vars tree))
  (princ " : ") (princ (eproof-name (option-tree-eproof tree))) (princ " : ")
  (princ (option-tree-ranking tree)) (princ "]"))

(defun timetonum (number)
  (declare (special search-time-limit))
  (let ((max-time (or search-time-limit (if first-order-mode-ms 8 30))))
    (1+ (/ number max-time))))

(defun print-option-tree-subs (tree)
  (progn
  (vpwin-update-four tree)
  (let ((vars (remove-duplicates (flatten (option-tree-free-vars* tree))))
	(subs-exist nil))
    (msg "Considering option tree " (option-tree-name tree) " for the " (timetonum (option-tree-tried tree)) (ordinal-print (timetonum (option-tree-tried tree))) "time." t) 
    (if monitorflag (funcall (symbol-function *current-monitorfn*) 'new-otree (list (cons 'otree (princ-to-string (option-tree-name tree))))))
    (msg "Substitutions: " t)
    (dolist (var vars)
      (let ((exp-var (nth (position var (etree-components (etree-parent
						       var)))
		      (expansion-terms (etree-parent var)))))
	(if (not (eq (exp-var-var exp-var) (exp-var-subst exp-var)))
	    (progn
	      (msg ((exp-var-var exp-var) . gwff) "  -->  "
		   ((exp-var-subst exp-var) . gwff) t)
	      (setq subs-exist t)))))
    (if (not subs-exist) (msg "None." t)))))

(defun print-option-tree-long (option-tree)
  (let ((current-eproof (option-tree-eproof option-tree)))
    (msg t t)
    (msgf (option-tree-name option-tree) ":")
    (msgf "Proof name: " (eproof-name current-eproof))
    (msgf "Free-vars:" (option-tree-free-vars option-tree))
    (msgf "Ranking:" (option-tree-ranking option-tree))
    (msgf "Children:")
    (mapc #'(lambda (x) (msg (option-tree-name x)
			     1))
	  (option-tree-children option-tree))
    (msg "  Back:")
    (if (option-tree-back option-tree) 
	(msg (option-tree-back option-tree)))
    (msg "  Forward:")
    (if (option-tree-forward option-tree) 
	(msg (option-tree-forward option-tree)))
    (msg "  Parent:")
    (if (option-tree-parent option-tree) 
	(msg (option-tree-parent option-tree)))
    (mapcar #'print-option-tree-long (option-tree-children option-tree))
    (if (option-tree-back option-tree) 
	(print-option-tree-long (option-tree-back option-tree)))))

(defun num-vpaths-ranking (option-tree)
  (let ((current-eproof (option-tree-eproof option-tree)))
    (+ (option-tree-ranking option-tree)
       (number-of-vertical-paths 
        (eproof-etree (option-tree-eproof option-tree))))))

(defun count-leaves (option-tree)
  (+ (option-tree-ranking option-tree)
     (count-if-not #'dead-node-p 
		   (eproof-leaf-list (option-tree-eproof option-tree)))))

(defun expansions-below (etree)
  "Search the tree, return the nodes immediately below each
expansion node which lies below ETREE."
  ; want to make sure that all component lists are copied
  (apply #'concatenate
	 (cons 'list (mapcar #'etree-components 
			     (find-exp-nodes etree)))))

(defun expansions-above (etree)
  (do ((l nil)
       (node* etree
              (etree-parent node*)))
      ((null node*) l)
    (if (expansion-p (etree-parent node*))
        (push node* l))))

;;; find all nodes below node and above node which are children of
;;; expansion nodes

(defun find-exps-above-and-below (etree)
  "Returns list of all children of expansion nodes which lie above
ETREE or below ETREE."
  (append (expansions-above etree)
	  (expansions-below etree)))



(defun find-exp-nodes (etree)
  "Return list of all expansion nodes which lie below ETREE."
  (find-etree-nodes #'expansion-p etree nil t))


(defun find-common-ancestor (etree nodes)
  (if (= 1 (length nodes)) 
      (car nodes)
      (do ((root etree)
	   (oldroot etree (if root root oldroot)))
	  ((null root) oldroot)
	(setq root
	      (find-if #'(lambda (x) (every #'(lambda (y) (find-etree-node-name
							    (etree-name y) x))
					    nodes))
		       (etree-components oldroot))))))


(defun option-tree-leaves (tree)
  (do ((leaves nil)
       (tree tree (car others))
       (others nil))
      ((null tree) leaves)
    (when (null (option-tree-children tree))
      (push tree leaves))
    (setq others (append (cdr others) (option-tree-children tree)))))


(defun make-new-option-tree-node (parent nodes)
  (let ((new-node
	  (make-option-tree :parent parent 
			    :free-vars 
			    (if (listp (car nodes)) nodes (list nodes))
			    :eproof (if parent
					(copy-eproof*
					  (option-tree-eproof 
					    parent)))
			    :children nil )))
    new-node))

(defun flatten (list-of-lists)
  (apply #'append list-of-lists))


(defun make-new-option-tree-node* (model nodes)
  (let* ((new-node
	  (make-option-tree :parent nil :free-vars nodes
			    :eproof (copy-eproof*
				      (option-tree-eproof 
					model))
			    :children nil ))
	(current-eproof (option-tree-eproof new-node)))
    (dolist (var (flatten (option-tree-free-vars model)))
      #+comment(setf (eproof-statuses current-eproof)
            (delete (etree-name var)
                    (eproof-statuses current-eproof)
                    :key #'car :test #'string-equal))
      (update-status nil var 0))
    new-node))

(defun copy-eproof* (eproof)
  (let ((new-eproof 
	 (make-eproof :etree (eproof-etree eproof)
		      :all-banned (eproof-all-banned eproof)
		      :inst-exp-vars-params (eproof-inst-exp-vars-params eproof)
	       :Free-vars-in-etree
	       (copy-list (eproof-free-vars-in-etree eproof))
	       :skolem-constants
	       (copy-list (eproof-skolem-constants eproof))
	       :substitution-list
	       (copy-list (eproof-substitution-list eproof))
	       :leaf-list
	       (eproof-leaf-list eproof)
	       :skolem-method
	       (eproof-skolem-method eproof)
	       :symmetry
	       (eproof-symmetry eproof))))
    (setf (symbol-value (eproof-name new-eproof)) new-eproof)))

(defun copy-option-tree* (option-tree parent)
  (unless option-tree (return-from copy-option-tree* nil))
  (let* ((new-option-tree (make-option-tree :parent parent
					   :free-vars 
					   (option-tree-free-vars
                                            option-tree)
					   :eproof 
                                           (copy-eproof*
                                            (option-tree-eproof option-tree))
					   :children nil 
					   ))
	 (current-eproof (option-tree-eproof new-option-tree)))
    (setf (eproof-mating-list (option-tree-eproof new-option-tree))
	  nil)
    (setf (option-tree-children new-option-tree)
	  (mapcar #'(lambda (x) (copy-option-tree* x new-option-tree))
		  (option-tree-children option-tree)))
    (setf (option-tree-back new-option-tree)
	  (copy-option-tree* (option-tree-back option-tree) nil))
    (when (option-tree-back new-option-tree)
      (setf (option-tree-forward (option-tree-back new-option-tree))
	new-option-tree))
    new-option-tree))

(defun find-ho-exps (exps)
  (remove-if-not
    #'(lambda (x) (higher-order-var (bindvar (expansion-shallow x))))
    exps))

(defun get-first-born (exp-nodes)
  (mapcan #'(lambda (x) (list (car (etree-components x)))) exp-nodes))

(defun find-option-tree-node (free-vars option-tree)
  (when (subsetp free-vars (flatten (option-tree-free-vars option-tree)))
   (return-from find-option-tree-node option-tree))
  (do ((result nil)
       (nodes (append (option-tree-children option-tree)
		      (if (option-tree-back option-tree)
			  (list (option-tree-back option-tree))))
	      (cdr nodes)))
      ((or result (null nodes)) result)
    (find-option-tree-node (car nodes) option-tree)))

(defun find-option-tree-nodes (free-vars option-tree)
  (let ((kids-found (mapcan #'(lambda (x) (find-option-tree-nodes
					    free-vars x))
			    (option-tree-children option-tree)))
	(back-found (if (option-tree-back option-tree)
			(find-option-tree-nodes free-vars
						(option-tree-back option-tree)))))
    (if (subsetp free-vars (flatten (option-tree-free-vars option-tree)))
	(cons option-tree (append kids-found back-found))
	(append kids-found back-found))))

;;; Initializing an option tree

(defun init-option-tree (option-tree)
  (init-option-tree-statuses option-tree)
  (init-option-tree-free-vars option-tree)
  (init-option-tree-ranking option-tree))

(defun init-option-tree-statuses (option-tree)
  (let ((current-eproof (option-tree-eproof option-tree)))
    (update-statuses-in-eproof (option-tree-free-vars* option-tree))))

#+comment(defun init-option-tree-free-vars (option-tree)
  (let ((current-eproof (option-tree-eproof option-tree)))
    (setf (eproof-free-vars-in-etree current-eproof)
          (remove-if #'(lambda (x) 
                         (let ((var (car x)) (exp (cdr x)))
                           (zerop (auto::etree-status* 
                                   (nth (position var 
                                                  (auto::expansion-terms exp)
                                                  :test #'free-in)
                                        (etree-components exp))))))
                     (auto::eproof-free-vars-in-etree master-eproof)
                     ))))

;;; Don't try to initialize this, just use the value in the master-eproof
;;; Saves a lot of time. DAN 20MAY91

(defun init-option-tree-free-vars (option-tree)
  (let ((current-eproof (option-tree-eproof option-tree)))
    (setf (eproof-free-vars-in-etree current-eproof)
      (eproof-free-vars-in-etree master-eproof))))



(defvar *make-jform-fn* 'etree-to-prop-jform)

(defun init-option-tree-jform (option-tree)
  (let ((current-eproof (option-tree-eproof option-tree)))
    (setf (eproof-jform (option-tree-eproof option-tree))
	  (funcall *make-jform-fn*
                   (eproof-etree current-eproof)))))

(defun init-option-tree-ranking (option-tree)
  (setf (option-tree-ranking option-tree) 
    (if *ignore-first-order-dups*
	0
	(funcall (symbol-function rank-eproof-fn) option-tree))))

;;; Recursively descends from option-tree
(defun init-option-tree-and-kids (option-tree)
  (init-option-tree option-tree)
  (mapc #'init-option-tree-and-kids (option-tree-children option-tree)))


#+comment(defun update-statuses-in-eproof (free-vars)
  (setq free-vars (mapcan #'expansions-above (flatten free-vars)))
  (setf (eproof-statuses current-eproof) nil)
  (do ((node (eproof-etree current-eproof) (car nodes))
       (nodes nil (cdr nodes)))
      ((null node) (eproof-statuses current-eproof))
    (setf (eproof-statuses current-eproof)
	  (acons (etree-name node) 1 (eproof-statuses
				      current-eproof)))
    (if (expansion-p node)
	(setq nodes 
	      (append nodes 
		      (remove-if-not #'(lambda (x)
					 (memq x free-vars))
					   (etree-components node))))
	(setq nodes (append nodes (etree-components node))))))

(defun update-statuses-in-eproof (free-vars)
  (setq free-vars (mapcan #'expansions-above (flatten free-vars)))
  (do ((node (eproof-etree current-eproof) (car nodes))
       (nodes nil (cdr nodes)))
      ((null node) (eproof-statuses current-eproof))
    (update-status nil node 1)
    (if (expansion-p node)
	(setq nodes 
	      (append nodes 
		      (remove-if-not #'(lambda (x)
					 (memq x free-vars))
					   (etree-components node))))
	(setq nodes (append nodes (etree-components node))))))

(defun setup-vars (node)
  (if (null node) nil
      (progn (setup-vars (option-tree-back node))
	     (mapc #'setup-vars (option-tree-children node))
	     (setf (symbol-value (option-tree-name node)) node))))

(defun option-tree-parent* (tree)
  (do* ((tree tree (option-tree-forward tree))
	(parent (option-tree-parent tree)
		(if tree (option-tree-parent tree))))
      ((or parent (null tree))
       (if parent parent nil))))


(defun option-tree-free-vars* (tree)
  (if (null tree) 
      nil
      (append (option-tree-free-vars* (option-tree-parent* tree))
	      (option-tree-free-vars tree))))

(defun expunge-old ()
  (do-symbols (sym (find-package "CL-USER"))
    (when (or (and (boundp sym)
		 (option-tree-p (symbol-value sym))
		 (> (length (string sym)) 3)
		 (string-equal (subseq (string sym) 0 3) "OPT"))
	    (and (boundp sym)
		 (eproof-p (symbol-value sym))
		 (> (length (string sym)) 3)
		 (string-equal (subseq (string sym) 0 3) "EPR")))
	(setf (symbol-value sym) nil)
        (unintern sym))))


;;;The following expunge is more effective in some sense.

(defun expunge ()
  (let (valuelist)
    (do-symbols (sym (find-package "CL-USER"))
       (when (or (and (boundp sym)
		      (option-tree-p (symbol-value sym))
		      (> (length (string sym)) 3)
		      (string-equal (subseq (string sym) 0 3) "OPT"))
		 (and (boundp sym)
		      (etree-p (symbol-value sym))
		      (> (length (string sym)) 3)
		      (member (subseq (string sym) 0 3) '("CON" "DIS" "EXP" "FAL" "IMP" 
							  "LEA" "NEG" "REW" "SEL" "SKO" "TRU")))
	         (and (boundp sym)
		      (eproof-p (symbol-value sym))
		      (> (length (string sym)) 3)
		      (string-equal (subseq (string sym) 0 3) "EPR")))
	     (push (symbol-value sym) valuelist))
       (when (and (not (boundp sym)) (> (length (string sym)) 3) 
		  (member (string-right-trim "1234567890" (string sym)) 
			  '("CONJ" "DISJ" "EXP" "FALSE" "IMP" "MAT" "EPR" "OPT"
			    "LEAF" "NEG" "REW" "SEL" "SKOL" "TRUE") :test 'string=))
	     (unintern sym "CL-USER")))
     (dolist (val valuelist)
         (do-symbols (sym1 (find-package "CL-USER"))
             (when (and (boundp sym1) (eq (symbol-value sym1) val))
                         (setf (symbol-value sym1) nil) (unintern sym1 "CL-USER"))))))

(defun expunge-vars ()
  (let ((exempted-vars nil))
    (do-symbols (sym (find-package "CL-USER"))
		(when (gwff-q sym) (setq exempted-vars (remove-duplicates (append (get-variables sym) exempted-vars)
									  :test 'eq))))
    (do-symbols (sym (find-package "CL-USER"))
		(when (get sym 'core::ren-counter)
		      (setf (get sym 'core::ren-counter) 1))
		(when (and (not (memq sym exempted-vars))
			   (lsymbol-p sym) (propsym-p sym) (propsym-q sym) (not (get sym 'pmpropsym)))
		      (setf (symbol-value sym) nil)
		      (unintern sym)))))

(defun get-variables (sym)
  (let ((retlist 
	 (append (get-variables-real (get sym 'defn) (find-package "CL-USER"))
		 (get-variables-real (get sym 'represents) (find-package "CL-USER")))))
    (dolist (p (get sym 'equiv-to) retlist) 
	    (setq retlist (append retlist (get-variables-real p (find-package "CL-USER")))))))

(defun get-variables-real (gwff pack)
  (if (null gwff) nil
    (if (consp gwff) (append (get-variables-real (car gwff) pack) (get-variables-real (cdr gwff) pack))
      (list gwff))))
