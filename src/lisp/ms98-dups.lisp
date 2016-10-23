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

(deffile ms98-dups
  (part-of ms98)
  (extension lisp)
  (mhelp "Miscellaneous functions to handle duplication and primitive substitution for MS98-1"))

(defmateop ms98-prim
  (mate-alias ms98-prim)
  (mate-result-> ignore)
  (mhelp "Make all possible primitive substitutions and 
then NUM-OF-DUPS duplications in the current etree."))

(defmateop ms98-dup
  (mate-alias ms98-dup)
  (mate-result-> ignore)
  (mhelp "Make NUM-OF-DUPS duplications in the current etree."))

(defun ms98-prim (&optional (num num-of-dups))
  (if (eq primsub-method 'PR00)
      (generate-pr00-setsubs 
       (if ms98-rew-primsubs (mapcar #'car *leibniz-var-list*) nil)
       ms98-max-prims)
    (progn
      (if ms98-rew-primsubs
	  (apply-prim-subs-all-counted (eproof-etree current-eproof))
	(apply-prim-subs-notrew (eproof-etree current-eproof)))
      (ms98-dup num)
      (when ms98-dup-below-primsubs (find-primsubs-and-dup-below)))))

(defun ms98-make-primlist (&optional (num num-of-dups))
  (if (eq primsub-method 'PR00)
      (generate-pr00-setsubs
       (if ms98-rew-primsubs (mapcar #'car *leibniz-var-list*) nil)
       0)
    (progn
      (setq *primsubs-for-ms98* (remove-if #'(lambda (x) (null (car x)))
					   (make-primsubs-all-counted (eproof-etree current-eproof))))
      (setq *current-primsub* nil)
      (unless ms98-rew-primsubs
	(setq *primsubs-for-ms98*
	  (remove-if #'(lambda (x) (assoc (exp-var-var (car x)) *leibniz-var-list*)) *primsubs-for-ms98*)))
      (ms98-dup num (unless ms98-dup-primsubs (mapcar #'car *primsubs-for-ms98*)))
      (setq *primsubs-for-ms98*
	(remove-if #'(lambda (x) (not (cdr x))) ; cebrown - 5/24/00 - in case there are no gensubs generated for an exp-var
		   (mapcar #'(lambda (x) (cons (cdr (assoc (car x) (eproof-free-vars-in-etree current-eproof)))
					       (cdr x))) *primsubs-for-ms98*))))))

(defun make-primsubs-all-counted (etree)
  (declare (special prim-bdtypes prim-bdtypes-auto prim-hashtable))
  (case prim-bdtypes-auto
	((replace) (setq prim-bdtypes (find-prim-types (get-deep etree))))
	((replace-sub) (setq prim-bdtypes (find-prim-subtypes (get-deep etree))))
	((append) (setq prim-bdtypes (union prim-bdtypes (find-prim-types (get-deep etree)))))
	((append-sub) (setq prim-bdtypes 
                        (union prim-bdtypes (find-prim-subtypes (get-deep etree))))))
  (clrhash prim-hashtable)
  (apply #'append (mapcar #'dont-apply-prim-subs (find-ho-exps (find-exp-nodes etree)))))

(defun apply-prim-subs-notrew (etree)
  (setq *after-primsub* t)
  (let ((newetree (apply-prim-subs-notrew* etree)))
    (if (eq etree (eproof-etree current-eproof))
	(setf (eproof-etree current-eproof) newetree))
    (if (eq etree (eproof-etree master-eproof))
	(setf (eproof-etree master-eproof) newetree))
    newetree))

(defun apply-prim-subs-notrew* (etree)
  (cond ((leaf-p* etree) etree)
	((and (expansion-p etree) (not (assoc (exp-var-var (car (expansion-terms etree))) *leibniz-var-list*)))
	 (mapc #'apply-prim-subs-notrew* 
	       (etree-components etree))
	 (apply-prim-subs-counted etree))
	(t (mapc #'apply-prim-subs-notrew*
		 (etree-components etree))
	   etree)))

(defun apply-prim-subs-all-counted (etree)
  (setq *after-primsub* t)
  (let ((newetree (apply-prim-subs-all-counted* etree)))
    (if (eq etree (eproof-etree current-eproof))
	(setf (eproof-etree current-eproof) newetree))
    (if (eq etree (eproof-etree master-eproof))
	(setf (eproof-etree master-eproof) newetree))
    newetree))

(defun apply-prim-subs-all-counted* (etree)
  (cond ((leaf-p* etree) etree)
	((expansion-p etree)
	 (mapc #'apply-prim-subs-all-counted* 
	       (etree-components etree))
	 (apply-prim-subs-counted etree))
	(t (mapc #'apply-prim-subs-all-counted*
		 (etree-components etree))
	   etree)))

(defun apply-prim-subs-counted (etree &optional (vars* nil))
  (setq *after-primsub* t)
  (let* ((vars (if vars* 
                   (mapcar #'list vars*)
                   (expansion-prim-vars etree)))
         (bdwff (get-shallow etree))
         (scope-bdwff (gdr bdwff))
         (vars-of-scope (substitutable-vars-of scope-bdwff)))
      (dolist (var vars)
	(setq var (car var))
	(unless (cdr (assoc var (expansion-prim-vars etree))) ; prim subs already
          (when (apply-prim-subst-for-var var)
	      (let* ((term (dolist (term (expansion-terms etree) nil)
			    (when (memq var (hvars term))
			      (return term))))
                     new-term
                     (new-kids nil)
                     (new-vars-of-term 
                      (when term 
                        (mapcar #'exp-var-var
                                (set-difference 
                                  (substitutable-vars-of term)
                                  vars-of-scope)))))
		(when term
		  (dolist (prim-sub (if ms98-primsub-count
					(subseq (find-prim-substs var) 0 ms98-primsub-count)
				      (find-prim-substs var))
				    (push (cons var (nreverse new-kids))
					  (expansion-prim-vars etree)))
		    (setq new-term 
                          (substitute-term-var (subst-term prim-sub)
                                               (strip-exp-vars var)
                                               (strip-exp-vars term)))
                    (dolist (new-var new-vars-of-term)
                      (setq new-term
                            (substitute-term-var (funcall ren-var-fn new-var)
                                                 new-var
                                                 new-term)))
		    (duplicate-var etree)
		    (substitute-in-etree new-term 
					 (car (last (expansion-terms
						     etree)))
					 etree)
		    (push (car (last (etree-components etree)))
			  new-kids))))))))
  etree)

(defun find-primsubs-and-dup-below ()
  (dolist (tree (find-primsubs-in-etree (eproof-etree current-eproof)))
    (ms98-dup-subtree tree)))

(defun find-primsubs-in-etree (tree)
  (let (returnlist)
    (dolist (exp (find-etree-nodes #'expansion-p tree) returnlist)
      (do ((term (expansion-terms exp) (cdr term))
	   (child (etree-components exp) (cdr child)))
	  ((null term))
	(when (neq (exp-var-var (car term)) (exp-var-subst (car term)))
	  (push (car child) returnlist))))))

(defun ms98-dup (&optional (num num-of-dups) (dont-dup nil))
  (do ((n 0 (1+ n)))
      ((= n num) (deepen-to-literals (eproof-etree current-eproof)))
    (ccs-dup-outers (eproof-etree current-eproof) dont-dup)))

(defun ms98-dup-subtree (tree &optional (num num-of-dups) (dont-dup nil))
  (do ((n 0 (1+ n)))
      ((= n num) (deepen-to-literals tree))
    (ccs-dup-outers tree dont-dup)))

(defun ccs-dup-outers (etree dont-dup)
  (let ((newetree (ccs-dup-outers* etree dont-dup)))
    (if (eq etree (eproof-etree current-eproof))
	(setf (eproof-etree current-eproof) newetree))
    (if (eq etree (eproof-etree master-eproof))
	(setf (eproof-etree master-eproof) newetree))
    newetree))

					; cebrown - 1/30/01
					; changed this to indicate when an actual setsub is the subst,
					; not just some simpler subst
(defun dup-outers-fn-1 (x) 
;  (eq (exp-var-var x) (exp-var-subst x)))
  (let ((h (head (exp-var-subst x))))
    (not (or (eq h 'NOT) ; added this case 9/22/01 - when setsub starts with ~
	     (anyabbrev-p h) ; added this case 9/21/01 - CR-THEOREM is an example where the setsub has an abbrev at the head
	     (compound-formula-p
	      (strip-exp-vars x))))))
   
(defun ccs-dup-outers* (etree dont-dup)
  (cond ((leaf-p* etree) etree)
	((expansion-p etree)
	 (if (and (not (intersection (expansion-terms etree) dont-dup))
		  (or ms98-dup-primsubs
		      (null (remove-if #'dup-outers-fn-1 (expansion-terms etree)))))
	     (duplicate-var etree)
	   etree))
	(t (mapc #'(lambda (x) (ccs-dup-outers* x dont-dup))
		 (etree-components* etree))
	   etree)))

(defun pbelow-fn-1 (x) (cons (car x) (cadr x)))

(defun get-prims-below (disj-assoc)
  (prims-below-map (eproof-etree current-eproof) (mapcar #'pbelow-fn-1 disj-assoc)))

(defun prims-below-map (etree list)
  (cond ((leaf-p* etree) nil)
	((expansion-p etree)
	 (if (remove-if #'dup-outers-fn-1 (expansion-terms etree))
	     ;;if there are any primsubs here
	     (mapcar #'(lambda (x) (fragments-covered-by x list)) (etree-components etree))
	   (reduce #'append  (mapcar #'(lambda (x) (prims-below-map x list))
				    (etree-components* etree)))))
;;following is wrong: we want to consider the copy that has no primsub as a "primsub of lambda x . x"
;;	 (mapcar #'(lambda (x) (fragments-covered-by (cdr x) list))
;;		 (remove-if #'(lambda (x) (eq (exp-var-var (car x)) (exp-var-subst (car x))))
;;			    (mapcar #'cons (expansion-terms etree) (etree-components etree)))))
	(t (reduce #'append (mapcar #'(lambda (x) (prims-below-map x list))
				    (etree-components* etree))))))

(defun fragments-covered-by (tree list)
  (let ((leaves (mapcar #'leaf-name (find-etree-nodes #'leaf-p* tree))))
    (mapcar #'car (remove-if-not #'(lambda (x) (intersection leaves (mapcar #'literal-name (cdr x)))) list))))

(defun get-sk-below-primsubs (&optional (etree (eproof-etree current-eproof)))
  (cond ((leaf-p* etree) nil)
	((expansion-p etree)
	 (if (remove-if #'dup-outers-fn-1 (expansion-terms etree))
	     ;;if there are any primsubs here
	     (mapcar #'get-selections-here (etree-components etree) (expansion-terms etree))
	   (reduce #'append  (mapcar #'get-sk-below-primsubs (etree-components* etree)))))
	(t (reduce #'append (mapcar #'get-sk-below-primsubs (etree-components* etree))))))

(defun getsel-fn-1 (x) (skolem-term-parameter (car (selection-terms x))))

(defun get-selections-here (etree term)
  (let ((nodes (mapcar #'getsel-fn-1 (find-etree-nodes #'selection-p etree)))
	(vars (free-vars-of (exp-var-subst term))))
    (cons vars nodes)))

(defun get-sk-below-dups (&optional (etree (eproof-etree current-eproof)))
  (cond ((leaf-p* etree) nil)
	((expansion-p etree)
	 (if (and (> (length (expansion-terms etree)) 1)
		  (> (length (remove-if-not #'dup-outers-fn-1
					    (cdr (expansion-terms etree)))) 0)) ;;cdr because the first will the original
	     ;;if there are any primsubs here
	     (mapcar #'get-selections-here (cdr (etree-components etree)) (cdr (expansion-terms etree)))
	   (reduce #'append  (mapcar #'get-sk-below-dups (etree-components* etree)))))
	(t (reduce #'append (mapcar #'get-sk-below-dups (etree-components* etree))))))

					; cebrown - 7/24/00, added exceptions for when exp node is top of tree 
(defun hack-primsubs-about ()
  (if (eq PRIMSUB-METHOD 'PR00)		; handled differently - cebrown - 10/20/00
      (when *pr00-setsubs*
	(ftree-to-etree (caar *pr00-setsubs*)) ; destructive; sets eproof-etree to the etree version of the ftree, and many globals
	(setq ms98-current-ftree (caar *pr00-setsubs*))
	(setf (eproof-dissolve current-eproof) (cdar *pr00-setsubs*))
	(when first-order-mode-ms	; may have been set to T if the instantiated dissolved jform is first order
	  (setq *using-unifhash* nil))	; so the value for *using-unifhash* set in init-everything may be inappropriate
					; (note FIRST-ORDER-MODE-MS T + *USING-UNIFHASH* T are incompatible; the dags will cause a bug)
					; - cebrown 11/15/00
	(pop *pr00-setsubs*))
    (progn
      (when *current-primsub*		;(oldtree .  variable) thingy
	(setf (eproof-dissolve current-eproof) nil)
	(setf (eproof-allow-nonleaf-conns current-eproof) nil)
	(let* ((enode (car *current-primsub*))
	       (parent (etree-parent enode))
	       (pos (if parent 
			(or (position enode (etree-components parent)) (throwfail "node " enode " not in tree")))))
	  (if parent			; otherwise, assume it is the topnode - cebrown - 7/24/00
	      (setf (etree-components parent)
		    (mapcar #'(lambda (x) (if (eq x enode)
					      (create-leaf-node (get-shallow enode) (etree-positive enode)
								(etree-free-vars parent) parent)
					    x))
			    (etree-components parent)))
	    (setq current-topnode (create-leaf-node (get-shallow enode) (etree-positive enode) nil nil)))
	  (update-statuses current-topnode)
	  (if parent ; cebrown - 7/24/00
	      (deepen-to-literals* (nth pos (etree-components parent)))
	    (setq current-topnode (deepen-to-literals* current-topnode))) ; cebrown - 7/24/00
	  (when (eq (caar *primsubs-for-ms98*) enode) 
	    (rplaca *primsubs-for-ms98* (cons (if parent ; cebrown - 7/24/00
						  (nth pos (etree-components parent))
						current-topnode) ; cebrown - 7/24/00
					      (cdar *primsubs-for-ms98*))))
	  (setf (etree-parent enode) nil)
	  (setq *current-primsub* nil)))
      (let* ((next-prim-var (car (expansion-terms (caar *primsubs-for-ms98*))))
	     (next-prim-sub (cadar *primsubs-for-ms98*))
	     (enode (caar *primsubs-for-ms98*)))
	(msgf (next-prim-var . gvar) " : " ((subst-term (get next-prim-sub 'subst)) . gwff))
	(setq *current-primsub* (cons enode next-prim-var))
	(rplaca (car *primsubs-for-ms98*) enode) ; cebrown 9/22/00
	(if (cddar *primsubs-for-ms98*)
	    (rplacd (car *primsubs-for-ms98*)
		    (cddar *primsubs-for-ms98*))
	  (pop *primsubs-for-ms98*))
	(prim-single next-prim-sub next-prim-var current-topnode))
      (when delay-setvars
	(setq ms98-current-ftree (etree-to-ftree current-topnode)))))
  (when ms98-dup-below-primsubs (find-primsubs-and-dup-below))
;  (display-vp-diag current-topnode)
)

