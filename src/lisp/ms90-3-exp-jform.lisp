;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile ms90-3-exp-jform
  (part-of ms90-3)
  (extension lisp)
  (mhelp "Functions for manipulating jforms in MS90-3."))

(defun find-all-dups (jform dup-record substs fo-mode-ms)
  (declare (special jform))
  (when (eq (jform-type jform) 'universal)
    (let ((new-jform (make-conjunction :components (list jform)
				       :type 'conjunction :pos t
				       :parent nil)))
      (setf (jform-parent jform) new-jform)
      (setq jform new-jform)))
  (let ((dup-stack (list jform)))
    (declare (special dup-stack))
    (setq substs (find-exp-terms substs fo-mode-ms))
    (setq dup-record (mapcar #'(lambda (x) (if (and (jform-p x) (universal-p x)) (universal-qvars x) x))
			     dup-record))
    ;the line above is a hack, and I don't know why it works. MB Sun Sep  8 16:12:13 1996
    ;try to prove THM173 in MODE-X5200-C and you'll see what happens if you don't have it.
    (exp-jform nil jform (nreverse dup-record) substs)
    ;;(setf (get 'S 'core::counter) 0)
    ;;(rename-lits jform)
    (recover-position jform)
    jform))

(defun locate-exp-var (var jform)
  (case (jform-type jform)
    (literal nil)
    (disjunction
     (let ((found nil))
       (dolist (disj (disjunction-components jform) nil)
         (setq found (locate-exp-var var disj))
         (when found (return found)))))
    (conjunction
     (let ((found nil))
       (dolist (conj (conjunction-components jform) nil)
         (setq found (locate-exp-var var conj))
         (when found (return found)))))
    (universal (if (eq (car (universal-qvars jform)) var) jform
                   (locate-exp-var var (universal-scope jform))))
    (t (throwfail "Unknown type " (jform-type jform) " in locate-exp-var"))))

(defun add-exp-node-jform (jform dup-jform &optional (endp t))
  (declare (special dup-stack))
  (let ((parent (jform-parent jform)))
    (if (and parent (eq (jform-type parent) 'conjunction))
        (if endp (setf (conjunction-components parent)
                       (nconc (conjunction-components parent)
                              (list dup-jform)))
            (do ((conjs (conjunction-components parent) (cdr conjs))
                 (new-conjs nil (cons (car conjs) new-conjs)))
                ((or (null conjs) (eq (car conjs) jform))
                 (when (null conjs) (throwfail "err2"))
                 (setf (conjunction-components parent)
                       (nreconc new-conjs (cons jform (cons dup-jform
                                                            (cdr conjs))))))))
        (let ((new-parent (make-conjunction
                           :components (list jform dup-jform)
                           :type 'conjunction :pos t
                           :position (jform-position jform)
                           :parent parent)))
          (setf (jform-parent jform) new-parent)
          (setf (jform-parent dup-jform) new-parent)
          (when parent
            (case (jform-type parent)
              (disjunction
               (setf (disjunction-components parent)
                     (nsubstitute
                      new-parent jform (disjunction-components parent)
                      :test #'eq)))
              (universal (setf (universal-scope parent) new-parent)  ;)) moved these 2 lines down... surely OK? MB 9/96
			 (push (cons (car (universal-qvars jform)) new-parent) 
			       *changed-parents*)))) ))
    (push dup-jform dup-stack)))


(defun exp-jform (dup-jform active-jform dup-record substs)
  (declare (special dup-stack jform))
  (do ((dup-record dup-record (cdr dup-record )))
      ((or (null dup-record)
           (and (consp (car dup-record))
                (eq (cdar dup-record) active-jform)))
       dup-record)
    (let ((elt (car dup-record)))
      (if (consp elt)
          (if (numberp (cdr elt))
              ;;implicit duplication. new copy.
              (let ((jform (dolist (dup dup-stack (throwfail "Err1"))
                             (let ((found (locate-exp-var (car elt) dup)))
                               (when found (return found)))))
                    (vars nil))
                (unless (eq jform dup-jform)
                  (setq dup-jform (duplicate-jform-ren
                                   jform (jform-parent jform)))
                  (add-exp-node-jform jform dup-jform nil))
                (dolist (qvar (universal-qvars jform) (push '$ dup-record))
                  ;; push '$ so that the do loop above is happy.
			(declare (ignore qvar))
			(push (pop dup-record) vars))
                (setf (universal-substs dup-jform) (cons vars substs))
                (subst-in-lits dup-jform vars substs))
              (when (and (universal-p dup-jform)
			 (eq elt (universal-qvars dup-jform)))
                (pop dup-stack)
                (setq dup-jform (car dup-stack))))
          ;;explicit duplication
          (let* ((newparent (cdr (assoc (car (universal-qvars elt)) *changed-parents*)))
                 (dup-jform (duplicate-jform-ren elt 
                                (if newparent 
                                    (setf (jform-parent elt) newparent) 
                                    (jform-parent elt)))))
;;;The reason for the fix can be found around the definition of *changed-parents*
;;;in file ms90-3-expand-etree.lisp.
;;;          (let ((dup-jform (duplicate-jform-ren elt (jform-parent elt))))
            ;;elt's parent was set to nil during search. so find the
            ;;real parent.
            (unless (jform-parent elt)
              (if (and (eq (jform-type ms90-3-jform) 'universal)
                       (eq (universal-qvars elt)
                           (universal-qvars ms90-3-jform)))
                  (setf (jform-parent elt) (jform-parent ms90-3-jform))
                  (setf (jform-parent elt) (universal-prnt elt))))
            ;;;The parent may have been changed during calling add-exp-node-jform.
;;;The following fix is not correct.
;;;            (let ((newparent (cdr (assoc (jform-parent elt) *changed-parents*))))
;;;               (if newparent (setf (jform-parent elt) newparent)))
            (setf (jform-parent dup-jform) (jform-parent elt))
            ;;add it at the end of conjunction components
            (add-exp-node-jform elt dup-jform)
            (setq dup-record
                  (exp-jform dup-jform elt (cdr dup-record) substs)))))))

(defun subst-in-lits (jform vars substs)
  (case (jform-type jform)
    (literal
     (setf (jform-represents jform)
           (lambda-norm
	    (subst-in-literal (jform-represents jform) vars substs))))
    (disjunction
     (dolist (disj (disjunction-components jform))
       (subst-in-lits disj vars substs)))
    (conjunction
     (dolist (conj (conjunction-components jform))
       (subst-in-lits conj vars substs)))
    (universal (subst-in-lits (universal-scope jform) vars substs))))


(defun subst-in-literal (term vars substs)
  (if (lsymbol-q term)
      (let ((var (assoc term vars)))
        (if var 
	    (or (cdr (assoc var substs)) 
		(generic-constant term)) 
	  term))
      (cons (subst-in-literal (car term) vars substs)
            (subst-in-literal (cdr term) vars substs))))


;; compute substitutions.

(defun find-exp-terms (substs first-order-mode-ms)
  (setq substs (nreverse substs))
  (let ((stack nil))
    (if first-order-mode-ms
        (dolist (subst substs)
          (push (cons (car subst)
                      (expand-subst (cadr subst) (cddr subst) substs))
                stack))
        (dolist (subst substs)
          (unless (= (cdar subst) -2)
            (push (cons (car subst)
                        (ho-unif-lnorm-1 nil (cdr subst) nil substs nil))
                  stack))))
    (nreverse stack)))


;;; Don't return $ anymore, just make new constant of appropriate
;;; type.

(defun expand-subst (term env substs)
  (if (lsymbol-q term)
      (let ((var (assoc term env)))
	(if var 
	    (let ((sub  (cdr (assoc var substs))))
	      (if sub 
		  (expand-subst (car sub) (cdr sub) substs)
		(generic-constant term)))
            term))
      (cons (expand-subst (car term) env substs)
            (expand-subst (cdr term) env substs))))

(defun ho-unif-lnorm-1 (neg gwff bdvars sstack bstack)
  (multiple-value-bind (gwff neg)
      (ho-unif-lnorm-1-rec neg gwff bdvars sstack bstack)
    (if neg (cons 'not gwff) gwff)))

;;; Don't return $ anymore, just make a new constant.
(defun ho-unif-lnorm-1-rec (neg gwff bdvars sstack bstack)
  (cond ((symbolp gwff)
         (if (memq gwff bdvars) (values gwff neg)
             (let ((term (assoc gwff bstack)))
               (if term (ho-unif-lnorm-1-rec
                         neg (cdr term) nil sstack bstack) 
                   (values gwff neg)))))
        ((numberp (cdr gwff))
         (let ((term (assoc gwff sstack)))
           (if term (ho-unif-lnorm-1-rec neg (cdr term) bdvars sstack bstack)
               (values (generic-constant (car gwff))
		       neg))))
        ((not-p gwff)
         (ho-unif-lnorm-1-rec (not neg) (cdr gwff) bdvars sstack bstack))
        ((lambda-bd-p gwff)
         (cons (car gwff) (ho-unif-lnorm-1
                           nil (cdr gwff) (cons (caar gwff) bdvars)
                           sstack bstack)))
        (t (if (lambda-bd-p (car gwff))
               (ho-unif-lnorm-1-rec
                neg (cdar gwff) bdvars sstack
                (acons (caaar gwff)
		       (ho-unif-lnorm-1 nil (cdr gwff) bdvars sstack bstack)
                       bstack))
	     #+comment(let ((gwff (cons (ho-unif-lnorm-1
                                  nil (car gwff) bdvars sstack bstack)
				 (ho-unif-lnorm-1 nil (cdr gwff) bdvars 
                                                  sstack bstack))))
                 (if (lambda-bd-p (car gwff))
                     (ho-unif-lnorm-1-rec
                      neg (cdar gwff) bdvars sstack
                      (acons (caaar gwff) (cdr gwff) bstack))
                     (values gwff neg)))
	     (let ((gwff (cons (ho-unif-lnorm-1
				nil (car gwff) bdvars sstack bstack)
			       (cdr gwff))))
	       (if (lambda-bd-p (car gwff))
		   (ho-unif-lnorm-1-rec
		    neg (cdar gwff) bdvars sstack
		    (acons (caaar gwff) (cdr gwff) bstack))
		 (values (cons (car gwff) (ho-unif-lnorm-1 nil (cdr gwff) bdvars sstack bstack)) neg)))
))))

(defun rename-lits (jform)
  (case (jform-type jform)
    (literal (setf (jform-represents jform)
                   (subst-in-literal (jform-represents jform) nil nil))
             (setf (literal-name jform) (create-namestring 'S)))
    (disjunction (mapc #'rename-lits (disjunction-components jform)))
    (conjunction (mapc #'rename-lits (conjunction-components jform)))
    (universal (rename-lits (universal-scope jform)))))

(defun collapse-jform-barrier (jform new-jform)
  (let ((type (jform-type jform)))
    (flet ((jform-components (jform) 
	       (if (eq 'disjunction type)
		   (disjunction-components jform)
		 (conjunction-components jform))))
       (let* ((components (mapcar #'(lambda (jform)
				     (cnvrt-prop-jform jform new-jform))
				 (jform-components jform))))
	  (when components
		(do ((first components)
		     (second (cdr components) (cdr second)))
		    ((null second)
		     (if (eq type (jform-type (car components)))
			 (let ((subcomponents (jform-components (car components))))
			   (setq components
				 (if subcomponents
				     (dolist (com subcomponents (nconc subcomponents (cdr components)))
					     (setf (jform-parent com) new-jform))
				   (cdr components))))))
		    (if (eq type (jform-type (car second)))
			(let ((subcomponents (jform-components (car second))))
			  (when subcomponents
				(dolist (com subcomponents)
					(setf (jform-parent com) new-jform))
				(rplacd first subcomponents)
				(setq first (last subcomponents)))
			  (rplacd first (cdr second)))
		      (setq first second))))
	  (if (eq 'disjunction type)
	      (setf (disjunction-components new-jform) components)
	    (setf (conjunction-components new-jform) components))))))
	
(defun cnvrt-prop-jform (jform parent)
  (if (universal-p jform)
      (cnvrt-prop-jform (universal-scope jform) parent)
    (let ((new-jform (copy-jform jform)))
      (setf (jform-parent new-jform) parent)
      (if (or (disjunction-p jform) (conjunction-p jform)) (collapse-jform-barrier jform new-jform))
      new-jform)))

(defun duplicate-jform-ren (jform parent)
  (let (new-jform)
    (case (jform-type jform)
      (universal
       (setq new-jform (copy-universal jform))
       (setf (universal-dup new-jform) t)
       (setf (universal-prnt new-jform) (jform-parent jform))
       (setf (universal-scope new-jform)
             (duplicate-jform-ren (universal-scope jform) new-jform)))
      (disjunction
       (setq new-jform (copy-disjunction jform))
       (setf (disjunction-components new-jform)
             (mapcar #'(lambda (jform) (duplicate-jform-ren jform new-jform))
                     (disjunction-components jform))))
      (conjunction
       (setq new-jform (copy-conjunction jform))
       (setf (conjunction-components new-jform)
             (mapcar #'(lambda (jform) (duplicate-jform-ren jform new-jform))
                     (conjunction-components jform))))
      (literal (setq new-jform (copy-literal jform))
	       (setf (literal-name new-jform) (create-namestring lit-name))))
    (setf (jform-parent new-jform) parent)
    new-jform))
