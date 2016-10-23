;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;



(in-package :auto)

;;;File: rigid.lisp
;;;This is Sunil's implementation of the rigid-path check for 
;;;un90, the unification algorithm used by path-focused duplication
;;;See notes in /afs/cs/user/andrews/rigid-path.doc for relevant
;;;comments



(defun simpl-term-binder (term binder)
  (if (or (symbolp term) (numberp (cdr term))) (values term binder)
      (if (lambda-bd-p term)
          (simpl-term-binder (cdr term) (cons (caar term) binder))
          (values term binder))))

;;check that the initial binder is empty and var is of base type
;;before calling this function.

(defun rigid-path-p (var term)
  (let ((term (simpl-term-binder term nil)))
    (if (symbolp term) nil
      (if (numberp (cdr term))
	  (and (eq (car term) (car var))
	       (= (cdr term) (cdr var)))
	(let ((head (simpl-head term)))
	  (if (consp head) nil
	    (or (rigid-path-p var (car term))
		(rigid-path-p var (cdr term)))))))))

(defun ho-simpl (node)
  (do ((dpairs (copy-list (unode-dpairs node)) (cdr dpairs))
       (subst-stack (unode-substs node))
       (frpairs nil)
       (ffpairs nil)
       first second)
      ((null dpairs)
       (prog1 (if frpairs 1 0)
         (setf (unode-dpairs node) (nconc frpairs ffpairs))))

    (setq first (ho-unif-lnorm nil (caar dpairs) nil subst-stack nil))
    (setq second (ho-unif-lnorm nil (cdar dpairs) nil subst-stack nil))

    (let ((head1 (simpl-head first))
          (head2 (simpl-head second))
          (rh1 nil)
          (rh2 nil))
      (setq rh1 (symbolp head1))
      (setq rh2 (symbolp head2))

      (if (eq first second) nil
          (progn
            (when (or (and (consp first)
                           (numberp (cdr first))
                           (not (ho-free-in first second subst-stack)))
                      (when (and (consp second)
                                 (numberp (cdr second))
                                 (not (ho-free-in second first subst-stack)))
			(psetq first second second first)
			t))
              (push (cons first second) (unode-substs node))
              (setf (unode-dpairs node) (nconc (cdr dpairs) frpairs ffpairs))
              (return-from ho-simpl (ho-simpl node)))

            (when (or (and (consp first) (numberp (cdr first)))
                      (when (and (consp second) (numberp (cdr second)))
                        (psetq first second second first)
                        (psetq rh1 rh2 rh2 rh1)
                        t))
              (when (and (symbolp (type (car first)));;basetype
                         rh2)
                (multiple-value-bind (second binder)
                    (simpl-term-binder second nil)
                  (when (and (not binder)
                             (rigid-path-p first second))
		    (msg T "Rigid path check succeeded" first second)
                    (return-from ho-simpl nil)))))

            (if rh1
                (if rh2
                    (multiple-value-bind (fail new-dpairs)
                        (stepa-simpl first second )
                      (if fail (return-from ho-simpl nil)
                          (setq dpairs (nconc dpairs new-dpairs))))
                    (push (cons first second) frpairs))
                (if rh2
                    (push (cons second first) frpairs)
                    (push (cons first second) ffpairs))))))))

