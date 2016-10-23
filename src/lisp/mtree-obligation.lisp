;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :AUTO)


(deffile mtree-obligation
  (part-of mst)
  (extension lsp)
  (mhelp "Defines obligations, as used by matingstree."))

(defmacro obligation-type (obligation)
   `(jform-type (obligation-jform ,obligation)))

(defun ob-go-up (obligation)
  (or (obligation-last obligation)
      (throwfail "You are at the top of the obligation.")))

(defun ob-go-down (nth obligation)
  (or (nth nth (obligation-next obligation))
      (throwfail "The matingstree doesn't have the given branch.")))

;;;jform-check is used to check whether the jform is on an path.
;;;The function is frequently called to make sure whether a block
;;;is legal.
(defun jform-check (jform obligation)
   (let ((parent (obligation-last obligation)))
     (and parent 
          (case (obligation-type parent)
             (conjunction
              (or (member jform (conjunction-components (obligation-jform parent)))
                  (jform-check jform parent)))
             (T (jform-check jform parent))))))

(defun literal-check (literal obligation)
   (let ((parent (obligation-last obligation)))
     (and parent 
          (case (obligation-type parent)
             (literal (or (eq literal (obligation-jform parent))
                          (literal-check literal parent)))
             (T (literal-check literal parent))))))


(defun disj-block (nth disjunction obligation)
   (if (eq (jform-type disjunction) 'disjunction)
       (let (newobs)
          (dolist (component (disjunction-components disjunction))
               (push (make-obligation :last obligation 
                                      :jform component
                                      :used-univs (init-used-univs component)
                                      :disjunction disjunction)
                   newobs))
          (setf (obligation-next obligation) (nreverse newobs))
	  (auto-expand obligation)
          (nth nth (obligation-next obligation)))
       (throwfail "Sorry, the type of the jform is not a disjunction.")))

(defun auto-expand (obligation)
  (if (obligation-next obligation)
      (mapc #'auto-expand (obligation-next obligation))
      (case (obligation-type obligation)
        ((conjunction universal literal) obligation)
        (disjunction (disj-block 0 (obligation-jform obligation) obligation)
		     (mapc #'auto-expand (obligation-next obligation))))))

(defun init-used-univs (jform)
   (let (numlist)
     (case (jform-type jform)
        ((disjunction literal) nil)
        (conjunction (dolist (com (conjunction-components jform) (nreverse numlist))
                             (if (eq (jform-type com) 'universal) 
                                 (push 1 numlist) (push 0 numlist))))
        (universal '(1)))))

(defun setf-obligation-closed (ob name)
  (setf (obligation-closed ob) name)
  (do ((hxlast (obligation-last ob) (obligation-last hxlast)))
      ((not hxlast))
      (if (dolist (next (obligation-next hxlast))
                  (if (not (obligation-closed next)) (return T)))
          (return nil)
          (setf (obligation-closed hxlast) "CLOSED"))))
         
;;;-----------------------------------------------------------
