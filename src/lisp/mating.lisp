;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package "AUTO")
;(part-of mating-search)
(part-of ms88)

(deffile mating
;  (part-of mating-search)
  (part-of ms88)
  (mhelp "Functions to modify matings."))

;*;(defun store-incomp-clist (clist incomp-lists)
;*;  (setq clist (sort (copy clist) #'<))
;*;  (do ((new-lists nil)
;*;       (length (length clist))
;*;       (lists incomp-lists (cdr lists)))
;*;      ((or (> (length (car lists)) length) (null lists))
;*;       (push clist new-lists)
;*;       (dolist (elt lists)
;*;	 (if (subsetp clist elt :test #'=) nil (push elt new-lists)))
;*;       (nreverse new-lists))
;*;    (if (subsetp (car lists) clist :test #'=)
;*;	(return incomp-lists)
;*;	(push (car lists) new-lists))))

(defun store-incomp-clist (clist incomp-lists)
  "This function modifies INCOMP-LISTS destructively."
  (setq clist (sort (copy clist) #'<))
  (do ((length (length clist))
       (lists incomp-lists (cdr lists)))
      ((or (> (length (car lists)) length) (null lists))
       (cond (lists
	      (destructive-push clist lists)
	      (do ((lists lists))
		  ((null (cdr lists)) incomp-lists)
		(if (subsetp clist (cadr lists) :test #'=)
		    (rplacd lists (cddr lists))
		    (setq lists (cdr lists)))))
	     (T (nconc incomp-lists (list clist)))))
    (if (subsetp (car lists) clist :test #'=)
	(return incomp-lists))))

(defun add-connection (connection mating)
  (push connection (mating-clist mating))
  (unless first-order-mode-ms
    (setf (mating-success-nodes mating) nil)
    (setf (mating-utree mating)
	  (insert-conn-in-utree (list connection) (mating-utree mating)))))

(defun remove-last-connection (mating)
  (let ((connection (pop (mating-clist mating))))
    ;;(setf (mating-utree mating)
    ;;(remove-conn-from-utree connection (mating-utree mating)))
    (if (integerp (mating-bktrack active-mating)) (incf (mating-bktrack active-mating)))
    (setf (mating-completep mating) nil)
    connection))

(defun remove-connection (connection mating)
  (setf (mating-clist mating)
	(delete connection (mating-clist mating) :test #'=))
  (setf (mating-completep mating) nil)
  (if (integerp (mating-bktrack active-mating)) (incf (mating-bktrack active-mating)))
  ;;(setf (mating-utree mating)
  ;;(remove-conn-from-utree connection (mating-utree mating)))
  )
