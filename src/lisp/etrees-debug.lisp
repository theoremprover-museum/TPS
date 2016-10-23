;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

; author: cebrown

; This file is not actually needed for TPS to function properly.
; No user commands depend on the functions defined in the file.
; In a compact version of TPS, it could be excluded.  To exclude
; the file, one would need to delete the reference to it in defpck.lisp,
; specifically in the module EXPANSION-TREE.

; This file contains functions useful for debugging code dealing
; with etrees.  

(in-package :auto)
(part-of expansion-tree)

(context etr-nat)

; check-etree-structure recursively checks structural properties of an etree.
; The idea is that one can temporarily insert 
; (check-etree-structure-break <etree>) in suspicious parts of the code
; to find out when an etree loses its integrity.  If the etree does not
; have structural integrity, a break is called, sending the user (programmer)
; to the debugger.  If one wants to insert this in several places in the code,
; one may want to include a message m to identify which caused the break.
(defun check-etree-structure-break (etree &optional m)
  (declare (special cl-user::bad-etree))
  (let ((z (check-etree-structure etree)))
    (when z
	  (setq cl-user::bad-etree z)
	  (msg m)
	  (break))))

(defun check-etree-structure (etree &optional (shallow 'unknown) (positive 'unknown) parent)
  (update-statuses etree)
  (when (eq shallow 'unknown)
	(setq shallow (strip-exp-vars (get-shallow etree))))
  (when (eq positive 'unknown)
	(setq positive (etree-positive etree)))
  (when (and parent (neq parent (etree-parent etree)))
    (msgf "Bad Parent: " parent " -> " etree)
;    (setq cl-user::etree9 etree cl-user::parent9 parent)    
    (return-from check-etree-structure etree))
  (if (wffeq-ab shallow (get-shallow etree))
      (if (eq positive (etree-positive etree))
	  (etypecase etree
		     (true
		      (unless (wffeq (get-shallow etree) 'TRUTH)
			      (msg "Bad truth node")
			      etree))
		     (false
		      (unless (wffeq (get-shallow etree) 'FALSEHOOD)
			      (msg "Bad truth node")
			      etree))
		     ((or empty-dup-info leaf) nil)
		     (negation
		      (if (and (consp shallow)
			       (eq (car shallow) 'NOT))
			  (if (not (etree-components etree))
			      (progn
				(msg "Negation node has no components")
				etree)
			    (if (cdr (etree-components etree))
				(progn
				  (msg "Negation node has too many components")
				  etree)
			      (check-etree-structure (car (etree-components etree))
						     (cdr shallow)
						     (not positive)
						     etree)))
			(progn
			  (msg "Bad negation node")
			  etree)))
		     (econjunction
		      (check-etree-structure-2 etree 'AND shallow positive))
		     (edisjunction
		      (check-etree-structure-2 etree 'OR shallow positive))
		     (implication
		      (check-etree-structure-2 etree 'IMPLIES shallow positive))
		     (skolem
		      (if (and (consp shallow)
			       (consp (car shallow))
			       (eq (cdar shallow) (if (etree-positive etree)
						      'exists
						    'forall)))
			  (if (not (etree-components etree))
			      (progn
				(msg "Skolem node has no components")
				etree)
			    (if (cdr (etree-components etree))
				(progn
				  (msg "Skolem node has too many components")
				  etree)
			      (check-etree-structure (car (etree-components etree)) 
						     'unknown positive etree)))
			(progn
			  (msg "Bad Skolem node")
			  etree)))
		     (selection
		      (if (and (consp shallow)
			       (consp (car shallow))
			       (eq (cdar shallow) (if (etree-positive etree)
						      'exists
						    'forall)))
			  (if (not (etree-components etree))
			      (progn
				(msg "Selection node has no components")
				etree)
			    (if (cdr (etree-components etree))
				(progn
				  (msg "Selection node has too many components")
				  etree)
			      (check-etree-structure (car (etree-components etree)) 
						     'unknown positive etree)))
			(progn
			  (msg "Bad selection node")
			  etree)))
		     (expansion
		      (if (and (consp shallow)
			       (consp (car shallow))
			       (eq (cdar shallow) (if (etree-positive etree)
						      'forall
						    'exists)))
			  (dolist (c (etree-components etree))
				  (let ((z (check-etree-structure c 'unknown positive etree)))
				    (if z
					(return z))))))
		     (rewrite
		      (if (not (etree-components etree))
			  (progn
			    (msg "Rewrite node has no components")
			    etree)
			(if (cdr (etree-components etree))
			    (progn
			      (msg "Rewrite node has too many components")
			      etree)
			  (let* ((ksh (strip-exp-vars (get-shallow (car (etree-components etree)))))
				 (z
				  (case (rewrite-justification etree)
					(LAMBDA
					 (unless (or (and (or (or-p shallow)
							      (and-p shallow))
							  (symbolp (cdar shallow))
							  (wffeq-ab (lnorm (cdr shallow))
								    (lnorm (cdr ksh))))
						     (and (or (or-p ksh)
							      (and-p ksh))
							  (symbolp (cdar ksh))
							  (wffeq-ab (lnorm (cdr shallow))
								    (lnorm (cdr ksh))))
						     (wffeq-ab (lnorm shallow)
							       (lnorm ksh)))
					   (msgf "Bad Lambda Rew")
					   etree))
				       (DUAL
					(unless
					    (or (wffeq (acons 'AND shallow shallow) ksh)
						(wffeq (acons 'OR shallow shallow) ksh))
					  (msgf "Bad Dual Rew")
					  etree))
				       (EQUIVWFFS 
					(unless
					    (or (and (or (and-p ksh) (or-p ksh))
						     (symbolp (cdar ksh))
						     (wffeq-defeq shallow (cdr ksh)))
						(wffeq-defeq shallow ksh))
					  (msgf "Bad EQUIVWFFS")
					  etree))
				       (t nil))))
			    (or z
				(check-etree-structure (car (etree-components etree))
						       'unknown positive etree)))))))
	(progn
	  (msg "Node has wrong sign")
	  etree))
    (progn
      (msg "Node has wrong shallow formula" t
	   "Expected " shallow)
      etree)))

(defun check-etree-structure-2 (etree connective shallow positive)
  (if (and (consp shallow)
	   (consp (car shallow))
	   (eq (caar shallow) connective))
      (if (or (not (etree-components etree))
	      (not (cdr (etree-components etree))))
	  (progn
	    (msg connective " node has too few components")
	    etree)
	(if (cddr (etree-components etree))
	    (progn
	      (msg "Negation node has too many components")
	      etree)
	  (or (check-etree-structure (car (etree-components etree)) 
				     (cdar shallow)
				     (if (eq connective 'IMPLIES)
					 (not positive)
				       positive) etree)
	      (check-etree-structure (cadr (etree-components etree))
				     (cdr shallow)
				     positive etree))))
    (progn
      (msg "Bad " connective " node")
      etree)))


