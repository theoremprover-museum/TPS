;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of PRIMITIVE-SUBST)

;;;
;;; File: PRIM
;;; Functions for generating primitive substitutions.

(deffile prim-edops
  (part-of PRIMITIVE-SUBST)
  (extension clisp)
  (mhelp "Interface to the primitive substitution package."))

(context primsubs)

(defflag prim-prefix
  (flagtype symbol)
  (default 'prim)
  (subjects primsubs)
  (mhelp "Prefix for weak labels associated with primitive substitutions."))

(defflag neg-prim-sub
  (flagtype boolean)
  (default nil)
  (change-fn (lambda (a b c)
	       (declare (ignore a b c))
	       (ini-prim-hashtable)))
  (subjects primsubs transmit)
  (relevancy-preconditions
   (primsub-method (AND (NEQ PRIMSUB-METHOD 'PR97C) (NEQ PRIMSUB-METHOD 'PR00))))
  (irrelevancy-preconditions
   (primsub-method (or (EQ PRIMSUB-METHOD 'PR97C) (EQ PRIMSUB-METHOD 'PR00))))
  (mhelp "When T, one of the primitive substitutions will introduce negation."))

;;printing

(defun prt-prim (substs &optional (indent nil))
  (dolist (subst substs)
    (msg t)
    (if indent (spaces indent))
    (when (label-q subst)
      (msg subst 3)
      (setq subst (get subst 'subst)))
    (if (numberp (subst-type subst))
	(msg "Projection" 2)
; was previously (msg (subst-type subst) "Projection" 2)
	(if (string= (subst-type subst)  "PrimConst")
	    (msg "LogConst" 5)
	    (msg (subst-type subst) 3)))
    (msg ((subst-term subst) . gwff))))

(defun primlist (list)
  (dolist (elt list)
    (msg t "Var: " 2 ((car elt) . gvar) t)
    (prt-prim (cdr elt) 5)))

(defedop prt-prim
  (alias primsubsts)
  (result-> primlist)
  (edwff-argname gwff))

(defwffop primsubsts
  (argtypes gwff)
  (argnames gwff)
  (applicable-p (lambda (gwff) (declare (ignore gwff)) (or t)))
  (mhelp "Prints primitive substitutions for the head variables of a wff."))

(defun primsubsts (gwff)
  (mapcar #'(lambda (var) (cons var (find-prim-substs var "Pr"))) (hvars gwff)))

(defedop name-prim
  (alias name-primsubsts)
  (result-> primlist)
  (edwff-argname gwff))

(defwffop name-primsubsts
  (argtypes gwff)
  (argnames gwff)
  (applicable-p (lambda (gwff) (declare (ignore gwff)) t))
  (mhelp "Creates weak labels for primitive substitutions for the head
    variables of a wff."))

(defun name-primsubsts (gwff)
  (declare (special prim-bdtypes prim-bdtypes-auto))
  (case prim-bdtypes-auto
	((replace) (setq prim-bdtypes (find-prim-types gwff)))
	((replace-sub) (setq prim-bdtypes (find-prim-subtypes gwff)))
	((append) (setq prim-bdtypes (union prim-bdtypes (find-prim-types gwff))))
	((append-sub) (setq prim-bdtypes 
                        (union prim-bdtypes (find-prim-subtypes gwff)))))
  (clrhash prim-hashtable)
  (mapcar #'(lambda (var)
	      (cons var
		    (mapcar #'(lambda (prsub)
				(let ((name (create-name prim-prefix)))
				  (putprop name prsub 'subst)
				  (create-weak name (subst-term prsub))))
			    (find-prim-substs var))))
	  (hvars gwff)))

(defflag prim-bdtypes
  (flagtype typesymlist-nil)
  (default '(cl-user::I))
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs important transmit)
  (mhelp "List of types of quantified variables used to construct primitive
substitutions. This list will always be used when constructing primitive
substitutions interactively, but see the flag PRIM-BDTYPES-AUTO for more
information on the types that will be used by automatic search procedures."))

(defflag prim-bdtypes-auto
  (flagtype symbol)
  (default 'replace)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs important transmit)
  (relevant-kids ((member PRIM-BDTYPES-AUTO '(APPEND APPEND-SUB IGNORE)) '(prim-bdtypes))) ; cebrown 10/2/00
  (irrelevant-kids ((member PRIM-BDTYPES-AUTO '(REPLACE REPLACE-SUB)) '(prim-bdtypes))) ; cebrown 10/2/00
  (irrelevancy-preconditions
   (primsub-method (eq primsub-method 'pr00)))
  (mhelp "Has five possible values: REPLACE, REPLACE-SUB, APPEND, 
APPEND-SUB and IGNORE.
Determines how the procedures that use primitive substitutions
handle the flag PRIM-BDTYPES, as follows:
REPLACE -- the value of PRIM-BDTYPES will be changed to an 
automatically-generated list of all the primitive types used in 
the gwff to be proven.
REPLACE-SUB -- as for replace, except that the list will be of all
the subtypes of the types that appear in the gwff.
APPEND -- the same list is calculated as for REPLACE, but instead
of replacing the current setting of PRIM-BDTYPES it will be appended
to it.
APPEND-SUB -- the same list is calculated as for APPEND, but instead
of replacing the current setting of PRIM-BDTYPES it will be appended
to it.
IGNORE -- no list will be generated, and the user's setting of 
PRIM-BDTYPES will be left intact."))

(definfo replace
  (mhelp "A flag setting for PRIM-BDTYPES-AUTO.
The value of PRIM-BDTYPES will be changed to an 
automatically-generated list of all the primitive types used in 
the gwff to be proven."))

(definfo replace-sub
  (mhelp "A flag setting for PRIM-BDTYPES-AUTO.
The value of PRIM-BDTYPES will be changed to an 
automatically-generated list of all the subtypes of the 
types that appear in the gwff."))

(definfo append
  (mhelp "A flag setting for PRIM-BDTYPES-AUTO.
The same list is calculated as for REPLACE, but instead
of replacing the current setting of PRIM-BDTYPES it will be appended
to it."))

(definfo append-sub
  (mhelp "A flag setting for PRIM-BDTYPES-AUTO.
The same list is calculated as for APPEND, but instead
of replacing the current setting of PRIM-BDTYPES it will be appended
to it."))

(definfo ignore
  (mhelp "A flag setting for PRIM-BDTYPES-AUTO.
The user's setting of PRIM-BDTYPES will be left intact."))
