;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :auto)

(part-of mating)

(deffile etrees-exp-vars
  (part-of mating)
  (extension lisp)
  (mhelp "Defines the flavor EXP-VAR for use in expansion trees."))

(context expansion-trees)

;;; An EXP-VAR will be a structure with two slots.  The first, VAR,
;;; is a variable (lsymbol) which should always be new, i.e., no two EXP-VARs
;;; should have the same VAR property.  The VAR slot should never change.
;;; The second slot, SUBST, is initially the same as the VAR slot, but
;;; if a substitution is made for the variable SUBST is given the
;;; value of the substitution.  Example, suppose we are proving the
;;; theorem "exists p(O) p".  There will be an expansion node whose
;;; initial expansion term is an EXP-VAR which will look (to the
;;; debugger) like #S(EXP-VAR VAR |p0<O>| SUBST |p0<O>|).  What the user
;;; will see if this wff prints is "p0(O)", i.e., only the SUBST
;;; property.  Now suppose the user substitutes for this variable the
;;; wff "A or ~A".  This is now what the user will see, because
;;; internally the variable is 
;;; #S(EXP-VAR VAR |p0<O>| SUBST ((OR . A<O>) NOT . A<O>)).
;;; That is, it is exactly the same structure, with an updated SUBST
;;; slot.
;;;
;;; Why is this useful?  Note that in an expansion tree, a variable once
;;; introduced may appear in many parts of the tree.  If we want to
;;; substitute for the variable, we don't want to have to recurse
;;; through the entire tree, replacing each occurrence of the variable
;;; by the substitution.  By making each variable a structure, we need
;;; merely change the structure, and each place the variable occurs is
;;; automatically updated. 
;;;
;;; Actually the example we gave above was a little simplistic.  In
;;; reality, when we substitute "A or ~A" for "p", the variable "A" is
;;; a free variable, so we would actually create an EXP-VAR for it,
;;; and use that in the SUBST rather than the symbol A<O> alone.
;;;
;;; This points up some complications that this scheme causes.
;;; 1.  The user types in wffs as they are printed.  New variables in
;;; user-supplied substitutions must be replaced by EXP-VARS in the
;;; actual etree.  Variables already in the etree must be replaced by
;;; their corresponding already-existing EXP-VAR. (The same is already true of
;;; any Skolem constants in user-supplied substitutions.)
;;; 2.  The unification routines do not know how to handle EXP-VARs
;;; (nor SKOLEM-TERMs), so in the etree-to-jform routine the EXP-VARs
;;; are stripped out, and replaced by their SUBST slot.
;;; 3.  Likewise, the substitutions returned by unification have to be
;;; processed to make the substitutions properly.
;;; 4.  The natural deduction proof code doesn't handle EXP-VARs, so
;;; they are removed during merging of the completed expansion proof.




(eval-when (compile load eval)
(defflavor exp-var
  (structured t)
  (mhelp "An EXP-VAR is used to represent a variable (one which can be
substituted for) in an expansion tree.  It has two main properties:
a variable and a substitution (which may be the same as the variable if
no substitution has yet been made.")
  (structured t)
  (instance-attributes
   var
   subst
   selected)))


;;; If non-nil, printwff will print the variable, not its substitution
;;; (if any). Usually for debugging. Like the variable show-skolem.
(defvar *show-exp-var* nil)


;;; note that lambda-norm, ab-normalize-main, subst-term-var-rec and
;;; others apply 
;;; the operation to the SUBST slot, updating it, and returning the
;;; entire EXP-VAR so that the EXP-VAR remains in the surrounding wff.

(defflavor exp-var
  (wffeq (lambda (wff1 wff2)
           (wffeq (exp-var-subst wff1) wff2)))
  (core::wffeq-ab11 (lambda (wff1 wff2 varstack)
	       (core::wffeq-ab1 (exp-var-subst wff1) wff2 varstack)))
  (core::wffeq-ab12 (lambda (wff1 wff2 varstack)
	       (core::wffeq-ab1 wff1 (exp-var-subst wff2) varstack)))
  (subst-term-var-rec
   (lambda (term var inwff) 
     (if (wffeq var inwff)
         term
         (let ((newwff (subst-term-var-rec term var (exp-var-subst
                                                     inwff))))
           (when newwff
             (setf (exp-var-subst inwff) newwff)
             inwff)))))
  (subst-l-term-rec
   (lambda (term var inwff) (subst-term-var-rec term var inwff)))
  (free-for
   (lambda (term var inwff) (free-for term var (exp-var-subst inwff))))
  (printwff (lambda (wff bracket depth)
              (if *show-exp-var*
                  (printwff (exp-var-var wff) bracket depth)
                  (printwff (exp-var-subst wff) bracket depth))))
  (prt-symbol-p (lambda (gwff) (prt-symbol-p (exp-var-subst gwff))))
  (prt-infix-op (lambda (gwff) (prt-infix-op (exp-var-subst gwff))))
  (prt-prefix-op (lambda (gwff) (prt-prefix-op (exp-var-subst gwff))))  
  (prt-associative-p (lambda (gwff) (prt-associative-p (exp-var-subst gwff))))
  ;; Misc. required operations follow
  (type (lambda (gwff) (type (exp-var-var gwff))))
  (gwff-p (lambda (gwff) (declare (ignore gwff)) T))
  (gwff-q (lambda (gwff) (declare (ignore gwff)) T))
  (legal-type-p1 (lambda (gwff) (legal-type-p1 (exp-var-var gwff))))
  (infix-p (lambda (label) (infix-p (exp-var-subst label))))
  (boundwff-p (lambda (label) (boundwff-p (exp-var-subst label))))
  (lambda-bd-p (lambda (label) (lambda-bd-p (exp-var-subst label))))
  ;; The next gives the default for test operations.
  ;; Stepping operations follow
  (gar (lambda (gwff) (gar (exp-var-subst gwff))))
  (gdr (lambda (gwff) (gdr (exp-var-subst gwff))))
  (glr (lambda (gwff) (glr (exp-var-subst gwff))))
  (grr (lambda (gwff) (grr (exp-var-subst gwff))))
  (free-vars (lambda (gwff bindlist) 
               (free-vars (exp-var-subst gwff)
                          bindlist)))
  (symbols (lambda (gwff bindlist) 
	     (symbols (exp-var-subst gwff)
		      bindlist)))
  (lsymbol-q (lambda (gwff) (lsymbol-q (exp-var-subst gwff))))
  (logconst-q (lambda (gwff) (logconst-q (exp-var-subst gwff))))
  (propsym-q (lambda (gwff) (propsym-q (exp-var-subst gwff))))
  (lambda-norm (lambda (gwff)
                 (setf (exp-var-subst gwff) (lambda-norm (exp-var-subst gwff)))
                   gwff))
  (etanorm (lambda (gwff)
              (setf (exp-var-subst gwff) (etanorm (exp-var-subst gwff)))
                gwff))
  (mqs-applicable (lambda (gwff) (mqs-applicable (exp-var-subst gwff))))
  (min-quant-scope (lambda (gwff) 
			    (let ((subst (exp-var-subst gwff)))
			      (if (mqs-applicable subst)
				  (setf (exp-var-subst gwff) (min-quant-scope subst)))
			      gwff)))
  (intern-subst (lambda (gwff var) (declare (ignore var)) gwff))
  (contains-defn-not-equiv (lambda (wff1)
                             (contains-defn-not-equiv (exp-var-subst wff1))))
  (contains-some-defn (lambda (wff1)
                             (contains-some-defn (exp-var-subst wff1))))
  (get-all-defns-rec (lambda (gwff) (get-all-defns-rec (exp-var-subst gwff))))
  (simul-substitute-term-var-rec 
   (lambda (theta label)
     (let ((newwff (assoc (exp-var-var label) theta)))
       (when newwff
         (setf (exp-var-subst label) newwff)
         label)
       )))
  (binding (lambda (gwff) (binding (exp-var-subst gwff))))
  (bindvar (lambda (gwff) (bindvar (exp-var-subst gwff))))
  (bindhead (lambda (gwff) (bindhead (exp-var-subst gwff))))
  (and-p (lambda (gwff)
           (and-p (exp-var-subst gwff))))
  (or-p (lambda (gwff)
          (or-p (exp-var-subst gwff))))
  (implies-p (lambda (gwff)
          (implies-p (exp-var-subst gwff))))
  (not-p (lambda (gwff)
           (not-p (exp-var-subst gwff))))
  (ae-bd-wff-p (lambda (gwff)
                 (ae-bd-wff-p (exp-var-subst gwff))))
  (a-bd-wff-p (lambda (gwff)
                (a-bd-wff-p (exp-var-subst gwff))))
  (e-bd-wff-p (lambda (gwff)
                (e-bd-wff-p (exp-var-subst gwff))))
  (equality-p (lambda (gwff)
                (equality-p (exp-var-subst gwff))))
  (contains-= (lambda (gwff)
                (contains-= (exp-var-subst gwff))))
  (contains-ext= (lambda (gwff)
                   (contains-ext= (exp-var-subst gwff))))
  (contains-equality  (lambda (gwff complex) (contains-equality (exp-var-subst gwff) complex)))
  (reduct-p (lambda (gwff)
              (reduct-p (exp-var-subst gwff))))
  (equiv-p (lambda (gwff)
             (equiv-p (exp-var-subst gwff))))
  (contains-defn
   (lambda (gwff)
     (contains-defn (exp-var-subst gwff))))
  (contains-defn-1
   (lambda (gwff)
     (contains-defn-1 (exp-var-subst gwff))))
  (instantiate-definitions 
   (lambda (wff chkfn chkargs)
     (declare (special *instantiate-fiddle*))
;     (instantiate-definitions (exp-var-subst wff) chkfn chkargs)))
;the following messes with exp-var-subst, which we don't always want to do (eg when PR97C puts
;an abbreviation into the substitution... MB Thu Oct 30 13:46:12 1997
     (if (contains-defn (exp-var-subst wff))
	 (let ((newwff
		(instantiate-definitions (exp-var-subst wff) chkfn chkargs)))
	   (if (and newwff (or (not (boundp '*instantiate-fiddle*))
			       *instantiate-fiddle*))
					;the case with *instantiate-fiddle* is for ms90-3-expand-etree only...
	       (progn (setf (exp-var-subst wff) newwff) wff)
	     newwff))
       wff)))
  (expand-top=
   (lambda (gwff)
     (if (contains-= (exp-var-subst gwff))
	 (expand-top=-real (exp-var-subst gwff))
       gwff)))
  (expand-all-equalities (lambda (gwff) (expand-all-equalities (exp-var-subst gwff))))
  (instantiate-= (lambda (gwff ckfn ckarg) (instantiate-= (exp-var-subst gwff) ckfn ckarg)))
  (substitutable-vars-of 
   (lambda (label)
     (if (eq (exp-var-var label) (exp-var-subst label))
         (list label)
         (substitutable-vars-of (exp-var-subst label)))))
;  (ab-normalize-main (lambda (gwff) nil))
  (ab-normalize-main
   (lambda (gwff)
     (let ((newwff (ab-normalize-main (exp-var-subst gwff))))
       (when newwff
         (setf (exp-var-subst gwff) newwff))
     gwff)))

  (hnf-rec
   (lambda (pos term args binder bdvars stack subst-stack)
     (hnf-rec pos (exp-var-subst term) args binder bdvars stack
	      subst-stack)))
  (fo-hnf
    (lambda (pos term args subst-stack)
      (fo-hnf pos (exp-var-subst term) args subst-stack)))
  (core::hvars-rec 
    (lambda (gwff)
      (if (eq (exp-var-var gwff) (exp-var-subst gwff)) 
          (list gwff)
          (core::hvars-rec (exp-var-subst gwff)))))
  (core::head
    (lambda (term) (core::head (exp-var-subst term))))
  (nameroot
   (lambda (gwff) (nameroot (exp-var-var gwff))))
  (getnameroot
   (lambda (gwff) (getnameroot (exp-var-var gwff))))
  (nameroot-type
   (lambda (gwff) (nameroot-type (exp-var-var gwff))))
  (ren-var-xa
   (lambda (gwff type) (ren-var-xa (exp-var-var gwff) type)))
  (ren-var-xa-internal
   (lambda (gwff type) (ren-var-xa-internal (exp-var-var gwff) type)))
  (ren-var-x11
   (lambda (gwff type) (ren-var-x11 (exp-var-var gwff) type)))
  (ren-var-x1 
   (lambda (gwff type) (ren-var-x1 (exp-var-var gwff) type)))
  (free-in (lambda (var label)
             (or (eq var label)
		 (member var (free-vars-of (exp-var-subst label))
			 :test #'wffeq))))
;  (free-in (lambda (var label)
;             (or (eq var label)
;                 (free-in var (exp-var-subst label)))))
  (infix-op-p (lambda (label) (infix-op-p (exp-var-subst label))))
  (bdvar-free-in-term (lambda (a b c) (declare (ignore a b c)) (error "BDVAR-FREE-IN-TERM")))
  (wff-applic-p (lambda (x) (wff-applic-p (exp-var-subst x))))
)























