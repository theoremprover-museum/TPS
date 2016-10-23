;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-OPS-ABB)

(deffile wffabb
  (part-of wff-ops-abb)
  (extension clisp)
  (mhelp "Defines basic recursive wffs for definitions."))

(context abbrev-ops)
(defwffop substitute-bdvar-scope
  (argtypes gvar gvar gvar gwff gwff)
  (wffargtypes "A" "A" "B" "B" "C")
  (resulttype gwff)
  (wffop-type "C")
  (wffop-typelist "A" "B" "C")
  (argnames def-var newvar scope-var scope inwff)
  (arghelp "Old bound variable" "New bound variable"
	   "Variable for which scope is substituted"
	   "Scope of instantiated binder")
  (mhelp "Creates instantiation from binder definition, etc."))

(defun substitute-bdvar-scope (def-var newvar scope-var scope inwff)
  (cond ((label-q inwff) (apply-label inwff
				      (substitute-bdvar-scope
				       def-var newvar scope-var scope inwff)))
  	((lsymbol-q inwff)
	 (cond ((equal inwff def-var) newvar)
	       ((equal inwff scope-var) scope)
	       (t inwff)))
	((boundwff-q inwff)
	 (cond ((equal (bindvar inwff) def-var)
		(bind-var-wff (binding inwff) newvar
			      (substitute-bdvar-scope def-var newvar scope-var
						      scope (gdr inwff))))
	       ((and (free-in (bindvar inwff) scope)
		     (free-in scope-var (gdr inwff)))
		(substitute-bdvar-scope def-var newvar scope-var
					scope (rename-bd-var inwff)))
	       (t (bind-var-wff
		   (binding inwff) (bindvar inwff)
		   (substitute-bdvar-scope
		    def-var newvar scope-var scope (gdr inwff))))))
	(t (apply-wff
	    (substitute-bdvar-scope
	     def-var newvar scope-var scope (gar inwff))
	    (substitute-bdvar-scope
	     def-var newvar scope-var scope (gdr inwff))))))

(defun get-def-binder (binder var scope)
  (let* ((typlis (get binder 'typelist))
	 (scope-var (get binder 'def-scope))
	 (def-var (get binder 'def-var))
	 (defn (get binder 'defn))
	 (type-cons-1 (ds-con-match (get binder 'var-type) (type var) typlis))
	 (type-vars-try (car type-cons-1))
	 (type-instances (cdr type-cons-1))
	 (type-cons-2 (ds-con-match (get binder 'scope-type) (type scope)
				    typlis))
	 (type-vars (setintersect type-vars-try (car type-cons-2))))
    (dolist (tcons (cdr type-cons-2))
      (if (member (car tcons) type-vars-try)
	  (push tcons type-instances)
	  (if (equal tcons (assoc (car tcons) type-instances))
	      nil (throwfail "Type mismatch for binder"))))
    (setq type-vars-try nil)
    (dolist (tvar type-vars)
      (let ((ntvar (next-proposed-tvar)))
	(putprop ntvar t 'parsed-type)
	(push (cons tvar ntvar) type-vars-try)))
    (setq def-var (substitute-types type-vars-try def-var)
	  scope-var (substitute-types type-vars-try scope-var)
	  defn (substitute-types type-vars-try defn)
	  def-var (substitute-types type-instances def-var)
	  scope-var (substitute-types type-instances scope-var)
	  defn (substitute-types type-instances defn))
;;; Added check to see if bd-vars in defn should be renamed DAN 2-22-88
;;;(HX) This is not a correct fix: imagine the def-var gets renamed in
;;;(ab-normalize defn). EXIST1 is such a definition.
    #+comment
    (if rename-all-bd-vars
	(substitute-bdvar-scope def-var var scope-var scope (ab-normalize defn))
      (substitute-bdvar-scope def-var var scope-var scope defn))
    (substitute-bdvar-scope def-var var scope-var scope defn)
))

(defun get-def-binder1 (bdwff)
  (get-def-binder (binding bdwff) (bindvar bdwff) (gdr bdwff)))

(defwffrec instantiate-definitions
  (argnames inwff chkfn chkarg))

(defun instantiate-definitions (inwff chkfn chkarg)
  (cond ((and (label-q inwff) (not (and (lsymbol-q inwff) (abbrev-q inwff))))
	 ;this is a blatant hack -- the library definitions were being made into weak labels
	 ;as well as abbreviations, and so INSTALL was complaining that it couldn't instantiate
	 ;a weak label...
	 (apply-label inwff (instantiate-definitions inwff chkfn chkarg)))
	((lsymbol-q inwff)
	 (cond ((or (logconst-q inwff) (propsym-q inwff) (pmpropsym-q inwff))
		inwff)
	       ((pmabbrev-q inwff)
		(if (funcall chkfn (get inwff 'stands-for) chkarg)
		    (get-pmdefn inwff) inwff))
	       ((abbrev-q inwff)
		(if (funcall chkfn inwff chkarg) (get-defn inwff) inwff))))
	((label-q inwff)
	 (apply-label inwff (instantiate-definitions inwff chkfn chkarg)))
	((boundwff-q inwff)
	 (if (and (anyabbrev-q (binding inwff))
		  (funcall chkfn (binding inwff) chkarg))
	     (get-def-binder (binding inwff) (bindvar inwff) (gdr inwff))
	     (cons (car inwff)
		   (instantiate-definitions (gdr inwff) chkfn chkarg))))
	(t (let ((newcar (instantiate-definitions (car inwff) chkfn chkarg)))
	     (if (and (lambda-bd-p newcar) (not (lambda-bd-p (car inwff))))
		 (lcontr (cons newcar
			       (instantiate-definitions (cdr inwff)
							chkfn chkarg)))
		 (cons newcar
		       (instantiate-definitions (cdr inwff) chkfn chkarg)))))))

(defwffop top-level-defn
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (resulttype boolean)
  (mhelp "Tests whether the argument is a top-level definition."))

(defun top-level-defn (gwff)
  (cond ((label-q gwff) (apply-label gwff (top-level-defn gwff)))
	((lsymbol-q gwff)
	 (if (or (logconst-q gwff) (propsym-q gwff) (pmpropsym-q gwff))
	     nil t)) ;; Here it must be an abbrev or pmabbrev..
	((boundwff-q gwff) (get (binding gwff) 'defn))
	(t ;; Look at CAR, but not at CDR to find top-level abbrev.
	 (top-level-defn (car gwff)))))

(defwffop contains-defn
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (resulttype boolean)
  (mhelp "Tests whether the argument contains a definition."))

(defun contains-defn (gwff)
  (cond ((label-q gwff) (apply-label gwff (contains-defn gwff)))
	((lsymbol-q gwff)
	 (if (or (logconst-q gwff) (propsym-q gwff) (pmpropsym-q gwff))
	     nil t)) ;; Here it must be an abbrev or pmabbrev.
	((boundwff-q gwff)
	 (or (get (binding gwff) 'defn) (contains-defn (gdr gwff))))
	(t (or (contains-defn (gar gwff)) (contains-defn (gdr gwff))))))

(defwffop abbr-list
  (argtypes gwff)
  (argnames gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (resulttype symbollist)
  (mhelp "Lists all the abbreviations used in a gwff."))

(defun abbr-list (gwff &optional (l nil))
  (cond ((weak-label-p gwff) (abbr-list (get gwff 'represents) l))
	((label-q gwff) (apply-label gwff (abbr-list gwff l)))
	((lsymbol-q gwff)
	 (if (or (logconst-q gwff) (propsym-q gwff) (pmpropsym-q gwff))
	     l (cons gwff l))) ;; Here it must be an abbrev or pmabbrev.
	(t (abbr-list (gar gwff) (abbr-list (gdr gwff) l)))))

(defwffop new-defs
  (argtypes gwff)
  (argnames gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (resulttype symbollist)
  (mhelp "Lists all the definitions used in a gwff that are
either library abbreviations or weak labels."))

(defun new-defs (gwff)
  (let ((outwff nil))
    (if (weak-label-p gwff) 
	(setq outwff (get gwff 'represents))
      (setq outwff gwff))
    (append (mapcar #'(lambda (x) (get x 'core::stands-for)) (lib-abbr-list outwff))
	    (weak-label-list outwff))))

(defun weak-label-list (gwff &optional (l nil))
  (cond ((weak-label-p gwff) (cons gwff l))
	((label-q gwff) (apply-label gwff (abbr-list gwff l)))
	((lsymbol-q gwff) l)
	(t (weak-label-list (gar gwff) (weak-label-list (gdr gwff) l)))))

(defwffop lib-abbr-list
  (argtypes gwff)
  (argnames gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (resulttype symbollist)
  (mhelp "Lists all the library abbreviations used in a gwff."))

(defun lib-abbr-list (gwff)
  (remove-if-not #'(lambda (x) (library-theorem-p (get x 'stands-for))) (abbr-list gwff)))

(defwffop const-list
  (argtypes gwff)
  (argnames gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (resulttype symbollist)
  (mhelp "Lists all the logical constants used in a gwff, apart 
from the primitive constants AND FALSEHOOD IMPLIES NOT OR TRUTH."))

(defun const-list (gwff)
  (remove-duplicates (const-list-real gwff)))

(defun const-list-real (gwff &optional (l nil))
  (cond ((label-q gwff) (apply-label gwff (const-list-real gwff l)))
	((lsymbol-q gwff)
	 (if (and (logconst-q gwff) (not (rassoc 'prim-obj (get gwff 'contexts))))
	     (cons gwff l)
	   (if (pmpropsym-p gwff) ; cebrown 8/29/01
	       (cons gwff l) ; so we also get polymorphic lib-consts like INFIX-LESS
	     l)))
	(t (const-list-real (gar gwff) (const-list-real (gdr gwff) l)))))

(defflag rewrite-equalities
  (flagtype rewrite-defns)
  (default 'all)
  (subjects mating-search wff-prims important ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms98-1 transmit)
  (mhelp "One of the following:
NONE: do not rewrite equalities
ONLY-EXT: rewrite only those equalities that can be rewritten using
          extensionality.
LEIBNIZ: rewrite all equalities using the Leibniz definition.
ALL: rewrite all equalities, to an equivalence for those of type OOO,
     to the extensional form 
      [lambda f(AB) lambda g(AB) forall x(B) f x = g x]
     for those of type O(AB)(AB), and to the Leibniz form
      [lambda x(A) lambda y(A) forall q(OA). q x implies q y]
     for those of type OAA.
LAZY2: As for ALL, but keeping a duplicate leaf as in the LAZY2
       setting of the flag REWRITE-DEFNS.
PARITY1: Uses the parity to determine whether equalities should be
    rewritten as the setting LEIBNIZ or as the setting ALL.  For example,
    using PARITY1 when trying to prove the wff 
              A(OI) = B(OI) implies C
    the equality is expaned using Leibniz, and when trying to prove the wff
              D implies A(OI) = B(OI)
    the equality is expanded using extensionality.  The heuristic
    is that we often use the substitutivity property when we use an equation
    and use extensionality to show an equation."))

(definfo only-ext
  (mhelp "A flag setting for REWRITE-EQUALITIES.
When rewriting an equality (during a ND proof or when constructing 
an etree), rewrite only those equalities that can be rewritten using
extensionality."))

(definfo leibniz
  (mhelp "A flag setting for REWRITE-EQUALITIES.
When rewriting an equality (during a ND proof or when constructing 
an etree), rewrite every equality using the Leibniz definition
      [lambda x(A) lambda y(A) forall q(OA). q x implies q y]"))

(definfo all
  (mhelp "A flag setting for REWRITE-EQUALITIES.
When rewriting an equality (during a ND proof or when constructing 
an etree), rewrite every equality as follows:
 to an equivalence for those of type OOO,
 to the extensional form 
      [lambda f(AB) lambda g(AB) forall x(B) f x = g x]
      for those of type O(AB)(AB)
 to the Leibniz form
      [lambda x(A) lambda y(A) forall q(OA). q x implies q y]
      for those of type OAA."))

