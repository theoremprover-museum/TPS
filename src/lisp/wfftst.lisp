;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFFS)

;;;
;;; File: WFFTST
;;;
;;; defines tests for particular kinds of wffs.
;;;

(deffile wfftst
  (part-of wffs)
  (extension clisp)
  (mhelp "Contains tests on wffs."))

(context wfftst-obj)

(push '(defwfftest . wffop) global-definelist)

(defmacro defwfftest (tps-object &rest props)
  `(defwffop ,tps-object
	  (argtypes gwff-ill)
	  (resulttype boolean)
	  (argnames gwff)
	  (arghelp "gwff")
	  ,@props))

(defutil defwfftest
  (form-type macro)
  (keywords define)
  (mhelp "DEFWFFTEST expands to a DEFWFFOP, where certain attributes are
given defaults.  Its intended use is for predicates on wffs.
(DEFWFFTEST tps-object &rest props)
 will set RESULTTYPE to BOOLEAN, ARGTYPES to (GWFF-ILL) and ARGNAMES to
(GWFF).  Additional properties may be defined and defaults overridden
through props."))

(defwfftest logconst-p
  (mhelp "Test for a logical constant (e.g. AND, OR, etc.)"))

;;;
;;;(dc logconst-q) Call after knowing we have a literal atom. 
;;;

(defun logconst-q (gwff) (get gwff 'logconst))

(defun logconst-p (gwff)
  (and (symbolp gwff) (get gwff 'type) (get gwff 'logconst)))
	

(defwfftest propsym-p
  (mhelp "Test whether argument is a proper symbol."))

;;;
;;;(dc propsym-q) Call after testing for LOGCONST.
;;;  Argument must be a logical symbol.
;;;

(defun propsym-q (gwff) (and (not (get gwff 'stands-for))
			     (not (get gwff 'defn))))

(defun propsym-p (gwff)
  (and (symbolp gwff)
       (get gwff 'type) (not (get gwff 'logconst))
       (not (get gwff 'stands-for))))

(defwfftest pmpropsym-p
  (mhelp "Test for a polymorphic proper symbol (e.g. something
standing for PI or IOTA)."))

;;;
;;;(dc pmpropsym-q) Call after testing for LOGCONST, PROPSYM. 
;;;  Argument must be a logical symbol.
;;;

(defun pmpropsym-q (gwff) (pmprsym-q (get gwff 'stands-for)))

(defun pmpropsym-p (gwff)
  (and (symbolp gwff)
       (get gwff 'type) (get gwff 'stands-for)
       (pmprsym-p (get gwff 'stands-for))))

(defun pmprsym-p (symbol)
  (and (get symbol 'type) (get symbol 'typelist)
       (not (get symbol 'defn))))
(defun pmprsym-q (symbol)
  (and symbol (not (get symbol 'defn))))

(defwfftest pmabbrev-p
  (mhelp "Test for a polymorphic abbreviation (e.g. something
standing for SUBSET or IMAGE)."))

;;;
;;;(dc pmabbrev-q) Call after testing for LOGCONST, PROPSYM, PMPROPSYM. 
;;;  Argument must be a logical symbol.
;;;

(defun pmabbrev-q (gwff) (pmabbsym-q (get gwff 'stands-for)))

(defun pmabbrev-p (gwff)
  (and (symbolp gwff)
       (get gwff 'type) (get gwff 'stands-for)
       (pmabbsym-p (get gwff 'stands-for))))

(defun pmabbsym-p (symbol)
  (and (get symbol 'type) (get symbol 'typelist)
       (get symbol 'defn)))
(defun pmabbsym-q (symbol)
  (and symbol (get symbol 'defn)))


(defwfftest abbrev-p
  (mhelp "Test for a non-polymorphic abbreviation."))

;;;
;;;(dc abbrev-q) Call after testing for LOGCONST, PROPSYM, PMPROPSYM, PMABBREV.
;;;  Argument must be a logical symbol.
;;;

(defun abbrev-q (gwff) (and (symbolp gwff) (get gwff 'defn)))

(defun abbrev-p (gwff)
  (and (symbolp gwff)
       (get gwff 'type)
       (get gwff 'defn) (not (get gwff 'typelist))))

(defwfftest anyabbrev-p
  (mhelp "Test for defined symbol."))

(defun anyabbrev-q (gwff) (or (abbrev-q gwff) (pmabbrev-q gwff)))

(defun anyabbrev-p (gwff) (or (abbrev-p gwff) (pmabbrev-p gwff)))

(defwfftest anypropsym-p
  (mhelp "Test for undefined symbol."))

(defun anypropsym-q (gwff) (or (propsym-q gwff) (pmpropsym-q gwff)))

(defun anypropsym-p (gwff) (or (propsym-p gwff) (pmpropsym-p gwff)))

(defwfftest lsymbol-p
  (mhelp "Test for a logical symbol (formerly HATOM)."))

;;;
;;;(dc lsymbol-q) Call after testing for LABEL. 
;;;

(defun lsymbol-q (gwff) (symbolp gwff))

(defun lsymbol-p (gwff)
  (or (propsym-p gwff)
      (logconst-p gwff)
      (abbrev-p gwff)
      (pmabbrev-p gwff)
      (pmpropsym-p gwff)))

;;; Added this because other wff tests were using consp to check for
;;; this condition, without determining that their argument was not a label.
;;; DAN 12APR91

(defwfftest wff-applic-p
  (mhelp "Test for an application of a wff (function) to another wff (arg)."))

(defun wff-applic-p (gwff)
  (cond ((consp gwff) t)
	((label-q gwff) (apply-label gwff (wff-applic-p gwff)))
	(t nil)))


(defwfftest boundwff-p
  (mhelp "Test for a top-level binder (e.g. LAMBDA, FORALL)."))

;;;
;;;(dc boundwff-q) Call after knowing we do not have a literal atom. 
;;;

#|(defun boundwff-q (gwff)
  (and (consp gwff) (consp (car gwff)) (binder-q (cdar gwff))))

(defun boundwff-p (gwff)
  (and (consp gwff) (consp (car gwff))
       (binder-p (cdar gwff)) (lsymbol-p (caar gwff))))
|#

;;; in following functions we don't know that gwff is not a label,
;;; but we assume that it is a gwff.
;;; Assumption is that all bound wffs will have a gar of form (var . binder)

(defun boundwff-q (gwff)
  (and (wff-applic-p gwff) 
       (let ((gar (if (label-q gwff) (apply-label gwff (gar gwff))
		    (car gwff))))
	 (and (consp gar)
	      (binder-q (cdr gar))))))

(defun boundwff-p (gwff)
  (and (wff-applic-p gwff) 
       (let ((gar (if (label-q gwff) (apply-label gwff (gar gwff))
		    (car gwff))))
	 (and (consp gar)
	      (binder-p (cdr gar))
	      (lsymbol-p (car gar))))))


(defun binder-q (symbol) (and (symbolp symbol) (get symbol 'binder)))

(defun binder-p (symbol)
  (and (symbolp symbol)
       (get symbol 'binder)
       (get symbol 'var-type) (get symbol 'scope-type)
       (get symbol 'wff-type)))

(defwfftest gwff-p
  (mhelp "Test for a gwff (general well-formed formula)."))

(defvar gwff-p-looping nil) ; cebrown 4/1/02

(defun gwff-p (gwff)
  (declare (special again))
  (if gwff-p-looping
      (and (gwff-q gwff) (legal-type-p gwff))
    (unwind-protect
	(progn
	  (setq gwff-p-looping t) ; cebrown 4/1/02 - to fix a loop when expertflag is NIL
	  (or (and (gwff-q gwff) (legal-type-p gwff))
	      (and (symbolp gwff) (module-loaded-p 'library)
		   (prompt-read again nil (msgf gwff " is not a known gwff. Search for it in library?") 
				'yesno 'yes nil)
		   (go-fetch-gwff gwff))))
      (setq gwff-p-looping nil))))

(defun go-fetch-gwff (gwff)
  (declare (special global-type show-all-libobjects))
  (let ((load-warn-p nil)
	(dummy-global-type global-type)
	(global-type nil))
    (if (or (retrieve-libobject gwff :type 'gwff :multiple show-all-libobjects)
	    (retrieve-libobject gwff :type 'abbr :multiple show-all-libobjects))
	(let ((global-type dummy-global-type))
	  (getwff-subtype #'gwff-p gwff))
      ;type-check what we got - it may be that we're looking for a gwff0 and we just got an abbr of some other type.
      (progn (msgf "Can't find a gwff or abbr called " gwff t t) nil))))

(defwffrec gwff-q
  (argnames gwff))

(defun gwff-q (gwff)
  (cond ((label-p gwff) (apply-label gwff (gwff-q gwff)))
	((lsymbol-p gwff) t)
	((atom gwff) nil)
	((and (boundwff-p gwff) (gvar-p (caar gwff)) (gwff-q (cdr gwff))))
	((and (gwff-q (car gwff)) (gwff-q (cdr gwff))))))

(defwfftest gvar-p
  (mhelp "Test for a logical variable (a logical symbol, but no abbrev.)."))

(defun gvar-p (gwff) (or (propsym-p gwff) (pmpropsym-p gwff) (logconst-p gwff)))
(defun gvar-q (gwff) (gvar-p gwff))

;;;Modified with new implementation of flavors 9/26/87 DAN

(defwfftest label-p
  (mhelp "Test for a label (of any flavor)."))

;;; not-true-or-nil is introduced solely for efficiency, to avoid
;;; evaluating (get gwff 'flavor) more than once  DAN

(defun not-true-or-nil (x)
  (if x (neq x t)))


;;; A label is either a symbol or a structure that includes the
;;; type structured-tps-label (see the file flavoring).

(defun label-p (gwff)
 (label-q gwff))

(defun label-q (gwff)
  (or (and (symbolp gwff) (not-true-or-nil (get gwff 'flavor)))
      (structured-tps-label-p gwff)))

(defwfftest reduct-p
  (mhelp "Test for a top-level reduct."))

(defun reduct-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (reduct-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) nil)
	(t (lambda-bd-p (car gwff)))))

(defwfftest legal-type-p
  (mhelp "Test for a legal type."))

(defun legal-type-p (gwff)
  (%catch% (legal-type-p1 gwff) (fail nil)))

(defwffrec legal-type-p1
  (argnames gwff))

(defun legal-type-p1 (gwff)
  (cond ((label-q gwff) (apply-label gwff (legal-type-p1 gwff)))
	((lsymbol-q gwff) (get gwff 'type))
	((boundwff-q gwff) (legal-boundwfftype-p1 gwff))
	(t (let ((cartype (legal-type-p1 (car gwff)))
		 (cdrtype (legal-type-p1 (cdr gwff))))
	     (cond ((equal-type-p (type-cdr cartype) cdrtype)
		    (type-car cartype))
		   (t (throwfail t)))))))

(defun legal-boundwfftype-p1 (gwff)
  (let ((bd (cdar gwff))
	polytypevarlist)
    (setq polytypevarlist
	  (mapcar #'(lambda (tvar) (cons tvar (next-temp-tvar)))
		  (get bd 'typelist)))
    (%catch% (matchtwo (legal-type-p1 (caar gwff))
		       (sublis polytypevarlist (get bd 'var-type)))
	     (fail (throwfail "Incorrect type for variable bound by "
			      (bd . fsym)
			      ".")))
    (%catch% (matchtwo (legal-type-p1 (cdr gwff))
		       (sublis polytypevarlist (get bd 'scope-type)))
	     (fail (throwfail "Incorrect type for the scope of the binder "
			      (bd . fsym)
			      ".")))
    (collecttype (sublis polytypevarlist (get bd 'wff-type)))))


(defwffop infix-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Test for a wff with top-level infix operator."))

(defun infix-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (infix-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) nil)
	(t (and (wff-applic-p (gar gwff)) (infix-op-p (gar (gar gwff)))))))

(defwffop infix-op-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Test whether gwff is an infix operator."))

(defun infix-op-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (infix-op-p gwff)))
	((lsymbol-q gwff) (get gwff 'infix))
	(t nil)))

(defwffop type-equal
  (argtypes gwff gwff)
  (resulttype boolean)
  (argnames gwff1 gwff2)
  (arghelp "gwff1" "gwff2")
  (mhelp "Test whether the types of two wffs are the same."))

(defun type-equal (gwff1 gwff2)
  (equal (unabbreviated-type gwff1) (unabbreviated-type gwff2)))

(defwffop equal-type-p
  (argtypes wfftype wfftype)
  (argnames type1 type2)
  (arghelp "Type" "Type")
  (resulttype boolean)
  (mhelp "Test whether two types are the same."))

(defun equal-type-p (type1 type2)
  (cond ((or (atom type1) (atom type2))
	 (cond ((not (atom type1)) (equal-type-p type2 type1))
	       ((eq type1 type2))
	       ((typeabbrev-p type1) (equal-type-p (get-type-def type1) type2))
	       ((typeabbrev-p type2) (equal-type-p type1 (get-type-def type2)))
	       (t nil)))
	(t (and (equal-type-p (car type1) (car type2))
		(equal-type-p (cdr type1) (cdr type2))))))

(defwffop implies-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp gwff)
  (mhelp "Test whether wff is an implication."))

(defun implies-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (implies-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) nil)
	(t (and (wff-applic-p (car gwff)) (eq (gar (car gwff)) 'implies)))))

(defwffop equiv-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp gwff)
  (mhelp "Test whether wff is an equivalence."))

(defun equiv-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (equiv-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) nil)
	(t (and (wff-applic-p (car gwff)) (eq (gar (car gwff)) 'equiv)))))

(defwffop and-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp gwff)
  (mhelp "Test whether wff is an conjunction."))

(defun and-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (and-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) nil)
	(t (and (wff-applic-p (car gwff)) (eq (gar (car gwff)) 'and)))))


(defwffop or-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp gwff)
  (mhelp "Test whether wff is a disjunction."))

(defun or-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (or-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) nil)
	(t (and (wff-applic-p (car gwff)) (eq (gar (car gwff)) 'or)))))

(defwffop not-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp gwff)
  (mhelp "Test whether wff is negated."))

(defun not-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (not-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) nil)
	(t (eq (car gwff) 'not))))

(defwffop ae-bd-wff-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp gwff)
  (mhelp "Test whether wff is universally or existentially quantified."))

(defun ae-bd-wff-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (ae-bd-wff-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff)
	 (or (eq (cdar gwff) 'forall)
	     (eq (cdar gwff) 'exists)))
	(t nil)))

(defwffop lambda-bd-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp gwff)
  (mhelp "Test whether wff is bound by lambda."))

(defun lambda-bd-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (lambda-bd-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff)
	 (eq (cdar gwff) 'lambda))
	(t nil)))


(defwffop free-in
  (argtypes gvar gwff)
  (resulttype boolean)
  (argnames gvar inwff)
  (arghelp "var" "inwff")
  (mhelp "Test whether a variable is free in a gwff."))

(defun free-in (var inwff)
  (cond ((label-q inwff) (apply-label inwff (free-in var inwff)))
	((lsymbol-q inwff) (eq var inwff))
	((boundwff-q inwff) (and (not (eq var (caar inwff)))
				 (free-in var (cdr inwff))))
	(t (or (free-in var (car inwff)) (free-in var (cdr inwff))))))

(defun free-for (term var inwff)
  (cond ((label-q inwff)
	 (apply-label inwff (free-for term var inwff)))
  	((lsymbol-q inwff) t)
	((boundwff-q inwff)
	 (cond ((eq (caar inwff) var) t)
	       ((free-in (caar inwff) term)
		(not (free-in var (cdr inwff))))
	       (t (free-for term var (cdr inwff)))))
	(t (and (free-for term var (car inwff))
		(free-for term var (cdr inwff))))))

(defwffop a-bd-wff-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Test whether wff is universally quantified."))

(defun a-bd-wff-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (a-bd-wff-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff)
	 (eq (cdar gwff) 'forall))
	(t nil)))

(defwffop e-bd-wff-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp "gwff")
  (mhelp "Test whether wff is existentially quantified."))

(defun e-bd-wff-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (e-bd-wff-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff)
	 (eq (cdar gwff) 'exists))
	(t nil)))

(defmacro infix-op (gwff)
  `(caar ,gwff))

(defwffop equals-p
  (argtypes gwff)
  (resulttype boolean)
  (argnames gwff)
  (arghelp gwff)
  (mhelp "Test whether wff is an equality."))

(defun equals-p (gwff)
  (cond ((label-q gwff) (apply-label gwff (equality-p gwff)))
	((lsymbol-q gwff) nil)
	((boundwff-q gwff) nil)
	(t (and (wff-applic-p (car gwff)) (equality-p (gar (car gwff)))))))

(defun equality-p (gwff &optional (complex 'ignore))
  (if (label-q gwff)
      (apply-label gwff (equality-p gwff))
      (and (symbolp gwff) 
           (eq '= (get gwff 'stands-for))
            (let ((argtype (cdr (get gwff 'unabbreviated-type))))
              (case complex
                 (ignore t)
                 (iota (eq 'ml::I argtype))
                 (ext= (or (eq 'ml::O argtype) (consp argtype))))))))			                   
           

(defun contains-equality (gwff complex)
  (cond ((label-q gwff) (apply-label gwff (contains-equality gwff complex)))
	((lsymbol-q gwff) (equality-p gwff complex))
	((boundwff-q gwff) (contains-equality (gdr gwff) complex))
	(t (or (contains-equality (car gwff) complex) 
               (contains-equality (cdr gwff) complex)))))

(defun contains-= (gwff)
   (contains-equality gwff 'ignore))

(defun contains-ext= (gwff)
  (contains-equality gwff 'ext=))

(defun contains-ext=i (gwff)
  (contains-equality gwff 'iota))

(defun real-ext=-p (gwff complex)
   (cond ((label-q gwff) 
          (apply-label gwff (real-ext=-p gwff complex)))
         ((lsymbol-q gwff) nil)
         ((boundwff-q gwff) nil)
         (t  (and (wff-applic-p (car gwff))
                  (let ((head (gar (car gwff))))
                     (and (symbolp head)
                          (eq '= (get head 'stands-for))
                          (case complex
                                (compose (consp (cdr (get head
			                                  'unabbreviated-type))))
                                (o (eq 'ml::O (cdr (get head
			                            'unabbreviated-type)))))))))))
(defmacro ext=-p (gwff)
   `(real-ext=-p ,gwff 'compose))

(defmacro ext=0-p (gwff)
   `(real-ext=-p ,gwff 'o))

