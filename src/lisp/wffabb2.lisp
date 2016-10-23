;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-OPS2)

;;;
;;; File: WFFABB2
;;;
;;; wffops for dealing with abbreviations.
;;;

(deffile wffabb2
  (part-of wff-ops2)
  (extension clisp)
  (mhelp "Contents pertain to abbreviations of wffs."))

(context abbrev-ops)

(defwffop instantiate-defn
  (argtypes symbol gwff)
  (resulttype gwff)
  (argnames gabbr inwff)
  (arghelp "abbrev" "inwff")
  (applicable-p (lambda (gabbr inwff) (declare (ignore inwff))
			(or (abbrev-p gabbr) (pmabbsym-p gabbr))))
  (mhelp "Instantiate all occurrences of an abbreviation.
The occurrences will be lambda-contracted, but not lambda-normalized."))

(defun instantiate-defn (gabbr inwff)
  (instantiate-definitions 
   inwff #'(lambda (abbsym chkarg) (eq abbsym chkarg)) gabbr))

(defwffop instantiate-all
  (argtypes gwff symbollist)
  (resulttype gwff)
  (argnames inwff exceptions)
  (arghelp "inwff" "exceptions")
  (defaultfns (lambda (&rest rest)
		(mapcar #'(lambda (argdefault arg) 
			    (if (eq arg '$) argdefault arg))
			'($ NIL) rest)))
  (mhelp "Instantiate all definitions, except the ones specified
in the second argument."))

(defwffop instantiate-all-rec
  (argtypes gwff symbollist)
  (resulttype gwff)
  (argnames inwff exceptions)
  (arghelp "inwff" "exceptions")
  (defaultfns (lambda (&rest rest)
		(mapcar #'(lambda (argdefault arg) 
			    (if (eq arg '$) argdefault arg))
			'($ NIL) rest)))
  (mhelp "Recursively instantiate all definitions, except the ones specified
in the second argument."))

(defun instantiate-all-rec (inwff exceptions)
  (do ((wff (instantiate-all inwff exceptions)
	    (instantiate-all wff exceptions))
       (wff2 inwff wff))
      ((wffeq-ab wff wff2) wff)))

(defun instantiate-all (inwff exceptions)
  (instantiate-definitions
   inwff #'(lambda (abbsym chkarg) (not (memq abbsym chkarg))) exceptions))

(defwffop instantiate-1
  (argtypes gwff)
  (resulttype gwff)
  (argnames inwff)
  (arghelp "inwff")
  (mhelp "Instantiate the first abbreviation, left-to-right."))

(defun instantiate-1 (inwff)
  (let ((oneflag nil))
    (declare (special oneflag))
    (instantiate-definitions
     inwff #'(lambda (abbsym chkarg)
	       (declare (ignore abbsym chkarg) (special oneflag))
	       (prog1 (not oneflag) (setq oneflag t)))
     nil)))

(defwffop instantiate-top-equality
  (argtypes gwff)
  (resulttype gwff)
  (argnames inwff)
  (arghelp "inwff")
  (wffargtypes "A")    ;
  (wffop-typelist "A") ; these 3 lines added 6/94 MB for equiv-eq-.. to work
  (wffop-type "A")     ;
  (defaultfns (lambda (&rest rest)
		(mapcar #'(lambda (argdefault arg) 
			    (if (eq arg '$) argdefault arg))
			'($) rest)))
  (mhelp "Instantiate outermost equality in gwff. Consults the flag
  REWRITE-EQUALITIES (but ignores it if it's set to NONE)."))

(defun instantiate-top-equality (inwff)
  (let ((oneflag nil))
    (declare (special oneflag))
    (instantiate-=
     inwff #'(lambda (abbsym chkarg)
	       (declare (ignore abbsym chkarg) (special oneflag))
	       (prog1 (not oneflag) (setq oneflag t)))
     nil)))

(defwffrec instantiate-=
  (argnames inwff chkfn chkarg))

(defun instantiate-= (inwff chkfn chkarg)
  (cond ((and (label-q inwff) (not (and (lsymbol-q inwff) (abbrev-q inwff))))
	 ;this is a blatant hack -- the library definitions were being made into weak labels
	 ;as well as abbreviations, and so INSTALL was complaining that it couldn't instantiate
	 ;a weak label...
	 (apply-label inwff (instantiate-= inwff chkfn chkarg)))
	((lsymbol-q inwff)
	 (if (and (equality-p inwff) (funcall chkfn inwff chkarg))
	     (if (memq rewrite-equalities '(only-ext all lazy2 dual none))
		 (instantiate-equality-smart inwff)
	       (instantiate-equality-basic inwff))
	   inwff))
	((label-q inwff)
	 (apply-label inwff (instantiate-= inwff chkfn chkarg)))
	((boundwff-q inwff)
	 (cons (car inwff)
	       (instantiate-= (gdr inwff) chkfn chkarg)))
	(t (let ((newcar (instantiate-= (car inwff) chkfn chkarg)))
	     (cons newcar
		   (instantiate-= (cdr inwff) chkfn chkarg))))))


(defwffop instantiate-equalities
  (argtypes gwff)
  (resulttype gwff)
  (argnames inwff)
  (arghelp "inwff")
  (wffargtypes "A")    ;
  (wffop-typelist "A") ; these 3 lines added 6/94 MB for equiv-eq-.. to work
  (wffop-type "A")     ;
  (defaultfns (lambda (&rest rest)
		(mapcar #'(lambda (argdefault arg) 
			    (if (eq arg '$) argdefault arg))
			'($) rest)))
  (mhelp "Instantiate all equalities in gwff. Consults the flag
  REWRITE-EQUALITIES (but ignores it if it's set to NONE)."))

(defun instantiate-equalities (gwff &optional (pos t))
  (if (eq rewrite-equalities 'core::both)
      ; this setting is only partly implemented
      ; the idea is to expand A(oa)=B(oa) using both leibniz and ext= to get a conjunction/disjunction
      ; see wffabb2, etrees-wffops, etrees-wffops2
      ; translation into ND is not written yet.
      ; MB Mon May 12 14:50:21 1997
      (let* ((rewrite-equalities 'auto::leibniz)
	     (left (expand-all-equalities gwff))
	     (rewrite-equalities 'auto::all)
	     (right (expand-all-equalities gwff)))
	(if (and left right (not (wffeq-ab left right)))
	    (values (cons (cons (if pos 'and 'or) (lnorm left)) (lnorm right)) 'both)
	  (expand-all-equalities gwff)))
    (expand-all-equalities gwff)))

(defun expand-all-equalities (gwff)
  (cond ((label-q gwff)
	 (apply-label gwff (expand-all-equalities gwff)))
	((lsymbol-q gwff) 
	 (if (equality-p gwff)
	     (if (memq rewrite-equalities '(only-ext all lazy2 dual none))
		 (instantiate-equality-smart gwff)
	       (instantiate-equality-basic gwff))
	   gwff))
	((boundwff-q gwff)
	 (cons (car gwff) (expand-all-equalities (cdr gwff))))
	(t (cons (expand-all-equalities (car gwff))
		 (expand-all-equalities (cdr gwff))))))

;;; Symbol is an = of some type

(defun instantiate-equality-smart (symbol)
  (let ((argtype (cdr (get symbol 'core::unabbreviated-type))))
    (cond ((eq 'O argtype)		; equality of type OOO
	   (values 'equiv 'auto::ext=))
	  ((consp argtype)		; functional type
	   (values (instantiate-equality-extensional symbol) 'auto::ext=))
	  ((eq rewrite-equalities 'only-ext) (values symbol nil))
	  (t (values (instantiate-equality-basic symbol) 'auto::leibniz=)))))

(defun instantiate-equality-extensional (symbol)
  (let* ((argtype (cdr (get symbol 'core::unabbreviated-type)))
	 (resulttype (car argtype))
	 (new-equal
	   (create-propsym '=
			   (cons (cons 'O resulttype) resulttype))))
    (when (null (get new-equal 'infix))
	  (setf (get new-equal 'prefix) nil)
	  (setf (get new-equal 'infix) 7)
	  (setf (get new-equal 'printnotype) t)
	  (setf (get new-equal 'core::polytypelist) (list resulttype))
	  (setf (get new-equal 'core::stands-for) '=))
    (do ((new-x ;(fresh-var (cdr argtype) '\x)
	  (create-propsym '\x (cdr argtype)))
	 (new-f (create-propsym '\f argtype)
		(if (logconst-p new-f)
		    (funcall ren-var-fn new-f)
		    new-f))
	 (new-g (create-propsym '\g argtype)
		(if (logconst-p new-g)
		    (funcall ren-var-fn new-g)
		    new-g)))
	((and (not (logconst-p new-f)) (not (logconst-p new-g)))
	 `((,new-f . lambda) (,new-g . lambda)
	   (,new-x . forall) (,new-equal ,new-f . ,new-x)
	                     ,new-g . ,new-x)))))



;;; Symbol is an = of some type, so we need to make some other symbols
;;; first

(defun instantiate-equality-basic (symbol)
  (let ((argtype (cdr (get symbol 'core::unabbreviated-type))))
    (do ((new-q ;(fresh-var (cons 'O argtype) '\q)
	  (create-propsym '\q (cons 'O argtype)))
	 (new-x (create-propsym '\x argtype)
		(if (logconst-p new-x)
		    (funcall ren-var-fn new-x)
		    new-x))
	 (new-y (create-propsym '\y argtype)
		(if (logconst-p new-y)
		    (funcall ren-var-fn new-y)
		    new-y)))
	((and (not (logconst-p new-x))
	      (not (logconst-p new-y)))
	 `((,new-x . lambda)  (,new-y . lambda) 
	   (,new-q . forall)  (implies ,new-q . ,new-x)
					,new-q . ,new-y)))))


