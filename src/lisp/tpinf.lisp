;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-PARSE)

;;;;
;;;; File : TPINF
;;;; Author: fp
;;;;
;;;; This file contains the functions which do type inference on a
;;;; PREWFF.
;;;;


(deffile tpinf
  (part-of wff-parse)
  (extension clisp)
  (mhelp "Contents allow type inferencing."))

(context wff-parsing)

;;; Base-type should be defined inside the logical system,
;;; not here.  Therfore, no base-type is assigned!
;;; It must be set to some type constant, eg I,
;;; in any particular logic library!

(eval-when (compile load eval)
(defmacro getsymtype (symbol)
  `(if (label-p ,symbol)
       (apply-label ,symbol (type ,symbol)) (get ,symbol 'type)))
)
(eval-when (compile load eval)
(defmacro get-temp-symtype (symbol) `(get ,symbol 'temp-type))
)
(eval-when (compile load eval)
(defmacro putsymtype (symbol type) `(putprop ,symbol ,type 'type))
)
(eval-when (compile load eval)
(defmacro put-temp-symtype (symbol type)
  `(putprop ,symbol ,type 'temp-type))
)
;*;(defmacro tvarp (l)
;*;  `(and (atom ,l) (not (get ,l 'parsed-type)) (not (get ,l 'typeconst))
;*;	(not (get ,l 'typeabbrev))))

;;; Changed to function because it's needed in a package loaded earlier
;;; 9/28/87 DAN

(defun tvarp (l)
  (and (atom l) (not (get l 'parsed-type)) (not (get l 'typeconst))
	(not (get l 'typeabbrev))))

;#+comment(eval-when (compile load eval)
;(defmacro tconstp (l) `(or (get ,l 'parsed-type) (get ,l 'typeconst)))
;)

;;; changed this to a function, because it is used in earlier package
;;; DAN 21FEB90
;(defun tconstp (l) 
;  (or (get l 'parsed-type) (get l 'typeconst)))

;;; Here we want to check if l is a type-abbreviation also DAN 27OCT90

(defun tconstp (l) 
  (or (get l 'parsed-type) (get l 'typeconst) (get l 'typeabbrev)))

(eval-when (compile load eval)
(defmacro puttsubst (symbol type)
  `(putprop ,symbol ,type 'typesubst))
)
;;; The next two macros were changed for the sake of global type inference:
;;;  with the first clause in the OR one can use temporary type variables
;;;  to keep information for the different wffargs of the same wffop.


;;; Changed to a function since needed in earlier file. DAN
;*;(defmacro gettsubst (l) `(or (get ,l 'finaltype) (get ,l 'typesubst)))

(defun gettsubst (l) 
 (or (get l 'finaltype) (get l 'typesubst)))

(defun tsubstp (l) (or (get l 'finaltype) (get l 'typesubst)))

(eval-when (compile load eval)
(defmacro putfinaltype (symbol type)
  `(prog1 (putprop ,symbol ,type 'finaltype)
	  (remprop ,symbol 'typesubst)))
)

(defflag untyped-lambda-calculus
  (flagtype boolean)
  (default nil)
  (change-fn (lambda (flag value pvalue) 
                     (declare (ignore flag) (special last-edwff edwff))
                     (if (eq value T) 
                         (progn (setf (get 'cl-user::LAM<I<II>> 'type) 
                                      (cons 'cl-user::i (cons 'cl-user::i 'cl-user::i)))
                                (setf (get 'cl-user::APP<III> 'type) 
                                      (acons 'cl-user::i 'cl-user::i 'cl-user::i))
                                (when (not pvalue) (setq last-edwff nil) (setq edwff nil))
                                (setf (get 'untyped-lambda-calculus 'printtypes) printtypes)
                                (setq printtypes nil))
                         (if (eq value NIL) 
                             (setq printtypes (get 'untyped-lambda-calculus 'printtypes))))))
  (subjects editor)
  (mhelp "Takes values T or NIL. To set it to T if you want to use the editor to deal with 
untyped lambda-calculus."))

;;;the following function is used to encode an untyped lambda term into a
;;;typed term. Only lambda term are allowed here.


(defun exists-typed (prewff)
   (cond ((lsymbol-q prewff) (get prewff 'type))
         ((not (consp prewff)) T)
         ((boundwff-q prewff) 
          (or (get (caar prewff) 'type) (exists-typed (cdr prewff))))
         (t (or (exists-typed (car prewff)) (exists-typed (cdr prewff))))))
   
(defun encode-untyped (prewff)
  (cond ((not (consp prewff)) prewff)
        ((boundwff-q prewff) 
         (cons 'cl-user::LAM<I<II>> 
               (cons (car prewff) (encode-untyped (cdr prewff)))))
        (t (acons 'cl-user::APP<III> 
                   (encode-untyped (car prewff)) 
                   (encode-untyped (cdr prewff))))))

;;;
;;; FINALSCAN assigns types to the logical symbols in PREWFF,
;;; using, basically, Milner's type inference algorithm.
;;;

(defun finalscan (prewff)
  (let ((prewff (if (and untyped-lambda-calculus (not (exists-typed prewff)))
                    (encode-untyped prewff) prewff))
        (nexttvarc 91))
    (declare (special nexttvarc global-type))
    (if global-type
	(%catch% (matchtwo global-type (typeinfer prewff))
	       (fail (throwfail
		      "The type of the parsed formula is not "
		      (global-type . typesym) ".")))
	(typeinfer prewff))
    (finaltype prewff)))


;;;
;;; TYPEINFER infers the types, using temporary type variables.
;;; The inference comes down to a unification algorithm.  The
;;; functions MATCHTWO and TYPEINFER call each other recursively.
;;;

(defun typeinfer (prewff)
  (cond ((atom prewff) (typeatom prewff))
	((boundwff-q prewff) (binder-type prewff))
	(t (let ((ltype (typeinfer (car prewff))))
	     (%catch% (matchtwo (cdrtype ltype)
				(typeinfer (cdr prewff)))
		      (fail (complain f "Types of " ((finaltype(car prewff)) . gwff)
				      " and " ((finaltype (cdr prewff)) . gwff)
				      " do not match.")))
	     (cartype ltype)))))

;;;
;;; TYPEATOM returns the temporary type of a (pre-)logical symbol
;;;

(defun typeatom (prewff)
  (declare (special typeassoc))
  (cond ((get prewff 'stands-for)
	 (getpolytype prewff))
	((get prewff 'meta-label)
	 (getmetatype prewff))
	((getsymtype prewff))
	((get-temp-symtype prewff))
	((get (cdr (assoc prewff typeassoc)) 'type))
	((put-temp-symtype prewff (next-temp-tvar)))))

;;;
;;;BINDER-TYPE returns the temporary type of a bound (pre-)wff.
;;;

(defun binder-type (prewff)
  (let* ((bd (cdar prewff))
	 (polytypevarlist (mapcar #'(lambda (tvar)
				      (cons tvar (next-temp-tvar)))
				  (get bd 'typelist))))
    (%catch% (matchtwo (typeatom (caar prewff))
		     (sublis polytypevarlist (get bd 'var-type)))
	   (fail (complain f "Incorrect type for variable bound by "
			   (bd . fsym)
			   ".")))
    (%catch% (matchtwo (typeinfer (cdr prewff))
		     (sublis polytypevarlist (get bd 'scope-type)))
	   (fail (complain f "Incorrect type for the scope of the binder "
			   (bd . fsym)
			   ".")))
    (sublis polytypevarlist (get bd 'wff-type))))

;;;
;;; GETPOLYTYPE gets the temporary type of a polymorphic object.
;;;

(defun getpolytype (genlsymbol)
  (let ((ab (get genlsymbol 'stands-for))
	(giventype (get genlsymbol 'type)))
    (let ((polytypevarlist
	   (mapcar #'(lambda (tvar) (cons tvar (next-temp-tvar)))
		   (get ab 'typelist))))
      (put-temp-symtype genlsymbol (sublis polytypevarlist (get ab 'type)))
      (putprop genlsymbol polytypevarlist 'polytypelist)
      (cond (giventype (%catch% (matchtwo (get-temp-symtype genlsymbol)
					giventype)
			      (fail (complain f "Incorrect type for polymorphic abbreviation " (ab . gwff)
					      ".")))
		       (put-temp-symtype genlsymbol giventype)
		       ;; In order to get the naming to work out right,
		       ;; we have to remove the 'type property.  otherwsie
		       ;; FINALTYPE will think that GENLSYMBOL should not
		       ;; be touched.  fp.
		       (remprop genlsymbol 'type)))
      (get-temp-symtype genlsymbol))))

;;;
;;; GETMETATYPE gets the temporary type of a meta-label, as arises from
;;; "`(lexpd [f x y x y x] x z `(1 3))".
;;;

(defun getmetatype (meta-label)
  (let ((wffop (get meta-label 'meta-label-wffop)))
    (if (not (get wffop 'wffop-type))
	;;(put-temp-symtype meta-label (next-temp-tvar))
	(throwfail "Illegal WFFOP: " wffop " has no type informations.")
	(let ((polytypevarlist
	       (mapcar #'(lambda (tvar) (cons tvar (next-temp-tvar)))
		       (get wffop 'wffop-typelist))))
	  (put-temp-symtype meta-label (sublis polytypevarlist
					       (get wffop 'wffop-type)))
	  (putprop meta-label polytypevarlist 'polytypelist)
	  (do ((args (get meta-label 'lexlist) (cdr args))
	       (argtps (get wffop 'wffargtypes) (cdr argtps)))
	      ((null args))
	    (when (car argtps)
		  (%catch% (matchtwo (typeinfer (car args))
				   (sublis polytypevarlist (car argtps)))
			 (fail (complain f "Incorrect type in call of wffop "
					 wffop ".")))))))
    (get-temp-symtype meta-label)))

;;;
;;; MATCHTWO does unification of two types
;;;


(defun matchtwo (a b)
  (cond ((eq a b))
	((and (consp a) (consp b))
	 (matchtwo (car a) (car b))
	 (matchtwo (cdr a) (cdr b)))
	((tvarp a)
	 (cond ((and (tvarp b) (tsubstp b))
		(matchtwo a (gettsubst b)))
	       ((tsubstp a) (matchtwo (gettsubst a) b))
	       ((containp a b)
		(throwfail "Type " a " contains " b "."))
	       (t (puttsubst a b))))
	((tvarp b)
	 (cond ((tsubstp b) (matchtwo a (gettsubst b)))
	       ((containp b a)
		(throwfail "Type " b " contains " a "."))
	       (t (puttsubst b a))))
	((typeabbrev-p a) (matchtwo (get-type-def a) b))
	((typeabbrev-p b) (matchtwo a (get-type-def b)))
	(t (throwfail "Clash of two types: " (a . typesym) " cannot match " 
                      (b . typesym) "."))))

;;;
;;; CONTAINP checks whether a type is ultimately contained in itself
;;; which indicates a failure of the unification algorithm.
;;;


(defun containp (x l)
  (cond ((eq x l))
	((atom l) (if (tsubstp l) (containp x (gettsubst l)) nil))
	((or (containp x (cartype l)) (containp x (cdrtype l))))))

;;;
;;;FINALTYPE collects the final assigned types of all (pre-)symbols
;;; in a prewff and destructively changes prewff, in the end
;;; returning a wff.
;;;

(defun finaltype (prewff)
  (cond ((label-q prewff) prewff)
	((lsymbol-q prewff)
	 (if (get prewff 'type) prewff (findfullname prewff)))
	(t (finaltype1 prewff) prewff)))

(defun finaltype1 (prewff)
  (cond ((label-q (car prewff)))
	((lsymbol-q (car prewff))
	 (cond ((not (get (car prewff) 'type))
		(rplaca prewff (findfullname (car prewff))))))
	((boundwff-q prewff)
	 (cond ((not (get (caar prewff) 'type))
		(rplaca (car prewff) (findfullname (caar prewff))))))
	(t (finaltype1 (car prewff))))
  (cond ((label-q (cdr prewff)))
	((lsymbol-q (cdr prewff))
	 (cond ((not (get (cdr prewff) 'type))
		(rplacd prewff (findfullname (cdr prewff))))))
	(t (finaltype1 (cdr prewff)))))

;;;
;;; FINDFULLNAME collects the type of a symbol and returns the full
;;; name of the symbol with the type coded into the identifier.
;;;

(defun findfullname (lsymbol)
  (declare (special typeassoc make-wffops-labels))
  (let (fname ftype stands-for)
    (cond ((setq fname (cdr (assoc lsymbol typeassoc))))
	  ((get lsymbol 'meta-label)
	   (setq ftype (collecttype (get-temp-symtype lsymbol)))
	   (do ((args (get lsymbol 'lexlist) (cdr args))
		(argtps (get (get lsymbol 'meta-label-wffop) 'wffargtypes)
			(cdr argtps))
		(final-args nil))
	       ((null argtps) (putprop lsymbol (nreverse final-args) 'lexlist))
	     (push (if (car argtps) (finaltype (car args))
		       (get (car args) 'lisp-token))
		   final-args))
	   (setq fname
		 (if make-wffops-labels (generate-wffop-label lsymbol ftype)
		     (apply (get lsymbol 'meta-label-wffop)
			    (get lsymbol 'lexlist)))))
	  ((setq stands-for (get lsymbol 'stands-for))
	   (setq ftype (collecttype (get-temp-symtype lsymbol)))
	   (setq fname (inherit-abbrev stands-for ftype
				       (mapcar
					#'(lambda (tppair)
					    (collecttype (cdr tppair)))
					(get lsymbol 'polytypelist))))
	   (remprop lsymbol 'temp-type)
	   ;;(remprop lsymbol 'stands-for)
	   ;; Previous line removed, since it sometimes removed the stands-for
	   ;; property from symbols such as SUBSET<O<OA><OA>>.  fp.
	   )
	  (t (setq ftype (collecttype (get-temp-symtype lsymbol)))
	     (setq fname (create-propsym lsymbol ftype))
	     ;;(putprop fname ftype 'type)
	     (remprop lsymbol 'temp-type)
	     (push (cons lsymbol fname) typeassoc)))
    fname))

;;;
;;;COLLECTTYPE goes through the temporary data structures created
;;; by the type-inference algorithm, returning the final type of
;;; a temporary type variable.
;;;

(defun collecttype (texp)
  (declare (special base-type))
  (cond ((consp texp)
	 (rplaca texp (collecttype (car texp)))
	 (rplacd texp (collecttype (cdr texp))))
	((tconstp texp) texp)
	((tvarp texp)
	 (cond ((get texp 'finaltype))
	       ((tsubstp texp)
		(putfinaltype texp (collecttype (gettsubst texp))))
	       ((or first-order-mode-parse type-iota-mode)
		(if base-type (putfinaltype texp base-type)
		    (throwfail "Variable BASE-TYPE not defined." t
			       "Please set TYPE-IOTA-MODE to NIL or define BASE-TYPE (probably as I).")))
	       (t (putfinaltype texp (next-final-tvar)))))
	(t texp)))

;;;
;;; NEXT-TEMP-TVAR generates another temporary type variable.
;;;

(defun next-temp-tvar () (gensym))

;;;
;;; NEXT-FINAL-TVAR generates the next user-friendly type-variable.
;;;

(defun next-final-tvar ()
  (declare (special nexttvarc))
  (prog (nxvar)
   nt (setq nexttvarc (- nexttvarc 1))
    (cond ((member nexttvarc '(84 43 42 41))
	   (go nt))
	  ((tconstp (intern (string (tps-ascii nexttvarc)) *user*))
	   (go nt))
	  ((< nexttvarc 65)
	   ;;;(error '"Ran out of typevariables.") 8/9/87 DAN
	   (throwfail "Ran out of typevariables.")))
    (setq nxvar (intern (string (tps-ascii nexttvarc)) *user*))
    (putprop nxvar t 'parsed-type)
    (return nxvar)))

(defun next-proposed-tvar ()
  (prog (nexttvarp)
    (setq nexttvarp 64)
   nt (setq nexttvarp (1+ nexttvarp))
    (cond ((member nexttvarp '(84 43 42 41))
	   (go nt))
	  ((tconstp (intern (string (tps-ascii nexttvarp))))
	   (go nt))
	  ((> nexttvarp 90)
	   ;;;(error '"Ran out of typevariables.")  8/9/87 DAN
	   (throwfail "Ran out of typevariables.")))
    (return (intern (string (tps-ascii nexttvarp))))))

;;;
;;; INITTYPES resets the structure of type variables and constants.
;;;  It should not be called by any function and is here mainly for
;;;  debugging purposes.  It's use is dangerous!
;;;

(defun inittypes ()
  (dolist (elt '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
    (remprop elt 'typesubst)
    (remprop elt 'finaltype)
    (remprop elt 'parsed-type)))

;;;
;;; CARTYPE returns the car of a type, and creates new temporary
;;; type varaibles if necessary.
;;;

(defun cartype (texp)
  (cond ((consp texp) (car texp))
	((or (get texp 'parsed-type) (get texp 'typeconst))
         (throwfail "Type " (texp . typesym)
		    "is a base type and not a function type ."))
        ((get texp 'typeabbrev)
         (cartype (get texp 'type-defn)))
	((tsubstp texp) (cartype (gettsubst texp)))
	((typeabbrev-p texp) (cartype (get-type-def texp)))
	(t (car (puttsubst texp (cons (next-temp-tvar) (next-temp-tvar)))))))

;;; like CARTYPE, just CDR.

(defun cdrtype (texp)
  (cond ((consp texp) (cdr texp))
	((or (get texp 'parsed-type) (get texp 'typeconst))
         (throwfail "Type " (texp . typesym)
		    "is a base type and not a function type ."))
        ((get texp 'typeabbrev)
         (cdrtype (get texp 'type-defn)))
	((tsubstp texp) (cdrtype (gettsubst texp)))
	((typeabbrev-p texp) (cdrtype (get-type-def texp)))
	(t (cdr (puttsubst texp (cons (next-temp-tvar) (next-temp-tvar)))))))

;;;
;;; INITTYPEVAR is like INITTYPES, just more selectively resets some
;;; types so they can be used as variables again.
;;;

(defun inittypevar (tsymlist)
  (dolist (tsym tsymlist)
    (remprop tsym 'typesubst)
    (remprop tsym 'finaltype)
    (remprop tsym 'parsed-type)))

