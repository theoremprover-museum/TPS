;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :core)
(part-of WFFS)

(deffile wfftyp
  (part-of wffs)
  (extension clisp)
  (mhelp "Contents pertaining to types of wffs."))

(context wfftyp-obj)

;;; PRTWFF is here because it is used in some printing functions
;;; for the argument type GWFF.  In that sense, the packages WFFS
;;; and WFF-PRINT cannot be separated completely.

(defmacro prtwff (gwff &rest bindings)
  `(let ,bindings 
     (pwff ,gwff)))

(defvar global-type nil)

(defutil prtwff
  (form-type macro)
  (keywords output)
  (mhelp "(PRTWFF gwff (flag1 value1) ... (flagn valuen))  is the one of
the two canonical ways of printing wffs in TPS.  It will bind flag1 to
value1 etc. and then print gwff.  This is useful to write commands or
functions which print gwff in a particular style.  For example
(PRTWFF A (USE-DOTS NIL) (PRINTDEPTH 0))
will print the wff A without using dots and showing all levels.
The other way of printing wffs with MSG is (MSG (A. GWFF)).
If a certain combination of flag settings is used more than once,
consider using (DEFMODE USEFUL-MODE ...) and (IN-MODE USEFUL-MODE (PRTWFF A))
instead."))

(defcategory wffrec%
  (define defwffrec)
  (properties
   (argnames multiple)
   (multiple-recursion single)
   (mhelp single))
  (global-list global-wffreclist)
  (mhelp-line "recursive wff function")
  (mhelp-fn princ-mhelp))

(eval-when (load compile eval)
(defcategory getgwfftype
  (define defgwff-type)
  (properties
   (checkfn singlefn)
   (getfn singlefn)
   (mhelp single))
  (global-list global-gwfftypelist)
  (mhelp-line "wff reference format")
  (mhelp-fn princ-mhelp))
)


(defun get-gwff0 (gwff0)
  (let ((global-type 'O))
    (declare (special global-type))
    (getwff-subtype 'gwff-p gwff0)))

(defun getwff-subtype (subtype rwff)
  (let ((temp (getrwff rwff)))
    (if (funcall subtype temp) temp
	(throwfail "GWFF is not a " subtype "."))))

(defun get-only-rwff (xxx)
  (dolist (op global-gwfftypelist
	      (let ((yyy (gwff-p xxx)))
		(if yyy (return yyy) (throwfail xxx " does not refer to a wff."))))
    (when (and (symbolp op) (funcall (get op 'checkfn) xxx))
      (return (funcall (get op 'getfn) xxx)))))

(defun getrwff (xxx)
  (let ((yyy (get-only-rwff xxx)))
    (if (gwff-p yyy) yyy
	(if yyy (throwfail "Expression is not well formed.")
	    (throwfail "Aborted.")))))

(defun good-occ (x lis) (or (eq lis t) (member x lis)))

(defun typesym-p (typesym)
  (if (atom typesym)
      (or (get typesym 'typeconst) (get typesym 'parsed-type)
	  (get typesym 'typevar))
      (and (typesym-p (car typesym)) (typesym-p (cdr typesym)))))

;;;Fixed following function so that real-typesym is the real type symbol
;;; for which typesym may stand.  If typesym is a type variable and also
;;; has a substitution prop.,that substitution will be printed, unless it again
;;; is a type variable and has a substitution prop., etc.  Avoids the ugly
;;; printing of messages like "parsed formula not of type "G3997"."
;;; 9/17/87 DAN

;;; Following is better if the typechecking fails early in the game.
;;; Substitutes letters like D, E, ..., for gensyms which are
;;; temporary type variables during matching.
;;; DAN 11OCT89 

(defun get-real-typesym (typesym)
  (let ((nexttvarp 64))
    (declare (special nexttvarp))
    (get-real-typesym-aux typesym)))

(defun get-real-typesym-aux (typesym)
  (cond ((and (atom typesym) (gettsubst typesym))
         (do ((typesym typesym (gettsubst typesym)))
	     ((not (and (tvarp typesym) (tsubstp typesym))) typesym)))
        ((and (atom typesym) (tconstp typesym)) typesym)
        ((atom typesym) 
         (next-proposed-tvar*))
        (t (cons (get-real-typesym-aux (car typesym))
                 (get-real-typesym-aux (cdr typesym))))))

(defun next-proposed-tvar* ()
  (declare (special nexttvarp))
  (incf nexttvarp)
  (do ()
      ((or (and (not (member nexttvarp '(84 43 42 41)))
                (not (tconstp (intern (string (tps-ascii nexttvarp))))))
           (> nexttvarp 90))
       (if (> nexttvarp 90) 
           (throwfail "Ran out of typevariables.")
           (intern (string (tps-ascii nexttvarp)))))
    (incf nexttvarp)))


(defun printype (typesym)
  (let ((style 'generic)
	(real-typesym (get-real-typesym typesym)))
    (declare (special style))
    (princ "\"") (pp-typesym real-typesym) (princ "\"")))


(defcategory wffop
  (define defwffop)
  (properties
   (argtypes multiple)
   (wffargtypes (declare (ignore wffop))
		(funcall (get 'typesymlist-nil 'getfn) wffargtypes))
   (wffop-typelist (declare (ignore wffop))
		   (funcall (get 'typesymlist 'getfn) wffop-typelist))
   (argnames multiple)
   (resulttype single)
   (wffop-type typesym)
   (arghelp multiple)
   (defaultfns multiplefns)
   (mainfns multiplefns)
   (applicable-q singlefn)
   (applicable-p singlefn)
   (replaces single)
   (print-op single)
   (multiple-recursion single)
   (mhelp single))
  (scribe-one-fn
   (lambda (item)
     (maint::scribe-doc-command
      (format nil "@IndexOther(~A)" (symbol-name item))
      (get item 'argnames)
      (cdr (assoc 'wffop (get item 'mhelp))))))
  (global-list global-wffoplist)
  (mhelp-line "wff operation")
  (mhelp-fn operation-mhelp))

(defgwff-type wffop-type
  (checkfn wffop-ckfn)
  (getfn wffop-getfn)
  (mhelp "wffop arg ... arg : A wff operation applied to arguments."))

(defun wffop-ckfn (xxx)
  (and (consp xxx)(symbolp (car xxx)) (get (car xxx) 'wffop)))

(defun wffop-getfn (xxx)
  (opdecode xxx))

; The following code is used by the typesubst inference rule.
;  -- cebrown

; typevar-in takes a typevar "a" and a wff "inwff"
; and checks if the typevar occurs in the wff.
(defun typevar-in (a inwff)
  (cond ((label-q inwff) (apply-label inwff (typevar-in a inwff)))
	((lsymbol-q inwff) (typevar-in-typesym a (type inwff)))
	((boundwff-q inwff) (or (typevar-in-typesym a (type (caar inwff)))
				(typevar-in a (cdr inwff))))
	(t (or (typevar-in a (car inwff))
	       (typevar-in a (cdr inwff))))))

; typevar-in-typesym takes a typevar "a" and a type "tp"
; and checks if the typevar occurs in the type.
(defun typevar-in-typesym (a tp)
  (if (consp tp)
      (or (typevar-in-typesym a (car tp))
	  (typevar-in-typesym a (cdr tp)))
    (eq a tp)))

; not-typevar-in-wffset takes a typevar "a" and a set [list]
; of wffs "wffset" and checks if the typevar occurs in *any*
; of the wffs in the set.  This is used to check that an application
; of the typesubst rule is legal.
(defun not-typevar-in-wffset (a wffset)
  (dolist (wff wffset t)
	  (when (typevar-in a wff) (return nil))))

; typevar-p checks if its input is a typevar.  Notice
; I consider any symbol other than typeconst's (e.g., O and I)
; and typeabbrev's (e.g., S) to be a typevar.  I could
; also check if the symbol has the property "parsed-type"
; (other places in the code check if a symbol is a typevar
;  by checking this property).
(defun typevar-p (tp)
  (and (symbolp tp)
       (not (get tp 'typeconst))
       (not (get tp 'typeabbrev))))

