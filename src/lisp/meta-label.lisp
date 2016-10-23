;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of METAWFFS)

;;;
;;; File: META-LABEL
;;; Package: metawffs
;;;
;;; defines some flavors of labels which are used inside the rules package.
;;;

(part-of metawffs)

(context flavor-obj)

(deffile meta-label
  (part-of metawffs)
  (extension lsp)
  (mhelp "Defines some flavors of labels which are used inside the rules package."))

(defflag print-meta
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If T, meta labels are printed, otherwise the wffop they represent
will be printed."))

(defflag meta-label-name
  (flagtype symbol)
  (subjects internal-names)
  (default ml)
  (mhelp "The prefix for names of meta labels (from wffops)."))

(defflavor meta
  (mhelp "A label created by the parser when it finds a meta-wff inside
a wff.")
  ;; For creating meta-wffs for the rules package, binding and substituting
  (make-wffschema1 make-meta-label-wffschema)
  (match-bind meta-label-match-bind)
  (meta-subst1 meta-label-subst)
  ;; Printing Wffops
  (printwff (lambda (wff brackets depth)
	      (declare (ignore brackets))
	      (if print-meta (pp-symbol-space wff)
		  (if ppvirtflag (throwfail "Cannot pretty print wffop.")
		      (if (get wff 'meta-wffop)
			  (progn (pp-symbol-space '|`|)
				 (pprinc "(")
				 (pprinc (get wff 'meta-wffop))
				 (dolist (arg (get wff 'meta-args))
				   (pprinc " ")
;*;				   (printwffscope args nil (1+ depth))
				   (printwffscope arg nil (1+ depth)))  ;;; Corrected 7/21/87 DAN
				 (pprinc ")"))
			  (progn (pp-symbol-space '|`|)
				 (pprinc (get wff 'lisp-token))))))))
  (prt-symbol-p (lambda (gwff)(declare (ignore gwff)) t))
  (prt-infix-op (lambda (gwff)(declare (ignore gwff)) nil))
  (prt-prefix-op (lambda (gwff)(declare (ignore gwff)) nil))
  (prt-associative-p (lambda (gwff)(declare (ignore gwff)) nil))
  ;; Misc required operations
  (type (lambda (gwff) (get gwff 'meta-type)))
  (gwff-p (lambda (gwff) (declare (ignore gwff)) t))
  (gwff-q (lambda (gwff)(declare (ignore gwff)) t))
  (legal-type-p1 (lambda (gwff) (get gwff 'meta-type)))
  ;; default for test operations does not exists.
  ;; Stepping operations follow
  (ntharg (lambda (n gwff)
	    (if (zerop n) (throwfail "0 is not legal here!")
		(let ((meta-args (nth (- n 1) (get gwff 'meta-args))))
		  (if (and (symbolp (car meta-args))
			   (meta-label-q (car meta-args))
			   (not (get (car meta-args) 'meta-type)))
		      (throwfail "Argument number " n " is not a gwff!")
		      meta-args)))))  ;;ntharg
  ;;rplaca in the next expression does not have the right number of args.
  ;;replace-ntharg doesn't seem to be defined anywhere, and hence I am
  ;;commenting it out. -si 12 Feb 1989
  ;;(replace-ntharg (lambda (gwff)
	;;	    (rplaca (nthcdr (- n 1) (get gwff 'meta-args)))))
  )

;*;			   (do ((args (get wff 'meta-args) (cdr args))
;*;				(aplicnlist
;*;				 '(((nil 0) . 0) . ((nil 0) . 0))
;*;				 (cons '((nil 0) . 0)
;*;				       (printwffscope
;*;					(car args) nil (+i depth)))))
;*;			       ((null args) aplicnlist))

(defun generate-wffop-label (meta-label label-type)
  (let ((label-name (create-name meta-label-name)))
    (putprop label-name 'meta 'flavor)
    (putprop label-name (get meta-label 'meta-label-wffop) 'meta-wffop)
    (putprop label-name (get meta-label 'lexlist) 'meta-args)
    (putprop label-name (get meta-label 'lisp-token) 'lisp-token)
    (putprop label-name label-type 'meta-type)
    label-name))

(defmacro copy-props (from to &rest props)
  (do ((props props (cdr props))
       (result-form (list 'progn)
		    (cons `(putprop ,to (get ,from ,(car props)) ,(car props))
			  result-form)))
      ((null props) (nreverse result-form))))

(defun rename-wffop-label (meta-label)
  (let ((label-name (create-name meta-label-name)))
    (copy-props meta-label label-name
		'flavor 'meta-wffop 'meta-args 'lisp-token 'meta-type)
    label-name))

(context moving)

(defwffop ntharg
  (argtypes integer+ gwff)
  (resulttype gwff)
  (argnames n gwff)
  (arghelp "Number of argument (0 means the function)")
  (applicable-p (lambda (n gwff) (declare (ignore n)) (or (label-q gwff) (not (lsymbol-q gwff)))))
  (mhelp "Move to the nth argument of a functional application,
or to the nth disjunct, conjunct, etc."))

(defun ntharg (n gwff)
  (cond ((label-q gwff) (apply-label gwff (ntharg n gwff)))
	((lsymbol-q gwff)
	 (throwfail "Can't apply NTHARG to a logical symbol."))
	((boundwff-q gwff)
	 (do ((i 0 (+ i 1))
	      (gwff gwff (gdr gwff)))
	     ((= i n) gwff)
	   (when (not (bd-wff-p gwff))
		 (throwfail "Only " i " consecutive binders."))))
	((infix-p gwff)
	 (nth-infix-arg n gwff nil))
	(t (nth-prefix-arg n gwff nil))))

(defwffrec nth-prefix-arg
  (argnames n gwff stack))

(defun nth-prefix-arg (n gwff stack)
  (cond ((label-q (car gwff)) (apply-label gwff (nth-prefix-arg n gwff stack)))
	((or (lsymbol-q (car gwff)) (boundwff-q (car gwff)))
	 (case n
	   (0 (car gwff))
	   (1 (cdr gwff))
	   (t (if (> n (+ (length stack) 1))
		  (throwfail "Only " (+ (length stack) 1) " arguments.")
		  (nth (- n 2) stack)))))
	(t (nth-prefix-arg n (car gwff) (cons (cdr gwff) stack)))))

(defun nth-infix-arg (n gwff stack)
  (declare (ignore n gwff stack))
  (throwfail "NTHARG is not yet implemented for infix operators."))
