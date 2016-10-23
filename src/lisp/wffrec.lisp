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
;;; File: wffrec
;;;

(part-of wffs)

(deffile wffrec
  (part-of wffs)
  (extension lsp)
  (mhelp "Defines some recursion macros for operations on wffs."))


(defmacro boolean-recursion-escape (wffop wffarg flagarg restargs escape)
  `(cond ((not-p ,wffarg)
	  (cons 'not (,wffop (gdr ,wffarg) (not ,flagarg) ,@restargs)))
	 ((or (and-p ,wffarg) (or-p ,wffarg))
	  (cons (cons (gar (gar ,wffarg))
		      (,wffop (gdr (gar ,wffarg)) ,flagarg ,@restargs))
		(,wffop (gdr ,wffarg) ,flagarg ,@restargs)))
	 ((implies-p ,wffarg)
	  (cons (cons (gar (gar ,wffarg))
		      (,wffop (gdr (gar ,wffarg))
			      (not ,flagarg) ,@restargs))
		(,wffop (gdr ,wffarg) ,flagarg ,@restargs)))
	 (t ,escape)))


(defmacro boolean-recursion (wffop wffarg flagarg)
  `(cond ((not-p ,wffarg)
	  (cons 'not (,wffop (gdr ,wffarg) (not ,flagarg))))
	 ((or (and-p ,wffarg) (or-p ,wffarg))
	  (cons (cons (gar (gar ,wffarg))
		      (,wffop (gdr (gar ,wffarg)) ,flagarg))
		(,wffop (gdr ,wffarg) ,flagarg)))
	 ((implies-p ,wffarg)
	  (cons (cons (gar (gar ,wffarg))
		      (,wffop (gdr (gar ,wffarg))
			      (not ,flagarg)))
		(,wffop (gdr ,wffarg) ,flagarg)))
	 (t ,wffarg)))

(defun essexist (binder univflag)
  (or (and univflag (eq (cdr binder) 'forall))
      (and (not univflag) (eq (cdr binder) 'exists))))

(defun essuniv (binder existflag)
  (or (and existflag (eq (cdr binder) 'exists))
      (and (not existflag) (eq (cdr binder) 'forall))))
