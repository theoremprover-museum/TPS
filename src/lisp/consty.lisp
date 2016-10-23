;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of CONCEPT-WFF)

;;;
;;; File CONSTY
;;; Package: CONCEPT-WFF
;;;
;;; defines CONCEPT and CONCEPT-S device styles.
;;;

(part-of concept-wff)

(deffile consty
  (part-of concept-wff)
  (extension lsp)
  (mhelp "Defines CONCEPT and CONCEPT-S device styles."))

(context concept-terminal)

(defstyle concept
  (print-symbol pp-symbol-generic)
  (print-space-p pp-space-p-generic)
  (terpri-heuristics terpri-heuristics-concept)
  (print-typesym pp-typesym-generic)
  (print-type-char pprinc)
  (print-indent indent-generic)
  (print-line print-line-generic)  
  (print-tab tab-generic)
  (print-nextpar nextpar-generic)
  (margin-correct margin-generic-tps)
  (begin-environment noop-1)
  (end-environment noop-1)
  (display-prefix terpri)
  (display-postfix terpri)
  (text-prefix noop)
  (text-postfix noop)
  (mhelp "CONCEPT stands for any terminal without special characters."))


(defstyle concept-s
  (print-symbol pp-symbol-concept-s)
  (print-space-p pp-space-p-concept-s)
  (terpri-heuristics terpri-heuristics-generic)
  (print-typesym pp-typesym-concept-s)
  (print-type-char pp-lowercase)
  (print-indent indent-generic)
  (print-line print-line-generic)
  (print-tab tab-generic)
  (print-nextpar nextpar-generic)
  (margin-correct margin-generic-tps)
  (begin-environment noop-1)
  (end-environment noop-1)
  (display-prefix terpri)  
  (display-postfix terpri)
  (text-prefix noop)
  (text-postfix noop)
  (char-cat concept-char)
  (mhelp "CONCEPT-S stands for any CONCEPT terminal with special characters."))

(defun pp-symbol-concept-s (symbol)
  (let ((*print-case* :upcase))
    (cond ((stringp symbol) (pprinc symbol))
	  ((get symbol 'cfont)
	   (pp-concept-special-symbol symbol)
	   (pp-enter-kset 0))
	  ((get symbol 'face)
	   (mapc #'pp-concept-special-symbol (get symbol 'face))
	   (pp-enter-kset 0))
	  (t (pprinc symbol)))))

(defun pp-concept-special-symbol (symbol)
  (let ((*print-case* :upcase)
	(cfont (and (symbolp symbol) (get symbol 'cfont))))
    (if cfont
	(progn (pp-enter-kset (car cfont))
	       (if (or (< (cdr cfont) 32) (= (cdr cfont) 127))
		   (pptyos (cdr cfont))
		   (pptyo (cdr cfont))))
	(progn (pp-enter-kset 0) (pprinc symbol)))))

(defun pp-space-p-concept-s (symbol pc)
  (pp-space-p symbol pc
	      (and (binder-q pc) (or (get pc 'cfont) (get pc 'face)))))

(defun pp-typesym-concept-s (tp)
  (pp-enter-kset 1)
  (pp-typesym tp)
  (pp-enter-kset 0))

;*;(defun terpri-heuristics-concept (symbol)
;*;  (let ((ppvirtflag T)
;*;	(ppwfflength 0))
;*;    (cond ((not (memq symbol '(/. /  /[ /])))
;*;	   (pp-lsymbol symbol))
;*;	  (T (setq ppwfflength 1)))
;*;    (cond ((and rightmargin
;*;		(> curpos (- rightmargin 31))
;*;		(> (+  ppwfflength curpos -1) rightmargin))
;*;	   (terpri) T)))) 


(defun terpri-heuristics-concept (symbol)
  (let ((ppvirtflag T)
	(ppwfflength 0))
    (if (member symbol '("." " " "[" "]") :test #'string=)
	(setq ppwfflength 1)
	(pp-lsymbol symbol))
    (when (and rightmargin
	       (> curpos (- rightmargin 31))
	       (> (+  ppwfflength curpos -1) rightmargin))
      (terpri)
      T)))

;;;
;;; The following defines special concept characters
;;;


(context wff-parsing)

(defun init-cfonttable ()
  (make-array '(4 128)))

(defvar cfonttable (init-cfonttable))

(defcategory concept-char
  (define defcfont)
  (properties
   (cfont (setf (aref cfonttable (car cfont) (cadr cfont)) concept-char)
	  (cons (car cfont) (cadr cfont)))
   (end-symbol boolean)
   (mhelp single))
  (global-list global-cfontlist)
  (mhelp-line "concept special character")
  (mhelp-fn princ-mhelp-char))



(init-cfonttable)

