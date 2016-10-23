;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of TPSDEF)

;;;
;;; File: Gensty
;;; Package: Tpsdef
;;;
;;; Establishes styles, defines style GENERIC and
;;; operations for style GENERIC which are independent of wffs.
;;;


(deffile gensty
  (part-of tpsdef)
  (extension clisp)
  (mhelp "Establishes styles, defines style GENERIC and
operations for style GENERIC which are independent of wffs."))

(eval-when (load compile eval)
(defcategory device-style
  (define defstyle)
  (properties
   (print-symbol singlefn)
   (print-space-p singlefn)
   (terpri-heuristics singlefn)
   (print-typesym singlefn)
   (print-type-char singlefn)
   (print-indent singlefn)
   (print-tab singlefn)
   (print-nextpar singlefn)
   (print-line singlefn)
   (margin-correct singlefn)
   (display-prefix singlefn)
   (display-postfix singlefn)
   (begin-environment singlefn)
   (end-environment singlefn)
   (text-prefix singlefn)
   (text-postfix singlefn)
   (char-cat single)
   (mhelp single))
  (global-list global-stylelist)
  (mhelp-line "style")
  (mhelp-fn princ-mhelp)))

;;;
;;;NOOP is used as the function called as prefix/postfix for some styles.
;;;

(defun noop (&rest ignore) (declare (ignore ignore)) nil)

(defun noop-1 (x)
  (declare (ignore x))
  nil)

(eval-when (load compile eval)
(defstyle generic
  (print-symbol pp-symbol-generic)
  (print-space-p pp-space-p-generic)
  (terpri-heuristics terpri-heuristics-generic)
  (print-typesym pp-typesym-generic)
  (print-type-char pprinc)
  (print-indent indent-generic)
  (print-line print-line-generic)
  (print-tab tab-generic)
  (print-nextpar nextpar-generic)
  (margin-correct margin-generic-tps)
  (begin-environment noop)
  (end-environment noop)
  (display-prefix terpri)
  (display-postfix terpri)
  (text-prefix noop)
  (text-postfix noop)
  (mhelp "GENERIC stands for any terminal without special characters."))
)

(eval-when (load compile eval)
(deftype% fsym
  (getfn identity)
  (testfn atom)
  (printfn (lambda (fsym) (declare (special style)) (pcall print-symbol fsym)))
  (mhelp "A symbol which may be printed differently depending on the style."))
)

;;; (DEFFLAG STYLE ...) comes after the definition of GENERIC, since
;;; GENERIC must be a dev-style so the default is recognized properly.
;;;Now in file GENSTY-*.

(defvar curpos 0)

(defflag alpha-lower-flag
  (flagtype boolean)
  (default nil)
  (subjects printing)
  (mhelp "If T, output from ? will be made more readable
(alphabetized, smaller left margin, mostly lower case)
If NIL, output is in the old style (non-alphabetized,
large left margin, mostly block capitals)."))

(defun nextpar-generic ()
  (declare (special leftmargin))
  (msg f) 
  (when (> leftmargin 0) (msg (e (tab leftmargin))))
  (setq curpos leftmargin))

;;CK this 
(defun tab-generic () 
  (setq curpos (if (<= curpos (curtab)) 
		   (curtab) 
		 (1+ curpos)))
  (msg (t (curtab))))

(defun curtab ()
  (declare (special leftmargin))
  (if alpha-lower-flag (+ leftmargin 5) (+ leftmargin 15)))

(defun margin-generic-tps (item)
  (declare (special rightmargin))
  (let ((string (string item)))
    (if (> (+ (length string) curpos) (- rightmargin 5))
	(progn (msg t (e (tab (curtab))))  (setq curpos (curtab))))
    (princ item)
    (setq curpos (+ (length string) curpos)))
  (if (>= curpos (- rightmargin 5)) nil (progn (incf curpos) (msg " "))))

;;; changed the follwoing macro to simply do a FUNCALL after a GET
;;; rather than expand at compile-time for greater generally,
;;; perhaps sacrificing some speed in some cases.


(defutil pcall
  (form-type macro)
  (keywords output)
  (mhelp "(PCALL operation arg1 ... argn)
is used inside functions whose output depends on the current value of
the STYLE parameter.  operation is typically something like PRINT-TYPESYM,
or BEGIN-ENVIRONMENT.  It expands in such a way that all the styles known
at compile-time are compiled in-line, but it will also work for styles
defined later, e.g. when another package is loaded.  arg1 ... argn are
handed to the function which is supposed to perform operation in the 
current style.  If an operation has not been defined for a particular
style, a THROWFAIL with an appropriate error message will be done."))

(context style)

(defflag style
  (flagtype dev-style)
  (default generic)
  (pre-change-fn (lambda (flag value old-value) ; cebrown 3/30/02
		   (declare (special *using-interface*) (ignore flag old-value))
		   (when *using-interface*
		     (unless (equal value 'istyle)
		       (throwfail "Must Use ISTYLE When Running TPS Using An Interface")))))
  (change-fn (lambda (a b c)
	       (declare (special *using-interface*))
	       (unless *using-interface* 
		 (when (eq b 'istyle)
		   (msgf "Cannot Use ISTYLE Unless Running TPS Using An Interface")
		   (set a c)))
	       (when (eq b 'concept-s) (reset))))
  (subjects printing)
  (mhelp "The style of the terminal output device."))
