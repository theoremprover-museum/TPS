;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of LOGIC-SCRIBE)

;;;
;;; File: SCRIBE
;;; Package: SCRIBE-WFF
;;;
;;; defines functions necessary for printing in style SCRIBE.
;;;

(deffile scribe
  (part-of logic-scribe)
  (extension clisp)
  (mhelp "Establishes SCRIBE style printing."))

(context wff-printing)

(defstyle scribe
  (print-symbol pp-symbol-scribe)
  (print-space-p pp-space-p-scribe)
  (terpri-heuristics terpri-heuristics-scribe)
  (print-typesym pp-typesym-scribe)
  (print-type-char pp-lowercase)
  (print-indent indent-generic) ;;indent-scribe
  (print-tab tab-scribe)
  (print-nextpar nextpar-scribe)
  (margin-correct margin-scribe)
  (display-prefix display-prefix-scribe)
  (display-postfix display-postfix-scribe)
  (begin-environment begin-env-scribe)
  (end-environment end-env-scribe)
  (text-prefix noop)
  (text-postfix noop)
  (print-line print-line-scribe)
  (char-cat scribe-char)
  (mhelp "SCRIBE stands for a file to be processed by SCRIBE before printing."))

(defun pp-symbol-scribe (symbol)
  (cond ((stringp symbol) (pprinc symbol))
	((get symbol 'dfont)
	 (pp-scribe-special-symbol symbol))
	((get symbol 'face)
	 (mapc #'pp-scribe-special-symbol (get symbol 'face)))
	(t (pprinc symbol))))

(defun pp-scribe-special-symbol (symbol)
  (let ((sp-sym (and (symbolp symbol) (get symbol 'dfont))))
    (if sp-sym
	(progn
	  (pprinc "@")
	  (pprinc0 sp-sym)
	  (pprinc0 "@;"))
      (pprinc symbol))))

(defun pp-space-p-scribe (symbol pc)
  (declare (special turnstile-indent-auto))
  (if (eq turnstile-indent-auto 'compress)
    (pp-space-p-scribe-compressed symbol pc
			   (and (binder-q pc) (or (get pc 'dfont) (get pc 'face))))
    (pp-space-p symbol pc
		(and (binder-q pc) (or (get pc 'dfont) (get pc 'face))))))

(defun pp-space-p-scribe-compressed (symbol pc no-space-p)
  (cond ((member pc '(" " "[" "." "~" "}" "{") :test #'string=) nil)
	((member symbol '("." " " "[" "{" "}" "]") :test #'string=) nil)
	(t (not no-space-p))))

(defun terpri-heuristics-scribe (symbol)
  (declare (ignore symbol))
  (when (> curpos 60) (setq curpos 0) (msg "@~") (terpri) (msg "@;")))

(defun pp-typesym-scribe (tp)
  (pprinc0 "@F12{")
  (pp-typesym tp)
  (pprinc0 "}"))

(defun indent-scribe (indent)
  (msg (tx indent)))

(defun nextpar-scribe () (msg f) (terpri))

(defun margin-scribe (item) (msg item " "))

(defun display-prefix-scribe ()
  (if ppwfflag (msg "@Begin(Verbatim, Spacing=1.5)" t)
      (msg "@Begin(Text, Spacing=1.5)" t)))

(defun display-postfix-scribe ()
  (if ppwfflag (msg "@End(Verbatim)" t)
      (msg "@end(Text)" t)))

(defun begin-env-scribe (env)
  (msgf "@Begin(" env ")" t))

(defun end-env-scribe (env)
  (msgf "@End(" env ")" t))

(defcategory scribe-char
  (define defscribefont)
  (properties
   (dfont single))
  (global-list global-scribecharlist)
  (mhelp-line "scribe special character")
  (mhelp-fn princ-mhelp-char))

(defun tab-scribe () (msg "@\\"))
