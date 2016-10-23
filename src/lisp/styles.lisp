;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-PRINT)

;;;
;;; File: STYLES
;;; Package: WFF-PRINTING
;;;
;;; defines objects necessary for defining styles and style GENERIC.
;;;

(deffile styles
  (part-of wff-print)
  (extension clisp)
  (mhelp "Defines GENERIC-STRING style and some functions used
for printing wffs in GENERIC and GENERIC-STRING styles."))

(context wff-printing)

(defstyle generic-string
  (print-symbol pp-symbol-generic)
  (print-space-p pp-space-p-generic)
  (terpri-heuristics terpri-heuristics-generic)
  (print-typesym pp-typesym-generic)
  (print-type-char pprinc)
  (print-indent indent-generic)
  (display-prefix terpri-doublequotes)
  (display-postfix doublequotes-terpri)
  (text-prefix doublequotes)
  (text-postfix doublequotes)
  (mhelp "GENERIC-STRING stands for re-readable string format.
It is used in conjunction with the RE-READ mode."))


(defun doublequotes () (incf curpos) (princ "\""))

(defun terpri-doublequotes () (terpri) (setq curpos 0) (doublequotes))

(defun doublequotes-terpri () (doublequotes) (setq curpos 0)(terpri))

(defun pp-symbol-generic (symbol)
  (let ((*print-case* :upcase))
    (pprinc (case symbol
	      (not "~")
	      (assert "!")
	      (T symbol)))))

(defun pp-space-p-generic (symbol pc)
  (pp-space-p symbol pc nil))

(defun terpri-heuristics-generic (symbol)
  (when (and rightmargin (> curpos (- rightmargin 31))
	     ;; Flatsizec ->Flatc 9/18/87 DAN
	     (> (+ (flatc symbol) curpos -1) rightmargin))
    (setq curpos 0)
    (terpri)))

(defun pp-typesym-generic (tp)
  (pprinc "(")
  (pp-typesym tp)
  (pprinc ")"))

(defun indent-generic (indent)
  (when (<= curpos indent)
    (spaces (- indent curpos))
    (setq curpos indent)))

(defmode re-read
  (flag-settings
   (print-meta nil)
   (atomvalflag nil)
   (displaywff nil)
   (first-order-print-mode nil)
   (leftmargin 1)
   (ppwfflag t)
   (printdepth 0)
   (printtypes t)
   (printtypes-all t)
   (rightmargin 78)
   (scope nil)
   (style generic-string))
  (mhelp "Used when writing out wffs to a file
in such a way that they may be read back in and parsed correctly
in higher-order mode."))

