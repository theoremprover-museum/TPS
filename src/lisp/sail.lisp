;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of SAIL-WFF)

;;;
;;; File: SAIL
;;; Package: SAIL-WFF
;;;
;;; defines the style SAIL for Parsing and Printing.
;;;

(part-of sail-wff)

(deffile sail
  (part-of sail-wff)
  (extension lsp)
  (mhelp "Defines SAIL style printing and parsing."))

;*;(defcontext sail-chars
;*;  (short-id "SAIL characters")
;*;  (order 33)
;*;  (mhelp "Related to the special characters in the SAIL character set."))

(context sail-chars)

(defstyle sail
  (print-symbol pp-symbol-sail)
  (print-space-p pp-space-p-sail)
  (terpri-heuristics terpri-heuristics-generic)
  (print-typesym pp-typesym-generic)
  (print-type-char pprinc)
  (print-indent indent-generic)
  (display-prefix terpri)
  (display-postfix terpri)
  (text-prefix noop)
  (text-postfix noop)
  (mhelp "SAIL stands for a file (or terminal) with SAIL characters."))

(defun pp-symbol-sail (symbol)
  (declare (special sailcharacters))
  (cond ((eq symbol 'exists1)
	 (pp-symbol-sail 'exists)
	 (pprinc '!))
	;;((get symbol 'sfont)
	;; (pptyo (get symbol 'sfont)))
	((assoc symbol sailcharacters)
	 (pptyo (cdr (assoc symbol sailcharacters))))
	(t (pprinc symbol))))

(defun pp-space-p-sail (symbol pc)
  (pp-space-p symbol pc (memq pc '(lambda forall exists exists1))))


(defvar sailcharacters
  '((forall . 20)
    (exists . 21)
    (or . 118)
    (not . 126)
    (south . 1)
    (alpha . 2)
    (beta . 3)
    (and . 4)
    (neg . 5)
    (epsilon . 6)
    (pi . 7)
    (lambda . 8)
    (infinity . 14)
    (delta . 15)
    (propersubset . 16)
    (impliedby . 16)
    (propersuperset . 17)
    (implies . 17)
    (intersect . 18)
    (union . 19)
    (tensor . 22)
    (iff1 . 23)
    (implied1 . 24)
    (imp1 . 25)
    (lesseq . 28)
    (equiv . 30)))

