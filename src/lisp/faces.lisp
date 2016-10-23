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
;;; File: FACES
;;; Package: WFF-PRINT
;;;
;;; allows the definiton of faces.  Most useful for styles other than
;;; GENERIC.
;;;

;;; Faces can be defined explicitly with DEFFACE or through the
;;; FACE property of logical symbols.

(part-of wff-print)

(deffile faces
  (part-of wff-print)
  (extension lsp)
  (mhelp "Allows definition of printing faces."))

(context prim-obj)

(defcategory print-face
  (define defface)
  (properties
   (face (putprop (collapse-face face) print-face 'short-name)
	 face)
   (mhelp single))
  (global-list global-facelist)
  (mhelp-line "face")
  (mhelp-fn princ-mhelp))


(defprintprop face
  (printproptype symbollist)
  (readfn (lambda (prop-name face log-symbol)
	    (declare (ignore prop-name))
	    (putprop (collapse-face face) log-symbol 'short-name)
	    face))
  (mhelp "

The face of a logical symbol, identical for all devices.  This may be
a list of symbols to be concatenated.  If left undefined in an
abbreviation, TPS will attempt to find a symbol in the current style
with the same name as the abbreviation.

The list of symbols can include symbols such as X, %, + or even | |
for an empty space, or the name of a special character.  In styles
which do not have a given special character, the name of the character
will be printed instead.

To see a list of names of special characters available in styles TEX and SCRIBE,
use HELP TEX-CHAR and HELP SCRIBE-CHAR.

To see a list of names of special characters available in style XTERM,
experts can evaluate the expression (mapcar 'car core::xterm-characters)
"))

(defun princ-mhelp-char (keyword category)
  (let ((mhelp (or (categ-mhelp keyword category)
		   (when (eq category 'edop)
			 (categ-mhelp (get keyword 'alias) 'wffop)))))
    (if mhelp
	(progn (if (consp mhelp) (eval (cons 'msgf mhelp))
		   (msg f mhelp))
	       (msg "  "))
	(msg f))
    (msg "Prints as " (keyword . fsym))))
