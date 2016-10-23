;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of AUTO-DOC)

;;;
;;; File: Docdef
;;; Package: Auto-Doc
;;;
;;; macro file for the AUTO-DOC package.  Defines contexts and
;;; scribe-doc mode.
;;;

(part-of auto-doc)

(deffile docdef
  (part-of auto-doc)
  (extension lsp)
  (mhelp "Macro file for automatic documentation."))

(context coll-help)

(defmode scribe-doc
  (flag-settings
   (allscopeflag nil)
   (atomvalflag nil)
   (displaywff nil)
   (first-order-print-mode nil)
   (flushleftflag nil)
   (leftmargin 0)
   (localleftflag nil)
   (ppwfflag nil)
   (printdepth 0)
   (printtypes t)
   (rightmargin 70)
   (scope nil)
   (style scribe))
  (mhelp "Mode used for producing documentation in Scribe."))

(defmode scribe-doc-first-order
  (flag-settings
   (allscopeflag nil)
   (atomvalflag nil)
   (displaywff nil)
   (first-order-print-mode t)
   (flushleftflag nil)
   (leftmargin 0)
   (localleftflag nil)
   (ppwfflag nil)
   (printdepth 0)
   (printtypes nil)
   (rightmargin 70)
   (scope nil)
   (style scribe))
  (mhelp "Mode used for producing documentation in Scribe in first-order mode."))

;;; The following macro is to be used in for producing scribe-able
;;; documentation.

(defmacro in-auto-doc-mode (&rest forms)
  `(if first-order-print-mode
       (in-mode scribe-doc-first-order ,@forms)
       (in-mode scribe-doc ,@forms)))
