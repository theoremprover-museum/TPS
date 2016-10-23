;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of mode-ml)

;;;
;;; File: ml-mode
;;;

(deffile ml-mode
  (part-of mode-ml)
  (extension clisp)
  (mhelp "Defines ML mode for printing and parsing of wffs."))

(defmode ml
  (flag-settings
   (first-order-mode-parse nil)
   (first-order-print-mode nil)
   (type-iota-mode t)
   (base-type i)
   (printtypes t))
  (mhelp "Puts parser and printer into higher-order mode for
Lisp package ML."))
