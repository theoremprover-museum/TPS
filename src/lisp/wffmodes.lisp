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
;;; File: wffmodes
;;;

(part-of wffs)

(deffile wffmodes
  (part-of wffs)
  (extension lisp)
  (mhelp "Defines some modes for printing and parsing of wffs."))

(defmode first-order
  (flag-settings
   (first-order-mode-parse t)
   (type-iota-mode t)
   (first-order-print-mode t)
   (printtypes nil))
  (mhelp "Puts parser and printer into first-order mode."))


(defmode higher-order
  (flag-settings
   (first-order-mode-parse nil)
   (first-order-print-mode nil)
   (printtypes t))
  (mhelp "Puts parser and printer into higher-order mode."))
