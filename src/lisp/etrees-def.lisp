;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of expansion-tree)

(deffile etrees-def
  (part-of expansion-tree)
  (extension clisp)
  (mhelp "Expansion tree macro file."))

(context expansion-trees)

;;; Note that ETREE-P is automatically defined for the structure  ETREE,
;;; but doesn't really check inside the tree.

(defun etree-q (wff)
  (etree-p wff))


(defgwff-type etrees-labels
  (checkfn etree-q)
  (getfn etrees-labels-getfn)
  (mhelp "Labels used in expansion trees."))

(defun etrees-labels-getfn (label)
  (if (etree-q label) label))

