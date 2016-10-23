;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :auto)
(part-of ms89)

;;; Written by Dan Nesmith

(deffile option-tree-macros
  (part-of ms89)
  (extension lisp)
  (mhelp "Defines option trees."))


(defstruct (option-tree (:print-function print-option-tree))
  (eproof nil)
  (children nil)
  (parent nil)
  (ranking 1)
  (back nil)
  (forward nil)
  (free-vars nil)
  (name (gentemp "OPT"))
  (tried 0))

;;; want following true when using ms90-3 as search procedure.  
;;; It will handle all duplications. We will make the nodes anyway,
;;; so that we can apply primsubs to new variables, but don't want
;;; to ever search them. 

(defvar *ignore-first-order-dups* nil)
(defvar *option-tree-ms* nil)

