;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(deffile mtree-datastructure
  (part-of expansion-tree)
  (extension lsp)
  (mhelp "Defines data structures used by matingstree."))

(defvar obligation-name 'ob)
(defvar current-obligation nil)
(defvar current-jform nil)

(defflag matingstree-name
  (flagtype symbol)
  (default mstree)
  (subjects mtree-top etrees)
  (mhelp "Prefix for labels associated with nodes in a matingstree."))

(defstruct (obligation (:print-function (lambda (x y z) (declare (ignore y z)) 
						  (princ (obligation-name x)))))
  (name (intern (create-namestring obligation-name) (find-package "CL-USER")))
  (last nil :type (or null obligation))
  (next nil :type list)
  (closed nil)
  (jform nil)
  (from-expanding nil)
  (used-univs nil :type list)
  (disjunction nil :type (or null disjunction))
  (eligible-list nil :type list)
  (other nil :type list))

(defstruct (matingstree (:print-function (lambda (x y z) (declare (ignore y z)) 
					   (princ (matingstree-name x)))))
  (name (intern (create-namestring matingstree-name) (find-package "CL-USER")))
  (parent nil :type (or null matingstree))
  (sons nil :type list)
  (literal-pair nil)
  (merged nil)
  (mating nil)
  (dead nil)
  (obligation nil :type (or null obligation)))

(defvar already-added-clists nil)
