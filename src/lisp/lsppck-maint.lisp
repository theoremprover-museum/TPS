;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :maint)

(deffile lsppck-maint
  (part-of BARE)
  (extension lisp)
  (mhelp "Functions in the MAINT package to do with lisp packages."))

(context lisp-packages)

(defmexpr use
  (argnames lisp-package)
  (argtypes lisp-package)
  (arghelp "LISP Package")
  (mainfns use-one)
  (mhelp "Make a Lisp package accessible in the current Lisp package.
An error will be issued by Lisp if this leads to name conflicts."))


(defmexpr pack-stat
  (mainfns pack-status)
  (mhelp "Give information about the current status of the Lisp
package structure."))
