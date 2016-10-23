;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of TPS2-RULEP)

(deffile rulep-edops
  (part-of tps2-rulep)
  (extension clisp)
  (mhelp "Defines WFFOPs and EDOPs for validity testing."))

(context rulep-test)

(defwffop valid-p
  (argtypes jform)
  (argnames jform)
  (resulttype boolean)
  (arghelp "Quantifier-Free Wff")
  (mhelp "Check whether a propositional wff is valid."))

(defedop valid
  (alias valid-p)
  (result-> (lambda (boolean)
	      (if boolean (msg "Wff is valid.") (msg "Wff is not valid."))))
  (edwff-argname jform))

(defwffop sat-p
  (argtypes jform)
  (argnames jform)
  (resulttype boolean)
  (arghelp "Quantifier-Free Wff")
  (mhelp "Check whether a propositional wff is satisfiable."))

(defedop sat
  (alias sat-p)
  (result-> (lambda (boolean)
	      (if boolean (msg "Wff is satisfiable.")
		  (msg "Wff is not satisfiable."))))
  (edwff-argname jform))

(defun sat-p (jform)
  (cond ((jform-p jform)(setq jform (jform-to-gwff jform))))
  (sat-p-main (local-gwff-to-jform jform t)))

