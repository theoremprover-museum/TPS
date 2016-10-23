;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of UNIFICATION-interface)
(context unification)
;;;
;;; File: UNIF-macros
;;; Package: UNIFICATION-interface
;;;
;;; defines the unifop category
;;;

(deffile unif-macros
  (part-of unification-interface)
  (extension lisp)
  (mhelp "Contents define unifop category."))

(context unification)

(deftoplevel unif-top
  (top-prompt-fn unif-top-prompt)
  (command-interpreter unif-command-interpreter)
  (print-* unif-print-*)
  (top-level-category unifop)
  (top-level-ctree unif-command-ctree)
  (top-cmd-decode unif-opdecode)
  (mhelp "The top level of unification search."))

(defcategory unifop
  (define defunifop)
  (properties
    (unif-argtypes multiple)
    (unif-argnames multiple)
    (unif-arghelp multiple)
    (unif-defaultfns multiplefns)
    (unif-applicablep singlefn)
    (unif-mainfns singlefn)
    (print-command single)
    (move-command single)
    (mhelp single))
  (global-list global-unifoplist)
  (shadow t)
  (mhelp-line "unification command")
  (scribe-one-fn
    (lambda (item)
      (maint::scribe-doc-command 
	(format nil "@IndexOther(~A)" (symbol-name item))
	(get item 'unif-argnames)
	(cdr (assoc 'unifop (get item 'mhelp))))))
  (mhelp-fn unifcmd-mhelp))

(defun unifcmd-mhelp (keyword category)
  (declare (special short-help))
  (princ-mhelp keyword category)
  (if (get keyword 'print-command) (msgf keyword " is a printing command.")
    (if (get keyword 'move-command) (msgf keyword " is a unification tree moving command.")
    (unless short-help
      (when *doing-html* (msg " fnord "))
      (msgf "The command format for " keyword " is:" -2 "<Unif>" keyword)
      (print-tps-format* keyword " "
			 (+ 5 (length (format nil "~A" keyword)))
			 (get keyword 'unif-argnames)
			 (get keyword 'unif-argtypes)
			 nil)))))
