;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :maint)
(part-of tpsdef)

(deffile contexts-maint
  (part-of tpsdef)
  (extension lsp)
  (mhelp "Defines contexts used in the MAINT package."))

(defcontext coll-help
            (short-id "Collecting Help")
            (order 12)
            (mhelp "Concerning the automatic collection of help messages."))

(defcontext file-operations
            (short-id "File Utilities")
	    (order 136)
            (mhelp "Utilities dealing with files and keeping records."))

(defcontext tps-maintenance
            (short-id "Maintenance")
            (order 140)
            (mhelp "TPS-objects which help in maintaining TPS."))

;;;The package has been used in boot1.lisp before being defined.
(defcontext modules-in-tps
  (short-id "Modules")
  (order 142)
  (mhelp "TPS objects dealing with the module structure."))

(defcontext rule-run
            (short-id "Rules Module")
            (order 144)
            (mhelp
              "TPS objects useful in running the RULES module to produce
a set of commands implementing the rules of inference of a logical system."))

(defcontext rules-object
  (short-id "Rules object")
  (order 150)
  (mhelp "An object from the rules module."))
