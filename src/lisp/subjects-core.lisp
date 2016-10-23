;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :core)
(part-of tpsdef)

(deffile subjects-core
  (part-of tpsdef)
  (mhelp "Defines subjects used in the CORE package."))

(context subtoplevels)
(defsubject editor
  (mhelp "Flags concerning the operation of the wff editor."))

(context tps-events)
(defsubject events
  (mhelp "Dealing with EVENTS."))

(context flavor-obj)
(defsubject internal-names
  (mhelp "Choice of names for flavors of internal labels."))

(context library)
(defsubject library
  (mhelp "About the library facility."))

(context otl-object)
(defsubject outline
  (mhelp "Flags having to do with outline manipulations."))

(context suggestions)
(defsubject suggests
  (mhelp "About SUGGESTIONS and GO."))

(context otl-object)
(defsubject otl-vars
  (mhelp "Variables needed by the otlnl (outline) package."))

(context save-work-obj)
(defsubject saving-work
  (mhelp "About saving and restoring work."))

(context wff-parsing)
(defsubject parsing
  (mhelp "About parsing wffs."))

(context wff-printing)
(defsubject printing
  (mhelp "About printing wffs."))

(defsubject printing-tex
  (mhelp "About formatting TeX output."))

(defsubject window-props
  (mhelp "Properties of windows (e.g., editor, proof windows, vpform windows)."))

(context prim-obj)
(defsubject wff-prims
  (mhelp "Flags for wff primitives, not related to parsing or printing."))

