;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :maint)
(part-of tpsdef)

(deffile subjects-maint
  (part-of tpsdef)
  (mhelp "Defines subjects used in the MAINT package."))

(context tps-maintenance)
(defsubject maintain
  (mhelp "Flags useful for system maintainers"))
(defsubject system
  (mhelp "Flags containing system constants."))

(context rules-object)
(defsubject rules-mod
  (mhelp "Flags having to do with the operation of the rules module."))



