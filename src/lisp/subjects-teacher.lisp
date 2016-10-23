;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :teacher)
(part-of tpsdef)

(deffile subjects-teacher
  (part-of tpsdef)
  (mhelp "Defines subjects used in the GRADER package."))

(context grader-object)
(defsubject gr-filenames
  (mhelp "Files used by the grading package."))

(defsubject gr-misc
  (mhelp "Miscellaneous variables associated with the grading package."))

