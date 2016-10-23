;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
;(part-of ms89)

;;; Written by Dan Nesmith

(deffile ms90-9
  (part-of ms90-9)
  (extension lisp)
  (mhelp "Contains mateops for using option tree search procedure with
path-focused duplication."))

(context ms90-9)

(defmateop ms90-9
  (mate-result-> ignore)
  (mate-alias ms90-9-real) ;was auto-search-ms90-9
  (mhelp "Begin mating search MS90-9 on the current expansion proof.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS90-3 is used.  The flags
MAX-SEARCH-LIMIT, SEARCH-TIME-LIMIT, and RANK-EPROOF-FN are used to
control the search.  See also the command SHOW-OPTION-TREE."))

(defun ms90-9-real ()
  (let ((default-ms 'ms90-9))
    (matingsearch-controller)))

(defun auto-search-ms90-9 (&rest ignore)
  (declare (ignore ignore))
  (let ((default-ms 'ms90-9))
    (auto-search-aux)))

