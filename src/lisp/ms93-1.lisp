;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
;(part-of ms92-9)

;;; Written by Matt Bishop

(deffile ms93-1
  (part-of ms90-3)
  (extension lisp)
  (mhelp "Definitions, functions, etc., needed by ms93-1 and not already
          provided by ms92-9. This is basically an extension to MS92-9, which 
          is why it's in the package MS90-3."))

(context ms93-1)

(defmateop ms93-1
  (mate-result-> ignore)
  (mate-alias ms93-1-real) ;was auto-search-ms93-1
  (mhelp "Begin mating search MS93-1 on the current expansion proof.
The search is basically identical to MS89, but is performed using the 
internal variable representations of MS90-9.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS92-9 is used.  The flags
MAX-SEARCH-LIMIT, SEARCH-TIME-LIMIT, and RANK-EPROOF-FN are used to
control the search. See also the command SHOW-OPTION-TREE."))

(defun ms93-1-real ()
  (let ((default-ms 'ms93-1))
    (matingsearch-controller)))

(defvar dummy-N-O-D 99)

(defun auto-search-ms93-1 (&rest ignore)
  (declare (ignore ignore))
  (let ((default-ms 'ms93-1))
    (auto-search-aux)))


