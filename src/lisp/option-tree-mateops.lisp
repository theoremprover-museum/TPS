;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of ms89)

;;; Written by Dan Nesmith

(deffile option-tree-mateops
  (part-of ms89)
  (extension lisp)
  (mhelp "Contains mateops for using option tree search procedure."))

(context ms89)

(defmateop ms89
  (mate-result-> ignore)
  (mate-alias ms89-real) ;was auto-search-ms89
  (mhelp "Begin mating search MS89 on the current expansion proof.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS88 is used.  The flags
MAX-SEARCH-LIMIT, SEARCH-TIME-LIMIT, and RANK-EPROOF-FN are used to
control the search.  See also the command SHOW-OPTION-TREE."))

(defun ms89-real ()
  (let ((default-ms 'ms89))
    (matingsearch-controller)))

(defun auto-search-ms89 (&rest ignore)
  (declare (ignore ignore))
  (let ((default-ms 'ms89))
    (auto-search-aux)))

(defflag max-search-limit 
  (flagtype null-or-posinteger)
  (subjects mating-search important ms89 ms90-9 ms91-6 ms91-7 ms93-1 transmit)
  (default nil)
  (mhelp "If integer-valued, is an upper limit on the TOTAL amount of time
(in seconds) which can be spent on searching for a proof in any particular 
option.  If null, then search time is unbounded. The flag is not affected 
by Garbage Collecting time whenever the value of the flag excluding-gc-time 
is T. Please read the help message for EXCLUDING-GC-TIME for more information.")
)

(defflag search-time-limit 
  (flagtype null-or-posinteger)
  (subjects mating-search important ms89 ms90-9 ms91-6 ms91-7 ms93-1 transmit)
  (default nil)
  (mhelp "If integer-valued, is an upper limit on the CONTINUAL amount of time
(in seconds) which can be spent on searching for a proof in any particular 
option.  If null, then an ad hoc bound is used by the search procedure.
The flag is not affected by Garbage Collecting time whenever the value of the 
flag excluding-gc-time is T. Please read the help message for EXCLUDING-GC-TIME
for more information.")
)

(context miscellaneous)
(defmateop expunge-old
  (mate-result-> ignore)
  (mhelp "Frees up space by getting rid of all old expansion proofs and 
option trees. If you'd like to get rid of all(not only old) expansion
proofs and option trees, you must use EXPUNGE to do your job.
Warning : Never use EXPUNGE-OLD if you are going to use EXPUNGE, or you
cannot get the expected result!"))

(defmateop expunge
  (mate-result-> ignore)
  (mhelp "Frees up space by getting rid of all expansion proofs and option 
trees. If you only want to get rid of old expansion proofs and 
option trees, you can use EXPUNGE-OLD to do you job. 
Warning : After using EXPUNGE, many commands such as ETD, VP, ...,
don't work until you re-initialize the current expansion proof by using
commands such as SUB, MATE, ..."))


(defun auto-search-aux (&rest ignore)
  (declare (ignore ignore))
  (auto-search current-eproof))

(context wff-printing)
(defvar option-tree-root nil)

(defmateop show-option-tree
  (mate-result-> ignore)
  (mate-applicable-p (lambda (&rest ignore) (declare (ignore ignore)) (boundp 'option-tree-root)))
  (mhelp "Show the current option-tree."))

(defun show-option-tree (&rest ignore)
  (declare (ignore ignore))
  (if (option-tree-p option-tree-root)
      (print-option-tree-long option-tree-root)))
