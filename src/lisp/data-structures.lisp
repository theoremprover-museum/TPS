;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;(part-of mating-search)
;(part-of ms88)
(context ms88)

(deffile data-structures
;  (part-of ms88)
  (part-of mating)
  (extension lisp)
  (mhelp "Defines data structures associated with mating search MS88."))

;;;UTREE can be:
;;;1. node 2.dpair list (for first order case) 3. NIL

(defstruct (mating (:print-function print-mating))
  (name (intern (create-namestring mating-name) (find-package "CL-USER")))
  (clist nil :type list)
  (utree nil)
  (utree-depth max-utree-depth)
  (success-nodes nil)
  (bktrack 0)
  (chain nil)
  (completep nil)
  (stats (make-stats))
  (first-path nil)
  (last-path nil)
  subst-hash-table
  (h-var-counter initial-value-h-var-counter))

(defun print-mating (mating *output-stream* level)
  (declare (ignore level))
  (let ((*standard-output* *output-stream*))
    (terpri)
    (princ "Name:  ")
    (princ (symbol-name (mating-name mating)))
    (princ "   ")
    (princ (mating-utree-depth mating))
    (princ "   ")
    (princ (mating-bktrack mating))
    (terpri) (princ "Connections:   ") (princ (mating-clist mating))
    (dolist (node (mating-success-nodes mating))
	    (princ (node-print-name node)) (princ " "))
    (mating-completep mating)))

(defflag prim-quantifier
  (flagtype boolean)
  (default t)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag) (special prim-hashtable))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (subjects primsubs mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "When NIL, primitive substitutions containing new quantifiers will
not be applied."))

(defflag mate-ffpair
  (flagtype boolean)
  (default nil)
  (subjects mating-search ms88 ms89 ms91-6 transmit)
  (mhelp "Controls whether to consider a pair of literals with flexible
heads as a potential connection. The MS controller will locally modify
it under certain conditions; in particular, it will always be set locally
to T in the following cases, among others:
a) for first-order problems (when FIRST-ORDER-MODE-MS is T).
b) when a mating is removed because it is incompatible with the etree.
c) when using the interactive command ADD-CONN."))

(defflag search-complete-paths
  (flagtype boolean)
  (default nil)
  (subjects mating-search ms88 ms89 ms91-6 transmit)
  (mhelp "Not yet implemented.
If NIL paths are generated only to a length until a connection can be
located on it. Otherwise full paths are generated."))

(defflag occurs-check
  (flagtype boolean)
  (default t)
  (subjects mating-search ms88 ms89 ms91-6 transmit)
;;;The meaning of the old help message is quite obscure. I could not verify
;;;whether it is correct or wrong because it is beyond my understandings.
;;;The new help message I give here is based on my understanding of the code.
#+comment
  (mhelp "If T occurs check in first-order unification is postponed till a
    mating is complete. In higher-order unification this will have the effect
    of not having the rigid path check, and thus unification problems that
    may terminate with the rigid path check may possibly fail to terminate.
    At present this is relevant only when FIRST-ORDER-MODE-MS is T, since
    the rigid path check is not yet implemented.")
  (mhelp "This flag is not effective unless FIRST-ORDER-MODE-MS is T. If its
    value is T, occurs check in first-order unification is postponed till a
    mating is complete."))

(defflag prop-strategy
  (flagtype symbol)
  (default allow-duplicates)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "This flag is only used in PROPOSITIONAL proof search, which can 
    be one of (1) allow-duplicates (2) hash-table (3) pushnew
    (1) Adds CONNECTION to the mating even though it might already be in the
    mating.
    In case of (2) and (3) adds CONNECTION to the mating only if it is not
    already in the mating.
    (2) uses HASH-TABLE to determine this.
    (3) uses CLISP macro PUSHNEW to determine this."))

(definfo allow-duplicates
  (mhelp "A setting for PROP-STRATEGY.
In propositional proof search, one can add a connection to a mating even 
if it is already present."))

(definfo hash-table
  (mhelp "A setting for PROP-STRATEGY.
In propositional proof search, one can add a connection to a mating only
if it is not already present in the hash-table."))

(definfo pushnew
  (mhelp "A setting for PROP-STRATEGY.
In propositional proof search, one can add a connection to a mating only
if it is not already present according to the clisp macro PUSHNEW."))

(defflag ms-split
  (flagtype boolean)
  (default t)
  (subjects mating-search ms88 ms89 ms91-6 transmit)
  (mhelp "If T mating search attempts to split the proof."))

(defflag dup-allowed
  (flagtype boolean)
  (default t)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "If T mating search duplicates quantifiers whenever necessary."))

(defflag ms-init-path
  (flagtype boolean)
  (default nil)
  (subjects mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "If NIL MS considers the current path when a new mating is started.
Otherwise, starts from the beginning in the natural ordering on paths in a 
jform."))
