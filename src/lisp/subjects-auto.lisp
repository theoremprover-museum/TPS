;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :auto)
(part-of tpsdef)

(deffile subjects-auto
  (part-of tpsdef)
  (mhelp "Defines subjects used in the AUTO package."))

(context expansion-trees)
(defsubject etrees
  (mhelp "Variables associated with expansion trees."))

(context etr-nat)
(defsubject etr-nat
  (mhelp "Pertaining to the translation from expansion tree proofs to
natural deduction proofs."))

(context jforms1)
(defsubject jforms
  (mhelp "Variables associated with jforms."))

(context ms88)
(defsubject ms88
  (mhelp "Flags relevant to the MS88 mating-search procedure."))

(context ms89)
(defsubject ms89
  (mhelp "Flags relevant to the MS89 mating-search procedure."))

(context mating-search)
(defsubject mating-search
  (mhelp "Flags concerning mating search."))
(defsubject important
  (mhelp "The crucial flags that need to be set for automatic proofs."))

(defsubject transmit
  (mhelp "Flags which should be transmitted from a slave tps to a master tps
when piy2 or diy2 is used.  This is so the appropriate flag values can
be recorded by a daterec after such a run."))

(context ext-search)
(defsubject ext-search
  (mhelp "Flags concerning extensional proof search.  These include all flags
relevant to either of the search procedures MS03-7 or MS04-2."))

(defsubject ms03-7
  (mhelp "Flags concerning the proof search procedure MS03-7 which incorporates
extensional reasoning, equality reasoning, and set constraints.  This uses
extensional expansion dags instead of expansion trees.  See Chad E. Brown's thesis."))

(defsubject ms04-2
  (mhelp "Flags concerning the proof search procedure MS04-2 which incorporates
extensional reasoning, equality reasoning, and set constraints.  This uses
extensional expansion dags instead of expansion trees.  See Chad E. Brown's thesis."))

(context ms90-3)
(defsubject ms90-3
  (mhelp "Flags relevant to the MS90-3 mating-search procedure."))

(context ms90-9)
(defsubject ms90-9
  (mhelp "Flags relevant to the MS90-9 mating-search procedure."))

(context ms91)
(defsubject ms91-6
  (mhelp "Flags relevant to the MS91-6 mating-search procedure."))
(defsubject ms91-7
  (mhelp "Flags relevant to the MS91-7 mating-search procedure."))

(context ms92-9)
(defsubject ms92-9
  (mhelp "Flags relevant to the MS92-9 mating-search procedure."))

(context ms93-1)
(defsubject ms93-1
  (mhelp "Flags relevant to the MS93-1 mating-search procedure."))

(context mtree-ops)
(defsubject mtree-top
  (mhelp "Flags concerning the operation of the matingstree top level."))
(defsubject mtree
  (mhelp "Flags concerning matingstree."))

(context unification)
(defsubject Unification
  (mhelp "Variables associated with Unification"))

(context primsubs)
(defsubject primsubs
  (mhelp "Variables associated with primitive substitutions."))

(context tactics)
(defsubject tactics
  (mhelp "Flags concerning tactics."))

(context subtoplevels)
(defsubject test-top
  (mhelp "About the test-top top level."))

(context semantics)
(defsubject semantic-bounds
  (mhelp "Bounds related to models"))
