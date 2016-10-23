;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of TPS2-RULEP)

(deffile rulep-mac
  (part-of tps2-rulep)
  (extension lisp)
  (mhelp "Flags for deciding how RULEP works."))

(context rulep-test)

(defflag rulep-wffeq
  (flagtype symbol)
  (default wffeq-ab)
  (subjects jforms mating-search ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "The wffop used for testing whether two wffs are equal when checking
RULEP and propositional mating search."))

