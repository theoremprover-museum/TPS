;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of UNIFICATION-interface)
(context unification)
;;;
;;; File: UNIF-MENUS
;;; Package: UNIFICATION-interface
;;;
;;; defines menus unification top-level
;;; 5/6/02

(deffile unif-menus
  (part-of unification-interface)
  (extension lisp)
  (mhelp "Menus for unification top-level."))

(context unification)

(defmenuitem UNIFICATION0
  (display-name "UNIFY")
  (placement 3)
  (command "UNIFY")
  (parent TOP-LEVELS)
  (hotkey #\u)
  (mhelp ""))

(defmenu UNIFICATION
  (display-name "Unification")
  (placement 2)
  (parent UNIF-TOP)
  (mhelp ""))

(defmenuitem |0-2|
  (display-name "|0| (Up One Level)")
  (placement 2)
  (command "|0|")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem APPLY-SUBST
  (display-name "APPLY-SUBST")
  (placement 3)
  (command "APPLY-SUBST")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem EPROOF-UTREE
  (display-name "EPROOF-UTREE")
  (placement 4)
  (command "EPROOF-UTREE")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem GO23456
  (display-name "GO")
  (placement 5)
  (command "GO")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem GOTO23
  (display-name "GOTO")
  (placement 6)
  (command "GOTO")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem MATCH
  (display-name "MATCH")
  (placement 7)
  (command "MATCH")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem MATCH-PAIR
  (display-name "MATCH-PAIR")
  (placement 8)
  (command "MATCH-PAIR")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem NAME-DPAIR
  (display-name "NAME-DPAIR")
  (placement 9)
  (command "NAME-DPAIR")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem NTH-SON
  (display-name "NTH-SON")
  (placement 10)
  (command "NTH-SON")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem P23
  (display-name "P")
  (placement 11)
  (command "P")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem PALL2
  (display-name "PALL")
  (placement 12)
  (command "PALL")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem PP23
  (display-name "PP")
  (placement 13)
  (command "PP")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem PP*
  (display-name "PP*")
  (placement 14)
  (command "PP*")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem SIMPLIFY
  (display-name "SIMPLIFY")
  (placement 15)
  (command "SIMPLIFY")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem STATS2
  (display-name "STATS")
  (placement 16)
  (command "STATS")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem SUBST-STACK
  (display-name "SUBST-STACK")
  (placement 17)
  (command "SUBST-STACK")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem UTREE
  (display-name "UTREE")
  (placement 18)
  (command "UTREE")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem UTREE*
  (display-name "UTREE*")
  (placement 19)
  (command "UTREE*")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem ^2
  (display-name "^")
  (placement 20)
  (command "^")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem ^^
  (display-name "^^")
  (placement 21)
  (command "^^")
  (parent UNIFICATION)
  (mhelp ""))

(defmenuitem LEAVE8
  (display-name "LEAVE")
  (placement 100)
  (command "LEAVE")
  (parent UNIFICATION)
  (mhelp ""))

(defmenu DPAIRS
  (display-name "DPairs")
  (placement 2)
  (parent UNIF-TOP)
  (mhelp ""))

(defmenuitem ADD-DPAIR
  (display-name "ADD-DPAIR")
  (placement 2)
  (command "ADD-DPAIR")
  (parent DPAIRS)
  (mhelp ""))

(defmenuitem ADD-DPAIRS-TO-NODE
  (display-name "ADD-DPAIRS-TO-NODE")
  (placement 3)
  (command "ADD-DPAIRS-TO-NODE")
  (parent DPAIRS)
  (mhelp ""))

(defmenuitem ADD-DPAIRS-TO-UTREE
  (display-name "ADD-DPAIRS-TO-UTREE")
  (placement 4)
  (command "ADD-DPAIRS-TO-UTREE")
  (parent DPAIRS)
  (mhelp ""))

(defmenuitem FIND-NESTING
  (display-name "FIND-NESTING")
  (placement 5)
  (command "FIND-NESTING")
  (parent DPAIRS)
  (mhelp ""))

(defmenuitem PRUNE2
  (display-name "PRUNE")
  (placement 6)
  (command "PRUNE")
  (parent DPAIRS)
  (mhelp ""))

(defmenuitem RM-DPAIR
  (display-name "RM-DPAIR")
  (placement 7)
  (command "RM-DPAIR")
  (parent DPAIRS)
  (mhelp ""))

(defmenuitem SHOW-DPAIRSET
  (display-name "SHOW-DPAIRSET")
  (placement 8)
  (command "SHOW-DPAIRSET")
  (parent DPAIRS)
  (mhelp ""))

(defmenuitem UNIF-PROBLEM
  (display-name "UNIF-PROBLEM")
  (placement 9)
  (command "UNIF-PROBLEM")
  (parent DPAIRS)
  (mhelp ""))

(defmenu UNIFICATION-FLAGS
  (display-name "Unification Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem MAX-SUBSTS-PROJ
  (display-name "MAX-SUBSTS-PROJ")
  (placement 2)
  (command "MAX-SUBSTS-PROJ")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MAX-SUBSTS-PROJ-TOTAL
  (display-name "MAX-SUBSTS-PROJ-TOTAL")
  (placement 3)
  (command "MAX-SUBSTS-PROJ-TOTAL")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MAX-SUBSTS-QUICK
  (display-name "MAX-SUBSTS-QUICK")
  (placement 4)
  (command "MAX-SUBSTS-QUICK")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MAX-SUBSTS-VAR
  (display-name "MAX-SUBSTS-VAR")
  (placement 5)
  (command "MAX-SUBSTS-VAR")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem NUM-OF-DUPS
  (display-name "NUM-OF-DUPS")
  (placement 6)
  (command "NUM-OF-DUPS")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem APPLY-MATCH
  (display-name "APPLY-MATCH")
  (placement 7)
  (command "APPLY-MATCH")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem COUNTSUBS-FIRST
  (display-name "COUNTSUBS-FIRST")
  (placement 8)
  (command "COUNTSUBS-FIRST")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem DNEG-IMITATION
  (display-name "DNEG-IMITATION")
  (placement 9)
  (command "DNEG-IMITATION")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem ETA-RULE
  (display-name "ETA-RULE")
  (placement 10)
  (command "ETA-RULE")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem IMITATION-FIRST
  (display-name "IMITATION-FIRST")
  (placement 11)
  (command "IMITATION-FIRST")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem LEIBNIZ-SUB-CHECK
  (display-name "LEIBNIZ-SUB-CHECK")
  (placement 12)
  (command "LEIBNIZ-SUB-CHECK")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MAX-DUP-PATHS
  (display-name "MAX-DUP-PATHS")
  (placement 13)
  (command "MAX-DUP-PATHS")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MAX-SEARCH-DEPTH
  (display-name "MAX-SEARCH-DEPTH")
  (placement 14)
  (command "MAX-SEARCH-DEPTH")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MAX-UTREE-DEPTH
  (display-name "MAX-UTREE-DEPTH")
  (placement 15)
  (command "MAX-UTREE-DEPTH")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MIN-QUICK-DEPTH
  (display-name "MIN-QUICK-DEPTH")
  (placement 16)
  (command "MIN-QUICK-DEPTH")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MS-DIR
  (display-name "MS-DIR")
  (placement 17)
  (command "MS-DIR")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem MS90-3-QUICK
  (display-name "MS90-3-QUICK")
  (placement 18)
  (command "MS90-3-QUICK")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem PRUNING
  (display-name "PRUNING")
  (placement 19)
  (command "PRUNING")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem REDUCE-DOUBLE-NEG
  (display-name "REDUCE-DOUBLE-NEG")
  (placement 20)
  (command "REDUCE-DOUBLE-NEG")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem RIGID-PATH-CK
  (display-name "RIGID-PATH-CK")
  (placement 21)
  (command "RIGID-PATH-CK")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem STOP-AT-TSN
  (display-name "STOP-AT-TSN")
  (placement 22)
  (command "STOP-AT-TSN")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem SUBSUMPTION-CHECK
  (display-name "SUBSUMPTION-CHECK")
  (placement 23)
  (command "SUBSUMPTION-CHECK")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem SUBSUMPTION-DEPTH
  (display-name "SUBSUMPTION-DEPTH")
  (placement 24)
  (command "SUBSUMPTION-DEPTH")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem SUBSUMPTION-NODES
  (display-name "SUBSUMPTION-NODES")
  (placement 25)
  (command "SUBSUMPTION-NODES")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem TOTAL-NUM-OF-DUPS
  (display-name "TOTAL-NUM-OF-DUPS")
  (placement 26)
  (command "TOTAL-NUM-OF-DUPS")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem UNI-SEARCH-HEURISTIC
  (display-name "UNI-SEARCH-HEURISTIC")
  (placement 27)
  (command "UNI-SEARCH-HEURISTIC")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem UNIF-COUNTER
  (display-name "UNIF-COUNTER")
  (placement 28)
  (command "UNIF-COUNTER")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem UNIF-COUNTER-OUTPUT
  (display-name "UNIF-COUNTER-OUTPUT")
  (placement 29)
  (command "UNIF-COUNTER-OUTPUT")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem UNIF-TRIGGER
  (display-name "UNIF-TRIGGER")
  (placement 30)
  (command "UNIF-TRIGGER")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))

(defmenuitem UNIFY-VERBOSE
  (display-name "UNIFY-VERBOSE")
  (placement 31)
  (command "UNIFY-VERBOSE")
  (parent UNIFICATION-FLAGS)
  (mhelp ""))
