;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :AUTO)

;;;
;;; File: MTREE-MENUS
;;; Package: AUTO
;;;
;;; Defines the matingstree toplevel menus
;;; cebrown 5/6/02

(deffile mtree-menus
  (part-of mst)
  (extension lsp)
  (mhelp "Defines matingstree toplevel menus."))

(context subtoplevels)

(defmenuitem MATING-TREE
  (display-name "MTREE")
  (placement 4)
  (command "MTREE")
  (parent TOP-LEVELS)
  (mhelp ""))

(defmenu MTREE
  (display-name "Mtree")
  (placement 1)
  (parent MTREE-TOP)
  (mhelp ""))

(defmenuitem GO234
  (display-name "GO")
  (placement 2)
  (command "GO")
  (parent MTREE)
  (mhelp ""))

(defmenuitem ADD-ALL-LIT
  (display-name "ADD-ALL-LIT")
  (placement 3)
  (command "ADD-ALL-LIT")
  (parent MTREE)
  (mhelp ""))

(defmenuitem ADD-ALL-OB
  (display-name "ADD-ALL-OB")
  (placement 4)
  (command "ADD-ALL-OB")
  (parent MTREE)
  (mhelp ""))

(defmenuitem EXPAND-LEAVES
  (display-name "EXPAND-LEAVES")
  (placement 5)
  (command "EXPAND-LEAVES")
  (parent MTREE)
  (mhelp ""))

(defmenuitem MT94-11
  (display-name "MT94-11")
  (placement 6)
  (command "MT94-11")
  (parent MTREE)
  (mhelp ""))

(defmenuitem MT94-12
  (display-name "MT94-12")
  (placement 7)
  (command "MT94-12")
  (parent MTREE)
  (mhelp ""))

(defmenuitem MT95-1
  (display-name "MT95-1")
  (placement 8)
  (command "MT95-1")
  (parent MTREE)
  (mhelp ""))

(defmenuitem QRY
  (display-name "QRY")
  (placement 9)
  (command "QRY")
  (parent MTREE)
  (mhelp ""))

(defmenuitem LEAVE5
  (display-name "LEAVE")
  (placement 100)
  (command "LEAVE")
  (parent MTREE)
  (mhelp ""))

(defmenu MTREE-OPS
  (display-name "Mtree Ops")
  (placement 2)
  (parent MTREE-TOP)
  (mhelp ""))

(defmenuitem ADD-CONN2
  (display-name "ADD-CONN")
  (placement 2)
  (command "ADD-CONN")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem CHOOSE-BRANCH
  (display-name "CHOOSE-BRANCH")
  (placement 3)
  (command "CHOOSE-BRANCH")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem COMPLETE-P2
  (display-name "COMPLETE-P")
  (placement 4)
  (command "COMPLETE-P")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem D23
  (display-name "D")
  (placement 5)
  (command "D")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem GOTO2
  (display-name "GOTO")
  (placement 6)
  (command "GOTO")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem INIT
  (display-name "INIT")
  (placement 7)
  (command "INIT")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem KILL
  (display-name "KILL")
  (placement 8)
  (command "KILL")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem PICK
  (display-name "PICK")
  (placement 9)
  (command "PICK")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem PRUNE
  (display-name "PRUNE")
  (placement 10)
  (command "PRUNE")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem REM-NODE
  (display-name "REM-NODE")
  (placement 11)
  (command "REM-NODE")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem RESURRECT
  (display-name "RESURRECT")
  (placement 12)
  (command "RESURRECT")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem SHOW-MATING2
  (display-name "SHOW-MATING")
  (placement 13)
  (command "SHOW-MATING")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem SHOW-SUBSTS2
  (display-name "SHOW-SUBSTS")
  (placement 14)
  (command "SHOW-SUBSTS")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem SIB
  (display-name "SIB")
  (placement 15)
  (command "SIB")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem UNIFY2
  (display-name "UNIFY")
  (placement 16)
  (command "UNIFY")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenuitem UP2
  (display-name "UP")
  (placement 17)
  (command "UP")
  (parent MTREE-OPS)
  (mhelp ""))

(defmenu MTREE-PRINT
  (display-name "MTree Print")
  (placement 3)
  (parent MTREE-TOP)
  (mhelp ""))

(defmenuitem CONNS-ADDED
  (display-name "CONNS-ADDED")
  (placement 2)
  (command "CONNS-ADDED")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem LIVE-LEAVES
  (display-name "LIVE-LEAVES")
  (placement 3)
  (command "LIVE-LEAVES")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem PM-NODE
  (display-name "PM-NODE")
  (placement 4)
  (command "PM-NODE")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem PMTR
  (display-name "PMTR")
  (placement 5)
  (command "PMTR")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem PMTR*
  (display-name "PMTR*")
  (placement 6)
  (command "PMTR*")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem PMTR-FLAT
  (display-name "PMTR-FLAT")
  (placement 7)
  (command "PMTR-FLAT")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem POB
  (display-name "POB")
  (placement 8)
  (command "POB")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem POB-LITS
  (display-name "POB-LITS")
  (placement 9)
  (command "POB-LITS")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem POB-NODE
  (display-name "POB-NODE")
  (placement 10)
  (command "POB-NODE")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem POTR
  (display-name "POTR")
  (placement 11)
  (command "POTR")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem POTR*-FLAT
  (display-name "POTR*-FLAT")
  (placement 12)
  (command "POTR*-FLAT")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem POTR-FLAT
  (display-name "POTR-FLAT")
  (placement 13)
  (command "POTR-FLAT")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem PPATH
  (display-name "PPATH")
  (placement 14)
  (command "PPATH")
  (parent MTREE-PRINT)
  (mhelp ""))

(defmenuitem PPATH*
  (display-name "PPATH*")
  (placement 15)
  (command "PPATH*")
  (parent MTREE-PRINT)
  (mhelp ""))
