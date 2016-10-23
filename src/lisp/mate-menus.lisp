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
;;; File: MATE-MENUS
;;; Package: AUTO
;;;
;;; Defines the mate toplevel menus
;;; cebrown 5/6/02

(deffile mate-menus
  (part-of mating)
  (extension lsp)
  (mhelp "Defines matingstree toplevel menus."))

(context subtoplevels)

(defmenuitem MATE0
  (display-name "MATE")
  (placement 2)
  (command "MATE")
  (parent TOP-LEVELS)
  (hotkey #\m)
  (mhelp ""))

(defmenu MATE
  (display-name "Mate")
  (placement 1)
  (parent MATE-TOP)
  (mhelp ""))

(defmenuitem ETREE-INFO
  (display-name "ETR-INFO")
  (placement 2)
  (command "ETR-INFO")
  (parent MATE)
  (mhelp ""))

(defmenuitem STATS
  (display-name "STATS")
  (placement 3)
  (command "STATS")
  (parent MATE)
  (mhelp ""))

(defmenuitem EXPUNGE
  (display-name "EXPUNGE")
  (placement 4)
  (command "EXPUNGE")
  (parent MATE)
  (mhelp ""))

(defmenuitem EXPUNGE-OLD
  (display-name "EXPUNGE-OLD")
  (placement 5)
  (command "EXPUNGE-OLD")
  (parent MATE)
  (mhelp ""))

(defmenu SCRIBE-RECORD
  (display-name "Scribe Record")
  (placement 6)
  (parent MATE)
  (mhelp ""))

(defmenuitem O2
  (display-name "O")
  (placement 2)
  (command "O")
  (parent SCRIBE-RECORD)
  (mhelp ""))

(defmenuitem REM2
  (display-name "REM")
  (placement 3)
  (command "REM")
  (parent SCRIBE-RECORD)
  (mhelp ""))

(defmenuitem LEAVE4
  (display-name "LEAVE")
  (placement 100)
  (command "LEAVE")
  (parent MATE)
  (mhelp ""))

(defmenu MATE-PRINTING
  (display-name "Mate Printing")
  (placement 5)
  (parent MATE-TOP)
  (mhelp ""))

(defmenuitem ETD
  (display-name "ETD")
  (placement 2)
  (command "ETD")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem ETP
  (display-name "ETP")
  (placement 3)
  (command "ETP")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem P2
  (display-name "P")
  (placement 4)
  (command "P")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem PDEEP
  (display-name "PDEEP")
  (placement 5)
  (command "PDEEP")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem PP2
  (display-name "PP")
  (placement 6)
  (command "PP")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem PPDEEP
  (display-name "PPDEEP")
  (placement 7)
  (command "PPDEEP")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem PPF
  (display-name "PPF")
  (placement 8)
  (command "PPF")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem PSH
  (display-name "PSH")
  (placement 9)
  (command "PSH")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem PTREE
  (display-name "PTREE")
  (placement 10)
  (command "PTREE")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem PTREE*
  (display-name "PTREE*")
  (placement 11)
  (command "PTREE*")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem PTREE-FILE
  (display-name "PTREE-FILE")
  (placement 12)
  (command "PTREE-FILE")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenuitem SHOW-OPTION-TREE
  (display-name "SHOW-OPTION-TREE")
  (placement 13)
  (command "SHOW-OPTION-TREE")
  (parent MATE-PRINTING)
  (mhelp ""))

(defmenu EXP-TREE-OPS
  (display-name "Exp Tree Ops")
  (placement 3)
  (parent MATE-TOP)
  (mhelp ""))

(defmenuitem DP
  (display-name "DP")
  (placement 2)
  (command "DP")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem DP*
  (display-name "DP*")
  (placement 3)
  (command "DP*")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem DP=
  (display-name "DP=")
  (placement 4)
  (command "DP=")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem DPTREE
  (display-name "DPTREE")
  (placement 5)
  (command "DPTREE")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem DUP-ALL
  (display-name "DUP-ALL")
  (placement 6)
  (command "DUP-ALL")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem DUP-OUTER
  (display-name "DUP-OUTER")
  (placement 7)
  (command "DUP-OUTER")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem DUP-VAR
  (display-name "DUP-VAR")
  (placement 8)
  (command "DUP-VAR")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem EXP
  (display-name "EXP")
  (placement 9)
  (command "EXP")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem MOD-STATUS
  (display-name "MOD-STATUS")
  (placement 10)
  (command "MOD-STATUS")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem NAME-PRIM2
  (display-name "NAME-PRIM")
  (placement 11)
  (command "NAME-PRIM")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem PRIM-ALL
  (display-name "PRIM-ALL")
  (placement 12)
  (command "PRIM-ALL")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem PRIM-OUTER
  (display-name "PRIM-OUTER")
  (placement 13)
  (command "PRIM-OUTER")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem PRIM-SINGLE
  (display-name "PRIM-SINGLE")
  (placement 14)
  (command "PRIM-SINGLE")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem PRIM-SUB
  (display-name "PRIM-SUB")
  (placement 15)
  (command "PRIM-SUB")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem MS98-DUP
  (display-name "MS98-DUP")
  (placement 16)
  (command "MS98-DUP")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem MS98-PRIM
  (display-name "MS98-PRIM")
  (placement 17)
  (command "MS98-PRIM")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem RESTORE-ETREE
  (display-name "RESTORE-ETREE")
  (placement 18)
  (command "RESTORE-ETREE")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem SAVE-ETREE
  (display-name "SAVE-ETREE")
  (placement 19)
  (command "SAVE-ETREE")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem SEL
  (display-name "SEL")
  (placement 20)
  (command "SEL")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem SET-SEARCH-TREE
  (display-name "SET-SEARCH-TREE")
  (placement 21)
  (command "SET-SEARCH-TREE")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem SUB2
  (display-name "SUB")
  (placement 22)
  (command "SUB")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem SUB-ETREE
  (display-name "SUB-ETREE")
  (placement 23)
  (command "SUB-ETREE")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem TERMS
  (display-name "TERMS")
  (placement 24)
  (command "TERMS")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem APPLY-SUBSTS
  (display-name "APPLY-SUBSTS")
  (placement 25)
  (command "APPLY-SUBSTS")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem MERGE-TREE
  (display-name "MERGE-TREE")
  (placement 26)
  (command "MERGE-TREE")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenuitem ADD-EXT-LEMMAS
  (display-name "ADD-EXT-LEMMAS")
  (placement 27)
  (command "ADD-EXT-LEMMAS")
  (parent EXP-TREE-OPS)
  (mhelp ""))

(defmenu MATING-SEARCH
  (display-name "Mating Search")
  (placement 2)
  (parent MATE-TOP)
  (mhelp ""))

(defmenuitem GO23
  (display-name "GO")
  (placement 2)
  (command "GO")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem NOOP2
  (display-name "NOOP")
  (placement 3)
  (command "NOOP")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem UNIFY
  (display-name "UNIFY")
  (placement 4)
  (command "UNIFY")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem ADD-CONN
  (display-name "ADD-CONN")
  (placement 5)
  (command "ADD-CONN")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem ADD-CONN*
  (display-name "ADD-CONN*")
  (placement 6)
  (command "ADD-CONN*")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem INIT-MATING
  (display-name "INIT-MATING")
  (placement 7)
  (command "INIT-MATING")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem PROP-MSEARCH
  (display-name "PROP-MSEARCH")
  (placement 8)
  (command "PROP-MSEARCH")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MINIMAL-P
  (display-name "MINIMAL-P")
  (placement 9)
  (command "MINIMAL-P")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem COMPLETE-P
  (display-name "COMPLETE-P")
  (placement 10)
  (command "COMPLETE-P")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS88
  (display-name "MS88")
  (placement 11)
  (command "MS88")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS88-SUB
  (display-name "MS88-SUB")
  (placement 12)
  (command "MS88-SUB")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS89
  (display-name "MS89")
  (placement 13)
  (command "MS89")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem EXPAND-ETREE
  (display-name "EXPAND-ETREE")
  (placement 14)
  (command "EXPAND-ETREE")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS90-3
  (display-name "MS90-3")
  (placement 15)
  (command "MS90-3")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS90-9
  (display-name "MS90-9")
  (placement 16)
  (command "MS90-9")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS91-6
  (display-name "MS91-6")
  (placement 17)
  (command "MS91-6")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS91-7
  (display-name "MS91-7")
  (placement 18)
  (command "MS91-7")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS92-9
  (display-name "MS92-9")
  (placement 19)
  (command "MS92-9")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS93-1
  (display-name "MS93-1")
  (placement 20)
  (command "MS93-1")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem MS98-1
  (display-name "MS98-1")
  (placement 21)
  (command "MS98-1")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem REM-CONN
  (display-name "REM-CONN")
  (placement 22)
  (command "REM-CONN")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem REM-CONN*
  (display-name "REM-CONN*")
  (placement 23)
  (command "REM-CONN*")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem REM-LAST-CONN
  (display-name "REM-LAST-CONN")
  (placement 24)
  (command "REM-LAST-CONN")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem DEL-DUP-CONNS
  (display-name "DEL-DUP-CONNS")
  (placement 25)
  (command "DEL-DUP-CONNS")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem SHOW-MATING
  (display-name "SHOW-MATING")
  (placement 26)
  (command "SHOW-MATING")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenuitem SHOW-SUBSTS
  (display-name "SHOW-SUBSTS")
  (placement 27)
  (command "SHOW-SUBSTS")
  (parent MATING-SEARCH)
  (mhelp ""))

(defmenu JFORMS
  (display-name "JForms")
  (placement 4)
  (parent MATE-TOP)
  (mhelp ""))

(defmenuitem CJFORM2
  (display-name "CJFORM")
  (placement 2)
  (command "CJFORM")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem CW2
  (display-name "CW")
  (placement 3)
  (command "CW")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem CWD
  (display-name "CWD")
  (placement 4)
  (command "CWD")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem CWS
  (display-name "CWS")
  (placement 5)
  (command "CWS")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem NUM-HPATHS2
  (display-name "NUM-HPATHS")
  (placement 6)
  (command "NUM-HPATHS")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem NUM-VPATHS2
  (display-name "NUM-VPATHS")
  (placement 7)
  (command "NUM-VPATHS")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem VP2
  (display-name "VP")
  (placement 8)
  (command "VP")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem VPD2
  (display-name "VPD")
  (placement 9)
  (command "VPD")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem VPETREE
  (display-name "VPETREE")
  (placement 10)
  (command "VPETREE")
  (parent JFORMS)
  (mhelp ""))

(defmenuitem VPT2
  (display-name "VPT")
  (placement 11)
  (command "VPT")
  (parent JFORMS)
  (mhelp ""))

(defmenu MOVING
  (display-name "Moving")
  (placement 2)
  (parent MATE-TOP)
  (mhelp ""))

(defmenuitem |0|
  (display-name "0 (Up One Level)")
  (placement 2)
  (command "|0|")
  (parent MOVING)
  (mhelp ""))

(defmenuitem D2
  (display-name "D")
  (placement 3)
  (command "D")
  (parent MOVING)
  (mhelp ""))

(defmenuitem FB
  (display-name "FB")
  (placement 4)
  (command "FB")
  (parent MOVING)
  (mhelp ""))

(defmenuitem FI
  (display-name "FI")
  (placement 5)
  (command "FI")
  (parent MOVING)
  (mhelp ""))

(defmenuitem GOTO
  (display-name "GOTO")
  (placement 6)
  (command "GOTO")
  (parent MOVING)
  (mhelp ""))

(defmenuitem L
  (display-name "L")
  (placement 7)
  (command "L")
  (parent MOVING)
  (mhelp ""))

(defmenuitem R
  (display-name "R")
  (placement 8)
  (command "R")
  (parent MOVING)
  (mhelp ""))

(defmenuitem UP
  (display-name "UP")
  (placement 9)
  (command "UP")
  (parent MOVING)
  (mhelp ""))

(defmenuitem ^
  (display-name "^")
  (placement 10)
  (command "^")
  (parent MOVING)
  (mhelp ""))
