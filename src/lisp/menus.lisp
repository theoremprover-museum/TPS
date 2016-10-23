;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :maint)
(part-of maintain)

(deffile menus
  (part-of maintain)
  (extension lisp)
  (mhelp "Defines the top level menus for the user interface.
Sublevel menus and menu items are defined throughout the lisp files,
usually near the appropriate defflag or defmexpr."))

; The "placement" of a menu or menuitem is a real number giving the placement in
; which the menus or items should appear (smallest to largest).
; Whenever the placement passes a multiple of 100, we put a "-" separator between
; this menu or menuitem and the previous menu/menuitem.
; For example, if we want to separate an item like "HISTORY" from "EXIT",
; this could be accomplished by letting the placement of "HISTORY" be 199 and
; the placement of "EXIT" be 200.  The result will be a menu looking like
;
; HISTORY
; -------
; EXIT

(context tps-maintenance)

; menubar is the root of the menu tree (actually, menu dag).  It doesn't
; show up as a menu.  Each top level may have menus that should be put
; onto the menubar when TPS is in this top level
(defmenu mbar
  (display-name "MenuBar")
  (placement 0)
  (mhelp "The root of the menu tree.  The menus with mbar as a parent
appear on the menu bar of the interface window."))

(defmenu main
  (display-name "Main")
  (placement 1)
  (parent mbar)
  (mhelp "The Main menu for commonly used TPS commands."))

(defmenu flags
  (display-name "Flags")
  (placement 10)
  (parent mbar)
  (mhelp "Main menu for most flags."))

(defmenu rules
  (display-name "Rules")
  (placement 20)
  (parent mbar)
  (mhelp "Main menu for most flags."))

(defmenu misc-commands
  (display-name "Misc-commands")
  (placement 30)
  (parent mbar)
  (mhelp "Menu for Miscellaneous Commands."))

(defmenu top-levels
  (display-name "Top-levels")
  (placement 40)
  (parent mbar)
  (mhelp "Menu for Changing Top Levels."))

(defmenuitem PROVE
  (display-name "PROVE")
  (placement 2)
  (command "PROVE")
  (parent MAIN)
  (hotkey #\p)
  (etps t)
  (mhelp ""))
  
(defmenuitem EXERCISE
  (display-name "EXERCISE")
  (placement 2.5)
  (command "EXERCISE")
  (parent MAIN)
  (etps t)
  (mhelp ""))

(defmenu SET-MODE
  (display-name "Set Mode")
  (placement 3)
  (parent MAIN)
  (mhelp ""))

(defmenuitem LIBRARY-MODE
  (display-name "MODE")
  (placement 2)
  (command "MODE")
  (parent SET-MODE)
  (etps t)
  (mhelp ""))

(defmenuitem MS98-FO-MODE
  (display-name "MODE MS98-FO-MODE")
  (placement 3)
  (command "MODE MS98-FO-MODE")
  (parent SET-MODE)
  (etps t)
  (mhelp ""))

(defmenuitem MS98-HO-MODE
  (display-name "MODE MS98-HO-MODE")
  (placement 4)
  (command "MODE MS98-HO-MODE")
  (parent SET-MODE)
  (etps t)
  (mhelp ""))

(defmenuitem EASY-SV-MODE
  (display-name "MODE EASY-SV-MODE")
  (placement 5)
  (command "MODE EASY-SV-MODE")
  (parent SET-MODE)
  (etps t)
  (mhelp ""))

(defmenuitem MAIN-DIY
  (display-name "DIY (Auto Search)")
  (placement 3)
  (command "DIY")
  (parent MAIN)
  (hotkey #\d)
  (mhelp ""))

(defmenuitem PALL
  (display-name "PALL (Display Proof)")
  (placement 4)
  (command "PALL")
  (parent MAIN)
  (etps t)
  (mhelp ""))

(defmenuitem RECONSIDER-PROOF
  (display-name "RECONSIDER")
  (placement 5)
  (command "RECONSIDER")
  (parent MAIN)
  (etps t)
  (mhelp ""))

(defmenuitem RESTORE-PROOF
  (display-name "RESTOREPROOF")
  (placement 6)
  (command "RESTOREPROOF")
  (parent MAIN)
  (etps t)
  (mhelp ""))

(defmenuitem HELP
  (display-name "HELP")
  (placement 7)
  (command "HELP")
  (parent MAIN)
  (hotkey #\h)
  (etps t)
  (mhelp ""))

(defmenuitem INTERRUPT
  (display-name "INTERRUPT")
  (placement 200)
  (command "INTERRUPT")
  (parent MAIN)
  (hotkey #\c)
  (etps t)
  (mhelp ""))

(defmenuitem HISTORY
  (display-name "HISTORY")
  (placement 300)
  (command "HISTORY")
  (parent MAIN)
  (etps t)
  (mhelp ""))

(defmenuitem EXIT
  (display-name "EXIT")
  (placement 400)
  (command "EXIT")
  (parent MAIN)
  (hotkey #\x)
  (etps t)
  (mhelp ""))

(defmenu SEARCH-FLAGS
  (display-name "Search Flags")
  (placement 2)
  (parent FLAGS)
  (mhelp ""))

(defmenu SET-SUBSTITUTIONS
  (display-name "Set Substitutions")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem BAD-VAR-CONNECTED-PRUNE
  (display-name "BAD-VAR-CONNECTED-PRUNE")
  (placement 2)
  (command "BAD-VAR-CONNECTED-PRUNE")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem DELAY-SETVARS
  (display-name "DELAY-SETVARS")
  (placement 3)
  (command "DELAY-SETVARS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem INCLUDE-COINDUCTION-PRINCIPLE
  (display-name "INCLUDE-COINDUCTION-PRINCIPLE")
  (placement 4)
  (command "INCLUDE-COINDUCTION-PRINCIPLE")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem INCLUDE-INDUCTION-PRINCIPLE
  (display-name "INCLUDE-INDUCTION-PRINCIPLE")
  (placement 5)
  (command "INCLUDE-INDUCTION-PRINCIPLE")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem MAX-CONSTRAINT-SIZE
  (display-name "MAX-CONSTRAINT-SIZE")
  (placement 6)
  (command "MAX-CONSTRAINT-SIZE")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem MAX-NUM-CONSTRAINTS
  (display-name "MAX-NUM-CONSTRAINTS")
  (placement 7)
  (command "MAX-NUM-CONSTRAINTS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PRIMSUB-VAR-SELECT
  (display-name "PRIMSUB-VAR-SELECT")
  (placement 8)
  (command "PRIMSUB-VAR-SELECT")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem MAX-PRIM-DEPTH
  (display-name "MAX-PRIM-DEPTH")
  (placement 9)
  (command "MAX-PRIM-DEPTH")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem MAX-PRIM-LITS
  (display-name "MAX-PRIM-LITS")
  (placement 10)
  (command "MAX-PRIM-LITS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem MIN-PRIM-DEPTH
  (display-name "MIN-PRIM-DEPTH")
  (placement 11)
  (command "MIN-PRIM-DEPTH")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem MIN-PRIM-LITS
  (display-name "MIN-PRIM-LITS")
  (placement 12)
  (command "MIN-PRIM-LITS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem NEG-PRIM-SUB
  (display-name "NEG-PRIM-SUB")
  (placement 13)
  (command "NEG-PRIM-SUB")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PR00-ALLOW-SUBNODE-CONNS
  (display-name "PR00-ALLOW-SUBNODE-CONNS")
  (placement 14)
  (command "PR00-ALLOW-SUBNODE-CONNS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PR00-MAX-SUBSTS-VAR
  (display-name "PR00-MAX-SUBSTS-VAR")
  (placement 15)
  (command "PR00-MAX-SUBSTS-VAR")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PR00-NUM-ITERATIONS
  (display-name "PR00-NUM-ITERATIONS")
  (placement 16)
  (command "PR00-NUM-ITERATIONS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PR00-REQUIRE-ARG-DEPS
  (display-name "PR00-REQUIRE-ARG-DEPS")
  (placement 17)
  (command "PR00-REQUIRE-ARG-DEPS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PR97C-MAX-ABBREVS
  (display-name "PR97C-MAX-ABBREVS")
  (placement 18)
  (command "PR97C-MAX-ABBREVS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PR97C-PRENEX
  (display-name "PR97C-PRENEX")
  (placement 19)
  (command "PR97C-PRENEX")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PRIM-BDTYPES
  (display-name "PRIM-BDTYPES")
  (placement 20)
  (command "PRIM-BDTYPES")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PRIM-BDTYPES-AUTO
  (display-name "PRIM-BDTYPES-AUTO")
  (placement 21)
  (command "PRIM-BDTYPES-AUTO")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PRIM-PREFIX
  (display-name "PRIM-PREFIX")
  (placement 22)
  (command "PRIM-PREFIX")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem PRIMSUB-METHOD
  (display-name "PRIMSUB-METHOD")
  (placement 23)
  (command "PRIMSUB-METHOD")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenuitem WHICH-CONSTRAINTS
  (display-name "WHICH-CONSTRAINTS")
  (placement 24)
  (command "WHICH-CONSTRAINTS")
  (parent SET-SUBSTITUTIONS)
  (mhelp ""))

(defmenu EQUALITY-FLAGS
  (display-name "Equality Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem LAMBDA-CONV
  (display-name "LAMBDA-CONV")
  (placement 2)
  (command "LAMBDA-CONV")
  (parent EQUALITY-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem REWRITE-DEFNS
  (display-name "REWRITE-DEFNS")
  (placement 3)
  (command "REWRITE-DEFNS")
  (parent EQUALITY-FLAGS)
  (mhelp ""))

(defmenuitem REWRITE-EQUALITIES
  (display-name "REWRITE-EQUALITIES")
  (placement 4)
  (command "REWRITE-EQUALITIES")
  (parent EQUALITY-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem REWRITE-EQUIVS
  (display-name "REWRITE-EQUIVS")
  (placement 5)
  (command "REWRITE-EQUIVS")
  (parent EQUALITY-FLAGS)
  (etps t)
  (mhelp ""))

(defmenu JFORM-FLAGS
  (display-name "JForm Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem ALLOW-NONLEAF-CONNS
  (display-name "ALLOW-NONLEAF-CONNS")
  (placement 2)
  (command "ALLOW-NONLEAF-CONNS")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem DISSOLVE
  (display-name "DISSOLVE")
  (placement 3)
  (command "DISSOLVE")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem MATE-UP-TO-NNF
  (display-name "MATE-UP-TO-NNF")
  (placement 4)
  (command "MATE-UP-TO-NNF")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem PLACEMENT-COMPONENTS
  (display-name "PLACEMENT-COMPONENTS")
  (placement 5)
  (command "PLACEMENT-COMPONENTS")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem PRINT-LIT-NAME
  (display-name "PRINT-LIT-NAME")
  (placement 6)
  (command "PRINT-LIT-NAME")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem PRINTVPDFLAG
  (display-name "PRINTVPDFLAG")
  (placement 7)
  (command "PRINTVPDFLAG")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem TEXFORMAT
  (display-name "TEXFORMAT")
  (placement 8)
  (command "TEXFORMAT")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPD-BRIEF
  (display-name "VPD-BRIEF")
  (placement 9)
  (command "VPD-BRIEF")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPD-FILENAME
  (display-name "VPD-FILENAME")
  (placement 10)
  (command "VPD-FILENAME")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPD-PTYPES
  (display-name "VPD-PTYPES")
  (placement 11)
  (command "VPD-PTYPES")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPD-STYLE
  (display-name "VPD-STYLE")
  (placement 12)
  (command "VPD-STYLE")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPD-VPFPAGE
  (display-name "VPD-VPFPAGE")
  (placement 13)
  (command "VPD-VPFPAGE")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPFORM-LABELS
  (display-name "VPFORM-LABELS")
  (placement 14)
  (command "VPFORM-LABELS")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPFORM-TEX-MAGNIFICATION
  (display-name "VPFORM-TEX-MAGNIFICATION")
  (placement 15)
  (command "VPFORM-TEX-MAGNIFICATION")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPFORM-TEX-NEST
  (display-name "VPFORM-TEX-NEST")
  (placement 16)
  (command "VPFORM-TEX-NEST")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPFORM-TEX-PREAMBLE
  (display-name "VPFORM-TEX-PREAMBLE")
  (placement 17)
  (command "VPFORM-TEX-PREAMBLE")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPW-HEIGHT
  (display-name "VPW-HEIGHT")
  (placement 18)
  (command "VPW-HEIGHT")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenuitem VPW-WIDTH
  (display-name "VPW-WIDTH")
  (placement 19)
  (command "VPW-WIDTH")
  (parent JFORM-FLAGS)
  (mhelp ""))

(defmenu MATING-SEARCH-FLAGS
  (display-name "Mating Search Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem ASSERT-LEMMAS
  (display-name "ASSERT-LEMMAS")
  (placement 2)
  (command "ASSERT-LEMMAS")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-EXPAND
  (display-name "DEFAULT-EXPAND")
  (placement 3)
  (command "DEFAULT-EXPAND")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-MATE
  (display-name "DEFAULT-MATE")
  (placement 4)
  (command "DEFAULT-MATE")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-MS
  (display-name "DEFAULT-MS")
  (placement 5)
  (command "DEFAULT-MS")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem INTERRUPT-ENABLE
  (display-name "INTERRUPT-ENABLE")
  (placement 6)
  (command "INTERRUPT-ENABLE")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem MATING-VERBOSE
  (display-name "MATING-VERBOSE")
  (placement 7)
  (command "MATING-VERBOSE")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem MONITORFLAG
  (display-name "MONITORFLAG")
  (placement 8)
  (command "MONITORFLAG")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem NEW-MATING-AFTER-DUP
  (display-name "NEW-MATING-AFTER-DUP")
  (placement 9)
  (command "NEW-MATING-AFTER-DUP")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem QUERY-USER
  (display-name "QUERY-USER")
  (placement 10)
  (command "QUERY-USER")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem REC-MS-FILE
  (display-name "REC-MS-FILE")
  (placement 11)
  (command "REC-MS-FILE")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem REC-MS-FILENAME
  (display-name "REC-MS-FILENAME")
  (placement 12)
  (command "REC-MS-FILENAME")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem USE-DIY
  (display-name "USE-DIY")
  (placement 13)
  (command "USE-DIY")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem USE-EXT-LEMMAS
  (display-name "USE-EXT-LEMMAS")
  (placement 14)
  (command "USE-EXT-LEMMAS")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem USE-FAST-PROP-SEARCH
  (display-name "USE-FAST-PROP-SEARCH")
  (placement 15)
  (command "USE-FAST-PROP-SEARCH")
  (parent MATING-SEARCH-FLAGS)
  (mhelp ""))

(defmenu EXPANSION-TREE-FLAGS
  (display-name "Expansion Tree Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem ADD-TRUTH
  (display-name "ADD-TRUTH")
  (placement 2)
  (command "ADD-TRUTH")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem DUPLICATION-STRATEGY
  (display-name "DUPLICATION-STRATEGY")
  (placement 3)
  (command "DUPLICATION-STRATEGY")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem DUPLICATION-STRATEGY-PFD
  (display-name "DUPLICATION-STRATEGY-PFD")
  (placement 4)
  (command "DUPLICATION-STRATEGY-PFD")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem INITIAL-BKTRACK-LIMIT
  (display-name "INITIAL-BKTRACK-LIMIT")
  (placement 5)
  (command "INITIAL-BKTRACK-LIMIT")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem MIN-QUANTIFIER-SCOPE
  (display-name "MIN-QUANTIFIER-SCOPE")
  (placement 6)
  (command "MIN-QUANTIFIER-SCOPE")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem PRINT-DEEP
  (display-name "PRINT-DEEP")
  (placement 7)
  (command "PRINT-DEEP")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem PRINT-NODENAMES
  (display-name "PRINT-NODENAMES")
  (placement 8)
  (command "PRINT-NODENAMES")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem SHOW-SKOLEM
  (display-name "SHOW-SKOLEM")
  (placement 9)
  (command "SHOW-SKOLEM")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem SKOLEM-DEFAULT
  (display-name "SKOLEM-DEFAULT")
  (placement 10)
  (command "SKOLEM-DEFAULT")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenuitem TRUTHVALUES-HACK
  (display-name "TRUTHVALUES-HACK")
  (placement 11)
  (command "TRUTHVALUES-HACK")
  (parent EXPANSION-TREE-FLAGS)
  (mhelp ""))

(defmenu SEQUENT-CALCULUS-FLAGS
  (display-name "Sequent Calculus Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem PSEQ-USE-LABELS
  (display-name "PSEQ-USE-LABELS")
  (placement 2)
  (command "PSEQ-USE-LABELS")
  (parent SEQUENT-CALCULUS-FLAGS)
  (mhelp ""))

(defmenu MS98-1-FLAGS
  (display-name "MS98-1 Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem BREAK-AT-QUANTIFIERS
  (display-name "BREAK-AT-QUANTIFIERS")
  (placement 2)
  (command "BREAK-AT-QUANTIFIERS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem FF-DELAY
  (display-name "FF-DELAY")
  (placement 3)
  (command "FF-DELAY")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem HPATH-THRESHOLD
  (display-name "HPATH-THRESHOLD")
  (placement 4)
  (command "HPATH-THRESHOLD")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MAXIMIZE-FIRST
  (display-name "MAXIMIZE-FIRST")
  (placement 5)
  (command "MAXIMIZE-FIRST")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MEASUREMENTS
  (display-name "MEASUREMENTS")
  (placement 6)
  (command "MEASUREMENTS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-BASE-PRIM
  (display-name "MS98-BASE-PRIM")
  (placement 7)
  (command "MS98-BASE-PRIM")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-DUP-BELOW-PRIMSUBS
  (display-name "MS98-DUP-BELOW-PRIMSUBS")
  (placement 8)
  (command "MS98-DUP-BELOW-PRIMSUBS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-DUP-PRIMSUBS
  (display-name "MS98-DUP-PRIMSUBS")
  (placement 9)
  (command "MS98-DUP-PRIMSUBS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-FIRST-FRAGMENT
  (display-name "MS98-FIRST-FRAGMENT")
  (placement 10)
  (command "MS98-FIRST-FRAGMENT")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-FORCE-H-O
  (display-name "MS98-FORCE-H-O")
  (placement 11)
  (command "MS98-FORCE-H-O")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-FRAGMENT-PLACEMENT
  (display-name "MS98-FRAGMENT-PLACEMENT")
  (placement 12)
  (command "MS98-FRAGMENT-PLACEMENT")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-INIT
  (display-name "MS98-INIT")
  (placement 13)
  (command "MS98-INIT")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-LOW-MEMORY
  (display-name "MS98-LOW-MEMORY")
  (placement 14)
  (command "MS98-LOW-MEMORY")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-MAX-COMPONENTS
  (display-name "MS98-MAX-COMPONENTS")
  (placement 15)
  (command "MS98-MAX-COMPONENTS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-MAX-PRIMS
  (display-name "MS98-MAX-PRIMS")
  (placement 16)
  (command "MS98-MAX-PRIMS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-MEASURE
  (display-name "MS98-MEASURE")
  (placement 17)
  (command "MS98-MEASURE")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-MERGE-DAGS
  (display-name "MS98-MERGE-DAGS")
  (placement 18)
  (command "MS98-MERGE-DAGS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-MINIMALITY-CHECK
  (display-name "MS98-MINIMALITY-CHECK")
  (placement 19)
  (command "MS98-MINIMALITY-CHECK")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-NUM-OF-DUPS
  (display-name "MS98-NUM-OF-DUPS")
  (placement 20)
  (command "MS98-NUM-OF-DUPS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-PRIMSUB-COUNT
  (display-name "MS98-PRIMSUB-COUNT")
  (placement 21)
  (command "MS98-PRIMSUB-COUNT")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-REW-PRIMSUBS
  (display-name "MS98-REW-PRIMSUBS")
  (placement 22)
  (command "MS98-REW-PRIMSUBS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-REWRITE-DEPTH
  (display-name "MS98-REWRITE-DEPTH")
  (placement 23)
  (command "MS98-REWRITE-DEPTH")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-REWRITE-MODEL
  (display-name "MS98-REWRITE-MODEL")
  (placement 24)
  (command "MS98-REWRITE-MODEL")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-REWRITE-PRUNE
  (display-name "MS98-REWRITE-PRUNE")
  (placement 25)
  (command "MS98-REWRITE-PRUNE")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-REWRITE-SIZE
  (display-name "MS98-REWRITE-SIZE")
  (placement 26)
  (command "MS98-REWRITE-SIZE")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-REWRITE-UNIF
  (display-name "MS98-REWRITE-UNIF")
  (placement 27)
  (command "MS98-REWRITE-UNIF")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-REWRITES
  (display-name "MS98-REWRITES")
  (placement 28)
  (command "MS98-REWRITES")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-TRACE
  (display-name "MS98-TRACE")
  (placement 29)
  (command "MS98-TRACE")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-UNIF-HACK
  (display-name "MS98-UNIF-HACK")
  (placement 30)
  (command "MS98-UNIF-HACK")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-UNIF-HACK2
  (display-name "MS98-UNIF-HACK2")
  (placement 31)
  (command "MS98-UNIF-HACK2")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-USE-COLORS
  (display-name "MS98-USE-COLORS")
  (placement 32)
  (command "MS98-USE-COLORS")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-VALID-PAIR
  (display-name "MS98-VALID-PAIR")
  (placement 33)
  (command "MS98-VALID-PAIR")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-VARIABLE-PLACEMENT
  (display-name "MS98-VARIABLE-PLACEMENT")
  (placement 34)
  (command "MS98-VARIABLE-PLACEMENT")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenuitem MS98-VERBOSE
  (display-name "MS98-VERBOSE")
  (placement 35)
  (command "MS98-VERBOSE")
  (parent MS98-1-FLAGS)
  (mhelp ""))

(defmenu MS91-FLAGS
  (display-name "MS91 Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem MS91-INTERLEAVE
  (display-name "MS91-INTERLEAVE")
  (placement 2)
  (command "MS91-INTERLEAVE")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem MS91-PREFER-SMALLER
  (display-name "MS91-PREFER-SMALLER")
  (placement 3)
  (command "MS91-PREFER-SMALLER")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem MS91-TIME-BY-VPATHS
  (display-name "MS91-TIME-BY-VPATHS")
  (placement 4)
  (command "MS91-TIME-BY-VPATHS")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem MS91-WEIGHT-LIMIT-RANGE
  (display-name "MS91-WEIGHT-LIMIT-RANGE")
  (placement 5)
  (command "MS91-WEIGHT-LIMIT-RANGE")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem NEW-OPTION-SET-LIMIT
  (display-name "NEW-OPTION-SET-LIMIT")
  (placement 6)
  (command "NEW-OPTION-SET-LIMIT")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem OPTIONS-GENERATE-ARG
  (display-name "OPTIONS-GENERATE-ARG")
  (placement 7)
  (command "OPTIONS-GENERATE-ARG")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem OPTIONS-GENERATE-FN
  (display-name "OPTIONS-GENERATE-FN")
  (placement 8)
  (command "OPTIONS-GENERATE-FN")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem OPTIONS-GENERATE-UPDATE
  (display-name "OPTIONS-GENERATE-UPDATE")
  (placement 9)
  (command "OPTIONS-GENERATE-UPDATE")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem OPTIONS-VERBOSE
  (display-name "OPTIONS-VERBOSE")
  (placement 10)
  (command "OPTIONS-VERBOSE")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem PENALTY-FOR-EACH-PRIMSUB
  (display-name "PENALTY-FOR-EACH-PRIMSUB")
  (placement 11)
  (command "PENALTY-FOR-EACH-PRIMSUB")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem PENALTY-FOR-MULTIPLE-PRIMSUBS
  (display-name "PENALTY-FOR-MULTIPLE-PRIMSUBS")
  (placement 12)
  (command "PENALTY-FOR-MULTIPLE-PRIMSUBS")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem PENALTY-FOR-MULTIPLE-SUBS
  (display-name "PENALTY-FOR-MULTIPLE-SUBS")
  (placement 13)
  (command "PENALTY-FOR-MULTIPLE-SUBS")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem PENALTY-FOR-ORDINARY-DUP
  (display-name "PENALTY-FOR-ORDINARY-DUP")
  (placement 14)
  (command "PENALTY-FOR-ORDINARY-DUP")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem RECONSIDER-FN
  (display-name "RECONSIDER-FN")
  (placement 15)
  (command "RECONSIDER-FN")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem WEIGHT-A-COEFFICIENT
  (display-name "WEIGHT-A-COEFFICIENT")
  (placement 16)
  (command "WEIGHT-A-COEFFICIENT")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem WEIGHT-A-FN
  (display-name "WEIGHT-A-FN")
  (placement 17)
  (command "WEIGHT-A-FN")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem WEIGHT-B-COEFFICIENT
  (display-name "WEIGHT-B-COEFFICIENT")
  (placement 18)
  (command "WEIGHT-B-COEFFICIENT")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem WEIGHT-B-FN
  (display-name "WEIGHT-B-FN")
  (placement 19)
  (command "WEIGHT-B-FN")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem WEIGHT-C-COEFFICIENT
  (display-name "WEIGHT-C-COEFFICIENT")
  (placement 20)
  (command "WEIGHT-C-COEFFICIENT")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenuitem WEIGHT-C-FN
  (display-name "WEIGHT-C-FN")
  (placement 21)
  (command "WEIGHT-C-FN")
  (parent MS91-FLAGS)
  (mhelp ""))

(defmenu MS90-3-FLAGS
  (display-name "MS90-3 Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem MAX-MATES
  (display-name "MAX-MATES")
  (placement 2)
  (command "MAX-MATES")
  (parent MS90-3-FLAGS)
  (mhelp ""))

(defmenuitem MIN-QUANT-ETREE
  (display-name "MIN-QUANT-ETREE")
  (placement 3)
  (command "MIN-QUANT-ETREE")
  (parent MS90-3-FLAGS)
  (mhelp ""))

(defmenuitem MS90-3-DUP-STRATEGY
  (display-name "MS90-3-DUP-STRATEGY")
  (placement 4)
  (command "MS90-3-DUP-STRATEGY")
  (parent MS90-3-FLAGS)
  (mhelp ""))

(defmenuitem NUM-FRPAIRS
  (display-name "NUM-FRPAIRS")
  (placement 5)
  (command "NUM-FRPAIRS")
  (parent MS90-3-FLAGS)
  (mhelp ""))

(defmenuitem PRINT-MATING-COUNTER
  (display-name "PRINT-MATING-COUNTER")
  (placement 6)
  (command "PRINT-MATING-COUNTER")
  (parent MS90-3-FLAGS)
  (mhelp ""))

(defmenuitem SHOW-TIME
  (display-name "SHOW-TIME")
  (placement 7)
  (command "SHOW-TIME")
  (parent MS90-3-FLAGS)
  (mhelp ""))

(defmenu MS89-FLAGS
  (display-name "MS89 Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem MAX-SEARCH-LIMIT
  (display-name "MAX-SEARCH-LIMIT")
  (placement 2)
  (command "MAX-SEARCH-LIMIT")
  (parent MS89-FLAGS)
  (mhelp ""))

(defmenuitem RANK-EPROOF-FN
  (display-name "RANK-EPROOF-FN")
  (placement 3)
  (command "RANK-EPROOF-FN")
  (parent MS89-FLAGS)
  (mhelp ""))

(defmenuitem SEARCH-TIME-LIMIT
  (display-name "SEARCH-TIME-LIMIT")
  (placement 4)
  (command "SEARCH-TIME-LIMIT")
  (parent MS89-FLAGS)
  (mhelp ""))

(defmenu MS88-FLAGS
  (display-name "MS88 Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem ADDED-CONN-ENABLED0
  (display-name "ADDED-CONN-ENABLED")
  (placement 2)
  (command "ADDED-CONN-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem CONSIDERED-CONN-ENABLED0
  (display-name "CONSIDERED-CONN-ENABLED")
  (placement 3)
  (command "CONSIDERED-CONN-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem DUP-ALLOWED
  (display-name "DUP-ALLOWED")
  (placement 4)
  (command "DUP-ALLOWED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem DUPE-ENABLED0
  (display-name "DUPE-ENABLED")
  (placement 5)
  (command "DUPE-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem DUPE-VAR-ENABLED0
  (display-name "DUPE-VAR-ENABLED")
  (placement 6)
  (command "DUPE-VAR-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem EXCLUDING-GC-TIME
  (display-name "EXCLUDING-GC-TIME")
  (placement 7)
  (command "EXCLUDING-GC-TIME")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem FIRST-PLACEMENT-MODE-MS
  (display-name "FIRST-PLACEMENT-MODE-MS")
  (placement 8)
  (command "FIRST-PLACEMENT-MODE-MS")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem INCOMP-MATING-ENABLED0
  (display-name "INCOMP-MATING-ENABLED")
  (placement 9)
  (command "INCOMP-MATING-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem MATE-FFPAIR
  (display-name "MATE-FFPAIR")
  (placement 10)
  (command "MATE-FFPAIR")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem MATE-SUBSUMED-TEST-ENABLED0
  (display-name "MATE-SUBSUMED-TEST-ENABLED")
  (placement 11)
  (command "MATE-SUBSUMED-TEST-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem MATE-SUBSUMED-TRUE-ENABLED0
  (display-name "MATE-SUBSUMED-TRUE-ENABLED")
  (placement 12)
  (command "MATE-SUBSUMED-TRUE-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem MATING-CHANGED-ENABLED0
  (display-name "MATING-CHANGED-ENABLED")
  (placement 13)
  (command "MATING-CHANGED-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem MS-INIT-PATH
  (display-name "MS-INIT-PATH")
  (placement 14)
  (command "MS-INIT-PATH")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem MS-SPLIT
  (display-name "MS-SPLIT")
  (placement 15)
  (command "MS-SPLIT")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem OCCURS-CHECK
  (display-name "OCCURS-CHECK")
  (placement 16)
  (command "OCCURS-CHECK")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem PRIM-QUANTIFIER
  (display-name "PRIM-QUANTIFIER")
  (placement 17)
  (command "PRIM-QUANTIFIER")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem PRIMSUB-ENABLED0
  (display-name "PRIMSUB-ENABLED")
  (placement 18)
  (command "PRIMSUB-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem PROP-STRATEGY
  (display-name "PROP-STRATEGY")
  (placement 19)
  (command "PROP-STRATEGY")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem REMOVED-CONN-ENABLED0
  (display-name "REMOVED-CONN-ENABLED")
  (placement 20)
  (command "REMOVED-CONN-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem SEARCH-COMPLETE-PATHS
  (display-name "SEARCH-COMPLETE-PATHS")
  (placement 21)
  (command "SEARCH-COMPLETE-PATHS")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem START-TIME-ENABLED0
  (display-name "START-TIME-ENABLED")
  (placement 22)
  (command "START-TIME-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem STOP-TIME-ENABLED0
  (display-name "STOP-TIME-ENABLED")
  (placement 23)
  (command "STOP-TIME-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem TIMING-NAMED
  (display-name "TIMING-NAMED")
  (placement 24)
  (command "TIMING-NAMED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem UNIF-SUBSUMED-TEST-ENABLED0
  (display-name "UNIF-SUBSUMED-TEST-ENABLED")
  (placement 25)
  (command "UNIF-SUBSUMED-TEST-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenuitem UNIF-SUBSUMED-TRUE-ENABLED0
  (display-name "UNIF-SUBSUMED-TRUE-ENABLED")
  (placement 26)
  (command "UNIF-SUBSUMED-TRUE-ENABLED")
  (parent MS88-FLAGS)
  (mhelp ""))

(defmenu MATING-TREE-FLAGS
  (display-name "Mating Tree Flags")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem MT-SUBSUMPTION-CHECK
  (display-name "MT-SUBSUMPTION-CHECK")
  (placement 2)
  (command "MT-SUBSUMPTION-CHECK")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenuitem MT94-12-TRIGGER
  (display-name "MT94-12-TRIGGER")
  (placement 3)
  (command "MT94-12-TRIGGER")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenuitem MTREE-FILTER-DUPS
  (display-name "MTREE-FILTER-DUPS")
  (placement 4)
  (command "MTREE-FILTER-DUPS")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenuitem MTREE-STOP-IMMEDIATELY
  (display-name "MTREE-STOP-IMMEDIATELY")
  (placement 5)
  (command "MTREE-STOP-IMMEDIATELY")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenuitem TAG-CONN-FN
  (display-name "TAG-CONN-FN")
  (placement 6)
  (command "TAG-CONN-FN")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenuitem TAG-MATING-FN
  (display-name "TAG-MATING-FN")
  (placement 7)
  (command "TAG-MATING-FN")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-OB
  (display-name "DEFAULT-OB")
  (placement 8)
  (command "DEFAULT-OB")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenuitem MT-DUPS-PER-QUANT
  (display-name "MT-DUPS-PER-QUANT")
  (placement 9)
  (command "MT-DUPS-PER-QUANT")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenuitem MT-DEFAULT-OB-MATE
  (display-name "MT-DEFAULT-OB-MATE")
  (placement 10)
  (command "MT-DEFAULT-OB-MATE")
  (parent MATING-TREE-FLAGS)
  (mhelp ""))

(defmenu TEST-SEARCHLISTS
  (display-name "Test Searchlists")
  (placement 2)
  (parent SEARCH-FLAGS)
  (mhelp ""))

(defmenuitem TEST-EASIER-IF-HIGH
  (display-name "TEST-EASIER-IF-HIGH")
  (placement 2)
  (command "TEST-EASIER-IF-HIGH")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-EASIER-IF-LOW
  (display-name "TEST-EASIER-IF-LOW")
  (placement 3)
  (command "TEST-EASIER-IF-LOW")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-EASIER-IF-NIL
  (display-name "TEST-EASIER-IF-NIL")
  (placement 4)
  (command "TEST-EASIER-IF-NIL")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-EASIER-IF-T
  (display-name "TEST-EASIER-IF-T")
  (placement 5)
  (command "TEST-EASIER-IF-T")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-FASTER-IF-HIGH
  (display-name "TEST-FASTER-IF-HIGH")
  (placement 6)
  (command "TEST-FASTER-IF-HIGH")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-FASTER-IF-LOW
  (display-name "TEST-FASTER-IF-LOW")
  (placement 7)
  (command "TEST-FASTER-IF-LOW")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-FASTER-IF-NIL
  (display-name "TEST-FASTER-IF-NIL")
  (placement 8)
  (command "TEST-FASTER-IF-NIL")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-FASTER-IF-T
  (display-name "TEST-FASTER-IF-T")
  (placement 9)
  (command "TEST-FASTER-IF-T")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-FIX-UNIF-DEPTHS
  (display-name "TEST-FIX-UNIF-DEPTHS")
  (placement 10)
  (command "TEST-FIX-UNIF-DEPTHS")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-INCREASE-TIME
  (display-name "TEST-INCREASE-TIME")
  (placement 11)
  (command "TEST-INCREASE-TIME")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-INITIAL-TIME-LIMIT
  (display-name "TEST-INITIAL-TIME-LIMIT")
  (placement 12)
  (command "TEST-INITIAL-TIME-LIMIT")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-MAX-SEARCH-VALUES
  (display-name "TEST-MAX-SEARCH-VALUES")
  (placement 13)
  (command "TEST-MAX-SEARCH-VALUES")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-NEXT-SEARCH-FN
  (display-name "TEST-NEXT-SEARCH-FN")
  (placement 14)
  (command "TEST-NEXT-SEARCH-FN")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-REDUCE-TIME
  (display-name "TEST-REDUCE-TIME")
  (placement 15)
  (command "TEST-REDUCE-TIME")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TEST-VERBOSE
  (display-name "TEST-VERBOSE")
  (placement 16)
  (command "TEST-VERBOSE")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TESTWIN-HEIGHT
  (display-name "TESTWIN-HEIGHT")
  (placement 17)
  (command "TESTWIN-HEIGHT")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenuitem TESTWIN-WIDTH
  (display-name "TESTWIN-WIDTH")
  (placement 18)
  (command "TESTWIN-WIDTH")
  (parent TEST-SEARCHLISTS)
  (mhelp ""))

(defmenu NATURAL-DEDUCTION-FLAGS
  (display-name "Natural Deduction Flags")
  (placement 2)
  (parent FLAGS)
  (mhelp ""))

(defmenu RULE-P-FLAGS
  (display-name "Rule P Flags")
  (placement 2)
  (parent NATURAL-DEDUCTION-FLAGS)
  (mhelp ""))

(defmenuitem USE-RULEP
  (display-name "USE-RULEP")
  (placement 2)
  (command "USE-RULEP")
  (parent RULE-P-FLAGS)
  (mhelp ""))

(defmenuitem RULEP-MAINFN
  (display-name "RULEP-MAINFN")
  (placement 3)
  (command "RULEP-MAINFN")
  (parent RULE-P-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem RULEP-WFFEQ
  (display-name "RULEP-WFFEQ")
  (placement 4)
  (command "RULEP-WFFEQ")
  (parent RULE-P-FLAGS)
  (mhelp ""))

(defmenu MANIPULATION-FLAGS
  (display-name "Manipulation Flags")
  (placement 2)
  (parent NATURAL-DEDUCTION-FLAGS)
  (mhelp ""))

(defmenuitem AUTO-GENERATE-HYPS
  (display-name "AUTO-GENERATE-HYPS")
  (placement 2)
  (command "AUTO-GENERATE-HYPS")
  (parent MANIPULATION-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem CLEANUP-RULEC
  (display-name "CLEANUP-RULEC")
  (placement 3)
  (command "CLEANUP-RULEC")
  (parent MANIPULATION-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem CLEANUP-SAME
  (display-name "CLEANUP-SAME")
  (placement 4)
  (command "CLEANUP-SAME")
  (parent MANIPULATION-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem DEFAULT-WFFEQ
  (display-name "DEFAULT-WFFEQ")
  (placement 5)
  (command "DEFAULT-WFFEQ")
  (parent MANIPULATION-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem PRINT-DOTS
  (display-name "PRINT-DOTS")
  (placement 6)
  (command "PRINT-DOTS")
  (parent MANIPULATION-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTLINEFLAG
  (display-name "PRINTLINEFLAG")
  (placement 7)
  (command "PRINTLINEFLAG")
  (parent MANIPULATION-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem SHORT-HELP
  (display-name "SHORT-HELP")
  (placement 8)
  (command "SHORT-HELP")
  (parent MANIPULATION-FLAGS)
  (etps t)
  (mhelp ""))

(defmenu SUGGESTION-FLAGS
  (display-name "Suggestion Flags")
  (placement 2)
  (parent NATURAL-DEDUCTION-FLAGS)
  (mhelp ""))

(defmenuitem GO-INSTRUCTIONS
  (display-name "GO-INSTRUCTIONS")
  (placement 2)
  (command "GO-INSTRUCTIONS")
  (parent SUGGESTION-FLAGS)
  (mhelp ""))

(defmenuitem QUIETLY-USE-DEFAULTS
  (display-name "QUIETLY-USE-DEFAULTS")
  (placement 3)
  (command "QUIETLY-USE-DEFAULTS")
  (parent SUGGESTION-FLAGS)
  (mhelp ""))

(defmenuitem RESOLVE-CONFLICT
  (display-name "RESOLVE-CONFLICT")
  (placement 4)
  (command "RESOLVE-CONFLICT")
  (parent SUGGESTION-FLAGS)
  (mhelp ""))

(defmenu ENTERING-FLAGS
  (display-name "Entering Flags")
  (placement 2)
  (parent NATURAL-DEDUCTION-FLAGS)
  (mhelp ""))

(defmenuitem COMPLETION-OPTIONS
  (display-name "COMPLETION-OPTIONS")
  (placement 2)
  (command "COMPLETION-OPTIONS")
  (parent ENTERING-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem HISTORY-SIZE
  (display-name "HISTORY-SIZE")
  (placement 3)
  (command "HISTORY-SIZE")
  (parent ENTERING-FLAGS)
  (etps t)
  (mhelp ""))

(defmenu RULES-OBJECT
  (display-name "Rules Object")
  (placement 2)
  (parent NATURAL-DEDUCTION-FLAGS)
  (mhelp ""))

(defmenuitem BUILD-MATCH
  (display-name "BUILD-MATCH")
  (placement 2)
  (command "BUILD-MATCH")
  (parent RULES-OBJECT)
  (mhelp ""))

(defmenuitem HLINE-JUSTIFICATION
  (display-name "HLINE-JUSTIFICATION")
  (placement 3)
  (command "HLINE-JUSTIFICATION")
  (parent RULES-OBJECT)
  (etps t)
  (mhelp ""))

(defmenuitem TREAT-HLINES-AS-DLINES
  (display-name "TREAT-HLINES-AS-DLINES")
  (placement 4)
  (command "TREAT-HLINES-AS-DLINES")
  (parent RULES-OBJECT)
  (etps t)
  (mhelp ""))

(defmenu PROOF-TRANSLATIONS
  (display-name "Proof Translations")
  (placement 2)
  (parent FLAGS)
  (mhelp ""))

(defmenu TACTIC-FLAGS
  (display-name "Tactic Flags")
  (placement 2)
  (parent PROOF-TRANSLATIONS)
  (mhelp ""))

(defmenuitem USE-SYMSIMP
  (display-name "USE-SYMSIMP")
  (placement 2)
  (command "USE-SYMSIMP")
  (parent TACTIC-FLAGS)
  (mhelp ""))

(defmenuitem UI-HERBRAND-LIMIT
  (display-name "UI-HERBRAND-LIMIT")
  (placement 3)
  (command "UI-HERBRAND-LIMIT")
  (parent TACTIC-FLAGS)
  (mhelp ""))

(defmenuitem DEFAULT-TACTIC
  (display-name "DEFAULT-TACTIC")
  (placement 4)
  (command "DEFAULT-TACTIC")
  (parent TACTIC-FLAGS)
  (mhelp ""))

(defmenuitem TACMODE
  (display-name "TACMODE")
  (placement 5)
  (command "TACMODE")
  (parent TACTIC-FLAGS)
  (mhelp ""))

(defmenuitem TACTIC-VERBOSE
  (display-name "TACTIC-VERBOSE")
  (placement 6)
  (command "TACTIC-VERBOSE")
  (parent TACTIC-FLAGS)
  (mhelp ""))

(defmenuitem TACUSE
  (display-name "TACUSE")
  (placement 7)
  (command "TACUSE")
  (parent TACTIC-FLAGS)
  (mhelp ""))

(defmenu ETREE-TO-NAT-FLAGS
  (display-name "Etree to Nat Flags")
  (placement 2)
  (parent PROOF-TRANSLATIONS)
  (mhelp ""))

(defmenuitem ETREE-NAT-VERBOSE
  (display-name "ETREE-NAT-VERBOSE")
  (placement 2)
  (command "ETREE-NAT-VERBOSE")
  (parent ETREE-TO-NAT-FLAGS)
  (mhelp ""))

(defmenuitem REMOVE-LEIBNIZ
  (display-name "REMOVE-LEIBNIZ")
  (placement 3)
  (command "REMOVE-LEIBNIZ")
  (parent ETREE-TO-NAT-FLAGS)
  (mhelp ""))

(defmenuitem MATINGSTREE-NAME
  (display-name "MATINGSTREE-NAME")
  (placement 4)
  (command "MATINGSTREE-NAME")
  (parent ETREE-TO-NAT-FLAGS)
  (mhelp ""))

(defmenuitem MERGE-MINIMIZE-MATING
  (display-name "MERGE-MINIMIZE-MATING")
  (placement 5)
  (command "MERGE-MINIMIZE-MATING")
  (parent ETREE-TO-NAT-FLAGS)
  (mhelp ""))

(defmenu NAT-TO-ETREE-FLAGS
  (display-name "Nat to Etree Flags")
  (placement 2)
  (parent PROOF-TRANSLATIONS)
  (mhelp ""))

(defmenuitem NAT-ETREE-VERSION
  (display-name "NAT-ETREE-VERSION")
  (placement 2)
  (command "NAT-ETREE-VERSION")
  (parent NAT-TO-ETREE-FLAGS)
  (mhelp ""))

(defmenuitem NATREE-DEBUG
  (display-name "NATREE-DEBUG")
  (placement 3)
  (command "NATREE-DEBUG")
  (parent NAT-TO-ETREE-FLAGS)
  (mhelp ""))

(defmenuitem RENUMBER-LEAVES
  (display-name "RENUMBER-LEAVES")
  (placement 4)
  (command "RENUMBER-LEAVES")
  (parent NAT-TO-ETREE-FLAGS)
  (mhelp ""))

(defmenu PRINTING-FLAGS
  (display-name "Printing Flags")
  (placement 2)
  (parent FLAGS)
  (mhelp ""))

(defmenuitem INFIX-NOTATION
  (display-name "INFIX-NOTATION")
  (placement 2)
  (command "INFIX-NOTATION")
  (parent PRINTING-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem RIGHTMARGIN
  (display-name "RIGHTMARGIN")
  (placement 3)
  (command "RIGHTMARGIN")
  (parent PRINTING-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem SCOPE
  (display-name "SCOPE")
  (placement 4)
  (command "SCOPE")
  (parent PRINTING-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem USE-DOT
  (display-name "USE-DOT")
  (placement 5)
  (command "USE-DOT")
  (parent PRINTING-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem PRINT-WEAK
  (display-name "PRINT-WEAK")
  (placement 6)
  (command "PRINT-WEAK")
  (parent PRINTING-FLAGS)
  (etps t)
  (mhelp ""))

(defmenu TEX
  (display-name "Tex")
  (placement 7)
  (parent PRINTING-FLAGS)
  (mhelp ""))

(defmenuitem LATEX-POSTAMBLE
  (display-name "LATEX-POSTAMBLE")
  (placement 2)
  (command "LATEX-POSTAMBLE")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem LATEX-PREAMBLE
  (display-name "LATEX-PREAMBLE")
  (placement 3)
  (command "LATEX-PREAMBLE")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem TEX-1-POSTAMBLE
  (display-name "TEX-1-POSTAMBLE")
  (placement 4)
  (command "TEX-1-POSTAMBLE")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem TEX-1-PREAMBLE
  (display-name "TEX-1-PREAMBLE")
  (placement 5)
  (command "TEX-1-PREAMBLE")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem TEX-LINE-WIDTH
  (display-name "TEX-LINE-WIDTH")
  (placement 6)
  (command "TEX-LINE-WIDTH")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem TEX-POSTAMBLE
  (display-name "TEX-POSTAMBLE")
  (placement 7)
  (command "TEX-POSTAMBLE")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem TEX-PREAMBLE
  (display-name "TEX-PREAMBLE")
  (placement 8)
  (command "TEX-PREAMBLE")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem TPSTEX
  (display-name "TPSTEX")
  (placement 9)
  (command "TPSTEX")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem VPDTEX
  (display-name "VPDTEX")
  (placement 10)
  (command "VPDTEX")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem IN-TEX-MATH-MODE
  (display-name "IN-TEX-MATH-MODE")
  (placement 11)
  (command "IN-TEX-MATH-MODE")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem LATEX-EMULATION
  (display-name "LATEX-EMULATION")
  (placement 12)
  (command "LATEX-EMULATION")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem PAGELENGTH
  (display-name "PAGELENGTH")
  (placement 13)
  (command "PAGELENGTH")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenuitem TEX-MIMIC-SCRIBE
  (display-name "TEX-MIMIC-SCRIBE")
  (placement 14)
  (command "TEX-MIMIC-SCRIBE")
  (parent TEX)
  (etps t)
  (mhelp ""))

(defmenu SCRIBE
  (display-name "Scribe")
  (placement 7)
  (parent PRINTING-FLAGS)
  (mhelp ""))

(defmenuitem SCRIBE-LINE-WIDTH
  (display-name "SCRIBE-LINE-WIDTH")
  (placement 2)
  (command "SCRIBE-LINE-WIDTH")
  (parent SCRIBE)
  (etps t)
  (mhelp ""))

(defmenuitem SCRIBE-POSTAMBLE
  (display-name "SCRIBE-POSTAMBLE")
  (placement 3)
  (command "SCRIBE-POSTAMBLE")
  (parent SCRIBE)
  (etps t)
  (mhelp ""))

(defmenuitem SCRIBE-PREAMBLE
  (display-name "SCRIBE-PREAMBLE")
  (placement 4)
  (command "SCRIBE-PREAMBLE")
  (parent SCRIBE)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTEDTFILE
  (display-name "PRINTEDTFILE")
  (placement 5)
  (command "PRINTEDTFILE")
  (parent SCRIBE)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTEDTFLAG
  (display-name "PRINTEDTFLAG")
  (placement 6)
  (command "PRINTEDTFLAG")
  (parent SCRIBE)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTEDTFLAG-SLIDES
  (display-name "PRINTEDTFLAG-SLIDES")
  (placement 7)
  (command "PRINTEDTFLAG-SLIDES")
  (parent SCRIBE)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTEDTOPS
  (display-name "PRINTEDTOPS")
  (placement 8)
  (command "PRINTEDTOPS")
  (parent SCRIBE)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTMATEFILE
  (display-name "PRINTMATEFILE")
  (placement 9)
  (command "PRINTMATEFILE")
  (parent SCRIBE)
  (mhelp ""))

(defmenuitem PRINTMATEFLAG
  (display-name "PRINTMATEFLAG")
  (placement 10)
  (command "PRINTMATEFLAG")
  (parent SCRIBE)
  (mhelp ""))

(defmenuitem PRINTMATEFLAG-SLIDES
  (display-name "PRINTMATEFLAG-SLIDES")
  (placement 11)
  (command "PRINTMATEFLAG-SLIDES")
  (parent SCRIBE)
  (mhelp ""))

(defmenuitem PRINTMATEOPS
  (display-name "PRINTMATEOPS")
  (placement 12)
  (command "PRINTMATEOPS")
  (parent SCRIBE)
  (mhelp ""))

(defmenu NATURAL-DEDUCTION-DISPLAY
  (display-name "Natural Deduction Display")
  (placement 7)
  (parent PRINTING-FLAGS)
  (mhelp ""))

(defmenuitem PRINT-COMBINED-EGENS
  (display-name "PRINT-COMBINED-EGENS")
  (placement 2)
  (command "PRINT-COMBINED-EGENS")
  (parent NATURAL-DEDUCTION-DISPLAY)
  (etps t)
  (mhelp ""))

(defmenuitem PRINT-COMBINED-UGENS
  (display-name "PRINT-COMBINED-UGENS")
  (placement 3)
  (command "PRINT-COMBINED-UGENS")
  (parent NATURAL-DEDUCTION-DISPLAY)
  (etps t)
  (mhelp ""))

(defmenuitem PRINT-COMBINED-UIS
  (display-name "PRINT-COMBINED-UIS")
  (placement 4)
  (command "PRINT-COMBINED-UIS")
  (parent NATURAL-DEDUCTION-DISPLAY)
  (etps t)
  (mhelp ""))

(defmenuitem PRINT-UNTIL-UI-OR-EGEN
  (display-name "PRINT-UNTIL-UI-OR-EGEN")
  (placement 5)
  (command "PRINT-UNTIL-UI-OR-EGEN")
  (parent NATURAL-DEDUCTION-DISPLAY)
  (etps t)
  (mhelp ""))

(defmenu MISC
  (display-name "Misc")
  (placement 7)
  (parent PRINTING-FLAGS)
  (mhelp ""))

(defmenuitem PRINT-COMMENTS
  (display-name "PRINT-COMMENTS")
  (placement 2)
  (command "PRINT-COMMENTS")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem ALLSCOPEFLAG
  (display-name "ALLSCOPEFLAG")
  (placement 3)
  (command "ALLSCOPEFLAG")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem ATOMVALFLAG
  (display-name "ATOMVALFLAG")
  (placement 4)
  (command "ATOMVALFLAG")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem BLANK-LINES-INSERTED
  (display-name "BLANK-LINES-INSERTED")
  (placement 5)
  (command "BLANK-LINES-INSERTED")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem CHARSIZE
  (display-name "CHARSIZE")
  (placement 6)
  (command "CHARSIZE")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem DISPLAYWFF
  (display-name "DISPLAYWFF")
  (placement 7)
  (command "DISPLAYWFF")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem ELIM-DEFNS
  (display-name "ELIM-DEFNS")
  (placement 8)
  (command "ELIM-DEFNS")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem FILLINEFLAG
  (display-name "FILLINEFLAG")
  (placement 9)
  (command "FILLINEFLAG")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem FIRST-PLACEMENT-PRINT-MODE
  (display-name "FIRST-PLACEMENT-PRINT-MODE")
  (placement 10)
  (command "FIRST-PLACEMENT-PRINT-MODE")
  (parent MISC)
  (mhelp ""))

(defmenuitem FLUSHLEFTFLAG
  (display-name "FLUSHLEFTFLAG")
  (placement 11)
  (command "FLUSHLEFTFLAG")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem LEFTMARGIN
  (display-name "LEFTMARGIN")
  (placement 12)
  (command "LEFTMARGIN")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem LOCALLEFTFLAG
  (display-name "LOCALLEFTFLAG")
  (placement 13)
  (command "LOCALLEFTFLAG")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem PPWFFLAG
  (display-name "PPWFFLAG")
  (placement 14)
  (command "PPWFFLAG")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTDEPTH
  (display-name "PRINTDEPTH")
  (placement 15)
  (command "PRINTDEPTH")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTTYPES
  (display-name "PRINTTYPES")
  (placement 16)
  (command "PRINTTYPES")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem PRINTTYPES-ALL
  (display-name "PRINTTYPES-ALL")
  (placement 17)
  (command "PRINTTYPES-ALL")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem RETAIN-INITIAL-TYPE
  (display-name "RETAIN-INITIAL-TYPE")
  (placement 18)
  (command "RETAIN-INITIAL-TYPE")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem SLIDES-TURNSTILE-INDENT
  (display-name "SLIDES-TURNSTILE-INDENT")
  (placement 19)
  (command "SLIDES-TURNSTILE-INDENT")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem SLIDES-TURNSTYLE-INDENT
  (display-name "SLIDES-TURNSTYLE-INDENT")
  (placement 20)
  (command "SLIDES-TURNSTYLE-INDENT")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem SLIDES-PREAMBLE
  (display-name "SLIDES-PREAMBLE")
  (placement 21)
  (command "SLIDES-PREAMBLE")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem SUPPORT-NUMBERS
  (display-name "SUPPORT-NUMBERS")
  (placement 22)
  (command "SUPPORT-NUMBERS")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem TURNSTILE-INDENT
  (display-name "TURNSTILE-INDENT")
  (placement 23)
  (command "TURNSTILE-INDENT")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem TURNSTILE-INDENT-AUTO
  (display-name "TURNSTILE-INDENT-AUTO")
  (placement 24)
  (command "TURNSTILE-INDENT-AUTO")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem TURNSTYLE-INDENT
  (display-name "TURNSTYLE-INDENT")
  (placement 25)
  (command "TURNSTYLE-INDENT")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem TURNSTYLE-INDENT-AUTO
  (display-name "TURNSTYLE-INDENT-AUTO")
  (placement 26)
  (command "TURNSTYLE-INDENT-AUTO")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenuitem USE-INTERNAL-PRINT-MODE
  (display-name "USE-INTERNAL-PRINT-MODE")
  (placement 27)
  (command "USE-INTERNAL-PRINT-MODE")
  (parent MISC)
  (etps t)
  (mhelp ""))

(defmenu PROOF-WINDOWS
  (display-name "Proof Windows")
  (placement 2)
  (parent FLAGS)
  (mhelp ""))

(defmenuitem PROOFW-ACTIVE
  (display-name "PROOFW-ACTIVE")
  (placement 2)
  (command "PROOFW-ACTIVE")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFW-ACTIVE+NOS
  (display-name "PROOFW-ACTIVE+NOS")
  (placement 3)
  (command "PROOFW-ACTIVE+NOS")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFW-ACTIVE+NOS-HEIGHT
  (display-name "PROOFW-ACTIVE+NOS-HEIGHT")
  (placement 4)
  (command "PROOFW-ACTIVE+NOS-HEIGHT")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFW-ACTIVE+NOS-WIDTH
  (display-name "PROOFW-ACTIVE+NOS-WIDTH")
  (placement 5)
  (command "PROOFW-ACTIVE+NOS-WIDTH")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFW-ACTIVE-HEIGHT
  (display-name "PROOFW-ACTIVE-HEIGHT")
  (placement 6)
  (command "PROOFW-ACTIVE-HEIGHT")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFW-ACTIVE-WIDTH
  (display-name "PROOFW-ACTIVE-WIDTH")
  (placement 7)
  (command "PROOFW-ACTIVE-WIDTH")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFW-ALL
  (display-name "PROOFW-ALL")
  (placement 8)
  (command "PROOFW-ALL")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFW-ALL-HEIGHT
  (display-name "PROOFW-ALL-HEIGHT")
  (placement 9)
  (command "PROOFW-ALL-HEIGHT")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFW-ALL-WIDTH
  (display-name "PROOFW-ALL-WIDTH")
  (placement 10)
  (command "PROOFW-ALL-WIDTH")
  (parent PROOF-WINDOWS)
  (etps t)
  (mhelp ""))

(defmenu MISC-FLAGS
  (display-name "Misc Flags")
  (placement 2)
  (parent FLAGS)
  (mhelp ""))

(defmenuitem SUPPRESS-FLAGS
  (display-name "SUPPRESS-FLAGS")
  (placement 2)
  (command "SUPPRESS-FLAGS")
  (parent MISC-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem SUPPRESS-FLAGS-LIST
  (display-name "SUPPRESS-FLAGS-LIST")
  (placement 3)
  (command "SUPPRESS-FLAGS-LIST")
  (parent MISC-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem LAST-MODE-NAME
  (display-name "LAST-MODE-NAME")
  (placement 4)
  (command "LAST-MODE-NAME")
  (parent MISC-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem ALPHA-LOWER-FLAG
  (display-name "ALPHA-LOWER-FLAG")
  (placement 5)
  (command "ALPHA-LOWER-FLAG")
  (parent MISC-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem SHOW-ALL-PACKAGES
  (display-name "SHOW-ALL-PACKAGES")
  (placement 6)
  (command "SHOW-ALL-PACKAGES")
  (parent MISC-FLAGS)
  (etps t)
  (mhelp ""))

(defmenu NAMING
  (display-name "Naming")
  (placement 7)
  (parent MISC-FLAGS)
  (mhelp ""))

(defmenuitem TRUE-NAME
  (display-name "TRUE-NAME")
  (placement 2)
  (command "TRUE-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem REWRITE-NAME
  (display-name "REWRITE-NAME")
  (placement 3)
  (command "REWRITE-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem NEG-NAME
  (display-name "NEG-NAME")
  (placement 4)
  (command "NEG-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem LEAF-NAME
  (display-name "LEAF-NAME")
  (placement 5)
  (command "LEAF-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem MATING-NAME
  (display-name "MATING-NAME")
  (placement 6)
  (command "MATING-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem FALSE-NAME
  (display-name "FALSE-NAME")
  (placement 7)
  (command "FALSE-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem IMP-NAME
  (display-name "IMP-NAME")
  (placement 8)
  (command "IMP-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem ECONJ-NAME
  (display-name "ECONJ-NAME")
  (placement 9)
  (command "ECONJ-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem EDISJ-NAME
  (display-name "EDISJ-NAME")
  (placement 10)
  (command "EDISJ-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem EMPTY-DUP-INFO-NAME
  (display-name "EMPTY-DUP-INFO-NAME")
  (placement 11)
  (command "EMPTY-DUP-INFO-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem EPROOF-NAME
  (display-name "EPROOF-NAME")
  (placement 12)
  (command "EPROOF-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem EXPANSION-NAME
  (display-name "EXPANSION-NAME")
  (placement 13)
  (command "EXPANSION-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem SELECTION-NAME
  (display-name "SELECTION-NAME")
  (placement 14)
  (command "SELECTION-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem SKOLEM-SELECTION-NAME
  (display-name "SKOLEM-SELECTION-NAME")
  (placement 15)
  (command "SKOLEM-SELECTION-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem NAME-SKOLEM-FN
  (display-name "NAME-SKOLEM-FN")
  (placement 16)
  (command "NAME-SKOLEM-FN")
  (parent NAMING)
  (mhelp ""))

(defmenuitem META-LABEL-NAME
  (display-name "META-LABEL-NAME")
  (placement 17)
  (command "META-LABEL-NAME")
  (parent NAMING)
  (etps t)
  (mhelp ""))

(defmenuitem LIT-NAME
  (display-name "LIT-NAME")
  (placement 18)
  (command "LIT-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenuitem VPD-LIT-NAME
  (display-name "VPD-LIT-NAME")
  (placement 19)
  (command "VPD-LIT-NAME")
  (parent NAMING)
  (mhelp ""))

(defmenu SAVING-FLAGS
  (display-name "Saving")
  (placement 7)
  (parent MISC-FLAGS)
  (mhelp ""))

(defmenuitem SAVE-INTERVAL
  (display-name "SAVE-INTERVAL")
  (placement 2)
  (command "SAVE-INTERVAL")
  (parent SAVING)
  (etps t)
  (mhelp ""))

(defmenuitem SAVE-WORK-ON-START-UP
  (display-name "SAVE-WORK-ON-START-UP")
  (placement 3)
  (command "SAVE-WORK-ON-START-UP")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem SAVE-WORK-P
  (display-name "SAVE-WORK-P")
  (placement 4)
  (command "SAVE-WORK-P")
  (parent SAVING)
  (etps t)
  (mhelp ""))

(defmenu MAINT
  (display-name "Maint")
  (placement 7)
  (parent MISC-FLAGS)
  (mhelp ""))

(defmenuitem COMPILED-EXTENSION
  (display-name "COMPILED-EXTENSION")
  (placement 2)
  (command "COMPILED-EXTENSION")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem EXPERTFLAG
  (display-name "EXPERTFLAG")
  (placement 3)
  (command "EXPERTFLAG")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem INIT-DIALOGUE
  (display-name "INIT-DIALOGUE")
  (placement 4)
  (command "INIT-DIALOGUE")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem INIT-DIALOGUE-FN
  (display-name "INIT-DIALOGUE-FN")
  (placement 5)
  (command "INIT-DIALOGUE-FN")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem LISP-IMPLEMENTATION-TYPE
  (display-name "LISP-IMPLEMENTATION-TYPE")
  (placement 6)
  (command "LISP-IMPLEMENTATION-TYPE")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem LOAD-WARN-P
  (display-name "LOAD-WARN-P")
  (placement 7)
  (command "LOAD-WARN-P")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem MACHINE-INSTANCE
  (display-name "MACHINE-INSTANCE")
  (placement 8)
  (command "MACHINE-INSTANCE")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem MACHINE-TYPE
  (display-name "MACHINE-TYPE")
  (placement 9)
  (command "MACHINE-TYPE")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem NEWS-DIR
  (display-name "NEWS-DIR")
  (placement 10)
  (command "NEWS-DIR")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem READ-LLOAD-SOURCES-P
  (display-name "READ-LLOAD-SOURCES-P")
  (placement 11)
  (command "READ-LLOAD-SOURCES-P")
  (parent MAINT)
  (mhelp ""))

(defmenuitem SAVE-FILE
  (display-name "SAVE-FILE")
  (placement 12)
  (command "SAVE-FILE")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem SHORT-SITE-NAME
  (display-name "SHORT-SITE-NAME")
  (placement 13)
  (command "SHORT-SITE-NAME")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem SOURCE-EXTENSION
  (display-name "SOURCE-EXTENSION")
  (placement 14)
  (command "SOURCE-EXTENSION")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem SOURCE-PATH
  (display-name "SOURCE-PATH")
  (placement 15)
  (command "SOURCE-PATH")
  (parent MAINT)
  (etps t)
  (mhelp ""))

(defmenuitem TEST-MODIFY
  (display-name "TEST-MODIFY")
  (placement 16)
  (command "TEST-MODIFY")
  (parent MAINT)
  (mhelp ""))

(defmenuitem TEST-THEOREMS
  (display-name "TEST-THEOREMS")
  (placement 17)
  (command "TEST-THEOREMS")
  (parent MAINT)
  (mhelp ""))

(defmenu EVENTS
  (display-name "Events")
  (placement 7)
  (parent MISC-FLAGS)
  (mhelp ""))

(defmenuitem ADVICE-ASKED-ENABLED0
  (display-name "ADVICE-ASKED-ENABLED")
  (placement 2)
  (command "ADVICE-ASKED-ENABLED")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem ADVICE-FILE
  (display-name "ADVICE-FILE")
  (placement 3)
  (command "ADVICE-FILE")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem COMMAND-ENABLED0
  (display-name "COMMAND-ENABLED")
  (placement 4)
  (command "COMMAND-ENABLED")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem COMMAND-FILE
  (display-name "COMMAND-FILE")
  (placement 5)
  (command "COMMAND-FILE")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem DONE-EXC-ENABLED0
  (display-name "DONE-EXC-ENABLED")
  (placement 6)
  (command "DONE-EXC-ENABLED")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem ERROR-ENABLED0
  (display-name "ERROR-ENABLED")
  (placement 7)
  (command "ERROR-ENABLED")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem ERROR-FILE
  (display-name "ERROR-FILE")
  (placement 8)
  (command "ERROR-FILE")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem EVENT-CYCLE
  (display-name "EVENT-CYCLE")
  (placement 9)
  (command "EVENT-CYCLE")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem EVENTS-ENABLED0
  (display-name "EVENTS-ENABLED")
  (placement 10)
  (command "EVENTS-ENABLED")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem INPUT-ERROR-ENABLED0
  (display-name "INPUT-ERROR-ENABLED")
  (placement 11)
  (command "INPUT-ERROR-ENABLED")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem INPUT-ERROR-FILE
  (display-name "INPUT-ERROR-FILE")
  (placement 12)
  (command "INPUT-ERROR-FILE")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOF-ACTION-ENABLED0
  (display-name "PROOF-ACTION-ENABLED")
  (placement 13)
  (command "PROOF-ACTION-ENABLED")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem PROOF-FILE
  (display-name "PROOF-FILE")
  (placement 14)
  (command "PROOF-FILE")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem QUIET-EVENTS
  (display-name "QUIET-EVENTS")
  (placement 15)
  (command "QUIET-EVENTS")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem RULE-ERROR-ENABLED0
  (display-name "RULE-ERROR-ENABLED")
  (placement 16)
  (command "RULE-ERROR-ENABLED")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem RULE-ERROR-FILE
  (display-name "RULE-ERROR-FILE")
  (placement 17)
  (command "RULE-ERROR-FILE")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenuitem SCORE-FILE
  (display-name "SCORE-FILE")
  (placement 18)
  (command "SCORE-FILE")
  (parent EVENTS)
  (etps t)
  (mhelp ""))

(defmenu PARSING
  (display-name "Parsing")
  (placement 7)
  (parent MISC-FLAGS)
  (mhelp ""))

(defmenuitem BASE-TYPE
  (display-name "BASE-TYPE")
  (placement 2)
  (command "BASE-TYPE")
  (parent PARSING)
  (etps t)
  (mhelp ""))

(defmenuitem FIRST-PLACEMENT-MODE-PARSE
  (display-name "FIRST-PLACEMENT-MODE-PARSE")
  (placement 3)
  (command "FIRST-PLACEMENT-MODE-PARSE")
  (parent PARSING)
  (mhelp ""))

(defmenuitem LOWERCASERAISE
  (display-name "LOWERCASERAISE")
  (placement 4)
  (command "LOWERCASERAISE")
  (parent PARSING)
  (etps t)
  (mhelp ""))

(defmenuitem TYPE-IOTA-MODE
  (display-name "TYPE-IOTA-MODE")
  (placement 5)
  (command "TYPE-IOTA-MODE")
  (parent PARSING)
  (etps t)
  (mhelp ""))

(defmenuitem UNTYPED-LAMBDA-CALCULUS
  (display-name "UNTYPED-LAMBDA-CALCULUS")
  (placement 6)
  (command "UNTYPED-LAMBDA-CALCULUS")
  (parent PARSING)
  (etps t)
  (mhelp ""))

(defmenuitem MAKE-WFFOPS-LABELS
  (display-name "MAKE-WFFOPS-LABELS")
  (placement 7)
  (command "MAKE-WFFOPS-LABELS")
  (parent PARSING)
  (etps t)
  (mhelp ""))

(defmenuitem PRINT-META
  (display-name "PRINT-META")
  (placement 8)
  (command "PRINT-META")
  (parent PARSING)
  (etps t)
  (mhelp ""))

(defmenu VARS
  (display-name "Vars")
  (placement 7)
  (parent MISC-FLAGS)
  (mhelp ""))

(defmenuitem META-BDVAR-NAME
  (display-name "META-BDVAR-NAME")
  (placement 2)
  (command "META-BDVAR-NAME")
  (parent VARS)
  (etps t)
  (mhelp ""))

(defmenuitem META-VAR-NAME
  (display-name "META-VAR-NAME")
  (placement 3)
  (command "META-VAR-NAME")
  (parent VARS)
  (etps t)
  (mhelp ""))

(defmenuitem REN-VAR-FN
  (display-name "REN-VAR-FN")
  (placement 4)
  (command "REN-VAR-FN")
  (parent VARS)
  (etps t)
  (mhelp ""))

(defmenuitem RENAME-ALL-BD-VARS
  (display-name "RENAME-ALL-BD-VARS")
  (placement 5)
  (command "RENAME-ALL-BD-VARS")
  (parent VARS)
  (etps t)
  (mhelp ""))

(defmenu EDITOR-FLAGS
  (display-name "Editor Flags")
  (placement 7)
  (parent MISC-FLAGS)
  (mhelp ""))

(defmenuitem EDPPWFFLAG
  (display-name "EDPPWFFLAG")
  (placement 2)
  (command "EDPPWFFLAG")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDPRINTDEPTH
  (display-name "EDPRINTDEPTH")
  (placement 3)
  (command "EDPRINTDEPTH")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-CURRENT
  (display-name "EDWIN-CURRENT")
  (placement 4)
  (command "EDWIN-CURRENT")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-CURRENT-HEIGHT
  (display-name "EDWIN-CURRENT-HEIGHT")
  (placement 5)
  (command "EDWIN-CURRENT-HEIGHT")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-CURRENT-WIDTH
  (display-name "EDWIN-CURRENT-WIDTH")
  (placement 6)
  (command "EDWIN-CURRENT-WIDTH")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-TOP
  (display-name "EDWIN-TOP")
  (placement 7)
  (command "EDWIN-TOP")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-TOP-HEIGHT
  (display-name "EDWIN-TOP-HEIGHT")
  (placement 8)
  (command "EDWIN-TOP-HEIGHT")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-TOP-WIDTH
  (display-name "EDWIN-TOP-WIDTH")
  (placement 9)
  (command "EDWIN-TOP-WIDTH")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-VPFORM
  (display-name "EDWIN-VPFORM")
  (placement 10)
  (command "EDWIN-VPFORM")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-VPFORM-HEIGHT
  (display-name "EDWIN-VPFORM-HEIGHT")
  (placement 11)
  (command "EDWIN-VPFORM-HEIGHT")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem EDWIN-VPFORM-WIDTH
  (display-name "EDWIN-VPFORM-WIDTH")
  (placement 12)
  (command "EDWIN-VPFORM-WIDTH")
  (parent EDITOR-FLAGS)
  (etps t)
  (mhelp ""))

(defmenuitem SAME
  (display-name "SAME")
  (placement 2)
  (command "SAME")
  (parent RULES)
  (etps t)
  (mhelp ""))

(defmenu PROPOSITIONAL
  (display-name "Propositional")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem RULE-P
  (display-name "RULEP")
  (placement 2)
  (command "RULEP")
  (parent PROPOSITIONAL)
  (etps t)
  (mhelp ""))

(defmenu INDIRECT-RULES
  (display-name "Indirect")
  (placement 3)
  (parent PROPOSITIONAL)
  (mhelp ""))

(defmenuitem INDIRECT
  (display-name "INDIRECT")
  (placement 2)
  (command "INDIRECT")
  (parent INDIRECT-RULES)
  (etps t)
  (mhelp ""))

(defmenuitem INDIRECT1
  (display-name "INDIRECT1")
  (placement 3)
  (command "INDIRECT1")
  (parent INDIRECT-RULES)
  (etps t)
  (mhelp ""))

(defmenuitem INDIRECT2
  (display-name "INDIRECT2")
  (placement 4)
  (command "INDIRECT2")
  (parent INDIRECT-RULES)
  (etps t)
  (mhelp ""))

(defmenu NEGATION
  (display-name "Negation")
  (placement 3)
  (parent PROPOSITIONAL)
  (mhelp ""))

(defmenuitem ENEG
  (display-name "ENEG")
  (placement 2)
  (command "ENEG")
  (parent NEGATION)
  (etps t)
  (mhelp ""))

(defmenuitem INEG
  (display-name "INEG")
  (placement 3)
  (command "INEG")
  (parent NEGATION)
  (etps t)
  (mhelp ""))

(defmenuitem NNF
  (display-name "NNF")
  (placement 4)
  (command "NNF")
  (parent NEGATION)
  (etps t)
  (mhelp ""))

(defmenuitem NNF-EXPAND
  (display-name "NNF-EXPAND")
  (placement 5)
  (command "NNF-EXPAND")
  (parent NEGATION)
  (etps t)
  (mhelp ""))

(defmenuitem PULLNEG
  (display-name "PULLNEG")
  (placement 6)
  (command "PULLNEG")
  (parent NEGATION)
  (etps t)
  (mhelp ""))

(defmenuitem PUSHNEG
  (display-name "PUSHNEG")
  (placement 7)
  (command "PUSHNEG")
  (parent NEGATION)
  (etps t)
  (mhelp ""))

(defmenu DISJUNCTION
  (display-name "Disjunction")
  (placement 3)
  (parent PROPOSITIONAL)
  (mhelp ""))

(defmenuitem CASES
  (display-name "CASES")
  (placement 2)
  (command "CASES")
  (parent DISJUNCTION)
  (etps t)
  (mhelp ""))

(defmenuitem CASES3
  (display-name "CASES3")
  (placement 3)
  (command "CASES3")
  (parent DISJUNCTION)
  (etps t)
  (mhelp ""))

(defmenuitem CASES4
  (display-name "CASES4")
  (placement 4)
  (command "CASES4")
  (parent DISJUNCTION)
  (etps t)
  (mhelp ""))

(defmenuitem IDISJ-LEFT
  (display-name "IDISJ-LEFT")
  (placement 5)
  (command "IDISJ-LEFT")
  (parent DISJUNCTION)
  (etps t)
  (mhelp ""))

(defmenuitem IDISJ-RIGHT
  (display-name "IDISJ-RIGHT")
  (placement 6)
  (command "IDISJ-RIGHT")
  (parent DISJUNCTION)
  (etps t)
  (mhelp ""))

(defmenuitem IMP-DISJ
  (display-name "IMP-DISJ")
  (placement 7)
  (command "IMP-DISJ")
  (parent DISJUNCTION)
  (etps t)
  (mhelp ""))

(defmenuitem IMP-DISJ-L
  (display-name "IMP-DISJ-L")
  (placement 8)
  (command "IMP-DISJ-L")
  (parent DISJUNCTION)
  (etps t)
  (mhelp ""))

(defmenuitem IMP-DISJ-R
  (display-name "IMP-DISJ-R")
  (placement 9)
  (command "IMP-DISJ-R")
  (parent DISJUNCTION)
  (etps t)
  (mhelp ""))

(defmenu CONJUNCTION
  (display-name "Conjunction")
  (placement 3)
  (parent PROPOSITIONAL)
  (mhelp ""))

(defmenuitem ECONJ
  (display-name "ECONJ")
  (placement 2)
  (command "ECONJ")
  (parent CONJUNCTION)
  (etps t)
  (mhelp ""))

(defmenuitem ICONJ
  (display-name "ICONJ")
  (placement 3)
  (command "ICONJ")
  (parent CONJUNCTION)
  (etps t)
  (mhelp ""))

(defmenu IMPLICATION
  (display-name "Implication")
  (placement 3)
  (parent PROPOSITIONAL)
  (mhelp ""))

(defmenuitem DEDUCT
  (display-name "DEDUCT")
  (placement 2)
  (command "DEDUCT")
  (parent IMPLICATION)
  (etps t)
  (mhelp ""))

(defmenuitem MP
  (display-name "MP")
  (placement 3)
  (command "MP")
  (parent IMPLICATION)
  (etps t)
  (mhelp ""))

(defmenuitem DISJ-IMP
  (display-name "DISJ-IMP")
  (placement 4)
  (command "DISJ-IMP")
  (parent IMPLICATION)
  (etps t)
  (mhelp ""))

(defmenuitem DISJ-IMP-L
  (display-name "DISJ-IMP-L")
  (placement 5)
  (command "DISJ-IMP-L")
  (parent IMPLICATION)
  (etps t)
  (mhelp ""))

(defmenuitem DISJ-IMP-R
  (display-name "DISJ-IMP-R")
  (placement 6)
  (command "DISJ-IMP-R")
  (parent IMPLICATION)
  (etps t)
  (mhelp ""))

(defmenu EQUIVALENCE
  (display-name "Equivalence")
  (placement 3)
  (parent PROPOSITIONAL)
  (mhelp ""))

(defmenuitem EQUIV-IMPLICS
  (display-name "EQUIV-IMPLICS")
  (placement 2)
  (command "EQUIV-IMPLICS")
  (parent EQUIVALENCE)
  (etps t)
  (mhelp ""))

(defmenuitem IMPLICS-EQUIV
  (display-name "IMPLICS-EQUIV")
  (placement 3)
  (command "IMPLICS-EQUIV")
  (parent EQUIVALENCE)
  (etps t)
  (mhelp ""))

(defmenuitem SUBST-EQUIV
  (display-name "SUBST-EQUIV")
  (placement 4)
  (command "SUBST-EQUIV")
  (parent EQUIVALENCE)
  (etps t)
  (mhelp ""))

(defmenuitem ABSURD
  (display-name "ABSURD")
  (placement 3)
  (command "ABSURD")
  (parent PROPOSITIONAL)
  (etps t)
  (mhelp ""))

(defmenuitem ITRUTH
  (display-name "ITRUTH")
  (placement 4)
  (command "ITRUTH")
  (parent PROPOSITIONAL)
  (etps t)
  (mhelp ""))

(defmenuitem ASSOC-LEFT
  (display-name "ASSOC-LEFT")
  (placement 5)
  (command "ASSOC-LEFT")
  (parent PROPOSITIONAL)
  (etps t)
  (mhelp ""))

(defmenu QUANTIFIERS
  (display-name "Quantifiers")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem AB*
  (display-name "AB*")
  (placement 2)
  (command "AB*")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenuitem ABE
  (display-name "ABE")
  (placement 3)
  (command "ABE")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenuitem ABU
  (display-name "ABU")
  (placement 4)
  (command "ABU")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenuitem UGEN
  (display-name "UGEN")
  (placement 5)
  (command "UGEN")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenuitem UI
  (display-name "UI")
  (placement 6)
  (command "UI")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenuitem EGEN
  (display-name "EGEN")
  (placement 7)
  (command "EGEN")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenuitem RULEC
  (display-name "RULEC")
  (placement 8)
  (command "RULEC")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenuitem RULEC1
  (display-name "RULEC1")
  (placement 9)
  (command "RULEC1")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenuitem LET
  (display-name "LET")
  (placement 10)
  (command "LET")
  (parent QUANTIFIERS)
  (etps t)
  (mhelp ""))

(defmenu LAMBDA
  (display-name "Lambda")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem BETA*
  (display-name "BETA*")
  (placement 2)
  (command "BETA*")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenuitem ETA*
  (display-name "ETA*")
  (placement 3)
  (command "ETA*")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenuitem LAMBDA*
  (display-name "LAMBDA*")
  (placement 4)
  (command "LAMBDA*")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenuitem LCONTR*
  (display-name "LCONTR*")
  (placement 5)
  (command "LCONTR*")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenuitem LCONTR*-BETA
  (display-name "LCONTR*-BETA")
  (placement 6)
  (command "LCONTR*-BETA")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenuitem LCONTR*-ETA
  (display-name "LCONTR*-ETA")
  (placement 7)
  (command "LCONTR*-ETA")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenuitem LEXPD*
  (display-name "LEXPD*")
  (placement 8)
  (command "LEXPD*")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenuitem LEXPD*-BETA
  (display-name "LEXPD*-BETA")
  (placement 9)
  (command "LEXPD*-BETA")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenuitem LEXPD*-ETA
  (display-name "LEXPD*-ETA")
  (placement 10)
  (command "LEXPD*-ETA")
  (parent LAMBDA)
  (etps t)
  (mhelp ""))

(defmenu DEFINITIONS
  (display-name "Definitions")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem EQUIV-WFFS
  (display-name "EQUIV-WFFS")
  (placement 2)
  (command "EQUIV-WFFS")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem IDEF
  (display-name "IDEF")
  (placement 3)
  (command "IDEF")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem EDEF
  (display-name "EDEF")
  (placement 4)
  (command "EDEF")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem EQUIV-EQ
  (display-name "EQUIV-EQ")
  (placement 5)
  (command "EQUIV-EQ")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem EQUIV-EQ-CONTR
  (display-name "EQUIV-EQ-CONTR")
  (placement 6)
  (command "EQUIV-EQ-CONTR")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem EQUIV-EQ-CONTR*
  (display-name "EQUIV-EQ-CONTR*")
  (placement 7)
  (command "EQUIV-EQ-CONTR*")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem EQUIV-EQ-EXPD
  (display-name "EQUIV-EQ-EXPD")
  (placement 8)
  (command "EQUIV-EQ-EXPD")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem EQUIV-EQ-EXPD*
  (display-name "EQUIV-EQ-EXPD*")
  (placement 9)
  (command "EQUIV-EQ-EXPD*")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem EXT=
  (display-name "EXT=")
  (placement 10)
  (command "EXT=")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem EXT=0
  (display-name "EXT=0")
  (placement 11)
  (command "EXT=0")
  (parent DEFINITIONS)
  (etps t)
  (mhelp ""))

(defmenu SUBSTITIONS
  (display-name "Substitions")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem SUBSTITUTE
  (display-name "SUBSTITUTE")
  (placement 2)
  (command "SUBSTITUTE")
  (parent SUBSTITIONS)
  (etps t)
  (mhelp ""))

(defmenuitem TYPESUBST
  (display-name "TYPESUBST")
  (placement 3)
  (command "TYPESUBST")
  (parent SUBSTITIONS)
  (etps t)
  (mhelp ""))

(defmenu EQUATIONS
  (display-name "Equations")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem SUBST=
  (display-name "SUBST=")
  (placement 2)
  (command "SUBST=")
  (parent EQUATIONS)
  (etps t)
  (mhelp ""))

(defmenuitem SUBST=L
  (display-name "SUBST=L")
  (placement 3)
  (command "SUBST=L")
  (parent EQUATIONS)
  (etps t)
  (mhelp ""))

(defmenuitem SUBST=R
  (display-name "SUBST=R")
  (placement 4)
  (command "SUBST=R")
  (parent EQUATIONS)
  (etps t)
  (mhelp ""))

(defmenuitem SYM=
  (display-name "SYM=")
  (placement 5)
  (command "SYM=")
  (parent EQUATIONS)
  (etps t)
  (mhelp ""))

(defmenu REWRITE-RULES
  (display-name "Rewrite Rules")
  (placement 6)
  (parent EQUATIONS)
  (mhelp ""))

(defmenuitem ACTIVATE-RULES
  (display-name "ACTIVATE-RULES")
  (placement 2)
  (command "ACTIVATE-RULES")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem DEACTIVATE-RULES
  (display-name "DEACTIVATE-RULES")
  (placement 3)
  (command "DEACTIVATE-RULES")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem DELETE-RRULE
  (display-name "DELETE-RRULE")
  (placement 4)
  (command "DELETE-RRULE")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem LIST-RRULES
  (display-name "LIST-RRULES")
  (placement 5)
  (command "LIST-RRULES")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem MAKE-ABBREV-RRULE
  (display-name "MAKE-ABBREV-RRULE")
  (placement 6)
  (command "MAKE-ABBREV-RRULE")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem MAKE-INVERSE-RRULE
  (display-name "MAKE-INVERSE-RRULE")
  (placement 7)
  (command "MAKE-INVERSE-RRULE")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem MAKE-THEORY
  (display-name "MAKE-THEORY")
  (placement 8)
  (command "MAKE-THEORY")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem PERMUTE-RRULES
  (display-name "PERMUTE-RRULES")
  (placement 9)
  (command "PERMUTE-RRULES")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem REWRITE-SUPP*
  (display-name "REWRITE-SUPP*")
  (placement 10)
  (command "REWRITE-SUPP*")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem REWRITE-SUPP1
  (display-name "REWRITE-SUPP1")
  (placement 11)
  (command "REWRITE-SUPP1")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem SIMPLIFY-PLAN
  (display-name "SIMPLIFY-PLAN")
  (placement 12)
  (command "SIMPLIFY-PLAN")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem SIMPLIFY-PLAN*
  (display-name "SIMPLIFY-PLAN*")
  (placement 13)
  (command "SIMPLIFY-PLAN*")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem SIMPLIFY-SUPP
  (display-name "SIMPLIFY-SUPP")
  (placement 14)
  (command "SIMPLIFY-SUPP")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem SIMPLIFY-SUPP*
  (display-name "SIMPLIFY-SUPP*")
  (placement 15)
  (command "SIMPLIFY-SUPP*")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem UNREWRITE-PLAN*
  (display-name "UNREWRITE-PLAN*")
  (placement 16)
  (command "UNREWRITE-PLAN*")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem UNREWRITE-PLAN1
  (display-name "UNREWRITE-PLAN1")
  (placement 17)
  (command "UNREWRITE-PLAN1")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem USE-RRULES
  (display-name "USE-RRULES")
  (placement 18)
  (command "USE-RRULES")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenuitem USE-THEORY
  (display-name "USE-THEORY")
  (placement 19)
  (command "USE-THEORY")
  (parent REWRITE-RULES)
  (mhelp ""))

(defmenu MODIFY
  (display-name "Modify")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem ASSERT
  (display-name "ASSERT")
  (placement 2)
  (command "ASSERT")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem HYP
  (display-name "HYP")
  (placement 3)
  (command "HYP")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem LEMMA
  (display-name "LEMMA")
  (placement 4)
  (command "LEMMA")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem ADD-HYPS
  (display-name "ADD-HYPS")
  (placement 5)
  (command "ADD-HYPS")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem DELETE
  (display-name "DELETE")
  (placement 6)
  (command "DELETE")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem DELETE*
  (display-name "DELETE*")
  (placement 7)
  (command "DELETE*")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem DELETE-HYPS
  (display-name "DELETE-HYPS")
  (placement 8)
  (command "DELETE-HYPS")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem INTRODUCE-GAP
  (display-name "INTRODUCE-GAP")
  (placement 9)
  (command "INTRODUCE-GAP")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem MAKE-ASSERT-A-HYP
  (display-name "MAKE-ASSERT-A-HYP")
  (placement 10)
  (command "MAKE-ASSERT-A-HYP")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem ELIMINATE-ALL-RULEP-APPS
  (display-name "ELIMINATE-ALL-RULEP-APPS")
  (placement 11)
  (command "ELIMINATE-ALL-RULEP-APPS")
  (parent MODIFY)
  (mhelp ""))

(defmenuitem ELIMINATE-CONJ*-RULEP-APPS
  (display-name "ELIMINATE-CONJ*-RULEP-APPS")
  (placement 12)
  (command "ELIMINATE-CONJ*-RULEP-APPS")
  (parent MODIFY)
  (mhelp ""))

(defmenuitem ELIMINATE-RULEP-LINE
  (display-name "ELIMINATE-RULEP-LINE")
  (placement 13)
  (command "ELIMINATE-RULEP-LINE")
  (parent MODIFY)
  (mhelp ""))

(defmenuitem LOCK-LINE
  (display-name "LOCK-LINE")
  (placement 14)
  (command "LOCK-LINE")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem MODIFY-GAPS
  (display-name "MODIFY-GAPS")
  (placement 15)
  (command "MODIFY-GAPS")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem MOVE
  (display-name "MOVE")
  (placement 16)
  (command "MOVE")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem MOVE*
  (display-name "MOVE*")
  (placement 17)
  (command "MOVE*")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem PLAN
  (display-name "PLAN")
  (placement 18)
  (command "PLAN")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem RENUMBERALL
  (display-name "RENUMBERALL")
  (placement 19)
  (command "RENUMBERALL")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem SQUEEZE
  (display-name "SQUEEZE")
  (placement 20)
  (command "SQUEEZE")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem UNLOCK-LINE
  (display-name "UNLOCK-LINE")
  (placement 21)
  (command "UNLOCK-LINE")
  (parent MODIFY)
  (etps t)
  (mhelp ""))

(defmenuitem DE-ASSERT-LEMMAS
  (display-name "DEASSERT-LEMMAS")
  (placement 22)
  (command "DEASSERT-LEMMAS")
  (parent MODIFY)
  (mhelp ""))

(defmenuitem NORMALIZE-PROOF
  (display-name "NORMALIZE-PROOF")
  (placement 23)
  (command "NORMALIZE-PROOF")
  (parent MODIFY)
  (mhelp ""))

(defmenu STATUS
  (display-name "Status")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem ARE-WE-USING
  (display-name "ARE-WE-USING")
  (placement 2)
  (command "ARE-WE-USING")
  (parent STATUS)
  (etps t)
  (mhelp ""))

(defmenuitem COUNT-LINES
  (display-name "COUNT-LINES")
  (placement 3)
  (command "COUNT-LINES")
  (parent STATUS)
  (etps t)
  (mhelp ""))

(defmenuitem PSTATUS
  (display-name "PSTATUS")
  (placement 4)
  (command "PSTATUS")
  (parent STATUS)
  (etps t)
  (mhelp ""))

(defmenuitem SPONSOR
  (display-name "SPONSOR")
  (placement 5)
  (command "SPONSOR")
  (parent STATUS)
  (etps t)
  (mhelp ""))

(defmenuitem SUBPROOF
  (display-name "SUBPROOF")
  (placement 6)
  (command "SUBPROOF")
  (parent STATUS)
  (etps t)
  (mhelp ""))

(defmenuitem UNSPONSOR
  (display-name "UNSPONSOR")
  (placement 7)
  (command "UNSPONSOR")
  (parent STATUS)
  (etps t)
  (mhelp ""))

(defmenu SUGGESTIONS
  (display-name "Suggestions")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem ADVICE
  (display-name "ADVICE")
  (placement 2)
  (command "ADVICE")
  (parent SUGGESTIONS)
  (etps t)
  (mhelp ""))

(defmenuitem CHECK-STRUCTURE
  (display-name "CHECK-STRUCTURE")
  (placement 3)
  (command "CHECK-STRUCTURE")
  (parent SUGGESTIONS)
  (etps t)
  (mhelp ""))

(defmenuitem GO
  (display-name "GO")
  (placement 4)
  (command "GO")
  (parent SUGGESTIONS)
  (mhelp ""))

(defmenuitem GO2
  (display-name "GO2")
  (placement 5)
  (command "GO2")
  (parent SUGGESTIONS)
  (mhelp ""))

(defmenuitem MONSTRO
  (display-name "MONSTRO")
  (placement 6)
  (command "MONSTRO")
  (parent SUGGESTIONS)
  (mhelp ""))

(defmenuitem SUGGEST
  (display-name "SUGGEST")
  (placement 7)
  (command "SUGGEST")
  (parent SUGGESTIONS)
  (mhelp ""))

(defmenu PRINTING
  (display-name "Printing")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem BUILD-PROOF-HIERARCHY
  (display-name "BUILD-PROOF-HIERARCHY")
  (placement 2)
  (command "BUILD-PROOF-HIERARCHY")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem DEPTH
  (display-name "DEPTH")
  (placement 3)
  (command "DEPTH")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem EXPLAIN
  (display-name "EXPLAIN")
  (placement 4)
  (command "EXPLAIN")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem FIND-LINE
  (display-name "FIND-LINE")
  (placement 5)
  (command "FIND-LINE")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PALL1
  (display-name "PALL (Display Proof)")
  (placement 6)
  (command "PALL")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PBRIEF
  (display-name "PBRIEF")
  (placement 7)
  (command "PBRIEF")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PL
  (display-name "PL")
  (placement 8)
  (command "PL")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PL*
  (display-name "PL*")
  (placement 9)
  (command "PL*")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PLINE
  (display-name "PLINE")
  (placement 10)
  (command "PLINE")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PPLAN
  (display-name "PPLAN")
  (placement 11)
  (command "PPLAN")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PRINT-PROOF-STRUCTURE
  (display-name "PRINT-PROOF-STRUCTURE")
  (placement 12)
  (command "PRINT-PROOF-STRUCTURE")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PRW
  (display-name "PRW")
  (placement 13)
  (command "PRW")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PW
  (display-name "PW")
  (placement 14)
  (command "PW")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PWSCOPE
  (display-name "PWSCOPE")
  (placement 15)
  (command "PWSCOPE")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem PWTYPES
  (display-name "PWTYPES")
  (placement 16)
  (command "PWTYPES")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem SHOWNOTYPES
  (display-name "SHOWNOTYPES")
  (placement 17)
  (command "SHOWNOTYPES")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem SHOWTYPES
  (display-name "SHOWTYPES")
  (placement 18)
  (command "SHOWTYPES")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem TABLEAU
  (display-name "TABLEAU")
  (placement 19)
  (command "TABLEAU")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem ^P
  (display-name "^P")
  (placement 20)
  (command "^P")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenuitem ^PN
  (display-name "^PN")
  (placement 21)
  (command "^PN")
  (parent PRINTING)
  (etps t)
  (mhelp ""))

(defmenu ENTERING
  (display-name "Entering")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem CLEANUP
  (display-name "CLEANUP")
  (placement 2)
  (command "CLEANUP")
  (parent ENTERING)
  (etps t)
  (mhelp ""))

(defmenuitem DONE
  (display-name "DONE")
  (placement 3)
  (command "DONE")
  (parent ENTERING)
  (etps t)
  (mhelp ""))

(defmenuitem NEWS
  (display-name "NEWS")
  (placement 5)
  (command "NEWS")
  (parent ENTERING)
  (etps t)
  (mhelp ""))

(defmenuitem RECONSIDER
  (display-name "RECONSIDER")
  (placement 6)
  (command "RECONSIDER")
  (parent ENTERING)
  (etps t)
  (mhelp ""))

(defmenuitem REMARK
  (display-name "REMARK")
  (placement 7)
  (command "REMARK")
  (parent ENTERING)
  (etps t)
  (mhelp ""))

(defmenuitem SUMMARY
  (display-name "SUMMARY")
  (placement 8)
  (command "SUMMARY")
  (parent ENTERING)
  (etps t)
  (mhelp ""))

(defmenuitem ALIAS
  (display-name "ALIAS")
  (placement 9)
  (command "ALIAS")
  (parent ENTERING)
  (etps t)
  (mhelp ""))

(defmenuitem UNALIAS
  (display-name "UNALIAS")
  (placement 10)
  (command "UNALIAS")
  (parent ENTERING)
  (etps t)
  (mhelp ""))

(defmenu TACTICS
  (display-name "Tactics")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem ECHO
  (display-name "ECHO")
  (placement 2)
  (command "ECHO")
  (parent TACTICS)
  (mhelp ""))

(defmenuitem USE-TACTIC
  (display-name "USE-TACTIC")
  (placement 3)
  (command "USE-TACTIC")
  (parent TACTICS)
  (mhelp ""))

(defmenu ETREE-TO-NAT
  (display-name "Etree to Nat")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem ETREE-NAT
  (display-name "ETREE-NAT")
  (placement 2)
  (command "ETREE-NAT")
  (parent ETREE-TO-NAT)
  (mhelp ""))

(defmenuitem TIDY-PROOF
  (display-name "TIDY-PROOF")
  (placement 3)
  (command "TIDY-PROOF")
  (parent ETREE-TO-NAT)
  (mhelp ""))

(defmenu NAT-TO-ETREE
  (display-name "Nat to Etree")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem NAT-ETREE
  (display-name "NAT-ETREE")
  (placement 2)
  (command "NAT-ETREE")
  (parent NAT-TO-ETREE)
  (mhelp ""))

(defmenuitem PFNAT
  (display-name "PFNAT")
  (placement 3)
  (command "PFNAT")
  (parent NAT-TO-ETREE)
  (mhelp ""))

(defmenuitem PNTR
  (display-name "PNTR")
  (placement 4)
  (command "PNTR")
  (parent NAT-TO-ETREE)
  (mhelp ""))

(defmenu PROOF-OUTLINES
  (display-name "Proof Outlines")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem CREATE-SUBPROOF
  (display-name "CREATE-SUBPROOF")
  (placement 2)
  (command "CREATE-SUBPROOF")
  (parent PROOF-OUTLINES)
  (etps t)
  (mhelp ""))

(defmenuitem LINE-COMMENT
  (display-name "LINE-COMMENT")
  (placement 3)
  (command "LINE-COMMENT")
  (parent PROOF-OUTLINES)
  (etps t)
  (mhelp ""))

(defmenuitem MERGE-PROOFS
  (display-name "MERGE-PROOFS")
  (placement 4)
  (command "MERGE-PROOFS")
  (parent PROOF-OUTLINES)
  (etps t)
  (mhelp ""))

(defmenuitem PROOF-COMMENT
  (display-name "PROOF-COMMENT")
  (placement 5)
  (command "PROOF-COMMENT")
  (parent PROOF-OUTLINES)
  (etps t)
  (mhelp ""))

(defmenuitem PROOFLIST
  (display-name "PROOFLIST")
  (placement 6)
  (command "PROOFLIST")
  (parent PROOF-OUTLINES)
  (etps t)
  (mhelp ""))

(defmenuitem TRANSFER-LINES
  (display-name "TRANSFER-LINES")
  (placement 7)
  (command "TRANSFER-LINES")
  (parent PROOF-OUTLINES)
  (etps t)
  (mhelp ""))

(defmenu FILES
  (display-name "Files")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem PRINTPROOF
  (display-name "PRINTPROOF")
  (placement 2)
  (command "PRINTPROOF")
  (parent FILES)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem SCRIBEPROOF
  (display-name "SCRIBEPROOF")
  (placement 3)
  (command "SCRIBEPROOF")
  (parent FILES)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem SETUP-SLIDE-STYLE
  (display-name "SETUP-SLIDE-STYLE")
  (placement 4)
  (command "SETUP-SLIDE-STYLE")
  (parent FILES)
  (etps t)
  (mhelp ""))

(defmenuitem SLIDEPROOF
  (display-name "SLIDEPROOF")
  (placement 5)
  (command "SLIDEPROOF")
  (parent FILES)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem TEXPROOF
  (display-name "TEXPROOF")
  (placement 6)
  (command "TEXPROOF")
  (parent FILES)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenu RULE-RUN
  (display-name "Rule Run")
  (placement 3)
  (parent RULES)
  (mhelp ""))

(defmenuitem ASSEMBLE-FILE
  (display-name "ASSEMBLE-FILE")
  (placement 2)
  (command "ASSEMBLE-FILE")
  (parent RULE-RUN)
  (mhelp ""))

(defmenuitem ASSEMBLE-MOD
  (display-name "ASSEMBLE-MOD")
  (placement 3)
  (command "ASSEMBLE-MOD")
  (parent RULE-RUN)
  (mhelp ""))

(defmenuitem BUILD
  (display-name "BUILD")
  (placement 4)
  (command "BUILD")
  (parent RULE-RUN)
  (mhelp ""))

(defmenuitem WRITE-RULE
  (display-name "WRITE-RULE")
  (placement 5)
  (command "WRITE-RULE")
  (parent RULE-RUN)
  (mhelp ""))

(defmenu LISP-PACKAGES
  (display-name "Lisp Packages")
  (placement 2)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem PACK-STAT
  (display-name "PACK-STAT")
  (placement 2)
  (command "PACK-STAT")
  (parent LISP-PACKAGES)
  (mhelp ""))

(defmenuitem UNUSE
  (display-name "UNUSE")
  (placement 3)
  (command "UNUSE")
  (parent LISP-PACKAGES)
  (etps t)
  (mhelp ""))

(defmenuitem USE
  (display-name "USE")
  (placement 4)
  (command "USE")
  (parent LISP-PACKAGES)
  (mhelp ""))

(defmenu TPS-MODULES
  (display-name "Tps Modules")
  (placement 2)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem LOADED-MODS
  (display-name "LOADED-MODS")
  (placement 2)
  (command "LOADED-MODS")
  (parent TPS-MODULES)
  (mhelp ""))

(defmenuitem MODULES
  (display-name "MODULES")
  (placement 3)
  (command "MODULES")
  (parent TPS-MODULES)
  (mhelp ""))

(defmenuitem UNLOADED-MODS
  (display-name "UNLOADED-MODS")
  (placement 4)
  (command "UNLOADED-MODS")
  (parent TPS-MODULES)
  (mhelp ""))

(defmenu TPS-MAINTENANCE
  (display-name "Tps Maintenance")
  (placement 2)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem CLOAD
  (display-name "CLOAD")
  (placement 2)
  (command "CLOAD")
  (parent TPS-MAINTENANCE)
  (etps t)
  (mhelp ""))

(defmenuitem CLOAD-MODULES
  (display-name "CLOAD-MODULES")
  (placement 3)
  (command "CLOAD-MODULES")
  (parent TPS-MAINTENANCE)
  (etps t)
  (mhelp ""))

(defmenuitem COMPILE-LIST
  (display-name "COMPILE-LIST")
  (placement 4)
  (command "COMPILE-LIST")
  (parent TPS-MAINTENANCE)
  (mhelp ""))

(defmenuitem COMPL
  (display-name "COMPL")
  (placement 5)
  (command "COMPL")
  (parent TPS-MAINTENANCE)
  (mhelp ""))

(defmenuitem FILETYPE
  (display-name "FILETYPE")
  (placement 6)
  (command "FILETYPE")
  (parent TPS-MAINTENANCE)
  (etps t)
  (mhelp ""))

(defmenuitem LEDIT
  (display-name "LEDIT")
  (placement 7)
  (command "LEDIT")
  (parent TPS-MAINTENANCE)
  (mhelp ""))

(defmenuitem LOAD-SLOW
  (display-name "LOAD-SLOW")
  (placement 8)
  (command "LOAD-SLOW")
  (parent TPS-MAINTENANCE)
  (mhelp ""))

(defmenuitem ORGANIZE
  (display-name "ORGANIZE")
  (placement 9)
  (command "ORGANIZE")
  (parent TPS-MAINTENANCE)
  (mhelp ""))

(defmenuitem QLOAD
  (display-name "QLOAD")
  (placement 10)
  (command "QLOAD")
  (parent TPS-MAINTENANCE)
  (etps t)
  (mhelp ""))

(defmenuitem SYS-LOAD
  (display-name "SYS-LOAD")
  (placement 11)
  (command "SYS-LOAD")
  (parent TPS-MAINTENANCE)
  (etps t)
  (mhelp ""))

(defmenuitem TEST-INIT
  (display-name "TEST-INIT")
  (placement 12)
  (command "TEST-INIT")
  (parent TPS-MAINTENANCE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem TLIST
  (display-name "TLIST")
  (placement 13)
  (command "TLIST")
  (parent TPS-MAINTENANCE)
  (mhelp ""))

(defmenuitem TLOAD
  (display-name "TLOAD")
  (placement 14)
  (command "TLOAD")
  (parent TPS-MAINTENANCE)
  (etps t)
  (mhelp ""))

(defmenuitem TPS-TEST
  (display-name "TPS-TEST")
  (placement 15)
  (command "TPS-TEST")
  (parent TPS-MAINTENANCE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem TPS-TEST2
  (display-name "TPS-TEST2")
  (placement 16)
  (command "TPS-TEST2")
  (parent TPS-MAINTENANCE)
  (remote-expert T)
  (mhelp ""))

(defmenuitem TPS3-SAVE
  (display-name "TPS3-SAVE")
  (placement 17)
  (command "TPS3-SAVE")
  (parent TPS-MAINTENANCE)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem DISPLAY-TIME
  (display-name "DISPLAY-TIME")
  (placement 2)
  (command "DISPLAY-TIME")
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem DISABLE-EVENTS
  (display-name "DISABLE-EVENTS")
  (placement 3)
  (command "DISABLE-EVENTS")
  (parent MISC-COMMANDS)
  (etps t)
  (mhelp ""))

(defmenuitem LEAST-SEARCH-DEPTH
  (display-name "LEAST-SEARCH-DEPTH")
  (placement 4)
  (command "LEAST-SEARCH-DEPTH")
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenu MS91
  (display-name "MS91")
  (placement 5)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem SEARCH-PLACEMENT
  (display-name "SEARCH-PLACEMENT")
  (placement 2)
  (command "SEARCH-PLACEMENT")
  (parent MS91)
  (mhelp ""))

(defmenu MATING-SEARCH-COMMANDS
  (display-name "Mating Search Commands")
  (placement 5)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem CLOSE-TESTWIN
  (display-name "CLOSE-TESTWIN")
  (placement 2)
  (command "CLOSE-TESTWIN")
  (parent MATING-SEARCH-COMMANDS)
  (mhelp ""))

(defmenuitem DIY
  (display-name "DIY")
  (placement 3)
  (command "DIY")
  (parent MATING-SEARCH-COMMANDS)
  (mhelp ""))

(defmenuitem DIY-L
  (display-name "DIY-L")
  (placement 4)
  (command "DIY-L")
  (parent MATING-SEARCH-COMMANDS)
  (mhelp ""))

(defmenuitem EPROOFLIST
  (display-name "EPROOFLIST")
  (placement 5)
  (command "EPROOFLIST")
  (parent MATING-SEARCH-COMMANDS)
  (mhelp ""))

(defmenuitem MONITOR
  (display-name "MONITOR")
  (placement 6)
  (command "MONITOR")
  (parent MATING-SEARCH-COMMANDS)
  (mhelp ""))

(defmenuitem MONITORLIST
  (display-name "MONITORLIST")
  (placement 7)
  (command "MONITORLIST")
  (parent MATING-SEARCH-COMMANDS)
  (mhelp ""))

(defmenuitem NOMONITOR
  (display-name "NOMONITOR")
  (placement 8)
  (command "NOMONITOR")
  (parent MATING-SEARCH-COMMANDS)
  (mhelp ""))

(defmenuitem SET-EPROOF
  (display-name "SET-EPROOF")
  (placement 9)
  (command "SET-EPROOF")
  (parent MATING-SEARCH-COMMANDS)
  (mhelp ""))

(defmenu SEARCH-SUGGESTIONS
  (display-name "Search Suggestions")
  (placement 5)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem AUTO-SUGGEST
  (display-name "AUTO-SUGGEST")
  (placement 2)
  (command "AUTO-SUGGEST")
  (parent SEARCH-SUGGESTIONS)
  (mhelp ""))

(defmenuitem SET-BACKGROUND-EPROOF
  (display-name "SET-BACKGROUND-EPROOF")
  (placement 3)
  (command "SET-BACKGROUND-EPROOF")
  (parent SEARCH-SUGGESTIONS)
  (mhelp ""))

(defmenuitem ETR-AUTO-SUGGEST
  (display-name "ETR-AUTO-SUGGEST")
  (placement 4)
  (command "ETR-AUTO-SUGGEST")
  (parent SEARCH-SUGGESTIONS)
  (mhelp ""))

(defmenu SEQUENT-CALCULUS
  (display-name "Sequent Calculus")
  (placement 5)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem PSEQ
  (display-name "PSEQ")
  (placement 2)
  (command "PSEQ")
  (parent SEQUENT-CALCULUS)
  (mhelp ""))

(defmenuitem PSEQL
  (display-name "PSEQL")
  (placement 3)
  (command "PSEQL")
  (parent SEQUENT-CALCULUS)
  (mhelp ""))

(defmenuitem SEQ-TO-NAT
  (display-name "SEQ-TO-NAT")
  (placement 4)
  (command "SEQ-TO-NAT")
  (parent SEQUENT-CALCULUS)
  (mhelp ""))

(defmenuitem SEQLIST
  (display-name "SEQLIST")
  (placement 5)
  (command "SEQLIST")
  (parent SEQUENT-CALCULUS)
  (mhelp ""))

(defmenu SAVING
  (display-name "Save")
  (placement 5)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem APPEND-WFF
  (display-name "APPEND-WFF")
  (placement 2)
  (command "APPEND-WFF")
  (parent SAVING)
  (etps t)
  (mhelp ""))

(defmenuitem APPEND-WFFS
  (display-name "APPEND-WFFS")
  (placement 3)
  (command "APPEND-WFFS")
  (parent SAVING)
  (etps t)
  (mhelp ""))

(defmenuitem EXECUTE-FILE
  (display-name "EXECUTE-FILE")
  (placement 4)
  (command "EXECUTE-FILE")
  (parent SAVING)
  (etps t)
  (mhelp ""))

(defmenuitem FINDPROOF
  (display-name "FINDPROOF")
  (placement 5)
  (command "FINDPROOF")
  (parent SAVING)
  (etps t)
  (mhelp ""))

(defmenuitem FINISH-SAVE
  (display-name "FINISH-SAVE")
  (placement 6)
  (command "FINISH-SAVE")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem PAUSE
  (display-name "PAUSE")
  (placement 7)
  (command "PAUSE")
  (parent SAVING)
  (etps t)
  (mhelp ""))

(defmenuitem RESTORE-WORK
  (display-name "RESTORE-WORK")
  (placement 8)
  (command "RESTORE-WORK")
  (parent SAVING)
  (etps t)
  (mhelp ""))

(defmenuitem RESUME-SAVE
  (display-name "RESUME-SAVE")
  (placement 10)
  (command "RESUME-SAVE")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem SAVE-FLAGS-AND-WORK
  (display-name "SAVE-FLAGS-AND-WORK")
  (placement 11)
  (command "SAVE-FLAGS-AND-WORK")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem SAVE-SUBPROOF
  (display-name "SAVE-SUBPROOF")
  (placement 12)
  (command "SAVE-SUBPROOF")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem SAVE-WORK
  (display-name "SAVE-WORK")
  (placement 13)
  (command "SAVE-WORK")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem SAVEPROOF
  (display-name "SAVEPROOF")
  (placement 14)
  (command "SAVEPROOF")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem SCRIPT
  (display-name "SCRIPT")
  (placement 15)
  (command "SCRIPT")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem STOP-SAVE
  (display-name "STOP-SAVE")
  (placement 16)
  (command "STOP-SAVE")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem UNSCRIPT
  (display-name "UNSCRIPT")
  (placement 17)
  (command "UNSCRIPT")
  (parent SAVING)
  (remote-expert T)
  (etps t)
  (mhelp ""))

; (defmenu CONCEPT-TERMINAL
;   (display-name "CONCEPT-TERMINAL")
;   (placement 5)
;   (parent MISC-COMMANDS)
;   (mhelp ""))
;
; (defmenuitem LOADKEY
;   (display-name "LOADKEY")
;   (placement 2)
;   (command "LOADKEY")
;   (parent CONCEPT-TERMINAL)
;   (mhelp ""))
; 
; (defmenuitem RESET
;   (display-name "RESET")
;   (placement 3)
;   (command "RESET")
;   (parent CONCEPT-TERMINAL)
;   (mhelp ""))

(defmenu COLL-HELP
  (display-name "COLL-HELP")
  (placement 5)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem CHARDOC
  (display-name "CHARDOC")
  (placement 2)
  (command "CHARDOC")
  (parent COLL-HELP)
  (mhelp ""))

(defmenuitem COLLECT-HELP
  (display-name "COLLECT-HELP")
  (placement 3)
  (command "COLLECT-HELP")
  (parent COLL-HELP)
  (mhelp ""))

(defmenuitem HELP-LIST
  (display-name "HELP-LIST")
  (placement 4)
  (command "HELP-LIST")
  (parent COLL-HELP)
  (mhelp ""))

(defmenu AUTO-GEN
  (display-name "Automatically Generate Data")
  (placement 4.5)
  (parent MISC-COMMANDS)
  (remote-expert T)
  (mhelp ""))
  
(defmenuitem HTML-DOC
  (display-name "HTML-DOC")
  (placement 5)
  (command "HTML-DOC")
  (parent AUTO-GEN)
  (remote-expert T)
  (mhelp ""))

(defmenuitem GENERATE-JAVA-MENUS
  (display-name "GENERATE-JAVA-MENUS")
  (placement 6)
  (command "GENERATE-JAVA-MENUS")
  (parent AUTO-GEN)
  (remote-expert T)
  (etps t)
  (mhelp ""))

(defmenuitem QUICK-REF
  (display-name "QUICK-REF")
  (placement 6)
  (command "QUICK-REF")
  (parent COLL-HELP)
  (mhelp ""))

(defmenuitem SCRIBE-DOC
  (display-name "SCRIBE-DOC")
  (placement 7)
  (command "SCRIBE-DOC")
  (parent COLL-HELP)
  (mhelp ""))

(defmenu HELP-OBJ
  (display-name "HELP-OBJ")
  (placement 5)
  (parent MISC-COMMANDS)
  (mhelp ""))

(defmenuitem ?
  (display-name "?")
  (placement 2)
  (command "?")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem ??
  (display-name "??")
  (placement 3)
  (command "??")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem ABBREVIATIONS
  (display-name "ABBREVIATIONS")
  (placement 4)
  (command "ABBREVIATIONS")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem ENVIRONMENT
  (display-name "ENVIRONMENT")
  (placement 5)
  (command "ENVIRONMENT")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem HELP2
  (display-name "HELP")
  (placement 6)
  (command "HELP")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem HELP*
  (display-name "HELP*")
  (placement 7)
  (command "HELP*")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem HELP-GROUP
  (display-name "HELP-GROUP")
  (placement 8)
  (command "HELP-GROUP")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem LIST-RULES
  (display-name "LIST-RULES")
  (placement 9)
  (command "LIST-RULES")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem LIST-RULES*
  (display-name "LIST-RULES*")
  (placement 10)
  (command "LIST-RULES*")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem OOPS
  (display-name "OOPS")
  (placement 11)
  (command "OOPS")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem PROBLEMS
  (display-name "PROBLEMS")
  (placement 12)
  (command "PROBLEMS")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenuitem SEARCH
  (display-name "SEARCH")
  (placement 13)
  (command "SEARCH")
  (parent HELP-OBJ)
  (etps t)
  (mhelp ""))

(defmenu LIBRARY-TOP-LEVELS
  (display-name "Library Top Levels")
  (placement 2)
  (parent TOP-LEVELS)
  (mhelp ""))

(defmenuitem LIBRARY0
  (display-name "LIB")
  (placement 2)
  (command "LIB")
  (parent LIBRARY-TOP-LEVELS)
  (hotkey #\l)
  (mhelp ""))

(defmenuitem UNIXLIBRARY0
  (display-name "UNIXLIB")
  (placement 3)
  (command "UNIXLIB")
  (parent LIBRARY-TOP-LEVELS)
  (mhelp ""))

(defmenuitem BEGIN-PRFW
  (display-name "BEGIN-PRFW")
  (placement 100)
  (command "BEGIN-PRFW")
  (parent TOP-LEVELS)
  (etps t)
  (mhelp ""))

(defmenuitem END-PRFW
  (display-name "END-PRFW")
  (placement 101)
  (command "END-PRFW")
  (parent TOP-LEVELS)
  (etps t)
  (mhelp ""))

(defmenuitem PUSH-TO-TOP
  (display-name "PUSH")
  (placement 400)
  (command "PUSH")
  (parent TOP-LEVELS)
  (etps t)
  (mhelp ""))

(defmenuitem POP-FROM-TOP
  (display-name "POP")
  (placement 401)
  (command "POP")
  (parent TOP-LEVELS)
  (etps t)
  (mhelp ""))

(defmexpr generate-java-menus
  (argtypes filespec)
  (argnames filename)
  (arghelp "filename")
  (defaultfns (lambda (filename)
		(list
		 (if (eq filename '$)
		     "menus.java"
		   filename))))
  (mhelp "Generate Java code for menus.  This command should only be used
by programmers.  See the TPS3 Programmer's Guide.  This should be run and the
resulting code appropriately inserted into TpsWin.java whenever the
menu structure has been changed."))

(defun generate-java-menus (filename)
  (let* ((f (open filename :direction :output :if-exists :supersede
		 :if-does-not-exist :create))
	 (top-menus nil)
	 (menu-hash (make-hash-table))
	 (name-hash (make-hash-table))
	 (hide-menus-in-etps (make-hash-table))
	 (i 0))
    (declare (special hide-menus-in-etps))
    (dolist (m global-menulist)
	    (when (symbolp m)
	      (let ((par (get m 'parent)))
		(when (or (eq par 'mbar) (member par global-toplevellist))
		  (let ((n (read-from-string (format nil "m~d" i))))
		    (incf i)
		    (push m top-menus)
		    (setf (gethash m name-hash) n)
		    (unless (eq m 'mbar)
		      (format f "    public Menu ~d = new Menu(new String(\"~d\"));~%"
			      n (get m 'display-name)))
		    (push m (gethash par menu-hash)))))))
    (format f "    public MenuBar mbar = new MenuBar();~2%")
    (format f "    private void etpsMenuItem(Menu m,String s1,String s2) {~%")
    (format f "        MenuItem mi = new MenuItem(new String(s1));~%")
    (format f "        TpsMenuActionListener ml = new TpsMenuActionListener();~%")
    (format f "        ml.tpsMenuActionInit(this,new String(s2));~%")
    (format f "        mi.addActionListener(ml);~%")
    (format f "        m.add(mi);~%")
    (format f "    }~2%")
    (format f "    private void etpsMenuItem(Menu m,String s1,String s2,int key) {~%")
    (format f "        MenuItem mi = new MenuItem(new String(s1),new MenuShortcut(key));~%")
    (format f "        TpsMenuActionListener ml = new TpsMenuActionListener();~%")
    (format f "        ml.tpsMenuActionInit(this,new String(s2));~%")
    (format f "        mi.addActionListener(ml);~%")
    (format f "        m.add(mi);~%")
    (format f "    }~2%")
    (format f "    private void tpsMenuItem(Menu m,String s1,String s2) {~%")
    (format f "        if (!etps) {~%")
    (format f "          etpsMenuItem(m,s1,s2);~%")
    (format f "        }~%")
    (format f "    }~2%")
    (format f "    private void tpsMenuItem(Menu m,String s1,String s2,int key) {~%")
    (format f "        if (!etps) {~%")
    (format f "          etpsMenuItem(m,s1,s2,key);~%")
    (format f "        }~%")
    (format f "    }~2%")
    (format f "    public void TpsWinMenuInit(boolean expert) {~%")
    (dolist (m global-menulist)
	    (when (and (symbolp m) (not (member m top-menus)))
	      (when (gethash m name-hash)
		(complain m " defined in menus twice"))
	      (let ((n (read-from-string (format nil "m~d" i))))
		(setf (gethash m name-hash) n)
		(incf i)
		(unless (eq m 'mbar)
		  (format f "        Menu ~d = new Menu(new String(\"~d\"));~%"
			  n (get m 'display-name)))
		(let ((par (get m 'parent)))
		  (push m (gethash par menu-hash))))))
    (dolist (m global-menuitemlist)
	    (when (symbolp m)
	      (let* ((par (get m 'parent)))
		(if (member m (gethash par menu-hash))
		    (complain "Menu " m " defined twice")
		  (push m (gethash par menu-hash))))))
    (dolist (h1 (append global-toplevellist global-menulist))
	    (when (symbolp h1)
	      (when (gethash h1 menu-hash)
		(setf (gethash h1 menu-hash)
		      (sort (gethash h1 menu-hash)
			    #'(lambda (x y) (< (get x 'placement)
					       (get y 'placement))))))))
    (hide-menus-in-etps menu-hash hide-menus-in-etps)
    (let ((main-name (gethash 'main name-hash)))
      (format f "        if (expert) {~%")
      (format f "            MenuItem comi = new MenuItem(\"COMMAND (Enter Command)\",new MenuShortcut(97));~%")
      (format f "            TpsMenuActionListener coml = new TpsMenuActionListener();~%")
      (format f "            coml.tpsMenuActionGenCommand(sendtps);~%")
      (format f "            comi.addActionListener(coml);~%")
      (format f "            ~d.add(comi);~%" main-name)
      (format f "        }~%"))
    (dolist (m (gethash 'mbar menu-hash))
	    (if (get m 'remote-expert)
		(progn
		  (format f "        if (expert) {~%")
		  (format f "            mbar.add(~d);~%" (gethash m name-hash))
		  (format f "        }~%"))
	      (format f "        mbar.add(~d);~%" (gethash m name-hash)))
	    (generate-java-menus-1 f m menu-hash name-hash))
    (dolist (h global-toplevellist)
	    (when (symbolp h)
	      (msgf "top level " h t)
	      (dolist (k (gethash h menu-hash))
		      (msgf "top level menu " k t)
		      (generate-java-menus-1 f k menu-hash name-hash))))
    (format f "        setMenuBar(mbar);~%")
    (format f "    }~2%")
    (format f "    public void adjustMenus(String newtoplevel) {~%")
    (format f "        toplevel = newtoplevel;~%")
    (let ((fst t))
      (dolist (h1 global-toplevellist)
	      (when (symbolp h1)
		(if fst
		    (progn
		      (format f "        if (toplevel.equals(\"~d\")) {~%"
			      h1)
		      (setq fst nil))
		  (format f "        } else if (toplevel.equals(\"~d\")) {~%"
			  h1))
		(dolist (h2 global-toplevellist)
			(when (and (symbolp h2) (neq h1 h2))
			  (dolist (g (gethash h2 menu-hash))
				  (format f "            mbar.remove(~d);~%" 
					  (gethash g name-hash)))))
		(dolist (g (gethash h1 menu-hash))
			(format f "            mbar.add(~d);~%" 
				(gethash g name-hash))))))
    (format f "        }~%    }~%")
    (close f)))

(defun hide-menus-in-etps (menu-hash hide-menus-in-etps)
  (maphash #'(lambda (key value)
	       (declare (ignore value))
	       (hide-menus-in-etps-1 key menu-hash hide-menus-in-etps))
	   menu-hash))

(defun hide-menus-in-etps-1 (m menu-hash hide-menus-in-etps)
  (or (gethash m hide-menus-in-etps)
      (setf (gethash m hide-menus-in-etps)
	    (hide-menus-in-etps-2 (gethash m menu-hash) menu-hash hide-menus-in-etps))))

(defun hide-menus-in-etps-2 (vals menu-hash hide-menus-in-etps)
  (if vals
      (if (gethash (car vals) menu-hash)
	  (let* ((eom1 (hide-menus-in-etps-1 (car vals) menu-hash hide-menus-in-etps))
		 (eom2 (hide-menus-in-etps-2 (cdr vals) menu-hash hide-menus-in-etps)))
	    (if (and (eq eom1 'YES) (eq eom2 'YES))
		'YES
	      'NO))
	(if (get (car vals) 'etps)
	    'NO
	  'YES))
    'YES))

(defun generate-java-menus-1 (f par menu-hash name-hash &optional expert)
  (declare (special hide-menus-in-etps))
  (let ((npar (gethash par name-hash))
	(place 0))
    (when npar
      (dolist (m (gethash par menu-hash))
	      (let ((newexpert (and (get m 'remote-expert) (not expert))))
		(when newexpert
		  (format f "        if (expert) {~%"))
		(let ((n (gethash m name-hash))
		      (p (get m 'placement)))
		  (when (> (floor (/ p 100)) (floor (/ place 100)))
		    (when (or newexpert expert)
		      (format f "    "))
		    (format f "        ~d.add(new MenuItem(\"-\"));~%" npar))
		  (setq place p)
		  (when (or newexpert expert)
		    (format f "    "))
		  (if n ; then it's a submenu
		      (progn
			(when (eq (gethash m hide-menus-in-etps) 'YES)
			  (format f "        if (!etps) {~%"))
			(format f "         ~d.add(~d);~%"
				npar n)
			(when (eq (gethash m hide-menus-in-etps) 'YES)
			  (format f "        }~%"))
			(generate-java-menus-1 f m menu-hash name-hash
					       (or newexpert expert)))
		    (if (get m 'hotkey) ; otherwise it's an item
			(if (get m 'etps)
			    (format f "        etpsMenuItem(~d,\"~d\",\"~d\",~d);~%"
				    npar (get m 'display-name) (get m 'command)
				    (char-code (get m 'hotkey)))
			  (format f "        tpsMenuItem(~d,\"~d\",\"~d\",~d);~%"
				  npar (get m 'display-name) (get m 'command)
				  (char-code (get m 'hotkey))))
		      (if (get m 'etps)
			  (format f "        etpsMenuItem(~d,\"~d\",\"~d\");~%"
				  npar (get m 'display-name) (get m 'command))
			(format f "        tpsMenuItem(~d,\"~d\",\"~d\");~%"
				npar (get m 'display-name) (get m 'command)))))
		  (when newexpert
		    (format f "        }~%"))))))))
  
