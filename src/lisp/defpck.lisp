;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :core)
;;; Next line is commented out, since TPS modules are not yet defined.
;;;(part-of BOOTSTRAP)

;;;
;;; File: DEFPCK
;;;
;;; Defines modules as they are known to TPS.
;;;

(deffile defpck
  (part-of bootstrap)
  (extension clisp)
  (mhelp "Defines packages as they are known to TPS3."))

(context lisp-packages)

(def-lisp-package core
  (needed-lisp-packages)
  (mhelp "The core system for TPS containing many of its TPS packages."))

(def-lisp-package maint
  (needed-lisp-packages core)
  (mhelp "System maintenance packages including automatic documentation and
the rules package."))

(def-lisp-package auto
  (needed-lisp-packages core)
  (mhelp "The automatic component, including unification and matingsearch."))

(def-lisp-package ml
  (needed-lisp-packages core)
  (mhelp "The Math Logic I & II logic."))

;(def-lisp-package tait
;  (needed-lisp-packages core)
;  (mhelp "The TAIT-system logic."))

;(def-lisp-package sieg
;  (needed-lisp-packages core)
;  (mhelp "The Philosophy Dept's logic."))

(def-lisp-package teacher
  (needed-lisp-packages core)
  (mhelp "For teachers using ETPS in their courses."))

;;; Now come the TPS module definitions.

(context maint::modules-in-tps)

(defmodule bootstrap
  (needed-modules)
  (lisp-pack core)
  (files boot0 boot1 defpck)
  (mhelp "All files needed to bootstrap TPS."))

(defmodule tpsdef
  (needed-modules bootstrap)
  (lisp-pack core)
  (macro-files contexts-core contexts-auto contexts-maint contexts-teacher
	       contexts-ml subjects-core subjects-auto subjects-teacher subjects-maint)
  (files tpstop argtyp flagging gensty)
  (mhelp "The module allowing definitions of TPS-objects."))

(defmodule bare
  (needed-modules tpsdef)
  (lisp-pack core)
  (files tops20 lsppck-core top macsys linereadp tps3-save)
  (mhelp "The barest possible TPS."))

(defmodule tps-modules
  (needed-modules bare)
  (lisp-pack maint)
  (macro-files pck)
  (mhelp "Defines commands to deal with modules."))
	
(defmodule file-ops
  (needed-modules bare)
  (lisp-pack maint)
  (files filsys)
  (mhelp "Some file utilities, e.g. FILETYPE."))

(defmodule concept-bare
  (needed-modules bare)
  (lisp-pack core)
  (macro-files concpt)
  (mhelp "Defines functions specific to the Concept-100."))

(defmodule maintain
  (needed-modules bare)
  (lisp-pack maint)
  (macro-files argtyp-maint)
  (files maint compl lsppck-maint menus) ; compilis
  (mhelp "Defines useful commands for maintaining TPS."))

(defmodule save-tps-work
  (needed-modules bare)
  (lisp-pack core)
  (files save-work)
  (mhelp "Defines commands for saving and restoring work."))

(defmodule wffs
  (needed-modules bare)
  (lisp-pack core)
  (macro-files wffmacros wfftyp flavoring wfftst wffcat wffmodes wffrec)
  (files wffprim wffmve)
  (mhelp "Defines wffs and some operations on them."))

(defmodule wff-print
  (needed-modules wffs)
  (lisp-pack core)
  (macro-files wffout styles prtprp faces interface-style)
  (files prt pprint prtop prtcmd)
  (mhelp "Defines wffs-printing operations and commands."))

(defmodule wff-parse
  (needed-modules wff-print)
  (lisp-pack core)
  (macro-files wffinm)
  (files wffin tpinf) ; wffing
  (mhelp "Defines wff parsing functions common to all styles."))

(defmodule wff-ops-abb
  (needed-modules wff-parse)
  (lisp-pack core)
  (files wffabb)
  (mhelp "Defines basic recursive functions for instantiating definitions."))

(defmodule wff-ops1
  (needed-modules wff-parse)
  (lisp-pack core)
  (files wffsub1 wffneg1 wffequ1 wffchange wffmbed)
  (mhelp "Defines some basic operations on wffs in first-order logic."))

(defmodule wff-ops2
  (needed-modules  wff-ops-abb)
  (lisp-pack core)
  (macro-files wfflmbd-macros)
  (files wffabb2 wffsub2 wfflmbd2 wffequ2)
  (mhelp "Defines some basic operations on wffs in higher-order logic."))

(defmodule wff-editor
  (needed-modules wff-ops1 wff-ops2)
  (lisp-pack core)
  (macro-files edtop)
  (files ; edtop-3  ; now part of edtop 10/12/87 DAN
   edopera edmove edabb edprt edill edsub edchange edmbed eddev
   ed-menus) ; cebrown 5/6/02
  (mhelp "The kernel of the wff editor."))

(defmodule weak-label
  (needed-modules wff-editor)
  (lisp-pack core)
  (macro-files weak-mac)
  (files weak)
  (mhelp "Defines the flavor WEAK of labels."))

(defmodule save-wffs
  (needed-modules weak-label)
  (lisp-pack core)
  (macro-files wffsav-mac)
  (files wffsav)
  (mhelp "Allows writing of weak labels into files."))

(defmodule lambda-calc
  (needed-modules wff-editor)
  (lisp-pack core)
  (files edlmbd cnf)
  (mhelp "Defines some operations of the typed lambda-calculus."))

(defmodule auto-basic
  (needed-modules wff-editor vpforms)
  (lisp-pack auto)
  (macro-files argtyp-auto)
  (files node)
  (mhelp "Files needed by various TPS modules in auto package."))

(defmodule expansion-tree
  (needed-modules mating)
  (lisp-pack auto)
  (macro-files etrees-def )
  (files etrees-wffops etrees-wffops2 etrees-print etrees-jforms
	 ftrees				; ftree representation of etrees - cebrown 12/1/00
         etrees-debug ; this is not necessary for TPS, but is useful for debugging
                      ; see the comments at the beginning of etrees-debug.lisp
	 etrees-renumber mtree-datastructure) ;this last is here because ACL3.1 wants the structures
                                              ;defined before they're used in e.g. diy.lisp   MB 3/95
  (mhelp "Defines expansion trees and associated wffops."))

(defmodule ext-dags
  (needed-modules tactics ms90-3)
  (lisp-pack auto)
  (macro-files ext-exp-dag-macros)
  (files ext-seq ext-seq-top ext-mate-top ext-exp-dags ext-exp-open-dags 
	 ext-seq-tactics ext-exp-dags-nd 
	 ext-search ms04-search)
  (mhelp "Extensional expansion dags and extensional sequent calculus related code.
See Chad E. Brown's thesis."))

(defmodule mating
  (needed-modules auto-basic)
  (lisp-pack auto)
  (macro-files etrees-flags etrees-exp-vars etrees-skolem etrees-labels 
               mating-top data-structures mating-macros monitor-macros
	       test-macros)
  (files mating-move mating-mateops timing monitor test-top-top test-top-slists
	 test-top-search
	 mate-menus test-top-menus) ; cebrown 5/6/02
  (mhelp "Defines mating search top level and basic mating operations."))

(defmodule ms88
  (needed-modules expansion-tree events unification skolemizing save-tps-work
		  primitive-subst otlrulep tactics)
  (lisp-pack auto)
  (files mating-aux connections mating mating-paths unif-mat mating-dir
	 mating-events mating-prop unif-fo mating-sub) ; mating-cmds
  (mhelp "The MS88 mating search module."))

(defmodule mst
  (needed-modules ms88)
  (lisp-pack auto)
  (files mtree-obligation mtree-top mtree-print mtree-unification mtree-query
	 mtree-duplication
	 mtree-menus) ; cebrown 5/6/02
  (mhelp "The matinsgtree module."))

(defmodule ms90-3
  (needed-modules expansion-tree events skolemizing)
  (lisp-pack auto)
  (macro-files ms90-3-node ms90-3-data)
  (files ms90-3-unif-simpl ms90-3-path-enum ms90-3-path-bkup 
         ms90-3-unif-match ms90-3-unif-tree ms90-3-unif-fo ms90-3-top
         ms90-3-expand-etree ms90-3-exp-jform min-quant-etree 
	 ms90-3-prop ms92-9-top ms93-1)
  (mhelp "The mating search module MS90-3.  This search procedure
incorporates Issar's path-focused duplication, working on a single jform.
Note that the search will proceed in an automatic mode, and none of the 
interactive facilities described either in this top-level or 
elsewhere in TPS will work."))

(defmodule mating-transform
  (needed-modules ;mating-search
   ms88)
  (lisp-pack auto)
  (files mating-trans mating-merge mating-merge2 mating-merge-eq)
  (mhelp "Functions to reduce and modify spanning mating."))

(defmodule skolemizing
  (needed-modules wff-editor)
  (lisp-pack auto)
  (macro-files wff-skolem-mac)
  (files wff-skolem)
  (mhelp "Define different ways of skolemizing."))

#+comment(defmodule development-seqs
  (needed-modules wff-editor)
  (lisp-pack auto)
  (files eddev)
  (mhelp "Defines some operations dealing with development sequences."))

(defmodule concept-wff
  (needed-modules wff-parse concept-bare)
  (lisp-pack core)
  (macro-files consty)
  (files cfont) ; wffinc
  (mhelp "Defines functions for printing and parsing on a Concept."))

(defmodule sail-wff
  (needed-modules wff-print)
  (lisp-pack core)
  (files sail)
  (mhelp "Defines output style SAIL."))
  
(defmodule logic-scribe
  (needed-modules wff-print)
  (lisp-pack core)
  (macro-files scribe)
  (files ml1-scribe)
  (mhelp "Defines output style SCRIBE for Math Logic Course. "))

(defmodule scribe-wff
  (needed-modules logic-scribe)
  (lisp-pack core)
  (files dfont)
  (mhelp "Defines output style SCRIBE."))

(defmodule tex-wff
  (needed-modules wff-print)
  (lisp-pack core)
  (macro-files deftex)
  (files texchr)
  (mhelp "Defines the TeX device style."))

(defmodule tps-help
  (needed-modules bare)
  (lisp-pack core)
  (files mhelp read-help) ;; REORGANIZE is now part of BOOT1 -SI  (Feb 11, 87)
  (mhelp "Defines HELP facility."))

(defmodule environment
  (needed-modules tps-help)
  (lisp-pack core)
  (files environ)
  (mhelp "Defines the ENVIRONMENT facility."))

(defmodule auto-doc
  (needed-modules tps-help wff-print)
  (lisp-pack maint)
  (macro-files docdef)
  (files latexdoc scrdoc plurals collect-help htmldoc omdoc)
  (mhelp "Defines commands to automatically produce TPS documentation."))

(defmodule review-flags
  (needed-modules tps-help)
  (lisp-pack core)
  (files review
	 review-menus flag-deps) ; cebrown 5/6/02
  (mhelp "Defines the REVIEW top-level."))

(defmodule saving-modes
  (needed-modules review-flags)
  (lisp-pack core)
  (files modsav)
  (mhelp "Allows definition and saving of MODEs."))

(defmodule grader-top
  (lisp-pack teacher)
  (needed-modules bare)
  (files grades-top) 
  (mhelp "The grading package."))

(defmodule grader
  (lisp-pack teacher)
  (needed-modules etps-events grader-top)
  (macro-files gr-macros)
  (files grades1 grades2)  ;; examgrade
  (mhelp "The grading package."))

(defmodule jforms
  (needed-modules wff-parse)
  (lisp-pack auto)
  (macro-files jforms-defns)
  (files jforms-labels jforms order-components weak-mac-auto jforms-edops)
  (mhelp "Defines operations associated with creating jforms."))

(defmodule vpforms
  (needed-modules wff-editor jforms tex-wff)
  (lisp-pack auto)
  (macro-files vpforms-macros)
  (files vpforms vpforms-tex)
  (mhelp "Editor operations associated with creating and displaying VPFORMS."))


(defmodule tps2-rulep
  (needed-modules jforms) ; needs jforms, not vpforms DAN 10/26/87
  (lisp-pack auto)
  (macro-files rulep-mac)
  (files rulep-edops newrulep-tsts)  ;; rulep-tsts
  (mhelp "Defines edops to check satisfiability and validity."))


(defmodule metawffs
  (needed-modules wff-print)
  (lisp-pack core)
  (macro-files meta-label meta-var meta-var2) ; mkaminski 11/11/2005
					      ; -- added meta-var2
  (mhelp "Defines META-WFFS as used in the rules and outline modules."))

(defmodule wffmatch
  (needed-modules metawffs)
  (lisp-pack core)
  (macro-files match-macros)
  (files match-wffs)
  (mhelp "Defines objects dealing with matching as needed in the rules
and outline modules."))

(defmodule theorems
  (needed-modules wff-parse)
  (lisp-pack core)
  (macro-files theorem-mac)
  (mhelp "Defines ways of defining theorems, exercises, etc."))

(defmodule read-rules
  (needed-modules wff-parse)
  (lisp-pack core)
  (macro-files read-rdef-mac)
  (files read-ruledefs)
  (mhelp "Allows reading of rules for help or rules module."))

(defmodule ops-otlrules
  (needed-modules wff-ops1 wff-ops-abb)
  (lisp-pack core)
  (files wffop-otl)
  (mhelp "Wffops needed by both rule and outline modules."))

(defmodule rules
  (needed-modules wffmatch ops-otlrules read-rules)
  (lisp-pack maint)
  (macro-files rule-wffop rule-idef)
  (files rule-build rule-bb rule-build-default rule-build-check rule-cmds
	 rule-build-match rule-build-tac ; for building tactics
	 )				; LOGIC-RULES left out for now.
  (mhelp "The RULES module which generates inference rules from specifications."))

(defmodule otlrules
  (needed-modules wffmatch ops-otlrules)
  (lisp-pack core)
  (macro-files otl-cmddef)
  (files otl-aux)
  (mhelp "Functions needed to execute rules generated by the rules module."))

(defmodule otlhelp
  (needed-modules read-rules otlrules otlnl)
  (lisp-pack core)
  (files otl-help)
  (mhelp "Functions to give nice help on rules."))

(defmodule event-signal
  (needed-modules bare)
  (lisp-pack core)
  (files event-signal-utils)
  (mhelp "Lets the system signal events."))

(defmodule otlnl
  (needed-modules wff-print event-signal)
  (lisp-pack core)
  (macro-files otl-macros otl-typ)  
  (files linenumber1 linenumber2 otlnl prtotl otl-fileout
	 otl-rearrange otl-prt saveproof pbrief)
  (mhelp "Creates and updates proof structure."))

(defmodule otlscribe
  (needed-modules otlnl logic-scribe)
  (lisp-pack core)
  (files otl-scribeout)
  (mhelp "Printing proofs in style SCRIBE."))

(defmodule otlrulep
  (needed-modules otlnl tps2-rulep)
  (lisp-pack core)
  (macro-files)
  (files otl-rulep)
  (mhelp "Defines the interface between the tautology checker and
outline rules."))

(defmodule otlsuggest
  (needed-modules otlrules otlnl)
  (lisp-pack core)
  (macro-files otl-sugg-mac)
  (files otl-suggest)
  (mhelp "Defines commands connected with automatic help."))

(defmodule otlgo
  (needed-modules otlsuggest)
  (lisp-pack core)
  (macro-files otl-go-mac)
  (files otl-go)
  (mhelp "Defines the GO facility."))

(defmodule otlcleanup
  (needed-modules otlnl read-rules)
  (lisp-pack core)
  (files otl-cleanup)
  (mhelp "Defines various forms of clean-up commands."))

(defmodule otladvice
  (needed-modules otlsuggest otlcleanup)
  (lisp-pack core)
  (files otl-advice)
  (mhelp "Defines the ADVICE facility for ETPS."))

(defmodule otlschema2
  (needed-modules otlnl otlrules)
  (lisp-pack core)
  (files otl-schema2)
  (mhelp "Module to use theorems as lemmas in other proofs with type inference."))

(defmodule mode-ml
  (lisp-pack ml)
  (files ml-mode)
  (mhelp "Defines mode ML, as other files in ML module have to be loaded
in that mode."))

(defmodule math-logic-1-wffs
  (needed-modules wff-parse mode-ml)
  (lisp-pack ml)
  (files ml1-const ml1-abbrev)
  (mhelp "Defines wffs for Mathematical Logic I course."))

(defmodule math-logic-2-wffs
  (needed-modules wff-parse mode-ml)
  (lisp-pack ml)
  (files ml2-const ml2-abbrev ml2-abbrev2
	 ml2-axioms ml2-replace ; moved here from math-logic-2-rules module since they are NOT rules files.
					; ASSEMBLE-MOD would fail for math-logic-2-rules otherwise. - cebrown 2/17/03
	 )  ;; ml2-axioms
  (mhelp "Defines wffs for Mathematical Logic II course."))

(defmodule math-logic-2-exercises
  (needed-modules math-logic-2-wffs theorems)
  (lisp-pack ml)
  (files ml1-theorems ml2-theorems)
  (mhelp "Exercises for Mathematical Logic II."))

(defmodule math-logic-1-rules
  (needed-modules otlsuggest math-logic-1-wffs)
  (lisp-pack ml)
  (macro-files ml1-prior)
  (files ml1-logic0 ml1-logic1 ml1-logic2 ml1-logic3a ml1-logic3b ml1-logic4)
  (mhelp "Defines rules for Mathematical Logic I course."))

(defmodule math-logic-2-rules
  (needed-modules otlsuggest math-logic-2-wffs theorems replace)
  (lisp-pack ml)
  (macro-files ml2-prior)
  (files ml1-logic0 ml2-logic1a ml2-logic1b ml2-logic1c ml2-logic2a ml2-logic2b
	 ml1-logic3a ml1-logic3b ml2-logic4a 
	 ml2-logic4b ml2-logic4c ml2-logic5a ml2-logic5b
	 ml2-logic7a ml2-logic7b ml2-logic7c
					; ml2-axioms ; this is not a .rules file - cebrown 2/17/03
					; ml2-replace ; this is not a .rules file - cebrown 2/17/03
	 ; there's some overlap between 5 and 7
	 ml2-hacks)
  (mhelp "Defines rules for Mathematical Logic II course."))

(defmodule math-logic-1
  (needed-modules math-logic-1-rules);  math-logic-1-exercises)
  (lisp-pack ml)
  (mhelp "Defines wffs and rules for Mathematical Logic I course."))


(defmodule math-logic-2
  (needed-modules math-logic-2-rules math-logic-2-exercises)
  (lisp-pack ml)
  (mhelp "Defines wffs, rules, and exercises for Mathematical Logic II
course."))


;(defmodule sieg-1-wffs
;  (needed-modules wff-parse)
;  (lisp-pack sieg)
;  (files sieg-const sieg-abbrev)
;  (mhelp "Defines wffs for Philosophy department logic course."))

;(defmodule sieg-1-theorems
;  (needed-modules sieg-1-wffs theorems)
;  (lisp-pack sieg)
;  (files sieg-theorems)
;  (mhelp "Exercises for Philosophy department logic course."))

;(defmodule sieg-1-rules
;  (needed-modules otlsuggest sieg-1-wffs)
;  (lisp-pack sieg)
;  (macro-files sieg-prior)
;  (files sieg-logic0 sieg-logic1 sieg-logic2 sieg-logic3 sieg-logic4
;	 sieg-logic5 sieg-logic6 sieg-logic7)
;  (mhelp "Defines rules for Philosophy department logic course."))

;(defmodule sieg-1
;  (needed-modules sieg-1-rules sieg-1-theorems)
;  (lisp-pack sieg)
;  (mhelp "Defines wffs, rules, and exercises for Philosophy department
;logic course."))


(defmodule events
  (needed-modules bare)
  (lisp-pack core)
  (macro-files events-mac)
  (files events)
  (mhelp "Defines category of EVENT and associated functions."))

(defmodule etps-events
  (needed-modules events)
  (lisp-pack core)
  (files etps-events tps3-error)	
  (mhelp "Defines events which could be signalled in ETPS."))

(defmodule primitive-subst
  (needed-modules auto-basic)
  (lisp-pack auto)
  (files prim prim-edops pr00 constraints) ; added pr00 - cebrown 12/1/00
  (mhelp "Creates primitive-substitution tool."))

;(defmodule cpk-primitive-subst
;  (lisp-pack auto)
;  (macro-files  cpk-prim-exp cpk-primprim-3)
;  (files cpk-primflav cpk-primprint cpk-primsub-3 cpk-primed)
;  (mhelp "Carl's primitive substitution module."))

(defmodule report
  (needed-modules event-signal etps-events)
  (lisp-pack teacher)
  (files report report-stats report-init)
  (mhelp "The REPORT module."))

(defmodule replace
  (needed-modules wff-editor)
  (lisp-pack core)
  (files replace)
  (mhelp "Replacement of symbols by equivalent wffs."))

;(defmodule tait
;  (needed-modules wff-editor tex-wff) 
;  (lisp-pack tait)
;  (macro-files taitdef)
;  (files taittop taitprt taitrule)
;  (mhelp "Defines TAIT-style proofs."))

(defmodule tactics
  (needed-modules otlnl otlrulep)
  (lisp-pack auto)
  (macro-files tactics-macros tacticals-macros)
  (files tacticals tactics-aux)
  (mhelp "Defines functions needed to use tactics and tacticals."))

(defmodule etr-nat
  (needed-modules mating-transform tactics )
  (lisp-pack auto)
  (files etr-nat-macros diy nat-etr symsimp symsimp2 etrees-auto-suggest ftree-seq
	 hx-natree-top ceb-nat-seq ceb-nat-etr lemmas)
  (mhelp "Defines functions needed for conversion from expansion tree proofs
to natural deduction proofs and vice versa."))

(defmodule ml-tactics
  (needed-modules tactics math-logic-2-rules)
  (lisp-pack ml)
  (files ml-tactics-aux ml-tactics-prop ml-tactics-quant)
  (mhelp "Defines tactics for natural deduction proofs using 
math logic II rules."))

(defmodule tactics-nd
  (needed-modules ml-tactics)
  (lisp-pack ml)
  (files master-tactic)
  (mhelp "Defines higher-level tactics for natural deduction proofs using 
math logic II rules."))

(defmodule ml-etr-tactics
  (needed-modules etr-nat math-logic-2-rules)
  (lisp-pack ml)
  (files ml-etr-tactics-main ml-etr-tactics-pline ml-etr-tactics-sline
         ml-etr-tactics-book ml-etr-tactics-eq ml-etr-tactics-neg
	 ml-etr-tactics-symsimp ml-etr-tactics-symsimp2
	 ml-nat-etr1 ml-nat-etr2 ;;; you can delete most functions in these two files: they are no longer needed.
	 hx-natree-duplication hx-natree-rulep
         hx-natree-aux hx-natree-cleanup hx-natree-debug)
  (mhelp "Defines tactics for translating between expansion proofs and
natural deduction proofs using math logic II rules."))

(defmodule unification
  (lisp-pack auto)
  (needed-modules auto-basic)
  (files unif-lambda unif-simpl unif-match unif-tree unif-aux unif-subs
	 unif-menus) ; 5/6/02
  (mhelp "The higher-order unification module."))

(defmodule unification-interface
  (lisp-pack auto)
  (needed-modules auto-basic unification tex-wff)
  (macro-files unif-macros)
  (files unif-top unif-user
	 unif-menus) ; cebrown 5/6/02
  (mhelp "Interface to higher-order unification module."))

;;;PBA added prfw 1992-Sept-8
(defmodule xwindows
  (lisp-pack core)
  (needed-modules wff-parse)
  (files xterm prfw)
  (mhelp "Files which allow the use of the X window system."))

(defmodule library
  (lisp-pack core)
  (needed-modules review-flags wff-parse unification)
  (macro-files lib-macros)
  (files lib-ops lib-objects library1 library2 library3 test-top-lib lib-bug
	 unix-library1
	 lib-menus unix-lib-menus) ; cebrown 5/6/02
  (mhelp "Files which allow the use of LIBRARY module."))

(defmodule ms89
  (lisp-pack auto)
  (needed-modules ms88)
  (macro-files option-tree-macros)
  (files option-tree option-tree-aux option-tree-mateops option-tree-search)
  (mhelp "Files which define option trees and their use in searching
for an expansion proof."))

(defmodule ms90-9
  (lisp-pack auto)
  (needed-modules ms89 ms90-3)
  (files ms90-9)
  (mhelp "Defines functions for integrating option trees with the search
procedure ms90-3."))

(defmodule ms91
  (needed-modules ms89 ms90-9)
  (lisp-pack auto)
  (macro-files ms91-basic ms91-weights)
  (files ms91-enumerate ms91-search)
  (mhelp "Files needed to run ms91-6 and ms91-7."))


(defmodule ml2-rewrite
  (needed-modules otlsuggest math-logic-2-wffs theorems replace rrules)   
  (lisp-pack ml)
  (files ml2-rewrite)
  (mhelp "Rewrite rules for ND proofs."))

(defmodule rrules
  (needed-modules library wff-editor)
  (lisp-pack core)
  (macro-files lib-objects2)
  (files edrew)
  (mhelp "Files defining rewrite rules."))

(defmodule ms98
  (needed-modules expansion-tree skolemizing)
  (lisp-pack auto)
  (macro-files ms98-macros)
  (files ms98-weights ms98-top ms98-unif ms98-dagify ms98-jform ms98-dups ms98-paths ms98-rewrite
	 ms98-rewrite2 ms98-paths2)
  (mhelp "The mating search module MS98.  This search procedure
implements component search with rewriting of equalities."))

(defmodule semantics
  (needed-modules )
  (lisp-pack auto)
  (macro-files semantics-macros)
  (files models)
  (mhelp "The module for code dealing with semantics of higher-order logic.
This includes the MODELS top level for experimenting with standard models
where the base types (hence all types) are a power of 2."))

#+(and allegro-version>= (version>= 5 0))
(defmodule external-interface
  (needed-modules)
  (lisp-pack core)
  (files external-interface)
  (mhelp "Files for using an external interface, e.g., the Java interface."))

#-(and allegro-version>= (version>= 5 0))
(defmodule external-interface
  (needed-modules)
  (lisp-pack core)
  (files )
  (mhelp "Files for using an external interface, e.g., the Java interface."))

#+(and allegro-version>= (version>= 5 0))
(defmodule external-services
  (needed-modules etr-nat external-interface)
  (lisp-pack auto)
  (files socket process serv tps-processes external)
  (mhelp "Files for providing services for external programs such as Omega and to access MathWeb services."))

#-(and allegro-version>= (version>= 5 0))
(defmodule external-services
  (needed-modules etr-nat external-interface)
  (lisp-pack auto)
  (files )
  (mhelp "Files for providing services for external programs such as Omega and to access MathWeb services."))

(defmodule s-eqn
  (needed-modules)
  (lisp-pack auto)
  (macro-files s-eqn-macros)
  (files s-eqn-top s-eqn-rew s-eqn-prfw)
  (mhelp "The REWRITING top level."))
