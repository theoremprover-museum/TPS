;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :core)
(part-of tpsdef)

(deffile contexts-core
  (part-of tpsdef)
  (extension lsp)
  (mhelp "Defines contexts used in the CORE package."))

(defcontext subtoplevels
  (short-id "Top Levels")
  (order 1)
  (mhelp "TPS objects having to do with flow of control between top levels."))

(defcontext flags
  (short-id "Review")
  (order 5)
  (mhelp "A TPS object connected to REVIEW."))

(defcontext style
  (short-id "Style")
  (order 4.9)
  (mhelp "A TPS object associated with STYLE."))

(defcontext flag-review
  (short-id "Flags")
  (order 6)
  (mhelp "Examining and changing flags."))

(defcontext flag-modes
  (short-id "Modes")
  (order 7)
  (mhelp "Defining, saving, and switching modes."))

(defcontext rd-help
  (short-id "Reading help")
  (order 10)
  (mhelp "Concerning the automatic reading of help messages."))

(defcontext help-obj
  (short-id "Help")
  (order 11)
  (mhelp "TPS object providing help or giving information."))

(defcontext concept-terminal
  (short-id "Concept")
  (order 20)
  (mhelp "TPS objects dealing with the Concept terminal."))

(defcontext otl-entering
  (short-id "Starting and Finishing")
  (order 25)
  (mhelp "Commands for entering and leaving ETPS."))

(defcontext otl-object
  (short-id "OTL Object")
  (order 27)
  (mhelp "Objects from the outline package."))

(defcontext otl-printing
  (short-id "Printing")
  (order 30)
  (mhelp "Commands for looking at the proof outline."))

(defcontext wff-printing
  (short-id "Printing")
  (order 31)
  (mhelp "TPS objects which have to do with printing of wffs."))

(defcontext print-internals
  (short-id "Internal for Printing")
  (order 32)
  (mhelp "Operations used internally for printing purposes."))

(defcontext sail-chars
  (short-id "SAIL characters")
  (order 33)
  (mhelp
   "Related to the special characters in the SAIL character set."))

(defcontext script-letters
  (short-id "Script Letters")
  (order 34)
  (mhelp "Uppercase script letters."))

(defcontext subscripts
  (short-id "Subscripts")
  (order 35)
  (mhelp "Non-greek subscript symbols."))

(defcontext superscripts
  (short-id "Superscripts")
  (order 36)
  (mhelp "Symbols which print as superscripts."))

(defcontext greek-letters-lowercase
  (short-id "Lowercase Greek")
  (order 37)
  (mhelp "Lowercase Greek letters."))

(defcontext greek-letters-uppercase
  (short-id "Uppercase Greek")
  (order 38)
  (mhelp "Uppercase Greek letters."))

(defcontext greek-subscripts
  (short-id "Greek Subscripts")
  (order 39)
  (mhelp "Greek Subscripts as used for type symbols."))

(defcontext bold-letters
  (short-id "Bold Letters")
  (order 40)
  (mhelp "Upper case very bold letters."))

(defcontext tex-style
  (short-id "TeX")
  (order 41)
  (mhelp "TPS objects having to do with the TeX output style."))

(defcontext xwindows
  (short-id "X Windows")
  (order 41.5)
  (mhelp "TPS objects related to the use of the X window system."))

(defcontext misc-symbols
  (short-id "Other Symbols")
  (order 42)
  (mhelp
   "Other symbols, which are not superscripts, subscripts or letters."))

(defcontext weak-labels
  (short-id "Weak Labels")
  (order 50)
  (mhelp "Related to WEAK labels (which dissolve under substitution)."))

(defcontext flavor-obj
  (short-id "Flavors of Labels")
  (order 51)
  (mhelp "TPS objects dealing with flavors of labels."))

(defcontext save-work-obj
  (short-id "Saving Work")
  (order 55)
  (mhelp "TPS objects concerning saving and restoring work."))

(defcontext saving-wffs
  (short-id "Saving Wffs")
  (order 56)
  (mhelp "Having to do with writing weak labels to a file."))

(defcontext scribe-record
  (short-id "Recording")
  (order 57)
  (mhelp "TPS Objects concerned with recording wffs into files."))

(defcontext otl-files
  (short-id "Printing Proofs into Files")
  (order 60)
  (mhelp "Dealing with writing files in the outline package."))

(defcontext proof-outline
  (short-id "Proof Outline")
  (order 61)
  (mhelp "Objects used in proof outlines."))

(defcontext otl-rearranging
  (short-id "Rearranging the Proof")
  (order 63)
  (mhelp "Commands for rearranging the proof outline."))

(defcontext otl-status
  (short-id "Status")
  (order 64)
  (mhelp
   "Commands for looking at the status information for the proof outline."))

(defcontext tps-theorems
  (short-id "Theorems")
  (order 90)
  (mhelp "Having to do with theorems."))

(defcontext editor-obj
  (short-id "Wff Editor")
  (order 100)
  (mhelp "TPS objects connected with the wff editor."))

(defcontext well-ff
  (short-id "well-formed formula")
  (order 102)
  (mhelp "Having to do with well-formed formulae."))

(defcontext prim-obj
  (short-id "wff Primitives")
  (order 103)
  (mhelp "TPS objects connected to primitives concerning wffs."))

(defcontext wff-parsing
  (short-id "Wff Parsing")
  (order 104)
  (mhelp "TPS object related to the parsing of wffs."))

(defcontext wffequal
  (short-id "Equality between Wffs")
  (order 105)
  (mhelp "Test for equality between wffs and related normalizations."))

(defcontext wfftst-obj
  (short-id "Predicates on Wffs")
  (order 106)
  (mhelp "TPS objects concerning predicates on wffs."))

(defcontext wfftyp-obj
  (short-id "Wff Types")
  (order 107)
  (mhelp "TPS objects concerning types of wffs."))

(defcontext moving
  (short-id "Moving Commands")
  (order 108)
  (mhelp "Commands which move around in a wff."))

(defcontext changing
  (short-id "Changing Commands")
  (order 108.1)
  (mhelp "Commands which change wffs."))

(defcontext recursively-changing
  (short-id "Recursively Changing Commands")
  (order 108.2)
  (mhelp "Commands which change a wff as well as the subwffs of the wff."))

(defcontext embedding
  (short-id "Embedding Commands")
  (order 108.3)
  (mhelp "Commands which embed a wff within a quantifier or connective."))

(defcontext rewriting
  (short-id "Rewriting commands")
  (order 108.4)
  (mhelp "Commands to do with rewriting wffs."))

(defcontext substitution
  (short-id "Substitution")
  (order 109)
  (mhelp "TPS objects doing substitution in and for wffs."))

(defcontext abbrev-ops
  (short-id "Basic Abbreviations")
  (order 110)
  (mhelp "TPS objects having to do with logical abbreviations."))

(defcontext lambda-op
  (short-id "Lambda-Calculus")
  (order 112)
  (mhelp "TPS object dealing with operations in the lambda-calculus."))

(defcontext neg-ops
  (short-id "Negation movers")
  (order 113)
  (mhelp
   "Change scopes of negations. May later be part of similar
context for quantifiers."))

(defcontext misc-edops
  (short-id "Miscellaneous")
  (order 114)
  (mhelp "Edops dealing with miscellaneous operations on gwffs."))

(defcontext develop-seqs
  (short-id "Quantifier Commands")
  (order 117)
  (mhelp "TPS objects having to do with development sequences."))

(defcontext ill-formed
  (short-id "Wellformedness")
  (order 119)
  (mhelp "TPS objects dealing with potentially ill-formed formulas."))

(defcontext suggestions
  (short-id "suggestions")
  (order 61.93)
  (mhelp "Concerning automatic suggestions in the outline package."))

(defcontext tps-events
  (short-id "Events")
  (order 135)
  (mhelp "Having to do with events."))

(defcontext basics
  (short-id "Basics")
  (order 141)
  (mhelp "Basic TPS objects (inside the package BARE)."))

(defcontext rule-commands
  (short-id "Rule Commands")
  (order 143)
  (mhelp "Commands implementing rule of inference."))

;;;The package has been used in boot1.lisp before being defined.
(defcontext lisp-packages
  (short-id "Lisp packages")
  (order 145)
  (mhelp "Functions relating to LISP packages."))

(defcontext lisp-source
  (short-id "Lisp Source")
  (order 195)
  (mhelp "Lisp source files."))

(defcontext command-declaration
  (short-id "Command declaration")
  (order 195)
  (mhelp "PCL and DCL files for creating commands on exec."))

(defcontext documentation
  (short-id "Documentation")
  (order 195)
  (mhelp "Files for TPS documentation."))

(defcontext batch
  (short-id "Batch Control")
  (order 195)
  (mhelp "Batch control files."))

(defcontext indirect
  (short-id "Indirect Files")
  (order 195)
  (mhelp "Files containing arguments for exec commands."))

(defcontext core-image
  (short-id "Core Images")
  (order 195)
  (mhelp "Executable files."))

(defcontext system-news
  (short-id "News")
  (order 195)
  (mhelp "News files for insiders. Note files for public notice."))

(defcontext miscellaneous
  (short-id "Miscellaneous")
  (order 200)
  (mhelp "Miscellaneous TPS objects."))

(defcontext unclassified
  (short-id "Unclassified")
  (order 201)
  (mhelp "TPS object not classified into any context."))

(defcontext library
  (short-id "Library")
  (order 203)
  (mhelp "Library objects."))

(defcontext lib-display 
  (short-id "Display")
  (order 205)
  (mhelp "Commands associated with displaying objects, especially library objects."))

(defcontext lib-reading
  (short-id "Reading")
  (order 207)
  (mhelp "Commands associated with reading library objects into TPS."))

(defcontext lib-structure
    (short-id "Library Structure")
  (order 208)
  (mhelp "Commands for manipulating the directory and file structure of the library."))

(defcontext lib-writing
  (short-id "Editing")
  (order 209)
  (mhelp "Commands associated with modifying library objects."))

(defcontext lib-bugs
  (short-id "Bugs")
  (order 211)
  (mhelp "Commands associated with reading and writing bug records."))

(defcontext lib-keys
  (short-id "Keywords")
  (order 210)
  (mhelp "Commands associated with keywords in the library"))

(defcontext lib-modes
  (short-id "Best modes")
  (order 210.5)
  (mhelp "Commands associated with best modes in the library"))

(defcontext lib-class
  (short-id "Library Classification")
  (order 210.6)
  (mhelp "Commands associated with Classification Schemes for the library."))

(defcontext interface
  (short-id "Interface")
  (order 211)
  (mhelp "Commands associated with TPS interfaces, e.g., the Java interface."))
