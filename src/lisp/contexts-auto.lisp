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

(deffile contexts-auto
  (part-of tpsdef)
  (extension lsp)
  (mhelp "Defines contexts used in the AUTO package."))

(defcontext jforms1
  (short-id "Vpforms")
  (order 62)
  (mhelp
   "Commands for converting wffs to jforms, converting jforms to wffs,
displaying jforms, and printing vertical path diagrams."))

(defcontext mating-search
  (short-id "Mating search")
  (order 61.9)
  (mhelp "Concerning mating search."))

(defcontext ms88
  (short-id "MS88 search procedure")
  (order 61.901)
  (mhelp "Concerning mating search procedure MS88."))

(defcontext ms89
  (short-id "MS89 search procedure")
  (order 61.902)
  (mhelp "Concerning mating search procedure MS89."))

(defcontext ms90-3
  (short-id "MS90-3 search procedure")
  (order 61.903)
  (mhelp "Concerning mating search procedure MS90-3."))

(defcontext ms90-9
  (short-id "MS90-9 search procedure")
  (order 61.904)
  (mhelp "Concerning mating search procedure MS90-9."))

(defcontext ms91
  (short-id "MS91-6 and MS91-7 search procedures")
  (order 61.905)
  (mhelp "Concerning mating search procedures MS91-6 and MS91-7."))

(defcontext ms92-9
  (short-id "MS92-9 search procedure")
  (order 61.906)
  (mhelp "Concerning mating search procedure MS92-9."))

(defcontext ms93-1
  (short-id "MS93-1 search procedure")
  (order 61.907)
  (mhelp "Concerning mating search procedure MS93-1."))

(defcontext ms98-1
  (short-id "MS98-1 search procedure")
  (order 61.908)
  (mhelp "Concerning mating search procedure MS98-1."))

(defcontext etr-nat
  (short-id "Proof Translation")
  (order 61.91)
  (mhelp "Concerning translation between expansion proofs and natural
deduction proofs."))

; cebrown 4/12/00
(defcontext search-analysis
    (short-id "Search Analysis")
  (order 61.92)
  (mhelp "Concerning analyzing the search for automatic proofs."))

(defcontext test-searchlists
  (short-id "Searchlists")
  (order 61.95)
  (mhelp "Concerning construction of test-top searchlists."))

(defcontext test-lib
  (short-id "Library")
  (order 61.96)
  (mhelp "Concerning library objects in the test-top top level."))

(defcontext primsubs
  (short-id "Primitive Substitutions")
  (order 114)
  (mhelp "For creating substitutable wffs."))

(defcontext rulep-test
  (short-id "RuleP")
  (order 115)
  (mhelp "Concerning testing of tautologies."))

(defcontext skolems
  (short-id "Skolemizing")
  (order 116)
  (mhelp "Having to do with Skolem functions and Skolemizing."))

(defcontext expansion-trees
  (short-id "Expansion Trees")
  (order 61.8)
  (mhelp "TPS objects dealing with expansion trees."))

(defcontext tactics
  (short-id "Tactics")
  (order 61.92)
  (mhelp "Tactics and related functions."))

(defcontext prop-tactics
  (short-id "Propositional")
  (order 1191/10)
  (mhelp "Tactics which carry out propositional rules."))

(defcontext quant-tactics 
  (short-id "Quantifiers")
  (order 1192/10)
  (mhelp "Tactics which operate on quantifiers."))

(defcontext equality-tactics 
  (short-id "Equality")
  (order 1193/10)
  (mhelp "Tactics which use equality rules."))

(defcontext defn-tactics 
  (short-id "Definitions")
  (order 1194/10)
  (mhelp "Tactics which handle wff definitions."))

(defcontext lambda-tactics 
  (short-id "Lambda")
  (order 1195/10)
  (mhelp "Tactics which use lambda-calculus operations."))

(defcontext compound-tactics
  (short-id "Compound")
  (order 11901/100)
  (mhelp "Compound tactics."))

(defcontext aux-tactics
  (short-id "Auxiliary")
  (order 1197/10)
  (mhelp "Auxiliary tactics."))

(defcontext unification
  (order 61.911)
  (short-id "Unification")
  (mhelp "Commands for unification."))

(defcontext unification-dpairs
  (order 61.912)
  (short-id "Dpairs")
  (mhelp "Disagreement pairs in the unification problems."))

(defcontext mtree-ops
  (short-id "Mtree Operations")
  (order 61.81)
  (mhelp "TPS objects dealing with manipulating matingstrees."))

(defcontext mtree-print
  (short-id "Mtree Printing")
  (order 61.82)
  (mhelp "TPS objects dealing with displaying matingstrees."))

(defcontext mtree-auto
  (short-id "Mtree Auto")
  (order 61.83)
  (mhelp "Automatic commands to do with matingstrees."))


(defcontext search-suggestions
  (short-id "Search Suggestions")
  (order 61.84)
  (mhelp "Flag setting suggestions for automatic search."))

(defcontext dec-frags
    (short-id "Decidable Fragments")
  (order 61.85)
  (mhelp "Functions related to decidable fragments of type theory."))

(defcontext pers
    (short-id "Per Refined Models")
  (order 61.86)
  (mhelp "Computing with Per Refined Models of Type Theory."))

(defcontext ext-exp-dags
  (short-id "Extensional Expansion Dags")
  (order 62)
  (mhelp "TPS objects dealing with extensional expansion dags."))

(defcontext ext-seq
  (short-id "Extensional Sequent Calculus")
  (order 62)
  (mhelp "TPS objects dealing with extensional sequent derivations"))

(defcontext ext-seq-entering
  (short-id "Extensional Sequent Entering")
  (order 62.1)
  (mhelp "Functions for starting and manipulating extensional sequent derivations"))

(defcontext ext-seq-printing
  (short-id "Extensional Sequent Printing")
  (order 62.2)
  (mhelp "Printing functions for extensional sequent derivations"))

(defcontext ext-seq-rules
  (short-id "Extensional Sequent Rules")
  (order 62.3)
  (mhelp "Rules for extensional sequent derivations"))

(defcontext ext-seq-derived-rules
  (short-id "Extensional Sequent Derived Rules")
  (order 62.4)
  (mhelp "Derived rules for extensional sequent derivations"))

(defcontext ext-seq-tactics
  (short-id "Extensional Sequent Tactics")
  (order 62.5)
  (mhelp "Tactics for extensional sequent derivations"))

(defcontext ext-seq-files
  (short-id "Extensional Sequent Files")
  (order 63)
  (mhelp "Commands dealing with files for extensional sequent derivations"))

(defcontext ext-exp-dags
  (short-id "Extensional Expansion Dags")
  (order 64)
  (mhelp "Extensional Expansion Dags"))

(defcontext ext-search
  (short-id "Extensional Search")
  (order 61.909)
  (mhelp "Extensional Search"))

(defcontext semantics
  (short-id "Semantics")
  (order 66)
  (mhelp "Semantics"))

(defcontext models
  (short-id "Models")
  (order 66.2)
  (mhelp "Models"))

(defcontext log-relns
  (short-id "Logical Relations")
  (order 66.4)
  (mhelp "Logical Relations on Models"))

(defcontext s-eqn
  (short-id "Rewriting Toplevel")
  (order 67)
  (mhelp "Rewriting in the simply typed lambda-calculus"))

(defcontext s-eqn-entering
  (short-id "Starting and Finishing")
  (order 67.1)
  (mhelp "Functions for starting and manipulating derivations"))

(defcontext s-eqn-printing
  (short-id "Printing")
  (order 67.2)
  (mhelp "Printing functions for equational derivations"))

(defcontext s-eqn-axioms
  (short-id "Equational Axioms")
  (order 67.3)
  (mhelp "Equational axioms for the simply typed lambda-calculus"))

(defcontext s-eqn-rules
  (short-id "Applying Rules")
  (order 67.4)
  (mhelp "Rules for equational derivations"))

(defcontext s-eqn-rearrange
  (short-id "Rearranging the Derivation")
  (order 67.5)
  (mhelp "Rules for rearranging equational proofs"))

(defcontext s-eqn-lambda
  (short-id "Lambda Conversion")
  (order 67.6)
  (mhelp "Rules for applying lambda conversion within equational proofs"))

(defcontext s-eqn-theories
  (short-id "Theories")
  (order 67.7)
  (mhelp "Loading, saving and modifying rewrite theories"))
