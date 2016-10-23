;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of EXT-DAGS)

;;;
;;; File: MS04-SEARCH  - cebrown - 2/04

(deffile ms04-search
    (part-of EXT-DAGS)
  (extension clisp)
  (mhelp "File dealing with MS04-2 search using extensional expansion DAG's."))

(context ext-search)

(definfo ms04-2
  (mhelp "A setting for DEFAULT-MS, DEFAULT-MATE and DEFAULT-EXPAND.
This uses the MS04-2 mating search procedure which incorporates 
extensionality reasoning, equality reasoning, and set variable reasoning 
as described in Chad E. Brown's thesis.

The search procedures MS03-7 and MS04-2 are similar in that they are
both extensional search procedures.  MS03-7 performs a kind of saturation search.
MS04-2 performs a depth-first search (with weights to control the order of choices)
with backtracking and a depth bound.  Iterative deepening is used to ensure
completeness.

MS04-2 is proven complete in Chad E. Brown's thesis.

See Also: MS03-7."))

(defflag MS04-VERBOSE
  (default MED)
  (flagtype symbol)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
;  (irrelevancy-preconditions ; actually, ms04-verbose may cause effects if we're lifting in the EXT-MATE level
;   (default-ms (not (eq default-ms 'ms04-2))))
  (mhelp "Determines level of verbosity of MS04-2 search.
Value should be MIN, MED or MAX."))

(defflag MS04-TRACE
  (default nil)
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (not (eq default-ms 'ms04-2))))
  (mhelp "If T, MS04-2 will gather information about the search which will be
used to suggest values for flag settings (if search is successful)."))

(defflag MS04-BACKTRACK-METHOD
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 1)
  (mhelp "Determines which choices are used for backtracking.

1.  Backtrack on all choices.

2.  Do not backtrack over connections.

3.  Do not backtrack over connections or duplications."))

(defflag MS04-MP-OPTIONS
  (flagtype symbollist)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (not (eq default-ms 'ms04-2))))
  (default NIL)
  (mhelp "In Allegro, any MS04-2 option listed in the value of this flag will
cause TPS to use multiprocessing to consider the option in parallel to
consideration of other options.

The main MS04-2 options which may be included on the list are DUP,
PRIMSUB and ADD-SET-CONSTRAINT.  Other MS04-2 options which may be
included are MATE, EUNIF1, EUNIF2, SUBST, MATE-FLEXRIGID,
MATE-FLEXRIGID-NEG, MATE-FLEXRIGID-PROJ, MATE-FLEXRIGID-NEG-PROJ,
FLEX-EUNIF, PRIMSUB-QUANT-GENTP, DELAY-UNIF, DELAY-CONN and
SOLVE-SET-CONSTRAINTS."))

(defflag MS04-INITIAL-DEPTH
  (flagtype posinteger)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (not (eq default-ms 'ms04-2))))
  (default 100)
  (mhelp "This sets the initial bound for the depth of the search procedure
MS04-2.  Once the search to this depth has failed, MS04-INCR-DEPTH
is used to increase the bound.

SEE ALSO: MS04-INCR-DEPTH, MS04-MAX-DEPTH"))

(defflag MS04-INCR-DEPTH
  (flagtype posinteger)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (memq default-ms '(ms04-2)) (not max-search-limit)))
   (max-search-limit (and (memq default-ms '(ms04-2)) (not max-search-limit))))
  (irrelevancy-preconditions
   (default-ms (or (not (memq default-ms '(ms04-2))) max-search-limit))
   (max-search-limit (or (not (memq default-ms '(ms04-2))) max-search-limit)))
  (default 100)
  (mhelp "Every time MS04-2 has completed the search space up to a given bound,
the bound is increased by MS04-INCR-DEPTH.

SEE ALSO: MS04-INITIAL-DEPTH, MS04-MAX-DEPTH"))

(defflag MS04-MAX-DEPTH
  (flagtype integer+-or-infinity)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (memq default-ms '(ms04-2)) (not max-search-limit)))
   (max-search-limit (and (memq default-ms '(ms04-2)) (not max-search-limit))))
  (irrelevancy-preconditions
   (default-ms (or (not (memq default-ms '(ms04-2))) max-search-limit))
   (max-search-limit (or (not (memq default-ms '(ms04-2))) max-search-limit)))
  (default infinity)
  (mhelp "This sets an absolute maximum on the depth of the search.
For completeness, this should be set to infinity.

SEE ALSO: MS04-INITIAL-DEPTH, MS04-INCR-DEPTH"))

(defflag MS04-DUP-WEIGHT
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 300)
  (mhelp "Sets the weight for duplicating an expansion node in MS04-2.
This controls how often MS04-2 will duplicate expansion nodes.  The higher
the weight, the less often duplication occurs."))

(defflag MS04-WEIGHT-MULTIPLE-MATES
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 10)
  (mhelp "This controls the extra weight every time a node is mated more than once.
This is similar to MAX-MATES."))

(defflag MS04-WEIGHT-MULTIPLE-EUNIF1S
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 10)
  (mhelp "This controls the extra weight every time a node is eunified more than once.
This is similar to MAX-MATES."))

(defflag MS04-WEIGHT-MULTIPLE-EUNIF2S
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 10)
  (mhelp "This controls the extra weight every time a node is symmetrically eunified
more than once.  This is similar to MAX-MATES."))

(defflag MS04-PRENEX-PRIMSUBS
  (flagtype boolean)
  (subjects ext-search ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default t)
  (mhelp "If T, only primsubs in conjunctive-prenex normal forms will be generated."))

(defflag MS04-WEIGHT-PRIMSUB-NEXTTP
  (flagtype integer+)
  (subjects ext-search ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 100)
  (mhelp "Sets the weight for each higher type we generate for a primsub using
either FORALL or EXISTS.  This controls how often MS04-2 will use
primsubs with higher types.  The higher the weight, the less often
higher types are used."))

(defflag MS04-WEIGHT-PRIMSUB-FIRST-NOT
  (flagtype integer+)
  (subjects ext-search ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) (null ms04-prenex-primsubs)))
   (ms04-prenex-primsubs (and (eq default-ms 'ms04-2) (null ms04-prenex-primsubs))))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-prenex-primsubs ms04-prenex-primsubs))
  (default 1000)
  (mhelp "Controls when MS04-2 first tries a primsub using a negation.

This is only used when MS04-PRENEX-PRIMSUBS is NIL."))

(defflag MS04-WEIGHT-PRIMSUB-NEXT-NOT
  (flagtype integer+)
  (subjects ext-search ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) (null ms04-prenex-primsubs)))
   (ms04-prenex-primsubs (and (eq default-ms 'ms04-2) (null ms04-prenex-primsubs))))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-prenex-primsubs ms04-prenex-primsubs))
  (default 1000)
  (mhelp "Controls when MS04-2 tries a primsub using a negation after the first time.

This is only used when MS04-PRENEX-PRIMSUBS is NIL."))

(defflag MS04-WEIGHT-PRIMSUB-OCCURS-CONST
  (flagtype integer+)
  (subjects ext-search ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) (null ms04-prenex-primsubs)))
   (ms04-prenex-primsubs (and (eq default-ms 'ms04-2) (null ms04-prenex-primsubs))))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-prenex-primsubs ms04-prenex-primsubs))
  (default 1000)
  (mhelp "Some logical constants occur embedded in the terms of a theorem.
This flag controls when MS04-2 tries a primsub using one of these logical constants
if the logical constant will not be tried by other primsubs.
This is only used if MS04-PRENEX-PRIMSUBS is NIL.

See Also: MS04-PRENEX-PRIMSUBS"))

(defflag MS04-WEIGHT-EUNIF-DECS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 1000)
  (mhelp "Controls how often EUnification is applied to equation goals that are
decomposable, i.e., have shallow formula of the form:

     [H . . .] = [H . . .]

There are cases where one needs to do this, but often one wants to avoid it."))

(defflag MS04-WEIGHT-EUNIF-DIFF-HEADS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 2000)
  (mhelp "An extra weight on EUNIF1 steps of the form [A = B]^+ to [C = D]^-
where the heads of A and C are different and the heads of B and D
are different.  The weight is also added to EUNIF2 steps when the
heads A and D are different and the heads of B and C are different."))

(defflag MS04-DUP-EARLY
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default NIL)
  (mhelp "If set to T, MS04-2 will only duplicate expansion nodes before making
any substitutions or connections (on paths that share the expansion
node).  Originally, MS04-2 always did this, but only MS04-2 with
duplications allowed anytime (when the value of MS04-DUP-EARLY is NIL)
is shown complete in Chad E. Brown's thesis."))

(defflag MS04-DELAY-UNIF-CONSTRAINTS
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default T)
  (mhelp "If set to T, the MS04-2 search procedure will delay
considering vertical paths that contain certain equation goals
which are being used to weight further options.  The procedure
is complete with this set to T or NIL.  Setting it to T creates
more nondeterminism, but can lead to faster proofs."))

(defflag MS04-EAGER-UNIF-SUBST
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints))
   (ms04-delay-unif-constraints (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-delay-unif-constraints (not ms04-delay-unif-constraints)))
  (default T)
  (mhelp "If set to T (and MS04-DELAY-UNIF-CONSTRAINTS is T), the MS04-2 search
procedure will substitute for parts of the pattern part of the current
unification problem."))

(defflag MS04-WEIGHT-DELAY-UNIF
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints))
   (ms04-delay-unif-constraints (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-delay-unif-constraints (not ms04-delay-unif-constraints)))
  (default 0)
  (mhelp "If MS04-DELAY-UNIF-CONSTRAINTS is T, this weight is used to determine
when to add an equation goal node to the collection of delayed
unification constraints.

See Also: MS04-DELAY-UNIF-CONSTRAINTS"))

(defflag MS04-SOLVE-UNIF-DEPTH
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints))
   (ms04-delay-unif-constraints (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-delay-unif-constraints (not ms04-delay-unif-constraints)))
  (default 5)
  (mhelp "If MS04-DELAY-UNIF-CONSTRAINTS is T, MS04-SOLVE-UNIF-DEPTH determines
how deeply MS04-2 will try to solve unification constraints after
every vertical path can be solved by the delayed unification
constraints.

See Also: MS04-DELAY-UNIF-CONSTRAINTS"))

(defflag MS04-CHECK-UNIF-DEPTH
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints))
   (ms04-delay-unif-constraints (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-delay-unif-constraints (not ms04-delay-unif-constraints)))
  (default 3)
  (mhelp "If MS04-DELAY-UNIF-CONSTRAINTS is T, MS04-CHECK-UNIF-DEPTH determines
how deeply MS04-2 will try to unify in order to prune out states where
the unification problem is unsolvable.

See Also: MS04-DELAY-UNIF-CONSTRAINTS"))

(defflag MS04-DELAY-FLEXRIGID-MATES
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints))
   (ms04-delay-unif-constraints (and (eq default-ms 'ms04-2) ms04-delay-unif-constraints)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-delay-unif-constraints (not ms04-delay-unif-constraints)))
  (default T)
  (mhelp "If MS04-DELAY-UNIF-CONSTRAINTS is T and MS04-DELAY-FLEXRIGID-MATES is T,
then potential connections between flexible nodes and atomic nodes are delayed
and the dpair is added to the unification problem.  In particular, this may
allow projections to be used to unify flexible nodes with atomic nodes."))

(defflag MS04-ALLOW-FLEX-EUNIFS
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default T)
  (mhelp "If MS04-ALLOW-FLEX-EUNIFS is T, then MS04-2 will try to mate
flexible nodes with positive equation nodes and negative equation goal nodes.
To do this, MS04-2 will imitate the equality (or negation of equality) first.
This is not necessary for completeness (since an equality primsub will eventually
be considered), but is sometimes helpful."))

(defflag MS04-ALLOW-FLEXRIGID-PROJ-MATE
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default T)
  (mhelp "If MS04-ALLOW-FLEXRIGID-PROJ-MATE is T, then MS04-2 will try to mate
flexible nodes with atoms using a projection.  This is not necessary
for completeness (since a projection primsub will eventually be
considered), but is sometimes helpful."))

(defflag MS04-WEIGHT-FLEX-EUNIF
  (flagtype integer+)
  (subjects ext-search ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEX-EUNIFS))
   (ms04-allow-flex-eunifs (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEX-EUNIFS)))
  (irrelevancy-preconditions
   (default-ms (not (eq default-ms 'ms04-2)))
   (ms04-allow-flex-eunifs (not MS04-ALLOW-FLEX-EUNIFS)))
  (default 2)
  (mhelp "This value is added to the weight for adding any connection between
any flexible literal and an equation."))

(defflag MS04-WEIGHT-FLEXRIGID-PROJ-MATE
  (flagtype integer+)
  (subjects ext-search ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEXRIGID-PROJ-MATE))
   (ms04-allow-flexrigid-proj-mate (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEXRIGID-PROJ-MATE)))
  (irrelevancy-preconditions
   (default-ms (not (eq default-ms 'ms04-2)))
   (ms04-allow-flexrigid-proj-mate (not MS04-ALLOW-FLEXRIGID-PROJ-MATE)))
  (default 2)
  (mhelp "This value is added to the weight for adding any connection between a
flexible literal and an atom using a projection on the head of the
flexible literal."))

(defflag MS04-USE-SET-CONSTRAINTS
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default NIL)
  (mhelp "If set to T, the MS04-2 search procedure will use set constraints
and set existence lemmas to solve for set variables."))

(defflag MS04-WEIGHT-ADD-SET-CONSTRAINT
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-use-set-constraints))
   (ms04-use-set-constraints (and (eq default-ms 'ms04-2) ms04-use-set-constraints)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-use-set-constraints (not ms04-use-set-constraints)))
  (default 1)
  (mhelp "If MS04-USE-SET-CONSTRAINTS is T, this weight is used to determine
when to add another constraint for a set variable.

See Also: MS04-USE-SET-CONSTRAINTS, MAX-NUM-CONSTRAINTS, MAX-CONSTRAINT-SIZE"))

(defflag MS04-WEIGHT-SOLVE-SET-CONSTRAINTS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-use-set-constraints))
   (ms04-use-set-constraints (and (eq default-ms 'ms04-2) ms04-use-set-constraints)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-use-set-constraints (not ms04-use-set-constraints)))
  (default 1)
  (mhelp "If MS04-USE-SET-CONSTRAINTS is T, this weight is used to determine
when to stop adding constraints for a set variable.

See Also: MS04-USE-SET-CONSTRAINTS, MAX-NUM-CONSTRAINTS, MAX-CONSTRAINT-SIZE"))

(defflag MS04-MAX-DUPS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 3)
  (mhelp "The maximum number of duplications MS04-2 will consider
(on the first iteration of search)."))

(defflag MS04-MAX-DELAYED-CONNS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 10)
  (mhelp "The maximum number of delayed connections (waiting to be unified)
MS04-2 will consider (on the first iteration of search)."))

(defflag MS04-MAX-RIGID-MATES
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 10)
  (mhelp "The maximum number of mates between nodes which are already rigid
MS04-2 will consider (on the first iteration of search)."))

(defflag MS04-MAX-EUNIF1S
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 3)
  (mhelp "The maximum number of E-unification connections MS04-2 will consider
(on the first iteration of search)."))

(defflag MS04-MAX-EUNIF2S
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 3)
  (mhelp "The maximum number of symmetric E-unification connections MS04-2 will consider
(on the first iteration of search)."))

(defflag MS04-MAX-FLEXRIGID-MATES
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 5)
  (mhelp "The maximum number of mates between a flexible node and a rigid atom of opposite polarity 
MS04-2 will consider (by imitating the head of the rigid atom).
This value is increased by 1 after each failed iteration of the search."))

(defflag MS04-MAX-FLEXRIGID-NEG-MATES
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 5)
  (mhelp "The maximum number of mates between a flexible node and a rigid atom of the same polarity 
MS04-2 will consider (by using a negation and imitating the head of the rigid atom).
This value is increased by 1 after each failed iteration of the search."))

(defflag MS04-MAX-FLEXRIGID-PROJ-MATES
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEXRIGID-PROJ-MATE))
   (ms04-allow-flexrigid-proj-mate (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEXRIGID-PROJ-MATE)))
  (irrelevancy-preconditions
   (default-ms (not (eq default-ms 'ms04-2)))
   (ms04-allow-flexrigid-proj-mate (not MS04-ALLOW-FLEXRIGID-PROJ-MATE)))
  (default 5)
  (mhelp "The maximum number of mates between a flexible node and a rigid atom of opposite polarity 
MS04-2 will consider using projections instead of imitations.
This flag is only relevant if MS04-ALLOW-FLEXRIGID-PROJ-MATE is T."))

(defflag MS04-MAX-FLEXRIGID-NEG-PROJ-MATES
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEXRIGID-PROJ-MATE))
   (ms04-allow-flexrigid-proj-mate (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEXRIGID-PROJ-MATE)))
  (irrelevancy-preconditions
   (default-ms (not (eq default-ms 'ms04-2)))
   (ms04-allow-flexrigid-proj-mate (not MS04-ALLOW-FLEXRIGID-PROJ-MATE)))
  (default 5)
  (mhelp "The maximum number of mates between a flexible node and a rigid atom of the same polarity 
MS04-2 will consider using projections with a negation instead of imitations.
This flag is only relevant if MS04-ALLOW-FLEXRIGID-PROJ-MATE is T."))

(defflag MS04-MAX-FLEX-EUNIFS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEX-EUNIFS))
   (ms04-allow-flex-eunifs (and (eq default-ms 'ms04-2) MS04-ALLOW-FLEX-EUNIFS)))
  (irrelevancy-preconditions
   (default-ms (not (eq default-ms 'ms04-2)))
   (ms04-allow-flex-eunifs (not MS04-ALLOW-FLEX-EUNIFS)))
  (default 2)
  (mhelp "The maximum number of times MS04-2 will instantiate the head
of a flexible node with an equality of base type (or the negation of an equality)
in order to E-unify the instantiated node with a positive equation node or an equation goal node.
This flag is only relevant if MS04-ALLOW-FLEX-EUNIFS is set to T."))

(defflag MS04-MAX-IMITS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 10)
  (mhelp "The maximum number of imitations (for unification) MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PROJS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 10)
  (mhelp "The maximum number of projections (for unification) MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-NOT
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) (null ms04-prenex-primsubs)))
   (ms04-prenex-primsubs (and (eq default-ms 'ms04-2) (null ms04-prenex-primsubs))))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-prenex-primsubs ms04-prenex-primsubs))
  (default 1)
  (mhelp "The maximum number of negation primsubs MS04-2 will attempt during an iteration of the search.
Negation primsubs are only tried if MS04-PRENEX-PRIMSUBS is NIL.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-PROJ
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 1)
  (mhelp "The maximum number of projection primsubs MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-NOT-PROJ
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-prenex-primsubs))
   (ms04-prenex-primsubs (and (eq default-ms 'ms04-2) ms04-prenex-primsubs)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-prenex-primsubs (not ms04-prenex-primsubs)))
  (default 1)
  (mhelp "The maximum number of negated projection primsubs MS04-2 will attempt during an iteration of the search.
Negated projection primsubs are only tried if MS04-PRENEX-PRIMSUBS is T.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-EQUALS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 1)
  (mhelp "The maximum number of primsubs using equality (at base type) MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-NOT-EQUALS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-prenex-primsubs))
   (ms04-prenex-primsubs (and (eq default-ms 'ms04-2) ms04-prenex-primsubs)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-prenex-primsubs (not ms04-prenex-primsubs)))
  (default 1)
  (mhelp "The maximum number of primsubs using negated equality (at base type) MS04-2 will attempt during an iteration of the search.
Negated equality primsubs are only tried if MS04-PRENEX-PRIMSUBS is T.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-AND
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 1)
  (mhelp "The maximum number of conjunction primsubs MS04-2 will attempt during an iteration of the search.
Conjunction primsubs are only tried if MS04-PRENEX-PRIMSUBS is T.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-OR
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 1)
  (mhelp "The maximum number of disjunction primsubs MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-FORALL
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 1)
  (mhelp "The maximum number of FORALL primsubs MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration."))

(defflag MS04-MAX-PRIMSUB-EXISTS
  (flagtype integer+)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default 1)
  (mhelp "The maximum number of EXISTS primsubs MS04-2 will attempt during an iteration of the search.
Conjunction primsubs are only tried if MS04-PRENEX-PRIMSUBS is T.
The value is increased by 1 after every failed iteration."))

(defflag MS04-USE-SEMANTICS
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms04-2)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2)))
  (default NIL)
  (mhelp "If set to T, the MS04-2 search procedure will use semantics to
guide the search.

See Also: MODELS, MAX-DOMAIN-SIZE, MAX-BINDER-COMPUTATION"))

(defflag MS04-SEMANTIC-PRUNING
  (flagtype boolean)
  (subjects ext-search ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (and (eq default-ms 'ms04-2) ms04-use-semantics))
   (ms04-use-semantics (and (eq default-ms 'ms04-2) ms04-use-semantics)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms04-2))
   (ms04-use-semantics (not ms04-use-semantics)))
  (default NIL)
  (mhelp "If set to T, the MS04-2 search procedure will try to prune search
states using semantics.

See Also: MODELS, MAX-DOMAIN-SIZE, MAX-BINDER-COMPUTATION"))

;(defflag MS04-SEMANTIC-CONSTRAINTS
;  (flagtype integer+)
;  (subjects ext-search ms04-2 transmit)
;  (relevancy-preconditions
;   (default-ms (and (eq default-ms 'ms04-2) ms04-use-semantics))
;   (ms04-use-semantics (and (eq default-ms 'ms04-2) ms04-use-semantics)))
;  (irrelevancy-preconditions
;   (default-ms (neq default-ms 'ms04-2))
;   (ms04-use-semantics (not ms04-use-semantics)))
;  (default 10)
;  (mhelp "Use the standard model with 2 elements at base types to indicate
;if a collection of constraints is unsolvable.
;
;See Also: MS04-USE-SEMANTICS, MODELS, MAX-DOMAIN-SIZE, MAX-BINDER-COMPUTATION"))

;(defflag MS04-LR-PRIMSUBS
;  (flagtype integer+)
;  (subjects ext-search ms04-2 transmit)
;  (relevancy-preconditions
;   (default-ms (and (eq default-ms 'ms04-2) ms04-use-semantics))
;   (ms04-use-semantics (and (eq default-ms 'ms04-2) ms04-use-semantics)))
;  (irrelevancy-preconditions
;   (default-ms (neq default-ms 'ms04-2))
;   (ms04-use-semantics (not ms04-use-semantics)))
;  (default 100)
;  (mhelp "Use logical relations on the standard model with 2 elements at base
;types to weight possible primsubs for set variables.
;
;See Also: MS04-USE-SEMANTICS, MODELS, MAX-DOMAIN-SIZE, MAX-BINDER-COMPUTATION"))

;(defflag MS04-WEIGHT-EXTEND-CONSTRAINTS
;  (flagtype integer+)
;  (subjects ext-search ms04-2 transmit)
;  (relevancy-preconditions
;   (default-ms (eq default-ms 'ms04-2)))
;  (irrelevancy-preconditions
;   (default-ms (neq default-ms 'ms04-2)))
;  (default 200)
;  (mhelp ""))

(defun ms04-initialize-search (wff)
  (declare (special *edag-lift-info* *individual-types* *eeod-stamp*))
  (setq *eeod-stamp* 0)
  (setq *individual-types* (remove 'S (find-prim-types wff)))
  (unless *individual-types*
    (push 'CL-USER::I *individual-types*))
  (setq *extra-log-consts*
	(if MS04-PRENEX-PRIMSUBS
	    nil
	  (remove-duplicates
	   (ms04-find-extra-log-consts wff)
	   :test #'equal)))
  (dolist (x (free-vars-of wff))
    (setf (get x 'ext-exp-var) nil)
    (setf (get x 'exp-vars-above) nil))
  (if (or query-user MS04-TRACE)
      (setq *edag-lift-info* (intern (gensym "LIFT")))
    (setq *edag-lift-info* nil))
  (setq *ms04-search-done* NIL)
  (setq *ms04-search-bound* MS04-INITIAL-DEPTH)
  (setq *ms04-bound-reached* T)
  (when MS04-USE-SEMANTICS
    (setq *ms04-semantic-hash-table*
	  (make-hash-table :test #'equal))))

(defun ms04-controller (&rest ignore)
  (let ((wff (get dproof 'represents)))
    (ms04-initialize-search wff)
    (ms04-search wff)))

(defun ms04-search (wff)
  (create-ext-exp-open-dag wff)
  (ms04-search-1))

(defun ms04-search-1 ()
  (declare (special *edag-lift-info*))
  (let ((LAMBDA-CONV 'BETA-ETA-TOGETHER)
	(REWRITE-EQUALITIES 'only-ext)
	(REWRITE-DEFNS '(lazy1))
	(EXT-MATE-RECOMPUTE-JFORMS NIL))
    (declare (special LAMBDA-CONV REWRITE-EQUALITIES REWRITE-DEFNS EXT-MATE-RECOMPUTE-JFORMS))
    (let ((b (ms04-search-2)))
      (case b
	(SUCCESS
	 (msgf "Proof Found!")
	 (breakcount 'mating)
	 (runcount 'mating-ctr)
	 (breakcount 'mating-ctr)
	 (display-time 'mating)
	 (runcount 'mating)
	 (when *edag-lift-info*
	   (lift-eed-to-ms04-info *edag-lift-info*))
	 t)
	(t
	 (breakcount 'mating)
	 (runcount 'mating-ctr)
	 (breakcount 'mating-ctr)
	 (display-time 'mating)
	 (runcount 'mating)
	 nil)))))

(defun ms04-search-2 ()
    (declare (special *edag-lift-info* *ms04-search-bound* *ms04-search-done* *ms04-bound-reached*
		      *ms04-MAX-PRIMSUB-EXISTS* *ms04-MAX-PRIMSUB-FORALL* *ms04-MAX-PRIMSUB-NOT*
		      *ms04-MAX-PRIMSUB-OR* *ms04-MAX-PRIMSUB-AND* *ms04-MAX-PRIMSUB-NOT-EQUALS*
		      *ms04-MAX-PRIMSUB-EQUALS* *ms04-MAX-PRIMSUB-NOT-PROJ* *ms04-MAX-PRIMSUB-PROJ*
		      *ms04-MAX-PROJS* *ms04-MAX-IMITS* *ms04-MAX-FLEXRIGID-PROJ-MATES*
		      *ms04-MAX-FLEXRIGID-MATES* *ms04-MAX-EUNIF2S* *ms04-MAX-EUNIF1S*
		      *ms04-MAX-RIGID-MATES* *ms04-MAX-FLEX-EUNIFS* *ms04-MAX-FLEXRIGID-NEG-PROJ-MATES*
		      *ms04-MAX-FLEXRIGID-NEG-MATES* *ms04-MAX-DUPS* *ms04-MAX-DELAYED-CONNS*
		      ))
  (declare (special *edag-lift-info*))
  (startcount 'mating)
  (let ((curr (make-ms04-search :name "R" :edag *current-edag* :weight 0)))
    (setq *ms04-MAX-PRIMSUB-EXISTS* MS04-MAX-PRIMSUB-EXISTS)
    (setq *ms04-MAX-PRIMSUB-FORALL* MS04-MAX-PRIMSUB-FORALL)
    (setq *ms04-MAX-PRIMSUB-NOT* MS04-MAX-PRIMSUB-NOT)
    (setq *ms04-MAX-PRIMSUB-OR* MS04-MAX-PRIMSUB-OR)
    (setq *ms04-MAX-PRIMSUB-AND* MS04-MAX-PRIMSUB-AND)
    (setq *ms04-MAX-PRIMSUB-NOT-EQUALS* MS04-MAX-PRIMSUB-NOT-EQUALS)
    (setq *ms04-MAX-PRIMSUB-EQUALS* MS04-MAX-PRIMSUB-EQUALS)
    (setq *ms04-MAX-PRIMSUB-NOT-PROJ* MS04-MAX-PRIMSUB-NOT-PROJ)
    (setq *ms04-MAX-PRIMSUB-PROJ* MS04-MAX-PRIMSUB-PROJ)
    (setq *ms04-MAX-PROJS* MS04-MAX-PROJS)
    (setq *ms04-MAX-IMITS* MS04-MAX-IMITS)
    (setq *ms04-MAX-FLEXRIGID-PROJ-MATES* MS04-MAX-FLEXRIGID-PROJ-MATES)
    (setq *ms04-MAX-FLEXRIGID-MATES* MS04-MAX-FLEXRIGID-MATES)
    (setq *ms04-MAX-EUNIF2S* MS04-MAX-EUNIF2S)
    (setq *ms04-MAX-EUNIF1S* MS04-MAX-EUNIF1S)
    (setq *ms04-MAX-RIGID-MATES* MS04-MAX-RIGID-MATES)
    (setq *ms04-MAX-FLEX-EUNIFS* MS04-MAX-FLEX-EUNIFS)
    (setq *ms04-MAX-FLEXRIGID-NEG-PROJ-MATES* MS04-MAX-FLEXRIGID-NEG-PROJ-MATES)
    (setq *ms04-MAX-FLEXRIGID-NEG-MATES* MS04-MAX-FLEXRIGID-NEG-MATES)
    (setq *ms04-MAX-DUPS* MS04-MAX-DUPS)
    (setq *ms04-MAX-DELAYED-CONNS* MS04-MAX-DELAYED-CONNS)
    (set-ms04-banned-vars-0 curr)
    (when (and MS04-USE-SEMANTICS MS04-SEMANTIC-PRUNING)
      (push (cons 'REBUILD-SEMANTIC-SOLNS T) (ms04-search-aux-info curr)))
    (loop while *ms04-bound-reached* do
	  (setq *ms04-bound-reached* nil)
	  (msgf "Searching to Depth " *ms04-search-bound*)
	  (when *edag-lift-info* ; we're starting over, so reset all the lifting info
	    (ms04-initialize-edag-lift-info curr))
	  (ms04-search-3 curr)
	  (setq *ms04-search-bound* (+ *ms04-search-bound* MS04-INCR-DEPTH))
	  (if (or *ms04-search-done* (and (integerp MS04-MAX-DEPTH) (>= *ms04-search-bound* MS04-MAX-DEPTH)))
	      (setq *ms04-bound-reached* nil) ; give up
	    (progn ; ow, increment the max bounds
	      (incf *ms04-MAX-PRIMSUB-EXISTS*)
	      (incf *ms04-MAX-PRIMSUB-FORALL*)
	      (incf *ms04-MAX-PRIMSUB-NOT*)
	      (incf *ms04-MAX-PRIMSUB-OR*)
	      (incf *ms04-MAX-PRIMSUB-AND*)
	      (incf *ms04-MAX-PRIMSUB-NOT-EQUALS*)
	      (incf *ms04-MAX-PRIMSUB-EQUALS*)
	      (incf *ms04-MAX-PRIMSUB-NOT-PROJ*)
	      (incf *ms04-MAX-PROJS*)
	      (incf *ms04-MAX-IMITS*)
					;	    (incf *ms04-MAX-FLEXRIGID-PROJ-MATES*) ; not nec for completeness
	      (incf *ms04-MAX-FLEXRIGID-MATES*)
					;	    (incf *ms04-MAX-FLEXRIGID-NEG-PROJ-MATES*) ; not nec for completeness
	      (incf *ms04-MAX-FLEXRIGID-NEG-MATES*)
	      (incf *ms04-MAX-RIGID-MATES*)
	      (incf *ms04-MAX-EUNIF1S*)
	      (incf *ms04-MAX-EUNIF2S*)
					;	    (incf *ms04-MAX-FLEX-EUNIFS*) ; not nec for completeness
	      (incf *ms04-MAX-DUPS*)
					;	    (incf *ms04-MAX-DELAYED-CONNS*) ; not nec for completeness
	      )))
    (if *ms04-search-done*
	'SUCCESS
      'FAILED)))

#+(and allegro-version>= (version>= 5 0))
(defun ms04-mp-available () MS04-MP-OPTIONS)

#-(and allegro-version>= (version>= 5 0))
(defun ms04-mp-available () nil)

; curr - ms04-search structure
; search to *ms04-search-bound* 
(defun ms04-search-3 (curr)
  (declare (special *ms04-search-done*))
  (when (member ms04-verbose '(MED MAX))
    (msgf "Current State: "  curr)
    (when (eq ms04-verbose 'MAX)
      (ext-mate-show-mating-1 (ms04-search-edag curr))
      (extmate-show-exp-terms-1 (ms04-search-edag curr)))
    )
  (unless (ms04-search-done-p curr) ; if everything left has a flexflex, solve it and stop
    (let ((options (ms04-compute-options curr)))
      (when (ms04-mp-available)
	(let ((seqoptions nil)
	      (i 0)
	      (name (ms04-search-name curr))
	      (procs nil))
	  (dolist (op options)
	    (if (member (car op) MS04-MP-OPTIONS)
		(let ((new (ms04-search-do-option curr op)))
		  (when (ms04-search-ok new)
		    #+(and allegro-version>= (version>= 5 0))
		    (push (mp:process-run-function (format nil "~d.~d" name (incf i))
						   #'ms04-search-3
						   new)
			  procs)
		    #-(and allegro-version>= (version>= 5 0))
		    (throwfail "Multiprocessing Should Only Be Called in Allegro >= 5.0")
		    ))
	      (push op seqoptions)))
	  (setq options (reverse seqoptions))
	  #+(and allegro-version>= (version>= 5 0))
	  (progn
	    (mp:process-wait name
			     #'(lambda ()
				 (or *ms04-search-done*
				     (not (find-if #'(lambda (p)
						       (mp:process-active-p p))
						   procs)))))
	    (when *ms04-search-done*
	      (dolist (p procs)
		(mp:process-kill p))))
	  ))
      (loop while (and options (not *ms04-search-done*)) do
	    (let ((new (ms04-search-do-option curr (car options))))
	      (when (ms04-search-ok new)
		(ms04-search-3 new)))
	    (if (ms04-backtrack-option-p (car options))
		(pop options)
	      (setq options nil)))
      (unless (or *ms04-search-done* (neq ms04-verbose 'MAX))
	(msgf "Backtracking" t)))))

(defun ms04-backtrack-option-p (op)
  (cond ((equal MS04-BACKTRACK-METHOD 2)
	 (not (member (car op) '(MATE EUNIF1 EUNIF2))))
	((equal MS04-BACKTRACK-METHOD 3)
	 (not (member (car op) '(DUP MATE EUNIF1 EUNIF2))))
	(t T)))

; if every vp has a flex flex, we're done.
; more generally if every vp has a flex flex or a negative flex, we're done
; also, if every vp has a flex flex or a positive flex, we're done
; Even more generally, and I should change to this:
; If there is a partition X U Y of the setvars such that
; every vp has a flex-flex, a negative flex with head from X, or a positive flex with head from Y,
; then we're done.  (idea: send each x in X to TRUTH and y in Y to FALSEHOOD, everything else to a const)
; The complication is finding the partition if it exists.
(defun ms04-search-done-p (curr)
  (declare (special *edag-lift-info*))
  (let ((z (ms04-search-done-1-p curr)))
    (when z
      (when (member MS04-VERBOSE '(MED MAX))
	(msgf "Solution Found At Depth: " (ms04-search-weight curr)))
      (when *edag-lift-info*
	(setf (get *edag-lift-info* 'ms04-search-solution) curr))
      T)))

(defun ms04-search-done-1-p (curr)
  (declare (special *current-edag* *current-edag-lemma-ftree-pfs* *current-edag-lemmas* *ms04-search-done*))
  (let ((nonffvp1 (ms04-first-vp (list (ms04-search-edag curr))
				 (ms04-search-dupd-exps curr)
				 nil
				 :flexflex nil
				 :posandnegflex nil
				 :posflex nil
				 :negflex t)))
    (if nonffvp1  
	(let ((nonffvp2 (ms04-first-vp (list (ms04-search-edag curr))
				       (ms04-search-dupd-exps curr)
				       nil
				       :flexflex nil
				       :posandnegflex nil
				       :posflex t
				       :negflex nil)))
	  (if nonffvp2 ; not finished
	      nil 
	    (progn ; otherwise, every vp has a flex-flex or a negative flex node 
	      (unless *ms04-search-done*
		(setq *current-edag* 
		      (ms04-eeod-solve-flex-flex (ms04-search-edag curr) 'TRUTH))
		(setq *ms04-search-done* t))
	      t)))
      (progn ; otherwise, every vp has a flex-flex or a positive flex node 
	(unless *ms04-search-done*
	  (setq *current-edag* 
		(ms04-eeod-solve-flex-flex (ms04-search-edag curr) 'FALSEHOOD))
	  (setq *current-edag-lemmas* (ms04-search-lemmas curr))
	  (setq *current-edag-lemma-ftree-pfs* (ms04-search-lemma-ftree-pfs curr))
	  (setq *ms04-search-done* t))
	t))))

(defun ms04-compute-options (curr)
  (declare (special *edag-lift-info* *ms04-search-bound* *ms04-bound-reached*))
  (when (eq ms04-verbose 'MAX)
    (msgf "Computing Options for " curr)
    (display-vp-diag (eeod-to-jform (ms04-search-edag curr) :posflex t :negflex t :flexflex t)))
  (let ((options (ms04-compute-options-1 curr))
	(lifted-options nil)
	(lifted nil)
	(currw (ms04-search-weight curr)))
    (setq options ; go ahead and remove options with will exceed the current depth bound
	  (remove-if #'(lambda (op)
			 (if (> (+ currw (caadr op)) *ms04-search-bound*)
			     (progn
			       (setq *ms04-bound-reached* T)
			       T)
			   NIL))
		     options))
    (when *edag-lift-info*
      (push (cons curr (mapcar #'(lambda (op)
				   (cdadr op))
			       options))
	    (get *edag-lift-info* 'ms04-options-weights))
      (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	(if li
	    (let ((node-assoc (cadr li))
		  (theta (caddr li))
		  (psi (cadddr li)))
	      (setq lifted t)
	      (if query-user
		  (setq lifted-options
			(remove-if-not #'(lambda (op)
					   (ms04-valid-lift op node-assoc theta psi (eq MS04-VERBOSE 'MAX)))
				       options))
		(setq options
		      (remove-if-not #'(lambda (op)
					 (let ((vl (ms04-valid-lift op node-assoc theta psi (eq MS04-VERBOSE 'MAX))))
					   (if vl
					       T
					     (progn
					       (when (eq MS04-VERBOSE 'MAX)
						 (msgf "Pruning option ")
						 (print-ms04-option op))
					       NIL))))
				     options))))
	  (when (and (eq MS04-VERBOSE 'MAX) (get *edag-lift-info* 'eed))
	    (msgf "Lost Lifting Info For: " curr)))))
    (setq options (sort options #'(lambda (op1 op2) (< (caadr op1) (caadr op2)))))
    (when query-user
      (when (and options (or (cdr options) (and lifted (null lifted-options)))) ; choose
	(let ((l (length options))
	      (start 0)
	      (chosen nil)
	      (ch nil))
	  (when lifted-options
	    (msgf "Options marked *>* should lead to a solution."))
	  (loop until chosen do
		(let* ((d (- l start))
		       (m (min 10 d)))
		  (if (> start 0)
		      (msgf "Next " m " Possible Steps:" t)
		    (msgf "Top " m " Next Possible Steps:" t))
		  (dotimes (i m)
		    (let ((op (nth (+ start i) options)))
		      (if (member op lifted-options)
			  (msgf (1+ i) " *>* ")
			(msgf (1+ i) " > "))
		      (print-ms04-option op)
		      (when (eq MS04-VERBOSE 'MAX)
			(msgf "(" (caadr op) ") ")
			(print-ms04-weight-formula (cdadr op)))))
		  (if (> d 10)
		      (msgf "11 > More Options")
		    (msgf (1+ m) " > Backtrack"))
		  (msgf "Choose a number:")
		  (setq ch (get-a-number (1+ m) 1))
		  (if (= ch 11)
		      (if (> d 10)
			  (setq start (+ start 10))
			nil)
		    (progn
		      (setq chosen t ch (+ ch start))))))
	  (unless (= ch 1)
	    (let ((op (nth (- ch 1) options)))
	      (if op
		  (progn
		    (setq options (remove op options))
		    (push op options))
		(progn ; backtrack
		  (setq options NIL))))))))
    options))

(defun ms04-compute-options-1 (curr)
  (let ((val nil))
    (cond ((ms04-search-primsub-quant-evar curr) ; we're generating a type for a quantifier primsub (no vp)
	   (ms04-compute-primsub-quant-options (ms04-search-primsub-quant-evar curr)
					       curr))
	  ((and (or (ms04-search-unif-dpairs curr) (ms04-search-unif-subst curr)) ; doing some delayed unification 
		MS04-DELAY-UNIF-CONSTRAINTS)
	   (let ((dpairs (ms04-search-unif-dpairs curr))
		 (subst (ms04-search-unif-subst curr))
		 (evars (mapcar #'car (ms04-search-banned curr)))
		 (flexvp (ms04-first-vp (list (ms04-search-edag curr)) ; get *any* remaining vp
					(ms04-search-dupd-exps curr)
					nil
					:flexflex t
					:posandnegflex t
					:posflex t :negflex t
					:delayed (ms04-search-delayed curr))))
	     (if flexvp
		 (progn
		   (when (eq ms04-verbose 'MAX) (msgf "Vertical Path " (car flexvp)) t)
		   (ms04-compute-delayed-unif-options (car flexvp) curr))
					; Finally, find a soln to the unification problem - only return first soln
	       (multiple-value-bind (res thetas depth)
		   (ms04-unify dpairs subst (ms04-search-banned curr) MS04-SOLVE-UNIF-DEPTH '(SUCCESS))
		 (when (eq res 'SUCCESS)
		   (when *edag-lift-info*
		     (setf (get *edag-lift-info* 'ms04-solve-unif-depth)
			   (max (or (get *edag-lift-info* 'ms04-solve-unif-depth) 0)
				(or depth 0))))
		   (mapcar #'(lambda (theta)
			       (list 'SUBST '(0) 'U 
				     (remove-if-not #'(lambda (y)
							(member (car y) evars))
						    theta)))
			   thetas))))))
	  ((ms04-search-set-constrs curr) ; already forming set constraints - add another or solve
	   (let ((ev (car (ms04-search-set-constrs curr))))
	     (when (eq MS04-VERBOSE 'MAX)
	       (let ((maxmin (cadr (ms04-search-set-constrs curr))))
		 (msgf maxmin " Set Constraints for " (ev . gwff) t)
		 (dolist (sc (caddr (ms04-search-set-constrs curr)))
		   (if (eq maxmin 'MAX)
		       (progn
			 (msg (ext-exp-open-dag-name (car sc)) " -> ")
			 (if (cdr sc)
			     (progn
			       (msg (ext-exp-open-dag-name (cadr sc)))
			       (dolist (lit (cddr sc))
				 (msg ", " (ext-exp-open-dag-name lit))))
			   (msg ".")))
		     (progn
		       (if (cdr sc)
			   (progn
			     (msg (ext-exp-open-dag-name (cadr sc)))
			     (dolist (lit (cddr sc))
			       (msg ", " (ext-exp-open-dag-name lit))))
			 (msg "."))
		       (msg " -> " (ext-exp-open-dag-name (car sc)))))
		   (msg t))))
	     (ms04-compute-set-constr-options ev curr)))
					; try to find a rigid vp
	  ((setq val (ms04-first-vp (list (ms04-search-edag curr))
				    (ms04-search-dupd-exps curr)
				    nil
				    :flexflex nil
				    :posandnegflex nil
				    :posflex nil :negflex nil
				    :delayed (ms04-search-delayed curr)))
	   (when (eq ms04-verbose 'MAX)
	     (msgf "Rigid Vertical Path " (car val) t)
	     (dolist (n (car val))
	       (msgf (ext-exp-open-dag-name n) " : " ((ext-exp-open-dag-shallow n) . gwff) t)))
	   (ms04-compute-rigid-options (car val) curr))
					; otherwise every vp contains a flexible literal - might try set constraints
	  ((setq val 
		 (let ((bothpol (and MS04-USE-SET-CONSTRAINTS
				     (member 'MIN WHICH-CONSTRAINTS))))
		   (ms04-first-vp (list (ms04-search-edag curr))
				  (ms04-search-dupd-exps curr)
				  nil
				  :flexflex nil
				  :posandnegflex bothpol
				  :posflex t :negflex bothpol
				  :delayed (ms04-search-delayed curr))))
	   (when (eq ms04-verbose 'MAX)
	     (msgf "Vertical Path " (car val) t)
	     (dolist (n (car val))
	       (msgf (ext-exp-open-dag-name n) " : " ((ext-exp-open-dag-shallow n) . gwff) t)))
	   (ms04-compute-mixedflex-options (car val) curr))
	  (t NIL))))

					; mate, eunif, unif, dup an exp
(defun ms04-compute-rigid-options (vp curr)
  (let ((exps (ms04-exps-of-vp vp))
	(unif-nodes nil)
	(options nil))
    (setq options (ms04-compute-dup-options exps curr))
    (dolist (n1 vp)
      (when (and MS04-DELAY-UNIF-CONSTRAINTS
		 (not (ms04-search-set-constrs curr))
		 (eeod-flexrigid-goal-p n1))
	(push n1 unif-nodes)))
    (when unif-nodes
      (setq options
	    (mapcar #'(lambda (x)
			(let ((m (ms04-measure-delay-unif x curr)))
			  (list 'DELAY-UNIF m x exps)))
		    unif-nodes)))
    (let* ((aux-eqn-nodes
	    (remove-if-not #'(lambda (x)
			       (and (ext-exp-open-dag-positive x)
				    (eq (ext-exp-open-dag-kind x) 'EQN)))
			   vp))
	   (aux-eqns
	    (mapcar #'(lambda (x)
			(let* ((e (ext-exp-open-dag-shallow x))
			       (tp (cdr (unabbreviated-type (caar e)))))
			  (list tp (cdar e) (cdr e))))
		    aux-eqn-nodes)))
      (append options (ms04-compute-rigid-options-1 vp curr aux-eqns exps nil)))))

(defun ms04-check-max-bound (curr k)
  (declare (special *ms04-bound-reached*
		    *ms04-MAX-PRIMSUB-EXISTS* *ms04-MAX-PRIMSUB-FORALL* *ms04-MAX-PRIMSUB-NOT*
		    *ms04-MAX-PRIMSUB-OR* *ms04-MAX-PRIMSUB-AND* *ms04-MAX-PRIMSUB-NOT-EQUALS*
		    *ms04-MAX-PRIMSUB-EQUALS* *ms04-MAX-PRIMSUB-NOT-PROJ* *ms04-MAX-PRIMSUB-PROJ*
		    *ms04-MAX-PROJS* *ms04-MAX-IMITS* *ms04-MAX-FLEXRIGID-PROJ-MATES*
		    *ms04-MAX-FLEXRIGID-MATES* *ms04-MAX-EUNIF2S* *ms04-MAX-EUNIF1S*
		    *ms04-MAX-RIGID-MATES* *ms04-MAX-FLEX-EUNIFS* *ms04-MAX-FLEXRIGID-NEG-PROJ-MATES*
		    *ms04-MAX-FLEXRIGID-NEG-MATES* *ms04-MAX-DUPS* *ms04-MAX-DELAYED-CONNS*))
  (let ((b 
	 (case k
	   (DUPS
	    (< (ms04-search-num-dups curr) *ms04-MAX-DUPS*))
	   (DELAY-CONN
	    (< (ms04-search-num-delayed-conns curr) *ms04-MAX-DELAYED-CONNS*))
	   (RIGID-MATES
	    (< (ms04-search-num-rigid-mates curr) *ms04-MAX-RIGID-MATES*))
	   (EUNIF1
	    (< (ms04-search-num-eunif1s curr) *ms04-MAX-EUNIF1S*))
	   (EUNIF2
	    (< (ms04-search-num-eunif2s curr) *ms04-MAX-EUNIF2S*))
	   (FLEXRIGID-MATES
	    (< (ms04-search-num-flexrigid-mates curr) *ms04-MAX-FLEXRIGID-MATES*))
	   (FLEXRIGID-NEG-MATES
	    (< (ms04-search-num-flexrigid-neg-mates curr) *ms04-MAX-FLEXRIGID-NEG-MATES*))
	   (FLEXRIGID-PROJ-MATES
	    (< (ms04-search-num-flexrigid-proj-mates curr) *ms04-MAX-FLEXRIGID-PROJ-MATES*))
	   (FLEXRIGID-NEG-PROJ-MATES
	    (< (ms04-search-num-flexrigid-neg-proj-mates curr) *ms04-MAX-FLEXRIGID-NEG-PROJ-MATES*))
	   (FLEX-EUNIFS
	    (< (ms04-search-num-flex-eunifs curr) *ms04-MAX-FLEX-EUNIFS*))
	   (PRIMSUB-EXISTS
	    (< (ms04-search-num-exists curr) *ms04-MAX-PRIMSUB-EXISTS*))
	   (PRIMSUB-FORALL
	    (< (ms04-search-num-foralls curr) *ms04-MAX-PRIMSUB-FORALL*))
	   (PRIMSUB-NOT
	    (< (ms04-search-num-nots curr) *ms04-MAX-PRIMSUB-NOT*))
	   (PRIMSUB-OR
	    (< (ms04-search-num-ors curr) *ms04-MAX-PRIMSUB-OR*))
	   (PRIMSUB-AND
	    (< (ms04-search-num-ands curr) *ms04-MAX-PRIMSUB-AND*))
	   (PRIMSUB-EQUALS
	    (< (ms04-search-num-equals curr) *ms04-MAX-PRIMSUB-EQUALS*))
	   (PRIMSUB-NOT-EQUALS
	    (< (ms04-search-num-not-equals curr) *ms04-MAX-PRIMSUB-NOT-EQUALS*))
	   (PRIMSUB-PROJ
	    (< (ms04-search-num-projs curr) *ms04-MAX-PRIMSUB-PROJ*))
	   (PRIMSUB-NOT-PROJ
	    (< (ms04-search-num-not-projs curr) *ms04-MAX-PRIMSUB-NOT-PROJ*))
	   (UPROJ
	    (< (ms04-search-num-uprojs curr) *ms04-MAX-PROJS*))
	   (UIMIT
	    (< (ms04-search-num-uimits curr) *ms04-MAX-IMITS*))
	   (t t))))
    (if b
	t
      (progn
	(setq *ms04-bound-reached* t)
	nil))))

(defun ms04-compute-rigid-options-1 (vp curr aux-eqns exps setvars)
  (let ((options nil))
    (do ((nl vp (cdr nl)))
	((null nl))
      (let ((n1 (car nl)))
	(case (ext-exp-open-dag-kind n1)
	  (ATOM
	   (when (ms04-check-max-bound curr 'RIGID-MATES)
	     (let ((h1 (head (ext-exp-open-dag-shallow n1)))
		   (pos1 (ext-exp-open-dag-positive n1)))
	       (dolist (n2 (cdr nl))
		 (when (and (eq (ext-exp-open-dag-kind n2) 'ATOM)
			    (not (equal pos1 (ext-exp-open-dag-positive n2)))
			    (eq h1 (head (ext-exp-open-dag-shallow n2)))
			    (not (eeod-mated-p n1 n2)))
		   (let ((posatom (if pos1 n1 n2))
			 (negatom (if pos1 n2 n1)))
		     (let* ((dpairs (mapcar #'(lambda (x y)
						(list (unabbreviated-type x) x y nil))
					    (args (ext-exp-open-dag-shallow posatom))
					    (args (ext-exp-open-dag-shallow negatom))))
			    (v (ms04-quick-eunification-weight dpairs aux-eqns
							       (ms04-search-banned curr)))
			    (mm (+ (length (ext-exp-open-dag-arcs posatom))
				   (length (ext-exp-open-dag-arcs negatom)))))
		       (push (cons mm 'MS04-WEIGHT-MULTIPLE-MATES) v)
		       (push 'MS03-WEIGHT-RIGID-MATE v)
		       (push (list 'MATE (cons (evaluate-ext-weight-1 v) v)
				   posatom negatom exps setvars)
			     options))))))))
	  (EQNGOAL
	   (let* ((sh (ext-exp-open-dag-shallow n1))
		  (lft (cdar sh))
		  (rght (cdr sh))
		  (lh (head lft))
		  (rh (head rght))
		  (utp (type lft)))
	     (dolist (n2 (cdr nl))
	       (when (and (ext-exp-open-dag-positive n2)
			  (eq (ext-exp-open-dag-kind n2) 'EQN)
			  (equal utp (type (cdar (ext-exp-open-dag-shallow n2)))))
		 (unless (or (eeod-eunif1-p n2 n1)
			     (not (ms04-check-max-bound curr 'EUNIF1)))
		   (push (list 'EUNIF1 (ms04-measure-eunif1 n2 n1 aux-eqns (ms04-search-banned curr))
			       n2 n1 exps setvars) options))
		 (unless (or (eeod-eunif2-p n2 n1)
			     (not (ms04-check-max-bound curr 'EUNIF2)))
		   (push (list 'EUNIF2 (ms04-measure-eunif2 n2 n1 aux-eqns (ms04-search-banned curr))
			       n2 n1 exps setvars) options))))
	     (when (ext-exp-var-p lh)
	       (setq options
		     (append (ms04-compute-unif-options n1 lh lft rh rght aux-eqns curr exps setvars)
			     options)))
	     (when (ext-exp-var-p rh)
	       (setq options
		     (append (ms04-compute-unif-options n1 rh rght lh lft aux-eqns curr exps setvars)
			     options)))))
	  (EQN
	   (when (ext-exp-open-dag-positive n1)
	     (let* ((sh (ext-exp-open-dag-shallow n1))
		    (lft (cdar sh))
		    (utp (type lft)))
	       (dolist (n2 (cdr nl))
		 (when (and (not (ext-exp-open-dag-positive n2))
			    (eq (ext-exp-open-dag-kind n2) 'EQNGOAL)
			    (equal utp (type (cdar (ext-exp-open-dag-shallow n2)))))
		   (unless (or (eeod-eunif1-p n1 n2)
			       (not (ms04-check-max-bound curr 'EUNIF1)))
		     (push (list 'EUNIF1 (ms04-measure-eunif1 n1 n2 aux-eqns (ms04-search-banned curr))
				 n1 n2 exps setvars) options))
		   (unless (or (eeod-eunif2-p n1 n2)
			       (not (ms04-check-max-bound curr 'EUNIF2)))
		     (push (list 'EUNIF2 (ms04-measure-eunif2 n1 n2 aux-eqns (ms04-search-banned curr))
				 n1 n2 exps setvars) options)))))))
	  (t nil))))
    options))

(defun ms04-compute-unif-options (n x ftrm h rtrm aux-eqns curr exps setvars)
  (declare (ignore n))
  (if (ms04-search-delayed curr) ; if there are delayed nodes along this branch of search, don't do primitive unif steps
      nil
    (let* ((tp (unabbreviated-type x))
	   (projs (ext-possible-projections tp))
	   (options nil))
      (unless (ms04-banned-sel-var-p x h curr)
	(when (ms04-check-max-bound curr 'UIMIT)
	  (multiple-value-bind (imit new-evars)
	      (create-imit-subst x h)
	    (declare (ignore new-evars))
	    (let ((meas
		   (ms04-measure-imit (lnorm (substitute-l-term-var imit x ftrm))
				      (lnorm (substitute-l-term-var imit x rtrm))
				      aux-eqns (ms04-search-banned curr))))
	      (push (list 'SUBST meas 'I (acons x imit nil) exps setvars)
		    options)))))
      (when (ms04-check-max-bound curr 'UPROJ)
	(dolist (i projs)
	  (multiple-value-bind (proj new-evars)
	      (create-proj-subst x i)
	    (declare (ignore new-evars))
	    (let ((meas
		   (ms04-measure-proj (lnorm (substitute-l-term-var proj x ftrm))
				      (lnorm (substitute-l-term-var proj x rtrm))
				      aux-eqns (ms04-search-banned curr))))
	      (push (list 'SUBST meas 'P (acons x proj nil) exps setvars)
		    options)))))
      options)))

(defun ms04-compute-mixedflex-options (vp curr)
  (let ((exps (ms04-exps-of-vp vp))
	(setvars nil)
	(setcon-ops nil)
	(options nil))
    (setq options (ms04-compute-dup-options exps curr))
    (dolist (n1 vp)
      (when (eq (ext-exp-open-dag-kind n1) 'FLEX)
	(let ((setvar (head (ext-exp-open-dag-shallow n1))))
	  (when (and MS04-USE-SET-CONSTRAINTS (not (ms04-search-delayed curr))) ; not already unifying or doing set constrs
	    (if (ext-exp-open-dag-positive n1)
		(when (member 'MAX WHICH-CONSTRAINTS)
		  (dolist (constr (ms04-compute-one-set-constr (remove n1 vp) n1 setvar curr))
		    (push (list 'ADD-SET-CONSTRAINT (list MS04-WEIGHT-ADD-SET-CONSTRAINT 'MS04-WEIGHT-ADD-SET-CONSTRAINT)
				'MAX setvar constr)
			  setcon-ops)))
	      (when (member 'MIN WHICH-CONSTRAINTS)
		(dolist (constr (ms04-compute-one-set-constr (remove n1 vp) n1 setvar curr))
		  (push (list 'ADD-SET-CONSTRAINT (list MS04-WEIGHT-ADD-SET-CONSTRAINT 'MS04-WEIGHT-ADD-SET-CONSTRAINT)
			      'MIN setvar constr)
			setcon-ops)))))
	  (unless (member setvar (ms04-search-almost-atomic-evars curr)) ; unless already went through primsub phase
	    (push setvar setvars))))) ; go into primsub phase with these setvars
    (dolist (setvar setvars)
      (setq options
	    (append (ms04-compute-primsub-options setvar curr exps) ; go into any primsub phase for a setvar we haven't handled yet
		    options)))
					; otherwise assume we're not doing primsubs on the flex's and generate other options
    (let* ((aux-eqn-nodes
	    (remove-if-not #'(lambda (x)
			       (and (ext-exp-open-dag-positive x)
				    (eq (ext-exp-open-dag-kind x) 'EQN)))
			   vp))
	   (aux-eqns
	    (mapcar #'(lambda (x)
			(let* ((e (ext-exp-open-dag-shallow x))
			       (tp (cdr (unabbreviated-type (caar e)))))
			  (list tp (cdar e) (cdr e))))
		    aux-eqn-nodes)))
      (append setcon-ops options
	      (ms04-compute-rigid-options-1 vp curr aux-eqns exps setvars)
	      (ms04-compute-mixedflex-options-1 vp curr aux-eqns exps setvars)))))

(defun ms04-compute-mixedflex-options-1 (vp curr aux-eqns exps setvars)
  (let ((options nil))
    (dolist (n1 vp)
      (when (eq (ext-exp-open-dag-kind n1) 'FLEX)
	(let* ((sh (ext-exp-open-dag-shallow n1))
	       (setvar (head sh)))
	  (dolist (n2 vp)
	    (when (and MS04-ALLOW-FLEX-EUNIFS (not (ms04-search-delayed curr))
		       (ms04-check-max-bound curr 'FLEX-EUNIFS)
		       (or (eq (ext-exp-open-dag-kind n2) 'EQNGOAL)
			   (and (eq (ext-exp-open-dag-kind n2) 'EQN)
				(ext-exp-open-dag-positive n2))))
	      (let* ((sh2 (ext-exp-open-dag-shallow n2))
		     (h (head sh2)))
					; imitate equality or negation of equality, then try to eunif1 the two nodes
		(multiple-value-bind (imit new-evars)
		    (create-imit-subst setvar h)
		  (declare (ignore new-evars))
		  (if (equal (ext-exp-open-dag-positive n1) (ext-exp-open-dag-positive n2))
		      (multiple-value-bind (nwff new-evars2)
			  (create-imit-subst setvar 'NOT)
			(multiple-value-bind (nimit new-evars3)
			    (create-imit-subst (car new-evars2) h)
			  (declare (ignore new-evars3))
			  (let ((wff4 (lnorm (substitute-l-term-var nimit (car new-evars2) nwff))))
			    (let ((meas
				   (ms04-measure-eunif1-flexrigid n1 n2 (acons setvar imit nil)
								  aux-eqns (ms04-search-banned curr))))
			      (push (list 'FLEX-EUNIF meas
					  n1 n2
					  (acons setvar wff4 nil) exps setvars)
				    options)))))
		    (let ((meas
			   (ms04-measure-eunif1-flexrigid n1 n2 (acons setvar imit nil)
							  aux-eqns (ms04-search-banned curr))))
		      (push (list 'FLEX-EUNIF meas
				  n1 n2 
				  (acons setvar imit nil) exps setvars)
			    options))))))
	    (when (eq (ext-exp-open-dag-kind n2) 'ATOM)
	      (unless (ms04-search-delayed curr) ; if there are delayed nodes along this branch of search, don't do mates w/flex rigid (do them on another branch)
		(let* ((sh2 (ext-exp-open-dag-shallow n2))
		       (h (head sh2)))
		  (unless (ms04-banned-sel-var-p setvar h curr)
		    (multiple-value-bind (imit new-evars)
			(create-imit-subst setvar h)
		      (declare (ignore new-evars))
		      (if (equal (ext-exp-open-dag-positive n1) (ext-exp-open-dag-positive n2))
			  (when (ms04-check-max-bound curr 'FLEXRIGID-NEG-MATES)
			    (multiple-value-bind (nwff new-evars2)
				(create-imit-subst setvar 'NOT)
			      (multiple-value-bind (nimit new-evars3)
				  (create-imit-subst (car new-evars2) h)
				(declare (ignore new-evars3))
				(let ((wff4 (lnorm (substitute-l-term-var nimit (car new-evars2) nwff))))
				  (let ((meas
					 (ms04-measure-mate-flexrigid n1 n2 (acons setvar imit nil)
								      aux-eqns (ms04-search-banned curr))))
				    (push (list 'MATE-FLEXRIGID-NEG meas
						n1 n2 'I
						(acons setvar wff4 nil) exps setvars)
					  options))))))
			(when (ms04-check-max-bound curr 'FLEXRIGID-MATES)
			  (let ((meas
				 (ms04-measure-mate-flexrigid n1 n2 (acons setvar imit nil)
							      aux-eqns (ms04-search-banned curr))))
			    (push (list 'MATE-FLEXRIGID meas
					n1 n2 'I
					(acons setvar imit nil) exps setvars)
				  options))))))))
	      (when MS04-ALLOW-FLEXRIGID-PROJ-MATE
		(dolist (i (ext-possible-projections (unabbreviated-type setvar)))
		  (if (equal (ext-exp-open-dag-positive n1) (ext-exp-open-dag-positive n2))
		      (when (ms04-check-max-bound curr 'FLEXRIGID-NEG-PROJ-MATES)
			(multiple-value-bind (nwff new-evars2)
			    (create-imit-subst setvar 'NOT)
			  (multiple-value-bind (nproj new-evars3)
			      (create-proj-subst (car new-evars2) i)
			    (declare (ignore new-evars3))
			    (let ((wff4 (lnorm (substitute-l-term-var nproj (car new-evars2) nwff))))
			      (let ((meas
				     (ms04-measure-mate-flexrigid-proj n1 n2 (acons setvar wff4 nil)
								       aux-eqns (ms04-search-banned curr))))
				(push (list 'MATE-FLEXRIGID-NEG-PROJ meas n1 n2
					    (acons setvar wff4 nil) exps setvars)
				      options))))))
		    (when (ms04-check-max-bound curr 'FLEXRIGID-PROJ-MATES)
		      (let* ((proj (create-proj-subst setvar i))
			     (meas
			      (ms04-measure-mate-flexrigid-proj n1 n2 (acons setvar proj nil)
								aux-eqns (ms04-search-banned curr))))
			(push (list 'MATE-FLEXRIGID-PROJ meas n1 n2
				    (acons setvar proj nil) exps setvars)
			      options))))))
	      (when (and MS04-DELAY-UNIF-CONSTRAINTS MS04-DELAY-FLEXRIGID-MATES)
		(push (list 'DELAY-CONN
			    (ms04-measure-delay-flexconn n1 n2 curr)
			    n1 n2 exps setvars)
		      options)))
	    ))))
    options))

; vp may contain flexflex goal nodes
(defun ms04-compute-delayed-unif-options (vp curr)
  (let ((exps (ms04-exps-of-vp vp))
	(options nil))
    (setq options (ms04-compute-dup-options exps curr))
    (when (ms04-check-max-bound curr 'DELAY-CONN)
      (dolist (n1 vp)
	(cond ((eq (ext-exp-open-dag-kind n1) 'FLEX)
	       (dolist (n2 vp)
		 (cond ((eq (ext-exp-open-dag-kind n2) 'ATOM)
			(push (list 'DELAY-CONN
				    (ms04-measure-delay-flexconn n1 n2 curr)
				    n1 n2 exps nil)
			      options))
		       ((and MS04-ALLOW-FLEX-EUNIFS
			     (or (eq (ext-exp-open-dag-kind n2) 'EQNGOAL)
				 (and (eq (ext-exp-open-dag-kind n2) 'EQN)
				      (ext-exp-open-dag-positive n2))))
			(push (list 'DELAY-CONN
				    (ms04-measure-delay-flexconn n1 n2 curr)
				    n1 n2 exps nil)
			      options))
		       (t nil))))
	      ((and (eq (ext-exp-open-dag-kind n1) 'ATOM) (ext-exp-open-dag-positive n1))
	       (let* ((sh1 (ext-exp-open-dag-shallow n1))
		      (h1 (head sh1)))
		 (dolist (n2 vp)
		   (when (and (eq (ext-exp-open-dag-kind n2) 'ATOM) (not (ext-exp-open-dag-positive n2)))
		     (let* ((sh2 (ext-exp-open-dag-shallow n2))
			    (h2 (head sh2)))
		       (when (and (eq h1 h2) (not (eeod-mated-p n1 n2)))
			 (push (list 'DELAY-CONN
				     (ms04-measure-delay-rigid-mate n1 n2 curr)
				     n1 n2 exps nil)
			       options)))))))
	      ((and (eq (ext-exp-open-dag-kind n1) 'EQN) (ext-exp-open-dag-positive n1))
	       (let* ((sh (ext-exp-open-dag-shallow n1))
		      (lft (cdar sh))
		      (utp (type lft)))
		 (dolist (n2 vp)
		   (when (and (not (ext-exp-open-dag-positive n2))
			      (eq (ext-exp-open-dag-kind n2) 'EQNGOAL)
			      (equal utp (type (cdar (ext-exp-open-dag-shallow n2))))
			      (not (eeod-eunif1-p n1 n2)))
		     (push (list 'DELAY-CONN
				 (ms04-measure-delay-eunif1 n1 n2 curr)
				 n1 n2 exps nil)
			   options)))))
	      ((eq (ext-exp-open-dag-kind n1) 'EQNGOAL)
	       (push (list 'DELAY-UNIF (ms04-measure-delay-unif n1 curr) n1 exps) options))
	      (t nil))))
    options))

					; duplication phase
(defun ms04-compute-dup-options (exps curr)
  (let ((options nil))
    (when (ms04-check-max-bound curr 'DUPS)
      (dolist (exp exps)
	(push (list 'DUP (ms04-measure-dup exp) exp) options))) ; do another dup
    options))

					; primsub phase
(defun ms04-compute-primsub-options (setvar curr exps)
  (let ((options nil))
    (when (not MS04-PRENEX-PRIMSUBS)
      (dolist (c *extra-log-consts*)
	(let ((v (list 'MS04-WEIGHT-PRIMSUB-OCCURS-CONST)))
	  (if (atom c)
	      (multiple-value-bind (wff evars)
		  (create-imit-subst setvar c)
		(push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
			    setvar (acons setvar wff nil) evars c exps)
		      options))
					; otherwise quantifier
	    (let ((b (car c))
		  (tp (cdr c)))
	      (multiple-value-bind (wff evars)
		  (create-quant-subst setvar tp b)
		(push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
			    setvar (acons setvar wff nil)
			    evars b tp exps)
		      options)))))))
    (when (and *individual-types*
	       (not (member setvar (ms04-search-cnf-evars curr)))
	       (not (member setvar (ms04-search-clause-evars curr)))
	       (not (member setvar (ms04-search-lit-evars curr))))
      (let ((v1 (list 'MS03-WEIGHT-PRIMSUB-FIRST-FORALL
		      (cons (ms04-search-num-foralls curr) 'MS03-WEIGHT-PRIMSUB-NEXT-FORALL)))
	    (v2 (list 'MS03-WEIGHT-PRIMSUB-FIRST-EXISTS
		      (cons (ms04-search-num-exists curr) 'MS03-WEIGHT-PRIMSUB-NEXT-EXISTS))))
	(when (ms04-check-max-bound curr 'PRIMSUB-FORALL)
	  (push (list 'PRIMSUB-QUANT-GENTP (cons (evaluate-ext-weight-1 v1) v1)
		      (list 'FORALL setvar 0) exps)
		options))
	(when (and MS04-PRENEX-PRIMSUBS (ms04-check-max-bound curr 'PRIMSUB-EXISTS))
	  (push (list 'PRIMSUB-QUANT-GENTP (cons (evaluate-ext-weight-1 v2) v2)
		      (list 'EXISTS setvar 0) exps)
		options))))
    (when (and MS04-PRENEX-PRIMSUBS
	       (not (member setvar (ms04-search-clause-evars curr)))
	       (not (member setvar (ms04-search-lit-evars curr))))
      (when (ms04-check-max-bound curr 'PRIMSUB-AND)
	(multiple-value-bind (wff evars)
	    (create-imit-subst setvar 'AND)
	  (let ((v (list 'MS03-WEIGHT-PRIMSUB-FIRST-AND
			 (cons (ms04-search-num-ands curr) 'MS03-WEIGHT-PRIMSUB-NEXT-AND))))
	    (push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
			setvar (acons setvar wff nil) evars 'AND nil exps)
		  options)))))
    (when (not (member setvar (ms04-search-lit-evars curr)))
      (when (ms04-check-max-bound curr 'PRIMSUB-OR)
	(multiple-value-bind (wff evars)
	    (create-imit-subst setvar 'OR)
	  (let ((v (list 'MS03-WEIGHT-PRIMSUB-FIRST-OR
			 (cons (ms04-search-num-ors curr) 'MS03-WEIGHT-PRIMSUB-NEXT-OR))))
	    (push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
			setvar (acons setvar wff nil) evars 'OR nil exps)
		  options)))))
    (when (not MS04-PRENEX-PRIMSUBS)
      (when (ms04-check-max-bound curr 'PRIMSUB-NOT)
	(multiple-value-bind (wff evars)
	    (create-imit-subst setvar 'NOT)
	  (let ((v (list 'MS04-WEIGHT-PRIMSUB-FIRST-NOT
			 (cons (ms04-search-num-nots curr) 'MS04-WEIGHT-PRIMSUB-NEXT-NOT))))
	    (push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
			setvar (acons setvar wff nil) evars 'NOT nil exps)
		  options)))))
    (dolist (tp *individual-types*)
      (let ((q (inherit-abbrev '= (acons 'O tp tp) (list tp))))
	(when (ms04-check-max-bound curr 'PRIMSUB-EQUALS)
	  (multiple-value-bind (wff evars)
	      (create-imit-subst setvar q)
	    (declare (ignore evars))
	    (let ((v (list 'MS03-WEIGHT-PRIMSUB-FIRST-EQUALS
			   (cons (ms04-search-num-equals curr) 'MS03-WEIGHT-PRIMSUB-NEXT-EQUALS))))
	      (push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
			  setvar
			  (acons setvar wff nil)
			  nil 'EQUALS nil exps)
		    options))))
	(when MS04-PRENEX-PRIMSUBS
	  (when (ms04-check-max-bound curr 'PRIMSUB-NOT-EQUALS)
	    (multiple-value-bind (nwff evars)
		(create-imit-subst setvar 'NOT)
	      (let ((v (list 'MS03-WEIGHT-PRIMSUB-FIRST-NOT-EQUALS
			     (cons (ms04-search-num-equals curr) 'MS03-WEIGHT-PRIMSUB-NEXT-NOT-EQUALS))))
		(let ((ev3 (car evars)))
		  (multiple-value-bind (wff3 evars4)
		      (create-imit-subst ev3 q)
		    (declare (ignore evars4))
		    (push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
				setvar
				(acons setvar (lnorm (substitute-l-term-var wff3 ev3 nwff)) nil)
				nil 'NOT-EQUALS nil exps)
			  options)))))))))
    (dolist (i (ext-possible-projections (unabbreviated-type setvar)))
      (multiple-value-bind (wff evars)
	  (create-proj-subst setvar i)
	(declare (ignore evars))
	(when (ms04-check-max-bound curr 'PRIMSUB-PROJ)
	  (let ((v (list 'MS03-WEIGHT-PRIMSUB-FIRST-PROJ
			 (cons (ms04-search-num-projs curr) 'MS03-WEIGHT-PRIMSUB-NEXT-PROJ))))
	    (push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
			setvar (acons setvar wff nil) nil 'PROJ nil exps)
		  options)))
	(when (and MS04-PRENEX-PRIMSUBS
		   (ms04-check-max-bound curr 'PRIMSUB-NOT-PROJ))
	  (multiple-value-bind (nwff evars)
	      (create-imit-subst setvar 'NOT)
	    (let ((v (list 'MS03-WEIGHT-PRIMSUB-FIRST-NOT-PROJ
			   (cons (ms04-search-num-not-projs curr) 'MS03-WEIGHT-PRIMSUB-NEXT-NOT-PROJ))))
	      (let ((ev3 (car evars)))
		(multiple-value-bind (wff3 evars4)
		    (create-proj-subst ev3 i)
		  (declare (ignore evars4))
		  (push (list 'PRIMSUB (cons (evaluate-ext-weight-1 v) v)
			      setvar
			      (acons setvar (lnorm (substitute-l-term-var wff3 ev3 nwff)) nil)
			      nil 'NOT-PROJ nil exps)
			options))))))))
    options))

; primsub quant type generation phase
(defun ms04-compute-primsub-quant-options (qinfo curr)
  (declare (special *individual-types*) (ignore curr))
  (let* ((b (car qinfo))
	 (setvar (cadr qinfo))
	 (tpn (caddr qinfo))
	 (tp (ms04-nth-primsub-type tpn (length *individual-types*) *individual-types*)))
    (multiple-value-bind (wff evars)
	(create-quant-subst setvar tp b)
      (list (list 'PRIMSUB-QUANT-GENTP
		  (list MS04-WEIGHT-PRIMSUB-NEXTTP 'MS04-WEIGHT-PRIMSUB-NEXTTP)
		  (list b setvar (1+ tpn)))
	    (list 'PRIMSUB ; go out of generation phase and do the primsub at the current type
		  '(0) ; already weighted it when we started generating the type
		  setvar
		  (acons setvar wff nil)
		  evars
		  b tp nil)))))

(defun ms04-compute-set-constr-options (ev curr)
  (let* ((options nil)
	 (maxmin (cadr (ms04-search-set-constrs curr)))
	 (pos (eq maxmin 'MAX)))
    (when (< (length (caddr (ms04-search-set-constrs curr))) MAX-NUM-CONSTRAINTS) ; then we can add another
      (let ((tempvp (ms04-first-vp (list (ms04-search-edag curr))
				   (ms04-search-dupd-exps curr)
				   nil
				   :flexflex t
				   :posandnegflex t
				   :posflex t :negflex t
				   :delayed (ms04-search-delayed curr)))
	    (delayed (ms04-search-delayed curr))
	    (flexvp nil))
	(loop until (or (not tempvp) flexvp) do
	      (if (find-if #'(lambda (x)
			       (and (eq (ext-exp-open-dag-kind x) 'FLEX)
				    (equal (ext-exp-open-dag-positive x) pos)
				    (eq (head (ext-exp-open-dag-shallow x)) ev)))
			   (car tempvp))
		    (setq flexvp tempvp)
		(progn
		  (push (cons 'DELAY (car tempvp)) delayed)
		  (setq tempvp (ms04-first-vp (list (ms04-search-edag curr))
					      (ms04-search-dupd-exps curr)
					      nil
					      :flexflex t
					      :posandnegflex t
					      :posflex t :negflex t
					      :delayed delayed)))))
	(when flexvp
	  (dolist (node (car flexvp))
	    (when (and (eq (ext-exp-open-dag-kind node) 'FLEX)
		       (eq (head (ext-exp-open-dag-shallow node)) ev)
		       (or (and (eq maxmin 'MAX)
				(ext-exp-open-dag-positive node))
			   (and (eq maxmin 'MIN)
				(not (ext-exp-open-dag-positive node)))))
	      (dolist (constr
		       (ms04-compute-one-set-constr (remove node (car flexvp)) node ev curr))
		(push (list 'ADD-SET-CONSTRAINT (list MS04-WEIGHT-ADD-SET-CONSTRAINT 'MS04-WEIGHT-ADD-SET-CONSTRAINT)
			    maxmin ev constr)
		      options)))))))
    (push (list 'SOLVE-SET-CONSTRAINTS (list MS04-WEIGHT-SOLVE-SET-CONSTRAINTS 'MS04-WEIGHT-SOLVE-SET-CONSTRAINTS) ev)
	  options)
    options))

(defun ms04-compute-one-set-constr (vp node ev curr)
  (let* ((banned (cdr (assoc ev (ms04-search-banned curr))))
	 (sh (ext-exp-open-dag-shallow node))
	 (bannedoccurs (intersection (free-vars-of sh) banned)))
    (remove-if #'(lambda (x)
		   (or (and BAD-VAR-CONNECTED-PRUNE
			    (not (lits-sel-conn-p x bannedoccurs banned)))
		       (and (not (cdr x))
			    (eeod-pattern-p-4 sh bannedoccurs))))
	       (mapcar #'(lambda (x) (cons node x))
		       (ms04-compute-one-set-constr-1 vp node ev curr)))))

(defun ms04-compute-one-set-constr-1 (vp node ev curr)
  (if vp
      (let ((node2 (car vp))
	    (constrs (ms04-compute-one-set-constr-1 (cdr vp) node ev curr)))
	(if (or (not (eq (ext-exp-open-dag-kind node2) 'FLEX))
		(not (eq (head (ext-exp-open-dag-shallow node2)) ev))
		(not (equal (ext-exp-open-dag-positive node) (ext-exp-open-dag-positive node2))))
	    (append (mapcar #'(lambda (x)
				(cons (car vp) x))
			    (remove-if-not #'(lambda (y)
					       (< (1+ (length y)) MAX-CONSTRAINT-SIZE))
					   constrs))
		    constrs)
	  constrs))
    (list nil)))

(defun ms04-exps-of-vp (vp)
  (remove-if-not #'(lambda (x) (eq (ext-exp-open-dag-kind x) 'EXP)) vp))

(defun ms04-setvars-of-vp (vp curr)
  (let ((setvars nil))
    (dolist (n1 vp)
      (when (eq (ext-exp-open-dag-kind n1) 'FLEX)
	(let ((p (head (ext-exp-open-dag-shallow n1))))
	  (unless (or (member p setvars) (member p (ms04-search-almost-atomic-evars curr)))
	    (push p setvars)))))
    setvars))

(defun ms04-search-do-option (curr op)
  (declare (special *edag-lift-info*))
  (let ((new (ms04-search-do-option-0 curr op)))
    (when *edag-lift-info*
      (push (list new curr (cdadr op)) (get *edag-lift-info* 'ms04-trace))
      (when (eq MS04-VERBOSE 'MAX)
	(let ((li (assoc new (get *edag-lift-info* 'ms04-search-lifts))))
	  (if li
	      (progn
		(msgf "Lifting Info for " new)
		(let ((lnode-assoc (cadr li))
		      (ltheta (caddr li))
		      (lpsi (cadddr li)))
		  (msgf "Node Association" t)
		  (dolist (a lnode-assoc) (msgf (car a) " -> " (cdr a) t))
		  (msgf "psi ")
		  (dolist (xt lpsi) (msg "[" ((car xt) . gwff) " -> " ((cdr xt) . gwff) "] "))
		  (msgf "theta ")
		  (dolist (g ltheta)
		    (msgf ((car g) . gwff) " |-> " ((cdr g) . gwff)))))
	    (msgf "No Lifting Info For " new))))
      )
    new))

(defun ms04-search-do-option-0 (curr op)
  (declare (special *edag-lift-info*))
  (when (member ms04-verbose '(MED MAX))
    (msgf "Option: ")
    (print-ms04-option op))
  (case (car op)
    (MATE
     (let ((posatom (nth 2 op))
	   (negatom (nth 3 op))
	   (exps (nth 4 op))
	   (setvars (nth 5 op))
	   (node-assoc nil))
       (declare (special node-assoc))
       (let* ((edag2 (copy-eeod-rec-1 (ms04-search-edag curr))) ; overkill, but safe
	      (posatom1 (cdr (assoc posatom node-assoc)))
	      (negatom1 (cdr (assoc negatom node-assoc)))
	      (matenode1 (eeod-mate posatom1 negatom1))
	      (new (copy-ms04-search-1 curr "M")))
	 (when *edag-lift-info*
	   (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	     (when li
	       (let* ((lnode-assoc (cadr li))
		      (lnode-assoc1 (factor-node-assoc node-assoc lnode-assoc))
		      (ltheta (caddr li))
		      (lpsi (cadddr li))
		      (lposatom (cdr (assoc posatom lnode-assoc)))
		      (lnegatom (cdr (assoc negatom lnode-assoc)))
		      (lsuccess nil))
		 (when (and lposatom lnegatom)
		   (let ((e (car (eed-mated-p lposatom lnegatom))))
		     (when e
		       (multiple-value-bind
			   (lnode-assoc2 ltheta2 lpsi2)
			   (ms04-lift-eed-to-eeod-close
			    (acons matenode1 e lnode-assoc1) ltheta lpsi)
			 (setq lsuccess t)
			 (push (list new lnode-assoc2 ltheta2 lpsi2)
			       (get *edag-lift-info* 'ms04-search-lifts))))))
		 (unless lsuccess
		   (msgf "Problem Lifting Mate"))))))
	 (ms04-stop-duping exps new)
	 (ms04-stop-primsubing setvars new)
	 (incf (ms04-search-num-rigid-mates new))
	 (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
	 (setf (ms04-search-edag new) edag2)
	 (setf (ms04-search-banned new) 'COMPUTE)
	 (ms04-update-delayed new node-assoc)
	 new)))
    ((EUNIF1 EUNIF2)
     (let ((node-assoc nil))
       (declare (special node-assoc))
       (let* ((edag2 (copy-eeod-rec-1 (ms04-search-edag curr))) ; overkill, but safe
	      (eqn (nth 2 op))
	      (eqngoal (nth 3 op))
	      (exps (nth 4 op))
	      (setvars (nth 5 op))
	      (eqn1 (cdr (assoc eqn node-assoc)))
	      (eqngoal1 (cdr (assoc eqngoal node-assoc)))
	      (eunifnode1 (if (eq (car op) 'EUNIF1) (eeod-eunif1 eqn1 eqngoal1) (eeod-eunif2 eqn1 eqngoal1)))
	      (new (copy-ms04-search-1 curr (if (eq (car op) 'EUNIF1) "E" "Es"))))
	 (when *edag-lift-info*
	   (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	     (when li
	       (let* ((lnode-assoc (cadr li))
		      (lnode-assoc1 (factor-node-assoc node-assoc lnode-assoc))
		      (ltheta (caddr li))
		      (lpsi (cadddr li))
		      (leqn (cdr (assoc eqn lnode-assoc)))
		      (leqngoal (cdr (assoc eqngoal lnode-assoc)))
		      (lsuccess nil))
		 (when (and leqn leqngoal)
		   (let ((e (car (eed-eunifk-p leqn leqngoal (car op)))))
		     (when e
		       (multiple-value-bind
			   (lnode-assoc2 ltheta2 lpsi2)
			   (ms04-lift-eed-to-eeod-close
			    (acons eunifnode1 e lnode-assoc1) ltheta lpsi)
			 (setq lsuccess t)
			 (push (list new lnode-assoc2 ltheta2 lpsi2)
			       (get *edag-lift-info* 'ms04-search-lifts))))))
		 (unless lsuccess
		   (msgf "Problem Lifting " (car op)))))))
	 (if (eq (car op) 'EUNIF1)
	     (incf (ms04-search-num-eunif1s new))
	   (incf (ms04-search-num-eunif2s new)))
	 (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
	 (ms04-stop-duping exps new)
	 (ms04-stop-primsubing setvars new)
	 (setf (ms04-search-edag new) edag2)
	 (setf (ms04-search-banned new) 'COMPUTE)
	 (ms04-update-delayed new node-assoc)
	 new)))
    (SUBST
     (let ((theta (nth 3 op))
	   (new nil)
	   (exps (nth 4 op))
	   (setvars (nth 5 op))
	   (node-assoc nil))
       (multiple-value-setq (new node-assoc)
	 (ms04-search-do-subst theta curr (format nil "~d" (nth 2 op))))
       (case (nth 2 op)
	 (I (incf (ms04-search-num-uimits new)))
	 (P (incf (ms04-search-num-uprojs new)))
	 (t nil))
       (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
       (ms04-stop-duping exps new)
       (ms04-stop-primsubing setvars new)
       (setf (ms04-search-banned new) 'COMPUTE)
       (when (and MS04-USE-SEMANTICS MS04-SEMANTIC-PRUNING)
	 (push (cons 'REBUILD-SEMANTIC-SOLNS T) (ms04-search-aux-info new)))
      (ms04-kill-constraints new node-assoc) ; substitutions make us stop delaying and restart with full problem
       new))
    (DUP
     (let* ((exp (nth 2 op))
	    (newarc (eeod-duplicate-expnode exp))
	    (node-assoc nil))
       (declare (special node-assoc))
       (let ((edag2 (copy-eeod-rec-1 (ms04-search-edag curr))) ; this is overkill, but safe
	     (new (copy-ms04-search-1 curr "D")))
	 (when *edag-lift-info*
	   (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	     (when li
	       (let* ((lnode-assoc (cadr li))
		      (lnode-assoc1 (factor-node-assoc node-assoc lnode-assoc))
		      (ltheta (caddr li))
		      (lpsi (cadddr li)))
		 (let* ((exp1 (cdr (assoc exp node-assoc)))
			(gexp (cdr (assoc exp lnode-assoc)))
			(unlifted-exparc1
			 (find-if-not #'(lambda (arc)
					  (assoc (ext-exp-open-arc-node arc) lnode-assoc1))
				      (ext-exp-open-dag-arcs exp1)))
			(unlifted-exparc2
			 (when gexp
			   (find-if-not #'(lambda (arc)
					    (let ((x (ext-exp-arc-node arc)))
					      (rassoc (ext-exp-dag-below-lambdaeqw x) lnode-assoc1)))
					(ext-exp-dag-arcs gexp)))))
		   (when (and unlifted-exparc1 unlifted-exparc2)
		     (let ((ev (ext-exp-open-arc-exp-term unlifted-exparc1))
			   (cwff (ext-exp-arc-exp-term unlifted-exparc2)))
		       (push (cons ev cwff) ltheta)
		       (push (cons (ext-exp-open-arc-node unlifted-exparc1)
				   (ext-exp-arc-node unlifted-exparc2))
			     lnode-assoc1))))
		 (multiple-value-bind
		     (lnode-assoc2 ltheta2 lpsi2)
		     (ms04-lift-eed-to-eeod-close lnode-assoc1 ltheta lpsi)
		   (push (list new lnode-assoc2 ltheta2 lpsi2) (get *edag-lift-info* 'ms04-search-lifts)))))))
	 (incf (ms04-search-num-dups new))
	 (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
	 (setf (ms04-search-edag new) edag2)
	 (setf (ms04-search-banned new) 'COMPUTE)
	 (ms04-update-delayed new node-assoc)
	 (setf (ext-exp-open-dag-arcs exp) ; go back to the original edag by removing arc
	       (remove newarc (ext-exp-open-dag-arcs exp)))
	 new)))
    ((MATE-FLEXRIGID MATE-FLEXRIGID-NEG)
     (let* ((flexatom (nth 2 op))
	    (rigatom (nth 3 op))
	    (posatom1 nil)
	    (negatom1 nil)
	    (matenode1 nil)
	    (theta (nth 5 op))
	    (exps (nth 6 op))
	    (setvars (nth 7 op))
	    (node-assoc nil))
       (declare (special node-assoc))
       (let ((edag2 (ms04-eeod-subst-deepen-1 theta (ms04-search-edag curr)))
	     (new (copy-ms04-search-1 curr (format nil "~dM" (nth 4 op)))))
	 (let ((node1 (cdr (assoc flexatom node-assoc)))
	       (node2 (cdr (assoc rigatom node-assoc))))
	   (when (and (ext-exp-open-dag-p node1)
		      (ext-exp-open-dag-p node2)
		      (eq (ext-exp-open-dag-kind node2) 'ATOM))
	     (let ((node1s (if (eq (car op) 'MATE-FLEXRIGID-NEG)
			       (if (eq (ext-exp-open-dag-kind node1) 'NEG)
				   (car (ext-exp-open-dag-kids node1))
				 nil)
			     node1)))
	       (when (and (ext-exp-open-dag-p node1s)
			  (not (equal (ext-exp-open-dag-positive node1s)
				      (ext-exp-open-dag-positive node2))))
		 (setq posatom1 (if (ext-exp-open-dag-positive node1s) node1s node2))
		 (setq negatom1 (if (ext-exp-open-dag-positive node1s) node2 node1s))
		 (setq matenode1 (eeod-mate posatom1 negatom1)))))) ; if we found everything ok, then mate them - otherwise we just did an imit, still progress
	 (when *edag-lift-info*
	   (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	     (when li
	       (let* ((lnode-assoc (cadr li))
		      (lnode-assoc1 (factor-node-assoc node-assoc lnode-assoc))
		      (ltheta (caddr li))
		      (lpsi (cadddr li))
		      (lposatom (cdr (assoc posatom1 lnode-assoc1)))
		      (lnegatom (cdr (assoc negatom1 lnode-assoc1))))
		 (multiple-value-bind (factors newtheta)
		     (ms04-lift-factor theta ltheta lpsi t)
		   (if factors
		       (progn
			 (when (and lposatom lnegatom)
			   (let ((e (car (eed-mated-p lposatom lnegatom))))
			     (when e
			       (push (cons matenode1 e) lnode-assoc1))))
			 (multiple-value-setq
			     (lnode-assoc1 newtheta lpsi)
			   (ms04-lift-eed-to-eeod-close lnode-assoc1 newtheta lpsi t))
			 (push (list new lnode-assoc1 newtheta lpsi)
			       (get *edag-lift-info* 'ms04-search-lifts)))
		     (msgf "Problem Lifting Subst for " (car op))))))))
	 (if (eq (car op) 'MATE-FLEXRIGID)
	     (incf (ms04-search-num-flexrigid-mates new))
	   (incf (ms04-search-num-flexrigid-neg-mates new)))
	 (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
	 (ms04-stop-duping exps new)
	 (ms04-stop-primsubing setvars new)
	 (setf (ms04-search-edag new) edag2)
	 (setf (ms04-search-banned new) 'COMPUTE)
	 (ms04-kill-constraints new node-assoc) ; substitutions make us stop delaying and restart with full problem
	 (when (and MS04-USE-SEMANTICS MS04-SEMANTIC-PRUNING)
	   (push (cons 'REBUILD-SEMANTIC-SOLNS T) (ms04-search-aux-info new)))
	 new)))
    ((MATE-FLEXRIGID-PROJ MATE-FLEXRIGID-NEG-PROJ FLEX-EUNIF)
     (let* ((flexatom (nth 2 op))
	    (rigatom (nth 3 op))
	    (neg (equal (ext-exp-open-dag-positive flexatom)
			(ext-exp-open-dag-positive rigatom)))
	    (theta (nth 4 op))
	    (exps (nth 5 op))
	    (setvars (nth 6 op))
	    (node-assoc nil))
       (declare (special node-assoc))
       (let ((edag2 (ms04-eeod-subst-deepen-1 theta (ms04-search-edag curr)))
	     (new (copy-ms04-search-1 curr "C")))
	 (let ((node1 (cdr (assoc flexatom node-assoc)))
	       (node2 (cdr (assoc rigatom node-assoc))))
	   (when (and (ext-exp-open-dag-p node1)
		      (ext-exp-open-dag-p node2))
	     (let ((node1s (if neg
			       (if (eq (ext-exp-open-dag-kind node1) 'NEG)
				   (car (ext-exp-open-dag-kids node1))
				 nil)
			     node1)))
	       (when node1s
		 (catch 'fail
		   (eeod-generalized-connection-1 node1s node2))))))
	 (when *edag-lift-info*
	   (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	     (when li
	       (let* ((lnode-assoc (cadr li))
		      (lnode-assoc1 (factor-node-assoc node-assoc lnode-assoc))
		      (ltheta (caddr li))
		      (lpsi (cadddr li)))
		 (multiple-value-bind (factors newtheta)
		     (ms04-lift-factor theta ltheta lpsi t)
		   (if factors
		       (multiple-value-bind
			   (lnode-assoc2 newtheta2 lpsi2)
			   (ms04-lift-eed-to-eeod-close lnode-assoc1 newtheta lpsi t)
			 (push (list new lnode-assoc2 newtheta2 lpsi2)
			       (get *edag-lift-info* 'ms04-search-lifts)))
		     (msgf "Problem Lifting Subst for " (car op))))))))
	 (case (car op)
	   (MATE-FLEXRIGID-PROJ (incf (ms04-search-num-flexrigid-proj-mates new)))
	   (MATE-FLEXRIGID-NEG-PROJ (incf (ms04-search-num-flexrigid-neg-proj-mates new)))
	   (FLEX-EUNIF (incf (ms04-search-num-flex-eunifs new)))
	   (t nil))
	 (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
	 (ms04-stop-duping exps new)
	 (ms04-stop-primsubing setvars new)
	 (setf (ms04-search-edag new) edag2)
	 (setf (ms04-search-banned new) 'COMPUTE)
	 (ms04-kill-constraints new node-assoc) ; substitutions make us stop delaying and restart with full problem
	 (when (and MS04-USE-SEMANTICS MS04-SEMANTIC-PRUNING)
	   (push (cons 'REBUILD-SEMANTIC-SOLNS T) (ms04-search-aux-info new)))
	 new)))
    (PRIMSUB-QUANT-GENTP
     (let ((qinfo (nth 2 op))
	   (new (copy-ms04-search-1 curr "T")))
       (when *edag-lift-info*
	 (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	   (when li
	     (push (cons new (cdr li)) (get *edag-lift-info* 'ms04-search-lifts)))))
       (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
       (setf (ms04-search-primsub-quant-evar new) qinfo)
       new))
    (PRIMSUB
     (let* ((setvar (nth 2 op))
	    (theta (nth 3 op))
	    (pkind (nth 5 op))
	    (exps (nth 7 op))
	    (node-assoc nil))
       (declare (special node-assoc))
       (let ((edag2 (ms04-eeod-subst-deepen-1 theta (ms04-search-edag curr))) ; this version of subst copies - vital here
	     (new (copy-ms04-search-1 curr
				      (format nil "~d"
					      (case pkind
						(FORALL 'F)
						(EXISTS 'E)
						(AND 'A)
						(OR 'O)
						(EQUALS '=)
						(PROJ 'P)
						(NOT "~")
						(NOT-EQUALS "~=")
						(NOT-PROJ "~P")
						(t 'Y))))))
	 (when *edag-lift-info*
	   (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	     (when li
	       (let* ((lnode-assoc (cadr li))
		      (lnode-assoc1 (factor-node-assoc node-assoc lnode-assoc))
		      (ltheta (caddr li))
		      (lpsi (cadddr li)))
		 (multiple-value-bind (factors newtheta)
		     (ms04-lift-factor theta ltheta lpsi t)
		   (if factors
		       (multiple-value-bind
			   (lnode-assoc2 newtheta2 lpsi2)
			   (ms04-lift-eed-to-eeod-close lnode-assoc1 newtheta lpsi)
			 (push (list new lnode-assoc2 newtheta2 lpsi2)
			       (get *edag-lift-info* 'ms04-search-lifts)))
		     (msgf "Problem Lifting " (car op))))))))
	 (case pkind
	   (FORALL (incf (ms04-search-num-foralls new)))
	   (EXISTS (incf (ms04-search-num-exists new)))
	   (AND (incf (ms04-search-num-ands new)))
	   (OR (incf (ms04-search-num-ors new)))
	   (NOT (incf (ms04-search-num-nots new)))
	   (EQUALS (incf (ms04-search-num-equals new)))
	   (PROJ (incf (ms04-search-num-projs new)))
	   (NOT-EQUALS (incf (ms04-search-num-not-equals new)))
	   (NOT-PROJ (incf (ms04-search-num-not-projs new))))
	 (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
	 (ms04-stop-duping exps new)
	 (setf (ms04-search-primsub-quant-evar new) nil)
	 (when MS04-PRENEX-PRIMSUBS
	   (case pkind
	     ((FORALL EXISTS)
	      nil) ; do nothing, still allowed any primsub
	     (AND
	      (unless (member setvar (ms04-search-cnf-evars new)) ; no more quants
		(push setvar (ms04-search-cnf-evars new))))
	     (OR
	      (unless (member setvar (ms04-search-clause-evars new)) ; no more quants/conjunctions
		(push setvar (ms04-search-clause-evars new))))
	     (t ; no more quants/conjunctions/disjunctions
	      (push setvar (ms04-search-lit-evars new)))))
	 (setf (ms04-search-edag new) edag2)
	 (setf (ms04-search-banned new) 'COMPUTE)
	 (ms04-kill-constraints new node-assoc) ; substitutions make us stop delaying and restart with full problem
	 new)))
    (DELAY-UNIF
     (let* ((eqngoal (caddr op))
	    (exps (nth 3 op))
	    (wff (ext-exp-open-dag-shallow eqngoal))
	    (lft (cdar wff))
	    (rght (cdr wff))
	    (utp (unabbreviated-type lft))
	    (subst0 (ms04-search-unif-subst curr))
	    (new nil))
       (multiple-value-bind (dpairs subst)
	   (ms04-search-decidable-unification
	    (cons (list utp
			(simul-substitute-l-term-var subst0 lft)
			(simul-substitute-l-term-var subst0 rght)
			nil)
		  (ms04-search-unif-dpairs curr))
	    (ms04-search-unif-subst curr)
	    (ms04-search-banned curr))
	 (unless (or (eq dpairs 'FAILED) ; if new stays NIL, it will get pruned by ms04-search-ok
		     (and (> MS04-CHECK-UNIF-DEPTH 0)
			  (eq 'FAILED (ms04-unify dpairs subst
						  (ms04-search-banned curr) MS04-CHECK-UNIF-DEPTH '(MORE SUCCESS)))))
	   (if (and MS04-EAGER-UNIF-SUBST subst)
	       (progn
		 (setq new (ms04-search-do-subst subst curr "Ud"))
		 (setf (ms04-search-unif-subst new) nil)
		 (setf (ms04-search-unif-dpairs new) nil)
		 (setf (ms04-search-delayed new) nil))
	     (progn
	       (setq new (copy-ms04-search-1 curr "Ud"))
	       (setf (ms04-search-unif-subst new) subst)
	       (setf (ms04-search-unif-dpairs new) dpairs)
	       (push (list 'UNIF eqngoal) (ms04-search-delayed new))
	       (when (and *edag-lift-info* new)
		 (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
		   (when li
		     (push (cons new (cdr li)) (get *edag-lift-info* 'ms04-search-lifts)))))))))
       (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
       (ms04-stop-duping exps new)
       new))
    (DELAY-CONN
     (let* ((flex (caddr op))
	    (rig (nth 3 op))
	    (exps (nth 4 op))
	    (setvars (nth 5 op))
	    (wff1 (ext-exp-open-dag-shallow flex))
	    (wff2 (ext-exp-open-dag-shallow rig))
	    (subst0 (ms04-search-unif-subst curr))
	    (new (copy-ms04-search-1 curr "Fcd")))
       (incf (ms04-search-num-delayed-conns new))
       (multiple-value-bind (dpairs subst)
	   (ms04-search-decidable-unification
	    (cons (list 'O
			(lnorm (simul-substitute-l-term-var subst0 wff1))
			(if (equal (ext-exp-open-dag-positive flex)
				   (ext-exp-open-dag-positive rig))
			    (cons 'NOT (lnorm (simul-substitute-l-term-var subst0 wff2)))
			  (lnorm (simul-substitute-l-term-var subst0 wff2)))
			nil)
		  (ms04-search-unif-dpairs curr))
	    (ms04-search-unif-subst curr)
	    (ms04-search-banned curr))
	 (if (or (eq dpairs 'FAILED)
		 (and (> MS04-CHECK-UNIF-DEPTH 0)
		      (eq 'FAILED (ms04-unify dpairs subst (ms04-search-banned curr) MS04-CHECK-UNIF-DEPTH '(MORE SUCCESS)))))
	     (setf (ms04-search-unif-subst new) 'FAILED) ; this will get pruned by ms04-search-ok
	   (progn
	     (setf (ms04-search-unif-dpairs new) dpairs)
	     (setf (ms04-search-unif-subst new) subst)
	     (push (list 'CONN flex rig) (ms04-search-delayed new)))))
       (when *edag-lift-info*
	 (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	   (when li
	     (push (cons new (cdr li)) (get *edag-lift-info* 'ms04-search-lifts)))))
       (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
       (ms04-stop-duping exps new)
       (ms04-stop-primsubing setvars new)
       new))
    (ADD-SET-CONSTRAINT
     (let* ((maxmin (caddr op))
	    (setvar (nth 3 op))
	    (constr (nth 4 op))
	    (a (ms04-search-set-constrs curr))
	    (new (copy-ms04-search-1 curr "Sc")))
       (push (cons 'SET-CONSTR constr) (ms04-search-delayed new))
       (if a
	   (setf (ms04-search-set-constrs new)
		 (list setvar maxmin (cons constr (caddr a))))
	 (setf (ms04-search-set-constrs new)
	       (list setvar maxmin (list constr))))
       (when *edag-lift-info*
	 (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	   (when li
	     (push (cons new (cdr li)) (get *edag-lift-info* 'ms04-search-lifts)))))
       (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
       new))
    (SOLVE-SET-CONSTRAINTS ; lifting is abandoned completely at this point
     (let* ((sconstrs (ms04-search-set-constrs curr))
	    (setvar (car sconstrs))
	    (maxmin (cadr sconstrs))
	    (constrl (caddr sconstrs))
	    (new (copy-ms04-search-1 curr "So")))
       (ms04-solve-set-constraints setvar maxmin constrl new)
       (setf (ms04-search-set-constrs new) nil)
       (setf (ms04-search-delayed new) nil)
       (setf (ms04-search-weight new) (+ (ms04-search-weight new) (caadr op)))
       new))
    (t
     (msgf "Unrecognized Option: " (car op))
     nil)))

(defun ms04-stop-duping (exps new)
  (when MS04-DUP-EARLY
    (dolist (exp exps)
      (let ((n (ext-exp-open-dag-name exp)))
	(unless (member n (ms04-search-dupd-exps new))
	  (push n (ms04-search-dupd-exps new)))))))

(defun ms04-stop-primsubing (setvars new)
  (dolist (sv setvars)
    (unless (member sv (ms04-search-almost-atomic-evars new))
      (push sv (ms04-search-almost-atomic-evars new)))))

(defun ms04-search-do-subst (theta curr str)
  (let ((node-assoc nil))
    (declare (special node-assoc))
    (let ((edag2 (ms04-eeod-subst-deepen-1 theta (ms04-search-edag curr))) ; this version of subst copies - vital here
	  (new (copy-ms04-search-1 curr str)))
      (setf (ms04-search-edag new) edag2)
      (when *edag-lift-info*
	(let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	  (when li
	    (let* ((lnode-assoc (cadr li))
		   (lnode-assoc1 (factor-node-assoc node-assoc lnode-assoc))
		   (ltheta (caddr li))
		   (lpsi (cadddr li)))
	      (multiple-value-bind (factors newtheta)
		  (ms04-lift-factor theta ltheta lpsi t)
		(if factors
		    (multiple-value-bind
			(lnode-assoc2 newtheta2 lpsi2)
			(ms04-lift-eed-to-eeod-close lnode-assoc1 newtheta lpsi)
		      (push (list new lnode-assoc2 newtheta2 lpsi2)
			    (get *edag-lift-info* 'ms04-search-lifts)))
		  (msgf "Problem Lifting Subst")))))))
      (values new node-assoc))))

(defun factor-node-assoc (node-assoc1 node-assoc2)
  (mapcar #'(lambda (na)
	      (let ((a1 (assoc (car na) node-assoc1)))
;		(unless a1
;		  (setq cl-user::node-assoc1 node-assoc1 cl-user::node-assoc2 node-assoc2 cl-user::na na)
;		  (break))
		(cons (cdr a1) (cdr na))))
	  node-assoc2))

(defun ms04-kill-constraints (curr node-assoc)
  (let ((newconn nil)
	(*current-edag* (ms04-search-edag curr)))
    (declare (special *current-edag*))
    (dolist (d (ms04-search-delayed curr))
      (when (eq (car d) 'CONN)
	(let ((n1 (cdr (assoc (cadr d) node-assoc)))
	      (n2 (cdr (assoc (caddr d) node-assoc))))
	  (when (and n1 n2 (ext-exp-open-dag-p n1) (ext-exp-open-dag-p n2))
	    (if (equal (ext-exp-open-dag-positive n1) (ext-exp-open-dag-positive n2))
		(if (equal (ext-exp-open-dag-kind n1) 'NEG)
		    (progn
		      (setq newconn T)
		      (catch 'fail
			(eeod-generalized-connection-1 (car (ext-exp-open-dag-kids n1)) n2)))
		  nil)
	      (progn
		(setq newconn T)
		(catch 'fail
		  (eeod-generalized-connection-1 n1 n2))))))))
    (when (and *edag-lift-info* newconn)
      (let ((li (assoc curr (get *edag-lift-info* 'ms04-search-lifts))))
	(when li
	  (let* ((lnode-assoc (cadr li))
		 (ltheta (caddr li))
		 (lpsi (cadddr li)))
	    (multiple-value-bind (lnode-assoc2 ltheta2 lpsi2)
		(ms04-lift-eed-to-eeod-close lnode-assoc ltheta lpsi t)
	      (setf (get *edag-lift-info* 'ms04-search-lifts)
		    (cons (list curr lnode-assoc2 ltheta2 lpsi2)
			  (remove li (get *edag-lift-info* 'ms04-search-lifts))))))))))
  (setf (ms04-search-delayed curr) NIL)
  (setf (ms04-search-unif-dpairs curr) NIL)
  (setf (ms04-search-unif-subst curr) NIL)
  (setf (ms04-search-set-constrs curr) NIL))

(defun ms04-update-delayed (curr node-assoc)
  (when (ms04-search-set-constrs curr)
    (setf (ms04-search-set-constrs curr)
	  (list (car (ms04-search-set-constrs curr))
		(cadr (ms04-search-set-constrs curr))
		(mapcar #'(lambda (x)
			    (mapcar #'(lambda (y)
					(cdr (assoc y node-assoc)))
				    x))
			(caddr (ms04-search-set-constrs curr))))))
  (setf (ms04-search-delayed curr)
	(mapcar #'(lambda (x)
		    (cons (car x) 
			  (mapcar #'(lambda (y)
				      (cdr (assoc y node-assoc)))
				  (cdr x))))
		(ms04-search-delayed curr))))

(defun copy-ms04-search-1 (s str)
  (let ((s2 (copy-ms04-search s)))
    (setf (ms04-search-name s2) (format nil "~d~d" (ms04-search-name s) str))
    (incf (ms04-search-weight s2))
    s2))

(defun ms04-map-constraints (constraints node-assoc)
  (mapcar #'(lambda (c)
	      (mapcar #'(lambda (n)
			  (cdr (assoc n node-assoc)))
		      c))
	  constraints))

(defun ms04-measure-delay-unif (eqngoal curr)
  (declare (ignore eqngoal curr))
  (list MS04-WEIGHT-DELAY-UNIF 'MS04-WEIGHT-DELAY-UNIF))

(defun ms04-measure-delay-flexconn (flex rig curr)
  (declare (ignore flex rig curr))
  (let ((v '(MS04-WEIGHT-DELAY-UNIF MS03-WEIGHT-FLEXRIGID-MATE)))
    (cons (evaluate-ext-weight-1 v) v)))

(defun ms04-measure-delay-rigid-mate (n1 n2 curr)
  (declare (ignore n1 n2 curr))
  (let ((v '(MS04-WEIGHT-DELAY-UNIF MS03-WEIGHT-RIGID-MATE)))
    (cons (evaluate-ext-weight-1 v) v)))

(defun ms04-measure-delay-eunif1 (n1 n2 curr)
  (declare (ignore n1 n2 curr))
  (let ((v '(MS04-WEIGHT-DELAY-UNIF MS03-WEIGHT-EUNIF1)))
    (cons (evaluate-ext-weight-1 v) v)))

(defun ms04-measure-dup (exp)
  (declare (ignore exp))  
  (list MS04-DUP-WEIGHT 'MS04-DUP-WEIGHT))

(defun ms04-measure-eunif1 (eqn eqngoal aux-eqns banned)
  (let* ((sh1 (ext-exp-open-dag-shallow eqn))
	 (sh2 (ext-exp-open-dag-shallow eqngoal))
	 (lft1 (cdar sh1))
	 (rght1 (cdr sh1))
	 (lft2 (cdar sh2))
	 (rght2 (cdr sh2))
	 (lh1 (head lft1))
	 (rh1 (head rght1))
	 (lh2 (head lft2))
	 (rh2 (head rght2))
	 (utp (unabbreviated-type lft1))
	 (dpairs (list (list utp lft1 lft2 nil)
		       (list utp rght1 rght2 nil))))
    (let ((v (ms04-quick-eunification-weight dpairs aux-eqns banned))
	  (mm (+ (length (ext-exp-open-dag-arcs eqn))
		 (length (ext-exp-open-dag-arcs eqngoal)))))
      (push (cons mm 'MS04-WEIGHT-MULTIPLE-EUNIF1S) v)
      (push 'MS03-WEIGHT-EUNIF1 v)
      (when (and (not (ext-exp-var-p lh2))
		 (eq lh2 rh2))
	(push 'MS04-WEIGHT-EUNIF-DECS v))
      (unless (or (and (not (ext-exp-var-p lh1)) (not (ext-exp-var-p lh2))
		       (eq lh1 lh2))
		  (and (not (ext-exp-var-p rh1)) (not (ext-exp-var-p rh2))
		       (eq rh1 rh2)))
	(push 'MS04-WEIGHT-EUNIF-DIFF-HEADS v))
      (cons (evaluate-ext-weight-1 v) v))))

(defun ms04-measure-eunif2 (eqn eqngoal aux-eqns banned)
  (let* ((sh1 (ext-exp-open-dag-shallow eqn))
	 (sh2 (ext-exp-open-dag-shallow eqngoal))
	 (lft1 (cdar sh1))
	 (rght1 (cdr sh1))
	 (lft2 (cdar sh2))
	 (rght2 (cdr sh2))
	 (lh1 (head lft1))
	 (rh1 (head rght1))
	 (lh2 (head lft2))
	 (rh2 (head rght2))
	 (utp (unabbreviated-type lft1))
	 (dpairs (list (list utp lft1 rght2 nil)
		       (list utp rght1 lft2 nil))))
    (let ((v (ms04-quick-eunification-weight dpairs aux-eqns banned))
	  (mm (+ (length (ext-exp-open-dag-arcs eqn))
		 (length (ext-exp-open-dag-arcs eqngoal)))))
      (push (cons mm 'MS04-WEIGHT-MULTIPLE-EUNIF2S) v)
      (push 'MS03-WEIGHT-EUNIF2 v)
      (when (and (not (ext-exp-var-p lh2))
		 (eq lh2 rh2))
	(push 'MS04-WEIGHT-EUNIF-DECS v))
      (unless (or (and (not (ext-exp-var-p lh1)) (not (ext-exp-var-p rh2))
		       (eq lh1 rh2))
		  (and (not (ext-exp-var-p rh1)) (not (ext-exp-var-p lh2))
		       (eq rh1 lh2)))
	(push 'MS04-WEIGHT-EUNIF-DIFF-HEADS v))
      (cons (evaluate-ext-weight-1 v) v))))

(defun ms04-measure-mate-flexrigid (flexatom rigatom theta aux-eqns banned)
  (let* ((sh1 (lnorm (simul-substitute-l-term-var theta (ext-exp-open-dag-shallow flexatom))))
	 (sh2 (lnorm (simul-substitute-l-term-var theta (ext-exp-open-dag-shallow rigatom))))
	 (dpairs (mapcar #'(lambda (x y)
			     (list (unabbreviated-type x) x y nil))
			 (args sh1)
			 (args sh2))))
    (let ((v (ms04-quick-eunification-weight dpairs aux-eqns banned))
	  (mm (length (ext-exp-open-dag-arcs rigatom))))
      (push (cons mm 'MS04-WEIGHT-MULTIPLE-MATES) v)
      (push 'MS03-WEIGHT-FLEXRIGID-MATE v)
      (cons (evaluate-ext-weight-1 v) v))))

(defun ms04-measure-mate-flexrigid-proj (flexatom rigatom theta aux-eqns banned)
  (let* ((sh1 (lnorm (simul-substitute-l-term-var theta (ext-exp-open-dag-shallow flexatom))))
	 (sh2 (lnorm (simul-substitute-l-term-var theta (ext-exp-open-dag-shallow rigatom))))
	 (dpairs (mapcar #'(lambda (x y)
			     (list (unabbreviated-type x) x y nil))
			 (args sh1)
			 (args sh2))))
    (let ((v (ms04-quick-eunification-weight dpairs aux-eqns banned))
	  (mm (length (ext-exp-open-dag-arcs rigatom))))
      (push (cons mm 'MS04-WEIGHT-MULTIPLE-MATES) v)
      (push 'MS03-WEIGHT-FLEXRIGID-MATE v)
      (push 'MS04-WEIGHT-FLEXRIGID-PROJ-MATE v)
      (cons (evaluate-ext-weight-1 v) v))))

(defun ms04-measure-eunif1-flexrigid (flexatom rigatom theta aux-eqns banned)
  (let* ((sh1 (lnorm (simul-substitute-l-term-var theta (ext-exp-open-dag-shallow flexatom))))
	 (sh2 (lnorm (simul-substitute-l-term-var theta (ext-exp-open-dag-shallow rigatom))))
	 (dpairs (mapcar #'(lambda (x y)
			     (list (unabbreviated-type x) x y nil))
			 (args sh1)
			 (args sh2))))
    (let ((v (ms04-quick-eunification-weight dpairs aux-eqns banned))
	  (mm (length (ext-exp-open-dag-arcs rigatom))))
      (push (cons mm 'MS04-WEIGHT-MULTIPLE-MATES) v)
      (push 'MS04-WEIGHT-FLEX-EUNIF v)
      (cons (evaluate-ext-weight-1 v) v))))

(defun ms04-measure-imit (trm1 trm2 aux-eqns banned)
  (let ((v (ms04-quick-eunification-weight
	    (list (list (unabbreviated-type trm1) trm1 trm2 nil))
	    aux-eqns banned)))
    (push 'MS03-WEIGHT-IMITATE v)
    (cons (evaluate-ext-weight-1 v) v)))

(defun ms04-measure-proj (trm1 trm2 aux-eqns banned)
  (let ((v (ms04-quick-eunification-weight
	    (list (list (unabbreviated-type trm1) trm1 trm2 nil))
	    aux-eqns banned)))
    (push 'MS03-WEIGHT-PROJECT v)
    (cons (evaluate-ext-weight-1 v) v)))

; may use semantics in general - try to check if constraints are mutually solvable
; or if bound has been exceeded
; It would make sense to set the banned reln here and return NIL if acyclicity is detected.
; Then we'd be allowed to generate some options with general theta's that may lead to acyclicity.
(defun ms04-search-ok (curr)
  (declare (special *ms04-bound-reached* *ms04-search-bound*))
  (if (ms04-search-p curr)
      (if (set-ms04-banned-vars curr) ; if NIL, there is a cycle - not OK ; also sets the banned slot of curr
	  (cond ((> (ms04-search-weight curr) *ms04-search-bound*)
		 (setq *ms04-bound-reached* t)
		 (when (eq MS04-VERBOSE 'MAX)
		   (msgf "Exceeded Depth Bound - Pruning " curr))
		 NIL)
		((eq (ms04-search-unif-subst curr) 'FAILED) ; unif constraints are unsolvable, reject it
		 (when (eq MS04-VERBOSE 'MAX)
		   (msgf "Unification Failed - Pruning " curr))
		 NIL)
		((and MS04-USE-SEMANTICS MS04-SEMANTIC-PRUNING)
		 (let ((accept NIL)
		       (reject NIL))
		   (let ((a1 (assoc 'REBUILD-SEMANTIC-SOLNS (ms04-search-aux-info curr))))
		     (when a1
		       (setf (ms04-search-aux-info curr) (remove a1 (ms04-search-aux-info curr)))
					; We prune the search state if there is an interpretation of the parameters
					; (everything except exp vars)
					; and a vertical path such that the signed shallow formula of
					; every leaf (relative to the vp) is true
					; (i.e., neg nodes are false and pos nodes are true)
					; with respect to every assignment of exp vars -
					; this doesn't take the banning relation into account.
					; This will not prune out states that could come from a lifting of a ground proof,
					; so we do not lose completeness.
		       (let ((old-iwff (cdr (assoc 'INT-DUAL-WFF (ms04-search-aux-info curr))))
			     (iwff (ms04-intermediate-dual-formula curr)))
			 (unless (wffeq iwff old-iwff) ; if they're equal, don't retest semantics
			   (when (eq MS04-VERBOSE 'MAX)
			     (msgf "New Intermediate Wff:" t (iwff . gwff)))
			   (let* ((usyms (uninterp-syms-of iwff))
				  (evars (remove-if-not #'ext-exp-var-p usyms))
				  (params (remove-if #'ext-exp-var-p usyms))
				  (solns (find-model-solutions params evars iwff)))
			     (if (and (consp solns) (eq (car solns) 'NO-SOLN))
				 (progn
				   (when (eq MS04-VERBOSE 'MAX)
				     (msgf "Semantically Pruned " (ms04-search-name curr) t "Using Interpretation " t (cdr solns) t
					   "For Variables " (params . gwfflist)))
				   (setq reject T)) ; we can prune it - there are no solutions for some interp of params
			       (let ((a2 (assoc 'SEMANTIC-SOLNS (ms04-search-aux-info curr))))
				 (setf (ms04-search-aux-info curr) (remove a2 (ms04-search-aux-info curr)))
				 (if (eq solns 'FAILED)
				     (progn ; problem is too hard to look at semantically, we surpassed some bound
				       (push (cons 'SEMANTIC-SOLNS 'FAILED) (ms04-search-aux-info curr))) ; remember - didn't get solns
				   (setq accept T))
				 (progn ; otherwise we save the set of solns for each interp of params and accept it
				   (push (list 'SEMANTIC-SOLNS params evars solns) (ms04-search-aux-info curr)) ; save the solns for weighting options further down the branch
				   (setq accept T))))))))
		     (if (or accept reject)
			 accept
		       T)))) ; accept it by default
		(T T)) ; we'll accept it as a valid search state by default
	nil) ; acyclicity violation
    nil)) ; not an ms04-search struct, so certainly not OK

(defun ms04-print-search-state (s &optional str d)
  (declare (ignore d))
  (format str "~d [~d]~%" (ms04-search-name s) (ms04-search-weight s)))

(defun ms04-print-constraint (maxmin c)
  (if (eq maxmin 'MAX)
      (msg (car c) " -> " ((mapcar #'(lambda (x) (ext-exp-open-dag-name x)) (cdr c)) . symbollist))
    (msg ((mapcar #'(lambda (x) (ext-exp-open-dag-name x)) (cdr c)) . symbollist) " -> " (car c))))

(defun eeod-flexflex-goal-p (node)
  (and (eq (ext-exp-open-dag-kind node) 'EQNGOAL)
       (let ((sh (ext-exp-open-dag-shallow node)))
	 (and (ext-exp-var-p (head (cdar sh)))
	      (ext-exp-var-p (head (cdr sh)))))))

(defun eeod-flexrigid-goal-p (node)
  (and (eq (ext-exp-open-dag-kind node) 'EQNGOAL)
       (let* ((sh (ext-exp-open-dag-shallow node))
	      (lh (head (cdar sh)))
	      (rh (head (cdr sh))))
	 (or
	  (and (ext-exp-var-p lh)
	       (not (ext-exp-var-p rh)))
	  (and (ext-exp-var-p rh)
	       (not (ext-exp-var-p lh)))))))

; include exp's, atom's, eqn+'s, eqngoal's
; dupd-exps: list of names of exp nodes we can go beneath - otherwise just include the exp node for a possible duplication,
;            if an exp node is on this list, we won't duplicate it anymore, so don't include it on the vpath
; returns NIL or (<list of nodes>)
(defun ms04-first-vp (nodes &optional dupd-exps others &key (flexflex nil) (posandnegflex t) (posflex t) (negflex t) (delayed nil))
  (if (position-if #'(lambda (d)
		       (subsetp (cdr d) others))
		   delayed)
      nil ; we're delaying vps passing through these nodes
    (if nodes
	(let ((node (find-if-not #'(lambda (x)
				     (eq (eeod-junctive x) 'DIS))
				 nodes)))
	  (if node
	      (let ((k (ext-exp-open-dag-kind node))
		    (pos (ext-exp-open-dag-positive node)))
		(cond ((and (eeod-flexflex-goal-p node) (not flexflex))
		       nil)
		      ((eq k 'FLEX)
		       (if (and posflex negflex)
			   (if posandnegflex
			       (ms04-first-vp (remove node nodes) dupd-exps (cons node others)
					      :flexflex flexflex :posandnegflex t
					      :posflex t :negflex t :delayed delayed)
			     (if (find-if #'(lambda (x) (and (eq (ext-exp-open-dag-kind x) 'FLEX)
							     (not (equal (ext-exp-open-dag-positive x)
									 pos))))
					  others)
				 nil ; the path has a pos and neg flex - don't return it
			       (ms04-first-vp (remove node nodes) dupd-exps (cons node others)
					      :flexflex flexflex :posandnegflex nil
					      :posflex t :negflex t :delayed delayed)))
			 (if (or (and pos posflex) (and (not pos) negflex))
			     (ms04-first-vp (remove node nodes) dupd-exps (cons node others)
					    :flexflex flexflex :posandnegflex posandnegflex
					    :posflex posflex :negflex negflex :delayed delayed)
			   nil)))
		      ((or (member k '(ATOM EQNGOAL))
			   (and (ext-exp-open-dag-positive node) (eq k 'EQN)))
		       (let* ((nodes1 (remove node nodes))
			      (conns (ext-exp-open-dag-kids node)))
			 (dolist (oth others)
			   (let ((kids2 (ext-exp-open-dag-kids oth)))
			     (setq nodes1 (append (intersection conns kids2) nodes1))))
			 (ms04-first-vp nodes1 dupd-exps (cons node others)
					:flexflex flexflex :posandnegflex posandnegflex
					:posflex posflex :negflex negflex :delayed delayed)))
		      ((eq k 'EXP)
		       (if MS04-DUP-EARLY
			   (if (member (ext-exp-open-dag-name node) dupd-exps)
			       (ms04-first-vp
				(append (ext-exp-open-dag-kids node) (remove node nodes))
				dupd-exps others ; don't include exp node on vp if we're done dup'ing it
				:flexflex flexflex :posandnegflex posandnegflex
				:posflex posflex :negflex negflex :delayed delayed)
			     (ms04-first-vp
			      (append (ext-exp-open-dag-kids node) (remove node nodes))
			      dupd-exps (cons node others) ; include exp node on vp so we can dup it
			      :flexflex flexflex :posandnegflex posandnegflex
			      :posflex posflex :negflex negflex :delayed delayed))
			 (ms04-first-vp
			  (append (ext-exp-open-dag-kids node) (remove node nodes))
			  dupd-exps (cons node others) ; not dup'ing early - include exp node in case we want to dup now AND go beneath it
			  :flexflex flexflex :posandnegflex posandnegflex
			  :posflex posflex :negflex negflex :delayed delayed)))
		      (t
		       (ms04-first-vp
			(append (ext-exp-open-dag-kids node) (remove node nodes))
			dupd-exps others
			:flexflex flexflex :posandnegflex posandnegflex
			:posflex posflex :negflex negflex :delayed delayed))))
	    (let ((dnode (car nodes)))
	      (do ((kids (ext-exp-open-dag-kids dnode) (cdr kids))
		   (vp nil
		       (ms04-first-vp (cons (car kids) (cdr nodes)) dupd-exps others
				      :flexflex flexflex :posandnegflex posandnegflex
				      :posflex posflex :negflex negflex :delayed delayed)))
		  ((or vp (null kids)) vp)))))
      (list others))))

; lifting - see if the option makes sense in terms of the ground images
(defun ms04-valid-lift (op node-assoc theta psi &optional verbose)
  (case (car op)
    (MATE
     (let* ((posatom (nth 2 op))
	    (negatom (nth 3 op))
	    (gposatom (cdr (assoc posatom node-assoc)))
	    (gnegatom (cdr (assoc negatom node-assoc))))
       (when (and gposatom gnegatom)
	 (when verbose
	   (msgf "Checking ")
	   (print-ms04-option op)
	   (msgf ((ext-exp-dag-shallow gposatom) . gwff) " . " ((ext-exp-dag-shallow gnegatom) . gwff)))
	 (eed-mated-p gposatom gnegatom))))
    ((EUNIF1 EUNIF2)
     (let* ((eqn (nth 2 op))
	    (eqngoal (nth 3 op))
	    (geqn (cdr (assoc eqn node-assoc)))
	    (geqngoal (cdr (assoc eqngoal node-assoc))))
       (when (and geqn geqngoal)
	 (when verbose
	   (msgf "Checking ")
	   (print-ms04-option op)
	   (msgf ((ext-exp-dag-shallow geqn) . gwff) " . " ((ext-exp-dag-shallow geqngoal) . gwff)))
	 (eed-eunifk-p geqn geqngoal (car op)))))
    ((SUBST PRIMSUB)
     (let ((optheta (nth 3 op)))
       (when verbose
	 (msgf "Checking ")
	 (print-ms04-option op)
	 (dolist (th optheta)
	   (msgf "  " ((simul-substitute-l-term-var (append psi theta) (car th)) . gwff) t
		 " -> " ((simul-substitute-l-term-var (append psi theta) (cdr th)) . gwff) t)))
       ; ok if we can factor theta as (theta2 ; (psi ; optheta))
       (ms04-lift-factor optheta theta psi)))
    (DUP
     (let* ((exp (nth 2 op))
	    (gexp (cdr (assoc exp node-assoc)))
	    (unlifted-expkid
	     (when gexp
	       (find-if-not #'(lambda (x)
				(rassoc (ext-exp-dag-below-lambdaeqw x) node-assoc))
			    (ext-exp-dag-kids gexp)))))
       (if (eq (car op) 'DUP)
	   unlifted-expkid
	 (not unlifted-expkid))))
    ((MATE-FLEXRIGID MATE-FLEXRIGID-NEG)
     (let* ((flexatom (nth 2 op))
	    (rigatom (nth 3 op))
	    (gflexatom (cdr (assoc flexatom node-assoc)))
	    (grigatom (cdr (assoc rigatom node-assoc)))
	    (optheta (nth 5 op)))
       (when (and gflexatom grigatom)
	 (when verbose
	   (msgf "Checking ")
	   (print-ms04-option op)
	   (msgf ((ext-exp-dag-shallow gflexatom) . gwff) t
		 ((ext-exp-dag-shallow grigatom) . gwff) t)
	   (msgf "ground terms:" t)
	   (dolist (th optheta)
	     (msgf "  " ((simul-substitute-l-term-var (append psi theta) (car th)) . gwff) t
		   " -> " ((simul-substitute-l-term-var (append psi theta) (cdr th)) . gwff) t)))
	 (when (eq (car op) 'MATE-FLEXRIGID-NEG)
	   (if (eq (ext-exp-dag-kind gflexatom) 'NEG)
	       (setq gflexatom (car (ext-exp-dag-kids gflexatom)))
	     (setq gflexatom nil)))
	 (and gflexatom
	      (eed-mated-p gflexatom grigatom)
	      (ms04-lift-factor optheta theta psi)))))
    ((MATE-FLEXRIGID-PROJ MATE-FLEXRIGID-NEG-PROJ FLEX-EUNIF)
     (let* ((flexatom (nth 2 op))
	    (rigatom (nth 3 op))
	    (neg (equal (ext-exp-open-dag-positive flexatom) (ext-exp-open-dag-positive rigatom)))
	    (optheta (nth 4 op)))
       (when verbose
	 (msgf "Checking ")
	 (print-ms04-option op)
	 (dolist (th optheta)
	   (msgf "  " ((lnorm (simul-substitute-l-term-var (append psi theta) (car th))) . gwff) t
		 " -> " ((lnorm (simul-substitute-l-term-var (append psi theta) (cdr th))) . gwff) t)))
       (if neg
	   (wffeq-ab (lnorm (simul-substitute-l-term-var (append psi theta) (ext-exp-open-dag-shallow flexatom)))
		     (cons 'NOT (lnorm (simul-substitute-l-term-var (append psi theta) (ext-exp-open-dag-shallow rigatom)))))
	 (wffeq-ab (lnorm (simul-substitute-l-term-var (append psi theta) (ext-exp-open-dag-shallow flexatom)))
		   (lnorm (simul-substitute-l-term-var (append psi theta) (ext-exp-open-dag-shallow rigatom)))))))
    (PRIMSUB-QUANT-GENTP
     (let* ((qinfo (nth 2 op))
	    (b (car qinfo))
	    (setvar (cadr qinfo))
	    (tpn (caddr qinfo)) ; look at the previous type to see if we need to keep generating
	    (cwff (cdr (assoc setvar theta))))
       (if cwff
	   (let ((cl (classify-lambda-term cwff)))
	     (if (eq (car cl) b) ; we need to primsub with this binder
		 (let ((n (ms04-quant-type-code (cadr cl) (length *individual-types*) *individual-types*))) ; get the code for the type (if it has one)
		   (if n
		       (if (<= tpn n) ; haven't gotten there yet
			   t
			 nil)
		     nil)) ; we can't generate this type
	       nil)) ; abandon this - we don't need a primsub with this binder
	 nil)))
    (DELAY-UNIF
     (let* ((eqngoal (caddr op))
	    (wff (ext-exp-open-dag-shallow eqngoal))
	    (lft (cdar wff))
	    (rght (cdr wff))
	    (ilft (lnorm (simul-substitute-l-term-var (append psi theta) lft)))
	    (irght (lnorm (simul-substitute-l-term-var (append psi theta) rght))))
       (when verbose
	 (msgf "Checking ")
	 (print-ms04-option op)
	 (msgf "  " (ilft . gwff) t " -> " (irght . gwff)))
       (wffeq-ab ilft irght)))
    (DELAY-CONN
     (let* ((flex (caddr op))
	    (rig (nth 3 op))
	    (wff1 (ext-exp-open-dag-shallow flex))
	    (wff2 (ext-exp-open-dag-shallow rig))
	    (iwff1 (lnorm (simul-substitute-l-term-var (append psi theta) wff1)))
	    (iwff2 (lnorm (simul-substitute-l-term-var (append psi theta) wff2))))
       (when verbose
	 (msgf "Checking ")
	 (print-ms04-option op)
	 (msgf "  " (iwff1 . gwff) t " -> " (iwff2 . gwff)))
       (if (equal (ext-exp-open-dag-positive flex)
		  (ext-exp-open-dag-positive rig))
	   (and (not-p iwff1)
		(wffeq-ab (cdr iwff1) iwff2))
	 (wffeq-ab iwff1 iwff2))))
    (ADD-SET-CONSTRAINT
     (let* ((constr (nth 4 op))
	    (gconstr (remove-if #'null (mapcar #'(lambda (x) (cdr (assoc x node-assoc))) constr))))
       ; check if there are vertical paths through the image of the new constr, reject it if there are any
       (ext-exp-dag-complete-conjs gconstr)))
    (SOLVE-SET-CONSTRAINTS
     T) ; always go ahead and say T here - don't test this
    (t nil)))

(defun ms04-lift-factor (newtheta gtheta psi &optional print-result)
  (declare (ignore print-result))
  (let ((newtheta2 (mapcar #'(lambda (x)
			       (cons (car x)
				     (simul-substitute-l-term-var psi (cdr x))))
			   newtheta)))
    (multiple-value-bind (factors gtheta2)
	(ms04-lift-factor-1 newtheta2 gtheta nil)
      (if factors
	  (progn
	    (dolist (g gtheta)
	      (unless (assoc (car g) newtheta2)
		(push g gtheta2)))
;	    (when (and (eq MS04-VERBOSE 'MAX) gtheta2 print-result)
;	      (msgf "Lifting Subst:" t)
;	      (dolist (g gtheta2)
;		(msgf ((car g) . gwff) " |-> " ((cdr g) . gwff))))
	    (values T gtheta2))
	(values NIL NIL)))))

(defun ms04-lift-factor-1 (theta gtheta newtheta)
  (if theta
      (let* ((ev (caar theta))
	     (pattern (cdar theta))
	     (a (assoc ev gtheta))
	     (cwff (cdr a)))
	(if cwff
	    (multiple-value-bind (factors newtheta2)
		(ms04-lift-factor-2 cwff pattern newtheta)
	      (if factors
		  (ms04-lift-factor-1 (cdr theta) gtheta newtheta2)
		(values nil nil)))
	  (values nil nil)))
    (values T newtheta)))

(defun ms04-lift-factor-2 (cwff pattern newtheta)
  (let ((tp1 (unabbreviated-type cwff))
	(tp2 (unabbreviated-type pattern)))
    (if (equal tp1 tp2)
	(ms04-lift-factor-3 tp1 (lambda-norm cwff) (lambda-norm pattern) newtheta nil)
      (values nil nil))))

(defun ms04-lift-factor-3 (tp cwff pattern newtheta boundvars)
  (if (consp tp)
      (let ((x (fresh-var-1 (cdr tp))))
	(ms04-lift-factor-3 (car tp) (lambda-norm (cons cwff x)) (lambda-norm (cons pattern x))
			    newtheta (cons x boundvars)))
    (ms04-lift-factor-4 tp cwff pattern newtheta boundvars)))

(defun ms04-lift-factor-4 (tp cwff pattern newtheta boundvars)
  (if (a-bd-wff-p cwff)
      (if (a-bd-wff-p pattern)
	  (let* ((y (bindvar cwff))
		 (z (bindvar pattern))
		 (qtp (unabbreviated-type y))
		 (qtp2 (unabbreviated-type z)))
	    (if (equal qtp qtp2)
		(let ((x (fresh-var-1 qtp)))
		  (ms04-lift-factor-4
		   tp
		   (substitute-l-term-var x y (cdr cwff))
		   (substitute-l-term-var x (bindvar pattern) (cdr pattern))
		   newtheta
		   (cons x boundvars)))
	      (values nil nil)))
	(if (or (boundwff-p pattern) (not-p pattern))
	    (values nil nil)
	  (let ((h2 (head pattern)))
	    (if (ext-exp-var-p h2)
		(ms04-lift-factor-6 tp cwff pattern h2 (args pattern) newtheta boundvars)
	      (values nil nil)))))
    (if (e-bd-wff-p cwff)
	(if (e-bd-wff-p pattern)
	  (let* ((y (bindvar cwff))
		 (z (bindvar pattern))
		 (qtp (unabbreviated-type y))
		 (qtp2 (unabbreviated-type z)))
	    (if (equal qtp qtp2)
		(let ((x (fresh-var-1 qtp)))
		  (ms04-lift-factor-4
		   tp
		   (substitute-l-term-var x y (cdr cwff))
		   (substitute-l-term-var x (bindvar pattern) (cdr pattern))
		   newtheta
		   (cons x boundvars)))
	      (values nil nil)))
	  (if (or (boundwff-p pattern) (not-p pattern))
	      (values nil nil)
	    (let ((h2 (head pattern)))
	      (if (ext-exp-var-p h2)
		  (ms04-lift-factor-6 tp cwff pattern h2 (args pattern) newtheta boundvars)
		(values nil nil)))))
      (if (not-p cwff)
	  (if (not-p pattern)
	      (ms04-lift-factor-4 tp (cdr cwff) (cdr pattern) newtheta boundvars)
	    (if (boundwff-p pattern)
		(values nil nil)
	      (let ((h2 (head pattern)))
		(if (ext-exp-var-p h2)
		    (ms04-lift-factor-6 tp cwff pattern h2 (args pattern) newtheta boundvars)
		  (values nil nil)))))
	(if (or (boundwff-p pattern) (not-p pattern))
	    (values nil nil)
	  (ms04-lift-factor-5 tp cwff pattern (head cwff) (args cwff) (head pattern) (args pattern) newtheta boundvars))))))

(defun ms04-lift-factor-5 (tp cwff pattern h1 args1 h2 args2 newtheta boundvars)
  (if (ext-exp-var-p h2)
      (let ((p (assoc h2 newtheta)))
	(if p
	    (ms04-lift-factor-3 tp cwff (lambda-norm (substitute-l-term-var (cdr p) (car p) pattern))
				    newtheta boundvars)
	  (ms04-lift-factor-6 tp cwff pattern h2 args2 newtheta boundvars)))
    (if (eq h1 h2)
	(let ((factors t))
	  (do ((args3 args1 (cdr args3))
	       (args4 args2 (cdr args4)))
	      ((or (null args3) (null args4) (not factors))
	       (if factors
		   (values T newtheta)
		 (values nil nil)))
	    (multiple-value-setq (factors newtheta)
	      (ms04-lift-factor-3 (unabbreviated-type (car args3))
				  (car args3) (car args4)
				  newtheta boundvars))))
      (values NIL NIL))))

(defun ms04-lift-factor-6 (tp cwff pattern h2 args2 newtheta boundvars)
  (let ((p (assoc h2 newtheta)))
    (if p
	(ms04-lift-factor-3 tp cwff (lambda-norm (substitute-l-term-var (cdr p) (car p) pattern))
			    newtheta boundvars)
      (if (eeod-pattern-p-4 pattern boundvars)
	  (if (find-if #'(lambda (v)
			   (and (not (member v args2)) (free-in v cwff)))
		       boundvars)
	      (values nil nil)
	    (let ((wff cwff))
	      (dolist (v (reverse args2))
		(setq wff (acons v 'lambda wff)))
	      (values T (acons h2 wff newtheta))))
	(values NIL NIL)))))

(defun lift-eed-to-ms04 (eed)
  (declare (special *edag-lift-info*))
  (ms04-initialize-search (ext-exp-dag-shallow eed))
  (create-ext-exp-open-dag (ext-exp-dag-shallow eed))
  (setq *edag-lift-info* (intern (gensym "LIFT")))
  (setf (get *edag-lift-info* 'eed) eed)
  (setf (get *edag-lift-info* 'selvars) (ext-exp-dag-sel-vars eed))
  (if (ms04-search-1)
      (msgf "Succeeded in lifting proof") ; ms04-search-1 calls lift-eed-to-ms04-info to give some help for flags
    (msgf "Failed to lift proof")))

(defun lift-eed-to-ms04-info (sym)
  (let ((soln (get sym 'ms04-search-solution))
	(root nil)
	(soln-trace (get sym 'ms04-trace))
	(soln-path nil)
	(good-states nil)
	(unif-depth (get sym 'ms04-solve-unif-depth))
	(soln-abs-depth 0) ; number, in terms of steps
	(soln-depth nil) ; in terms of flags
	(soln-depth2 nil) ; in terms of flags
	(minimize nil))
    (do* ((curr soln (cadr step))
	  (step (assoc curr soln-trace)
		(assoc curr soln-trace)))
	((null step))
      (let ((goodwt (caddr step)))
	(setq root (cadr step))
	(push step soln-path)
	(push curr good-states)
	(incf soln-abs-depth)
	(setq soln-depth (append goodwt soln-depth))))
    (msgf "Solution Depth (without weights):" soln-abs-depth)
    (dolist (d soln-depth)
      (let ((flag (if (consp d) (cdr d) d)))
	(push flag minimize)))
    (setq minimize (remove-duplicates minimize))
    (dolist (flag minimize)
      (let ((z (lift-eed-to-saturation-flag-coeff flag soln-depth)))
	(when (> z 0)
	  (push (cons z flag) soln-depth2))))
    (setq minimize (mapcar #'cdr soln-depth2))
    (when soln-depth2
      (msgf "Formula for Depth of Solution:" t)
      (msg soln-abs-depth)
      (dolist (zf soln-depth2)
	(let ((z (car zf))
	      (flag (cdr zf)))
	  (if (= z 1)
	      (msg " + " flag)
	    (msg " + " z "*" flag)))))
    (dolist (step soln-trace)
      (let ((kid (car step))
	    (par (cadr step))
	    (wt (caddr step)))
	(unless (find-if #'(lambda (x)
			     (or (and (consp x) (not (member (cdr x) minimize)))
				 (not (member x minimize))))
			 wt)
	  (let ((a (assoc 'KIDS (ms04-search-aux-info par))))
	    (if a
		(push (list wt kid) (cdr a))
	      (push (list 'KIDS (list wt kid)) (ms04-search-aux-info par)))))))
    (dolist (opwt (get sym 'ms04-options-weights))
      (let* ((par (car opwt))
	     (a (assoc 'KIDS (ms04-search-aux-info par)))
	     (b (if a (cdr a) nil)))
	(unless a
	  (setq a (list 'KIDS))
	  (push a (ms04-search-aux-info par)))
	(dolist (wt (cdr opwt))
	  (unless (assoc wt b :test #'equal)
	    (push (list wt NIL) (cdr a))))))
    (lift-eed-to-ms04-flag-suggest root soln minimize soln-abs-depth soln-depth2 unif-depth)))

(defun lift-eed-to-ms04-flag-suggest (root soln minimize soln-abs-depth soln-depth unif-depth)
  (let* ((flag-sugg (mapcar #'(lambda (f) (cons f 0)) minimize))
	 (depth soln-abs-depth)
	 (meas (lift-eed-to-ms04-flag-measure flag-sugg root soln depth))
	 (n (length flag-sugg))
	 (ch 10)
	 (i 0)
	 (weight-flags '(
					; basic weights
			 MS03-WEIGHT-RIGID-MATE 
			 MS03-WEIGHT-FLEXRIGID-MATE
			 MS03-WEIGHT-EUNIF1 MS03-WEIGHT-EUNIF2
			 MS03-WEIGHT-IMITATE MS03-WEIGHT-PROJECT
			 MS04-WEIGHT-EUNIF-DECS
			 MS04-WEIGHT-EUNIF-DIFF-HEADS
			 MS04-WEIGHT-MULTIPLE-MATES
			 MS04-WEIGHT-MULTIPLE-EUNIF1S
			 MS04-WEIGHT-MULTIPLE-EUNIF2S
			 MS04-WEIGHT-FLEX-EUNIF
			 ; weights associated with quick unification
			 MS03-WEIGHT-FLEXFLEXSAME-O
			 MS03-WEIGHT-FLEXFLEXDIFF-O MS03-WEIGHT-FLEXFLEXSAME MS03-WEIGHT-FLEXFLEXDIFF
			 MS03-WEIGHT-FLEXRIGID-BRANCH MS03-WEIGHT-FLEXRIGID-O MS03-WEIGHT-FLEXRIGID-O
			 MS03-WEIGHT-RIGIDRIGIDSAME-O MS03-WEIGHT-RIGIDRIGIDDIFF-O MS03-WEIGHT-RIGIDRIGID-EQN
			 MS03-WEIGHT-RIGIDRIGID-FLEXEQN MS03-WEIGHT-RIGIDRIGID-NOEQN MS03-WEIGHT-FLEXRIGID-EQN
			 MS03-WEIGHT-FLEXRIGID-FLEXEQN MS03-WEIGHT-FLEXRIGID-NOEQN
			 MS03-WEIGHT-OCCURS-CHECK MS03-WEIGHT-BANNED-SELS
			 ; other unification related weights
			 MS04-WEIGHT-DELAY-UNIF
			 ; dup weight
			 MS04-DUP-WEIGHT
			 ; primsub weights
			 MS03-WEIGHT-PRIMSUB-FIRST-FORALL MS03-WEIGHT-PRIMSUB-FIRST-EXISTS
			 MS03-WEIGHT-PRIMSUB-NEXT-FORALL MS03-WEIGHT-PRIMSUB-NEXT-EXISTS
			 MS04-WEIGHT-PRIMSUB-NEXTTP
			 MS03-WEIGHT-PRIMSUB-FIRST-AND MS03-WEIGHT-PRIMSUB-FIRST-OR
			 MS03-WEIGHT-PRIMSUB-NEXT-AND MS03-WEIGHT-PRIMSUB-NEXT-OR
			 MS03-WEIGHT-PRIMSUB-FIRST-PROJ MS03-WEIGHT-PRIMSUB-NEXT-PROJ
			 MS03-WEIGHT-PRIMSUB-FIRST-NOT-PROJ MS03-WEIGHT-PRIMSUB-NEXT-NOT-PROJ ; only if MS04-PRENEX-PRIMSUBS T
			 MS03-WEIGHT-PRIMSUB-FIRST-EQUALS MS03-WEIGHT-PRIMSUB-NEXT-EQUALS
			 MS03-WEIGHT-PRIMSUB-FIRST-NOT-EQUALS MS03-WEIGHT-PRIMSUB-NEXT-NOT-EQUALS ; only if MS04-PRENEX-PRIMSUBS T
			 MS04-WEIGHT-PRIMSUB-FIRST-NOT MS04-WEIGHT-PRIMSUB-NEXT-NOT ; only if MS04-PRENEX-PRIMSUBS NIL
					; set constraint weight
			 MS04-WEIGHT-ADD-SET-CONSTRAINT
			 )))
    (push (cons 'MS04-SOLVE-UNIF-DEPTH (or unif-depth 0)) flag-sugg)
    (if unif-depth
	(push (cons 'MS04-CHECK-UNIF-DEPTH (ash unif-depth -1)) flag-sugg)
      (push (cons 'MS04-CHECK-UNIF-DEPTH 0) flag-sugg))
    (push (cons 'MS04-MAX-DUPS (ms04-search-num-dups soln)) flag-sugg)
    (push (cons 'MS04-MAX-RIGID-MATES (ms04-search-num-rigid-mates soln)) flag-sugg)
    (push (cons 'MS04-MAX-EUNIF2S (ms04-search-num-eunif2s soln)) flag-sugg)
    (push (cons 'MS04-MAX-EUNIF1S (ms04-search-num-eunif1s soln)) flag-sugg)
    (push (cons 'MS04-MAX-FLEXRIGID-NEG-MATES (ms04-search-num-flexrigid-neg-mates soln)) flag-sugg)
    (push (cons 'MS04-MAX-FLEXRIGID-MATES (ms04-search-num-flexrigid-mates soln)) flag-sugg)
    (push (cons 'MS04-MAX-FLEXRIGID-NEG-PROJ-MATES (ms04-search-num-flexrigid-neg-proj-mates soln)) flag-sugg)
    (push (cons 'MS04-MAX-FLEXRIGID-PROJ-MATES (ms04-search-num-flexrigid-proj-mates soln)) flag-sugg)
    (push (cons 'MS04-MAX-FLEX-EUNIFS (ms04-search-num-flex-eunifs soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-EXISTS (ms04-search-num-exists soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-FORALL (ms04-search-num-foralls soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-NOT (ms04-search-num-nots soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-OR (ms04-search-num-ors soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-AND (ms04-search-num-ands soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-NOT-EQUALS (ms04-search-num-not-equals soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-EQUALS (ms04-search-num-equals soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-PROJ (ms04-search-num-projs soln)) flag-sugg)
    (push (cons 'MS04-MAX-PRIMSUB-NOT-PROJ (ms04-search-num-not-projs soln)) flag-sugg)
    (push (cons 'MS04-MAX-PROJS (ms04-search-num-uprojs soln)) flag-sugg)
    (push (cons 'MS04-MAX-IMITS (ms04-search-num-uimits soln)) flag-sugg)
    (push (cons 'MS04-MAX-DELAYED-CONNS (ms04-search-num-delayed-conns soln)) flag-sugg)
    (loop while (and (> ch 0) (< i 100)) do
	  (let ((bestchoice nil)
		(bestmeas meas))
	    (dotimes (j n)
	      (let ((a (nth j flag-sugg)))
		(rplacd a (+ (cdr a) ch))
		(let ((meas2 (lift-eed-to-ms04-flag-measure flag-sugg root soln
							    (+ (* ch (lift-eed-to-saturation-flag-coeff (car a) soln-depth))
							       depth))))
		  (rplacd a (- (cdr a) ch))
		  (when (< meas2 bestmeas)
		    (setq bestchoice j bestmeas meas2)))))
	    (if bestchoice
		(progn
		  (let ((a (nth bestchoice flag-sugg)))
		    (rplacd a (+ (cdr a) ch))
		    (setq depth (+ (* ch (lift-eed-to-saturation-flag-coeff (car a) soln-depth))
				   depth)))
		  (setq meas bestmeas))
	      (decf ch))
	    (incf i)
	    (when (= (mod i 10) 0)
	      (decf ch))))
    (push (cons 'MS04-INITIAL-DEPTH (max 1 depth)) flag-sugg)
    (dolist (w weight-flags)
      (unless (assoc w flag-sugg)
	(push (cons w (* 100 (floor (/ (+ depth 200) 100)))) flag-sugg)))
    (msgf "Suggested Values:" t)
    (dolist (fv flag-sugg)
      (msgf (car fv) " : " (cdr fv) t))
    (msgf "Options:" t)
    (msgf "1) Set Flags to these values." t)
    (msgf "2) Create a Mode for these flag values." t)
    (msgf "3) Do Nothing" t)
    (let ((o (get-a-number 3 1)))
      (cond ((equal o 1)
	     (dolist (fv flag-sugg)
	       (set-flag (car fv) (cdr fv))))
	    ((equal o 2)
	     (let ((mode-flags (cons (list 'DEFAULT-MS 'MS04-2)
				     (mapcar #'(lambda (x)
						 (list (car x) (cdr x)))
					     flag-sugg)))
		   (symb (intern (concatenate 'string "MODE-MS04-SUGG"))))
	       (dolist (f global-flaglist)
		 (unless (consp f)
		   (when (intersection (get f 'subjects) '(MS04-2 EXT-SEARCH IMPORTANT))
		     (unless (assoc f mode-flags)
		       (push (list f (maint::process-flag f)) mode-flags)))))
	       (do ((name nil))
		   ((not (or (null name) (memq name global-modelist)))
		    (eval `(defmode ,name ,(cons 'flag-settings (core::flagsort mode-flags))
			     (mhelp "Mode created by MS04-2 lifting.")))
		    (reorganize)
		    (mode name))
		 (prompt-read name nil (msgf "Name for mode? ") 'symbol symb
			      ((? (msgf "Provide a name for the new mode."))
			       (?? (mhelp 'tps-mode))))
		 (when (and (memq name global-modelist)
			    (query "That mode already exists. Overwrite it?" t))
		   (dolist (prop (plist name))
		     (remprop name prop))
		   (setq global-modelist (delete name global-modelist))))))
	    (t nil)))))

;    (msgf "Choose Small Values : " neg-flags t)
;    (msgf "Choose Large Values : " pos-flags t)
;    (msgf "Choose Wisely : " used-flags t)
;    (let ((smart-matr nil)
;	  (smart-bdvec nil)
;	  (smart-maximize nil))
;      (do ((rows matr (cdr rows))
;	   (vec bdvec (cdr vec)))
;	  ((null rows))
;	(let* ((row (car rows))
;	       (sm-row (mapcar #'(lambda (f)
;				   (let* ((i (position f weight-flags)))
;				     (nth i row)))
;			       used-flags)))
;	  (when (find-if-not #'(lambda (x) (equal x 0)) sm-row)
;	    (push sm-row smart-matr)
;	    (push (car vec) smart-bdvec))))
;      (dolist (row maximize)
;	(let ((sm-row (mapcar #'(lambda (f)
;				  (let* ((i (position f weight-flags)))
;				    (nth i row)))
;			      used-flags)))
;	  (when (find-if-not #'(lambda (x) (equal x 0)) sm-row)
;	    (push sm-row smart-maximize))))
;      (msgf "Flag Matrix :" t)
;      (dolist (row smart-matr)
;	(msgf row t))
;      (msgf "Lower Bound Vector :" t smart-bdvec)
;      (msgf "==========================" t "Maximize" t)
;      (dolist (z smart-maximize)
;	(msgf z t)))))

(defun lift-eed-to-ms04-flag-measure (flag-sugg root soln depth)
  (if (eq root soln)
      (values 0 T)
    (if (< depth 0)
	(values 0 NIL) ; won't reach this state now
      (if root
	  (let* ((kids (cdr (assoc 'KIDS (ms04-search-aux-info root))))
		 (kids-w (mapcar #'(lambda (x) (cons x (lift-eed-to-ms04-eval-weight flag-sugg (car x))))
				 kids))
		 (ordered (sort (copy-list kids-w) #'< :key #'cdr))
		 (meas 0)
		 (done nil))
	    (do ((kl ordered (cdr kl)))
		((or (null kl) done)
		 (values meas done))
	      (setq meas (+ meas 1
			    (lift-eed-to-ms04-flag-measure flag-sugg (cadaar kl) soln
							   (- depth (1+ (cdar kl))))))))
	(values depth NIL))))) ; more that could have been explored - probably should weigh this exponentially, but that's not helpful

(defun lift-eed-to-ms04-eval-weight (flag-sugg w)
  (apply #'+
	 (mapcar #'(lambda (x)
		     (let ((f (if (consp x) (cdr x) x))
			   (v (if (consp x) (car x) 1)))
		       (let* ((a (assoc f flag-sugg))
			      (b (if a (cdr a) 0)))
			 (* v b))))
		 w)))

(defun lift-eed-to-ms04-flag-suggest-2 (row psoln n weight-flags)
  (let ((al (mapcar #'(lambda (a f mi incr) (list a f mi incr))
		    row weight-flags (cadr psoln) (caddr psoln))))
    (if (find-if #'(lambda (a)
		     (when (and (not (= (car a) 0)) (eq (caddr a) 'MAX))
;		       (when (eq MS04-VERBOSE 'MAX)
;			 (msgf "Flag Constraint Satisfied By Maximizing " (cadr a) t))
		       T))
		 al)
	(list psoln) ; already a soln
      (let* ((pl (remove-if-not #'(lambda (a)
				    (> (car a) 0)) al))
	     (nl (remove-if-not #'(lambda (a)
				    (< (car a) 0)) al))
	     (worst-pval 
	      (apply #'+
		     (mapcar #'(lambda (a)
				 (* (car a) (caddr a)))
			     pl)))
	     (worst-nval
	      (apply #'+
		     (mapcar #'(lambda (a)
				 (* (car a) (+ (caddr a) (cadddr a))))
			     nl))))
	(when (eq MS04-VERBOSE 'MAX)
;	  (msgf "Trying To Satisfy:" t)
;	  (if pl
;	      (progn
;		(msg (caar pl) "*" (cadar pl))
;		(dolist (a (cdr pl)) (msg " + " (car a) "*" (cadr a))))
;	    (msg "0"))
;	  (msg " > ")
;	  (if nl
;	      (progn
;		(msg (caar nl) "*" (cadar nl))
;		(dolist (a (cdr nl)) (msg " + " (car a) "*" (cadr a))))
;	    (msg "0"))
	  (dolist (a (append pl nl))
	    (msgf (caddr a) " <= " (cadr a) " <= " (cadddr a))))
	(if (> (+ worst-pval worst-nval) 0) ; already a soln
	    (list psoln)
					; otherwise, not a soln
	  (let ((best-pval
		 (apply #'+
		     (mapcar #'(lambda (a)
				 (* (car a) (+ (caddr a) (cadddr a))))
			     pl)))
		(best-nval
		 (apply #'+
			(mapcar #'(lambda (a)
				    (* (car a) (caddr a)))
				nl)))
		(psolns (list (list (+ (ash n 3) (car psoln)) (cadr psoln) (caddr psoln)))))
	    (unless (<= (+ best-pval best-nval) 0) ; can't be made into a soln
	      (dolist (p pl) ; try to force solutions by partitioning bounds for pl's
		(let* ((pl2 (mapcar #'(lambda (a)
					(let ((y (ash (cadddr a) -1)))
					  (list (car a) (cadr a) (+ (caddr a) y)
						y)))
				    (remove p pl)))
		       (nl2 (mapcar #'(lambda (a)
					(list (car a) (cadr a) (caddr a)
					      (ash (cadddr a) -1)))
				    nl))
		       (worst-pval2
			(apply #'+
			       (mapcar #'(lambda (a)
					   (* (car a) (caddr a)))
				       pl2)))
		       (worst-nval2
			(apply #'+
			       (mapcar #'(lambda (a)
					   (* (car a) (+ (caddr a) (cadddr a))))
				       nl2)))
		       (worst-val (+ worst-pval2 worst-nval2))
		       (incr (1+ (floor (/ (- worst-val) (car p))))))
		  (unless (> incr (cadddr p))
		    (let ((lowerbd1
			   (mapcar #'(lambda (f)
				       (if (eq f (cadr p))
					   (+ (caddr p) incr)
					 (let ((p2 (find-if #'(lambda (a) (eq f (cadr a))) pl2)))
					   (if p2
					       (caddr p2)
					     (let ((n2 (find-if #'(lambda (a) (eq f (cadr a))) nl2)))
					       (if n2
						   (caddr n2)
						 (let ((a2 (find-if #'(lambda (a) (eq f (cadr a))) al)))
						   (caddr a2))))))))
				   weight-flags))
			  (incrbd1
			   (mapcar #'(lambda (f)
				       (if (eq f (cadr p))
					   (- (cadddr p) incr)
					 (let ((p2 (find-if #'(lambda (a) (eq f (cadr a))) pl2)))
					   (if p2
					       (cadddr p2)
					     (let ((n2 (find-if #'(lambda (a) (eq f (cadr a))) nl2)))
					       (if n2
						   (cadddr n2)
						 (let ((a2 (find-if #'(lambda (a) (eq f (cadr a))) al)))
						   (cadddr a2))))))))
				   weight-flags))
			  (goodsolnnum (car psoln)))
		      (unless (or (find-if-not #'(lambda (x) ; sanity check
						   (or (eq x 'MAX) (>= x 0)))
					       lowerbd1)
				  (find-if-not #'(lambda (x) ; sanity check
						   (>= x 0))
					       incrbd1))
			(push (list goodsolnnum lowerbd1 incrbd1) psolns)))))))
	    psolns))))))

(defun lift-eed-to-ms04-flag-suggest-3 (psoln maximize-matr maxval)
  (let ((n (car psoln))
	(lowerbd (mapcar #'(lambda (x)
			     (if (eq x 'MAX)
				 maxval
			       x))
			 (cadr psoln)))
	(m 0))
    (dolist (row maximize-matr)
      (setq m (+ m
		 (apply #'+ (mapcar #'* lowerbd row)))))
    (- n m)))

(defun ms04-initialize-edag-lift-info (curr)
  (declare (special *edag-lift-info*))
  (setf (get *edag-lift-info* 'ms04-trace) nil)
  (setf (get *edag-lift-info* 'ms04-search-lifts) nil)
  (setf (get *edag-lift-info* 'ms04-options-weights) nil)
  (when (get *edag-lift-info* 'eed)
    (let ((eed (get *edag-lift-info* 'eed)))
      (multiple-value-bind (node-assoc theta psi)
	  (ms04-lift-eed-to-eeod-close-1 (acons (ms04-search-edag curr) eed nil) nil nil)
	(setf (get *edag-lift-info* 'ms04-search-lifts)
	      (list (list curr node-assoc theta psi)))
	(when (eq ms04-verbose 'MAX)
	  (msgf "Lifting Initialization" t)
	  (msgf "Master EProof:" t)
	  (print-ext-exp-dag-verbose eed)
	  (msgf "Initial Search Edag:" t)
	  (print-ext-exp-open-dag-verbose (ms04-search-edag curr))
	  (msgf "Node Association" t)
	  (dolist (a node-assoc) (msgf (car a) " -> " (cdr a) t))
	  (msgf "psi ")
	  (dolist (xt psi) (msg "[" ((car xt) . gwff) " -> " ((cdr xt) . gwff) "] ")))
	nil))))

; psi (selvar param subst) changes as we go through sel arcs
; we may optionally go beneath connections
(defun ms04-lift-eed-to-eeod-close (node-assoc theta psi &optional (conns nil))
  (multiple-value-bind
      (node-assoc2 theta2 psi2)
      (if conns
	  (ms04-lift-eed-to-eeod-close-2 node-assoc theta psi)
	(ms04-lift-eed-to-eeod-close-1 node-assoc theta psi))
;    (when (eq MS04-VERBOSE 'MAX)
;      (msgf "Extended Node Association" t)
;      (dolist (a node-assoc2) (msgf (car a) " -> " (cdr a) t))
;      (msgf "Extended psi ")
;      (dolist (xt psi2) (msg "[" ((car xt) . gwff) " -> " ((cdr xt) . gwff) "] ")))
    (values node-assoc2 theta2 psi2)))

(defun ms04-lift-eed-to-eeod-close-1 (node-assoc theta psi)
  (if node-assoc
      (let ((eeod (caar node-assoc))
	    (eed (cdar node-assoc))
	    (nl2 (cdr node-assoc)))
	(if (and (eq (ext-exp-dag-kind eed) 'REW)
		 (member (ext-exp-dag-rew-just eed) '(LAMBDA EQUIVWFFS)))
	    (let ((eed2 (ext-exp-arc-node (car (ext-exp-dag-arcs eed)))))
	      (ms04-lift-eed-to-eeod-close-1
	       (acons eeod eed2 nl2) theta psi))
	  (if (eq (ext-exp-open-dag-kind eeod) (ext-exp-dag-kind eed))
	      (case (ext-exp-open-dag-kind eeod)
		((NEG DIS CON IMP REW TRUE FALSE)
		 (mapc #'(lambda (arc1 arc2)
			   (unless (assoc (ext-exp-open-arc-node arc1) node-assoc)
			     (push (cons (ext-exp-open-arc-node arc1)
					 (ext-exp-arc-node arc2))
				   nl2)))
		       (ext-exp-open-dag-arcs eeod)
		       (ext-exp-dag-arcs eed))
		 (multiple-value-bind (node-assoc2 theta2 psi2)
		     (ms04-lift-eed-to-eeod-close-1 nl2 theta psi)
		   (values (cons (car node-assoc) node-assoc2) theta2 psi2)))
		(DEC
		 (dolist (arc1 (ext-exp-dag-arcs eed))
		   (let* ((i (ext-exp-arc-dec-index arc1))
			  (arc2 (find-if #'(lambda (arc) (equal (ext-exp-open-arc-dec-index arc) i))
					 (ext-exp-open-dag-arcs eeod))))
		     (when arc2
		       (unless (assoc (ext-exp-open-arc-node arc2) node-assoc)
			 (let ((x (cons (ext-exp-open-arc-node arc2)
					(ext-exp-arc-node arc1))))
			   (push x nl2))))))
		 (multiple-value-bind (node-assoc2 theta2 psi2)
		     (ms04-lift-eed-to-eeod-close-1 nl2 theta psi)
		   (values (cons (car node-assoc) node-assoc2) theta2 psi2)))
		(EXP
		 (let ((theta1 theta))
		   (when (and (ext-exp-open-dag-arcs eeod)
			      (not (cdr (ext-exp-open-dag-arcs eeod)))
			      (ext-exp-dag-arcs eed))
		     (let* ((exparc1 (car (ext-exp-open-dag-arcs eeod)))
			    (exparc2 (car (ext-exp-dag-arcs eed)))
			    (ev (ext-exp-open-arc-exp-term exparc1)))
		       (when (ext-exp-var-p ev)
			 (push (cons ev (ext-exp-arc-exp-term exparc2)) theta1)
			 (push (cons (ext-exp-open-arc-node exparc1)
				     (ext-exp-arc-node exparc2))
			       nl2))))
		   (multiple-value-bind (node-assoc2 theta2 psi2)
		       (ms04-lift-eed-to-eeod-close-1 nl2 theta1 psi)
		     (values (cons (car node-assoc) node-assoc2) theta2 psi2))))
		(SEL
		 (let* ((arc1 (car (ext-exp-open-dag-arcs eeod)))
			(arc2 (car (ext-exp-dag-arcs eed)))
			(sv1 (ext-exp-open-arc-sel-var arc1))
			(sv2 (ext-exp-arc-sel-var arc2)))
		   (unless (assoc (ext-exp-open-arc-node arc1) node-assoc)
		     (let ((x (cons (ext-exp-open-arc-node arc1)
				    (ext-exp-arc-node arc2))))
		       (push x nl2)
		       (push (cons sv1 sv2) psi)))
		   (multiple-value-bind (node-assoc2 theta2 psi2)
		       (ms04-lift-eed-to-eeod-close-1 nl2 theta psi)
		     (values (cons (car node-assoc) node-assoc2) theta2 psi2))))
		(EQN
		 (if (ext-exp-open-dag-positive eeod)
		     (multiple-value-bind (node-assoc2 theta2 psi2)
			 (ms04-lift-eed-to-eeod-close-1 nl2 theta psi)
		       (values (cons (car node-assoc) node-assoc2) theta2 psi2))
		   (progn
		     (dolist (arc2 (ext-exp-dag-arcs eed))
		       (let* ((k (ext-exp-arc-kind arc2))
			      (arc1 (find-if #'(lambda (arc)
						 (eq (ext-exp-open-arc-kind arc) k))
					     (ext-exp-open-dag-arcs eeod))))
			 (when (and arc1 (not (assoc (ext-exp-open-arc-node arc1) node-assoc)))
			   (push (cons (ext-exp-open-arc-node arc1)
				       (ext-exp-arc-node arc2))
				 nl2))))
		     (multiple-value-bind (node-assoc2 theta2 psi2)
			 (ms04-lift-eed-to-eeod-close-1 nl2 theta psi)
		       (values (cons (car node-assoc) node-assoc2) theta2 psi2)))))
		(t
		 (multiple-value-bind (node-assoc2 theta2 psi2)
		     (ms04-lift-eed-to-eeod-close-1 nl2 theta psi)
		   (values (cons (car node-assoc) node-assoc2) theta2 psi2))))
	    (multiple-value-bind (node-assoc2 theta2 psi2)
		(ms04-lift-eed-to-eeod-close-1 nl2 theta psi)
	      (values (cons (car node-assoc) node-assoc2) theta2 psi2)))))
    (values nil theta psi)))

; this version finds connections and deepens the lifting relation along them
(defun ms04-lift-eed-to-eeod-close-2 (node-assoc theta psi)
  (multiple-value-bind
      (node-assoc2 theta2 psi2)
      (ms04-lift-eed-to-eeod-close-1 node-assoc theta psi)
    (let ((node-assoc3 node-assoc2))
      (do ((na node-assoc2 (cdr na)))
	  ((null na)
	   (if (eq node-assoc3 node-assoc2)
	       (values node-assoc2 theta2 psi2)
	     (ms04-lift-eed-to-eeod-close-1 node-assoc3 theta2 psi2)))
	(let ((eeod (caar na))
	      (eed (cdar na)))
	  (cond ((eq (ext-exp-open-dag-kind eeod) 'ATOM)
		 (dolist (na2 (cdr na))
		   (let ((eeod2 (car na2))
			 (eed2 (cdr na2)))
		     (when (eq (ext-exp-open-dag-kind eeod2) 'ATOM)
		       (let ((m1 (eeod-mated-p eeod eeod2))
			     (m2 (eed-mated-p eed eed2)))
			 (when (and m1 m2)
			   (push (cons (car m1) (car m2)) node-assoc3)))))))
		((or (eq (ext-exp-open-dag-kind eeod) 'EQNGOAL)
		     (and (eq (ext-exp-open-dag-kind eeod) 'EQN)
			  (ext-exp-open-dag-positive eeod)))
		 (dolist (na2 (cdr na))
		   (let ((eeod2 (car na2))
			 (eed2 (cdr na2)))
		     (when (or (eq (ext-exp-open-dag-kind eeod2) 'EQNGOAL)
			       (and (eq (ext-exp-open-dag-kind eeod2) 'EQN)
				    (ext-exp-open-dag-positive eeod2)))
		       (let ((m1 (eeod-eunif1-p eeod eeod2))
			     (m2 (eed-eunif1-p eed eed2)))
			 (when (and m1 m2)
			   (push (cons (car m1) (car m2)) node-assoc3)))
		       (let ((m1 (eeod-eunif2-p eeod eeod2))
			     (m2 (eed-eunif2-p eed eed2)))
			 (when (and m1 m2)
			   (push (cons (car m1) (car m2)) node-assoc3)))))))))))))

(defun eta-long-subst-measure (theta)
  (apply #'+ (mapcar #'(lambda (x) (eta-long-term-measure (cdr x)))
		     theta)))

(defun eta-long-term-measure (trm)
  (eta-long-term-measure-1 trm (unabbreviated-type trm)))

(defun eta-long-term-measure-1 (trm tp)
  (if (consp tp)
      (if (lambda-bd-p trm)
	  (eta-long-term-measure-1 (cdr trm) (car tp))
	(eta-long-term-measure-1 (cons trm (fresh-var-1 (cdr tp)))
				 (car tp)))
    (eta-long-term-measure-2 trm)))

(defun eta-long-term-measure-2 (trm)
  (if (boundwff-p trm)
      (1+ (eta-long-term-measure (cdr trm)))
    (if (consp trm)
	(+ (eta-long-term-measure-2 (car trm))
	   (eta-long-term-measure (cdr trm)))
      1)))

(defun ms04-banned-sel-var-p (var par curr)
  (let ((b (assoc var (ms04-search-banned curr))))
    (and b (member par (cdr b)))))

; sets the banned slot of s to assoc list of (<expvar> <list of banned selvars>)
; returns T if there's no cycle, NIL if there is a cycle.
(defun set-ms04-banned-vars (s)
  (if (eq (ms04-search-banned s) 'COMPUTE)
      (set-ms04-banned-vars-0 s)
    T)) ; assume it's OK

(defun set-ms04-banned-vars-0 (s)
  (let ((nodes-selvars nil)
	(selvars-exparcs nil)
	(expvars-exparcs nil)
	(selvars (eeod-sel-vars (ms04-search-edag s))))
    (declare (special nodes-selvars selvars-exparcs expvars-exparcs selvars))
    (setf (ms04-search-banned s) nil)
    (dolist (sv selvars)
      (setf (get sv 'exp-vars-above) nil))
    (set-ms04-banned-vars-1 (ms04-search-edag s))
					; set banned relation
    (dolist (v expvars-exparcs)
      (push (cons (car v) (eeod-set-banned-vars-2 (cdr v) nodes-selvars selvars-exparcs))
	    (ms04-search-banned s)))
					; also, check for a cycle, return NIL if there is a cycle and T otherwise
    (let ((exparcs-selvars nil) ; assoc list of (<exparc> <sel vars dom by exparc>)
	  (cycle nil))
      (dolist (sv-eas selvars-exparcs)
	(dolist (ea (cdr sv-eas))
	  (unless (assoc ea exparcs-selvars)
	    (push (cons ea (cdr (assoc (ext-exp-open-arc-node ea) nodes-selvars)))
		  exparcs-selvars))))
      (let ((checked nil))
	(declare (special checked))
	(do ((vars selvars (cdr vars)))
	    ((or (null vars) cycle))
	  (unless (member (car vars) checked)
	    (setq cycle
		  (ms04-check-for-cycle-from (car vars) selvars-exparcs exparcs-selvars))))
	(not cycle)))))

(defun ms04-check-for-cycle-from (sv selvars-exparcs exparcs-selvars &optional prevs)
  (declare (special checked))
  (if (member sv prevs)
      T ; cycle detected
    (let ((cycle nil))
      (push sv checked)
      (do ((eas (cdr (assoc sv selvars-exparcs))
		(cdr eas)))
	  ((or (null eas) cycle))
	(do ((svl (cdr (assoc (car eas) exparcs-selvars)) (cdr svl)))
	    ((or (null svl) cycle))
	  (setq cycle
		(ms04-check-for-cycle-from (car svl) selvars-exparcs exparcs-selvars
					   (cons sv prevs)))))
      cycle)))

; after this,
; nodes-selvars is assoc list of (<node> <selvars of sels dom by node>)
; selvars-exparcs is assoc list of (<selvar> <exp arcs selvar occurs in>)
; expvars-exparcs is assoc list of (<expvar> <exp arcs expvar occurs in>)
; returns: selvars selected by node below eeod (or by eeod)
(defun set-ms04-banned-vars-1 (eeod)
  (declare (special nodes-selvars selvars-exparcs expvars-exparcs selvars))
  (let ((a (assoc eeod nodes-selvars)))
    (if a
	(cdr a)
      (let ((selvars1 nil))
	(dolist (arc (ext-exp-open-dag-arcs eeod))
	  (let ((selvars0 (set-ms04-banned-vars-1 (ext-exp-open-arc-node arc))))
	    (setq selvars1 (union selvars0 selvars1))
	    (when (ext-exp-open-arc-sel-var arc)
	      (let ((sv (ext-exp-open-arc-sel-var arc)))
		(push sv selvars1)))
	    (when (ext-exp-open-arc-exp-term arc)
	      (dolist (fv (free-vars-of (ext-exp-open-arc-exp-term arc)))
		(when (member fv selvars)
		  (let ((z (assoc fv selvars-exparcs)))
		    (if z
			(push arc (cdr z))
		      (push (list fv arc) selvars-exparcs))))
		(when (ext-exp-var-p fv)
		  (let ((z (assoc fv expvars-exparcs)))
		    (if z
			(push arc (cdr z))
		      (push (list fv arc) expvars-exparcs))))))))
	(push (cons eeod selvars1) nodes-selvars)
	selvars1))))

; we copy every node and ignore parents.
; return two values:
; the resulting root node (so calling with flex is OK)
; node-assoc giving assoc list for all nodes
(defun ms04-eeod-subst-deepen (theta eeod)
  (let ((node-assoc nil))
    (declare (special node-assoc))
    (let ((n (ms04-eeod-subst-deepen-1 theta eeod)))
      (values n node-assoc))))

; ignore parents
(defun ms04-eeod-subst-deepen-1 (theta eeod)
  (declare (special node-assoc))
  (let ((a (assoc eeod node-assoc)))
    (if a
	(cdr a)
      (let* ((node2 nil)
	     (k (ext-exp-open-dag-kind eeod))
	     (pos (ext-exp-open-dag-positive eeod))
	     (sh (ext-exp-open-dag-shallow eeod))
	     (sh2 (simul-substitute-l-term-var theta sh))
	     (nsh2 (lazy-abbrev-normalize sh2)))
	(if (and (eq k 'FLEX) (neq (head sh) (head sh2)))
	    (let ((simple-edag t))
	      (declare (special simple-edag))
	      (setq node2 (create-ext-exp-open-dag-2 nsh2 pos nil)))
	  (let ((arcs (ext-exp-open-dag-arcs eeod))
		(arcs2 nil))
	    (setq node2 (copy-ext-exp-open-dag eeod))
	    (setf (ext-exp-open-dag-shallow node2) nsh2)
	    (dolist (arc (reverse arcs))
	      (let ((arc2 (copy-ext-exp-open-arc arc)))
		(when (ext-exp-open-arc-exp-term arc)
		  (setf (ext-exp-open-arc-exp-term arc2)
			(etanorm
			 (lambda-norm
			  (simul-substitute-l-term-var theta (ext-exp-open-arc-exp-term arc))))))
		(setf (ext-exp-open-arc-node arc2)
		      (ms04-eeod-subst-deepen-1 theta (ext-exp-open-arc-node arc2)))
		(push arc2 arcs2)))
	    (setf (ext-exp-open-dag-arcs node2) arcs2)))
	(when (eq k 'EQN)
	  (unless (or pos (find-if #'(lambda (arc)
				       (eq (ext-exp-open-arc-kind arc) 'EQNDEC))
				   (ext-exp-open-dag-arcs eeod)))
	    (push-simple-eqndec-arc nsh2 node2)))
	(push (cons eeod node2) node-assoc)
	node2))))

    
; r = 2^i(2j+1), return (values i j)
(defun extract-coded-integer-pair (r)
  (if (<= r 0)
      (values 0 0)
    (let ((i 0))
      (loop while (evenp r) do
	    (setq r (ash r -1))
	    (incf i))
      (values i (ash (- r 1) -1)))))

; r = 2^i(2j+1), return r
(defun encode-integer-pair (i j)
  (* (ash 1 i)
     (1+ (ash j 1))))

; k = length ind-types
; the first k types are 
(defun ms04-nth-primsub-type (n k ind-types)
  (if (< n k)
      (nth n ind-types)
    (let ((r (- n k)))
      (if (< r k)
	  (cons 'O (nth r ind-types))
	(progn
	  (setq r (1+ (- r k)))
	  ; at this point, assume r encodes a pair of numbers <i,j> by 2^i(2j+1)
	  (multiple-value-bind
	      (i j)
	      (extract-coded-integer-pair r)
	    (cons (ms04-nth-primsub-type i k ind-types) ; the return types grow more slowly than dom types
		  (ms04-nth-primsub-type j k ind-types)) ; i.e., j gets big faster than i
	    ))))))

(defun ms04-quant-type-code (tp k ind-types)
  (if (consp tp)
      (if (eq (car tp) 'O)
	  (if (consp (cdr tp))
	      nil ; we don't code these types
	    (let ((n (position (cdr tp) ind-types)))
	      (+ k n)))
	(let* ((i (ms04-quant-type-code (car tp) k ind-types))
	       (j (ms04-quant-type-code (cdr tp) k ind-types))
	       (r (encode-integer-pair i j)))
	  (+ (ash k 1) r)))
    (position tp ind-types)))

(defun ms04-next-primsub-type (ind-types used-types)
  (if ind-types
      (let ((k (length ind-types)))
	(do* ((n 0 (1+ n))
	      (qtp (ms04-nth-primsub-type n k ind-types)
		   (ms04-nth-primsub-type n k ind-types)))
	    ((not (member qtp used-types :test #'equal))
	     qtp)))
    nil))

; allows KTRUTH and KFALSEHOOD to be used for setvars
(defun ms04-eeod-solve-flex-flex (&optional (node *current-edag*) (prop 'TRUTH))
  (let ((const-assoc (acons 'O prop nil))
	(expvars (eeod-exp-vars node))
	(theta nil))
    (dolist (ev expvars)
      (do ((tp (unabbreviated-type ev) (car tp)))
	  ((not (consp tp))
	   (let* ((a (assoc tp const-assoc :test #'equal))
		  (c (if a (cdr a)
		       (let ((c2 (fresh-var tp '|w|)))
			 (push (cons tp c2) const-assoc)
			 c2))))
	     (push (cons ev (create-imit-subst ev c)) theta)))))
    (ms04-eeod-subst-deepen theta node)))

(defun print-ms04-option (op)
  (case (car op)
    (DUP
     (let ((exp (nth 2 op)))
       (if (eq ms04-verbose 'MAX)
	   (msg "Duplicate " (ext-exp-open-dag-name exp) " : " ((ext-exp-open-dag-shallow exp) . gwff))
	 (msg "Duplicate " (ext-exp-open-dag-name exp)))))
    (PRIMSUB-QUANT-GENTP
     (let ((qinfo (nth 2 op)))
       (msg "Generate Next Type for " (car qinfo) " for " ((cadr qinfo) . gwff))))
    (PRIMSUB
     (let ((setvar (nth 2 op))
	   (theta (nth 3 op))
	   (pkind (nth 5 op)))
       (if (eq ms04-verbose 'MAX)
	   (msg "Primsub " pkind " for " (setvar . gwff) "  " ((cdar theta) . gwff))
	 (msg "Primsub " pkind " for " (setvar . gwff)))))
    (SUBST
     (let ((theta (nth 3 op)))
       (when theta
	 (msg "Subst ")
	 (if (cdr theta)
	     (dolist (xt theta) (msg "[" ((car xt) . gwff) " -> " ((cdr xt) . gwff) "] "))
	   (msg ((caar theta) . gwff) " -> " ((cdar theta) . gwff))))))
    ((MATE MATE-FLEXRIGID MATE-FLEXRIGID-NEG EUNIF1 EUNIF2 MATE-FLEXRIGID-PROJ MATE-FLEXRIGID-NEG-PROJ FLEX-EUNIF)
     (let* ((node1 (nth 2 op))
	    (node2 (nth 3 op)))
       (if (eq ms04-verbose 'MAX)
	   (msg (car op) " " (ext-exp-open-dag-name node1) " " ((ext-exp-open-dag-shallow node1) . gwff) 
		" to " (ext-exp-open-dag-name node2) " " ((ext-exp-open-dag-shallow node2) . gwff) t)
	 (msg (car op) " " (ext-exp-open-dag-name node1) " to " (ext-exp-open-dag-name node2) t))))
    (DELAY-UNIF
     (let* ((eqngoal (caddr op)))
       (msg "Add " ((ext-exp-open-dag-shallow eqngoal) . gwff) " to unification constraints.")))
    (DELAY-CONN
     (let* ((flex (caddr op))
	    (rig (nth 3 op))
	    (rwff (ext-exp-open-dag-shallow rig))
	    (wff2 (if (equal (ext-exp-open-dag-positive flex) (ext-exp-open-dag-positive rig))
		      (cons 'NOT rwff)
		    rwff)))
       (msg "Connect " flex " to " rig " and" t
	    "Add " ((ext-exp-open-dag-shallow flex) . gwff) " = " (wff2 . gwff)
	    " to unification constraints.")))
    (ADD-SET-CONSTRAINT
     (let ((maxmin (caddr op))
	   (setvar (nth 3 op))
	   (constr (nth 4 op)))
       (msg "Add " maxmin " Set Constraint for " (setvar . gwff) " ")
       (ms04-print-constraint maxmin constr)))
    (SOLVE-SET-CONSTRAINTS
     (msg "Solve Set Constraints for " ((caddr op). gwff)))
    (t (msg (car op)))))

(defun print-ms04-weight-formula (wf)
  (let ((flagshown nil)
	(fst t))
    (dolist (zf wf)
      (let* ((flag (if (consp zf) (cdr zf) zf))
	     (z (lift-eed-to-saturation-flag-coeff flag wf)))
	(unless (or (member flag flagshown) (= z 0))
	  (push flag flagshown)
	  (if fst
	      (setq fst nil)
	    (msg " + "))
	  (if (= z 1)
	      (msg flag)
	    (msg z "*" flag)))))))

; the lower the weight, the more likely unification will be successful.
; probably should use a hash table to prevent recomputation.
(defun ms04-quick-eunification-weight (dpairs eqns banned)
  (runcount 'unification)
  (let ((changed t)
	(steps 0))
    (loop while (and changed (< steps MS03-QUICK-EUNIFICATION-LIMIT)) do
	  (setq changed nil)
	  (let ((same nil)
		(decs nil)
		(funcs nil)
		(flexflexs nil)
		(flexrigids nil)
		(rigidflexs nil))
	    (dolist (d dpairs)
	      (if (wffeq-ab (cadr d) (caddr d))
		  (push d same)
		(if (consp (car d))
		    (push d funcs)
		  (unless (eq (car d) 'O)
		    (let ((h1 (lhead (cadr d)))
			  (h2 (lhead (caddr d))))
		      (if (ext-exp-var-p h1)
			  (if (ext-exp-var-p h2)
			      (push d flexflexs)
			    (push d flexrigids))
			(if (ext-exp-var-p h2)
			    (push d rigidflexs)
			  (when (eq h1 h2) ; same, decompose
			    (push d decs)))))))))
	    (when same
	      (setq dpairs (set-difference dpairs same))
	      (setq changed t))
	    (when funcs
	      (dolist (d funcs)
		(let* ((tp (car d))
		       (w (fresh-var (cdr tp) '|w|)))
		  (setq dpairs
			(cons (list (car tp)
				    (lambda-norm (cons (cadr d) w))
				    (lambda-norm (cons (caddr d) w))
				    (cons w (cadddr d)))
			      (remove d dpairs)))))
	      (setq changed t))
	    (when decs
	      (dolist (d decs)
		(let ((bvars (cadddr d)))
		  (setq dpairs
			(append (mapcar #'(lambda (x y)
					    (list (unabbreviated-type x) x y bvars))
					(args (cadr d))
					(args (caddr d)))
				(remove d dpairs)))))
	      (setq changed t))
	    (unless changed
	      (do ((fs flexrigids (cdr fs)))
		  ((or changed (null fs)))
		(let* ((flex (cadar fs))
		       (rig (caddar fs))
		       (fh (lhead flex))
		       (rh (lhead rig))
		       (bound (cadddr (car fs))))
		  (if (and (not (member rh bound)) (not (member rh (cdr (assoc fh banned)))))
		      (unless (or (ext-possible-projections (unabbreviated-type fh))
				  (ext-rigid-occurs-check-p fh rig))
			(multiple-value-bind
			    (wff evars)
			    (create-imit-subst fh rh)
			  (setq banned (ms04-update-banned fh evars banned))
;			  (push (list 'IMIT fh rh evars) insts)
			  (setq dpairs
				(mapcar #'(lambda (d)
					    (list (car d)
						  (lambda-norm (substitute-l-term-var wff fh (cadr d)))
						  (lambda-norm (substitute-l-term-var wff fh (caddr d)))
						  (cadddr d)))
					dpairs)))
			(incf steps)
			(setq changed t))
		    (let ((projs (ext-possible-projections (unabbreviated-type fh))))
		      (when (= (length projs) 1)
			(multiple-value-bind
			    (wff evars)
			    (create-proj-subst fh (car projs))
			  (setq banned (ms04-update-banned fh evars banned))
;			  (push (list 'PROJ fh (car projs) evars) insts)
			  (setq dpairs
				(mapcar #'(lambda (d)
					    (list (car d)
						  (lambda-norm (substitute-l-term-var wff fh (cadr d)))
						  (lambda-norm (substitute-l-term-var wff fh (caddr d)))
						  (cadddr d)))
					dpairs)))
			(incf steps)
			(setq changed t))))))
	      (do ((fs rigidflexs (cdr fs)))
		  ((or changed (null fs)))
		(let* ((flex (caddar fs))
		       (rig (cadar fs))
		       (fh (lhead flex))
		       (rh (lhead rig))
		       (bound (cadddr (car fs))))
		  (if (and (not (member rh (cdr (assoc fh banned))))
			   (not (member rh bound)))
		      (unless (or (ext-possible-projections (unabbreviated-type fh))
				  (ext-rigid-occurs-check-p fh rig))
			(multiple-value-bind
			    (wff evars)
			    (create-imit-subst fh rh)
			  (setq banned (ms04-update-banned fh evars banned))
;			  (push (list 'IMIT fh rh evars) insts)
			  (setq dpairs
				(mapcar #'(lambda (d)
					    (list (car d)
						  (lambda-norm (substitute-l-term-var wff fh (cadr d)))
						  (lambda-norm (substitute-l-term-var wff fh (caddr d)))
						  (cadddr d)))
					dpairs)))
			(incf steps)
			(setq changed t))
		    (let ((projs (ext-possible-projections (unabbreviated-type fh))))
		      (when (= (length projs) 1)
			(multiple-value-bind
			    (wff evars)
			    (create-proj-subst fh (car projs))
			  (setq banned (ms04-update-banned fh evars banned))
;			  (push (list 'PROJ fh (car projs) evars) insts)
			  (setq dpairs
				(mapcar #'(lambda (d)
					    (list (car d)
						  (lambda-norm (substitute-l-term-var wff fh (cadr d)))
						  (lambda-norm (substitute-l-term-var wff fh (caddr d)))
						  (cadddr d)))
					dpairs)))
			(incf steps)
			(setq changed t))))))
	      (do ((fs flexflexs (cdr fs)))
		  ((or changed (null fs)))
		(let* ((f1 (cadar fs))
		       (f2 (caddar fs))
		       (h1 (lhead f1))
		       (h2 (lhead f2))
		       (bound (cadddr (car fs)))
		       (h1bann (cdr (assoc h1 banned)))
		       (h2bann (cdr (assoc h2 banned)))
		       (bound1 (append bound h1bann))
		       (bound2 (append bound h2bann)))
		  (when (and (eeod-pattern-p-4 f1 bound1)
			     (eeod-pattern-p-4 f2 bound2))
		    (if (eq h1 h2)
			(multiple-value-bind
			    (wff ev)
			    (create-pattern-flexflex-same h1 (args (etanorm f1)) (args (etanorm f2)))
			  (setq banned (ms04-update-banned h1 (list ev) banned))
;			  (push (list 'PATTERN-FLEXFLEX-SAME h1 wff ev) insts)
			  (setq dpairs
				(mapcar #'(lambda (d)
					    (list (car d)
						  (lambda-norm (substitute-l-term-var wff h1 (cadr d)))
						  (lambda-norm (substitute-l-term-var wff h1 (caddr d)))
						  (cadddr d)))
					(remove (car fs) dpairs)))
			  (incf steps)
			  (setq changed t))
		      (multiple-value-bind
			  (wff1 wff2 ev)
			  (create-pattern-flexflex-diff h1 h2 (args (etanorm f1)) (args (etanorm f2)))
;			(push (list 'PATTERN-FLEXFLEX-DIFF h1 h2 wff1 wff2 ev) insts)
			(let ((theta (acons h1 wff1 (acons h2 wff2 nil)))
			      (h1bann (cdr (assoc h1 banned)))
			      (h2bann (cdr (assoc h2 banned))))
			  (push (cons ev (append h1bann h2bann)) banned)
			  (setq dpairs
				(mapcar #'(lambda (d)
					    (list (car d)
						  (lambda-norm (simul-substitute-l-term-var theta (cadr d)))
						  (lambda-norm (simul-substitute-l-term-var theta (caddr d)))
						  (cadddr d)))
					(remove (car fs) dpairs)))
			  (incf steps)
			  (setq changed t))))))))))
    (breakcount 'unification)
    (values
     (mapcar #'(lambda (d)
		 (ms04-quick-eunification-weight-1 (car d) (cadr d) (caddr d) (cadddr d) eqns banned))
	     dpairs)
     dpairs)))

; return a list of subst's that solve the problem up to depth MS04-SOLVE-UNIF-DEPTH
; or a single solution if we only want one
; each dpair is (<tp> <wff1> <wff2> <boundvars>)
; each subst is (<evar> . <wff>)
(defun ms04-unify (dpairs subst banned depth-bound &optional stop-on-success)
  (runcount 'unification)
  (multiple-value-bind
      (res substs depth)
      (ms04-unify-1 0 dpairs subst banned depth-bound stop-on-success)
    (breakcount 'unification)
    (values res substs depth)))

(defun ms04-unify-1 (depth dpairs subst banned depth-bound stop-on-success)
  (declare (special *edag-lift-info*))
  (if (> depth depth-bound)
      (values 'MORE nil depth) ; stop looking down this path
    (multiple-value-bind
	(dpairs2 subst2 banned2)
	(ms04-search-decidable-unification dpairs subst banned)
      (if (eq dpairs2 'FAILED)
	  (values 'FAILED NIL depth)
	(let ((d (find-if-not #'(lambda (d)
				  (and (not (boundwff-p (cadr d)))
				       (ext-exp-var-p (lhead (cadr d)))
				       (not (boundwff-p (caddr d)))
				       (ext-exp-var-p (lhead (caddr d)))))
			      dpairs2)))
	  (if d
	      (let* ((substs nil) ; try each imitation and projection
		     (fstrig (or (boundwff-p (cadr d)) (not (ext-exp-var-p (lhead (cadr d))))))
		     (wff1 (if fstrig (caddr d) (cadr d))) ; flex
		     (wff2 (if fstrig (cadr d) (caddr d))) ; rigid
		     (h1 (lhead wff1))
		     (bound (cadddr d))
		     (res 'FAILED)
		     (depth9 0))
		(if (boundwff-p wff2) ; imitation works a little different here
		    (multiple-value-bind (wff evars)
			(create-quant-subst h1 (unabbreviated-type (bdvar wff2)) (binder wff2))
		      (let ((banned3 (ms04-update-banned h1 evars banned2)))
			(multiple-value-bind (dpairs3 subst3)
			    (ms04-search-unification-subst dpairs2 subst2 (acons h1 wff nil))
			  (multiple-value-bind (res0 substs0 depth0)
			      (ms04-unify-1 (1+ depth)
					    dpairs3 subst3 banned3 depth-bound stop-on-success)
			    (setq res res0)
			    (setq substs (append substs substs0))
			    (setq depth0 (max depth9 depth0))))))
		  (let ((h2 (lhead wff2)))
		    (unless (or (member h2 (cdr (assoc h1 banned2)))
				(member h2 bound))
		      (multiple-value-bind (wff evars)
			  (create-imit-subst h1 h2)
			(let ((banned3 (ms04-update-banned h1 evars banned2)))
			  (multiple-value-bind (dpairs3 subst3)
			      (ms04-search-unification-subst dpairs2 subst2 (acons h1 wff nil))
			    (multiple-value-bind (res0 substs0 depth0)
				(ms04-unify-1 (1+ depth)
					      dpairs3 subst3 banned3 depth-bound stop-on-success)
			      (setq res res0)
			      (setq substs (append substs substs0))
			      (setq depth0 (max depth9 depth0)))))))))
		(do ((projs (ext-possible-projections (unabbreviated-type h1)) (cdr projs))) ; now try projections
		    ((or (null projs) (member res stop-on-success))
		     (values res substs depth9))
		  (let ((i (car projs)))
		    (multiple-value-bind (wff evars)
			(create-proj-subst h1 i)
		      (let ((banned3 (ms04-update-banned h1 evars banned2)))
			(multiple-value-bind (dpairs3 subst3)
			    (ms04-search-unification-subst dpairs2 subst2 (acons h1 wff nil))
			  (multiple-value-bind (res0 substs0 depth0)
			      (ms04-unify-1 (1+ depth)
					    dpairs3 subst3 banned3 depth-bound stop-on-success)
			    (setq res (ms04-unify-combine-res res res0))
			    (setq substs (append substs substs0))
			    (setq depth0 (max depth9 depth0)))))))))
	    (values 'SUCCESS (list subst2) depth))))))) ; successful preunifier

(defun ms04-unify-combine-res (res1 res2)
  (if (or (eq res1 'SUCCESS) (eq res2 'SUCCESS))
      'SUCCESS
    (if (or (eq res1 'MORE) (eq res2 'MORE))
	'MORE
      'FAILED)))

; do the decidable part of unification (patterns) returning values <dpairs> <subst> <banned-reln> or 'FAILED
; each dpair is (<tp> <wff1> <wff2> <boundvars>)
; each subst is (<evar> . <wff>)
(defun ms04-search-decidable-unification (dpairs subst banned)
  (runcount 'unification)
  (let ((changed t)
	(failed nil))
    (loop while (and changed (not failed)) do
	  (setq changed nil)
	  (setq dpairs (ms04-search-simplification dpairs)) ; simplify
	  (when (eq dpairs 'FAILED)
	    (setq failed T))
	  (unless failed
	    (let ((pattpair ; find a pattern pair (if there are any, otherwise we're done)
		   (find-if #'(lambda (d)
				(and
				 (is-a-pattern (cadr d)
					       (ext-exp-vars-of (cadr d))
					       (cadddr d))
				 (is-a-pattern (caddr d)
					       (ext-exp-vars-of (caddr d))
					       (cadddr d))))
			    dpairs)))
	      (when pattpair
		(setq changed t)
		(let* ((fstrig (or (boundwff-p (cadr pattpair)) (not (ext-exp-var-p (lhead (cadr pattpair))))))
		       (wff1 (if fstrig (caddr pattpair) (cadr pattpair)))
		       (wff2 (if fstrig (cadr pattpair) (caddr pattpair))))
		  (if (or fstrig (boundwff-p wff2) (not (ext-exp-var-p (lhead wff2))))
		      (let ((h1 (lhead wff1))
			    (bound (cadddr pattpair)))
			(if (free-in h1 wff2) ; occurs check
			    (setq failed T)
			  (if (setdiff (intersection (free-vars-of wff2) bound) (free-vars-of wff1)) ; need no extra bdvars around
			      (setq failed T)
			    (let ((wff3 wff2))
			      (do ((wff1a wff1 (car wff1a)))
				  ((not (consp wff1a)))
				(let ((b (etanorm (cdr wff1a))))
				  (setq wff3 (acons b 'LAMBDA wff3))))
			      (setq banned (ms04-update-banned h1 (ext-exp-vars-of wff3) banned))
			      (multiple-value-setq
				  (dpairs subst)
				(ms04-search-unification-subst
				 dpairs subst (acons h1 wff3 nil)))))))
		    (let ((h1 (lhead wff1)) ; flex flex cases
			  (h2 (lhead wff2)))
		      (if (eq h1 h2) 
			  (multiple-value-bind (wff3 ev) ; flex flex same
			      (ms04-create-pattern-flexflex-same h1 (args (etanorm wff1)) (args (etanorm wff2)))
			    (setq banned (ms04-update-banned h1 (list ev) banned))
			    (multiple-value-setq
				(dpairs subst)
			      (ms04-search-unification-subst
			       dpairs subst (acons h1 wff3 nil))))
			(multiple-value-bind (wff3 wff4 ev) ; flex flex diff
			    (ms04-create-pattern-flexflex-diff h1 h2 (args (etanorm wff1)) (args (etanorm wff2)))
			  (let ((h1bann (cdr (assoc h1 banned)))
				(h2bann (cdr (assoc h2 banned))))
			    (push (cons ev (append h1bann h2bann)) banned))
			  (multiple-value-setq
			      (dpairs subst)
			    (ms04-search-unification-subst
			     dpairs subst (acons h1 wff3 (acons h2 wff4 nil)))))))))))))
    (breakcount 'unification)
    (if failed
	'FAILED
      (values dpairs subst banned))))

(defun ms04-search-simplification (dpairs)
  (let ((failed nil)
	(dpairs3 nil))
    (loop while (and dpairs (not failed)) do
	  (let* ((d (pop dpairs))
		 (tp (car d))
		 (wff1 (cadr d))
		 (wff2 (caddr d))
		 (bvars (cadddr d)))
	    (unless (wffeq-ab wff1 wff2)
	      (if (consp tp)
		  (let ((w (fresh-var (cdr tp) '|w|)))
		    (push (list (car tp)
				(lambda-norm (cons wff1 w))
				(lambda-norm (cons wff2 w))
				(cons w bvars))
			  dpairs))
		(if (boundwff-p wff1)
		    (if (boundwff-p wff2)
			(if (and (eq (binder wff1) (binder wff2))
				 (equal (unabbreviated-type (bdvar wff1)) (unabbreviated-type (bdvar wff2))))
			    (let* ((bv (bdvar wff1))
				   (w (fresh-var (unabbreviated-type bv) '|w|)))
			      (push (list 'O
					  (lambda-norm (substitute-l-term-var w bv (cdr wff1)))
					  (lambda-norm (substitute-l-term-var w (bdvar wff2) (cdr wff2)))
					  (cons w bvars))
				    dpairs))
			  (setq failed T)) ; a conflict, can't syntactically unify			  
		      (if (ext-exp-var-p (lhead wff2))
			  (push d dpairs3)
			(setq failed T))) ; a conflict, can't syntactically unify
		  (if (boundwff-p wff2)
		      (if (ext-exp-var-p (lhead wff1))
			  (push d dpairs3)
			(setq failed T)) ; a conflict, can't syntactically unify
		    (let ((h1 (lhead (cadr d)))
			  (h2 (lhead (caddr d))))
		      (if (or (ext-exp-var-p h1) (ext-exp-var-p h2))
			  (push d dpairs3)
			(if (eq h1 h2) ; decompose 
			    (mapc #'(lambda (x y)
				      (push (list (unabbreviated-type x) x y bvars) dpairs))
				  (args wff1)
				  (args wff2))
			  (setq failed T)))))))))) ; rigid conflict
    (if failed 'FAILED dpairs3)))

(defun ms04-search-unification-subst (dpairs subst theta)
  (let ((newdpairs (mapcar #'(lambda (d)
				(cons (lnorm (simul-substitute-l-term-var theta (car d)))
				      (lnorm (simul-substitute-l-term-var theta (cdr d)))))
			    dpairs))
	(newsubst (mapcar #'(lambda (d)
			      (cons (car d)
				    (lnorm (simul-substitute-l-term-var theta (cdr d)))))
			  subst)))
    (values newdpairs (append newsubst theta))))
   
(defun ms04-update-banned (x evl banned)
  (let ((a (cdr (assoc x banned))))
    (when a
      (dolist (ev evl)
	(let ((b (assoc ev banned)))
	  (if b
	      (setq banned
		    (cons (cons ev (append a (cdr b)))
			  (remove b banned)))
	    (push (cons ev a) banned)))))
    banned))

(defun ms04-quick-eunification-weight-1 (tp lft rght bound eqns banned)
  (let ((h1 (lhead lft))
	(h2 (lhead rght)))
    (if (ext-exp-var-p h1)
	(if (ext-exp-var-p h2) ; flex flex
	    (if (eq tp 'O)
		(if (eq h1 h2)
		    'MS03-WEIGHT-FLEXFLEXSAME-O
		  'MS03-WEIGHT-FLEXFLEXDIFF-O)
	      (if (eq h1 h2)
		  'MS03-WEIGHT-FLEXFLEXSAME
		'MS03-WEIGHT-FLEXFLEXDIFF))
					; flex rigid
	  (if (eq tp 'O)
	      'MS03-WEIGHT-FLEXRIGID-O
	    (if (ext-possible-projections (unabbreviated-type h1))
		'MS03-WEIGHT-FLEXRIGID-BRANCH
	      (if (not (member h2 (cdr (assoc h1 banned))))
		  (if (ext-rigid-occurs-check-p h1 rght)
		      'MS03-WEIGHT-OCCURS-CHECK
		    'MS03-WEIGHT-FLEXRIGID-BRANCH)
		(if (or (member h2 (cdr (assoc h1 banned))) (member h2 bound))
		    'MS03-WEIGHT-BANNED-SELS
		  (if (find-if #'(lambda (x)
				   (and (eq tp (car x))
					(or (eq (lhead (cadr x)) h2)
					    (eq (lhead (caddr x)) h2))))
			       eqns)
		      'MS03-WEIGHT-FLEXRIGID-EQN
		    (if (find-if #'(lambda (x)
				     (and (eq tp (car x))
					  (or (ext-exp-var-p (lhead (cadr x)))
					      (ext-exp-var-p (lhead (cadr x))))))
				 eqns)
			'MS03-WEIGHT-FLEXRIGID-FLEXEQN
		      'MS03-WEIGHT-FLEXRIGID-NOEQN)))))))
      (if (ext-exp-var-p h2) ; rigid flex
	  (if (eq tp 'O)
	      'MS03-WEIGHT-FLEXRIGID-O
	    (if (ext-possible-projections (unabbreviated-type h2))
		'MS03-WEIGHT-FLEXRIGID-BRANCH
	      (if (not (member h1 (cdr (assoc h2 banned))))
		  (if (ext-rigid-occurs-check-p h2 lft)
		      'MS03-WEIGHT-OCCURS-CHECK
		    'MS03-WEIGHT-FLEXRIGID-BRANCH)
		(if (or (member h1 (cdr (assoc h2 banned))) (member h1 bound))
		    'MS03-WEIGHT-BANNED-SELS
		  (if (find-if #'(lambda (x)
				   (and (eq tp (car x))
					(or (eq (lhead (cadr x)) h1)
					    (eq (lhead (caddr x)) h1))))
			       eqns)
		      'MS03-WEIGHT-FLEXRIGID-EQN
		    (if (find-if #'(lambda (x)
				     (and (eq tp (car x))
					  (or (ext-exp-var-p (lhead (cadr x)))
					      (ext-exp-var-p (lhead (cadr x))))))
				 eqns)
			'MS03-WEIGHT-FLEXRIGID-FLEXEQN
		      'MS03-WEIGHT-FLEXRIGID-NOEQN))))))
					; rigid rigid
	(if (eq tp 'O)
	    (if (eq h1 h2)
		'MS03-WEIGHT-RIGIDRIGIDSAME-O
	      'MS03-WEIGHT-RIGIDRIGIDDIFF-O)
					; (different lheads - otherwise would have decomposed first)
					; deps on eqns
	  (if (find-if #'(lambda (x)
			   (and (eq tp (car x))
				(or (and (eq h1 (lhead (cadr x)))
					 (eq h2 (lhead (caddr x))))
				    (and (eq h2 (lhead (cadr x)))
					 (eq h1 (lhead (caddr x)))))))
		       eqns)
	      'MS03-WEIGHT-RIGIDRIGID-EQN
	    (if (find-if #'(lambda (x)
			     (and (eq tp (car x))
				  (or (ext-exp-var-p (lhead (cadr x)))
				      (ext-exp-var-p (lhead (caddr x))))))
			 eqns)
		'MS03-WEIGHT-RIGIDRIGID-FLEXEQN
	      'MS03-WEIGHT-RIGIDRIGID-NOEQN)))))))
 
(defun ms04-intermediate-dual-formula (curr)
  (ms04-intermediate-dual-formula-1 (ms04-search-edag curr) (ms04-search-dupd-exps curr)))

(defun ms04-intermediate-dual-formula-1 (edag expnames)
  (let ((k (ext-exp-open-dag-kind edag)))
    (case k
      (EXP
       (let ((wff nil))
	 (if (member (ext-exp-open-dag-name edag) expnames)
	     (progn
	       (dolist (kid (ext-exp-open-dag-kids edag))
		 (if wff
		     (setq wff (acons (if (ext-exp-open-dag-positive edag) 'AND 'OR)
				      wff (ms04-intermediate-dual-formula-1 kid expnames)))
		   (setq wff (ms04-intermediate-dual-formula-1 kid expnames))))
	       (if wff
		   wff
		 (if (ext-exp-open-dag-positive edag) 'TRUTH 'FALSEHOOD)))
	   (setq wff (ext-exp-open-dag-shallow edag))) ; not done duplicating, so return the quantified version
	 wff))
      ((SEL NEG REW)
       (let ((wff (ms04-intermediate-dual-formula-1 (car (ext-exp-open-dag-kids edag)) expnames)))
	 (if (eq k 'NEG)
	     (cons 'NOT wff)
	   wff)))
      ((DIS CON IMP)
       (let* ((kids (ext-exp-open-dag-kids edag)))
	 (acons (case k
		  (DIS 'OR)
		  (CON 'AND)
		  (IMP 'IMPLIES))
		(ms04-intermediate-dual-formula-1 (car kids) expnames)
		(ms04-intermediate-dual-formula-1 (cadr kids) expnames))))
      (t 
       (ext-exp-open-dag-shallow edag)))))
       
; assumes arguments are in eta-short form
(defun ms04-create-pattern-flexflex-same (ev args1 args2)
  (let ((bindvars nil)
	(rbindvars nil)
	(bindvars2 nil)
	(wff nil)
	(ev2 nil)
	(rtp nil))
    (do ((tp (unabbreviated-type ev) (car tp)))
	((not (consp tp)) (setq rtp tp))
      (push (fresh-var (cdr tp) '|w|) bindvars))
    (setq rbindvars (reverse bindvars))
    (dotimes (i (length args1))
      (when (eq (nth i args1) (nth i args2))
	(push (nth i rbindvars) bindvars2)))
    (setq bindvars2 (reverse bindvars2))
    (setq wff (new-applied-evar rtp bindvars2 '|h| nil))
    (setq ev2 (lhead wff))
    (dolist (bv bindvars)
      (setq wff (acons bv 'LAMBDA wff)))
    (values wff ev2)))

; assumes arguments are in eta-short form
(defun ms04-create-pattern-flexflex-diff (ev1 ev2 args1 args2)
  (let ((bindvars1 nil)
	(bindvars2 nil)
	(rbindvars1 nil)
	(rbindvars2 nil)
	(args3 nil)
	(args4 nil)
	(wff3 nil)
	(wff4 nil)
	(rtp nil))
    (do ((tp (unabbreviated-type ev1) (car tp)))
	((not (consp tp)) (setq rtp tp))
      (push (fresh-var (cdr tp) '|w|) bindvars1))
    (do ((tp (unabbreviated-type ev2) (car tp)))
	((not (consp tp)))
      (push (fresh-var (cdr tp) '|w|) bindvars2))
    (setq rbindvars1 (reverse bindvars1))
    (setq rbindvars2 (reverse bindvars2))
    (dotimes (i (length args1))
      (let ((k (position (nth i args1) args2)))
	(when k
	  (push (nth i rbindvars1) args3)
	  (push (nth k rbindvars2) args4))))
    (dolist (arg args3)
      (setq rtp (cons rtp (unabbreviated-type arg))))
    (let ((ev3 (fresh-var rtp '|h|)))
      (setq wff3 ev3)
      (setq wff4 ev3)
      (setf (get ev3 'ext-exp-var) t)
      (dolist (arg (reverse args3))
	(setq wff3 (cons wff3 arg)))
      (dolist (arg (reverse args4))
	(setq wff4 (cons wff4 arg)))
      (dolist (bv bindvars1)
	(setq wff3 (acons bv 'LAMBDA wff3)))
      (dolist (bv bindvars2)
	(setq wff4 (acons bv 'LAMBDA wff4)))
      (values wff3 wff4 ev3))))

(defun ext-exp-vars-in-dpairs (dpairs)
  (let ((evars nil))
    (dolist (d dpairs evars)
      (setq evars (union (ext-exp-vars-of (car d))
			 (ext-exp-vars-of (car d))
			 evars)))))

(defun lhead (wff)
  (or (loghead wff) (head wff)))

(defun ms04-solve-set-constraints (v maxmin constrs curr)
  (declare (special *current-edag* *current-edag-lemmas* *current-edag-lemma-ftree-pfs*))
  (let* ((substitutable-vars (eeod-exp-vars (ms04-search-edag curr)))
	 (misc-vars (append (eeod-sel-vars (ms04-search-edag curr)) substitutable-vars))
	 (vsel (fresh-var (unabbreviated-type v) (getnameroot v)))
	 (problem nil)
	 (arc-assoc nil)
	 (node-assoc nil)
	 (rec-flag nil)
	 (paths nil)
	 (misc-occurs nil)
	 (banned (cdr (assoc v (ms04-search-banned curr))))
	 (banned-occurs nil))
    (declare (special node-assoc))
    (let ((*current-edag* (ms04-eeod-subst-deepen-1 (acons v vsel nil) (ms04-search-edag curr))))
      (declare (special *current-edag*))
      (setf (ms04-search-edag curr) *current-edag*)
      (setq arc-assoc (eeod-var-exparc-assoc *current-edag* v vsel))
      (dolist (arcpair arc-assoc)
	(eeod-node-assoc-1 (ext-exp-open-arc-node (car arcpair))
			   (ext-exp-open-arc-node (cdr arcpair))))
      (dolist (constr constrs)
	(let* ((mainnode (cdr (assoc (car constr) node-assoc)))
	       (aux-nodes (mapcar #'(lambda (n)
				      (cdr (assoc n node-assoc)))
				  (cdr constr)))
	       (path nil)
	       (banned-occs nil))
	  (dolist (n (cons mainnode aux-nodes))
	    (when (ext-exp-open-dag-p n)
	      (setf (get (ext-exp-open-dag-name n) 'ext-exp-open-dag) n)))
	  (if (and (ext-exp-open-dag-p mainnode)
		   (not (find-if-not #'(lambda (x) (ext-exp-open-dag-p x)) aux-nodes))
		   (eq (head (ext-exp-open-dag-shallow mainnode)) vsel)
		   (not (find-if #'(lambda (x) (free-in v (ext-exp-open-dag-shallow x))) aux-nodes)))
	      (progn
		(dolist (n (cons mainnode aux-nodes))
		  (let* ((wff (ext-exp-open-dag-shallow n))
			 (free-vars (free-vars-of wff))
			 (f (make-ftree :name (ext-exp-open-dag-name n)
					:kind 'LEAF
					:positive (ext-exp-open-dag-positive n)
					:shallow wff)))
		    (push f path) ; fall back on using ftrees since that code is written
		    (unless (eq n mainnode)
		      (when (member vsel free-vars)
			(setq rec-flag t)))
		    (dolist (x free-vars)
		      (if (member x banned)
			  (setq banned-occs (adjoin x banned-occs))
			(if (member x misc-vars)
			    (setq misc-occurs (adjoin x misc-occurs)))))))
		(push banned-occs banned-occurs)
		(push (reverse path) paths))
	    (progn
	      (setq problem t)
	      (when (eq ms04-verbose 'MAX)
		(if (eq maxmin 'MAX)
		    (msgf "Problem solving upper bound constraint " t mainnode " -> " aux-nodes t)
		  (msgf "Problem solving lower bound constraint " t aux-nodes " -> " mainnode t)))))))
      (unless problem
	(catch 'fail ; to catch cases where there's trouble solving the constraints
	  (multiple-value-bind
	      (negf posf clist1)
	      (make-ftree-setvar-soln vsel maxmin paths
				      banned-occurs
				      misc-occurs
				      substitutable-vars
				      rec-flag)
	    (let ((subst nil)
		  (node-assoc nil)
		  (selvars nil))
	      (declare (special subst node-assoc selvars))
	      (ftree-clear-eeod-names negf)
	      (ftree-clear-eeod-names posf)
	      (let ((pose (ftree-to-edag-3 posf))
		    (lemmas (if rec-flag
				(list (intern (create-namestring maxmin))
				      (list (intern (create-namestring 'KNASTER-TARSKI-FPTHM))))
			      (list (intern (create-namestring maxmin))))))
		(dolist (conn clist1)
		  (catch 'fail
		    (eeod-generalized-connection (car conn) (cdr conn))))
		(let ((*current-edag-lemmas* (ms04-search-lemmas curr))
		      (*current-edag-lemma-ftree-pfs* (ms04-search-lemma-ftree-pfs curr)))
		  (declare (special *current-edag-lemmas* *current-edag-lemma-ftree-pfs*))
		  (ext-exp-open-dag-add-lemma lemmas pose negf clist1)
		  (setf (ms04-search-edag curr) *current-edag*)
		  (setf (ms04-search-lemmas curr) *current-edag-lemmas*)
		  (setf (ms04-search-lemma-ftree-pfs curr) *current-edag-lemma-ftree-pfs*)
		  )))))))))
  
  
; find log consts which occur in wff embedded in a term which will not be primsub'd for naturally
(defun ms04-find-extra-log-consts (wff)
  (cond ((or (and-p wff) (or-p wff) (implies-p wff) (equiv-p wff))
	 (append (ms04-find-extra-log-consts (cdar wff))
		 (ms04-find-extra-log-consts (cdr wff))))
	((or (not-p wff) (ae-bd-wff-p wff))
	 (ms04-find-extra-log-consts (cdr wff)))
	((anyabbrev-p (head wff))
	 (ms04-find-extra-log-consts (instantiate-1 wff)))
	(t
	 (let ((ret nil))
	   (do ((z wff (car z)))
	       ((not (consp z))
		ret)
	     (setq ret (append (ms04-find-extra-log-consts-1 (cdr z))
			       ret)))))))

(defun ms04-find-extra-log-consts-1 (wff)
  (cond ((member wff '(TRUTH FALSEHOOD IMPLIES EQUIV AND))
	 (list wff))
	((equality-p wff)
	 (let ((tp (unabbreviated-type wff)))
	   (if (and (consp tp)
		    (or (consp (cdr tp))
			(eq (cdr tp) 'O)))
	       (list wff)
	     nil)))
	((e-bd-wff-p wff)
	 (let ((tp (unabbreviated-type (bindvar wff))))
	   (list (cons 'EXISTS tp))))
	((a-bd-wff-p wff)
	 (let ((tp (unabbreviated-type (bindvar wff))))
	   (if (ms04-generated-type-p tp)
	       nil
	     (list (cons 'FORALL tp)))))
	((boundwff-p wff)
	 (ms04-find-extra-log-consts-1 (cdr wff)))
	((anyabbrev-p wff)
	 (ms04-find-extra-log-consts-1 (instantiate-1 wff)))
	((consp wff)
	 (append (ms04-find-extra-log-consts-1 (car wff))
		 (ms04-find-extra-log-consts-1 (cdr wff))))
	(t
	 nil)))

(defun ms04-generated-type-p (tp)
  (if (consp tp)
      (if (and (eq (car tp) 'O)
	       (atom (cdr tp))
	       (not (eq (cdr tp) 'O)))
	  t
	(and (ms04-generated-type-p (car tp))
	     (ms04-generated-type-p (cdr tp))))
    (if (eq tp 'O)
	nil
      t)))
  
