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
;;; File: EXT-SEARCH  - cebrown - 4/03

(deffile ext-search
    (part-of EXT-DAGS)
  (extension clisp)
  (mhelp "File dealing with search using extensional expansion DAG's."))

(context ext-search)

(defflag ext-search-limit 
  (flagtype integer+-or-infinity)
  (subjects ext-search ms03-7 transmit)
  (default infinity)
  (mhelp "If EXT-SEARCH-LIMIT is an integer which will place a limit on the extensional search
procedure MS03-7.  Given such a limit, search is incomplete and guaranteed to eventually terminate.
If EXT-SEARCH-LIMIT is set to infinity, then the search may not terminate."))

(definfo ms03-7
  (mhelp "A setting for DEFAULT-MS, DEFAULT-MATE and DEFAULT-EXPAND.
This uses the MS03-7 mating search procedure which incorporates 
extensionality reasoning, equality reasoning, and set variable reasoning 
as described in Chad E. Brown's thesis.

The search procedures MS03-7 and MS04-2 are similar in that they are
both extensional search procedures.  MS03-7 does a saturation style
search (with no backtracking).

MS04-2 is proven complete in Chad E. Brown's thesis.  MS03-7 is
probably complete, but this has not been proven.

See Also: MS04-2."))

(defflag ms03-verbose
  (default nil)
  (flagtype boolean)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7))))
;  (irrelevancy-preconditions ; actually, ms03-verbose may cause effects if we're lifting in the EXT-MATE level
;   (default-ms (not (memq default-ms '(ms03-7)))))
  (mhelp "If T, print extra information during MS03-7 search."))

(defflag MS03-USE-SET-CONSTRAINTS
  (flagtype boolean)
  (subjects ext-search ms03-7 primsubs transmit)
  (relevancy-preconditions
   (default-ms (and (memq default-ms '(ms03-7)) ms03-use-jforms))
   (ms03-use-jforms (and (memq default-ms '(ms03-7)) ms03-use-jforms)))
  (irrelevancy-preconditions
   (default-ms (or (not (memq default-ms '(ms03-7))) (not ms03-use-jforms)))
   (ms03-use-jforms (or (not (memq default-ms '(ms03-7))) (not ms03-use-jforms))))
  (default nil)
  (mhelp "If this flag and MS03-USE-JFORMS are T,
MS03-7 uses set constraints in addition to primsubs 
to determine potential set substitutions."))

(defflag MS03-USE-JFORMS
  (flagtype boolean)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7))))
  (irrelevancy-preconditions
   (default-ms (neq default-ms '(ms03-7))))
  (default t)
  (mhelp "If T, MS03-7 uses (dissolved) jforms during search.
Constructing and dissolving jforms can be time consuming, but in
principle can restrict the branching of search.  If NIL, jforms
are not used, which may result in the consideration of connections
which only span paths already spanned by other connections."))

(defflag MS03-WEIGHT-DISJ-MATE
  (flagtype integer+)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7)))))
  (default 10)
  (mhelp "When attempting to mate two literals a and b, this weight is
multiplied by disjdepth(a) * disjdepth(b) where disjdepth of a literal
is the number of disjunctions above the literal on the jform.
The effect of this is to prefer mating nodes that are closer to being 'global'.

If MS03-USE-JFORMS is set to NIL, the disjdepth of a node is
measured by the number of disjunctive nodes above the node in the edag.
This measure is less precise, since dissolution isn't used.

See Also: MS03-WEIGHT-DISJ-EUNIF, MS03-WEIGHT-DISJ-UNIF"))

(defflag MS03-WEIGHT-DISJ-EUNIF
  (flagtype integer+)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7)))))
  (default 10)
  (mhelp "When attempting to E-unify two literals a and b, this weight is
multiplied by disjdepth(a) * disjdepth(b) where disjdepth of a literal
is the number of disjunctions above the literal on the jform.
The effect of this is to prefer mating nodes that are closer to being 'global'.

If MS03-USE-JFORMS is set to NIL, the disjdepth of a node is
measured by the number of disjunctive nodes above the node in the edag.
This measure is less precise, since dissolution isn't used.

See Also: MS03-WEIGHT-DISJ-MATE, MS03-WEIGHT-DISJ-UNIF"))

(defflag MS03-WEIGHT-DISJ-UNIF
  (flagtype integer+)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 )))))
  (default 10)
  (mhelp "When performing a unification (imitation or projection) step
on a negative equation literal, this value is multiplied by the
disjdepth of the literal.  The disjdepth is the number of disjunctions
above the literal in the jform.

If MS03-USE-JFORMS is set to NIL, the disjdepth of the negative equation
node is measured by the number of disjunctive nodes above the node in the edag.

See Also: MS03-WEIGHT-DISJ-MATE, MS03-WEIGHT-DISJ-EUNIF"))

(defflag MS03-WEIGHT-RIGID-MATE
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 1)
  (mhelp "This value is added to the weight for adding any connection between
two rigid literals."))

(defflag MS03-WEIGHT-FLEXRIGID-MATE
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 1)
  (mhelp "This value is added to the weight for adding any connection between
any rigid literal and flexible literal."))

(defflag MS03-WEIGHT-EUNIF1
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 1)
  (mhelp "This value is added to the weight for adding any eunif1
(E-unification without symmetry) between two equation literals."))

(defflag MS03-WEIGHT-EUNIF2
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 1)
  (mhelp "This value is added to the weight for adding any eunif2
(E-unification with symmetry) between two equation literals."))

(defflag MS03-WEIGHT-IMITATE
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 1)
  (mhelp "This value is added to the weight for any imitation unification steps."))
  
(defflag MS03-WEIGHT-PROJECT
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 1)
  (mhelp "This value is added to the weight for any projection unification steps."))
  

(defflag MS03-WEIGHT-FLEXFLEXSAME-O
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 20)
  (mhelp "Controls the penalty for trying to unify two terms that require
unifying two flexible terms of type O with the same head."))

(defflag MS03-WEIGHT-FLEXFLEXDIFF-O
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 10)
  (mhelp "Controls the penalty for trying to unify two terms that require
unifying two flexible terms of type O with different heads."))

(defflag MS03-WEIGHT-FLEXFLEXSAME
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 5)
  (mhelp "Controls the penalty for trying to unify two terms that require
unifying two flexible terms of a base type other than O with the same head."))

(defflag MS03-WEIGHT-FLEXFLEXDIFF
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 3)
  (mhelp "Controls the penalty for trying to unify two terms that require
unifying two flexible terms of a base type other than O with different heads."))

(defflag MS03-WEIGHT-FLEXRIGID-BRANCH
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 6)
  (mhelp "Controls the penalty for trying to unify two terms that require
solving a branching (higher-order) flex-rigid disagreement pair of a
base type other than O."))

(defflag MS03-WEIGHT-FLEXRIGID-O
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 20)
  (mhelp "Controls the penalty for trying to unify two terms that require
solving a branching (higher-order) flex-rigid disagreement pair of
type O."))

(defflag MS03-WEIGHT-RIGIDRIGIDSAME-O
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 15)
  (mhelp "Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of type O with the same head.
Extensionality is required to solve these cases."))

(defflag MS03-WEIGHT-RIGIDRIGIDDIFF-O
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 40)
  (mhelp "Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of type O with the different heads.
Extensionality is required to solve these cases."))

(defflag MS03-WEIGHT-RIGIDRIGID-EQN
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 50)
  (mhelp "Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of a base type other than O in the presence
of an equation which is between a pair of rigid terms sharing a head
with the disagreement pair.  Some form of equational reasoning is
required to solve these cases."))

(defflag MS03-WEIGHT-RIGIDRIGID-FLEXEQN
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 60)
  (mhelp "Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of a base type other than O in the presence
of an equation which is between a rigid and a flexible term.  Some
form of equational reasoning is required to solve these cases."))

(defflag MS03-WEIGHT-OCCURS-CHECK
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 150)
  (mhelp "Controls the penalty for trying to unify two terms that require
getting around an occurs check (using equational reasoning)."))

(defflag MS03-WEIGHT-BANNED-SELS
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 300)
  (mhelp "Controls the penalty for trying to unify two terms that require
getting around using a banned selected variable (using duplication or
equational reasoning)."))

(defflag MS03-WEIGHT-RIGIDRIGID-NOEQN
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 500)
  (mhelp "Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of a base type other than O in the absence
of any equations of the same base type.  Some form of equational
reasoning is required to solve these cases, but we may need to mate
two nodes before an appropriate equation has appeared in the search.
Such a case is unusual so it makes sense for this flag to be set to a
high value."))

(defflag MS03-WEIGHT-FLEXRIGID-EQN
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 100)
  (mhelp "Controls the penalty for trying to unify two terms that require a
solving a flex-rigid pair of a base type other than O when no
imitation and no projection is appropriate and there is an an equation
which is between a pair of rigid terms sharing a head with the
disagreement pair."))

(defflag MS03-WEIGHT-FLEXRIGID-FLEXEQN
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 100)
  (mhelp "Controls the penalty for trying to unify two terms that require a
solving a flex-rigid pair of a base type other than O when no
imitation and no projection is appropriate and there is a flex-rigid
equation between terms of the same base type."))

(defflag MS03-WEIGHT-FLEXRIGID-NOEQN
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 unification transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 500)
  (mhelp "Controls the penalty for trying to unify two terms that require a
solving a flex-rigid pair of a base type other than O when no
imitation and no projection is appropriate and there are no flex-rigid
equations between terms of the same base type."))

(defflag MS03-WEIGHT-PRIMSUB-FIRST-PROJ
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 500)
  (mhelp "Controls when MS03-7 or MS04-2 first tries a primsub using a projection."))

(defflag MS03-WEIGHT-PRIMSUB-NEXT-PROJ
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 500)
  (mhelp "Controls how often MS03-7 or MS04-2 tries a primsub using a projection after the first time."))

(defflag MS03-WEIGHT-PRIMSUB-FIRST-NOT-PROJ
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 500)
  (mhelp "Controls when MS03-7 or MS04-2 first tries a primsub using negation and a projection."))

(defflag MS03-WEIGHT-PRIMSUB-NEXT-NOT-PROJ
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 500)
  (mhelp "Controls how often MS03-7 or MS04-2 tries a primsub using negation and a
projection after the first time."))

(defflag MS03-WEIGHT-PRIMSUB-FIRST-FORALL
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls when MS03-7 or MS04-2 first tries a primsub using FORALL."))

(defflag MS03-WEIGHT-PRIMSUB-FIRST-EXISTS
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls when MS03-7 or MS04-2 first tries a primsub using EXISTS."))

(defflag MS03-WEIGHT-PRIMSUB-NEXT-FORALL
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls how often MS03-7 or MS04-2 tries a primsub using FORALL at various
types after the first time."))

(defflag MS03-WEIGHT-PRIMSUB-NEXT-EXISTS
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls how often MS03-7 or MS04-2 tries a primsub using EXISTS at various
types after the first time."))

(defflag MS03-WEIGHT-PRIMSUB-FIRST-AND
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls when MS03-7 or MS04-2 first tries a primsub using AND."))

(defflag MS03-WEIGHT-PRIMSUB-FIRST-OR
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls when MS03-7 or MS04-2 first tries a primsub using OR."))

(defflag MS03-WEIGHT-PRIMSUB-NEXT-AND
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls how often MS03-7 or MS04-2 tries a primsub using AND after the first time."))

(defflag MS03-WEIGHT-PRIMSUB-NEXT-OR
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls how often MS03-7 or MS04-2 tries a primsub using OR after the first time."))

(defflag MS03-WEIGHT-PRIMSUB-FIRST-EQUALS
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls when MS03-7 or MS04-2 first tries a primsub using equality at a base type."))

(defflag MS03-WEIGHT-PRIMSUB-NEXT-EQUALS
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls how often MS03-7 or MS04-2 tries a primsub using equality at a base
type after the first time."))

(defflag MS03-WEIGHT-PRIMSUB-FIRST-NOT-EQUALS
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls when MS03-7 or MS04-2 first tries a primsub using negation and equality
at a base type."))

(defflag MS03-WEIGHT-PRIMSUB-NEXT-NOT-EQUALS
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 200)
  (mhelp "Controls how often MS03-7 or MS04-2 tries a primsub using negation and equality
at a base type after the first time."))

(defflag MS03-WEIGHT-PRIMSUB-TRUTH
  (flagtype integer+)
  (subjects ext-search ms03-7 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 )))))
  (default 50)
  (mhelp "Controls how often MS03-7 tries a primsub using TRUTH"))

(defflag MS03-WEIGHT-PRIMSUB-FALSEHOOD
  (flagtype integer+)
  (subjects ext-search ms03-7 primsubs transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 )))))
  (default 50)
  (mhelp "Controls how often MS03-7 tries a primsub using FORALL"))

(defflag MS03-QUICK-EUNIFICATION-LIMIT
  (flagtype integer+)
  (subjects ext-search ms03-7 ms04-2 transmit)
  (relevancy-preconditions
   (default-ms (memq default-ms '(ms03-7 ms04-2))))
  (irrelevancy-preconditions
   (default-ms (not (memq default-ms '(ms03-7 ms04-2)))))
  (default 50)
  (mhelp "This provides a bound on how much E-unification MS03-7 and MS04-2 attempt to do
before deciding what to mate."))

(defflag MS03-WEIGHT-DUP-VAR
  (flagtype integer+)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms03-7)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms03-7)))
  (default 300)
  (mhelp "Controls how often MS03-7 tries to duplicate an expansion variable in
order to substitute a banned selected variable for the new expansion
variable."))

(defflag MS03-WEIGHT-CHANGE-DUPS
  (flagtype integer+)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (and (memq default-ms '(ms03-7)) (not max-search-limit)))
   (max-search-limit (and (memq default-ms '(ms03-7)) (not max-search-limit))))
  (irrelevancy-preconditions
   (default-ms (or (not (memq default-ms '(ms03-7))) max-search-limit))
   (max-search-limit (or (not (memq default-ms '(ms03-7))) max-search-limit)))
  (default 100)
  (mhelp "If MAX-SEARCH-LIMIT is NIL, then MS03-WEIGHT-CHANGE-DUPS controls
how often MS03-7 changes which expansion terms are considered.

SEE ALSO:  MS03-DUP-METHOD"))

(defflag MS03-SOLVE-RIGID-PARTS
  (flagtype boolean)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (and (memq default-ms '(ms03-7)) ms03-use-jforms))
   (ms03-use-jforms (and (memq default-ms '(ms03-7)) ms03-use-jforms)))
  (irrelevancy-preconditions
   (default-ms (or (not (memq default-ms '(ms03-7))) (not ms03-use-jforms)))
   (ms03-use-jforms (or (not (memq default-ms '(ms03-7))) (not ms03-use-jforms))))
  (default t)
  (mhelp "If T, MS03-7 tries to find quick solutions to the rigid parts of a
problem.  This only applies when MS03-USE-JFORMS is T."))

(defflag MS03-SOLVE-RIGID-PARTS-ALLOW-RECONNECTS
  (flagtype boolean)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (and (memq default-ms '(ms03-7)) ms03-use-jforms))
   (ms03-use-jforms (and (memq default-ms '(ms03-7)) ms03-use-jforms)))
  (irrelevancy-preconditions
   (default-ms (or (not (memq default-ms '(ms03-7))) (not ms03-use-jforms)))
   (ms03-use-jforms (or (not (memq default-ms '(ms03-7))) (not ms03-use-jforms))))
  (default t)
  (mhelp "When trying to solve the rigid part of a jform,
we might consider connecting two literals that are already connected.
Sometimes this speeds up the search, presumably by keeping
us from looking at possible connections beneath connections (needed to show
equivalences)."))

(defflag MS03-DUP-METHOD
  (flagtype posnumber)
  (subjects ext-search ms03-7 transmit)
  (relevancy-preconditions
   (default-ms (eq default-ms 'ms03-7)))
  (irrelevancy-preconditions
   (default-ms (neq default-ms 'ms03-7)))
  (default 1)
  (mhelp "The method by which different duplication options are considered
by the MS03-7 search procedure.

1.  Simply add the oldest expansion arc that has not been considered yet
(and any arcs related to it) each time a new option is tried.
This will lead to extremely large jforms in most cases.

2.  Works like 1 except with respect to expansion arcs that either
contain a nontrivial set substitution (ie, one with logical connectives)
or are associated with a set existence lemma.  With respect to these
'set expansion arcs', we remove whatever such arcs are in the current
option and replace them with a new set expansion arc (thus considering
a new set expansion option).  If every single set expansion option
has been considered, we begin considering two at a time, and so on.

3.  Works like 2 except we treat every expansion of set type
as a set expansion arc instead of just the ones with nontrivial
set substitutions.

See Also: MS03-WEIGHT-CHANGE-DUPS, MAX-SEARCH-LIMIT"))

(defun ext-matingsearch-controller-test (&rest ignore)
  (ext-matingsearch-controller (get dproof 'represents)))

(defun ext-matingsearch-controller (wff &rest ignore)
   (declare (ignore ignore))
   (startcount 'mating-ctr)
   (runcount 'unification) (breakcount 'unification) ; timing gets confused about whethere unification was interrupted
   (runcount 'unification) (breakcount 'unification) ; if these get called only once, so here we call them twice to get started
   (unwind-protect  
       (ext-matingsearch-controller-real wff)
     (runcount 'mating)
     (breakcount 'mating)
     (breakcount 'mating-ctr)))

(defun ext-matingsearch-controller-real (wff &rest ignore)
  (declare (ignore ignore) (special num-of-splitting))
  (setq num-of-splitting 0);In order to cope with the splitting while recording time
  (prog1
      (case default-ms
	(ms03-7 (ext-saturation-search wff))
	(ms04-2 (ms04-search wff))
	(t (throwfail "DEFAULT-MS " default-ms " is not appropriate for extensional search.")))
    (runcount 'mating-ctr)))

(defun ext-initialize-search (wff)
  (declare (special *edag-lift-info* *individual-types* *eeod-stamp*))
  (setq *eeod-stamp* 0)
  (setq *individual-types* (find-prim-types wff))
  (dolist (x (free-vars-of wff))
    (setf (get x 'banned-forall) nil)
    (setf (get x 'banned-exists) nil)
    (setf (get x 'banned-projs) nil)
    (setf (get x 'banned-imitations) nil)
    (setf (get x 'banned-not-projs) nil)
    (setf (get x 'banned-not-imitations) nil)
    (setf (get x 'banned-sel-vars) nil)
    (setf (get x 'already-duped-for) nil)
    (setf (get x 'banned-abstract-formulas) nil)
    (setf (get x 'ext-exp-var) nil)
    (setf (get x 'ext-exp-var-subst) nil)
    (setf (get x 'ext-exp-var-arcs) nil)
    (setf (get x 'exp-vars-above) nil))
  (setq *edag-lift-info* nil))

(defun eeod-to-top-jform (eeod)
  (normalize-jform (eeod-to-top-jform-1 eeod)))

(defun eeod-to-top-jform-1 (eeod)
  (let ((k (ext-exp-open-dag-kind eeod))
	(pos (ext-exp-open-dag-positive eeod))
	(j (eeod-junctive eeod)))
    (cond ((or (eq k 'ATOM) (eq k 'EQNGOAL) (and (eq k 'EQN) pos))
	   (make-literal :name (ext-exp-open-dag-name eeod) :pos pos
			 :represents (ext-exp-open-dag-shallow eeod)))
	  ((eq j 'DIS)
	   (make-disjunction :components
			     (mapcar #'(lambda (arc)
					 (eeod-to-top-jform-1 (ext-exp-open-arc-node arc)))
				     (ext-exp-open-dag-arcs eeod))))
	  ((eq j 'CON)
	   (make-conjunction :components
			     (mapcar #'(lambda (arc)
					 (eeod-to-top-jform-1 (ext-exp-open-arc-node arc)))
				     (ext-exp-open-dag-arcs eeod))))
	  ((= (length (ext-exp-open-dag-arcs eeod)) 1)
	   (eeod-to-top-jform-1 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs eeod)))))
	  (t (throwfail "Unknown case for computing jform - " t
			eeod " is neither conjunction, disjunction, nor has a single child")))))

(defun eeod-to-jform (eeod &key (posflex t) (negflex t) (flexflex nil))
  (let ((jform-assoc nil))
    (declare (special jform-assoc))
    (let ((eeod-jform (normalize-jform
		       (eeod-to-jform-1 eeod :posflex posflex :negflex negflex
					 :flexflex flexflex)))
	  (nodes-delayed nil)
	  (nodes-done nil))
      (declare (special nodes-delayed nodes-done eeod-jform))
      (eeod-to-jform-2 eeod)
      eeod-jform)))

(defun eeod-to-jform-1 (eeod &key (posflex t) (negflex t) (flexflex nil))
  (declare (special jform-assoc))
  (let ((a (assoc eeod jform-assoc)))
    (if a
	(cdr a)
      (let ((k (ext-exp-open-dag-kind eeod))
	    (pos (ext-exp-open-dag-positive eeod))
	    (j (eeod-junctive eeod)))
	(cond ((or (eq k 'ATOM) (eq k 'LEAF) (and (eq k 'EQN) pos))
	       (dolist (arc (ext-exp-open-dag-arcs eeod))
		 (eeod-to-jform-1 (ext-exp-open-arc-node arc)
				   :posflex posflex :negflex negflex :flexflex flexflex))
	       (let ((j (make-literal :name (ext-exp-open-dag-name eeod) :pos pos
					 :represents (ext-exp-open-dag-shallow eeod))))
		 (push (cons eeod j) jform-assoc)
		 j))
	      ((eq k 'EQNGOAL)
	       (dolist (arc (ext-exp-open-dag-arcs eeod))
		 (eeod-to-jform-1 (ext-exp-open-arc-node arc)
				   :posflex posflex :negflex negflex :flexflex flexflex))
	       (let* ((sh (ext-exp-open-dag-shallow eeod))
		      (j (if (or flexflex
				 (not (ext-exp-var-p (head (cdar sh))))
				 (not (ext-exp-var-p (head (cdr sh)))))
			     (make-literal :name (ext-exp-open-dag-name eeod) :pos pos
					   :represents sh)
			   (make-disjunction))))
		 (push (cons eeod j) jform-assoc)
		 j))
	      ((eq k 'FLEX)
	       (let ((j (if (or (and posflex pos)
				(and negflex (not pos)))
			    (make-literal :name (ext-exp-open-dag-name eeod) :pos pos
					  :represents (ext-exp-open-dag-shallow eeod))
			  (make-disjunction))))
		 (push (cons eeod j) jform-assoc)
		 j))
	      ((eq j 'DIS)
	       (let ((j (make-disjunction :components
					  (mapcar #'(lambda (arc)
						      (eeod-to-jform-1 (ext-exp-open-arc-node arc)
									:posflex posflex :negflex negflex
									:flexflex flexflex))
						  (ext-exp-open-dag-arcs eeod)))))
		 (push (cons eeod j) jform-assoc)
		 j))
	      ((eq k 'EXP)
	       (let* ((arcs (ext-exp-open-dag-arcs eeod)))
		 (make-conjunction :components
				   (mapcar #'(lambda (arc)
					       (let ((expvars
						      (remove-duplicates
						       (remove-if-not
							#'ext-exp-var-p
							(free-vars-of (ext-exp-open-arc-exp-term arc))))))
						 (make-universal
						  :qvars expvars
						  :scope
						  (eeod-to-jform-1 (ext-exp-open-arc-node arc)
								   :posflex posflex :negflex negflex
								   :flexflex flexflex))))
					   arcs))))
	      ((eq j 'CON)
	       (let* ((arcs (ext-exp-open-dag-arcs eeod))
		      (j (make-conjunction :components
					  (mapcar #'(lambda (arc)
						      (eeod-to-jform-1 (ext-exp-open-arc-node arc)
									:posflex posflex :negflex negflex
									:flexflex flexflex))
						  arcs))))
		 (push (cons eeod j) jform-assoc)
		 j))
	      ((= (length (ext-exp-open-dag-arcs eeod)) 1)
	       (let ((j (eeod-to-jform-1 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs eeod)))
					  :posflex posflex :negflex negflex :flexflex flexflex)))
		 (push (cons eeod j) jform-assoc)
		 j))
	      (t (throwfail "Unknown case for computing jform - " t
			    eeod " is neither conjunction, disjunction, nor has a single child")))))))

(defun eeod-to-jform-2 (eeod)
  (declare (special jform-assoc nodes-delayed nodes-done eeod-jform))
  (let ((k (ext-exp-open-dag-kind eeod))
	(pos (ext-exp-open-dag-positive eeod)))
    (if (or (eq k 'ATOM) (eq k 'EQNGOAL) (and (eq k 'EQN) pos))
	(dolist (arc (ext-exp-open-dag-arcs eeod))
	  (let ((node (ext-exp-open-arc-node arc)))
	    (unless (member node nodes-done)
	      (if (member node nodes-delayed)
		  (progn
		    (push node nodes-done)
		    (let* ((newsubj (cdr (assoc node jform-assoc)))
			   (parent-arcs (ext-exp-open-dag-parent-arcs node))
			   (arc1 (car parent-arcs))
			   (arc2 (cadr parent-arcs))
			   (litname1 (ext-exp-open-dag-name (ext-exp-open-arc-parent arc1)))
			   (litname2 (ext-exp-open-dag-name (ext-exp-open-arc-parent arc2))))
		      (setq eeod-jform (jform-mate-pair litname1 litname2 eeod-jform newsubj))
		      (eeod-to-jform-2 node)))
		(push node nodes-delayed)))))
      (dolist (arc (ext-exp-open-dag-arcs eeod))
	(eeod-to-jform-2 (ext-exp-open-arc-node arc))))))

(defun jform-mate-pair (litname1 litname2 jform newsubj)
  (let ((anc1 (jform-ancestors litname1 jform))
	(anc2 (jform-ancestors litname2 jform)))
    (normalize-jform
     (jform-mate-pair-1 litname1 anc1 litname2 anc2 jform newsubj))))

(defun jform-mate-pair-1 (litname1 anc1 litname2 anc2 jform newsubj)
  (case (jform-type jform)
    (disjunction
     (let ((disjs nil))
       (dolist (j (disjunction-components jform))
	 (if (and (member j anc1) (member j anc2))
	     (push (jform-mate-pair-1 litname1 anc1 litname2 anc2 j newsubj) disjs)
	   (push j disjs)))
       (make-disjunction :components disjs)))
    (conjunction
     (let ((conj1 nil)
	   (conj2 nil)
	   (conjs nil))
       (dolist (j (conjunction-components jform))
	 (if (member j anc1)
	     (if (member j anc2)
		 (push (jform-mate-pair-1 litname1 anc1 litname2 anc2 j newsubj) conjs)
	       (setq conj1 j))
	   (if (member j anc2)
	       (setq conj2 j)
	     (push j conjs))))
       (if (and conj1 conj2)
	   (let* ((cc1l (dissolve-cc litname1 conj1))
		  (cc2l (dissolve-cc litname2 conj2))
		  (cc1 (if cc1l (car cc1l) (make-disjunction :components nil)))
		  (cc2 (if cc2l (car cc2l) (make-disjunction :components nil)))
		  (cpe1 (dissolve-cpe litname1 conj1))
		  (cpe2 (dissolve-cpe litname2 conj2))
		  (disj (make-disjunction
			 :components
			 (list (make-conjunction :components (list cc1 conj2))
			       (make-conjunction :components (list cpe1 cc2))
			       (make-conjunction :components (list newsubj cpe1 cpe2))))))
	     (make-conjunction :components (cons disj conjs)))
	 (progn
	   (when conj1 (push conj1 conjs))
	   (when conj2 (push conj2 conjs))
	   (make-conjunction :components conjs)))))
    (universal
     (make-universal :qvars (universal-qvars jform)
		     :scope (jform-mate-pair-1 litname1 anc1 litname2 anc2
					       (universal-scope jform)
					       newsubj)))
    (t jform)))

(defun jform-ancestors (litname jform)
  (case (jform-type jform)
    (disjunction
     (let ((anc (apply #'append (mapcar #'(lambda (x)
					    (jform-ancestors litname x))
					(disjunction-components jform)))))
       (if anc
	   (cons jform anc)
	 nil)))
    (conjunction
     (let ((anc (apply #'append (mapcar #'(lambda (x)
					    (jform-ancestors litname x))
					(conjunction-components jform)))))
       (if anc
	   (cons jform anc)
	 nil)))
    (universal
     (let ((anc (jform-ancestors litname (universal-scope jform))))
       (if anc
	   (cons jform anc)
	 nil)))
    (literal
     (if (eq litname (literal-name jform))
	 (list jform)
       nil))
    (t nil)))

(defun eeod-update-jform (jform &optional parent posflex negflex flexflex)
  (case (jform-type jform)
    (disjunction
     (let ((dis (make-disjunction :parent parent)))
       (setf (disjunction-components dis)
	     (mapcar #'(lambda (x)
			 (eeod-update-jform x dis posflex negflex flexflex))
		     (disjunction-components jform)))
       dis))
    (conjunction
     (let ((con (make-conjunction :parent parent)))
       (setf (conjunction-components con)
	     (mapcar #'(lambda (x)
			 (eeod-update-jform x con posflex negflex flexflex))
		     (conjunction-components jform)))
       con))
    (universal
     (let ((qvars (remove-if #'(lambda (x)
				 (get x 'ext-exp-var-subst))
			     (universal-qvars jform))))
       (if qvars
	   (let ((univ (make-universal :qvars qvars :scope (universal-scope jform))))
	     (setf (universal-scope univ)
		   (eeod-update-jform (universal-scope jform)
				      univ posflex negflex flexflex))
	     univ)
	 (eeod-update-jform (universal-scope jform)
			    parent posflex negflex flexflex))))
    (literal
     (let ((node (name-to-eeod (literal-name jform))))
       (if (eq (ext-exp-open-dag-kind node) 'EQNGOAL)
	   (eeod-to-jform
	    (ext-exp-open-arc-parent
	     (car (ext-exp-open-dag-parent-arcs node)))
	    :posflex posflex :negflex negflex :flexflex flexflex)
	 (eeod-to-jform
	  node
	  :posflex posflex :negflex negflex :flexflex flexflex))))))

(defun conjunctively-related-literals (pred jform)
  (let ((par (jform-parent jform)))
    (if par
	(let ((lits (when (conjunction-p par)
		      (apply #'append
			     (mapcar #'(lambda (x)
					 (find-literals-in-jform pred x))
				     (remove jform (conjunction-components par)))))))
	  (append lits (conjunctively-related-literals pred par)))
      nil)))

(defun conjunctively-related-nodes (pred node)
  (conjunctively-related-nodes-1 pred node))

(defun conjunctively-related-nodes-1 (pred node)
  (let ((parent-arcs (ext-exp-open-dag-parent-arcs node)))
    (if parent-arcs
	(let* ((par-arc (car parent-arcs))
	       (par (ext-exp-open-arc-parent par-arc))
	       (ret (conjunctively-related-nodes-2 pred par-arc par)))
	  (dolist (par-arc (cdr parent-arcs) ret)
	    (setq ret
		  (intersection ret
				(conjunctively-related-nodes-2
				 pred par-arc (ext-exp-open-arc-parent par-arc)))))
	  (when (funcall pred node)
	    (setq ret (adjoin node ret)))
	  ret)
      nil)))

(defun conjunctively-related-nodes-2 (pred arc node)
  (let ((ret (conjunctively-related-nodes-1 pred node)))
    (when (eq (eeod-junctive node) 'CON)
      (let ((nodes-done nil))
	(declare (special nodes-done))
	(setq ret (union ret (eeod-get-nodes-l
			      pred
			      (mapcar #'(lambda (x)
					  (ext-exp-open-arc-node x))
				      (remove arc (ext-exp-open-dag-arcs node))))))))
    ret))

; adds mates between any positive and negative rigid atoms that share a vertical path
; returns NIL if no new rigid mates are added
(defun eeod-add-all-rigid-mates (&optional (eeod *current-edag*) (jform *ext-rigid-jform*))
  (let* ((poslits
	  (find-literals-in-jform
	   #'(lambda (x)
	       (and (jform-pos x)
		    (not (equals-p (jform-represents x)))
		    (not (ext-exp-var-p (head (jform-represents x))))))
	   jform))
	 (poslitnames (remove-duplicates (mapcar #'(lambda (x)
						     (literal-name x))
						 poslits)))
	 (posatoms (mapcar #'(lambda (x) (name-to-eeod x)) poslitnames))
	 added-mate)
    (declare (special added-mate))
    (dolist (pa posatoms)
      (eeod-add-all-rigid-mates-1 pa poslits))
    added-mate))

(defun eeod-add-all-rigid-mates-1 (pa poslits)
  (declare (special added-mate))
  (let* ((possh (ext-exp-open-dag-shallow pa))
	 (name (ext-exp-open-dag-name pa))
	 (h (head possh))
	 (neglits nil))
    (dolist (plit poslits)
      (when (eq name (literal-name plit))
	(setq neglits (append neglits
			      (conjunctively-related-literals
			       #'(lambda (x)
				   (and (not (jform-pos x))
					(not (equals-p (jform-represents x)))
					(eq h (head (jform-represents x)))))
			       plit)))))
    (let* ((neglitnames (remove-duplicates (mapcar #'(lambda (x)
						       (literal-name x))
						   neglits)))
	   (negatoms (mapcar #'(lambda (x) (name-to-eeod x)) neglitnames)))
      (dolist (na negatoms)
	(setq added-mate (or (eeod-mate pa na) added-mate))))))

(defun eeod-mateable-p (posatom negatom)
  (and (ext-exp-open-dag-p posatom)
       (ext-exp-open-dag-p negatom)
       (eq (ext-exp-open-dag-kind posatom) 'ATOM)
       (eq (ext-exp-open-dag-kind negatom) 'ATOM)
       (ext-exp-open-dag-positive posatom)
       (not (ext-exp-open-dag-positive negatom))
       (let ((h1 (head (ext-exp-open-dag-shallow posatom)))
	     (h2 (head (ext-exp-open-dag-shallow negatom))))
	 (and (eq h1 h2)
	      (not (ext-exp-var-p h1))))))

(defun eeod-mate (posatom negatom)
  (unless (eeod-mated-p posatom negatom)
    (let* ((possh (ext-exp-open-dag-shallow posatom))
	   (negsh (ext-exp-open-dag-shallow negatom))
	   (h1 (head possh))
	   (h2 (head negsh)))
      (unless (eq h1 h2)
	(throwfail "Heads " h1 " and " h2 " do not match"))
      (when (ext-exp-var-p h1)
	(throwfail "Head " h1 " is not rigid"))
      (let* ((n (length (args possh)))
	     (arc1 (make-ext-exp-open-arc :parent posatom :kind 'MATE))
	     (arc2 (make-ext-exp-open-arc :parent negatom :kind 'MATE))
	     (eqwff (acons (inherit-abbrev '= '((O . O) . O) '(O)) negsh possh))
	     (node (make-ext-exp-open-dag :parent-arcs (list arc1 arc2) :shallow eqwff :positive nil
					  :kind 'DEC
					  :name (intern-str (create-namestring 'DEC))))
	     (decarcs (create-simple-ext-exp-open-arc-decs n negsh possh node)))
	(setf (ext-exp-open-arc-node arc1) node)
	(setf (ext-exp-open-arc-node arc2) node)
	(push arc1 (ext-exp-open-dag-arcs posatom))
	(push arc2 (ext-exp-open-dag-arcs negatom))
	(setf (ext-exp-open-dag-arcs node) decarcs)
	node))))

(defun eeod-eunifable-p (poseqn eqngoal)
  (and (ext-exp-open-dag-p poseqn)
       (ext-exp-open-dag-p eqngoal)
       (eq (ext-exp-open-dag-kind poseqn) 'EQN)
       (eq (ext-exp-open-dag-kind eqngoal) 'EQNGOAL)
       (ext-exp-open-dag-positive poseqn)
       (let* ((possh (ext-exp-open-dag-shallow poseqn))
	      (negsh (ext-exp-open-dag-shallow eqngoal)))
	 (and (equals-p possh)
	      (equals-p negsh)
	      (eq (caar possh) (caar negsh)))))) ; same equality (i.e., equality at same [basic] types)

(defun eeod-eunifk (poseqn negeqn euk)
  (unless (eeod-eunifk-p poseqn negeqn euk)
    (let* ((possh (ext-exp-open-dag-shallow poseqn))
	   (negsh (ext-exp-open-dag-shallow negeqn))
	   (lft1 (cdar possh))
	   (rght1 (cdr possh))
	   (lft2 (cdar negsh))
	   (rght2 (cdr negsh))
	   (arc1 (make-ext-exp-open-arc :parent poseqn :kind euk))
	   (arc2 (make-ext-exp-open-arc :parent negeqn :kind euk))
	   (tp (type lft1))
	   (eqwff1 (acons (inherit-abbrev '= (acons 'O tp tp) (list tp))
			  lft1
			  (if (eq euk 'EUNIF1) lft2 rght2)))
	   (eqwff2 (acons (inherit-abbrev '= (acons 'O tp tp) (list tp))
			  rght1
			  (if (eq euk 'EUNIF1) rght2 lft2)))
	   (con (acons 'AND eqwff1 eqwff2))
	   (node (create-simple-ext-exp-open-dag con nil (list arc1 arc2))))
      (setf (ext-exp-open-arc-node arc1) node)
      (setf (ext-exp-open-arc-node arc2) node)
      (push arc1 (ext-exp-open-dag-arcs poseqn))
      (push arc2 (ext-exp-open-dag-arcs negeqn))
      node)))

(defun eeod-eunif1 (poseqn negeqn)
  (eeod-eunifk poseqn negeqn 'EUNIF1))

(defun eeod-eunif2 (poseqn negeqn)
  (eeod-eunifk poseqn negeqn 'EUNIF2))

(defun eeod-mated-p (node1 node2)
  (intersection (ext-exp-open-dag-kids node1 'MATE)
		(ext-exp-open-dag-kids node2 'MATE)))

(defun eeod-eunifk-p (node1 node2 euk)
  (intersection (ext-exp-open-dag-kids node1 euk)
		(ext-exp-open-dag-kids node2 euk)))

(defun eeod-eunif1-p (node1 node2)
  (eeod-eunifk-p node1 node2 'EUNIF1))

(defun eeod-eunif2-p (node1 node2)
  (eeod-eunifk-p node1 node2 'EUNIF2))

(defun eeod-connected-p (node1 node2)
  (intersection (ext-exp-open-dag-kids node1)
		(ext-exp-open-dag-kids node2)))

(defun eeod-duplicate-and-lift-subst (theta)
  (let ((ev-assoc nil))
    (dolist (p theta ev-assoc)
      (setq ev-assoc (append (eeod-duplicate-and-lift-subst-1 (car p) (cdr p))
			     ev-assoc)))))

(defun eeod-duplicate-and-lift-subst-1 (ev wff)
  (let ((cl (classify-lambda-term wff)))
    (case (car cl)
      (PROJECTION
       (multiple-value-bind
	   (ev2 evars1)
	   (eeod-duplicate-and-project ev (cadr cl))
	 (acons ev ev2
		(eeod-duplicate-and-lift-subst
		 (lift-subst-theta (get ev 'ext-exp-var-subst) wff)))))
      ((IMITATION EQUALS)
       (unless (or (member (cadr cl) (get ev 'banned-sel-vars))
		   (ext-exp-var-p (cadr cl)))
	 (multiple-value-bind
	     (ev2 evars1)
	     (eeod-duplicate-and-imitate ev (cadr cl))
	   (acons ev ev2
		  (eeod-duplicate-and-lift-subst
		   (lift-subst-theta (get ev 'ext-exp-var-subst) wff))))))
      (FORALL
       (let ((ev2 (eeod-duplicate-and-subst-forall ev (cadr cl))))
	 (acons ev ev2
		(eeod-duplicate-and-lift-subst
		 (lift-subst-theta (get ev 'ext-exp-var-subst) wff)))))
      (EXISTS
       (let ((ev2 (eeod-duplicate-and-subst-exist ev (cadr cl))))
	 (acons ev ev2
		(eeod-duplicate-and-lift-subst
		 (lift-subst-theta (get ev 'ext-exp-var-subst) wff)))))
      ((AND OR IMPLIES EQUIV)
       (multiple-value-bind
	   (ev2 evars1)
	   (eeod-duplicate-and-imitate ev (car cl))
	 (acons ev ev2
		(eeod-duplicate-and-lift-subst
		 (lift-subst-theta (get ev 'ext-exp-var-subst) wff)))))
      (NOT
       (multiple-value-bind
	   (ev2 evars1)
	   (eeod-duplicate-and-imitate ev 'NOT)
	 (acons ev ev2
		(eeod-duplicate-and-lift-subst
		 (lift-subst-theta (get ev 'ext-exp-var-subst) wff)))))
      (TRUTH
       (multiple-value-bind
	   (ev2 evars1)
	   (eeod-duplicate-and-imitate ev 'TRUTH)
	 (acons ev ev2
		(eeod-duplicate-and-lift-subst
		 (lift-subst-theta (get ev 'ext-exp-var-subst) wff)))))
      (FALSEHOOD
       (multiple-value-bind
	   (ev2 evars1)
	   (eeod-duplicate-and-imitate ev 'FALSEHOOD)
	 (acons ev ev2
		(eeod-duplicate-and-lift-subst
		 (lift-subst-theta (get ev 'ext-exp-var-subst) wff)))))
      (t nil))))

; assumes wff1 is of the form [lambda z-bar . h [ev1 z-bar] . . . [evn z-bar]]
(defun lift-subst-theta (wff1 wff2)
  (lift-subst-theta-1 (unabbreviated-type wff1) wff1 wff2))

(defun lift-subst-theta-1 (tp wff1 wff2 &optional bdvars)
  (if (consp tp)
      (let ((w (fresh-var (cdr tp) '|w|)))
	(lift-subst-theta-1 (car tp) (cons wff1 w) (cons wff2 w) (cons w bdvars)))
    (lift-subst-theta-2 (lambda-norm wff1) (lambda-norm wff2) bdvars)))

(defun lift-subst-theta-2 (wff1 wff2 bdvars)
  (let ((h1 (head wff1))
	(h2 (head wff2)))
    (if (and (eq h1 h2) (not (ext-exp-var-p h1)))
	(remove-if-not #'identity
		       (mapcar #'(lambda (x y)
				   (lift-subst-theta-3 (unabbreviated-type x) x y bdvars))
			       (args wff1) (args wff2)))
      nil)))

(defun lift-subst-theta-3 (tp wff1 wff2 bdvars)
  (if (consp tp)
      (let ((w (fresh-var (cdr tp) '|w|)))
	(lift-subst-theta-3 (car tp) (cons wff1 w) (cons wff2 w) (cons w bdvars)))
    (lift-subst-theta-4 (lambda-norm wff1) (lambda-norm wff2) bdvars)))

(defun lift-subst-theta-4 (wff1 wff2 bdvars)
  (let ((ev (head wff1)))
    (if (ext-exp-var-p ev)
	(let ((args (args wff1))
	      (wff3 wff2))
	  (dolist (a (reverse args) (if wff3 (cons ev wff3) nil))
	    (when wff3
	      (if (member a bdvars)
		  (setq wff3 (acons a 'lambda wff3) bdvars (remove a bdvars))
		(setq wff3 nil)))))
      nil)))

(defun eeod-duplicate-and-subst-forall (ev tp)
  (let ((ev2 (eeod-duplicate-expvar ev)))
    (setf (get ev2 'banned-abstract-formulas) (get ev 'banned-abstract-formulas))
    (setf (get ev2 'banned-forall) (cons tp (get ev 'banned-forall)))
    (setf (get ev2 'banned-exists) (get ev 'banned-exists))
    (setf (get ev2 'banned-imitations) (get ev 'banned-imitations))
    (setf (get ev2 'banned-projs) (get ev 'banned-projs))
    (eeod-subst-forall ev tp)
    ev2))

(defun eeod-duplicate-and-subst-exist (ev tp)
  (let ((ev2 (eeod-duplicate-expvar ev)))
    (setf (get ev2 'banned-abstract-formulas) (get ev 'banned-abstract-formulas))
    (setf (get ev2 'banned-forall) (get ev 'banned-forall))
    (setf (get ev2 'banned-exists) (cons tp (get ev 'banned-exists)))
    (setf (get ev2 'banned-imitations) (get ev 'banned-imitations))
    (setf (get ev2 'banned-projs) (get ev 'banned-projs))
    (eeod-subst-exists ev tp)
    ev2))

; put a negation in front of the imitation
(defun eeod-duplicate-and-imitate-not (ev h)
  (multiple-value-bind
      (ev2 evars2)
      (eeod-duplicate-and-imitate ev 'NOT)
    (let ((ev3 (car evars2)))
      (multiple-value-bind
	  (ev4 evars4)
	  (eeod-duplicate-and-imitate ev3 h)
	(values ev2 ev4 ev3 evars4)))))

; put a negation in front of the projection
(defun eeod-duplicate-and-project-not (ev i)
  (multiple-value-bind
      (ev2 evars2)
      (eeod-duplicate-and-imitate ev 'NOT)
    (let ((ev3 (car evars2)))
      (multiple-value-bind
	  (ev4 evars4)
	  (eeod-duplicate-and-project ev3 i)
	(values ev2 ev4 ev3 evars4)))))

(defun eeod-duplicate-and-imitate (ev h)
  (if (member h (get ev 'banned-sel-vars))
      'banned
    (let ((ev2 (eeod-duplicate-expvar ev)))
      (setf (get ev2 'banned-abstract-formulas) (get ev 'banned-abstract-formulas))
      (setf (get ev2 'banned-forall) (get ev 'banned-forall))
      (setf (get ev2 'banned-exists) (get ev 'banned-exists))
      (setf (get ev2 'banned-imitations) (cons h (get ev 'banned-imitations)))
      (setf (get ev2 'banned-projs) (get ev 'banned-projs))
      (let ((evars (eeod-subst-imit ev h)))
	(values ev2 evars)))))

(defun eeod-duplicate-and-project (ev i)
  (let ((ev2 (eeod-duplicate-expvar ev)))
    (setf (get ev2 'banned-abstract-formulas) (get ev 'banned-abstract-formulas))
    (setf (get ev2 'banned-forall) (get ev 'banned-forall))
    (setf (get ev2 'banned-exists) (get ev 'banned-exists))
    (setf (get ev2 'banned-imitations) (get ev 'banned-imitations))
    (setf (get ev2 'banned-projs) (cons i (get ev 'banned-projs)))
    (let ((evars (eeod-subst-proj ev i)))
      (values ev2 evars))))

(defun new-applied-evar (tp bindvars nameroot banned)
  (if bindvars
      (cons (new-applied-evar (cons tp (unabbreviated-type (car bindvars)))
			      (cdr bindvars) nameroot banned)
	    (car bindvars))
    (let ((ev (fresh-var tp nameroot)))
      (setf (get ev 'ext-exp-var) t)
      (setf (get ev 'banned-sel-vars) banned)
      ev)))

(defun eeod-subst-forall (ev qtp)
  (when (member qtp (get ev 'banned-forall) :test #'equal)
    (throwfail "forall at type " qtp " used already for expansion associated with " (ev . gwff)))
  (when (member 'ALL (get ev 'banned-forall))
    (throwfail "no universal quantifiers are allowed for " qtp))
  (eeod-subst-quant ev qtp 'FORALL))

(defun eeod-subst-exists (ev qtp)
  (when (member qtp (get ev 'banned-exists) :test #'equal)
    (throwfail "exist at type " qtp " used already for expansion associated with " (ev . gwff)))
  (when (member 'ALL (get ev 'banned-exists))
    (throwfail "no existential quantifiers are allowed for " qtp))
  (eeod-subst-quant ev qtp 'EXISTS))

(defun create-quant-subst (ev qtp quant)
  (let* ((bindvars nil)
	 (qx (fresh-var qtp '|w|))
	 (evtp (unabbreviated-type ev)))
    (do ((tp evtp (car tp)))
	((not (consp tp)))
      (push (fresh-var (cdr tp) '|w|) bindvars))
    (let* ((wff (new-applied-evar 'O (cons qx bindvars) '|h| (get ev 'banned-sel-vars)))
	   (ev1 (head wff)))
      (setq wff (acons qx quant wff))
      (dolist (bv bindvars)
	(setq wff (acons bv 'LAMBDA wff)))
      (values wff (list ev1)))))

(defun eeod-subst-quant (ev qtp quant)
  (multiple-value-bind
      (wff evars)
      (create-quant-subst ev qtp quant)
    (let ((arcs (get ev 'ext-exp-var-arcs)))
      (dolist (ev1 evars)
	(setf (get ev1 'ext-exp-var-arcs) arcs)
	(dolist (y (get ev 'banned-sel-vars))
	  (push ev1 (get y 'exp-vars-above))))
      (eeod-subst-deepen (acons ev wff nil) *current-edag*))))

(defun create-imit-subst (ev h)
  (let ((bindvars nil)
	(new-evs nil)
	(wff h))
    (do ((tp (unabbreviated-type ev) (car tp)))
	((not (consp tp)))
      (push (fresh-var (cdr tp) '|w|) bindvars))
    (do ((tp (unabbreviated-type h) (car tp)))
	((not (consp tp)))
      (let* ((arg1 (new-applied-evar (cdr tp) bindvars '|h| (get ev 'banned-sel-vars)))
	     (ev1 (head arg1)))
	(setq wff (cons wff arg1))
	(push ev1 new-evs)))
    (dolist (bv bindvars)
      (setq wff (acons bv 'LAMBDA wff)))
    (values wff new-evs)))

(defun eeod-subst-imit (ev h)
  (when (member h (get ev 'banned-sel-vars))
    (throwfail "Sel Var " (h . gwff) " is banned for Exp Var " (ev . gwff)))
  (when (member h (get ev 'banned-imitations))
    (throwfail "Head " (h . gwff) " is banned for Exp Var " (ev . gwff) " (already used)"))
  (multiple-value-bind
      (wff evars)
      (create-imit-subst ev h)
    (let ((arcs (get ev 'ext-exp-var-arcs)))
      (dolist (x (get h 'exp-vars-above))
	(setf (get x 'banned-sel-vars) (append (get x 'banned-sel-vars)
					       (get ev 'banned-sel-vars))))
      (dolist (ev1 evars)
	(setf (get ev1 'ext-exp-var-arcs) arcs))
      (dolist (y (get ev 'banned-sel-vars))
	(setf (get y 'exp-vars-above) (append evars
					      (get y 'exp-vars-above)
					      (get h 'exp-vars-above))))
      (eeod-subst-deepen (acons ev wff nil) *current-edag*)
      evars)))

(defun create-proj-subst (ev i)
  (let ((bindvars nil)
	(bindvars2 nil)
	(wff nil)
	(new-evs nil))
    (do ((tp (unabbreviated-type ev) (car tp)))
	((not (consp tp)))
      (push (fresh-var (cdr tp) '|w|) bindvars))
    (setq bindvars2 (reverse bindvars))
    (dotimes (j i) (pop bindvars2))
    (setq wff (car bindvars2))
    (do ((tp (unabbreviated-type wff) (car tp)))
	((not (consp tp)))
      (let* ((arg1 (new-applied-evar (cdr tp) bindvars '|h| (get ev 'banned-sel-vars)))
	     (ev1 (head arg1)))
	(setq wff (cons wff arg1))
	(push ev1 new-evs)))
    (dolist (bv bindvars)
      (setq wff (acons bv 'LAMBDA wff)))
    (values wff new-evs)))

(defun eeod-subst-proj (ev i)
  (when (member i (get ev 'banned-projs) :test #'equal)
    (throwfail "Projection " i " is banned for Exp Var " (ev . gwff) " (already used)"))
  (multiple-value-bind
      (wff evars)
      (create-proj-subst ev i)
    (let ((arcs (cdr (get ev 'ext-exp-var-arcs))))
      (dolist (ev1 evars)
	(setf (get ev1 'ext-exp-var-arcs) arcs))
      (dolist (y (get ev 'banned-sel-vars))
	(setf (get y 'exp-vars-above) (append evars
					      (get y 'exp-vars-above))))
      (eeod-subst-deepen (acons ev wff nil) *current-edag*)
      evars)))

; assumes arguments are in eta-short form
(defun create-pattern-flexflex-same (ev args1 args2)
  (let ((bindvars nil)
	(rbindvars nil)
	(bindvars2 nil)
	(banned-projs (get ev 'banned-projs))
	(banned-projs2 nil)
	(wff nil)
	(ev2 nil)
	(rtp nil))
    (do ((tp (unabbreviated-type ev) (car tp)))
	((not (consp tp)) (setq rtp tp))
      (push (fresh-var (cdr tp) '|w|) bindvars))
    (setq rbindvars (reverse bindvars))
    (let ((j 0))
      (dotimes (i (length args1))
	(when (eq (nth i args1) (nth i args2))
	  (push (nth i rbindvars) bindvars2)
	  (when (member i banned-projs :test #'=)
	    (push j banned-projs2))
	  (incf j))))
    (setq bindvars2 (reverse bindvars2))
    (setq wff (new-applied-evar rtp bindvars2 '|h| (get ev 'banned-sel-vars)))
    (setq ev2 (head wff))
    (setf (get ev2 'banned-projs) (reverse banned-projs2))
    (setf (get ev2 'banned-forall) (get ev 'banned-forall))
    (setf (get ev2 'banned-exists) (get ev 'banned-exists))
    (setf (get ev2 'banned-imitations) (get ev 'banned-imitations))
    (dolist (bv bindvars)
      (setq wff (acons bv 'LAMBDA wff)))
    (values wff ev2)))

; assumes arguments are in eta-short form
(defun create-pattern-flexflex-diff (ev1 ev2 args1 args2)
  (let ((bindvars1 nil)
	(bindvars2 nil)
	(rbindvars1 nil)
	(rbindvars2 nil)
	(args3 nil)
	(args4 nil)
	(banned-projs1 (get ev1 'banned-projs))
	(banned-projs2 (get ev2 'banned-projs))
	(banned-projs3 nil)
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
    (let ((j 0))
      (dotimes (i (length args1))
	(let ((k (position (nth i args1) args2)))
	  (when k
	    (push (nth i rbindvars1) args3)
	    (push (nth k rbindvars2) args4)
	    (when (or (member i banned-projs1 :test #'=)
		      (member k banned-projs2 :test #'=))
	      (push j banned-projs3))
	    (incf j)))))
    (dolist (arg args3)
      (setq rtp (cons rtp (unabbreviated-type arg))))
    (let ((ev3 (fresh-var rtp '|h|)))
      (setq wff3 ev3)
      (setq wff4 ev3)
      (setf (get ev3 'ext-exp-var) t)
      (setf (get ev3 'banned-sel-vars)
	    (union (get ev1 'banned-sel-vars)
		   (get ev2 'banned-sel-vars)))
      (setf (get ev3 'banned-projs) (reverse banned-projs3))
      (setf (get ev3 'banned-forall)
	    (union (get ev1 'banned-forall)
		   (get ev2 'banned-forall)))
      (setf (get ev3 'banned-exists)
	    (union (get ev1 'banned-exists)
		   (get ev2 'banned-exists)))
      (setf (get ev3 'banned-imitations)
	    (union (get ev1 'banned-imitations)
		   (get ev2 'banned-imitations)))
      (dolist (arg (reverse args3))
	(setq wff3 (cons wff3 arg)))
      (dolist (arg (reverse args4))
	(setq wff4 (cons wff4 arg)))
      (dolist (bv bindvars1)
	(setq wff3 (acons bv 'LAMBDA wff3)))
      (dolist (bv bindvars2)
	(setq wff4 (acons bv 'LAMBDA wff4)))
      (values wff3 wff4 ev3))))

(defun eeod-get-earliest (nodes)
  (let ((earliest (car nodes)))
    (dolist (node (cdr nodes) earliest)
      (when (< (ext-exp-open-dag-stamp node) (ext-exp-open-dag-stamp earliest))
	(setq earliest node)))))

(defun ext-imitation-possible-p (wff)
  (let ((h1 (head (cdar wff)))
	(h2 (head (cdr wff))))
    (if (and (ext-exp-var-p h1) (not (ext-exp-var-p h2)))
	(ext-imitation-heads-possible-p h1 h2)
      (and (not (ext-exp-var-p h1)) (ext-exp-var-p h2)
	   (ext-imitation-heads-possible-p h2 h1)))))

; h1 - expvar, h2 - sel-var or const
(defun ext-imitation-heads-possible-p (h1 h2)
  (and (not (member h2 (get h1 'banned-imitations)))
       (not (member h2 (get h1 'banned-sel-vars)))))

(defun ext-possible-projections (tp &optional arg-ret-types)
  (if (consp tp)
      (do ((tp2 (cdr tp) (car tp2)))
	  ((not (consp tp2))
	   (ext-possible-projections (car tp)
				     (cons tp2 arg-ret-types))))
    (let ((posl nil)
	  (i 0))
      (dolist (tp2 (reverse arg-ret-types) posl)
	(when (eq tp tp2)
	  (push i posl))
	(incf i)))))

; assumes h1 is flex head and h2 is rigid head
(defun exists-imit-or-proj-p (h1 h2)
  (or (ext-imitation-heads-possible-p h1 h2)
      (ext-possible-var-projections h1)))

(defun ext-possible-var-projections (h)
  (set-difference (ext-possible-projections (unabbreviated-type h)) (get h 'banned-projs)
		  :test #'=))

(defun ext-possible-var-not-projections (h)
  (set-difference (ext-possible-projections (unabbreviated-type h)) (get h 'banned-not-projs)
		  :test #'=))

(defun ext-projection-possible-p (wff)
  (let ((h1 (head (cdar wff)))
	(h2 (head (cdr wff))))
    (or (and (ext-exp-var-p h1)
	     (ext-possible-var-projections h1))
	(and (ext-exp-var-p h2)
	     (ext-possible-var-projections h2)))))

(defun ext-rigid-occurs-check-p (ev wff)
  (if (boundwff-p wff)
      (ext-rigid-occurs-check-p ev (cdr wff))
    (if (not-p wff)
	(ext-rigid-occurs-check-p ev (cdr wff))
      (let ((h (head wff)))
	(if (eq h ev)
	    t
	  (if (ext-exp-var-p h)
	      nil
	    (ext-rigid-occurs-check-p-1 ev wff)))))))

(defun ext-rigid-occurs-check-p-1 (ev wff)
  (if (consp wff)
      (or (ext-rigid-occurs-check-p-1 ev (car wff))
	  (ext-rigid-occurs-check-p ev (cdr wff)))
    nil))

(defun purely-propositional-tp (tp)
  (or (eq tp 'O)
      (and (consp tp)
	   (purely-propositional-tp (cdr tp))
	   (purely-propositional-tp (car tp)))))

(defun nth-primsub-type (n k ind-types)
  (if (< n k)
      (nth n ind-types)
    (let ((r (- n k)))
      (if (evenp r)
	  (cons 'O (nth-primsub-type (/ r 2) k ind-types))
	(let ((b (+ (/ (- r 1) 2) 1))
	      (c 0))
	  (loop while (evenp b) do
		(incf c)
		(setq b (/ b 2)))
	  (cons (nth-primsub-type (/ (- b 1) 2) k ind-types) (nth-primsub-type c k ind-types)))))))

(defun next-primsub-type (ind-types used-types)
  (if ind-types
      (let ((k (length ind-types)))
	(do* ((n 0 (1+ n))
	      (qtp (nth-primsub-type n k ind-types)
		   (nth-primsub-type n k ind-types)))
	    ((not (member qtp used-types :test #'equal))
	     qtp)))
    nil))

(defun ext-saturation-search (wff)
  (create-ext-exp-open-dag wff)
  (ext-saturation-search-1))

(defun ext-saturation-search-1 ()
  (let ((b (ext-saturation-search-2))
	(LAMBDA-CONV 'BETA-ETA-TOGETHER)
	(REWRITE-EQUALITIES 'only-ext)
	(REWRITE-DEFNS '(lazy1)))
    (declare (special LAMBDA-CONV REWRITE-EQUALITIES REWRITE-DEFNS))
    (case b
      (SUCCESS
       (breakcount 'mating)
       (runcount 'mating-ctr)
       (breakcount 'mating-ctr)
       (display-time 'mating)
       (runcount 'mating)
       t)
      (t
       (breakcount 'mating)
       (runcount 'mating-ctr)
       (breakcount 'mating-ctr)
       (display-time 'mating)
       (runcount 'mating)
       nil))))

(defun ext-saturation-search-2 ()
  (startcount 'mating)
  (let* ((bd-p (integerp ext-search-limit))
	 (ext-selvar-arcs (eeod-sel-var-arcs *current-edag*))
	 (ext-level 1)
	 (ext-options (if MAX-SEARCH-LIMIT
			  nil
			(list (list 'CHANGE-DUPS nil MS03-WEIGHT-CHANGE-DUPS 1))))
	 (ext-search-done nil)
	 (ext-next-proj 0)
	 (ext-next-not-proj 0)
	 (ext-next-forall 0)
	 (ext-next-exists 0)
	 (ext-next-and 0)
	 (ext-next-or 0)
	 (ext-next-equals 0)
	 (ext-next-not-equals 0)
	 (ext-set-vars-constraints nil)
	 (ext-next-dup 2)
	 (ext-dup-arc-order nil)
	 (ext-crucial-arcs nil) ; ie, if the parent exp node is there, this arc should be
	 (ext-all-exps nil)
	 (ext-min-exps nil)
	 (ext-min-lemmas nil)
	 (ext-min-exp-arcs nil)
	 (ext-all-lemmas nil) ; list of (<lemma-names> <pos eeod node> <eeo arcs> <neg ftree for lemmas> <clist for lemmas>)
	 (ext-used-options nil)
	 (ext-considered-set-arcs nil)
	 (ext-num-simul-set-arcs 1)
	 (ext-generated-primsubs nil)
	 (ext-flag-constraints nil)
	 (ext-option-stop-time
	  (when MAX-SEARCH-LIMIT
	    (+ (get-universal-time) MAX-SEARCH-LIMIT)))
	 )
    (declare (special ext-level ext-search-done ext-options ext-all-lemmas ext-min-lemmas
		      ext-next-proj ext-next-not-proj ext-next-forall ext-next-exists ext-used-options
		      ext-next-or ext-next-and ext-next-equals ext-next-not-equals
		      ext-set-vars-constraints ext-considered-set-arcs ext-num-simul-set-arcs
		      ext-selvar-arcs ext-crucial-arcs
		      ext-generated-primsubs
		      ext-next-dup ext-option-stop-time
		      ext-all-exps ext-min-exps ext-min-exp-arcs ext-dup-arc-order
		      ext-flag-constraints
		      ))
    (dolist (node (eeod-get-nodes #'(lambda (x) (eq (ext-exp-open-dag-kind x) 'EXP)) *current-edag*))
      (let ((z (cons node (ext-exp-open-dag-arcs node))))
	(setq ext-min-exp-arcs (append (cdr z) ext-min-exp-arcs))
	(push z ext-min-exps)
	(push z ext-all-exps)))
    (if MS03-USE-JFORMS
	(loop until (or ext-search-done (and bd-p (> ext-level ext-search-limit))) do
	      (ext-saturation-search-jforms))
      (loop until (or ext-search-done (and bd-p (> ext-level ext-search-limit))) do
	    (ext-saturation-search-no-jforms)))
    (runcount 'mating)
    (when (and query-user (not *edag-lift-info*))
      (msgf "Ending Search")
      (lift-eed-to-saturation-flag-suggest ext-flag-constraints))
    ext-search-done))

(defun ext-saturation-search-no-jforms ()
  (declare (special ext-search-done ext-level ext-option-stop-time))
  (if (ext-exp-open-dag-complete *current-edag*)
      (progn
	(eeod-solve-flex-flex)
	(setq ext-search-done 'success)) ; success
    (progn
      (when (and MAX-SEARCH-LIMIT (> (get-universal-time) ext-option-stop-time))
	(ext-saturation-full-exps)
	(ext-saturation-change-dups)
	(ext-saturation-min-exps)
	(setq ext-option-stop-time (+ (get-universal-time) MAX-SEARCH-LIMIT)))
      (when ms03-verbose
	(msgf "-------------- LEVEL: " ext-level " ---------------" t))
      (ext-saturation-search-eeod-options *current-edag*)
      (ext-saturation-search-do)
      (incf ext-level))))

(defun ext-saturation-search-jforms ()
  (declare (special *ext-rigid-jforms* *ext-jform* ext-search-done ext-level ext-option-stop-time ext-min-exp-arcs))
  (when (and MAX-SEARCH-LIMIT (> (get-universal-time) ext-option-stop-time))
    (ext-saturation-full-exps)
    (ext-saturation-change-dups)
    (ext-saturation-min-exps)
    (setq ext-option-stop-time (+ (get-universal-time) MAX-SEARCH-LIMIT)))
  (setq *ext-rigid-jform* (eeod-to-jform *current-edag* :posflex nil :negflex nil :flexflex nil))
  (if (empty-disjunction *ext-rigid-jform*)
      (progn
	(setq *ext-jform* (eeod-to-jform *current-edag* :posflex t :negflex t :flexflex nil))
	(if (empty-disjunction *ext-jform*)
	    (progn
	      (eeod-solve-flex-flex)
	      (setq ext-search-done 'success)) ; success!
	  (progn
	    (when ms03-verbose
	      (msgf "-------------- LEVEL: " ext-level " ---------------" t)
	      (display-vp-diag *ext-jform*))
	    (ext-saturation-search-options *ext-jform*)
	    (when MS03-USE-SET-CONSTRAINTS
	      (ext-saturation-search-constraints *ext-jform*))
	    (ext-saturation-search-do)
	    (incf ext-level))))
    (progn
      (when ms03-verbose
	(msgf "-------------- LEVEL: " ext-level " ---------------" t)
	(display-vp-diag *ext-rigid-jform*))
      (unless (and MS03-SOLVE-RIGID-PARTS
		   (< (mod ext-level 5) 4) ; to ensure completeness
		   (ext-saturation-solve-rigid *ext-rigid-jform*))
	(ext-saturation-search-options-rigid *ext-rigid-jform*)
	(ext-saturation-search-do))
      (incf ext-level))))

(defun print-ext-sat-option (op)
  (case (car op)
    ((MATE-RIGID MATE-FLEXRIGID EUNIF1 EUNIF2)
     (let* ((nodes (nth 3 op))
	    (node1 (car nodes))
	    (node2 (cadr nodes)))
       (if ms03-verbose
	   (msg (car op) " " (ext-exp-open-dag-name node1) " " ((ext-exp-open-dag-shallow node1) . gwff) t
		 " to " (ext-exp-open-dag-name node2) " " ((ext-exp-open-dag-shallow node2) . gwff) t)
	 (msg (car op) " " (ext-exp-open-dag-name node1) " to " (ext-exp-open-dag-name node2) t))))
    (IMIT
     (let ((ev (nth 3 op))
	   (h (nth 4 op)))
       (msg "Instantiate " (ev . gwff) " by Imitating " (h . gwff) t)))
    ((PROJ PRIMSUB-PROJ)
     (let ((ev (nth 3 op))
	   (i (nth 4 op)))
       (msg "Instantiate " (ev . gwff) " using " i " projection" t)))
    (PRIMSUB-NOT-PROJ
     (let ((ev (nth 3 op))
	   (i (nth 4 op)))
       (msg "Instantiate " (ev . gwff) " using negated " i " projection" t)))
    (PRIMSUB-FORALL
     (let ((ev (nth 3 op))
	   (tp (nth 4 op)))
       (msg "Primsub for " (ev . gwff) " using forall at type " tp t)))
    (PRIMSUB-EXISTS
     (let ((ev (nth 3 op))
	   (tp (nth 4 op)))
       (msg "Primsub for " (ev . gwff) " using exists at type " tp t)))
    (PRIMSUB-AND
     (let ((ev (nth 3 op)))
       (msg "Primsub for " (ev . gwff) " using AND." t)))
    (PRIMSUB-OR
     (let ((ev (nth 3 op)))
       (msg "Primsub for " (ev . gwff) " using OR." t)))
    (PRIMSUB-TRUTH
     (let ((ev (nth 3 op)))
       (msg "Primsub for " (ev . gwff) " using TRUTH." t)))
    (PRIMSUB-FALSEHOOD
     (let ((ev (nth 3 op)))
       (msg "Primsub for " (ev . gwff) " using FALSEHOOD." t)))
    (PRIMSUB-EQUALS
     (let ((ev (nth 3 op))
	   (q (nth 4 op)))
       (msg "Primsub for " (ev . gwff) " using = at type " (cdr (unabbreviated-type q)) t)))
    (PRIMSUB-NOT-EQUALS
     (let ((ev (nth 3 op))
	   (q (nth 4 op)))
       (msg "Primsub for " (ev . gwff) " using ~= at type " (cdr (unabbreviated-type q)) t)))
    (INST
     (let ((ev (nth 3 op))
	   (wff (nth 4 op)))
       (msg "Instantiate " (ev . gwff) " to " t
	     (wff . gwff) t)))
    (DUP-VAR-FOR-BANNED
     (let ((ev (nth 3 op))
	   (bv (nth 4 op)))
       (msg "Duplicate " (ev . gwff) " and instantion duplicate with " (bv . gwff) t)))
    (CHANGE-DUPS
     (msg "Change Duplicate Expansions to Consider." t))
    (t (msg (car op) " " (cdddr op)))))

(defun ext-saturation-solve-rigid (j)
  (declare (special ext-option-stop-time))
  (let ((done nil)
	(back-stack nil)
	(mating-etc nil)
	(theta nil)
	(dpairs nil)
	(incomp-choices nil)
	(next-action nil))
    (loop until done do
	  (if (and MAX-SEARCH-LIMIT (> (get-universal-time) ext-option-stop-time))
	      (setq done 'timedout)
	    (progn
	      (push (list mating-etc theta dpairs) back-stack)
	      (setq next-action (ext-saturation-solve-rigid-next-action j mating-etc theta dpairs
									incomp-choices))
	      (loop while next-action do
		    (case (car next-action)
		      (COMPLETE (setq done 'success next-action nil))
		      (REFL
		       (let* ((lit (cadr next-action))
			      (name (literal-name lit))
			      (wff (jform-represents lit))
			      (tp (cdr (unabbreviated-type (caar wff)))))
			 (push (list 'REFL (literal-name (cadr next-action))) mating-etc)
			 (setq next-action
			       (list 'UNIFY
				     (list (list tp (cdar wff) (cdr wff)))))))
		      (MATE
		       (let* ((lit1 (cadr next-action))
			      (name1 (literal-name lit1))
			      (wff1 (jform-represents lit1))
			      (lit2 (caddr next-action))
			      (name2 (literal-name lit2))
			      (wff2 (jform-represents lit2)))
			 (push (list 'MATE name1 name2) mating-etc)
			 (setq next-action
			       (list 'UNIFY (mapcar #'(lambda (x y)
							(list (unabbreviated-type x) x y))
						    (args wff1) (args wff2))))))
		      (EUNIF1
		       (let* ((lit1 (cadr next-action))
			      (name1 (literal-name lit1))
			      (wff1 (jform-represents lit1))
			      (lit2 (caddr next-action))
			      (name2 (literal-name lit2))
			      (wff2 (jform-represents lit2))
			      (tp (cdr (unabbreviated-type (caar wff1)))))
			 (push (list 'EUNIF1 name1 name2) mating-etc)
			 (setq next-action
			       (list 'UNIFY (list (list tp (cdar wff1) (cdar wff2))
						  (list tp (cdr wff1) (cdr wff2)))))))
		      (EUNIF2
		       (let* ((lit1 (cadr next-action))
			      (name1 (literal-name lit1))
			      (wff1 (jform-represents lit1))
			      (lit2 (caddr next-action))
			      (name2 (literal-name lit2))
			      (wff2 (jform-represents lit2))
			      (tp (cdr (unabbreviated-type (caar wff1)))))
			 (push (list 'EUNIF2 name1 name2) mating-etc)
			 (setq next-action
			       (list 'UNIFY (list (list tp (cdar wff1) (cdr wff2))
						  (list tp (cdr wff1) (cdar wff2)))))))
		      (UNIFY
		       (let ((new-dpairs (append (mapcar #'(lambda (d)
							     (list (car d)
								   (etanorm (lambda-norm
									     (simul-substitute-l-term-var theta (cadr d))))
								   (etanorm (lambda-norm
									     (simul-substitute-l-term-var theta (caddr d))))))
							 (cadr next-action))
						 dpairs)))
			 (setq next-action nil)
			 (multiple-value-bind
			     (success theta2 dpairs2)
			     (ext-saturation-quick-unify new-dpairs theta)
			   (if success
			       (setq theta theta2 dpairs dpairs2)
			     (progn
			       (push mating-etc incomp-choices)
			       (if back-stack
				   (let ((info (pop back-stack)))
				     (setq mating-etc (car info)
					   theta (cadr info)
					   dpairs (caddr info)))
				 (setq theta nil dpairs nil mating-etc nil)))))))
		      (t (setq next-action nil done 'failed)))))))
    (if (eq done 'success)
	(progn
	  (when ms03-verbose
	    (msgf "Step: Solving Rigid Part Using " t)
	    (dolist (m mating-etc)
	      (case (car m)
		((MATE EUNIF1 EUNIF2) (msgf (cadr m) " . " (caddr m)))))
	    (dolist (p theta)
	      (msgf ((car p) . gwff) " |-> " ((cdr p) . gwff))))
					; temporarily put all expansions back in while we modify the (full) edag
	  (ext-saturation-full-exps)
	  (let ((ev-assoc (eeod-duplicate-and-lift-subst theta)))
	    (dolist (m mating-etc)
	      (case (car m)
		(MATE
		 (let ((node1 (name-to-eeod (cadr m)))
		       (node2 (name-to-eeod (caddr m))))
		   (unless (eeod-mated-p node1 node2)
		     (if (wffeq-ab (ext-exp-open-dag-shallow node1)
				   (ext-exp-open-dag-shallow node2))
			 (eeod-generalized-connection-1 node1 node2)
		       (eeod-mate node1 node2)))))
		(EUNIF1
		 (let ((node1 (name-to-eeod (cadr m)))
		       (node2 (name-to-eeod (caddr m))))
		   (unless (eeod-eunif1-p node1 node2)
		     (if (wffeq-ab (ext-exp-open-dag-shallow node1)
				   (ext-exp-open-dag-shallow node2))
			 (eeod-generalized-connection-1 node1 node2)
		       (eeod-eunif1 node1 node2)))))
		(EUNIF2
		 (let ((eunif (eeod-eunif2 (name-to-eeod (cadr m))
					   (name-to-eeod (caddr m)))))
		   (when eunif
		     (let* ((conjs (ext-exp-open-dag-kids eunif))
			    (conj1 (car conjs))
			    (conj2 (cadr conjs))
			    (eq1 (ext-exp-open-dag-shallow conj1))
			    (eq2 (ext-exp-open-dag-shallow conj2)))
		       (when (and (equals-p eq1)
				  (wffeq-ab (cdar eq1) (cdr eq1)))
			 (eeod-generalized-refl-1 conj1))
		       (when (and (equals-p eq2)
				  (wffeq-ab (cdar eq2) (cdr eq2)))
			 (eeod-generalized-refl-1 conj2))))))))
	    (remove-bad-ext-options (mapcar #'car ev-assoc))
	    (move-ext-options-to-duplicates ev-assoc))
					; go back to smaller edag
	  (ext-saturation-min-exps)
	  t)
      nil)))

(defun ext-saturation-quick-unify (dpairs theta)
  (runcount 'unification)
  (let ((changed t)
	(failed nil)
	(steps 0))
    (loop while (and (not failed) changed (< steps MS03-QUICK-EUNIFICATION-LIMIT)) do
	  (setq changed nil)
	  (let ((same nil)
		(decs nil)
		(funcs nil)
		(flexrigids nil)
		(rigidflexs nil))
	    (dolist (d dpairs)
	      (if (wffeq-ab (cadr d) (caddr d))
		  (push d same)
		(if (consp (car d))
		    (push d funcs)
		  (let ((h1 (head (cadr d)))
			(h2 (head (caddr d))))
		    (if (ext-exp-var-p h1)
			(unless (ext-exp-var-p h2)
			  (push d flexrigids))
		      (if (ext-exp-var-p h2)
			  (push d rigidflexs)
			(if (eq h1 h2) ; same, decompose
			    (push d decs)
			  (setq failed t))))))))
	    (unless failed
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
			 (fh (head flex))
			 (rh (head rig))
			 (bound (cadddr (car fs))))
		    (if (ext-rigid-occurs-check-p fh rig)
			(setq failed t)
		      (if (and (ext-imitation-heads-possible-p fh rh) (not (member rh bound)))
			  (unless (ext-possible-var-projections fh)
			    (multiple-value-bind
				(wff evars)
				(create-imit-subst fh rh)
			      (setq dpairs
				    (mapcar #'(lambda (d)
						(list (car d)
						      (lambda-norm (substitute-l-term-var wff fh (cadr d)))
						      (lambda-norm (substitute-l-term-var wff fh (caddr d)))
						      (cadddr d)))
					    dpairs))
			      (setq theta
				    (cons (cons fh wff) 
					  (mapcar #'(lambda (d)
						      (cons (car d)
							    (substitute-l-term-var wff fh (cdr d))))
						  theta))))
			    (incf steps)
			    (setq changed t))
			(let ((projs (ext-possible-var-projections fh)))
			  (if projs
			      (when (= (length projs) 1)
				(multiple-value-bind
				    (wff evars)
				    (create-proj-subst fh (car projs))
				  (setq dpairs
					(mapcar #'(lambda (d)
						    (list (car d)
							  (lambda-norm (substitute-l-term-var wff fh (cadr d)))
							  (lambda-norm (substitute-l-term-var wff fh (caddr d)))
							  (cadddr d)))
						dpairs))
				  (setq theta (cons (cons fh wff) 
						    (mapcar #'(lambda (d)
								(cons (car d)
								      (substitute-l-term-var wff fh (cdr d))))
							    theta))))
				(incf steps)
				(setq changed t))
			    (setq failed t)))))))
		(do ((fs rigidflexs (cdr fs)))
		    ((or changed (null fs)))
		  (let* ((flex (caddar fs))
			 (rig (cadar fs))
			 (fh (head flex))
			 (rh (head rig))
			 (bound (cadddr (car fs))))
		    (if (ext-rigid-occurs-check-p fh rig)
			(setq failed t)
		      (if (and (ext-imitation-heads-possible-p fh rh) (not (member rh bound)))
			  (unless (ext-possible-var-projections fh)
			    (multiple-value-bind
				(wff evars)
				(create-imit-subst fh rh)
			      (setq dpairs
				    (mapcar #'(lambda (d)
						(list (car d)
						      (lambda-norm (substitute-l-term-var wff fh (cadr d)))
						      (lambda-norm (substitute-l-term-var wff fh (caddr d)))
						      (cadddr d)))
					    dpairs))
			      (setq theta (cons (cons fh wff) 
						(mapcar #'(lambda (d)
							    (cons (car d)
								  (substitute-l-term-var wff fh (cdr d))))
							theta))))
			    (incf steps)
			    (setq changed t))
			(let ((projs (ext-possible-var-projections fh)))
			  (if projs
			      (when (= (length projs) 1)
				(multiple-value-bind
				    (wff evars)
				    (create-proj-subst fh (car projs))
				  (setq dpairs
					(mapcar #'(lambda (d)
						    (list (car d)
							  (lambda-norm (substitute-l-term-var wff fh (cadr d)))
							  (lambda-norm (substitute-l-term-var wff fh (caddr d)))
							  (cadddr d)))
						dpairs))
				  (setq theta (cons (cons fh wff) 
						    (mapcar #'(lambda (d)
								(cons (car d)
								      (substitute-l-term-var wff fh (cdr d))))
							    theta))))
				(incf steps)
				(setq changed t))
			    (setq failed t)))))))))))
    (breakcount 'unification)
    (if failed
	nil
      (values t theta dpairs))))

(defun ext-saturation-solve-rigid-next-action (j mating-etc theta dpairs incomp-choices)
  (multiple-value-bind
      (complete path)
      (ext-cheapest-open-path j mating-etc)
    (if complete
	(list 'COMPLETE)
      (let ((next-action nil))
	(do ((lits1 path (cdr lits1)))
	    ((or (null lits1) next-action)
	     (unless next-action
	       (setq next-action '(FAIL))))
	  (let* ((lit1 (car lits1))
		 (name1 (literal-name lit1))
		 (p1 (jform-pos lit1))
		 (wff1 (jform-represents lit1)))
	    (when (and (not p1)
		       (equals-p wff1)
		       (not (ext-saturation-solve-rigid-incompatible-p (cons (list 'REFL name1) mating-etc) incomp-choices)))
	      (setq next-action (list 'REFL lit1)))
	    (unless next-action
	      (do ((lits2 (cdr lits1) (cdr lits2)))
		  ((or (null lits2) next-action))
		(let* ((lit2 (car lits2))
		       (name2 (literal-name lit2))
		       (p2 (jform-pos lit2))
		       (wff2 (jform-represents lit2)))
		  (when (and (not (equal p1 p2)) (eq (head wff1) (head wff2))
			     (or MS03-SOLVE-RIGID-PARTS-ALLOW-RECONNECTS
				 (not (eeod-connected-p (get name1 'ext-exp-open-dag)
							(get name2 'ext-exp-open-dag)))))
		    (if (equals-p wff1)
			(if (ext-saturation-solve-rigid-incompatible-p (cons (if p1
										 (list 'EUNIF1 name1 name2)
									       (list 'EUNIF1 name2 name1))
									     mating-etc)
								       incomp-choices)
			    (unless (ext-saturation-solve-rigid-incompatible-p (cons (if p1
											 (list 'EUNIF2 name1 name2)
										       (list 'EUNIF2 name2 name1))
										     mating-etc)
									       incomp-choices)
			      (setq next-action (if p1 (list 'EUNIF2 lit1 lit2) (list 'EUNIF2 lit2 lit1))))
			  (setq next-action (if p1 (list 'EUNIF1 lit1 lit2) (list 'EUNIF1 lit2 lit1))))
		      (unless (ext-saturation-solve-rigid-incompatible-p (cons (if p1
										   (list 'MATE name1 name2)
										 (list 'MATE name2 name1))
									       mating-etc)
									 incomp-choices)
			(setq next-action (if p1 (list 'MATE lit1 lit2) (list 'MATE lit2 lit1)))))))))))
	next-action))))

(defun ext-saturation-solve-rigid-incompatible-p (mating-etc incomp-choices)
  (find-if #'(lambda (x)
	       (subsetp x mating-etc :test #'equal))
	   incomp-choices))

(defun ext-cheapest-open-path (j mating-etc &optional conjs path)
  (case (jform-type j)
    (literal
     (let ((name (literal-name j))
	   (p (jform-pos j)))
       (if (or (find-if #'(lambda (x)
			    (and (eq (car x) 'REFL) (eq (cadr x) name)))
			mating-etc)
	       (find-if #'(lambda (y)
			    (find-if #'(lambda (x)
					 (and (neq (car x) 'REFL)
					      (or (and p (eq (cadr x) name) (eq (caddr x) (literal-name y)))
						  (and (not p) (eq (caddr x) name) (eq (cadr x) (literal-name y))))))
				     mating-etc))
			path))
	   (values t nil)
	 (if conjs
	     (ext-cheapest-open-path (car conjs) mating-etc (cdr conjs)
				     (cons j path))
	   (values nil (cons j path))))))
    (disjunction
     (let ((path2 nil))
       (do ((jl (disjunction-components j) (cdr jl)))
	   ((or (null jl) path2)
	    (if path2
		(values nil path2)
	      (values t nil)))
	 (multiple-value-bind
	     (complete path3)
	     (ext-cheapest-open-path (car jl) mating-etc conjs path)
	   (unless complete
	     (setq path2 path3))))))
    (conjunction
     (if (conjunction-components j)
	 (ext-cheapest-open-path (car (conjunction-components j)) mating-etc
				 (append (cdr (conjunction-components j)) conjs)
				 path)
       (if conjs
	   (ext-cheapest-open-path (car conjs) mating-etc (cdr conjs) path)
	 (values nil path))))
    (universal
     (ext-cheapest-open-path (universal-scope j) mating-etc conjs path))
    (existential
     (ext-cheapest-open-path (existential-scope j) mating-etc conjs path))))

(defun ext-saturation-search-do ()
  (declare (special ext-level ext-options ext-min-exp-arcs ext-flag-constraints))
  (when *edag-lift-info*
    (let ((novals nil))
      (setq ext-options
	    (remove-if-not
	     #'(lambda (x)
		 (let ((b (ext-saturation-valid-lift x)))
		   (prog1
		       b
		     (unless b
		       (push (ext-saturation-option-flag-info x) novals)
		       (when ms03-verbose
			 (msgf "Deleting Option ")
			 (print-ext-sat-option x))))))
	     ext-options))
      (dolist (f novals)
	(when f
	  (push (list 'MAX f) (get *edag-lift-info* 'flag-constraints))))))
  (let ((ext-options-now
	 (remove-if-not
	  #'(lambda (x)
	      (subsetp (cadr x) ext-min-exp-arcs))
	  ext-options)))
    (when (and query-user (cdr ext-options-now))
      (let ((l (length ext-options-now))
	    (start 0)
	    (chosen nil)
	    (ch nil))
	(when (or (and (eq query-user 'query-jforms) (not max-search-limit) 
		       (not (query "Search On This JForm?" t)))
		  (and (eq query-user 'show-jforms) (not max-search-limit)))
	  (let ((f (find-if #'(lambda (op) (eq (car op) 'CHANGE-DUPS))
			    ext-options-now)))
	    (when f (setq chosen t
			  ch (1+ (position f ext-options-now))))))
	(loop until chosen do
	      (let* ((d (- l start))
		     (m (min 10 d)))
		(if (> start 0)
		    (msgf "Next " m " Possible Steps:" t)
		  (msgf "Top " m " Next Possible Steps:" t))
		(dotimes (i m)
		  (let ((op (nth (+ start i) ext-options-now)))
		    (msgf (1+ i) " > ")
		    (print-ext-sat-option op)))
		(when (> d 10)
		  (msgf "11 > More Options"))
		(msgf "Choose a number:")
		(setq ch (get-a-number (if (> l 10) 11 m) 1))
		(if (= ch 11)
		    (setq start (+ start 10))
		  (progn
		    (setq chosen t ch (+ ch start))))))
	(unless (= ch 1)
	  (let* ((op (nth (- ch 1) ext-options-now)))
	    (setq ext-options (remove op ext-options-now))
	    (push op ext-options-now)))))
    (when (and *edag-lift-info* (cdr ext-options-now))
      (let ((x (ext-saturation-option-flag-info (car ext-options-now)))
	    (y (ext-saturation-option-flag-info (cadr ext-options-now))))
	(when (and x y)
	  (push (list '< x y)
		(get *edag-lift-info* 'flag-constraints)))))
    (when (and (not *edag-lift-info*) query-user (cdr ext-options-now))
      (let ((x (ext-saturation-option-flag-info (car ext-options-now)))
	    (y (ext-saturation-option-flag-info (cadr ext-options-now))))
	(when (and x y)
	  (push (list '< x y) ext-flag-constraints))))
    (ext-saturation-search-do-1 ext-options-now)))

(defun ext-saturation-search-do-1 (ops)
  (declare (special ext-level ext-options ext-search-done ext-option-stop-time ext-next-dup))
  (if ops
      (let* ((op (car ops))
	     (c (car op))
	     (inst-evs nil)
	     (ev-assoc nil)
	     (w (caddr op)))
	(setq ext-options (remove op ext-options))
	(when ms03-verbose
	  (msgf "Step: ") (print-ext-sat-option op))
					; temporarily put all expansions back in while we modify the (full) edag
	(ext-saturation-full-exps)
	(case c
	  (MATE-RIGID
	   (let ((nodes (nth 3 op))
		 (insts (nth 5 op)))
	     (eeod-mate (car nodes) (cadr nodes))
	     (setq ev-assoc (ext-saturation-search-do-more-unifs insts nil))))
	  (MATE-FLEXRIGID
	   (let* ((nodes (nth 3 op))
		  (insts (nth 5 op))
		  (evl (nth 6 op))
		  (evla nil)
		  (flexnode (car nodes))
		  (rignode (cadr nodes))
		  (flexname (ext-exp-open-dag-name flexnode))
		  (rig (ext-exp-open-dag-shallow rignode))
		  (flex (ext-exp-open-dag-shallow flexnode))
		  (inode nil)
		  (ev (head flex))
		  (h (head rig)))
	     (runcount 'unification)
	     (if (eq (ext-exp-open-dag-positive rignode)
		     (ext-exp-open-dag-positive flexnode))
		 (multiple-value-bind
		     (ev2 ev4 ev3 evars4)
		     (eeod-duplicate-and-imitate-not ev h)
		   (setq ev-assoc (list (cons ev ev2)))
		   (setq evla (mapcar #'cons evars4 evl))
		   (setq inode
			 (ext-exp-open-arc-node
			  (car (ext-exp-open-dag-arcs
				(name-to-eeod flexname))))))
	       (multiple-value-bind
		   (ev2 evars2)
		   (eeod-duplicate-and-imitate ev h)
		 (setq evla (mapcar #'cons evars2 evl))
		 (setq ev-assoc (list (cons ev ev2)))
		 (setq inode (name-to-eeod flexname))))
	     (breakcount 'unification)
	     (if (ext-exp-open-dag-positive inode)
		 (eeod-mate inode rignode)
	       (eeod-mate rignode inode))
	     (setq ev-assoc (append ev-assoc
				    (ext-saturation-search-do-more-unifs insts evla)))))
	  (EUNIF1
	   (let ((nodes (nth 3 op))
		 (insts (nth 5 op)))
	     (eeod-eunif1 (car nodes) (cadr nodes))
	     (setq ev-assoc (ext-saturation-search-do-more-unifs insts nil))))
	  (EUNIF2
	   (let ((nodes (nth 3 op))
		 (insts (nth 5 op)))
	     (eeod-eunif2 (car nodes) (cadr nodes))
	     (setq ev-assoc (ext-saturation-search-do-more-unifs insts nil))))
	  (IMIT
	   (let ((ev (nth 3 op))
		 (h (nth 4 op))
		 (insts (nth 5 op))
		 (evl (nth 6 op)))
	     (runcount 'unification)
	     (multiple-value-bind
		 (ev2 evars2)
		 (eeod-duplicate-and-imitate ev h)
	       (breakcount 'unification)
	       (setq ev-assoc (cons
			       (cons ev ev2)
			       (ext-saturation-search-do-more-unifs insts (mapcar #'cons evars2 evl)))))))
	  (INST
	   (let* ((ev (nth 3 op))
		  (wff (nth 4 op))
		  (ev2 (eeod-duplicate-expvar ev)))
	     (setf (get ev2 'banned-abstract-formulas)
		   (cons (ms03-abstract-formula wff) (get ev 'banned-abstract-formulas)))
	     (setf (get ev2 'banned-forall) (get ev 'banned-forall))
	     (setf (get ev2 'banned-exists) (get ev 'banned-exists))
	     (setf (get ev2 'banned-imitations) (get ev 'banned-imitations))
	     (setf (get ev2 'banned-projs) (get ev 'banned-projs))
	     (push (cons ev ev2) ev-assoc)
	     (eeod-subst-deepen (acons ev wff nil) *current-edag*)
	     ))
	  (PROJ
	   (let ((ev (nth 3 op))
		 (i (nth 4 op))
		 (insts (nth 5 op))
		 (evl (nth 6 op)))
	     (runcount 'unification)
	     (multiple-value-bind
		 (ev2 evars2)
		 (eeod-duplicate-and-project ev i)
	       (setq ev-assoc (list (cons ev ev2)))
	       (breakcount 'unification)
	       (setq ev-assoc (ext-saturation-search-do-more-unifs insts (mapcar #'cons evars2 evl))))))
	  (PRIMSUB-PROJ
	   (let ((ev (nth 3 op))
		 (i (nth 4 op)))
	     (runcount 'unification)
	     (multiple-value-bind
		 (ev2 evars2)
		 (eeod-duplicate-and-project ev i)
	       (setq ev-assoc (list (cons ev ev2)))
	       (breakcount 'unification))))
	  (PRIMSUB-NOT-PROJ
	   (let ((ev (nth 3 op))
		 (i (nth 4 op)))
	     (runcount 'unification)
	     (multiple-value-bind
		 (ev2 evars2)
		 (eeod-duplicate-and-project-not ev i)
	       (setq ev-assoc (list (cons ev ev2)))
	       (breakcount 'unification))))
	  (PRIMSUB-FORALL
	   (let* ((ev (nth 3 op))
		  (tp (nth 4 op))
		  (ev2 (eeod-duplicate-and-subst-forall ev tp)))
	     (setq ev-assoc (list (cons ev ev2)))))
	  (PRIMSUB-EXISTS
	   (let* ((ev (nth 3 op))
		  (tp (nth 4 op))
		  (ev2 (eeod-duplicate-and-subst-exist ev tp)))
	     (setq ev-assoc (list (cons ev ev2)))))
	  (PRIMSUB-AND
	   (let* ((ev (nth 3 op))
		  (ev2 (eeod-duplicate-and-imitate ev 'AND)))
	     (setq ev-assoc (list (cons ev ev2)))))
	  (PRIMSUB-OR
	   (let* ((ev (nth 3 op))
		  (ev2 (eeod-duplicate-and-imitate ev 'OR)))
	     (setq ev-assoc (list (cons ev ev2)))))
	  (PRIMSUB-TRUTH
	   (let* ((ev (nth 3 op))
		  (ev2 (eeod-duplicate-and-imitate ev 'TRUTH)))
	     (setq ev-assoc (list (cons ev ev2)))))
	  (PRIMSUB-FALSEHOOD
	   (let* ((ev (nth 3 op))
		  (ev2 (eeod-duplicate-and-imitate ev 'FALSEHOOD)))
	     (setq ev-assoc (list (cons ev ev2)))))
	  (PRIMSUB-EQUALS
	   (let* ((ev (nth 3 op))
		  (q (nth 4 op))
		  (ev2 (eeod-duplicate-and-imitate ev q)))
	     (setq ev-assoc (list (cons ev ev2)))))
	  (PRIMSUB-NOT-EQUALS
	   (let* ((ev (nth 3 op))
		  (q (nth 4 op)))
	     (multiple-value-bind
		 (ev2 ev4 ev3 evars4)
		 (eeod-duplicate-and-imitate-not ev q)
	       (setq ev-assoc (list (cons ev ev2))))))
	  (DUP-VAR-FOR-BANNED
	   (let ((ev (nth 3 op))
		 (bv (nth 4 op)))
	     (unless (member bv (get ev 'already-duped-for))
	       (let ((ev2 (eeod-duplicate-expvar ev)))
		 (push bv (get ev 'already-duped-for))
		 (unless (member bv (get ev2 'banned-sel-vars))
		   (eeod-subst-imit ev2 bv))))))
	  (CHANGE-DUPS
	   (ext-saturation-change-dups)
	   (unless ext-search-done
	     (setq ext-options
		   (merge 'list
			  (list (list 'CHANGE-DUPS nil (+ (* ext-next-dup MS03-WEIGHT-CHANGE-DUPS) ext-level) ext-next-dup))
			  ext-options
			  #'ext-options-order))
	     (incf ext-next-dup))))
					; remove destroyed options
	(setq inst-evs (mapcar #'car ev-assoc))
	(remove-bad-ext-options inst-evs)
					; move vars in options to new duplicates
	(move-ext-options-to-duplicates ev-assoc)
					; go back to smaller edag
	(ext-saturation-min-exps)
	(when *edag-lift-info* (update-edag-lift-info *current-edag*))
	)
    (if max-search-limit ; change dups
	(progn
	  (ext-saturation-full-exps)
	  (ext-saturation-change-dups)
	  (ext-saturation-min-exps))
      (setq ext-search-done 'failed)))) ; if no options and didn't time out, really fail

(defun remove-bad-ext-options (inst-evs)
  (declare (special ext-options))
  (setq ext-options
	(remove-if #'(lambda (x)
		       (or (and (eq (car x) 'MATE-FLEXRIGID)
				(find-if #'(lambda (y)
					     (neq (name-to-eeod (ext-exp-open-dag-name y))
						  y)) ; ie, node has changed
					 (nth 3 x)))
			   (and (member (car x) '(MATE-RIGID EUNIF1 EUNIF2))
				(find-if #'(lambda (more-unif)
					     (case (car more-unif)
					       ((IMIT PROJ PATTERN-FLEXFLEX-SAME)
						(member (cadr more-unif)
							inst-evs))
					       (PATTERN-FLEXFLEX-DIFF
						(or
						 (member (cadr more-unif)
							 inst-evs)
						 (member (caddr more-unif)
							 inst-evs)))
					       (t nil)))
					 (nth 5 x)))))
		   ext-options)))

(defun move-ext-options-to-duplicates (ev-assoc)
  (declare (special ext-options ext-set-vars-constraints))
  (setq ext-options
	(mapcar #'(lambda (x)
		    (if (member (car x)
				'(IMIT PROJ PRIMSUB-PROJ PRIMSUB-FORALL
				       PRIMSUB-EXISTS
				       PRIMSUB-AND
				       PRIMSUB-OR
				       PRIMSUB-TRUTH
				       PRIMSUB-FALSEHOOD
				       PRIMSUB-EQUALS
				       PRIMSUB-NOT-EQUALS
				       INST
				       ))
			(let ((a (assoc (nth 3 x) ev-assoc)))
			  (if a
			      (let ((arc-assoc (ext-dup-exp-arc-assoc (car a) (cdr a))))
				(cons (car x)
				      (cons (mapcar #'(lambda (arc)
							(or (cdr (assoc arc arc-assoc))
							    arc))
						    (cadr x))
					    (cons (caddr x)
						  (cons (cdr a) (cddddr x))))))
			    x))
		      x))
		ext-options))
  (dolist (eva ev-assoc)
    (when (member (car eva) ext-set-vars-constraints)
      (push (cdr eva) ext-set-vars-constraints)))
  (remove-bad-ext-options-substs))

(defun remove-bad-ext-options-substs ()
  (declare (special ext-options))
  (setq ext-options
	(remove-if #'(lambda (x)
		       (case (car x)
			 ((IMIT PRIMSUB-EQUALS) (member (nth 4 x) (get (nth 3 x) 'banned-imitations)))
			 ((PRIMSUB-PROJ PROJ) (member (nth 4 x) (get (nth 3 x) 'banned-projs) :test #'equal))
			 (PRIMSUB-FORALL (or (member (nth 4 x) (get (nth 3 x) 'banned-forall) :test #'equal)
					     (member 'ALL (get (nth 3 x) 'banned-forall))))
			 (PRIMSUB-EXISTS (or (member (nth 4 x) (get (nth 3 x) 'banned-exists) :test #'equal)
					     (member 'ALL (get (nth 3 x) 'banned-exists))))
			 (PRIMSUB-NOT-EQUALS (member 'NOT (get (nth 3 x) 'banned-imitations)))
			 (PRIMSUB-AND (member 'AND (get (nth 3 x) 'banned-imitations)))
			 (PRIMSUB-OR (member 'OR (get (nth 3 x) 'banned-imitations)))
			 (PRIMSUB-TRUTH (member 'TRUTH (get (nth 3 x) 'banned-imitations)))
			 (PRIMSUB-FALSEHOOD (member 'FALSEHOOD (get (nth 3 x) 'banned-imitations)))
			 (t nil)))
		   ext-options)))
  
(defun ext-dup-exp-arc-assoc (ev1 ev2)
  (let ((arc-assoc nil))
    (dolist (arc1 (get ev1 'ext-exp-var-arcs))
      (let* ((par (ext-exp-open-arc-parent arc1))
	     (arc2 (find-if #'(lambda (arc)
				(member arc (get ev2 'ext-exp-var-arcs)))
			    (ext-exp-open-dag-arcs par))))
	(when arc2
	  (push (cons arc1 arc2) arc-assoc))))
    arc-assoc))

(defun ext-saturation-search-do-more-unifs (insts ev-a)
  (setq ev-a (remove-if-not #'(lambda (x)
				(equal (unabbreviated-type (car x))
				       (unabbreviated-type (cdr x))))
			    ev-a))
  (let ((other-evars nil)
	(ev-assoc nil))
    (dolist (u insts)
      (case (car u)
	(IMIT
	 (let* ((ev1 (cadr u))
		(h (caddr u))
		(evars1 (cadddr u))
		(b (member ev1 other-evars))
		(ev2 (if b
			 (car (rassoc ev1 ev-a))
		       ev1)))
	   (setq other-evars (append evars1 other-evars))
	   (when (and ev2 (not (get ev2 'ext-exp-var-subst)))
	     (multiple-value-bind
		 (ev3 evars3)
		 (eeod-duplicate-and-imitate ev2 h)
	       (unless b
		 (push (cons ev2 ev3) ev-assoc))
	       (setq ev-a (append ev-a (mapcar #'cons evars3 evars1)))))))
	(PROJ
	 (let* ((ev1 (cadr u))
		(i (caddr u))
		(evars1 (cadddr u))
		(b (member ev1 other-evars))
		(ev2 (if b
			 (car (rassoc ev1 ev-a))
		       ev1)))
	   (setq other-evars (append evars1 other-evars))
	   (when (and ev2 (not (get ev2 'ext-exp-var-subst)))
	     (multiple-value-bind
		 (ev3 evars3)
		 (eeod-duplicate-and-project ev2 i)
	       (unless b
		 (push (cons ev2 ev3) ev-assoc))
	       (setq ev-a (append ev-a (mapcar #'cons evars3 evars1)))))))
;      (PATTERN-FLEXFLEX-SAME
;       (let* ((ev1 (cadr u))
;	      (wff (caddr u))
;	      (ev3 (nth 3 u))
;	      (ev2 (car (rassoc ev1 ev-a))))
;	 (when ev2
;	   (let ((d (eeod-duplicate-expvar ev2)))
;	     (setf (get d 'banned-forall) (get ev2 'banned-forall))
;	     (setf (get d 'banned-exists) (get ev2 'banned-exists))
;	     (setf (get d 'banned-imitations) (get ev2 'banned-imitations))
;	     (eeod-subst-deepen ev2 wff (list ev3))
;	     (push (cons ev3 ev3) ev-a)))))
;      (PATTERN-FLEXFLEX-DIFF
;       )
	(t nil)))
    ev-assoc))

; put back in all exp arcs and all lemmas
(defun ext-saturation-full-exps ()
  (declare (special ext-all-exps ext-all-lemmas ext-selvar-arcs))
  (when *current-edag-lemmas*
    (setq *current-edag* (cadr (ext-exp-open-dag-kids *current-edag*)))
    (setf (ext-exp-open-dag-parent-arcs *current-edag*) nil))
  (setq *current-edag-lemmas* nil)
  (dolist (exp-arcs ext-all-exps)
    (setf (ext-exp-open-dag-arcs (car exp-arcs)) (cdr exp-arcs)))
  (dolist (lemm ext-all-lemmas)
    (ext-exp-open-dag-add-lemma (car lemm) (cadr lemm) (nth 3 lemm) (nth 4 lemm)))
  (setq ext-selvar-arcs (eeod-sel-var-arcs *current-edag*))
  )

; reset ext-all-exps to remember the full dag, but then minimize to the ones currently being used
(defun ext-saturation-min-exps ()
  (declare (special ext-min-exps ext-all-exps ext-dup-arc-order ext-all-lemmas ext-min-lemmas
		    ext-min-exp-arcs ext-selvar-arcs))
  (let* ((selvars (mapcar #'car ext-selvar-arcs)))
    (setq ext-all-exps nil ext-dup-arc-order nil)
    (dolist (exp (eeod-get-nodes #'(lambda (x) (eq (ext-exp-open-dag-kind x) 'EXP)) *current-edag*))
      (let ((arcs (sort (copy-list (ext-exp-open-dag-arcs exp))
			#'(lambda (x y)
			    (< (ext-exp-open-dag-stamp (ext-exp-open-arc-node x))
			       (ext-exp-open-dag-stamp (ext-exp-open-arc-node y)))))))
	(push (cons exp arcs) ext-all-exps)
	(dolist (arc arcs)
	  (let* ((etrm (ext-exp-open-arc-exp-term arc))
		 (svl (intersection selvars (free-vars-of etrm)))
		 (earcs (eeod-exp-arcs-above-l (cons (ext-exp-open-arc-parent arc)
						     (mapcar #'(lambda (sv)
								 (ext-exp-open-arc-parent
								  (cdr (assoc sv ext-selvar-arcs))))
							     svl))))
		 (elems nil))
	    (dolist (lem ext-all-lemmas)
	      (when (or (intersection (caddr lem) earcs)
			(eeod-get-nodes
			 #'(lambda (node)
			     (intersection (ext-exp-open-dag-arcs node) earcs))
			 (cadr lem) nil))
		(push lem elems)))
	    (push (list arc earcs elems) ext-dup-arc-order)))))
    (setq ext-all-exps
	  (sort ext-all-exps
		#'(lambda (x y)
		    (< (ext-exp-open-dag-stamp (car x))
		       (ext-exp-open-dag-stamp (car y))))))
    (when *current-edag-lemmas*
      (setq *current-edag* (cadr (ext-exp-open-dag-kids *current-edag*)))
      (setf (ext-exp-open-dag-parent-arcs *current-edag*) nil))
    (setq *current-edag-lemmas* nil *current-edag-lemma-ftree-pfs* nil)
    (dolist (exp-arcs ext-min-exps)
      (setf (ext-exp-open-dag-arcs (car exp-arcs)) (cdr exp-arcs)))
    (dolist (lem ext-min-lemmas)
      (ext-exp-open-dag-add-lemma (car lem) (cadr lem) (nth 3 lem) (nth 4 lem))
      )
    )
  )

(defun ext-exp-open-dag-add-lemma (lemm node negf clist)
  (if *current-edag-lemmas*
      (let* ((arc (car (ext-exp-open-dag-arcs *current-edag*)))
	     (l (ext-exp-open-arc-node arc))
	     (conj (make-ext-exp-open-dag :positive t))
	     (arc1 (make-ext-exp-open-arc :parent conj :node node))
	     (arc2 (make-ext-exp-open-arc :parent conj :node l)))
	(setf (ext-exp-open-dag-shallow conj)
	      (acons 'AND
		     (ext-exp-open-dag-shallow node)
		     (ext-exp-open-dag-shallow l)))
	(setf (ext-exp-open-dag-shallow *current-edag*)
	      (acons 'IMPLIES
		     (ext-exp-open-dag-shallow conj)
		     (cdr (ext-exp-open-dag-shallow *current-edag*))))
	(setf (ext-exp-open-arc-node arc) conj)
	(setf (ext-exp-open-dag-parent-arcs conj) (list arc))
	(setf (ext-exp-open-dag-parent-arcs l) (list arc2))
	(setf (ext-exp-open-dag-parent-arcs node) (list arc1))
	(push lemm *current-edag-lemmas*)
	(let ((negf2 (car *current-edag-lemma-ftree-pfs*))
	      (clist2 (cadr *current-edag-lemma-ftree-pfs*)))
	  (setq *current-edag-lemma-ftree-pfs*
		(list (make-ftree-con negf negf2)
		      (append clist clist2))))
	(set-eeod-con conj arc1 arc2))
    (let* ((top (make-ext-exp-open-dag :positive nil))
	   (arc1 (make-ext-exp-open-arc :parent top :node node))
	   (arc2 (make-ext-exp-open-arc :parent top :node *current-edag*)))
      (setf (ext-exp-open-dag-shallow top)
	    (acons 'IMPLIES
		   (ext-exp-open-dag-shallow node)
		   (ext-exp-open-dag-shallow *current-edag*)))
      (setf (ext-exp-open-dag-parent-arcs node) (list arc1))
      (setf (ext-exp-open-dag-parent-arcs *current-edag*) (list arc2))
      (push lemm *current-edag-lemmas*)
      (setq *current-edag-lemma-ftree-pfs* (list negf clist))
      (set-eeod-imp top arc1 arc2)
      (setq *current-edag* top))))

(defun ext-saturation-change-dups ()
  (declare (special ext-options ext-search-done ext-used-options ext-min-exp-arcs
		    ext-num-simul-set-arcs ext-considered-set-arcs
		    ))
  (if (ext-sat-full-dup-p) ; already at full duplication
      (if (or (not ext-options) MAX-SEARCH-LIMIT) ; if no more options or timed out, then search failed
	  (setq ext-search-done 'failed)
	(progn ; else, start over working on options
	  (when ms03-verbose (msgf "Restarting Options"))
	  (setq ext-used-options nil)
	  (setq ext-min-exp-arcs nil)
	  (setq ext-num-simul-set-arcs 1)
	  (setq ext-considered-set-arcs nil)
	  (ext-saturation-new-dups)))
    (ext-saturation-new-dups))
  )

(defun ext-saturation-new-dups ()
  (declare (special ext-min-exps ext-min-exp-arcs ext-all-exps
		    ext-dup-arc-order ext-min-lemmas ext-all-lemmas
		    ext-used-options))
  (ext-saturation-new-dups-1)
  (when ms03-verbose
    (dolist (lem ext-min-lemmas)
      (msgf "Including Lemma " (car lem) t
	    ((ext-exp-open-dag-shallow (cadr lem)) . gwff))))
  (setq ext-min-exps (mapcar #'(lambda (exp-arcs)
				 (cons (car exp-arcs)
				       (intersection (cdr exp-arcs) ext-min-exp-arcs)))
			     ext-all-exps))
  (when ms03-verbose
    (dolist (earcs ext-min-exps)
      (when (cdr earcs)
	(if (cddr earcs)
	    (msgf "For Expansion Node " (car earcs) " Including Arcs:" t
		  (cdr earcs))
	  (msgf "For Expansion Node " (car earcs) " Including Arc " 
		(cadr earcs)))))))

(defun ext-saturation-new-dups-1 ()
  (declare (special ext-dup ext-all-exps ext-min-exps ext-dup-arc-order ext-min-exp-arcs
		    ext-min-lemmas ext-all-lemmas ext-level ext-selvar-arcs
		    ext-considered-set-arcs ext-num-simul-set-arcs
		    ext-used-options))
  (cond ((member MS03-DUP-METHOD '(2 3) :test #'equal)
	 (push (list ext-min-exp-arcs ext-min-lemmas) ext-used-options)
	 (let ((set-exp-arcs nil))
	   (dolist (earcs ext-all-exps)
	     (if (= MS03-DUP-METHOD 2)
		 (dolist (arc (cdr earcs))
		   (when (or (find-if #'(lambda (lem)
					  (member arc (caddr lem)))
				      ext-all-lemmas)
			     (compound-formula-p (ext-exp-open-arc-exp-term arc) t))
		     (push arc set-exp-arcs)))
	       (when (eq (inmost-car (unabbreviated-type (bindvar (ext-exp-open-dag-shallow (car earcs))))) 'O)
		 (setq set-exp-arcs (append (cdr earcs) set-exp-arcs)))))
	   (let* ((curr-set-arcs (intersection set-exp-arcs ext-min-exp-arcs)))
	     (setq ext-considered-set-arcs (append curr-set-arcs ext-considered-set-arcs))
	     (multiple-value-setq
		 (ext-min-exp-arcs ext-min-lemmas)
	       (remove-inaccessible-arcs
		(set-difference ext-min-exp-arcs set-exp-arcs)
		nil))
	     (dotimes (i ext-num-simul-set-arcs)
	       (let ((new-set-arc (earliest-arc (set-difference set-exp-arcs
								ext-considered-set-arcs))))
		 (unless new-set-arc
		   (setq ext-considered-set-arcs nil)
;		   (incf ext-num-simul-set-arcs)
		   (setq new-set-arc (earliest-arc set-exp-arcs)))
		 (when new-set-arc
		   (push new-set-arc ext-considered-set-arcs)
		   (multiple-value-setq
		       (ext-min-exp-arcs ext-min-lemmas)
		     (new-arc-closure new-set-arc ext-min-exp-arcs ext-min-lemmas ext-selvar-arcs)))))
	       ; also, add another nonset arc
	     (let ((new-arc (first-new-exp-arc ext-min-exp-arcs ext-min-lemmas set-exp-arcs)))
	       (when new-arc
		 (multiple-value-setq
		     (ext-min-exp-arcs ext-min-lemmas)
		   (new-arc-closure new-arc ext-min-exp-arcs ext-min-lemmas ext-selvar-arcs))))
	     (multiple-value-setq
		 (ext-min-exp-arcs ext-min-lemmas)
	       (ensure-one-exp-arc ext-min-exp-arcs ext-min-lemmas ext-selvar-arcs))
	     (multiple-value-setq
		 (ext-min-exp-arcs ext-min-lemmas)
	       (ensure-new-option ext-min-exp-arcs ext-min-lemmas ext-selvar-arcs
				  #'(lambda (arc1 arc2)
				      (if (eeod-flex-below-arc-p arc1)
					  (if (eeod-flex-below-arc-p arc2)
					      (if (member arc1 set-exp-arcs)
						  (if (member arc2 set-exp-arcs)
						      (< (ext-exp-open-dag-stamp
							  (ext-exp-open-arc-node arc1))
							 (ext-exp-open-dag-stamp
							  (ext-exp-open-arc-node arc2)))
						    nil)
						(if (member arc2 set-exp-arcs)
						    t
						  (< (ext-exp-open-dag-stamp
						      (ext-exp-open-arc-node arc1))
						     (ext-exp-open-dag-stamp
						      (ext-exp-open-arc-node arc2)))))
					    nil)
					(if (eeod-flex-below-arc-p arc2)
					    t
					  (if (member arc1 set-exp-arcs)
					      (if (member arc2 set-exp-arcs)
						  (< (ext-exp-open-dag-stamp
						      (ext-exp-open-arc-node arc1))
						     (ext-exp-open-dag-stamp
						      (ext-exp-open-arc-node arc2)))
						nil)
					    (if (member arc2 set-exp-arcs)
						t
					      (< (ext-exp-open-dag-stamp
						  (ext-exp-open-arc-node arc1))
						 (ext-exp-open-dag-stamp
						  (ext-exp-open-arc-node arc2))))))))
				  ))
	     )))
	(t ; treat all others like 1
	 (let ((new-arc (first-new-exp-arc ext-min-exp-arcs ext-min-lemmas)))
	   (when new-arc
	     (multiple-value-setq
		 (ext-min-exp-arcs ext-min-lemmas)
	       (new-arc-closure new-arc ext-min-exp-arcs ext-min-lemmas ext-selvar-arcs))
	     (multiple-value-setq
		 (ext-min-exp-arcs ext-min-lemmas)
	       (ensure-one-exp-arc ext-min-exp-arcs ext-min-lemmas ext-selvar-arcs)))))))

; add new arcs until we have an untried option (or all arcs)
(defun ensure-new-option (arcs lems selvar-arcs &optional arc-less)
  (declare (special ext-used-options))
  (let ((u (find-if #'(lambda (u) (and (subsetp arcs (car u))
				       (subsetp lems (cadr u) :test #'equal)))
		    ext-used-options)))
    (if u
	(let ((new-arc (first-new-exp-arc arcs lems (car u) arc-less)))
	  (when new-arc
	    (multiple-value-setq
		(arcs lems)
	      (new-arc-closure new-arc arcs lems selvar-arcs))
	    (multiple-value-setq
		(arcs lems)
	      (ensure-one-exp-arc arcs lems selvar-arcs arc-less))
	    (multiple-value-setq
		(arcs lems)
	      (ensure-new-option arcs lems selvar-arcs arc-less)))
	  (values arcs lems))
      (values arcs lems))))

; find all exp arcs and lems which should sensibly be included to include newarc
; adding newarc to arcs (newarc should be an expansion arc)
(defun new-arc-closure (newarc arcs lems selvar-arcs)
  (if (member newarc arcs)
      (values arcs lems)
    (new-arc-closure-1 newarc (cons newarc arcs) lems selvar-arcs)))

; find all exp arcs and lems which should sensibly be included to include newarc
; not adding newarc to arcs (newarc need not be an exp arc)
(defun new-arc-closure-1 (newarc arcs lems selvar-arcs)
  (declare (special ext-all-lemmas))
  (let ((ret-arcs arcs)
	(ret-lems lems)
	(above-arcs (eeod-get-arcs-above #'(lambda (x) t)
					 (ext-exp-open-arc-parent newarc))))
    (dolist (exparc (remove-if-not #'(lambda (x) (eq (ext-exp-open-arc-kind x) 'EXP)) above-arcs))
      (multiple-value-setq
	  (ret-arcs ret-lems)
	(new-arc-closure exparc ret-arcs ret-lems selvar-arcs)))
    (dolist (lem ext-all-lemmas)
      (let ((node (cadr lem)))
	(when (find-if #'(lambda (arc) (eq node (ext-exp-open-arc-node arc))) above-arcs)
	  (unless (member lem ret-lems :test #'equal)
	    (push lem ret-lems)))))
    (let ((trm (ext-exp-open-arc-exp-term newarc)))
      (when trm
	(dolist (v (free-vars-of trm))
	  (let ((a (assoc v selvar-arcs)))
	    (when a
	      (multiple-value-setq
		  (ret-arcs ret-lems)
		(new-arc-closure-1 (cdr a) ret-arcs ret-lems selvar-arcs)))))))
    (values ret-arcs ret-lems)))

(defun ensure-one-exp-arc (arcs lems selvar-arcs &optional arc-less)
  (declare (special ext-crucial-arcs))
  (let ((expnodes (eeod-get-nodes-restr-exps
		   #'(lambda (x)
		       (and (eq (ext-exp-open-dag-kind x) 'EXP)
			    (not (intersection (ext-exp-open-dag-arcs x) arcs))))
		   arcs lems)))
    (if expnodes
	(let ((ret-arcs arcs)
	      (ret-lems lems))
	  (dolist (exp expnodes)
	    (let* ((arcs1 (ext-exp-open-dag-arcs exp))
		   (carcs (intersection arcs1 ext-crucial-arcs)))
	      (if carcs
		  (dolist (arc carcs)
		    (multiple-value-setq
			(ret-arcs ret-lems)
		      (new-arc-closure arc ret-arcs ret-lems selvar-arcs)))
		(let ((earliest-arc (earliest-arc arcs1 arc-less)))
		  (when earliest-arc
		    (multiple-value-setq
			(ret-arcs ret-lems)
		      (new-arc-closure earliest-arc ret-arcs ret-lems selvar-arcs)))))))
	  (ensure-one-exp-arc ret-arcs ret-lems selvar-arcs))
      (values arcs lems))))

(defun first-new-exp-arc (arcs lems &optional exc arc-less)
  (let ((expnodes (eeod-get-nodes-restr-exps #'(lambda (x)
						 (eq (ext-exp-open-dag-kind x) 'EXP))
					     arcs lems))
	(n nil)
	(earliest-arc nil))
    (dolist (expnode expnodes)
      (dolist (arc (ext-exp-open-dag-arcs expnode))
	(unless (or (member arc arcs) (member arc exc))
	  (if earliest-arc
	      (when (if arc-less
			(funcall arc-less arc earliest-arc)
		      (< (ext-exp-open-dag-stamp (ext-exp-open-arc-node arc))
			 n))
		(setq earliest-arc arc)
		(unless arc-less
		  (setq n 
			(ext-exp-open-dag-stamp (ext-exp-open-arc-node arc)))))
	    (progn
	      (setq earliest-arc arc)
	      (unless arc-less
		(setq n 
		      (ext-exp-open-dag-stamp (ext-exp-open-arc-node arc)))))))))
    earliest-arc))

(defun earliest-arc (arcs &optional arc-less)
  (let ((n nil)
	(earliest-arc nil))
    (dolist (arc arcs)
      (if earliest-arc
	  (when (if arc-less
		    (funcall arc-less arc earliest-arc)
		  (< (ext-exp-open-dag-stamp (ext-exp-open-arc-node arc))
		     n))
	    (setq earliest-arc arc)
	    (unless arc-less
	      (setq n 
		    (ext-exp-open-dag-stamp (ext-exp-open-arc-node arc)))))
	(progn
	  (setq earliest-arc arc)
	  (unless arc-less
	    (setq n
		  (ext-exp-open-dag-stamp (ext-exp-open-arc-node arc)))))))
    earliest-arc))

(defun remove-inaccessible-arcs (arcs lems)
  (declare (special ext-all-lemmas))
  (let ((ret-arcs nil)
	(ret-lems nil)
	(expnodes (eeod-get-nodes-restr-exps #'(lambda (x)
						 (eq (ext-exp-open-dag-kind x) 'EXP))
					     arcs lems)))
    (dolist (arc arcs)
      (when (member (ext-exp-open-arc-parent arc) expnodes)
	(push arc ret-arcs)))
    (dolist (lem lems)
      (when (intersection (caddr lem) ret-arcs)
	(push lem ret-lems)))
    (unless (and (subsetp lems ret-lems :test #'equal) (subsetp arcs ret-arcs))
      (multiple-value-setq
	  (ret-arcs ret-lems)
	(remove-inaccessible-arcs ret-arcs ret-lems)))
    (values ret-arcs ret-lems)))

(defun eeod-get-nodes-restr-exps (testfn exparcs lems &optional (multiple t))
  (declare (special ext-all-lemmas))
  (let ((nodes-done nil)
	(nodes nil)
	(ret nil))
    (declare (special nodes-done))
    (if ext-all-lemmas
	(push (cadr (ext-exp-open-dag-kids *current-edag*)) nodes)
      (push *current-edag* nodes))
    (dolist (l lems)
      (push (cadr l) nodes))
    (if multiple
	(dolist (n nodes)
	  (setq ret (append (eeod-get-nodes-restr-exps-1 testfn n exparcs t) ret)))
      (do ((nl nodes (cdr nl)))
	  ((or (null nl) ret))
	(setq ret (eeod-get-nodes-restr-exps-1 testfn (car nl) exparcs nil))))
    ret))

(defun eeod-get-nodes-restr-exps-1 (testfn eeod exparcs &optional (multiple t))
  (declare (special nodes-done))
  (if (or (member eeod nodes-done) (not (ext-exp-open-dag-p eeod))) ; delete extra check once debugged
      nil
    (progn
      (push eeod nodes-done)
      (if (funcall testfn eeod)
	  (if multiple
	      (cons eeod (eeod-get-nodes-restr-exps-2 testfn (ext-exp-open-dag-arcs-restr-exps eeod exparcs) exparcs multiple))
	    eeod)
	(eeod-get-nodes-restr-exps-2 testfn (ext-exp-open-dag-arcs-restr-exps eeod exparcs) exparcs multiple)))))

(defun ext-exp-open-dag-arcs-restr-exps (eeod exparcs)
  (if (eq (ext-exp-open-dag-kind eeod) 'EXP)
      (intersection (ext-exp-open-dag-arcs eeod) exparcs)
    (ext-exp-open-dag-arcs eeod)))

(defun eeod-get-nodes-restr-exps-2 (testfn arcs exparcs &optional (multiple t))
  (if arcs
      (if multiple
	  (append (eeod-get-nodes-restr-exps-1 testfn (ext-exp-open-arc-node (car arcs)) exparcs multiple)
		  (eeod-get-nodes-restr-exps-2 testfn (cdr arcs) exparcs multiple))
	(or (eeod-get-nodes-restr-exps-1 testfn (ext-exp-open-arc-node (car arcs)) exparcs multiple)
	    (eeod-get-nodes-restr-exps-2 testfn (cdr arcs) exparcs multiple)))
    nil))

(defun ext-sat-full-dup-p ()
  (declare (special ext-min-exp-arcs ext-all-exps))
  (not (find-if #'(lambda (exp-arcs)
		    (not (subsetp (cdr exp-arcs) ext-min-exp-arcs)))
		ext-all-exps)))

(defun ext-eeod-disj-val (node)
  (let ((node-assoc nil))
    (declare (special node-assoc))
    (ext-eeod-disj-val-1 node)))

(defun ext-eeod-disj-val-1 (node)
  (declare (special node-assoc))
  (let ((a (assoc node node-assoc)))
    (if a
	(cdr a)
      (let ((res 0))
	(dolist (arc (ext-exp-open-dag-parent-arcs node))
	  (setq res (+ res
		       (ext-eeod-disj-val-1 (ext-exp-open-arc-parent arc)))))
	(push (cons node res) node-assoc)
	res))))

(defun ext-jform-disj-val (j)
  (if (jform-parent j)
      (if (disjunction-p j)
	  (1+ (ext-jform-disj-val (jform-parent j)))
	(ext-jform-disj-val (jform-parent j)))
    0))

(defun ext-saturation-search-options-rigid (jform)
  (declare (special ext-level ext-options ext-search-done))
  (dolist (plit 
	   (find-literals-in-jform
	    #'(lambda (x)
		(and (jform-pos x)
		     (not (equals-p (jform-represents x)))))
	    jform))
    (let* ((name (literal-name plit))
	   (wff (jform-represents plit))
	   (h (head wff))
	   (neglits (conjunctively-related-literals
		     #'(lambda (x)
			 (and (not (jform-pos x))
			      (not (equals-p (jform-represents x)))
			      (eq h (head (jform-represents x)))))
		     plit))
	   (pddepth (ext-jform-disj-val plit)))
      (dolist (nlit neglits)
	(ext-saturation-search-option-rigid-mates
	 (name-to-eeod (literal-name plit))
	 (name-to-eeod (literal-name nlit))
	 (ext-auxiliary-eqns-2 plit nlit)
	 pddepth (ext-jform-disj-val nlit)))))
  (dolist (nlit
	   (find-literals-in-jform
	    #'(lambda (x)
		(and (not (jform-pos x))
		     (not (equals-p (jform-represents x)))))
	    jform))
    (let* ((name (literal-name nlit))
	   (wff (jform-represents nlit))
	   (h (head wff))
	   (poslits (conjunctively-related-literals
		     #'(lambda (x)
			 (and (jform-pos x)
			      (not (equals-p (jform-represents x)))
			      (eq h (head (jform-represents x)))))
		     nlit))
	   (nddepth (ext-jform-disj-val nlit)))
      (dolist (plit poslits)
	(ext-saturation-search-option-rigid-mates
	 (name-to-eeod (literal-name plit))
	 (name-to-eeod (literal-name nlit))
	 (ext-auxiliary-eqns-2 plit nlit)
	 (ext-jform-disj-val plit) nddepth))))
  (dolist (peqn
	   (find-literals-in-jform
	    #'(lambda (x)
		(and (jform-pos x)
		     (equals-p (jform-represents x))))
	    jform))
    (let* ((name (literal-name peqn))
	   (wff (jform-represents peqn))
	   (tp (cdr (unabbreviated-type (caar wff))))
	   (negeqns (conjunctively-related-literals
		     #'(lambda (x)
			 (and (not (jform-pos x))
			      (equals-p (jform-represents x))
			      (eq tp (cdr (unabbreviated-type (caar (jform-represents x)))))))
		     peqn))
	   (pddepth (ext-jform-disj-val peqn)))
      (dolist (neqn negeqns)
	(ext-saturation-search-option-eunif
	 (name-to-eeod (literal-name peqn))
	 (name-to-eeod (literal-name neqn))
	 (ext-auxiliary-eqns-2 peqn neqn)
	 pddepth (ext-jform-disj-val neqn)))))
  (dolist (neqn 
	   (find-literals-in-jform
	    #'(lambda (x)
		(and (not (jform-pos x))
		     (equals-p (jform-represents x))))
	    jform))
    (let* ((name (literal-name neqn))
	   (wff (jform-represents neqn))
	   (tp (cdr (unabbreviated-type (caar wff))))
	   (nddepth (ext-jform-disj-val neqn)))
      (when (ext-exp-var-p (head (cdar wff))) ; can't be flexflex (those are not in jform)
	(ext-saturation-search-option-unif (cdar wff) (cdr wff)
					   (name-to-eeod name)
					   (ext-auxiliary-eqns-1 neqn) nddepth))
      (when (ext-exp-var-p (head (cdr wff))) ; can't be flexflex (those are not in jform)
	(ext-saturation-search-option-unif (cdr wff) (cdar wff)
					   (name-to-eeod name)
					   (ext-auxiliary-eqns-1 neqn) nddepth))
      (dolist (peqn (conjunctively-related-literals
		     #'(lambda (x)
			 (and (jform-pos x)
			      (equals-p (jform-represents x))
			      (eq tp (cdr (unabbreviated-type (caar (jform-represents x)))))))
		     neqn))
	(ext-saturation-search-option-eunif
	 (name-to-eeod (literal-name peqn))
	 (name-to-eeod (literal-name neqn))
	 (ext-auxiliary-eqns-2 peqn neqn)
	 (ext-jform-disj-val peqn) nddepth)))))

(defun ext-saturation-search-options (jform)
  (declare (special ext-level ext-options ext-search-done))
  (dolist (plit 
	   (find-literals-in-jform
	    #'(lambda (x)
		(and (jform-pos x)
		     (not (equals-p (jform-represents x)))
		     (not (ext-exp-var-p (head (jform-represents x))))))
	    jform))
    (let* ((name (literal-name plit))
	   (wff (jform-represents plit))
	   (h (head wff))
	   (neglits (conjunctively-related-literals
		     #'(lambda (x)
			 (and (not (jform-pos x))
			      (not (equals-p (jform-represents x)))
			      (eq h (head (jform-represents x)))))
		     plit))
	   (flexlits (conjunctively-related-literals
		      #'(lambda (x)
			  (and (not (equals-p (jform-represents x)))
			       (ext-exp-var-p (head (jform-represents x)))))
		      plit))
	   (pddepth (ext-jform-disj-val plit)))
      (dolist (nlit neglits)
	(ext-saturation-search-option-rigid-mates
	 (name-to-eeod (literal-name plit))
	 (name-to-eeod (literal-name nlit))
	 (ext-auxiliary-eqns-2 plit nlit)
	 pddepth (ext-jform-disj-val nlit)))
      (dolist (flit flexlits)
	(ext-saturation-search-option-flex-rigid-mates
	 (name-to-eeod (literal-name flit))
	 (name-to-eeod (literal-name plit))
	 (ext-auxiliary-eqns-2 flit plit)
	 pddepth (ext-jform-disj-val flit)))))
  (dolist (nlit
	   (find-literals-in-jform
	    #'(lambda (x)
		(and (not (jform-pos x))
		     (not (equals-p (jform-represents x)))
		     (not (ext-exp-var-p (head (jform-represents x))))))
	    jform))
    (let* ((name (literal-name nlit))
	   (wff (jform-represents nlit))
	   (h (head wff))
	   (poslits (conjunctively-related-literals
		     #'(lambda (x)
			 (and (jform-pos x)
			      (not (equals-p (jform-represents x)))
			      (eq h (head (jform-represents x)))))
		     nlit))
	   (flexlits (conjunctively-related-literals
		      #'(lambda (x)
			  (and (not (equals-p (jform-represents x)))
			       (ext-exp-var-p (head (jform-represents x)))))
		      nlit))
	   (nddepth (ext-jform-disj-val nlit)))
      (dolist (plit poslits)
	(ext-saturation-search-option-rigid-mates
	 (name-to-eeod (literal-name plit))
	 (name-to-eeod (literal-name nlit))
	 (ext-auxiliary-eqns-2 plit nlit)
	 (ext-jform-disj-val plit) nddepth))
      (dolist (flit flexlits)
	(ext-saturation-search-option-flex-rigid-mates
	 (name-to-eeod (literal-name flit))
	 (name-to-eeod (literal-name nlit))
	 (ext-auxiliary-eqns-2 flit nlit)
	 (ext-jform-disj-val flit) nddepth))))
  (dolist (peqn
	   (find-literals-in-jform
	    #'(lambda (x)
		(and (jform-pos x)
		     (equals-p (jform-represents x))))
	    jform))
    (let* ((name (literal-name peqn))
	   (wff (jform-represents peqn))
	   (tp (cdr (unabbreviated-type (caar wff))))
	   (pddepth (ext-jform-disj-val peqn))
	   (negeqns (conjunctively-related-literals
		     #'(lambda (x)
			 (and (not (jform-pos x))
			      (equals-p (jform-represents x))
			      (eq tp (cdr (unabbreviated-type (caar (jform-represents x)))))))
		     peqn)))
      (dolist (neqn negeqns)
	(ext-saturation-search-option-eunif
	 (name-to-eeod (literal-name peqn))
	 (name-to-eeod (literal-name neqn))
	 (ext-auxiliary-eqns-2 peqn neqn)
	 pddepth (ext-jform-disj-val neqn)))))
  (dolist (neqn 
	   (find-literals-in-jform
	    #'(lambda (x)
		(and (not (jform-pos x))
		     (equals-p (jform-represents x))))
	    jform))
    (let* ((name (literal-name neqn))
	   (wff (jform-represents neqn))
	   (tp (cdr (unabbreviated-type (caar wff))))
	   (nddepth (ext-jform-disj-val neqn)))
      (when (ext-exp-var-p (head (cdar wff))) ; can't be flexflex (those are not in jform)
	(ext-saturation-search-option-unif (cdar wff) (cdr wff)
					   (name-to-eeod name)
					   (ext-auxiliary-eqns-1 neqn)
					   nddepth))
      (when (ext-exp-var-p (head (cdr wff))) ; can't be flexflex (those are not in jform)
	(ext-saturation-search-option-unif (cdr wff) (cdar wff)
					   (name-to-eeod name)
					   (ext-auxiliary-eqns-1 neqn)
					   nddepth))
      (let ((poseqns (conjunctively-related-literals
		      #'(lambda (x)
			  (and (jform-pos x)
			       (equals-p (jform-represents x))
			       (eq tp (cdr (unabbreviated-type (caar (jform-represents x)))))))
		      neqn)))
	(dolist (peqn poseqns)
	  (ext-saturation-search-option-eunif
	   (name-to-eeod (literal-name peqn))
	   (name-to-eeod (literal-name neqn))
	   (ext-auxiliary-eqns-2 peqn neqn)
	   (ext-jform-disj-val peqn) nddepth)))))
  (dolist (flit (find-literals-in-jform
		 #'(lambda (x)
		     (and (not (equals-p (jform-represents x)))
			  (ext-exp-var-p (head (jform-represents x)))))
		 jform))
    (ext-saturation-search-option-primsub (name-to-eeod (literal-name flit)))))

(defun ext-saturation-search-eeod-options (node)
  (declare (special ext-level ext-options ext-search-done))
  (dolist (plit 
	   (eeod-get-nodes
	    #'(lambda (x)
		(and (ext-exp-open-dag-positive x)
		     (eq (ext-exp-open-dag-kind x) 'ATOM)))
	    node))
    (let* ((name (ext-exp-open-dag-name plit))
	   (wff (ext-exp-open-dag-shallow plit))
	   (h (head wff))
	   (neglits (conjunctively-related-nodes
		     #'(lambda (x)
			 (and (not (ext-exp-open-dag-positive x))
			      (eq (ext-exp-open-dag-kind x) 'ATOM)
			      (eq h (head (ext-exp-open-dag-shallow x)))))
		     plit))
	   (flexlits (conjunctively-related-nodes
		      #'(lambda (x)
			  (eq (ext-exp-open-dag-kind x) 'FLEX))
		      plit))
	   (pddepth (ext-eeod-disj-val plit)))
	(dolist (nlit neglits)
	  (ext-saturation-search-option-rigid-mates
	   plit nlit
	   (ext-auxiliary-eeod-eqns-2 plit nlit)
	   pddepth (ext-eeod-disj-val nlit)))
	(dolist (flit flexlits)
	  (ext-saturation-search-option-flex-rigid-mates
	   flit plit
	   (ext-auxiliary-eeod-eqns-2 flit plit)
	   pddepth (ext-eeod-disj-val plit)))))
  (dolist (nlit
	   (eeod-get-nodes
	    #'(lambda (x)
		(and (not (ext-exp-open-dag-positive x))
		     (eq (ext-exp-open-dag-kind x) 'ATOM)))
	    node))
    (let* ((name (ext-exp-open-dag-name nlit))
	   (wff (ext-exp-open-dag-shallow nlit))
	   (h (head wff))
	   (poslits (conjunctively-related-nodes
		     #'(lambda (x)
			 (and (ext-exp-open-dag-positive x)
			      (eq (ext-exp-open-dag-kind x) 'ATOM)
			      (eq h (head (ext-exp-open-dag-shallow x)))))
		     nlit))
	   (flexlits (conjunctively-related-nodes
		      #'(lambda (x)
			  (eq (ext-exp-open-dag-kind x) 'FLEX))
		      nlit))
	   (nddepth (ext-eeod-disj-val nlit)))
	(dolist (plit poslits)
	  (ext-saturation-search-option-rigid-mates
	   plit nlit
	   (ext-auxiliary-eeod-eqns-2 plit nlit)
	   (ext-eeod-disj-val plit) nddepth))
	(dolist (flit flexlits)
	  (ext-saturation-search-option-flex-rigid-mates
	   flit nlit
	   (ext-auxiliary-eeod-eqns-2 flit nlit)
	   (ext-eeod-disj-val flit) nddepth))))
  (dolist (peqn
	   (eeod-get-nodes
	    #'(lambda (x)
		(and (ext-exp-open-dag-positive x)
		     (eq (ext-exp-open-dag-kind x) 'EQN)))
	    node))
    (let* ((name (ext-exp-open-dag-name peqn))
	   (wff (ext-exp-open-dag-shallow peqn))
	   (tp (cdr (unabbreviated-type (caar wff))))
	   (pddepth (ext-eeod-disj-val peqn)))
      (let ((negeqns (conjunctively-related-nodes
		      #'(lambda (x)
			  (and (eq (ext-exp-open-dag-kind x) 'EQNGOAL)
			       (eq tp (cdr (unabbreviated-type (caar (ext-exp-open-dag-shallow x)))))))
		      peqn)))
	(dolist (neqn negeqns)
	  (ext-saturation-search-option-eunif
	   peqn neqn
	   (ext-auxiliary-eeod-eqns-2 peqn neqn)
	   pddepth (ext-eeod-disj-val neqn))))))
  (dolist (neqn 
	   (eeod-get-nodes
	    #'(lambda (x)
		(eq (ext-exp-open-dag-kind x) 'EQNGOAL))
	    node))
    (let* ((name (ext-exp-open-dag-name neqn))
	   (wff (ext-exp-open-dag-shallow neqn))
	   (h1 (head (cdar wff)))
	   (h2 (head (cdr wff)))
	   (tp (cdr (unabbreviated-type (caar wff))))
	   (nddepth (ext-eeod-disj-val neqn)))
      (unless (and (ext-exp-var-p h1) (ext-exp-var-p h2)) ; ignore flexflex during search
	(when (ext-exp-var-p h1)
	  (ext-saturation-search-option-unif (cdar wff) (cdr wff) neqn
					     (ext-auxiliary-eeod-eqns-1 neqn)
					     nddepth))
	(when (ext-exp-var-p h2)
	  (ext-saturation-search-option-unif (cdr wff) (cdar wff) neqn
					     (ext-auxiliary-eeod-eqns-1 neqn)
					     nddepth))
	(let ((poseqns (conjunctively-related-nodes
			#'(lambda (x)
			    (and (ext-exp-open-dag-positive x)
				 (eq (ext-exp-open-dag-kind x) 'EQN)
				 (eq tp (cdr (unabbreviated-type (caar (ext-exp-open-dag-shallow x)))))))
			neqn)))
	  (dolist (peqn poseqns)
	    (ext-saturation-search-option-eunif
	     peqn neqn
	     (ext-auxiliary-eeod-eqns-2 peqn neqn)
	     (ext-eeod-disj-val peqn) nddepth))))))
  (dolist (flit (eeod-get-nodes
		 #'(lambda (x)
		     (eq (ext-exp-open-dag-kind x) 'FLEX))
		 node))
    (ext-saturation-search-option-primsub flit)))

(defun ext-auxiliary-eqns-1 (lit)
  (mapcar #'(lambda (x)
	      (let* ((e (jform-represents x))
		     (tp (cdr (unabbreviated-type (caar e)))))
		(list tp (cdar e) (cdr e))))
	  (conjunctively-related-literals
	   #'(lambda (x)
	       (and (jform-pos x)
		    (equals-p (jform-represents x))))
	   lit)))

(defun ext-auxiliary-eqns-2 (lit1 lit2)
  (let ((eqns1 (ext-auxiliary-eqns-1 lit1))
	(eqns2 (ext-auxiliary-eqns-1 lit2)))
    (intersection eqns1 eqns2 :test #'equal)))

(defun ext-auxiliary-eeod-eqns-1 (lit)
  (mapcar #'(lambda (x)
	      (let* ((e (ext-exp-open-dag-shallow x))
		     (tp (cdr (unabbreviated-type (caar e)))))
		(list tp (cdar e) (cdr e))))
	  (conjunctively-related-nodes
	   #'(lambda (x)
	       (and (ext-exp-open-dag-positive x)
		    (eq (ext-exp-open-dag-kind x) 'EQN)))
	   lit)))

(defun ext-auxiliary-eeod-eqns-2 (lit1 lit2)
  (let ((eqns1 (ext-auxiliary-eeod-eqns-1 lit1))
	(eqns2 (ext-auxiliary-eeod-eqns-1 lit2)))
    (intersection eqns1 eqns2 :test #'equal)))

; the lower the weight, the more likely unification will be successful.
; probably should use a hash table to prevent recomputation.
(defun ext-quick-eunification-weight (dpairs eqns)
  (runcount 'unification)
  (let ((changed t)
	(steps 0)
	(insts nil))
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
		    (let ((h1 (head (cadr d)))
			  (h2 (head (caddr d))))
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
		       (fh (head flex))
		       (rh (head rig))
		       (bound (cadddr (car fs))))
		  (if (ext-imitation-heads-possible-p fh rh)
		      (unless (or (ext-possible-var-projections fh)
				  (ext-rigid-occurs-check-p fh rig))
			(multiple-value-bind
			    (wff evars)
			    (create-imit-subst fh rh)
			  (push (list 'IMIT fh rh evars) insts)
			  (setq dpairs
				(mapcar #'(lambda (d)
					    (list (car d)
						  (lambda-norm (substitute-l-term-var wff fh (cadr d)))
						  (lambda-norm (substitute-l-term-var wff fh (caddr d)))
						  (cadddr d)))
					dpairs)))
			(incf steps)
			(setq changed t))
		    (let ((projs (ext-possible-var-projections fh)))
		      (when (= (length projs) 1)
			(multiple-value-bind
			    (wff evars)
			    (create-proj-subst fh (car projs))
			  (push (list 'PROJ fh (car projs) evars) insts)
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
		       (fh (head flex))
		       (rh (head rig))
		       (bound (cadddr (car fs))))
		  (if (ext-imitation-heads-possible-p fh rh)
		      (unless (or (ext-possible-var-projections fh)
				  (ext-rigid-occurs-check-p fh rig))
			(multiple-value-bind
			    (wff evars)
			    (create-imit-subst fh rh)
			  (push (list 'IMIT fh rh evars) insts)
			  (setq dpairs
				(mapcar #'(lambda (d)
					    (list (car d)
						  (lambda-norm (substitute-l-term-var wff fh (cadr d)))
						  (lambda-norm (substitute-l-term-var wff fh (caddr d)))
						  (cadddr d)))
					dpairs)))
			(incf steps)
			(setq changed t))
		    (let ((projs (ext-possible-var-projections fh)))
		      (when (= (length projs) 1)
			(multiple-value-bind
			    (wff evars)
			    (create-proj-subst fh (car projs))
			  (push (list 'PROJ fh (car projs) evars) insts)
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
		       (h1 (head f1))
		       (h2 (head f2))
		       (bound (cadddr (car fs)))
		       (bound1 (append bound (get h1 'banned-sel-vars) (get h1 'banned-imitations)))
		       (bound2 (append bound (get h2 'banned-sel-vars) (get h2 'banned-imitations))))
		  (when (and (eeod-pattern-p-4 f1 bound1)
			     (eeod-pattern-p-4 f2 bound2))
		    (if (eq h1 h2)
			(multiple-value-bind
			    (wff ev)
			    (create-pattern-flexflex-same h1 (args (etanorm f1)) (args (etanorm f2)))
			  (push (list 'PATTERN-FLEXFLEX-SAME h1 wff ev) insts)
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
			(push (list 'PATTERN-FLEXFLEX-DIFF h1 h2 wff1 wff2 ev) insts)
			(let ((theta (acons h1 wff1 (acons h2 wff2 nil))))
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
		 (ext-quick-eunification-weight-1 (car d) (cadr d) (caddr d) (cadddr d) eqns))
	     dpairs)
     (reverse insts))))

(defun ext-quick-eunification-weight-1 (tp lft rght banned eqns)
  (let ((h1 (head lft))
	(h2 (head rght)))
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
	    (if (ext-possible-var-projections h1)
		'MS03-WEIGHT-FLEXRIGID-BRANCH
	      (if (ext-imitation-heads-possible-p h1 h2)
		  (if (ext-rigid-occurs-check-p h1 rght)
		      'MS03-WEIGHT-OCCURS-CHECK
		    'MS03-WEIGHT-FLEXRIGID-BRANCH)
		(if (member h2 (get h1 'banned-sel-vars))
		    'MS03-WEIGHT-BANNED-SELS
		  (if (find-if #'(lambda (x)
				   (and (eq tp (car x))
					(or (eq (head (cadr x)) h2)
					    (eq (head (caddr x)) h2))))
			       eqns)
		      'MS03-WEIGHT-FLEXRIGID-EQN
		    (if (find-if #'(lambda (x)
				     (and (eq tp (car x))
					  (or (ext-exp-var-p (head (cadr x)))
					      (ext-exp-var-p (head (cadr x))))))
				 eqns)
			'MS03-WEIGHT-FLEXRIGID-FLEXEQN
		      'MS03-WEIGHT-FLEXRIGID-NOEQN)))))))
      (if (ext-exp-var-p h2) ; rigid flex
	  (if (eq tp 'O)
	      'MS03-WEIGHT-FLEXRIGID-O
	    (if (ext-possible-var-projections h2)
		'MS03-WEIGHT-FLEXRIGID-BRANCH
	      (if (ext-imitation-heads-possible-p h2 h1)
		  (if (ext-rigid-occurs-check-p h2 lft)
		      'MS03-WEIGHT-OCCURS-CHECK
		    'MS03-WEIGHT-FLEXRIGID-BRANCH)
		(if (member h1 (get h2 'banned-sel-vars))
		    'MS03-WEIGHT-BANNED-SELS
		  (if (find-if #'(lambda (x)
				   (and (eq tp (car x))
					(or (eq (head (cadr x)) h1)
					    (eq (head (caddr x)) h1))))
			       eqns)
		      'MS03-WEIGHT-FLEXRIGID-EQN
		    (if (find-if #'(lambda (x)
				     (and (eq tp (car x))
					  (or (ext-exp-var-p (head (cadr x)))
					      (ext-exp-var-p (head (cadr x))))))
				 eqns)
			'MS03-WEIGHT-FLEXRIGID-FLEXEQN
		      'MS03-WEIGHT-FLEXRIGID-NOEQN))))))
					; rigid rigid
	(if (eq tp 'O)
	    (if (eq h1 h2)
		'MS03-WEIGHT-RIGIDRIGIDSAME-O
	      'MS03-WEIGHT-RIGIDRIGIDDIFF-O)
					; (different heads - otherwise would have decomposed first)
					; deps on eqns
	  (if (find-if #'(lambda (x)
			   (and (eq tp (car x))
				(or (and (eq h1 (head (cadr x)))
					 (eq h2 (head (caddr x))))
				    (and (eq h2 (head (cadr x)))
					 (eq h1 (head (caddr x)))))))
		       eqns)
	      'MS03-WEIGHT-RIGIDRIGID-EQN
	    (if (find-if #'(lambda (x)
			     (and (eq tp (car x))
				  (or (ext-exp-var-p (head (cadr x)))
				      (ext-exp-var-p (head (caddr x))))))
			 eqns)
		'MS03-WEIGHT-RIGIDRIGID-FLEXEQN
	      'MS03-WEIGHT-RIGIDRIGID-NOEQN)))))))
 
(defun ext-options-order (x y)
  (< (caddr x) (caddr y)))

(defun evaluate-ext-weight (w)
  (evaluate-ext-weight-1 (cadr w) (car w)))

(defun evaluate-ext-weight-1 (w &optional (res 0))
  (dolist (v w res)
    (if (consp v)
	(setq res (+ res (* (car v) (eval (cdr v)))))
      (setq res (+ res (eval v))))))

(defun ext-saturation-search-constraints (j)
  (declare (special ext-set-vars-constraints))
  (when (member 'MIN WHICH-CONSTRAINTS)
    (let ((ev-constraints nil))
      (dolist (nflit (find-literals-in-jform
		      #'(lambda (x)
			  (and (not (jform-pos x))
			       (not (equals-p (jform-represents x)))
			       (ext-exp-var-p (head (jform-represents x)))))
		      j))
	(let* ((wff (jform-represents nflit))
	       (ev (head wff)))
	  (unless (or (member ev ext-set-vars-constraints)
		      (find-if #'(lambda (arg) (free-in ev arg)) (args wff)))
	    (let ((c (assoc ev ev-constraints))
		  (rellits (conjunctively-related-literals
			    #'(lambda (y)
				(and (not (equals-p (jform-represents y)))
				     (or (not (ext-exp-var-p (head (jform-represents y))))
					 (and (jform-pos y)
					      (eq (head (jform-represents y)) ev)
					      (not (find-if #'(lambda (arg)
								(free-in ev arg))
							    (args (jform-represents y))))))))
			    nflit)))
	      (if c
		  (push (list nflit nil rellits)
			(cdr c))
		(push (list ev (list nflit nil rellits))
		      ev-constraints))))))
      (dotimes (i max-constraint-size)
	(setq ev-constraints
	      (mapcar
	       #'(lambda (ev-constr)
		   (let ((ev (car ev-constr))
			 (constrs (cdr ev-constr)))
		     (setq constrs
			   (apply #'append
				  (mapcar #'(lambda (c)
					      (let ((lit (car c))
						    (lits (cadr c))
						    (rellits (caddr c)))
						(cons c ; leave old one, and add new ones
						      (if (= (length lits) i)
							  (let ((stamps (mapcar #'(lambda (lit0)
										    (let ((node (get (literal-name lit0) 'ext-exp-open-dag)))
										      (if (and node (ext-exp-open-dag-p node))
											  (ext-exp-open-dag-stamp node)
											0)))
										lits)))
							    (mapcar #'(lambda (newlit)
									(list lit (cons newlit lits)
									      (intersection
									       rellits
									       (conjunctively-related-literals
										#'(lambda (y) t)
										newlit))))
								    (remove-if #'(lambda (newlit)
										   (let* ((node (get (literal-name newlit) 'ext-exp-open-dag))
											  (stamp (if (and node (ext-exp-open-dag-p node))
												     (ext-exp-open-dag-stamp node)
												   0)))
										     (find-if #'(lambda (s) (<= s stamp)) stamps)))
									       rellits)))
							nil))))
					  constrs)))
		     (cons ev constrs)))
	       ev-constraints)))
      (setq ev-constraints
	    (mapcar #'(lambda (ev-constr)
			(cons (car ev-constr)
			      (remove-if #'(lambda (c)
					     (or
					; remove trivial constrains (w/ only FULL or EMPTY soln)
					      (and (null (cadr c))
						   (let ((wff (jform-represents (car c))))
						     (eeod-pattern-p-4 wff (get (car ev-constr) 'banned-sel-vars))))
					; remove those that don't share a banned var
					      (and BAD-VAR-CONNECTED-PRUNE
						   (not
						    (lits-sel-conn-p (cons (car c) (cadr c))
								     (intersection
								      (free-vars-of (literal-represents (car c)))
								      (get (car ev-constr) 'banned-sel-vars))
								     (get (car ev-constr) 'banned-sel-vars))))))
					 (cdr ev-constr))))
		    ev-constraints))
      (setq ev-constraints
	    (mapcar #'(lambda (ev-constr)
			(cons (car ev-constr)
			      (sort
			       (constraint-sets-up-to (cdr ev-constr) max-num-constraints)
			       #'constraint-order)))
		    ev-constraints))
      (when ev-constraints
	(ext-saturation-full-exps)
	(dolist (ev-constrs ev-constraints)
	  (push (car ev-constrs) ext-set-vars-constraints)
	  (when ms03-verbose
	    (msgf (length (cdr ev-constrs)) " lower bound constraints generated for " ((car ev-constrs) . gwff) t))
	  (dolist (c (cdr ev-constrs))
	    (when ms03-verbose
	      (msgf "---------")
	      (dolist (l c)
		(msgf (cadr l) " -> " (car l) t)))
	    (ext-saturation-search-solve-constraints (car ev-constrs) c 'MIN)))
	(ext-saturation-min-exps))))
  (when (member 'MAX WHICH-CONSTRAINTS)
    (let ((ev-constraints nil))
      (dolist (nflit (find-literals-in-jform
		      #'(lambda (x)
			  (and (jform-pos x)
			       (not (equals-p (jform-represents x)))
			       (ext-exp-var-p (head (jform-represents x)))))
		      j))
	(let* ((wff (jform-represents nflit))
	       (ev (head wff)))
	  (unless (or (member ev ext-set-vars-constraints)
		      (find-if #'(lambda (arg) (free-in ev arg)) (args wff)))
	    (let ((c (assoc ev ev-constraints))
		  (rellits (conjunctively-related-literals
			    #'(lambda (y)
				(and (not (equals-p (jform-represents y)))
				     (or (not (ext-exp-var-p (head (jform-represents y))))
					 (and (not (jform-pos y))
					      (eq (head (jform-represents y)) ev)
					      (not (find-if #'(lambda (arg)
								(free-in ev arg))
							    (args (jform-represents y))))))))
			    nflit)))
	      (if c
		  (push (list nflit nil rellits)
			(cdr c))
		(push (list ev (list nflit nil rellits))
		      ev-constraints))))))
      (dotimes (i max-constraint-size)
	(setq ev-constraints
	      (mapcar
	       #'(lambda (ev-constr)
		   (let ((ev (car ev-constr))
			 (constrs (cdr ev-constr)))
		     (setq constrs
			   (apply #'append
				  (mapcar #'(lambda (c)
					      (let ((lit (car c))
						    (lits (cadr c))
						    (rellits (caddr c)))
						(cons c ; leave old one, and add new ones
						      (if (= (length lits) i)
							  (let ((stamps (mapcar #'(lambda (lit0)
										    (let ((node (get (literal-name lit0) 'ext-exp-open-dag)))
										      (if (and node (ext-exp-open-dag-p node))
											  (ext-exp-open-dag-stamp node)
											0)))
										lits)))
							    (mapcar #'(lambda (newlit)
									(list lit (cons newlit lits)
									      (intersection
									       rellits
									       (conjunctively-related-literals
										#'(lambda (y) t)
										newlit))))
								    (remove-if #'(lambda (newlit)
										   (let* ((node (get (literal-name newlit) 'ext-exp-open-dag))
											  (stamp (if (and node (ext-exp-open-dag-p node))
												     (ext-exp-open-dag-stamp node)
												   0)))
										     (find-if #'(lambda (s) (<= s stamp)) stamps)))
									       rellits)))
							nil))))
					  constrs)))
		     (cons ev constrs)))
	       ev-constraints)))
      (setq ev-constraints
	    (mapcar #'(lambda (ev-constr)
			(cons (car ev-constr)
			      (remove-if #'(lambda (c)
					     (or
					; remove trivial constrains (w/ only FULL or EMPTY soln)
					      (and (null (cadr c))
						   (let ((wff (jform-represents (car c))))
						     (eeod-pattern-p-4 wff (get (car ev-constr) 'banned-sel-vars))))
					; remove those that don't share a banned var
					      (and BAD-VAR-CONNECTED-PRUNE
						   (not
						    (lits-sel-conn-p (cons (car c) (cadr c))
								     (intersection
								      (free-vars-of (literal-represents (car c)))
								      (get (car ev-constr) 'banned-sel-vars))
								     (get (car ev-constr) 'banned-sel-vars))))))
					 (cdr ev-constr))))
		    ev-constraints))
      (setq ev-constraints
	    (mapcar #'(lambda (ev-constr)
			(cons (car ev-constr)
			      (sort
			       (constraint-sets-up-to (cdr ev-constr) max-num-constraints)
			       #'constraint-order)))
		    ev-constraints))
      (when ev-constraints
	(ext-saturation-full-exps)
	(dolist (ev-constrs ev-constraints)
	  (push (car ev-constrs) ext-set-vars-constraints)
	  (when ms03-verbose
	    (msgf (length (cdr ev-constrs)) " upper bound constraints generated for " ((car ev-constrs) . gwff) t))
	  (dolist (c (cdr ev-constrs))
	    (when ms03-verbose
	      (msgf "---------")
	      (dolist (l c)
		(msgf (car l) " -> " (cadr l) t)))
	    (ext-saturation-search-solve-constraints (car ev-constrs) c 'MAX)))
	(ext-saturation-min-exps)))))

(defun ext-saturation-search-solve-constraints (v constrs maxmin)
  (declare (special ext-all-lemmas ext-crucial-arcs))
  (let* ((substitutable-vars (eeod-exp-vars *current-edag*))
	 (misc-vars (append (eeod-sel-vars *current-edag*) substitutable-vars))
	 (vsel (fresh-var (unabbreviated-type v) (getnameroot v)))
	 (ev2 (eeod-duplicate-expvar v))
	 (problem nil)
	 (arc-assoc nil)
	 (node-assoc nil)
	 (rec-flag nil)
	 (paths nil)
	 (misc-occurs nil)
	 (banned (get ev2 'banned-sel-vars))
	 (banned-occurs nil))
    (declare (special node-assoc))
    (eeod-subst-deepen (acons ev2 vsel nil) *current-edag*)
    (setq arc-assoc (eeod-var-exparc-assoc *current-edag* v vsel))
    (dolist (arcpair arc-assoc)
      (eeod-node-assoc-1 (ext-exp-open-arc-node (car arcpair))
			 (ext-exp-open-arc-node (cdr arcpair))))
    (dolist (constr constrs)
      (let* ((mainlit (car constr))
	     (aux-lits (cadr constr))
	     (mainnode (or (cdr (assoc (name-to-eeod (literal-name mainlit)) node-assoc))
			   (name-to-eeod (literal-name mainlit))))
	     (aux-nodes (mapcar #'(lambda (lit)
				    (or (cdr (assoc (name-to-eeod (literal-name lit)) node-assoc))
					(name-to-eeod (literal-name lit))))
				aux-lits))
	     (path nil)
	     (banned-occs nil))
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
	    (when ms03-verbose
	      (if (eq maxmin 'MAX)
		  (msgf "Problem solving upper bound constraint " t mainlit " -> " aux-lits t)
		(msgf "Problem solving lower bound constraint " t aux-lits " -> " mainlit t)))))))
    (unless problem
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
	      (eeod-generalized-connection (car conn) (cdr conn)))
	    (let ((lemm (list lemmas pose (mapcar #'cdr arc-assoc) negf clist1)))
	      (push lemm ext-all-lemmas)
	      (ext-exp-open-dag-add-lemma lemmas pose negf clist1)
	      (let ((selvars (eeod-sel-vars *current-edag*)))
		(let ((lexps (eeod-get-nodes #'(lambda (x)
						 (eq (ext-exp-open-dag-kind x) 'EXP))
					     pose)))
		  (dolist (lexp lexps)
		    (dolist (larc (ext-exp-open-dag-arcs lexp))
		      (let ((trm (ext-exp-open-arc-exp-term larc)))
			(when (member trm selvars)
			  (push larc ext-crucial-arcs)))))))
	      )))))))

(defun constraint-order (c1 c2)
  (< (constraint-disj-val c1) (constraint-disj-val c2)))

(defun constraint-disj-val (c)
  (if c
      (/ (apply #'+ (mapcar #'constraint-disj-val-1 c))
	 (length c))
    1))

(defun constraint-disj-val-1 (l)
  (let ((lit (car l))
	(lits (cadr l)))
    (+ (ext-jform-disj-val lit)
       (if lits
	   (apply #'+ (mapcar #'ext-jform-disj-val lits))
	 0))))

(defun constraint-sets-up-to (l n)
  (remove nil (constraint-sets-up-to-1 l n)))

(defun constraint-sets-up-to-1 (l n)
  (if (<= n 0)
      (list nil)
    (let* ((k (- n 1))
	   (ps (constraint-sets-up-to-1 l k))
	   (ret nil))
      (dolist (s ps ret)
	(push s ret)
	(when (= (length s) k)
	  (dolist (a l)
	    (unless (or (member a s :test 
				#'(lambda (x y)
				    (and (eq (car x) (car y))
					 (or (subsetp (cadr x) (cadr y))
					     (subsetp (cadr y) (cadr x))))))
			(member (cons a s) ret
				:test #'(lambda (x y)
					  (and (subsetp x y)
					       (subsetp y x)))))
	      (push (cons a s) ret))))))))

(defun ext-saturation-search-option-rigid-mates (pnode nnode aux-eqns pddepth nddepth)
  (declare (special ext-level ext-options))
  (let ((nodes (list pnode nnode)))
    (unless (or (find-if #'(lambda (x)
			     (and (eq (car x) 'MATE-RIGID)
				  (equal (nth 3 x) nodes)))
			 ext-options)
		(eeod-mated-p pnode nnode))
      (multiple-value-bind
	  (v insts)
	  (ext-quick-eunification-weight (mapcar #'(lambda (x y)
						     (list (unabbreviated-type x) x y nil))
						 (args (ext-exp-open-dag-shallow pnode))
						 (args (ext-exp-open-dag-shallow nnode)))
					 aux-eqns)
	(push (cons (* pddepth nddepth) 'MS03-WEIGHT-DISJ-MATE) v)
	(push MS03-WEIGHT-RIGID-MATE v)
	(setq ext-options
	      (merge 'list
		     (list (list 'MATE-RIGID (eeod-exp-arcs-above-l (list pnode nnode))
				 (evaluate-ext-weight (list ext-level v))
				 (list pnode nnode)
				 nil insts nil v))
		     ext-options
		     #'ext-options-order))))))

; only uses IMITATION - projections are primsubs
(defun ext-saturation-search-option-flex-rigid-mates (flexnode rignode aux-eqns pddepth nddepth)
  (declare (special ext-level ext-options))
  (let* ((nodes (list flexnode rignode))
	 (rig (ext-exp-open-dag-shallow rignode))
	 (flex (ext-exp-open-dag-shallow flexnode))
	 (ev (head flex))
	 (h (head rig))
	 (h2 (if (eq (ext-exp-open-dag-positive rignode)
		     (ext-exp-open-dag-positive flexnode))
		 'NOT
	       h))
	 (wff2 nil)
	 (evl2 nil))
    (if (member h (get ev 'banned-sel-vars))
	(unless (or (member h (get ev 'already-duped-for))
		    (find-if #'(lambda (x)
				 (and (eq (car x) 'DUP-VAR-FOR-BANNED)
				      (eq (nth 3 x) ev)
				      (eq (nth 4 x) h)))
			     ext-options))
	  (setq ext-options (merge 'list
				   (list (list 'DUP-VAR-FOR-BANNED (eeod-exp-arcs-above-l (list rignode flexnode))
					       (+ ext-level MS03-WEIGHT-DUP-VAR)
					       ev h))
				   ext-options
				   #'ext-options-order)))
      (unless (or (find-if #'(lambda (x)
			       (and (eq (car x) 'MATE-FLEXRIGID)
				    (equal (nth 3 x) nodes)))
			   ext-options)
		  (member h2 (get ev 'banned-imitations)
			  :test #'equal))
	(multiple-value-setq
	    (wff2 evl2)
	  (create-imit-subst ev h))
	(multiple-value-bind
	    (v insts)
	    (ext-quick-eunification-weight (mapcar #'(lambda (x y)
						       (list (unabbreviated-type x) x y nil))
						   (args (etanorm (lambda-norm (substitute-l-term-var wff2 ev flex))))
						   (args rig))
					   aux-eqns)
	  (push (cons (* pddepth nddepth) 'MS03-WEIGHT-DISJ-MATE) v)
	  (push MS03-WEIGHT-FLEXRIGID-MATE v)
	  (setq ext-options
		(merge 'list
		       (list (list 'MATE-FLEXRIGID (eeod-exp-arcs-above-l nodes)
				   (evaluate-ext-weight (list ext-level v))
				   nodes nil insts evl2 v))
		       ext-options
		       #'ext-options-order)))))))

(defun ext-saturation-search-option-eunif (pnode nnode aux-eqns pddepth nddepth)
  (declare (special ext-level ext-options))
  (let* ((nodes (list pnode nnode))
	 (poseq (ext-exp-open-dag-shallow pnode))
	 (negeq (ext-exp-open-dag-shallow nnode))
	 (lft1 (cdar poseq))
	 (rght1 (cdr poseq))
	 (lft2 (cdar negeq))
	 (rght2 (cdr negeq))
	 (tp (cdr (unabbreviated-type (caar poseq)))))
    (unless (or (find-if #'(lambda (x)
			     (and (eq (car x) 'EUNIF1)
				  (equal (nth 3 x) nodes)))
			 ext-options)
		(eeod-eunif1-p pnode nnode))
      (multiple-value-bind
	  (v insts)
	  (ext-quick-eunification-weight
	   (list (list tp lft1 lft2) (list tp rght1 rght2))
	   aux-eqns)
	(push (cons (* pddepth nddepth) 'MS03-WEIGHT-DISJ-EUNIF) v)
	(push MS03-WEIGHT-EUNIF1 v)
	(setq ext-options
	      (merge 'list
		     (list (list 'EUNIF1 (eeod-exp-arcs-above-l (list pnode nnode))
				 (evaluate-ext-weight (list ext-level v))
				 (list pnode nnode) nil insts nil v))
		     ext-options
		     #'ext-options-order))))
    (unless (or (find-if #'(lambda (x)
			     (and (eq (car x) 'EUNIF2)
				  (equal (nth 3 x) nodes)))
			 ext-options)
		(eeod-eunif2-p pnode nnode))
      (multiple-value-bind
	  (v insts)
	  (ext-quick-eunification-weight
	   (list (list tp lft1 rght2) (list tp rght1 lft2))
	   aux-eqns)
	(push (cons (* pddepth nddepth) 'MS03-WEIGHT-DISJ-EUNIF) v)
	(push MS03-WEIGHT-EUNIF2 v)
	(setq ext-options
	      (merge 'list
		     (list (list 'EUNIF2 (eeod-exp-arcs-above-l (list pnode nnode))
				 (evaluate-ext-weight (list ext-level v))
				 (list pnode nnode) nil insts nil v))
		     ext-options
		     #'ext-options-order))))))

(defun ext-saturation-search-option-unif (flex rig node aux-eqns nddepth)
  (declare (special ext-level ext-options))
  (let* ((fh (head flex))
	 (rh (head rig))
	 (nodes (list node)))
    (unless (find-if #'(lambda (x)
			 (and (eq (car x) 'IMIT)
			      (eq (nth 3 x) fh)
			      (eq (nth 4 x) rh)))
		     ext-options)
      (when (ext-imitation-heads-possible-p fh rh)
	(multiple-value-bind
	    (wff evl)
	    (create-imit-subst fh rh)
	  (multiple-value-bind
	      (v insts)
	      (ext-quick-eunification-weight
	       (list (list (unabbreviated-type rig)
			   (lambda-norm (substitute-l-term-var wff fh flex))
			   (lambda-norm (substitute-l-term-var wff fh rig))
			   nil))
	       aux-eqns)
	    (push (cons nddepth 'MS03-WEIGHT-DISJ-UNIF) v)
	    (push MS03-WEIGHT-IMITATE v)
	    (setq ext-options
		  (merge 'list
			 (list (list 'IMIT (eeod-exp-arcs-above node)
				     (evaluate-ext-weight (list ext-level v))
				     fh rh insts evl v))
			 ext-options
			 #'ext-options-order))))))
    (when (and (member rh (get fh 'banned-sel-vars))
	       (not (member rh (get fh 'already-duped-for)))
	       (not (find-if #'(lambda (x)
				 (and (eq (car x) 'DUP-VAR-FOR-BANNED)
				      (eq (nth 3 x) fh)
				      (eq (nth 4 x) rh)))
			     ext-options)))
      (setq ext-options (merge 'list
			       (list (list 'DUP-VAR-FOR-BANNED (eeod-exp-arcs-above node)
					   (+ ext-level MS03-WEIGHT-DUP-VAR)
					   fh rh))
			       ext-options
			       #'ext-options-order)))
    (let ((projs (ext-possible-var-projections fh)))
      (dolist (i projs)
	(unless (find-if #'(lambda (x)
			     (and (eq (car x) 'PROJ)
				  (eq (nth 3 x) fh)
				  (equal (nth 4 x) i)))
			 ext-options)
	  (multiple-value-bind
	      (wff evl)
	      (create-proj-subst fh i)
	    (multiple-value-bind
		(v insts)
		(ext-quick-eunification-weight
		 (list (list (unabbreviated-type rig)
			     (lambda-norm (substitute-l-term-var wff fh flex))
			     (lambda-norm (substitute-l-term-var wff fh rig))
			     nil))
		 aux-eqns)
	      (push (cons nddepth 'MS03-WEIGHT-DISJ-UNIF) v)
	      (push MS03-WEIGHT-PROJECT v)
	      (setq ext-options
		    (merge 'list
			   (list (list 'PROJ (eeod-exp-arcs-above node)
				       (evaluate-ext-weight (list ext-level v))
				       fh i insts evl v))
			   ext-options
			   #'ext-options-order)))))))))

(defun ext-saturation-search-option-primsub (node)
  (declare (special ext-level ext-options ext-next-proj ext-next-forall ext-next-exists ext-next-or
		    ext-next-and ext-next-equals ext-next-not-equals
		    ext-generated-primsubs))
  (let* ((flex (ext-exp-open-dag-shallow node))
	 (ev (head flex))
	 (nodes (list node)))
    (unless (member ev ext-generated-primsubs)
      (push ev ext-generated-primsubs)
      (when ms03-verbose
	(msgf "Generating Primsubs for " (ev . gwff)))
      (let ((projs (ext-possible-var-projections ev)))
	(dolist (i projs)
	  (unless (find-if #'(lambda (x)
			       (and (eq (car x) 'PRIMSUB-PROJ)
				    (equal (nth 3 x) ev)
				    (equal (nth 4 x) i)))
			   ext-options)
	    (setq ext-options
		  (merge 'list
			 (list (list 'PRIMSUB-PROJ (eeod-exp-arcs-above node)
				     (+ ext-level MS03-WEIGHT-PRIMSUB-FIRST-PROJ
					(* ext-next-proj MS03-WEIGHT-PRIMSUB-NEXT-PROJ))
				     ev i ext-next-proj))
			 ext-options #'ext-options-order))
	    (incf ext-next-proj))))
      (let ((projs (ext-possible-var-not-projections ev)))
	(dolist (i projs)
	  (unless (find-if #'(lambda (x)
			       (and (eq (car x) 'PRIMSUB-NOT-PROJ)
				    (equal (nth 3 x) ev)
				    (equal (nth 4 x) i)))
			   ext-options)
	    (setq ext-options
		  (merge 'list
			 (list (list 'PRIMSUB-NOT-PROJ (eeod-exp-arcs-above node)
				     (+ ext-level MS03-WEIGHT-PRIMSUB-FIRST-NOT-PROJ
					(* ext-next-not-proj MS03-WEIGHT-PRIMSUB-NEXT-NOT-PROJ))
				     ev i ext-next-not-proj))
			 ext-options #'ext-options-order))
	    (incf ext-next-not-proj))))
      (unless (or (member 'ALL (get ev 'banned-forall)) (not *individual-types*))
	(let ((qtp (next-primsub-type *individual-types* (get ev 'banned-forall))))
	  (unless (find-if #'(lambda (x)
			       (and (eq (car x) 'PRIMSUB-FORALL)
				    (equal (nth 3 x) ev)
				    (equal (nth 4 x) qtp)))
			   ext-options)
	    (setq ext-options
		  (merge 'list
			 (list (list 'PRIMSUB-FORALL (eeod-exp-arcs-above node)
				     (+ ext-level MS03-WEIGHT-PRIMSUB-FIRST-FORALL
					(* ext-next-forall MS03-WEIGHT-PRIMSUB-NEXT-FORALL))
				     ev qtp ext-next-forall))
			 ext-options #'ext-options-order))
	    (incf ext-next-forall))))
      (unless (or (member 'ALL (get ev 'banned-exists)) (not *individual-types*))
	(let ((qtp (next-primsub-type *individual-types* (get ev 'banned-exists))))
	  (unless (find-if #'(lambda (x)
			       (and (eq (car x) 'PRIMSUB-EXISTS)
				    (equal (nth 3 x) ev)
				    (equal (nth 4 x) qtp)))
			   ext-options)
	    (setq ext-options
		  (merge 'list
			 (list (list 'PRIMSUB-EXISTS (eeod-exp-arcs-above node)
				     (+ ext-level MS03-WEIGHT-PRIMSUB-FIRST-EXISTS
					(* ext-next-exists MS03-WEIGHT-PRIMSUB-NEXT-EXISTS))
				     ev qtp ext-next-exists))
			 ext-options #'ext-options-order))
	    (incf ext-next-exists))))
      (if (ext-exp-open-dag-positive node)
	  (unless (or (member 'FALSEHOOD (get ev 'banned-imitations))
		      (find-if #'(lambda (x)
				   (and (eq (car x) 'PRIMSUB-FALSEHOOD)
					(equal (nth 3 x) ev)))
			       ext-options))
	    (setq ext-options
		  (merge 'list
			 (list (list 'PRIMSUB-FALSEHOOD (eeod-exp-arcs-above node)
				     (+ ext-level MS03-WEIGHT-PRIMSUB-FALSEHOOD)
				     ev))
			 ext-options #'ext-options-order)))
	(unless (or (member 'TRUTH (get ev 'banned-imitations))
		    (find-if #'(lambda (x)
				 (and (eq (car x) 'PRIMSUB-TRUTH)
				      (equal (nth 3 x) ev)))
			     ext-options))
	  (setq ext-options
		(merge 'list
		       (list (list 'PRIMSUB-TRUTH (eeod-exp-arcs-above node)
				   (+ ext-level MS03-WEIGHT-PRIMSUB-TRUTH)
				   ev))
		       ext-options #'ext-options-order))))
      (unless (or (member 'AND (get ev 'banned-imitations))
		  (find-if #'(lambda (x)
			       (and (eq (car x) 'PRIMSUB-AND)
				    (equal (nth 3 x) ev)))
			   ext-options))
	(setq ext-options
	      (merge 'list
		     (list (list 'PRIMSUB-AND (eeod-exp-arcs-above node)
				 (+ ext-level MS03-WEIGHT-PRIMSUB-FIRST-AND
				    (* ext-next-and MS03-WEIGHT-PRIMSUB-NEXT-AND))
				 ev ext-next-and))
		     ext-options #'ext-options-order))
	(incf ext-next-and))
      (unless (or (member 'OR (get ev 'banned-imitations))
		  (find-if #'(lambda (x)
			       (and (eq (car x) 'PRIMSUB-OR)
				    (equal (nth 3 x) ev)))
			   ext-options))
	(setq ext-options
	      (merge 'list
		     (list (list 'PRIMSUB-OR (eeod-exp-arcs-above node)
				 (+ ext-level MS03-WEIGHT-PRIMSUB-FIRST-OR
				    (* ext-next-or MS03-WEIGHT-PRIMSUB-NEXT-OR))
				 ev ext-next-or))
		     ext-options #'ext-options-order))
	(incf ext-next-or))
					; eqns and noneqns
      (dolist (tp *individual-types*)
	(let ((q (inherit-abbrev '= (acons 'O tp tp) (list tp))))
	  (unless (or (member q (get ev 'banned-imitations))
		      (find-if #'(lambda (x)
				   (and (eq (car x) 'PRIMSUB-EQUALS)
					(equal (nth 3 x) ev)
					(equal (nth 4 x) q)))
			       ext-options))
	    (setq ext-options
		  (merge 'list
			 (list (list 'PRIMSUB-EQUALS (eeod-exp-arcs-above node)
				     (+ ext-level MS03-WEIGHT-PRIMSUB-FIRST-EQUALS
					(* ext-next-equals MS03-WEIGHT-PRIMSUB-NEXT-EQUALS))
				     ev q ext-next-equals))
			 ext-options
			 #'ext-options-order))
	    (incf ext-next-equals))
	  (unless (or (member 'NOT (get ev 'banned-imitations))
		      (find-if #'(lambda (x)
				   (and (eq (car x) 'PRIMSUB-NOT-EQUALS)
					(eq (nth 3 x) ev)
					(eq (nth 4 x) q)))
			       ext-options))
	    (setq ext-options
		  (merge 'list
			 (list (list 'PRIMSUB-NOT-EQUALS (eeod-exp-arcs-above node)
				     (+ ext-level MS03-WEIGHT-PRIMSUB-FIRST-NOT-EQUALS
					(* ext-next-not-equals MS03-WEIGHT-PRIMSUB-NEXT-NOT-EQUALS))
				     ev q ext-next-not-equals))
			 ext-options
			 #'ext-options-order))
	    (incf ext-next-not-equals)))))))

(defun eeod-solve-flex-flex (&optional (node *current-edag*))
  (let ((const-assoc nil)
	(expvars (eeod-exp-vars node)))
    (dolist (ev expvars)
      (do ((tp (unabbreviated-type ev) (car tp)))
	  ((not (consp tp))
	   (let* ((a (assoc tp const-assoc :test #'equal))
		  (c (if a (cdr a)
		       (let ((c2 (fresh-var tp '|w|)))
			 (push (cons tp c2) const-assoc)
			 c2))))
	     (eeod-subst-imit ev c)))))))

(defun eeod-add-all-rigid-mates-rec ()
  (do ((changed (eeod-add-all-rigid-mates) (eeod-add-all-rigid-mates)))
      ((not changed))
    (setq *ext-rigid-jform* (eeod-to-jform *current-edag* :posflex nil :negflex nil :flexflex nil))))

; this gives a way to duplicate an expansion variable
; which may be used in instantiation for several expansion nodes.
; currently, it does not copy mate and eunif arcs - there is code
; to try to do this, but it is commented out since there are problems
; when sel nodes are beneath the relevant expansions.
(defun eeod-duplicate-expvar (ev)
  (let ((ev2 (fresh-var (type ev) (getnameroot ev)))
	(selvars nil))
    (setf (get ev2 'ext-exp-var) t)
    (dolist (arc (get ev 'ext-exp-var-arcs))
      (let* ((theta (acons ev ev2 nil))
	     (expnode (ext-exp-open-arc-parent arc))
	     (node-assoc nil)
	     (nodes-delayed nil))
	(declare (special theta node-assoc nodes-delayed))
	(let ((arc2 (eeod-copy-duplicate-arc arc expnode)))
	  (push arc2 (ext-exp-open-dag-arcs expnode))
;	  (eeod-copy-duplicate-delayed)
	  )))
    ev2))

(defun eeod-copy-duplicate-delayed ()
  (declare (special nodes-delayed))
  (loop while nodes-delayed do
	(let ((min-delayed (car nodes-delayed)))
	  (dolist (a (cdr nodes-delayed))
	    (when (< (ext-exp-open-dag-stamp (car a)) (ext-exp-open-dag-stamp (car min-delayed)))
	      (setq min-delayed a)))
	  (setq nodes-delayed (remove min-delayed nodes-delayed))
	  (let* ((kid (car min-delayed)) ; in the old tree
		 (arc (cadr min-delayed)) ; in the old tree
		 (parent (caddr min-delayed)) ; in the new tree
		 (arc2 (eeod-copy-duplicate-arc arc parent))
		 (kid2 (ext-exp-open-arc-node arc2))
		 (other-arc (car (remove arc (ext-exp-open-dag-parent-arcs kid))))
		 (arc3 (copy-ext-exp-open-arc other-arc))
		 (other-parent (ext-exp-open-arc-parent other-arc)))
	    (push arc2 (ext-exp-open-dag-arcs parent))
	    (setf (ext-exp-open-arc-node arc3) kid2)
	    (push arc3 (ext-exp-open-dag-parent-arcs kid2))
	    (push arc3 (ext-exp-open-dag-arcs other-parent))))))

(defun eeod-copy-duplicate-arc (arc parent)
  (declare (special theta))
  (let ((arc2 (copy-ext-exp-open-arc arc)))
    (setf (ext-exp-open-arc-parent arc2) parent)
    (cond ((eq (ext-exp-open-arc-kind arc) 'SEL)
	   (let* ((sv (ext-exp-open-arc-sel-var arc))
		  (sv2 (fresh-var (type sv) (getnameroot sv)))
		  (wff (ext-exp-open-dag-shallow parent))
		  (expvars (eeod-exp-vars-above parent)))
	     (setq theta (acons sv sv2 theta))
	     (setf (get sv2 'exp-vars-above) expvars)
	     (dolist (ev expvars)
	       (push sv2 (get ev 'banned-sel-vars)))
	     (setf (ext-exp-open-arc-sel-var arc2) sv2)))
	  ((eq (ext-exp-open-arc-kind arc) 'EXP)
	   (let* ((trm (simul-substitute-l-term-var theta (ext-exp-open-arc-exp-term arc)))
		  (frees (ext-exp-vars-of trm)))
	     (dolist (v frees)
	       (push arc2 (get v 'ext-exp-var-arcs)))
	     (setf (ext-exp-open-arc-exp-term arc2) trm))))
    (setf (ext-exp-open-arc-node arc2)
	  (eeod-copy-duplicate-node (ext-exp-open-arc-node arc) arc2))
    arc2))

(defun eeod-copy-duplicate-node (node parent)
  (declare (special theta node-assoc nodes-delayed))
  (let ((node2 (copy-eeod node))
	(k (ext-exp-open-dag-kind node)))
    (setf (ext-exp-open-dag-parent-arcs node2) (list parent))
    (setf (ext-exp-open-dag-shallow node2)
	  (simul-substitute-l-term-var
	   theta (ext-exp-open-dag-shallow node)))
    (if (or (eq k 'ATOM)
	    (and (eq k 'EQN) (ext-exp-open-dag-positive node))
	    (eq k 'EQNGOAL))
	(progn
	  (setf (ext-exp-open-dag-arcs node2) nil)
;	  (dolist (arc (ext-exp-open-dag-arcs node))
;	    (let* ((kid (ext-exp-open-arc-node arc))
;		   (a (assoc kid nodes-delayed)))
;	      (if a
;		  (let* ((arc1 (eeod-copy-duplicate-arc (cadr a) (caddr a)))
;			 (node3 (ext-exp-open-arc-node arc1))
;			 (arc2 (copy-ext-exp-open-arc arc)))
;		    (setq nodes-delayed (remove a nodes-delayed :test #'equal))
;		    (setf (ext-exp-open-arc-parent arc2) node2)
;		    (setf (ext-exp-open-arc-node arc2)
;			  (ext-exp-open-arc-node arc1)))
;		(push (list kid arc node2) nodes-delayed))))
	  )
      (setf (ext-exp-open-dag-arcs node2)
	    (mapcar #'(lambda (arc)
			(eeod-copy-duplicate-arc arc node2))
		    (ext-exp-open-dag-arcs node))))
    (push (cons node node2) node-assoc)
    node2))

(defun eeod-duplicate-jform (ev ev2 node-assoc jform)
  (case (jform-type jform)
    (conjunction
     (make-conjunction :components
		       (mapcar #'(lambda (j)
				   (eeod-duplicate-jform ev ev2 node-assoc j))
			       (conjunction-components jform))))
    (disjunction
     (make-disjunction :components
		       (mapcar #'(lambda (j)
				   (eeod-duplicate-jform ev ev2 node-assoc j))
			       (disjunction-components jform))))
    (literal jform)
    (universal
     (let ((qvars (universal-qvars jform)))
       (if (member ev qvars)
	   (let ((jform2 
		  (make-universal :qvars (cons ev2 (remove ev qvars))
				  :scope
				  (eeod-duplicate-jform-1
				   node-assoc
				   (universal-scope jform)))))
	     (make-conjunction :components (list jform jform2)))
	 (make-universal :qvars qvars
			 :scope
			 (eeod-duplicate-jform ev ev2 node-assoc
					       (universal-scope jform))))))))

(defun eeod-duplicate-jform-1 (node-assoc jform)
  (case (jform-type jform)
    (conjunction
     (make-conjunction :components
		       (mapcar #'(lambda (j)
				   (eeod-duplicate-jform-1 node-assoc j))
			       (conjunction-components jform))))
    (disjunction
     (make-disjunction :components
		       (mapcar #'(lambda (j)
				   (eeod-duplicate-jform-1 node-assoc j))
			       (disjunction-components jform))))
    (literal
     (let* ((node (name-to-eeod (literal-name jform)))
	    (node2 (cdr (assoc node node-assoc)))
	    (name (ext-exp-open-dag-name node2)))
       (make-literal :name name :represents (ext-exp-open-dag-shallow node2)
		     :pos (ext-exp-open-dag-positive node2))))
    (universal
     (make-universal :qvars (universal-qvars jform)
		     :scope
		     (eeod-duplicate-jform-1 node-assoc (universal-scope jform))))))

; lifting - gather search info for eed
(defun ext-saturation-valid-lift (op)
  (declare (special *edag-lift-info*))
  (case (car op)
    ((MATE-RIGID MATE-FLEXRIGID)
     (let* ((nodes (nth 3 op))
	    (node1 (car nodes))
	    (node2 (cadr nodes))
	    (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (when (eq (cadar a) node1)
	   (let* ((eed (caar a))
		  (eed1 (if (and (eq (car op) 'MATE-FLEXRIGID)
				 (eq (ext-exp-dag-kind eed) 'NEG))
			    (ext-exp-arc-node (car (ext-exp-dag-arcs eed)))
			  eed)))
	     (do ((b (get *edag-lift-info* 'node-assoc) (cdr b)))
		 ((or (null b) found))
	       (when (eq (cadar b) node2)
		 (setq found
		       (intersection
			(mapcar #'(lambda (arc) (ext-exp-arc-node arc))
				(ext-exp-dag-arcs eed1))
			(mapcar #'(lambda (arc) (ext-exp-arc-node arc))
				(ext-exp-dag-arcs (caar b))))))))))))
    ((EUNIF1 EUNIF2)
     (let* ((nodes (nth 3 op))
	    (node1 (car nodes))
	    (node2 (cadr nodes))
	    (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (when (eq (cadar a) node1)
	   (do ((b (get *edag-lift-info* 'node-assoc) (cdr b)))
	       ((or (null b) found))
	     (when (eq (cadar b) node2)
	       (setq found
		     (intersection
		      (mapcar #'(lambda (arc) (ext-exp-arc-node arc))
			      (remove-if-not
			       #'(lambda (arc)
				   (eq (ext-exp-arc-kind arc) (car op)))
			       (ext-exp-dag-arcs (caar a))))
		      (mapcar #'(lambda (arc) (ext-exp-arc-node arc))
			      (remove-if-not
			       #'(lambda (arc)
				   (eq (ext-exp-arc-kind arc) (car op)))
			       (ext-exp-dag-arcs (caar b))))))))))))
    (IMIT
     (let ((ev (nth 3 op))
	   (h (nth 4 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when (and p (eq h (head (cdr p))))
	     (setq found t))))))
    ((PROJ PRIMSUB-PROJ)
     (let ((ev (nth 3 op))
	   (i (nth 4 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (let ((cl (classify-lambda-term (cdr p))))
	       (when (and (eq (car cl) 'PROJECTION)
			  (equal (cadr cl) i))
		 (setq found t))))))))
					; since quant primsubs generate types, we may need to use this primsub just to get to the one
					; with the type we really want
    (PRIMSUB-FORALL
     (let ((ev (nth 3 op))
	   (tp (nth 4 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (let ((cl (classify-lambda-term (cdr p))))
	       (when (and (eq (car cl) 'FORALL)
			  (not (member (cadr cl) (get ev 'banned-forall))))
		 (setq found t))))))))
    (PRIMSUB-EXISTS
     (let ((ev (nth 3 op))
	   (tp (nth 4 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (let ((cl (classify-lambda-term (cdr p))))
	       (when (and (eq (car cl) 'EXISTS)
			  (not (member (cadr cl) (get ev 'banned-exists))))
		 (setq found t))))))))
    (PRIMSUB-AND
     (let ((ev (nth 3 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (let ((cl (classify-lambda-term (cdr p))))
	       (when (eq (car cl) 'AND)
		 (setq found t))))))))
    (PRIMSUB-OR
     (let ((ev (nth 3 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (let ((cl (classify-lambda-term (cdr p))))
	       (when (eq (car cl) 'OR)
		 (setq found t))))))))
    (PRIMSUB-TRUTH
     (let ((ev (nth 3 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (let ((cl (classify-lambda-term (cdr p))))
	       (when (eq (car cl) 'TRUTH)
		 (setq found t))))))))
    (PRIMSUB-FALSEHOOD
     (let ((ev (nth 3 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (let ((cl (classify-lambda-term (cdr p))))
	       (when (eq (car cl) 'FALSEHOOD)
		 (setq found t))))))))
    (PRIMSUB-EQUALS
     (let ((ev (nth 3 op))
	   (q (nth 4 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (let ((cl (classify-lambda-term (cdr p))))
	       (when (and (eq (car cl) 'EQUALS)
			  (equal (cdr (unabbreviated-type q)) (cadr cl)))
		 (setq found t))))))))
    (PRIMSUB-NOT-EQUALS
     (let ((ev (nth 3 op))
	   (q (nth 4 op))
	   (found nil))
       (do ((a (get *edag-lift-info* 'node-assoc) (cdr a)))
	   ((or (null a) found) found)
	 (let ((p (assoc ev (caddar a))))
	   (when p
	     (do ((wff (cdr p) (cdr wff)))
		 ((not (lambda-bd-p wff))
		  (when (and (not-p wff)
			     (equals-p (cdr wff))
			     (eq q (head (cdr wff))))
		    (setq found t)))))))))
    (t t)))

(defun ext-saturation-option-flag-info (op)
  (declare (special ext-level))
  (case (car op)
    ((MATE-RIGID MATE-FLEXRIGID EUNIF1 EUNIF2 IMIT PROJ)
     (list ext-level (nth 7 op)))
    (PRIMSUB-PROJ
     (list ext-level (list 'MS03-WEIGHT-PRIMSUB-FIRST-PROJ
			   (cons (nth 5 op) 'MS03-WEIGHT-PRIMSUB-NEXT-PROJ))))
    (PRIMSUB-FORALL
     (list ext-level (list 'MS03-WEIGHT-PRIMSUB-FIRST-FORALL
			   (cons (nth 5 op) 'MS03-WEIGHT-PRIMSUB-NEXT-FORALL))))
    (PRIMSUB-EXISTS
     (list ext-level (list 'MS03-WEIGHT-PRIMSUB-FIRST-EXISTS
			   (cons (nth 5 op) 'MS03-WEIGHT-PRIMSUB-NEXT-EXISTS))))
    (PRIMSUB-AND
     (list ext-level (list 'MS03-WEIGHT-PRIMSUB-FIRST-AND
			   (cons (nth 4 op) 'MS03-WEIGHT-PRIMSUB-NEXT-AND))))
    (PRIMSUB-OR
     (list ext-level (list 'MS03-WEIGHT-PRIMSUB-FIRST-OR
			   (cons (nth 4 op) 'MS03-WEIGHT-PRIMSUB-NEXT-OR))))
    (PRIMSUB-TRUTH (list ext-level (list 'MS03-WEIGHT-PRIMSUB-TRUTH)))
    (PRIMSUB-FALSEHOOD (list ext-level (list 'MS03-WEIGHT-PRIMSUB-FALSEHOOD)))
    (PRIMSUB-EQUALS
     (list ext-level (list 'MS03-WEIGHT-PRIMSUB-FIRST-EQUALS
			   (cons (nth 5 op) 'MS03-WEIGHT-PRIMSUB-NEXT-EQUALS))))
    (PRIMSUB-NOT-EQUALS
     (list ext-level (list 'MS03-WEIGHT-PRIMSUB-FIRST-NOT-EQUALS
			   (cons (nth 5 op) 'MS03-WEIGHT-PRIMSUB-NEXT-NOT-EQUALS))))
    (DUP-VAR-FOR-BANNED
     (list ext-level (list 'MS03-WEIGHT-DUP-VAR)))
    (CHANGE-DUPS
     (list ext-level (list (cons (nth 3 op) 'MS03-WEIGHT-CHANGE-DUPS))))
    (INST (list ext-level (nth 5 op)))))

(defun lift-eed-to-saturation (eed)
  (declare (special *edag-lift-info*))
  (ext-initialize-search (ext-exp-dag-shallow eed))
  (create-ext-exp-open-dag (ext-exp-dag-shallow eed))
  (setq *edag-lift-info* (gensym))
  (setf (get *edag-lift-info* 'eed) eed)
  (setf (get *edag-lift-info* 'selvars) (ext-exp-dag-sel-vars eed))
  (update-edag-lift-info *current-edag*)
  (if (ext-saturation-search-1)
      (progn
	(msgf "Succeeded in lifting proof") ; give some help for flags!
	(lift-eed-to-saturation-flag-suggest (reverse (get *edag-lift-info* 'flag-constraints))))
    (msgf "Failed to lift proof")))

(defun lift-eed-to-saturation-flag-suggest (flag-constraints)
  (let* ((weight-flags '(
			 MS03-WEIGHT-RIGID-MATE 
			 MS03-WEIGHT-FLEXRIGID-MATE
			 MS03-WEIGHT-EUNIF1 MS03-WEIGHT-EUNIF2
			 MS03-WEIGHT-IMITATE MS03-WEIGHT-PROJECT
			 MS03-WEIGHT-FLEXFLEXSAME-O
			 MS03-WEIGHT-FLEXFLEXDIFF-O MS03-WEIGHT-FLEXFLEXSAME MS03-WEIGHT-FLEXFLEXDIFF
			 MS03-WEIGHT-FLEXRIGID-BRANCH MS03-WEIGHT-FLEXRIGID-O MS03-WEIGHT-FLEXRIGID-O
			 MS03-WEIGHT-RIGIDRIGIDSAME-O MS03-WEIGHT-RIGIDRIGIDDIFF-O MS03-WEIGHT-RIGIDRIGID-EQN
			 MS03-WEIGHT-RIGIDRIGID-FLEXEQN MS03-WEIGHT-RIGIDRIGID-NOEQN MS03-WEIGHT-FLEXRIGID-EQN
			 MS03-WEIGHT-FLEXRIGID-FLEXEQN MS03-WEIGHT-FLEXRIGID-NOEQN
			 MS03-WEIGHT-PRIMSUB-FIRST-PROJ MS03-WEIGHT-PRIMSUB-NEXT-PROJ
			 MS03-WEIGHT-PRIMSUB-FIRST-FORALL MS03-WEIGHT-PRIMSUB-FIRST-EXISTS
			 MS03-WEIGHT-PRIMSUB-NEXT-FORALL MS03-WEIGHT-PRIMSUB-NEXT-EXISTS
			 MS03-WEIGHT-PRIMSUB-FIRST-EQUALS MS03-WEIGHT-PRIMSUB-NEXT-EQUALS
			 MS03-WEIGHT-PRIMSUB-FIRST-NOT-EQUALS MS03-WEIGHT-PRIMSUB-NEXT-NOT-EQUALS
			 MS03-WEIGHT-PRIMSUB-FIRST-AND MS03-WEIGHT-PRIMSUB-FIRST-OR
			 MS03-WEIGHT-PRIMSUB-NEXT-AND MS03-WEIGHT-PRIMSUB-NEXT-OR
			 MS03-WEIGHT-OCCURS-CHECK MS03-WEIGHT-BANNED-SELS
			 MS03-WEIGHT-PRIMSUB-FALSEHOOD MS03-WEIGHT-PRIMSUB-TRUTH
			 MS03-WEIGHT-DISJ-UNIF
			 MS03-WEIGHT-DISJ-MATE MS03-WEIGHT-DISJ-EUNIF
			 MS03-WEIGHT-CHANGE-DUPS MS03-WEIGHT-DUP-VAR))
	 (pos-flags nil)
	 (neg-flags nil)
	 (used-flags nil)
	 (sugg-vals nil)
	 (bdvec nil)
	 (matr nil)
	 (maximize nil)
	 (n (length flag-constraints)))
    (when (= n 0) (setq n 1))
    (dolist (c flag-constraints)
      (cond ((eq (car c) '<)
	     (let* ((small (cadr c))
		    (big (caddr c))
		    (m (- (car small) (car big)))
		    (row 
		     (mapcar #'(lambda (f)
				 (let ((z (- (lift-eed-to-saturation-flag-coeff f (cadr big))
					     (lift-eed-to-saturation-flag-coeff f (cadr small)))))
				   (when (and (> z 0) (not (member f pos-flags)))
				     (push f pos-flags))
				   (when (and (< z 0) (not (member f neg-flags)))
				     (push f neg-flags))
				   z))
			     weight-flags)))
	       (push m bdvec) ; lower bound
	       (push row matr)))
	    ((eq (car c) 'MAX)
	     (let* ((m (caadr c))
		    (fl (cadadr c)))
	       (push (mapcar #'(lambda (f)
				 (let ((z (lift-eed-to-saturation-flag-coeff f fl)))
				   (when (and (> z 0) (not (member f pos-flags)))
				     (push f pos-flags))
				   (when (and (< z 0) (not (member f neg-flags)))
				     (push f neg-flags))
				   z))
			     weight-flags)
		     maximize)))))
					; at this point, we could do something like the simplex method,
					; but I'll do something easier (at least for now) -
					; try to choose flag vals to maximize
    (setq used-flags (intersection pos-flags neg-flags)) ; important flags
    (setq pos-flags (set-difference pos-flags used-flags))
    (setq neg-flags (set-difference neg-flags used-flags))
    (dolist (f pos-flags)
      (push (cons f (* n 100))
	    sugg-vals))
    (dolist (f neg-flags)
      (push (cons f 1) sugg-vals))
    (when used-flags
      (let ((maximize2 nil)
	    (pn-flags (append pos-flags neg-flags)))
	(dolist (row matr)
	  (unless (find-if #'(lambda (f)
			       (not (equal (nth (position f weight-flags) row) 0)))
			   pn-flags)
	    (push row maximize2)))
	(dolist (row maximize)
	  (unless (find-if #'(lambda (f)
			       (not (equal (nth (position f weight-flags) row) 0)))
			   pn-flags)
	    (push row maximize2)))
	(let ((fva nil))
	  (dolist (f used-flags)
	    (let ((i (position f weight-flags))
		  (v 0))
	      (dolist (row maximize2)
		(setq v (+ v (nth i row))))
	      (push (cons f v) fva)))
	  (setq fva (sort fva #'(lambda (x y) (< (cdr x) (cdr y)))))
	  (let ((fac (/ n (length used-flags)))
		(j 10))
	    (dolist (fv fva)
	      (push (cons (car fv) (floor j)) sugg-vals)
	      (setq j (+ j fac)))))))
    (msgf "Suggested Values:" t)
    (dolist (fv sugg-vals)
      (msgf (car fv) " : " (cdr fv) t))
    (msgf "Options:" t)
    (msgf "1) Set Flags to these values." t)
    (msgf "2) Create a Mode for these flag values." t)
    (msgf "3) Do Nothing" t)
    (let ((o (get-a-number 3 1)))
      (cond ((equal o 1)
	     (dolist (fv sugg-vals)
	       (set-flag (car fv) (cdr fv))))
	    ((equal o 2)
	     (let ((mode-flags (cons (list 'DEFAULT-MS 'MS03-7)
				     (mapcar #'(lambda (x)
						 (list (car x) (cdr x)))
					     sugg-vals)))
		   (symb (intern (concatenate 'string "MODE-MS03-SUGG"))))
	       (dolist (f global-flaglist)
		 (unless (consp f)
		   (when (intersection (get f 'subjects) '(MS03-7 EXT-SEARCH IMPORTANT))
		     (unless (assoc f mode-flags)
		       (push (list f (eval f)) mode-flags)))))
	       (do ((name nil))
		   ((not (or (null name) (memq name global-modelist)))
		    (eval `(defmode ,name ,(cons 'flag-settings (core::flagsort mode-flags))
			     (mhelp "Mode created by MS03-7 lifting.")))
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
    

(defun lift-eed-to-saturation-flag-coeff (f trms)
  (let ((v 0))
    (dolist (trm trms)
      (if (consp trm)
	  (when (eq (cdr trm) f)
	    (setq v (+ v (car trm))))
	(when (eq trm f)
	  (incf v))))
    v))

(defun update-edag-lift-info (eeod)
  (declare (special *edag-lift-info*))
  (let ((eed (get *edag-lift-info* 'eed))
	(selvars (get *edag-lift-info* 'selvars)))
    (setf (get *edag-lift-info* 'arc-assoc) nil)
    (setf (get *edag-lift-info* 'sel-theta) nil)
    (setf (get *edag-lift-info* 'node-assoc) (list (list eed eeod nil)))
    (lift-eed-to-eeod (list (list eed eeod nil)) selvars)
    ))

(defun lift-eed-to-eeod (nl selvars)
  (declare (special *edag-lift-info*))
  (let ((a (find-if #'(lambda (x)
			(or (member (ext-exp-dag-kind (car x)) '(REW NEG DIS CON IMP SEL TRUE FALSE LEAF DEC))
			    (and (eq (ext-exp-dag-kind (car x)) 'EQN)
				 (not (ext-exp-dag-positive (car x))))))
		    nl)))
    (if a
	(let ((eed (car a))
	      (eeod (cadr a))
	      (theta (caddr a))
	      (nl2 (remove a nl)))
	  (if (and (eq (ext-exp-dag-kind eed) 'REW)
		   (member (ext-exp-dag-rew-just eed) '(LAMBDA EQUIVWFFS)))
	      (let ((x (list (ext-exp-arc-node (car (ext-exp-dag-arcs eed))) eeod theta)))
		(push x (get *edag-lift-info* 'node-assoc))
		(lift-eed-to-eeod (cons x nl2) selvars))
	    (if (wffeq-ab (simul-substitute-l-term-var
			   (get *edag-lift-info* 'sel-theta)
			   (ext-exp-dag-shallow eed))
			  (etanorm
			   (lambda-norm
			    (simul-substitute-l-term-var
			     theta
			     (ext-exp-open-dag-shallow eeod)))))
		(if (eq (ext-exp-dag-kind eed) (ext-exp-open-dag-kind eeod))
		    (case (ext-exp-dag-kind eed)
		      ((NEG DIS CON IMP REW TRUE FALSE)
		       (mapc #'(lambda (arc1 arc2)
				 (push (list arc2 arc1 theta) (get *edag-lift-info* 'arc-assoc))
				 (let ((x (list (ext-exp-arc-node arc1)
						(ext-exp-open-arc-node arc2)
						theta)))
				   (push x (get *edag-lift-info* 'node-assoc))
				   (push x nl2)))
			     (ext-exp-dag-arcs eed)
			     (ext-exp-open-dag-arcs eeod))
		       (lift-eed-to-eeod nl2 selvars))
		      (DEC
		       (dolist (arc1 (ext-exp-dag-arcs eed))
			 (let* ((i (ext-exp-arc-dec-index arc1))
				(arc2 (find-if #'(lambda (arc) (equal (ext-exp-open-arc-dec-index arc) i))
					       (ext-exp-open-dag-arcs eeod))))
			   (when arc2
			     (push (list arc2 arc1 theta) (get *edag-lift-info* 'arc-assoc))
			     (let ((x (list (ext-exp-arc-node arc1)
					    (ext-exp-open-arc-node arc2)
					    theta)))
			       (push x (get *edag-lift-info* 'node-assoc))
			       (push x nl2)))))
		       (lift-eed-to-eeod nl2 selvars))
		      (EQN
		       (dolist (arc1 (ext-exp-dag-arcs eed))
			 (let* ((k (ext-exp-arc-kind arc1))
				(arc2 (find-if #'(lambda (arc)
						   (eq (ext-exp-open-arc-kind arc) k))
					       (ext-exp-open-dag-arcs eeod)))
				(x (when arc2
				     (list (ext-exp-arc-node arc1)
					   (ext-exp-open-arc-node arc2)
					   theta))))
			   (when arc2
			     (push (list arc2 arc1 theta) (get *edag-lift-info* 'arc-assoc))
			     (push x (get *edag-lift-info* 'node-assoc))
			     (push x nl2))))
		       (lift-eed-to-eeod nl2 selvars))
		      (LEAF
		       (lift-eed-to-eeod nl2 selvars))
		      (SEL
		       (let* ((arc1 (car (ext-exp-dag-arcs eed)))
			      (arc2 (car (ext-exp-open-dag-arcs eeod)))
			      (sv (ext-exp-arc-sel-var arc1))
			      (x (list (ext-exp-arc-node arc1)
				       (ext-exp-open-arc-node arc2)
				       theta)))
			 (push (cons sv (ext-exp-open-arc-sel-var arc2))
			       (get *edag-lift-info* 'sel-theta))
			 (push (list arc2 arc1 theta) (get *edag-lift-info* 'arc-assoc))
			 (push x (get *edag-lift-info* 'node-assoc))
			 (push x nl2)
			 (lift-eed-to-eeod nl2 (remove sv selvars)))))
		  (progn
;		    (msgf "Kinds of " eed " and " eeod " do not match" t)
		    (lift-eed-to-eeod nl2 selvars)))
	      (progn
;		(msgf "Shallows for " eed " and " eeod " do not match" t
;		      ((ext-exp-dag-shallow eed) . gwff) t
;		      ((ext-exp-open-dag-shallow eeod) . gwff) t)
		(lift-eed-to-eeod nl2 selvars)))))
      (let ((found nil)
	    (nl2 nl))
	(do ((nl0 nl (cdr nl0)))
	    ((or (null nl0) found)
	     (when found
	       (lift-eed-to-eeod nl2 selvars)))
	  (let ((eed (caar nl0))
		(eeod (cadar nl0)))
	    (when (eq (ext-exp-dag-kind eed) (ext-exp-open-dag-kind eeod))
	      (case (ext-exp-dag-kind eed)
		(EXP
		 (let* ((arcs (ext-exp-dag-arcs eed))
			(arc1 (find-if-not #'(lambda (arc)
					       (intersection (free-vars-of (ext-exp-arc-exp-term arc))
							     selvars))
					   arcs))
			(theta (caddr (car nl0))))
		   (when arc1
		     (let ((trm (ext-exp-arc-exp-term arc1)))
		       (do ((arcs2 (ext-exp-open-dag-arcs eeod) (cdr arcs2)))
			   ((or (null arcs2) found))
			 (let* ((arc2 (car arcs2))
				(new-theta (lift-eed-to-eeod-exp (simul-substitute-l-term-var
								  (get *edag-lift-info* 'sel-theta)
								  trm)
								 (ext-exp-open-arc-exp-term arc2)
								 theta)))
			   (unless (eq new-theta 'FAIL)
			     (let ((eed2 (copy-eed eed)))
			       (setf (ext-exp-dag-arcs eed2) (remove arc1 arcs))
			       (push (list arc1 arc2 new-theta) (get *edag-lift-info* 'arc-assoc))
			       (let ((x1 (list eed2 eeod theta))
				     (x (list (ext-exp-arc-node arc1)
					      (ext-exp-open-arc-node arc2)
					      new-theta)))
				 (push x (get *edag-lift-info* 'node-assoc))
				 (setq nl2 (cons x (cons x1 (remove (car nl0) nl2))))
				 (setq found t))))))))))
		(ATOM
		 (let* ((mate-nodes1 (mapcar #'(lambda (arc) (ext-exp-arc-node arc)) (ext-exp-dag-arcs eed)))
			(mated-to (find-if #'(lambda (x)
					       (and (eq (ext-exp-dag-kind (car x)) 'ATOM)
						    (intersection
						     mate-nodes1
						     (mapcar #'(lambda (arc) (ext-exp-arc-node arc))
							     (ext-exp-dag-arcs (car x))))))
					   (cdr nl0)))
			(mate-arc2 (when mated-to
				     (find-if #'(lambda (arc)
						  (member (ext-exp-arc-node arc) mate-nodes1))
					      (ext-exp-dag-arcs (car mated-to)))))
			(mate-node (when mate-arc2
				     (ext-exp-arc-node mate-arc2)))
			(mate-arc1 (when mate-node
				     (find-if #'(lambda (arc)
						  (eq (ext-exp-arc-node arc) mate-node))
					      (ext-exp-dag-arcs eed)))))
		   (when (and mate-node
			      (eq (ext-exp-open-dag-kind eeod) 'ATOM)
			      (eq (ext-exp-open-dag-kind (cadr mated-to)) 'ATOM))
		     (let* ((eed2 (car mated-to))
			    (eeod2 (cadr mated-to))
			    (eeod-mate
			     (car (intersection
				   (mapcar #'(lambda (arc)
					       (ext-exp-open-arc-node arc))
					   (ext-exp-open-dag-arcs eeod))
				   (mapcar #'(lambda (arc)
					       (ext-exp-open-arc-node arc))
					   (ext-exp-open-dag-arcs eeod2))))))
		       (when eeod-mate
			 (let* ((eeod-arc1
				 (find-if #'(lambda (arc) (eq (ext-exp-open-arc-node arc) eeod-mate))
					  (ext-exp-open-dag-arcs eeod)))
				(eeod-arc2
				 (find-if #'(lambda (arc) (eq (ext-exp-open-arc-node arc) eeod-mate))
					  (ext-exp-open-dag-arcs eeod2)))
				(theta1 (caddr (car nl0)))
				(theta2 (caddr mated-to))
				(theta3 theta1))
			   (dolist (p theta2)
			     (unless (eq theta3 'fail)
			       (let* ((x (car p))
				  (trm (cdr p))
				  (p3 (assoc x theta3)))
			     (if p3
				 (unless (wffeq-ab trm (cdr p3))
				   (setq theta3 'fail))
			       (push p theta3)))))
			   (unless (eq theta3 'fail)
			     (let ((eed3 (copy-eed eed))
				   (eeod3 (copy-eeod eeod))
				   (eed4 (copy-eed eed2))
				   (eeod4 (copy-eeod eeod2)))
			       (setf (ext-exp-dag-arcs eed3)
				     (remove mate-arc1 (ext-exp-dag-arcs eed3)))
			       (setf (ext-exp-open-dag-arcs eeod3)
				     (remove eeod-arc1 (ext-exp-open-dag-arcs eeod3)))
			       (setf (ext-exp-dag-arcs eed4)
				     (remove mate-arc2 (ext-exp-dag-arcs eed4)))
			       (setf (ext-exp-open-dag-arcs eeod4)
				     (remove eeod-arc2 (ext-exp-open-dag-arcs eeod4)))
			       (let ((x1 (list eed3 eeod3 theta1))
				     (x2 (list eed4 eeod4 theta2))
				     (x3 (list mate-node eeod-mate theta3)))
				 (push (list mate-arc1 eeod-arc1 theta3) (get *edag-lift-info* 'arc-assoc))
				 (push (list mate-arc2 eeod-arc2 theta3) (get *edag-lift-info* 'arc-assoc))
				 (push x3 (get *edag-lift-info* 'node-assoc))
				 (setq nl2 (cons x1 (cons x2 (cons x3 (set-difference nl2 (list (car nl0) mated-to))))))
				 (setq found t))))))))))
		(EQN
		 (when (ext-exp-dag-positive eed)
		   (let* ((eunif-nodes1 (mapcar #'(lambda (arc) (ext-exp-arc-node arc)) (ext-exp-dag-arcs eed)))
			  (eunif-to (find-if #'(lambda (x)
						 (and (eq (ext-exp-dag-kind (car x)) 'EQNGOAL)
						      (intersection
						       eunif-nodes1
						       (mapcar #'(lambda (arc) (ext-exp-arc-node arc))
							       (ext-exp-dag-arcs (car x))))))
					     (cdr nl0)))
			  (eunif-arc2 (when eunif-to
					(find-if #'(lambda (arc)
						     (member (ext-exp-arc-node arc) eunif-nodes1))
						 (ext-exp-dag-arcs (car eunif-to)))))
			  (eunif-node (when eunif-arc2
					(ext-exp-arc-node eunif-arc2)))
			  (eunif-k (when eunif-arc2 (ext-exp-arc-kind eunif-arc2)))
			  (eunif-arc1 (when eunif-node
					(find-if #'(lambda (arc)
						     (eq (ext-exp-arc-node arc) eunif-node))
						 (ext-exp-dag-arcs eed)))))
		     (when (and eunif-node
				(eq (ext-exp-open-dag-kind (cadr eunif-to)) 'EQNGOAL))
		       (let* ((eed2 (car eunif-to))
			      (eeod2 (cadr eunif-to))
			      (eeod-eunif
			       (car
				(intersection
				 (ext-exp-open-dag-kids eeod eunif-k)
				 (ext-exp-open-dag-kids eeod2 eunif-k)))))
			 (when eeod-eunif
			   (let* ((eeod-arc1
				   (find-if #'(lambda (arc) (eq (ext-exp-open-arc-node arc) eeod-eunif))
					    (ext-exp-open-dag-arcs eeod)))
				  (eeod-arc2
				   (find-if #'(lambda (arc) (eq (ext-exp-open-arc-node arc) eeod-eunif))
					    (ext-exp-open-dag-arcs eeod2)))
				  (theta1 (caddr (car nl0)))
				  (theta2 (caddr eunif-to))
				  (theta3 theta1))
			     (dolist (p theta2)
			       (unless (eq theta3 'fail)
				 (let* ((x (car p))
					(trm (cdr p))
					(p3 (assoc x theta3)))
				   (if p3
				       (unless (wffeq-ab trm (cdr p3))
					 (setq theta3 'fail))
				     (push p theta3)))))
			     (unless (eq theta3 'fail)
			       (let ((eed3 (copy-eed eed))
				     (eeod3 (copy-eeod eeod))
				     (eed4 (copy-eed eed2))
				     (eeod4 (copy-eeod eeod2)))
				 (setf (ext-exp-dag-arcs eed3)
				       (remove eunif-arc1 (ext-exp-dag-arcs eed3)))
				 (setf (ext-exp-open-dag-arcs eeod3)
				       (remove eeod-arc1 (ext-exp-open-dag-arcs eeod3)))
				 (setf (ext-exp-dag-arcs eed4)
				       (remove eunif-arc2 (ext-exp-dag-arcs eed4)))
				 (setf (ext-exp-open-dag-arcs eeod4)
				       (remove eeod-arc2 (ext-exp-open-dag-arcs eeod4)))
				 (let ((x1 (list eed3 eeod3 theta1))
				       (x2 (list eed4 eeod4 theta2))
				       (x3 (list eunif-node eeod-eunif theta3)))
				   (push (list eunif-arc1 eeod-arc1 theta3) (get *edag-lift-info* 'arc-assoc))
				   (push (list eunif-arc2 eeod-arc2 theta3) (get *edag-lift-info* 'arc-assoc))
				   (push x3 (get *edag-lift-info* 'node-assoc))
				   (setq nl2 (cons x1 (cons x2 (cons x3 (set-difference nl2 (list (car nl0) eunif-to))))))
				   (setq found t)))))))))))
		(EQNGOAL
		 (let* ((eunif-nodes1 (mapcar #'(lambda (arc) (ext-exp-arc-node arc)) (ext-exp-dag-arcs eed)))
			(eunif-to (find-if #'(lambda (x)
					       (and (eq (ext-exp-dag-kind (car x)) 'EQN)
						    (ext-exp-dag-positive (car x))
						    (intersection
						     eunif-nodes1
						     (mapcar #'(lambda (arc) (ext-exp-arc-node arc))
							     (ext-exp-dag-arcs (car x))))))
					   (cdr nl0)))
			(eunif-arc2 (when eunif-to
				      (find-if #'(lambda (arc)
						   (member (ext-exp-arc-node arc) eunif-nodes1))
					       (ext-exp-dag-arcs (car eunif-to)))))
			(eunif-node (when eunif-arc2
				      (ext-exp-arc-node eunif-arc2)))
			(eunif-k (when eunif-arc2 (ext-exp-arc-kind eunif-arc2)))
			(eunif-arc1 (when eunif-node
				      (find-if #'(lambda (arc)
						   (eq (ext-exp-arc-node arc) eunif-node))
					       (ext-exp-dag-arcs eed)))))
		   (when (and eunif-node
			      (eq (ext-exp-open-dag-kind (cadr eunif-to)) 'EQN))
		     (let* ((eed2 (car eunif-to))
			    (eeod2 (cadr eunif-to))
			    (eeod-eunif
			     (car
			      (intersection
			       (ext-exp-open-dag-kids eeod eunif-k)
			       (ext-exp-open-dag-kids eeod2 eunif-k)))))
		       (when eeod-eunif
			 (let* ((eeod-arc1
				 (find-if #'(lambda (arc) (eq (ext-exp-open-arc-node arc) eeod-eunif))
					  (ext-exp-open-dag-arcs eeod)))
				(eeod-arc2
				 (find-if #'(lambda (arc) (eq (ext-exp-open-arc-node arc) eeod-eunif))
					  (ext-exp-open-dag-arcs eeod2)))
				(theta1 (caddr (car nl0)))
				(theta2 (caddr eunif-to))
				(theta3 theta1))
			   (dolist (p theta2)
			     (unless (eq theta3 'fail)
			       (let* ((x (car p))
				      (trm (cdr p))
				      (p3 (assoc x theta3)))
				 (if p3
				     (unless (wffeq-ab trm (cdr p3))
				       (setq theta3 'fail))
				   (push p theta3)))))
			   (unless (eq theta3 'fail)
			     (let ((eed3 (copy-eed eed))
				   (eeod3 (copy-eeod eeod))
				   (eed4 (copy-eed eed2))
				   (eeod4 (copy-eeod eeod2)))
			       (setf (ext-exp-dag-arcs eed3)
				     (remove eunif-arc1 (ext-exp-dag-arcs eed3)))
			       (setf (ext-exp-open-dag-arcs eeod3)
				     (remove eeod-arc1 (ext-exp-open-dag-arcs eeod3)))
			       (setf (ext-exp-dag-arcs eed4)
				     (remove eunif-arc2 (ext-exp-dag-arcs eed4)))
			       (setf (ext-exp-open-dag-arcs eeod4)
				     (remove eeod-arc2 (ext-exp-open-dag-arcs eeod4)))
			       (let ((x1 (list eed3 eeod3 theta1))
				     (x2 (list eed4 eeod4 theta2))
				     (x3 (list eunif-node eeod-eunif theta3)))
				 (push (list eunif-arc1 eeod-arc1 theta3) (get *edag-lift-info* 'arc-assoc))
				 (push (list eunif-arc2 eeod-arc2 theta3) (get *edag-lift-info* 'arc-assoc))
				 (push x3 (get *edag-lift-info* 'node-assoc))
				 (setq nl2 (cons x1 (cons x2 (cons x3 (set-difference nl2 (list (car nl0) eunif-to))))))
				 (setq found t))))))))))
		(t nil)))))))))

(defun lift-eed-to-eeod-exp (cwff pattern theta)
  (let ((tp1 (unabbreviated-type cwff))
	(tp2 (unabbreviated-type pattern)))
    (if (equal tp1 tp2)
	(lift-eed-to-eeod-exp-1 tp1 (lambda-norm cwff) (lambda-norm pattern) theta nil)
      'fail)))

(defun lift-eed-to-eeod-exp-1 (tp cwff pattern theta boundvars)
  (if (consp tp)
      (let ((x (fresh-var-1 (cdr tp))))
	(lift-eed-to-eeod-exp-1 (car tp) (lambda-norm (cons cwff x)) (lambda-norm (cons pattern x))
				theta (cons x boundvars)))
    (lift-eed-to-eeod-exp-2 tp cwff pattern theta boundvars)))

(defun lift-eed-to-eeod-exp-2 (tp cwff pattern theta boundvars)
  (if (a-bd-wff-p cwff)
      (if (a-bd-wff-p pattern)
	  (let* ((y (bindvar cwff))
		 (z (bindvar pattern))
		 (qtp (unabbreviated-type y))
		 (qtp2 (unabbreviated-type z)))
	    (if (equal qtp qtp2)
		(let ((x (fresh-var-1 qtp)))
		  (lift-eed-to-eeod-exp-2
		   tp
		   (substitute-l-term-var x y (cdr cwff))
		   (substitute-l-term-var x (bindvar pattern) (cdr pattern))
		   theta
		   (cons x boundvars)))
	      'fail))
	(if (or (boundwff-p pattern) (not-p pattern))
	    'fail
	  (let ((h2 (head pattern)))
	    (if (ext-exp-var-p h2)
		(lift-eed-to-eeod-exp-4 tp cwff pattern h2 (args pattern) theta boundvars)
	      'fail))))
    (if (e-bd-wff-p cwff)
	(if (e-bd-wff-p pattern)
	  (let* ((y (bindvar cwff))
		 (z (bindvar pattern))
		 (qtp (unabbreviated-type y))
		 (qtp2 (unabbreviated-type z)))
	    (if (equal qtp qtp2)
		(let ((x (fresh-var-1 qtp)))
		  (lift-eed-to-eeod-exp-2
		   tp
		   (substitute-l-term-var x y (cdr cwff))
		   (substitute-l-term-var x (bindvar pattern) (cdr pattern))
		   theta
		   (cons x boundvars)))
	      'fail))
	  (if (or (boundwff-p pattern) (not-p pattern))
	      'fail
	    (let ((h2 (head pattern)))
	      (if (ext-exp-var-p h2)
		  (lift-eed-to-eeod-exp-4 tp cwff pattern h2 (args pattern) theta boundvars)
		'fail))))
      (if (not-p cwff)
	  (if (not-p pattern)
	      (lift-eed-to-eeod-exp-2 tp (cdr cwff) (cdr pattern) theta boundvars)
	    (if (boundwff-p pattern)
		'fail
	      (let ((h2 (head pattern)))
		(if (ext-exp-var-p h2)
		    (lift-eed-to-eeod-exp-4 tp cwff pattern h2 (args pattern) theta boundvars)
		  'fail))))
	(if (or (boundwff-p pattern) (not-p pattern))
	    'fail
	  (lift-eed-to-eeod-exp-3 tp cwff pattern (head cwff) (args cwff) (head pattern) (args pattern) theta boundvars))))))
  
(defun lift-eed-to-eeod-exp-3 (tp cwff pattern h1 args1 h2 args2 theta boundvars)
  (if (ext-exp-var-p h2)
      (let ((p (assoc h2 theta)))
	(if p
	    (lift-eed-to-eeod-exp-1 tp cwff (lambda-norm (substitute-l-term-var (cdr p) (car p) pattern))
				    theta boundvars)
	  (if (and (member h1 (get h2 'already-duped-for))
		   (member h1 (get h2 'banned-sel-vars)))
	      'fail
	    (lift-eed-to-eeod-exp-4 tp cwff pattern h2 args2 theta boundvars))))
    (if (eq h1 h2)
	(let ((new-theta theta))
	  (do ((args3 args1 (cdr args3))
	       (args4 args2 (cdr args4)))
	      ((or (null args3) (null args4) (eq new-theta 'fail))
	       new-theta)
	    (setq new-theta
		  (lift-eed-to-eeod-exp-1 (unabbreviated-type (car args3))
					  (car args3) (car args4)
					  new-theta boundvars))))
      'fail)))

(defun lift-eed-to-eeod-exp-4 (tp cwff pattern h2 args2 theta boundvars)
  (let ((p (assoc h2 theta)))
    (if p
	(lift-eed-to-eeod-exp-1 tp cwff (lambda-norm (substitute-l-term-var (cdr p) (car p) pattern))
				theta boundvars)
      (if (eeod-pattern-p-4 pattern boundvars)
	  (if (find-if #'(lambda (v)
			   (and (not (member v args2)) (free-in v cwff)))
		       boundvars)
	      'fail
	    (let ((wff cwff))
	      (dolist (v (reverse args2))
		(setq wff (acons v 'lambda wff)))
	      (acons h2 wff theta)))
	'fail))))

(defun eeod-pattern-pair-p (lft rght)
  (and (eeod-pattern-p lft)
       (eeod-pattern-p rght)))

(defun eeod-pattern-p (trm)
  (eeod-pattern-p-1 (type trm) trm nil))

(defun eeod-pattern-p-1 (tp trm boundvars)
  (if (consp tp)
      (let ((x (fresh-var-1 (cdr tp))))
	(eeod-pattern-p-1 (car tp) (cons trm x)
			       (cons x boundvars)))
    (if (eq tp 'O)
	nil
      (eeod-pattern-p-2 (lambda-norm trm) boundvars))))

(defun eeod-pattern-p-2 (trm boundvars)
  (let ((h (head trm)))
    (if (ext-exp-var-p h)
	(eeod-pattern-p-4 trm (append boundvars (get h 'banned-sel-vars) (get h 'banned-imitations)))
      (eeod-pattern-p-3 trm boundvars))))
  
(defun eeod-pattern-p-3 (trm boundvars)
  (if (consp trm)
      (and (eeod-pattern-p-1 (type (cdr trm)) trm boundvars)
	   (eeod-pattern-p-3 (car trm) boundvars))
    t))

(defun eeod-pattern-p-4 (trm boundvars)
  (if (consp trm)
      (let ((y (etanorm (cdr trm))))
	(if (member y boundvars)
	    (eeod-pattern-p-4 (car trm) (remove y boundvars))
	  nil))
    t))

(defun classify-lambda-term (trm &optional bindvars)
  (if (lambda-bd-p trm)
      (classify-lambda-term (cdr trm) (cons (bindvar trm) bindvars))
    (cond ((a-bd-wff-p trm)
	   (list 'FORALL (unabbreviated-type (bindvar trm))))
	  ((e-bd-wff-p trm)
	   (list 'EXISTS (unabbreviated-type (bindvar trm))))
	  ((equals-p trm)
	   (list 'EQUALS (unabbreviated-type (cdr trm))))
	  ((or (and-p trm) (or-p trm) (implies-p trm) (equiv-p trm))
	   (list (caar trm)))
	  ((not-p trm) 
	   (let ((h (head (cdr trm))))
	     (if (member h bindvars)
		 (list 'NOT 'PROJECTION (position h (reverse bindvars)))
	       (list 'NOT 'IMITATION h))))
	  ((member trm '(TRUTH FALSEHOOD)) (list trm))
	  (t
	   (let ((h (head trm)))
	     (if (member h bindvars)
		 (list 'PROJECTION (position h (reverse bindvars)))
	       (list 'IMITATION h)))))))

(defun ms03-abstract-formula (wff)
  (cond ((boundwff-p wff)
	 (list (cons (unabbreviated-type (bindvar wff))
		     (binder wff))
	       (ms03-abstract-formula (cdr wff))))
	((not-p wff) (list 'NOT (ms03-abstract-formula (cdr wff))))
	((or (and-p wff) (or-p wff) (implies-p wff) (equiv-p wff) (equals-p wff))
	 (list (caar wff)
	       (ms03-abstract-formula (cdar wff))
	       (ms03-abstract-formula (cdr wff))))
	(t nil)))
