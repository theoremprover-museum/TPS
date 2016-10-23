;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)

(context ms98-1)

(deffile ms98-macros
  (part-of ms98)
  (extension lisp)
  (mhelp "Defines the global variables, flags and structures for MS98-1"))

(defconstnt *arbitrary-const* 97) 
;the dag array will be of size (#vars * (*arbitrary-const*)^3)
;in ACL, this may not exceed about 16 500 000
(defvar *one*) 
(defvar *two*)
(defvar *touches*)
;;the above are used by fiddle-comp2list and comp2list-pruner in ms98-top only.
(defvar ccso)
(defvar *dag-vars-checked*)
(defvar *primsubs-for-ms98*)
(defvar *current-primsub*)
(defvar *ms98-start-time*)
(defvar *banme*)
(defvar *lookup*)
(defvar *sortedvar* t)
(defvar *find-cycle-fn-list*)
(defvar *dnode-count*)
(defvar *vafindex*)
(defvar *vaf* 0)
(defvar *valid-hash*)
(defvar *openfrag-list*)
(defvar *extra-sk-vars*)
(defvar *fo-delete*)
(defvar *temp-dups*)
(defvar *subs-involving*)
(defvar *usc-list*)
(defvar *rewrite-unif-hash*)
(defvar *rewrite-varcheck*)
(defvar *uavp-hash*)
(defvar *uavp-count*)
(defvar *sk-below-dups*)
(defvar *sk-below-primsubs*)
(defvar primehash1)
(defvar primehash2)
(defvar pairhash)
(defvar dphash)
(defvar disj-assoc)
(defvar component-count)
(defvar *global-sublist*)
(defvar *global-constlist*)
(defvar *using-unifhash*)
(defvar *full-unifhash*)
(defvar *ccs-substs*)
(defvar *ordered-vars*)
(defvar *banned-list*)
(defvar *veq-hash*)
(defvar *selection-banned*)
(defvar *length-list*)
(defvar lengths)
(defvar *allowed-subs-list*)
(defvar dup-priority)
(defvar conn-hash)
(defvar fragjform)
(defvar *positive-leaves*)
(defvar *negative-leaves*)
(defvar ccs-original)
(defvar *single-lit-fragments*)
(defvar *single-hpath-fragments*)
(defvar *complete-weights*)
(defvar *prim-lookup*)
(defvar *primsubs*)
(defvar *skolem-matters*)
(defvar *allsub-hash*)
(defvar *allb*)
(defvar *live-leaves*)
(defvar *dead-fragments*)
(defvar *successful-sub*)
(defvar *rewrite-hash*)
(defvar *ccs-rewrites*)
(defvar *daghash* (make-hash-table :test 'equal))
(defvar *depth-hash* nil)
(defvar *global-rewrite-dtree*)
(defvar *rewrite-dtree-list*)
(defvar *rewrite-model*)
(defvar *global-rewrite-model*)
(defvar *local-rewrites*)
(defvar *global-rewrites*)
(defvar *active-rewrites*)
(defvar *all-rewrites*)
(defvar *model-flag*)
(defvar *parity-preserved*)
(defvar rcount 0)
(defvar *rrule-fo-list*)
(defvar rconsts)
(defvar unif-count)
(defvar *use-chain* t)
(defvar *rrule-sub-list*)
(defvar *comps-tried*)
(defvar *dmark*)

; mkaminski 10/1/2005
(defvar *external-rewrites*)
(defvar *active-external-rewrites*)

(defstruct (dnode  ;node of a dag.
	    (:print-function (lambda (d s k)
			       (declare (ignore k))
			       (format s "DNODE: ~d " (dnode-keys d)))))
  (sons nil)  ;;will be a dotted list of the form ((unifier number list) . dnode)
              ;;unifier number is either 0 or the number of a unifier; list will be of length (length vars)
              ;;(e.g. ((1 2 0) . dnode)
  (keys nil)  ;;see below
  (merge)
  )

;;definition of dnode-keys: (A B C)
;;A is a (unique) integer name for this node, or the negation of this if get-collect-sons is T.
;;    -- was (dnode-name dnode), is now (car (dnode-keys dnode))
;;B is key1 from make-dnode1 (i.e. branchbelow)
;;    -- was (caar (dnode-keys dnode)), is now (cadr (dnode-keys dnode))
;;C is key3 from make-dnode1 (i.e. sumbelow)
;;    -- was (cdar (dnode-keys dnode)), is now (caddr (dnode-keys dnode))

(defstruct dtree
  (symbol nil)
  (rewrites nil) ;nil means "not rewritten yet", t means "there are no rewrites", o/w it's a list of wffs.
  (refers nil)
  (sons nil))

(defstruct rtree
  (rule nil :type symbol) ;the rule that got us this node from its parent
  (right nil) ;did we use the rhs of the rule (T) or the lhs (NIL)?
  (gwff nil) ;the gwff at this point
  (code 0) ;a quick check for strict wffeq
  (parent nil)
  (sons nil))

(defstruct qnode 
  (dpairs nil :type list)
  (substs nil :type list)
  (other))

(defstruct component
  (clist nil :type list)
  (key 0)
  (litkey 0)
  (lits nil)
  (blocks nil)
  (touches nil)
  (name nil)
  (measure -9999999)
  (tried nil)
  (original nil)
  (openpath nil)
  (final-subs nil)
  (ddf nil)
;  (nondet-rating nil)			; cebrown, intended to measure the amount of nondeterminism associated with choosing this component (right now I'm only thinking of single connections actually)
  ; the code to compute this is not written yet, so this is currently unused.
  (good nil)) ; good added - cebrown 2/20/00 - used for tracing Matt's procedure.

(defsubject ms98-1
  (mhelp "Pertaining to the component search MS98-1."))

(defsubject ms98-minor
  (mhelp "Less important flags for MS98-1."))

(defvar given-clist nil)

(defflag ms98-trace
  (default nil)
  (flagtype symbollist)
  (subjects ms98-minor)
  (mhelp "Given a mating in advance, this is used to trace the progress 
of MS98-1 search for a mating.  This is a list of symbols which
indicate what to trace.  The possible symbols are:

1. MATING - Search as usual, keeping a record of when good connections and
components are formed.  The value of *ms98-trace-file* is a string giving the
name of a file into which this information is stored.
2. MATING-FILTER - The search is filtered to only consider good
connections and components.  This is useful for a quick check
if the search can possibly succeed.  Typically, when MATING-FILTER
is on the list, then so is MATING.

If the list is nonempty at all, then the trace is considered 'on'.
The consequence of this is that duplications and primsubs are
skipped at the beginning of search, and that the output of the
trace will be sent to the file indicated by the global variable
*ms98-trace-file*."))

(defflag ms98-rewrite-model
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "If T, ask the user for a model of the rewrite rules
to help slim down the unification tree."))

(defflag ms98-minimality-check
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "If T, check each new component for minimality and reject 
those which are non-minimal. If NIL, don't bother."))

(defflag ms98-max-components
  (default nil)
  (flagtype null-or-posinteger)
  (subjects ms98-minor transmit)
  (mhelp "If non-NIL, the maximum number of components that can be 
considered on any iteration of the MS98 search."))

(defflag ms98-variable-order
  (default 1)
  (flagtype integer+)
  (subjects ms98-minor transmit)
  (mhelp "Determines the variable ordering for the unification
graph. Only affects higher-order searches.
Suppose N is the maximum number of unifiers for a 
given list of variables, and K is the length of the list.
For values 0--3, the variables are first grouped into lists of duplicate
copies (so each variable is listed with its duplicates, if any)
0 : Sort by N, largest first.
1 : Sort by N, smallest first.
2 : Sort by K, largest first.
3 : Sort by K, smallest first.
10--13 : Group the variables into lists of length 1, and then proceed
as 0--3.
20--23 : Group the variables into lists that occur together (i.e. two
variables go into the same list if their expansion nodes are not separated 
by any junctive node in the etree) and then proceed as for 0--3.
30--33 : Group the variables as for 0--3, and then reduce the lists 
to length 1 while keeping the variables in the same order.
40--43 : Group the variables as for 20--23, and then reduce the lists 
to length 1 while keeping the variables in the same order.
Other values X will behave like (X div 10) for variable grouping and
(X mod 10) for ordering the groups."))

(defflag ms98-low-memory
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "If T, try to keep memory use low. This will probably
make the search take longer."))

(defflag ms98-init
  (default 0)
  (flagtype integer+)
  (subjects ms98-1 transmit)
  (relevant-kids ((> ms98-init 1) '(ms98-base-prim ms98-max-prims ms98-primsub-count ms98-dup-below-primsubs ms98-dup-primsubs
				    ms98-rew-primsubs)))
  (irrelevant-kids ((< ms98-init 2) '(ms98-base-prim ms98-max-prims ms98-primsub-count ms98-dup-below-primsubs ms98-dup-primsubs
				      ms98-rew-primsubs)))
  (mhelp "Before doing ms98-1 search:
If 0, do nothing at first; after each failure, duplicate one more quantifier.
If 1, duplicate all outer quantifiers NUM-OF-DUPS times.
If 2, apply primsubs and duplicate all outer quantifiers NUM-OF-DUPS times.
If 3, cycle through primsubs one at a time, and duplicate all outer
      quantifiers NUM-OF-DUPS times. The time spent on each primsub will
      be at least MAX-SEARCH-LIMIT seconds, unless the search fails before
      then."))

(defflag ms98-rewrite-prune
  (default t)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "If T, delete any unifiers which are duplicates modulo
rewriting (this can be slow). If NIL, don't."))

(defflag ms98-verbose
  (default nil)
  (flagtype boolean)
  (subjects ms98-1 transmit)
  (mhelp "If T, print extra information during MS98-1 search."))

(defflag hpath-threshold
  (default 1)
  (flagtype null-or-posinteger)
  (subjects ms98-minor transmit)
  (mhelp "If NIL, break on major conjunctions. If n, break at
conjunctions and also on disjunctions having more than n hpaths."))

(defflag ms98-num-of-dups
  (default nil)
  (flagtype null-or-posinteger)
  (subjects ms98-1 transmit)
  (irrelevancy-preconditions (num-of-dups (= num-of-dups 0)))
  (relevancy-preconditions (num-of-dups (> num-of-dups 0)))
  (mhelp "If NIL, we can use every duplication that's present.
If some positive integer n, we reject any component using more than n 
of the duplications."))

(defflag ff-delay
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (irrelevancy-preconditions (default-ms (neq default-ms 'ms98-1)))
  (relevancy-preconditions (default-ms (eq default-ms 'ms98-1)))
  (mhelp "If T, delay unifying f-f pairs for single connections, and unify
them in context when some f-r pairs are added. If NIL, unify them as usual."))

(defflag ms98-base-prim
  (default nil)
  (flagtype boolean)
  (subjects ms98-1 transmit)
  (mhelp "If T, we allow the search to begin with a fragment which is part
of a primitive substitution. If NIL, we always choose a fragment which is outside 
the primitive substitutions (if possible)."))

(defflag ms98-unif-hack
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "If T, do not introduce new constants during unification.
(NOTE: This is a hack; we *do* need to introduce new constants, in 
general, but in most cases we needn't bother.)"))

(defflag ms98-unif-hack2
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "If T, during the generation of unifiers, prevent the 
occurrence of subformulas of type o which contain no variables
(except for TRUTH and FALSEHOOD, if they are allowed by MS98-UNIF-HACK).
If NIL, allow these to be generated."))

(defflag ms98-measure
  (default 0)
  (flagtype integer+)
  (subjects ms98-1 transmit)
  (mhelp "Determines the measure which is used on components.
If 0, count the components blocked and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 1, the standard measure using the # of components blocked and touched
If 2, as for 1 but also take account of the number of dups
If 3, just count the number of components blocked
If 4, as for 2 but also count the no of matings for the smallest 
      component touched
If 5, multiply the no of matings for the smallest touched by the 
      number of subs.
If 6, use the ratio of blocked to touched components and the ratio
      of the number of blocked components to the number of connections.
If 7, prefer matings where positive leaves are mated to negative leaves
      and vice versa.
If 8, use the ratio of blocked to touched components.
If 9, favour large components satisfying max-mates 1.
If 10, do as for 9 and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 11, do as for 6 and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 12, do as for 8 and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 13, weight in favour of components with max-mates 1 and then 
      weight heavily against the situation described by MS98-VALID-PAIR.
If 14, do as for 7 and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 15, take the average of 11 and 14."))

(defflag ms98-valid-pair
  (default 1)
  (flagtype integer+)
  (subjects ms98-minor transmit)
  (mhelp "Given two disjuncts X OR Y and A OR B, this flag 
determines when we are allowed to make a component containing 
connections X-A and Y-B (assuming they're unifiable connections).
The higher the number, the more stringent (and more time-consuming)
the test; any correct mating is guaranteed to pass any of these tests:
1: MAX-MATES is not 1.
2: As for 1, plus we require an extra mate for each of X,Y,A and B.
3: As for 2, plus we require that all of these new mates be 
   pairwise compatible with each other.
4: As for 3, plus we require that all of these new mates be 
   simultaneously compatible with each other.

3 and 4 are only applicable to higher-order searches.

There is an extra value, 0, which rejects any such connections
even if max-mates is not 1. This results in an incomplete search,
but is often acceptable."))

(defflag ms98-rewrites
  (default nil)
  (flagtype boolean)
  (subjects ms98-1 transmit)
  (mhelp "When T, make all of the global equalities into rewrites."))

(defflag ms98-rewrite-depth
  (default 2)
  (flagtype posinteger)
  (subjects ms98-1 transmit)
  (mhelp "When attempting to rewrite one term into another,
the maximum number of steps of rewriting that are allowed."))

(defflag ms98-rewrite-size
  (default nil)
  (flagtype null-or-posinteger)
  (subjects ms98-1 transmit)
  (mhelp "The maximum size of a (lambda-normalized) term that can be 
produced by rewriting, measured as the number of nodes in the parse
tree of that term. NIL means that there is no maximum."))

(defflag ms98-rewrite-unif
  (default nil)
  (flagtype null-or-posinteger)
  (subjects ms98-1 transmit)
  (mhelp "When a rewrite rule can introduce a new variable, this
flag governs the size of the allowed substitutions for that
variable. Essentially, this is a special case of MAX-SUBSTS-VAR."))

; mkaminski 10/1/2005
(defflag ms98-external-rewrites
  (flagtype boolean)
  (default nil)
  (subjects mating-search ms98-1)
  (mhelp "When set to T, MS98-1 uses the currently active rewrite rules as
global rewrites in addition to those it extracts from the formula.
See Matt Bishop's thesis for details on rewriting in MS98-1.
If MS98-REWRITES is set to NIL, this flag is irrelevant."))

; mkaminski 10/1/2005
(defflag ms98-pollute-global-rewrites
  (flagtype boolean)
  (default nil)
  (subjects mating-search ms98-1)
  (mhelp "When set to T, rewrites generated by MS98-1 are not removed from
the list of global rewrite rules after the search is complete.
See Matt Bishop's thesis for details on rewriting in MS98-1.
If MS98-REWRITES is set to NIL, this flag is irrelevant."))

(defflag ms98-dup-primsubs
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "When T, MS98-DUP duplicates variables which have primsubs;
when NIL, it doesn't. (Note that duplicating the variable will
not duplicate the primsub; it will produce another copy of the 
unsubstituted-for tree below that expansion node.)"))

(defflag ms98-dup-below-primsubs
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (relevancy-preconditions
   (primsub-method (and (not (eq primsub-method 'pr00)) 
			(or (equal ms98-init 2) (equal ms98-init 3))))
   (ms98-init (and (not (eq primsub-method 'pr00)) 
		   (or (equal ms98-init 2) (equal ms98-init 3)))))
  (irrelevancy-preconditions
   (primsub-method (eq primsub-method 'pr00))
   (ms98-init (or (equal ms98-init 0) (equal ms98-init 1))))
  (mhelp "When T, duplicate the quantifiers which occur below 
a primitive substitution NUM-OF-DUPS times. When NIL, don't."))

(defflag ms98-rew-primsubs
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (relevancy-preconditions
   (primsub-method (and (neq primsub-method 'pr00)
			(or (equal ms98-init 2) (equal ms98-init 3))))
   (ms98-init (and (neq primsub-method 'pr00)
		   (or (equal ms98-init 2) (equal ms98-init 3)))))
  (irrelevancy-preconditions
   (primsub-method (eq primsub-method 'pr00))
   (ms98-init (or (equal ms98-init 0) (equal ms98-init 1))))
  (mhelp "When T, MS98-DUP does primsubs for Leibniz variables which 
have become rewrites; when NIL, it doesn't."))

(defflag ms98-max-prims
  (default 1)
  (flagtype null-or-posinteger)
  (subjects ms98-1 transmit)
  (mhelp "The maximum number of primsubs allowed in any component."))

(defflag ms98-primsub-count
  (default 3)
  (flagtype null-or-posinteger)
  (subjects ms98-1 transmit)
  (mhelp "The maximum number of primsubs to be applied each
set variable in the expansion tree."))

(defflag break-at-quantifiers
  (default nil)
  (flagtype boolean)
  (subjects ms98-1 transmit)
  (mhelp "Applies only to quantifiers which cannot be duplicated 
later in the search. If T, then fragments will be broken so as 
not to contain any quantifiers; if NIL, fragments may contain
quantifiers of the sort specified."))

(defflag maximize-first
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "For each component which is being extended, do not create 
any new components which exceed MAX-MATES 1 until there are no other
ways to extend the component. This only works for higher-order
problems, and will be ignored in the first-order case."))

(defflag ms98-fragment-order
  (default 1)
  (flagtype integer+)
  (subjects ms98-1 transmit)
  (mhelp "The order in which the fragments are considered. This
principally affects which fragment will become the starting 
point of the search, and which of the touched but not blocked
fragments will be blocked next. See also MS98-FIRST-FRAGMENT.
0 : consider the number of ways to block the given fragment.
1 : consider the number of ways that the results for 0 might be
    extended (i.e. look ahead two steps in the search process)
2 : as for 1, but then weight in favour of ground fragments
    (i.e. those containing no variables)."))

(defflag ms98-first-fragment
  (default nil)
  (flagtype symbol-or-integer)
  (subjects ms98-1 transmit)
  (mhelp "If non-NIL, this will move a single fragment to the
beginning of the literal ordering, as follows:
T : set of support strategy, more or less. The starting 
    fragment will be the last non-duplicate fragment 
    enumerated. This will be the rightmost part of the wff
    to be proven.
n : (for integer n) the starting fragment will be whichever
    fragment contains LEAFn. If this leaf is part of a duplicate
    fragment, or does not exist at all, then this will behave
    like T.

NB: This flag overrides MS98-BASE-PRIM; the chosen fragment may
always be part of a primitve substitution. 
See also MS98-FRAGMENT-ORDER."))

(defflag ms98-merge-dags
  (default 0)
  (flagtype integer+)
  (subjects ms98-minor transmit)
  (mhelp "For higher-order searches only. Affects the way in 
which the unification graphs of elementary components are 
computed.
0 : Check that the graphs of the connections are pairwise
    compatible. Only compute the full graph of a component
    when necessary.
1 : Check that the graphs of the connections are compatible
    taken all together. (This can take a while for large
    sets of connections.) Only compute the full graph when 
    necessary.
2 : Always compute the full graph. This overrides FF-DELAY."))

(defflag ms98-force-h-o
  (default nil)
  (flagtype boolean)
  (subjects ms98-minor transmit)
  (mhelp "If T, use higher-order unification graphs even for 
first-order searches. If NIL, use the normal first-order
unification."))

(defflag ms98-use-colors
  (default t)
  (flagtype boolean)
  (subjects ms98-1 transmit)
  (mhelp ""))

; for ms98-trace
(defvar *ms98-trace-file* "ms98-trace")

					; for saving the value of first-order-mode-ms which TPS changes sometimes during mating search
					; it may be appropriate to change it for the mating search on the dissolved jform, then
					; change it back to get the full mating.  See ms98-top.lisp
(defvar *saved-first-order-mode-ms* nil)

; should probably be moved.
(defflag MEASUREMENTS
    (default nil)
  (flagtype symbol-data-list)
  (subjects library transmit)
  (mhelp "A flag set by the system to give information about
the complexity of the last problem worked on by TPS.
Should be included in the value of RECORDFLAGS
so that daterec will record the information.

Currently this records the number of vertical and horizontal paths,
number of literals, and number of acceptable connections."))

(defvar ms98-full-jform nil)
(defvar ms98-current-ftree nil)
(defvar ms98-setvars nil)
(defvar *ccs-constrained-solns* nil)
(defvar *ccs-constrained-solns-prune* nil)

