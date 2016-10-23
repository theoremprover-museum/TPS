(defmode MODE-THM589
 (FLAG-SETTINGS (USE-SYMSIMP T) (USE-RULEP T) (USE-FAST-PROP-SEARCH T) (USE-DIY NIL) (UNIFY-VERBOSE MED) (UNIF-TRIGGER NIL) (UNIF-COUNTER-OUTPUT 0) (UNIF-COUNTER 0) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (TRUTHVALUES-HACK NIL) (TOTAL-NUM-OF-DUPS NIL) (TIMING-NAMED NIL) (SUBSUMPTION-NODES LP-NODES) (SUBSUMPTION-DEPTH INFINITY) (SUBSUMPTION-CHECK NIL) (STOP-AT-TSN T) (SKOLEM-DEFAULT NIL) (SHOW-TIME T) (SEARCH-TIME-LIMIT NIL) (SEARCH-COMPLETE-PATHS NIL) (RULEP-WFFEQ WFFEQ-AB) (RIGID-PATH-CK T) (REWRITE-EQUIVS 1) (REWRITE-EQUALITIES DUAL) (REWRITE-DEFNS (EAGER)) (REMOVE-LEIBNIZ T) (REDUCE-DOUBLE-NEG T) (RECORDFLAGS NIL) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (QUERY-USER NIL) (PRUNING NIL) (PROP-STRATEGY ALLOW-DUPLICATES) (PRINTMATEOPS ALWAYS-TRUE) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEFLAG NIL) (PRINTMATEFILE "mate.mss") (PRINT-MATING-COUNTER 300000) (PRIMSUB-VAR-SELECT T) (PRIMSUB-METHOD PR97C) (PRIM-QUANTIFIER T) (PRIM-PREFIX PRIM) (PRIM-BDTYPES-AUTO REPLACE) (PRIM-BDTYPES ("I")) (PR97C-PRENEX T) (PR97C-MAX-ABBREVS 1) (PR00-REQUIRE-ARG-DEPS NIL) (PR00-NUM-ITERATIONS 1) (PR00-MAX-SUBSTS-VAR 4) (ORDER-COMPONENTS T) (OCCURS-CHECK T) (NUM-OF-DUPS 0) (NUM-FRPAIRS 5) (NEW-MATING-AFTER-DUP NIL) (NEG-PRIM-SUB NIL) (NATREE-DEBUG T) (MS98-VERBOSE NIL) (MS98-VARIABLE-ORDER 1) (MS98-VALID-PAIR 1) (MS98-UNIF-HACK2 NIL) (MS98-UNIF-HACK NIL) (MS98-TRACE NIL) (MS98-REWRITES NIL) (MS98-REWRITE-UNIF NIL) (MS98-REWRITE-SIZE 2) (MS98-REWRITE-PRUNE T) (MS98-REWRITE-MODEL NIL) (MS98-REWRITE-DEPTH 1) (MS98-REW-PRIMSUBS NIL) (MS98-PRIMSUB-COUNT 3) (MS98-NUM-OF-DUPS 1) (MS98-MINIMALITY-CHECK NIL) (MS98-MERGE-DAGS 0) (MS98-MEASURE 0) (MS98-MAX-PRIMS 1) (MS98-MAX-COMPONENTS NIL) (MS98-LOW-MEMORY NIL) (MS98-INIT 3) (MS98-FRAGMENT-ORDER 1) (MS98-FORCE-H-O NIL) (MS98-FIRST-FRAGMENT NIL) (MS98-DUP-PRIMSUBS NIL) (MS98-DUP-BELOW-PRIMSUBS NIL) (MS98-BASE-PRIM NIL) (MS91-INTERLEAVE 5) (MS90-3-QUICK NIL) (MS90-3-DUP-STRATEGY 1) (MS-SPLIT T) (MS-INIT-PATH NIL) (MS-DIR QUASI-TPS1) (MONITORFLAG NIL) (MIN-QUICK-DEPTH 3) (MIN-QUANTIFIER-SCOPE NIL) (MIN-QUANT-ETREE T) (MIN-PRIM-LITS 3) (MIN-PRIM-DEPTH 2) (MERGE-MINIMIZE-MATING T) (MAXIMIZE-FIRST NIL) (MAX-UTREE-DEPTH 5) (MAX-SUBSTS-VAR 3) (MAX-SUBSTS-QUICK NIL) (MAX-SUBSTS-PROJ-TOTAL NIL) (MAX-SUBSTS-PROJ NIL) (MAX-SEARCH-LIMIT 120) (MAX-SEARCH-DEPTH NIL) (MAX-PRIM-LITS 3) (MAX-PRIM-DEPTH 2) (MAX-MATES 1) (MAX-DUP-PATHS INFINITY) (MATING-VERBOSE MED) (MATE-FFPAIR NIL) (LEIBNIZ-SUB-CHECK NIL) (LAST-MODE-NAME "Mode : MODE-THM589-SUGGEST, but with MS98-INIT set to 1, and REWRITE-EQUALITIES set to DUAL, and MS98-REWRITES set to T, and MS98-REWRITE-SIZE set to 2, and MS98-REWRITE-DEPTH set to 1, and MS98-REWRITES set to NIL, and MAX-SUBSTS-VAR set to 2, and MAX-SUBSTS-VAR set to 1, and MS98-INIT set to 3, and MAX-PRIM-LITS set to 3, and PRIMSUB-METHOD set to PR97C, and MIN-PRIM-LITS set to 3, and MAX-SUBSTS-VAR set to 3, and MAX-SEARCH-LIMIT set to 120, and MAX-SUBSTS-VAR set to 2, and MAX-SUBSTS-VAR set to 3, and QUERY-USER set to QUERY-JFORMS, and RIGHTMARGIN set to 140, and RIGHTMARGIN set to 190, and RIGHTMARGIN set to 191, and RIGHTMARGIN set to 140, and QUERY-USER set to NIL") (INTERRUPT-ENABLE T) (INITIAL-BKTRACK-LIMIT INFINITY) (IMITATION-FIRST T) (HPATH-THRESHOLD 1) (FIRST-ORDER-MODE-MS NIL) (FF-DELAY NIL) (EXCLUDING-GC-TIME NIL) (ETA-RULE T) (DUPLICATION-STRATEGY-PFD DUP-INNER) (DUPLICATION-STRATEGY DUP-OUTER) (DUP-ALLOWED T) (DNEG-IMITATION CONST-FLEX) (DISSOLVE NIL) (DEFAULT-MS MS98-1) (DEFAULT-MATE MS98-1) (DEFAULT-EXPAND MS98-1) (COUNTSUBS-FIRST NIL) (BREAK-AT-QUANTIFIERS NIL) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (ALLOW-NONLEAF-CONNS NIL) (ADD-TRUTH IF-NEEDED))
 (mhelp "A mode to solve THM589 - requires a primsub - special case of THM588.
This example is derived from a challenge problem John Harrison sent me (Chad) 
in January, 2001."))
