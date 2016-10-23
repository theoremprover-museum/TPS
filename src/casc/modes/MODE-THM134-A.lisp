(defmode MODE-THM134-A
 (FLAG-SETTINGS (OCCURS-CHECK T) (PRINTMATEFILE "mate.mss") (PRINTMATEFLAG NIL) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEOPS ALWAYS-TRUE) (DEFAULT-MS MS90-3) (MATING-VERBOSE MIN) (MONITORFLAG NIL) (EXCLUDING-GC-TIME T) (MAX-MATES 2) (PRINT-MATING-COUNTER 300000) (MS90-3-DUP-STRATEGY 1) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (SHOW-TIME T) (DUP-ALLOWED T) (DUPLICATION-STRATEGY DUP-OUTER) (FIRST-ORDER-MODE-MS NIL) (INITIAL-BKTRACK-LIMIT 3) (INTERRUPT-ENABLE T) (MATE-FFPAIR NIL) (MAX-SEARCH-LIMIT 50) (MIN-QUANTIFIER-SCOPE NIL) (MS-DIR QUASI-TPS1) (MS-INIT-PATH NIL) (MS-SPLIT T) (NEW-MATING-AFTER-DUP NIL) (ORDER-COMPONENTS T) (PRIM-QUANTIFIER T) (PROP-STRATEGY ALLOW-DUPLICATES) (QUERY-USER NIL) (REMOVE-LEIBNIZ T) (REWRITE-DEFNS (LAZY1)) (REWRITE-EQUALITIES ALL) (RULEP-WFFEQ WFFEQ-AB) (SEARCH-COMPLETE-PATHS NIL) (SEARCH-TIME-LIMIT 50) (SKOLEM-DEFAULT SK1) (UNIFY-VERBOSE T) (USE-RULEP T) (USE-SYMSIMP NIL) (NUM-OF-DUPS 0) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS SHORT-SITE-NAME MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ MAX-PRIM-DEPTH MIN-PRIM-DEPTH PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (NUM-FRPAIRS 5) (RIGID-PATH-CK T) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (COUNTSUBS-FIRST T) (ETA-RULE T) (IMITATION-FIRST T) (MAX-SEARCH-DEPTH 7) (MAX-UTREE-DEPTH 7) (MIN-QUICK-DEPTH 3) (REDUCE-DOUBLE-NEG T) (STOP-AT-TSN T) (SUBSUMPTION-CHECK NIL) (UNI-SEARCH-HEURISTIC BREADTH-FIRST))
 (mhelp "Proves THM134 in 10 seconds internal-runtime. Note that USE-RULEP is T."))