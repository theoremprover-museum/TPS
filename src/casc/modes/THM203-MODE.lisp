(defmode THM203-MODE
 (FLAG-SETTINGS (OCCURS-CHECK T) (DEFAULT-MS MS88) (MAX-MATES 1) (DUP-ALLOWED T) (DUPLICATION-STRATEGY DUP-OUTER) (FIRST-ORDER-MODE-MS NIL) (INITIAL-BKTRACK-LIMIT INFINITY) (INTERRUPT-ENABLE T) (MATE-FFPAIR T) (MIN-QUANTIFIER-SCOPE NIL) (MS-DIR QUASI-TPS1) (MS-INIT-PATH NIL) (MS-SPLIT T) (NEW-MATING-AFTER-DUP NIL) (ORDER-COMPONENTS NIL) (PRIM-QUANTIFIER T) (PROP-STRATEGY ALLOW-DUPLICATES) (QUERY-USER NIL) (REMOVE-LEIBNIZ T) (REWRITE-DEFNS (EAGER)) (REWRITE-EQUALITIES ALL) (RULEP-WFFEQ WFFEQ-AB) (SEARCH-COMPLETE-PATHS NIL) (SKOLEM-DEFAULT SK1) (UNIFY-VERBOSE MED) (USE-RULEP T) (USE-SYMSIMP T) (MAX-PRIM-DEPTH 1) (MIN-PRIM-DEPTH 1) (PRINTMATEFILE "mate.mss") (PRINTMATEFLAG NIL) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEOPS ALWAYS-TRUE) (TRUTHVALUES-HACK NIL) (MATING-VERBOSE MED) (MONITORFLAG NIL) (EXCLUDING-GC-TIME T) (PRINT-MATING-COUNTER 300000) (MS90-3-DUP-STRATEGY 1) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (SHOW-TIME T) (MAX-SEARCH-LIMIT NIL) (SEARCH-TIME-LIMIT 60) (NUM-OF-DUPS 2) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS SHORT-SITE-NAME MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (NUM-FRPAIRS 5) (RIGID-PATH-CK T) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (COUNTSUBS-FIRST NIL) (ETA-RULE T) (IMITATION-FIRST T) (MAX-SEARCH-DEPTH 7) (MAX-UTREE-DEPTH 7) (MIN-QUICK-DEPTH 7) (REDUCE-DOUBLE-NEG T) (STOP-AT-TSN T) (SUBSUMPTION-CHECK NIL) (UNI-SEARCH-HEURISTIC BREADTH-FIRST))
 (mhelp "mode for proving trcl-minimal"))
