(defmode THM200-MODE
 (FLAG-SETTINGS (USE-SYMSIMP T) (USE-RULEP NIL) (UNIFY-VERBOSE MED) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (TRUTHVALUES-HACK NIL) (SUBSUMPTION-CHECK NIL) (STOP-AT-TSN T) (SKOLEM-DEFAULT SK1) (SHOW-TIME T) (SEARCH-TIME-LIMIT 60) (SEARCH-COMPLETE-PATHS NIL) (RULEP-WFFEQ WFFEQ-AB) (RIGID-PATH-CK T) (REWRITE-EQUALITIES ALL) (REWRITE-DEFNS (EAGER)) (REMOVE-LEIBNIZ T) (REDUCE-DOUBLE-NEG T) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS SHORT-SITE-NAME MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (QUERY-USER NIL) (PROP-STRATEGY ALLOW-DUPLICATES) (PRINTMATEOPS ALWAYS-TRUE) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEFLAG NIL) (PRINTMATEFILE "mate.mss") (PRINT-MATING-COUNTER 300000) (PRIM-QUANTIFIER T) (ORDER-COMPONENTS NIL) (OCCURS-CHECK T) (NUM-OF-DUPS 2) (NUM-FRPAIRS 5) (NEW-MATING-AFTER-DUP NIL) (MS90-3-DUP-STRATEGY 1) (MS-SPLIT T) (MS-INIT-PATH NIL) (MS-DIR QUASI-TPS1) (MONITORFLAG NIL) (MIN-QUICK-DEPTH 3) (MIN-QUANTIFIER-SCOPE NIL) (MIN-PRIM-DEPTH 1) (MAX-UTREE-DEPTH 20) (MAX-SEARCH-LIMIT NIL) (MAX-SEARCH-DEPTH 20) (MAX-PRIM-DEPTH 1) (MAX-MATES 2) (MATING-VERBOSE MED) (MATE-FFPAIR NIL) (INTERRUPT-ENABLE T) (INITIAL-BKTRACK-LIMIT INFINITY) (IMITATION-FIRST T) (FIRST-ORDER-MODE-MS T) (EXCLUDING-GC-TIME T) (ETA-RULE T) (DUPLICATION-STRATEGY DUP-OUTER) (DUP-ALLOWED T) (DEFAULT-MS MS88) (COUNTSUBS-FIRST NIL) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS))
 (mhelp "mode for thm200"))
