(defmode MODE-THM15A-1
 (FLAG-SETTINGS (WEIGHT-C-FN OPTION-SET-NUM-VPATHS) (WEIGHT-C-COEFFICIENT 0) (WEIGHT-B-FN SIMPLEST-WEIGHT-B-FN) (WEIGHT-B-COEFFICIENT 1) (WEIGHT-A-FN EXPANSION-LEVEL-WEIGHT-A) (WEIGHT-A-COEFFICIENT 0) (USE-SYMSIMP T) (USE-RULEP T) (USE-FAST-PROP-SEARCH T) (USE-DIY NIL) (UNIFY-VERBOSE SILENT) (UNIF-TRIGGER NIL) (UNIF-COUNTER-OUTPUT 0) (UNIF-COUNTER 0) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (TRUTHVALUES-HACK NIL) (TOTAL-NUM-OF-DUPS NIL) (TIMING-NAMED NIL) (SUBSUMPTION-NODES ALL-NODES) (SUBSUMPTION-DEPTH 5) (SUBSUMPTION-CHECK NIL) (STOP-AT-TSN T) (SKOLEM-DEFAULT SK1) (SHOW-TIME T) (SEARCH-TIME-LIMIT 10) (SEARCH-COMPLETE-PATHS NIL) (RULEP-WFFEQ WFFEQ-AB) (RIGID-PATH-CK T) (REWRITE-EQUIVS 1) (REWRITE-EQUALITIES ALL) (REWRITE-DEFNS (EAGER)) (REMOVE-LEIBNIZ T) (REDUCE-DOUBLE-NEG T) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS MACHINE-INSTANCE MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ MAX-PRIM-DEPTH MIN-PRIM-DEPTH PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (RECONSIDER-FN INF-WEIGHT) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (QUERY-USER NIL) (PRUNING NIL) (PROP-STRATEGY ALLOW-DUPLICATES) (PRINTMATEOPS (LAMBDA (EDOP) (DECLARE (IGNORE EDOP)) T)) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEFLAG NIL) (PRINTMATEFILE "mate.mss") (PRINT-MATING-COUNTER 300000) (PRIMSUB-VAR-SELECT T) (PRIMSUB-METHOD PR97) (PRIM-QUANTIFIER T) (PRIM-PREFIX PRIM) (PRIM-BDTYPES-AUTO IGNORE) (PRIM-BDTYPES ("OI")) (PR97C-PRENEX T) (PR97C-MAX-ABBREVS 1) (PENALTY-FOR-ORDINARY-DUP INFINITY) (PENALTY-FOR-MULTIPLE-SUBS 1000) (PENALTY-FOR-MULTIPLE-PRIMSUBS 1000) (PENALTY-FOR-EACH-PRIMSUB 1) (ORDER-COMPONENTS NIL) (OPTIONS-VERBOSE NIL) (OPTIONS-GENERATE-UPDATE IDENT-ARG) (OPTIONS-GENERATE-FN ADD-OPTIONS-ORIGINAL) (OPTIONS-GENERATE-ARG 75) (OCCURS-CHECK T) (NUM-OF-DUPS 0) (NUM-FRPAIRS 5) (NEW-OPTION-SET-LIMIT 3) (NEW-MATING-AFTER-DUP NIL) (NEG-PRIM-SUB T) (MS91-WEIGHT-LIMIT-RANGE 100) (MS91-TIME-BY-VPATHS NIL) (MS91-PREFER-SMALLER T) (MS91-INTERLEAVE 5) (MS90-3-QUICK NIL) (MS90-3-DUP-STRATEGY 1) (MS-SPLIT T) (MS-INIT-PATH NIL) (MS-DIR QUASI-TPS1) (MONITORFLAG NIL) (MIN-QUICK-DEPTH NIL) (MIN-QUANTIFIER-SCOPE NIL) (MIN-QUANT-ETREE T) (MIN-PRIM-LITS 1) (MIN-PRIM-DEPTH 1) (MERGE-MINIMIZE-MATING T) (MAX-UTREE-DEPTH 18) (MAX-SUBSTS-VAR 3) (MAX-SUBSTS-QUICK 3) (MAX-SUBSTS-PROJ-TOTAL NIL) (MAX-SUBSTS-PROJ NIL) (MAX-SEARCH-LIMIT 1) (MAX-SEARCH-DEPTH 18) (MAX-PRIM-LITS 2) (MAX-PRIM-DEPTH 2) (MAX-MATES 1) (MAX-DUP-PATHS INFINITY) (MATING-VERBOSE SILENT) (MATE-FFPAIR T) (LEIBNIZ-SUB-CHECK NIL) (LAST-MODE-NAME "Mode : MODE-THM15B-PR97-A, but with MAX-PRIM-DEPTH set to 2, and MIN-PRIM-LITS set to 1, and MS91-INTERLEAVE set to 5, and NEG-PRIM-SUB set to T, and SEARCH-TIME-LIMIT set to 10") (INTERRUPT-ENABLE T) (INITIAL-BKTRACK-LIMIT INFINITY) (IMITATION-FIRST T) (FIRST-ORDER-MODE-MS NIL) (EXCLUDING-GC-TIME T) (ETA-RULE T) (DUPLICATION-STRATEGY-PFD DUP-INNER) (DUPLICATION-STRATEGY DUP-OUTER) (DUP-ALLOWED NIL) (DNEG-IMITATION CONST-FLEX) (DEFAULT-MS MS91-6) (DEFAULT-MATE NPFD) (DEFAULT-EXPAND OSET) (COUNTSUBS-FIRST T) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (ADD-TRUTH IF-NEEDED))
 (mhelp "Proves THM15A"))
