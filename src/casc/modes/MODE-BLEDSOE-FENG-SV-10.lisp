(defmode MODE-BLEDSOE-FENG-SV-10
 (FLAG-SETTINGS (USE-SYMSIMP T) (USE-RULEP T) (USE-FAST-PROP-SEARCH T) (USE-DIY NIL) (UNIFY-VERBOSE MED) (UNIF-TRIGGER NIL) (UNIF-COUNTER-OUTPUT 0) (UNIF-COUNTER 0) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (TRUTHVALUES-HACK NIL) (TOTAL-NUM-OF-DUPS NIL) (TIMING-NAMED T) (SUBSUMPTION-NODES LP-NODES) (SUBSUMPTION-DEPTH INFINITY) (SUBSUMPTION-CHECK NIL) (STOP-AT-TSN T) (SKOLEM-DEFAULT NIL) (SHOW-TIME T) (SEARCH-TIME-LIMIT 60) (SEARCH-COMPLETE-PATHS NIL) (RULEP-WFFEQ WFFEQ-AB) (RIGID-PATH-CK T) (REWRITE-EQUIVS 1) (REWRITE-EQUALITIES ALL) (REWRITE-DEFNS (LAZY1)) (REMOVE-LEIBNIZ T) (REDUCE-DOUBLE-NEG T) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS SHORT-SITE-NAME MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (QUERY-USER NIL) (PRUNING NIL) (PROP-STRATEGY ALLOW-DUPLICATES) (PRINTMATEOPS ALWAYS-TRUE) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEFLAG NIL) (PRINTMATEFILE "mate.mss") (PRINT-MATING-COUNTER 300000) (PRIMSUB-VAR-SELECT NIL) (PRIMSUB-METHOD PR00) (PRIM-QUANTIFIER T) (PRIM-PREFIX PRIM) (PRIM-BDTYPES-AUTO IGNORE) (PRIM-BDTYPES ("OB")) (PR97C-PRENEX T) (PR97C-MAX-ABBREVS 1) (ORDER-COMPONENTS T) (OCCURS-CHECK T) (NUM-OF-DUPS 0) (NUM-FRPAIRS 5) (NEW-MATING-AFTER-DUP NIL) (NEG-PRIM-SUB NIL) (NATREE-DEBUG NIL) (MS98-VERBOSE T) (MS98-VARIABLE-ORDER 1) (MS98-VALID-PAIR 1) (MS98-UNIF-HACK2 T) (MS98-UNIF-HACK T) (MS98-TRACE NIL) (MS98-REWRITES NIL) (MS98-REWRITE-UNIF NIL) (MS98-REWRITE-SIZE NIL) (MS98-REWRITE-PRUNE T) (MS98-REWRITE-MODEL NIL) (MS98-REWRITE-DEPTH 2) (MS98-REW-PRIMSUBS NIL) (MS98-PRIMSUB-COUNT 3) (MS98-NUM-OF-DUPS 1) (MS98-MINIMALITY-CHECK NIL) (MS98-MERGE-DAGS 0) (MS98-MEASURE 0) (MS98-MAX-PRIMS 1) (MS98-MAX-COMPONENTS NIL) (MS98-LOW-MEMORY NIL) (MS98-INIT 3) (MS98-FRAGMENT-ORDER 1) (MS98-FORCE-H-O NIL) (MS98-FIRST-FRAGMENT NIL) (MS98-DUP-PRIMSUBS NIL) (MS98-DUP-BELOW-PRIMSUBS NIL) (MS98-BASE-PRIM NIL) (MS91-INTERLEAVE 5) (MS90-3-QUICK NIL) (MS90-3-DUP-STRATEGY 1) (MS-SPLIT T) (MS-INIT-PATH NIL) (MS-DIR QUASI-TPS1) (MONITORFLAG NIL) (MIN-QUICK-DEPTH 3) (MIN-QUANTIFIER-SCOPE NIL) (MIN-QUANT-ETREE T) (MIN-PRIM-LITS 2) (MIN-PRIM-DEPTH 2) (MERGE-MINIMIZE-MATING T) (MAXIMIZE-FIRST NIL) (MAX-UTREE-DEPTH 20) (MAX-SUBSTS-VAR 2) (MAX-SUBSTS-QUICK NIL) (MAX-SUBSTS-PROJ-TOTAL NIL) (MAX-SUBSTS-PROJ NIL) (MAX-SEARCH-LIMIT 7200) (MAX-SEARCH-DEPTH 20) (MAX-PRIM-LITS 2) (MAX-PRIM-DEPTH 2) (MAX-MATES 1) (MAX-DUP-PATHS INFINITY) (MATING-VERBOSE SILENT) (MATE-FFPAIR NIL) (LEIBNIZ-SUB-CHECK NIL) (LAST-MODE-NAME "Mode : MODE-THM572-TEST3, but with PRIMSUB-METHOD set to PR97, and PRIMSUB-METHOD set to PR00, and PR00-WHICH-CONSTRAINTS set to (MAX), and PR00-WHICH-CONSTRAINTS set to (MAX MIN), and PR00-WHICH-CONSTRAINTS set to (MIN), and PR00-WHICH-CONSTRAINTS set to (MIN MAX), and PRIMSUB-VAR-SELECT set to NIL, and TRUTHVALUES-HACK set to T, and NUM-OF-DUPS set to 0, and NUM-OF-DUPS set to 1, and MAX-MATES set to 1, and RIGHTMARGIN set to 140, and TRUTHVALUES-HACK set to NIL, and TACTIC-VERBOSE set to MAX, and NUM-OF-DUPS set to 0") (INTERRUPT-ENABLE T) (INITIAL-BKTRACK-LIMIT INFINITY) (IMITATION-FIRST T) (HPATH-THRESHOLD 1) (FIRST-ORDER-MODE-MS NIL) (FF-DELAY NIL) (EXCLUDING-GC-TIME T) (ETA-RULE T) (DUPLICATION-STRATEGY-PFD DUP-INNER) (DUPLICATION-STRATEGY DUP-OUTER) (DUP-ALLOWED T) (DNEG-IMITATION CONST-FLEX) (DEFAULT-MS MS98-1) (DEFAULT-MATE MS98-1) (DEFAULT-EXPAND MS98-1) (COUNTSUBS-FIRST NIL) (BREAK-AT-QUANTIFIERS NIL) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (ADD-TRUTH IF-NEEDED))
 (mhelp "A mode for Bledsoe and Feng's topology example.
This uses the primsub method for solving set variable constraints."))
