(defmode MODE-BLEDSOE-FENG
 (FLAG-SETTINGS (ADD-TRUTH IF-NEEDED) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (BREAK-AT-QUANTIFIERS NIL) (COUNTSUBS-FIRST NIL) (DEFAULT-EXPAND MS98-1) (DEFAULT-MATE MS98-1) (DEFAULT-MS MS98-1) (DNEG-IMITATION CONST-FLEX) (DUP-ALLOWED T) (DUPLICATION-STRATEGY DUP-OUTER) (DUPLICATION-STRATEGY-PFD DUP-INNER) (ETA-RULE T) (EXCLUDING-GC-TIME T) (FF-DELAY NIL) (FIRST-ORDER-MODE-MS NIL) (HPATH-THRESHOLD 1) (IMITATION-FIRST T) (INITIAL-BKTRACK-LIMIT INFINITY) (INTERRUPT-ENABLE T) (LAST-MODE-NAME "Mode : MODE-THM146-MS98, but with MAX-PRIM-DEPTH set to 2, and MAX-PRIM-LITS set to 2, and MIN-PRIM-LITS set to 2, and PRIM-BDTYPES set to ((O . I)), and PRIM-BDTYPES-AUTO set to IGNORE") (LEIBNIZ-SUB-CHECK NIL) (MATE-FFPAIR NIL) (MATING-VERBOSE SILENT) (MAX-DUP-PATHS INFINITY) (MAX-MATES 1) (MAX-PRIM-DEPTH 2) (MAX-PRIM-LITS 2) (MAX-SEARCH-DEPTH NIL) (MAX-SEARCH-LIMIT 500) (MAX-SUBSTS-PROJ NIL) (MAX-SUBSTS-PROJ-TOTAL NIL) (MAX-SUBSTS-QUICK 4) (MAX-SUBSTS-VAR 4) (MAX-UTREE-DEPTH NIL) (MAXIMIZE-FIRST NIL) (MERGE-MINIMIZE-MATING T) (MIN-PRIM-DEPTH 2) (MIN-PRIM-LITS 2) (MIN-QUANT-ETREE T) (MIN-QUANTIFIER-SCOPE NIL) (MIN-QUICK-DEPTH NIL) (MONITORFLAG NIL) (MS-DIR QUASI-TPS1) (MS-INIT-PATH NIL) (MS-SPLIT T) (MS90-3-DUP-STRATEGY 1) (MS90-3-QUICK NIL) (MS91-INTERLEAVE 5) (MS98-BASE-PRIM NIL) (MS98-DUP-BELOW-PRIMSUBS NIL) (MS98-DUP-PRIMSUBS NIL) (MS98-FIRST-FRAGMENT NIL) (MS98-FORCE-H-O NIL) (MS98-FRAGMENT-ORDER 2) (MS98-INIT 3) (MS98-LOW-MEMORY NIL) (MS98-MAX-COMPONENTS NIL) (MS98-MAX-PRIMS 1) (MS98-MEASURE 15) (MS98-MERGE-DAGS 0) (MS98-MINIMALITY-CHECK NIL) (MS98-NUM-OF-DUPS 1) (MS98-PRIMSUB-COUNT 3) (MS98-REW-PRIMSUBS NIL) (MS98-REWRITE-DEPTH 2) (MS98-REWRITE-MODEL NIL) (MS98-REWRITE-PRUNE T) (MS98-REWRITE-SIZE NIL) (MS98-REWRITE-UNIF NIL) (MS98-REWRITES NIL) (MS98-UNIF-HACK T) (MS98-UNIF-HACK2 T) (MS98-VALID-PAIR 1) (MS98-VARIABLE-ORDER 0) (MS98-VERBOSE NIL) (NEG-PRIM-SUB NIL) (NEW-MATING-AFTER-DUP NIL) (NUM-FRPAIRS 5) (NUM-OF-DUPS 1) (OCCURS-CHECK T) (ORDER-COMPONENTS T) (PR97C-MAX-ABBREVS 1) (PR97C-PRENEX T) (PRIM-BDTYPES ("OI")) (PRIM-BDTYPES-AUTO IGNORE) (PRIM-PREFIX PRIM) (PRIM-QUANTIFIER T) (PRIMSUB-METHOD PR95) (PRIMSUB-VAR-SELECT T) (PRINT-MATING-COUNTER 300000) (PRINTMATEFILE "mate.mss") (PRINTMATEFLAG NIL) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEOPS ALWAYS-TRUE) (PROP-STRATEGY ALLOW-DUPLICATES) (PRUNING NIL) (QUERY-USER NIL) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS SHORT-SITE-NAME MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (REDUCE-DOUBLE-NEG T) (REMOVE-LEIBNIZ T) (REWRITE-DEFNS (EAGER)) (REWRITE-EQUALITIES ALL) (REWRITE-EQUIVS 1) (RIGID-PATH-CK T) (RULEP-WFFEQ WFFEQ-AB) (SEARCH-COMPLETE-PATHS NIL) (SEARCH-TIME-LIMIT 60) (SHOW-TIME T) (SKOLEM-DEFAULT NIL) (STOP-AT-TSN T) (SUBSUMPTION-CHECK NIL) (SUBSUMPTION-DEPTH INFINITY) (SUBSUMPTION-NODES LP-NODES) (TIMING-NAMED T) (TOTAL-NUM-OF-DUPS NIL) (TRUTHVALUES-HACK NIL) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (UNIF-COUNTER 0) (UNIF-COUNTER-OUTPUT 0) (UNIF-TRIGGER NIL) (UNIFY-VERBOSE SILENT) (USE-DIY NIL) (USE-FAST-PROP-SEARCH T) (USE-RULEP T) (USE-SYMSIMP T))
 (mhelp "MS98 mode for proving the wff proved in bledsoe-feng-sv-i2-b.prf (in proofs/complete/),
This corresponds to the formula BLEDSOE-FENG-SV-I2
There are other modes for this formula, MODE-BLEDSOE-FENG-SV-I2-{A,B,C}."))