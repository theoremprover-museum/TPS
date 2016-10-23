(defmode MODE-THM580
 (FLAG-SETTINGS (USE-SYMSIMP T) (USE-RULEP T) (USE-FAST-PROP-SEARCH T) (USE-DIY NIL) (UNIFY-VERBOSE SILENT) (UNIF-TRIGGER NIL) (UNIF-COUNTER-OUTPUT 0) (UNIF-COUNTER 0) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (TRUTHVALUES-HACK NIL) (TOTAL-NUM-OF-DUPS NIL) (TIMING-NAMED T) (SUBSUMPTION-NODES LP-NODES) (SUBSUMPTION-DEPTH INFINITY) (SUBSUMPTION-CHECK NIL) (STOP-AT-TSN T) (SKOLEM-DEFAULT NIL) (SHOW-TIME T) (SEARCH-TIME-LIMIT 5) (SEARCH-COMPLETE-PATHS NIL) (RULEP-WFFEQ WFFEQ-AB) (RIGID-PATH-CK NIL) (REWRITE-EQUIVS 1) (REWRITE-EQUALITIES NONE) (REWRITE-DEFNS (EAGER)) (REMOVE-LEIBNIZ T) (REDUCE-DOUBLE-NEG T) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS SHORT-SITE-NAME MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-DUP-PATHS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ MAX-PRIM-DEPTH MIN-PRIM-DEPTH PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (QUERY-USER NIL) (PRUNING NIL) (PROP-STRATEGY ALLOW-DUPLICATES) (PRINTMATEOPS ALWAYS-TRUE) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEFLAG NIL) (PRINTMATEFILE "mate.mss") (PRINT-MATING-COUNTER 300000) (PRIMSUB-VAR-SELECT T) (PRIMSUB-METHOD PR97) (PRIM-QUANTIFIER T) (PRIM-PREFIX PRIM) (PRIM-BDTYPES-AUTO IGNORE) (PRIM-BDTYPES ("B")) (PR97C-PRENEX T) (PR97C-MAX-ABBREVS 1) (PR00-MAX-SUBSTS-VAR 4) (ORDER-COMPONENTS PREFER-RIGID2) (OCCURS-CHECK T) (NUM-OF-DUPS 0) (NUM-FRPAIRS 5) (NEW-MATING-AFTER-DUP NIL) (NEG-PRIM-SUB NIL) (NATREE-DEBUG NIL) (MS98-VERBOSE NIL) (MS98-VARIABLE-ORDER 1) (MS98-VALID-PAIR 1) (MS98-UNIF-HACK2 T) (MS98-UNIF-HACK T) (MS98-TRACE NIL) (MS98-REWRITES NIL) (MS98-REWRITE-UNIF NIL) (MS98-REWRITE-SIZE NIL) (MS98-REWRITE-PRUNE T) (MS98-REWRITE-MODEL NIL) (MS98-REWRITE-DEPTH 2) (MS98-REW-PRIMSUBS NIL) (MS98-PRIMSUB-COUNT 3) (MS98-NUM-OF-DUPS NIL) (MS98-MINIMALITY-CHECK NIL) (MS98-MERGE-DAGS 0) (MS98-MEASURE 0) (MS98-MAX-PRIMS 1) (MS98-MAX-COMPONENTS NIL) (MS98-LOW-MEMORY NIL) (MS98-INIT 3) (MS98-FRAGMENT-ORDER 1) (MS98-FORCE-H-O NIL) (MS98-FIRST-FRAGMENT NIL) (MS98-DUP-PRIMSUBS NIL) (MS98-DUP-BELOW-PRIMSUBS T) (MS98-BASE-PRIM NIL) (MS91-INTERLEAVE 6) (MS90-3-QUICK T) (MS90-3-DUP-STRATEGY 1) (MS-SPLIT T) (MS-INIT-PATH NIL) (MS-DIR QUASI-TPS1) (MONITORFLAG NIL) (MIN-QUICK-DEPTH NIL) (MIN-QUANTIFIER-SCOPE NIL) (MIN-QUANT-ETREE T) (MIN-PRIM-LITS 1) (MIN-PRIM-DEPTH 1) (MERGE-MINIMIZE-MATING T) (MAXIMIZE-FIRST NIL) (MAX-UTREE-DEPTH NIL) (MAX-SUBSTS-VAR 4) (MAX-SUBSTS-QUICK 3) (MAX-SUBSTS-PROJ-TOTAL NIL) (MAX-SUBSTS-PROJ NIL) (MAX-SEARCH-LIMIT 500) (MAX-SEARCH-DEPTH NIL) (MAX-PRIM-LITS 2) (MAX-PRIM-DEPTH 1) (MAX-MATES 1) (MAX-DUP-PATHS INFINITY) (MATING-VERBOSE SILENT) (MATE-FFPAIR NIL) (LEIBNIZ-SUB-CHECK NIL) (LAST-MODE-NAME "Mode : MODE-THM579-PR00, but with DISSOLVE set to ((REW507 . EXP327)), and DISSOLVE set to ((EXP355 . SEL194)), and ALLOW-NONLEAF-CONNS set to (EXP355 SEL194), and DISSOLVE set to NIL, and ALLOW-NONLEAF-CONNS set to NIL, and PR00-MAX-SUBSTS-VAR set to 6, and PR00-MAX-SUBSTS-VAR set to 4, and REWRITE-EQUALITIES set to NONE, and OPTIONS-VERBOSE set to T, and PR00-WHICH-KINDS set to NIL, and RIGHTMARGIN set to 140, and MAX-SUBSTS-VAR set to 5, and MAX-SEARCH-LIMIT set to 500, and MAX-SUBSTS-VAR set to 4, and PRIMSUB-METHOD set to PR97") (INTERRUPT-ENABLE T) (INITIAL-BKTRACK-LIMIT INFINITY) (IMITATION-FIRST T) (HPATH-THRESHOLD 1) (FIRST-ORDER-MODE-MS NIL) (FF-DELAY NIL) (EXCLUDING-GC-TIME T) (ETA-RULE T) (DUPLICATION-STRATEGY-PFD DUP-INNER) (DUPLICATION-STRATEGY DUP-OUTER) (DUP-ALLOWED T) (DNEG-IMITATION CONST-FLEX) (DISSOLVE NIL) (DEFAULT-MS MS98-1) (DEFAULT-MATE MS98-1) (DEFAULT-EXPAND MS98-1) (COUNTSUBS-FIRST T) (BREAK-AT-QUANTIFIERS NIL) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (ALLOW-NONLEAF-CONNS NIL) (ADD-TRUTH IF-NEEDED))
 (mhelp "Mode for THM580.  Proven by induction, but the set-sub is only one literal,
so the PRIMSUB-METHOD is irrelevant."))