(defmode MODE-THM15B-MS98-3
 (FLAG-SETTINGS (USE-SYMSIMP T) (USE-RULEP T) (USE-FAST-PROP-SEARCH T) (USE-DIY NIL) (UNIFY-VERBOSE SILENT) (UNIF-TRIGGER NIL) (UNIF-COUNTER-OUTPUT 0) (UNIF-COUNTER 0) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (TRUTHVALUES-HACK NIL) (TOTAL-NUM-OF-DUPS NIL) (TIMING-NAMED T) (SUBSUMPTION-NODES ALL-NODES) (SUBSUMPTION-DEPTH 5) (SUBSUMPTION-CHECK NIL) (STOP-AT-TSN T) (SKOLEM-DEFAULT NIL) (SHOW-TIME T) (SEARCH-TIME-LIMIT 1) (SEARCH-COMPLETE-PATHS NIL) (RULEP-WFFEQ WFFEQ-AB) (RIGID-PATH-CK T) (REWRITE-EQUIVS 1) (REWRITE-EQUALITIES ALL) (REWRITE-DEFNS (EAGER)) (REMOVE-LEIBNIZ T) (REDUCE-DOUBLE-NEG T) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS SHORT-SITE-NAME MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ MAX-PRIM-DEPTH MIN-PRIM-DEPTH PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (QUERY-USER NIL) (PRUNING NIL) (PROP-STRATEGY ALLOW-DUPLICATES) (PRINTMATEOPS (LAMBDA (EDOP) (DECLARE (IGNORE EDOP)) T)) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEFLAG NIL) (PRINTMATEFILE "mate.mss") (PRINT-MATING-COUNTER 300000) (PRIMSUB-VAR-SELECT T) (PRIMSUB-METHOD PR97) (PRIM-QUANTIFIER T) (PRIM-PREFIX PRIM) (PRIM-BDTYPES-AUTO IGNORE) (PRIM-BDTYPES ("OI")) (PR97C-PRENEX T) (PR97C-MAX-ABBREVS 1) (ORDER-COMPONENTS NIL) (OCCURS-CHECK T) (NUM-OF-DUPS 0) (NUM-FRPAIRS 5) (NEW-MATING-AFTER-DUP NIL) (NEG-PRIM-SUB NIL) (MS98-VERBOSE NIL) (MS98-VALID-PAIR 1) (MS98-UNIF-HACK NIL) (MS98-REWRITES NIL) (MS98-REWRITE-UNIF 3) (MS98-REWRITE-SIZE 6) (MS98-REWRITE-PRUNE T) (MS98-REWRITE-MODEL NIL) (MS98-REWRITE-DEPTH 1) (MS98-REW-PRIMSUBS NIL) (MS98-PRIMSUB-COUNT 3) (MS98-NUM-OF-DUPS NIL) (MS98-MEASURE 0) (MS98-MAX-PRIMS 1) (MS98-INIT 2) (MS98-FRAGMENT-ORDER 2) (MS98-DUP-PRIMSUBS NIL) (MS98-BASE-PRIM NIL) (MS91-INTERLEAVE 3) (MS90-3-QUICK NIL) (MS90-3-DUP-STRATEGY 1) (MS-SPLIT T) (MS-INIT-PATH NIL) (MS-DIR QUASI-TPS1) (MONITORFLAG NIL) (MIN-QUICK-DEPTH NIL) (MIN-QUANTIFIER-SCOPE NIL) (MIN-QUANT-ETREE T) (MIN-PRIM-LITS 2) (MIN-PRIM-DEPTH 1) (MERGE-MINIMIZE-MATING T) (MAXIMIZE-FIRST NIL) (MAX-UTREE-DEPTH 18) (MAX-SUBSTS-VAR 3) (MAX-SUBSTS-QUICK 3) (MAX-SUBSTS-PROJ-TOTAL NIL) (MAX-SUBSTS-PROJ NIL) (MAX-SEARCH-LIMIT 1) (MAX-SEARCH-DEPTH 18) (MAX-PRIM-LITS 2) (MAX-PRIM-DEPTH 1) (MAX-MATES 1) (MAX-DUP-PATHS INFINITY) (MATING-VERBOSE SILENT) (MATE-FFPAIR T) (LEIBNIZ-SUB-CHECK NIL) (LAST-MODE-NAME "Mode : MODE-THM15B-PR97-A, but with REWRITE-DEFNS set to (DUAL), and REWRITE-EQUALITIES set to DUAL, and DEFAULT-MS set to MS98-1, and MS98-VALID-PAIR set to 1, and SKOLEM-DEFAULT set to NIL, and REWRITE-DEFNS set to (EAGER), and MS98-FRAGMENT-ORDER set to 2, and MS98-REWRITES set to T, and MS98-REWRITE-DEPTH set to 1, and MS98-REWRITE-UNIF set to 3, and REWRITE-EQUALITIES set to ALL, and MS98-INIT set to 2, and MS98-REWRITE-SIZE set to 6, and MS98-REWRITE-MODEL set to T, and MS98-REWRITE-MODEL set to NIL, and MS98-REWRITES set to NIL") (INTERRUPT-ENABLE T) (INITIAL-BKTRACK-LIMIT INFINITY) (IMITATION-FIRST T) (HPATH-THRESHOLD 1) (FIRST-ORDER-MODE-MS NIL) (FF-DELAY NIL) (EXCLUDING-GC-TIME T) (ETA-RULE T) (DUPLICATION-STRATEGY-PFD DUP-INNER) (DUPLICATION-STRATEGY DUP-OUTER) (DUP-ALLOWED NIL) (DNEG-IMITATION CONST-FLEX) (DEFAULT-MS MS98-1) (DEFAULT-MATE MS98-1) (DEFAULT-EXPAND MS98-1) (COUNTSUBS-FIRST T) (BREAK-AT-QUANTIFIERS NIL) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (ADD-TRUTH IF-NEEDED))
 (mhelp "Proves thm15b in 1.5 minutes in ms98-1 without rewriting (this is MODE-THM15B-MS98-2 with MS98-REWRITES NIL)"))
