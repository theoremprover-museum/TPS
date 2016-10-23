(defmode MS04-LIFT-MODE
 (FLAG-SETTINGS (WHICH-CONSTRAINTS (MAX MIN)) (USE-SYMSIMP T) (USE-RULEP NIL) (USE-FAST-PROP-SEARCH T) (USE-EXT-LEMMAS NIL) (USE-DIY NIL) (UNIFY-VERBOSE MED) (UNIF-TRIGGER NIL) (UNIF-COUNTER-OUTPUT 0) (UNIF-COUNTER 0) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (TRUTHVALUES-HACK NIL) (TOTAL-NUM-OF-DUPS NIL) (TIMING-NAMED NIL) (SUBSUMPTION-NODES LP-NODES) (SUBSUMPTION-DEPTH INFINITY) (SUBSUMPTION-CHECK NIL) (STOP-AT-TSN T) (SKOLEM-DEFAULT NIL) (SHOW-TIME T) (SEARCH-TIME-LIMIT 60) (SEARCH-COMPLETE-PATHS NIL) (RULEP-WFFEQ WFFEQ-AB) (RIGID-PATH-CK T) (REWRITE-EQUIVS 1) (REWRITE-EQUALITIES ALL) (REWRITE-DEFNS (EAGER)) (REMOVE-LEIBNIZ T) (REDUCE-DOUBLE-NEG T) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS MEASUREMENTS SHORT-SITE-NAME MACHINE-TYPE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (QUERY-USER T) (PRUNING NIL) (PROP-STRATEGY ALLOW-DUPLICATES) (PRINTMATEOPS ALWAYS-TRUE) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEFLAG NIL) (PRINTMATEFILE "mate.mss") (PRINT-MATING-COUNTER 300000) (PRIMSUB-VAR-SELECT T) (PRIMSUB-METHOD PR93) (PRIM-QUANTIFIER T) (PRIM-PREFIX PRIM) (PRIM-BDTYPES-AUTO REPLACE) (PRIM-BDTYPES ("I")) (PR97C-PRENEX T) (PR97C-MAX-ABBREVS 1) (PR00-REQUIRE-ARG-DEPS NIL) (PR00-NUM-ITERATIONS 1) (PR00-MAX-SUBSTS-VAR 4) (PR00-ALLOW-SUBNODE-CONNS T) (ORDER-COMPONENTS T) (OCCURS-CHECK T) (NUM-OF-DUPS 0) (NUM-FRPAIRS 5) (NEW-MATING-AFTER-DUP NIL) (NEG-PRIM-SUB NIL) (NATREE-DEBUG NIL) (MS91-INTERLEAVE 5) (MS90-3-QUICK NIL) (MS90-3-DUP-STRATEGY 1) (MS04-WEIGHT-SOLVE-SET-CONSTRAINTS 1) (MS04-WEIGHT-PRIMSUB-NEXTTP 100) (MS04-WEIGHT-PRIMSUB-NEXT-NOT 100) (MS04-WEIGHT-PRIMSUB-FIRST-NOT 100) (MS04-WEIGHT-MULTIPLE-MATES 10) (MS04-WEIGHT-MULTIPLE-EUNIF2S 10) (MS04-WEIGHT-MULTIPLE-EUNIF1S 10) (MS04-WEIGHT-FLEXRIGID-PROJ-MATE 2) (MS04-WEIGHT-FLEX-EUNIF 2) (MS04-WEIGHT-EUNIF-DIFF-HEADS 2000) (MS04-WEIGHT-EUNIF-DECS 1000) (MS04-WEIGHT-DELAY-UNIF 0) (MS04-WEIGHT-ADD-SET-CONSTRAINT 1) (MS04-VERBOSE MAX) (MS04-USE-SET-CONSTRAINTS NIL) (MS04-USE-SEMANTICS NIL) (MS04-TRACE NIL) (MS04-SOLVE-UNIF-DEPTH 5000) (MS04-SEMANTIC-PRUNING NIL) (MS04-PRENEX-PRIMSUBS T) (MS04-MP-OPTIONS NIL) (MS04-MAX-RIGID-MATES 100) (MS04-MAX-PROJS 100) (MS04-MAX-PRIMSUB-PROJ 100) (MS04-MAX-PRIMSUB-OR 100) (MS04-MAX-PRIMSUB-NOT-PROJ 100) (MS04-MAX-PRIMSUB-NOT-EQUALS 100) (MS04-MAX-PRIMSUB-NOT 100) (MS04-MAX-PRIMSUB-FORALL 100) (MS04-MAX-PRIMSUB-EXISTS 100) (MS04-MAX-PRIMSUB-EQUALS 100) (MS04-MAX-PRIMSUB-AND 100) (MS04-MAX-IMITS 100) (MS04-MAX-FLEXRIGID-PROJ-MATES 500) (MS04-MAX-FLEXRIGID-NEG-PROJ-MATES 500) (MS04-MAX-FLEXRIGID-NEG-MATES 500) (MS04-MAX-FLEXRIGID-MATES 500) (MS04-MAX-FLEX-EUNIFS 200) (MS04-MAX-EUNIF2S 300) (MS04-MAX-EUNIF1S 300) (MS04-MAX-DUPS 300) (MS04-MAX-DEPTH INFINITY) (MS04-MAX-DELAYED-CONNS 100) (MS04-INITIAL-DEPTH 10000000) (MS04-INCR-DEPTH 100) (MS04-EAGER-UNIF-SUBST T) (MS04-DUP-WEIGHT 300) (MS04-DELAY-UNIF-CONSTRAINTS T) (MS04-DELAY-FLEXRIGID-MATES T) (MS04-CHECK-UNIF-DEPTH 3) (MS04-ALLOW-FLEXRIGID-PROJ-MATE T) (MS04-ALLOW-FLEX-EUNIFS T) (MS03-WEIGHT-RIGIDRIGIDSAME-O 15) (MS03-WEIGHT-RIGIDRIGIDDIFF-O 40) (MS03-WEIGHT-RIGIDRIGID-NOEQN 500) (MS03-WEIGHT-RIGIDRIGID-FLEXEQN 60) (MS03-WEIGHT-RIGIDRIGID-EQN 50) (MS03-WEIGHT-RIGID-MATE 1) (MS03-WEIGHT-PROJECT 1) (MS03-WEIGHT-PRIMSUB-TRUTH 50) (MS03-WEIGHT-PRIMSUB-NEXT-PROJ 500) (MS03-WEIGHT-PRIMSUB-NEXT-OR 200) (MS03-WEIGHT-PRIMSUB-NEXT-NOT-PROJ 500) (MS03-WEIGHT-PRIMSUB-NEXT-NOT-EQUALS 200) (MS03-WEIGHT-PRIMSUB-NEXT-FORALL 200) (MS03-WEIGHT-PRIMSUB-NEXT-EXISTS 200) (MS03-WEIGHT-PRIMSUB-NEXT-EQUALS 200) (MS03-WEIGHT-PRIMSUB-NEXT-AND 200) (MS03-WEIGHT-PRIMSUB-FIRST-PROJ 500) (MS03-WEIGHT-PRIMSUB-FIRST-OR 200) (MS03-WEIGHT-PRIMSUB-FIRST-NOT-PROJ 500) (MS03-WEIGHT-PRIMSUB-FIRST-NOT-EQUALS 200) (MS03-WEIGHT-PRIMSUB-FIRST-FORALL 200) (MS03-WEIGHT-PRIMSUB-FIRST-EXISTS 200) (MS03-WEIGHT-PRIMSUB-FIRST-EQUALS 200) (MS03-WEIGHT-PRIMSUB-FIRST-AND 200) (MS03-WEIGHT-PRIMSUB-FALSEHOOD 50) (MS03-WEIGHT-OCCURS-CHECK 150) (MS03-WEIGHT-IMITATE 1) (MS03-WEIGHT-FLEXRIGID-O 20) (MS03-WEIGHT-FLEXRIGID-NOEQN 500) (MS03-WEIGHT-FLEXRIGID-MATE 1) (MS03-WEIGHT-FLEXRIGID-FLEXEQN 100) (MS03-WEIGHT-FLEXRIGID-EQN 100) (MS03-WEIGHT-FLEXRIGID-BRANCH 6) (MS03-WEIGHT-FLEXFLEXSAME-O 20) (MS03-WEIGHT-FLEXFLEXSAME 5) (MS03-WEIGHT-FLEXFLEXDIFF-O 10) (MS03-WEIGHT-FLEXFLEXDIFF 3) (MS03-WEIGHT-EUNIF2 1) (MS03-WEIGHT-EUNIF1 1) (MS03-WEIGHT-BANNED-SELS 300) (MS03-USE-SET-CONSTRAINTS NIL) (MS03-QUICK-EUNIFICATION-LIMIT 50) (MS-SPLIT T) (MS-INIT-PATH NIL) (MS-DIR QUASI-TPS1) (MONITORFLAG NIL) (MIN-QUICK-DEPTH 3) (MIN-QUANTIFIER-SCOPE NIL) (MIN-QUANT-ETREE T) (MIN-PRIM-LITS 2) (MIN-PRIM-DEPTH 1) (MERGE-MINIMIZE-MATING T) (MAX-UTREE-DEPTH 20) (MAX-SUBSTS-VAR 2) (MAX-SUBSTS-QUICK NIL) (MAX-SUBSTS-PROJ-TOTAL NIL) (MAX-SUBSTS-PROJ NIL) (MAX-SEARCH-LIMIT NIL) (MAX-SEARCH-DEPTH 20) (MAX-PRIM-LITS 4) (MAX-PRIM-DEPTH 1) (MAX-NUM-CONSTRAINTS 2) (MAX-MATES 2) (MAX-DUP-PATHS INFINITY) (MAX-DOMAIN-SIZE 65536) (MAX-CONSTRAINT-SIZE 3) (MAX-BINDER-COMPUTATION 1048576) (MATING-VERBOSE MED) (MATE-UP-TO-NNF T) (MATE-FFPAIR NIL) (LEIBNIZ-SUB-CHECK NIL) (LAST-MODE-NAME ", and SOURCE-PATH set to (/home/theorem/tps/bin/ /home/theorem/tps/lisp/ /home/theorem/tps/proofs/complete/ /home/theorem/tps/proofs/complete/cleaned /home/theorem/tps/proofs/incomplete/ /home/theorem/tps/cebrown/ /home/theorem/tps/cebrown/domain/), and RIGHTMARGIN set to 130, and PSEQ-USE-LABELS set to NIL, and DEFAULT-MS set to MS04-2, and QUERY-USER set to T, and MS04-VERBOSE set to T, and USE-RULEP set to NIL, and MS03-WEIGHT-EUNIF1 set to 100, and MS03-WEIGHT-EUNIF2 set to 100, and MS03-WEIGHT-EUNIF2 set to 1, and MS03-WEIGHT-EUNIF1 set to 1, and MS04-MAX-DELAYED-CONNS set to 100, and MS04-MAX-DUPS set to 300, and MS04-MAX-EUNIF1S set to 300, and MS04-MAX-EUNIF2S set to 300, and MS04-MAX-FLEX-EUNIFS set to 200, and MS04-MAX-FLEXRIGID-MATES set to 500, and MS04-MAX-FLEXRIGID-NEG-MATES set to 500, and MS04-MAX-FLEXRIGID-NEG-PROJ-MATES set to 500, and MS04-MAX-FLEXRIGID-PROJ-MATES set to 500, and MS04-MAX-IMITS set to 100, and MS04-MAX-PRIMSUB-AND set to 100, and MS04-MAX-PRIMSUB-EQUALS set to 100, and MS04-MAX-PRIMSUB-EXISTS set to 100, and MS04-MAX-PRIMSUB-FORALL set to 100, and MS04-MAX-PRIMSUB-NOT set to 100, and MS04-MAX-PRIMSUB-NOT-EQUALS set to 100, and MS04-MAX-PRIMSUB-NOT-PROJ set to 100, and MS04-MAX-PRIMSUB-OR set to 100, and MS04-MAX-PRIMSUB-PROJ set to 100, and MS04-MAX-PROJS set to 100, and MS04-MAX-RIGID-MATES set to 100, and MS04-SOLVE-UNIF-DEPTH set to 500, and MS04-SOLVE-UNIF-DEPTH set to 5000, and MS04-VERBOSE set to MAX, and MS04-WEIGHT-PRIMSUB-FIRST-NOT set to 100, and MS04-WEIGHT-PRIMSUB-NEXT-NOT set to 100") (INTERRUPT-ENABLE T) (INITIAL-BKTRACK-LIMIT INFINITY) (INCLUDE-INDUCTION-PRINCIPLE NIL) (INCLUDE-COINDUCTION-PRINCIPLE NIL) (IMITATION-FIRST T) (FIRST-ORDER-MODE-MS NIL) (EXCLUDING-GC-TIME NIL) (ETA-RULE T) (DUPLICATION-STRATEGY-PFD DUP-INNER) (DUPLICATION-STRATEGY DUP-OUTER) (DUP-ALLOWED T) (DNEG-IMITATION CONST-FLEX) (DISSOLVE NIL) (DELAY-SETVARS NIL) (DEFAULT-MS MS04-2) (DEFAULT-MATE MS04-2) (DEFAULT-EXPAND MS04-2) (COUNTSUBS-FIRST NIL) (BAD-VAR-CONNECTED-PRUNE T) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS) (ALLOW-NONLEAF-CONNS NIL) (ADD-TRUTH IF-NEEDED))
 (mhelp ""))
