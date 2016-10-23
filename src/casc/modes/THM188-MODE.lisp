(defmode THM188-MODE
 (FLAG-SETTINGS (USE-SYMSIMP T) (USE-RULEP T) (USE-FAST-PROP-SEARCH T) (UNIFY-VERBOSE MED) (UNIF-TRIGGER NIL) (UNIF-COUNTER-OUTPUT 0) (UNIF-COUNTER 0) (UNI-SEARCH-HEURISTIC BREADTH-FIRST) (TRUTHVALUES-HACK NIL) (SUBSUMPTION-NODES LP-NODES) (SUBSUMPTION-DEPTH INFINITY) (SUBSUMPTION-CHECK NIL) (STOP-AT-TSN T) (SKOLEM-DEFAULT SK1) (SHOW-TIME T) (SEARCH-TIME-LIMIT 60) (SEARCH-COMPLETE-PATHS NIL) (RULEP-WFFEQ WFFEQ-AB) (RIGID-PATH-CK T) (REWRITE-EQUALITIES LEIBNIZ) (REWRITE-DEFNS (NONE)) (REMOVE-LEIBNIZ T) (REDUCE-DOUBLE-NEG T) (RECORDFLAGS (LAST-MODE-NAME DEFAULT-MS DEFAULT-MATE DEFAULT-EXPAND SHORT-SITE-NAME MACHINE-TYPE MACHINE-INSTANCE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ MAX-PRIM-DEPTH MIN-PRIM-DEPTH PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE)) (RANK-EPROOF-FN NUM-VPATHS-RANKING) (QUERY-USER NIL) (PRUNING NIL) (PROP-STRATEGY ALLOW-DUPLICATES) (PRINTMATEOPS ALWAYS-TRUE) (PRINTMATEFLAG-SLIDES NIL) (PRINTMATEFLAG NIL) (PRINTMATEFILE "mate.mss") (PRINT-MATING-COUNTER 300000) (PRIMSUB-METHOD PR93) (PRIM-QUANTIFIER T) (PRIM-PREFIX PRIM) (PRIM-BDTYPES-AUTO REPLACE) (PRIM-BDTYPES ("A")) (ORDER-COMPONENTS NIL) (OCCURS-CHECK T) (NUM-OF-DUPS 0) (NUM-FRPAIRS 5) (NEW-MATING-AFTER-DUP NIL) (NEG-PRIM-SUB NIL) (MS90-3-DUP-STRATEGY 1) (MS-SPLIT T) (MS-INIT-PATH NIL) (MS-DIR QUASI-TPS1) (MONITORFLAG NIL) (MIN-QUICK-DEPTH 3) (MIN-QUANTIFIER-SCOPE NIL) (MIN-QUANT-ETREE T) (MIN-PRIM-LITS 2) (MIN-PRIM-DEPTH 1) (MAX-UTREE-DEPTH 17) (MAX-SUBSTS-VAR 6) (MAX-SUBSTS-QUICK NIL) (MAX-SUBSTS-PROJ-TOTAL 11) (MAX-SUBSTS-PROJ 4) (MAX-SEARCH-LIMIT NIL) (MAX-SEARCH-DEPTH 17) (MAX-PRIM-LITS 4) (MAX-PRIM-DEPTH 1) (MAX-MATES 1) (MAX-DUP-PATHS INFINITY) (MATING-VERBOSE MED) (MATE-FFPAIR NIL) (LAST-MODE-NAME ", and CHARSIZE set to MAX, and REWRITE-EQUAL-EXT set to NIL, and REWRITE-DEFNS set to NIL, and REWRITE-DEFNS-EAGER set to NIL, and LETTER-GRADE-FILE set to , and LIB-MASTERINDEX-FILE set to libindex.rec, and LISP-IMPLEMENTATION-TYPE set to CMU Common Lisp 17e, and MACHINE-INSTANCE set to DTPS.TPS.CS.CMU.EDU, and MACHINE-TYPE set to HPPA, and MAX-MATES set to 2, and NEWS-DIR set to /users/theorem/tps/hp_cmucl/, and NUM-OF-DUPS set to 2, and OLD-GRADE-FILE set to , and OLD-TOTALS-GRADE-FILE set to , and PATCH-FILE set to grader.patch, and PPWFFLAG set to T, and PRIM-BDTYPES set to (A), and PRINTMATEFILE set to mate.mss, and PRINTTYPES set to T, and REC-MS-FILENAME set to mating.rec, and RECORDFLAGS set to (LAST-MODE-NAME DEFAULT-MS DEFAULT-MATE DEFAULT-EXPAND SHORT-SITE-NAME MACHINE-TYPE MACHINE-INSTANCE LISP-IMPLEMENTATION-TYPE MAX-SEARCH-LIMIT SEARCH-TIME-LIMIT MAX-UTREE-DEPTH MAX-SEARCH-DEPTH MIN-QUICK-DEPTH INITIAL-BKTRACK-LIMIT NUM-FRPAIRS FIRST-ORDER-MODE-MS NUM-OF-DUPS MAX-MATES ORDER-COMPONENTS REWRITE-DEFNS REWRITE-EQUALITIES REMOVE-LEIBNIZ MAX-PRIM-DEPTH MIN-PRIM-DEPTH PRIM-QUANTIFIER PRIM-BDTYPES NEG-PRIM-SUB OCCURS-CHECK RANK-EPROOF-FN SKOLEM-DEFAULT MIN-QUANTIFIER-SCOPE), and RENUMBER-LEAVES set to T, and REWRITE-DEFNS set to (QUOTE (NONE)), and RULE-ERROR-FILE set to etps3.rerror, and SAVE-FILE set to /users/theorem/tps/hp_cmucl/tps3, and SCORE-FILE set to etps3.scores, and SCRIBE-POSTAMBLE set to @End(Verbatim), and SCRIBE-PREAMBLE set to @Use(Database=/afs/cs.cmu.edu/project/tps/tps/doc/lib)
@LibraryFile(KSets)
@LibraryFile(Mathematics10)
@LibraryFile(symb10)
@Include (/afs/cs.cmu.edu/project/tps/tps/doc/lib/tps.mss)
, and SHORT-SITE-NAME set to CMU-SCS, and SLIDES-PREAMBLE set to @make(slides)
@Use(Database=/afs/cs.cmu.edu/project/tps/tps/doc/lib)
@modify(verbatim, spacing 2, linewidth 51)
@style(rightmargin = .25in)
@style(leftmargin = .25in)
@libraryfile(tps18)
@include (/afs/cs.cmu.edu/project/tps/tps/doc/lib/tps.mss)
, and SOURCE-EXTENSION set to lisp, and SOURCE-PATH set to (/users/theorem/tps/hp_cmucl/bin/ /users/theorem/tps/hp_cmucl/lisp/ /afs/cs/project/tps/tps/proofs/ /afs/cs/project/tps/tps/proofs/incomplete/), and STATISTICAL-OPTIONS set to (-MEAN- -MEDIAN- -SDEV-), and TEST-EASIER-IF-HIGH set to (MAX-SEARCH-DEPTH SEARCH-TIME-LIMIT NUM-OF-DUPS MAX-UTREE-DEPTH MAX-MATES MAX-SEARCH-LIMIT), and TEST-EASIER-IF-LOW set to (MIN-QUICK-DEPTH), and TEST-EASIER-IF-T set to (ETA-RULE MIN-QUANTIFIER-SCOPE MS-SPLIT), and TEST-FASTER-IF-HIGH set to (MIN-QUICK-DEPTH), and TEST-FASTER-IF-LOW set to (MAX-SEARCH-DEPTH SEARCH-TIME-LIMIT NUM-OF-DUPS MAX-UTREE-DEPTH MAX-MATES MAX-SEARCH-LIMIT), and TEST-FASTER-IF-T set to (MIN-QUANTIFIER-SCOPE MS-SPLIT), and TEST-THEOREMS set to ((THM30 MODE-THM30) (THM47 MODE-THM47-G) (THM48 MODE-THM48-E) (THM67 MODE-THM67-A) (THM104 MODE-THM104-A) (THM112 MODE-THM112-B) (THM112A MODE-THM112A-TRY4) (THM117C MODE-THM117B) (THM129 MODE-THM129-E) (THM130 MODE-THM129-B) (THM131 MODE-THM131-A) (THM133 MODE-X5200) (THM134 MODE-THM134-A) (THM135 MODE-THM135-1) (THM300A MODE-THM300A-2) (THM301A MODE-THM301-A) (THM303 MODE-THM303-DTPS) (BLEDSOE-FENG-SV-I1 MODE-THM129-D) (BLEDSOE-FENG-SV-I2 MODE-BLEDSOE-FENG-SV-I2-C) (X2115 MODE-X2129-A) (X2116 MODE-X2116) (X2129 MODE-X2129-C) (X5200 MODE-X5200-A) (X5205 MODE-X5205) (X5304 MODE-X5304) (X5305 MODE-X5305) (X5308 MODE-X5308-B) (X5310 MODE-X5310-A)), and TEX-1-POSTAMBLE set to \\vfill\\eject\\end, and TEX-1-PREAMBLE set to 
\\parindent=0pt, and TEX-POSTAMBLE set to \\eject\\end, and TEX-PREAMBLE set to 
\\raggedright
{\\baselineskip=4pt
\\halign{#\\hfil&\\quad\\vtop{\\rightskip=0truept plus 1truein\\parindent=0truept\\hsize=1truein#}\\hfil&\\quad#
          \\hfil&\\quad\\vtop{\\rightskip=0truept plus 3truein\\parindent=0truept\\hsize=3.5truein\\tabskip=0.5truein plus 1truein minus 0.45truein#}
          \\hfil&\\quad\\vtop{\\rightskip=0truept plus 1truein\\parindent=0truept\\hsize=1truein#}\\hfil\\cr
, and TOTALS-GRADE-FILE set to , and TPSTEX set to /afs/cs/project/tps/tps/doc/lib/tps.tex, and VPD-FILENAME set to vpd.vpf, and VPFORM-TEX-PREAMBLE set to \\magnification=\\magstep~D
\\input /afs/cs.cmu.edu/project/tps/tps/doc/lib/tps.tex
\\input /afs/cs.cmu.edu/project/tps/tps/doc/lib/vpd.tex, and NUM-OF-DUPS set to 0, and NUM-OF-DUPS set to 1, and MAX-SUBSTS-VAR set to NIL, and MAX-SUBSTS-PROJ set to NIL, and MAX-SUBSTS-PROJ-TOTAL set to NIL, and NUM-OF-DUPS set to 0, and MAX-SEARCH-DEPTH set to NIL, and MAX-UTREE-DEPTH set to NIL, and MIN-QUICK-DEPTH set to NIL, and MAX-SEARCH-DEPTH set to 17, and MAX-UTREE-DEPTH set to 17, and MIN-QUICK-DEPTH set to 17, and MIN-QUICK-DEPTH set to 3, and MAX-SUBSTS-VAR set to 6, and RENUMBER-LEAVES set to NIL, and MAX-MATES set to 1, and MAX-SUBSTS-PROJ set to 4, and MAX-SUBSTS-PROJ-TOTAL set to 11, and SOURCE-PATH set to (/users/theorem/tps/hp_allegro4.2/bin/ /users/theorem/tps/hp_allegro4.2/lisp/ /afs/cs/project/tps/tps/proofs/ /afs/cs/project/tps/tps/proofs/incomplete/)") (INTERRUPT-ENABLE T) (INITIAL-BKTRACK-LIMIT INFINITY) (IMITATION-FIRST T) (FIRST-ORDER-MODE-MS NIL) (EXCLUDING-GC-TIME T) (ETA-RULE T) (DUPLICATION-STRATEGY-PFD DUP-INNER) (DUPLICATION-STRATEGY DUP-OUTER) (DUP-ALLOWED T) (DNEG-IMITATION CONST-FLEX) (DEFAULT-MS MS91-7) (DEFAULT-MATE PFD) (DEFAULT-EXPAND OSET) (COUNTSUBS-FIRST NIL) (APPLY-MATCH APPLY-MATCH-ALL-FRDPAIRS))
 (mhelp ""))
