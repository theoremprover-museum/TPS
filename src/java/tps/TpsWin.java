/*
 *
 */

package tps;
 
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.io.*;
import tps.TpsApplet;
import tps.TpsThread;
import tps.TpsWindowListener;
import tps.TpsEventListener;
import tps.TpsKeyListener;
import tps.TpsMenuActionListener;
import tps.TpsBarListener;
 
public class TpsWin extends Frame {
    public boolean inAnApplet = true;
    TpsApplet tpsApplet;
    TpsThread tpsThread;
    String tpsServHost;
    int tpsServPort;
    public Scrollbar vBar;
    InetAddress addr;
    Socket sock;
    DataOutputStream sendtps;
    String commandStr = "COMMAND";
    String rightmarginStr = "RIGHTMARGIN";
    TpsWindowListener winListener;
    TpsEventListener eventListener;
    TpsKeyListener keyListener;
    String toplevel = "CMD-TOP";
    public boolean popups = true;
    public TextField resp;
    public boolean etps = false;
    public int rightOffset = 0;
    public int bottomOffset = 0;
 
    public TpsWin() {
	vBar = new Scrollbar(Scrollbar.VERTICAL);
    }

    // Menu Code BEGIN
    public Menu M0 = new Menu(new String("Changing"));
    public Menu M1 = new Menu(new String("Ed Moving"));
    public Menu M2 = new Menu(new String("Editor"));
    public Menu M3 = new Menu(new String("Exp Tree Ops"));
    public Menu M4 = new Menu(new String("JForms"));
    public Menu M5 = new Menu(new String("Library"));
    public Menu M6 = new Menu(new String("Mate"));
    public Menu M7 = new Menu(new String("Mate Printing"));
    public Menu M8 = new Menu(new String("Mating Search"));
    public Menu M9 = new Menu(new String("Modes"));
    public Menu M10 = new Menu(new String("Moving"));
    public Menu M11 = new Menu(new String("Mtree"));
    public Menu M12 = new Menu(new String("Mtree Ops"));
    public Menu M13 = new Menu(new String("MTree Print"));
    public Menu M14 = new Menu(new String("Review"));
    public Menu M15 = new Menu(new String("Searchlists"));
    public Menu M16 = new Menu(new String("Test"));
    public Menu M17 = new Menu(new String("Test Lib"));
    public Menu M18 = new Menu(new String("Unix-style Lib"));
    public Menu M19 = new Menu(new String("DPairs"));
    public Menu M20 = new Menu(new String("Unification"));
    public Menu M21 = new Menu(new String("Flags"));
    public Menu M22 = new Menu(new String("Main"));
    public Menu M23 = new Menu(new String("Misc-commands"));
    public Menu M24 = new Menu(new String("Rules"));
    public Menu M25 = new Menu(new String("Top-levels"));
    public MenuBar mbar = new MenuBar();

    private void etpsMenuItem(Menu m,String s1,String s2) {
        MenuItem mi = new MenuItem(new String(s1));
        TpsMenuActionListener ml = new TpsMenuActionListener();
        ml.tpsMenuActionInit(this,new String(s2));
        mi.addActionListener(ml);
        m.add(mi);
    }

    private void etpsMenuItem(Menu m,String s1,String s2,int key) {
        MenuItem mi = new MenuItem(new String(s1),new MenuShortcut(key));
        TpsMenuActionListener ml = new TpsMenuActionListener();
        ml.tpsMenuActionInit(this,new String(s2));
        mi.addActionListener(ml);
        m.add(mi);
    }

    private void tpsMenuItem(Menu m,String s1,String s2) {
        if (!etps) {
          etpsMenuItem(m,s1,s2);
        }
    }

    private void tpsMenuItem(Menu m,String s1,String s2,int key) {
        if (!etps) {
          etpsMenuItem(m,s1,s2,key);
        }
    }

    public void TpsWinMenuInit(boolean expert) {
        Menu M26 = new Menu(new String("Abbrev Ops"));
        Menu M27 = new Menu(new String("Best Modes"));
        Menu M28 = new Menu(new String("Ed JForms"));
        Menu M29 = new Menu(new String("Ed Scribe Record"));
        Menu M30 = new Menu(new String("Embedding"));
        Menu M31 = new Menu(new String("Ill-Formed Wff Ops"));
        Menu M32 = new Menu(new String("Inner Quant Ops"));
        Menu M33 = new Menu(new String("Keywords"));
        Menu M34 = new Menu(new String("Lambda Ops"));
        Menu M35 = new Menu(new String("Lib"));
        Menu M36 = new Menu(new String("Lib Class"));
        Menu M37 = new Menu(new String("Lib Display"));
        Menu M38 = new Menu(new String("Lib Read"));
        Menu M39 = new Menu(new String("Library Flags"));
        Menu M40 = new Menu(new String("Misc Ops"));
        Menu M41 = new Menu(new String("Negation Ops"));
        Menu M42 = new Menu(new String("Primsub Ops"));
        Menu M43 = new Menu(new String("Print"));
        Menu M44 = new Menu(new String("Rec Changing"));
        Menu M45 = new Menu(new String("Review Unification"));
        Menu M46 = new Menu(new String("Rewriting"));
        Menu M47 = new Menu(new String("Scribe Record"));
        Menu M48 = new Menu(new String("Skolemize"));
        Menu M49 = new Menu(new String("Struct"));
        Menu M50 = new Menu(new String("Substitution"));
        Menu M51 = new Menu(new String("Unixlib Class"));
        Menu M52 = new Menu(new String("Display"));
        Menu M53 = new Menu(new String("Read"));
        Menu M54 = new Menu(new String("Weak Labels"));
        Menu M55 = new Menu(new String("Write"));
        Menu M56 = new Menu(new String("Unification Flags"));
        Menu M57 = new Menu(new String("Automatically Generate Data"));
        Menu M58 = new Menu(new String("COLL-HELP"));
        Menu M59 = new Menu(new String("Conjunction"));
        Menu M60 = new Menu(new String("Definitions"));
        Menu M61 = new Menu(new String("Disjunction"));
        Menu M62 = new Menu(new String("Editor Flags"));
        Menu M63 = new Menu(new String("Entering"));
        Menu M64 = new Menu(new String("Entering Flags"));
        Menu M65 = new Menu(new String("Equality Flags"));
        Menu M66 = new Menu(new String("Equations"));
        Menu M67 = new Menu(new String("Equivalence"));
        Menu M68 = new Menu(new String("Etree to Nat"));
        Menu M69 = new Menu(new String("Etree to Nat Flags"));
        Menu M70 = new Menu(new String("Events"));
        Menu M71 = new Menu(new String("Expansion Tree Flags"));
        Menu M72 = new Menu(new String("Files"));
        Menu M73 = new Menu(new String("HELP-OBJ"));
        Menu M74 = new Menu(new String("Implication"));
        Menu M75 = new Menu(new String("Indirect"));
        Menu M76 = new Menu(new String("JForm Flags"));
        Menu M77 = new Menu(new String("Lambda"));
        Menu M78 = new Menu(new String("Library Top Levels"));
        Menu M79 = new Menu(new String("Lisp Packages"));
        Menu M80 = new Menu(new String("Maint"));
        Menu M81 = new Menu(new String("Manipulation Flags"));
        Menu M82 = new Menu(new String("Mating Search Commands"));
        Menu M83 = new Menu(new String("Mating Search Flags"));
        Menu M84 = new Menu(new String("Mating Tree Flags"));
        Menu M86 = new Menu(new String("Misc"));
        Menu M87 = new Menu(new String("Misc Flags"));
        Menu M88 = new Menu(new String("Modify"));
        Menu M89 = new Menu(new String("MS88 Flags"));
        Menu M90 = new Menu(new String("MS89 Flags"));
        Menu M91 = new Menu(new String("MS90-3 Flags"));
        Menu M92 = new Menu(new String("MS91"));
        Menu M93 = new Menu(new String("MS91 Flags"));
        Menu M94 = new Menu(new String("MS98-1 Flags"));
        Menu M95 = new Menu(new String("Naming"));
        Menu M96 = new Menu(new String("Nat to Etree"));
        Menu M97 = new Menu(new String("Nat to Etree Flags"));
        Menu M98 = new Menu(new String("Natural Deduction Display"));
        Menu M99 = new Menu(new String("Natural Deduction Flags"));
        Menu M100 = new Menu(new String("Negation"));
        Menu M101 = new Menu(new String("Parsing"));
        Menu M102 = new Menu(new String("Printing"));
        Menu M103 = new Menu(new String("Printing Flags"));
        Menu M104 = new Menu(new String("Proof Outlines"));
        Menu M105 = new Menu(new String("Proof Translations"));
        Menu M106 = new Menu(new String("Proof Windows"));
        Menu M107 = new Menu(new String("Propositional"));
        Menu M108 = new Menu(new String("Quantifiers"));
        Menu M109 = new Menu(new String("Rewrite Rules"));
        Menu M110 = new Menu(new String("Rule P Flags"));
        Menu M111 = new Menu(new String("Rule Run"));
        Menu M112 = new Menu(new String("Rules Object"));
        Menu M113 = new Menu(new String("Save"));
        Menu M114 = new Menu(new String("Saving"));
        Menu M115 = new Menu(new String("Scribe"));
        Menu M116 = new Menu(new String("Search Flags"));
        Menu M117 = new Menu(new String("Search Suggestions"));
        Menu M118 = new Menu(new String("Sequent Calculus"));
        Menu M119 = new Menu(new String("Sequent Calculus Flags"));
        Menu M120 = new Menu(new String("Set Mode"));
        Menu M121 = new Menu(new String("Set Substitutions"));
        Menu M122 = new Menu(new String("Status"));
        Menu M123 = new Menu(new String("Substitions"));
        Menu M124 = new Menu(new String("Suggestion Flags"));
        Menu M125 = new Menu(new String("Suggestions"));
        Menu M126 = new Menu(new String("Tactic Flags"));
        Menu M127 = new Menu(new String("Tactics"));
        Menu M128 = new Menu(new String("Test Searchlists"));
        Menu M129 = new Menu(new String("Tex"));
        Menu M130 = new Menu(new String("Tps Maintenance"));
        Menu M131 = new Menu(new String("Tps Modules"));
        Menu M132 = new Menu(new String("Vars"));
        Menu M133 = new Menu(new String("Search"));
        if (expert) {
            MenuItem comi = new MenuItem("COMMAND (Enter Command)",new MenuShortcut(97));
            TpsMenuActionListener coml = new TpsMenuActionListener();
            coml.tpsMenuActionGenCommand(sendtps);
            comi.addActionListener(coml);
            M22.add(comi);
        }
        mbar.add(M22);
        etpsMenuItem(M22,"PROVE","PROVE",112);
        etpsMenuItem(M22,"EXERCISE","EXERCISE");
        tpsMenuItem(M22,"DIY (Auto Search)","DIY",100);
         M22.add(M120);
        etpsMenuItem(M120,"MODE","MODE");
        etpsMenuItem(M120,"MODE MS98-FO-MODE","MODE MS98-FO-MODE");
        etpsMenuItem(M120,"MODE MS98-HO-MODE","MODE MS98-HO-MODE");
        etpsMenuItem(M120,"MODE EASY-SV-MODE","MODE EASY-SV-MODE");
        etpsMenuItem(M22,"PALL (Display Proof)","PALL");
        etpsMenuItem(M22,"RECONSIDER","RECONSIDER");
        etpsMenuItem(M22,"RESTOREPROOF","RESTOREPROOF");
        etpsMenuItem(M22,"HELP","HELP",104);
        M22.add(new MenuItem("-"));
        etpsMenuItem(M22,"INTERRUPT","INTERRUPT",99);
        M22.add(new MenuItem("-"));
        etpsMenuItem(M22,"HISTORY","HISTORY");
        M22.add(new MenuItem("-"));
        etpsMenuItem(M22,"EXIT","EXIT",120);
        mbar.add(M21);
         M21.add(M116);
        if (!etps) {
         M116.add(M128);
        }
        tpsMenuItem(M128,"TEST-EASIER-IF-HIGH","TEST-EASIER-IF-HIGH");
        tpsMenuItem(M128,"TEST-EASIER-IF-LOW","TEST-EASIER-IF-LOW");
        tpsMenuItem(M128,"TEST-EASIER-IF-NIL","TEST-EASIER-IF-NIL");
        tpsMenuItem(M128,"TEST-EASIER-IF-T","TEST-EASIER-IF-T");
        tpsMenuItem(M128,"TEST-FASTER-IF-HIGH","TEST-FASTER-IF-HIGH");
        tpsMenuItem(M128,"TEST-FASTER-IF-LOW","TEST-FASTER-IF-LOW");
        tpsMenuItem(M128,"TEST-FASTER-IF-NIL","TEST-FASTER-IF-NIL");
        tpsMenuItem(M128,"TEST-FASTER-IF-T","TEST-FASTER-IF-T");
        tpsMenuItem(M128,"TEST-FIX-UNIF-DEPTHS","TEST-FIX-UNIF-DEPTHS");
        tpsMenuItem(M128,"TEST-INCREASE-TIME","TEST-INCREASE-TIME");
        tpsMenuItem(M128,"TEST-INITIAL-TIME-LIMIT","TEST-INITIAL-TIME-LIMIT");
        tpsMenuItem(M128,"TEST-MAX-SEARCH-VALUES","TEST-MAX-SEARCH-VALUES");
        tpsMenuItem(M128,"TEST-NEXT-SEARCH-FN","TEST-NEXT-SEARCH-FN");
        tpsMenuItem(M128,"TEST-REDUCE-TIME","TEST-REDUCE-TIME");
        tpsMenuItem(M128,"TEST-VERBOSE","TEST-VERBOSE");
        tpsMenuItem(M128,"TESTWIN-HEIGHT","TESTWIN-HEIGHT");
        tpsMenuItem(M128,"TESTWIN-WIDTH","TESTWIN-WIDTH");
        if (!etps) {
         M116.add(M121);
        }
        tpsMenuItem(M121,"BAD-VAR-CONNECTED-PRUNE","BAD-VAR-CONNECTED-PRUNE");
        tpsMenuItem(M121,"DELAY-SETVARS","DELAY-SETVARS");
        tpsMenuItem(M121,"INCLUDE-COINDUCTION-PRINCIPLE","INCLUDE-COINDUCTION-PRINCIPLE");
        tpsMenuItem(M121,"INCLUDE-INDUCTION-PRINCIPLE","INCLUDE-INDUCTION-PRINCIPLE");
        tpsMenuItem(M121,"MAX-CONSTRAINT-SIZE","MAX-CONSTRAINT-SIZE");
        tpsMenuItem(M121,"MAX-NUM-CONSTRAINTS","MAX-NUM-CONSTRAINTS");
        tpsMenuItem(M121,"PRIMSUB-VAR-SELECT","PRIMSUB-VAR-SELECT");
        tpsMenuItem(M121,"MAX-PRIM-DEPTH","MAX-PRIM-DEPTH");
        tpsMenuItem(M121,"MAX-PRIM-LITS","MAX-PRIM-LITS");
        tpsMenuItem(M121,"MIN-PRIM-DEPTH","MIN-PRIM-DEPTH");
        tpsMenuItem(M121,"MIN-PRIM-LITS","MIN-PRIM-LITS");
        tpsMenuItem(M121,"NEG-PRIM-SUB","NEG-PRIM-SUB");
        tpsMenuItem(M121,"PR00-ALLOW-SUBNODE-CONNS","PR00-ALLOW-SUBNODE-CONNS");
        tpsMenuItem(M121,"PR00-MAX-SUBSTS-VAR","PR00-MAX-SUBSTS-VAR");
        tpsMenuItem(M121,"PR00-NUM-ITERATIONS","PR00-NUM-ITERATIONS");
        tpsMenuItem(M121,"PR00-REQUIRE-ARG-DEPS","PR00-REQUIRE-ARG-DEPS");
        tpsMenuItem(M121,"PR97C-MAX-ABBREVS","PR97C-MAX-ABBREVS");
        tpsMenuItem(M121,"PR97C-PRENEX","PR97C-PRENEX");
        tpsMenuItem(M121,"PRIM-BDTYPES","PRIM-BDTYPES");
        tpsMenuItem(M121,"PRIM-BDTYPES-AUTO","PRIM-BDTYPES-AUTO");
        tpsMenuItem(M121,"PRIM-PREFIX","PRIM-PREFIX");
        tpsMenuItem(M121,"PRIMSUB-METHOD","PRIMSUB-METHOD");
        tpsMenuItem(M121,"WHICH-CONSTRAINTS","WHICH-CONSTRAINTS");
        if (!etps) {
         M116.add(M119);
        }
        tpsMenuItem(M119,"PSEQ-USE-LABELS","PSEQ-USE-LABELS");
        if (!etps) {
         M116.add(M94);
        }
        tpsMenuItem(M94,"BREAK-AT-QUANTIFIERS","BREAK-AT-QUANTIFIERS");
        tpsMenuItem(M94,"FF-DELAY","FF-DELAY");
        tpsMenuItem(M94,"HPATH-THRESHOLD","HPATH-THRESHOLD");
        tpsMenuItem(M94,"MAXIMIZE-FIRST","MAXIMIZE-FIRST");
        tpsMenuItem(M94,"MEASUREMENTS","MEASUREMENTS");
        tpsMenuItem(M94,"MS98-BASE-PRIM","MS98-BASE-PRIM");
        tpsMenuItem(M94,"MS98-DUP-BELOW-PRIMSUBS","MS98-DUP-BELOW-PRIMSUBS");
        tpsMenuItem(M94,"MS98-DUP-PRIMSUBS","MS98-DUP-PRIMSUBS");
        tpsMenuItem(M94,"MS98-FIRST-FRAGMENT","MS98-FIRST-FRAGMENT");
        tpsMenuItem(M94,"MS98-FORCE-H-O","MS98-FORCE-H-O");
        tpsMenuItem(M94,"MS98-FRAGMENT-PLACEMENT","MS98-FRAGMENT-PLACEMENT");
        tpsMenuItem(M94,"MS98-INIT","MS98-INIT");
        tpsMenuItem(M94,"MS98-LOW-MEMORY","MS98-LOW-MEMORY");
        tpsMenuItem(M94,"MS98-MAX-COMPONENTS","MS98-MAX-COMPONENTS");
        tpsMenuItem(M94,"MS98-MAX-PRIMS","MS98-MAX-PRIMS");
        tpsMenuItem(M94,"MS98-MEASURE","MS98-MEASURE");
        tpsMenuItem(M94,"MS98-MERGE-DAGS","MS98-MERGE-DAGS");
        tpsMenuItem(M94,"MS98-MINIMALITY-CHECK","MS98-MINIMALITY-CHECK");
        tpsMenuItem(M94,"MS98-NUM-OF-DUPS","MS98-NUM-OF-DUPS");
        tpsMenuItem(M94,"MS98-PRIMSUB-COUNT","MS98-PRIMSUB-COUNT");
        tpsMenuItem(M94,"MS98-REW-PRIMSUBS","MS98-REW-PRIMSUBS");
        tpsMenuItem(M94,"MS98-REWRITE-DEPTH","MS98-REWRITE-DEPTH");
        tpsMenuItem(M94,"MS98-REWRITE-MODEL","MS98-REWRITE-MODEL");
        tpsMenuItem(M94,"MS98-REWRITE-PRUNE","MS98-REWRITE-PRUNE");
        tpsMenuItem(M94,"MS98-REWRITE-SIZE","MS98-REWRITE-SIZE");
        tpsMenuItem(M94,"MS98-REWRITE-UNIF","MS98-REWRITE-UNIF");
        tpsMenuItem(M94,"MS98-REWRITES","MS98-REWRITES");
        tpsMenuItem(M94,"MS98-TRACE","MS98-TRACE");
        tpsMenuItem(M94,"MS98-UNIF-HACK","MS98-UNIF-HACK");
        tpsMenuItem(M94,"MS98-UNIF-HACK2","MS98-UNIF-HACK2");
        tpsMenuItem(M94,"MS98-USE-COLORS","MS98-USE-COLORS");
        tpsMenuItem(M94,"MS98-VALID-PAIR","MS98-VALID-PAIR");
        tpsMenuItem(M94,"MS98-VARIABLE-PLACEMENT","MS98-VARIABLE-PLACEMENT");
        tpsMenuItem(M94,"MS98-VERBOSE","MS98-VERBOSE");
        if (!etps) {
         M116.add(M93);
        }
        tpsMenuItem(M93,"MS91-INTERLEAVE","MS91-INTERLEAVE");
        tpsMenuItem(M93,"MS91-PREFER-SMALLER","MS91-PREFER-SMALLER");
        tpsMenuItem(M93,"MS91-TIME-BY-VPATHS","MS91-TIME-BY-VPATHS");
        tpsMenuItem(M93,"MS91-WEIGHT-LIMIT-RANGE","MS91-WEIGHT-LIMIT-RANGE");
        tpsMenuItem(M93,"NEW-OPTION-SET-LIMIT","NEW-OPTION-SET-LIMIT");
        tpsMenuItem(M93,"OPTIONS-GENERATE-ARG","OPTIONS-GENERATE-ARG");
        tpsMenuItem(M93,"OPTIONS-GENERATE-FN","OPTIONS-GENERATE-FN");
        tpsMenuItem(M93,"OPTIONS-GENERATE-UPDATE","OPTIONS-GENERATE-UPDATE");
        tpsMenuItem(M93,"OPTIONS-VERBOSE","OPTIONS-VERBOSE");
        tpsMenuItem(M93,"PENALTY-FOR-EACH-PRIMSUB","PENALTY-FOR-EACH-PRIMSUB");
        tpsMenuItem(M93,"PENALTY-FOR-MULTIPLE-PRIMSUBS","PENALTY-FOR-MULTIPLE-PRIMSUBS");
        tpsMenuItem(M93,"PENALTY-FOR-MULTIPLE-SUBS","PENALTY-FOR-MULTIPLE-SUBS");
        tpsMenuItem(M93,"PENALTY-FOR-ORDINARY-DUP","PENALTY-FOR-ORDINARY-DUP");
        tpsMenuItem(M93,"RECONSIDER-FN","RECONSIDER-FN");
        tpsMenuItem(M93,"WEIGHT-A-COEFFICIENT","WEIGHT-A-COEFFICIENT");
        tpsMenuItem(M93,"WEIGHT-A-FN","WEIGHT-A-FN");
        tpsMenuItem(M93,"WEIGHT-B-COEFFICIENT","WEIGHT-B-COEFFICIENT");
        tpsMenuItem(M93,"WEIGHT-B-FN","WEIGHT-B-FN");
        tpsMenuItem(M93,"WEIGHT-C-COEFFICIENT","WEIGHT-C-COEFFICIENT");
        tpsMenuItem(M93,"WEIGHT-C-FN","WEIGHT-C-FN");
        if (!etps) {
         M116.add(M91);
        }
        tpsMenuItem(M91,"MAX-MATES","MAX-MATES");
        tpsMenuItem(M91,"MIN-QUANT-ETREE","MIN-QUANT-ETREE");
        tpsMenuItem(M91,"MS90-3-DUP-STRATEGY","MS90-3-DUP-STRATEGY");
        tpsMenuItem(M91,"NUM-FRPAIRS","NUM-FRPAIRS");
        tpsMenuItem(M91,"PRINT-MATING-COUNTER","PRINT-MATING-COUNTER");
        tpsMenuItem(M91,"SHOW-TIME","SHOW-TIME");
        if (!etps) {
         M116.add(M90);
        }
        tpsMenuItem(M90,"MAX-SEARCH-LIMIT","MAX-SEARCH-LIMIT");
        tpsMenuItem(M90,"RANK-EPROOF-FN","RANK-EPROOF-FN");
        tpsMenuItem(M90,"SEARCH-TIME-LIMIT","SEARCH-TIME-LIMIT");
        if (!etps) {
         M116.add(M89);
        }
        tpsMenuItem(M89,"ADDED-CONN-ENABLED","ADDED-CONN-ENABLED");
        tpsMenuItem(M89,"CONSIDERED-CONN-ENABLED","CONSIDERED-CONN-ENABLED");
        tpsMenuItem(M89,"DUP-ALLOWED","DUP-ALLOWED");
        tpsMenuItem(M89,"DUPE-ENABLED","DUPE-ENABLED");
        tpsMenuItem(M89,"DUPE-VAR-ENABLED","DUPE-VAR-ENABLED");
        tpsMenuItem(M89,"EXCLUDING-GC-TIME","EXCLUDING-GC-TIME");
        tpsMenuItem(M89,"FIRST-PLACEMENT-MODE-MS","FIRST-PLACEMENT-MODE-MS");
        tpsMenuItem(M89,"INCOMP-MATING-ENABLED","INCOMP-MATING-ENABLED");
        tpsMenuItem(M89,"MATE-FFPAIR","MATE-FFPAIR");
        tpsMenuItem(M89,"MATE-SUBSUMED-TEST-ENABLED","MATE-SUBSUMED-TEST-ENABLED");
        tpsMenuItem(M89,"MATE-SUBSUMED-TRUE-ENABLED","MATE-SUBSUMED-TRUE-ENABLED");
        tpsMenuItem(M89,"MATING-CHANGED-ENABLED","MATING-CHANGED-ENABLED");
        tpsMenuItem(M89,"MS-INIT-PATH","MS-INIT-PATH");
        tpsMenuItem(M89,"MS-SPLIT","MS-SPLIT");
        tpsMenuItem(M89,"OCCURS-CHECK","OCCURS-CHECK");
        tpsMenuItem(M89,"PRIM-QUANTIFIER","PRIM-QUANTIFIER");
        tpsMenuItem(M89,"PRIMSUB-ENABLED","PRIMSUB-ENABLED");
        tpsMenuItem(M89,"PROP-STRATEGY","PROP-STRATEGY");
        tpsMenuItem(M89,"REMOVED-CONN-ENABLED","REMOVED-CONN-ENABLED");
        tpsMenuItem(M89,"SEARCH-COMPLETE-PATHS","SEARCH-COMPLETE-PATHS");
        tpsMenuItem(M89,"START-TIME-ENABLED","START-TIME-ENABLED");
        tpsMenuItem(M89,"STOP-TIME-ENABLED","STOP-TIME-ENABLED");
        tpsMenuItem(M89,"TIMING-NAMED","TIMING-NAMED");
        tpsMenuItem(M89,"UNIF-SUBSUMED-TEST-ENABLED","UNIF-SUBSUMED-TEST-ENABLED");
        tpsMenuItem(M89,"UNIF-SUBSUMED-TRUE-ENABLED","UNIF-SUBSUMED-TRUE-ENABLED");
        if (!etps) {
         M116.add(M84);
        }
        tpsMenuItem(M84,"MT-SUBSUMPTION-CHECK","MT-SUBSUMPTION-CHECK");
        tpsMenuItem(M84,"MT94-12-TRIGGER","MT94-12-TRIGGER");
        tpsMenuItem(M84,"MTREE-FILTER-DUPS","MTREE-FILTER-DUPS");
        tpsMenuItem(M84,"MTREE-STOP-IMMEDIATELY","MTREE-STOP-IMMEDIATELY");
        tpsMenuItem(M84,"TAG-CONN-FN","TAG-CONN-FN");
        tpsMenuItem(M84,"TAG-MATING-FN","TAG-MATING-FN");
        tpsMenuItem(M84,"DEFAULT-OB","DEFAULT-OB");
        tpsMenuItem(M84,"MT-DUPS-PER-QUANT","MT-DUPS-PER-QUANT");
        tpsMenuItem(M84,"MT-DEFAULT-OB-MATE","MT-DEFAULT-OB-MATE");
        if (!etps) {
         M116.add(M83);
        }
        tpsMenuItem(M83,"ASSERT-LEMMAS","ASSERT-LEMMAS");
        tpsMenuItem(M83,"DEFAULT-EXPAND","DEFAULT-EXPAND");
        tpsMenuItem(M83,"DEFAULT-MATE","DEFAULT-MATE");
        tpsMenuItem(M83,"DEFAULT-MS","DEFAULT-MS");
        tpsMenuItem(M83,"INTERRUPT-ENABLE","INTERRUPT-ENABLE");
        tpsMenuItem(M83,"MATING-VERBOSE","MATING-VERBOSE");
        tpsMenuItem(M83,"MONITORFLAG","MONITORFLAG");
        tpsMenuItem(M83,"NEW-MATING-AFTER-DUP","NEW-MATING-AFTER-DUP");
        tpsMenuItem(M83,"QUERY-USER","QUERY-USER");
        tpsMenuItem(M83,"REC-MS-FILE","REC-MS-FILE");
        tpsMenuItem(M83,"REC-MS-FILENAME","REC-MS-FILENAME");
        tpsMenuItem(M83,"USE-DIY","USE-DIY");
        tpsMenuItem(M83,"USE-EXT-LEMMAS","USE-EXT-LEMMAS");
        tpsMenuItem(M83,"USE-FAST-PROP-SEARCH","USE-FAST-PROP-SEARCH");
        if (!etps) {
         M116.add(M76);
        }
        tpsMenuItem(M76,"ALLOW-NONLEAF-CONNS","ALLOW-NONLEAF-CONNS");
        tpsMenuItem(M76,"DISSOLVE","DISSOLVE");
        tpsMenuItem(M76,"MATE-UP-TO-NNF","MATE-UP-TO-NNF");
        tpsMenuItem(M76,"PLACEMENT-COMPONENTS","PLACEMENT-COMPONENTS");
        tpsMenuItem(M76,"PRINT-LIT-NAME","PRINT-LIT-NAME");
        tpsMenuItem(M76,"PRINTVPDFLAG","PRINTVPDFLAG");
        tpsMenuItem(M76,"TEXFORMAT","TEXFORMAT");
        tpsMenuItem(M76,"VPD-BRIEF","VPD-BRIEF");
        tpsMenuItem(M76,"VPD-FILENAME","VPD-FILENAME");
        tpsMenuItem(M76,"VPD-PTYPES","VPD-PTYPES");
        tpsMenuItem(M76,"VPD-STYLE","VPD-STYLE");
        tpsMenuItem(M76,"VPD-VPFPAGE","VPD-VPFPAGE");
        tpsMenuItem(M76,"VPFORM-LABELS","VPFORM-LABELS");
        tpsMenuItem(M76,"VPFORM-TEX-MAGNIFICATION","VPFORM-TEX-MAGNIFICATION");
        tpsMenuItem(M76,"VPFORM-TEX-NEST","VPFORM-TEX-NEST");
        tpsMenuItem(M76,"VPFORM-TEX-PREAMBLE","VPFORM-TEX-PREAMBLE");
        tpsMenuItem(M76,"VPW-HEIGHT","VPW-HEIGHT");
        tpsMenuItem(M76,"VPW-WIDTH","VPW-WIDTH");
        if (!etps) {
         M116.add(M71);
        }
        tpsMenuItem(M71,"ADD-TRUTH","ADD-TRUTH");
        tpsMenuItem(M71,"DUPLICATION-STRATEGY","DUPLICATION-STRATEGY");
        tpsMenuItem(M71,"DUPLICATION-STRATEGY-PFD","DUPLICATION-STRATEGY-PFD");
        tpsMenuItem(M71,"INITIAL-BKTRACK-LIMIT","INITIAL-BKTRACK-LIMIT");
        tpsMenuItem(M71,"MIN-QUANTIFIER-SCOPE","MIN-QUANTIFIER-SCOPE");
        tpsMenuItem(M71,"PRINT-DEEP","PRINT-DEEP");
        tpsMenuItem(M71,"PRINT-NODENAMES","PRINT-NODENAMES");
        tpsMenuItem(M71,"SHOW-SKOLEM","SHOW-SKOLEM");
        tpsMenuItem(M71,"SKOLEM-DEFAULT","SKOLEM-DEFAULT");
        tpsMenuItem(M71,"TRUTHVALUES-HACK","TRUTHVALUES-HACK");
         M116.add(M65);
        etpsMenuItem(M65,"LAMBDA-CONV","LAMBDA-CONV");
        tpsMenuItem(M65,"REWRITE-DEFNS","REWRITE-DEFNS");
        etpsMenuItem(M65,"REWRITE-EQUALITIES","REWRITE-EQUALITIES");
        etpsMenuItem(M65,"REWRITE-EQUIVS","REWRITE-EQUIVS");
         M21.add(M106);
        etpsMenuItem(M106,"PROOFW-ACTIVE","PROOFW-ACTIVE");
        etpsMenuItem(M106,"PROOFW-ACTIVE+NOS","PROOFW-ACTIVE+NOS");
        etpsMenuItem(M106,"PROOFW-ACTIVE+NOS-HEIGHT","PROOFW-ACTIVE+NOS-HEIGHT");
        etpsMenuItem(M106,"PROOFW-ACTIVE+NOS-WIDTH","PROOFW-ACTIVE+NOS-WIDTH");
        etpsMenuItem(M106,"PROOFW-ACTIVE-HEIGHT","PROOFW-ACTIVE-HEIGHT");
        etpsMenuItem(M106,"PROOFW-ACTIVE-WIDTH","PROOFW-ACTIVE-WIDTH");
        etpsMenuItem(M106,"PROOFW-ALL","PROOFW-ALL");
        etpsMenuItem(M106,"PROOFW-ALL-HEIGHT","PROOFW-ALL-HEIGHT");
        etpsMenuItem(M106,"PROOFW-ALL-WIDTH","PROOFW-ALL-WIDTH");
        if (!etps) {
         M21.add(M105);
        }
        if (!etps) {
         M105.add(M126);
        }
        tpsMenuItem(M126,"USE-SYMSIMP","USE-SYMSIMP");
        tpsMenuItem(M126,"UI-HERBRAND-LIMIT","UI-HERBRAND-LIMIT");
        tpsMenuItem(M126,"DEFAULT-TACTIC","DEFAULT-TACTIC");
        tpsMenuItem(M126,"TACMODE","TACMODE");
        tpsMenuItem(M126,"TACTIC-VERBOSE","TACTIC-VERBOSE");
        tpsMenuItem(M126,"TACUSE","TACUSE");
        if (!etps) {
         M105.add(M97);
        }
        tpsMenuItem(M97,"NAT-ETREE-VERSION","NAT-ETREE-VERSION");
        tpsMenuItem(M97,"NATREE-DEBUG","NATREE-DEBUG");
        tpsMenuItem(M97,"RENUMBER-LEAVES","RENUMBER-LEAVES");
        if (!etps) {
         M105.add(M69);
        }
        tpsMenuItem(M69,"ETREE-NAT-VERBOSE","ETREE-NAT-VERBOSE");
        tpsMenuItem(M69,"REMOVE-LEIBNIZ","REMOVE-LEIBNIZ");
        tpsMenuItem(M69,"MATINGSTREE-NAME","MATINGSTREE-NAME");
        tpsMenuItem(M69,"MERGE-MINIMIZE-MATING","MERGE-MINIMIZE-MATING");
         M21.add(M103);
        etpsMenuItem(M103,"INFIX-NOTATION","INFIX-NOTATION");
        etpsMenuItem(M103,"RIGHTMARGIN","RIGHTMARGIN");
        etpsMenuItem(M103,"SCOPE","SCOPE");
        etpsMenuItem(M103,"USE-DOT","USE-DOT");
        etpsMenuItem(M103,"PRINT-WEAK","PRINT-WEAK");
         M103.add(M129);
        etpsMenuItem(M129,"LATEX-POSTAMBLE","LATEX-POSTAMBLE");
        etpsMenuItem(M129,"LATEX-PREAMBLE","LATEX-PREAMBLE");
        etpsMenuItem(M129,"TEX-1-POSTAMBLE","TEX-1-POSTAMBLE");
        etpsMenuItem(M129,"TEX-1-PREAMBLE","TEX-1-PREAMBLE");
        etpsMenuItem(M129,"TEX-LINE-WIDTH","TEX-LINE-WIDTH");
        etpsMenuItem(M129,"TEX-POSTAMBLE","TEX-POSTAMBLE");
        etpsMenuItem(M129,"TEX-PREAMBLE","TEX-PREAMBLE");
        etpsMenuItem(M129,"TPSTEX","TPSTEX");
        etpsMenuItem(M129,"VPDTEX","VPDTEX");
        etpsMenuItem(M129,"IN-TEX-MATH-MODE","IN-TEX-MATH-MODE");
        etpsMenuItem(M129,"LATEX-EMULATION","LATEX-EMULATION");
        etpsMenuItem(M129,"PAGELENGTH","PAGELENGTH");
        etpsMenuItem(M129,"TEX-MIMIC-SCRIBE","TEX-MIMIC-SCRIBE");
         M103.add(M115);
        etpsMenuItem(M115,"SCRIBE-LINE-WIDTH","SCRIBE-LINE-WIDTH");
        etpsMenuItem(M115,"SCRIBE-POSTAMBLE","SCRIBE-POSTAMBLE");
        etpsMenuItem(M115,"SCRIBE-PREAMBLE","SCRIBE-PREAMBLE");
        etpsMenuItem(M115,"PRINTEDTFILE","PRINTEDTFILE");
        etpsMenuItem(M115,"PRINTEDTFLAG","PRINTEDTFLAG");
        etpsMenuItem(M115,"PRINTEDTFLAG-SLIDES","PRINTEDTFLAG-SLIDES");
        etpsMenuItem(M115,"PRINTEDTOPS","PRINTEDTOPS");
        tpsMenuItem(M115,"PRINTMATEFILE","PRINTMATEFILE");
        tpsMenuItem(M115,"PRINTMATEFLAG","PRINTMATEFLAG");
        tpsMenuItem(M115,"PRINTMATEFLAG-SLIDES","PRINTMATEFLAG-SLIDES");
        tpsMenuItem(M115,"PRINTMATEOPS","PRINTMATEOPS");
         M103.add(M98);
        etpsMenuItem(M98,"PRINT-COMBINED-EGENS","PRINT-COMBINED-EGENS");
        etpsMenuItem(M98,"PRINT-COMBINED-UGENS","PRINT-COMBINED-UGENS");
        etpsMenuItem(M98,"PRINT-COMBINED-UIS","PRINT-COMBINED-UIS");
        etpsMenuItem(M98,"PRINT-UNTIL-UI-OR-EGEN","PRINT-UNTIL-UI-OR-EGEN");
         M103.add(M86);
        etpsMenuItem(M86,"PRINT-COMMENTS","PRINT-COMMENTS");
        etpsMenuItem(M86,"ALLSCOPEFLAG","ALLSCOPEFLAG");
        etpsMenuItem(M86,"ATOMVALFLAG","ATOMVALFLAG");
        etpsMenuItem(M86,"BLANK-LINES-INSERTED","BLANK-LINES-INSERTED");
        etpsMenuItem(M86,"CHARSIZE","CHARSIZE");
        etpsMenuItem(M86,"DISPLAYWFF","DISPLAYWFF");
        etpsMenuItem(M86,"ELIM-DEFNS","ELIM-DEFNS");
        etpsMenuItem(M86,"FILLINEFLAG","FILLINEFLAG");
        tpsMenuItem(M86,"FIRST-PLACEMENT-PRINT-MODE","FIRST-PLACEMENT-PRINT-MODE");
        etpsMenuItem(M86,"FLUSHLEFTFLAG","FLUSHLEFTFLAG");
        etpsMenuItem(M86,"LEFTMARGIN","LEFTMARGIN");
        etpsMenuItem(M86,"LOCALLEFTFLAG","LOCALLEFTFLAG");
        etpsMenuItem(M86,"PPWFFLAG","PPWFFLAG");
        etpsMenuItem(M86,"PRINTDEPTH","PRINTDEPTH");
        etpsMenuItem(M86,"PRINTTYPES","PRINTTYPES");
        etpsMenuItem(M86,"PRINTTYPES-ALL","PRINTTYPES-ALL");
        etpsMenuItem(M86,"RETAIN-INITIAL-TYPE","RETAIN-INITIAL-TYPE");
        etpsMenuItem(M86,"SLIDES-TURNSTILE-INDENT","SLIDES-TURNSTILE-INDENT");
        etpsMenuItem(M86,"SLIDES-TURNSTYLE-INDENT","SLIDES-TURNSTYLE-INDENT");
        etpsMenuItem(M86,"SLIDES-PREAMBLE","SLIDES-PREAMBLE");
        etpsMenuItem(M86,"SUPPORT-NUMBERS","SUPPORT-NUMBERS");
        etpsMenuItem(M86,"TURNSTILE-INDENT","TURNSTILE-INDENT");
        etpsMenuItem(M86,"TURNSTILE-INDENT-AUTO","TURNSTILE-INDENT-AUTO");
        etpsMenuItem(M86,"TURNSTYLE-INDENT","TURNSTYLE-INDENT");
        etpsMenuItem(M86,"TURNSTYLE-INDENT-AUTO","TURNSTYLE-INDENT-AUTO");
        etpsMenuItem(M86,"USE-INTERNAL-PRINT-MODE","USE-INTERNAL-PRINT-MODE");
         M21.add(M99);
        if (!etps) {
         M99.add(M124);
        }
        tpsMenuItem(M124,"GO-INSTRUCTIONS","GO-INSTRUCTIONS");
        tpsMenuItem(M124,"QUIETLY-USE-DEFAULTS","QUIETLY-USE-DEFAULTS");
        tpsMenuItem(M124,"RESOLVE-CONFLICT","RESOLVE-CONFLICT");
        if (!etps) {
         M99.add(M112);
        }
        tpsMenuItem(M112,"BUILD-MATCH","BUILD-MATCH");
        etpsMenuItem(M112,"HLINE-JUSTIFICATION","HLINE-JUSTIFICATION");
        etpsMenuItem(M112,"TREAT-HLINES-AS-DLINES","TREAT-HLINES-AS-DLINES");
        if (!etps) {
         M99.add(M110);
        }
        tpsMenuItem(M110,"USE-RULEP","USE-RULEP");
        etpsMenuItem(M110,"RULEP-MAINFN","RULEP-MAINFN");
        tpsMenuItem(M110,"RULEP-WFFEQ","RULEP-WFFEQ");
         M99.add(M81);
        etpsMenuItem(M81,"AUTO-GENERATE-HYPS","AUTO-GENERATE-HYPS");
        etpsMenuItem(M81,"CLEANUP-RULEC","CLEANUP-RULEC");
        etpsMenuItem(M81,"CLEANUP-SAME","CLEANUP-SAME");
        etpsMenuItem(M81,"DEFAULT-WFFEQ","DEFAULT-WFFEQ");
        etpsMenuItem(M81,"PRINT-DOTS","PRINT-DOTS");
        etpsMenuItem(M81,"PRINTLINEFLAG","PRINTLINEFLAG");
        etpsMenuItem(M81,"SHORT-HELP","SHORT-HELP");
         M99.add(M64);
        etpsMenuItem(M64,"COMPLETION-OPTIONS","COMPLETION-OPTIONS");
        etpsMenuItem(M64,"HISTORY-SIZE","HISTORY-SIZE");
         M21.add(M87);
        etpsMenuItem(M87,"SUPPRESS-FLAGS","SUPPRESS-FLAGS");
        etpsMenuItem(M87,"SUPPRESS-FLAGS-LIST","SUPPRESS-FLAGS-LIST");
        etpsMenuItem(M87,"LAST-MODE-NAME","LAST-MODE-NAME");
        etpsMenuItem(M87,"ALPHA-LOWER-FLAG","ALPHA-LOWER-FLAG");
        etpsMenuItem(M87,"SHOW-ALL-PACKAGES","SHOW-ALL-PACKAGES");
         M87.add(M132);
        etpsMenuItem(M132,"META-BDVAR-NAME","META-BDVAR-NAME");
        etpsMenuItem(M132,"META-VAR-NAME","META-VAR-NAME");
        etpsMenuItem(M132,"REN-VAR-FN","REN-VAR-FN");
        etpsMenuItem(M132,"RENAME-ALL-BD-VARS","RENAME-ALL-BD-VARS");
         M87.add(M114);
         M87.add(M101);
        etpsMenuItem(M101,"BASE-TYPE","BASE-TYPE");
        tpsMenuItem(M101,"FIRST-PLACEMENT-MODE-PARSE","FIRST-PLACEMENT-MODE-PARSE");
        etpsMenuItem(M101,"LOWERCASERAISE","LOWERCASERAISE");
        etpsMenuItem(M101,"TYPE-IOTA-MODE","TYPE-IOTA-MODE");
        etpsMenuItem(M101,"UNTYPED-LAMBDA-CALCULUS","UNTYPED-LAMBDA-CALCULUS");
        etpsMenuItem(M101,"MAKE-WFFOPS-LABELS","MAKE-WFFOPS-LABELS");
        etpsMenuItem(M101,"PRINT-META","PRINT-META");
        if (!etps) {
         M87.add(M95);
        }
        tpsMenuItem(M95,"TRUE-NAME","TRUE-NAME");
        tpsMenuItem(M95,"REWRITE-NAME","REWRITE-NAME");
        tpsMenuItem(M95,"NEG-NAME","NEG-NAME");
        tpsMenuItem(M95,"LEAF-NAME","LEAF-NAME");
        tpsMenuItem(M95,"MATING-NAME","MATING-NAME");
        tpsMenuItem(M95,"FALSE-NAME","FALSE-NAME");
        tpsMenuItem(M95,"IMP-NAME","IMP-NAME");
        tpsMenuItem(M95,"ECONJ-NAME","ECONJ-NAME");
        tpsMenuItem(M95,"EDISJ-NAME","EDISJ-NAME");
        tpsMenuItem(M95,"EMPTY-DUP-INFO-NAME","EMPTY-DUP-INFO-NAME");
        tpsMenuItem(M95,"EPROOF-NAME","EPROOF-NAME");
        tpsMenuItem(M95,"EXPANSION-NAME","EXPANSION-NAME");
        tpsMenuItem(M95,"SELECTION-NAME","SELECTION-NAME");
        tpsMenuItem(M95,"SKOLEM-SELECTION-NAME","SKOLEM-SELECTION-NAME");
        tpsMenuItem(M95,"NAME-SKOLEM-FN","NAME-SKOLEM-FN");
        etpsMenuItem(M95,"META-LABEL-NAME","META-LABEL-NAME");
        tpsMenuItem(M95,"LIT-NAME","LIT-NAME");
        tpsMenuItem(M95,"VPD-LIT-NAME","VPD-LIT-NAME");
         M87.add(M80);
        etpsMenuItem(M80,"COMPILED-EXTENSION","COMPILED-EXTENSION");
        etpsMenuItem(M80,"EXPERTFLAG","EXPERTFLAG");
        etpsMenuItem(M80,"INIT-DIALOGUE","INIT-DIALOGUE");
        etpsMenuItem(M80,"INIT-DIALOGUE-FN","INIT-DIALOGUE-FN");
        etpsMenuItem(M80,"LISP-IMPLEMENTATION-TYPE","LISP-IMPLEMENTATION-TYPE");
        etpsMenuItem(M80,"LOAD-WARN-P","LOAD-WARN-P");
        etpsMenuItem(M80,"MACHINE-INSTANCE","MACHINE-INSTANCE");
        etpsMenuItem(M80,"MACHINE-TYPE","MACHINE-TYPE");
        etpsMenuItem(M80,"NEWS-DIR","NEWS-DIR");
        tpsMenuItem(M80,"READ-LLOAD-SOURCES-P","READ-LLOAD-SOURCES-P");
        etpsMenuItem(M80,"SAVE-FILE","SAVE-FILE");
        etpsMenuItem(M80,"SHORT-SITE-NAME","SHORT-SITE-NAME");
        etpsMenuItem(M80,"SOURCE-EXTENSION","SOURCE-EXTENSION");
        etpsMenuItem(M80,"SOURCE-PATH","SOURCE-PATH");
        tpsMenuItem(M80,"TEST-MODIFY","TEST-MODIFY");
        tpsMenuItem(M80,"TEST-THEOREMS","TEST-THEOREMS");
         M87.add(M70);
        etpsMenuItem(M70,"ADVICE-ASKED-ENABLED","ADVICE-ASKED-ENABLED");
        etpsMenuItem(M70,"ADVICE-FILE","ADVICE-FILE");
        etpsMenuItem(M70,"COMMAND-ENABLED","COMMAND-ENABLED");
        etpsMenuItem(M70,"COMMAND-FILE","COMMAND-FILE");
        etpsMenuItem(M70,"DONE-EXC-ENABLED","DONE-EXC-ENABLED");
        etpsMenuItem(M70,"ERROR-ENABLED","ERROR-ENABLED");
        etpsMenuItem(M70,"ERROR-FILE","ERROR-FILE");
        etpsMenuItem(M70,"EVENT-CYCLE","EVENT-CYCLE");
        etpsMenuItem(M70,"EVENTS-ENABLED","EVENTS-ENABLED");
        etpsMenuItem(M70,"INPUT-ERROR-ENABLED","INPUT-ERROR-ENABLED");
        etpsMenuItem(M70,"INPUT-ERROR-FILE","INPUT-ERROR-FILE");
        etpsMenuItem(M70,"PROOF-ACTION-ENABLED","PROOF-ACTION-ENABLED");
        etpsMenuItem(M70,"PROOF-FILE","PROOF-FILE");
        etpsMenuItem(M70,"QUIET-EVENTS","QUIET-EVENTS");
        etpsMenuItem(M70,"RULE-ERROR-ENABLED","RULE-ERROR-ENABLED");
        etpsMenuItem(M70,"RULE-ERROR-FILE","RULE-ERROR-FILE");
        etpsMenuItem(M70,"SCORE-FILE","SCORE-FILE");
         M87.add(M62);
        etpsMenuItem(M62,"EDPPWFFLAG","EDPPWFFLAG");
        etpsMenuItem(M62,"EDPRINTDEPTH","EDPRINTDEPTH");
        etpsMenuItem(M62,"EDWIN-CURRENT","EDWIN-CURRENT");
        etpsMenuItem(M62,"EDWIN-CURRENT-HEIGHT","EDWIN-CURRENT-HEIGHT");
        etpsMenuItem(M62,"EDWIN-CURRENT-WIDTH","EDWIN-CURRENT-WIDTH");
        etpsMenuItem(M62,"EDWIN-TOP","EDWIN-TOP");
        etpsMenuItem(M62,"EDWIN-TOP-HEIGHT","EDWIN-TOP-HEIGHT");
        etpsMenuItem(M62,"EDWIN-TOP-WIDTH","EDWIN-TOP-WIDTH");
        etpsMenuItem(M62,"EDWIN-VPFORM","EDWIN-VPFORM");
        etpsMenuItem(M62,"EDWIN-VPFORM-HEIGHT","EDWIN-VPFORM-HEIGHT");
        etpsMenuItem(M62,"EDWIN-VPFORM-WIDTH","EDWIN-VPFORM-WIDTH");
        if (!etps) {
         M21.add(M39);
        }
        tpsMenuItem(M39,"DEFAULT-BUG-DIR","DEFAULT-BUG-DIR");
        tpsMenuItem(M39,"USE-DEFAULT-BUG-DIR","USE-DEFAULT-BUG-DIR");
        tpsMenuItem(M39,"CLASS-DIRECTION","CLASS-DIRECTION");
        tpsMenuItem(M39,"CLASS-SCHEME","CLASS-SCHEME");
        tpsMenuItem(M39,"ADD-SUBDIRECTORIES","ADD-SUBDIRECTORIES");
        tpsMenuItem(M39,"BACKUP-LIB-DIR","BACKUP-LIB-DIR");
        tpsMenuItem(M39,"DEFAULT-LIB-DIR","DEFAULT-LIB-DIR");
        tpsMenuItem(M39,"DEFAULT-LIBFILE-TYPE","DEFAULT-LIBFILE-TYPE");
        tpsMenuItem(M39,"DEFAULT-LIBINDEX-TYPE","DEFAULT-LIBINDEX-TYPE");
        tpsMenuItem(M39,"LIB-BESTMODE-FILE","LIB-BESTMODE-FILE");
        tpsMenuItem(M39,"LIB-KEYWORD-FILE","LIB-KEYWORD-FILE");
        tpsMenuItem(M39,"LIB-MASTERINDEX-FILE","LIB-MASTERINDEX-FILE");
        tpsMenuItem(M39,"RECORDFLAGS","RECORDFLAGS");
        tpsMenuItem(M39,"REMOVE-TRAILING-DIR","REMOVE-TRAILING-DIR");
        tpsMenuItem(M39,"SHOW-ALL-LIBOBJECTS","SHOW-ALL-LIBOBJECTS");
        tpsMenuItem(M39,"UNIXLIB-SHOWPATH","UNIXLIB-SHOWPATH");
        mbar.add(M24);
        etpsMenuItem(M24,"SAME","SAME");
        if (!etps) {
         M24.add(M127);
        }
        tpsMenuItem(M127,"ECHO","ECHO");
        tpsMenuItem(M127,"USE-TACTIC","USE-TACTIC");
         M24.add(M125);
        etpsMenuItem(M125,"ADVICE","ADVICE");
        etpsMenuItem(M125,"CHECK-STRUCTURE","CHECK-STRUCTURE");
        tpsMenuItem(M125,"GO","GO");
        tpsMenuItem(M125,"GO2","GO2");
        tpsMenuItem(M125,"MONSTRO","MONSTRO");
        tpsMenuItem(M125,"SUGGEST","SUGGEST");
         M24.add(M123);
        etpsMenuItem(M123,"SUBSTITUTE","SUBSTITUTE");
        etpsMenuItem(M123,"TYPESUBST","TYPESUBST");
         M24.add(M122);
        etpsMenuItem(M122,"ARE-WE-USING","ARE-WE-USING");
        etpsMenuItem(M122,"COUNT-LINES","COUNT-LINES");
        etpsMenuItem(M122,"PSTATUS","PSTATUS");
        etpsMenuItem(M122,"SPONSOR","SPONSOR");
        etpsMenuItem(M122,"SUBPROOF","SUBPROOF");
        etpsMenuItem(M122,"UNSPONSOR","UNSPONSOR");
        if (!etps) {
         M24.add(M111);
        }
        tpsMenuItem(M111,"ASSEMBLE-FILE","ASSEMBLE-FILE");
        tpsMenuItem(M111,"ASSEMBLE-MOD","ASSEMBLE-MOD");
        tpsMenuItem(M111,"BUILD","BUILD");
        tpsMenuItem(M111,"WRITE-RULE","WRITE-RULE");
         M24.add(M108);
        etpsMenuItem(M108,"AB*","AB*");
        etpsMenuItem(M108,"ABE","ABE");
        etpsMenuItem(M108,"ABU","ABU");
        etpsMenuItem(M108,"UGEN","UGEN");
        etpsMenuItem(M108,"UI","UI");
        etpsMenuItem(M108,"EGEN","EGEN");
        etpsMenuItem(M108,"RULEC","RULEC");
        etpsMenuItem(M108,"RULEC1","RULEC1");
        etpsMenuItem(M108,"LET","LET");
         M24.add(M107);
        etpsMenuItem(M107,"RULEP","RULEP");
        etpsMenuItem(M107,"ABSURD","ABSURD");
         M107.add(M100);
        etpsMenuItem(M100,"ENEG","ENEG");
        etpsMenuItem(M100,"INEG","INEG");
        etpsMenuItem(M100,"NNF","NNF");
        etpsMenuItem(M100,"NNF-EXPAND","NNF-EXPAND");
        etpsMenuItem(M100,"PULLNEG","PULLNEG");
        etpsMenuItem(M100,"PUSHNEG","PUSHNEG");
         M107.add(M75);
        etpsMenuItem(M75,"INDIRECT","INDIRECT");
        etpsMenuItem(M75,"INDIRECT1","INDIRECT1");
        etpsMenuItem(M75,"INDIRECT2","INDIRECT2");
         M107.add(M74);
        etpsMenuItem(M74,"DEDUCT","DEDUCT");
        etpsMenuItem(M74,"MP","MP");
        etpsMenuItem(M74,"DISJ-IMP","DISJ-IMP");
        etpsMenuItem(M74,"DISJ-IMP-L","DISJ-IMP-L");
        etpsMenuItem(M74,"DISJ-IMP-R","DISJ-IMP-R");
         M107.add(M67);
        etpsMenuItem(M67,"EQUIV-IMPLICS","EQUIV-IMPLICS");
        etpsMenuItem(M67,"IMPLICS-EQUIV","IMPLICS-EQUIV");
        etpsMenuItem(M67,"SUBST-EQUIV","SUBST-EQUIV");
         M107.add(M61);
        etpsMenuItem(M61,"CASES","CASES");
        etpsMenuItem(M61,"CASES3","CASES3");
        etpsMenuItem(M61,"CASES4","CASES4");
        etpsMenuItem(M61,"IDISJ-LEFT","IDISJ-LEFT");
        etpsMenuItem(M61,"IDISJ-RIGHT","IDISJ-RIGHT");
        etpsMenuItem(M61,"IMP-DISJ","IMP-DISJ");
        etpsMenuItem(M61,"IMP-DISJ-L","IMP-DISJ-L");
        etpsMenuItem(M61,"IMP-DISJ-R","IMP-DISJ-R");
         M107.add(M59);
        etpsMenuItem(M59,"ECONJ","ECONJ");
        etpsMenuItem(M59,"ICONJ","ICONJ");
        etpsMenuItem(M107,"ITRUTH","ITRUTH");
        etpsMenuItem(M107,"ASSOC-LEFT","ASSOC-LEFT");
         M24.add(M104);
        etpsMenuItem(M104,"CREATE-SUBPROOF","CREATE-SUBPROOF");
        etpsMenuItem(M104,"LINE-COMMENT","LINE-COMMENT");
        etpsMenuItem(M104,"MERGE-PROOFS","MERGE-PROOFS");
        etpsMenuItem(M104,"PROOF-COMMENT","PROOF-COMMENT");
        etpsMenuItem(M104,"PROOFLIST","PROOFLIST");
        etpsMenuItem(M104,"TRANSFER-LINES","TRANSFER-LINES");
         M24.add(M102);
        etpsMenuItem(M102,"BUILD-PROOF-HIERARCHY","BUILD-PROOF-HIERARCHY");
        etpsMenuItem(M102,"DEPTH","DEPTH");
        etpsMenuItem(M102,"EXPLAIN","EXPLAIN");
        etpsMenuItem(M102,"FIND-LINE","FIND-LINE");
        etpsMenuItem(M102,"PALL (Display Proof)","PALL");
        etpsMenuItem(M102,"PBRIEF","PBRIEF");
        etpsMenuItem(M102,"PL","PL");
        etpsMenuItem(M102,"PL*","PL*");
        etpsMenuItem(M102,"PLINE","PLINE");
        etpsMenuItem(M102,"PPLAN","PPLAN");
        etpsMenuItem(M102,"PRINT-PROOF-STRUCTURE","PRINT-PROOF-STRUCTURE");
        etpsMenuItem(M102,"PRW","PRW");
        etpsMenuItem(M102,"PW","PW");
        etpsMenuItem(M102,"PWSCOPE","PWSCOPE");
        etpsMenuItem(M102,"PWTYPES","PWTYPES");
        etpsMenuItem(M102,"SHOWNOTYPES","SHOWNOTYPES");
        etpsMenuItem(M102,"SHOWTYPES","SHOWTYPES");
        etpsMenuItem(M102,"TABLEAU","TABLEAU");
        etpsMenuItem(M102,"^P","^P");
        etpsMenuItem(M102,"^PN","^PN");
        if (!etps) {
         M24.add(M96);
        }
        tpsMenuItem(M96,"NAT-ETREE","NAT-ETREE");
        tpsMenuItem(M96,"PFNAT","PFNAT");
        tpsMenuItem(M96,"PNTR","PNTR");
         M24.add(M88);
        etpsMenuItem(M88,"ASSERT","ASSERT");
        etpsMenuItem(M88,"HYP","HYP");
        etpsMenuItem(M88,"LEMMA","LEMMA");
        etpsMenuItem(M88,"ADD-HYPS","ADD-HYPS");
        etpsMenuItem(M88,"DELETE","DELETE");
        etpsMenuItem(M88,"DELETE*","DELETE*");
        etpsMenuItem(M88,"DELETE-HYPS","DELETE-HYPS");
        etpsMenuItem(M88,"INTRODUCE-GAP","INTRODUCE-GAP");
        etpsMenuItem(M88,"MAKE-ASSERT-A-HYP","MAKE-ASSERT-A-HYP");
        tpsMenuItem(M88,"ELIMINATE-ALL-RULEP-APPS","ELIMINATE-ALL-RULEP-APPS");
        tpsMenuItem(M88,"ELIMINATE-CONJ*-RULEP-APPS","ELIMINATE-CONJ*-RULEP-APPS");
        tpsMenuItem(M88,"ELIMINATE-RULEP-LINE","ELIMINATE-RULEP-LINE");
        etpsMenuItem(M88,"LOCK-LINE","LOCK-LINE");
        etpsMenuItem(M88,"MODIFY-GAPS","MODIFY-GAPS");
        etpsMenuItem(M88,"MOVE","MOVE");
        etpsMenuItem(M88,"MOVE*","MOVE*");
        etpsMenuItem(M88,"PLAN","PLAN");
        etpsMenuItem(M88,"RENUMBERALL","RENUMBERALL");
        etpsMenuItem(M88,"SQUEEZE","SQUEEZE");
        etpsMenuItem(M88,"UNLOCK-LINE","UNLOCK-LINE");
        tpsMenuItem(M88,"DEASSERT-LEMMAS","DEASSERT-LEMMAS");
        tpsMenuItem(M88,"NORMALIZE-PROOF","NORMALIZE-PROOF");
         M24.add(M77);
        etpsMenuItem(M77,"BETA*","BETA*");
        etpsMenuItem(M77,"ETA*","ETA*");
        etpsMenuItem(M77,"LAMBDA*","LAMBDA*");
        etpsMenuItem(M77,"LCONTR*","LCONTR*");
        etpsMenuItem(M77,"LCONTR*-BETA","LCONTR*-BETA");
        etpsMenuItem(M77,"LCONTR*-ETA","LCONTR*-ETA");
        etpsMenuItem(M77,"LEXPD*","LEXPD*");
        etpsMenuItem(M77,"LEXPD*-BETA","LEXPD*-BETA");
        etpsMenuItem(M77,"LEXPD*-ETA","LEXPD*-ETA");
         M24.add(M72);
        if (expert) {
            etpsMenuItem(M72,"PRINTPROOF","PRINTPROOF");
        }
        if (expert) {
            etpsMenuItem(M72,"SCRIBEPROOF","SCRIBEPROOF");
        }
        etpsMenuItem(M72,"SETUP-SLIDE-STYLE","SETUP-SLIDE-STYLE");
        if (expert) {
            etpsMenuItem(M72,"SLIDEPROOF","SLIDEPROOF");
        }
        if (expert) {
            etpsMenuItem(M72,"TEXPROOF","TEXPROOF");
        }
        if (!etps) {
         M24.add(M68);
        }
        tpsMenuItem(M68,"ETREE-NAT","ETREE-NAT");
        tpsMenuItem(M68,"TIDY-PROOF","TIDY-PROOF");
         M24.add(M66);
        etpsMenuItem(M66,"SUBST=","SUBST=");
        etpsMenuItem(M66,"SUBST=L","SUBST=L");
        etpsMenuItem(M66,"SUBST=R","SUBST=R");
        etpsMenuItem(M66,"SYM=","SYM=");
        if (!etps) {
         M66.add(M109);
        }
        tpsMenuItem(M109,"ACTIVATE-RULES","ACTIVATE-RULES");
        tpsMenuItem(M109,"DEACTIVATE-RULES","DEACTIVATE-RULES");
        tpsMenuItem(M109,"DELETE-RRULE","DELETE-RRULE");
        tpsMenuItem(M109,"LIST-RRULES","LIST-RRULES");
        tpsMenuItem(M109,"MAKE-ABBREV-RRULE","MAKE-ABBREV-RRULE");
        tpsMenuItem(M109,"MAKE-INVERSE-RRULE","MAKE-INVERSE-RRULE");
        tpsMenuItem(M109,"MAKE-THEORY","MAKE-THEORY");
        tpsMenuItem(M109,"PERMUTE-RRULES","PERMUTE-RRULES");
        tpsMenuItem(M109,"REWRITE-SUPP*","REWRITE-SUPP*");
        tpsMenuItem(M109,"REWRITE-SUPP1","REWRITE-SUPP1");
        tpsMenuItem(M109,"SIMPLIFY-PLAN","SIMPLIFY-PLAN");
        tpsMenuItem(M109,"SIMPLIFY-PLAN*","SIMPLIFY-PLAN*");
        tpsMenuItem(M109,"SIMPLIFY-SUPP","SIMPLIFY-SUPP");
        tpsMenuItem(M109,"SIMPLIFY-SUPP*","SIMPLIFY-SUPP*");
        tpsMenuItem(M109,"UNREWRITE-PLAN*","UNREWRITE-PLAN*");
        tpsMenuItem(M109,"UNREWRITE-PLAN1","UNREWRITE-PLAN1");
        tpsMenuItem(M109,"USE-RRULES","USE-RRULES");
        tpsMenuItem(M109,"USE-THEORY","USE-THEORY");
         M24.add(M63);
        etpsMenuItem(M63,"CLEANUP","CLEANUP");
        etpsMenuItem(M63,"DONE","DONE");
        etpsMenuItem(M63,"NEWS","NEWS");
        etpsMenuItem(M63,"RECONSIDER","RECONSIDER");
        etpsMenuItem(M63,"REMARK","REMARK");
        etpsMenuItem(M63,"SUMMARY","SUMMARY");
        etpsMenuItem(M63,"ALIAS","ALIAS");
        etpsMenuItem(M63,"UNALIAS","UNALIAS");
         M24.add(M60);
        etpsMenuItem(M60,"EQUIV-WFFS","EQUIV-WFFS");
        etpsMenuItem(M60,"IDEF","IDEF");
        etpsMenuItem(M60,"EDEF","EDEF");
        etpsMenuItem(M60,"EQUIV-EQ","EQUIV-EQ");
        etpsMenuItem(M60,"EQUIV-EQ-CONTR","EQUIV-EQ-CONTR");
        etpsMenuItem(M60,"EQUIV-EQ-CONTR*","EQUIV-EQ-CONTR*");
        etpsMenuItem(M60,"EQUIV-EQ-EXPD","EQUIV-EQ-EXPD");
        etpsMenuItem(M60,"EQUIV-EQ-EXPD*","EQUIV-EQ-EXPD*");
        etpsMenuItem(M60,"EXT=","EXT=");
        etpsMenuItem(M60,"EXT=0","EXT=0");
        mbar.add(M23);
        tpsMenuItem(M23,"DISPLAY-TIME","DISPLAY-TIME");
        if (!etps) {
         M23.add(M131);
        }
        tpsMenuItem(M131,"LOADED-MODS","LOADED-MODS");
        tpsMenuItem(M131,"MODULES","MODULES");
        tpsMenuItem(M131,"UNLOADED-MODS","UNLOADED-MODS");
         M23.add(M130);
        etpsMenuItem(M130,"CLOAD","CLOAD");
        etpsMenuItem(M130,"CLOAD-MODULES","CLOAD-MODULES");
        tpsMenuItem(M130,"COMPILE-LIST","COMPILE-LIST");
        tpsMenuItem(M130,"COMPL","COMPL");
        etpsMenuItem(M130,"FILETYPE","FILETYPE");
        tpsMenuItem(M130,"LEDIT","LEDIT");
        tpsMenuItem(M130,"LOAD-SLOW","LOAD-SLOW");
        tpsMenuItem(M130,"ORGANIZE","ORGANIZE");
        etpsMenuItem(M130,"QLOAD","QLOAD");
        etpsMenuItem(M130,"SYS-LOAD","SYS-LOAD");
        if (expert) {
            tpsMenuItem(M130,"TEST-INIT","TEST-INIT");
        }
        tpsMenuItem(M130,"TLIST","TLIST");
        etpsMenuItem(M130,"TLOAD","TLOAD");
        if (expert) {
            tpsMenuItem(M130,"TPS-TEST","TPS-TEST");
        }
        if (expert) {
            tpsMenuItem(M130,"TPS-TEST2","TPS-TEST2");
        }
        if (expert) {
            etpsMenuItem(M130,"TPS3-SAVE","TPS3-SAVE");
        }
        if (!etps) {
         M23.add(M79);
        }
        tpsMenuItem(M79,"PACK-STAT","PACK-STAT");
        etpsMenuItem(M79,"UNUSE","UNUSE");
        tpsMenuItem(M79,"USE","USE");
        etpsMenuItem(M23,"DISABLE-EVENTS","DISABLE-EVENTS");
        tpsMenuItem(M23,"LEAST-SEARCH-DEPTH","LEAST-SEARCH-DEPTH");
        if (expert) {
            if (!etps) {
         M23.add(M57);
        }
            tpsMenuItem(M57,"HTML-DOC","HTML-DOC");
            etpsMenuItem(M57,"GENERATE-JAVA-MENUS","GENERATE-JAVA-MENUS");
        }
        if (!etps) {
         M23.add(M118);
        }
        tpsMenuItem(M118,"PSEQ","PSEQ");
        tpsMenuItem(M118,"PSEQL","PSEQL");
        tpsMenuItem(M118,"SEQ-TO-NAT","SEQ-TO-NAT");
        tpsMenuItem(M118,"SEQLIST","SEQLIST");
        if (!etps) {
         M23.add(M117);
        }
        tpsMenuItem(M117,"AUTO-SUGGEST","AUTO-SUGGEST");
        tpsMenuItem(M117,"SET-BACKGROUND-EPROOF","SET-BACKGROUND-EPROOF");
        tpsMenuItem(M117,"ETR-AUTO-SUGGEST","ETR-AUTO-SUGGEST");
         M23.add(M113);
        etpsMenuItem(M113,"SAVE-INTERVAL","SAVE-INTERVAL");
        etpsMenuItem(M113,"APPEND-WFF","APPEND-WFF");
        if (expert) {
            etpsMenuItem(M113,"SAVE-WORK-ON-START-UP","SAVE-WORK-ON-START-UP");
        }
        etpsMenuItem(M113,"APPEND-WFFS","APPEND-WFFS");
        etpsMenuItem(M113,"SAVE-WORK-P","SAVE-WORK-P");
        etpsMenuItem(M113,"EXECUTE-FILE","EXECUTE-FILE");
        etpsMenuItem(M113,"FINDPROOF","FINDPROOF");
        if (expert) {
            etpsMenuItem(M113,"FINISH-SAVE","FINISH-SAVE");
        }
        etpsMenuItem(M113,"PAUSE","PAUSE");
        etpsMenuItem(M113,"RESTORE-WORK","RESTORE-WORK");
        if (expert) {
            etpsMenuItem(M113,"RESUME-SAVE","RESUME-SAVE");
        }
        if (expert) {
            etpsMenuItem(M113,"SAVE-FLAGS-AND-WORK","SAVE-FLAGS-AND-WORK");
        }
        if (expert) {
            etpsMenuItem(M113,"SAVE-SUBPROOF","SAVE-SUBPROOF");
        }
        if (expert) {
            etpsMenuItem(M113,"SAVE-WORK","SAVE-WORK");
        }
        if (expert) {
            etpsMenuItem(M113,"SAVEPROOF","SAVEPROOF");
        }
        if (expert) {
            etpsMenuItem(M113,"SCRIPT","SCRIPT");
        }
        if (expert) {
            etpsMenuItem(M113,"STOP-SAVE","STOP-SAVE");
        }
        if (expert) {
            etpsMenuItem(M113,"UNSCRIPT","UNSCRIPT");
        }
        if (!etps) {
         M23.add(M92);
        }
        tpsMenuItem(M92,"SEARCH-PLACEMENT","SEARCH-PLACEMENT");
        if (!etps) {
         M23.add(M82);
        }
        tpsMenuItem(M82,"CLOSE-TESTWIN","CLOSE-TESTWIN");
        tpsMenuItem(M82,"DIY","DIY");
        tpsMenuItem(M82,"DIY-L","DIY-L");
        tpsMenuItem(M82,"EPROOFLIST","EPROOFLIST");
        tpsMenuItem(M82,"MONITOR","MONITOR");
        tpsMenuItem(M82,"MONITORLIST","MONITORLIST");
        tpsMenuItem(M82,"NOMONITOR","NOMONITOR");
        tpsMenuItem(M82,"SET-EPROOF","SET-EPROOF");
         M23.add(M73);
        etpsMenuItem(M73,"?","?");
        etpsMenuItem(M73,"??","??");
        etpsMenuItem(M73,"ABBREVIATIONS","ABBREVIATIONS");
        etpsMenuItem(M73,"ENVIRONMENT","ENVIRONMENT");
        etpsMenuItem(M73,"HELP","HELP");
        etpsMenuItem(M73,"HELP*","HELP*");
        etpsMenuItem(M73,"HELP-GROUP","HELP-GROUP");
        etpsMenuItem(M73,"LIST-RULES","LIST-RULES");
        etpsMenuItem(M73,"LIST-RULES*","LIST-RULES*");
        etpsMenuItem(M73,"OOPS","OOPS");
        etpsMenuItem(M73,"PROBLEMS","PROBLEMS");
        etpsMenuItem(M73,"SEARCH","SEARCH");
        if (!etps) {
         M23.add(M58);
        }
        tpsMenuItem(M58,"CHARDOC","CHARDOC");
        tpsMenuItem(M58,"COLLECT-HELP","COLLECT-HELP");
        tpsMenuItem(M58,"HELP-LIST","HELP-LIST");
        tpsMenuItem(M58,"QUICK-REF","QUICK-REF");
        tpsMenuItem(M58,"SCRIBE-DOC","SCRIBE-DOC");
        mbar.add(M25);
        tpsMenuItem(M25,"MATE","MATE",109);
        if (!etps) {
         M25.add(M78);
        }
        tpsMenuItem(M78,"LIB","LIB",108);
        tpsMenuItem(M78,"LIB","UNIXLIB");
        tpsMenuItem(M25,"UNIFY","UNIFY",117);
        tpsMenuItem(M25,"MTREE","MTREE");
        etpsMenuItem(M25,"ED","ED",101);
        etpsMenuItem(M25,"REVIEW","REVIEW",114);
        M25.add(new MenuItem("-"));
        etpsMenuItem(M25,"BEGIN-PRFW","BEGIN-PRFW");
        etpsMenuItem(M25,"END-PRFW","END-PRFW");
        M25.add(new MenuItem("-"));
        tpsMenuItem(M25,"OPEN-MATEVPW","OPEN-MATEVPW \"VPWIN\"");
        tpsMenuItem(M25,"CLOSE-MATEVPW","CLOSE-MATEVPW");
        M25.add(new MenuItem("-"));
        tpsMenuItem(M25,"TEST","TEST",116);
        tpsMenuItem(M25,"UNIFORM-SEARCH","UNIFORM-SEARCH");
        tpsMenuItem(M25,"UNIFORM-SEARCH-L","UNIFORM-SEARCH-L");
        M25.add(new MenuItem("-"));
        etpsMenuItem(M25,"PUSH","PUSH");
        etpsMenuItem(M25,"POP","POP");
        tpsMenuItem(M2,"SAVE","SAVE");
        if (!etps) {
         M2.add(M54);
        }
        tpsMenuItem(M54,"CW","CW");
        etpsMenuItem(M54,"DELWEAK","DELWEAK");
        tpsMenuItem(M54,"DW","DW");
        tpsMenuItem(M54,"DW*","DW*");
        etpsMenuItem(M54,"NAME","NAME");
        tpsMenuItem(M54,"RW","RW");
        if (!etps) {
         M2.add(M43);
        }
        tpsMenuItem(M43,"P","P");
        tpsMenuItem(M43,"PP","PP");
        tpsMenuItem(M43,"PS","PS");
        tpsMenuItem(M43,"PT","PT");
         M2.add(M29);
        etpsMenuItem(M29,"O","O");
        tpsMenuItem(M29,"REM","REM");
        if (!etps) {
         M2.add(M28);
        }
        tpsMenuItem(M28,"CJFORM","CJFORM");
        tpsMenuItem(M28,"DJFORM","DJFORM");
        tpsMenuItem(M28,"NUM-HPATHS","NUM-HPATHS");
        tpsMenuItem(M28,"NUM-VPATHS","NUM-VPATHS");
        tpsMenuItem(M28,"PJ","PJ");
        tpsMenuItem(M28,"PROP-CJFORM","PROP-CJFORM");
        tpsMenuItem(M28,"VP","VP");
        tpsMenuItem(M28,"VPD","VPD");
        tpsMenuItem(M28,"VPF","VPF");
        tpsMenuItem(M28,"VPT","VPT");
        M2.add(new MenuItem("-"));
        etpsMenuItem(M2,"NOOP","NOOP");
        M2.add(new MenuItem("-"));
        etpsMenuItem(M2,"OK","OK");
        M2.add(new MenuItem("-"));
        etpsMenuItem(M2,"LEAVE","LEAVE");
        etpsMenuItem(M1,"|0| (Up One Level)","|0|");
        etpsMenuItem(M1,"A","A");
        etpsMenuItem(M1,"D","D");
        etpsMenuItem(M1,"FB (First Binder)","FB");
        etpsMenuItem(M1,"FI (First Infix)","FI");
        etpsMenuItem(M1,"L (Left)","L");
        etpsMenuItem(M1,"R (Right)","R");
        etpsMenuItem(M1,"UNDO","UNDO");
        etpsMenuItem(M1,"XTR","XTR");
        etpsMenuItem(M1,"^ (Goto Top)","^");
        etpsMenuItem(M0,"ASRB","ASRB");
        etpsMenuItem(M0,"ASSL","ASSL");
        etpsMenuItem(M0,"ASSR","ASSR");
        etpsMenuItem(M0,"CMRG","CMRG");
        etpsMenuItem(M0,"CMUT","CMUT");
        etpsMenuItem(M0,"CNTOP","CNTOP");
        etpsMenuItem(M0,"DIST-CTR","DIST-CTR");
        etpsMenuItem(M0,"DIST-EXP","DIST-EXP");
        tpsMenuItem(M0,"DL","DL");
        etpsMenuItem(M0,"DNEG","DNEG");
        tpsMenuItem(M0,"DR","DR");
        etpsMenuItem(M0,"MRG","MRG");
        etpsMenuItem(M0,"PMUT","PMUT");
        etpsMenuItem(M0,"SUBEQ","SUBEQ");
        etpsMenuItem(M0,"SUBIM","SUBIM");
         M0.add(M44);
        etpsMenuItem(M44,"ASRB*","ASRB*");
        etpsMenuItem(M44,"ASSL*","ASSL*");
        etpsMenuItem(M44,"ASSR*","ASSR*");
        etpsMenuItem(M44,"CMRG*","CMRG*");
        etpsMenuItem(M44,"CMUT*","CMUT*");
        etpsMenuItem(M44,"DIST-CTR*","DIST-CTR*");
        etpsMenuItem(M44,"DIST-EXP*","DIST-EXP*");
        etpsMenuItem(M44,"DNEG*","DNEG*");
        etpsMenuItem(M44,"MRG*","MRG*");
        etpsMenuItem(M44,"PMUT*","PMUT*");
        etpsMenuItem(M44,"SUBEQ*","SUBEQ*");
        etpsMenuItem(M44,"SUBIM*","SUBIM*");
         M0.add(M30);
        etpsMenuItem(M30,"MBED-AL","MBED-AL");
        etpsMenuItem(M30,"MBED-AR","MBED-AR");
        etpsMenuItem(M30,"MBED-E","MBED-E");
        etpsMenuItem(M30,"MBED-E1","MBED-E1");
        etpsMenuItem(M30,"MBED-F","MBED-F");
        etpsMenuItem(M30,"MBED-IL","MBED-IL");
        etpsMenuItem(M30,"MBED-IR","MBED-IR");
        etpsMenuItem(M30,"MBED-L","MBED-L");
        etpsMenuItem(M30,"MBED-OL","MBED-OL");
        etpsMenuItem(M30,"MBED-OR","MBED-OR");
        etpsMenuItem(M30,"MBED-QL","MBED-QL");
        etpsMenuItem(M30,"MBED-QR","MBED-QR");
        etpsMenuItem(M30,"MBED=L","MBED=L");
        etpsMenuItem(M30,"MBED=R","MBED=R");
        if (!etps) {
         M30.add(M50);
        }
        tpsMenuItem(M50,"AB","AB");
        tpsMenuItem(M50,"IB","IB");
        etpsMenuItem(M50,"PRIM-SUBST","PRIM-SUBST");
        tpsMenuItem(M50,"REW-EQUIV","REW-EQUIV");
        tpsMenuItem(M50,"RP","RP");
        tpsMenuItem(M50,"RPALL","RPALL");
        etpsMenuItem(M50,"SUB","SUB");
        tpsMenuItem(M50,"SUBST","SUBST");
        etpsMenuItem(M50,"SUBSTYP","SUBSTYP");
        if (!etps) {
         M30.add(M48);
        }
        tpsMenuItem(M48,"SK1","SK1");
        tpsMenuItem(M48,"SK3","SK3");
        if (!etps) {
         M30.add(M46);
        }
        tpsMenuItem(M46,"ARR","ARR");
        tpsMenuItem(M46,"ARR*","ARR*");
        tpsMenuItem(M46,"ARR1","ARR1");
        tpsMenuItem(M46,"ARR1*","ARR1*");
        tpsMenuItem(M46,"MAKE-RRULE","MAKE-RRULE");
        tpsMenuItem(M46,"UNARR","UNARR");
        tpsMenuItem(M46,"UNARR*","UNARR*");
        tpsMenuItem(M46,"UNARR1","UNARR1");
        tpsMenuItem(M46,"UNARR1*","UNARR1*");
        if (!etps) {
         M30.add(M42);
        }
        tpsMenuItem(M42,"NAME-PRIM","NAME-PRIM");
        tpsMenuItem(M42,"PRT-PRIM","PRT-PRIM");
        if (!etps) {
         M30.add(M41);
        }
        tpsMenuItem(M41,"NEG","NEG");
        etpsMenuItem(M41,"NNF","NNF");
        tpsMenuItem(M41,"PULL-NEG","PULL-NEG");
        tpsMenuItem(M41,"PUSH-NEG","PUSH-NEG");
        if (!etps) {
         M30.add(M40);
        }
        tpsMenuItem(M40,"CLAUSE-FORM","CLAUSE-FORM");
        etpsMenuItem(M40,"CNF","CNF");
        etpsMenuItem(M40,"HEAD","HEAD");
        etpsMenuItem(M40,"HVARS","HVARS");
        tpsMenuItem(M40,"MIN-SCOPE","MIN-SCOPE");
        tpsMenuItem(M40,"SUBFORMULAS","SUBFORMULAS");
        if (!etps) {
         M30.add(M34);
        }
        tpsMenuItem(M34,"ABNORM","ABNORM");
        tpsMenuItem(M34,"ETAB","ETAB");
        tpsMenuItem(M34,"ETAC","ETAC");
        tpsMenuItem(M34,"ETAN","ETAN");
        tpsMenuItem(M34,"ETAX","ETAX");
        tpsMenuItem(M34,"LETA","LETA");
        tpsMenuItem(M34,"LEXP","LEXP");
        etpsMenuItem(M34,"LNORM","LNORM");
        etpsMenuItem(M34,"LNORM-BETA","LNORM-BETA");
        etpsMenuItem(M34,"LNORM-ETA","LNORM-ETA");
        tpsMenuItem(M34,"RED","RED");
        tpsMenuItem(M34,"ULNORM","ULNORM");
        if (!etps) {
         M30.add(M32);
        }
        tpsMenuItem(M32,"DB","DB");
        tpsMenuItem(M32,"EP","EP");
        tpsMenuItem(M32,"OP","OP");
        if (!etps) {
         M30.add(M31);
        }
        tpsMenuItem(M31,"DUPW","DUPW");
        etpsMenuItem(M31,"EDILL","EDILL");
        tpsMenuItem(M31,"ILL","ILL");
        tpsMenuItem(M31,"TP","TP");
        tpsMenuItem(M31,"WFFP","WFFP");
        if (!etps) {
         M30.add(M26);
        }
        tpsMenuItem(M26,"ABBR","ABBR");
        tpsMenuItem(M26,"CONSTANTS","CONSTANTS");
        tpsMenuItem(M26,"EXPAND=","EXPAND=");
        tpsMenuItem(M26,"EXPAND=*","EXPAND=*");
        tpsMenuItem(M26,"INST","INST");
        tpsMenuItem(M26,"INST1","INST1");
        tpsMenuItem(M26,"INSTALL","INSTALL");
        tpsMenuItem(M26,"LIB-ABBR","LIB-ABBR");
        etpsMenuItem(M26,"NEW-DEFS","NEW-DEFS");
        if (expert) {
            if (!etps) {
         M5.add(M55);
        }
            tpsMenuItem(M55,"CHANGE-PROVABILITY","CHANGE-PROVABILITY");
            tpsMenuItem(M55,"COPY-LIBOBJECT","COPY-LIBOBJECT");
            tpsMenuItem(M55,"DELETE","DELETE");
            tpsMenuItem(M55,"FIX-MODES","FIX-MODES");
            tpsMenuItem(M55,"IMPORT-NEEDED-OBJECTS","IMPORT-NEEDED-OBJECTS");
            tpsMenuItem(M55,"INSERT","INSERT");
            tpsMenuItem(M55,"MOVE-LIBOBJECT","MOVE-LIBOBJECT");
            tpsMenuItem(M55,"REFORMAT","REFORMAT");
            tpsMenuItem(M55,"REINDEX","REINDEX");
            tpsMenuItem(M55,"RENAME-OBJECT","RENAME-OBJECT");
            tpsMenuItem(M55,"SORT","SORT");
            tpsMenuItem(M55,"SPRING-CLEAN","SPRING-CLEAN");
        }
        if (!etps) {
         M5.add(M49);
        }
        if (expert) {
            tpsMenuItem(M49,"COPY-LIBDIR","COPY-LIBDIR");
        }
        if (expert) {
            tpsMenuItem(M49,"COPY-LIBFILE","COPY-LIBFILE");
        }
        if (expert) {
            tpsMenuItem(M49,"CREATE-LIB-DIR","CREATE-LIB-DIR");
        }
        if (expert) {
            tpsMenuItem(M49,"CREATE-LIB-SUBDIR","CREATE-LIB-SUBDIR");
        }
        if (expert) {
            tpsMenuItem(M49,"DELETE-LIB-DIR","DELETE-LIB-DIR");
        }
        if (expert) {
            tpsMenuItem(M49,"DELETE-LIBFILE","DELETE-LIBFILE");
        }
        if (expert) {
            tpsMenuItem(M49,"MOVE-LIBFILE","MOVE-LIBFILE");
        }
        if (expert) {
            tpsMenuItem(M49,"RENAME-LIBDIR","RENAME-LIBDIR");
        }
        if (expert) {
            tpsMenuItem(M49,"RENAME-LIBFILE","RENAME-LIBFILE");
        }
        if (expert) {
            tpsMenuItem(M49,"UPDATE-LIBDIR","UPDATE-LIBDIR");
        }
        if (!etps) {
         M5.add(M38);
        }
        tpsMenuItem(M38,"DESTROY","DESTROY");
        tpsMenuItem(M38,"FETCH","FETCH");
        tpsMenuItem(M38,"FIND-PROVABLE","FIND-PROVABLE");
        tpsMenuItem(M38,"RESTORE-MASTERINDEX","RESTORE-MASTERINDEX");
        tpsMenuItem(M38,"RETRIEVE-FILE","RETRIEVE-FILE");
        if (!etps) {
         M5.add(M37);
        }
        tpsMenuItem(M37,"KEY","KEY");
        tpsMenuItem(M37,"LIBFILES","LIBFILES");
        tpsMenuItem(M37,"LIBOBJECTS-IN-FILE","LIBOBJECTS-IN-FILE");
        tpsMenuItem(M37,"LIST-OF-LIBOBJECTS","LIST-OF-LIBOBJECTS");
        tpsMenuItem(M37,"SCRIBE-ALL-WFFS","SCRIBE-ALL-WFFS");
        if (expert) {
            tpsMenuItem(M37,"SCRIBELIBDIR","SCRIBELIBDIR");
        }
        if (expert) {
            tpsMenuItem(M37,"SCRIBELIBFILE","SCRIBELIBFILE");
        }
        tpsMenuItem(M37,"SEARCH","SEARCH");
        tpsMenuItem(M37,"SEARCH2","SEARCH2");
        tpsMenuItem(M37,"SHOW","SHOW");
        tpsMenuItem(M37,"SHOW*-WFF","SHOW*-WFF");
        tpsMenuItem(M37,"SHOW-ALL-WFFS","SHOW-ALL-WFFS");
        tpsMenuItem(M37,"SHOW-HELP","SHOW-HELP");
        tpsMenuItem(M37,"SHOW-OBJECTS-IN-FILE","SHOW-OBJECTS-IN-FILE");
        tpsMenuItem(M37,"SHOW-TIMING","SHOW-TIMING");
        tpsMenuItem(M37,"SHOW-WFF","SHOW-WFF");
        tpsMenuItem(M37,"SHOW-WFF&HELP","SHOW-WFF&HELP");
        tpsMenuItem(M37,"SHOW-WFFS-IN-FILE","SHOW-WFFS-IN-FILE");
        if (expert) {
            tpsMenuItem(M37,"TEX-ALL-WFFS","TEX-ALL-WFFS");
        }
        if (expert) {
            tpsMenuItem(M37,"TEXLIBDIR","TEXLIBDIR");
        }
        if (expert) {
            tpsMenuItem(M37,"TEXLIBFILE","TEXLIBFILE");
        }
        if (!etps) {
         M5.add(M36);
        }
        tpsMenuItem(M36,"CLASSIFY-CLASS","CLASSIFY-CLASS");
        tpsMenuItem(M36,"CLASSIFY-ITEM","CLASSIFY-ITEM");
        tpsMenuItem(M36,"CREATE-CLASS-SCHEME","CREATE-CLASS-SCHEME");
        tpsMenuItem(M36,"CREATE-LIBCLASS","CREATE-LIBCLASS");
        tpsMenuItem(M36,"FETCH-DOWN","FETCH-DOWN");
        tpsMenuItem(M36,"FETCH-LIBCLASS","FETCH-LIBCLASS");
        tpsMenuItem(M36,"FETCH-LIBCLASS*","FETCH-LIBCLASS*");
        tpsMenuItem(M36,"FETCH-UP","FETCH-UP");
        tpsMenuItem(M36,"GOTO-CLASS","GOTO-CLASS");
        tpsMenuItem(M36,"PCLASS","PCLASS");
        tpsMenuItem(M36,"PCLASS-SCHEME-TREE","PCLASS-SCHEME-TREE");
        tpsMenuItem(M36,"PCLASS-TREE","PCLASS-TREE");
        tpsMenuItem(M36,"PINTERSECT","PINTERSECT");
        tpsMenuItem(M36,"PINTERSECT*","PINTERSECT*");
        tpsMenuItem(M36,"PSCHEMES","PSCHEMES");
        tpsMenuItem(M36,"ROOT-CLASS","ROOT-CLASS");
        tpsMenuItem(M36,"UNCLASSIFY-CLASS","UNCLASSIFY-CLASS");
        tpsMenuItem(M36,"UNCLASSIFY-ITEM","UNCLASSIFY-ITEM");
        if (!etps) {
         M5.add(M33);
        }
        if (expert) {
            tpsMenuItem(M33,"ADD-KEYWORD","ADD-KEYWORD");
        }
        if (expert) {
            tpsMenuItem(M33,"CHANGE-KEYWORDS","CHANGE-KEYWORDS");
        }
        tpsMenuItem(M33,"SHOW-KEYWORDS","SHOW-KEYWORDS");
        if (expert) {
            tpsMenuItem(M33,"UPDATE-KEYWORDS","UPDATE-KEYWORDS");
        }
        if (!etps) {
         M5.add(M27);
        }
        tpsMenuItem(M27,"SHOW-BESTMODE-THMS","SHOW-BESTMODE-THMS");
        if (expert) {
            tpsMenuItem(M27,"ADD-BESTMODE","ADD-BESTMODE");
        }
        if (expert) {
            tpsMenuItem(M27,"DELETE-BESTMODE","DELETE-BESTMODE");
        }
        tpsMenuItem(M27,"FIND-DUP-MODES","FIND-DUP-MODES");
        if (expert) {
            tpsMenuItem(M27,"MODIFY-BESTMODE","MODIFY-BESTMODE");
        }
        tpsMenuItem(M27,"SHOW-BESTMODE","SHOW-BESTMODE");
        tpsMenuItem(M27,"SHOW-NEW-BESTMODES","SHOW-NEW-BESTMODES");
        if (expert) {
            tpsMenuItem(M27,"UPDATE-PROVABILITY","UPDATE-PROVABILITY");
        }
        tpsMenuItem(M5,"CHECK-NEEDED-OBJECTS","CHECK-NEEDED-OBJECTS");
        M5.add(new MenuItem("-"));
        tpsMenuItem(M5,"Leave","Leave");
        tpsMenuItem(M11,"GO","GO");
        tpsMenuItem(M11,"ADD-ALL-LIT","ADD-ALL-LIT");
        tpsMenuItem(M11,"ADD-ALL-OB","ADD-ALL-OB");
        tpsMenuItem(M11,"EXPAND-LEAVES","EXPAND-LEAVES");
        tpsMenuItem(M11,"MT94-11","MT94-11");
        tpsMenuItem(M11,"MT94-12","MT94-12");
        tpsMenuItem(M11,"MT95-1","MT95-1");
        tpsMenuItem(M11,"QRY","QRY");
        M11.add(new MenuItem("-"));
        tpsMenuItem(M11,"LEAVE","LEAVE");
        tpsMenuItem(M12,"ADD-CONN","ADD-CONN");
        tpsMenuItem(M12,"CHOOSE-BRANCH","CHOOSE-BRANCH");
        tpsMenuItem(M12,"COMPLETE-P","COMPLETE-P");
        tpsMenuItem(M12,"D","D");
        tpsMenuItem(M12,"GOTO","GOTO");
        tpsMenuItem(M12,"INIT","INIT");
        tpsMenuItem(M12,"KILL","KILL");
        tpsMenuItem(M12,"PICK","PICK");
        tpsMenuItem(M12,"PRUNE","PRUNE");
        tpsMenuItem(M12,"REM-NODE","REM-NODE");
        tpsMenuItem(M12,"RESURRECT","RESURRECT");
        tpsMenuItem(M12,"SHOW-MATING","SHOW-MATING");
        tpsMenuItem(M12,"SHOW-SUBSTS","SHOW-SUBSTS");
        tpsMenuItem(M12,"SIB","SIB");
        tpsMenuItem(M12,"UNIFY","UNIFY");
        tpsMenuItem(M12,"UP","UP");
        tpsMenuItem(M13,"CONNS-ADDED","CONNS-ADDED");
        tpsMenuItem(M13,"LIVE-LEAVES","LIVE-LEAVES");
        tpsMenuItem(M13,"PM-NODE","PM-NODE");
        tpsMenuItem(M13,"PMTR","PMTR");
        tpsMenuItem(M13,"PMTR*","PMTR*");
        tpsMenuItem(M13,"PMTR-FLAT","PMTR-FLAT");
        tpsMenuItem(M13,"POB","POB");
        tpsMenuItem(M13,"POB-LITS","POB-LITS");
        tpsMenuItem(M13,"POB-NODE","POB-NODE");
        tpsMenuItem(M13,"POTR","POTR");
        tpsMenuItem(M13,"POTR*-FLAT","POTR*-FLAT");
        tpsMenuItem(M13,"POTR-FLAT","POTR-FLAT");
        tpsMenuItem(M13,"PPATH","PPATH");
        tpsMenuItem(M13,"PPATH*","PPATH*");
        etpsMenuItem(M14,"CHANGED-FLAGS","CHANGED-FLAGS");
        etpsMenuItem(M14,"DESCRIBE","DESCRIBE");
        etpsMenuItem(M14,"DESCRIBE*","DESCRIBE*");
        etpsMenuItem(M14,"KEY","KEY");
        etpsMenuItem(M14,"LIST","LIST");
        etpsMenuItem(M14,"SET","SET");
        etpsMenuItem(M14,"SETFLAG","SETFLAG");
        etpsMenuItem(M14,"SETFLAGS1","SETFLAGS1");
        etpsMenuItem(M14,"SETFLAGS2","SETFLAGS2");
        etpsMenuItem(M14,"SUBJECTS","SUBJECTS");
        etpsMenuItem(M14,"UPDATE","UPDATE");
        etpsMenuItem(M14,"UPDATE-RELEVANT","UPDATE-RELEVANT");
        if (expert) {
            etpsMenuItem(M14,"SAVE-FLAG-RELEVANCY-INFO","SAVE-FLAG-RELEVANCY-INFO");
        }
        if (expert) {
            etpsMenuItem(M14,"SHOW-RELEVANCE-PATHS","SHOW-RELEVANCE-PATHS");
        }
        if (!etps) {
         M14.add(M45);
        }
        tpsMenuItem(M45,"UNIF-DEPTHS","UNIF-DEPTHS");
        tpsMenuItem(M45,"UNIF-NODEPTHS","UNIF-NODEPTHS");
        M14.add(new MenuItem("-"));
        etpsMenuItem(M14,"LEAVE","LEAVE");
        tpsMenuItem(M9,"FIND-MODE","FIND-MODE");
        etpsMenuItem(M9,"ADD-FLAG-TO-MODE","ADD-FLAG-TO-MODE");
        etpsMenuItem(M9,"COMPARE-MODES","COMPARE-MODES");
        etpsMenuItem(M9,"COPY-MODE","COPY-MODE");
        etpsMenuItem(M9,"MODE","MODE");
        etpsMenuItem(M9,"REMOVE-FLAG-FROM-MODE","REMOVE-FLAG-FROM-MODE");
        tpsMenuItem(M16,"BREADTH-FIRST-SEARCH","BREADTH-FIRST-SEARCH");
        tpsMenuItem(M16,"CLOSE-TESTWIN","CLOSE-TESTWIN");
        tpsMenuItem(M16,"CONTINUE","CONTINUE");
        tpsMenuItem(M16,"EXHAUSTIVE-SEARCH","EXHAUSTIVE-SEARCH");
        tpsMenuItem(M16,"FIND-BEST-MODE","FIND-BEST-MODE");
        tpsMenuItem(M16,"GO","GO");
        tpsMenuItem(M16,"OPEN-TESTWIN","OPEN-TESTWIN");
        tpsMenuItem(M16,"PRESS-DOWN","PRESS-DOWN");
        tpsMenuItem(M16,"PRESS-DOWN-2","PRESS-DOWN-2");
        tpsMenuItem(M16,"PUSH-UP","PUSH-UP");
        tpsMenuItem(M16,"PUSH-UP-2","PUSH-UP-2");
        tpsMenuItem(M16,"SEARCH-PLACEMENT","SEARCH-PLACEMENT");
        M16.add(new MenuItem("-"));
        tpsMenuItem(M16,"LEAVE","LEAVE");
        tpsMenuItem(M15,"ADD-FLAG","ADD-FLAG");
        tpsMenuItem(M15,"ADD-FLAG*","ADD-FLAG*");
        tpsMenuItem(M15,"ADD-FUNCTION","ADD-FUNCTION");
        tpsMenuItem(M15,"ADD-SUBJECTS","ADD-SUBJECTS");
        tpsMenuItem(M15,"NEW-SEARCHLIST","NEW-SEARCHLIST");
        tpsMenuItem(M15,"QUICK-DEFINE","QUICK-DEFINE");
        tpsMenuItem(M15,"REM-FLAG","REM-FLAG");
        tpsMenuItem(M15,"REM-FLAG*","REM-FLAG*");
        tpsMenuItem(M15,"REVISE-DEFAULTS","REVISE-DEFAULTS");
        tpsMenuItem(M15,"SCALE-DOWN","SCALE-DOWN");
        tpsMenuItem(M15,"SCALE-UP","SCALE-UP");
        tpsMenuItem(M15,"SEARCHLISTS","SEARCHLISTS");
        tpsMenuItem(M15,"SHOW-SEARCHLIST","SHOW-SEARCHLIST");
        tpsMenuItem(M15,"VARY-MODE","VARY-MODE");
        tpsMenuItem(M17,"DELETE","DELETE");
        tpsMenuItem(M17,"FETCH","FETCH");
        tpsMenuItem(M17,"INSERT","INSERT");
        if (!etps) {
         M18.add(M53);
        }
        tpsMenuItem(M53,"DESTROY","DESTROY");
        tpsMenuItem(M53,"FETCH","FETCH");
        if (!etps) {
         M18.add(M52);
        }
        tpsMenuItem(M52,"LS","LS");
        tpsMenuItem(M52,"LS-ITEMS*","LS-ITEMS*");
        tpsMenuItem(M52,"PINTERSECT","PINTERSECT");
        tpsMenuItem(M52,"PINTERSECT*","PINTERSECT*");
        tpsMenuItem(M52,"SHOW","SHOW");
        tpsMenuItem(M52,"SHOW-ALL-WFFS","SHOW-ALL-WFFS");
        tpsMenuItem(M52,"SHOW-HELP","SHOW-HELP");
        tpsMenuItem(M52,"SHOW-WFF","SHOW-WFF");
        tpsMenuItem(M52,"SHOW-WFF&HELP","SHOW-WFF&HELP");
        tpsMenuItem(M52,"PUP","PUP");
        tpsMenuItem(M52,"PDOWN","PDOWN");
        if (!etps) {
         M18.add(M51);
        }
        tpsMenuItem(M51,"CD","CD");
        tpsMenuItem(M51,"CLASSIFY-ITEM","CLASSIFY-ITEM");
        tpsMenuItem(M51,"CP","CP");
        tpsMenuItem(M51,"MV","MV");
        tpsMenuItem(M51,"RENAME-CLASS","RENAME-CLASS");
        tpsMenuItem(M51,"COPY-CLASS-SCHEME","COPY-CLASS-SCHEME");
        tpsMenuItem(M51,"LN","LN");
        tpsMenuItem(M51,"MKDIR","MKDIR");
        tpsMenuItem(M51,"PWD","PWD");
        tpsMenuItem(M51,"RM","RM");
        if (!etps) {
         M18.add(M133);
        }
        tpsMenuItem(M133,"LOCATE","NIL");
        M18.add(new MenuItem("-"));
        tpsMenuItem(M18,"Leave","Leave");
        tpsMenuItem(M6,"ETR-INFO","ETR-INFO");
        tpsMenuItem(M6,"STATS","STATS");
        tpsMenuItem(M6,"EXPUNGE","EXPUNGE");
        tpsMenuItem(M6,"EXPUNGE-OLD","EXPUNGE-OLD");
        if (!etps) {
         M6.add(M47);
        }
        tpsMenuItem(M47,"O","O");
        tpsMenuItem(M47,"REM","REM");
        M6.add(new MenuItem("-"));
        tpsMenuItem(M6,"LEAVE","LEAVE");
        tpsMenuItem(M10,"0 (Up One Level)","|0|");
        tpsMenuItem(M10,"D","D");
        tpsMenuItem(M10,"FB","FB");
        tpsMenuItem(M10,"FI","FI");
        tpsMenuItem(M10,"GOTO","GOTO");
        tpsMenuItem(M10,"L","L");
        tpsMenuItem(M10,"R","R");
        tpsMenuItem(M10,"UP","UP");
        tpsMenuItem(M10,"^","^");
        tpsMenuItem(M8,"GO","GO");
        tpsMenuItem(M8,"NOOP","NOOP");
        tpsMenuItem(M8,"UNIFY","UNIFY");
        tpsMenuItem(M8,"ADD-CONN","ADD-CONN");
        tpsMenuItem(M8,"ADD-CONN*","ADD-CONN*");
        tpsMenuItem(M8,"INIT-MATING","INIT-MATING");
        tpsMenuItem(M8,"PROP-MSEARCH","PROP-MSEARCH");
        tpsMenuItem(M8,"MINIMAL-P","MINIMAL-P");
        tpsMenuItem(M8,"COMPLETE-P","COMPLETE-P");
        tpsMenuItem(M8,"MS88","MS88");
        tpsMenuItem(M8,"MS88-SUB","MS88-SUB");
        tpsMenuItem(M8,"MS89","MS89");
        tpsMenuItem(M8,"EXPAND-ETREE","EXPAND-ETREE");
        tpsMenuItem(M8,"MS90-3","MS90-3");
        tpsMenuItem(M8,"MS90-9","MS90-9");
        tpsMenuItem(M8,"MS91-6","MS91-6");
        tpsMenuItem(M8,"MS91-7","MS91-7");
        tpsMenuItem(M8,"MS92-9","MS92-9");
        tpsMenuItem(M8,"MS93-1","MS93-1");
        tpsMenuItem(M8,"MS98-1","MS98-1");
        tpsMenuItem(M8,"REM-CONN","REM-CONN");
        tpsMenuItem(M8,"REM-CONN*","REM-CONN*");
        tpsMenuItem(M8,"REM-LAST-CONN","REM-LAST-CONN");
        tpsMenuItem(M8,"DEL-DUP-CONNS","DEL-DUP-CONNS");
        tpsMenuItem(M8,"SHOW-MATING","SHOW-MATING");
        tpsMenuItem(M8,"SHOW-SUBSTS","SHOW-SUBSTS");
        tpsMenuItem(M3,"DP","DP");
        tpsMenuItem(M3,"DP*","DP*");
        tpsMenuItem(M3,"DP=","DP=");
        tpsMenuItem(M3,"DPTREE","DPTREE");
        tpsMenuItem(M3,"DUP-ALL","DUP-ALL");
        tpsMenuItem(M3,"DUP-OUTER","DUP-OUTER");
        tpsMenuItem(M3,"DUP-VAR","DUP-VAR");
        tpsMenuItem(M3,"EXP","EXP");
        tpsMenuItem(M3,"MOD-STATUS","MOD-STATUS");
        tpsMenuItem(M3,"NAME-PRIM","NAME-PRIM");
        tpsMenuItem(M3,"PRIM-ALL","PRIM-ALL");
        tpsMenuItem(M3,"PRIM-OUTER","PRIM-OUTER");
        tpsMenuItem(M3,"PRIM-SINGLE","PRIM-SINGLE");
        tpsMenuItem(M3,"PRIM-SUB","PRIM-SUB");
        tpsMenuItem(M3,"MS98-DUP","MS98-DUP");
        tpsMenuItem(M3,"MS98-PRIM","MS98-PRIM");
        tpsMenuItem(M3,"RESTORE-ETREE","RESTORE-ETREE");
        tpsMenuItem(M3,"SAVE-ETREE","SAVE-ETREE");
        tpsMenuItem(M3,"SEL","SEL");
        tpsMenuItem(M3,"SET-SEARCH-TREE","SET-SEARCH-TREE");
        tpsMenuItem(M3,"SUB","SUB");
        tpsMenuItem(M3,"SUB-ETREE","SUB-ETREE");
        tpsMenuItem(M3,"TERMS","TERMS");
        tpsMenuItem(M3,"APPLY-SUBSTS","APPLY-SUBSTS");
        tpsMenuItem(M3,"MERGE-TREE","MERGE-TREE");
        tpsMenuItem(M3,"ADD-EXT-LEMMAS","ADD-EXT-LEMMAS");
        tpsMenuItem(M4,"CJFORM","CJFORM");
        tpsMenuItem(M4,"CW","CW");
        tpsMenuItem(M4,"CWD","CWD");
        tpsMenuItem(M4,"CWS","CWS");
        tpsMenuItem(M4,"NUM-HPATHS","NUM-HPATHS");
        tpsMenuItem(M4,"NUM-VPATHS","NUM-VPATHS");
        tpsMenuItem(M4,"VP","VP");
        tpsMenuItem(M4,"VPD","VPD");
        tpsMenuItem(M4,"VPETREE","VPETREE");
        tpsMenuItem(M4,"VPT","VPT");
        tpsMenuItem(M7,"ETD","ETD");
        tpsMenuItem(M7,"ETP","ETP");
        tpsMenuItem(M7,"P","P");
        tpsMenuItem(M7,"PDEEP","PDEEP");
        tpsMenuItem(M7,"PP","PP");
        tpsMenuItem(M7,"PPDEEP","PPDEEP");
        tpsMenuItem(M7,"PPF","PPF");
        tpsMenuItem(M7,"PSH","PSH");
        tpsMenuItem(M7,"PTREE","PTREE");
        tpsMenuItem(M7,"PTREE*","PTREE*");
        tpsMenuItem(M7,"PTREE-FILE","PTREE-FILE");
        tpsMenuItem(M7,"SHOW-OPTION-TREE","SHOW-OPTION-TREE");
        tpsMenuItem(M20,"|0| (Up One Level)","|0|");
        tpsMenuItem(M20,"APPLY-SUBST","APPLY-SUBST");
        tpsMenuItem(M20,"EPROOF-UTREE","EPROOF-UTREE");
        tpsMenuItem(M20,"GO","GO");
        tpsMenuItem(M20,"GOTO","GOTO");
        tpsMenuItem(M20,"MATCH","MATCH");
        tpsMenuItem(M20,"MATCH-PAIR","MATCH-PAIR");
        tpsMenuItem(M20,"NAME-DPAIR","NAME-DPAIR");
        tpsMenuItem(M20,"NTH-SON","NTH-SON");
        tpsMenuItem(M20,"P","P");
        tpsMenuItem(M20,"PALL","PALL");
        tpsMenuItem(M20,"PP","PP");
        tpsMenuItem(M20,"PP*","PP*");
        tpsMenuItem(M20,"SIMPLIFY","SIMPLIFY");
        tpsMenuItem(M20,"STATS","STATS");
        tpsMenuItem(M20,"SUBST-STACK","SUBST-STACK");
        tpsMenuItem(M20,"UTREE","UTREE");
        tpsMenuItem(M20,"UTREE*","UTREE*");
        tpsMenuItem(M20,"^","^");
        tpsMenuItem(M20,"^^","^^");
        M20.add(new MenuItem("-"));
        tpsMenuItem(M20,"LEAVE","LEAVE");
        tpsMenuItem(M19,"ADD-DPAIR","ADD-DPAIR");
        tpsMenuItem(M19,"ADD-DPAIRS-TO-NODE","ADD-DPAIRS-TO-NODE");
        tpsMenuItem(M19,"ADD-DPAIRS-TO-UTREE","ADD-DPAIRS-TO-UTREE");
        tpsMenuItem(M19,"FIND-NESTING","FIND-NESTING");
        tpsMenuItem(M19,"PRUNE","PRUNE");
        tpsMenuItem(M19,"RM-DPAIR","RM-DPAIR");
        tpsMenuItem(M19,"SHOW-DPAIRSET","SHOW-DPAIRSET");
        tpsMenuItem(M19,"UNIF-PROBLEM","UNIF-PROBLEM");
        setMenuBar(mbar);
    }

    public void adjustMenus(String newtoplevel) {
        toplevel = newtoplevel;
        if (toplevel.equals("CMD-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
        } else if (toplevel.equals("ED-TOP")) {
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
            mbar.add(M2);
            mbar.add(M1);
            mbar.add(M0);
        } else if (toplevel.equals("EXT-MATE-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
        } else if (toplevel.equals("EXT-SEQ-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
        } else if (toplevel.equals("LIBRARY-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
            mbar.add(M5);
        } else if (toplevel.equals("MTREE-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
            mbar.add(M11);
            mbar.add(M12);
            mbar.add(M13);
        } else if (toplevel.equals("PRFW-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
        } else if (toplevel.equals("REVIEW-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
            mbar.add(M14);
            mbar.add(M9);
        } else if (toplevel.equals("TEST-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
            mbar.add(M16);
            mbar.add(M15);
            mbar.add(M17);
        } else if (toplevel.equals("UNIX-LIBRARY-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
            mbar.add(M18);
        } else if (toplevel.equals("MATE-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M20);
            mbar.remove(M19);
            mbar.add(M6);
            mbar.add(M10);
            mbar.add(M8);
            mbar.add(M3);
            mbar.add(M4);
            mbar.add(M7);
        } else if (toplevel.equals("UNIF-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.add(M20);
            mbar.add(M19);
        } else if (toplevel.equals("GRADE-TOP")) {
            mbar.remove(M2);
            mbar.remove(M1);
            mbar.remove(M0);
            mbar.remove(M5);
            mbar.remove(M11);
            mbar.remove(M12);
            mbar.remove(M13);
            mbar.remove(M14);
            mbar.remove(M9);
            mbar.remove(M16);
            mbar.remove(M15);
            mbar.remove(M17);
            mbar.remove(M18);
            mbar.remove(M6);
            mbar.remove(M10);
            mbar.remove(M8);
            mbar.remove(M3);
            mbar.remove(M4);
            mbar.remove(M7);
            mbar.remove(M20);
            mbar.remove(M19);
        }
    }
    // Menu Code END

    public void TpsWinInit(int maxChars,String host,int port,String fontsize,int fontshift) {
        tpsServHost = host;
	if (etps) {
	    setTitle("ETPS (Java Interface)");
	} else {
	    setTitle("TPS (Java Interface)");
	}
        try {
            addr = InetAddress.getByName(tpsServHost);
            sock  = new Socket(addr,port);
            sendtps = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));
        } catch (IOException ex) {
	    System.out.println("IOException - could not open socket to " + host + ":" + port);
            endTpsWin();
        }
	//	System.out.println("new TpsApplet"); // delete me
        tpsApplet = new TpsApplet();
	tpsApplet.bottomOffset = bottomOffset;
	//	System.out.println("initializing TpsApplet"); // delete me
        tpsApplet.tpsInit(maxChars,fontsize,fontshift,this);

	add("Center",tpsApplet);
	add("East",vBar);
	if (!popups) {
	    Panel southPanel = new Panel();
	    resp = new TextField(79);
	    TpsMenuActionListener respl = new TpsMenuActionListener();
	    respl.tpsLineActionCommand(this,sendtps);
	    resp.addActionListener(respl);
	    southPanel.add("West",resp);
	    add("South",southPanel);
	    keyListener = new TpsKeyListener(this);
	    resp.addKeyListener(keyListener);
	}
	//	System.out.println("initializing window and event listeners"); // delete me
	winListener = new TpsWindowListener(this);
	eventListener = new TpsEventListener(this,tpsApplet);

	vBar.addAdjustmentListener(new TpsBarListener(tpsApplet));
	addComponentListener(eventListener);
	addWindowListener(winListener);
	//	System.out.println("initializing thread"); // delete me
        tpsThread = new TpsThread(this,tpsApplet,addr,sock,sendtps,tpsServHost,fontshift);
	//	System.out.println("starting thread"); // delete me
        tpsThread.start();
    }

    private void sendTpsLine(String ln) {
        byte[] bl = ln.getBytes();
        try {
            sendtps.write(bl);
            sendtps.write(0);
            sendtps.flush();
        } catch (IOException ex) {
            System.out.println("IO Exception sending TPS line " + ln);
            tpsApplet.tpsPrintLine("Lost Connection.");
        }
    }

    public void sendTpsCommand(String comm) {
        sendTpsLine(commandStr);
        sendTpsLine(comm);
    }

    public void sendRightMargin(int rm) {
        sendTpsLine(rightmarginStr);
        sendTpsLine(" " + rm);
    }

    public void sendExpertStatus() {
	sendTpsLine("REMOTE-EXPERT");
    }

    public void sendNoPopups() {
	sendTpsLine("NOPOPUPS");
    }

    public void exitTpsWin() {
	try {
	    sendtps.close();
	    sock.close();
	} catch (IOException ex) {
	    System.out.println("IOException closing stream to " + tpsServHost + ":" + tpsServPort);
	}
	endTpsWin();
    }

    public void endTpsWin() {
        if (inAnApplet) {
            dispose();
        } else {
            System.exit(0);
        }
    }
}
