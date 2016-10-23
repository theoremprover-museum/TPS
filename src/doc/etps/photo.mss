@part(Examples3, root "ETPS.mss")
@ChapterPh(Sample Proofs)@label(examples)
The following are transcripts of proofs of sample theorems obtained
by using @wt{script} before starting @ETPS.  Remarks are added in italics.
It may be a good idea to look ahead a little bit, i.e., look at the final
proof first to see what we are trying to obtain. You can 
execute these proof steps on your own computer and use the @T(PALL)
command frequently to get a good picture of how the proof grows, or
(if you are running @ETPS under X-windows or using the Java interface)
use the @T(BEGIN-PRFW) command to open proofwindows, and watch the proof 
grow in them. As mentioned in Section @ref(Introduction), you will probably find it best to set the
style flag to xterm and have logical formulas displayed with logical symbols
if you can. However, in this chapter we display formulas in generic style.

@Section(Example 1)

@Begin(Verbatim, LineWidth 80, LeftMargin -4)
@TabSet(1.5 inches)
>etps

etps for issar. Version from Saturday, September 23, 1989 at  5:59:15.. 
(c) Copyrighted 1988 by Carnegie Mellon University. All rights reserved. 
**************************************************************************** 
WARNING -- Be sure that you when you begin ETPS, your current directory is  
           one for which you have write access, e.g., your home directory. 
**************************************************************************** 
**************************************************************************** 
WARNING -- You cannot use the Unix ~ convention in specifying file names. 
           Use the full pathname instead, e.g., instead of entering 
           "~/foo.work", enter "/afs/andrew/usr11/dn0z/foo.work". 
**************************************************************************** 
**************************************************************************** 
ANNOUNCING -- ETPS can now be run on the sun3_35 workstation type, as well 
	      as on the Microvax. 
	      The more memory on the machine, the faster ETPS will run.  To 
              check the amount of memory available on a Sun-3, type  
	      "/etc/dmesg | grep avail" in your typescript. 
**************************************************************************** 
 
[Loading changes ... 
                 ...done] 
Loading /afs/andrew.cmu.edu/math/etps/etps.ini 
Finished loading /afs/andrew.cmu.edu/math/etps/etps.ini 

@\ @i{If you are using ETPS in an environment where proofwindows are}
@\ @i{available, issue the @T(BEGIN-PRFW) command now to open proofwindows.}

<1>@indexcommand{exercise} x2108
(100)     ! FORALL x EXISTS y.P x IMPLIES P y                          PLAN1

@\ @i{Since this theorem is universally quantified, we will first use}
@\ @i{universal generalization.  Note that to accept the defaults that @ETPS}
@\ @i{offers, we just hit a <Return>.}

<2>@indexsrule(ugen)
P2 (LINE): Universally Quantified Line [100]>
P1 (LINE): Line with Scope of Universal Quantifier [99]>
(99)      ! EXISTS y.P x IMPLIES P y                                   PLAN2

@\ @I{As we will see later, the justification of line @T(100) has been changed}
@\ @I{from @T(PLAN1) to @T(UGen), which stands for universal generalization.}
@\ @i{Now the formula we are trying to prove is existentially quantified,}
@\ @i{so we use the appropriate rule.}

<3>@indexsrule(egen)
P2 (LINE): Existentially Quantified Line [99]>
P1 (LINE): Line to be Existentially Generalized [98]>
t (GWFF): Term to be Generalized Upon [No Default]>(@itt(ED) 99)

@\ @i{Let's use the editor to pick out the term we want from line @t(99).}

<Ed1>p
EXISTS y.P x IMPLIES P y     @i{Here's the current formula.}
<Ed2>d
P x IMPLIES P y              @i{We move inside the quantifier.}
<Ed3>l d                     @i{We use two commands to get to x.}
x
<Ed4>ok                      @i{Return x as the @t(GWFF) we were asked for.}
(98)      ! P x IMPLIES P x                                            PLAN3

@\ @i{All that remains is an easy application of @t(RULEP).}

<4>@indexsrule(rulep)
P1 (PLINE): Plan Line [98]>
L (EXISTING-LINELIST): List of Lines [()]>

<5>@indexcommand(squeeze)

@\ @i{@t(SQUEEZE) removes any unnecessary gaps.  Now we take a look at the}
@\ @i{completed proof.}

<6>@indexcommand(pall)

(1)       ! P x IMPLIES P x                                            RuleP
(2)       ! EXISTS y.P x IMPLIES P y                               EGen: x 1
(3)       ! FORALL x EXISTS y.P x IMPLIES P y                      UGen: x 2

<7>@indexcommand(done)
Score file updated.

@\ @i{The @t(DONE) command is crucial!  Not only does it}
@\ @i{verify that the proof is complete, it also ensures that you get credit}
@\ @i{for doing it.  Now let's make a nice copy of the proof.}

<8>@indexcommand(texproof)
FILENAME (FILESPEC): Filename [x2108.tex]>
Written file x2108.tex

<9>^Z

@\ @i{Let's interrupt @ETPS and print the current proof.}

>tex x2108
This is TeX, Version 3.14159 (C version 6.1)
(x2108.tex
Hyphenation patterns for english, german, loaded.
(/afs/cs/project/tps/tps/doc/lib/tps.tex) [1] )
Output written on x2108.dvi (1 page, 628 bytes).
Transcript written on x2108.log.

>dvips x2108 -o x2108.ps
This is dvipsk 5.58f Copyright 1986, 1994 Radical Eye Software
' TeX output 1998.09.04:1219' -> x2108.ps
<tex.pro>. [1] 

>lpr -Pprinter x2108.ps

@\ @i{We'll now resume our @ETPS session. The unix command @t{fg} continues the}
@\ @i{last job that was interrupted.}

>fg

@\ @i{Let's now prove this same theorem in a different way,}
@\ @i{and save it in a new file.}

<1>@indexcommand(stop-save)
File x2108.work written.

<2>@indexcommand(save-work)
SAVEFILE (FILESPEC): SAVE-WORK file [work.work]>"x2108b.work"

<3>@indexcommand(exercise) x2108
(100)     ! FORALL x EXISTS y.P x IMPLIES P y                          PLAN1

<4>@indexsrule(indirect)
P3 (LINE): Line to be Proven by Contradiction [100]>
P2 (LINE): Line with Contradiction [99]>
H1 (LINE): Line with Assumed Negation [1]>
(1)   1   ! ~ FORALL x EXISTS y.P x IMPLIES P y              Assume negation
(99)  1   ! FALSEHOOD                                                  PLAN2

@\ @i{We can always use indirect proof.  }
@\ @i{As you can see, line @t(1) is negated, so let's push in that negation.}

<5>@indexsrule(pushneg)
D1 (LINE): Line with Negation [1]>
D2 (LINE): Line after Pushing in Negation one Step [2]>
(2)   1   ! EXISTS x.~ EXISTS y.P x IMPLIES P y                       Neg: 1

@\ @i{We use the @t(^P) command to show the lines which are now relevant.}

<6>@indexcommand(^p)
(2)   1   ! EXISTS x.~ EXISTS y.P x IMPLIES P y                       Neg: 1
               ...
(99)  1   ! FALSEHOOD                                                  PLAN2

@\ @i{@wt(RULEC) is often required when trying to prove a statement}
@\ @i{from an existentially quantified line.  It is probably the most}
@\ @i{complicated rule you will use, so you might wish to study the}
@\ @i{description of @wt(RULEC) in the previous chapter first, as well}
@\ @i{the description in the textbook.}

<7>@indexsrule(rulec)
P4 (LINE): Conclusion without Additional Hypothesis [99]>
D1 (LINE): Existentially Quantified Line [2]>
D3 (LINE): Conclusion with Additional Hypothesis [98]>
H2 (LINE): Hypothesis with Chosen Variable [3]>
y (GWFF): Chosen Variable Name [No Default]>"x"
(3)   1,3 ! ~ EXISTS y.P x IMPLIES P y                             Choose: x
(98)  1,3 ! FALSEHOOD                                                  PLAN5

@\ @i{The last command created a negated statement, so we can use}
@\ @i{@wt(PUSHNEG) again.}

<8>@indexsrule(pushneg) 3 4
(4)   3,1 ! FORALL y.~.P x IMPLIES P y                                Neg: 3

<9>@indexsrule(ui)
D1 (LINE): Universally Quantified Line [4]>
D2 (LINE): Instantiated Line [5]>
t (GWFF): Substitution Term [No Default]>"x"
(5)   1,3 ! ~.P x IMPLIES P x                                        UI: x 4

<10>@indexcommand(^p)
(4)   3,1 ! FORALL y.~.P x IMPLIES P y                                Neg: 3
(5)   1,3 ! ~.P x IMPLIES P x                                        UI: x 4
               ...
(98)  1,3 ! FALSEHOOD                                                  PLAN5

@\ @i{Note that line @t(5) is a contradiction, so we can use it to justify}
@\ @i{line @t(98) by @t(RULEP).  Line @t(4) isn't necessary for this step.}

<1>@indexsrule(rulep) 98
L (EXISTING-LINELIST): List of Lines [(5 4)]>(5)

<2>@indexcommand(squeeze)

<3>@indexcommand(cleanup)
No lines can be deleted.

@\ @i{@t(CLEANUP) will remove unnecessary lines and hypotheses from}
@\ @i{your finished proof.}

<4>@indexcommand(pall)

(1)   1   ! ~ FORALL x EXISTS y.P x IMPLIES P y              Assume negation
(2)   1   ! EXISTS x.~ EXISTS y.P x IMPLIES P y                       Neg: 1
(3)   3   ! ~ EXISTS y.P x IMPLIES P y                             Choose: x
(4)   3   ! FORALL y.~.P x IMPLIES P y                                Neg: 3
(5)   3   ! ~.P x IMPLIES P x                                        UI: x 4
(6)   3   ! FALSEHOOD                                               RuleP: 5
(7)   1   ! FALSEHOOD                                             RuleC: 2 6
(8)       ! FORALL x EXISTS y.P x IMPLIES P y                    Indirect: 7

@\ @i{Have we finished?}

<5>@indexcommand(done)
Score file updated.

@\ @i{Yes.  Let's make a nice copy of this proof.  Note that we have }
@\ @i{to specify a new file name to keep @ETPS from overwriting}
@\ @i{the first file we made.}

<6>@indexcommand(texproof)
FILENAME (FILESPEC): Filename [x2108.tex]>"x2108b"
Written file x2108b.tex

@\ @i{If you have open proofwindows, close them now with the}
@\ @i{command @T(END-PRFW).}

<7>@indexcommand(exit)
File x2108b.work written.
@end(verbatim)

@Section(Example 2)
@Begin(Verbatim, LineWidth 80, LeftMargin -4)
@TabSet(1.5 inches)
>etps

etps for issar. Version from Saturday, September 23, 1989 at  5:59:15.. 
(c) Copyrighted 1988 by Carnegie Mellon University. All rights reserved. 
**************************************************************************** 
WARNING -- Be sure that you when you begin ETPS, your current directory is  
           one for which you have write access, e.g., your home directory. 
**************************************************************************** 
**************************************************************************** 
WARNING -- You cannot use the Unix ~ convention in specifying file names. 
           Use the full pathname instead, e.g., instead of entering 
           "~/foo.work", enter "/afs/andrew/usr11/dn0z/foo.work". 
**************************************************************************** 
**************************************************************************** 
ANNOUNCING -- ETPS can now be run on the sun3_35 workstation type, as well 
	      as on the Microvax. 
	      The more memory on the machine, the faster ETPS will run.  To 
              check the amount of memory available on a Sun-3, type  
	      "/etc/dmesg | grep avail" in your typescript. 
**************************************************************************** 
 
[Loading changes ... 
                 ...done] 
Loading /afs/andrew.cmu.edu/math/etps/etps.ini 
Finished loading /afs/andrew.cmu.edu/math/etps/etps.ini 
 
<1>prove
WFF (GWFF0): Prove Wff [No Default]>"exists x forall y P x y implies
forall y exists x P x y"
PREFIX (SYMBOL): Name of the Proof [No Default]>example1
NUM (LINE): Line Number for Theorem [100]>
(100)     !  EXISTS x FORALL y P x y IMPLIES FORALL y EXISTS x P x y   PLAN1

@\ @I{If we were trying to prove one of the exercises in the text, we would}
@\ @I{have used @indexcommand(EXERCISE) instead of @indexcommand(prove).}

@\ @I{Note that @T(EXISTS) (for example) was typed in lower case, but is always}
@\ @I{printed in upper case.}

<2>deduct
P3 (LINE): Line with Implication [100]>
D2 (LINE): Line with Conclusion [99]>
H1 (LINE): Line with Hypothesis [1]>
(1)   1   !  EXISTS x FORALL y P x y                                     Hyp
(99)  1   !  FORALL y EXISTS x P x y                                   PLAN2

@\ @I{@indexsrule(DEDUCT) is often the right way to start the proof of an implication.}
@\ @I{Note that the defaults were just what we wanted anyway, so we selected}
@\ @I{them by simply typing @wt{<Return>}.}

<3>ugen
P2 (LINE): Universally Quantified Line [99]>
P1 (LINE): Line with Scope of Universal Quantifier [98]>
(98)  1   !  EXISTS x P x y                                            PLAN3

<4>rulec
P4 (LINE): Conclusion without Additional Hypothesis [98]>
D1 (LINE): Existentially Quantified Line [1]>
D3 (LINE): Conclusion with Additional Hypothesis [97]>
H2 (LINE): Hypothesis with Chosen Variable [2]>
y (GWFF): Chosen Variable Name [No Default]>"x"
(2)   1,2 !  FORALL y P x y                                        Choose: x
(97)  1,2 !  EXISTS x P x y                                            PLAN5

@\ @I{We now do a @indexcommand{^P} (note that this is not a control-character) to}
@\ @I{see what still has to be proven.}

<5>^P
(2)   1,2 !  FORALL y P x y                                        Choose: x
               ...
(97)  1,2 !  EXISTS x P x y                                            PLAN5

<6>ui 2
D2 (LINE): Instantiated Line [3]>
t (GWFF): Substitution Term [No Default]>"y"
(3)   2,1 !  P x y                                                   UI: y 2

@\ @I{We are closing in. You can now see that line @T(97) follows immediately from}
@\ @I{line @T(3) by existential generalization.  Therefore we use the}
@\ @i{@indexsrule(EGEN) command, and display the proof with the }
@\ @i{@indexcommand{PALL} command.}

<7>egen
P2 (LINE): Existentially Quantified Line [97]>
P1 (LINE): Line to be Existentially Generalized [96]>3
t (GWFF): Term to be Generalized Upon [No Default]>"x"

<8>pall

(1)   1   !  EXISTS x FORALL y P x y                                     Hyp
(2)   1,2 !  FORALL y P x y                                        Choose: x
(3)   2,1 !  P x y                                                   UI: y 2
(97)  1,2 !  EXISTS x P x y                                        EGen: x 3
(98)  1   !  EXISTS x P x y                                      RuleC: 1 97
(99)  1   !  FORALL y EXISTS x P x y                              UGen: y 98 
(100)     !  EXISTS x FORALL y P x y IMPLIES FORALL y EXISTS x P x y
                                                                  Deduct: 99

@\ @I{This is what our completed proof looks like.  Let's make sure that}
@\ @I{we are done and print the proof into a file before exiting @ETPS.}

<9>@indexcommand(done)
You completed the proof.  Since this is not an assigned exercise,
the score file will not be updated.

<10>@indexcommand(texproof)
FILENAME (FILESPEC): Filename [example1.tex]>
Written file example1.tex

<10>@indexcommand(exit) 

@end(verbatim)
@Section(Example 3)

@Begin(Verbatim, LineWidth 80, LeftMargin -4)
@TabSet(1.5 inches)
>etps

etps for issar. Version from Saturday, September 23, 1989 at  5:59:15.. 
(c) Copyrighted 1988 by Carnegie Mellon University. All rights reserved. 
**************************************************************************** 
WARNING -- Be sure that you when you begin ETPS, your current directory is  
           one for which you have write access, e.g., your home directory. 
**************************************************************************** 
**************************************************************************** 
WARNING -- You cannot use the Unix ~ convention in specifying file names. 
           Use the full pathname instead, e.g., instead of entering 
           "~/foo.work", enter "/afs/andrew/usr11/dn0z/foo.work". 
**************************************************************************** 
**************************************************************************** 
ANNOUNCING -- ETPS can now be run on the sun3_35 workstation type, as well 
	      as on the Microvax. 
	      The more memory on the machine, the faster ETPS will run.  To 
              check the amount of memory available on a Sun-3, type  
	      "/etc/dmesg | grep avail" in your typescript. 
**************************************************************************** 
 
[Loading changes ... 
                 ...done] 
Loading /afs/andrew.cmu.edu/math/etps/etps.ini 
Finished loading /afs/andrew.cmu.edu/math/etps/etps.ini 

<1>@indexcommand(prove)
WFF (GWFF0): Prove Wff [No Default]>"forall x [forall y P x y implies Q x x]
implies. forall z [P a z and P b z] implies . Q a a and Q b b"
PREFIX (SYMBOL): Name of the Proof [No Default]>example2
NUM (LINE): Line Number for Theorem [100]>
(100)     !          FORALL x [FORALL y P x y IMPLIES Q x x]
                 IMPLIES.FORALL z [P a z AND P b z] IMPLIES Q a a AND Q b b
                                                                       PLAN1

@\ @I{This example does not involve an existential quantifier, but has a more} 
@\ @I{complicated structure. Since our theorem is an implication, we use the}
@\ @I{deduction theorem again right away.}

<2>@indexsrule(deduct)
P3 (LINE): Line with Implication [100]>!
(1)   1   !  FORALL x.FORALL y P x y IMPLIES Q x x                       Hyp
(99)  1   !  FORALL z [P a z AND P b z] IMPLIES Q a a AND Q b b        PLAN2

@\ @I{Note that we used @t(!) to specify that we want to choose the defaults}
@\ @i{for the remaining arguments.}

@\ @I{It is clear that we need to instantiate @T(x) with @T(a) and @T(b).}
@\ @I{We do this in the next two steps.}

<3>@indexsrule(ui)
D1 (LINE): Universally Quantified Line [1]>!
Some defaults could not be determined.
t (GWFF): Substitution Term [No Default]>"a"
(2)   1   !  FORALL y P a y IMPLIES Q a a                            UI: a 1

@\ @I{We again used @t[!], but @ETPS couldn't determine all the defaults,}
@\ @I{so it prompted us again for the arguments for which it couldn't figure}
@\ @I{the defaults.}

<4>ui
D1 (LINE): Universally Quantified Line [2]>1
D2 (LINE): Instantiated Line [3]>
t (GWFF): Substitution Term [No Default]>"b"
(3)   1   !  FORALL y P b y IMPLIES Q b b                            UI: b 1

@\ @I{The planned line @T(99) is again an implication, which suggests using} 
@\ @I{@wt{DEDUCT} again.}

<5>deduct
P3 (LINE): Line with Implication [99]>
D2 (LINE): Line with Conclusion [98]>
H1 (LINE): Line with Hypothesis [4]>
(4)   1,4 !  FORALL z.P a z AND P b z                                    Hyp
(98)  1,4 !  Q a a AND Q b b                                           PLAN5

@\ @I{We now use universal instantiation again, this time to distribute the}
@\ @I{universal quantifier over a conjunction.}

<6>ui
D1 (LINE): Universally Quantified Line [4]>
D2 (LINE): Instantiated Line [5]>
t (GWFF): Substitution Term [No Default]>"y"
(5)   4,1 !  P a y AND P b y                                         UI: y 4

@\ @I{Just to make sure @indexsrule(ECONJ) is the inference rule we want, let's call the}
@\ @I{@indexcommand(HELP) command.}

<7>help econj 
*(D1) H   !A AND B                                          
 (D2) H   !A                                  Conj: D1
 (D3) H   !B                                  Conj: D1
Transformation: (pp D1 ss) ==> (pp D2 D3 ss) 

@\ @I{Yes, that's what we need.}

<8>econj 
D1 (LINE): Line with Conjunction [5]>
D3 (LINE): Line with Right Conjunct [7]>
D2 (LINE): Line with Left Conjunct [6]>
(6)   1,4 !  P a y                                                   Conj: 5
(7)   1,4 !  P b y                                                   Conj: 5

@\ @I{Let's look at the current planned line and its support lines.}

<9>@indexcommand(pplan)
PLINE (PLINE): Print planned line [98]>
(1)   1   !  FORALL x.FORALL y P x y IMPLIES Q x x                       Hyp
(2)   1   !  FORALL y P a y IMPLIES Q a a                            UI: a 1
(3)   1   !  FORALL y P b y IMPLIES Q b b                            UI: b 1
(4)   1,4 !  FORALL z.P a z AND P b z                                    Hyp
(6)   1,4 !  P a y                                                   Conj: 5
(7)   1,4 !  P b y                                                   Conj: 5
               ...
(98)  1,4 !  Q a a AND Q b b                                           PLAN5

<10>@indexsrule(iconj)
P3 (LINE): Line with Conjunction [98]>
P2 (LINE): Line with Right Conjunct [97]>
P1 (LINE): Line with Left Conjunct [52]>
(52)  1,4 !  Q a a                                                     PLAN9
(97)  1,4 !  Q b b                                                     PLAN8

<1>@indexsrule(mp)
D2 (LINE): Line with Implication [6]>2
D3 (LINE): Line with Succedent of Implication [30]>52
P1 (LINE): Line with Antecedent of Implication [29]>
(29)  1,4 !  FORALL y P a y                                           PLAN11

<2>pplan
PLINE (PLINE): Print planned line [29]>
(1)   1   !  FORALL x.FORALL y P x y IMPLIES Q x x                       Hyp
(3)   1   !  FORALL y P b y IMPLIES Q b b                            UI: b 1
(4)   1,4 !  FORALL z.P a z AND P b z                                    Hyp
(6)   1,4 !  P a y                                                   Conj: 5
(7)   1,4 !  P b y                                                   Conj: 5
               ...
(29)  1,4 !  FORALL y P a y                                           PLAN11

<3>@indexsrule(ugen)
P2 (LINE): Universally Quantified Line [29]>
P1 (LINE): Line with Scope of Universal Quantifier [28]>6

<4>pplan
PLINE (PLINE): Print planned line [97]>
(1)   1   !  FORALL x.FORALL y P x y IMPLIES Q x x                       Hyp
(3)   1   !  FORALL y P b y IMPLIES Q b b                            UI: b 1
(4)   1,4 !  FORALL z.P a z AND P b z                                    Hyp
(6)   1,4 !  P a y                                                   Conj: 5
(7)   1,4 !  P b y                                                   Conj: 5
(29)  1,4 !  FORALL y P a y                                        UGen: y 6
(52)  1,4 !  Q a a                                                  MP: 29 2 
               ...
(97)  1,4 !  Q b b                                                     PLAN8

<5>@indexsrule(mp)
D2 (LINE): Line with Implication [52]>3
D3 (LINE): Line with Succedent of Implication [75]>97
P1 (LINE): Line with Antecedent of Implication [74]>
(74)  1,4 !  FORALL y P b y                                           PLAN14

<6>@indexsrule(ugen)
P2 (LINE): Universally Quantified Line [74]>
P1 (LINE): Line with Scope of Universal Quantifier [73]>7

@\ @i{The proof is complete. Let's look at the entire proof.}

<7>@indexcommand(pall)
(1)   1   !  FORALL x.FORALL y P x y IMPLIES Q x x                       Hyp
(2)   1   !  FORALL y P a y IMPLIES Q a a                            UI: a 1
(3)   1   !  FORALL y P b y IMPLIES Q b b                            UI: b 1
(4)   1,4 !  FORALL z.P a z AND P b z                                    Hyp
(5)   4,1 !  P a y AND P b y                                         UI: y 4
(6)   1,4 !  P a y                                                   Conj: 5
(7)   1,4 !  P b y                                                   Conj: 5
(29)  1,4 !  FORALL y P a y                                        UGen: y 6
(52)  1,4 !  Q a a                                                  MP: 29 2
(74)  1,4 !  FORALL y P b y                                        UGen: y 7
(97)  1,4 !  Q b b                                                  MP: 74 3
(98)  1,4 !  Q a a AND Q b b                                     Conj: 52 97
(99)  1   !  FORALL z [P a z AND P b z] IMPLIES Q a a AND Q b b   Deduct: 98
(100)     !          FORALL x [FORALL y P x y IMPLIES Q x x]
              IMPLIES.FORALL z [P a z AND P b z] IMPLIES Q a a AND Q b b
                                                                  Deduct: 99

@\ @I{We'll next use @indexcommand(SQUEEZE) to remove gaps from the proof structure.}
@\ @I{We could also have used @indexcommand(MODIFY-GAPS) 1 1).}

<9>squeeze

@\ @I{Let's see how the proof looks now.}

<10>pall

(1)   1   !  FORALL x.FORALL y P x y IMPLIES Q x x                       Hyp
(2)   1   !  FORALL y P a y IMPLIES Q a a                            UI: a 1
(3)   1   !  FORALL y P b y IMPLIES Q b b                            UI: b 1
(4)   1,4 !  FORALL z.P a z AND P b z                                    Hyp
(5)   4,1 !  P a y AND P b y                                         UI: y 4
(6)   1,4 !  P a y                                                   Conj: 5
(7)   1,4 !  P b y                                                   Conj: 5
(8)   1,4 !  FORALL y P a y                                        UGen: y 6
(9)   1,4 !  Q a a                                                   MP: 8 2
(10)  1,4 !  FORALL y P b y                                        UGen: y 7
(11)  1,4 !  Q b b                                                  MP: 10 3
(12)  1,4 !  Q a a AND Q b b                                      Conj: 9 11
(13)  1   !  FORALL z [P a z AND P b z] IMPLIES Q a a AND Q b b   Deduct: 12
(14)      !          FORALL x [FORALL y P x y IMPLIES Q x x]
              IMPLIES.FORALL z [P a z AND P b z] IMPLIES Q a a AND Q b b
                                                                  Deduct: 13

<1>@indexcommand(done)
You completed the proof.  Since this is not an assigned exercise,
the score file will not be updated.

@\ @I{The proof is complete. Let's print it into a file, so we can print it later}

<2>@indexcommand(printproof)
FILENAME (FILESPEC): Filename [example2.prt]>
Written file example2.prt

<3>@indexcommand(exit)

@End(Verbatim)
