@Section(Example of a Proof of a Higher-Order Theorem)
The following is an annotated transcript of part of a proof in type theory.
Basic familiarity with @ETPS is assumed.

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

<1>@indexcommand(Exercise) X5209 
(100)        !    POWERSET [D(OA) INTERSECT E(OA)]
                 = POWERSET D INTERSECT POWERSET E                     PLAN1

<2>@indexsrule(ext=)
P2 (LINE): Line with Equality [100]>
P1 (LINE): Universally Quantified Equality [99]>
x (GWFF): Universally Quantified Variable [No Default]>"S(OA)"
(99)         !  FORALL S(OA).  POWERSET [D(OA) INTERSECT E(OA)] S
                              = [POWERSET D INTERSECT POWERSET E] S    PLAN2

<3>@indexsrule(ugen) !
(98)         !    POWERSET [D(OA) INTERSECT E(OA)] S(OA)
                 = [POWERSET D INTERSECT POWERSET E] S                 PLAN3

<4>@indexsrule(ext=0)
P2 (LINE): Line with Equality [98]>
P1 (LINE): Line with Equivalence [97]>
(97)         !        POWERSET [D(OA) INTERSECT E(OA)] S(OA)
                 EQUIV [POWERSET D INTERSECT POWERSET E] S             PLAN4

<5>@indexsrule(implics-equiv)
P2 (LINE): Line with Equivalence [97]>
P1 (LINE): Line with Implications in Both Directions [96]>
(96)         !      [        POWERSET [D(OA) INTERSECT E(OA)] S(OA)
                      IMPLIES [POWERSET D INTERSECT POWERSET E] S]
                 AND.        [POWERSET D INTERSECT POWERSET E] S
                      IMPLIES POWERSET [D INTERSECT E] S               PLAN5

<6>iconj !
(48)         !          POWERSET [D(OA) INTERSECT E(OA)] S(OA)
                 IMPLIES [POWERSET D INTERSECT POWERSET E] S           PLAN7
(95)         !          [POWERSET D(OA) INTERSECT POWERSET E(OA)] S(OA)
                 IMPLIES POWERSET [D INTERSECT E] S                    PLAN6

@\ @I{In this example we will prove only line @T(95).  It may be a}
@\ @I{a good exercise to try to prove line @T(48).}

<7>@indexcommand(subproof)
PLINE (PLINE): Line to prove [48]>95

<8>@indexsrule(deduct) !
(49)  49     !  [POWERSET D(OA) INTERSECT POWERSET E(OA)] S(OA)          Hyp
(94)  49     !  POWERSET [D(OA) INTERSECT E(OA)] S(OA)                 PLAN8

@\ @I{Now we eliminate the @T(POWERSET).  @T(S) is in the powerset}
@\ @I{of @wt{ D @intersect E } iff @T(S) is a subset of @wt{ D @intersect E}.}

<9>@indexsrule(idef) !
(93)  49     !  S(OA) SUBSET D(OA) INTERSECT E(OA)                     PLAN9

@\ @I{Now we eliminate the @T(INTERSECT) from the justified line @T(49).}
@\ @I{We therefore have to use the command symmetric to @t(IDEF),}
@\ @I{which is @t(EDEF).}

<10>@indexsrule(edef) !
(50)  49     !  POWERSET D(OA) S(OA) AND POWERSET E(OA) S           Defn: 49

<1>@indexcommand(^P)
(50)  49     !  POWERSET D(OA) S(OA) AND POWERSET E(OA) S           Defn: 49
               ...
(93)  49     !  S(OA) SUBSET D(OA) INTERSECT E(OA)                     PLAN9

<2>@indexsrule(econj) !
(51)  49     !  POWERSET D(OA) S(OA)                                Conj: 50
(52)  49     !  POWERSET E(OA) S(OA)                                Conj: 50

@\ @I{From here on we go through a sequence of routine elimination of definitions.}

<3>edef ! 
(53)  49     !  S(OA) SUBSET D(OA)                                  Defn: 51

<4>edef 
D1 (LINE): Line with Definition [53]>52
D2 (LINE): Line with Instantiated Definition [54]>
(54)  49     !  S(OA) SUBSET E(OA)                                  Defn: 52

<5>^P
(53)  49     !  S(OA) SUBSET D(OA)                                  Defn: 51
(54)  49     !  S(OA) SUBSET E(OA)                                  Defn: 52
               ...
(93)  49     !  S(OA) SUBSET D(OA) INTERSECT E(OA)                     PLAN9

@\ @I{We are on the right track!  From @wt{S @subset D} and @wt{S @subset E} we should be} 
@\ @I{able to infer that @wt{S @subset D @intersect E}}

<6>edef 53
D2 (LINE): Line with Instantiated Definition [55]>
(55)  49     !  FORALL x(A).S(OA) x IMPLIES D(OA) x                 Defn: 53

<7>edef 54
D2 (LINE): Line with Instantiated Definition [56]>
(56)  49     !  FORALL x(A).S(OA) x IMPLIES E(OA) x                 Defn: 54

<8>idef !
(92)  49     !  FORALL x(A).S(OA) x IMPLIES [D(OA) INTERSECT E(OA)] x PLAN16

<9>^P
(55)  49     !  FORALL x(A).S(OA) x IMPLIES D(OA) x                 Defn: 53
(56)  49     !  FORALL x(A).S(OA) x IMPLIES E(OA) x                 Defn: 54
               ...
(92)  49     !  FORALL x(A).S(OA) x IMPLIES [D(OA) INTERSECT E(OA)] x PLAN16

@\ @I{We have to get rid of the universal quantifier, but we have to be careful to give}
@\ @I{ our variable the right type, namely @alpha.}

<10>@indexsrule(ui) 55 
D2 (LINE): Instantiated Line [57]>
t (GWFF): Substitution Term [No Default]>"x(A)"
(57)  49     !  S(OA) x(A) IMPLIES D(OA) x                          UI: x 55

<1>ui 56
D2 (LINE): Instantiated Line [58]>
@\ @I{Let's use the editor to extract the variable.}
t (GWFF): Substitution Term [No Default]>(ed 56)

<Ed1>a
x(A)
<Ed2>ok
(58)  49     !  S(OA) x(A) IMPLIES E(OA) x                          UI: x 56

<2>@indexsrule(ugen) !
(91)  49     !  S(OA) x(A) IMPLIES [D(OA) INTERSECT E(OA)] x          PLAN19

<3>@indexsrule(deduct) !
(59)  49,59  !  S(OA) x(A)                                               Hyp
(90)  49,59  !  [D(OA) INTERSECT E(OA)] x(A)                          PLAN20

<4>@indexsrule(idef) !
(89)  49,59  !  D(OA) x(A) AND E(OA) x                                PLAN21

<5>^P
(55)  49     !  FORALL x(A).S(OA) x IMPLIES D(OA) x                 Defn: 53
(56)  49     !  FORALL x(A).S(OA) x IMPLIES E(OA) x                 Defn: 54
(57)  49     !  S(OA) x(A) IMPLIES D(OA) x                          UI: x 55
(58)  49     !  S(OA) x(A) IMPLIES E(OA) x                          UI: x 56
(59)  49,59  !  S(OA) x(A)                                               Hyp
               ...
(89)  49,59  !  D(OA) x(A) AND E(OA) x                                PLAN21

@\ @i{We don't need the universally quantified sponsoring lines any more}
@\ @i{in order to prove line @t(89), so let's use @indexcommand(UNSPONSOR).}

<6>unsponsor 
PLINE (PLINE):Planned line [89]>
LINELIST (EXISTING-LINELIST): Sponsoring lines [(59 58 56 57 55)]>(55 56)

<7>@indexsrule(rulep) !

<8>pall

               ...
(48)         !          POWERSET [D(OA) INTERSECT E(OA)] S(OA)
                 IMPLIES [POWERSET D INTERSECT POWERSET E] S           PLAN7
(49)  49     !  [POWERSET D(OA) INTERSECT POWERSET E(OA)] S(OA)          Hyp
(50)  49     !  POWERSET D(OA) S(OA) AND POWERSET E(OA) S           Defn: 49
(51)  49     !  POWERSET D(OA) S(OA)                                Conj: 50
(52)  49     !  POWERSET E(OA) S(OA)                                Conj: 50
(53)  49     !  S(OA) SUBSET D(OA)                                  Defn: 51
(54)  49     !  S(OA) SUBSET E(OA)                                  Defn: 52
(55)  49     !  FORALL x(A).S(OA) x IMPLIES D(OA) x                 Defn: 53
(56)  49     !  FORALL x(A).S(OA) x IMPLIES E(OA) x                 Defn: 54
(57)  49     !  S(OA) x(A) IMPLIES D(OA) x                          UI: x 55
(58)  49     !  S(OA) x(A) IMPLIES E(OA) x                          UI: x 56
(59)  49,59  !  S(OA) x(A)                                               Hyp
(89)  49,59  !  D(OA) x(A) AND E(OA) x                       Rulep: 59 58 57
(90)  49,59  !  [D(OA) INTERSECT E(OA)] x(A)                        Defn: 89
(91)  49     !  S(OA) x(A) IMPLIES [D(OA) INTERSECT E(OA)] x      Deduct: 90
(92)  49     !  FORALL x(A).S(OA) x IMPLIES [D(OA) INTERSECT E(OA)] x
                                                                  Ugen: x 91
(93)  49     !  S(OA) SUBSET D(OA) INTERSECT E(OA)                  Defn: 92
(94)  49     !  POWERSET [D(OA) INTERSECT E(OA)] S(OA)              Defn: 93
(95)         !          [POWERSET D(OA) INTERSECT POWERSET E(OA)] S(OA)
                 IMPLIES POWERSET [D INTERSECT E] S               Deduct: 94
(96)         !      [        POWERSET [D(OA) INTERSECT E(OA)] S(OA)
                      IMPLIES [POWERSET D INTERSECT POWERSET E] S]
                 AND.        [POWERSET D INTERSECT POWERSET E] S
                      IMPLIES POWERSET [D INTERSECT E] S         Conj: 48 95
(97)         !        POWERSET [D(OA) INTERSECT E(OA)] S(OA)
                 EQUIV [POWERSET D INTERSECT POWERSET E] S      ImpEquiv: 96
(98)         !    POWERSET [D(OA) INTERSECT E(OA)] S(OA)
                 = [POWERSET D INTERSECT POWERSET E] S              Ext=: 97
(99)         !  FORALL S(OA).  POWERSET [D(OA) INTERSECT E(OA)] S
                              = [POWERSET D INTERSECT POWERSET E] S
                                                                  Ugen: S 98
(100)        !    POWERSET [D(OA) INTERSECT E(OA)]
                 = POWERSET D INTERSECT POWERSET E                  Ext=: 99

<9>exit

@End(Verbatim)
