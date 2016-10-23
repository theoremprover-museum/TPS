@begin(comment)
@ChapterPh(Suggested Rules)
The internal name of this category is @t{SRULE}.
A suggested rule can be defined using @t{DEFSRULE}.
Allowable properties are @t{MATCHFN}, @t{MATCH1FN}, @t{SHORTFN}, @t{PRIORITY}, and more.

@Begin(Description)
Miscellaneous Rules:@\ @t{HYP}, @t{LEMMA}, @t{SAME}.

Propositional Rules:@\ @t{CASES}, @t{DEDUCT}, @t{DISJ-IMP}, @t{ECONJ}, @t{EQUIV-IMPLICS}, @t{ICONJ}, @t{IMP-DISJ}, @t{IMPLICS-EQUIV}, @t{INDIRECT}, @t{INDIRECT1}, @t{INDIRECT2}, @t{MP}.

Negation Rules:@\ @t(ABSURD), @t(ENEG), @t(INEG), @t{PULLNEG}, @t{PUSHNEG}.

Quantifier Rules:@\ @t{AB*}, @t{ABE}, @t{ABU}, @t{EGEN}, @t{RULEC}, @t{UGEN}, @t{UI}.

Substitution Rules:@\ @t{SUBSTITUTE}.

Equality Rules:@\ @t{EXT=}, @t{EXT=0}, @t{LET}, @t{SUBST=}, @t{SUBST=L}, @t{SUBST=R}, @t{SUBST-EQUIV}, @t{SYM=}.

Definition Rules:@\ @t{EDEF}, @t{EQUIV-WFFS}, @t{IDEF}.

Lambda Conversion Rules:@\ @t(LAMBDA*), @t{LCONTR*}, @t{LEXPD*}.
@End(Description)
@end(comment)



@Section(Special Rules)
@Tabclear()
@Begin(Format)@TabSet(0.5inch,1.0in,1.3in)
@Indexsrule(RULEP)@label(RULEP)@\ @\ @\Justify a line by Rule P. 
@end(format)
@begin(text, leftmargin +0.66inch, indent 0)
Infer B@F12{o} from A@Up(1)@F12{o}@; and @Ldots and A@Up(n)@F12{o}, provided that [[A@Up(1)@F12{o}@; @and@; @Ldots @and@; A@Up(n)@F12{o}]@; @implies@; B@F12{o}] is a substitution
instance of a tautology. As a special case, infer B@F12{o} if it is a substitution instance of a tautology. 
The first argument must be the  line to be justified; the second argument must be a list of
lines (possibly empty) from which this line follows by Rule P.  The flag
@IndexFlag(RULEP-MAINFN) controls which of two functions will be used by @t(RULEP).  When
RULEP-MAINFN is set to @IndexFunction(RULEP-SIMPLE), @t(RULEP) will merely ensure that the planned
line follows by Rule P from the specified support lines.  When RULEP-MAINFN is set to
@IndexFunction(RULEP-DELUXE) (which is the default), @t(RULEP) will find a minimal subset of the
support lines which suffices to prove the planned line by Rule P (if any). Note that
@indexfunction(RULEP-DELUXE) will be somewhat slower than @indexfunction(RULEP-SIMPLE). In order to
check the setting of RULEP-MAINFN, merely enter "RULEP-MAINFN" at the top-level.  You will be
prompted for a new value and the current value will be displayed.  Hit <return> to accept the
current value, or enter the new value.
@end(text)

@Begin(Format)@TabSet(0.5inch,1.0in,1.3in)

@Indexsrule(ASSERT)@label(ASSERT)@\ @\ Assert a theorem known to @ETPS.
@end(format)
@begin(text, leftmargin +0.66inch, indent 0)

Use a theorem known to @ETPS (see Appendix @ref(theorem)) as a lemma in the current proof. Such a
theorem can only be used if allowed by the teacher for that exercise.  The first argument is the
name of the theorem; the second argument is the line number. If the line already exists, ETPS will
check whether it is a legal instance of the theorem schema, otherwise it will prompt for the
metavariables in the theorem schema (usually x or P, Q, ...).
@end(text)

@Begin(Format)@TabSet(0.5inch,1.0in,1.3in)

@Indexsrule(ADD-HYPS)@label(ADD-HYPS)@\ @\ Weaken a line to include extra hypotheses.
@end(format)
@begin(text, leftmargin +0.66inch, indent 0)
Adding the hypotheses to the line may cause some lines to become planned lines.  If possible, the
user is given the option of adding hypotheses to lines after the given line so that no lines will
become planned.
@end(text)

@Begin(Format)@TabSet(0.5inch,1.0in,1.3in)
@Indexsrule(DELETE-HYPS)@label(DELETE-HYPS)@\ @\ Delete some hypotheses from the given line.
@end(format)
@begin(text, leftmargin +0.66inch, indent 0)

This may leave the given line as a planned line.  The user is given the option of also deleting some
hypotheses from lines after the given line.  If possible, the user is given the option of deleting
some hypotheses from lines before the given line so that the given line does not become a planned
line.
@end(text)


@Section(Miscellaneous Rules)
@Begin(Description)
@begin(group)
@Indexsrule(HYP)@label(HYP)@\ 
Introduce a new hypothesis line into the proof outline.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H1     @assert@;A@F12{o}@>Hyp
*(P2)  H      @assert@;B@F12{o}@> 
Transformation: (P2 ss) ==> (P2 H1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(LEMMA)@label(LEMMA)@\ 
Introduce a Lemma.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
*(P2)  H      @assert@;B@F12{o}@> 
Transformation: (P2 ss) ==> (P2 P1 ss) (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(SAME)@label(SAME)@\ 
Use the fact that two lines are identical to justify a planned line.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
*(P2)  H      @assert@;A@F12{o}@>Same as: D1
Transformation: (P2 D1 ss) ==> @End(Verbatim)
@end(group)@End(Description)

@Section(Propositional Rules)
@Begin(Description)
@begin(group)
@Indexsrule(CASES)@label(CASES)@\ 
Rule of Cases.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @or@; B@F12{o}@> 
 (H2)  H,H2   @assert@;A@F12{o}@>Case 1: D1
 (P3)  H,H2   @assert@;C@F12{o}@> 
 (H4)  H,H4   @assert@;B@F12{o}@>Case 2: D1
 (P5)  H,H4   @assert@;C@F12{o}@> 
*(P6)  H      @assert@;C@F12{o}@>Cases: D1 P3 P5
Transformation: (P6 D1 ss) ==> (P3 H2 ss) (P5 H4 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(DEDUCT)@label(DEDUCT)@\ 
The deduction rule.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;A@F12{o}@>Hyp
 (D2)  H,H1   @assert@;B@F12{o}@> 
*(P3)  H      @assert@;A@F12{o} @implies@; B@F12{o}@>Deduct: D2
Transformation: (P3 ss) ==> (D2 H1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(DISJ-IMP)@label(DISJ-IMP)@\ 
Rule to replace a disjunction by an implication.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@not@; A@F12{o} @or@; B@F12{o}@> 
 (D2)  H      @assert@;A@F12{o} @implies@; B@F12{o}@>Disj-Imp: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(ECONJ)@label(ECONJ)@\ 
Rule to eliminate a conjunction by inferring its two conjuncts.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @and@; B@F12{o}@> 
 (D2)  H      @assert@;A@F12{o}@>Conj: D1
 (D3)  H      @assert@;B@F12{o}@>Conj: D1
Transformation: (pp D1 ss) ==> (pp D2 D3 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(EQUIV-IMPLICS)@label(EQUIV-IMPLICS)@\ 
Rule to convert an equivalence into twin implications.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;P@F12{o} @equiv@; R@F12{o}@> 
 (D2)  H      @assert@;[P@F12{o} @implies@; R@F12{o}] @and@;.R @implies@; P@>EquivImp: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(ICONJ)@label(ICONJ)@\ 
Rule to introduce a conjunction by inferring it from two conjuncts.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
 (P2)  H      @assert@;B@F12{o}@> 
*(P3)  H      @assert@;A@F12{o} @and@; B@F12{o}@>Conj: P1 P2
Transformation: (P3 ss) ==> (P1 ss) (P2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(IMP-DISJ)@label(IMP-DISJ)@\ 
Rule to replace an implication by a disjunction.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @implies@; B@F12{o}@> 
 (D2)  H      @assert@;@not@; A@F12{o} @or@; B@F12{o}@>Imp-Disj: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(IMPLICS-EQUIV)@label(IMPLICS-EQUIV)@\ 
Rule to convert twin implications into an equivalence.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;[P@F12{o} @implies@; R@F12{o}] @and@;.R @implies@; P@> 
*(P2)  H      @assert@;P@F12{o} @equiv@; R@F12{o}@>ImpEquiv: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(INDIRECT)@label(INDIRECT)@\ 
Rule of Indirect Proof.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;@not@; A@F12{o}@>Assume negation
 (P2)  H,H1   @assert@;@falsehood@> 
*(P3)  H      @assert@;A@F12{o}@>Indirect: P2 
Transformation: (P3 ss) ==> (P2 H1 ss) @End(Verbatim)
@end(group)@indexentry[key="%1",Entry="@falsehood",number]

@begin(group)
@Indexsrule(INDIRECT1)@label(INDIRECT1)@\ 
Rule of Indirect Proof using one contradictory line.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;@not@; A@F12{o}@>Assume negation
 (P2)  H,H1   @assert@;B@F12{o} @and@; @not@; B@F12{o}@> 
*(P3)  H      @assert@;A@F12{o}@>Indirect: P2 
Transformation: (P3 ss) ==> (P2 H1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(INDIRECT2)@label(INDIRECT2)@\ 
Rule of Indirect Proof using two contradictory lines.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;@not@; A@F12{o}@>Assume negation
 (P2)  H,H1   @assert@;B@F12{o}@> 
 (P3)  H,H1   @assert@;@not@; B@F12{o}@> 
*(P4)  H      @assert@;A@F12{o}@>Indirect: P2 P3
Transformation: (P4 ss) ==> (P2 H1 ss) (P3 H1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(MP)@label(MP)@\ 
Modus Ponens.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
*(D2)  H      @assert@;A@F12{o} @implies@; B@F12{o}@> 
 (D3)  H      @assert@;B@F12{o}@>MP: P1 D2
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1) @End(Verbatim)
@end(group)
@End(Description)
@Section(Negation Rules)
@Begin(Description)
@begin(group)
@Indexsrule(ABSURD)@label(ABSURD)@\ 
From falsehood, deduce anything.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;@falsehood@;@>
*(P2)  H      @assert@;A@F12{o}@>Absurd: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(ENEG)@label(ENEG)@\ 
Eliminate a negation.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@not@;A@F12{o}@>
 (P2)  H      @assert@;A@F12{o}@>
*(P3)  H      @assert@;@falsehood@;@>NegElim: D1 P2
Transformation: (P3 D1 ss) ==> (P2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(INEG)@label(INEG)@\ 
Introduce a negation.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;A@F12{o}@>Hyp
 (P2)  H,H1   @assert@;@falsehood@;@>
*(P3)  H      @assert@;@not@;A@F12{o}@>NegIntro: P2
Transformation: (P3 ss) ==> (P2 H1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(PULLNEG)@label(PULLNEG)@\ 
Pull out negation.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(PUSH-NEGATION  [@not@; A@F12{o}])@> 
*(P2)  H      @assert@;@not@; A@F12{o}@>Neg: P1
Restrictions:  (NON-ATOMIC A@F12{o})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(PUSHNEG)@label(PUSHNEG)@\ 
Push in negation.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@not@; A@F12{o}@> 
 (D2)  H      @assert@;`(PUSH-NEGATION  [@not@; A@F12{o}])@>Neg: D1
Restrictions:  (NON-ATOMIC A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)
@End(Description)

@Section(Quantifier Rules)
@Begin(Description)
@begin(group)
@Indexsrule(AB*)@label(AB*)@\ 
Rule to alphabetically change embedded bound variables.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;B@F12{o}@>AB: D1
Restrictions:  (WFFEQ-AB A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(ABE)@label(ABE)@\ 
Rule to change a top-level occurrence of an existentially
 quantified variable.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@exists@;x@F12{a} A@F12{o}@> 
 (D2)  H      @assert@;@exists@;y@F12{a} `(S  y  x@F12{a}  A@F12{o})@>AB: y D1
Restrictions:  (FREE-FOR y@F12{a} x@F12{a} A@F12{o}) (NOT-FREE-IN y@F12{a} A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(ABU)@label(ABU)@\ 
Rule to change a top-level occurrence of a universally quantified
 variable.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;@forall@;y@F12{a} `(S  y  x@F12{a}  A@F12{o})@> 
*(P2)  H      @assert@;@forall@;x@F12{a} A@F12{o}@>AB: x P1
Restrictions:  (FREE-FOR y@F12{a} x@F12{a} A@F12{o}) (NOT-FREE-IN y@F12{a} A@F12{o})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(EGEN)@label(EGEN)@\ 
Rule of Existential Generalization.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(LCONTR  [[@g{l}@;x@F12{a} A@F12{o}] t@F12{a}])@> 
*(P2)  H      @assert@;@exists@;x@F12{a} A@F12{o}@>EGen: t@F12{a} P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(RULEC)@label(RULEC)@\ 
Rule C.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@exists@;x@F12{a} B@F12{o}@> 
 (H2)  H,H2   @assert@;`(LCONTR  [[@g{l}@;x@F12{a} B@F12{o}] y@F12{a}])@>Choose: y
 (D3)  H,H2   @assert@;A@F12{o}@> 
*(P4)  H      @assert@;A@F12{o}@>RuleC: D1 D3
Restrictions:  (IS-VARIABLE y@F12{a}) (NOT-FREE-IN-HYPS y@F12{a}) 
(NOT-FREE-IN y@F12{a} [@exists@;x@F12{a} B@F12{o}]) (NOT-FREE-IN y@F12{a} A@F12{o})
Transformation: (P4 D1 ss) ==> (D3 H2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(UGEN)@label(UGEN)@\ 
Rule of Universal Generalization.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
*(P2)  H      @assert@;@forall@;x@F12{a} A@F12{o}@>UGen: x P1
Restrictions:  (NOT-FREE-IN-HYPS x@F12{a})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(UI)@label(UI)@\ 
Rule of Universal Instantiation.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@forall@;x@F12{a} A@F12{o}@> 
 (D2)  H      @assert@;`(LCONTR  [[@g{l}@;x@F12{a} A@F12{o}] t@F12{a}])@>UI: t D1
Transformation: (pp D1 ss) ==> (pp D2 D1 ss) @End(Verbatim)
@end(group)
@End(Description)
@Section(Substitution Rules)
@Begin(Description)
@begin(group)
@Indexsrule(SUBSTITUTE)@label(SUBSTITUTE)@\ 
Rule to substitute a term for a variable.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(S  t@F12{a}  x@F12{a}  A@F12{o})@>Subst: t  x D1
Restrictions:  (NOT-FREE-IN-HYPS x@F12{a}) (FREE-FOR t@F12{a} x@F12{a} A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss D1) @End(Verbatim)
@end(group)
@End(Description)


@Section(Equality Rules)
@Begin(Description)
@begin(group)
@Indexsrule(EXT=)@label(EXT=)@\ 
Rule of Extensionality.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;@forall@;x@F12{b}.f@F12{ab} x = g@F12{ab} x@> 
*(P2)  H      @assert@;f@F12{ab} = g@F12{ab}@>Ext=: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(EXT=0)@label(EXT=0)@\ 
Rule to derive an equality at type o from an equivalence.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o} @equiv@; R@F12{o}@> 
*(P2)  H      @assert@;P@F12{o} = R@F12{o}@>Ext=: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(LET)@label(LET)@\ 
Rule to produce a new variable which will represent an entire formula during part of a proof.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
(D1)  H      @assert@;A@F12{a} = A@>Refl=
 (D2)  H      @assert@;@exists@;x@F12{a}.x = A@F12{a}@>EGen: x D1
 (H3)  H,H3   @assert@;x@F12{a} = A@F12{a}@>Choose: x
 (P4)  H,H3   @assert@;C@F12{o}@>
*(P5)  H      @assert@;C@F12{o}@>RuleC: D2 P4
Restrictions:  (NOT-FREE-IN-HYPS x@F12{a}) (NOT-FREE-IN x@F12{a} C@F12{o})
Transformation: (P5 ss) ==> (P4 ss D1 D2 H3) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(SUBST=)@label(SUBST=)@\ 
Substitution of Equality. Performs whichever of the @t(SUBST=L) and @t(SUBST=R) 
rules is appropriate.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o}@>
*(D2)  H      @assert@;s@F12{a} = t@F12{a}@>
 (D3)  H      @assert@;R@F12{o}@>Sub=: P1 D2
Restrictions:  (SAME-MODULO-EQUALITY P@F12{o} R@F12{o} s@F12{a} t@F12{a})
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1 D2) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(SUBST=L)@label(SUBST=L)@\ 
Substitution of Equality.  Replaces some occurrences of the left hand
side by the right hand side.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o}@> 
*(D2)  H      @assert@;s@F12{a} = t@F12{a}@> 
 (D3)  H      @assert@;R@F12{o}@>Subst=: P1 D2
Restrictions:  (R-PRIME-RESTR s@F12{a} P@F12{o} t@F12{a} R@F12{o})
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1 D2) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(SUBST=R)@label(SUBST=R)@\ 
Substitution of Equality.  Replaces some occurrences of the right
hand side by the left hand side.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o}@> 
*(D2)  H      @assert@;t@F12{a} = s@F12{a}@> 
 (D3)  H      @assert@;R@F12{o}@>Subst=: P1 D2
Restrictions:  (R-PRIME-RESTR s@F12{a} P@F12{o} t@F12{a} R@F12{o})
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1 D2) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(SUBST-EQUIV)@label(SUBST-EQUIV)@\ 
Substitution of Equivalence. Useable when R and P are the same modulo
the equivalence s EQUIV t.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o}@> 
*(D2)  H      @assert@;s@F12{o} @equiv@; t@F12{o}@> 
 (D3)  H      @assert@;R@F12{o}@>Sub-equiv: P1 D2
Restrictions:  (SAME-MODULO-EQUALITY P@F12{o} R@F12{o} s@F12{o} t@F12{o})
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1 D2) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(SYM=)@label(SYM=)@\ 
Rule of Symmetry of Equality.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{a} = B@F12{a}@> 
*(P2)  H      @assert@;B@F12{a} = A@F12{a}@>Sym=: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)
@End(Description)

@Section(Definition Rules)
@Begin(Description)
@begin(group)
@Indexsrule(EDEF)@label(EDEF)@\ 
Rule to eliminate first definition, left to right.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(INST-DEF  A@F12{o})@>Defn: D1
Restrictions:  (CONTAINS-DEFN A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(EQUIV-WFFS)@label(EQUIV-WFFS)@\ 
Rule to assert equivalence of lines up to definition.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;P@F12{o}@> 
 (D2)  H      @assert@;R@F12{o}@>EquivWffs: D1
Restrictions:  (WFFEQ-DEF P@F12{o} R@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(IDEF)@label(IDEF)@\ 
Rule to introduce a definition.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(INST-DEF  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Defn: P1
Restrictions:  (CONTAINS-DEFN A@F12{o})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)
@End(Description)

@Section(Lambda Conversion Rules)
@Begin(Description)
@begin(group)
@Indexsrule(LAMBDA*)@label(LAMBDA*)@\ 
Rule to infer a line from one which is equal up to lambda conversion
using both beta and eta rules and alphabetic change of bound variables.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@>
 (D2)  H      @assert@;B@F12{o}@>Lambda=: D1
Restrictions:  (WFFEQ-AB-LAMBDA A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(BETA*)@label(BETA*)@\ 
Rule to infer a line from one which is equal up to lambda conversion
using beta rule (but NOT eta rule) and alphabetic change of bound variables.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}
@> 
 (D2)  H      @assert@;B@F12{o}
@>Beta Rule: D1
Restrictions:  (WFFEQ-AB-BETA A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(ETA*)@label(ETA*)@\ 
Rule to infer a line from one which is equal up to lambda conversion
using eta rule (but NOT beta rule) and alphabetic change of bound variables.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}
@> 
 (D2)  H      @assert@;B@F12{o}
@>Beta Rule: D1
Restrictions:  (WFFEQ-AB-ETA A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(LCONTR*)@label(LCONTR*)@\ 
Rule to put an inferred line into Lambda-normal form using both 
beta and eta conversion.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(LNORM  A@F12{o})@>Lambda: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(LCONTR*-BETA)@label(LCONTR*-BETA)@\ 
Rule to put an inferred line into beta-normal form.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(LNORM-BETA  A@F12{o})@>Lambda: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(LCONTR*-ETA)@label(LCONTR*-ETA)@\ 
Rule to put an inferred line into eta-normal form.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(LNORM-ETA  A@F12{o})@>Lambda: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(LEXPD*)@label(LEXPD*)@\ 
Rule to put a planned line into Lambda-normal form using both 
beta and eta conversion.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(LNORM  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Lambda: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(LEXPD*-BETA)@label(LEXPD*-BETA)@\ 
Rule to put a planned line into beta-normal form.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(LNORM-BETA  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Lambda: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)

@begin(group)
@Indexsrule(LEXPD*-ETA)@label(LEXPD*-ETA)@\ 
Rule to put a planned line into eta-normal form.

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(LNORM-ETA  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Lambda: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@end(group)
@End(Description)



