@ChapterPh(Inference Rules) @label(inference)
@Section(How to Read Inference Rules)@label(InfeRead)

The user works within a @i(proof-outline), which is a sequence of lines.
Each line is either a hypothesis, a consequence of lines with lower numbers
or an unjustified planned line.
In general, a line of the proof has the following form:
@Begin(Verbatim)
(@i{n}) @i(H)@-{1},...,@i(H)@-{m} ! @i{assertion} @>@i{justification}: @i(wffs) @I(l)@-{1} ... @I( l)@-{k}
@End(Verbatim)
@i(n) is a number which serves as a label for the line.
Each of the @i(H)@-{i}'s is the number of a line asserting a hypothesis.
@T(!) stands for the turnstyle @Assert, and @I(l)@-{1} ... @I( l)@-{k} are
the numbers of the lines which are used to justify this line.

Every description of a logical inference rule states that certain
lines of a proof may be inferred from certain other lines, provided that
certain restrictions are satisfied, and notes the change which the rule
effects on the proof @i(status) (see Section @ref[proofstatus]).
Inference commands apply inference rules, and they may be used in various ways
to complete the proof.
They may generate either new planned or sponsoring lines, or close up a proof
by justifying planned lines with sponsoring lines. However, there is usually
a most natural way to use a rule. This is indicated in the statement of
the rule by labelling those lines which are usually sponsors, before or after
the rule is applied, with a @T(D) (for deduced) followed by a number.
Similarly, those lines the rule expects to be planned lines are labelled
with a @t(P) followed by a number. 

The @i(transformation) statement in an inference rule description indicates the change
in the proof status effected by the most natural use of the rule.@index(status)
The lists before the arrow @t(==>) are matched against the initial status;
those after the arrow describe what the new status should be.
The first element of a list is always a planned line and the remaining elements
are its sponsors. Each element of a list is either a label for a line in
the rule, @t(pp) or @t(ss). The symbol @t(pp) refers to all matched
planned lines and the symbol @t(ss) to all (other) sponsoring lines for each
of the matched planned lines. 

Certain lines in a rule
description are expected to exist in the proof before the rule is applied;
these are indicated by an asterisk.
If a line does not already exist when you apply an inference command, and its
corresponding line in the rule asserts a wff which cannot be formed from
the wffs in rule lines corresponding to existing proof lines, then the wff
asserted by that new line will have to be entered.
Thus, in order to avoid typing in long wffs, you should try to organize your
work (often by working up from the bottom) so that such lines will be
introduced before they are needed.

@ETPS automatically applies the metatheorem that if 
@wt[@scriptH@-{1} @ASSERT A] and @wt[@scriptH@-(1) @SUBSET @scriptH@-(2)]
then @wt[@scriptH@-(2) @ASSERT A], so that normally
you do not have to worry about expanding sets of hypotheses before applying
inference rules.


Some rules use @i(wffops), operations on well-formed formulae, in their
descriptions. For example, a rule might form the assertion of one line by
substituting a term, @t(t), for all free occurrences of a variable, @t(x),
in a wff, @t(A), asserted in another line. The new assertion would then be
@wt[`(S t x A)], where @t(S) is the wffop performing the substitution.
(The backquote tells @ETPS that the application of a wffop is being asserted,
and not a wff.) However, this substitution will only be allowed when @t(t) is
free for @t(x) in @t(A). Thus, the form @wt[(FREE-FOR t x A)] would appear in
the restrictions for the rule. @t(FREE-FOR) is the wffop which checks that
@t(t) is free for @t(x) in @t(A). A catalogue of the wffops used in inference
rules is provided in Appendix @ref(wffops). It is included in this manual only
to help you understand the descriptions of the inference rules.

