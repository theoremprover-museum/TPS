@Appendix(More Editor Commands) @label(more-editor)

The most commonly used editor commands were listed in Section
@ref(locating). Here we list the rest of the commands available in the
editor.  Note that some of the commands available within the editor
are needed for type theory, and may be ignored by the user who is
interested only in first-order logic.


@Section(Labels)
@Begin(Description)

@T{<Edn>@indexedop(DELWEAK) @i{label}}@\ 
Replace all occurrences of the label in the current edwff by the wff it represents.

@T{<Edn>@indexedop(DW)}@\ 
Replace a top-level occurrence of label by the wff it represents.

@T{<Edn>@indexedop(DW*)}@\ 
Replace all labels in a wff by the wffs represented by them.

@T{<Edn>@indexedop(NAME) @i{label}}@\ 
Assign a label to the edwff, and replace the edwff with this label.

@T{<Edn>@indexedop(RW) @i{label}}@\ 
Makes current edwff the new value of label (which must already exist).@End(Description)

@begin(comment)
@Section(Substitution)
@Begin(Description)
@T{<Edn>@indexedop(AB) @i{newvar}}@\ 
Alphabetic change of variable at top-level.

@T{<Edn>@indexedop(IB) @i{term}}@\ 
Instantiate a top-level universal or existential binder with a term.

@T{<Edn>@indexedop(REW-EQUIV) @i{gwff}}@\ 
Replaces each equivalence in the gwff with a conjunction of implications.

@T{<Edn>@indexedop(RP) @i{rep-sym} @i{rep-by}}@\ 
Replace one occurrence of a symbol rep-sym (such as AND) by a predefined
equivalent wff involving the symbol rep-by (such as [@g(l) p @g(l) q.~.p @IMPLIES ~q]). In this
example, rep-sym is AND and rep-by is IMPLIES. To see if a symbol can be
replaced using this command, enter HELP symbol; any such replacements will be
listed under the heading `Replaceable Symbols'.

@T{<Edn>@indexedop(RPALL) @i{rep-sym} @i{rep-by}}@\ 
Replace all occurrences of a symbol by a predefined equivalent wff.

@T{<Edn>@indexedop(SUB) @i{gwff}}@\ 
Replaces the current wff by the wff supplied.

@T{<Edn>@indexedop(SUBST) @i{term} @i{var}}@\ 
Substitute a term for the free occurrences of variable in a gwff.
Bound variables may be renamed, using the function in the global
variable REN-VAR-FN.


@T{<Edn>@indexedop(SUBSTYP) @i{typevar} @i{typesym}}@\ 
Substitute typevar with typesym.@End(Description)
@end(comment)

@Section(Basic Abbreviations)
@Begin(Description)
@T{<Edn>@indexedop(EXPAND=)}@\ 
Instantiates all equalities.

@T{<Edn>@indexedop(INST) @i{gabbr}}@\ 
Instantiate all occurrences of an abbreviation.
The occurrences will be lambda-contracted, but not lambda-normalized.

@T{<Edn>@indexedop(INST1)}@\ 
Instantiate the first abbreviation, left-to-right.

@T{<Edn>@indexedop(INSTALL) @i{exceptions}}@\ 
Instantiate all definitions, except the ones specified
in the second argument.@End(Description)

@Section(Lambda-Calculus)
@Begin(Description)
@T{<Edn>@indexedop(ABNORM)}@\ 
Convert the gwff to alphabetic normal form. 

@T{<Edn>@indexedop(ETAB) @i{gwff}}@\ 
Eta-expands until original wff is part of a wff of base type.

@T{<Edn>@indexedop(ETAC)}@\ 
Reduces [lambda x.fx] to f at the top.

@T{<Edn>@indexedop(ETAN)}@\ 
Reduces [lambda x.fx] to f from inside out.

@T{<Edn>@indexedop(ETAX)}@\ 
Performs one step of eta expansion of the gwff.

@T{<Edn>@indexedop(LEXP) @i{var} @i{term} @i{occurs}}@\ 
Converts the wff into the application of a function to the term.
The function is formed by replacing given valid occurrences of a term
with the variable and binding the result.

@T{<Edn>@indexedop(LNORM)}@\ 
Put a wff into lambda-normal form; equivalent to @indexedop(LNORM-BETA) followed 
by @indexedop(LNORM-ETA).

@T{<Edn>@indexedop(LNORM-BETA)}@\ 
Put a wff into beta-normal form.

@T{<Edn>@indexedop(LNORM-ETA)}@\ 
Put a wff into eta-normal form (exactly equivalent to @indexedop(ETAN)).

@T{<Edn>@indexedop(RED)}@\ 
Lambda-contract a top-level reduct.
Bound variables may be renamed.@End(Description)

@begin(comment)
@Section(Negation movers)
@Begin(Description)
@T{<Edn>@indexedop(NEG)}@\ 
Negates current wff, erasing double negations.

@T{<Edn>@indexedop(NNF)}@\ 
Return the negation normal form of the given wff.

@T{<Edn>@indexedop(PULL-NEG)}@\ 
Pulls negations out one level.

@T{<Edn>@indexedop(PUSH-NEG)}@\ 
Pushes negation through the outermost operator or quantifier.@End(Description)
@end(comment)

@Section(Quantifier Commands)
@begin(description)
@T{<Edn>@indexedop(DB)}@\ 
Delete the leftmost binder in a wff.

@T{<Edn>@indexedop(EP)}@\ 
Delete all essentially existential quantifiers in a wff.

@T{<Edn>@indexedop(OP)}@\ 
Delete the leftmost binder in a wff.
Delete all essentially existential quantifiers in a wff.
@end{description}

@Section(Embedding Commands)
There are a range of embedding commands, which take the current edwff and embed it
below a quantifier or connective. These commands all begin @t(MBED), and then have a 
suffix denoting the quantifier or connective, and a further suffix (if appropriate)
denoting whether the current wff is to become the left or right side of the new formula.

They are:
@begin(description)
@T{<Edn>@indexedop(MBED-AL)}@\ Embed the current wff below AND, on the left side.

@T{<Edn>@indexedop(MBED-AR)}@\ Embed the current wff below AND, on the right side.

@T{<Edn>@indexedop(MBED-E)}@\ Embed the current wff below an EXISTS quantifier.

@T{<Edn>@indexedop(MBED-E1)}@\ Embed the current wff below an EXISTS1 quantifier.

@T{<Edn>@indexedop(MBED-F)}@\ Embed the current wff below a FORALL quantifier.

@T{<Edn>@indexedop(MBED-IL)}@\ Embed the current wff below IMPLIES, on the left side.

@T{<Edn>@indexedop(MBED-IR)}@\ Embed the current wff below IMPLIES, on the right side.

@T{<Edn>@indexedop(MBED-L)}@\ Embed the current wff below a LAMBDA binder.

@T{<Edn>@indexedop(MBED-OL)}@\ Embed the current wff below OR, on the left side.

@T{<Edn>@indexedop(MBED-OR)}@\ Embed the current wff below OR, on the right side.

@T{<Edn>@indexedop(MBED-QL)}@\ Embed the current wff below EQUIV, on the left side.

@T{<Edn>@indexedop(MBED-QR)}@\ Embed the current wff below EQUIV, on the right side.

@T{<Edn>@indexedop(MBED=L)}@\ Embed the current wff below =, on the left side.

@T{<Edn>@indexedop(MBED=R)}@\ Embed the current wff below =, on the right side.
@end{description}

@Section(Changing and Recursive Changing Commands)
Many of these commands operate only on the current wff, but have a recursive form 
that will perform the same operation on the current wff and all of its subwffs.

@begin(description)
@T{<Edn>@indexedop(ASRB)}@\ 
Absorbs unnecessary @t(AND) and @t(OR) connectives; see the help message for examples.
This command also has a recursive form, @t(@indexedop(ASRB*)).

@T{<Edn>@indexedop(ASSL)}@\ 
Applies the left associative law, changing @t<(A * (B * C))> to @t<( (A * B) * C)>.
This command also has a recursive form, @t(@indexedop(ASSL*)).

@T{<Edn>@indexedop(ASSR)}@\ 
Applies the right associative law, changing @t<( (A * B) * C)> to @t<(A * (B * C))>
This command also has a recursive form, @t(@indexedop(ASSR*)).

@T{<Edn>@indexedop(CMRG)}@\ 
Deletes the constants @t(TRUTH) and @t(FALSEHOOD) from a wff; see the help message
for examples.
This command also has a recursive form, @t(@indexedop(CMRG*)).

@T{<Edn>@indexedop(CMUT)}@\ 
Applies the commutative laws to a formula; see the help message for examples.
This command also has a recursive form, @t(@indexedop(CMUT*)).

@T{<Edn>@indexedop(CNTOP) @i(connective-or-quantifier)}@\ 
Changes the outermost connective or quantifier to that specified.

@T{<Edn>@indexedop(DIST-CTR)}@\ 
Applies the laws of distributivity to a wff in the contracting direction; see
the help message for examples.
This command also has a recursive form, @t(@indexedop(DIST-CTR*)).

@T{<Edn>@indexedop(DIST-EXP)}@\ 
Applies the laws of distributivity to a wff in the expanding direction; see
the help message for examples.
This command also has a recursive form, @t(@indexedop(DIST-EXP*)).

@T{<Edn>@indexedop(DNEG)}@\ 
Removes a double negation from a wff.
This command also has a recursive form, @t(@indexedop(DNEG*)).

@T{<Edn>@indexedop(MRG)}@\ 
Merges redundant connectives in a wff; see help message for examples.
This command also has a recursive form, @t(@indexedop(MRG*)).

@T{<Edn>@indexedop(PMUT)}@\ 
Permutes the two components of an infix operator.
This command also has a recursive form, @t(@indexedop(PMUT*)).

@T{<Edn>@indexedop(SUBEQ)}@\ 
Reduces an equivalence to a conjunction of implications.
This command also has a recursive form, @t(@indexedop(SUBEQ*)).

@T{<Edn>@indexedop(SUBIM)}@\ 
Reduces an implication to a disjunction.
This command also has a recursive form, @t(@indexedop(SUBIM*)).
@end(description)

@Section(Miscellaneous)
@Begin(Description)
@T{<Edn>@indexedop(CNF)}@\ 
Find the conjunctive normal form of a wff.

@T{<Edn>@indexedop(HEAD)}@\ 
Finds the head of a gwff.

@T{<Edn>@indexedop(HVARS)}@\ 
Returns all the head variables of a gwff.

@T{<Edn>@indexedop(MIN-SCOPE)}@\ 
Minimises the scope of quantifiers in a gwff.

@End(Description)

@Section(Wellformedness)
@begin(description)
@T{<Edn>@indexedop(DUPW) @i(connective)}@\ 
Duplicates a wff across a connective.

@T{<Edn>@indexedop(EDILL)}@\ 
Finds a minimal ill-formed subformula.

@T{<Edn>@indexedop(ILL)}@\ 
Returns a list of messages, each describing the error 
in a minimal ill-formed subformula.

@T{<Edn>@indexedop(TP)}@\ 
Returns the type of a gwff.

@T{<Edn>@indexedop(WFFP)}@\ 
Tests for a gwff (general well-formed formula).

@end(description)

@Section(Saving and Recording Wffs)
Saving wffs into a file is governed by the two flags @Indexflag(PRINTEDTFILE) 
and @Indexflag(PRINTEDTFLAG), which determine the name of the file being written
and whether or not wffs are currently being written to it, respectively.

@begin(description)
@T{<Edn>@indexedop(O)}@\ 
Toggles recording on and off (i.e. inverts the current value
of @t(PRINTEDTFLAG)).

@T{<Edn>@indexedop(REM) @i{string}}@\ 
Writes a comment into the current output file.

@T{<Edn>@indexedop(SAVE) @i{label}}@\ 
Saves a gwff by appending it to the file @t(savedwffs).
@end(description)
