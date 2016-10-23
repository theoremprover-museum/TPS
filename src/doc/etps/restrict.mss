@Appendix(Wff Operations)@label(wffops)

This is a list of those wffops mentioned in Chapter @ref(inference); you can
get a complete list of wffops by typing @t(ENVIRONMENT) and then @t(WFFOP).

@Section(Equality between Wffs)
@Begin(Description)
@T{(@indexother(WFFEQ-AB) @i{wff1} @i{wff2})}@\ 
Tests for equality modulo alphabetic change of bound variables.

@T{(@indexother(WFFEQ-AB-BETA) @i{wff1} @i{wff2})}@\ 
Tests for equality modulo alphabetic change of bound variables
and beta-conversion.

@T{(@indexother(WFFEQ-AB-ETA) @i{wff1} @i{wff2})}@\ 
Tests for equality modulo alphabetic change of bound variables
and eta-conversion.

@T{(@indexother(WFFEQ-AB-LAMBDA) @i{wff1} @i{wff2})}@\ 
Tests for equality modulo alphabetic change of bound variables
and both beta- and eta-conversion.

@T{(@indexother(WFFEQ-DEF) @i{wff1} @i{wff2})}@\ 
Tests for equality modulo definitions, lambda conversion and alphabetic 
change of bound variables.
@End(Description)

@Section(Predicates on Wffs)
@Begin(Description)
@T{(@indexother(FREE-FOR) @i{term} @i{var} @i{inwff})}@\ 
Tests whether a term is free for a variable in a wff.

@T{(@indexother(IS-VARIABLE) @i{gwff})}@\ 
Tests whether a wff is a logical variable.

@T{(@indexother(NON-ATOMIC) @i{gwff})}@\ 
Tests whether a wff is not atomic, that is, negated, quantified or
the result of joining two wffs with a binary connective.

@T{(@indexother(NOT-FREE-IN) @i{gvar} @i{inwff})}@\ 
Tests whether a variable is not free in a wff.

@T{(@indexother(NOT-FREE-IN-HYPS) @i{gvar})}@\ 
Tests whether a variable is not free in the set of hypotheses of a rule.

@t{(@indexother(R-PRIME-RESTR) @i{term1} @i{wff1} @i{term2} @i{wff2})}@\ 
Verifies that @i{wff2} follows from @i{wff1} by 
Rule R' using equality @i{term1}@_=@_@i{term2}.

@t{(@indexother(SAME-MODULO-EQUALITY) @i{wff1} @i{wff2} @i{term1} @i{term2})}@\ 
Verifies that @i{wff2} follows from @i{wff1} by Rule R' (possibly
iterated) using the equality @i{term1=term2}.

@End(Description)

@Section(Substitution)
@Begin(Description)
@T{(@indexother(S) @i{term} @i{var} @i{inwff})}@\ 
Substitute a term for the free occurrences of variable in a gwff.
@End(Description)

@Section(Basic Abbreviations)
@Begin(Description)
@t{(@indexother(CONTAINS-DEFN) @i{wff})}@\ 
Tests whether the argument contains a definition.

@T{(@indexother(INST-DEF) @i{inwff})}@\ 
Instantiate the first abbreviation, left-to-right.
@End(Description)

@Section(Lambda-Calculus)
@Begin(Description)
@t{(@indexother(LCONTR) @i{reduct})}@\ 
Lambda-contract a top-level reduct.

@t{(@indexother(LNORM) @i{wff})}@\ 
Put a wff into lambda-normal form (equivalent to @t(LNORM-BETA) followed by
@t(LNORM-ETA)).

@t{(@indexother(LNORM-BETA) @i{wff})}@\ 
Put a wff into beta-normal form.

@t{(@indexother(LNORM-ETA) @i{wff})}@\ 
Put a wff into eta-normal form.
@end(description)

@Section(Negation movers)
@Begin(Description)
@T{(@indexother(PUSH-NEGATION) @i{gwff})}@\ 
Pushes negation through the outermost operator or quantifier.
@End(Description)
