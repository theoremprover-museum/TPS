@part(typetheory, root "ETPS.mss")
@chapterPH(Type Theory in @HETPS)@label(hol)

@Section(Using @HETPS for Type Theory)

@ETPS can be used for higher-order logic as well as for first-order logic.
Wffs of type theory  are written
essentially as they are expressed in the logic book.  There are
a few additional inference rules and the parsing and printing of wffs
is slightly different, while everything else described in the previous
chapters is still valid.
@begin(comment)
@Subsection(Typing Wffs of Higher-Order)

In higher-order logic wffs can still be entered as strings in
double quotes.  Strings may still contain @wt{<Return>}'s for
formatting purposes.  Case does matter, i.e.,  @T(x)
and @T(X) represent two distinct wffs.
In contrast to first-order logic, identifiers do not just consist of
one letter.  Therefore @i[identifiers have to be separated by spaces]!
@end(comment)

@comment<
Also note that @ETPS assumes nothing about the types of arguments to
inference rules, even though this could be done.
For example, while in first-order logic substitution terms are
generally of type @iota, this is not the case in higher-order logic.
Therefore, the types of terms and wffs must be specified in arguments.
It is no longer correct to simply reply @wt{"P x"}, but one must type
 @wt{"P(OI) x"}, as explained below.  The default type is @iota, so
@wt{x} by itself still stands for @wt{x@subiota}.>

@Subsection(Types in Higher-Order Logic)@Label(holwffs)
There is a very direct translation from the way types are represented in
the logic book and the way types are represented in @ETPS.  Since Greek
subscripts are not available on most terminals, the Greek letters are
transliterated to uppercase Roman letters.  The most commonly used types are
@Begin(Format)
@TabDivide(6)
@T(I) for @iota@\ @T(O) for @omicron@\ @T(S) for @sigma
@T(A) for @alpha@\ @T(B) for @beta@\ @T(C) for @gamma
@End(Format)
The same conventions for parentheses are used as in the logic book, i.e.,
association to the left is assumed.  Note, however, that the outermost pair of
parentheses must be preserved in order to distinguish types from identifiers.

Types are entered as strings, such as "@t<(O(OA))>"; typically they are substrings
of a string representing a wff and serve to give type information about the
symbols in that wff, e.g., "@t<p(O(OA))>". If entered separately, the opening and
closing double-quotes must still be provided. Indeed, all of the string input
rules apply; for example, carriage returns may be embedded.  For more 
examples of entering typed wffs, see Section @ref(holwffex).

@ETPS has a powerful type-inference mechanism which makes explicit
typing mostly unnecessary within wffs.  Often the type of one variable
is enough to uniquely determine the type of every identifier in a
wff. Within a wff, all occurrences of a variable are assumed to have
the same type, unless the contrary is specifically indicated. If the
type of a variable remains undetermined after all other type
information has been used, @iota is assumed.  Take care to specify
types if you use ``type variables'' like @alpha.  Also note that
type-inference is local, i.e., the type of an identifier is determined
anew for each wff parsed by @ETPS.

@SubSection(Abbreviations and Special Symbols)
@ETPS allows polymorphic abbreviations.  These are abbreviations with variable
type, which may have multiple occurrences  with different types in the same wff.  Since
their special symbols cannot be typed on most keyboards, there is a
``long'' form of each of them, which has to be used in @ETPS.  The
following is a temporary list of special symbols, the binary operators
ordered according to their binding priority and abbreviations marked with
@i[(abb)].

@Begin(Format, ScriptPush Yes)
@TabDivide(5)
@begin(group)
Improper symbols
@lambda@\ @T(@ITT(LAMBDA))@\ The @lambda-binder
@forall@\ @T(@ITT(FORALL))
@exists@\ @T(@ITT(EXISTS))
@exists@;@-{1}@;@\ @T(@ITT(EXISTS1))@\ @t{@g{S}@;@+{1}@;@F12{o(oa)}.@g{l}@;x@F12{a} A@F12{o}}
@\ @T(@ITT(EXISTSN))@\ @t{@exists@;z@F12{s}.NAT z @and@; A@F12{o}}
@\ @T(@ITT(FORALLN))@\ @t{@forall@;z@F12{s}.NAT z @implies@; A@F12{o}}
@t{@g{m}@;}@\ @T(@ITT(MU-BIND))@\ @t{mu@F12{s(os)}.@g{l}@;z@F12{s} A@F12{o}}
@\ @T(@ITT(THAT))@\ @t{@g{i}@;.@g{l}@;z@F12{c} A@F12{o}}

@end(group)

Unary operators with equal binding priority (except @T(NOT) which has the least binding priority):

@T(~)@\ @t<@ITT(NOT)>
@indexentry[key="%3",Entry="~",number]

@T(%)@ @ @ @I[(abb)]@\ @t<@ITT(%)>@~
@\ @t{@g{l}@;f@F12{ab} @g{l}@;x@F12{ob} @g{l}@;z@F12{a} @EXISTS@;t@F12{b}.x t @AND@; z = f t}.  
@comment<@\ @\ Do not to use @T(#) which is special to MacLisp.>

@T(@powerset@F12{o(oa)(oa)})@ @ @ @I[(abb)]@\ @T<@ITT(POWERSET)>@~
@\ @t[@g{l}p@f12{oa}@g{l}q@f12{oa}.q @subset p]

@T(@setintersect)@ @ @ @I[(abb)]@\ @T<@ITT(SETINTERSECT)>@\ @t[@g(l)s@F12{o(oa)}@g{l}x@f12(a)@forall p@f12{oa}.s p @implies p x]
@\ @\ Intersection of a collection of sets

@T(@setunion)@ @ @ @I[(abb)]@\ @T<@ITT(SETUNION)>@~
@\ @T[@g{l}s@F12{o(oa)}@g{l}x@F12(a)@exists@;p@f12{oa}.s p @and p x]
@\ @\ Union of a collection of sets

@t{@falsehood@;}@\ @T(@ITT(FALSEHOOD))

@t{@truth@;}@\ @T(@ITT(TRUTH))
@indexentry[key="%2",Entry="@truth",number]
@indexentry[key="%1",Entry="@falsehood",number]

Binary operators, strongest binding first:

@T(@intersect)@ @ @ @I[(abb)]@\ @T<@ITT(INTERSECT)>@~
@\ @t[@G{l}p@f12{oa}@g{l}q@f12{oa}@g{l}x@f12{a}.p x @and q x]
@\ @\ Intersection of two sets

@T(@union)@ @ @ @I[(abb)]@\ @T<@ITT(UNION)>@~
@\ @t[@G{l}p@f12{oa}@g{l}q@f12{oa}@g{l}x@f12{a}.p x @or q x]
@\ @\ Union of two sets

@T(@subset)@ @ @ @I[(abb)]@\ @T<@ITT(SUBSET)>@~
@\ @t[@G{l}p@f12{oa}@g{l}q@f12{oa}@forall x@f12{a}.p x @implies q x]

@T(=)@f12(oaa)@\ @T<=>@\ Equality at type @g(a)
@indexentry[key="%4",Entry="=",number]

@begin(comment)
@T(=)@ @ @ @I[(abb)]@\ @T(=)@~
@\ @t[@g{l}x@f12{a}@g{l}y@f12{a}@forall q@f12{oa}. q x @implies q y]
@\ @\ Note that for some exercises this has been replaced by @wt{=S}.
@end(comment)

@T(=S)@ @ @ @I[(abb)]@\ @T<@ITT(SETEQUIV)>@~
@\ @t[@G{l}p@f12{oa}@g{l}q@f12{oa}.p @subset q @and. q @subset p]
@\ @\ Equality between sets.

@T(=S)@ @ @ @I[(abb)]@\ @T(@ITT{EQUIVS})
@\ @t{@g{l}@;P@F12{oa} @g{l}@;R@F12{oa} @forall@;x@F12{a}.P x @equiv@; R x}
@\ @\ Elementwise equality between sets. This is equivalent to
@\ @\ equality between sets, if one assumes extensionality.

@T(<=)@ @ @ @I[(abb)]@\ @T<<=>@\ @t{@g{l}@;x@F12{s} @g{l}@;y@F12{s} @forall@;p@F12{os}.p x @and@; @forall@;z@F12{s} [p z @implies@; p.SUCC@F12{ss} z] @implies@; p y}
@\ @\ Less than or equal to, for natural numbers.
@indexentry[key="%5",Entry="<=",number]

@AND@\ @T<@ITT(AND)>@\ 

@OR@\ @T<@ITT(OR)>@\ 

@IMPLIES@\ @T<@ITT(IMPLIES)>@\ 

@EQUIV@ @ @ @I[(abb)]@\ @T<@ITT(EQUIV)>@\ Equality at type @t(o)
@begin(comment)
@\ @t[@g{l}p@f12{o}@g{l}q@f12{o}.[p @implies q]@and. q @implies p]
@\ @\ Propositional equivalence.  Note that this is an abbreviation
@\ @\ and will convert to a conjunction of implications.
@end(comment)

Other abbreviations:

Conditional@\ @T<@ITT{COND}>
@\ @t{@g{l}@;x@F12{c} @g{l}@;y@F12{c} @g{l}@;p@F12{o} THAT q@F12{c}.p @and@; x = q @or@; @not@;p @and@; y = q}

Equipollence@\ @T<@ITT{EQP}>
@\ @t(@G{l}p@f12{ob}@g{l}q@f12{oa}@exists@;s@f12{ab}.@forall@;x@f12{b})@~
@t([p x @implies q.s x])@~
@t(@and @forall@;y@f12{a}.q y @implies @exists@f11{1}x@f12{b}.p x @and y)@~
@t( = s x)

Zero@\ @T<@ITT(ZERO)>@\ @t<[@g{l}p@f12{oi}.@not@exists@;x@f12{i}p x]>

Successor@\ @t<@ITT{SUCC}>@~
@\ @t[@g{l}n@f12{o(oi)}@g{l}p@f12{oi}@exists x@f12{i}.p x @and n@~
[@g{l}t@f12{i}.t @f2{=} x @and p t]]

One@\ @t<@ITT{ONE}>@\ @t(SUCC@f12{ss}O@f12{s})

Finite@\ @t<@ITT{FINITE}>@\ @t{@g{l}@;p@F12{oi} @exists@;n@F12{o(oi)}.NAT n @and@; n p}

@t{@g{m}@;}@\ @t<@ITT{MU}>
@\ @t{@g{l}@;p@F12{os} THAT x@F12{s}.NAT x @and@; p x @and@; FORALLN y@F12{s}.p y @implies@; x <= y}

Natural No.@\ @t<@ITT{NAT}>
@\ @t{@g{l}@;n@F12{o(oi)} @forall@;p@F12{os}.p ZERO@F12{s} @and@; @forall@;x@F12{s} [p x @implies@; p.SUCC@F12{ss} x] @implies@; p n}

NC@\ @t<@ITT{NC}>@\ @t{@g{l}@;u@F12{o(ob)} @exists@;p@F12{ob}.u = @eqp@;@F12{o(ob)(ob)} p}

Recursion@\ @t<@ITT{RECURSION}>
@\ @t{@g{l}@;h@F12{sss} @g{l}@;g@F12{s} @g{l}@;n@F12{o(oi)} THAT m@F12{s} @forall@;w@F12{oss}.w ZERO@F12{s} g @and@; @forall@;x@F12{s} @forall@;y@F12{s} [w x y @implies@; 
@\   w [SUCC@F12{ss} x].h x y] @implies@; w n m}

@t(@CapSigma@+{1})@\ @t<@ITT[SIGMA1]>@~
@\ @t([@g{l}p@f12{oa}@exists@;y@f12{a}.p@f12{oa} = .= y])

@t{@scriptu@;}@\ @t<@ITT(UNITSET)>@\ @t{@g{l}@;x@F12{a} @g{l}@;y@F12{a}.x = y}

@End(Format)

@SubSection(Some Examples of Higher-Order Wffs)@label(holwffex)

Here are some examples of higher-order wffs.  The first line shows how
the formula is printed in the logic book, the second line shows
how it could be entered into @ETPS as a string, and the third line shows how @ETPS would
print it with type symbols (for example with the @T(PWTYPES) command).  Look
at these carefully and make sure you understand how to type in wffs
of higher-order logic.

@Begin(Verbatim)
@begin(group)
@Begin(Verbatim,Spacing=1.5)
@exists@;f@F12[i(oi)]@forall@;S@F12[oi].@exists@;x@F12[i][S x] @implies S.f S
@End(Verbatim)
"EXISTS f FORALL S. EXISTS x S x IMPLIES S . f S"
EXISTS f(I(OI)) FORALL S(OI).EXISTS x(I)[S x] IMPLIES S.f S
@end(group)

@Begin(Verbatim,Spacing=1.5)
@exists@;f@F12[oi(o(oi))]@forall@;S@F12[o(oi)].@exists@;x@F12[oi][S x] @implies S.f S
@End(Verbatim)
"EXISTS f(OI(O(OI))) FORALL S. EXISTS x S x IMPLIES S.f S"
EXISTS f(OI(O(OI))) FORALL S(O(OI)).EXISTS x(OI)[S x] IMPLIES S.f S

@Begin(Verbatim,Spacing=1.5)
@exists@;f@F12[a(oa)]@forall@;S@F12[oa].@exists@;x@F12[a][S x] @implies S.f S
@End(Verbatim)
"EXISTS f FORALL S. EXISTS x(A) S x IMPLIES S.f S"
EXISTS f(A(OA)) FORALL S(OA).EXISTS x(A)[S x] IMPLIES S.f S

@Begin(Verbatim,Spacing=1.5)
p@F12[o] @equiv q@F12[o] @equiv [@G(l)r@F12[o]@G(l)s@F12[o].[r @implies s] @and.s @implies r] p q
@End(Verbatim)
"[p EQUIV q] EQUIV . [LAMBDA r LAMBDA s . [r IMPLIES s] AND .s IMPLIES r] p q"
      p(O) EQUIV q(O)
 EQUIV[LAMBDA r(O) LAMBDA s(O).[r IMPLIES s] AND.s IMPLIES r] p q

@Begin(Verbatim,Spacing=1.5)
@forall@;A@F12[o]@exists@;f@F12[oi]@forall@;P@F12[o(oi)].P[@G(l)p@F12[i] A] @equiv P f
@End(Verbatim)
"FORALL A(O) EXISTS f FORALL P . P [LAMBDA p A] EQUIV P f"
FORALL A(O) EXISTS f(OI) FORALL P(O(OI)).P[LAMBDA p(I) A] EQUIV P f

@Begin(Verbatim,Spacing=1.5)
@forall@;A@F12[o]@exists@;f@F12[oo]@forall@;P@F12[o(oo)].P[@G(l)p@F12[o] A] @equiv P f
@End(Verbatim)

"FORALL A(O) EXISTS f(OO) FORALL P. P[LAMBDA p A] EQUIV P f"
FORALL A(O) EXISTS f(OO) FORALL P(O(OO)).P[LAMBDA p(O) A] EQUIV P f
@End(Verbatim)
