@Begin(Description)
@IndexTypes2{ANYTHING}@\ 
Any legal LISP object.

@IndexTypes2(SYMBOL)@\ Any legal LISP symbol.

@IndexTypes2{BOOLEAN}@\ 
A Boolean value (NIL for false, T for true).

@IndexTypes2(YESNO)@\ @t(y) or @t(yes) to mean @T(YES), @T(n) or @T(no) to mean @t(NO).

@IndexTypes2(INTEGER+)@\ A nonnegative integer.

@IndexTypes2(POSINTEGER)@\ A positive integer .

@IndexTypes2(STRING)@\ A string delimited by double-quotes. For example
@wt{"This is a remark."}. Strings may contain @wt{<Return>}, but no double-quotes.

@begin(comment)
@IndexTypes2(TERMINALTYPE)@\ One of the following codes for terminals:
@Begin(VERBATIM)
C100    - HDS Concept 100, 108 or CLNZ;
Fox     - Perkin-Elmer 1100;
Glass   - Any display terminal with backspace;
H19     - Heath H-19, Zenith, or IBM PC running Kermit;
MB4     - Minibee 4;
T1061   - Teleray 1061;
VT100   - DEC VT100 in ANSI mode;
VT52    - DEC VT52 (DecScope) or IBM PC running Kermit;
UNKNOWN - Anything else.
@End(VERBATIM)
@end(comment)

@IndexTypes2(FILESPEC)@\ A file specification of the form
@wt{"@I(<dir>)@I(name).@I(ext)"}, @wt{"@I(name).@I(ext)"}, or simply
@wt{@I(name)}.  For example:@wt["<FP01>EXAMPLE1.MSS"]. The 
defaults for @I(dir) and @I(ext) are usually correct and it is enough
to specify @I(name).

@IndexTypes2(LINE)@\ The number of a line.

@IndexTypes2(LINE-RANGE)@\ A range of lines from M through N, written M--N, where M and N
are positive integers and M <= N.  As shortcuts, one may write M, which
represents the range M--M;  M--, which stands for the range from line M
through the last line of the current proof; and --N, which represents the
range from the first line of the proof through line N.  Hence --
represents the range consisting of every line in the proof.

@IndexTypes2(EXISTING-LINE)@\ The number of a line in the current outline.

@IndexTypes2(PLINE)@\ The number of a planned line.

@IndexTypes2(GWFF)@\ A well-formed formula (wff).  @ETPS prompts you
for a @T(GWFF) if it needs a wff, term or variable, and it will
usually tell you which one of these it expects in the brief help message in the
prompt.

@\ A @IndexTypes2(GWFF)  can be one of the following:
@Begin(Enumerate)
A string representing a wff in double quotes. Strings may  
contain @wt{<Return>}'s for formatting purposes.  
  Case does matter for variables and constants like
 @T("x"), @T("y"), @T("P"). For example  @T("x") is not the same as
 @T("X").  Case is ignored, however, for special keywords, like
quantifiers, connectives, etc. All logical symbols must be separated by spaces.
In addition, the bracketing conventions used in
the logic textbook are used in @ETPS, and the symbol @t("~") can be
used as an abbreviation for @t("NOT"); thus
@wt("forall x.P x y implies ~ [Q x or Q y]") represents the same
gwff as @wt("FORALL x[P x y IMPLIES NOT .Q x OR Q y]").
In general you may type wffs just as they appear
in the logic text.  See  Chapter  @ref(examples) for some examples
of typed wffs and variables, and Chapter @ref(hol) (especially
Section @ref(holwffex)) for 
examples of wffs of higher-order logic (type theory).
For more examples, execute the command @IndexCommand(PROBLEMS) in ETPS
with style GENERIC and answer "yes" when you are asked if you
wish to see definitions.
@comment(Since superscripts cannot be displayed,
they cannot be part of a formula in @ETPS.  Instead, 
enclose a function
symbol and all of its arguments by brackets. For example @wt{P[f x][g x x]} 
stands for @wt{Pf@+{1}xg@+{2}xx}.)  
Superscripts can be used, but unlike the
textbook, they are not used to indicate the arity of functions. Instead,
they are used to distinguish variables.  Superscripts are indicated by using
a "^".  Valid superscripts must follow these rules.  
@begin(itemize)
  Only strings of the form [0-9]+ can be superscripts. 

  The user will explicitly indicate a superscript by the use of the "^".
    E.g., "x^0", "foo^1234567".  A "^" which is not followed by a legal 
    superscript is treated as any (non-logical-constant) character would be.
    Thus "x^" is legal input, as is "^" or "^^", or "x^y".

 A superscript can only be used at the end of a variable, not in the middle.
    Hence "x^1y" will be parsed as "x^1(II) y(I)" (x^1 applied to y) not 
    as "x^1y(I)" (a single variable).  

 Generic style will show the superscripts with the ^, i.e.,  if you enter
    "x^1(I)", then it will print as "x^1(I)" when the style is generic and
    @IndexFlag(PRINTTYPES) is T. 

  Entering "x1" results in "x1", not "x^1", i.e., superscripts will not
    be created from the user's input unless explicitly indicated.

@end(itemize)


A number of a line in the proof, for example @T(100).  @ETPS will replace it
with the wff asserted in that line.

A label referring to a wff or the name of an exercise or lemma. A label
can be assigned by the @indexedop(CW) command (see page @pageref(CW)).

@wt{(ed gwff)} which allows you to locate a sub-expression of another
@IndexTypes2(GWFF). For example @wt{(ed 100)} can be used to extract a 
subformula from the assertion of line @T(100).  See Section
 @ref(locating) for an explanation of how to use this option.

A backquoted form, calling a wffop. For example, @wt[`(S x y A)] is
the wff resulting from the substitution of @t(x) for @t(y) in @t(A).
See Appendix @ref(wffops) for the most commonly used wffops.
@End(Enumerate)

@IndexTypes2(GWFF0)@\ A gwff of type o.  Two special constants of type o
are provided: @ITT(TRUTH) and @ITT(FALSEHOOD).
These gwffs act just as you might expect, e.g., @w{TRUTH @t(@equiv p@or@not@;p)}
and @w{FALSEHOOD @t(@equiv p@and@;@not@;p)}.  After running a proof through
@t(SCRIBE), TRUTH will print as @truth and FALSEHOOD will print as
@falsehood. @indexentry[key="%2",Entry="@truth",number]
@indexentry[key="%1",Entry="@falsehood",number]
@seealso[primary="TRUTH", other="@truth"]
@seealso[primary="FALSEHOOD", other="@falsehood"]

@IndexTypes2(GVAR)@\ A gwff which must be a logical variable.

@IndexTypes2{TYPESYM}@\ 
The string representation of a type symbol. See Section @ref(holwffs).

@begin(comment)
@IndexTypes2{WFFSET}@\ 
A symbol standing for a set of wffs in a hypothesis.
@end(comment)

@IndexTypes2(BOOK-THEOREM)@\ A theorem in the book. See the 
@IndexCommand(PROBLEMS) command  (page @pageref(problems)).

@IndexTypes2{EXERCISE}@\ 
An exercise which may be assigned.

@IndexTypes2{PRACTICE}@\ 
An unscored practice exercise.

@IndexTypes2{TEST-PROBLEM}@\ 
A potential test problem.
@End(Description)

Several argument types are lists or pairs of other types. They are specified
in parentheses, for example @wt{(1 2 99)}.  The empty list is specified
by @wt{()}. @comment<When typing a singleton list the parentheses may be omitted,
for example @wt{3} stands for @wt{(3)}.> Pairs are entered as two element
lists where the two elements are separated by a period. For example, you
might enter the pair @wt[("x" . "y")].  Do not put commas into your input!

@comment<In addition, an argument type may be the union of other types.
This is usually a pair of argument types where one is a list and
the other is a symbol. In any event, the argument types should be
distinguishable without parsing or other elaborate computation.
>
We list the most common of the composite argument types below:

@Begin(Description)
@begin(comment)
@IndexTypes2{SYMBOLLIST}@\ 
A list of references to logical labels.
@end(comment)
@IndexTypes2{OCCLIST}@\ 
A list of occurrences (counted left-to-right) of a subwff in a wff.
This list may be entered as a list of positive numbers or the symbol @T(ALL).
ALL refers to all occurrences of the subwff.

@IndexTypes2(LINELIST)@\ A list of numbers of lines.  

@IndexTypes2(LINE-RANGE-LIST)@\ A list of line ranges.

@IndexTypes2(EXISTING-LINELIST)@\ A list of numbers of lines in the current outline.

@IndexTypes2(GWFFLIST)@\ A list of @T(GWFF)s.

@IndexTypes2(GVAR-LIST)@\ A list of @T(GVAR)s.

@begin(comment)
@IndexTypes2{GWFFALIST}@\ 
A list of substitutions. Each substitution is a pair of @t(GWFF)s.

@IndexTypes2{TYPESYM-NIL}@\ 
The string representation of a type or NIL.

@IndexTypes2{TYPESYMLIST}@\ 
A list of string representations of types.

@IndexTypes2{TYPESYMLIST-NIL}@\ 
A list of type symbols or NIL.
@end(comment)
@End(Description)

@Section(Flags and Amenities) 

Many aspects of @ETPS are controlled by flags. @index(flag)
See section @ref(flags) for 
some information about flags.

@ETPS incorporates several features of the Unix C-shell
(csh) top-level.  These features include various control characters,
command sequences, a  history
mechanism, and aliases. @index(alias)
See Appendix @ref(amenities) for details.

You may wish to set certain flags and define certain aliases
each time you run @ETPS.  A good way to do this without having
to repeat the commands is to start a work file (using
@t(@indexcommand(SAVE-WORK))), then set the flags and define your
aliases, then use @t(@indexcommand(STOP-SAVE)) to stop saving into the
file. When you subsequently use @ETPS, you can use
@t(@indexcommand(EXECUTE-FILE)) to automatically execute all the commands 
in the work file to set the flags and define the aliases.


@Section(Bugs and Error Messages)
Typing or logical errors are usually noticed by @ETPS, which issues
an appropriate diagnostic message and typically throws you back to top-level.

Most bugs in @ETPS itself will be caught by an error handler which
appends an appropriate message to a file in the teacher's area.  This of
course only applies to real bugs in the @ETPS software or Common Lisp,
not typing errors  which are caught by the command interpreter.  You may try
again after you get a bug error message, and often you will discover
that you just made a mistake which was not caught by the usual error
handling routines.  If you still get an error send mail to the teacher
or send a message with the @T(REMARK) command. If you think that you have
discovered a bug in @ETPS, don't delete the @T(.WORK) file for that session
but rename that file (say, to @t(Y)@i(exercise-number)@t(.work)) so that
your work is not overwritten, then allow read access for it and send mail
to the teacher with a pointer to that file.

