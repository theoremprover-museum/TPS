@SECTION(@HETPS Editor) @label(locating)

@comment{@SECTION(Locating Subformulas) @label(locating)}

The @ETPS editor can be used to construct new formulas and extract
subformulas from formulas already known to @ETPS.  You can enter the
editor with the top-level command @IndexCommand(ED).  Use @wt{(ed @I(gwff))}
when asked for a @T(GWFF).  This will prompt you with @wt{<Ed@i(n)>}.
The wff you are editing will be referred to as @t(EDWFF). Using the
editor commands, move to a subformula and/or modify the @t(EDWFF)
until you have the @T(GWFF) you desire.  Then exit the editor by using
the command @T(OK); the current value of @t(EDWFF) will be returned.

For example, suppose that @ETPS has asked you for a @t(GWFF) and the @t(GWFF)
you would like to supply is @t(B), a subwff of the assertion in line 1, which
is @wt(A AND B IMPLIES C).  Enter
@T<(ed 1)> to enter the editor with that formula.  Then use the following
sequence of commands:

@verbatim(
<Ed1>L      @i(This moves to the left of the implication sign.)
A AND B
<Ed2>R      @i(This moves to the right of the conjunction.)
B
<Ed3>OK     @i(Since we have what we want, we exit the editor.)
)

This is of course a trivial example, but if @t(B) had been a very
complicated formula, using the editor would have been both faster and less
susceptible to error than typing it in would have been.

You can also use multiple commands on a single editor input line, which
will save more time. We could have done the above example as follows:

@verbatim(
<Ed1>L R OK
)

When @ETPS is running under X-windows or through the Java interface (see Section @ref(JavaInterface)),
the command @t(ED) will also start up two windows which display the top formula and the current 
formula in the editor. 
These windows are automatically updated
as commands are issued to modify the formula, and they
will disappear when you use @t(OK) to leave the editor. 
(By scrolling up in these windows, you can see the previous displays.)
The windows may be moved around and resized by the usual methods
for manipulating windows.

To prevent the windows from appearing, modify the flags @IndexFlag(EDWIN-TOP)
and @IndexFlag(EDWIN-CURRENT).

@begin(comment)
You can put more than one @t(ED) command in one line, which may save a lot
of time.  If you have used @t(ED) a few times, you can specify the commands
to @t(ED) in the command directly.  @ETPS will then not prompt you, but
compute the subformula immediately.  For example @wt{(ed 1 r)} refers to
the right hand side of the top-level infix operator of the assertion of line 
@T(1). If the assertion in line 1 is @wt(A @F8[J] B), then @wt[(ED 1 R)]
refers to @T(B).
@end(comment)

The following sections give brief descriptions of the most commonly
used editor commands. Other editor commands are listed in
Appendix @ref(more-editor).


@begin(comment)
@Begin(Description)
@t(P)@\ print the current expression in short format, i.e., some subformulas
will be replaced by @wt{&}.

@t(PP)@\ print the current expression fully.

@T(L)@\ for an infix-operator, move to the left argument.

@T(R)@\ for an infix-operator, move to the right argument.

@T(D)@\ for an expression like @wt{P x y}, move to the rightmost element;
in this example @wt{y}.  For a quantified expression it will move
to the scope of the quantifier.

@T(A)@\ for an expression like @wt{P x y}, delete the rightmost element;
in this example the result will be to make @wt{Px} the current expression.
For a quantified expression, it will move to the quantified variable.

@T(XTR)@\ replace the top-level wff by the current wff.

@T(0)@ @ (zero)@\ Move up one-level, i.e., undo the last @T(L), @T(R), @T(D),
or @T(A) command.

@T(^)@ @ (hat or uparrow)
@\ Move all the way back to the first current expression.

@T(OK)@\ leave @T(ED).  The current expression will be the result of the 
@T(ED).
@End(Description)
@end(comment)

