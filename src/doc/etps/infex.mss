
Now we shall give an example  which demonstrates how to read a command. Before
proceeding the reader should look at the description of @T(DEDUCT) in Section
@ref(deduct) below.

Suppose the proof originally contains the line

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
@comment[
@begin(VLine)(P3)  H @ASSERT@;  @End(Vline)@Begin(TLine)@$A @IMPLIES@; B@>@w'PLAN4'@End(TLine)]
@begin(VLine)(P3)  @ScriptH @ASSERT@;  @End(Vline)@Begin(TLine)@$A @IMPLIES@; B@>@w'PLAN4'@End(TLine)

@r(and we apply the command:)

<n>DEDUCT    P3     H1     D2
@end(verbatim)
Here
@begin(itemize)
@t(P3) stands for the planned line that you are trying to prove.

@t(H1) stands for the number of the line which asserts the new hypothesis 
(the wff @t(A) in this case).
@begin(comment)
The @t(<O>) need not concern the novice; it refers to 
the fact that the wff @T(A) is of type @t(O) in the sense of type theory.
@end(comment)

@t(D2) stands for the number of the new planned line (whose assertion is 
@t(B) in this case).
@end(itemize)
After the rule is applied, the proof will contain the lines:

@\ @Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
@comment[
 @Begin(VLine)(H1)  H,A      @ASSERT@;  @End(VLine)@Begin(TLine)@$A@>@w'Hyp'@End(TLine)
 @Begin(VLine)(D2)  H,A      @ASSERT@;  @End(VLine)@Begin(TLine)@$B@>@w'PLAN5'@End(TLine)
 @Begin(VLine)(P3)  H        @ASSERT@;  @End(VLine)@Begin(TLine)@$A @IMPLIES@; B@>@w'Deduct: D2'@End(TLine)]
 @Begin(VLine)(H1)  @ScriptH,A      @ASSERT@;  @End(VLine)@Begin(TLine)@$A@>@w'Hyp'@End(TLine)
 @Begin(VLine)(D2)  @ScriptH,A      @ASSERT@;  @End(VLine)@Begin(TLine)@$B@>@w'PLAN5'@End(TLine)
 @Begin(VLine)(P3)  @ScriptH        @ASSERT@;  @End(VLine)@Begin(TLine)@$A @IMPLIES@; B@>@w'Deduct: D2'@End(TLine)
@end(verbatim)


