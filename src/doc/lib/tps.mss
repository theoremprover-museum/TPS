@comment(The following is a hack to allow old scribeproof files
to be printed in either dover or postscript format.)
@case[device, dover {

@Define(T, FaceCode F)

@Define(FixWidth, Use Format, RightMargin -0.25inches, 
		LongLines Wrap, FaceCode F, EOFOK)
@Modify(Verbatim, LongLines wrap)

@SpecialFont( F0="SAILA  10",
				F1="SAILA   8",
				F2="Math   10",
				F3="Hippo  10",
				F4="GACHA10BRR",
				F5="CMSY10S10",
				F6="Hippo   8",
				F8="Math    8",
				F9="CMathX10S10")

@TextForm(	F10="@+<@F1<@parm<text>>>",
		F11="@-<@F1<@parm<text>>>",
		F12="@-<@F6<@parm<text>>>",
		F13="@ovp(/)@F2<@parm<text>>",
                overlinecap="@ovp<@parm(text)>@~
                        @begin(transparent,script 1line)@~
                        @ux<@hsp(1 quad)>@end(transparent)",
)

@Define(VLine, Use Verbatim,Spacing=1.5,Above=0,Below=0,Break=Off)
@Define(TLine, Use Text,Spacing=1.5,Above=0,Below=0,Break=Off)

@CommandString'
	AND="@F5<^>",
	ASSERT="@F2<p>",
	EQUIV="@F2<W>",
	EQP="@i<E>",
	EXISTS="@F5<9>",
	FALSEHOOD="@F2<T>",
	FORALL="@F5<8>",
	IMPLIES="@F8<J>",
	INTERSECT="@F5<\>",
	UNION="@F5<[>",
	IOTA="@F3<i>",
	LAMBDA="@F3<l>",
	NOT="@F5<>",
	ONE="@overlinecap(1)",
	OR="@F5<_>",
	POWERSET="@F5<P>",
	SCRIPTP="@F5<P>",
	SUBSET="@F2<I>",
	SETINTERSECT="@F2<y>",
	SETUNION="@F2<U>",	
	TRUTH="@F5(>)",
'
},
postscript = {
@comment(Here we are assuming that current style has Helvetica fontfamily)

@Define(T, FaceCode F)
@Define(F3, FaceCode J, size +2)
@define(jsym, facecode J)
@define(ksym, facecode K)
@Define(F5, FaceCode B)
@Define(F7, Facecode D)
@Define(F9, Facecode E)

@Define(FixWidth, Use Format, RightMargin -0.25inches, 
		LongLines Wrap, FaceCode F, EOFOK)
@Modify(Verbatim, LongLines wrap)

@specialfont(F6="Symbol", F0="Helvetica", F1="Helvetica", F2="Symbol")

@modify(F1, size -2)
@modify(F6, size -2)

@TextForm(F4="@F5<@parm<text>>",
        	F10="@+<@F1<@parm<text>>>",
		F11="@-<@F1<@parm<text>>>",
		F12="@-<@F6<@parm<text>>>",
		F13="@ovp(/)@jsym<@parm<text>>",
           overlinecap="@ovp<@parm(text)>@~
                        @begin(transparent,script 1line)@~
                        @ux<@hsp(1 quad)>@end(transparent)",
)

@Define(VLine, Use Verbatim,Spacing=1.5,Above=0,Below=0,Break=Off)
@Define(TLine, Use Text,Spacing=1.5,Above=0,Below=0,Break=Off)
@Define(size14, size 14)

@CommandString'
	AND="@ksym(C)",
	ASSERT="@!|@/@hsp<2pt>@begin(jsym, script +0.02em)-@end(jsym)@ ",
        COMPOSE="@size14[@-[@jsym[N]]]",
	EQUIV="@jsym(X)",
	EXISTS="@F2($)",
	EQP="@i<E>",
	FALSEHOOD="@jsym(^)",
	FORALL=<@F2(")>,
	IMPLIES="@jsym<m>",
	INTERSECT="@jsym<k>",
	UNION="@jsym<l>",
	IOTA="@g<i>",
	LAMBDA="@g<l>",
	NOT="@jsym<~>",
	ONE="@Overlinecap<1>",
	OR="@ksym<D>",
	POWERSET="@scriptp",
	SCRIPTP="@begin(jsym,script +0.3em)g@end(jsym)",
	SCRIPTU="@i<U>",
	SUBSET="@jsym<q>",
	SETINTERSECT="@F3<k>",
	SETUNION="@size14[@union]",
	TRUTH="@f5<T>"
'
}
]



