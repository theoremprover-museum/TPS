@Make(Manual)

@Comment<Modify this for double-sided/single-sided versions>
@String(DoubleSided="Yes")
@Style(DoubleSided=Yes)

@String(KsetSize="10")

@Use(Database = "../lib")
@LibraryFile(TPSDocuments)
@IndexCategory(RefName="IndexSrule",IndexName="Inference Rule",FaceCode="T",
	SecondaryName="Inference Rules")
@Define(TB, FaceCode T)
@CommandString(ETPS="@b[E]@cb[TPS]",HETPS="ETPS")
@CommandString(LISP="@b[L]@cb[ISP]",HLISP="LISP")
@CommandString(BaseSystem="TOPSD")
@String(DocName="ETPS User's Manual")

@Modify(Description, LeftMargin +0.7inches, Indent -0.7inches,
		     Spread 0.5 Spacing)

@Comment<
	This is the title page information.
	>

@blankspace(2.25inches)

@MajorHeading{ETPS User's Manual}
@Heading(@value(date)

Frank Pfenning
Sunil Issar
Dan Nesmith
Peter B. Andrews
Hongwei Xi
Matthew Bishop
Chad E. Brown

Version for
Basic Logic
Mathematical Logic I and II)

@value(copyright-and-research-credit)
@comment<@Begin(ResearchCredit)
@value(Research-Credit)
@End(ResearchCredit)>
@include(user)
@include(user-end)
@Include(system)
@Include(edtop)
@include(editor)
@include(infrul)
@include(infex)
@include(quick-ref)
@include(quick-ref-auto)
@include(inference-rules)
@include(photo)
@include(hol)
@include(hol-exampl)
@include(top-level-amenities)
@include(editor-long)
@include(restrict)
@include(thms)
@StopHeaders
