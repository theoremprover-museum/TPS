@Make(Manual)

@Comment<Modify this for double-sided/single-sided versions>
@String(DoubleSided="Yes")
@Style(DoubleSided=Yes)

@String(KsetSize="10")

@Use(Database = "../lib")
@LibraryFile(Mathematics10)
@LibraryFile(BtpsDocuments)

@String(DocName="GRADER Manual")

@Modify(Description, LeftMargin +0.7inches, Indent -0.7inches,
		     Spread 0.5 Spacing)

@Comment<
	This is the title page information.
	>

@blankspace(2.25inches)

@MajorHeading{GRADER Manual}
@style(date="1952 March 8")
@Heading(
@value(date)


Sunil Issar
Peter B. Andrews
Frank Pfenning
Dan Nesmith)

@value(copyright-and-research-credit)
@comment<@Begin(ResearchCredit)
@value(Research-Credit)
@End(ResearchCredit)>

@include(intro.mss)

@include(howit.mss)

@include(coming.mss)

@include(setup.mss)

@include(records.mss)

@include(figures.mss)

@comment<@include(btpscom.mss)>

@StopHeaders
