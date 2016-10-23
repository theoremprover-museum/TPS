@Comment<
	This file contains the TPS3 Facilities Guide Short Version.
	>

@Comment<
	Editions:
	First: Being prepared
	>

@Make(Manual)
@modify(description, spread .3)

@Comment<Modify this for double-sided/single-sided versions>
@String(DoubleSided="No")
@Style(DoubleSided=No)

@String(KsetSize="10")
@String(DocName="TPS3 Facilities Guide
for Users")

@Use(Database = "../lib")
@LibraryFile(TPSDocuments)
@comment<@Include(/usr/theorem/doc/lib/indexcat.mss)>

@Comment<
	This is the title page information.
	>

@blankspace(2.25inches)

@MajorHeading{TPS3 Facilities Guide 
for Users}
@style(date="1952 March 8")
@Heading(@value(date)

Peter Andrews
Sunil Issar
Dan Nesmith
Frank Pfenning
Hongwei Xi
Matthew Bishop
Chad E. Brown)


@BlankSpace(1.5inches)
@comment[@Center(Working Edition)]

@value(copyright-and-research-credit)

@Include(preamble-short.mss)
@Include(facilities-short.mss)
@Stopheaders

