@Comment<commands-only facilities-guide-maker, for packages we don't always load>
@Make(Manual)
@modify(description, spread .3)
@String(DoubleSided="No")
@Style(DoubleSided=No)
@String(KsetSize="10")
@String(DocName="TPS3 Facilities Guide
for Users")
@Use(Database = "../lib")
@LibraryFile(TPJDocuments)
@blankspace(2.25inches)
@MajorHeading{TPS3 Facilities Guide 
for Users}
@style(date="1952 March 8")
@Heading(@value(date)

Peter Andrews
Matthew Bishop
Sunil Issar
Dan Nesmith
Frank Pfenning
Hongwei Xi)

@BlankSpace(1.5inches)
@value(copyright-and-research-credit)
@Include(facilities-cmd.mss)
@Stopheaders

