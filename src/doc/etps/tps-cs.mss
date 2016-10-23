@make(manual)
@Use(Database = "../lib")
@LibraryFile(TPSDocuments)
@set(chapter=-1)
@chapter(Using @HTPS in the CS cell)

@section(Starting @HTPS)

You must be in the tpsguest group to have access to @tps.

To begin @TPS in the CMU CS cell, do the following:

@t(% /afs/cs/project/tps/bin/tps3)

@begin(comment)You should begin @ETPS in a private directory, one in which
others cannot read the files you will be creating.
@end(comment)

To make things easier, add the line 
@t(source /afs/cs/project/tps/tps.cshrc) to your ~/.cshrc file.
This will set up aliases for you, so that @t(% tps3) will
start up @TPS without you having to remember the exact path name.

@TPS can be run on these workstation types: Sun Sparcstations
and Pmax (Decstation 3100).

@begin(comment)If you do not have access to a workstation, you can 
run @ETPS on one of the
so-called "unix servers".  These machines, which are named unix1, unix2,
..., unix9, are Microvaxes with large memories.  They are
set up to allow multiple users and must be reached by using the
program telnet.  

When you log in to a unix server, you will want to choose the one which
is least loaded.  To find out which one to use, log into any of them
with the userid @b(muptime), as follows:

@begin(verbatim)
% telnet unix1.andrew.cmu.edu
Trying...
Connected to UNIX1.ANDREW.CMU.EDU
Escape character is '^]'.

4.2 BSD UNIX (unix1.andrew.cmu.edu)

login: muptime
@end(verbatim)

Each of the machines will be listed, along with their current loads.
Choose the machine with the lowest load.
You will then have to open a new telnet connection to the machine
you select.
@end(comment)

@section(Using @HTPS under X11)

If you are running the X11 window system, you can run @TPS in a window
with fonts that have special symbols for the logical connectives and
quantifiers, so that formulas will appear just as they do in the textbook.
There are a few things you need to do to set this up.

First, you need to tell your X server where the fonts are.  This is
accomplished by placing the line
@verbatim(/afs/cs/project/tps/tps.Xclients) in your ~/.Xclients
or ~/.xinitrc (if you are using one), or by issuing that command at the 
shell prompt once 
the window system has started up. 

Second, the file /afs/cs/project/tps/tps.cshrc (which you should have
sourced in your ~/.cshrc file), has set up two aliases for using @TPS
under X11.  The first is @t(xtps3).  Just issue this at the shell prompt,
and a window will be started with @TPS running inside.  Click with the
mouse to place the left corner of the window.

Third, once the window has appeared and @TPS has started, you need to
tell @TPS that you want to use the special fonts for output.  At the
@TPS prompt, issue the command @t(<0> setflag style) then, at the
subsequent prompt @t(xterm).  If you have 
followed all the steps above, the special symbols should appear when
any wffs are printed by @TPS.

The second alias for running @TPS under X11 is @t(xtps3-big).  This uses
very large X fonts, and the window will take up most of your screen.

If you resize your @TPS X window and make it wider or narrower, you will
want to change the setting of the flag @t(rightmargin).  This is normally
set at 79 for 80 column output.  For example, if you change the window to be 
only 50 columns wide, issue the command @t(<1> setflag rightmargin),  then
enter @t(49) at the prompt.

If you wish to simply run ETPS from a cs machine, try issuing the command
xterm -geometry 80x54 -rw -fn vtsingle -fb vtsymbold -sb -n ETPS -T ETPS -e /afs/andrew/mcs/math/etps/bin/etps &

@section(Aborting a Command)

Use the control character @t(CTRL-C) to abort the current command and
return to the top level.  If you are being prompted for something by
@TPS, you may enter @t(abort) to abort the current command.

@section(Stopping @HTPS)

To temporarily suspend @TPS, use the control character @t(CTRL-Z).  Then
you can restart @TPS with the @t(fg) Unix command.  Of course, if you
are running @TPS in an X window, you should not need to suspend it. You
can just make it into an icon.

To kill @TPS, use the @t(EXIT) command from within @TPS, or use
the Unix @t(kill) command from outside @TPS.  
Warning:  it is possible to start
multiple versions of @TPS and leave them hanging around.  
This is a bad idea, since those that
you are not using will be a drag on the system, slowing things down
considerably.

@section(Filenames)

In the Unix system, filenames are case sensitive.  When
you enter a filename in @TPS, it will be translated to lower case
@i(unless you enclose the filename in double quotes).  For example, if
you answer @t(X2106.WoRK) when prompted for a filename, @TPS will interpret
this as @t(x2106.work), but the entry @t("X2106.WoRK") will be interpreted
as @t(X2106.WoRK), just as you entered it. 

@begin(comment)In the Microvax version of @ETPS, you cannot use the "~user"
convention of referring to a user's home directory.
@end(comment)

@section(TeX, Scribe and Printing)

The command @t(TEXPROOF) works just as described in the @ETPS manual.
Once you have created your ".tex" file on Andrew, however, you should just
start TeX by, for example,

@t(% tex x2106)

This will produce the file x2106.dvi, and you can use the system command 
dvips to create x2106.ps from this.

To print such a file, do the following, replacing @i(printer) with
the name of one of the Postscript printers such as birch, pine, etc.

@begin(verbatim)
% lpr -P@i(printer) x2106.ps
@end(verbatim)

To create output suitable for LaTeX
rather than TeX, set the flag @t(LATEX-EMULATION) to T.

Alternatively, you can use the @t(SCRIBEPROOF) command in @ETPS to produce a file
such as x2106.mss, run it through scribe with the command "scribe x2106" to produce
the file x2106.ps. Scribe is not available on some machine types.

The default device for Scribe at CS is Postscript, and Scribe will
produce a file with the extension ".ps", e.g., "x2106.ps".

@section(Possible Problems)

Various @TPS commands involve writing files.  These commands include
DONE, SAVEPROOF, and TEXPROOF.  Unfortunately, it quite often
happens that the fileservers which handle the directory in which you
wish to write are down.  This is especially common at night, when
the backing-up of AFS volumes is done.  When this happens, you will get
an error message saying something like "Clisp error trapped".  If this occurs,
try the following:

@begin(enumerate)
Try storing the file on your local workstation disk instead over AFS.
As an example, instead of @verbatim(texproof "x2106.tex") 
try @verbatim(texproof "/usr/tmp/x2106.tex").  

Later, after the 
fileservers are back up, you can copy the file to your home directory on AFS.

If the DONE command has failed, use SAVEPROOF to save your proof, then later
you can restore the proof and try the DONE command again.
@end(enumerate)


