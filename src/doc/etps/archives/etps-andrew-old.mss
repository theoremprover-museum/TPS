@make(article)
@use(database ="/afs/andrew.cmu.edu/mcs/math/TPS/doc/lib")
@libraryfile(tpsdocuments)
@set(chapter=-1)
@set(page=1)


@Heading(Using @HETPS on Andrew)

Remember that your work on @ETPS exercises, like other exercises,
is to be done independently unless collaboration is specifically
authorized. You are obligated to work on these exercises 
without help from other people, documents which contain
specific solutions, or programs which could be used to generate
solutions to the exercises. You need to think about these exercises
for yourself in order to learn from them.

@section(Documentation)

To use @ETPS efficiently, you should be familiar with the
@ETPS manual. The postscript file for this manual can be found at:@*
http://gtps.math.cmu.edu/tps-mans.html

@ETPS online documentation is at:@*
http://gtps.math.cmu.edu/htmldoc-etps

@section(Starting @HETPS)

@ETPS can be run on Unix-based Andrew workstations.
@comment[Sun workstations which may be found
in rooms of the Wean Hall 5200 corridor and the WestWing dorm cluster.]
You may  find that you can also run it on other Unix-based
workstations which are connected to the campus network.


@begin[comment]
 which are scattered around various offices on campus.
If you wish to use a workstation on which @ETPS does not currently run, send
mail to pa01@@andrew to see if something can be worked out.

@ETPS can be run on these workstation types: 
@comment[Sun3,] Decstation 3100, Decstation 5000, SPARCstation 5
@comment[sun4_413], SPARC Ultra, @comment[which is sun4_55]
HP712 @comment[hp700_ux90].

If you start up @ETPS in a typescript window, it probably won't
respond to your commands; try starting it up in an xterm window
instead.  (If you start it up with the @t[xetps] command mentioned
below, this will be done automatically for you.)

@end[comment]


If you are not logged in on an Andrew workstation, you should do@*
klog <andrew userid> -c andrew.cmu.edu@*
so that @ETPS will be able to write in the score file when you
execute the DONE command.

You should run @ETPS from a private directory, where the files you
will be creating cannot be read by others.

To start @ETPS on Andrew, do the following:

@t(/afs/andrew/mcs/math/etps/bin/etps)

To make things easier, add the line@*
@t(source /afs/andrew/mcs/math/etps/etps.cshrc)@*
to your ~/.cshrc file.
This will set up aliases for you, so that the command@*
@t(etps)@*
will start up @ETPS without you having to remember the exact path name.
(The x-server reads the .cshrc file when it starts up an xterm.)

@section(Using @HETPS under X11)

The Unix X11-window system makes it possible for you to run @ETPS in a
window with fonts that have special symbols for the logical
connectives and quantifiers. Formulas will appear just as they do in
the textbook.

@subsection(Setting the Fontpath)

If you sourced the file /afs/andrew/mcs/math/etps/etps.cshrc in your
~/.cshrc file, it will tell your X server where the fonts are.  If you
did not source it, you should place the line
@verbatim(/afs/andrew/mcs/math/etps/etps.Xclients) in your ~/.Xclients
file @comment[Next phrase has not been tested] or your ~/.xinitrc file
(if you are using one), or issue that command at the shell prompt once
the window system has started up.
Alternatively, just issue the command:@*
@t(xset +fp /afs/andrew.cmu.edu/mcs/math/etps/fonts/decfonts/)@*
or@*
@t(/usr/misc/bin/xset fp+ /afs/andrew.cmu.edu/mcs/math/etps/fonts/decfonts/).

@subsection(XETPS and XETPS-BIG)

The file /afs/andrew/mcs/math/etps/etps.cshrc (which you should have
sourced in your ~/.cshrc file) has set up two aliases for using @ETPS
under X11.  The first is @t(xetps).  Just issue this at the shell prompt,
and a window will be started with @ETPS running inside.  Click with the
mouse to place the left corner of the window.

If you do not have an alias set up for @t(xetps), you can start it up with 
the command:@*
@t(xterm -geometry 80x54 -rw -fn vtsingle -fb vtsymbold -sb -n ETPS -T ETPS -e /afs/andrew/mcs/math/etps/bin/etps &)

Once the window has appeared and @ETPS has started, you need to tell
@ETPS that you want to use the special fonts for output.  At the @ETPS
prompt, issue the command @*
@t(<0> setflag style) @*
then, at the subsequent prompt @*
@t(xterm)@*
If you have followed all the steps
above, the special symbols should appear when any wffs are printed by
@ETPS.

The second alias for running @ETPS under X11 is @t(xetps-big).  This uses
very large X fonts, and the window will take up most of your screen.

If you resize your @ETPS X-window and make it wider or narrower, you will
want to change the setting of the flag @t(rightmargin).  This is normally
set at 79 for 80 column output.  For example, if you change the window to be 
only 50 columns wide, issue the command @*
@t(<1> setflag rightmargin)@*
then at the prompt enter@*
 @t(49) 

@subsection(Proofwindows)

Once you have started @t(xetps), you will probably wish to use the
BEGIN-PRFW command to start up windows containing the current subproof
and the complete proof.  You will need to iconify the lower window or
move it up on your screen by the usual methods for manipulating
X-windows so that you will have room to issue @ETPS commands.

@begin[comment]


To start @t(xetps) from  an SCS DecStation 3100 (and perhaps also from
an SCS DecStation 5000), execute the command:@*
xterm -geometry 80x54+505+0 '#+723+0' -fn vtsingle -fb vtsymbold -sb -n ETPS -T ETPS -e  /afs/andrew/mcs/math/etps/bin/etps-from-cs &
@comment[ was /afs/andrew.cmu.edu/mcs/math/etps/pmax_cs/.etps.exe &]
@end[comment]


@section(Running @HETPS Remotely)

While @ETPS can be installed on many computers, you must use the
@ETPS which is installed on Andrew in order to get credit for your work
when you issue the DONE command.

If you wish to run @ETPS remotely, you can connect by telnet or ssh
from your local machine to one of the Andrew "unix servers" and run
@ETPS there. The Unix servers are set up to allow multiple users and
are named unix1, unix2, unix3, ... .

@subsection(Using Muptime)

When you log in to a unix server, you will want to choose the one which
is least loaded.  To find out which one to use, log into any of them
with the userid @b(muptime), as follows:

@begin(verbatim)
% telnet -l muptime unix7.andrew.cmu.edu
@end(verbatim)

Each of the machines will be listed, along with their current loads.
Choose the machine with the lowest load.
You will then have to open a new  connection to the machine
you select.

You can also issue the @t(muptime) command while logged in under your own
userid and get the same information.

@subsection(Xhost)

If you select unix17 (for example), you may need to issue the command@*
@t(xhost unix17.andrew.cmu.edu)@*
on your local machine (provided that it has 
X-windows) so that  it will permit unix17 to open a window on your screen
when you run @t(xetps) as described above. If you are using ssh instead of
telnet, this will probably not be necessary. 

@subsection(An Alternative to etps.cshrc)

If you will be connecting to an Andrew workstation from another
machine, you may find
that the xset command in etps.cshrc causes confusion for the local
machine, resulting in error messages. In this case, you can source
/afs/andrew.cmu.edu/mcs/math/etps/etps-no-xset.cshrc
instead of /afs/andrew.cmu.edu/mcs/math/etps/etps.cshrc
in your ~/.cshrc file.
In this case, your font path will not automatically contain the @ETPS
fonts when you do log on to an Andrew workstation, but
an alias @t(get-etps-fonts) is defined in etps-no-xset.cshrc,
so you can simply issue the command @t(get-etps-fonts) when you log
onto an Andrew workstation to run @ETPS.

@subsection(Running XETPS Remotely with the ETPS Fonts)

If you run @ETPS remotely using a Unix server, you can always use the
GENERIC style to display wffs. However, if X-windows are available on
your local machine, it may be worth your while to set things up so
that you can use the style XTERM to display wffs, particularly as the
wffs you deal with become more complex. You will need to install the
fonts on the local machine, and probably adjust the font path, in
order to use @t(xetps) successfully.

If your local machine is a PC running some version of Windows, you may
be able to use XWin32 to bring up simulated X-windows on your local machine.
The details involved may vary somewhat for different versions of
Windows, but here is some potentially helpful information from
students who have done this previously:

@paragraph(One Student's Experience Running XETPS on a Windows Machine)

I got the fonts for @ETPS to work on my Windows machine! It was actually
really simple. All I did was download all the fonts from
/afs/andrew.cmu.edu/mcs/math/etps/fonts to the font directory for XWin32,
which is by default C:\Program Files\StarNet\X-Win32 5.1\Fonts\.
Then, from the Configuration screen for X-Win32 (X-Config), under the Font
tab, I selected each of the font paths it listed and hit the "Make
FONTS.DIR" button.

After that, XETPS worked fine with the fonts and all.

I think I actually did more than was necessary to get it to work,
since I don't think you need ALL the fonts in the @ETPS font directory,
just whichever vtsymbol font @ETPS uses, but I'm not sure which since
there are around 5 different vtsymbol fonts in there and I didn't take
time to see exactly what was needed.

@paragraph(Another Student's Experience Running XETPS on a Windows Machine)

Ftp all the fonts and the font.dir (although I think font.dir's not
necessary)  from the decfonts directory to the XWin32\lib\fonts\etps-fonts
directory, where you have created the etps-fonts directory.  

Go through the described method of adding fonts to the directory
described in the README file for xwin32, which
is:  run X-config, add this newly-created font dir etps-fonts to the path
(the "Font" tab in the newest version 5.03), rebuild font.dir (click on
apply button), close and restart XWin32, and proceed as usual (setenv
DISPLAY <hostname>:0.0, xetps, etc.).  

This at least worked on my home machine.  I am not sure if we have
enough of the permissions necessary to do this in the cluster.

Now, when you setflag style to xterm again, it should work.  

@paragraph(Another Student's Experience Running XETPS on a Windows Machine)

I finally got things to work running XETPS remotely with XWin32
as described above. The key step was to 
source /afs/andrew.cmu.edu/mcs/math/etps/etps-no-xset.cshrc@*
instead of /afs/andrew.cmu.edu/mcs/math/etps/etps.cshrc.

@section(Aborting a Command)

Respond to a prompt for an argument with @t(ABORT) to abort the current
command and return to the top level.  You can also use the control character
@t(CTRL-C) or @t(CTRL-G) (depending on what machine and version of LISP is
being used) to interrupt @ETPS and return to the top level.

@section(Stopping @HETPS)

To temporarily suspend @ETPS, use the control character @t(CTRL-Z).  Then
you can restart @ETPS with the  Unix command @t(fg).  Of course, if you
are running @ETPS in an X-window, you should not need to suspend it. You
can just make it into an icon.

To kill @ETPS, use the @t(EXIT) command from within @ETPS, or use
the Unix @t(kill) command from outside @ETPS.  
Warning:  it is possible to start
multiple versions of @ETPS and leave them hanging around.  
This is a bad idea, since those that
you are not using will be a drag on the system, slowing things down
considerably.



@section(Filenames)

In the Unix system, filenames are case sensitive.  When
you enter a filename in @ETPS, it will be translated to lower case
@i(unless you enclose the filename in double quotes).  For example, if
you answer @t(X2106.WoRK) when prompted for a filename, @ETPS will interpret
this as @t(x2106.work), but the entry @t("X2106.WoRK") will be interpreted
as @t(X2106.WoRK), just as you entered it. 

@comment[
In the Microvax version of @ETPS, you cannot use the "~user"
convention of referring to a user's home directory.
]

@section(Tex, Scribe and Printing)

The command @t(TEXPROOF) works just as described in the @ETPS manual.
Once you have created your ".tex" file on Andrew, however, you should just
start TeX by, for example,
@begin(verbatim)
% tex x2106
This is TeX, Version 3.1415 (C version 6.1)
(x2106.tex (/afs/andrew.cmu.edu/mcs/math/etps/sun4_55/doc/lib/tps.tex))
[1]
Output written on x2106.dvi (1 page, 1212 bytes).
Transcript written on x2106.log.
@end(verbatim)
This will produce the file x2106.dvi, and you can use the system command 
dvips to create x2106.ps from this:
@begin(verbatim)
dvips x2106
@end(verbatim)
@comment[dvips x2106 -o x2106.ps]
To print such a file, do the following, replacing @i(printer) with
the name of one of the Postscript printers such as birch, pine, etc.
@begin(verbatim)
% lpr -P@i(printer) x2106.ps
@end(verbatim)

You can look at the file after running tex but before printing it with the command
@begin(verbatim)
xdvi x2106
@end(verbatim)
Exit xdvi with the command "q".

To create output suitable for LaTeX
rather than TeX, set the flag @t(LATEX-EMULATION) to T while you are running @ETPS.

Alternatively, you can use the @t(SCRIBEPROOF) command in @ETPS to produce a file
such as x2106.mss, run it through scribe with the command "scribe x2106" to produce
the file x2106.ps. Scribe is not available on some machine types.

The default device for Andrew's Scribe is Postscript, and Scribe will
produce a file with the extension ".ps", e.g., "x2106.ps".

@section(Possible Problems)

Various @ETPS commands involve writing files.  These commands include
DONE, SAVEPROOF, and TEXPROOF.  Unfortunately, it quite often
happens that the fileservers which handle the directory in which you
wish to write are down.  This is especially common at night, when
the backing-up of AFS volumes is done.  When this happens, you will get
an error message saying something like "Clisp error trapped".  If this occurs,
try the following:

@begin(enumerate)
Try storing the file on your local workstation disk instead of AFS.
As an example, instead of @verbatim(texproof "x2106.tex") 
try @verbatim(texproof "/usr/tmp/x2106.tex").  

Later, after the 
fileservers are back up, you can copy the file to your home directory on AFS.

If the DONE command has failed, use SAVEPROOF to save your proof, then later
you can restore the proof and try the DONE command again.
@end(enumerate)

