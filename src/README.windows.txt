     NOTES ON SETTING THINGS UP UNDER WINDOWS OPERATING SYSTEMS

These instructions have been developed for WINDOWS 98, ME (2000) and XP.
The instructions may not apply to other versions of Windows.         

		Compiling TPS

TPS has been compiled in several versions of Common Lisp: 
Allegro Common Lisp (version 3.1 or higher); Lucid Common Lisp; 
CMU Common Lisp (CMUCL); Steel Bank Common Lisp (SBCL);
Kyoto Common Lisp; Austin Kyoto Common Lisp; Ibuki Common Lisp,
a commercial version of Kyoto Common Lisp; and DEC-20 Common Lisp.
Several source files contain compiler directives (#+) which use different
definitions of variables and functions for different versions of lisp.
Allegro Lisp now exists in several versions (case sensitive, case insensitive, etc.).
We currently claim only that TPS runs under the case insensitive version alisp8.
If your lisp is not one of these, see TROUBLESHOOTING below.

The instructions below are written primarily for Allegro Common Lisp
(www.franz.com).

The Makefile exists only for Unix users.  Windows users can use the
lisp file make-tps-windows.lisp instead.

To install TPS under Windows, perform the following steps:

1.  Create a folder for TPS, e.g., open "My Computer",
then "C:", then "Program Files", and choose "New > Folder"
from the file menu.  Then rename the created folder to
TPS.  Now you have a folder C:\Program Files\TPS\.

2.  Download the gzip'd tps tar file and unzip it (using,
for example, NetZip or WinZip) into the C:\Program Files\TPS\
folder. 

3.  Create a C:\Program Files\TPS\bin folder, if one does not exist.
Also, create a top level folder C:\dev.  This is so TPS can
send output to \dev\null.

4.  Determine if you have a Java compiler.  You can do this by running
"Find" or "Search" (on "Files and Folders") (probably available
through the "Start" menu) and searching for a file named "javac.exe".
A Java compiler is not necessary to install TPS.  The installation
will still create TPS and ETPS, but you will not be able to use the
Java interface.  Note that if you do not have a Java compiler, you can
download Java SDK (with a compiler) from http://java.sun.com/.

5.  Lisp (preferably Allegro 5.0 or greater) will probably be in
"Programs" under the "Start" menu.  Start Lisp (by choosing it from
there) and do the following:

(load "C:\\Program Files\\TPS\\make-tps-windows.lisp")

This should prompt you for information used to compile and build TPS,
as well as compiling the Java files (if you have a Java compiler).  It
will also create executable batch files, e.g., 
C:\Program Files\TPS\tps3.bat which you can use to start TPS after it 
has been built.

After Lisp says "FINISHED", enter (exit).

6.  If for some reason make-tps-windows.lisp fails to compile and build
TPS (and ETPS), you can look at make-tps-windows.lisp to try to figure
out how to build it by hand.  The remaining steps are an outline
of what is needed.

6A.  If make-tps-windows.lisp did not create files tps3.sys and
etps.sys, then rename tps3.sys.windows.example to tps3.sys and rename
etps.sys.windows.example to etps.sys.  You may want to edit the value
of the constant expert-list to include your user name.  In Windows,
the user name is often "ABSOLUTE", which is already included on the
list.

If the TPS directory is something other than C:\Program Files\TPS\,
then you will need to edit tps3.sys and etps.sys by replacing each
"C:\\Program Files\\TPS\\" with "whatever\\".
Also, you will need to edit the
files tps-compile-windows.lisp, tps-build-windows.lisp (and
etps-compile-windows.lisp and etps-build-windows.lisp if you intend to
use etps) by replacing the line
  (setq tps-dir "C:\\Program Files\\TPS\\"))
by 
  (setq tps-dir "whatever\\"))

6B.  Make sure the bin directory is empty.
If you have previously built tps or etps, start by 
sending all files from the bin directory to the Recycle Bin.

6C.   Run Lisp.  Load the tps-compile-windows.lisp
file from C:\Program Files\TPS\:

> (load "C:\\Program Files\\TPS\\tps-compile-windows.lisp")

This will compile the lisp source files in the C:\Program Files\TPS\lisp
folder into the C:\Program Files\TPS\bin folder.

6D.   Exit and restart lisp.  Load the tps-build-windows.lisp
file from C:\Program Files\TPS\:

> (load "C:\\Program Files\\TPS\\tps-build-windows.lisp")

If you try to load tps-build-windows.lisp after loading
tps-compile-windows.lisp without restarting Lisp, you will probably
get an error because packages are being redefined.  So, it
is important to exit and start a new Lisp session before
loading tps-build-windows.lisp.  

The end of tps-build-windows.lisp calls tps3-save, which
saves the image file.  Under Allegro, this should be tps3.dxl.
(The name and location of the image file is determined by
the values of sys-dir and save-file in tps3.sys.)

6E.  Repeat steps (6B-6D) using etps-compile-windows.lisp and etps-build-windows.lisp.

6F.  If you have a Java compiler, use it to compile the java files in
C:\Program Files\TPS\java\tps
and then
C:\Program Files\TPS\java\
(See the TPS User manual chapter on setting things up.)

6G.  Create batch files such as tps3.bat containing something like
@echo off
call "C:\<lisppath>\alisp.exe" -I "C:\Program Files\TPS\tps3.dxl"

You need the quotes because Windows easily gets confused about spaces
in pathnames.

6H.  If you have compiled the java code and make-tps-windows.lisp
did not create the batch files for starting TPS and ETPS with the 
Java interface, then you can create files like tps-java.bat
containing something like

@echo off
call "C:\<lisppath>\alisp.exe" -I "C:\Program Files\TPS\tps3.dxl" -- -javainterface java -classpath "C:\Program Files\TPS\java" TpsStart


          STARTING TPS

As mentioned above, make-tps-windows.lisp creates batch files
tps3.bat and etps.bat which can be used to start TPS and ETPS
by simply double clicking.  You can also copy these files
to any other folder to start TPS or ETPS from this other folder.
(Each folder may have its own tps3.ini file which TPS will read
when initializing if you start TPS as an expert.)

The following instructions give an alternative way to start TPS
if you are using Allegro Lisp.

1.  Put a copy of the Lisp executable (such as lisp.exe) into the 
C:\Program Files\TPS\ folder, and rename it tps3.exe. 
(You may only need to explicitly change "lisp" to "tps3"
in order to rename lisp.exe to tps3.exe.)

2.  Copy acl*.epll or acl*.pll (or similarly named files) 
from the Allegro Lisp directory to the TPS directory.  
You may also need to copy a license file *.lic
from the Allegro Lisp directory to the TPS directory.

3.  Double-click on tps3.exe to start up TPS.  This will automatically
find tps3.dxl as the image (since it is in the same directory and has the
same root name).  If Allegro complains that some file isn't found,
look for that file under the Allegro Lisp directory and copy it to
the TPS directory.

	See the last Chapter of the Tps User Manual for more information.


 ----------------------------------------------------------
         TROUBLESHOOTING

If your lisp is not in the list above, you may need to change some of the 
system-dependent functions. The features used by TPS are "tops-20", "lucid", 
":cmu", "kcl", "allegro" and "ibcl".  System-dependent files include

CORE.EXP 
Some symbols (such as memq) need to be exported in some lisps
(such as Allegro).

SPECIAL.EXP
Contains symbols which cannot be exported in some lisps. These 
are found by trial and error.

BOOT0.LISP, BOOT1.LISP
Contain some lisp and operating-system dependent functions and macros,
like file manipulation.

TOPS20.LISP
Redefining the lisp top-level, saving a core image, exiting, etc.

TPS3-SAVE.LISP
Some I/O functions which should work for Unix lisps, also the original
definition of expert-list.

TPS3-ERROR.LISP
Redefinitions of trapped error functions, as used in ETPS.



	REPORTING BUGS AND SUGGESTED IMPROVEMENTS

  If you find any bugs, or can suggest any improvements, please let us
know.  Of course, we can't promise to fix everything.

Contact:

Internet: Andrews@CMU.EDU

US Mail: Professor Peter Andrews
         Department of Mathematical Sciences
	 Carnegie Mellon University
	 Pittsburgh PA 15213-3890
	 USA


