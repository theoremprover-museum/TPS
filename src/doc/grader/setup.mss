@ChapBTPS(Setting Up)@label(setup)

@begin(comment)
@Section(Compiling and Building @HGRADER)
This section is intended for system maintainers and 
may be skipped by most users of @GRADER.

To compile source files for @GRADER (MacLisp version), retrieve the
core image @t[<TPS>CTPS.EXE], and issue the commands below in order (replacing
file-to-be-compiled by the name of the file to be compiled but without the 
extension, or version number).

You may insert this text in a file, and submit a batch job instead.
An implicit assumption is that the file to be compiled has the extension .lsp
You can, of course, compile any number of files - just keep typing
the filenames when prompted (before typing ^G)

@Begin(TPSExample)
@@conn <tps>
@@ctps
*(load "gr-lsp.fasl")
*(maklap)
*file-to-be-compiled
*^G
*(quit)
@End(TPSExample)

To rebuild @GRADER:

@Begin(Enumerate)
Retrieve the files needed to rebuild @GRADER. These are listed in the file
@t[<TPS>GR-NEEDED.FILES].


Submit a batch job by issuing the following command to the monitor:
@Begin(TpsExample)
submit <tps>build-grader.ctl
@End(TPSExample)

If the core image is successfully created, rename it to @t[<TPS>GRADER.EXE].
@End(Enumerate)

The text for this manual is in
 /afs/cs/project/tps/doc/grader/manual.mss 
@end(comment)

@Section(Variables: CHG-VARS and GR-REVIEW)
@label(chg-vars)
Just after entering @GRADER, you should make sure that the values of @GRADER 's
variables are appropriate.
The default values set by @GRADER are listed below.  Note that all file names
are strings, i.e., enclosed by double quotes.

@begin(verbatim)
@tabclear
@tabset(.5 inches, 2.5 inches)
@\ @GrVarI(course-name)           @\ "course"
@\ @GrVarI(grade-file)            @\ "@GrVar(course-name).grades"
@\ @GrVarI(totals-grade-file)     @\ "@grVar(course-name).totals"
@\ @GrVarI(old-grade-File)        @\ "@GrVar(course-name).xgrades"
@\ @GrVarI(old-totals-grade-file) @\ "@GrVar(course-name).xtotals"
@\ @GrVarI(letter-grade-file)     @\ "@grvar(course-name).letter"
@\ @GrVarI(etps-file)             @\ ""
@\ @GrVarI(statistical-options)   @\ (-sdev- -median- -mean-)
@\ @GrVarI(print-n-digits)        @\ 0
@\ @GrVarI(cal-percentage)        @\ NIL
@\ @GrVarI(letter-grade-flag)     @\ T
@\ @GrVarI(new-item)              @\ NIL
@\ @GrVarI(drop-min)              @\ NIL
@\ @grvarI(due-date-flag)         @\ T
@\ @GrvarI(default-penalty-fn)    @\ NO-PENALTY

@end(verbatim)

These default values may be redefined by having a file in your directory
called @wt(grader.ini) which contains lines of the 
form  (@t{setq} @i{variable} '@i{new-default}). This is how variables are
set in @LISP, the native tongue of @GRADER. @GRADER will read this file
when you start up.

@Exempli[@t{(setq letter-grade-flag 'nil)} would make @t(NIL) the default for
@GrVar(letter-grade-flag). @ @ @! @ @^@/@Gratia] 

A typical @wt(grader.ini) file for a course called LOGIC would look like this:
@begin(verbatim)
(setq course-name "logic")
(setq default-penalty-fn 'penalty%10)
(setq cal-percentage T)
(setq statistical-options nil)
@end(verbatim)
 
One thing to note is that setting the value of @grvar(course-name) in your
@wt(grader.ini) file will have the effect of changing the values of
all the variables above whose values depend on that of @grvar(course-name).

Once inside @GRADER, you may change or examine the values of these variables
by using either of the following commands:

@Hand[@GrComI(CHG-VARS) displays a menu similar to the above list and asks
you to select the variable you wish to modify.]

@Begin(Hand)
@GrComI(GR-REVIEW) puts you into a new top-level called @REVIEW.
The basic commands in @REVIEW are:
@End(Hand)
@begin (itemize)
@RevComI(SUBJECTS) lists the subjects into which the variables are classified.

@RevComI(LIST) gives the current settings of all the variables in a subject.

@RevComI(SETFLAG) lets you reset the value of a variable.

@RevComI(UPDATE) lets you reset the value of any or all variables in a subject.

@RevComI(LEAVE) leaves @REVIEW and returns to @GRADER with the new variable
settings.
@end(itemize)
@Exempli[A sample @REVIEW session follows:]

@Begin(TPSExample)
<Gr1>gr-review

<R1>subjects 
GR-FILENAMES GR-MISC 
<R2>update 
SUBJECTLIST (SUBJECTLIST):  [No Default]>gr-misc
Subject: GR-MISC
CAL-PERCENTAGE [NIL]>
COURSE-NAME [COURSE]>"victoria"
DEFAULT-PENALTY-FN [NO-PENALTY]>
DROP-MIN [NIL]>
DUE-DATE-FLAG [T]>
LETTER-GRADE-FLAG [T]>
NEW-ITEM [NIL]>
PRINT-N-DIGITS [0]>
STATISTICAL-OPTIONS [(-MEAN- -MEDIAN- -SDEV-)]>  @^
<R3>leave

[Left REVIEW.]
<Gr2> @Gratia

@End(TPSExample)

Again, changing the value of @grvar(course-name) inside @GRcom(CHG-VARS) or
@REVIEW will cause changes in the names of the files which depend upon it.

Filenames may be specified by
@wt{"@i{name}.@i{ext}"}, or simply
@wt{"@i{name}"}.  The quotation marks must be typed.

@Section(The Grade-File: CREATE-GRADEFILE)
@label(creating) Once the variables are set to your liking, you will
next need to create a @GrVarI(grade-file) for each course. This
@GrVarI(grade-file) should be created before the course begins.
Subsequent @GrVarI(grade-files) for the course are generated from this initial
@GrVarI(grade-file).

To create this @GrVarI(grade-file), you must first create a file (with
extension @wt[.lst]) containing a list (enclosed in parentheses)
of student-ids for each student in the course. 
The form of these student-ids depends on whether you
intend to use the @GrCom(ETPS-GRADE) command. If you do, @GRADER will
need to know the names the computer gives your students. Thus the
student-ids, in this case, will be lists of the form:
@LispCode{(@i{student-name} @i{student-user-id})}

@Exempli[The student file class.lst for our hypothetical course on the
Victorian Era might look like:]

@LispCode{
((ruskin-john jr03) 
 (arnold-matthew ma24)
 (dickens-charles cd51)
 (gaskell-elizabeth eg21) @ @ @ @ @  @^
 (carlyle-thomas tc32)
 (disraeli-benjamin bd79)) @Gratia}

If you do not intend to use the @GrCom(ETPS-GRADE) command, the
student-ids will just be the @i{student-names}.

@Exempli[In this case the student file might look like:]

@begin(verbatim)
(
ruskin-john
arnold-matthew 
dickens-charles 
gaskell-elizabeth
carlyle-thomas
disraeli-benjamin
)
@end(verbatim)


Since @LISP uses blanks to separate elements in a list, and commas and
periods for more esoteric parsing, none of these characters may
appear in any @i{student-name} or @i{student-user-id}.
We recommend using hyphens between first and last names.

@Begin(Hand)
@GrComI(CREATE-GRADEFILE) can now be used to create your
@GrVarI(grade-file). Once this command is invoked, you will be prompted for
the name of the class-list file you just formed. @GrCom(CREATE-GRADEFILE)
will order the students alphabetically by name and form a 
@GrVarI(grade-file), using that class list.
@End(Hand)

@Exempli[Running @GrCom(CREATE-GRADEFILE) on the student list in the previous
example produced a @GrVarI(grade-file) with the following contents.]

@Begin(TPSExample)
(
(NAME                ID)
(TYPE                **)
(--MAX--             **)
(-MEAN-              **)
(-MEDIAN-            **)
(-SDEV-              **)
(-WEIGHT-            **)
(ARNOLD-MATTHEW    MA24)
(CARLYLE-THOMAS    TC32)
(DICKENS-CHARLES   CD51)
(DISRAELI-BENJAMIN BD79)
(GASKELL-ELIZABETH EG21)
(RUSKIN-JOHN       JR03)
)
(;;;Record of late assignments: 
()
;;;Record of penalty functions and due-dates: 
()
;;;List of actual names@ @ @ @ @ @ @ @  @^
NIL) @Gratia
@End(TPSExample)

@Section(Precautions and Protections)
@label(Precautions)


Given the risk of a computer malfunction, it is advisable to keep
a recent, printed copy of these records.
For this, you must use your system's
print command on the @GrVar(totals-grade-file) if you want the totals,
or on the @GrVar(grade-file) if you want the grades for each assignment.

@GRADER writes over the old versions of its files when it  creates new
versions. Make copies of these  files with new names if you wish to keep the
old versions.

Don't forget to keep your grade files in a protected directory, where
unauthorized persons cannot access them.

@begin(comment)
Protection of files is usually a system procedure which is,
understandably, not accessible within programs. This procedure is also
unique to the system you use. However, the DEC-20's on which @GRADER
is currently available all use the same protection code. The code uses
six digits grouped by pairs. The first pair indicates the access given
to you, the second to users in your group (however defined) and the
last to all other users. A protection code is assigned to each
directory and to each file. The protection code for the directory
takes precedence, but it is standard procedure to make the second and
third pairs refer to the protection codes of the individual files.
With full access given to you at the directory level, this is achieved
with a 774040 directory protection code (see chart below).

The commands for setting these codes are also the same. They are 
@t(SET DIRECTORY PROTECTION) and @t(SET FILE PROTECTION). However,
the establishment of user-groups are different; and it is this
procedure which is most important for our purposes. You should
invoke @t(HELP PROTECTION) at system level to find the best
procedure for you on your system.

The meaning of code digits is given in the following chart.

@Begin(TPSExample)


		DIRECTORY PROTECTION DIGITS
               -----------------------------

Digits			Permit

 77		Full access to the directory
 40		Access according to protection on individual files.
 10		Connecting to directory without password.  Undeleting
                files.  Expunging directory. Changing times, dates, and
                accounting information.
 04		Creating files in the directory.
 00		No access


       		 FILE PROTECTION DIGITS
               --------------------------

Digits			Permit

 77		Full access to the file
 40		Reading the file; this allows users to copy the file.
 20		Writing the file; "writing" does NOT include any modification
                (e.g. editing with EMACS) which results in a new generation of
                the file.
 10		Executing the file; "executing" means using the RUN command
                with an .EXE file - to use the EXECUTE command requires read
                access as well as execute access.
 04		Appending to the file
 02		Listing the file specifications using the DIRECTORY
                command.
 00		No access
@End(TPSExample)

A safe protection code for the grade files is 770000. Since programs run
by students presumably append to the file @grvar(etps-file), it would need a
protection  such as 770404.


Allowing @LISP expressions gives the user access to programming
power, but it presents the danger that the user might redefine
functions which are crucial to @GRADER or @BTPS. So if something bizarre
occurs, this may be the problem; leave and re-enter @GRADER to get
a clean copy of the program.

@end(comment)

There is another danger to which @GRADER is susceptible.
Among its inputs are the names of students, exercises and
exercise-types. These names become variables in @GRADER.
In addition, @GRADER has certain program variables which
subtly affect the way it works. Together with the @LISP constants,
@t(T) and @t(NIL), these constitute the global objects of
@GRADER. That is, if any two of these have the same name,
then a value given to one will be given to the other.
Clearly, this is not an advisable circumstance and should be avoided.
