@part(SystemCmds, root "ETPS.MSS")
@ChapterPh(System Commands) @label(system)
@section(Communication)
@begin(description)
@IndexCommand(HELP) @i(subject)@\ Will list help available on the subject. The help
messages for inference rules can be very long; you may wish to set the flag  
@IndexFlag(SHORT-HELP) to T, to prevent this. (The default value of this flag is 
NIL.)

@indexCommand(?)@\ Will list all available commands.

@IndexCommand(LIST-RULES)@\ Will list all the available inference rules.

@IndexCommand(PROBLEMS)@\ Lists all exercises available in @ETPS, and shows
which are practice exercises for which @IndexCommand[ADVICE] is available.
Also lists theorems which can be asserted with the @T(ASSERT)
command.@label(problems)

@IndexCommand(NEWS)@\ Will list all bugs fixed, changes, improvements, 
et cetera. The most recent @ETPS news items are announced whenever you start @ETPS.

@IndexCommand(REMARK) @I(string)@\ Will mail the @I(string)
to the teacher, or will include it as a comment in a
recordfile created in the teacher's directory.

@end(description)
@section(Starting and Finishing)
@label(start-finish)
@begin(description)
@begin(comment)
@indexCommand(TERMINAL) @I(termtype)@\ @i(Termtype) can be any one of 
the following:
@Begin(description,Spread 0)
@T(C100)@\ HDS Concept 100

@T(Fox)@\ Perkin-Elmer 1100

@T(Glass)@\ Any display terminal with backspace

@T(H19)@\ Heath H-19 (or Zenith)

@T(MB4)@\ Minibee 4

@T(T1061)@\ Teleray 1061

@T(VT100)@\ DEC VT100 in ANSI mode

@T(VT52)@\ DEC VT52 (DecScope, has blue and red keys)

@T(UNKNOWN)@\ Anything else.
@End(Description)
@end(comment)
@IndexCommand(EXERCISE) @I(label)@\ Set up the proof outline to 
attempt the proof of exercise @i(label) from the logic text.

@IndexCommand(PROVE) @I(gwff0 label line)@\ Similar to @T(EXERCISE), but lets 
you prove your own wff.  @I(label) is the name of the proof, @I(line) is
number of the last line of the proof, i.e.,  the number of the line which
will assert the theorem.

@IndexCommand(PROOFLIST)@\ Gives a list of all the completed or partially 
completed proofs in memory, any of which may be @T(RECONSIDER)ed.

@IndexCommand(RECONSIDER) @I(label)@\ Allows you to return to a previous proof.
@I(label) is the name that you gave the proof when you originally started it 
using @T(EXERCISE) or @T(PROVE); all of the proofs that you have worked on
in the current session with ETPS may be @T(RECONSIDER)ed.

@IndexCommand(CLEANUP)@\ Deletes, in the current completed proof, unnecessary 
lines and redundant lines introduced by the @T(SAME) rule. This command is
applicable only if the proof is complete.
@t(CLEANUP) asks for confirmation before actually deleting lines.

@IndexCommand(DONE)@\ Signals that you think that you have 
completed the current proof.  @ETPS will not believe you if you are not
really done.  The @T(DONE) command appends a message to the recordfile
in the teacher's directory.  If you fail to use it, you may not get
credit for your work.

@Indexcommand(SUMMARY)@\ 
Tells the user what exercises have been completed.

@IndexCommand(EXIT)@\ Leave @ETPS. See Section @REF(reentering) for some information 
on  reentering @ETPS. This command will automatically close open work files.
@seealso[Primary="Quitting",Other="@t(EXIT)"]
@seealso[Primary="Quitting",Other="@t(END-PRFW)"]
@seealso[Primary="Quitting",Other="@t(OK)"]
@seealso[Primary="Quitting",Other="@t(LEAVE)"]
@indexentry[key="Quitting",entry="Quitting"]

@Indexcommand(HISTORY) @i{n} @i{reverse}@\ 
Show history list.  Shows the N most recent events; N defaults to 
the value of @indexflag(HISTORY-SIZE), showing entire history list.  REVERSE defaults 
to NO; if YES, most recent commands will be shown first.

@IndexCommand(ALIAS) @i{name} @i{def}@\ 
Define an alias DEF for the symbol NAME.  Works just like the
alias command in the Unix csh.  If the value of NAME is *ALL*, all
aliases will be printed; if the value of DEF is the empty string, then
the current alias definition of NAME will be printed.  See UNALIAS.

@IndexCommand(UNALIAS) @i{name}@\ 
Remove an alias for the symbol NAME.  Like the Unix csh
unalias, except that NAME must exactly match the existing alias; no
filename completion is done.

See Section @ref(aliases) for more discussion of aliases.

@end(description)

@section(Starting the Java Interface)@label(JavaInterface)

There is a Java interface for @ETPS running under Allegro Lisp
(version 5.0 or greater).
Special symbol fonts,
proofwindows (see Section @ref(Proofwindows)) and editor windows (see Section @ref(locating))
are available when using the Java interface.

@Begin(Description)

@IndexCommand(JAVAWIN) @\ Start the Java interface.
This should open a Java window with menus, a display area
for @ETPS output, and possibly a prompt at the bottom of
the window.  
All @ETPS output after the Java window opens will be
printed into the Java window.  
Also, all user input must be entered via the Java window,
either using the menus or using the prompt at the bottom of the window.
To enter input into the prompt, the user may need to click on
the prompt area to bring it into focus.

@End(Description)

@section(Proofwindows)@label(Proofwindows)

When @ETPS is running under X-windows or through the Java interface
(see section @ref(JavaInterface)), it is possible to start up separate
windows displaying the current subproof (which is described in Section
@ref(proofstatus) and can be printed on the screen with the
@indexcommand(^P) command), the current subproof plus line numbers
(which can be printed with the @indexcommand(^PN) command) and the
complete proof (which can be printed with the @indexcommand(PALL)
command).  These windows will be automatically updated as commands are
issued to modify the proof interactively.  (By scrolling up in these
windows, you can see the previous displays.)  The windows may be moved
around and resized by the usual methods for manipulating windows.
@IndexCommand(PSTATUS) will update the proofwindows.  Printing in the
proofwindows can be modified by changing the flags
@IndexFlag(PROOFW-ACTIVE), @IndexFlag(PROOFW-ALL),
@indexflag(PROOFW-ACTIVE+NOS), and @IndexFlag(PRINTLINEFLAG).  For
more information about the proofwindows, type @t(HELP BEGIN-PRFW).

@Begin(Description)

@IndexCommand(BEGIN-PRFW) @\ Begin proofwindow top-level; 
open Current Subproof, Current Subproof & Line Numbers, and Complete Proof
windows. 


@IndexCommand(END-PRFW) @\ End proofwindow top-level; close all
the proofwindows.

@End(Description)

If you forget to use the @IndexCommand(END-PRFW) command before
issuing the @IndexCommand(EXIT) command to leave ETPS, the proofwindows
may not disappear. To get rid of such a window, put the cursor into it
and hit @t(^C) (control-C). 


@section(The Current Subproof)@label(proofstatus)

@ETPS maintains a list which contains the @I(status) @index(status)
information for the current proof outline.  The status information
consists of the @I(planned lines) @index(planned) (lines not yet
justified) and the lines (called @I(sponsoring lines)@index(sponsor))
which @ETPS thinks you may wish to use in the proof of the associated
planned line.  The planned line which is the focus of current
attention and its sponsoring lines constitute the Current
Subproof. This is displayed in the windows mentioned in Section
@ref(Proofwindows) and can be printed on the screen by using the
@indexcommand(^P) command.  The following commands allow you to
examine and modify the current subproof @index(current subproof) and
the status information.

@Begin(Description)
@IndexCommand(PSTATUS)@\ This will print the status information in the form
@Begin(Verbatim)
(@I(p)@-{1} @I(l)@-{11} ... @I(l)@-{1n}) ... (@I(p)@-{m} @I(l)@-{m1} ... @I(l)@-{mk})
@End(Verbatim)
where @I(p)@-{1} ... @I(p)@-{m} are the  planned lines and
the rest of each list are the sponsors for the planned line.  The first
list corresponds to the ``current'' plan. In addition, it'll issue a message
if you are currently saving work.

@IndexCommand(SUBPROOF) @I(pline)@\ Tells @ETPS that you now wish to
focus on the planned line @I(pline).  This changes the current
subproof; it mainly affects the displays in the proofwindows, the
results of the @T(^P) and @T(^PN) commands, and the defaults offered
for outline manipulations commands.

@IndexCommand(SPONSOR) @I(pline existing-linelist)@\ Tells @ETPS 
to add the lines in the list @I(existing-linelist)
of existing proof lines to the list of sponsors for the planned line
@I(pline).

@IndexCommand(UNSPONSOR) @I(pline existing-linelist)@\ Tells @ETPS 
to remove the lines in the list @I(existing-linelist)
of existing proof lines from the list of sponsors for the planned line
@I(pline).

@end(description)


@section(Saving Work)@label(savework)
@Begin(Description)
@IndexCommand(SAVEPROOF) @i(filename)@\ Saves the current natural deduction 
proof to the specified file in a form in which it can be restored.  Use
RESTOREPROOF to restore the proof. Overwrites the file if it already exists.

@IndexCommand(RESTOREPROOF) @i(filename)@\ Reads a natural deduction proof
from a file created by SAVEPROOF
and makes it the current proof.  A security feature prevents the 
restoration of saved proofs which have been altered in any way.

@IndexCommand(SAVE-WORK) @i(filename)@\ Starts to save subsequent commands
in the save-work file @i(filename).  Notice that this is not necessary,
unless you want to specify your own filename before starting an exercise
or if you did a @T(STOP-SAVE) some time before.  A typical use would be
to switch save-work files when you are done with one exercise and are starting
the next one without leaving @ETPS.  The extension of @i(filename) defaults
to @t(.WORK).

@IndexCommand(STOP-SAVE)@\ Stops saving into the current save-work file.
All commands that have been given but not yet saved will be written out
to the file.

@begin(comment)
@IndexCommand(SAVE-INTERVAL) @i(n)@\ Sets an internal variable which controls
how many commands to accumulate before writing them out into the save-work
file.  The default is 5, so you lose at most 5 commands if the computer
happens to crash.
@end(comment)

@IndexCommand(RESTORE-WORK) @i(filename) @i(show-lines) @i(exec-print)
@i(outfile)@\ 
Executes commands from @i(filename) and continues to save in that file.
When the end of the file
is reached, you will be back at @ETPS' command level.  @i(show-lines)
controls whether proof lines will be shown when restoring the proof.
This is very time consuming, therefore the default is @t(NO).  @i(exec-print)
controls whether printing commands will be executed when restoring the
proof.  These are commands like @t(PLINE), @t(PRINTPROOF),
@t(HELP), or @T(PROBLEMS).
The default is @t(NO).  @i(outfile) is the name of a file where the
commands and the output is echoed to, while they are re-executed.  The
default is @T(TTY:), the terminal, so you can see how far @ETPS has
progressed.  To speed up the process you may select @T(NUL:).
@T(RESTORE-WORK) will not re-execute any of the following: @T(EXIT),
@T(RESUME-SAVE), @T(RESTORE-WORK), @T(EXECUTE-FILE), 
@T(SAVE-WORK), @T(STOP-SAVE).  They
usually don't make sense when reading commands from a save-work file.
If you aborted a command with a Ctrl-C, the Ctrl-C will be in the file
and will abort the execution of the commands.

@begin(comment)
@IndexCommand(RESUME-WORK)@\ Resumes saving work after a temporarily interrupted
session when the @ETPS fork was saved and reentered.
Note that to use this command you must have been saving work 
immediately before leaving @ETPS.  Work is saved 
in (appended to) the same file that was used in the previous session.
@end(comment)

@indexcommand(RESUME-SAVE)@\ Use this command to resume saving commands into the most recent
save-work file.  Unlike @t(RESTORE-WORK), this command doesn't
execute commands from the file, but simply appends subsequent commands to
the file.  You may not use this command if you are already saving work.
Also, you may run into trouble if you forgot to save some commands. 

@comment<Note that this command is not necessary if you left @ETPS by using the
@t(EXIT) command and returned using @t(REENTER); in this case @ETPS
will automatically resume saving (if you were saving before you
exited).  If, however, you left @ETPS by using the @t(EXIT) command
and returned by using @t(CONTINUE), you must use this command to
resume saving work.>


@IndexCommand(EXECUTE-FILE) @i(filename) @i(show-lines) @i(exec-print)
@i(outfile)@\ Works like @t(RESTORE-WORK), but does not continue saving
into a file after executing the commands in the file.

@IndexCommand(SCRIPT) @i{scriptfile} @i{if-exists-append}@\ 
Saves a transcript of session to a file.

@IndexCommand(UNSCRIPT)@\ 
Closes the most recent file opened with the SCRIPT command.

@End(Description)
@section(Printing)@label(printing)
@Begin(Description)
@IndexCommand(DEPTH) @I[n]@\ @i(n) is a number. This command causes all 
subformulas at depth greater than @i(n) to be  printed as @T(&).
For example the wff @wt{"FORALL x FORALL y FORALL z.P x y z"}
will be printed as below after the command @T(DEPTH 4):
@wt{FORALL x FORALL y FORALL z.&}.
This command may save time in printing huge formulas, particularly in 
higher-order logic.

@IndexCommand(PW) @I(gwff)@\ Print @I(gwff).

@IndexCommand(PWSCOPE) @I(gwff)@\ Print @I(gwff) with all brackets restored. 
This is sometimes useful if you are not sure which connective has precedence
over another.

@IndexCommand(PLINE) @I(line)@\ Print a specified line.

@IndexCommand(PL) @I(lower upper)@\ Print all lines in the range from 
@I(lower) to @I(upper).

@IndexCommand(PL*) @I(print-ranges)@\ Print all proof lines in given ranges.

@IndexCommand(PPLAN) @I(pline)@\ Prints the planned line @I(pline) and all 
of its sponsors. A similar effect can be achieved with the 
@comment<special control-character>
@t(^P), provided @I(pline) is the current planned line.
@T(SUBPROOF) will change the current planned line.  See 
Section @ref(proofstatus) for more
information on @T(SUBPROOF).

@IndexCommand(^P)@\ Same as @T(PPLAN) for the current planned line.
@comment{	@t(^P) 
can be used at any time: on top-level, while typing in arguments, etcetera.}
Note that @t(^P) is not a control-character, but two characters @t(^) @t(P)
followed by a @t(<Return>).

@indexcommand(^PN)@\ As for @T(^P), but also prints the line numbers (only) of 
all the other lines of the proof. @t(^PN) is not a control character, but three
characters @t(^), @t(P) and @t(N).

@IndexCommand(PALL)@\ Print all the lines in the current outline.

@IndexCommand(PRINTPROOF) @I(filespec)@\ This will print the current proof into
a file.

@IndexCommand(SCRIBEPROOF) @I(filespec)@\ This will also print the current proof
into a file, but uses special symbols.
In order to print this file, you must first run it through @t(SCRIBE).
@I(filespec) has the same format as in @T(PRINTPROOF).
The extension defaults to @T(MSS).

@IndexCommand(TEXPROOF) @i{filespec}@\ 
Print the current proof into a tex file.
After leaving tps, run this .tex file through @t(TeX) and print the resulting
file.

@End(Description)


@section(Rearranging the Proof)
This section describes commands which rearrange the proof outline,
which is described in Section @ref(InfeRead).
The first two commands are frequently useful for starting over a part
of the proof after you realize you have tried a wrong approach.
@Begin(Description)
@IndexCommand(DELETE) @I(existing-linelist)@\ Delete the lines in
@I(existing-linelist) from the 
proof.  If you delete a hypothesis line, all lines which use this hypothesis
will also be deleted.  If a line justifying another line is deleted, the
justification of that line is changed to @wt{PLAN@i(n)}.
Lines are shown as they are deleted.

@indexcommand(DELETE*) @I(ranges) @\ Delete ranges of lines from the proof
outline. 

@IndexCommand(PLAN) @i(existing-line)@\ Change a justified line back to 
a planned line. 
@End(Description)
The next few commands allow you to change the numbers of lines in the proof,
or even change the order of lines, as long as the conclusion of a rule of
inference comes after the justifying lines. All references to line numbers
are changed automatically whenever the numbers are changed.
@Begin(Description)
@IndexCommand(MOVE) @I(from to)@\ Moves a line in the proof.  @ETPS checks to 
make sure the move is legal, i.e., the lines justifying a given line appear
before it in the proof.

@indexcommand(MOVE*) @I(range-to-move new-start)@\ Move all proof lines in given range to begin at new start
number, but preserving the relative distances between the lines.

@IndexCommand(RENUMBERALL) @I(increment)@\ Renumbers all the lines in the proof
with an increment of @I(increment).
@end(Description)

As a proof is constructed, new lines must be inserted into the outline
and given new line numbers between occupied line numbers. A space
allotted for this task is called a gap. @Index(Gaps) 
Gaps are indicated in the outline by ellipses (...) and may be adjusted by
the command @IndexCommand(MODIFY-GAPS).

@Begin(description)

@IndexCommand(INTRODUCE-GAP) @I(existing-line) @I(increment)@\ Introduce a 
new gap (or increase an existing gap) above  @i(existing-line)
by increasing the line numbers  by @i(increment) of all 
lines beginning with line @i(existing-line).

@indexcommand(MODIFY-GAPS) @i(lower upper)@\ Removes unnecessary gaps
in line numbers from the proof structure. Also, gaps with length less than
@i(lower) have their length  increased to @i(lower), while gaps with length
greater than @i(upper) have their length decreased to @i(upper). @i(lower)
must be less than or equal to @i(upper).

@IndexCommand(SQUEEZE)@\ Removes unnecessary gaps in line numbers from the proof
structure. Leaves necessary gaps (those just above planned lines) alone.
@End(Description)

There is no UNDO @index(UNDO) command in ETPS. Usually one can undo the
results of commands fairly easily by such measures as deleting lines
from the proof. However, if this seems complicated,
the following procedure can often be used
to restore the proof to one of its previous states.
@ETPS is probably creating a save-work file. Execute the
@IndexCommand(STOP-SAVE) command, make a backup copy of the save-work
file for safety,  edit the save-work file by deleting the commands
you wish you had not executed, then start a new @ETPS and use
@IndexCommand(RESTORE-WORK) with the edited save-work file.

@Section(Proof Assistance)

@Begin(Description)

@IndexCommand(ADVICE)@\ Initially gives hints based on the current structure
of the proof. The next time it is executed, it suggests the inference command
based on the previous hint. It repeats this flip-flopping between hints
and suggestions until it has no more suggestions. Advice may not be
available for some exercises. @ETPS will tell you if advice cannot be given
and ask for confirmation if the advice would deduct points from your score.

@IndexCommand(CHECK-STRUCTURE)@\ Finds those lines which are not integrated
into the proof and suggests their deletion. These lines are deduced lines
which have not been used to justify another line and are no longer supports
for any planned line. In addition, @t(CHECK-STRUCTURE) looks for extraneous
hypotheses in each of the lines of the proof.

@End(Description)
@begin(comment)
@section(Miscellaneous Commands)
@begin(description)
@IndexCommand(n)@\ where n is a number between 1 and 10. It'll retype the nth
command.

@IndexCommand(ALLOCATE)@\ This command increases the list space. Use this only
if you have to (for example when your printing is  aborted because of
insufficient list space).

@IndexCommand(SETL) @I(label rwff)@\ define @I(label) so it refers to @I(rwff).  From
now on, you may use @I(label) as an @T(RWFF), for example in command arguments.
@end(description)
@end(comment)	

@section(Higher-Order Logic)
@Begin(Description)
@IndexCommand(PWTYPES) @I(gwff)@\ Print @I(wff) with type symbols.  

@begin(group)
@IndexCommand(SHOWTYPES)@\ From now on show the types of all wffs.

@IndexCommand(SHOWNOTYPES)@\ From now on suppress types of all wffs.
@end(group)
@End(Description)

@section(Flags and Review)@label(flags)

Many aspects of @ETPS are controlled by flags. @index(flags)
Most of the time you can ignore these, but if you wish to
change some aspect of @ETPS (such as the widths of the lines in a proof),
you may be able to do so by changing the value of a flag
(such as @IndexFlag(RIGHTMARGIN)). 
HELP @i(flag) will provide
information about a particular flag.
Use the REVIEW top-level 
to find what flags are available. 
Enter @indexcommand(?) for a list of all the commands
in this top-level; the following is just a selection of those available.

@Begin(Description)
@IndexCommand(REVIEW)@\ Enter the review top-level.

@IndexCommand(SETFLAG)@\ Change the value of a flag.

@IndexCommand(SUBJECTS)@\ Each flag is associated with one or more
subjects; this command lists all the known subjects. Some of these subjects
may be irrelevant to @ETPS, but  used in a larger system of which
@ETPS is a component; you can ignore them.

@IndexCommand(LIST) @I(subjects)@\ List all the flags associated with these subjects.

@IndexCommand(LEAVE)@\ Return to the main top-level.

@End(Description)
