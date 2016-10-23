@ChapterPh(Top-Level Commands)
The internal name of this category is 
MEXPR.
A top-level command can be defined using DEFMEXPR.
Allowable properties are: @t{ARGTYPES}, @t{WFFARGTYPES}, @t{WFFOP-TYPELIST}, @t{ARGNAMES}, @t{ARGHELP}, @t{DEFAULTFNS}, @t{MAINFNS}, @t{ENTERFNS}, @t{CLOSEFNS}, @t{PRINT-COMMAND}, @t{DONT-RESTORE}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexMexpr(BEGIN-PRFW)@\
Begin proofwindow top level.
Open Current Subproof, Current Subproof & Line Numbers, and Complete Proof
windows with text size determined by the value of the flag CHARSIZE.
Printing in various windows can be modified by changing the flags
PROOFW-ACTIVE, PROOFW-ALL, PROOFW-ACTIVE+NOS, BLANK-LINES-INSERTED 
and PRINTLINEFLAG.
The initial size of the windows can be modified with the flags
PROOFW-ALL-HEIGHT, PROOFW-ALL-WIDTH, PROOFW-ACTIVE-HEIGHT,
PROOFW-ACTIVE-WIDTH, PROOFW-ACTIVE+NOS-HEIGHT, and 
PROOFW-ACTIVE+NOS-WIDTH; after the windows are open, they can simply 
be resized as normal. PSTATUS will update the proofwindows manually
if necessary. 
Close the proofwindows with END-PRFW.

@IndexMexpr(DO-GRADES)@\
Invoke the grading package.

@IndexMexpr(ED) @i{edwff}@\
Enter the editor on a given wff.
Editor windows may be initialized, depending the values of the flags 
EDWIN-TOP, EDWIN-CURRENT, EDWIN-VPFORM. The flags BLANK-LINES-INSERTED and 
CHARSIZE determine the layout of these windows. The flags 
EDWIN-{CURRENT,TOP,VPFORM}-WIDTH and EDWIN-{CURRENT,TOP,VPFORM}-HEIGHT 
determine the intial size of these windows; they may be resized after they are
opened in the usual way.
WARNING: Since editing is non-destructive, nothing is done with the result
of the editing process!

@IndexMexpr(END-PRFW)@\
End proofwindow top level; close all open proofwindows.

@IndexMexpr(EXT-MATE)@\
Enter the EXT-MATE top level for building and manipulating
extensional expansion dags (see Chad E. Brown's thesis).

@IndexMexpr(EXT-SEQ)@\
Enter the EXT-SEQ top level for building and manipulating
extensional sequent derivations (see Chad E. Brown's thesis).

@IndexMexpr(HISTORY) @i{n} @i{reverse}@\
Show history list.  Shows the N most recent events; N defaults to 
the value of HISTORY-SIZE, showing entire history list. Values of N that 
are greater than HISTORY-SIZE have the same effect as the default value. 
REVERSE defaults to NO; if YES, most recent commands will be shown first.

@IndexMexpr(LIB)@\
Enter the library top-level.

See Also: UNIXLIB (an alternative library top level)

@IndexMexpr(MATE) @i{gwff} @i{deepen} @i{reinit} @i{window}@\
Begin an expansion proof for a gwff.

@IndexMexpr(MODELS)@\
Enter the MODELS top level for working with standard models
in which the base types (hence all types) are a power of 2.

@IndexMexpr(MTREE) @i{gwff} @i{deepen} @i{reset} @i{window}@\
Begin to enter the mating tree top level.

@IndexMexpr(POP)@\
Return from a top level started with PUSH.

@IndexMexpr(PUSH)@\
Start a new top level. This command is almost useless,
except from within a prompt (e.g. one can type PUSH in the middle 
of converting an etree to a ND proof interactively, call SCRIBEPROOF,
and then type POP to return to the conversion).

@IndexMexpr(REVIEW)@\
Enter REVIEW to examine and change flags or parameters.

@IndexMexpr(REWRITE) @i{p2} @i{p1} @i{a} @i{b} @i{p2-hyps} @i{p1-hyps}@\
Rewrite a line of the current natural deduction proof in the
REWRITING top level. When finished rewriting, use OK to leave the REWRITING top
level, modifying the main proof accordingly.

@IndexMexpr(REWRITE-IN) @i{theory} @i{p2} @i{p1} @i{a} @i{b} @i{p2-hyps} @i{p1-hyps}@\
Rewrite a line in the REWRITING top level using a particular
theory.

@IndexMexpr(REWRITING)@\
Enter the REWRITING top level.

@IndexMexpr(TEST) @i{gwff} @i{deepen} @i{reinit} @i{window}@\
Enter the test top level. In this top level, the user can search
for an optimal mode in which to prove a particular theorem, by defining
a list of flags to be varied and then running matingsearch repeatedly 
with different flag settings.
It only works if the value of the flag DEFAULT-MS is one of thse:
MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, MS93-1, MS98-1, 
MS03-7 and MS04-2.

@IndexMexpr(UNIFORM-SEARCH) @i{gwff} @i{window} @i{mode} @i{slist} @i{modify}@\
Enter the test top level to search for any mode that will prove
a given theorem. The mode provided by the user should list flag settings
that are not to be varied, and the searchlist provided by the user should
list all of the flags to be varied. The default settings for the mode and
searchlist are UNIFORM-SEARCH-MODE and UNIFORM-SEARCH-2.
If you opt for the searchlist to be automatically modified, TPS will inspect
the given wff to check whether it is first order, whether it contains any 
definitions, whether it contains any equalities (and if so whether the LEIBNIZ
and ALL instantiations are different), and whether it has any possible primitive
substitutions, and will then remove or modify any unnecessary flags from the 
searchlist (respectively, unification bounds will be deleted, REWRITE-DEFNS will
be deleted, REWRITE-EQUALITIES will be deleted or modified, and DEFAULT-MS will
be changed to a search without option sets).
Also, if you opt for the searchlist to be modified and there is a proof of this
theorem in memory, AUTO-SUGGEST will be run and you will be asked whether to 
modify the searchlist using the results it provides.

After entering the test top level with this command, type GO ! to start 
searching for a successful mode.

@IndexMexpr(UNIFORM-SEARCH-L) @i{goal} @i{support} @i{line-range} @i{window} @i{mode} @i{slist} @i{modify}@\
Enter the test top level to search for any mode that will prove
a given lemma. (Compare DIY-L) The mode provided by the user should list flag 
settings that are not to be varied, and the searchlist provided by the user should
list all of the flags to be varied. The default settings for the mode and
searchlist are UNIFORM-SEARCH-MODE and UNIFORM-SEARCH-2.
If you opt for the searchlist to be automatically modified, TPS will inspect
the given wff to check whether it is first order, whether it contains any 
definitions, whether it contains any equalities (and if so whether the LEIBNIZ
and ALL instantiations are different), and whether it has any possible primitive
substitutions, and will then remove or modify any unnecessary flags from the 
searchlist (respectively, unification bounds will be deleted, REWRITE-DEFNS will
be deleted, REWRITE-EQUALITIES will be deleted or modified, and DEFAULT-MS will
be changed to a search without option sets).
After entering the test top level with this command, type GO ! to start 
searching for a successful mode.

@IndexMexpr(UNIFY)@\
Enter the unification top-level. The user can define disagreement sets
using the command ADD-DPAIR available in the unification top-level. If you
are entering from the MATE top level, the unification tree associated with 
the active-mating is passed on to the unification top-level.
Any changes made to this tree are destructive. Applicable only for
a higher-order unification problem. Uses MS88-style unification.

@IndexMexpr(UNIXLIB)@\
Enter the library top-level with a unix style interface.

The value of the flag CLASS-SCHEME determines what classification scheme
is used to determine the virtual directory structure.

If the flag UNIXLIB-SHOWPATH is T, the prompt will be
<<CLASSSCHEME>:<PATH TO CLASS><num>>

If the flag UNIXLIB-SHOWPATH is NIL, the prompt will be
<LIB:<CLASS><num>>

See Also: LIB, PSCHEMES, CLASS-SCHEME, UNIXLIB-SHOWPATH, CD, LS, PWD, LN, RM,
MKDIR, FETCH, SHOW@End(Description)

@Section(Help)

@Begin(Description)
@IndexMexpr(?)@\
Type ? to obtain a list of possible options.

@IndexMexpr(??)@\
Type ?? to get general help on TPS, command completion
and history substitution.

@IndexMexpr(ABBREVIATIONS) @i{show-defns}@\
This command will list the names of all abbreviations available in TPS.

@IndexMexpr(ENVIRONMENT)@\
Helps to find out about TPS' current environment, i.e.
categories of TPS objects, commands, argument types, logical constants, etc.

@IndexMexpr(HELP) @i{keyword}@\
Give information about a TPS object like a command or argument type.
The amount of help given for inference rules may be changed by setting the
flag SHORT-HELP.

Online help can be found at the web site:
 
http://gtps.math.cmu.edu/tps.html

Typing "?" will show you all available commands at this level.

The web site includes online documentation as well
as postscript manuals.

@IndexMexpr(HELP*) @i{keywords}@\
Give information about each of a list of TPS objects.
This is equivalent to doing HELP on each of them.
The amount of help given for inference rules may be changed by setting the
flag SHORT-HELP.

@IndexMexpr(HELP-GROUP) @i{keywords}@\
Give information about a group of TPS objects; specifically,
given the name of a category, a context, or a top level, 
list the help messages for every object in that class.
If given a list of names, it will list the help messages
for all the objects that fall into the intersection of these
classes (e.g. HELP-GROUP (MEXPR REWRITING) will show all the 
top-level commands in the context REWRITING).
NOTE: Remember that the name of a context is not necessarily
the name that prints on the screen; do HELP CONTEXT
to show their real names.

@IndexMexpr(LIST-RULES)@\
List all rules with their suggestion priority.

@IndexMexpr(LIST-RULES*)@\
List all rules with their intermediate rule definition help

@IndexMexpr(OOPS) @i{position} @i{replacement}@\
Replace the word at a given position in the previous line
with another word. Positions start from 0, and the substituted-for
command will be entered into the command history list, so for example:
<9>HELP GR-FILENAMES
<10>OOPS 0 LIST      (calls LIST GR-FILENAMES instead)
<11>OOPS 1 GR-MISC   (calls LIST GR-MISC)

@IndexMexpr(PROBLEMS) @i{show-defns}@\
This command will list the names of all exercises available in ETPS.

@IndexMexpr(SEARCH) @i{phrase} @i{search-names}@\
Look for a key phrase in all help strings (or just all names) 
of TPS objects. See also KEY, in the review top level (where it searches 
through the flags) and the library top level (where it searches through
the library objects).@End(Description)

@Section(Collecting Help)

@Begin(Description)
@IndexMexpr(CHARDOC) @i{output-style} @i{styles} @i{filename}@\
List the special characters of certain output styles in a TeX or Scribe
file. The output file can be processed by TeX or Scribe and will have 
multicolumn format.

@IndexMexpr(COLLECT-HELP) @i{modules} @i{categories} @i{filename}@\
Collect help for the specified modules into a file. 
Prints out a # every time it finds a help message, and a * every time it 
finds a TPS object with no help message.

@IndexMexpr(HELP-LIST) @i{category} @i{filename}@\
List all help available for objects of the given category
into a file.

@IndexMexpr(HTML-DOC) @i{directory}@\
Produce HTML documentation in the specified directory.
This requires an empty directory and a lot of disk space, and
will take quite some time to produce.

@IndexMexpr(LATEX-DOC) @i{category-list} @i{context-list} @i{filename}@\
Produce Latex documentation about the specified categories.

@IndexMexpr(LATEX-QUICK-REF) @i{filename}@\
Produce a quick Latex reference to the rules available in TPS.

@IndexMexpr(OMDOC-ASSERTION) @i{wff} @i{wff-name} @i{filename}@\
Print a wff in OMDoc notation.

@IndexMexpr(OMDOC-CLASS-SCHEME) @i{name}@\
Print the library into OMDoc files using the given
Classification Scheme to collect library items into theories.

@IndexMexpr(OMDOC-LIB)@\
Print the library into OMDoc files
in OMDoc notation.

@IndexMexpr(OMDOC-PROOF) @i{filename}@\
Print the current proof into an OMDoc file
in OMDoc notation.

@IndexMexpr(QUICK-REF) @i{filename}@\
Produce a quick reference to the rules available in TPS.

@IndexMexpr(SCRIBE-DOC) @i{category-list} @i{context-list} @i{filename}@\
Produce Scribe documentation about the specified categories.@End(Description)

@Section(Concept)

@Begin(Description)
@IndexMexpr(LOADKEY) @i{key} @i{mssg}@\
Load one of the function keys f1-f10 on a concept terminal with a string.

@IndexMexpr(RESET)@\
Put a Concept terminal into correct mode and load the function keys.@End(Description)

@Section(Starting and Finishing)

@Begin(Description)
@IndexMexpr(ALIAS) @i{name} @i{def}@\
Define an alias DEF for the symbol NAME.  Works just like the
alias command in the Unix csh.  If the value of NAME is *ALL*, all
aliases will be printed; if the value of DEF is the empty string, then
the current alias definition of NAME will be printed.  See UNALIAS.

@IndexMexpr(CLEANUP)@\
If the proof is complete, will delete unnecessary lines 
from a proof.  It may also eliminate or suggest eliminating
unnecessary hypotheses.
If the proof is incomplete, will do a partial cleanup in which
only unnecessary lines justified by SAME will be removed.

@IndexMexpr(DONE)@\
Signal that the current proof is complete.

@IndexMexpr(EXERCISE) @i{excno}@\
Start the proof of a new exercise.

@IndexMexpr(EXIT)@\
Exit from TPS.

@IndexMexpr(NEWS)@\
Type TPS news on the terminal.

@IndexMexpr(PROVE) @i{wff} @i{prefix} @i{num}@\
Start a new proof of a given wff.

@IndexMexpr(RECONSIDER) @i{prefix}@\
Reconsider a proof. The following proofs are in memory:

For more details, use the PROOFLIST command.


@IndexMexpr(REMARK) @i{remark}@\
Send a message to the teacher or maintainer.

@IndexMexpr(SUMMARY)@\
Tells the user what exercises have been completed.

@IndexMexpr(UNALIAS) @i{name}@\
Remove an alias for the symbol NAME.  Like the Unix csh
unalias, except that NAME must exactly match the existing alias; no
filename completion is done.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexMexpr(BUILD-PROOF-HIERARCHY)@\
This command builds hierarchical information into the proof outline.
The information includes associations between lines and linear chains
of inferences which trace the consequences of the most recent hypothesis
of a line.  That is, a line 

ln) Hn,m |- an

would be associated with a linear chain of lines l1,...,ln
where m is the line corresponding to the most recent hypothesis and
the proof would justify the modified lines

l1) H1,m |- l1
l2) H2,l1 |- l2
l3) H3,l2 |- l3  
. . .
ln) Hn,ln-1 |- ln

where H1 < H2 < . . . < Hn (subset relation). 

That is, we trace the consequences of the hypothesis m to the
consequence ln.  Such a linear chain is on one level of the hierarchy.
One level down on the hierarchy would be the linear chains associated
with each of the lines used to justify l1,...,ln (except those which
appear in the chain l1,...,ln).  If the proof is complete,
then lines l1 and m will be the same.

Lines without hypotheses are also associated with such
"linear chains", following the rule that l1 < l2 if
the proof justifies the inference l1 |- l2.

The resulting hierarchy information is used by PBRIEF, EXPLAIN,
and PRINT-PROOF-STRUCTURE to help users focus on the logical
structure of a proof.

@IndexMexpr(DEPTH) @i{num}@\
Causes all subformulas at depth greater than n to be printed as &.

@IndexMexpr(EXPLAIN) @i{line} @i{depth}@\
This command explains a line of a proof outline.
In particular, the command BUILD-PROOF-HIERARCHY builds
dependency information into a proof outline which allows
the proof outline to be viewed as a hierarchy of subproofs (see
help for BUILD-PROOF-HIERARCHY).  The command EXPLAIN shows
the lines included in the levels of this hierarchy (to the 
specified depth) starting at the level associated with the 
specified line.  Some flags which affect the printing include:
PRINT-COMBINED-UIS, PRINT-COMBINED-UGENS, PRINT-COMBINED-EGENS,
and PRINT-UNTIL-UI-OR-EGEN.

@IndexMexpr(FIND-LINE) @i{wff} @i{vars} @i{meta}@\
Find all lines matching a certain wff, up to
alphabetic change of bound variables and (possibly) 
alphabetic change of a given list of free variables.
Optionally, you can treat the remaining free variables
as matching any given term (as you might do if you were
asserting an axiom).
e.g. (suppose P is an abbreviation or constant):
FIND-LINE "P a" () NO       finds all lines that say "P a"
FIND-LINE "P a" ("a") NO    also finds "P x" and "P y"
FIND-LINE "P a" () YES      finds all the above, plus 
                              "P [COMPOSE f g]"
FIND-LINE "a x" ("x") YES   finds all lines of the form
                              "SOME-TERM some-var"

@IndexMexpr(PALL)@\
	Print all the lines in the current proof outline.

@IndexMexpr(PBRIEF) @i{depth}@\
This command prints a proof outline, hiding some lines.
In particular, the command BUILD-PROOF-HIERARCHY builds
dependency information into a proof outline which allows
the proof outline to be viewed as a hierarchy of subproofs (see
help for BUILD-PROOF-HIERARCHY).  The command PBRIEF shows
the lines included in the top levels of this hierarchy (to the 
specified depth). PBRIEF is essentially a call to the command EXPLAIN
with the last line of the proof outline as the LINE argument (see help 
for EXPLAIN).  Some flags which affect the printing include:
PRINT-COMBINED-UIS, PRINT-COMBINED-UGENS, PRINT-COMBINED-EGENS,
and PRINT-UNTIL-UI-OR-EGEN.

@IndexMexpr(PL) @i{num1} @i{num2}@\
	Print all proof lines in a given range.

@IndexMexpr(PL*) @i{print-ranges}@\
Print all proof lines in given ranges.

@IndexMexpr(PLINE) @i{line}@\
	Print a specified line.

@IndexMexpr(PPLAN) @i{pline}@\
	Print a planned line and all its supports.

@IndexMexpr(PRINT-PROOF-STRUCTURE)@\
This prints the structure of the proof outline.  The
structure is generated by BUILD-PROOF-HIERARCHY.  Linear chains
of line numbers are printed which indicate the logical chains of
inferences.  Each link in a linear chain is indicated by an
arrow (l1)->(l2) where l1 and l2 are line numbers.  If line l2
does not follow in a single step from l1 (i.e., by a single application 
of an inference rules), then PRINT-PROOF-STRUCTURE will also show the
linear chains of inference used to justify (l1)->(l2).  Some lines
(such as those without hypotheses and planned lines) are
exceptions.  These top level lines are sometimes printed alone 
(instead of in arrow notation).  This could be read TRUE->(l) 
to maintain consistent notation, but the notation (l) appears 
more readable in practice.

@IndexMexpr(PRW) @i{gwff}@\
Print real wff. Turns off special characters
(including FACE definitions), infix notation, and dot 
notation, and then prints the wff.

@IndexMexpr(PW) @i{gwff}@\
Print gwff.

@IndexMexpr(PWSCOPE) @i{gwff}@\
print gwff with all brackets restored.

@IndexMexpr(PWTYPES) @i{gwff}@\
Prints a wff showing types.

@IndexMexpr(SHOWNOTYPES)@\
Suppress the printing of types on all wffs.

@IndexMexpr(SHOWTYPES)@\
From now on show the types on all wffs.

@IndexMexpr(TABLEAU) @i{line}@\
Print the part of the proof which justifies the given line,
in a natural deduction tableau format.

@IndexMexpr(^P)@\
Print current plan-support pair in the proof.

@IndexMexpr(^PN)@\
Print current plan-support pair in the proof, as in ^P, but 
also print just the line numbers of the other lines in the proof.@End(Description)

@Section(Saving Work)

@Begin(Description)
@IndexMexpr(EXECUTE-FILE) @i{comfil} @i{execprint} @i{outfil} @i{stepping}@\
Execute commands from a SAVE-WORK file.
Call this from the main top level or the proofwindows top level of TPS.
Note that this will not save subsequent commands in the same file,
which distinguishes it from RESTORE-WORK.
In the cases where EXECUTE-FILE doesn't work, one can usually just 
load the .work file into an editor and then cut and paste it, whole, 
into the TPS window.
Single-stepping only works between commands on the main top level;
it will not stop at prompts which are internal to a command, nor between
commands on a different top level. To force a work-file to stop in
such a place, use the PAUSE command when creating the work file.
If you are single-stepping through a file, you can 
abort at any time by typing ^G<RETURN>.

@IndexMexpr(FINDPROOF) @i{name}@\
Searches your home directory and the directories listed in
SOURCE-PATH, looking for a proof whose name contains the given string.

@IndexMexpr(FINISH-SAVE)@\
Finishing saving work in a file.
The difference between STOP-SAVE and FINISH-SAVE is: 
the former is temporary because you can use RESUME-SAVE
to resume saving work into the same file; the latter closes 
the output stream, so you can not save work into the same 
file after executing it.

@IndexMexpr(PAUSE)@\
Force a work file to stop and query the user. PAUSE, like ABORT, is valid
both as a top-level command and as a response to a prompt; it prints the 
message "Press RETURN, or Ctrl-G RETURN to abort.", waits for such a response 
from the user, and then repeats the original prompt.
This command is of no use unless a work file is being created; see EXECUTE-FILE 
for more details.

@IndexMexpr(RESTORE-WORK) @i{comfil} @i{execprint} @i{outfil}@\
Execute commands from a SAVE-WORK file and continue
to save in that file. See EXECUTE-FILE for more information.

@IndexMexpr(RESTOREPROOF) @i{savefile}@\
Reads a natural deduction proof from a file created by SAVEPROOF
and makes it the current proof.  A security feature prevents the 
restoration of saved proofs which have been altered in any way.
Retrieve any definitions which are used in the proof and stored in the
library before restoring the proof. If you don't specify a directory,
it will first try your home directory and then all the directories 
listed in SOURCE-PATH.

@IndexMexpr(RESUME-SAVE)@\
Use this command to resume saving commands into the most recent
save-work file.  Unlike RESTORE-WORK, this command doesn't
execute commands from the file, but simply appends subsequent commands to
the file.  You can not use this command if you are already saving work.
Also, you may run into trouble if you forgot to save some commands. 

@IndexMexpr(SAVE-FLAGS-AND-WORK) @i{savefile}@\
Start saving commands in the specified file, first storing all
flag settings.

@IndexMexpr(SAVE-SUBPROOF) @i{savefile} @i{lines} @i{subname}@\
Saves part of the current natural deduction proof to 
the specified file in a form in which it can be restored.  
The line ranges specified will be increased to include all the
other lines on which the given lines depend. See the help 
message for LINE-RANGE to find out what a line-range should 
look like. An example list is: 1--10 15--23 28 34--35
Also creates a new proof in memory with the given name, and
makes that the current proof.
Use RESTOREPROOF to restore the proof.
Overwrites the file if it already exists.

@IndexMexpr(SAVE-WORK) @i{savefile}@\
Start saving commands in the specified file. These commands can be
executed subsequently by using EXECUTE-FILE or RESTORE-WORK. If you are 
creating a work file for a demonstration, and need it to pause at certain
points as it is reloaded by TPS, then see the help message for EXECUTE-FILE
for more information on how to do this.

@IndexMexpr(SAVEPROOF) @i{savefile}@\
Saves the current natural deduction proof to the specified file in
a form in which it can be restored.  Use RESTOREPROOF to restore the proof.
Overwrites the file if it already exists.

@IndexMexpr(SCRIPT) @i{scriptfile} @i{if-exists-append}@\
Saves a transcript of session to a file. If the current setting
of STYLE is SCRIBE or TEX, an appropriate header will be output to the 
script file (unless the file already exists).
**NOTE** If you start SCRIPT from a PUSHed top level, be sure to do 
UNSCRIPT before you POP that top level, or your transcript may be lost.
The same also applies to starting SCRIPT from subtoplevels such as MATE;
you can enter further subtoplevels like LIB and ED from the MATE top level,
and SCRIPT will carry on recording, but before leaving the MATE top level 
you should type UNSCRIPT or your work will be lost.

@IndexMexpr(STOP-SAVE)@\
Stop saving commands in a SAVE-WORK file.

@IndexMexpr(UNSCRIPT)@\
Closes the most recent file opened with the SCRIPT command.@End(Description)

@Section(Saving Wffs)

@Begin(Description)
@IndexMexpr(APPEND-WFF) @i{weak-label} @i{help-string} @i{filename}@\
Append a definition of a weak label to a file.  If the file does
not yet exist, it will be created. You may wish to use LIB instead.

@IndexMexpr(APPEND-WFFS) @i{weak-labels} @i{filename}@\
Append the definitions of a list of weak labels to a file.
If the file does not yet exist, it will be created. You may wish to use LIB
    instead.@End(Description)

@Section(Printing Proofs into Files)

@Begin(Description)
@IndexMexpr(PRINTPROOF) @i{filename}@\
Print the current proof into a file.

@IndexMexpr(SCRIBEPROOF) @i{filename} @i{timing}@\
Print the current proof into a MSS file.
After leaving TPS, run this .MSS file through Scribe and print the resulting
file.

@IndexMexpr(SETUP-SLIDE-STYLE)@\
Sets flags to produce slides in scribe style.

@IndexMexpr(SLIDEPROOF) @i{filename}@\
Print the current proof into a MSS file. Use this command to make
slides.  After leaving TPS, run this .MSS file through Scribe and print the 
resulting file.

@IndexMexpr(TEXPROOF) @i{filename} @i{timing}@\
Print the current proof into a tex file.
After leaving tps, run this .tex file through tex and print the resulting
file.

Many flags affect the output of texproof.
See: USE-INTERNAL-PRINT-MODE, TURNSTILE-INDENT-AUTO, TURNSTILE-INDENT,
LATEX-EMULATION, TEX-MIMIC-SCRIBE, PPWFFLAG, DISPLAYWFF, INFIX-NOTATION,
PAGELENGTH, PAGEWIDTH, TEX-BREAK-BEFORE-SYMBOLS, LOCALLEFTFLAG, SCOPE,
ALLSCOPEFLAG, USE-DOT, FIRST-ORDER-PRINT-MODE, FILLINEFLAG, ATOMVALFLAG.@End(Description)

@Section(Proof Outline)

@Begin(Description)
@IndexMexpr(CREATE-SUBPROOF) @i{lines} @i{subname}@\
Creates a new proof in memory from the given lines,
plus all the lines on which they depend, and makes that 
the current proof.

@IndexMexpr(LINE-COMMENT) @i{line} @i{comment}@\
Attach a comment to a given existing line.
The comment will be parsed for gwffs and line numbers
as follows: anything enclosed in # symbols is assumed to
be a gwff, and anything enclosed in $ symbols is assumed
to be the number of an existing line. Line numbers in 
comments will be updated as lines are moved around;
gwffs will be printed in the current STYLE.
Examples:
"1st copy of line $5$, instantiated with #COMPOSE#"
"2nd copy of line $5$, instantiated with ITERATE"
"3rd copy of line $5$, instantiated with #a OR b#"
(The first prints the definition of COMPOSE; the
second prints the word "ITERATE", and the third prints
the given gwff. If line 5 is subsequently renumbered, 
the line number will change in all these comments.)

@IndexMexpr(MERGE-PROOFS) @i{proof} @i{subproof}@\
Merges all of the lines of a subproof into the current 
proof. If EXPERTFLAG is NIL, no line number may occur in both proofs.
If EXPERTFLAG is T, then if a line number occurs in both proofs, the 
lines to which they refer must be the same (with one exception: if 
one is a planned line and the other is the same line with a 
justification, then the justified line will overwrite the planned one).
Compare TRANSFER-LINES.

The following proofs are in memory:

For more details, use the PROOFLIST command.


@IndexMexpr(PROOF-COMMENT) @i{comment}@\
Attaches a comment to the current proof.
The default value is the current comment. Uses the same
comment syntax as LINE-COMMENT; see the help message of that command
for more information. You can see the comments on all the 
current proofs by using PROOFLIST.

@IndexMexpr(PROOFLIST)@\
Print a list of all proofs or partial proofs currently in memory.
Also prints the final line of each proof and the comment, if
any, attached to it.

@IndexMexpr(TRANSFER-LINES) @i{proof} @i{subproof} @i{lines}@\
Copies all of the given lines of a subproof, and
all lines on which they depend, into the current proof. 
If EXPERTFLAG is NIL, no line number may occur in both proofs.
If EXPERTFLAG is T, then if a line number occurs in both proofs, the 
lines to which they refer must be the same (with one exception: if 
one is a planned line and the other is the same line with a 
justification, then the justified line will overwrite the planned one).
Different comments from two otherwise identical lines will be 
concatenated to form the comment in the resulting proof.

This is equivalent to CREATE-SUBPROOF followed by MERGE-PROOFS.

The following proofs are in memory:

For more details, use the PROOFLIST command.

@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexMexpr(PSEQ) @i{prefix}@\
Print a Sequent Calculus Derivation

SEE ALSO: pseq-use-labels, pseql

@IndexMexpr(PSEQL) @i{prefix} @i{lbd} @i{ubd}@\
Print a Sequent Calculus Derivation

SEE ALSO: pseq-use-labels, pseq

@IndexMexpr(SEQ-TO-NAT) @i{sname} @i{prefix}@\
Translates a Sequent Calculus Derivation (possibly with Cuts)
to a Natural Deduction Proof

@IndexMexpr(SEQLIST)@\
Print a list of all sequent calculus derivations currently in memory.@End(Description)

@Section(Search Suggestions)

@Begin(Description)
@IndexMexpr(AUTO-SUGGEST)@\
Given a completed natural deduction proof (which must
be the current dproof; use RECONSIDER to return to an old proof),
suggest flag settings for an automatic proof of the same theorem.

This will also automatically remove all uses of SUBST=
and SYM= from the proof (you will be prompted before this
happens, as it permanently modifies the proof).

This will show all of the instantiations (and primitive substitutions)
that are necessary for the proof, and suggest settings for 
NUM-OF-DUPS, MAX-MATES, DEFAULT-MS, MAX-PRIM-DEPTH, MAX-PRIM-LITS 
and REWRITE-DEFNS

@IndexMexpr(ETR-AUTO-SUGGEST)@\
Given an eproof, suggest flag settings for 
an automatic proof of the same theorem.  Such an eproof may
be the result of translating a natural deduction proof using
nat-etree.

This will show all of the instantiations (and primitive substitutions)
that are necessary for the proof, and suggest settings for 
NUM-OF-DUPS, MS98-NUM-OF-DUPS, and MAX-MATES.@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexMexpr(CLOSE-TESTWIN)@\
Closes the window that displays the test-top and TPS-TEST summary.
Use ..../tps/utilities/vpshow (from a shell, not from TPS) 
to view the output file again.

@IndexMexpr(DEASSERT-LEMMAS) @i{prefix}@\
Combine a collection of natural deduction proofs where
some lines contain ASSERT justifications where the asserted line
has a natural deduction proof into a single natural deduction proof.

@IndexMexpr(DIY) @i{goal} @i{support} @i{window}@\
DO IT YOURSELF. Calls matingsearch procedure specified by the flag 
DEFAULT-MS with specified planned line and supports, then translates the 
resulting proof to natural deduction. Allows some of the output to be sent to 
a separate vpform window (equivalent to issuing the OPEN-MATEVPW command
before typing DIY).

@IndexMexpr(DIY-L) @i{goal} @i{support} @i{window} @i{range}@\
DIY for lemmas. Behaves as for DIY, but puts all new lines into
a specified range rather than scattering them throughout the proof.

@IndexMexpr(DIY-L-WITH-TIMEOUT) @i{goal} @i{support} @i{timeout} @i{window}@\
DIY for lemmas (with timeout).  Calls diy-l with a timeout value
in seconds.  The timeout value applies only to mating search.  That is,
as long as mating search succeeds within the allotted time, merging
and translation to natural deduction can take as long as necessary.

This is only available for TPS running under Lisps with multiprocessing
(e.g., Allegro >= 5.0).

See Also: DIY-L, DIY-WITH-TIMEOUT

@IndexMexpr(DIY-WITH-TIMEOUT) @i{goal} @i{support} @i{timeout} @i{window}@\
DO IT YOURSELF (with timeout).  Calls diy with a timeout value
in seconds.  The timeout value applies only to mating search.  That is,
as long as mating search succeeds within the allotted time, merging
and translation to natural deduction can take as long as necessary.

This is only available for TPS running under Lisps with multiprocessing
(e.g., Allegro >= 5.0).

See Also: DIY, DIY-L-WITH-TIMEOUT

@IndexMexpr(DIY2) @i{goal} @i{support} @i{quiet-run} @i{expu} @i{newcore} @i{output} @i{timing} @i{testwin}@\
DO IT YOURSELF 2.  Tries to prove an existing line
using a variety of given modes.  This essentially combines the
commands TEST-INIT and TPS-TEST.  See the help message for
TPS-TEST for more information about options.

See Also: DIY, DIY-L, DIY2-L, PIY, PIY2, TEST-INIT, TPS-TEST

@IndexMexpr(DIY2-L) @i{goal} @i{support} @i{line-range} @i{quiet-run} @i{expu} @i{newcore} @i{output} @i{timing} @i{testwin}@\
DO IT YOURSELF 2 with line range for new lines.  Tries to prove an existing line
using a variety of given modes.  If successful, the new lines are put into the gap
specified.   This essentially combines the commands TEST-INIT and TPS-TEST.
See the help message for TPS-TEST for more information about options.

See Also: DIY, DIY-L, DIY2, PIY, PIY2, TEST-INIT, TPS-TEST

@IndexMexpr(EPROOFLIST) @i{complete}@\
Print a list of all expansion proofs currently in memory.

@IndexMexpr(MONITOR)@\
Turns the monitor on, and prints out the current monitor 
function and parameters. See NOMONITOR. See also QUERY-USER for an
alternative way to monitor the progress of the matingsearch.
For a list of monitor functions, type MONITORLIST. To change the current
monitor function, enter the name of the desired new monitor function from
the main top level or the mate top level.

@IndexMexpr(MONITORLIST)@\
List all monitor functions.

@IndexMexpr(NOMONITOR)@\
Turns the monitor off, and prints out the current monitor 
function and parameters. See MONITOR.
For a list of monitor functions, type MONITORLIST. To change the current 
monitor function, enter the name of the desired new monitor function from
the main top level or the mate top level.

@IndexMexpr(PIY) @i{wff} @i{prefix} @i{num} @i{window}@\
PROVE IT YOURSELF. Combines the prove command with diy - allowing a choice
of a mode for trying to prove a theorem automatically.

@IndexMexpr(PIY2) @i{wff} @i{prefix} @i{num} @i{quiet-run} @i{expu} @i{newcore} @i{output} @i{timing} @i{testwin}@\
PROVE IT YOURSELF 2.  Tries to prove a theorem
using a variety of given modes.  This essentially combines the
commands PROVE, TEST-INIT and TPS-TEST.  See the help message
for TPS-TEST for more information about options.

See Also: PIY, DIY, DIY-L, DIY2, DIY2-L, TEST-INIT, TPS-TEST

@IndexMexpr(SET-EPROOF) @i{epf}@\
Set the current expansion proof.

To see a list of expansion proofs in memory, use EPROOFLIST@End(Description)

@Section(MS91-6 and MS91-7 search procedures)

@Begin(Description)
@IndexMexpr(SEARCH-ORDER) @i{num} @i{vpf} @i{verb}@\
Generates the first n option sets that will be searched under 
the current flag settings (assuming that the first (n-1) searches fail
because they run out of time rather than for any other reason).
This will show the names and weights of the option sets, the 
primitive substitutions and duplications. 
Note : "Ordinary" duplications are duplications that have not 
had a primsub applied to them. So, for example, "X has 2 primsubs 
plus 3 ordinary duplications" means that the vpform now contains 
five copies of the relevant quantifier, two of which have had 
primsubs applied to them.@End(Description)

@Section(Proof Translation)

@Begin(Description)
@IndexMexpr(ETREE-NAT) @i{prefix} @i{num} @i{tac} @i{mode}@\
Translates the current expansion proof, which is value of internal
variable current-eproof, into a natural deduction style proof. The default
value of the tactic is given by the flag DEFAULT-TACTIC.

@IndexMexpr(NAT-ETREE) @i{prefix}@\
Translates a natural deduction proof, (which must be the current dproof
-- use RECONSIDER to return to an old proof in memory), into an 
expansion proof. This will not work on all proofs: in particular,
proofs containing ASSERT of anything but REFL= and SYM=, proofs 
using rewrite rules and proofs containing SUBST= or SUB= cannot be
translated at present.  

There are several versions of nat-etree.  Set the flag
NAT-ETREE-VERSION to determine which version to use.

In all but the OLD version, the user is given the option of
removing lines justified by SUBST=, SUB=, or SYM= and replacing the justification
with a subproof.  This permanently modifies the proof.  (AUTO-SUGGEST
also gives such an option.)

@IndexMexpr(NORMALIZE-PROOF) @i{prefix}@\
Normalize a natural deduction proof.  The actual procedure
uses DEASSERT-LEMMAS to combine asserted lemmas into one big natural
deduction proof.  This is the converted into a sequent calculus
derivations with cuts.  A cut elimination (which may not terminate
in principle) creates a cut-free proof which is translated back
to a normal natural deduction proof.

To suppress excessive output, try setting the following flags
NATREE-DEBUG, ETREE-NAT-VERBOSE and PRINTLINEFLAG
to NIL and TACTIC-VERBOSE to MIN.

@IndexMexpr(PFNAT) @i{proof}@\
To generate a NATREE from given proof and store it in CURRENT-NATREE. This may
evolve into a command for rearranging natural deduction style proofs.

@IndexMexpr(PNTR)@\
Print out the current natree stored in CURRENT-NATREE. Mainly for
the purpose of debugging.

@IndexMexpr(TIDY-PROOF) @i{old-prfname} @i{new-prfname}@\
Translate a ND proof to an eproof and back again 
(into a proof with a new name) in the hope of tidying it up a 
bit. Equivalent to
NAT-ETREE; MATE ! ; PROP-MSEARCH ; MERGE-TREE ; LEAVE ; 
ETREE-NAT ; CLEANUP ; SQUEEZE@End(Description)

@Section(Unification)

@Begin(Description)
@IndexMexpr(LEAST-SEARCH-DEPTH)@\
Print the least needed unification tree depth for the last proven 
higher-order theorem. Also suggest to lower flags MAX-SEARCH-DEPTH to the least
needed value if they are greater than it.@End(Description)

@Section(Search Analysis)

@Begin(Description)
@IndexMexpr(ELIMINATE-ALL-RULEP-APPS) @i{pfname}@\
Expands applications of RuleP in the current natural deduction proof
into more primitive rules.  This works by calling fast propositional search with
the current flag settings except USE-RULEP is set to NIL.
BASIC-PROP-TAC is used to translate to natural deduction.

This command also eliminates other 'fancy' propositional justifications:
Assoc (Assoc-Left), EquivConj (in favor of EquivImplics),
Imp-Disj-L, Imp-Disj-R, Imp-Disj, Disj-Imp-L, Disj-Imp-R, and Disj-Imp.

See Also: 
ELIMINATE-RULEP-LINE - which eliminates a particular application of RuleP.
ELIMINATE-CONJ*-RULEP-APPS - which does not depend on automatic search.

@IndexMexpr(ELIMINATE-CONJ*-RULEP-APPS) @i{pfname}@\
Expands applications of RuleP in the current natural deduction proof
when they can be replaced by a sequence of IConj or EConj applications.

This reverses the effect of the ICONJ* and ECONJ* tactics which are often
used when translating from an expansion proof to a natural deduction proof.

SEE ALSO: ELIMINATE-ALL-RULEP-APPS, ELIMINATE-RULEP-LINE

@IndexMexpr(ELIMINATE-RULEP-LINE) @i{line}@\
Expands an application of RuleP in the current natural deduction proof
into more primitive rules.  This works by calling fast propositional search with
the current flag settings except USE-RULEP is set to NIL.
BASIC-PROP-TAC is used to translate to natural deduction.

This command can also eliminate other 'fancy' propositional justifications:
Assoc (Assoc-Left), EquivConj (in favor of EquivImplics),
Imp-Disj-L, Imp-Disj-R, Imp-Disj, Disj-Imp-L, Disj-Imp-R, and Disj-Imp.

SEE ALSO: ELIMINATE-ALL-RULEP-APPS, ELIMINATE-CONJ*-RULEP-APPS

@IndexMexpr(SET-BACKGROUND-EPROOF) @i{epr}@\
Sets the background eproof to be used by MS98-TRACE.
These are automatically set when nat-etree is run.@End(Description)

@Section(Tactics)

@Begin(Description)
@IndexMexpr(ECHO) @i{echothing}@\
Echo a string.

@IndexMexpr(USE-TACTIC) @i{tac} @i{tac-use} @i{tac-mode}@\
Use a tactic on the current goal. The default tactic
is given by the flag DEFAULT-TACTIC.@End(Description)

@Section(suggestions)

@Begin(Description)
@IndexMexpr(ADVICE)@\
Give some advice on how to proceed with the current proof.

@IndexMexpr(CHECK-STRUCTURE)@\
Check various structural properties of the current proof.
You will be informed about suspect constellations in the incomplete proof
which may make it difficult for ETPS to provide advice or for you to
finish the proof.

@IndexMexpr(GO)@\
Start producing and applying suggestions until no more are found.
Suggestions are treated according to their priority and the state of
the global parameter GO-INSTRUCTIONS.

@IndexMexpr(GO2) @i{tacmode}@\
Apply all possible invertible tactics, until no more are possible.
This is equivalent to typing USE-TACTIC GO2-TAC NAT-DED.
The amount of output to the main window and the proofwindows is 
determined by the flag ETREE-NAT-VERBOSE.

@IndexMexpr(MONSTRO) @i{tacmode}@\
This is equivalent to typing USE-TACTIC MONSTRO-TAC NAT-DED.
It applies all the same tactics as GO2, and also ui-herbrand-tac.
The amount of output to the main window and the proofwindows is 
determined by the flag ETREE-NAT-VERBOSE.

@IndexMexpr(SUGGEST) @i{pline}@\
Suggest some applicable inference rule for proving a planned line.@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexMexpr(CLOSE-MATEVPW)@\
Closes the window that displays the current vpform and
substitution stack. Use ..../tps/utilities/vpshow (from a shell, 
not from TPS) to view the output file again.

@IndexMexpr(OPEN-MATEVPW) @i{filename}@\
Open a window which will display the current vpform and substitution stack,
if any. The window can be closed with the command CLOSE-MATEVPW. The size 
of the text is determined by the flag CHARSIZE, and the current width of the window 
by the flag VPW-WIDTH. The initial height of the window is determined by VPW-HEIGHT
Use ..../tps/utilities/vpshow to view the file from the monitor level.@End(Description)

@Section(Rearranging the Proof)

@Begin(Description)
@IndexMexpr(ADD-HYPS) @i{hyps} @i{line}@\
Weaken a line to include extra hypotheses.
Adding the hypotheses to the line may cause some lines to
become planned lines.  If possible, the user is given the option 
of adding hypotheses to lines after the given line so that no 
lines will become planned.

@IndexMexpr(DELETE) @i{del-lines}@\
	Delete lines from the proof outline.

@IndexMexpr(DELETE*) @i{ranges}@\
Delete ranges of lines from the proof outline.

@IndexMexpr(DELETE-HYPS) @i{hyps} @i{line}@\
Delete some hypotheses from the given line.
This may leave the given line as a planned line.
The user is given the option of also deleting some hypotheses
from lines after the given line.  If possible, the user
is given the option of deleting some hypotheses from lines
before the given line so that the given line does not become
a planned line.

@IndexMexpr(INTRODUCE-GAP) @i{line} @i{num}@\
Introduce a gap in an existing proof.

@IndexMexpr(LOCK-LINE) @i{line}@\
Prevent a line from being deleted.

@IndexMexpr(MAKE-ASSERT-A-HYP) @i{l}@\
Take a line justified by Assert, change its justification to Hyp,
make lines after it include this as a hypothesis, and perform a 
Deduct at the end so that the new proof does not depend on the Assert.

We may want to use this before calling nat-etree, since this does
not handle most Asserts.

@IndexMexpr(MODIFY-GAPS) @i{num1} @i{num2}@\
Remove unnecessary gaps from the proof structure, and modify 
line numbers so that the length of each gap is neither less than the first
argument, nor greater than the second.

@IndexMexpr(MOVE) @i{old-line} @i{new-line}@\
	Renumber one particular line.

@IndexMexpr(MOVE*) @i{range-to-move} @i{new-start}@\
Move all proof lines in given range to begin at new start
number, but preserving the relative distances between the lines.

@IndexMexpr(PLAN) @i{line}@\
	Change a justified line to a planned line.

@IndexMexpr(RENUMBERALL) @i{num}@\
Renumber all the lines in the current proof.

@IndexMexpr(SQUEEZE)@\
Removes unnecessary gaps from the proof structure.

@IndexMexpr(UNLOCK-LINE) @i{line}@\
The opposite of LOCK-LINE.@End(Description)

@Section(Status)

@Begin(Description)
@IndexMexpr(ARE-WE-USING) @i{linelist}@\
Determines if given lines are being used to justify 
any other lines. Notice that the argument is a list of lines,
not a range (i.e. 1 2 3 4 rather than 1--4).

@IndexMexpr(COUNT-LINES)@\
Show the number of lines in the current proof.

@IndexMexpr(PSTATUS)@\
Give the current status information, i.e. planned lines and their
 supports. If work is being saved, issues an appropriate message.

@IndexMexpr(SPONSOR) @i{pline} @i{linelist}@\
	Add new sponsoring lines to the sponsors of a planned line.

@IndexMexpr(SUBPROOF) @i{pline}@\
	Concentrate on proving a particular planned line.

@IndexMexpr(UNSPONSOR) @i{pline} @i{linelist}@\
      Remove a list of unwanted sponsoring lines from among the 
sponsors of a planned line.@End(Description)

@Section(Miscellaneous Rules)

@Begin(Description)
@IndexMexpr(ASSERT) @i{theorem} @i{line}@\
Use a theorem as a lemma in the current proof.
If the line already exists, ETPS will check whether it is a legal
instance of the theorem schema, otherwise it will prompt for the
metavariables in the theorem schema (usually x or P, Q, ...).

@IndexMexpr(ASSERT2) @i{theorem} @i{line}@\
Use a theorem as a lemma in the current proof.
If the line already exists, ETPS will check whether it is a legal
instance of the theorem schema, otherwise it will prompt for the
metavariables in the theorem schema (usually x or P, Q, ...).
This version of ASSERT ensures correct behaviour for theorems
containing bound variables.

@IndexMexpr(HYP) @i{p2} @i{h1} @i{a} @i{b} @i{p2-hyps} @i{h1-hyps}@\
Introduce a new hypothesis line into the proof outline.

@IndexMexpr(LEMMA) @i{p2} @i{p1} @i{a} @i{b} @i{p2-hyps} @i{p1-hyps}@\
Introduce a Lemma.

@IndexMexpr(SAME) @i{p2} @i{d1} @i{a} @i{p2-hyps} @i{d1-hyps}@\
Use the fact that two lines are identical to justify a planned line.@End(Description)

@Section(Propositional Rules)

@Begin(Description)
@IndexMexpr(ASSOC-LEFT) @i{d1} @i{d2} @i{p} @i{assoc-l} @i{d1-hyps} @i{d2-hyps}@\
Rule to associate a support line leftwards. Use before
calling CASES3 or CASES4.

@IndexMexpr(CASES) @i{p6} @i{d1} @i{p5} @i{h4} @i{p3} @i{h2} @i{b} @i{a} @i{c} @i{p6-hyps} @i{d1-hyps} @i{p5-hyps} @i{h4-hyps} @i{p3-hyps} @i{h2-hyps}@\
Rule of Cases.

@IndexMexpr(CASES3) @i{p8} @i{d1} @i{p7} @i{h6} @i{p5} @i{h4} @i{p3} @i{h2} @i{c} @i{b} @i{a} @i{d} @i{p8-hyps} @i{d1-hyps} @i{p7-hyps} @i{h6-hyps} @i{p5-hyps} @i{h4-hyps} @i{p3-hyps} @i{h2-hyps}@\
Rule of Cases.

@IndexMexpr(CASES4) @i{p10} @i{d1} @i{p9} @i{h8} @i{p7} @i{h6} @i{p5} @i{h4} @i{p3} @i{h2} @i{d} @i{c} @i{b} @i{a} @i{e} @i{p10-hyps} @i{d1-hyps} @i{p9-hyps} @i{h8-hyps} @i{p7-hyps} @i{h6-hyps} @i{p5-hyps} @i{h4-hyps} @i{p3-hyps} @i{h2-hyps}@\
Rule of Cases.

@IndexMexpr(DEDUCT) @i{p3} @i{d2} @i{h1} @i{b} @i{a} @i{p3-hyps} @i{d2-hyps} @i{h1-hyps}@\
The deduction rule.

@IndexMexpr(DISJ-IMP) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to replace a disjunction by an implication.

@IndexMexpr(DISJ-IMP-L) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to replace a disjunction by an implication.

@IndexMexpr(DISJ-IMP-R) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to replace a disjunction by an implication.

@IndexMexpr(ECONJ) @i{d1} @i{d3} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d3-hyps} @i{d2-hyps}@\
Rule to infer two conjuncts from a conjunction.

@IndexMexpr(EQUIV-IMPLICS) @i{d1} @i{d2} @i{r} @i{p} @i{d1-hyps} @i{d2-hyps}@\
Rule to convert an equivalence into twin implications.

@IndexMexpr(ICONJ) @i{p3} @i{p2} @i{p1} @i{b} @i{a} @i{p3-hyps} @i{p2-hyps} @i{p1-hyps}@\
Rule to infer a conjunction from two conjuncts.

@IndexMexpr(IDISJ-LEFT) @i{p2} @i{p1} @i{b} @i{a} @i{p2-hyps} @i{p1-hyps}@\
Introduce a disjunction (left version).

@IndexMexpr(IDISJ-RIGHT) @i{p2} @i{p1} @i{a} @i{b} @i{p2-hyps} @i{p1-hyps}@\
Introduce a disjunction (right version).

@IndexMexpr(IMP-DISJ) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to replace an implication by a disjunction.

@IndexMexpr(IMP-DISJ-L) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to replace an implication by a disjunction.

@IndexMexpr(IMP-DISJ-R) @i{d1} @i{d2} @i{a} @i{b} @i{d1-hyps} @i{d2-hyps}@\
Rule to replace an implication by a disjunction.

@IndexMexpr(IMPLICS-EQUIV) @i{p2} @i{p1} @i{r} @i{p} @i{p2-hyps} @i{p1-hyps}@\
Rule to convert twin implications into an equivalence.

@IndexMexpr(INDIRECT) @i{p3} @i{p2} @i{h1} @i{a} @i{p3-hyps} @i{p2-hyps} @i{h1-hyps}@\
Rule of Indirect Proof.

@IndexMexpr(INDIRECT1) @i{p3} @i{p2} @i{h1} @i{b} @i{a} @i{p3-hyps} @i{p2-hyps} @i{h1-hyps}@\
Rule of Indirect Proof Using One Contradictory Line.

@IndexMexpr(INDIRECT2) @i{p4} @i{p3} @i{p2} @i{h1} @i{b} @i{a} @i{p4-hyps} @i{p3-hyps} @i{p2-hyps} @i{h1-hyps}@\
Rule of Indirect Proof Using Two Contradictory Lines.

@IndexMexpr(ITRUTH) @i{p1} @i{p1-hyps}@\
Rule to infer TRUTH

@IndexMexpr(MP) @i{d2} @i{d3} @i{p1} @i{b} @i{a} @i{d2-hyps} @i{d3-hyps} @i{p1-hyps}@\
Modus Ponens.

@IndexMexpr(RULEP) @i{conclusion} @i{antecedents}@\
Justify the CONSEQUENT line by RULEP using the lines in the
list ANTECEDENTS. 

@IndexMexpr(SUBST-EQUIV) @i{d2} @i{d3} @i{p1} @i{p} @i{r} @i{t} @i{s} @i{d2-hyps} @i{d3-hyps} @i{p1-hyps}@\
Substitution of Equivalence.  Usable when R and P are the same modulo
the equivalence s EQUIV t.@End(Description)

@Section(Negation Rules)

@Begin(Description)
@IndexMexpr(ABSURD) @i{p2} @i{p1} @i{a} @i{p2-hyps} @i{p1-hyps}@\
Rule of Intuitionistic Absurdity.

@IndexMexpr(ENEG) @i{p3} @i{d1} @i{p2} @i{a} @i{p3-hyps} @i{d1-hyps} @i{p2-hyps}@\
Rule of Negation Elimination.

@IndexMexpr(INEG) @i{p3} @i{p2} @i{h1} @i{a} @i{p3-hyps} @i{p2-hyps} @i{h1-hyps}@\
Rule of Negation Introduction

@IndexMexpr(NNF) @i{d1} @i{d2} @i{a} @i{neg-norm} @i{d1-hyps} @i{d2-hyps}@\
Put Wff in Negation Normal Form.

@IndexMexpr(NNF-EXPAND) @i{p2} @i{p1} @i{a} @i{neg-norm} @i{p2-hyps} @i{p1-hyps}@\
Expand Wff from Negation Normal Form.

@IndexMexpr(PULLNEG) @i{p2} @i{p1} @i{a} @i{push-negation} @i{p2-hyps} @i{p1-hyps}@\
Pull out negation.

@IndexMexpr(PUSHNEG) @i{d1} @i{d2} @i{a} @i{push-negation} @i{d1-hyps} @i{d2-hyps}@\
Push in negation.@End(Description)

@Section(Quantifier Rules)

@Begin(Description)
@IndexMexpr(AB*) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to alphabetically change embedded quantified variables.

@IndexMexpr(ABE) @i{d1} @i{d2} @i{y} @i{a} @i{x} @i{s} @i{d1-hyps} @i{d2-hyps}@\
Rule to change a top level occurrence of an existentially
 quantified variable.

@IndexMexpr(ABU) @i{p2} @i{p1} @i{y} @i{a} @i{x} @i{s} @i{p2-hyps} @i{p1-hyps}@\
Rule to change a top level occurrence of a universally quantified
 variable.

@IndexMexpr(EGEN) @i{p2} @i{p1} @i{t} @i{a} @i{x} @i{lcontr} @i{p2-hyps} @i{p1-hyps}@\
Rule of Existential Generalization.

@IndexMexpr(RULEC) @i{p4} @i{d1} @i{d3} @i{h2} @i{y} @i{b} @i{x} @i{a} @i{lcontr} @i{p4-hyps} @i{d1-hyps} @i{d3-hyps} @i{h2-hyps}@\
RuleC

@IndexMexpr(RULEC1) @i{p4} @i{d1} @i{d3} @i{h2} @i{b} @i{x} @i{a} @i{p4-hyps} @i{d1-hyps} @i{d3-hyps} @i{h2-hyps}@\
RuleC1 -- the special case of RULEC where the chosen
variable has the same name as the bound variable.

@IndexMexpr(UGEN) @i{p2} @i{p1} @i{a} @i{x} @i{p2-hyps} @i{p1-hyps}@\
Rule of Universal Generalization.

@IndexMexpr(UI) @i{d1} @i{d2} @i{t} @i{a} @i{x} @i{lcontr} @i{d1-hyps} @i{d2-hyps}@\
Rule of Universal Instantiation.@End(Description)

@Section(Substitution Rules)

@Begin(Description)
@IndexMexpr(SUBSTITUTE) @i{d1} @i{d2} @i{x} @i{t} @i{a} @i{s} @i{d1-hyps} @i{d2-hyps}@\
Rule to substitute a term for a variable.

@IndexMexpr(TYPESUBST) @i{d} @i{p} @i{a} @i{b}@\
Substitute for a type variable in one line to infer another line.
The type variable must not appear in any hypothesis.@End(Description)

@Section(Equality Rules)

@Begin(Description)
@IndexMexpr(EQUIV-EQ) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to infer a line from one which is equal up to 
definitions, lambda conversion, alphabetic change of bound variables 
and the Leibniz definition of the symbol = . You may use the editor 
command EXPAND= to create the desired line from the existing one.

@IndexMexpr(EQUIV-EQ-CONTR) @i{p2} @i{p1} @i{a} @i{instantiate-top-equality} @i{p2-hyps} @i{p1-hyps}@\
Rule to contract the outermost instance of the Leibniz definition of 
equality into instances of the symbol = .

@IndexMexpr(EQUIV-EQ-CONTR*) @i{p2} @i{p1} @i{a} @i{instantiate-equalities} @i{p2-hyps} @i{p1-hyps}@\
Rule to contract all instances of the Leibniz definition of 
equality into instances of the symbol = .

@IndexMexpr(EQUIV-EQ-EXPD) @i{d1} @i{d2} @i{a} @i{instantiate-top-equality} @i{d1-hyps} @i{d2-hyps}@\
Rule to expand the outermost equality using the Leibniz definition.

@IndexMexpr(EQUIV-EQ-EXPD*) @i{d1} @i{d2} @i{a} @i{instantiate-equalities} @i{d1-hyps} @i{d2-hyps}@\
Rule to expand all equalities using the Leibniz definition.

@IndexMexpr(EXT=) @i{p2} @i{p1} @i{x} @i{g} @i{f} @i{p2-hyps} @i{p1-hyps}@\
Rule of Extensionality.

@IndexMexpr(EXT=0) @i{p2} @i{p1} @i{r} @i{p} @i{p2-hyps} @i{p1-hyps}@\
Rule to convert equality at type o into an equivalence.

@IndexMexpr(LET) @i{p5} @i{p4} @i{h3} @i{d2} @i{d1} @i{a} @i{x} @i{c} @i{p5-hyps} @i{p4-hyps} @i{h3-hyps} @i{d2-hyps} @i{d1-hyps}@\
Bind a  variable to a term.

@IndexMexpr(SUBST=) @i{d2} @i{d3} @i{p1} @i{p} @i{r} @i{t} @i{s} @i{d2-hyps} @i{d3-hyps} @i{p1-hyps}@\
Substitution of Equality.  Usable when R and P are the same modulo
the equality s=t.

@IndexMexpr(SUBST=L) @i{d2} @i{d3} @i{p1} @i{p} @i{r} @i{t} @i{s} @i{d2-hyps} @i{d3-hyps} @i{p1-hyps}@\
Substitution of Equality.  Replaces some occurrences of the left hand
side by the right hand side.

@IndexMexpr(SUBST=R) @i{d2} @i{d3} @i{p1} @i{p} @i{r} @i{s} @i{t} @i{d2-hyps} @i{d3-hyps} @i{p1-hyps}@\
Substitution of Equality.  Replaces some occurrences of the right
hand side by the left hand side.

@IndexMexpr(SYM=) @i{p2} @i{p1} @i{a} @i{b} @i{p2-hyps} @i{p1-hyps}@\
Rule of Symmetry of Equality.@End(Description)

@Section(Definition Rules)

@Begin(Description)
@IndexMexpr(EDEF) @i{d1} @i{d2} @i{a} @i{inst-def} @i{d1-hyps} @i{d2-hyps}@\
Rule to eliminate first definition, left to right.

@IndexMexpr(EQUIV-WFFS) @i{d1} @i{d2} @i{r} @i{p} @i{d1-hyps} @i{d2-hyps}@\
Rule to assert equivalence of lines up to definition.

@IndexMexpr(IDEF) @i{p2} @i{p1} @i{a} @i{inst-def} @i{p2-hyps} @i{p1-hyps}@\
Rule to introduce a definition.@End(Description)

@Section(Lambda Conversion Rules)

@Begin(Description)
@IndexMexpr(BETA*) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to infer a line from one which is equal up to lambda conversion
using beta rule (but NOT eta rule) and alphabetic change of bound variables.

@IndexMexpr(ETA*) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to infer a line from one which is equal up to lambda conversion
using eta rule (but NOT beta rule) and alphabetic change of bound variables.

@IndexMexpr(LAMBDA*) @i{d1} @i{d2} @i{b} @i{a} @i{d1-hyps} @i{d2-hyps}@\
Rule to infer a line from one which is equal up to lambda conversion
using both beta and eta rules and alphabetic change of bound variables.

@IndexMexpr(LCONTR*) @i{d1} @i{d2} @i{a} @i{lnorm} @i{d1-hyps} @i{d2-hyps}@\
Rule to put an inferred line into Lambda-normal form using both 
beta and eta conversion.

@IndexMexpr(LCONTR*-BETA) @i{d1} @i{d2} @i{a} @i{lnorm-beta} @i{d1-hyps} @i{d2-hyps}@\
Rule to put an inferred line into beta-normal form.

@IndexMexpr(LCONTR*-ETA) @i{d1} @i{d2} @i{a} @i{lnorm-eta} @i{d1-hyps} @i{d2-hyps}@\
Rule to put an inferred line into eta-normal form.

@IndexMexpr(LEXPD*) @i{p2} @i{p1} @i{a} @i{lnorm} @i{p2-hyps} @i{p1-hyps}@\
Rule to put a planned line into Lambda-normal form using both 
beta and eta conversion.

@IndexMexpr(LEXPD*-BETA) @i{p2} @i{p1} @i{a} @i{lnorm-beta} @i{p2-hyps} @i{p1-hyps}@\
Rule to put a planned line into beta-normal form.

@IndexMexpr(LEXPD*-ETA) @i{p2} @i{p1} @i{a} @i{lnorm-eta} @i{p2-hyps} @i{p1-hyps}@\
Rule to put a planned line into eta-normal form.@End(Description)

@Section(Rewriting commands)

@Begin(Description)
@IndexMexpr(ACTIVATE-RULES) @i{rlist}@\
Activate a list of rewrite rules.
Activating a rule which is already active has no effect.

@IndexMexpr(ACTIVE-THEORY)@\
Show which theory is currently active. Any new derivation in the
REWRITING top level will use this theory.

@IndexMexpr(DEACTIVATE-RULES) @i{rlist}@\
Deactivate a list of rewrite rules.
Deactivating a rule which is already inactive has no effect.

@IndexMexpr(DEACTIVATE-THEORY)@\
Deactivate all the rewrite rules in the active theory.

@IndexMexpr(DELETE-RRULE) @i{rule}@\
Delete a rewrite rule from TPS.

@IndexMexpr(LIST-RRULES)@\
Show all the current rewrite rules.

@IndexMexpr(MAKE-ABBREV-RRULE) @i{name} @i{bidir}@\
Make a rewrite rule corresponding to a known
abbreviation.

@IndexMexpr(MAKE-INVERSE-RRULE) @i{rule} @i{newname}@\
Make the inverse rewrite rule of an existing
rule.

@IndexMexpr(MAKE-THEORY) @i{name} @i{extends} @i{axioms} @i{rrules} @i{other} @i{sign} @i{reflexive} @i{congruence} @i{mhelp}@\
Create a new theory. A theory is defined by (optionally) starting 
from an old theory, and adding rewrite rules and axioms. You can also attach
other library objects to the theory, which will then be loaded with it.
This will also make an abbreviation of the same name.
All of the objects in the theory should be defined in the library.

@IndexMexpr(PERMUTE-RRULES)@\
Permute the list of rewrite rules.

@IndexMexpr(REWRITE-SUPP*) @i{d1} @i{d2} @i{a} @i{apply-rrule-any*} @i{d1-hyps} @i{d2-hyps}@\
Rewrite a supporting line using all rewrite rules 
possible.

@IndexMexpr(REWRITE-SUPP1) @i{d1} @i{d2} @i{a} @i{apply-rrule-any} @i{d1-hyps} @i{d2-hyps}@\
Rewrite a supporting line using the first rewrite 
rule that applies.

@IndexMexpr(SIMPLIFY-PLAN) @i{p2} @i{p1} @i{a} @i{simplify-up} @i{p2-hyps} @i{p1-hyps}@\
Justify a planned line using the first rewrite rule that 
applies.

@IndexMexpr(SIMPLIFY-PLAN*) @i{p2} @i{p1} @i{a} @i{simplify-up*} @i{p2-hyps} @i{p1-hyps}@\
Justify a planned line using the first rewrite rule that 
applies.

@IndexMexpr(SIMPLIFY-SUPP) @i{d1} @i{d2} @i{a} @i{simplify-down} @i{d1-hyps} @i{d2-hyps}@\
Rewrite a supporting line using the first rewrite 
rule that applies.

@IndexMexpr(SIMPLIFY-SUPP*) @i{d1} @i{d2} @i{a} @i{simplify-down*} @i{d1-hyps} @i{d2-hyps}@\
Rewrite a supporting line using the first rewrite 
rule that applies.

@IndexMexpr(UNREWRITE-PLAN*) @i{p2} @i{p1} @i{a} @i{unapply-rrule-any*} @i{p2-hyps} @i{p1-hyps}@\
Justify a planned line using all rewrite rules possible.

@IndexMexpr(UNREWRITE-PLAN1) @i{p2} @i{p1} @i{a} @i{unapply-rrule-any} @i{p2-hyps} @i{p1-hyps}@\
Justify a planned line using the first rewrite rule that 
applies.

@IndexMexpr(USE-RRULES) @i{p2} @i{p1} @i{a} @i{b} @i{p2-hyps} @i{p1-hyps}@\
Rewrite a line. The line may be rewritten several steps,
but rewrites may not be nested.

@IndexMexpr(USE-THEORY) @i{theory}@\
Activate all the rewrite rules in a theory, and
deactivate all other rewrite rules.@End(Description)

@Section(Events)

@Begin(Description)
@IndexMexpr(DISABLE-EVENTS)@\
Disable recording of TPS events. You will need to start a new session
of TPS to enable recording of events after they have been disabled.@End(Description)

@Section(Statistics)

@Begin(Description)
@IndexMexpr(DATEREC) @i{name} @i{type} @i{comment}@\
Records times used in the following processes:
DIY, Mating Search, Merging Expansion Tree, Proof Transformation.
All times recorded are in seconds.
Internal-runtime includes GC-time.
GC-time is garbage-collecting-time.
I-GC-time is Internal-runtime minus GC-time.
DATEREC also records the values of the flags listed in RECORDFLAGS,
and will offer the user the chance to reset the provability 
status of a gwff in the library.

@IndexMexpr(DISPLAY-TIME) @i{name}@\
Show time used in several processes:
display-time diy: show the time used in DIY process
display-time mating: show the time used in mating-search process
display-time merge: show the time used in merging-expansion-tree process
display-time eproof: show the time used in proof-transformation process
display-time all: show all the times above
All times are in seconds.
Internal-runtime includes GC-time.
GC-time is garbage-collecting-time.
I-GC-time is Internal-runtime minus GC-time.@End(Description)

@Section(Maintenance)

@Begin(Description)
@IndexMexpr(CLOAD) @i{file}@\
Compile and load a file.

@IndexMexpr(CLOAD-MODULES) @i{modules}@\
Compile and Load a list of modules.

@IndexMexpr(COMPILE-LIST) @i{directory-list} @i{source-only}@\
Returns a list of files that need to be compiled.

@IndexMexpr(COMPL) @i{filespeclist}@\
Compile 1 or more files.

@IndexMexpr(EXTRACT-TEST-INFO) @i{file}@\
Extract and report information from a file generated by a run of tps-test.
The user has several options for what information to extract.

See Also: TPS-TEST

The options include:

1 - All Theorems Proven
2 - Theorems Proven With Times
3 - Theorems Proven With Successful Modes
4 - Theorems Proven With Times and Successful Modes
5 - Theorems and Modes That Timed Out
6 - Theorems and Modes That Failed


@IndexMexpr(FILETYPE) @i{filename}@\
Type a file on the screen.  TPS will look for the file in a list
of directories.

@IndexMexpr(GENERATE-JAVA-MENUS) @i{filename}@\
Generate Java code for menus.  This command should only be used
by programmers.  See the TPS3 Programmer's Guide.  This should be run and the
resulting code appropriately inserted into TpsWin.java whenever the
menu structure has been changed.

@IndexMexpr(LEDIT)@\
Call the resident Lisp editor (if there is one) inside TPS. 
It takes a filename as an optional argument. In most lisps, this will probably
start up Emacs. In CMU lisp, this will start up Hemlock; use ^X^Z to leave Hemlock 
again. In some lisps, this command may not work at all.

@IndexMexpr(LOAD-SLOW) @i{filename}@\
Step through loading a file.

@IndexMexpr(ORGANIZE)@\
Organizes the ENVIRONMENT help tree (e.g. after loading modules).

@IndexMexpr(QLOAD) @i{filespec}@\
Load the most recent compiled or uncompiled file from your default
directory, home directory, or source path. In general, the following rules
are used to determine whether compiled or uncompiled file should be load in:
(1) If the file name with extension '.lisp', always load the uncompiled source code.
(2) If the file name without extension, then
    (2.1) if both compiled and uncompiled file exist, and 
          (2.1.1) the compiled one is newer, it is loaded in.
          (2.1.2) the uncompiled one is newer, 
                  (2.1.2.1) if the flag 'expertflag' is NIL, always load the uncompiled 
                            source code.
                  (2.1.2.2) if the flag 'expertflag' is T, ask user whether load the uncompiled
                            one, or compile it and load the compiled one then.
    (2.2) if only the compiled one exists, load it in.
    (2.3) if only the uncompiled one exists, do the same as case (2.1.2)

@IndexMexpr(SETUP-ONLINE-ACCESS)@\
SETUP-ONLINE-ACCESS allows a user to set up a file of userids and passwords
for remote access to a TPS server over the web.  For example, this can
be used by a teacher to set up a file of userids and passwords
for a class to use ETPS online.

See Also: USER-PASSWD-FILE

@IndexMexpr(SYS-LOAD) @i{modulelist}@\
Load all the modules in the given list, whether they
are loaded already or not.

@IndexMexpr(TEST-INIT)@\
Initialize the flag TEST-THEOREMS to test a collection
of theorems on a collection of modes.  This command should be
followed by TPS-TEST which actually tries to prove the theorems
with the modes.

There are currently several possibilities:

1.  Set TEST-THEOREMS to test a given set of theorems on
a given set of modes.  The default set of modes is determined
by the value of the flag GOODMODES.

2.  Set TEST-THEOREMS to test the set of modes given by the flag
GOODMODES on theorems that have a bestmode in the library
(determined by DEFAULT-LIB-DIR and BACKUP-LIB-DIR) but are not 
known to be provable by some mode in the GOODMODES list.

3.  Set TEST-THEOREMS to test a set of modes given by the flag
GOODMODES on all the theorems the modes are supposed to prove.
(This tests whether a list of GOODMODES is still complete 
with respect to the corresponding list of theorems.)

4. Set TEST-THEOREMS to test all of the best
modes known to the library on all the theorems listed with
the best modes. By default, this will choose the first
mode listed for each theorem in the bestmodes.rec file; if you
choose to use multiple modes then it will test each theorem
with all of the modes listed for it in that file.  The examples
are listed in order from quickest to longest.
(This checks that all the theorems associated with bestmodes 
can still be proven by these bestmodes.)

@IndexMexpr(TLIST) @i{symbol}@\
Use a help function to display all of the property list of a symbol.

@IndexMexpr(TLOAD) @i{filespec}@\
Load the most recent compiled or uncompiled file from your default
directory, home directory, or source-path. In general, the following rules are
used to determine whether compiled or uncompiled file should be load in:
(1) If both compiled and uncompiled file exist, and 
    (1.1) the compiled one is newer, it is loaded in.
    (1.2) the uncompiled one is newer, then
          (1.2.1) if the global variable core::*allow-compile-source* is T, the
                  name of the file contains extension 

@IndexMexpr(TPS-TEST) @i{stop-on-success} @i{mate-only} @i{record} @i{moderec} @i{quiet-run} @i{expu} @i{newcore} @i{modify} @i{output} @i{timing} @i{testwin}@\
Attempt to prove a list of theorems.

The list of theorems, with the modes to be used, is stored as (theorem . mode) 
pairs in the flag TEST-THEOREMS. These theorems and modes will be fetched from
the library, if they cannot be found in TPS and if you have a library. You 
should set DEFAULT-LIB-DIR and BACKUP-LIB-DIR appropriately. You can only do
DATEREC after each theorem if you have a library you can write to. 

The first argument STOP-ON-SUCCESS decides whether TPS-TEST should stop
trying to prove a particular theorem with different modes after one
mode has succeeded.  If this is T, then after TPS-TEST proves THM with MODE1,
where (THM . MODE1) is on TEST-INIT, TPS-TEST will not try to prove (THM . MODE2)
for any (THM . MODE2) on TEST-INIT.  It will however, continue to try to prove
other theorems on TEST-INIT with different modes (if there are any).

Quiet running uses the mode QUIET to switch off as much screen output as possible.

You can EXPUNGE between proofs (this will reduce the amount of memory
required, but will mean that other expansion proofs in the memory may
be lost; it will also re-assert your default flag values between each
proof).  Expunging does not really recover all the space used by TPS,
so many repeated proof attempts will result in running out of memory.
To remedy this situation, TPS-TEST can start a new core image for each
proof attempt.  In this case, each core image will start with a fresh
memory.  (When this option is chosen, expunging is irrelevant.)
Certain operating systems and versions of Lisp may not support
this option.

If TPS-TEST is running a new core image for each proof attempt, the
user can interrupt the slave core image using Control-C.  This should
throw one to the debugger level of the slave image.  In Allegro Lisp,
:res will cause the slave to die and throw the user to the debugger
level of the master core image.  Another :res will return the user to
the TPS top level of the master core image.

If the argument MODIFY is T, then the flag TEST-MODIFY can be 
used to change flag settings after loading each mode but before
searching.  See the help message for TEST-MODIFY for more information.

In versions of Common Lisp with multiprocessing (e.g., Allegro 5.0 or
later), the user can specify a time limit for each proof attempt.  The
user can also ask TPS-TEST to iterate trying every (THM . MODE) on
TEST-THEOREMS, increasing the time limit by a factor on each
iteration.  A (THM . MODE) is only tried again with a longer time if
it timed out on the previous attempt.  When multiprocessing is not
available (or if the user specifies an INFINITE time limit), TPS will
search for a proof using a given mode as long as permitted by that
mode.

If TPS-TEST encounters a bug, it will go on to the next (THM . MODE) pair.

The output file is kept independently of DATEREC records, and consists of a record
for each (THM . MODE) pair stating that the theorem was proved at a certain
time using a certain mode, or that the proof terminated with proof lines still 
remaining or that tps encountered an error.  Timing information can also
be sent to the short file if necessary.

If the short file already exists, the old copy will be renamed by adding 
.bak to its name.

See the help messages for TEST-THEOREMS, TEST-INIT and TEST-MODIFY for more information.

@IndexMexpr(TPS-TEST2) @i{searchlist} @i{quiet-run} @i{expu} @i{output} @i{testwin}@\
Like TPS-TEST (see the help message for that command), but calls
the TEST top level and attempts to prove one theorem repeatedly with several
different values of some crucial flags, to see how the time taken will vary.

TEST-THEOREMS should contain a list of dotted pairs of theorems and modes 
in which they can be proven; the searchlist which is used should have at 
least one setting in which the theorem can be proven (otherwise tps-test2
will never finish that theorem).

The output file (by default, tps-test2-output.doc) will contain a summary of 
the results. If this file already exists, it will be renamed by adding .bak 
to its name.

@IndexMexpr(TPS3-SAVE)@\
Save the current TPS3 as the new TPS3 core image.@End(Description)

@Section(Modules)

@Begin(Description)
@IndexMexpr(LOADED-MODS)@\
Returns list of loaded modules.

@IndexMexpr(MODULES) @i{modulelist}@\
Load the specified modules.

@IndexMexpr(UNLOADED-MODS)@\
Returns list of unloaded modules.@End(Description)

@Section(Rules Module)

@Begin(Description)
@IndexMexpr(ASSEMBLE-FILE) @i{rule-file} @i{part-of}@\
Parse, build and write every rule in a given rule file.
Be sure to set the correct mode (MODE RULES) before using this command.

@IndexMexpr(ASSEMBLE-MOD) @i{module}@\
Produce a file with rule commands for every rule file in a module.

@IndexMexpr(BUILD) @i{rule}@\
Process a rule without writing the resulting code to a file.

@IndexMexpr(WRITE-RULE) @i{rule} @i{filename}@\
Write the various functions and definitions for a rule into a file.@End(Description)

@Section(Lisp packages)

@Begin(Description)
@IndexMexpr(PACK-STAT)@\
Give information about the current status of the Lisp
package structure.

@IndexMexpr(UNUSE) @i{lisp-package}@\
Make a Lisp package inaccessible.

@IndexMexpr(USE) @i{lisp-package}@\
Make a Lisp package accessible in the current Lisp package.
An error will be issued by Lisp if this leads to name conflicts.@End(Description)

@Section(Display)

@Begin(Description)
@IndexMexpr(DISPLAYFILE) @i{filename} @i{bigwin}@\
Open a (big) window in which the contents of the given file will be displayed.
Once the end of the file is reached, a message will be printed and
some additional blank lines will be added. Once the end of the
blank lines is reached, the window will vanish.

@IndexMexpr(LS)@\
List the files in the current directory.@End(Description)

@Section(Best modes)

@Begin(Description)
@IndexMexpr(MODEREC)@\
Attempts to create an entry in bestmodes.rec, in a similar way to 
the way that DATEREC works.@End(Description)

@Section(Library Classification)

@Begin(Description)
@IndexMexpr(PSCHEMES)@\
Prints a list of Library Classification Schemes in memory.

See Also: CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, GOTO-CLASS,
CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
FETCH-LIBCLASS, FETCH-LIBCLASS*@End(Description)

@Section(Bugs)

@Begin(Description)
@IndexMexpr(BUG-DELETE) @i{name}@\
Delete a bug record. Exactly the same as the library
DELETE command, but will use the DEFAULT-BUG-DIR if 
USE-DEFAULT-BUG-DIR is T.

@IndexMexpr(BUG-HELP) @i{name}@\
Show the help message of a bug record.

@IndexMexpr(BUG-LIST)@\
Show all the saved bugs in the appropriate directory.
See USE-DEFAULT-BUG-DIR.

@IndexMexpr(BUG-RESTORE) @i{name}@\
Restore a bug from the library (see USE-DEFAULT-BUG-DIR). 
This must have been a bug which was saved with BUG-SAVE; 
this command will reload all the necessary library objects, 
reset all the flags and reload the proof.
This does NOT create a new mode; it just resets the flags.

@IndexMexpr(BUG-SAVE) @i{name} @i{comment}@\
Records details of a bug. Saves the current flag settings, the output
of the HISTORY command, all currently loaded library objects, the 
current proof, the date and time and any comments (the best idea 
is to copy any error messages in to the "comments" prompt). 
This setup can then be retrieved with BUG-RESTORE.
The details are saved as a MODE1, under the name that the user provides
(in a file of the same name) with the assertion and library objects in
other-attributes and other-remarks respectively, and the context set
to BUG. The file will be saved in an appropriate directory (see
USE-DEFAULT-BUG-DIR).@End(Description)

@Section(Interface)

@Begin(Description)
@IndexMexpr(JAVAWIN) @i{fontsize} @i{popups}@\
Begin a Java Interface window to be used for the remainder of this
TPS session.@End(Description)
@ChapterPh(Inference Rules)
The internal name of this category is 
SRULE.
An inference rule can be defined using DEFSRULE.
Allowable properties are: @t{MATCHFN}, @t{MATCH1FN}, @t{SHORTFN}, @t{PRIORITY}.

@Section(Miscellaneous Rules)

@Begin(Description)
@IndexRule(HYP)@label(HYP)@\
Introduce a new hypothesis line into the proof outline.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H1     @assert@;A@F12{o}@>Hyp
*(P2)  H      @assert@;B@F12{o}@> 
Transformation: (P2 ss) ==> (P2 H1 ss) @End(Verbatim)


@IndexRule(LEMMA)@label(LEMMA)@\
Introduce a Lemma.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H1     @assert@;A@F12{o}@> 
*(P2)  H2     @assert@;B@F12{o}@> 
Transformation: (P2 ss) ==> (P2 P1 ss) (P1 ss) @End(Verbatim)


@IndexRule(SAME)@label(SAME)@\
Use the fact that two lines are identical to justify a planned line.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
*(P2)  H      @assert@;A@F12{o}@>Same as: D1
Transformation: (P2 D1 ss) ==> @End(Verbatim)
@End(Description)

@Section(Propositional Rules)

@Begin(Description)
@IndexRule(ASSOC-LEFT)@label(ASSOC-LEFT)@\
Rule to associate a support line leftwards. Use before
calling CASES3 or CASES4.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;P@F12{o}@> 
 (D2)  H      @assert@;`(ASSOC-L  P@F12{o})@>Assoc: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(CASES)@label(CASES)@\
Rule of Cases.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @or@; B@F12{o}@> 
 (H2)  H,H2   @assert@;A@F12{o}@>Case 1: D1
 (P3)  H,H2   @assert@;C@F12{o}@> 
 (H4)  H,H4   @assert@;B@F12{o}@>Case 2: D1
 (P5)  H,H4   @assert@;C@F12{o}@> 
*(P6)  H      @assert@;C@F12{o}@>Cases: D1 P3 P5
Transformation: (P6 D1 ss) ==> (P3 H2 ss) (P5 H4 ss) @End(Verbatim)


@IndexRule(CASES3)@label(CASES3)@\
Rule of Cases.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @or@; B@F12{o} @or@; C@F12{o}@> 
 (H2)  H,H2   @assert@;A@F12{o}@>Case 1: D1
 (P3)  H,H2   @assert@;D@F12{o}@> 
 (H4)  H,H4   @assert@;B@F12{o}@>Case 2: D1
 (P5)  H,H4   @assert@;D@F12{o}@> 
 (H6)  H,H6   @assert@;C@F12{o}@>Case 3: D1
 (P7)  H,H6   @assert@;D@F12{o}@> 
*(P8)  H      @assert@;D@F12{o}@>Cases: D1 P3 P5 P7
Transformation: (P8 D1 ss) ==> (P3 H2 ss) (P5 H4 ss) (P7 H6 ss) @End(Verbatim)


@IndexRule(CASES4)@label(CASES4)@\
Rule of Cases.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @or@; B@F12{o} @or@; C@F12{o} @or@; D@F12{o}@> 
 (H2)  H,H2   @assert@;A@F12{o}@>Case 1: D1
 (P3)  H,H2   @assert@;E@F12{o}@> 
 (H4)  H,H4   @assert@;B@F12{o}@>Case 2: D1
 (P5)  H,H4   @assert@;E@F12{o}@> 
 (H6)  H,H6   @assert@;C@F12{o}@>Case 3: D1
 (P7)  H,H6   @assert@;E@F12{o}@> 
 (H8)  H,H8   @assert@;D@F12{o}@>Case 4: D1
 (P9)  H,H8   @assert@;E@F12{o}@> 
*(P10) H      @assert@;E@F12{o}@>Cases: D1 P3 P5 P7 P9
Transformation: (P10 D1 ss) ==> (P3 H2 ss) (P5 H4 ss) (P7 H6 ss) (P9 H8 ss) @End(Verbatim)


@IndexRule(DEDUCT)@label(DEDUCT)@\
The deduction rule.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;A@F12{o}@>Hyp
 (D2)  H,H1   @assert@;B@F12{o}@> 
*(P3)  H      @assert@;A@F12{o} @implies@; B@F12{o}@>Deduct: D2
Transformation: (P3 ss) ==> (D2 H1 ss) @End(Verbatim)


@IndexRule(DISJ-IMP)@label(DISJ-IMP)@\
Rule to replace a disjunction by an implication.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@not@;A@F12{o} @or@; B@F12{o}@> 
 (D2)  H      @assert@;A@F12{o} @implies@; B@F12{o}@>Disj-Imp: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(DISJ-IMP-L)@label(DISJ-IMP-L)@\
Rule to replace a disjunction by an implication.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @or@; B@F12{o}@> 
 (D2)  H      @assert@;@not@;A@F12{o} @implies@; B@F12{o}@>Disj-Imp-L: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(DISJ-IMP-R)@label(DISJ-IMP-R)@\
Rule to replace a disjunction by an implication.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @or@; B@F12{o}@> 
 (D2)  H      @assert@;@not@;B@F12{o} @implies@; A@F12{o}@>Disj-Imp-R: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(ECONJ)@label(ECONJ)@\
Rule to infer two conjuncts from a conjunction.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @and@; B@F12{o}@> 
 (D2)  H      @assert@;A@F12{o}@>Conj: D1
 (D3)  H      @assert@;B@F12{o}@>Conj: D1
Transformation: (pp D1 ss) ==> (pp D2 D3 ss) @End(Verbatim)


@IndexRule(EQUIV-IMPLICS)@label(EQUIV-IMPLICS)@\
Rule to convert an equivalence into twin implications.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;P@F12{o} @equiv@; R@F12{o}@> 
 (D2)  H      @assert@;[P@F12{o} @implies@; R@F12{o}] @and@;.R @implies@; P@>EquivImp: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(ICONJ)@label(ICONJ)@\
Rule to infer a conjunction from two conjuncts.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
 (P2)  H      @assert@;B@F12{o}@> 
*(P3)  H      @assert@;A@F12{o} @and@; B@F12{o}@>Conj: P1 P2
Transformation: (P3 ss) ==> (P1 ss) (P2 ss) @End(Verbatim)


@IndexRule(IDISJ-LEFT)@label(IDISJ-LEFT)@\
Introduce a disjunction (left version).
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
*(P2)  H      @assert@;A@F12{o} @or@; B@F12{o}@>Idisj-L: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(IDISJ-RIGHT)@label(IDISJ-RIGHT)@\
Introduce a disjunction (right version).
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
*(P2)  H      @assert@;B@F12{o} @or@; A@F12{o}@>Idisj-R: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(IMP-DISJ)@label(IMP-DISJ)@\
Rule to replace an implication by a disjunction.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o} @implies@; B@F12{o}@> 
 (D2)  H      @assert@;@not@;A@F12{o} @or@; B@F12{o}@>Imp-Disj: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(IMP-DISJ-L)@label(IMP-DISJ-L)@\
Rule to replace an implication by a disjunction.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@not@;A@F12{o} @implies@; B@F12{o}@> 
 (D2)  H      @assert@;A@F12{o} @or@; B@F12{o}@>Imp-Disj-L: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(IMP-DISJ-R)@label(IMP-DISJ-R)@\
Rule to replace an implication by a disjunction.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@not@;B@F12{o} @implies@; A@F12{o}@> 
 (D2)  H      @assert@;A@F12{o} @or@; B@F12{o}@>Imp-Disj-R: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(IMPLICS-EQUIV)@label(IMPLICS-EQUIV)@\
Rule to convert twin implications into an equivalence.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;[P@F12{o} @implies@; R@F12{o}] @and@;.R @implies@; P@> 
*(P2)  H      @assert@;P@F12{o} @equiv@; R@F12{o}@>ImpEquiv: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(INDIRECT)@label(INDIRECT)@\
Rule of Indirect Proof.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;@not@;A@F12{o}@>Assume negation
 (P2)  H,H1   @assert@;@falsehood@;@> 
*(P3)  H      @assert@;A@F12{o}@>Indirect: P2
Transformation: (P3 ss) ==> (P2 H1 ss) @End(Verbatim)


@IndexRule(INDIRECT1)@label(INDIRECT1)@\
Rule of Indirect Proof Using One Contradictory Line.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;@not@;A@F12{o}@>Assume negation
 (P2)  H,H1   @assert@;B@F12{o} @and@; @not@;B@> 
*(P3)  H      @assert@;A@F12{o}@>Indirect: P2
Transformation: (P3 ss) ==> (P2 H1 ss) @End(Verbatim)


@IndexRule(INDIRECT2)@label(INDIRECT2)@\
Rule of Indirect Proof Using Two Contradictory Lines.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;@not@;A@F12{o}@>Assume negation
 (P2)  H,H1   @assert@;B@F12{o}@> 
 (P3)  H,H1   @assert@;@not@;B@F12{o}@> 
*(P4)  H      @assert@;A@F12{o}@>Indirect: P2 P3
Transformation: (P4 ss) ==> (P2 H1 ss) (P3 H1 ss) @End(Verbatim)


@IndexRule(ITRUTH)@label(ITRUTH)@\
Rule to infer TRUTH
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(P1)  H      @assert@;@truth@;@>Truth
Transformation: (P1 ss) ==> @End(Verbatim)


@IndexRule(MP)@label(MP)@\
Modus Ponens.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
*(D2)  H      @assert@;A@F12{o} @implies@; B@F12{o}@> 
 (D3)  H      @assert@;B@F12{o}@>MP: P1 D2
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1) @End(Verbatim)


@IndexRule(SUBST-EQUIV)@label(SUBST-EQUIV)@\
Substitution of Equivalence.  Usable when R and P are the same modulo
the equivalence s EQUIV t.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o}@> 
*(D2)  H      @assert@;s@F12{o} @equiv@; t@F12{o}@> 
 (D3)  H      @assert@;R@F12{o}@>Sub-equiv: P1 D2
Restrictions:  (SAME-MODULO-EQUALITY P@F12{o} R@F12{o} s@F12{o} t@F12{o})
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1 D2) @End(Verbatim)
@End(Description)

@Section(Negation Rules)

@Begin(Description)
@IndexRule(ABSURD)@label(ABSURD)@\
Rule of Intuitionistic Absurdity.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;@falsehood@;@> 
*(P2)  H      @assert@;A@F12{o}@>Absurd: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(ENEG)@label(ENEG)@\
Rule of Negation Elimination.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@not@;A@F12{o}@> 
 (P2)  H      @assert@;A@F12{o}@> 
*(P3)  H      @assert@;@falsehood@;@>NegElim: D1 P2
Transformation: (P3 D1 ss) ==> (P2 ss) @End(Verbatim)


@IndexRule(INEG)@label(INEG)@\
Rule of Negation Introduction
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (H1)  H,H1   @assert@;A@F12{o}@>Hyp
 (P2)  H,H1   @assert@;@falsehood@;@> 
*(P3)  H      @assert@;@not@;A@F12{o}@>NegIntro: P2
Transformation: (P3 ss) ==> (P2 H1 ss) @End(Verbatim)


@IndexRule(NNF)@label(NNF)@\
Put Wff in Negation Normal Form.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(NEG-NORM  A@F12{o})@>NNF: D1
Restrictions:  (NON-ATOMIC-OR-TRUTHVALUE A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(NNF-EXPAND)@label(NNF-EXPAND)@\
Expand Wff from Negation Normal Form.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(NEG-NORM  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>NNF-Expand: P1
Restrictions:  (NON-ATOMIC A@F12{o})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(PULLNEG)@label(PULLNEG)@\
Pull out negation.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(PUSH-NEGATION  [@not@;A@F12{o}])@> 
*(P2)  H      @assert@;@not@;A@F12{o}@>Neg: P1
Restrictions:  (NON-ATOMIC A@F12{o})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(PUSHNEG)@label(PUSHNEG)@\
Push in negation.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@not@;A@F12{o}@> 
 (D2)  H      @assert@;`(PUSH-NEGATION  [@not@;A@F12{o}])@>Neg: D1
Restrictions:  (NON-ATOMIC-OR-TRUTHVALUE A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)
@End(Description)

@Section(Quantifier Rules)

@Begin(Description)
@IndexRule(AB*)@label(AB*)@\
Rule to alphabetically change embedded quantified variables.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;B@F12{o}@>AB: D1
Restrictions:  (WFFEQ-AB A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(ABE)@label(ABE)@\
Rule to change a top level occurrence of an existentially
 quantified variable.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@exists@;x@F12{a} A@F12{o}@> 
 (D2)  H      @assert@;@exists@;y@F12{a} `(S  y  x@F12{a}  A@F12{o})@>AB: y@F12{a} D1
Restrictions:  (FREE-FOR y@F12{a} x@F12{a} A@F12{o}) (IS-VARIABLE y@F12{a}) (IS-VARIABLE x@F12{a}) (NOT-FREE-IN y@F12{a} A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(ABU)@label(ABU)@\
Rule to change a top level occurrence of a universally quantified
 variable.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;@forall@;y@F12{a} `(S  y  x@F12{a}  A@F12{o})@> 
*(P2)  H      @assert@;@forall@;x@F12{a} A@F12{o}@>AB: x@F12{a} P1
Restrictions:  (FREE-FOR y@F12{a} x@F12{a} A@F12{o}) (IS-VARIABLE y@F12{a}) (IS-VARIABLE x@F12{a}) (NOT-FREE-IN y@F12{a} A@F12{o})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(EGEN)@label(EGEN)@\
Rule of Existential Generalization.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(LCONTR  [[@g{l}@;x@F12{a} A@F12{o}] t@F12{a}])@> 
*(P2)  H      @assert@;@exists@;x@F12{a} A@F12{o}@>EGen: t@F12{a} P1
Restrictions:  (IS-VARIABLE x@F12{a})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(RULEC)@label(RULEC)@\
RuleC
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@exists@;x@F12{a} B@F12{o}@> 
 (H2)  H,H2   @assert@;`(LCONTR  [[@g{l}@;x@F12{a} B@F12{o}] y@F12{a}])@>Choose: y@F12{a} D1
 (D3)  H,H2   @assert@;A@F12{o}@> 
*(P4)  H      @assert@;A@F12{o}@>RuleC: D1 D3
Restrictions:  (IS-VARIABLE y@F12{a}) (NOT-FREE-IN-HYPS y@F12{a}) (NOT-FREE-IN y@F12{a} [@exists@;x@F12{a} B@F12{o}]) (NOT-FREE-IN y@F12{a} A@F12{o})
Transformation: (P4 D1 ss) ==> (D3 H2 ss) @End(Verbatim)


@IndexRule(RULEC1)@label(RULEC1)@\
RuleC1 -- the special case of RULEC where the chosen
variable has the same name as the bound variable.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@exists@;x@F12{a} B@F12{o}@> 
 (H2)  H,H2   @assert@;B@F12{o}@>Choose: x@F12{a} D1
 (D3)  H,H2   @assert@;A@F12{o}@> 
*(P4)  H      @assert@;A@F12{o}@>RuleC: D1 D3
Restrictions:  (NOT-FREE-IN-HYPS x@F12{a}) (IS-VARIABLE x@F12{a}) (NOT-FREE-IN x@F12{a} A@F12{o})
Transformation: (P4 D1 ss) ==> (D3 H2 ss) @End(Verbatim)


@IndexRule(UGEN)@label(UGEN)@\
Rule of Universal Generalization.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
*(P2)  H      @assert@;@forall@;x@F12{a} A@F12{o}@>UGen: x@F12{a} P1
Restrictions:  (IS-VARIABLE x@F12{a}) (NOT-FREE-IN-HYPS x@F12{a})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(UI)@label(UI)@\
Rule of Universal Instantiation.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;@forall@;x@F12{a} A@F12{o}@> 
 (D2)  H      @assert@;`(LCONTR  [[@g{l}@;x@F12{a} A@F12{o}] t@F12{a}])@>UI: t@F12{a} D1
Restrictions:  (IS-VARIABLE x@F12{a})
Transformation: (pp D1 ss) ==> (pp D2 D1 ss) @End(Verbatim)
@End(Description)

@Section(Substitution Rules)

@Begin(Description)
@IndexRule(SUBSTITUTE)@label(SUBSTITUTE)@\
Rule to substitute a term for a variable.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(S  t@F12{a}  x@F12{a}  A@F12{o})@>Subst: t@F12{a}  x@F12{a} D1
Restrictions:  (NOT-FREE-IN-HYPS x@F12{a}) (IS-VARIABLE x@F12{a}) (FREE-FOR t@F12{a} x@F12{a} A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss D1) @End(Verbatim)
@End(Description)

@Section(Equality Rules)

@Begin(Description)
@IndexRule(EQUIV-EQ)@label(EQUIV-EQ)@\
Rule to infer a line from one which is equal up to 
definitions, lambda conversion, alphabetic change of bound variables 
and the Leibniz definition of the symbol = . You may use the editor 
command EXPAND= to create the desired line from the existing one.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;B@F12{o}@>Equiv-eq: D1
Restrictions:  (WFFEQ-DEFEQ A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(EQUIV-EQ-CONTR)@label(EQUIV-EQ-CONTR)@\
Rule to contract the outermost instance of the Leibniz definition of 
equality into instances of the symbol = .
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(INSTANTIATE-TOP-EQUALITY  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Equiv-eq: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(EQUIV-EQ-CONTR*)@label(EQUIV-EQ-CONTR*)@\
Rule to contract all instances of the Leibniz definition of 
equality into instances of the symbol = .
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(INSTANTIATE-EQUALITIES  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Equiv-eq: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(EQUIV-EQ-EXPD)@label(EQUIV-EQ-EXPD)@\
Rule to expand the outermost equality using the Leibniz definition.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(INSTANTIATE-TOP-EQUALITY  A@F12{o})@>Equiv-eq: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(EQUIV-EQ-EXPD*)@label(EQUIV-EQ-EXPD*)@\
Rule to expand all equalities using the Leibniz definition.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(INSTANTIATE-EQUALITIES  A@F12{o})@>Equiv-eq: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(EXT=)@label(EXT=)@\
Rule of Extensionality.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;@forall@;x@F12{b}.f@F12{ab} x = g@F12{ab} x@> 
*(P2)  H      @assert@;f@F12{ab} = g@F12{ab}@>Ext=: P1
Restrictions:  (IS-VARIABLE x@F12{b}) (NOT-FREE-IN x@F12{b} f@F12{ab}) (NOT-FREE-IN x@F12{b} g@F12{ab})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(EXT=0)@label(EXT=0)@\
Rule to convert equality at type o into an equivalence.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o} @equiv@; R@F12{o}@> 
*(P2)  H      @assert@;P@F12{o} = R@F12{o}@>Ext=: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(LET)@label(LET)@\
Bind a  variable to a term.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (D1)  H      @assert@;A@F12{a} = A@>Refl=
 (D2)  H      @assert@;@exists@;x@F12{a}.x = A@F12{a}@>EGen: x@F12{a} D1
 (H3)  H,H3   @assert@;x@F12{a} = A@F12{a}@>Choose: x@F12{a}
 (P4)  H,H3   @assert@;C@F12{o}@> 
*(P5)  H      @assert@;C@F12{o}@>RuleC: D2 P4
Restrictions:  (NOT-FREE-IN-HYPS x@F12{a}) (IS-VARIABLE x@F12{a}) (NOT-FREE-IN x@F12{a} C@F12{o})
Transformation: (P5 ss) ==> (P4 ss D1 D2 H3) @End(Verbatim)


@IndexRule(SUBST=)@label(SUBST=)@\
Substitution of Equality.  Usable when R and P are the same modulo
the equality s=t.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o}@> 
*(D2)  H      @assert@;s@F12{a} = t@F12{a}@> 
 (D3)  H      @assert@;R@F12{o}@>Sub=: P1 D2
Restrictions:  (SAME-MODULO-EQUALITY P@F12{o} R@F12{o} s@F12{a} t@F12{a})
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1 D2) @End(Verbatim)


@IndexRule(SUBST=L)@label(SUBST=L)@\
Substitution of Equality.  Replaces some occurrences of the left hand
side by the right hand side.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o}@> 
*(D2)  H      @assert@;s@F12{a} = t@F12{a}@> 
 (D3)  H      @assert@;R@F12{o}@>Subst=: P1 D2
Restrictions:  (R-PRIME-RESTR s@F12{a} P@F12{o} t@F12{a} R@F12{o})
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1 D2) @End(Verbatim)


@IndexRule(SUBST=R)@label(SUBST=R)@\
Substitution of Equality.  Replaces some occurrences of the right
hand side by the left hand side.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;P@F12{o}@> 
*(D2)  H      @assert@;t@F12{a} = s@F12{a}@> 
 (D3)  H      @assert@;R@F12{o}@>Subst=: P1 D2
Restrictions:  (R-PRIME-RESTR s@F12{a} P@F12{o} t@F12{a} R@F12{o})
Transformation: (pp D2 ss) ==> (P1 ss) (pp D3 ss P1 D2) @End(Verbatim)


@IndexRule(SYM=)@label(SYM=)@\
Rule of Symmetry of Equality.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{a} = B@F12{a}@> 
*(P2)  H      @assert@;B@F12{a} = A@F12{a}@>Sym=: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@End(Description)

@Section(Definition Rules)

@Begin(Description)
@IndexRule(EDEF)@label(EDEF)@\
Rule to eliminate first definition, left to right.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(INST-DEF  A@F12{o})@>Defn: D1
Restrictions:  (CONTAINS-DEFN A@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(EQUIV-WFFS)@label(EQUIV-WFFS)@\
Rule to assert equivalence of lines up to definition.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;P@F12{o}@> 
 (D2)  H      @assert@;R@F12{o}@>EquivWffs: D1
Restrictions:  (WFFEQ-DEF P@F12{o} R@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(IDEF)@label(IDEF)@\
Rule to introduce a definition.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(INST-DEF  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Defn: P1
Restrictions:  (CONTAINS-DEFN A@F12{o})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@End(Description)

@Section(Lambda Conversion Rules)

@Begin(Description)
@IndexRule(BETA*)@label(BETA*)@\
Rule to infer a line from one which is equal up to lambda conversion
using beta rule (but NOT eta rule) and alphabetic change of bound variables.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;B@F12{o}@>Beta Rule: D1
Restrictions:  (WFFEQ-AB-BETA A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(ETA*)@label(ETA*)@\
Rule to infer a line from one which is equal up to lambda conversion
using eta rule (but NOT beta rule) and alphabetic change of bound variables.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;B@F12{o}@>Eta Rule: D1
Restrictions:  (WFFEQ-AB-ETA A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(LAMBDA*)@label(LAMBDA*)@\
Rule to infer a line from one which is equal up to lambda conversion
using both beta and eta rules and alphabetic change of bound variables.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;B@F12{o}@>Lambda=: D1
Restrictions:  (WFFEQ-AB-LAMBDA A@F12{o} B@F12{o})
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(LCONTR*)@label(LCONTR*)@\
Rule to put an inferred line into Lambda-normal form using both 
beta and eta conversion.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(LNORM  A@F12{o})@>Lambda: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(LCONTR*-BETA)@label(LCONTR*-BETA)@\
Rule to put an inferred line into beta-normal form.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(LNORM-BETA  A@F12{o})@>Beta rule: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(LCONTR*-ETA)@label(LCONTR*-ETA)@\
Rule to put an inferred line into eta-normal form.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(LNORM-ETA  A@F12{o})@>Eta rule: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(LEXPD*)@label(LEXPD*)@\
Rule to put a planned line into Lambda-normal form using both 
beta and eta conversion.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(LNORM  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Lambda: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(LEXPD*-BETA)@label(LEXPD*-BETA)@\
Rule to put a planned line into beta-normal form.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(LNORM-BETA  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Beta rule: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(LEXPD*-ETA)@label(LEXPD*-ETA)@\
Rule to put a planned line into eta-normal form.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(LNORM-ETA  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Eta rule: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@End(Description)

@Section(Rewriting commands)

@Begin(Description)
@IndexRule(REWRITE-SUPP*)@label(REWRITE-SUPP*)@\
Rewrite a supporting line using all rewrite rules 
possible.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(APPLY-RRULE-ANY*  A@F12{o})@>Rewrites: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(REWRITE-SUPP1)@label(REWRITE-SUPP1)@\
Rewrite a supporting line using the first rewrite 
rule that applies.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(APPLY-RRULE-ANY  A@F12{o})@>Rewrite: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(SIMPLIFY-PLAN)@label(SIMPLIFY-PLAN)@\
Justify a planned line using the first rewrite rule that 
applies.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(SIMPLIFY-UP  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Rewrite: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(SIMPLIFY-PLAN*)@label(SIMPLIFY-PLAN*)@\
Justify a planned line using the first rewrite rule that 
applies.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(SIMPLIFY-UP*  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Rewrite: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(SIMPLIFY-SUPP)@label(SIMPLIFY-SUPP)@\
Rewrite a supporting line using the first rewrite 
rule that applies.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(SIMPLIFY-DOWN  A@F12{o})@>Rewrite: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(SIMPLIFY-SUPP*)@label(SIMPLIFY-SUPP*)@\
Rewrite a supporting line using the first rewrite 
rule that applies.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
*(D1)  H      @assert@;A@F12{o}@> 
 (D2)  H      @assert@;`(SIMPLIFY-DOWN*  A@F12{o})@>Rewrite: D1
Transformation: (pp D1 ss) ==> (pp D2 ss) @End(Verbatim)


@IndexRule(UNREWRITE-PLAN*)@label(UNREWRITE-PLAN*)@\
Justify a planned line using all rewrite rules possible.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(UNAPPLY-RRULE-ANY*  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Rewrites: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(UNREWRITE-PLAN1)@label(UNREWRITE-PLAN1)@\
Justify a planned line using the first rewrite rule that 
applies.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;`(UNAPPLY-RRULE-ANY  A@F12{o})@> 
*(P2)  H      @assert@;A@F12{o}@>Rewrite: P1
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)


@IndexRule(USE-RRULES)@label(USE-RRULES)@\
Rewrite a line. The line may be rewritten several steps,
but rewrites may not be nested.
@\@Begin(Verbatim, Scriptpush no, Spacing=1.5, Group)
 (P1)  H      @assert@;A@F12{o}@> 
*(P2)  H      @assert@;B@F12{o}@>Rewrite: P1
Restrictions:  (INSTANCE-OF-REWRITING A@F12{o} B@F12{o})
Transformation: (P2 ss) ==> (P1 ss) @End(Verbatim)
@End(Description)
@ChapterPh(Extensional Sequent Commands)
The internal name of this category is 
EXTSEQCMD.
An extensional sequent command can be defined using DEFEXTSEQ.
Allowable properties are: @t{EXTSEQ-ARGTYPES}, @t{EXTSEQ-ARGNAMES}, @t{EXTSEQ-ARGHELP}, @t{EXTSEQ-DEFAULTFNS}, @t{EXTSEQ-MAINFNS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
Leave EXT-SEQ to the next enclosing top level.@End(Description)

@Section(Proof Translation)

@Begin(Description)
@IndexOther(CUTFREE-TO-EDAG) @i{conc}@\
Translate a complete, cut-free extensional sequent derivation
to an extensional expansion dag proof.  The conclusion of the
derivation must be a sequent with a single formula.@End(Description)

@Section(Extensional Sequent Entering)

@Begin(Description)
@IndexOther(DELETE) @i{del-lines}@\
Delete Lines in an existing derivation

@IndexOther(EXPAND-ALL-DERIVED-RULES)@\
Remove all applications of derived rules in terms of basic rules.
The derived rules include: false-, and-, and+, implies-, implies+,
equiv-, equiv+, exists-, exists+

@IndexOther(EXPAND-ALL-INITS-AND-REFLS)@\
Remove all applications of Inits and Refls in terms of basic rules.

@IndexOther(INTRODUCE-GAP) @i{line} @i{num}@\
Introduce a gap in an existing derivation.

@IndexOther(PROOFLIST)@\
Print a list of all extensional sequent derivations or partial derivations currently in memory.
Also prints the final sequent of each proof.

@IndexOther(PROVE) @i{wff} @i{prefix} @i{num}@\
Start a sequent calculus derivation for a sequent with one wff.
Use WEAKEN to add more wffs to the main sequent.

@IndexOther(RECONSIDER) @i{prefix}@\
Reconsider an extensional sequent derivation. The following proofs are in memory:

For more details, use the PROOFLIST command.


@IndexOther(SQUEEZE)@\
Removes unnecessary gaps from the sequent derivation.

@IndexOther(WEAKEN) @i{wff}@\
Weaken the sequent calculus derivation by adding a wff.@End(Description)

@Section(Extensional Sequent Printing)

@Begin(Description)
@IndexOther(PALL)@\
Print all the lines in the current extensional sequent derivation.

@IndexOther(PPLAN) @i{pline}@\
Print a planned line

@IndexOther(PSTATUS)@\
Give the current status of the extensional sequent derivation.@End(Description)

@Section(Extensional Sequent Rules)

@Begin(Description)
@IndexOther(ALL+) @i{p2} @i{p1} @i{y}@\
Infer (p2) Gamma,[FORALL x M] from (p1) Gamma,[y/x]M.

@IndexOther(ALL-) @i{p2} @i{p1} @i{trm}@\
Infer (p2) Gamma,~[FORALL x M] from (p1) Gamma,~[trm/x]M.

@IndexOther(CONTR) @i{p2} @i{p1}@\
Infer (p2) Gamma,A from (p1) Gamma,A,A.

@IndexOther(CUT) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, C
and
(p2) Gamma, ~C
infer
(p3) Gamma

@IndexOther(DEC) @i{p} @i{pl}@\
From
(p1) Gamma, [A1 = B1]
. . .
(pn) Gamma, [An = Bn]
infer
(p) Gamma, [[H A1 . . . An] = [H B1 . . . Bn]]

@IndexOther(DNEG) @i{p2} @i{p1}@\
Infer (p2) Gamma,~~A from (p1) Gamma,A

@IndexOther(EQFUNC) @i{p2} @i{p1} @i{trm}@\
Infer (p2) Gamma,~forall x M from (p1) Gamma,~[trm/x]M.

@IndexOther(EQO) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, A, B
and
(p2) Gamma, ~A, ~B
infer
(p3) Gamma, ~[A = B]

@IndexOther(EQUIVWFFS+) @i{p2} @i{p1}@\
Infer (p2) Gamma,A from (p1) Gamma,B where B is obtained from A by
expanding an abbreviation at the head of A if A is not an equation.
If A is an equation of base type other than O, the abbreviation must 
be at the head of the left or right side.

@IndexOther(EQUIVWFFS-) @i{p2} @i{p1}@\
Infer (p2) Gamma,~A from (p1) Gamma,~B where B is obtained from A by
expanding an abbreviation at the head of A if A is not an equation.
If A is an equation of base type other than O, the abbreviation must 
be at the head of the left or right side.

@IndexOther(EUNIF1) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, ~[a = b], [a = c]
and
(p2) Gamma, ~[a = b], [b = d]
infer
(p3) Gamma, ~[a = b], [c = d]

@IndexOther(EUNIF2) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, ~[a = b], [a = d]
and
(p2) Gamma, ~[a = b], [b = c]
infer
(p3) Gamma, ~[a = b], [c = d]

@IndexOther(EXTFUNC) @i{p2} @i{p1} @i{y}@\
Infer (p2) Gamma,forall x M from (p1) Gamma,[a/x]M.

@IndexOther(EXTO) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, ~A, B
and
(p2) Gamma, A, ~B
infer
(p3) Gamma, [A = B]

@IndexOther(INIT) @i{p}@\
Infer (p) Gamma, ~A, A

@IndexOther(INITEQ) @i{p} @i{pl}@\
From
(p1) Gamma, [A1 = B1]
. . .
(pn) Gamma, [An = Bn]
infer
(p) Gamma, [P A1 . . . An], ~[P B1 . . . Bn]

@IndexOther(INTERNALIZE+) @i{p2} @i{p1}@\
Infer (p2) Gamma,A from (p1) Gamma,#(A) where #(A) is the 'externalized' version of A.
This corresponds to the # rule in Chad E. Brown's thesis.

@IndexOther(INTERNALIZE-) @i{p2} @i{p1}@\
Infer (p2) Gamma,~A from (p1) Gamma,~#(A) where #(A) is the 'externalized' version of A.
This corresponds to the ~# rule in Chad E. Brown's thesis.

@IndexOther(LAM) @i{p2} @i{p1}@\
Infer (p2) Gamma,A from (p1) Gamma,N where N is the lambda normal form of A

@IndexOther(OR+) @i{p2} @i{p1}@\
From
(p1) Gamma, A, B
infer
(p3) Gamma, [A OR B]

@IndexOther(OR-) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, ~A
and
(p2) Gamma, ~B
infer
(p3) Gamma, ~[A OR B]

@IndexOther(REFL) @i{p}@\
Infer (p) Gamma, t = t

@IndexOther(TRUE+) @i{p}@\
Infer (p) Gamma, TRUTH@End(Description)

@Section(Extensional Sequent Derived Rules)

@Begin(Description)
@IndexOther(AND+) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, A
and
(p2) Gamma, B
infer
(p3) Gamma, [A AND B]

@IndexOther(AND-) @i{p2} @i{p1}@\
From
(p1) Gamma, ~A, ~B
infer
(p3) Gamma, ~[A AND B]

@IndexOther(EQUIV+) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, ~A, B
and
(p2) Gamma, A, ~B
infer
(p3) Gamma, [A EQUIV B]

@IndexOther(EQUIV-) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, A, B
and
(p2) Gamma, ~A, ~B
infer
(p3) Gamma, ~[A EQUIV B]

@IndexOther(EXISTS+) @i{p2} @i{p1} @i{trm}@\
Infer (p2) Gamma,[EXISTS x M] from (p1) Gamma,[trm/x]M.

@IndexOther(EXISTS-) @i{p2} @i{p1} @i{y}@\
Infer (p2) Gamma,~[EXISTS x M] from (p1) Gamma,~[y/x]M.

@IndexOther(FALSE-) @i{p}@\
Infer (p) Gamma, ~FALSEHOOD

@IndexOther(IMPLIES+) @i{p2} @i{p1}@\
From
(p1) Gamma, ~A, B
infer
(p3) Gamma, [A IMPLIES B]

@IndexOther(IMPLIES-) @i{p3} @i{p1} @i{p2}@\
From
(p1) Gamma, A
and
(p2) Gamma, ~B
infer
(p3) Gamma, ~[A IMPLIES B]@End(Description)

@Section(Extensional Sequent Files)

@Begin(Description)
@IndexOther(RESTOREPROOF) @i{savefile}@\
Reads an extensional sequent derivation from a file created
by SAVEPROOF in the EXT-SEQ top level and makes it the current
derivation.  A security feature prevents the restoration of saved
proofs which have been altered in any way.  Retrieve any definitions
which are used in the proof and stored in the library before restoring
the proof. If you don't specify a directory, it will first try your
home directory and then all the directories listed in SOURCE-PATH.

@IndexOther(SAVEPROOF) @i{savefile}@\
Saves the current natural deduction proof to the specified file in
a form in which it can be restored.  Use RESTOREPROOF to restore the proof.
Overwrites the file if it already exists.

@IndexOther(SCRIBEPROOF) @i{filename} @i{timing}@\
Print the current proof into a MSS file.
After leaving TPS, run this .MSS file through Scribe and print the resulting
file.

@IndexOther(TEXPROOF) @i{filename} @i{timing}@\
Print the current proof into a tex file.
After leaving tps, run this .tex file through tex and print the resulting
file.

Many flags affect the output of texproof.
See: USE-INTERNAL-PRINT-MODE, TURNSTILE-INDENT-AUTO, TURNSTILE-INDENT,
LATEX-EMULATION, TEX-MIMIC-SCRIBE, PPWFFLAG, DISPLAYWFF, INFIX-NOTATION,
PAGELENGTH, PAGEWIDTH, TEX-BREAK-BEFORE-SYMBOLS, LOCALLEFTFLAG, SCOPE,
ALLSCOPEFLAG, USE-DOT, FIRST-ORDER-PRINT-MODE, FILLINEFLAG, ATOMVALFLAG.@End(Description)

@Section(Compound)

@Begin(Description)
@IndexOther(GO2) @i{tacmode}@\
Apply all possible extensional sequent tactics.@End(Description)
@ChapterPh(Tactics)
The internal name of this category is 
TACTIC.
A tactic can be defined using DEFTACTIC.
Allowable properties are: @t{NAT-DED}, @t{ETREE-NAT}, @t{MATE-SRCH}, @t{EXT-SEQ}.

@Section(Compound)

@Begin(Description)
@IndexOther(ALL+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(ALL-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(AND+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(AND-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(AUTO-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(REPEAT (ORELSE MIN-PROP DIY-TAC))
@end(flushleft)
Does minimal propositional actions then calls mating search if
necessary, and translates the resulting proof.

@end(description)

@IndexOther(BOOK-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE SAME-TAC UNSPONSOR-TAC UNNEC-EXP-TAC)
@end(flushleft)


@end(description)

@IndexOther(COMPLETE-TRANSFORM*-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE (CALL PRINT-ROUTINES) SAME-TAC NNF-TAC ABSURD-TAC TRUTH-TAC
 REFL=-TAC (IFTHEN USE-RULEP-TAC RULEP-TAC) (MAKE-ROOM :USE NAT-DED)
 DUPLICATE-SUPPORT-TAC
 (THEN** (IFTHEN USE-RULEP-TAC ECONJ*-TAC ECONJ-TAC) UNSPONSOR-TAC)
 DEDUCT-TAC REWRITE-SLINE-TAC REWRITE-PLINE-TAC RULEC-TAC UGEN-TAC
 EGEN-TAC (THEN** UI-TAC UNSPONSOR-TAC)
 (THEN** UNNEC-EXP-TAC UNSPONSOR-TAC) (THEN** IDISJ-TAC UNSPONSOR-TAC)
 (THEN** (IFTHEN USE-RULEP-TAC ICONJ*-TAC ICONJ-TAC) UNSPONSOR-TAC)
 (THEN** CASES-TAC UNSPONSOR-TAC) PUSHNEG-TAC ML::NEG-EQUIV-SLINE-TAC
 NEG-NEG-PLAN-TAC INEG-TAC SUBST=-TAC MP-TAC CLASS-DISJ-TAC
 INDIRECT2-TAC INESS-PLINE-TAC NEG-AND-SLINE-TAC NEG-AND-PLAN-TAC
 NEG-EQUAL-SLINE-TAC INDIRECT-TAC)
@end(flushleft)


@end(description)

@IndexOther(COMPLETE-TRANSFORM-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(REPEAT COMPLETE-TRANSFORM*-TAC)
@end(flushleft)


@end(description)

@IndexOther(CONTRACT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(DEC+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(DIY-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Calls matingsearch procedure specified by the flag DEFAULT-MS on current
planned line and its supports, then translates the expansion proof to 
natural deduction.  The actual supports used will be the universal closure 
of the supports over any free variables which are not free in their 
hypotheses.

@end(description)

@IndexOther(ELIM-DEFNS-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE EDEF-TAC IDEF-TAC)
@end(flushleft)


@end(description)

@IndexOther(EQFUNC-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EQO-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EQUIV+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EQUIV-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EQUIVWFFS+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EQUIVWFFS-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EUNIF1-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EUNIF2-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EXISTS+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EXISTS-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EXTFUNC+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(EXTO+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(FALSE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(GO2-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(REPEAT
 (ORELSE (CALL PRINT-ROUTINES) SAME-TAC REFL=-TAC SYM=-TAC RULEP-TAC
  PROP-INTRO-RULES-TAC PROP-ELIM-RULES-TAC PUSHNEG-TAC UGEN-TAC
  RULEC-TAC SUB=-TAC PULLNEG-TAC INDIRECT-EXISTS-PLINE-TAC
  INDIRECT-DISJ-PLINE-TAC EQUIV-EQ-CONTR-TAC EQUIV-EQ-EXPD-TAC EXT=-TAC
  EXT=0-TAC ELIM-DEFNS-TAC LEXPD*-VARY-TAC LCONTR*-VARY-TAC))
@end(flushleft)


EXT-SEQ: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(REPEAT
 (ORELSE (CALL PRINT-ROUTINES) TRUE+TAC FALSE-TAC INIT-TAC REFL+TAC
  LAMBDA-TAC NOT-TAC OR+TAC AND-TAC IMPLIES+TAC ALL+TAC EXISTS-TAC
  EXTFUNC+TAC OR-TAC AND+TAC IMPLIES-TAC EQUIV+TAC EQUIV-TAC EXTO+TAC
  EQO-TAC ALL-TAC EXISTS+TAC EQFUNC-TAC EQUIVWFFS+TAC EQUIVWFFS-TAC
  INITEQ-TAC DEC+TAC EUNIF1-TAC EUNIF2-TAC CONTRACT-TAC INTERNALIZE+TAC
  INTERNALIZE-TAC))
@end(flushleft)


@end(description)

@IndexOther(IMPLIES+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(IMPLIES-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(INIT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(INITEQ-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(INTERNALIZE+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(INTERNALIZE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(LAMBDA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(MIN-PROP) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE SAME-TAC (IFTHEN USE-RULEP-TAC RULEP-TAC) TRUTH-TAC ABSURD-TAC
 INDIRECT2-TAC MAKE-ROOM DEDUCT-TAC
 (IFTHEN USE-RULEP-TAC ECONJ*-TAC ECONJ-TAC)
 (IFTHEN USE-RULEP-TAC ICONJ*-TAC ICONJ-TAC))
@end(flushleft)


@end(description)

@IndexOther(MONSTRO-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(REPEAT
 (ORELSE (CALL PRINT-ROUTINES) SAME-TAC REFL=-TAC SYM=-TAC RULEP-TAC
  PROP-INTRO-RULES-TAC PROP-ELIM-RULES-TAC PUSHNEG-TAC UGEN-TAC
  RULEC-TAC SUB=-TAC PULLNEG-TAC UI-HERBRAND-TAC
  INDIRECT-EXISTS-PLINE-TAC INDIRECT-DISJ-PLINE-TAC EQUIV-EQ-CONTR-TAC
  EQUIV-EQ-EXPD-TAC EXT=-TAC EXT=0-TAC ELIM-DEFNS-TAC LEXPD*-VARY-TAC
  LCONTR*-VARY-TAC))
@end(flushleft)


@end(description)

@IndexOther(NOT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(OR+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(OR-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(PFENNING*-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE (CALL PRINT-ROUTINES) SAME-TAC NNF-TAC ABSURD-TAC TRUTH-TAC
 REFL=-TAC (MAKE-ROOM :USE NAT-DED) DUPLICATE-SUPPORT-TAC
 (THEN** ECONJ-TAC UNSPONSOR-TAC) DEDUCT-TAC REWRITE-SLINE-TAC
 REWRITE-PLINE-TAC RULEC-TAC UGEN-TAC EGEN-TAC
 (THEN** UI-TAC UNSPONSOR-TAC) (THEN** UNNEC-EXP-TAC UNSPONSOR-TAC)
 (THEN** IDISJ-TAC UNSPONSOR-TAC) (THEN** ICONJ-TAC UNSPONSOR-TAC)
 (THEN** CASES-TAC UNSPONSOR-TAC) INEG-TAC ENEG-TAC SUBST=L-TAC
 SUBST=R-TAC MP-TAC (IFTHEN USE-SYMSIMP-TAC SYMSIMP-TAC CLASS-DISJ-TAC)
 INESS-PLINE-TAC INDIRECT-TAC NEG-REW-SLINE-TAC ML::NEG-EQUIV-SLINE-TAC)
@end(flushleft)
Intended to be the same as the tactics advocated in Pfenning's thesis.

@end(description)

@IndexOther(PFENNING-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(REPEAT PFENNING*-TAC)
@end(flushleft)
Intended to be the same as the tactics advocated in Pfenning's thesis.

@end(description)

@IndexOther(PLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE DEDUCT-TAC ICONJ-TAC IDISJ-RIGHT-TAC IDISJ-LEFT-TAC
 IMPLICS-EQUIV-TAC LEXPD*-VARY-TAC AB-PLAN-TAC EQUIV-WFFS-PLAN-TAC
 EQUALITY-PLAN-TAC RULEQ-PLAN-TAC UGEN-TAC EGEN-TAC TRUTH-TAC)
@end(flushleft)


@end(description)

@IndexOther(PROP-ELIM-RULES-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE INDIRECT2-TAC MAKE-ROOM ECONJ*-TAC CASES-TAC EQUIV-IMPLICS-TAC)
@end(flushleft)


@end(description)

@IndexOther(PROP-INTRO-RULES-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE TRUTH-TAC ABSURD-TAC MAKE-ROOM ICONJ*-TAC DEDUCT-TAC INEG-TAC
 IMPLICS-EQUIV-TAC)
@end(flushleft)


@end(description)

@IndexOther(REFL+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)

@IndexOther(REWRITE-PLINE-P-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Returns success if planned line represents a rewrite node.

@end(description)

@IndexOther(REWRITE-PLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(IFTHEN (REWRITE-PLINE-P-TAC)
 (ORELSE AB-PLAN-TAC EQUALITY-PLAN-TAC EQUIV-WFFS-PLAN-TAC
  RULEQ-PLAN-TAC LEXPD*-VARY-TAC IMPLICS-EQUIV-TAC
  ML::TRUTHP-REWRITE-PLAN-TAC DISJ-EQUIV-TAC))
@end(flushleft)


@end(description)

@IndexOther(REWRITE-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(IFTHEN (REWRITE-SLINE-P-TAC)
 (ORELSE AB-SLINE-TAC EQUALITY-SLINE-TAC EQUIV-WFFS-SLINE-TAC
  RULEQ-SLINE-TAC LCONTR*-VARY-TAC EQUIV-DISJ-TAC EQUIV-IMPLICS-TAC))
@end(flushleft)


@end(description)

@IndexOther(SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE ECONJ-TAC CASES-TAC MP-TAC UI-TAC RULEC-TAC ML::NEG-NEG-TAC
 NEG-AND-SLINE-TAC NEG-OR-SLINE-TAC NEG-IMP-SLINE-TAC NEG-SEL-SLINE-TAC
 NEG-EXP-SLINE-TAC EQUIV-DISJ-TAC EQUIV-IMPLICS-TAC LCONTR*-VARY-TAC
 EQUIV-WFFS-SLINE-TAC AB-SLINE-TAC RULEQ-SLINE-TAC EQUALITY-SLINE-TAC)
@end(flushleft)


@end(description)

@IndexOther(SUB=-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE SUBST=L-TAC SUBST=R-TAC)
@end(flushleft)


@end(description)

@IndexOther(TRUE+TAC) @\ Defined for the following uses:
@begin(description,spread 0)
EXT-SEQ:  is a primitive tactic.

@end(description)
@End(Description)

@Section(Propositional)

@Begin(Description)
@IndexOther(ABSURD-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
If a support line is FALSEHOOD applies absurdity rule.

ETREE-NAT:  is a primitive tactic.
If a support line is FALSEHOOD applies absurdity rule.

@end(description)

@IndexOther(BACKCHAIN-LEMMA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is an implication, sets up a symmetric simplification
problem using the antecedent of the implication in the lemma.  Then
symmetric simplification is performed.

@end(description)

@IndexOther(BASIC-PROP*-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE SAME-TAC ABSURD-TAC TRUTH-TAC NEG-ATOM-ELIM-TAC
 (MAKE-ROOM :USE NAT-DED) DUPLICATE-SUPPORT-TAC
 (THEN** ECONJ-TAC UNSPONSOR-TAC) DEDUCT-TAC
 (THEN** IDISJ-TAC UNSPONSOR-TAC) (THEN** ICONJ-TAC UNSPONSOR-TAC)
 (THEN** CASES-TAC UNSPONSOR-TAC) INEG-TAC MP-TAC IMPLICS-EQUIV-TAC
 EQUIV-IMPLICS-TAC ML::NEG-EQUIV-SLINE-TAC CLASS-DISJ-TAC
 NEG-NEG-ELIM-TAC NEG-AND-ELIM-TAC NEG-IMP-ELIM-TAC
 NEG-OR-ELIM-SIMPLE-TAC NEG-OR-ELIM-DUP-TAC INESS-PLINE-TAC
 INDIRECT-TAC)
@end(flushleft)
Similar to a subset of Pfenning*-tac using only basic propositional rules, avoiding rules such as RuleP

@end(description)

@IndexOther(BASIC-PROP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(REPEAT ML::BASIC-PROP*-TAC)
@end(flushleft)
Similar to a subset of Pfenning*-tac using only basic propositional rules, avoiding rules such as RuleP

@end(description)

@IndexOther(CASES-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies CASES if a support line is a disjunction.

ETREE-NAT:  is a primitive tactic.
If a support line is a disjunction, applies rule of cases. 
Pfenning's tactic 202.

@end(description)

@IndexOther(CLASS-DISJ-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line corresponds to a disjunction, and both of the disjuncts
are essential, applies indirect proof.  Same as Pfenning's tactic 229.

@end(description)

@IndexOther(DEDUCT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies DEDUCT if planned line is an implication.

ETREE-NAT:  is a primitive tactic.
Applies deduction rule if planned line corresponds to an implication node.
Same as Pfenning's tactic 191.

@end(description)

@IndexOther(DISJ-EQUIV-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.

@end(description)

@IndexOther(DISJ-IMP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies DISJ-IMP if a support line is of the form "~A or B".

@end(description)

@IndexOther(ECONJ*-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies conjunction elimination to a support line if applicable.
If support line is a multiple conjunction, completely breaks it up.

ETREE-NAT:  is a primitive tactic.
Applies conjunction elimination to a support line if applicable.
If support line is a multiple conjunction, completely breaks it up.

@end(description)

@IndexOther(ECONJ-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies ECONJ if a support line is a conjunction.

ETREE-NAT:  is a primitive tactic.
Applies conjunction elimination to a support line if applicable.
Pfenning's tactics 199-200, but regardless of whether the conjuncts
are both essential to proving the planned line.

@end(description)

@IndexOther(ENEG-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies ENEG if a support line is a negation and planned line is
FALSEHOOD.

ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE NEG-ATOM-ELIM-TAC NEG-NEG-ELIM-TAC NEG-AND-ELIM-TAC
 NEG-IMP-ELIM-TAC NEG-UNIV-ELIM-TAC NEG-EXISTS-ELIM-SIMPLE-TAC
 NEG-EXISTS-ELIM-DUP-TAC NEG-OR-ELIM-SIMPLE-TAC NEG-OR-ELIM-DUP-TAC
 NEG-EQUAL-ELIM-TAC)
@end(flushleft)


@end(description)

@IndexOther(EQUIV-DISJ-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a rewrite node from an equivalence to a disjunction,
carries out the rewrite.

@end(description)

@IndexOther(EQUIV-IMPLICS-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies EQUIV-IMPLICS if a support line is an equivalence.

ETREE-NAT:  is a primitive tactic.
If a support line is a rewrite node for an equivalence to a
conjunction, applies the equiv-implics rule.

@end(description)

@IndexOther(ICONJ*-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
If planned line corresponds to a conjunction node, splits into
subgoals.  Will break up a multiple conjunction into separate conjuncts.

ETREE-NAT:  is a primitive tactic.
If planned line corresponds to a conjunction node, splits into
subgoals.  Will break up a multiple conjunction into separate conjuncts.

@end(description)

@IndexOther(ICONJ-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies ICONJ if the planned line is a conjunction.

ETREE-NAT:  is a primitive tactic.
Applies ICONJ if planned line corresponds to  a conjunction node.
Same as Pfenning's tactic 186.

@end(description)

@IndexOther(IDISJ-LEFT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line corresponds to a disjunction, and the right disjunct
is inessential, infers the planned line from the left disjunct by RuleP.
Same as Pfenning's tactic 188.

@end(description)

@IndexOther(IDISJ-RIGHT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a disjunction and the left disjunct
is inessential, infers the planned line from the right disjunct by RuleP.
Same as Pfenning's tactic 189.

@end(description)

@IndexOther(IDISJ-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE IDISJ-RIGHT-TAC IDISJ-LEFT-TAC)
@end(flushleft)


@end(description)

@IndexOther(IMP-DISJ-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies IMP-DISJ if a support line is an implication.

@end(description)

@IndexOther(IMPLICS-EQUIV-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies IMPLICS-EQUIV if planned line is an equivalence.

ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a rewrite node with justification
equiv-implics, applies implics-equiv rule.

@end(description)

@IndexOther(INDIRECT-DISJ-PLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies INDIRECT rule, then pushes negation through quantifier, 
if planned line is a disjunction.

@end(description)

@IndexOther(INDIRECT-EXISTS-PLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies INDIRECT rule, then pushes negation through quantifier, 
if planned line is an existentially quantified line.

@end(description)

@IndexOther(INDIRECT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies INDIRECT as long as planned line is not FALSEHOOD.

ETREE-NAT:  is a primitive tactic.
Applies indirect proof.  This can almost always be applied when the
planned line is not FALSEHOOD.  It does not apply if the planned line
corresponds to a mated node and one of the support line corresponds
to the negation of that node.

@end(description)

@IndexOther(INDIRECT2-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies INDIRECT2 if two support lines are contradictory.

ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD, two support lines are contradictory, 
and are mated, applies indirect2 rule.  Same as Pfenning's tactic 212.

@end(description)

@IndexOther(INEG-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies INEG if the planned line is a negated formula.

ETREE-NAT:  is a primitive tactic.
Applies INEG if planned line is a negation.

@end(description)

@IndexOther(MP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies MP if a support line is an implication.

ETREE-NAT:  is a primitive tactic.
If a support line is an implication, planned line follows from the
succedent and the antecedent is provable, applies Modus Ponens.  Same
as Pfenning's tactic 209.

@end(description)

@IndexOther(NEG-AND-ELIM-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD, and a support line is a negated conjunction,
applies eneg rule.  Same as Pfenning's tactic 215.

@end(description)

@IndexOther(NEG-AND-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is a negated conjunction, applies indirect proof,
assuming negated planned line with new goal of falsehood.

@end(description)

@IndexOther(NEG-AND-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated conjunction, applies indirect proof.
Similar to Pfenning's tactic 215.

@end(description)

@IndexOther(NEG-ATOM-ELIM-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD and it has two complementary support lines
which are mated, applies eneg rule.  Same as Pfenning's tactic 212.

@end(description)

@IndexOther(NEG-EQUAL-ELIM-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated equality and planned line is falsehood,
applies eneg.  Similar to Pfenning's tactic 217.

@end(description)

@IndexOther(NEG-EXISTS-ELIM-DUP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD, and a support line is a negated existentially
quantified formula with more than one expansion, one of which is admissible,
applies eneg rule, adding the line with its other expansions as a support.
Same as Pfenning's tactic 221.

@end(description)

@IndexOther(NEG-EXISTS-ELIM-SIMPLE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD, and a support line is a negated existentially
quantified formula with exactly one admissible expansion, applies eneg rule.
Same as Pfenning's tactic 220.

@end(description)

@IndexOther(NEG-IMP-ELIM-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD, and a support line is a negated implication,
applies eneg rule.  Same as Pfenning's tactic 216.

@end(description)

@IndexOther(NEG-IMP-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned is a negated implication, applies pullneg rule.

@end(description)

@IndexOther(NEG-IMP-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated implication, pushes the negation through
creating a conjunction.

@end(description)

@IndexOther(NEG-NEG-ELIM-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD and it has doubly-negated support line,
applies eneg rule.  Same as Pfenning's tactic 214.

@end(description)

@IndexOther(NEG-NEG-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is a double negation, applies the pullneg
rule.

@end(description)

@IndexOther(NEG-NEG-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a double negation, removes the negations.

@end(description)

@IndexOther(NEG-OR-ELIM-DUP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD, and a support line is a negated disjunction
both of whose disjuncts is essential, applies eneg rule, 
adding the line with its other expansions as a support.
Same as Pfenning's tactic 219.

@end(description)

@IndexOther(NEG-OR-ELIM-SIMPLE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD, and a support line is a negated disjunction,
one of whose disjuncts is inessential (but not both), applies eneg rule.
Same as Pfenning's tactic 218.

@end(description)

@IndexOther(NEG-OR-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is a negated disjunction, applies pullneg rule.

@end(description)

@IndexOther(NEG-OR-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated disjunction, pushes the negation
through, creating a conjunction.

@end(description)

@IndexOther(NEG-UNIV-ELIM-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is FALSEHOOD, and a support line is a negated universally
quantified formula, applies eneg rule.  Same as Pfenning's tactic 217.

@end(description)

@IndexOther(OR-LEMMA-LEFT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Pfenning's tactic 265.

@end(description)

@IndexOther(OR-LEMMA-RIGHT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Pfenning's tactic 265.

@end(description)

@IndexOther(OR-LEMMA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Applies either or-lemma-right-tac or or-lemma-left-tac if applicable.

@end(description)

@IndexOther(PROP-PRIM) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE SAME-TAC TRUTH-TAC ABSURD-TAC INDIRECT2-TAC MAKE-ROOM ECONJ-TAC
 ICONJ-TAC EQUIV-IMPLICS-TAC IMPLICS-EQUIV-TAC PUSHNEG-TAC PULLNEG-TAC
 DEDUCT-TAC MP-TAC CASES-TAC INDIRECT-TAC)
@end(flushleft)
Much like tactic defined in Felty's master's thesis, p. 64.

@end(description)

@IndexOther(PROPOSITIONAL) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(REPEAT
 (ORELSE MAKE-ROOM (TRY (REPEAT PROP-PRIM))
  (THEN INDIRECT-TAC PROPOSITIONAL)))
@end(flushleft)
First tries PROP-PRIM repeatedly.  If any goals remain, what work
was done is thrown away, indirect proof is applied, and PROPOSITIONAL
is called recursively on the new goal.

@end(description)

@IndexOther(PULLNEG-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies PULLNEG if the planned line is a negated non-literal formula.

ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(IFTHEN (NEG-PLINE-P-TAC)
 (ORELSE NEG-NEG-PLAN-TAC NEG-OR-PLAN-TAC NEG-IMP-PLAN-TAC
  NEG-SEL-PLAN-TAC NEG-EXP-PLAN-TAC NEG-REW-PLAN-TAC))
@end(flushleft)


@end(description)

@IndexOther(PUSHNEG-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies PUSHNEG if a support line is a negated non-literal formula.

ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(IFTHEN (NEG-SLINE-P-TAC)
 (ORELSE NEG-NEG-SLINE-TAC NEG-OR-SLINE-TAC NEG-IMP-SLINE-TAC
  NEG-SEL-SLINE-TAC NEG-EXP-SLINE-TAC NEG-REW-SLINE-TAC))
@end(flushleft)


@end(description)

@IndexOther(RULEP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Attempts to apply RULEP; fails if planned line doesn't follow
from supports by RuleP.

ETREE-NAT:  is a primitive tactic.
Applies RuleP if possible.

@end(description)

@IndexOther(SAME-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies SAME if planned line is the same as a support line.

ETREE-NAT:  is a primitive tactic.
If planned line is the same as a support line, and they are mated,
applies SAME.  Pfenning's tactic 173.

@end(description)

@IndexOther(SUBST=-BACKCHAIN-LEMMA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If substitution of equality can be applied to a support line, creates
a new disjunctive lemma based on the formula to which the equality can
be applied.  Then symmetric simplification is used to simplify the lemma.

@end(description)

@IndexOther(TRUTH-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies RuleP if the planned line is TRUTH.

ETREE-NAT:  is a primitive tactic.
Applies ITruth if the planned line is TRUTH.

@end(description)

@IndexOther(TRUTHP-REWRITE-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a rewrite node with justification
truthp, justifies the line by ad hoc Truthp,
and makes a new planned line with the rewritten wff.

@end(description)
@End(Description)

@Section(Quantifiers)

@Begin(Description)
@IndexOther(AB-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a rewrite node with justification
ab, applies the ab* rule.

@end(description)

@IndexOther(AB-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a rewrite node justified by ab, applies the
ab* rule.

@end(description)

@IndexOther(ABU-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
If planned line is universally quantified, will apply ABU, prompting
for a variable if in interactive mode.

@end(description)

@IndexOther(EDEF-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies EDEF if a support line contains a definition.

@end(description)

@IndexOther(EGEN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
If the planned line is existentially quantified, will apply EGEN,
prompting for the term if in interactive mode.

ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a expansion node with a single
admissible expansion term, applies EGEN using that term.  Same as Pfenning's
tactic 195.

@end(description)

@IndexOther(EXISTS-LEMMA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Pfenning's tactic 264.

@end(description)

@IndexOther(IDEF-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies IDEF if planned line contains a definition.

@end(description)

@IndexOther(NEG-EXP-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is a negated expansion node with only one expansion term,
applies pullneg rule.

@end(description)

@IndexOther(NEG-EXP-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated expansion node, pushes negation through
the quantifier.

@end(description)

@IndexOther(NEG-SEL-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned is a negated selection node, applies pullneg.

@end(description)

@IndexOther(NEG-SEL-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated selection node, pushes the negation
through the quantifier.

@end(description)

@IndexOther(QUANTIFICATIONAL) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE UGEN-TAC (THEN ABU-TAC UGEN-TAC) EGEN-TAC UI-TAC)
@end(flushleft)


@end(description)

@IndexOther(RULEC-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
If a support line is existentially quantified, will apply RULEC
with a brand new variable.

ETREE-NAT:  is a primitive tactic.
If a support line corresponds to a selection node, applies 
RuleC.  Same as Pfenning's tactic 207.

@end(description)

@IndexOther(RULEQ-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a rewrite node with justification
ruleq (minimized quantifier scopes), justifies the line by ad hoc RuleQ,
and makes a new planned line with the rewritten wff.

@end(description)

@IndexOther(RULEQ-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a rewrite node justified by ruleq, applies
the rewrite.

@end(description)

@IndexOther(SYMSIMP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE EXISTS-LEMMA-TAC OR-LEMMA-TAC)
@end(flushleft)
Pfenning's symmetric simplification tactics.

@end(description)

@IndexOther(UGEN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies UGEN if planned line is universally quantified.

ETREE-NAT:  is a primitive tactic.
If the planned line is a skolem or selection node, applies UGEN.
Same as Pfenning's tactic 194.

@end(description)

@IndexOther(UI-HERBRAND-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
UI-HERBRAND-TAC is a tactic for automatically applying universal
instantiation.  The terms that are used are generated by finding all
subterms of the appropriate type (except quantified variables) and applying
to them all functions of the appropriate type to get all possible new terms.
I.e., you can think of it as constructing the Herbrand universe one level
at a time.  The number of times that this can be done for any individual 
quantified formula is controlled by the flag UI-HERBRAND-LIMIT.

@end(description)

@IndexOther(UI-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
If a support line is universally quantified, will instantiate it.  In
interactive mode will ask for a term, otherwise will use the bound
variable itself.

ETREE-NAT:  is a primitive tactic.
If a support node is an expansion node with an admissible expansion,
applies universal instantiation.  Pfenning's tactics 204/205.  If a support
line has multiple expansions, it will be duplicated, with the duplication
receiving just the excess expansion terms.  The instantiated line will
not become a support of any other goal than the current one, since it
is not known if it is yet admissible for others.  The original support
line will be dropped from the supports of the current goal, but
remain as a support for any other goals.  The new support lines
will be supports only for the current goal.

@end(description)

@IndexOther(UNNEC-EXP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If the planned line is an expansion node, deletes any unnecessary
expansion terms.

@end(description)
@End(Description)

@Section(Equality)

@Begin(Description)
@IndexOther(EQUALITY-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE EXT=-PLAN-TAC LEIBNIZ=-PLAN-TAC)
@end(flushleft)
If the planned line corresponds to rewrite node with justification
for a rewritten equality, justifies the line appropriately,
and makes a new planned line with the rewritten wff.

@end(description)

@IndexOther(EQUALITY-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE EXT=-SLINE-TAC LEIBNIZ=-SLINE-TAC)
@end(flushleft)
If a support line is a rewrite node rewritten because of an equality,
carries out the rewrite.

@end(description)

@IndexOther(EXT=-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to rewrite node with justification
for a rewritten equality using extensionality, justifies the line 
appropriately, and makes a new planned line with the rewritten wff.

@end(description)

@IndexOther(EXT=-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line corresponds to rewrite node with justification
for a rewritten equality using extensionality, justifies the line 
appropriately, and makes a new support line with the rewritten wff.

@end(description)

@IndexOther(LEIBNIZ=-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to rewrite node with justification
for a rewritten equality using the Leibniz definition, justifies the line 
appropriately, and makes a new planned line with the rewritten wff.

@end(description)

@IndexOther(LEIBNIZ=-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line corresponds to rewrite node with justification
for a rewritten equality using the Leibniz definition, justifies the line 
appropriately, and makes a new support line with the rewritten wff.

@end(description)

@IndexOther(NEG-EQUAL-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated equality and planned line is falsehood,
applies indirect proof.  Similar to Pfenning's tactic 217.

@end(description)

@IndexOther(REFL=-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies rule for reflexivity of equality if planned line is of
form a=a.

ETREE-NAT:  is a primitive tactic.
If the planned line is a rewrite node with justification REFL=, applies
the ASSERT rule for reflexivity of equality.  See Pfenning's theorem 141.1.

@end(description)

@IndexOther(SUBST=-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Applies either SUBST=L-TAC or SUBST=R-TAC as appropriate.

@end(description)

@IndexOther(SUBST=L-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies SUBST=L if planned line follows by this rule from a support line.

ETREE-NAT:  is a primitive tactic.
If a support line is an equality, and the planned line follows from the
substituting the right-hand-side for the left-hand-side in some wff provable
from the other supports, applies Subst=L.  See Pfenning's theorem 141.

@end(description)

@IndexOther(SUBST=R-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies SUBST=R if planned line follows by this rule from a support line.

ETREE-NAT:  is a primitive tactic.
If a support line is an equality, and the planned line follows from the
substituting the left-hand-side for the right-hand-side in some wff provable
from the other supports, applies Subst=R.  See Pfenning's theorem 141.

@end(description)

@IndexOther(SYM=-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies symmetry of equality if planned line follows by that rule from
some support line.

@end(description)
@End(Description)

@Section(Definitions)

@Begin(Description)
@IndexOther(EQUIV-WFFS-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a rewrite node with justification
equivwffs (instantiated definitions), applies equiv-wffs rule.

@end(description)

@IndexOther(EQUIV-WFFS-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a rewrite node justified by equiv-wffs (instantiating
definitions), applies the appropriate rule.

@end(description)

@IndexOther(NEG-EQUIV-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated equiv-implics rewrite node,
and the planned line is FALSEHOOD, do an eneg to make the support line
the planned line without the negation, then do the rewrite.

@end(description)

@IndexOther(NEG-REW-PLAN-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is a negated rewrite node, carry out the rewrite,
leaving the negation.

@end(description)

@IndexOther(NEG-REW-SLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is a negated rewrite node, carry out the rewrite,
leaving the negation above.

@end(description)
@End(Description)

@Section(Lambda)

@Begin(Description)
@IndexOther(BETA-ETA-SEPARATE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Returns success if LAMBDA-CONV is BETA-ETA-SEPARATE.

ETREE-NAT:  is a primitive tactic.
Returns success if LAMBDA-CONV is BETA-ETA-SEPARATE.

@end(description)

@IndexOther(BETA-ETA-TOGETHER-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Returns success if LAMBDA-CONV is BETA-ETA-TOGETHER.

ETREE-NAT:  is a primitive tactic.
Returns success if LAMBDA-CONV is BETA-ETA-TOGETHER.

@end(description)

@IndexOther(BETA-ONLY-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Returns success if LAMBDA-CONV is BETA-ONLY.

ETREE-NAT:  is a primitive tactic.
Returns success if LAMBDA-CONV is BETA-ONLY.

@end(description)

@IndexOther(EQUIV-EQ-CONTR-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies EQUIV-EQ-CONTR if planned line is appropriate.

@end(description)

@IndexOther(EQUIV-EQ-EXPD-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies EQUIV-EQ-EXPD, if that will change the support line.

@end(description)

@IndexOther(EXT=-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies EXT= if planned line is appropriate.

@end(description)

@IndexOther(EXT=0-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies EXT=0 if planned line is appropriate.

@end(description)

@IndexOther(LCONTR*-BETA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies LCONTR*-BETA, if that will change the support line.

ETREE-NAT:  is a primitive tactic.
If a support line is a rewrite node justified by beta, applies
lcontr*-beta rule.

@end(description)

@IndexOther(LCONTR*-ETA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies LCONTR*-ETA, if that will change the support line.

ETREE-NAT:  is a primitive tactic.
If a support line is a rewrite node justified by eta, applies
lcontr*-eta rule.

@end(description)

@IndexOther(LCONTR*-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies LCONTR*, if that will change the support line.

ETREE-NAT:  is a primitive tactic.
If a support line is a rewrite node justified by lambda, applies
lcontr* rule.

@end(description)

@IndexOther(LCONTR*-VARY-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE (IFTHEN BETA-ETA-TOGETHER-TAC LCONTR*-TAC)
 (IFTHEN BETA-ONLY-TAC LCONTR*-BETA-TAC)
 (IFTHEN BETA-ETA-SEPARATE-TAC
  (ORELSE LCONTR*-BETA-TAC LCONTR*-ETA-TAC)))
@end(flushleft)
Decides which sort of lambda contraction to do, based
on the setting of LAMBDA-CONV.

ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE (IFTHEN BETA-ETA-TOGETHER-TAC LCONTR*-TAC)
 (IFTHEN BETA-ONLY-TAC LCONTR*-BETA-TAC)
 (IFTHEN BETA-ETA-SEPARATE-TAC
  (ORELSE LCONTR*-BETA-TAC LCONTR*-ETA-TAC)))
@end(flushleft)
Decides which sort of lambda contraction to do, based
on the setting of LAMBDA-CONV.

@end(description)

@IndexOther(LEXPD*-BETA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies LEXPD*-BETA, if that will change the planned line.

ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a rewrite node with justification
beta, applies lexpd*-beta rule.

@end(description)

@IndexOther(LEXPD*-ETA-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies LEXPD*-ETA, if that will change the planned line.

ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a rewrite node with justification
eta, applies lexpd*-eta rule.

@end(description)

@IndexOther(LEXPD*-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Applies LEXPD*, if that will change the planned line.

ETREE-NAT:  is a primitive tactic.
If the planned line corresponds to a rewrite node with justification
lambda, applies lexpd* rule.

@end(description)

@IndexOther(LEXPD*-VARY-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE (IFTHEN BETA-ETA-TOGETHER-TAC LEXPD*-TAC)
 (IFTHEN BETA-ONLY-TAC LEXPD*-BETA-TAC)
 (IFTHEN BETA-ETA-SEPARATE-TAC (ORELSE LEXPD*-BETA-TAC LEXPD*-ETA-TAC)))
@end(flushleft)
Decides which sort of lambda expansion to do, based
on the setting of LAMBDA-CONV.

ETREE-NAT: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(ORELSE (IFTHEN BETA-ETA-TOGETHER-TAC LEXPD*-TAC)
 (IFTHEN BETA-ONLY-TAC LEXPD*-BETA-TAC)
 (IFTHEN BETA-ETA-SEPARATE-TAC (ORELSE LEXPD*-BETA-TAC LEXPD*-ETA-TAC)))
@end(flushleft)
Decides which sort of lambda expansion to do, based
on the setting of LAMBDA-CONV.

@end(description)
@End(Description)

@Section(Auxiliary)

@Begin(Description)
@IndexOther(DUPLICATE-SUPPORT-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If a support line is part of the mating, the duplicate the
line, where the original line will remain a support line and
where support line tactics can be applied to the copy.
This is needed to make proofs with non-leaf matings translate properly.
See Pfenning's Tactic 183.

@end(description)

@IndexOther(FINISHED-P) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Returns success if current proof has no remaining planned lines.

@end(description)

@IndexOther(INESS-PLINE-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
If planned line is not FALSEHOOD and it is inessential, applies
absurdity rule.  Same as Pfenning's tactic 224.

@end(description)

@IndexOther(MAKE-NICE) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED: 
@begin(flushleft, font smallbodyfont,facecode f, size -2)
(SEQUENCE (CALL CLEANUP) (CALL SQUEEZE) (CALL PALL))
@end(flushleft)
Cleans up a completed proof.

@end(description)

@IndexOther(MAKE-ROOM) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Ensures that there is room for at least four new lines before
the planned line.

@end(description)

@IndexOther(NEG-PLINE-P-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Returns success if planned line represents a negation node.

@end(description)

@IndexOther(NEG-SLINE-P-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Returns success if some support line represents a negation node.

@end(description)

@IndexOther(NNF-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Closes a gap when a support line is the same as the planned line up to NNF, and the nodes are mated.

@end(description)

@IndexOther(RESTRICT-MATING-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Restricts the mating of the planned line to only those connections
involving the line and its supports.  Always succeeds.

@end(description)

@IndexOther(REWRITE-SLINE-P-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Returns success if some support line represents a rewrite node.

@end(description)

@IndexOther(SHOW-CURRENT-PLAN) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Shows the current planned line.

ETREE-NAT:  is a primitive tactic.
Shows the current planned line.

@end(description)

@IndexOther(SHOW-PLANS) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Shows current plan support structure for all planned lines.

ETREE-NAT:  is a primitive tactic.
Shows current plan support structure for all planned lines.

@end(description)

@IndexOther(UNIVERSAL-GOAL-P) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Returns success if planned line is universally quantified.

@end(description)

@IndexOther(UNSPONSOR-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Removes any support lines which are not required for the planned line.

@end(description)

@IndexOther(USE-RULEP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
NAT-DED:  is a primitive tactic.
Returns success if value of the flag USE-RULEP is T.

ETREE-NAT:  is a primitive tactic.
Returns success if value of the flag USE-RULEP is T.

@end(description)

@IndexOther(USE-SYMSIMP-TAC) @\ Defined for the following uses:
@begin(description,spread 0)
ETREE-NAT:  is a primitive tactic.
Returns success if value of the flag USE-SYMSIMP is T.

@end(description)
@End(Description)
@ChapterPh(Tacticals)
The internal name of this category is 
TACTICAL.
A tactical can be defined using DEFTACTICAL.
Allowable properties are: @t{DEFN}, @t{MHELP}.

@Section(Tactics)

@Begin(Description)
@IndexOther(CALL)@\
(CALL command) will execute command as if it were entered at the
top level by the user.  CALL is used only for side effects, the goal is
always returned.

@IndexOther(COMPOSE)@\
(COMPOSE tac1 tac2 ... tacn) will apply its argument tactics
in order, composing their results until one of them fails.

@IndexOther(FAILTAC)@\
Tactical which always fails, returns its goal unchanged.

@IndexOther(IDTAC)@\
Tactical which always succeeds, returns its goal unchanged.

@IndexOther(IFTHEN)@\
(IFTHEN test tactic1 [tactic2]) will first evaluate test, which may
be either a tactical or (if user is an expert) an arbitrary LISP expression.
If test is a tactical and does not fail, or is a LISP expression which does
not evaluate to nil, then tactic1 will be executed and IFTHEN will return its
results.  If test fails or is nil, then tactic2  (if present) will be executed
and its results returned by IFTHEN.  Tactic2 is optional; if not specified,
and test fails, IFTHEN will return failure.

@IndexOther(NO-GOAL)@\
(NO-GOAL) succeeds iff the goal with which it is invoked is nil.

@IndexOther(ORELSE)@\
Given a list of tactics, ORELSE will apply the first one which succeeds.

@IndexOther(REPEAT)@\
(REPEAT tactic) will apply tactic repeatedly until it fails on
every subgoal which has been created.

@IndexOther(SEQUENCE)@\
(SEQUENCE TAC1 ... TACn) applies tactics TAC1, ..., TACn in order,
regardless of their success or failure. 

@IndexOther(THEN)@\
(THEN tactic1 tactic2) will first apply tactic1; if it fails
 then failure is returned, otherwise tactic2 is applied to each resulting
 goal.  If tactic2 fails on any of these goals, then failure is returned,
 otherwise the new goals obtained from the calls to tactic2 are returned.

@IndexOther(THEN*)@\
(THEN* tactic1 tactic2) will first apply tactic1; if it fails
 then failure is returned, otherwise tactic2 is applied to each resulting
 goal.  If tactic2 fails on any of these goals, then the new goals obtained
 as a result of applying tactic1 are returned, otherwise the new goals
 obtained as the result of applying both tactic1 and tactic2 are returned.

@IndexOther(THEN**)@\
(THEN** tactic1 tactic2) will first apply tactic1 to the current
goal.  If it does not fail, tactic2 will be applied to the goals which are
produced by tactic1, and success will be returned along with any new goals
produced.  If tactic1 fails, failure will be returned.  Differs from THEN
and THEN* in that the current goal will never be copied.

@IndexOther(TRY)@\
(TRY tactic) will use tactic on the current object.  If any goals
remain after tactic finishes, then the original object will be restored,
otherwise the work done by tactic will be kept.@End(Description)
@ChapterPh(Mating-Search Commands)
The internal name of this category is 
MATEOP.
A mating-search command can be defined using DEFMATEOP.
Allowable properties are: @t{MATE-ALIAS}, @t{MATE-RESULT->}, @t{MATEWFF-ARGNAME}, @t{MATE-DEFAULTFNS}, @t{MATE-APPLICABLE-P}, @t{MATE-MOVE-FN}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
Exit mating-search.  If the current expansion proof is
complete, the user will be prompted as to whether to apply MERGE-TREE 
before exiting.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(ETD)@\
Etree Display: print an expansion tree into list form,
printing shallow formulas for leaf nodes only. The format used is
NODE [selection and expansion terms] ; CHILDREN or SHALLOW FORMULA

@IndexOther(ETP)@\
Etree Print: print an expansion tree into list form, 
printing shallow formulas for all nodes. The format used is 
NODE [selection and expansion terms] ; CHILDREN ; SHALLOW FORMULA

@IndexOther(P)@\
Print the current node

@IndexOther(PDEEP)@\
Print the deep formula of an expansion tree.

@IndexOther(PP)@\
Print an expansion tree with node-names.

@IndexOther(PPDEEP)@\
Pretty-print the deep formula of an expansion tree.

@IndexOther(PPF)@\
Print the current proof.

@IndexOther(PPNODE)@\
Print an expansion tree with node-names.

@IndexOther(PSH)@\
Print the shallow formula of an expansion tree.

@IndexOther(PTREE) @i{gwff}@\
Print out the etree below the current topnode, showing expansion
variables, skolem terms, selection terms, and rewrite justifications.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider, or use SHOWNOTYPES. See also PTREE*

@IndexOther(PTREE*) @i{gwff}@\
Print out the etree below the current topnode, showing expansion
variables, skolem terms, selection terms, and rewrite justifications. For
all other nodes, show the shallow formula at that node.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider, or use SHOWNOTYPES. See also PTREE

@IndexOther(PTREE-FILE) @i{file} @i{width} @i{fmlas}@\
As for PTREE or PTREE*, but send the output to a file.
For a width of 200 characters, you can print the results using
some variant of the following:
"enscript -r -fCourier-Bold6 -dberyl <filename> "

@IndexOther(SHOW-OPTION-TREE)@\
Show the current option-tree.@End(Description)

@Section(Recording)

@Begin(Description)
@IndexOther(O)@\
Invert PRINTMATEFLAG, that is switch automatic recording of mating-search
into a file either on or off. This has not actually been implemented!

@IndexOther(REM) @i{rm}@\
Write a remark into the PRINTMATEFILE.@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexOther(DP)@\
Deepen a single leaf of an expansion tree.

@IndexOther(DP*)@\
Iteratively deepen an expansion tree until all leaves are
literals.

@IndexOther(DP=)@\
Deepen top level equality in the etree.

@IndexOther(DPTREE)@\
Deepen every leaf node of an expansion tree.

@IndexOther(DUP-ALL)@\
Duplicate all variables in an expansion tree.

@IndexOther(DUP-OUTER)@\
Duplicate all outermost variables in an expansion tree.

@IndexOther(DUP-VAR)@\
Duplicate a variable at an expansion node.

@IndexOther(EXP) @i{term}@\
EXPAND a given universal or existential quantifier.

@IndexOther(MOD-STATUS) @i{status}@\
Set the status of the current-topnode to the specified
value. If the status of a node is not positive, it is ignored during
mating search.

@IndexOther(NAME-PRIM)@\
If PRIMSUB-METHOD is something other than PR00,
NAME-PRIM lists all possible primitive substitutions for the current
shallow formula. See the flags PRIM-BDTYPES, MIN-PRIM-DEPTH, 
MAX-PRIM-DEPTH and PRIM-QUANTIFIER for information on how to change
which substitutions are generated.  One can use PRIM-SINGLE to
instantiate a set variable with one of the generated primsubs.

If PRIMSUB-METHOD is PR00, this creates a list of instantiated
etrees.  One can choose to do a mating search on one of these
using the mate operation SET-SEARCH-TREE.

@IndexOther(PRIM-ALL)@\
Apply primitive substitutions at all outermost expansion nodes.

@IndexOther(PRIM-OUTER)@\
Apply primitive substitutions at all outer expansion nodes.

@IndexOther(PRIM-SINGLE) @i{subst} @i{var}@\
Applies a single primsub. These can be generated by using
the NAME-PRIM command. The command PRIM-SINGLE destructively alters
the etree and creates a new jform, and is basically equivalent to
SUB-ETREE followed by DP* and CJFORM. The variable must be specified
in full detail, with both superscript and type, as in the vpform 
(e.g. "r^1(ob(ob))").

@IndexOther(PRIM-SUB)@\
Apply primitive substitutions at an expansion node.

@IndexOther(RESTORE-ETREE) @i{loadfile}@\
Loads an etree and makes this the current etree.

Example of how to use SAVE-ETREE for X2106 and later use RESTORE-ETREE:
<3>MATE x2106
<Mate4>GO
<Mate5>MERGE-TREE
<Mate6>SAVE-ETREE
SAVEFILE (FILESPEC): File in which to save the etree ["x2106.etr"]>
Later come back into TPS and do the following:
<0>MATE x2108 (or MATE whatever)
<Mate1>RESTORE-ETREE
LOADFILE (FILESPEC): File in which to load the etree ["x2108.etr"]>"x2106.etr"
<Mate2>GO
<Mate3>LEAVE
Merge the expansion tree? [Yes]>Y
Now ETREE-NAT should work.


@IndexOther(SAVE-ETREE) @i{savefile}@\
Converts the current etree to an internal representation and saves this to a file.
This currently only works for etrees generated with SKOLEM-DEFAULT nil.

Example of how to use SAVE-ETREE for X2106 and later use RESTORE-ETREE:
<3>MATE x2106
<Mate4>GO
<Mate5>MERGE-TREE
<Mate6>SAVE-ETREE
SAVEFILE (FILESPEC): File in which to save the etree ["x2106.etr"]>
Later come back into TPS and do the following:
<0>MATE x2108 (or MATE whatever)
<Mate1>RESTORE-ETREE
LOADFILE (FILESPEC): File in which to load the etree ["x2108.etr"]>"x2106.etr"
<Mate2>GO
<Mate3>LEAVE
Merge the expansion tree? [Yes]>Y
Now ETREE-NAT should work.


@IndexOther(SEL)@\
SELECT for a given universal or existential quantifier.

@IndexOther(SET-SEARCH-TREE) @i{etree}@\
Set the current etree to be a tree generated and named by NAME-PRIM
when PRIMSUB-METHOD is PR00.

@IndexOther(SUB) @i{gwff} @i{skolemize} @i{deepen}@\
Create an expansion tree from a gwff0.

@IndexOther(SUB-ETREE) @i{term} @i{var}@\
Substitute a term for a variable throughout an expansion tree. 
Destructively alters the expansion tree.

@IndexOther(TERMS)@\
Get the expansion terms of an expansion node or the
selected variable of a selection node.@End(Description)

@Section(Search Suggestions)

@Begin(Description)
@IndexOther(ETR-INFO)@\
Print information about the expansion tree@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexOther(ADD-EXT-LEMMAS)@\
Automatically add extensionality lemmas to the expansion tree.

See Also:  USE-EXT-LEMMAS

@IndexOther(GO)@\
Start mating search using default mating search (controlled
by flag DEFAULT-MS).

@IndexOther(NOOP)@\
Do nothing. (TPS uses this internally.)

@IndexOther(UNIFY)@\
Call unification in interactive mode for active mating. The unification
tree associated with the active-mating is passed on to the unification
top-level. Any changes made to this tree are destructive. Applicable only for
a higher-order unification problem. Uses MS88-style unification.@End(Description)

@Section(MS88 search procedure)

@Begin(Description)
@IndexOther(ADD-CONN) @i{first} @i{second}@\
Add a connection to the current mating. TPS will not allow you to
add a connection to a mating if adding it causes the resulting mating to be
non unifiable. No check is made to determine if the connection spans
an open path.

@IndexOther(ADD-CONN*)@\
Repeatedly call ADD-CONN.

@IndexOther(APPLY-SUBSTS)@\
Apply substitutions found during mating search to JFORM. Applicable
only if mating is complete.

@IndexOther(COMPLETE-P)@\
Test whether current mating is complete. Will return a path that
is not spanned by the mating otherwise.

@IndexOther(INIT-MATING)@\
No further help available.  Sorry.

@IndexOther(MINIMAL-P)@\
A mating M is non-minimal if it contains some connection 
c such that M-{c} spans exactly the same vertical paths as M.
MINIMAL-P will find such a connection if it exists; otherwise
it will report that the mating is minimal.

@IndexOther(MS88)@\
Call mating search procedure on the current eproof.  This
procedure uses a naive level-saturation method, exhaustively searching
a single jform before applying any duplications. Quantifier duplications
are applied uniformly to outermost quantifiers. Will try primitive
substitution for outermost variable only.  Works on only a single
jform at a time.

@IndexOther(MS88-SUB) @i{etree}@\
Call MS88 on a partial expansion tree (subtree).

@IndexOther(REM-CONN) @i{first} @i{second}@\
Remove a connection from the current mating.

@IndexOther(REM-CONN*)@\
Repeatedly call REM-CONN.

@IndexOther(REM-LAST-CONN)@\
Remove the last connection to the current mating.

@IndexOther(SHOW-MATING)@\
Show the connections in the current mating.

@IndexOther(SHOW-SUBSTS)@\
Show the substitutions suggested by mating search for the complete
    active mating.@End(Description)

@Section(MS89 search procedure)

@Begin(Description)
@IndexOther(MS89)@\
Begin mating search MS89 on the current expansion proof.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS88 is used.  The flags
MAX-SEARCH-LIMIT, SEARCH-TIME-LIMIT, and RANK-EPROOF-FN are used to
control the search.  See also the command SHOW-OPTION-TREE.@End(Description)

@Section(MS90-3 search procedure)

@Begin(Description)
@IndexOther(EXPAND-ETREE)@\
Convert the jform proof found by path-focused duplication
procedures MS90-3 and MS90-9 into an expansion proof.

@IndexOther(MS90-3)@\
Start mating search procedure MS90-3 on current eproof.
This search procedure incorporates Issar's path-focused duplication,
but works on just one jform at a time.  Only duplications are done,
not primitive substitutions.  This is not an interactive procedure.

@IndexOther(PROP-MSEARCH)@\
Start Sunil's propositional mating search procedure.
This search procedure only works on propositional jforms.@End(Description)

@Section(MS90-9 search procedure)

@Begin(Description)
@IndexOther(MS90-9)@\
Begin mating search MS90-9 on the current expansion proof.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS90-3 is used.  The flags
MAX-SEARCH-LIMIT, SEARCH-TIME-LIMIT, and RANK-EPROOF-FN are used to
control the search.  See also the command SHOW-OPTION-TREE.@End(Description)

@Section(MS91-6 and MS91-7 search procedures)

@Begin(Description)
@IndexOther(MS91-6)@\
Begin mating search MS91-6 on the current expansion proof.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS88 is used.  The flags
MAX-SEARCH-LIMIT and SEARCH-TIME-LIMIT are used to control the amount
of time spent on each jform.

The order in which the possible jforms are considered depends on a 
number of flags. Firstly, the primitive substitutions which are generated 
are determined by the values of MAX-PRIM-DEPTH, MIN-PRIM-DEPTH,
PRIM-QUANTIFIER and NEG-PRIM-SUB. If DUP-ALLOWED is T, then additional
options are generated corresponding to duplicated quantifiers. These
options are then combined into sets; because there can be many such sets,
the flag NEW-OPTION-SET-LIMIT controls how many are generated at once.
Each set is given a weighting (see flags WEIGHT-x-COEFFICIENT and 
WEIGHT-x-FN, for x = A,B,C), and the lowest-weighted set is chosen
for searching. If the weight of the lowest-weighted set is too large,
TPS may generate more sets; the interpretation of "too large" is given
by MS91-WEIGHT-LIMIT-RANGE. If the search fails, it will be discarded;
if it runs out of time then it will be re-weighted to be continued
later (see RECONSIDER-FN).

@IndexOther(MS91-7)@\
Begin mating search MS91-7 on the current expansion proof.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS90-3 is used.  The flags
MAX-SEARCH-LIMIT and SEARCH-TIME-LIMIT are used to control the amount
of time spent on each jform.

The order in which the possible jforms are considered depends on a 
number of flags. Firstly, the primitive substitutions which are generated 
are determined by the values of MAX-PRIM-DEPTH, MIN-PRIM-DEPTH,
PRIM-QUANTIFIER and NEG-PRIM-SUB. If DUP-ALLOWED is T, then additional
options are generated corresponding to duplicated quantifiers. These
options are then combined into sets; because there can be many such sets,
the flag NEW-OPTION-SET-LIMIT controls how many are generated at once.
Each set is given a weighting (see flags WEIGHT-x-COEFFICIENT and 
WEIGHT-x-FN, for x = A,B,C), and the lowest-weighted set is chosen
for searching. If the weight of the lowest-weighted set is too large,
TPS may generate more sets; the interpretation of "too large" is given
by MS91-WEIGHT-LIMIT-RANGE. If the search fails, it will be discarded;
if it runs out of time then it will be re-weighted to be continued
later (see RECONSIDER-FN).@End(Description)

@Section(MS92-9 search procedure)

@Begin(Description)
@IndexOther(MS92-9)@\
Call mating search procedure MS92-9 on the current eproof.  This
procedure uses a naive level-saturation method, exhaustively searching
a single jform before applying any duplications. Quantifier duplications
are applied uniformly to outermost quantifiers. Will try primitive
substitution for outermost variable only.  Works on only a single
jform at a time.
The procedure is almost identical to MS88, except that the flag
NUM-OF-DUPS is used to govern how many times the outermost quantifier
may be duplicated. The internal representation of variables is as in
MS90-3.@End(Description)

@Section(MS93-1 search procedure)

@Begin(Description)
@IndexOther(MS93-1)@\
Begin mating search MS93-1 on the current expansion proof.
The search is basically identical to MS89, but is performed using the 
internal variable representations of MS90-9.
Primitive substitutions and duplications are performed systematically,
with multiple jforms being worked on simultaneously.  On each
particular jform, the search procedure MS92-9 is used.  The flags
MAX-SEARCH-LIMIT, SEARCH-TIME-LIMIT, and RANK-EPROOF-FN are used to
control the search. See also the command SHOW-OPTION-TREE.@End(Description)

@Section(MS98-1 search procedure)

@Begin(Description)
@IndexOther(MS98-1)@\
Begin the MS98-1 mating search. 
See Matt Bishop's thesis for details.

@IndexOther(MS98-DUP)@\
Make NUM-OF-DUPS duplications in the current etree.

@IndexOther(MS98-PRIM)@\
Make all possible primitive substitutions and 
then NUM-OF-DUPS duplications in the current etree.@End(Description)

@Section(Proof Translation)

@Begin(Description)
@IndexOther(MERGE-TREE)@\
If the mating is complete, applies substitutions to the expansion 
tree, then applies Pfenning's MERGE algorithm, eliminating redundant 
expansion terms.@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexOther(CJFORM)@\
Create a new jform for the expansion tree associated with the
current mating-search top-level. You need to use this command only if
you modify the expansion tree interactively and you are constructing a
mating interactively.

@IndexOther(CW) @i{label}@\

@IndexOther(CWD) @i{label}@\

@IndexOther(CWS) @i{label}@\

@IndexOther(NUM-HPATHS)@\
Counts the number of horizontal paths through the given jform.

@IndexOther(NUM-VPATHS)@\
Counts the number of vertical paths through the given jform.

@IndexOther(VP)@\
Use this operation for displaying vertical path diagram on the
terminal with default settings. For complete control over the defaults
use edop VPF.

@IndexOther(VPD)@\
Use this operation for saving VP diagrams in a file. You may want
to change the values of the variables VPD-FILENAME, VPD-STYLE, VPD-PTYPES,
VPD-BRIEF, VPD-VPFPAGE.

@IndexOther(VPETREE)@\
Display the VP diagram of the ETREE as used in mating-search.

@IndexOther(VPT) @i{file}@\
Prints the path diagram, in a format understood by TeX, for a JForm 
or a GWFF. At present, it chops off whatever will not fit on one page.
The following flags affect the output:
    1. VPD-BRIEF controls whether labels or wffs are printed.
    2. VPD-PTYPES controls whether types are printed.
    3. TEXFORMAT controls whether the vertical or horizontal path diagram is
printed.
    4. ALLSCOPEFLAG controls where square brackets are printed.@End(Description)

@Section(Moving Commands)

@Begin(Description)
@IndexOther(0)@\
Move back to previous node, e.g., undo the last L or R
command. Note that 0 stands for the numeral zero.

@IndexOther(D)@\
Move down one node in etree (to leftmost node if more than
one successor).

@IndexOther(FB)@\
Find the topmost binder.

@IndexOther(FI)@\
Find an infix node.

@IndexOther(GOTO) @i{node}@\
Move to a specified node.

@IndexOther(L)@\
For an infix etree node, move to the left argument.

@IndexOther(R)@\
For an infix etree node, move to the right argument.

@IndexOther(UP)@\
Move up one node in etree.

@IndexOther(^)@\
Move upwards to root of expansion tree.@End(Description)

@Section(Statistics)

@Begin(Description)
@IndexOther(DEL-DUP-CONNS)@\
Deletes duplicate connections from a mating. This should be necessary
    only for propositional formulas.

@IndexOther(STATS)@\
Display statistics for the active mating and totals for all
matings in this expansion proof.@End(Description)

@Section(Miscellaneous)

@Begin(Description)
@IndexOther(EXPUNGE)@\
Frees up space by getting rid of all expansion proofs and option 
trees. If you only want to get rid of old expansion proofs and 
option trees, you can use EXPUNGE-OLD to do you job. 
Warning : After using EXPUNGE, many commands such as ETD, VP, ...,
don't work until you re-initialize the current expansion proof by using
commands such as SUB, MATE, ...

@IndexOther(EXPUNGE-OLD)@\
Frees up space by getting rid of all old expansion proofs and 
option trees. If you'd like to get rid of all(not only old) expansion
proofs and option trees, you must use EXPUNGE to do your job.
Warning : Never use EXPUNGE-OLD if you are going to use EXPUNGE, or you
cannot get the expected result!@End(Description)
@ChapterPh(Extensional Expansion Dag Commands)
The internal name of this category is 
EXTMATECMD.
An extensional expansion dag command can be defined using DEFEXTMATE.
Allowable properties are: @t{EXTMATE-ARGTYPES}, @t{EXTMATE-ARGNAMES}, @t{EXTMATE-ARGHELP}, @t{EXTMATE-DEFAULTFNS}, @t{EXTMATE-MAINFNS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
Leave EXT-MATE to the next enclosing top level.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(ETD)@\
Show the current the extensional expansion dag, only printing some shallow formulas

@IndexOther(ETP)@\
Show the current the extensional expansion dag, printing all shallow formulas

@IndexOther(P)@\
Print the current extensional expansion dag node.

@IndexOther(PDEEP)@\
Print the deep formula of an extensional expansion dag node.

@IndexOther(PP)@\
Print an extensional expansion dag with node-names.

@IndexOther(PPDEEP)@\
Pretty-print the deep formula of an extensional expansion dag node.

@IndexOther(PPF)@\
Prints information about the current extensional expansion dag.

@IndexOther(PSH)@\
Print the shallow formula of an extensional expansion dag.

@IndexOther(SHOW-EXP-TERMS)@\
Show expansion terms in expansion dag.

@IndexOther(SHOW-EXP-VARS)@\
Show expansion vars in expansion dag.

@IndexOther(SHOW-MATING)@\
Show the current mating in the extensional expansion dag

@IndexOther(SHOW-SEL-VARS)@\
Show selection vars in expansion dag.@End(Description)

@Section(Extensional Search)

@Begin(Description)
@IndexOther(COMPLETE-P)@\
Indicate if the current extensional expansion dag is complete,
and print an open path if it is not complete.

@IndexOther(MS03-LIFT)@\
Use lifting to guide the search for a proof using diy
with default-ms MS03-7.  If successful, values are suggested for many
relevant flags in the subject MS03-7.

Setting QUERY-USER to T allows the user more control over lifting.

See Also: LIST MS03-7

@IndexOther(MS04-LIFT)@\
Use lifting to guide the search for a proof using diy
with default-ms MS04-2.  If successful, values are suggested for many
relevant flags in the subject MS04-2.

Setting QUERY-USER to T allows the user more control over lifting.

See Also: LIST MS04-2@End(Description)

@Section(Proof Translation)

@Begin(Description)
@IndexOther(ETREE-NAT) @i{prefix} @i{num} @i{tac} @i{mode}@\
Translate a complete edag proof into natural deduction.

@IndexOther(MERGE-TREE)@\
Merge a complete edag.@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexOther(CJFORM) @i{posflex} @i{negflex} @i{flexflex}@\
Create (or update) for the edag.  You can choose to
leave out positive and/or negative flexible literals.
You can also choose to leave out flex/flex equation goals.

@IndexOther(NUM-HPATHS)@\
Print the number of horizontal paths in the jform for the edag.

@IndexOther(NUM-VPATHS)@\
Print the number of vertical paths in the jform for the edag.

@IndexOther(VP)@\
Print the jform for the edag as a VP diagram.

@IndexOther(VPD)@\
Save the jform for the edag as a VP diagram in a file
The variables VPD-FILENAME, VPD-STYLE, VPD-PTYPES, VPD-BRIEF and VPD-VPFPAGE
control this.@End(Description)

@Section(Extensional Expansion Dags)

@Begin(Description)
@IndexOther(ADD-CONN) @i{first} @i{second}@\
Add a connection between two atoms or equations in the edag.

@IndexOther(ADD-CONN*)@\
Repeatedly call add-conn

@IndexOther(DUP-AND-IMITATE) @i{evar} @i{head}@\
Duplicate an expansion var and substitute a general imitation term for the original var.

@IndexOther(DUP-AND-PROJECT) @i{evar} @i{argnum}@\
Duplicate an expansion var and substitute a general projection term for the original var.

@IndexOther(DUP-AND-SUBST-EXISTS) @i{evar} @i{tp}@\
Duplicate an expansion var and substitute a primsub with an existential quantifier for the original var.

@IndexOther(DUP-AND-SUBST-FORALL) @i{evar} @i{tp}@\
Duplicate an expansion var and substitute a primsub with a universal quantifier for the original var.

@IndexOther(DUP-NODE) @i{expnode}@\
Create a new expansion arc from an expansion node in an expansion dag.

@IndexOther(DUP-VAR) @i{evar}@\
Duplicate an expansion var in an expansion dag.

@IndexOther(EXPAND-EXISTS) @i{evar} @i{tp}@\
Given an expansion variable x(A) and a variable y of type B,
let p be the primsub using forall of type B.  For every expansion arc
with expansion term t containing the given expansion variable x, add a
new expansion arc using expansion term [p/x]t.

@IndexOther(EXPAND-FORALL) @i{evar} @i{tp}@\
Given an expansion variable x(A) and a variable y of type B,
let p be the primsub using forall of type B.  For every expansion arc
with expansion term t containing the given expansion variable x, add a
new expansion arc using expansion term [p/x]t.

@IndexOther(EXPAND-IMITATE) @i{evar} @i{head}@\
Given an expansion variable x(A) and a head H (appropriate
for an imitation term of type A), let H' be the general imitation term
for H of type x.  For every expansion arc with expansion term t
containing the given expansion variable x, add a new expansion arc
using expansion term [H'/x]t.

@IndexOther(EXPAND-PROJECT) @i{evar} @i{argnum}@\
Given an expansion variable x(A) and integer i (appropriate
for a projection term of type A), let p be the i'th projection term
for type A.  For every expansion arc with expansion term t containing
the given expansion variable x, add a new expansion arc using
expansion term [p/x]t.

@IndexOther(EXPAND-SUBST) @i{evar} @i{wff}@\
Given an expansion variable x(A) and a wff W(A), for every
expansion arc with expansion term t containing the given expansion
variable x, add a new expansion arc using expansion term [W/x]t.  (The
free variables of W are not considered new expansion variables.)

@IndexOther(IMITATE) @i{evar} @i{head}@\
Substitute a general imitation term for the original var.

@IndexOther(PROJECT) @i{evar} @i{argnum}@\
Substitute a general projection term for the original var.

@IndexOther(REM-CONN) @i{first} @i{second}@\
Remove a connection between two atoms or equations in the edag.

@IndexOther(REM-CONN*)@\
Repeatedly call rem-conn

@IndexOther(SUBST) @i{evar} @i{wff}@\
Substitute a term for an expansion var in an expansion dag.

@IndexOther(SUBST-EXISTS) @i{evar} @i{tp}@\
Substitute a primsub with an existential quantifier for the original var.

@IndexOther(SUBST-FORALL) @i{evar} @i{tp}@\
Substitute a primsub with a universal quantifier for the original var.@End(Description)

@Section(Moving Commands)

@Begin(Description)
@IndexOther(0)@\
Move back to previous node, e.g., undo the last L or R
command. Note that 0 stands for the numeral zero.

@IndexOther(D)@\
Move down one node in extensional expansion dag
(to leftmost node if more than one successor).

@IndexOther(FB)@\
Move down to the first expansion or selection node
(those whose shallow formulas start with a binder).

@IndexOther(FI)@\
Move down to the first infix node.

@IndexOther(GOTO) @i{node}@\
Go to a node in the extensional expansion dag.

@IndexOther(L)@\
For an infix edag node, move to the left argument.

@IndexOther(R)@\
For an infix edag node, move to the right argument.

@IndexOther(UP)@\
Move up one node in the edag.

@IndexOther(^)@\
Move up to the root of the edag.@End(Description)
@ChapterPh(Matingstree Commands)
The internal name of this category is 
MTREEOP.
A matingstree command can be defined using DEFMTREEOP.
Allowable properties are: @t{MTREE-ALIAS}, @t{MTREE-MOVE}, @t{MTREE-PRINT}, @t{MTREE-DEFAULT}, @t{MTREE-ARGS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
leaving the mtree top level.@End(Description)

@Section(Mtree Operations)

@Begin(Description)
@IndexOther(ADD-CONN) @i{literal1} @i{oblig1} @i{literal2} @i{oblig2}@\
Add a connection. The subsumption is considered.
The usage of the command is exactly as the usage of ADD-CONN in MATE.

@IndexOther(CHOOSE-BRANCH)@\
Remove all matingstree branches except the one leading to the 
current matingstree node (which must be a leaf of the matingstree, and must
be complete). This will also do some preliminary merging, by deleting all 
of the jforms which are associated with the deleted nodes.

@IndexOther(COMPLETE-P)@\
Check the completeness of the current mating. The usage of the command is 
exactly the same as the usage of the mate command COMPLETE-P.

@IndexOther(D) @i{node} @i{matingstree}@\
Go down one level. D <nth> means go down along the nth subnode.
Counting begins from 0. Without argument, D means go down along the 
leftmost subnode.

@IndexOther(GOTO) @i{node} @i{matingstree}@\
GOTO <node> means to go to the given node. If <node> is not given,
it means to go to the root of the matingstree

@IndexOther(INIT)@\
Initialize the matingstree. This is done automatically 
when you enter the matingstree top level, but can be used 
subsequently to return everything to the state it was in when 
you first entered the mtree top level.

@IndexOther(KILL) @i{node}@\
KILL <node> means to mark the given node and 
all nodes below it as dead.

@IndexOther(PICK) @i{literal} @i{obligation}@\
Pick a leaf which you may try to mate with another later.
(MB: I think that PICK N behaves as though you had just 
added a connection to N, and generates the appropriate
obligations, without actually demanding another leaf to
connect with. I think.)

@IndexOther(PRUNE)@\
Remove all dead leaves below (but not including) 
the current matingstree.

@IndexOther(REM-NODE)@\
Remove the last connection. The subsumption is considered.
If the node is the root, the whole matingstree is removed. The 
usage of the command is exactly the as the usage of REM-LAST-CONN. 
Please check the help message for REM-LAST-CONN if necessary.

@IndexOther(RESURRECT) @i{node}@\
RESURRECT <node> means to mark the given node and 
all nodes below it as alive.

@IndexOther(SHOW-MATING)@\
Show the connections in the mating associated with the current node.

@IndexOther(SHOW-SUBSTS)@\
Show the substitution stack associated with a matingstree node.

@IndexOther(SIB) @i{matingstree}@\
Go to the next sibling of this node.

@IndexOther(UNIFY)@\
Go into UNIFY toplevel and check the UTREE structure
associated with the current node in the matingstree. The 
unification tree associated with the mating is passed on 
to the unification top-level. Any changes made to this tree 
are destructive. Applicable only for a higher-order unification 
problem. Mainly use to check the UTREE structure.

@IndexOther(UP) @i{matingstree}@\
Go up one level.@End(Description)

@Section(Mtree Printing)

@Begin(Description)
@IndexOther(CONNS-ADDED) @i{name}@\
Print out all of the connections which have already 
been added to the given matingstree node. If no node
is given, the current node is used.

@IndexOther(LIVE-LEAVES) @i{name}@\
Print out all of the live leaves in the tree below 
the given matingstree node. If no node is given, the root 
node is used.

@IndexOther(PM-NODE) @i{name}@\
Print out the given matingstree node in detail. If no 
node is given, the current matingstree is used.

@IndexOther(PMTR) @i{name}@\
Print out the given matingstree as a tree, showing the obligations at each 
node. If no matingstree is given, the current-matingstree is printed out. 

Matingstrees enclosed in curly brackets are marked as dead.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider. See also PMTR*.

@IndexOther(PMTR*) @i{name}@\
Print out the given matingstree as a tree, showing the obligations at each 
node. If no matingstree is given, the current-matingstree is printed out. 

Numbers in round brackets are open obligations. If the brackets end in "..",
there are too many open obligations to fit under the mstree label. 

Leaves underlined with ^'s are closed matingstrees. 
Matingstrees enclosed in curly brackets are marked as dead.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider. See also PMTR.

@IndexOther(PMTR-FLAT) @i{name}@\
Print out the given matingstree in a flat format. 
If no matingstree is given, the current matingstree is printed out.

@IndexOther(POB) @i{name}@\
Print out the vpform associated with the given obligation node.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.

@IndexOther(POB-LITS) @i{name}@\
Print out the unblocked literals in a given obligation tree.
If no argument is given, the current-obligation tree is the default.

@IndexOther(POB-NODE) @i{name}@\
Print out the given obligation in detail. If no 
obligation is given, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.

@IndexOther(POTR) @i{name}@\
Print out the given obligation tree as a tree. If no obligation is given,
the tree below the current obligation is printed out. 

Numbers in round brackets are open obligations; those in square brackets are
closed. 
Branches with *'s denote nodes that are being omitted for lack of space.
The cure for this is to either start printing from a node lower in the tree,
or make the screen wider.

@IndexOther(POTR*-FLAT) @i{name}@\
Print out the given obligation tree in flat form, 
with the jforms attached to all nodes. If no argument is given,
the whole obligation tree is printed out.

@IndexOther(POTR-FLAT) @i{name}@\
Print out the given obligation tree in flat form, 
with the jforms attached to the leaves. If no argument is 
given, the current-obligation tree is printed out.

@IndexOther(PPATH) @i{name}@\
Print out the path containing the given obligation.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.

@IndexOther(PPATH*) @i{name}@\
Print out the path containing the given obligation,
and show all of the obligations on this path.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.@End(Description)

@Section(Mtree Auto)

@Begin(Description)
@IndexOther(ADD-ALL-LIT) @i{literal} @i{obligation}@\
Attempt to mate a literal with all potential mates on 
the current path.

@IndexOther(ADD-ALL-OB) @i{obligation}@\
Attempt to mate all literals in an obligation 
with all potential mates on the current path.

@IndexOther(EXPAND-LEAVES) @i{mtree}@\
Apply ADD-ALL-OB to all live leaves of the current matingstree
that lie below the given node (or the current node, if no node is given).
WARNING: Potential combinatorial explosion!

@IndexOther(GO)@\
Call the matingstree procedure given in DEFAULT-MS.

@IndexOther(MT94-11) @i{mtree}@\
Apply EXPAND-LEAVES repeatedly to all live leaves of the current 
matingstree that lie below the given node (or the current node, if 
no node is given), until a closed leaf is generated.
WARNING: Potential combinatorial explosion!

@IndexOther(MT94-12) @i{mtree}@\
Least Branching Search: In each leaf node, take the current
obligation and find a literal that can be mated, but with as few 
mates as possible. Add all of these mates as sons to this node.
Repeat until a closed leaf is generated.
This search is probably not complete.

@IndexOther(MT95-1) @i{mtree}@\
Fewest Obligations Search: Choose the matingstree node (from the 
entire tree, not just the tree below the current node) with the 
fewest open obligations. Go to that node and do one step of MT94-12
(i.e. choose the literal with the fewest number of mates, and generate
all of the associated branches of the mtree).
Repeat until a closed leaf is generated.
This search is probably not complete.

@IndexOther(QRY) @i{literal} @i{obligation}@\
Output a list of literals which can be mated with a given literal.@End(Description)
@ChapterPh(Unification Commands)
The internal name of this category is 
UNIFOP.
An unification command can be defined using DEFUNIFOP.
Allowable properties are: @t{UNIF-ARGTYPES}, @t{UNIF-ARGNAMES}, @t{UNIF-ARGHELP}, @t{UNIF-DEFAULTFNS}, @t{UNIF-APPLICABLEP}, @t{UNIF-MAINFNS}, @t{PRINT-COMMAND}, @t{MOVE-COMMAND}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
Exit unification.@End(Description)

@Section(Unification)

@Begin(Description)
@IndexOther(0)@\
Replace the current topnode with the node on top of the nodestack.
Generally, typing an integer n will go to the nth son of the current
node. Compare the command NTH-SON.

@IndexOther(APPLY-SUBST) @i{var} @i{term}@\
Apply a substitution, suggested by the user, to the current topnode.
Modifies the unification tree.

@IndexOther(EPROOF-UTREE)@\
Create a new utree whose root has all the dpairs
associated with the current mating. (The existing utree may
have some of the dpairs added lower down the tree; this will
bring them all to the top). See also NAME-DPAIR.

@IndexOther(GO)@\
Call unification in automatic mode. Will search for unifiers
only below the current-topnode.

@IndexOther(GOTO) @i{name}@\
Go to the specified node in the unification tree.

@IndexOther(MATCH)@\
This command is applicable only if current-topnode is a non-terminal
leaf node. Calls TPS's version of Huet's MATCH algorithm to find substitutions
at the current topnode. The pair selected by MATCH is determined by the value 
of the flag APPLY-MATCH.

@IndexOther(MATCH-PAIR) @i{n}@\
This command is applicable only if current-topnode is a non-terminal
leaf node. Calls TPS's version of Huet's MATCH algorithm to find 
substitutions at the current topnode. n refers to the nth dpair, 
and this must be a flexible-rigid dpair.

@IndexOther(NAME-DPAIR) @i{name}@\
Give a name to the dpairset associated with the current topnode.
This is most useful when UNIFY has been issued from the MATE top
level, and you want to name the current dpair so that you can save
it in the library. See also EPROOF-UTREE.

@IndexOther(NTH-SON) @i{n}@\
Go to the nth descendant of the current-topnode. Instead of
using this command, you can simply type n on the unification top level to
go to the nth descendant. It has no effect if the current-topnode has
no descendents.

@IndexOther(P) @i{name}@\
Displays the current unification node; show its name,
measure, number of descendants, substitutions added and free
variables. Does not display the disagreement pairs (use PP or
PP* for that), or the cumulative substitutions from this node 
to the root (use SUBST-STACK for that).

@IndexOther(PALL) @i{name} @i{filename} @i{verbose}@\
Displays all the disagreement pairs at every node below the
given node. (Similar to PP, but for the entire tree below the current
node.)

@IndexOther(PP)@\
Displays the disagreement pairs at the current node. See also PP*. 
More information about the current node is given by the command P.

@IndexOther(PP*)@\
Displays the disagreement pairs at the current-topnode, including the order of
each pair and other information. See also PP. The other information displayed
includes (for each wff, each disagreement pair and the whole set of 
disagreement pairs):
1) the order (e.g. "x(i)" is first order, and so on).
2) whether it is monadic (all function constants are unary).
3) whether it is linear (all free vars occur once only).
4) whether it is a matching problem (one side of a dpair has no free vars).
5) whether it is a relaxed pattern (all free vars have only bound vars as
   arguments).
6) whether it is a pattern (all free vars have distinct bound vars as 
   arguments).
7) whether a disagreement pair is variable-disjoint (the free vars on the
   left are disjoint from those on the right).
8) whether the set of disagreement pairs can be partitioned into sets in 
   which each free var in the whole problem occurs in at most one set.
9) whether there are any free vars that occur only once, or not at all, in 
   the whole problem.
These conditions all appear in the literature on higher-order unification; 
see, for example, Prehofer's paper in CADE '94.

More information about the current node is given by the command P.

@IndexOther(SIMPLIFY)@\
A call to TPS's version of Huet's SIMPL algorithm. Dpairs in the 
current topnode are replaced by the dpairs returned by the call. It will 
also find substitutions of the form (var . term) provided `var' does not
occur in `term'. This command will alter the unification tree.

@IndexOther(STATS)@\
Statistics about the current unification tree.

@IndexOther(SUBST-STACK) @i{filename}@\
Displays the substitution stack for the current topnode.
See also P, PP, PP* for other information about the current node.

@IndexOther(UTREE) @i{name} @i{filename} @i{verbose}@\
Displays the unification tree and the associated substitutions at
each node which is below the specified node. Display is in a flat
format; UTREE* prints the same information in a tree format.

@IndexOther(UTREE*) @i{name} @i{print-subs}@\
Displays the unification tree and the associated substitutions at
each node which is below the specified node. Display is in a tree
format; UTREE prints the same information in a flat format. Display 
shows nodes as numbers, followed by I for imitation, P for projection,
~ for negation, A for administrative (e.g. anything generated by SIMPL).
Optionally shows the most recent substitution on the subst-stack at 
each node.

@IndexOther(^)@\
Go to the parent node of the current-topnode.
(i.e. move up one level in the tree).

@IndexOther(^^)@\
Go to the root node in the unification tree 
(the node with name "0").@End(Description)

@Section(Dpairs)

@Begin(Description)
@IndexOther(ADD-DPAIR) @i{name} @i{elt1} @i{elt2}@\
If the disagreement set already exists, insert a disagreement pair at
the front. Else create a new disagreement set consisting of this dpair only.

@IndexOther(ADD-DPAIRS-TO-NODE) @i{name} @i{free-vars}@\
Add new dpairs to the disagreement set at the CURRENT-TOPNODE. Applicable 
only if CURRENT-TOPNODE is a non failure leaf node. `Name', the first argument 
to this command must already represent a disagreement set. Use the command 
ADD-DPAIR,etc., to create this set.

@IndexOther(ADD-DPAIRS-TO-UTREE) @i{name} @i{free-vars}@\
Add new dpairs at all non failure leaf nodes.

@IndexOther(FIND-NESTING)@\
Find the values for MAX-SUBSTS-* implied by
the current node.

@IndexOther(PRUNE)@\
Prune all the branches which have either reached the maximum
allowed depth, or which end only in failure nodes.

@IndexOther(RM-DPAIR) @i{name} @i{elt1} @i{elt2}@\
Remove a disagreement pair from a disagreement set.

@IndexOther(SHOW-DPAIRSET) @i{name}@\
Show a disagreement set.

@IndexOther(UNIF-PROBLEM) @i{name} @i{free-vars}@\
Set up a new unification problem. `Name', the first argument to this 
command must already represent a disagreement set. Use the command ADD-DPAIR
to create this set. This is in some ways the inverse of the NAME-DPAIR 
command.@End(Description)
@ChapterPh(Test-Top Commands)
The internal name of this category is 
TESTCMD.
A test-top command can be defined using DEFTEST.
Allowable properties are: @t{TEST-ARGTYPES}, @t{TEST-ARGNAMES}, @t{TEST-ARGHELP}, @t{TEST-DEFAULTFNS}, @t{TEST-MAINFNS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
Leave TEST-TOP to the next enclosing top level.@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexOther(BREADTH-FIRST-SEARCH) @i{modename} @i{testwin}@\
Equivalent to setting TEST-NEXT-SEARCH-FN to BREADTH-FIRST-SEARCH
and then typing GO. Permanently changes TEST-NEXT-SEARCH-FN.

@IndexOther(CLOSE-TESTWIN)@\
Closes the window that displays the test-top summary.
Use ..../tps/utilities/vpshow (from a shell, not from TPS) 
to view the output file again.

@IndexOther(CONTINUE) @i{modename} @i{testwin}@\
Continue searching with current searchlist & current problem
(similar to GO, but will continue from the last point reached rather
than restarting at the beginning again).

@IndexOther(EXHAUSTIVE-SEARCH) @i{modename} @i{testwin}@\
Equivalent to setting TEST-NEXT-SEARCH-FN to EXHAUSTIVE-SEARCH
and then typing GO. Permanently changes TEST-NEXT-SEARCH-FN.

@IndexOther(FIND-BEST-MODE) @i{modename} @i{testwin}@\
This command effectively runs PUSH-UP until it finds
a mode that works, and then runs PRESS-DOWN until it finds the
best mode it can.
Before using this command, use the MODE command to 
set up a mode in which the current theorem can not be proven. Also
check the value of the TEST-INCREASE-TIME flag (it should probably
not be zero).
Then PUSH-UP will systematically vary the values of the flags
listed in the TEST-EASIER-IF-* flags, using the PUSH-UP search
function (see the help message for TEST-NEXT-SEARCH-FN).
Once a correct mode is discovered, it will systematically vary
the values of the flags listed in the TEST-FASTER-IF-* flags,
using the PRESS-DOWN search function, until it finds as good a 
mode as it can.
The values of TEST-REDUCE-TIME, TEST-NEXT-SEARCH-FN, 
TEST-INCREASE-TIME and TEST-FIX-UNIF-DEPTHS will be permanently
changed.

@IndexOther(GO) @i{modename} @i{testwin}@\
Start searching with current searchlist & current problem.

@IndexOther(OPEN-TESTWIN) @i{filename}@\
Open a window which will display a summary of the test-top output.
The window can be closed with the command CLOSE-TESTWIN. The size 
of the text is determined by the flag CHARSIZE, and the current width of the window 
by the flag TESTWIN-WIDTH. The initial height of the window is determined by 
TESTWIN-HEIGHT.

@IndexOther(PRESS-DOWN) @i{modename} @i{testwin}@\
Before using this command, use the MODE command to 
set up a mode in which the current theorem can be proven. Also
check the value of the TEST-INITIAL-TIME-LIMIT flag (it should
be high enough that the first attempt at proof will succeed).
Then PRESS-DOWN will systematically vary the values of the flags
listed in the TEST-FASTER-IF-* flags, using the PRESS-DOWN search
function (see the help message for TEST-NEXT-SEARCH-FN).
The values of TEST-REDUCE-TIME, TEST-NEXT-SEARCH-FN and 
TEST-FIX-UNIF-DEPTHS will be permanently changed (to T, PRESS-DOWN
and T respectively).

Note that this is NOT the same as PRESS-DOWN-2, since it automatically
generates a searchlist rather than relying on the user to provide one.

@IndexOther(PRESS-DOWN-2) @i{modename} @i{testwin}@\
Equivalent to setting TEST-NEXT-SEARCH-FN to PRESS-DOWN-2
and then typing GO. Permanently changes TEST-NEXT-SEARCH-FN.
Note that this is NOT the same as typing PRESS-DOWN; this will
use the user-defined searchlist rather than an automatically
generated one.

@IndexOther(PUSH-UP) @i{modename} @i{testwin}@\
This command effectively runs PUSH-UP until it finds
a mode that works, and then stops.
Before using this command, use the MODE command to 
set up a mode in which the current theorem can not be proven. Also
check the value of the TEST-INCREASE-TIME flag (it should probably
not be zero).
Then PUSH-UP will systematically vary the values of the flags
listed in the TEST-EASIER-IF-* flags, using the PUSH-UP search
function (see the help message for TEST-NEXT-SEARCH-FN).
The value of TEST-NEXT-SEARCH-FN will be changed to PUSH-UP.

Note that this is NOT the same as PUSH-UP-2, since it automatically
generates a searchlist rather than relying on the user to provide one

@IndexOther(PUSH-UP-2) @i{modename} @i{testwin}@\
Equivalent to setting TEST-NEXT-SEARCH-FN to PUSH-UP-2
and then typing GO. Permanently changes TEST-NEXT-SEARCH-FN.
Note that this is NOT the same as typing PUSH-UP; this will
use the user-defined searchlist rather than an automatically
generated one.

@IndexOther(SEARCH-ORDER) @i{name}@\
Show the order in which things will be changed if the search is
started now using the given searchlist.@End(Description)

@Section(Searchlists)

@Begin(Description)
@IndexOther(ADD-FLAG) @i{flag} @i{init} @i{range}@\
Add a single flag to the current searchlist.
To change the current searchlist, use NEW-SEARCHLIST.

@IndexOther(ADD-FLAG*)@\
Repeatedly add new flags to the current searchlist.

@IndexOther(ADD-FUNCTION) @i{name}@\
Add a function to a searchlist. This function will be evaluated on 
every iteration of the search, and will generally reset certain flags.
The special functions defined so far are:
UNIFORM-SEARCH-FUNCTION sets max-utree-depth, max-search-limit and max-substs-quick
using the values of max-search-depth, search-time-limit and max-substs-var respectively,
and then sets TEST-INITIAL-TIME-LIMIT to allow 5 option sets on the first try, then
10, then 15, and so on.
BASIC-SEARCH-THEN-UNIFORM-SEARCH runs the current searchlist once over, allowing
1 hour for each setting of the flags. Then it switches the searchlist to
UNIFORM-SEARCH-2 and continues with that.

@IndexOther(ADD-SUBJECTS) @i{subjects}@\
Add all the flags concerning the given subjects to the current 
searchlist.

@IndexOther(NEW-SEARCHLIST) @i{name}@\
Make a new searchlist; i.e. begin a new list of flags to be varied.
This command also changes the current searchlist.

@IndexOther(QUICK-DEFINE) @i{name} @i{succ}@\
Define a searchlist the quick and dirty way!
If the current flag settings are OK (i.e. are a successful mode),
will create a searchlist in which the flags given in the values of 
the TEST-FASTER-* flags (do LIST TEST-TOP for a listing) vary over 
values which ought to give a faster search than the current values.
If the current flag settings are not OK, will create a searchlist in 
which the flags given in the values of the TEST-EASIER-* flags vary
over values which ought to make the search easier than the current 
values.
The maximum number of values for any flag to take is governed 
by TEST-MAX-SEARCH-VALUES.

@IndexOther(REM-FLAG) @i{flag}@\
Remove a single flag from the current searchlist.
To change the current searchlist, use NEW-SEARCHLIST.

@IndexOther(REM-FLAG*)@\
Repeatedly remove flags from the current searchlist.

@IndexOther(REVISE-DEFAULTS) @i{old-slist} @i{new-slist}@\
For each flag in the given searchlist, change the default setting to 
the current value of the flag, and put the default setting into the range
(unless it's already there). This is useful in conjunction with SCALE-UP 
and SCALE-DOWN; you can keep one searchlist (let's call it MASTER-SLIST) 
containing all of the flags you're likely to want to vary. Then if the 
current flag settings are a good mode and you want to try and find a better 
one, do REVISE-DEFAULTS followed by SCALE-DOWN MASTER-SLIST; if the current
settings are a bad mode and you want to try to find one that works, do 
REVISE-DEFAULTS followed by SCALE-UP MASTER-SLIST.

@IndexOther(SCALE-DOWN) @i{old-slist} @i{new-slist}@\
Rewrites a searchlist under the assumption that the initial
values in the searchlist (together with appropriate settings of the other
flags) constitute a successful mode, and that TEST is being run in order 
to find a faster mode. This will discard all settings that would make the
search slower, and will arrange the range of values in such a way that the
bounds of the search will gradually decrease until the proof cannot be 
completed. If this makes the range empty or a singleton, the flag is removed
from the searchlist. See the TEST-FASTER-* flags

@IndexOther(SCALE-UP) @i{old-slist} @i{new-slist}@\
Rewrites a searchlist under the assumption that the initial
values in the searchlist (together with appropriate settings of the other
flags) do not constitute a successful mode, and that TEST is being run in order
to find a mode that works. This will discard all settings that would make the 
search harder, and will arrange the range of values in such a way that the 
bounds of the search will gradually increase until the proof (with a bit of 
luck) can be completed. If this makes the range empty or a singleton, the flag 
is removed from the searchlist. See the TEST-EASIER-* flags.

@IndexOther(SEARCHLISTS)@\
Print a list of all searchlists currently in memory.

@IndexOther(SHOW-SEARCHLIST) @i{name}@\
Show contents of a searchlist.

@IndexOther(VARY-MODE) @i{modename} @i{slistname} @i{use-mode}@\
Go through an existing mode, flag by flag, creating a searchlist
by picking out relevant flags from it. All useless flags (i.e. ones that 
cannot affect the search time) will be automatically stripped out. The
default flag value in the searchlist will be its value in the mode.
You can also optionally set the current flag values to the values 
in the mode (equivalent to the MODE command).@End(Description)

@Section(Library)

@Begin(Description)
@IndexOther(DELETE) @i{name} @i{type}@\
Delete a saved searchlist or mode (equivalent to the library
command DELETE.

@IndexOther(FETCH) @i{name} @i{type}@\
Retrieve a searchlist or mode from the library. Exactly like the 
library function FETCH, except that when a searchlist is retrieved, it
will become the current searchlist.

@IndexOther(INSERT) @i{name} @i{type} @i{comment}@\
Like the library command INSERT; will save a 
searchlist in the library. Will also save a mode that has been found 
by using GO.@End(Description)
@ChapterPh(Models Commands)
The internal name of this category is 
MODELSCMD.
A models command can be defined using DEFMODELS.
Allowable properties are: @t{MODELS-ARGTYPES}, @t{MODELS-ARGNAMES}, @t{MODELS-ARGHELP}, @t{MODELS-DEFAULTFNS}, @t{MODELS-MAINFNS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
Leave MODELS to the next enclosing top level.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(PELT) @i{tp} @i{elt}@\
Print the integer in notation appropriate to the given type.
For example, elements of type (OA) are printed in set notation.
The empty set is called EMPTY and the universal set is called FULL.

Constant functions are denoted by Kc.

A few special cases are T and F at type O, NOT at type (OO),
the binary connectives AND, OR, IMPLIES, EQUIV and XOR at type (OOO),
PI and SIGMA at types of the form (O(OA)),
= at types of the form (OAA) and 
ID at types of the form (AA).

EMPTY at a type (OA) corresponds to the empty set.

FULL at a type (OA) corresponds to the set of all elements of type A.

PI at a type (O(OA)) corresponds to the singleton {FULL} where FULL 
corresponds to the set of all elements of type A.

SIGMA at a type (O(OA)) corresponds to the set containing all sets of
type A except EMPTY.

For elements of low types the command PELT-REC may also be helpful.

SEE ALSO: PELT-REC

@IndexOther(PELT-REC) @i{tp} @i{elt}@\
Print the integer in notation appropriate to the given type.
For example, elements of type (OA) are printed in set notation.
The empty set is called EMPTY and the universal set is called FULL.

Constant functions are denoted by K(c).

A few special cases are T and F at type O, NOT at type (OO), 
the binary connectives AND, OR, IMPLIES, EQUIV and XOR at type (OOO),
PI and SIGMA at types of the form (O(OA)),
= at types of the form (OAA) and 
ID at types of the form (AA).

EMPTY at a type (OA) corresponds to the empty set.

FULL at a type (OA) corresponds to the set of all elements of type A.

PI at a type (O(OA)) corresponds to the singleton {FULL} where FULL 
corresponds to the set of all elements of type A.

SIGMA at a type (O(OA)) corresponds to the set containing all sets of
type A except EMPTY.

This command is recursive.  For low types this is helpful, but
the notation becomes unwieldy for higher types.  For higher types
the command PELT is more appropriate.

SEE ALSO: PELT

@IndexOther(PELTS) @i{tp}@\
Print all the elements of the given type as both integers
and the notation of PELT.

SEE ALSO: PELT

@IndexOther(PELTS-REC) @i{tp}@\
Print all the elements of the given type as both integers
and the notation of PELT-REC.

SEE ALSO: PELT-REC

@IndexOther(PSIZE) @i{tp}@\
Print the size of the domain of the given type.
The elements of the type are 0, . . ., n-1 where n is the size.

@IndexOther(SHOW-ASSIGNMENTS)@\
Show all currently assigned values.  To see the value of any
particular variable, use INTERPRET.  To assign a value or remove an
assignment, use ASSIGN-VAR or UNASSIGN-VAR.

SEE ALSO: ASSIGN-VAR, UNASSIGN-VAR, REMOVE-ALL-ASSIGNMENTS, INTERPRET@End(Description)

@Section(Models)

@Begin(Description)
@IndexOther(ASSIGN-VAR) @i{v}@\
Assign a value to a variable in the current model.

SEE ALSO: REMOVE-ALL-ASSIGNMENTS, UNASSIGN-VAR, INTERPRET, SHOW-ASSIGNMENTS

@IndexOther(CHANGE-BASE-TYPE) @i{basetp} @i{num}@\
Change the number of elements in a base type.
This must be a power of 2.

@IndexOther(COND-PROBABILITY) @i{wff1} @i{wff2}@\
Computes the conditional probability that a wff2 is true if a wff1 is
true in the model.  Assigned variables are considered fixed.  All
unassigned variables are allowed to vary over the appropriate domains.
The probability is the number of values for these unassigned variables
for which wff1 and wff2 are true over the number of values for which
wff1 is true.

SEE ALSO: PROBABILITY, INTERPRET, MAX-BINDER-COMPUTATION, MAX-DOMAIN-SIZE

@IndexOther(INTERPRET) @i{wff}@\
Interpret a formula in the current model.  The evaluation is lazy so
if a function is constant, the argument is not evaluated.  The flags
MAX-BINDER-COMPUTATION and MAX-DOMAIN-SIZE bound how complicated the
wff can be before interpret will fail.

SEE ALSO: ASSIGN-VAR, SHOW-ASSIGNMENTS, REMOVE-ALL-ASSIGNMENTS, UNASSIGN-VAR,
  MAX-BINDER-COMPUTATION, MAX-DOMAIN-SIZE

@IndexOther(PROBABILITY) @i{wff}@\
Computes the probability that a formula is true in the model.
Assigned variables are considered fixed.  All unassigned variables are
allowed to vary over the appropriate domains.  The probability is the
number of values for these unassigned variables for which the wff is true
over the total number of values for the unassigned variables.

SEE ALSO: COND-PROBABILITY, INTERPRET, MAX-BINDER-COMPUTATION, MAX-DOMAIN-SIZE

@IndexOther(REMOVE-ALL-ASSIGNMENTS)@\
Remove all assignments for variables in the current model.

SEE ALSO: UNASSIGN-VAR, ASSIGN-VAR, INTERPRET, SHOW-ASSIGNMENTS

@IndexOther(SOLVE) @i{invars} @i{outvars} @i{wff}@\
Solve for values for the output variables for any values of the input
variables so that the given proposition is true.

If the domains involved are large, TPS will ask the user whether to
print the values to the screen or save them to a file.

TPS will always tell the user whether there are no solutions for any
inputs, solutions for some but not all inputs, solutions for all
inputs and whether there are unique solutions for some inputs.

@IndexOther(UNASSIGN-VAR) @i{v}@\
Remove an assignment for a variable in the current model.

SEE ALSO: REMOVE-ALL-ASSIGNMENTS, ASSIGN-VAR, INTERPRET, SHOW-ASSIGNMENTS@End(Description)
@ChapterPh(Editor Commands)
The internal name of this category is 
EDOP.
An editor command can be defined using DEFEDOP.
Allowable properties are: @t{ALIAS}, @t{RESULT->}, @t{EDWFF-ARGNAME}, @t{DEFAULTFNS}, @t{MOVE-FN}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexEdop(LEAVE)@\
Exit the editor with all the changes in place.

@IndexEdop(NOOP)@\
Do nothing.

@IndexEdop(OK)@\
Exit the editor with all the changes in place.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexEdop(P)@\
Print a wff using the global settings of all flags.

@IndexEdop(PP)@\
Pretty-print a wff.

@IndexEdop(PS)@\
Print a wff showing all brackets and dots.

@IndexEdop(PT)@\
Print a wff showing types.@End(Description)

@Section(Weak Labels)

@Begin(Description)
@IndexEdop(CW) @i{label}@\
Assigns a label to the edwff, but does not change the edwff. You can
use the label to refer to this wff later.

@IndexEdop(DELWEAK) @i{label}@\
Replaces all occurrences of the label with the wff it represents
in the current wff.

@IndexEdop(DW)@\
Replace a top level occurrence of the label by the wff it represents.

@IndexEdop(DW*)@\
Replace all labels in a wff by the wffs represented by them.

@IndexEdop(NAME) @i{label}@\
Assign a label to the edwff, and replace the edwff with this label.

@IndexEdop(RW) @i{label}@\
Makes current edwff the new value of label (which must 
already exist).@End(Description)

@Section(Saving Wffs)

@Begin(Description)
@IndexEdop(SAVE) @i{label}@\
Save a wff by appending it to the file SAVEDWFFS. The 
weak label name should not already exist (if it does, remove it
using RW). The wffs that are saved to this file can be reloaded
using the command QLOAD "savedwffs.lisp".
  This command dates from before the LIBRARY top level was 
introduced; you should probably avoid it. If you want to save 
a gwff, use CW to create a weak label, then go into the library
with LIB and use INSERT to save the wff.@End(Description)

@Section(Recording)

@Begin(Description)
@IndexEdop(O)@\
Invert PRINTEDTFLAG, that is switch automatic recording of wffs
in a file either on or off.  When switching on, the current wff will be
written to the PRINTEDTFILE. Notice that the resulting file will be in 
Scribe format; if you want something you can reload into TPS, then use
the SAVE command.

@IndexEdop(REM) @i{rm}@\
Write a remark into the PRINTEDTFILE.@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexEdop(CJFORM)@\
Converts the given GWFF to JFORM.

@IndexEdop(DJFORM)@\
Converts the given JFORM to GWFF. May not work with skolemized jforms.

@IndexEdop(NUM-HPATHS)@\
Counts the number of horizontal paths through the given jform.

@IndexEdop(NUM-VPATHS)@\
Counts the number of vertical paths through the given jform.

@IndexEdop(PJ)@\
Prints the given gwff, using lists for jforms.

@IndexEdop(PROP-CJFORM)@\
Converts the given GWFF to JFORM.

@IndexEdop(VP)@\
Prints a vertical path diagram. This is like VP in the MATE 
top level, but will use the current edwff to create a jform 
if none is currently available.

@IndexEdop(VPD)@\
Use this operation for saving VP diagrams in a file. You may want
to change the values of the variables VPD-FILENAME, VPD-STYLE, VPD-PTYPES,
VPD-BRIEF, VPD-VPFPAGE.

@IndexEdop(VPF) @i{file} @i{style} @i{ptypes} @i{brief} @i{vpfpage} @i{comment}@\
Prints the vertical path diagram for a JForm or a GWFF.

@IndexEdop(VPT) @i{file}@\
Prints the path diagram, in a format understood by TeX, for a JForm 
or a GWFF. At present, it chops off whatever will not fit on one page.
The following flags affect the output:
    1. VPD-BRIEF controls whether labels or wffs are printed.
    2. VPD-PTYPES controls whether types are printed.
    3. TEXFORMAT controls whether the vertical or horizontal path diagram is
printed.
    4. ALLSCOPEFLAG controls where square brackets are printed.@End(Description)

@Section(Moving Commands)

@Begin(Description)
@IndexEdop(0)@\
Move up one-level, i.e., undo the last L, R, D,
or A command. Note that 0 stands for the numeral zero.

@IndexEdop(A)@\
for an expression like @wt{P x y}, delete the rightmost element;
in this example the result will be to make @wt{Px} the current expression.
For a quantified expression, it will move to the quantified variable.

@IndexEdop(D)@\
for an expression like @wt{P x y}, move to the rightmost element;
in this example @wt{y}.  For a quantified expression it will move
to the scope of the quantifier.

@IndexEdop(FB)@\
Find the first binder (left to right)

@IndexEdop(FI)@\
Find an infix operator.

@IndexEdop(L)@\
for an infix-operator, move to the left argument.

@IndexEdop(R)@\
for an infix-operator, move to the right argument.

@IndexEdop(UNDO)@\
Moves up (like 0), but throws away any editing since your last
downward moving command (typically A,D,L,or R.

@IndexEdop(XTR)@\
Makes the current edwff the top wff.

@IndexEdop(^)@\
Move upwards through enclosing wffs all the way to the top.@End(Description)

@Section(Changing Commands)

@Begin(Description)
@IndexEdop(ASRB)@\
Apply the following laws to a wff:
   A and (A or B), (A or B) and A --> A or B
   A and (B or A), (B or A) and A --> B or A
   A or (A and B), (A and B) or A --> A
   (B and A) or A, (B and A) or A --> A.

@IndexEdop(ASSL)@\
Apply the left associative law:
   A op (B op C) --> (A op B) op C.

@IndexEdop(ASSR)@\
Apply the right associative law:
   (A op B) op C --> A op (B op C).

@IndexEdop(CMRG)@\
Delete the truth constants from a wff:
   A and TRUTH, TRUTH and A --> A
   A and FALSEHOOD, FALSEHOOD and A --> FALSEHOOD
   A or TRUTH, TRUTH or A --> TRUTH
   A or FALSEHOOD, FALSEHOOD or A --> A
   A implies TRUTH --> TRUTH
   TRUTH implies A --> A
   A implies FALSEHOOD --> not A
   FALSEHOOD implies A --> TRUTH
   A equiv TRUTH, TRUTH equiv A --> A
   A equiv FALSEHOOD, FALSEHOOD equiv A --> not A
   not TRUTH --> FALSEHOOD
   not FALSEHOOD --> TRUTH.

@IndexEdop(CMUT)@\
Apply the commutative laws to a formula:
   A and B --> B and A
   A or B --> B or A
   A implies B --> not B implies not A
   A equiv B --> B equiv A.

@IndexEdop(CNTOP) @i{conn}@\
Change the top connective of a formula. For example,
"cntop or" will change "A and B" into "A or B";
"cntop exists" will change "forall x P x" into "exists x P x".

@IndexEdop(DIST-CTR)@\
Apply the distributivity laws to a wff in 
   the contracting direction:
  (A and B) or (A and C) --> A and (B or C)
  (A or B) and (A or C) --> A or (B and C)
  (B and A) or (C and A) --> (B or C) and A 
  (B or A) and (C or A) --> (B and C) or A.

@IndexEdop(DIST-EXP)@\
Apply the distributivity laws to a wff in 
   the expanding direction:
   A and (B or C) --> (A and B) or (A and C)
   A or (B and C) --> (A or B) and (A or C)
   (B or C) and A --> (B and A) or (C and A)
   (B and C) or A --> (B or A) and (C or A).

@IndexEdop(DL)@\
Delete the topmost binary connective and its left scope

@IndexEdop(DNEG)@\
Remove a double negation:
   not not A --> A.

@IndexEdop(DR)@\
Delete the topmost binary connective and its right scope

@IndexEdop(MRG)@\
Apply the following laws to a wff:
   A and A --> A
   A or A --> A
   A implies A --> TRUTH
   A and not A, not A and A --> FALSEHOOD
   A or not A, not A or A --> TRUTH
   A implies not A --> not A
   not A implies A --> A
   A equiv not A, not A equiv A --> FALSEHOOD.

@IndexEdop(PMUT)@\
Permute the two components of an infix operator:
   A op B --> B op A

@IndexEdop(SUBEQ)@\
Apply the following law to a formula:
  A equiv B --> (A implies B) and (B implies A).

@IndexEdop(SUBIM)@\
Apply the following law to a formula:
   A implies B --> not A or B.@End(Description)

@Section(Recursively Changing Commands)

@Begin(Description)
@IndexEdop(ASRB*)@\
Recursively apply the following laws to a wff:
   A and (A or B), (A or B) and A --> A or B
   A and (B or A), (B or A) and A --> B or A
   A or (A and B), (A and B) or A --> A
   (B and A) or A, (B and A) or A --> A.

@IndexEdop(ASSL*)@\
Recursively apply the left associative law:
   A op (B op C) --> (A op B) op C.

@IndexEdop(ASSR*)@\
Recursively apply the right associative law:
   (A op B) op C --> A op (B op C).

@IndexEdop(CMRG*)@\
Recursively delete the truth constants in a wff:
   A and TRUTH, TRUTH and A --> A
   A and FALSEHOOD, FALSEHOOD and A --> FALSEHOOD
   A or TRUTH, TRUTH or A --> TRUTH
   A or FALSEHOOD, FALSEHOOD or A --> A
   A implies TRUTH --> TRUTH
   TRUTH implies A --> A
   A implies FALSEHOOD --> not A
   FALSEHOOD implies A --> TRUTH
   A equiv TRUTH, TRUTH equiv A --> A
   A equiv FALSEHOOD, FALSEHOOD equiv A --> not A
   not TRUTH --> FALSEHOOD
   not FALSEHOOD --> TRUTH.

@IndexEdop(CMUT*)@\
Recursively apply the commutative laws to a formula:
   A and B --> B and A
   A or B --> B or A
   A implies B --> not B implies not A
   A equiv B --> B equiv A.

@IndexEdop(DIST-CTR*)@\
Recursively apply the distributive laws to a wff in 
   the contracting direction:
  (A and B) or (A and C) --> A and (B or C)
  (A or B) and (A or C) --> A or (B and C)
  (B and A) or (C and A) --> (B or C) and A 
  (B or A) and (C or A) --> (B and C) or A.

@IndexEdop(DIST-EXP*)@\
Recursively apply the distributive laws to a wff in 
   the expanding direction:
   A and (B or C) --> (A and B) or (A and C)
   A or (B and C) --> (A or B) and (A or C)
   (B or C) and A --> (B and A) or (C and A)
   (B and C) or A --> (B or A) and (C or A).

@IndexEdop(DNEG*)@\
Recursively remove double negations:
   not not A --> A.

@IndexEdop(MRG*)@\
Recursively apply the following laws to a wff:
   A and A --> A
   A or A --> A
   A implies A --> TRUTH
   A equiv A --> TRUTH
   A and not A, not A and A --> FALSEHOOD
   A or not A, not A or A --> TRUTH
   A implies not A --> not A
   not A implies A --> A
   A equiv not A, not A equiv A --> FALSEHOOD.

@IndexEdop(PMUT*)@\
Recursively permute the two components of an infix operator:
   A op B --> B op A

@IndexEdop(SUBEQ*)@\
Recursively apply the following law to a formula:
   A equiv B --> (A implies B) and (B implies A).

@IndexEdop(SUBIM*)@\
Recursively apply the following law to a formula:
   A implies B  --> not A or B.@End(Description)

@Section(Embedding Commands)

@Begin(Description)
@IndexEdop(MBED-AL) @i{rgwff}@\
Embed the current edwff in the left scope of AND. 
The right scope is provided by the user.

@IndexEdop(MBED-AR) @i{lgwff}@\
Embed the current edwff in the right scope of AND. 
The left scope is provided by the user.

@IndexEdop(MBED-E) @i{vquant}@\
Embed the current edwff in the scope of an existential quantifier. 
The variable of quantification is provided by the user.

@IndexEdop(MBED-E1) @i{vquant}@\
Embed the current edwff in the scope of an exists1 quantifier. 
The variable of quantification is provided by the user.

@IndexEdop(MBED-F) @i{vquant}@\
Embed the current edwff in the scope of a universal quantifier. 
The variable of quantification is provided by the user.

@IndexEdop(MBED-IL) @i{rgwff}@\
Embed the current edwff as the antecedent of a conditional. 
The consequent is provided by the user.

@IndexEdop(MBED-IR) @i{lgwff}@\
Embed the current edwff as the consequent of a conditional. 
The antecedent is provided by the user.

@IndexEdop(MBED-L) @i{vquant}@\
Embed the current edwff in the scope of lambda. 
The variable of quantification is provided by the user.

@IndexEdop(MBED-OL) @i{rgwff}@\
Embed the current edwff in the left scope of OR. 
The right scope is provided by the user.

@IndexEdop(MBED-OR) @i{lgwff}@\
Embed the current edwff in the right scope of OR. 
The left scope is provided by the user.

@IndexEdop(MBED-QL) @i{rgwff}@\
Embed the current edwff on the left side of equivalence. 
The right side is provided by the user.

@IndexEdop(MBED-QR) @i{lgwff}@\
Embed the current edwff on the right side of equivalence. 
The left side is provided by the user.

@IndexEdop(MBED=L) @i{rgwff}@\
Embed the current edwff on the left side of equality. 
The right side is provided by the user.

@IndexEdop(MBED=R) @i{lgwff}@\
Embed the current edwff on the right side of equality. 
The left side is provided by the user.@End(Description)

@Section(Rewriting commands)

@Begin(Description)
@IndexEdop(ARR)@\
Apply one active rewrite rule to the current edwff; attempt 
different active rules in the order in which they are listed by 
LIST-RRULES until one works. If any current rules are 
bidirectional, you will be prompted about which direction to 
apply them in.

@IndexEdop(ARR*)@\
Apply one active rewrite rule to the current edwff; attempt 
different active rules in the order in which they are listed by 
LIST-RRULES until one works. If any current rules are 
bidirectional, you will be prompted about which direction to 
apply them in. Repeat this until no more rules
are applicable. CAUTION: may not terminate.

@IndexEdop(ARR1) @i{rule}@\
Apply a rewrite rule (active or inactive) to the 
current edwff. If the rule is bidirectional, you will be 
prompted about which direction to apply it in.

@IndexEdop(ARR1*) @i{rule}@\
Apply a rewrite rule (active or inactive) repeatedly 
to the current edwff. If the rule is bidirectional, you will 
be prompted about which direction to apply it in.
CAUTION: may not terminate.

@IndexEdop(MAKE-RRULE) @i{name} @i{gwff2} @i{func} @i{types} @i{bidir} @i{appfn} @i{mhelp}@\
Create a rewrite rule whose left-hand side is 
the current edwff.

@IndexEdop(UNARR)@\
Unapply one active rewrite rule to the current edwff (i.e. apply
it in the reverse direction); attempt different active rules in the 
order in which they are listed by LIST-RRULES until one works.
If any current rules are bidirectional, you will be prompted 
about which direction to apply them in.

@IndexEdop(UNARR*)@\
Unapply one active rewrite rule to the current edwff (i.e. 
apply it in the reverse direction); attempt different active rules in 
the order in which they are listed by LIST-RRULES until one works. 
Repeat this until no more rules are applicable. If any current rules
are bidirectional, you will be prompted about which direction to 
apply them in. CAUTION: may not terminate.

@IndexEdop(UNARR1) @i{rule}@\
Unapply a rewrite rule (active or inactive) to the current 
edwff. (i.e. apply it in the reverse direction).
If the rule is bidirectional, you will be prompted about
which direction to apply it in.

@IndexEdop(UNARR1*) @i{rule}@\
Unapply a rewrite rule (active or inactive) repeatedly to 
the current edwff. (i.e. apply it in the reverse direction).
If the rule is bidirectional, you will be prompted about
which direction to apply it in.
CAUTION: may not terminate.@End(Description)

@Section(Substitution)

@Begin(Description)
@IndexEdop(AB) @i{newvar}@\
Alphabetic change of variable at top-level.

@IndexEdop(IB) @i{term}@\
Instantiate a top-level universal or existential binder with a term.

@IndexEdop(PRIM-SUBST) @i{var} @i{sub}@\
Replaces a variable with a primitive substitution.
Differs from SUBST in that it will also replace quantified
variables, and their quantifiers, as necessary.

@IndexEdop(REW-EQUIV)@\
Replaces all occurrences of the form `A EQUIV B'
according to the setting of the flag REWRITE-EQUIVS.

@IndexEdop(RP) @i{rep-sym} @i{rep-by}@\
Replace one occurrence of a symbol (such as AND) by a predefined 
equivalent wff (such as [lambda p lambda q.~.p IMPLIES ~q]).  In this
example repsym is AND and rep-by is IMPLIES.  To see if a symbol can be
replaced by this command, enter HELP symbol; any such replacements will be
listed under the heading 'Replaceable Symbols'.

@IndexEdop(RPALL) @i{rep-sym} @i{rep-by}@\
Replace a all occurrences of a symbol by a predefined equivalent wff.

@IndexEdop(SUB) @i{gwff}@\
Replaces the current wff by the wff supplied.

@IndexEdop(SUBST) @i{term} @i{var}@\
Substitute a term for the free occurrences of variable in a gwff.
Bound variables may be renamed, using the function in the global
variable REN-VAR-FN.

@IndexEdop(SUBSTYP) @i{typevar} @i{typesym}@\
Substitutes a type for a type variable in edwff.@End(Description)

@Section(Basic Abbreviations)

@Begin(Description)
@IndexEdop(ABBR)@\
Lists all the abbreviations used in a gwff.

@IndexEdop(CONSTANTS)@\
Lists all the logical constants used in a gwff, apart 
from the primitive constants AND FALSEHOOD IMPLIES NOT OR TRUTH.

@IndexEdop(EXPAND=)@\
Instantiate outermost equality in gwff. Consults the flag
  REWRITE-EQUALITIES (but ignores it if it's set to NONE).

@IndexEdop(EXPAND=*)@\
Instantiate all equalities in gwff. Consults the flag
  REWRITE-EQUALITIES (but ignores it if it's set to NONE).

@IndexEdop(INST) @i{gabbr}@\
Instantiate all occurrences of an abbreviation.
The occurrences will be lambda-contracted, but not lambda-normalized.

@IndexEdop(INST1)@\
Instantiate the first abbreviation, left-to-right.

@IndexEdop(INSTALL) @i{exceptions}@\
Instantiate all definitions, except the ones specified
in the second argument.

@IndexEdop(INSTALL-REC) @i{exceptions}@\
Recursively instantiate all definitions, except the ones specified
in the second argument.

@IndexEdop(LIB-ABBR)@\
Lists all the library abbreviations used in a gwff.

@IndexEdop(NEW-DEFS)@\
Lists all the definitions used in a gwff that are
either library abbreviations or weak labels.@End(Description)

@Section(Lambda-Calculus)

@Begin(Description)
@IndexEdop(ABNORM)@\
Convert the gwff to alphabetic normal form. 

@IndexEdop(ETAB)@\
Eta-expands until original wff is part of a wff of base type.

@IndexEdop(ETAC)@\
Reduces [lambda x.fx] to f at top.

@IndexEdop(ETAN)@\
Reduces [lambda x.fx] to f from inside out.

@IndexEdop(ETAX)@\
Performs a one-step eta expansion.

@IndexEdop(LETA)@\
Returns the long-eta normal form of wff.

@IndexEdop(LEXP) @i{var} @i{term} @i{occurs}@\
Converts the wff into the application of a function to the term.
The function is formed by replacing given valid occurrences of a term
with the variable and binding the result.

@IndexEdop(LNORM)@\
Put a wff into lambda-normal form, using beta or beta-eta conversion 
according to the value of flag LAMBDA-CONV. Compare LNORM-BETA and LNORM-ETA.

@IndexEdop(LNORM-BETA)@\
Put a wff into beta-normal form, not using eta 
conversion. Compare LNORM and LNORM-ETA.

@IndexEdop(LNORM-ETA)@\
Put a wff into eta-normal form, not using beta
conversion. Compare LNORM-BETA and LNORM.

@IndexEdop(RED)@\
Lambda-contract a top-level reduct.
Bound variables may be renamed using REN-VAR-FN

@IndexEdop(ULNORM)@\
Convert a untyped wff into lambda-normal form. Be aware of unterminated reduction 
in untyped lambda calculus.@End(Description)

@Section(Negation movers)

@Begin(Description)
@IndexEdop(NEG)@\
Negates current wff, erasing double negations.

@IndexEdop(NNF)@\
Return the negation normal form of the given wff.

@IndexEdop(PULL-NEG)@\
Pulls negations out one level.

@IndexEdop(PUSH-NEG)@\
Pushes negation through the outermost operator or quantifier.@End(Description)

@Section(Primitive Substitutions)

@Begin(Description)
@IndexEdop(NAME-PRIM)@\
Creates weak labels for primitive substitutions for the head
    variables of a wff.

@IndexEdop(PRT-PRIM)@\
Prints primitive substitutions for the head variables of a wff.@End(Description)

@Section(Miscellaneous)

@Begin(Description)
@IndexEdop(CLAUSE-FORM)@\
Converts the given wff to clause form, as if the resulting wff is to
    be given to a resolution theorem prover.  The gwff is skolemized,
    rectified, etc.

@IndexEdop(CNF)@\
Find the conjunctive normal form of a wff.

@IndexEdop(HEAD)@\
Find the head of a gwff.

@IndexEdop(HVARS)@\
Find all head variables of a wff.

@IndexEdop(MIN-SCOPE)@\
Minimize the scope of quantifiers in a gwff. Deletes vacuous
quantifiers. During proof transformation, the gap between a formula
and its min-quant-scope version is filled by RULEQ.

@IndexEdop(SUBFORMULAS) @i{type}@\
Find all subformulas of a given type in a wff.@End(Description)

@Section(RuleP)

@Begin(Description)
@IndexEdop(SAT)@\
Check whether a propositional wff is satisfiable.

@IndexEdop(VALID)@\
Check whether a propositional wff is valid.@End(Description)

@Section(Skolemizing)

@Begin(Description)
@IndexEdop(SK1) @i{univflag}@\
Skolemize a wff using method S1. See page 127 of Andrews' book.
   If equivalences are present, you must eliminate them first by REW-EQUIV.

@IndexEdop(SK3) @i{univflag}@\
Skolemize a wff using method S3.  At the moment it takes only
those free variables which are universally quantified somewhere before,
all other variables are considered to be constants.
    See page 127 of Andrews' book.
    If equivalences are present, you must eliminate them first by REW-EQUIV.@End(Description)

@Section(Quantifier Commands)

@Begin(Description)
@IndexEdop(DB)@\
Delete the leftmost binder in a wff.

@IndexEdop(EP)@\
Delete all accessible essentially existential quantifiers.

@IndexEdop(OP)@\
Delete all accessible essentially universal quantifiers.@End(Description)

@Section(Wellformedness)

@Begin(Description)
@IndexEdop(DUPW) @i{connective}@\
duplicates wff across connective.

@IndexEdop(EDILL)@\
Find a minimal ill-formed subformula.

@IndexEdop(ILL)@\
Return a list of messages, each the describing the error
in a minimal ill-formed subparts of the argument.

@IndexEdop(TP)@\
Return the type of a gwff.

@IndexEdop(WFFP)@\
Test for a gwff (general well-formed formula).@End(Description)
@ChapterPh(Replaceable Symbols)
The internal name of this category is 
REPSYMBOL.
A replaceable symbol can be defined using DEFREPSYMBOL.
Allowable properties are: @t{EQUIV-TO}, @t{MHELP}.

@Section(Basic Abbreviations)

@Begin(Description)
@TabSet(+40pts, +80pts)
@IndexOther(AND)@\@begin(verbatim)
AND may be replaced by any of:
INVERSE   @g{l}@;p@F12{o} @g{l}@;q@F12{o}.q @and@; p
IMPLIES   @g{l}@;p@F12{o} @g{l}@;q@F12{o}.@not@;.p @implies@; @not@;q
OR        @g{l}@;p@F12{o} @g{l}@;q@F12{o}.@not@;.@not@;p @or@; @not@;q@end(verbatim)

@IndexOther(IMPLIES)@\@begin(verbatim)
IMPLIES may be replaced by any of:
INVERSE   @g{l}@;p@F12{o} @g{l}@;q@F12{o}.@not@;q @implies@; @not@;p
AND       @g{l}@;p@F12{o} @g{l}@;q@F12{o}.@not@;.p @and@; @not@;q
OR        @g{l}@;p@F12{o} @g{l}@;q@F12{o}.@not@;p @or@; q@end(verbatim)

@IndexOther(OR)@\@begin(verbatim)
OR may be replaced by any of:
INVERSE   @g{l}@;p@F12{o} @g{l}@;q@F12{o}.q @or@; p
IMPLIES   @g{l}@;p@F12{o} @g{l}@;q@F12{o}.@not@;p @implies@; q
AND       @g{l}@;p@F12{o} @g{l}@;q@F12{o}.@not@;.@not@;p @and@; @not@;q@end(verbatim)

@IndexOther(SUBSET)@\@begin(verbatim)
SUBSET may be replaced by any of:
INTERSECT @g{l}@;p@F12{oa} @g{l}@;q@F12{oa}.p @intersect@; q = p
IMPLIES   @g{l}@;p@F12{oa} @g{l}@;q@F12{oa} @forall@;x@F12{a}.p x @implies@; q x
INVERSE   @g{l}@;p@F12{oa} @g{l}@;q@F12{oa}.~ q @subset@; ~ p@end(verbatim)
@End(Description)
@ChapterPh(Theorems)

@Section(Book Theorems)

@Begin(Description)
@IndexOther(DESCR)@ @ @ @ @ (Axiom of description at all types.)@\@t{@g{i}@; [= Y@F12{a}] = Y}


@IndexOther(EXT)@ @ @ @ @ (Axiom of extensionality at all types.)@\@t{@forall@;x@F12{b} [f@F12{ab} x = g@F12{ab} x] @implies@; f = g}


@IndexOther(EXT-LEIB)@ @ @ @ @ (Extensional equality of f and g implies Leibniz equality of f and g.)@\@t{@forall@;f@F12{ab} @forall@;g@F12{ab}.@forall@;x@F12{b} [f x = g x] @implies@; @forall@;q@F12{o(ab)}.q f @implies@; q g}


@IndexOther(REFL=)@ @ @ @ @ (Reflexivity of Equality.)@\@t{A@F12{a} = A}


@IndexOther(SYM=)@ @ @ @ @ (Symmetry of Equality.)@\@t{A@F12{a} = B@F12{a} @implies@; B = A}


@IndexOther(T5302)@ @ @ @ @ (Symmetry of Equality.)@\@t{x@F12{a} = y@F12{a} =.y = x}


@IndexOther(T5310)@ @ @ @ @ (Theorem about descriptions.)@\@t{@forall@;z@F12{a} [p@F12{oa} z @equiv@; y@F12{a} = z] @implies@; @g{i}@; p = y}


@IndexOther(T5310A)@ @ @ @ @ (Theorem about descriptions.)@\@t{@forall@;z@F12{a} [p@F12{oa} z @equiv@; z = y@F12{a}] @implies@; @g{i}@; p = y}@End(Description)

@Section(First-Order Logic)

@Begin(Description)
@IndexOther(X2106)@\@t{@forall@;x [R x @implies@; P x] @and@; @forall@;x [@not@;Q x @implies@; R x] @implies@; @forall@;x.P x @or@; Q x}


@IndexOther(X2107)@\@t{R a b @and@; @forall@;x @forall@;y [R x y @implies@; R y x @and@; Q x y] @and@; @forall@;u @forall@;v [Q u v @implies@; Q u u] @implies@; Q a a @and@; Q b b}


@IndexOther(X2108)@\@t{@forall@;x @exists@;y.P x @implies@; P y}


@IndexOther(X2109)@\@t{@exists@;x [p @and@; Q x] @equiv@; p @and@; @exists@;x Q x}


@IndexOther(X2110)@\@t{@exists@;x R x @and@; @forall@;y [R y @implies@; @exists@;z Q y z] @and@; @forall@;x @forall@;y [Q x y @implies@; Q x x] @implies@; @exists@;x @exists@;y.Q x y @and@; R y}


@IndexOther(X2111)@\@t{@forall@;x [@exists@;y P x y @implies@; @forall@;y Q x y] @and@; @forall@;z @exists@;y P z y @implies@; @forall@;y @forall@;x Q x y}


@IndexOther(X2112)@\@t{@exists@;v @forall@;x P x v @and@; @forall@;x [S x @implies@; @exists@;y Q y x] @and@; @forall@;x @forall@;y [P x y @implies@; @not@;Q x y] @implies@; @exists@;u.@not@;S u}


@IndexOther(X2113)@\@t{@forall@;y @exists@;w R y w @and@; @exists@;z @forall@;x [P x @implies@; @not@;R z x] @implies@; @exists@;x.@not@;P x}


@IndexOther(X2114)@\@t{@forall@;x R x b @and@; @forall@;y [@exists@;z R y z @implies@; R a y] @implies@; @exists@;u @forall@;v R u v}


@IndexOther(X2115)@\@t{@forall@;x [@exists@;y P x y @implies@; @forall@;z P z z] @and@; @forall@;u @exists@;v [P u v @or@; M u @and@; Q.f u v] @and@; @forall@;w [Q w @implies@; @not@;M.g w] @implies@; @forall@;u @exists@;v.P [g u] v @and@; P u u}


@IndexOther(X2116)@\@t{@forall@;x @exists@;y [P x @implies@; R x [g.h y] @and@; P y] @and@; @forall@;w [P w @implies@; P [g w] @and@; P.h w] @implies@; @forall@;x.P x @implies@; @exists@;y.R x y @and@; P y}


@IndexOther(X2117)@\@t{@forall@;u @forall@;v [R u u @equiv@; R u v] @and@; @forall@;w @forall@;z [R w w @equiv@; R z w] @implies@;.@exists@;x R x x @implies@; @forall@;y R y y}


@IndexOther(X2118)@\@t{@forall@;x [p @and@; Q x @or@; @not@;p @and@; R x] @implies@; @forall@;x Q x @or@; @forall@;x R x}


@IndexOther(X2119)@\@t{@exists@;y @forall@;x.P y @implies@; P x}


@IndexOther(X2120)@\@t{@forall@;u @forall@;v @forall@;w [P u v @or@; P v w] @implies@; @exists@;x @forall@;y P x y}


@IndexOther(X2121)@\@t{@exists@;v @forall@;y @exists@;z.P a y [h y] @or@; P v y [f y] @implies@; P v y z}


@IndexOther(X2122)@\@t{@exists@;x R x x @implies@; @forall@;y R y y @implies@; @exists@;u @forall@;v.R u u @implies@; R v v}


@IndexOther(X2123)@\@t{@exists@;y [P y @implies@; Q x] @implies@; @exists@;y.P y @implies@; Q y}


@IndexOther(X2124)@\@t{@exists@;x [P x @implies@; Q x] @equiv@; @forall@;x P x @implies@; @exists@;x Q x}


@IndexOther(X2125)@\@t{@exists@;x @forall@;y [P x @equiv@; P y] @equiv@;.@exists@;x P x @equiv@; @forall@;y P y}


@IndexOther(X2126)@\@t{@forall@;x [P x @equiv@; @exists@;y P y] @equiv@;.@forall@;x P x @equiv@; @exists@;y P y}


@IndexOther(X2127)@\@t{@exists@;x @forall@;y [P y @equiv@; P x] @implies@; @forall@;x P x @or@; @forall@;x.@not@;P x}


@IndexOther(X2128)@\@t{@forall@;x [P x @equiv@; @forall@;y P y] @equiv@;.@exists@;x P x @equiv@; @forall@;y P y}


@IndexOther(X2129)@\@t{@exists@;x @forall@;y [P x @equiv@; P y] @equiv@; [@exists@;x Q x @equiv@; @forall@;y P y] @equiv@;.@exists@;x @forall@;y [Q x @equiv@; Q y] @equiv@;.@exists@;x P x @equiv@; @forall@;y Q y}


@IndexOther(X2130)@\@t{@forall@;x P x @implies@; @not@;@exists@;y Q y @or@; @exists@;z.P z @implies@; Q z}


@IndexOther(X2131)@\@t{@forall@;x P x @implies@; @exists@;y.@forall@;x @forall@;z Q x y z @implies@; @not@;@forall@;z.P z @and@; @not@;Q y y z}


@IndexOther(X2132)@\@t{@forall@;w [@not@;R w w] @implies@; @exists@;x @exists@;y.@not@;R x y @and@;.Q y x @implies@; @forall@;z Q z z}


@IndexOther(X2133)@\@t{@forall@;x [@exists@;y Q x y @implies@; P x] @and@; @forall@;v @exists@;u Q u v @and@; @forall@;w @forall@;z [Q w z @implies@; Q z w @or@; Q z z] @implies@; @forall@;z P z}


@IndexOther(X2134)@\@t{@forall@;z @exists@;x [@forall@;y P x y @or@; Q x z] @implies@; @forall@;y @exists@;x.P x y @or@; Q x y}


@IndexOther(X2135)@\@t{@exists@;x @forall@;y.P x @and@; Q y @implies@; Q x @or@; P y}


@IndexOther(X2136)@\@t{@exists@;x @exists@;y @forall@;u.P x y z @implies@; P u x x}


@IndexOther(X2137)@\@t{@exists@;x @forall@;y.P x @implies@; Q x @or@; P y}


@IndexOther(X2138)@\@t{@forall@;x @exists@;y F x y @and@; @exists@;x @forall@;e @exists@;n @forall@;w [S n w @implies@; D w x e] @and@; @forall@;e @exists@;d @forall@;a @forall@;b [D a b d @implies@; @forall@;y @forall@;z.F a y @and@; F b z @implies@; D y z e] @implies@; @exists@;y @forall@;e @exists@;m @forall@;w.S m w @implies@; @forall@;z.F w z @implies@; D z y e}@End(Description)

@Section(Higher-Order Logic)

@Begin(Description)
@IndexOther(X5200)@\@t{x@F12{oa} @union@; y@F12{oa} = @setunion@;.@g{l}@;v@F12{oa}.v = x @or@; v = y}


@IndexOther(X5201)@\@t{x@F12{oa} @intersect@; y@F12{oa} = @setintersect@;.@g{l}@;v@F12{oa}.v = x @or@; v = y}


@IndexOther(X5202)@\@t{% f@F12{ab} [x@F12{ob} @union@; y@F12{ob}] = % f x @union@; % f y}


@IndexOther(X5203)@\@t{% f@F12{ab} [x@F12{ob} @intersect@; y@F12{ob}] @subset@; % f x @intersect@; % f y}


@IndexOther(X5204)@\@t{% f@F12{ab} [@setunion@; w@F12{o(ob)}] = @setunion@;.% [% f] w}


@IndexOther(X5205)@\@t{% f@F12{ab} [@setintersect@; w@F12{o(ob)}] @subset@; @setintersect@;.% [% f] w}


@IndexOther(X5206)@\@t{% f@F12{ab} [x@F12{ob} @union@; y@F12{ob}] = % f x @union@; % f y}


@IndexOther(X5207)@\@t{% f@F12{ab} [x@F12{ob} @intersect@; y@F12{ob}] @subset@; % f x @intersect@; % f y}


@IndexOther(X5208)@\@t{@exists@;S@F12{oi} @forall@;x@F12{i} [[S x @or@; P@F12{oi} x] @and@;.@not@;S x @or@; Q@F12{oi} x] @equiv@; @forall@;y@F12{i}.P y @or@; Q y}


@IndexOther(X5209)@\@t{@powerset@;@F12{o(oa)(oa)} [D@F12{oa} @intersect@; E@F12{oa}] = @powerset@; D @intersect@; @powerset@; E}


@IndexOther(X5210)@\@t{[= x@F12{a}] = @g{l}@;z@F12{a} @exists@;y@F12{a}.y = x @and@; z = y}


@IndexOther(X5211)@\@t{y@F12{oa} = @setunion@;.@g{l}@;z@F12{oa} @exists@;x@F12{a}.y x @and@; z = [= x]}


@IndexOther(X5212)@\@t{@g{l}@;z@F12{a} @exists@;x@F12{b} [g@F12{ob} x @and@; z = f@F12{ab} x] = % f g}


@IndexOther(X5303)@\@t{= = @g{l}@;x@F12{a} @g{l}@;y@F12{a} @forall@;p@F12{oaa}.@forall@;z@F12{a} p z z @implies@; p x y}


@IndexOther(X5304)@\@t{@not@;@exists@;g@F12{oaa} @forall@;f@F12{oa} @exists@;j@F12{a}.g j = f}


@IndexOther(X5305)@\@t{@forall@;s@F12{oa}.@not@;@exists@;g@F12{oaa} @forall@;f@F12{oa}.f @subset@; s @implies@; @exists@;j@F12{a}.s j @and@; g j = f}


@IndexOther(X5308)@\@t{@exists@;j@F12{b(ob)} @forall@;p@F12{ob} [@exists@;x@F12{b} p x @implies@; p.j p] @implies@;.@forall@;x@F12{a} @exists@;y@F12{b} r@F12{oba} x y @equiv@; @exists@;f@F12{ba} @forall@;x r x.f x}


@IndexOther(X5309)@\@t{@not@;@exists@;h@F12{i(oi)} @forall@;p@F12{oi} @forall@;q@F12{oi}.h p = h q @implies@; p = q}


@IndexOther(X5310)@\@t{@forall@;r@F12{ob(ob)} [@forall@;x@F12{ob} @exists@;y@F12{b} r x y @implies@; @exists@;f@F12{b(ob)} @forall@;x r x.f x] @implies@; @exists@;j@F12{b(ob)} @forall@;p@F12{ob}.@exists@;z@F12{b} p z @implies@; p.j p}


@IndexOther(X5500)@\@t{@forall@;P@F12{ob} [@exists@;x@F12{b} P x @implies@; P.J@F12{b(ob)} P] @implies@; @forall@;f@F12{ab} @forall@;g@F12{ab}.f [J.@g{l}@;x.@not@;.f x = g x] = g [J.@g{l}@;x.@not@;.f x = g x] @implies@; f = g}


@IndexOther(X6004)@\@t{@eqp@;@F12{o(oa)(ob)} [= x@F12{b}].= y@F12{a}}


@IndexOther(X6101)@\@t{@one@; = @g{S}@;@+{1}@;@F12{o(oi)}}


@IndexOther(X6104)@\@t{@exists@;i@F12{o(aa)(aa)}.@forall@;g@F12{aa} [i g [@g{l}@;x@F12{a} x] @and@; i g.@g{l}@;x g.g x] @and@; @forall@;f@F12{aa} @forall@;y@F12{a}.i [@g{l}@;x y] f @implies@; f y = y}


@IndexOther(X6105)@ @ @ @ @ (This is a lemma for X6106. 
You may need to ASSERT DESCR or T5310 or T5310A)@\@t{@forall@;n@F12{o(oi)}.NAT n @implies@; @forall@;q@F12{oi}.n q @implies@; @exists@;j@F12{i(oi)} @forall@;r@F12{oi}.r @subset@; q @and@; @exists@;x@F12{i} r x @implies@; r.j r}


@IndexOther(X6106)@\@t{FINITE [@g{l}@;x@F12{i} @truth@;] @implies@; @exists@;j@F12{i(oi)} @forall@;r@F12{oi}.@exists@;x r x @implies@; r.j r}


@IndexOther(X6201)@\@t{@exists@;r@F12{oaa} @forall@;x@F12{a} @forall@;y@F12{a} @forall@;z@F12{a} [@exists@;w@F12{a} r x w @and@; @not@;r x x @and@;.r x y @implies@;.r y z @implies@; r x z] @implies@; @exists@;R@F12{o(oa)(oa)} @forall@;X@F12{oa} @forall@;Y@F12{oa} @forall@;Z@F12{oa}.@exists@;W@F12{oa} R X W @and@; @not@;R X X @and@;.R X Y @implies@;.R Y Z @implies@; R X Z}


@IndexOther(X8030A)@\@t{[g@F12{oo} @truth@; @and@; g @falsehood@;] = @forall@;x@F12{o} g x}@End(Description)
@ChapterPh(Logical Abbreviations)
The internal name of this category is 
ABBREV.
A logical abbreviation can be defined using DEF-ABBREV.
Allowable properties are: @t{TYPE}, @t{TYPELIST}, @t{DEFN}, @t{DEFN-FUN}, @t{MHELP}, and more.

@Section(Basic Abbreviations)

@Begin(Description)
@TabSet(+40pts, +80pts)
@IndexOther(<=)@\@\7 (Infix)@\@t{@g{l}@;x@F12{s} @g{l}@;y@F12{s} @forall@;p@F12{os}.p x @and@; @forall@;z@F12{s} [p z @implies@; p.SUCC@F12{ss} z] @implies@; p y}.  

@IndexOther(COND)@\@\@\@t{@g{l}@;x@F12{c} @g{l}@;y@F12{c} @g{l}@;p@F12{o} THAT q@F12{c}.p @and@; x = q @or@; @not@;p @and@; y = q}.  

@IndexOther(EQP)@\@t{@eqp@;}@\@\@t{@g{l}@;p@F12{ob} @g{l}@;q@F12{oa} @exists@;s@F12{ab}.@forall@;x@F12{b} [p x @implies@; q.s x] @and@; @forall@;y@F12{a}.q y @implies@; @exists@;@-{1}@;x.p x @and@; y = s x}.  

@IndexOther(EQUIV)@\@t{@equiv@;}@\2 (Infix)@\@t{=}.  

@IndexOther(FINITE)@\@\@\@t{@g{l}@;p@F12{oi} @exists@;n@F12{o(oi)}.NAT n @and@; n p}.  

@IndexOther(MU)@\@t{@g{m}@;}@\@\@t{@g{l}@;p@F12{os} THAT x@F12{s}.NAT x @and@; p x @and@; FORALLN y@F12{s}.p y @implies@; x <= y}.  

@IndexOther(NAT)@\@\@\@t{@g{l}@;n@F12{o(oi)} @forall@;p@F12{os}.p ZERO@F12{s} @and@; @forall@;x@F12{s} [p x @implies@; p.SUCC@F12{ss} x] @implies@; p n}.  

@IndexOther(NC)@\@\@\@t{@g{l}@;u@F12{o(ob)} @exists@;p@F12{ob}.u = @eqp@;@F12{o(ob)(ob)} p}.  

@IndexOther(ONE)@\@t{@one@;}@\@\@t{SUCC@F12{ss} ZERO@F12{s}}.  

@IndexOther(RECURSION)@\@\@\@t{@g{l}@;h@F12{sss} @g{l}@;g@F12{s} @g{l}@;n@F12{o(oi)} THAT m@F12{s} @forall@;w@F12{oss}.w ZERO@F12{s} g @and@; @forall@;x@F12{s} @forall@;y@F12{s} [w x y @implies@; w [SUCC@F12{ss} x].h x y] @implies@; w n m}.  

@IndexOther(SIGMA1)@\@t{@g{S}@;@+{1}@;}@\@\@t{@g{l}@;P@F12{oa} @exists@;y@F12{a}.P = [= y]}.  

@IndexOther(SUBSET)@\@t{@subset@;}@\8 (Infix)@\@t{@g{l}@;P@F12{oa} @g{l}@;R@F12{oa} @forall@;x@F12{a}.P x @implies@; R x}.  

@IndexOther(SUCC)@\@\@\@t{@g{l}@;n@F12{o(oi)} @g{l}@;p@F12{oi} @exists@;x@F12{i}.p x @and@; n.@g{l}@;t@F12{i}.@not@;[t = x] @and@; p t}.  

@IndexOther(UNITSET)@\@t{@scriptu@;}@\@\@t{@g{l}@;x@F12{a} @g{l}@;y@F12{a}.x = y}.  

@IndexOther(ZERO)@\@\@\@t{@g{l}@;p@F12{oi}.@not@;@exists@;x@F12{i} p x}.  
@End(Description)

@Section(Set Abbreviations)

@Begin(Description)
@TabSet(+40pts, +80pts)
@IndexOther(%)@\@\@\@t{@g{l}@;f@F12{ab} @g{l}@;x@F12{ob} @g{l}@;z@F12{a} @exists@;t@F12{b}.x t @and@; z = f t}.  

@IndexOther(COMPLEMENT)@\@t{~}@\11 (Prefix)@\@t{@g{l}@;S@F12{oa} @g{l}@;x@F12{a}.@not@;S x}.  

@IndexOther(EQUIVS)@\@t{@equiv@;@+{s}@;}@\7 (Infix)@\@t{@g{l}@;P@F12{oa} @g{l}@;R@F12{oa} @forall@;x@F12{a}.P x @equiv@; R x}.  

@IndexOther(INTERSECT)@\@t{@intersect@;}@\10 (Infix)@\@t{@g{l}@;P@F12{oa} @g{l}@;R@F12{oa} @g{l}@;x@F12{a}.P x @and@; R x}.  

@IndexOther(POWERSET)@\@t{@powerset@;}@\@\@t{@g{l}@;P@F12{oa} @g{l}@;R@F12{oa}.R @subset@; P}.  

@IndexOther(SETEQUIV)@\@t{@equiv@;@+{s}@;}@\7 (Infix)@\@t{@g{l}@;P@F12{oa} @g{l}@;R@F12{oa}.P @subset@; R @and@; R @subset@; P}.  

@IndexOther(SETINTERSECT)@\@t{@setintersect@;}@\@\@t{@g{l}@;D@F12{o(oa)} @g{l}@;x@F12{a} @forall@;S@F12{oa}.D S @implies@; S x}.  

@IndexOther(SETUNION)@\@t{@setunion@;}@\@\@t{@g{l}@;D@F12{o(oa)} @g{l}@;x@F12{a} @exists@;S@F12{oa}.D S @and@; S x}.  

@IndexOther(UNION)@\@t{@union@;}@\9 (Infix)@\@t{@g{l}@;P@F12{oa} @g{l}@;R@F12{oa} @g{l}@;z@F12{a}.P z @or@; R z}.  
@End(Description)
@ChapterPh(Binders)
The internal name of this category is 
BINDER.
A binder can be defined using DEF-BINDER.
Allowable properties are: @t{TYPELIST}, @t{VAR-TYPE}, @t{SCOPE-TYPE}, @t{WFF-TYPE}, @t{DEF-VAR}, @t{DEF-SCOPE}, @t{DEFN}, @t{MHELP}, and more.

@Section(wff Primitives)

@Begin(Description)
@TabSet(+40pts, +80pts)
@IndexOther(LAMBDA)@\@t{@g{l}@;}@\100 (Prefix)@\
Church's lambda binder.
@End(Description)

@Section(Basic Abbreviations)

@Begin(Description)
@TabSet(+40pts, +80pts)
@IndexOther(EXISTS)@\@t{@exists@;}@\100 (Prefix)@\
Existential quantifier.

@IndexOther(EXISTS1)@\@t{@exists@;@-{1}@;}@\100 (Prefix)@\@t{@g{S}@;@+{1}@;@F12{o(oa)}.@g{l}@;x@F12{a} A@F12{o}}.  

@IndexOther(EXISTSN)@\@\100 (Prefix)@\@t{@exists@;z@F12{s}.NAT z @and@; A@F12{o}}.  

@IndexOther(FORALL)@\@t{@forall@;}@\100 (Prefix)@\
Universal quantifier.

@IndexOther(FORALLN)@\@\100 (Prefix)@\@t{@forall@;z@F12{s}.NAT z @implies@; A@F12{o}}.  

@IndexOther(MU-BIND)@\@t{@g{m}@;}@\100 (Prefix)@\@t{@g{m}@;.@g{l}@;z@F12{s} A@F12{o}}.  

@IndexOther(THAT)@\@\100 (Prefix)@\@t{@g{i}@;.@g{l}@;z@F12{c} A@F12{o}}.  
@*@\@\@\
Description binder: Selects the unique term such that.
@End(Description)
@ChapterPh(Logical Constants)
The internal name of this category is 
LOGCONST.
A logical constant can be defined using DEF-LOGCONST.
Allowable properties are: @t{TYPE}, @t{MHELP}, and more.

@Section(wff Primitives)

@Begin(Description)
@TabSet(+40pts, +80pts)
@IndexOther(AND)@\@t{@and@;}@\5 (Infix)@\
Denotes conjunction.

@IndexOther(FALSEHOOD)@\@t{@falsehood@;}@\@\
Denotes falsehood.

@IndexOther(IMPLIES)@\@t{@implies@;}@\3 (Infix)@\
Denotes implication.

@IndexOther(NOT)@\@t{@not@;}@\8 (Prefix)@\
Denotes negation.

@IndexOther(OR)@\@t{@or@;}@\4 (Infix)@\
Denotes (inclusive) disjunction.

@IndexOther(TRUTH)@\@t{@truth@;}@\@\
Denotes truth.
@End(Description)
@ChapterPh(Polymorphic Proper Symbols)
The internal name of this category is 
PMPROPSYM.
A polymorphic proper symbol can be defined using DEF-PMPROPSYM.
Allowable properties are: @t{TYPE}, @t{TYPELIST}, @t{MHELP}, and more.

@Section(wff Primitives)

@Begin(Description)
@TabSet(+40pts, +80pts)
@IndexOther(=)@\
Equality

@IndexOther(IOTA)@\
Description operator@End(Description)
@ChapterPh(Typeconstants)
The internal name of this category is 
TYPECONST.
A typeconstant can be defined using DEF-TYPECONST.
Allowable properties are: @t{DEFN}, @t{MHELP}.

@Section(wff Primitives)

@Begin(Description)
@IndexOther(I)@\
The type of individuals.

@IndexOther(O)@\
The type of truth values.@End(Description)
@ChapterPh(Type Abbreviations)
The internal name of this category is 
TYPEABBREV.
A type abbreviation can be defined using DEF-TYPEABBREV.
Allowable properties are: @t{TYPE-DEFN}, @t{MHELP}.

@Section(wff Primitives)

@Begin(Description)
@IndexOther(S)@\
The type of natural numbers.@End(Description)
@ChapterPh(Library Commands)
The internal name of this category is 
LIBRARYCMD.
A library command can be defined using DEFLIBRARY.
Allowable properties are: @t{LIB-ARGTYPES}, @t{LIB-ARGNAMES}, @t{LIB-ARGHELP}, @t{LIB-DEFAULTFNS}, @t{LIB-MAINFNS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
Leave LIBRARY to the next enclosing top level.@End(Description)

@Section(Display)

@Begin(Description)
@IndexOther(KEY) @i{string} @i{backup}@\
Search for a string in the names of all library objects. If
the given string is also a keyword (see SHOW-KEYWORDS), then the keywords
for each library object will also be searched. This command does not search
the help messages of library objects.

@IndexOther(LIBFILES)@\
Lists all library files in the current default directories,
or in a single chosen directory.

@IndexOther(LIBOBJECTS-IN-FILE) @i{file}@\
Lists the contents of a file.

If more than one file of the given name is found in the library directories
in DEFAULT-LIB-DIR and BACKUP-LIB-DIR, the user is prompted to choose one.

@IndexOther(LIST-OF-LIBOBJECTS) @i{type} @i{backup}@\
List all objects or all objects of specified TYPE.

@IndexOther(SCRIBE-ALL-WFFS) @i{backup} @i{filter} @i{fname} @i{verbosity}@\
Write all wffs in all files in DEFAULT-LIB-DIR (and optionally BACKUP-LIB-DIR)
to an mss file.
The three verbosity settings are: MIN, which just shows the names of the 
objects, MED, which shows the help messages, keywords, provability and wffs 
as well, and MAX, which shows everything. 
As a filter, you can select any known keywords; only the wffs which 
satisfy all of the given keywords will be shown. See SHOW-KEYWORDS 
for a list of keywords.

@IndexOther(SCRIBELIBDIR) @i{directory} @i{types} @i{filename} @i{verbosity} @i{eject}@\
Print all the library files in a given directory into 
MSS files. See SCRIBELIBFILE for details.

@IndexOther(SCRIBELIBFILE) @i{filenamesin} @i{filenamesout} @i{verbosity}@\
Print the specified library files into MSS files.
The three verbosity settings are: MIN, which just shows the names of the 
objects, MED, which shows the help messages, keywords, provability and wffs 
as well, and MAX, which shows everything. It can take a list of filenames and 
a corresponding list of output files; if the latter list is too long it will 
be truncated, and if it is too short then the last filename given will be used 
for all the remaining output (so you can write a group of library files to a 
single output file by only supplying one output filename).  
After leaving TPS, run the .mss files through Scribe and print the resulting
files.

Some files in the list of library files may be ambiguous, in the 
sense that more than one file of the given name exists in the 
library directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR.  
In this case, the user is prompted to disambiguate each ambiguous
filename from first to last.

@IndexOther(SEARCH) @i{type} @i{stringlist} @i{backup}@\
Search the entire library, including all comments, for any one
of a given list of strings, and return the names of all objects which
contain such a string. This is useful for finding out, for example,
which gwffs can be proven using either MS88 or MS89.
WARNING: THIS COMMAND IS SLOW, AND CAN USE A LOT OF MEMORY.
You might want to think about using the Unix "grep" command instead.

@IndexOther(SEARCH2) @i{type} @i{stringlist} @i{backup}@\
Search the entire library, including all comments, for a 
given combination of strings. See also SEARCH.
The syntax for the given list is essentially conjunctive normal
form -- it should be a list of conjuncts, each of which is a list of 
disjuncts.
For example:
((MS88) (THM)) finds everything containing THM and MS88
((MS88 THM)) finds everything containing THM or MS88
((MS88 MS89) (THM EXERCISE)) finds everything containing (MS88 or MS89)
                             and (THM or EXERCISE).
WARNING: THIS COMMAND IS SLOW, AND CAN USE A LOT OF MEMORY.
You might want to think about using the Unix "grep" command instead.

@IndexOther(SHOW) @i{name} @i{type}@\
Display a library object.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate.

@IndexOther(SHOW*-WFF) @i{name}@\
Display the wff of a gwff in the library, with the associated
help message, keywords and provability status.
Also shows any needed objects, such as the definition and
help for abbrevations used in the gwff.

@IndexOther(SHOW-ALL-WFFS) @i{backup} @i{filter}@\
Show all wffs in all files in DEFAULT-LIB-DIR (and optionally BACKUP-LIB-DIR).
As a filter, you can select any known keywords; only the wffs which 
satisfy all of the given keywords will be shown. See SHOW-KEYWORDS 
for a list of keywords.

@IndexOther(SHOW-HELP) @i{name} @i{type}@\
Display the help message associated with a library object.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate.

@IndexOther(SHOW-OBJECTS-IN-FILE) @i{file} @i{types}@\
Lists all the objects of the given type (or types) in a file.

If more than one file of the given name is found in the library directories
in DEFAULT-LIB-DIR and BACKUP-LIB-DIR, the user is prompted to choose one.

@IndexOther(SHOW-TIMING) @i{name} @i{screen}@\
Display the timing information of a gwff in the library.
NOTE: Will only display timing information that has been recorded 
in standard DATEREC format.
If you opt for output to go to a file as well as to the screen, 
the format of the file will be SCRIBE or TEX if this is the current
value of the STYLE flag, and GENERIC otherwise.

@IndexOther(SHOW-WFF) @i{name}@\
Display the wff of a gwff in the library.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate.

@IndexOther(SHOW-WFF&HELP) @i{name}@\
Display the wff of a gwff in the library, with the associated
help message, keywords and provability status.

@IndexOther(SHOW-WFFS-IN-FILE) @i{file}@\
Lists the wffs in a file.

@IndexOther(TEX-ALL-WFFS) @i{backup} @i{filter} @i{fname} @i{verbosity}@\
Write all wffs in all files in DEFAULT-LIB-DIR (and optionally BACKUP-LIB-DIR)
to a TeX file. 
The three verbosity settings are: MIN, which just shows the names of the 
objects, MED, which shows the help messages, provability and wffs as well, and MAX, 
which shows everything.
As a filter, you can select any known keywords; only the wffs which 
satisfy all of the given keywords will be shown. See SHOW-KEYWORDS 
for a list of keywords.

@IndexOther(TEXLIBDIR) @i{directory} @i{types} @i{filename} @i{verbosity} @i{eject}@\
Print all the library files in a given directory into 
TEX files. See TEXLIBFILE for details.

@IndexOther(TEXLIBFILE) @i{filenamesin} @i{filenamesout} @i{verbosity}@\
Print the specified library files into TeX files.
The three verbosity settings are: MIN, which just shows the names of the 
objects, MED, which shows the help messages, keywords, provability and wffs 
as well, and MAX, which shows everything. It can take a list of filenames 
and a corresponding list of output files; if the latter list is too long it 
will be truncated, and if it is too short then the last filename given will 
be used for all the remaining output (so you can write a group of library 
files to a single output file by only supplying one output filename).  
After leaving TPS, run the .tex files through TeX and print the resulting
files.@End(Description)

@Section(Reading)

@Begin(Description)
@IndexOther(DESTROY) @i{name}@\
Remove a library object from TPS (the object will remain
stored in the library).

@IndexOther(FETCH) @i{name} @i{type}@\
Make a library object available in TPS.
Will create a new TPS object if EXPERTFLAG is set to T, otherwise
will create a weak label for the new library object.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate.

@IndexOther(FIND-PROVABLE) @i{backup}@\
Look for gwffs with a certain provability status.

@IndexOther(RESTORE-MASTERINDEX)@\
Restore library master index. Normally this need not be done by the
user as it is done automatically when TPS is first entered. However, if
the contents of the library may have been changed from outside of TPS
(e.g. by a text editor) since TPS was started, then this command will
re-initialize the library index.

@IndexOther(RETRIEVE-FILE) @i{file}@\
Make all objects in a library file available in TPS. Objects in a
file are retrieved in the same order as they are stored in the file.

If more than one file of the given name is found in the library directories
in DEFAULT-LIB-DIR and BACKUP-LIB-DIR, the user is prompted to choose one.@End(Description)

@Section(Library Structure)

@Begin(Description)
@IndexOther(COPY-LIBDIR) @i{omit-other-remarks}@\
COPY-LIBDIR can be used to copy a library directory into a
new library directory which TPS will automatically create,
or it can be used to copy the contents of a library directory into
an existing library directory.  If COPY-LIBDIR is copying into
an existing directory, and an object of the same name and type
exists in both the source and destination directory, the original
object remains in the destination directory instead of being overwritten.
The user has the option of omitting the other-remarks property
of the library objects.  If any needed-objects are left over,
the user is given the option of copying these extra needed-objects
into a new library file in the destination library directory.

COPY-LIBDIR will also copy the bestmodes and keywords files, if they
exist.  If the target directory already has a bestmodes or keywords
file, then the corresponding files will be merged.

@IndexOther(COPY-LIBFILE) @i{oldfile} @i{newfile} @i{omit-other-remarks}@\
Copy a file of library objects. The source file will
be found among the directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR
(the user will be prompted if more than one such file exists, and also
if there is a choice of directories for the new file).
Needed objects are not copied.

@IndexOther(CREATE-LIB-DIR) @i{directory}@\
Create a directory to store files containing library items.
This will not only create the directory, but create a file libindex.rec
so that TPS will recognize the directory as a library directory.
This command can be executed for the latter purpose even if the
directory already exists. 
This command will automatically add the directory to DEFAULT-LIB-DIR in
the current session of TPS.

@IndexOther(CREATE-LIB-SUBDIR) @i{subdir}@\
Creates a subdirectory of a current library directory
in DEFAULT-LIB-DIR to store files containing library items.
This will not only create the directory, but also creates a
LIB-MASTERINDEX-FILE so that TPS will recognize the directory as a 
library directory.  This command will also add the subdirectory to
DEFAULT-LIB-DIR.  TPS automatically looks for subdirectories when setting
DEFAULT-LIB-DIR, so there is no need to add the subdirectory to
the DEFAULT-LIB-DIR setting in the tps3.ini file.

@IndexOther(DELETE-LIB-DIR)@\
Deletes a library directory and removes it from DEFAULT-LIB-DIR.
The command will fail if the directory contains any library objects
(i.e., if the index file is not empty).

@IndexOther(DELETE-LIBFILE) @i{filename}@\
Delete a Library File

@IndexOther(MOVE-LIBFILE) @i{oldfile} @i{newfile}@\
Move a file of library objects. The source file will
be found among the directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR
(the user will be prompted if more than one such file exists, and also
if there is a choice of directories for the new file).
Needed objects are not moved.

@IndexOther(RENAME-LIBDIR)@\
Rename a Library Directory

@IndexOther(RENAME-LIBFILE) @i{oldfile} @i{newfile}@\
Rename a Library File (within the same library directory)

@IndexOther(UPDATE-LIBDIR) @i{omit-other-remarks} @i{directory}@\
UPDATE-LIBDIR can be used to update a (common) library directory
by copying any object from a directory DEFAULT-LIB-DIR or BACKUP-LIB-DIR
into the (common) library directory, if it is not already there.
Before updating from a library directory, the user is asked whether
to update from this directory.  This is so one can choose a collection
of library directories to combine into the common destination directory.

This has the same effect of 

1. calling COPY-LIBDIR with copying from each (chosen) directory in
DEFAULT-LIB-DIR and BACKUP-LIB-DIR into the (common) destination
library directory.

2. Calling IMPORT-NEEDED-OBJECTS to ensure all needed-objects
are also put into the destination directory.

If one wants to get the latest version of all library items,
specify the complete pathname of a nonexistent directory when 
TPS prompts for a destination directory.@End(Description)

@Section(Editing)

@Begin(Description)
@IndexOther(ADD-GOODMODES) @i{modes-gwffs} @i{newmodes} @i{newthms}@\
Add modes to a list of goodmodes.  Also, add theorems that these goodmodes can prove.

@IndexOther(CHANGE-PROVABILITY) @i{name}@\
Change the PROVABILITY attribute of a stored gwff.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate.

@IndexOther(CHECK-NEEDED-OBJECTS)@\
Checks for library objects which are not stored
in the chosen directory, but are needed by some object in
that directory.

@IndexOther(COPY-LIBOBJECT) @i{name} @i{type} @i{filename} @i{omit-other-remarks}@\
Copy an object from some specified directory to the default directory.
Does not copy the library entries of needed objects.

If more than one library object of this name is stored in
the library and SHOW-ALL-LIBOBJECTS is set to T,
the user is prompted to disambiguate.

@IndexOther(DELETE) @i{names} @i{type}@\
Delete an object from the library.

If more than one library object of this name is stored in
the library, the user is prompted to disambiguate.

@IndexOther(FIX-MODES)@\
Change all references to obsolete flags into the 
appropriate new flag setting, for every mode in your library
directory. You only need to do this once.
You will be prompted before anything is changed, and
you should probably keep a backup copy of your old library in
case disaster strikes!
THE CODE FOR THIS COMMAND SHOULD BE REWRITTEN FOR EACH 
RELEVANT CHANGE TO THE TPS FLAGS.
At the minute, it's set up to remove references to 
REWRITE-DEFNS-EAGER, REWRITE-EQUAL-EXT and REWRITE-ONLY-EXT,
which have been removed, and to reset REWRITE-DEFNS
and REWRITE-EQUALITIES to appropriate values.
It also puts LAST-MODE-NAME at the head of all settings
for RECORDFLAGS.

@IndexOther(IMPORT-NEEDED-OBJECTS) @i{omit-other-remarks}@\
Copies library objects which are not stored
in the chosen directory, but are needed by some object in
that directory, into the directory.  If there is a choice
of objects to import, and SHOW-ALL-LIBOBJECTS is set to T,
then the user is prompted to choose one.

@IndexOther(INSERT) @i{name} @i{type}@\
Insert an item in the library. 
The INSERT command can be used to create a new library object or
to modify existing entries in the library.  If SHOW-ALL-LIBOBJECTS
is set to T, the user is prompted to indicate which existing
library object to modify or which library directory into which
the new object should be inserted. If AUTO-KEYWORDS is set to T, 
executing INSERT-LIBOBJECT requires expanding all definitions, 
which can take an enormous amount of time when definitions are 
deeply nested.

All the items will be replaced by whatever you write
(or kept the same if you use the default) except for "additional
remarks"; what you specify here will be added to whatever is already
there. If you don't want to add additional remarks, respond with
<space><return>. Use your favorite editor to make any changes within
the existing comment.

@IndexOther(INSERT-TPTP) @i{source-file} @i{destination-file} @i{suffix}@\
Insert a TPTP Problem into the library. 
The INSERT-TPTP command can be used to create a new library file 
containing abbreviations and theorems from a TPTP formatted file 
(.tps file). The destination directory is set according to the flag
AUTO-LIB-DIR. It will not overwrite or delete any existing items: 
new items need to have different names, hence the suffix.
If AUTO-KEYWORDS is set to T, executing INSERT-TPTP requires 
expanding all definitions, which can take an enormous amount of time 
when definitions are deeply nested.

@IndexOther(INSERT-TPTP*) @i{source-directory}@\
For each TPTP Problem in the source directory, insert 
a new file into the library. The INSERT-TPTP* command can be used 
to create new library files containing abbreviations and theorems 
from a directory of TPTP formatted files (.tps file). The destination
directory is set accordig to the flag AUTO-LIB-DIR. It will not 
overwrite or delete any existing items: new items need to have 
different names.
If AUTO-KEYWORDS is set to T, executing INSERT-TPTP* requires 
expanding all definitions, which can take an enormous amount of time 
when definitions are deeply nested.

@IndexOther(MOVE-LIBOBJECT) @i{name} @i{type} @i{filename}@\
Move an object from one library file to another.
This command will also move a list of objects (either all of the same type, or
all of type NIL), into a single named file.

@IndexOther(REFORMAT) @i{file}@\
Reformat the specified file. Will attempt to load all the objects 
in a given file and then to rewrite that file in the standard library format.
This can be useful if you manually edit your library files a lot and they've 
started to look a little disorganized.
To reformat all files in your directories, use SPRING-CLEAN.

@IndexOther(REINDEX) @i{file} @i{reformat}@\
Reindex and reformat the specified file --- i.e. reconstruct the 
entries in the library master index relating to the objects in a 
particular file (you should only need this if you've been manually editing 
the libindex.rec file and have accidentally lost some entries...), and then
attempt to load and rewrite the file.
To reindex all files in your directories, use SPRING-CLEAN.
If you get an error because of parsing problems, try again but answer
no to "Reformat?" (it is not possible to format a file without 
parsing it).

@IndexOther(REMOVE-GOODMODES) @i{modes-gwffs} @i{rmodes} @i{rthms}@\
Remove modes from a list of goodmodes.  Also, remove theorems that these goodmodes can prove.

@IndexOther(RENAME-OBJECT) @i{name} @i{type} @i{newname}@\
Change the name of a library object. Does not move the object or 
alter it in any other way.

@IndexOther(SORT) @i{file} @i{head}@\
Sort the specified file into alphabetical order, except for the
given list of objects which are put at the head of the file (if they were 
originally in the file). This command reads in the entire file and then
rewrites it; it will incidentally also catch any parsing errors.

@IndexOther(SPRING-CLEAN) @i{reindex} @i{reformat} @i{sort} @i{delete}@\
Will do its best to reindex, reformat and/or sort every file in the 
default library directory. If your files are a real mess, you might consider
using emacs to get rid of the worst of the problems before using SPRING-CLEAN.
It will also delete any file in the directory that doesn't belong there
Generally this means everything except .lib and libindex.rec files; you will be 
asked for confirmation before each file is deleted.
If you get an error because of parsing problems, try again but answer
no to "Reformat?" and "Sort?" (it is not possible to reformat or sort 
a file that cannot be parsed). Better yet, delete the unparseable entry and
try again.@End(Description)

@Section(Keywords)

@Begin(Description)
@IndexOther(ADD-KEYWORD) @i{keyword} @i{defn}@\
Add a keyword to the keywords.rec file in your default directory.
This must be done before the keyword can be used anywhere else in the 
library.

@IndexOther(CHANGE-KEYWORDS) @i{name}@\
Change the keywords attribute of a stored library object.
NOTE: not all keywords can be changed. TPS may modify your list of
keywords -- for example, if you specify FIRST-ORDER for a problem
that is higher-order, TPS will change it.

@IndexOther(SHOW-KEYWORDS)@\
List all of the current acceptable keywords for the library.

@IndexOther(UPDATE-KEYWORDS)@\
For each library entry, update the keywords field
to include all of those keywords that can be determined automatically.
Any other keywords will be left untouched.
If you answer NO to the question about checking existing keywords, then 
this command will just attempt to fill in keywords for those objects 
which have none. If you answer YES, keywords will be generated for all 
of the objects (but existing user-defined  keywords will not be overwritten).

This command will almost certainly crash if it discovers any untypable 
definitions, missing needed-objects, circular definitions, misprints, etc...
in your library.
This probably won't damage your library, but you might want to make
a backup of all your files before you call this, just in case...@End(Description)

@Section(Best modes)

@Begin(Description)
@IndexOther(ADD-BESTMODE) @i{theorem} @i{mode} @i{date} @i{time} @i{comment} @i{auto-test}@\
Add a mode for the specified theorem to the list in your
bestmodes.rec file. If the theorem and mode are already present in 
the list (either in your directory or in another user's), you will
be asked to confirm the creation of a new entry. If they are already 
present in your own directory, you will be given the option of 
overwriting them.

The TEST-INIT command sets the flag TEST-THEOREMS to a collection
of theorems associated with bestmodes.  TPS-TEST uses this list
to perform automatic testing.  ADD-BESTMODE gives you the option 
(using the argument AUTO-TEST) of having TEST-INIT include the 
new theorem/bestmode pair for automatic testing.  (The default 
is to include it.)  If the mode is intended to be used interactively 
(e.g., for a demo), then it should not be included for automatic testing.

See Also: TPS-TEST, TEST-INIT, TEST-THEOREMS

@IndexOther(DELETE-BESTMODE) @i{theorem}@\
Remove an existing entry in your own bestmodes.rec file.
Attempting to remove an entry in another user's bestmode.rec
file will fail.

@IndexOther(FIND-DUP-MODES)@\
List all potential duplicates in the bestmodes.rec file.

@IndexOther(MODIFY-BESTMODE) @i{theorem}@\
Edit an existing entry in the bestmodes.rec file. 
Attempting to modify a read-only mode (i.e. one in another user's
directory) will create a modified copy in your own directory.

@IndexOther(SHOW-BESTMODE) @i{theorem}@\
List all of the current best modes for theorems in the library.
Shows mode name, date, time for proof, and whether the mode is read/write
(in your library) or read-only (in someone else's library).

@IndexOther(SHOW-BESTMODE-THMS)@\
List all of the theorems that have bestmodes in bestmodes.rec files.

@IndexOther(SHOW-NEW-BESTMODES) @i{date}@\
List all of the best modes which have been added since the 
given date. This will search all available bestmodes.rec files,
including those in other people's library directories.

@IndexOther(UPDATE-PROVABILITY)@\
Update the PROVABILITY attribute of all the gwffs for which a 
best mode is known.@End(Description)

@Section(Library Classification)

@Begin(Description)
@IndexOther(CLASSIFY-CLASS) @i{class1} @i{class2}@\
Classifies class1 under class2 within the current library classification scheme.

See Also: UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
GOTO-CLASS, CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(CLASSIFY-ITEM) @i{itemname} @i{classname}@\
Puts the library item into the given class within the
current library classification scheme.  If the item has needed objects,
TPS also offers to classify these.  If the flag CLASS-DIRECTION
is set to UP, the needed objects must be classified in ancestors
of the given class.  If the flag CLASS-DIRECTION is set to DOWN, 
the needed objects must be classified in descendants of the given class.  

See Also: CLASSIFY-CLASS, UNCLASSIFY-CLASS, UNCLASSIFY-ITEM,
GOTO-CLASS, CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(CREATE-CLASS-SCHEME) @i{name} @i{help}@\
Create a classification scheme for the library.  A classification scheme
is a way of organizing library items into a tree (actually 
a directed acyclic graph) of classes.  Each class can have 
classes as children.  Each class has associated libitems.

This classification scheme can itself be saved in the library
and retrieved from the library as an object of type LIBCLASS.

A classification scheme can also be used to access the TPS
library using a Unix-style interface.  Use the command
UNIXLIB to enter the Unix-style top level for the library.

See Also: UNIXLIB, PSCHEMES, CLASS-SCHEME, GOTO-CLASS, CREATE-LIBCLASS, 
CLASSIFY-CLASS, CLASSIFY-ITEM, PCLASS-SCHEME, PCLASS-SCHEME-TREE, 
PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(CREATE-LIBCLASS) @i{name}@\
Creates a new class in the current classification scheme.

See Also: CREATE-CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, GOTO-CLASS, CLASS-SCHEME, PSCHEMES,
FETCH-LIBCLASS, FETCH-LIBCLASS*, PCLASS, PCLASS-SCHEME-TREE, PCLASS-TREE

@IndexOther(FETCH-DOWN) @i{name}@\
Fetches all the library items classified in the current class
and in all the descendents of that class are also fetched.

See Also: CLASS-DIRECTION, FETCH-LIBCLASS*, FETCH-UP, FETCH-LIBCLASS,
CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, GOTO-CLASS, PSCHEMES,
PCLASS, PCLASS-SCHEME-TREE, PCLASS-TREE, CREATE-CLASS-SCHEME

@IndexOther(FETCH-LIBCLASS) @i{name}@\
Fetches all the library items classified in the current class
within the current library classification scheme.

See Also: FETCH-LIBCLASS*, CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, GOTO-CLASS, PSCHEMES,
PCLASS, PCLASS-SCHEME-TREE, PCLASS-TREE, CREATE-CLASS-SCHEME

@IndexOther(FETCH-LIBCLASS*) @i{name}@\
Fetches all the library items classified in the current class
within the current library classification scheme.  If the flag
CLASS-DIRECTION is set to Up, then FETCH-LIBCLASS* also fetches
all the libitems classified in ancestor classes.  If the flag
CLASS-DIRECTION is set to Down, then FETCH-LIBCLASS* also fetches
all the libitems classified in descendant classes.

See Also: FETCH-UP, FETCH-DOWN, FETCH-LIBCLASS, CLASS-DIRECTION,
CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM,
UNCLASSIFY-ITEM, GOTO-CLASS, ROOT-CLASS, PSCHEMES, PCLASS, PCLASS-SCHEME-TREE,
PCLASS-TREE, CREATE-CLASS-SCHEME

@IndexOther(FETCH-UP) @i{name}@\
Fetches all the library items classified in the current class
and in all the ancestors of that class are also fetched.

See Also: SUBCLASS-DIRECTION, FETCH-LIBCLASS*, FETCH-DOWN, FETCH-LIBCLASS,
CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, GOTO-CLASS, PSCHEMES,
PCLASS, PCLASS-SCHEME-TREE, PCLASS-TREE, CREATE-CLASS-SCHEME

@IndexOther(GENERATE-CLASS-SCHEME) @i{name} @i{help}@\
Generate a classification scheme for all abbreviations,
constants, and gwffs.  TPS does some of the work, and prompts the
user to interactively make other choices.

This command can also be used to update an existing class-scheme
by including all library items which are not classified in
the existing class-scheme.

NOTE:  It is best to run this with a fresh core image.  Otherwise,
TPS may confuse items previously fetched from the library with
objects defined in the core TPS image.

@IndexOther(GOTO-CLASS) @i{name}@\
Searches for classes of the given name within the
current library classification scheme.  If one
is found, that class is made the current class.
If several are found, the user is asked to choose.

See Also: CLASS-SCHEME, ROOT-CLASS, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(PCLASS) @i{name}@\
Prints information about the current library class in the current
classification scheme.

See Also: CLASS-SCHEME, CREATE-CLASS-SCHEME, PSCHEMES, 
PCLASS-SCHEME-TREE, PCLASS-TREE, GOTO-CLASS, CLASSIFY-CLASS, 
UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(PCLASS-SCHEME-TREE) @i{name}@\
Prints the classification scheme as a tree starting from the root class.
A list of known classification schemes is printed by PSCHEMES.

See Also: PCLASS, PSCHEMES, PCLASS-TREE, CREATE-CLASS-SCHEME, 
CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM, 
GOTO-CLASS, CLASS-SCHEME, FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(PCLASS-TREE)@\
Prints the current class and its children as a tree.

See Also: PCLASS, PSCHEMES, PCLASS-SCHEME-TREE, CREATE-CLASS-SCHEME, 
CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM, 
GOTO-CLASS, CLASS-SCHEME, FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(PINTERSECT) @i{classnames}@\
Print the objects that are classified in all the specified classes.

See Also: pintersect*

@IndexOther(PINTERSECT*) @i{classnames}@\
Finds and prints the name of all the objects which, for each
specified class, are classified in the class or a 'subclass'.

If CLASS-DIRECTION is set to DOWN, 'subclass' means a descendant class.

If CLASS-DIRECTION is set to UP, 'subclass' means a ancestor class.

See Also: pintersect

@IndexOther(PSCHEMES)@\
Prints a list of Library Classification Schemes in memory.

See Also: CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, GOTO-CLASS,
CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(ROOT-CLASS)@\
Makes the root class of the current library classification scheme
the current class.

See Also: CLASS-SCHEME, GOTO-CLASS.

@IndexOther(UNCLASSIFY-CLASS) @i{class1} @i{class2}@\
Removes class1 from class2 within the current library classification scheme.

See Also: CLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
GOTO-CLASS, CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(UNCLASSIFY-ITEM) @i{itemname} @i{classname}@\
Removes the library item from the given class within the
current library classification scheme.

See Also: CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM,
GOTO-CLASS, CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*@End(Description)
@ChapterPh(Library Objects)
The internal name of this category is 
LIBOBJECT.
A library object can be defined using DEFLIBOBJECT.
Allowable properties are: @t{LIB-PROMPTFN}, @t{LIB-DESCR-READFN}, @t{LIB-ATTR-READFN}, @t{LIB-TPSOBJECT}, @t{LIB-PRINTFN}, @t{MHELP}.

@Section(Miscellaneous)

@Begin(Description)
@IndexOther(ABBR)@\
Saving abbreviations. Abbreviations should be closed wffs. 

@IndexOther(CLASS-SCHEME)@\
Classification Scheme for a library.
A classification scheme is a way of organizing library items into a tree 
(actually a directed acyclic graph) of classes.  Each class can have 
classes as children.  Each class has associated libitems.

To see what classification schemes are available call:
LIST-OF-LIBOBJECTS CLASS-SCHEME
from the lib top level.

See Also: CREATE-CLASS-SCHEME, PSCHEMES, PCLASS-SCHEME-TREE, 
PCLASS-TREE, CREATE-LIBCLASS, CLASSIFY-CLASS, CLASSIFY-ITEM, 
FETCH-LIBCLASS, FETCH-LIBCLASS*

@IndexOther(DPAIRSET)@\
Set of disagreement pairs.

@IndexOther(GWFF)@\
Gwff

@IndexOther(LIB-CONST)@\
Constants and Polymorphic Proper Symbols. 
These are like abbreviations, but will never be expanded
by TPS and hence have no definition.

@IndexOther(MODE)@\
Define a new mode, and save it in the library. Note that you will
    have to explicitly set the all the flag settings that you want to save
    even if the mode already exists in the library. Also see MODE1.

@IndexOther(MODE1)@\
Define a new mode, and save it in the library. All the current flag
    settings for the subjects that you specify will be saved. Also see MODE.

@IndexOther(MODES-GWFFS)@\
A list of 'good' modes.  Generally, this should be a list of
modes which can be used to prove many theorems automatically.
We usually want a list of goodmodes to be 'complete' in the following sense:
For any theorem that has a bestmode, there is some goodmode that proves the
theorem.

SEE ALSO: GOODMODES, TEST-INIT, ADD-GOODMODES, REMOVE-GOODMODES

@IndexOther(RRULE)@\
Rewrite rule

@IndexOther(THEORY)@\
A theory (a set of axioms and rewrite rules).@End(Description)

@Section(Library)

@Begin(Description)
@IndexOther(SLIST)@\
The library object corresponding to a searchlist.@End(Description)
@ChapterPh(Classification Scheme For The Library.s)
The internal name of this category is 
CLASS-SCHEME.
A Classification Scheme for the library. can be defined using DEF-CLASS-SCHEME.
Allowable properties are: @t{CLASS-DIRECTION}, @t{LIBCLASS}.

@Section(Modules)

@Begin(Description)
@IndexOther(LIBDIR)@\
LIBDIR is a classification scheme built based purely on the
directory structure of the library directories in DEFAULT-LIB-DIR
and BACKUP-LIB-DIR.  Other classification schemes may be stored in
and retrieved from the library.

See Also:  UNIXLIB, DEFAULT-LIB-DIR, BACKUP-LIB-DIR@End(Description)
@ChapterPh(Library Command Using A Unix Style Interfaces)
The internal name of this category is 
UNIX-LIBRARYCMD.
A library command using a unix style interface can be defined using DEFUNIXLIBRARY.
Allowable properties are: @t{ULIB-ARGTYPES}, @t{ULIB-ARGNAMES}, @t{ULIB-ARGHELP}, @t{ULIB-DEFAULTFNS}, @t{ULIB-MAINFNS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(LEAVE)@\
No further help available.  Sorry.@End(Description)

@Section(Display)

@Begin(Description)
@IndexOther(FIND-GENERATED-CLASS)@\
No further help available.  Sorry.

@IndexOther(GENERATE-CLASS-SCHEME) @i{name} @i{help}@\
No further help available.  Sorry.

@IndexOther(IMPORT-CLASS)@\
No further help available.  Sorry.

@IndexOther(LOCATE)@\
No further help available.  Sorry.

@IndexOther(LS-ITEMS*)@\
No further help available.  Sorry.

@IndexOther(PDOWN)@\
No further help available.  Sorry.

@IndexOther(PINTERSECT) @i{classnames}@\
No further help available.  Sorry.

@IndexOther(PINTERSECT*) @i{classnames}@\
No further help available.  Sorry.

@IndexOther(PUP)@\
No further help available.  Sorry.

@IndexOther(PWD)@\
No further help available.  Sorry.

@IndexOther(SHOW) @i{name} @i{type}@\
No further help available.  Sorry.

@IndexOther(SHOW-ALL-WFFS) @i{backup} @i{filter}@\
No further help available.  Sorry.

@IndexOther(SHOW-HELP) @i{name} @i{type}@\
No further help available.  Sorry.

@IndexOther(SHOW-WFF) @i{name}@\
No further help available.  Sorry.

@IndexOther(SHOW-WFF&HELP) @i{name}@\
No further help available.  Sorry.@End(Description)

@Section(Reading)

@Begin(Description)
@IndexOther(DESTROY) @i{name}@\
No further help available.  Sorry.

@IndexOther(FETCH) @i{name} @i{type}@\
No further help available.  Sorry.@End(Description)

@Section(Library Classification)

@Begin(Description)
@IndexOther(CD)@\
No further help available.  Sorry.

@IndexOther(CLASSIFY-ITEM) @i{itemname} @i{classname}@\
No further help available.  Sorry.

@IndexOther(COPY-CLASS-SCHEME)@\
No further help available.  Sorry.

@IndexOther(CP)@\
No further help available.  Sorry.

@IndexOther(LN)@\
No further help available.  Sorry.

@IndexOther(LS)@\
No further help available.  Sorry.

@IndexOther(MKDIR)@\
No further help available.  Sorry.

@IndexOther(MV)@\
No further help available.  Sorry.

@IndexOther(RENAME-CLASS)@\
No further help available.  Sorry.

@IndexOther(RM)@\
No further help available.  Sorry.@End(Description)
@ChapterPh(Review Commands)
The internal name of this category is 
REVIEWCMD.
A review command can be defined using DEFREVIEW.
Allowable properties are: @t{ARGTYPES}, @t{ARGNAMES}, @t{ARGHELP}, @t{DEFAULTFNS}, @t{MAINFNS}, @t{CLOSEFNS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexReviewcmd(LEAVE)@\
Leave REVIEW to the next enclosing top level.@End(Description)

@Section(Flags)

@Begin(Description)
@IndexReviewcmd(CHANGED-FLAGS) @i{omit}@\
List all those flags whose current value is not
the default value.

@IndexReviewcmd(DESCRIBE) @i{flag}@\
Describe a flag.

@IndexReviewcmd(DESCRIBE*) @i{subjectlist}@\
List all flags under the subjects requested, along with their descriptions.

@IndexReviewcmd(KEY) @i{phrase} @i{subjectlist} @i{search-names}@\
Look for a key phrase in the help strings (or just the names) 
of flags of given subjects. See also SEARCH, at the main top level.

@IndexReviewcmd(LIST) @i{subjectlist}@\
List all flags in the given subjects with their current value.

@IndexReviewcmd(SAVE-FLAG-RELEVANCY-INFO) @i{filename}@\
Save Flag Relevancy Info built from Lisp Source Files

SEE ALSO:  UPDATE-RELEVANT, SHOW-RELEVANCE-PATHS

@IndexReviewcmd(SET) @i{flag} @i{flag-value}@\
Directly set the value of a flag.

@IndexReviewcmd(SETFLAG) @i{flag}@\
Set the value of a flag after examining it.

@IndexReviewcmd(SETFLAGS1) @i{fvlist}@\
Simultaneously sets multiple flags of the form ((FLAG1 . VALUE1)
(FLAG2 . VALUE2)...) (the dots may be omitted); intended for use
when cutting and pasting records from library or bug files. The opening
and closing parentheses must be supplied.

@IndexReviewcmd(SETFLAGS2) @i{whole}@\
Simultaneously sets multiple flags of the form "FLAG1: VALUE1
FLAG2: VALUE2 ...". Intended for use when cutting and pasting records 
from library or bug files. User must provide double quotes before and 
after pasting the record, and each flag and value pair should be 
separated by a newline. Flag-names containing double quotes must be 
set separately. This command cannot handle such cases.

@IndexReviewcmd(SHOW-RELEVANCE-PATHS) @i{func-or-flag} @i{flag}@\
Given a function F or flag A to start from and a flag B to end at,
show all paths which explain why the flag B should be relevant when F is called
or when the flag A has a certain value.

@IndexReviewcmd(SUBJECTS) @i{show-help}@\
Print a list of currently defined subjects for REVIEW.

@IndexReviewcmd(UPDATE) @i{subjectlist}@\
Update all the flags concerning the given subjects. ! will
leave the remaining flags unchanged.

@IndexReviewcmd(UPDATE-RELEVANT) @i{flag}@\
Update a flag and flags that are known to be relevant to the value given.
For example,

update-relevant DEFAULT-MS

will allow the user to first set DEFAULT-MS.  If the user sets DEFAULT-MS to MS98-1,
then TPS will ask the user to set flags relevant to MS98-1.

When update-relevant is called, the user is given the option of using the current
flag relevancy information in memory, loading flag relevancy information saved to
a file using SAVE-FLAG-RELEVANCY, or rebuilding flag relevancy information from
the Lisp source files.@End(Description)

@Section(Modes)

@Begin(Description)
@IndexReviewcmd(ADD-FLAG-TO-MODE) @i{mode} @i{flag}@\
Add a flag to a mode. The flag will be added with its
current setting. If the flag is already present, its value in the
mode will be changed to its current setting.

@IndexReviewcmd(COMPARE-MODES) @i{mode1} @i{mode2}@\
Compare two different modes; print a list of the values on 
which they differ.

@IndexReviewcmd(COPY-MODE) @i{oldname} @i{newname}@\
Make a copy of a mode, with a new name. To delete the
old mode from memory, use DESTROY.

@IndexReviewcmd(MODE) @i{mode}@\
Set a group of flags by switching to a mode.

@IndexReviewcmd(REMOVE-FLAG-FROM-MODE) @i{mode} @i{flag}@\
Delete a flag from a mode. If the flag is not present
in the mode, this command will do nothing.@End(Description)

@Section(Unification)

@Begin(Description)
@IndexReviewcmd(UNIF-DEPTHS)@\
Turn off all the MAX-SUBSTS checking in unification,
and use only the flags MAX-SEARCH-DEPTH, MAX-UTREE-DEPTH
and MIN-QUICK-DEPTH.

@IndexReviewcmd(UNIF-NODEPTHS)@\
Turn off all the depth checking in unification,
and set the MAX-SUBSTS-VAR and MAX-SUBSTS-QUICK flags.@End(Description)

@Section(Best modes)

@Begin(Description)
@IndexReviewcmd(FIND-MODE) @i{thm}@\
Find a mode from bestmodes.rec for the given theorem,
and (after prompting the user) switch to the selected mode.
This will search all of the bestmodes.rec files which occur
in any of the directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR.@End(Description)
@ChapterPh(Subjects)
The internal name of this category is 
REVIEW-SUBJECT.
A subject can be defined using DEFSUBJECT.
Allowable properties are: @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(EDITOR)@\
Flags concerning the operation of the wff editor.
@begin(format)
blank-lines-inserted@=charsize@>edppwfflag
edprintdepth@=edwin-current@>edwin-current-height
edwin-current-width@=edwin-top@>edwin-top-height
edwin-top-width@=edwin-vpform@>edwin-vpform-height
edwin-vpform-width@=printedtfile@>printedtflag
printedtflag-slides@=printedtops@>printvpdflag
untyped-lambda-calculus
@end(format)

@IndexOther(TEST-TOP)@\
About the test-top top level.
@begin(format)
test-easier-if-high@=test-easier-if-low@>test-easier-if-nil
test-easier-if-t@=test-faster-if-high@>test-faster-if-low
test-faster-if-nil@=test-faster-if-t@>test-fix-unif-depths
test-increase-time@=test-initial-time-limit@>test-max-search-values
test-next-search-fn@=test-reduce-time@>test-verbose
testwin-height@=testwin-width
@end(format)@End(Description)

@Section(OTL Object)

@Begin(Description)
@IndexOther(OTL-VARS)@\
Variables needed by the otlnl (outline) package.
@begin(format)
cleanup-rulec@=cleanup-same@>history-size
print-dots@=printlineflag@>proofw-active
proofw-active+nos@=proofw-active+nos-height@>proofw-active+nos-width
proofw-active-height@=proofw-active-width@>proofw-all
proofw-all-height@=proofw-all-width@>scribe-line-width
short-help@=slides-turnstile-indent@>slides-turnstyle-indent
support-numbers@=tex-line-width@>turnstile-indent
turnstile-indent-auto@=turnstyle-indent@>turnstyle-indent-auto
use-diy
@end(format)

@IndexOther(OUTLINE)@\
Flags having to do with outline manipulations.
@begin(format)
auto-generate-hyps@=default-wffeq@>print-comments
support-numbers
@end(format)@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(PRINTING)@\
About printing wffs.
@begin(format)
allscopeflag@=alpha-lower-flag@>atomvalflag
blank-lines-inserted@=charsize@>displaywff
edppwfflag@=edprintdepth@>edwin-current
edwin-top@=edwin-vpform@>elim-defns
etree-nat-verbose@=fillineflag@>first-order-print-mode
flushleftflag@=infix-notation@>leftmargin
localleftflag@=pagelength@>ppwfflag
print-combined-egens@=print-combined-ugens@>print-combined-uis
print-comments@=print-deep@>print-dots
print-meta@=print-nodenames@>print-until-ui-or-egen
print-weak@=printdepth@>printedtfile
printedtflag@=printedtflag-slides@>printedtops
printlineflag@=printmatefile@>printmateflag
printmateflag-slides@=printmateops@>printtypes
printtypes-all@=proofw-active@>proofw-active+nos
proofw-all@=retain-initial-type@>rightmargin
scope@=scribe-postamble@>scribe-preamble
slides-preamble@=style@>suppress-flags
suppress-flags-list@=suppress-irrelevance-warnings@>turnstile-indent
turnstile-indent-auto@=turnstyle-indent@>turnstyle-indent-auto
use-dot@=use-internal-print-mode
@end(format)

@IndexOther(PRINTING-TEX)@\
About formatting TeX output.
@begin(format)
displaywff@=in-tex-math-mode@>infix-notation
latex-emulation@=latex-postamble@>latex-preamble
pagelength@=pagewidth@>ppwfflag
tex-1-postamble@=tex-1-preamble@>tex-break-before-symbols
tex-mimic-scribe@=tex-postamble@>tex-preamble
tpstex@=turnstile-indent@>turnstile-indent-auto
turnstyle-indent@=turnstyle-indent-auto@>use-internal-print-mode
vpdtex
@end(format)

@IndexOther(WINDOW-PROPS)@\
Properties of windows (e.g., editor, proof windows, vpform windows).
@begin(format)
blank-lines-inserted@=edwin-current-height@>edwin-current-width
edwin-top-height@=edwin-top-width@>edwin-vpform-height
edwin-vpform-width@=etree-nat-verbose@>proofw-active
proofw-active+nos@=proofw-active+nos-height@>proofw-active+nos-width
proofw-active-height@=proofw-active-width@>proofw-all
proofw-all-height@=proofw-all-width@>testwin-height
testwin-width@=use-window-style@>vpw-height
vpw-width@=window-style
@end(format)@End(Description)

@Section(Flavors of Labels)

@Begin(Description)
@IndexOther(INTERNAL-NAMES)@\
Choice of names for flavors of internal labels.
@begin(format)
meta-bdvar-name@=meta-label-name@>meta-var-name
@end(format)@End(Description)

@Section(Saving Work)

@Begin(Description)
@IndexOther(SAVING-WORK)@\
About saving and restoring work.
@begin(format)
save-interval@=save-work-on-start-up@>save-work-p
@end(format)@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexOther(ETREES)@\
Variables associated with expansion trees.
@begin(format)
add-truth@=default-ob@>econj-name
edisj-name@=empty-dup-info-name@>eproof-name
expansion-name@=false-name@>imp-name
lambda-conv@=leaf-name@>mating-name
matingstree-name@=merge-minimize-mating@>min-quant-etree
min-quantifier-scope@=mt-dups-per-quant@>mt94-12-trigger
mtree-filter-dups@=mtree-stop-immediately@>neg-name
print-deep@=print-nodenames@>remove-leibniz
rewrite-name@=selection-name@>skolem-selection-name
true-name@=truthvalues-hack
@end(format)@End(Description)

@Section(Mtree Operations)

@Begin(Description)
@IndexOther(MTREE)@\
Flags concerning matingstree.
@begin(format)
@end(format)

@IndexOther(MTREE-TOP)@\
Flags concerning the operation of the matingstree top level.
@begin(format)
default-expand@=default-mate@>default-ms
default-ob@=matingstree-name@>mt-default-ob-mate
mt-dups-per-quant@=mt-subsumption-check@>mt94-12-trigger
mtree-filter-dups@=mtree-stop-immediately@>tag-conn-fn
tag-mating-fn
@end(format)@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexOther(IMPORTANT)@\
The crucial flags that need to be set for automatic proofs.
@begin(format)
bad-var-connected-prune@=default-ms@>include-coinduction-principle
include-induction-principle@=max-constraint-size@>max-mates
max-num-constraints@=max-prim-depth@>max-prim-lits
max-search-depth@=max-search-limit@>max-substs-quick
max-substs-var@=max-utree-depth@>min-prim-depth
min-prim-lits@=num-of-dups@>order-components
pr00-num-iterations@=pr97c-max-abbrevs@>pr97c-prenex
prim-bdtypes@=prim-bdtypes-auto@>primsub-method
rewrite-defns@=rewrite-equalities@>rewrite-equivs
search-time-limit@=total-num-of-dups@>which-constraints
@end(format)

@IndexOther(MATING-SEARCH)@\
Flags concerning mating search.
@begin(format)
add-truth@=allow-nonleaf-conns@>bad-var-connected-prune
default-expand@=default-mate@>default-ms
dissolve@=dup-allowed@>duplication-strategy
duplication-strategy-pfd@=excluding-gc-time@>first-order-mode-ms
include-coinduction-principle@=include-induction-principle@>initial-bktrack-limit
interrupt-enable@=last-mode-name@>mate-ffpair
mate-up-to-nnf@=mating-verbose@>max-constraint-size
max-dup-paths@=max-mates@>max-num-constraints
max-search-limit@=merge-minimize-mating@>min-quant-etree
min-quantifier-scope@=monitorflag@>ms-dir
ms-init-path@=ms-split@>ms90-3-dup-strategy
ms98-external-rewrites@=ms98-pollute-global-rewrites@>natree-debug
new-mating-after-dup@=num-of-dups@>occurs-check
order-components@=prim-quantifier@>print-mating-counter
printmatefile@=printmateflag@>printmateflag-slides
printmateops@=prop-strategy@>query-user
rank-eproof-fn@=recordflags@>remove-leibniz
rewrite-defns@=rewrite-equalities@>rewrite-equivs
rulep-wffeq@=search-complete-paths@>search-time-limit
show-time@=skolem-default@>timing-named
total-num-of-dups@=truthvalues-hack@>unify-verbose
use-diy@=use-ext-lemmas@>use-fast-prop-search
use-rulep@=use-symsimp@>which-constraints
@end(format)

@IndexOther(TRANSMIT)@\
Flags which should be transmitted from a slave tps to a master tps
when piy2 or diy2 is used.  This is so the appropriate flag values can
be recorded by a daterec after such a run.
@begin(format)
add-truth@=allow-nonleaf-conns@>apply-match
assert-lemmas@=bad-var-connected-prune@>break-at-quantifiers
countsubs-first@=default-expand@>default-mate
default-ms@=default-ob@>default-tactic
delay-setvars@=dissolve@>dneg-imitation
dup-allowed@=duplication-strategy@>duplication-strategy-pfd
eta-rule@=etree-nat-verbose@>ext-search-limit
ff-delay@=first-order-mode-ms@>hpath-threshold
imitation-first@=include-coinduction-principle@>include-induction-principle
initial-bktrack-limit@=last-mode-name@>leibniz-sub-check
mate-ffpair@=mate-up-to-nnf@>mating-verbose
max-constraint-size@=max-dup-paths@>max-mates
max-num-constraints@=max-prim-depth@>max-prim-lits
max-search-depth@=max-search-limit@>max-substs-proj
max-substs-proj-total@=max-substs-quick@>max-substs-var
max-utree-depth@=maximize-first@>measurements
merge-minimize-mating@=min-prim-depth@>min-prim-lits
min-quant-etree@=min-quantifier-scope@>min-quick-depth
ms-dir@=ms-init-path@>ms-split
ms03-dup-method@=ms03-quick-eunification-limit@>ms03-solve-rigid-parts
ms03-solve-rigid-parts-allow-reconnects@=ms03-use-jforms@>ms03-use-set-constraints
ms03-verbose@=ms03-weight-banned-sels@>ms03-weight-change-dups
ms03-weight-disj-eunif@=ms03-weight-disj-mate@>ms03-weight-disj-unif
ms03-weight-dup-var@=ms03-weight-eunif1@>ms03-weight-eunif2
ms03-weight-flexflexdiff@=ms03-weight-flexflexdiff-o@>ms03-weight-flexflexsame
ms03-weight-flexflexsame-o@=ms03-weight-flexrigid-branch@>ms03-weight-flexrigid-eqn
ms03-weight-flexrigid-flexeqn@=ms03-weight-flexrigid-mate@>ms03-weight-flexrigid-noeqn
ms03-weight-flexrigid-o@=ms03-weight-imitate@>ms03-weight-occurs-check
ms03-weight-primsub-falsehood@=ms03-weight-primsub-first-and@>ms03-weight-primsub-first-equals
ms03-weight-primsub-first-exists@=ms03-weight-primsub-first-forall@>ms03-weight-primsub-first-not-equals
ms03-weight-primsub-first-not-proj@=ms03-weight-primsub-first-or@>ms03-weight-primsub-first-proj
ms03-weight-primsub-next-and@=ms03-weight-primsub-next-equals@>ms03-weight-primsub-next-exists
ms03-weight-primsub-next-forall@=ms03-weight-primsub-next-not-equals@>ms03-weight-primsub-next-not-proj
ms03-weight-primsub-next-or@=ms03-weight-primsub-next-proj@>ms03-weight-primsub-truth
ms03-weight-project@=ms03-weight-rigid-mate@>ms03-weight-rigidrigid-eqn
ms03-weight-rigidrigid-flexeqn@=ms03-weight-rigidrigid-noeqn@>ms03-weight-rigidrigiddiff-o
ms03-weight-rigidrigidsame-o@=ms04-allow-flex-eunifs@>ms04-allow-flexrigid-proj-mate
ms04-backtrack-method@=ms04-check-unif-depth@>ms04-delay-flexrigid-mates
ms04-delay-unif-constraints@=ms04-dup-early@>ms04-dup-weight
ms04-eager-unif-subst@=ms04-incr-depth@>ms04-initial-depth
ms04-max-delayed-conns@=ms04-max-depth@>ms04-max-dups
ms04-max-eunif1s@=ms04-max-eunif2s@>ms04-max-flex-eunifs
ms04-max-flexrigid-mates@=ms04-max-flexrigid-neg-mates@>ms04-max-flexrigid-neg-proj-mates
ms04-max-flexrigid-proj-mates@=ms04-max-imits@>ms04-max-primsub-and
ms04-max-primsub-equals@=ms04-max-primsub-exists@>ms04-max-primsub-forall
ms04-max-primsub-not@=ms04-max-primsub-not-equals@>ms04-max-primsub-not-proj
ms04-max-primsub-or@=ms04-max-primsub-proj@>ms04-max-projs
ms04-max-rigid-mates@=ms04-mp-options@>ms04-prenex-primsubs
ms04-semantic-pruning@=ms04-solve-unif-depth@>ms04-trace
ms04-use-semantics@=ms04-use-set-constraints@>ms04-verbose
ms04-weight-add-set-constraint@=ms04-weight-delay-unif@>ms04-weight-eunif-decs
ms04-weight-eunif-diff-heads@=ms04-weight-flex-eunif@>ms04-weight-flexrigid-proj-mate
ms04-weight-multiple-eunif1s@=ms04-weight-multiple-eunif2s@>ms04-weight-multiple-mates
ms04-weight-primsub-first-not@=ms04-weight-primsub-next-not@>ms04-weight-primsub-nexttp
ms04-weight-primsub-occurs-const@=ms04-weight-solve-set-constraints@>ms90-3-dup-strategy
ms90-3-quick@=ms91-interleave@>ms91-prefer-smaller
ms91-time-by-vpaths@=ms91-weight-limit-range@>ms98-base-prim
ms98-dup-below-primsubs@=ms98-dup-primsubs@>ms98-first-fragment
ms98-force-h-o@=ms98-fragment-order@>ms98-init
ms98-low-memory@=ms98-max-components@>ms98-max-prims
ms98-measure@=ms98-merge-dags@>ms98-minimality-check
ms98-num-of-dups@=ms98-primsub-count@>ms98-rew-primsubs
ms98-rewrite-depth@=ms98-rewrite-model@>ms98-rewrite-prune
ms98-rewrite-size@=ms98-rewrite-unif@>ms98-rewrites
ms98-unif-hack@=ms98-unif-hack2@>ms98-use-colors
ms98-valid-pair@=ms98-variable-order@>ms98-verbose
mt-default-ob-mate@=mt-dups-per-quant@>mt-subsumption-check
mt94-12-trigger@=mtree-filter-dups@>mtree-stop-immediately
neg-prim-sub@=new-mating-after-dup@>new-option-set-limit
num-frpairs@=num-of-dups@>occurs-check
options-generate-arg@=options-generate-fn@>options-generate-update
options-verbose@=order-components@>penalty-for-each-primsub
penalty-for-multiple-primsubs@=penalty-for-multiple-subs@>penalty-for-ordinary-dup
pr00-allow-subnode-conns@=pr00-max-substs-var@>pr00-num-iterations
pr00-require-arg-deps@=pr97c-max-abbrevs@>pr97c-prenex
prim-bdtypes@=prim-bdtypes-auto@>prim-quantifier
primsub-method@=primsub-var-select@>print-mating-counter
prop-strategy@=pruning@>query-user
rank-eproof-fn@=reconsider-fn@>reduce-double-neg
remove-leibniz@=rewrite-defns@>rigid-path-ck
rulep-wffeq@=search-complete-paths@>search-time-limit
show-time@=skolem-default@>stop-at-tsn
subsumption-check@=subsumption-depth@>subsumption-nodes
tacmode@=tactic-verbose@>tacuse
total-num-of-dups@=truthvalues-hack@>uni-search-heuristic
unif-counter@=unif-counter-output@>unif-trigger
unify-verbose@=use-diy@>use-ext-lemmas
use-fast-prop-search@=use-rulep@>use-symsimp
weight-a-coefficient@=weight-a-fn@>weight-b-coefficient
weight-b-fn@=weight-c-coefficient@>weight-c-fn
which-constraints
@end(format)@End(Description)

@Section(MS88 search procedure)

@Begin(Description)
@IndexOther(MS88)@\
Flags relevant to the MS88 mating-search procedure.
@begin(format)
default-expand@=default-mate@>default-ms
dup-allowed@=duplication-strategy@>first-order-mode-ms
initial-bktrack-limit@=interrupt-enable@>mate-ffpair
max-dup-paths@=max-mates@>max-prim-depth
max-prim-lits@=merge-minimize-mating@>min-prim-depth
min-prim-lits@=min-quantifier-scope@>ms-dir
ms-init-path@=ms-split@>natree-debug
new-mating-after-dup@=occurs-check@>order-components
pr97c-max-abbrevs@=pr97c-prenex@>prim-quantifier
primsub-method@=prop-strategy@>query-user
remove-leibniz@=rewrite-defns@>rewrite-equalities
rewrite-equivs@=rigid-path-ck@>rulep-wffeq
search-complete-paths@=skolem-default@>unify-verbose
use-rulep@=use-symsimp
@end(format)@End(Description)

@Section(MS89 search procedure)

@Begin(Description)
@IndexOther(MS89)@\
Flags relevant to the MS89 mating-search procedure.
@begin(format)
default-expand@=default-mate@>default-ms
dup-allowed@=first-order-mode-ms@>initial-bktrack-limit
interrupt-enable@=mate-ffpair@>max-dup-paths
max-mates@=max-prim-depth@>max-prim-lits
max-search-limit@=merge-minimize-mating@>min-prim-depth
min-prim-lits@=min-quantifier-scope@>ms-dir
ms-init-path@=ms-split@>ms90-3-dup-strategy
natree-debug@=new-mating-after-dup@>occurs-check
order-components@=pr97c-max-abbrevs@>pr97c-prenex
prim-quantifier@=primsub-method@>prop-strategy
query-user@=rank-eproof-fn@>remove-leibniz
rewrite-defns@=rewrite-equalities@>rewrite-equivs
rigid-path-ck@=rulep-wffeq@>search-complete-paths
search-time-limit@=skolem-default@>unify-verbose
use-rulep@=use-symsimp
@end(format)@End(Description)

@Section(MS90-3 search procedure)

@Begin(Description)
@IndexOther(MS90-3)@\
Flags relevant to the MS90-3 mating-search procedure.
@begin(format)
default-expand@=default-mate@>default-ms
dup-allowed@=duplication-strategy-pfd@>first-order-mode-ms
initial-bktrack-limit@=interrupt-enable@>max-dup-paths
max-mates@=max-prim-depth@>max-prim-lits
merge-minimize-mating@=min-prim-depth@>min-prim-lits
min-quant-etree@=min-quantifier-scope@>ms-init-path
ms90-3-dup-strategy@=ms90-3-quick@>natree-debug
new-mating-after-dup@=num-frpairs@>num-of-dups
order-components@=pr97c-max-abbrevs@>pr97c-prenex
prim-quantifier@=primsub-method@>print-mating-counter
prop-strategy@=query-user@>remove-leibniz
rewrite-defns@=rewrite-equalities@>rewrite-equivs
rigid-path-ck@=rulep-wffeq@>show-time
skolem-default@=total-num-of-dups@>unify-verbose
use-rulep@=use-symsimp
@end(format)@End(Description)

@Section(MS90-9 search procedure)

@Begin(Description)
@IndexOther(MS90-9)@\
Flags relevant to the MS90-9 mating-search procedure.
@begin(format)
default-expand@=default-mate@>default-ms
dup-allowed@=duplication-strategy-pfd@>first-order-mode-ms
initial-bktrack-limit@=interrupt-enable@>max-dup-paths
max-mates@=max-prim-depth@>max-prim-lits
max-search-limit@=merge-minimize-mating@>min-prim-depth
min-prim-lits@=min-quant-etree@>min-quantifier-scope
ms-init-path@=ms90-3-dup-strategy@>ms90-3-quick
natree-debug@=new-mating-after-dup@>num-frpairs
num-of-dups@=order-components@>pr97c-max-abbrevs
pr97c-prenex@=prim-quantifier@>primsub-method
print-mating-counter@=prop-strategy@>query-user
rank-eproof-fn@=remove-leibniz@>rewrite-defns
rewrite-equalities@=rewrite-equivs@>rigid-path-ck
rulep-wffeq@=search-time-limit@>show-time
skolem-default@=unify-verbose@>use-rulep
use-symsimp
@end(format)@End(Description)

@Section(MS91-6 and MS91-7 search procedures)

@Begin(Description)
@IndexOther(MS91-6)@\
Flags relevant to the MS91-6 mating-search procedure.
@begin(format)
default-expand@=default-mate@>default-ms
dup-allowed@=first-order-mode-ms@>initial-bktrack-limit
interrupt-enable@=mate-ffpair@>max-dup-paths
max-mates@=max-prim-depth@>max-prim-lits
max-search-limit@=merge-minimize-mating@>min-prim-depth
min-prim-lits@=min-quantifier-scope@>ms-dir
ms-init-path@=ms-split@>ms91-interleave
ms91-prefer-smaller@=ms91-time-by-vpaths@>ms91-weight-limit-range
natree-debug@=new-mating-after-dup@>new-option-set-limit
occurs-check@=options-generate-arg@>options-generate-fn
options-generate-update@=options-verbose@>order-components
penalty-for-each-primsub@=penalty-for-multiple-primsubs@>penalty-for-multiple-subs
penalty-for-ordinary-dup@=pr97c-max-abbrevs@>pr97c-prenex
prim-quantifier@=primsub-method@>prop-strategy
query-user@=reconsider-fn@>remove-leibniz
rewrite-defns@=rewrite-equalities@>rewrite-equivs
rigid-path-ck@=rulep-wffeq@>search-complete-paths
search-time-limit@=skolem-default@>unify-verbose
use-rulep@=use-symsimp@>weight-a-coefficient
weight-a-fn@=weight-b-coefficient@>weight-b-fn
weight-c-coefficient@=weight-c-fn
@end(format)

@IndexOther(MS91-7)@\
Flags relevant to the MS91-7 mating-search procedure.
@begin(format)
default-expand@=default-mate@>default-ms
dup-allowed@=duplication-strategy-pfd@>first-order-mode-ms
initial-bktrack-limit@=interrupt-enable@>max-dup-paths
max-mates@=max-prim-depth@>max-prim-lits
max-search-limit@=merge-minimize-mating@>min-prim-depth
min-prim-lits@=min-quant-etree@>min-quantifier-scope
ms-init-path@=ms90-3-dup-strategy@>ms90-3-quick
ms91-interleave@=ms91-prefer-smaller@>ms91-time-by-vpaths
ms91-weight-limit-range@=natree-debug@>new-mating-after-dup
new-option-set-limit@=num-frpairs@>num-of-dups
options-generate-arg@=options-generate-fn@>options-generate-update
options-verbose@=order-components@>penalty-for-each-primsub
penalty-for-multiple-primsubs@=penalty-for-multiple-subs@>penalty-for-ordinary-dup
pr97c-max-abbrevs@=pr97c-prenex@>prim-quantifier
primsub-method@=print-mating-counter@>prop-strategy
query-user@=reconsider-fn@>remove-leibniz
rewrite-defns@=rewrite-equalities@>rewrite-equivs
rigid-path-ck@=rulep-wffeq@>search-time-limit
show-time@=skolem-default@>unify-verbose
use-rulep@=use-symsimp@>weight-a-coefficient
weight-a-fn@=weight-b-coefficient@>weight-b-fn
weight-c-coefficient@=weight-c-fn
@end(format)@End(Description)

@Section(MS92-9 search procedure)

@Begin(Description)
@IndexOther(MS92-9)@\
Flags relevant to the MS92-9 mating-search procedure.
@begin(format)
default-expand@=default-mate@>default-ms
dup-allowed@=duplication-strategy-pfd@>first-order-mode-ms
initial-bktrack-limit@=interrupt-enable@>max-dup-paths
max-mates@=max-prim-depth@>max-prim-lits
merge-minimize-mating@=min-prim-depth@>min-prim-lits
min-quant-etree@=min-quantifier-scope@>ms-init-path
ms90-3-dup-strategy@=ms90-3-quick@>natree-debug
new-mating-after-dup@=num-frpairs@>num-of-dups
order-components@=pr97c-max-abbrevs@>pr97c-prenex
prim-quantifier@=primsub-method@>prop-strategy
query-user@=remove-leibniz@>rewrite-defns
rewrite-equalities@=rewrite-equivs@>rigid-path-ck
rulep-wffeq@=show-time@>skolem-default
unify-verbose@=use-rulep@>use-symsimp
@end(format)@End(Description)

@Section(MS93-1 search procedure)

@Begin(Description)
@IndexOther(MS93-1)@\
Flags relevant to the MS93-1 mating-search procedure.
@begin(format)
default-expand@=default-mate@>default-ms
dup-allowed@=duplication-strategy-pfd@>first-order-mode-ms
initial-bktrack-limit@=interrupt-enable@>max-dup-paths
max-mates@=max-prim-depth@>max-prim-lits
max-search-limit@=merge-minimize-mating@>min-prim-depth
min-prim-lits@=min-quant-etree@>min-quantifier-scope
ms-init-path@=ms90-3-dup-strategy@>ms90-3-quick
natree-debug@=new-mating-after-dup@>num-frpairs
num-of-dups@=order-components@>pr97c-max-abbrevs
pr97c-prenex@=prim-quantifier@>primsub-method
prop-strategy@=query-user@>rank-eproof-fn
remove-leibniz@=rewrite-defns@>rewrite-equalities
rewrite-equivs@=rigid-path-ck@>rulep-wffeq
search-time-limit@=show-time@>skolem-default
unify-verbose@=use-rulep@>use-symsimp
@end(format)@End(Description)

@Section(MS98-1 search procedure)

@Begin(Description)
@IndexOther(MS98-1)@\
Pertaining to the component search MS98-1.
@begin(format)
break-at-quantifiers@=default-ms@>first-order-mode-ms
max-mates@=max-substs-quick@>max-substs-var
merge-minimize-mating@=min-quantifier-scope@>ms98-base-prim
ms98-external-rewrites@=ms98-first-fragment@>ms98-fragment-order
ms98-init@=ms98-max-prims@>ms98-measure
ms98-num-of-dups@=ms98-pollute-global-rewrites@>ms98-primsub-count
ms98-rewrite-depth@=ms98-rewrite-size@>ms98-rewrite-unif
ms98-rewrites@=ms98-use-colors@>ms98-verbose
num-of-dups@=rewrite-defns@>rewrite-equalities
rewrite-equivs@=skolem-default
@end(format)

@IndexOther(MS98-MINOR)@\
Less important flags for MS98-1.
@begin(format)
ff-delay@=hpath-threshold@>maximize-first
ms98-dup-below-primsubs@=ms98-dup-primsubs@>ms98-force-h-o
ms98-low-memory@=ms98-max-components@>ms98-merge-dags
ms98-minimality-check@=ms98-rew-primsubs@>ms98-rewrite-model
ms98-rewrite-prune@=ms98-trace@>ms98-unif-hack
ms98-unif-hack2@=ms98-valid-pair@>ms98-variable-order
@end(format)@End(Description)

@Section(Extensional Search)

@Begin(Description)
@IndexOther(EXT-SEARCH)@\
Flags concerning extensional proof search.  These include all flags
relevant to either of the search procedures MS03-7 or MS04-2.
@begin(format)
ext-mate-recompute-jforms@=ext-search-limit@>ms03-dup-method
ms03-quick-eunification-limit@=ms03-solve-rigid-parts@>ms03-solve-rigid-parts-allow-reconnects
ms03-use-jforms@=ms03-use-set-constraints@>ms03-verbose
ms03-weight-banned-sels@=ms03-weight-change-dups@>ms03-weight-disj-eunif
ms03-weight-disj-mate@=ms03-weight-disj-unif@>ms03-weight-dup-var
ms03-weight-eunif1@=ms03-weight-eunif2@>ms03-weight-flexflexdiff
ms03-weight-flexflexdiff-o@=ms03-weight-flexflexsame@>ms03-weight-flexflexsame-o
ms03-weight-flexrigid-branch@=ms03-weight-flexrigid-eqn@>ms03-weight-flexrigid-flexeqn
ms03-weight-flexrigid-mate@=ms03-weight-flexrigid-noeqn@>ms03-weight-flexrigid-o
ms03-weight-imitate@=ms03-weight-occurs-check@>ms03-weight-primsub-falsehood
ms03-weight-primsub-first-and@=ms03-weight-primsub-first-equals@>ms03-weight-primsub-first-exists
ms03-weight-primsub-first-forall@=ms03-weight-primsub-first-not-equals@>ms03-weight-primsub-first-not-proj
ms03-weight-primsub-first-or@=ms03-weight-primsub-first-proj@>ms03-weight-primsub-next-and
ms03-weight-primsub-next-equals@=ms03-weight-primsub-next-exists@>ms03-weight-primsub-next-forall
ms03-weight-primsub-next-not-equals@=ms03-weight-primsub-next-not-proj@>ms03-weight-primsub-next-or
ms03-weight-primsub-next-proj@=ms03-weight-primsub-truth@>ms03-weight-project
ms03-weight-rigid-mate@=ms03-weight-rigidrigid-eqn@>ms03-weight-rigidrigid-flexeqn
ms03-weight-rigidrigid-noeqn@=ms03-weight-rigidrigiddiff-o@>ms03-weight-rigidrigidsame-o
ms04-allow-flex-eunifs@=ms04-allow-flexrigid-proj-mate@>ms04-backtrack-method
ms04-check-unif-depth@=ms04-delay-flexrigid-mates@>ms04-delay-unif-constraints
ms04-dup-early@=ms04-dup-weight@>ms04-eager-unif-subst
ms04-incr-depth@=ms04-initial-depth@>ms04-max-delayed-conns
ms04-max-depth@=ms04-max-dups@>ms04-max-eunif1s
ms04-max-eunif2s@=ms04-max-flex-eunifs@>ms04-max-flexrigid-mates
ms04-max-flexrigid-neg-mates@=ms04-max-flexrigid-neg-proj-mates@>ms04-max-flexrigid-proj-mates
ms04-max-imits@=ms04-max-primsub-and@>ms04-max-primsub-equals
ms04-max-primsub-exists@=ms04-max-primsub-forall@>ms04-max-primsub-not
ms04-max-primsub-not-equals@=ms04-max-primsub-not-proj@>ms04-max-primsub-or
ms04-max-primsub-proj@=ms04-max-projs@>ms04-max-rigid-mates
ms04-mp-options@=ms04-prenex-primsubs@>ms04-semantic-pruning
ms04-solve-unif-depth@=ms04-trace@>ms04-use-semantics
ms04-use-set-constraints@=ms04-verbose@>ms04-weight-add-set-constraint
ms04-weight-delay-unif@=ms04-weight-eunif-decs@>ms04-weight-eunif-diff-heads
ms04-weight-flex-eunif@=ms04-weight-flexrigid-proj-mate@>ms04-weight-multiple-eunif1s
ms04-weight-multiple-eunif2s@=ms04-weight-multiple-mates@>ms04-weight-primsub-first-not
ms04-weight-primsub-next-not@=ms04-weight-primsub-nexttp@>ms04-weight-primsub-occurs-const
ms04-weight-solve-set-constraints
@end(format)

@IndexOther(MS03-7)@\
Flags concerning the proof search procedure MS03-7 which incorporates
extensional reasoning, equality reasoning, and set constraints.  This uses
extensional expansion dags instead of expansion trees.  See Chad E. Brown's thesis.
@begin(format)
default-ms@=ext-search-limit@>ms03-dup-method
ms03-quick-eunification-limit@=ms03-solve-rigid-parts@>ms03-solve-rigid-parts-allow-reconnects
ms03-use-jforms@=ms03-use-set-constraints@>ms03-verbose
ms03-weight-banned-sels@=ms03-weight-change-dups@>ms03-weight-disj-eunif
ms03-weight-disj-mate@=ms03-weight-disj-unif@>ms03-weight-dup-var
ms03-weight-eunif1@=ms03-weight-eunif2@>ms03-weight-flexflexdiff
ms03-weight-flexflexdiff-o@=ms03-weight-flexflexsame@>ms03-weight-flexflexsame-o
ms03-weight-flexrigid-branch@=ms03-weight-flexrigid-eqn@>ms03-weight-flexrigid-flexeqn
ms03-weight-flexrigid-mate@=ms03-weight-flexrigid-noeqn@>ms03-weight-flexrigid-o
ms03-weight-imitate@=ms03-weight-occurs-check@>ms03-weight-primsub-falsehood
ms03-weight-primsub-first-and@=ms03-weight-primsub-first-equals@>ms03-weight-primsub-first-exists
ms03-weight-primsub-first-forall@=ms03-weight-primsub-first-not-equals@>ms03-weight-primsub-first-not-proj
ms03-weight-primsub-first-or@=ms03-weight-primsub-first-proj@>ms03-weight-primsub-next-and
ms03-weight-primsub-next-equals@=ms03-weight-primsub-next-exists@>ms03-weight-primsub-next-forall
ms03-weight-primsub-next-not-equals@=ms03-weight-primsub-next-not-proj@>ms03-weight-primsub-next-or
ms03-weight-primsub-next-proj@=ms03-weight-primsub-truth@>ms03-weight-project
ms03-weight-rigid-mate@=ms03-weight-rigidrigid-eqn@>ms03-weight-rigidrigid-flexeqn
ms03-weight-rigidrigid-noeqn@=ms03-weight-rigidrigiddiff-o@>ms03-weight-rigidrigidsame-o
query-user
@end(format)

@IndexOther(MS04-2)@\
Flags concerning the proof search procedure MS04-2 which incorporates
extensional reasoning, equality reasoning, and set constraints.  This uses
extensional expansion dags instead of expansion trees.  See Chad E. Brown's thesis.
@begin(format)
default-ms@=max-binder-computation@>max-domain-size
ms03-quick-eunification-limit@=ms03-weight-banned-sels@>ms03-weight-eunif1
ms03-weight-eunif2@=ms03-weight-flexflexdiff@>ms03-weight-flexflexdiff-o
ms03-weight-flexflexsame@=ms03-weight-flexflexsame-o@>ms03-weight-flexrigid-branch
ms03-weight-flexrigid-eqn@=ms03-weight-flexrigid-flexeqn@>ms03-weight-flexrigid-mate
ms03-weight-flexrigid-noeqn@=ms03-weight-flexrigid-o@>ms03-weight-imitate
ms03-weight-occurs-check@=ms03-weight-primsub-first-and@>ms03-weight-primsub-first-equals
ms03-weight-primsub-first-exists@=ms03-weight-primsub-first-forall@>ms03-weight-primsub-first-not-equals
ms03-weight-primsub-first-not-proj@=ms03-weight-primsub-first-or@>ms03-weight-primsub-first-proj
ms03-weight-primsub-next-and@=ms03-weight-primsub-next-equals@>ms03-weight-primsub-next-exists
ms03-weight-primsub-next-forall@=ms03-weight-primsub-next-not-equals@>ms03-weight-primsub-next-not-proj
ms03-weight-primsub-next-or@=ms03-weight-primsub-next-proj@>ms03-weight-project
ms03-weight-rigid-mate@=ms03-weight-rigidrigid-eqn@>ms03-weight-rigidrigid-flexeqn
ms03-weight-rigidrigid-noeqn@=ms03-weight-rigidrigiddiff-o@>ms03-weight-rigidrigidsame-o
ms04-allow-flex-eunifs@=ms04-allow-flexrigid-proj-mate@>ms04-backtrack-method
ms04-check-unif-depth@=ms04-delay-flexrigid-mates@>ms04-delay-unif-constraints
ms04-dup-early@=ms04-dup-weight@>ms04-eager-unif-subst
ms04-incr-depth@=ms04-initial-depth@>ms04-max-delayed-conns
ms04-max-depth@=ms04-max-dups@>ms04-max-eunif1s
ms04-max-eunif2s@=ms04-max-flex-eunifs@>ms04-max-flexrigid-mates
ms04-max-flexrigid-neg-mates@=ms04-max-flexrigid-neg-proj-mates@>ms04-max-flexrigid-proj-mates
ms04-max-imits@=ms04-max-primsub-and@>ms04-max-primsub-equals
ms04-max-primsub-exists@=ms04-max-primsub-forall@>ms04-max-primsub-not
ms04-max-primsub-not-equals@=ms04-max-primsub-not-proj@>ms04-max-primsub-or
ms04-max-primsub-proj@=ms04-max-projs@>ms04-max-rigid-mates
ms04-mp-options@=ms04-prenex-primsubs@>ms04-semantic-pruning
ms04-solve-unif-depth@=ms04-trace@>ms04-use-semantics
ms04-use-set-constraints@=ms04-verbose@>ms04-weight-add-set-constraint
ms04-weight-delay-unif@=ms04-weight-eunif-decs@>ms04-weight-eunif-diff-heads
ms04-weight-flex-eunif@=ms04-weight-flexrigid-proj-mate@>ms04-weight-multiple-eunif1s
ms04-weight-multiple-eunif2s@=ms04-weight-multiple-mates@>ms04-weight-primsub-first-not
ms04-weight-primsub-next-not@=ms04-weight-primsub-nexttp@>ms04-weight-primsub-occurs-const
ms04-weight-solve-set-constraints
@end(format)@End(Description)

@Section(Proof Translation)

@Begin(Description)
@IndexOther(ETR-NAT)@\
Pertaining to the translation from expansion tree proofs to
natural deduction proofs.
@begin(format)
assert-lemmas@=etree-nat-verbose@>lambda-conv
merge-minimize-mating@=nat-etree-version@>pseq-use-labels
remove-leibniz@=use-diy@>use-rulep
use-symsimp
@end(format)@End(Description)

@Section(Unification)

@Begin(Description)
@IndexOther(UNIFICATION)@\
Variables associated with Unification
@begin(format)
apply-match@=countsubs-first@>dneg-imitation
eta-rule@=imitation-first@>leibniz-sub-check
max-search-depth@=max-substs-proj@>max-substs-proj-total
max-substs-quick@=max-substs-var@>max-utree-depth
min-quick-depth@=ms03-weight-banned-sels@>ms03-weight-eunif1
ms03-weight-eunif2@=ms03-weight-flexflexdiff@>ms03-weight-flexflexdiff-o
ms03-weight-flexflexsame@=ms03-weight-flexflexsame-o@>ms03-weight-flexrigid-branch
ms03-weight-flexrigid-eqn@=ms03-weight-flexrigid-flexeqn@>ms03-weight-flexrigid-mate
ms03-weight-flexrigid-noeqn@=ms03-weight-flexrigid-o@>ms03-weight-imitate
ms03-weight-occurs-check@=ms03-weight-project@>ms03-weight-rigid-mate
ms03-weight-rigidrigid-eqn@=ms03-weight-rigidrigid-flexeqn@>ms03-weight-rigidrigid-noeqn
ms03-weight-rigidrigiddiff-o@=ms03-weight-rigidrigidsame-o@>ms04-weight-flex-eunif
ms04-weight-flexrigid-proj-mate@=ms90-3-quick@>num-frpairs
pr00-max-substs-var@=pruning@>reduce-double-neg
rigid-path-ck@=stop-at-tsn@>subsumption-check
subsumption-depth@=subsumption-nodes@>uni-search-heuristic
unif-counter@=unif-counter-output@>unif-trigger
unify-verbose
@end(format)@End(Description)

@Section(Tactics)

@Begin(Description)
@IndexOther(TACTICS)@\
Flags concerning tactics.
@begin(format)
default-tactic@=lambda-conv@>tacmode
tactic-verbose@=tacuse@>ui-herbrand-limit
use-diy@=use-rulep@>use-symsimp
@end(format)@End(Description)

@Section(suggestions)

@Begin(Description)
@IndexOther(SUGGESTS)@\
About SUGGESTIONS and GO.
@begin(format)
go-instructions@=quietly-use-defaults@>resolve-conflict
@end(format)@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexOther(JFORMS)@\
Variables associated with jforms.
@begin(format)
lit-name@=order-components@>print-lit-name
printvpdflag@=renumber-leaves@>rulep-wffeq
texformat@=vpd-brief@>vpd-filename
vpd-lit-name@=vpd-ptypes@>vpd-style
vpd-vpfpage@=vpform-labels@>vpform-tex-magnification
vpform-tex-nest@=vpform-tex-preamble@>vpw-height
vpw-width
@end(format)@End(Description)

@Section(Semantics)

@Begin(Description)
@IndexOther(SEMANTIC-BOUNDS)@\
Bounds related to models
@begin(format)
max-binder-computation@=max-domain-size
@end(format)@End(Description)

@Section(wff Primitives)

@Begin(Description)
@IndexOther(WFF-PRIMS)@\
Flags for wff primitives, not related to parsing or printing.
@begin(format)
name-skolem-fn@=ren-var-fn@>rename-all-bd-vars
rewrite-equalities
@end(format)@End(Description)

@Section(Wff Parsing)

@Begin(Description)
@IndexOther(PARSING)@\
About parsing wffs.
@begin(format)
base-type@=first-order-mode-parse@>lowercaseraise
make-wffops-labels@=type-iota-mode
@end(format)@End(Description)

@Section(Primitive Substitutions)

@Begin(Description)
@IndexOther(PRIMSUBS)@\
Variables associated with primitive substitutions.
@begin(format)
bad-var-connected-prune@=delay-setvars@>include-coinduction-principle
include-induction-principle@=max-constraint-size@>max-num-constraints
max-prim-depth@=max-prim-lits@>min-prim-depth
min-prim-lits@=ms03-use-set-constraints@>ms03-weight-primsub-falsehood
ms03-weight-primsub-first-and@=ms03-weight-primsub-first-equals@>ms03-weight-primsub-first-exists
ms03-weight-primsub-first-forall@=ms03-weight-primsub-first-not-equals@>ms03-weight-primsub-first-not-proj
ms03-weight-primsub-first-or@=ms03-weight-primsub-first-proj@>ms03-weight-primsub-next-and
ms03-weight-primsub-next-equals@=ms03-weight-primsub-next-exists@>ms03-weight-primsub-next-forall
ms03-weight-primsub-next-not-equals@=ms03-weight-primsub-next-not-proj@>ms03-weight-primsub-next-or
ms03-weight-primsub-next-proj@=ms03-weight-primsub-truth@>ms04-prenex-primsubs
ms04-weight-primsub-first-not@=ms04-weight-primsub-next-not@>ms04-weight-primsub-nexttp
ms04-weight-primsub-occurs-const@=ms91-interleave@>neg-prim-sub
pr00-allow-subnode-conns@=pr00-max-substs-var@>pr00-num-iterations
pr00-require-arg-deps@=pr97c-max-abbrevs@>pr97c-prenex
prim-bdtypes@=prim-bdtypes-auto@>prim-prefix
prim-quantifier@=primsub-method@>primsub-var-select
which-constraints
@end(format)@End(Description)

@Section(Events)

@Begin(Description)
@IndexOther(EVENTS)@\
Dealing with EVENTS.
@begin(format)
added-conn-enabled@=advice-asked-enabled@>advice-file
command-enabled@=command-file@>considered-conn-enabled
done-exc-enabled@=dupe-enabled@>dupe-var-enabled
error-enabled@=error-file@>event-cycle
events-enabled@=incomp-mating-enabled@>input-error-enabled
input-error-file@=mate-subsumed-test-enabled@>mate-subsumed-true-enabled
mating-changed-enabled@=primsub-enabled@>proof-action-enabled
proof-file@=quiet-events@>rec-ms-file
rec-ms-filename@=removed-conn-enabled@>rule-error-enabled
rule-error-file@=score-file@>start-time-enabled
stop-time-enabled@=unif-subsumed-test-enabled@>unif-subsumed-true-enabled
user-passwd-file
@end(format)@End(Description)

@Section(Grader)

@Begin(Description)
@IndexOther(GR-FILENAMES)@\
Files used by the grading package.
@begin(format)
etps-file@=grade-dir@>grade-file
letter-grade-file@=old-grade-file@>old-totals-grade-file
patch-file@=totals-grade-file
@end(format)

@IndexOther(GR-MISC)@\
Miscellaneous variables associated with the grading package.
@begin(format)
cal-percentage@=course-name@>default-penalty-fn
drop-min@=due-date-flag@>letter-grade-flag
new-item@=print-n-digits@>statistical-options
@end(format)@End(Description)

@Section(Maintenance)

@Begin(Description)
@IndexOther(MAINTAIN)@\
Flags useful for system maintainers
@begin(format)
compiled-extension@=completion-options@>diy2-init-time-limit
diy2-num-iterations@=diy2-time-increase-factor@>expertflag
goodmodes@=history-size@>init-dialogue
init-dialogue-fn@=java-comm@>load-warn-p
news-dir@=omdoc-aut-creator@>omdoc-catalogue
omdoc-rights@=omdoc-source@>omdoc-trc-creator
omdoc-type@=read-lload-sources-p@>save-file
show-all-packages@=source-extension@>source-path
test-modify@=test-theorems
@end(format)

@IndexOther(SYSTEM)@\
Flags containing system constants.
@begin(format)
excluding-gc-time@=lisp-implementation-type@>machine-instance
machine-type@=short-site-name@>timing-named
xterm-ansi-bold
@end(format)@End(Description)

@Section(Rules object)

@Begin(Description)
@IndexOther(RULES-MOD)@\
Flags having to do with the operation of the rules module.
@begin(format)
@end(format)@End(Description)

@Section(Library)

@Begin(Description)
@IndexOther(LIBRARY)@\
About the library facility.
@begin(format)
add-subdirectories@=auto-keywords@>auto-lib-dir
backup-lib-dir@=class-direction@>class-scheme
default-bug-dir@=default-lib-dir@>default-libfile-type
default-libindex-type@=elim-defns@>lib-bestmode-file
lib-keyword-file@=lib-masterindex-file@>measurements
recordflags@=remove-trailing-dir@>show-all-libobjects
use-default-bug-dir
@end(format)@End(Description)
@ChapterPh(Flag Or Parameters)
The internal name of this category is 
FLAG.
A flag or parameter can be defined using DEFFLAG%.
Allowable properties are: @t{FLAGTYPE}, @t{DEFAULT}, @t{PRE-CHANGE-FN}, @t{CHANGE-FN}, @t{SUBJECTS}, @t{RELEVANCY-PRECONDITIONS}, @t{IRRELEVANCY-PRECONDITIONS}, @t{RELEVANT-KIDS}, @t{IRRELEVANT-KIDS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexFlag(EXT-MATE-RECOMPUTE-JFORMS)@\
If T, JForms are eagerly recomputed after modifications are made to
extensional expansion dags in the EXT-MATE top level.  Otherwise, the
user must use the command CJFORM to update the JForm.  Even if the
value is T, CJFORM is useful for obtaining special JForms where
Flex-Flex or Flexible nodes are left out.
It takes values of type BOOLEAN and belongs to subjects @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MT-DUPS-PER-QUANT)@\
The maximum number of times that each individual quantifier
may be duplicated in the MATINGSTREE search procedures. This flag is 
overridden by NUM-OF-DUPS, which governs the maximum total number of 
duplications of all quantifiers in the matingstree search.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{ETREES}, @t{MTREE-TOP}.  The default value is @T{INFINITY}.

@IndexFlag(PROOFW-ACTIVE)@\
If T, active lines of the current proof are printed in the 
Current Subproof window, if this window exists.
It takes values of type BOOLEAN and belongs to subjects @t{WINDOW-PROPS}, @t{PRINTING}, @t{OTL-VARS}.  The default value is @T{T}.

@IndexFlag(PROOFW-ACTIVE+NOS)@\
If T, active lines of the current proof are printed in the 
Current Subproof & Line Numbers window, if this window exists.
It takes values of type BOOLEAN and belongs to subjects @t{WINDOW-PROPS}, @t{PRINTING}, @t{OTL-VARS}.  The default value is @T{T}.

@IndexFlag(PROOFW-ACTIVE+NOS-HEIGHT)@\
Controls the initial height of the Current Subproof & Line Numbers
window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{OTL-VARS}.  The default value is @T{24}.

@IndexFlag(PROOFW-ACTIVE+NOS-WIDTH)@\
Controls the initial width of the Current Subproof & Line Numbers
window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{OTL-VARS}.  The default value is @T{80}.

@IndexFlag(PROOFW-ACTIVE-HEIGHT)@\
Controls the initial height of the Current Subproof window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{OTL-VARS}.  The default value is @T{24}.

@IndexFlag(PROOFW-ACTIVE-WIDTH)@\
Controls the initial width of the Current Subproof window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{OTL-VARS}.  The default value is @T{80}.

@IndexFlag(PROOFW-ALL)@\
If T, entire proof so far is printed in the Complete Proof 
window, if this window exists.
It takes values of type BOOLEAN and belongs to subjects @t{WINDOW-PROPS}, @t{PRINTING}, @t{OTL-VARS}.  The default value is @T{T}.

@IndexFlag(PROOFW-ALL-HEIGHT)@\
Controls the initial height of the Complete Proof window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{OTL-VARS}.  The default value is @T{24}.

@IndexFlag(PROOFW-ALL-WIDTH)@\
Controls the initial width of the Complete Proof window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{OTL-VARS}.  The default value is @T{80}.

@IndexFlag(UNIXLIB-SHOWPATH)@\
If T, print the current class as a directory in the prompt
in the Unix Style Library Top Level.

If the value is T, the prompt will be
<<CLASSSCHEME>:<PATH TO CLASS><num>>

If the value is NIL, the prompt will be
<LIB:<CLASS><num>>

See Also: UNIXLIB, PSCHEMES, CLASS-SCHEME, CD, LS, PWD, LN, RM,
MKDIR, FETCH, SHOW
It takes values of type BOOLEAN and belongs to subjects The default value is @T{T}.
@End(Description)

@Section(Style)

@Begin(Description)
@IndexFlag(STYLE)@\
The style of the terminal output device.
It takes values of type DEV-STYLE and belongs to subjects @t{PRINTING}.  The default value is @T{GENERIC}.
@End(Description)

@Section(Review)

@Begin(Description)
@IndexFlag(ALPHA-LOWER-FLAG)@\
If T, output from ? will be made more readable
(alphabetized, smaller left margin, mostly lower case)
If NIL, output is in the old style (non-alphabetized,
large left margin, mostly block capitals).
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(LAST-MODE-NAME)@\
LAST-MODE-NAME contains the name of the last MODE used. There
is no point in the user's altering its value, since TPS only ever 
writes to it, and never reads from it.
It takes values of type STRING and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}.  The default value is @T{""}.
@End(Description)

@Section(Flags)

@Begin(Description)
@IndexFlag(SUPPRESS-IRRELEVANCE-WARNINGS)@\
If SUPPRESS-IRRELEVANCE-WARNINGS is T, TPS does not warn when the user
sets a flag that has no effect given the current settings of other flags.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.
@End(Description)

@Section(Modes)

@Begin(Description)
@IndexFlag(SUPPRESS-FLAGS)@\
If T, will suppress the printing of any flags in SUPPRESS-FLAGS-LIST
by the HELP MODE, COMPARE-MODES, LIST, DESCRIBE*, UPDATE and CHANGED-FLAGS
commands.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(SUPPRESS-FLAGS-LIST)@\
If SUPPRESS-FLAGS is T, these flags will not be printed.
SUPPRESS-FLAGS-LIST itself is always suppressed, because it's very large.
It takes values of type TPSFLAGLIST and belongs to subjects @t{PRINTING}.  The default value is @T{()}.
@End(Description)

@Section(Help)

@Begin(Description)
@IndexFlag(SHOW-ALL-PACKAGES)@\
Determines whether ENVIRONMENT will show symbols in all packages
or merely accessible symbols.
It takes values of type BOOLEAN and belongs to subjects @t{MAINTAIN}.  The default value is @T{NIL}.
@End(Description)

@Section(Collecting Help)

@Begin(Description)
@IndexFlag(OMDOC-AUT-CREATOR)@\
The aut creator listed in metadata of TPS omdoc files.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{"The TPS Project"}.

@IndexFlag(OMDOC-CATALOGUE)@\
The omdoc catalogue location.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{"../logics/catalogue.omdoc"}.

@IndexFlag(OMDOC-RIGHTS)@\
The rights listed in metadata of TPS omdoc files.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{"The formalization can be freely distributed, maintaining reference to the TPS source."}.

@IndexFlag(OMDOC-SOURCE)@\
The source listed in metadata of TPS omdoc files.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{"The TPS library: http://gtps.math.cmu.edu/tps.html"}.

@IndexFlag(OMDOC-TRC-CREATOR)@\
The trc creator listed in metadata of TPS omdoc files.
If this is the empty string, the userid is used.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{""}.

@IndexFlag(OMDOC-TYPE)@\
The type listed in metadata of TPS omdoc files.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{"Dataset"}.
@End(Description)

@Section(Starting and Finishing)

@Begin(Description)
@IndexFlag(COMPLETION-OPTIONS)@\
If T, then the user will be offered a choice between
multiple completions of a command. Also, the commands offered 
will come from the current top level, the main top level and the 
flags.
If NIL, command completion will try first the current top level,
then the main top level, and then the flags, and will fail if the 
first of these which contains any completions also contains
multiple completions.
For example (when T)
<1>displ& 

3 matching commands or flags have been found.
1) DISPLAYFILE
2) DISPLAY-TIME
3) DISPLAYWFF
4) None of these.
Input a number between 1 and 4: [1]>

(when NIL)
<2>displ& 
TPS error while reading.
Multiple completions for DISPL: DISPLAYFILE DISPLAY-TIME 
It takes values of type BOOLEAN and belongs to subjects @t{MAINTAIN}.  The default value is @T{T}.

@IndexFlag(HISTORY-SIZE)@\
Maximum number of commands to save. If NIL, all commands
will be saved.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{MAINTAIN}, @t{OTL-VARS}.  The default value is @T{25}.
@End(Description)

@Section(OTL Object)

@Begin(Description)
@IndexFlag(ASSERT-RRULES)@\
When T, PROVE adds to the asserted line the active rewrite
rules as equational premises.
It takes values of type BOOLEAN and belongs to subjects @t{OTL-OBJECT}.  The default value is @T{NIL}.

@IndexFlag(AUTO-GENERATE-HYPS)@\
If T, hypotheses for lines computed and filled in automatically,
if NIL, the user will be asked for confirmation for each set of hypotheses.
It takes values of type BOOLEAN and belongs to subjects @t{OUTLINE}.  The default value is @T{T}.

@IndexFlag(CLEANUP-RULEC)@\
If T, cleanup-same works on lines with multiple-line justifications.
It takes values of type BOOLEAN and belongs to subjects @t{OTL-VARS}.  The default value is @T{T}.

@IndexFlag(CLEANUP-SAME)@\
If NIL, identical lines are not replaced when doing CLEANUP.
It takes values of type BOOLEAN and belongs to subjects @t{OTL-VARS}.  The default value is @T{T}.

@IndexFlag(DEFAULT-WFFEQ)@\
The name of the functions which checks for equality of wffs.
It takes values of type SYMBOL and belongs to subjects @t{OUTLINE}.  The default value is @T{WFFEQ-AB}.

@IndexFlag(PRINT-DOTS)@\
If nil, ... are not printed before a plan line.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{OTL-VARS}.  The default value is @T{T}.

@IndexFlag(PRINTLINEFLAG)@\
If nil, lines in the proof outline are not printed.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{OTL-VARS}.  The default value is @T{T}.

@IndexFlag(SHORT-HELP)@\
If T, only the rule specification will be shown when asking for help
on a rule, and the command format of a command will not be shown.
It takes values of type BOOLEAN and belongs to subjects @t{OTL-VARS}.  The default value is @T{NIL}.
@End(Description)

@Section(Printing)

@Begin(Description)
@IndexFlag(PRINT-COMBINED-EGENS)@\
When set to t, the commands PBRIEF and EXPLAIN will combine
lines which are a sequence of existential generalizations and print a
single line.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(PRINT-COMBINED-UGENS)@\
When set to t, the commands PBRIEF and EXPLAIN will combine
lines which are a sequence of universal generalizations and print a
single line.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(PRINT-COMBINED-UIS)@\
When set to t, the commands PBRIEF and EXPLAIN will combine
lines which are a sequence of universal instantiations and print a
single line.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(PRINT-UNTIL-UI-OR-EGEN)@\
When set to t, the commands PBRIEF and EXPLAIN will
continue to print beyond the depth specified until a line justified
by UI or Egen is encountered.  The intuition is that these are
the real choice points in the proof.  When set to nil, PBRIEF 
and EXPLAIN print only to the depth specified.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.
@End(Description)

@Section(Printing)

@Begin(Description)
@IndexFlag(ALLSCOPEFLAG)@\
If T, all brackets will be printed; no implicit scoping is assumed.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(ATOMVALFLAG)@\
If T, the name of every atom will be printed below its value.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(BLANK-LINES-INSERTED)@\
Number of blank lines printed in the proofwindows between different
stages of each proof.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{PRINTING}, @t{EDITOR}.  The default value is @T{24}.

@IndexFlag(CHARSIZE)@\
Should be one of MIN, MED or MAX. 
Determines the size of characters used by Proofwindows and Editor Windows.
Currently, MIN and MED are the same size.
It takes values of type SYMBOL and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{MED}.

@IndexFlag(DISPLAYWFF)@\
If T, formulas are printed on separate lines.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING-TEX}, @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(ELIM-DEFNS)@\
When printing a wff, first instantiate all of the definitions
and lambda-normalize. This instantiation will ignore REWRITE-DEFNS, but 
will use the current setting of REWRITE-EQUALITIES.
It's best to leave this at NIL (i.e. off), since output with it set to T can 
be confusing.
It takes values of type BOOLEAN and belongs to subjects @t{LIBRARY}, @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(FILLINEFLAG)@\
If NIL, every argument of an associative infix operator will have a
separate line.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(FIRST-ORDER-PRINT-MODE)@\
If T, formulas are printed so they can be parsed when 
FIRST-ORDER-MODE-PARSE is set to T.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(FLUSHLEFTFLAG)@\
Currently this flag does nothing.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(LEFTMARGIN)@\
The global left margin of the terminal in characters.
It takes values of type INTEGER+ and belongs to subjects @t{PRINTING}.  The default value is @T{0}.

@IndexFlag(LOCALLEFTFLAG)@\
If T, arguments of infix operators start in the same column as
the operator.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(PPWFFLAG)@\
If T, formulas will generally be pretty-printed
(except for the editor).  For pretty-printing to work properly,
the flag INFIX-NOTATION must be set to T.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING-TEX}, @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(PRINTDEPTH)@\
If 0, all printing will be done to arbitrary recursive depth,
if n > 0 subformulas of depth n will be replaced by '&'.
It takes values of type INTEGER+ and belongs to subjects @t{PRINTING}.  The default value is @T{0}.

@IndexFlag(PRINTTYPES)@\
If NIL, type symbols will never be printed.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(PRINTTYPES-ALL)@\
This flag only applies when the flag PRINTTYPES is T.
If PRINTTYPES-ALL is NIL, type symbols will be printed only on the first 
occurrence of a variable name. If it is T, type symbols will be printed on
every occurrence of a variable name.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(RETAIN-INITIAL-TYPE)@\
If T, type property is inherited from the previous occurrence (if any)
of the logical symbols. Else, it is modified whenever the parser encounters
a fresh occurrence.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(RIGHTMARGIN)@\
The global right margin of the terminal in characters.

See Also:  PAGEWIDTH
It takes values of type INTEGER+ and belongs to subjects @t{PRINTING}.  The default value is @T{79}.

@IndexFlag(SCOPE)@\
If T, all wffs will be enclosed in square brackets.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.

@IndexFlag(SLIDES-PREAMBLE)@\
The preamble that is printed into the first lines of all 
the Scribe slides files produced by TPS. See also SCRIBE-PREAMBLE.
It takes values of type STRING and belongs to subjects @t{PRINTING}.  The default value is @T{""}.

@IndexFlag(USE-DOT)@\
If T, formulas are printed using Church's dot notation.
If NIL, only brackets will be used.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(USE-INTERNAL-PRINT-MODE)@\
If T, the internally-defined modes SCRIBE-OTL,
TEX-OTL and TEX-1-OTL will be used for printing Scribe and
TeX output. (See the help message for TEX-MIMIC-SCRIBE for 
help on the difference between the last two.)
These are usually good enough, but if you want to use a 
custom-defined flag setting, then set this flag to NIL to
override the internal modes.  This may cause problems,
in which case set this flag back to T.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING-TEX}, @t{PRINTING}.  The default value is @T{NIL}.
@End(Description)

@Section(Internal for Printing)

@Begin(Description)
@IndexFlag(INFIX-NOTATION)@\
If T, infix notation can be used for connectives and
abbreviations which have an INFIX property. If NIL, infix
notation is disallowed. (Note: If you set this to NIL, 
library objects saved with infix notation will become 
unreadable.  Also, if you set this to NIL, you should
also set PPWFFLAG to NIL since pretty-printing will 
not work properly without using infix notation.)
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING-TEX}, @t{PRINTING}.  The default value is @T{T}.
@End(Description)

@Section(TeX)

@Begin(Description)
@IndexFlag(IN-TEX-MATH-MODE)@\
If T, $'s will not be printed around wffs in style TeX.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{NIL}.

@IndexFlag(LATEX-EMULATION)@\
If T, all of the printing commands that produce TeX output
will produce output suitable for LaTeX instead. See LATEX-PREAMBLE,
LATEX-POSTAMBLE.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{NIL}.

@IndexFlag(PAGELENGTH)@\
Number of lines on an output page.  Used by printing routines to
determine where to break output.
It takes values of type POSINTEGER and belongs to subjects @t{PRINTING-TEX}, @t{PRINTING}.  The default value is @T{55}.

@IndexFlag(PAGEWIDTH)@\
Width of a page.  When creating a TeX file, RIGHTMARGIN gets
temporarily set to this value.

See Also: RIGHTMARGIN
It takes values of type POSINTEGER and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{85}.

@IndexFlag(TEX-BREAK-BEFORE-SYMBOLS)@\
A list of symbols that TeX will allow linebreaks before (when the
flags PPWFFLAG and DISPLAYWFF are NIL).  The command TEXPROOF already allows
line breaks before logical constants, quantifiers, abbreviations and
infix constants.

Users normally don't need to change this flag.
It takes values of type SYMBOLLIST and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{()}.

@IndexFlag(TEX-MIMIC-SCRIBE)@\
If T, TEXPROOF will give a good-looking tex output.
If NIL, TEXPROOF cannot break formulas in terms of the connectives in it.
So the output is a little bit ugly. Change the flag into NIL only when you
cannot get a good-looking output by setting it to T.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{T}.
@End(Description)

@Section(X Windows)

@Begin(Description)
@IndexFlag(USE-WINDOW-STYLE)@\
If T, uses the style given by WINDOW-STYLE for output
to windows other than the main one. If NIL, windows will all be
in the style given by STYLE.
It takes values of type BOOLEAN and belongs to subjects @t{WINDOW-PROPS}.  The default value is @T{T}.

@IndexFlag(WINDOW-STYLE)@\
The style of output that will be used in all the windows
besides the main one, if USE-WINDOW-STYLE is T. Ignored if
USE-WINDOW-STYLE is NIL.
It takes values of type DEV-STYLE and belongs to subjects @t{WINDOW-PROPS}.  The default value is @T{XTERM}.

@IndexFlag(XTERM-ANSI-BOLD)@\
The number corresponding to the ANSI code for switching to bold font.
The default is 53 (ASCII for character 5) which corresponds to blink (often displayed as bold).
An alternative is 49 (ASCII for character 1) which is the ANSI standard for bold.

Further information is contained in the User's Manual and Programmer's Guide.
It takes values of type INTEGER+ and belongs to subjects @t{SYSTEM}.  The default value is @T{53}.
@End(Description)

@Section(Weak Labels)

@Begin(Description)
@IndexFlag(PRINT-WEAK)@\
If T, weak labels are printed, otherwise they wff the represent
will be printed.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{T}.
@End(Description)

@Section(Flavors of Labels)

@Begin(Description)
@IndexFlag(MAKE-WFFOPS-LABELS)@\
If T, meta labels are created by the parser, if NIL, wffops are
evaluated at parse-time.
It takes values of type BOOLEAN and belongs to subjects @t{PARSING}.  The default value is @T{NIL}.

@IndexFlag(META-LABEL-NAME)@\
The prefix for names of meta labels (from wffops).
It takes values of type SYMBOL and belongs to subjects @t{INTERNAL-NAMES}.  The default value is @T{ML}.

@IndexFlag(PRINT-META)@\
If T, meta labels are printed, otherwise the wffop they represent
will be printed.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}.  The default value is @T{NIL}.
@End(Description)

@Section(Saving Work)

@Begin(Description)
@IndexFlag(SAVE-INTERVAL)@\
Interval of file-write of saved commands.
It takes values of type INTEGER+ and belongs to subjects @t{SAVING-WORK}.  The default value is @T{5}.

@IndexFlag(SAVE-WORK-ON-START-UP)@\
If T, work is saved automatically whenever TPS3 is started. 
It takes values of type BOOLEAN and belongs to subjects @t{SAVING-WORK}.  The default value is @T{NIL}.

@IndexFlag(SAVE-WORK-P)@\
If T, work is saved automatically.
It takes values of type BOOLEAN and belongs to subjects @t{SAVING-WORK}.  The default value is @T{T}.
@End(Description)

@Section(Recording)

@Begin(Description)
@IndexFlag(PRINTEDTFILE)@\
The name of the file in which wffs are recorded.
It takes values of type FILESPEC and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{"edt.mss"}.

@IndexFlag(PRINTEDTFLAG)@\
If T, editor operations are recorded into open transcript files.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{NIL}.

@IndexFlag(PRINTEDTFLAG-SLIDES)@\
If T, editor operations are recorded in slides style. This flag has 
no effect unless PRINTEDTFLAG is T.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{NIL}.

@IndexFlag(PRINTEDTOPS)@\
The function or name of the function which test whether the
result of a particular edop should be written to a file.
It takes values of type ANYTHING and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{ALWAYS-TRUE}.

@IndexFlag(PRINTMATEFILE)@\
The name of the file in which mateops are recorded. This has not 
yet been implemented, although one can record remarks (only) into the 
file.
It takes values of type FILESPEC and belongs to subjects @t{PRINTING}, @t{MATING-SEARCH}.  The default value is @T{"mate.mss"}.

@IndexFlag(PRINTMATEFLAG)@\
If T, mating-search operations are recorded into open transcript files.
Not currently implemented.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(PRINTMATEFLAG-SLIDES)@\
If T, mating-search operations are recorded in slides style. This flag has 
no effect unless PRINTMATEFLAG is T. (In fact, it has no effect even if 
PRINTMATEFLAG is T, since it hasn't been implemented.)
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(PRINTMATEOPS)@\
The function or name of the function which test whether the
result of a particular mateop should be written to a file. This has not
been implemented.
It takes values of type ANYTHING and belongs to subjects @t{PRINTING}, @t{MATING-SEARCH}.  The default value is @T{ALWAYS-TRUE}.
@End(Description)

@Section(Printing Proofs into Files)

@Begin(Description)
@IndexFlag(LATEX-POSTAMBLE)@\
The standard way in which TPS will end a TeX file
when LATEX-EMULATION is T.
It takes values of type STRING and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{"\\end{document}"}.

@IndexFlag(LATEX-PREAMBLE)@\
The preamble that is printed into the beginning of all TeX 
files produced by TPS when LATEX-EMULATION is T.
It takes values of type STRING and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{"\\documentclass{article}
\\setlength{\\parindent}{0pt}
\\topmargin 0in
\\footskip 0pt
\\textheight 8.5in
\\oddsidemargin 0in
\\evensidemargin 0pt
\\textwidth 7in 
\\def\\endf{\\end{document}}
\\input /afs/andrew/mcs/math/TPS/doc/lib/tps.sty
\\input /afs/andrew/mcs/math/TPS/doc/lib/tps.tex
\\input /afs/andrew/mcs/math/TPS/doc/lib/vpd.tex
\\newcommand{\\markhack}[1]{\\vspace*{-0.6in}{#1}\\vspace*{0.35in}\\markright{{#1}}}
%a hack to get us a fake header on page 1 without having to do begin{titlepage} ~ end{titlepage}
\\begin{document}
"}.

@IndexFlag(SCRIBE-LINE-WIDTH)@\
Width of a proofline in characters.
It takes values of type INTEGER+ and belongs to subjects @t{OTL-VARS}.  The default value is @T{75}.

@IndexFlag(SCRIBE-POSTAMBLE)@\
The postamble that is printed into all Scribe files
immediately before they are closed by TPS. See SCRIBE-PREAMBLE.
It takes values of type STRING and belongs to subjects @t{PRINTING}.  The default value is @T{""}.

@IndexFlag(SCRIBE-PREAMBLE)@\
The preamble that is printed into the first lines of all 
the Scribe files produced by TPS, except those that are in SLIDES
style. See also SLIDES-PREAMBLE, TEX-PREAMBLE.
It takes values of type STRING and belongs to subjects @t{PRINTING}.  The default value is @T{""}.

@IndexFlag(TEX-1-POSTAMBLE)@\
Another TeX postamble, used when TEX-MIMIC-SCRIBE is T.
See TEX-POSTAMBLE.
It takes values of type STRING and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{"\\vfill\\eject\\end"}.

@IndexFlag(TEX-1-PREAMBLE)@\
Another TeX preamble, used when TEX-MIMIC-SCRIBE is T. 
See TEX-PREAMBLE.
It takes values of type STRING and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{"\\parindent=0pt
"}.

@IndexFlag(TEX-LINE-WIDTH)@\
width of a proofline in characters.
It takes values of type INTEGER+ and belongs to subjects @t{OTL-VARS}.  The default value is @T{75}.

@IndexFlag(TEX-POSTAMBLE)@\
The standard way in which TPS will end a TeX file.
See TEX-PREAMBLE, TEX-1-POSTAMBLE.
It takes values of type STRING and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{"\\eject\\end"}.

@IndexFlag(TEX-PREAMBLE)@\
The preamble that is printed into the beginning of all TeX 
files produced by TPS. See also VPFORM-TEX-PREAMBLE, TEX-1-PREAMBLE, 
TEX-POSTAMBLE.
It takes values of type STRING and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{""}.

@IndexFlag(TPSTEX)@\
The pathname of the tps.tex file on your system. Should 
be initialized by the tps3.ini file.
It takes values of type STRING and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{""}.

@IndexFlag(VPDTEX)@\
The pathname of the vpd.tex file on your system. Should 
be initialized by the tps3.ini file.
It takes values of type STRING and belongs to subjects @t{PRINTING-TEX}.  The default value is @T{""}.
@End(Description)

@Section(Proof Outline)

@Begin(Description)
@IndexFlag(PRINT-COMMENTS)@\
If T, print the comments attached to lines
and proofs. See LINE-COMMENT and PROOF-COMMENT.
It takes values of type BOOLEAN and belongs to subjects @t{OUTLINE}, @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(SLIDES-TURNSTILE-INDENT)@\
Number of columns (from leftmargin) that turnstile should be
indented when making slides. Compare TURNSTILE-INDENT.
This flag and SLIDES-TURNSTYLE-INDENT are synonymous.
It takes values of type INTEGER+ and belongs to subjects @t{OTL-VARS}.  The default value is @T{4}.

@IndexFlag(SLIDES-TURNSTYLE-INDENT)@\
Number of columns (from leftmargin) that turnstile should be
indented when making slides. Compare TURNSTYLE-INDENT.
This flag and SLIDES-TURNSTILE-INDENT are synonymous.
It takes values of type INTEGER+ and belongs to subjects @t{OTL-VARS}.  The default value is @T{4}.

@IndexFlag(SUPPORT-NUMBERS)@\
This has three possible settings:
GAP: new support lines will be put in the gap between the 
current planned line and the previous line, whatever it is.
PLAN: new support lines will be put immediately after the
previous (lower-numbered) planned line, if there is one 
(and as for NIL if there isn't).
NIL (or anything else): new support lines will be put in 
whatever seems to be a sensible place.

This flag may well be useless (although non-NIL values
will force it to do the right thing, TPS will probably
do the right thing anyway).
It takes values of type SYMBOL and belongs to subjects @t{OUTLINE}, @t{OTL-VARS}.  The default value is @T{NIL}.

@IndexFlag(TURNSTILE-INDENT)@\
Number of columns (from leftmargin) that turnstile should be 
indented when writing proofs in a SCRIBE file. Notice that slides use 
a different flag, SLIDES-TURNSTILE-INDENT.
This flag and TURNSTYLE-INDENT are synonymous.
It takes values of type INTEGER+ and belongs to subjects @t{OTL-VARS}, @t{PRINTING}, @t{PRINTING-TEX}.  The default value is @T{13}.

@IndexFlag(TURNSTILE-INDENT-AUTO)@\
Decides how turnstiles are printed in proofs. This flag works in
all styles other than TEX; in particular, it works in XTERM, GENERIC, 
SCRIBE and SLIDES styles. There are four possible settings:
FIX : put the turnstile in the column indicated by TURNSTYLE-INDENT 
      (or SLIDES-TURNSTYLE-INDENT, in style SLIDES).
MIN : print the turnstile as far to the left as possible while still having
      it in the same column on every line.  (If this puts it off the right 
      margin, then this will default to the same behaviour as FIX.)
COMPRESS : similar to VARY, but also removes spaces at other points in the
           proof (e.g. around dots, and between line numbers and hypotheses).
VARY : print the turnstile one space after the hypotheses in each line
       (so it will move from line to line).
It takes values of type INDENTATION and belongs to subjects @t{OTL-VARS}, @t{PRINTING}, @t{PRINTING-TEX}.  The default value is @T{VARY}.

@IndexFlag(TURNSTYLE-INDENT)@\
Number of columns (from leftmargin) that turnstile should be 
indented when writing proofs in a SCRIBE file or on the screen. Notice 
that slides use a different flag, SLIDES-TURNSTYLE-INDENT.
This flag and TURNSTILE-INDENT are synonymous.
It takes values of type INTEGER+ and belongs to subjects @t{PRINTING-TEX}, @t{PRINTING}, @t{OTL-VARS}.  The default value is @T{13}.

@IndexFlag(TURNSTYLE-INDENT-AUTO)@\
Decides how turnstiles are printed in proofs. This flag works in
all styles other than TEX; in particular, it works in XTERM, GENERIC, 
SCRIBE and SLIDES styles. There are four possible settings:
FIX : put the turnstile in the column indicated by TURNSTYLE-INDENT 
      (or SLIDES-TURNSTYLE-INDENT, in style SLIDES).
MIN : print the turnstile as far to the left as possible while still having
      it in the same column on every line. (If this puts it off the right 
      margin, then this will default to the same behaviour as FIX.)
COMPRESS : similar to VARY, but also removes spaces at other points in the
           proof (e.g. around dots, and between line numbers and hypotheses).
VARY : print the turnstile one space after the hypotheses in each line
       (so it will move from line to line).
It takes values of type INDENTATION and belongs to subjects @t{PRINTING-TEX}, @t{PRINTING}, @t{OTL-VARS}.  The default value is @T{VARY}.
@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexFlag(ADD-TRUTH)@\
When set to IF-NEEDED, tests whether the etree has any path of 
length 1; if it does, then adds a conjunct TRUTH to the vpform.
When set to T, it will always add this conjunct.
When set to NIL, it will never add this conjunct.
(When TRUTHVALUES-HACK is NIL, it will also add a conjunct NOT FALSEHOOD).
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}, @t{ETREES}.  The default value is @T{IF-NEEDED}.

@IndexFlag(DUPLICATION-STRATEGY)@\
The name of a duplication strategy.  Currently, either
DUP-ALL or DUP-OUTER. Only applies to MS88.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{DUP-OUTER}.

@IndexFlag(DUPLICATION-STRATEGY-PFD)@\
The name of a duplication strategy for path-focused procedures.
It may have either of two values: DUP-INNER and DUP-OUTER. DUP-INNER
means inner quantifiers get duplicated before outer ones, while DUP-OUTER
means vice versa.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MATING-SEARCH}.  The default value is @T{DUP-INNER}.

@IndexFlag(ECONJ-NAME)@\
Prefix for labels associated with conjunction nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{CONJ}.

@IndexFlag(EDISJ-NAME)@\
Prefix for labels associated with disjunction nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{DISJ}.

@IndexFlag(EMPTY-DUP-INFO-NAME)@\
Prefix for labels associated with empty-dup-info nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{EMP}.

@IndexFlag(EPROOF-NAME)@\
Prefix for names of expansion proofs.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{EPR}.

@IndexFlag(EXPANSION-NAME)@\
Prefix for labels associated with expansion nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{EXP}.

@IndexFlag(FALSE-NAME)@\
Prefix for labels associated with FALSEHOOD nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{FALSE}.

@IndexFlag(IMP-NAME)@\
Prefix for labels associated with implication nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{IMP}.

@IndexFlag(INITIAL-BKTRACK-LIMIT)@\
Initial backtrack limit.  If a mating exceeds this limit, a new
    mating will be started, and the limit incremented. If the value of the 
    flag is set to INFINITY, then this will never happen.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{INFINITY}.

@IndexFlag(LEAF-NAME)@\
Prefix for labels associated with leaf nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{LEAF}.

@IndexFlag(MATING-NAME)@\
Prefix for names of matings.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{MAT}.

@IndexFlag(MIN-QUANTIFIER-SCOPE)@\
When this flag is T, the scope of quantifiers is minimized before
    starting expansion proofs.
If an eproof is found with this flag set to T, during the translation
of the eproof to an ND proof RULEQ is called to fill the gap between
the theorem as originally stated and its min-quantifier-scope version.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}, @t{ETREES}.  The default value is @T{NIL}.

@IndexFlag(NEG-NAME)@\
Prefix for labels associated with negation nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{NEG}.

@IndexFlag(PRINT-DEEP)@\
T will print the deep formula of an expansion or selection node,
NIL will print the shallow formula, both only if PRINT-NODENAMES is NIL.
It takes values of type BOOLEAN and belongs to subjects @t{ETREES}, @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(PRINT-NODENAMES)@\
T will print the names of expansion and selection nodes,
NIL will print either the deep or shallow formula of the node.
(see the flag PRINT-DEEP).
It takes values of type BOOLEAN and belongs to subjects @t{ETREES}, @t{PRINTING}.  The default value is @T{T}.

@IndexFlag(PSEQ-USE-LABELS)@\
Set to T if pseq should abbreviate formulas and print a legend.
It takes values of type BOOLEAN and belongs to subjects @t{ETR-NAT}.  The default value is @T{T}.

@IndexFlag(REWRITE-DEFNS)@\
A list whose first element is one of NONE, EAGER, LAZY1 and DUAL,
and whose other (optional) elements are lists whose first element is
one of these four options and whose other elements are the names of 
definitions.
The first element is the default behaviour for rewriting definitions,
and the other lists are lists of exceptions to this default, with a
different behaviour specified.
NONE:  do not rewrite this definition at all.
EAGER: rewrite all of these definitions, in one big step, as soon 
       as possible.
LAZY1: rewrite these, one step at a time, when there are no more
       EAGER rewrites to do.
DUAL: as LAZY1, but rewrite these abbreviations A to a conjunction of
       A and A, and then deepen only one of these conjuncts. (e.g.
       TRANSITIVE p becomes 
       TRANSITIVE p AND FORALL x y z . [pxy AND pyz] IMPLIES pxz
LAZY2: synonym for DUAL.

For example: the value
(EAGER)
would be interpreted as "Rewrite every definition in one step."

(DUAL (EAGER TRANSITIVE) (NONE INJECTIVE SURJECTIVE))
would be interpreted as "Rewrite TRANSITIVE whenever it appears.
Don't ever rewrite INJECTIVE or SURJECTIVE. Rewrite every other 
definition in the DUAL way."
It takes values of type REWRITE-DEFNS-LIST and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{IMPORTANT}, @t{MATING-SEARCH}.  The default value is @T{(EAGER)}.

@IndexFlag(REWRITE-NAME)@\
Prefix for labels associated with rewrite nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{REW}.

@IndexFlag(SELECTION-NAME)@\
Prefix for labels associated with selection nodes (in a 
non-skolem etree).
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{SEL}.

@IndexFlag(SHOW-SKOLEM)@\
When true, skolem terms are shown when a wff containing them
is printed, otherwise a parameter is printed instead.
It takes values of type BOOLEAN and belongs to subjects The default value is @T{NIL}.

@IndexFlag(SKOLEM-DEFAULT)@\
Default method for skolemizing, in which wffs of the form
EXISTS y . M are replaced by M(g(...)). There are three possible ways to
do this:
SK1 is the original method due to Skolem, where the Skolem constants
 g take as arguments all the x such that FORALL x occurs in the wff 
 and EXISTS y . M is in its scope.
SK3 is the method in which the arguments of g are the free variables
 of EXISTS y . M.
NIL means don't Skolemize at all; use selection nodes instead.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{SK1}.

@IndexFlag(SKOLEM-SELECTION-NAME)@\
Prefix for labels associated with selection nodes (in a 
skolem etree).
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{SKOL}.

@IndexFlag(TRUE-NAME)@\
Prefix for labels associated with TRUTH nodes.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}.  The default value is @T{TRUE}.

@IndexFlag(TRUTHVALUES-HACK)@\
When this flag is T, leaves of truthvalues will not deepened into
an empty disjunction or an empty conjunction. this allows us to deal with 
truthvalues in formulas, especially, higher-order formulas. In order to deal 
with truthvalues in definitions, such as NULLSET, the definitions containing 
falsehood should be rewritten. Please put new definitions containing falsehood
into truthvalues-hack-updatelist so that they can be rewritten appropriately.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}, @t{ETREES}.  The default value is @T{NIL}.
@End(Description)

@Section(Mtree Operations)

@Begin(Description)
@IndexFlag(DEFAULT-OB)@\
If DEEPEST, the default next obligation is found by depth-first
search of the obtree, if HIGHEST it is found by breadth-first-search, 
if D-SMALLEST then the deepest of the set of smallest obligations (i.e.
the set of all obligations with the fewest possible literals) is 
chosen, if H-SMALLEST then the highest of this set is chosen.
It takes values of type OBDEFAULT and belongs to subjects @t{TRANSMIT}, @t{ETREES}, @t{MTREE-TOP}.  The default value is @T{D-SMALLEST}.

@IndexFlag(MT-DEFAULT-OB-MATE)@\
Determines how ADD-CONN chooses the default obligation for
the second literal of the given pair (it is possible that this literal
will occur several times on the path, in several different obligations).
Options are:
LOWEST : Chooses the obligation which lies lowest (i.e. furthest from the
root)
HIGHEST : Chooses the obligation nearest to the root (but not the root).
HI-LO : Finds the obligation which occurs lowest; this obligation was 
first added at some point in the matingstree. Then chooses the highest 
obligation which was added at the same point in the matingstree.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MTREE-TOP}.  The default value is @T{LOWEST}.
@End(Description)

@Section(Mtree Auto)

@Begin(Description)
@IndexFlag(MT-SUBSUMPTION-CHECK)@\
If SAME-CONNS or T, will check whether the node about to be added is
duplicated elsewhere in the tree, and will reject it if it is. (This will use
the SAME-TAG function described below, and then do a more thorough check if the 
tags match.)

If SUBSET-CONNS, will check whether the connections at the node about to be
added are a subset of those at some other node. (This is only really useful in
MT94-11, where all possible new nodes are added, breadth-first, to the tree.
It is probably too restrictive for the other mtree searches.)

If SAME-TAG will check whether the tag (an integer generated from the list of 
connections) is the same as any other existing tag, and will reject it if it is.
See TAG-CONN-FN and TAG-LIST-FN. (Note that most tag functions can produce the
same tag for different matings, so this may reject connections unnecessarily.)

If NIL, will turn off subsumption checking altogether.
It takes values of type MT-SUBSUMPTION and belongs to subjects @t{TRANSMIT}, @t{MTREE-TOP}.  The default value is @T{SAME-CONNS}.

@IndexFlag(MT94-12-TRIGGER)@\
If the current obligation contains fewer than MT94-12-TRIGGER 
literals, MT94-12 will behave in the same way as MT94-11
If it contains MT94-12-TRIGGER or more, MT94-12 will choose a literal
with as few mates as possible. There are two extrema: infinity 
means that the least branch will only be chosen if the obligation
is as big as the initial obligation; 0 means that the least branch 
will always be chosen.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{ETREES}, @t{MTREE-TOP}.  The default value is @T{INFINITY}.

@IndexFlag(MTREE-FILTER-DUPS)@\
If T, will not add the same link to a mating twice on
the same branch of a matingstree during automatic search. If NIL,
will add it as many times as it wants to.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{ETREES}, @t{MTREE-TOP}.  The default value is @T{T}.

@IndexFlag(MTREE-STOP-IMMEDIATELY)@\
If T, will stop an automatic search as soon as a closed 
node is found. If NIL, will continue to generate whatever level of
the tree it was working on, and will check for closed nodes when
it finishes.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{ETREES}, @t{MTREE-TOP}.  The default value is @T{T}.

@IndexFlag(TAG-CONN-FN)@\
Determines how the tag (a number attached to each mating) is calculated.
Should be the name of a function which, given a connection, will generate an
integer from it. See MT-SUBSUMPTION-CHECK and TAG-MATING-FN.

Current settings are 
TAG-CONN-QUICK, which uses TPS's internal number for the connection. (Actually,
it uses (1 + this number), so as to avoid multiplying by one.)
TAG-CONN-LEAFNO, which multiplies the integer parts of the two leaf names
in the given connection.
It takes values of type SYMBOL and belongs to subjects @t{MTREE-TOP}.  The default value is @T{TAG-CONN-LEAFNO}.

@IndexFlag(TAG-MATING-FN)@\
Determines how the tags for each connection are combined to produce
a tag for the entire mating.
Should be the name of a function which, given two integers, will generate a
third integer. See MT-SUBSUMPTION-CHECK and TAG-MATING-FN.

Current settings are 
MULTIPLY-TAG-LIST, which simply multiplies the numbers together.
It takes values of type SYMBOL and belongs to subjects @t{MTREE-TOP}.  The default value is @T{MULTIPLY-TAG-LIST}.
@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexFlag(ASSERT-LEMMAS)@\
If this is set to T, Lemmas are justified in the natural deduction 
proofs using an Assert.  The Assert gives the name of the proof of the Lemma.

Lemmas may be introduced in the following circumstances:

. when extensionality is used (USE-EXT-LEMMAS must be set to T)
. when set variables are solved instantiated using constraints
  (DELAY-SETVARS must be set to T)

If lemmas L1, . . ., Ln are used to prove A, then the full proof
consists of proofs of each of the Li and a proof of A using the
lemmas Li.  In other words, it is a proof of

[L1 and . . . and Ln] and [[L1 and . . . and Ln] implies A]
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{ETR-NAT}.  The default value is @T{T}.

@IndexFlag(DEFAULT-EXPAND)@\
Used with DEFAULT-MATE to determine a setting for DEFAULT-MS.
Combinations marked N/A will result in DEFAULT-MS being set to NIL.
Notice that for otree and oset searches, the actual primsubs generated will
depend on the setting of PRIMSUB-METHOD.
Takes values: none, ms98-1, ms03-7, ms04-2, otree and oset.
The values MS98-1, MS03-7 and MS04-2 are exceptional settings used for both this flag and 
DEFAULT-MATE to denote the MS98-1, MS03-7 and MS04-2 procedures.
Changes DEFAULT-MS as follows:
        DEFAULT-EXPAND:        |  NONE   |  OTREE  |  OSET  |
       ========================+=========+=========+========+
DEFAULT-MATE:          NPFD    |  MS88   |   MS89  | MS91-6 |
                 --------------+---------+---------+--------+
                      NPFD-1   | MS92-9  |  MS93-1 |  N/A   |
                 --------------+---------+---------+--------+
                        PFD    | MS90-3  |  MS90-9 | MS91-7 |
                 --------------+---------+---------+--------+
                      MTREE    | MT94-11 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-1   | MT94-12 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-2   | MT95-1  |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MTREE-TOP}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{OTREE}.

@IndexFlag(DEFAULT-MATE)@\
Used with DEFAULT-EXPAND to determine a setting for DEFAULT-MS.
Combinations marked N/A will result in DEFAULT-MS being set to NIL.
(Notice that for otree and oset searches, the actual primsubs generated will
depend on the setting of PRIMSUB-METHOD.)
Takes values: ms98-1, ms03-7, ms04-2, npfd, npfd-1, pfd, mtree, mtree-1 and mtree-2.
The values MS98-1, MS03-7 and MS04-2 are exceptional settings used for both this flag and 
DEFAULT-EXPAND to denote the MS98-1, MS03-7 and MS04-2 procedures.
Changes DEFAULT-MS as follows:
        DEFAULT-EXPAND:        |  NONE   |  OTREE  |  OSET  |
       ========================+=========+=========+========+
DEFAULT-MATE:          NPFD    |  MS88   |   MS89  | MS91-6 |
                 --------------+---------+---------+--------+
                      NPFD-1   | MS92-9  |  MS93-1 |  N/A   |
                 --------------+---------+---------+--------+
                        PFD    | MS90-3  |  MS90-9 | MS91-7 |
                 --------------+---------+---------+--------+
                      MTREE    | MT94-11 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-1   | MT94-12 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-2   | MT95-1  |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MTREE-TOP}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{PFD}.

@IndexFlag(DEFAULT-MS)@\
The default mating search procedure to be used when either the DIY command or the
mate level GO command is invoked. This will be changed if you set the 
DEFAULT-MATE and DEFAULT-EXPAND flags (they may also change DEFAULT-MS 
to NIL, if you pick a non-existent combination -- see the help messages
for those flags). Conversely, setting DEFAULT-MS will set the values 
of DEFAULT-MATE and DEFAULT-EXPAND, as follows:
(Notice that for otree and oset searches, the actual primsubs generated will
depend on the setting of PRIMSUB-METHOD.)
        DEFAULT-EXPAND:        |  NONE   |  OTREE  |  OSET  |
       ========================+=========+=========+========+
DEFAULT-MATE:          NPFD    |  MS88   |   MS89  | MS91-6 |
                 --------------+---------+---------+--------+
                      NPFD-1   | MS92-9  |  MS93-1 |  N/A   |
                 --------------+---------+---------+--------+
                        PFD    | MS90-3  |  MS90-9 | MS91-7 |
                 --------------+---------+---------+--------+
                      MTREE    | MT94-11 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-1   | MT94-12 |   N/A   |  N/A   |
                 --------------+---------+---------+--------+
                     MTREE-2   | MT95-1  |   N/A   |  N/A   |
                 --------------+---------+---------+--------+

(Setting DEFAULT-MS to MS98-1, MS03-7 or MS04-2 will also set both DEFAULT-EXPAND and DEFAULT-MATE
to MS98-1, MS03-7 or MS04-2, since those procedures don't really fit into the above table.)
Possible values are MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, 
MS93-1, MT94-11, MT94-12, MT95-1, MS98-1, MS03-7 and MS04-2.
It takes values of type SEARCHTYPE and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MTREE-TOP}, @t{MS04-2}, @t{MS03-7}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{MS90-3}.

@IndexFlag(DIY2-INIT-TIME-LIMIT)@\
Initial time limit for running DIY2 and PIY2 iteratively with 
increasing time limits.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{MAINTAIN}.  The default value is @T{2}.

@IndexFlag(DIY2-NUM-ITERATIONS)@\
Number of iterations for DIY2 and PIY2 to run on the same mode with 
increasing time limits.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{MAINTAIN}.  The default value is @T{1}.

@IndexFlag(DIY2-TIME-INCREASE-FACTOR)@\
Factor to increase time limit on each iteration when running 
DIY2 and PIY2.
It takes values of type POSNUMBER and belongs to subjects @t{MAINTAIN}.  The default value is @T{2}.

@IndexFlag(INTERRUPT-ENABLE)@\
When true, allows user to interrupt mating search by typing
a <RETURN>; otherwise mating search will continue until it succeeds
or is aborted by a CTRL-G.  You may want to set this flag to nil
if you are going to have input commands (examples to run, etc.) read
in from a file.
It takes values of type BOOLEAN and belongs to subjects @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(MATING-VERBOSE)@\
Should be one of SILENT, MIN, MED, or MAX.  Determines the amount of
information given about the current mating process.
It takes values of type VERBOSE and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}.  The default value is @T{MED}.

@IndexFlag(MONITORFLAG)@\
The monitor is switched on if this flag is T and off if it is NIL.
This flag is set by the command MONITOR, and unset by the command NOMONITOR
(and may of course also be set manually).
It takes values of type BOOLEAN and belongs to subjects @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(NEW-MATING-AFTER-DUP)@\
This flag affects the way a complete mating is constructed after
    duplication.  If nil, mating search attempts to extend only those matings
    which were inextensible earlier. Otherwise, it starts constructing new
    matings.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(QUERY-USER)@\
Has the following effects according to its value:
T :  User will be queried by the mating search process as to whether 
  a duplication of variables should occur, unification
  depth should be increased, etc.
NIL :  The mating search process will take some action that makes sense.
QUERY-JFORMS : The mating search process will stop after printing each 
vpform and ask whether to search on this vpform or to generate another.
(Note: in MS90-3, this is pointless, since the vpform never changes.)
SHOW-JFORMS : Like QUERY-JFORMS, but automatically answers no to each 
question (and hence never actually proceeds with a search).
QUERY-SLISTS : In the TEST top level, stops after each setting of the
flags and asks whether to search with those settings.
It takes values of type QUERYTYPE and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(REC-MS-FILE)@\
If true, mating search events are recorded in file named by flag
rec-ms-filename. This only works for npfd procedures MS88, MS89 and MS91-6.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{NIL}.

@IndexFlag(REC-MS-FILENAME)@\
Name of file in which mating search events are recorded. (See
REC-MS-FILE.)
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"mating.rec"}.

@IndexFlag(USE-DIY)@\
When T, proof lines which are proven by DIY, DIY-L or UNIFORM-SEARCH-L 
will not be translated into natural deduction style, but will instead be 
justified in a single step, as "Automatic" from the support lines. 
A comment will be added to the relevant line of the proof showing the
time taken and the mode used for the automatic proof.

Obviously, ND proofs containing justifications of this sort cannot be translated by
NAT-ETREE.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{OTL-VARS}, @t{TACTICS}, @t{MATING-SEARCH}, @t{ETR-NAT}.  The default value is @T{NIL}.

@IndexFlag(USE-EXT-LEMMAS)@\
If this is set to T, then diy finds all positive and
negative literals which have a proper subterm of propositional,
set, or relation types.  For example, the jform may have a positive
literal P X(OA) and a negative literal P Y(OA).  For each pair
of subterms such as X and Y, extensionality lemmas of the form

    forall x [X x EQUIV Y x] implies X = Y

are added to the expansion tree before beginning mating search.
Note that the type A is determined by the types of the
subterms X and Y.

See Also:  ADD-EXT-LEMMAS
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(USE-FAST-PROP-SEARCH)@\
If T, will attempt to use the path-focused fast propositional
theorem prover on all problems, before switching to the usual default
mating-search if this fails. If NIL, will use the default mating-search 
only.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}.  The default value is @T{T}.
@End(Description)

@Section(MS88 search procedure)

@Begin(Description)
@IndexFlag(ADDED-CONN-ENABLED)@\
If NIL, recording events of type ADDED-CONN is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(CONSIDERED-CONN-ENABLED)@\
If NIL, recording events of type CONSIDERED-CONN is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(DUP-ALLOWED)@\
If T mating search duplicates quantifiers whenever necessary.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(DUPE-ENABLED)@\
If NIL, recording events of type DUPE is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(DUPE-VAR-ENABLED)@\
If NIL, recording events of type DUPE-VAR is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(EXCLUDING-GC-TIME)@\
If T, we can use the function get-net-internal-run-time to exclude 
the gc time in recordings. Otherwise, get-net-internal-run-time is the
same as get-internal-run-time. The value of the flag should not be changed. 
This is a nominal flag, whose value does not affect the system at all except
telling users the message above. Check the flags SEARCH-TIME-LIMIT and
MAX-SEARCH-LIMIT to get more information.
It takes values of type BOOLEAN and belongs to subjects @t{MATING-SEARCH}, @t{SYSTEM}.  The default value is @T{NIL}.

@IndexFlag(FIRST-ORDER-MODE-MS)@\
If T first-order unification is called during mating search, else
higher-order unification is used. TPS changes the value of this flag
to T when it is called by  DIY to work on a first-order problem,
but not when it is called from MATE.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(INCOMP-MATING-ENABLED)@\
If NIL, recording events of type INCOMP-MATING is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(MATE-FFPAIR)@\
Controls whether to consider a pair of literals with flexible
heads as a potential connection. The MS controller will locally modify
it under certain conditions; in particular, it will always be set locally
to T in the following cases, among others:
a) for first-order problems (when FIRST-ORDER-MODE-MS is T).
b) when a mating is removed because it is incompatible with the etree.
c) when using the interactive command ADD-CONN.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS91-6}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MATE-SUBSUMED-TEST-ENABLED)@\
If NIL, recording events of type MATE-SUBSUMED-TEST is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(MATE-SUBSUMED-TRUE-ENABLED)@\
If NIL, recording events of type MATE-SUBSUMED-TRUE is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(MATING-CHANGED-ENABLED)@\
If NIL, recording events of type MATING-CHANGED is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(MS-INIT-PATH)@\
If NIL MS considers the current path when a new mating is started.
Otherwise, starts from the beginning in the natural ordering on paths in a 
jform.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS-SPLIT)@\
If T mating search attempts to split the proof.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS91-6}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(OCCURS-CHECK)@\
This flag is not effective unless FIRST-ORDER-MODE-MS is T. If its
    value is T, occurs check in first-order unification is postponed till a
    mating is complete.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS91-6}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(PRIM-QUANTIFIER)@\
When NIL, primitive substitutions containing new quantifiers will
not be applied.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}, @t{PRIMSUBS}.  The default value is @T{T}.

@IndexFlag(PRIMSUB-ENABLED)@\
If NIL, recording events of type PRIMSUB is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(PROP-STRATEGY)@\
This flag is only used in PROPOSITIONAL proof search, which can 
    be one of (1) allow-duplicates (2) hash-table (3) pushnew
    (1) Adds CONNECTION to the mating even though it might already be in the
    mating.
    In case of (2) and (3) adds CONNECTION to the mating only if it is not
    already in the mating.
    (2) uses HASH-TABLE to determine this.
    (3) uses CLISP macro PUSHNEW to determine this.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{ALLOW-DUPLICATES}.

@IndexFlag(REMOVED-CONN-ENABLED)@\
If NIL, recording events of type REMOVED-CONN is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(SEARCH-COMPLETE-PATHS)@\
Not yet implemented.
If NIL paths are generated only to a length until a connection can be
located on it. Otherwise full paths are generated.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS91-6}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(START-TIME-ENABLED)@\
If NIL, recording events of type START-TIME is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(STOP-TIME-ENABLED)@\
If NIL, recording events of type STOP-TIME is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(TIMING-NAMED)@\
If T, the labels printed by display-time will be shortened 
to allow room for the name of the current dproof, if there is one.
If NIL, then they won't.
Abbreviations used are: PRE - preprocessing, MS - mating search, 
U - unification, PPR - postprocessing, MRG - merging, 
TRA - translation, PRT - printing.
It takes values of type BOOLEAN and belongs to subjects @t{MATING-SEARCH}, @t{SYSTEM}.  The default value is @T{NIL}.

@IndexFlag(UNIF-SUBSUMED-TEST-ENABLED)@\
If NIL, recording events of type UNIF-SUBSUMED-TEST is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(UNIF-SUBSUMED-TRUE-ENABLED)@\
If NIL, recording events of type UNIF-SUBSUMED-TRUE is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.
@End(Description)

@Section(MS89 search procedure)

@Begin(Description)
@IndexFlag(MAX-SEARCH-LIMIT)@\
If integer-valued, is an upper limit on the TOTAL amount of time
(in seconds) which can be spent on searching for a proof in any particular 
option.  If null, then search time is unbounded. The flag is not affected 
by Garbage Collecting time whenever the value of the flag excluding-gc-time 
is T. Please read the help message for EXCLUDING-GC-TIME for more information.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS89}, @t{IMPORTANT}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(RANK-EPROOF-FN)@\
The name of a function which should take as its single
argument an incomplete expansion proof, and return a nonnegative
integer ranking the proof's likelihood of success, with 0 meaning no
success (so don't try), and, otherwise, the better the likelihood, the
lower the returned value.
The only currently defined value for this flag is NUM-VPATHS-RANKING.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS90-9}, @t{MS89}, @t{MATING-SEARCH}.  The default value is @T{NUM-VPATHS-RANKING}.

@IndexFlag(SEARCH-TIME-LIMIT)@\
If integer-valued, is an upper limit on the CONTINUAL amount of time
(in seconds) which can be spent on searching for a proof in any particular 
option.  If null, then an ad hoc bound is used by the search procedure.
The flag is not affected by Garbage Collecting time whenever the value of the 
flag excluding-gc-time is T. Please read the help message for EXCLUDING-GC-TIME
for more information.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS89}, @t{IMPORTANT}, @t{MATING-SEARCH}.  The default value is @T{NIL}.
@End(Description)

@Section(MS90-3 search procedure)

@Begin(Description)
@IndexFlag(MAX-MATES)@\
Max number of mates for a literal. If the search attempts to
add a mate that would exceed this limit, then this connection is not added.
Copies of a literal created by path-focused duplication are regarded as
the same when computing this number.
Set MAX-MATES to INFINITY to allow an unlimited number of mates for any 
literal.
It takes values of type POSINTEGER-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS88}, @t{MS89}, @t{MS91-6}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MATING-SEARCH}.  The default value is @T{2}.

@IndexFlag(MIN-QUANT-ETREE)@\
Only affects path-focused search procedures. When this flag
is T, the scope of quantifiers is minimized in primsubs appearing in
the expansion proof after searching is done and before the
propositional proof checker starts. This allows the corresponding
instantiation terms in the ND proof to be in non-prenex form, often
giving more readable proofs.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MATING-SEARCH}, @t{ETREES}.  The default value is @T{T}.

@IndexFlag(MS90-3-DUP-STRATEGY)@\
1 to select any combination of duplications (2 1 3 1 is
allowed), any thing else to select duplications in non decreasing
order only. (2 1 3 1 is not allowed, but 1 1 2 3 is allowed.)
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MATING-SEARCH}.  The default value is @T{1}.

@IndexFlag(NUM-FRPAIRS)@\
The match routine considers at most NUM-FRPAIRS frpairs,
before selecting a frpair. However, if it finds a pair that has at
most 1 substitution, it will automatically select this pair.
Applies to UN90 only.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{UNIFICATION}.  The default value is @T{5}.

@IndexFlag(PRINT-MATING-COUNTER)@\
Prints the current mating after this many iterations in the
top level ms90-3 search. Applicable only for path-focused duplication
search procedures
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MATING-SEARCH}.  The default value is @T{300000}.

@IndexFlag(SHOW-TIME)@\
When true, print the time taken by MS90-3 and MS90-9.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MATING-SEARCH}.  The default value is @T{T}.
@End(Description)

@Section(MS91-6 and MS91-7 search procedures)

@Begin(Description)
@IndexFlag(MS91-INTERLEAVE)@\
In MS91-*, primitive substitutions are generated by NAME-PRIM,
and they are applied to the master eproof before the search mechanism 
chooses particular parts of that eproof (and hence particular 
substitutions) to try and prove.

If MS91-INTERLEAVE is NIL, all of the substitutions generated by NAME-PRIM
are applied at once, and then the search mechanism chooses among them, probably
in the order in which they were generated. The process of applying them to
the eproof can take a very long time.

If MS91-INTERLEAVE is an integer n, we take n primsubs at a time for each 
variable which has primsubs, and apply only those to the eproof. Once we
have searched through those (to be specific, once we decide to generate new
options), we take the next n primsubs for each variable and apply them, 
and so on. This is much quicker, and has the advantage of not having to 
work through every primsub for the first variable before starting work on 
the next variable.

If MS91-INTERLEAVE is non-NIL, and NEW-OPTION-SET-LIMIT is greater than
MS91-INTERLEAVE * (# of vars that have primsubs), then TPS will reduce 
NEW-OPTION-SET-LIMIT. This ensures that single substitutions are 
generated before multiple substitutions.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{5}.

@IndexFlag(MS91-PREFER-SMALLER)@\
When T, smaller option-sets will be preferred to any larger ones.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{T}.

@IndexFlag(MS91-TIME-BY-VPATHS)@\
When T, the amount of time given by SEARCH-TIME-LIMIT and 
MAX-SEARCH-LIMIT will be multiplied by the number of vertical paths
through the vpform and then divided by the number of paths through 
the initial vpform (so the first vpform will get SEARCH-TIME-LIMIT
seconds, and if the next has twice as many paths it will get twice
as many seconds, and so on...).
When NIL, every option set will get the same search time.
This flag only applies in MS91 procedures.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{NIL}.

@IndexFlag(MS91-WEIGHT-LIMIT-RANGE)@\
New option-sets, when constructed, will be accepted if their weights lie
in the range [current weight limit, current weight limit + MS91-WEIGHT-LIMIT-RANGE].
Hence increasing this value means that more option-sets will be acceptable during 
the creation stage. If this range is very small, there is a risk that no option
sets at all will be accepted and the search will waste time recreating these sets
with a higher current weight limit. If it is too large, then there is a risk that
high-weighted sets will be considered before lower-weighted ones.
Note: option sets of weight INFINITY will never be accepted, no matter what.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{1}.

@IndexFlag(NEW-OPTION-SET-LIMIT)@\
The maximum number of new option-sets that can be created at
any one time. See MS91-INTERLEAVE.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{20}.

@IndexFlag(OPTIONS-GENERATE-ARG)@\
The argument used by the function given in the flag 
OPTIONS-GENERATE-FN. If this argument is INFINITY then new options will
never be generated. See the help message for OPTIONS-GENERATE-FN.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{75}.

@IndexFlag(OPTIONS-GENERATE-FN)@\
This is the function for deciding when to add new options
to the list from which option sets are generated. This is only called 
when new option sets are being generated, so if you are generating large
numbers of options sets at a time then you might not see an effect until
some time after your given criterion is satisfied. (Check the value of
NEW-OPTION-SETS-LIMIT if this seems to be the case.) The argument for 
this function is in the flag OPTIONS-GENERATE-ARG, and the function
to update that argument is in the flag OPTIONS-GENERATE-UPDATE.
The options are:
* ADD-OPTIONS-ORIGINAL generates new options when over 
  OPTIONS-GENERATE-ARG percent of the possible option sets have been 
  used, and each option appears in at least one option set.
* ADD-OPTIONS-COUNT generates new options when more than 
  OPTIONS-GENERATE-ARG different option sets have been tried.
* ADD-OPTIONS-WEIGHT generates new options when the lower end of the
  acceptable weight bracket for a new option set exceeds 
  OPTIONS-GENERATE-ARG.
* ADD-OPTIONS-SUBS generates new options when the number of 
  substitutions and duplications in the next option set (i.e.
  its SIMPLEST-WEIGHT-B) exceeds OPTIONS-GENERATE-ARG.
If OPTIONS-GENERATE-ARG is INFINITY, no new options are ever generated.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{ADD-OPTIONS-ORIGINAL}.

@IndexFlag(OPTIONS-GENERATE-UPDATE)@\
The function used to update the value of the flag
OPTIONS-GENERATE-ARG. Current possibilities are:
* IDENT-ARG leaves the value unchanged.
* DOUBLE-ARG doubles the value.
* SQUARE-ARG squares the value.
* INF-ARG makes the value INFINITY.
Note that a value of INFINITY means that new options will never be 
generated.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{IDENT-ARG}.

@IndexFlag(OPTIONS-VERBOSE)@\
If T, will output extra information about the options being 
considered.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{NIL}.

@IndexFlag(PENALTY-FOR-EACH-PRIMSUB)@\
Used in computing weight-b in MS91 search procedures.  Should be 
a nonnegative integer or the symbol INFINITY, and will be the amount 
of penalty given for using each primitive substitution. See WEIGHT-B-FN.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{3}.

@IndexFlag(PENALTY-FOR-MULTIPLE-PRIMSUBS)@\
Used in computing weight-b in MS91 search procedures.  Should be 
a nonnegative integer or the symbol INFINITY, and will be the amount 
of penalty given for using more than one primitive substitution for a single
variable. See WEIGHT-B-FN.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{5}.

@IndexFlag(PENALTY-FOR-MULTIPLE-SUBS)@\
Used in computing weight-b in MS91 search procedures.  Should be 
a nonnegative integer or the symbol INFINITY, and will be the amount 
of penalty given for using more than one substitution for a single
variable. See WEIGHT-B-FN.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{5}.

@IndexFlag(PENALTY-FOR-ORDINARY-DUP)@\
Used in computing weight-b in MS91 search procedures.  Should be 
a nonnegative integer or the symbol INFINITY, and will be the amount 
of penalty given for each duplicate copy of a quantifier which is not 
used by a primitive substitution. See WEIGHT-B-FN.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{INFINITY}.

@IndexFlag(RECONSIDER-FN)@\
A function that should take a weight as argument and
return a value to be used as a new weight after the associated option set
runs out of time. Currently, the predefined functions are INF-WEIGHT 
SQUARE-WEIGHT, DOUBLE-WEIGHT and INCREMENT-WEIGHT (which, respectively,
make reconsidering an old option set impossible, very unlikely,
quite likely and probable). INCREMENT-WEIGHT actually adds 10 to the weight 
of an option set, as adding 1 is insignificant under most circumstances.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{DOUBLE-WEIGHT}.

@IndexFlag(WEIGHT-A-COEFFICIENT)@\
Coefficient to be used in multiplying weight-a of options in
the option-set of which we are computing weight-d. See WEIGHT-A-FN.
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{0}.

@IndexFlag(WEIGHT-A-FN)@\
A function that should take an option as argument and
return a value to be used as its weight-a. Currently, the only 
such predefined function is EXPANSION-LEVEL-WEIGHT-A, which returns
the expansion level of the option to be used as a weight.
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{EXPANSION-LEVEL-WEIGHT-A}.

@IndexFlag(WEIGHT-B-COEFFICIENT)@\
Coefficient to be used in multiplying weight-b of option/option-subset
pairs for the option-set of which we are computing weight-d. See
WEIGHT-B-FN. The total weight of a set of options is the weight-a of 
each option plus the weight-b of the set plus the weight-c of the set.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{1}.

@IndexFlag(WEIGHT-B-FN)@\
A function that should take an option set and return a value 
to be used as its weight-b. Currently, the only such predefined functions are: 
* SIMPLE-WEIGHT-B-FN, which returns the sum of the penalties for the primsubs, 
  multiple subs and duplications used in the option set (see the flags 
  PENALTY-FOR-EACH-PRIMSUB, PENALTY-FOR-MULTIPLE-PRIMSUBS and  
  PENALTY-FOR-MULTIPLE-SUBS for more information),
* ALL-PENALTIES-FN which is much the same as SIMPLE-WEIGHT-B-FN but also adds
  a penalty for extra duplications given by the PENALTY-FOR-ORDINARY-DUP 
  flag, and 
* SIMPLEST-WEIGHT-B-FN, which returns 1 for the original option set and adds 
  1 for each primsub or duplication (the idea is to set the coefficients of 
  weight-a and weight-c to zero while using SIMPLEST-WEIGHT-B-FN).
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{SIMPLEST-WEIGHT-B-FN}.

@IndexFlag(WEIGHT-C-COEFFICIENT)@\
Coefficient to be used in multiplying weight-c of options in
the option-set of which we are computing weight-d. See WEIGHT-C-FN.
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{0}.

@IndexFlag(WEIGHT-C-FN)@\
A function that should take an list of options as argument and
return a value to be used as its weight-c. Currently, the only such 
predefined functions are OPTION-SET-NUM-VPATHS, which returns the number of
vertical paths through the relevant etree, and OPTION-SET-NUM-LEAVES, which 
returns the number of leaves in the relevant etree.
The total weight of a set of options is the weight-a of each option
plus the weight-b of the set plus the weight-c of the set.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS91-7}, @t{MS91-6}.  The default value is @T{OPTION-SET-NUM-LEAVES}.
@End(Description)

@Section(MS98-1 search procedure)

@Begin(Description)
@IndexFlag(BREAK-AT-QUANTIFIERS)@\
Applies only to quantifiers which cannot be duplicated 
later in the search. If T, then fragments will be broken so as 
not to contain any quantifiers; if NIL, fragments may contain
quantifiers of the sort specified.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{NIL}.

@IndexFlag(FF-DELAY)@\
If T, delay unifying f-f pairs for single connections, and unify
them in context when some f-r pairs are added. If NIL, unify them as usual.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(HPATH-THRESHOLD)@\
If NIL, break on major conjunctions. If n, break at
conjunctions and also on disjunctions having more than n hpaths.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{1}.

@IndexFlag(MAXIMIZE-FIRST)@\
For each component which is being extended, do not create 
any new components which exceed MAX-MATES 1 until there are no other
ways to extend the component. This only works for higher-order
problems, and will be ignored in the first-order case.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MEASUREMENTS)@\
A flag set by the system to give information about
the complexity of the last problem worked on by TPS.
Should be included in the value of RECORDFLAGS
so that daterec will record the information.

Currently this records the number of vertical and horizontal paths,
number of literals, and number of acceptable connections.
It takes values of type SYMBOL-DATA-LIST and belongs to subjects @t{TRANSMIT}, @t{LIBRARY}.  The default value is @T{()}.

@IndexFlag(MS98-BASE-PRIM)@\
If T, we allow the search to begin with a fragment which is part
of a primitive substitution. If NIL, we always choose a fragment which is outside 
the primitive substitutions (if possible).
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{NIL}.

@IndexFlag(MS98-DUP-BELOW-PRIMSUBS)@\
When T, duplicate the quantifiers which occur below 
a primitive substitution NUM-OF-DUPS times. When NIL, don't.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-DUP-PRIMSUBS)@\
When T, MS98-DUP duplicates variables which have primsubs;
when NIL, it doesn't. (Note that duplicating the variable will
not duplicate the primsub; it will produce another copy of the 
unsubstituted-for tree below that expansion node.)
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-EXTERNAL-REWRITES)@\
When set to T, MS98-1 uses the currently active rewrite rules as
global rewrites in addition to those it extracts from the formula.
See Matt Bishop's thesis for details on rewriting in MS98-1.
If MS98-REWRITES is set to NIL, this flag is irrelevant.
It takes values of type BOOLEAN and belongs to subjects @t{MS98-1}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS98-FIRST-FRAGMENT)@\
If non-NIL, this will move a single fragment to the
beginning of the literal ordering, as follows:
T : set of support strategy, more or less. The starting 
    fragment will be the last non-duplicate fragment 
    enumerated. This will be the rightmost part of the wff
    to be proven.
n : (for integer n) the starting fragment will be whichever
    fragment contains LEAFn. If this leaf is part of a duplicate
    fragment, or does not exist at all, then this will behave
    like T.

NB: This flag overrides MS98-BASE-PRIM; the chosen fragment may
always be part of a primitve substitution. 
See also MS98-FRAGMENT-ORDER.
It takes values of type SYMBOL-OR-INTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{NIL}.

@IndexFlag(MS98-FORCE-H-O)@\
If T, use higher-order unification graphs even for 
first-order searches. If NIL, use the normal first-order
unification.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-FRAGMENT-ORDER)@\
The order in which the fragments are considered. This
principally affects which fragment will become the starting 
point of the search, and which of the touched but not blocked
fragments will be blocked next. See also MS98-FIRST-FRAGMENT.
0 : consider the number of ways to block the given fragment.
1 : consider the number of ways that the results for 0 might be
    extended (i.e. look ahead two steps in the search process)
2 : as for 1, but then weight in favour of ground fragments
    (i.e. those containing no variables).
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{1}.

@IndexFlag(MS98-INIT)@\
Before doing ms98-1 search:
If 0, do nothing at first; after each failure, duplicate one more quantifier.
If 1, duplicate all outer quantifiers NUM-OF-DUPS times.
If 2, apply primsubs and duplicate all outer quantifiers NUM-OF-DUPS times.
If 3, cycle through primsubs one at a time, and duplicate all outer
      quantifiers NUM-OF-DUPS times. The time spent on each primsub will
      be at least MAX-SEARCH-LIMIT seconds, unless the search fails before
      then.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{0}.

@IndexFlag(MS98-LOW-MEMORY)@\
If T, try to keep memory use low. This will probably
make the search take longer.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-MAX-COMPONENTS)@\
If non-NIL, the maximum number of components that can be 
considered on any iteration of the MS98 search.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-MAX-PRIMS)@\
The maximum number of primsubs allowed in any component.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{1}.

@IndexFlag(MS98-MEASURE)@\
Determines the measure which is used on components.
If 0, count the components blocked and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 1, the standard measure using the # of components blocked and touched
If 2, as for 1 but also take account of the number of dups
If 3, just count the number of components blocked
If 4, as for 2 but also count the no of matings for the smallest 
      component touched
If 5, multiply the no of matings for the smallest touched by the 
      number of subs.
If 6, use the ratio of blocked to touched components and the ratio
      of the number of blocked components to the number of connections.
If 7, prefer matings where positive leaves are mated to negative leaves
      and vice versa.
If 8, use the ratio of blocked to touched components.
If 9, favour large components satisfying max-mates 1.
If 10, do as for 9 and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 11, do as for 6 and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 12, do as for 8 and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 13, weight in favour of components with max-mates 1 and then 
      weight heavily against the situation described by MS98-VALID-PAIR.
If 14, do as for 7 and then weight heavily against 
      the situation described by MS98-VALID-PAIR.
If 15, take the average of 11 and 14.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{0}.

@IndexFlag(MS98-MERGE-DAGS)@\
For higher-order searches only. Affects the way in 
which the unification graphs of elementary components are 
computed.
0 : Check that the graphs of the connections are pairwise
    compatible. Only compute the full graph of a component
    when necessary.
1 : Check that the graphs of the connections are compatible
    taken all together. (This can take a while for large
    sets of connections.) Only compute the full graph when 
    necessary.
2 : Always compute the full graph. This overrides FF-DELAY.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{0}.

@IndexFlag(MS98-MINIMALITY-CHECK)@\
If T, check each new component for minimality and reject 
those which are non-minimal. If NIL, don't bother.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-NUM-OF-DUPS)@\
If NIL, we can use every duplication that's present.
If some positive integer n, we reject any component using more than n 
of the duplications.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{NIL}.

@IndexFlag(MS98-POLLUTE-GLOBAL-REWRITES)@\
When set to T, rewrites generated by MS98-1 are not removed from
the list of global rewrite rules after the search is complete.
See Matt Bishop's thesis for details on rewriting in MS98-1.
If MS98-REWRITES is set to NIL, this flag is irrelevant.
It takes values of type BOOLEAN and belongs to subjects @t{MS98-1}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS98-PRIMSUB-COUNT)@\
The maximum number of primsubs to be applied each
set variable in the expansion tree.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{3}.

@IndexFlag(MS98-REW-PRIMSUBS)@\
When T, MS98-DUP does primsubs for Leibniz variables which 
have become rewrites; when NIL, it doesn't.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-REWRITE-DEPTH)@\
When attempting to rewrite one term into another,
the maximum number of steps of rewriting that are allowed.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{2}.

@IndexFlag(MS98-REWRITE-MODEL)@\
If T, ask the user for a model of the rewrite rules
to help slim down the unification tree.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-REWRITE-PRUNE)@\
If T, delete any unifiers which are duplicates modulo
rewriting (this can be slow). If NIL, don't.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{T}.

@IndexFlag(MS98-REWRITE-SIZE)@\
The maximum size of a (lambda-normalized) term that can be 
produced by rewriting, measured as the number of nodes in the parse
tree of that term. NIL means that there is no maximum.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{NIL}.

@IndexFlag(MS98-REWRITE-UNIF)@\
When a rewrite rule can introduce a new variable, this
flag governs the size of the allowed substitutions for that
variable. Essentially, this is a special case of MAX-SUBSTS-VAR.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{NIL}.

@IndexFlag(MS98-REWRITES)@\
When T, make all of the global equalities into rewrites.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{NIL}.

@IndexFlag(MS98-TRACE)@\
Given a mating in advance, this is used to trace the progress 
of MS98-1 search for a mating.  This is a list of symbols which
indicate what to trace.  The possible symbols are:

1. MATING - Search as usual, keeping a record of when good connections and
components are formed.  The value of *ms98-trace-file* is a string giving the
name of a file into which this information is stored.
2. MATING-FILTER - The search is filtered to only consider good
connections and components.  This is useful for a quick check
if the search can possibly succeed.  Typically, when MATING-FILTER
is on the list, then so is MATING.

If the list is nonempty at all, then the trace is considered 'on'.
The consequence of this is that duplications and primsubs are
skipped at the beginning of search, and that the output of the
trace will be sent to the file indicated by the global variable
*ms98-trace-file*.
It takes values of type SYMBOLLIST and belongs to subjects @t{MS98-MINOR}.  The default value is @T{()}.

@IndexFlag(MS98-UNIF-HACK)@\
If T, do not introduce new constants during unification.
(NOTE: This is a hack; we *do* need to introduce new constants, in 
general, but in most cases we needn't bother.)
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-UNIF-HACK2)@\
If T, during the generation of unifiers, prevent the 
occurrence of subformulas of type o which contain no variables
(except for TRUTH and FALSEHOOD, if they are allowed by MS98-UNIF-HACK).
If NIL, allow these to be generated.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{NIL}.

@IndexFlag(MS98-USE-COLORS)@\
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{T}.

@IndexFlag(MS98-VALID-PAIR)@\
Given two disjuncts X OR Y and A OR B, this flag 
determines when we are allowed to make a component containing 
connections X-A and Y-B (assuming they're unifiable connections).
The higher the number, the more stringent (and more time-consuming)
the test; any correct mating is guaranteed to pass any of these tests:
1: MAX-MATES is not 1.
2: As for 1, plus we require an extra mate for each of X,Y,A and B.
3: As for 2, plus we require that all of these new mates be 
   pairwise compatible with each other.
4: As for 3, plus we require that all of these new mates be 
   simultaneously compatible with each other.

3 and 4 are only applicable to higher-order searches.

There is an extra value, 0, which rejects any such connections
even if max-mates is not 1. This results in an incomplete search,
but is often acceptable.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{1}.

@IndexFlag(MS98-VARIABLE-ORDER)@\
Determines the variable ordering for the unification
graph. Only affects higher-order searches.
Suppose N is the maximum number of unifiers for a 
given list of variables, and K is the length of the list.
For values 0--3, the variables are first grouped into lists of duplicate
copies (so each variable is listed with its duplicates, if any)
0 : Sort by N, largest first.
1 : Sort by N, smallest first.
2 : Sort by K, largest first.
3 : Sort by K, smallest first.
10--13 : Group the variables into lists of length 1, and then proceed
as 0--3.
20--23 : Group the variables into lists that occur together (i.e. two
variables go into the same list if their expansion nodes are not separated 
by any junctive node in the etree) and then proceed as for 0--3.
30--33 : Group the variables as for 0--3, and then reduce the lists 
to length 1 while keeping the variables in the same order.
40--43 : Group the variables as for 20--23, and then reduce the lists 
to length 1 while keeping the variables in the same order.
Other values X will behave like (X div 10) for variable grouping and
(X mod 10) for ordering the groups.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS98-MINOR}.  The default value is @T{1}.

@IndexFlag(MS98-VERBOSE)@\
If T, print extra information during MS98-1 search.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-1}.  The default value is @T{NIL}.
@End(Description)

@Section(Extensional Search)

@Begin(Description)
@IndexFlag(EXT-SEARCH-LIMIT)@\
If EXT-SEARCH-LIMIT is an integer which will place a limit on the extensional search
procedure MS03-7.  Given such a limit, search is incomplete and guaranteed to eventually terminate.
If EXT-SEARCH-LIMIT is set to infinity, then the search may not terminate.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{INFINITY}.

@IndexFlag(MS03-DUP-METHOD)@\
The method by which different duplication options are considered
by the MS03-7 search procedure.

1.  Simply add the oldest expansion arc that has not been considered yet
(and any arcs related to it) each time a new option is tried.
This will lead to extremely large jforms in most cases.

2.  Works like 1 except with respect to expansion arcs that either
contain a nontrivial set substitution (ie, one with logical connectives)
or are associated with a set existence lemma.  With respect to these
'set expansion arcs', we remove whatever such arcs are in the current
option and replace them with a new set expansion arc (thus considering
a new set expansion option).  If every single set expansion option
has been considered, we begin considering two at a time, and so on.

3.  Works like 2 except we treat every expansion of set type
as a set expansion arc instead of just the ones with nontrivial
set substitutions.

See Also: MS03-WEIGHT-CHANGE-DUPS, MAX-SEARCH-LIMIT
It takes values of type POSNUMBER and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS03-QUICK-EUNIFICATION-LIMIT)@\
This provides a bound on how much E-unification MS03-7 and MS04-2 attempt to do
before deciding what to mate.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{50}.

@IndexFlag(MS03-SOLVE-RIGID-PARTS)@\
If T, MS03-7 tries to find quick solutions to the rigid parts of a
problem.  This only applies when MS03-USE-JFORMS is T.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS03-SOLVE-RIGID-PARTS-ALLOW-RECONNECTS)@\
When trying to solve the rigid part of a jform,
we might consider connecting two literals that are already connected.
Sometimes this speeds up the search, presumably by keeping
us from looking at possible connections beneath connections (needed to show
equivalences).
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS03-USE-JFORMS)@\
If T, MS03-7 uses (dissolved) jforms during search.
Constructing and dissolving jforms can be time consuming, but in
principle can restrict the branching of search.  If NIL, jforms
are not used, which may result in the consideration of connections
which only span paths already spanned by other connections.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS03-USE-SET-CONSTRAINTS)@\
If this flag and MS03-USE-JFORMS are T,
MS03-7 uses set constraints in addition to primsubs 
to determine potential set substitutions.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS03-VERBOSE)@\
If T, print extra information during MS03-7 search.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS03-WEIGHT-BANNED-SELS)@\
Controls the penalty for trying to unify two terms that require
getting around using a banned selected variable (using duplication or
equational reasoning).
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{300}.

@IndexFlag(MS03-WEIGHT-CHANGE-DUPS)@\
If MAX-SEARCH-LIMIT is NIL, then MS03-WEIGHT-CHANGE-DUPS controls
how often MS03-7 changes which expansion terms are considered.

SEE ALSO:  MS03-DUP-METHOD
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{100}.

@IndexFlag(MS03-WEIGHT-DISJ-EUNIF)@\
When attempting to E-unify two literals a and b, this weight is
multiplied by disjdepth(a) * disjdepth(b) where disjdepth of a literal
is the number of disjunctions above the literal on the jform.
The effect of this is to prefer mating nodes that are closer to being 'global'.

If MS03-USE-JFORMS is set to NIL, the disjdepth of a node is
measured by the number of disjunctive nodes above the node in the edag.
This measure is less precise, since dissolution isn't used.

See Also: MS03-WEIGHT-DISJ-MATE, MS03-WEIGHT-DISJ-UNIF
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS03-WEIGHT-DISJ-MATE)@\
When attempting to mate two literals a and b, this weight is
multiplied by disjdepth(a) * disjdepth(b) where disjdepth of a literal
is the number of disjunctions above the literal on the jform.
The effect of this is to prefer mating nodes that are closer to being 'global'.

If MS03-USE-JFORMS is set to NIL, the disjdepth of a node is
measured by the number of disjunctive nodes above the node in the edag.
This measure is less precise, since dissolution isn't used.

See Also: MS03-WEIGHT-DISJ-EUNIF, MS03-WEIGHT-DISJ-UNIF
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS03-WEIGHT-DISJ-UNIF)@\
When performing a unification (imitation or projection) step
on a negative equation literal, this value is multiplied by the
disjdepth of the literal.  The disjdepth is the number of disjunctions
above the literal in the jform.

If MS03-USE-JFORMS is set to NIL, the disjdepth of the negative equation
node is measured by the number of disjunctive nodes above the node in the edag.

See Also: MS03-WEIGHT-DISJ-MATE, MS03-WEIGHT-DISJ-EUNIF
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS03-WEIGHT-DUP-VAR)@\
Controls how often MS03-7 tries to duplicate an expansion variable in
order to substitute a banned selected variable for the new expansion
variable.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{300}.

@IndexFlag(MS03-WEIGHT-EUNIF1)@\
This value is added to the weight for adding any eunif1
(E-unification without symmetry) between two equation literals.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS03-WEIGHT-EUNIF2)@\
This value is added to the weight for adding any eunif2
(E-unification with symmetry) between two equation literals.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS03-WEIGHT-FLEXFLEXDIFF)@\
Controls the penalty for trying to unify two terms that require
unifying two flexible terms of a base type other than O with different heads.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{3}.

@IndexFlag(MS03-WEIGHT-FLEXFLEXDIFF-O)@\
Controls the penalty for trying to unify two terms that require
unifying two flexible terms of type O with different heads.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS03-WEIGHT-FLEXFLEXSAME)@\
Controls the penalty for trying to unify two terms that require
unifying two flexible terms of a base type other than O with the same head.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{5}.

@IndexFlag(MS03-WEIGHT-FLEXFLEXSAME-O)@\
Controls the penalty for trying to unify two terms that require
unifying two flexible terms of type O with the same head.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{20}.

@IndexFlag(MS03-WEIGHT-FLEXRIGID-BRANCH)@\
Controls the penalty for trying to unify two terms that require
solving a branching (higher-order) flex-rigid disagreement pair of a
base type other than O.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{6}.

@IndexFlag(MS03-WEIGHT-FLEXRIGID-EQN)@\
Controls the penalty for trying to unify two terms that require a
solving a flex-rigid pair of a base type other than O when no
imitation and no projection is appropriate and there is an an equation
which is between a pair of rigid terms sharing a head with the
disagreement pair.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{100}.

@IndexFlag(MS03-WEIGHT-FLEXRIGID-FLEXEQN)@\
Controls the penalty for trying to unify two terms that require a
solving a flex-rigid pair of a base type other than O when no
imitation and no projection is appropriate and there is a flex-rigid
equation between terms of the same base type.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{100}.

@IndexFlag(MS03-WEIGHT-FLEXRIGID-MATE)@\
This value is added to the weight for adding any connection between
any rigid literal and flexible literal.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS03-WEIGHT-FLEXRIGID-NOEQN)@\
Controls the penalty for trying to unify two terms that require a
solving a flex-rigid pair of a base type other than O when no
imitation and no projection is appropriate and there are no flex-rigid
equations between terms of the same base type.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{500}.

@IndexFlag(MS03-WEIGHT-FLEXRIGID-O)@\
Controls the penalty for trying to unify two terms that require
solving a branching (higher-order) flex-rigid disagreement pair of
type O.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{20}.

@IndexFlag(MS03-WEIGHT-IMITATE)@\
This value is added to the weight for any imitation unification steps.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS03-WEIGHT-OCCURS-CHECK)@\
Controls the penalty for trying to unify two terms that require
getting around an occurs check (using equational reasoning).
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{150}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FALSEHOOD)@\
Controls how often MS03-7 tries a primsub using FORALL
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{50}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FIRST-AND)@\
Controls when MS03-7 or MS04-2 first tries a primsub using AND.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FIRST-EQUALS)@\
Controls when MS03-7 or MS04-2 first tries a primsub using equality at a base type.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FIRST-EXISTS)@\
Controls when MS03-7 or MS04-2 first tries a primsub using EXISTS.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FIRST-FORALL)@\
Controls when MS03-7 or MS04-2 first tries a primsub using FORALL.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FIRST-NOT-EQUALS)@\
Controls when MS03-7 or MS04-2 first tries a primsub using negation and equality
at a base type.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FIRST-NOT-PROJ)@\
Controls when MS03-7 or MS04-2 first tries a primsub using negation and a projection.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{500}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FIRST-OR)@\
Controls when MS03-7 or MS04-2 first tries a primsub using OR.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-FIRST-PROJ)@\
Controls when MS03-7 or MS04-2 first tries a primsub using a projection.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{500}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-NEXT-AND)@\
Controls how often MS03-7 or MS04-2 tries a primsub using AND after the first time.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-NEXT-EQUALS)@\
Controls how often MS03-7 or MS04-2 tries a primsub using equality at a base
type after the first time.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-NEXT-EXISTS)@\
Controls how often MS03-7 or MS04-2 tries a primsub using EXISTS at various
types after the first time.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-NEXT-FORALL)@\
Controls how often MS03-7 or MS04-2 tries a primsub using FORALL at various
types after the first time.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-NEXT-NOT-EQUALS)@\
Controls how often MS03-7 or MS04-2 tries a primsub using negation and equality
at a base type after the first time.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-NEXT-NOT-PROJ)@\
Controls how often MS03-7 or MS04-2 tries a primsub using negation and a
projection after the first time.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{500}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-NEXT-OR)@\
Controls how often MS03-7 or MS04-2 tries a primsub using OR after the first time.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{200}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-NEXT-PROJ)@\
Controls how often MS03-7 or MS04-2 tries a primsub using a projection after the first time.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{500}.

@IndexFlag(MS03-WEIGHT-PRIMSUB-TRUTH)@\
Controls how often MS03-7 tries a primsub using TRUTH
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{50}.

@IndexFlag(MS03-WEIGHT-PROJECT)@\
This value is added to the weight for any projection unification steps.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS03-WEIGHT-RIGID-MATE)@\
This value is added to the weight for adding any connection between
two rigid literals.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS03-WEIGHT-RIGIDRIGID-EQN)@\
Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of a base type other than O in the presence
of an equation which is between a pair of rigid terms sharing a head
with the disagreement pair.  Some form of equational reasoning is
required to solve these cases.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{50}.

@IndexFlag(MS03-WEIGHT-RIGIDRIGID-FLEXEQN)@\
Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of a base type other than O in the presence
of an equation which is between a rigid and a flexible term.  Some
form of equational reasoning is required to solve these cases.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{60}.

@IndexFlag(MS03-WEIGHT-RIGIDRIGID-NOEQN)@\
Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of a base type other than O in the absence
of any equations of the same base type.  Some form of equational
reasoning is required to solve these cases, but we may need to mate
two nodes before an appropriate equation has appeared in the search.
Such a case is unusual so it makes sense for this flag to be set to a
high value.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{500}.

@IndexFlag(MS03-WEIGHT-RIGIDRIGIDDIFF-O)@\
Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of type O with the different heads.
Extensionality is required to solve these cases.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{40}.

@IndexFlag(MS03-WEIGHT-RIGIDRIGIDSAME-O)@\
Controls the penalty for trying to unify two terms that require a
solving a rigid-rigid pair of type O with the same head.
Extensionality is required to solve these cases.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{MS03-7}, @t{EXT-SEARCH}.  The default value is @T{15}.

@IndexFlag(MS04-ALLOW-FLEX-EUNIFS)@\
If MS04-ALLOW-FLEX-EUNIFS is T, then MS04-2 will try to mate
flexible nodes with positive equation nodes and negative equation goal nodes.
To do this, MS04-2 will imitate the equality (or negation of equality) first.
This is not necessary for completeness (since an equality primsub will eventually
be considered), but is sometimes helpful.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS04-ALLOW-FLEXRIGID-PROJ-MATE)@\
If MS04-ALLOW-FLEXRIGID-PROJ-MATE is T, then MS04-2 will try to mate
flexible nodes with atoms using a projection.  This is not necessary
for completeness (since a projection primsub will eventually be
considered), but is sometimes helpful.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS04-BACKTRACK-METHOD)@\
Determines which choices are used for backtracking.

1.  Backtrack on all choices.

2.  Do not backtrack over connections.

3.  Do not backtrack over connections or duplications.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-CHECK-UNIF-DEPTH)@\
If MS04-DELAY-UNIF-CONSTRAINTS is T, MS04-CHECK-UNIF-DEPTH determines
how deeply MS04-2 will try to unify in order to prune out states where
the unification problem is unsolvable.

See Also: MS04-DELAY-UNIF-CONSTRAINTS
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{3}.

@IndexFlag(MS04-DELAY-FLEXRIGID-MATES)@\
If MS04-DELAY-UNIF-CONSTRAINTS is T and MS04-DELAY-FLEXRIGID-MATES is T,
then potential connections between flexible nodes and atomic nodes are delayed
and the dpair is added to the unification problem.  In particular, this may
allow projections to be used to unify flexible nodes with atomic nodes.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS04-DELAY-UNIF-CONSTRAINTS)@\
If set to T, the MS04-2 search procedure will delay
considering vertical paths that contain certain equation goals
which are being used to weight further options.  The procedure
is complete with this set to T or NIL.  Setting it to T creates
more nondeterminism, but can lead to faster proofs.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS04-DUP-EARLY)@\
If set to T, MS04-2 will only duplicate expansion nodes before making
any substitutions or connections (on paths that share the expansion
node).  Originally, MS04-2 always did this, but only MS04-2 with
duplications allowed anytime (when the value of MS04-DUP-EARLY is NIL)
is shown complete in Chad E. Brown's thesis.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS04-DUP-WEIGHT)@\
Sets the weight for duplicating an expansion node in MS04-2.
This controls how often MS04-2 will duplicate expansion nodes.  The higher
the weight, the less often duplication occurs.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{300}.

@IndexFlag(MS04-EAGER-UNIF-SUBST)@\
If set to T (and MS04-DELAY-UNIF-CONSTRAINTS is T), the MS04-2 search
procedure will substitute for parts of the pattern part of the current
unification problem.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS04-INCR-DEPTH)@\
Every time MS04-2 has completed the search space up to a given bound,
the bound is increased by MS04-INCR-DEPTH.

SEE ALSO: MS04-INITIAL-DEPTH, MS04-MAX-DEPTH
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{100}.

@IndexFlag(MS04-INITIAL-DEPTH)@\
This sets the initial bound for the depth of the search procedure
MS04-2.  Once the search to this depth has failed, MS04-INCR-DEPTH
is used to increase the bound.

SEE ALSO: MS04-INCR-DEPTH, MS04-MAX-DEPTH
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{100}.

@IndexFlag(MS04-MAX-DELAYED-CONNS)@\
The maximum number of delayed connections (waiting to be unified)
MS04-2 will consider (on the first iteration of search).
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS04-MAX-DEPTH)@\
This sets an absolute maximum on the depth of the search.
For completeness, this should be set to infinity.

SEE ALSO: MS04-INITIAL-DEPTH, MS04-INCR-DEPTH
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{INFINITY}.

@IndexFlag(MS04-MAX-DUPS)@\
The maximum number of duplications MS04-2 will consider
(on the first iteration of search).
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{3}.

@IndexFlag(MS04-MAX-EUNIF1S)@\
The maximum number of E-unification connections MS04-2 will consider
(on the first iteration of search).
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{3}.

@IndexFlag(MS04-MAX-EUNIF2S)@\
The maximum number of symmetric E-unification connections MS04-2 will consider
(on the first iteration of search).
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{3}.

@IndexFlag(MS04-MAX-FLEX-EUNIFS)@\
The maximum number of times MS04-2 will instantiate the head
of a flexible node with an equality of base type (or the negation of an equality)
in order to E-unify the instantiated node with a positive equation node or an equation goal node.
This flag is only relevant if MS04-ALLOW-FLEX-EUNIFS is set to T.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{2}.

@IndexFlag(MS04-MAX-FLEXRIGID-MATES)@\
The maximum number of mates between a flexible node and a rigid atom of opposite polarity 
MS04-2 will consider (by imitating the head of the rigid atom).
This value is increased by 1 after each failed iteration of the search.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{5}.

@IndexFlag(MS04-MAX-FLEXRIGID-NEG-MATES)@\
The maximum number of mates between a flexible node and a rigid atom of the same polarity 
MS04-2 will consider (by using a negation and imitating the head of the rigid atom).
This value is increased by 1 after each failed iteration of the search.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{5}.

@IndexFlag(MS04-MAX-FLEXRIGID-NEG-PROJ-MATES)@\
The maximum number of mates between a flexible node and a rigid atom of the same polarity 
MS04-2 will consider using projections with a negation instead of imitations.
This flag is only relevant if MS04-ALLOW-FLEXRIGID-PROJ-MATE is T.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{5}.

@IndexFlag(MS04-MAX-FLEXRIGID-PROJ-MATES)@\
The maximum number of mates between a flexible node and a rigid atom of opposite polarity 
MS04-2 will consider using projections instead of imitations.
This flag is only relevant if MS04-ALLOW-FLEXRIGID-PROJ-MATE is T.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{5}.

@IndexFlag(MS04-MAX-IMITS)@\
The maximum number of imitations (for unification) MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS04-MAX-PRIMSUB-AND)@\
The maximum number of conjunction primsubs MS04-2 will attempt during an iteration of the search.
Conjunction primsubs are only tried if MS04-PRENEX-PRIMSUBS is T.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PRIMSUB-EQUALS)@\
The maximum number of primsubs using equality (at base type) MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PRIMSUB-EXISTS)@\
The maximum number of EXISTS primsubs MS04-2 will attempt during an iteration of the search.
Conjunction primsubs are only tried if MS04-PRENEX-PRIMSUBS is T.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PRIMSUB-FORALL)@\
The maximum number of FORALL primsubs MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PRIMSUB-NOT)@\
The maximum number of negation primsubs MS04-2 will attempt during an iteration of the search.
Negation primsubs are only tried if MS04-PRENEX-PRIMSUBS is NIL.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PRIMSUB-NOT-EQUALS)@\
The maximum number of primsubs using negated equality (at base type) MS04-2 will attempt during an iteration of the search.
Negated equality primsubs are only tried if MS04-PRENEX-PRIMSUBS is T.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PRIMSUB-NOT-PROJ)@\
The maximum number of negated projection primsubs MS04-2 will attempt during an iteration of the search.
Negated projection primsubs are only tried if MS04-PRENEX-PRIMSUBS is T.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PRIMSUB-OR)@\
The maximum number of disjunction primsubs MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PRIMSUB-PROJ)@\
The maximum number of projection primsubs MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-MAX-PROJS)@\
The maximum number of projections (for unification) MS04-2 will attempt during an iteration of the search.
The value is increased by 1 after every failed iteration.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS04-MAX-RIGID-MATES)@\
The maximum number of mates between nodes which are already rigid
MS04-2 will consider (on the first iteration of search).
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS04-MP-OPTIONS)@\
In Allegro, any MS04-2 option listed in the value of this flag will
cause TPS to use multiprocessing to consider the option in parallel to
consideration of other options.

The main MS04-2 options which may be included on the list are DUP,
PRIMSUB and ADD-SET-CONSTRAINT.  Other MS04-2 options which may be
included are MATE, EUNIF1, EUNIF2, SUBST, MATE-FLEXRIGID,
MATE-FLEXRIGID-NEG, MATE-FLEXRIGID-PROJ, MATE-FLEXRIGID-NEG-PROJ,
FLEX-EUNIF, PRIMSUB-QUANT-GENTP, DELAY-UNIF, DELAY-CONN and
SOLVE-SET-CONSTRAINTS.
It takes values of type SYMBOLLIST and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{()}.

@IndexFlag(MS04-PRENEX-PRIMSUBS)@\
If T, only primsubs in conjunctive-prenex normal forms will be generated.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{T}.

@IndexFlag(MS04-SEMANTIC-PRUNING)@\
If set to T, the MS04-2 search procedure will try to prune search
states using semantics.

See Also: MODELS, MAX-DOMAIN-SIZE, MAX-BINDER-COMPUTATION
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS04-SOLVE-UNIF-DEPTH)@\
If MS04-DELAY-UNIF-CONSTRAINTS is T, MS04-SOLVE-UNIF-DEPTH determines
how deeply MS04-2 will try to solve unification constraints after
every vertical path can be solved by the delayed unification
constraints.

See Also: MS04-DELAY-UNIF-CONSTRAINTS
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{5}.

@IndexFlag(MS04-TRACE)@\
If T, MS04-2 will gather information about the search which will be
used to suggest values for flag settings (if search is successful).
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS04-USE-SEMANTICS)@\
If set to T, the MS04-2 search procedure will use semantics to
guide the search.

See Also: MODELS, MAX-DOMAIN-SIZE, MAX-BINDER-COMPUTATION
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS04-USE-SET-CONSTRAINTS)@\
If set to T, the MS04-2 search procedure will use set constraints
and set existence lemmas to solve for set variables.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(MS04-VERBOSE)@\
Determines level of verbosity of MS04-2 search.
Value should be MIN, MED or MAX.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{MED}.

@IndexFlag(MS04-WEIGHT-ADD-SET-CONSTRAINT)@\
If MS04-USE-SET-CONSTRAINTS is T, this weight is used to determine
when to add another constraint for a set variable.

See Also: MS04-USE-SET-CONSTRAINTS, MAX-NUM-CONSTRAINTS, MAX-CONSTRAINT-SIZE
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.

@IndexFlag(MS04-WEIGHT-DELAY-UNIF)@\
If MS04-DELAY-UNIF-CONSTRAINTS is T, this weight is used to determine
when to add an equation goal node to the collection of delayed
unification constraints.

See Also: MS04-DELAY-UNIF-CONSTRAINTS
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{0}.

@IndexFlag(MS04-WEIGHT-EUNIF-DECS)@\
Controls how often EUnification is applied to equation goals that are
decomposable, i.e., have shallow formula of the form:

     [H . . .] = [H . . .]

There are cases where one needs to do this, but often one wants to avoid it.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1000}.

@IndexFlag(MS04-WEIGHT-EUNIF-DIFF-HEADS)@\
An extra weight on EUNIF1 steps of the form [A = B]^+ to [C = D]^-
where the heads of A and C are different and the heads of B and D
are different.  The weight is also added to EUNIF2 steps when the
heads A and D are different and the heads of B and C are different.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{2000}.

@IndexFlag(MS04-WEIGHT-FLEX-EUNIF)@\
This value is added to the weight for adding any connection between
any flexible literal and an equation.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{2}.

@IndexFlag(MS04-WEIGHT-FLEXRIGID-PROJ-MATE)@\
This value is added to the weight for adding any connection between a
flexible literal and an atom using a projection on the head of the
flexible literal.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{2}.

@IndexFlag(MS04-WEIGHT-MULTIPLE-EUNIF1S)@\
This controls the extra weight every time a node is eunified more than once.
This is similar to MAX-MATES.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS04-WEIGHT-MULTIPLE-EUNIF2S)@\
This controls the extra weight every time a node is symmetrically eunified
more than once.  This is similar to MAX-MATES.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS04-WEIGHT-MULTIPLE-MATES)@\
This controls the extra weight every time a node is mated more than once.
This is similar to MAX-MATES.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{10}.

@IndexFlag(MS04-WEIGHT-PRIMSUB-FIRST-NOT)@\
Controls when MS04-2 first tries a primsub using a negation.

This is only used when MS04-PRENEX-PRIMSUBS is NIL.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1000}.

@IndexFlag(MS04-WEIGHT-PRIMSUB-NEXT-NOT)@\
Controls when MS04-2 tries a primsub using a negation after the first time.

This is only used when MS04-PRENEX-PRIMSUBS is NIL.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1000}.

@IndexFlag(MS04-WEIGHT-PRIMSUB-NEXTTP)@\
Sets the weight for each higher type we generate for a primsub using
either FORALL or EXISTS.  This controls how often MS04-2 will use
primsubs with higher types.  The higher the weight, the less often
higher types are used.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{100}.

@IndexFlag(MS04-WEIGHT-PRIMSUB-OCCURS-CONST)@\
Some logical constants occur embedded in the terms of a theorem.
This flag controls when MS04-2 tries a primsub using one of these logical constants
if the logical constant will not be tried by other primsubs.
This is only used if MS04-PRENEX-PRIMSUBS is NIL.

See Also: MS04-PRENEX-PRIMSUBS
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1000}.

@IndexFlag(MS04-WEIGHT-SOLVE-SET-CONSTRAINTS)@\
If MS04-USE-SET-CONSTRAINTS is T, this weight is used to determine
when to stop adding constraints for a set variable.

See Also: MS04-USE-SET-CONSTRAINTS, MAX-NUM-CONSTRAINTS, MAX-CONSTRAINT-SIZE
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS04-2}, @t{EXT-SEARCH}.  The default value is @T{1}.
@End(Description)

@Section(Proof Translation)

@Begin(Description)
@IndexFlag(ETREE-NAT-VERBOSE)@\
Should be a list of print-functions (see the help message 
for PRINT-FUNCTION), which will be executed after each tactic during
ETREE-NAT.
It takes values of type PRINT-FUNCTION-LIST and belongs to subjects @t{TRANSMIT}, @t{WINDOW-PROPS}, @t{PRINTING}, @t{ETR-NAT}.  The default value is @T{(PRFW-PALL PRFW-^P PRFW-^PN ^PN)}.

@IndexFlag(MATINGSTREE-NAME)@\
Prefix for labels associated with nodes in a matingstree.
It takes values of type SYMBOL and belongs to subjects @t{ETREES}, @t{MTREE-TOP}.  The default value is @T{MSTREE}.

@IndexFlag(MERGE-MINIMIZE-MATING)@\
If T, merging will attempt to minimize the mating
by removing any unnecessary connections. If NIL, it won't.
T will sometimes produce a more readable ND proof, but can
also take a very long time.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{ETR-NAT}, @t{ETREES}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(NAT-ETREE-VERSION)@\
Determines which version of NAT-ETREE to use:
OLD -- The original version.
HX  -- Hongwei Xi's version which is intended to work on
       any natural deduction proof, normal or not.  This
       version has problems, but might work.
CEB -- Which is intended to only work on normal proofs,
       and should in principle always work on normal proofs.
It takes values of type NAT-ETREE-VERSION-TYPE and belongs to subjects @t{ETR-NAT}.  The default value is @T{CEB}.

@IndexFlag(NATREE-DEBUG)@\
To invoke the debugging facilities mentioned in the Programmers
Guide associated with NAT-ETREE. If NATREE-VERSION is set to CEB and
NATREE-DEBUG is set to T, then the code doublechecks that a mating exists,
giving the user lots of information.  This should eventually evolve into
a flag with more choices.
It takes values of type BOOLEAN and belongs to subjects @t{MS93-1}, @t{MS92-9}, @t{MS88}, @t{MS89}, @t{MS91-6}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MATING-SEARCH}.  The default value is @T{NIL}.

@IndexFlag(REMOVE-LEIBNIZ)@\
If TRUE, selection parameters corresponding to Leibniz equality
definitions will be removed from expansion proofs during merging 
(cf. Pfenning's thesis, theorem 138).
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{ETR-NAT}, @t{ETREES}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(RENUMBER-LEAVES)@\
If this flag is T, copies of leafN will be numbered 
leafN.1, leafN.2, etc. If the flag is NIL, they will be given
the next available number, as determined by an internal counter.
It takes values of type BOOLEAN and belongs to subjects @t{JFORMS}.  The default value is @T{T}.
@End(Description)

@Section(Unification)

@Begin(Description)
@IndexFlag(APPLY-MATCH)@\
Heuristic to decide the pair that should be given to match.
UN88 procedures:
APPLY-MATCH-ALL-FRDPAIRS applies match to all flexible-rigid pairs 
   and chooses whichever will have fewest substitutions.
APPLY-MATCH-ALL-FRDPAIRS-MSV does the same, but also checks for
   MAX-SUBSTS-VAR violations at the same time.
APPLY-MATCH-MAX-SUBSTS applies match to whichever flexible-rigid
   pair is closest to exceeding the bound in MAX-SUBSTS-VAR.
   If it finds one with a unique substitution, it uses that.
APPLY-MATCH-MIN-SUBSTS is like the above, but chooses the pair
   which is farthest from the MAX-SUBSTS-VAR bound.
APPLY-MATCH-MOST-CONSTS applies match to whichever flex-rigid
   pair contains the most constant symbols.
(The last two of these are all but useless; both of the SUBSTS
versions will be disastrous if MAX-SUBSTS-VAR is NIL...)

UN90 procedures:
This flag is almost always ignored (the default behaviour is
much like APPLY-MATCH-ALL-FRDPAIRS, but see NUM-FRPAIRS and
COUNTSUBS-FIRST for more details). The exception is if it is
APPLY-MATCH-MAX-SUBSTS, in which case it will go for whichever
pair is closest to exceeding the MAX-SUBSTS-VAR bound (but will
still use NUM-FRPAIRS and COUNTSUBS-FIRST).
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{APPLY-MATCH-ALL-FRDPAIRS}.

@IndexFlag(COUNTSUBS-FIRST)@\
if NIL, the substitutions which MATCH generates for each dpair in the
unification process are generated and counted, and then MATCH is
actually applied to the variable for which this number is smallest; if
T, the substitutions are counted before they are generated, and only
those which will be applied are actually generated.
Applies to UN90 only.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(DNEG-IMITATION)@\
Determine when to produce imitation terms that contain
double negations. Only applies in UN88 when REDUCE-DOUBLE-NEG is T
(in UN88 otherwise, it is implicitly set to ALWAYS; in UN90 it is
implicitly set to CONST-FLEX).
When TPS mates two flexible literals f and g, it adds (f . ~g) as
a dpair. Because it may really have needed (g . ~f), we allow 
imitation terms to contain double negations even if REDUCE-DOUBLE-NEG
is T. The options are as follows:
ALWAYS always allows double negations to be used.
CONST forbids them for dpairs of the form (f . ~G), where G is a
  constant, but allows them otherwise.
FLEX forbids them for (f . ~g) if g was created by a double negation
  in the first place (this prevents endless cycles), but allows them
  otherwise.
CONST-FLEX forbids them in the two cases for CONST and FLEX, but allows
  them otherwise.
NEVER forbids them outright.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{CONST-FLEX}.

@IndexFlag(ETA-RULE)@\
If T, eta rule is permitted in the unification package.
This can be T or NIL for the UN88 procedure, but it can only be
T for the UN90 procedure. (In fact, UN90 ignores this flag.)
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{T}.

@IndexFlag(IMITATION-FIRST)@\
Controls whether imitations are considered before projections during
unification procedure UN88. No effect in UN90.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{T}.

@IndexFlag(LEIBNIZ-SUB-CHECK)@\
When T, check substitutions which are made for Leibniz
variables, to ensure that they are relevant in their first argument.
When NIL, don't do this.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(MAX-DUP-PATHS)@\
Any universal jform which has more than MAX-DUP-PATHS paths below it cannot get
duplicated during search process.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS88}, @t{MS89}, @t{MS91-6}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MATING-SEARCH}.  The default value is @T{INFINITY}.

@IndexFlag(MAX-SEARCH-DEPTH)@\
If non nil, search to depth MAX-SEARCH-DEPTH, else search to
arbitrary depth. Takes precedence over all other flags that may 
control the search depth in a unification tree (i.e. no tree is ever
generated to a greater depth, although other flags may cause the 
unification search to stop temporarily at a shallower depth. Used 
in all search procedures, and in UN88 and UN90. 
See flag MAX-UTREE-DEPTH also.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(MAX-UTREE-DEPTH)@\
If non-NIL, maximum depth to which unification tree is to be 
generated. Used only in UN88 procedures. This variable is incremented 
during mating-search to allow unification tree to grow to 
greater depth as the search progresses. The unification tree
is, however, never searched or generated to a depth greater than
MAX-SEARCH-DEPTH provided it is non NIL and a positive integer. One can
also consider this variable to be the initial value to which unification
trees are generated during mating-search.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{UNIFICATION}.  The default value is @T{5}.

@IndexFlag(MIN-QUICK-DEPTH)@\
The minimum depth to which a unification tree should be generated
when unification tree is searched only to non branching depth. Setting this
flag to 1 has the effect of generating the tree to non branching depth.
Applicable only to UN88.
MIN-QUICK-DEPTH is used only in the process of checking whether two
literals are potential mates. It is used to construct the connection graph.
See flag MAX-SEARCH-DEPTH also.
See MAX-SUBSTS-QUICK for a different way to achieve a similar effect.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{3}.

@IndexFlag(MS-DIR)@\
The director to be used in mating search.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS91-6}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}.  The default value is @T{QUASI-TPS1}.

@IndexFlag(MS90-3-QUICK)@\
If T, do MS88 quick unification on dpairs in MS90-3.
If NIL, don't.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{MS92-9}, @t{MS93-1}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}.  The default value is @T{NIL}.

@IndexFlag(PRUNING)@\
If T, the unification routine will prune the tree as it goes.
Only works for BREADTH-FIRST and BEST-FIRST unification, and
only then in MS88.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(REDUCE-DOUBLE-NEG)@\
If T double negations are eliminated during lambda contraction
at a unification node. This only applies in UN88.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{T}.

@IndexFlag(RIGID-PATH-CK)@\
If T, apply rigid-path checking when doing unification. If NIL, 
switch to original unification. Both UN90 and UN88 unification 
procedures are affected by the flag.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{UNIFICATION}.  The default value is @T{T}.

@IndexFlag(STOP-AT-TSN)@\
If T the unification algorithm terminates at a terminal success 
node. Otherwise, it continues generating the tree.
This only applies to UN88.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{T}.

@IndexFlag(SUBSUMPTION-CHECK)@\
Limited subsumption check should be done during unification when
this flag is set. Applies for procedures UN88 and UN90, although it is
much more useful in UN88 (UN90 does not generate as many subsumed nodes,
and so subsumption-checking tends to be a waste of time). 
See also SUBSUMPTION-NODES and SUBSUMPTION-DEPTH.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(SUBSUMPTION-DEPTH)@\
Subsumption checking takes a lot of time, compared 
to unification, which means that checking a new node may
take more time than it could possibly save, particularly
if the node is almost at the maximum depth for the 
unification tree.
In the unification tree, new nodes at depth SUBSUMPTION-DEPTH
or deeper will not be subsumption-checked; other new nodes
will be. Having SUBSUMPTION-DEPTH INFINITY means that all new 
nodes are subsumption-checked; SUBSUMPTION-DEPTH 0 is just
a slower way of turning subsumption-checking off altogether.
(You should use SUBSUMPTION-CHECK NIL to do that!)
This flag only applies when SUBSUMPTION-CHECK is T.
See also SUBSUMPTION-NODES.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{INFINITY}.

@IndexFlag(SUBSUMPTION-NODES)@\
When SUBSUMPTION-CHECK is T, this flag determines
which other nodes should be examined to see if they subsume the
new node being considered. The values are as follows, arranged
in order with the quickest first:
PATH-NODES checks only those nodes on the path from the root to
  the new node.
LEAF-NODES checks only the leaf nodes in the tree.
LP-NODES checks leaf nodes and those on the path to the
  new node.
ALL-NODES checks every node in the tree.
Some nodes will always be excluded from subsumption checking,
regardless of the value of this flag. In particular, two nodes
representing different sets of connections will not be 
compared. 
This flag only applies to the UN88 procedure; in UN90, if
subsumption-checking is used at all, it is implicitly
set to ALL-NODES.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{LP-NODES}.

@IndexFlag(TOTAL-NUM-OF-DUPS)@\
Max number of duplications allowed at any time 
during a search using path-focused duplication. Compare
NUM-OF-DUPS. This flag will be ignored if set to NIL.
THE IMPLEMENTATION OF THIS IS BUGGY; setting it to NIL is safest.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS90-3}, @t{MATING-SEARCH}, @t{IMPORTANT}.  The default value is @T{NIL}.

@IndexFlag(UNI-SEARCH-HEURISTIC)@\
Search strategy used to select the next node in the unification tree.
BREADTH-FIRST and DEPTH-FIRST are exactly as they sound;
BEST-FIRST takes whichever leaf node has the fewest free
variables (and is not already terminal).
All of these options work for UN90 (ms90-*, ms91-7, ms92-*);
BREADTH-FIRST and BEST-FIRST are the only options for UN88 (ms88, ms89, 
ms91-6, mtree).
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{BREADTH-FIRST}.

@IndexFlag(UNIF-COUNTER)@\
If this flag is non-zero, PP* will be called to 
print out information about the current unification tree at 
regular intervals. This flag determines the length of
the intervals, measured by the number of calls to the
unification procedure. The amount of information is
determined by the setting of UNIF-COUNTER-OUTPUT.
If the flag is set to 0, this feature will be turned off.
This flag only applies in UN88 unification.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{0}.

@IndexFlag(UNIF-COUNTER-OUTPUT)@\
See UNIF-COUNTER and UNIF-TRIGGER for the use of this flag.
Settings are:
0: Print the entire tree in flat format with details. (PALL)
1: Print the entire tree in flat format without details. (PALL)
2: Print the tree in tree format with subs. (UTREE*)
3: Print the tree in tree format without subs. (UTREE*)
4: Print just the subs and details in flat format. (UTREE)
5: Print just the subs in flat format. (UTREE)
6: Print full details of the last node. (P and PP*)
7: Print some details of the last node. (P and PP)
8: Print the last node and its properties only.
9: Print the statistics for the tree so far. (STATS)
10: Print the average values for STATS, after a mating is found.
This flag only applies in UN88 unification.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{0}.

@IndexFlag(UNIF-TRIGGER)@\
If this flag is non-NIL, PP* will be called to 
print out information about the current unification tree 
after certain events (compare UNIF-COUNTER).
Settings are:
NIL: Print nothing.
UTREE-END: Printout whenever a tree has come to an end
  (either failure or success; NB UNIF-COUNTER-OUTPUT 6 or 7
   will not work with this setting.)
UTREE-END1: As UTREE-END, but also gives output when quick
  unification ends a tree without completing it.
UTREE-BEGIN: Printout the root node when unification is
  first called.
PROPS-CHANGE: Printout whenever the properties of a node 
  are different from those of its parent. (Best used with
  UNIF-COUNTER-OUTPUT 6 or 7.) 
The amount of information is determined by the setting of 
UNIF-COUNTER-OUTPUT. If the flag is set to NIL, this 
feature will be turned off.
This flag only applies in UN88 unification.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(UNIFY-VERBOSE)@\
Takes values SILENT=NIL, MIN, MED or MAX=T, and governs the amount
of output relating to the unification process.
It takes values of type VERBOSE and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}, @t{UNIFICATION}.  The default value is @T{MED}.
@End(Description)

@Section(Tactics)

@Begin(Description)
@IndexFlag(DEFAULT-TACTIC)@\
The default tactic for ETREE-NAT and USE-TACTIC.
See the help messages for these commands for more information.
It takes values of type TACTIC-EXP and belongs to subjects @t{TRANSMIT}, @t{TACTICS}.  The default value is @T{(IDTAC)}.

@IndexFlag(TACMODE)@\
The default mode for tactics. 
It takes values of type TACTIC-MODE and belongs to subjects @t{TRANSMIT}, @t{TACTICS}.  The default value is @T{INTERACTIVE}.

@IndexFlag(TACTIC-VERBOSE)@\
Determines which of the three levels of verbosity will be used:
MAX -- prints the message returned by each tactic called, even if it fails.
MED -- prints messages only when tactic succeeds.
MIN -- prints nothing.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{TACTICS}.  The default value is @T{MED}.

@IndexFlag(TACUSE)@\
The default use for tactics. 
It takes values of type TACTIC-USE and belongs to subjects @t{TRANSMIT}, @t{TACTICS}.  The default value is @T{NAT-DED}.
@End(Description)

@Section(suggestions)

@Begin(Description)
@IndexFlag(GO-INSTRUCTIONS)@\
A list of instructions for GO to decide what to do with suggestions.
It is a list of pairs (priority action), action being among DO, ASK, SHOW, 
FORGET.
The default setting ((0 DO) (5 ASK) (9 SHOW) (100 FORGET)) means
do suggestions of priority 0, ask me about doing suggestions of
priority 5 or less, otherwise just show me suggestions of priority
9 or less and then quit.
It takes values of type GO-INSTRUCT and belongs to subjects @t{SUGGESTS}.  The default value is @T{((0
                                                                                                     DO)
                                                                                                    (5
                                                                                                     ASK)
                                                                                                    (9
                                                                                                     SHOW)
                                                                                                    (100
                                                                                                     FORGET))}.

@IndexFlag(QUIETLY-USE-DEFAULTS)@\
If T, GO will fill in arguments with their defaults without
asking for confirmation.  If NIL, the command will be executed like
any other command issued at the top level.
It takes values of type BOOLEAN and belongs to subjects @t{SUGGESTS}.  The default value is @T{T}.

@IndexFlag(RESOLVE-CONFLICT)@\
If T, always the first of several suggestions is chosen,
if NIL, the user will be asked.
It takes values of type BOOLEAN and belongs to subjects @t{SUGGESTS}.  The default value is @T{T}.
@End(Description)

@Section(Searchlists)

@Begin(Description)
@IndexFlag(TEST-EASIER-IF-HIGH)@\
The list of flags that, if set to high numbers, make 
mating-search easier. Used by SCALE-UP. "Easier" in this context
means "more likely to succeed eventually, although possibly
taking longer about it". Compare TEST-FASTER-IF-HIGH; the list is 
somewhat debatable, which is why you're allowed to change it.
It takes values of type TPSFLAGLIST and belongs to subjects @t{TEST-TOP}.  The default value is @T{(MAX-SEARCH-DEPTH SEARCH-TIME-LIMIT NUM-OF-DUPS MAX-UTREE-DEPTH MAX-MATES MAX-SEARCH-LIMIT)}.

@IndexFlag(TEST-EASIER-IF-LOW)@\
The list of flags that, if set to low numbers, make 
mating-search easier. Used by SCALE-UP. "Easier" in this context
means "more likely to succeed eventually, although possibly
taking longer about it". Compare TEST-FASTER-IF-LOW; the list is 
somewhat debatable, which is why you're allowed to change it.
It takes values of type TPSFLAGLIST and belongs to subjects @t{TEST-TOP}.  The default value is @T{(MIN-QUICK-DEPTH)}.

@IndexFlag(TEST-EASIER-IF-NIL)@\
The list of flags that, if set to NIL, make 
mating-search easier. Used by SCALE-UP. "Easier" in this context
means "more likely to succeed eventually, although possibly
taking longer about it". Compare TEST-FASTER-IF-NIL; the list is 
somewhat debatable, which is why you're allowed to change it.
It takes values of type TPSFLAGLIST and belongs to subjects @t{TEST-TOP}.  The default value is @T{()}.

@IndexFlag(TEST-EASIER-IF-T)@\
The list of flags that, if set to T, make 
mating-search easier.  Used by SCALE-UP. "Easier" in this context
means "more likely to succeed eventually, although possibly
taking longer about it". Compare TEST-FASTER-IF-T; the list is 
somewhat debatable, which is why you're allowed to change it.
It takes values of type TPSFLAGLIST and belongs to subjects @t{TEST-TOP}.  The default value is @T{(ETA-RULE MIN-QUANTIFIER-SCOPE MS-SPLIT)}.

@IndexFlag(TEST-FASTER-IF-HIGH)@\
The list of flags that, if set to high numbers, make 
mating-search faster.  Used by SCALE-DOWN. "Faster" in this
context means "if it succeeds at all, it does so more quickly".
Compare TEST-EASIER-IF-HIGH; the list is somewhat debatable, which is 
why you're allowed to change it.
It takes values of type TPSFLAGLIST and belongs to subjects @t{TEST-TOP}.  The default value is @T{(MIN-QUICK-DEPTH)}.

@IndexFlag(TEST-FASTER-IF-LOW)@\
The list of flags that, if set to low numbers, make 
mating-search faster. Used by SCALE-DOWN. "Faster" in this
context means "if it succeeds at all, it does so more quickly".
Compare TEST-EASIER-IF-LOW; the list is somewhat debatable, which is 
why you're allowed to change it.
It takes values of type TPSFLAGLIST and belongs to subjects @t{TEST-TOP}.  The default value is @T{(MAX-SEARCH-DEPTH SEARCH-TIME-LIMIT NUM-OF-DUPS MAX-UTREE-DEPTH MAX-MATES MAX-SEARCH-LIMIT)}.

@IndexFlag(TEST-FASTER-IF-NIL)@\
The list of flags that, if set to NIL, make 
mating-search run faster. Used by SCALE-DOWN. "Faster" in this
context means "if it succeeds at all, it does so more quickly".
Compare TEST-EASIER-IF-NIL; the list is somewhat debatable, which is 
why you're allowed to change it.
It takes values of type TPSFLAGLIST and belongs to subjects @t{TEST-TOP}.  The default value is @T{()}.

@IndexFlag(TEST-FASTER-IF-T)@\
The list of flags that, if set to T, make 
mating-search faster.  Used by SCALE-DOWN. "Faster" in this
context means "if it succeeds at all, it does so more quickly".
Compare TEST-EASIER-IF-T; the list is somewhat debatable, which is 
why you're allowed to change it.
It takes values of type TPSFLAGLIST and belongs to subjects @t{TEST-TOP}.  The default value is @T{(MIN-QUANTIFIER-SCOPE MS-SPLIT)}.

@IndexFlag(TEST-FIX-UNIF-DEPTHS)@\
If T, then LEAST-SEARCH-DEPTH will be used to fix the unification depths
MAX-UTREE-DEPTH and MAX-SEARCH-DEPTH as soon as a search in the TEST top 
level is successful, and these will not be varied again. Destructively 
alters the search list, by changing the range of these two flags to a 
single element.
It takes values of type BOOLEAN and belongs to subjects @t{TEST-TOP}.  The default value is @T{T}.

@IndexFlag(TEST-INCREASE-TIME)@\
After each unsuccessful search in the test top level,
the value of TEST-INITIAL-TIME-LIMIT will be increased by this 
proportion. (So, e.g., setting this flag to 10 will result in a 10% 
increase on each attempt; setting it to 100 will double 
TEST-INITIAL-TIME-LIMIT every time around.)
NOTE: After the first successful search, this flag will be set to
zero. The change will be permanent, in order to allow CONTINUE to
work properly.
It takes values of type INTEGER+ and belongs to subjects @t{TEST-TOP}.  The default value is @T{0}.

@IndexFlag(TEST-INITIAL-TIME-LIMIT)@\
The time limit to be used for each individual search. This
limit will be increased if it is found to be insufficient. See 
also the flags TEST-INCREASE-TIME and TEST-REDUCE-TIME. 
The time referred to will be internal time without counting 
garbage collection, if possible (see the flag EXCLUDING-GC-TIME).
It takes values of type POSINTEGER and belongs to subjects @t{TEST-TOP}.  The default value is @T{30}.

@IndexFlag(TEST-MAX-SEARCH-VALUES)@\
The maximum number of values that will be put in the
range of any flag in an automatically-generated searchlist.
(In a manually-generated list, you can have as large a range
as you like.)
It takes values of type POSINTEGER and belongs to subjects @t{TEST-TOP}.  The default value is @T{10}.

@IndexFlag(TEST-NEXT-SEARCH-FN)@\
The name of a function which should take a searchlist and the time taken for
the previous attempt as arguments, and should set the flags in the list
appropriately for the next search. This function should also return T in
*finished-flag* if all settings have been tried.
The only values defined so far are:
EXHAUSTIVE-SEARCH, which tries all combinations of flags in a searchlist, 
 varying one flag through its entire range before trying the next flag.
BREADTH-FIRST-SEARCH, which also tries all combinations of flags, but 
 varies each flag a little at a time.
PRESS-DOWN, which is used by the PRESS-DOWN command.
PRESS-DOWN-2, which behaves like breadth-first search except that if varying
 a flag makes the search faster, that flag is then prevented
 from returning above its original value (the range of each flag is 
 assumed to be ordered; if the range is (A B C D), and setting it to
 C results in a faster search, it will never again be set to A or B).
PUSH-UP, which is used by the PUSH-UP command.
PUSH-UP-2, which is like breadth-first search but terminates once a 
 successful mode is discovered; it is used for relaxing an unsuccessful
 mode until it is successful.
It takes values of type SYMBOL and belongs to subjects @t{TEST-TOP}.  The default value is @T{EXHAUSTIVE-SEARCH}.

@IndexFlag(TEST-REDUCE-TIME)@\
If T, then TEST-INITIAL-TIME-LIMIT will be reduced every time a faster 
combination of flags is found. If NIL, then it won't be.
It takes values of type BOOLEAN and belongs to subjects @t{TEST-TOP}.  The default value is @T{T}.

@IndexFlag(TEST-VERBOSE)@\
If NIL, suppresses a lot of the output of the test top level.
It takes values of type BOOLEAN and belongs to subjects @t{TEST-TOP}.  The default value is @T{T}.

@IndexFlag(TESTWIN-HEIGHT)@\
Contains the initial height of the testwindow.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{TEST-TOP}.  The default value is @T{24}.

@IndexFlag(TESTWIN-WIDTH)@\
Contains the initial width of the testwindow.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{TEST-TOP}.  The default value is @T{80}.
@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexFlag(ALLOW-NONLEAF-CONNS)@\
The value of this flag is a list of symbols.
If ALL is in the list, then the jform contains literals for each node
(except LAMBDA rewrites).

If REWRITES is in the list, then the jform contains literals
for each rewrite node (except LAMBDA's).

If the name of an etree node is in the list, then the jform contains literals
for the specified node.

NOTE:  This flag affects the way jforms are generated.  Consequently,
different search procedures may (or may not) be affected by it.
It takes values of type SYMBOLLIST and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}.  The default value is @T{()}.

@IndexFlag(DISSOLVE)@\
DISSOLVE is set to a list of connections which are used to perform dissolution
when forming the jform from the etree.  If the list of connections is NIL the jform
is constructed as usual.
(See Murray, Rosenthal, Dissolution: Making Paths Vanish, JACM 40, 3, July 1993, pp. 504-535)
It takes values of type MATINGPAIRLIST and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}.  The default value is @T{()}.

@IndexFlag(LIT-NAME)@\
Prefix for labels associated with literals.
It takes values of type SYMBOL and belongs to subjects @t{JFORMS}.  The default value is @T{LIT}.

@IndexFlag(MATE-UP-TO-NNF)@\
If MATE-UP-TO-NNF is T, then literals represent the
negation normal form of formulas or their negation.
This allows connections between formulas that are
only equal up to negation normal form.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(ORDER-COMPONENTS)@\
When T or PATHNUM, the components of a jform node will be
rearranged in order of the number of paths which lie below them (go 
through them).
When T-REVERSED or PATHNUM-REVERSED, the components of a jform node will be 
rearranged in reverse order of the number of paths which lie below them (go 
through them).
When NIL or COMMON, then the jform of the current eproof will not be modified 
by the mating search;
When REVERSE, the order of the components in the jform of current eproof will 
be reversed;
When PREFER-RIGID2, the order of the components in the jform of the current 
eproof will be sorted in terms of the number of rigid literals in a jform 
before beginning the mating search.
When PREFER-RIGID3, the components in the jform of the current eproof will 
be sorted as for PREFER-RIGID2, but with preference given to literals that 
arise from DUAL rewriting.

(PREFER-RIGID1 is still available; it is an obsolete version of PREFER-RIGID2.)
It takes values of type ORDERCOM and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{IMPORTANT}, @t{JFORMS}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(PRINT-LIT-NAME)@\
If the value of this flag is true, labels (instead
 of wffs associated with literal, or neg-literal) are printed inside
 the editor.
It takes values of type BOOLEAN and belongs to subjects @t{JFORMS}.  The default value is @T{T}.

@IndexFlag(PRINTVPDFLAG)@\
If T, vertical path diagrams are written into the VPD-FILENAME
whenever wffs are written into the PRINTEDTFILE.  In particular PRINTEDTFLAG
must be T, for the automatic writing to take place.
It takes values of type BOOLEAN and belongs to subjects @t{EDITOR}, @t{JFORMS}.  The default value is @T{NIL}.

@IndexFlag(TEXFORMAT)@\
HPD for a horizontal path diagram (p.d.) of the positive wff.
VPD for a vertical p.d. of the negated wff.
VPP (or anything else) for a vertical p.d. of the positive wff.
It takes values of type SYMBOL and belongs to subjects @t{JFORMS}.  The default value is @T{VPP}.

@IndexFlag(VPD-BRIEF)@\
The default value for BRIEF when printing VP diagrams in a file.
Currently the options are:
T   = no atom values will show in VP diagram
A   = atom values but no labels will appear in VP diagram
NIL = atom values and labels will show in VP diagram
LT  = atom values and  labels and a legend will show in VP diagram 
L   = labels but no atom values will show in VP diagram,
      and a legend will show both
B   = boxed labels and atoms will show in the VP diagram.
BT  = boxed labels will show in the diagram, and the atom values
      will be listed below.
B and BT only work in TeX format (i.e. with the VPT command).
It takes values of type VPFORMAT and belongs to subjects @t{JFORMS}.  The default value is @T{L}.

@IndexFlag(VPD-FILENAME)@\
Default filename when printing VP diagrams in a file.
It takes values of type FILESPEC and belongs to subjects @t{JFORMS}.  The default value is @T{"vpd.vpf"}.

@IndexFlag(VPD-LIT-NAME)@\
Prefix for labels associated with literals when VP diagrams are
created automatically within the editor.
It takes values of type SYMBOL and belongs to subjects @t{JFORMS}.  The default value is @T{V}.

@IndexFlag(VPD-PTYPES)@\
If T, print types when printing VP diagrams in a file.
It takes values of type BOOLEAN and belongs to subjects @t{JFORMS}.  The default value is @T{T}.

@IndexFlag(VPD-STYLE)@\
The default value for STYLE when printing VP diagrams in a file.
It takes values of type VPSTYLE and belongs to subjects @t{JFORMS}.  The default value is @T{GENERIC}.

@IndexFlag(VPD-VPFPAGE)@\
The default value for the width of the page when printing VP diagrams in a file.
It takes values of type POSINTEGER and belongs to subjects @t{JFORMS}.  The default value is @T{78}.

@IndexFlag(VPFORM-LABELS)@\
In the editor, a value of T for this flag will suppress
printing of labels in vpforms; if it is NIL, labels and atom values
will be printed.
If this flag is set the default value for argument BRIEF will
be A. Unless one decides to override the default value, labels will not be
printed.  This flag has no effect on the editor command VPD, and on the
wffop DISPLAY-VPD.  To suppress labels when using these commands, please
set the flag VPD-BRIEF to A.
It takes values of type BOOLEAN and belongs to subjects @t{JFORMS}.  The default value is @T{NIL}.

@IndexFlag(VPFORM-TEX-MAGNIFICATION)@\
The magnification factor to use for TeX files containing vpforms.
This has two possible settings: if it is lower than 10, then it is used
in the form \magnification=\magstepN
Roughly, 0 = 10pt, 1 = 12pt, 2 = 14pt, 3 = 17pt, 5 = 25pt.

Otherwise, it is used in the form \magnificationN, in which case 
1000 corresponds to "normal size" (12pt), 800 is 80%, 1200 is 120%, and
so on.
It takes values of type INTEGER+ and belongs to subjects @t{JFORMS}.  The default value is @T{1000}.

@IndexFlag(VPFORM-TEX-NEST)@\
Maximal number of boxes to nest in path diagrams for TeX.
0 means not to break into boxes.
It takes values of type INTEGER+ and belongs to subjects @t{JFORMS}.  The default value is @T{4}.

@IndexFlag(VPFORM-TEX-PREAMBLE)@\
The string to be put at the beginning of a TeX file containing
vpforms.
It takes values of type STRING and belongs to subjects @t{JFORMS}.  The default value is @T{""}.

@IndexFlag(VPW-HEIGHT)@\
Contains the intial height of the vpform window; there is no need to update
this if the window is resized after being opened.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{JFORMS}.  The default value is @T{25}.

@IndexFlag(VPW-WIDTH)@\
Contains the current width of the vpform window; should be updated by the 
user if the window is resized after being opened.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{JFORMS}.  The default value is @T{120}.
@End(Description)

@Section(Semantics)

@Begin(Description)
@IndexFlag(MAX-BINDER-COMPUTATION)@\
The maximum number of elements TPS is willing to consider when
interpreting binders (quantifiers and lambdas) in a model.  This depends on the size of
domains and the nesting of binders in the formula.
It takes values of type INTEGER+ and belongs to subjects @t{MS04-2}, @t{SEMANTIC-BOUNDS}.  The default value is @T{1048576}.

@IndexFlag(MAX-DOMAIN-SIZE)@\
The maximum size of semantic domains TPS will consider. It does not
make sense to set this to any value other than a size such a domain
may have.  For example, the default value 2^16 is 65536.  Assuming
every base type is of size 2, the next reasonable value would be 2^32,
which is over 4 billion.  Consequently, the value of this flag should
not be changed until TPS is either considering models other than
standard models based on powers of 2 or computing power increases
tremendously.
It takes values of type INTEGER+ and belongs to subjects @t{MS04-2}, @t{SEMANTIC-BOUNDS}.  The default value is @T{65536}.
@End(Description)

@Section(Printing)

@Begin(Description)
@IndexFlag(REWRITING-RELATION-SYMBOL)@\
Contains the symbol that is printed between lines obtained
by rewriting from immediately preceding lines.
It takes values of type SYMBOL and belongs to subjects @t{S-EQN}.  The default value is @T{=}.

@IndexFlag(VERBOSE-REWRITE-JUSTIFICATION)@\
When set to T, justification of lines obtained by rewriting
in the REWRITING top level will indicate the rewriting theory used to
obtain the transformation.
It takes values of type BOOLEAN and belongs to subjects @t{S-EQN}.  The default value is @T{T}.
@End(Description)

@Section(Applying Rules)

@Begin(Description)
@IndexFlag(APP*-REWRITE-DEPTH)@\
The maximal rewrite depth of an app* application.
It takes values of type NULL-OR-POSINTEGER and belongs to subjects @t{S-EQN}.  The default value is @T{50}.

@IndexFlag(REWRITING-AUTO-DEPTH)@\
The maximal depth of a search tree when applying AUTO. For the
SIMPLE search procedure, the number corresponds to the maximal rewrite
depth, whereas for BIDIR and BIDIR-SORTED the maximal search depth is
twice the specified number.
It takes values of type POSINTEGER and belongs to subjects @t{S-EQN}.  The default value is @T{5}.

@IndexFlag(REWRITING-AUTO-GLOBAL-SORT)@\
When NIL, BIDIR-SORTED will choose the next wff to be rewritten from
the successors of the current wff. When T, it will choose the next wff from
all unexplored wffs obtained so far from the initial or the target wff,
respectively. See the flag REWRITING-AUTO-SEARCH-TYPE.
It takes values of type BOOLEAN and belongs to subjects @t{S-EQN}.  The default value is @T{NIL}.

@IndexFlag(REWRITING-AUTO-MAX-WFF-SIZE)@\
The maximal size of a wff to be rewritten when applying AUTO.
It takes values of type POSINTEGER and belongs to subjects @t{S-EQN}.  The default value is @T{15}.

@IndexFlag(REWRITING-AUTO-MIN-DEPTH)@\
The minimal depth of a search tree needed by AUTO to find a
derivation. The value should be less or equal to that of REWRITING-AUTO-DEPTH,
otherwise no search will be performed.
It takes values of type INTEGER+ and belongs to subjects @t{S-EQN}.  The default value is @T{0}.

@IndexFlag(REWRITING-AUTO-SEARCH-TYPE)@\
The search procedure to use with AUTO. Currently defined are SIMPLE,
BIDIR and BIDIR-SORTED. BIDIR-SORTED will try to rewrite shorter wffs first.
When this is not needed, use BIDIR. The precise behaviour of BIDIR-SORTED
depends on the flag REWRITING-AUTO-GLOBAL-SORT.
It takes values of type AUTO-SEARCHTYPE and belongs to subjects @t{S-EQN}.  The default value is @T{BIDIR-SORTED}.

@IndexFlag(REWRITING-AUTO-SUBSTS)@\
List of terms to substitute for any free variables which may be
introduced during rewriting by AUTO. If NIL, the list will be generated
automatically from atomic subwffs of the source and the target wff.
It takes values of type GWFFLIST and belongs to subjects @t{S-EQN}.  The default value is @T{()}.

@IndexFlag(REWRITING-AUTO-TABLE-SIZE)@\
The maximal size of a search table used by AUTO. Note that while the
SIMPLE search procedure uses only one table of that size, BIDIR and
BIDIR-SORTED use two.
It takes values of type POSINTEGER and belongs to subjects @t{S-EQN}.  The default value is @T{10000}.
@End(Description)

@Section(Propositional Rules)

@Begin(Description)
@IndexFlag(RULEP-MAINFN)@\
The main function used for RULEP.  Defaults to RULEP-DELUXE, in
which case RULEP will find a minimal subset of the support lines
which suffices to justify the planned line.  If set to RULEP-SIMPLE,
RULEP will merely check that the planned line follows from the
support lines that are specified by the user.
It takes values of type RULEP-MAINFN-TYPE and belongs to subjects @t{RULES-MOD}.  The default value is @T{RULEP-DELUXE}.
@End(Description)

@Section(Wff Editor)

@Begin(Description)
@IndexFlag(EDPPWFFLAG)@\
If T, wffs are always pretty-printed in the formula editor.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{NIL}.

@IndexFlag(EDPRINTDEPTH)@\
The depth to which wffs are printed in the formula editor.
It takes values of type INTEGER+ and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{24}.

@IndexFlag(EDWIN-CURRENT)@\
If T, the Current Edwff window is opened to display the current
   wff being edited when the editor is started.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{T}.

@IndexFlag(EDWIN-CURRENT-HEIGHT)@\
Controls the initial height of the Current Edwff window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{EDITOR}.  The default value is @T{3}.

@IndexFlag(EDWIN-CURRENT-WIDTH)@\
Controls the initial width of the Current Edwff window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{EDITOR}.  The default value is @T{80}.

@IndexFlag(EDWIN-TOP)@\
If T, the Top Edwff window is opened to display the entire
   wff being edited when the editor is started.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{T}.

@IndexFlag(EDWIN-TOP-HEIGHT)@\
Controls the initial height of the Top Edwff window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{EDITOR}.  The default value is @T{3}.

@IndexFlag(EDWIN-TOP-WIDTH)@\
Controls the initial width of the Top Edwff window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{EDITOR}.  The default value is @T{80}.

@IndexFlag(EDWIN-VPFORM)@\
If T, the Current Vpform window is opened 
   to display the vpform of the current wff being edited 
   when the editor is started. This flag is ignored in ETPS, where
   the Vpform window is never opened.
It takes values of type BOOLEAN and belongs to subjects @t{PRINTING}, @t{EDITOR}.  The default value is @T{NIL}.

@IndexFlag(EDWIN-VPFORM-HEIGHT)@\
Controls the initial height of the Current Vpform window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{EDITOR}.  The default value is @T{30}.

@IndexFlag(EDWIN-VPFORM-WIDTH)@\
Controls the initial width of the Current Vpform window.
It takes values of type POSINTEGER and belongs to subjects @t{WINDOW-PROPS}, @t{EDITOR}.  The default value is @T{60}.
@End(Description)

@Section(wff Primitives)

@Begin(Description)
@IndexFlag(META-BDVAR-NAME)@\
The prefix for names of bound meta variables.
It takes values of type SYMBOL and belongs to subjects @t{INTERNAL-NAMES}.  The default value is @T{BD}.

@IndexFlag(META-VAR-NAME)@\
The prefix for names of meta variables.
It takes values of type SYMBOL and belongs to subjects @t{INTERNAL-NAMES}.  The default value is @T{MV}.

@IndexFlag(REN-VAR-FN)@\
The value of this flag is a function to be called when a variable
must be renamed automatically. It has three possible settings:
REN-VAR-X1 is the standard renaming function. It renames y to y^1,
 then to y^2, and so on. If there is another variable y, of a different 
 type, it makes no difference.
REN-VAR-X11 is much like REN-VAR-X1, except it will avoid creating
 two variables of the same name at different types (so it tends to 
 produce higher exponents than REN-VAR-X1).
REN-VAR-XA renames alphabetically, turning y into ya, then yba, 
 and so on.
It takes values of type SYMBOL and belongs to subjects @t{WFF-PRIMS}.  The default value is @T{REN-VAR-X1}.

@IndexFlag(RENAME-ALL-BD-VARS)@\
When T, all bound variables inside a definition will be
renamed before instantiation.
It takes values of type BOOLEAN and belongs to subjects @t{WFF-PRIMS}.  The default value is @T{NIL}.
@End(Description)

@Section(Wff Parsing)

@Begin(Description)
@IndexFlag(BASE-TYPE)@\
If not NIL, it should be the `default' type for individual
variables in a logic system.  Typically I (for iota).
It takes values of type SYMBOL and belongs to subjects @t{PARSING}.  The default value is @T{I}.

@IndexFlag(FIRST-ORDER-MODE-PARSE)@\
If T, every letter by itself is a symbol for the parser,
with the exception of keywords like FORALL, AND etc.,
which can be in mixed case.  If NIL, symbols must be separated by
spaces (or brackets, dots, etc.).
It takes values of type BOOLEAN and belongs to subjects @t{PARSING}.  The default value is @T{NIL}.

@IndexFlag(LOWERCASERAISE)@\
If T, lower case characters will be raised to upper case, when read.
Has no effect in first-order mode.
It takes values of type BOOLEAN and belongs to subjects @t{PARSING}.  The default value is @T{NIL}.

@IndexFlag(TYPE-IOTA-MODE)@\
If T, type variables are always assumed to be iota.
It takes values of type BOOLEAN and belongs to subjects @t{PARSING}.  The default value is @T{T}.

@IndexFlag(UNTYPED-LAMBDA-CALCULUS)@\
Takes values T or NIL. To set it to T if you want to use the editor to deal with 
untyped lambda-calculus.
It takes values of type BOOLEAN and belongs to subjects @t{EDITOR}.  The default value is @T{NIL}.
@End(Description)

@Section(Basic Abbreviations)

@Begin(Description)
@IndexFlag(REWRITE-EQUALITIES)@\
One of the following:
NONE: do not rewrite equalities
ONLY-EXT: rewrite only those equalities that can be rewritten using
          extensionality.
LEIBNIZ: rewrite all equalities using the Leibniz definition.
ALL: rewrite all equalities, to an equivalence for those of type OOO,
     to the extensional form 
      [lambda f(AB) lambda g(AB) forall x(B) f x = g x]
     for those of type O(AB)(AB), and to the Leibniz form
      [lambda x(A) lambda y(A) forall q(OA). q x implies q y]
     for those of type OAA.
LAZY2: As for ALL, but keeping a duplicate leaf as in the LAZY2
       setting of the flag REWRITE-DEFNS.
PARITY1: Uses the parity to determine whether equalities should be
    rewritten as the setting LEIBNIZ or as the setting ALL.  For example,
    using PARITY1 when trying to prove the wff 
              A(OI) = B(OI) implies C
    the equality is expaned using Leibniz, and when trying to prove the wff
              D implies A(OI) = B(OI)
    the equality is expanded using extensionality.  The heuristic
    is that we often use the substitutivity property when we use an equation
    and use extensionality to show an equation.
It takes values of type REWRITE-DEFNS and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{IMPORTANT}, @t{WFF-PRIMS}, @t{MATING-SEARCH}.  The default value is @T{ALL}.
@End(Description)

@Section(Lambda-Calculus)

@Begin(Description)
@IndexFlag(LAMBDA-CONV)@\
BETA-ETA-TOGETHER means that BETA and ETA rules are used together; 
BETA-ETA-SEPARATE means BETA and ETA rules are used separately; BETA-ONLY 
means that only BETA rule is allowed.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{TACTICS}, @t{ETR-NAT}, @t{ETREES}.  The default value is @T{BETA-ETA-TOGETHER}.
@End(Description)

@Section(Primitive Substitutions)

@Begin(Description)
@IndexFlag(BAD-VAR-CONNECTED-PRUNE)@\
When generating set constraints, prune those which do not
have bad variables (selected variables the set variable cannot depend upon)
shared between the literals in the constraints.  For example,
if p cannot depend on x or y, the constraints

   p 0 -> A x
   p x -> A 0
   p x -> A x, B y
   p y -> A x, B y

would be pruned while the constraints

   p x -> A x
   p x -> A x y, B y

would not be pruned.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{IMPORTANT}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(DELAY-SETVARS)@\
If T, first solve the rigid part of the jform,
then try to solve the flexible parts using setvar constraints.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}.  The default value is @T{NIL}.

@IndexFlag(INCLUDE-COINDUCTION-PRINCIPLE)@\
When solving co-closure set-variable constraints we
include in the lemma a higher-order statement that we have
the greatest solution.

For example, suppose we want a set N such that

~X 0
and
forall z [X [f z] implies [X z]]

If include-coinduction-principle is set to T, then the lemma
will include a conjunct of the form

forall p . ~[p 0] and [forall z [p [f z] implies [p z]]]
       implies forall x [p x implies N x].
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{PRIMSUBS}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(INCLUDE-INDUCTION-PRINCIPLE)@\
When solving closure set-variable constraints we
include in the lemma a higher-order statement that we have
the least solution.

For example, suppose we want a set N such that

N 0
and
forall n [N n implies [N [S n]]]

If include-induction-principle is set to T, then the lemma
will include a conjunct of the form

forall p . p 0 and [forall n [p n implies [p [S n]]]]
       implies forall x [N x implies p x].
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{PRIMSUBS}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(MAX-CONSTRAINT-SIZE)@\
Maximum number of literals allowed in a single constraint
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{IMPORTANT}, @t{MATING-SEARCH}.  The default value is @T{3}.

@IndexFlag(MAX-NUM-CONSTRAINTS)@\
Maximum number of combined constraints in each constraint set.
It takes values of type INTEGER+-OR-INFINITY and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{IMPORTANT}, @t{MATING-SEARCH}.  The default value is @T{2}.

@IndexFlag(MAX-PRIM-DEPTH)@\
Maximum depth to which primsubs with quantifiers are generated.
The types of the quantified variables range over the values in PRIM-BDTYPES.
With PRIMSUB-METHOD PR89 : 
 This flag is ignored. Primsubs of the form "exists x . literal" and 
 "forall x . literal" will be generated.
With PRIMSUB-METHOD PR93 :
 At depth 1, a single quantifier is introduced, as in PR89. 
 At depth N>1, we have (N-1) quantifiers ranging over a formula
 containing (N-1) conjunctions {disjunctions} of (N-2) 
 disjunctions {conjunctions}.
With PRIMSUB-METHOD PR95 :
 At depth 1, as in PR89.
 At depth N>1, we have (N-1) quantifiers ranging over a formula
 with between MIN-PRIM-LITS and MAX-PRIM-LITS literals, with
 all combinations of connectives between them.
With PRIMSUB-METHOD PR97 :
 At depth N>0, we have (N-1) quantifiers ranging over each 
 subformula taken from the etree which contains between 
 MIN-PRIM-LITS and MAX-PRIM-LITS literals. You can see these
 subformulas by doing ETP from the MATE top level.
With PRIMSUB-METHOD PR97A :
 As in PR97, but all substitutions are in negation normal form.
With PRIMSUB-METHOD PR97B :
 The substitutions from PR97A and PR95 are interleaved. The order
 is determined firstly by the number of literals, then by the number of
 quantifiers, and lastly with PR97 substs taking precedence over PR95.
With PRIMSUB-METHOD PR97C :
 If set to N, all primsubs will have < N quantifiers.
With PRIMSUB-METHOD PR00  :
 This is ignored.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{PRIMSUBS}.  The default value is @T{1}.

@IndexFlag(MAX-PRIM-LITS)@\
Maximum no. of literals allowed in a primsub.
Does not apply for PRIMSUB-METHOD PR89 or PR93. 
See the help message for MIN-PRIM-DEPTH, which explains how primsubs
are generated.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{PRIMSUBS}.  The default value is @T{4}.

@IndexFlag(MIN-PRIM-DEPTH)@\
Minimum depth at which primsubs with quantifiers are generated.
The types of the quantified variables range over the values in PRIM-BDTYPES.
With PRIMSUB-METHOD PR89 : 
 This flag is ignored. Primsubs of the form "exists x . literal" and 
 "forall x . literal" will be generated.
With PRIMSUB-METHOD PR93 :
 At depth 1, a single quantifier is introduced, as in PR89. 
 At depth N>1, we have (N-1) quantifiers ranging over a formula
 containing (N-1) conjunctions {disjunctions} of (N-2) 
 disjunctions {conjunctions}.
With PRIMSUB-METHOD PR95 :
 At depth 1, as in PR89.
 At depth N>1, we have (N-1) quantifiers ranging over a formula
 with between MIN-PRIM-LITS and MAX-PRIM-LITS literals, with
 all combinations of connectives between them.
With PRIMSUB-METHOD PR97 :
 At depth N>0, we have (N-1) quantifiers ranging over each 
 subformula taken from the etree which contains between 
 MIN-PRIM-LITS and MAX-PRIM-LITS literals. You can see these
 subformulas by doing NAME-PRIM from the MATE top level.
With PRIMSUB-METHOD PR97A :
 As in PR97, but all substitutions are in negation normal form.
With PRIMSUB-METHOD PR97B :
 The substitutions from PR97A and PR95 are interleaved. The order
 is determined firstly by the number of literals, then by the number of
 quantifiers, and lastly with PR97 substs taking precedence over PR95.
With PRIMSUB-METHOD PR97C :
 If set to N, the number of quantifiers in any primsub will be >= N-1.
With PRIMSUB-METHOD PR00  :
 The value is ignored.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{PRIMSUBS}.  The default value is @T{1}.

@IndexFlag(MIN-PRIM-LITS)@\
Minimum no. of literals allowed in a primsub.
Does not apply for PRIMSUB-METHOD PR89 or PR93. 
See the help message for MIN-PRIM-DEPTH, which explains how primsubs
are generated.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{PRIMSUBS}.  The default value is @T{2}.

@IndexFlag(NEG-PRIM-SUB)@\
When T, one of the primitive substitutions will introduce negation.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}.  The default value is @T{NIL}.

@IndexFlag(PR00-ALLOW-SUBNODE-CONNS)@\
If T, we allow connections between nodes and their subnodes.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}.  The default value is @T{T}.

@IndexFlag(PR00-MAX-SUBSTS-VAR)@\
The setting for MAX-SUBSTS-VAR when generating
set variable instantiations by unification
using PRIMSUB-METHOD PR00.
It takes values of type NULL-OR-INTEGER and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}, @t{PRIMSUBS}.  The default value is @T{4}.

@IndexFlag(PR00-NUM-ITERATIONS)@\
Number of times to iterate the PR00 Set Substitution process.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{PRIMSUBS}.  The default value is @T{1}.

@IndexFlag(PR00-REQUIRE-ARG-DEPS)@\
If T, do not consider set subsitutions which do not
depend on some argument.  For example, do not consider
    P --> lambda x y PHI
where neither x nor y is free in PHI.  This often rules out many
setsubs generated by unification.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}.  The default value is @T{NIL}.

@IndexFlag(PR97C-MAX-ABBREVS)@\
The maximum number of abbreviations that may appear in a PR97C primsub.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{PRIMSUBS}.  The default value is @T{1}.

@IndexFlag(PR97C-PRENEX)@\
If T, PR97C generates substitutions in prenex normal form. If NIL, it doesn't.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{PRIMSUBS}.  The default value is @T{T}.

@IndexFlag(PRIM-BDTYPES)@\
List of types of quantified variables used to construct primitive
substitutions. This list will always be used when constructing primitive
substitutions interactively, but see the flag PRIM-BDTYPES-AUTO for more
information on the types that will be used by automatic search procedures.
It takes values of type TYPESYMLIST-NIL and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{PRIMSUBS}.  The default value is @T{("I")}.

@IndexFlag(PRIM-BDTYPES-AUTO)@\
Has five possible values: REPLACE, REPLACE-SUB, APPEND, 
APPEND-SUB and IGNORE.
Determines how the procedures that use primitive substitutions
handle the flag PRIM-BDTYPES, as follows:
REPLACE -- the value of PRIM-BDTYPES will be changed to an 
automatically-generated list of all the primitive types used in 
the gwff to be proven.
REPLACE-SUB -- as for replace, except that the list will be of all
the subtypes of the types that appear in the gwff.
APPEND -- the same list is calculated as for REPLACE, but instead
of replacing the current setting of PRIM-BDTYPES it will be appended
to it.
APPEND-SUB -- the same list is calculated as for APPEND, but instead
of replacing the current setting of PRIM-BDTYPES it will be appended
to it.
IGNORE -- no list will be generated, and the user's setting of 
PRIM-BDTYPES will be left intact.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{PRIMSUBS}.  The default value is @T{REPLACE}.

@IndexFlag(PRIM-PREFIX)@\
Prefix for weak labels associated with primitive substitutions.
It takes values of type SYMBOL and belongs to subjects @t{PRIMSUBS}.  The default value is @T{PRIM}.

@IndexFlag(PRIMSUB-METHOD)@\
Takes one of the values PR89, PR93, PR95, PR97, PR97A, PR97B.
This determines how primsubs will be generated, in 
conjunction with MAX-PRIM-DEPTH, MIN-PRIM-DEPTH, 
MAX-PRIM-LITS and MIN-PRIM-LITS.
With PRIMSUB-METHOD PR89 : 
 Primsubs of the form "exists x . literal" and 
 "forall x . literal" will be generated.
With PRIMSUB-METHOD PR93 :
 For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
  At depth 1, a single quantifier is introduced, as in PR89. 
  At depth N>1, we have (N-1) quantifiers ranging over a formula
  containing (N-1) conjunctions {disjunctions} of (N-2) 
  disjunctions {conjunctions}.
With PRIMSUB-METHOD PR95 :
 For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
  At depth 1, as in PR89.
  At depth N>1, we have (N-1) quantifiers ranging over a formula
  with between MIN-PRIM-LITS and MAX-PRIM-LITS literals, with
  all combinations of connectives between them.
With PRIMSUB-METHOD PR97 :
 For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
  At depth N>0, we have (N-1) quantifiers ranging over each 
  subformula taken from the etree which contains between 
  MIN-PRIM-LITS and MAX-PRIM-LITS literals. You can see these
 subformulas by doing NAME-PRIM from the MATE top level. (Note:
 both the instantiated and uninstantiated versions of each 
 definition are used.)
With PRIMSUB-METHOD PR97A :
 As in PR97, but all substitutions are in negation normal form.
With PRIMSUB-METHOD PR97B :
 The substitutions from PR97A and PR95 are interleaved. The order
 is determined firstly by the number of literals, then by the number of
 quantifiers, and lastly with PR97 substs taking precedence over PR95.
With PRIMSUB-METHOD PR97C :
 Using the connectives AND and OR, and the quantifiers EXISTS and
 FORALL (ranging over variables of types PRIM-BDTYPES), and also using
 any abbreviations or equalities that occur in the gwff to be proven, 
 primsubs are built up using the bounds given by MIN- and MAX-PRIM-LITS
 and MIN- and MAX-PRIM-DEPTH. See also PR97C-PRENEX and PR97C-MAX-ABBREVS.
With PRIMSUB-METHOD PR00  :
 This uses higher order unification to determine set substitutions
 that solve part of the mating search in advance.  PR00 only works with 
 DEFAULT-MS MS98-1 and SKOLEM-DEFAULT NIL. PR00 can be controlled using the flags
 PR00-MAX-SUBSTS-VAR, PR00-REQUIRE-ARG-DEPS, PR00-NUM-ITERATIONS.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{IMPORTANT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{PRIMSUBS}.  The default value is @T{PR93}.

@IndexFlag(WHICH-CONSTRAINTS)@\
Which kinds of set constraints should be generated and solved.

. MAX:  Constraints for p of the form Psi | p t ==> Gamma(p)
        solved using maximal solution.
. MIN:  Constraints for p of the form Psi | Gamma(p) ==> p t
        solved using minimal solution.
. PR00: Generates instantiated ftrees and connections by mating
        nonleaves.
It takes values of type SYMBOLLIST and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}, @t{IMPORTANT}, @t{MATING-SEARCH}.  The default value is @T{(MAX MIN)}.
@End(Description)

@Section(Miscellaneous)

@Begin(Description)
@IndexFlag(REWRITE-EQUIVS)@\
This chooses one of the two ways of constructing an etree
from an equivalence A EQUIV B:
1 chooses the option with the fewest vertical paths
   (positive: A AND B OR ~A AND ~B
    negative: A IMPLIES B AND B IMPLIES A)
2 chooses the option with the fewest horizontal paths
   (negative: A AND B OR ~A AND ~B
    positive: A IMPLIES B AND B IMPLIES A)
3 behaves as for 2 except for the first equivalence it finds, 
  when it behaves as for 1. (This means that a gwff which is a 
  quantified equivalence will produce an etree which can be split.)
4 always chooses A IMPLIES B AND B IMPLIES A
5 always chooses A AND B OR ~A AND ~B
Any other setting will behave like 1.

This does not work with MIN-QUANTIFIER-SCOPE T; in that case, 
etrees will be constructed as in case 1, regardless of the setting
of this flag.
It takes values of type POSINTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{IMPORTANT}, @t{MATING-SEARCH}.  The default value is @T{1}.
@End(Description)

@Section(RuleP)

@Begin(Description)
@IndexFlag(RULEP-WFFEQ)@\
The wffop used for testing whether two wffs are equal when checking
RULEP and propositional mating search.
It takes values of type SYMBOL and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{MATING-SEARCH}, @t{JFORMS}.  The default value is @T{WFFEQ-AB}.
@End(Description)

@Section(Skolemizing)

@Begin(Description)
@IndexFlag(NAME-SKOLEM-FN)@\
Name of the functions which names a Skolem function.
It takes values of type SYMBOL and belongs to subjects @t{WFF-PRIMS}.  The default value is @T{NAME-SKOLEM-CAP}.
@End(Description)

@Section(Quantifiers)

@Begin(Description)
@IndexFlag(UI-HERBRAND-LIMIT)@\
Maximum number of times to apply ui-herbrand-tac to the
same universally-quantified formula.
It takes values of type POSINTEGER and belongs to subjects @t{TACTICS}.  The default value is @T{3}.
@End(Description)

@Section(Auxiliary)

@Begin(Description)
@IndexFlag(USE-RULEP)@\
When true, indicates that RuleP should be used when possible
in translating from expansion proof to natural deduction proof.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{TACTICS}, @t{ETR-NAT}, @t{MATING-SEARCH}.  The default value is @T{T}.

@IndexFlag(USE-SYMSIMP)@\
When true, indicates that symmetric simplification should be 
used when possible in translating from expansion proof to natural deduction 
proof.  Consult Pfenning's thesis for a description of symmetric
simplification.
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS91-6}, @t{MS90-9}, @t{MS90-3}, @t{MS89}, @t{MS88}, @t{TACTICS}, @t{ETR-NAT}, @t{MATING-SEARCH}.  The default value is @T{T}.
@End(Description)

@Section(Events)

@Begin(Description)
@IndexFlag(ADVICE-ASKED-ENABLED)@\
If NIL, recording events of type ADVICE-ASKED is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(ADVICE-FILE)@\
The file recording advice.
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"etps3.advice"}.

@IndexFlag(COMMAND-ENABLED)@\
If NIL, recording events of type COMMAND is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(COMMAND-FILE)@\
The file recording commands.
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"/home/pa01/etps3.command"}.

@IndexFlag(DONE-EXC-ENABLED)@\
If NIL, recording events of type DONE-EXC is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(ERROR-ENABLED)@\
If NIL, recording events of type ERROR is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(ERROR-FILE)@\
The file recording the events of errors.
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"etps3.error"}.

@IndexFlag(EVENT-CYCLE)@\
The indivisible unit in number of inputs.
When WRITE-WHEN for an EVENT is `n', the event info will be written
every n * event-cycle inputs.  n=0 means don't write.
It takes values of type INTEGER+ and belongs to subjects @t{EVENTS}.  The default value is @T{5}.

@IndexFlag(EVENTS-ENABLED)@\
If nil, all events are disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(INPUT-ERROR-ENABLED)@\
If NIL, recording events of type INPUT-ERROR is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(INPUT-ERROR-FILE)@\
The file recording illegal inputs caught by TPS.
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"etps3.ierror"}.

@IndexFlag(PROOF-ACTION-ENABLED)@\
If NIL, recording events of type PROOF-ACTION is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(PROOF-FILE)@\
The file recording started and completed proofs.
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"/home/pa01/etps3.proof"}.

@IndexFlag(QUIET-EVENTS)@\
If T, no message will be given when events are written.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(RULE-ERROR-ENABLED)@\
If NIL, recording events of type RULE-ERROR is disabled.
It takes values of type BOOLEAN and belongs to subjects @t{EVENTS}.  The default value is @T{T}.

@IndexFlag(RULE-ERROR-FILE)@\
The file recording illegal rules caught by TPS.
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"etps3.rerror"}.

@IndexFlag(SCORE-FILE)@\
The file recording completed exercises.
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"etps3.scores"}.

@IndexFlag(USER-PASSWD-FILE)@\
The file recording user id's and passwords for a class
using ETPS over the web.
It takes values of type FILESPEC and belongs to subjects @t{EVENTS}.  The default value is @T{"user-passwd"}.
@End(Description)

@Section(Grader)

@Begin(Description)
@IndexFlag(CAL-PERCENTAGE)@\
The program calculates percentage based on total scores if the
	 value of this variable is T.
It takes values of type BOOLEAN and belongs to subjects @t{GR-MISC}.  The default value is @T{NIL}.

@IndexFlag(COURSE-NAME)@\
Name of the course. Also used as a suffix for various
	 files which are created or modified by the grading package.
It takes values of type STRING and belongs to subjects @t{GR-MISC}.  The default value is @T{"course"}.

@IndexFlag(DEFAULT-PENALTY-FN)@\
Default penalty function for late exercises. The default is no-penalty which
 doesn't take any points off.
It takes values of type FUNCTION and belongs to subjects @t{GR-MISC}.  The default value is @T{NO-PENALTY}.

@IndexFlag(DROP-MIN)@\
When calculating totals, the program drops the minimum scores
	 on each of the items in this list.
It takes values of type CONSP1 and belongs to subjects @t{GR-MISC}.  The default value is @T{NIL}.

@IndexFlag(DUE-DATE-FLAG)@\
If this flag is nil, the user is not prompted for due dates (in the command
ETPS-GRADE) and it's assumed that all exercises were submitted in time.
It takes values of type BOOLEAN and belongs to subjects @t{GR-MISC}.  The default value is @T{T}.

@IndexFlag(ETPS-FILE)@\
Name of the file which contains ETPS records.
It takes values of type FILESPEC and belongs to subjects @t{GR-FILENAMES}.  The default value is @T{""}.

@IndexFlag(GRADE-DIR)@\
Name of the directory in which the grader files are to be found,
or "" for the directory from which grader was started. This name should 
end with a backslash, as in : "/usr/teacher/course-grades/".
When this flag is changed, all of the other filenames will change with it.
Note that in old versions of CMU lisp, the "" option will not work properly.
It takes values of type STRING and belongs to subjects @t{GR-FILENAMES}.  The default value is @T{""}.

@IndexFlag(GRADE-FILE)@\
Name of the GRADE-FILE.
It takes values of type FILESPEC and belongs to subjects @t{GR-FILENAMES}.  The default value is @T{""}.

@IndexFlag(LETTER-GRADE-FILE)@\
Name of the file which will contain letter grades.
It takes values of type FILESPEC and belongs to subjects @t{GR-FILENAMES}.  The default value is @T{""}.

@IndexFlag(LETTER-GRADE-FLAG)@\
The program creates a separate file containing letter grades
	 if the value of this variable is true.
It takes values of type BOOLEAN and belongs to subjects @t{GR-MISC}.  The default value is @T{T}.

@IndexFlag(NEW-ITEM)@\
The list of new items to be calculated when calculating
	 totals. See the manual for more details.
It takes values of type CONSP1 and belongs to subjects @t{GR-MISC}.  The default value is @T{NIL}.

@IndexFlag(OLD-GRADE-FILE)@\
Name of the back-up GRADE-FILE.
It takes values of type FILESPEC and belongs to subjects @t{GR-FILENAMES}.  The default value is @T{""}.

@IndexFlag(OLD-TOTALS-GRADE-FILE)@\
Name of the back-up TOTALS-GRADE-FILE .
It takes values of type FILESPEC and belongs to subjects @t{GR-FILENAMES}.  The default value is @T{""}.

@IndexFlag(PATCH-FILE)@\
Name of the file containing changes to the grader core image.
It takes values of type FILESPEC and belongs to subjects @t{GR-FILENAMES}.  The default value is @T{"grader.patch"}.

@IndexFlag(PRINT-N-DIGITS)@\
The number of digits to be printed after the decimal.
It takes values of type INTEGER+ and belongs to subjects @t{GR-MISC}.  The default value is @T{0}.

@IndexFlag(STATISTICAL-OPTIONS)@\
List of statistical data to be calculated. Currently the
	 program can calculate mean, median , standard deviation.
	 The default is (-mean- -median- -sdev-).
It takes values of type CONSP1 and belongs to subjects @t{GR-MISC}.  The default value is @T{(-MEAN-
                                                                                              -MEDIAN-
                                                                                              -SDEV-)}.

@IndexFlag(TOTALS-GRADE-FILE)@\
Name of the file which will contain totals.
It takes values of type FILESPEC and belongs to subjects @t{GR-FILENAMES}.  The default value is @T{""}.
@End(Description)

@Section(Maintenance)

@Begin(Description)
@IndexFlag(COMPILED-EXTENSION)@\
The extension of compiled files in TPS3.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{"fasl"}.

@IndexFlag(EXPERTFLAG)@\
If T, arbitrary Lisp expression may be evaluated on top levels.
It takes values of type BOOLEAN and belongs to subjects @t{MAINTAIN}.  The default value is @T{NIL}.

@IndexFlag(GOODMODES)@\
A name for a pair MODES and GWFFS where MODES is a list of modes
and GWFFS is a list of theorems.  Every theorem in GWFFS should be 
provable using some mode in MODES.  To check this, or to use these modes
to try to prove a new theorem, one can use TEST-INIT and TPS-TEST.

SEE ALSO: MODES-GWFFS, TEST-INIT, TPS-TEST, ADD-GOODMODES, REMOVE-GOODMODES
It takes values of type MODES-GWFFS and belongs to subjects @t{MAINTAIN}.  The default value is @T{EMPTYGOODMODES}.

@IndexFlag(INIT-DIALOGUE)@\
If T, the value of INIT-DIALOGUE-FN will be called on startup
after the INI file has been read and the terminal is initialized.
It takes values of type BOOLEAN and belongs to subjects @t{MAINTAIN}.  The default value is @T{NIL}.

@IndexFlag(INIT-DIALOGUE-FN)@\
The value of this flag is a function of no arguments,
which will be called after the INI file has been read, if
the flag INIT-DIALOGUE is T.  It may be used to set the terminal type
correctly, load some libraries, if the user wishes, or even decide
between expert and non-expert modes. The default function does nothing;
the function INIT-DEFINE-MY-DEFAULT-MODE defines a mode called
MY-DEFAULT-MODE containing the state of all the system's flags at 
the point immediately after the INI file is read.
It takes values of type ANYTHING and belongs to subjects @t{MAINTAIN}.  The default value is @T{INIT-DIALOGUE-DEFAULT-FN}.

@IndexFlag(JAVA-COMM)@\
How to start the Tps java interface.

An example for Unix is
cd /home/theorem/tps/java ; java TpsStart

An example for Windows is
java -classpath C:\TPS\java\ TpsStart
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{""}.

@IndexFlag(LISP-IMPLEMENTATION-TYPE)@\
Tells what Common Lisp we are running on.  Initialized 
when TPS starts up.  Can't be changed.
It takes values of type STRING and belongs to subjects @t{SYSTEM}.  The default value is @T{""}.

@IndexFlag(LOAD-WARN-P)@\
If T, library files will be checked while building 
the library master index; also, warning messages will be 
printed when redefining TPS-objects while loading a file 
or fetching library objects.
It takes values of type BOOLEAN and belongs to subjects @t{MAINTAIN}.  The default value is @T{T}.

@IndexFlag(MACHINE-INSTANCE)@\
Tells what particular machine we are running on.  Initialized
when TPS starts up.  Can't be changed.
It takes values of type STRING and belongs to subjects @t{SYSTEM}.  The default value is @T{""}.

@IndexFlag(MACHINE-TYPE)@\
Tells what hardware that we are running on.  Initialized 
when TPS starts up.  Can't be changed.
It takes values of type STRING and belongs to subjects @t{SYSTEM}.  The default value is @T{""}.

@IndexFlag(NEWS-DIR)@\
The directory with the NEWS and NOTE files.
It takes values of type DIRSPEC and belongs to subjects @t{MAINTAIN}.  The default value is @T{""}.

@IndexFlag(READ-LLOAD-SOURCES-P)@\
If T while LLoading, one can later Ledit compiled functions.
It takes values of type BOOLEAN and belongs to subjects @t{MAINTAIN}.  The default value is @T{T}.

@IndexFlag(SAVE-FILE)@\
The name of the file in which to save the core-image for TPS3.
It takes values of type FILESPEC and belongs to subjects @t{MAINTAIN}.  The default value is @T{"tps3.exe"}.

@IndexFlag(SHORT-SITE-NAME)@\
Tells what site we are running at.  Initialized
when TPS starts up.  Can't be changed.
It takes values of type STRING and belongs to subjects @t{SYSTEM}.  The default value is @T{""}.

@IndexFlag(SOURCE-EXTENSION)@\
The extensions (:type) of source files in TPS3.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{"lisp"}.

@IndexFlag(SOURCE-PATH)@\
A list of pathnames with source files for TPS3.
It takes values of type DIRSPECLIST and belongs to subjects @t{MAINTAIN}.  The default value is @T{()}.

@IndexFlag(TEST-MODIFY)@\
A string which will be evaluated in exactly the same way as an alias.
May contain any valid lisp commands, and will be evaluated after setting the 
mode during tps-test. So, for example, setting it to 
"(set-flag 'skolem-default nil) 
(when search-time-limit (setq search-time-limit (* 2 search-time-limit)))
(when max-search-limit (setq max-search-limit (* 2 max-search-limit)))"
would make tps-test changed SKOLEM-DEFAULT to NIL and double the time limits
before each search.
It takes values of type STRING and belongs to subjects @t{MAINTAIN}.  The default value is @T{""}.

@IndexFlag(TEST-THEOREMS)@\
A list of pairs; the first of each pair is the name of a theorem; 
the second is the name of a mode. If the mode name is NIL, TPS will 
attempt to choose a mode from the list of best modes in the library.
This flag is used by the command TPS-TEST, and can be set automatically by
the command TEST-INIT.

The default setting is a sample list of two standard TPS exercises, both 
to be run in mode ML (also standard in TPS). If you set this flag yourself,
beware of unexported symbols --- which is to say, make sure that the 
symbols you use are all in the USER package (this is particularly 
necessary if you are using library theorems which are not yet loaded
into TPS, or they may end up interned in the wrong package). If in doubt,
put "USER::" before all symbols, thus:

(setq test-theorems '((cl-user::thm30 . cl-user::mode-thm30) (cl-user::x2112 . cl-user::ml)))

You can use the flag TEST-MODIFY to alter modes on the fly as TPS-TEST runs.
See the help messages for TEST-INIT and TEST-MODIFY for more information.
It takes values of type SYMBOLPAIRLIST and belongs to subjects @t{MAINTAIN}.  The default value is @T{((X2106   ML) (X2108   ML))}.
@End(Description)

@Section(Rules object)

@Begin(Description)
@IndexFlag(BUILD-MATCH)@\
If T, <rule>-MATCH functions for use with SUGGEST will be built.
It takes values of type BOOLEAN and belongs to subjects @t{RULES-PACK}.  The default value is @T{T}.

@IndexFlag(HLINE-JUSTIFICATION)@\
The justification for hlines, if TREAT-HLINES-AS-DLINES is NIL.
It takes values of type STRING and belongs to subjects @t{RULES-OBJECT}.  The default value is @T{"Hyp"}.

@IndexFlag(TREAT-HLINES-AS-DLINES)@\
If T, hlines may have multiple hypotheses and a justification,
if NIL, hlines can only have one hypothesis (itself) and `Hyps' as
justification.
It takes values of type BOOLEAN and belongs to subjects @t{RULES-OBJECT}.  The default value is @T{T}.
@End(Description)

@Section(Unclassified)

@Begin(Description)
@IndexFlag(MAX-SUBSTS-PROJ)@\
The total number of projection substitutions 
allowed for any given variable. See also MAX-SUBSTS-VAR
and MAX-SUBSTS-PROJ-TOTAL.
This applies to higher-order unification (UN88 or UN90) only.
It takes values of type NULL-OR-INTEGER and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(MAX-SUBSTS-PROJ-TOTAL)@\
The total number of projection substitutions 
allowed for any given dpairset. See also MAX-SUBSTS-VAR
and MAX-SUBSTS-PROJ.
This applies to higher-order unification (UN88 or UN90) only.
It takes values of type NULL-OR-INTEGER and belongs to subjects @t{TRANSMIT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(MAX-SUBSTS-QUICK)@\
When NIL, quick unification is governed by the MIN-QUICK-DEPTH
flag, and only minimal amounts of MAX-SUBSTS checking are done during 
quick unification.
When MIN-SUBSTS-QUICK is a positive integer, quick unification 
(i.e. partial unification of a possible connection) is considered as a 
special case of normal unification, with MAX-SUBSTS-VAR temporarily 
equal to the value of MAX-SUBSTS-QUICK.
When MIN-SUBSTS-QUICK is 0, quick unification goes down as far as it can 
until it is forced to either branch or violate MAX-SUBSTS-VAR. (This is 
almost equivalent to MAX-SUBSTS-QUICK NIL and MIN-QUICK-DEPTH 1.) 

Note: non-NIL values of MAX-SUBSTS-QUICK only take effect if MAX-SUBSTS-VAR
is also non-NIL. In this case, other flags will also be affected, as follows:
APPLY-MATCH will be ignored (the matching routine that is used will be a 
variant of APPLY-MATCH-ALL-FRDPAIRS)
COUNTSUBS-FIRST and STOP-AT-TSN will be T.
SUBSUMPTION-CHECK, UNIF-COUNTER and UNIF-TRIGGER will be NIL.
UNI-SEARCH-HEURISTIC will be BREADTH-FIRST.
MIN-QUICK-DEPTH and MAX-UTREE-DEPTH will be ignored.
It takes values of type NULL-OR-INTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{IMPORTANT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(MAX-SUBSTS-VAR)@\
The maximum number of substitutions allowed for any given
free variable in a dpairset. This is cumulative (i.e. if an old 
variable f is replaced by h1, which is in turn replaced by h2,
that counts as two substitutions for f). Only projections or
imitations are counted; eliminating substitutions are not.
See also MAX-SUBSTS-PROJ and MAX-SUBSTS-PROJ-TOTAL.
This applies to higher-order unification (UN88 or UN90) only.
It takes values of type NULL-OR-INTEGER and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{IMPORTANT}, @t{UNIFICATION}.  The default value is @T{NIL}.

@IndexFlag(NUM-OF-DUPS)@\
Max number of duplications allowed on any path in 
    search procedures using path-focused duplication.
    This flag may be set to 0.
It takes values of type INTEGER+ and belongs to subjects @t{TRANSMIT}, @t{MS98-1}, @t{MS93-1}, @t{MS92-9}, @t{MS91-7}, @t{MS90-9}, @t{MS90-3}, @t{MATING-SEARCH}, @t{IMPORTANT}.  The default value is @T{2}.

@IndexFlag(PRIMSUB-VAR-SELECT)@\
If T, primsubs will only be applied to those variables
which occur both negatively and positively as the head variable
of some leaves in the current eproof.
If NIL, primsubs will be applied to any variable which occurs
either negatively or positively or both, anywhere
It takes values of type BOOLEAN and belongs to subjects @t{TRANSMIT}, @t{PRIMSUBS}.  The default value is @T{T}.
@End(Description)

@Section(Library)

@Begin(Description)
@IndexFlag(ADD-SUBDIRECTORIES)@\
When restoring the library index, search the directories in 
DEFAULT-LIB-DIR and BACKUP-LIB-DIR for subdirectories which also contain 
library files, and add these to the flags. This flag only works for Allegro,
CMU, Kyoto and Lucid Common Lisps.
It takes values of type BOOLEAN and belongs to subjects @t{LIBRARY}.  The default value is @T{T}.

@IndexFlag(BACKUP-LIB-DIR)@\
The list of all backup directories of library files.
These should be directories to which the user has read access.
No attempt will be made to write to a directory on this list.
See also DEFAULT-LIB-DIR and SHOW-ALL-LIBOBJECTS.
It takes values of type DIRSPECLIST and belongs to subjects @t{LIBRARY}.  The default value is @T{()}.

@IndexFlag(DEFAULT-LIB-DIR)@\
The list of writeable directories containing 
library files. All of the directories in this list ought to be 
library directories to which the user has write access.
See also BACKUP-LIB-DIR and SHOW-ALL-LIBOBJECTS.
It takes values of type DIRSPECLIST and belongs to subjects @t{LIBRARY}.  The default value is @T{()}.

@IndexFlag(DEFAULT-LIBFILE-TYPE)@\
The default value for the extension of library files.
It takes values of type STRING and belongs to subjects @t{LIBRARY}.  The default value is @T{"lib"}.

@IndexFlag(DEFAULT-LIBINDEX-TYPE)@\
The default value for the extension of library index files.
It takes values of type STRING and belongs to subjects @t{LIBRARY}.  The default value is @T{"rec"}.

@IndexFlag(LIB-BESTMODE-FILE)@\
Name of the file containing best modes for the theorems in the library.
It takes values of type FILESPEC and belongs to subjects @t{LIBRARY}.  The default value is @T{"bestmodes.rec"}.

@IndexFlag(LIB-KEYWORD-FILE)@\
Name of the file containing acceptable keywords for the library.
It takes values of type FILESPEC and belongs to subjects @t{LIBRARY}.  The default value is @T{"keywords.rec"}.

@IndexFlag(LIB-MASTERINDEX-FILE)@\
Name of the file containing index of entries in the library.
It takes values of type FILESPEC and belongs to subjects @t{LIBRARY}.  The default value is @T{"libindex.rec"}.

@IndexFlag(RECORDFLAGS)@\
List of flags to be saved when using the mateop DATEREC.
It takes values of type TPSFLAGLIST and belongs to subjects @t{MATING-SEARCH}, @t{LIBRARY}.  The default value is @T{()}.

@IndexFlag(REMOVE-TRAILING-DIR)@\
If T, the parts of the directory specification that are the same 
for all library files will be removed before printing. If NIL, the full
directory will be printed.
It takes values of type BOOLEAN and belongs to subjects @t{LIBRARY}.  The default value is @T{T}.

@IndexFlag(SHOW-ALL-LIBOBJECTS)@\
When loading an object, if there are multiple objects of that name 
and type, when NIL then accept the first object found (searching 
DEFAULT-LIB-DIR and then BACKUP-LIB-DIR in order). When T, show a list
of all the objects and ask the user to choose.
It takes values of type BOOLEAN and belongs to subjects @t{LIBRARY}.  The default value is @T{T}.
@End(Description)

@Section(Editing)

@Begin(Description)
@IndexFlag(AUTO-KEYWORDS)@\
If T, keywords will automatically be generated and attached to the library object.
However, setting auto-keywords to T requires expanding all definitions, 
which can take an enormous amount of time when definitions are deeply nested.
It takes values of type BOOLEAN and belongs to subjects @t{LIBRARY}.  The default value is @T{NIL}.

@IndexFlag(AUTO-LIB-DIR)@\
A writeable directory containing 
library files, used for automatic library insertion.
See the LIBRARY command INSERT-TPTP and INSERT-TPTP*
It takes values of type DIRSPEC and belongs to subjects @t{LIBRARY}.  The default value is @T{""}.
@End(Description)

@Section(Library Classification)

@Begin(Description)
@IndexFlag(CLASS-DIRECTION)@\
Suppose A is a class with child class B.
If the value of CLASS-DIRECTION is Up, we think of
B as depending on A (eg, A could be GROUPS and B could be FIELDS).
If the value of CLASS-DIRECTION is Down, we think of
A as depending on B (eg, B could be GROUPS and A could be FIELDS).

The value of this flag affects the behavior of CLASSIFY-ITEM
and FETCH-CLASS*.

See Also: CLASSIFY-ITEM, FETCH-CLASS*, FETCH-UP, FETCH-DOWN
It takes values of type UPDOWN and belongs to subjects @t{LIBRARY}.  The default value is @T{Down}.

@IndexFlag(CLASS-SCHEME)@\
The classification scheme used to organize the library interface.
A classification scheme is a way of organizing library items into a tree 
(actually a directed acyclic graph) of classes.  Each class can have 
classes as children.  Each class has associated libitems.

See Also: CREATE-CLASS-SCHEME, PSCHEMES, PCLASS-SCHEME-TREE, 
PCLASS-TREE, CREATE-LIBCLASS, CLASSIFY-CLASS, CLASSIFY-ITEM, 
FETCH-LIBCLASS, FETCH-LIBCLASS*, FETCH-UP, FETCH-DOWN,
GOTO-CLASS, ROOT-CLASS
It takes values of type SYMBOL and belongs to subjects @t{LIBRARY}.  The default value is @T{LIBDIR}.
@End(Description)

@Section(Bugs)

@Begin(Description)
@IndexFlag(DEFAULT-BUG-DIR)@\
If USE-DEFAULT-BUG-DIR is T, this is the default value
for the directory where bugs generated by BUG-SAVE will be stored,
and the first directory that will be searched by BUG-RESTORE.
If USE-DEFAULT-BUG-DIR is NIL, this flag is ignored, and bugs
will be saved like normal library objects, in the directories
listed in DEFAULT-LIB-DIR.
It takes values of type DIRSPEC and belongs to subjects @t{LIBRARY}.  The default value is @T{""}.

@IndexFlag(USE-DEFAULT-BUG-DIR)@\
Determines whether or not to use the directory given
by DEFAULT-BUG-DIR for saving. If T, bugs are saved to and 
restored from DEFAULT-BUG-DIR, otherwise they aren't.
See DEFAULT-BUG-DIR.
It takes values of type BOOLEAN and belongs to subjects @t{LIBRARY}.  The default value is @T{T}.
@End(Description)
@ChapterPh(Modes)
The internal name of this category is 
FLAG-MODE.
A mode can be defined using DEFMODE.
Allowable properties are: @t{FLAG-SETTINGS}, @t{MHELP}.

@Section(Collecting Help)

@Begin(Description)
@IndexOther(SCRIBE-DOC)@\
Mode used for producing documentation in Scribe. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
ALLSCOPEFLAG @\NIL

ATOMVALFLAG @\NIL

DISPLAYWFF @\NIL

FIRST-ORDER-PRINT-MODE @\NIL

FLUSHLEFTFLAG @\NIL

LEFTMARGIN @\0

LOCALLEFTFLAG @\NIL

PPWFFLAG @\NIL

PRINTDEPTH @\0

PRINTTYPES @\T

RIGHTMARGIN @\70

SCOPE @\NIL

STYLE @\SCRIBE

@End(Description)

@IndexOther(SCRIBE-DOC-FIRST-ORDER)@\
Mode used for producing documentation in Scribe in first-order mode. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
ALLSCOPEFLAG @\NIL

ATOMVALFLAG @\NIL

DISPLAYWFF @\NIL

FIRST-ORDER-PRINT-MODE @\T

FLUSHLEFTFLAG @\NIL

LEFTMARGIN @\0

LOCALLEFTFLAG @\NIL

PPWFFLAG @\NIL

PRINTDEPTH @\0

PRINTTYPES @\NIL

RIGHTMARGIN @\70

SCOPE @\NIL

STYLE @\SCRIBE

@End(Description)
@End(Description)

@Section(OTL Object)

@Begin(Description)
@IndexOther(RULES)@\
Set flags so that the rules package can be run successfully. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
FIRST-ORDER-MODE-PARSE @\NIL

MAKE-WFFOPS-LABELS @\T

@End(Description)

@IndexOther(SCRIBE-OTL)@\
Mode used for printing proofs in Scribe. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
ALLSCOPEFLAG @\NIL

ATOMVALFLAG @\NIL

DISPLAYWFF @\NIL

FLUSHLEFTFLAG @\NIL

LEFTMARGIN @\0

LOCALLEFTFLAG @\NIL

PPWFFLAG @\T

PRINTDEPTH @\0

RIGHTMARGIN @\70

SCOPE @\NIL

STYLE @\SCRIBE

@End(Description)

@IndexOther(TEX-1-OTL)@\
mode used for printing proofs in tex. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
ALLSCOPEFLAG @\NIL

ATOMVALFLAG @\NIL

DISPLAYWFF @\NIL

FLUSHLEFTFLAG @\NIL

LEFTMARGIN @\0

LOCALLEFTFLAG @\NIL

PPWFFLAG @\T

PRINTDEPTH @\0

RIGHTMARGIN @\85

SCOPE @\NIL

STYLE @\TEX-1

@End(Description)

@IndexOther(TEX-OTL)@\
mode used for printing proofs in tex. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
ALLSCOPEFLAG @\NIL

ATOMVALFLAG @\NIL

DISPLAYWFF @\NIL

FLUSHLEFTFLAG @\NIL

LEFTMARGIN @\0

LOCALLEFTFLAG @\NIL

PPWFFLAG @\T

PRINTDEPTH @\0

RIGHTMARGIN @\70

SCOPE @\NIL

STYLE @\TEX

@End(Description)
@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(RE-READ)@\
Used when writing out wffs to a file
in such a way that they may be read back in and parsed correctly
in higher-order mode. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
PRINT-META @\NIL

ATOMVALFLAG @\NIL

DISPLAYWFF @\NIL

FIRST-ORDER-PRINT-MODE @\NIL

LEFTMARGIN @\1

PPWFFLAG @\T

PRINTDEPTH @\0

PRINTTYPES @\T

PRINTTYPES-ALL @\T

RIGHTMARGIN @\78

SCOPE @\NIL

STYLE @\GENERIC-STRING

@End(Description)
@End(Description)

@Section(Recording)

@Begin(Description)
@IndexOther(SCRIBE-EDWFF)@\
Mode used for writing formulas from the editor. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
ALLSCOPEFLAG @\NIL

ATOMVALFLAG @\NIL

DISPLAYWFF @\T

FIRST-ORDER-PRINT-MODE @\NIL

FLUSHLEFTFLAG @\NIL

LEFTMARGIN @\0

LOCALLEFTFLAG @\NIL

PPWFFLAG @\T

PRINTDEPTH @\0

PRINTTYPES @\T

RIGHTMARGIN @\70

SCOPE @\NIL

STYLE @\SCRIBE

@End(Description)

@IndexOther(SCRIBE-MATEWFF)@\
Mode used for writing formulas from mating search. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
ALLSCOPEFLAG @\NIL

ATOMVALFLAG @\NIL

DISPLAYWFF @\T

FIRST-ORDER-PRINT-MODE @\NIL

FLUSHLEFTFLAG @\NIL

LEFTMARGIN @\0

LOCALLEFTFLAG @\NIL

PPWFFLAG @\T

PRINTDEPTH @\0

PRINTTYPES @\T

RIGHTMARGIN @\70

SCOPE @\NIL

STYLE @\SCRIBE

@End(Description)
@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexOther(NAIVE)@\
Sets flags so all definitions and equalities will be rewritten,
skolemizing will be done using SK1, but equalities will be rewritten
using the Leibniz definition. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
SKOLEM-DEFAULT @\SK1

REWRITE-DEFNS @\'(LAZY1)

REWRITE-EQUALITIES @\'LEIBNIZ

REMOVE-LEIBNIZ @\T

MIN-QUANTIFIER-SCOPE @\NIL

USE-RULEP @\NIL

USE-SYMSIMP @\NIL

@End(Description)
@End(Description)

@Section(MS91-6 and MS91-7 search procedures)

@Begin(Description)
@IndexOther(MS91-DEEP)@\
Generates one new option set at a time and accepts it, 
irrespective of its weight. Does not generate option sets with ordinary 
duplications (i.e. duplications not used by a primsub), nor sets with 
multiple primsubs for the same variable; will instead generate recursive 
substitutions (i.e. will substitute for the expansion variables introduced
by the first lot of substitutions). Does not set the PRIMSUBS flags. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
MS91-WEIGHT-LIMIT-RANGE @\INFINITY

NEW-OPTION-SET-LIMIT @\1

WEIGHT-A-COEFFICIENT @\0

WEIGHT-B-COEFFICIENT @\1

WEIGHT-C-COEFFICIENT @\0

WEIGHT-B-FN @\ALL-PENALTIES-FN

RECONSIDER-FN @\INF-WEIGHT

PENALTY-FOR-EACH-PRIMSUB @\3

PENALTY-FOR-MULTIPLE-PRIMSUBS @\5

PENALTY-FOR-MULTIPLE-SUBS @\INFINITY

PENALTY-FOR-ORDINARY-DUP @\INFINITY

OPTIONS-GENERATE-FN @\ADD-OPTIONS-ORIGINAL

OPTIONS-GENERATE-ARG @\75

OPTIONS-GENERATE-UPDATE @\IDENT-ARG

@End(Description)

@IndexOther(MS91-NODUPS)@\
Generates one new option set at a time and accepts it, irrespective
of its weight. Does not generate option sets with ordinary duplications (i.e.
duplications not used by a primsub). Does not set the PRIMSUBS flags. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
MS91-WEIGHT-LIMIT-RANGE @\INFINITY

NEW-OPTION-SET-LIMIT @\1

WEIGHT-A-COEFFICIENT @\0

WEIGHT-B-COEFFICIENT @\1

WEIGHT-C-COEFFICIENT @\0

WEIGHT-B-FN @\ALL-PENALTIES-FN

RECONSIDER-FN @\INF-WEIGHT

PENALTY-FOR-EACH-PRIMSUB @\3

PENALTY-FOR-MULTIPLE-PRIMSUBS @\5

PENALTY-FOR-MULTIPLE-SUBS @\5

PENALTY-FOR-ORDINARY-DUP @\INFINITY

OPTIONS-GENERATE-FN @\ADD-OPTIONS-ORIGINAL

OPTIONS-GENERATE-ARG @\75

OPTIONS-GENERATE-UPDATE @\IDENT-ARG

@End(Description)

@IndexOther(MS91-ORIGINAL)@\
The original flag settings. Does not set the PRIMSUBS flags. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
MS91-WEIGHT-LIMIT-RANGE @\3

NEW-OPTION-SET-LIMIT @\5

WEIGHT-A-COEFFICIENT @\1

WEIGHT-B-COEFFICIENT @\1

WEIGHT-C-COEFFICIENT @\1

WEIGHT-A-FN @\EXPANSION-LEVEL-WEIGHT-A

WEIGHT-B-FN @\SIMPLE-WEIGHT-B-FN

WEIGHT-C-FN @\OPTION-SET-NUM-LEAVES

RECONSIDER-FN @\INF-WEIGHT

PENALTY-FOR-EACH-PRIMSUB @\3

PENALTY-FOR-MULTIPLE-PRIMSUBS @\5

PENALTY-FOR-MULTIPLE-SUBS @\5

OPTIONS-GENERATE-FN @\ADD-OPTIONS-ORIGINAL

OPTIONS-GENERATE-ARG @\75

OPTIONS-GENERATE-UPDATE @\IDENT-ARG

@End(Description)

@IndexOther(MS91-SIMPLEST)@\
Generates option sets in the simplest possible order, in 
batches of five. Does not set the PRIMSUBS flags. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
MS91-WEIGHT-LIMIT-RANGE @\1

WEIGHT-A-COEFFICIENT @\0

WEIGHT-B-COEFFICIENT @\1

WEIGHT-C-COEFFICIENT @\0

WEIGHT-B-FN @\SIMPLEST-WEIGHT-B-FN

RECONSIDER-FN @\INF-WEIGHT

NEW-OPTION-SET-LIMIT @\5

OPTIONS-GENERATE-FN @\ADD-OPTIONS-ORIGINAL

OPTIONS-GENERATE-ARG @\75

OPTIONS-GENERATE-UPDATE @\IDENT-ARG

@End(Description)
@End(Description)

@Section(wff Primitives)

@Begin(Description)
@IndexOther(FIRST-ORDER)@\
Puts parser and printer into first-order mode. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
FIRST-ORDER-MODE-PARSE @\T

TYPE-IOTA-MODE @\T

FIRST-ORDER-PRINT-MODE @\T

PRINTTYPES @\NIL

@End(Description)

@IndexOther(HIGHER-ORDER)@\
Puts parser and printer into higher-order mode. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
FIRST-ORDER-MODE-PARSE @\NIL

FIRST-ORDER-PRINT-MODE @\NIL

PRINTTYPES @\T

@End(Description)
@End(Description)

@Section(Maintenance)

@Begin(Description)
@IndexOther(QUIET)@\
Turn off all output that can be turned off, without affecting search
at all. Should make most other modes run a bit faster. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
PRINTLINEFLAG @\NIL

UNIFY-VERBOSE @\SILENT

MATING-VERBOSE @\SILENT

ETREE-NAT-VERBOSE @\NIL

MS98-VERBOSE @\NIL

TACTIC-VERBOSE @\MIN

OPTIONS-VERBOSE @\NIL

LOAD-WARN-P @\NIL

@End(Description)
@End(Description)

@Section(Unclassified)

@Begin(Description)
@IndexOther(MATH-LOGIC-2-MODE)@\
Mode to be used for Math Logic II. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
FIRST-ORDER-MODE-PARSE @\NIL

TYPE-IOTA-MODE @\T

FIRST-ORDER-PRINT-MODE @\NIL

PRINTTYPES @\T

TREAT-HLINES-AS-DLINES @\T

DEFAULT-WFFEQ @\WFFEQ-AB

@End(Description)

@IndexOther(ML)@\
Puts parser and printer into higher-order mode for
Lisp package ML. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
FIRST-ORDER-MODE-PARSE @\NIL

FIRST-ORDER-PRINT-MODE @\NIL

TYPE-IOTA-MODE @\T

BASE-TYPE @\I

PRINTTYPES @\T

@End(Description)

@IndexOther(MSV-OFF)@\
Turn off all of the MAX-SUBSTS-* routines. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
MAX-SUBSTS-VAR @\NIL

MAX-SUBSTS-PROJ-TOTAL @\NIL

MAX-SUBSTS-PROJ @\NIL

MAX-SUBSTS-QUICK @\NIL

APPLY-MATCH @\'APPLY-MATCH-ALL-FRDPAIRS

@End(Description)

@IndexOther(MSV-ON)@\
Turn on the MAX-SUBSTS-* routines and increase
the unification depths to infinity. The settings of the flags are:
@Begin(Description, Spread 0, leftmargin +19, indent -19)
MAX-SUBSTS-VAR @\5

MAX-SUBSTS-PROJ-TOTAL @\NIL

MAX-SUBSTS-PROJ @\NIL

MAX-SUBSTS-QUICK @\5

APPLY-MATCH @\'APPLY-MATCH-ALL-FRDPAIRS

MAX-UTREE-DEPTH @\NIL

MAX-SEARCH-DEPTH @\NIL

MIN-QUICK-DEPTH @\NIL

@End(Description)
@End(Description)
@ChapterPh(Flag Setting Or Other Piece Of Informations)
The internal name of this category is 
INFO.
A flag setting or other piece of information can be defined using DEFINFO.
Allowable properties are: @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(COMMAND-LINE-SWITCHES)@\
Several switches can be given on the command line when
TPS is started up. They are as follows:

-grader starts TPS in the GRADER top level.
-batch <file1> will execute the work file <filename>.work and 
               then quit TPS.
-service <name> <in> <out> will start a TPS with identifier <name>
               looking for requests from <in> and sending output to <out>.
               This gives a general way for external programs to ask
               TPS to prove a thm and receive the proof.
-lservice <portnum>
               Similar to -service, but assumes there is a listener
               on the machine at port <portnum>.  TPS connects to this
               and uses the socket to take requests and send output.
-server <tps-image-file> <etps-image-file> [-logdir <directory for log files>] [-port <portnum>]
               This starts TPS or ETPS as a web server.  Browsers
               can connect via http://<machine-name>:<portnum>
               where the default <portnum> is 29090 (but another can
               be explicitly given).  Once a browser connects to this
               TPS server, the client can start a new TPS or ETPS image
               (assuming the client has access rights, see SETUP-ONLINE-ACCESS).
               The server can also send html files to the client.
-remoteuser <userid> <portnum>
               This starts TPS or ETPS for a remote user.  This option
               is used when TPS or ETPS is started by a running TPS server.
               It should rarely (or never) be used when TPS is started directly.
               <portnum> is the port number of a passive socket waiting for a 
               connection.  Once TPS or ETPS starts for a remoteuser, it connects
               to this socket and sends it the port number of a new passive
               socket that the client can use to connect to this TPS or ETPS.
-javainterface <java command> [-other <java args>]
               This command line switch tells TPS to start a java
               interface from which it will receive input and to which
               it will send output.  The arguments after -javainterface
               and (possibly) before a -other switch indicate how to start
               the java interface.  For example, java TpsWin.
               This will be appended to the name of the machine and a port number
               (determined at runtime).
               If there is a -other switch, then the arguments after this will be 
               appended after the port number.
-omega will prevent -batch from quitting TPS
-outfile <file2.prf>, in the presence of -omega and -batch, runs the 
                  work file <filename1>.work and then remains in
                  TPS. When the user exits, <file2.prf> will be 
                  written, containing the current version of the 
                  dproof created by the work file. A file <file2.prt>
                  will also be written. Note that the given filename
                  filename MUST end with .prf
-outfile <file2>, in the presence of -batch alone, sends a script
                  of the entire session to <file2>.
-problem -mode -slist belong together; they will execute the given problem
                  using the given mode and searchlist.
              
Examples:

tps3 -- -batch thm266 
  runs thm266.work through tps3, showing the output on the terminal.
tps3 -- -batch thm266 -outfile thm266.script
  does the same but directs the output to thm266.script.
tps3 -- -omega -batch thm266 -outfile thm266.prf
  starts TPS, runs thm266.work and then enters the TPS command-line 
  interface. When the user exits, it writes the current proof into
  the file thm266.prf
tps3 -- -batch thm266 -outfile /dev/null
  does the same but discards the output.

Notice that the "--" is required for allegro lisp, but not for cmucl,
where the equivalent commands are of the form:   tps3cmu -batch thm266
@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(PRFW-PALL)@\
An option for ETREE-NAT-VERBOSE.
After each tactic during ETREE-NAT, in the proofwindow "Complete Proof",
print the current proof.

@IndexOther(PRFW-^P)@\
An option for ETREE-NAT-VERBOSE.
After each tactic during ETREE-NAT, in the proofwindow "Current Subproof",
print the current plan-support pair in the proof.

@IndexOther(PRFW-^PN)@\
An option for ETREE-NAT-VERBOSE.
After each tactic during ETREE-NAT, in the proofwindow "Current Subproof 
and Line Numbers", print the current plan-support pair in the proof, and
also print just the line numbers of the other lines in the proof.@End(Description)

@Section(Proof Outline)

@Begin(Description)
@IndexOther(COMPRESS)@\
A flag setting for TURNSTILE-INDENT-AUTO.
Similar to VARY, but also removes other spaces in the proof (e.g. around
dots, and between line numbers and hypotheses).

@IndexOther(FIX)@\
A flag setting for TURNSTILE-INDENT-AUTO.
When printing a proof, fixes the turnstiles in the column given by 
TURNSTILE-INDENT (so they'll all line up with one another).
Lines with large numbers of hypotheses will push the turnstile onto the
following line.

@IndexOther(MIN)@\
A flag setting for TURNSTILE-INDENT-AUTO.
When printing a proof, fixes the turnstiles as far to the left as possible
while still putting it in the same column on every line.
Lines with large numbers of hypotheses will push the column of turnstiles
far to the right of the page; if it moves too far to the right, then 
this flag will be treated as though it were set to FIX instead.

MIN is also a setting for a good many other flags, where it is mostly
self-explanatory.

@IndexOther(VARY)@\
A flag setting for TURNSTILE-INDENT-AUTO.
Print the turnstile one space after the hypotheses in each line, so 
the turnstiles will not all line up in one column in the final proof.@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexOther(DUAL)@\
A flag setting for REWRITE-DEFNS.
When constructing an etree, rewrite all definitions (or a specified
list of definitions), one step at a time, once there are no more
EAGER rewrites to do. Furthermore, rewrite each definition to a
conjunction (or disjunction) of a leaf containing that definition
and an etree containing a rewrite of the definition.

See Selectively Instantiating Definitions, CADE-15.

A flag setting for REWRITE-EQUALITIES.
As above for definitions, but with equalities.

@IndexOther(DUP-ALL)@\
A setting for the flag DUPLICATION-STRATEGY.
When duplication of quantifiers is needed (in non-path-focused search),
duplicate all the quantifiers.

@IndexOther(DUP-INNER)@\
A setting for the flag DUPLICATION-STRATEGY-PFD.
When duplication of quantifiers is needed (in path-focused search),
duplicate the innermost quantifier first.

@IndexOther(DUP-OUTER)@\
A setting for the flags DUPLICATION-STRATEGY-PFD 
and DUPLICATION-STRATEGY.
When duplication of quantifiers is needed in path-focused search,
duplicate the outermost quantifier first. In other searches,
duplicate the outermost quantifiers only.

@IndexOther(EAGER)@\
A flag setting for REWRITE-DEFNS.
When constructing an etree, rewrite all definitions (or a specified
list of definitions), in one big step, as soon as possible.

@IndexOther(LAZY1)@\
A flag setting for REWRITE-DEFNS.
When constructing an etree, rewrite all definitions (or a specified
list of definitions), one step at a time, once there are no more
EAGER rewrites to do.

@IndexOther(LAZY2)@\
A flag setting for REWRITE-DEFNS.
When constructing an etree, rewrite all definitions (or a specified
list of definitions), one step at a time, once there are no more
EAGER rewrites to do. Furthermore, rewrite each definition to a
conjunction (or disjunction) of a leaf containing that definition
and an etree containing a rewrite of the definition.

See Selectively Instantiating Definitions, CADE-15.

A flag setting for REWRITE-EQUALITIES.
As above for definitions, but with equalities.

@IndexOther(NIL)@\
A setting for the flag SKOLEM-DEFAULT.
Instead of skolemizing a wff, use selection nodes and constrain the
unification tree, as explained in Miller's thesis.

@IndexOther(NONE)@\
A flag setting for REWRITE-DEFNS.
When constructing an etree, do not rewrite the specified definitions.

A flag setting for REWRITE-EQUALITIES.
When constructing an etree, do not rewrite equalities.

A flag setting for DEFAULT-EXPAND.
Do not use option trees or option sets.

@IndexOther(SK1)@\
A setting for the flag SKOLEM-DEFAULT.
SK1 is the original method due to Skolem, where wffs of the form
EXISTS y . M are replaced by M(g(...)), and the Skolem constants
g take as arguments all the x such that FORALL x occurs in the wff 
and EXISTS y . M is in its scope.

@IndexOther(SK3)@\
A setting for the flag SKOLEM-DEFAULT.
SK3 is a variant of the original method due to Skolem, where wffs 
of the form EXISTS y . M are replaced by M(g(...)), and the Skolem 
constants g take as arguments all the free variables
of EXISTS y . M.  When SK3 is used to find an expansion proof,
the translation to a natural deduction proof may fail, since
the appropriately general rules of inference are not implemented
in TPS at present.@End(Description)

@Section(Mtree Operations)

@Begin(Description)
@IndexOther(D-HIGHEST)@\
A setting for DEFAULT-OB.
The default next obligation in mtree is the highest element of the 
set of smallest obligations (i.e. given the set of all obligations with
the fewest possible literals, the first element of this set to be found
by breadth-first search).

@IndexOther(D-SMALLEST)@\
A setting for DEFAULT-OB.
The default next obligation in mtree is the deepest element of the 
set of smallest obligations (i.e. given the set of all obligations with
the fewest possible literals, the first element of this set to be found
by depth-first search).

@IndexOther(DEEPEST)@\
A setting for DEFAULT-OB.
The default next obligation in mtree is found by depth-first search
of the obligation tree.

@IndexOther(HI-LO)@\
A setting for DEFAULT-OB-MATE.
When applying ADD-CONN to an mtree, choose the default obligation by
finding the obligation which occurs lowest; this obligation was 
first added at some point in the matingstree. Then chooses the highest 
obligation which was added at the same point in the matingstree.

@IndexOther(HIGHEST)@\
A setting for DEFAULT-OB.
The default next obligation in mtree is found by breadth-first search
of the obligation tree.

A setting for MT-DEFAULT-OB-MATE
When applying ADD-CONN to an mtree, choose the default obligation by
choosing the obligation which lies highest (i.e. nearest to the
root, but not the root itself).

@IndexOther(LOWEST)@\
A setting for DEFAULT-OB-MATE.
When applying ADD-CONN to an mtree, choose the default obligation by
choosing the obligation which lies lowest (i.e. furthest from the
root).@End(Description)

@Section(Mtree Auto)

@Begin(Description)
@IndexOther(MULTIPLY-TAG-LIST)@\
A setting for TAG-MATING-FN.
Given a list of tags for connections, multiply them together to get
a tag for the mating.

@IndexOther(SAME-CONNS)@\
A setting for MT-SUBSUMPTION-CHECK.
Will check whether the mtree node about to be added is
duplicated elsewhere in the tree, and will reject it if it is. (This will use
the SAME-TAG function, and then do a more thorough check if the 
tags match.)

@IndexOther(SAME-TAG)@\
A setting for MT-SUBSUMPTION-CHECK.
Will check whether the tag of the mtree node about to be added (an integer 
generated from the list of connections) is the same as any other existing tag, 
and will reject it if it is. See TAG-CONN-FN and TAG-LIST-FN. (Note that most 
tag functions can produce the same tag for different matings, so this may 
reject connections unnecessarily.)

@IndexOther(SUBSET-CONNS)@\
A setting for MT-SUBSUMPTION-CHECK.
Will check whether the connections at the mtree node about to be
added are a subset of those at some other node. (This is only really useful in
MT94-11, where all possible new nodes are added, breadth-first, to the tree.
It is probably too restrictive for the other mtree searches.)

@IndexOther(TAG-CONN-LEAFNO)@\
A setting for TAG-CONN-FN.
Given a connection, return the product of the integer parts of the two leaf names
in the given connection.

@IndexOther(TAG-CONN-QUICK)@\
A setting for TAG-CONN-FN.
Given a connection, return TPS's internal number for the connection. (Actually,
it uses (1 + this number), so as to avoid multiplying by one.)@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexOther(MS98-1)@\
A setting for DEFAULT-MATE and DEFAULT-EXPAND.
Use the MS98-1 procedure.

@IndexOther(MTREE)@\
A setting for DEFAULT-MATE.
Use the matingstree procedure MT94-11.

@IndexOther(MTREE-1)@\
A setting for DEFAULT-MATE.
Use the matingstree procedure MT94-12.

@IndexOther(MTREE-2)@\
A setting for DEFAULT-MATE.
Use the matingstree procedure MT95-1.

@IndexOther(NPFD)@\
A setting for DEFAULT-MATE.
Use a non-path-focused procedure (MS88, MS89 or MS91-6).

@IndexOther(NPFD-1)@\
A setting for DEFAULT-MATE.
Use a non-path-focused version of a path-focused procedure 
(MS92-9 or MS93-1)

@IndexOther(OSET)@\
A setting for DEFAULT-EXPAND.
Use a mating search that has option sets. (MS91-6 or MS91-7)

@IndexOther(OTREE)@\
A setting for DEFAULT-EXPAND.
Use a mating search that has option trees. (MS89, MS93-1 or MS90-9)

@IndexOther(PFD)@\
A setting for DEFAULT-MATE.
Use a path-focused procedure (MS90-3, MS90-9 or MS91-7)

@IndexOther(QUERY-JFORMS)@\
A flag setting for QUERY-USER.
The mating search process will stop after printing each 
vpform and ask whether to search on this vpform or to generate another.
(Note: in MS90-3, this is pointless, since the vpform never changes.)

@IndexOther(QUERY-SLISTS)@\
A flag setting for QUERY-USER.
In the TEST top level, stops after each setting of the
flags and asks whether to search with those settings.

@IndexOther(SHOW-JFORMS)@\
A flag setting for QUERY-USER.
Like QUERY-JFORMS, but automatically answers no to each 
question (and hence never actually proceeds with a search).@End(Description)

@Section(MS88 search procedure)

@Begin(Description)
@IndexOther(ALLOW-DUPLICATES)@\
A setting for PROP-STRATEGY.
In propositional proof search, one can add a connection to a mating even 
if it is already present.

@IndexOther(HASH-TABLE)@\
A setting for PROP-STRATEGY.
In propositional proof search, one can add a connection to a mating only
if it is not already present in the hash-table.

@IndexOther(PUSHNEW)@\
A setting for PROP-STRATEGY.
In propositional proof search, one can add a connection to a mating only
if it is not already present according to the clisp macro PUSHNEW.@End(Description)

@Section(MS89 search procedure)

@Begin(Description)
@IndexOther(NUM-VPATHS-RANKING)@\
A flag setting for RANK-EPROOF-FN.
Returns the number of vpaths in an expansion proof.@End(Description)

@Section(MS91-6 and MS91-7 search procedures)

@Begin(Description)
@IndexOther(ADD-OPTIONS-COUNT)@\
A flag setting for OPTIONS-GENERATE-FN.
Generate new options when more than OPTIONS-GENERATE-ARG 
different option sets have been tried.

@IndexOther(ADD-OPTIONS-ORIGINAL)@\
A flag setting for OPTIONS-GENERATE-FN.
Generate new options when over OPTIONS-GENERATE-ARG percent of 
the possible option sets have been used, and each option appears 
in at least one option set.

@IndexOther(ADD-OPTIONS-SUBS)@\
A flag setting for OPTIONS-GENERATE-FN.
Generate new options when the number of substitutions and 
duplications in the next option set (i.e. its SIMPLEST-WEIGHT-B)
exceeds OPTIONS-GENERATE-ARG.

@IndexOther(ADD-OPTIONS-WEIGHT)@\
A flag setting for OPTIONS-GENERATE-FN.
Generate new options when the lower end of the acceptable weight 
bracket for a new option set exceeds OPTIONS-GENERATE-ARG.

@IndexOther(ALL-PENALTIES-FN)@\
A setting for WEIGHT-B-FN.
Much the same as SIMPLE-WEIGHT-B-FN but also adds a penalty for 
extra duplications given by the PENALTY-FOR-ORDINARY-DUP flag.

@IndexOther(DOUBLE-ARG)@\
A flag setting for OPTIONS-GENERATE-UPDATE.
Each time options are updated, double the value of 
OPTIONS-GENERATE-ARG.

@IndexOther(DOUBLE-WEIGHT)@\
A flag setting for RECONSIDER-FN.
When an option set runs out of time, double its weight.

@IndexOther(EXPANSION-LEVEL-WEIGHT-A)@\
A setting for the flag WEIGHT-A-FN.
Returns the expansion level of the option to be used as a weight.
The expansion level is (roughly) the number of times that NAME-PRIM
had to be called in order to generate this option -- usually 1.

@IndexOther(IDENT-ARG)@\
A flag setting for OPTIONS-GENERATE-UPDATE.
Each time options are updated, leave the value of 
OPTIONS-GENERATE-ARG unchanged.

@IndexOther(INCREMENT-WEIGHT)@\
A flag setting for RECONSIDER-FN.
When an option set runs out of time, add 10 to its weight.

@IndexOther(INF-ARG)@\
A flag setting for OPTIONS-GENERATE-UPDATE.
Each time options are update, make the value of 
OPTIONS-GENERATE-ARG infinity.

@IndexOther(INF-WEIGHT)@\
A flag setting for RECONSIDER-FN.
When an option set runs out of time, reset its weight to INFINITE
(and hence prevent its ever being reconsidered).

@IndexOther(OPTION-SET-NUM-LEAVES)@\
A flag setting for WEIGHT-C-FN.
Returns the number of leaves in the relevant etree.

@IndexOther(OPTION-SET-NUM-VPATHS)@\
A flag setting for WEIGHT-C-FN.
Returns the number of vertical paths through the relevant etree.

@IndexOther(SIMPLE-WEIGHT-B-FN)@\
A setting for WEIGHT-B-FN.
Returns the sum of the penalties for the primsubs, multiple subs 
and duplications used in the option set (see the flags 
PENALTY-FOR-EACH-PRIMSUB, PENALTY-FOR-MULTIPLE-PRIMSUBS and  
PENALTY-FOR-MULTIPLE-SUBS for more information)

@IndexOther(SIMPLEST-WEIGHT-B-FN)@\
A setting for WEIGHT-B-FN.
Returns 1 for the original option set and adds 
1 for each primsub or duplication (the idea is to set the coefficients of 
weight-a and weight-c to zero while using SIMPLEST-WEIGHT-B-FN).

@IndexOther(SQUARE-ARG)@\
A flag setting for OPTIONS-GENERATE-UPDATE.
Each time options are updated, square the value of 
OPTIONS-GENERATE-ARG.

@IndexOther(SQUARE-WEIGHT)@\
A flag setting for RECONSIDER-FN.
When an option set runs out of time, square its weight.@End(Description)

@Section(Extensional Search)

@Begin(Description)
@IndexOther(MS03-7)@\
A setting for DEFAULT-MS, DEFAULT-MATE and DEFAULT-EXPAND.
This uses the MS03-7 mating search procedure which incorporates 
extensionality reasoning, equality reasoning, and set variable reasoning 
as described in Chad E. Brown's thesis.

The search procedures MS03-7 and MS04-2 are similar in that they are
both extensional search procedures.  MS03-7 does a saturation style
search (with no backtracking).

MS04-2 is proven complete in Chad E. Brown's thesis.  MS03-7 is
probably complete, but this has not been proven.

See Also: MS04-2.

@IndexOther(MS04-2)@\
A setting for DEFAULT-MS, DEFAULT-MATE and DEFAULT-EXPAND.
This uses the MS04-2 mating search procedure which incorporates 
extensionality reasoning, equality reasoning, and set variable reasoning 
as described in Chad E. Brown's thesis.

The search procedures MS03-7 and MS04-2 are similar in that they are
both extensional search procedures.  MS03-7 performs a kind of saturation search.
MS04-2 performs a depth-first search (with weights to control the order of choices)
with backtracking and a depth bound.  Iterative deepening is used to ensure
completeness.

MS04-2 is proven complete in Chad E. Brown's thesis.

See Also: MS03-7.@End(Description)

@Section(Unification)

@Begin(Description)
@IndexOther(ALL-NODES)@\
A setting for SUBSUMPTION-NODES.
Checks all nodes in the unification tree.

@IndexOther(ALWAYS)@\
A setting for DNEG-IMITATION.
Always allow double negations to be used as imitation terms.

@IndexOther(APPLY-MATCH-ALL-FRDPAIRS)@\
A setting for APPLY-MATCH.
In unification search, applies match to all flexible-rigid pairs 
and chooses whichever will have fewest substitutions.

@IndexOther(APPLY-MATCH-ALL-FRDPAIRS-MSV)@\
A setting for APPLY-MATCH.
As for APPLY-MATCH-ALL-FRDPAIRS, but also checks for
MAX-SUBSTS-VAR violations at the same time. This is obsolete,
and is ignored by path-focused procedures.

@IndexOther(APPLY-MATCH-MAX-SUBSTS)@\
A setting for APPLY-MATCH.
In unification search, applies match to whichever flexible-rigid
pair is closest to exceeding the bound in MAX-SUBSTS-VAR.
If it finds one with a unique substitution, it uses that.

@IndexOther(APPLY-MATCH-MIN-SUBSTS)@\
A setting for APPLY-MATCH.
The opposite of APPLY-MATCH-MAX-SUBSTS: chooses the pair
which is farthest from the MAX-SUBSTS-VAR bound. This only
works for non-path-focused procedures, and should be deleted
someday because it's useless.

@IndexOther(APPLY-MATCH-MOST-CONSTS)@\
A setting for APPLY-MATCH.
In unification search, applies match to whichever flex-rigid
pair contains the most constant symbols. This only
works for non-path-focused procedures, and should be deleted
someday because it's useless.

@IndexOther(BEST-FIRST)@\
A setting for UNI-SEARCH-HEURISTIC.
Search the unification tree best-first (take whichever leaf node
has the fewest free variables). BREADTH-FIRST is faster than this.

@IndexOther(BREADTH-FIRST)@\
A setting for UNI-SEARCH-HEURISTIC.
Search the unification tree breadth-first.

@IndexOther(CONST)@\
A setting for DNEG-IMITATION.
Forbid double negations to be used as imitation terms for dpairs 
of the form (f . ~G), where G is a constant, but allows them otherwise.

@IndexOther(CONST-FLEX)@\
A setting for DNEG-IMITATION.
Forbid double negations to be used as imitation terms 
in the two cases CONST and FLEX (see help messages for these
cases), but allow them otherwise.

@IndexOther(DEPTH-FIRST)@\
A setting for UNI-SEARCH-HEURISTIC.
Search the unification tree depth-first, for path-focused procedures.
(There is no reason for this, and you should avoid doing it.)

@IndexOther(FLEX)@\
A setting for DNEG-IMITATION.
Forbid double negations to be used as imitation terms for dpairs 
of the form (f . ~g) if g was created by a double negation
in the first place (this prevents endless cycles), but allow them
otherwise.

@IndexOther(LEAF-NODES)@\
A setting for SUBSUMPTION-NODES.
Checks only those nodes in the unification tree which are leaves.

@IndexOther(NEVER)@\
A setting for DNEG-IMITATION.
Forbid double negations to be used as imitation terms, ever.

@IndexOther(PATH-NODES)@\
A setting for SUBSUMPTION-NODES.
Checks only those nodes in the unification tree on the path 
from the root to the new node.

@IndexOther(QUASI-TPS1)@\
A flag setting for MS-DIR.
The only possible setting for MS-DIR, this is the main routine
which governs the behaviour of MS88 and MS89.@End(Description)

@Section(Tactics)

@Begin(Description)
@IndexOther(AUTO)@\
A flag setting for TACMODE.
Apply tactics in automatic mode (i.e. without user input).

@IndexOther(ETREE-NAT)@\
A flag setting for TACUSE.
Use tactics in etree-nat translation style (i.e. apply them to the
current eproof to create lines of a natural deduction proof).

@IndexOther(INTERACTIVE)@\
A flag setting for TACMODE.
Apply tactics in interactive mode (i.e. prompting the user 
before each application).

@IndexOther(MATE-SRCH)@\
A flag setting for TACUSE.
Unused setting. Eventually, copy and save eproofs with this tactic use.

@IndexOther(NAT-DED)@\
A flag setting for TACUSE.
Use tactics in natural deduction style (i.e. apply them to the
lines of the current dproof).@End(Description)

@Section(suggestions)

@Begin(Description)
@IndexOther(ASK)@\
An action for GO-INSTRUCTIONS.
Ask for input from the user for the next step of GO.

@IndexOther(DO)@\
An action for GO-INSTRUCTIONS.
Generate a list of suggestions for the next step of GO,
and do whatever seems most likely to work.

@IndexOther(FORGET)@\
An action for GO-INSTRUCTIONS.
Do nothing.

@IndexOther(SHOW)@\
An action for GO-INSTRUCTIONS.
Show the suggestions for the next step of GO.@End(Description)

@Section(Searchlists)

@Begin(Description)
@IndexOther(BREADTH-FIRST-SEARCH)@\
A setting for TEST-NEXT-SEARCH-FN.
Tries all combinations of flags, but varies each flag a little at a time
rather than varying one flag through its entire range before trying the next.

@IndexOther(EXHAUSTIVE-SEARCH)@\
A setting for TEST-NEXT-SEARCH-FN.
Tries all combinations of flags in a searchlist, 
varying one flag through its entire range before trying the next flag.

@IndexOther(PRESS-DOWN)@\
A setting for TEST-NEXT-SEARCH-FN.
This setting is used internally by the PRESS-DOWN command.

@IndexOther(PRESS-DOWN-2)@\
A setting for TEST-NEXT-SEARCH-FN.
This behaves like breadth-first search except that if varying
a flag makes the search faster, that flag is then prevented
from returning above its original value (the range of each flag is 
assumed to be ordered; if the range is (A B C D), and setting it to
C results in a faster search, it will never again be set to A or B).

@IndexOther(PUSH-UP)@\
A setting for TEST-NEXT-SEARCH-FN.
This setting is used internally by the PUSH-UP command.

@IndexOther(PUSH-UP-2)@\
A setting for TEST-NEXT-SEARCH-FN.
This setting is like breadth-first search but terminates once a 
successful mode is discovered; it is used for relaxing an unsuccessful
mode until it is successful.@End(Description)

@Section(wff Primitives)

@Begin(Description)
@IndexOther(REN-VAR-X1)@\
A flag setting for REN-VAR-FN.
This is the standard renaming function. It renames y to y^1,
then to y^2, and so on. If there is another variable y, of a different 
type, it makes no difference.

@IndexOther(REN-VAR-X11)@\
A flag setting for REN-VAR-FN.
This is much like REN-VAR-X1, except it will avoid creating
two variables of the same name at different types (so it tends to 
produce higher exponents than REN-VAR-X1).

@IndexOther(REN-VAR-XA)@\
A flag setting for REN-VAR-FN.
This renames variables alphabetically, turning y into ya, then yba, 
and so on.@End(Description)

@Section(Basic Abbreviations)

@Begin(Description)
@IndexOther(ALL)@\
A flag setting for REWRITE-EQUALITIES.
When rewriting an equality (during a ND proof or when constructing 
an etree), rewrite every equality as follows:
 to an equivalence for those of type OOO,
 to the extensional form 
      [lambda f(AB) lambda g(AB) forall x(B) f x = g x]
      for those of type O(AB)(AB)
 to the Leibniz form
      [lambda x(A) lambda y(A) forall q(OA). q x implies q y]
      for those of type OAA.

@IndexOther(LEIBNIZ)@\
A flag setting for REWRITE-EQUALITIES.
When rewriting an equality (during a ND proof or when constructing 
an etree), rewrite every equality using the Leibniz definition
      [lambda x(A) lambda y(A) forall q(OA). q x implies q y]

@IndexOther(ONLY-EXT)@\
A flag setting for REWRITE-EQUALITIES.
When rewriting an equality (during a ND proof or when constructing 
an etree), rewrite only those equalities that can be rewritten using
extensionality.@End(Description)

@Section(Lambda-Calculus)

@Begin(Description)
@IndexOther(BETA-ETA-ONLY)@\
A flag setting for LAMBDA-CONV.
When doing lambda-conversion, only use beta rule, not eta rule (for example,
when translating an eproof into ND style).

@IndexOther(BETA-ETA-SEPARATE)@\
A flag setting for LAMBDA-CONV.
When doing lambda-conversion, use beta and eta rules together (for example,
when translating an eproof into ND style).

@IndexOther(BETA-ETA-TOGETHER)@\
A flag setting for LAMBDA-CONV.
When doing lambda-conversion, use beta and eta rules together (for example,
when translating an eproof into ND style).@End(Description)

@Section(Primitive Substitutions)

@Begin(Description)
@IndexOther(APPEND)@\
A flag setting for PRIM-BDTYPES-AUTO.
The same list is calculated as for REPLACE, but instead
of replacing the current setting of PRIM-BDTYPES it will be appended
to it.

@IndexOther(APPEND-SUB)@\
A flag setting for PRIM-BDTYPES-AUTO.
The same list is calculated as for APPEND, but instead
of replacing the current setting of PRIM-BDTYPES it will be appended
to it.

@IndexOther(IGNORE)@\
A flag setting for PRIM-BDTYPES-AUTO.
The user's setting of PRIM-BDTYPES will be left intact.

@IndexOther(PR00)@\
A flag setting for PRIMSUB-METHOD.
This uses higher order unification to determine set substitutions
that solve part of the mating search in advance.
PR00 only works with 

DEFAULT-MS MS98-1

and

SKOLEM-DEFAULT NIL.

PR00 can be controlled using the flags
PR00-MAX-SUBSTS-VAR, PR00-REQUIRE-ARG-DEPS, PR00-NUM-ITERATIONS.

@IndexOther(PR89)@\
A flag setting for PRIMSUB-METHOD.
Only primsubs of the form "exists x . literal" and 
 "forall x . literal" will be generated.

@IndexOther(PR93)@\
A flag setting for PRIMSUB-METHOD.
For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
At depth 1, a single quantifier is introduced, as in PR89. 
At depth N>1, we have (N-1) quantifiers ranging over a formula
containing (N-1) conjunctions {disjunctions} of (N-2) 
disjunctions {conjunctions}.

@IndexOther(PR95)@\
A flag setting for PRIMSUB-METHOD.
For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
At depth 1, as in PR89.
At depth N>1, we have (N-1) quantifiers ranging over a formula
with between MIN-PRIM-LITS and MAX-PRIM-LITS literals, with
all combinations of connectives between them.

@IndexOther(PR97)@\
A flag setting for PRIMSUB-METHOD.
For all integers from MIN-PRIM-DEPTH to MAX-PRIM-DEPTH:
At depth N>0, we have (N-1) quantifiers ranging over each 
subformula taken from the etree which contains between 
MIN-PRIM-LITS and MAX-PRIM-LITS literals. You can see these
subformulas by doing NAME-PRIM from the MATE top level. (Note:
both the instantiated and uninstantiated versions of each 
definition are used.)

@IndexOther(PR97A)@\
A flag setting for PRIMSUB-METHOD.
Exactly as for PR97, but all substitutions are put into 
negation normal form.

@IndexOther(PR97B)@\
A flag setting for PRIMSUB-METHOD.
The substitutions from PR97A and PR95 are interleaved. The order
is determined firstly by the number of literals, then by the number of
quantifiers, and lastly with PR97 substs taking precedence over PR95.

@IndexOther(PR97C)@\
A flag setting for PRIMSUB-METHOD.
Using the connectives AND and OR, and the quantifiers EXISTS and
FORALL (ranging over variables of types PRIM-BDTYPES), and also using
any abbreviations or equalities that occur in the gwff to be proven, 
primsubs are built up using the bounds given by MIN- and MAX-PRIM-LITS
and MIN- and MAX-PRIM-DEPTH. See also PR97C-PRENEX and PR97C-MAX-ABBREVS.

@IndexOther(REPLACE)@\
A flag setting for PRIM-BDTYPES-AUTO.
The value of PRIM-BDTYPES will be changed to an 
automatically-generated list of all the primitive types used in 
the gwff to be proven.

@IndexOther(REPLACE-SUB)@\
A flag setting for PRIM-BDTYPES-AUTO.
The value of PRIM-BDTYPES will be changed to an 
automatically-generated list of all the subtypes of the 
types that appear in the gwff.@End(Description)

@Section(Maintenance)

@Begin(Description)
@IndexOther(INIT-DEFINE-MY-DEFAULT-MODE)@\
A setting for INIT-DIALOGUE-FN.
Define a mode MY-DEFAULT-MODE containing all the flag settings as they
were immediately after startup (after the .ini files were read).

@IndexOther(INIT-DIALOGUE-DEFAULT-FN)@\
A setting for INIT-DIALOGUE-FN.
Does nothing (except complain that you need to pick a different
setting for INIT-DIALOGUE-FN!).@End(Description)

@Section(Modules)

@Begin(Description)
@IndexOther(BIG-BACKUP-LIB-DIR)@\
BIG-BACKUP-LIB-DIR is an alias, defined as:
(set-flag 'backup-lib-dir
	  '("/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/andrews/" 
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/andrews-at-itps/" 
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/cebrown/" 
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/cebrown-saar/" 
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/cebrown-at-cebtps/" 
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/mbishop/"
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/mszudzik/"
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/chrisb/"
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/hwxi/" 
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/kaminski/"
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/kaminski-at-mx/"
            "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/hardt/"
            "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/mwasson/"
            "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/pmckenne/"
            "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/jkilgall/"
            "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/chretien/"
            "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/tptp/"
	    "/afs/andrew.cmu.edu/mcs/math/TPS/tpslib/huilong/"))

@IndexOther(DISPLAYTITLE)@\
DISPLAYTITLE is an alias, defined as:
DISPLAYFILE "/afs/andrew.cmu.edu/mcs/math/TPS/tutorial/title"

@IndexOther(DISPLAYTLC)@\
DISPLAYTLC is an alias, defined as:
DISPLAYFILE  "/afs/andrew.cmu.edu/mcs/math/TPS/tutorial/tlc"

@IndexOther(ECONJ*)@\
ECONJ* is an alias, defined as:
use-tactic econj*-tac nat-ded auto

@IndexOther(ICONJ*)@\
ICONJ* is an alias, defined as:
use-tactic iconj*-tac nat-ded auto

@IndexOther(LOUD)@\
LOUD is an alias, defined as:
(setq auto::unify-verbose 'max  auto::mating-verbose 'max  auto::ms98-verbose t auto::etree-nat-verbose '(^p pall pstatus ^pn prfw-^p prfw-^pn prfw-pall)  auto::tactic-verbose 'max  auto::options-verbose t  load-warn-p t  print-nodenames nil  auto::conn-debug t  auto::natree-debug t  auto::merge-debug t  auto::*print-symmetry-verbose* t  auto::*print-eproof-verbose* t printlineflag t)

@IndexOther(QUIET)@\
QUIET is an alias, defined as:
(setq auto::unify-verbose 'auto::silent  auto::mating-verbose 'auto::silent  auto::ms98-verbose nil auto::etree-nat-verbose nil  auto::tactic-verbose 'min  auto::options-verbose nil  load-warn-p nil  print-nodenames t  auto::conn-debug nil  auto::natree-debug nil  auto::merge-debug nil  auto::*print-symmetry-verbose* nil  auto::*print-eproof-verbose* nil printlineflag nil)

@IndexOther(SETUP2B)@\
SETUP2B is an alias, defined as:
RIGHTMARGIN 80 &  
   CHARSIZE MAX &
   PROOFW-ALL-WIDTH  80 &
   PROOFW-ACTIVE NIL &
   PROOFW-ACTIVE+NOS NIL &
   EDWIN-TOP T &
   EDWIN-TOP-HEIGHT 4 &
   EDWIN-CURRENT T &
   EDWIN-VPFORM NIL &
   EDWIN-TOP-WIDTH 80 &
   EDWIN-CURRENT-WIDTH 80 &
   TURNSTILE-INDENT-AUTO VARY &
   ETREE-NAT-VERBOSE (PRFW-PALL PRFW-^P PRFW-^PN ^PN)

@IndexOther(SETUP3E)@\
SETUP3E is an alias, defined as:
RIGHTMARGIN 100 &  
   CHARSIZE MAX &
   PROOFW-ALL-WIDTH  100 &
   PROOFW-ALL-HEIGHT 22 &
   PROOFW-ACTIVE-WIDTH 100 &
   PROOFW-ACTIVE-HEIGHT 6 &
   PROOFW-ACTIVE+NOS NIL &
   EDWIN-TOP-WIDTH 100 &
   EDWIN-CURRENT-WIDTH 100 &
   EDWIN-VPFORM-WIDTH 100 &
   TURNSTILE-INDENT-AUTO VARY &
   ETREE-NAT-VERBOSE (PRFW-PALL PRFW-^P PRFW-^PN ^PN)

@IndexOther(TEST-BOOL)@\
TEST-BOOL is an alias, defined as:
(set-flag 'test-theorems '((cl-user::bool-prop-23 cl-user::bool-prop-mode)(cl-user::bool-prop-25 cl-user::bool-prop-mode)(cl-user::bool-prop-27 cl-user::bool-prop-mode)(cl-user::bool-prop-29 cl-user::bool-prop-mode)(cl-user::bool-prop-30 cl-user::bool-prop-mode)(cl-user::bool-prop-31 cl-user::bool-prop-mode)(cl-user::bool-prop-32 cl-user::bool-prop-mode)(cl-user::bool-prop-33 cl-user::bool-prop-mode)(cl-user::bool-prop-34 cl-user::bool-prop-mode)(cl-user::bool-prop-35 cl-user::bool-prop-mode)(cl-user::bool-prop-37 cl-user::bool-prop-mode)(cl-user::bool-prop-38 cl-user::bool-prop-mode)(cl-user::bool-prop-39 cl-user::bool-prop-mode)(cl-user::bool-prop-40 cl-user::bool-prop-mode)(cl-user::bool-prop-41 cl-user::bool-prop-mode)(cl-user::bool-prop-42 cl-user::bool-prop-mode)(cl-user::bool-prop-44 cl-user::bool-prop-mode)(cl-user::bool-prop-45 cl-user::bool-prop-mode)(cl-user::bool-prop-46 cl-user::bool-prop-mode)(cl-user::bool-prop-47 cl-user::bool-prop-mode)(cl-user::bool-prop-48 cl-user::bool-prop-mode)(cl-user::bool-prop-49 cl-user::bool-prop-mode)(cl-user::bool-prop-50 cl-user::bool-prop-mode)(cl-user::bool-prop-51 cl-user::bool-prop-mode)(cl-user::bool-prop-52 cl-user::bool-prop-mode)(cl-user::bool-prop-53 cl-user::bool-prop-mode)(cl-user::bool-prop-54 cl-user::bool-prop-mode)(cl-user::bool-prop-55 cl-user::bool-prop-mode)(cl-user::bool-prop-56 cl-user::bool-prop-mode2)(cl-user::bool-prop-57 cl-user::bool-prop-mode2)(cl-user::bool-prop-58 cl-user::bool-prop-mode)(cl-user::bool-prop-59 cl-user::bool-prop-mode)(cl-user::bool-prop-60 cl-user::bool-prop-mode)(cl-user::bool-prop-61 cl-user::bool-prop-mode)(cl-user::bool-prop-64 cl-user::bool-prop-mode)(cl-user::bool-prop-67 cl-user::bool-prop-mode)(cl-user::bool-prop-68 cl-user::bool-prop-mode)(cl-user::bool-prop-69 cl-user::bool-prop-mode)(cl-user::bool-prop-70 cl-user::bool-prop-mode)(cl-user::bool-prop-71 cl-user::bool-prop-mode)(cl-user::bool-prop-72 cl-user::bool-prop-mode)(cl-user::bool-prop-74 cl-user::bool-prop-mode)(cl-user::bool-prop-75 cl-user::bool-prop-mode)(cl-user::bool-prop-76 cl-user::bool-prop-mode)(cl-user::bool-prop-77 cl-user::bool-prop-mode)(cl-user::bool-prop-78 cl-user::bool-prop-mode)(cl-user::bool-prop-79 cl-user::bool-prop-mode)(cl-user::bool-prop-80 cl-user::bool-prop-mode)(cl-user::bool-prop-81 cl-user::bool-prop-mode)(cl-user::bool-prop-82 cl-user::bool-prop-mode)(cl-user::bool-prop-83 cl-user::bool-prop-mode)(cl-user::bool-prop-84 cl-user::bool-prop-mode)(cl-user::bool-prop-85 cl-user::bool-prop-mode)(cl-user::bool-prop-86 cl-user::bool-prop-mode)(cl-user::bool-prop-87 cl-user::bool-prop-mode)(cl-user::bool-prop-88 cl-user::bool-prop-mode)(cl-user::bool-prop-89 cl-user::bool-prop-mode)(cl-user::bool-prop-90 cl-user::bool-prop-mode)(cl-user::bool-prop-92 cl-user::bool-prop-mode)(cl-user::bool-prop-93 cl-user::bool-prop-mode)(cl-user::bool-prop-95 cl-user::bool-prop-mode)(cl-user::bool-prop-96 cl-user::bool-prop-mode)(cl-user::bool-prop-97 cl-user::bool-prop-mode)(cl-user::bool-prop-98 cl-user::bool-prop-mode)(cl-user::bool-prop-99 cl-user::bool-prop-mode)(cl-user::bool-prop-100 cl-user::bool-prop-mode)(cl-user::bool-prop-101 cl-user::bool-prop-mode)(cl-user::bool-prop-102 cl-user::bool-prop-mode)(cl-user::bool-prop-104 cl-user::bool-prop-mode)(cl-user::bool-prop-110 cl-user::bool-prop-mode)(cl-user::bool-prop-111 cl-user::bool-prop-mode)(cl-user::bool-prop-112 cl-user::bool-prop-mode)(cl-user::bool-prop-113 cl-user::bool-prop-mode)(cl-user::bool-prop-114 cl-user::bool-prop-mode)(cl-user::bool-prop-115 cl-user::bool-prop-mode)(cl-user::bool-prop-116 cl-user::bool-prop-mode)(cl-user::bool-prop-117 cl-user::bool-prop-mode)(cl-user::bool-prop-118 cl-user::bool-prop-mode)(cl-user::bool-prop-120 cl-user::bool-prop-mode)))

@IndexOther(TEST-DEFAULT)@\
TEST-DEFAULT is an alias, defined as:
(set-flag 'test-theorems '((cl-user::thm30  cl-user::mode-thm30) (cl-user::thm47  cl-user::mode-thm47-g) (cl-user::thm48  cl-user::mode-thm48-e) (cl-user::thm67  cl-user::mode-thm67-a) (cl-user::thm112  cl-user::mode-thm112-b) (cl-user::thm112a  cl-user::mode-thm112a-try5)
 (cl-user::thm115 cl-user::mode-thm115-pr97a)
 (cl-user::thm117c  cl-user::mode-thm117b) (cl-user::thm129  cl-user::mode-thm129-e) (cl-user::thm130  cl-user::mode-thm129-b) (cl-user::thm133  cl-user::mode-x5200) (cl-user::thm134  cl-user::mode-thm134-a) (cl-user::thm135  cl-user::mode-thm135-1) (cl-user::thm300a  cl-user::mode-thm300a-4) (cl-user::thm301a  cl-user::mode-thm301-a) (cl-user::thm303  cl-user::mode-thm303-dtps) (cl-user::bledsoe-feng-sv-i1  cl-user::mode-thm129-d) (cl-user::x2115  cl-user::mode-x2129-a) (cl-user::x2116  cl-user::mode-x2116) (cl-user::x2129  cl-user::mode-x2129-c) (cl-user::x5200  cl-user::mode-x5200-a) (cl-user::x5205  cl-user::mode-x5205) (cl-user::x5304  cl-user::mode-x5304) (cl-user::x5305  cl-user::mode-x5305) (cl-user::x5308  cl-user::mode-x5308-b) (cl-user::x5310  cl-user::mode-x5310-a)
 (cl-user::thm15b  cl-user::MODE-THM15B-NEW1)))

@IndexOther(TEST-LONG)@\
TEST-LONG is an alias, defined as:
(set-flag 'test-theorems '((cl-user::thm104  cl-user::mode-thm104-a) (cl-user::thm112  cl-user::mode-thm112-b) (cl-user::thm112a  cl-user::mode-thm112a-try5) (cl-user::thm30  cl-user::mode-thm30) (cl-user::thm47  cl-user::mode-thm47-g) (cl-user::thm48  cl-user::mode-thm48-e) (cl-user::thm67  cl-user::mode-thm67-a)
 (cl-user::thm115 cl-user::mode-thm115-pr97a)
 (cl-user::thm117c  cl-user::mode-thm117b) (cl-user::thm129  cl-user::mode-thm129-e) (cl-user::thm130  cl-user::mode-thm129-b) (cl-user::thm131 cl-user::mode-thm131-a) (cl-user::thm133  cl-user::mode-x5200) (cl-user::thm134  cl-user::mode-thm134-a) (cl-user::thm135  cl-user::mode-thm135-1) (cl-user::thm300a  cl-user::mode-thm300a-4) (cl-user::thm301a  cl-user::mode-thm301-a) (cl-user::thm303  cl-user::mode-thm303-dtps) (cl-user::bledsoe-feng-sv-i1  cl-user::mode-thm129-d) (cl-user::bledsoe-feng-sv-i2 cl-user::mode-bledsoe-feng-sv-i2-b) (cl-user::x2115  cl-user::mode-x2129-a) (cl-user::x2116  cl-user::mode-x2116) (cl-user::x2129  cl-user::mode-x2129-c) (cl-user::x5200  cl-user::mode-x5200-a) (cl-user::x5205  cl-user::mode-x5205) (cl-user::x5304  cl-user::mode-x5304) (cl-user::x5305  cl-user::mode-x5305) (cl-user::x5308  cl-user::mode-x5308-b) (cl-user::x5310  cl-user::mode-x5310-a)
(cl-user::thm15a cl-user::mode-thm15a-1)
(cl-user::thm15b  cl-user::MODE-THM15B-NEW1) ))

@IndexOther(TEST-MS98)@\
TEST-MS98 is an alias, defined as:
(set-flag 'test-theorems '((cl-user::x2106 cl-user::ms98-fo-mode) (cl-user::x2107 cl-user::ms98-fo-mode) (cl-user::x2109 cl-user::ms98-fo-mode) (cl-user::x2110 cl-user::ms98-fo-mode) (cl-user::x2111 cl-user::ms98-fo-mode) (cl-user::x2113 cl-user::ms98-fo-mode) (cl-user::x2114 cl-user::ms98-fo-mode) (cl-user::x2115 cl-user::ms98-fo-mode) (cl-user::x2116 cl-user::ms98-fo-mode) (cl-user::x2118 cl-user::ms98-fo-mode) (cl-user::x2119 cl-user::ms98-fo-mode) (cl-user::x2121 cl-user::ms98-fo-mode) (cl-user::x2122 cl-user::ms98-fo-mode) (cl-user::x2123 cl-user::ms98-fo-mode) (cl-user::x2124 cl-user::ms98-fo-mode) (cl-user::x2126 cl-user::ms98-fo-mode) (cl-user::x2127 cl-user::ms98-fo-mode) (cl-user::x2128 cl-user::ms98-fo-mode) (cl-user::x2131 cl-user::ms98-fo-mode) (cl-user::x2132 cl-user::ms98-fo-mode) (cl-user::x2134 cl-user::ms98-fo-mode) (cl-user::x2135 cl-user::ms98-fo-mode) (cl-user::x2136 cl-user::ms98-fo-mode) (cl-user::x2137 cl-user::ms98-fo-mode) (cl-user::x2138 cl-user::ms98-fo-mode) (cl-user::x2108 cl-user::ms98-fo-mode) (cl-user::x2112 cl-user::ms98-fo-mode) (cl-user::x2117 cl-user::ms98-fo-mode) (cl-user::x2120 cl-user::ms98-fo-mode) (cl-user::x2125 cl-user::ms98-fo-mode) (cl-user::x2130 cl-user::ms98-fo-mode) (cl-user::x2133 cl-user::ms98-fo-mode) (cl-user::lx1 cl-user::ms98-fo-mode) (cl-user::pell19 cl-user::ms98-fo-mode) (cl-user::pell21 cl-user::ms98-fo-mode) (cl-user::pell25 cl-user::ms98-fo-mode) (cl-user::pell26 cl-user::ms98-fo-mode) (cl-user::pell27 cl-user::ms98-fo-mode) (cl-user::pell28 cl-user::ms98-fo-mode) (cl-user::pell29 cl-user::ms98-fo-mode) (cl-user::pell35 cl-user::ms98-fo-mode) (cl-user::pell40 cl-user::ms98-fo-mode) (cl-user::russell1 cl-user::ms98-fo-mode) (cl-user::thm25 cl-user::ms98-fo-mode) (cl-user::thm31 cl-user::ms98-fo-mode) (cl-user::thm39 cl-user::ms98-fo-mode) (cl-user::thm68 cl-user::ms98-fo-mode) (cl-user::thm69 cl-user::ms98-fo-mode) (cl-user::thm72 cl-user::ms98-fo-mode) (cl-user::thm75 cl-user::ms98-fo-mode) (cl-user::x2150 cl-user::ms98-fo-mode) (cl-user::x3411 cl-user::ms98-fo-mode) (cl-user::y2141 cl-user::ms98-fo-mode) (cl-user::thm147 cl-user::ms98-fo-mode) (cl-user::thm100 cl-user::thm100-mode-b) (cl-user::pell42 cl-user::ms98-fo-mode) (cl-user::thm119 cl-user::mode-thm119-ms98) (cl-user::x5200 cl-user::ms98-ho-mode) (cl-user::x5201 cl-user::ms98-ho-mode) (cl-user::x5202 cl-user::ms98-ho-mode) (cl-user::x5203 cl-user::ms98-ho-mode) (cl-user::x5205 cl-user::ms98-ho-mode) (cl-user::x5207 cl-user::ms98-ho-mode2) (cl-user::x5208 cl-user::ms98-ho-mode) (cl-user::x5209 cl-user::ms98-ho-mode) (cl-user::x5210 cl-user::ms98-ho-mode) (cl-user::x5212 cl-user::ms98-ho-mode) (cl-user::x5304 cl-user::ms98-ho-mode3) (cl-user::x5305 cl-user::ms98-ho-primsubs) (cl-user::x5308 cl-user::ms98-ho-mode) (cl-user::x5310 cl-user::mode-x5310-ms98) (cl-user::x6004 cl-user::ms98-ho-mode) (cl-user::thm126 cl-user::mode-thm126-ms98) (cl-user::thm136 cl-user::mode-thm136-ms98) (cl-user::thm270 cl-user::mode-thm270-ms98) (cl-user::grp-comm2 cl-user::mode-grp-comm2-ms98) (cl-user::equiv-01-02 cl-user::mode-equiv123-ms98) (cl-user::equiv-01-03 cl-user::mode-equiv123-ms98) (cl-user::equiv-02-03 cl-user::mode-equiv123-ms98) (cl-user::cd-lattice-thm cl-user::mode-cd-lattice-thm) (cl-user::distrib-thm cl-user::mode-distrib-thm-ms98) (cl-user::pentagon-thm2b cl-user::mode-pentagon-thm2b) (cl-user::modular-thm cl-user::mode-modular-thm-ms98) (cl-user::pa-thm2 cl-user::mode-pa-thm2-ms98) (cl-user::thm15b cl-user::mode-thm15b-ms98-3) (cl-user::cr-theorem cl-user::ms98-cr-theorem-mode) (cl-user::3-diamond-thm cl-user::mode-pentagon-thm2b)))

@IndexOther(TEST-PR00)@\
TEST-PR00 is an alias, defined as:
(set-flag 'test-theorems '((x5310 mode-x5310-pr00)
(thm578 mode-thm578-pr00)
(thm579 mode-thm579-pr00)
(thm581 mode-thm581-pr00)
(thm582 mode-thm582-pr00)
(thm583 mode-thm583-pr00)
(thm584 mode-thm584-pr00)
(THM112A MODE-THM112A-PR00)
))

@IndexOther(TEST-SHORT)@\
TEST-SHORT is an alias, defined as:
(set-flag 'test-theorems '((cl-user::thm104  cl-user::mode-thm104-a) (cl-user::thm112  cl-user::mode-thm112-b) (cl-user::thm30  cl-user::mode-thm30) (cl-user::thm47  cl-user::mode-thm47-g) (cl-user::thm48  cl-user::mode-thm48-e) (cl-user::thm67  cl-user::mode-thm67-a)
 (cl-user::thm115 cl-user::mode-thm115-pr97a)
 (cl-user::thm117c  cl-user::mode-thm117b) (cl-user::thm129  cl-user::mode-thm129-e) (cl-user::thm130  cl-user::mode-thm129-b) (cl-user::thm131 cl-user::mode-thm131-a) (cl-user::thm133  cl-user::mode-x5200) (cl-user::thm134  cl-user::mode-thm134-a) (cl-user::thm300a  cl-user::mode-thm300a-4) (cl-user::thm301a  cl-user::mode-thm301-a) (cl-user::bledsoe-feng-sv-i1  cl-user::mode-thm129-d) (cl-user::bledsoe-feng-sv-i2 cl-user::mode-bledsoe-feng-sv-i2-b) (cl-user::x2115  cl-user::mode-x2129-a) (cl-user::x2116  cl-user::mode-x2116) (cl-user::x5200  cl-user::mode-x5200-a) (cl-user::x5205  cl-user::mode-x5205) (cl-user::x5304  cl-user::mode-x5304) (cl-user::x5305  cl-user::mode-x5305) (cl-user::x5308  cl-user::mode-x5308-b) ))

@IndexOther(TEST-UN88)@\
TEST-UN88 is an alias, defined as:
(set-flag 'test-theorems '((cl-user::thm112c cl-user::mode-thm112c-msq)(cl-user::thm130 cl-user::mode-thm130-msq)(cl-user::thm130a cl-user::mode-thm130a-msq)(cl-user::thm301 cl-user::mode-thm301-msq)(cl-user::thm301a cl-user::mode-thm301a-msq)(cl-user::thm30 cl-user::mode-thm30-msq)(cl-user::x5304 cl-user::mode-x5304-msq)(cl-user::x5305 cl-user::mode-x5305-msq)(cl-user::x5308 cl-user::mode-x5308-msq)(cl-user::thm171 cl-user::mode-thm171-msq)(cl-user::thm117b cl-user::mode-thm117b-msq)(cl-user::thm117c cl-user::mode-thm117c-msq)(cl-user::thm141 cl-user::mode-thm141-msq)(cl-user::thm7 cl-user::mode-thm7-msq)(cl-user::thm112 cl-user::mode-thm112-msq)))

@IndexOther(UGEN*)@\
UGEN* is an alias, defined as:
use-tactic (repeat ugen-tac) nat-ded auto@End(Description)
@ChapterPh(Grader Commands)
The internal name of this category is 
GEXPR.
A Grader Command can be defined using DEFGEXPR.
Allowable properties are: @t{ARGTYPES}, @t{ARGNAMES}, @t{ARGHELP}, @t{MAINFNS}, @t{PRINT-COMMAND}, @t{DONT-RESTORE}, @t{MHELP}.

@Section(Getting Out and Help)

@Begin(Description)
@IndexOther(GR-EXIT)@\
Leave GRADING PACKAGE, and exit TPS.

@IndexOther(GR-LEAVE)@\
Leave GRADING PACKAGE to the next enclosing top level.

@IndexOther(LEAVE)@\
Leave GRADING PACKAGE to the next enclosing top level.@End(Description)

@Section(Variables)

@Begin(Description)
@IndexOther(CHG-VARS)@\
Change the values of various variables.

@IndexOther(GR-REVIEW)@\
Enter REVIEW to change VARIABLES.@End(Description)

@Section(The Grade-File)

@Begin(Description)
@IndexOther(CREATE-GRADEFILE)@\
Create a new grade file.@End(Description)

@Section(Manual Grades)

@Begin(Description)
@IndexOther(ALTER-GRADE)@\
Change the existing grades of some students.

@IndexOther(INSERT-GRADES)@\
Insert one or more grades in the grade file.

@IndexOther(LATE-EXERCISES)@\
Use this command to keep track of students who submit late
	 assignments.

@IndexOther(MODIFY-GRADE)@\
Change the existing grades of some students.

@IndexOther(RESUME-INSERT-GRADES)@\
Resume entering grades from a previously interrupted session.@End(Description)

@Section(Automatic Grades)

@Begin(Description)
@IndexOther(DUE-DATES)@\
Assign due-dates to exercises.

@IndexOther(ETPS-GRADE)@\
Copy grades from ETPS record file to GRADE FILE.@End(Description)

@Section(The Class List)

@Begin(Description)
@IndexOther(ADD-STUDENTS)@\
Insert students in the grade file.

@IndexOther(DELETE-STUDENT)@\
Delete some students from the grade file.@End(Description)

@Section(Making the Output Convenient)

@Begin(Description)
@IndexOther(ALIASES)@\
Assign actual names to exercises. The teacher may use
	 short names for the assignments (to obtain a display which
	 can fit on paper), and use this function to keep track of
	 their actual names.

@IndexOther(CHANGE-SEQUENCE)@\
change the sequence of assignments

@IndexOther(COMMENT)@\
To insert comments in the grade file.@End(Description)

@Section(Generating Values)

@Begin(Description)
@IndexOther(STATISTICS)@\
Compute statistical data.@End(Description)

@Section(Displaying Information)

@Begin(Description)
@IndexOther(DISPLAY)@\
Display student-grades on the terminal.

@IndexOther(INFO-EXERCISES)@\
Display aliases, penalty-fns, statistical data, weight, and
due-dates for the exercises on the terminal.

@IndexOther(NUMBER-OF-STUDENTS)@\
Use this command to find the number of students in the grade-file@End(Description)

@Section(Totaling)

@Begin(Description)
@IndexOther(CALCULATE-GRADE)@\
Compute totals.

@IndexOther(CHANGE-WEIGHT)@\
Change existing weighting factors.

@IndexOther(PENALTY-FNS)@\
Assign penalty functions for various exercises.@End(Description)

@Section(Sorting)

@Begin(Description)
@IndexOther(SORT-FN)@\
Sort the grades.@End(Description)

@Section(Letter-Grades)

@Begin(Description)
@IndexOther(LETTER-GRADE)@\
Assign letter grades.@End(Description)
@ChapterPh(Events)
The internal name of this category is 
EVENT.
An event can be defined using DEFEVENT.
Allowable properties are: @t{EVENT-ARGS}, @t{TEMPLATE}, @t{TEMPLATE-NAMES}, @t{WRITE-WHEN}, @t{WRITE-FILE}, @t{SIGNAL-HOOK}, @t{WRITE-HOOK}, @t{MHELP}.

@Section(MS88 search procedure)

@Begin(Description)
@IndexOther(ADDED-CONN)@\
Event which is signalled whenever a connection is added to a mating.

@IndexOther(CONSIDERED-CONN)@\
Event which is signalled whenever a connection is considered.

@IndexOther(DUPE)@\
Event which is signalled whenever a variable duplication is
done in a mating.

@IndexOther(DUPE-VAR)@\
Event which is signalled whenever a variable is duplicated.

@IndexOther(INCOMP-MATING)@\
Event which is signalled whenever an incompatible mating is found.

@IndexOther(MATE-SUBSUMED-TEST)@\
Event which is signalled whenever a mating is tested for 
subsumption.

@IndexOther(MATE-SUBSUMED-TRUE)@\
Event which is signalled whenever a mating is subsumed by
an incompatible mating.

@IndexOther(MATING-CHANGED)@\
Event which is signalled whenever a different mating is considered.

@IndexOther(PRIMSUB)@\
Event which is signalled whenever a primitive substitution
is applied to an expansion tree.

@IndexOther(REMOVED-CONN)@\
Event which is signalled whenever a connection is removed from a mating.

@IndexOther(START-TIME)@\
Event which is signalled whenever a mating should have its run
time started, such as when it becomes the active mating.

@IndexOther(STOP-TIME)@\
Event which is signalled whenever a mating should have its run
time stopped, such as when it is no longer the active mating.

@IndexOther(UNIF-SUBSUMED-TEST)@\
Event which is signalled whenever a set of disagreement pairs
unification is tested for subsumption.

@IndexOther(UNIF-SUBSUMED-TRUE)@\
Event which is signalled whenever a set of disagreement pairs is
found to be subsumed by an ununifiable set.@End(Description)

@Section(Events)

@Begin(Description)
@IndexOther(ADVICE-ASKED)@\
Event of user asking for advice.

@IndexOther(COMMAND)@\
Event of user issuing a command.

@IndexOther(DONE-EXC)@\
The event of completing an exercise.

@IndexOther(ERROR)@\
The event of a Lisp Error.

@IndexOther(INPUT-ERROR)@\
Event of illegal input caught by TPS.

@IndexOther(PROOF-ACTION)@\
The event of completing any proof.

@IndexOther(RULE-ERROR)@\
Event of illegal rule applications caught by TPS.@End(Description)
@ChapterPh(Lisp Packages)
The internal name of this category is 
LISP-PACK.
A Lisp package can be defined using DEF-LISP-PACKAGE1.
Allowable properties are: @t{NEEDED-LISP-PACKAGES}, @t{MHELP}.

@Section(Lisp packages)

@Begin(Description)
@IndexOther(AUTO)@\
The automatic component, including unification and matingsearch.

@IndexOther(CORE)@\
The core system for TPS containing many of its TPS packages.

@IndexOther(MAINT)@\
System maintenance packages including automatic documentation and
the rules package.

@IndexOther(ML)@\
The Math Logic I & II logic.

@IndexOther(TEACHER)@\
For teachers using ETPS in their courses.@End(Description)
@ChapterPh(Modules)
The internal name of this category is 
MODULE.
A module can be defined using DEFMODULE.
Allowable properties are: @t{NEEDED-MODULES}, @t{LISP-PACK}, @t{MACRO-FILES}, @t{FILES}, @t{MHELP}.

@Section(Modules)

@Begin(Description)
@IndexOther(AUTO-BASIC) @\
Files needed by various TPS modules in auto package.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{ARGTYP-AUTO   }

files: @\@t{NODE   }

needed-modules: @\@t{WFF-EDITOR   }@t{VPFORMS   }

@End(Description)

@IndexOther(AUTO-DOC) @\
Defines commands to automatically produce TPS documentation.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{DOCDEF   }

files: @\@t{LATEXDOC   }@t{SCRDOC   }@t{PLURALS   }@t{COLLECT-HELP   }@t{HTMLDOC   }@t{OMDOC   }

needed-modules: @\@t{TPS-HELP   }@t{WFF-PRINT   }

@End(Description)

@IndexOther(BARE) @\
The barest possible TPS.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{TOPS20   }@t{LSPPCK-CORE   }@t{TOP   }@t{MACSYS   }@t{LINEREADP   }@t{TPS3-SAVE   }

needed-modules: @\@t{TPSDEF   }

@End(Description)

@IndexOther(BOOTSTRAP) @\
All files needed to bootstrap TPS.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{BOOT0   }@t{BOOT1   }@t{DEFPCK   }

@End(Description)

@IndexOther(CONCEPT-BARE) @\
Defines functions specific to the Concept-100.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{CONCPT   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(CONCEPT-WFF) @\
Defines functions for printing and parsing on a Concept.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{CONSTY   }

files: @\@t{CFONT   }

needed-modules: @\@t{WFF-PARSE   }@t{CONCEPT-BARE   }

@End(Description)

@IndexOther(ENVIRONMENT) @\
Defines the ENVIRONMENT facility.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ENVIRON   }

needed-modules: @\@t{TPS-HELP   }

@End(Description)

@IndexOther(ETPS-EVENTS) @\
Defines events which could be signalled in ETPS.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ETPS-EVENTS   }@t{TPS3-ERROR   }

needed-modules: @\@t{EVENTS   }

@End(Description)

@IndexOther(ETR-NAT) @\
Defines functions needed for conversion from expansion tree proofs
to natural deduction proofs and vice versa.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ETR-NAT-MACROS   }@t{DIY   }@t{NAT-ETR   }@t{SYMSIMP   }@t{SYMSIMP2   }@t{ETREES-AUTO-SUGGEST   }@t{FTREE-SEQ   }@t{HX-NATREE-TOP   }@t{CEB-NAT-SEQ   }@t{CEB-NAT-ETR   }@t{LEMMAS   }

needed-modules: @\@t{MATING-TRANSFORM   }@t{TACTICS   }

@End(Description)

@IndexOther(EVENT-SIGNAL) @\
Lets the system signal events.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{EVENT-SIGNAL-UTILS   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(EVENTS) @\
Defines category of EVENT and associated functions.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{EVENTS-MAC   }

files: @\@t{EVENTS   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(EXPANSION-TREE) @\
Defines expansion trees and associated wffops.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{ETREES-DEF   }

files: @\@t{ETREES-WFFOPS   }@t{ETREES-WFFOPS2   }@t{ETREES-PRINT   }@t{ETREES-JFORMS   }@t{FTREES   }@t{ETREES-DEBUG   }@t{ETREES-RENUMBER   }@t{MTREE-DATASTRUCTURE   }

needed-modules: @\@t{MATING   }

@End(Description)

@IndexOther(EXT-DAGS) @\
Extensional expansion dags and extensional sequent calculus related code.
See Chad E. Brown's thesis.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{EXT-EXP-DAG-MACROS   }

files: @\@t{EXT-SEQ   }@t{EXT-SEQ-TOP   }@t{EXT-MATE-TOP   }@t{EXT-EXP-DAGS   }@t{EXT-EXP-OPEN-DAGS   }@t{EXT-SEQ-TACTICS   }@t{EXT-EXP-DAGS-ND   }@t{EXT-SEARCH   }@t{MS04-SEARCH   }

needed-modules: @\@t{TACTICS   }@t{MS90-3   }

@End(Description)

@IndexOther(EXTERNAL-INTERFACE) @\
Files for using an external interface, e.g., the Java interface.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{EXTERNAL-INTERFACE   }

@End(Description)

@IndexOther(EXTERNAL-SERVICES) @\
Files for providing services for external programs such as Omega and to access MathWeb services.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{SOCKET   }@t{PROCESS   }@t{SERV   }@t{TPS-PROCESSES   }@t{EXTERNAL   }

needed-modules: @\@t{ETR-NAT   }@t{EXTERNAL-INTERFACE   }

@End(Description)

@IndexOther(FILE-OPS) @\
Some file utilities, e.g. FILETYPE.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{FILSYS   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(GRADER) @\
The grading package.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{GR-MACROS   }

files: @\@t{GRADES1   }@t{GRADES2   }

needed-modules: @\@t{ETPS-EVENTS   }@t{GRADER-TOP   }

@End(Description)

@IndexOther(GRADER-TOP) @\
The grading package.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{GRADES-TOP   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(JFORMS) @\
Defines operations associated with creating jforms.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{JFORMS-DEFNS   }

files: @\@t{JFORMS-LABELS   }@t{JFORMS   }@t{ORDER-COMPONENTS   }@t{WEAK-MAC-AUTO   }@t{JFORMS-EDOPS   }

needed-modules: @\@t{WFF-PARSE   }

@End(Description)

@IndexOther(LAMBDA-CALC) @\
Defines some operations of the typed lambda-calculus.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{EDLMBD   }@t{CNF   }

needed-modules: @\@t{WFF-EDITOR   }

@End(Description)

@IndexOther(LIBRARY) @\
Files which allow the use of LIBRARY module.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{LIB-MACROS   }

files: @\@t{LIB-OPS   }@t{LIB-OBJECTS   }@t{LIBRARY1   }@t{LIBRARY2   }@t{LIBRARY3   }@t{TEST-TOP-LIB   }@t{LIB-BUG   }@t{UNIX-LIBRARY1   }@t{LIB-MENUS   }@t{UNIX-LIB-MENUS   }

needed-modules: @\@t{REVIEW-FLAGS   }@t{WFF-PARSE   }@t{UNIFICATION   }

@End(Description)

@IndexOther(LOGIC-SCRIBE) @\
Defines output style SCRIBE for Math Logic Course. 
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{SCRIBE   }

files: @\@t{ML1-SCRIBE   }

needed-modules: @\@t{WFF-PRINT   }

@End(Description)

@IndexOther(MAINTAIN) @\
Defines useful commands for maintaining TPS.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{ARGTYP-MAINT   }

files: @\@t{MAINT   }@t{COMPL   }@t{LSPPCK-MAINT   }@t{MENUS   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(MATH-LOGIC-1) @\
Defines wffs and rules for Mathematical Logic I course.
It consists of:
@Begin(Description, Spread 0)
needed-modules: @\@t{MATH-LOGIC-1-RULES   }

@End(Description)

@IndexOther(MATH-LOGIC-1-RULES) @\
Defines rules for Mathematical Logic I course.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{ML1-PRIOR   }

files: @\@t{ML1-LOGIC0   }@t{ML1-LOGIC1   }@t{ML1-LOGIC2   }@t{ML1-LOGIC3A   }@t{ML1-LOGIC3B   }@t{ML1-LOGIC4   }

needed-modules: @\@t{OTLSUGGEST   }@t{MATH-LOGIC-1-WFFS   }

@End(Description)

@IndexOther(MATH-LOGIC-1-WFFS) @\
Defines wffs for Mathematical Logic I course.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ML1-CONST   }@t{ML1-ABBREV   }

needed-modules: @\@t{WFF-PARSE   }@t{MODE-ML   }

@End(Description)

@IndexOther(MATH-LOGIC-2) @\
Defines wffs, rules, and exercises for Mathematical Logic II
course.
It consists of:
@Begin(Description, Spread 0)
needed-modules: @\@t{MATH-LOGIC-2-RULES   }@t{MATH-LOGIC-2-EXERCISES   }

@End(Description)

@IndexOther(MATH-LOGIC-2-EXERCISES) @\
Exercises for Mathematical Logic II.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ML1-THEOREMS   }@t{ML2-THEOREMS   }

needed-modules: @\@t{MATH-LOGIC-2-WFFS   }@t{THEOREMS   }

@End(Description)

@IndexOther(MATH-LOGIC-2-RULES) @\
Defines rules for Mathematical Logic II course.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{ML2-PRIOR   }

files: @\@t{ML1-LOGIC0   }@t{ML2-LOGIC1A   }@t{ML2-LOGIC1B   }@t{ML2-LOGIC1C   }@t{ML2-LOGIC2A   }@t{ML2-LOGIC2B   }@t{ML1-LOGIC3A   }@t{ML1-LOGIC3B   }@t{ML2-LOGIC4A   }@t{ML2-LOGIC4B   }@t{ML2-LOGIC4C   }@t{ML2-LOGIC5A   }@t{ML2-LOGIC5B   }@t{ML2-LOGIC7A   }@t{ML2-LOGIC7B   }@t{ML2-LOGIC7C   }@t{ML2-HACKS   }

needed-modules: @\@t{OTLSUGGEST   }@t{MATH-LOGIC-2-WFFS   }@t{THEOREMS   }@t{REPLACE   }

@End(Description)

@IndexOther(MATH-LOGIC-2-WFFS) @\
Defines wffs for Mathematical Logic II course.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ML2-CONST   }@t{ML2-ABBREV   }@t{ML2-ABBREV2   }@t{ML2-AXIOMS   }@t{ML2-REPLACE   }

needed-modules: @\@t{WFF-PARSE   }@t{MODE-ML   }

@End(Description)

@IndexOther(MATING) @\
Defines mating search top level and basic mating operations.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{ETREES-FLAGS   }@t{ETREES-EXP-VARS   }@t{ETREES-SKOLEM   }@t{ETREES-LABELS   }@t{MATING-TOP   }@t{DATA-STRUCTURES   }@t{MATING-MACROS   }@t{MONITOR-MACROS   }@t{TEST-MACROS   }

files: @\@t{MATING-MOVE   }@t{MATING-MATEOPS   }@t{TIMING   }@t{MONITOR   }@t{TEST-TOP-TOP   }@t{TEST-TOP-SLISTS   }@t{TEST-TOP-SEARCH   }@t{MATE-MENUS   }@t{TEST-TOP-MENUS   }

needed-modules: @\@t{AUTO-BASIC   }

@End(Description)

@IndexOther(MATING-TRANSFORM) @\
Functions to reduce and modify spanning mating.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{MATING-TRANS   }@t{MATING-MERGE   }@t{MATING-MERGE2   }@t{MATING-MERGE-EQ   }

needed-modules: @\@t{MS88   }

@End(Description)

@IndexOther(METAWFFS) @\
Defines META-WFFS as used in the rules and outline modules.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{META-LABEL   }@t{META-VAR   }@t{META-VAR2   }

needed-modules: @\@t{WFF-PRINT   }

@End(Description)

@IndexOther(ML-ETR-TACTICS) @\
Defines tactics for translating between expansion proofs and
natural deduction proofs using math logic II rules.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ML-ETR-TACTICS-MAIN   }@t{ML-ETR-TACTICS-PLINE   }@t{ML-ETR-TACTICS-SLINE   }@t{ML-ETR-TACTICS-BOOK   }@t{ML-ETR-TACTICS-EQ   }@t{ML-ETR-TACTICS-NEG   }@t{ML-ETR-TACTICS-SYMSIMP   }@t{ML-ETR-TACTICS-SYMSIMP2   }@t{ML-NAT-ETR1   }@t{ML-NAT-ETR2   }@t{HX-NATREE-DUPLICATION   }@t{HX-NATREE-RULEP   }@t{HX-NATREE-AUX   }@t{HX-NATREE-CLEANUP   }@t{HX-NATREE-DEBUG   }

needed-modules: @\@t{ETR-NAT   }@t{MATH-LOGIC-2-RULES   }

@End(Description)

@IndexOther(ML-TACTICS) @\
Defines tactics for natural deduction proofs using 
math logic II rules.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ML-TACTICS-AUX   }@t{ML-TACTICS-PROP   }@t{ML-TACTICS-QUANT   }

needed-modules: @\@t{TACTICS   }@t{MATH-LOGIC-2-RULES   }

@End(Description)

@IndexOther(ML2-REWRITE) @\
Rewrite rules for ND proofs.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ML2-REWRITE   }

needed-modules: @\@t{OTLSUGGEST   }@t{MATH-LOGIC-2-WFFS   }@t{THEOREMS   }@t{REPLACE   }@t{RRULES   }

@End(Description)

@IndexOther(MODE-ML) @\
Defines mode ML, as other files in ML module have to be loaded
in that mode.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{ML-MODE   }

@End(Description)

@IndexOther(MS88) @\
The MS88 mating search module.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{MATING-AUX   }@t{CONNECTIONS   }@t{MATING   }@t{MATING-PATHS   }@t{UNIF-MAT   }@t{MATING-DIR   }@t{MATING-EVENTS   }@t{MATING-PROP   }@t{UNIF-FO   }@t{MATING-SUB   }

needed-modules: @\@t{EXPANSION-TREE   }@t{EVENTS   }@t{UNIFICATION   }@t{SKOLEMIZING   }@t{SAVE-TPS-WORK   }@t{PRIMITIVE-SUBST   }@t{OTLRULEP   }@t{TACTICS   }

@End(Description)

@IndexOther(MS89) @\
Files which define option trees and their use in searching
for an expansion proof.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{OPTION-TREE-MACROS   }

files: @\@t{OPTION-TREE   }@t{OPTION-TREE-AUX   }@t{OPTION-TREE-MATEOPS   }@t{OPTION-TREE-SEARCH   }

needed-modules: @\@t{MS88   }

@End(Description)

@IndexOther(MS90-3) @\
The mating search module MS90-3.  This search procedure
incorporates Issar's path-focused duplication, working on a single jform.
Note that the search will proceed in an automatic mode, and none of the 
interactive facilities described either in this top-level or 
elsewhere in TPS will work.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{MS90-3-NODE   }@t{MS90-3-DATA   }

files: @\@t{MS90-3-UNIF-SIMPL   }@t{MS90-3-PATH-ENUM   }@t{MS90-3-PATH-BKUP   }@t{MS90-3-UNIF-MATCH   }@t{MS90-3-UNIF-TREE   }@t{MS90-3-UNIF-FO   }@t{MS90-3-TOP   }@t{MS90-3-EXPAND-ETREE   }@t{MS90-3-EXP-JFORM   }@t{MIN-QUANT-ETREE   }@t{MS90-3-PROP   }@t{MS92-9-TOP   }@t{MS93-1   }

needed-modules: @\@t{EXPANSION-TREE   }@t{EVENTS   }@t{SKOLEMIZING   }

@End(Description)

@IndexOther(MS90-9) @\
Defines functions for integrating option trees with the search
procedure ms90-3.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{MS90-9   }

needed-modules: @\@t{MS89   }@t{MS90-3   }

@End(Description)

@IndexOther(MS91) @\
Files needed to run ms91-6 and ms91-7.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{MS91-BASIC   }@t{MS91-WEIGHTS   }

files: @\@t{MS91-ENUMERATE   }@t{MS91-SEARCH   }

needed-modules: @\@t{MS89   }@t{MS90-9   }

@End(Description)

@IndexOther(MS98) @\
The mating search module MS98.  This search procedure
implements component search with rewriting of equalities.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{MS98-MACROS   }

files: @\@t{MS98-WEIGHTS   }@t{MS98-TOP   }@t{MS98-UNIF   }@t{MS98-DAGIFY   }@t{MS98-JFORM   }@t{MS98-DUPS   }@t{MS98-PATHS   }@t{MS98-REWRITE   }@t{MS98-REWRITE2   }@t{MS98-PATHS2   }

needed-modules: @\@t{EXPANSION-TREE   }@t{SKOLEMIZING   }

@End(Description)

@IndexOther(MST) @\
The matinsgtree module.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{MTREE-OBLIGATION   }@t{MTREE-TOP   }@t{MTREE-PRINT   }@t{MTREE-UNIFICATION   }@t{MTREE-QUERY   }@t{MTREE-DUPLICATION   }@t{MTREE-MENUS   }

needed-modules: @\@t{MS88   }

@End(Description)

@IndexOther(OPS-OTLRULES) @\
Wffops needed by both rule and outline modules.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{WFFOP-OTL   }

needed-modules: @\@t{WFF-OPS1   }@t{WFF-OPS-ABB   }

@End(Description)

@IndexOther(OTLADVICE) @\
Defines the ADVICE facility for ETPS.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{OTL-ADVICE   }

needed-modules: @\@t{OTLSUGGEST   }@t{OTLCLEANUP   }

@End(Description)

@IndexOther(OTLCLEANUP) @\
Defines various forms of clean-up commands.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{OTL-CLEANUP   }

needed-modules: @\@t{OTLNL   }@t{READ-RULES   }

@End(Description)

@IndexOther(OTLGO) @\
Defines the GO facility.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{OTL-GO-MAC   }

files: @\@t{OTL-GO   }

needed-modules: @\@t{OTLSUGGEST   }

@End(Description)

@IndexOther(OTLHELP) @\
Functions to give nice help on rules.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{OTL-HELP   }

needed-modules: @\@t{READ-RULES   }@t{OTLRULES   }@t{OTLNL   }

@End(Description)

@IndexOther(OTLNL) @\
Creates and updates proof structure.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{OTL-MACROS   }@t{OTL-TYP   }

files: @\@t{LINENUMBER1   }@t{LINENUMBER2   }@t{OTLNL   }@t{PRTOTL   }@t{OTL-FILEOUT   }@t{OTL-REARRANGE   }@t{OTL-PRT   }@t{SAVEPROOF   }@t{PBRIEF   }

needed-modules: @\@t{WFF-PRINT   }@t{EVENT-SIGNAL   }

@End(Description)

@IndexOther(OTLRULEP) @\
Defines the interface between the tautology checker and
outline rules.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{OTL-RULEP   }

needed-modules: @\@t{OTLNL   }@t{TPS2-RULEP   }

@End(Description)

@IndexOther(OTLRULES) @\
Functions needed to execute rules generated by the rules module.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{OTL-CMDDEF   }

files: @\@t{OTL-AUX   }

needed-modules: @\@t{WFFMATCH   }@t{OPS-OTLRULES   }

@End(Description)

@IndexOther(OTLSCHEMA2) @\
Module to use theorems as lemmas in other proofs with type inference.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{OTL-SCHEMA2   }

needed-modules: @\@t{OTLNL   }@t{OTLRULES   }

@End(Description)

@IndexOther(OTLSCRIBE) @\
Printing proofs in style SCRIBE.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{OTL-SCRIBEOUT   }

needed-modules: @\@t{OTLNL   }@t{LOGIC-SCRIBE   }

@End(Description)

@IndexOther(OTLSUGGEST) @\
Defines commands connected with automatic help.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{OTL-SUGG-MAC   }

files: @\@t{OTL-SUGGEST   }

needed-modules: @\@t{OTLRULES   }@t{OTLNL   }

@End(Description)

@IndexOther(PRIMITIVE-SUBST) @\
Creates primitive-substitution tool.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{PRIM   }@t{PRIM-EDOPS   }@t{PR00   }@t{CONSTRAINTS   }

needed-modules: @\@t{AUTO-BASIC   }

@End(Description)

@IndexOther(READ-RULES) @\
Allows reading of rules for help or rules module.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{READ-RDEF-MAC   }

files: @\@t{READ-RULEDEFS   }

needed-modules: @\@t{WFF-PARSE   }

@End(Description)

@IndexOther(REPLACE) @\
Replacement of symbols by equivalent wffs.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{REPLACE   }

needed-modules: @\@t{WFF-EDITOR   }

@End(Description)

@IndexOther(REPORT) @\
The REPORT module.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{REPORT   }@t{REPORT-STATS   }@t{REPORT-INIT   }

needed-modules: @\@t{EVENT-SIGNAL   }@t{ETPS-EVENTS   }

@End(Description)

@IndexOther(REVIEW-FLAGS) @\
Defines the REVIEW top-level.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{REVIEW   }@t{REVIEW-MENUS   }@t{FLAG-DEPS   }

needed-modules: @\@t{TPS-HELP   }

@End(Description)

@IndexOther(RRULES) @\
Files defining rewrite rules.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{LIB-OBJECTS2   }

files: @\@t{EDREW   }

needed-modules: @\@t{LIBRARY   }@t{WFF-EDITOR   }

@End(Description)

@IndexOther(RULES) @\
The RULES module which generates inference rules from specifications.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{RULE-WFFOP   }@t{RULE-IDEF   }

files: @\@t{RULE-BUILD   }@t{RULE-BB   }@t{RULE-BUILD-DEFAULT   }@t{RULE-BUILD-CHECK   }@t{RULE-CMDS   }@t{RULE-BUILD-MATCH   }@t{RULE-BUILD-TAC   }

needed-modules: @\@t{WFFMATCH   }@t{OPS-OTLRULES   }@t{READ-RULES   }

@End(Description)

@IndexOther(S-EQN) @\
The REWRITING top level.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{S-EQN-MACROS   }

files: @\@t{S-EQN-TOP   }@t{S-EQN-REW   }@t{S-EQN-PRFW   }

@End(Description)

@IndexOther(SAIL-WFF) @\
Defines output style SAIL.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{SAIL   }

needed-modules: @\@t{WFF-PRINT   }

@End(Description)

@IndexOther(SAVE-TPS-WORK) @\
Defines commands for saving and restoring work.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{SAVE-WORK   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(SAVE-WFFS) @\
Allows writing of weak labels into files.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{WFFSAV-MAC   }

files: @\@t{WFFSAV   }

needed-modules: @\@t{WEAK-LABEL   }

@End(Description)

@IndexOther(SAVING-MODES) @\
Allows definition and saving of MODEs.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{MODSAV   }

needed-modules: @\@t{REVIEW-FLAGS   }

@End(Description)

@IndexOther(SCRIBE-WFF) @\
Defines output style SCRIBE.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{DFONT   }

needed-modules: @\@t{LOGIC-SCRIBE   }

@End(Description)

@IndexOther(SEMANTICS) @\
The module for code dealing with semantics of higher-order logic.
This includes the MODELS top level for experimenting with standard models
where the base types (hence all types) are a power of 2.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{SEMANTICS-MACROS   }

files: @\@t{MODELS   }

@End(Description)

@IndexOther(SKOLEMIZING) @\
Define different ways of skolemizing.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{WFF-SKOLEM-MAC   }

files: @\@t{WFF-SKOLEM   }

needed-modules: @\@t{WFF-EDITOR   }

@End(Description)

@IndexOther(TACTICS) @\
Defines functions needed to use tactics and tacticals.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{TACTICS-MACROS   }@t{TACTICALS-MACROS   }

files: @\@t{TACTICALS   }@t{TACTICS-AUX   }

needed-modules: @\@t{OTLNL   }@t{OTLRULEP   }

@End(Description)

@IndexOther(TACTICS-ND) @\
Defines higher-level tactics for natural deduction proofs using 
math logic II rules.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{MASTER-TACTIC   }

needed-modules: @\@t{ML-TACTICS   }

@End(Description)

@IndexOther(TEX-WFF) @\
Defines the TeX device style.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{DEFTEX   }

files: @\@t{TEXCHR   }

needed-modules: @\@t{WFF-PRINT   }

@End(Description)

@IndexOther(THEOREMS) @\
Defines ways of defining theorems, exercises, etc.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{THEOREM-MAC   }

needed-modules: @\@t{WFF-PARSE   }

@End(Description)

@IndexOther(TPS-HELP) @\
Defines HELP facility.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{MHELP   }@t{READ-HELP   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(TPS-MODULES) @\
Defines commands to deal with modules.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{PCK   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(TPS2-RULEP) @\
Defines edops to check satisfiability and validity.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{RULEP-MAC   }

files: @\@t{RULEP-EDOPS   }@t{NEWRULEP-TSTS   }

needed-modules: @\@t{JFORMS   }

@End(Description)

@IndexOther(TPSDEF) @\
The module allowing definitions of TPS-objects.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{CONTEXTS-CORE   }@t{CONTEXTS-AUTO   }@t{CONTEXTS-MAINT   }@t{CONTEXTS-TEACHER   }@t{CONTEXTS-ML   }@t{SUBJECTS-CORE   }@t{SUBJECTS-AUTO   }@t{SUBJECTS-TEACHER   }@t{SUBJECTS-MAINT   }

files: @\@t{TPSTOP   }@t{ARGTYP   }@t{FLAGGING   }@t{GENSTY   }

needed-modules: @\@t{BOOTSTRAP   }

@End(Description)

@IndexOther(UNIFICATION) @\
The higher-order unification module.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{UNIF-LAMBDA   }@t{UNIF-SIMPL   }@t{UNIF-MATCH   }@t{UNIF-TREE   }@t{UNIF-AUX   }@t{UNIF-SUBS   }@t{UNIF-MENUS   }

needed-modules: @\@t{AUTO-BASIC   }

@End(Description)

@IndexOther(UNIFICATION-INTERFACE) @\
Interface to higher-order unification module.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{UNIF-MACROS   }

files: @\@t{UNIF-TOP   }@t{UNIF-USER   }@t{UNIF-MENUS   }

needed-modules: @\@t{AUTO-BASIC   }@t{UNIFICATION   }@t{TEX-WFF   }

@End(Description)

@IndexOther(VPFORMS) @\
Editor operations associated with creating and displaying VPFORMS.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{VPFORMS-MACROS   }

files: @\@t{VPFORMS   }@t{VPFORMS-TEX   }

needed-modules: @\@t{WFF-EDITOR   }@t{JFORMS   }@t{TEX-WFF   }

@End(Description)

@IndexOther(WEAK-LABEL) @\
Defines the flavor WEAK of labels.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{WEAK-MAC   }

files: @\@t{WEAK   }

needed-modules: @\@t{WFF-EDITOR   }

@End(Description)

@IndexOther(WFF-EDITOR) @\
The kernel of the wff editor.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{EDTOP   }

files: @\@t{EDOPERA   }@t{EDMOVE   }@t{EDABB   }@t{EDPRT   }@t{EDILL   }@t{EDSUB   }@t{EDCHANGE   }@t{EDMBED   }@t{EDDEV   }@t{ED-MENUS   }

needed-modules: @\@t{WFF-OPS1   }@t{WFF-OPS2   }

@End(Description)

@IndexOther(WFF-OPS-ABB) @\
Defines basic recursive functions for instantiating definitions.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{WFFABB   }

needed-modules: @\@t{WFF-PARSE   }

@End(Description)

@IndexOther(WFF-OPS1) @\
Defines some basic operations on wffs in first-order logic.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{WFFSUB1   }@t{WFFNEG1   }@t{WFFEQU1   }@t{WFFCHANGE   }@t{WFFMBED   }

needed-modules: @\@t{WFF-PARSE   }

@End(Description)

@IndexOther(WFF-OPS2) @\
Defines some basic operations on wffs in higher-order logic.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{WFFLMBD-MACROS   }

files: @\@t{WFFABB2   }@t{WFFSUB2   }@t{WFFLMBD2   }@t{WFFEQU2   }

needed-modules: @\@t{WFF-OPS-ABB   }

@End(Description)

@IndexOther(WFF-PARSE) @\
Defines wff parsing functions common to all styles.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{WFFINM   }

files: @\@t{WFFIN   }@t{TPINF   }

needed-modules: @\@t{WFF-PRINT   }

@End(Description)

@IndexOther(WFF-PRINT) @\
Defines wffs-printing operations and commands.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{WFFOUT   }@t{STYLES   }@t{PRTPRP   }@t{FACES   }@t{INTERFACE-STYLE   }

files: @\@t{PRT   }@t{PPRINT   }@t{PRTOP   }@t{PRTCMD   }

needed-modules: @\@t{WFFS   }

@End(Description)

@IndexOther(WFFMATCH) @\
Defines objects dealing with matching as needed in the rules
and outline modules.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{MATCH-MACROS   }

files: @\@t{MATCH-WFFS   }

needed-modules: @\@t{METAWFFS   }

@End(Description)

@IndexOther(WFFS) @\
Defines wffs and some operations on them.
It consists of:
@Begin(Description, Spread 0)
macro-files: @\@t{WFFMACROS   }@t{WFFTYP   }@t{FLAVORING   }@t{WFFTST   }@t{WFFCAT   }@t{WFFMODES   }@t{WFFREC   }

files: @\@t{WFFPRIM   }@t{WFFMVE   }

needed-modules: @\@t{BARE   }

@End(Description)

@IndexOther(XWINDOWS) @\
Files which allow the use of the X window system.
It consists of:
@Begin(Description, Spread 0)
files: @\@t{XTERM   }@t{PRFW   }

needed-modules: @\@t{WFF-PARSE   }

@End(Description)
@End(Description)
@ChapterPh(Files)
(The category of TPS files.)
The internal name of this category is 
TPS-FILE.
A file can be defined using DEFFILE1.
Allowable properties are: @t{TPS-IMPORT}, @t{TPS-EXPORT}, @t{EXTENSION}, @t{PART-OF}, @t{MHELP}.

@Section(Lisp Source)

@Begin(Description)
@IndexFile(ARGTYP)@\ Part of the TPSDEF module. 
Contains the definitions of the ARGTYPE category plus 
some common argument types which don't belong anywhere else.

@IndexFile(ARGTYP-AUTO)@\ Part of the AUTO-BASIC module. 
Contains the definitions of types used in AUTO.

@IndexFile(ARGTYP-MAINT)@\ Part of the MAINTAIN module. 
Contains the definitions of types used in MAINT.

@IndexFile(BOOT0)@\ Part of the BOOTSTRAP module. 
Defines categories, mexprs and various other essential stuff.

@IndexFile(BOOT1)@\ Part of the BOOTSTRAP module. 
Defines modules, message handling and various other stuff.

@IndexFile(CFONT)@\ Part of the CONCEPT-WFF module. 
Defines characters for printing and parsing on Concepts.

@IndexFile(CNF)@\ Part of the LAMBDA-CALC module. 
Contains functions required to find conjunctive normal form of a wff.

@IndexFile(COLLECT-HELP)@\ Part of the AUTO-DOC module. 
Looks through a list of modules and writes the help-string in
them into file(s), sorted alphabetically.

@IndexFile(COMPL)@\ Part of the MAINTAIN module. 
Functions to do with compiling and loading code.

@IndexFile(CONCPT)@\ Part of the CONCEPT-BARE module. 
Contains functions for Concept terminal if neither the windows
nor the Pad are used.

@IndexFile(CONNECTIONS)@\ Part of the MS88 module. 
Functions to find connections in a ETREE.

@IndexFile(CONSTRAINTS)@\ Part of the PRIMITIVE-SUBST module. 
Functions for dealing with set constraints.

@IndexFile(CONSTY)@\ Part of the CONCEPT-WFF module. 
Defines CONCEPT and CONCEPT-S device styles.

@IndexFile(CONTEXTS-AUTO)@\ Part of the TPSDEF module. 
Defines contexts used in the AUTO package.

@IndexFile(CONTEXTS-CORE)@\ Part of the TPSDEF module. 
Defines contexts used in the CORE package.

@IndexFile(CONTEXTS-MAINT)@\ Part of the TPSDEF module. 
Defines contexts used in the MAINT package.

@IndexFile(CONTEXTS-ML)@\ Part of the TPSDEF module. 
Defines contexts used in the ML package.

@IndexFile(CONTEXTS-TEACHER)@\ Part of the TPSDEF module. 
Defines contexts used in the TEACHER package.

@IndexFile(DATA-STRUCTURES)@\ Part of the MATING module. 
Defines data structures associated with mating search MS88.

@IndexFile(DEFPCK)@\ Part of the BOOTSTRAP module. 
Defines packages as they are known to TPS3.

@IndexFile(DEFTEX)@\ Part of the TEX-WFF module. 
Creates TeX style printing.

@IndexFile(DFONT)@\ Part of the SCRIBE-WFF module. 
Defines SCRIBE style characters.

@IndexFile(DIY)@\ Part of the ETR-NAT module. 
Defines functions for calling matingsearch on current planned line.

@IndexFile(DOCDEF)@\ Part of the AUTO-DOC module. 
Macro file for automatic documentation.

@IndexFile(ED-MENUS)@\ Part of the WFF-EDITOR module. 
Define menus for editor top-level.

@IndexFile(EDABB)@\ Part of the WFF-EDITOR module. 
Contains editor operations for abbreviations.

@IndexFile(EDCHANGE)@\ Part of the WFF-EDITOR module. 
Contains editor operations to apply idempotent, commutative, 
associative laws, etc., to 'edwff'.

@IndexFile(EDDEV)@\ Part of the WFF-EDITOR module. 
Contains operations for quantifiers in the editor.

@IndexFile(EDILL)@\ Part of the WFF-EDITOR module. 
Contains editing operations for ill-formed formulae.

@IndexFile(EDLMBD)@\ Part of the LAMBDA-CALC module. 
Contains operations on typed lambda-calculus.

@IndexFile(EDMBED)@\ Part of the WFF-EDITOR module. 
Contains editor operations to embed the current gwff
within the scope of a connective or quantifier.

@IndexFile(EDMOVE)@\ Part of the WFF-EDITOR module. 
Defines editor moving operations from wff operations.

@IndexFile(EDOPERA)@\ Part of the WFF-EDITOR module. 
Contains miscellaneous editor operations.

@IndexFile(EDPRT)@\ Part of the WFF-EDITOR module. 
Contains wff printing operations for editor.

@IndexFile(EDREW)@\ Part of the RRULES module. 
Contains operations on rewrite rules.

@IndexFile(EDSUB)@\ Part of the WFF-EDITOR module. 
Contains editor substitution operations.

@IndexFile(EDTOP)@\ Part of the WFF-EDITOR module. 
Contents define editor top-level and ED command.

@IndexFile(ENVIRON)@\ Part of the ENVIRONMENT module. 
Defines the ENVIRONMENT help facility.

@IndexFile(ETPS-EVENTS)@\ Part of the ETPS-EVENTS module. 
Defines common events in ETPS.
They can be disabled or enabled in some common init file.

@IndexFile(ETR-NAT-MACROS)@\ Part of the ETR-NAT module. 
Functions and macros needed for translating from expansion
trees to natural deduction proofs.

@IndexFile(ETREES-DEF)@\ Part of the EXPANSION-TREE module. 
Expansion tree macro file.

@IndexFile(ETREES-EXP-VARS)@\ Part of the MATING module. 
Defines the flavor EXP-VAR for use in expansion trees.

@IndexFile(ETREES-FLAGS)@\ Part of the EXPANSION-TREE module. 
Macros and flags for expansion trees.

@IndexFile(ETREES-JFORMS)@\ Part of the EXPANSION-TREE module. 
Etree to Jform conversion commands.

@IndexFile(ETREES-LABELS)@\ Part of the MATING module. 
Defines flavors of expansion tree labels.

@IndexFile(ETREES-PRINT)@\ Part of the EXPANSION-TREE module. 
Functions for printing etrees and proofs.

@IndexFile(ETREES-RENUMBER)@\ Part of the EXPANSION-TREE module. 
Defines renumber-leaves and associated functions.

@IndexFile(ETREES-SKOLEM)@\ Part of the MATING module. 
Contains code concerned with skolem terms and skolemizing
in etrees.

@IndexFile(ETREES-WFFOPS)@\ Part of the EXPANSION-TREE module. 
Defines wffops used with expansion trees.

@IndexFile(ETREES-WFFOPS2)@\ Part of the EXPANSION-TREE module. 
Defines wffops used with expansion trees.

@IndexFile(EVENT-SIGNAL-UTILS)@\ Part of the EVENT-SIGNAL module. 
Defines the function which assigns a code for exercises completed
by the students.

@IndexFile(EVENTS)@\ Part of the EVENTS module. 
Defines functions handling events. Events currently only work
for the non path-focused duplication procedures ms88, ms89 and ms91-6.

@IndexFile(EVENTS-MAC)@\ Part of the EVENTS module. 
Defines category of EVENT and some flags etc.

@IndexFile(EXT-EXP-DAGS)@\ Part of the EXT-DAGS module. 
Extensional Expansion Dags

@IndexFile(EXT-EXP-DAGS-ND)@\ Part of the EXT-DAGS module. 
Translation from Extensional Expansion Dags to Natural Deduction

@IndexFile(EXT-EXP-OPEN-DAGS)@\ Part of the EXT-DAGS module. 
Open Extensional Expansion Dags (i.e., with expansion variables)

@IndexFile(EXT-MATE-TOP)@\ Part of the EXT-DAGS module. 
Top Level for Extensional Expansion Dags.  See Chad E. Brown's thesis.

@IndexFile(EXT-SEARCH)@\ Part of the EXT-DAGS module. 
File dealing with search using extensional expansion DAG's.

@IndexFile(EXT-SEQ)@\ Part of the EXT-DAGS module. 
Extensional Sequent Calculus.  See Chad E. Brown's thesis.

@IndexFile(EXT-SEQ-TACTICS)@\ Part of the EXT-DAGS module. 
Tactics for Extensional Sequent Calculus.

@IndexFile(EXT-SEQ-TOP)@\ Part of the EXT-DAGS module. 
Top Level for Extensional Sequent Calculus.  See Chad E. Brown's thesis.

@IndexFile(EXTERNAL)@\ Part of the EXTERNAL-SERVICES module. 
Defines functions for providing services for external programs
that wish to call tps.  In particular, this is used to communicate
with Omega.  This file is only supported for Allegro at the moment,
because it makes use of Allegro multiprocessing.

@IndexFile(FACES)@\ Part of the WFF-PRINT module. 
Allows definition of printing faces.

@IndexFile(FLAGGING)@\ Part of the TPSDEF module. 
Defines DEFFLAG and other flag-related TPS-objects.

@IndexFile(FLAVORING)@\ Part of the WFFS module. 
Contains macros and functions for flavors of labels.

@IndexFile(FTREE-SEQ)@\ Part of the EXPANSION-TREE module. 
Implementation of a Sequent Calculus corresponding to Ftrees

@IndexFile(FTREES)@\ Part of the EXPANSION-TREE module. 
Functional version of expansion trees.

@IndexFile(GENSTY)@\ Part of the TPSDEF module. 
Establishes styles, defines style GENERIC and
operations for style GENERIC which are independent of wffs.

@IndexFile(GR-MACROS)@\ Part of the GRADER module. 
Macro file for the grading package.

@IndexFile(GRADES-TOP)@\ Part of the GRADER-TOP module. 
Creates the grading package top-level.

@IndexFile(GRADES1)@\ Part of the GRADER module. 
Creates the grading package top-level.

@IndexFile(GRADES2)@\ Part of the GRADER module. 
Creates the grading package top-level.

@IndexFile(HTMLDOC)@\ Part of the AUTO-DOC module. 
Allows generation of HTML documentation.

@IndexFile(HX-NATREE-AUX)@\ Part of the ETR-NAT module. 
Auxiliary functions for translating from natural deduction
proofs to expansion proofs.

@IndexFile(HX-NATREE-RULEP)@\ Part of the ETR-NAT module. 
Functions for handling RULEP when translating natural deduction
proofs to expansion proofs.

@IndexFile(HX-NATREE-TOP)@\ Part of the ETR-NAT module. 
Functions for translating from natural deduction
proofs to expansion proofs.

@IndexFile(INTERFACE-STYLE)@\ Part of the WFF-PRINT module. 
Defines ISTYLE style printing and parsing.

@IndexFile(JFORMS)@\ Part of the JFORMS module. 
Jform-Wff conversion commands.

@IndexFile(JFORMS-DEFNS)@\ Part of the JFORMS module. 
Jform Macro file.

@IndexFile(JFORMS-EDOPS)@\ Part of the JFORMS module. 
Jform-Wff conversion commands.

@IndexFile(JFORMS-LABELS)@\ Part of the JFORMS module. 
Defines flavors of jform labels and jform printing commands.

@IndexFile(LATEXDOC)@\ Part of the AUTO-DOC module. 
Allows generation of LaTeX-able documentation.

@IndexFile(LEMMAS)@\ Part of the ETR-NAT module. 
Functions for dealing with lemmas.

@IndexFile(LIB-BUG)@\ Part of the LIBRARY module. 
Defines BUG-SAVE and BUG-RESTORE.

@IndexFile(LIB-MACROS)@\ Part of the LIBRARY module. 
Defines LIBRARY operations.

@IndexFile(LIB-MENUS)@\ Part of the LIBRARY module. 
Defines top-level menus for library.

@IndexFile(LIB-OBJECTS)@\ Part of the LIBRARY module. 
Functions to handle various TYPES of objects to be stored
    in the library.

@IndexFile(LIB-OBJECTS2)@\ Part of the RRULES module. 
Functions to handle rewrite rules, theories and other types
of objects not loaded into all versions of the library.

@IndexFile(LIB-OPS)@\ Part of the LIBRARY module. 
Defines LIBRARY operations.

@IndexFile(LIBRARY1)@\ Part of the LIBRARY module. 
Defines top-level for library.

@IndexFile(LIBRARY2)@\ Part of the LIBRARY module. 
Defines top-level for library.

@IndexFile(LIBRARY3)@\ Part of the LIBRARY module. 
Defines library keywords and best modes.

@IndexFile(LINENUMBER1)@\ Part of the OTLNL module. 
Defines functions which update the proof outline and provide
 defaults for line numbers.

@IndexFile(LINENUMBER2)@\ Part of the OTLNL module. 
Defines functions which update the proof outline and provide
 defaults for line numbers.

@IndexFile(LINEREADP)@\ Part of the BARE module. 
Functions for reading the input from the command line.

@IndexFile(LSPPCK-CORE)@\ Part of the BARE module. 
Functions in the CORE package to do with lisp packages.

@IndexFile(LSPPCK-MAINT)@\ Part of the BARE module. 
Functions in the MAINT package to do with lisp packages.

@IndexFile(MACSYS)@\ Part of the BARE module. 
Miscellaneous system functions for the Bare package.

@IndexFile(MAINT)@\ Part of the MAINTAIN module. 
Contains functions maintaining TPS.

@IndexFile(MASTER-TACTIC)@\ Part of the TACTICS-ND module. 
Defines monstro-tac and ui-herbrand-tac for doing a lot of work
   in natural deduction proofs. Defines go2-tac which is same as
   monstro-tac, except that it does not invoke ui-herbrand-tac.

@IndexFile(MATCH-MACROS)@\ Part of the WFFMATCH module. 
Defines macros and TPS objects to deal with matching.

@IndexFile(MATCH-WFFS)@\ Part of the WFFMATCH module. 
Defines the MATCH-BIND and SUBSTITUTE-BINDINGS functions used
by the rules package and the outline commands produced by it.

@IndexFile(MATE-MENUS)@\ Part of the MATING module. 
Defines matingstree toplevel menus.

@IndexFile(MATING)@\ Part of the MS88 module. 
Functions to modify matings.

@IndexFile(MATING-AUX)@\ Part of the MS88 module. 
Auxiliary functions used by the mating search package.

@IndexFile(MATING-DIR)@\ Part of the MS88 module. 
Functions to direct the mating search package.
    Applies to MS88.  In this version of the file, after backtracking
    TPS continues working on the same path, which prevents floundering.

@IndexFile(MATING-EVENTS)@\ Part of the MS88 module. 
Contains functions used in signalling events during mating search.

@IndexFile(MATING-MACROS)@\ Part of the MATING module. 
Contains macros needed for mating search.

@IndexFile(MATING-MATEOPS)@\ Part of the MS88 module. 
Interface to the mating search package.

@IndexFile(MATING-MERGE)@\ Part of the MATING-TRANSFORM module. 
Contains functions for merging two expansion trees.

@IndexFile(MATING-MERGE-EQ)@\ Part of the MATING-TRANSFORM module. 
Contains functions for removing Leibniz equalities from
expansion trees.

@IndexFile(MATING-MERGE2)@\ Part of the MATING-TRANSFORM module. 
Contains additional functions for merging expansion trees.

@IndexFile(MATING-MOVE)@\ Part of the MATING module. 
Defines mating-search moving operations from wff operations.

@IndexFile(MATING-PATHS)@\ Part of the MS88 module. 
Functions for finding mating paths.

@IndexFile(MATING-PROP)@\ Part of the MS88 module. 
MS88 mating search for propositional cases.

@IndexFile(MATING-SUB)@\ Part of the MS88 module. 
Functions to call mating search procedure MS88 on subtrees.

@IndexFile(MATING-TOP)@\ Part of the MATING module. 
Contents define mating-search top-level and MATE command.

@IndexFile(MATING-TRANS)@\ Part of the MATING-TRANSFORM module. 
Functions to check whether a mating is spanning.

@IndexFile(MENUS)@\ Part of the MAINTAIN module. 
Defines the top level menus for the user interface.
Sublevel menus and menu items are defined throughout the lisp files,
usually near the appropriate defflag or defmexpr.

@IndexFile(META-LABEL)@\ Part of the METAWFFS module. 
Defines some flavors of labels which are used inside the rules package.

@IndexFile(META-VAR)@\ Part of the METAWFFS module. 
Defines the META-VAR flavor for labels and some functions on them.

@IndexFile(META-VAR2)@\ Part of the METAWFFS module. 
Further functions on labels.

@IndexFile(MHELP)@\ Part of the TPS-HELP module. 
Defines general TPS help facilities: HELP and ENVIRONMENT.

@IndexFile(MIN-QUANT-ETREE)@\ Part of the MS90-3 module. 
Contains functions for minimizing quantifier scopes in
primsubs appearing in expansion trees. This allows the corresponding
instantiation terms in the ND proof to be in non-prenex form.  When
flag MIN-QUANT-ETREE is T, these functions are applied after searching
is done and before propositional proof checker starts.

@IndexFile(ML-ETR-TACTICS-BOOK)@\ Part of the ML-ETR-TACTICS module. 
Defines bookkeeping tactics as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs.

@IndexFile(ML-ETR-TACTICS-EQ)@\ Part of the ML-ETR-TACTICS module. 
Defines tactics for equalities as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs.

@IndexFile(ML-ETR-TACTICS-MAIN)@\ Part of the ML-ETR-TACTICS module. 
Defines main tactics for translating expansion proofs to
natural deduction proofs.

@IndexFile(ML-ETR-TACTICS-NEG)@\ Part of the ML-ETR-TACTICS module. 
Defines tactics for negations as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs.

@IndexFile(ML-ETR-TACTICS-PLINE)@\ Part of the ML-ETR-TACTICS module. 
Defines planned line tactics as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs.

@IndexFile(ML-ETR-TACTICS-SLINE)@\ Part of the ML-ETR-TACTICS module. 
Defines support line tactics as used in Pfenning's thesis
for translating expansion proofs to natural deduction proofs.

@IndexFile(ML-ETR-TACTICS-SYMSIMP)@\ Part of the ML-ETR-TACTICS module. 
Defines tactics for symmetric simplification.

@IndexFile(ML-ETR-TACTICS-SYMSIMP2)@\ Part of the ML-ETR-TACTICS module. 
Defines tactics for symmetric simplification.

@IndexFile(ML-MODE)@\ Part of the MODE-ML module. 
Defines ML mode for printing and parsing of wffs.

@IndexFile(ML-NAT-ETR1)@\ Part of the ML-ETR-TACTICS module. 
Functions and macros needed for translating from natural deduction
proofs to expansion proofs.

@IndexFile(ML-NAT-ETR2)@\ Part of the ML-ETR-TACTICS module. 
Functions for translating from natural deduction
proofs to expansion proofs.

@IndexFile(ML-TACTICS-AUX)@\ Part of the ML-TACTICS module. 
Auxiliary functions/tactics needed by ML tactics.

@IndexFile(ML-TACTICS-PROP)@\ Part of the ML-TACTICS module. 
Defines tactics for use with propositional rules.

@IndexFile(ML-TACTICS-QUANT)@\ Part of the ML-TACTICS module. 
Defines tactics for quantifier rules.

@IndexFile(ML1-SCRIBE)@\ Part of the LOGIC-SCRIBE module. 
Defines SCRIBE style characters for ml1.

@IndexFile(ML1-THEOREMS)@\ Part of the MATH-LOGIC-2-EXERCISES module. 
Defines theorems with numbers X21nn.

@IndexFile(ML2-ABBREV)@\ Part of the MATH-LOGIC-2-WFFS module. 
Abbreviations for Math Logic II.

@IndexFile(ML2-ABBREV2)@\ Part of the MATH-LOGIC-2-WFFS module. 
Abbreviations for Math Logic II.

@IndexFile(ML2-AXIOMS)@\ Part of the MATH-LOGIC-2-RULES module. 
Axioms REFL=, SYM=, DESCR, EXT, etc.

@IndexFile(ML2-CONST)@\ Part of the MATH-LOGIC-2-WFFS module. 
Defines logical constants.

@IndexFile(ML2-PRIOR)@\ Part of the MATH-LOGIC-2-RULES module. 
Flag settings for Mathematical Logic II.

@IndexFile(ML2-REPLACE)@\ Part of the MATH-LOGIC-2-RULES module. 
Defines wffop used by ERP and IRP rules.

@IndexFile(ML2-THEOREMS)@\ Part of the MATH-LOGIC-2-EXERCISES module. 
Defines theorems x5200 to x6201.

@IndexFile(MODELS)@\ Part of the SEMANTICS module. 
Top level for computing with standard models where the base types are powers of 2.
Usually the base types have 2 elements: 0 and 1.
In particular at type O, 0 means false (F) and 1 means true (T).
The cardinality of every type is a power of 2 and the elements
are 0,1,...,n where n is (2^k)-1 for some k.

Functions in a type (AB) are coded as integers.
Suppose the elements in type A are 0,...,n and in type B are 0,...,m.
Suppose f is a function from B to A.  Then f is determined by
its values f(0),...,f(m) and each value f(i) is between 0 and n.
The string 'f(m)...f(0)' represents a number between 0 and (n+1)^(m+1)
written in base n+1.  This number is the code for the function f in type (AB).

@IndexFile(MONITOR)@\ Part of the MATING module. 
Defines the monitor functions.

@IndexFile(MONITOR-MACROS)@\ Part of the MATING module. 
Defines the defmonitor command and all related functions.

@IndexFile(MS04-SEARCH)@\ Part of the EXT-DAGS module. 
File dealing with MS04-2 search using extensional expansion DAG's.

@IndexFile(MS90-3-DATA)@\ Part of the MS90-3 module. 
Containing the data structure used for CONNECTION
and some macros. It is a good idea to put more data structures
into the code for ms90-3 search process from now on.

@IndexFile(MS90-3-EXP-JFORM)@\ Part of the MS90-3 module. 
Functions for manipulating jforms in MS90-3.

@IndexFile(MS90-3-EXPAND-ETREE)@\ Part of the MS90-3 module. 
Contains functions for converting from a jform created by
ms90-3 to an expansion tree.

@IndexFile(MS90-3-NODE)@\ Part of the MS90-3 module. 
Definitions, Functions, etc., needed by unification, mating search,
	 etc. Version ms90-3. Implementation of Path-focused duplication.

@IndexFile(MS90-3-PATH-BKUP)@\ Part of the MS90-3 module. 
Functions for locating an earlier path when backtracking in
Path-focused duplication.

@IndexFile(MS90-3-PATH-ENUM)@\ Part of the MS90-3 module. 
Path enumerator used in the implementation of Path-focused
duplication.

@IndexFile(MS90-3-PROP)@\ Part of the MS90-3 module. 
More functions for MS90-3.

@IndexFile(MS90-3-TOP)@\ Part of the MS90-3 module. 
Main functions implementing Path-focused duplication.
Detailed description in the file.

@IndexFile(MS90-3-UNIF-FO)@\ Part of the MS90-3 module. 
First-order unification functions needed in the
implementation of Path-focused duplication.

@IndexFile(MS90-3-UNIF-MATCH)@\ Part of the MS90-3 module. 
Implementation of Huet's Match routine for Path-focused
duplication.

@IndexFile(MS90-3-UNIF-SIMPL)@\ Part of the MS90-3 module. 
Implementation of Huet's Simpl routine for Path-focused
duplication.

@IndexFile(MS90-3-UNIF-TREE)@\ Part of the MS90-3 module. 
Implementation of Huet's unification algorithm for Path-focused
duplication.

@IndexFile(MS90-9)@\ Part of the MS90-9 module. 
Contains mateops for using option tree search procedure with
path-focused duplication.

@IndexFile(MS91-BASIC)@\ Part of the MS91 module. 
Basic data structures used in ms91-6 and ms91-7 search procedures.

@IndexFile(MS91-ENUMERATE)@\ Part of the MS91 module. 
Functions dealing with enumeration of option-sets for use in
mating-search procedures MS91-6 and MS91-7.

@IndexFile(MS91-SEARCH)@\ Part of the MS91 module. 
Functions dealing with overall structure of MS91-6 and MS91-7
mating-search procedures.

@IndexFile(MS91-WEIGHTS)@\ Part of the MS91 module. 
Functions and flags for computing weights in MS91-6 and MS91-7
mating-search procedures.

@IndexFile(MS92-9-TOP)@\ Part of the MS90-3 module. 
Definitions, functions, etc., needed by ms92-9 and not already
          provided by ms90-3.

@IndexFile(MS93-1)@\ Part of the MS90-3 module. 
Definitions, functions, etc., needed by ms93-1 and not already
          provided by ms92-9. This is basically an extension to MS92-9, which 
          is why it's in the package MS90-3.

@IndexFile(MS98-DAGIFY)@\ Part of the MS98 module. 
The functions to handle directed acyclic graphs for MS98-1

@IndexFile(MS98-DUPS)@\ Part of the MS98 module. 
Miscellaneous functions to handle duplication and primitive substitution for MS98-1

@IndexFile(MS98-JFORM)@\ Part of the MS98 module. 
Miscellaneous functions to handle jforms for MS98-1

@IndexFile(MS98-MACROS)@\ Part of the MS98 module. 
Defines the global variables, flags and structures for MS98-1

@IndexFile(MS98-PATHS)@\ Part of the MS98 module. 
Functions that implement the completeness checker in MS98-1

@IndexFile(MS98-PATHS2)@\ Part of the MS98 module. 
Functions that implement the minimality checker in MS98-1

@IndexFile(MS98-REWRITE)@\ Part of the MS98 module. 
Functions that implement rewriting of equalities in MS98-1

@IndexFile(MS98-REWRITE2)@\ Part of the MS98 module. 
More functions that implement rewriting of equalities in MS98-1

@IndexFile(MS98-TOP)@\ Part of the MS98 module. 
The main functions for MS98-1

@IndexFile(MS98-UNIF)@\ Part of the MS98 module. 
The unification functions for MS98-1

@IndexFile(MS98-WEIGHTS)@\ Part of the MS98 module. 
The functions to handle weightings and numbered lists for MS98-1

@IndexFile(MTREE-DATASTRUCTURE)@\ Part of the EXPANSION-TREE module. 
Defines data structures used by matingstree.

@IndexFile(MTREE-DUPLICATION)@\ Part of the MST module. 
Defines quantifier duplication functions for matingstree.

@IndexFile(MTREE-MENUS)@\ Part of the MST module. 
Defines matingstree toplevel menus.

@IndexFile(MTREE-OBLIGATION)@\ Part of the MST module. 
Defines obligations, as used by matingstree.

@IndexFile(MTREE-PRINT)@\ Part of the MST module. 
Defines printing functions for matingstree.

@IndexFile(MTREE-QUERY)@\ Part of the MST module. 
Defines automatic search functions for matingstree.

@IndexFile(MTREE-TOP)@\ Part of the MST module. 
Defines matingstree toplevel.

@IndexFile(MTREE-UNIFICATION)@\ Part of the MST module. 
Defines unification as used in matingstree.

@IndexFile(NAT-ETR)@\ Part of the ETR-NAT module. 
Functions for translating from natural deduction
proofs to expansion proofs.

@IndexFile(NEWRULEP-TSTS)@\ Part of the TPS2-RULEP module. 
Functions testing for validity and satisfiability.

@IndexFile(NODE)@\ Part of the AUTO-BASIC module. 
Definitions, Functions, etc., needed by unification, mating search,
	 etc.

@IndexFile(OMDOC)@\ Part of the OMDOC module. 
Functions for OMDoc output.

@IndexFile(OPTION-TREE)@\ Part of the MS89 module. 
Contains code implementing option trees.

@IndexFile(OPTION-TREE-AUX)@\ Part of the MS89 module. 
Contains auxiliary code for dealing with option trees.

@IndexFile(OPTION-TREE-MACROS)@\ Part of the MS89 module. 
Defines option trees.

@IndexFile(OPTION-TREE-MATEOPS)@\ Part of the MS89 module. 
Contains mateops for using option tree search procedure.

@IndexFile(OPTION-TREE-SEARCH)@\ Part of the MS89 module. 
Contains code implementing search procedure for option trees.
    Applies to MS89.  In this version of the file, after backtracking
    TPS continues working on the same path, which prevents floundering.

@IndexFile(ORDER-COMPONENTS)@\ Part of the MS88 module. 
The file order-components is used to rearrange the current 
jform with the help of some heuristics.

@IndexFile(OTL-ADVICE)@\ Part of the OTLADVICE module. 
Defines commands giving advice to the student.

@IndexFile(OTL-AUX)@\ Part of the OTLRULES module. 
Auxiliary functions needed by the commands created by the rules package.

@IndexFile(OTL-CLEANUP)@\ Part of the OTLCLEANUP module. 
Defines cleanup commands.

@IndexFile(OTL-CMDDEF)@\ Part of the OTLRULES module. 
Defines functions and macros which are used inside the final rule
command definitions.

@IndexFile(OTL-FILEOUT)@\ Part of the OTLNL module. 
Contains functions which allow writing into files inside the
outline package.

@IndexFile(OTL-GO)@\ Part of the OTLGO module. 
Defines categories etc. to allow automatic suggestion of inference
rules without the benefit of an expansion tree.

@IndexFile(OTL-GO-MAC)@\ Part of the OTLGO module. 
Defines flags etc for GO.

@IndexFile(OTL-HELP)@\ Part of the OTLHELP module. 
Defines help function for rule definitions.

@IndexFile(OTL-MACROS)@\ Part of the OTLNL module. 
Macro file for the outline package.

@IndexFile(OTL-PRT)@\ Part of the OTLNL module. 
Commands for looking at parts of the proof, and wffs in proof.

@IndexFile(OTL-REARRANGE)@\ Part of the OTLNL module. 
Defines the functions for rearranging the proof outline.

@IndexFile(OTL-RULEP)@\ Part of the OTLRULEP module. 
Things useful for RULEP, including the RULEP mainfns,
defaultfn, and enterfn.

@IndexFile(OTL-SCHEMA2)@\ Part of the OTLSCHEMA2 module. 
Defines a way of using theorem schemas without restrictions as lemmas.

@IndexFile(OTL-SCRIBEOUT)@\ Part of the OTLSCRIBE module. 
Contains functions which allow writing into files inside the
outline package.

@IndexFile(OTL-SUGG-MAC)@\ Part of the OTLSUGGEST module. 
Defines category of suggested rule.

@IndexFile(OTL-SUGGEST)@\ Part of the OTLSUGGEST module. 
Defines categories etc. to allow automatic suggestion of inference
rules without the benefit of an expansion tree.

@IndexFile(OTL-TYP)@\ Part of the OTLNL module. 
Defines argument types for the outline package.

@IndexFile(OTLNL)@\ Part of the OTLNL module. 
Defines the functions for maintaining proof outline.

@IndexFile(PBRIEF)@\ Part of the OTLNL module. 
Defines the commands PBRIEF, EXPLAIN, BUILD-PROOF-HIERARCHY and PRINT-PROOF-STRUCTURE

@IndexFile(PCK)@\ Part of the TPS-MODULES module. 
Contains commands for loading modules.

@IndexFile(PLURALS)@\ Part of the AUTO-DOC module. 
A file of language hacks in lieu of Common Lisp.

@IndexFile(PPRINT)@\ Part of the WFF-PRINT module. 
Contents print using PPLIST generated by pretty-printing function.

@IndexFile(PR00)@\ Part of the PRIMITIVE-SUBST module. 
PR00 set substitution functions

@IndexFile(PRFW)@\ Part of the XWINDOWS module. 
Defines proofwindows for use by those using xwindows.

@IndexFile(PRIM)@\ Part of the PRIMITIVE-SUBST module. 
Basic primitive-substitution functions.

@IndexFile(PRIM-EDOPS)@\ Part of the PRIMITIVE-SUBST module. 
Interface to the primitive substitution package.

@IndexFile(PRT)@\ Part of the WFF-PRINT module. 
Contains functions for printing and pretty-printing wffs.

@IndexFile(PRTCMD)@\ Part of the WFF-PRINT module. 
Contains printing commands and operations.

@IndexFile(PRTOP)@\ Part of the WFF-PRINT module. 
Contains wff operations for printing only.

@IndexFile(PRTOTL)@\ Part of the OTLNL module. 
Defines functions associated with printing of lines.

@IndexFile(PRTPRP)@\ Part of the WFF-PRINT module. 
Defines basic printing properties and PRINTPROP category.

@IndexFile(READ-HELP)@\ Part of the TPS-HELP module. 
Looks through a list of packages and writes the help-string in
them into file(s), sorted alphabetically.

@IndexFile(READ-RDEF-MAC)@\ Part of the READ-RULES module. 
Defines macros necessary to digest rule definitions.

@IndexFile(READ-RULEDEFS)@\ Part of the READ-RULES module. 
Defines macros and functions necessary to digest rule definitions.

@IndexFile(REPLACE)@\ Part of the REPLACE module. 
Functions for replacing one symbol or wff with another.

@IndexFile(REVIEW)@\ Part of the REVIEW-FLAGS module. 
Defines top-level for reviewing flags.

@IndexFile(REVIEW-MENUS)@\ Part of the REVIEW-FLAGS module. 
Defines menus for review top-level.

@IndexFile(RULE-BB)@\ Part of the RULES module. 
Defines functions which build the function which actually construct
the lines for the outline before they are inserted.

@IndexFile(RULE-BUILD)@\ Part of the RULES module. 
Contains functions building the rule command from the intermediate
rule definition.

@IndexFile(RULE-BUILD-CHECK)@\ Part of the RULES module. 
Defines the functions which build the definition of the functions
<rule>-legal-hyps and <rule>-legal-wffs.

@IndexFile(RULE-BUILD-DEFAULT)@\ Part of the RULES module. 
Defines the functions which build the definition of the function
<rule>-defaults.

@IndexFile(RULE-BUILD-MATCH)@\ Part of the RULES module. 
Defines the functions which build the definition of the function
<rule>-match

@IndexFile(RULE-BUILD-TAC)@\ Part of the RULES module. 
Defines the functions which build the definition of the function
<rule>-match

@IndexFile(RULE-CMDS)@\ Part of the RULES module. 
Defines some commands, argument types etc. which are useful when
running the RULES module.

@IndexFile(RULE-IDEF)@\ Part of the RULES module. 
Defines the category of intermediate rule definitions (IRULEDEF)
and some functions on them.

@IndexFile(RULE-WFFOP)@\ Part of the RULES module. 
Defines some argument types and wffops useful for the rules package

@IndexFile(RULEP-EDOPS)@\ Part of the TPS2-RULEP module. 
Defines WFFOPs and EDOPs for validity testing.

@IndexFile(RULEP-MAC)@\ Part of the TPS2-RULEP module. 
Flags for deciding how RULEP works.

@IndexFile(S-EQN-REW)@\ Part of the S-EQN module. 
Additional rewriting facilities used by S-EQN.

@IndexFile(S-EQN-TOP)@\ Part of the S-EQN module. 
Commands for the REWRITING top-level.

@IndexFile(S-PRFW)@\ Part of the S-EQN module. 
Proofwindow support for the REWRITING toplevel.

@IndexFile(SAIL)@\ Part of the SAIL-WFF module. 
Defines SAIL style printing and parsing.

@IndexFile(SAVE-WORK)@\ Part of the SAVE-TPS-WORK module. 
Contains commands for saving and restoring work.

@IndexFile(SAVEPROOF)@\ Part of the OTLNL module. 
Functions for saving and restoring natural deduction proofs.

@IndexFile(SCRDOC)@\ Part of the AUTO-DOC module. 
Allows generation of SCRIBE-able documentation.

@IndexFile(SCRIBE)@\ Part of the LOGIC-SCRIBE module. 
Establishes SCRIBE style printing.

@IndexFile(STYLES)@\ Part of the WFF-PRINT module. 
Defines GENERIC-STRING style and some functions used
for printing wffs in GENERIC and GENERIC-STRING styles.

@IndexFile(SUBJECTS-AUTO)@\ Part of the TPSDEF module. 
Defines subjects used in the AUTO package.

@IndexFile(SUBJECTS-CORE)@\ Part of the TPSDEF module. 
Defines subjects used in the CORE package.

@IndexFile(SUBJECTS-MAINT)@\ Part of the TPSDEF module. 
Defines subjects used in the MAINT package.

@IndexFile(SUBJECTS-TEACHER)@\ Part of the TPSDEF module. 
Defines subjects used in the GRADER package.

@IndexFile(SYMSIMP)@\ Part of the ETR-NAT module. 
Defines functions used for symmetric simplification.

@IndexFile(SYMSIMP2)@\ Part of the ETR-NAT module. 
Defines additional functions used for symmetric simplification.

@IndexFile(TACTICALS)@\ Part of the TACTICS module. 
Defines standard tacticals.

@IndexFile(TACTICALS-MACROS)@\ Part of the TACTICS module. 
Functions used by tacticals.

@IndexFile(TACTICS-AUX)@\ Part of the TACTICS module. 
Auxiliary tactics.

@IndexFile(TACTICS-MACROS)@\ Part of the TACTICS module. 
Functions and macros needed by tactics and tacticals.

@IndexFile(TEST-MACROS)@\ Part of the MATING module. 
Defines structures and flags for test-top.

@IndexFile(TEST-TOP-LIB)@\ Part of the LIBRARY module. 
Defines functions to do with searchlists and modes for test-top.

@IndexFile(TEST-TOP-MENUS)@\ Part of the MATING module. 
Defines menus for test-top.

@IndexFile(TEST-TOP-SEARCH)@\ Part of the MATING module. 
Defines the search procedures for test-top.

@IndexFile(TEST-TOP-SLISTS)@\ Part of the MATING module. 
Defines functions to do with searchlists for test-top.

@IndexFile(TEST-TOP-TOP)@\ Part of the MATING module. 
Defines top-level for test-top.

@IndexFile(TEXCHR)@\ Part of the TEX-WFF module. 
Defines some TeX characters.

@IndexFile(THEOREM-MAC)@\ Part of the THEOREMS module. 
Define defines the category of THEOREM with its various attributes.

@IndexFile(TIMING)@\ Part of the MS88 module. 
Timing stuff to the mating search package.

@IndexFile(TOP)@\ Part of the BARE module. 
Defines default top-level.

@IndexFile(TOPS20)@\ Part of the BARE module. 
System-dependent and implementation-dependent functions.

@IndexFile(TPINF)@\ Part of the WFF-PARSE module. 
Contents allow type inferencing.

@IndexFile(TPS3-ERROR)@\ Part of the ETPS-EVENTS module. 
Error-handling routines for various implementations of lisp.

@IndexFile(TPS3-SAVE)@\ Part of the BARE module. 
Routines for saving core image, and list of expert users.

@IndexFile(TPSTOP)@\ Part of the TPSDEF module. 
Defines the command decoder for all command top levels like REVIEW, 
or the absolute top level.

@IndexFile(UNIF-AUX)@\ Part of the UNIFICATION module. 
Functions used by unification routines.

@IndexFile(UNIF-FO)@\ Part of the MS88 module. 
First-order unification.

@IndexFile(UNIF-LAMBDA)@\ Part of the UNIFICATION module. 
Unification functions.

@IndexFile(UNIF-MACROS)@\ Part of the UNIFICATION-INTERFACE module. 
Contents define unifop category.

@IndexFile(UNIF-MAT)@\ Part of the MS88 module. 
Interface between mating search and unification.

@IndexFile(UNIF-MATCH)@\ Part of the UNIFICATION module. 
Unification functions.

@IndexFile(UNIF-MENUS)@\ Part of the UNIFICATION-INTERFACE module. 
Menus for unification top-level.

@IndexFile(UNIF-SIMPL)@\ Part of the UNIFICATION module. 
Unification functions.

@IndexFile(UNIF-SUBS)@\ Part of the UNIFICATION module. 
Unification functions.

@IndexFile(UNIF-TOP)@\ Part of the UNIFICATION-INTERFACE module. 
Contents define unification top-level.

@IndexFile(UNIF-TREE)@\ Part of the UNIFICATION module. 
Unification functions.

@IndexFile(UNIF-USER)@\ Part of the UNIFICATION-INTERFACE module. 
Contents define unification top-level.

@IndexFile(UNIX-LIB-MENUS)@\ Part of the LIBRARY module. 
Defines top-level menus for unix-style library interface.

@IndexFile(UNIX-LIBRARY1)@\ Part of the LIBRARY module. 
Defines top-level for unix-style library interface.

@IndexFile(VPFORMS)@\ Part of the VPFORMS module. 
Printing vertical path diagram commands.

@IndexFile(VPFORMS-MACROS)@\ Part of the VPFORMS module. 
VPFORM Macro file.

@IndexFile(VPFORMS-TEX)@\ Part of the VPFORMS module. 
Printing vertical path diagram commands to be processes by TeX. 

@IndexFile(WEAK)@\ Part of the WEAK-LABEL module. 
Defines the WEAK label for wffs.

@IndexFile(WEAK-MAC)@\ Part of the WEAK-LABEL module. 
Flags and labels of weak flavor.

@IndexFile(WEAK-MAC-AUTO)@\ Part of the JFORMS module. 
Jform-Wff conversion commands.

@IndexFile(WFF-SKOLEM)@\ Part of the SKOLEMIZING module. 
Wffops and Edops for Skolemizing a la S1 and S3.

@IndexFile(WFF-SKOLEM-MAC)@\ Part of the SKOLEMIZING module. 
Flags and Macros for Skolemizing.

@IndexFile(WFFABB)@\ Part of the WFF-OPS-ABB module. 
Defines basic recursive wffs for definitions.

@IndexFile(WFFABB2)@\ Part of the WFF-OPS2 module. 
Contents pertain to abbreviations of wffs.

@IndexFile(WFFCAT)@\ Part of the WFFS module. 
Defines categories of objects in wffs like binders, abbreviations
etc., without defining any objects in those categories.

@IndexFile(WFFCHANGE)@\ Part of the WFF-OPS1 module. 
Contains operation to apply idempotent, commutative, 
associative laws, etc., to 'edwff'.

@IndexFile(WFFEQU1)@\ Part of the WFF-OPS1 module. 
Contains tests for equality between wffs.

@IndexFile(WFFEQU2)@\ Part of the WFF-OPS2 module. 
Contains tests for equality between wffs.

@IndexFile(WFFIN)@\ Part of the WFF-PARSE module. 
Contains the parsing function for GENERIC terminal input.

@IndexFile(WFFINM)@\ Part of the WFF-PRINT module. 
Contains flags and macros for wff parsing.

@IndexFile(WFFLMBD-MACROS)@\ Part of the WFF-OPS2 module. 
Contains macros for lambda operations.

@IndexFile(WFFLMBD2)@\ Part of the WFF-OPS2 module. 
Contains lambda operations.

@IndexFile(WFFMACROS)@\ Part of the WFFS module. 
Contains macros for wffs.

@IndexFile(WFFMBED)@\ Part of the WFF-OPS1 module. 
Contains editor operations to embed the current gwff
within the scope of a connective or quantifier.

@IndexFile(WFFMODES)@\ Part of the WFFS module. 
Defines some modes for printing and parsing of wffs.

@IndexFile(WFFMVE)@\ Part of the WFFS module. 
Contents allow movement within wffs.

@IndexFile(WFFNEG1)@\ Part of the WFF-OPS1 module. 
Contains operations changing scope of negations.

@IndexFile(WFFOP-OTL)@\ Part of the OPS-OTLRULES module. 
Defines wffops, argument types etc. for use with commands
generated by the rules package.

@IndexFile(WFFOUT)@\ Part of the WFF-PRINT module. 
Contains flags and macros for printing wffs.

@IndexFile(WFFPRIM)@\ Part of the WFFS module. 
Contains basic stuff for wffs.

@IndexFile(WFFREC)@\ Part of the WFFS module. 
Defines some recursion macros for operations on wffs.

@IndexFile(WFFSAV)@\ Part of the SAVE-WFFS module. 
Commands and functions for saving wffs in files.

@IndexFile(WFFSAV-MAC)@\ Part of the SAVE-WFFS module. 
Categories, argument types etc. for saving wffs in files.

@IndexFile(WFFSUB1)@\ Part of the WFF-OPS1 module. 
Contains substitution commands for wffs without lambda binders.

@IndexFile(WFFSUB2)@\ Part of the WFF-OPS2 module. 
Contains substitution commands for wffs without lambda binders.

@IndexFile(WFFTST)@\ Part of the WFFS module. 
Contains tests on wffs.

@IndexFile(WFFTYP)@\ Part of the WFFS module. 
Contents pertaining to types of wffs.

@IndexFile(XTERM)@\ Part of the XWINDOWS module. 
Defines XTERM style printing and parsing.@End(Description)
@ChapterPh(Top Levels)
The internal name of this category is 
TOPLEVEL.
A top level can be defined using DEFTOPLEVEL.
Allowable properties are: @t{TOP-PROMPT-FN}, @t{COMMAND-INTERPRETER}, @t{PRINT-*}, @t{TOP-LEVEL-CATEGORY}, @t{TOP-LEVEL-CTREE}, @t{TOP-CMD-INTERPRET}, @t{TOP-CMD-DECODE}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(CMD-TOP)
@\
The initial command top level of TPS.
@\Its prompt is <@i(n)> and it takes top-level commands as input. 

@IndexOther(ED-TOP)
@\
The top level of the formula editor.
@\Its prompt is <0:Ed@i(n)> and it takes editor commands as input. 

@IndexOther(EXT-MATE-TOP)
@\
The top level for building and manipulating Extensional Expansion Dags.
@\Its prompt is <EXT-MATE@i(n)> and it takes extensional expansion dag commands as input. 

@IndexOther(EXT-SEQ-TOP)
@\
The top level for building and manipulating Extensional Sequent Derivations.
@\Its prompt is <EXT-SEQ@i(n)> and it takes extensional sequent commands as input. 

@IndexOther(LIBRARY-TOP)
@\
The top level of LIBRARY.
@\Its prompt is <lib@i(n)> and it takes library commands as input. 

@IndexOther(MODELS-TOP)
@\
The top level of MODELS.
@\Its prompt is <MODELS:@i(n)> and it takes models commands as input. 

@IndexOther(MTREE-TOP)
@\
The top level of MTREE.
@\Its prompt is <0:Mtree:@i(n)> and it takes matingstree commands as input. 

@IndexOther(PRFW-TOP)
@\
The command top level of TPS, supplemented by proofwindow output.
@\Its prompt is <PRFW@i(n)> and it takes top-level commands as input. 

@IndexOther(REVIEW-TOP)
@\
The top level of REVIEW.
@\Its prompt is <R@i(n)> and it takes review commands as input. 

@IndexOther(S-PRFW-TOP)
@\
The REWRITING top level, supplemented by proofwindow output.
@\Its prompt is <REW-PRFW@i(n)> and it takes rewriting commands as input. 

@IndexOther(TEST-TOP)
@\
The TEST-TOP top level.
@\Its prompt is <test@i(n)> and it takes test-top commands as input. 

@IndexOther(UNIX-LIBRARY-TOP)
@\
The top level of for accessing the TPS Library using a Unix-style Interface.

The value of the flag CLASS-SCHEME determines what classification scheme
is used to determine the virtual directory structure.

If the flag UNIXLIB-SHOWPATH is T, the prompt will be
<<CLASSSCHEME>:<PATH TO CLASS><num>>

If the flag UNIXLIB-SHOWPATH is NIL, the prompt will be
<LIB:<CLASS><num>>

See Also: UNIXLIB, PSCHEMES, CLASS-SCHEME, UNIXLIB-SHOWPATH, CD, LS, PWD,
LN, RM, MKDIR, FETCH, SHOW, PINTERSECT, PINTERSECT*
@\Its prompt is <LIB:CLASS:@i(n)> and it takes library command using a unix style interfaces as input. @End(Description)

@Section(Mating search)

@Begin(Description)
@IndexOther(MATE-TOP)
@\
The top level of mating search.
@\Its prompt is <0:Mate@i(n)> and it takes mating-search commands as input. @End(Description)

@Section(Unification)

@Begin(Description)
@IndexOther(UNIF-TOP)
@\
The top level of unification search.
@\Its prompt is <Unif@i(n)> and it takes unification commands as input. @End(Description)

@Section(Grader)

@Begin(Description)
@IndexOther(GRADE-TOP)
@\
The top level of the GRADING PACKAGE.
@\Its prompt is <Gr@i(n)> and it takes Grader Commands as input. @End(Description)

@Section(Unclassified)

@Begin(Description)
@IndexOther(S-EQN-TOP)
@\
The REWRITING top level.
@\Its prompt is <REWRITING@i(n)> and it takes rewriting commands as input. @End(Description)
@ChapterPh(Contexts)
The internal name of this category is @t(CONTEXT).
A context can be defined using 
DEFCONTEXT.
Allowable properties are: @t{SHORT-ID}, @t{ORDER}, @t{MHELP}.
@Begin(Description, Spread 0.5)
SUBTOPLEVELS@\ is for top levels.
TPS objects having to do with flow of control between top levels.

STYLE@\ is for style.
A TPS object associated with STYLE.

FLAGS@\ is for review.
A TPS object connected to REVIEW.

FLAG-REVIEW@\ is for flags.
Examining and changing flags.

FLAG-MODES@\ is for modes.
Defining, saving, and switching modes.

RD-HELP@\ is for reading help.
Concerning the automatic reading of help messages.

HELP-OBJ@\ is for help.
TPS object providing help or giving information.

COLL-HELP@\ is for collecting help.
Concerning the automatic collection of help messages.

CONCEPT-TERMINAL@\ is for concept.
TPS objects dealing with the Concept terminal.

OTL-ENTERING@\ is for starting and finishing.
Commands for entering and leaving ETPS.

OTL-OBJECT@\ is for otl object.
Objects from the outline package.

OTL-PRINTING@\ is for printing.
Commands for looking at the proof outline.

WFF-PRINTING@\ is for printing.
TPS objects which have to do with printing of wffs.

PRINT-INTERNALS@\ is for internal for printing.
Operations used internally for printing purposes.

SAIL-CHARS@\ is for sail characters.
Related to the special characters in the SAIL character set.

SCRIPT-LETTERS@\ is for script letters.
Uppercase script letters.

SUBSCRIPTS@\ is for subscripts.
Non-greek subscript symbols.

SUPERSCRIPTS@\ is for superscripts.
Symbols which print as superscripts.

GREEK-LETTERS-LOWERCASE@\ is for lowercase greek.
Lowercase Greek letters.

GREEK-LETTERS-UPPERCASE@\ is for uppercase greek.
Uppercase Greek letters.

GREEK-SUBSCRIPTS@\ is for greek subscripts.
Greek Subscripts as used for type symbols.

BOLD-LETTERS@\ is for bold letters.
Upper case very bold letters.

TEX-STYLE@\ is for tex.
TPS objects having to do with the TeX output style.

XWINDOWS@\ is for x windows.
TPS objects related to the use of the X window system.

MISC-SYMBOLS@\ is for other symbols.
Other symbols, which are not superscripts, subscripts or letters.

WEAK-LABELS@\ is for weak labels.
Related to WEAK labels (which dissolve under substitution).

FLAVOR-OBJ@\ is for flavors of labels.
TPS objects dealing with flavors of labels.

SAVE-WORK-OBJ@\ is for saving work.
TPS objects concerning saving and restoring work.

SAVING-WFFS@\ is for saving wffs.
Having to do with writing weak labels to a file.

SCRIBE-RECORD@\ is for recording.
TPS Objects concerned with recording wffs into files.

OTL-FILES@\ is for printing proofs into files.
Dealing with writing files in the outline package.

PROOF-OUTLINE@\ is for proof outline.
Objects used in proof outlines.

EXPANSION-TREES@\ is for expansion trees.
TPS objects dealing with expansion trees.

MTREE-OPS@\ is for mtree operations.
TPS objects dealing with manipulating matingstrees.

MTREE-PRINT@\ is for mtree printing.
TPS objects dealing with displaying matingstrees.

MTREE-AUTO@\ is for mtree auto.
Automatic commands to do with matingstrees.

SEARCH-SUGGESTIONS@\ is for search suggestions.
Flag setting suggestions for automatic search.

DEC-FRAGS@\ is for decidable fragments.
Functions related to decidable fragments of type theory.

PERS@\ is for per refined models.
Computing with Per Refined Models of Type Theory.

MATING-SEARCH@\ is for mating search.
Concerning mating search.

MS88@\ is for ms88 search procedure.
Concerning mating search procedure MS88.

MS89@\ is for ms89 search procedure.
Concerning mating search procedure MS89.

MS90-3@\ is for ms90-3 search procedure.
Concerning mating search procedure MS90-3.

MS90-9@\ is for ms90-9 search procedure.
Concerning mating search procedure MS90-9.

MS91@\ is for ms91-6 and ms91-7 search procedures.
Concerning mating search procedures MS91-6 and MS91-7.

MS92-9@\ is for ms92-9 search procedure.
Concerning mating search procedure MS92-9.

MS93-1@\ is for ms93-1 search procedure.
Concerning mating search procedure MS93-1.

MS98-1@\ is for ms98-1 search procedure.
Concerning mating search procedure MS98-1.

EXT-SEARCH@\ is for extensional search.
Extensional Search

ETR-NAT@\ is for proof translation.
Concerning translation between expansion proofs and natural
deduction proofs.

UNIFICATION@\ is for unification.
Commands for unification.

UNIFICATION-DPAIRS@\ is for dpairs.
Disagreement pairs in the unification problems.

TACTICS@\ is for tactics.
Tactics and related functions.

SEARCH-ANALYSIS@\ is for search analysis.
Concerning analyzing the search for automatic proofs.

SUGGESTIONS@\ is for suggestions.
Concerning automatic suggestions in the outline package.

TEST-SEARCHLISTS@\ is for searchlists.
Concerning construction of test-top searchlists.

TEST-LIB@\ is for library.
Concerning library objects in the test-top top level.

EXT-SEQ@\ is for extensional sequent calculus.
TPS objects dealing with extensional sequent derivations

JFORMS1@\ is for vpforms.
Commands for converting wffs to jforms, converting jforms to wffs,
displaying jforms, and printing vertical path diagrams.

EXT-SEQ-ENTERING@\ is for extensional sequent entering.
Functions for starting and manipulating extensional sequent derivations

EXT-SEQ-PRINTING@\ is for extensional sequent printing.
Printing functions for extensional sequent derivations

EXT-SEQ-RULES@\ is for extensional sequent rules.
Rules for extensional sequent derivations

EXT-SEQ-DERIVED-RULES@\ is for extensional sequent derived rules.
Derived rules for extensional sequent derivations

EXT-SEQ-TACTICS@\ is for extensional sequent tactics.
Tactics for extensional sequent derivations

EXT-SEQ-FILES@\ is for extensional sequent files.
Commands dealing with files for extensional sequent derivations

OTL-REARRANGING@\ is for rearranging the proof.
Commands for rearranging the proof outline.

EXT-EXP-DAGS@\ is for extensional expansion dags.
Extensional Expansion Dags

OTL-STATUS@\ is for status.
Commands for looking at the status information for the proof outline.

SEMANTICS@\ is for semantics.
Semantics

MODELS@\ is for models.
Models

LOG-RELNS@\ is for logical relations.
Logical Relations on Models

S-EQN@\ is for rewriting toplevel.
Rewriting in the simply typed lambda-calculus

S-EQN-ENTERING@\ is for starting and finishing.
Functions for starting and manipulating derivations

S-EQN-PRINTING@\ is for printing.
Printing functions for equational derivations

S-EQN-AXIOMS@\ is for equational axioms.
Equational axioms for the simply typed lambda-calculus

S-EQN-RULES@\ is for applying rules.
Rules for equational derivations

S-EQN-REARRANGE@\ is for rearranging the derivation.
Rules for rearranging equational proofs

S-EQN-LAMBDA@\ is for lambda conversion.
Rules for applying lambda conversion within equational proofs

S-EQN-THEORIES@\ is for theories.
Loading, saving and modifying rewrite theories

RULES-1-MISC@\ is for miscellaneous rules.
 

RULES-2-PROP@\ is for propositional rules.
 

RULES-3-NEG@\ is for negation rules.
 

RULES-4-QUANT@\ is for quantifier rules.
 

RULES-5-SUBST@\ is for substitution rules.
 

RULES-6-EQUALITY@\ is for equality rules.
 

RULES-7-DEFN@\ is for definition rules.
 

RULES-8-LAMBDA@\ is for lambda conversion rules.
Having to do with lambda conversion rules.

BOOK-THEOREMS@\ is for book theorems.
Book Theorems.

TPS-THEOREMS@\ is for theorems.
Having to do with theorems.

ML1-EXERCISES@\ is for first-order logic.
Having to do with exercises for first order logic.

ML2-EXERCISES@\ is for higher-order logic.
Having to do with exercises for higher order logic.

EDITOR-OBJ@\ is for wff editor.
TPS objects connected with the wff editor.

WELL-FF@\ is for well-formed formula.
Having to do with well-formed formulae.

PRIM-OBJ@\ is for wff primitives.
TPS objects connected to primitives concerning wffs.

WFF-PARSING@\ is for wff parsing.
TPS object related to the parsing of wffs.

WFFEQUAL@\ is for equality between wffs.
Test for equality between wffs and related normalizations.

WFFTST-OBJ@\ is for predicates on wffs.
TPS objects concerning predicates on wffs.

WFFTYP-OBJ@\ is for wff types.
TPS objects concerning types of wffs.

MOVING@\ is for moving commands.
Commands which move around in a wff.

CHANGING@\ is for changing commands.
Commands which change wffs.

RECURSIVELY-CHANGING@\ is for recursively changing commands.
Commands which change a wff as well as the subwffs of the wff.

EMBEDDING@\ is for embedding commands.
Commands which embed a wff within a quantifier or connective.

REWRITING@\ is for rewriting commands.
Commands to do with rewriting wffs.

SUBSTITUTION@\ is for substitution.
TPS objects doing substitution in and for wffs.

ABBREV-OPS@\ is for basic abbreviations.
TPS objects having to do with logical abbreviations.

ABBREV-SET-OPS@\ is for set abbreviations.
Set-theoretic logical abbreviations.

LAMBDA-OP@\ is for lambda-calculus.
TPS object dealing with operations in the lambda-calculus.

NEG-OPS@\ is for negation movers.
Change scopes of negations. May later be part of similar
context for quantifiers.

PRIMSUBS@\ is for primitive substitutions.
For creating substitutable wffs.

MISC-EDOPS@\ is for miscellaneous.
Edops dealing with miscellaneous operations on gwffs.

RULEP-TEST@\ is for rulep.
Concerning testing of tautologies.

SKOLEMS@\ is for skolemizing.
Having to do with Skolem functions and Skolemizing.

DEVELOP-SEQS@\ is for quantifier commands.
TPS objects having to do with development sequences.

ILL-FORMED@\ is for wellformedness.
TPS objects dealing with potentially ill-formed formulas.

COMPOUND-TACTICS@\ is for compound.
Compound tactics.

PROP-TACTICS@\ is for propositional.
Tactics which carry out propositional rules.

QUANT-TACTICS@\ is for quantifiers.
Tactics which operate on quantifiers.

EQUALITY-TACTICS@\ is for equality.
Tactics which use equality rules.

DEFN-TACTICS@\ is for definitions.
Tactics which handle wff definitions.

LAMBDA-TACTICS@\ is for lambda.
Tactics which use lambda-calculus operations.

AUX-TACTICS@\ is for auxiliary.
Auxiliary tactics.

TPS-EVENTS@\ is for events.
Having to do with events.

REPORT-OBJECT@\ is for report package.
Objects used in the REPORT package.

FILE-OPERATIONS@\ is for file utilities.
Utilities dealing with files and keeping records.

REPORT-EXAMPLES@\ is for example of report.
Dealing with examples of reports.

STATS@\ is for statistics.
The statistics of commands, error, etc.

GRADER-OBJECT@\ is for grader.
Objects to do with the TEACHER package.

GR-A-OUT@\ is for getting out and help.
Grader Commands for leaving Grader.

GR-B-VARS@\ is for variables.
Grader Command for changing values of variables.

GR-C-GRADEFILE@\ is for the grade-file.
Grader Command for creating Grade-File.

GR-D-MANUAL-GRADES@\ is for manual grades.
Grader Commands for modifying grades.

GR-E-AUTOMATIC-GRADES@\ is for automatic grades.
Grader Commands for collecting grades from ETPS file.

GR-F-CLASS-LIST@\ is for the class list.
Grader Commands for modifying class list.

GR-G-OUTPUT@\ is for making the output convenient.
Grader Commands for making the output convenient.

GR-H-STAT@\ is for generating values.
Grader Command for calculating statistical data.

GR-I-DISPLAY@\ is for displaying information.
Grader Commands for looking at various items in the class list.

GR-J-TOTALS@\ is for totaling.
Grader Commands for calculating grades.

TPS-MAINTENANCE@\ is for maintenance.
TPS-objects which help in maintaining TPS.

GR-K-SORTING@\ is for sorting.
Grader Command for sorting grades.

GR-L-LETTER-GRADE@\ is for letter-grades.
Grader Command for assigning letter grades.

BASICS@\ is for basics.
Basic TPS objects (inside the package BARE).

MODULES-IN-TPS@\ is for modules.
TPS objects dealing with the module structure.

RULE-COMMANDS@\ is for rule commands.
Commands implementing rule of inference.

RULE-RUN@\ is for rules module.
TPS objects useful in running the RULES module to produce
a set of commands implementing the rules of inference of a logical system.

LISP-PACKAGES@\ is for lisp packages.
Functions relating to LISP packages.

RULES-OBJECT@\ is for rules object.
An object from the rules module.

SYSTEM-NEWS@\ is for news.
News files for insiders. Note files for public notice.

CORE-IMAGE@\ is for core images.
Executable files.

INDIRECT@\ is for indirect files.
Files containing arguments for exec commands.

BATCH@\ is for batch control.
Batch control files.

DOCUMENTATION@\ is for documentation.
Files for TPS documentation.

COMMAND-DECLARATION@\ is for command declaration.
PCL and DCL files for creating commands on exec.

LISP-SOURCE@\ is for lisp source.
Lisp source files.

MISCELLANEOUS@\ is for miscellaneous.
Miscellaneous TPS objects.

UNCLASSIFIED@\ is for unclassified.
TPS object not classified into any context.

LIBRARY@\ is for library.
Library objects.

LIB-DISPLAY@\ is for display.
Commands associated with displaying objects, especially library objects.

LIB-READING@\ is for reading.
Commands associated with reading library objects into TPS.

LIB-STRUCTURE@\ is for library structure.
Commands for manipulating the directory and file structure of the library.

LIB-WRITING@\ is for editing.
Commands associated with modifying library objects.

LIB-KEYS@\ is for keywords.
Commands associated with keywords in the library

LIB-MODES@\ is for best modes.
Commands associated with best modes in the library

LIB-CLASS@\ is for library classification.
Commands associated with Classification Schemes for the library.

INTERFACE@\ is for interface.
Commands associated with TPS interfaces, e.g., the Java interface.

LIB-BUGS@\ is for bugs.
Commands associated with reading and writing bug records.

@End(Description)
@ChapterPh(Argument Types)
The internal name of this category is 
ARGTYPE.
An argument type can be defined using DEFTYPE%%.
Allowable properties are: @t{TESTFN}, @t{GETFN}, @t{PRINTFN}, @t{SHORT-PROMPT}, @t{MHELP}.

@Section(Style)

@Begin(Description)
@IndexOther(FONTSIZESTRING)@\
A string describing the fontsize for an interface:
The empty string "" means normal sized fonts.
The string "-big" means big fonts.
The string "-x2" or "-x4" means normal sized fonts times 2 or 4.
The string "-big -x2" or "-big -x4" means big fonts times 2 or 4.@End(Description)

@Section(Review)

@Begin(Description)
@IndexOther(ANYLIST)@\
A list.

@IndexOther(DEV-STYLE)@\
This specifies the style for the output file.
Currently any of: 
@Begin(description, spread 0.3)

CONCEPT:@\concept concept-s 

PRINTING:@\generic-string istyle scribe 

REVIEW:@\generic 

SAIL CHARACTERS:@\sail 

TEX:@\tex tex-1 

X WINDOWS:@\xterm 
@End(description)

@IndexOther(FSYM)@\
A symbol which may be printed differently depending on the style.

@IndexOther(MODES-GWFFS)@\
A symbol naming a pair of MODES and GWFFS where MODES is a list of modes and GWFFS is a list of GWFFS

@IndexOther(SUBJECT)@\
A subject in REVIEW.
Currently any of: 
@Begin(description, spread 0.3)

EVENTS:@\events 

EXPANSION TREES:@\etrees 

EXTENSIONAL SEARCH:@\ext-search ms03-7 ms04-2 

FLAVORS OF LABELS:@\internal-names 

GRADER:@\gr-filenames gr-misc 

LIBRARY:@\library 

MS88 SEARCH PROCEDURE:@\ms88 

MS89 SEARCH PROCEDURE:@\ms89 

MS90-3 SEARCH PROCEDURE:@\ms90-3 

MS90-9 SEARCH PROCEDURE:@\ms90-9 

MS91-6 AND MS91-7 SEARCH PROCEDURES:@\ms91-6 ms91-7 

MS92-9 SEARCH PROCEDURE:@\ms92-9 

MS93-1 SEARCH PROCEDURE:@\ms93-1 

MS98-1 SEARCH PROCEDURE:@\ms98-1 ms98-minor 

MAINTENANCE:@\maintain system 

MATING SEARCH:@\important mating-search transmit 

MTREE OPERATIONS:@\mtree mtree-top 

OTL OBJECT:@\otl-vars outline 

PRIMITIVE SUBSTITUTIONS:@\primsubs 

PRINTING:@\printing printing-tex window-props 

PROOF TRANSLATION:@\etr-nat 

RULES OBJECT:@\rules-mod 

SAVING WORK:@\saving-work 

SEMANTICS:@\semantic-bounds 

TACTICS:@\tactics 

TOP LEVELS:@\editor test-top 

UNIFICATION:@\unification 

VPFORMS:@\jforms 

WFF PARSING:@\parsing 

SUGGESTIONS:@\suggests 

WFF PRIMITIVES:@\wff-prims 
@End(description)

@IndexOther(SUBJECTLIST)@\
A list of subjects in REVIEW or ALL for all subjects.
Currently any of: 
@Begin(description, spread 0.3)

EVENTS:@\events 

EXPANSION TREES:@\etrees 

EXTENSIONAL SEARCH:@\ext-search ms03-7 ms04-2 

FLAVORS OF LABELS:@\internal-names 

GRADER:@\gr-filenames gr-misc 

LIBRARY:@\library 

MS88 SEARCH PROCEDURE:@\ms88 

MS89 SEARCH PROCEDURE:@\ms89 

MS90-3 SEARCH PROCEDURE:@\ms90-3 

MS90-9 SEARCH PROCEDURE:@\ms90-9 

MS91-6 AND MS91-7 SEARCH PROCEDURES:@\ms91-6 ms91-7 

MS92-9 SEARCH PROCEDURE:@\ms92-9 

MS93-1 SEARCH PROCEDURE:@\ms93-1 

MS98-1 SEARCH PROCEDURE:@\ms98-1 ms98-minor 

MAINTENANCE:@\maintain system 

MATING SEARCH:@\important mating-search transmit 

MTREE OPERATIONS:@\mtree mtree-top 

OTL OBJECT:@\otl-vars outline 

PRIMITIVE SUBSTITUTIONS:@\primsubs 

PRINTING:@\printing printing-tex window-props 

PROOF TRANSLATION:@\etr-nat 

RULES OBJECT:@\rules-mod 

SAVING WORK:@\saving-work 

SEMANTICS:@\semantic-bounds 

TACTICS:@\tactics 

TOP LEVELS:@\editor test-top 

UNIFICATION:@\unification 

VPFORMS:@\jforms 

WFF PARSING:@\parsing 

SUGGESTIONS:@\suggests 

WFF PRIMITIVES:@\wff-prims 
@End(description)

@IndexOther(SYMBOLLIST)@\
A list of symbols.

@IndexOther(TPS-MODE)@\
A TPS mode in REVIEW. If it is not loaded, search for it in library.
Currently any of: 
@Begin(description, spread 0.3)

COLLECTING HELP:@\scribe-doc scribe-doc-first-order 

EXPANSION TREES:@\naive 

MS91-6 AND MS91-7 SEARCH PROCEDURES:@\ms91-deep ms91-nodups ms91-original ms91-simplest 

MAINTENANCE:@\quiet 

OTL OBJECT:@\rules scribe-otl tex-1-otl tex-otl 

PRINTING:@\re-read 

RECORDING:@\scribe-edwff scribe-matewff 

UNCLASSIFIED:@\math-logic-2-mode ml msv-off msv-on 

WFF PRIMITIVES:@\first-order higher-order 
@End(description)
@End(Description)

@Section(Flags)

@Begin(Description)
@IndexOther(FLAG-AND-VAL)@\
Type for dotted pair of flag name & value.

@IndexOther(FV-LIST)@\
A list of dotted pairs of flags and values.

@IndexOther(TPSFLAG)@\
A global flag or parameter.
Currently any of: 
@Begin(description, spread 0.3)

APPLYING RULES:@\app*-rewrite-depth rewriting-auto-depth rewriting-auto-global-sort rewriting-auto-max-wff-size rewriting-auto-min-depth rewriting-auto-search-type rewriting-auto-substs rewriting-auto-table-size 

AUXILIARY:@\use-rulep use-symsimp 

BASIC ABBREVIATIONS:@\rewrite-equalities 

BUGS:@\default-bug-dir use-default-bug-dir 

COLLECTING HELP:@\omdoc-aut-creator omdoc-catalogue omdoc-rights omdoc-source omdoc-trc-creator omdoc-type 

EDITING:@\auto-keywords auto-lib-dir 

EVENTS:@\advice-asked-enabled advice-file command-enabled command-file done-exc-enabled error-enabled error-file event-cycle events-enabled input-error-enabled input-error-file proof-action-enabled proof-file quiet-events rule-error-enabled rule-error-file score-file user-passwd-file 

EXPANSION TREES:@\add-truth duplication-strategy duplication-strategy-pfd econj-name edisj-name empty-dup-info-name eproof-name expansion-name false-name imp-name initial-bktrack-limit leaf-name mating-name min-quantifier-scope neg-name print-deep print-nodenames pseq-use-labels rewrite-defns rewrite-name selection-name show-skolem skolem-default skolem-selection-name true-name truthvalues-hack 

EXTENSIONAL SEARCH:@\ext-search-limit ms03-dup-method ms03-quick-eunification-limit ms03-solve-rigid-parts ms03-solve-rigid-parts-allow-reconnects ms03-use-jforms ms03-use-set-constraints ms03-verbose ms03-weight-banned-sels ms03-weight-change-dups ms03-weight-disj-eunif ms03-weight-disj-mate ms03-weight-disj-unif ms03-weight-dup-var ms03-weight-eunif1 ms03-weight-eunif2 ms03-weight-flexflexdiff ms03-weight-flexflexdiff-o ms03-weight-flexflexsame ms03-weight-flexflexsame-o ms03-weight-flexrigid-branch ms03-weight-flexrigid-eqn ms03-weight-flexrigid-flexeqn ms03-weight-flexrigid-mate ms03-weight-flexrigid-noeqn ms03-weight-flexrigid-o ms03-weight-imitate ms03-weight-occurs-check ms03-weight-primsub-falsehood ms03-weight-primsub-first-and ms03-weight-primsub-first-equals ms03-weight-primsub-first-exists ms03-weight-primsub-first-forall ms03-weight-primsub-first-not-equals ms03-weight-primsub-first-not-proj ms03-weight-primsub-first-or ms03-weight-primsub-first-proj ms03-weight-primsub-next-and ms03-weight-primsub-next-equals ms03-weight-primsub-next-exists ms03-weight-primsub-next-forall ms03-weight-primsub-next-not-equals ms03-weight-primsub-next-not-proj ms03-weight-primsub-next-or ms03-weight-primsub-next-proj ms03-weight-primsub-truth ms03-weight-project ms03-weight-rigid-mate ms03-weight-rigidrigid-eqn ms03-weight-rigidrigid-flexeqn ms03-weight-rigidrigid-noeqn ms03-weight-rigidrigiddiff-o ms03-weight-rigidrigidsame-o ms04-allow-flex-eunifs ms04-allow-flexrigid-proj-mate ms04-backtrack-method ms04-check-unif-depth ms04-delay-flexrigid-mates ms04-delay-unif-constraints ms04-dup-early ms04-dup-weight ms04-eager-unif-subst ms04-incr-depth ms04-initial-depth ms04-max-delayed-conns ms04-max-depth ms04-max-dups ms04-max-eunif1s ms04-max-eunif2s ms04-max-flex-eunifs ms04-max-flexrigid-mates ms04-max-flexrigid-neg-mates ms04-max-flexrigid-neg-proj-mates ms04-max-flexrigid-proj-mates ms04-max-imits ms04-max-primsub-and ms04-max-primsub-equals ms04-max-primsub-exists ms04-max-primsub-forall ms04-max-primsub-not ms04-max-primsub-not-equals ms04-max-primsub-not-proj ms04-max-primsub-or ms04-max-primsub-proj ms04-max-projs ms04-max-rigid-mates ms04-mp-options ms04-prenex-primsubs ms04-semantic-pruning ms04-solve-unif-depth ms04-trace ms04-use-semantics ms04-use-set-constraints ms04-verbose ms04-weight-add-set-constraint ms04-weight-delay-unif ms04-weight-eunif-decs ms04-weight-eunif-diff-heads ms04-weight-flex-eunif ms04-weight-flexrigid-proj-mate ms04-weight-multiple-eunif1s ms04-weight-multiple-eunif2s ms04-weight-multiple-mates ms04-weight-primsub-first-not ms04-weight-primsub-next-not ms04-weight-primsub-nexttp ms04-weight-primsub-occurs-const ms04-weight-solve-set-constraints 

FLAGS:@\suppress-irrelevance-warnings 

FLAVORS OF LABELS:@\make-wffops-labels meta-label-name print-meta 

GRADER:@\cal-percentage course-name default-penalty-fn drop-min due-date-flag etps-file grade-dir grade-file letter-grade-file letter-grade-flag new-item old-grade-file old-totals-grade-file patch-file print-n-digits statistical-options totals-grade-file 

HELP:@\show-all-packages 

INTERNAL FOR PRINTING:@\infix-notation 

LAMBDA-CALCULUS:@\lambda-conv 

LIBRARY:@\add-subdirectories backup-lib-dir default-lib-dir default-libfile-type default-libindex-type lib-bestmode-file lib-keyword-file lib-masterindex-file recordflags remove-trailing-dir show-all-libobjects 

LIBRARY CLASSIFICATION:@\class-direction class-scheme 

MS88 SEARCH PROCEDURE:@\added-conn-enabled considered-conn-enabled dup-allowed dupe-enabled dupe-var-enabled excluding-gc-time first-order-mode-ms incomp-mating-enabled mate-ffpair mate-subsumed-test-enabled mate-subsumed-true-enabled mating-changed-enabled ms-init-path ms-split occurs-check prim-quantifier primsub-enabled prop-strategy removed-conn-enabled search-complete-paths start-time-enabled stop-time-enabled timing-named unif-subsumed-test-enabled unif-subsumed-true-enabled 

MS89 SEARCH PROCEDURE:@\max-search-limit rank-eproof-fn search-time-limit 

MS90-3 SEARCH PROCEDURE:@\max-mates min-quant-etree ms90-3-dup-strategy num-frpairs print-mating-counter show-time 

MS91-6 AND MS91-7 SEARCH PROCEDURES:@\ms91-interleave ms91-prefer-smaller ms91-time-by-vpaths ms91-weight-limit-range new-option-set-limit options-generate-arg options-generate-fn options-generate-update options-verbose penalty-for-each-primsub penalty-for-multiple-primsubs penalty-for-multiple-subs penalty-for-ordinary-dup reconsider-fn weight-a-coefficient weight-a-fn weight-b-coefficient weight-b-fn weight-c-coefficient weight-c-fn 

MS98-1 SEARCH PROCEDURE:@\break-at-quantifiers ff-delay hpath-threshold maximize-first measurements ms98-base-prim ms98-dup-below-primsubs ms98-dup-primsubs ms98-external-rewrites ms98-first-fragment ms98-force-h-o ms98-fragment-order ms98-init ms98-low-memory ms98-max-components ms98-max-prims ms98-measure ms98-merge-dags ms98-minimality-check ms98-num-of-dups ms98-pollute-global-rewrites ms98-primsub-count ms98-rew-primsubs ms98-rewrite-depth ms98-rewrite-model ms98-rewrite-prune ms98-rewrite-size ms98-rewrite-unif ms98-rewrites ms98-trace ms98-unif-hack ms98-unif-hack2 ms98-use-colors ms98-valid-pair ms98-variable-order ms98-verbose 

MAINTENANCE:@\compiled-extension expertflag goodmodes init-dialogue init-dialogue-fn java-comm lisp-implementation-type load-warn-p machine-instance machine-type news-dir read-lload-sources-p save-file short-site-name source-extension source-path test-modify test-theorems 

MATING SEARCH:@\assert-lemmas default-expand default-mate default-ms diy2-init-time-limit diy2-num-iterations diy2-time-increase-factor interrupt-enable mating-verbose monitorflag new-mating-after-dup query-user rec-ms-file rec-ms-filename use-diy use-ext-lemmas use-fast-prop-search 

MISCELLANEOUS:@\rewrite-equivs 

MODES:@\suppress-flags suppress-flags-list 

MTREE AUTO:@\mt-subsumption-check mt94-12-trigger mtree-filter-dups mtree-stop-immediately tag-conn-fn tag-mating-fn 

MTREE OPERATIONS:@\default-ob mt-default-ob-mate 

OTL OBJECT:@\assert-rrules auto-generate-hyps cleanup-rulec cleanup-same default-wffeq print-dots printlineflag short-help 

PRIMITIVE SUBSTITUTIONS:@\bad-var-connected-prune delay-setvars include-coinduction-principle include-induction-principle max-constraint-size max-num-constraints max-prim-depth max-prim-lits min-prim-depth min-prim-lits neg-prim-sub pr00-allow-subnode-conns pr00-max-substs-var pr00-num-iterations pr00-require-arg-deps pr97c-max-abbrevs pr97c-prenex prim-bdtypes prim-bdtypes-auto prim-prefix primsub-method which-constraints 

PRINTING:@\print-combined-egens print-combined-ugens print-combined-uis print-until-ui-or-egen 

PRINTING:@\allscopeflag atomvalflag blank-lines-inserted charsize displaywff elim-defns fillineflag first-order-print-mode flushleftflag leftmargin localleftflag ppwfflag printdepth printtypes printtypes-all retain-initial-type rightmargin scope slides-preamble use-dot use-internal-print-mode 

PRINTING:@\rewriting-relation-symbol verbose-rewrite-justification 

PRINTING PROOFS INTO FILES:@\latex-postamble latex-preamble scribe-line-width scribe-postamble scribe-preamble tex-1-postamble tex-1-preamble tex-line-width tex-postamble tex-preamble tpstex vpdtex 

PROOF OUTLINE:@\print-comments slides-turnstile-indent slides-turnstyle-indent support-numbers turnstile-indent turnstile-indent-auto turnstyle-indent turnstyle-indent-auto 

PROOF TRANSLATION:@\etree-nat-verbose matingstree-name merge-minimize-mating nat-etree-version natree-debug remove-leibniz renumber-leaves 

PROPOSITIONAL RULES:@\rulep-mainfn 

QUANTIFIERS:@\ui-herbrand-limit 

RECORDING:@\printedtfile printedtflag printedtflag-slides printedtops printmatefile printmateflag printmateflag-slides printmateops 

REVIEW:@\alpha-lower-flag last-mode-name 

RULEP:@\rulep-wffeq 

RULES OBJECT:@\build-match hline-justification treat-hlines-as-dlines 

SAVING WORK:@\save-interval save-work-on-start-up save-work-p 

SEARCHLISTS:@\test-easier-if-high test-easier-if-low test-easier-if-nil test-easier-if-t test-faster-if-high test-faster-if-low test-faster-if-nil test-faster-if-t test-fix-unif-depths test-increase-time test-initial-time-limit test-max-search-values test-next-search-fn test-reduce-time test-verbose testwin-height testwin-width 

SEMANTICS:@\max-binder-computation max-domain-size 

SKOLEMIZING:@\name-skolem-fn 

STARTING AND FINISHING:@\completion-options history-size 

STYLE:@\style 

TACTICS:@\default-tactic tacmode tactic-verbose tacuse 

TEX:@\in-tex-math-mode latex-emulation pagelength pagewidth tex-break-before-symbols tex-mimic-scribe 

TOP LEVELS:@\ext-mate-recompute-jforms mt-dups-per-quant proofw-active proofw-active+nos proofw-active+nos-height proofw-active+nos-width proofw-active-height proofw-active-width proofw-all proofw-all-height proofw-all-width unixlib-showpath 

UNCLASSIFIED:@\max-substs-proj max-substs-proj-total max-substs-quick max-substs-var num-of-dups primsub-var-select 

UNIFICATION:@\apply-match countsubs-first dneg-imitation eta-rule imitation-first leibniz-sub-check max-dup-paths max-search-depth max-utree-depth min-quick-depth ms-dir ms90-3-quick pruning reduce-double-neg rigid-path-ck stop-at-tsn subsumption-check subsumption-depth subsumption-nodes total-num-of-dups uni-search-heuristic unif-counter unif-counter-output unif-trigger unify-verbose 

VPFORMS:@\allow-nonleaf-conns dissolve lit-name mate-up-to-nnf order-components print-lit-name printvpdflag texformat vpd-brief vpd-filename vpd-lit-name vpd-ptypes vpd-style vpd-vpfpage vpform-labels vpform-tex-magnification vpform-tex-nest vpform-tex-preamble vpw-height vpw-width 

WEAK LABELS:@\print-weak 

WFF EDITOR:@\edppwfflag edprintdepth edwin-current edwin-current-height edwin-current-width edwin-top edwin-top-height edwin-top-width edwin-vpform edwin-vpform-height edwin-vpform-width 

WFF PARSING:@\base-type first-order-mode-parse lowercaseraise type-iota-mode untyped-lambda-calculus 

X WINDOWS:@\use-window-style window-style xterm-ansi-bold 

SUGGESTIONS:@\go-instructions quietly-use-defaults resolve-conflict 

WFF PRIMITIVES:@\meta-bdvar-name meta-var-name ren-var-fn rename-all-bd-vars 
@End(description)
@End(Description)

@Section(Help)

@Begin(Description)
@IndexOther(HELP*-LIST)@\
A list of names of TPS objects. Only used by HELP*

@IndexOther(SYMBOL-OR-INTEGER-LIST)@\
No more help available.  Sorry.@End(Description)

@Section(Collecting Help)

@Begin(Description)
@IndexOther(CONTEXT)@\
A context.

@IndexOther(CONTEXTLIST)@\
A list of contexts or ALL or (ALL- ...).
Currently any of: 
@Begin(description, spread 0.3)

APPLYING RULES:@\s-eqn-rules 

AUTOMATIC GRADES:@\gr-e-automatic-grades 

AUXILIARY:@\aux-tactics 

BASIC ABBREVIATIONS:@\abbrev-ops 

BASICS:@\basics 

BATCH CONTROL:@\batch 

BEST MODES:@\lib-modes 

BOLD LETTERS:@\bold-letters 

BOOK THEOREMS:@\book-theorems 

BUGS:@\lib-bugs 

CHANGING COMMANDS:@\changing 

COLLECTING HELP:@\coll-help 

COMMAND DECLARATION:@\command-declaration 

COMPOUND:@\compound-tactics 

CONCEPT:@\concept-terminal 

CORE IMAGES:@\core-image 

DECIDABLE FRAGMENTS:@\dec-frags 

DEFINITION RULES:@\rules-7-defn 

DEFINITIONS:@\defn-tactics 

DISPLAY:@\lib-display 

DISPLAYING INFORMATION:@\gr-i-display 

DOCUMENTATION:@\documentation 

DPAIRS:@\unification-dpairs 

EDITING:@\lib-writing 

EMBEDDING COMMANDS:@\embedding 

EQUALITY:@\equality-tactics 

EQUALITY RULES:@\rules-6-equality 

EQUALITY BETWEEN WFFS:@\wffequal 

EQUATIONAL AXIOMS:@\s-eqn-axioms 

EVENTS:@\tps-events 

EXAMPLE OF REPORT:@\report-examples 

EXPANSION TREES:@\expansion-trees 

EXTENSIONAL EXPANSION DAGS:@\ext-exp-dags 

EXTENSIONAL SEARCH:@\ext-search 

EXTENSIONAL SEQUENT CALCULUS:@\ext-seq 

EXTENSIONAL SEQUENT DERIVED RULES:@\ext-seq-derived-rules 

EXTENSIONAL SEQUENT ENTERING:@\ext-seq-entering 

EXTENSIONAL SEQUENT FILES:@\ext-seq-files 

EXTENSIONAL SEQUENT PRINTING:@\ext-seq-printing 

EXTENSIONAL SEQUENT RULES:@\ext-seq-rules 

EXTENSIONAL SEQUENT TACTICS:@\ext-seq-tactics 

FILE UTILITIES:@\file-operations 

FIRST-ORDER LOGIC:@\ml1-exercises 

FLAGS:@\flag-review 

FLAVORS OF LABELS:@\flavor-obj 

GENERATING VALUES:@\gr-h-stat 

GETTING OUT AND HELP:@\gr-a-out 

GRADER:@\grader-object 

GREEK SUBSCRIPTS:@\greek-subscripts 

HELP:@\help-obj 

HIGHER-ORDER LOGIC:@\ml2-exercises 

INDIRECT FILES:@\indirect 

INTERFACE:@\interface 

INTERNAL FOR PRINTING:@\print-internals 

KEYWORDS:@\lib-keys 

LAMBDA:@\lambda-tactics 

LAMBDA CONVERSION:@\s-eqn-lambda 

LAMBDA CONVERSION RULES:@\rules-8-lambda 

LAMBDA-CALCULUS:@\lambda-op 

LETTER-GRADES:@\gr-l-letter-grade 

LIBRARY:@\test-lib 

LIBRARY:@\library 

LIBRARY CLASSIFICATION:@\lib-class 

LIBRARY STRUCTURE:@\lib-structure 

LISP SOURCE:@\lisp-source 

LISP PACKAGES:@\lisp-packages 

LOGICAL RELATIONS:@\log-relns 

LOWERCASE GREEK:@\greek-letters-lowercase 

MS88 SEARCH PROCEDURE:@\ms88 

MS89 SEARCH PROCEDURE:@\ms89 

MS90-3 SEARCH PROCEDURE:@\ms90-3 

MS90-9 SEARCH PROCEDURE:@\ms90-9 

MS91-6 AND MS91-7 SEARCH PROCEDURES:@\ms91 

MS92-9 SEARCH PROCEDURE:@\ms92-9 

MS93-1 SEARCH PROCEDURE:@\ms93-1 

MS98-1 SEARCH PROCEDURE:@\ms98-1 

MAINTENANCE:@\tps-maintenance 

MAKING THE OUTPUT CONVENIENT:@\gr-g-output 

MANUAL GRADES:@\gr-d-manual-grades 

MATING SEARCH:@\mating-search 

MISCELLANEOUS:@\misc-edops 

MISCELLANEOUS:@\miscellaneous 

MISCELLANEOUS RULES:@\rules-1-misc 

MODELS:@\models 

MODES:@\flag-modes 

MODULES:@\modules-in-tps 

MOVING COMMANDS:@\moving 

MTREE AUTO:@\mtree-auto 

MTREE OPERATIONS:@\mtree-ops 

MTREE PRINTING:@\mtree-print 

NEGATION RULES:@\rules-3-neg 

NEGATION MOVERS:@\neg-ops 

NEWS:@\system-news 

OTL OBJECT:@\otl-object 

OTHER SYMBOLS:@\misc-symbols 

PER REFINED MODELS:@\pers 

PREDICATES ON WFFS:@\wfftst-obj 

PRIMITIVE SUBSTITUTIONS:@\primsubs 

PRINTING:@\otl-printing 

PRINTING:@\wff-printing 

PRINTING:@\s-eqn-printing 

PRINTING PROOFS INTO FILES:@\otl-files 

PROOF OUTLINE:@\proof-outline 

PROOF TRANSLATION:@\etr-nat 

PROPOSITIONAL:@\prop-tactics 

PROPOSITIONAL RULES:@\rules-2-prop 

QUANTIFIER COMMANDS:@\develop-seqs 

QUANTIFIER RULES:@\rules-4-quant 

QUANTIFIERS:@\quant-tactics 

READING:@\lib-reading 

READING HELP:@\rd-help 

REARRANGING THE DERIVATION:@\s-eqn-rearrange 

REARRANGING THE PROOF:@\otl-rearranging 

RECORDING:@\scribe-record 

RECURSIVELY CHANGING COMMANDS:@\recursively-changing 

REVIEW:@\flags 

REWRITING TOPLEVEL:@\s-eqn 

REWRITING COMMANDS:@\rewriting 

RULE COMMANDS:@\rule-commands 

RULEP:@\rulep-test 

RULES MODULE:@\rule-run 

RULES OBJECT:@\rules-object 

SAIL CHARACTERS:@\sail-chars 

SAVING WFFS:@\saving-wffs 

SAVING WORK:@\save-work-obj 

SCRIPT LETTERS:@\script-letters 

SEARCH ANALYSIS:@\search-analysis 

SEARCH SUGGESTIONS:@\search-suggestions 

SEARCHLISTS:@\test-searchlists 

SEMANTICS:@\semantics 

SET ABBREVIATIONS:@\abbrev-set-ops 

SKOLEMIZING:@\skolems 

SORTING:@\gr-k-sorting 

STARTING AND FINISHING:@\otl-entering 

STARTING AND FINISHING:@\s-eqn-entering 

STATISTICS:@\stats 

STATUS:@\otl-status 

STYLE:@\style 

SUBSCRIPTS:@\subscripts 

SUBSTITUTION:@\substitution 

SUBSTITUTION RULES:@\rules-5-subst 

SUPERSCRIPTS:@\superscripts 

TACTICS:@\tactics 

TEX:@\tex-style 

THE CLASS LIST:@\gr-f-class-list 

THE GRADE-FILE:@\gr-c-gradefile 

THEOREMS:@\tps-theorems 

THEORIES:@\s-eqn-theories 

TOP LEVELS:@\subtoplevels 

TOTALING:@\gr-j-totals 

UNCLASSIFIED:@\unclassified 

UNIFICATION:@\unification 

UPPERCASE GREEK:@\greek-letters-uppercase 

VARIABLES:@\gr-b-vars 

VPFORMS:@\jforms1 

WEAK LABELS:@\weak-labels 

WELLFORMEDNESS:@\ill-formed 

WFF EDITOR:@\editor-obj 

WFF PARSING:@\wff-parsing 

WFF TYPES:@\wfftyp-obj 

X WINDOWS:@\xwindows 

REPORT PACKAGE:@\report-object 

SUGGESTIONS:@\suggestions 

WELL-FORMED FORMULA:@\well-ff 

WFF PRIMITIVES:@\prim-obj 
@End(description)

@IndexOther(DEV-STYLELIST)@\
A list of device styles.

@IndexOther(TPSCAT)@\
A category of TPS objects.

@IndexOther(TPSCATLIST)@\
A list of categories or ALL or (ALL- ...).  Currently any of 
(%THEOREM% ABBREV ARGTYPE BINDER CLASS-SCHEME CONCEPT-CHAR CONTEXT
 DEVICE-STYLE EDOP EVENT EXTMATECMD EXTSEQCMD FLAG FLAG-MODE FLAVOR
 GETGWFFTYPE GEXPR INFO IRULEDEF LIBOBJECT LIBRARYCMD LISP-PACK
 LOGCONST MATEOP MENU MENUITEM MEXPR MODELSCMD MODES-GWFFS MODULE
 MONITORFN MTREEOP ORDERCOMPONENTS PMPROPSYM PRINT-FACE PRINTPROP
 REPSYMBOL REVIEW-SUBJECT REVIEWCMD REWRITE-RULE RULEHELP SAVEDWFF
 SCRIBE-CHAR SEQNCMD SRULE TACTIC TACTICAL TESTCMD TEX-CHAR THEORY
 TOPLEVEL TPS-FILE TYPEABBREV TYPECONST UNIFOP UNIX-LIBRARYCMD UTILITY
 WFFOP WFFREC%)@End(Description)

@Section(Starting and Finishing)

@Begin(Description)
@IndexOther(DIRSPEC)@\
The name of a file directory, written as a string (delimited
by double-quotes).

@IndexOther(DIRSPECLIST)@\
No more help available.  Sorry.

@IndexOther(FILESPEC)@\
The name of a file as a string or TTY for terminal. @End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(PRINT-FUNCTION)@\
Should be one of PALL, ^P, ^PN, PSTATUS, PRFW-^P, PRFW-^PN or PRFW-PALL.

@IndexOther(PRINT-FUNCTION-LIST)@\
A list of elements of type print-function, which is to say
a list containing some or all (or none!) of 
PALL, ^P, ^PN, PSTATUS, PRFW-^P, PRFW-^PN and PRFW-PALL.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(IGNORE)@\
Used as RESULTTYPE for wff printing operations.

@IndexOther(INDENTATION)@\
Should be one of MIN, VARY, COMPRESS or FIX. Used by the flag 
TURNSTILE-INDENT-AUTO.@End(Description)

@Section(Saving Wffs)

@Begin(Description)
@IndexOther(WEAK-LABEL)@\
A weak label.

@IndexOther(WEAK-LABEL-LIST)@\
A list of weak labels.@End(Description)

@Section(Proof Outline)

@Begin(Description)
@IndexOther(EXISTING-LINE)@\
Line number of an existing line.

@IndexOther(EXISTING-LINELIST)@\
A list of existing lines.

@IndexOther(JUSTIFICATION)@\
The justification of a line in a proof in the form (string wfflist linelist).

@IndexOther(LINE)@\
A line number.

@IndexOther(LINE-RANGE)@\
A range of lines from M through N, written M--N, where M and N 
are positive integers and M <= N.  As shortcuts, one may write M, which 
represents the range M--M;  M--, which stands for the range from line M 
through the last line of the current proof; and --N, which represents the 
range from the first line of the proof through line N.  Hence -- 
represents the range consisting of every line in the proof.

@IndexOther(LINE-RANGE-LIST)@\
A list of line ranges. See the help message for LINE-RANGE
for examples.

@IndexOther(LINELIST)@\
A list of lines.  Examples: (1 3 4), (), (25)

@IndexOther(LVARCONST)@\
A logical variable or constant, not a polymorphic symbol or abbreviation.

@IndexOther(OCCLIST)@\
	A list of occurrences (counted left-to-right) of a subwff in a wff.
	ALL refers to all occurrences of the subwff.

@IndexOther(PLINE)@\
Line number of an existing planned line.

@IndexOther(RLINE)@\
Dummy line definition for the rules packages.

@IndexOther(RLINELIST)@\
A list of dummy lines for the rules package.@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexOther(BOOLEAN-OR-ABBREVLIST)@\
T, NIL or a list of abbreviations.

@IndexOther(ETREE)@\
An expansion tree or a gwff.

@IndexOther(REWRITE-DEFNS-LIST)@\
A list whose first element is one of NONE, EAGER, LAZY1 and DUAL,
and whose other (optional) elements are lists whose first element is
one of these four options and whose other elements are the names of 
definitions.
The first element is the default behaviour for rewriting definitions,
and the other lists are lists of exceptions to this default, with a
different behaviour specified.
NONE:  do not rewrite this definition at all.
EAGER: rewrite all of these definitions, in one big step, as soon 
       as possible.
LAZY1: rewrite these, one step at a time, when there are no more
       EAGER rewrites to do.
DUAL: as LAZY1, but rewrite these abbreviations A to a conjunction of
       A and A, and then deepen only one of these conjuncts. (e.g.
       TRANSITIVE p becomes 
       TRANSITIVE p AND FORALL x y z . [pxy AND pyz] IMPLIES pxz
LAZY2: synonym for DUAL.

For example: the value
(EAGER)
would be interpreted as "Rewrite every definition in one step."

(DUAL (EAGER TRANSITIVE) (NONE INJECTIVE SURJECTIVE))
would be interpreted as "Rewrite TRANSITIVE whenever it appears.
Don't ever rewrite INJECTIVE or SURJECTIVE. Rewrite every other 
definition in the DUAL way."@End(Description)

@Section(Mtree Printing)

@Begin(Description)
@IndexOther(MATINGSTREE)@\
An expansion tree or a gwff.

@IndexOther(OBDEFAULT)@\
Should be one of DEEPEST, HIGHEST, D-SMALLEST or H-SMALLEST.
Used by the flag DEFAULT-OB.@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexOther(EPROOF)@\
An Expansion Proof

@IndexOther(GWFF0)@\
A reference to a wff of type O.
Currently any of: 
@Begin(description, spread 0.3)

EXPANSION TREES:@\etrees-labels 

FLAVORS OF LABELS:@\flavor-type 

MATING SEARCH:@\current-eproof-type last-eproof-type 

PROOF OUTLINE:@\line-number 

THEOREMS:@\theorem-type 

TOP LEVELS:@\dproof-line-ref rewriting-line-ref 

VPFORMS:@\jforms-labels 

WEAK LABELS:@\weak-type 

WFF EDITOR:@\edit-wff edwff-type last-edwff-type 

WFF PARSING:@\string-bound-var string-type 

WFF TYPES:@\wffop-type 
@End(description)

@IndexOther(GWFF0-OR-EPROOF)@\
Either a gwff of type O, CURRENT-EPROOF, LAST-EPROOF, an eproof, 
or a symbol which names an eproof.

@IndexOther(GWFF0-OR-LABEL-OR-EDAG)@\
Either a gwff of type O, an extensional expansion dag,
or a symbol which names an extensional expansion dag. If it is a symbol 
representing a gwff, getfn returns the symbol instead of the gwff. 
Checking type gwff0-or-label for more details.

@IndexOther(GWFF0-OR-LABEL-OR-EPROOF)@\
Either a gwff of type O, CURRENT-EPROOF, LAST-EPROOF, an eproof, 
or a symbol which names an eproof. If it is a symbol representing a gwff,
getfn returns the symbol instead of the gwff. Checking type gwff0-or-label
for more details.

@IndexOther(LEAFTYPE)@\
The type of leaf names; i.e. symbol, integer or a restricted set of reals.

@IndexOther(MATE-COMMAND)@\
A list with mating-search commands.

@IndexOther(MATINGPAIR)@\
A mating connection in the form (LEAFn . LEAFm). Actually, any dotted pair 
of symbols will do; it is up to the user to ensure that it's really a connection.

@IndexOther(MATINGPAIRLIST)@\
No more help available.  Sorry.

@IndexOther(MT-SUBSUMPTION)@\
Should be one of NIL, SUBSET-CONNS, SAME-CONNS, SAME-TAG, T. 
See the flag MT-SUBSUMPTION-CHECK.

@IndexOther(NAT-ETREE-VERSION-TYPE)@\
Should be one of OLD, HX, CEB

@IndexOther(QUERYTYPE)@\
Should be one of T, NIL, QUERY-SLISTS, SHOW-JFORMS or QUERY-JFORMS.
Used in the flag QUERY-USER.

@IndexOther(SEARCHTYPE)@\
Should be one of MS88, MS89, MS90-3, MS90-9, MS91-6, MS91-7, MS92-9, MS93-1, MT94-11, MT94-12, MT95-1, MS98-1, MS03-7, MS04-2@End(Description)

@Section(Unification)

@Begin(Description)
@IndexOther(VERBOSE)@\
Should be one of SILENT=NIL, MIN, MED, or MAX=T, used in the flag MATING-VERBOSE, UNIFY-VERBOSE.@End(Description)

@Section(Tactics)

@Begin(Description)
@IndexOther(TACTIC-EXP)@\
Either the name of a tactic or a compound tactic expression. 
Currently defined tactics are:
ALL+TAC ALL-TAC AND+TAC AND-TAC AUTO-TAC BOOK-TAC COMPLETE-TRANSFORM*-TAC COMPLETE-TRANSFORM-TAC CONTRACT-TAC DEC+TAC DIY-TAC ELIM-DEFNS-TAC EQFUNC-TAC EQO-TAC EQUIV+TAC EQUIV-TAC EQUIVWFFS+TAC EQUIVWFFS-TAC EUNIF1-TAC EUNIF2-TAC EXISTS+TAC EXISTS-TAC EXTFUNC+TAC EXTO+TAC FALSE-TAC GO2-TAC IMPLIES+TAC IMPLIES-TAC INIT-TAC INITEQ-TAC INTERNALIZE+TAC INTERNALIZE-TAC LAMBDA-TAC MIN-PROP MONSTRO-TAC NOT-TAC OR+TAC OR-TAC PFENNING*-TAC PFENNING-TAC PLINE-TAC PROP-ELIM-RULES-TAC PROP-INTRO-RULES-TAC REFL+TAC REWRITE-PLINE-P-TAC REWRITE-PLINE-TAC REWRITE-SLINE-TAC SLINE-TAC SUB=-TAC TRUE+TAC ABSURD-TAC BACKCHAIN-LEMMA-TAC ML::BASIC-PROP*-TAC BASIC-PROP-TAC CASES-TAC CLASS-DISJ-TAC DEDUCT-TAC DISJ-EQUIV-TAC DISJ-IMP-TAC ECONJ*-TAC ECONJ-TAC ENEG-TAC EQUIV-DISJ-TAC EQUIV-IMPLICS-TAC ICONJ*-TAC ICONJ-TAC IDISJ-LEFT-TAC IDISJ-RIGHT-TAC IDISJ-TAC IMP-DISJ-TAC IMPLICS-EQUIV-TAC INDIRECT-DISJ-PLINE-TAC INDIRECT-EXISTS-PLINE-TAC INDIRECT-TAC INDIRECT2-TAC INEG-TAC MP-TAC NEG-AND-ELIM-TAC NEG-AND-PLAN-TAC NEG-AND-SLINE-TAC NEG-ATOM-ELIM-TAC NEG-EQUAL-ELIM-TAC NEG-EXISTS-ELIM-DUP-TAC NEG-EXISTS-ELIM-SIMPLE-TAC NEG-IMP-ELIM-TAC NEG-IMP-PLAN-TAC NEG-IMP-SLINE-TAC NEG-NEG-ELIM-TAC NEG-NEG-PLAN-TAC NEG-NEG-SLINE-TAC NEG-OR-ELIM-DUP-TAC NEG-OR-ELIM-SIMPLE-TAC NEG-OR-PLAN-TAC NEG-OR-SLINE-TAC NEG-UNIV-ELIM-TAC OR-LEMMA-LEFT-TAC OR-LEMMA-RIGHT-TAC OR-LEMMA-TAC PROP-PRIM PROPOSITIONAL PULLNEG-TAC PUSHNEG-TAC RULEP-TAC SAME-TAC SUBST=-BACKCHAIN-LEMMA-TAC TRUTH-TAC ML::TRUTHP-REWRITE-PLAN-TAC AB-PLAN-TAC AB-SLINE-TAC ABU-TAC EDEF-TAC EGEN-TAC EXISTS-LEMMA-TAC IDEF-TAC NEG-EXP-PLAN-TAC NEG-EXP-SLINE-TAC NEG-SEL-PLAN-TAC NEG-SEL-SLINE-TAC QUANTIFICATIONAL RULEC-TAC RULEQ-PLAN-TAC RULEQ-SLINE-TAC SYMSIMP-TAC UGEN-TAC UI-HERBRAND-TAC UI-TAC UNNEC-EXP-TAC EQUALITY-PLAN-TAC EQUALITY-SLINE-TAC EXT=-PLAN-TAC EXT=-SLINE-TAC LEIBNIZ=-PLAN-TAC LEIBNIZ=-SLINE-TAC NEG-EQUAL-SLINE-TAC REFL=-TAC SUBST=-TAC SUBST=L-TAC SUBST=R-TAC SYM=-TAC EQUIV-WFFS-PLAN-TAC EQUIV-WFFS-SLINE-TAC ML::NEG-EQUIV-SLINE-TAC NEG-REW-PLAN-TAC NEG-REW-SLINE-TAC BETA-ETA-SEPARATE-TAC BETA-ETA-TOGETHER-TAC BETA-ONLY-TAC EQUIV-EQ-CONTR-TAC EQUIV-EQ-EXPD-TAC EXT=-TAC EXT=0-TAC LCONTR*-BETA-TAC LCONTR*-ETA-TAC LCONTR*-TAC LCONTR*-VARY-TAC LEXPD*-BETA-TAC LEXPD*-ETA-TAC LEXPD*-TAC LEXPD*-VARY-TAC DUPLICATE-SUPPORT-TAC FINISHED-P INESS-PLINE-TAC MAKE-NICE MAKE-ROOM NEG-PLINE-P-TAC NEG-SLINE-P-TAC NNF-TAC RESTRICT-MATING-TAC REWRITE-SLINE-P-TAC SHOW-CURRENT-PLAN SHOW-PLANS UNIVERSAL-GOAL-P UNSPONSOR-TAC USE-RULEP-TAC USE-SYMSIMP-TAC .

@IndexOther(TACTIC-MODE)@\
The mode in which a tactic will be used.  Allowable values are:
AUTO INTERACTIVE .

@IndexOther(TACTIC-USE)@\
The use to which a tactic will be put.  Allowable values are:
NAT-DED ETREE-NAT MATE-SRCH EXT-SEQ .@End(Description)

@Section(suggestions)

@Begin(Description)
@IndexOther(EXEC-FORM)@\
A list of GO instructions ((priority action) ...),
where each ACTION is either DO, ASK, SHOW or FORGET.

@IndexOther(GO-INSTRUCT)@\
A list of GO instructions ((priority action) ...),
where each ACTION is either DO, ASK, SHOW or FORGET.@End(Description)

@Section(Searchlists)

@Begin(Description)
@IndexOther(ANYTHING-LIST)@\
No more help available.  Sorry.@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexOther(JFORM)@\
A jform or a gwff.

@IndexOther(VPFORMAT)@\
T   = no atom values will show in VP diagram
A   = atom values but no labels will appear in VP diagram
NIL = atom values and labels will show in VP diagram
LT  = atom values and  labels and a legend will show in VP diagram 
L   = labels but no atom values will show in VP diagram,
      and a legend will show both
B   = boxed labels and atoms will show in the VP diagram.
BT  = boxed labels will show in the diagram, and the atom values
      will be listed below.
B and BT only work in TeX format (i.e. with the VPT command).

@IndexOther(VPSTYLE)@\
Styles supported for vertical path diagrams.
Currently any of CONCEPT, CONCEPT-S, SAIL, SCRIBE, SCRIBE-SLIDES, GENERIC.
(Use the VPT command to print in TEX style.)
The linelength associated with various SCRIBE fonts is: (8 99) (10 79) 
 (12 65) (14 56) (18 43).
The linelength associated with various SAIL fonts is: 	(4L 301) (4P 216)
(5L 240) (5P 172) (6L 199) (6P 143) (7L 171) (7P 123) (8L 149) (8P 107)
(9L 133) (9P 95) (10L 120) (10P 86) (12L 99) (12P 71) (14L 85) (14P 61)
(18L 66) (18P 47).  @End(Description)

@Section(Propositional Rules)

@Begin(Description)
@IndexOther(RULEP-MAINFN-TYPE)@\
A RuleP main function. Currently, one of the following:
RULEP-SIMPLE RULEP-DELUXE @End(Description)

@Section(Theorems)

@Begin(Description)
@IndexOther(BOOK-THEOREM)@\
A theorem proven in the book.

@IndexOther(EXERCISE)@\
An exercise which may be assigned.

@IndexOther(LIB-THEOREM)@\
A theorem loaded from a library.

@IndexOther(PRACTICE)@\
An unscored practice exercise.

@IndexOther(TEST-PROBLEM)@\
A potential test problem.

@IndexOther(THEOREM)@\
A theorem.  Exercises and practice exercises are theorems.

@IndexOther(THEOREMLIST)@\
A list of theorems.@End(Description)

@Section(Wff Editor)

@Begin(Description)
@IndexOther(ED-COMMAND)@\
A list with editor commands.
This is mainly useful as resulttype for editor operations like EDSEARCH.

@IndexOther(MSGLIST)@\
A list with message instructions a la UCI-Lisp's MSG function.
In addition it may contain pairs (item . argtype)

@IndexOther(MSGLISTLIST)@\
A list of message lists (see argument type MSGLIST).@End(Description)

@Section(Wff Types)

@Begin(Description)
@IndexOther(GVAR)@\
A gwff which must be a logical variable.
Currently any of: 
@Begin(description, spread 0.3)

EXPANSION TREES:@\etrees-labels 

FLAVORS OF LABELS:@\flavor-type 

MATING SEARCH:@\current-eproof-type last-eproof-type 

PROOF OUTLINE:@\line-number 

THEOREMS:@\theorem-type 

TOP LEVELS:@\dproof-line-ref rewriting-line-ref 

VPFORMS:@\jforms-labels 

WEAK LABELS:@\weak-type 

WFF EDITOR:@\edit-wff edwff-type last-edwff-type 

WFF PARSING:@\string-bound-var string-type 

WFF TYPES:@\wffop-type 
@End(description)

@IndexOther(GVARLIST)@\
A list of variables.
Currently any of: 
@Begin(description, spread 0.3)

EXPANSION TREES:@\etrees-labels 

FLAVORS OF LABELS:@\flavor-type 

MATING SEARCH:@\current-eproof-type last-eproof-type 

PROOF OUTLINE:@\line-number 

THEOREMS:@\theorem-type 

TOP LEVELS:@\dproof-line-ref rewriting-line-ref 

VPFORMS:@\jforms-labels 

WEAK LABELS:@\weak-type 

WFF EDITOR:@\edit-wff edwff-type last-edwff-type 

WFF PARSING:@\string-bound-var string-type 

WFF TYPES:@\wffop-type 
@End(description)

@IndexOther(GWFF)@\
A reference to a wff.
Currently any of: 
@Begin(description, spread 0.3)

EXPANSION TREES:@\etrees-labels 

FLAVORS OF LABELS:@\flavor-type 

MATING SEARCH:@\current-eproof-type last-eproof-type 

PROOF OUTLINE:@\line-number 

THEOREMS:@\theorem-type 

TOP LEVELS:@\dproof-line-ref rewriting-line-ref 

VPFORMS:@\jforms-labels 

WEAK LABELS:@\weak-type 

WFF EDITOR:@\edit-wff edwff-type last-edwff-type 

WFF PARSING:@\string-bound-var string-type 

WFF TYPES:@\wffop-type 
@End(description)

@IndexOther(GWFF-ILL)@\
A reference to a well- or ill-formed formula.

@IndexOther(GWFF0-OR-LABEL)@\
A reference to a wff of type O. If the gwff0 is a label
the getfn will give the label name instead of the wff represented
by the label.
Currently any of: 
@Begin(description, spread 0.3)

EXPANSION TREES:@\etrees-labels 

FLAVORS OF LABELS:@\flavor-type 

MATING SEARCH:@\current-eproof-type last-eproof-type 

PROOF OUTLINE:@\line-number 

THEOREMS:@\theorem-type 

TOP LEVELS:@\dproof-line-ref rewriting-line-ref 

VPFORMS:@\jforms-labels 

WEAK LABELS:@\weak-type 

WFF EDITOR:@\edit-wff edwff-type last-edwff-type 

WFF PARSING:@\string-bound-var string-type 

WFF TYPES:@\wffop-type 
@End(description)

@IndexOther(GWFFALIST)@\
A list of substitutions for meta-variables.

@IndexOther(GWFFLIST)@\
A list of GWFFs, used for lists of expansions terms.

@IndexOther(GWFFPAIR)@\
A pair of GWFFs. In unification, a disagreement pair.

@IndexOther(GWFFPAIRLIST)@\
A list of GWFFPAIRs. In unification, a disagreement set.

@IndexOther(OCC-LIST)@\
A list of positive integers or ALL.

@IndexOther(ORDERCOM)@\
This specifies the value of order-components for mating search.

@IndexOther(TYPEALIST)@\
An a-list of type symbols.

@IndexOther(TYPESYM)@\
The string representation of a type.

@IndexOther(TYPESYM-CONS)@\
A cons-cell of type symbols.

@IndexOther(TYPESYM-NIL)@\
The string representation of a type or NIL.

@IndexOther(TYPESYMLIST)@\
A list of string representations of types.

@IndexOther(TYPESYMLIST-NIL)@\
A list of type symbols or NIL.

@IndexOther(WFFSET)@\
A symbol standing for a set of wffs in a hypothesis.@End(Description)

@Section(Rewriting commands)

@Begin(Description)
@IndexOther(RRULE)@\
A rewrite rule.
Currently any of: 
@Begin(description, spread 0.3)

@End(description)

@IndexOther(RRULELIST)@\
A list of rewrite rules.

@IndexOther(THEORY)@\
A theory.
Currently any of: 
@Begin(description, spread 0.3)

@End(description)
@End(Description)

@Section(Substitution)

@Begin(Description)
@IndexOther(REPSYM)@\
A replaceable symbol.@End(Description)

@Section(Basic Abbreviations)

@Begin(Description)
@IndexOther(REWRITE-DEFNS)@\
One of the following:
NONE: do not rewrite equalities
ONLY-EXT: rewrite only those equalities that can be rewritten using
          extensionality.
LEIBNIZ: rewrite all equalities using the Leibniz definition.
ALL: rewrite all equalities, using extensionality where possible
     and the Leibniz definition otherwise.
DUAL: As in the flag REWRITE-DEFNS.
PARITY1: Uses the parity to determine whether equalities should be
    rewritten as the setting LEIBNIZ or as the setting ALL.  For example,
    using PARITY1 when trying to prove the wff 
              A(OI) = B(OI) implies C
    the equality is expaned using Leibniz, and when trying to prove the wff
              D implies A(OI) = B(OI)
    the equality is expanded using extensionality.  The heuristic
    is that we often use the substitutivity property when we use an equation
    and use extensionality to show an equation.@End(Description)

@Section(Skolemizing)

@Begin(Description)
@IndexOther(AUTO-SEARCHTYPE)@\
Should be one of SIMPLE, BIDIR, BIDIR-SORTED.

@IndexOther(GWFF-OR-LABEL)@\
A reference to a wff. If the gwff is a label the getfn will
give the label name instead of the wff represented by the label.

@IndexOther(GWFF-OR-NIL)@\
A reference to a wff or NIL.

@IndexOther(GWFF-OR-SELECTION)@\
A selection from a number of given wffs or a reference to a wff.

@IndexOther(LINE-GE-2)@\
A line number >=2.

@IndexOther(REL-OR-LABEL)@\
A reference to a relation. If the relation is a label
the getfn will give the label name instead of the wff represented
by the label.

@IndexOther(SUBST-ALIST)@\
List of (gvar . gwff) pairs.

@IndexOther(SUBST-PAIR)@\
Means substitute gwff for gvar.

@IndexOther(SYMBOL-DATA-LIST)@\
List of (SYMBOL . <anything>) pairs.

@IndexOther(SYMBOL-DATA-PAIR)@\
A (SYMBOL . <data>) pair@End(Description)

@Section(Grader)

@Begin(Description)
@IndexOther(CONSP1)@\
A list.

@IndexOther(FUNCTION)@\
A function.@End(Description)

@Section(Maintenance)

@Begin(Description)
@IndexOther(SYMBOLPAIR)@\
The type of a dotted pair of symbols.

@IndexOther(SYMBOLPAIRLIST)@\
The type of a list of dotted pairs of symbols@End(Description)

@Section(Basics)

@Begin(Description)
@IndexOther(ANYTHING)@\
Any legal LISP object.

@IndexOther(BOOLEAN)@\
A Boolean value (NIL for false, T for true).

@IndexOther(INTEGER+)@\
A nonnegative integer.

@IndexOther(INTEGER+-OR-INFINITY)@\
A nonnegative integer or the symbol INFINITY.

@IndexOther(NULL-OR-INTEGER)@\
NIL or a nonnegative integer.

@IndexOther(NULL-OR-POSINTEGER)@\
NIL or a positive integer.

@IndexOther(POSINTEGER)@\
A positive integer.

@IndexOther(POSINTEGER-OR-INFINITY)@\
A positive integer or the symbol INFINITY.

@IndexOther(POSINTEGERLIST)@\
No more help available.  Sorry.

@IndexOther(POSNUMBER)@\
A positive number of any kind.

@IndexOther(STRING)@\
A string enclosed in double-quotes.

@IndexOther(SYMBOL)@\
Any legal LISP symbol (must be able to have property list).

@IndexOther(SYMBOL-OR-INTEGER)@\
Any legal LISP symbol (must be able to have property list) or integer.

@IndexOther(UPDOWN)@\
u or up for Up, d or down for Down.

@IndexOther(YESNO)@\
y or yes for YES, n or no for NO.@End(Description)

@Section(Modules)

@Begin(Description)
@IndexOther(MODULELIST)@\
A list of modules.
Currently any of: 
@Begin(description, spread 0.3)

MODULES:@\auto-basic auto-doc bare bootstrap concept-bare concept-wff environment etps-events etr-nat event-signal events expansion-tree ext-dags external-interface external-services file-ops grader grader-top jforms lambda-calc library logic-scribe maintain math-logic-1 math-logic-1-rules math-logic-1-wffs math-logic-2 math-logic-2-exercises math-logic-2-rules math-logic-2-wffs mating mating-transform metawffs ml-etr-tactics ml-tactics ml2-rewrite mode-ml ms88 ms89 ms90-3 ms90-9 ms91 ms98 mst ops-otlrules otladvice otlcleanup otlgo otlhelp otlnl otlrulep otlrules otlschema2 otlscribe otlsuggest primitive-subst read-rules replace report review-flags rrules rules s-eqn sail-wff save-tps-work save-wffs saving-modes scribe-wff semantics skolemizing tactics tactics-nd tex-wff theorems tps-help tps-modules tps2-rulep tpsdef unification unification-interface vpforms weak-label wff-editor wff-ops-abb wff-ops1 wff-ops2 wff-parse wff-print wffmatch wffs xwindows 
@End(description)

@IndexOther(TPS-MODULE)@\
A module.
Currently any of: 
@Begin(description, spread 0.3)

MODULES:@\auto-basic auto-doc bare bootstrap concept-bare concept-wff environment etps-events etr-nat event-signal events expansion-tree ext-dags external-interface external-services file-ops grader grader-top jforms lambda-calc library logic-scribe maintain math-logic-1 math-logic-1-rules math-logic-1-wffs math-logic-2 math-logic-2-exercises math-logic-2-rules math-logic-2-wffs mating mating-transform metawffs ml-etr-tactics ml-tactics ml2-rewrite mode-ml ms88 ms89 ms90-3 ms90-9 ms91 ms98 mst ops-otlrules otladvice otlcleanup otlgo otlhelp otlnl otlrulep otlrules otlschema2 otlscribe otlsuggest primitive-subst read-rules replace report review-flags rrules rules s-eqn sail-wff save-tps-work save-wffs saving-modes scribe-wff semantics skolemizing tactics tactics-nd tex-wff theorems tps-help tps-modules tps2-rulep tpsdef unification unification-interface vpforms weak-label wff-editor wff-ops-abb wff-ops1 wff-ops2 wff-parse wff-print wffmatch wffs xwindows 
@End(description)
@End(Description)

@Section(Rules Module)

@Begin(Description)
@IndexOther(RULE)@\
A rule that has been defined through DEFIRULE.
Currently any of: 
@Begin(description, spread 0.3)

@End(description)
@End(Description)

@Section(Lisp packages)

@Begin(Description)
@IndexOther(LISP-PACKAGE)@\
A LISP package known to TPS.  Currently any of: 
@Begin(description, spread 0.3)

LISP PACKAGES:@\auto core maint ml teacher 
@End(description)

@IndexOther(LISP-PACKAGE-LIST)@\
A list of Lisp packages known to TPS.@End(Description)

@Section(Library)

@Begin(Description)
@IndexOther(FILESPECLIST)@\
No more help available.  Sorry.

@IndexOther(GWFF-PROP)@\
One of the following properties of gwffs:
ALL : true for all gwffs
FIRST-ORDER : true for first-order gwffs
SK-FIRST-ORDER : true for gwffs that are first-order after skolemizing.
HIGHER-ORDER : true for non-first-order gwffs
SK-HIGHER-ORDER : true for gwffs that are non-first-order after skolemizing.
WITH-EQUALITY : true of gwffs that contain an equality
WITH-DEFN : true of gwffs that contain a definition
PROVEN : true of gwffs that have been marked as proven in the library
UNPROVEN : true of all gwffs that aren't PROVEN
AUTO-PROOF : true of all gwffs with automatic or semi-automatic proofs.

For the first- and higher-order checks, equalities are rewritten as specified
by the flag REWRITE-EQUALITIES; if any equalities remain in the gwff after 
rewriting, these are considered first-order if they are equalities between
base types.

@IndexOther(GWFF-PROP-LIST)@\
A list of some of the following properties of gwffs:
ALL : true for all gwffs
FIRST-ORDER : true for first-order gwffs
SK-FIRST-ORDER : true for gwffs that are first-order after skolemizing.
HIGHER-ORDER : true for non-first-order gwffs
SK-HIGHER-ORDER : true for gwffs that are non-first-order after skolemizing.
WITH-EQUALITY : true of gwffs that contain an equality
WITH-DEFN : true of gwffs that contain a definition
PROVEN : true of gwffs that have been marked as proven in the library
UNPROVEN : true of all gwffs that aren't PROVEN
AUTO-PROOF : true of all gwffs with automatic or semi-automatic proofs.

For the first- and higher-order checks, equalities are rewritten as specified
by the flag REWRITE-EQUALITIES; if any equalities remain in the gwff after 
rewriting, these are considered first-order if they are equalities between
base types.

@IndexOther(KEYWORD-LIST)@\
A list of keywords.  Use show-keywords to see a list of known keywords.

@IndexOther(KEYWORD-PROP)@\
A keyword used to signify that a gwff has a certain property.
Use show-keywords to see a list of known keywords.

@IndexOther(LIB-ARGTYPE)@\
Type of object that can be stored in the library.
Currently any of: 
@Begin(description, spread 0.3)

LIBRARY:@\slist 

MISCELLANEOUS:@\abbr class-scheme dpairset gwff lib-const mode mode1 modes-gwffs rrule theory 
@End(description)

@IndexOther(LIB-ARGTYPE-LIST)@\
A list of lib-argtypes; see the help message for LIB-ARGTYPE.

@IndexOther(LIB-ARGTYPE-OR-NIL)@\
NIL or Type of object that can be stored in the library.
Currently any of: 
@Begin(description, spread 0.3)

LIBRARY:@\slist 

MISCELLANEOUS:@\abbr class-scheme dpairset gwff lib-const mode mode1 modes-gwffs rrule theory 
@End(description)

@IndexOther(LIBCLASS)@\
A libclass is a directed acyclic graph (DAG) classifying objects
in the library.  A classification scheme for the library is a libclass
along with a direction (up or down).

@IndexOther(STRINGLIST)@\
A list of strings.

@IndexOther(STRINGLISTLIST)@\
A list of lists of strings.

@IndexOther(TPSFLAGLIST)@\
No more help available.  Sorry.@End(Description)

@Section(Best modes)

@Begin(Description)
@IndexOther(SHORT-DATE)@\
A valid date, in the form YYYYMMDD. YYYY must be >= 1900. Any
non-integer charactes will be ignored (so 1999-04-12, 1999/04/12 and 19990412
are all considered the same, and are all valid).@End(Description)
@ChapterPh(Utilities)
The internal name of this category is 
UTILITY.
An utility can be defined using DEFUTIL.
Allowable properties are: @t{FORM-TYPE}, @t{KEYWORDS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(PROMPT-READ)@\
PROMPT-READ is the canonical way of doing input in TPS.
It provides argument type checking, a default mechanism and options
which allow ? and ?? help and arbitrarily many other special responses.
Its form is
@Begin(LispCode)

 (PROMPT-READ internal-var external-var
              initial-message-form argument-type default-value
              ((response form ...) (response form ...)))
@End(LispCode)
@\
@Begin(description)

internal-var will hold the internal representation of the user's response
after the input.

external-var will hold the external representation of what the user typed.
If external-var = NIL, the external form of the input is thrown away.

initial-message-form is evaluated and should somehow output the initial
part of the prompt.

argument-type is the type of the object that the user is supposed to input.
Common here is 'YESNO

default-value is the internal representation of the default for the input.
A default-value of $ means that there is no default.

((response form ...) (response form ...)) are forms to handle special responses
like ?, ?? or perhaps <Esc>.  response is either a single symbol or a list
of symbols and form ... are evaluated in case one of the corresponding
    responses has been typed.  A common use is
@\
@Begin(LispCode)

 ((? (msgf "Please decide whether you want to see any more news."))
  (?? (mhelp 'yesno)))
@End(LispCode)
@End(Description)

@\Here is a complete example of a use of PROMPT-READ within an initialization
dialogue:
@\
@Begin(LispCode)

  (let (ldefp)
    (prompt-read
      ldefp nil
      (msgf "Load private definitions? ")
      'yesno 'nil
      ((? (msgf "Load PPS:DEFS and PPS:MODES.INI ?"))
       (?? (mhelp 'yesno)))))
    
    (when ldefp (lload "pps:defs") (lload "pps:modes.ini")))
@End(LispCode)

@IndexOther(QUERY)@\
QUERY is the canonical way of obtaining a yes-no response
from the user. It calls PROMPT-READ with appropriate arguments. The only
difference between these two macros is that prompt-read sets a variable,
while query just returns T or nil. Its form is
@Begin(LispCode)

       (query initial-message-form default-value)
@End(LispCode)
@\
@Begin(description)

initial-message-form is evaluated and should somehow output the initial
part of the prompt.

default-value is the internal representation of the default for the input.
A default-value of $ means that there is no default.
@End(Description)
@End(Description)

@Section(Review)

@Begin(Description)
@IndexOther(IN-MODE)@\
(IN-MODE mode form1 ... formn) is an extremely useful macro.
It will locally bind all flags and parameters affected by mode to the
value in the mode and the execute form1 ... formn.  Note that no initfn
is called when the flags are set.  Note also that the macro (of course!)
is expanded at compile-time and therefore changing the definition of
mode will have no effect on the execution of form1 ... formn until
the IN-MODE is recompiled or loaded in uncompiled form.  Examples
of uses are
 (IN-MODE SCRIBE-DOC (MSG (A . GWFF))) will print the gwff A in style
Scribe.
 (IN-MODE RE-READ (MSG (A . GWFF))) will print the gwff A in such a
way that it can be read back.

@IndexOther(PCALL)@\
(PCALL operation arg1 ... argn)
is used inside functions whose output depends on the current value of
the STYLE parameter.  operation is typically something like PRINT-TYPESYM,
or BEGIN-ENVIRONMENT.  It expands in such a way that all the styles known
at compile-time are compiled in-line, but it will also work for styles
defined later, e.g. when another package is loaded.  arg1 ... argn are
handed to the function which is supposed to perform operation in the 
current style.  If an operation has not been defined for a particular
style, a THROWFAIL with an appropriate error message will be done.@End(Description)

@Section(Flags)

@Begin(Description)
@IndexOther(ANALYZE-FLAG-DEPENDENCIES)@\
Analyze the functions defined in TPS lisp files
and print a list of search related flags and conditions under 
which they are relevant.

@IndexOther(UPDATE-FLAG)@\
(UPDATE-FLAG flag)
is used to give the user a chance to change a flag or parameter.
The user will be prompted for a new value of flag, the default being
its current value.  This is useful in initialization dialogues.  For example:
(update-flag 'style)
will prompt the user for a style.  It the user simply types return, it will
be unchanged.@End(Description)

@Section(Collecting Help)

@Begin(Description)
@IndexOther(PRINT-HTML)@\
PRINT-HTML outputs help messages in HTML format, with links to the
other help messages in TPS. It takes three arguments:
  PRINT-HTML "arbitrary string" "/home/theorem/tps/doc/htmldoc/" ignore-tags
where the first argument is any string and the second argument is a prefix 
which should be a string containing the URL of the home directory of the 
TPS documentation. The third argument is optional, defaulting to NIL; 
if it's set to T, then PRINT-HTML will attempt to preserve existing HTML tags 
in the input string while still producing correct HTML output; if NIL, it won't 
try to do this. Output is produced on the screen, using the MSG command; it's 
up to the user to redirect it to a file (see the help messages for 
REROUTE-OUTPUT and REROUTE-OUTPUT-APPEND) or a string (using the lisp 
function (with-output-to-string (*standard-output*) <form>)).

For example:
PRINT-HTML "The flag NUM-OF-DUPS" "/users/foo"
 will return 
The flag <a href="/users/foo/num-of-dups.html">NUM-OF-DUPS</a>

The URL prefix should usually be the localised version given 
above, but if you're running this on a system outside CMU and you want to 
link to the documentation at CMU, use the prefix 
"http://gtps.math.cmu.edu/htmldoc/" instead.@End(Description)

@Section(Starting and Finishing)

@Begin(Description)
@IndexOther(REROUTE-OUTPUT)@\
REROUTE-OUTPUT is the canonical way of routing output of TPS
to a file exclusively.  (REROUTE-OUTPUT filename default form1 ... formn)
will open a file filename using default for figuring out parts of filename
which were not specified.  It then executes form1 ... formn such that
all output goes to filename.  Note that you can still send messages to the
terminal with COMPLAIN or TTYMSG, but MSG output will go to filename.
When the writing is completed, a message with the true filename will
be printed. If you want to suppress this message, set REROUTE-CLOSE-MESSAGE
to NIL.  Please think about the defaults, but if you want to use
the (most likely wrong) CLISP default, just use the global variable
*default-pathname-defaults*.

@IndexOther(REROUTE-OUTPUT-APPEND)@\
REROUTE-OUTPUT-APPEND is like REROUTE-OUTPUT, but appends to the end
of the file rather than superseding it, if it already exists.

@IndexOther(STRINGDT)@\
(STRINGDT) prints out the date and time to the current output stream
(usually the terminal), and then returns NIL. (STRINGDT stream) directs
the output to some other stream, and (STRINGDT nil) prints nothing and 
returns a string containing the date and time.

@IndexOther(STRINGDTL)@\
(STRINGDTL) prints out a newline followed by the date and time to 
the current output stream (usually the terminal), and then returns NIL. 
(STRINGDTL stream) directs the output to some other stream, and 
(STRINGDTL nil) prints nothing and returns a string containing a newline 
followed by the date and time.@End(Description)

@Section(Predicates on Wffs)

@Begin(Description)
@IndexOther(DEFWFFTEST)@\
DEFWFFTEST expands to a DEFWFFOP, where certain attributes are
given defaults.  Its intended use is for predicates on wffs.
(DEFWFFTEST tps-object &rest props)
 will set RESULTTYPE to BOOLEAN, ARGTYPES to (GWFF-ILL) and ARGNAMES to
(GWFF).  Additional properties may be defined and defaults overridden
through props.@End(Description)

@Section(Wff Types)

@Begin(Description)
@IndexOther(PRTWFF)@\
(PRTWFF gwff (flag1 value1) ... (flagn valuen))  is the one of
the two canonical ways of printing wffs in TPS.  It will bind flag1 to
value1 etc. and then print gwff.  This is useful to write commands or
functions which print gwff in a particular style.  For example
(PRTWFF A (USE-DOTS NIL) (PRINTDEPTH 0))
will print the wff A without using dots and showing all levels.
The other way of printing wffs with MSG is (MSG (A. GWFF)).
If a certain combination of flag settings is used more than once,
consider using (DEFMODE USEFUL-MODE ...) and (IN-MODE USEFUL-MODE (PRTWFF A))
instead.@End(Description)

@Section(Basics)

@Begin(Description)
@IndexOther(%CATCH%)@\
This is the old UCI-Lisp CATCH.
See the UCI-Lisp Manual for documentation.

@IndexOther(%THROW%)@\
This is the old UCI-Lisp THROW.
See the UCI-Lisp Manual for documentation.

@IndexOther(COMPLAIN)@\
COMPLAIN is the canonical way of announcing an error by the user.
(COMPLAIN msg1 ... msgn) will ring a bell at the terminal, and then call
(MSG msg1 ... msgn) after making sure that the messages go to the terminal 
only. 

@IndexOther(COPY)@\
(COPY sexpr) will recursively copy the whole sexpr.  For something
less dramatic see also COPY-LIST.

@IndexOther(DEFCONSTYPE)@\
Like DEFLISTTYPE, but for cons-cells rather than lists. 

@IndexOther(DEFLISTTYPE)@\
DEFLISTTYPE is a macro that expands into a deftype%.

@\(DEFLISTTYPE list-type single-type rest-props) defines the type of
lists with elements of type single-type.  rest-props can be used to
override any inherited attributes from the single-type, typically
used for the MHELP property.

@\In an alternative form, one can write (DEFLISTTYPE list-type
single-type (OTHER-KEYS (test form ...) (test form ...) ...) rest-props)
where form ... is executed if the corresponding test is non-NIL.
The variable list-type will hold the typed expression.  If none of
the tests is true, the usual will be done.

@IndexOther(FOR-EACH)@\
(FOR-EACH mapfn varlist list1 ... listn form1 ...)
is an iteration macro which applied mapfn (if omitted MAP) to
list1 ... listn, binding in turn each variable in varlist, then
executing form1 ...  It is roughly equivalent to
(mapfn #'(lambda varlist form1 ...) list1 ... listn)

@IndexOther(MSG)@\
MSG is the canonical way of producing text output, error or warning
messages etc.  It has the general form (MSG item1 ... itemn) where each item
can be one of the following forms:
T -> (TERPRI)
F -> (FRESH-LINE) ((TERPRI), but only of not at beginning of line)
THROW -> print (again using MSG) value of most recent THROW, usually THROWFAIL
(T n) -> (TAB n)
(TX n) -> (TABX n) (tabs without using <tab> characters)
(E form) -> evaluates form without printing the result
(L list) -> (PRINLC list) (print list without outermost parens)
(form . argtype) -> calls the printfn for argtype on form.  This is extremely
                    useful for wffs, lines, type symbols etc.
n, n>0 -> (SPACES n)
n, n<0 -> -n times (TERPRI)
otherwise -> (PRINC otherwise)

@IndexOther(MSGF)@\
(MSGF ...) expands to (MSG F ...).  It does a (FRESH-LINE) and then
calls MSG on the arguments.

@IndexOther(SET-OF)@\
(SET-OF var list form1 ... formn)
will take list and build a new list, in which every element which does not
satisfy form1 ... formn will be deleted.  E. g.
(SET-OF X '(0 1 -1 2 1 -2) (> X 0)) -> (0 1 2 1)

@IndexOther(THROWFAIL)@\
THROWFAIL is the canonical way of signalling errors in TPS.
The format is (THROWFAIL msg1 ... msgn) where msg1 ... msgn are
instructions for MSG.  See there.

@IndexOther(TPS-WARNING)@\
WARNING is the canonical way of warning the user.
(WARNING msg1 ... msgn) will call (MSG T "Warning:  " msg1 ... msgn) after
making sure that the messages go to the terminal only.

@IndexOther(TTYMSG)@\
(TTYMSG msg1 ... msgn) will call (MSG msg1 ... msgn) after
making sure that the messages go to the terminal only.@End(Description)
@ChapterPh(Wff Operations)
The internal name of this category is 
WFFOP.
A wff operation can be defined using DEFWFFOP.
Allowable properties are: @t{ARGTYPES}, @t{WFFARGTYPES}, @t{WFFOP-TYPELIST}, @t{ARGNAMES}, @t{RESULTTYPE}, @t{WFFOP-TYPE}, @t{ARGHELP}, @t{DEFAULTFNS}, @t{MAINFNS}, @t{APPLICABLE-Q}, @t{APPLICABLE-P}, @t{REPLACES}, @t{PRINT-OP}, @t{MULTIPLE-RECURSION}, @t{MHELP}.

@Section(OTL Object)

@Begin(Description)
@IndexOther(MATCH) @i{wffschema} @i{gwff}@\
Test whether a wff matches a wff schema.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(PRW) @i{gwff}@\
Print real wff. Turns off special characters
(including FACE definitions), infix notation, and dot 
notation, and then prints the wff.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(DISPLAY-ETREE) @i{gwff}@\
Etree Display: print an expansion tree into list form,
printing shallow formulas for leaf nodes only. The format used is
NODE [selection and expansion terms] ; CHILDREN or SHALLOW FORMULA

@IndexOther(DISPLAY-ETREE-ALL) @i{gwff}@\
Etree Print: print an expansion tree into list form, 
printing shallow formulas for all nodes. The format used is 
NODE [selection and expansion terms] ; CHILDREN ; SHALLOW FORMULA

@IndexOther(ETREE-TO-LIST) @i{gwff}@\
Print an expansion tree into list form.

@IndexOther(PNODE) @i{gwff}@\
Print the current node

@IndexOther(PPROOF) @i{gwff}@\
Print the current proof.

@IndexOther(PPW) @i{gwff}@\
Pretty-print a wff.

@IndexOther(PPWDEEP) @i{gwff}@\
Pretty-print the deep formula of an expansion tree.

@IndexOther(PW) @i{gwff}@\
Print a wff using the global settings of all flags.

@IndexOther(PWDEEP) @i{gwff}@\
Print the deep formula of an expansion tree.

@IndexOther(PWNODE) @i{gwff}@\
Print an expansion tree with node-names.

@IndexOther(PWSCOPE) @i{gwff}@\
Print a wff showing all brackets and dots.

@IndexOther(PWSHALLOW) @i{gwff}@\
Print the shallow formula of an expansion tree.

@IndexOther(PWTYPES) @i{gwff}@\
Print a wff showing types.

@IndexOther(TR-PRINT-ETREE) @i{gwff}@\
Print out the etree below the current topnode, showing expansion
variables, skolem terms, selection terms, and rewrite justifications.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider, or use SHOWNOTYPES. See also PTREE*

@IndexOther(TR-PRINT-ETREE*) @i{gwff}@\
Print out the etree below the current topnode, showing expansion
variables, skolem terms, selection terms, and rewrite justifications. For
all other nodes, show the shallow formula at that node.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider, or use SHOWNOTYPES. See also PTREE

@IndexOther(TR-PRINT-ETREE-FILE*) @i{etree} @i{file} @i{width} @i{fmlas}@\
As for PTREE or PTREE*, but send the output to a file.
For a width of 200 characters, you can print the results using
some variant of the following:
"enscript -r -fCourier-Bold6 -dberyl <filename> "@End(Description)

@Section(Internal for Printing)

@Begin(Description)
@IndexOther(PRT-APLICN-P) @i{gwff}@\
Decides if a given wff is not printed as a symbol.

@IndexOther(PRT-ASSOCIATIVE-P) @i{gwff}@\
Returns T, if gwff prints as an associative operator, NIL otherwise.

@IndexOther(PRT-INFIX-OP) @i{gwff}@\
Returns NIL, if the argument is not an infix operator,
its binding priority otherwise.

@IndexOther(PRT-PREFIX-OP) @i{gwff}@\
Returns NIL, if the argument is not a declared prefix operator,
its binding priority otherwise.

@IndexOther(PRT-SYMBOL-P) @i{gwff}@\
Decides if a given wff is printed a symbol.@End(Description)

@Section(Weak Labels)

@Begin(Description)
@IndexOther(CREATE-WEAK) @i{label} @i{gwff}@\
Assigns a label to the edwff, but does not change the edwff. You can
use the label to refer to this wff later.

@IndexOther(DELETE-WEAK) @i{label} @i{gwff}@\
Replace a weak label by the wff it represents.

@IndexOther(DISSOLVE-WEAK) @i{gwff}@\
Replace a top level occurrence of the label by the wff it represents.

@IndexOther(DISSOLVE-WEAK*) @i{gwff}@\
Replace all labels in a wff by the wffs represented by them.

@IndexOther(REDEF-WEAK) @i{label} @i{gwff}@\
Makes current edwff the new value of label (which must 
already exist).@End(Description)

@Section(Saving Wffs)

@Begin(Description)
@IndexOther(SV-WFF) @i{label} @i{gwff}@\
Save a wff by appending it to the file SAVEDWFFS. The 
weak label name should not already exist (if it does, remove it
using RW). The wffs that are saved to this file can be reloaded
using the command QLOAD "savedwffs.lisp".
  This command dates from before the LIBRARY top level was 
introduced; you should probably avoid it. If you want to save 
a gwff, use CW to create a weak label, then go into the library
with LIB and use INSERT to save the wff.@End(Description)

@Section(Recording)

@Begin(Description)
@IndexOther(REMARK-PRINTEDTFILE) @i{rm}@\
Write a remark into the PRINTEDTFILE.

@IndexOther(REMARK-PRINTMATEFILE) @i{rm}@\
Write a remark into the PRINTMATEFILE.@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexOther(APPLY-PRIM-SUBS) @i{gwff}@\
Apply primitive substitutions at an expansion node.

@IndexOther(APPLY-PRIM-SUBS-ALL) @i{gwff}@\
Apply primitive substitutions at all outermost expansion nodes.

@IndexOther(APPLY-PRIM-SUBS-OUTER) @i{gwff}@\
Apply primitive substitutions at all outer expansion nodes.

@IndexOther(DEEPEN-ETREE) @i{etree}@\
Deepen every leaf node of an expansion tree.

@IndexOther(DEEPEN-ONE) @i{leaf}@\
Deepen a single leaf of an expansion tree.

@IndexOther(DEEPEN-TO-LITERALS) @i{gwff}@\
Iteratively deepen an expansion tree until all leaves are
literals.

@IndexOther(DEEPEN=) @i{etree}@\
Deepen top level equality in the etree.

@IndexOther(DUPLICATE-ALL-OUTER-VARS) @i{gwff}@\
Duplicate all outermost variables in an expansion tree.

@IndexOther(DUPLICATE-ALL-VARS) @i{gwff}@\
Duplicate all variables in an expansion tree.

@IndexOther(EXPAND) @i{term} @i{etree}@\
EXPAND a given universal or existential quantifier.

@IndexOther(GWFF-TO-ETREE-SUB) @i{gwff} @i{skolemize} @i{deepen}@\
Create an expansion tree from a gwff0.

@IndexOther(MODIFY-STATUS) @i{status} @i{etree}@\
Set the status of the current-topnode to the specified
value. If the status of a node is not positive, it is ignored during
mating search.

@IndexOther(NAME-PRIMSUBSTS2) @i{etree}@\
This is exactly the same function as name-primsubsts, but applies
to etrees rather than gwffs.

@IndexOther(PRIM-SINGLE) @i{subst} @i{var} @i{etree}@\
Applies a single primsub. These can be generated by using
the NAME-PRIM command. The command PRIM-SINGLE destructively alters
the etree and creates a new jform, and is basically equivalent to
SUB-ETREE followed by DP* and CJFORM. The variable must be specified
in full detail, with both superscript and type, as in the vpform 
(e.g. "r^1(ob(ob))").

@IndexOther(RESTORE-ETREE) @i{loadfile}@\
Loads an etree and makes this the current etree.

Example of how to use SAVE-ETREE for X2106 and later use RESTORE-ETREE:
<3>MATE x2106
<Mate4>GO
<Mate5>MERGE-TREE
<Mate6>SAVE-ETREE
SAVEFILE (FILESPEC): File in which to save the etree ["x2106.etr"]>
Later come back into TPS and do the following:
<0>MATE x2108 (or MATE whatever)
<Mate1>RESTORE-ETREE
LOADFILE (FILESPEC): File in which to load the etree ["x2108.etr"]>"x2106.etr"
<Mate2>GO
<Mate3>LEAVE
Merge the expansion tree? [Yes]>Y
Now ETREE-NAT should work.


@IndexOther(SAVE-ETREE) @i{savefile}@\
Converts the current etree to an internal representation and saves this to a file.
This currently only works for etrees generated with SKOLEM-DEFAULT nil.

Example of how to use SAVE-ETREE for X2106 and later use RESTORE-ETREE:
<3>MATE x2106
<Mate4>GO
<Mate5>MERGE-TREE
<Mate6>SAVE-ETREE
SAVEFILE (FILESPEC): File in which to save the etree ["x2106.etr"]>
Later come back into TPS and do the following:
<0>MATE x2108 (or MATE whatever)
<Mate1>RESTORE-ETREE
LOADFILE (FILESPEC): File in which to load the etree ["x2108.etr"]>"x2106.etr"
<Mate2>GO
<Mate3>LEAVE
Merge the expansion tree? [Yes]>Y
Now ETREE-NAT should work.


@IndexOther(SEL-EXP-TERMS) @i{gwff}@\
Get the expansion terms of an expansion node or the
selected variable of a selection node.

@IndexOther(SELECT) @i{etree}@\
SELECT for a given universal or existential quantifier.

@IndexOther(SET-SEARCH-TREE) @i{etree}@\
Set the current etree to be a tree generated and named by NAME-PRIM
when PRIMSUB-METHOD is PR00.@End(Description)

@Section(Mtree Operations)

@Begin(Description)
@IndexOther(ADD-CONN-OB) @i{literal1} @i{oblig1} @i{literal2} @i{oblig2}@\
Add a connection to the current mating. TPS will not allow you to
add a connection to a mating if adding it causes the resulting mating to be
non unifiable. No check is made to determine if the connection spans
an open path.

@IndexOther(MST-GO-DOWN) @i{node} @i{matingstree}@\
Go down one level in the matingstree.

@IndexOther(MST-GO-SIB) @i{matingstree}@\
Go to the next sibling of this node.

@IndexOther(MST-GO-UP) @i{matingstree}@\
Go up one level in the matingstree.

@IndexOther(MST-GOTO) @i{node} @i{matingstree}@\
Move to specified node in an matingstree.

@IndexOther(MST-KILL) @i{node}@\
KILL <node> means to mark the given node 
and all nodes below it as dead.

@IndexOther(MST-RESURRECT) @i{node}@\
RESURRECT <node> means to mark the given node 
and all nodes below it as alive.

@IndexOther(PICK-LIT) @i{literal} @i{obligation}@\
Pick a leaf which you may try to mate with another later.@End(Description)

@Section(Mtree Printing)

@Begin(Description)
@IndexOther(MST-CONNS-ADDED) @i{name}@\
Print out all of the connections which have already 
been added to the given matingstree node. If no node
is given, the current node is used.

@IndexOther(PPRINT-OBLIGATION) @i{name}@\
Print out the given obligation tree with the jforms attached to all nodes. 
If no argument is given,the whole obligation tree is printed out.

@IndexOther(PPRINT-OBLIGATION-PATH) @i{name}@\
Print out the path containing the given obligation,
and show all of the obligations on this path.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.

@IndexOther(PRINT-LIVE-LEAVES) @i{name}@\
Print out all of the live leaves in the tree below 
the given matingstree node. If no node is given, the root 
node is used.

@IndexOther(PRINT-MATINGSTREE) @i{name}@\
Print out the given matingstree. If no matingstree is given,
the current-matingstree is printed out.

@IndexOther(PRINT-MATINGSTREE-NODE) @i{name}@\
Print out the given matingstree node in detail. If no 
node is given, the current matingstree is used.

@IndexOther(PRINT-OBLIGATION) @i{name}@\
Print out the given obligation tree with the jforms attached to the
leaves. If no argument is given, the current-obligation tree is printed out.

@IndexOther(PRINT-OBLIGATION-JFORM) @i{name}@\
Print out the vpform associated with the given obligation node.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.

@IndexOther(PRINT-OBLIGATION-LITERAL) @i{name}@\
Print out the unblocked literals in a given obligation tree.
If no argument is given, the current-obligation tree is the default.

@IndexOther(PRINT-OBLIGATION-PATH) @i{name}@\
Print out the path containing the given obligation.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.

@IndexOther(PRINT-OBTREE-NODE) @i{name}@\
Print out the given  obligation in detail. If no 
obligation is given, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.

@IndexOther(TR-POBTREE) @i{name}@\
Print out the given obligation tree as a tree. If no obligation is given,
the tree below the current obligation is printed out. 

Numbers in round brackets are open obligations; those in square brackets are
closed. 
Branches with *'s denote nodes that are being omitted for lack of space.
The cure for this is to either start printing from a node lower in the tree,
or make the screen wider.

@IndexOther(TR-PRINT-MATINGSTREE) @i{name}@\
Print out the given matingstree as a tree, showing the obligations at each 
node. If no matingstree is given, the current-matingstree is printed out. 

Matingstrees enclosed in curly brackets are marked as dead.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider.

@IndexOther(TR-PRINT-MATINGSTREE-OB) @i{name}@\
Print out the given matingstree as a tree, showing the obligations at each 
node. If no matingstree is given, the current-matingstree is printed out. 

Numbers in round brackets are open obligations. If the brackets end in "..",
there are too many open obligations to fit under the mstree label. 

Leaves underlined with ^'s are closed matingstrees. 
Matingstrees enclosed in curly brackets are marked as dead.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider.@End(Description)

@Section(Mtree Auto)

@Begin(Description)
@IndexOther(ADD-ALL-LIT) @i{literal} @i{obligation}@\
Attempt to mate a literal with all potential mates on 
the current path.

@IndexOther(ADD-ALL-OB) @i{obligation}@\
Attempt to mate all literals in an obligation with 
all potential mates on the current path.

@IndexOther(EXPAND-MST-LEAVES) @i{mtree}@\
Apply ADD-ALL-OB to all live leaves of the current matingstree
that lie below the given node (or the current node, if no node is given).
WARNING: Potential combinatorial explosion!

@IndexOther(MST-BASIC-SEARCH) @i{mtree}@\
Apply EXPAND-LEAVES repeatedly to all live leaves of the current 
matingstree that lie below the given node (or the current node, if 
no node is given), until a closed leaf is generated.
WARNING: Potential combinatorial explosion!

@IndexOther(MST-FEWEST-OB-SEARCH) @i{mtree}@\
Fewest Obligations Search: Choose the matingstree node (from the 
entire tree, not just the tree below the current node) with the 
fewest open obligations. Go to that node and do one step of MT94-12
(i.e. choose the literal with the fewest number of mates, and generate
all of the associated branches of the mtree).
Repeat until a closed leaf is generated.
This search is probably not complete.

@IndexOther(MST-LB-SEARCH) @i{mtree}@\
Least Branching Search: In each leaf node, take the current
obligation and find a literal that can be mated, but with as few 
mates as possible. Add all of these mates as sons to this node.
Repeat until a closed leaf is generated.
This search is probably not complete.

@IndexOther(QUERY-OB) @i{literal} @i{obligation}@\
Output a list of literals which can be mated with a given literal.@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexOther(CALL-UNIFY)@\
Call unification in interactive mode for active mating. The unification
tree associated with the active-mating is passed on to the unification
top-level. Any changes made to this tree are destructive. Applicable only for
a higher-order unification problem. Uses MS88-style unification.@End(Description)

@Section(MS88 search procedure)

@Begin(Description)
@IndexOther(ADD-CONN) @i{first} @i{second}@\
Add a connection to the current mating. TPS will not allow you to
add a connection to a mating if adding it causes the resulting mating to be
non unifiable. No check is made to determine if the connection spans
an open path.

@IndexOther(ADD-CONN*)@\
Repeatedly call ADD-CONN.

@IndexOther(APPLY-SUBSTS-MS)@\
Apply substitutions found during mating search to JFORM. Applicable
only if mating is complete.

@IndexOther(COMPLETE-P)@\
Test whether current mating is complete. Will return a path that
is not spanned by the mating otherwise.

@IndexOther(INIT-MATING)@\
Initializes a new mating. This is the recommended way for
starting an interactive session in MS.

@IndexOther(MINIMAL-P)@\
A mating M is non-minimal if it contains some connection 
c such that M-{c} spans exactly the same vertical paths as M.
MINIMAL-P will find such a connection if it exists; otherwise
it will report that the mating is minimal.

@IndexOther(MS88-SUB) @i{etree}@\
Call MS88 on a partial expansion tree (subtree).

@IndexOther(REM-CONN) @i{first} @i{second}@\
Remove a connection from the current mating.

@IndexOther(REM-CONN*)@\
Repeatedly call REM-CONN.

@IndexOther(REM-LAST-CONN)@\
Remove the last connection to the current mating.

@IndexOther(SHOW-MATING)@\
Show the connections in the current mating.

@IndexOther(SHOW-SUBSTS)@\
Show the substitutions suggested by mating search for the complete
    active mating.@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexOther(CR-EPROOF-JFORM)@\
Create a new jform for the expansion tree associated with the
current mating-search top-level. You need to use this command only if
you modify the expansion tree interactively and you are constructing a
mating interactively.

@IndexOther(CW-DEEP) @i{label} @i{gwff}@\
Create a weak label from the deep formula of an etree.

@IndexOther(CW-JFORM) @i{label} @i{gwff}@\
Create a weak label from the current jform representation
of an etree.

@IndexOther(CW-SHALLOW) @i{label} @i{gwff}@\
Create a weak label from the shallow formula of an etree.

@IndexOther(DISPLAY-VP-DIAG) @i{jform}@\
Use this operation for displaying vertical path diagram on the
terminal with default settings. For complete control over the defaults
use edop VPF.

@IndexOther(DISPLAY-VP-DIAG-ED) @i{jform}@\
Prints a vertical path diagram. This is like VP in the MATE 
top level, but will use the current edwff to create a jform 
if none is currently available.

@IndexOther(DISPLAY-VP-ETREE)@\
Display the VP diagram of the ETREE as used in mating-search.

@IndexOther(DISPLAY-VPD) @i{jform}@\
Use this operation for saving VP diagrams in a file. You may want
to change the values of the variables VPD-FILENAME, VPD-STYLE, VPD-PTYPES,
VPD-BRIEF, VPD-VPFPAGE.

@IndexOther(GWFF-TO-JFORM) @i{gwff}@\
Converts the given GWFF to JFORM.

@IndexOther(GWFF-TO-PROP-JFORM) @i{gwff} @i{pos}@\
Converts the given GWFF (considered as a propositional GWFF) to JFORM.

@IndexOther(JFORM-TO-GWFF) @i{jform}@\
Converts the given JFORM to GWFF. May not work with skolemized jforms.

@IndexOther(NUMBER-OF-HORIZONTAL-PATHS) @i{gwff}@\
Counts the number of horizontal paths through the given jform.

@IndexOther(NUMBER-OF-VERTICAL-PATHS) @i{gwff}@\
Counts the number of vertical paths through the given jform.

@IndexOther(PRINT-JLIST) @i{gwff}@\
Prints the given gwff, using lists for jforms.

@IndexOther(VP-TEX) @i{jform} @i{file}@\
Prints the path diagram, in a format understood by TeX, for a JForm 
or a GWFF. At present, it chops off whatever will not fit on one page.
The following flags affect the output:
    1. VPD-BRIEF controls whether labels or wffs are printed.
    2. VPD-PTYPES controls whether types are printed.
    3. TEXFORMAT controls whether the vertical or horizontal path diagram is
printed.
    4. ALLSCOPEFLAG controls where square brackets are printed.

@IndexOther(VPFORM) @i{jform} @i{file} @i{style} @i{ptypes} @i{brief} @i{vpfpage} @i{comment}@\
Prints the vertical path diagram for a JForm or a GWFF.@End(Description)

@Section(wff Primitives)

@Begin(Description)
@IndexOther(APPLY-WFF) @i{wff1} @i{wff2}@\
Applies first wff to second.

@IndexOther(BINDHEAD) @i{gwff}@\
Returns head of top-level binding.

@IndexOther(BINDING) @i{gwff}@\
Returns top-level binder of wff.

@IndexOther(BINDVAR) @i{gwff}@\
Returns variable bound at top-level.

@IndexOther(CHANGE-PRINT-TYPE) @i{gvar} @i{typesym}@\
Use the type specified whenever this symbol is printed. Note that
this type may be overridden, if the flag retain-initial-type is NIL.

@IndexOther(DUPWFF) @i{gwff} @i{connective}@\
duplicates wff across connective.

@IndexOther(FREE-VARS-OF) @i{inwff}@\
Creates a list of variables free in the wff.

@IndexOther(INTERN-SUBST) @i{gwff} @i{var}@\
Converts term to desired form for substitution.

@IndexOther(MAKE-WFFSCHEMA) @i{gwff}@\
Translate a gwff into a wffschema by replacing proper symbols
by labels of type META-VAR.

@IndexOther(RENAME-BD-VAR) @i{bdwff}@\
Rename the top-level bound variable using the value of the
global parameter REN-VAR-FN.

@IndexOther(SUBST-1-TYPE) @i{typevar} @i{typesym} @i{gwff}@\
Substitute typevar with typesym.

@IndexOther(SUBSTITUTE-TYPES) @i{alist} @i{gwff}@\
Substitute for types from list ((old . new) ...) in gwff.

@IndexOther(TYPE) @i{gwff}@\
Return the type of a gwff.

@IndexOther(TYPE-OF-ARG-1) @i{gwff}@\
Finds type of first argument.@End(Description)

@Section(Equality between Wffs)

@Begin(Description)
@IndexOther(INMOST-GAR) @i{wff}@\
Returns the head of a wff. This will be a logical symbol or a bound wff.

@IndexOther(NOT-WFFEQ) @i{wff1} @i{wff2}@\
Check, whether two wffs are not the same.

@IndexOther(WFFEQ) @i{wff1} @i{wff2}@\
Check whether two wffs are the same.

@IndexOther(WFFEQ-AB) @i{wff1} @i{wff2}@\
Tests for equality modulo alphabetic change of bound variables.

@IndexOther(WFFEQ-DEF) @i{wff1} @i{wff2}@\
Tests for equality modulo definitions, lambda conversion and
alphabetic change of bound variables.

@IndexOther(WFFEQ-DEFEQ) @i{wff1} @i{wff2}@\
Tests for equality modulo definitions, lambda conversion,
alphabetic change of bound variables and the definition of the symbol = .

@IndexOther(WFFEQ-LNORM) @i{wff1} @i{wff2}@\
Test for equality modulo lambda conversions.

@IndexOther(WFFEQ-NNF) @i{wff1} @i{wff2}@\
Test for equality modulo negation normal form.@End(Description)

@Section(Predicates on Wffs)

@Begin(Description)
@IndexOther(A-BD-WFF-P) @i{gwff}@\
Test whether wff is universally quantified.

@IndexOther(ABBREV-P) @i{gwff}@\
Test for a non-polymorphic abbreviation.

@IndexOther(AE-BD-WFF-P) @i{gwff}@\
Test whether wff is universally or existentially quantified.

@IndexOther(AND-P) @i{gwff}@\
Test whether wff is an conjunction.

@IndexOther(ANYABBREV-P) @i{gwff}@\
Test for defined symbol.

@IndexOther(ANYPROPSYM-P) @i{gwff}@\
Test for undefined symbol.

@IndexOther(BOUNDWFF-P) @i{gwff}@\
Test for a top-level binder (e.g. LAMBDA, FORALL).

@IndexOther(E-BD-WFF-P) @i{gwff}@\
Test whether wff is existentially quantified.

@IndexOther(EQUAL-TYPE-P) @i{type1} @i{type2}@\
Test whether two types are the same.

@IndexOther(EQUALS-P) @i{gwff}@\
Test whether wff is an equality.

@IndexOther(EQUIV-P) @i{gwff}@\
Test whether wff is an equivalence.

@IndexOther(FREE-FOR) @i{term} @i{var} @i{inwff}@\
Tests whether a term is free for a variable in a wff.

@IndexOther(FREE-IN) @i{gvar} @i{inwff}@\
Test whether a variable is free in a gwff.

@IndexOther(GVAR-P) @i{gwff}@\
Test for a logical variable (a logical symbol, but no abbrev.).

@IndexOther(GWFF-P) @i{gwff}@\
Test for a gwff (general well-formed formula).

@IndexOther(IMPLIES-P) @i{gwff}@\
Test whether wff is an implication.

@IndexOther(INFIX-OP-P) @i{gwff}@\
Test whether gwff is an infix operator.

@IndexOther(INFIX-P) @i{gwff}@\
Test for a wff with top-level infix operator.

@IndexOther(IS-VARIABLE) @i{gwff}@\
Test whether a wff is a logical variable.

@IndexOther(LABEL-P) @i{gwff}@\
Test for a label (of any flavor).

@IndexOther(LAMBDA-BD-P) @i{gwff}@\
Test whether wff is bound by lambda.

@IndexOther(LEGAL-TYPE-P) @i{gwff}@\
Test for a legal type.

@IndexOther(LOGCONST-P) @i{gwff}@\
Test for a logical constant (e.g. AND, OR, etc.)

@IndexOther(LSYMBOL-P) @i{gwff}@\
Test for a logical symbol (formerly HATOM).

@IndexOther(NON-ATOMIC) @i{gwff}@\
Tests whether a wff is not atomic, that is, negated, quantified or the result of joining two wffs with a binary connective.

@IndexOther(NON-ATOMIC-OR-TRUTHVALUE) @i{gwff}@\
Tests whether a wff is not atomic or a truth value, that is, truth, falsehood, negated, 
quantified or the result of joining two wffs with a binary connective.

@IndexOther(NOT-FREE-IN) @i{gvar} @i{inwff}@\
Tests whether a variable is not free in a wff.

@IndexOther(NOT-FREE-IN-HYPS) @i{gvar}@\
Tests whether a variable is not free in the set of hypotheses of a rule.

@IndexOther(NOT-FREE-IN-WFFSET) @i{gvar} @i{wffset}@\
Tests whether a variable is not free in a set of wffs.

@IndexOther(NOT-P) @i{gwff}@\
Test whether wff is negated.

@IndexOther(OR-P) @i{gwff}@\
Test whether wff is a disjunction.

@IndexOther(PMABBREV-P) @i{gwff}@\
Test for a polymorphic abbreviation (e.g. something
standing for SUBSET or IMAGE).

@IndexOther(PMPROPSYM-P) @i{gwff}@\
Test for a polymorphic proper symbol (e.g. something
standing for PI or IOTA).

@IndexOther(PROPSYM-P) @i{gwff}@\
Test whether argument is a proper symbol.

@IndexOther(R-PRIME-RESTR) @i{term1} @i{wff1} @i{term2} @i{wff2}@\
Verifies that wff2 follows from wff1 by Rule R' using equality term1=term2.

@IndexOther(REDUCT-P) @i{gwff}@\
Test for a top-level reduct.

@IndexOther(SAME-MODULO-EQUALITY) @i{wff1} @i{wff2} @i{term1} @i{term2}@\
Verifies that wff2 follows from wff1 by Rule R' (possibly
iterated) using equality term1=term2.

@IndexOther(SUBST-OCCS) @i{term1} @i{wff1} @i{term2} @i{wff2} @i{pvs}@\
Checks to see if wff2 is the result of replacing some
occurrences of term1 in wff1 with term2. The pvs must not be
bound at such occurrences of term1.

@IndexOther(SUBST-SOME-OCCURRENCES) @i{term1} @i{wff1} @i{term2} @i{wff2}@\
Checks to see if wff2 is the result of replacing some
occurrences of term1 in wff1 with term2.

@IndexOther(TYPE-EQUAL) @i{gwff1} @i{gwff2}@\
Test whether the types of two wffs are the same.

@IndexOther(WFF-APPLIC-P) @i{gwff}@\
Test for an application of a wff (function) to another wff (arg).@End(Description)

@Section(Moving Commands)

@Begin(Description)
@IndexOther(FIND-BINDER) @i{gwff}@\
Find the first binder (left to right)

@IndexOther(FIND-INFIX) @i{gwff}@\
Find an infix operator.

@IndexOther(FIND-INFIX-ETREE) @i{etree}@\
Find first infix node in etree.

@IndexOther(GAR) @i{gwff}@\
Extract the 'function' part of an application.
Returns the bound variable from a wff with top-level binder.

@IndexOther(GDR) @i{gwff}@\
Extract the 'argument' part of an application.
Returns the scope of the binder from a wff with top-level binder.

@IndexOther(GLR) @i{gwff}@\
Extract the left-hand side of an infix operator.

@IndexOther(GOTO-NODE) @i{node} @i{etree}@\
Move to specified node in an etree.

@IndexOther(GRR) @i{gwff}@\
Extract the right-hand side of an infix operator.

@IndexOther(NTHARG) @i{n} @i{gwff}@\
Move to the nth argument of a functional application,
or to the nth disjunct, conjunct, etc.

@IndexOther(REPLACE-GAR) @i{gwff} @i{newgarwff}@\
Replace the 'function' part of an application non-destructively.

@IndexOther(REPLACE-GDR) @i{gwff} @i{newgdrwff}@\
Replace the 'argument' part of an application non-destructively.

@IndexOther(REPLACE-GLR) @i{gwff} @i{newglrwff}@\
Replace the left-hand side of an infix operator non-destructively.

@IndexOther(REPLACE-GRR) @i{gwff} @i{newgrrwff}@\
Replace the right-hand side of an infix operator non-destructively.@End(Description)

@Section(Changing Commands)

@Begin(Description)
@IndexOther(CHANGE-TOP) @i{conn} @i{gwff}@\
Change the top connective of a formula. For example,
"cntop or" will change "A and B" into "A or B";
"cntop exists" will change "forall x P x" into "exists x P x".

@IndexOther(DELETE-TOPCONN-LSCOPE) @i{gwff}@\
Delete the topmost binary connective and its left scope

@IndexOther(DELETE-TOPCONN-RSCOPE) @i{gwff}@\
Delete the topmost binary connective and its right scope

@IndexOther(MBED-AND-LEFT) @i{lgwff} @i{rgwff}@\
Embed the current edwff in the left scope of AND. 
The right scope is provided by the user.

@IndexOther(MBED-AND-RIGHT) @i{rgwff} @i{lgwff}@\
Embed the current edwff in the right scope of AND. 
The left scope is provided by the user.

@IndexOther(MBED-EQUIV-LEFT) @i{lgwff} @i{rgwff}@\
Embed the current edwff on the left side of equivalence. 
The right side is provided by the user.

@IndexOther(MBED-EQUIV-RIGHT) @i{lgwff} @i{rgwff}@\
Embed the current edwff on the right side of equivalence. 
The left side is provided by the user.

@IndexOther(MBED-EXISTENTIAL) @i{vquant} @i{crwff}@\
Embed the current edwff in the scope of a existential quantifier. 
The variable of quantification is provided by the user.

@IndexOther(MBED-EXISTENTIAL1) @i{vquant} @i{crwff}@\
Embed the current edwff in the scope of an exists1 quantifier. 
The variable of quantification is provided by the user.

@IndexOther(MBED-FORALL) @i{vquant} @i{crwff}@\
Embed the current edwff in the scope of a universal quantifier. 
The variable of quantification is provided by the user.

@IndexOther(MBED-IMPLICS-LEFT) @i{lgwff} @i{rgwff}@\
Embed the current edwff as the antecedent of a conditional. 
The consequent is provided by the user.

@IndexOther(MBED-IMPLICS-RIGHT) @i{lgwff} @i{rgwff}@\
Embed the current edwff as the consequent of a conditional. 
The antecedent is provided by the user.

@IndexOther(MBED-LAMBDA) @i{vquant} @i{crwff}@\
Embed the current edwff in the scope of lambda. 
The variable of quantification is provided by the user.

@IndexOther(MBED-OR-LEFT) @i{lgwff} @i{rgwff}@\
Embed the current edwff in the left scope of OR. 
The right scope is provided by the user.

@IndexOther(MBED-OR-RIGHT) @i{rgwff} @i{lgwff}@\
Embed the current edwff in the right scope of OR. 
The left scope is provided by the user.

@IndexOther(MBED=LEFT) @i{lgwff} @i{rgwff}@\
Embed the current edwff on the left side of equality. 
The right side is provided by the user.

@IndexOther(MBED=RIGHT) @i{rgwff} @i{lgwff}@\
Embed the current edwff on the right side of equality. 
The left side is provided by the user.

@IndexOther(MERGE-CONSTANT) @i{gwff}@\
Remove constant truth values TRUTH and FALSEHOOD in a wff.

@IndexOther(MERGE-IDEMPOTENT) @i{gwff}@\
Merges idempotent component(s) of a formula.

@IndexOther(WFF-ABSORB) @i{gwff}@\
Apply absorption laws to a formula.

@IndexOther(WFF-ASSOCIATIVE-L) @i{gwff}@\
Apply the left associative law to a formula:
   A op (B op C) --> (A op B) op C.

@IndexOther(WFF-ASSOCIATIVE-R) @i{gwff}@\
Apply the right associative law to a formula:
   (A op B) op C --> A op (B op C).

@IndexOther(WFF-COMMUTATIVE) @i{gwff}@\
Apply commutativity laws to a formula:
   A and B --> B and A
   A or B --> B or A
   A implies B --> not B implies not A
   A equiv B --> B equiv A.

@IndexOther(WFF-DIST-CONTRACT) @i{gwff}@\
Apply distributivity laws to a formula in the contracting direction:
   (A and B) or (A and C) --> A and (B or C)
   (A or B) and (A or C) --> A or (B and C)
   (B and A) or (C and A) --> (B or C) and A 
   (B or A) and (C or A) --> (B and C) or A.

@IndexOther(WFF-DIST-EXPAND) @i{gwff}@\
Apply distributivity laws to a formula in the expanding direction:
   A and (B or C) --> (A and B) or (A and C)
   A or (B and C) --> (A or B) and (A or C)
   (B or C) and A --> (B and A) or (C and A)
   (B and C) or A --> (B or A) and (C or A).

@IndexOther(WFF-DOUBLE-NEGATION) @i{gwff}@\
Remove a double negation:
   not not A --> A.

@IndexOther(WFF-PERMUTE) @i{gwff}@\
Permute the two components of an infix operator:
   A op B --> B op A.

@IndexOther(WFF-SUB-EQUIV) @i{gwff}@\
Apply following law to a formula:
   A equiv B --> (A implies B) and (B implies A).

@IndexOther(WFF-SUB-IMPLIES) @i{gwff}@\
Apply the following law to a formula:
   A implies B --> not A or B.@End(Description)

@Section(Recursively Changing Commands)

@Begin(Description)
@IndexOther(ASSOC-L) @i{gwff}@\
Recursively apply the left associative law to a formula.
Used in the rule ASSOC.

@IndexOther(MERGE-CONSTANT*) @i{gwff}@\
Recursively remove truth constants TRUTH and FALSEHOOD in a wff.

@IndexOther(MERGE-IDEMPOTENT*) @i{gwff}@\
Recursively merges idempotent component(s) of a formula.

@IndexOther(WFF-ABSORB*) @i{gwff}@\
Apply absorption laws to a formula.

@IndexOther(WFF-ASSOCIATIVE-L*) @i{gwff}@\
Recursively apply the left associative law to a formula:
   A op (B op C) --> (A op B) op C.

@IndexOther(WFF-ASSOCIATIVE-R*) @i{gwff}@\
Recursively apply the right associative law to a formula:
   (A op B) op C --> A op (B op C).

@IndexOther(WFF-COMMUTATIVE*) @i{gwff}@\
Recursively apply commutativity laws to a formula:
   A and B --> B and A
   A or B --> B or A
   A implies B --> not B implies not A
   A equiv B --> B equiv A.

@IndexOther(WFF-DIST-CONTRACT*) @i{gwff}@\
Recursively apply distributivity laws to a formula in 
the contracting direction:
   (A and B) or (A and C) --> A and (B or C)
   (A or B) and (A or C) --> A or (B and C)
   (B and A) or (C and A) --> (B or C) and A 
   (B or A) and (C or A) --> (B and C) or A.

@IndexOther(WFF-DIST-EXPAND*) @i{gwff}@\
Recursively apply distributivity laws to a formula in 
the expanding direction:
   A and (B or C) --> (A and B) or (A and C)
   A or (B and C) --> (A or B) and (A or C)
   (B or C) and A --> (B and A) or (C and A)
   (B and C) or A --> (B or A) and (C or A).

@IndexOther(WFF-DOUBLE-NEGATION*) @i{gwff}@\
Recursively remove double negations:
   not not A --> A.

@IndexOther(WFF-PERMUTE*) @i{gwff}@\
Recursively permute the two components of an infix operator:
   A op B --> B op A

@IndexOther(WFF-SUB-EQUIV*) @i{gwff}@\
Recursively apply the following law to a formula:
   A equiv B --> (A implies B) and (B implies A).

@IndexOther(WFF-SUB-IMPLIES*) @i{gwff}@\
Recursively apply the following law to a formula:
   A implies B --> not A or B.@End(Description)

@Section(Rewriting commands)

@Begin(Description)
@IndexOther(APPLY-RRULE-1) @i{gwff} @i{rule}@\
Apply a rewrite rule (active or inactive) to the 
current edwff. If the rule is bidirectional, you will be 
prompted about which direction to apply it in.

@IndexOther(APPLY-RRULE-1*) @i{gwff} @i{rule}@\
Apply a rewrite rule (active or inactive) repeatedly 
to the current edwff. If the rule is bidirectional, you will 
be prompted about which direction to apply it in.
CAUTION: may not terminate.

@IndexOther(APPLY-RRULE-ANY) @i{gwff}@\
Apply one active rewrite rule to the current edwff; attempt 
different active rules in the order in which they are listed by 
LIST-RRULES until one works. If any current rules are 
bidirectional, you will be prompted about which direction to 
apply them in.

@IndexOther(APPLY-RRULE-ANY*) @i{gwff}@\
Apply one active rewrite rule to the current edwff; attempt 
different active rules in the order in which they are listed by 
LIST-RRULES until one works. If any current rules are 
bidirectional, you will be prompted about which direction to 
apply them in. Repeat this until no more rules
are applicable. CAUTION: may not terminate.

@IndexOther(CREATE-REWRITE-RULE) @i{name} @i{gwff1} @i{gwff2} @i{func} @i{types} @i{bidir} @i{appfn} @i{mhelp}@\
Creates a new rewrite rule with the given left and right
sides, such that the left-hand gwff rewrites to the result of 
applying the function to the right-hand gwff.

@IndexOther(INSTANCE-OF-REWRITING) @i{inwff} @i{outwff}@\
Test to see whether one gwff can be obtained from another
by non-overlapping rewrite rules.

@IndexOther(SIMPLIFY-DOWN) @i{gwff}@\
Apply any active rewrite rule
A --> B or A <--> B to the current gwff in the forward direction.
(i.e. subformulas A are rewritten to B, modulo any functions
attached to the rules, so that the resulting formula will be a 
rewrite instance of the original formula.)

@IndexOther(SIMPLIFY-DOWN*) @i{gwff}@\
Apply all active rewrite rules
A --> B or A <--> B to the current gwff in the forward direction.
(i.e. subformulas A are rewritten to B, modulo any functions
attached to the rules, so that the resulting formula will be a 
rewrite instance of the original formula.)

@IndexOther(SIMPLIFY-UP) @i{gwff}@\
Apply any one active rewrite rule B <--> A in the backward 
direction. (i.e. subformulas A are rewritten to B, modulo any functions
attached to the rules, so that the original gwff will be a rewrite
instance of the resulting gwff.)

@IndexOther(SIMPLIFY-UP*) @i{gwff}@\
Unapply all active rewrite rules
A --> B, and apply all active rewrite rules B <--> A in the backward 
direction. (i.e. subformulas A are rewritten to B, modulo any functions
attached to the rules, so that the original gwff will be a rewrite
instance of the resulting gwff.) 

@IndexOther(UNAPPLY-RRULE-1) @i{gwff} @i{rule}@\
Unapply a rewrite rule (active or inactive) to the current 
edwff. (i.e. apply it in the reverse direction).
If the rule is bidirectional, you will be prompted about
which direction to apply it in.

@IndexOther(UNAPPLY-RRULE-1*) @i{gwff} @i{rule}@\
Unapply a rewrite rule (active or inactive) repeatedly to 
the current edwff. (i.e. apply it in the reverse direction).
If the rule is bidirectional, you will be prompted about
which direction to apply it in.
CAUTION: may not terminate.

@IndexOther(UNAPPLY-RRULE-ANY) @i{gwff}@\
Unapply one active rewrite rule to the current edwff (i.e. apply
it in the reverse direction); attempt different active rules in the 
order in which they are listed by LIST-RRULES until one works.
If any current rules are bidirectional, you will be prompted 
about which direction to apply them in.

@IndexOther(UNAPPLY-RRULE-ANY*) @i{gwff}@\
Unapply one active rewrite rule to the current edwff (i.e. 
apply it in the reverse direction); attempt different active rules in 
the order in which they are listed by LIST-RRULES until one works. 
Repeat this until no more rules are applicable. If any current rules
are bidirectional, you will be prompted about which direction to 
apply them in. CAUTION: may not terminate.@End(Description)

@Section(Substitution)

@Begin(Description)
@IndexOther(DO-PRIMSUB) @i{inwff} @i{var} @i{sub}@\
Replaces a variable with a primitive substitution.
Differs from SUBST in that it will also replace quantified
variables, and their quantifiers, as necessary.

@IndexOther(DUPLICATE-VAR) @i{gwff}@\
Duplicate a variable at an expansion node.

@IndexOther(INSTANTIATE-BINDER) @i{term} @i{bdwff}@\
Instantiate a top-level universal or existential binder with a term.

@IndexOther(REPLACE-EQUIV) @i{rep-sym} @i{rep-by} @i{rep-in}@\
Replace one occurrence of a symbol (such as AND) by a predefined 
equivalent wff (such as [lambda p lambda q.~.p IMPLIES ~q]).  In this
example repsym is AND and rep-by is IMPLIES.  To see if a symbol can be
replaced by this command, enter HELP symbol; any such replacements will be
listed under the heading 'Replaceable Symbols'.

@IndexOther(REPLACE-EQUIV-ALL) @i{rep-sym} @i{rep-by} @i{rep-in}@\
Replace a all occurrences of a symbol by a predefined equivalent wff.

@IndexOther(S) @i{term} @i{var} @i{inwff}@\
Substitute a term for the free occurrences of variable in a gwff.

@IndexOther(SUBST-SOME-OCCS) @i{replaced-term} @i{in-wff} @i{replaced-by-term} @i{result-wff}@\
Tests whether a wff is the result of replacing 0 or more
occurrences of a term by another in a given wff.

@IndexOther(SUBSTITUTE-IN-ETREE) @i{term} @i{var} @i{etree}@\
Substitute a term for a variable throughout an expansion tree. 
Destructively alters the expansion tree.

@IndexOther(SUBSTITUTE-L-TERM-VAR) @i{term} @i{var} @i{inwff}@\
Substitute a term for the free occurrences of variable in a gwff.
Bound variables may be renamed, using the function in the global
variable REN-VAR-FN.

@IndexOther(SUBSTITUTE-TERM-VAR) @i{term} @i{var} @i{inwff}@\
Substitute a term for the free occurrences of variable in a gwff.

@IndexOther(WFF-IDENTITY) @i{gwff}@\
The identity function on gwff.@End(Description)

@Section(Basic Abbreviations)

@Begin(Description)
@IndexOther(ABBR-LIST) @i{gwff}@\
Lists all the abbreviations used in a gwff.

@IndexOther(CONST-LIST) @i{gwff}@\
Lists all the logical constants used in a gwff, apart 
from the primitive constants AND FALSEHOOD IMPLIES NOT OR TRUTH.

@IndexOther(CONTAINS-DEFN)@\
Tests whether the argument contains a definition.

@IndexOther(INST-DEF) @i{inwff}@\
Instantiate the first abbreviation, left-to-right.

@IndexOther(INSTANTIATE-1) @i{inwff}@\
Instantiate the first abbreviation, left-to-right.

@IndexOther(INSTANTIATE-ALL) @i{inwff} @i{exceptions}@\
Instantiate all definitions, except the ones specified
in the second argument.

@IndexOther(INSTANTIATE-ALL-REC) @i{inwff} @i{exceptions}@\
Recursively instantiate all definitions, except the ones specified
in the second argument.

@IndexOther(INSTANTIATE-DEFN) @i{gabbr} @i{inwff}@\
Instantiate all occurrences of an abbreviation.
The occurrences will be lambda-contracted, but not lambda-normalized.

@IndexOther(INSTANTIATE-EQUALITIES) @i{inwff}@\
Instantiate all equalities in gwff. Consults the flag
  REWRITE-EQUALITIES (but ignores it if it's set to NONE).

@IndexOther(INSTANTIATE-TOP-EQUALITY) @i{inwff}@\
Instantiate outermost equality in gwff. Consults the flag
  REWRITE-EQUALITIES (but ignores it if it's set to NONE).

@IndexOther(LIB-ABBR-LIST) @i{gwff}@\
Lists all the library abbreviations used in a gwff.

@IndexOther(NEW-DEFS) @i{gwff}@\
Lists all the definitions used in a gwff that are
either library abbreviations or weak labels.

@IndexOther(RPIN) @i{inwff}@\
Prompt for a replaceable symbol and the name of a replacement
and replace the first occurrence of the symbol.

@IndexOther(SUBSTITUTE-BDVAR-SCOPE) @i{def-var} @i{newvar} @i{scope-var} @i{scope} @i{inwff}@\
Creates instantiation from binder definition, etc.

@IndexOther(TOP-LEVEL-DEFN)@\
Tests whether the argument is a top-level definition.@End(Description)

@Section(Lambda-Calculus)

@Begin(Description)
@IndexOther(AB-CHANGE) @i{gwff} @i{newvar}@\
Alphabetic change of variable at top-level.

@IndexOther(AB-NORMAL-P) @i{gwff}@\
Check whether the gwff is in alphabetic normal form. 

@IndexOther(AB-NORMALIZE) @i{gwff}@\
Convert the gwff to alphabetic normal form. 

@IndexOther(ETA-EXP) @i{gwff}@\
Performs a one-step eta expansion.

@IndexOther(ETA-TO-BASE) @i{gwff}@\
Eta-expands until original wff is part of a wff of base type.

@IndexOther(ETACONTR) @i{gwff}@\
Reduces [lambda x.fx] to f at top.

@IndexOther(ETANORM) @i{gwff}@\
Reduces [lambda x.fx] to f from inside out.

@IndexOther(LAMBDA-NORM) @i{gwff}@\
Convert a wff into lambda-normal form.

@IndexOther(LCONTR) @i{reduct}@\
Lambda-contract a top-level reduct.
Bound variables may be renamed using REN-VAR-FN

@IndexOther(LEXPD) @i{var} @i{term} @i{inwff} @i{occurs}@\
Converts the wff into the application of a function to the term.
The function is formed by replacing given valid occurrences of a term
with the variable and binding the result.

@IndexOther(LNORM) @i{gwff}@\
Put a wff into lambda-normal form, using beta or beta-eta conversion 
according to the value of flag LAMBDA-CONV. Compare LNORM-BETA and LNORM-ETA.

@IndexOther(LNORM-BETA) @i{gwff}@\
Put a wff into beta-normal form, not using eta 
conversion. Compare LNORM and LNORM-ETA.

@IndexOther(LNORM-ETA) @i{gwff}@\
Put a wff into eta-normal form, not using beta
conversion. Compare LNORM-BETA and LNORM.

@IndexOther(LONG-ETA) @i{gwff}@\
Returns the long-eta normal form of wff.

@IndexOther(REWRITE-ALL-EQUIVALENCE) @i{gwff}@\
Replaces all occurrences of the form `A EQUIV B'
according to the setting of the flag REWRITE-EQUIVS.

@IndexOther(UNTYPED-LAMBDA-NORM) @i{gwff}@\
Convert a untyped wff into lambda-normal form. Be aware of unterminated reduction 
in untyped lambda calculus.

@IndexOther(WFFEQ-AB-BETA) @i{wff1} @i{wff2}@\
Verifies that wff1 and wff2 are equal up to
lambda-normalization with beta rule only, and alphabetic change of 
bound variables. (Compare WFFEQ-AB-ETA, WFFEQ-AB-LAMBDA.)

@IndexOther(WFFEQ-AB-ETA) @i{wff1} @i{wff2}@\
Verifies that wff1 and wff2 are equal up to
lambda-normalization with eta rule only, and alphabetic change of 
bound variables. (Compare WFFEQ-AB-BETA, WFFEQ-AB-LAMBDA.)

@IndexOther(WFFEQ-AB-LAMBDA) @i{wff1} @i{wff2}@\
Verifies that wff1 and wff2 are equal up to
lambda-normalization and alphabetic change of bound variables.
Uses both eta and beta rules (compare WFFEQ-AB-ETA and WFFEQ-AB-BETA).@End(Description)

@Section(Negation movers)

@Begin(Description)
@IndexOther(NEG-NORM) @i{gwff}@\
Return the negation normal form of the given wff.

@IndexOther(NEGWFF) @i{gwff}@\
Negates current wff, erasing double negations.

@IndexOther(PULL-NEGATION) @i{gwff}@\
Pulls negations out one level.

@IndexOther(PUSH-NEGATION) @i{gwff}@\
Pushes negation through the outermost operator or quantifier.@End(Description)

@Section(Primitive Substitutions)

@Begin(Description)
@IndexOther(NAME-PRIMSUBSTS) @i{gwff}@\
Creates weak labels for primitive substitutions for the head
    variables of a wff.

@IndexOther(PRIMSUBSTS) @i{gwff}@\
Prints primitive substitutions for the head variables of a wff.@End(Description)

@Section(Miscellaneous)

@Begin(Description)
@IndexOther(CLAUSE-FORM) @i{gwff}@\
Converts the given wff to clause form, as if the resulting wff is to
    be given to a resolution theorem prover.  The gwff is skolemized,
    rectified, etc.

@IndexOther(CONJUNCTIVE-NORMAL-FORM) @i{gwff}@\
Find the conjunctive normal form of a wff.

@IndexOther(FIND-SUBFORMULAS) @i{gwff} @i{type}@\
Find all subformulas of a given type in a wff.

@IndexOther(HEAD) @i{gwff}@\
Find the head of a gwff.

@IndexOther(HVARS) @i{gwff}@\
Find all head variables of a wff.

@IndexOther(MIN-QUANT-SCOPE) @i{gwff}@\
Minimize the scope of quantifiers in a gwff. Deletes vacuous
quantifiers. During proof transformation, the gap between a formula
and its min-quant-scope version is filled by RULEQ.@End(Description)

@Section(RuleP)

@Begin(Description)
@IndexOther(SAT-P) @i{jform}@\
Check whether a propositional wff is satisfiable.

@IndexOther(VALID-P) @i{jform}@\
Check whether a propositional wff is valid.@End(Description)

@Section(Skolemizing)

@Begin(Description)
@IndexOther(SIMUL-SUBSTITUTE-L-TERM-VAR) @i{alist} @i{inwff}@\
Simultaneously substitute terms for the free occurrences of variables.

@IndexOther(SKOLEMS1) @i{gwff} @i{univflag}@\
Skolemize a wff using method S1. See page 127 of Andrews' book.
   If equivalences are present, you must eliminate them first by REW-EQUIV.

@IndexOther(SKOLEMS3) @i{gwff} @i{univflag}@\
Skolemize a wff using method S3.  At the moment it takes only
those free variables which are universally quantified somewhere before,
all other variables are considered to be constants.
    See page 127 of Andrews' book.
    If equivalences are present, you must eliminate them first by REW-EQUIV.@End(Description)

@Section(Quantifier Commands)

@Begin(Description)
@IndexOther(DELETE-BINDER) @i{bdwff}@\
Delete a top-level universal or existential binder.

@IndexOther(DELETE-LEFTMOST-BINDER) @i{gwff}@\
Delete the leftmost binder in a wff.

@IndexOther(OPENWFFA) @i{gwff}@\
Delete all accessible essentially universal quantifiers.

@IndexOther(OPENWFFE) @i{gwff}@\
Delete all accessible essentially existential quantifiers.@End(Description)

@Section(Wellformedness)

@Begin(Description)
@IndexOther(CULPRIT-P) @i{unwff}@\
Test whether the unwff is a minimal ill-formed part.

@IndexOther(FIND-CULPRIT) @i{gwff}@\
Find a minimal ill-formed subformula.

@IndexOther(LOCATEUNWFFS) @i{unwff}@\
Return a list of messages, each the describing the error
in a minimal ill-formed subparts of the argument.@End(Description)

@Section(Statistics)

@Begin(Description)
@IndexOther(DELETE-DUPLICATE-CONNS)@\
Deletes duplicate connections from a mating. This should be necessary
    only for propositional formulas.

@IndexOther(SHOW-MATING-STATS)@\
Display statistics for the active mating and totals for all
matings in this expansion proof.@End(Description)
@ChapterPh(Recursive Wff Functions)
The internal name of this category is 
WFFREC%.
A recursive wff function can be defined using DEFWFFREC.
Allowable properties are: @t{ARGNAMES}, @t{MULTIPLE-RECURSION}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(EDSEARCH)@\
No more help available.  Sorry.@End(Description)

@Section(OTL Object)

@Begin(Description)
@IndexOther(META-SUBST)@\
No more help available.  Sorry.

@IndexOther(META-SUBST1)@\
No more help available.  Sorry.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(PRINTWFF)@\
No more help available.  Sorry.@End(Description)

@Section(wff Primitives)

@Begin(Description)
@IndexOther(FREE-VARS)@\
Finds free variables of a wff.

@IndexOther(MAKE-WFFSCHEMA1)@\
No more help available.  Sorry.

@IndexOther(UNINTERPRETED-SYMS)@\
Finds uninterpreted symbols (variables and constants) of a wff.@End(Description)

@Section(Equality between Wffs)

@Begin(Description)
@IndexOther(WFFEQ-AB1)@\
No more help available.  Sorry.

@IndexOther(WFFEQ-DEF1)@\
No more help available.  Sorry.

@IndexOther(WFFEQ-LNORM1)@\
No more help available.  Sorry.

@IndexOther(WFFEQ-NNF1)@\
No more help available.  Sorry.@End(Description)

@Section(Predicates on Wffs)

@Begin(Description)
@IndexOther(GWFF-Q)@\
No more help available.  Sorry.

@IndexOther(LEGAL-TYPE-P1)@\
No more help available.  Sorry.

@IndexOther(S-S-O-REC)@\
Recursive part of SUBST-SOME-OCCURRENCES.@End(Description)

@Section(Moving Commands)

@Begin(Description)
@IndexOther(NTH-PREFIX-ARG)@\
No more help available.  Sorry.@End(Description)

@Section(Substitution)

@Begin(Description)
@IndexOther(DO-PRIMSUB-REC)@\

@IndexOther(REPLACE-EQUIV-WFF)@\
No more help available.  Sorry.

@IndexOther(SUBST-L-TERM-REC)@\
Recursive part of SUBSTITUTE-L-TERM-VAR.@End(Description)

@Section(Basic Abbreviations)

@Begin(Description)
@IndexOther(INSTANTIATE-=)@\
No more help available.  Sorry.

@IndexOther(INSTANTIATE-DEFINITIONS)@\
No more help available.  Sorry.@End(Description)

@Section(Lambda-Calculus)

@Begin(Description)
@IndexOther(LEXPD-REC)@\
Recursive part of lambda expansion.

@IndexOther(PREPARE-FOR)@\
Makes alphabetic change to avoid binding of variable replacing term.@End(Description)

@Section(Quantifier Commands)

@Begin(Description)
@IndexOther(OPENWFFA1)@\
No more help available.  Sorry.

@IndexOther(OPENWFFE1)@\
No more help available.  Sorry.@End(Description)
@ChapterPh(Wff Reference Formats)
The internal name of this category is 
GETGWFFTYPE.
A wff reference format can be defined using DEFGWFF-TYPE.
Allowable properties are: @t{CHECKFN}, @t{GETFN}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(DPROOF-LINE-REF)@\
No more help available.  Sorry.

@IndexOther(REWRITING-LINE-REF)@\
No more help available.  Sorry.@End(Description)

@Section(Weak Labels)

@Begin(Description)
@IndexOther(WEAK-TYPE)@\
weak label : the wff represented by a weak label.@End(Description)

@Section(Flavors of Labels)

@Begin(Description)
@IndexOther(FLAVOR-TYPE)@\
label : a label for a wff.@End(Description)

@Section(Proof Outline)

@Begin(Description)
@IndexOther(LINE-NUMBER)@\
Number : the assertion of a line in the current outline.@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexOther(ETREES-LABELS)@\
Labels used in expansion trees.@End(Description)

@Section(Mating search)

@Begin(Description)
@IndexOther(CURRENT-EPROOF-TYPE)@\
current-eproof : The mating-search name for the eproof being worked on.

@IndexOther(LAST-EPROOF-TYPE)@\
last-eproof : The name for the last expansion proof when outside mating search.@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexOther(JFORMS-LABELS)@\
Labels used in JFORMS.@End(Description)

@Section(Theorems)

@Begin(Description)
@IndexOther(THEOREM-TYPE)@\
theorem: a theorem (exercise, practice, or theorem from the book).@End(Description)

@Section(Wff Editor)

@Begin(Description)
@IndexOther(EDIT-WFF)@\
A specification of the form (ED gwff) to edit gwff.

@IndexOther(EDWFF-TYPE)@\
edwff : The editor's name for the wff being edited.

@IndexOther(LAST-EDWFF-TYPE)@\
last-edwff : The name for the last edited wff when outside the editor.@End(Description)

@Section(Wff Parsing)

@Begin(Description)
@IndexOther(STRING-BOUND-VAR)@\
bound variable: variable bound to a string.

@IndexOther(STRING-TYPE)@\
string : quoted sequence of symbols.@End(Description)

@Section(Wff Types)

@Begin(Description)
@IndexOther(WFFOP-TYPE)@\
wffop arg ... arg : A wff operation applied to arguments.@End(Description)
@ChapterPh(Flavors)
The internal name of this category is 
FLAVOR.
A flavor can be defined using DEFNEVERUSED.
Allowable properties are: @t{INHERIT-PROPERTIES}, @t{INSTANCE-ATTRIBUTES}, @t{INCLUDE}, @t{PRINTFN}, @t{MHELP}, and more.

@Section(Weak Labels)

@Begin(Description)
@IndexOther(WEAK)@\
A weak label stands for another wff, but dissolves under most
operations like substitution etc.@End(Description)

@Section(Flavors of Labels)

@Begin(Description)
@IndexOther(META)@\
A label created by the parser when it finds a meta-wff inside
a wff.@End(Description)

@Section(Expansion Trees)

@Begin(Description)
@IndexOther(ECONJUNCTION)@\
An econjunction label stands for a conjunction node.

@IndexOther(EDISJUNCTION)@\
An edisjunction label stands for a disjunction node.

@IndexOther(EMPTY-DUP-INFO)@\
EMPTY is solely used in translation part of code!

@IndexOther(ETREE)@\
Defines common properties of expansion tree nodes.

@IndexOther(EXP-VAR)@\
An EXP-VAR is used to represent a variable (one which can be
substituted for) in an expansion tree.  It has two main properties:
a variable and a substitution (which may be the same as the variable if
no substitution has yet been made.

@IndexOther(EXPANSION)@\
An EXPANSION label stands for an expansion node.

@IndexOther(FALSE)@\
A false node stands for the logical constant FALSEHOOD.

@IndexOther(IMPLICATION)@\
An implication node stands for an implication node.

@IndexOther(LEAF)@\
A leaf label stands for a leaf node of an etree.

@IndexOther(NEGATION)@\
A negation label stands for a negation node.

@IndexOther(REWRITE)@\
A rewrite node stands for a node which has been rewritten.

@IndexOther(SELECTION)@\
A SELECTION label stands for a selection node in a (non-skolem)
expansion tree

@IndexOther(SKOLEM)@\
A skolem node stands for a skolemized node in a (skolem) expansion
tree.

@IndexOther(SKOLEM-TERM)@\
A skolem-term label contains both a skolem term, which is a
skolem function applied to some free variables (if any), and a parameter,
which is a new constant.  Skolem-terms may be printed in either of the
two ways: the flag SHOW-SKOLEM controls how they are printed.

@IndexOther(TRUE)@\
A true node stands for the logical constant TRUTH.@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexOther(CONJUNCTION)@\
A conjunction label stands for a conjunction of wffs.

@IndexOther(DISJUNCTION)@\
A disjunction label stands for a disjunction of wffs.

@IndexOther(EXISTENTIAL)@\
An existential label stands for a wff which is existentially bound.

@IndexOther(JFORM)@\
Defines common properties of jforms.

@IndexOther(LITERAL)@\
A literal label stands for a wff which is not a conjunction,
	 disjunction, universally or existentially bound, or a negation.

@IndexOther(UNIVERSAL)@\
A universal label stands for a wff which is universally bound.@End(Description)

@Section(wff Primitives)

@Begin(Description)
@IndexOther(META-BD)@\
A label created when a bound meta-variable appears.

@IndexOther(META-VAR)@\
A label which stands for a meta-variable.@End(Description)
@ChapterPh(Styles)
The internal name of this category is 
DEVICE-STYLE.
A style can be defined using DEFSTYLE.
Allowable properties are: @t{PRINT-SYMBOL}, @t{PRINT-SPACE-P}, @t{TERPRI-HEURISTICS}, @t{PRINT-TYPESYM}, @t{PRINT-TYPE-CHAR}, @t{PRINT-INDENT}, @t{PRINT-TAB}, @t{PRINT-NEXTPAR}, @t{PRINT-LINE}, @t{MARGIN-CORRECT}, @t{DISPLAY-PREFIX}, @t{DISPLAY-POSTFIX}, @t{BEGIN-ENVIRONMENT}, @t{END-ENVIRONMENT}, @t{TEXT-PREFIX}, @t{TEXT-POSTFIX}, @t{CHAR-CAT}, @t{MHELP}.

@Section(Review)

@Begin(Description)
@IndexOther(GENERIC)@\
GENERIC stands for any terminal without special characters.@End(Description)

@Section(Concept)

@Begin(Description)
@IndexOther(CONCEPT)@\
CONCEPT stands for any terminal without special characters.

@IndexOther(CONCEPT-S)@\
CONCEPT-S stands for any CONCEPT terminal with special characters.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(GENERIC-STRING)@\
GENERIC-STRING stands for re-readable string format.
It is used in conjunction with the RE-READ mode.

@IndexOther(ISTYLE)@\
ISTYLE stands for tps running with an interface.

@IndexOther(SCRIBE)@\
SCRIBE stands for a file to be processed by SCRIBE before printing.@End(Description)

@Section(SAIL characters)

@Begin(Description)
@IndexOther(SAIL)@\
SAIL stands for a file (or terminal) with SAIL characters.@End(Description)

@Section(TeX)

@Begin(Description)
@IndexOther(TEX)@\
TEX stands for an output style to be run through TeX
(or LaTeX, if the flag LATEX-EMULATION is set).

@IndexOther(TEX-1)@\
TEX-1 stands for an output style to be run through TeX
(or LaTeX, if the flag LATEX-EMULATION is set).@End(Description)

@Section(X Windows)

@Begin(Description)
@IndexOther(XTERM)@\
XTERM stands for a terminal running xterm with normal font 
vtsingle and bold font vtsymbold.@End(Description)
@ChapterPh(Printing Properties)
The internal name of this category is 
PRINTPROP.
A printing property can be defined using DEFPRINTPROP.
Allowable properties are: @t{PRINTPROPTYPE}, @t{READFN}, @t{MHELP}.

@Section(Printing)

@Begin(Description)
@IndexOther(FO-SINGLE-SYMBOL)@\
If T, the symbol is special in first-order mode.
This will generally be the case for any new abbreviation.

@IndexOther(INFIX)@\
The binding priority of an infix operator.

@IndexOther(PREFIX)@\
The binding priority of a 'prefix operator'.

@IndexOther(PRINTNOTYPE)@\
If T, types of the symbol will never be printed.

@IndexOther(PRT-ASSOCIATIVE)@\
If T for an infix operator, it is assumed to be associative
for printing purposes.@End(Description)

@Section(wff Primitives)

@Begin(Description)
@IndexOther(FACE)@\


The face of a logical symbol, identical for all devices.  This may be
a list of symbols to be concatenated.  If left undefined in an
abbreviation, TPS will attempt to find a symbol in the current style
with the same name as the abbreviation.

The list of symbols can include symbols such as X, %, + or even | |
for an empty space, or the name of a special character.  In styles
which do not have a given special character, the name of the character
will be printed instead.

To see a list of names of special characters available in styles TEX and SCRIBE,
use HELP TEX-CHAR and HELP SCRIBE-CHAR.

To see a list of names of special characters available in style XTERM,
experts can evaluate the expression (mapcar 'car core::xterm-characters)
@End(Description)
@ChapterPh(Faces)
The internal name of this category is 
PRINT-FACE.
A face can be defined using DEFFACE.
Allowable properties are: @t{FACE}, @t{MHELP}.
@ChapterPh(Theories)
The internal name of this category is 
THEORY.
A theory can be defined using DEFTHEORY.
Allowable properties are: @t{GWFFS}, @t{RRULES}, @t{EXTENDS}, @t{OTHER-STUFF}, @t{RELATION-SIGN}, @t{REFLEXIVE}, @t{CONGRUENT}, @t{DERIVED-APPFN}, @t{DERIVED-REWFN}, @t{MHELP}.
@ChapterPh(Tex Special Characters)
The internal name of this category is 
TEX-CHAR.
A tex special character can be defined using DEFTEXFONT.
Allowable properties are: @t{TEXNAME}, @t{MHELP}.

@Section(TeX)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(%)@\

@IndexOther(->E)@\

@IndexOther(->I)@\

@IndexOther(<=)@\

@IndexOther(AE)@\

@IndexOther(AI)@\

@IndexOther(ALEPH)@\@t{@aleph@;}

@IndexOther(ALPHA)@\@t{@g{a}@;}

@IndexOther(AND)@\@t{@and@;}@\
No more help available.  Sorry.

@IndexOther(ANDI)@\

@IndexOther(ANDNOT)@\

@IndexOther(ANGLE)@\

@IndexOther(APPROX)@\@t{@approx@;}

@IndexOther(ARROW)@\

@IndexOther(ASSERT)@\@t{@assert@;}@\
No more help available.  Sorry.

@IndexOther(ASSERTEDBY)@\@t{@assertedby@;}

@IndexOther(ASTERISK)@\@t{@ast@;}

@IndexOther(BAR)@\

@IndexOther(BETA)@\@t{@g{b}@;}

@IndexOther(BIGBAR)@\

@IndexOther(BOLDA)@\@t{@bolda@;}

@IndexOther(BOLDB)@\@t{@boldb@;}

@IndexOther(BOLDC)@\@t{@boldc@;}

@IndexOther(BOLDD)@\@t{@boldd@;}

@IndexOther(BOLDE)@\@t{@bolde@;}

@IndexOther(BOLDF)@\@t{@boldf@;}

@IndexOther(BOLDG)@\@t{@boldg@;}

@IndexOther(BOLDH)@\@t{@boldh@;}

@IndexOther(BOLDI)@\@t{@boldi@;}

@IndexOther(BOLDJ)@\@t{@boldj@;}

@IndexOther(BOLDK)@\@t{@boldk@;}

@IndexOther(BOLDL)@\@t{@boldl@;}

@IndexOther(BOLDM)@\@t{@boldm@;}

@IndexOther(BOLDN)@\@t{@boldn@;}

@IndexOther(BOLDO)@\@t{@boldo@;}

@IndexOther(BOLDP)@\@t{@boldp@;}

@IndexOther(BOLDQ)@\@t{@boldq@;}

@IndexOther(BOLDR)@\@t{@boldr@;}

@IndexOther(BOLDS)@\@t{@bolds@;}

@IndexOther(BOLDT)@\@t{@boldt@;}

@IndexOther(BOLDU)@\@t{@boldu@;}

@IndexOther(BOLDV)@\@t{@boldv@;}

@IndexOther(BOLDW)@\@t{@boldw@;}

@IndexOther(BOLDX)@\@t{@boldx@;}

@IndexOther(BOLDY)@\@t{@boldy@;}

@IndexOther(BOLDZ)@\@t{@boldz@;}

@IndexOther(BOT)@\

@IndexOther(BOTTOM)@\

@IndexOther(CAPALPHA)@\@t{@g{A}@;}

@IndexOther(CAPBETA)@\@t{@g{B}@;}

@IndexOther(CAPCHI)@\@t{@g{C}@;}

@IndexOther(CAPDELTA)@\@t{@g{D}@;}

@IndexOther(CAPEPSILON)@\@t{@g{E}@;}

@IndexOther(CAPETA)@\@t{@g{H}@;}

@IndexOther(CAPGAMMA)@\@t{@g{G}@;}

@IndexOther(CAPIOTA)@\@t{@g{I}@;}

@IndexOther(CAPKAPPA)@\@t{@g{K}@;}

@IndexOther(CAPLAMBDA)@\@t{@g{L}@;}

@IndexOther(CAPMU)@\@t{@g{M}@;}

@IndexOther(CAPNU)@\@t{@g{N}@;}

@IndexOther(CAPOMEGA)@\@t{@g{W}@;}

@IndexOther(CAPOMICRON)@\@t{@g{O}@;}

@IndexOther(CAPPHI)@\@t{@g{F}@;}

@IndexOther(CAPPI)@\@t{@g{P}@;}

@IndexOther(CAPPSI)@\@t{@g{Y}@;}

@IndexOther(CAPRHO)@\@t{@g{R}@;}

@IndexOther(CAPSIGMA)@\@t{@g{S}@;}

@IndexOther(CAPTAU)@\@t{@g{T}@;}

@IndexOther(CAPTHETA)@\@t{@g{Q}@;}

@IndexOther(CAPUPSILON)@\@t{@g{U}@;}

@IndexOther(CAPXI)@\@t{@g{X}@;}

@IndexOther(CAPZETA)@\@t{@g{Z}@;}

@IndexOther(CEILING1)@\@t{@ceiling1@;}

@IndexOther(CEILING2)@\@t{@ceiling2@;}

@IndexOther(CHI)@\@t{@g{c}@;}

@IndexOther(CIRCLEDOT)@\@t{@circledot@;}

@IndexOther(CIRCLEMINUS)@\@t{@ominus@;}

@IndexOther(COMPOSE)@\@t{@compose@;}@\
No more help available.  Sorry.

@IndexOther(COND)@\

@IndexOther(CONTRACTION)@\

@IndexOther(CUT)@\@\
No more help available.  Sorry.

@IndexOther(DEFN)@\

@IndexOther(DEL)@\@t{@partial@;}

@IndexOther(DELTA)@\@t{@g{d}@;}

@IndexOther(DIAMOND)@\@t{@diamond@;}

@IndexOther(DIRECTSUM)@\@t{@directsum@;}

@IndexOther(DIVIDE)@\@t{@divide@;}

@IndexOther(ELBOW)@\

@IndexOther(EPSILON)@\@t{@g{e}@;}

@IndexOther(EQP)@\@t{@eqp@;}

@IndexOther(EQUIV)@\@t{@equiv@;}

@IndexOther(ETA)@\@t{@g{h}@;}

@IndexOther(EXISTS)@\@t{@exists@;}@\
No more help available.  Sorry.

@IndexOther(EXISTSI)@\

@IndexOther(EXISTSNOT)@\

@IndexOther(FALSEHOOD)@\@t{@falsehood@;}@\
No more help available.  Sorry.

@IndexOther(FINITE)@\

@IndexOther(FLAT)@\

@IndexOther(FLOOR1)@\@t{@floor1@;}

@IndexOther(FLOOR2)@\@t{@floor2@;}

@IndexOther(FORALL)@\@t{@forall@;}@\
No more help available.  Sorry.

@IndexOther(FORALLI)@\

@IndexOther(FORALLNOT)@\

@IndexOther(GAMMA)@\@t{@g{g}@;}

@IndexOther(GRADIENT)@\@t{@nabla@;}

@IndexOther(GREATEQ)@\@t{@greateq@;}

@IndexOther(IFF1)@\@t{@iff1@;}

@IndexOther(IFF2)@\@t{@iff2@;}

@IndexOther(IMP1)@\@t{@imp1@;}

@IndexOther(IMP2)@\@t{@imp2@;}

@IndexOther(IMPLIED1)@\@t{@implied1@;}

@IndexOther(IMPLIED2)@\@t{@implied2@;}

@IndexOther(IMPLIEDBY)@\@t{@impliedby@;}

@IndexOther(IMPLIES)@\@t{@implies@;}@\
No more help available.  Sorry.

@IndexOther(INFINITY)@\@t{@infinity@;}

@IndexOther(INTEGRAL2)@\

@IndexOther(INTERSECT)@\@t{@intersect@;}

@IndexOther(IOTA)@\@t{@g{i}@;}@\
No more help available.  Sorry.

@IndexOther(JOIN)@\@t{@join@;}

@IndexOther(KAPPA)@\@t{@g{k}@;}

@IndexOther(LAMBDA)@\@t{@g{l}@;}@\
No more help available.  Sorry.

@IndexOther(LESSEQ)@\@t{@lesseq@;}

@IndexOther(MEET)@\@t{@meet@;}

@IndexOther(MEMBER1)@\@t{@member1@;}

@IndexOther(MINPLUS)@\@t{@mp@;}

@IndexOther(MIX)@\

@IndexOther(MU)@\@t{@g{m}@;}

@IndexOther(NAT)@\

@IndexOther(NC)@\

@IndexOther(NEG)@\@t{@neg@;}@\
No more help available.  Sorry.

@IndexOther(NONMEMBER)@\@t{@nonmember@;}

@IndexOther(NORM)@\@t{@norm1@;}

@IndexOther(NORTH)@\@t{@north@;}

@IndexOther(NORTHEAST)@\@t{@northeast@;}

@IndexOther(NORTHWEST)@\@t{@northwest@;}

@IndexOther(NOT)@\@t{@not@;}@\
No more help available.  Sorry.

@IndexOther(NOTASSERT)@\@t{@notassert@;}

@IndexOther(NOTEQ)@\@t{@noteq@;}

@IndexOther(NOTEQUIV)@\@t{@notequiv@;}

@IndexOther(NOTNOT)@\

@IndexOther(NOTVALID)@\@t{@notvalid@;}

@IndexOther(NU)@\@t{@g{n}@;}

@IndexOther(NULLSET)@\@t{@emptyset@;}

@IndexOther(OMEGA)@\@t{@omega1@;}

@IndexOther(OMICRON)@\@t{@g{o}@;}

@IndexOther(ONE)@\@t{@one@;}

@IndexOther(OR)@\@t{@or@;}@\
No more help available.  Sorry.

@IndexOther(ORI)@\

@IndexOther(ORNOT)@\

@IndexOther(PHI)@\@t{@g{f}@;}

@IndexOther(PHI2)@\@t{@phi2@;}

@IndexOther(PI)@\@t{@g{p}@;}

@IndexOther(PLUSMIN)@\@t{@pm@;}

@IndexOther(POWERSET)@\@t{@powerset@;}

@IndexOther(PROPERSUBSET)@\@t{@PrSubset@;}

@IndexOther(PROPERSUPERSET)@\@t{@PrSupset@;}

@IndexOther(PSI)@\@t{@g{y}@;}

@IndexOther(RECURSION)@\

@IndexOther(RHO)@\@t{@g{r}@;}

@IndexOther(SCRIPTA)@\@t{@scripta@;}

@IndexOther(SCRIPTB)@\@t{@scriptb@;}

@IndexOther(SCRIPTC)@\@t{@scriptc@;}

@IndexOther(SCRIPTD)@\@t{@scriptd@;}

@IndexOther(SCRIPTE)@\@t{@scripte@;}

@IndexOther(SCRIPTF)@\@t{@scriptf@;}

@IndexOther(SCRIPTG)@\@t{@scriptg@;}

@IndexOther(SCRIPTH)@\@t{@scripth@;}

@IndexOther(SCRIPTI)@\@t{@scripti@;}

@IndexOther(SCRIPTJ)@\@t{@scriptj@;}

@IndexOther(SCRIPTK)@\@t{@scriptk@;}

@IndexOther(SCRIPTL)@\@t{@scriptl@;}

@IndexOther(SCRIPTM)@\@t{@scriptm@;}

@IndexOther(SCRIPTN)@\@t{@scriptn@;}

@IndexOther(SCRIPTO)@\@t{@scripto@;}

@IndexOther(SCRIPTP)@\@t{@scriptp@;}

@IndexOther(SCRIPTQ)@\@t{@scriptq@;}

@IndexOther(SCRIPTR)@\@t{@scriptr@;}

@IndexOther(SCRIPTS)@\@t{@scripts@;}

@IndexOther(SCRIPTT)@\@t{@scriptt@;}

@IndexOther(SCRIPTU)@\@t{@scriptu@;}

@IndexOther(SCRIPTV)@\@t{@scriptv@;}

@IndexOther(SCRIPTW)@\@t{@scriptw@;}

@IndexOther(SCRIPTX)@\@t{@scriptx@;}

@IndexOther(SCRIPTY)@\@t{@scripty@;}

@IndexOther(SCRIPTZ)@\@t{@scriptz@;}

@IndexOther(SETINTERSECT)@\@t{@setintersect@;}

@IndexOther(SETUNION)@\@t{@setunion@;}

@IndexOther(SIGMA)@\@t{@g{s}@;}

@IndexOther(SIMILAR)@\@t{@similar@;}

@IndexOther(SOUTH)@\@t{@south@;}

@IndexOther(SOUTHEAST)@\@t{@southeast@;}

@IndexOther(SOUTHWEST)@\

@IndexOther(SQRT)@\@t{@squareroot@;}

@IndexOther(SQUARE)@\@t{@square@;}

@IndexOther(STAR)@\@t{@star@;}

@IndexOther(SUB0)@\@t{@-{0}@;}

@IndexOther(SUB1)@\@t{@-{1}@;}

@IndexOther(SUB2)@\@t{@-{2}@;}@\
No more help available.  Sorry.

@IndexOther(SUB3)@\@t{@-{3}@;}

@IndexOther(SUB4)@\@t{@-{4}@;}

@IndexOther(SUB5)@\@t{@-{5}@;}

@IndexOther(SUB6)@\@t{@-{6}@;}

@IndexOther(SUB7)@\@t{@-{7}@;}

@IndexOther(SUB8)@\@t{@-{8}@;}

@IndexOther(SUB9)@\@t{@-{9}@;}

@IndexOther(SUBALPHA)@\@t{@-{@g{a}}@;}

@IndexOther(SUBBETA)@\@t{@-{@g{b}}@;}

@IndexOther(SUBCHI)@\@t{@-{@g{c}}@;}

@IndexOther(SUBDELTA)@\@t{@-{@g{d}}@;}

@IndexOther(SUBEPSILON)@\@t{@-{@g{e}}@;}

@IndexOther(SUBETA)@\@t{@-{@g{h}}@;}

@IndexOther(SUBGAMMA)@\@t{@-{@g{g}}@;}

@IndexOther(SUBIOTA)@\@t{@-{@g{i}}@;}

@IndexOther(SUBKAPPA)@\@t{@-{@g{k}}@;}

@IndexOther(SUBLAMBDA)@\@t{@-{@g{l}}@;}

@IndexOther(SUBLPAREN)@\@t{@-{(}@;}

@IndexOther(SUBMEMBER)@\@t{@submember@;}

@IndexOther(SUBMU)@\@t{@-{@g{m}}@;}

@IndexOther(SUBNU)@\@t{@-{@g{n}}@;}

@IndexOther(SUBNULLSET)@\@t{@subnullset@;}

@IndexOther(SUBOMEGA)@\@t{@-{@g{w}}@;}

@IndexOther(SUBOMICRON)@\@t{@-{@g{o}}@;}

@IndexOther(SUBPHI)@\@t{@-{@g{f}}@;}

@IndexOther(SUBPI)@\@t{@-{@g{p}}@;}

@IndexOther(SUBPSI)@\@t{@-{@g{y}}@;}

@IndexOther(SUBRHO)@\@t{@-{@g{r}}@;}

@IndexOther(SUBRPAREN)@\@t{@-{)}@;}

@IndexOther(SUBSET)@\@t{@subset@;}

@IndexOther(SUBSIGMA)@\@t{@-{@g{s}}@;}

@IndexOther(SUBTAU)@\@t{@-{@g{t}}@;}

@IndexOther(SUBTHETA)@\@t{@-{@g{q}}@;}

@IndexOther(SUBUPSILON)@\@t{@-{@g{u}}@;}

@IndexOther(SUBXI)@\@t{@-{@g{x}}@;}

@IndexOther(SUBZETA)@\@t{@-{@g{z}}@;}

@IndexOther(SUCC)@\

@IndexOther(SUP0)@\@t{@+{0}@;}

@IndexOther(SUP1)@\@t{@+{1}@;}

@IndexOther(SUP2)@\@t{@+{2}@;}

@IndexOther(SUP3)@\@t{@+{3}@;}

@IndexOther(SUP4)@\@t{@+{4}@;}

@IndexOther(SUP5)@\@t{@+{5}@;}

@IndexOther(SUP6)@\@t{@+{6}@;}

@IndexOther(SUP7)@\@t{@+{7}@;}

@IndexOther(SUP8)@\@t{@+{8}@;}

@IndexOther(SUP9)@\@t{@+{9}@;}

@IndexOther(SUPA)@\@t{@+{a}@;}

@IndexOther(SUPB)@\@t{@+{b}@;}

@IndexOther(SUPC)@\@t{@+{c}@;}

@IndexOther(SUPD)@\@t{@+{d}@;}

@IndexOther(SUPE)@\@t{@+{e}@;}

@IndexOther(SUPERSET)@\@t{@supset@;}

@IndexOther(SUPF)@\@t{@+{f}@;}

@IndexOther(SUPG)@\@t{@+{g}@;}

@IndexOther(SUPH)@\@t{@+{h}@;}

@IndexOther(SUPI)@\@t{@+{i}@;}

@IndexOther(SUPJ)@\@t{@+{j}@;}

@IndexOther(SUPK)@\@t{@+{k}@;}

@IndexOther(SUPL)@\@t{@+{l}@;}

@IndexOther(SUPLPAREN)@\@t{@+{(}@;}

@IndexOther(SUPM)@\@t{@+{m}@;}

@IndexOther(SUPMINUS)@\@t{@+{-}@;}

@IndexOther(SUPN)@\@t{@+{n}@;}

@IndexOther(SUPO)@\@t{@+{o}@;}

@IndexOther(SUPP)@\@t{@+{p}@;}

@IndexOther(SUPPLUS)@\@t{@+{+}@;}

@IndexOther(SUPQ)@\@t{@+{q}@;}

@IndexOther(SUPR)@\@t{@+{r}@;}

@IndexOther(SUPRPAREN)@\@t{@+{)}@;}

@IndexOther(SUPS)@\@t{@+{s}@;}

@IndexOther(SUPSET)@\

@IndexOther(SUPT)@\@t{@+{t}@;}

@IndexOther(SUPU)@\@t{@+{u}@;}

@IndexOther(SUPV)@\@t{@+{v}@;}

@IndexOther(SUPW)@\@t{@+{w}@;}

@IndexOther(SUPX)@\@t{@+{x}@;}

@IndexOther(SUPY)@\@t{@+{y}@;}

@IndexOther(SUPZ)@\@t{@+{z}@;}

@IndexOther(TAU)@\@t{@g{t}@;}

@IndexOther(TENSOR)@\@t{@tensor@;}

@IndexOther(THETA)@\@t{@theta1@;}

@IndexOther(TIMES)@\@t{@times@;}

@IndexOther(TRUTH)@\@t{@truth@;}@\
No more help available.  Sorry.

@IndexOther(TURNSTILE)@\

@IndexOther(UNION)@\@t{@union@;}

@IndexOther(UPSILON)@\@t{@g{u}@;}

@IndexOther(VALID)@\@t{@valid@;}

@IndexOther(XI)@\@t{@g{x}@;}

@IndexOther(ZERO)@\

@IndexOther(ZETA)@\@t{@g{z}@;}
@End(Description)
@ChapterPh(Rewriting Commands)
The internal name of this category is 
SEQNCMD.
A rewriting command can be defined using DEFSEQN.
Allowable properties are: @t{S-EQN-ARGTYPES}, @t{S-EQN-ARGNAMES}, @t{S-EQN-ARGHELP}, @t{S-EQN-DEFAULTFNS}, @t{S-EQN-MAINFNS}, @t{S-EQN-CLOSEFNS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(ASSERT-TOP) @i{line} @i{p1}@\
Leave the REWRITING top-level, inserting the obtained relation as a
lemma into the current natural deduction proof.

@IndexOther(BEGIN-PRFW)@\
Begin proofwindow top level.
Open Current Subproof, Current Subproof & Line Numbers, and Complete Proof
windows with text size determined by the value of the flag CHARSIZE.
Printing in various windows can be modified by changing the flags
PROOFW-ALL, BLANK-LINES-INSERTED and PRINTLINEFLAG.
The initial size of the windows can be modified with the flags
PROOFW-ALL-HEIGHT and PROOFW-ALL-WIDTH; after the windows are open, they can
simply be resized as normal. PSTATUS will update the proofwindows manually
if necessary. 
Close the proofwindows with END-PRFW.

@IndexOther(END-PRFW)@\
End REW-PRFW top level; close all open proofwindows.

@IndexOther(LEAVE)@\
Leave the REWRITING top level.

@IndexOther(OK) @i{p2} @i{p1} @i{a} @i{b} @i{p2-hyps} @i{p1-hyps} @i{num}@\
Leave the REWRITING top level, completing a REWRITE command.@End(Description)

@Section(Starting and Finishing)

@Begin(Description)
@IndexOther(DERIVE) @i{wff} @i{prefix}@\
Begin a rewrite derivation without a fixed target wff.

@IndexOther(DERIVE-IN) @i{theory} @i{wff} @i{prefix}@\
Start a derivation by rewriting using a particular theory.

@IndexOther(DONE) @i{p1}@\
Check whether the current derivation is complete. For rewriting
proofs, DONE checks whether the target line was obtained from the initial
line. In case of derivations without a target line, DONE prompts for a
line which is to be regarded as the target.

@IndexOther(PROOFLIST)@\
Print a list of all rewrite derivations currently in memory.
For proofs, the corresponding proof assertions are printed. For general
derivations, the corresponding initial lines are printed.

@IndexOther(PROVE) @i{relation} @i{prefix} @i{num}@\
Prove a relation by rewriting.

@IndexOther(PROVE-IN) @i{theory} @i{relation} @i{prefix} @i{num}@\
Prove a relation by rewriting using a particular theory.

@IndexOther(RECONSIDER) @i{prefix}@\
Reconsider a derivation. The following derivations are in memory:

For more details, use the PROOFLIST command.

@IndexOther(RESTOREPROOF) @i{savefile}@\
Reads a rewriting proof from a file created by SAVEPROOF
and makes it the current proof.  A security feature prevents the 
restoration of saved proofs which have been altered in any way.
Retrieve any definitions which are used in the proof and stored in the
library before restoring the proof. If you don't specify a directory,
it will first try your home directory and then all the directories 
listed in SOURCE-PATH.

@IndexOther(SAVEPROOF) @i{savefile}@\
Saves the current rewriting proof to the specified file in
a form in which it can be restored.  Use RESTOREPROOF to restore the proof.
Overwrites the file if it already exists.@End(Description)

@Section(Printing)

@Begin(Description)
@IndexOther(PALL)@\
Print all the lines in the current derivation.

@IndexOther(TEXPROOF) @i{filename} @i{timing}@\
Print the current proof into a tex file.
After leaving tps, run this .tex file through tex and print the resulting
file.

Many flags affect the output of texproof.
See: USE-INTERNAL-PRINT-MODE, TURNSTILE-INDENT-AUTO, TURNSTILE-INDENT,
LATEX-EMULATION, TEX-MIMIC-SCRIBE, PPWFFLAG, DISPLAYWFF, INFIX-NOTATION,
PAGELENGTH, PAGEWIDTH, TEX-BREAK-BEFORE-SYMBOLS, LOCALLEFTFLAG, SCOPE,
ALLSCOPEFLAG, USE-DOT, FIRST-ORDER-PRINT-MODE, FILLINEFLAG, ATOMVALFLAG.@End(Description)

@Section(Applying Rules)

@Begin(Description)
@IndexOther(ANY) @i{p1} @i{p2} @i{a} @i{b}@\
Try to apply any active rewrite rule from the current theory and all
its subtheories. If there is no current theory, all active rewrite rules
will be tried.

@IndexOther(ANY*) @i{p1} @i{p2} @i{a} @i{b}@\
Justify a line by a sequence of applications of any active
rewrite rules from the current theory in the forward direction, starting
from a preceding line. In most cases, this command will apply rewrite
rules in the forward direction as often as possible or until a specified
target wff is obtained. If the wff after rewriting is specified but the
one before rewriting is set to NIL, rewrite rules will be applied in the
backward direction, starting from the target formula.
CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL.

@IndexOther(ANY*-IN) @i{theory} @i{p1} @i{p2} @i{a} @i{b}@\
Justify a line by a sequence of applications of any active
rewrite rules from the specified subtheory of the current theory in the
forward direction, starting from a preceding line. In most cases, this
command will apply rewrite rules in the forward direction as often as
possible or until a specified target wff is obtained. If the wff after
rewriting is specified but the one before rewriting is set to NIL,
rewrite rules will be applied in the backward direction, starting from
the target formula.
CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL.

@IndexOther(APP) @i{rule} @i{p1} @i{p2} @i{a} @i{b}@\
Apply a rewrite rule.

@IndexOther(APP*) @i{rule} @i{p1} @i{p2} @i{a} @i{b}@\
Justify a line by a sequence of applications of a rewrite rule in
the forward direction, starting from a preceding line. In most cases, this
command will apply a rewrite rule in the forward direction as often as
possible or until a specified target wff is obtained. If the wff after
rewriting is specified but the one before rewriting is set to NIL, the rewrite
rule will be applied in the backward direction, starting from the target
formula. CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL.

@IndexOther(AUTO) @i{p1} @i{p2} @i{a} @i{b}@\
Search for a rewrite sequence between two lines using any active
rewrite rules from the current theory. The exact behaviour is affected by
following flags: REWRITING-AUTO-DEPTH, REWRITING-AUTO-TABLE-SIZE,
REWRITING-AUTO-MAX-WFF-SIZE, REWRITING-AUTO-SUBSTS

@IndexOther(SAME) @i{p1} @i{p2} @i{a} @i{b}@\
Use reflexivity of equality. The wffs A and B need to be identical
up to alphabetic change of bound variables.

@IndexOther(UNANY*) @i{p1} @i{p2} @i{a} @i{b}@\
Justify a line by a sequence of applications of any active
rewrite rules from the current theory in the backward direction, starting
from a preceding line. In most cases, this command will apply rewrite
rules in the backward direction as often as possible or until a specified
target wff is obtained. If the wff after rewriting is specified but the
one before rewriting is set to NIL, rewrite rules will be applied in the
forward direction, starting from the target formula.
CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL.

@IndexOther(UNANY*-IN) @i{theory} @i{p1} @i{p2} @i{a} @i{b}@\
Justify a line by a sequence of applications of any active
rewrite rules from the specified subtheory of the current theory in the
backward direction, starting from a preceding line. In most cases, this
command will apply rewrite rules in the backward direction as often as
possible or until a specified target wff is obtained. If the wff after
rewriting is specified but the one before rewriting is set to NIL,
rewrite rules will be applied in the forward direction, starting from
the target formula.
CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL.

@IndexOther(UNAPP*) @i{rule} @i{p1} @i{p2} @i{a} @i{b}@\
Justify a line by a sequence of applications of a rewrite rule in
the backward direction, starting from a preceding line. In most cases, this
command will apply a rewrite rule in the backward direction as often as
possible or until a specified target wff is obtained. If the wff after
rewriting is specified but the one before rewriting is set to NIL, the rewrite
rule will be applied in the forward direction, starting from the target
formula. CAUTION: may not terminate if APP*-REWRITE-DEPTH is set to NIL.@End(Description)

@Section(Rearranging the Derivation)

@Begin(Description)
@IndexOther(CLEANUP)@\
Deletes unnecessary lines from a derivation.

@IndexOther(CONNECT) @i{p1} @i{p2}@\
Given two identical lines, delete the lower one, rearranging the
derivation appropriately. With symmetric relations, the command will also
rearrange the lines from which the higher-numbered line was obtained to
follow from the lower-numbered line.

@IndexOther(DELETE) @i{del-lines}@\
      Delete lines from the proof outline.

@IndexOther(INTRODUCE-GAP) @i{line} @i{num}@\
Introduce a gap in an existing derivation.

@IndexOther(MOVE) @i{old-line} @i{new-line}@\
Renumber one particular line.

@IndexOther(SQUEEZE)@\
Removes unnecessary gaps from the derivation.@End(Description)

@Section(Lambda Conversion)

@Begin(Description)
@IndexOther(BETA-EQ) @i{p1} @i{p2} @i{a} @i{b}@\
Assert that two lines are beta-equivalent.

@IndexOther(BETA-NF) @i{p1} @i{p2} @i{a}@\
Beta-normalize a line.

@IndexOther(ETA-EQ) @i{p1} @i{p2} @i{a} @i{b}@\
Assert that two lines are eta-equivalent.

@IndexOther(ETA-NF) @i{p1} @i{p2} @i{a}@\
Eta-normalize a line.

@IndexOther(LAMBDA-EQ) @i{p1} @i{p2} @i{a} @i{b}@\
Assert that two lines are lambda-equivalent.

@IndexOther(LAMBDA-NF) @i{p1} @i{p2} @i{a}@\
Lambda-normalize a line.

@IndexOther(LONG-ETA-NF) @i{p1} @i{p2} @i{a}@\
Compute the long-eta normal form of a line.@End(Description)

@Section(Theories)

@Begin(Description)
@IndexOther(CURRENT-THEORY)@\
    Show the theory associated with current rewrite derivation.

@IndexOther(DERIVE-RRULE) @i{left} @i{right} @i{name} @i{help} @i{typelist} @i{bidir}@\
Create a derived rewrite rule from two provably related lines.
If the relation was proven using bidirectional rules only, the derived rule
may be made bidirectional.

@IndexOther(MAKE-RRULE) @i{name} @i{gwff1} @i{gwff2} @i{types} @i{bidir} @i{appfn} @i{rewfn} @i{vars} @i{mhelp}@\
Create a new rewrite rule with the given left and right
sides in memory.

@IndexOther(SAVE-RRULE) @i{name}@\
Save a rewrite rule into the library.@End(Description)
@ChapterPh(Scribe Special Characters)
The internal name of this category is 
SCRIBE-CHAR.
A scribe special character can be defined using DEFSCRIBEFONT.
Allowable properties are: @t{DFONT}.

@Section(Script Letters)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(SCRIPTA)@\@t{@scripta@;}

@IndexOther(SCRIPTB)@\@t{@scriptb@;}

@IndexOther(SCRIPTC)@\@t{@scriptc@;}

@IndexOther(SCRIPTD)@\@t{@scriptd@;}

@IndexOther(SCRIPTE)@\@t{@scripte@;}

@IndexOther(SCRIPTF)@\@t{@scriptf@;}

@IndexOther(SCRIPTG)@\@t{@scriptg@;}

@IndexOther(SCRIPTH)@\@t{@scripth@;}

@IndexOther(SCRIPTI)@\@t{@scripti@;}

@IndexOther(SCRIPTJ)@\@t{@scriptj@;}

@IndexOther(SCRIPTK)@\@t{@scriptk@;}

@IndexOther(SCRIPTL)@\@t{@scriptl@;}

@IndexOther(SCRIPTM)@\@t{@scriptm@;}

@IndexOther(SCRIPTN)@\@t{@scriptn@;}

@IndexOther(SCRIPTO)@\@t{@scripto@;}

@IndexOther(SCRIPTP)@\@t{@scriptp@;}

@IndexOther(SCRIPTQ)@\@t{@scriptq@;}

@IndexOther(SCRIPTR)@\@t{@scriptr@;}

@IndexOther(SCRIPTS)@\@t{@scripts@;}

@IndexOther(SCRIPTT)@\@t{@scriptt@;}

@IndexOther(SCRIPTU)@\@t{@scriptu@;}

@IndexOther(SCRIPTV)@\@t{@scriptv@;}

@IndexOther(SCRIPTW)@\@t{@scriptw@;}

@IndexOther(SCRIPTX)@\@t{@scriptx@;}

@IndexOther(SCRIPTY)@\@t{@scripty@;}

@IndexOther(SCRIPTZ)@\@t{@scriptz@;}
@End(Description)

@Section(Subscripts)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(SUB0)@\@t{@-{0}@;}

@IndexOther(SUB1)@\@t{@-{1}@;}

@IndexOther(SUB2)@\@t{@-{2}@;}@\
No more help available.  Sorry.

@IndexOther(SUB3)@\@t{@-{3}@;}

@IndexOther(SUB4)@\@t{@-{4}@;}

@IndexOther(SUB5)@\@t{@-{5}@;}

@IndexOther(SUB6)@\@t{@-{6}@;}

@IndexOther(SUB7)@\@t{@-{7}@;}

@IndexOther(SUB8)@\@t{@-{8}@;}

@IndexOther(SUB9)@\@t{@-{9}@;}

@IndexOther(SUBLPAREN)@\@t{@-{(}@;}

@IndexOther(SUBMEMBER)@\@t{@submember@;}

@IndexOther(SUBNULLSET)@\@t{@subnullset@;}

@IndexOther(SUBRPAREN)@\@t{@-{)}@;}
@End(Description)

@Section(Superscripts)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(SUP0)@\@t{@+{0}@;}

@IndexOther(SUP1)@\@t{@+{1}@;}

@IndexOther(SUP2)@\@t{@+{2}@;}

@IndexOther(SUP3)@\@t{@+{3}@;}

@IndexOther(SUP4)@\@t{@+{4}@;}

@IndexOther(SUP5)@\@t{@+{5}@;}

@IndexOther(SUP6)@\@t{@+{6}@;}

@IndexOther(SUP7)@\@t{@+{7}@;}

@IndexOther(SUP8)@\@t{@+{8}@;}

@IndexOther(SUP9)@\@t{@+{9}@;}

@IndexOther(SUPA)@\@t{@+{a}@;}

@IndexOther(SUPB)@\@t{@+{b}@;}

@IndexOther(SUPC)@\@t{@+{c}@;}

@IndexOther(SUPD)@\@t{@+{d}@;}

@IndexOther(SUPE)@\@t{@+{e}@;}

@IndexOther(SUPF)@\@t{@+{f}@;}

@IndexOther(SUPG)@\@t{@+{g}@;}

@IndexOther(SUPH)@\@t{@+{h}@;}

@IndexOther(SUPI)@\@t{@+{i}@;}

@IndexOther(SUPJ)@\@t{@+{j}@;}

@IndexOther(SUPK)@\@t{@+{k}@;}

@IndexOther(SUPL)@\@t{@+{l}@;}

@IndexOther(SUPLPAREN)@\@t{@+{(}@;}

@IndexOther(SUPM)@\@t{@+{m}@;}

@IndexOther(SUPMINUS)@\@t{@+{-}@;}

@IndexOther(SUPN)@\@t{@+{n}@;}

@IndexOther(SUPO)@\@t{@+{o}@;}

@IndexOther(SUPP)@\@t{@+{p}@;}

@IndexOther(SUPPLUS)@\@t{@+{+}@;}

@IndexOther(SUPQ)@\@t{@+{q}@;}

@IndexOther(SUPR)@\@t{@+{r}@;}

@IndexOther(SUPRPAREN)@\@t{@+{)}@;}

@IndexOther(SUPS)@\@t{@+{s}@;}

@IndexOther(SUPT)@\@t{@+{t}@;}

@IndexOther(SUPU)@\@t{@+{u}@;}

@IndexOther(SUPV)@\@t{@+{v}@;}

@IndexOther(SUPW)@\@t{@+{w}@;}

@IndexOther(SUPX)@\@t{@+{x}@;}

@IndexOther(SUPY)@\@t{@+{y}@;}

@IndexOther(SUPZ)@\@t{@+{z}@;}
@End(Description)

@Section(Lowercase Greek)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(ALPHA)@\@t{@g{a}@;}

@IndexOther(BETA)@\@t{@g{b}@;}

@IndexOther(CHI)@\@t{@g{c}@;}

@IndexOther(DELTA)@\@t{@g{d}@;}

@IndexOther(EPSILON)@\@t{@g{e}@;}

@IndexOther(ETA)@\@t{@g{h}@;}

@IndexOther(GAMMA)@\@t{@g{g}@;}

@IndexOther(IOTA)@\@t{@g{i}@;}@\
No more help available.  Sorry.

@IndexOther(KAPPA)@\@t{@g{k}@;}

@IndexOther(LAMBDA)@\@t{@g{l}@;}@\
No more help available.  Sorry.

@IndexOther(MU)@\@t{@g{m}@;}

@IndexOther(NU)@\@t{@g{n}@;}

@IndexOther(OMEGA)@\@t{@omega1@;}

@IndexOther(OMICRON)@\@t{@g{o}@;}

@IndexOther(PHI)@\@t{@g{f}@;}

@IndexOther(PI)@\@t{@g{p}@;}

@IndexOther(PSI)@\@t{@g{y}@;}

@IndexOther(RHO)@\@t{@g{r}@;}

@IndexOther(SIGMA)@\@t{@g{s}@;}

@IndexOther(TAU)@\@t{@g{t}@;}

@IndexOther(THETA)@\@t{@theta1@;}

@IndexOther(UPSILON)@\@t{@g{u}@;}

@IndexOther(XI)@\@t{@g{x}@;}

@IndexOther(ZETA)@\@t{@g{z}@;}
@End(Description)

@Section(Uppercase Greek)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(CAPALPHA)@\@t{@g{A}@;}

@IndexOther(CAPBETA)@\@t{@g{B}@;}

@IndexOther(CAPCHI)@\@t{@g{C}@;}

@IndexOther(CAPDELTA)@\@t{@g{D}@;}

@IndexOther(CAPEPSILON)@\@t{@g{E}@;}

@IndexOther(CAPETA)@\@t{@g{H}@;}

@IndexOther(CAPGAMMA)@\@t{@g{G}@;}

@IndexOther(CAPIOTA)@\@t{@g{I}@;}

@IndexOther(CAPKAPPA)@\@t{@g{K}@;}

@IndexOther(CAPLAMBDA)@\@t{@g{L}@;}

@IndexOther(CAPMU)@\@t{@g{M}@;}

@IndexOther(CAPNU)@\@t{@g{N}@;}

@IndexOther(CAPOMEGA)@\@t{@g{W}@;}

@IndexOther(CAPOMICRON)@\@t{@g{O}@;}

@IndexOther(CAPPHI)@\@t{@g{F}@;}

@IndexOther(CAPPI)@\@t{@g{P}@;}

@IndexOther(CAPPSI)@\@t{@g{Y}@;}

@IndexOther(CAPRHO)@\@t{@g{R}@;}

@IndexOther(CAPSIGMA)@\@t{@g{S}@;}

@IndexOther(CAPTAU)@\@t{@g{T}@;}

@IndexOther(CAPTHETA)@\@t{@g{Q}@;}

@IndexOther(CAPUPSILON)@\@t{@g{U}@;}

@IndexOther(CAPXI)@\@t{@g{X}@;}

@IndexOther(CAPZETA)@\@t{@g{Z}@;}
@End(Description)

@Section(Greek Subscripts)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(SUBALPHA)@\@t{@-{@g{a}}@;}

@IndexOther(SUBBETA)@\@t{@-{@g{b}}@;}

@IndexOther(SUBCHI)@\@t{@-{@g{c}}@;}

@IndexOther(SUBDELTA)@\@t{@-{@g{d}}@;}

@IndexOther(SUBEPSILON)@\@t{@-{@g{e}}@;}

@IndexOther(SUBETA)@\@t{@-{@g{h}}@;}

@IndexOther(SUBGAMMA)@\@t{@-{@g{g}}@;}

@IndexOther(SUBIOTA)@\@t{@-{@g{i}}@;}

@IndexOther(SUBKAPPA)@\@t{@-{@g{k}}@;}

@IndexOther(SUBLAMBDA)@\@t{@-{@g{l}}@;}

@IndexOther(SUBMU)@\@t{@-{@g{m}}@;}

@IndexOther(SUBNU)@\@t{@-{@g{n}}@;}

@IndexOther(SUBOMEGA)@\@t{@-{@g{w}}@;}

@IndexOther(SUBOMICRON)@\@t{@-{@g{o}}@;}

@IndexOther(SUBPHI)@\@t{@-{@g{f}}@;}

@IndexOther(SUBPI)@\@t{@-{@g{p}}@;}

@IndexOther(SUBPSI)@\@t{@-{@g{y}}@;}

@IndexOther(SUBRHO)@\@t{@-{@g{r}}@;}

@IndexOther(SUBSIGMA)@\@t{@-{@g{s}}@;}

@IndexOther(SUBTAU)@\@t{@-{@g{t}}@;}

@IndexOther(SUBTHETA)@\@t{@-{@g{q}}@;}

@IndexOther(SUBUPSILON)@\@t{@-{@g{u}}@;}

@IndexOther(SUBXI)@\@t{@-{@g{x}}@;}

@IndexOther(SUBZETA)@\@t{@-{@g{z}}@;}
@End(Description)

@Section(Bold Letters)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(BOLDA)@\@t{@bolda@;}

@IndexOther(BOLDB)@\@t{@boldb@;}

@IndexOther(BOLDC)@\@t{@boldc@;}

@IndexOther(BOLDD)@\@t{@boldd@;}

@IndexOther(BOLDE)@\@t{@bolde@;}

@IndexOther(BOLDF)@\@t{@boldf@;}

@IndexOther(BOLDG)@\@t{@boldg@;}

@IndexOther(BOLDH)@\@t{@boldh@;}

@IndexOther(BOLDI)@\@t{@boldi@;}

@IndexOther(BOLDJ)@\@t{@boldj@;}

@IndexOther(BOLDK)@\@t{@boldk@;}

@IndexOther(BOLDL)@\@t{@boldl@;}

@IndexOther(BOLDM)@\@t{@boldm@;}

@IndexOther(BOLDN)@\@t{@boldn@;}

@IndexOther(BOLDO)@\@t{@boldo@;}

@IndexOther(BOLDP)@\@t{@boldp@;}

@IndexOther(BOLDQ)@\@t{@boldq@;}

@IndexOther(BOLDR)@\@t{@boldr@;}

@IndexOther(BOLDS)@\@t{@bolds@;}

@IndexOther(BOLDT)@\@t{@boldt@;}

@IndexOther(BOLDU)@\@t{@boldu@;}

@IndexOther(BOLDV)@\@t{@boldv@;}

@IndexOther(BOLDW)@\@t{@boldw@;}

@IndexOther(BOLDX)@\@t{@boldx@;}

@IndexOther(BOLDY)@\@t{@boldy@;}

@IndexOther(BOLDZ)@\@t{@boldz@;}
@End(Description)

@Section(Other Symbols)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(!)@\@t{@assert@;}

@IndexOther(ALEPH)@\@t{@aleph@;}

@IndexOther(AND)@\@t{@and@;}@\
No more help available.  Sorry.

@IndexOther(APPROX)@\@t{@approx@;}

@IndexOther(ASSERT)@\@t{@assert@;}@\
No more help available.  Sorry.

@IndexOther(ASSERTEDBY)@\@t{@assertedby@;}

@IndexOther(ASTERISK)@\@t{@ast@;}

@IndexOther(CEILING1)@\@t{@ceiling1@;}

@IndexOther(CEILING2)@\@t{@ceiling2@;}

@IndexOther(CIRCLEDOT)@\@t{@circledot@;}

@IndexOther(CIRCLEMINUS)@\@t{@ominus@;}

@IndexOther(COMPOSE)@\@t{@compose@;}@\
No more help available.  Sorry.

@IndexOther(DEL)@\@t{@partial@;}

@IndexOther(DIAMOND)@\@t{@diamond@;}

@IndexOther(DIRECTSUM)@\@t{@directsum@;}

@IndexOther(DIVIDE)@\@t{@divide@;}

@IndexOther(DOUBTILDE)@\@t{@approx@;}

@IndexOther(EQP)@\@t{@eqp@;}

@IndexOther(EQUIV)@\@t{@equiv@;}

@IndexOther(EXISTS)@\@t{@exists@;}@\
No more help available.  Sorry.

@IndexOther(FALSEHOOD)@\@t{@falsehood@;}@\
No more help available.  Sorry.

@IndexOther(FLOOR1)@\@t{@floor1@;}

@IndexOther(FLOOR2)@\@t{@floor2@;}

@IndexOther(FORALL)@\@t{@forall@;}@\
No more help available.  Sorry.

@IndexOther(GRADIENT)@\@t{@nabla@;}

@IndexOther(GREATEQ)@\@t{@greateq@;}

@IndexOther(IFF1)@\@t{@iff1@;}

@IndexOther(IFF2)@\@t{@iff2@;}

@IndexOther(IMP1)@\@t{@imp1@;}

@IndexOther(IMP2)@\@t{@imp2@;}

@IndexOther(IMP3)@\@t{@imp3@;}

@IndexOther(IMPLIED1)@\@t{@implied1@;}

@IndexOther(IMPLIED2)@\@t{@implied2@;}

@IndexOther(IMPLIEDBY)@\@t{@impliedby@;}

@IndexOther(IMPLIES)@\@t{@implies@;}@\
No more help available.  Sorry.

@IndexOther(INFINITY)@\@t{@infinity@;}

@IndexOther(INTERSECT)@\@t{@intersect@;}

@IndexOther(JOIN)@\@t{@join@;}

@IndexOther(LESSEQ)@\@t{@lesseq@;}

@IndexOther(MEET)@\@t{@meet@;}

@IndexOther(MEMBER1)@\@t{@member1@;}

@IndexOther(MINPLUS)@\@t{@mp@;}

@IndexOther(NEG)@\@t{@neg@;}@\
No more help available.  Sorry.

@IndexOther(NEWPAR)@\@t{@newpar@;}

@IndexOther(NONMEMBER)@\@t{@nonmember@;}

@IndexOther(NORM)@\@t{@norm1@;}

@IndexOther(NORTH)@\@t{@north@;}

@IndexOther(NORTHEAST)@\@t{@northeast@;}

@IndexOther(NORTHWEST)@\@t{@northwest@;}

@IndexOther(NOT)@\@t{@not@;}@\
No more help available.  Sorry.

@IndexOther(NOTASSERT)@\@t{@notassert@;}

@IndexOther(NOTEQ)@\@t{@noteq@;}

@IndexOther(NOTEQUIV)@\@t{@notequiv@;}

@IndexOther(NOTVALID)@\@t{@notvalid@;}

@IndexOther(NULLSET)@\@t{@emptyset@;}

@IndexOther(ONE)@\@t{@one@;}

@IndexOther(OR)@\@t{@or@;}@\
No more help available.  Sorry.

@IndexOther(PHI2)@\@t{@phi2@;}

@IndexOther(PLUSMIN)@\@t{@pm@;}

@IndexOther(POWERSET)@\@t{@powerset@;}

@IndexOther(PROPERSUBSET)@\@t{@PrSubset@;}

@IndexOther(PROPERSUPERSET)@\@t{@PrSupset@;}

@IndexOther(SETINTERSECT)@\@t{@setintersect@;}

@IndexOther(SETUNION)@\@t{@setunion@;}

@IndexOther(SIMILAR)@\@t{@similar@;}

@IndexOther(SOUTH)@\@t{@south@;}

@IndexOther(SOUTHEAST)@\@t{@southeast@;}

@IndexOther(SQRT)@\@t{@squareroot@;}

@IndexOther(SQUARE)@\@t{@square@;}

@IndexOther(STAR)@\@t{@star@;}

@IndexOther(SUBSET)@\@t{@subset@;}

@IndexOther(SUPERSET)@\@t{@supset@;}

@IndexOther(TENSOR)@\@t{@tensor@;}

@IndexOther(TIMES)@\@t{@times@;}

@IndexOther(TRUTH)@\@t{@truth@;}@\
No more help available.  Sorry.

@IndexOther(UNION)@\@t{@union@;}

@IndexOther(VALID)@\@t{@valid@;}
@End(Description)
@ChapterPh(Saved Wffs)
The internal name of this category is 
SAVEDWFF.
A saved wff can be defined using DEFSAVEDWFF.
Allowable properties are: @t{REPRESENTS}, @t{MHELP}.

@Section(First-Order Logic)

@Begin(Description)
@IndexOther(X2200)@\
No more help available.  Sorry.

@IndexOther(X2201)@\
No more help available.  Sorry.

@IndexOther(X2202)@\
No more help available.  Sorry.

@IndexOther(X2203)@\
No more help available.  Sorry.

@IndexOther(X2204)@\
No more help available.  Sorry.

@IndexOther(X2205)@\
No more help available.  Sorry.

@IndexOther(X2206)@\
No more help available.  Sorry.

@IndexOther(X2207)@\
No more help available.  Sorry.

@IndexOther(X2208)@\
No more help available.  Sorry.

@IndexOther(X2209)@\
No more help available.  Sorry.

@IndexOther(X2210)@\
No more help available.  Sorry.

@IndexOther(X2211)@\
No more help available.  Sorry.

@IndexOther(X2212)@\
No more help available.  Sorry.

@IndexOther(X2213)@\
No more help available.  Sorry.

@IndexOther(X2214)@\
No more help available.  Sorry.@End(Description)
@ChapterPh(Intermediate Rule Definitions)
The internal name of this category is 
RULEHELP.
An intermediate rule definition can be defined using DEFRULEHELP.
Allowable properties are: @t{LINES}, @t{RESTRICTIONS}, @t{PRIORITY}, @t{SUPPORT-TRANSFORMATION}, @t{ITEMSHELP}, @t{MHELP}.

@Section(Modules)

@Begin(Description)
@IndexOther(AB*)@\
Rule to alphabetically change embedded quantified variables.

@IndexOther(ABE)@\
Rule to change a top level occurrence of an existentially
 quantified variable.

@IndexOther(ABSURD)@\
Rule of Intuitionistic Absurdity.

@IndexOther(ABU)@\
Rule to change a top level occurrence of a universally quantified
 variable.

@IndexOther(ASSOC-LEFT)@\
Rule to associate a support line leftwards. Use before
calling CASES3 or CASES4.

@IndexOther(BETA*)@\
Rule to infer a line from one which is equal up to lambda conversion
using beta rule (but NOT eta rule) and alphabetic change of bound variables.

@IndexOther(CASES)@\
Rule of Cases.

@IndexOther(CASES3)@\
Rule of Cases.

@IndexOther(CASES4)@\
Rule of Cases.

@IndexOther(DEDUCT)@\
The deduction rule.

@IndexOther(DISJ-IMP)@\
Rule to replace a disjunction by an implication.

@IndexOther(DISJ-IMP-L)@\
Rule to replace a disjunction by an implication.

@IndexOther(DISJ-IMP-R)@\
Rule to replace a disjunction by an implication.

@IndexOther(ECONJ)@\
Rule to infer two conjuncts from a conjunction.

@IndexOther(EDEF)@\
Rule to eliminate first definition, left to right.

@IndexOther(EGEN)@\
Rule of Existential Generalization.

@IndexOther(ENEG)@\
Rule of Negation Elimination.

@IndexOther(EQUIV-EQ)@\
Rule to infer a line from one which is equal up to 
definitions, lambda conversion, alphabetic change of bound variables 
and the Leibniz definition of the symbol = . You may use the editor 
command EXPAND= to create the desired line from the existing one.

@IndexOther(EQUIV-EQ-CONTR)@\
Rule to contract the outermost instance of the Leibniz definition of 
equality into instances of the symbol = .

@IndexOther(EQUIV-EQ-CONTR*)@\
Rule to contract all instances of the Leibniz definition of 
equality into instances of the symbol = .

@IndexOther(EQUIV-EQ-EXPD)@\
Rule to expand the outermost equality using the Leibniz definition.

@IndexOther(EQUIV-EQ-EXPD*)@\
Rule to expand all equalities using the Leibniz definition.

@IndexOther(EQUIV-IMPLICS)@\
Rule to convert an equivalence into twin implications.

@IndexOther(EQUIV-WFFS)@\
Rule to assert equivalence of lines up to definition.

@IndexOther(ETA*)@\
Rule to infer a line from one which is equal up to lambda conversion
using eta rule (but NOT beta rule) and alphabetic change of bound variables.

@IndexOther(EXT=)@\
Rule of Extensionality.

@IndexOther(EXT=0)@\
Rule to convert equality at type o into an equivalence.

@IndexOther(HYP)@\
Introduce a new hypothesis line into the proof outline.

@IndexOther(ICONJ)@\
Rule to infer a conjunction from two conjuncts.

@IndexOther(IDEF)@\
Rule to introduce a definition.

@IndexOther(IDISJ-LEFT)@\
Introduce a disjunction (left version).

@IndexOther(IDISJ-RIGHT)@\
Introduce a disjunction (right version).

@IndexOther(IMP-DISJ)@\
Rule to replace an implication by a disjunction.

@IndexOther(IMP-DISJ-L)@\
Rule to replace an implication by a disjunction.

@IndexOther(IMP-DISJ-R)@\
Rule to replace an implication by a disjunction.

@IndexOther(IMPLICS-EQUIV)@\
Rule to convert twin implications into an equivalence.

@IndexOther(INDIRECT)@\
Rule of Indirect Proof.

@IndexOther(INDIRECT1)@\
Rule of Indirect Proof Using One Contradictory Line.

@IndexOther(INDIRECT2)@\
Rule of Indirect Proof Using Two Contradictory Lines.

@IndexOther(INEG)@\
Rule of Negation Introduction

@IndexOther(ITRUTH)@\
Rule to infer TRUTH

@IndexOther(LAMBDA*)@\
Rule to infer a line from one which is equal up to lambda conversion
using both beta and eta rules and alphabetic change of bound variables.

@IndexOther(LCONTR*)@\
Rule to put an inferred line into Lambda-normal form using both 
beta and eta conversion.

@IndexOther(LCONTR*-BETA)@\
Rule to put an inferred line into beta-normal form.

@IndexOther(LCONTR*-ETA)@\
Rule to put an inferred line into eta-normal form.

@IndexOther(LEMMA)@\
Introduce a Lemma.

@IndexOther(LET)@\
Bind a  variable to a term.

@IndexOther(LEXPD*)@\
Rule to put a planned line into Lambda-normal form using both 
beta and eta conversion.

@IndexOther(LEXPD*-BETA)@\
Rule to put a planned line into beta-normal form.

@IndexOther(LEXPD*-ETA)@\
Rule to put a planned line into eta-normal form.

@IndexOther(MP)@\
Modus Ponens.

@IndexOther(NNF)@\
Put Wff in Negation Normal Form.

@IndexOther(NNF-EXPAND)@\
Expand Wff from Negation Normal Form.

@IndexOther(PULLNEG)@\
Pull out negation.

@IndexOther(PUSHNEG)@\
Push in negation.

@IndexOther(REWRITE-SUPP*)@\
Rewrite a supporting line using all rewrite rules 
possible.

@IndexOther(REWRITE-SUPP1)@\
Rewrite a supporting line using the first rewrite 
rule that applies.

@IndexOther(RULEC)@\
RuleC

@IndexOther(RULEC1)@\
RuleC1 -- the special case of RULEC where the chosen
variable has the same name as the bound variable.

@IndexOther(SAME)@\
Use the fact that two lines are identical to justify a planned line.

@IndexOther(SIMPLIFY-PLAN)@\
Justify a planned line using the first rewrite rule that 
applies.

@IndexOther(SIMPLIFY-PLAN*)@\
Justify a planned line using the first rewrite rule that 
applies.

@IndexOther(SIMPLIFY-SUPP)@\
Rewrite a supporting line using the first rewrite 
rule that applies.

@IndexOther(SIMPLIFY-SUPP*)@\
Rewrite a supporting line using the first rewrite 
rule that applies.

@IndexOther(SUBST-EQUIV)@\
Substitution of Equivalence.  Usable when R and P are the same modulo
the equivalence s EQUIV t.

@IndexOther(SUBST=)@\
Substitution of Equality.  Usable when R and P are the same modulo
the equality s=t.

@IndexOther(SUBST=L)@\
Substitution of Equality.  Replaces some occurrences of the left hand
side by the right hand side.

@IndexOther(SUBST=R)@\
Substitution of Equality.  Replaces some occurrences of the right
hand side by the left hand side.

@IndexOther(SUBSTITUTE)@\
Rule to substitute a term for a variable.

@IndexOther(SYM=)@\
Rule of Symmetry of Equality.

@IndexOther(UGEN)@\
Rule of Universal Generalization.

@IndexOther(UI)@\
Rule of Universal Instantiation.

@IndexOther(UNREWRITE-PLAN*)@\
Justify a planned line using all rewrite rules possible.

@IndexOther(UNREWRITE-PLAN1)@\
Justify a planned line using the first rewrite rule that 
applies.

@IndexOther(USE-RRULES)@\
Rewrite a line. The line may be rewritten several steps,
but rewrites may not be nested.@End(Description)
@ChapterPh(Rewrite Rules)
The internal name of this category is 
REWRITE-RULE.
A rewrite rule can be defined using DEFREWRULE.
Allowable properties are: @t{BEFORE}, @t{AFTER}, @t{REWFN}, @t{RTYPELIST}, @t{APPFN}, @t{BIDIRECTIONAL}, @t{VARIABLES}, @t{DERIVED-IN}, @t{ACTIVE}, @t{MHELP}.
@ChapterPh(Argument For Order-Componentses)
The internal name of this category is 
ORDERCOMPONENTS.
An argument for order-components can be defined using DEFORDERCOM.
Allowable properties are: @t{INIT-JFORM-MSPATH}, @t{TREE-SORTING}, @t{SORT-MS90-3-JFORM}, @t{MHELP}.

@Section(Vpforms)

@Begin(Description)
@IndexOther(COMMON)@\
COMMON is the same as NIL. If the flag ORDER-COMPONENTS is set to COMMON
then the jform of the current eproof will not be modified by the mating search.

@IndexOther(NIL)@\
NIL is the same as COMMON. If the flag ORDER-COMPONENTS is set to NIL
then the jform of the current eproof will not be modified by the mating search.

@IndexOther(PATHNUM)@\
PATHNUM is the same as T. If the flag ORDER-COMPONENTS is set to PATHNUM
then the components of a jform node will be rearranged in order of the number 
of paths which lie below them (go through them). In ms90-*, this will sort the 
top-level conjuncts into decreasing order (based on the number of paths through 
them).

@IndexOther(PATHNUM-REVERSED)@\
PATHNUM-REVERSED is the same as T-REVERSED. If the flag 
ORDER-COMPONENTS is set to T-REVERSED then the components of a jform node will 
be rearranged in reverse order of the number of paths which lie below them 
(go through them).

@IndexOther(PREFER-RIGID1)@\
If the flag ORDER-COMPONENTS is set to PREFER-RIGID1, then 
the order of the components in the jform of the current eproof will be sorted 
in terms of the number of rigid literals in a jform before beginning the 
mating search.

@IndexOther(PREFER-RIGID2)@\
If the flag ORDER-COMPONENTS is set to PREFER-RIGID2, then 
the order of the components in the jform of the current eproof will be sorted 
in terms of the number of rigid literals in a jform before beginning the 
mating search.

@IndexOther(PREFER-RIGID3)@\
If the flag ORDER-COMPONENTS is set to PREFER-RIGID3, then 
the components in the jform of the current eproof will be sorted 
as for PREFER-RIGID2, but with preference given to literals that 
arise from DUAL rewriting.

@IndexOther(REVERSE)@\
If the flag ORDER-COMPONENTS is set to REVERSE, then 
the order of the components in the jform of the current eproof will
be reversed before beginning the mating search.

@IndexOther(T)@\
T is the same as PATHNUM. If the flag ORDER-COMPONENTS is set to T
then the components of a jform node will be rearranged in order of the number 
of paths which lie below them (go through them).

@IndexOther(T-REVERSED)@\
T-REVERSED is the same as PATHNUM-REVERSED. If the flag 
ORDER-COMPONENTS is set to T-REVERSED then the components of a jform node will 
be rearranged in reverse order of the number of paths which lie below them 
(go through them).@End(Description)
@ChapterPh(Monitor Functions)
The internal name of this category is 
MONITORFN.
A monitor function can be defined using DEFMONITOR.
Allowable properties are: @t{ARGTYPES}, @t{ARGNAMES}, @t{ARGHELP}, @t{DEFAULTFNS}, @t{MAINFNS}, @t{PRINT-COMMAND}, @t{DONT-RESTORE}, @t{MHELP}.

@Section(Mating search)

@Begin(Description)
@IndexOther(FOCUS-MATING)@\
Reset some flags when a particular mating is reached. 
The default mating is the mating that is current at the
time when this command is invoked (so the user can often enter the 
mate top level, construct the mating manually and then type 
FOCUS-MATING). Otherwise, the mating should be typed in the form
((LEAFa . LEAFb) (LEAFc . LEAFd) ...) The order in which the connections
are specified within the mating, and the order of the literals within each
connection, do not matter.

@IndexOther(FOCUS-MATING*)@\
Reset some flags when a particular mating is reached. Differs
from FOCUS-MATING in that it returns the flags to their original 
settings afterwards. The default mating is the mating that 
is current at the time when this command is invoked (so the user 
can often enter the mate top level, construct the mating manually and 
then type FOCUS-MATING*). Otherwise, the mating should be typed in the form
((LEAFa . LEAFb) (LEAFc . LEAFd) ...). The order in which the connections
are specified within the mating, and the order of the literals within each
connection, do not matter.
The values used for the "original" flag settings will also
be those that are current at the time when this command is invoked.

@IndexOther(FOCUS-OSET)@\
Reset some flags when a particular option set is reached. 
The option set should be entered in the form "oset-n" where n is a
positive integer. See also FOCUS-OSET*.
This only works for the procedures MS91-6 and MS91-7.
There is a similar monitor function for MS89 and MS90-9, 
called FOCUS-OTREE.

@IndexOther(FOCUS-OSET*)@\
Reset some flags when a particular option set is reached, 
and then set the flags back again when the option set changes again.
The option set should be entered in the form "oset-n" where n is a
positive integer.
The values for the flags to revert to are those which are current at 
the time you typed FOCUS-OSET*. See also FOCUS-OSET.
This only works for the procedures MS91-6 and MS91-7.
There is a similar monitor function for MS89 and MS90-9, 
called FOCUS-OTREE*.

@IndexOther(FOCUS-OTREE)@\
Reset some flags when a particular option tree is reached. 
The option tree should be entered in the form "OPTn" where n is a
positive integer. This only works for the procedures MS89 and MS90-9.
See also FOCUS-OTREE*.
There is a similar monitor function for MS91-6 and MS91-7, 
called FOCUS-OSET.

@IndexOther(FOCUS-OTREE*)@\
Reset some flags when a particular option tree is reached, 
and then set the flags back again when the option tree changes again.
The option tree should be entered in the form "OPTn" where n is a
positive integer.
The values for the flags to revert to are those which are current at 
the time you typed FOCUS-OTREE*. See also FOCUS-OTREE.
This only works for the procedures MS89 and MS90-9.
There is a similar monitor function for MS91-6 and MS91-7, 
called FOCUS-OSET*.

@IndexOther(MONITOR-CHECK)@\
Prints out the given string every time the monitor is called, 
followed by the place from which it was called.

@IndexOther(PUSH-MATING)@\
Executes a PUSH (i.e. halts and starts a new top level) when a 
particular mating is reached. The default mating is the mating that is current 
at the time when this command is invoked (so the user can often enter the 
mate top level, construct the mating manually and then type 
PUSH-MATING). Otherwise, the mating should be typed in the form
((LEAFa . LEAFb) (LEAFc . LEAFd) ...) The order in which the connections
are specified within the mating, and the order of the literals within each
connection, do not matter.
When PUSH-MATING is invoked, typing POP will leave the new top level and
continue with the search.@End(Description)
@ChapterPh(Pair Of List Of Modes And List Of Gwffses)
The internal name of this category is 
MODES-GWFFS.
A pair of list of modes and list of gwffs can be defined using DEF-MODES-GWFFS.
Allowable properties are: @t{MODES-GWFFS-MODES}, @t{MODES-GWFFS-GWFFS}, @t{MHELP}.

@Section(Maintenance)

@Begin(Description)
@IndexOther(EMPTYGOODMODES)@\
A pair of no modes and no gwffs.  Default value of the flag GOODMODES.@End(Description)

@Section(Modules)

@Begin(Description)
@IndexOther(GOODMODES1)@\
A default list of goodmodes generated automatically in 2003
and updated Jan 2005.
This list of modes could prove every theorem that had a bestmode as of Jan 2005.@End(Description)
@ChapterPh(Menu Item For The User Interfaces)
The internal name of this category is 
MENUITEM.
A Menu Item for the User Interface can be defined using DEFMENUITEM.
Allowable properties are: @t{DISPLAY-NAME}, @t{COMMAND}, @t{HOTKEY}, @t{PLACEMENT}, @t{PARENT}, @t{REMOTE-EXPERT}, @t{ETPS}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(0)@\

@IndexOther(A)@\

@IndexOther(AB)@\

@IndexOther(ABBR)@\

@IndexOther(ABNORM)@\

@IndexOther(ADD-ALL-LIT)@\

@IndexOther(ADD-ALL-OB)@\

@IndexOther(ADD-BESTMODE)@\

@IndexOther(ADD-CONN)@\

@IndexOther(ADD-CONN*)@\

@IndexOther(ADD-CONN2)@\

@IndexOther(ADD-EXT-LEMMAS)@\

@IndexOther(ADD-FLAG)@\

@IndexOther(ADD-FLAG*)@\

@IndexOther(ADD-FLAG-TO-MODE)@\

@IndexOther(ADD-FUNCTION)@\

@IndexOther(ADD-KEYWORD)@\

@IndexOther(ADD-SUBDIRECTORIES)@\

@IndexOther(ADD-SUBJECTS)@\

@IndexOther(APPLY-SUBSTS)@\

@IndexOther(ARR)@\

@IndexOther(ARR*)@\

@IndexOther(ARR1)@\

@IndexOther(ARR1*)@\

@IndexOther(ASRB)@\

@IndexOther(ASRB*)@\

@IndexOther(ASSL)@\

@IndexOther(ASSL*)@\

@IndexOther(ASSR)@\

@IndexOther(ASSR*)@\

@IndexOther(BACKUP-LIB-DIR)@\

@IndexOther(BREADTH-FIRST-SEARCH)@\

@IndexOther(BUG-DELETE)@\

@IndexOther(BUG-HELP)@\

@IndexOther(BUG-LIST)@\

@IndexOther(BUG-RESTORE)@\

@IndexOther(BUG-SAVE)@\

@IndexOther(CD)@\

@IndexOther(CHANGE-KEYWORDS)@\

@IndexOther(CHANGE-PROVABILITY)@\

@IndexOther(CHANGED-FLAGS)@\

@IndexOther(CHECK-NEEDED-OBJECTS)@\

@IndexOther(CHOOSE-BRANCH)@\

@IndexOther(CJFORM)@\

@IndexOther(CJFORM2)@\

@IndexOther(CLASS-DIRECTION)@\

@IndexOther(CLASS-SCHEME)@\

@IndexOther(CLASSIFY-CLASS)@\

@IndexOther(CLASSIFY-ITEM)@\

@IndexOther(CLASSIFY-ITEM2)@\

@IndexOther(CLAUSE-FORM)@\

@IndexOther(CLOSE-TESTWIN2)@\

@IndexOther(CMRG)@\

@IndexOther(CMRG*)@\

@IndexOther(CMUT)@\

@IndexOther(CMUT*)@\

@IndexOther(CNF)@\

@IndexOther(CNTOP)@\

@IndexOther(COMPARE-MODES)@\

@IndexOther(COMPLETE-P)@\

@IndexOther(COMPLETE-P2)@\

@IndexOther(CONNS-ADDED)@\

@IndexOther(CONSTANTS)@\

@IndexOther(CONTINUE)@\

@IndexOther(COPY-LIBDIR)@\

@IndexOther(COPY-LIBFILE)@\

@IndexOther(COPY-LIBOBJECT)@\

@IndexOther(COPY-MODE)@\

@IndexOther(CP)@\

@IndexOther(CREATE-CLASS-SCHEME)@\

@IndexOther(CREATE-LIB-DIR)@\

@IndexOther(CREATE-LIB-SUBDIR)@\

@IndexOther(CREATE-LIBCLASS)@\

@IndexOther(CW)@\

@IndexOther(CW2)@\

@IndexOther(CWD)@\

@IndexOther(CWS)@\

@IndexOther(D)@\

@IndexOther(D2)@\

@IndexOther(D23)@\

@IndexOther(DATEREC)@\

@IndexOther(DB)@\

@IndexOther(DEFAULT-BUG-DIR)@\

@IndexOther(DEFAULT-LIB-DIR)@\

@IndexOther(DEFAULT-LIBFILE-TYPE)@\

@IndexOther(DEFAULT-LIBINDEX-TYPE)@\

@IndexOther(DEL-DUP-CONNS)@\

@IndexOther(DELETE-BESTMODE)@\

@IndexOther(DELETE-LIB-DIR)@\

@IndexOther(DELETE-LIBFILE)@\

@IndexOther(DELETE2)@\

@IndexOther(DELETE23)@\

@IndexOther(DELWEAK)@\

@IndexOther(DESCRIBE)@\

@IndexOther(DESCRIBE*)@\

@IndexOther(DESTROY)@\

@IndexOther(DESTROY2)@\

@IndexOther(DISPLAYFILE)@\

@IndexOther(DIST-CTR)@\

@IndexOther(DIST-CTR*)@\

@IndexOther(DIST-EXP)@\

@IndexOther(DIST-EXP*)@\

@IndexOther(DJFORM)@\

@IndexOther(DL)@\

@IndexOther(DNEG)@\

@IndexOther(DNEG*)@\

@IndexOther(DP)@\

@IndexOther(DP*)@\

@IndexOther(DP=)@\

@IndexOther(DPTREE)@\

@IndexOther(DR)@\

@IndexOther(DUP-ALL)@\

@IndexOther(DUP-OUTER)@\

@IndexOther(DUP-VAR)@\

@IndexOther(DUPW)@\

@IndexOther(DW)@\

@IndexOther(DW*)@\

@IndexOther(EDILL)@\

@IndexOther(EDITOR0)@\

@IndexOther(EP)@\

@IndexOther(ETAB)@\

@IndexOther(ETAC)@\

@IndexOther(ETAN)@\

@IndexOther(ETAX)@\

@IndexOther(ETD)@\

@IndexOther(ETP)@\

@IndexOther(ETREE-INFO)@\

@IndexOther(EXHAUSTIVE-SEARCH)@\

@IndexOther(EXP)@\

@IndexOther(EXPAND-ETREE)@\

@IndexOther(EXPAND-LEAVES)@\

@IndexOther(EXPAND=)@\

@IndexOther(EXPAND=*)@\

@IndexOther(EXPUNGE)@\

@IndexOther(EXPUNGE-OLD)@\

@IndexOther(FB)@\

@IndexOther(FETCH)@\

@IndexOther(FETCH-DOWN)@\

@IndexOther(FETCH-LIBCLASS)@\

@IndexOther(FETCH-LIBCLASS*)@\

@IndexOther(FETCH-UP)@\

@IndexOther(FETCH2)@\

@IndexOther(FETCH23)@\

@IndexOther(FI)@\

@IndexOther(FIND-BEST-MODE)@\

@IndexOther(FIND-DUP-MODES)@\

@IndexOther(FIND-MODE)@\

@IndexOther(FIND-PROVABLE)@\

@IndexOther(FIRST-BINDER)@\

@IndexOther(FIRST-INFIX)@\

@IndexOther(FIX-MODES)@\

@IndexOther(GO23)@\

@IndexOther(GO234)@\

@IndexOther(GO2345)@\

@IndexOther(GOTO)@\

@IndexOther(GOTO-CLASS)@\

@IndexOther(GOTO-TOP)@\

@IndexOther(GOTO2)@\

@IndexOther(HEAD)@\

@IndexOther(HVARS)@\

@IndexOther(IB)@\

@IndexOther(ILL)@\

@IndexOther(IMPORT-NEEDED-OBJECTS)@\

@IndexOther(INIT)@\

@IndexOther(INIT-MATING)@\

@IndexOther(INSERT)@\

@IndexOther(INSERT2)@\

@IndexOther(INST)@\

@IndexOther(INST1)@\

@IndexOther(INSTALL)@\

@IndexOther(KEY)@\

@IndexOther(KEY2)@\

@IndexOther(KILL)@\

@IndexOther(L)@\

@IndexOther(LEAVE)@\

@IndexOther(LEAVE2)@\

@IndexOther(LEAVE3)@\

@IndexOther(LEAVE4)@\

@IndexOther(LEAVE5)@\

@IndexOther(LEAVE6)@\

@IndexOther(LEAVE7)@\

@IndexOther(LEFT)@\

@IndexOther(LETA)@\

@IndexOther(LEXP)@\

@IndexOther(LIB-ABBR)@\

@IndexOther(LIB-BESTMODE-FILE)@\

@IndexOther(LIB-KEYWORD-FILE)@\

@IndexOther(LIB-MASTERINDEX-FILE)@\

@IndexOther(LIBFILES)@\

@IndexOther(LIBOBJECTS-IN-FILE)@\

@IndexOther(LIST)@\

@IndexOther(LIST-OF-LIBOBJECTS)@\

@IndexOther(LIVE-LEAVES)@\

@IndexOther(LN)@\

@IndexOther(LNORM)@\

@IndexOther(LNORM-BETA)@\

@IndexOther(LNORM-ETA)@\

@IndexOther(MAKE-RRULE)@\

@IndexOther(MATE0)@\

@IndexOther(MATING-TREE)@\

@IndexOther(MBED-AL)@\

@IndexOther(MBED-AR)@\

@IndexOther(MBED-E)@\

@IndexOther(MBED-E1)@\

@IndexOther(MBED-F)@\

@IndexOther(MBED-IL)@\

@IndexOther(MBED-IR)@\

@IndexOther(MBED-L)@\

@IndexOther(MBED-OL)@\

@IndexOther(MBED-OR)@\

@IndexOther(MBED-QL)@\

@IndexOther(MBED-QR)@\

@IndexOther(MBED=L)@\

@IndexOther(MBED=R)@\

@IndexOther(MERGE-TREE)@\

@IndexOther(MIN-SCOPE)@\

@IndexOther(MINIMAL-P)@\

@IndexOther(MKDIR)@\

@IndexOther(MOD-STATUS)@\

@IndexOther(MODE)@\

@IndexOther(MODEREC)@\

@IndexOther(MODIFY-BESTMODE)@\

@IndexOther(MOVE-LIBFILE)@\

@IndexOther(MOVE-LIBOBJECT)@\

@IndexOther(MRG)@\

@IndexOther(MRG*)@\

@IndexOther(MS88)@\

@IndexOther(MS88-SUB)@\

@IndexOther(MS89)@\

@IndexOther(MS90-3)@\

@IndexOther(MS90-9)@\

@IndexOther(MS91-6)@\

@IndexOther(MS91-7)@\

@IndexOther(MS92-9)@\

@IndexOther(MS93-1)@\

@IndexOther(MS98-1)@\

@IndexOther(MS98-DUP)@\

@IndexOther(MS98-PRIM)@\

@IndexOther(MT94-11)@\

@IndexOther(MT94-12)@\

@IndexOther(MT95-1)@\

@IndexOther(NAME)@\

@IndexOther(NAME-PRIM)@\

@IndexOther(NAME-PRIM2)@\

@IndexOther(NEG)@\

@IndexOther(NEW-DEFS)@\

@IndexOther(NEW-SEARCHLIST)@\

@IndexOther(NNF2)@\

@IndexOther(NOOP)@\

@IndexOther(NOOP2)@\

@IndexOther(NUM-HPATHS)@\

@IndexOther(NUM-HPATHS2)@\

@IndexOther(NUM-VPATHS)@\

@IndexOther(NUM-VPATHS2)@\

@IndexOther(O)@\

@IndexOther(O2)@\

@IndexOther(OK)@\

@IndexOther(OP)@\

@IndexOther(OPEN-TESTWIN)@\

@IndexOther(P)@\

@IndexOther(P2)@\

@IndexOther(PCLASS)@\

@IndexOther(PCLASS-SCHEME-TREE)@\

@IndexOther(PCLASS-TREE)@\

@IndexOther(PDEEP)@\

@IndexOther(PICK)@\

@IndexOther(PINTERSECT)@\

@IndexOther(PINTERSECT*)@\

@IndexOther(PINTERSECT*2)@\

@IndexOther(PINTERSECT2)@\

@IndexOther(PJ)@\

@IndexOther(PM-NODE)@\

@IndexOther(PMTR)@\

@IndexOther(PMTR*)@\

@IndexOther(PMTR-FLAT)@\

@IndexOther(PMUT)@\

@IndexOther(PMUT*)@\

@IndexOther(POB)@\

@IndexOther(POB-LITS)@\

@IndexOther(POB-NODE)@\

@IndexOther(POTR)@\

@IndexOther(POTR*-FLAT)@\

@IndexOther(POTR-FLAT)@\

@IndexOther(PP)@\

@IndexOther(PP2)@\

@IndexOther(PPATH)@\

@IndexOther(PPATH*)@\

@IndexOther(PPDEEP)@\

@IndexOther(PPF)@\

@IndexOther(PRESS-DOWN)@\

@IndexOther(PRESS-DOWN-2)@\

@IndexOther(PRIM-ALL)@\

@IndexOther(PRIM-OUTER)@\

@IndexOther(PRIM-SINGLE)@\

@IndexOther(PRIM-SUB)@\

@IndexOther(PRIM-SUBST)@\

@IndexOther(PROP-CJFORM)@\

@IndexOther(PROP-MSEARCH)@\

@IndexOther(PRT-PRIM)@\

@IndexOther(PRUNE)@\

@IndexOther(PS)@\

@IndexOther(PSCHEMES)@\

@IndexOther(PSCHEMES2)@\

@IndexOther(PSH)@\

@IndexOther(PT)@\

@IndexOther(PTREE)@\

@IndexOther(PTREE*)@\

@IndexOther(PTREE-FILE)@\

@IndexOther(PULL-NEG)@\

@IndexOther(PUSH-NEG)@\

@IndexOther(PUSH-UP)@\

@IndexOther(PUSH-UP-2)@\

@IndexOther(PWD)@\

@IndexOther(QRY)@\

@IndexOther(QUICK-DEFINE)@\

@IndexOther(R)@\

@IndexOther(RECORDFLAGS)@\

@IndexOther(RED)@\

@IndexOther(REFORMAT)@\

@IndexOther(REINDEX)@\

@IndexOther(REM)@\

@IndexOther(REM-CONN)@\

@IndexOther(REM-CONN*)@\

@IndexOther(REM-FLAG)@\

@IndexOther(REM-FLAG*)@\

@IndexOther(REM-LAST-CONN)@\

@IndexOther(REM-NODE)@\

@IndexOther(REM2)@\

@IndexOther(REMOVE-FLAG-FROM-MODE)@\

@IndexOther(REMOVE-TRAILING-DIR)@\

@IndexOther(RENAME-LIBDIR)@\

@IndexOther(RENAME-LIBFILE)@\

@IndexOther(RENAME-OBJECT)@\

@IndexOther(RESTORE-ETREE)@\

@IndexOther(RESTORE-MASTERINDEX)@\

@IndexOther(RESURRECT)@\

@IndexOther(RETRIEVE-FILE)@\

@IndexOther(REVIEW0)@\

@IndexOther(REVISE-DEFAULTS)@\

@IndexOther(REW-EQUIV)@\

@IndexOther(RIGHT)@\

@IndexOther(RM)@\

@IndexOther(ROOT-CLASS)@\

@IndexOther(RP)@\

@IndexOther(RPALL)@\

@IndexOther(RW)@\

@IndexOther(SAVE)@\

@IndexOther(SAVE-ETREE)@\

@IndexOther(SCALE-DOWN)@\

@IndexOther(SCALE-UP)@\

@IndexOther(SCRIBE-ALL-WFFS)@\

@IndexOther(SCRIBELIBDIR)@\

@IndexOther(SCRIBELIBFILE)@\

@IndexOther(SEARCH-PLACEMENT2)@\

@IndexOther(SEARCH2)@\

@IndexOther(SEARCH22)@\

@IndexOther(SEARCHLISTS2)@\

@IndexOther(SEL)@\

@IndexOther(SET)@\

@IndexOther(SET-SEARCH-TREE)@\

@IndexOther(SETFLAG)@\

@IndexOther(SETFLAGS1)@\

@IndexOther(SETFLAGS2)@\

@IndexOther(SHOW)@\

@IndexOther(SHOW*-WFF)@\

@IndexOther(SHOW-ALL-LIBOBJECTS)@\

@IndexOther(SHOW-ALL-WFFS)@\

@IndexOther(SHOW-ALL-WFFS2)@\

@IndexOther(SHOW-BESTMODE)@\

@IndexOther(SHOW-HELP)@\

@IndexOther(SHOW-HELP2)@\

@IndexOther(SHOW-KEYWORDS)@\

@IndexOther(SHOW-MATING)@\

@IndexOther(SHOW-MATING2)@\

@IndexOther(SHOW-NEW-BESTMODES)@\

@IndexOther(SHOW-OBJECTS-IN-FILE)@\

@IndexOther(SHOW-OPTION-TREE)@\

@IndexOther(SHOW-SEARCHLIST)@\

@IndexOther(SHOW-SUBSTS)@\

@IndexOther(SHOW-SUBSTS2)@\

@IndexOther(SHOW-TIMING)@\

@IndexOther(SHOW-WFF)@\

@IndexOther(SHOW-WFF&HELP)@\

@IndexOther(SHOW-WFF&HELP2)@\

@IndexOther(SHOW-WFF2)@\

@IndexOther(SHOW-WFFS-IN-FILE)@\

@IndexOther(SHOW2)@\

@IndexOther(SIB)@\

@IndexOther(SK1)@\

@IndexOther(SK3)@\

@IndexOther(SORT)@\

@IndexOther(SPRING-CLEAN)@\

@IndexOther(STATS)@\

@IndexOther(SUB)@\

@IndexOther(SUB-ETREE)@\

@IndexOther(SUB2)@\

@IndexOther(SUBEQ)@\

@IndexOther(SUBEQ*)@\

@IndexOther(SUBFORMULAS)@\

@IndexOther(SUBIM)@\

@IndexOther(SUBIM*)@\

@IndexOther(SUBJECTS)@\

@IndexOther(SUBST)@\

@IndexOther(SUBSTYP)@\

@IndexOther(TERMS)@\

@IndexOther(TEST0)@\

@IndexOther(TEX-ALL-WFFS)@\

@IndexOther(TEXLIBDIR)@\

@IndexOther(TEXLIBFILE)@\

@IndexOther(TP)@\

@IndexOther(ULNORM)@\

@IndexOther(UNARR)@\

@IndexOther(UNARR*)@\

@IndexOther(UNARR1)@\

@IndexOther(UNARR1*)@\

@IndexOther(UNCLASSIFY-CLASS)@\

@IndexOther(UNCLASSIFY-ITEM)@\

@IndexOther(UNDO)@\

@IndexOther(UNIF-DEPTHS)@\

@IndexOther(UNIF-NODEPTHS)@\

@IndexOther(UNIFORM-SEARCH)@\

@IndexOther(UNIFORM-SEARCH-L)@\

@IndexOther(UNIFY)@\

@IndexOther(UNIFY2)@\

@IndexOther(UNIX-STYLE)@\

@IndexOther(UNIXLIB-SHOWPATH)@\

@IndexOther(UP)@\

@IndexOther(UP-ONE-LEVEL)@\

@IndexOther(UP2)@\

@IndexOther(UPDATE)@\

@IndexOther(UPDATE-KEYWORDS)@\

@IndexOther(UPDATE-LIBDIR)@\

@IndexOther(UPDATE-PROVABILITY)@\

@IndexOther(UPDATE-RELEVANT)@\

@IndexOther(USE-DEFAULT-BUG-DIR)@\

@IndexOther(VARY-MODE)@\

@IndexOther(VP)@\

@IndexOther(VP2)@\

@IndexOther(VPD)@\

@IndexOther(VPD2)@\

@IndexOther(VPETREE)@\

@IndexOther(VPF)@\

@IndexOther(VPT)@\

@IndexOther(VPT2)@\

@IndexOther(WFFP)@\

@IndexOther(XTR)@\

@IndexOther(^)@\
@End(Description)

@Section(Flags)

@Begin(Description)
@IndexOther(SAVE-FLAG-RELEVANCY-INFO)@\

@IndexOther(SHOW-RELEVANCE-PATHS)@\
@End(Description)

@Section(Unification)

@Begin(Description)
@IndexOther(0-2)@\

@IndexOther(ADD-DPAIR)@\

@IndexOther(ADD-DPAIRS-TO-NODE)@\

@IndexOther(ADD-DPAIRS-TO-UTREE)@\

@IndexOther(APPLY-MATCH)@\

@IndexOther(APPLY-SUBST)@\

@IndexOther(COUNTSUBS-FIRST)@\

@IndexOther(DNEG-IMITATION)@\

@IndexOther(EPROOF-UTREE)@\

@IndexOther(ETA-RULE)@\

@IndexOther(FIND-NESTING)@\

@IndexOther(GO23456)@\

@IndexOther(GOTO23)@\

@IndexOther(IMITATION-FIRST)@\

@IndexOther(LEAVE8)@\

@IndexOther(LEIBNIZ-SUB-CHECK)@\

@IndexOther(MATCH)@\

@IndexOther(MATCH-PAIR)@\

@IndexOther(MAX-DUP-PATHS)@\

@IndexOther(MAX-SEARCH-DEPTH)@\

@IndexOther(MAX-SUBSTS-PROJ)@\

@IndexOther(MAX-SUBSTS-PROJ-TOTAL)@\

@IndexOther(MAX-SUBSTS-QUICK)@\

@IndexOther(MAX-SUBSTS-VAR)@\

@IndexOther(MAX-UTREE-DEPTH)@\

@IndexOther(MIN-QUICK-DEPTH)@\

@IndexOther(MS-DIR)@\

@IndexOther(MS90-3-QUICK)@\

@IndexOther(NAME-DPAIR)@\

@IndexOther(NTH-SON)@\

@IndexOther(NUM-OF-DUPS)@\

@IndexOther(P23)@\

@IndexOther(PALL2)@\

@IndexOther(PP*)@\

@IndexOther(PP23)@\

@IndexOther(PRUNE2)@\

@IndexOther(PRUNING)@\

@IndexOther(REDUCE-DOUBLE-NEG)@\

@IndexOther(RIGID-PATH-CK)@\

@IndexOther(RM-DPAIR)@\

@IndexOther(SHOW-DPAIRSET)@\

@IndexOther(SIMPLIFY)@\

@IndexOther(STATS2)@\

@IndexOther(STOP-AT-TSN)@\

@IndexOther(SUBST-STACK)@\

@IndexOther(SUBSUMPTION-CHECK)@\

@IndexOther(SUBSUMPTION-DEPTH)@\

@IndexOther(SUBSUMPTION-NODES)@\

@IndexOther(TOTAL-NUM-OF-DUPS)@\

@IndexOther(UNI-SEARCH-HEURISTIC)@\

@IndexOther(UNIF-COUNTER)@\

@IndexOther(UNIF-COUNTER-OUTPUT)@\

@IndexOther(UNIF-PROBLEM)@\

@IndexOther(UNIF-TRIGGER)@\

@IndexOther(UNIFICATION0)@\

@IndexOther(UNIFY-VERBOSE)@\

@IndexOther(UTREE)@\

@IndexOther(UTREE*)@\

@IndexOther(^2)@\

@IndexOther(^^)@\
@End(Description)

@Section(Vpforms)

@Begin(Description)
@IndexOther(CLOSE-MATEVPW)@\

@IndexOther(OPEN-MATEVPW)@\
@End(Description)

@Section(Maintenance)

@Begin(Description)
@IndexOther(?)@\

@IndexOther(??)@\

@IndexOther(AB*)@\

@IndexOther(ABBREVIATIONS)@\

@IndexOther(ABE)@\

@IndexOther(ABSURD)@\

@IndexOther(ABU)@\

@IndexOther(ACTIVATE-RULES)@\

@IndexOther(ADD-HYPS)@\

@IndexOther(ADD-TRUTH)@\

@IndexOther(ADDED-CONN-ENABLED0)@\

@IndexOther(ADVICE)@\

@IndexOther(ADVICE-ASKED-ENABLED0)@\

@IndexOther(ADVICE-FILE)@\

@IndexOther(ALIAS)@\

@IndexOther(ALLOW-NONLEAF-CONNS)@\

@IndexOther(ALLSCOPEFLAG)@\

@IndexOther(ALPHA-LOWER-FLAG)@\

@IndexOther(APPEND-WFF)@\

@IndexOther(APPEND-WFFS)@\

@IndexOther(ARE-WE-USING)@\

@IndexOther(ASSEMBLE-FILE)@\

@IndexOther(ASSEMBLE-MOD)@\

@IndexOther(ASSERT)@\

@IndexOther(ASSERT-LEMMAS)@\

@IndexOther(ASSOC-LEFT)@\

@IndexOther(ATOMVALFLAG)@\

@IndexOther(AUTO-GENERATE-HYPS)@\

@IndexOther(AUTO-SUGGEST)@\

@IndexOther(BAD-VAR-CONNECTED-PRUNE)@\

@IndexOther(BASE-TYPE)@\

@IndexOther(BEGIN-PRFW)@\

@IndexOther(BETA*)@\

@IndexOther(BLANK-LINES-INSERTED)@\

@IndexOther(BREAK-AT-QUANTIFIERS)@\

@IndexOther(BUILD)@\

@IndexOther(BUILD-MATCH)@\

@IndexOther(BUILD-PROOF-HIERARCHY)@\

@IndexOther(CASES)@\

@IndexOther(CASES3)@\

@IndexOther(CASES4)@\

@IndexOther(CHARDOC)@\

@IndexOther(CHARSIZE)@\

@IndexOther(CHECK-STRUCTURE)@\

@IndexOther(CLEANUP)@\

@IndexOther(CLEANUP-RULEC)@\

@IndexOther(CLEANUP-SAME)@\

@IndexOther(CLOAD)@\

@IndexOther(CLOAD-MODULES)@\

@IndexOther(CLOSE-TESTWIN)@\

@IndexOther(COLLECT-HELP)@\

@IndexOther(COMMAND-ENABLED0)@\

@IndexOther(COMMAND-FILE)@\

@IndexOther(COMPILE-LIST)@\

@IndexOther(COMPILED-EXTENSION)@\

@IndexOther(COMPL)@\

@IndexOther(COMPLETION-OPTIONS)@\

@IndexOther(CONSIDERED-CONN-ENABLED0)@\

@IndexOther(COUNT-LINES)@\

@IndexOther(CREATE-SUBPROOF)@\

@IndexOther(DE-ASSERT-LEMMAS)@\

@IndexOther(DEACTIVATE-RULES)@\

@IndexOther(DEDUCT)@\

@IndexOther(DEFAULT-EXPAND)@\

@IndexOther(DEFAULT-MATE)@\

@IndexOther(DEFAULT-MS)@\

@IndexOther(DEFAULT-OB)@\

@IndexOther(DEFAULT-TACTIC)@\

@IndexOther(DEFAULT-WFFEQ)@\

@IndexOther(DELAY-SETVARS)@\

@IndexOther(DELETE)@\

@IndexOther(DELETE*)@\

@IndexOther(DELETE-HYPS)@\

@IndexOther(DELETE-RRULE)@\

@IndexOther(DEPTH)@\

@IndexOther(DISABLE-EVENTS)@\

@IndexOther(DISJ-IMP)@\

@IndexOther(DISJ-IMP-L)@\

@IndexOther(DISJ-IMP-R)@\

@IndexOther(DISPLAY-TIME)@\

@IndexOther(DISPLAYWFF)@\

@IndexOther(DISSOLVE)@\

@IndexOther(DIY)@\

@IndexOther(DIY-L)@\

@IndexOther(DONE)@\

@IndexOther(DONE-EXC-ENABLED0)@\

@IndexOther(DUP-ALLOWED)@\

@IndexOther(DUPE-ENABLED0)@\

@IndexOther(DUPE-VAR-ENABLED0)@\

@IndexOther(DUPLICATION-STRATEGY)@\

@IndexOther(DUPLICATION-STRATEGY-PFD)@\

@IndexOther(EASY-SV-MODE)@\

@IndexOther(ECHO)@\

@IndexOther(ECONJ)@\

@IndexOther(ECONJ-NAME)@\

@IndexOther(EDEF)@\

@IndexOther(EDISJ-NAME)@\

@IndexOther(EDPPWFFLAG)@\

@IndexOther(EDPRINTDEPTH)@\

@IndexOther(EDWIN-CURRENT)@\

@IndexOther(EDWIN-CURRENT-HEIGHT)@\

@IndexOther(EDWIN-CURRENT-WIDTH)@\

@IndexOther(EDWIN-TOP)@\

@IndexOther(EDWIN-TOP-HEIGHT)@\

@IndexOther(EDWIN-TOP-WIDTH)@\

@IndexOther(EDWIN-VPFORM)@\

@IndexOther(EDWIN-VPFORM-HEIGHT)@\

@IndexOther(EDWIN-VPFORM-WIDTH)@\

@IndexOther(EGEN)@\

@IndexOther(ELIM-DEFNS)@\

@IndexOther(ELIMINATE-ALL-RULEP-APPS)@\

@IndexOther(ELIMINATE-CONJ*-RULEP-APPS)@\

@IndexOther(ELIMINATE-RULEP-LINE)@\

@IndexOther(EMPTY-DUP-INFO-NAME)@\

@IndexOther(END-PRFW)@\

@IndexOther(ENEG)@\

@IndexOther(ENVIRONMENT)@\

@IndexOther(EPROOF-NAME)@\

@IndexOther(EPROOFLIST)@\

@IndexOther(EQUIV-EQ)@\

@IndexOther(EQUIV-EQ-CONTR)@\

@IndexOther(EQUIV-EQ-CONTR*)@\

@IndexOther(EQUIV-EQ-EXPD)@\

@IndexOther(EQUIV-EQ-EXPD*)@\

@IndexOther(EQUIV-IMPLICS)@\

@IndexOther(EQUIV-WFFS)@\

@IndexOther(ERROR-ENABLED0)@\

@IndexOther(ERROR-FILE)@\

@IndexOther(ETA*)@\

@IndexOther(ETR-AUTO-SUGGEST)@\

@IndexOther(ETREE-NAT)@\

@IndexOther(ETREE-NAT-VERBOSE)@\

@IndexOther(EVENT-CYCLE)@\

@IndexOther(EVENTS-ENABLED0)@\

@IndexOther(EXCLUDING-GC-TIME)@\

@IndexOther(EXECUTE-FILE)@\

@IndexOther(EXERCISE)@\

@IndexOther(EXIT)@\

@IndexOther(EXPANSION-NAME)@\

@IndexOther(EXPERTFLAG)@\

@IndexOther(EXPLAIN)@\

@IndexOther(EXT=)@\

@IndexOther(EXT=0)@\

@IndexOther(FALSE-NAME)@\

@IndexOther(FF-DELAY)@\

@IndexOther(FILETYPE)@\

@IndexOther(FILLINEFLAG)@\

@IndexOther(FIND-LINE)@\

@IndexOther(FINDPROOF)@\

@IndexOther(FINISH-SAVE)@\

@IndexOther(FIRST-PLACEMENT-MODE-MS)@\

@IndexOther(FIRST-PLACEMENT-MODE-PARSE)@\

@IndexOther(FIRST-PLACEMENT-PRINT-MODE)@\

@IndexOther(FLUSHLEFTFLAG)@\

@IndexOther(GENERATE-JAVA-MENUS)@\

@IndexOther(GO)@\

@IndexOther(GO-INSTRUCTIONS)@\

@IndexOther(GO2)@\

@IndexOther(HELP)@\

@IndexOther(HELP*)@\

@IndexOther(HELP-GROUP)@\

@IndexOther(HELP-LIST)@\

@IndexOther(HELP2)@\

@IndexOther(HISTORY)@\

@IndexOther(HISTORY-SIZE)@\

@IndexOther(HLINE-JUSTIFICATION)@\

@IndexOther(HPATH-THRESHOLD)@\

@IndexOther(HTML-DOC)@\

@IndexOther(HYP)@\

@IndexOther(ICONJ)@\

@IndexOther(IDEF)@\

@IndexOther(IDISJ-LEFT)@\

@IndexOther(IDISJ-RIGHT)@\

@IndexOther(IMP-DISJ)@\

@IndexOther(IMP-DISJ-L)@\

@IndexOther(IMP-DISJ-R)@\

@IndexOther(IMP-NAME)@\

@IndexOther(IMPLICS-EQUIV)@\

@IndexOther(IN-TEX-MATH-MODE)@\

@IndexOther(INCLUDE-COINDUCTION-PRINCIPLE)@\

@IndexOther(INCLUDE-INDUCTION-PRINCIPLE)@\

@IndexOther(INCOMP-MATING-ENABLED0)@\

@IndexOther(INDIRECT)@\

@IndexOther(INDIRECT1)@\

@IndexOther(INDIRECT2)@\

@IndexOther(INEG)@\

@IndexOther(INFIX-NOTATION)@\

@IndexOther(INIT-DIALOGUE)@\

@IndexOther(INIT-DIALOGUE-FN)@\

@IndexOther(INITIAL-BKTRACK-LIMIT)@\

@IndexOther(INPUT-ERROR-ENABLED0)@\

@IndexOther(INPUT-ERROR-FILE)@\

@IndexOther(INTERRUPT)@\

@IndexOther(INTERRUPT-ENABLE)@\

@IndexOther(INTRODUCE-GAP)@\

@IndexOther(ITRUTH)@\

@IndexOther(LAMBDA*)@\

@IndexOther(LAMBDA-CONV)@\

@IndexOther(LAST-MODE-NAME)@\

@IndexOther(LATEX-EMULATION)@\

@IndexOther(LATEX-POSTAMBLE)@\

@IndexOther(LATEX-PREAMBLE)@\

@IndexOther(LCONTR*)@\

@IndexOther(LCONTR*-BETA)@\

@IndexOther(LCONTR*-ETA)@\

@IndexOther(LEAF-NAME)@\

@IndexOther(LEAST-SEARCH-DEPTH)@\

@IndexOther(LEDIT)@\

@IndexOther(LEFTMARGIN)@\

@IndexOther(LEMMA)@\

@IndexOther(LET)@\

@IndexOther(LEXPD*)@\

@IndexOther(LEXPD*-BETA)@\

@IndexOther(LEXPD*-ETA)@\

@IndexOther(LIBRARY-MODE)@\

@IndexOther(LIBRARY0)@\

@IndexOther(LINE-COMMENT)@\

@IndexOther(LISP-IMPLEMENTATION-TYPE)@\

@IndexOther(LIST-RRULES)@\

@IndexOther(LIST-RULES)@\

@IndexOther(LIST-RULES*)@\

@IndexOther(LIT-NAME)@\

@IndexOther(LOAD-SLOW)@\

@IndexOther(LOAD-WARN-P)@\

@IndexOther(LOADED-MODS)@\

@IndexOther(LOCALLEFTFLAG)@\

@IndexOther(LOCK-LINE)@\

@IndexOther(LOWERCASERAISE)@\

@IndexOther(MACHINE-INSTANCE)@\

@IndexOther(MACHINE-TYPE)@\

@IndexOther(MAIN-DIY)@\

@IndexOther(MAKE-ABBREV-RRULE)@\

@IndexOther(MAKE-ASSERT-A-HYP)@\

@IndexOther(MAKE-INVERSE-RRULE)@\

@IndexOther(MAKE-THEORY)@\

@IndexOther(MAKE-WFFOPS-LABELS)@\

@IndexOther(MATE-FFPAIR)@\

@IndexOther(MATE-SUBSUMED-TEST-ENABLED0)@\

@IndexOther(MATE-SUBSUMED-TRUE-ENABLED0)@\

@IndexOther(MATE-UP-TO-NNF)@\

@IndexOther(MATING-CHANGED-ENABLED0)@\

@IndexOther(MATING-NAME)@\

@IndexOther(MATING-VERBOSE)@\

@IndexOther(MATINGSTREE-NAME)@\

@IndexOther(MAX-CONSTRAINT-SIZE)@\

@IndexOther(MAX-MATES)@\

@IndexOther(MAX-NUM-CONSTRAINTS)@\

@IndexOther(MAX-PRIM-DEPTH)@\

@IndexOther(MAX-PRIM-LITS)@\

@IndexOther(MAX-SEARCH-LIMIT)@\

@IndexOther(MAXIMIZE-FIRST)@\

@IndexOther(MEASUREMENTS)@\

@IndexOther(MERGE-MINIMIZE-MATING)@\

@IndexOther(MERGE-PROOFS)@\

@IndexOther(META-BDVAR-NAME)@\

@IndexOther(META-LABEL-NAME)@\

@IndexOther(META-VAR-NAME)@\

@IndexOther(MIN-PRIM-DEPTH)@\

@IndexOther(MIN-PRIM-LITS)@\

@IndexOther(MIN-QUANT-ETREE)@\

@IndexOther(MIN-QUANTIFIER-SCOPE)@\

@IndexOther(MODIFY-GAPS)@\

@IndexOther(MODULES)@\

@IndexOther(MONITOR)@\

@IndexOther(MONITORFLAG)@\

@IndexOther(MONITORLIST)@\

@IndexOther(MONSTRO)@\

@IndexOther(MOVE)@\

@IndexOther(MOVE*)@\

@IndexOther(MP)@\

@IndexOther(MS-INIT-PATH)@\

@IndexOther(MS-SPLIT)@\

@IndexOther(MS90-3-DUP-STRATEGY)@\

@IndexOther(MS91-INTERLEAVE)@\

@IndexOther(MS91-PREFER-SMALLER)@\

@IndexOther(MS91-TIME-BY-VPATHS)@\

@IndexOther(MS91-WEIGHT-LIMIT-RANGE)@\

@IndexOther(MS98-BASE-PRIM)@\

@IndexOther(MS98-DUP-BELOW-PRIMSUBS)@\

@IndexOther(MS98-DUP-PRIMSUBS)@\

@IndexOther(MS98-FIRST-FRAGMENT)@\

@IndexOther(MS98-FO-MODE)@\

@IndexOther(MS98-FORCE-H-O)@\

@IndexOther(MS98-FRAGMENT-PLACEMENT)@\

@IndexOther(MS98-HO-MODE)@\

@IndexOther(MS98-INIT)@\

@IndexOther(MS98-LOW-MEMORY)@\

@IndexOther(MS98-MAX-COMPONENTS)@\

@IndexOther(MS98-MAX-PRIMS)@\

@IndexOther(MS98-MEASURE)@\

@IndexOther(MS98-MERGE-DAGS)@\

@IndexOther(MS98-MINIMALITY-CHECK)@\

@IndexOther(MS98-NUM-OF-DUPS)@\

@IndexOther(MS98-PRIMSUB-COUNT)@\

@IndexOther(MS98-REW-PRIMSUBS)@\

@IndexOther(MS98-REWRITE-DEPTH)@\

@IndexOther(MS98-REWRITE-MODEL)@\

@IndexOther(MS98-REWRITE-PRUNE)@\

@IndexOther(MS98-REWRITE-SIZE)@\

@IndexOther(MS98-REWRITE-UNIF)@\

@IndexOther(MS98-REWRITES)@\

@IndexOther(MS98-TRACE)@\

@IndexOther(MS98-UNIF-HACK)@\

@IndexOther(MS98-UNIF-HACK2)@\

@IndexOther(MS98-USE-COLORS)@\

@IndexOther(MS98-VALID-PAIR)@\

@IndexOther(MS98-VARIABLE-PLACEMENT)@\

@IndexOther(MS98-VERBOSE)@\

@IndexOther(MT-DEFAULT-OB-MATE)@\

@IndexOther(MT-DUPS-PER-QUANT)@\

@IndexOther(MT-SUBSUMPTION-CHECK)@\

@IndexOther(MT94-12-TRIGGER)@\

@IndexOther(MTREE-FILTER-DUPS)@\

@IndexOther(MTREE-STOP-IMMEDIATELY)@\

@IndexOther(NAME-SKOLEM-FN)@\

@IndexOther(NAT-ETREE)@\

@IndexOther(NAT-ETREE-VERSION)@\

@IndexOther(NATREE-DEBUG)@\

@IndexOther(NEG-NAME)@\

@IndexOther(NEG-PRIM-SUB)@\

@IndexOther(NEW-MATING-AFTER-DUP)@\

@IndexOther(NEW-OPTION-SET-LIMIT)@\

@IndexOther(NEWS)@\

@IndexOther(NEWS-DIR)@\

@IndexOther(NNF)@\

@IndexOther(NNF-EXPAND)@\

@IndexOther(NOMONITOR)@\

@IndexOther(NORMALIZE-PROOF)@\

@IndexOther(NUM-FRPAIRS)@\

@IndexOther(OCCURS-CHECK)@\

@IndexOther(OOPS)@\

@IndexOther(OPTIONS-GENERATE-ARG)@\

@IndexOther(OPTIONS-GENERATE-FN)@\

@IndexOther(OPTIONS-GENERATE-UPDATE)@\

@IndexOther(OPTIONS-VERBOSE)@\

@IndexOther(ORGANIZE)@\

@IndexOther(PACK-STAT)@\

@IndexOther(PAGELENGTH)@\

@IndexOther(PALL)@\

@IndexOther(PALL1)@\

@IndexOther(PAUSE)@\

@IndexOther(PBRIEF)@\

@IndexOther(PENALTY-FOR-EACH-PRIMSUB)@\

@IndexOther(PENALTY-FOR-MULTIPLE-PRIMSUBS)@\

@IndexOther(PENALTY-FOR-MULTIPLE-SUBS)@\

@IndexOther(PENALTY-FOR-ORDINARY-DUP)@\

@IndexOther(PERMUTE-RRULES)@\

@IndexOther(PFNAT)@\

@IndexOther(PL)@\

@IndexOther(PL*)@\

@IndexOther(PLACEMENT-COMPONENTS)@\

@IndexOther(PLAN)@\

@IndexOther(PLINE)@\

@IndexOther(PNTR)@\

@IndexOther(POP-FROM-TOP)@\

@IndexOther(PPLAN)@\

@IndexOther(PPWFFLAG)@\

@IndexOther(PR00-ALLOW-SUBNODE-CONNS)@\

@IndexOther(PR00-MAX-SUBSTS-VAR)@\

@IndexOther(PR00-NUM-ITERATIONS)@\

@IndexOther(PR00-REQUIRE-ARG-DEPS)@\

@IndexOther(PR97C-MAX-ABBREVS)@\

@IndexOther(PR97C-PRENEX)@\

@IndexOther(PRIM-BDTYPES)@\

@IndexOther(PRIM-BDTYPES-AUTO)@\

@IndexOther(PRIM-PREFIX)@\

@IndexOther(PRIM-QUANTIFIER)@\

@IndexOther(PRIMSUB-ENABLED0)@\

@IndexOther(PRIMSUB-METHOD)@\

@IndexOther(PRIMSUB-VAR-SELECT)@\

@IndexOther(PRINT-COMBINED-EGENS)@\

@IndexOther(PRINT-COMBINED-UGENS)@\

@IndexOther(PRINT-COMBINED-UIS)@\

@IndexOther(PRINT-COMMENTS)@\

@IndexOther(PRINT-DEEP)@\

@IndexOther(PRINT-DOTS)@\

@IndexOther(PRINT-LIT-NAME)@\

@IndexOther(PRINT-MATING-COUNTER)@\

@IndexOther(PRINT-META)@\

@IndexOther(PRINT-NODENAMES)@\

@IndexOther(PRINT-PROOF-STRUCTURE)@\

@IndexOther(PRINT-UNTIL-UI-OR-EGEN)@\

@IndexOther(PRINT-WEAK)@\

@IndexOther(PRINTDEPTH)@\

@IndexOther(PRINTEDTFILE)@\

@IndexOther(PRINTEDTFLAG)@\

@IndexOther(PRINTEDTFLAG-SLIDES)@\

@IndexOther(PRINTEDTOPS)@\

@IndexOther(PRINTLINEFLAG)@\

@IndexOther(PRINTMATEFILE)@\

@IndexOther(PRINTMATEFLAG)@\

@IndexOther(PRINTMATEFLAG-SLIDES)@\

@IndexOther(PRINTMATEOPS)@\

@IndexOther(PRINTPROOF)@\

@IndexOther(PRINTTYPES)@\

@IndexOther(PRINTTYPES-ALL)@\

@IndexOther(PRINTVPDFLAG)@\

@IndexOther(PROBLEMS)@\

@IndexOther(PROOF-ACTION-ENABLED0)@\

@IndexOther(PROOF-COMMENT)@\

@IndexOther(PROOF-FILE)@\

@IndexOther(PROOFLIST)@\

@IndexOther(PROOFW-ACTIVE)@\

@IndexOther(PROOFW-ACTIVE+NOS)@\

@IndexOther(PROOFW-ACTIVE+NOS-HEIGHT)@\

@IndexOther(PROOFW-ACTIVE+NOS-WIDTH)@\

@IndexOther(PROOFW-ACTIVE-HEIGHT)@\

@IndexOther(PROOFW-ACTIVE-WIDTH)@\

@IndexOther(PROOFW-ALL)@\

@IndexOther(PROOFW-ALL-HEIGHT)@\

@IndexOther(PROOFW-ALL-WIDTH)@\

@IndexOther(PROP-STRATEGY)@\

@IndexOther(PROVE)@\

@IndexOther(PRW)@\

@IndexOther(PSEQ)@\

@IndexOther(PSEQ-USE-LABELS)@\

@IndexOther(PSEQL)@\

@IndexOther(PSTATUS)@\

@IndexOther(PULLNEG)@\

@IndexOther(PUSH-TO-TOP)@\

@IndexOther(PUSHNEG)@\

@IndexOther(PW)@\

@IndexOther(PWSCOPE)@\

@IndexOther(PWTYPES)@\

@IndexOther(QLOAD)@\

@IndexOther(QUERY-USER)@\

@IndexOther(QUICK-REF)@\

@IndexOther(QUIET-EVENTS)@\

@IndexOther(QUIETLY-USE-DEFAULTS)@\

@IndexOther(RANK-EPROOF-FN)@\

@IndexOther(READ-LLOAD-SOURCES-P)@\

@IndexOther(REC-MS-FILE)@\

@IndexOther(REC-MS-FILENAME)@\

@IndexOther(RECONSIDER)@\

@IndexOther(RECONSIDER-FN)@\

@IndexOther(RECONSIDER-PROOF)@\

@IndexOther(REMARK)@\

@IndexOther(REMOVE-LEIBNIZ)@\

@IndexOther(REMOVED-CONN-ENABLED0)@\

@IndexOther(REN-VAR-FN)@\

@IndexOther(RENAME-ALL-BD-VARS)@\

@IndexOther(RENUMBER-LEAVES)@\

@IndexOther(RENUMBERALL)@\

@IndexOther(RESOLVE-CONFLICT)@\

@IndexOther(RESTORE-PROOF)@\

@IndexOther(RESTORE-WORK)@\

@IndexOther(RESUME-SAVE)@\

@IndexOther(RETAIN-INITIAL-TYPE)@\

@IndexOther(REWRITE-DEFNS)@\

@IndexOther(REWRITE-EQUALITIES)@\

@IndexOther(REWRITE-EQUIVS)@\

@IndexOther(REWRITE-NAME)@\

@IndexOther(REWRITE-SUPP*)@\

@IndexOther(REWRITE-SUPP1)@\

@IndexOther(RIGHTMARGIN)@\

@IndexOther(RULE-ERROR-ENABLED0)@\

@IndexOther(RULE-ERROR-FILE)@\

@IndexOther(RULE-P)@\

@IndexOther(RULEC)@\

@IndexOther(RULEC1)@\

@IndexOther(RULEP-MAINFN)@\

@IndexOther(RULEP-WFFEQ)@\

@IndexOther(SAME)@\

@IndexOther(SAVE-FILE)@\

@IndexOther(SAVE-FLAGS-AND-WORK)@\

@IndexOther(SAVE-INTERVAL)@\

@IndexOther(SAVE-SUBPROOF)@\

@IndexOther(SAVE-WORK)@\

@IndexOther(SAVE-WORK-ON-START-UP)@\

@IndexOther(SAVE-WORK-P)@\

@IndexOther(SAVEPROOF)@\

@IndexOther(SCOPE)@\

@IndexOther(SCORE-FILE)@\

@IndexOther(SCRIBE-DOC)@\

@IndexOther(SCRIBE-LINE-WIDTH)@\

@IndexOther(SCRIBE-POSTAMBLE)@\

@IndexOther(SCRIBE-PREAMBLE)@\

@IndexOther(SCRIBEPROOF)@\

@IndexOther(SCRIPT)@\

@IndexOther(SEARCH)@\

@IndexOther(SEARCH-COMPLETE-PATHS)@\

@IndexOther(SEARCH-PLACEMENT)@\

@IndexOther(SEARCH-TIME-LIMIT)@\

@IndexOther(SELECTION-NAME)@\

@IndexOther(SEQ-TO-NAT)@\

@IndexOther(SEQLIST)@\

@IndexOther(SET-BACKGROUND-EPROOF)@\

@IndexOther(SET-EPROOF)@\

@IndexOther(SETUP-SLIDE-STYLE)@\

@IndexOther(SHORT-HELP)@\

@IndexOther(SHORT-SITE-NAME)@\

@IndexOther(SHOW-ALL-PACKAGES)@\

@IndexOther(SHOW-SKOLEM)@\

@IndexOther(SHOW-TIME)@\

@IndexOther(SHOWNOTYPES)@\

@IndexOther(SHOWTYPES)@\

@IndexOther(SIMPLIFY-PLAN)@\

@IndexOther(SIMPLIFY-PLAN*)@\

@IndexOther(SIMPLIFY-SUPP)@\

@IndexOther(SIMPLIFY-SUPP*)@\

@IndexOther(SKOLEM-DEFAULT)@\

@IndexOther(SKOLEM-SELECTION-NAME)@\

@IndexOther(SLIDEPROOF)@\

@IndexOther(SLIDES-PREAMBLE)@\

@IndexOther(SLIDES-TURNSTILE-INDENT)@\

@IndexOther(SLIDES-TURNSTYLE-INDENT)@\

@IndexOther(SOURCE-EXTENSION)@\

@IndexOther(SOURCE-PATH)@\

@IndexOther(SPONSOR)@\

@IndexOther(SQUEEZE)@\

@IndexOther(START-TIME-ENABLED0)@\

@IndexOther(STOP-SAVE)@\

@IndexOther(STOP-TIME-ENABLED0)@\

@IndexOther(SUBPROOF)@\

@IndexOther(SUBST-EQUIV)@\

@IndexOther(SUBST=)@\

@IndexOther(SUBST=L)@\

@IndexOther(SUBST=R)@\

@IndexOther(SUBSTITUTE)@\

@IndexOther(SUGGEST)@\

@IndexOther(SUMMARY)@\

@IndexOther(SUPPORT-NUMBERS)@\

@IndexOther(SUPPRESS-FLAGS)@\

@IndexOther(SUPPRESS-FLAGS-LIST)@\

@IndexOther(SYM=)@\

@IndexOther(SYS-LOAD)@\

@IndexOther(TABLEAU)@\

@IndexOther(TACMODE)@\

@IndexOther(TACTIC-VERBOSE)@\

@IndexOther(TACUSE)@\

@IndexOther(TAG-CONN-FN)@\

@IndexOther(TAG-MATING-FN)@\

@IndexOther(TEST-EASIER-IF-HIGH)@\

@IndexOther(TEST-EASIER-IF-LOW)@\

@IndexOther(TEST-EASIER-IF-NIL)@\

@IndexOther(TEST-EASIER-IF-T)@\

@IndexOther(TEST-FASTER-IF-HIGH)@\

@IndexOther(TEST-FASTER-IF-LOW)@\

@IndexOther(TEST-FASTER-IF-NIL)@\

@IndexOther(TEST-FASTER-IF-T)@\

@IndexOther(TEST-FIX-UNIF-DEPTHS)@\

@IndexOther(TEST-INCREASE-TIME)@\

@IndexOther(TEST-INIT)@\

@IndexOther(TEST-INITIAL-TIME-LIMIT)@\

@IndexOther(TEST-MAX-SEARCH-VALUES)@\

@IndexOther(TEST-MODIFY)@\

@IndexOther(TEST-NEXT-SEARCH-FN)@\

@IndexOther(TEST-REDUCE-TIME)@\

@IndexOther(TEST-THEOREMS)@\

@IndexOther(TEST-VERBOSE)@\

@IndexOther(TESTWIN-HEIGHT)@\

@IndexOther(TESTWIN-WIDTH)@\

@IndexOther(TEX-1-POSTAMBLE)@\

@IndexOther(TEX-1-PREAMBLE)@\

@IndexOther(TEX-LINE-WIDTH)@\

@IndexOther(TEX-MIMIC-SCRIBE)@\

@IndexOther(TEX-POSTAMBLE)@\

@IndexOther(TEX-PREAMBLE)@\

@IndexOther(TEXFORMAT)@\

@IndexOther(TEXPROOF)@\

@IndexOther(TIDY-PROOF)@\

@IndexOther(TIMING-NAMED)@\

@IndexOther(TLIST)@\

@IndexOther(TLOAD)@\

@IndexOther(TPS-TEST)@\

@IndexOther(TPS-TEST2)@\

@IndexOther(TPS3-SAVE)@\

@IndexOther(TPSTEX)@\

@IndexOther(TRANSFER-LINES)@\

@IndexOther(TREAT-HLINES-AS-DLINES)@\

@IndexOther(TRUE-NAME)@\

@IndexOther(TRUTHVALUES-HACK)@\

@IndexOther(TURNSTILE-INDENT)@\

@IndexOther(TURNSTILE-INDENT-AUTO)@\

@IndexOther(TURNSTYLE-INDENT)@\

@IndexOther(TURNSTYLE-INDENT-AUTO)@\

@IndexOther(TYPE-IOTA-MODE)@\

@IndexOther(TYPESUBST)@\

@IndexOther(UGEN)@\

@IndexOther(UI)@\

@IndexOther(UI-HERBRAND-LIMIT)@\

@IndexOther(UNALIAS)@\

@IndexOther(UNIF-SUBSUMED-TEST-ENABLED0)@\

@IndexOther(UNIF-SUBSUMED-TRUE-ENABLED0)@\

@IndexOther(UNIXLIBRARY0)@\

@IndexOther(UNLOADED-MODS)@\

@IndexOther(UNLOCK-LINE)@\

@IndexOther(UNREWRITE-PLAN*)@\

@IndexOther(UNREWRITE-PLAN1)@\

@IndexOther(UNSCRIPT)@\

@IndexOther(UNSPONSOR)@\

@IndexOther(UNTYPED-LAMBDA-CALCULUS)@\

@IndexOther(UNUSE)@\

@IndexOther(USE)@\

@IndexOther(USE-DIY)@\

@IndexOther(USE-DOT)@\

@IndexOther(USE-EXT-LEMMAS)@\

@IndexOther(USE-FAST-PROP-SEARCH)@\

@IndexOther(USE-INTERNAL-PRINT-MODE)@\

@IndexOther(USE-RRULES)@\

@IndexOther(USE-RULEP)@\

@IndexOther(USE-SYMSIMP)@\

@IndexOther(USE-TACTIC)@\

@IndexOther(USE-THEORY)@\

@IndexOther(VPD-BRIEF)@\

@IndexOther(VPD-FILENAME)@\

@IndexOther(VPD-LIT-NAME)@\

@IndexOther(VPD-PTYPES)@\

@IndexOther(VPD-STYLE)@\

@IndexOther(VPD-VPFPAGE)@\

@IndexOther(VPDTEX)@\

@IndexOther(VPFORM-LABELS)@\

@IndexOther(VPFORM-TEX-MAGNIFICATION)@\

@IndexOther(VPFORM-TEX-NEST)@\

@IndexOther(VPFORM-TEX-PREAMBLE)@\

@IndexOther(VPW-HEIGHT)@\

@IndexOther(VPW-WIDTH)@\

@IndexOther(WEIGHT-A-COEFFICIENT)@\

@IndexOther(WEIGHT-A-FN)@\

@IndexOther(WEIGHT-B-COEFFICIENT)@\

@IndexOther(WEIGHT-B-FN)@\

@IndexOther(WEIGHT-C-COEFFICIENT)@\

@IndexOther(WEIGHT-C-FN)@\

@IndexOther(WHICH-CONSTRAINTS)@\

@IndexOther(WRITE-RULE)@\

@IndexOther(^P)@\

@IndexOther(^PN)@\
@End(Description)

@Section(Display)

@Begin(Description)
@IndexOther(LS)@\

@IndexOther(LS-ITEMS*)@\

@IndexOther(UNIXLIB-LOCATE)@\

@IndexOther(UNIXLIB-PDOWN)@\

@IndexOther(UNIXLIB-PUP)@\
@End(Description)

@Section(Best modes)

@Begin(Description)
@IndexOther(SHOW-BESTMODE-THMS)@\
@End(Description)

@Section(Library Classification)

@Begin(Description)
@IndexOther(COPY-CLASS-SCHEME)@\

@IndexOther(MV)@\

@IndexOther(RENAME-CLASS)@\
@End(Description)
@ChapterPh(Menu For The User Interfaces)
The internal name of this category is 
MENU.
A Menu for the User Interface can be defined using DEFMENU.
Allowable properties are: @t{DISPLAY-NAME}, @t{PLACEMENT}, @t{PARENT}, @t{REMOTE-EXPERT}, @t{MHELP}.

@Section(Top Levels)

@Begin(Description)
@IndexOther(ABBREV-OPS)@\

@IndexOther(BEST-MODES)@\

@IndexOther(CHANGING)@\

@IndexOther(ED-JFORMS)@\

@IndexOther(ED-MOVING)@\

@IndexOther(ED-SCRIBE-RECORD)@\

@IndexOther(EDITOR)@\

@IndexOther(EMBEDDING)@\

@IndexOther(EXP-TREE-OPS)@\

@IndexOther(ILL-FORMED-WFF-OPS)@\

@IndexOther(INNER-QUANT-OPS)@\

@IndexOther(JFORMS)@\

@IndexOther(KEYWORDS)@\

@IndexOther(LAMBDA-OPS)@\

@IndexOther(LIB)@\

@IndexOther(LIB-CLASS)@\

@IndexOther(LIB-DISPLAY)@\

@IndexOther(LIB-READ)@\

@IndexOther(LIBRARY)@\

@IndexOther(LIBRARY-FLAGS)@\

@IndexOther(MATE)@\

@IndexOther(MATE-PRINTING)@\

@IndexOther(MATING-SEARCH)@\

@IndexOther(MISC-OPS)@\

@IndexOther(MODES)@\

@IndexOther(MOVING)@\

@IndexOther(MTREE)@\

@IndexOther(MTREE-OPS)@\

@IndexOther(MTREE-PRINT)@\

@IndexOther(NEGATION-OPS)@\

@IndexOther(PRIMSUB-OPS)@\

@IndexOther(PRINT)@\

@IndexOther(REC-CHANGING)@\

@IndexOther(REVIEW)@\

@IndexOther(REVIEW-UNIFICATION)@\

@IndexOther(REWRITING)@\

@IndexOther(SCRIBE-RECORD)@\

@IndexOther(SEARCHLISTS)@\

@IndexOther(SKOLEMIZE)@\

@IndexOther(STRUCT)@\

@IndexOther(SUBSTITUTION)@\

@IndexOther(TEST)@\

@IndexOther(TEST-LIB)@\

@IndexOther(UNIX-STYLE-LIB)@\

@IndexOther(UNIXLIB-CLASS)@\

@IndexOther(UNIXLIB-DISPLAY)@\

@IndexOther(UNIXLIB-READ)@\

@IndexOther(WEAK-LABELS)@\

@IndexOther(WRITE)@\
@End(Description)

@Section(Unification)

@Begin(Description)
@IndexOther(DPAIRS)@\

@IndexOther(UNIFICATION)@\

@IndexOther(UNIFICATION-FLAGS)@\
@End(Description)

@Section(Maintenance)

@Begin(Description)
@IndexOther(AUTO-GEN)@\

@IndexOther(COLL-HELP)@\

@IndexOther(CONJUNCTION)@\

@IndexOther(DEFINITIONS)@\

@IndexOther(DISJUNCTION)@\

@IndexOther(EDITOR-FLAGS)@\

@IndexOther(ENTERING)@\

@IndexOther(ENTERING-FLAGS)@\

@IndexOther(EQUALITY-FLAGS)@\

@IndexOther(EQUATIONS)@\

@IndexOther(EQUIVALENCE)@\

@IndexOther(ETREE-TO-NAT)@\

@IndexOther(ETREE-TO-NAT-FLAGS)@\

@IndexOther(EVENTS)@\

@IndexOther(EXPANSION-TREE-FLAGS)@\

@IndexOther(FILES)@\

@IndexOther(FLAGS)@\
Main menu for most flags.

@IndexOther(HELP-OBJ)@\

@IndexOther(IMPLICATION)@\

@IndexOther(INDIRECT-RULES)@\

@IndexOther(JFORM-FLAGS)@\

@IndexOther(LAMBDA)@\

@IndexOther(LIBRARY-TOP-LEVELS)@\

@IndexOther(LISP-PACKAGES)@\

@IndexOther(MAIN)@\
The Main menu for commonly used TPS commands.

@IndexOther(MAINT)@\

@IndexOther(MANIPULATION-FLAGS)@\

@IndexOther(MATING-SEARCH-COMMANDS)@\

@IndexOther(MATING-SEARCH-FLAGS)@\

@IndexOther(MATING-TREE-FLAGS)@\

@IndexOther(MBAR)@\
The root of the menu tree.  The menus with mbar as a parent
appear on the menu bar of the interface window.

@IndexOther(MISC)@\

@IndexOther(MISC-COMMANDS)@\
Menu for Miscellaneous Commands.

@IndexOther(MISC-FLAGS)@\

@IndexOther(MODIFY)@\

@IndexOther(MS88-FLAGS)@\

@IndexOther(MS89-FLAGS)@\

@IndexOther(MS90-3-FLAGS)@\

@IndexOther(MS91)@\

@IndexOther(MS91-FLAGS)@\

@IndexOther(MS98-1-FLAGS)@\

@IndexOther(NAMING)@\

@IndexOther(NAT-TO-ETREE)@\

@IndexOther(NAT-TO-ETREE-FLAGS)@\

@IndexOther(NATURAL-DEDUCTION-DISPLAY)@\

@IndexOther(NATURAL-DEDUCTION-FLAGS)@\

@IndexOther(NEGATION)@\

@IndexOther(PARSING)@\

@IndexOther(PRINTING)@\

@IndexOther(PRINTING-FLAGS)@\

@IndexOther(PROOF-OUTLINES)@\

@IndexOther(PROOF-TRANSLATIONS)@\

@IndexOther(PROOF-WINDOWS)@\

@IndexOther(PROPOSITIONAL)@\

@IndexOther(QUANTIFIERS)@\

@IndexOther(REWRITE-RULES)@\

@IndexOther(RULE-P-FLAGS)@\

@IndexOther(RULE-RUN)@\

@IndexOther(RULES)@\
Main menu for most flags.

@IndexOther(RULES-OBJECT)@\

@IndexOther(SAVING)@\

@IndexOther(SAVING-FLAGS)@\

@IndexOther(SCRIBE)@\

@IndexOther(SEARCH-FLAGS)@\

@IndexOther(SEARCH-SUGGESTIONS)@\

@IndexOther(SEQUENT-CALCULUS)@\

@IndexOther(SEQUENT-CALCULUS-FLAGS)@\

@IndexOther(SET-MODE)@\

@IndexOther(SET-SUBSTITUTIONS)@\

@IndexOther(STATUS)@\

@IndexOther(SUBSTITIONS)@\

@IndexOther(SUGGESTION-FLAGS)@\

@IndexOther(SUGGESTIONS)@\

@IndexOther(TACTIC-FLAGS)@\

@IndexOther(TACTICS)@\

@IndexOther(TEST-SEARCHLISTS)@\

@IndexOther(TEX)@\

@IndexOther(TOP-LEVELS)@\
Menu for Changing Top Levels.

@IndexOther(TPS-MAINTENANCE)@\

@IndexOther(TPS-MODULES)@\

@IndexOther(VARS)@\
@End(Description)

@Section(Display)

@Begin(Description)
@IndexOther(UNIXLIB-SEARCH)@\
@End(Description)
@ChapterPh(Intermediate Rule Definitions)
The internal name of this category is 
IRULEDEF.
An intermediate rule definition can be defined using DEFIRULE.
Allowable properties are: @t{LINES}, @t{RESTRICTIONS}, @t{PRIORITY}, @t{SUPPORT-TRANSFORMATION}, @t{ITEMSHELP}, @t{HYP-RESTRICT}, @t{MHELP}.
@ChapterPh(Concept Special Characters)
The internal name of this category is 
CONCEPT-CHAR.
A concept special character can be defined using DEFCFONT.
Allowable properties are: @t{CFONT}, @t{END-SYMBOL}, @t{MHELP}.

@Section(Concept)

@Begin(Description, ScriptPush No, Spread 0)
@TabSet(+40pts)
@IndexOther(ALEPH)@\@t{@aleph@;}

@IndexOther(ALPHA)@\@t{@g{a}@;}

@IndexOther(AND)@\@t{@and@;}@\
No more help available.  Sorry.

@IndexOther(ANGLE)@\

@IndexOther(APPROX)@\@t{@approx@;}

@IndexOther(ASSERT)@\@t{@assert@;}@\
No more help available.  Sorry.

@IndexOther(ASSERTEDBY)@\@t{@assertedby@;}

@IndexOther(ASTERISK)@\@t{@ast@;}

@IndexOther(BAR)@\

@IndexOther(BETA)@\@t{@g{b}@;}

@IndexOther(BIGBAR)@\

@IndexOther(BOLDA)@\@t{@bolda@;}

@IndexOther(BOLDB)@\@t{@boldb@;}

@IndexOther(BOLDC)@\@t{@boldc@;}

@IndexOther(BOLDD)@\@t{@boldd@;}

@IndexOther(BOLDE)@\@t{@bolde@;}

@IndexOther(BOLDF)@\@t{@boldf@;}

@IndexOther(BOLDG)@\@t{@boldg@;}

@IndexOther(BOLDH)@\@t{@boldh@;}

@IndexOther(BOLDI)@\@t{@boldi@;}

@IndexOther(BOLDJ)@\@t{@boldj@;}

@IndexOther(BOLDK)@\@t{@boldk@;}

@IndexOther(BOLDL)@\@t{@boldl@;}

@IndexOther(BOLDM)@\@t{@boldm@;}

@IndexOther(BOLDN)@\@t{@boldn@;}

@IndexOther(BOLDO)@\@t{@boldo@;}

@IndexOther(BOLDP)@\@t{@boldp@;}

@IndexOther(BOLDQ)@\@t{@boldq@;}

@IndexOther(BOLDR)@\@t{@boldr@;}

@IndexOther(BOLDS)@\@t{@bolds@;}

@IndexOther(BOLDT)@\@t{@boldt@;}

@IndexOther(BOLDU)@\@t{@boldu@;}

@IndexOther(BOLDV)@\@t{@boldv@;}

@IndexOther(BOLDW)@\@t{@boldw@;}

@IndexOther(BOLDX)@\@t{@boldx@;}

@IndexOther(BOLDY)@\@t{@boldy@;}

@IndexOther(BOLDZ)@\@t{@boldz@;}

@IndexOther(CAPDELTA)@\@t{@g{D}@;}

@IndexOther(CAPGAMMA)@\@t{@g{G}@;}

@IndexOther(CAPLAMBDA)@\@t{@g{L}@;}

@IndexOther(CAPOMEGA)@\@t{@g{W}@;}

@IndexOther(CAPPHI)@\@t{@g{F}@;}

@IndexOther(CAPPI)@\@t{@g{P}@;}

@IndexOther(CAPPSI)@\@t{@g{Y}@;}

@IndexOther(CAPSIGMA)@\@t{@g{S}@;}

@IndexOther(CAPTHETA)@\@t{@g{Q}@;}

@IndexOther(CAPUPSILON)@\@t{@g{U}@;}

@IndexOther(CAPXI)@\@t{@g{X}@;}

@IndexOther(CEILING1)@\@t{@ceiling1@;}

@IndexOther(CEILING2)@\@t{@ceiling2@;}

@IndexOther(CHI)@\@t{@g{c}@;}

@IndexOther(CIRCLEDOT)@\@t{@circledot@;}

@IndexOther(CIRCLEMINUS)@\@t{@ominus@;}

@IndexOther(COMPOSE)@\@t{@compose@;}@\
No more help available.  Sorry.

@IndexOther(CONGRUENT)@\

@IndexOther(DEFINEEQ)@\

@IndexOther(DEL)@\@t{@partial@;}

@IndexOther(DELTA)@\@t{@g{d}@;}

@IndexOther(DIAMOND)@\@t{@diamond@;}

@IndexOther(DIRECTSUM)@\@t{@directsum@;}

@IndexOther(DIVIDE)@\@t{@divide@;}

@IndexOther(DOUBTILDE)@\@t{@approx@;}

@IndexOther(EPSILON)@\@t{@g{e}@;}

@IndexOther(EQUIV)@\@t{@equiv@;}

@IndexOther(ETA)@\@t{@g{h}@;}

@IndexOther(EXISTS)@\@t{@exists@;}@\
No more help available.  Sorry.

@IndexOther(FALSEHOOD)@\@t{@falsehood@;}@\
No more help available.  Sorry.

@IndexOther(FLAT)@\

@IndexOther(FLOOR1)@\@t{@floor1@;}

@IndexOther(FLOOR2)@\@t{@floor2@;}

@IndexOther(FORALL)@\@t{@forall@;}@\
No more help available.  Sorry.

@IndexOther(GAMMA)@\@t{@g{g}@;}

@IndexOther(GRADIENT)@\@t{@nabla@;}

@IndexOther(GREATEQ)@\@t{@greateq@;}

@IndexOther(IFF1)@\@t{@iff1@;}

@IndexOther(IFF2)@\@t{@iff2@;}

@IndexOther(IMP1)@\@t{@imp1@;}

@IndexOther(IMP2)@\@t{@imp2@;}

@IndexOther(IMP3)@\@t{@imp3@;}

@IndexOther(IMPLIED1)@\@t{@implied1@;}

@IndexOther(IMPLIED2)@\@t{@implied2@;}

@IndexOther(IMPLIEDBY)@\@t{@impliedby@;}

@IndexOther(IMPLIES)@\@t{@implies@;}@\
No more help available.  Sorry.

@IndexOther(INFINITY)@\@t{@infinity@;}

@IndexOther(INTEGRAL2)@\

@IndexOther(INTERSECT)@\@t{@intersect@;}

@IndexOther(IOTA)@\@t{@g{i}@;}@\
No more help available.  Sorry.

@IndexOther(JOIN)@\@t{@join@;}

@IndexOther(KAPPA)@\@t{@g{k}@;}

@IndexOther(LAMBDA)@\@t{@g{l}@;}@\
No more help available.  Sorry.

@IndexOther(LEFTCORNER)@\

@IndexOther(LESSEQ)@\@t{@lesseq@;}

@IndexOther(MEET)@\@t{@meet@;}

@IndexOther(MEMBER1)@\@t{@member1@;}

@IndexOther(MINPLUS)@\@t{@mp@;}

@IndexOther(MONUS)@\

@IndexOther(MU)@\@t{@g{m}@;}

@IndexOther(NATURAL)@\

@IndexOther(NEG)@\@t{@neg@;}@\
No more help available.  Sorry.

@IndexOther(NEWPAR)@\@t{@newpar@;}

@IndexOther(NONMEMBER)@\@t{@nonmember@;}

@IndexOther(NORM)@\@t{@norm1@;}

@IndexOther(NORTH)@\@t{@north@;}

@IndexOther(NORTHEAST)@\@t{@northeast@;}

@IndexOther(NORTHWEST)@\@t{@northwest@;}

@IndexOther(NOT)@\@t{@not@;}@\
No more help available.  Sorry.

@IndexOther(NOTASSERT)@\@t{@notassert@;}

@IndexOther(NOTEQ)@\@t{@noteq@;}

@IndexOther(NOTEQUIV)@\@t{@notequiv@;}

@IndexOther(NOTVALID)@\@t{@notvalid@;}

@IndexOther(NU)@\@t{@g{n}@;}

@IndexOther(NULLSET)@\@t{@emptyset@;}

@IndexOther(OMEGA)@\@t{@omega1@;}

@IndexOther(OMICRON)@\@t{@g{o}@;}

@IndexOther(OR)@\@t{@or@;}@\
No more help available.  Sorry.

@IndexOther(PARALLELOGRAM)@\

@IndexOther(PHI)@\@t{@g{f}@;}

@IndexOther(PHI2)@\@t{@phi2@;}

@IndexOther(PI)@\@t{@g{p}@;}

@IndexOther(PLUSMIN)@\@t{@pm@;}

@IndexOther(POWERSET)@\@t{@powerset@;}

@IndexOther(PROPERSUBSET)@\@t{@PrSubset@;}

@IndexOther(PROPERSUPERSET)@\@t{@PrSupset@;}

@IndexOther(PSI)@\@t{@g{y}@;}

@IndexOther(QUANTIFIER)@\

@IndexOther(RHO)@\@t{@g{r}@;}

@IndexOther(RIGHTCORNER)@\

@IndexOther(SCRIPTA)@\@t{@scripta@;}

@IndexOther(SCRIPTB)@\@t{@scriptb@;}

@IndexOther(SCRIPTC)@\@t{@scriptc@;}

@IndexOther(SCRIPTD)@\@t{@scriptd@;}

@IndexOther(SCRIPTE)@\@t{@scripte@;}

@IndexOther(SCRIPTF)@\@t{@scriptf@;}

@IndexOther(SCRIPTG)@\@t{@scriptg@;}

@IndexOther(SCRIPTH)@\@t{@scripth@;}

@IndexOther(SCRIPTI)@\@t{@scripti@;}

@IndexOther(SCRIPTJ)@\@t{@scriptj@;}

@IndexOther(SCRIPTK)@\@t{@scriptk@;}

@IndexOther(SCRIPTL)@\@t{@scriptl@;}

@IndexOther(SCRIPTM)@\@t{@scriptm@;}

@IndexOther(SCRIPTN)@\@t{@scriptn@;}

@IndexOther(SCRIPTO)@\@t{@scripto@;}

@IndexOther(SCRIPTP)@\@t{@scriptp@;}

@IndexOther(SCRIPTQ)@\@t{@scriptq@;}

@IndexOther(SCRIPTR)@\@t{@scriptr@;}

@IndexOther(SCRIPTS)@\@t{@scripts@;}

@IndexOther(SCRIPTT)@\@t{@scriptt@;}

@IndexOther(SCRIPTU)@\@t{@scriptu@;}

@IndexOther(SCRIPTV)@\@t{@scriptv@;}

@IndexOther(SCRIPTW)@\@t{@scriptw@;}

@IndexOther(SCRIPTX)@\@t{@scriptx@;}

@IndexOther(SCRIPTY)@\@t{@scripty@;}

@IndexOther(SCRIPTZ)@\@t{@scriptz@;}

@IndexOther(SETINTERSECT)@\@t{@setintersect@;}

@IndexOther(SETUNION)@\@t{@setunion@;}

@IndexOther(SIGMA)@\@t{@g{s}@;}

@IndexOther(SIMILAR)@\@t{@similar@;}

@IndexOther(SOUTH)@\@t{@south@;}

@IndexOther(SOUTHEAST)@\@t{@southeast@;}

@IndexOther(SOUTHWEST)@\

@IndexOther(SQRT)@\@t{@squareroot@;}

@IndexOther(SQUARE)@\@t{@square@;}

@IndexOther(STAR)@\@t{@star@;}

@IndexOther(SUB0)@\@t{@-{0}@;}

@IndexOther(SUB1)@\@t{@-{1}@;}

@IndexOther(SUB2)@\@t{@-{2}@;}@\
No more help available.  Sorry.

@IndexOther(SUB3)@\@t{@-{3}@;}

@IndexOther(SUB4)@\@t{@-{4}@;}

@IndexOther(SUB5)@\@t{@-{5}@;}

@IndexOther(SUB6)@\@t{@-{6}@;}

@IndexOther(SUB7)@\@t{@-{7}@;}

@IndexOther(SUB8)@\@t{@-{8}@;}

@IndexOther(SUB9)@\@t{@-{9}@;}

@IndexOther(SUBALPHA)@\@t{@-{@g{a}}@;}

@IndexOther(SUBBETA)@\@t{@-{@g{b}}@;}

@IndexOther(SUBCHI)@\@t{@-{@g{c}}@;}

@IndexOther(SUBDELTA)@\@t{@-{@g{d}}@;}

@IndexOther(SUBEPSILON)@\@t{@-{@g{e}}@;}

@IndexOther(SUBETA)@\@t{@-{@g{h}}@;}

@IndexOther(SUBGAMMA)@\@t{@-{@g{g}}@;}

@IndexOther(SUBIOTA)@\@t{@-{@g{i}}@;}

@IndexOther(SUBKAPPA)@\@t{@-{@g{k}}@;}

@IndexOther(SUBLAMBDA)@\@t{@-{@g{l}}@;}

@IndexOther(SUBLPAREN)@\@t{@-{(}@;}

@IndexOther(SUBMEMBER)@\@t{@submember@;}

@IndexOther(SUBMU)@\@t{@-{@g{m}}@;}

@IndexOther(SUBNU)@\@t{@-{@g{n}}@;}

@IndexOther(SUBNULLSET)@\@t{@subnullset@;}

@IndexOther(SUBOMEGA)@\@t{@-{@g{w}}@;}

@IndexOther(SUBOMICRON)@\@t{@-{@g{o}}@;}

@IndexOther(SUBPHI)@\@t{@-{@g{f}}@;}

@IndexOther(SUBPI)@\@t{@-{@g{p}}@;}

@IndexOther(SUBPSI)@\@t{@-{@g{y}}@;}

@IndexOther(SUBRHO)@\@t{@-{@g{r}}@;}

@IndexOther(SUBRPAREN)@\@t{@-{)}@;}

@IndexOther(SUBSET)@\@t{@subset@;}

@IndexOther(SUBSIGMA)@\@t{@-{@g{s}}@;}

@IndexOther(SUBTAU)@\@t{@-{@g{t}}@;}

@IndexOther(SUBTHETA)@\@t{@-{@g{q}}@;}

@IndexOther(SUBUPSILON)@\@t{@-{@g{u}}@;}

@IndexOther(SUBXI)@\@t{@-{@g{x}}@;}

@IndexOther(SUBZETA)@\@t{@-{@g{z}}@;}

@IndexOther(SUP0)@\@t{@+{0}@;}

@IndexOther(SUP1)@\@t{@+{1}@;}

@IndexOther(SUP2)@\@t{@+{2}@;}

@IndexOther(SUP3)@\@t{@+{3}@;}

@IndexOther(SUP4)@\@t{@+{4}@;}

@IndexOther(SUP5)@\@t{@+{5}@;}

@IndexOther(SUP6)@\@t{@+{6}@;}

@IndexOther(SUP7)@\@t{@+{7}@;}

@IndexOther(SUP8)@\@t{@+{8}@;}

@IndexOther(SUP9)@\@t{@+{9}@;}

@IndexOther(SUPA)@\@t{@+{a}@;}

@IndexOther(SUPB)@\@t{@+{b}@;}

@IndexOther(SUPC)@\@t{@+{c}@;}

@IndexOther(SUPD)@\@t{@+{d}@;}

@IndexOther(SUPE)@\@t{@+{e}@;}

@IndexOther(SUPERSET)@\@t{@supset@;}

@IndexOther(SUPF)@\@t{@+{f}@;}

@IndexOther(SUPG)@\@t{@+{g}@;}

@IndexOther(SUPH)@\@t{@+{h}@;}

@IndexOther(SUPI)@\@t{@+{i}@;}

@IndexOther(SUPJ)@\@t{@+{j}@;}

@IndexOther(SUPK)@\@t{@+{k}@;}

@IndexOther(SUPL)@\@t{@+{l}@;}

@IndexOther(SUPLPAREN)@\@t{@+{(}@;}

@IndexOther(SUPM)@\@t{@+{m}@;}

@IndexOther(SUPMINUS)@\@t{@+{-}@;}

@IndexOther(SUPN)@\@t{@+{n}@;}

@IndexOther(SUPO)@\@t{@+{o}@;}

@IndexOther(SUPP)@\@t{@+{p}@;}

@IndexOther(SUPPLUS)@\@t{@+{+}@;}

@IndexOther(SUPQ)@\@t{@+{q}@;}

@IndexOther(SUPR)@\@t{@+{r}@;}

@IndexOther(SUPRPAREN)@\@t{@+{)}@;}

@IndexOther(SUPS)@\@t{@+{s}@;}

@IndexOther(SUPT)@\@t{@+{t}@;}

@IndexOther(SUPU)@\@t{@+{u}@;}

@IndexOther(SUPV)@\@t{@+{v}@;}

@IndexOther(SUPW)@\@t{@+{w}@;}

@IndexOther(SUPX)@\@t{@+{x}@;}

@IndexOther(SUPY)@\@t{@+{y}@;}

@IndexOther(SUPZ)@\@t{@+{z}@;}

@IndexOther(TAU)@\@t{@g{t}@;}

@IndexOther(TENSOR)@\@t{@tensor@;}

@IndexOther(THETA)@\@t{@theta1@;}

@IndexOther(TIMES)@\@t{@times@;}

@IndexOther(TRUTH)@\@t{@truth@;}@\
No more help available.  Sorry.

@IndexOther(UNCAPPI)@\

@IndexOther(UNION)@\@t{@union@;}

@IndexOther(UNTILDE)@\

@IndexOther(UPSILON)@\@t{@g{u}@;}

@IndexOther(VALID)@\@t{@valid@;}

@IndexOther(XI)@\@t{@g{x}@;}

@IndexOther(ZETA)@\@t{@g{z}@;}
@End(Description)
