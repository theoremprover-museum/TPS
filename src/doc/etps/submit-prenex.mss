@make(text)
@use(database ="/afs/andrew.cmu.edu/mcs/math/TPS/doc/lib")
@libraryfile(tpsdocuments)
@comment(See [gtps]/home/theorem/project/doc/etps/prenex.hlp)
@heading[Submitting Prenex Normal Form Solutions]
@center[for 21-300 and 21-600]

You must submit your solutions to prenex normal form exercises (from section 22) in two forms,
machine-readable and human-readable. You need not show how you obtained 
your answers.

@subheading(Creating the prenex normal form in ETPS)

All of the exercises from x2200 to x2214 have been added to
ETPS as saved wffs. This means that, for example, you can type @t(ED X2201) to
examine  and manipulate X2201 in the editor.  

You may wish to  issue the command @t(setflag style), and then, at the
subsequent prompt, @t(generic) so that you can see examples of how to type these
wffs into ETPS.  You may find it convenient to write your prenex
normal forms in a text file and then copy them into the ETPS editor.

However, a better procedure is to use the commands in the ETPS editor
to transform the wff given in the exercise into the desired prenex
normal form. Editor commands which you may find useful include CW, AB,
DB, MBED-E, and MBED-F.  These are discussed in the ETPS manual, and
you can also find out about them by using the help command in
ETPS. For example, type @t[HELP MBED-F].  Note that if you use DB to
delete a quantifier, the current wff in the editor (edwff) will become
the former scope of that quantifier, and you may wish to use the
moving command ^ to go back to the top (front) of the wff.

@subheading(Machine-readable submissions)

Suppose you are creating a prenex normal form for X2201. From the editor, you
can use the SAVE command to store your answer in a file. (Note: this command
will save the "current" edwff, not the entire wff of which the current edwff
may be a subformula.)  You will be prompted for a name for this wff; please
use the format EXERCISE-NAME for the name. For example, if your  name
is Jim Smith, you should give the prenex normal form of X2201 the name
X2201-SMITH. 
@comment[However, if you think there may be other students in the class with 
the same last name as yours, include your first name, and 
give the prenex normal form of X2201 a name like
X2201-SMITH-JIM.]

This will create a file savedwffs.lisp in the directory from which you
are running ETPS. In this file, you will find an expression of the
form
@begin(verbatim)
(DEFSAVEDWFF X2201-SMITH
  (REPRESENTS "some well-formed formula"))
@end(verbatim)

If ETPS asks you to choose another name for saving (as it will, if you 
already have a wff called X2201-SMITH), save it under a different name and 
then edit the savedwffs.lisp file. There will be some expression of the form 
@begin(verbatim)
(DEFSAVEDWFF THE-NAME-YOU-CHOSE
  (REPRESENTS "some well-formed formula"))
@end(verbatim)
Just change THE-NAME-YOU-CHOSE to X2201-SMITH before you submit it.

If you cannot finish all the prenex normal form exercises in the same session
with ETPS, you can load 
the wffs that you previously  saved in savedwffs.lisp into ETPS by 
using the command @t(QLOAD "savedwffs.lisp") from ETPS.

Please email the file containing the answers to all the assigned prenex normal
form exercises, and nothing else, as plain text (not an attachment), to
okv@@andrew.cmu.edu, with Subject "prenex-21-300" or "prenex-21-600", depending
on which class you are registered for. It is important that you use exactly 
this subject heading so that the procedure for collecting these homework
submissions will not miss yours.  Be careful to include the
@i(entire) expression for each exercise answer. Do not break up any lines,
even if they appear to be extremely long. If you really cannot avoid
breaking lines, be sure you do it where there are spaces in the lines.

@subheading(Human-readable submissions)

Please also hand in a paper showing what your answers look like, in
traditional logical notation. This can be handwritten, but as long as you
already have these in ETPS, you may find it convenient to proceed as follows:
give your answers temporary names by using the CW command before you leave the
editor, create a proof of an arbitrary wff like p with the command @t(PROVE
"p"), insert your wffs in this proof by using the LEMMA command, indicate
which wff corresponds to which exercise by using the LINE-COMMENT command or
by numbering the lines of the proof appropriately, and use TEXPROOF or
SCRIBEPROOF to get a nice printout.



