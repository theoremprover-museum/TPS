@part(Introduction, root "ETPS.mss")
@ChapterPH(@HETPS User Interface)

@Section(Introduction) @label(introduction)
You may find the Table of Contents at the end of this manual.

@ETPS is a program which is designed to help you do your logic homework.
It was developed from @TPS, an ongoing research project in automated
theorem proving. It is written in Common Lisp.

To do your homework on the computer, first enter @ETPS.  It is highly
recommended that you run @ETPS in an X-window (on a Unix or Linux
system) or use the Java interface for @ETPS (if these facilities are
available on your system), so that formulas can be printed on the
screen using special fonts which contain logical symbols, and so that
the proof you are developing can be printed and updated in special
proofwindows as you work.

You can start the Java interface for @ETPS using the @IndexCommand(JAVAWIN) command;
see Section @ref(JavaInterface).
If you are running
@ETPS in an X-window equipped with the special fonts used to print logical
symbols, you need to tell @ETPS that you want to use the special fonts for
output.  At the @ETPS prompt, issue the command@*
@t(setflag style)@*
 then, at the subsequent prompt@*
@t(xterm)@*
This will cause the special symbols to appear
when any wffs are printed by @ETPS. (If for some reason the special symbols
won't print properly, just change the style back from  @t(xterm) to @t[generic].)

If you are running @ETPS in an X-window or using the Java interface,
you will probably also wish to use the @IndexCommand(BEGIN-PRFW)
command to start up windows containing the current subproof and the
complete proof; see Section @ref(Proofwindows). You may need to
iconify a window or move it up on your screen by the usual methods for
manipulating windows so that you will have room to issue @ETPS
commands.  You can eventually close the proofwindows with the
@IndexCommand(END-PRFW) comand.  See Section @ref(proofstatus) for
commands which control what is regarded as the current subproof.

To help you learn how to use @ETPS, transcripts of sample sessions
are provided in Chapter @ref(examples). Just follow these examples exactly
on your own computer, and you will soon have a general idea of what to do.
You can also do some practice
(unassigned) exercises (which you can find with the aid of the 
@IndexCommand(PROBLEMS) command), and make frequent use of the
@IndexCommand(ADVICE) command, which will offer suggestions about what
to do next.

You should note that the only means of identification available
to @ETPS is the @i(userid) of the account from which it is run. It will
credit all work to the owner of that account @i(and to no other user).
Thus, in order to receive credit for an exercise, you @i(must) run @ETPS 
from @i(your own) account. Run it from a private directory so that the
files which @ETPS creates containing your proofs will not be accessible
to others.

Start work on an exercise with the command @T(EXERCISE)
@i(exercise-name); see Section @ref(start-finish).  Next construct a
complete proof using the inference rules described in Chapter
@ref(Inference). However, some of the more powerful inference rules
may not be allowed for certain exercises. To find out which rules are
prohibited, you should invoke the @IndexCommand(HELP) command on the
exercise.  If you cannot figure out what the next step in a proof
should be, you may get hints by using the @IndexCommand(ADVICE)
command; but beware: these hints are not always helpful and can be
misleading.

A partially completed proof will be called a @i(proof outline) or
simply an @i(outline).  When you start proving a theorem with the
@IndexCommand{EXERCISE} or @IndexCommand{PROVE} command, @ETPS will
create an outline for you which contains a single line: the theorem
you would like to prove.  It is not yet justified since you are only
planning to prove it.  In place of the justification there will be the
word @T(PLAN1).  This last line of the outline is therefore called a
@i{planned} line.  Lines which are justified can be introduced by
justifying a planned line, introducing a hypothesis, or deriving
consequences of already justified lines.  Proofs may be built down
from the top, adding consequences of established lines, or up from the
bottom, justifying planned lines by asserting new planned lines. It is
a good idea to work up from the bottom as much as possible, so that
@ETPS will already know about most of the wffs you need, and you will
not have to type them in.

@Begin(comment)
These lines are
called @I(deduced) lines.  Hypotheses will appear as
@Begin(Verbatim)
(@I(n)) @I(n) ! @I(assertion) @>Hyp
@End(Verbatim)
@End(comment)

@ETPS was originally developed for use with the textbook
@b[An Introduction to Mathematical Logic and Type Theory: To Truth
Through Proof] by Peter B. Andrews. A second edition was published
by Kluwer Academic Publishers in 2002, and is now available from 
Springer, which which has taken over Kluwer.
Exercises from this textbook are available in @ETPS. However, the
inference commands available in your version of @ETPS may depend on
the particular logical system chosen by your teacher.  Inference
commands are specified as inference rules and are listed in Chapter
@ref(inference).  Ideally, after repeatedly applying rules to planned
and deduced lines, they will ``meet'' and every planned line will be
justified.  The examples in Chapter @ref(examples) should make all
this clearer.

When you have finished the proof, issue the @T(DONE) command. This will record
the completion of the exercise @i(if) you used the @T(EXERCISE) command.
The @T(PROVE) command lets you prove arbitrary wffs, @i(but it will not
give you credit for any exercises to which these wffs correspond.) 
@ETPS sends only a partial record of your session to a file on the teacher's 
area. You must submit a printed copy of the finished proof, as well.
To make this copy, first print the proof into a file, using the @T(TEXPROOF)
command. When you have exited from @ETPS, run this file through @T(TEX).
Then print the press file generated by @T(TEX) and
hand this output to your teacher.

A word of advice to the user: @ETPS is intended to aid you in
learning logic, but if you use it thoughtlessly, you might be able
to do exercises without learning much. @ETPS does not allow you to use
the rules of logic in an incorrect manner, but it's important that
you learn for yourself how to apply the rules correctly. By allowing
only correct applications of the rules, @ETPS encourages the user to
spend more time learning the techniques of proving theorems in logic.
 It is strongly
recommended that when @ETPS does not allow you to do something, you 
think about why what you tried was incorrect.

@Section(Saving and Restoring Your Work)

@ETPS saves your work automatically as you try to prove a theorem.
This facility is very similar to Emacs' auto-save feature.  
@ETPS commands @indexcommand(SAVE-WORK),
@indexcommand{STOP-SAVE}, @indexcommand{RESUME-WORK}
and @indexcommand{RESUME-SAVE} allow you to explicitly use this feature.
@ETPS also provides the commands @indexcommand{SAVEPROOF} and 
@indexcommand{RESTOREPROOF} for saving your proofs.
Unlike the automatic saving facility, which
saves everything typed by  the user (and later re-executes every command in the
file), this feature only saves a proof (and later restores this
proof, thus avoiding the re-execution of everything that was done to 
achieve this state). It is, however, up to you to decide when to save a
proof. Although the auto saving feature starts to save your work
whenever you start an exercise, you need to explicitly use the command
@indexcommand{SAVEPROOF} when you wish to save a proof.
 Typically, you will want to save the proof whenever you need
to interrupt a session in which you are constructing a proof and  wish to
continue this proof later.
@ETPS commands associated with saving work are described in Section 
@ref(savework). 

As soon as
you start an exercise (but not a proof of your own theorem), @ETPS will save
your commands with the appropriate arguments in a file.  The name of
this file will be @t(@i(exercise).work) where @i(exercise) is the first
exercise attempted in your current session with @ETPS.  
When @ETPS is saving work, it echoes every character you type
in a stream obtained by opening the save-work file. The echo stream is 
closed only when the user issues the command @T(STOP-SAVE) or
the command @T(EXIT) (to exit @ETPS in the usual way). The save-work
file is not available until @ETPS closes this stream. 
@comment<Ideally, this 
stream should be closed and reopened after every few commands (so that
if the computer crashes the user can restore the status quo by executing
the @T(RESTORE-WORK) command on the save-work file).
Because of a Common Lisp bug, we are unable to do this. The echo stream is 
closed only when the user issues the command @T(STOP-SAVE) or
the command @T(EXIT) (to exit @ETPS in the usual way).
Hence, it's recommended that if you are saving your work you should not
use @T(Ctrl-C) to leave @ETPS. Also, if you must guard against
sudden computer crashes, you can stop saving (command @T(STOP-SAVE)),
restart saving (command @T(SAVE-WORK)) in a different file, and finally
merge the files in the proper order using your favorite editor. But
beware: use this technique with great discretion. If you use it too
often, it's possible that you may not be able to save your work after
a while because you have exceeded the Common Lisp limit on the number of 
I/O streams.>

One more caution:  When starting the same exercise in two different
sessions, the @i(same filename) will be used.  The work for the new
attempt will overwrite the old file @T(@i(exercise).work).
To save the old attempt, rename it before restarting @ETPS.
@comment<If you forget to do so, you may still save the old version @i(before
you log out). Use the Exec command @wt(@@UNDELETE @i(exercise).work.*) and then
rename older versions, e.g., @wt(@@RENAME @i(exercise).work.1
@i(exercise)-OLD.work).>

After your work has been restored by @T(RESTORE-WORK), @ETPS will continue
to save subsequent work into the same file by appending it to the end.
If you would like to prevent that, give the @T(STOP-SAVE) command
right after @T(RESTORE-WORK); better yet, use the @T(EXECUTE-FILE)
command.

When commands are read from a save-work file, most errors such as illegal
arguments or logical mistakes are faithfully re-executed, since some of
them have side-effects.  Only Lisp errors will lead to an abort of
the @T(RESTORE-WORK) and the state of the proof will be the same as after
the last correct command read from the file. 

You may edit the save-work file in Emacs to delete wrong commands or
correct illegal arguments, but you'll be skating on thin ice.  It's easy
to make a mistake in editing the save-work file and you may not be able
to recover the proof you wanted to restore! The inquisitive user may note
that lines beginning with a semi-colon are ignored when the proof is 
being restored.

There are several options you have when using this auto-save feature.
You may switch off saving with the @IndexCommand(STOP-SAVE) command.
Also, you can explicitly save into a different file with the
@IndexCommand(SAVE-WORK) command. 
To check whether your work is being saved, use the system command
@IndexCommand(PSTATUS).

@comment<You also have the option of keeping a complete record of the session,
including the system responses, in a @T(PHOTO.LOG) file. If you want to prepare
such a copy, issue the command @t{@@PHOTO} before you enter @ETPS. Use the
Exec command @t(@@HELP PHOTO)  for more details. Note, however, that the file
obtained this way cannot be used to restore your work as described earlier in 
this section.>

You also have the option of keeping a complete record of the session,
including the system responses, in a file. If you want to prepare
such a copy, issue the @ETPS command @IndexCommand{SCRIPT}. Note, however, 
that the file obtained this way cannot be used to restore your work as 
described earlier in this section.


@Section(Exiting and Reentering @HETPS)@label(reentering)

If you wish to  end a session with @ETPS or temporarily
interrupt your work,  use the @indexcommand(EXIT) command.
This allows @ETPS to halt gracefully and to close an
open save-work file. 

If you are running @ETPS under Unix and wish to interrupt a session
temporarily, you can also use @t(Ctrl-Z).  This will return you
immediately to the monitor; if you are currently saving work, the
save-work file @b(will not be closed).  Thus any work will be lost
unless you return to @ETPS. Once out of @ETPS, you can run other
programs, such as @t(TEX).  To reeneter @ETPS, use the Unix command
@t(fg).

@comment<, but you must take some action to ensure that the current state 
of @ETPS will be kept by TOPS-20.  Otherwise, you will have to begin an
entirely new 
session of @ETPS and recreate or restore any work which you had done 
before.  There are several ways to make sure that the @ETPS ``fork'', as
TOPS-20 calls it, is kept.  One way is to add the following line to
your @t(COMAND.CMD) file: @wt(set program etps keep reenter).  This will
always keep @ETPS in memory; then, when you wish to go back to @ETPS, issue
the command @t(@@etps).  The @t(reenter) part of the command line will
cause @ETPS to resume saving into a save-work file if you were saving work
when you exited @ETPS, and if you exited via the @t(EXIT) command.

Another way to keep @ETPS in memory is to issue the command
@wt{@@push} immediately after exiting, then perform whatever 
commands you wish, e.g. @t(@@NEW:TEX), then do
a @wt{@@pop}.  To restart @ETPS, use the command @wt{@@reenter} if you
exited via the @t(EXIT) command, or @wt(@@continue) if you exited via
a @t(Ctrl-C).

Here let us point out a significant difference between the
TOPS-20 commands @t(continue) and @t(reenter).  
@t(continue) will restart @ETPS exactly as it was when it
exited to the TOPS-20 monitor.  If @ETPS closed a save-work file just
before it exited, that file will remain closed, and you will have to
use the @indexcommand(RESUME-SAVE) command to resume saving.  The
TOPS-20 @t(reenter) command, in contrast, will cause @ETPS to restart in
such a way that any save-work file that was closed upon exiting will
be automatically reopened; in addition, you will return to the
@ETPS top-level, even if you exited by using a @t(Ctrl-C) during the
execution of a command.  In general, if you exit by typing a @t(Ctrl-C), you
will want to restart @ETPS by using @t(continue); use @wt(reenter) if
you exited by the @t(EXIT) command.  This is why we recommend that you
@i(always) leave @ETPS with the @t(EXIT) command, and then subsequently
use @t(reenter) to restart @ETPS.  @t(Ctrl-C) is useful, however,
if you somehow get @ETPS into an infinite loop and can't abort the
current command with a @t(Ctrl-G).

One final way to keep the current @ETPS in memory 
is to issue a @t(@@keep etps) command to TOPS-20 immediately upon exiting.  
To return to @ETPS, type @t(@@etps).
Note, however, that this will cause TOPS-20 to do a @t(continue), 
rather than a @t(reenter).  

At some point, you may wish to get rid of a ``kept'' @ETPS and start fresh 
with a new @ETPS (such as if @ETPS has inexplicably gone into an 
infinite loop). You can do so by issuing the TOPS-20 command
@wt(@@unkeep etps), then simply start again.  In addition,
note that no matter which of the above methods you use, logging out 
will destroy even a ``kept'' @ETPS.>

@Section(Top-level Interaction)@label(top-level)

In @ETPS every command line is identified with a statement number,
shown in angle brackets. 
After a while, command line numbers are
reused, i.e., commands issued long ago are forgotten.

The following is a list of useful control characters@Index(control
characters) and their meaning in @ETPS. Some of them may not work
under certain operating systems or display environments. Additional
control characters@Index(control
characters) are discussed in Section @ref(control-characters).

@Begin(Description)
@T(<Rubout>)@\ Delete the last character and back over it.

@T(Ctrl-C)@\ Quit current command and return to top-level. (In some
implementations of @ETPS you must use @T(Ctrl-G) instead.) Using this 
command may cause problems for any save-work file that is being created,
so it may be better to use ABORT.

@T(Ctrl-U)@\ Delete current input line.

@T(Ctrl-W)@\ Delete last input word.

@End(Description)

The next three characters are special on the top-level only and are currently
not to be used when typing in arguments to commands.

@Begin(Description)
@T(@@)@\ Complete the name of the current command. If @indexflag(COMPLETION-OPTIONS)
is NIL, this works only if there is a unique completion; if it is T, you will be offered a list
of completions to choose from.

@T(<Escape>)@\ Exactly equivalent to @T(@@). This character can confuse some terminals;
we recommend using @t(@@) instead.

@indexcommand(?)@\ When typed at the top-level, @ETPS will list all the available
commands. Note that @T(?) must be followed by a @T[<Return>].
@seealso[primary="Help",other="@t(?)"]
@indexentry[key="Help",entry="Help"]
@comment<	
Provide a list of possible completions of the partially typed
command.  A @T(?) typed at the prompt will give a list of all possible
@ETPS commands.>

@T{<Linefeed>}@\ This starts a new line on the terminal without
terminating the input.  This is useful for long command arguments.
@End(Description)

@begin(comment)
The following special character is extremely useful for redisplaying
the relevant part of the current proof.
@Begin(Description)
@T(^P)@\ Redisplay the current planned line and all of its support
lines.  See Section @ref(printing)  for a more detailed explanation.
@End(Description)

 Typing an integer at the top-level will retype the command line with
that number.  You can then change it with @T(<Rubout>) or @T(^W) or
simply type @T(<Return>) to reexecute this command.  Type @T(^U) if
this wasn't the command you had in mind.

@Begin(Description)
@IndexCommand(n)@\ Retype the nth command.
@End(Description)
@end(comment)

@SECTION(Using Commands and Defining Wffs)
The commands available in @ETPS are classified into system commands and
inference commands. The system commands, which are discussed in Chapter
@ref(SYSTEM), deal with communication facilities, starting and finishing
a session with @ETPS, 
the Java interface, proofwindows, the current subproof,
saving work, printing, 
rearranging the proof, getting assistance, displaying types in higher-order
logic, setting flags,
and locating
and constructing wffs.
The inference commands, which are discussed in Chapter @ref(INference),
correspond to inference rules.  They transform one proof outline into another
by applying the rules.

Common to all commands is the way they are invoked.
Simply type the name of the command (you may use @t(<Esc>) to complete
its name) and then type @t(<Return>). The command may be printed in either
upper or lower case. If the command takes arguments,
@ETPS will prompt you in succession for each argument.  The prompt
takes the general form
@Begin(Verbatim)
@i(argument name) (@i(argument type)) : @I(help) [@I(default)]>

For example:

<1>@T(PROVE)
WFF (GWFF0): Prove wff [No Default]>
PREFIX (SYMBOL): Name of the proof [No Default]>
NUM (LINE): Line number for theorem [100]>
@end(verbatim)

You may also specify command arguments on the command line itself.
@ETPS will then prompt you only for the arguments you haven't specified.
This is a useful option for commands like @wt{PL 2 50}, which directly prints
every line in the range from @T(2) to @T(50).

After @ETPS issues a prompt, you have the following options for your reply
besides typing in the argument.
@Begin(Description)
@t(<Return>)@\ This selects the default, if there is one.

@t(!)@\ This selects the default, not only for this argument, but all remaining
arguments. This can also be used on the command line itself.

@t(?)@\ This gives help about the type of the argument @ETPS is waiting for.

@t(??)@\ This gives help about the command for which you are supplying
arguments. In particular, when applying an inference rule, this will
give you the statement of the rule.

@t(@indexother(ABORT))@\ Aborts current command.

@IndexCommand(PUSH)@\ Temporarily suspend whatever you're doing and start a new top-level. 
The command @IndexCommand(POP) will return you to the point at which you typed @t(PUSH).

@t(Ctrl-G<Return> or Ctrl-C)@\   To abort.  This is a system-dependent feature, and one or the other, or both, may not work on your system.
 Using this command may cause problems for any save-work file that is being 
created, so it may be better to use ABORT.
@End(Description)
If a mistake in an argument can be detected right away, @ETPS will 
complain and let you try again. Sometimes @ETPS will note that something is 
wrong after all the arguments are typed in.  You will then get an error 
message and be thrown back to the top-level of @ETPS.

The @i(argument name) is usually only important when typing in
arguments to inference commands.  If you are in doubt
what wff you are supposed to type in now, look at the description
of the inference rule.  The name of the argument will be the same
as the name of a wff in the rule description.

The @i(argument type) tells you what kind of object @ETPS expects.
The most important argument types are listed below. (You may 
omit most of the rest of this section when first reading this manual. However,
be sure to read the description of GWFF.)
