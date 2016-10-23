@Appendix(Amenities) @label(amenities).

@PageHeading(Immediate, Right <@b(Appendix @Ref(Appendix))>, Odd)
@PageHeading(Immediate, Left  <@b(@Value(DocName))>,         Even)

@ETPS incorporates several features of the Unix C-shell
(csh) top-level.  These features include various control characters,
command sequences, a  history
mechanism, and aliases.

@Section(Control characters for Unix)@label(control-characters)

If you are running @ETPS under Unix (or Linux), you may be able to use
the following  control characters@Index(control characters) 
in addition to those discussed in Section @ref(top-level).

@Begin(Description)

@T(Ctrl-S)@\ Freeze output.

@T(Ctrl-Q)@\ Proceed with output.

@T(Ctrl-Z)@\ Suspend the current  program (@ETPS), and return to the monitor.
@comment<
@T(^X)@\ Quit current command and return to the initial (@ETPS) top-level.
@T(^X)@\ Abort @ETPS and return to the monitor.>

@T(Ctrl-R)@\ Redisplay the current input line.

@comment<@T(Ctrl-O)@\ Flush output up to the next @T(Ctrl-O) or a prompt.>
@End(Description)

@section(Command Sequences)

You may enter a series of commands on the same command line 
by using the ampersand (@t(&)) as a separator.  This is analogous to the
C-shell's use of the semicolon (@t(;)).  That is, entering

@t(<0> command@-<1> & command@-<2> & ... & command@-<n>)

will cause @ETPS to sequentially execute @i(command@-<1>) through
@i(command@-<n>) as though you had typed them in one at a time.

For example, after you have finished a proof, you may want to enter the
sequence:

@t(<0> cleanup & squeeze & done & texproof !)

@section(History Substitutions)

It is often convenient to be able to refer to commands and arguments
that you have already typed.  
As in the C-shell, the exclamation point (@t<!>) is used to indicate
a history substitution, with two exceptions.  An exclamation point that 
is followed by whitespace will not be interpreted as a history reference,
nor will an exclamation point that is immediately preceded by a 
a backslash (@t<\>).
Any input line that contains history
substitutions will, before execution, be echoed on the screen as it would
appear without the history references.

In @ETPS, each command line is given a
unique number; this number is part of the top-level prompts.  
A certain number of previous commands are saved by @ETPS; the
number saved is determined by the flag @indexflag(HISTORY-SIZE).  
The previous command is always saved.
In
addition, each line is parsed into a series of @i(tokens).  Unlike the
C-shell, these tokens are not distinguished simply by surrounding whitespace,
but rather by their Lisp syntax.  All that the user needs to know is
that, in general, each argument entered on a command line will be considered 
a separate
token.  On each input line, the tokens are numbered from left to right, 
beginning at 0.  For example,  the
input line 

@t{<n> rulep 27 (1 2 7 14)}

would be parsed into three
tokens: @t(rulep), @t(27) and @t<(1 2 7 14)>, which would be numbered 0, 1
and 2, respectively. 

The @indexcommand(HISTORY) command is used to examine the list of
input lines that have been saved by @ETPS.  It takes two arguments,
the first being the number of lines to show (defaulting to the entire
list), and the second being whether to show them in reverse numerical
order (defaulting to no).  The number of each input line is also
given.  The lines are saved in the history list as they appear after
all history substitutions are made.

Previous lines can be referred to using the following input line specifiers:
@begin(description)

@t(!n) @\ the command line whose number is @t(n). 

@t(!-n) @\ the input line that was entered @t(n) lines above
the current one.

@t(!!) @\ the previous line.

@t(!@i<str>)@\ the most recent command that begins with the string @i(str).

@t(!?@i<str>?)@\ the most recent command that has a token
containing the substring @i(str).
@end(description)

Here are some examples.  Suppose we had the following output
from the @t(@indexcommand(HISTORY)) command
@verbatim(
<10> history 5 !
    6 exercise x2106
    7 pstatus
    8 ^p
    9 pall
   10 history 5 !
)

Then, as input line 11, we could refer to line 7 by @t(!ps) or @t(!-4),
or even by @t(!?tat?). 

Used alone, the above references merely insert every token of the line
referred to into the the current input line.  In order to select
particular tokens from an input line, a colon (@t<:>) follows the 
input line specifier, along with a selector for the intended tokens.
Here is the syntax for the token selectors, where @i(x) and @i(y)
indicate arbitrary token selectors.
@begin(description)
         @t(0)@\     first (command) word

         @i(n)@\     @i(n)'th argument

         @t(^)@\     first argument,  i.e. @t(1)

         @t($)@\     last argument

         @t(%)@\     word matched by (immediately preceding) @t(?@i(str)?) search

         @t<@i(x)-@i(y)>@\   range of words from the @i(x)'th through the @i(y)'th

         @t(-)@i(y)@\    abbreviates @t<0-@i(y)>

         @t(*)@\     abbreviates @t<^-$>, or nothing if only 1 word in
input line referred to

         @t(@i<x>*)@\    abbreviates @t<@i(x)-$>

         @t<@i(x)->@\    like @t<@i(x)*> but omitting word @t($)
@end(description)

The @t(:) separating the event specification from the token
selector can be omitted if the token selector begins
with a @t(^), @t($), @t(*), @t(-) or @t(%). 

Going back to our example, we can then create the input line 

@verbatim(<11> help x2106)

by entering @t(help !5:*), or @t(help !ex:$), or
@t(help !?2?%).

Here is a longer example of the use of history substitutions.
We will omit the output of the commands themselves, showing only the
results of history substitutions in italics.  
@begin(verbatim)
<38> prove "[[A and B] and C] implies [B or C]" foo 100
<39> deduct !!:$ 99 50
@i(deduct 100 99 50)
<40> econj !39:$ !
@i(econj 50 !)
<41> !e:0 !
@i(econj !)
<42> texproof "!?implies?:2_proof1.mss" 
@i(texproof "foo_proof1.mss")
@end(verbatim)

One cautionary note: It is unwise to use absolute references to input
line numbers (e.g., @t(!25)) in your work files, because when the file
is executed again, it is unlikely that a particular line numbered
@t(n)  will be the same as
line @t(n) was when the work file was created.

You may wish to know what a command history substitution will look
like without executing it.  In order to do that, merely choose a word
that is not a command (such as "foobar"), and prefix your history 
substitution by that 
word.  @ETPS will first echo the substituted line, then just complain
that "foobar" is an unknown command.


@section(Aliases) @label(aliases)

@ETPS maintains a list of aliases which can be created, 
printed and removed by the @t(@indexcommand(ALIAS)) and 
@t(@indexcommand(UNALIAS))
commands.  Each input line is separated
into distinct commands and the first word of each command
is checked to see if it has an alias.  If it
does, then the text which is the alias for that command is
reread with the history mechanism available as though that
command were the previous input line.  The resulting tokens
replace the command and argument list.  If no history references appear,
then the argument list is not changed.

As an example, if the alias for @t(ded) is @t("deduct"), 
the command @t(ded 100 !)
would be interpreted as @t(deduct 100 !).  If the alias for @t(pr) was 
@t("prove \!\!:1 foo 100"), then @t(pr x2106) would become
@t(prove x2106 foo 100).  Note that any occurrences of @t(!) in 
the alias definition that are meant to be expanded when the alias
is invoked must be escaped with a backslash (@t<\>) to keep them
from being interpreted as history substitutions when the alias is defined.

If an alias is found, the token transformation of the input
text is performed and the aliasing process begins again on
the new input line.  Looping is prevented if the first
word of the new text is the same as the old by flagging it
to prevent further aliasing.  

The command @t(@indexcommand(ALIAS)) can be used to create or display an
alias,
or to display all existing aliases.  The command @t(@indexcommand(UNALIAS))
can be used to delete an existing alias.
The following example will illustrate:
@begin(verbatim)

@\ @i(We define an alias.)

<135>alias d "deduct \!\!:1-$ \!"

@\ @i(We show its definition.)

<136>alias !
d         deduct !!:1-$ !
<137>exercise x2106
<138>d

@\ @i(This expands to "deduct !!:1-$ !" but here $ is 0.)

TPS error while reading.
!!:1-$: Bad ! arg selector.  Last of range is less than first.

@\ @i(We'll remove this definition, and try again.)

<139>unalias d

@\ @i(The * selector is what we want.)

<140>alias d "deduct \!\!:* \!"
<141>d

@\ @i(This expands to "deduct !", which is what we intend.)
@\ @i(Suppose now that we have finished the proof.)

<155>alias finish "cleanup & squeeze & done & pall & texproof \!\!:*"
T
<156>finish "myproof.mss"
@end(verbatim)








