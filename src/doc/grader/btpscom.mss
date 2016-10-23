@AppBTPS(BTPS Commands)@label(btps-command)

@Section(On-line Help: HELP, ENVIRONMENT, NEWS)
@Hand[@BTPSComI(HELP) @i(tps-object) prints a brief description
of what @i(tps-object) is or does.]

@begin[comment]
@Hand[@BTPSComI(ENVIRONMENT) lets you investigate the information-tree
for the current environment. For our purposes, this is a @BTPS
with @GRADER loaded.]
@end[comment]

@Hand[@BTPSComI(NEWS) tells you what is new in @BTPS-land.]

@Section(System: EXIT, TERMINAL, DO-GRADES)

@Hand[@BTPSComI(TERMINAL) @i{terminal-type} tells @BTPS to treat
your terminal as a @i{terminal-type} terminal.]

@Hand[@BTPSComI(EXIT) takes you out of @GRADER and @BTPS.]

@Hand[@BTPSComI(DO-GRADES) enters @GRADER from @BTPS. The @GRADER
commands are the subject of this manual.]

@Section(Concept Terminal: LOADKEY,RESET)

@Hand[@BTPSComI(RESET) puts the Concept in the right mode and loads the 
function keys.]

@Hand[@BTPSComI(LOADKEY) @i(function-key-number) @i(input) loads
the key @t(f)@i(function-key-number) so that when it is pressed,
@i(input) is added to the current input line.]

@Exempli[I load function key @b(f5) with the input @t(HELP):]
@Begin(TPSExample)
<2>loadkey 5 help
@End(TPSExample)

Now I press @b(f5) and return. The key was not loaded with the return.

@Begin(TPSExample)
<3>HELP
KEYWORD (SYMBOL): TPS object [No Default]>reset
RESET is a command.
Put a Concept terminal into correct mode and load the function @^keys.
The command format for RESET is:

<n>RESET   @Gratia
@End(TPSExample)
