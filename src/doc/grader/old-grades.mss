@chapterPh(Grading Package)
@section(Introduction)
The grading package is a computer program which can be used to
create and maintain a record analogous to an instructor's grade book,
and to process and display the recorded information. It allows the
user to create a new class list, add or delete
students from the class list, insert new grades, modify
existing grades, calculate totals and statistical data, compute
weighted scores, assign letter grades, sort grades, print
grades (in a file) in  various formats (some of which are designed
to make comparison easy), etc. Section @ref(top-level) gives a list
of available commands.

The program records the following information (in addition to student 
records) in the @t(grade-file):
@begin(itemize)
A row containing names of exercises 

A row containing the types of the exercises (This is meant to classify
the exercises as @wt(HW) (homework), @t(EXAM), etc.)

A row containing weights assigned to the exercises. (This allows the
user the option of assigning different weights to different exercises.
The default weight for each exercise is 1. The user can change it to any
desired number by invoking the command @wt(change-weight) within the grading 
package.)

A row labeled @t(--MAX--)  showing the maximum scores
on the exercises
@end(itemize)

The program allows the user the facility to update the @t(grade-file) by
merging grades from a file which is maintained by a different package.
The user may refer to section @ref(etps) for a more detailed discussion.

When the program is used to calculate totals, it allows the user the
option of computing weighted scores. This facility can be used to
compute arbitrary linear combinations of the different classes
that are specified in the @t(grade-file). For example: Suppose the user
has classified the exercises in the @t(grade-file) as @t(HW), @t(EXAM), and 
@t(FINAL). Then the user can compute any linear combination of these
entries by assigning appropriate weights (say 1.5 to @t(HW), 0 to
@t(EXAM), and 3.43 to @t(FINAL)).

The program also has the capability of creating a file containing
the names of the students and letter grades assigned to them. Actually
it creates this file (unless @wt(letter-grade-flag) is @t(nil))
whenever it is used to assign letter grades.

Whenever the program makes some changes which alters the record of any
student, it automatically creates a new copy of the @t(grade-file). The name
of this file depends on the current value of the variable @wt(grade-file).
 If this file already exists, it renames it (the new
name depends on the current value of the variable @wt(old-grade-file))
 before creating the new file. 
Similarly when the program computes the total scores, it renames the
current version of @wt(totals-grade-file). The new name depends on the
current value of the variable @wt(old-totals-grade-file).
The program, however, doesn't create a back up file
for @wt(letter-grade-file) which is created whenever the program is
used to assign letter grades.

@section(Top Level Interaction)@label(top-level)
@subsection(Entering And Exiting The Grading Package)
On @wt(CMU-CC-TF) issue the command  @wt(@@run <ET99>BTPS) to the monitor. On
@wt(CMU-CS-C), use the command  @wt(@@run <TPS>BTPS). Then use
the command @Wt(do-grades).
You will now enter  the top level of the grading package. The top level
of the grading package can be recognized by the prompt @wt{<Gr@i(n)>}, where
@i(n) is an integer between 1 and 10. Typing @t(?) in response to the 
top level prompt will provide you with a list of available commands.
Also, you can get a brief help message about any top level command
(telling you what the command does) by typing @wt(help command-name), where
@i(command-name) is the command for which you desire help. 

Command completion is available within the grading package. 
The user can partially type any top level command, and hit the @t(<ESC>)
key. The grading package will then  complete that (top-level)
command (if it can do it uniquely). The grading package can also
complete partially typed student names and exercise names.
(This facility, however, is available only when the program 
expects the user to respond with student names or exercise names.)

To exit the grading package, use the command @t(EXIT) or @t(QUIT).
@subsection(Top Level Commands)
The various top level commands are@foot(The section numbers in front
of these commands refer to sections containing a brief description of
these commands.):
@tabclear
@tabset(2inch)
@begin(format, use R)
ADD-STUDENTS@\ Section @ref(add-s)

ALIASES@\ Section @ref(aliases)

CALCULATE-GRADE@\ Section @ref(calc)

CHANGE-WEIGHT@\ Section @ref(chan)

CHG-VARS@\ Section @ref(chgv)

COMMENT@\ Section @ref(commnt)

CREATE-GRADEFILE@\ Section @ref(create)

DELETE-STUDENT@\ Section @ref(del)

DISPLAY@\ Section @ref(display)

DUE-DATES@\ Section @ref(duedates)

ETPS-GRADE@\ Section @ref(etps)

EXIT

INFO-EXERCISES@\ Section @ref(info-exercises)

INSERT-GRADES@\ Section @ref(newgrades)

LATE-EXERCISES@\ Section @ref(late-exe)

LETTER-GRADE@\ Section @ref(letter) 
	
MODIFY-GRADE@\ Section @ref(modify)

NUMBER-OF-STUDENTS@\ Section @ref(number-of-students)

PENALTY-FNS@\ Section @ref(penalty)

QUIT

SORT-FN@\ Section @ref(sort)

STATISTICS@\ Section @ref(stat) 
@end(format)	

@section(Creating New Grade Files)@label(create)
The command for creating new @t(grade-file) is @t(CREATE-GRADEFILE).

The program will ask you to type the name of the file 
containing a list of students.
(The list of students need not be in alphabetic order. The program
arranges the students in alphabetic order.)
The default for this file is 
@wt("<@i(Directory)>Student.Lst"). You can specify a  different
filename, if you desire. 

This file must contain a list of students, but
can have either of the following formats:
@begin(itemize)
@t{(,,name1 ,,name2 ,,name3 ...   ,,nameN)}

If you plan to use this program to merge the @wt(grade-file) with the file
containing @ETPS records, then you must use the following style:@*
((@i(name1 userid1))(@i<name2 userid2>)...(@i<nameN useridN>))
@end(itemize)

The program expects only one word for the name of each student. If you
want to use more than one word, please use a hyphen (or any other non blank
character) instead of blanks to separate the first and last names. However,
periods and commas are not allowed.

@section(Changing or Examining Variables)@label(chgv)
The command for changing or examining the values of variables
is @wt{CHG-VARS}.

The default value@foot{The user can specify different default values
by having appropriate @t(LISP) expressions in the file @t(ETPS.INI).
(e.g., The @t(lisp) expression @i[(setq course-name 'math)] will force
the grading package to use the value @t(MATH) instead of @t(COURSE)
as the default value for the variable @t(course-name).}
of some of the variables is@foot(A brief discussion
about the meaning of these variables follows immediately after this
table.):

@tabclear
@tabset(0.3inch,2.5inch)
@begin(format, use R)
1@\ @t(Course-Name)@\ COURSE
2@\ @t(Grade-File)@\ PS:<@i<directory>>@foot(In this chapter
<@i<directory>> will refer to the directory that the user is 
logged on.)COURSE.GRADES
3@\ @t(Totals-Grade-File)@\ PS:<@i<directory>>COURSE.TOTALS
4@\ @t(Old-Grade-File)@\ PS:<@i<directory>>COURSE.XGRADES
5@\ @t(Old-Totals-Grade-File)@\ PS:<@i<directory>>COURSE.XTOTALS
6@\ @t(Letter-Grade-File)@\ PS:<@i<directory>>COURSE.LETTER
7@\ @t(ETPS-File)@\ PS:<TPS>ETPS.REC@foot(This is the default value on
CMU-CS-C. On CMU-CC-TF the default value is PS:<ET99>ETPS.REC.)
8@\ @t(Statistical-Options)@\ (-SDEV- -MEDIAN- -MEAN-)
9@\ @t(Print-n-Digits)@\ 0@foot(The
value of this variable must be a non negative integer.)
10@\ @t(Cal-Percentage)@\ NIL
11@\ @t(Letter-Grade-Flag)@\ T
12@\ @t(New-Item)@\ NIL
13@\ @t(Drop-Min)@\ NIL
@end(format)
@begin(itemize)
To change the names of any files, type
@wt{"@I(<dir>)@I(name).@I(ext)"}, @wt{"@I(name).@I(ext)"}, or simply
@wt{@I(name)}. The @i(<dir>) here can be any directory that the
user wants to specify. The quotation marks must be typed.


When calculating totals, etc., the program prints @i(print-n-digits) digits
after the decimal.@*For example, if the value of @t(print-n-digits) is 0,
1 or 2, then 23.546 will be printed as 24, 23.5, or 23.55 respectively.

If the variable @t(cal-percentage) is non-@t(nil), the program
(when used to calculate totals)	computes
the percentage score (on the basis of total scores) for each student.

If the variable @t(letter-grade-flag) is non-@t(nil), the program
(when used to assign letter grades) creates a file (whose name depends
on the variable @t(letter-grade-file)) containing names of students and
the letter grades assigned to them.

If the variable @t(drop-min) has value @i{(item1 item2 item3...)}, the
program (when used to calculate totals) drops the minimum scores on
each of these items.

If the variable @t(new-item) has value@*
@i{((name1 item11 weight11 item12 weight12 ...)}@*
@i{(name2 item21 weight21 item22 weight22 ...)...)},@*
the program (when used to calculate totals) computes @i(name1) by assigning
@i(weight11) to @i(item11), @i(weight12) to @i(item12), etc., 
computes @i(name2) by assigning @i(weight21) to @i(item21), @i(weight22) 
to @i(item22), etc. For each of the items that does not occur, the program 
assigns a weight of 1.
@* For example: Suppose the @t(grade-file) has 3 different
categories of entries (these correspond to distinct entries in the row
with @T(TYPE) as the first entry in the @t(grade-file))	@t(HW), @T(TEST),
and @T(FINAL). Also assume that the value of the variable @t(new-item)
is @t[((abc test 0)(xyz hw 1.5 final 2))], and there's a (arbitrary)
student named @t(John) who has scored 30 on the HW, 60 on the TEST,
and 50 on the exam. Then the program will compute items @t(abc) and 
@t(xyz) for John as follows:@*
abc  (30*1)+(60*0)+(50*1)   = 80@*
xyz  (30*1.5)+(60*1)+(50*2) = 205@*
@i(Note that the program used a weight of 1, whenever the weight was
not specified.)

The program calculates mean, median, and standard
deviation based on the value of the variable @t(Statistical-Options).
The default
value is @t[(-sdev- -median- -mean-)]. The program computes this data
for the items which have been modified during the current session,
whenever it updates the @i(grade-file) or @i(totals-grade-file).
@end(itemize)

@section(Displaying grades on the terminal)@label(display)
The command for looking at the grades on the terminal is @t(DISPLAY).

You can ask the program to display on the terminal, the 
@t(-mean-, -sdev-, -median-)
or grades for one or all of the students
on the desired exercises. @t(-mean-, -sdev- ) and @t( -median- ) should be
requested when you are prompted for a student name.

If all the grades for a student are requested, then the late exercises and 
their number of days late will also be shown.

@section(Adding Students)@label(add-s)
The command for adding students to the @t(grade-file) is @t(ADD-STUDENTS).

When you use this command,
the program will prompt you for a list of students. Now specify the
names of the students in the following format:

@begin(verbatim ,use i)
(name1 grade1 ... gradeN)(name2 grade1 ... gradeN)...
@end(verbatim)

For each student the program checks whether the number of grades
specified is equal to the number of grades in the existing @t(grade-file).
If the number is more, it deletes additional items from the end.
On the other hand, if the number of grades is less, it pads the
student record with an appropriate number of @t(~ 's).

@section(Deleting Students)@label(del)
The command for deleting students from the @t(grade-file) is @t(DELETE-STUDENT).

When you use this command, the program
will ask you to specify a list of students. Now specify the
names of the students in the following format:

@begin(verbatim, use i)
name1 name2 name3 ...
@end(verbatim)

@section(Inserting New Grades)@label(newgrades)
The command for inserting new grades is @wt(INSERT-GRADES).

You can insert one or more grades at the same time. Please make sure
that the names assigned to the grades are distinct from the names
of the students, and the various types. Also, you can't use @t(T) as
the name of an exercise (because this is a special @t(LISP) symbol).
For each
of the grades that you want to insert, the program asks you to specify its
type. (The user can specify any arbitrary type.)
Note that any letter grade must have the type @t(LE).
Otherwise, the program assumes that it's some type of numerical grade,
and it'll automatically convert it to zero when you ask it to calculate
the totals.

You can specify the scores in any of the following formats:
@begin(enumerate)
As a number.

As a @T(LISP) expression, e.g., @t{(* (+ 2 3) 4)}  (This is the same as
specifying 20).

Any arbitrary object, e.g., * (perhaps to denote that the student
did not do an exercise). All non numerical grades are considered to
be 0 (unless it's type is @t(LE)), when the program is used to
calculate @t(totals).
@end(enumerate)

The new scores should be entered as items separated by blanks.
If an item is a list, it will have its components added.

@section(Modifying Student Records)@label(modify)
The command for modifying student records is @wt(MODIFY-GRADE).

If you decide to change the existing grade(s),
the program prompts you to type the names of the students whose
grades you want to modify. For each such student, it asks you the
names of the exercises whose scores you want to modify.
The program then types the student's old scores and prompts you
for the new scores. (You can specify the new scores in any of the 
formats that are described in Section @ref(newgrades).)

@section(Merging Records Maintained by Another Program)@label(ETPS)
The command for merging records maintained by another program with
the grade-file maintained by this grading package is @wt(ETPS-GRADE).

The grading package can be used to copy any scores from the file @t(ETPS-FILE)
to the @t(GRADE-FILE). The only requirements are:
@begin(itemize)
The file @t(ETPS-FILE) must contain a sequence of lists each of which
has the following format:@*
@t{(,,userid ,,message ,,exe-name  ... ,,date ,,time)}@*
where @i(message) is  @t(DONE) when the exercise is complete, and @t( ... ) can
be replaced by arbitrary data.

The file @t(GRADE-FILE) must have a userid for each of the students. If this
is not known @t( **** ) may be specified as the userid.
@end(itemize)

Whenever the user
wants to update the @t(grade-file) by copying new scores from the 
@wt(ETPS-FILE) to the @wt(grade-file),
the program automatically  updates the scores for
any  exercises that were copied before.
It, however, doesn't alter any existing scores. These exercises are
given an @t(ET) exercise-type. [The user may define other exercise-types,
e.g. @t(HW). These allow grading-schemes and penalty-functions to be
defined in a manner appropriate to the exercise-type.]

The program also
produces a record of the students who finished the exercise late
(based on the due dates that have been assigned) and remembers the
number of days that the assignment is late. If the program can't find
the due date for an exercise, it prompts the user.

@section(Calculating Grades)@label(calc)
The command for calculating grades is @wt(CALCULATE-GRADE).

When you invoke this command, the program
@begin(itemize)
forms a list of all letter grades, i.e., grades which have type @t(LE)
(@t(LE) stands for Letter)

calculates the sum of grades for each distinct type

drops the minimum scores if the value of the variable @t(drop-min) is 
non-@t(nil) (See the discussion on @t(drop-min) in section @ref(chgv)
for a brief description of how to set the value of this variable)

computes the grand total

for each student computes the percentage score (on the basis of the
totals), if the variable @t(cal-percentage) is non-@t(nil)

computes any other linear combinations desired by the user, if
the value of the variable @t(new-item) is non-@t(nil).
(See the discussion on @t(new-item) in section @ref(chgv)
for a brief description of how to set the value of this variable)
@end(itemize)
After computing as above, the program
records all these  grades, in alphabetic order (by student names),
in a file whose name depends on the current
value of the variable @wt(totals-grade-file).

@section(Sorting Grades)@label(sort)
The command for sorting grades is @wt(SORT-FN).
The program first prompts you for the field to be
used in sorting. It then sorts the grades according to the field
specified by you, appends the sorted grades to the @wt(totals-grade-file),
and asks if you want to sort the grades using a different field.
Although you can sort the grades alphabetically, yet you may
note that the grades which are first recorded in the
@wt(totals-grade-file) are arranged alphabetically (by student names).

Before it appends the sorted grades to the @wt(totals-grade-file),
the program asks the user to specify the items to be displayed (in
addition to the one that was used for sorting).
The program is capable of printing sorted grades in the following
styles:
@begin(description)
Incremental Display@\ The program prompts you for an increment (call it
@t(I , ) the default is 5). It then prints the highest score (call it 
@t(X , ) rounded up to the nearest multiple of 5),prints the records of all 
those students whose scores lie between @t(X) and @wt(X-I), prints @wt(X-I), 
prints the records of all those students whose scores lie between
@wt(X-I) and @wt(X-2*I), etc.

Simple Display@\ A plain display without increments.
@end(description)

@section(Assigning Letter Grades)@label(letter)
The command for assigning letter grades is @wt(LETTER-GRADE).
The user can use the program to assign letter grades
on the basis of some numerical grade. These grades can be recorded in
@wt(letter-grade-file) or @wt(totals-grade-file). 

The program also creates a new file, whose name depends on the current value
of the variables @wt(letter-grade-file), which 
contains only the names of students and the letter grades assigned to them.
(This file is not created if the value of @wt(letter-grade-flag) is @t(nil).)

The program first prompts the user for the name of the new letter grade.
It then prompts the user for the name of the field which is to be
used in assigning letter grades. The program then asks the user
for a list of letter grades and the corresponding  minimum scores.
These grades need not be in numerical order. For example, the
user can respond with:

@begin(verbatim, use t)
(b 70)(a+ 80)(c 60)(b- 75)(r 10)
@end(verbatim)
This would be interpreted as follows (assume that the score is @i(x)):
@tabclear
@tabset(0.2inch,+0.2inch,+0.2inch,+0.2inch,+1inch,+2inch)
@begin(format, use R)
Score@\ @\ @\ @\ Letter

@\ @\ @i(x)@\ @math{@gte}@\ 80@\ A+
80@\ @math{@gt}@\ @i(x)@\ @math{@gte}@\ 75@\ B-
75@\ @math{@gt}@\ @i(x)@\ @math{@gte}@\ 70@\ B
70@\ @math{@gt}@\ @i(x)@\ @math{@gte}@\ 60@\ C
60@\ @math{@gt}@\ @i(x)@\ @math{@gte}@\ 10@\ R
10@\ @math{@gt}@\ @i(x)@\ @\ @\ ~. It assigns this grade, and prints a message,
@\ @\ @\ @\ @\ whenever it fails to decide the grade.
@end(format)

@section(Changing Weighting Factors)@label(chan)
The command for changing weights is @WT(CHANGE-WEIGHTS).

The default weighting factor for each @t(HW, ) @ETPS exercise, exam, etc., is
1. You can change it to any desired number. This change will be in
effect, until you change it again (even when you exit the grading
package and start it at a later time).

When you invoke this command, the program prompts you for the names of
the exercises whose weights are to be changed. It then displays the
current weights of each of these exercises, and prompts you for the
new weights.

@section(Assigning Due Dates)@label(duedates)
The command for assigning due dates is @WT(DUE-DATES).

The program expects the dates in the following format:
@begin(verbatim, use R)
	yy mm dd     @i(where 0 < mm < 13 and 0 < dd < 32)
     e.g., 84 10 29 which stands for 1984 Oct 29.
@end(verbatim)

@section(Late Exercises)@label(late-exe)
The command for keeping track of students who submit assignments
after the due-dates is @t(LATE-EXERCISES).

The program will prompt you for the  names of the students, for the
exercises that were submitted late, and the number of days that each
exercise was late.

@section(Aliases)@label(aliases)
The command for assigning actual names to the exercises is @t(ALIASES).

The instructor can use short names (e.g.@t(, a, b, c,) etc.) as the 
names of the exercises (so that when the scores are displayed they
can fit on one paper), and use this command to keep track of the 
actual names of these exercises.

@section(Assigning Penalty Functions)@label(penalty)
The command for assigning penalty functions is @wt(PENALTY-FNS).
These functions are used by the program for assigning appropriate
penalty (as determined by the instructor) for exercises that are
submitted late. The instructor can specify different penalty
functions for different exercises.

The default value for these functions is @t(PENALTY). (This function
deducts one point for each day that the exercise was late.) The functions
@t(PENALTY-0) and @t(PENALTY%10) are also available. @t(PENALTY-0) assigns 
0 points for each late exercise. @t(PENALTY%10) deducts @t(10%) for each day 
late. 

The user may assign penalties using his own @t(LISP) functions.
These functions should expect to receive three arguments
@i(score, ) @i(days-late, ) and @i(max-score, ) and must return the appropriate
score after assigning the penalty. The function @t(penalty%, ) a generalization
of @t(PENALTY%10), is available.

@*For Example:  

@\ @\ @t{(defun penalty%,,n (score dayslate max-score)}
@\ @\ @t{  (penalty% score dayslate max-score ,,n))}

will deduct @t(,,n% ) for each day late.

@section(Inserting Comments in the Grade-File)@label(commnt)
The command for inserting comments is @t(COMMENT).

The user has the option of inserting arbitrary comments in the 
@t(grade-file).
After the user has typed the comments (the comments must be enclosed
in double-quotes), the program appends them at the end of the @wt(grade-file).
The program doesn't try to interpret these comments.

@section(Number of Students) @label(number-of-students)
The command for finding the number of students in the @t(grade-file)
is @t(NUMBER-OF-STUDENTS). 

The program responds with the number of students in the @t(grade-file).

@section(Statistical Data)@label(stat)
The command for computing statistical data is @t(STATISTICS).

The user will hardly ever need to use this command, since the program 
keeps track of the items for which it needs to recompute the
statistical data, and automatically invokes this function before 
printing grades in the @i(grade-file). If, however, the user wishes to 
recompute the data for one or more items, the value of the variable 
@i(ListOfChanges) must be changed before invoking this command.
The value of this variable can be set in various ways from the top level
of the grading package:
@*For Example:  @wt{(push ,,item listofchanges)} where @i(item) is the
name of the item for which the user wants to recompute the data.

@section(info-exercises)@label(info-exercises)

This command displays the due-date, penalty-functions, maximum score
and alias for the desired exercises. The user is prompted for a list
of exercise names.
