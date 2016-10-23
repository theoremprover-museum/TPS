@ChapBTPS(Keeping Records)@label(keepbook)


@Section(Manual Grades: INSERT-GRADES, @W{RESUME-INSERT-GRADES}, @w(MODIFY-GRADE),  @w[LATE-EXERCISES])
@Label(insertg)

@Begin(Hand)
@GrComI(INSERT-GRADES) must be used every time you manually enter grades 
for a new assignment. If you later wish to change the grades on that
assignment, you should use @GrCom(MODIFY-GRADE).
@End(Hand)

@GrCom(INSERT-GRADES) first prompts you for the names of the new exercises.
Recall the warning in Section @ref(Precautions) about the names of
exercises et al. Also, you should use short names to improve the
legibility of your output. You can then use the @GrCom(ALIASES)
command to assign the real name to the short name. See Section
@ref(alias) for details.

Next, the type for each exercise must be given. There are two
pre-defined types, @t(LE) and @t(ET). @t(ET) is the type for the
computer-generated grades retrieved by @GrCom(ETPS-GRADE).
@t(LE) is for letter-grades, the only type of grades which 
@GrCom(CALCULATE-GRADE) (see Chapter @ref(calc)) treats 
as symbols rather than numbers.

Finally, you are asked for the grades. For each student in the class,
you will be prompted for that student's grades on the new exercises.
These grades may be entered as any @LISP expression. You will usually
give expressions such as: @t[98, 4.5, B+, D- or *]. (Standing alone,
* indicates that the exercise was not done; when * is the first element of a list,
it is the multiplication operator, e.g. @t[(* 2 3)] means @t(6)).
@Index(missing grade)@Index(blank grade)@Index(*)

@Exempli[
]
@Begin(TPSExample)
<Gr3>insert-grades

Please enter the names of the exercises, e.g.,HW1 EXAM1 LETTER1: 
essay1
ESSAY1
Please enter 1 items specifying the type of each @^exercise: 
hw
--MAX-- : 60
ARNOLD-MATTHEW : 57
CARLYLE-THOMAS : (* 9 6)
DICKENS-CHARLES : *
DISRAELI-BENJAMIN : (+ 4 (* 6 7))
GASKELL-ELIZABETH : (+ 25 23 1)
RUSKIN-JOHN : 52    @Gratia
@End(TPSExample)

Producing the following record in the @grvar(grade-file):

@Begin(TPSExample)
(
(NAME                ID ESSAY1)
(TYPE                **     HW)
(--MAX--             **     60)
(-MEAN-              **     52)
(-MEDIAN-            **     52)
(-SDEV-              **      4)
(-WEIGHT-            **      1)
(ARNOLD-MATTHEW    MA24     57)
(CARLYLE-THOMAS    TC32     54)
(DICKENS-CHARLES   CD51      *)
(DISRAELI-BENJAMIN BD79     46)
(GASKELL-ELIZABETH EG21     49)
(RUSKIN-JOHN       JR03     52)
)
(@t[;;;]Record of late assignments: 
()
@t[;;;]Record of penalty functions and due-dates:  @ @ @ @  @^
()
@t[;;;]List of actual names
NIL) @gratia
@End(TPSExample)


@Begin(Hand)
@GrComI(RESUME-INSERT-GRADES) allows you to resume entering grades.
@end(hand)	

@Begin(Hand)
@GrComI(MODIFY-GRADE) lets you enter or revise the grades on an old exercise.
This command will first prompt you for the names of the students whose
grades you want to modify. For each such student, you will be asked
for the exercises involved. The old grades will be printed and you
will be prompted for the revised grades.
@End(Hand)

@Exempli[
]
@Begin(TPSExample)
<Gr4>modify-grade

Please type names of the students whose grades need to be modified: 
dickens-charles gaskell-elizabeth
DICKENS-CHARLES
Please type the names of the grades that you want to change:
essay1
DICKENS-CHARLES's scores are: (ESSAY1 . *)
Please enter the new scores: score1 score2 ...:
53
GASKELL-ELIZABETH
Please type the names of the grades that you want to change:@^
essay1
GASKELL-ELIZABETH's scores are: (ESSAY1 . 49)
Please enter the new scores: score1 score2 ...: 
51	@Gratia
@End(TPSExample)

@Begin(Hand)
@GrComI(ALTER-GRADE) is a supplement to MODIFY-GRADE. 
It takes a list of grade names and prompts 
the user with the name of each student, allowing the grades in the list 
for each of those students to be changed. 
@End(Hand)

@begin(comment)
@Begin(Hand)
@GrComI(EXAM-GRADE) provides a convenient way to enter examination
grades. You will prompted for the name of the exam (with the prompt: @t(EXAM))
and the names of its problems (with the prompt @t(PROBS)), as well as
an output file. Then, for each student, you will be given a prompt with
the students name. You should respond with "Yes" (the default) if the student
took the exam. [Since @t(--MAX--) is treated as a student, you will need
to respond "Yes" to its prompt in order to enter the maximum scores for
the exam.] For each student that you claim took the exam, you will be prompted
for the score that student received on each problem. The totals are computed
and entered in the score file under the exam name given earlier. Also,
the output file, when run through @B(SCRIBE) and printed, will produce score
sheets (showing the score on each problem and the total grade on the exam)
for each student and a sheet showing the grades for the class.
@End(Hand)

@Exempli[
]
@Begin(TPSExample)
<Gr5>exam-grade
Exam ["Final"]>"Composition"
Probs [No Default]>Phrases Entire-Work
File [exgr.mss]>composition
--MAX-- [Yes]>
PHRASES [0]>10
ENTIRE-WORK [0]>10
ARNOLD-MATTHEW [Yes]>
PHRASES [0]>3
ENTIRE-WORK [0]>9
CARLYLE-THOMAS [Yes]>
PHRASES [0]>1
ENTIRE-WORK [0]>8
DICKENS-CHARLES [Yes]>
PHRASES [0]>3
ENTIRE-WORK [0]>9
DISRAELI-BENJAMIN [Yes]>
PHRASES [0]>9
ENTIRE-WORK [0]>2
GASKELL-ELIZABETH [Yes]>
PHRASES [0]>2
ENTIRE-WORK [0]>9
RUSKIN-JOHN [Yes]>
PHRASES [0]>3                     @^
ENTIRE-WORK [0]>7   @Gratia

@End(TPSExample)
@end(comment)

@Begin(Hand) 
@GrComI(LATE-EXERCISES) lets the instructor record the number of days
that a  manually entered exercise is late.
You will be prompted for the students, exercises and days of lateness.
This information may be used later by @GrCom(CALCULATE-GRADE).
@End(Hand)

@Exempli[
]
@Begin(TPSExample)
<Gr6>late-exercises

Please type the names of the students: dickens-charles

DICKENS-CHARLES
    Please enter the names of the exercises: essay1 @^

Please specify days late: 
ESSAY1: 3  @Gratia
@End(TPSExample)

@Section(Making the Output Convenient: ALIASES, COMMENT)@label(alias)

When you are printing the files from @GRADER, you may find
that complete exercise names take up too much space. Therefore, it
is convenient to refer to exercises by
one- or two- letter names, which we refer to as short names.
These, however, are not very mnemonic, so you may want to
keep a record of the actual names which the short names represent.

@Begin(Hand)
@GrComI(ALIASES) records the actual names corresponding
to the short names you have
already given the exercises.
@End(Hand)


@Exempli[
]

@Begin(TPSExample)
<Gr10>aliases
(ID ESSAY1)
Please type the (short) names of the exercises: essay1
Please specify the alias (the actual name): 
ESSAY1: bulgarian-atrocities

@End(TPSExample)

@Begin(Hand)
@GrComI(COMMENT)
lets you include some explanations or comments in
@GrVarI(grade-file) without stopping everything to use a file-editor.
This command will ask for your comments, which must
be enclosed in double-quotes, and append them to the end of
@GrVar(grade-file).
@End(Hand)

@Exempli[
]

@Begin(TPSExample)
<Gr1>comment

Please enter the comment (enclosed in double-quotes): 
"Charles missed TEST1 on account of illness."
@End(TPSExample)

Puts that comment in the @GrVar(grade-file).

@begin(COMMENT)
@Begin(TPSExample)
(
(NAME                ID ESSAY1)
(TYPE                **     HW)
(--MAX--             **     60)
(-MEAN-              **     53)
(-MEDIAN-            **     53)
(-SDEV-              **      4)
(-WEIGHT-            **      1)
(ARNOLD-MATTHEW    MA24     57)
(DICKENS-CHARLES   CD51     53)
(DISRAELI-BENJAMIN BD79     46)
(RUSKIN-JOHN       JR03     52)
(WILDE-OSCAR       OW36     59)
)
(@t{;;;}Record of late assignments: 
((DICKENS-CHARLES ESSAY1 3 ))                 @^
@t{;;;}Record of penalty functions and due-dates: 
@t{;;;}List of actual names
(ESSAY1 BULGARIAN-ATROCITIES))

We are not amused.  @Gratia
@End(TPSExample)
@end(COMMENT)



@Section(Automatic Grades: ETPS-GRADE, DUE-DATES)

@Hand[@GrComI(DUE-DATES) lets you assign due-dates before using the
@GrComI(ETPS-GRADE) command described below, so that information about
exercises completed late will automatically be recorded in the
@GrVar(grade-file). If due-dates have already been assigned, you can
change them with this command.  This command provides a convenient way
of keeping track of due-dates even if the @GrComI(ETPS-GRADE) command
will not be used.]
The date should be in the form 
@wt[@i{yy} @i{mm} @i{dd}], where @t[85 02 13] (for example)
means 1985 Feb 13.

@Begin(Hand)
@GrComI(ETPS-GRADE) collects grades from the record file, @GrVarI(etps-file),
generated by a teaching program.
@begin(comment) Following is totally wrong. Works only for ETPS done commands. DAN @end(comment)
@begin(comment)
 The data in this record file is expected
to be a column of parenthesized lists. Only lists whose second entry is
@t(DONE) are considered. For these lists, the first entry must be a
user-id, the third an exercise name and the penultimate a three-element list
indicating the date. (Other entries can encode any information you wish
to record; they will be ignored by @GrCom(ETPS-GRADE).)
The date-list is of the form @wt[@i{yy} @i{mm} @i{dd}],
e.g. @t[85 02 13] means 1985 Feb 13.
@end(comment)
The data in this record file is expected to be a column of parenthesized lists.
The format is as follows: the student's user-id is the first element of
the list; the exercise name is the second element; and the date completed
should be the fifth element of the list.  The date should be in the form of
a list such as @wt[(@i{yy} @i{mm} @i{dd})], e.g., @t[(85 02 13)]
means 1985 Feb 13.
See the chapter @i(Notes on setting things up) in the TPS User's Manual
for information about the creation of the @GrVarI(etps-file) to 
record grades for @GrCom(ETPS) exercises.
@End(Hand)

As mentioned in Chapter @ref(setup), user-id's are required for all
students in the class. If a student does not have one or it is not
known, the dummy id @t(****) may be specified.

@GrCom(ETPS-GRADE) will prompt you for exercises and the grades
given for completing them on time.
For each list in the @GrVar(etps-file) showing a student completing
one of these exercises, @GrCom(ETPS-GRADE) gives that student 
the grade corresponding to that exercise. 
When @GrCom(ETPS-GRADE) is called at a later date, it will leave
previously recorded scores alone and insert scores for newly completed
exercises. @Grader remembers the exercises which @GrCom(ETPS-GRADE)
has earlier processed, so only new exercises need to be entered when you are
prompted for exercises. (If there are no new exercises, just reply
with a return.)

If the assignment was late, the exercise and the number of days
late are recorded. If no due-date can be found for an exercise,
you will be prompted for it. 

@Exempli[If the the @GrVar(etps-file) contains the following lists:]
@Begin(LispCode)
(bd79 diplo1 done 1 (85 05 02))
(ma24 diplo1 done 2 (85 05 08))
(jr03 diplo1 done 3 (85 05 08))
@end(LispCode)
then running @GrCom(ETPS-GRADE) 

@Begin(TPSExample)
<Gr7>etps-grade
Please enter a list of exercises and scores. e.g. (x6200 15)(X6201 20) : 
(diplo1 40)

[merging ...]
Please specify the due-date for DIPLO1: 85 05 08
@End(TPSExample)

produces the new record:

@Begin(TPSExample)
NAME                ID ESSAY1 DIPLO1 
--MAX--            NIL     60     40 
ARNOLD-MATTHEW    MA24     57     40 
CARLYLE-THOMAS    TC32     54      * 
DICKENS-CHARLES   CD51     53      * 
DISRAELI-BENJAMIN BD79     46     40  @^
GASKELL-ELIZABETH EG21     51      * 
RUSKIN-JOHN       JR03     52     40 @Gratia
@End(TPSExample)

If aliases for exercises are being used, the actual names may appear
in the @grvar(etps-file), but you should refer to the exercises by
their short names when calling @GrCom(ETPS-GRADE). Thus, you should
first call @GrCom(DUE-DATES) using the short names to get these names
into the @grvar(grade-file), then call @GrCom(ALIASES) so @Grader will
know which actual names in the @grvar(etps-file) correspond to which
short names, then call @GrCom(ETPS-GRADE). Be sure that @t{ET} grades 
have this type.

@Exempli[Suppose we wish to update the grade file by entering the scores
the students have obtained on two exercises, x2109 and x2110, to which
we wish to give the aliases E3 and E4, respectively. We shall give each of
these exercises the due date 2006 October 27 (which we shall enter as
06 10 27), and we shall assign 9 points to E3 and 12 points to E4.
]
@Begin(TPSExample)
<Gr3>due-dates
ID A D C B G E I F H J T1 T1L E1 E2 K MID M L 
Please type the names of the exercises(exe1 exe2 ...): E3 E4   
Please specify the due-date: 
E3: 06 10 27
E4: 06 10 27

<Gr4>aliases

ID A D C B G E I F H J T1 T1L E1 E2 K MID M L E3 E4 
Please type the (short) names of the exercises: E3 E4
Please specify the alias (the actual name): 
E3: x2109
E4: x2110

<Gr5>etps-grade
Please enter a list of exercises and scores. eg. (x6200 15)(X6201 20)
Type <return> if you only want to update the previous scores
:(E3 9)(E4 12)

@End(TPSExample)



If you are using grader for several courses or sections of one course 
and you need to use @GrCom(ETPS-GRADE) for each of these,
exit @GRADER 
and start fresh for each section to avoid complications.


@Section(Updating the Class List: ADD-STUDENTS, @w(DELETE-STUDENT))

There are two commands for updating the class list in @GrVar(grade-file):

@Begin(Hand)
@GrComI(ADD-STUDENTS) prompts you for a list of student records.
Each student record is itself a parenthesized list where the first
element is the student's name, the second his user-id (if these are
used) and the subsequent elements are
his grades on the exercises to date, in chronological order. 
Be warned that grades past the latest exercise will be ignored. 
@GrCom(ADD-STUDENTS) will not introduce new exercises. 
If fewer grades are listed than exercises, the student will be  
given a grade of @t(*) on the later exercises.

@End(Hand)

@Exempli[
]
@Begin(TPSExample)
<Gr8>add-students

Please type a list of students and their grades, e.g., 
(student1 grade1 grade2)(student2 grade1 grade2) etc.:
(wilde-oscar ow36 59 40)                         @^

Inserting WILDE-OSCAR.	@Gratia

@End(TPSExample)

@Begin(Hand)
@GrComI(DELETE-STUDENT) prompts you for the names of students
to be taken off the list.
@End(Hand)

@Exempli[
]
@Begin(TPSExample)
<Gr9>delete-student

Please type the names of the  students: 
carlyle-thomas gaskell-elizabeth

Deleted CARLYLE-THOMAS.
Deleted GASKELL-ELIZABETH.
@End(TPSExample)

So now the @grvar(grade-file) contains:

@Begin(TPSExample)
(
(NAME                ID ESSAY1 DIPLO1)
(TYPE                **     HW     ET)
(--MAX--             **     60     40)
(-MEAN-              **     53     40)
(-MEDIAN-            **     53     40)
(-SDEV-              **      4      0)
(-WEIGHT-            **      1      1)
(ARNOLD-MATTHEW    MA24     57     40)
(DICKENS-CHARLES   CD51     53      *)
(DISRAELI-BENJAMIN BD79     46     40)
(RUSKIN-JOHN       JR03     52     40)
(WILDE-OSCAR       OW36     59     40)
)
(@t[;;;]Record of late assignments: 
((DICKENS-CHARLES ESSAY1 3 ))
@t[;;;]Record of penalty functions and due-dates:  @^
(DIPLO1 NONE (85 5 8))
@t[;;;]List of actual names
(ESSAY1 BULGARIAN-ATROCITIES))@Gratia
@End(TPSExample)

@GrComI(DELETE-STUDENT) puts a record of students who have been
removed from the class list at the end of the @grvar(grade-file).


@Section(Generated Values: STATISTICS)@Label(Stats)

@GRADER automatically generates statistical data when the record
is updated. The statistical measures it updates are indicated by
the value of @GrVarI(statistical-options).
The default value for this variable is (@t{-sdev-} @t{-median-} @t{-mean-}), 
which stand for standard deviation, median and mean, respectively.

@Begin(Hand)
@GrComI(STATISTICS) forces the calculation of the statistical measures.
Normally this is done automatically, so this command is rarely needed.
@begin(comment)
As an example, if you wish to compute the statistics for ESSAY1, as well
as other items that were previously changed (DIPLO1 in this example)
proceed as follows:
@end(comment)
@End(Hand)

@begin(comment)
@Begin(TPSExample)
<Gr2>push 'essay1 listofchanges

(ESSAY1 DIPLO1)

<Gr3>statistics
@End(TPSExample)

@GrCom(STATISTICS) then computes all of the measures in 
@GrVar(statistical-options) for @t{ESSAY1}, as well as @t{DIPLO1}. 
@end(comment)

@section(Displaying Information: DISPLAY, @w(NUMBER-OF-STUDENTS), @w(INFO-EXERCISES))

@Begin(Hand)
@GrComI(DISPLAY) will show the grades for one or all of the students 
on the desired exercises.
If all the grades for a student are requested, the late exercises and 
their number of days late will also be shown.
The mean, standard deviation or median for an exercise may be found
by responding with @t(-mean-, -sdev- ) or @t( -median- ) when
prompted for a student name.
@End(Hand)

@Begin(Hand)
@GrComI(NUMBER-OF-STUDENTS) gives you the size of the class.
@End(Hand)

@Begin(Hand)
@GrComI(INFO-EXERCISES) lists the due-dates, 
maximum scores, aliases,
weights and penalty-functions 
(see section @ref(penalty-fns))
for the desired exercises.
@End(Hand)

@Exempli[
]
@Begin(TPSExample)
<Gr4>infO-EXERCISES 

Please type the names of the exercises: DIPLO1
DIPLO1:
     Maximum Score:      40
     Weight:             1
     Mean:               40
     Median:             40
     Standard Dev:       0
     Penalty Function:   NO-PENALTY
     Due-Date:           (85 5 8)@Gratia
@End(TPSExample)
