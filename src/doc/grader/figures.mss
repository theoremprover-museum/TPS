@ChapBTPS(Figuring Grades)@label(calc)

When you are ready to assign course grades, you should first make sure
that the @grvar(grade-file) is up to date. You might need to use 
@GrCom(ETPS-GRADES) and @GrCom(LATE-EXERCISES) once more. Then use
@GrCom(CHANGE-WEIGHT), @GrCom(PENALTY-FNS), and
@GrCom(CHG-VARS) to set the variables appropriately. Then call
@GrCom(CALCULATE-GRADE) to get the score on each item you wish to
consider for each student. (An item can be a weighted total of various
scores on exercises.) Then use @GrCom(SORT-FN) to get nice
displays of these scores. This should facilitate deciding which item
you wish to use as the basis for assigning grades, and where the
cut-offs between various grades (such as C+ and B-) should be. 
Then use @GrCom(LETTER-GRADE) to record the letter grade for each 
student. (If you leave @Grader to examine the displays, you will
probably have to run @GrCom(CALCULATE-GRADE) again before using
@GrCom(LETTER-GRADE), since the scores computed by
@GrCom(CALCULATE-GRADE) are considered temporary and are not recorded
in @grvar(grade-file).)

@Section(Totaling: CHANGE-WEIGHT, CALCULATE-GRADE, PENALTY-FNS)
@label(penalty-fns)
@begin(Hand)
@GrComI(CHANGE-WEIGHT) allows you to examine and change the weights
associated with exercises. The default weight is 1.
@end(Hand)

@Begin(Hand)
@GrComI(CALCULATE-GRADE) computes the totals for
each student in the class and prints the results in 
@GrVarI(totals-grade-file). All numbers calculated by @GRADER
are printed to values rounded @GrVarI(print-n-digits) places past the decimal.
Thus, when @GrVar(print-n-digits) is @t(2), the value
@t(64.98715) is printed @t(64.99).

@End(Hand)

For each student, @GrCom(CALCULATE-GRADE)

@begin(enumerate)
Sets aside the exercises with type @t(LE). Their scores, being
letter grades, are only symbols to @GrCom(CALCULATE-GRADE).

Reduces the effective score for each exercise submitted late by
applying the penalty function assigned to that exercise. (See below.)

Drops the lowest effective score of each exercise-type which
is an element of the list @GrVarI(drop-min).

Multiplies the effective score of each remaining exercise by the weight of the
exercise,
 and sums these 
weighted scores for each exercise type. These sums for exercise types
are recorded in @GrVarI(totals-grade-file) along with the items
mentioned below.

Adds these sums for exercise types together, giving the total grade.

Computes the percentage score for this total grade, if 
@GrVarI(cal-percentage) is not @t(NIL).

Computes the value of each item defined in @GrVar(new-item).
@end(enumerate)

The elements of @GrVarI(new-item) are lists such as 
(SCORE1 HW 1.8 ET 0.5),
in which the first element is the name of the item and subsequent pairs
of elements are each composed of the name of an exercise-type and the weight
to be given that type. The value of the item is found by adding
together the sums for the exercise-types multiplied by 
the corresponding weights.

@Exempli{Items should be added using @GrCom(CHG-VARS):}
@begin(TPSExample)
<Gr5>chg-vars
The current value of some of the variables is:
1   Course-Name             "victoria"
2   Grade-File              "victoria.grades"
3   Totals-Grade-File       "victoria.totals"
4   Old-Grade-File          "victoria.xgrades"
5   Old-Totals-Grade-File   "victoria.xtotals"
6   Letter-grade-file       "victoria.letter"
7   ETPS-file               "etps.rec"
8   Statistical-options     (-MEAN- -MEDIAN- -SDEV-)
9   print-n-digits          0
10  cal-percentage          NIL
11  letter-grade-flag       T
12  new-item                NIL
13  drop-min                NIL
14  due-date-flag           T
15  default-penalty-fn      NO-PENALTY

Please type one of the numbers in the first column, or 16 to look at the
variables, or 17 to exit[Default:17]:12
Please type the new item(s): (home-study hw 1.5)
Do you want to change another variable,or look at their values? [No]>

<Gr6>calculate-grade

The old file victoria.totals has been renamed to 
victoria.xtotals
Creating new copy of file victoria.totals
@End(TPSExample)

This creates the following table in the @GrVar(totals-grade-file):

@Begin(TPSExample)

NAME              HOME-STUDY TOTAL ET HW
-SDEV-                     7    17 16  4
--MAX--                   90   100 40 60
-MEAN-                    80    85 32 53
-MEDIAN-                  80    92 40 53
ARNOLD-MATTHEW            86    97 40 57
DICKENS-CHARLES           80    53  0 53
DISRAELI-BENJAMIN         69    86 40 46
RUSKIN-JOHN               78    92 40 52
WILDE-OSCAR               88    99 40 59     @^

HOME-STUDY:
   HW 1.5@Gratia
@End(TPSExample)

@Begin(Hand)
@GrComI(PENALTY-FNS) assigns the penalty-functions
used by @GrCom(CALCULATE-GRADE). These assignments are then recorded
in the grade-file.
The default penalty-function is @t(NO-PENALTY). 
This function does not deduct any points for late exercises.  
The user can specify a different default penalty-function, by setting
the @GrVarI(default-penalty-fn) to the name of the default function of
his choice.
The functions @T(PENALTY),
@t(PENALTY-0) and @t(PENALTY%10) are also available. 
@t(PENALTY) deducts one point for each day that the exercise was late. 
@t(PENALTY-0) assigns 
0 points for each late exercise. @t(PENALTY%10) deducts @t(10%) for each day 
late. 
@End(Hand)

With the default penalty-function, @GrCom(CALCULATE-GRADE) produced
the above table in @GrVar(totals-grade-file).

However, if we execute @GrCom(PENALTY-FNS) as follows:
@Begin(TPSExample)
<Gr7>penALTY-FNS 
ID ESSAY1 DIPLO1 
Please type the names of the exercises: essay1 
Please specify the penalty-fn: 
ESSAY1: penalty%10
@End(TPSExample)

and now have the grade-file:

@Begin(TPSExample)
(
(NAME                ID ESSAY1 DIPLO1)
(TYPE                **     HW     ET)
(--MAX--             **     60     40)
(-MEAN-               *     53     40)
(-MEDIAN-             *     53     40)
(-SDEV-               *      4      0)
(-WEIGHT-             *      1      1)
(ARNOLD-MATTHEW    MA24     57     40)
(DICKENS-CHARLES   CD51     53      *)
(DISRAELI-BENJAMIN BD79     46     40)
(RUSKIN-JOHN       JR03     52     40)
(WILDE-OSCAR       OW36     59     40)
)
(;;;Record of late assignments: 
((DICKENS-CHARLES ESSAY1 3 ))
;;;Record of penalty functions and due-dates: 
(ESSAY1 PENALTY%10 NONE
DIPLO1 NONE (85 5 8))	
;;;List of actual names
(ESSAY1 BULGARIAN-ATROCITIES))

@End(TPSExample)

then @GrCom(CALCULATE-GRADE) gives:

@Begin(TPSExample)
NAME              HOME-STUDY TOTAL ET HW
-SDEV-                    13    24 16  9

--MAX--                   90   100 40 60
-MEAN-                    75    82 32 50
-MEDIAN-                  78    92 40 52
ARNOLD-MATTHEW            86    97 40 57
DICKENS-CHARLES           52    35  0 35
DISRAELI-BENJAMIN         69    86 40 46
RUSKIN-JOHN               78    92 40 52
WILDE-OSCAR               89    99 40 59 @^

HOME-STUDY:
   HW 1.5@Gratia
@End(TPSExample)

The user may assign penalties using his own @LISP functions,
defined in his @t(GRADER.INI) file.
These functions should expect to receive three arguments
@i(score, ) @i(days-late, ) and @i(max-score, ) and must return the
penalized score. The function @t(penalty%, ) a generalization
of @t(PENALTY%10), is provided for this purpose.

@Exempli[
]
@begin(LispCode)
(defun penalty%5 (score dayslate max-score)
  (penalty% score dayslate max-score 5))
@end(LispCode)
will deduct 5% for each day late. @ @ @ @ @ @ @ @! @ @^@/@Gratia

@Section(Sorting: SORT-FN)
@Begin(Hand)
@GrComI(SORT-FN) will sort grades alphanumerically by the desired field.
These fields either come with @GRADER or are created by 
@GrCom(CALCULATE-GRADE).
The sorted grades are appended to the @GrVarI(totals-grade-file).
The first table of grades in @GrVar(totals-grade-file) are already
sorted by student name.
@End(Hand)

Before appending the sorted grades, @GrCom(SORT-FN) will ask you
which fields to display in addition to the student-name and the sorting fields.
It will also prompt you for the style of the display, either
Simple or Incremental.

In the Simple Display, the values in the fields are printed by descending 
order of the sorting field.

For the Incremental Display, you will be asked for an increment.
The resulting display is partitioned by multiples of that increment.

@Exempli[
]
@Begin(TPSExample)
<Gr8>sort-fn
HW ET TOTAL HOME-STUDY NAME ESSAY1 DIPLO1 
Please choose the field: total
Do you want a display with increments? [Yes]>
ID ESSAY1 DIPLO1 HOME-STUDY TOTAL ET HW 
Please select the items to be displayed (in addition to TOTAL)
[Default: NONE]: home-study
The maximum and minimum scores on TOTAL are 100 and 35 respectively.
Please specify an increment for the display[Default:5]: 10
[Updating victoria.totals]

[Done.]
Do you want to sort the grades using another field? [No]>y
HW ET TOTAL HOME-STUDY NAME ESSAY1 DIPLO1 
Please choose the field: essay1
ID ESSAY1 DIPLO1 HOME-STUDY TOTAL ET HW 
Please select the items to be displayed (in addition to ESSAY1)
[Default: NONE]: diplo1
The maximum and minimum scores on ESSAY1 are 60 and 46 respectively.
Please specify an increment for the display[Default:5]: 
[Updating victoria.totals]

[Done.]
Do you want to sort the grades using another field? [No]>

@End(TPSExample)

The @grvar(totals-grade-file) would now contain the following tables:

@Begin(TPSExample)
NAME              HOME-STUDY TOTAL ET HW
-SDEV-                    13    24 16  9

--MAX--                   90   100 40 60
-MEAN-                    75    82 32 50
-MEDIAN-                  78    92 40 52
ARNOLD-MATTHEW            86    97 40 57
DICKENS-CHARLES           52    35  0 35
DISRAELI-BENJAMIN         69    86 40 46
RUSKIN-JOHN               78    92 40 52
WILDE-OSCAR               88    99 40 59

HOME-STUDY:
   HW 1.5

Grades sorted on the basis of TOTAL
     NAME              TOTAL HOME-STUDY
     


100  --MAX--             100         90
     WILDE-OSCAR          99         88
     ARNOLD-MATTHEW       97         86
     -MEDIAN-             92         78
     RUSKIN-JOHN          92         78
90   DISRAELI-BENJAMIN    86         69
     -MEAN-               82         75
80
70
60
50
40   DICKENS-CHARLES      35         52

Grades sorted on the basis of ESSAY1
     NAME              ESSAY1 DIPLO1
     


60   --MAX--               60     40
     WILDE-OSCAR           59     40
     ARNOLD-MATTHEW        57     40
55   -MEDIAN-              53     40
     -MEAN-                53     40
     DICKENS-CHARLES       52      *
     RUSKIN-JOHN           52     40 @ @ @ @ @^
50   DISRAELI-BENJAMIN     46     40 @Gratia
@End(TPSExample)

@Section(Letter Grades: LETTER-GRADE)
 
@Begin(Hand)
@GrComI(LETTER-GRADE) assigns letter grades for a field
with a numerical grade. A field can be an exercise,
a type, a total or an item in @GrVar(new-item).
These grades will be recorded in
@GrVarI(letter-grade-file), if the value of @GrVarI(letter-grade-flag) is
not @t(nil). They are also included in either @GrVar(grade-file) or
@GrVar(totals-grade-file). 
@End(Hand)

@GrCom(LETTER-GRADE) will prompt you for the name of the new letter grade,
the choice of @GrVar(grade-file) or @GrVar(totals-grade-file) and
the field to which the letter grade is to be assigned.

Finally, you are prompted
for a list of letter grades and the corresponding  minimum scores.
These grades need not be in numerical order. 
If a score below all the minimum scores is encountered, a warning
message will be printed and the grade @t(*) assigned.

@Exempli[
]
@Begin(TPSExample)
<Gr9>letter-grade
Please type the name for the new letter grade: midsem
Please type 1 if you want to add a letter grade to victoria.grades,
       type 2 if you want to add a letter grade to victoria.totals,
       type 3 if you want to add a letter grade to both: 2
ESSAY1 DIPLO1 HOME-STUDY TOTAL ET HW 
Please choose the field: total
Please type a list of letter grades and the corresponding minimum scores,
e.g., (a 90)(b+ 80)(a- 85)(b 75)...(r 0):
(c 70)(a 90)(b 80)(r 40)
Unable to decide grade for DICKENS-CHARLES. Assigned *. 
Creating file victoria.letter.
Updating file victoria.totals
DONE

@End(TPSExample)

Producing in @GrVar(letter-grade-file):

@Begin(TPSExample)
ARNOLD-MATTHEW         A  
DICKENS-CHARLES        *
DISRAELI-BENJAMIN      B
RUSKIN-JOHN            A
WILDE-OSCAR            A  
@End(TPSExample)

It also appends the following table at the end of @grvar(totals-grade-file):

@Begin(TPSExample)
NAME              HOME-STUDY TOTAL ET HW MIDSEM @^
-SDEV-                    13    24 16  9      *
--MAX--                   90   100 40 60      A
-MEAN-                    75    82 32 50      B
-MEDIAN-                  78    92 40 52      A
ARNOLD-MATTHEW            86    97 40 57      A
DICKENS-CHARLES           52    35  0 35      *
DISRAELI-BENJAMIN         69    86 40 46      B
RUSKIN-JOHN               78    92 40 52      A
WILDE-OSCAR               88    99 40 59      A

TOTAL:
((A 90) (B 80) (C 70) (R 40))@gratia
@End(TPSExample)
