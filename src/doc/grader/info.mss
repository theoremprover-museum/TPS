@ChapBTPS(Obtaining Information)

@Section(On Certain Items: NUMBER-OF-STUDENTS, INFO-EXERCISES)

@Begin(Hand)
@GrComI(NUMBER-OF-STUDENTS) gives you the size of the class.
@End(Hand)

@Begin(Hand)
@GrComI(INFO-EXERCISES) lists the due-dates, penalty-functions,
maximum scores and aliases for the desired exercises.
@End(Hand)

@Exempli[
]
@Begin(TPSExample)
<Gr5>info-exercises

Please type the names of the exercises: diplo1

DIPLO1:
   Maximum Score:    40
   Mean:             40
   Median:           40
   Standard Dev:     0
   Penalty Function: PENALTY%10   @^
   Due-Date: (85 5 8)	@Gratia
@End(TPSExample)

@Section(Class Records: DISPLAY)

@Begin(Hand)
@GrComI(DISPLAY) will show the grades for one or all of the students 
on the desired exercises.
If all the grades for a student are requested, the late exercises and 
their number of days late will also be shown.
The mean, standard deviation or median for an exercise may be found
by responding with @t(-mean-, -sdev- ) or @t( -median- ) when
prompted for a student name.
@End(Hand)
