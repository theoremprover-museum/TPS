;;; -*- Mode:LISP; Package:TEACHER -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
;;; ;;; File Grades2 ;;;

(in-package :teacher)
(part-of grader)

(deffile grades2
  (part-of grader)
  (extension lisp)
  (mhelp "Creates the grading package top-level."))

(context gr-c-gradefile)

(defgexpr create-gradefile
  (mhelp "Create a new grade file."))

(defun create-gradefile ()
  (declare (special studentlist actual-name-list distinct-types totalslist namelist))
  (let ((list1 nil)
	(defaultf
	  (merge-pathnames grade-dir (make-pathname% :name "student" :type "lst")))
	(donefile nil))
    (msg F
	 "Please enter the name of file containing list of students"
	 T "(default: " (defaultf . filespec) "): ")
    (let ((filespec (car (linereadp))))
      (setq filespec (if filespec
			 (merge-pathnames grade-dir 
			   (parse-namestring
			     (string-downcase (string filespec)))
			   defaultf)
			 defaultf))
      (do ((filespec (merge-pathnames grade-dir filespec))) 
	  ((probe-file filespec) (setq donefile filespec))
	(msg F (filespec . filespec) " doesn't exist. Please try again: ")
	(setq filespec (parse-namestring
			 (string-downcase (string (car (linereadp))))))
	(setq filespec (if filespec (merge-pathnames filespec defaultf)
			   defaultf))))
    (with-open-file (donefile donefile :direction :input)
      (setq list1 (read donefile nil)))
    (setq studentlist (list '--MAX--)
	  listOfChanges nil
	  actual-name-list nil
	  distinct-types nil
	  totalslist nil)
    (cond ((atom list1)
	   (msg t  (donefile . filespec) " doesn't have the proper format.")
	   (throwfail nil))
	  ((atom (car list1))
	   (putprop '--max-- '--max-- 'name)
	   (dolist (student list1)
	     (when (listp student)
	       (msg t "Error while reading " (donefile . filespec) "."
		    t "Expecting name, found list or ':" student)
	       (throwfail nil))
	     (putprop student student 'name)
	     (push student studentlist))
	   (setq namelist '(name))
	   (putprop 'type 'type 'name))
	  (T (putprop '--max-- '--max-- 'name)
	     (putprop '--max-- '** 'id)
	     (dolist (xxx list1)
	       (let ((student (car xxx)))
		 (putprop student student 'name)
		 (putprop student (cadr xxx) 'id)
		 (push student studentlist)))
	     (setq namelist '(name id))
	     (putprop 'type '** 'id)
	     (putprop 'type 'type 'name)))
    (setq gradefileflag T)
    (setq Statistical-options '(-mean- -median- -sdev-))
    (print-grades nil)))

(context gr-d-manual-grades)

(defgexpr resume-insert-grades
  (mhelp  "Resume entering grades from a previously interrupted session."))

(defun resume-insert-grades ()
  (declare (special temporary-file))
  (if (probe-file temporary-file)
      (insert-grades-main3)
      (msg "Unable to resume entering as file "
	   (temporary-file . filespec) T "can not be located.")))

(defgexpr insert-grades
  (mhelp  "Insert one or more grades in the grade file."))
	    
(defun insert-grades ()
  (declare (special namelist studentlist))
  (msg F
       "Please enter the names of the exercises, e.g.,HW1 EXAM1 LETTER1: " T)
  (let* ((listofexercises (get-exename))
	 (N (length listofexercises))
	 type)
    (msg F (l listofexercises) T "Please enter " N " item" (if (> N 1) "s" "")
	 " specifying the type of each exercise: " T)
    (setq type (linereadp2 N))
    (let ((new-types nil))
      (do ((type1 (car type) (car type))
	   (exe-name (car listOfExercises) (car listOfExercises))
	   (listOfExercises (cdr listOfExercises) (cdr listOfExercises))
	   (type (cdr type) (cdr type)))
	  ((null type1)) 
	(if (or (member type1 namelist) (not (symbolp type1))
		(member type1 '(t nil)))
	    (do ((xxx type1 (car (linereadp))))
		((and (not (member xxx namelist)) (symbolp xxx)
		      (not (member xxx '(t nil))))
		 (push xxx new-types))
	      (msg F xxx)
	      (cond ((member xxx '(T nil))
		     (msg " is a special LISP symbol."))
		    ((symbolp xxx)
		     (msg " is being used as an exercise name."))
		    (T (msg " is not a symbol.")))
	      (msg T "Please specify a different type: "))
	    (push type1 new-types))
	(when (eq type1 'Le)
	  (putprop '-mean- '* exe-name)
	  (putprop '-sdev- '* exe-name)
	  (putprop '-median- '* exe-name)))
      (setq type (nreverse new-types)))
    (insert-grades-main1 listofexercises type T)
    (insert-grades-main2 listofexercises
			 (sort (copy studentlist) #'alphalessp) N))
    T)

(defun insert-grades-main1 (listofexercises type flag)
  (declare (special temporary-file namelist distinct-types))
  (if flag
      (let ((reroute-close-message nil))	 
	(reroute-output temporary-file *default-pathname-defaults*
	  (msg ";;;" (status-userid) " saving into file "
	       (temporary-file . filespec) T ";;;on ")
	  (stringdt)
	  (msg T listofexercises T type T))))
  (setq namelist (append namelist listofexercises))
  (setq listOfChanges (append listOfChanges listofexercises))
  (do ((exe-name (car listofexercises) (car listofexercises))
       (type1 (car type) (car type))
       (type (cdr type) (cdr type))
       (listofexercises (cdr listofexercises) (cdr listofexercises)))
      ((null exe-name))
    (unless (member type1 distinct-types)
      (putprop type1 1 'weight)
      (push type1 distinct-types))
    (putprop exe-name type1 'type)
    (putprop 'type type1 exe-name)
    (putprop '-weight-  1 exe-name)
    (push exe-name (get type1 'exe-name))
    (putprop exe-name 1 'weight)))

(defun insert-grades-main2 (listofexercises studentlist1 N)
  (declare (special temporary-file))
  (dolist (student studentlist1)
    (msg F student " : ")
    (let ((newscores (linereadp2 N)))
      (let ((reroute-close-message nil))	 
	(reroute-output-append temporary-file *default-pathname-defaults*
	  (princ (cons student newscores))
	  (msg T)))
      (do ((exe-name (car listofexercises) (car listofexercises))
	   (score (car newscores) (car newscores))
	   (newscores (cdr newscores) (cdr newscores))
	   (listofexercises (cdr listofexercises) (cdr listofexercises)))
	  ((null exe-name))
	(putprop student score exe-name))))
  (print-grades T)
  (delete-file temporary-file)
  ;;(initialize-exercise-ctree)
  )

(defun insert-grades-main3 ()
  (declare (special temporary-file studentlist))
  (with-open-file (donefile temporary-file :direction :input)
    (let ((listofexercises (read donefile nil))
	  (type (read donefile nil))
	  (studentlist1 (sort (copy studentlist) #'alphalessp))
	  (N 0))
      (setq N (length listofexercises))
      (insert-grades-main1 listofexercises type nil)
      (do ((xxx (read donefile nil eof-value) (read donefile nil eof-value))
	   (student (car studentlist1) (car studentlist1)))
	  ((equal xxx eof-value))
	(setq studentlist1 (cdr studentlist1))
	(if (equal (car xxx) student)
	    (do ((newscores (cdr xxx) (cdr newscores))
		 (listofexercises listofexercises (cdr listofexercises)))
		((null listofexercises))
	      (putprop student (car newscores) (car listofexercises)))))
      (insert-grades-main2 listofexercises studentlist1 N))))

(defgexpr alter-grade
  (mhelp "Change the existing grades of some students."))

(defun alter-grade ()
  (declare (special namelist studentlist))
  (msg F 
       "Please type the names of the grades that you want to change:" T)
  (let ((gradelist (linereadp-e))
	(newgradelist nil))
    (dolist (grade gradelist
		   (setq newgradelist (nreverse newgradelist)))
      (cond ((member grade namelist)
	     (push grade listofchanges)
	     (push grade newgradelist))
	    (t (msg F "Unable to locate " grade "."))))    
    (dolist (student studentlist)
     (when newgradelist
      (msg F student "'s scores old are: ")
      (dolist (exe-name newgradelist)
	(princ (cons exe-name (get student exe-name))))
      (msg T
	   "Please enter the new scores, or press <cr> to leave unchanged: score1 score2 ...:" T)
      (do ((newscore (linereadp2 (length newgradelist) (mapcar #'(lambda (x) (get student x))
							       newgradelist))
		     (cdr newscore))
	   (newgradelist newgradelist (cdr newgradelist)))
	  ((null newgradelist))
	(putprop student (car newscore) (car newgradelist))))))
  (print-grades T))

(defgexpr modify-grade
  (mhelp "Change the existing grades of some students."))

(defun modify-grade ()
  (declare (special studentlist))
  (msg F
       "Please type names of the students whose grades need to be modified: "
       T)
  (let ((listOfstudents (linereadp-s)))
    (dolist (student listOfStudents)
      (if (member student studentlist)
	  (modify-grade1 student)
	  (msg F "Unable to locate " student ".")))
    (print-grades T))
  nil)

(defun modify-grade1 (student)
  (declare (special namelist))
  (msg F student T
       "Please type the names of the grades that you want to change:" T)
  (let ((gradelist (linereadp-e))
	(newgradelist nil))
    (dolist (grade gradelist
		   (setq newgradelist (nreverse newgradelist)))
      (cond ((member grade namelist)
	     (push grade listofchanges)
	     (push grade newgradelist))
	    (t (msg F "Unable to locate " grade "."))))
    (when newgradelist
      (msg F student "'s scores are: ")
      (dolist (exe-name newgradelist)
	(princ (cons exe-name (get student exe-name))))
      (msg T
	   "Please enter the new scores: score1 score2 ...:" T)
      (do ((newscore (linereadp2 (length newgradelist)) (cdr newscore))
	   (newgradelist newgradelist (cdr newgradelist)))
	  ((null newgradelist))
	(putprop student (car newscore) (car newgradelist))))))

(defgexpr late-exercises
  (mhelp "Use this command to keep track of students who submit late
	 assignments."))

(defun late-exercises ()
  (declare (special studentlist namelist))
  (msg T "Please type the names of the students: ")
  (let ((list-of-students (linereadp-s))
	(print-flag t))
    (dolist (student list-of-students (if print-flag (print-grades T)))
      (msg T student)
      (cond ((member student studentlist)
	     (msg T "   Please enter the names of the exercises: ")
	     (let ((exe-list (linereadp-e))
		   (msg-flag T))
	       (dolist (exe-name exe-list)
		 (when (and msg-flag (member exe-name (cdr namelist)))
		   (msg T "Please specify days late: "))
		 (msg T exe-name)
		 (cond ((member exe-name (cdr namelist))
			(setq msg-flag nil)
			(msg ": ")
			(let ((dayslate (read1))
			      (old (cadr
				    (member exe-name
					    (get student 'late-exe)))))
			  (cond (old
				 (msg F
				      "WARNING: There's already a record which shows that this exercise was"
				      T "submitted "  old " days late.")
				 (when (query
					"Do you want to use the new value"
					T)
				   (putprop student
					    (replace1
					     (get student 'late-exe)
					     exe-name dayslate)
					    'late-exe)
				   (setq print-flag T)))
				(T (setq print-flag T)
				   (push dayslate (get student 'late-exe))
				   (push exe-name (get student 'late-exe))))))
		       (t (msg " doesn't exist. No action taken."))))))
	    (T (msg "   Unable to locate this student."))))))

(context gr-e-automatic-grades)

(defgexpr etps-grade
  (mhelp "Copy grades from ETPS record file to GRADE FILE."))

(defun etps-grade ()
  (declare (special userid dproof date distinct-types studentlist namelist))
  (setq msg-template (or (get 'done-exc 'template-names) msg-template))
  (msg F
    "Please enter a list of exercises and scores. eg. (x6200 15)(X6201 20)"
    T "Type <return> if you only want to update the previous scores" T ":")
  (let ((exerciselist (car (linereadp1))))
    (if (atom (car exerciselist)) (setq exerciselist (list exerciselist)))
    (with-open-file (donefile ETPS-FILE :direction :input)
      (do ((record (read donefile nil eof-value)
		   (read donefile nil eof-value)))
	  ((equal record eof-value))
	(progv msg-template record
	       (let ((id  (if (stringp userid)
			      (intern userid)
			      (intern (string-upcase (symbol-name
					       userid))))))
		 (unless (assoc id (get dproof 'etps-record))
		   (push (cons id date) (get dproof 'etps-record)))))))
    (msg T "[MERGING ...")
    (cond ((member 'ET distinct-types)
	   (let ((old-etps (get 'et 'exe-name)))
	     (setq listofchanges (append listofchanges old-etps))
	     (dolist (exe-name old-etps)
	       (let ((actual-name (get exe-name 'real-name exe-name))
		     (sco (get '--max-- exe-name))
		     (due-date (get  exe-name 'due-date '(99 12 31)))
		     dayslate)
		 (dolist (student studentlist)
		   (when (eq (get student exe-name) '*)
		     (let ((date (cdr (assoc (get student 'id)
					     (get actual-name
						  'etps-record)))))
		       (when date
			 (setq dayslate (days-late due-date date))
			 (when (plusp dayslate)
			   (push dayslate (get student 'late-exe))
			   (push exe-name (get student 'late-exe)))
			 (putprop student sco exe-name)))))))))
	  (t (push 'et distinct-types)
	     (putprop 'et nil 'exe-name)
	     (putprop 'et 1 'weight)))
    (do ((excno (caar exerciselist) (caar exerciselist))
	 (sco (cadar exerciselist) (cadar exerciselist))
	 (exerciselist (cdr exerciselist) (cdr exerciselist)))
	((null excno))
      (cond ((or (null (get 'type excno))(eq (get 'type excno) '*))	     
	     (push excno listofchanges)
	     (if (not (member excno namelist))
		 (setq namelist (nconc namelist (list excno))))
	     (putprop excno 'et 'type)
	     (push excno (get 'et 'exe-name))
	     (putprop 'type 'et excno)
	     (putprop '-weight- 1 excno)
	     (putprop excno 1 'weight)
	     (let ((due-date (get excno 'due-date))
		   (actual-name (get excno 'real-name excno))
		   dayslate)
	       (unless due-date
		 (cond (due-date-flag
			 (msg T "Please specify the due-date for ")
			 (setq due-date (duedate-main excno)))
		       (T (setq due-date '(99 12 31)))))
	       (dolist (student studentlist)
		 (let ((date (cdr (assoc (get student 'id)
					 (get actual-name 'etps-record)))))
		   (if date
		       (progn
			(setq dayslate (days-late due-date date))
			(when (plusp dayslate)
			  (push dayslate (get student 'late-exe))
			  (push excno (get student 'late-exe)))
			(putprop student sco excno))
		       (putprop student '* excno)))))
	     (putprop '--max-- sco excno))
	    (T (msg F excno " already exists. No action taken!"))))
    (print-grades T)
    (msg T 10 " ... DONE]")))


(defgexpr due-dates
  (mhelp "Assign due-dates to exercises."))

(defun due-dates ()
  (declare (special namelist))
  (msg F (l (cdr namelist)) T
       "Please type the names of the exercises(exe1 exe2 ...): ")
  (let ((exe-list (linereadp-e)))
    (msg T "Please specify the due-date: ")
    (dolist (exe-name exe-list)
      (unless (member exe-name (cdr namelist))
	(do ((xxx exe-name (car (linereadp-e))))
	    ((and (symbolp xxx) (not (member xxx '(t nil))))
	     (unless (member exe-name (cdr namelist))
	       (putprop exe-name exe-name 'exe-name)
	       (setq namelist (append namelist (list exe-name)))))
	  (msg T xxx
	       (if (symbolp xxx)
		   " is a special LISP symbol."  " is not a symbol.")
	       T "Please specify a different name: ")))
      (duedate-main exe-name))
    ;;(if new-exe (initialize-exercise-ctree))
    )
  (print-grades T))

(defun duedate-main (exe-name)
  (msg F exe-name ": ")
  (do ((xxx (linereadp) (linereadp)))
      ((and (= (length xxx) 3)
	    (dolist (elt xxx t)
	      (if (and (integerp elt) (plusp elt)) t
		  (return nil)))
	    (< (cadr xxx) 13)
	    (< (caddr xxx) 32))
       (putprop exe-name xxx 'due-date))
    (msg T xxx " is not in the format: yr mm da" T
	 "Please try again: ")))

(context gr-f-class-list)

(defgexpr add-students
  (mhelp "Insert students in the grade file."))

(defun add-students ()
  (declare (special namelist studentlist))
  (msg F "Please type a list of students and their grades, e.g., " T
       "(student1 grade1 grade2)(student2 grade1 grade2) etc.:" T)
  (let ((ListOfStudents (linereadp-s))
	(N (length namelist)))
    (when (and listofstudents (atom (car listofstudents)))
      (setq listofstudents (list listOfstudents)))
    (dolist (student listOfStudents)
      (let ((student-name (car student)))
	(if (member student-name studentlist)
	    (msg F student-name " already exists. Not inserted.")
	    (progn
	      (cond ((< (length student) N)
		     (msg F 
			  "The following student record requires more items, adding '*' at the end: "
			  T student))
		    ((> (length student) N)
		     (msg F
			  "The following student record has additional items, ignoring from the
   end: " T student)))
	      (msg T "Inserting  " student-name ".")
	      (push student-name studentlist)
	      (do ((namelist namelist (cdr namelist))
		   (student student (cdr student)))
		  ((null namelist))
		(putprop student-name (or (car student) '*) (car namelist)))
	      ))))
    (setq listofchanges (cdr namelist))
    (print-grades T)
    ;;(initialize-student-ctree)
    )
  nil)

(defgexpr delete-student
  (mhelp "Delete some students from the grade file."))

(defun delete-student ()
  (declare (special namelist studentlist))
  (msg F "Please type the names of the  students: " T)
  (let ((listOfStudents (linereadp-s)) (deleted-list nil))	
    (dolist (name listofstudents)
      (cond ((memq name studentlist)
	     (setq studentlist (delete name studentlist))
	     (push name deleted-list)
	     (msg T "Deleted "))
	    (t (msg T "Unable to locate ")))      
      (msg name "."))
    (when deleted-list
      (let ((reroute-close-message nil))
	(reroute-output-append grade-file *default-pathname-defaults*
	  (msg T "DELETED STUDENTS: ")
	  (stringdt)
	  (msg T (l namelist) T)
	  (dolist (student deleted-list)
	    (dolist (item namelist)
	      (msg (get student item) 1))
	    (msg T))))
      (setq listofchanges (cdr namelist))
      ;;(initialize-student-ctree)
      (print-grades T))))

(context gr-g-output)

(defgexpr change-sequence
  (mhelp "change the sequence of assignments"))

(defun change-sequence ()
  (declare (special namelist))
  (let ((input_newseq nil)
	(copylist namelist)
	(accepted nil))
  (prompt-read input_newseq nil (msgf "current sequence is:" (cdr namelist) t "please enter new sequence:")
	       'symbollist (cdr namelist) ((? (msgf "A permutation of the given list of exercises."))
					   (?? (mhelp 'symbollist))))
  (setq input_newseq (cons 'name input_newseq))
  (setq accepted 
	(dolist (elem_inputlist input_newseq 
				(if copylist
				    (query "New sequence is not a permutation of old list, still continue?" nil)
				  t))
		(if (not (find elem_inputlist copylist))
		    (unless 
		     (query "New sequence is not a permutation of old list, still continue?" nil)
		     (return nil))
		  (setq copylist (remove-one elem_inputlist copylist))))
	)
  (when accepted (setq namelist input_newseq) (print-grades nil))))

(defun remove-one (elem l)
  (if (eq elem (car l)) 
      (cdr l)
    (cons (car l) (remove-one elem (cdr l)))))

(defgexpr aliases
  (mainfns aliases1)
  (mhelp "Assign actual names to exercises. The teacher may use
	 short names for the assignments (to obtain a display which
	 can fit on paper), and use this function to keep track of
	 their actual names."))

(defun aliases1 ()
  (declare (special namelist actual-name-list))
  (msg F (l (cdr namelist)) T
       "Please type the (short) names of the exercises: ")
  (let ((exe-list (linereadp-e))
	(chg-flag nil))
    (dolist (exe-name exe-list)
      (cond ((member exe-name (cdr namelist))
	     (if (not chg-flag)
		 (msg F "Please specify the alias (the actual name): "))
	     (msg T exe-name ": ")
	     (do ((xxx (car (linereadp)) (car (linereadp))))
		 (xxx
		   (cond ((or (member xxx namelist)
			      (member xxx actual-name-list))
			  (msg T xxx
			       " is already being used as an exe-name or an alias." T
			       "No action taken."))
			 ((member exe-name actual-name-list)
			  (msg T exe-name " already has an alias. ")
			  (when (query "Do you want to change it?" T)
			    (setq actual-name-list
				  (replace1 actual-name-list
					    exe-name xxx))
			    (setq chg-flag T)))		       
			 (T (setq chg-flag T		     
				  actual-name-list
				  (nconc actual-name-list (list exe-name xxx)))
			    (putprop exe-name xxx 'real-name))))
	       (msg T "Please try again: ")))
	    (t (msg T exe-name " doesn't exist! No action taken."))))
    (when chg-flag (print-grades T))))

(defgexpr comment
  (mainfns commnt)
  (mhelp "To insert comments in the grade file."))

(defun commnt ()
  (msg F
       "Please enter the comment (enclosed in double-quotes): " T)
  (let ((remark (car (linereadp)))
	(reroute-close-message nil))
    (reroute-output-append grade-file *default-pathname-defaults*
      (msg T remark T)))
  (msg T "Comment added to file " (grade-file . filespec) "."))

(context gr-h-stat)

(defgexpr statistics
  (mainfns statistics-main)
  (mhelp "Compute statistical data."))

(defun statistics-main ()
  (declare (special namelist))
  (setq listofchanges (append listofchanges (cdr namelist)))
  (statistics)
  (print-grades t))

(defun statistics ()
  (when statistical-options
    (statistics1)))

(defun statistics1 ()
  (declare (special studentlist))
  (let ((temporary (delete 'id listofchanges)))
    (setq listofchanges nil)
    (dolist (item temporary)
      (if (eq (get item 'type) 'Le)
          (progn
            (unless (get '-mean- item) (putprop '-mean- '* item))
            (unless (get '-median- item) (putprop '-median- '* item))
            (unless (get '-sdev- item) (putprop '-sdev- '* item)))
          (pushnew item listofchanges))))
  (do ((field (car listofchanges) (car listofchanges))
       (listofchanges (cdr listofchanges) (cdr listofchanges))
       (studentlist1 (remove '--max-- studentlist))
       (mean 0 0)
       (SD 0 0)
       (scorelist nil nil))
      ((null field))
    (dolist (student studentlist1)
      (if (numberp (get student field))
	  (push (get student field) scorelist)))
    (if scorelist
        (let* ((num-student (length scorelist))
               (mid (1- (round (/ (1+ num-student) 2)))))
          (setq scorelist (sort scorelist #'<)
                mean (/ (apply #'+ scorelist) num-student))
          (putprop '-mean- (round-off mean print-n-digits) field)
          (dolist (score scorelist)
            (setq sd (+ sd (expt (- score mean) 2))))
          (putprop '-sdev- (round-off (sqrt (/ sd num-student))
                                      print-n-digits)
                   field)
          (putprop '-median-
                   (if (oddp num-student)
                       (round-off (nth mid scorelist) print-n-digits)
                       ;; Note that as mid is defined above,
                       ;; it is the elt which is on the
                       ;; low side of the list if num-student
                       ;; is even.  So need to average it
                       ;; with its successor (which must exist).
                       (round-off
                        (/ (+ (nth mid scorelist) (nth (1- mid) scorelist))
                           2) print-n-digits))
                   field))
        (progn (putprop '-median- '* field)
               (putprop '-mean- '* field)
               (putprop '-sdev- '* field))))
  (setq listofchanges nil))

(context gr-i-display)

(defgexpr number-of-students
  (mhelp "Use this command to find the number of students in the grade-file"))

(defun number-of-students ()
  (declare (special studentlist))
  (do ((studentlist studentlist (cdr studentlist))
       (number 0 (if (member (car studentlist)
			     '(--max-- -weight- -sdev- -mean- -median-))
		     number (1+ number))))
      ((null studentlist) number)))

(defgexpr display
  (mhelp "Display student-grades on the terminal."))

(defun display ()
  (declare (special namelist studentlist totalslist))
  (msg F
    "Please type the name of the student(s) [Type ALL if you want to look at"
       T "all the students] [Default: ALL]: ")
  (let ((name (or (linereadp-s) '(all)))
	maxlist typelist1 errant-flag)
    (let ((listofchoices (append (cdr namelist) (cdr totalslist))))
      (msg F (l listofchoices) T
	   "Please select the items that you want to look at [Type ALL if you"
	   t "want to look at all the scores][Default: ALL]:" T)
      (let ((listofitems (linereadp-e))
	    (temporary nil))
	(cond ((or (null listofitems) (eq (car listofitems) 'all))
	       (setq errant-flag T
		     typelist1 listofchoices))
	      (T (setq errant-flag nil)
		 (dolist (xxx listofitems)
		   (if (member xxx listofchoices) (push xxx temporary)
		       (msg F "Unable to locate " xxx ".")))
		 (setq typelist1 (nreverse temporary))))))
    (push 'name typelist1)
    (setq maxlist (maxsizelist studentlist typelist1))
    (cond ((eq (car name) 'all)
	   (setq studentlist (sort studentlist #'alphalessp))
	   (display1 typelist1 maxlist)
	   (dolist (student studentlist)
	     (displayterminal student maxlist typelist1)))
	  (name (let ((unable nil))
	       (display1 typelist1 maxlist)
	       (displayterminal '--max-- maxlist typelist1)
	       (when (cdr name)
		 (setq name (sort (delete '--max-- name) #'alphalessp)
		       errant-flag nil))
	       (dolist (name name)
		 (cond ((get name 'name)
			(displayterminal name maxlist typelist1)
			(if (and errant-flag (get name 'late-exe))
			    (msg F "Record of Late assignments (exe-name"
				 " dayslate ...):" T (t 15)
				 (get name 'late-exe))))
		       (t (push name unable))))
	       (if unable
		   (msg F "Unable to locate the following students: " T
			(l unable)))))
	  (t nil)))
  nil)

(defgexpr info-exercises
  (mhelp "Display aliases, penalty-fns, statistical data, weight, and
due-dates for the exercises on the terminal."))

(defun info-exercises ()
  (declare (special namelist actual-name-list))
  (let ((list-of-exercises nil))
    (do ((exe-list nil (linereadp-e)))
	((car exe-list) (setq list-of-exercises exe-list))
      (msg T "Please type the names of the exercises: "))
    (dolist (exe-name list-of-exercises)
      (msg F exe-name ":")
      (cond ((member exe-name (cdr namelist))
	     (let ((xxx (cadr (member exe-name actual-name-list))))
	       (if xxx (msg T (t 5) "Alias:" (t 25) xxx)))
	     (msg T (t 5) "Maximum Score:" (t 25) (get '--max-- exe-name))
	     (msg T (t 5) "Weight:" (t 25) (get1 exe-name 'weight))
	     (if (member '-mean- statistical-options)
		 (msg T (t 5) "Mean:" (t 25) (get '-mean- exe-name)))
	     (if (member '-median- statistical-options)
		 (msg T (t 5) "Median:" (t 25) (get '-median- exe-name)))
	     (if (member '-sdev- statistical-options)
		 (msg T (t 5) "Standard Dev:" (t 25)
		      (get '-sdev- exe-name)))
	     (msg T (t 5) "Penalty Function: " (t 25)
		  (get exe-name 'penalty default-penalty-fn))
	     (if (get exe-name 'due-date)
		 (msg T (t 5) "Due-Date: " (t 25)
		      (get exe-name 'due-date))))
	    (T (msg (t 10) "Unable to locate."))))))

(context gr-j-totals)

(defgexpr change-weight
  (mhelp "Change existing weighting factors."))

(defun change-weight ()
  (declare (special namelist))
  (msg F "The various exercises and their weights are: " T)
  (dolist (exe-name (cdr namelist))
    (princ (cons exe-name (get exe-name 'weight))))
  (msg T
       "Please enter a list of exercises (whose weights you want to change)"
       T "and their weights, e.g., (x2001 2)(x2002 3): " T)
  (let ((temporary (car (linereadp1))))
    (if (and temporary (atom (car temporary)))
	(setq temporary (list temporary)))
    (dolist (temp temporary)
      (let ((exe-name (car temp))
	    (weight (cadr temp)))
	(cond ((not (numberp weight))
	       (msg F "The weighting factor " weight " specified for exercise "
		    exe-name " is not a number." T "No action taken!"))
	      ((member exe-name namelist)
	       (putprop exe-name weight 'weight)
	       (putprop '-weight- weight exe-name))
	      (t (msg F "Unable to locate " exe-name "."))))))
  (print-grades T)
  nil)

(defgexpr calculate-grade
  (mhelp "Compute totals."))

(defun calculate-grade ()
  (declare (special namelist studentlist distinct-types totalslist))
  (dolist (student studentlist)
    (let ((late-exe-list (get student 'late-exe)))
      (when late-exe-list
	(do ((exe-name (car late-exe-list) (car late-exe-list))
	     (dayslate (cadr late-exe-list) (cadr late-exe-list))
	     (late-exe-list (cddr late-exe-list) (cddr late-exe-list)))
	    ((null exe-name))
	  (putprop student
		   (funcall (or (get exe-name 'penalty) default-penalty-fn)
			    (get1 student exe-name) dayslate
			    (get1 '--max-- exe-name))
		   exe-name)))))
  (dolist (student studentlist)
    (let ((total 0))
      (dolist (type distinct-types)
	(putprop student 0 type))
      (if (member 'Le distinct-types)
	  (putprop student nil 'Le))
      (dolist (name namelist)
	(let ((type (get name 'type)))
	  (cond ((eq type 'Le)
		 (push (get student name) (get student type)))
		((memq name '(name id)))
		(t (putprop student
			    (+ (get student type)
			       (* (get1 name 'weight) (get1 student name)))
			    type)))))
      (dolist (elt drop-min)
	(when (member elt distinct-types)
	  (do ((exe-list (get elt 'exe-name) (cdr exe-list))
	       (score 1000))
	      ((null exe-list)
	       (putprop student
			(round-off
			  (- (get student (car drop-min)) score)
			  print-n-digits)
			elt))
	    (setq score (min score
			     (* (get1 (car exe-list) 'weight)
				(get1 student (car exe-list))))))))
      (dolist (name distinct-types)
	(when (not (eq name 'Le))
	  (setq total (+ total (get student name)))
	  (putprop student (round-off (get student name) print-n-digits)
		   name)))
      (putprop student (round-off total print-n-digits) 'total)))
  (setq totalslist distinct-types)
  (push 'total totalslist)
  (when cal-percentage
    (push 'total% totalslist)
    (let ((maxscore (get '--max-- 'total)))
      (dolist (student studentlist)
	(putprop student
		 (round-off (* 100 (/ (get student 'total) maxscore))
			    print-n-digits)
		 'total%))))
  (dolist (item new-item)
    (let ((name (car item)))
      (push name totalslist)
      (dolist (student studentlist)
	(do ((item (cdr item)(cddr item))
	     (score 0))
	    ((null item)
	     (putprop student (round-off score print-n-digits) name))
	  (setq score
		(+ score (* (if (numberp (cadr item)) (cadr item) 0)
			       (get1 student (car item)))))))))
  (when (member 'Le totalslist)
    (putprop '-sdev- '* 'Le))
  (setq listOfChanges (append listOfChanges (delete 'Le (copy totalslist))))
  (push 'name totalslist)
  (let ((xxx totalslist))
    (%catch% (initialize nil) improper-file)
    (setq totalslist xxx))
  (printgrade1 studentlist totalslist 'out)
  (if new-item
      (reroute-output-append totals-grade-file *default-pathname-defaults*
	(do ((new-item new-item (cdr new-item)))
	    ((null new-item))
	  (msg T (caar new-item) ":" T 5 (l (cdar new-item)))))))

(defgexpr penalty-fns
  (mhelp "Assign penalty functions for various exercises."))

(defun penalty-fns ()
  (declare (special namelist))
  (msg F (l (cdr namelist)) T "Please type the names of the exercises: ")
  (let ((exe-list (linereadp-e)))
    (msg F "Please specify the penalty-fn: ")
    (dolist (exe-name exe-list)
      (msg T exe-name ": ")
      (let ((xxx (car (linereadp))))
	(putprop exe-name xxx 'penalty)
	(if (not (fboundp xxx))
	    (msg F "Warning: Penalty function " xxx " for exercise "
		 exe-name " is not defined.")))
      (if (not (member exe-name (cdr namelist)))
	  (setq namelist (nconc namelist (list exe-name))))))
  (print-grades T))

(context gr-k-sorting) 

(defgexpr sort-fn
  (mhelp "Sort the grades."))

(defun sort-fn ()
  (declare (special namelist studentlist totalslist))
  (let ((listofchoices nil))
    (let ((temporary (delete 'id (copy (cdr namelist)))))
      (dolist (item temporary)
	(if (not (eq (get item 'type) 'Le))
	    (push item listofchoices))))
    (setq listofchoices (nreverse (append listofchoices
					  (delete 'Le (copy totalslist)))))
    (do ((flag t
	       (query
		 "Do you want to sort the grades using another field?"  nil))
	 (studentlist1 (copy (append Statistical-options studentlist)))
	 (incrementflag 'askuser)
	 (field))
	((not flag))
      (declare (special field))
      (msg F (l listofchoices) T "Please choose the field: ")
      (do ((xxx (car (linereadp-e)) (car (linereadp-e))))
	  ((member xxx listofchoices) (setq field xxx))
	(msg T xxx " is not defined." T "Please choose a different field: "))
      (setq studentlist1
	    (sort studentlist1 #'> :key #'(lambda (name)
					    (declare (special field))
					    (get1 name field))))
      (when (eq incrementflag 'askuser)
	(setq incrementflag
	      (query "Do you want a display with increments?" T)))
      (let ((totalslist1 nil))
	(msg F (l (append (cdr namelist) (cdr totalslist))) T
	     "Please select the items to be displayed (in addition to " 
	     field ")" T "[Default: NONE]: ")
	(let ((temporary (linereadp-e)))
	  (do  ((xxx (car temporary) (car temporary))
		(temporary (cdr temporary) (cdr temporary)))
	      ((null xxx) (setq totalslist1 (nreverse totalslist1)))
	    (if (or (member xxx namelist) (member xxx totalslist))
		(push xxx totalslist1)
		(msg F "Unable to locate " xxx "."))))
	(push field totalslist1)
	(push 'name totalslist1)
	(if incrementflag (displaygrade field totalslist1 studentlist1)
	    (displaygrade1 field totalslist1 studentlist1)))))
  nil)

(context gr-l-letter-grade)

(defgexpr letter-grade
  (mhelp "Assign letter grades."))

(defun letter-grade ()
  (declare (special namelist studentlist totalslist))
  (msg F "Please type the name for the new letter grade: ")
  (let ((name (car (get-exename)))
	flag field-list)
    ;;(initialize-exercise-ctree)
    (cond (totalslist
	   (msg F
		"Please type 1 if you want to add a letter grade to "
		(grade-file . filespec) "," T 7
		"type 2 if you want to add a letter grade to "
		(totals-grade-file . filespec) "," T 7
		"type 3 if you want to add a letter grade to both: ")
	   (do ((xxx (car (linereadp)) (car (linereadp))))
	       ((member xxx '(1 2 3)) (setq flag xxx))
	     (msg T "Please type 1, 2 or 3: ")))
	  (t (setq flag 1)))
    (setq field-list (cal-letter-grade name))
    (cond ((member flag '(1 3))
	   (putprop name 'Le 'type)
	   (putprop 'type 'Le name)
	   (putprop '-weight- '* name)
	   (putprop '-sdev- '* name)
	   (when (= flag 3)
		 (setq totalslist (append totalslist (list name)))
		 (printgrade1 studentlist totalslist 'append))
	   (setq namelist (append namelist (list name)))
	   (print-grades T))
	  (t (setq totalslist (append totalslist (list name)))
	     (putprop '-sdev- '* name)	   
	     (printgrade1 studentlist totalslist 'append)))
    (when (member flag '(2 3))
      (reroute-output-append
	  totals-grade-file *default-pathname-defaults*
	(msg T (car field-list) ":" T (cdr field-list) T))))
  nil)

(context gr-a-out)

(defgexpr gr-leave
  (mainfns grader-exit)
  (mhelp "Leave GRADING PACKAGE to the next enclosing top level."))

(defgexpr leave
  (mainfns grader-exit)
  (mhelp "Leave GRADING PACKAGE to the next enclosing top level."))

(defgexpr gr-exit
  (mhelp "Leave GRADING PACKAGE, and exit TPS."))

(defun gr-exit ()
  (eval (interpret-tps-top  '(exit))))

(defun grader-exit () (%throw% '|[Left GRADING PACKAGE]| exit-inferior-top))


