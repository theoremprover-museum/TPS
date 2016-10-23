;;; -*- Mode:LISP; Package:TEACHER -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
;;; ;;; File Grades1 ;;;

(in-package :teacher)
(part-of grader)

(deffile grades1
  (part-of grader)
  (extension lisp)
  (mhelp "Creates the grading package top-level."))

;(proclaim '(special actual-name-list command-interpreter ;;command-ctree
;		    distinct-types finalfileflag grade-command-ctree
;		    score-file namelist print-* studentlist temporary-file
;		    top-prompt-fn totalslist))

(defvar read-grade-ini nil)
(defvar listofchanges nil)
(defvar gradefileflag T)
(defvar msg-template nil)

(defun gradetop ()
  (let ((top-prompt-fn #'grade-top-prompt)
	(command-interpreter #'grade-command-interpreter)
	(print-* #'grade-print-*)
	;;(command-ctree grade-command-ctree)
	(top-level 'grade-top))
    (secondary-top)))

(defun grade-top-prompt (top-event-nr)
  (format nil "<Gr~A>" top-event-nr))

(defun grade-command-interpreter (cmd)
  (declare (special expertflag curr-cmd))
  (setq curr-cmd 'none)
  (cond ((null cmd) nil)
	((and (null (cdr cmd)) (atom (car cmd)))
	 (cond ((integerp (car cmd))
		  (throwfail "Unknown command."))
	       ((symbolp (car cmd))
		(cond ((get (car cmd) 'gexpr)
		       (cond ((not (or gradefileflag
				       (member (car cmd)
					       '(gr-review chg-vars gr-leave
							   gr-exit leave
							   create-gradefile))))
			      (complain 
			       "But there are no students! Please change the name of the GRADE-FILE"
			       T 
			       "or create a new GRADE-FILE.")
			      nil)
			     ((get (car cmd) 'mainfns))
			     (T cmd)))
		 ((get (car cmd) 'mexpr)
		  (setq curr-cmd (car cmd))
		  `(comdecode ',cmd))	       
		 ((get (car cmd) 'reviewcmd)
		  `(comdecode  ',cmd))
		 ((get (car cmd) 'flag)
		  `(comdecode '(setflag ,@cmd)))
		 ((null expertflag)
		  (throwfail "Unknown Command."))
		 ((boundp (car cmd)) (car cmd))
		 ((not (fboundp (car cmd)))
		  (throwfail ";" (car cmd) " - Unbound variable."))))
	       ;; Not a symbol or an integer
	       ((null expertflag)
		(throwfail "Unknown Command."))
	       (t (car cmd))))
	((and expertflag (null (cdr cmd)))
	 (cond ((not (and (symbolp (caar cmd)) (fboundp (caar cmd))))
		(throwfail ";" (car cmd) " - Undefined function."))
	       (t (car cmd))))
	((not (symbolp (car cmd)))
	 (throwfail "Illegal input - must be EXPERT to evaluate Lisp form."))
	((get (car cmd) 'mexpr)
	 (setq curr-cmd (car cmd))
	 `(comdecode ',cmd))
	((get (car cmd) 'reviewcmd)
	 `(comdecode ',cmd))
	((get (car cmd) 'flag)
	 (if (cdr cmd) `(comdecode '(set ,@cmd))
	     `(comdecode '(setflag ,@cmd))))
	((null expertflag)
	 (throwfail "Unknown command."))
	((not (fboundp (car cmd)))
	 (throwfail ";" (car cmd) " - Undefined function."))
	(t cmd)))

(defun grade-print-* (result)
  (if result (msg T result)))
    
(defun days-late (due-date date)
  (do ((xxx (- (+ (* 12 (- (car date) (car due-date))) (cadr date)) 1
	       (cadr due-date)))
       (i 0 (1+ i))
       (days 0)(due-date (caddr due-date))(date (caddr date))
       (yr (car due-date))
       (month (cadr due-date)))
      ((> i xxx) (let ((days (+ (- days due-date) date)))
		   (cond ((< xxx -1) 0) ((plusp days) days) (T 0))))
    (setq days (+ days
		  (case (+ i month)
		    (2 (if (zerop (mod yr 4))  29 28))
		    ((1 3 5 7 8 10 12) 31)
		    (T 30))))))

(defun defaultfn1 (default)
  (let ((n (car (linereadp))))
    (cond ((null n) default) 
	  ((numberp default)
	   (do ((xxx n (car (linereadp))))
	       ((or (null xxx) (and (integerp xxx) (> xxx 0)))
		(if xxx xxx default))
	     (msg F "Please type a positive integer or <cr>[Default: "
		  default "]: ")))
	  (T n))))

(defun get1 (name property)
  (let ((number (get name property)))
    (if (numberp number) number 0)))

(defun initialize (warning-flag)
  (declare (special studentlist namelist finalfileflag totalslist distinct-types actual-name-list))
  (when (and warning-flag (boundp 'studentlist) (boundp 'namelist))
    (dolist (student studentlist)
      (dolist (exe namelist)
	(putprop student nil exe)))
    (dolist (exe-name namelist)
      (putprop exe-name nil 'exe-name)
      (putprop exe-name nil 'penalty)
      (putprop exe-name nil 'due-date)
      (putprop exe-name nil 'type)
      (putprop exe-name nil 'real-name)
      (putprop exe-name nil 'weight)))
  (setq finalfileflag nil
	studentlist nil
	totalslist nil
	namelist nil
	distinct-types nil)
  (if (probe-file grade-file) 
      (let (studentlist1 exercise-list typelist late-list)
	(with-open-file (donefile grade-file :direction :input)
	  (setq studentlist1 (read donefile nil))
	  (let ((xxx (read donefile nil)))
	    (if (and (listp xxx) (listp (car xxx))
		     (listp (cadr xxx)) (listp (caddr xxx)))
		(setq late-list (car xxx)
		      exercise-list (cadr xxx)
		      actual-name-list (caddr xxx))
		(throw 'improper-file
		       "late-list, penalty-fns, or actual-name-list"))))
	(if (not (listp (setq namelist (car studentlist1))))
	    (throw 'improper-file "names of exercises"))
	(if (not (listp (setq typelist (cadr studentlist1))))
	    (throw 'improper-file "types of exercises"))
	(setq Statistical-options nil)
	(do ((exe-name (car namelist) (car namelist))
	     (type (car typelist) (car typelist))
	     (namelist (cdr namelist) (cdr namelist))
	     (typelist (cdr typelist) (cdr typelist))
	     (weightlist (assoc '-weight- studentlist1) (cdr weightlist)))
	    ((null exe-name))
	  (putprop exe-name exe-name 'exe-name)
	  (when (not (or (member type distinct-types)
			 (member exe-name '(name id))))
	    (putprop type nil 'exe-name)
	    (push type distinct-types)
	    (putprop type 1 'weight))
	  (let ((xxx (cdr (member exe-name exercise-list))))
	    (if xxx
		(let ((penalty (car xxx))
		      (due-date (cadr xxx)))
		  (when (not (eq penalty 'none))
		    (if (and warning-flag (not (fboundp penalty)))
			(msg F "WARNING: " penalty " which is used as "
			     "penalty function  for exercise " T
			     exe-name " is not defined." T))
		    (putprop exe-name penalty 'penalty))
		  (if (not (eq due-date 'none))
		      (putprop exe-name due-date 'due-date)))))
	  (putprop exe-name type 'type)
	  (push exe-name (get type 'exe-name))
	  (let ((real-name (cadr (member exe-name actual-name-list))))
	    (if real-name (putprop exe-name real-name 'real-name)))
	  (putprop exe-name (car weightlist) 'weight))
	(do ((record-length (length namelist))
	     (student (cadr studentlist1) (car studentlist1))
	     (studentlist1 (cddr studentlist1) (cdr studentlist1))
	     (student-name))
	    ((null student))
	  (setq student-name (car student))
	  (if (not (= (length student) record-length))
	      (msg F ";;;WARNING:" student-name
		   "'s record is not compatible with other records."))
	  (if (member student-name '(-mean- -median- -sdev-))
	      (push student-name Statistical-options)
	      (let ((xxx (assoc student-name late-list)))
		(push student-name studentlist)
		(if xxx (putprop student-name (cdr xxx) 'late-exe))))
	  (do ((student student (cdr student))
	       (namelist namelist (cdr namelist)))
	      ((null namelist))
	    (putprop student-name (car student) (car namelist))))
	(setq gradefileflag T)
	(setq studentlist
	      (nreverse (delete 'type (delete '-weight- studentlist)))))
      (setq gradefileflag nil))
  'OK)

(defun initialize1 ()
  (declare (special temporary-file))
  (let ((mssg (%catch% (initialize T) improper-file)))
    (cond ((null mssg)
	   (setq gradefileflag nil)
	   (msg F
		";;;Error while initializing student records." T
		";;;Initialization aborted. Please modify the file "
		(grade-file . filespec) T  " appropriately." T))
	  ((not (eq mssg 'OK))
	   (msg F
                ";;;Error while initializing the following record:" T
		15 mssg T
		";;;Initialization aborted. Please modify the file "
		(grade-file . filespec) T " appropriately." T)
	   (setq gradefileflag nil))
	  (T ;;(initialize-student-ctree)
	     ;;(initialize-exercise-ctree)
	     (when (probe-file temporary-file)
	       (msg F "File " (temporary-file . filespec)
		    " possibly contains grades that you started to" T
		    "enter, but didn't finish entering. ")
	       (if (query "Do you want to resume that process?" T)
		   (insert-grades-main3)
		   (msg F ";;WARNING: File " (temporary-file . filespec)
			" will be overwritten, if you use the" T
			"INSERT-GRADES command. " 
			"Use RESUME-INSERT-GRADES to resume "
			"entering grades.")))))))

(defun cal-letter-grade (name)
  (declare (special namelist totalslist studentlist))
  (let ((listOfTypes nil) field grade-list)
    (let ((temporary
	   (delete 'id (append (copy (cdr namelist))
			       (delete 'Le (cdr (copy totalslist)))))))
      (dolist (item temporary) (setq listoftypes (nreverse listoftypes))
	(if (not (eq (get item 'type) 'Le))
	    (push item listoftypes))))
    (msg F (l ListOfTypes) T "Please choose the field: ")
    (do ((xxx (car (linereadp-e)) (car (linereadp-e))))
	((memq xxx listoftypes) (setq field xxx))
      (msg T xxx " is not defined.  Please choose a different field: "))
    (msg F
	 "Please type a list of letter grades and the corresponding minimum scores,"
	 T "e.g., (a 90)(b+ 80)(a- 85)(b 75)...(r 0):" T)
    (setq grade-list (sort (linereadp) #'> :key #'cadr))
    (let ((length (1- (length grade-list)))
	  grade)
      (dolist (student (append studentlist Statistical-options))
	(do ((index 0 (1+ index))(score (get1 student field)))
	    ((or (> index length)
		 (not (> (cadr (nth index grade-list)) score)))
	     (cond ((> index length)
		    (setq grade '*)
		    (msg F "Unable to decide grade for " student
			 ". Assigned *."))
		   (t (setq grade (car (nth index grade-list)))))))
	(putprop student grade name)))
    (if (memq '-sdev- statistical-options) (putprop '-sdev- '* name))
    (when letter-grade-flag
      (msg F "Creating file " (letter-grade-file . filespec) ".")
      (print-letter-grades name))
    (cons field grade-list)))

(defun linereadp1 ()
  (let ((value (linereadp)))
    (if (or (atom (car value)) (not (= (length value) 1)))
	(list value) value)))

(defun linereadp2 (length1 &optional (default nil))
  (let ((list nil)
	(old-list
	  (do ((list1 (linereadp)  (linereadp)))
	      ((or (= (length list1) length1)
		   (and (null list1) (= (length default) length1))
		   (and (listp (car list1)) (= (length (car list1)) length1)
			(numberp (caar list1))))
	       (when (and (null list1) (= (length default) length1))
		     (setq list1 default))
	       (if (= (length list1) length1) list1 (car list1)))
	    (msg F "Please type " length1 " items: "))))
    (dolist (item old-list)
      (if (and (listp item) (fboundp (car item)))
	  (let ((value (eval item)))
	    (if value (push value list) (push item list)))
	  (push item list)))
    (nreverse list)))

(defun maxsizelist (list1 namelist)
  (let ((maxlist nil))
    (dolist (exe-name namelist (nreverse maxlist))
      (let ((maxsize (flatc exe-name)))
	(dolist (student list1 (push maxsize maxlist))
	  (setq maxsize (max maxsize (flatc (get student exe-name)))))))))

(defun penalty (score dayslate max)
  (declare (ignore max))
  (max 0 (- score dayslate)))

(defun penalty-0 (score dayslate max)
  (declare (ignore max))
  (if (plusp dayslate) 0 score))

(defun penalty%10 (score dayslate max)
  (penalty% score dayslate max 10))

(defun penalty1%10 (score dayslate max)
  (penalty1% score dayslate max 10))

(defun penalty2%10 (score dayslate max)
  (penalty2% score dayslate max 10))

(defun penalty% (score dayslate max percent)
  (max 0 (- score (/ (* percent max dayslate) 100))))

(defun penalty1% (score dayslate max percent)
  (declare (ignore max))
  (max 0 (- score (/ (* percent score dayslate) 100))))

(defun penalty2% (score dayslate max percent)
  (declare (ignore max))
  (do ((i 1 (1+ i))
       (score score (- score (/ (* percent score) 100))))
      ((> i dayslate) (max 0 (round-off score 0)))))

(defun get-exename ()
  (declare (special studentlist namelist distinct-types))
  (let ((listofexercises (do ((xxx (linereadp) (linereadp)))
			     ((car xxx) xxx)
			   (msg T "NIL is a special LISP symbol."
				T "Please type exe-names: ")))
	(new-list nil))
    (do ((exe-name (car listofexercises) (car listofexercises))
	 (listofexercises (cdr listofexercises) (cdr listofexercises)))
	((not (or exe-name listofexercises))
	 (nreverse new-list))
      label
      (cond ((member exe-name '(T nil))
	     (msg T "\"" exe-name "\" is a special LISP symbol. Please "
		  "specify a different name: ")
	     (setq exe-name (car (linereadp)))
	     (go label))
	    ((member exe-name studentlist)
	     (msg F exe-name
		  " is being used as a student name. Please specify a different name: ")
	     (setq exe-name (car (linereadp)))
	     (go label))	     
	    ((or (member exe-name namelist) (member exe-name distinct-types))
	     (cond ((get 'type exe-name)
		    (msg F exe-name
			 " already exists. Please specify a new name: ")
		    (setq exe-name (car (linereadp)))
		    (go label))
		   (t (push exe-name new-list)
		      (setq namelist (delete exe-name namelist)))))
	    ((not (symbolp exe-name))
	     (msg F exe-name
		  " is not a symbol. Please specify a new name: ")
	     (setq exe-name (car (linereadp)))
	     (go label))
	    (t (putprop exe-name exe-name 'exe-name)
	       (push exe-name new-list))))))

(defun display2 (student maxlist totalslist)
  (msg student)
  (do ((yyy (1+ (flatc student)) (1+ yyy))(numblanks (car maxlist)))
      ((> yyy numblanks))
    (msg 1))
  (do ((maxlist (cdr maxlist) (cdr maxlist))
       (totalslist (cdr totalslist) (cdr totalslist))
       (item nil))
      ((null maxlist))
    (setq item (get student  (car totalslist) '*))
    (do ((xxx 0 (1+ xxx))
	 (numblanks (- (car maxlist) (flatc item))))
	((> xxx numblanks) (msg item))
      (msg 1))))

(defun display1 (totalslist maxlist)
  (msg (car totalslist))
  (do ((yyy (1+ (flatc (car totalslist))) (1+ yyy)) (numblanks (car maxlist)))
      ((> yyy numblanks))
    (msg 1))
  (do ((maxlist (cdr maxlist) (cdr maxlist))
       (totalslist (cdr totalslist) (cdr totalslist)))
      ((null totalslist))
    (do ((xxx 0 (1+ xxx))
	 (numblanks (- (car maxlist) (flatc (car totalslist)))))
	((> xxx numblanks) (msg (car totalslist)))
      (msg 1))))

(defun displaygrade-main (field totalslist studentlist1 increment
				maximum minimum maxlist)
  (msg "Grades sorted on the basis of " field T 5)
  (display1 totalslist maxlist)
  (msg T 5)
  (when (member '-sdev- studentlist1)
    (display2 '-sdev- maxlist totalslist)
    (setq studentlist1 (delete '-sdev- studentlist1)))
  (msg -2)
  (let ((K maximum))
    (setq maximum (round maximum))
    (setq maximum (+ maximum
		     (mod (- increment (mod maximum increment))
			  increment)))
    (when (> K maximum)
      (setq maximum (1+ maximum))
      (setq maximum (+ maximum (mod
				 (- increment (mod maximum increment))
				 increment)))))
  (do ((increment-flag nil nil)(J maximum) (L nil) (K nil))
      ((< J minimum))
    (msg T J)
    (setq L J
	  J (- J increment)
	  K (get1 (car studentlist1) field))
    (unless studentlist1
      (setq K -50.0
	    J -25.0))
    (do ()
	((not (> K J)))
      (if increment-flag (msg T 5)
	  (do ((xxx (flatc L) (1+ xxx)))
	      ((> xxx 4) (setq increment-flag t))
	    (msg 1)))
      (display2 (car studentlist1) maxlist totalslist)
      (setq studentlist1 (cdr studentlist1)
	    K (get1 (car studentlist1) field))
      (unless studentlist1(setq K -50.0)))))

(defun displaygrade (field totalslist studentlist1)
  (declare (special finalfileflag))
  (setq studentlist1 (delete '-sdev- studentlist1))
  (let ((maxlist (maxsizelist studentlist1 totalslist))
	(maximum (get1 (car studentlist1) field))
	(minimum (get1 (car (last studentlist1)) field))
	increment)
    (msg F "The maximum and minimum scores on " field " are " maximum
	 " and " minimum " respectively." T
	 "Please specify an increment for the display[Default:5]: ")
    (setq increment (defaultfn1 5))
    (cond (finalfileflag
	   (msg F "[Updating " (totals-grade-file . filespec) "]")
	   (reroute-output-append totals-grade-file *default-pathname-defaults*
	     (msg T)
	     (tyo (char-int #\page))
	     (msg T)
	     (displaygrade-main
	      field totalslist studentlist1
	      increment maximum minimum maxlist)))
	  (t (rename-file% totals-grade-file old-totals-grade-file)	     
	     (setq finalfileflag T)
	     (reroute-output totals-grade-file *default-pathname-defaults* 
	       (displaygrade-main
		field totalslist studentlist1
		increment maximum minimum maxlist))))))

(defun displaygrade1-main (field totalslist studentlist1)
  (msg "Grades sorted on the basis of " field T)
  (let ((maxlist (maxsizelist studentlist1 totalslist)))
    (display1 totalslist maxlist)
    (msg T)
    (when (member '-sdev- studentlist1)
      (display2 '-sdev- maxlist totalslist)
      (setq studentlist1 (delete '-sdev- studentlist1)))
    (msg -2)
    (dolist (student studentlist1)
      (msg T)
      (display2 student maxlist totalslist))))

(defun displaygrade1 (field totalslist studentlist1)
  (declare (special finalfileflag))
  (cond (finalfileflag
	 (msg F "[Updating " (totals-grade-file . filespec) "]")
	 (reroute-output-append totals-grade-file *default-pathname-defaults*
	   (msg T) (tyo (char-int #\page)) (msg T)
	   (displaygrade1-main
	    field totalslist studentlist1)))
	(t (rename-file% totals-grade-file old-totals-grade-file) 
	   (setq finalfileflag T)
	   (reroute-output totals-grade-file *default-pathname-defaults* 
	     (displaygrade1-main
	      field totalslist studentlist1)))))

(defun displayterminal (student maxlist namelist)
  (let ((item (get student (car namelist)))
	(maxsize (car maxlist)))
    (msg F item)
    (do ((numblanks (flatc item) (1+ numblanks)))
	((> numblanks maxsize))
      (msg 1))
    (do ((item (get student (cadr namelist))
	       (get student (car namelist)))
	 (maxlist (cdr maxlist) (cdr maxlist))
	 (namelist (cddr namelist) (cdr namelist)))
	((null maxlist))
      (do ((xxx 1 (1+ xxx))
	   (numblanks (- (car maxlist) (flatc item))))
	  ((> xxx numblanks) (msg item))
	(msg 1))
      (msg 1))))

(defun print-grades-main ()
  (declare (special studentlist namelist actual-name-list))
  (let ((list (append Statistical-options studentlist)) maxlist)
    (setq namelist (remove-duplicates namelist)) 
					;**** above hack because if you pressed ^C during insert-grades 
					;**** and then did resume-insert-grades you got two copies of the 
					;**** interrupted columns. MB Wed Sep 17 11:51:45 1997
    (setq list (cons 'type (sort (cons '-weight- (copy list)) #'alphalessp)))
    (setq maxlist (maxsizelist list namelist))
    (msg "(" T "(")
    (display1 namelist maxlist)
    (msg ")" T)
    (dolist (elt list)
      (msg "(")
      (display2 elt maxlist namelist)
      (msg ")" T ))
    (msg ")")
    (msg T "(;;;Record of late assignments: " T "(")
    (let ((flag nil))
      (dolist (student (cdr list))
	(let ((late (get student 'late-exe)))
	  (when late
	    (if flag (msg T) (setq flag T))
	    (msg "(" student 1)
	    (dolist (elt late)
	      (msg elt 1))
	    (msg ")"))))))
  (msg ")" T ";;;Record of penalty functions and due-dates: " T "(")
  (let ((flag nil))
    (dolist (exe-name (cdr namelist))
      (let ((penalty (get exe-name 'penalty))
	    (due-date (get exe-name 'due-date)))
	(when (or penalty due-date)
	  (if flag (msg T))
	  (setq flag T)
	  (msg exe-name 1 (if penalty penalty 'none)
	       1 (if due-date due-date 'none))))))
  (msg ")" T ";;;List of actual names" T "(")
  (do ((i 0 (mod (1+ i) 4))
       (actual-name-list actual-name-list (cdr actual-name-list)))
      ((null actual-name-list) (msg "))" t))
    (if (zerop i) (msg T))
    (msg (car actual-name-list) " ")))

(defun print-grades (Comment-flag)
  (statistics)
  (let ((flag (rename-file% grade-file old-grade-file)))
    (reroute-output grade-file *default-pathname-defaults* (print-grades-main))
    (when (and flag comment-flag)
      (with-open-file
       (donefile1 old-grade-file :direction :input)
       (with-open-file
	(donefile grade-file :direction :output :if-exists :append)
	(read donefile1 nil)
	(read donefile1 nil)
	(do ((char (read-char donefile1 nil numeric-eof-value)
		   (read-char donefile1 nil numeric-eof-value)))
	    ((equal char numeric-eof-value))
	  (write-char char donefile)))))))

(defun printgrade1-main (studentlist namelist)
  (let ((list (sort (delete 'type (copy (append Statistical-options
						studentlist)))
		    #'alphalessp)) maxlist)
    (setq maxlist (maxsizelist list namelist))
    (display1 namelist maxlist)
    (msg T)
    (when (member '-sdev- Statistical-options)
      (display2 '-sdev- maxlist namelist)
      (setq list (delete '-sdev- list)))
    (msg T)
    (dolist (elt list)
      (display2 elt maxlist namelist)
      (msg T))))

(defun printgrade1 (studentlist namelist mode)
  (declare (special finalfileflag))
  (statistics)
  (setq finalfileflag T)
  (if (eq mode 'out)
      (progn
        (rename-file% totals-grade-file old-totals-grade-file)
        (reroute-output totals-grade-file *default-pathname-defaults*
          (printgrade1-main studentlist namelist)))
      (progn
        (msg T "Updating file " (totals-grade-file . filespec))
        (reroute-output-append totals-grade-file *default-pathname-defaults*
          (msg T) (tyo (char-int #\page)) (msg T)
          (printgrade1-main studentlist namelist)))))

(defun print-letter-grades (name)
  (declare (special studentlist))
  (let  ((list (sort (delete '--max-- (copy studentlist)) #'alphalessp))
	 maxlist namelist)
    (setq namelist (list 'name name)
          maxlist (maxsizelist list namelist))
    (reroute-output letter-grade-file *default-pathname-defaults*
      (dolist (elt list)
	(display2 elt maxlist namelist)
	(msg T)))))

(defun next-counter ()
  (declare (special next-num*))
  (incf next-num*))

(defun prompt4 ()
  (declare (special next-num*))
  (msg T (next-counter) (t 4) "Course-Name" (t 28) Course-Name)
  (msg T (next-counter) (t 4) "Grade-Dir" (t 28) "\"" grade-dir "\"")
  (msg T (next-counter) (t 4) "Grade-File"  (t 28) (grade-file . filespec))
  (msg T (next-counter) (t 4) "Totals-Grade-File"  (t 28)
       (Totals-grade-file . filespec))
  (msg T (next-counter) (t 4) "Old-Grade-File" (t 28) 
       (old-grade-file . filespec))
  (msg T (next-counter) (t 4) "Old-Totals-Grade-File" (t 28) 
       (old-totals-grade-file . filespec))
  (msg T (next-counter) (t 4) "Letter-grade-file" (t 28) 
       (letter-grade-file . filespec))
  (msg T (next-counter) (t 4) "ETPS-file" (t 28)
       (ETPS-FILE . filespec))
  (msg T (next-counter) (t 4) "Statistical-options" (t 28) 
       Statistical-options)
  (msg T (next-counter) (t 4) "print-n-digits" (t 28) 
       print-n-digits)
  (msg T (next-counter) (t 4) "cal-percentage" (t 28) 
       cal-percentage)
  (msg T (next-counter) (t 4) "letter-grade-flag" (t 28) 
       letter-grade-flag)
  (msg T (next-counter) (t 4) "new-item" (t 28) new-item)
  (msg T (next-counter) (t 4) "drop-min" (t 28) drop-min)
  (msg T (next-counter) (t 4) "due-date-flag" (t 28) due-date-flag)
  (msg T (next-counter) (t 4) "default-penalty-fn" (t 28)
       default-penalty-fn T))

;;The function READ1 makes sure that the user types a non-negative
;;;integer.

(defun read1 ()
  (do ((n (car (linereadp))(car (linereadp))))
      ((and (integerp n)(> n 0)) n)
    (msg F "Please type a positive integer: ")))

(defun rename-file% (oldname newname)
  (cond ((and (probe-file oldname) Newname)
	 (rename-file oldname Newname)
	 (msg F "The old file " (oldname . filespec)
	      " has been renamed to" T (Newname . filespec) "." T
	      "Creating new copy of file " (oldname . filespec) ".")
	 T)
	((not (probe-file oldname))
	 (msg F "Creating file " (oldname . filespec) ".")
	 nil)
	(t (msg F "Creating new copy of file " (oldname . filespec) ".")
	   nil)))

(defun round-off (number print-n-digits)
  (if (and (numberp number) (integerp print-n-digits)
           (not (minusp print-n-digits)))
      (let* ((foo (expt 10 print-n-digits))
             (result (/ (round (* number foo)) foo)))
        (if (integerp result) result (float result)))
      number))

(defun initialize-filenames ()
  (declare (special temporary-file))
  (if (symbolp course-name)
      (setq course-name (string-downcase (symbol-name course-name))))
  (if (and (stringp grade-file) (string= "" grade-file))
      (setq grade-file (merge-pathnames grade-dir 
	    (make-pathname% :name course-name :type "grades"))))
  (if  (and (stringp letter-grade-file) (string= "" letter-grade-file))
      (setq letter-grade-file (merge-pathnames grade-dir
	    (make-pathname% :name course-name :type "letter"))))
  (if  (and (stringp old-grade-file) (string= "" old-grade-file))
      (setq old-grade-file (merge-pathnames grade-dir
	    (make-pathname% :name course-name :type "xgrades"))))
  (if (and (stringp old-totals-grade-file) (string= "" old-totals-grade-file))
      (setq old-totals-grade-file (merge-pathnames grade-dir
	    (make-pathname% :name course-name :type "xtotals"))))
  (if  (and (stringp totals-grade-file) (string= "" totals-grade-file))
      (setq totals-grade-file (merge-pathnames grade-dir
	    (make-pathname% :name course-name :type "totals"))))
  (setq temporary-file (merge-pathnames grade-dir
	(make-pathname% :name course-name :type "temp"))))

(defun unbind-filenames ()
  (declare (special temporary-file))
  (setq grade-file ""
        letter-grade-file ""
        old-grade-file ""
        old-totals-grade-file ""
        totals-grade-file ""
        temporary-file ""))

(defun update-filenames ()
  (declare (special temporary-file))
  (setq grade-file (merge-pathnames grade-dir
	(make-pathname%  :name course-name :type "grades")))
  (setq letter-grade-file (merge-pathnames grade-dir
	(make-pathname%  :name course-name :type "letter")))
  (setq old-grade-file (merge-pathnames grade-dir
	(make-pathname%  :name course-name :type "xgrades")))
  (setq old-totals-grade-file (merge-pathnames grade-dir
	(make-pathname%  :name course-name :type "xtotals")))
  (setq totals-grade-file (merge-pathnames grade-dir
	(make-pathname%  :name course-name :type "totals")))
  (setq temporary-file (merge-pathnames grade-dir
	(make-pathname%  :name course-name :type "temp"))))

(defun replace1 (list item value)
  (do ((xxx (car list) (car list))
       (list (cdr list) (cdr list))
       (new-list nil (cons xxx new-list)))
      ((or (null xxx) (equal xxx item))
       (cond (xxx
	      (push xxx new-list)
	      (setq new-list
		    (append (nreverse (cons value new-list)) (cdr list))))
	     (T (setq new-list (nreverse new-list))))
       new-list)))

(context gr-b-vars)

(defgexpr chg-vars
  (mhelp "Change the values of various variables."))

(defun chg-vars ()
  (declare (special namelist finalfileflag))
  (let ((next-num* 0) temporary)
    (declare (special next-num*))
    (msg F "The current value of some of the variables is:")
    (prompt4)
    (do ((temp T)
	 (val-prompt (next-counter)) ;17
	 (exit-prompt (next-counter))) ;18
	((not temp))
      (setq next-num* 0)
      (msg T
	   "Please type one of the numbers in the first column, or " val-prompt
	   " to look at the"  T  "variables, or " exit-prompt " to exit. [Default:"
									 exit-prompt "]:")
      (do ((temp1 (defaultfn1 exit-prompt) (defaultfn1 exit-prompt)))
	  ((not (> temp1 exit-prompt))
	   (case temp1
	     (18 (setq temp nil))
	     (17 (prompt4))
	     (13 (msg "Please type the new item(s): ")
		 (setq temporary (linereadp))
		 (setq new-item (append new-item temporary)))
	     (14 (msg  "Please type the item(s): ")
		 (setq temporary (linereadp-e))
		 (setq drop-min (append drop-min temporary)))
	     ((1 9 10 11 12 15 16)
	      (msg t "Please type the new value: ")
	      (setq temporary (car (linereadp)))		 
	      (case temp1
		(15 (setq due-date-flag (if temporary T nil)))
		(16 (if (fboundp temporary)
			(setq default-penalty-fn temporary)
			(msg F "Undefined function " temporary
			     ". No action taken!")))
		(12 (setq letter-grade-flag (if temporary T nil)))
		(1 (setq course-name
			 (if (symbolp temporary)
			     (string-downcase (symbol-name temporary))
			     temporary))
		   (update-filenames)
		   (initialize1))
		(10 (if (numberp temporary)
		       (setq listofchanges
			     (append listofchanges (cdr namelist))
			     print-n-digits temporary)
		       (msg t temporary
			    " is not a number. No action taken!")))
		(11 (setq cal-percentage temporary))
		(9 
		 (setq Statistical-options nil)
		 (dolist (option temporary)
		   (if (member option '(-mean- -median- -sdev-))
		       (push option Statistical-options)
		       (msg T "Can't calculate " option)))))
	      (if (member temp1 '(1 9))
		  (setq listofchanges (append listofchanges (cdr namelist)))))
	     (t (msg "Please type the new value: ")
		(case temp1
		  (2 (setq grade-dir (string-downcase (string (car (linereadp)))))
		     (update-filenames) (setq gradefileflag nil) (initialize1))
		  (3 (setq grade-file
			   (merge-pathnames
			     ;; added string-downcase -- DAN 2-5-88
			    (parse-namestring (string-downcase 
						(string (car (linereadp)))))
			    (make-pathname% :name course-name :type "grades")))
		     (initialize1))
		  (4 (setq totals-grade-file
			     ;; added string-downcase -- DAN 2-5-88
			   (merge-pathnames
			    (parse-namestring (string-downcase 
						(string (car (linereadp)))))
			    (make-pathname% :name course-name :type "totals")))
		     (setq finalfileflag nil))
		  (5 (setq old-grade-file
			   (merge-pathnames
			     ;; added string-downcase -- DAN 2-5-88
			     (parse-namestring (string-downcase 
					      (string (car (linereadp)))))
			    (make-pathname% :name course-name :type "xgrade"))))
		  (6 (setq old-totals-grade-file
			   (merge-pathnames
			     ;; added string-downcase -- DAN 2-5-88
			    (parse-namestring (string-downcase
						(string (car (linereadp)))))
			    (make-pathname% :name course-name
					    :type "xtotals"))))
		  (8 (setq ETPS-FILE
			   (merge-pathnames
			     ;; added string-downcase -- DAN 2-5-88
			     (parse-namestring (string-downcase
						 (string (car (linereadp)))))
			    etps-file)))
		  (7 (setq letter-grade-file
			   (merge-pathnames
			     ;; added string-downcase -- DAN 2-5-88
			    (parse-namestring (string-downcase
						(string(car (linereadp)))))
			    (make-pathname% :name course-name
					    :type "letter")))))))
	   (if (< temp1 exit-prompt)
	       (setq temp
		     (query
		      "Do you want to change another variable,or look at their values?" 
		      nil))))
	(msg T "Please type a number between 1 and " exit-prompt ": ")))))


(defgexpr gr-review
  (mhelp "Enter REVIEW to change VARIABLES."))

(defun gr-review ()
  (let ((global-subjectlist '(Gr-FileNames Gr-Misc)))
    (review)))

