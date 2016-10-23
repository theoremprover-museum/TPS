;;; -*- Mode:LISP; Package:TEACHER -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
;;;
;;; File Gr-Macros
;;;

(in-package :teacher)
(part-of grader)

(context grader-object)

(deffile gr-macros
  (part-of grader)
  (extension lsp)
  (mhelp "Macro file for the grading package."))

(deftoplevel grade-top
  (top-prompt-fn grade-top-prompt)
  (command-interpreter grade-command-interpreter)
  (top-level-category gexpr)
  (print-* grade-print-*)
  (top-level-ctree grade-command-ctree)
  (top-cmd-decode comdecode)
  (mhelp "The top level of the GRADING PACKAGE."))

(defcategory gexpr
  (define defgexpr)
  (properties
   (argtypes multiple)
   (argnames multiple)
   (arghelp multiple)
   (mainfns multiplefns)
   (print-command single)
   (dont-restore single)
   (mhelp single))
  (global-list global-gexprlist)
  (shadow t)
  (mhelp-line "Grader Command")
  (mhelp-fn (command-mhelp gexpr '"<Gn>" category)))

(eval-when (load compile eval)
(deftype% consp1
  (getfn testfn)
  (testfn listp)
  (printfn princ)
  (mhelp "A list."))

(deftype% function
  (getfn testfn)
  (testfn fboundp)
  (printfn princ)
  (mhelp "A function."))
)

(defflag patch-file
  (flagtype filespec)
  (default "grader.patch")
  (subjects gr-filenames)
  (mhelp "Name of the file containing changes to the grader core image."))

(defflag grade-dir
  (flagtype string)
  (default "")
  (change-fn (lambda (a b c) (declare (ignore a c) (special gradefileflag))
	       (setq grade-dir b) (update-filenames) (setq gradefileflag nil) (initialize1)))
  (subjects gr-filenames)
  (mhelp "Name of the directory in which the grader files are to be found,
or \"\" for the directory from which grader was started. This name should 
end with a backslash, as in : \"/usr/teacher/course-grades/\".
When this flag is changed, all of the other filenames will change with it.
Note that in old versions of CMU lisp, the \"\" option will not work properly."))

(defflag grade-file
  (flagtype filespec)
  (default "")
  (change-fn (lambda (a b c)
	       (declare (ignore a c))
	       (setq grade-file (merge-pathnames grade-dir b)) (initialize1)))
  (subjects gr-filenames)
  (mhelp "Name of the GRADE-FILE."))

(defflag old-grade-file
  (flagtype filespec)
  (default "")
  (change-fn (lambda (a b c) (declare (ignore a c)) (setq old-grade-file (merge-pathnames grade-dir b))))
  (subjects gr-filenames)
  (mhelp "Name of the back-up GRADE-FILE."))

(defflag totals-grade-file
  (flagtype filespec)
  (default "")
  (change-fn (lambda (a b c) (declare (ignore a c) (special finalfileflag))
		     (setq totals-grade-file (merge-pathnames grade-dir b)) (setq finalfileflag nil)))
  (subjects gr-filenames)
  (mhelp "Name of the file which will contain totals."))

(defflag old-totals-grade-file
  (flagtype filespec)
  (default "")
  (change-fn (lambda (a b c) (declare (ignore a c)) (setq old-totals-grade-file (merge-pathnames grade-dir b))))
  (subjects gr-filenames)
  (mhelp "Name of the back-up TOTALS-GRADE-FILE ."))

(defflag letter-grade-file
  (flagtype filespec)
  (default "")
  (change-fn (lambda (a b c) (declare (ignore a c)) (setq letter-grade-file (merge-pathnames grade-dir b))))
  (subjects gr-filenames)
  (mhelp "Name of the file which will contain letter grades."))

(defflag  etps-file
  (flagtype filespec)
  (default "")
  (change-fn (lambda (a b c) (declare (ignore a c)) (setq etps-file (merge-pathnames grade-dir b))))
  (subjects gr-filenames)
  (mhelp "Name of the file which contains ETPS records."))

(defflag  course-name
  (flagtype string)
  (default "course")
  (change-fn (lambda (a b c)
	       (declare (ignore a c) (special listofchanges namelist))
	       (setq course-name (if (symbolp b)
				     (string-downcase (symbol-name b)) b))
	       (update-filenames)(initialize1)
	       (setq listofchanges (cdr namelist))))
  (subjects gr-misc)
  (mhelp "Name of the course. Also used as a suffix for various
	 files which are created or modified by the grading package."))

(defflag statistical-options
  (flagtype consp1) 
  (default (-mean- -median- -sdev-))
  (change-fn (lambda (a b c)
	       (declare (ignore a c) (special listofchanges namelist))
	       (dolist (option b)
		       (if (not (member option '(-mean- -median- -sdev-)))
			(throwfail "Illegal option " option ".")))
	       (setq listofchanges (cdr namelist))))
  (subjects gr-misc)
  (mhelp "List of statistical data to be calculated. Currently the
	 program can calculate mean, median , standard deviation.
	 The default is (-mean- -median- -sdev-)."))

(defflag print-n-digits
  (flagtype integer+)
  (default 0)
  (change-fn (lambda (a b c)
	       (declare (ignore a b c) (special listofchanges namelist))
	       (setq listofchanges (append listofchanges
					   (cdr namelist)))))
  (subjects gr-misc)
  (mhelp "The number of digits to be printed after the decimal."))

(defflag cal-percentage
  (flagtype boolean)
  (default nil)
  (subjects gr-misc)
  (mhelp "The program calculates percentage based on total scores if the
	 value of this variable is T."))

(defflag letter-grade-flag
  (flagtype boolean)
  (default T)
  (subjects gr-misc)
  (mhelp "The program creates a separate file containing letter grades
	 if the value of this variable is true."))

(defflag new-item
  (flagtype consp1) 
  (default nil)
  (subjects gr-misc)
  (mhelp "The list of new items to be calculated when calculating
	 totals. See the manual for more details."))

(defflag drop-min
  (flagtype consp1)
  (default nil)
  (subjects gr-misc)
  (mhelp "When calculating totals, the program drops the minimum scores
	 on each of the items in this list."))

(defflag due-date-flag
  (flagtype boolean)
  (default T)
  (subjects gr-misc)
  (mhelp "If this flag is nil, the user is not prompted for due dates (in the command
ETPS-GRADE) and it's assumed that all exercises were submitted in time."))

(eval-when (load compile eval)
(defun no-penalty (score dayslate max)
  (declare (ignore dayslate max))
  score)
)

(defflag default-penalty-fn
  (flagtype function)
  (default no-penalty)
  (subjects gr-misc)
  (mhelp "Default penalty function for late exercises. The default is no-penalty which
 doesn't take any points off."))

(defmacro linereadp-s ()
  `(linereadp))

(defmacro linereadp-e ()
  `(linereadp))

(defvar eof-value control-d-char)
(defvar numeric-eof-value control-d-char)
