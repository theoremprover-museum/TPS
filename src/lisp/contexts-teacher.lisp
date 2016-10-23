;;; -*- Mode:LISP; Package:TEACHER -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :teacher)
(part-of tpsdef)

(deffile contexts-teacher
  (part-of tpsdef)
  (extension lsp)
  (mhelp "Defines contexts used in the TEACHER package."))

(defcontext report-object
            (short-id "report package")
            (order 136)
            (mhelp "Objects used in the REPORT package."))

(defcontext report-examples
            (order 137)
            (short-id "Example of Report")
            (mhelp "Dealing with examples of reports."))

(defcontext stats
            (short-id "Statistics")
            (order 138)
            (mhelp "The statistics of commands, error, etc."))

(defcontext gr-a-out
  (short-id "Getting Out and Help")
  (order 139.1)
  (mhelp "Grader Commands for leaving Grader."))

(defcontext gr-b-vars
  (short-id "Variables")
  (order 139.2)
  (mhelp "Grader Command for changing values of variables."))

(defcontext gr-c-gradefile
  (short-id "The Grade-File")
  (order 139.3)
  (mhelp "Grader Command for creating Grade-File."))

(defcontext gr-d-manual-grades
  (short-id "Manual Grades")
  (order 139.4)
  (mhelp "Grader Commands for modifying grades."))

(defcontext gr-e-automatic-grades
  (short-id "Automatic Grades")
  (order 139.5)
  (mhelp "Grader Commands for collecting grades from ETPS file."))

(defcontext gr-f-class-list
  (short-id "The Class List")
  (order 139.6)
  (mhelp "Grader Commands for modifying class list."))

(defcontext gr-g-output
  (short-id "Making the Output Convenient")
  (order 139.7)
  (mhelp "Grader Commands for making the output convenient."))

(defcontext gr-h-stat
  (short-id "Generating Values")
  (order 139.8)
  (mhelp "Grader Command for calculating statistical data."))

(defcontext gr-i-display
  (short-id "Displaying Information")
  (order 139.9)
  (mhelp "Grader Commands for looking at various items in the class list."))

(defcontext gr-j-totals
  (short-id "Totaling")
  (order 140)
  (mhelp "Grader Commands for calculating grades."))

(defcontext gr-k-sorting
  (short-id "Sorting")
  (order 140.1)
  (mhelp "Grader Command for sorting grades."))

(defcontext gr-l-letter-grade
  (short-id "Letter-Grades")
  (order 140.2)
  (mhelp "Grader Command for assigning letter grades."))

(defcontext grader-object
  (short-id "Grader")
  (order 139)
  (mhelp "Objects to do with the TEACHER package."))