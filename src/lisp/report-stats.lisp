;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :teacher)
(part-of REPORT)

(deffile report-stats
  (part-of report)
  (extension lisp)
  (mhelp "Statistics package for the REPORT program."))

(context stats)

(defreport command-table
  (source-event command)
  (eventargs command completion-status)
  (argnames output-file)
  (argtypes filespec)
  (defaultfns (lambda (output-file)
		 (cond ((eq output-file '$)
			(setq output-file "TTY:")))
		 (list output-file)))
  (passed-args num-cmds command-list)
  (begin-fn ctable-begin)
  (do-fn ctable-do)
  (end-fn ctable-end)
  (mhelp "Prints a table of command usage."))

(defun ctable-begin (output-file)
  (declare (special num-cmds command-list)
	   (ignore output-file))
  (setq num-cmds 0)
  (setq command-list nil)
  (msgf "A # indicates that 100 commands have been read." t))

(defun ctable-do (command completion-status)
  (declare (special num-cmds command-list))
  (cond ((not (symbolp command)) (msgf "Format Error in file."))
	(t (setq num-cmds (1+ num-cmds))
	   (when (zerop (mod num-cmds 100)) (msg "#"))
	   (let ((usage (get command 'usage)))
	     (when (null usage)
	       (push command command-list)
	       (setq usage (list (cons 'OK 0) (cons '^G 0) (cons 'DR 0)
				 (cons 'DP 0) (cons 'IR 0) (cons 'EM 0)
				 (cons 'EE 0) (cons 'EC 0) (cons 'FL 0)))
	       (putprop command usage 'usage))
	     (let ((this-usage (assoc completion-status usage)))
	       (rplacd this-usage (1+ (cdr this-usage))))))))


(defun ctable-end (output-file)
  (declare (special num-cmds command-list))
  (reroute-output output-file *default-pathname-defaults*
    (for-each cmd command-list
      (declare (special num-cmds))
      (macrolet ((tnum (name) `(cdr (assoc ,name usage))))
      (let* ((usage (get cmd 'usage))
	     (total (apply #'+ (mapcar #'cdr usage)))
	     (stotal (- total (+ (tnum 'DP) (tnum 'DR)))))
	(push (cons '%OK (floor (* 100 (tnum 'OK)) stotal))
	      usage)
	(push (cons '%ER (floor (* 100 (+ (tnum '^G) (tnum 'EM) (tnum 'EE)
				       (tnum 'EC) (tnum 'IR)))
			     stotal))
	      usage)
	(push (cons '%FL (floor (* 100 (tnum 'FL)) stotal))
	      usage)
	(push (cons 'TOTAL total) usage)
	(push (cons '%TOTAL (floor (* 100 total) num-cmds)) usage)
	(putprop cmd usage 'usage))))
    (print-usage-detail (sort (copy command-list) #'alphalessp))
    (msg t)
    (write-char #\Page)
    (msg t)
    (print-usage-detail
     (sort (copy command-list)
	   #'(lambda (cmd1 cmd2)
	       (> (cdr (assoc 'TOTAL (get cmd1 'usage)))
		  (cdr (assoc 'TOTAL (get cmd2 'usage)))))))
    (msg t)
    (write-char #\Page)
    (msg t)
    (print-usage-summary
     (sort (copy command-list)
	   #'(lambda (cmd1 cmd2)
	       (< (cdr (assoc '%OK (get cmd1 'usage)))
		  (cdr (assoc '%OK (get cmd2 'usage)))))))
    (for-each cmd command-list (remprop cmd 'usage))))

(defun print-usage-detail (cmdlist)
  (declare (special num-cmds))
  (msgf "REPORT evaluating file "
	((eval (get 'command 'write-file)) . filespec) ".")
  (msg t t)
  (msgf "A total of " num-cmds " commands issued.")
  (msg t t)
  (msgf "Legend:")
  (msgf " OK - successful")
  (msgf " ^G -  aborted with ^G")
  (msgf " DR - not restored during RESTORE-WORK or EXECUTE-FILE")
  (msgf " DP - not restored during RESTORE-WORK or EXECUTE-FILE because it's a")
  (msgf "      print command")
  (msgf " IR - not executed because command or rule is illegal for current exercise.")
  (msgf " EM - error during a MAINFN")
  (msgf " EE - error during a ENTERFN")
  (msgf " EC - error during a CLOSEFN")
  (msgf " FL - uncaught THROWFAIL, probably during LINEREADP")
  (msg t t)
  (msgf "Command" (t 20) "Total" (t 30) "OK" (t 35) "^G" (t 40) "DR" (t 45)
	"DP" (t 50) "IR" (t 55) "EM" (t 60) "EE" (t 65) "EC" (t 70) "FL")
  (for-each cmd cmdlist
    (macrolet ((tnum (name) `(cdr (assoc ,name usage))))
    (let ((usage (get cmd 'usage)))
      (msgf cmd (t 20) (tnum 'TOTAL)
	    (t 30) (tnum 'OK) (t 35) (tnum '^G) (t 40) (tnum 'DR)
	    (t 45) (tnum 'DP) (t 50) (tnum 'IR) (t 55) (tnum 'EM)
	    (t 60) (tnum 'EE) (t 65) (tnum 'EC) (t 70) (tnum 'FL))))))

(defun print-usage-summary (cmdlist)
  (declare (special num-cmds))
  (msgf "REPORT evaluating file "
	((eval (get 'command 'write-file)) . filespec) ".")
  (msg t t)
  (msgf "A total of " num-cmds " commands issued.")
  (msg t t)
  (msgf "Legend:")
  (msgf " %OK - percentage of successful completions of command.")
  (msgf " %ER - percentage of errors in command (including ^G).")
  (msgf " %FL - percentage of fails in command (not due to user).")
  (msgf " %Total - percentage of command compared to total number of commands issued.")
  (msg t t)
  (msgf "Command" (t 20) "%OK" (t 25) "%ER" (t 30) "%FL" (t 35) "%Total")
  (msg t t)
  (for-each cmd cmdlist
    (macrolet ((tnum (name) `(cdr (assoc ,name usage))))
    (let ((usage (get cmd 'usage)))
      (msgf cmd (t 20) (tnum '%OK) (t 25) (tnum '%ER) (t 30) (tnum '%FL)
	    (t 35) (tnum '%TOTAL))))))

;;;
;;; Next we defined the INPUT-ERROR report
;;;

(defreport input-error-table
  (source-event input-error)
  (eventargs argtype input)
  (argnames output-file)
  (argtypes filespec)
  (defaultfns (lambda (output-file)
		 (cond ((eq output-file '$)
			(setq output-file "TTY:")))
		 (list output-file)))
  (passed-args num-errors argtype-list)
  (begin-fn ierror-begin)
  (do-fn ierror-do)
  (end-fn ierror-end)
  (mhelp "Prints a table of input errors."))

(defun ierror-begin (output-file)
  (declare (special num-errors argtype-list)
	   (ignore output-file))
  (setq num-errors 0)
  (setq argtype-list nil)
  (msgf "A # indicates that 100 errors have been read." t))

(defun ierror-do (argtype input)
  (declare (special num-errors argtype-list)
	   (ignore input))
  (cond ((not (symbolp argtype)) (msgf "Format Error in file."))
	(t (setq num-errors (1+ num-errors))
	   (when (zerop (mod num-errors 100)) (msg "#"))
	   (let ((usage (get argtype 'usage)))
	     (when (null usage)
	       (push argtype argtype-list)
	       (setq usage (list (cons 'FAILED 0)))
	       (putprop argtype usage 'usage))
	     (let ((this-usage (assoc 'FAILED usage)))
	       (rplacd this-usage (1+ (cdr this-usage))))))))

(defun ierror-end (output-file)
  (declare (special num-errors argtype-list))
  (reroute-output output-file *default-pathname-defaults*
    (print-ierrors (sort (copy argtype-list) #'alphalessp))
    (msg t)
    (write-char #\Page)
    (msg t)
    (print-ierrors
     (sort (copy argtype-list)
	   #'(lambda (argtype1 argtype2)
	       (> (cdr (assoc 'FAILED (get argtype1 'usage)))
		  (cdr (assoc 'FAILED (get argtype2 'usage)))))))
    (for-each argtype argtype-list (remprop argtype 'usage))))


(defun print-ierrors (argtypelist)
  (declare (special num-errors))
  (msgf "REPORT evaluating file "
	((eval (get 'input-error 'write-file)) . filespec) ".")
  (msg t t)
  (msgf "A total of " num-errors " input errors occurred.")
  (msg t t)
  (msgf "Argument Type" (t 15) "Number of Errors")
  (msg t t)
  (for-each argtype argtypelist
   (macrolet ((tnum (name) `(cdr (assoc ,name usage))))
    (let ((usage (get argtype 'usage)))
      (msgf argtype (t 20) (tnum 'FAILED))))))

;;;
;;; Next we try to analyze rule errors.
;;;


;;; First the utilities.
;;; 
;;; THROW-CODE will translate a thrown list into a list where
;;; strings are replaced by unique symbols and references
;;; to argtypes by the argtype itself.
;;;

(defun throw-code (throwlist)
  (do ((throwlist throwlist (cdr throwlist))
       (codelist nil (cons (find-code (car throwlist)) codelist)))
      ((null throwlist) (nreverse codelist))))

(defun find-code (throwitem)
  (cond ((stringp throwitem)
	 (string-code throwitem))
	((consp throwitem)
	 (cond ((eq (car throwitem) 'quote)
		'Q-ITEM)
	       ((and (symbolp (cdr throwitem)) (not (null (cdr throwitem))))
		(cdr throwitem))
	       (t 'L-ITEM)))
	(t throwitem)))

(defun string-code (string)
  (declare (special code-alist))
  (cond ((assoc-enter string code-alist))
	(t (push (cons (intern (gensym)) string) code-alist)
	   (caar code-alist))))

(defun assoc-enter (string alist)
  (do ((alist alist (cdr alist)))
      ((null alist) nil)
    (when (string= string (cdar alist))
      (return (caar alist)))))

;;;
;;; Now comes the definition of the report
;;;


(defreport rule-error-table
  (source-event rule-error)
  (eventargs rule mainfn complaint)
  (argnames output-file)
  (argtypes filespec)
  (defaultfns (lambda (output-file)
		 (cond ((eq output-file '$)
			(setq output-file "TTY:")))
		 (list output-file)))
  (passed-args num-cmds rule-error-list code-alist)
  (begin-fn rerrtable-begin)
  (do-fn rerrtable-do)
  (end-fn rerrtable-end)
  (mhelp "Prints a table of rule errors."))

(defun rerrtable-begin (output-file)
  (declare (special num-cmds rule-error-list code-alist)
	   (ignore output-file))
  (setq num-cmds 0)
  (setq rule-error-list nil)
  (setq code-alist nil)
  (msgf "A # indicates that 100 rule errors have been read." t))

(defun rerrtable-do (rule mainfn complaint)
  (declare (special num-cmds rule-error-list))
  (cond ((not (symbolp rule)) (msgf "Format Error in file."))
	(t (setq num-cmds (1+ num-cmds))
	   (when (zerop (mod num-cmds 100)) (msg "#"))
	   (let ((usage (get rule 'usage))
		 (complaint-code (cons mainfn (throw-code complaint))))
	     (when (null usage)
	       (push rule rule-error-list)
	       (setq usage (list (cons 'TOTAL 0)))
	       (putprop rule usage 'usage))
	     (rplacd (car usage) (1+ (cdar usage)))
	     (let ((this-usage (assoc complaint-code usage)))
	       (if (null this-usage)
		   (rplacd usage (cons (cons complaint-code 1) (cdr usage)))
		   (rplacd this-usage (1+ (cdr this-usage)))))))))


(defun rerrtable-end (output-file)
  (declare (special num-cmds rule-error-list))
  (reroute-output output-file *default-pathname-defaults*
    (print-rerror-detail (sort (copy rule-error-list) #'alphalessp))
    (msg t)
    (write-char #\Page)
    (msg t)
    (print-rerror-detail
     (sort (copy rule-error-list)
	   #'(lambda (rule1 rule2)
	       (> (cdr (assoc 'TOTAL (get rule1 'usage)))
		  (cdr (assoc 'TOTAL (get rule2 'usage)))))))
    (for-each cmd rule-error-list (remprop cmd 'usage))))

(defun print-rerror-detail (rule-list)
  (declare (special num-cmds))
  (msgf "REPORT evaluating file "
	((eval (get 'rule-error 'write-file)) . filespec) ".")
  (msg t t)
  (msgf "A total of " num-cmds " rule errors occurred.")
  (msg t t)
  (for-each rule rule-list
    (let ((usage (get rule 'usage)))
      (msgf rule (t 20) "(" (cdr (assoc 'TOTAL usage)) ")")
      (msg t t)
      (for-each code (cdr usage)
	(msgf (t 5) (cdr code))
	(msg-code (car code))))))

(defun msg-code (codelist)
  (declare (special code-alist))
  (msg (t 10))
  (do ((codelist codelist (cdr codelist))
       (code) (codestring))
      ((null codelist))
    (setq code (car codelist))
    (setq codestring (cdr (assoc code code-alist)))
    (cond (codestring (msg codestring))
	  (t (msg code)))
    (msg " ")))
