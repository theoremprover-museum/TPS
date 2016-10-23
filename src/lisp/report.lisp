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

;;; File: REPORT
;;; Package: REPORT
;;; Author: cpk
;;;

(deffile report
  (part-of report)
  (extension lsp)
  (mhelp "Defines the category of REPORTCMD."))

(context report-object)

(eval-when (compile load eval)
(deftype% date
  (getfn getdate)
  (testfn isdate)
  (printfn princ)
  (mhelp "a list of the form (yy mm dd)")))

(eval-when (compile load eval)
(defun unit (x) (= 1 (length x))))

(eval-when (compile load eval)
(defun divis (x y) (zerop (rem x y))))

(eval-when (compile load eval)
(defun surelist (x)
  (cond ((not (pairp x)) (list x))
	((equal (car x) 'QUOTE) (list x))
	(t x))))

(eval-when (compile load eval)
(defun getdate (lis)
  (cond ((unit lis) 
	 (cond ((atom (car lis)) lis)
	       (t (getdate (car lis)))))
	(t lis))))

(eval-when (compile load eval)
(defun isdate (lis)
  (and (= (length lis) 3)
       (let ((mon (cadr lis)) (day (caddr lis)) (yea (car lis)))
	 (and (fixp mon) (> mon 0) (> 13 mon)
	      (fixp yea) 
	      (fixp day) (> day 0) 
	      (cond ((= mon 2) (or (> 29 day) (and (= day 29) 
						    (divis yea 4))))
		    ((member mon '(4 6 9 11)) (> 31 day))
		    (t (> 32 day))))))))

(context subtoplevels)

(deftoplevel report-top
  (top-prompt-fn report-prompt)
  (command-interpreter report-interpret)
  (print-* report-print-*)
  (top-level-category reportcmd)
  (top-level-ctree report-ctree)
  (top-cmd-decode comdecode)
  (mhelp "The REPORT top-level."))

(defmexpr report
  (mhelp "Enter the REPORT package."))

(eval-when (compile load eval)
(defcategory reportcmd
  (define defreport1)
  (properties 
   (source-event single)
   (eventargs multiple)   ;; selected variables in the var-template of event
   (argnames multiple)
   (argtypes multiple)
   (arghelp multiple)
   (passed-args multiple) ;; values needed by DO-FN (init in BEGIN-FN)
   (defaultfns multiplefns)
   (begin-fn singlefn)    ;; args = argnames    
   (do-fn singlefn)       ;; args = eventargs ;; special = passed-args
   (end-fn singlefn)      ;; args = argnames
   (mhelp single))
  (global-list global-reportlist)
  (mhelp-line "report")
  (mhelp-fn princ-mhelp)
  (cat-help "A task to be done by REPORT.")))

(push (cons 'defreport 'reportcmd) global-definelist)

(defmacro defreport (report &rest lis)
  (let* ((argnames (assoc 'argnames lis))
	 (args (cdr argnames))
	 (passed-args (cdr (assoc 'passed-args lis)))
	 (argtypes (assoc 'argtypes lis))
	 (arghelp (assoc 'arghelp lis))
	 (defaultfns (assoc 'defaultfns lis))
	 (mhelp (assoc 'mhelp lis))
	 (proplist (delete nil
			   (list argnames argtypes arghelp defaultfns mhelp)))
	 (begin-fn (cadr (assoc 'begin-fn lis)))
	 (do-fn (cadr (assoc 'do-fn lis)))
	 (end-fn (cadr (assoc 'end-fn lis)))
	 (eventargs (cdr (assoc 'eventargs lis)))
	 (event (cadr (assoc 'source-event lis)))
	 (event-file (if (get event 'event) (get event 'write-file))))
    (if (and (not begin-fn) (not do-fn) (not end-fn)
	     (not event))
	`(progn
	  (defreport1 ,report ,@lis)
	  (defmexpr ,report ,@proplist))
	`(progn
	  (defreport1 ,report ,@lis)
	  (defmexpr ,report ,@proplist)
	  (defun ,report ,args
	    (let ,(mapcar #'(lambda (local) (list local nil))
			  (cons 'do-stop passed-args))
	      (declare (special do-stop ,event-file ,@passed-args))
	      (funcall #',begin-fn ,@args)
	      (with-open-file
	       (file ,event-file :direction :input)
	       (do ((inp (read file nil nil) (read file nil nil)))
		   ((or (null inp) do-stop))
		 (apply #',do-fn
			(get-evarg ',event ',eventargs inp))))
	      (funcall #',end-fn ,@args)))))))


(defvar report-ctree nil)

(defun report ()
  (%catch% (reportop)
	 (exit-inferior-top core::expand-catch-throw)))

(defun reportop ()
  (let ((top-prompt-fn #'report-prompt)
	(command-interpreter #'report-interpret)
	(print-* #'report-print-*)
	(top-level 'report-top)
	(command-ctree report-ctree))
    (declare (special top-prompt-fn command-interpreter print-*
		      top-level command-ctree))
    (secondary-top)))

(defun report-prompt (id)
  (format nil "<Rep~A>" id))

(defun report-print-* (result) (prin1 result))

(defun report-interpret (cmd)
  (declare (special expertflag))
  (cond ((null cmd) nil)
	((and (null (cdr cmd)) (atom (car cmd)))
	 (cond ((integerp (car cmd))
;;		  (setq cmd (redo-top (car cmd))) ;;; fn is obsolete - see  ../obsolete/top.save
;;		                               ;;; redefined to prevent crashing  MB 11/29/92
;;		  (%top-catch% (if (linereadp) nil
;;				   (report-interpret cmd))
;;			       nil))
		  (throwfail "Unknown command."))
	       ((get (car cmd) 'reportcmd)
		`(comdecode (quote ,cmd)))
	       ((get (car cmd) 'mexpr)
		`(comdecode (quote ,cmd)))
	       ((null expertflag)
		(throwfail "Unknown Report."))
	       ((boundp (car cmd)) (car cmd))
	       ((not (fboundp (car cmd)))
		(throwfail ";" (car cmd) " - Unbound variable."))
	       (t cmd)))
	((and expertflag (null (cdr cmd))) (car cmd))
	((get (car cmd) 'report)
	 `(comdecode (quote ,cmd)))
	((get (car cmd) 'mexpr) `(comdecode (quote ,cmd)))
	((null expertflag)
	 (throwfail "Unknown command."))
	(t cmd)))

(defun get-evarg (event args lis)
  (do ((tpl (get event 'template-names) (cdr tpl))
       (li lis (cdr li))
       (olis nil))
      ((null li) (nreverse olis))
    (if (member (car tpl) args)
	(push (car li) olis))))

(context report-examples)

(eval-when (compile load eval)
(defflag since-default
  (flagtype date)
  (default (80 1 1))
  (subjects maintain)
  (mhelp "The default for SINCE arguments to reports.")))

(defreport remark-list
  (source-event remark)
  (eventargs userid dproof remark-string date)
  (argnames since)
  (argtypes date)
  (defaultfns (lambda (since)
		(cond ((eq since '$) (setq since since-default)))
		(list since-default)))
  (passed-args since1)
  (begin-fn rem-beg)
  (do-fn rem-do)
  (end-fn rem-end)
  (mhelp "Prints list of remarks since given date."))

(defun priterate (str n)
  (dotimes (i n)
    (princ str)))

(defun rem-beg (since)		 
  (declare (special since1))		;declare your passed-args
  (msg "user" (t 10) "exercise" (t 20) "comments" t)
  (priterate '- 4) (msg (t 10))
  (priterate '- 8) (msg (t 20)) (priterate '- 8) (terpri)
  (setq since1 since))

(defun greatdate (date1 date2) ;; now > or =
  (or (null date2)
      (and date1
	   (let ((y1 (car date1)) (y2 (car date2)) 
				  (m1 (cadr date1)) (m2 (cadr date2)))
	     (if (> y1 y2) t
		 (if (= y1 y2)
		     (if (> m1 m2) t
			 (if (= m1 m2) 
			     (if (> (caddr date2) (caddr date1)) nil t)
			     nil))
		     nil))))))

(defun rem-do (userid dproof remark-string date)
  (declare (special since1))
  (if (greatdate date since1)
      (msg userid (t 10) dproof (t 20) remark-string t)))

(defun write-date (lis)
  (let ((mlis '("January" "February" "March" "April" "May"
			  "June" "July" "August" "September"
			  "October" "November" "December")))
    (msg (nth (1- (cadr lis)) mlis) " " (caddr lis) ", 19" (car lis))))

(defun rem-end (since)			;The passed-arg SINCE1 is not needed
  (msg t "Remarks made since ")		;so it is not declared
  (write-date since)
  (msg "." t))

(defun quitrep ()
  (%throw% "[Left REPORT package.]" exit-inferior-top))

(defreport quitrep
  (mhelp "Quit REPORT."))

(defreport exer-table
  (source-event done-exc)
  (eventargs userid dproof numberoflines date)
  (argtypes date)
  (argnames since)
  (defaultfns (lambda (since)
		(cond ((eq since '$) (setq since since-default)))
		(list since-default)))
  (passed-args since1 bin exerlis maxnam)
  (begin-fn exertable-beg)
  (do-fn exertable-do)
  (end-fn exertable-end)
  (mhelp "Constructs table of student performance."))

(defun exertable-beg (since)
  (declare (special since1 maxnam))	;the only non-Nil passed-args
  (setq since1 since)
  (setq maxnam 1))

(defun exertable-do (userid dproof numberoflines date)
  (declare (special since1 bin exerlis maxnam))
  (if (greatdate date since1)
      (progn
       (setq bin (cons (list userid dproof numberoflines) bin))
       (setq exerlis 
	     (if (member dproof exerlis) exerlis (cons dproof exerlis)))    
       (setq maxnam (max (flatc userid) maxnam)))))

(defun bin-sort (bi1 bi2)
  (alphalessp (cadr bi1) (cadr bi2)))

(defun cumadd (fst incr marg lis)
  (and lis
  (do ((li (cdr lis) (cdr li))
       (cur fst (+ nex incr))
       (nex (+ fst (car lis))
	    (+ nex incr (cond (li (car li)) (t nex))))
       (cumlis)
       (cumlislis))
      ((null li) (reverse
		  (cond ((> nex marg) 
			 (cons (list fst) (cons (reverse cumlis) cumlislis)))
			(t (cons (reverse (cons cur cumlis)) cumlislis)))))
    (cond ((> nex marg) 
	   (setq cumlislis (cons (reverse cumlis) cumlislis))
	   (setq cumlis (list fst))
	   (setq nex (+ fst (- nex cur)))
	   (setq cur fst))
	  (t (setq cumlis (cons cur cumlis)))))))

(defun binpt-sort (lis1 lis2)
  (if (alphalessp (car lis1) (car lis2))
      t
      (if (eq (car lis1) (car lis2))
	  (alphalessp (cadr lis1) (cadr lis2))
	  nil)))

(defun tab-and-print (lis)
  (do ((li lis (cdr li))
       (it (caar lis) (caadr li))
       (last nil it)
       (rest))
      ((endp li) (msg t)
		  (if rest (tab-and-print (nreverse rest))))
    (if (and last (eq it last)) (push (car li) rest)
	(msg (t it) (cdar li)))))

(defun get-corr (index indlis getlis)
  (do ((indlis indlis (cdr indlis))
       (getlis getlis (cdr getlis)))
      ((or (endp indlis) (endp getlis) (eq index (car indlis)))
       (if indlis
	   (if getlis (car getlis) (throwfail "GETLIST too short"))
	   (if getlis (throwfail "INDEXLIST too short")
	       (throwfail "INDEX not on INDEXLIST"))))))

(defun make-table (head-subjects bin maxnam) ;;assumes col-heads > entries
  (let* ((subjs (sort head-subjects 'alphalessp))
	 (subsizs (mapcar #'(lambda (x) (flatc x)) subjs))
	 ;; Previous line using LAMBDA because flatc is a macro
	 (colpos (cumadd (+ 4 maxnam) 2 (linewidth) subsizs))
	 (blis (sort bin 'bin-sort)))
    (do ((colis colpos (cdr colis))
	 (sbj nil nil) (nmlis nil nil) (binpt nil nil)
	 (cols (car colpos) (cadr colis)))
	((endp colis))
      (for-each
       num cols
       (let ((sb (car subjs)))
	 (push sb sbj) (pop subjs) num
	 (do ((bl (car blis) (car blis)))
	     ((or (endp blis) (not (equal sb (cadr bl)))))
	   (push bl binpt) (pop blis)
	   (if (member (car bl) nmlis) nil (push (car bl) nmlis)))))
      (setq nmlis (sort nmlis 'alphalessp))
      (setq binpt (sort binpt 'binpt-sort))
      (setq sbj (sort sbj 'alphalessp))
;;; Heading of the subtable
      (msg t t (t (1+ maxnam)) '\|)
      (do ((cls cols (cdr cls))
	   (sbs sbj (cdr sbs)))
	  ((endp cls) (msg t))
	(msg (t (car cls)) (car sbs)))
;;; Body of subtable
      (for-each nam nmlis
		(msg nam (t (1+ maxnam)) '\|)
		(do ((bn (car binpt) (car binpt))
		     (last nil (cadr bn))
		     (restlis))
		    ((not (equal nam (car bn))) (msg t)
		     (tab-and-print (nreverse restlis)))
		  (pop binpt)
		  (let* ((sj (cadr bn))
			 (ent (caddr bn))
			 (tab-ent (+ (- (flatc sj) (flatc ent))
				     (get-corr sj sbj cols))))
		  (if (equal last sj)
		      (push (cons tab-ent ent) restlis)
		      (msg (t tab-ent) ent))))))))

(defun exertable-end (since)
  (declare (special bin exerlis maxnam))
  (if bin 
      (progn
       (make-table exerlis bin maxnam)
       (msg t "On exercises completed since ")
       (write-date since)
       (msg "." t))
      (progn
       (msg t "No exercises completed since ")
       (write-date since)
       (msg "." t))))

(defreport security
  (source-event done-exc)
  (eventargs userid dproof numberoflines computed-code date daytime)
  (argtypes date)
  (argnames since)
  (defaultfns (lambda (since)
		(cond ((eq since '$) (setq since since-default)))
		(list since-default)))
  (passed-args since1)
  (begin-fn secure-beg)
  (do-fn secure-do)
  (end-fn secure-end)
  (mhelp "Checks for security violations."))

(defun secure-beg (since) 
  (declare (special since1))
  (msg t "Security status since ")
  (write-date since)
  (msg " :" t)
  (setq since1 since))

(defun secure-do (userid dproof numberoflines computed-code date daytime)
  (declare (special since1))
  (if (greatdate date since1)
      (if (= computed-code
	     (code-list (list userid dproof numberoflines 0 date daytime)))
	  t
	  (msg t "Security violation by " userid " on exercise " dproof))))

(defun secure-end (since)
  (declare (ignore since))
  (msgf "No (more) security violations.")
  (msg t))
