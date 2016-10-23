;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLNL)

;;;
;;; File: otlnl
;;; Package: otlnl
;;;
;;; defines the functions for maintaining proof outline.

(deffile otlnl
  (part-of otlnl)
  (extension clisp)
  (mhelp "Defines the functions for maintaining proof outline."))

(defun combine (list1 list2)
  (if list1 (acons (car list1) (car list2) (combine (cdr list1) (cdr list2)))
      nil))

;;;(IOTA N1 N2) returns the list of integers between N1 and N2, inclusively,
;;;   and in increasing order. Name from APL.

(defun iota (n1 n2)
  (when (> n1 n2) (cons n1 (iota (1+ n1) n2))))

(defun pline-default (pline)
  (declare (special dproof))
  (if (and (eq pline '$) (boundp 'dproof) (get dproof 'plans))
      (caar (get dproof 'plans)) pline))

(defun done ()
  (declare (special dproof))
  (cond ((proof-plans dproof)
	 (throwfail "There are still planned lines: "
		    ((current-plan-lines dproof) . existing-linelist)
		    "."))
	((not (proof-lines dproof))
	 (throwfail "The proof doesn't contain any lines!")))
  (dolist (eline (proof-lines dproof))
    (when (linealias eline) (ck-just eline (linealias eline) T))
    (cond ((not (linealias eline))
	   (setf (proof-lines dproof) (delete eline (proof-lines dproof))))
	  ((exists hyp (line-hypotheses eline)
		   (not (linealias hyp)))
	   (throwfail "A line which was a hypothesis of line "
		      (eline . line)
		      " was deleted.  You can't do that!"))
	  ((exists just (line-just-lines eline)
		   (not (linealias just)))
	   (throwfail "A line which justified line " (eline . line)
		      " was deleted.  This is no proof!"))))
  (when (theorem-p dproof)
    (cond ((not (wffeq (proof-assertion dproof)
		       (line-assertion (maxline dproof))))
	   (throwfail "What you have proven is not exercise " dproof
		      " !" t  "You should be ashamed of yourself!"))
	  ((line-hypotheses (maxline dproof))
	   (throwfail "The last line in your proof still has hypotheses."
		      t "Don't try to trick me like that!"))))
  (if (funcall (get 'exercise 'testfn) dproof)
      (do ()
	  ((signal-event 'done-exc (length (get dproof 'lines)))
	   (msgf "Score file updated."))
	(msgf "Could not write score file.  Trying again ... (abort with ^G)")
;*;	(sleep 0.5) CMU Lisp chokes on floating-point DAN 11/5/87
	(sleep 1/2))
      (msgf "You have completed the proof.  Since this is not an assigned exercise,"
	    t "the score file will not be updated."))
  (signal-event 'proof-action 'done))

(defun exercise (excno)
   (declare (special saving-work-p save-work-p again))
   (cond ((and saving-work-p (streamp save-work-output-stream))
          (prompt-read again nil 
                (princ (format nil "Would you like to store all your commands for the exercise
in a new file ~S.work?" excno))
                'yesno 'yes nil)
	  (when again
	        (finish-save)
   	        (open-save-file (string-downcase (string excno)))
	        (format save-work-output-stream "Exercise ~S~%" excno)))
	 ((and save-work-p (not saving-work-p)) 
	  (open-save-file (string-downcase (string excno)))
	  (format save-work-output-stream "Exercise ~S~%" excno)))
  (prove1 (getrwff excno) excno 100))

(defun linealias (line)
  (line-linenumber line)) 

(defun %linealias (line)
  (if (linealias line) (linealias line) (list line)))

(defun lineordering (x y) (< (linealias x) (linealias y)))

;;; It is known that the lines are ordered

(defun maxline (proof-name)
  (car (last (proof-lines proof-name))))

(defun ck-just (linelabel num throw)
  (when (remove-if #'null (line-just-lines linelabel))
    (dolist (label (line-just-lines linelabel))
      (let ((xxx (linealias label)))
	(cond ((> xxx num)
	       (if throw (throwfail "Line " xxx " which justifies line "
				    num
				    " occurs after line " num  ".")
		   (msg F "WARNING: Line " xxx " which justifies line "
			(linealias linelabel) " occurs after line "
			num ".")))
	      ((= xxx num)
	       (throwfail "You can't use line " num " to justify line "
			  num ".")))))))

(defun ck-hyp (linelabel num throw)
  (when (line-hypotheses linelabel)
    (dolist (xxx (line-hypotheses linelabel))
      (when (and (> (linealias xxx) num) (neq xxx linelabel))
	(if throw
	    (throwfail "Line "  (linealias xxx)
		       " which is a hypotheses for line "
		       (linealias linelabel) " occurs after line " num  ".")
	    (msg F "WARNING: Line "  (linealias xxx) 
		 " which is a hypotheses for line " (linealias linelabel)
		 " occurs after line " num "."))))))

(defun nextplan ()
  (PROG1 (list (intern (format nil "PLAN~S" (nextplan-no dproof))) nil nil)
	 (incf (nextplan-no dproof))))

(defun numalias (num)
  (cdr (assoc num (proof-linealiases dproof))))

(defun prove (wff prefix num)
  #+:tps(auto::timing-sethash 'prove 'auto::proof-count
			      (auto::timing-gethash 'mating 'auto::proof-count))
  (when (and (theorem-p prefix) (not (library-theorem-p prefix)))
      #-:tps(throwfail "There is already an exercise with this name!")
      (msgf "Warning: there is already an exercise with this name." t))
  (when (not (eq (unabbreviated-type wff) 'o))   ; added this check -- ceb 6/2/99
	(throwfail "Prove expects a wff of type O.
" (wff . gwff) 
		   " has type " ((unabbreviated-type wff) . typesym)))
  (prove1 (if (label-p wff) (get-gwff0 wff) wff) prefix num))

(defun remprop-lines (linelabel)
  (setf (line-assertion linelabel) nil)
  (setf (line-justification linelabel) nil)
  (setf (line-hypotheses linelabel) nil)
  (setf (line-linenumber linelabel) nil))
  
;;; nextplan-no is now a property of the proof, a macro defined in otl-macros
;;; DAN 3-23-88
;(defvar NEXTPLAN-no 0)

(defvar *dproof-list* nil)

;;;Removed gaps as a special variable -- 6/19/87 DAN

(defun prove1 (wff prefix num)
  (when (and (module-loaded-p 'library) (not maint:*dont-ask*))
    (if (symbolp wff)
	(find-mode wff nil)
      (if (and (symbolp *last-gwff-typed*)  ; added this check -- ceb 6/2/99
	       (wffeq-ab (get *last-gwff-typed* 'represents) wff))
	  (find-mode *last-gwff-typed* nil)
	(if (symbolp prefix)
	    (find-mode prefix nil)
	  ))))
  (prove2 wff prefix num))

(defun prove2 (wff prefix num)
  (let ((line (gentemp))
	(wff (if assert-rrules		; mkaminski 10/1/2005
		 (labels
		  ((insert-rules
		    (wff rrs)
		    (if (null rrs) wff
		      (let* ((rr (car rrs))
			     (lpart (get rr 'before))
			     (rpart (get rr 'after)))
			(insert-rules
			 (acons ;(inherit-abbrev 'IMPLIES '((O . O) . O) '())
			        'IMPLIES
				(acons (inherit-abbrev '= `((O . ,(type rpart)) . ,(type lpart))
						       (get rr 'rtypelist))
				       lpart rpart)
				wff)
			 (cdr rrs))))))
		  (insert-rules wff (remove-if-not 'active-p global-rewrite-rule-list)))
	       wff)))
    (setq dproof prefix)
    (setf (get dproof 'gwff-name) 
      (if (symbolp *last-gwff-typed*)
	  *last-gwff-typed*
	nil))				; cebrown - 4/12/01 - so saveproof can save the name
    (setq *dproof-list* (adjoin dproof *dproof-list*))
    (setf (proof-assertion dproof) wff)
    (when (proof-lines dproof)
      (mapc #'remprop-lines (proof-lines dproof)))
    (setf (proof-lines dproof) nil)
    (setf (proof-linealiases dproof) nil)
    (setf (proof-plans dproof) nil)
    (setf (nextplan-no dproof) 1)
    (setf (proof-linealiases dproof) (list (cons num line)))
    (setf (proof-plans dproof) (list (list line)))
    (setf (proof-lines dproof) (list line))
    (setf (line-assertion line) wff)
    (setf (line-justification line) (nextplan))
    (setf (line-linenumber line) num)
    (find-all-gaps)
    (signal-event 'proof-action 'started)
    line))

(context proof-outline)
(defmexpr prooflist
  (mhelp "Print a list of all proofs or partial proofs currently in memory.
Also prints the final line of each proof and the comment, if
any, attached to it."))

(defun prooflist ()
  (dolist (prf *dproof-list*) (msg prf " : ") (prtwff (get (maxline prf) 'assertion)) (msg t) (eval (get prf 'comment)) (msg t t)))

(defun problems (flag)
  (declare (special global-theoremlist))
  (msgf "Exercises: ") (when flag (msg t))
  (dolist (exc global-theoremlist)
	  (when (and (symbolp exc) (funcall (get 'exercise 'testfn) exc))
		(msg exc " ")
		(when flag 
		      (msg ": ") 
		      (prtwff (funcall (get 'theorem-type 'getfn) exc)
			      (curpos (+ 3 (length (princ-to-string exc)))) 
			      (leftmargin (+ 3 (length (princ-to-string exc)))))
		      (msg t))))
  (when flag (msg t)) (msgf "Practice Exercises: ") (when flag (msg t))
  (dolist (exc global-theoremlist)
	  (when (and (symbolp exc) (practice-p exc))
		(msg exc " ")
		(when flag 
		      (msg ": ") 
		      (prtwff (funcall (get 'theorem-type 'getfn) exc)
			      (curpos (+ 3 (length (princ-to-string exc))))
			      (leftmargin (+ 3 (length (princ-to-string exc)))))
		      (msg t))))
  (when flag (msg t)) (msgf "Book Theorems: ") (when flag (msg t))
  (dolist (exc global-theoremlist)
	  (when (and (symbolp exc) (book-theorem-p exc))
		(msg exc " ")
		(when flag 
		      (msg ": ") 
		      (prtwff (funcall (get 'theorem-type 'getfn) exc)
			      (curpos (+ 3 (length (princ-to-string exc)))) 
			      (leftmargin (+ 3 (length (princ-to-string exc)))))
		      (msg t)))))

#+comment(defun abbreviations ()
  (declare (special global-abbrevlist))
  (msgf "Abbreviations:" t)
  (dolist (abbrev global-abbrevlist)
    (when (symbolp abbrev)
      (msg abbrev " "))))

(defun abbreviations (flag)
  (declare (special global-abbrevlist))
  (msgf "Abbreviations:" t)
  (dolist (abbrev global-abbrevlist)
    (when (symbolp abbrev)
	  (msg abbrev " ")
	  (when flag 
		(msg ": " ((get abbrev 'defn) . gwff) t)))))

(defun reconsider (prefix)
  (declare (special dproof))
  (if (proof-lines prefix)
      (setq dproof prefix)
      (throwfail "Please use PROVE to start a new proof.")))

(context otl-entering)

(defmexpr done
  (mhelp "Signal that the current proof is complete."))

(defmexpr exercise
  (argtypes theorem)
  (argnames excno)
  (arghelp "Name of an exercise")
  (closefns %prtline)
  (mhelp "Start the proof of a new exercise."))

(defmexpr prove
  (argtypes gwff0-or-label symbol line)
  (argnames wff prefix num)
  (arghelp "Prove Wff" "Name of the Proof" "Line Number for Theorem")
  (defaultfns
    (lambda (pgwff &rest rest)
      (cons pgwff
	    (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
		    (list (if (label-p pgwff) pgwff 
			    (if (and (boundp '*last-gwff-typed*) 
				     *last-gwff-typed* (symbolp *last-gwff-typed*)) *last-gwff-typed*
			      '$)) 100) rest))))
  (closefns %prtline cstatus)
  (mhelp "Start a new proof of a given wff."))

(context help-obj)
(defmexpr problems
  (argtypes yesno)
  (argnames show-defns)
  (arghelp "Show definitions?")
  (defaultfns (lambda (show-defns) (list (if (eq show-defns '$) nil show-defns))))
  (mhelp "This command will list the names of all exercises available in ETPS."))

(defmexpr abbreviations
  (argtypes yesno)
  (argnames show-defns)
  (arghelp "Show definitions?")
  (defaultfns (lambda (show-defns) (list (if (eq show-defns '$) nil show-defns))))
  (mhelp "This command will list the names of all abbreviations available in TPS."))

(context otl-entering)
(defmexpr reconsider
  (argtypes symbol)
  (argnames prefix)
  (arghelp "Name of Proof")
  (defaultfns 
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '($) rest)))
  (mhelp ("Reconsider a proof. The following proofs are in memory:" t
(l *dproof-list*) t "For more details, use the PROOFLIST command." t t)))

;;; MAIL-REMARKS, if non-null, should hold a string consisting of
;;; userid's of the teacher or maintainer to whom any remarks should be
;;; sent via email, e.g., "pa01+@andrew.cmu.edu dn0z+@andrew.cmu.edu"

(defvar mail-remarks nil)

;;; REMARKS-FILE, if non-null, should hold a string with the name of the 
;;; file in which remarks are to be recorded, e.g., "etps.remarks".

(defvar remarks-file nil)

;;; One of the above variables should be set in TPS3.SYS or ETPS.SYS

(defmexpr remark
  (argtypes string)
  (argnames remark)
  (arghelp "Remark in Doublequotes")
  (print-command t)
  (mhelp "Send a message to the teacher or maintainer."))


(defun remark (string)
  (if mail-remarks
      (mail-remark string)
      (if remarks-file
	  (write-remarks-file string)
	  (throwfail "Unable to send a remark.  Notify teacher or maintainer."))))


(defun write-remarks-file (string)
  (let ((reroute-close-message nil))
    (reroute-output-append remarks-file *default-pathname-defaults*
       (format t "(~S ~S ~S ~S ~S)" 
	       (status-userid) dproof string
	       (status-date) (status-daytime)))))
    
(defparameter *mail-program* 
    #+:andrew "/usr/local/bin/mail"
    #-:andrew "/usr/ucb/mail"
    "A string that is the pathname of the Unix(tm) mail program to be used
when mailing a remark.")

(defun mail-remark (str)
  "Will mail a remark to the teacher or maintainers.  Very Unix-specific, uses
/usr/ucb/mail or value of *mail-program* if that file exists.  The value of 
the variable MAIL-REMARKS determines the recipients.  Returns T if it 
succeeds, else returns NIL."
     (zerop
      (call-system 
       (format nil
	       "~A -s ETPS-REMARK ~A << MAIL >/dev/null

Date: ~A
Time: ~A
Proof: ~A
~A

MAIL
"
	       ;; possible that students are using etps built on a machine 
	       ;; with a different mailer.  For example, somebody running 
	       ;; etps built under andrew from a cs machine.
	       (if (probe-file (pathname *mail-program*))
		   *mail-program*
		 "/usr/ucb/mail")
	       mail-remarks 
	       (status-date)  (status-daytime) dproof 
	       str ))))

;;; added 3 MAR89 DAN

(defun current-plan-lines (proof)
  "Returns a list of the current planned lines in the proof.  They are
NOT ordered by linenumber, but as they appear in proof-plans."
  (let ((newlist nil))
    (dolist (x (proof-plans proof) (nreverse newlist))
      (push (car x) newlist))))


(defun parse-comment (instring)
  (if (string= instring "") ""
    (let ((linenum-markers nil)
	  (gwff-markers nil))
      (do ((i (1- (length instring)) (1- i)))
	  ((< i 0))
	  (if (eq (char instring i) #\$)
	      (push i linenum-markers)
	    (when (eq (char instring i) #\#)
		  (push i gwff-markers))))
      (if (oddp (length linenum-markers))
	  (complain "This has an odd number of $ markers.")
	(if (oddp (length gwff-markers))
	    (complain t "This has an odd number of # markers.")
	  (parse-comment-2 instring 0 linenum-markers gwff-markers))))))

(defun parse-comment-2 (i point l g &optional (returning-list nil))
  ;note g and l are in increasing order.
  (if (and (null l) (null g) (>= point (1- (length i))))
      (append '(msg) returning-list)
    (progn 
    (if (and (not (null g)) (= (car g) point) (or (null l) (> (car l) (cadr g)))) 
	(progn (setq returning-list (append returning-list (parse-gwff i (1+ (car g)) (1- (cadr g)))))
	       (setq point (1+ (cadr g)))
	       (setq g (cddr g)))
      (if (and (not (null l)) (= (car l) point) (or (null g) (> (car g) (cadr l)))) 
	  (progn (setq returning-list (append returning-list (parse-linenum i (1+ (car l)) (1- (cadr l)))))
		 (setq point (1+ (cadr l)))
		 (setq l (cddr l)))
	(if (or (and (not (null l)) (= (car l) point)) (and (not (null g)) (= (car g) point)))
	    (throwfail t "The pairs of $ and # markers are interleaved.")
	  (progn 
	    (let ((newpt (if (null l)
			     (if (null g) (length i) (car g))
			   (if (null g) (car l) (min (car l) (car g))))))
	      (setq returning-list (append returning-list (parse-string i point (1- newpt))))
	      (setq point newpt))))))
    (parse-comment-2 i point l g returning-list))))

(defun parse-string (i s f)
  (do ((count f (1- count))
       (substring "" (concatenate 'string (string (char i count)) substring)))
      ((< count s) (list substring))))

(defun parse-gwff (i s f)
  (let ((substring "")
	(output nil)
	(symbols nil)
	(pointer (gensym)))
    (do ((count f (1- count)))
	((< count s))
	(setq substring (concatenate 'string (string (char i count)) substring)))
    (setq symbols (find-all-symbols (string-upcase substring)))
    (dolist (sym symbols)
	    (if (and (null output) (label-p sym)) (setq output (get sym 'represents))
	      (if (and (null output) (theorem-p sym)) (setq output (get sym 'assertion))
		(if (and (null output) (abbrev-q sym)) (setq output (get sym 'defn))))))
    (when (null output) (setq output substring))
    (unless (gwff-p output) (setq output (getwff-subtype 'gwff-p output)))
    (if output (progn (eval `(setq ,pointer ',output))
		      (list (cons pointer 'gwff)))
      (throwfail t substring "is not a gwff."))))

(defun parse-linenum (i s f)
    (do ((count f (1- count))
	 (mult 1 (* 10 mult))
	 (number 0)
	 (pointer (gensym)))
	((< count s) (if (numalias number) 
			 (progn (eval `(setq ,pointer (numalias ,number)))
				(list (cons pointer 'existing-line)))
		       (throwfail t "There is no line " number ".")))
	(let ((x (- (char-int (char i count)) 48)))
	  (if (and (>= x 0) (<= x 9))
	      (setq number (+ number (* x mult)))
	    (throwfail t (string (char i count)) " is not a number")))))

(defun comment-to-string (comment)
  (if (listp comment)
      (let ((comment (cdr comment))
	    (style 'generic))
	(eval (append '(concatenate 'string) (mapcar #'translate-comments comment))))
    ""))

(defun translate-comments (comment)
  (if (stringp comment) comment
    (let ((comment1 ""))
      (setq comment1 (with-output-to-string (*standard-output*) (eval `(msg ,comment))))
      (if (eq (cdr comment) 'gwff)
	  (concatenate 'string "#" comment1 "#")
	(concatenate 'string "$" comment1 "$")))))

(context proof-outline)
(defmexpr line-comment
  (argtypes existing-line string)
  (argnames line comment)
  (arghelp "Line" "Comment")
  (defaultfns (lambda (line comment)
		(list line (if (eq comment '$) (comment-to-string (get line 'comment)) comment))))
  (mhelp "Attach a comment to a given existing line.
The comment will be parsed for gwffs and line numbers
as follows: anything enclosed in # symbols is assumed to
be a gwff, and anything enclosed in $ symbols is assumed
to be the number of an existing line. Line numbers in 
comments will be updated as lines are moved around;
gwffs will be printed in the current STYLE.
Examples:
\"1st copy of line $5$, instantiated with #COMPOSE#\"
\"2nd copy of line $5$, instantiated with ITERATE\"
\"3rd copy of line $5$, instantiated with #a OR b#\"
(The first prints the definition of COMPOSE; the
second prints the word \"ITERATE\", and the third prints
the given gwff. If line 5 is subsequently renumbered, 
the line number will change in all these comments.)"))

(defun line-comment (line comment)
  (setf (get line 'comment) (parse-comment comment)))

(defflag print-comments
  (flagtype boolean)
  (default T)
  (subjects printing outline)
  (mhelp "If T, print the comments attached to lines
and proofs. See LINE-COMMENT and PROOF-COMMENT."))
