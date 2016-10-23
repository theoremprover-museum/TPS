;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
;;;

(deffile library3
  (part-of library)
  (extension lisp)
  (mhelp "Defines library keywords and best modes."))

(context lib-keys)

(deflibrary add-keyword
  (lib-argnames keyword defn)
  (lib-argtypes symbol string)
  (lib-arghelp "New keyword" "Brief description")
  (lib-mainfns add-keyword)
  (mhelp "Add a keyword to the keywords.rec file in your default directory.
This must be done before the keyword can be used anywhere else in the 
library."))

(defun add-keyword (keyword string)
  (msgf "The current keyword list is: " t (mapcar 'car lib-keyword-list))
  (if (member keyword (mapcar 'car lib-keyword-list))
      (msgf "...which already contains the keyword " keyword t)
    (when (query (format nil "Are you sure you want to add the keyword ~A?" keyword) t)
      (push (cons keyword (concatenate 'string "\"" string "\"")) lib-keyword-list)
      (dolist (direc default-lib-dir)
	(let ((q (make-pathname% :directory direc :name (namestring (pathname-name lib-keyword-file))
				 :type default-libindex-type)))
	  (when (probe-file q) (delete-file q))
	  (reroute-output q *default-pathname-defaults* (msg lib-keyword-list t)))))))
    
(deflibrary show-keywords
  (mhelp "List all of the current acceptable keywords for the library."))

(defun show-keywords ()
  (msg t)
  (dolist (elt lib-keyword-list)
    (msgf (car elt) " : " (cdr elt)))
  (msg t))

(deflibrary change-keywords
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Library entry to be changed")
  (mhelp "Change the keywords attribute of a stored library object.
NOTE: not all keywords can be changed. TPS may modify your list of
keywords -- for example, if you specify FIRST-ORDER for a problem
that is higher-order, TPS will change it."))

(defun change-keywords (name)
  (let* ((found (locate-item name :type nil :writeable t))
	 (count 0) keywords)
    (unless found (throwfail "No such entry exists in your library." t))
    (when found
          (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-msg))
	  (complain (length found) " items called " name " found. They are as follows:" t)
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	  (dolist (elt found)
		  (incf count) 
		  (complain count ") MODIFY " (car elt) " in file " (pfile (namestring (cdr elt)))))
	  (incf count) 
	  (complain count ") DO NOTHING.")
	  (setq count (1- (get-a-number count)))
	  (setq found (nth count found)))
    (when found
	  (let ((old-value (retrieve-item name :type nil :writeable t :multiple nil 
					 :preferred-dir (cdr found))))
	    (when (libitem-keywords old-value)
	      (msgf name " has keywords:" (libitem-keywords old-value)))
	    (msgf t "Acceptable keywords are: " (mapcar 'car lib-keyword-list) t t )
	    (setq keywords (input-keywords old-value))
	    (let ((item (make-libitem
			 :name name :type (libitem-type old-value) :description (libitem-description old-value)
			 :mhelp (libitem-mhelp old-value) :context (libitem-context old-value)
			 :provability (libitem-provability old-value)
			 :proof-date (libitem-proof-date old-value)
			 :keywords keywords
			 :other-attributes (libitem-other-attributes old-value)
			 :needed-objects (libitem-needed-objects old-value)
			 :other-remarks (libitem-other-remarks old-value)
			 :file (namestring (cdr found)))))
	      (update-libitem item))))))

(defun input-keywords (libitem)
  (let (dummy)
    (prompt-read dummy nil (msgf "Enter a list of keywords: ") 'symbollist (libitem-keywords libitem)
		 ((? (show-keywords))))
    (when (member (libitem-type libitem) '(abbr gwff))
      (setq dummy (get-keywords-for libitem dummy))
      (msgf "The list of keywords for this library object is now: " t dummy t))
    (setq dummy (remove-if-not #'keyword-p dummy))
    dummy))

(defun input-keywords-2 (libitem)
  ;;as input-keywords, but without input or output
  (let ((dummy (libitem-keywords libitem)))
    (when (member (libitem-type libitem) '(abbr gwff))
      (setq dummy (get-keywords-for libitem dummy)))
    (setq dummy (remove-if-not #'keyword-p dummy))
    dummy))

(defun keyword-p (word) (member word (mapcar 'car lib-keyword-list)))

(defun get-keywords-for (libitem list)
  (if (module-loaded-p 'auto-basic)
      (get-keywords-for-real libitem list)
    list))

(defun get-keywords-for-real (libitem list)
  (let ((auto::rewrite-defns '(EAGER))
	(gwff (libitem-description libitem))
	(prov (libitem-provability libitem))
	(rewrite-equalities 'ALL)
	(retlist (remove-if 
		  ;;you aren't allowed to change the following entries
		  #'(lambda (x) (member x '(SK-FIRST-ORDER SK-HIGHER-ORDER FIRST-ORDER HIGHER-ORDER AUTO-PROOF 
							   NO-AUTO-PROOF WITH-DEFN WITHOUT-DEFN WITH-EQUALITY
							   WITHOUT-EQUALITY PROVEN UNPROVEN) :test 'equal))
		  list)))
    (when (stringp gwff) (setq gwff (gettype 'gwff gwff)))
    (if (first-order-check gwff) 
	(progn (push 'SK-FIRST-ORDER retlist) 
	       (if (true-fo-check (eliminate-defns gwff)) (push 'FIRST-ORDER retlist) (push 'HIGHER-ORDER retlist)))
      (progn (push 'SK-HIGHER-ORDER retlist) (push 'HIGHER-ORDER retlist)))
    (if (member prov '("Successful mode for automatic proof found automatically."
		       "Automatic expansion proof and translation to natural deduction"
		       "Automatic expansion proof"
		       "Semi-automatic expansion proof in MATE top level")
		:test 'string=)
	(progn (push 'AUTO-PROOF retlist) (push 'PROVEN retlist))
      (progn (push 'NO-AUTO-PROOF retlist)
	     (if (proven libitem) (push 'PROVEN retlist) (push 'UNPROVEN retlist))))
    (if (auto::contains-defn-not-equiv gwff) (push 'WITH-DEFN retlist) (push 'WITHOUT-DEFN retlist))
    (if (auto::contains-equality-eventually gwff) (push 'WITH-EQUALITY retlist) (push 'WITHOUT-EQUALITY retlist))
    retlist))

(deflibrary update-keywords
  (mhelp "For each library entry, update the keywords field
to include all of those keywords that can be determined automatically.
Any other keywords will be left untouched.
If you answer NO to the question about checking existing keywords, then 
this command will just attempt to fill in keywords for those objects 
which have none. If you answer YES, keywords will be generated for all 
of the objects (but existing user-defined  keywords will not be overwritten).

This command will almost certainly crash if it discovers any untypable 
definitions, missing needed-objects, circular definitions, misprints, etc...
in your library.
This probably won't damage your library, but you might want to make
a backup of all your files before you call this, just in case..."))

(defun update-keywords ()
  ;;to change this, if we have more automatically-determined keywords, just change
  ;;get-keywords-for-real and input-keywords-2 (which currently only test gwffs/abbrs)
  (msgf t t "This command will almost certainly crash if it discovers any untypable
definitions, missing needed-objects, circular definitions, misprints, etc...
in your library.
This almost certainly won't damage your library, but you might want to make
a backup of all your files before you call this, just in case..." t t)
  (when (query "Continue? " nil)
    (let ((objlist nil)
	  (overwrite (query "Do you want to check all existing keywords?" nil))
	  (writeable (mapcar 'directory-namestring default-lib-dir))
	  (ga-list global-abbrevlist)
	  libitem keywords failed-list)
      (maphash #'(lambda (key val)
		   (when key (dolist (elt val) (setq objlist (cons (list key elt) objlist)))))
	       *lib-masterindex*)
      (setq objlist (remove-if-not #'(lambda (x) (member (directory-namestring (cdadr x)) writeable :test 'string=)) objlist))
      (dolist (x objlist)
	(msg (car x) "  ")
	(setq libitem (retrieve-item (car x) :type (caadr x) :multiple nil :writeable t
				     :preferred-dir (directory-namestring (cdadr x)) :fail-gently t))
	(unless libitem (push (car x) failed-list))
	(if (and libitem (or overwrite (null (libitem-keywords libitem))))
	    (progn (setq keywords (input-keywords-2 libitem))
		   (when (or (neq (length keywords) (length (libitem-keywords libitem)))
			     (setdiff keywords (libitem-keywords libitem))
			     (setdiff (libitem-keywords libitem) keywords))
		     (setf (libitem-keywords libitem) keywords)
		     (update-libitem libitem)
		     (dolist (a (setdiff global-abbrevlist ga-list))
		       (unless (listp a)
			 (destroy-libobject-quiet a))))) ;because we don't want multiple definitions to get confused
	  (unless libitem (msg "Couldn't find " (car x) ". Ignoring it."))))
      (when failed-list (msg t t "Failed on the following: " t failed-list)))))

(defun destroy-libobject-quiet (name)
  (declare (special global-theory-list global-rewrite-rule-list))
  (unless (object-present name nil)
	  (throwfail "This object is not currently loaded in TPS."))
  (dolist (prop (plist name))
	  (remprop name prop))
  (setq global-abbrevlist (delete name global-abbrevlist))
  (when (boundp 'global-theory-list) (setq global-theory-list (delete name global-theory-list)))
  (setq global-modelist (delete name global-modelist))
  (when (boundp 'global-rewrite-rule-list) (setq global-rewrite-rule-list (delete name global-rewrite-rule-list)))
  (setq global-logconstlist (delete name global-logconstlist)) 
  (setq global-pmpropsymlist (delete name global-pmpropsymlist))
  (when (find-package 'auto)
	(setq auto::*global-searchlist* (delete name auto::*global-searchlist*))))

(defun stuff-bestmodes-into-hashtable (bestmodes &optional (mine nil))
  (dolist (b bestmodes)
    (let ((new (append (cdr b) (list mine))))
      (setf (gethash (car b) lib-bestmodes) 
	    (sort (adjoin new (gethash (car b) lib-bestmodes) :test #'equal) ; removing duplicates - cebrown 5/22/03
		  #'< :key #'caddr)))))

(defun output-bestmodes-to-file ()
  (let ((reroute-close-message nil)
	objlist file temp-file)
    (declare (special reroute-close-message))
    (maphash #'(lambda (key val)
		 (when key (setq objlist (append (mapcar #'(lambda (c) (cons key c)) val) objlist))))
	     lib-bestmodes)
    (setq objlist (sort objlist #'string< :key #'car))
    (setq file (make-pathname% :directory (car default-lib-dir)
			       :name (pathname-name lib-bestmode-file)
			       :type default-libindex-type))
    (setq temp-file (make-pathname% :directory (car default-lib-dir)
				    :name (pathname-name lib-bestmode-file)
				    :type "tmp"))
    (when (probe-file temp-file) (delete-file temp-file))
    (when (probe-file file) (rename-file file temp-file))
    (unwind-protect
	(progn (reroute-output file
			       *default-pathname-defaults*
			       (msgf "(" t) 
			       (dolist (o objlist) 
				 (let* ((newo (reverse o))
					(print-this (car newo)))
				   (when print-this 
				     (format t "~S~%" (reverse (cdr newo))))))
			       (msgf ")" t))
	       (when (probe-file temp-file) (delete-file temp-file)))
      (when (probe-file temp-file) ;if the temp file is still there, we didn't finish the above
	(when (probe-file file) (delete-file file))
	(rename-file temp-file file)))))

(defun shuffle-backups ()
  (let ((reroute-close-message nil)
	(file (make-pathname% :directory (car default-lib-dir)
			      :name (pathname-name lib-bestmode-file)
			      :type default-libindex-type))
	(bk1 (make-pathname% :directory (car default-lib-dir)
			     :name (pathname-name lib-bestmode-file)
			     :type "bk1"))
	(bk2 (make-pathname% :directory (car default-lib-dir)
			     :name (pathname-name lib-bestmode-file)
			     :type "bk2"))
	(bk3 (make-pathname% :directory (car default-lib-dir)
			     :name (pathname-name lib-bestmode-file)
			     :type "bk3"))
	(bk4 (make-pathname% :directory (car default-lib-dir)
			     :name (pathname-name lib-bestmode-file)
			     :type "bk4"))
	(bk5 (make-pathname% :directory (car default-lib-dir)
			     :name (pathname-name lib-bestmode-file)
			     :type "bk5")))
    (when (and (probe-file file) ;;main file is there
	       (or (and (probe-file bk1) (different-files file bk1)) ;;and either first backup is different...
		   (not (probe-file bk1))))                           ;;...or first backup is absent.
      (when (probe-file bk5) (delete-file bk5))
      (when (probe-file bk4) (rename-file bk4 bk5))
      (when (probe-file bk3) (rename-file bk3 bk4))
      (when (probe-file bk2) (rename-file bk2 bk3))
      (when (probe-file bk1) (rename-file bk1 bk2))
      (when (probe-file file) (rename-file file bk1))
      (output-bestmodes-to-file)) ;;this plus the line above has the effect of copying the main file
    ))

(defun different-files (file1 file2)
  (let (file1-contains file2-contains)
    (with-open-file (*input-stream* file1 :direction :input)
		    (do ((item nil (read *input-stream* nil control-d-char)))
			((eq item control-d-char))
		      (when item 
			(push item file1-contains))))
    (with-open-file (*input-stream* file2 :direction :input)
		    (do ((item nil (read *input-stream* nil control-d-char)))
			((eq item control-d-char))
		      (when item 
			(push item file2-contains))))
    ;;just test for same length...
    (not (= (length (princ-to-string file1-contains)) (length (princ-to-string file2-contains))))))

(context lib-modes)

(defun output-today ()
  (multiple-value-bind (second minute hour date month year
			       day-of-week daylight-saving-time-p time-zone)
		       (get-decoded-time)
    (declare (ignore daylight-saving-time-p time-zone second minute hour day-of-week))
    (+ (* year 10000) (* month 100) date)))

(deftype% short-date
  (getfn get-short-date)
  (testfn is-short-date-p)
  (printfn princ-date)
  (mhelp "A valid date, in the form YYYYMMDD. YYYY must be >= 1900. Any
non-integer charactes will be ignored (so 1999-04-12, 1999/04/12 and 19990412
are all considered the same, and are all valid)."))

(defun princ-date (x)
  (let* ((year (floor (/ x 10000)))
	 (month (floor (/ (- x (* year 10000)) 100)))
	 (day (- (- x (* year 10000)) (* month 100))))
    (msg year "-")
    (when (< month 10) (msg "0"))
    (msg month "-")
    (when (< day 10) (msg "0"))
    (msg day)))

(defun get-short-date (x)
  (let ((stringval (princ-to-string x))
	(lookup-table (list (cons (char-int #\0) 0) (cons (char-int #\1) 1) (cons (char-int #\2) 2)
			    (cons (char-int #\3) 3) (cons (char-int #\4) 4) (cons (char-int #\5) 5)
			    (cons (char-int #\6) 6) (cons (char-int #\7) 7) (cons (char-int #\8) 8)
			    (cons (char-int #\9) 9)))
	;;there are less ugly ways to do that in ASCII, but don't want to assume anything about character set.
	current)
    (do ((i 0 (+ 1 i))
	 (retval 0))
	((= i (length stringval)) (or (is-short-date-p retval) (throwfail "Not a valid date!")))
      (setq current (char-int (aref stringval i)))
      (when (assoc current lookup-table)
	(setq retval (+ (* 10 retval) (cdr (assoc current lookup-table))))))))
	
(defun is-short-date-p (x)
  (and (integerp x)
       (integer-short-date-p x)))

(defun integer-short-date-p (x)
  (let* ((year (floor (/ x 10000)))
	 (month (floor (/ (- x (* year 10000)) 100)))
	 (day (- (- x (* year 10000)) (* month 100))))
    (cond ((< year 1900) nil)                                    ;; Year >= 1900; prevents dates like 990429.
	  ((or (< month 1) (> month 12)) nil)                    ;; Month must be between 1 and 12.
	  ((< day 1) nil)                                        ;; Day must be at least 1.
	  ((and (memq month '(1 3 5 7 8 10 12)) (> day 31)) nil) ;; this month only has 31 days.
	  ((and (memq month '(4 6 9 11)) (> day 30)) nil)        ;; this month only has 30 days.
	  ((and (eq month 2) (or (> day 29)                      ;; february can't have > 29 days...
				 (and (= day 29)                          ;; ...and if it does have 29...
				      (not (and (= (mod year 4) 0)        ;; ...then the year is divisible by 4...
						(or (neq (mod year 100) 0)         ;; ...and not by 100...
						    (= (mod year 400) 0)))))))     ;; ...unless also by 400.
	   nil)
	  (t x)))) ;; otherwise, we have a valid date.

(defmexpr moderec
  (mhelp "Attempts to create an entry in bestmodes.rec, in a similar way to 
the way that DATEREC works."))

(defun moderec (&optional (noprompt nil))
  (let ((theorem (if (theorem-p dproof) dproof *last-gwff-typed*))
	(mode *lastmode*)
	(date (output-today))
	(time (get-rough-time-estimate))
	(comment ""))
    (update-bestmodes-file) ;; just in case we're running 2 core images...
    (if noprompt
	(add-bestmode-2 theorem mode date time comment nil)
      (progn
	(msgf "About to create a bestmodes.rec entry as follows: " t t theorem " : ")
	(print-bestmodes-nicely (list (list mode date time comment t)) (+ 3 (length (princ-to-string theorem))))
	(msgf t t "LAST-MODE-NAME is " last-mode-name t "Make sure this corresponds with the value above!" t t)
	(when (query "Continue?" t)
	  (add-bestmode-2 theorem mode date time comment nil
			  (query "Should this mode be used for testing automatic search?" t)))))))
  
(deflibrary add-bestmode
  (lib-argnames theorem mode date time comment auto-test)
  (lib-argtypes symbol symbol short-date integer+ string boolean)
  (lib-arghelp "Theorem" "Mode" "Date [YYYY-MM-DD]" "Time for proof, in seconds" "Comment"
	       "Use this mode for testing (see TPS-TEST and TEST-INIT)")
  (lib-defaultfns 
   (lambda (thm mode date time comment auto-test)
     (list thm mode 
	   (if (eq date '$) (output-today) date) time (if (eq comment '$) "" comment)
	   (if (eq auto-test '$) T auto-test))))
  (lib-mainfns add-bestmode)
  (mhelp "Add a mode for the specified theorem to the list in your
bestmodes.rec file. If the theorem and mode are already present in 
the list (either in your directory or in another user's), you will
be asked to confirm the creation of a new entry. If they are already 
present in your own directory, you will be given the option of 
overwriting them.

The TEST-INIT command sets the flag TEST-THEOREMS to a collection
of theorems associated with bestmodes.  TPS-TEST uses this list
to perform automatic testing.  ADD-BESTMODE gives you the option 
(using the argument AUTO-TEST) of having TEST-INIT include the 
new theorem/bestmode pair for automatic testing.  (The default 
is to include it.)  If the mode is intended to be used interactively 
(e.g., for a demo), then it should not be included for automatic testing.

See Also: TPS-TEST, TEST-INIT, TEST-THEOREMS"))

(defun add-bestmode (theorem mode date time comment &optional auto-test)
  (update-bestmodes-file) ;;just in case we're running 2 core images...
  (let* ((existing-elt (remove-if-not #'(lambda (x) (string= (car x) mode)) (gethash theorem lib-bestmodes)))
	 (is-mine (when existing-elt (remove-if #'(lambda (x) (null (car (last x)))) existing-elt)))
	 (overwrite nil))
    (if existing-elt
	(progn 
	  (msgf "This theorem and mode is already recorded in "
		(if is-mine "your" "another user's") " bestmodes file.")
	  (when (query "Do you still want to add it to your file?" nil)
	    (when (and is-mine 
		       (query "Overwrite the existing record?" nil))
	      (setq overwrite t))
	    (add-bestmode-2 theorem mode date time comment overwrite auto-test)))
      (add-bestmode-2 theorem mode date time comment overwrite auto-test))))

(defun add-bestmode-2 (theorem mode date time comment overwrite &optional auto-test)
  (let ((existing-value (gethash theorem lib-bestmodes))
	newval)
    (when overwrite
      (dolist (e existing-value (setq existing-value (nreverse newval)))
	(unless (and (string= (car e) mode)
		     (car (last e)))
	  (push e newval))))
    (push (if auto-test 
	      (list mode date time comment t)
	    (list mode date time (list comment 'dont-auto-test) t))
	  existing-value)
    (setf (gethash theorem lib-bestmodes) (sort existing-value #'< :key #'caddr))
    (output-bestmodes-to-file)))

(deflibrary modify-bestmode
  (lib-argnames theorem)
  (lib-argtypes symbol)
  (lib-arghelp "Theorem")
  (lib-mainfns modify-bestmode)
  (mhelp "Edit an existing entry in the bestmodes.rec file. 
Attempting to modify a read-only mode (i.e. one in another user's
directory) will create a modified copy in your own directory."))

(deflibrary delete-bestmode
  (lib-argnames theorem)
  (lib-argtypes symbol)
  (lib-arghelp "Theorem")
  (lib-mainfns delete-bestmode)
  (mhelp "Remove an existing entry in your own bestmodes.rec file.
Attempting to remove an entry in another user's bestmode.rec
file will fail."))

(defun modify-bestmode (thm)
  (update-bestmodes-file)
  (let ((modes (gethash thm lib-bestmodes))
	(count 0)
	mode newdate newtime newcomment new-auto-test)
    (if modes 
	(progn
          (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-msg))
	  (if (> (length modes) 1)
	      (complain (length modes) " modes for this theorem are known:" t)
	    (complain "Only one mode for this theorem is known:" t))
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	  (dolist (elt modes)
	    (incf count) 
	    (complain count ") " (print-bestmodes-nicely (list elt) 4)))
	  (incf count) 
	  (complain count ") None of these.")
	  (setq mode (nth (1- (get-a-number count)) modes))
	  (when mode
	    (prompt-read newdate nil (msgf "New date") 'short-date (cadr mode) ((? (msgf "Date in YYYY-MM-DD form"))))
	    (prompt-read newtime nil (msgf "New time, in seconds") 'integer+ (caddr mode) nil)
	    (prompt-read newcomment nil (msgf "New comment") 'string (cadddr mode) nil)
	    (setq new-auto-test (query "Should this mode be used for testing automatic search?" (not (nth 4 mode))))
	    (when (cadr (cdddr mode))
	      (setf (gethash thm lib-bestmodes)
		    (remove-bestmode mode (gethash thm lib-bestmodes))))
	    (setf (gethash thm lib-bestmodes)
		  (sort (cons (list (car mode) newdate newtime
				    (if new-auto-test
					newcomment
				      (list newcomment 'dont-auto-test))
				    (cadr (cdddr mode)))
			      (gethash thm lib-bestmodes))
			#'< :key #'caddr))
	    (output-bestmodes-to-file)))
      (msgf "There are no recorded modes for this theorem."))))

(defun delete-bestmode (thm)
  (update-bestmodes-file)
  (let* ((modes (gethash thm lib-bestmodes))
	 (count 0)
	 (ro-modes (remove-if #'(lambda (x) (car (last x))) modes))
	 (rw-modes (remove-if-not #'(lambda (x) (car (last x))) modes))
	 mode)
    (if rw-modes 
	(progn
          (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-msg))
	  (if (> (length rw-modes) 1)
	      (complain (length rw-modes) " read/write modes for this theorem are known:" t)
	    (complain "Only one read/write mode for this theorem is known:" t))
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	  (dolist (elt rw-modes)
	    (incf count) 
	    (complain count ") ") (print-bestmodes-nicely (list elt) 4))
	  (incf count) 
	  (complain count ") None of these.")
	  (setq mode (nth (1- (get-a-number count)) rw-modes))
	  (when mode
	    (setf (gethash thm lib-bestmodes)
		  (sort (append ro-modes (remove-bestmode mode rw-modes)) #'< :key #'caddr))
	    (when (null (gethash thm lib-bestmodes))
	      (remhash thm lib-bestmodes))
	    (output-bestmodes-to-file)))
      (msgf "There are no read/write modes for this theorem."))))

(defun remove-bestmode (mode list)
  (let (newlist)
    (dolist (l list newlist)
      (unless (and (string= (car l) (car mode))
		   (eql (cadr l) (cadr mode))
		   (eql (caddr l) (caddr mode))
		   (equal (cadddr l) (cadddr mode))
		   (eq (cadr (cdddr l)) (cadr (cdddr mode))))
	(push l newlist)))))

(defun listeq (l1 l2)
  (or (and (null l1) (null l2))
      (and (equalp (car l1) (car l2))
	   (listeq (cdr l1) (cdr l2)))))

(deflibrary show-bestmode
  (lib-argnames theorem)
  (lib-argtypes symbol)
  (lib-arghelp "Theorem name, or NIL for all")
  (lib-defaultfns (lambda (x) (list (if (eq x '$) nil x))))
  (lib-mainfns show-bestmode)
  (mhelp "List all of the current best modes for theorems in the library.
Shows mode name, date, time for proof, and whether the mode is read/write
(in your library) or read-only (in someone else's library)."))

(deflibrary show-new-bestmodes
  (lib-argnames date)
  (lib-argtypes short-date)
  (lib-arghelp "Show modes added since which date?")
  (lib-defaultfns (lambda (x) (list (if (eq x '$) (output-today) x))))
  (mhelp "List all of the best modes which have been added since the 
given date. This will search all available bestmodes.rec files,
including those in other people's library directories."))

(defun show-new-bestmodes (date)
  (update-bestmodes-file)
  (let (objlist newo)
    (maphash #'(lambda (key val)
		 (when key (push (cons key val) objlist)))
	     lib-bestmodes)
    (setq objlist (sort objlist #'string< :key #'car))
    (dolist (o objlist)
      (setq newo (remove-if #'(lambda (x) (< x date)) (cdr o) :key #'cadr))
      (when newo
	(msgf (car o) " : ")
	(print-bestmodes-nicely newo (+ 3 (length (princ-to-string (car o))))))
    )))
  
(defun show-bestmode (theorem)
  (update-bestmodes-file)
  (if theorem
      (progn (msgf t theorem " : ") 
	     (print-bestmodes-nicely (gethash theorem lib-bestmodes) (+ 3 (length (princ-to-string theorem))))
	     (msgf t))
    (let (objlist)
      (maphash #'(lambda (key val)
		   (when key (push (cons key val) objlist)))
	       lib-bestmodes)
      (setq objlist (sort objlist #'string< :key #'car))
      (dolist (o objlist) 
	(msgf (car o) " : ")
	(print-bestmodes-nicely (cdr o) (+ 3 (length (princ-to-string (car o))))))
      )))

(deflibrary show-bestmode-thms
  (mhelp "List all of the theorems that have bestmodes in bestmodes.rec files."))

(defmenuitem SHOW-BESTMODE-THMS
  (display-name "SHOW-BESTMODE-THMS")
  (placement 1)
  (command "SHOW-BESTMODE-THMS")
  (parent BEST-MODES)
  (mhelp ""))

(defun show-bestmode-thms ()
  (let ((thms nil))
    (maphash #'(lambda (key val) (declare (ignore val)) (push key thms)) lib-bestmodes)
    (print-unordered-symbol-table thms)))

(defun print-bestmodes-nicely (best indent)
  (let ((first t))
  (dolist (b best)
    (unless first (msgf) (spaces indent))
    (let ((comment "")
	  (dont-auto-test nil))
      (setq first nil)
      (msg (car b) "  ") (princ-date (cadr b)) (msg "  " (caddr b) " seconds") 
      (if (caddr (cddr b)) (msg "  (read/write)") (msg "  (read only)"))
      (if (consp (cadddr b))
	  (progn
	    (setq comment (car (cadddr b)))
	    (setq dont-auto-test (cadr (cadddr b))))
	(setq comment (cadddr b)))
      (unless (string= "" comment)
	(msgf)
	(spaces (+ 2 indent))
	(do ((i 0 (1+ i))
	     (cursor (+ 2 indent) (1+ cursor)))
	    ((= i (length comment)))
	  (msg (princ-to-string (aref comment i)))
	  (when (> (+ 2 cursor) (max rightmargin (+ 20 indent))) (msgf) (setq cursor (+ 2 indent)) (spaces (+ 2 indent)))))
      (when dont-auto-test
	(msgf "Not used for testing automatic search")))
    )))

(defreview find-mode
  (argtypes symbol)
  (argnames thm)
  (arghelp "Theorem")
  (mhelp "Find a mode from bestmodes.rec for the given theorem,
and (after prompting the user) switch to the selected mode.
This will search all of the bestmodes.rec files which occur
in any of the directories in DEFAULT-LIB-DIR and BACKUP-LIB-DIR."))

(defun find-mode (gwff &optional (skip-query t) (query-def nil))
  (update-bestmodes-file)
  (let ((modes (gethash gwff lib-bestmodes))
	(mode nil)
	(count 0))
    (when (and (null modes) skip-query)
      (msgf t "No modes are known for the theorem " gwff "." t))
    (when (and modes (or skip-query (query "Would you like to load a mode for this theorem?" query-def)))
      (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-msg))
      (if (> (length modes) 1)
	  (complain (length modes) " modes for this theorem are known:" t)
	(complain "Only one mode for this theorem is known:" t))
      (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
      (dolist (elt modes)
	(incf count) 
	(complain count ") ") (print-bestmodes-nicely (list elt) 4))
      (incf count) 
      (complain count ") None of these.")
      (setq mode (car (nth (1- (get-a-number count)) modes)))
      (when mode
	(if (member mode global-modelist)
	    (progn (mode mode) t)
	  (let ((y (car (remove-if-not #'(lambda (x) (memq (car x) '(mode mode1)))
				       (gethash mode core::*lib-masterindex*)))))
	    (if y
		(progn (core::retrieve-libobject mode :type (car y) :multiple nil)
		       (mode mode) t)
	      (progn
		(msgf "Can't find that mode in the library." t)
		nil))))))))

(deflibrary find-dup-modes
  (mhelp "List all potential duplicates in the bestmodes.rec file."))

(defun find-dup-modes ()
  (update-bestmodes-file)
  (msgf "Looking for duplicate modes..." t)
  (let (objlist)
    (maphash #'(lambda (key val)
		 (when key (push (cons key val) objlist)))
	     lib-bestmodes)
    (setq objlist (sort objlist #'string< :key #'car))
    (do ((o objlist (cdr o))
	 temp)
	((null o))
      (when (and (> (length (cadr o)) 1)
		 (< (length (remove-duplicates (cdar o) :key #'car :test #'string=)) (length (cdar o))))
	(setq temp (sort (cdar o) #'string< :key #'car))
	(msgf (caar o) " : ")
	(print-duplicates-in temp (+ 3 (length (princ-to-string (caar o)))) nil t)))))

(defun print-duplicates-in (list indent &optional (printing nil) (first nil))
  (when list
    (if printing
	(progn (unless first (spaces indent))
	       (print-bestmodes-nicely (list (car list)) indent)
	       (msgf)
	       (if (string= (caar list) (caadr list))
		   (print-duplicates-in (cdr list) indent t)
		 (print-duplicates-in (cdr list) indent nil)))
      (if (string= (caar list) (caadr list))
	  (progn (unless first (spaces indent))
		 (print-bestmodes-nicely (list (car list)) indent)
		 (msgf)
		 (print-duplicates-in (cdr list) indent t))
	(print-duplicates-in (cdr list) indent nil)))))
  
  
(defun update-bestmodes-file ()
  (clrhash lib-bestmodes)
  (dolist (direc (reverse (append default-lib-dir backup-lib-dir)))
    (let ((lib-bestmode-file 
	   (make-pathname% :name (file-namestring (pathname-name (pathname lib-bestmode-file)))
			   :type default-libindex-type
			   :directory direc)))
      (when (probe-file lib-bestmode-file)
	(with-open-file (*input-stream* lib-bestmode-file :direction :input)
			(do ((item nil (read *input-stream* nil control-d-char)))
			    ((eq item control-d-char))
			  (when item 
			    (stuff-bestmodes-into-hashtable item 
							    (if (member direc default-lib-dir :test #'string=) t nil))))))))
  )

(deflibrary update-provability
  (mhelp "Update the PROVABILITY attribute of all the gwffs for which a 
best mode is known."))

(defun update-provability ()
  (let ((provability "Automatic expansion proof and translation to natural deduction")
	found objlist)
    (maphash #'(lambda (key val)
		 (declare (ignore val))
		 (when key (push key objlist)))
	     lib-bestmodes)
    (setq objlist (sort objlist #'string<))
    (dolist (name objlist)
      (setq found (locate-item name :type 'gwff :writeable t))
      (dolist (elt found)
	(let ((old-value (retrieve-item name :type 'gwff :writeable t :multiple nil 
					:preferred-dir (cdr elt))))
	  (unless (string= provability (libitem-provability old-value))
	    (msgf "Updating " name " in directory " (pfile (namestring (cdr elt))))
	    (let ((item (make-libitem
			 :name name :type 'gwff :description (libitem-description old-value)
			 :mhelp (libitem-mhelp old-value) :context (libitem-context old-value)
			 :provability provability
			 :keywords (libitem-keywords old-value)
			 :proof-date (stringdt nil)
			 :other-attributes (libitem-other-attributes old-value)
			 :needed-objects (libitem-needed-objects old-value)
			 :other-remarks (libitem-other-remarks old-value)
			 :file (namestring (cdr elt)))))
	      (setf (libitem-keywords item) (get-keywords-for item (libitem-keywords item)))
	      (update-libitem item))))))))

; classification schemes - cebrown - 11/9/01
(context lib-class)

(defflag CLASS-SCHEME
  (default LIBDIR)
  (flagtype symbol)
  (subjects library)
  (change-fn (lambda (flag value old-value)
	       (unless (get value 'libclass)
		 (let ((y (car (remove-if-not #'(lambda (x) (eq (car x) 'CLASS-SCHEME))
					      (gethash value core::*lib-masterindex*)))))
		   (when y
		     (when (query (format nil "Retrieve ~d from the library?" value) t)
		       (core::retrieve-libobject value :type 'class-scheme :multiple t)))
		   (unless (get value 'libclass)
		     (msgf value " is not a known classification scheme." t)
		     (if (query "Create a new class scheme?" t)
			 (let ((help ""))
			   (prompt-read help nil (msgf "Help Message") 'string ""
					((? (msgf "Help Message for New Classification Scheme"))
					 (?? (mhelp 'string))))
			   (create-class-scheme value help))
		       (progn
			 (msgf "Current classification scheme unchanged." t)
			 (setq value old-value)
			 (set flag old-value))))))
	       (when (get value 'class-direction)
		 (set-flag 'class-direction (get value 'class-direction)))
	       (when (get value 'libclass)
		 (setq *current-class* (get value 'libclass)))))
  (mhelp "The classification scheme used to organize the library interface.
A classification scheme is a way of organizing library items into a tree 
(actually a directed acyclic graph) of classes.  Each class can have 
classes as children.  Each class has associated libitems.

See Also: CREATE-CLASS-SCHEME, PSCHEMES, PCLASS-SCHEME-TREE, 
PCLASS-TREE, CREATE-LIBCLASS, CLASSIFY-CLASS, CLASSIFY-ITEM, 
FETCH-LIBCLASS, FETCH-LIBCLASS*, FETCH-UP, FETCH-DOWN,
GOTO-CLASS, ROOT-CLASS"))

(deflibrary create-class-scheme
  (lib-argnames name help)
  (lib-argtypes symbol string)
  (lib-arghelp "Name" "Help Message")
  (lib-defaultfns (lambda (x y) (list x (if (eq y '$) "" y))))
  (lib-mainfns create-class-scheme)
  (mhelp "Create a classification scheme for the library.  A classification scheme
is a way of organizing library items into a tree (actually 
a directed acyclic graph) of classes.  Each class can have 
classes as children.  Each class has associated libitems.

This classification scheme can itself be saved in the library
and retrieved from the library as an object of type LIBCLASS.

A classification scheme can also be used to access the TPS
library using a Unix-style interface.  Use the command
UNIXLIB to enter the Unix-style top level for the library.

See Also: UNIXLIB, PSCHEMES, CLASS-SCHEME, GOTO-CLASS, CREATE-LIBCLASS, 
CLASSIFY-CLASS, CLASSIFY-ITEM, PCLASS-SCHEME, PCLASS-SCHEME-TREE, 
PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*"))
   
(deflibrary pclass
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Class")
  (lib-defaultfns (lambda (x) (list (if (and (eq x '$) *current-class*)
					(libclass-name *current-class*)
				      x))))
  (lib-mainfns pclass)
  (mhelp "Prints information about the current library class in the current
classification scheme.

See Also: CLASS-SCHEME, CREATE-CLASS-SCHEME, PSCHEMES, 
PCLASS-SCHEME-TREE, PCLASS-TREE, GOTO-CLASS, CLASSIFY-CLASS, 
UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
FETCH-LIBCLASS, FETCH-LIBCLASS*"))


(defmexpr pschemes
  (mhelp "Prints a list of Library Classification Schemes in memory.

See Also: CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, GOTO-CLASS,
CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(deflibrary pschemes
  (lib-mainfns pschemes)
  (mhelp "Prints a list of Library Classification Schemes in memory.

See Also: CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, GOTO-CLASS,
CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(deflibrary root-class
  (lib-mainfns goto-root-class)
  (mhelp "Makes the root class of the current library classification scheme
the current class.

See Also: CLASS-SCHEME, GOTO-CLASS."))

(deflibrary goto-class
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Class")
  (lib-defaultfns (lambda (x) (list (if (and (eq x '$) *current-class*)
					(libclass-name *current-class*)
				      x))))
  (lib-mainfns goto-class)
  (mhelp "Searches for classes of the given name within the
current library classification scheme.  If one
is found, that class is made the current class.
If several are found, the user is asked to choose.

See Also: CLASS-SCHEME, ROOT-CLASS, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, FETCH-LIBCLASS, FETCH-LIBCLASS*"))


(deflibrary classify-class
  (lib-argnames class1 class2)
  (lib-argtypes symbol symbol)
  (lib-arghelp "The class to classify" "The new parent class")
  (lib-defaultfns (lambda (x y) (list (if (and (eq x '$) *current-class*)
					  (libclass-name *current-class*)
					x)
				      (if (and (eq y '$) *current-class*)
					  (libclass-name *current-class*)
					y))))
  (lib-mainfns classify-class)
  (mhelp "Classifies class1 under class2 within the current library classification scheme.

See Also: UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
GOTO-CLASS, CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(deflibrary unclassify-class
  (lib-argnames class1 class2)
  (lib-argtypes symbol symbol)
  (lib-arghelp "The Child Class" "The Parent Class")
  (lib-defaultfns (lambda (x y) 
		    (let ((x2 (if (and (eq x '$) *current-class*)
					  (libclass-name *current-class*)
					x)))

		      (list x2
			    (if (eq y '$)
				(if (and (libclass-p x2) (libclass-parents x2))
				    (car (libclass-parents x2))
				  (if *current-class*
				      (libclass-name *current-class*)
				    y))
			      y)))))
  (lib-mainfns unclassify-class)
  (mhelp "Removes class1 from class2 within the current library classification scheme.

See Also: CLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM,
GOTO-CLASS, CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(deflibrary classify-item
  (lib-argnames itemname classname)
  (lib-argtypes symbol symbol)
  (lib-arghelp "Library Item" "Class")
  (lib-defaultfns (lambda (x y) (list x
				      (if (and (eq y '$) *current-class*)
					  (libclass-name *current-class*)
					y))))
  (lib-mainfns classify-item)
  (mhelp "Puts the library item into the given class within the
current library classification scheme.  If the item has needed objects,
TPS also offers to classify these.  If the flag CLASS-DIRECTION
is set to UP, the needed objects must be classified in ancestors
of the given class.  If the flag CLASS-DIRECTION is set to DOWN, 
the needed objects must be classified in descendants of the given class.  

See Also: CLASSIFY-CLASS, UNCLASSIFY-CLASS, UNCLASSIFY-ITEM,
GOTO-CLASS, CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(deflibrary unclassify-item
  (lib-argnames itemname classname)
  (lib-argtypes symbol symbol)
  (lib-arghelp "Library Item" "Class")
  (lib-defaultfns (lambda (x y) (list x
				      (if (and (eq y '$) *current-class*)
					  (libclass-name *current-class*)
					y))))
  (lib-mainfns unclassify-item)
  (mhelp "Removes the library item from the given class within the
current library classification scheme.

See Also: CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM,
GOTO-CLASS, CLASS-SCHEME, CREATE-CLASS-SCHEME, PCLASS, PSCHEMES,
PCLASS-SCHEME-TREE, PCLASS-TREE, FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(deflibrary fetch-libclass
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Class")
  (lib-defaultfns (lambda (x) (list (if (and (eq x '$) *current-class*)
					(libclass-name *current-class*)
				      x))))
  (lib-mainfns fetch-libclass)
  (mhelp "Fetches all the library items classified in the current class
within the current library classification scheme.

See Also: FETCH-LIBCLASS*, CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, GOTO-CLASS, PSCHEMES,
PCLASS, PCLASS-SCHEME-TREE, PCLASS-TREE, CREATE-CLASS-SCHEME"))

(deflibrary fetch-libclass*
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Class")
  (lib-defaultfns (lambda (x) (list (if (and (eq x '$) *current-class*)
					(libclass-name *current-class*)
				      x))))
  (lib-mainfns fetch-libclass*)
  (mhelp "Fetches all the library items classified in the current class
within the current library classification scheme.  If the flag
CLASS-DIRECTION is set to Up, then FETCH-LIBCLASS* also fetches
all the libitems classified in ancestor classes.  If the flag
CLASS-DIRECTION is set to Down, then FETCH-LIBCLASS* also fetches
all the libitems classified in descendant classes.

See Also: FETCH-UP, FETCH-DOWN, FETCH-LIBCLASS, CLASS-DIRECTION,
CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM,
UNCLASSIFY-ITEM, GOTO-CLASS, ROOT-CLASS, PSCHEMES, PCLASS, PCLASS-SCHEME-TREE,
PCLASS-TREE, CREATE-CLASS-SCHEME"))

(defflag CLASS-DIRECTION
  (default DOWN)
  (flagtype updown)
  (subjects library)
  (mhelp "Suppose A is a class with child class B.
If the value of CLASS-DIRECTION is Up, we think of
B as depending on A (eg, A could be GROUPS and B could be FIELDS).
If the value of CLASS-DIRECTION is Down, we think of
A as depending on B (eg, B could be GROUPS and A could be FIELDS).

The value of this flag affects the behavior of CLASSIFY-ITEM
and FETCH-CLASS*.

See Also: CLASSIFY-ITEM, FETCH-CLASS*, FETCH-UP, FETCH-DOWN"))

(deflibrary fetch-up
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Class")
  (lib-defaultfns (lambda (x) (list (if (and (eq x '$) *current-class*)
					(libclass-name *current-class*)
				      x))))
  (lib-mainfns fetch-up)
  (mhelp "Fetches all the library items classified in the current class
and in all the ancestors of that class are also fetched.

See Also: SUBCLASS-DIRECTION, FETCH-LIBCLASS*, FETCH-DOWN, FETCH-LIBCLASS,
CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, GOTO-CLASS, PSCHEMES,
PCLASS, PCLASS-SCHEME-TREE, PCLASS-TREE, CREATE-CLASS-SCHEME"))

(deflibrary fetch-down
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Class")
  (lib-defaultfns (lambda (x) (list (if (and (eq x '$) *current-class*)
					(libclass-name *current-class*)
				      x))))
  (lib-mainfns fetch-down)
  (mhelp "Fetches all the library items classified in the current class
and in all the descendents of that class are also fetched.

See Also: CLASS-DIRECTION, FETCH-LIBCLASS*, FETCH-UP, FETCH-LIBCLASS,
CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, GOTO-CLASS, PSCHEMES,
PCLASS, PCLASS-SCHEME-TREE, PCLASS-TREE, CREATE-CLASS-SCHEME"))

(deflibrary create-libclass
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "New Class")
  (lib-mainfns create-libclass)
  (mhelp "Creates a new class in the current classification scheme.

See Also: CREATE-CLASS-SCHEME, CLASSIFY-CLASS, UNCLASSIFY-CLASS, 
CLASSIFY-ITEM, UNCLASSIFY-ITEM, GOTO-CLASS, CLASS-SCHEME, PSCHEMES,
FETCH-LIBCLASS, FETCH-LIBCLASS*, PCLASS, PCLASS-SCHEME-TREE, PCLASS-TREE")) 

(deflibrary pclass-scheme-tree
  (lib-argnames name)
  (lib-argtypes symbol)
  (lib-arghelp "Classification Scheme")
  (lib-defaultfns (lambda (x) (list (if (and (eq x '$) class-scheme)
					class-scheme
				      x))))
  (lib-mainfns pclass-scheme-tree)
  (mhelp "Prints the classification scheme as a tree starting from the root class.
A list of known classification schemes is printed by PSCHEMES.

See Also: PCLASS, PSCHEMES, PCLASS-TREE, CREATE-CLASS-SCHEME, 
CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM, 
GOTO-CLASS, CLASS-SCHEME, FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(deflibrary pclass-tree
  (lib-mainfns pclass-tree)
  (mhelp "Prints the current class and its children as a tree.

See Also: PCLASS, PSCHEMES, PCLASS-SCHEME-TREE, CREATE-CLASS-SCHEME, 
CLASSIFY-CLASS, UNCLASSIFY-CLASS, CLASSIFY-ITEM, UNCLASSIFY-ITEM, 
GOTO-CLASS, CLASS-SCHEME, FETCH-LIBCLASS, FETCH-LIBCLASS*"))

(defun create-class-scheme (name help)
  (when (get name 'libclass)
    (throwfail "There is already a class-scheme named " name t))
  (eval (list 'def-class-scheme name
	      (list 'class-direction class-direction)
	      (list 'libclass (list name nil nil))
	      (list 'mhelp help)))
  (when (query "Do you want to make this the current CLASS-SCHEME?" t)
    (set-flag 'CLASS-SCHEME name))
  nil)

(defun pclass (name)
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let ((cl (find-libclass name)))
    (when (libclass-p cl)
      (msgf "Class: " name t)
      (when (libclass-parents cl)
	(msgf "Parents: " t)
	(dolist (l (libclass-parents cl))
		(pclassinfo l)))
      (when (libclass-kids cl)
	(msgf "Kids: " t)
	(dolist (l (libclass-kids cl))
		(pclassinfo l)))
      (when (libclass-libitems cl)
	(msgf "Library Items: " t)
	(print-libclass-libitems (libclass-libitems cl)))))
  nil)

(defun print-libclass-libitems (items)
  (let ((str ""))
    (dolist (l items)
	    (let ((name (format nil "~d" l)))
	      (if (>= (+ (length str) (length name)) RIGHTMARGIN)
		  (if (= (length str) 0)
		      (msgf name)
		    (progn
		      (msgf str)
		      (setq str name)))
		(setq str (format nil "~d ~d" str name)))))
    (unless (= (length str) 0)
      (msgf str))))
  
(defun pclassinfo (c)
  (if (libclass-parents c)
      (if (libclass-kids c)
	  (if (libclass-libitems c)
	      (msgf (libclass-name c) " (has kids and classified libitems)" t)
	    (msgf (libclass-name c) " (has kids)" t))
	(if (libclass-libitems c)
	    (msgf (libclass-name c) " (has classified libitems)" t)
	  (msgf (libclass-name c) t)))
    (if (libclass-kids c)
	(if (libclass-libitems c)
	    (msgf (libclass-name c) " (root node - has kids and classified libitems)" t)
	  (msgf (libclass-name c) " (root node - has kids)" t))
      (if (libclass-libitems c)
	  (msgf (libclass-name c) " (root node - has classified libitems)" t)
	(msgf (libclass-name c) " (root node)" t))))
  nil)

(defun pschemes ()
  (msgf "Classification Schemes:" t)
  (dolist (name global-class-scheme-list)
    (when (symbolp name)
      (msgf name t))))

(defun goto-root-class ()
  (unless (and (symbolp class-scheme)
	       (libclass-p (get class-scheme 'libclass)))
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (setq *current-class* (get class-scheme 'libclass)))

(defun goto-class (name)
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let ((cl (find-libclass name)))
    (unless cl
      (throwfail "The class scheme " class-scheme " does not have a class " name))
    (when (libclass-p cl)
      (setq *current-class* cl)))
  nil)

(defun find-libclass (name &key (create nil))
  (unless (get class-scheme 'libclass)
    (throwfail "You need to set CLASS-SCHEME." t
	       "Use CREATE-CLASS-SCHEME to create a new scheme."))
  (if (and (libclass-p *current-class*) (eq name (libclass-name *current-class*)))
      *current-class*
    (let ((cll (find-libclasses name :create create)))
      (choose-libclass name cll '("None Of These" . NONE)))))
	
(defun find-libclasses (name &key (create nil))
  (unless (get class-scheme 'libclass)
    (throwfail "You need to set CLASS-SCHEME." t
	       "Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let ((found nil))
    (do ((l (list (get class-scheme 'libclass))
	    (union (libclass-kids (car l)) (cdr l))))
	((null l)
	 (or (remove-duplicates found)
	     (if create
		 (progn
		   (msgf "The class scheme " class-scheme " does not have a class " name)
		   (if (query "Create One?" t)
		       (list (create-libclass name))
		     nil))
	       nil)))
	(when (eq (libclass-name (car l)) name)
	  (push (car l) found)))))

(defun find-libsubclass (name cl)
  (if (and (libclass-p *current-class*) (eq name (libclass-name *current-class*)))
      *current-class*
    (let ((cll (find-libsubclasses name cl)))
      (choose-libclass name cll '("None Of These" . NONE)))))

(defun find-libsubclasses (name cl)
  (let ((found nil))
    (do ((l (list cl) (union (libclass-kids (car l)) (cdr l))))
	((null l) found)
	(when (eq (libclass-name (car l)) name)
	  (push (car l) found)))))

(defun is-a-class-ancestor (cl1 cl2)
  (is-a-class-ancestor-rec (libclass-kids cl1) cl2 nil))

(defun is-a-class-ancestor-rec (cll cl2 &optional already-checked)
  (if cll
      (let ((cl1 (car cll)))
	(if (eq cl1 cl2)
	    t
	  (if (member cl1 already-checked)
	      (is-a-class-ancestor-rec (cdr cll) cl2 already-checked)
	    (is-a-class-ancestor-rec (append (cdr cll) (libclass-kids cl1))
				     cl2 (cons cl1 already-checked)))))
    nil))

(defun find-libsuperclass (name cl)
  (let ((cll (find-libsuperclasses name cl)))
    (choose-libclass name cll '("None Of These" . NONE))))

(defun find-libsuperclasses (name cl)
  (let ((found nil))
    (do ((l (list cl) (union (libclass-parents (car l)) (cdr l))))
	((null l) found)
	(when (eq (libclass-name (car l)) name)
	  (push (car l) found)))))

(defun choose-libclass (name l &rest other-choices)
  (let ((choices nil)
	(choice nil))
    (dolist (n l)
	    (push n choices))
    (if (cdr choices)
	(let ((count 0))
          (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-msg))
	  (complain "Choose between " (length choices) " classes named " name "." t)
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	  (dolist (ch choices)
		  (incf count)
		  (complain count ") " (class-fullpath ch)))
	  (dolist (ch other-choices)
		  (incf count)
		  (complain count ") " (car ch)))
	  (setq count (1- (get-a-number count)))
	  (setq choice (nth count (append choices (mapcar #'cdr other-choices)))))
      (setq choice (car choices)))
    choice))


(defun classify-class (class1 class2)
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let ((cl1 (find-libclass class1 :create t))
	(cl2 (find-libclass class2 :create t)))
    (when (is-a-class-ancestor cl1 cl2)
      (throwfail class1 " is an ancestor of " class2 ", cannot create a classification loop"))
    (setf (libclass-parents cl1)
	  (append (libclass-parents cl1) (list cl2)))
    (setf (libclass-kids cl2)
	  (append (libclass-kids cl2) (list cl1))))
  nil)

(defun unclassify-class (class1 class2)
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let ((cl2 (find-libclass class2)))
    (unless cl2 
      (throwfail "Unknown class " class2))
    (when (libclass-p cl2)
      (let ((cl1 (find-if #'(lambda (x) (eq (libclass-name x) class1)) (libclass-kids cl2))))
	(unless cl1
	  (throwfail class1 " is not a child of " class2))
	(setf (libclass-kids cl2) (remove cl1 (libclass-kids cl2)))
	(setf (libclass-parents cl1) (remove cl2 (libclass-parents cl1)))
	(when (and (not (libclass-parents cl1)) (eq *current-class* cl1))
	  (setq *current-class* cl2)))))
  nil)

(defun items-classified (cl)
  (libclass-libitems cl))

(defun items-classified* (cl)
  (let ((classified (libclass-libitems cl)))
    (dolist (k (libclass-kids cl))
	    (setq classified (union classified (items-classified* k))))
    classified))

(defun classify-item (itemname classname)
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let* ((cl (find-libclass classname :create t))
	 (classified (items-classified* cl)))
    (declare (special classified))
    (when (libclass-p cl)
      (classify-item-1 itemname classname cl))))

(defun classify-item-1 (itemname classname cl)
  (if (or (member itemname core-abbrevlist)
	      (member itemname core-binderabbrevlist)
	      (member itemname core-theoremlist)
	      (gethash itemname *lib-masterindex*))
      (classify-item-2 itemname classname cl)
    (complain "Unknown library item " itemname)))

(defun classify-item-2 (itemname classname cl)
  (declare (special classified))
  (setf (libclass-libitems cl)
	(adjoin itemname (libclass-libitems cl)))
  (setq classified (adjoin itemname classified))
  (when (gethash itemname *lib-masterindex*)
    (let ((item (retrieve-item itemname :type nil :writeable nil :multiple nil :fail-gently t)))
      (when (libitem-p item)
	(let ((needed (setdiff (libitem-needed-objects item) classified)))
	  (when needed
	    (msgf itemname " depends on needed objects.")
	    (if show-all-libobjects
		(dolist (n needed)
		  (when (query (format nil "Would you like to classify ~d?" n) t)
		    (let ((classname2 nil)
			  (cl2 nil))
		      (loop until cl2 do
			    (prompt-read classname2 nil (msgf "Class")
					 'symbol classname
					 ((? 
					   (if (eq CLASS-DIRECTION 'UP)
					       (msgf classname " or a Class Above " classname)
					     (msgf classname " or a Class Below " classname)))))
			    (setq cl2 (if (eq CLASS-DIRECTION 'UP)
					  (find-libsuperclass classname2 cl)
					(find-libsubclass classname2 cl)))
			    (unless cl2
			      (if (eq CLASS-DIRECTION 'UP)
				  (complain classname2 " is not an ancestor of " classname)
				(complain classname2 " is not a descendent of " classname))))
		      (classify-item-1 n classname2 cl2))))
	      (msgf needed t)))))))
  nil)

(defun unclassify-item (itemname classname)
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let ((cl (find-libclass classname)))
    (unless cl
      (throwfail "Unknown class " classname))
    (when (libclass-p cl)
      (setf (libclass-libitems cl)
	    (remove itemname (libclass-libitems cl)))))
  nil)

(defun fetch-libclass (name)
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let ((cl (find-libclass name)))
    (unless cl
      (throwfail "The class scheme " class-scheme " does not have a class " name))
    (when (libclass-p cl)
      (fetch-libclass-1 cl))))

(defun fetch-libclass-1 (cl)
  (when (libclass-libitems cl)
    (msgf "Fetching Class " (libclass-name cl) t)
    (dolist (i (libclass-libitems cl))
      (when (gethash i *lib-masterindex*)
	(retrieve-libobject i :type nil
			    :preferred-dir nil
			    :multiple nil :fail-gently t)))))

(defun fetch-up (name)
  (fetch-libclass* name 'UP))

(defun fetch-down (name)
  (fetch-libclass* name 'DOWN))

(defun fetch-libclass* (name &optional (dir CLASS-DIRECTION))
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME"))
  (let ((cl (find-libclass name))
	(already-done nil))
    (declare (special already-done))
    (unless cl
      (throwfail "The class scheme " class-scheme " does not have a class " name))
    (when (libclass-p cl)
      (fetch-libclass*-1 cl dir))))

(defun fetch-libclass*-1 (cl dir)
  (declare (special already-done))
  (unless (member (libclass-name cl) already-done)
    (push (libclass-name cl) already-done)
    (fetch-libclass-1 cl)
    (dolist (k (if (eq dir 'UP)
		   (libclass-parents cl)
		 (libclass-kids cl)))
	    (fetch-libclass*-1 k dir))))

(defun create-libclass (name)
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (let ((parname nil))
    (prompt-read parname nil (msgf "Parent Class")
		 'symbol (if *current-class*
			     (libclass-name *current-class*)
			   (if class-scheme
			       class-scheme
			     '$))
		 ((? (msgf "Name of an Existing Library Class"))))
    (let ((cl (if (and *current-class* (eq parname (libclass-name *current-class*)))
		  *current-class*
		(find-libclass parname :create t))))
      (when (libclass-p cl)
	(let ((new (make-libclass :name name :libitems nil
				  :parents (list cl)
				  :kids nil)))
	  (push new (libclass-kids cl)))))))

(defun pclass-scheme-tree (name)
  (pclass-tree-main leftmargin rightmargin (get name 'libclass)))

(defun pclass-tree ()
  (declare (special *current-class*))
  (when (and class-scheme (not *current-class*))
    (setq *current-class* (get class-scheme 'libclass)))
  (unless class-scheme
    (throwfail "You need to set CLASS-SCHEME." t
"Use CREATE-CLASS-SCHEME to create a new scheme."))
  (pclass-tree-main leftmargin rightmargin *current-class*))

(defun pclass-tree-main (l r ctree)
  (let ((center (round (/ (+ l r) 2)))
	(old-tabstops nil))
    (msgf "Branches with *'s denote classes that are being omitted for lack of space." t)
    (msg T T (t (- center (round (1+ (/ (length (princ-to-string (libclass-name ctree))) 2)))))
	 (libclass-name ctree) t)
    (setq old-tabstops (list (cons center "|")))
    (do ((plist (libclass-kids ctree)
		(rec-libclass-kids plist))
	 (old-plist nil plist))
	((equal plist old-plist))
	(if old-plist
	    (setq old-tabstops
		  (rec-libclass-print l r old-plist old-tabstops))))))

(defun rec-libclass-kids (plist)
  (if (null plist) nil
    (if (listp (car plist)) (cons (rec-libclass-kids (car plist))
				  (rec-libclass-kids (cdr plist)))
      (cons (libclass-kids (car plist)) (rec-libclass-kids (cdr plist))))))

(defun rec-libclass-print (l r plist old-tabstops)
  (let ((tabstops (mapcar #'(lambda (x) (cons (round (car x)) (cdr x)))
					    (sort (calc-tabstops l r plist) 
						  #'(lambda (x y) (< (car x) (car y)))))))
    (dolist (tb old-tabstops) (msg (t (car tb)) "|"))
    (msg t) (setq core::curpos 0)
    (do* ((i 0 (1+ i))
	  (oldshape " " (or newshape " "))
	  (newshape (cdr (assoc i tabstops)) (cdr (assoc i tabstops))))
	 ((> i (caar (last tabstops))))
	 (if newshape (progn 
			(if (> (length newshape) 1) (setq newshape (char newshape 1)))
			(msg newshape) 
			(if (or (string= newshape "/") (string= newshape "+"))
			    (setq newshape "-")
			  (setq newshape " ")))
	   (progn (if (string= oldshape "-") (setq newshape "-")) (msg oldshape))))
    (msg t) (setq curpos 0)
    (dolist (tb tabstops) (msg (t (car tb)) "|"))
    (msg t) (setq curpos 0)
    (mapcar #'(lambda (x y z w) (p-libclass-elt x y z w))
	    tabstops (remove-if #'null (flatten-mstlist plist))
	    (append (cdr tabstops) (list (cons rightmargin 'foo)))
	    (cons (cons leftmargin 'foo) tabstops))
    (msg t) (setq curpos 0)
;    (mapcar #'(lambda (x y z w) (p-libclass-labels x y z w print-wffs))
;	    tabstops 
;	    (remove-if #'null (flatten-mstlist plist))
;	    (append (cdr tabstops) (list (cons rightmargin 'foo)))
;	    (cons (cons leftmargin 'foo) tabstops))
;    (msg t) (setq curpos 0)
    tabstops))

(defun p-libclass-elt (tb l nxtb lsttb)
  (let* ((string (princ-to-string l))
	 (obwidth (length string)))
    (if (string= (char (cdr tb) 0) "*") 
	(progn (spaces (1- (- (car tb) (1+ core::curpos))))
	  (msg (t (car tb)) "*") (setq core::curpos (car tb)))
      (if (>= obwidth (* 2 (min (1- (- (car nxtb) (car tb)))
				(1- (- (car tb) (max core::curpos (car lsttb)))))))
	  (progn (spaces (1- (- (car tb) (1+ core::curpos))))
		 (msg (t (car tb)) "*")  (setq core::curpos (car tb)))
	(if (= obwidth 0) (progn (spaces (1- (- (car tb) (1+ core::curpos))))
				 (msg (t (car tb)) "|")  (setq core::curpos (car tb)))
	  (progn
	    (spaces (1- (- (- (car tb) (round (/ obwidth 2))) (1+ core::curpos))))
	    (msg (t (- (car tb) (round (/ obwidth 2)))) string)
	    (setq core::curpos (1- (+ obwidth (- (car tb) (round (/ obwidth 2))))))))))))

(defun find-or-create-libclass (path cl)
  (if path
      (let ((cl2 (member (car path) (libclass-kids cl) :key #'libclass-name)))
	(if cl2
	    (find-or-create-libclass (cdr path) (car cl2))
	  (let ((cl3 (make-libclass :name (car path) :parents (list cl))))
	    (push cl3 (libclass-kids cl))
	    (find-or-create-libclass (cdr path) cl3))))
    cl))

(defun libclass-libitems* (cl)
  (let ((items (libclass-libitems cl)))
    (dolist (cl2 (if (eq class-direction 'down)
		     (libclass-kids cl)
		   (libclass-parents cl)))
	    (setq items (union items (libclass-libitems* cl2))))
    items))

(deflibrary pintersect
  (lib-argnames classnames)
  (lib-argtypes symbollist)
  (lib-arghelp "A list of class names.")
  (lib-mainfns pintersect)
  (mhelp "Print the objects that are classified in all the specified classes.

See Also: pintersect*"))

(deflibrary pintersect*
  (lib-argnames classnames)
  (lib-argtypes symbollist)
  (lib-arghelp "A list of class names.")
  (lib-mainfns pintersect*)
  (mhelp "Finds and prints the name of all the objects which, for each
specified class, are classified in the class or a 'subclass'.

If CLASS-DIRECTION is set to DOWN, 'subclass' means a descendant class.

If CLASS-DIRECTION is set to UP, 'subclass' means a ancestor class.

See Also: pintersect"))

(defun pintersect (classnames)
  (let ((classes nil))
    (dolist (p classnames)
	    (let ((cl (find-libclass p)))
	      (when (libclass-p cl)
		(push cl classes))))
    (when classes
      (let ((items (libclass-libitems (car classes))))
	(dolist (cl (cdr classes))
		(setq items
		      (intersection items (libclass-libitems cl))))
	(if items
	    (progn
	      (msgf "Items Classified under all of " classnames ":" t)
	      (print-libclass-libitems items))
	  (msgf "There are no items classified under all of " classnames "." t))))
    nil))

(defun pintersect* (classnames)
  (let ((classes nil))
    (dolist (p classnames)
	    (let ((cl (find-libclass p)))
	      (when (libclass-p cl)
		(push cl classes))))
    (when classes
      (let ((items (libclass-libitems* (car classes))))
	(dolist (cl (cdr classes))
		(setq items
		      (intersection items (libclass-libitems* cl))))
	(if items
	    (progn
	      (msgf "Items Classified under all of " classnames ":" t)
	      (print-libclass-libitems items))
	  (msgf "There are no items classified under all of " classnames "." t))))
    nil))

(deflibrary generate-class-scheme
  (lib-argnames name help)
  (lib-argtypes symbol string)
  (lib-arghelp "Name" "Help Message")
  (lib-defaultfns (lambda (x y) (list x (if (eq y '$) "" y))))
  (lib-mainfns generate-class-scheme)
  (mhelp "Generate a classification scheme for all abbreviations,
constants, and gwffs.  TPS does some of the work, and prompts the
user to interactively make other choices.

This command can also be used to update an existing class-scheme
by including all library items which are not classified in
the existing class-scheme.

NOTE:  It is best to run this with a fresh core image.  Otherwise,
TPS may confuse items previously fetched from the library with
objects defined in the core TPS image."))

(defun generate-class-scheme (name help)
  (unless (get name 'libclass)
    (create-class-scheme name help))
  (msgf "TPS will help you generate a classification scheme for the library." t)
					; someday we may have more choices here
  (msgf "Options:")
  (msgf "1) Classify UP by used abbrevs (so children are bigger theories)" t)
  (msgf "2) Classify DOWN by used abbrevs (so children are smaller theories)" t)
  (let ((g (get-a-number 2)))
    (let ((libdag (generate-class-scheme-abbrevs-info))
	  (rclass (get name 'libclass)))
      (if (= g 1)
	  (generate-class-scheme-abbrevs-up libdag rclass)
	(generate-class-scheme-abbrevs-down libdag rclass))
      (msgf "Finished.  You probably want to edit the class-scheme " name t
	    "in the UNIXLIB top level and save it in the library using INSERT." t
	    "The relevant UNIXLIB commands are FIND-GENERATED-CLASS and IMPORT-CLASS"))))

(defun generate-class-scheme-abbrevs-down (libdag rootclass)
  (msgf "Building A Classification Scheme with Generated Class Names GEN.*." t)
  (let ((cassoc nil))
    (dolist (a libdag)
      (let* ((needed (nth 3 a))
	     (needthis (nth 4 a))
	     (cl nil)
	     (typ-locs (mapcar #'(lambda (x)
				   (cons (caar a) x))
			       (cadr a)))
	     (cll (remove rootclass
			  (lowest-existing-classes-down (cdar a) 
							rootclass)))) ; only relevant if we're updating an existing scheme
	(if cll
	    (if (not (cdr cll))
		(setq cl (car cll))
	      (progn
		(setq cl (make-libclass :name (intern (format nil "GEN.UPDATE.~d.~d" (caar a) (cdar a)))
					:kids cll
					:libitems (list (cons (cdar a) typ-locs))))
		(dolist (cl1 cll)
		  (dolist (p (libclass-parents cl1))
		    (setf (libclass-kids p)
			  (adjoin cl (remove cl1 (libclass-kids p))))
		    (setf (libclass-parents cl)
			  (adjoin p (libclass-parents cl))))
		  (setf (libclass-parents cl1) (list cl))
		  (setf (libclass-libitems cl1)
			(remove (cdar a) (libclass-libitems cl1))))
		(dolist (k cll)
		  (push cl (libclass-parents k)))))
	  (progn
	    (setq cl (make-libclass
		      :name (intern (format nil "GEN.~d.~d" (caar a) (cdar a)))
		      :libitems (list (cons (cdar a) typ-locs))))
	    (unless needthis
	      (push cl (libclass-kids rootclass))
	      (push rootclass (libclass-parents cl)))))
	(push (cons (car a) cl) cassoc)
	(dolist (n needthis)
	  (let ((b (assoc n cassoc :test #'equal)))
	    (when (and b (not (is-a-class-ancestor (cdr b) cl)))
	      (push cl (libclass-kids (cdr b)))
	      (push (cdr b) (libclass-parents cl)))))
	(dolist (n needed)
	  (let ((b (assoc n cassoc :test #'equal)))
	    (when (and b (not (is-a-class-ancestor cl (cdr b))))
	      (push cl (libclass-parents (cdr b)))
	      (push (cdr b) (libclass-kids cl)))))))
    (remove-empty-classes (list rootclass))))

(defun generate-class-scheme-abbrevs-up (libdag rootclass)
  (msgf "Building A Classification Scheme with Generated Class Names GEN.*." t)
  (let ((cassoc nil)
	(updating (libclass-kids rootclass)))
    (dolist (a libdag)
      (let ((needed (nth 3 a))
	    (needthis (nth 4 a))
	    (cl nil)
	    (cll (first-existing-classes-up (cdar a)
					    rootclass))) ; only relevant if we're updating an existing scheme
	(if cll
	    (if (not (cdr cll))
		(setq cl (car cll))
					; make a common subclass of existing classes for a
	      (progn
		(setq cl (make-libclass
			  :name (intern (format nil "GEN.UPDATE.~d.~d" (caar a) (cdar a)))
			  :parents cll))
		(dolist (p cll)
		  (push cl (libclass-kids p)))))
	  (progn
	    (setq cl (make-libclass 
		      :name (intern (format nil "GEN.~d.~d" (caar a) (cdar a)))
		      :libitems (list (cdar a))))
	    (msgf "Creating Class For " (cdar a) t)
	    (msgf "Needed " needed t)
	    (unless needed
	      (push cl (libclass-kids rootclass))
	      (push rootclass (libclass-parents cl)))))
	(push (cons (car a) cl) cassoc)
	(dolist (n needed)
	  (let ((b (assoc n cassoc :test #'equal)))
	    (when (and b (not (eq (cdr b) cl))
		       (not (is-a-class-ancestor (cdr b) cl)))
	      (push cl (libclass-kids (cdr b)))
	      (push (cdr b) (libclass-parents cl)))))
	(dolist (n needthis)
	  (let ((b (assoc n cassoc :test #'equal)))
	    (when (and b (not (eq (cdr b) cl))
		       (not (is-a-class-ancestor cl (cdr b))))
	      (push cl (libclass-parents (cdr b)))
	      (push (cdr b) (libclass-kids cl)))))))
    (unless updating
      (force-unique-parent-classes (remove-if-not
				    #'(lambda (x)
					(let ((name (format nil "~d" (libclass-name x))))
					  (and (> (length name) 3)
					       (string-equal name "GEN." :start1 0 :end1 4))))
				    (mapcar #'cdr cassoc))))))

(defun first-existing-classes-up (name cl)
  (let ((CLASS-DIRECTION 'UP))
    (declare (special CLASS-DIRECTION))
    (first-existing-classes-up-rec name cl)))

(defun first-existing-classes-up-rec (name cl &optional cll already)
  (if (member cl already)
      (if cll
	  (first-existing-classes-up-rec name (car cll) (cdr cll) already)
	nil)
    (if (member name (libclass-libitems cl))
	(cons cl
	      (if cll
		  (first-existing-classes-up-rec name (car cll) (cdr cll)
						 (cons cl already))
		nil))
      (let ((cll2 (append cll (libclass-kids cl))))
	(if cll2
	    (first-existing-classes-up-rec name (car cll2) (cdr cll2)
					   (cons cl already))
	  nil)))))

(defun lowest-existing-classes-down (name cl)
  (let ((CLASS-DIRECTION 'DOWN))
    (declare (special CLASS-DIRECTION))
    (lowest-existing-classes-down-rec name cl)))

(defun lowest-existing-classes-down-rec (name cl &optional cll already)
  (if (member cl already)
      (if cll
	  (lowest-existing-classes-down-rec name (car cll) (cdr cll) already)
	nil)
    (if (member name (libclass-libitems* cl))
	(let ((cll2 (append cll (libclass-kids cl))))
	  (if cll2
	      (let ((r (lowest-existing-classes-down-rec name (car cll2) (cdr cll2)
							 (cons cl already))))
		(if (find-if #'(lambda (x) (is-a-class-ancestor cl x)) r)
		    r
		  (cons cl (remove-if #'(lambda (x) (is-a-class-ancestor x cl)) r))))
	    (list cl)))
      (if cll
	  (lowest-existing-classes-down-rec name (car cll) (cdr cll) (cons cl already))
	nil))))

(defun remove-empty-classes (cll &optional already-done)
  (if cll
      (let ((cl1 (car cll)))
	(if (member cl1 already-done)
	    (remove-empty-classes (cdr cll) already-done)
	  (if (and (not (libclass-kids cl1))
		   (not (libclass-libitems cl1)))
	      (progn
		(dolist (p (libclass-parents cl1))
		  (setf (libclass-kids p)
			(remove cl1 (libclass-kids p))))
		(remove-empty-classes (cdr cll) (cons cl1 already-done)))
	    (remove-empty-classes (append (cdr cll) (libclass-kids cl1))
				  (cons cl1 already-done)))))
    nil))
      

; modify the class scheme so each given (automatically generated) class
; has a unique parent.  The newly generated classes may have several
; parents.  The classes generated by this function correspond to
; a set of needed objects in an 'Up' class scheme.
(defun force-unique-parent-classes (cllist)
  (let ((subset-assoc (mapcar #'(lambda (cl) (cons (list cl) cl)) cllist)) ; start w/singletons
	(i 0))
    (dolist (cl cllist) ; at each step, only modify the parents of this cl
      (let ((pars (libclass-parents cl)))
	(when (cdr pars)
	  (let* ((pars2 (remove-ancestor-classes pars))
		 (cll (find-maximal-subset-classes pars2 subset-assoc)) ; parents of new class
		 (dll (find-minimal-superset-classes pars2 subset-assoc))) ; kids of new class
	    (if (and cll dll (not (cdr cll)) (not (cdr dll)) (eq (car cll) (car dll)))
		(let ((cl2 (car cll))) ; already have a perfect intermediate class
		  (push cl (libclass-kids cl2))
		  (dolist (p pars)
		    (setf (libclass-kids p)
			  (remove cl (libclass-kids p))))
		  (setf (libclass-parents cl) (list cl2)))
	      (let ((cl2 (make-libclass :kids (cons cl dll) :parents cll 
					:name (intern (format nil "GEN.NEEDED.CO.~d" i)))))
		(incf i)
		(dolist (p pars)
		  (setf (libclass-kids p)
			(remove cl (libclass-kids p))))
		(setf (libclass-parents cl) (list cl2))
		(dolist (x cll)
		  (setf (libclass-kids x) (cons cl2 (set-difference (libclass-kids x) dll))))
		(dolist (x dll)
		  (setf (libclass-parents x) (cons cl2 (set-difference (libclass-parents x) cll))))
		(push (cons pars2 cl2) subset-assoc)))))))))

(defun remove-ancestor-classes (cll)
  (let ((cll2 nil))
    (dolist (cl cll)
      (unless (find-if #'(lambda (x)
			   (and (not (eq x cl))
				(is-a-class-ancestor cl x)))
		       cll)
	(push cl cll2)))
    cll2))

(defun find-maximal-subset-classes (cll subset-assoc)
  (let ((msubset-assoc nil))
    (dolist (sub-cl subset-assoc)
      (when (and (subsetp (car sub-cl) cll)
		 (not (find-if #'(lambda (a) (subsetp (car sub-cl) (car a))) msubset-assoc)))
	(setq msubset-assoc
	      (cons sub-cl
		    (remove-if #'(lambda (a) (subsetp (car a) (car sub-cl))) msubset-assoc)))))
    (mapcar #'cdr msubset-assoc)))

(defun find-minimal-superset-classes (cll subset-assoc)
  (let ((msubset-assoc nil))
    (dolist (sub-cl subset-assoc)
      (when (and (subsetp cll (car sub-cl))
		 (not (find-if #'(lambda (a) (subsetp (car a) (car sub-cl))) msubset-assoc)))
	(setq msubset-assoc
	      (cons sub-cl
		    (remove-if #'(lambda (a) (subsetp (car sub-cl) (car a))) msubset-assoc)))))
    (mapcar #'cdr msubset-assoc)))

; generation of library DAG based on needed objects
; returns a list of elements of the form 
; ((<ABBR|GWFF|LIB-CONST> . <name>) <files> <wff> <needed> <needthis>)
; <wff> is NIL for LIB-CONST's
(defun generate-class-scheme-abbrevs-info ()
  (declare (special *lib-masterindex*))
  (msgf "Generating Abbreviation Dependencies In The Library . . ." t
	"(This May Take A While.)" t)
  (when (hash-table-p *lib-masterindex*)
    (let ((all-abbrevs nil)
	  (all-binder-abbrevs nil)
	  (all-consts nil)
	  (all-gwffs nil)
	  (libdag nil))
      (dolist (ga (remove 'equiv core-abbrevlist))
	(when (symbolp ga)
	  (let* ((wff (get ga 'defn))
		 (needed (needed-objects-with-types (remove 'equiv (get-all-defns wff)))))
	    (push (list ga '(:INTERNAL-ABBREV) wff needed) all-abbrevs))))
      (dolist (gb core-binderabbrevlist)
	(when (and (symbolp gb) (get gb 'defn))
	  (let* ((wff (get gb 'defn))
		 (needed (needed-objects-with-types (remove 'equiv (get-all-defns wff)))))
	    (push (list gb '(:INTERNAL-BINDER-ABBREV) wff needed) all-binder-abbrevs))))
      (dolist (gt core-theoremlist)
	(when (symbolp gt)
	  (let* ((wff1 (get gt 'assertion))
		 (wff (if (stringp wff1) (getrwff wff1) (or wff1 (get gt 'defn))))
		 (needed (needed-objects-with-types (remove 'equiv (get-all-defns wff)))))
	    (push (list gt '(:INTERNAL-THEOREM) wff needed) all-gwffs))))
      (maphash
       #'(lambda (key value)
	   (%catch%
	    (let ((abbrs (remove-if-not #'(lambda (x)
					    (and (consp x) (eq (car x) 'ABBR)))
					value))
		  (gwffs (remove-if-not #'(lambda (x)
					    (and (consp x) (eq (car x) 'GWFF)))
					value))
		  (consts (remove-if-not #'(lambda (x)
					     (and (consp x)
						  (eq (car x) 'LIB-CONST)))
					 value)))
	      (when abbrs
		(if (member key core-abbrevlist)
		    (progn
		      (complain key " is an abbreviation defined in the TPS core")
		      (if (cdr abbrs)
			  (msgf "Ignoring library entries for " key " in files:" t)
			(msgf "Ignoring library entry for " key " in file:" t))
		      (dolist (loc abbrs)
			(msgf (cdr loc) t)))
		  (let ((defns nil)
			(def nil))
		    (dolist (loc abbrs)
		      (let ((i (retrieve-item key :type 'ABBR
					      :preferred-dir (directory-namestring (cdr loc))
					      :multiple nil :fail-gently t)))
			(when i
			  (funcall (make-lib-tpsobject 'ABBR) i)
			  (let ((needed (when i (libitem-needed-objects i)))
				(a (assoc (cons (get key 'defn) 
						(get key 'infix)) defns
						:test #'(lambda (x y)
							  (and (consp x)
							       (consp y)
							       (wffeq-ab (car x) (car y))
							       (equal (cdr x) (cdr y)))))))
			    (if a
				(setq defns
				      (substitute
				       (cons (car a)
					     (cons (cons (cdr loc) needed)
						   (cdr a)))
				       a defns))
			      (push (list (cons (get key 'defn) (get key 'infix))
					  (cons (cdr loc) needed)) defns)))))
		      (destroy-all-extras))
		    (if (cdr defns)
			(progn
			  (complain "Conflict in Library Definitions of " key)
			  (let ((count 0))
			    (dolist (d defns)
			      (incf count)
			      (if (cddr d)
				  (progn
				    (msgf count ") Definition in files " t)
				    (dolist (f (cdr d))
				      (msgf "  " (car f) t)))
				(msgf count ") Definition in file " (caadr d) t))
			      (when (cdar d)
				(msgf "[INFIX = " (cdar d) "]" t))
			      (pwff (caar d)))
			    (incf count)
			    (msgf count ") None of These" t)
			    (msgf "Which do you want to classify?" t)
			    (let ((g (get-a-number count)))
			      (unless (= g count)
				(setq def (nth (- g 1) defns))))))
		      (setq def (car defns)))
		    (when def
		      (let* ((wff (caar def))
			     (files (mapcar #'car (cdr def)))
			     (needed (union
				      (remove-duplicates
				       (apply #'append
					      (mapcar #'cdr (cdr def))))
				      (remove 'equiv 
					      (get-all-defns ; includes abbrevs like SUBSET built into TPS
					       wff)))))
			(push (list key files wff (needed-objects-with-types needed))
			      all-abbrevs))))))
	      (when gwffs
		(let ((defns nil)
		      (def nil))
		  (dolist (loc gwffs)
		    (let ((i (retrieve-item key :type 'GWFF
					    :preferred-dir (directory-namestring (cdr loc))
					    :multiple nil :fail-gently t)))
		      (when i
			(funcall (make-lib-tpsobject 'GWFF) i)
			(let* ((needed (libitem-needed-objects i))
			       (wff1 (get key 'represents))
			       (wff (if (stringp wff1) 
					(getrwff wff1) 
				      (or wff1 (get key 'defn))))
			       (a (assoc wff defns :test #'wffeq-ab)))
			  (if a
			      (setq defns
				    (substitute
				     (cons (car a) 
					   (cons (cons (cdr loc) needed)
						 (cdr a)))
				     a defns))
			    (push (list wff (cons (cdr loc) needed)) defns)))))
		    (destroy-all-extras))
		  (if (cdr defns)
		      (if (member key core-theoremlist)
			  (let ((core-defn (assoc key all-gwffs))
				(good-files nil)
				(bad-files nil))
			    (dolist (d defns)
			      (if (wffeq-ab (car d) (caddr core-defn))
				  (setq good-files 
					(append good-files
						(mapcar #'car
							(cadr d))))
				(setq bad-files 
				      (append bad-files
					      (mapcar #'car
						      (cadr d))))))
			    (when good-files
			      (setq all-gwffs
				    (substitute
				     (list key (cons :INTERNAL-THEOREM good-files)
					   (caddr core-defn)
					   (cadddr core-defn))
				     core-defn all-gwffs)))
			    (when bad-files
			      (complain "Some gwffs " key " in library conflict with gwff in TPS core")
			      (if (cdr bad-files)
				  (msgf "Ignoring gwffs " key " in files:" t)
				(msgf "Ignoring gwff " key " in file:" t))
			      (dolist (bf bad-files)
				(msgf bf))))
			(progn
			  (complain "Conflict in Library Gwffs " key)
			  (let ((count 0))
			    (dolist (d defns)
			      (incf count)
			      (if (cddr d)
				  (progn
				    (msgf count ") Definition in files " t)
				    (dolist (f (cdr d))
				      (msgf (car f) t)))
				(msgf count ") Definition in file " (caadr d) t))
			      (pwff (car d)))
			    (incf count)
			    (msgf count ") None of These" t)
			    (msgf "Which do you want to classify?" t)
			    (let ((g (get-a-number count)))
			      (unless (= g count)
				(setq def (nth (- g 1) defns)))))))
		    (setq def (car defns)))
		  (when def
		    (let* ((wff (car def))
			   (files (mapcar #'car (cdr def)))
			   (needed
			    (union
			     (remove-duplicates
			      (apply #'append
				     (mapcar #'cdr (cdr def))))
			     (remove 'equiv
				     (get-all-defns wff)))))
		      (push (list key files wff (needed-objects-with-types needed))
			    all-gwffs)))))
	      (when consts
		(if (member key core-constlist)
		    (progn
		      (complain key " is a constant defined in the TPS core")
		      (if (cdr consts)
			  (msgf "Ignoring library entries for " key " in files:" t)
			(msgf "Ignoring library entry for " key " in file:" t))
		      (dolist (loc consts)
			(msgf (cdr loc) t)))
		  (let ((t-inf-l nil)
			(t-inf nil))
		    (dolist (loc consts)
		      (retrieve-libobject key :type 'LIB-CONST
					  :preferred-dir (directory-namestring (cdr loc))
					  :multiple nil :fail-gently t)
		      (let ((a (assoc (cons (get key 'type)
					    (get key 'infix)) t-inf-l
					    :test #'equal)))
			(if a
			    (setq t-inf-l
				  (substitute
				   (cons (car a) (cons (cdr loc) (cdr a)))
				   a t-inf-l))
			  (push (list (cons (get key 'type) (get key 'infix))
				      (cdr loc)) t-inf-l)))
		      (destroy-all-extras))
		    (if (cdr t-inf-l)
			(progn
			  (complain "Conflict in Library Declarations of " key)
			  (let ((count 0))
			    (dolist (t-inf t-inf-l)
			      (incf count)
			      (setf (get key 'type) (caar t-inf))
			      (if (cddr t-inf)
				  (progn
				    (msgf count ") Declaration " (key . gwff)
					  " in files:"
					  t)
				    (dolist (fi (cdr t-inf))
				      (msgf fi t)))
				(msgf count ") Declaration " (key . gwff) " in file "
				      (cadr t-inf) t))
			      (setf (get key 'type) nil)
			      (when (cdar t-inf)
				(msgf "[INFIX = " (cdar t-inf) "]" t)))
			    (incf count)
			    (msgf count ") None of These" t)
			    (msgf "Which do you want to classify?" t)
			    (let ((g (get-a-number count)))
			      (unless (= g count)
				(setq t-inf (nth g t-inf-l))))))
		      (setq t-inf (car t-inf-l)))
		    (when t-inf
		      (push (list key (cdr t-inf))
			    all-consts))))))
	    (fail (msgf "WARNING: Library Problem With " key t
			"Skipping It"))))
       *lib-masterindex*)
      (dolist (a all-gwffs)
	(push (acons 'GWFF (car a) (cdr a)) libdag))
      (dolist (a all-consts)
	(push (acons 'LIB-CONST (car a) (cdr a)) libdag))
      (dolist (a all-abbrevs)
	(push (acons 'ABBR (car a) (cdr a)) libdag))
      (dolist (a all-binder-abbrevs)
	(push (acons 'BINDER-ABBR (car a) (cdr a)) libdag))
      (msgf "Generating Needed Dependency DAG Information" t)
      (generate-class-scheme-abbrevs-info-simplify-needed libdag))))

(defun needed-objects-with-types (needed)
  (if needed
      (let ((ty (needed-object-type (car needed)))
	    (r (needed-objects-with-types (cdr needed))))
	(if ty
	    (acons ty (car needed) r)
	  r))
    nil))

(defun needed-object-type (x)
  (let ((h (gethash x *lib-masterindex*)))
    (cond ((or (member x core-abbrevlist) (assoc 'ABBR h))
	   'ABBR)
	  ((member x core-binderabbrevlist)
	   'BINDER-ABBR)
	  ((assoc 'LIB-CONST h)
	   'LIB-CONST)
	  ((or (member x core-theoremlist) (assoc 'GWFF h))
	   'GWFF)
	  (t
	   (msgf "Could not find needed type of " x)
	   NIL))))

(defun generate-class-scheme-abbrevs-info-simplify-needed (libdag)
  (let ((slibdag nil)
	(slibdag2 nil)
	(nassoc nil))
    (do ((libdag2 libdag (cdr libdag2)))
	((null libdag2))
      (let* ((i (car libdag2))
	     (needed (cadddr i))
	     (sneeded (generate-class-scheme-abbrevs-info-simplify-needed-1
		       needed libdag)))
	(dolist (s sneeded)
	  (let ((a (assoc s nassoc :test #'equal)))
	    (if a
		(setq nassoc (substitute (cons s (cons (car i) (cdr a)))
					 a nassoc :test #'equal))
	      (push (list s (car i)) nassoc))))
	(push (list (car i) (cadr i) (caddr i) sneeded) slibdag)))
    (dolist (i slibdag)
      (let ((a (assoc (car i) nassoc :test #'equal)))
	(push (list (car i) (cadr i) (caddr i) (cadddr i) (cdr a))
	      slibdag2)
	(setq nassoc (remove a nassoc))))
    slibdag2))
	      
(defun generate-class-scheme-abbrevs-info-simplify-needed-1 (needed libdag)
  (if needed
      (let ((sneeded (generate-class-scheme-abbrevs-info-simplify-needed-1 
		      (cdr needed) libdag)))
	(if (find-if #'(lambda (x)
			 (generate-class-scheme-abbrevs-info-needed*
			  x (car needed) libdag))
		     sneeded)
	    sneeded
	  (cons (car needed)
		(remove-if #'(lambda (x)
			       (generate-class-scheme-abbrevs-info-needed*
				(car needed) x libdag))
			   sneeded))))
    nil))

; return T if m is needed (recursively) by n
(defun generate-class-scheme-abbrevs-info-needed* (n m libdag)
  (let ((proc-needed (list n))
	(all-needed nil)
	(done nil))
    (if (equal n m)
	T
      (progn
	(loop while (and proc-needed (not done)) do
	      (let* ((n2 (car proc-needed))
		     (f (find-if #'(lambda (x) (equal (car x) n2)) libdag))
		     (new-needed (set-difference (when f (cadddr f))
						 (append all-needed proc-needed)
						 :test #'equal)))
		(if (member m new-needed :test #'equal)
		    (setq done t)
		  (progn
		    (push n2 all-needed)
		    (setq proc-needed (append (cdr proc-needed) new-needed))))))
	done))))

; destroy everything that's been loaded from the library, but not what's in the TPS core      
(defun destroy-all-extras ()
  (do ((a (pop global-abbrevlist) (pop global-abbrevlist)))
      ((consp a) (push a global-abbrevlist))
    (when (object-present a nil) (remove-props a)))
  (do ((a (pop global-theoremlist) (pop global-theoremlist)))
      ((consp a) (push a global-theoremlist))
    (when (object-present a nil) (remove-props a)))
  (do ((a (pop global-logconstlist) (pop global-logconstlist)))
      ((consp a) (push a global-logconstlist))
    (when (object-present a nil) (remove-props a)))
  (do ((a (pop global-pmpropsymlist) (pop global-pmpropsymlist)))
      ((consp a) (push a global-pmpropsymlist))
    (when (object-present a nil) (remove-props a))))

(defun remove-props (sym)
  (declare (special core-abbrevlist core-binderabbrevlist core-theoremlist core-constlist))
  (unless (or (member sym core-abbrevlist)
	      (member sym core-binderabbrevlist)
	      (member sym core-theoremlist)
	      (member sym core-constlist))
    (dolist (prop (plist sym))
      (remprop sym prop))))

; cebrown 1/29/03 - goodmodes
(context lib-writing)

(deflibrary add-goodmodes
  (lib-argnames modes-gwffs newmodes newthms)
  (lib-argtypes modes-gwffs symbollist symbollist)
  (lib-arghelp "A symbol naming a pair of a list of modes and a list of gwffs." "List of modes to add" "List of theorems to add")
  (lib-defaultfns (lambda (x y z)
		    (list (if (and (eq x '$) goodmodes (symbolp goodmodes))
			      goodmodes
			    x) y z)))
  (lib-mainfns add-goodmodes)
  (mhelp "Add modes to a list of goodmodes.  Also, add theorems that these goodmodes can prove."))

(deflibrary remove-goodmodes
  (lib-argnames modes-gwffs rmodes rthms)
  (lib-argtypes modes-gwffs symbollist symbollist)
  (lib-arghelp "A symbol naming a pair of a list of modes and a list of gwffs." "List of modes to remove" "List of theorems to remove")
  (lib-defaultfns (lambda (x y z)
		    (list (if (and (eq x '$) goodmodes (symbolp goodmodes))
			      goodmodes
			    x) y z)))
  (lib-mainfns remove-goodmodes)
  (mhelp "Remove modes from a list of goodmodes.  Also, remove theorems that these goodmodes can prove."))

(defun add-goodmodes (modes-gwffs newmodes newthms)
  (setf (get modes-gwffs 'modes-gwffs-modes)
	(union (get modes-gwffs 'modes-gwffs-modes) newmodes))
  (setf (get modes-gwffs 'modes-gwffs-gwffs)
	(union (get modes-gwffs 'modes-gwffs-gwffs) newthms))
  (msgf "Updated " modes-gwffs "." t
	"Use INSERT to save the new information in the library."))

(defun remove-goodmodes (modes-gwffs rmodes rthms)
  (setf (get modes-gwffs 'modes-gwffs-modes)
	(set-difference (get modes-gwffs 'modes-gwffs-modes) rmodes))
  (setf (get modes-gwffs 'modes-gwffs-gwffs)
	(set-difference (get modes-gwffs 'modes-gwffs-gwffs) rthms))
  (msgf "Updated " modes-gwffs "." t
	"Use INSERT to save the new information in the library."))
