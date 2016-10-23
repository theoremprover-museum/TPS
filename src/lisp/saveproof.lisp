;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :core)
(part-of otlnl)

(context save-work-obj)

;;; Author: Dan Nesmith 14 MAR 89
; needs : S-expressions? 

(deffile saveproof
  (extension lisp)
  (part-of otlnl)
  (mhelp "Functions for saving and restoring natural deduction proofs."))

;;;(HX) A slight modification is to use "no default" to replace second "user::dummy".
;;;(May 31, 1995)
(defun prompt-prfname ()
  (declare (special prfname))
  (do ((prfname (prompt-read prfname nil
		       (msgf "Enter the new name for the proof: ") 'symbol 'cl-user::dummy nil)
		(prompt-read prfname nil
		       (msgf "Enter the new name for the proof: ") 'symbol 'cl-user::dummy nil)))
      ((not (proof-lines prfname)) prfname)
      (msgf "Already have a proof by that name." t)))


;;;;;;;;;
;;;
;;; In the following code, the following form is used in order to keep
;;; downward compatibility.
;;; (if (stringp assertion) (gettype 'gwff assertion) assertion)
;;;      ^^^^^^^^^^^^^^^^^^
;;; From now on, all gwffs are recorded as S-expressions, not strings.
;;; (HX May 31 95)
;;;;;;;;;

(defmacro defsavedproof (name date (assertion assert)
			      (nextplan-no num) 
			      (plans plan-list)
			      (lines &rest line-list)
			      code
			 &optional (abbrev-list nil) (comment '(COMMENT "")) (locked '(LOCKED nil))
				   (gwff-name nil) ; cebrown 4/12/01
			      )
  "Reads in a representation of a natural deduction proof and reconstructs it.
CODE is used to test whether the proof has been altered since it was written."
  (declare (ignore assertion nextplan-no plans lines))
  (let ((prfname (gensym)))
  `(let ((,prfname ',name))
     (block defsavedproof
     (in-mode re-read
       (let ((first-order-mode-parse nil))
     (when (and (not expertflag) (proof-lines ,prfname))
       (unless (query (conc-strings "A proof by the name of " (string ,prfname)
				    " already exists.  Shall I overwrite it?") t)
	 (return-from defsavedproof nil)))
     (when (and expertflag (proof-lines ,prfname))
	   (unless (query (conc-strings "A proof by the name of " (string ,prfname)
					" already exists. Shall I overwrite it?") t)
		   (if (query "Shall I restore the proof under some other name?" t)
		       (setq ,prfname (prompt-prfname))
		     (return-from defsavedproof nil))))
     (when (and (not expertflag)
		(/= ,code (mod (easy-code-list (list ,prfname (status-userid) ',date ',assert
					   ',num ',plan-list
					   ',line-list))
			       *modulo*)))
       (throwfail "I can't understand this proof."))
     ;; don't want students to overwrite the assertion property of a theorem
     ;; since that is checked when DONE is issued.
     ,(dolist (abbr abbrev-list) (eval abbr)) ;; cebrown 3/11/2001 - changed this to define the abbreviations before trying to parse wffs
     (setf (get ,prfname 'comment) (cadr ',comment))
     (when (or expertflag (not (proof-assertion ,prfname)))
       (setf (proof-assertion ,prfname)
	     ',(if (stringp assert) (gettype 'gwff assert) (restoreproof-gwff assert))))
     (if (stringp (proof-assertion ,prfname))
	 (setf (proof-assertion ,prfname) 
	       (gettype 'gwff (proof-assertion ,prfname))))
     (setf (nextplan-no ,prfname) ,num)
     (let ((linealiases nil)
	   (proof-lines nil))
       (dolist (line ',line-list)
	 (let ((linename (gensym "L"))
	       (linenumber (car line))
	       (hyps (cadr line))
	       (assertion (third line))
	       (just-rule (fourth line))
	       (just-terms (fifth line))
	       (just-lines (sixth line))
	       (line-comment (seventh line))
	       (linelock (member (car line) (cadr ',locked))))
	   (push (cons linenumber linename) 
		 linealiases)
	   (push linename proof-lines)
	   (setf (line-linenumber linename) linenumber)
	   (setf (line-hypotheses linename) 
		 (mapcar #'(lambda (x) 
			     (cdr (assoc x linealiases)))
			 hyps))
	   (setf (line-assertion linename) 
		 (if (stringp assertion) (gettype 'gwff assertion)
		   (restoreproof-gwff assertion)))
	   (setf (line-justification linename) (make-list 3))
	   (setf (line-just-rule linename) just-rule)
	   (setf (line-just-terms linename) 
		 (mapcar #'(lambda (x) (if (stringp x) (gettype 'gwff x) (restoreproof-gwff x)))
			 just-terms))
	   (setf (get linename 'comment) line-comment)
	   (setf (get linename 'locked) (if linelock t nil))
	   (setf (line-just-lines linename) 
		 (mapcar #'(lambda (x) 
			     (cdr (assoc x linealiases)))
			 just-lines))))
       (setq dproof ,prfname)
       (setq *dproof-list* (adjoin dproof *dproof-list*))
       (setf (proof-plans ,prfname) 
	     (mapcar 
	       #'(lambda (y) 
		   (mapcar #'(lambda (x)
			       (cdr (assoc x linealiases)))
			   y))
	       ',plan-list))
       (setf (proof-linealiases ,prfname)
	     (nreverse linealiases))
       (setf (proof-lines ,prfname)
	 (nreverse proof-lines))
       (setf (get ,prfname 'gwff-name) ',gwff-name)
       (dolist (line (proof-lines ,prfname)) 
	       (when (get line 'comment) (setf (get line 'comment) (parse-comment (get line 'comment)))))
       (when (get dproof 'comment) (setf (get dproof 'comment) (parse-comment (get dproof 'comment))))
       (find-all-gaps) 
       (format t "Proof ~A restored." ,prfname) ,prfname)))))))

; cebrown 8/29/01 - to put type information on variables
; this is for backwards compatibility as some old .prf files
; saved wffs as s-expressions instead of strings
(defun restoreproof-gwff (gwff)
  (if (boundwff-p gwff)
      (acons (restoreproof-gwff (bindvar gwff))
	     (binder gwff)
	     (restoreproof-gwff (cdr gwff)))
    (if (consp gwff)
	(cons (restoreproof-gwff (car gwff))
	      (restoreproof-gwff (cdr gwff)))
      (if (logconst-p gwff)
	  gwff
	(if (symbolp gwff)
	    (let ((s (format nil "~A" gwff)))
	      (nsubstitute '#\( '#\< s)
	      (nsubstitute '#\) '#\> s)
	      (getrwff s))
	  gwff)))))

(defun write-saved-proof (proof &optional (name proof))
  (write-saved-proof-real proof "defsavedproof" name))

; mkaminski 11/21/2005 -- separated write-saved-proof and
;   write-saved-proof-real

(defun write-saved-proof-real (proof restore-macro &optional (name proof))
  (unless (proof-lines proof)
    (throwfail proof " is not a proof."))
  (in-mode re-read
    (let* ((lines (proof-lines proof))
	  (assertion (proof-assertion proof))
	  (plans (proof-plans proof))
	  (comment (comment-to-string (get proof 'comment)))
	  (locked (mapcar #'linealias 
			  (remove-if-not #'(lambda (x) (get x 'locked)) lines)))
	  (plan-list (mapcar #'(lambda (x)
			  (mapcar #'linealias x))
			     plans))
	  (gwff-name (get proof 'gwff-name)) ;  cebrown 4/12/01
	  (num (nextplan-no proof))
	  (date (status-date))
	  (abbrev-done nil)
	  (constl-done nil)
	  (abbrev nil)
	  (constl nil)
	  (outstring "")
	  (obj nil)
	  (code 0))
      (declare (special abbrev-done constl-done))
      (when (stringp assertion) (setq assertion (gettype 'gwff assertion)))
      (if expertflag
	  (progn
	    (msgf "(" restore-macro " " name t "  " date  t 
		  "  (assertion ")
	    (msg (assertion . gwff))
	    (setq abbrev (abbr-list assertion))
	    (msg ")" t "  (nextplan-no " num ")" t "  (plans ")
	    (princ plan-list)
	    (msg ")" t ;"  (comment " comment ")" t 
		 "  (lines " t)
	    (dolist (line lines)
	      (msg  "    (") 
	      (msg  (line-linenumber line) " ")
	      (princ (mapcar #'linealias (line-hypotheses line)))
	      (msg " " ((line-assertion line) . gwff) " ")
	      (setq abbrev (append (abbr-list (line-assertion line)) abbrev))
	      (setq constl (append (const-list (line-assertion line)) constl))
	      (prin1 (line-just-rule line))
	      (msg " (")
	      (dolist (term (line-just-terms line))
		      (msg " " (term . gwff)))
	      (msg ") ")
	      (princ (mapcar #'linealias (line-just-lines line)))
	      (when (get line 'comment) 
		(msg "  \"" (comment-to-string (get line 'comment)) "\"" ))
	      (msg ")" t))
	    (msg ") 0" t)
	    (when (or abbrev constl)
	      (msg "  ( " t)
	      (write-saved-proof-defs abbrev constl)
	      (msg ")" t))
	    (when (not (or abbrev constl)) (msg "( )" t))
	    (msg "  (comment \"" comment "\")" 
		 t "  (locked " locked ")" t
		 "  " gwff-name " " t) ; cebrown 4/12/01
	    (msg ")" t))
	(progn
	  (setq outstring
	    (with-output-to-string (*standard-output*)
	      (msgf "(" restore-macro " " proof t "  " date  t 
		    "  (assertion ")
	      ;(prin1 assertion) leftover from S-expressions, but it
	      ; bothers ETPS's EASY-CODE-LIST
	      (msg (assertion . gwff))
	      (msg ")" t "  (nextplan-no " num ")" t "  (plans ")
	      (princ plan-list)
	      (msg ")" t 
		   "  (lines " t)
	      (dolist (line lines)
		(msg  "    (") 
		(msg  (line-linenumber line) " ")
		(princ (mapcar #'linealias (line-hypotheses line)))
		(msg " " ((line-assertion line) . gwff) " ")
		(prin1 (line-just-rule line))
		(msg " (")
		(dolist (term (line-just-terms line))
			(msg " " (term . gwff)))
		(msg ") ")
		(princ (mapcar #'linealias (line-just-lines line)))
		(msg ")" t))
	      (msg "))")))
	      ;(msg ")" t "()" t " (comment \"" comment "\"))")))
	  (with-input-from-string (in outstring) (setq obj (read in)))
	  (setq code 
	    (rem 
	     (easy-code-list 
	      (list proof (status-userid) date
		    (cadr (fourth obj)) num
		    plan-list (cdr (seventh obj))))
	     *modulo*))
	  (write-string outstring *standard-output* :end (1- (length outstring)))
	  (format t " ~D ~%" code)
	  (msg "( ) (comment \"" comment "\")" t 
	       "  (locked " locked ")" t "  " gwff-name ")" t))))))

(defun write-saved-proof-defs (abbrev constl)
  (declare (special abbrev-done constl-done))
  (if constl
      (progn
	(write-saved-proof-defs abbrev (cdr constl))
	(let* ((d (car constl))
	       (c (or (get d 'stands-for) d)))
	  (unless (member c constl-done)
	    (push c constl-done)
	    (if (get c 'typelist)
		(msg "    (def-pmpropsym " c t)
	      (msg "    (def-logconst " c t))
	    (when (get c 'type) (msg "      (type \"") 
		  (type-to-lib-stream (get c 'type) *standard-output*) (msg "\")" t))
	    (when (get c 'typelist) (msg "      (typelist ") 
		  (prin1 (mapcar #'princ-to-string (get c 'typelist))) (msg ")" t))
	    (when (get c 'printnotype) (msg "      (printnotype t)" t))
	    (when (get c 'fo-single-symbol) (msg "      (fo-single-symbol t)" t))
	    (when (get c 'face) (msg "      (face ")
		  (prinlc (get c 'face)) (msg ")" t))
	    (when (get c 'infix) (msg "      (infix ") (prin1 (get c 'infix)) (msg ")" t))
	    (msg "      )" t))))
    (if abbrev
	(progn
	  (write-saved-proof-defs (cdr abbrev) nil)
	  (let* ((abbr0 (car abbrev))
		 (abbr (or (get abbr0 'stands-for) abbr0))
		 (def (get abbr 'defn)))
	    (unless (member abbr abbrev-done)
	      (push abbr abbrev-done)
	      (when def (write-saved-proof-defs (abbr-list def) (const-list def)))
	      (msg "    (def-abbrev " abbr t)
	      (when (get abbr 'type) (msg "      (type \"") 
		    (type-to-lib-stream (get abbr 'type) *standard-output*) (msg "\")" t))
	      (when (get abbr 'typelist) (msg "      (typelist ") 
		    (prin1 (mapcar #'princ-to-string (get abbr 'typelist))) (msg ")" t))
	      (when (get abbr 'printnotype) (msg "      (printnotype t)" t))
	      (when (get abbr 'fo-single-symbol) (msg "      (fo-single-symbol t)" t))
	      (when (get abbr 'face) (msg "      (face ")
		    (prinlc (get abbr 'face)) (msg ")" t))
	      (when (get abbr 'infix) (msg "      (infix ") (prin1 (get abbr 'infix)) (msg ")" t))
					;(when (get abbr 'thm-type) (msg "      (thm-type ") (prin1 (get abbr 'thm-type)) (msg ")" t))
	      (when def (msg "      (defn " (def . gwff) ")" t))
	      (when (get abbr 'defn-fun) (msg "      (defn-fun ") (prin1 (get abbr 'defn-fun)) (msg ")" t))
	      (msg "      )" t)))))))

(defun sort-abbrlist (a b)
  (let ((blist (mapcar #'(lambda (x) (or (get x 'stands-for) x)) (abbr-list (get b 'defn)))))
;idea is to return T if we really definitely need to load A before B
    (memq a blist)))

(defun depends-abbrlist (a b)
  (let ((blist (mapcar #'(lambda (x) (or (get x 'stands-for) x)) (abbr-list (get b 'defn))))
	(alist (mapcar #'(lambda (x) (or (get x 'stands-for) x)) (abbr-list (get a 'defn)))))
;idea is to return T if either A depends on B or vice versa
    (or (memq a blist) (memq b alist))))

(defun find-indep (alist)
  (let* ((rlist (find-indep2 alist nil))
	 (rlist (find-indep1 rlist alist nil)))
    rlist))

(defun find-indep1 (rlist alist blist)
  (if (null rlist) blist
    (if (abbr-depends (car rlist) alist)
	(find-indep1 (cdr rlist) alist blist)
      (find-indep1 (cdr rlist) alist (cons (car rlist) blist)))))
    
(defun find-indep2 (alist rlist)
  (if (null alist) rlist
    (if (abbr-depends (car alist) (cdr alist))
	(find-indep2 (cdr alist) rlist)
      (find-indep2 (cdr alist) (cons (car alist) rlist)))))

(defun abbr-depends (elt l)
  (if (null l) nil
    (if (sort-abbrlist (car l) elt)
	t
      (abbr-depends elt (cdr l)))))

(defun full-abbr-sort (alist)
  (let ((alist (remove-if #'null (remove-duplicates alist))))
    (if (< (length alist) 2) alist
      (let* ((indep (find-indep alist))
	     (alist (setdiff alist indep)))
	(if (null indep) 
	    (append (car alist) (full-abbr-sort (cdr alist)))
	  ;indep should never be null -- something must be defined without reference
	  ;to the remaining abbrs -- but this is just here to ensure termination if
	  ;something weird is happening.
	  (append indep (full-abbr-sort alist)))))))

(defun easy-code-list (object)
  (let ((sum 
	 (typecase object
	   (integer object)
	   ((or symbol string)
	    (reduce #'(lambda (n char) (+ n (char-int char)))
		    (string object)
		    :initial-value 0))
	   (list (reduce #'(lambda (n elm) (+ n (easy-code-list elm)))
			 object :initial-value 0))
	   (t (throwfail (format nil "Cannot code object ~S."
				 object))))))
    sum))




(defmexpr saveproof
  (argtypes filespec)
  (argnames savefile)
  (arghelp "File in which to save proof")
  (defaultfns (lambda (savefile)
		(list (if (eq savefile '$)
			  (namestring
			   (make-pathname% :name (string-downcase
						   (string dproof)) :type "prf"
					   :version :newest))
			  savefile))))
  (mainfns saveproof)
  (mhelp "Saves the current natural deduction proof to the specified file in
a form in which it can be restored.  Use RESTOREPROOF to restore the proof.
Overwrites the file if it already exists."))

(defmexpr save-subproof
  (argtypes filespec line-range-list symbol)
  (argnames savefile lines subname)
  (arghelp "File in which to save proof" "List of line ranges to save" "Name for new subproof")
  (defaultfns (lambda (savefile lines newname)
		(list (if (eq savefile '$)
			  (namestring
			   (make-pathname% :name (string-downcase
						   (string dproof)) :type "prf"
					   :version :newest))
			  savefile)
		      lines newname)))
  (mainfns save-subproof)
  (mhelp "Saves part of the current natural deduction proof to 
the specified file in a form in which it can be restored.  
The line ranges specified will be increased to include all the
other lines on which the given lines depend. See the help 
message for LINE-RANGE to find out what a line-range should 
look like. An example list is: 1--10 15--23 28 34--35
Also creates a new proof in memory with the given name, and
makes that the current proof.
Use RESTOREPROOF to restore the proof.
Overwrites the file if it already exists."))

(context proof-outline)
(defmexpr create-subproof
  (argtypes line-range-list symbol)
  (argnames lines subname)
  (arghelp "List of line ranges to use" "Name for new subproof")
  (mainfns create-subproof)
  (mhelp "Creates a new proof in memory from the given lines,
plus all the lines on which they depend, and makes that 
the current proof."))

(context save-work-obj)
(defmexpr restoreproof
  (argtypes filespec)
  (argnames savefile)
  (arghelp "File in which proof resides")
  (defaultfns (lambda (savefile)
		(list (if (eq savefile '$)
			  (namestring
			   (make-pathname% :name (string-downcase
						   (string dproof)) :type "prf"
					   :version :newest))
			  savefile))))
  (mainfns restoreproof)
  (mhelp "Reads a natural deduction proof from a file created by SAVEPROOF
and makes it the current proof.  A security feature prevents the 
restoration of saved proofs which have been altered in any way.
Retrieve any definitions which are used in the proof and stored in the
library before restoring the proof. If you don't specify a directory,
it will first try your home directory and then all the directories 
listed in SOURCE-PATH."))

(defmexpr findproof
  (argtypes string)
  (argnames name)
  (arghelp "Name to search for")
  (mainfns findproof)
  (mhelp "Searches your home directory and the directories listed in
SOURCE-PATH, looking for a proof whose name contains the given string."))

(defun saveproof (savefile)
  (when (pathnamep savefile) (setq savefile (namestring savefile)))
  (setq savefile
	(merge-pathnames savefile
			 (make-pathname% :name (string dproof) 
					 :type "prf"
					 :version :newest)))
  (with-open-file (*standard-output* savefile
				     :direction :output
				     :if-exists :supersede
				     :if-does-not-exist :create)
    (write-saved-proof dproof))
  (format t "File ~A written." (namestring savefile)))

(defun findproof (string)
  (findproof-real string (cons (user-homedir-pathname) source-path)))

(defun findproof-real (str source-path)
  (let ((list nil))
    (dolist (dir source-path)
	    (push (directory dir) list))
    (setq list (remove-if-not #'(lambda (x) (string-equal "prf" (pathname-type x)))
			      (apply 'append list)))
    (when list 
    (do* ((objs list (cdr objs))
	  (objtext (file-namestring (car list)) (if (car objs) (file-namestring (car objs))))
	  (file (car list) (car objs))
	  (searchtext (string-downcase str))
	  (offset (length searchtext))
	  (found nil nil))
	 ((null objtext))
	 (do ((counter 0 (1+ counter)))
	     ((or found (>= (+ counter offset) (1+ (length objtext)))))
	     (setq found (string= objtext searchtext :start1 counter :end1 (+ offset counter)))
	     (when found (msgf objtext ",  written on "
		   (multiple-value-bind (second minute hour date month year
						day-of-week daylight-saving-time-p time-zone)
					(decode-universal-time (file-write-date file))
					(declare (ignore daylight-saving-time-p time-zone))
					(format nil "~?, ~? ~2D, ~D at ~2D:~2,'0D:~2,'0D.~%"
						day-of-week-format-string
						#+spice day-of-week #-spice (list day-of-week)
						month-format-string #+spice month #-spice (list month)
						date year hour minute second))))
	     )))))


(defun restoreproof (savefile)
  (restoreproof-real savefile (cons nil source-path)))

(defun restoreproof-real (sav source-path)
  (let ((exp nil)
	(savefile nil))
    (setq savefile
	  (merge-pathnames sav
			   (make-pathname% :directory (car source-path)
					   :name (string dproof) 
					   :type "prf"
					   :version :newest)))
    (if (probe-file savefile)
	(progn 
	  (with-open-file (in savefile :direction :input
			      :if-does-not-exist :error)
			  (setq exp (read in)))
	  (if (and (consp exp) (eq (car exp) 'defsavedproof))
	      (eval exp)
	    (throwfail "File does not contain a saved proof.")))
      (if (cdr source-path) 
	  (restoreproof-real sav (cdr source-path))
	(throwfail "File " sav " does not exist; check SOURCE-PATH.")))))

(defun create-subproof (lines newname)
  (let ((savefile (new-tmp-filename)))
  (setq savefile
	(merge-pathnames savefile
			 (make-pathname% :name (string dproof) 
					 :type "prf-tmp"
					 :version :newest)))
  (write-saved-subproof dproof lines savefile newname)
  (delete-file savefile)))

(defun save-subproof (savefile lines newname)
  (setq savefile
	(merge-pathnames savefile
			 (make-pathname% :name (string dproof) 
					 :type "prf"
					 :version :newest)))
  (write-saved-subproof dproof lines savefile newname)
  (format t "File ~A written." (namestring savefile)))

(defun write-saved-subproof (proof lines savefile newname)
  (unless (proof-lines proof)
    (throwfail proof " is not a proof."))
  (let* ((lines (mapcan #'(lambda (x)
			   (let ((first (car x))
				 (last (cdr x))
				 (list nil))
			     (dotimes (i (1+ (- last first)) (nreverse list))
				      (if (numalias (+ i first))
					  (push (numalias (+ i first)) list))))
			   )
		       lines))
	 (lines (add-hypotheses (add-justifications (sort lines #'> :key 'linealias))))
	 (lines (mapcar #'(lambda (x) (get x 'linenumber)) lines)))
    ;lines is now a list of line numbers.
    (with-open-file (*standard-output* savefile
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)
		    (write-saved-proof proof newname))
					;this writes the entire proof to a file
    (eval (interpret-tps-top (list 'restoreproof savefile)))
					;this brings it back
					;(it's the easiest way to copy an entire proof)
    (with-open-file (*standard-output* "/dev/null"
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)
		    (dolist (line (reverse (proof-lines dproof)))
					;remember dproof is now a copy of the original
			    (unless (member (get line 'linenumber) lines)
				    (dellines (list line)))))
    (with-open-file (*standard-output* savefile
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)
		    (write-saved-proof dproof newname))))

(defun add-justifications (linelist)
  (let ((temp-ll linelist))
    (dolist (line linelist)
	    (setq linelist (apply #'append (append (list linelist) (last (get line 'justification))))))
    (setq linelist (sort (remove-duplicates (remove-if #'null linelist)) #'> :key 'linealias))
    (if (set-eq temp-ll linelist) linelist (add-justifications linelist))))

(defun add-hypotheses (linelist)
  (let ((temp-ll linelist))
    (dolist (line linelist)
	    (setq linelist (append linelist (get line 'hypotheses))))
    (setq linelist (sort (remove-duplicates (remove-if #'null linelist)) #'> :key 'linealias))
    (if (set-eq temp-ll linelist) linelist (add-hypotheses linelist))))

(context proof-outline)
(defmexpr proof-comment
  (argtypes string)
  (argnames comment)
  (arghelp "Comment to attach to proof")
  (defaultfns (lambda (comment)
		(list (if (eq comment '$) (comment-to-string (get dproof 'comment)) comment))))
  (mhelp "Attaches a comment to the current proof.
The default value is the current comment. Uses the same
comment syntax as LINE-COMMENT; see the help message of that command
for more information. You can see the comments on all the 
current proofs by using PROOFLIST."))

(defun proof-comment (comment)
  (setf (get dproof 'comment) (parse-comment comment)))

(defmexpr merge-proofs
  (argtypes symbol symbol)
  (argnames proof subproof)
  (arghelp "Name of main proof" "Name of subproof to merge into it")
  (mainfns merge-proofs)
  (mhelp ("Merges all of the lines of a subproof into the current 
proof. If EXPERTFLAG is NIL, no line number may occur in both proofs.
If EXPERTFLAG is T, then if a line number occurs in both proofs, the 
lines to which they refer must be the same (with one exception: if 
one is a planned line and the other is the same line with a 
justification, then the justified line will overwrite the planned one).
Compare TRANSFER-LINES." t t
"The following proofs are in memory:" t
(l *dproof-list*) t "For more details, use the PROOFLIST command." t t)))

(defmexpr transfer-lines
  (argtypes symbol symbol line-range-list)
  (argnames proof subproof lines)
  (arghelp "Name of main proof" "Name of proof to transfer lines from" "Range of lines to transfer")
  (mainfns merge-proofs-2)
  (mhelp ("Copies all of the given lines of a subproof, and
all lines on which they depend, into the current proof. 
If EXPERTFLAG is NIL, no line number may occur in both proofs.
If EXPERTFLAG is T, then if a line number occurs in both proofs, the 
lines to which they refer must be the same (with one exception: if 
one is a planned line and the other is the same line with a 
justification, then the justified line will overwrite the planned one).
Different comments from two otherwise identical lines will be 
concatenated to form the comment in the resulting proof.

This is equivalent to CREATE-SUBPROOF followed by MERGE-PROOFS."
t t
"The following proofs are in memory:" t
(l *dproof-list*) t "For more details, use the PROOFLIST command." t t)))

(defun merge-proofs-2 (proof subproof lines)
  (unless (and (proof-lines proof) (proof-lines subproof))
	  (throwfail "One of those is not a proof." ))
  (setq dproof subproof)
  (let* ((name (gensym)))
    (create-subproof lines name)
    (setq name dproof)
    (setq dproof proof)
    (merge-proofs proof name)
    (pall)
    (setf (proof-lines name) nil)
    (remove-if #'(lambda (x) (eq x name)) *dproof-list*)))

(defun merge-proofs (proof subproof)
  (unless (and (proof-lines proof) (proof-lines subproof))
	  (throwfail "One of those is not a proof." ))
  ;idea: hide the old (proof-lines proof)
  ;merge the two sets of prooflines appropriately.
  ;setf (proof-lines proof) to that merged set
  ;saveproof then restoreproof to deal with the gaps, etc.
  (reconsider proof)
  (let* ((newlines (merge-prooflines (proof-lines proof) (proof-lines subproof)))
	 (linealiases (mapcar #'(lambda (x) (cons (get x 'linenumber) x)) newlines))
	 (filename (new-filename))
	 (plans (remove-if-not #'(lambda (x) (member (car x) newlines)) (append (proof-plans proof) (proof-plans subproof)))))
    (setq filename
	  (merge-pathnames filename
			   (make-pathname% :name (string proof) 
					   :type "prf-tmp"
					   :version :newest)))
    (setf (proof-lines proof) newlines)
    (setf (proof-linealiases proof) linealiases)
    (setf (proof-plans proof) plans)
    (saveproof filename)
    (setq *dproof-list* (remove-if #'(lambda (x) (eq x proof)) *dproof-list*))
    (setf (proof-lines proof) nil)
    (restoreproof filename)
    (delete-file filename)
))

(defun merge-prooflines (l1 l2)
  (if (null l1) l2
    (if (null l2) l1
      (let ((n1 (get (car l1) 'linenumber))
	    (n2 (get (car l2) 'linenumber)))
	(if (> n1 n2) (cons (car l2) (merge-prooflines l1 (cdr l2)))
	  (if (> n2 n1) (cons (car l1) (merge-prooflines l2 (cdr l1)))
	    (if (not expertflag)
		(throwfail "Both the proof and the subproof have a line " n1 "." t 
			   "Cannot merge them while EXPERTFLAG is NIL.")
	      (let ((hyps1 (mapcar #'(lambda (x) (get x 'linenumber)) (get (car l1) 'hypotheses)))
		    (hyps2 (mapcar #'(lambda (x) (get x 'linenumber)) (get (car l2) 'hypotheses))))
		(if (not (set-eq hyps1 hyps2))
		    (throwfail "Line " n1 " appears in both proofs with different hypotheses.")
		  (let ((wff1 (get (car l1) 'assertion))
			(wff2 (get (car l2) 'assertion)))
		    (if (not (wffeq wff1 wff2))
			(throwfail "Line " n1 " appears in both proofs with different assertions.")
		      (if (or (symbolp (car (get (car l1) 'justification))) ;i.e. it's a plan
			      (symbolp (car (get (car l2) 'justification))) ;i.e. it's a plan
			      (string= (car (get (car l1) 'justification))
				       (car (get (car l2) 'justification))))
			  (let* ((comment1 (comment-to-string (get (car l1) 'comment)))
				 (comment2 (comment-to-string (get (car l2) 'comment)))
				 (same-comment (string= comment1 comment2)))
			    (if (symbolp (car (get (car l1) 'justification)))
				(progn (unless same-comment 
					       (line-comment (car l2) 
							     (concatenate 'string comment2 " " comment1)))
				       (cons (car l2) (merge-prooflines (cdr l1) (cdr l2))))
			      (progn (unless same-comment 
					     (line-comment (car l1) 
							   (concatenate 'string comment1 " " comment2)))
				     (cons (car l1) (merge-prooflines (cdr l1) (cdr l2))))))
			(throwfail "Line " n1 " is not a plan in either proof, but the justifications" t
				   "for it are different in each case.")
))))))))))))
				    

