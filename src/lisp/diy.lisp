;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of etr-nat)
(context mating-search)

(deffile diy
  (part-of etr-nat)
  (extension clisp)
  (mhelp "Defines functions for calling matingsearch on current planned line."))

(defun free-vars-in-lines (linelist)
  (let ((free-vars nil))
    (dolist (line linelist (delete-duplicates free-vars :test #'eq))
      (setq free-vars
	    (nconc free-vars (free-vars-of (line-assertion line)))))))

(defflag use-diy
  (flagtype boolean)
  (default nil)
  (subjects etr-nat mating-search tactics otl-vars transmit)
  (mhelp "When T, proof lines which are proven by DIY, DIY-L or UNIFORM-SEARCH-L 
will not be translated into natural deduction style, but will instead be 
justified in a single step, as \"Automatic\" from the support lines. 
A comment will be added to the relevant line of the proof showing the
time taken and the mode used for the automatic proof.

Obviously, ND proofs containing justifications of this sort cannot be translated by
NAT-ETREE."))

(defflag diy2-init-time-limit
  (flagtype integer+-or-infinity)
  (default 2)
  (subjects maintain)
  (mhelp "Initial time limit for running DIY2 and PIY2 iteratively with 
increasing time limits."))

(defflag diy2-num-iterations
  (flagtype integer+-or-infinity)
  (default 1)
  (subjects maintain)
  (mhelp "Number of iterations for DIY2 and PIY2 to run on the same mode with 
increasing time limits."))

(defflag diy2-time-increase-factor
  (flagtype posnumber)
  (default 2)
  (subjects maintain)
  (mhelp "Factor to increase time limit on each iteration when running 
DIY2 and PIY2."))

(defmexpr diy
  (argtypes pline existing-linelist yesno)
  (argnames goal support window)
  (arghelp "Planned Line" "Support Lines" "Open Vpform Window?")
  (defaultfns (lambda (z s w)
		(list (pline-default z)
		      (if (and (not (eq z '$)) (eq s '$))
			  (cdr (assoc z (proof-plans dproof)))
			  s) (if (eq w '$) nil w))))
  (mainfns (lambda (z s w) (diy z s w)))
  (mhelp "DO IT YOURSELF. Calls matingsearch procedure specified by the flag 
DEFAULT-MS with specified planned line and supports, then translates the 
resulting proof to natural deduction. Allows some of the output to be sent to 
a separate vpform window (equivalent to issuing the OPEN-MATEVPW command
before typing DIY)."))

(defmexpr piy
  (argtypes gwff0-or-label symbol line yesno)
  (argnames wff prefix num window)
  (arghelp "Prove Wff" "Name of the Proof" "Line Number for Theorem" "Open Vpform Window?")
  (defaultfns (lambda (wff prefix num window)
		(list (if (and (eq wff '$) (boundp '*last-gwff-typed*) *last-gwff-typed*
			       (not (stringp *last-gwff-typed*)))
			  *last-gwff-typed* wff)
		      (if (eq prefix '$)
			  (if (label-p wff) wff
			    (if (and (boundp '*last-gwff-typed*) *last-gwff-typed* (symbolp *last-gwff-typed*))
				*last-gwff-typed* prefix))
			prefix)
		      (if (eq num '$) 100 num)
		      (if (eq window '$) nil window))))
  (mhelp "PROVE IT YOURSELF. Combines the prove command with diy - allowing a choice
of a mode for trying to prove a theorem automatically."))

#+(and allegro-version>= (version>= 5 0))
(defmexpr diy-with-timeout
  (argtypes pline existing-linelist posinteger yesno)
  (argnames goal support timeout window)
  (arghelp "Planned Line" "Support Lines" "Timeout in Seconds" "Open Vpform Window?")
  (defaultfns (lambda (z s timeout w)
		(list (pline-default z)
		      (if (and (not (eq z '$)) (eq s '$))
			  (cdr (assoc z (proof-plans dproof)))
			  s)
		      (if (eq timeout '$) 1 timeout)
		      (if (eq w '$) nil w))))
  (mainfns (lambda (z s timeout w) (diy-with-timeout z s timeout w)))
  (mhelp "DO IT YOURSELF (with timeout).  Calls diy with a timeout value
in seconds.  The timeout value applies only to mating search.  That is,
as long as mating search succeeds within the allotted time, merging
and translation to natural deduction can take as long as necessary.

This is only available for TPS running under Lisps with multiprocessing
(e.g., Allegro >= 5.0).

See Also: DIY, DIY-L-WITH-TIMEOUT"))

#+(and allegro-version>= (version>= 5 0))
(defmexpr diy-l-with-timeout
  (argtypes pline existing-linelist posinteger yesno line-range)
  (argnames goal support timeout window)
  (arghelp "Planned Line" "Support Lines" "Timeout in Seconds" "Open Vpform Window?" "Line range for new lines?")
  (defaultfns (lambda (z s timeout w r)
		(list (pline-default z)
		      (if (and (not (eq z '$)) (eq s '$))
			  (cdr (assoc z (proof-plans dproof)))
			  s)
		      (if (eq timeout '$) 1 timeout)
		      (if (eq w '$) nil w)
		      (if (and (eq r '$) (not (eq z '$))) (get-useful-range z) r))))
  (mainfns (lambda (z s timeout w r) (diy-l-with-timeout z s timeout w r)))
  (mhelp "DIY for lemmas (with timeout).  Calls diy-l with a timeout value
in seconds.  The timeout value applies only to mating search.  That is,
as long as mating search succeeds within the allotted time, merging
and translation to natural deduction can take as long as necessary.

This is only available for TPS running under Lisps with multiprocessing
(e.g., Allegro >= 5.0).

See Also: DIY-L, DIY-WITH-TIMEOUT"))

(defmexpr diy2
  (argtypes pline existing-linelist yesno yesno yesno filespec yesno yesno)
  (argnames goal support quiet-run expu newcore output timing testwin)
  (arghelp "Planned Line" "Support Lines"
	   "Run quietly?" "EXPUNGE and assert defaults between each proof?"
	   "Start a new core image for each proof attempt?" "Short output file" 
	   "Send timing info to the short file?" "Send output to the TEST window?")
  (defaultfns (lambda (z s q e nc y tim tw)
		(list (pline-default z)
		      (if (and (not (eq z '$)) (eq s '$))
			  (cdr (assoc z (proof-plans dproof)))
			  s)
		      (if (eq q '$) 'yes q)
		      (if (eq e '$) 'yes e)
		      (if (eq nc '$) 'yes nc)
		      (if (eq y '$)
			  (format nil "tps-test-~d.doc" dproof)
			y)
		      (if (eq tim '$) 'yes tim)
		      (if (eq tw '$) 'yes tw))))
  (mhelp "DO IT YOURSELF 2.  Tries to prove an existing line
using a variety of given modes.  This essentially combines the
commands TEST-INIT and TPS-TEST.  See the help message for
TPS-TEST for more information about options.

See Also: DIY, DIY-L, DIY2-L, PIY, PIY2, TEST-INIT, TPS-TEST"))

(defmexpr diy2-l
  (argtypes pline existing-linelist line-range yesno yesno yesno filespec yesno yesno)
  (argnames goal support line-range quiet-run expu newcore output timing testwin)
  (arghelp "Planned Line" "Support Lines" "Line range for new lines?"
	   "Run quietly?" "EXPUNGE and assert defaults between each proof?"
	   "Start a new core image for each proof attempt?" "Short output file" 
	   "Send timing info to the short file?" "Send output to the TEST window?")
  (defaultfns (lambda (z s r q e nc y tim tw)
		(list (pline-default z)
		      (if (and (not (eq z '$)) (eq s '$))
			  (cdr (assoc z (proof-plans dproof)))
			  s)
		      (if (and (eq r '$) (not (eq z '$))) (get-useful-range z) r)
		      (if (eq q '$) 'yes q)
		      (if (eq e '$) 'yes e)
		      (if (eq nc '$) 'yes nc)
		      (if (eq y '$)
			  (format nil "tps-test-~d.doc" dproof)
			y)
		      (if (eq tim '$) 'yes tim)
		      (if (eq tw '$) 'yes tw))))
  (mhelp "DO IT YOURSELF 2 with line range for new lines.  Tries to prove an existing line
using a variety of given modes.  If successful, the new lines are put into the gap
specified.   This essentially combines the commands TEST-INIT and TPS-TEST.
See the help message for TPS-TEST for more information about options.

See Also: DIY, DIY-L, DIY2, PIY, PIY2, TEST-INIT, TPS-TEST"))

(defmexpr piy2
  (argtypes gwff0-or-label symbol line yesno yesno yesno filespec yesno yesno)
  (argnames wff prefix num quiet-run expu newcore output timing testwin)
  (arghelp "Prove Wff" "Name of the Proof" "Line Number for Theorem"
	   "Run quietly?" "EXPUNGE and assert defaults between each proof?"
	   "Start a new core image for each proof attempt?" "Short output file" 
	   "Send timing info to the short file?" "Send output to the TEST window?")
  (defaultfns (lambda (wff prefix num q e nc y tim tw)
		(list (if (and (eq wff '$) (boundp '*last-gwff-typed*) *last-gwff-typed*
			       (not (stringp *last-gwff-typed*)))
			  *last-gwff-typed* wff)
		      (if (eq prefix '$)
			  (if (label-p wff) wff
			    (if (and (boundp '*last-gwff-typed*) *last-gwff-typed* (symbolp *last-gwff-typed*))
				*last-gwff-typed* prefix))
			prefix)
		      (if (eq num '$) 100 num)
		      (if (eq q '$) 'yes q)
		      (if (eq e '$) 'yes e)
		      (if (eq nc '$) 'yes nc)
		      (if (eq y '$)
			  (format nil "tps-test-~d.doc" prefix)
			y)
		      (if (eq tim '$) 'yes tim)
		      (if (eq tw '$) 'yes tw))))
  (mhelp "PROVE IT YOURSELF 2.  Tries to prove a theorem
using a variety of given modes.  This essentially combines the
commands PROVE, TEST-INIT and TPS-TEST.  See the help message
for TPS-TEST for more information about options.

See Also: PIY, DIY, DIY-L, DIY2, DIY2-L, TEST-INIT, TPS-TEST"))

(context compound-tactics)
(deftactic diy-tac
  (nat-ded
   (lambda (pline)
     (let ((support (cdr (assoc pline (proof-plans dproof)))))
       (diy pline support nil)))
   "Calls matingsearch procedure specified by the flag DEFAULT-MS on current
planned line and its supports, then translates the expansion proof to 
natural deduction.  The actual supports used will be the universal closure 
of the supports over any free variables which are not free in their 
hypotheses."
))

(deftactic auto-tac
  (nat-ded
   (repeat 
    (orelse min-prop diy-tac))
   "Does minimal propositional actions then calls mating search if
necessary, and translates the resulting proof."))

(deftactic min-prop
  (nat-ded
   (orelse ml::same-tac 
	   (ifthen use-rulep-tac ml::rulep-tac)
	   ml::truth-tac
	   ml::absurd-tac
	   ml::indirect2-tac
	   make-room 
	   ml::deduct-tac 
	   (ifthen use-rulep-tac ml::econj*-tac ml::econj-tac)
	   (ifthen use-rulep-tac ml::iconj*-tac ml::iconj-tac) )))

(defun diy2 (goal support quiet-run expu newcore file timing testwin)
  (let ((gn (linealias goal))
	(suppn (mapcar #'(lambda (x) (linealias x)) support)))
    (diy2-gen (list
	       (list 'diy2 gn suppn)
	       (list 'diy gn suppn nil)
	       (list 'diy-with-timeout gn suppn)
	       (list nil))
	      goal quiet-run expu newcore file timing testwin)))

(defun diy2-l (goal support range quiet-run expu newcore file timing testwin)
  (let ((pre-linelist (get dproof 'lines))
	(lr1 (car range))
	(lr2 (cdr range)))
    (if (or (> lr2 (linealias goal))
	    (and (not (null support))
		 (< lr1 (reduce #'min (mapcar #'linealias support)))))
	(throwfail "The range should lie between the supports and the planned line."))
    (when (> lr1 lr2) ; cebrown 6/29/03 - shouldn't happen, but just in case
      (throwfail "The line range should be from a smaller line number to a larger line number."))
					; cebrown 6/29/03 - if there are already lines in the range, translation can fail
    (when (find-if #'(lambda (x) (and (<= lr1 (linealias x)) (>= lr2 (linealias x)))) pre-linelist)
      (throwfail "The line range should not contain any preexisting lines.  Use INTRODUCE-GAP."))
    (let ((gn (linealias goal))
	  (suppn (mapcar #'(lambda (x) (linealias x)) support)))
      (diy2-gen (list
		 (list 'diy2-l gn suppn (list 'quote range))
		 (list 'diy-l gn suppn nil (list 'quote range))
		 (list 'diy-l-with-timeout gn suppn)
		 (list nil (list 'quote range)))
		goal quiet-run expu newcore file timing testwin))))

(defun diy2-gen (diy-coms goal quiet-run expu newcore file timing testwin)
  (declare (special core::*lib-masterindex* filename *default-pathname-defaults*))
  #+:mswindows (mp:process-run-function "a" #'identity nil) ; just for windows to handle mp right - for some reason if the first mp: function is with-timeout, windows throws to the debugger
  (let ((modes (get goodmodes 'modes-gwffs-modes))
	(modes2 nil)
	(real-dproof dproof))
    (prompt-read modes nil (msg "Enter a list of modes.") 'symbollist modes
		 ((? (msg "A list of modes to use to test the theorems."))
		  (?? 'symbollist)))
					; we can't just set test-theorems and call tps-test
					; since we don't necessarily know the NAME of the theorem.
    (dolist (mode modes)
      (if (member mode global-modelist)
	  (push mode modes2)
	(when (module-loaded-p 'library)
	  (let ((y (car (remove-if-not #'(lambda (x) (memq (car x) '(mode mode1)))
				       (gethash mode core::*lib-masterindex*)))))
	    (if y
		(progn
		  (core::retrieve-libobject mode :type (car y) :multiple nil)
		  (push mode modes2))
	      (msgf "Cannot find mode " mode t "Deleting it from list of modes." t))))))
    (unless modes2
      (throwfail "No known modes to search with.  Aborting."))
    (setq modes2 (reverse modes2))
    (let ((timeout infinity)
	  (iterations 1)
	  (it-factor 2))
      #-(and allegro-version>= (version>= 5 0))
      (progn
	(msgf "TPS under this version of Lisp does not allow a maximum time limit for proof attempts.")
	(msgf "You may not want to continue.")
	(when (query "Abort Search?" t)
	  (throwfail "Search Aborted")))
      #+(and allegro-version>= (version>= 5 0))
      (progn
	(prompt-read timeout nil (msgf "Initial Timeout in Seconds (or INFINITY) for Each Proof Attempt") ; timeout uses Allegro's multiprocessing - cebrown
		     'integer+-or-infinity diy2-init-time-limit ((? (mhelp 'integer+-or-infinity)) (?? (msgf "The number of seconds to search for a proof of a theorem using a mode"))))
	(unless (equal diy2-init-time-limit timeout)
	  (set-flag 'diy2-init-time-limit timeout))
	(unless (member timeout (list 0 infinity) :test #'equal)
	  (prompt-read iterations nil (msgf "Number of Iteration Attempts")
		       'integer+-or-infinity diy2-num-iterations ((? (mhelp 'posinteger)) (?? (msgf "The number of times to iterate through the pairs of theorems and modes increasing the timeout on each iteration"))))
	  (unless (equal diy2-num-iterations iterations)
	    (set-flag 'diy2-num-iterations iterations))
	  (unless (member iterations '(0 1) :test #'equal)
	    (prompt-read it-factor nil (msgf "Factor by which to Increase Timeout on each Iteration")
			 'posnumber diy2-time-increase-factor
			 ((? (mhelp 'posnumber)) (?? (msgf "The number to multiply the timeout by on each iteration"))))
	    (unless (equal diy2-time-increase-factor it-factor)
	      (set-flag 'diy2-time-increase-factor it-factor)))))
      (let* ((batchcom nil)
	     (tmpdir #+:mswindows ""
		     #-:mswindows "/tmp/")
	     (file1 (extract-file-name file))
	     (prooffiletmp (format nil "~d~d-tmpp.prf" tmpdir file1))
	     (filetmp (format nil "~d~d-tmp" tmpdir file1))
	     (transtmp (format nil "~d~d-trans-tmp" tmpdir file1))
	     (workfileroot (format nil "~d~d-tmpw" tmpdir file1))
	     (workfile (format nil "~d~d-tmpw.work" tmpdir file1)))
	(saveproof prooffiletmp)
	(when newcore
	  (prompt-read batchcom nil (msgf "Command to start TPS in batch mode")
		       'string
		       #+(and allegro-version>= (version>= 5 0) (not mswindows))
		       (format nil "~d -I ~d~d.dxl -- -batch " (car (sys:command-line-arguments)) sys-dir core-name)
		       #-(and allegro-version>= (version>= 5 0) (not mswindows))
		       "tps -batch"
		       ((? (mhelp 'string)) (?? (msgf "The command for starting TPS in batch mode to execute a work file")))))
	(when timing
	  (startcount 'diy2))
	(when (probe-file file) (rename-file file (concatenate 'string (namestring file) ".bak")))
	(let ((success nil)
	      (timedout nil)
	      (timed-out-list nil)
	      (testwin-filename nil)
	      (i 0))
	  (when quiet-run (mode 'maint::quiet))
	  (when expu (unless (and (fboundp 'maint::my-default-mode) (tps-mode-p 'maint::my-default-mode))
		       (maint::init-define-my-default-mode)))
	  (when testwin
	    (unless *executing-batch*
	      (open-testwin-auto))
	    (when (boundp 'filename)
	      (setq testwin-filename filename)))
	  (reroute-output-append file *default-pathname-defaults* 
				 (msgf t t "Starting Search for " LISP-IMPLEMENTATION-TYPE t
				       " running on " MACHINE-TYPE " at " SHORT-SITE-NAME t t
				       " with MODES " t MODES2 t t t))
	  (loop while (and (not success) (or (eq iterations infinity) (< i iterations))) do
		(incf i)
		(dolist (mode modes2)
		  (unless (or success (and (> i 1) (not (member mode timed-out-list :test #'equal))))
		    (if newcore
			(progn
			  (reroute-output
			   workfile *default-pathname-defaults*
			   ; load in the partial proof
			   (msgf "restoreproof " (prooffiletmp . string) t)
			   ; call diy2/diy2-l
			   (dolist (a (car diy-coms))
			     (msg a " "))
			   (msg t quiet-run t expu t "NIL" t (filetmp . string) t timing t testwin t)
			   (msg mode t)
			   #+(and allegro-version>= (version>= 5 0))
			   (progn
			     (msgf timeout t)
			     (unless (member timeout (list 0 infinity) :test #'equal)
			       (msgf "1" t)))
			   (msgf t t)
			   )
			  (if testwin
			      (call-system (format nil "~d ~d -testwinfile ~d" batchcom workfileroot testwin-filename))
			    (call-system (format nil "~d ~d" batchcom workfileroot)))
			  (delete-file workfile)
			  (when (probe-file filetmp)
			    (reroute-output-append
			     file *default-pathname-defaults*
			     (let ((tf (open filetmp :direction :input)))
			       (do ((r (read-line tf nil nil) (read-line tf nil nil)))
				   ((null r)
				    (close tf)
				    (msgf t t)
				    (delete-file filetmp))
				 (if (string-equal "(saveproof" r :end2 (min 10 (length r)))
				     (let ((pffilename (cadr (read-from-string r))))
				       (when (probe-file pffilename)
					 (setf (proof-lines real-dproof) nil)
					 (restoreproof pffilename)
					 (delete-file pffilename)))
				   (if (string-equal "(transmit-file" r :end2 (min 14 (length r)))
				       (let ((miscfile (cadr (read-from-string r))))
					 (when (probe-file miscfile)
					   (read-transmit-info miscfile)
					   (delete-file miscfile)))
				     (progn
				       (when (or (string-equal "Proved" r :end2 (min 6 (length r)))
						 (string-equal "Mated" r :end2 (min 5 (length r))))
					 (setq timed-out-list (remove mode timed-out-list :test #'equal))
					 (setq success t))
				       (when (string-equal "Timed out" r :end2 (min 9 (length r)))
					 (push mode timed-out-list))
				       (when (string-equal "Proof of" r :end2 (min 8 (length r)))
					 (setq timed-out-list (remove mode timed-out-list :test #'equal)))
				       (msgf r)))))))))
		      (progn
			(when expu
			  (expunge)
			  (mode 'maint::my-default-mode) (gc))
			(mode mode)
			(when testwin (testwin-update-three "Proving " (princ-to-string dproof) " in mode " mode 
							    (if quiet-run " + QUIET" " ") t
							    t))
			(when quiet-run (mode 'maint::quiet))
			(if (equal timeout infinity)
			    (comdecode (cadr diy-coms))
			  #+(and allegro-version>= (version>= 5 0))
			  (progn
			    (setq *diy-timed-out* nil)
			    (comdecode (append (caddr diy-coms)
					       (list timeout)
					       (cadddr diy-coms)))
			    (setq timedout *diy-timed-out*))
			  #-(and allegro-version>= (version>= 5 0))
			  (comdecode (cadr diy-coms))
			  )
			(if timedout
			    (progn
			      (setq timedout nil)
			      (push mode timed-out-list)
			      (msgf t t "Proving " dproof " with mode " mode " ran out of time with " timeout " seconds as limit." t)
			      (reroute-output-append file *default-pathname-defaults* 
						     (msgf t t "Timed out trying to prove " dproof " with mode " mode " and time limit of " timeout " seconds." t)
						     (if timing 
							 (progn (stringdt) (msgf t "Timing information for " dproof ":")
								(display-time-in-daterec)
								(maint::memory-status))
						       (stringdt)))
			      (when testwin (testwin-update-three "Timed out trying to prove " (princ-to-string dproof) " with mode " mode " on " (stringdtl nil) "
Timing information for " (princ-to-string dproof) ":
" (with-output-to-string (*standard-output*) (display-time-in-daterec)) "

")))
			  (progn
			    (if (assoc goal (proof-plans dproof))
				(progn 
				  (setq timed-out-list (remove mode timed-out-list :test #'equal))
				  (reroute-output-append file *default-pathname-defaults* 
							 (msgf t t "Proof " dproof " with mode " mode " terminated with planned lines remaining on ")
							 (if timing 
							     (progn (stringdt) (msgf t "Timing information for " dproof ":") (auto::display-time-in-daterec) (maint::memory-status))
							   (stringdt)))
				  (when testwin (testwin-update-three "Proof " (princ-to-string dproof) " with mode " mode " terminated with planned lines remaining on " (stringdtl nil) "
Timing information for " (princ-to-string dproof) ":
" (with-output-to-string (*standard-output*) (display-time-in-daterec)) "

")))
			      (if (wffeq (line-assertion (car (last (proof-lines dproof)))) 
					 (proof-assertion dproof))
				  (progn
				    (setq success t)
				    (when *executing-batch*
				      (saveproof prooffiletmp)
				      (save-transmit-info transtmp timing)
				      )
				    (reroute-output-append file *default-pathname-defaults*
							   (when *executing-batch*
							     (msgf "(saveproof " (prooffiletmp . string) ")" t)
							     (msgf "(transmit-file " (transtmp . string) ")" t)
							     )
							   (msgf t t "Proved " dproof " with mode " mode " on ")
							   (if timing 
							       (progn (stringdt) (msgf t "Timing information for " dproof ":") (auto::display-time-in-daterec) (maint::memory-status))
							     (stringdt)))
				    (when testwin (auto::testwin-update-three "Proved " (princ-to-string dproof) " with mode " mode " on " (stringdtl nil) "
Timing information for " (princ-to-string dproof) ":
" (with-output-to-string (*standard-output*) (display-time-in-daterec)) "

")))
				(progn 
				  (setq timed-out-list (remove mode timed-out-list :test #'equal))
				  (reroute-output-append file *default-pathname-defaults* 
							 (msgf t t "Proof " dproof " with mode " mode " terminated with a proof 
whose last line is not the theorem" t)
							 (if timing 
							     (progn (stringdt) (msgf t "Timing information for " dproof ":") (display-time-in-daterec))
							   (stringdt)))
				  (when testwin (testwin-update-three "Proof " (princ-to-string dproof) " with mode " mode " terminated with a proof
whose last line is not the theorem on " (stringdtl nil) "
Timing information for " (princ-to-string dproof) ":
" (with-output-to-string (*standard-output*) (display-time-in-daterec)) "

")))))))))))
		(when (numberp timeout) (setq timeout (* timeout it-factor))))
	  (when newcore
	    (when (probe-file filetmp) (delete-file filetmp))
	    (when (probe-file workfile) (delete-file workfile)))
	  (when (and (not *executing-batch*) (probe-file prooffiletmp)) (delete-file prooffiletmp)))))
    (when timing
      (timing-sethash 'diy2 'proof-count 1)
      (runcount 'diy2)
      (display-time 'diy2))))

(defun piy2 (wff prefix num quiet-run expu newcore output timing testwin)
  (prove2 wff prefix num)
  (diy2 (numalias num) nil quiet-run expu newcore output timing testwin))

(defun piy (wff prefix num window)
  (unless maint:*dont-ask*
    (unless (if (symbolp wff)
		(find-mode wff nil t)
	      (if (and (symbolp *last-gwff-typed*)  ; added this check -- ceb 6/2/99
		       (wffeq-ab (get *last-gwff-typed* 'represents) wff))
		  (find-mode *last-gwff-typed* nil t)
		(if (symbolp prefix)
		    (find-mode prefix nil t)
		  nil)))
      (if (query "Would you like to give a mode for this theorem?" t)
	  (let ((mode nil))
	    (loop until mode do
		  (prompt-read mode nil (msgf "Mode")
			       'tps-mode '$
			       ((? (msgf "Name of a mode (indicating a collection of flag settings)"))
				(?? (msgf "A mode for automatic search."))))
		  (if (member mode global-modelist)
		      (mode mode)
		    (let ((y (car (remove-if-not #'(lambda (x) (memq (car x) '(mode mode1)))
						 (gethash mode core::*lib-masterindex*)))))
		      (if y
			  (progn
			    (core::retrieve-libobject mode :types (car y) :multiple nil)
			    (mode mode) t)
			(progn
			  (msgf "Can't find that mode in the library." t)
			  (setq mode nil)))))))
	(progn
	  (msgf "Warning: Search will proceed with current flag settings.")
	  (when (query "Abort Search?" t)
	    (throwfail "Search Aborted by User"))))))
  (prove2 wff prefix num)
  (msgf " --- Starting Automatic Search --- ")
  (diy (numalias num) nil window))

(defun extract-file-name (str)
  (let ((l (length str))
	(ret ""))
    (do ((i (- l 1) (- i 1)))
	((or (< i 0)
	     (eq (aref str i)
		 #+:mswindows #\\
		 #-:mswindows #\/
		 ))
	 ret)
      (setq ret (format nil "~d~d" (aref str i) ret)))))

(defun save-transmit-info (transmit-file timing)
  (let ((trans (open transmit-file :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)))
    (format trans "(AUTO::FLAG-INFO~%")
    (dolist (fl global-flaglist)
      (when (atom fl)
	(when (member 'TRANSMIT (get fl 'subjects))
	  (format trans " (~S ~S) " fl (eval fl)))))
    (format trans ")~%")
    (when timing
      (format trans "(AUTO::TIMING-INFO~%")
      (maphash #'(lambda (key val)
		   (unless (eq key 'DIY2)
		     (format trans " (~S ~S)~%" key val)))
	       timing-hash-table)
      (format trans ")~%"))
    (close trans)))

(defun read-transmit-info (transmit-file)
  (let ((a (open transmit-file :direction :input)))
    (do ((c (read a nil nil) (read a nil nil)))
	((null c) (close a))
      (when (consp c)
	(case (car c)
	  (timing-info
	   (dolist (kv (cdr c))
	     (setf (gethash (car kv) timing-hash-table)
		   (cadr kv))))
	  (flag-info
	   (dolist (fv (cdr c))
	     (set (car fv) (cadr fv)))) ; don't call set-flag to avoid side effects
	  (t nil))))))

(defvar *diy-timed-out* nil)

(defun diy-with-timeout (goal support timeout window)
  (diy goal support window nil timeout))

(defun diy-l-with-timeout (goal support timeout window range)
  (diy-lemma goal support window range timeout))

(defun diy (goal support window &optional (lemma nil) (timeout nil))
  (if (and window (not *vpwin-opened*)) (open-vpwin-auto))
  (unsponsor goal (cdr (assoc goal (proof-plans dproof))))
  (sponsor goal (list support))
  (rplacd (assoc goal (proof-plans dproof)) support)
  (setq co-proof-mating-count 
       (timing-gethash 'mating 'proof-count))
  (timing-sethash 'diy2 'proof-count -1)
  (unwind-protect 
     (if (memq default-ms '(mt94-11 mt94-12 mt95-1))
	 (diy-mt-real goal support lemma timeout)
       (if (memq default-ms '(ms03-7 ms04-2))
	   (diy-ext-real goal support lemma timeout)
	 (diy-real goal support lemma timeout)))
     (progn (breakcount 'diy)
	    (display-time 'all))))

(defun diy-real (goal support &optional (lemma nil) (timeout nil))
  (declare (special USE-EXT-LEMMAS *hacked-rewrites-list*
		    *leibniz-var-list* *instantiated-defs-list* *instantiated-eqs-list*))
  (stringdtl)
  (startcount 'diy);hx: Dec. 10, 1992
  (setq *hacked-rewrites-list* nil) ; set these to NIL at the beginning to avoid values from previous run - cebrown 4/3/03
  (setq *leibniz-var-list* nil)
  (setq *instantiated-defs-list* nil)
  (setq *instantiated-eqs-list* nil)
  (if (planp goal)
      (let* (skolem-terms imp-node supp-nodes left-node right-node)
        (setq skolem-terms 
	  (delete-duplicates
	   (mapcar #'(lambda (x) (cons (make-skolem-term :term x
                                                         :parameter x) 0))
                   (free-vars-in-lines 
                    (cons goal 
                          (apply #'append
                                 (mapcar #'(lambda (x) (line-hypotheses x))
                                         support)))))))
        (setq current-eproof
          (make-eproof
	   :free-vars-in-etree nil
	   :substitution-list nil
	   :leaf-list nil
	   :skolem-constants skolem-terms
	   :skolem-method skolem-default
	   :skolem-node-list nil))
	(setf (get (eproof-name current-eproof) 'EPROOF) current-eproof) ; 9/13/01
	(push (eproof-name current-eproof) *eproof-list*) ; 9/13/01
        (setq master-eproof current-eproof)
        (setq imp-node 
          (make-implication :junctive 'con :free-vars nil
                            :parent nil :positive nil))
        (setq supp-nodes
          (mapcar #'(lambda (x)
                      (deepen-to-literals*
                       (let ((node (make-universalized-node x)))
			 (init-symmetry node current-eproof)
			 node)))
                  support))
        (setq left-node
          (make-etree-conjunction supp-nodes imp-node))
        (setq right-node
	  (let ((leaf (make-leaf :shallow (line-assertion goal)
                                 :junctive nil
                                 :positive nil
                                 :free-vars nil)))
	    (update-status nil leaf 1)
	    (init-symmetry leaf current-eproof)
	    (deepen-to-literals* (if truthvalues-hack (add-falsehood leaf) leaf))))
        (setq first-order-mode-ms
          (first-order-problem-p
           (mapcar #'(lambda (x) (cons (exp-var-var (car x)) (cdr x)))
                   (eproof-free-vars-in-etree current-eproof))))
        (setq active-mating nil)
        (do ((supp-nodes supp-nodes (cdr supp-nodes))
             (support support (cdr support)))
            ((null support))
          (setf (line-node (car support)) (car supp-nodes)))
	(init-symmetry right-node current-eproof)
        (setf (line-node goal) right-node)
        (if left-node
            (progn
	      (init-symmetry left-node current-eproof)
              (setf (etree-parent left-node) imp-node)
              (setf (etree-parent right-node) imp-node)
              (setf (etree-components imp-node)
		(list left-node right-node))
              (setf (eproof-etree current-eproof) imp-node))
          (setf (eproof-etree current-eproof) right-node))
	(when USE-EXT-LEMMAS  ; cebrown 10/13/01
	  (extensionality-etree-lemmas))
	(when (and (not DELAY-SETVARS) ; cebrown 11/26/01
		   (or (eq add-truth t) (and (eq add-truth 'if-needed) 
					     (or (update-statuses (eproof-etree current-eproof)) t)
					     (truth-needed (etree-to-jform (eproof-etree current-eproof))))))
	      (setf (eproof-etree current-eproof) (add-trut (eproof-etree current-eproof) 'TRUTH))
	      (unless truthvalues-hack
		      ;in which case all the FALSEHOODs have gone, so we don't need a NOT FALSEHOOD
		      (setf (eproof-etree current-eproof) (add-trut (eproof-etree current-eproof) '(NOT . FALSEHOOD)))))
        (initialize-mating-search)
	(relabel-etree (eproof-etree current-eproof))
	(setq *leibniz-var-list* (leibniz-quant *leibniz-var-list*))
	(when (lazy2-used) (fiddle-with-def-leaves (eproof-etree current-eproof)))
        (update-statuses (eproof-etree current-eproof))
        (unless (gettype 'searchtype default-ms)
          (complain "TPS can not process proofs generated by " default-ms
                    "."  t "Using ms88 instead."))
        (runcount 'preprocess)
        (when 
	    #+(and allegro-version>= (version>= 5 0))
	    (if timeout
		(mp:with-timeout (timeout 
				  (msgf "Mating Search Timed Out After " timeout " Seconds.")
				  (setq *diy-timed-out* t)
				  nil)
				 (matingsearch-controller))
	      (matingsearch-controller))
	    #-(and allegro-version>= (version>= 5 0))
	    (matingsearch-controller)
	  (msg t "Merging expansion proof" t)
	  (stringdt)
	  (finish-output)
	  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
		(let ((*standard-output* *vpwin-opened*))
		  (msg t "Merging expansion proof ")
		  (stringdt)
		  (finish-output *standard-output*)))
	  (merge-tree :num-support-lines (length support))
;;;the following part is used to deal with the interactive calling of
;;;DIY. line-nodes of support-lines are not cleaned up during merging
;;;process. Thm130 is a very good example to illustrate this.
;;; If newnode is NIL, then there is no need to update the support line.
;;;                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;;The claim has not been justified, but it seems correct up to now.       
          (dolist (hxsupport support)
                  (let ((newnode (update-line-node hxsupport current-topnode)))
                    (if newnode (setf (line-node hxsupport) newnode))))
          (when left-node 
	      (if (eproof-lemmas current-eproof)
		  (let* ((imp (cadr (etree-components (eproof-etree
						       current-eproof))))
			 (imp2 (cadr (etree-components imp))))
		    (setf (etree-components imp)
			  (list (car (etree-components imp))
				(cadr (etree-components imp2))))
		    (setf (etree-parent (cadr (etree-components imp2))) imp))
		(setf (eproof-etree current-eproof)
		      (cadr (etree-components (eproof-etree
					       current-eproof))))))
	  (msg t "Translating expansion proof" t)
	  (stringdt)
	  (finish-output)
	  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
		(let ((*standard-output* *vpwin-opened*))
		  (msg t "Translating expansion proof ")
		  (stringdt)
		  (finish-output *standard-output*)))
          (if use-diy (diy-justify goal support)
	    (etree-nat dproof (linealias goal) nil nil))
          (unless (or (proof-plans dproof) lemma)
            (startcount 'printproof)
            (proof-count 'printproof)
	    (apply-tactic 'make-nice :use 'nat-ded :mode 'auto))
	  (runcount 'diy)
	  (terpri) 
	  (terpri)
	  (stringdt)
	  (terpri)
	  (unless (eq mating-verbose 'silent) ; cebrown 9/26/01
	    (tpsbell) (finish-output) (sleep 1/2) (tpsbell) (finish-output) (sleep 1/2) (tpsbell))
	  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
		(let ((*standard-output* *vpwin-opened*))
		  (msg t "Proof in Natural Deduction Style Completed ")
		  (stringdt)
		  (finish-output *standard-output*)))))
    (throwfail "Line " (goal . existing-line) " is not a planned
line.")))

(defun diy-justify (goal support)
  (multiple-value-bind (vh vt) (diffcount 'mating)
		       (declare (ignore vt))
		       (let* ((v1 (or (car vh) 0)) 
			      (v2 (/ (or (cadr vh) 0) internal-time-units-per-second))
			      (v3 (/ (or (caddr vh) 0) internal-time-units-per-second))
			      (str (if (or (< 60 v1) (< 60 v2)) (time-conversion-format v1 v2 v3 (- v2 v3))
				     "seconds")))
			 (setf (get goal 'justification) (list "Automatic" NIL support))
			 (setf (get goal 'core::comment) (list 'msg (concatenate 'string "Proved automatically in " 
								      (format nil "~,2F" (- v2 v3)) " " str ", using " 
								      last-mode-name ".")))
			 (setf (proof-plans dproof) (remove-if #'(lambda (x) (eq (car x) goal)) (proof-plans dproof))))))

(defun make-etree-conjunction (nodes parent)
  (cond ((null nodes) nil)
	((null (cdr nodes)) 
	 (setf (etree-parent (car nodes))
	       parent)
	 (car nodes))
	(t 
	 (let ((conj-node
		(make-econjunction 
		 :positive t
		 :junctive 'con
		 :free-vars nil
		 :parent parent)))
	   (setf (etree-components conj-node)
		 (list (car nodes)
		       (make-etree-conjunction 
			(cdr nodes) conj-node)))
	   (update-status nil conj-node 1)
	   #+comment(push (cons (etree-name conj-node) 1)
			  (auto::eproof-statuses current-eproof))
	   conj-node))))


(defun make-universalized-node (line)
  (let* ((ok-vars 
	 (delete-duplicates
	  (nset-difference (free-vars-of (line-assertion line))
			   (free-vars-in-lines (line-hypotheses line)))))
	 (wff (line-assertion line))
	 (node 
	   (if ok-vars
;;;	        (create-rewrite-node
;;;		 wff t
;;;		 (dolist (var ok-vars wff)
;;;		   (setq wff (acons var 'forall wff)))
;;;		 'ruleq nil nil)
	       (let ((rew 
		       (make-rewrite
			 :shallow wff
			 :positive t
			 :junctive 'neutral
;;;The following is changed to fix a bug in cleanup-rewrite-node (ruleq)
;;;			 :free-vars nil
                         :free-vars ok-vars 
			 :justification 'ruleq-univ
			 :parent nil)))
		 (setf (etree-components rew)
		       (list (make-leaf :shallow 
					(dolist (var ok-vars wff)
					  (setq wff (acons var 'forall wff)))
					:positive t
					:components nil
					:junctive  nil
					:free-vars nil
					:parent rew)))
		 rew)
	       (make-leaf :shallow wff
			  :junctive nil
			  :positive t
			  :components nil
			  :free-vars nil))))
    (update-statuses node)
    node))


;;;If newnode is NIL, then the support line is unused in the proof.
;;;Hence there is no need to update its line-node property.
(defun update-line-node (line etree)
   (let* ((node-name (etree-name (line-node line)))
          (newnode (find-etree-node-name node-name etree)))
      newnode
      #+comment
      (or newnode (throwfail "update-line-node fails. The failure is quite 
probably related to interactive calling of DIY."))))



(defun diy-mt-real (goal support &optional (lemma nil) (timeout nil))
  (declare (special USE-EXT-LEMMAS))
  (let ((old-renumber-leaves renumber-leaves)
	(renumber-leaves nil))
  (stringdtl)
  (startcount 'diy);hx: Dec. 10, 1992
  (if (planp goal)
      (let* (skolem-terms imp-node supp-nodes left-node right-node)
        (setq skolem-terms 
	  (delete-duplicates
	   (mapcar #'(lambda (x) (cons (make-skolem-term :term x
                                                         :parameter x) 0))
                   (free-vars-in-lines 
                    (cons goal 
                          (apply #'append
                                 (mapcar #'(lambda (x) (line-hypotheses x))
                                         support)))))))
        (setq current-eproof
          (make-eproof
	   :free-vars-in-etree nil
	   :substitution-list nil
	   :leaf-list nil
	   :skolem-constants skolem-terms
	   :skolem-method skolem-default
	   :skolem-node-list nil))
	(setf (get (eproof-name current-eproof) 'EPROOF) current-eproof) ; 9/13/01
	(push (eproof-name current-eproof) *eproof-list*) ; 9/13/01
        (setq master-eproof current-eproof)
        (setq imp-node 
          (make-implication :junctive 'con :free-vars nil
                            :parent nil :positive nil))
        (setq supp-nodes
          (mapcar #'(lambda (x)
                      (deepen-to-literals*
                       (let ((node (make-universalized-node x)))
			 (init-symmetry node current-eproof)
			 node)))
                  support))
        (setq left-node
          (make-etree-conjunction supp-nodes imp-node))
        (setq right-node
	  (let ((leaf (make-leaf :shallow (line-assertion goal)
                                 :junctive nil
                                 :positive nil
                                 :free-vars nil)))
	    (update-status nil leaf 1)
	    #+comment(push (cons (etree-name leaf) 1)
		  (eproof-statuses current-eproof))
	    (init-symmetry leaf current-eproof)
	    (deepen-to-literals* (if truthvalues-hack (add-falsehood leaf) leaf))))
        (setq first-order-mode-ms
          (first-order-problem-p
           (mapcar #'(lambda (x) (cons (exp-var-var (car x)) (cdr x)))
                   (eproof-free-vars-in-etree current-eproof))))
        (setq active-mating nil)
        (do ((supp-nodes supp-nodes (cdr supp-nodes))
             (support support (cdr support)))
            ((null support))
          (setf (line-node (car support)) (car supp-nodes)))
	(init-symmetry right-node current-eproof)
        (setf (line-node goal) right-node)
        (if left-node
            (progn
	      (init-symmetry left-node current-eproof)
              (setf (etree-parent left-node) imp-node)
              (setf (etree-parent right-node) imp-node)
              (setf (etree-components imp-node)
		(list left-node right-node))
              (setf (eproof-etree current-eproof) imp-node))
          (setf (eproof-etree current-eproof) right-node))
	(when USE-EXT-LEMMAS  ; cebrown 10/13/01
	  (extensionality-etree-lemmas))
	(when (and (not DELAY-SETVARS) ; cebrown 11/26/01
		   (or (eq add-truth t) (and (eq add-truth 'if-needed) 
					     (truth-needed (etree-to-jform (eproof-etree current-eproof))))))
	      (setf (eproof-etree current-eproof) (add-trut (eproof-etree current-eproof) 'TRUTH))
	      (unless truthvalues-hack
		      ;in which case all the FALSEHOODs have gone, so we don't need a NOT FALSEHOOD
		      (setf (eproof-etree current-eproof) (add-trut (eproof-etree current-eproof) '(NOT . FALSEHOOD)))))
        (initialize-mating-search)
	(when (lazy2-used) (fiddle-with-def-leaves (eproof-etree current-eproof)))
        (update-statuses (eproof-etree current-eproof))
        (runcount 'preprocess)
	(setq renumber-leaves old-renumber-leaves)
        (when 
	    #+(and allegro-version>= (version>= 5 0))
	    (if timeout
		(mp:with-timeout (timeout
				  (msgf "Mating Search Timed Out After " timeout " Seconds.")
				  (setq *diy-timed-out* t)
				  nil)
				 (mt-matingsearch-controller))
	      (mt-matingsearch-controller))
	    #-(and allegro-version>= (version>= 5 0))
	    (mt-matingsearch-controller)
;;;the following part is used to deal with the interactive calling of
;;;DIY. line-nodes of support-lines are not cleaned up during merging
;;;process. Thm130 is a very good example to illustrate this.
;;; If newnode is NIL, then there is no need to update the support line.
;;;                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;;The claim has not been justified, but it seems correct up to now.       
          #+comment(dolist (hxsupport support)
                    (setf (line-node hxsupport)
                          (update-line-node hxsupport current-topnode)))
          (dolist (hxsupport support)
                  (let ((newnode (update-line-node hxsupport current-topnode)))
                    (if newnode (setf (line-node hxsupport) newnode))))
          (when left-node 
	    (if (eproof-lemmas current-eproof)
		  (let* ((imp (cadr (etree-components (eproof-etree
						       current-eproof))))
			 (imp2 (cadr (etree-components imp))))
		    (setf (etree-components imp)
			  (list (car (etree-components imp))
				(cadr (etree-components imp2))))
		    (setf (etree-parent (cadr (etree-components imp2))) imp))
	      (setf (eproof-etree current-eproof) 
		    (cadr (etree-components (eproof-etree
					     current-eproof))))))
	  (msg t "Translating expansion proof" t)
	  (stringdt)
	  (finish-output)
	  (when *vpwin-opened* ; cebrown - 6/27/99
		(let ((*standard-output* *vpwin-opened*))
		  (msg t "Translating expansion proof ")
		  (stringdt)
		  (finish-output *standard-output*)))
          (etree-nat dproof (linealias goal) nil nil)
	  (when (proof-plans dproof)
		(apply-tactic '(call cleanup) :use 'nat-ded :mode 'auto))
          (unless (or (proof-plans dproof) lemma)
            (startcount 'printproof)
            (proof-count 'printproof)
	    (apply-tactic 'make-nice :use 'nat-ded :mode 'auto))
	  (runcount 'diy)
	  (terpri) 
	  (terpri)
	  (stringdt)
	  (terpri)
	  (unless (eq mating-verbose 'silent) ; cebrown 9/26/01
	    (tpsbell) (tpsbell) (tpsbell))
	  (when *vpwin-opened* ; cebrown - 7/14/99
		(let ((*standard-output* *vpwin-opened*))
		  (msg t "Proof in Natural Deduction Style Completed ")
		  (stringdt)
		  (finish-output *standard-output*)))))
    (throwfail "Line " (goal . existing-line) " is not a planned
line."))))

(defun mt-matingsearch-controller (&rest ignore)
   (declare (ignore ignore))
   (startcount 'mating-ctr) ;hx: April. 18, 1993
   (setq *unif-stats-store* nil)
   (setq ms88-unif-counter 0)
   (unwind-protect  
      (mt-matingsearch-controller-real)
      (breakcount 'mating-ctr)))

(defun mt-matingsearch-controller-real ()
  (cr-eproof-jform)
  (mtree-mating-2 current-eproof)
  t)

(defun mtree-mating-2 (gwff)
  (declare (special *initial-lits*))
  (progn (mate-wff-prefix gwff t nil)
	 (setq *initial-lits* (jform-to-literal-list (auto::eproof-jform current-eproof) nil))
	 (setq already-added-clists nil)
	 (setf (get matingstree-name 'core::counter) 0)
	 (setf (get obligation-name 'core::counter) 0)
	 (mtree-mating-3)))

(defun mtree-mating-3 ()
  (declare (special mtree-level current-matingstree))
  (let ((top-prompt-fn #'mtree-top-prompt)
	(mtree-level (1+ mtree-level))
	(command-interpreter #'mtree-command-interpreter)
	(top-level 'mtree-top)
	(print-* #'mtree-print-*)
	(strong-defaults 'strong-mate-defaults))
    (declare (special strong-defaults mtree-level))
    (init-matingstree nil)
    (eval (mtree-command-interpreter (list default-ms (matingstree-name current-matingstree))))
    (runcount 'mating-ctr)
    (exit-mtree-2)))

(defun  exit-mtree-prior-2 ()
  (declare (special current-matingstree))
 (when current-matingstree 
       (setq active-mating (matingstree-mating current-matingstree))
       (when (and (not (matingstree-merged current-matingstree))
                  (boundp 'current-eproof)
	          (eproof-p current-eproof)
	          (etree-p (eproof-etree current-eproof))
	          (boundp 'active-mating)
	          (mating-p active-mating)
	          (mating-completep active-mating))
            (msgf "Merging the expansion tree.  Please stand by." t)
	    (finish-output) ; cebrown - 6/27/99
	    (when *vpwin-opened* ; cebrown - 6/27/99
		  (let ((*standard-output* *vpwin-opened*))
		    (msg t "Merging expansion proof ")
		    (stringdt)
		    (finish-output *standard-output*)))
            (merge-tree)
            (setf (matingstree-merged current-matingstree) T))))

(defun  exit-mtree-2 ()
  (declare (special current-matingstree))
 (when current-matingstree
       (when (get-open-obs (matingstree-obligation current-matingstree))
	   ;then we aren't at a complete node
	   (let* ((ll (get-live-leaves-main 
		      (mst-goto 'mstroot current-matingstree)))
		  (llc (car (remove-if #'(lambda (x) (get-open-obs (matingstree-obligation x))) ll))))
	     (when llc
		   (eval (mtree-command-interpreter (list 'goto (matingstree-name llc)))))))
       (setq active-mating (matingstree-mating current-matingstree))
       (when (and (mst-quiet-complete-p)
		  (not (matingstree-merged current-matingstree))
                  (boundp 'current-eproof)
	          (eproof-p current-eproof)
	          (etree-p (eproof-etree current-eproof))
	          (boundp 'active-mating)
	          (mating-p active-mating)
	          (mating-completep active-mating))
	    (mst-choose-branch)))
 (exit-mtree-prior-2))


(context mating-search)
(defmexpr diy-l
  (argtypes pline existing-linelist yesno line-range)
  (argnames goal support window range)
  (arghelp "Planned Line" "Support Lines" "Open Vpform Window?" "Line range for new lines?")
  (defaultfns (lambda (z s w r)
		(list (pline-default z)
		      (if (and (not (eq z '$)) (eq s '$))
			  (cdr (assoc z (proof-plans dproof)))
			  s) 
		      (if (eq w '$) nil w) 
		      (if (and (eq r '$) (not (eq z '$))) (get-useful-range z) r))))
  (mainfns (lambda (z s w r) (diy-lemma z s w r)))
  (mhelp "DIY for lemmas. Behaves as for DIY, but puts all new lines into
a specified range rather than scattering them throughout the proof."))

(defun diy-lemma (z s w range &optional (timeout nil))
  (let ((pre-linelist (get dproof 'lines))
	(lr1 (car range))
	(lr2 (cdr range))
	(post-linelist nil) (introduced-lines nil) 
	(lines-before nil) (lines-after nil) (lines-in nil)
	(last-number nil))
    (if (or (> lr2 (linealias z))
	    (and (not (null s))
		 (< lr1 (reduce #'min (mapcar #'linealias s)))))
	(throwfail "The range should lie between the supports and the planned line."))
    (when (> lr1 lr2) ; cebrown 6/29/03 - shouldn't happen, but just in case
      (throwfail "The line range should be from a smaller line number to a larger line number."))
					; cebrown 6/29/03 - if there are already lines in the range, translation can fail
    (when (find-if #'(lambda (x) (and (<= lr1 (linealias x)) (>= lr2 (linealias x)))) pre-linelist)
      (throwfail "The line range should not contain any preexisting lines.  Use INTRODUCE-GAP."))
    (when timeout (setq *diy-timed-out* nil))
    (diy z s w t timeout)
    (unless (and timeout *diy-timed-out*)
      (setq post-linelist (get dproof 'lines))
      (setq last-number (get (car (last post-linelist)) 'linenumber))
      (setq introduced-lines (setdiff post-linelist pre-linelist))
    ;note: setdiff is a TPS function; it is guaranteed to keep the lines in order...
      (setq lines-before (remove-if #'(lambda (x) (> (get x 'linenumber) lr1)) introduced-lines))
      (setq lines-after (remove-if #'(lambda (x) (< (get x 'linenumber) lr2)) introduced-lines))
      (setq lines-in (remove-if-not #'(lambda (x) (< (get x 'linenumber) lr2)) 
				    (remove-if-not #'(lambda (x) (> (get x 'linenumber) lr1)) post-linelist)))
      (let ((stream-store nil)
	    (newf (new-tmp-filename)))
	(progn (setq stream-store *standard-output*)
	       (setq *standard-output* (open newf :direction :output :if-exists :append :if-does-not-exist :create)))
	(unwind-protect
	    (progn
	      (do* ((lines lines-in (cdr lines))
		    (line (car lines) (car lines))
		    (number (1+ last-number) (1+ number)))
		  ((null line))
		(move line number))
	      (if (numalias lr1) 
		  (make-room-after (numalias lr1) (length introduced-lines))
		(if (car lines-before) 
		    (progn (move (car lines-before) lr1)
			   (make-room-after (numalias lr1) (1- (length introduced-lines)))
			   (setq lines-before (cdr lines-before)))))
	      (do* ((lines (append (append lines-before lines-in) lines-after) (cdr lines))
		    (line (car lines) (car lines))
		    (number (1+ lr1) (1+ number)))
		  ((null line))
		(move line number)))
	  (progn (close *standard-output*)
		 (delete-file newf)
		 (setq *standard-output* stream-store))))
      (when cleanup-same (cleanup-same))
      (print-routines))))

(defun diy-ext-real (goal support &optional (lemma nil) (timeout nil))
  (let ((wff (line-assertion goal)))
    (stringdtl)
    (startcount 'diy)
    (unless (planp goal)
      (throwfail "Line " (goal . existing-line) " is not a planned line."))
    (dolist (l (reverse support))
      (setq wff (acons 'IMPLIES (line-assertion l) wff)))
    (case default-ms
      (MS03-7 (ext-initialize-search wff))
      (MS04-2 (ms04-initialize-search wff)))
    (runcount 'preprocess)
    (when 
	#+(and allegro-version>= (version>= 5 0))
	(if timeout
	    (mp:with-timeout (timeout
			      (msgf "Extensional Search Timed Out After " timeout " Seconds.")
			      (setq *diy-timed-out* t)
			      nil)
			     (ext-matingsearch-controller wff))
	  (ext-matingsearch-controller wff))
	#-(and allegro-version>= (version>= 5 0))
	(ext-matingsearch-controller wff)
	(let ((edag nil)
	      (supp-assoc nil)
	      (edag-lemmas nil))
	  (msgf "Proof Found!  Merging EDag Proof..." t)
	  (stringdt)
	  (finish-output)
	  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	    (let ((*standard-output* *vpwin-opened*))
	      (msgf "Proof Found!  Merging EDag Proof..." t)
	      (stringdt)
	      (finish-output *standard-output*)))
	  (startcount 'merge)
	  (setq edag (eeod-to-eed-node *current-edag* wff))
	  (msgf "Prettifying Expansion Dag..." t)
	  (stringdt)
	  (finish-output)
	  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	    (let ((*standard-output* *vpwin-opened*))
	      (msg t "Prettifying Expansion Dag..." t)
	      (stringdt)
	      (finish-output *standard-output*)))
	  (setq edag (ext-exp-dag-prettify edag))
	  (runcount 'merge)
	  (breakcount 'merge)
	  (startcount 'eproof)
	  (msgf "Translating to Natural Deduction" t)
	  (stringdt)
	  (finish-output)
	  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
		(let ((*standard-output* *vpwin-opened*))
		  (msg t "Translating to Natural Deduction" t)
		  (stringdt)
		  (finish-output *standard-output*)))
	  (when *current-edag-lemmas*
	    (setq edag-lemmas (ext-exp-arc-node (car (ext-exp-dag-arcs edag))))
	    (setq edag (ext-exp-arc-node (cadr (ext-exp-dag-arcs edag)))))
	  (dolist (l support)
	    (cond ((eq (ext-exp-dag-kind edag) 'IMP)
		   (push (cons (ext-exp-arc-node (car (ext-exp-dag-arcs edag))) l)
			 supp-assoc)
		   (setq edag (ext-exp-arc-node (cadr (ext-exp-dag-arcs edag)))))
		  ((and (eq (ext-exp-dag-kind edag) 'REW)
			(eq (ext-exp-dag-rew-just edag) 'LAMBDA)
			(implies-p (ext-exp-dag-shallow edag)))
		   (let ((awff (cdar (ext-exp-dag-shallow edag)))
			 (cwff (cdr (ext-exp-dag-shallow edag)))
			 (e0 (ext-exp-arc-node (car (ext-exp-dag-arcs edag)))))
		     (if (eq (ext-exp-dag-kind e0) 'IMP)
			 (let ((e1 (ext-exp-arc-node (car (ext-exp-dag-arcs e0))))
			       (e2 (ext-exp-arc-node (cadr (ext-exp-dag-arcs e0)))))
			   (setq e1 (make-eed-lambda awff e1))
			   (push (cons e1 l) supp-assoc)
			   (setq edag (make-eed-lambda cwff e2)))
		       (throwfail "Sorry - Translation problem with support lines."))))
		  (t
		   (throwfail "Sorry - Translation problem with support lines."))))
	  (if use-diy
	      (diy-justify goal support)
	    (ext-exp-pf-to-nd edag goal supp-assoc
			      edag-lemmas *current-edag-lemmas* *current-edag-lemma-ftree-pfs*))
	  (runcount 'eproof)
	  (breakcount 'eproof)
	  (unless (or (proof-plans dproof) lemma)
	    (startcount 'printproof)
	    (proof-count 'printproof)
	    (apply-tactic 'make-nice :use 'nat-ded :mode 'auto))
	  (runcount 'diy)
	  (terpri) 
	  (terpri)
	  (stringdt)
	  (terpri)
	  (unless (eq mating-verbose 'silent) ; cebrown 9/26/01
	    (tpsbell) (finish-output) (sleep 1/2) (tpsbell) (finish-output) (sleep 1/2) (tpsbell))
	  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	    (let ((*standard-output* *vpwin-opened*))
	      (msg t "Proof in Natural Deduction Style Completed ")
	      (stringdt)
	      (finish-output *standard-output*)))))))
