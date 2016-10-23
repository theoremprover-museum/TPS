;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :maint)
(part-of MAINTAIN)

;;;
;;; File: MAINT
;;; Package: MAINTAIN
;;;
;;; defines functions which help maintain the TPS system.
;;;

(deffile maint
  (part-of maintain)
  (extension clisp)
  (mhelp "Contains functions maintaining TPS."))

(context tps-maintenance)

(defflag read-lload-sources-p
  (flagtype boolean)
  (default t)
  (subjects maintain)
  (mhelp "If T while LLoading, one can later Ledit compiled functions."))

(defflag load-warn-p
  (flagtype boolean)
  (default t)
  (subjects maintain)
  (mhelp "If T, library files will be checked while building 
the library master index; also, warning messages will be 
printed when redefining TPS-objects while loading a file 
or fetching library objects."))

(defflag news-dir
  (flagtype dirspec)
  (default "") 
  (subjects maintain)
  (mhelp "The directory with the NEWS and NOTE files."))

(defflag source-path
  (flagtype dirspeclist) 
  (default ())
  (subjects maintain)
  (mhelp "A list of pathnames with source files for TPS3."))

(defflag source-extension
  (flagtype string)
  (default "lisp")
  (subjects maintain)
  (mhelp "The extensions (:type) of source files in TPS3."))

(defflag compiled-extension
  (flagtype string) 
  (default "fasl")
  (subjects maintain)
  (mhelp "The extension of compiled files in TPS3."))

(defflag save-file
  (flagtype filespec)
  (default "tps3.exe")
  (subjects maintain)
  (mhelp "The name of the file in which to save the core-image for TPS3."))

(defflag java-comm
  (flagtype string)
  (default "")
  (subjects maintain)
  (mhelp "How to start the Tps java interface.

An example for Unix is
cd /home/theorem/tps/java ; java TpsStart

An example for Windows is
java -classpath C:\\TPS\\java\\ TpsStart"))

(defflag init-dialogue
  (flagtype boolean)
  (default nil)
  (subjects maintain)
  (mhelp "If T, the value of INIT-DIALOGUE-FN will be called on startup
after the INI file has been read and the terminal is initialized."))

(defflag init-dialogue-fn
  (flagtype anything)
  (default init-dialogue-default-fn)
  (subjects maintain)
  (mhelp "The value of this flag is a function of no arguments,
which will be called after the INI file has been read, if
the flag INIT-DIALOGUE is T.  It may be used to set the terminal type
correctly, load some libraries, if the user wishes, or even decide
between expert and non-expert modes. The default function does nothing;
the function INIT-DEFINE-MY-DEFAULT-MODE defines a mode called
MY-DEFAULT-MODE containing the state of all the system's flags at 
the point immediately after the INI file is read."))

(definfo init-dialogue-default-fn
  (mhelp "A setting for INIT-DIALOGUE-FN.
Does nothing (except complain that you need to pick a different
setting for INIT-DIALOGUE-FN!)."))

(definfo init-define-my-default-mode
  (mhelp "A setting for INIT-DIALOGUE-FN.
Define a mode MY-DEFAULT-MODE containing all the flag settings as they
were immediately after startup (after the .ini files were read)."))

(defflag expertflag
  (flagtype boolean)
  (default nil)
  (pre-change-fn ck-expertlist)  ; cebrown, changed this from a change-fn to a pre-change-fn - 10/29/00
  (subjects maintain)
  (mhelp "If T, arbitrary Lisp expression may be evaluated on top levels."))

(defun init-dialogue-default-fn ()
  #'(lambda () (msgf "[No init dialogue function defined.]")))

(defun worth-saving (flag)
  (let ((subjects (get flag 'subjects)))
    (not (or (memq 'teacher::gr-misc subjects)
	     (memq 'teacher::gr-filenames subjects)
	     (memq 'core::system subjects)))))

;;;This function is too ad hoc. For example, how do you know
;;;(car x) is a symbol in USER package, and how do you know
;;;the exporting wouldn't cause trouble? Overall, why do
;;;you have to do the exporting? Simply for TEST-THEOREMS?

;; MB: Yes, it's just for test-theorems (which is currently the
;; only flag of type SYMBOLPAIRLIST. Maintainers beware!

(defun process-flag (flag)
  (case (get flag 'flagtype)
	((typesym typesym-nil) 
	 (princ-to-string (symbol-value flag)))
	((typesymlist typesymlist-nil) 
	 (mapcar #'princ-to-string (symbol-value flag)))
	((symbolpairlist)
	 (mapcar #'(lambda (x) (progn (export (car x) 'cl-user) (export (cdr x) 'cl-user)) (list (car x) (cdr x))) (symbol-value flag)))
	(t (symbol-value flag))))

(defun init-define-my-default-mode ()
  (let ((flaglist nil)
	(help (concatenate 'string "Default settings for " core-name " running on " core::short-site-name 
" " core::machine-type "
compiled in " core::LISP-IMPLEMENTATION-TYPE "
User: " (princ-to-string (status-userid)) ". Version from " date-tps3-saved ".
Running on " (stringdt nil))))
    (dolist (flag global-flaglist)
	    (if (and (symbolp flag) (worth-saving flag))
		(push (list flag (process-flag flag)) flaglist)))
;		(push (list flag (gettype (get flag 'flagtype) (symbol-value flag))) flaglist)))
    (eval `(defmode my-default-mode
	     (flag-settings ,flaglist)
	     (mhelp ,help)))
    (setf (get 'my-default-mode 'flag-settings) (car (get 'my-default-mode 'flag-settings)))
    t))

(defun ck-expertlist (flag-name new-value old-value)
  (if (or (null new-value)
	  (eq expert-list t)
	  (member (or *remote-userid* (status-userid)) expert-list :test #'string-equal))
      (inv-exp flag-name new-value old-value)
    (throwfail (or *remote-userid* (status-userid)) " is not allowed to alter this flag. Please
send mail to your friendly ETPS maintainer, if you think that you should be 
allowed to alter this flag!")))

(defun inv-exp (flag-name new-value old-value)
  (declare (ignore flag-name old-value))
  (setq expertflag new-value))

(defmexpr sys-load
  (argtypes modulelist)
  (argnames modulelist)
  (mhelp "Load all the modules in the given list, whether they
are loaded already or not."))

(defmexpr tload
  (argtypes filespec)
  (argnames filespec)
  (arghelp "The file of which to load the most recent version")
  (mhelp "Load the most recent compiled or uncompiled file from your default
directory, home directory, or source-path. In general, the following rules are
used to determine whether compiled or uncompiled file should be load in:
(1) If both compiled and uncompiled file exist, and 
    (1.1) the compiled one is newer, it is loaded in.
    (1.2) the uncompiled one is newer, then
          (1.2.1) if the global variable core::*allow-compile-source* is T, the
                  name of the file contains extension "lisp", and the global
                  variable core::*always-compile-source-if-newer* is T or user
                  answers 'Y', compile the uncompiled one and load it in.
          (1.2.2) load the uncompiled one.
(2) If only compiled one exists, then load it in.
(3) if only uncompiled one exists, do the same as case (1.2)
."))

(defmexpr qload
  (argtypes filespec)
  (argnames filespec)
  (arghelp "The file of which to load the most recent version")
  (mhelp "Load the most recent compiled or uncompiled file from your default
directory, home directory, or source path. In general, the following rules
are used to determine whether compiled or uncompiled file should be load in:
(1) If the file name with extension '.lisp', always load the uncompiled source code.
(2) If the file name without extension, then
    (2.1) if both compiled and uncompiled file exist, and 
          (2.1.1) the compiled one is newer, it is loaded in.
          (2.1.2) the uncompiled one is newer, 
                  (2.1.2.1) if the flag 'expertflag' is NIL, always load the uncompiled 
                            source code.
                  (2.1.2.2) if the flag 'expertflag' is T, ask user whether load the uncompiled
                            one, or compile it and load the compiled one then.
    (2.2) if only the compiled one exists, load it in.
    (2.3) if only the uncompiled one exists, do the same as case (2.1.2)
"))

;;;
;;; The commands in the next section allow to correct the command-completion
;;; behavior and the ENVIRONMENT help tree after some TPS-object has been
;;; added, redefined, or new categories have been defined.  (Some of these
;;; are called after the PACKAGES commmand automatically.
;;;

(defmexpr organize
  (mainfns organize-all) ; was reorganize, but this doesn't seem to work any more MB 6/94
  (mhelp "Organizes the ENVIRONMENT help tree (e.g. after loading modules)."))

(defmexpr tlist
  (argtypes symbol)
  (argnames symbol)
  (arghelp "The symbol whose property list to display.")
  (mhelp "Use a help function to display all of the property list of a symbol."))

(defmexpr ledit
  (dont-restore t)
  (mhelp "Call the resident Lisp editor (if there is one) inside TPS. 
It takes a filename as an optional argument. In most lisps, this will probably
start up Emacs. In CMU lisp, this will start up Hemlock; use ^X^Z to leave Hemlock 
again. In some lisps, this command may not work at all."))

(defun ledit (&optional (filename nil))
  (if filename
      (let ((located-file (locate-tps-file filename t)))
	(if located-file (ed located-file)
	    (progn (complain "File not found.") (ed))))
      (ed)))

(defun tps-filetype (filename)
  (let ((located-file (locate-tps-file filename t)))
    (if located-file (filetype located-file)
	(throwfail "File not found."))))

(defmexpr filetype
  (argtypes filespec)
  (argnames filename)
  (arghelp "File to type")
  (mainfns tps-filetype)
  (print-command t)
  (mhelp "Type a file on the screen.  TPS will look for the file in a list
of directories."))

(defmexpr tps3-save
  (mhelp "Save the current TPS3 as the new TPS3 core image."))

;;;See the file tops20.lisp for defun tps3-save for various versions
;;;of common lisp

(defun tlist (symbol)
  (do ((pl (symbol-plist symbol) (cddr pl)))
      ((null pl) t)
    (feat-help1 (car pl) (cadr pl) 15 78 #'prin1)))

(defun load-slow (filename)
  (let ((current-package *package*))
    (unwind-protect
        (with-open-file (input-stream (locate-tps-file filename t) 
                                      :direction :input)
          (let ((*terminal-line-mode* nil))
            (declare (special *terminal-line-mode*))
            (do ((form (read input-stream nil :$eof$)
                       (read input-stream nil :$eof$)))
                ((eq form :$eof$)
                 (format t "End of file reached."))
              (format t "~&")
              (write form :pretty t :level 2 :length 2)
              again
              (case (read-char)
                (#\e (format t "~S" (eval form)))
                (#\q (return nil))
                (#\p (write form :pretty t :level nil :length nil)
                 (go again))
                (#\s (eval `(step ,form)))
                (#\n nil)
                (#\Space (eval form))
                (#\Newline (go again))
                (#\? (format t "~@{~&~A - ~A~}"
                             "e" "Evaluate form and show result"
                             "q" "Exit load-slow"
                             "p" "Print form completely"
                             "s" "Step through evaluation of form"
                             " " "Evaluate form and move on"
                             "?" "Show this info"
                             "n" "Don't evaluate this")
                 (go again))))))
      (setq *package* current-package))))

(defmexpr load-slow
  (argtypes filespec)
  (argnames filename)
  (arghelp "File to load slowly")
  (mhelp "Step through loading a file."))

(defflag test-theorems
  (flagtype symbolpairlist)
  (default ((cl-user::x2106 cl-user::ml) (cl-user::x2108 cl-user::ml)))
  (subjects maintain)
  (mhelp "A list of pairs; the first of each pair is the name of a theorem; 
the second is the name of a mode. If the mode name is NIL, TPS will 
attempt to choose a mode from the list of best modes in the library.
This flag is used by the command TPS-TEST, and can be set automatically by
the command TEST-INIT.

The default setting is a sample list of two standard TPS exercises, both 
to be run in mode ML (also standard in TPS). If you set this flag yourself,
beware of unexported symbols --- which is to say, make sure that the 
symbols you use are all in the USER package (this is particularly 
necessary if you are using library theorems which are not yet loaded
into TPS, or they may end up interned in the wrong package). If in doubt,
put \"USER::\" before all symbols, thus:

(setq test-theorems '((cl-user::thm30 . cl-user::mode-thm30) (cl-user::x2112 . cl-user::ml)))

You can use the flag TEST-MODIFY to alter modes on the fly as TPS-TEST runs.
See the help messages for TEST-INIT and TEST-MODIFY for more information."))


(defflag test-modify
  (flagtype string)
  (default "")
  (subjects maintain)
  (mhelp "A string which will be evaluated in exactly the same way as an alias.
May contain any valid lisp commands, and will be evaluated after setting the 
mode during tps-test. So, for example, setting it to 
\"(set-flag 'skolem-default nil) 
(when search-time-limit (setq search-time-limit (* 2 search-time-limit)))
(when max-search-limit (setq max-search-limit (* 2 max-search-limit)))\"
would make tps-test changed SKOLEM-DEFAULT to NIL and double the time limits
before each search."))

(defmode quiet
  (flag-settings 
   (core::printlineflag nil) ; cebrown 6/18/2002
   (auto::unify-verbose auto::silent)
   (auto::mating-verbose auto::silent)
   (auto::etree-nat-verbose nil)
   (auto::ms98-verbose nil)
   (auto::tactic-verbose min)
   (auto::options-verbose nil)
   (load-warn-p nil))
  (mhelp "Turn off all output that can be turned off, without affecting search
at all. Should make most other modes run a bit faster."))

(defmexpr test-init
  (mhelp "Initialize the flag TEST-THEOREMS to test a collection
of theorems on a collection of modes.  This command should be
followed by TPS-TEST which actually tries to prove the theorems
with the modes.

There are currently several possibilities:

1.  Set TEST-THEOREMS to test a given set of theorems on
a given set of modes.  The default set of modes is determined
by the value of the flag GOODMODES.

2.  Set TEST-THEOREMS to test the set of modes given by the flag
GOODMODES on theorems that have a bestmode in the library
(determined by DEFAULT-LIB-DIR and BACKUP-LIB-DIR) but are not 
known to be provable by some mode in the GOODMODES list.

3.  Set TEST-THEOREMS to test a set of modes given by the flag
GOODMODES on all the theorems the modes are supposed to prove.
(This tests whether a list of GOODMODES is still complete 
with respect to the corresponding list of theorems.)

4. Set TEST-THEOREMS to test all of the best
modes known to the library on all the theorems listed with
the best modes. By default, this will choose the first
mode listed for each theorem in the bestmodes.rec file; if you
choose to use multiple modes then it will test each theorem
with all of the modes listed for it in that file.  The examples
are listed in order from quickest to longest.
(This checks that all the theorems associated with bestmodes 
can still be proven by these bestmodes.)"))

(eval-when (compile load eval)
(def-modes-gwffs EMPTYGOODMODES
  (mhelp "A pair of no modes and no gwffs.  Default value of the flag GOODMODES."))
)

(defflag GOODMODES
  (flagtype MODES-GWFFS)
  (default EMPTYGOODMODES)
  (subjects maintain)
  (mhelp "A name for a pair MODES and GWFFS where MODES is a list of modes
and GWFFS is a list of theorems.  Every theorem in GWFFS should be 
provable using some mode in MODES.  To check this, or to use these modes
to try to prove a new theorem, one can use TEST-INIT and TPS-TEST.

SEE ALSO: MODES-GWFFS, TEST-INIT, TPS-TEST, ADD-GOODMODES, REMOVE-GOODMODES"))

(defun test-init ()
  (declare (special core::*lib-masterindex* lib-bestmodes))
  (let ((usenum 1))
    (msgf "The flag GOODMODES is set to a MODES-GWFFS giving a list of modes MODES and a list of gwffs GWFFS." t)
    (msgf "Set the flag TEST-THEOREMS to test the specified Theorems with the specified Modes" t)
    (msgf "using one of the following options (type ?? for more explanation)." t)
    (msgf "1.  Theorems := User Specified; Modes := User Specified (with default MODES)." t)
    (msgf "2.  Theorems := Those with bestmodes which are not in GWFFS; Modes := MODES." t)
    (msgf "3.  Theorems := GWFFS; Modes := MODES." t)
    (msgf "4.  Theorems := Theorems with bestmodes; Modes := Bestmodes for each theorem." t)
    (prompt-read usenum nil (msg "Enter 1, 2, 3, or 4.") 'posinteger usenum 
		 ((? (msg "Enter 1, 2, 3, or 4 depending on how you want TEST-INIT to set TEST-THEOREMS."))
		  (?? (mhelp 'test-init))))
    (cond ((equal usenum 2)
	   (let ((modes (get goodmodes 'modes-gwffs-modes))
		 (knownthms (get goodmodes 'modes-gwffs-gwffs))
		 (thms nil))
	     (maphash #'(lambda (key value)
			  (declare (ignore value))
			  (unless (member key knownthms)
			    (push key thms)))
		      lib-bestmodes)
	     (setq test-theorems nil)
	     (dolist (thm thms)
	       (dolist (mode modes)
		 (push (cons thm mode) test-theorems)))
	     (when test-theorems
	       (msgf "TEST-THEOREMS has been set.  Now, run TPS-TEST."))))
	  ((equal usenum 3)
	   (let ((modes (get goodmodes 'modes-gwffs-modes))
		 (thms (get goodmodes 'modes-gwffs-gwffs)))
	     (setq test-theorems nil)
	     (dolist (thm thms)
	       (dolist (mode modes)
		 (push (cons thm mode) test-theorems)))
	     (when test-theorems
	       (msgf "TEST-THEOREMS has been set.  Now, run TPS-TEST."))))
	  ((equal usenum 1)
	   (let ((thms nil)
		 (modes (get goodmodes 'modes-gwffs-modes)))
	     (prompt-read thms nil (msg "Enter a list of theorems.") 'symbollist thms
			  ((? (msg "A list of theorems to test."))
			   (?? 'symbollist)))
	     (prompt-read modes nil (msg "Enter a list of modes.") 'symbollist modes
			  ((? (msg "A list of modes to use to test the theorems."))
			   (?? 'symbollist)))
	     (setq test-theorems nil)
	     (dolist (thm thms)
	       (dolist (mode modes)
		 (push (cons thm mode) test-theorems)))
	     (setq test-theorems (reverse test-theorems))
	     (when test-theorems
	       (msgf "TEST-THEOREMS has been set.  Now, run TPS-TEST."))))
	  ((equal usenum 4)
	   (let (result mult)
	     (setq mult (query "Use multiple modes for same theorem?" nil))
	     (maphash #'(lambda (key val)
			  (when (and key (gethash key core::*lib-masterindex*)) ; to make sure its in the library - cebrown 3/11/2001
			    (let ((val2 (remove-if #'(lambda (x) (and (consp (cadddr x))
								      (cadr (cadddr x)))) ; dont auto test
						   val)))
			      (when val2
				(if mult
				    (dolist (v val2) (push (cons (caddr v) (cons key (car v))) result))
				  (push (cons (caddar val2) (cons key (caar val2))) result))))))
		      lib-bestmodes)
	     (setq test-theorems
		   (remove-duplicates
		    (mapcar #'cdr
			    (sort result
				  #'(lambda (x y) 
				      (or (< (car x) (car y)) ; quickest first
					  (and (= (car x) (car y)) (string< (cadr x) (cadr y))) ; alphabetical order for same timings
					  (and (= (car x) (car y)) (string= (cadr x) (cadr y)) (string< (cddr x) (cddr y)))))))
		    :test #'equalp))
	     (when test-theorems
	       (msgf "TEST-THEOREMS has been set.  Now, run TPS-TEST.")))))))

(defmexpr tps-test
  (argtypes yesno yesno yesno yesno yesno yesno yesno yesno filespec yesno yesno)
  (argnames stop-on-success mate-only record moderec quiet-run expu newcore modify output timing testwin)
  (arghelp "Stop after a Theorem is Proven?" "Only do mating search?" "Do a DATEREC after each theorem is proven?" "Do a MODEREC after each theorem is proven?" "Run quietly?" "EXPUNGE and assert defaults between each proof?" "Start a new core image for each proof attempt?" "Modify modes?" "Short output file" "Send timing info to the short file?" "Send output to the TEST window?")
  (defaultfns (lambda (ss m x x1 z e nc mm y w v)
		(list (if (eq ss '$) t ss)
		      (if (eq m '$) nil m) (if (eq x '$) nil x) (if (eq x1 '$) nil x1) (if (eq z '$) 'yes z) (if (eq e '$) 'yes e) (if (eq nc '$) 'yes nc) (if (eq mm '$) nil mm)
		      (if (eq y '$) ; better default names - cebrown 5/22/03
			  (let ((thms (remove-duplicates (mapcar #'car test-theorems))))
			    (if (= (length thms) 1)
				(format nil "tps-test-~d.doc" (car thms))
			      (let ((modes (remove-duplicates (mapcar #'cdr test-theorems))))
				(if (= (length modes) 1)
				    (format nil "tps-test-~d.doc" (car modes))
				  "tps-test-output.doc"))))
			y)
		      (if (eq w '$) 'yes w) (if (eq v '$) 'yes v))))
  (mhelp "Attempt to prove a list of theorems.

The list of theorems, with the modes to be used, is stored as (theorem . mode) 
pairs in the flag TEST-THEOREMS. These theorems and modes will be fetched from
the library, if they cannot be found in TPS and if you have a library. You 
should set DEFAULT-LIB-DIR and BACKUP-LIB-DIR appropriately. You can only do
DATEREC after each theorem if you have a library you can write to. 

The first argument STOP-ON-SUCCESS decides whether TPS-TEST should stop
trying to prove a particular theorem with different modes after one
mode has succeeded.  If this is T, then after TPS-TEST proves THM with MODE1,
where (THM . MODE1) is on TEST-INIT, TPS-TEST will not try to prove (THM . MODE2)
for any (THM . MODE2) on TEST-INIT.  It will however, continue to try to prove
other theorems on TEST-INIT with different modes (if there are any).

Quiet running uses the mode QUIET to switch off as much screen output as possible.

You can EXPUNGE between proofs (this will reduce the amount of memory
required, but will mean that other expansion proofs in the memory may
be lost; it will also re-assert your default flag values between each
proof).  Expunging does not really recover all the space used by TPS,
so many repeated proof attempts will result in running out of memory.
To remedy this situation, TPS-TEST can start a new core image for each
proof attempt.  In this case, each core image will start with a fresh
memory.  (When this option is chosen, expunging is irrelevant.)
Certain operating systems and versions of Lisp may not support
this option.

If TPS-TEST is running a new core image for each proof attempt, the
user can interrupt the slave core image using Control-C.  This should
throw one to the debugger level of the slave image.  In Allegro Lisp,
:res will cause the slave to die and throw the user to the debugger
level of the master core image.  Another :res will return the user to
the TPS top level of the master core image.

If the argument MODIFY is T, then the flag TEST-MODIFY can be 
used to change flag settings after loading each mode but before
searching.  See the help message for TEST-MODIFY for more information.

In versions of Common Lisp with multiprocessing (e.g., Allegro 5.0 or
later), the user can specify a time limit for each proof attempt.  The
user can also ask TPS-TEST to iterate trying every (THM . MODE) on
TEST-THEOREMS, increasing the time limit by a factor on each
iteration.  A (THM . MODE) is only tried again with a longer time if
it timed out on the previous attempt.  When multiprocessing is not
available (or if the user specifies an INFINITE time limit), TPS will
search for a proof using a given mode as long as permitted by that
mode.

If TPS-TEST encounters a bug, it will go on to the next (THM . MODE) pair.

The output file is kept independently of DATEREC records, and consists of a record
for each (THM . MODE) pair stating that the theorem was proved at a certain
time using a certain mode, or that the proof terminated with proof lines still 
remaining or that tps encountered an error.  Timing information can also
be sent to the short file if necessary.

If the short file already exists, the old copy will be renamed by adding 
.bak to its name.

See the help messages for TEST-THEOREMS, TEST-INIT and TEST-MODIFY for more information."))

; list of options for extract-test-info.
; each option is a list (<string> <fn1> <fn2> <fn3>) where:
; <string> describes the option.
; <fn1> takes an entry (from the tps-test output file)
;       and returns "t" if this option should use the entry, and nil otherwise.
; <fn2> takes an entry and a list of entries collected and combines them
; <fn3> prints the appropriate information about the entry
(defvar *extract-test-info-options*
  '(("All Theorems Proven"
	     (lambda (x)
	       (find-if #'(lambda (y)
			    (and (consp y) (string-equal (car y) "RESULT")
				 (consp (cdr y))
				 (member (cadr y)
					 '("SUCCESS" "MATE-SUCCESS")
					 :test #'string-equal)))
			x))
	     (lambda (x collect) (cons x collect))
	     (lambda (x)
	       (let ((thm (cadr (assoc "THM" x :test #'string-equal))))
		 (msgf thm))))
	    ("Theorems Proven With Times"
	     (lambda (x)
	       (find-if #'(lambda (y)
			    (and (consp y) (string-equal (car y) "RESULT")
				 (consp (cdr y))
				 (member (cadr y)
					 '("SUCCESS" "MATE-SUCCESS")
					 :test #'string-equal)))
			x))
	     (lambda (x collect) (cons x collect))
	     (lambda (x)
	       (let ((thm (cadr (assoc "THM" x :test #'string-equal)))
		     (tim (cadr (assoc "TIMING" x :test #'string-equal))))
		 (msgf thm tim))))
	    ("Theorems Proven With Successful Modes"
	     (lambda (x)
	       (find-if #'(lambda (y)
			    (and (consp y) (string-equal (car y) "RESULT")
				 (consp (cdr y))
				 (member (cadr y)
					 '("SUCCESS" "MATE-SUCCESS")
					 :test #'string-equal)))
			x))
	     (lambda (x collect) (cons x collect))
	     (lambda (x)
	       (let ((thm (cadr (assoc "THM" x :test #'string-equal)))
		     (mode (cadr (assoc "MODE" x :test #'string-equal))))
		 (msgf thm " : " mode))))
	    ("Theorems Proven With Times and Successful Modes"
	     (lambda (x)
	       (find-if #'(lambda (y)
			    (and (consp y) (string-equal (car y) "RESULT")
				 (consp (cdr y))
				 (member (cadr y)
					 '("SUCCESS" "MATE-SUCCESS")
					 :test #'string-equal)))
			x))
	     (lambda (x collect) (cons x collect))
	     (lambda (x)
	       (let ((thm (cadr (assoc "THM" x :test #'string-equal)))
		     (mode (cadr (assoc "MODE" x :test #'string-equal)))
		     (tim (cadr (assoc "TIMING" x :test #'string-equal))))
		 (msgf thm " : " mode tim))))
	    ("Theorems and Modes That Timed Out"
	     (lambda (x)
	       (find-if #'(lambda (y)
			    (and (consp y) (string-equal (car y) "RESULT")
				 (consp (cdr y))
				 (member (cadr y)
					 '("TIMEDOUT" "MATE-TIMEDOUT")
					 :test #'string-equal)))
			x))
	     (lambda (x collect)
	       (let ((thm (cadr (assoc "THM" x :test #'string-equal)))
		     (mode (cadr (assoc "MODE" x :test #'string-equal)))
		     (realtime (caddr (assoc "RESULT" x :test #'string-equal))))
		 (when (numberp realtime)
		   (let ((alt (find-if #'(lambda (y)
					   (and (equal thm (cadr (assoc "THM" y :test #'string-equal)))
						(equal mode (cadr (assoc "MODE" y :test #'string-equal)))))
				       collect)))
		     (if alt
			 (let ((realtime2 (caddr (assoc "RESULT" alt :test #'string-equal))))
			   (if (and (numberp realtime2) (< realtime realtime2))
			       collect
			     (cons x (remove alt collect))))
		       (cons x collect))))))
	     (lambda (x)
	       (let ((thm (cadr (assoc "THM" x :test #'string-equal)))
		     (mode (cadr (assoc "MODE" x :test #'string-equal)))
		     (realtime (caddr (assoc "RESULT" x :test #'string-equal))))
		 (msgf thm " : " mode " timed out in " realtime " seconds"))))
	    ("Theorems and Modes That Failed"
	     (lambda (x)
	       (find-if #'(lambda (y)
			    (and (consp y) (string-equal (car y) "RESULT")
				 (consp (cdr y))
				 (member (cadr y)
					 '("FAILED" "MATE-FAILED")
					 :test #'string-equal)))
			x))
	     (lambda (x collect) (cons x collect))
	     (lambda (x)
	       (let ((thm (cadr (assoc "THM" x :test #'string-equal)))
		     (mode (cadr (assoc "MODE" x :test #'string-equal))))
		 (msgf thm " : " mode))))))

(defmexpr extract-test-info
  (argtypes filespec)
  (argnames file)
  (arghelp "File created by tps-test")
  (defaultfns (lambda (file)
		(list (if (and (eq file '$) (probe-file "tps-test-output.doc"))
			  "tps-test-output.doc"
			file))))
  (mhelp
"Extract and report information from a file generated by a run of tps-test.
The user has several options for what information to extract.

See Also: TPS-TEST"))

(setf (get 'extract-test-info 'mhelp)
      (let ((opstring "")
	    (i 0))
	(dolist (op *extract-test-info-options*)
	  (setq opstring (format nil "~d~%~d - ~d"
				 opstring
				 (incf i)
				 (car op))))
	(mapcar #'(lambda (x)
		    (if (and (consp x) (eq (car x) 'MEXPR))
			(cons (car x)
			      (format nil "~d~2%The options include:~%~d~2%"
				      (cdr x) opstring))
		      x))
		(get 'extract-test-info 'mhelp))))

; want help message to list and describe each option (could it be generated like list below?)
(defun extract-test-info (file)
  (declare (special *extract-test-info-options*))
  (unless (probe-file file)
    (throwfail "Cannot find file " file))
  (let* ((fin (open file :direction :input))
	 (r (read fin))
	 (numoptions (length *extract-test-info-options*))
	 tps-test-info options outfile)
    (unless (and (listp r) (string-equal (car r) "HEADER"))
      (close fin)
      (throwfail "File " file " does not appear to be a file generated by tps-test."))
    (do ((r (read fin nil 'eof) (read fin nil 'eof)))
	((eq r 'eof) (close fin))
      (when (and (listp r) (consp (car r)))
	(push r tps-test-info)))
    (if tps-test-info
	(progn
	  (msgf "Information to Extract and Report:" t)
	  (dotimes (i numoptions)
	    (msgf (1+ i) " - " (car (nth i *extract-test-info-options*)) t))
	  (prompt-read options nil
		       (msgf "List of Numbers for Options")
		       'posintegerlist
		       '$
		       ((? (mhelp 'posintegerlist))
			(?? (msg "A list of numbers indicating which information to extract and report."))))
	  (setq options (remove-if-not
			 #'(lambda (x) (and (integerp x) (> x 0) (<= x numoptions)))
			 options))
	  (when options
	    (when (query "Send Output to a File?" nil)
	      (prompt-read outfile nil
			   (msgf "Filename")
			   'filespec
			   "tps-test-report"
			   ((? (mhelp 'filespec))
			    (?? (msg "Name of file for report.")))))
	    (when outfile
	      (reroute-output-append outfile *default-pathname-defaults*
				     (msgf t "--------------------" t "Report from " file t)
				     (stringdt)))
	    (dolist (opn options)
	      (let* ((op (nth (- opn 1) *extract-test-info-options*))
		     (fn1 (cadr op))
		     (fn2 (caddr op))
		     (fn3 (cadddr op))
		     (collect nil))
		(dolist (x tps-test-info)
		  (when (funcall fn1 x)
		    (setq collect (funcall fn2 x collect))))
		(msgf t t "-----------------" t (car op) t t)
		(dolist (x collect)
		  (funcall fn3 x))
		(when outfile
		  (reroute-output-append outfile *default-pathname-defaults*
					 (msgf t t "-----------------" t (car op) t)
					 (dolist (x collect)
					   (funcall fn3 x))))))))
      (msgf "File " file " contains no tps-test information"))))

(defun tps-test (stop-on-success mate-only record moderec quiet-run expu newcore modif file timing testwin)
  (when (and modif moderec)
    (msgf "You have chosen to modify the modes during the search." t
	  "MODEREC will record the mode *without* these modifications." t)
    (unless (query "Are you sure you want to call MODEREC?" nil)
      (setq moderec nil)))
  (let ((prov (if record (query "Stop and ask about changing provability status after each proof?" nil) nil))
	(timeout infinity)
	(iterations 1)
	(it-factor 2))
    #+(and allegro-version>= (version>= 5 0))
    (progn
      (prompt-read timeout nil (msgf "Initial Timeout in Seconds (or INFINITY) for Each Proof Attempt") ; timeout uses Allegro's multiprocessing - cebrown
		   'integer+-or-infinity 2 ((? (mhelp 'integer+-or-infinity)) (?? (msgf "The number of seconds to search for a proof of a theorem using a mode"))))
      (unless (member timeout (list 0 infinity) :test #'equal)
	(prompt-read iterations nil (msgf "Number of Iteration Attempts")
		     'integer+-or-infinity 1 ((? (mhelp 'posinteger)) (?? (msgf "The number of times to iterate through the pairs of theorems and modes increasing the timeout on each iteration"))))
	(unless (member iterations '(0 1) :test #'equal)
	  (prompt-read it-factor nil (msgf "Factor by which to Increase Timeout on each Iteration")
		       'posnumber 2 ((? (mhelp 'posnumber)) (?? (msgf "The number to multiply the timeout by on each iteration")))))))
    (if newcore
	(tps-test-newcore stop-on-success mate-only record moderec quiet-run expu modif file timing testwin prov timeout iterations it-factor)
      (if mate-only
	  (tps-test-real2 stop-on-success record moderec quiet-run expu modif file timing testwin prov timeout iterations it-factor)
	(tps-test-real stop-on-success record moderec quiet-run expu modif file timing testwin prov timeout iterations it-factor)))))

(defvar *dont-ask* nil)

(defun tps-test-newcore (stop-on-success mate-only record moderec quiet-run expu modif file timing testwin prov timeout iterations it-factor)
  (declare (special auto::filename) (ignore prov))
  (let ((batchcom nil)
	(filetmp (format nil "~d-tmp" file))
	(workfileroot (format nil "~d-tmpw" file))
	(workfile (format nil "~d-tmpw.work" file)))
    (prompt-read batchcom nil (msgf "Command to start TPS in batch mode")
		 'string
		 #+(and allegro-version>= (version>= 5 0) (not mswindows))
		 (format nil "~d -I ~d~d.dxl -- -batch " (car (sys:command-line-arguments)) sys-dir core-name)
		 #-(and allegro-version>= (version>= 5 0) (not mswindows))
		 "tps -batch"
		 ((? (mhelp 'string)) (?? (msgf "The command for starting TPS in batch mode to execute a work file"))))
    (when (probe-file file) (rename-file file (concatenate 'string (namestring file) ".bak")))
    (let ((success-thms nil)
	  (timed-out-list nil)
	  (testwin-filename nil)
	  (i 0))
      (when testwin
	(auto::open-testwin-auto)
	(when (boundp 'auto::filename)
	  (setq testwin-filename auto::filename)))
      (reroute-output-append file *default-pathname-defaults* 
			     (msgf t t "; Starting TPS-TEST for " LISP-IMPLEMENTATION-TYPE t
				   ";  running on " MACHINE-TYPE " at " SHORT-SITE-NAME t t)
			     (let ((*print-pretty* t))
			       (declare (special *print-pretty*)) 
			       (format t "~S~%"
				       (list "HEADER"
					     (list 'LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-TYPE)
					     (list 'MACHINE-TYPE MACHINE-TYPE)
					     (list 'SHORT-SITE-NAME SHORT-SITE-NAME)
					     (list 'TEST-THEOREMS TEST-THEOREMS)))))
      (loop while (or (eq iterations infinity) (< i iterations)) do
	    (incf i)
	    (dolist (theorem test-theorems)
	      (let ((thm (car theorem))
		    (mode (cdr theorem)))
		(unless (or (member thm success-thms)
			    (and (> i 1) (not (member theorem timed-out-list :test #'equal))))
					; make work file
		  (reroute-output
		   workfile *default-pathname-defaults*
		   (msgf "TEST-THEOREMS " (list (list thm mode)))
		   (msgf "tps-test" t stop-on-success t mate-only t record t moderec t quiet-run t expu t "NIL" t 
			 modif t "\"" filetmp "\"" t timing t testwin t)
		   #+(and allegro-version>= (version>= 5 0))
		   (progn
		     (msgf timeout t)
		     (unless (member timeout (list 0 infinity) :test #'equal)
		       (msgf "1" t)))
		   )
					; open new tps core to execute the work file
		  (if testwin
		      (call-system (format nil "~d ~d -testwinfile ~d" batchcom workfileroot testwin-filename))
		    (call-system (format nil "~d ~d" batchcom workfileroot)))
					; move output from tmp file into file
		  (delete-file workfile)
		  (when (probe-file filetmp)
		    (reroute-output-append
		     file *default-pathname-defaults*
		     (let ((tf (open filetmp :direction :input)))
		       (do ((r (read tf nil nil) (read tf nil nil)))
			   ((null r)
			    (close tf)
			    (msgf t t)
			    (delete-file filetmp))
			 (when (and (listp r) (consp (car r)))
			   (let ((thm (cadr (assoc "THM" r :test #'string-equal))))
			     (when (string= (format nil "~d" thm) (format nil "~d" (car theorem)))
			       (let ((res (cadr (assoc "RESULT" r :test #'string-equal))))
				 (cond ((member res '("MATE-TIMEDOUT" "TIMEDOUT") :test #'string-equal)
					(push theorem timed-out-list))
				       ((member res '("MATE-SUCCESS" "SUCCESS") :test #'string-equal)
					(setq timed-out-list (remove theorem timed-out-list :test #'equal))
					(when stop-on-success
					  (push thm success-thms)))
				       (t
					(setq timed-out-list (remove theorem timed-out-list :test #'equal))))))))
			 (let ((*print-pretty* t))
			   (declare (special *print-pretty*)) 
			   (format t "~S~2%" r)))))))))
	    (when (numberp timeout)
	      (setq timeout (* it-factor timeout)))))))
    
(defun tps-test-real (stop-on-success record moderec quiet-run expu modif file timing testwin prov timeout iterations it-factor)
  (declare (special core::*lib-masterindex* lib-bestmodes default-lib-dir))
  (let ((timed-out-list nil)
	(i 0))
    (when quiet-run (mode 'quiet))
    (when expu (unless (and (fboundp 'my-default-mode) (tps-mode-p 'my-default-mode))
		 (init-define-my-default-mode)))
    (when (and testwin (not *executing-batch*)) (auto::open-testwin-auto))
    (dolist (theorem test-theorems)
      (let ((thm (car theorem))
	    (mode (cdr theorem)))
	(unless (theorem-p thm)
	  (when (module-loaded-p 'library) 
	    (core::retrieve-libobject thm :type 'gwff :multiple nil))
	  (unless (theorem-p thm)
	    (throwfail t thm " is not a known theorem. Check the setting of TEST-THEOREMS." t)))
	(unless mode
	  (setq mode (caar (gethash thm lib-bestmodes))))
	(unless (member mode global-modelist)
	  (when (module-loaded-p 'library) 
	    (let ((y (car (remove-if-not #'(lambda (x) (memq (car x) '(mode mode1)))
					 (gethash mode core::*lib-masterindex*)))))
	      (if y
		  (core::retrieve-libobject mode :type (car y) :multiple nil)
		(throwfail "Can't find mode " mode t))))
	  (unless (member mode global-modelist)
	    (throwfail t mode " is not a known mode. Check the setting of TEST-THEOREMS." t)))))
					;now we know that TEST-THEOREMS is OK, and we have all the necessary stuff loaded.
    (let ((finished-flag nil)
	  (success-thms nil) ; cebrown 12/29/02 - for stop-on-success
	  (timedout nil)) ; cebrown 12/29/02 - for timeout
      (when (probe-file file) (rename-file file (concatenate 'string (namestring file) ".bak")))
      (unwind-protect
	  (progn
	    (unless *executing-batch*
	      (reroute-output-append file *default-pathname-defaults* 
				     (msgf t t "; Starting TPS-TEST for " LISP-IMPLEMENTATION-TYPE t
					   "; running on " MACHINE-TYPE " at " SHORT-SITE-NAME t t)
				     (let ((*print-pretty* t))
				       (declare (special *print-pretty*))
				       (format t "~S~%"
					       (list "HEADER"
						     (list 'LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-TYPE)
						     (list 'MACHINE-TYPE MACHINE-TYPE)
						     (list 'SHORT-SITE-NAME SHORT-SITE-NAME)
						     (list 'TEST-THEOREMS TEST-THEOREMS)))))
	      (when testwin (auto::testwin-update-three "
Starting TPS-TEST for " LISP-IMPLEMENTATION-TYPE 
"
running on " MACHINE-TYPE " at " SHORT-SITE-NAME
"
TEST-THEOREMS is set to " TEST-THEOREMS "

")))
	    (loop while (or (eq iterations infinity) (< i iterations)) do
	      (incf i)
	      (dolist (theorem test-theorems)
		(handler-case
		 (let ((thm (car theorem))
		       (mod (cdr theorem)))
		   (setq timedout nil)
		   (unless (or (member thm success-thms)
			       (and (> i 1) (not (member theorem timed-out-list :test #'equal))))
		     (when expu 
		       (auto::expunge) 
		       (mode 'my-default-mode '(test-theorems)) (gc))
		     (unless mod
		       (setq mod (caar (gethash thm lib-bestmodes))))
		     (mode mod '(test-theorems))
		     (when testwin (auto::testwin-update-three "Proving " (princ-to-string thm) " in mode " mod 
							       (if quiet-run " + QUIET" " ") t
							       (if modif (concatenate 'string 
										      "   after applying " test-modify) " ")
							       t))
		     (reroute-output-append file *default-pathname-defaults* 
					    (format t "~2%~S~%"
						    (format nil
							    "Proving ~d in mode ~d~d~d"
							    (princ-to-string thm)
							    mod
							    (if quiet-run " + QUIET" "")
							    (if modif (concatenate 'string 
										   " after applying " test-modify)
							      " "))))
		     (if quiet-run (mode 'quiet '(test-theorems)))
		     (when modif (dolist (e (core::lisp-ize (core::tokenize-line test-modify)))
				   (eval e)))
		     (setq *dont-ask* t)
		     (if (library-theorem-p thm)
			 (eval (interpret-tps-top (list 'prove thm thm '!)))
		       (eval (interpret-tps-top (list 'exercise thm))))
		     (setq *dont-ask* nil)
		     (if (equal timeout infinity)
			 (eval (interpret-tps-top (list 'auto::diy '!)))
		       #+(and allegro-version>= (version>= 5 0))
		       (mp:with-timeout (timeout (setq timedout t))
					(eval (interpret-tps-top (list 'auto::diy '!))))
		       #-(and allegro-version>= (version>= 5 0))
		       (eval (interpret-tps-top (list 'auto::diy '!)))
		       )
		     (when (and (module-loaded-p 'library) record)
		       (if (core::locate-item thm :type 'gwff :writeable t :multiple nil)
			   (core::daterec dproof 'gwff "" t prov)
			 (progn
			   (let ((item (core::make-libitem
					:name thm :type 'gwff :description (get thm 'assertion)
					:mhelp "" :context nil
					:provability nil
					:proof-date nil
					:other-attributes nil
					:needed-objects nil
					:other-remarks ""
					:file (concatenate 'string (car default-lib-dir) "tps-test-output.lib"))))
			     (core::store-item item))
			   (core::daterec dproof 'gwff "" t prov))))
		     (when (and (module-loaded-p 'library) moderec)
		       (moderec t))
		     (if timedout
			 (progn
			   (push theorem timed-out-list)
			   (msgf t t "Theorem " thm " with mode " mod " ran out of time with " timeout " seconds as limit." t)
			   (reroute-output-append file *default-pathname-defaults* 
						  (format t "~2%~S~%"
							  (format nil
								  "Timed out trying to prove ~d with mode ~d with timeout ~d seconds."
								  thm mod timeout))
						  (let ((*print-pretty* t))
						    (declare (special *print-pretty*)) 
						    (format t "~%((THM ~S) (MODE ~S) (RESULT TIMEDOUT ~S) (DATE \""
							    thm mod timeout)
						    (stringdt)
						    (format t "\")")
						    (when timing
						      (format t "~%(TIMING~%\"~%")
						      (auto::display-time-in-daterec)
						      (format t "\")~%(MEMORY~%\"~%")
						      (memory-status)
						      (format t "\")"))
						    (format t ")~%")))
			   (when testwin (auto::testwin-update-three "Timed out trying to prove " (princ-to-string thm) " with mode " mod " on " (stringdtl nil) "
Timing information for " (princ-to-string thm) ":
" (with-output-to-string (*standard-output*) (auto::display-time-in-daterec)) "

")))
		       (progn
			 (setq timed-out-list (remove theorem timed-out-list :test #'equal))
			 (if (get dproof 'core::plans)
			     (progn 
			       (reroute-output-append file *default-pathname-defaults* 
						      (format t "~2%~S~%"
							      (format nil "Proof of ~d with mode ~d terminated with planned lines remaining."
								      (princ-to-string thm) mod))
						      (let ((*print-pretty* t))
							(declare (special *print-pretty*)) 
							(format t "~%((THM ~S) (MODE ~S) (RESULT FAILED) (DATE \""
								thm mod)
							(stringdt)
							(format t "\")")
							(when timing
							  (format t "~%(TIMING~%\"~%")
							  (auto::display-time-in-daterec)
							  (format t "\")~%(MEMORY~%\"~%")
							  (memory-status)
							  (format t "\")"))
							(format t ")~%")))
			       (when testwin (auto::testwin-update-three "Proof of " (princ-to-string thm) " with mode " mod " terminated with planned lines remaining on " (stringdtl nil) "
Timing information for " (princ-to-string thm) ":
" (with-output-to-string (*standard-output*) (auto::display-time-in-daterec)) "

")))
			   (if (wffeq (line-assertion (car (last (proof-lines dproof)))) 
				      (proof-assertion dproof))
			       (progn
				 (when stop-on-success
				   (push thm success-thms))
				 (reroute-output-append file *default-pathname-defaults*
							(format t "~2%~S~%"
								(format nil "Proved ~d with mode ~d" thm mod))
							(let ((*print-pretty* t))
							  (declare (special *print-pretty*)) 
							  (format t "~%((THM ~S) (MODE ~S) (RESULT SUCCESS) (DATE \"" thm mod)
							  (stringdt)
							  (format t "\")")
							  (when timing 
							    (let* ((vh (auto::diffcount 'auto::diy))
								   (v1 (or (car vh) 0)) 
								   (v2 (/ (or (cadr vh) 0) internal-time-units-per-second))
								   (v3 (/ (or (caddr vh) 0) internal-time-units-per-second)))
							      (format t "~% ~S ~S ~S ~S"
								      (list "REALTIME" (* 1.0 v1))
								      (list "INTERNAL-RUNTIME" (* 1.0 v2))
								      (list "GC-TIME" (* 1.0 v3))
								      (list "I-GC-TIME" (* 1.0 (- v2 v3)))))
							    (format t "~%(TIMING~%\"~%")
							    (auto::display-time-in-daterec)
							    (format t "\")~%(MEMORY~%\"~%")
							    (memory-status)
							    (format t "\")"))
							  (format t ")~%")))
				 (when testwin (auto::testwin-update-three "Proved " (princ-to-string thm) " with mode " mod " on " (stringdtl nil) "
Timing information for " (princ-to-string thm) ":
" (with-output-to-string (*standard-output*) (auto::display-time-in-daterec)) "

")))
			     (progn 
			       (reroute-output-append file *default-pathname-defaults* 
						      (format t "~2%~S~%"
							      (format nil
								      "Proof of ~d with mode ~d terminated with a proof whose last line is not the theorem"
								      thm mod))
						      (let ((*print-pretty* t))
							(declare (special *print-pretty*)) 
							(format t "~%((THM ~S) (MODE ~S) (RESULT WRONG-LAST-LINE) (DATE \"" thm mod)
							(stringdt)
							(format t "\")")
							(when timing 
							  (format t "~%(TIMING~%\"~%")
							  (auto::display-time-in-daterec)
							  (format t "\")~%(MEMORY~%\"~%")
							  (memory-status)
							  (format t "\")"))
							(format t ")~%")))
			       (when testwin (auto::testwin-update-three "Proof of " (princ-to-string thm) " with mode " mod " terminated with a proof
whose last line is not the theorem on " (stringdtl nil) "
Timing information for " (princ-to-string thm) ":
" (with-output-to-string (*standard-output*) (auto::display-time-in-daterec)) "

"))))))
		       )))
#-(or :sbcl :cmu)  	 (interrupt-signal (c)
		 		   (reroute-output-append file *default-pathname-defaults*
							  (msgf t t "; Explicitly Interrupted : " c t))
		 		   (throwfail "Explicit Interrupt " c))
		 (error (c)
			(msgf "***ERROR*** : " c t)
			(reroute-output-append file *default-pathname-defaults*
					       (format t "~2%~S~%"
						       (format nil "***ERROR*** : ~d" c)))))
		(setq finished-flag t))
	      (when (numberp timeout)
		(setq timeout (* it-factor timeout)))))
	(unless finished-flag
	  (reroute-output-append file *default-pathname-defaults* 
				 (msgf t "; Crashed without finishing all the proofs" ) 
				 (stringdt)
				 (format t "(CRASHED~%")
				 (when timing 
				   (format t "(TIMING \"~%")
				   (auto::display-time-in-daterec)
				   (format t "\")"))
				 (format t ")~%"))
	  (when testwin (auto::testwin-update-three "Crashed without finishing all the proofs at " (stringdtl nil)
						    "Final timing information at point of crash:" 
						    (with-output-to-string (*standard-output*) (auto::display-time-in-daterec)))))
	))))

(defun tps-test-real2 (stop-on-success record moderec quiet-run expu modif file timing testwin prov timeout iterations it-factor)
  (declare (special core::*lib-masterindex* lib-bestmodes default-lib-dir))
  (let ((timed-out-list nil)
	(i 0))
    (when quiet-run (mode 'quiet '(test-theorems)))
    (when expu (unless (and (fboundp 'my-default-mode) (tps-mode-p 'my-default-mode))
		 (init-define-my-default-mode)))
    (when (and testwin (not *executing-batch*)) (auto::open-testwin-auto))
    (dolist (theorem test-theorems)
      (let ((thm (car theorem))
	    (mode (cdr theorem)))
	(unless (theorem-p thm)
	  (when (module-loaded-p 'library) 
	    (core::retrieve-libobject thm :type 'gwff :multiple nil))
	  (unless (theorem-p thm)
	    (throwfail t thm " is not a known theorem. Check the setting of TEST-THEOREMS." t)))
	(unless mode
	  (setq mode (caar (gethash thm lib-bestmodes))))
	(unless (member mode global-modelist)
	  (when (module-loaded-p 'library)
	    (let ((y (car (remove-if-not #'(lambda (x) (memq (car x) '(mode mode1)))
					 (gethash mode core::*lib-masterindex*)))))
	      (if y
		  (core::retrieve-libobject mode :type (car y) :multiple nil)
		(throwfail "Can't find mode " mode t))))
	  (unless (member mode global-modelist)
	    (throwfail t mode " is not a known mode. Check the setting of TEST-THEOREMS." t)))))
					;now we know that TEST-THEOREMS is OK, and we have all the necessary stuff loaded. 
    (let ((finished-flag nil)
	  (success-thms nil) ; cebrown 12/29/02 - for stop-on-success
	  (timedout nil))
      (when (probe-file file) (rename-file file (concatenate 'string (namestring file) ".bak")))
      (unwind-protect
	  (progn
	    (unless *executing-batch*
	      (reroute-output-append file *default-pathname-defaults* 
				     (msgf t t "; Starting TPS-TEST for " LISP-IMPLEMENTATION-TYPE t
					   "; running on " MACHINE-TYPE " at " SHORT-SITE-NAME t t)
				     (let ((*print-pretty* t))
				       (declare (special *print-pretty*)) 
				       (format t "~S~%"
					       (list "HEADER"
						     (list 'LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-TYPE)
						     (list 'MACHINE-TYPE MACHINE-TYPE)
						     (list 'SHORT-SITE-NAME SHORT-SITE-NAME)
						     (list 'TEST-THEOREMS TEST-THEOREMS)))))
	      (when testwin (auto::testwin-update-three "
Starting TPS-TEST for " LISP-IMPLEMENTATION-TYPE 
"
running on " MACHINE-TYPE " at " SHORT-SITE-NAME
"
TEST-THEOREMS is set to " TEST-THEOREMS "

")))
	    (loop while (or (eq iterations infinity) (< i iterations)) do
		  (incf i)
		  (dolist (theorem test-theorems)
		    (handler-case
		     (let ((thm (car theorem))
			   (mod (cdr theorem)))
		       (unless (or (member thm success-thms)
				   (and (> i 1) (not (member theorem timed-out-list :test #'equal))))
			 (setq timedout nil)
			 (when expu 
			   (auto::expunge) 
			   (mode 'my-default-mode '(test-theorems)) (gc))
			 (unless mod
			   (setq mod (caar (gethash thm lib-bestmodes))))
			 (mode mod '(test-theorems))
			 (when testwin (auto::testwin-update-three "Mating " (princ-to-string thm) " in mode " mod 
								   (if quiet-run " + QUIET" " ") t
								   (if modif (concatenate 'string 
											  "   after applying " test-modify) " ")
								   t))
			 (reroute-output-append file *default-pathname-defaults* 
						(format t "~2%~S~%"
							(format nil "Mating ~d in mode ~d~d~d"
								(princ-to-string thm) mod 
								(if quiet-run " + QUIET" "")
								(if modif (concatenate 'string 
										       " after applying " test-modify) ""))))
			 (when quiet-run (mode 'quiet '(test-theorems)))
			 (when modif (dolist (e (core::lisp-ize (core::tokenize-line test-modify)))
				       (eval e)))
			 (mate-partial thm t nil)
			 (if (equal timeout infinity)
			     (auto::matingsearch-controller)
			   #+(and allegro-version>= (version>= 5 0))
			   (mp:with-timeout (timeout (setq timedout t))
					    (auto::matingsearch-controller))
			   #-(and allegro-version>= (version>= 5 0))
			   (auto::matingsearch-controller)
			   )
			 (when (and (module-loaded-p 'library) record)
			   (if (core::locate-item thm :type 'gwff :writeable t :multiple nil)
			       (core::daterec dproof 'gwff "" t prov)
			     (progn
			       (let ((item (core::make-libitem
					    :name thm :type 'gwff :description (get thm 'assertion)
					    :mhelp "" :context nil
					    :provability nil
					    :proof-date nil
					    :other-attributes nil
					    :needed-objects nil
					    :other-remarks ""
					    :file (concatenate 'string (car default-lib-dir) "tps-test-output.lib"))))
				 (core::store-item item))
			       (core::daterec dproof 'gwff "" t prov))))
			 (if timedout
			     (progn
			       (push theorem timed-out-list)
			       (msgf t t "Theorem " thm " with mode " mod " ran out of time with " timeout " seconds as limit." t)
			       (reroute-output-append file *default-pathname-defaults*
						      (format t "~2%~S~%"
							      (format nil
								      "Timed out trying to mate ~d with mode ~d and time limit ~d seconds"
								      thm mod timeout))
						      (let ((*print-pretty* t))
							(declare (special *print-pretty*)) 
							(format t "~%((THM ~S) (MODE ~S) (RESULT MATE-TIMEDOUT ~S) (DATE \""
								thm mod timeout)
							(stringdt)
							(format t "\")")
							(when timing
							  (format t "~%(TIMING~%\"~%")
							  (auto::display-time 'auto::mating)
							  (format t "\")~%(MEMORY~%\"~%")
							  (memory-status)
							  (format t "\")"))
							(format t ")~%")))
			       (when testwin (auto::testwin-update-three "Timed out trying to mate " (princ-to-string thm) " with mode " 
									 mod " on " (stringdtl nil) "
Timing information for " (princ-to-string thm) ":
" (with-output-to-string (*standard-output*) (auto::display-time 'auto::mating)) "

" )))
			   (if (and (auto::mating-p auto::active-mating)
				    (auto::mating-completep auto::active-mating))
			       (progn
				 (setq timed-out-list (remove theorem timed-out-list :test #'equal))
				 (when (and (module-loaded-p 'library) moderec)
				   (moderec t))
				 (when stop-on-success
				   (push thm success-thms))
				 (reroute-output-append file *default-pathname-defaults*
							(format t "~2%~S~%"
								(format nil
									"Mated ~d with mode ~d" thm mod))
							(let ((*print-pretty* t))
							  (declare (special *print-pretty*)) 
							  (format t "~%((THM ~S) (MODE ~S) (RESULT MATE-SUCCESS) (DATE \"" thm mod)
							  (stringdt)
							  (format t "\")")
							  (when timing 
							    (let* ((vh (auto::diffcount 'auto::mating))
								   (v1 (or (car vh) 0)) 
								   (v2 (/ (or (cadr vh) 0) internal-time-units-per-second))
								   (v3 (/ (or (caddr vh) 0) internal-time-units-per-second)))
							      (format t "~% ~S ~S ~S ~S"
								      (list "REALTIME" (* 1.0 v1))
								      (list "INTERNAL-RUNTIME" (* 1.0 v2))
								      (list "GC-TIME" (* 1.0 v3))
								      (list "I-GC-TIME" (* 1.0 (- v2 v3)))))
							    (format t "~%(TIMING~%\"~%")
							    (auto::display-time 'auto::mating)
							    (format t "\")~%(MEMORY~%\"~%")
							    (memory-status)
							    (format t "\")"))
							  (format t ")~%")))
				 (when testwin (auto::testwin-update-three "Mated " (princ-to-string thm) " with mode " 
									   mod " on " (stringdtl nil) "
Timing information for " (princ-to-string thm) ":
" (with-output-to-string (*standard-output*) (auto::display-time 'auto::mating)) "

" )))
			     (progn
			       (setq timed-out-list (remove theorem timed-out-list :test #'equal))
			       (reroute-output-append file *default-pathname-defaults* 
						      (format t "~2%~S~%"
							      (format nil "Mating Failed for ~d with mode ~d."
								      (princ-to-string thm) mod))
						      (let ((*print-pretty* t))
							(declare (special *print-pretty*)) 
							(format t "~%((THM ~S) (MODE ~S) (RESULT MATE-FAILED) (DATE \""
								thm mod)
							(stringdt)
							(format t "\")")
							(when timing
							  (format t "~%(TIMING~%\"~%")
							  (auto::display-time 'auto::mating)
							  (format t "\")~%(MEMORY~%\"~%")
							  (memory-status)
							  (format t "\")"))
							(format t ")~%")))
			       (when testwin (auto::testwin-update-three "Mating Failed for " (princ-to-string thm) " with mode " mod " on " (stringdtl nil) "
Timing information for " (princ-to-string thm) ":
" (with-output-to-string (*standard-output*) (auto::display-time 'auto::mating)) "

"))))
			   )))
#-(or :sbcl :cmu)  		     (interrupt-signal (c)
				       (reroute-output-append file *default-pathname-defaults*
							      (msgf t t "; Explicitly Interrupted : " c t))
				       (throwfail "Explicit Interrupt " c))
		     (error (c)
			    (msgf "***ERROR*** : " c t)
			    (reroute-output-append file *default-pathname-defaults*
						   (format t "~2%~S~%"
							   (format nil "***ERROR*** : ~d" c)))))
		    (setq finished-flag t))
		  (when (numberp timeout)
		    (setq timeout (* it-factor timeout)))))
	(unless finished-flag
	  (reroute-output-append file *default-pathname-defaults* 
				 (msgf t "; Crashed without finishing all the proofs" ) 
				 (stringdt)
				 (format t "(CRASHED~%")
				 (when timing 
				   (format t "(TIMING \"~%")
				   (auto::display-time 'auto::mating)
				   (format t "\")~%(MEMORY \"~%")
				   (memory-status)
				   (format t "\")"))
				 (format t ")~%"))
	  (when testwin (auto::testwin-update-three "Crashed without finishing all the proofs at " (stringdtl nil)
						    "Final timing information at point of crash:" 
						    (with-output-to-string (*standard-output*) 
									   (auto::display-time 'auto::mating)))))
	))))

;;---------code for experiments on how flags affect timings----------

(defmexpr tps-test2
  (argtypes symbol yesno yesno filespec yesno)
  (argnames searchlist quiet-run expu output testwin)
  (arghelp "Searchlist to use?" "Run quietly?" "EXPUNGE and assert defaults between each theorem?" "Short output file" "Send output to the TEST window?")
  (defaultfns (lambda (s z e y v) (list s (if (eq z '$) 'yes z) (if (eq e '$) 'yes e) (if (eq y '$) "tps-test2-output.doc" y) (if (eq v '$) 'yes v))))
  (mhelp "Like TPS-TEST (see the help message for that command), but calls
the TEST top level and attempts to prove one theorem repeatedly with several
different values of some crucial flags, to see how the time taken will vary.

TEST-THEOREMS should contain a list of dotted pairs of theorems and modes 
in which they can be proven; the searchlist which is used should have at 
least one setting in which the theorem can be proven (otherwise tps-test2
will never finish that theorem).

The output file (by default, tps-test2-output.doc) will contain a summary of 
the results. If this file already exists, it will be renamed by adding .bak 
to its name."))

(defun tps-test2 (searchlist quiet-run expu file testwin)
  (declare (special core::*lib-masterindex* auto::*global-searchlist* auto::*test-top*
		    lib-bestmodes))
  (when quiet-run (mode 'quiet '(test-theorems)))
  (when expu (unless (and (fboundp 'my-default-mode) (tps-mode-p 'my-default-mode))
	       (init-define-my-default-mode)))
  (when testwin (auto::open-testwin-auto))
  (dolist (theorem test-theorems)
    (let ((thm (car theorem))
	  (mode (cdr theorem)))
      (unless (theorem-p thm)
	(when (module-loaded-p 'library) 
	  (core::retrieve-libobject thm :type 'gwff :multiple nil))
	(unless (theorem-p thm)
	  (throwfail t thm " is not a known theorem. Check the setting of TEST-THEOREMS." t)))
      (unless mode
	(setq mode (caar (gethash thm lib-bestmodes))))
      (unless (member mode global-modelist)
	(when (module-loaded-p 'library)
	  (let ((y (car (remove-if-not #'(lambda (x) (memq (car x) '(mode mode1)))
				       (gethash mode core::*lib-masterindex*)))))
	    (if y
		(core::retrieve-libobject mode :type (car y) :multiple nil)
	      (throwfail "Can't find mode " mode t))))
	(unless (member mode global-modelist)
	  (throwfail t mode " is not a known mode. Check the setting of TEST-THEOREMS." t)))))
  (unless (member searchlist (mapcar #'auto::searchlist-name auto::*global-searchlist*))
    (when (module-loaded-p 'library) 
      (core::retrieve-libobject searchlist :type 'slist :multiple nil))
    (unless (member searchlist (mapcar #'auto::searchlist-name auto::*global-searchlist*))
      (throwfail t searchlist " is not a known searchlist." t)))
					;now we know that we have all the necessary stuff loaded.
  (auto::new-searchlist searchlist)
  (let ((finished-flag nil))
    (when (probe-file file) (rename-file file (concatenate 'string (namestring file) ".bak")))
    (unwind-protect
	(progn
	  (reroute-output-append file *default-pathname-defaults* 
				 (msgf t t "; Starting TPS-TEST2 for " LISP-IMPLEMENTATION-TYPE t
				       "; running on " MACHINE-TYPE " at " SHORT-SITE-NAME t t)
				     (let ((*print-pretty* t))
				       (declare (special *print-pretty*))
				       (format t "~S~%"
					       (list "HEADER"
						     (list 'LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-TYPE)
						     (list 'MACHINE-TYPE MACHINE-TYPE)
						     (list 'SHORT-SITE-NAME SHORT-SITE-NAME)
						     (list 'TEST-THEOREMS TEST-THEOREMS)))))
	  (when testwin (auto::testwin-update-three "
Starting TPS-TEST for " LISP-IMPLEMENTATION-TYPE 
"
running on " MACHINE-TYPE " at " SHORT-SITE-NAME
"
TEST-THEOREMS is set to " TEST-THEOREMS "

"))
	  (dolist (theorem test-theorems)
		  (let ((thm (car theorem))
			(mod (cdr theorem)))
		    (when expu (auto::expunge) (mode 'my-default-mode '(test-theorems)) (gc))
		    (unless mod
		      (setq mod (caar (gethash thm lib-bestmodes))))
		    (when testwin (auto::testwin-update-three "Proving " (princ-to-string thm) " in mode " mod 
							      (if quiet-run " + QUIET" " ") t
							      t))
		    (reroute-output-append file *default-pathname-defaults* 
					   (format t "~2%~S~%"
						   (format nil "Proving ~d in mode ~d~d" thm mod
							   (if quiet-run " + QUIET" " "))))
		    (mode mod '(test-theorems))
		    (set-flag 'auto::test-reduce-time nil)
		    (set-flag 'auto::test-increase-time 0)
		    (set-flag 'auto::test-next-search-fn 'auto::exhaustive-search)
		    (set-flag 'auto::test-fix-unif-depths nil)
		    (when quiet-run (mode 'quiet '(test-theorems)))
		    (test-partial thm t nil)
		    (auto::go-test (list file) nil)
		    (when testwin (auto::testwin-update-three "Finished " 
							      (princ-to-string thm) 
							      " on " (stringdtl nil))))
		  (setq finished-flag t)))
      (setq auto::*test-top* nil)
      (unless finished-flag
	      (reroute-output-append file *default-pathname-defaults* 
				     (msgf t "; Crashed without finishing all the proofs at " ) 
				     (stringdt)) 
	      (when testwin (auto::testwin-update-three "Crashed without finishing all the proofs at " 
							(stringdtl nil)))))))


(defun test-partial (gwff deepen window &optional (uniform nil))
  (declare (special auto::*test-gwff-name* auto::*test-gwff* auto::*test-mode-name* 
		    auto::*test-total-time* auto:*test-top*))
  (setq auto::*test-gwff-name* gwff)
  (setq auto::*test-gwff* (or 
			   (and (stringp gwff) (get-gwff0 gwff))
			      (and (listp gwff) gwff)
			      (and (auto::eproof-p gwff) gwff)
			      (get gwff 'represents)
			      (get gwff 'assertion)
			      (and (weak-label-p gwff) (get-weak gwff))))
  (when (stringp auto::*test-gwff*) (setq auto::*test-gwff* (get-gwff0 gwff)))
  (setq auto::*test-mode-name* (make-symbol (concatenate 'string (princ-to-string auto::*test-gwff-name*)
							 "-TEST-MODE")))
  (setq gwff auto::*test-gwff*)
  (setq auto::*test-total-time* 0)
  (let ((displaywff t)
	(ppwfflag nil)
	(prev-node nil)
	(nodestack nil)
	(cmdstack nil)
	(current-topnode nil)
	(strong-defaults 'strong-mate-defaults)
	(printtypes printtypes))
    (declare (special displaywff printdepth ppwfflag  prev-node nodestack
		      cmdstack strong-defaults  printtypes current-eproof
		      current-topnode))
    (auto::test-wff-prefix gwff deepen window uniform)
    (setq auto::*test-top* t)))

(defun mate-partial (gwff deepen window)
  (let* ((gwff (or (and (stringp gwff) (get-gwff0 gwff))
		  (and (listp gwff) gwff)
		  (and (auto::eproof-p gwff) gwff)
		  (get gwff 'represents)
		  (get gwff 'assertion)
		  (and (weak-label-p gwff) (get-weak gwff))))
	(gwff (if (stringp gwff) (get-gwff0 gwff) gwff))
	(displaywff t)
	(ppwfflag nil)
	(prev-node nil)
	(nodestack nil)
	(cmdstack nil)
	(current-topnode nil)
	(strong-defaults 'strong-mate-defaults)
	(printtypes printtypes))
    (declare (special displaywff printdepth ppwfflag  prev-node nodestack
		      cmdstack strong-defaults  printtypes current-eproof
		      current-topnode))
    (auto::mate-wff-prefix gwff deepen window)))

(defun memory-status ()
  (msgf "-------------------------------------------------" t)
  (msgf "Memory Status:" t)
  (room nil)
  (msgf "-------------------------------------------------" t))
  
(defmexpr setup-online-access
  (mhelp "SETUP-ONLINE-ACCESS allows a user to set up a file of userids and passwords
for remote access to a TPS server over the web.  For example, this can
be used by a teacher to set up a file of userids and passwords
for a class to use ETPS online.

See Also: USER-PASSWD-FILE"))

(defun setup-online-access ()
  (let ((exists nil)
	(modify nil)
	(new nil))
    (when (probe-file USER-PASSWD-FILE)
      (setq exists t)
      (msgf "There is already a file " USER-PASSWD-FILE t
	    "of userids and passwords." t)
      (if (query "Modify file?" t)
	  (setq modify t)
	(if (query "Delete and create a new file?" t)
	    (setq new t)
	  (throwfail "Cannot add userids and password."))))
    (if modify
	(modify-online-access)
      (let ((etps-access (modify-online-access-1 'ETPS nil nil))
	    (tps-access (modify-online-access-1 'TPS nil nil)))
	(let ((f (if exists
		     (open USER-PASSWD-FILE :direction :output
			   :if-exists (if new :supersede nil))
		   (open USER-PASSWD-FILE :direction :output
			 :if-does-not-exist :create))))
	  (format f "~S~%" (list etps-access tps-access))
	  (close f))))))

(defun modify-online-access ()
  (let* ((f1 (open USER-PASSWD-FILE :direction :input))
	 (current-access (read f1 nil nil))
	 (etps-access (assoc 'ETPS current-access))
	 (tps-access (assoc 'TPS current-access)))
    (close f1)
    (if etps-access
	(setq etps-access (modify-online-access-1 'ETPS (cadr etps-access) (caddr etps-access)))
      (setq etps-access (modify-online-access-1 'ETPS nil nil)))
    (if tps-access
	(setq tps-access (modify-online-access-1 'TPS (cadr tps-access) (caddr tps-access)))
      (setq tps-access (modify-online-access-1 'TPS nil nil)))
    (let ((f (open USER-PASSWD-FILE :direction :output :if-exists :supersede)))
      (format f "~S~%" (list etps-access tps-access))
      (close f))))

(defun modify-online-access-1 (mode all id-pass-list)
  (let ((id-pass-list2 nil)
	(again t)
	(userid "")
	(passwd ""))
    (prompt-read all nil
		 (msgf "Allow " mode " Anonymous Access To Everyone?")
		 'yesno all ((? (msg "Yes will mean userid and passwd are not required for online access"))
			     (?? (mhelp 'setup-online-access))))
    (when all
      (msgf "Although anyone can run " mode t
	    "You may still wish to add specific users which will be allowed to save files " t
	    "in a directory."))
    (dolist (id-pass id-pass-list)
      (msgf "User Id: " (car id-pass) t)
      (if (query "Keep this User with same Password?" t)
	  (push id-pass id-pass-list2)
	(if (query "Change this User's Password?" t)
	    (progn
	      (loop while (equal passwd "") do
		    (prompt-read passwd nil (msgf "Password ") 'string ""
				 ((? (msg "Password for remote user")) (?? (mhelp 'setup-online-access))))
		    (if (equal passwd "")
			(msgf "Cannot Give Empty Password.")
		      (push (list (car id-pass) (encrypt-password passwd)) id-pass-list2)))
	      (setq passwd ""))
	  (msgf "User Deleted"))))
    (prompt-read again nil (msgf "Add a userid? ") 'yesno 'yes nil)
    (loop while again do
	  (msg t)
	  (prompt-read userid nil (msgf "User Id ") 'string "" nil)
	  (unless (equal userid "")
	    (prompt-read passwd nil (msgf "Password ") 'string "" nil)
	    (if (equal passwd "")
		(msgf "Cannot give an empty password.  Not adding user " userid t)
	      (progn
		(push (list userid (encrypt-password passwd)) id-pass-list2)
		(msgf "Added user " userid t))))
	  (prompt-read again nil (msgf "Add another userid? ") 'yesno 'yes nil))
    (list mode all id-pass-list2)))
      
