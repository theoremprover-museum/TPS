;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of BARE)

(deffile tps3-save
  (part-of bare)
  (extension lisp)
  (mhelp "Routines for saving core image, and list of expert users."))

;;; core-name is the default name of the core image.  TPS will look for
;;; looks for core-name.PATCH, core-name.NEWS.
;;; This need not correspond to the SAVE-FILE, however (for building
;;; new experimental core images, it is good to allow them to be
;;; separate.

(defvar core-name #+tops-20 "TPS3" #-tops-20 "tps3")
(defvar *using-interface* NIL)
(defvar *simple-interface-prompts* NIL)
(defvar *running-remotely* NIL)
(defvar *expert-running-remotely* NIL)
(defvar *remote-userid* NIL)
(defvar *command-process* NIL)
(defvar *executing-batch* NIL)
(defvar *omega-switch* nil)
(defvar *omega-proof* nil)
(defvar *omega-proofname* nil)
(defvar etps-core-name #+tops-20 "ETPS" #-tops-20 "etps")
(defvar *tps-server-name* nil)
(defvar sys-dir "")
(defvar ini-file-dir "")
(defvar patch-file-dir "")
(defvar save-work-file nil)
(defvar core-abbrevlist nil)
(defvar core-theoremlist nil)
(defvar core-binderabbrevlist nil)
(defvar core-constlist nil)

;;; Added the check for first-entry 9/4/87 DAN

(defun secondary-top-main ()
  (declare (special init-dialogue init-dialogue-fn default-lib-dir backup-lib-dir
		    *executing* auto::*external-request-queue-process*
		    *lib-masterindex* global-abbrevlist global-theoremlist
		    global-binderlist global-logconstlist global-pmpropsymlist))
  #+(and (or (and :excl :vax) (and :allegro :andrew)) (not (and allegro-version>= (version>= 5 0))))
  (set-up-homedir-and-user)
  (setq lisp-implementation-type (concatenate 'string (lisp-implementation-type) " " (lisp-implementation-version))
   machine-type (machine-type) short-site-name (short-site-name) machine-instance (machine-instance))
  #+(and :excl :linux86)(fix-machine-instance)
  (clear-input *standard-input*)
  #+tops-20(initialize-sys-dir) 
  (when first-entry
 ;;; following goes directly into grader package if "-grader" appears
 ;;; on the command line when in CMUCL, AllegroCL, EXCL or IBCL -- DAN
    #+(or :sbcl :cmu :allegro :excl :ibcl :lcl3.0 :clisp)
    (let ((args #+:cmu extensions:*command-line-strings*
		  #+(or :allegro :excl)(sys:command-line-arguments)
		  #+:clisp ext::*args*
		  #+:sbcl sb-ext:*posix-argv*
		  #+:lcl3.0
		  (do* ((i 1 (1+ i))
			(arg (lcl:command-line-argument i)
			     (lcl:command-line-argument i))
			(args (if arg (list arg)) 
			      (if arg (cons arg args) args)))
		      ((null arg) args))
		  #+(or ibcl kcl)
		  (let ((args nil))
		    (dotimes (i (system:argc) args)
		      (push (system:argv i) args)))))
      (when (member "-grader" args :test #'string-equal)
	    (use-package "TEACHER" (find-package "CL-USER"))
	    (setq *default-pathname-defaults* (pathname ""))
	    (setq core::smart-load nil)
	    (teacher::do-grades)
	    (exit-from-lisp))
      (setq *omega-switch* nil *omega-proofname* nil *omega-proof* nil)
      (when (member "-omega" args :test #'string-equal)
	(setq *default-pathname-defaults* (pathname ""))
	(let ((savedname (if (member "-outfile" args :test #'string-equal)
			     (string-downcase 
			      (princ-to-string (cadr (member "-outfile" args :test #'string-equal))))
			   nil)))
	  (if (member "-batch" args :test #'string-equal) (execute-batch args) (execute-batch args nil t))
	  (when (null savedname)
	    (if (equal dproof '*****)
		(setq savedname "tps-omega.prf")
	      (setq savedname (concatenate 'string (string-downcase (string dproof)) "-result.prf"))))
	  (initialize-omega (make-pathname% :name savedname))
	  (absolute-top-top)))
      (when (member "-batch" args :test #'string-equal)
	    (if (member "-outfile" args :test #'string-equal)
		(progn (setq *default-pathname-defaults* (pathname ""))
		       (reroute-output-append (make-pathname% 
					       :name (princ-to-string (cadr (member "-outfile" args :test #'string-equal))))
					      *default-pathname-defaults*
					      (execute-batch args 
							     (princ-to-string (cadr (member "-outfile" args 
											    :test #'string-equal)))))
		       (exit-from-lisp))
	      (progn
		(execute-batch args)
		(exit-from-lisp))))
      (when (member "-problem" args :test #'string-equal)
	(if (member "-outfile" args :test #'string-equal)
	    (progn (setq *default-pathname-defaults* (pathname ""))
		   (reroute-output-append (make-pathname% 
					   :name (princ-to-string (cadr (member "-outfile" args :test #'string-equal))))
					  *default-pathname-defaults*
					  (execute-problem args 
							   (princ-to-string (cadr (member "-outfile" args 
											  :test #'string-equal)))))
		   (exit-from-lisp))
	  (progn
	    (execute-problem args)
	    (exit-from-lisp))))
    (msg T core-name " for " (status-userid) ". Version from " date-tps3-saved
	 T
	 "(c) Copyrighted by Carnegie Mellon University."
	 " All rights reserved.")
    #+ibcl(setq cond::*restart-clusters* nil)
    (%top-catch%
     (progn
       (let ((news-file (make-pathname% :directory news-dir :name core-name
					:type "news")))
	 (when (probe-file news-file) (filetype news-file)))
       (let ((patch-file (make-pathname% :directory patch-file-dir
					 :name core-name :type "patch")))
	 (when (probe-file patch-file)
	       ;; While loading patch-file, we want to be an expert. DAN 23OCT91
	       ;; changed further so that the expertflag is set only if the user is an expert.
	   (let ((old-expertflag expertflag))
	     (unwind-protect
		 (progn
		   (setq expertflag (or (eq expert-list t)
					(member (status-userid) expert-list :test #'string-equal)))
		   (msg F "[Loading changes ...")
		   (load patch-file)
		   (msg T "                 ... done]"))
	       (setq expertflag old-expertflag)))))
       (when (string-equal core-name "grader")
	 (disable-events)
	 (use-package "TEACHER" (find-package "CL-USER"))
	 (loop (%top-catch% (teacher::do-grades) (teacher::gradetop)))
	 )
       (when (boundp 'global-abbrevlist)
	 (setq core-abbrevlist (remove-if-not #'symbolp global-abbrevlist)))
       (when (boundp 'global-theoremlist)
	 (setq core-theoremlist (remove-if-not #'symbolp global-theoremlist)))
       (when (boundp 'global-binderlist)
	 (setq core-binderabbrevlist (remove-if-not 
				      #'(lambda (x)
					  (and (symbolp x)
					       (get x 'defn)))
				      global-binderlist)))
       (when (and (boundp 'global-logconstlist) (boundp 'global-pmpropsymlist))
	 (setq core-constlist 
	       (remove-if-not #'symbolp (append global-logconstlist global-pmpropsymlist))))
       (let* ((expert-flag (or (eq expert-list t)
			       (member (status-userid) expert-list :test #'string-equal)))
	      (ini-file1
	       (probe-file
		(make-pathname% :directory ini-file-dir 
				:name core-name :type "ini")))
	      (ini-file
	       (and expert-flag
		    (probe-file
		     (merge-pathnames 
		      (make-pathname% :name core-name :type "ini")
		      (user-homedir-pathname))))))
					; delay building the hash table until all ini files are loaded - 5/24/02
	 #+TPS(when (module-loaded-p 'library) (setf (gethash 'dont-hash *lib-masterindex*) t))
	 (when ini-file1 (msg F) (load ini-file1 :verbose expert-flag))
	 (when ini-file (msg F) (load ini-file :verbose t))
	 #+TPS(if init-dialogue (funcall init-dialogue-fn))
	 #+TPS(when (module-loaded-p 'library) (setf (gethash 'dont-hash *lib-masterindex*) nil) (restore-lib-hashtable))
	 (organize-all))))
    (let ((server-args (member "-server" args :test #'string-equal))) ; act as a server to start new TPS/ETPS's for remote access
      (when server-args
	#-(and allegro-version>= (version>= 5 0))
	(progn
	  (warning "External Services are only available for TPS running under Allegro CL Version >= 5.0")
	  (exit))
	#+(and allegro-version>= (version>= 5 0))
	(let* ((lisp-exec (car args)) ; first argument should be the lisp executable
	       (tps-image (nth 1 server-args))
	       (etps-image (nth 2 server-args))
	       (logdir-info (member "-logdir" server-args :test #'string-equal))
	       (logdir (if logdir-info (cadr logdir-info) "logs"))
	       (default-port-info (member "-port" server-args :test #'string-equal))
	       (default-port-1 29090)
	       (default-port-2 (if default-port-info (read-from-string (cadr default-port-info) nil nil)
				 default-port-1))
	       (default-port (if (numberp default-port-2) default-port-2 default-port-1)))
	  (tps-server-main lisp-exec tps-image etps-image logdir default-port))))
    (let ((remoteuser-args (member "-remoteuser" args :test #'string-equal))) ; remote
      (when remoteuser-args
	#-(and allegro-version>= (version>= 5 0))
	(progn
	  (warning "External Services are only available for TPS or ETPS running under Allegro CL Version >= 5.0")
	  (exit))
	#+(and allegro-version>= (version>= 5 0))
	(let* ((user (cadr remoteuser-args))
	       (oldport (read-from-string (caddr remoteuser-args)))
	       (str (acl-socket:make-socket :remote-host MACHINE-INSTANCE
					    :remote-port oldport))
	       (pass (acl-socket:make-socket :connect :passive))
	       (port (acl-socket:local-port pass)))
	  (ensure-directories-exist (windows-compatible-path (format nil "~d/" user)))
	  (chdir (windows-compatible-path (format nil "~d/" user)))
	  (format str "~d~%" port)
	  (force-output str)
	  (close str)
	  (let ((str (acl-socket:accept-connection pass))
		(stdout *standard-output*))
	    (msgf "Connection Established")
	    (setq *remote-userid* (intern user))
	    (setq *standard-output* str)
	    (let ((remote-input-proc
		   (mp:process-run-function "remote-input"
					    #'javaservice-remote-input
					    str stdout)))
	      (setf (mp:process-priority remote-input-proc) 1)
	      (setq *using-interface* T)
	      (setq *running-remotely* T)
	      (setq *expert-running-remotely* NIL)
	      (set-flag 'style 'istyle)
	      (set-flag 'window-style 'istyle)
	      (setq *command-process* (mp:process-run-function
				       "Main Command Process"
				       #'absolute-top-top))
	      (setf (mp:process-priority *command-process*) 10)
	      (mp:process-sleep (* 24 3600)) ; die after a day
	      (exit))))))
    (let ((javainterface-args (member "-javainterface" args :test #'string-equal))) ; local
      (when javainterface-args
	#-(and allegro-version>= (version>= 5 0))
	(progn
	  (warning "The Java interface only available for TPS running under Allegro CL Version >= 5.0")
	  (exit))
	#+(and allegro-version>= (version>= 5 0))
	(let ((java-comm "")
	      (rest-args (cdr javainterface-args)))
	  (do ((r rest-args (cdr r)))
	      ((or (null r)
		   (string-equal (car r) "-other"))
	       (setq rest-args (cdr r)))
	      (if (find #\Space (car r))
		  (setq java-comm (format nil "~d \"~d\"" java-comm (car r)))
		(setq java-comm (format nil "~d ~d" java-comm (car r)))))
	  (when (member "-nopopups" rest-args :test #'string=)
	    (setq *simple-interface-prompts* T))
	  (let* ((pass (acl-socket:make-socket :connect :passive))
		 (port (acl-socket:local-port pass))
		 (command 
		  (string-left-trim " " (format nil "~d ~d ~d" java-comm MACHINE-INSTANCE port))))
	    (dolist (arg rest-args) ; fontsize, etc
		    (setq command (format nil "~d ~d" command arg)))
	    #+ETPS(setq command (format nil "~d -etps" command))
	    (excl:run-shell-command command :wait nil)
	    (let* ((str (acl-socket:accept-connection pass))
		   (stdout *standard-output*))
	      (setq *standard-output* str)
	      (let ((remote-input-proc (mp:process-run-function "remote-input"
								#'javaservice-remote-input
								str stdout)))
		(setf (mp:process-priority remote-input-proc) 1)
		(setq *using-interface* T)
		(set-flag 'style 'istyle)
		(set-flag 'window-style 'istyle)
		(setq *command-process* (mp:process-run-function
					 "Main Command Process"
					 #'absolute-top-top))
		(setf (mp:process-priority *command-process*) 10)
		(mp:process-disable sys:*current-process*)))))))
    (let ((lservice-args (member "-lservice" args :test #'string-equal)))
      (when lservice-args
	#-(and allegro-version>= (version>= 5 0))
	(progn
	  (warning "External Services are only available for TPS running under Allegro CL Version >= 5.0")
	  (exit))
	#+(and allegro-version>= (version>= 5 0))
	(let ((local-port (read-from-string (nth 1 lservice-args))))
	  (auto::socket~reset)
	  (auto::socket~define :local)
	  (auto::socket~define :inout)
	  (auto::socket~define :service)
	  (auto::socket~define :inoutpass)
	  (auto::socket~define :servicepass)
	  (auto::socket~connect MACHINE-INSTANCE local-port :local)
	  (auto::socket~bind-new-port :inoutpass)
	  (auto::socket~bind-new-port :servicepass)
	  (auto::socket~write
	   (format nil "(~A ~A)"
		   (auto::socket~port :inoutpass)
		   (auto::socket~port :servicepass))
	   :local)
	  (auto::socket~close :local)
	  (auto::socket~accept :inoutpass :inout)
	  (auto::socket~accept :servicepass :service)
	  (setq *tps-server-name* (intern (format nil "TPS@~A-~A"
						  MACHINE-INSTANCE
						  (get-universal-time))))
	  (setq auto::*executing* t) ; to prevent user interrupts
	  (setq auto::*external-request-queue-process*
		(mp:process-run-function "the external request queue listener" #'auto::process-request-queue))
	  (auto::tpr~start)
	  (auto::tpr~initialize-listeners)
	  (mp:process-disable sys:*current-process*))))
    (let ((service-args (member "-service" args :test #'string-equal)))
      (when service-args
	#-(and allegro-version>= (version>= 5 0))
	(progn
	  (warning "External Services are only available for TPS running under Allegro CL Version >= 5.0")
	  (exit))
	#+(and allegro-version>= (version>= 5 0))
	(let ((host (nth 1 service-args))
	      (inout-port (read-from-string (nth 2 service-args)))
	      (serv-port (read-from-string (nth 3 service-args)))
	      )
	  (auto::socket~reset)
	  (auto::socket~define :inout)
	  (auto::socket~connect host inout-port :inout)
	  (auto::socket~define :service)
	  (auto::socket~connect host serv-port :service)
	  (setq *tps-server-name* (intern (format nil "TPS@~A-~A" host (get-universal-time))))
	  (setq auto::*executing* t) ; to prevent user interrupts
	  (setq auto::*external-request-queue-process*
		(mp:process-run-function "the external request queue listener" #'auto::process-request-queue))
	  (auto::tpr~start)
	  (auto::tpr~initialize-listeners)
	  (mp:process-disable sys:*current-process*)
	  )
	))))

  (setq first-entry nil)
  (when save-file-closed-on-exit
    (setq save-file-closed-on-exit nil)
    (open-save-file save-work-file)
    (format T "~A resuming save-work in file ~A."
	    (status-userid) save-work-file))
  (when (and (boundp 'save-work-on-start-up) save-work-on-start-up
	     (not saving-work-p))
    (open-save-file
     (multiple-value-bind (second minute hour date month year
				  day-of-week daylight-saving-time-p
				  time-zone) 
			  (get-decoded-time)
       (declare (ignore daylight-saving-time-p time-zone year
			day-of-week second))
       (format nil "~[abc~;January~;February~;March~;April~;May~;June~;July~;~
	    August~;September~;October~;November~;December~]~A-~2D-~2,'0D"
	       month date hour minute)))
    (format T "~A saving in file ~A."  (status-userid) save-work-file))
  (absolute-top-top)
)

(defun execute-batch (args &optional (filename) (nobatch))
  (declare (special init-dialogue init-dialogue-fn *lib-masterindex*
		    auto:test-next-search-fn auto::*test-mode-name*
		    auto:current-eproof auto:active-mating
		    auto:default-tactic default-lib-dir
		    *executing-batch*)
	   (ignore filename)) ; commented out msgs that use filename
  (setq *executing-batch* t)
  (setq *debug-io* *standard-output*)
  (setq *terminal-io* *standard-output*)
  (setq *error-output* *standard-output*)
  (msg T core-name " for " (status-userid) ". Version from " date-tps3-saved
       T
       "(c) Copyrighted by Carnegie Mellon University."
       " All rights reserved.")
  #+ibcl(setq cond::*restart-clusters* nil)
  (%top-catch%
   (progn
     (let ((news-file (make-pathname% :directory news-dir :name core-name
				      :type "news")))
       (when (probe-file news-file) (filetype news-file)))
     (let ((patch-file (make-pathname% :directory patch-file-dir
				       :name core-name :type "patch")))
       (when (probe-file patch-file)
	     (let ((old-expertflag expertflag))
	       (unwind-protect
		   (progn
		     (setq expertflag (or (eq expert-list t)
					  (member (status-userid) expert-list :test #'string-equal)))
		     (msg F "[Loading changes ...")
		     (load patch-file)
		     (msg T "                 ... done]"))
		 (setq expertflag old-expertflag)))))
     (let* ((expert-flag (or (eq expert-list t)
			     (member (status-userid) expert-list :test #'string-equal)))
	    (ini-file1
	     (probe-file
	      (make-pathname% :directory ini-file-dir 
			      :name core-name :type "ini")))
	    (ini-file
	     (and expert-flag
		  (probe-file
		   (merge-pathnames 
		    (make-pathname% :name core-name :type "ini")
		    (user-homedir-pathname))))))
       (when ini-file1 (msg F) (load ini-file1 :verbose expert-flag))
       (when ini-file (msg F) (load ini-file :verbose t))
       #+TPS(if init-dialogue (funcall init-dialogue-fn))
       (let ((f (member "-testwinfile" args :test 'string-equal)))
	 (when (and f (stringp (cadr f)) (probe-file (cadr f)))
	   (setq auto::*testwin-opened* (open (cadr f) :direction :output
					      :if-exists :append
					      :if-does-not-exist :create))))
       (organize-all))))
  (setq *default-pathname-defaults* (pathname ""))
  (unless nobatch
    (let ((file (namestring (make-pathname% 
			     :name (princ-to-string (cadr (member "-batch" args :test 'string-equal)))
			     :type "work"))))
      (setq core::smart-load nil)
      (unless (pathnamep (locate-tps-file file source-path))
	(setq file (namestring (make-pathname% :name 
					       (princ-to-string (cadr (member "-batch" args :test 'string-equal)))))))
;      (reroute-output-append "tps-batch-jobs" *default-pathname-defaults*
;			     (msgf "Attempting to run work file " file)
;			     (when filename (msg " with output to " filename)) 
;			     (msgf "   on " (stringdt nil) t))
      (msgf "Attempting to run work file " file)
      (if (pathnamep (locate-tps-file file source-path))
	  (progn
	    (restore-work (locate-tps-file file source-path) t "TTY:")
;	    (reroute-output-append "tps-batch-jobs" *default-pathname-defaults*
;				   (msgf "Ending run of work file " file)
;				   (when filename (msg " with output to " filename)) 
;				   (msgf "   on " (stringdt nil) t t t))
	    )
	(progn 
;	  (reroute-output-append "tps-batch-jobs" *default-pathname-defaults*
;				 (msgf "Aborted :no such work file as " file t "   on " (stringdt nil) t t t))
	  (msgf "No such work file."))))))

(defun execute-problem (args &optional (filename))
  (declare (special init-dialogue init-dialogue-fn *lib-masterindex* auto::*test-mode-name* auto::*TEST-MODE-NAME* AUTO:CURRENT-EPROOF AUTO:ACTIVE-MATING AUTO:DEFAULT-TACTIC DEFAULT-LIB-DIR AUTO::TEST-NEXT-SEARCH-FN)
	   (ignore filename)) ; commented out msgs that use filename
  (setq *debug-io* *standard-output*)
  (setq *terminal-io* *standard-output*)
  (setq *error-output* *standard-output*)
  (msg T core-name " for " (status-userid) ". Version from " date-tps3-saved
       T
       "(c) Copyrighted by Carnegie Mellon University."
       " All rights reserved.")
  #+ibcl(setq cond::*restart-clusters* nil)
  (%top-catch%
   (progn
     (let ((news-file (make-pathname% :directory news-dir :name core-name
				      :type "news")))
       (when (probe-file news-file) (filetype news-file)))
     (let ((patch-file (make-pathname% :directory patch-file-dir
				       :name core-name :type "patch")))
       (when (probe-file patch-file)
	     (let ((old-expertflag expertflag))
	       (unwind-protect
		   (progn
		     (setq expertflag (or (eq expert-list t)
					  (member (status-userid) expert-list :test #'string-equal)))
		     (msg F "[Loading changes ...")
		     (load patch-file)
		     (msg T "                 ... done]"))
		 (setq expertflag old-expertflag)))))
     (let* ((expert-flag (or (eq expert-list t)
			     (member (status-userid) expert-list :test #'string-equal)))
	    (ini-file1
	     (probe-file
	      (make-pathname% :directory ini-file-dir 
			      :name core-name :type "ini")))
	    (ini-file
	     (and expert-flag
		  (probe-file
		   (merge-pathnames 
		    (make-pathname% :name core-name :type "ini")
		    (user-homedir-pathname))))))
       (when ini-file1 (msg F) (load ini-file1 :verbose expert-flag))
       (when ini-file (msg F) (load ini-file :verbose t))
       #+TPS(if init-dialogue (funcall init-dialogue-fn))
       (organize-all))))
  (setq *default-pathname-defaults* (pathname ""))
  (let ((problem (princ-to-string (cadr (member "-problem" args :test 'string-equal))))
	(mode (princ-to-string (or (cadr (member "-mode" args :test 'string-equal)) 'uniform-search-mode)))
	(slist (princ-to-string (or (cadr (member "-slist" args :test 'string-equal)) 'uniform-search-2)))
	(record (member "-record" args :test 'string-equal))
	(fail-string nil))
    (setq core::smart-load nil)
    (setq problem (or (find-symbol problem (find-package 'user))
		      (let ((foo (intern (string-upcase problem) (find-package 'user))))
			(export foo)
			foo)))
    (setq mode (or (find-symbol mode (find-package 'user))
		   (let ((foo (intern (string-upcase mode) (find-package 'user))))
		     (export foo)
		     foo)))
    (setq slist (or (find-symbol slist (find-package 'user))
		    (let ((foo (intern (string-upcase slist) (find-package 'user))))
		      (export foo)
		      foo)))
    (unless (theorem-p problem)
	    (when (module-loaded-p 'library)
		  (retrieve-libobject problem :type 'gwff :multiple nil :fail-gently t))
	    (unless (theorem-p problem)
		    (setq fail-string (concatenate 'string
						   (princ-to-string problem)
						   " is not a known theorem. Check the setting of DEFAULT-LIB-DIR."))))
    (unless (or fail-string (member mode global-modelist))
	    (when (module-loaded-p 'library) 
		  (let ((y (car (remove-if-not #'(lambda (x) (memq (car x) '(mode mode1)))
					       (gethash mode core::*lib-masterindex*)))))
		    (when y
			  (retrieve-libobject mode :type (car y) :multiple nil :fail-gently t))))
	    (unless (member mode global-modelist)
		    (setq fail-string (concatenate 'string
						   (princ-to-string mode)
						   " is not a known mode. Use the -mode switch to specify a mode,
and check the setting of DEFAULT-LIB-DIR."))))
    (unless (or (auto::known-searchlist-p slist) fail-string)
	    (when (module-loaded-p 'library) 
		  (retrieve-libobject slist :type 'slist :multiple nil :fail-gently t))
	    (unless (auto::known-searchlist-p slist)
		    (setq fail-string (concatenate 'string
						   (princ-to-string slist)
						   " is not a known searchlist. Use the -slist switch to specify a searchlist,
and check the setting of DEFAULT-LIB-DIR."))))
;    (reroute-output-append "tps-batch-jobs" *default-pathname-defaults*
;			   (msgf "Attempting to run problem " problem " in mode " mode t "   with searchlist " slist)
;			   (when filename (msgf "   with output to " filename)) 
;			   (msgf "   on " (stringdt nil) t))
    (msgf "Attempting to run problem " problem " in mode " mode " with searchlist " slist)
    (if fail-string
	(progn
;	  (reroute-output-append "tps-batch-jobs" *default-pathname-defaults*
;				 (msgf "Aborted  on " (stringdt nil) t "  " fail-string t t))
	  (msgf "Aborted : " fail-string t))
      (%top-catch% (progn (auto::test-wff-unif-partial problem nil mode slist t)
			  (auto::startcount 'auto::diy)
			  (mode 'quiet)
			  (setq auto::test-next-search-fn 'auto::push-up-2)
			  (auto::go-test auto::*test-mode-name* nil)
			  (setq fail-string "There was an error in the merging or translation.")
			  (when (and (boundp 'auto::current-eproof)
				     (auto::eproof-p auto::current-eproof)
				     (auto::etree-p (auto::eproof-etree auto::current-eproof))
				     (boundp 'auto::active-mating)
				     (auto::mating-p auto::active-mating)
				     (auto::mating-completep auto::active-mating)
				     (not (auto::eproof-merged auto::current-eproof)))
				(msgf "Merging the expansion tree.  Please stand by." t)
				(auto::merge-tree)
				(auto::etree-nat problem 1000 auto::default-tactic 'auto)
				(when (and (get dproof 'assertion) (not (get dproof 'plans)))
				      (setq fail-string nil))
				(cleanup)
				(remove-unnecessary-gaps))
			  (when (and record (not fail-string))
				(when (module-loaded-p 'library)
				      (let ((comment (concatenate 'string "Running in batch mode, using mode " 
								  (princ-to-string mode) " with searchlist " 
								  (princ-to-string slist))))
					(if (locate-item problem :type 'gwff :writeable t :multiple nil)
					    (daterec problem 'gwff comment t nil)
					  (progn
					    (let ((item (make-libitem
							 :name problem :type 'gwff :description (get problem 'assertion)
							 :mhelp "" :context nil
							 :other-attributes nil
							 :provability nil
							 :proof-date nil
							 :needed-objects nil
							 :other-remarks ""
							 :file (concatenate 'string (car default-lib-dir)
									    "tps-test-output.lib"))))
					      (store-item item))
					    (daterec problem 'gwff comment t nil)))
					(do ((modename auto::*test-mode-name* 
						       (intern (concatenate 'string (princ-to-string modename) "A") 
							       (find-package 'user)))
					     (success nil))
					    (success)
					    (export modename)
					    (unless (or (locate-item modename :type 'mode :writeable t :multiple nil)
							(locate-item modename :type 'mode1 :writeable t :multiple nil))
					;i.e. "if this name doesn't already exist"...
						    (setq success t)
						    (let ((item (make-libitem
								 :name modename :type 'mode1 
								 :description (get auto::*test-mode-name* 'flag-settings)
								 :mhelp ""
								 :context nil
								 :provability nil
								 :proof-date nil
								 :other-attributes nil
								 :needed-objects nil
								 :other-remarks comment
								 :file (concatenate 'string (car default-lib-dir)
										    "tps-test-output.lib"))))
						      (store-item item))))))
				(saveproof (string-downcase dproof)))
;			  (reroute-output-append "tps-batch-jobs" *default-pathname-defaults*
;						 (msgf "Ending run of problem " problem " in mode " mode 
;						       t "   with searchlist " slist)
;						 (when filename (msg " with output to " filename))
;						 (when fail-string (msgf "  " fail-string))
;						 (msgf "   on " (stringdt nil) t t t))
			  )))))

#+(or (and :excl :vax) (and :allegro :andrew))
(defun set-up-homedir-and-user ()
  (let ((user (cadr (member "-user" (sys:command-line-arguments)
			    :test #'string=)))
	(homedir (cadr (member "-homedir" (sys:command-line-arguments)
                               :test #'string=))))
    (unless (and user homedir)
      (format t "Can't determine user's name and home directory. Exiting.~%")
      (lisp-exit))
    (setq user (intern user))
    (when (boundp '*redefinition-warnings*)
      (setq *redefinition-warnings* nil)) ;stifle the ACL warnings
    (defun status-userid ()
      user)
    (defun user-homedir-pathname ()
      (make-pathname :directory homedir))))

#+(and :excl :linux86)
(defun fix-machine-instance ()
  (when (probe-file "/tmp/machine-instance-file") (delete-file "/tmp/machine-instance-file"))
  (excl:run-shell-command "hostname --long >> /tmp/machine-instance-file" :wait t)
  (setq machine-instance 
	(with-open-file (ifile "/tmp/machine-instance-file" :direction :input)
			(read-line ifile)))
  (when (probe-file "/tmp/machine-instance-file") (delete-file "/tmp/machine-instance-file")))

(defun initialize-omega (pathname)
  (setq *omega-switch* t)
  (setq *omega-proof* pathname)
  (unless (equal dproof '*****) ;;in which case we haven't started a proof yet
    (setq *omega-proofname* dproof)))

(definfo command-line-switches
  (mhelp "Several switches can be given on the command line when
TPS is started up. They are as follows:

-grader starts TPS in the GRADER top level.
-batch <file1> will execute the work file <filename>.work and 
               then quit TPS.
-service <name> <in> <out> will start a TPS with identifier <name>
               looking for requests from <in> and sending output to <out>.
               This gives a general way for external programs to ask
               TPS to prove a thm and receive the proof.
-lservice <portnum>
               Similar to -service, but assumes there is a listener
               on the machine at port <portnum>.  TPS connects to this
               and uses the socket to take requests and send output.
-server <tps-image-file> <etps-image-file> [-logdir <directory for log files>] [-port <portnum>]
               This starts TPS or ETPS as a web server.  Browsers
               can connect via http://<machine-name>:<portnum>
               where the default <portnum> is 29090 (but another can
               be explicitly given).  Once a browser connects to this
               TPS server, the client can start a new TPS or ETPS image
               (assuming the client has access rights, see SETUP-ONLINE-ACCESS).
               The server can also send html files to the client.
-remoteuser <userid> <portnum>
               This starts TPS or ETPS for a remote user.  This option
               is used when TPS or ETPS is started by a running TPS server.
               It should rarely (or never) be used when TPS is started directly.
               <portnum> is the port number of a passive socket waiting for a 
               connection.  Once TPS or ETPS starts for a remoteuser, it connects
               to this socket and sends it the port number of a new passive
               socket that the client can use to connect to this TPS or ETPS.
-javainterface <java command> [-other <java args>]
               This command line switch tells TPS to start a java
               interface from which it will receive input and to which
               it will send output.  The arguments after -javainterface
               and (possibly) before a -other switch indicate how to start
               the java interface.  For example, java TpsWin.
               This will be appended to the name of the machine and a port number
               (determined at runtime).
               If there is a -other switch, then the arguments after this will be 
               appended after the port number.
-omega will prevent -batch from quitting TPS
-outfile <file2.prf>, in the presence of -omega and -batch, runs the 
                  work file <filename1>.work and then remains in
                  TPS. When the user exits, <file2.prf> will be 
                  written, containing the current version of the 
                  dproof created by the work file. A file <file2.prt>
                  will also be written. Note that the given filename
                  filename MUST end with .prf
-outfile <file2>, in the presence of -batch alone, sends a script
                  of the entire session to <file2>.
-problem -mode -slist belong together; they will execute the given problem
                  using the given mode and searchlist.
              
Examples:

tps3 -- -batch thm266 
  runs thm266.work through tps3, showing the output on the terminal.
tps3 -- -batch thm266 -outfile thm266.script
  does the same but directs the output to thm266.script.
tps3 -- -omega -batch thm266 -outfile thm266.prf
  starts TPS, runs thm266.work and then enters the TPS command-line 
  interface. When the user exits, it writes the current proof into
  the file thm266.prf
tps3 -- -batch thm266 -outfile /dev/null
  does the same but discards the output.

Notice that the \"--\" is required for allegro lisp, but not for cmucl,
where the equivalent commands are of the form:   tps3cmu -batch thm266
"))
; Each of these also writes entries to tps-batch-jobs as follows:
; Attempting to run work file thm266.work
;    on Monday, August 25, 1997 at 17:01:00.
; Ending run of work file thm266.work
;    on Monday, August 25, 1997 at 17:12:00."))
  
  
