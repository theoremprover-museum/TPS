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

;;;
;;; File: MACSYS-3
;;;
;;; Some misc. collection of system stuff belonging to the BARE package.
;;;

(deffile macsys
  (part-of bare)
  (extension clisp)
  (mhelp "Miscellaneous system functions for the Bare package."))

(context otl-entering)

(defun terminalp (filename)
  (and (stringp filename) (string= filename "TTY:")))

(defun fileprinc (filename)
  (cond ((terminalp filename) (prin1 "TTY")) ;;CK this
	((and (stringp filename) (string= filename "")) (prin1 ""))
	((probe-file filename) (prin1 (namestring (truename filename))))
	(t (prin1 (namestring filename)))))

(defun getfilespec (filespec)
  (cond ((and (symbolp filespec) (string-equal (symbol-name filespec) "TTY"))
	 "TTY:")
	((or (pathnamep filespec) (stringp filespec))
	 filespec)
	((symbolp filespec)
	 (string-downcase (symbol-name filespec)))
	(t (throwfail filespec " cannot be the name of a file."))))

(defun filespecp (filespec)
  (or (symbolp filespec) (pathnamep filespec) (stringp filespec)))


;;; DIRSPEC's are most commonly used as :directory keyword arguments
;;; to MAKE-PATHNAME.  Unfortunately, each Lisp has a different
;;; requirement for such arguments.  Thus, rather than trying to force
;;; the user to know the requirements of his/her own Lisp, we will
;;; make each variable of type DIRSPEC a string.  We will presume that
;;; the string the user gives is indeed a valid directory, except that
;;; in Mach/Unix systems we will check to see that it ends with a "/",
;;; and add a "/" if necessary. DAN 17MAY89

(defun getdirspec (str) 
     (if (stringp str) 
         #+(or mach unix)
              (if (plusp (length str))
                  (if (char= (char str (1- (length str))) #\/) 
                      str 
                      (concatenate 'string str "/"))
                  str)
         #-(or mach unix) 
           str 
         (throwfail str " is not in the form of a string.")))

;;; Returns t if getdirspec succeeds.  getdirspec will throwfail otherwise.
(defun dirspecp (str)
  (getdirspec str)
  t)

(defmexpr exit
  (mainfns userexit)
  (dont-restore t)
  (mhelp "Exit from TPS."))


(defvar save-file-closed-on-exit nil)
(defvar first-entry t)  ;; true if user is entering for first time
(defvar script-files nil)

(defun userexit ()
  (declare (special *prfw-pall-window* *prfw-^p-window* *prfw-^pn-window*
		    *prfw-pall-process* *prfw-^p-process* *prfw-^pn-process*
		    *omega-switch* *omega-proofname* *omega-proof* *using-interface*
		    allscopeflag printtypes printtypes-all infix-notation))
  (if (fboundp 'closerecord) (%catch% (closerecord) (fail nil)))
  (when (and (boundp 'saving-work-p) saving-work-p)
    (stop-save)
    (setq save-file-closed-on-exit T))
  (if (fboundp 'write-all-events) (write-all-events))
  ;; Added to close all script files. DAN 11-4-89 
  ;; Note that the defun will make linereadpp useless, so really have
  ;; to exit.
  (if *using-interface*
      (progn
	(close-window *prfw-pall-window*)
	(close-window *prfw-^p-window*)
	(close-window *prfw-^pn-window*))
    (progn
      (when (streamp *prfw-pall-window*) (close *prfw-pall-window*) 
	    (if (probe-file (pathname *prfw-pall-window*)) (delete-file (pathname *prfw-pall-window*))))
      (when (streamp *prfw-^p-window*) (close *prfw-^p-window*)  
	    (if (probe-file (pathname *prfw-^p-window*)) (delete-file (pathname *prfw-^p-window*))))
      (when (streamp *prfw-^pn-window*) (close *prfw-^pn-window*)  
	    (if (probe-file (pathname *prfw-^pn-window*)) (delete-file (pathname *prfw-^pn-window*))))
      (if *prfw-pall-window* (setq *prfw-pall-window* (kill-xterm-window *prfw-pall-process* )))
      (if *prfw-^pn-window* (setq *prfw-^pn-window* (kill-xterm-window *prfw-^pn-process* )))
      (if *prfw-^p-window* (setq *prfw-^p-window* (kill-xterm-window *prfw-^p-process* )))))
  (if (member (find-package 'AUTO) (package-use-list *package*))
      (auto::close-matevpw))
  (kill-edwin-windows)
  (when (and (boundp 'script-files) script-files)
    (defun linereadpp (&rest ignore) (declare (ignore ignore)) (list 'exit))
    (unscript)
    )
  (when *omega-switch* ;then we were called by omega, and should tidy up for it.
    (when *omega-proofname* (setq dproof *omega-proofname*)) ;in case we're looking at the wrong proof
    (unless (equal dproof '*****) ;we never did start a proof!
      (let ((prtname (make-pathname% :directory (pathname-directory *omega-proof*)
				     :name (pathname-name 
					    (translate-pathname (pathname-name *omega-proof*) "*.prf" "*.prt")))))
	(printproof (namestring prtname))
	(setq allscopeflag t printtypes t printtypes-all t infix-notation nil)
	(saveproof *omega-proof*))))
  (setq first-entry nil)  ;; Used by Secondary-top-main
                          ;; added so that we can distinguish between
                          ;; first entering and reentering 9/4/87 DAN
  (exit-from-lisp))

(defmexpr news
  (print-command t)
  (mhelp "Type TPS news on the terminal."))

(defun news ()
  (declare (special news-dir core-name))
  (let ((news-file (make-pathname% :directory news-dir :name core-name
				  :type "news")))
    (cond ((probe-file news-file) (filetype news-file))
	  (t (msgf "No news available.")))))


(defun curpos () 0)

(defun setcurpos (n) n)

(defmacro dont-count (&rest forms)
  `(let ((old-curpos (curpos)))
     (setcurpos 0)
     (prog1 ,@forms (setcurpos old-curpos))))

(defconstnt month-format-string
  "~[?~;January~;February~;March~;April~;May~;June~;July~;August~;September~;~
  October~;November~;December~]")

(defconstnt day-of-week-format-string
  "~[Monday~;Tuesday~;Wednesday~;Thursday~;Friday~;Saturday~;Sunday~]")

(defutil stringdt
  (form-type function)
  (keywords printing date)
  (mhelp "(STRINGDT) prints out the date and time to the current output stream
(usually the terminal), and then returns NIL. (STRINGDT stream) directs
the output to some other stream, and (STRINGDT nil) prints nothing and 
returns a string containing the date and time."))

(defun stringdt (&optional (output-stream *standard-output*))
  (multiple-value-bind (second minute hour date month year
			       day-of-week daylight-saving-time-p time-zone)
		       (get-decoded-time)
    (declare (ignore daylight-saving-time-p time-zone))
    (format output-stream "~?, ~? ~2D, ~D at ~2D:~2,'0D:~2,'0D."
	    day-of-week-format-string
	    #+spice day-of-week #-spice (list day-of-week)
	    month-format-string #+spice month #-spice (list month)
	    date year hour minute second)))

(defutil stringdtl
  (form-type function)
  (keywords printing date)
  (mhelp "(STRINGDTL) prints out a newline followed by the date and time to 
the current output stream (usually the terminal), and then returns NIL. 
(STRINGDTL stream) directs the output to some other stream, and 
(STRINGDTL nil) prints nothing and returns a string containing a newline 
followed by the date and time."))

(defun stringdtl (&optional (output-stream *standard-output*))
  (multiple-value-bind (second minute hour date month year
			       day-of-week daylight-saving-time-p time-zone)
		       (get-decoded-time)
    (declare (ignore daylight-saving-time-p time-zone))
    (format output-stream "~%~?, ~? ~2D, ~D at ~2D:~2,'0D:~2,'0D.~%"
	    day-of-week-format-string
	    #+spice day-of-week #-spice (list day-of-week)
	    month-format-string #+spice month #-spice (list month)
	    date year hour minute second)))

;;; The terminal stuff which follows is obsolete in Common Lisp.
;;; Eventually we may have to update it and make it available again,
;;; but the longer we can get by without it, the better.


(defun cmd-completion ()
  (dolist (toplevel (eval (get 'toplevel 'global-list)))
    (when (symbolp toplevel) (initialize-top-level-ctree toplevel))))


(defun initialize-top-command-ctree ()
  (initialize-top-level-ctree 'cmd-top))

(defun initialize-top-level-ctree (top-level)
  (set (get top-level 'top-level-ctree)
       (create-sorted-ctree
	(remove-if-not
	 #'symbolp (eval (get (get top-level 'top-level-category)
			      'global-list))))))

(defun create-sorted-ctree (wordlist)
  (create-ctree (sort wordlist #'(lambda (x y) (alphalessp y x)))))


;;; if init-dialogue is T, the function init-dialogue-fn will be called
;;; with no arguments for after everything else in the init phase
;;; has been taken care of.

;;; Whether or not to give a message when closing a rerouted file

(defvar reroute-close-message t)

(defmacro reroute-output-append (filename default &rest forms)
  `(let ((existent-file (probe-file (merge-pathnames ,filename ,default)))
	 written-file)
     (with-open-file
      (*standard-output* (merge-pathnames ,filename ,default)
			 :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
      (setq written-file (namestring (pathname *standard-output*)))
      ,@forms)
     (when reroute-close-message
       (if existent-file
	   (msg f "Appended to file " written-file ".")
	   (msg f "Written new file " written-file ".")))))

(defmacro reroute-output (filename default &rest forms)
  `(let (written-file)    
     (with-open-file
      (*standard-output* (merge-pathnames ,filename ,default)
			 :direction :output
                         :if-exists :supersede
			 :if-does-not-exist :create)
      (setq written-file (namestring (pathname *standard-output*)))
      ,@forms)
     (when reroute-close-message
	   (msg F "Written file " written-file "."))))

(defutil reroute-output
  (form-type macro)
  (keywords output)
  (mhelp "REROUTE-OUTPUT is the canonical way of routing output of TPS
to a file exclusively.  (REROUTE-OUTPUT filename default form1 ... formn)
will open a file filename using default for figuring out parts of filename
which were not specified.  It then executes form1 ... formn such that
all output goes to filename.  Note that you can still send messages to the
terminal with COMPLAIN or TTYMSG, but MSG output will go to filename.
When the writing is completed, a message with the true filename will
be printed. If you want to suppress this message, set REROUTE-CLOSE-MESSAGE
to NIL.  Please think about the defaults, but if you want to use
the (most likely wrong) CLISP default, just use the global variable
*default-pathname-defaults*."))

(defutil reroute-output-append
  (form-type macro)
  (keywords output)
  (mhelp "REROUTE-OUTPUT-APPEND is like REROUTE-OUTPUT, but appends to the end
of the file rather than superseding it, if it already exists."))

;;; For now do not %catch% errors, so they are recorded in a file
;;; if the event of an error is enabled.

(defmacro err-protect (%catch%-form error-form &rest forms)
  (declare (ignore error-form))
  `(%catch% (progn ,@forms)
	  (t ,%catch%-form)))

(defvar gctime0 0)

(defvar runtime0 0)

(defvar time0 0)


;;;
;;; Next function is called from an mexpr in FILSYS

(defun filetype (filespec)
  (with-open-file (input-stream filespec
				:direction :input
				;CMUCL doesn't like this ---   :element-type :default
				#-tops-20 :if-does-not-exist #-tops-20 nil)
    (fresh-line)
    (do ((char (read-char input-stream nil nil)
	       (read-char input-stream nil nil)))
	((not char))
      (write-char char))))

(defun copy-file (ofile nfile)
  "Copies ofile to nfile."
  (handler-case ; cebrown 10/2/02
   (with-open-file (*input-stream* ofile :direction :input)
		   (with-open-file (*output-stream* nfile :direction :output
						    :if-exists :error)
				   (do ((item (read *input-stream* nil control-d-char)
					      (read *input-stream* nil control-d-char)))
				       ((eq item control-d-char))
				     (write item :stream *output-stream*))))
   (error (condition)
	  (throwfail "Copy File failed." t condition))))

