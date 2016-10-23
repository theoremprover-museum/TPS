; This Lisp file can be used to build a version of TPS or ETPS.
; Under Windows, this is used as a substitute for the Makefile.
; To compile and build a version of TPS (and/or ETPS), start Lisp and call
; (load "<pathname>\\make-tps-windows.lisp")

; This file first prompts the user for path information
; and creates a file called tps3.sys (etps.sys).

; If you wish to set up TPS or ETPS in a particular way (with a special value
; for some TPS flag, say), then use the tps3.ini and etps.ini files for that
; purpose.  For example, we have (setq expertflag t) in our tps3.ini file,
; but (setq expertflag nil) in the etps.ini file. 

; We assume that you have set up your directory structure as it comes in the 
; TPS distribution.  Changing that would spell disaster here.

;-----------------------------------------------------------------------
; PARAMETERS YOU MIGHT WANT TO EDIT ARE GIVEN BELOW
;-----------------------------------------------------------------------

; You may want to change the default value of sys-dir here.
(setq sys-dir "C:\\Program\ Files\\TPS\\")

; The following is a list of those people who have EXPERTFLAG set to T.
; These people are allowed to evaluate any Lisp form, and may have a 
; tps3.ini file loaded on startup.
(setq experts '("ABSOLUTE" "andrews" "pfenning" "pa01" "cebrown" "local1"))

; You may want to enter the default value of java-dir 
; This should be the bin directory containing java and javac,
; but can be left empty if this bin directory is in the
; PATH environment variable.  In Windows XP, the PATH environment 
; variable can be changed by opening the Control Panel, then System, 
; then choosing Advanced and Environment Variables.
(setq java-dir "")

(format t "DISCLAIMER: make-tps-windows.lisp was written to build TPS/ETPS using Allegro Lisp~%")
(format t "under MS Windows.  It may or may not work in other contexts.~2%")

(defun make-prompt (&optional (msg "") (def "") possvals)
  (format t "~d [~d]>" msg def)
  (do ((resp (read-line) (read-line)))
      ((or (null possvals)
	   (equal resp "")
	   (member resp possvals :test #'equal))
       (let ((i (length resp)))
	 (when (and (> i 1)
		    (equal (aref resp 0) #\")
		    (equal (aref resp (- i 1)) #\"))
	   (setq resp (read-from-string resp))))
       (if (equal resp "")
	   def
	 resp))
      (format t "~d is not a legal response.~%" resp)
      (format t "~d [~d]>" msg def)))

(defun make-prompt-bool (&optional (msg "") (def "Yes"))
  (let ((a (make-prompt msg def '("Yes" "yes" "YES" "y" "Y" 
				  "T" "t" "No" "N" "n" "NO" "no"
				  "NIL" "nil" "Nil"))))
    (member a '("Yes" "yes" "YES" "y" "Y" "T" "t") :test #'equal)))

(defun make-prompt-nat (&optional (msg "") (def 0))
  (format t "~d [~d]>" msg def)
  (do ((resp (read-line) (read-line)))
      ((or (equal resp "")
	   (let ((respnat (read-from-string resp)))
	     (and (integerp respnat)
		  (>= respnat 0))))
       (if (equal resp "")
	   def
	 (read-from-string resp)))
      (format t "~d is not a legal response.~%" resp)
      (format t "~d [~d]>" msg def)))

(defun add-last-backslash (d)
  (let ((i (length d)))
    (if (and (> i 0) (not (equal (aref d (- i 1)) #\\)))
	(format nil "~d\\" d)
      d)))

(setq args
      #+:cmu extensions:*command-line-strings*
      #+(or :allegro :excl)(sys:command-line-arguments)
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
		 (push (system:argv i) args))))

(when args
  (setq lisp-exe (car args)))

(unless lisp-exe
  (format t "Please indicate how to call Lisp.~%")
  (setq lisp-exe (make-prompt "Lisp Executable ")))

(format t "~%Please indicate the main TPS directory (where make-tps-windows.lisp is found).~%")
(format t "Do not put quotes around the path.~%")
(format t "(Press Enter to accept the default value which is inside the [ ].)~%")
(setq sys-dir (add-last-backslash (make-prompt "TPS directory" sys-dir)))

(terpri)
#+:allegro
(if (make-prompt-bool "Do you have a Java compiler and executable?")
    (progn
      (setq has-java T)
      (format t "Enter the directory with the java executable \"java\" and compiler \"javac\"~%")
      (format t "If the bin directory for Java is in the PATH environment variable,~%")
      (format t "this can be left blank.  Otherwise, either put the full path or add~%")
      (format t "the Java bin directory to the PATH environment variable by opening the~%")
      (format t "Control Panel, then System, and choose Advanced and Environment Variables.~%")
      (setq java-dir (add-last-backslash (make-prompt "Java bin directory" java-dir))))
  (progn
    (setq has-java NIL)
    (format t "If you want to compile and use the Java interface for TPS and ETPS,~%")
    (format t "you can download Java SDK (with compiler) from http://java.sun.com~%")))

(terpri)

(let ((tps3-sys (open (format nil "~dtps3.sys" sys-dir) :direction :output
		      :if-exists :supersede :if-does-not-exist :create))
      (etps-sys (open (format nil "~detps.sys" sys-dir) :direction :output
		      :if-exists :supersede :if-does-not-exist :create)))
  (format tps3-sys "(in-package :cl-user)~%")
  (format etps-sys "(in-package :cl-user)~%")
  (format tps3-sys "(defvar core-name \"tps3\")~%")
  (format etps-sys "(defvar core-name \"etps\")~%")
  (format tps3-sys "(defvar sys-dir ~S)~%" sys-dir)
  (format etps-sys "(defvar sys-dir ~S)~%" sys-dir)
  (format tps3-sys "(defvar save-file ~%")
  (format etps-sys "(defvar save-file ~%")
  (format tps3-sys "  (if (member :allegro *features*)~%")
  (format etps-sys "  (if (member :allegro *features*)~%")
  (format tps3-sys "      ~S~%" 
	  (format nil "~dtps3.dxl" sys-dir))
  (format etps-sys "      ~S~%"
	  (format nil "~detps.dxl" sys-dir))
  (format tps3-sys "    ~S))~%" 
	  (format nil "~dtps3" sys-dir))
  (format etps-sys "    ~S))~%"
	  (format nil "~detps" sys-dir))
  (format tps3-sys "\#+:allegro(defvar java-comm ~S)~%"
	  (format nil "~djava -classpath \"~djava\" TpsStart" java-dir sys-dir))
  (format etps-sys "\#+:allegro(defvar java-comm ~S)~%"
	  (format nil "~djava -classpath \"~djava\" TpsStart" java-dir sys-dir))
  (format tps3-sys "\#-:allegro(defvar java-comm \"\")~%")
  (format etps-sys "\#-:allegro(defvar java-comm \"\")~%")
  (format tps3-sys "(defvar compiled-extension (or (if (member :lucid *features*)~%")
  (format etps-sys "(defvar compiled-extension (or (if (member :lucid *features*)~%")
  (format tps3-sys "                                   \"lbin\")~%")
  (format etps-sys "                                   \"lbin\")~%")
  (format tps3-sys "                               (if (or (member (find-symbol \"KCL\")~%")
  (format etps-sys "                               (if (or (member (find-symbol \"KCL\")~%")
  (format tps3-sys "                                               *features*)~%")
  (format etps-sys "                                               *features*)~%")
  (format tps3-sys "                                       (member (find-symbol \"IBCL\")~%")
  (format etps-sys "                                       (member (find-symbol \"IBCL\")~%")
  (format tps3-sys "                                               *features*)~%")
  (format etps-sys "                                               *features*)~%")
  (format tps3-sys "                                       )~%")
  (format etps-sys "                                       )~%")
  (format tps3-sys "                                 \"o\")~%")
  (format etps-sys "                                 \"o\")~%")
  (format tps3-sys "                             (if (member :cmu *features*)~%")
  (format etps-sys "                             (if (member :cmu *features*)~%")
  (format tps3-sys "                                 \"fasl\")~%")
  (format etps-sys "                                 \"fasl\")~%")
  (format tps3-sys "                             (if (member :excl *features*)~%")
  (format etps-sys "                             (if (member :excl *features*)~%")
  (format tps3-sys "                                 \"fasl\")~%")
  (format etps-sys "                                 \"fasl\")~%")
  (format tps3-sys "                             (if (member :allegro *features*)~%")
  (format etps-sys "                             (if (member :allegro *features*)~%")
  (format tps3-sys "                                 \"fasl\")~%")
  (format etps-sys "                                 \"fasl\")~%")
  (format tps3-sys "                             \"fasl\")~%")
  (format etps-sys "                             \"fasl\")~%")
  (format tps3-sys "  )~2%")
  (format etps-sys "  )~2%")
  (format tps3-sys "(defvar mail-remarks nil)~%")
  (format etps-sys "(defvar mail-remarks nil)~%")
  (format tps3-sys "(defvar remarks-file \"tps3.remarks\")~%")
  (format etps-sys "(defvar remarks-file \"etps.remarks\")~%")
  (format tps3-sys "(defvar news-dir ~S)~%" sys-dir)
  (format etps-sys "(defvar news-dir ~S)~%" sys-dir)
  (format tps3-sys "(defvar patch-file-dir ~S)~%" sys-dir)
  (format etps-sys "(defvar patch-file-dir ~S)~%" sys-dir)
  (format tps3-sys "(defvar ini-file-dir ~S)~%" sys-dir)
  (format etps-sys "(defvar ini-file-dir ~S)~%" sys-dir)
  (format tps3-sys "(defvar compiled-dir  ~S)~%"
	  (format nil "~dbin\\" sys-dir))
  (format etps-sys "(defvar compiled-dir  ~S)~%"
	  (format nil "~dbin\\" sys-dir))
  (format tps3-sys "(defconstant expert-list '~S)~%" experts)
  (format etps-sys "(defconstant expert-list '~S)~%" experts)
  (multiple-value-bind
   (sec min hour date month year day daylight time-zone)
   (get-decoded-time)
   (format tps3-sys "(defconstant building-start-time \"~d ~d ~d, ~d at ~2,'0D:~2,'0D:~2,'0D\")~%"
	   (nth day '("Mon" "Tues" "Wed" "Thurs" "Fri" "Sat" "Sun"))
	   (nth month '("" "Jan" "Feb" "Mar" "Apr" "May" "June" 
			"July" "Aug" "Sept" "Oct" "Nov" "Dec"))
	   date year hour min sec)
   (format etps-sys "(defconstant building-start-time \"~d ~d ~d, ~d at ~2,'0D:~2,'0D:~2,'0D\")~%"
	   (nth day '("Mon" "Tues" "Wed" "Thurs" "Fri" "Sat" "Sun"))
	   (nth month '("" "Jan" "Feb" "Mar" "Apr" "May" "June" 
			"July" "Aug" "Sept" "Oct" "Nov" "Dec"))
	   date year hour min sec))
  (format tps3-sys "(defvar source-dir ~S)~%" 
	  (format nil "~dlisp\\" sys-dir))
  (format etps-sys "(defvar source-dir ~S)~%" 
	  (format nil "~dlisp\\" sys-dir))
  (format tps3-sys "(defvar source-path (list compiled-dir source-dir))~%")
  (format etps-sys "(defvar source-path (list compiled-dir source-dir))~%")
  (format tps3-sys "(defvar source-extension \"lisp\")~%")
  (format etps-sys "(defvar source-extension \"lisp\")~%")
  (format tps3-sys "(pushnew :TPS *features*)~2%")
  (format etps-sys "(pushnew :ETPS *features*)~2%")
  
  (format tps3-sys "\#+lucid (progn (push compiled-extension *load-binary-pathname-types*)~%")
  (format etps-sys "\#+lucid (progn (push compiled-extension *load-binary-pathname-types*)~%")
  (format tps3-sys "                (push source-extension *load-source-pathname-types*)~%")
  (format etps-sys "                (push source-extension *load-source-pathname-types*)~%")
  (format tps3-sys "                (setq *ignore-binary-dependencies* t)~%")
  (format etps-sys "                (setq *ignore-binary-dependencies* t)~%")
  (format tps3-sys "                (proclaim '(optimize (compilation-speed 1)~%")
  (format etps-sys "                (proclaim '(optimize (compilation-speed 1)~%")
  (format tps3-sys "                                     (speed 3)~%")
  (format etps-sys "                                     (speed 3)~%")
  (format tps3-sys "                                     (safety 3))))~2%")
  (format etps-sys "                                     (safety 3))))~2%")
  
  (format tps3-sys ";;; Preambles used when printing proofs to a file in scribe or tex format~%")
  (format etps-sys ";;; Preambles used when printing proofs to a file in scribe or tex format~%")
  (format tps3-sys "(setq *print-case* :upcase)~%")
  (format etps-sys "(setq *print-case* :upcase)~%")
  (format tps3-sys "\#+ibcl(setq si::*default-time-zone* 5)~%")
  (format etps-sys "\#+ibcl(setq si::*default-time-zone* 5)~%")
  (format tps3-sys "\#+ibcl(setq si::*notify-gbc* t)~%")
  (format etps-sys "\#+ibcl(setq si::*notify-gbc* t)~%")
  (format tps3-sys "\#+ibcl(setq si::*ignore-eof-on-terminal-io* t)~%")
  (format etps-sys "\#+ibcl(setq si::*ignore-eof-on-terminal-io* t)~%")
  (format tps3-sys "\#+:excl(setq *gcprint* nil)~%")
  (format etps-sys "\#+:excl(setq *gcprint* nil)~%")
  (format tps3-sys "\#+:allegro(setq excl:*global-gc-behavior* :auto)~%")
  (format etps-sys "\#+:allegro(setq excl:*global-gc-behavior* :auto)~%")
  (close tps3-sys)
  (close etps-sys))

(format t "Written tps3.sys and etps.sys~2%")

(let ((tps3-bat (open (format nil "~dtps3.bat" sys-dir) :direction :output
		      :if-exists :supersede :if-does-not-exist :create)))
  (format tps3-bat "@echo off~%")
  (format tps3-bat "call \"~d\" -I \"~dtps3.dxl\"~%"
	  lisp-exe sys-dir)
  (close tps3-bat))

(let ((etps-bat (open (format nil "~detps.bat" sys-dir) :direction :output
		      :if-exists :supersede :if-does-not-exist :create)))
  (format etps-bat "@echo off~%")
  (format etps-bat "call \"~d\" -I \"~detps.dxl\"~%"
	  lisp-exe sys-dir)
  (close etps-bat))

(format t "Written executable batch files tps3.bat and etps.bat~%")
(format t "Double click on these to start tps or etps after the images are built.~2%")

#-:allegro(format t "*** Not Allegro *** Cannot Continue.~%To compile and build by hand, try the following:~%1) Make sure ~dbin\\ is empty.~%2) Start a new lisp.~%3) Load tps-compile-windows.lisp~%4) Exit Lisp.~%5) Start a new Lisp.~%6) Load tps-build-windows.lisp~%7) Call (tps3-save)~%8)  Do steps (1-7) again using etps-compile-windows.lisp and etps-build-windows.lisp~%9)  Edit tps3.bat and etps.bat appropriately for your Lisp."
	sys-dir)

#+:allegro
(let (compile-tps build-tps compile-etps build-etps compile-java build-java-bat
		  (bottomOffset 0) (rightOffset 0) (screenx 700) (screeny 500))
  (setq compile-tps (make-prompt-bool "Compile TPS Files?"))
  (setq build-tps (make-prompt-bool "Build TPS?"))
  (setq compile-etps (make-prompt-bool "Compile ETPS Files?"))
  (setq build-etps (make-prompt-bool "Build ETPS?"))
  (if has-java
      (setq compile-java (make-prompt-bool "Compile Java Files?"))
    (setq compile-java nil))
  (if has-java
      (setq build-java-bat (make-prompt-bool "Build Java Batch Files?"))
    (setq build-java-bat nil))
  (when build-java-bat
    (format t "~%Please enter integer values for the following parameters.~%")
    (format t "If unsure, accept the default values and rebuild the java batch files again later if~%")
    (format t "you decide you need to change the values.~2%")
    (format t "screenx : Initial horizontal size of the Java TPS display.~%")
    (format t "screeny : Initial vertical size of the Java TPS display.~%")
    (format t "bottomOffset : Extra space at the bottom of the Java TPS display (for the input line).~%")
    (format t "rightOffset : Extra space at the right of the Java TPS display (for the scroolbar).~2%")
    (setq screenx (make-prompt-nat "Initial horizontal size?" screenx))
    (setq screeny (make-prompt-nat "Initial vertical size?" screeny))
    (setq bottomOffset (make-prompt-nat "Bottom Offset for Java Window?" bottomOffset))
    (setq rightOffset (make-prompt-nat "Right Offset for Java Window?" rightOffset)))
  (when compile-tps
    (format t "Deleting files in ~dbin~%"
	    sys-dir)
    (dolist (f (directory (format nil "~dbin\\" sys-dir)))
	    (delete-file f))
    (format t "Compiling TPS files.~%")
    (excl:run-shell-command (format nil "~d +s \"~dtps-compile-windows.lisp\" -- \"~d\""
				    lisp-exe sys-dir sys-dir)))
  (when build-tps
    (format t "Building TPS.~%")
    (excl:run-shell-command (format nil "~d +s \"~dtps-build-windows.lisp\" -- \"~d\""
				    lisp-exe sys-dir sys-dir))
    (format t "You can now run tps by double clicking on the tps3 batch file in ~d.~%"
	    sys-dir))
  (when compile-etps
    (format t "Deleting files in ~dbin~%"
	    sys-dir)
    (dolist (f (directory (format nil "~dbin\\" sys-dir)))
	    (delete-file f))
    (format t "Compiling ETPS files.~%")
    (excl:run-shell-command (format nil "~d +s \"~detps-compile-windows.lisp\" -- \"~d\""
				    lisp-exe sys-dir sys-dir)))
  (when build-etps
    (format t "Building ETPS.~%")
    (excl:run-shell-command (format nil "~d +s \"~detps-build-windows.lisp\" -- \"~d\""
				    lisp-exe sys-dir sys-dir))
    (format t "You can now run etps by double clicking on the etps batch file in ~d.~%"
	    sys-dir))
  (when compile-java
    (let* ((javac-exe (format nil "~djavac" java-dir)))
      (format t "Compiling Java files.~%")
      (excl:run-shell-command 
       (format nil "~d -verbose -classpath \"~djava\" -d \"~djava\" \"~djava\\TpsStart.java\""
	       javac-exe sys-dir sys-dir sys-dir))))
  (when build-java-bat
    (let ((java-bat (open (format nil "~dtps3-java.bat" sys-dir) :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (format java-bat "@echo off~%")
      (format java-bat "call \"~d\" -I \"~dtps3.dxl\" -- -javainterface ~djava -classpath \"~djava\" TpsStart -other -nopopups -screenx ~d -screeny ~d -bottomOffset ~d -rightOffset ~d~%"
	      lisp-exe sys-dir java-dir sys-dir screenx screeny bottomOffset rightOffset)
      (close java-bat))
    (let ((java-bat (open (format nil "~dtps3-java-big.bat" sys-dir) :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (format java-bat "@echo off~%")
      (format java-bat "call \"~d\" -I \"~dtps3.dxl\" -- -javainterface ~djava -classpath \"~djava\" TpsStart -other -big -nopopups -screenx ~d -screeny ~d -bottomOffset ~d -rightOffset ~d~%"
	      lisp-exe sys-dir java-dir sys-dir screenx screeny bottomOffset rightOffset)
      (close java-bat))
    (let ((java-bat (open (format nil "~dtps3-java-popups.bat" sys-dir) :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (format java-bat "@echo off~%")
      (format java-bat "call \"~d\" -I \"~dtps3.dxl\" -- -javainterface ~djava -classpath \"~djava\" TpsStart -other -screenx ~d -screeny ~d -bottomOffset ~d -rightOffset ~d~%"
	      lisp-exe sys-dir java-dir sys-dir screenx screeny bottomOffset rightOffset)
      (close java-bat))
    (let ((java-bat (open (format nil "~dtps3-java-big-popups.bat" sys-dir) :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (format java-bat "@echo off~%")
      (format java-bat "call \"~d\" -I \"~dtps3.dxl\" -- -javainterface ~djava -classpath \"~djava\" TpsStart -other -big -screenx ~d -screeny ~d -bottomOffset ~d -rightOffset ~d~%"
	      lisp-exe sys-dir java-dir sys-dir screenx screeny bottomOffset rightOffset)
      (close java-bat))
    (let ((java-bat (open (format nil "~detps-java.bat" sys-dir) :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (format java-bat "@echo off~%")
      (format java-bat "call \"~d\" -I \"~detps.dxl\" -- -javainterface ~djava -classpath \"~djava\" TpsStart -other -nopopups -screenx ~d -screeny ~d -bottomOffset ~d -rightOffset ~d~%"
	      lisp-exe sys-dir java-dir sys-dir screenx screeny bottomOffset rightOffset)
      (close java-bat))
    (let ((java-bat (open (format nil "~detps-java-big.bat" sys-dir) :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (format java-bat "@echo off~%")
      (format java-bat "call \"~d\" -I \"~detps.dxl\" -- -javainterface ~djava -classpath \"~djava\" TpsStart -other -big -nopopups -screenx ~d -screeny ~d -bottomOffset ~d -rightOffset ~d~%"
	      lisp-exe sys-dir java-dir sys-dir screenx screeny bottomOffset rightOffset)
      (close java-bat))
    (let ((java-bat (open (format nil "~detps-java-popups.bat" sys-dir) :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (format java-bat "@echo off~%")
      (format java-bat "call \"~d\" -I \"~detps.dxl\" -- -javainterface ~djava -classpath \"~djava\" TpsStart -other -screenx ~d -screeny ~d -bottomOffset ~d -rightOffset ~d~%"
	      lisp-exe sys-dir java-dir sys-dir screenx screeny bottomOffset rightOffset)
      (close java-bat))
    (let ((java-bat (open (format nil "~detps-java-big-popups.bat" sys-dir) :direction :output
			       :if-exists :supersede :if-does-not-exist :create)))
      (format java-bat "@echo off~%")
      (format java-bat "call \"~d\" -I \"~detps.dxl\" -- -javainterface ~djava -classpath \"~djava\" TpsStart -other -big -screenx ~d -screeny ~d -bottomOffset ~d -rightOffset ~d~%"
	      lisp-exe sys-dir java-dir sys-dir screenx screeny bottomOffset rightOffset)
      (close java-bat))
    (format t "You can now run tps or etps using the java interface by double clicking on~%")
    (format t "one of the batch files tps3-java, tps3-java-big, tps3-java-popups,~%")
    (format t "tps3-java-big-popups, etps-java, etps-java-big, etps-java-popups,~%")
    (format t "or etps-java-big-popups in ~d.~%"
	    sys-dir))
  (format t "~%FINISHED.~2%"))


