;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of external-interface)
(context interface)

#-(and allegro-version>= (version>= 5 0))
(defun javaservice-remote-input (str)
  (declare (ignore str))
  nil)

#+(and allegro-version>= (version>= 5 0))
(defun javaservice-remote-input (str stdout)
  (format stdout "Waiting for input~%")
  (let ((insym nil))
  (setf (mp:process-quantum sys:*current-process*) .1)
  (loop while t do 
	(let ((instr (read-to-null str)))
	  (cond ((eq insym 'RIGHTMARGIN)
					; treated special so it can be set while running another command
		 (setq insym nil)
		 (let ((rm (read-from-string instr)))
		   (when (and (integerp rm) (> rm 0))
		     (set-flag 'RIGHTMARGIN rm))))
		((and (eq insym 'COMMAND) 
		      (string-equal instr "INTERRUPT"
				    :start1 0
				    :end1 (min (length instr) 9)))
					; kill a process and start a new prompt
		 (setq insym nil)
		 (when (and *command-process*
			    (mp:process-p *command-process*)
			    (mp:process-active-p *command-process*))
		   (mp:process-kill *command-process*))
		 (setq *command-process*
		       (mp:process-run-function
			"Main Command Process"
			#'absolute-top-top))
		 (setf (mp:process-priority *command-process*) 10))
		(insym
		 (setf (get insym 'RESPONSE) instr)
		 (format stdout "~d: ~d~%" insym instr)
		 (format *standard-input* "~d~%" instr) ; echoed into std input to make save-work work - 2/6/03
		 (setq insym nil))
		((string-equal instr "REMOTE-EXPERT")
		 (setq *expert-running-remotely* t))
		((string-equal instr "NOPOPUPS")
		 (setq *simple-interface-prompts* T))
		((or (string-equal instr "COMMAND"
				   :start1 0
				   :end1 (min (length instr) 7))
		     (string-equal instr "PROMPT"
				   :start1 0
				   :end1 (min (length instr) 6))
		     (string-equal instr "RIGHTMARGIN"
				   :start1 0
				   :end1 (min (length instr) 11)))
		 (setq insym (read-from-string instr))))))))

(defun read-to-null (str)
  (let ((l ""))
    (handler-bind
     ((acl-socket::socket-error 
       #'(lambda (arg)
	   (declare (ignore arg))
	   (userexit)))) ; exit when the connection breaks
     (do ((a (read-char str nil nil) (read-char str nil nil)))
	 ((or (null a) (equal a #\null))
	  (unless a (userexit)) ; exit when the connection breaks
	  l)
	 (setq l (format nil "~d~d" l a))))))


#-(and allegro-version>= (version>= 5 0))
(defmexpr javawin
  (argtypes fontsizestring boolean)
  (argnames fontsize popups)
  (arghelp "Fontsize" "Use Popup Windows for Prompts?")
  (defaultfns (lambda (y z)
		(list (if (eq y '$) "")
		      (if (eq z '$) nil z))))
  (mainfns javawin)
  (mhelp "The Java Interface is not available for TPS under this version of Lisp."))

#+(and allegro-version>= (version>= 5 0))
(defmexpr javawin
  (argtypes fontsizestring boolean)
  (argnames fontsize popups)
  (arghelp "Fontsize" "Use Popup Windows for Prompts?")
  (defaultfns (lambda (y z)
		(list (if (eq y '$) "" y)
		      (if (eq z '$) nil z))))
  (mainfns javawin)
  (mhelp "Begin a Java Interface window to be used for the remainder of this
TPS session."))

#-(and allegro-version>= (version>= 5 0))
(defun javawin (fontsize &optional (popups nil))
  (msgf "Sorry.  The Java Interface is not available for TPS under this version of Lisp."))

#+(and allegro-version>= (version>= 5 0))
(defun javawin (fontsize &optional (popups nil))
  (setq *simple-interface-prompts* (not popups))
  (let* ((pass (acl-socket:make-socket :connect :passive))
	 (port (acl-socket:local-port pass))
	 (command 
	  (format nil "~d 127.0.0.1 ~d ~d ~d" ; 127.0.0.1 is the universal internal IP address
		  JAVA-COMM
		  port
		  fontsize
		  (if popups
		      ""
		    "-nopopups"))))
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
	(mp:process-disable sys:*current-process*)))))

; this is naive
(defun encrypt-password (passwd)
  (let ((enc ""))
    (dotimes (i (length passwd) enc)
      (let* ((c (aref passwd i))
	     (c2 c))
	(cond ((and (> (char-code c) 96)
		    (< (char-code c) 123))
	       (setq c2 (code-char (+ 65 (mod (char-code c) 26)))))
	      ((and (> (char-code c) 64)
		    (< (char-code c) 91))
	       (setq c2 (code-char (+ 97 (mod (char-code c) 26)))))
	      ((and (> (char-code c) 47)
		    (< (char-code c) 58))
	       (setq c2 (code-char (+ 48 (mod (char-code c) 10))))))
	(setq enc (format nil "~d~d" enc c2))))))

(defun check-online-user-password (mode user passwd)
  (if (probe-file USER-PASSWD-FILE)
      (let* ((passfile (open USER-PASSWD-FILE :direction :input))
	     (access (read passfile nil nil))
	     (access2 (assoc mode access))
	     (encpasswd (encrypt-password passwd)))
	(close passfile)
	(if (find-if #'(lambda (x) (and (equal user (car x)) (equal encpasswd (cadr x))))
		     (caddr access2))
	    t
	  (if (cadr access2) ; everyone has access
	      'ANON
	    nil)))
    nil))

(defun check-online-access (mode)
  (if (probe-file USER-PASSWD-FILE)
      (let* ((passfile (open USER-PASSWD-FILE :direction :input))
	     (access (read passfile nil nil))
	     (access2 (assoc mode access)))
	(close passfile)
	(if (cadr access2)
	    (if (caddr access2)
		'ANON-AND-SOME
	      'ANON-ONLY)
	  (if (caddr access2)
	      'SOME
	    nil)))
    nil))

(defun run-tps-server (sock logdir lisp-exec tps-image etps-image)
  (handler-case
   (let* ((command (read-line sock nil nil)))
     (when (stringp command)
       (let ((n (length command))
	     (reqfile "")
	     (readmode 0)
	     (arg "")
	     (val "")
	     (argval-assoc nil)
	     (done nil))
	 (cond ((and (> n 5) (string-equal "GET" command :start2 0 :end2 3))
		(do ((i 4 (1+ i)))
		    ((or done (>= i n)))
		  (let ((ch (aref command i)))
		    (cond ((eq ch #\Space)
			   (when (= readmode 2)
			     (push (cons arg val) argval-assoc))
			   (setq done t))
			  ((and (= readmode 0) (eq ch #\?))
			   (setq readmode 1))
			  ((and (= readmode 1) (eq ch #\=))
			   (setq readmode 2))
			  ((and (= readmode 2) (eq ch #\&))
			   (push (cons arg val) argval-assoc)
			   (setq arg "" val "")
			   (setq readmode 1))
			  ((= readmode 0)
			   (setq reqfile (format nil "~d~d" reqfile ch)))
			  ((= readmode 1)
			   (setq arg (format nil "~d~d" arg ch)))
			  ((= readmode 2)
			   (setq val (format nil "~d~d" val ch))))))
		(cond ((string-equal reqfile "/tps-online.html")
		       (send-http-header sock)
		       (format sock "Connection: close")
		       (send-http-newline sock)
		       (format sock "Content-Type : text/html")
		       (send-http-newline sock 2)
		       (format sock "<HTML><HEAD><TITLE>TPS Online Start</TITLE></HEAD><BODY>")
		       (send-http-newline sock)
		       (send-tps-online-form sock 'TPS)
		       (format sock "</BODY></HTML>")
		       (send-http-newline sock))
		      ((string-equal reqfile "/etps-online.html")
		       (send-http-header sock)
		       (format sock "Connection: close")
		       (send-http-newline sock)
		       (format sock "Content-Type : text/html")
		       (send-http-newline sock 2)
		       (format sock "<HTML><HEAD><TITLE>ETPS Online Start</TITLE></HEAD><BODY>")
		       (send-http-newline sock)
		       (send-tps-online-form sock 'ETPS)
		       (format sock "</BODY></HTML>")
		       (send-http-newline sock))
		      (t
		       (let* ((filepath (windows-compatible-path (format nil "~d" reqfile)))
			      (pn (length filepath)))
			 (if (and (> pn 7) (string-equal ".class" filepath :start2 (- pn 6) :end2 pn)
				  (probe-file (format nil "java~d" filepath)))
			     (send-java-class-file sock (format nil "java~d" filepath))
			   (if (and (> pn 6) (string-equal ".html" filepath :start2 (- pn 5) :end2 pn)
				    (probe-file filepath))
			       (send-html-file sock filepath)
			     (let ((etps-access (check-online-access 'ETPS))
				   (tps-access (check-online-access 'TPS)))
			       (send-http-header sock)
			       (format sock "Connection: close")
			       (send-http-newline sock)
			       (format sock "Content-Type : text/html")
			       (send-http-newline sock 2)
			       (format sock "<HTML><HEAD><TITLE>TPS Server</TITLE></HEAD><BODY>")
			       (send-http-newline sock)
			       (if (or etps-access tps-access)
				   (progn
				     (when tps-access
				       (format sock "<P><A HREF=\"tps-online.html\">Tps Online</A></P>")
				       (send-http-newline sock))
				     (when etps-access
				       (format sock "<P><A HREF=\"etps-online.html\">Etps Online</A></P>")
				       (send-http-newline sock)))
				 (progn
				   (format sock "<P>Sorry.  This TPS server currently gives no access rights to run TPS or ETPS remotely</P>~%")
				   (send-http-newline sock)
				   (format sock "<P>The administrator needs to run SETUP-ONLINE-ACCESS from TPS.</P>~%")))
			       (format sock "</BODY></HTML>")
			       (send-http-newline sock))))))))
	       ((and (> n 6) (string-equal "POST" command :start2 0 :end2 4))
		(do ((i 5 (1+ i)))
		    ((or done (>= i n)))
		  (let ((ch (aref command i)))
		    (if (eq ch #\Space)
			(setq done t)
		      (setq reqfile (format nil "~d~d" reqfile ch)))))
		(when (member reqfile '("/tps-online.html" "/etps-online.html") :test #'string-equal)
		  (let ((mode (if (string-equal reqfile "/tps-online.html") 'TPS 'ETPS)))
		    (do ((l (read-line sock nil nil) (read-line sock nil nil)))
			((or (null l)
			     (and (> (length l) 11)
				  (string-equal "tpswebstart" l :start2 0 :end2 11)))))
		    (do ((l (read-line sock nil nil) (read-line sock nil nil)))
			((or (null l)
			     (and (> (length l) 9)
				  (string-equal "tpswebend" l :start2 0 :end2 9))))
		      (setq readmode 1)
		      (dotimes (i (length l))
			(let ((ch (aref l i)))
			  (cond ((and (eq ch #\=) (= readmode 1))
				 (setq readmode 2))
				((= readmode 1)
				 (setq arg (format nil "~d~d" arg ch)))
				((and (= readmode 2) (not (eq ch #\Return)))
				 (setq val (format nil "~d~d" val ch))))))
		      (unless (equal arg "")
			(push (cons arg val) argval-assoc))
		      (setq arg "" val ""))
		    (send-http-header sock)
		    (format sock "Connection: close")
		    (send-http-newline sock)
		    (format sock "Content-Type : text/html")
		    (send-http-newline sock 2)
		    (let* ((user (cdr (assoc "userid" argval-assoc :test #'string-equal)))
			   (passwd (cdr (assoc "password" argval-assoc :test #'string-equal)))
			   (access (check-online-user-password mode user passwd)))
		      (if access
			  (progn
			    (when (eq access 'ANON)
			      (setq user "anonymous"))
			    (let* ((pass (acl-socket:make-socket :connect :passive))
				   (port (acl-socket:local-port pass)))
			      (multiple-value-bind
				  (second minute hour date month year day-of-week daylight-saving-time-p time-zone)
				  (get-decoded-time)
				(declare (ignore time-zone daylight-saving-time-p day-of-week))
				(let ((logstream (open (windows-compatible-path
							(format nil "~d/~d-log-~d-~d-~d-~d:~d:~d"
								logdir user
								month date year
								hour minute second))
						       :direction :output :if-does-not-exist :create
						       :if-exists :supersede)))
				  (excl:run-shell-command ; start real tps or etps for remote user - try to make this work for windows
				   (format nil "\"~d\" -I \"~d\" -- -remoteuser ~S ~d"
					   lisp-exec
					   (if (eq mode 'TPS) tps-image etps-image)
					   user port)
				   :wait nil :output logstream)
				  (let ((str2 (acl-socket:accept-connection pass)))
				    (let ((port2 (read str2)))
				      (format sock "<HTML><HEAD><TITLE>~d for ~d</TITLE></HEAD><BODY>" mode user)
				      (send-http-newline sock)
				      (format sock "<applet code=TpsAppletStart.class width=30 height=30>")
				      (send-http-newline sock)
				      (format sock "<param name=server value=\"~d\">" MACHINE-INSTANCE)
				      (send-http-newline sock)
				      (format sock "<param name=port value=~d>" port2)
				      (send-http-newline sock)
				      (when (eq mode 'ETPS)
					(format sock "<param name=etps value=t>")
					(send-http-newline sock))
				      (unless (eq access 'ANON)
					(format sock "<param name=known value=t>")
					(send-http-newline sock))
				      (format sock "<param name=fontsize value=~d>"
					      (if (member '("bigfonts" . "t") argval-assoc :test #'equal)
						  "big"
						"small"))
				      (send-http-newline sock)
				      (when (assoc "popups" argval-assoc :test #'equal)
					(format sock "<param name=popups value=t>")
					(send-http-newline sock))
				      (let ((fsh (assoc "fontshift" argval-assoc :test #'equal)))
					(when fsh
					  (format sock "<param name=fontshift value=~d>" (cdr fsh))
					  (send-http-newline sock)))
				      (format sock "<blockquote><hr><em>Sorry, your browser doesn't understand Applets.</em><hr></blockquote>")
				      (send-http-newline sock)
				      (format sock "</applet>")
				      (send-http-newline sock)
				      (send-tps-online-tutorial sock mode)))))))
			(progn
			  (format sock "<P>Sorry, unrecognized userid or password for ~d.</P>" mode)
			  (send-http-newline sock))))
		    (format sock "</BODY></HTML>")
		    (send-http-newline sock)))))))
     (close sock))
   (error (condition)
	  (msgf "Failed: " condition)
	  (when (and (streamp sock) (open-stream-p sock))
	    (close sock))
	  nil)))

(defun send-http-newline (sock &optional (n 1))
  (dotimes (i n)
    (write-byte 13 sock)
    (write-byte 10 sock)))

(defun send-http-header (sock)
  (format sock "HTTP/1.1 200 OK")
  (send-http-newline sock)
  (multiple-value-bind
      (second minute hour date month year day-of-week daylight-saving-time-p time-zone)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore time-zone daylight-saving-time-p))
    (format sock "Date: ~d, ~d ~d ~d ~d:~d:~d GMT"
	    (nth day-of-week '("Mon" "Tues" "Wed" "Thurs" "Fri" "Sat" "Sun"))
	    date
	    (nth month '(nil "Jan" "Feb" "March" "April" "May" "June" "July" "Aug" "Sep" "Oct" "Nov" "Dec"))
	    year hour minute second)
    (send-http-newline sock))
  (format sock "Server: TPS running under Franz Allegro Common Lisp.")
  (send-http-newline sock))

#+:mswindows
(defun windows-compatible-path (path1)
  (let ((path2 ""))
    (dotimes (i (length path1) path2)
      (let ((ch (aref path1 i)))
	(case ch
	  (#\/ (setq path2 (format nil "~d\\" path2)))
	  (#\: (setq path2 (format nil "~d_" path2)))
	  (t (setq path2 (format nil "~d~d" path2 (aref path1 i)))))))))

#-:mswindows
(defun windows-compatible-path (path1)
  path1)

(defun send-html-file (sock file)
  (send-http-header sock)
  (format sock "Connection: close")
  (send-http-newline sock)
  (format sock "Content-Type : text/html")
  (send-http-newline sock 2)
  (let ((str (open file :direction :input)))
      (do ((b (read-byte str nil nil) (read-byte str nil nil)))
	  ((null b) (close str))
	(write-byte b sock))))

(defun send-java-class-file (sock javafile)
  (send-http-header sock)
  (when (probe-file javafile)
    (format sock "Accept-Ranges: bytes")
    (send-http-newline sock)
    (let ((numbytes 0)
	  (javastr (open javafile :direction :input)))
      (do ((b (read-byte javastr nil nil) (read-byte javastr nil nil)))
	  ((null b) (close javastr))
	(incf numbytes))
      (format sock "Content-Length: ~d" numbytes)
      (send-http-newline sock))
    (format sock "Connection: close")
    (send-http-newline sock)
    (format sock "Content-Type: application/octet-stream")
    (send-http-newline sock 2)
    (let ((javastr (open javafile :direction :input)))
      (do ((b (read-byte javastr nil nil) (read-byte javastr nil nil)))
	  ((null b) (close javastr))
	(write-byte b sock)))))

(defun send-tps-online-tutorial (sock mode)
  (let ((file (if (probe-file (format nil "~d-online-tutorial" mode))
		  (format nil "~d-online-tutorial" mode)
		(if (probe-file (format nil "online-tutorial"))
		    "online-tutorial"
		  nil))))
    (when file
      (let ((str (open file :direction :input)))
	(do ((b (read-byte str nil nil) (read-byte str nil nil)))
	    ((null b) (close str))
	  (write-byte b sock))))))

(defun send-tps-online-form (sock mode)
  (let ((acc (check-online-access mode)))
    (if acc
	(progn
	  (format sock "<FORM NAME='tpsweb' METHOD=POST enctype='text/plain'>")
	  (send-http-newline sock)
	  (format sock "<TABLE><TD><TR>")
	  (send-http-newline sock)
	  (format sock "<INPUT TYPE=hidden NAME='tpswebstart' VALUE='t'>")
	  (send-http-newline sock)
	  (when (member acc '(ANON-AND-SOME SOME))
	    (format sock "<INPUT TYPE=checkbox NAME='popups' VALUE='t'>Use Pop-up Prompts instead of a Command Line Interface")
	    (send-http-newline sock))
	  (format sock "</TR></TD><TR><TD>")
	  (send-http-newline sock)
	  (format sock "<INPUT TYPE=checkbox NAME='bigfonts' VALUE='t'>Big Fonts")
	  (send-http-newline sock)
	  (format sock "</TR></TD><TR><TD>")
	  (send-http-newline sock)
	  (format sock "<SELECT NAME='fontshift' VALUE='0' SIZE=3>")
	  (send-http-newline sock)
	  (format sock "<OPTION VALUE='0'>Fonts x 1")
	  (send-http-newline sock)
	  (format sock "<OPTION VALUE='1'>Fonts x 2")
	  (send-http-newline sock)
	  (format sock "<OPTION VALUE='2'>Fonts x 4")
	  (send-http-newline sock)
	  (format sock "</SELECT>")
	  (send-http-newline sock)
	  (format sock "</TR></TD>")
	  (send-http-newline sock)
	  (when (member acc '(ANON-ONLY ANON-AND-SOME))
	    (format sock "<TR><TD>Click the button to start ~d (as an anonymous user).</TR></TD><TR><TD>" mode)
	    (send-http-newline sock)
	    (format sock "<INPUT TYPE=submit NAME='start' VALUE='~d'>" mode)
	    (send-http-newline sock)
	    (format sock "</TR></TD>")
	    (send-http-newline sock))
	  (when (member acc '(ANON-AND-SOME SOME))
	    (format sock "<TR><TD></TR></TD><TR><TD></TR></TD><TR><TD></TR></TD><TR><TD>")
	    (send-http-newline sock)
	    (format sock "If privileges have been set up for you, enter a User Id and Password to run ~d with privileges." mode)
	    (send-http-newline sock)
	    (format sock "</TR></TD><TR><TD>")
	    (send-http-newline sock)
	    (format sock "User Id: <INPUT TYPE=text NAME='userid' VALUE=''>")
	    (send-http-newline sock)
	    (format sock "</TR></TD><TR><TD>")
	    (send-http-newline sock)
	    (format sock "Password: <INPUT TYPE=password NAME='password' VALUE=''>")
	    (send-http-newline sock)
	    (format sock "</TR></TD><TR><TD>")
	    (send-http-newline sock)
	    (format sock "<INPUT TYPE=submit NAME='start' VALUE='~d'>" mode)
	    (send-http-newline sock)
	    (format sock "</TR></TD>")
	    (send-http-newline sock))
	  (format sock "<INPUT TYPE=hidden NAME='tpswebend' VALUE='t'>")
	  (send-http-newline sock)
	  (format sock "</TABLE>")
	  (send-http-newline sock)
	  (format sock "</FORM>")
	  (send-http-newline sock))
      (format sock "Sorry.  This TPS server does not allow any remote to ~d." mode))))

(defun tps-server-main (lisp-exec tps-image etps-image logdir port)
  (let ((pass nil))
    (handler-case
     (progn 
       (ensure-directories-exist (windows-compatible-path (format nil "~d/" logdir)))
       (setq pass (acl-socket:make-socket :connect :passive :local-port port))
       (msgf "Waiting for a connection.")
       (do ((sock (acl-socket:accept-connection pass)
		  (acl-socket:accept-connection pass)))
	   (nil)
	 (msgf "Connection Established")
	 (mp:process-run-function "tps-server" #'run-tps-server
				  sock logdir lisp-exec tps-image etps-image)
	 (msgf "Waiting for a connection.")))
     (error (condition)
	    (progn 
	      (msgf condition t)
	      (msgf "Exiting TPS server")
	      (when (and (streamp pass) (open-stream-p pass))
		(close pass))
	      (userexit))))))

(context lib-display)

(defmexpr cl-user::ls ; cannot export ls from core
  (mainfns ls-files)
  (mhelp "List the files in the current directory."))

(defun ls-files ()
  (let ((files (mapcar #'(lambda (p)
			   (let ((n (pathname-name p))
				 (ty (pathname-type p)))
			     (if ty
				 (format nil "~d.~d" n ty)
			       n)))
		       (directory "."))))
    (when files
      (let* ((m (apply #'max (mapcar #'length files)))
	     (cols (max 1 (floor (/ rightmargin (+ m 2)))))
	     (i 1))
	(dolist (f files)
	  (let ((l (length f)))
	    (format t "~d" f)
	    (incf i)
	    (if (> i cols)
		(progn
		  (format t "~%")
		  (setq i 1))
	      (dotimes (j (- (+ m 2) l))
		(format t " ")))))))))

