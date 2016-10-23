;; ********************************************************************************************
;; *** This file is adapted from socket.lisp from the KEIM project written by Stephan Hess. ***
;; ********************************************************************************************
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 151150                                                      ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :AUTO)


; (mod~defmod SOCKET 
;             :uses nil
;             :documentation "A socket implementation in ACL."
;             :exports (
;                       socket~active?
;                       socket~close
;                       socket~connect
;                       socket~define
;                       socket~delete
;                       socket~fd
; 		      socket~find-socket
;                       socket~read
;                       socket~receives?
;                       socket~reset
;                       socket~write
;                       socket~write-file

; 		      socket*end-of-string
;                       socket*sockets
		      
;                       socket~accept
; 		      socket~bind
;                       socket~read-line
;                       socket~read-content
;                       socket~start-server
; 		      socket~get-peername

; 		      socket+def-error
; 		      socket+unknown-error
; 		      socket+connected-error
; 		      socket+unconnected-error
; 		      socket+bind-error
; 		      socket+accept-error
; 		      ))
		      

;; ---------------------------------------------------------------------------
;; Section 0 : foreign function definitions for socket implementation
;; ---------------------------------------------------------------------------

#+(not (and allegro-version>= (version>= 5 0)))
(progn 
(ff:defforeign 'perror :arguments '(string))
(ff:defforeign 'usocket)
(ff:defforeign 'ubind :arguments '(integer))
(ff:defforeign 'uaccept :arguments '(integer))
(ff:defforeign 'uclose :arguments '(integer))
(ff:defforeign 'uconnect :arguments '(string integer))
(ff:defforeign 'uread :arguments '(integer integer string) :return-type :integer)
(ff:defforeign 'ureadwait :arguments '(integer integer string) :return-type :integer)
(ff:defforeign 'uwrite :arguments '(integer string))
(ff:defforeign 'ugetpeername :arguments '(integer))
;; nobody needs this anymore and it causes problems at CMU -- afiedler
;;(ff:defforeign 'uwritefile :arguments '(integer string))
)

;; ---------------------------------------------------------------------------
;; Section 1 : The socket database. 
;; ---------------------------------------------------------------------------

#+(not (and allegro-version>= (version>= 5 0)))
(defvar socket*sockets nil "A property list of symbolic socket names and sockets.")

#+(and allegro-version>= (version>= 5 0))
(defvar socket*sockets (make-hash-table) "A hashtable of symbolic socket names and sockets.")

#+(not (and allegro-version>= (version>= 5 0)))
(defmacro socket=sockets ()

  ;;; Edited  : 02.12.1998
  ;;; Authors : serge
  ;;; Input   : /
  ;;; Effect  : /
  ;;; Value   : the actual value of `socket*sockets
  
  `socket*sockets)


#+(and allegro-version>= (version>= 5 0))
(defmacro socket=sockets ()
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   -)
;	   (effect  -)
;	   (value   "The actual value of `socket*sockets."))
  `socket*sockets)

#+(not (and allegro-version>= (version>= 5 0)))
(DEFMACRO MAPCF (BODY PROP.LIST)

  ;;; Input:  a functional with two arguments and a propertylist
  ;;; Effect: calls \verb$BODY$ with each pair indicator and value of \verb$PROP.LIST$
  ;;; Value:  the result of the last execution of \verb$BODY$
  
  (LET ((LOCAL.VAR  (gensym "intern-")))
       `(LET ((,LOCAL.VAR T))
             (DO* ((TAIL ,PROP.LIST (CDDR TAIL))
                   (INDICATOR (CAR TAIL) (CAR TAIL))
                   (VALUE (SECOND TAIL) (SECOND TAIL)))
                  ((NULL TAIL) ,LOCAL.VAR)
                  (SETQ ,LOCAL.VAR (FUNCALL ,BODY INDICATOR VALUE))))))

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~reset ()

  ;;; Edited  : 03.12.1998
  ;;; Authors : serge
  ;;; Input   : /
  ;;; Effect  : resets all sockets. It closes any connected socket and removes the entries from the
  ;;;           socket database. 
  ;;; Value   : undef.

  (setq socket*end-of-string (code-char 128))
  (mapcf #'(lambda (socketname socket)
	     (when (not (equal socket 'unconnected))
	       (socket~close socketname)))
	 (socket=sockets))
  (setq socket*sockets nil))

#+(and allegro-version>= (version>= 5 0))
(defun socket~reset ()
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   -)
;	   (effect  "Resets all sockets. It closes any connected socket"
;		    "and removes the entries from the socket database." )
;	   (value   -))
  (maphash #'(lambda (socketname socket)
	       (declare (ignore socket))
	       (socket~close socketname))
	   (socket=sockets))
  (clrhash (socket=sockets)))

#+(and allegro-version>= (version>= 5 0))
(defun socket~find-socket (socketname)
;  (declare (edited  "21-SEP-2000")
;	   (authors Afiedler)
;	   (input   "The name of a socket.")
;	   (value   "The corresponding socket if it exists, otherwise NIL."))
  (gethash socketname (socket=sockets)))

; the following code code seems to be dead. ArFi
#+deadcode(defmacro socket=buffer-string (socketname)

  ;;; Edited  : 12. Jan 1999
  ;;; Authors : serge       
  ;;; Input   : a socket name
  ;;; Effect  : /
  ;;; Value   : the buffer string of the socket described by socketname.

  `(getf (getf (socket=sockets) ,socketname) 'buffer-string))


;; ---------------------------------------------------------------------------
;; Section 2 : Defining/Undefining sockets.
;; ---------------------------------------------------------------------------


#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~define (socketname &optional (error-p nil))

  ;;; Edited  : 02.12.1998
  ;;; Authors : serge
  ;;; Input   : an sexpr which is the symbolic name of a socket. 
  ;;; Effect  : creates an entry for this socket. If error-p is not NIL an error is
  ;;;           signaled on failure.
  ;;; Value   : T for success, NIL for failure. 
  
  (cond ((and socketname (null (getf socket*sockets socketname)))
	 (setf (getf (getf socket*sockets socketname) 'port) nil)
	 (setf (getf (getf socket*sockets socketname) 'socket) 'unconnected)
	 (setf (getf (getf socket*sockets socketname) 'buffer-string) "")
	 T
	 )
	((null socketname)
	 (let ((message (format nil "Invalid socket name ~A!" socketname)))
	   (print message)
	   nil))
	(T 
	 (let ((message "Redefinition of existing socket-names is not allowed!"))
	   (print message)
	   nil))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~define (socketname &optional (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "An sexpr which is the symbolic name of a socket.")
;	   (effect  "Creates an entry for this socket.")
;	   (value   "T for success, NIL for failure."))
  (cond ((and socketname (null (gethash socketname socket*sockets)))
	 (setf (gethash socketname socket*sockets) 'unconnected)
	 T)
	((null socketname)
	 (let ((message (format nil "Invalid socket name ~A!" socketname)))
	   (print message)
	   nil))
	(T 
	 (let ((message "Redefinition of existing socket-names is not allowed!"))
	   (print message)
	   nil))))

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~delete (socketname &optional (error-p nil))
  (declare (ignore error-p))
  ;;; Edited  : 17.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : an sexpr defining a socket, i.e. a socketname
  ;;; Effect  : deletes the socketname
  ;;;           If error-p is not NIL an error is signaled on failure.
  ;;; Value   : T for success, NIL for failure.

  (let ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (equal (getf socket 'socket) 'unconnected))
	   (remf socket*sockets socketname)
	   T)
	  ((null  socket)
	   (format nil "Unknown socket ~A given to socket-undefine!"
		   socketname)
	   nil
;	    ""
	   )
	  (t 
	   (format t "Socket ~A is still connected in socket-delete!" socketname)
	   nil
;	     ""
	     ))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~delete (socketname &optional (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "An sexpr defining a socket, i.e. a socketname.")
;	   (effect  "Deletes the socketname.")
;	   (value   "T for success, NIL for failure."))
  (let ((socket (gethash socketname (socket=sockets))))
    (cond ((and socket (equal socket 'unconnected))
	   (remhash socketname socket*sockets)
	   T)
	  ((null  socket)
	   (format nil "Unknown socket ~A given to socket-undefine!"
		   socketname)
	   nil
;	    ""
	   )
	  (t 
	   (format t "Socket ~A is still connected in socket-delete!" socketname)
	   nil
;	     ""
	     ))))



#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~active? (socketname)

  ;;; Edited  : 11. Feb 1999
  ;;; Authors : serge       
  ;;; Input   : a socketname
  ;;; Effect  : checks whether a socket of this name exists and, if so, if it is connected.
  ;;; Value   : T, if the check succeeds; NIL otherwise.
  
  (let ((socket (getf (socket=sockets) socketname))
	(astring (make-string 1))
	val)
    (if (getf socket 'port) t
      (cond ((and socket (numberp (getf socket 'socket)) T)
	     (setq val (uread (getf socket 'socket) 1 astring))
	     (cond ((eq val -2) ;; Socket is there, but nothing on the socket.
		    T)
		   ((eq val 1) ;; Socket is there and read one character.
		  ;;; Saving the character in the buffer-string of the socket.
		    (setf (getf (getf (socket=sockets) socketname) 'buffer-string) 
			  (concatenate 'string 
				       (getf (getf (socket=sockets) socketname) 'buffer-string)
				       (copy-seq (string (elt  astring 0)))))
		  T)))))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~active? (socketname)
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "A socketname.")
;	   (effect  "Checks whether a socket of this name exists and, if so, if it is connected.")
;	   (value   "T, if the check succeeds, NIL otherwise."))
  (let ((socket (gethash socketname (socket=sockets))))
    (and socket
	 (or (and (streamp socket) (open-stream-p socket)
		  (not (and (listen socket)
			    (eq (peek-char nil socket nil 'eof) 'eof))))  ;active sockets
	     (not (equal socket 'unconnected))))))                       ;passive sockets

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~receives? (socketname)

  ;;; Edited  : 10. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : a socketname 
  ;;; Effect  : checks whether there is something coming over the socket.
  ;;; Value   : T, if there is something; NIL otherwise.

  (let ((socket (getf (socket=sockets) socketname))
	(astring (make-string 1))
	val)
    (cond ((and socket (numberp (getf socket 'socket)) T)
	   (setq val (uread (getf socket 'socket) 1 astring))
	   (cond ((eq val -2) ;; Socket is there, but nothing on the socket.
		  nil)
		 ((eq val 1) ;; Socket is there and read one character.
		  ;;; Saving the character in the buffer-string of the socket.
		  (setf (getf (getf (socket=sockets) socketname) 'buffer-string) 
		    (concatenate 'string 
		      (getf (getf (socket=sockets) socketname) 'buffer-string) (copy-seq (string (elt  astring 0)))))
		  T))))))
  
#+(and allegro-version>= (version>= 5 0))
(defun socket~receives? (socketname)
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "A socketname.")
;	   (effect  "Checks whether there is something coming over the socket.")
;	   (value   "T, if there is something; NIL otherwise."))
  (let ((socket (gethash  socketname (socket=sockets))))
    (and socket (socket~active? socketname)(stream::stream-listen socket))))

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~fd (socketname)

  ;;; Edited  : 12. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : a socketname
  ;;; Effect  : /
  ;;; Value   :the file descriptor of this socket

  (getf (getf (socket=sockets) socketname) 'socket))

#+(and allegro-version>= (version>= 5 0))
(defun socket~fd (socketname)
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "A socketname.")
;	   (effect  -)
;	   (value   "The file descriptor of this socket."))
  (let ((socket (gethash socketname (socket=sockets))))
    (when (socket~active? socketname) (socket::socket-os-fd socket))))



;; ---------------------------------------------------------------------------
;; Section 3 : Connecting/Closing sockets.
;; ---------------------------------------------------------------------------


#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~connect (host port socketname &optional (error-p nil))

  ;;; Edited  : 17.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : A host and a portnumber to connect to.
  ;;; Effect  : Connects to the specified socket. If error-p is not NIL an error is
  ;;;           signaled on failure
  ;;; Value   : T for success, NIL for failure.

  (let (socket)
    (cond ((and (setq socket (getf (socket=sockets) socketname)) 
		(eq (getf socket 'socket) 'unconnected))
	   (setf (getf (getf socket*sockets socketname) 'socket) (uconnect host port))
	   T)
	  ((null socket)
	   (let ((message (format nil "Socket name ~A not defined" socketname)))
	     (print message)
	     nil))
	  (t (let ((message (format nil "Socket ~A already connected." socketname)))
	       (print message)
	       nil)))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~connect (host port socketname)
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "A host and a portnumber to connect to.")
;	   (effect  "Connects to the specified socket.")
;	   (value   "T for success, NIL for failure."))
  (let (socket)
    (cond ((and (setq socket (gethash socketname (socket=sockets))) 
		(equal socket  'unconnected))
	   (setf (gethash socketname socket*sockets)
		 (socket::make-socket :remote-host host
				      :remote-port port
		                      :type :stream
				      :address-family :internet
				      :connect :active))
	   T)
	  ((null socket)
	   (let ((message (format nil "Socket name ~A not defined" socketname)))
	     (print message)
	     nil))
	  (t (let ((message (format nil "Socket ~A already connected." socketname)))
	       (print message)
	       nil)))))


#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~close (socketname &optional (error-p nil))
  (declare (ignore error-p))
  ;;; Edited  : 17.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : A socket name.
  ;;; Effect  : Closes the connection to the socket. If error-p is not NIL an error is
  ;;;           signaled on failure.
  ;;; Value   : T for success, NIL for failure.

  (let ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected)))
	   (uclose (getf socket 'socket))
	   (setf (getf socket 'socket) 'unconnected)
	   (setf (getf socket 'buffer-string) "")
	   t)
	  ((null  socket)
	   (format t "Unknown socket ~A given to socket-close!" socketname)
	   nil
;	   ""
	   )
	  (t 
	   (format t "Socket ~A is not connected in socket-close!" socketname)
	   nil
;	     ""
	     ))))


#+(and allegro-version>= (version>= 5 0))
(defun socket~close (socketname &optional (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "A socket name.")
;	   (effect  "Closes the connection to the socket. If error-p is not NIL an error is"
;		    "signaled on failure.")
;	   (value   "T for success, NIL for failure."))
    (let ((socket (gethash socketname (socket=sockets))))
    (cond ((and socket (not (equal socket 'unconnected)))
	   (socket::close socket)
	   (setf (gethash socketname (socket=sockets)) 'unconnected)
	   T)
	  ((null  socket)
	   (format t "Unknown socket ~A given to socket-close!" socketname)
	   nil
;	   ""
	   )
	  (t 
	     (format t "Socket ~A is not connected in socket-close!" socketname)
	     nil
;	     ""
	     ))))


;; ---------------------------------------------------------------------------
;; Section 4 : Read from/Write to sockets.
;; ---------------------------------------------------------------------------


(defvar socket*end-of-string 

  ;;; Edited  : 09.12.1998
  ;;; Authors : serge
  ;;; Descri. : the character indicating the end of a string to be read from a socket. 

  (code-char 128))


;(proclaim '(type character socket*end-of-string))

(defmacro socket=end-of-string ()

  ;;; Edited  : 09.12.1998
  ;;; Authors : serge
  ;;; Input   : /
  ;;; Effect  : /
  ;;; Value   : the actual value of `socket*end-of-string

  `socket*end-of-string)


#+(not (and allegro-version>= (version>= 5 0)))
(defun socket=readloop (socket &optional wait?)

  ;;; Edited  : 17.03.1997 01.12.1998 09.12.1998 12. Jan 1999 19. Feb 1999
  ;;; Authors : hess       serge      serge      serge        serge       
  ;;; Input   : a socket description, which is a property list ('SOCKET socket 'BUFFER-STRING string)
  ;;; Effect  : /
  ;;; Value   : A string read from the socket, if there is some terminating with (socket=end-of-string). MP:untill-character 
  ;;;           NIL if there is nothing on the socket. If there is something on the socket, but the end-of-string
  ;;;           character (socket=end-of-string) has not been read, waits for this character. 
  
  (let ((val 1)
	(astring (make-string 1 :initial-element (code-char 32)))
	(local-stream (make-string-output-stream))
	(overall-string (getf socket 'buffer-string))
	(the-socket (getf socket 'socket))
	(result nil))
    (do ()
	;;; Read from socket one character, until we got the end-of-string character or an error. 
	((or (eq (socket=end-of-string) (elt astring 0)) (and (< val 1) (not (= val -2)))))
      (when (eq 1 (setq val (if wait? (ureadwait the-socket 1 astring) 
			      (uread the-socket 1 astring))))
	;; Store the recently read character in the overall string.
	;; (if not eq to socket=end-of-string
	(unless (eq (socket=end-of-string) (elt astring 0))
	  (write-char (elt  astring 0) local-stream)))
      ;; (setq overall-string (concatenate 'string overall-string (copy-seq (string (elt astring 0))))))
      )
    (cond ((< val 1) ;;; if we got an error on the socket, save the string read so far in the socket buffer and return NIL.
	   (setq result nil)
	   (setf (getf socket 'buffer-string) 
	     (concatenate 'string overall-string (get-output-stream-string local-stream)))
	   (cond ;; ((eq val -2) ;; (format t "No more symbols on the socket.~%"))
	         ((eq val -1) (format t "General read error on the socket.~%"))
		 ((eq val 0) (format t "Got an EOF on the socket. The other side might have closed/lost the socket.~%")))
	   )
	  (T ;;; otherwise return the actual string and delete the socket buffer.
	   (setf (getf socket 'buffer-string) "")
	   (setq result 
	     (concatenate 'string overall-string (get-output-stream-string local-stream)))))
    result))
	     
#+(and allegro-version>= (version>= 5 0))
(defun socket=readloop (socketname)
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "A socket.")
;	   (effect  -)
;	   (value   "A string read from the socket, if there is some terminating with (socket=end-of-string)."
;		    "NIL if there is nothing on the socket. If there is something on the socket, but the end-of-string"
;		    "character (socket=end-of-string) has not been read, waits for this character."))
  (let 	((astring (make-string 1 :initial-element (code-char 32)))
	 (socket (gethash socketname (socket=sockets)))
	 (local-stream (make-string-output-stream)))
    (do ()
	;;; Read from socket one character, until we got the end-of-string character or an error. 
	((or (eq (socket=end-of-string) astring)
	    (not (socket~active? socketname))))
      (when (socket~receives? socketname) (setf astring (read-char socket))
	;; Store the recently read character in the overall string.
	;; (if not eq to socket=end-of-string
	(unless (eq (socket=end-of-string) astring)
	  (write-char astring local-stream)))
      ;; (setq overall-string (concatenate 'string overall-string (copy-seq (string (elt astring 0))))))
      )
    (if (socket~active? socketname)
	(get-output-stream-string local-stream)
      (format t "General read error on the socket.~%"))))


#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~read (&optional (socketname :inout) (wait? nil) (error-p nil))
  (declare (ignore error-p))
  ;;; Edited  : 24.03.1997 01.12.1998  19. Feb 1999
  ;;; Authors : hess       serge       serge       
  ;;; Input   : /
  ;;; Effect  : Write handshake signal to socket. If error-p is not NIL an error is
  ;;;           signaled on failure.
  ;;; Value   : The string read from the socket on success, the empty string on failure.
  
  (let* ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected))) 
	   (socket=readloop socket wait?))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t
	   (format t "Socket ~A is not connected in read-socket!" socketname)
	   ""))))


#+(and allegro-version>= (version>= 5 0))
(defun socket~read (&optional (socketname :inout) (wait? nil) (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "Socketname, wait-switch, signal-error-switch.")
;	   (effect  "Write handshake signal to socket. If error-p is not NIL an error is"
;		    "signaled on failure.")
;	   (value   "The string read from the socket on success, the empty string on failure."))
  (declare (ignore wait?))
  (let* ((socket (gethash socketname (socket=sockets))))
    (cond ((and socket (not (equal socket 'unconnected))) 
	   (socket=readloop socketname))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t
	   (format t "Socket ~A is not connected in read-socket!" socketname)
	   ""))))

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~write (string &optional (socketname :inout)
			    (eos (string socket*end-of-string))
			    (error-p nil))
  (declare (ignore error-p))  
  ;;; Edited  : 24.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : A string and the name of a socket.
  ;;; Effect  : Wait for ready signal, then write string to socket. If error-p is not NIL
  ;;;           an error is signaled on failure.
  ;;; Value   : T for success, NIL for failure. 

  (let ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected)))
	   (uwrite (getf socket 'socket) (concatenate 'string string eos))
	   T)
	  ((null socket)
	   (format t "Unknown socket ~A given to write to!" socketname)
	   nil
;	   ""
	   )
	  (t
	   (format t "Socket ~A is not connected in write-socket!" socketname)
	   nil
;	   ""
	   ))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~write (string &optional (socketname :inout)
			    (eos (string socket*end-of-string))
			    (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "20-SEP-2000")
;	   (authors Pollet)
;	   (input   "A string and the name of a socket.")
;	   (effect  "Wait for ready signal, then write string to socket..")
;	   (value   "T for success, NIL for failure."))
  (let ((socket (gethash socketname (socket=sockets))))
    (cond ((and socket (not (equal socket 'unconnected)))
	   (format socket "~A" (concatenate 'string string (string eos)))
	   (force-output socket)
	   T)
	  ((null socket)
	   (format t "Unknown socket ~A given to write to!" socketname)
	   nil
;	   ""
	   )
	  (t
	   (format t "Socket ~A is not connected in write-socket!" socketname)
	   nil
;	   ""
	   ))))

#|| nobody needs this anymore and it causes problems in socket.c at CMU -- afiedler
#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~write-file (header pathname &optional (socketname :inout)
			    (eos (string socket*end-of-string))
			    (error-p nil))
  (declare (ignore error-p))  
  ;;; Edited  : 24.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : A string and the name of a socket.
  ;;; Effect  : Wait for ready signal, then write string to socket. If error-p is not NIL
  ;;;           an error is signaled on failure.
  ;;; Value   : T for success, NIL for failure. 

  (let ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected)))
	   (uwrite (getf socket 'socket) header)
	   (uwritefile (getf socket 'socket) pathname)
	   T)
	  ((null socket)
	   (format t "Unknown socket ~A given to write to!" socketname)
	   nil
;	   ""
	   )
	  (t
	   (format t "Socket ~A is not connected in write-socket!" socketname)
	   nil
;	   ""
	   ))))
||#

;; ---------------------------------------------------------------------------
;; Section 5 : Stuff for TCP-Server and HTTP
;; ---------------------------------------------------------------------------

(defgeneric socket~start-server (port socketname)
;  (declare (edited  "04-AUG-2000")
;	   (authors Pollet)
;	   (input   "A port and a the socketname of a defined socket.")
;	   (effect  "Establish a server at port PORT.")
;	   (value   "-"))
  (:method ((port string) (socketname symbol))
	   (socket~start-server (read-from-string port) socketname))
  (:method ((port integer) (socketname symbol))
	   (socket~define socketname)
	   (unless (socket~bind port socketname)
	     (socket~delete socketname)))
  (:method (port socketname)
	   (declare (ignore port socketname))
	   nil))

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~bind (port socketname &optional (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "04-AUG-2000")
;	   (authors Pollet)
;	   (input   "A port and a the socketname of a defined socket.")
;	   (effect  "Establish a server at port PORT. If error-p in not NIL an error is"
;		    "signaled on failure.")
;	   (value   "T for success, NIL for failure."))
  (let ((socket (gethash socketname (socket=sockets))))
    (if socket
	(if (eq (getf socket 'socket) 'unconnected)
	    (let ((fd (ubind port)))
	      (if (and (numberp fd) (plusp fd))
		  (progn
		    (setf (getf socket 'socket) fd)
		    (setf (getf socket 'port) port)
		    t)
		(progn 
		       (format t "Problems to bind port ~A to socket ~A." port socketname)
		       nil)))
	  (progn 
		 (format t "Socket ~A already connected." socketname)
		 nil))
      (progn 
	     (format t "Socket name ~A not defined" socketname)
	     nil))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~bind (port socketname &optional (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "04-AUG-2000")
;	   (authors Pollet)
;	   (input   "A port and a the socketname of a defined socket.")
;	   (effect  "Establish a server at port PORT. If error-p in not NIL an error is"
;		    "signaled on failure.")
;	   (value   "T for success, NIL for failure."))
  (let ((socket (gethash socketname (socket=sockets))))
    (if socket
	(if (eq (gethash socketname socket*sockets) 'unconnected)
	    (handler-case
	     (progn (setf (gethash socketname socket*sockets)
			  (socket::make-socket :connect :passive
					       :local-port port))
		    T)
             (error (condition)
		    (progn 
		      (format t "Problems to bind port ~A to socket ~A, because ~A." port socketname condition)
		      nil)))
	  (progn 
	    (format t "Socket ~A already connected." socketname)
	    nil))
      (progn 
	(format t "Socket name ~A not defined" socketname)
	nil))))


#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~accept (serversocket connectsocket &optional (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "04-AUG-2000")
;	   (authors Pollet)
;	   (input   "Two socketnames.")
;	   (effect  "Waits for a connection on SERVERSOCKET. If this"
;		    "happens, CONNECTSOCKET will be connected to client."
;		    "If error-p is not NIL an error is signalled on failure.")
;	   (value   "T for success, NIL for failure."))
  (let ((ssock (getf (socket=sockets) serversocket))
	(csock (getf (socket=sockets) connectsocket)))
    (if (and csock ssock)
	(if (eq (getf csock 'socket) 'unconnected)
	    (let ((fd (uaccept (getf ssock 'socket))))
	      (if (and (numberp fd) (plusp fd))
		  (progn (setf (getf csock 'socket) fd)
			 t)
		(progn 
		  (format t "Problems to accept ~A." serversocket)
		  nil)))
	  (progn 
	    (format t "Socket ~A already connected." connectsocket)
	    nil))
      (progn
	(format t "Socket name ~A or ~A not defined" serversocket connectsocket)
	nil))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~accept (serversocket connectsocket &optional (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "04-AUG-2000")
;	   (authors Pollet)
;	   (input   "Two socketnames.")
;	   (effect  "Waits for a connection on SERVERSOCKET. If this"
;		    "happens, CONNECTSOCKET will be connected to client."
;		    "If error-p is not NIL an error is signalled on failure.")
;	   (value   "T for success, NIL for failure."))
  (let ((ssock (gethash serversocket (socket=sockets)))
	(csock (gethash connectsocket (socket=sockets))))
    (cond ((not (and csock ssock))
	   (format t "Socket name ~A or ~A not defined" serversocket connectsocket)
	   nil)
	  ((not (eq csock 'unconnected))
	   (format t "Socket ~A already connected." connectsocket)
	   nil)
	  ((eq ssock 'unconnected)
	   (format t "Socket ~A not connected." connectsocket)
	   nil)
	  ((streamp ssock)
	   (format t "Socket ~A is not a passive socket." connectsocket)
	   nil)
	  (T
	   (handler-case
	    (progn
	      (setf (gethash connectsocket (socket=sockets))
		    (socket::accept-connection ssock))
	      T)
	    (error (condition)
	       (progn 
		 (format t "Problems to accept ~A because ~A." serversocket condition)
		 nil)))))))



#+(not (and allegro-version>= (version>= 5 0)))
(defun socket=readuntil (socket what &optional wait?)
;  (declare (edited  "06-AUG-2000")
;	   (authors Pollet)
;	   (input   "a socket description, which is a property list ('SOCKET socket 'BUFFER-STRING string)"
;		    "and a NUMBER or CHAR.")
;	   (effect  "Reads NUMBER chars from the socket or untill CHAR or (socket=end-of-string)." )
;	   (value   "Returns a string containing the chars read."))  
  (let* ((val 1)
	 (astring (make-string 1 :initial-element (code-char 32)))
	 (local-stream (make-string-output-stream))
	 (overall-string (getf socket 'buffer-string))
	 (the-socket (getf socket 'socket))
	 (result nil)
	 (counter 0)
	 (test (etypecase what
		 (number #'(lambda (x)(declare (ignore x))(= counter what)))
		 (character #'(lambda (x)(eq what (elt x 0))))
		 (null #'(lambda (x)(declare (ignore x)) nil)))))
		
    (do ()
	;;; Read from socket one character, until we got the end-of-string character or an error. 
	((or (funcall test astring)
	     (eq (socket=end-of-string) (elt astring 0))
	     (= val 0)
	     ))
      (when (eq 1 (setq val (if wait? (ureadwait the-socket 1 astring) 
			      (uread the-socket 1 astring))))
	;; Store the recently read character in the overall string.
	;; (if not eq to socket=end-of-string
	(unless (or (eq (socket=end-of-string) (elt astring 0)))
	  (setq counter (1+ counter))
	  (write-char (elt  astring 0) local-stream)))
      ;; (setq overall-string (concatenate 'string overall-string (copy-seq (string (elt astring 0))))))
      )
    (cond ((and (< val 1)(not (= val -2))) ;;; if we got an error on the socket, save the string read so far in the socket buffer and return NIL.
	   (setq result nil)
	   (setf (getf socket 'buffer-string) 
	     (concatenate 'string overall-string (get-output-stream-string local-stream)))
	   (cond ;; ((eq val -2) ;; (format t "No more symbols on the socket.~%"))
	         ((eq val -1) (format t "General read error on the socket.~%"))
		 ((eq val 0) (format t "Got an EOF on the socket. The other side might have closed/lost the socket.~%")))
	   )
	  (T ;;; otherwise return the actual string and delete the socket buffer.
	   (setf (getf socket 'buffer-string) "")
	   (setq result 
	     (concatenate 'string overall-string (get-output-stream-string local-stream)))))
    result))

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~read-line (socketname &optional (wait? nil) (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "06-AUG-2000")
;	   (authors Pollet)
;	   (input   "Socketname.")
;	   (effect  "Write handshake signal to socket. If error-p is not NIL an error is"
;		    "signaled on failure.")
;	   (value   "The string containing the line read from the socket."))
  (let* ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected))) 
	   (socket=readuntil socket #\newline wait?))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t 
	   (format t "Socket ~A is not connected in read-socket!" socketname)
	   ""))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~read-line (socketname &optional (wait? nil) (error-p nil))
  (declare (ignore wait? error-p))
;  (declare (edited  "06-AUG-2000")
;	   (authors Pollet)
;	   (input   "Socketname.")
;	   (effect  "Write handshake signal to socket. If error-p is not NIL an error is"
;		    "signaled on failure.")
;	   (value   "The string containing the line read from the socket."))
  (let* ((socket (gethash socketname (socket=sockets))))
    (cond ((and socket (not (equal socket 'unconnected)))
	   (read-line socket))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t 
	     (format t "Socket ~A is not connected in read-socket!" socketname)
	     ""))))

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~read-content (size socketname &optional (wait? nil) (error-p nil))
  (declare (ignore error-p))
;  (declare (edited  "06-AUG-2000")
;	   (authors Pollet)
;	   (input   "Socketname and a number.")
;	   (effect  "Write handshake signal to socket. If error-p is not NIL an error is"
;		    "signaled on failure.")
;	   (value   "The string containing SIZE chars read from the socket."))
  (let* ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected))) 
	   (socket=readuntil socket size wait?))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t 
	     (format t "Socket ~A is not connected in read-socket!" socketname)
	     ""))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~read-content (size socketname &optional (wait? nil) (error-p nil))
  (declare (ignore wait? error-p))
;  (declare (edited  "06-AUG-2000")
;	   (authors Pollet)
;	   (input   "Socketname and a number.")
;	   (effect  "Write handshake signal to socket. If error-p is not NIL an error is"
;		    "signaled on failure.")
;	   (value   "The string containing SIZE chars read from the socket."))
  (let* ((socket (gethash socketname (socket=sockets))))
    (cond ((and socket (not (equal socket 'unconnected)))
	   (let ((input (make-string size)))
	     (read-sequence input socket)
	     input))	  
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t 
	     (format t "Socket ~A is not connected in read-socket!" socketname)
	     ""))))

#+(not (and allegro-version>= (version>= 5 0)))
(defun socket~get-peername (socketname)
;  (declare (edited  "29-AUG-2000")
;	   (authors Pollet)
;	   (input   "A Socketname")
;	   (effect  "Calls the foreign functions 'ugetpeername'.")
;	   (value   "A string with the IP number of the connected host."))
  (let ((socket (getf (socket=sockets) socketname))
	val)
    (when (and socket (numberp (getf socket 'socket))
	       (setq val (ugetpeername (getf socket 'socket))))
	  (unless (= val -1)
	    #+(and allegro-version>= (version>= 5 0))(excl:native-to-string val)
	    #+(or allegro-v4.3 allegro-v4.3.1)(ff:char*-to-string val)
	      ))))

#+(and allegro-version>= (version>= 5 0))
(defun socket~get-peername (socketname)
;  (declare (edited  "29-AUG-2000")
;	   (authors Pollet)
;	   (input   "A Socketname")
;	   (effect  -)
;	   (value   "A string with the IP number of the connected host."))
  (let ((socket (gethash socketname (socket=sockets))))
    (when socket
      (socket::ipaddr-to-dotted (socket::remote-host socket)))))

; (defmacro sys~define-condition (name (&rest parents) 
; 				     slot-specifiers
; 				     report-function
; 				     &rest options)
;     `(define-condition ,name ,parents ,new-slot-specifiers 
;       (:report ,report-function) ,@options)

; (sys~define-condition
;  socket+def-error (sys+error)
;  ((message))
;  (lambda (cond str)
;    (format str "~A" (socket+def-error-message cond))))

; (sys~define-condition
;  socket+unknown-error (sys+error)
;  ((name))
;  (lambda (cond str)
;    (format str "Unknown socket ~A!" (socket+unknown-error-name cond))))

; (sys~define-condition
;  socket+connected-error (sys+error)
;  ((name))
;  (lambda (cond str)
;    (format str "Socket ~A still connected!" (socket+connected-error-name cond))))

; (sys~define-condition
;  socket+unconnected-error (sys+error)
;  ((name))
;  (lambda (cond str)
;    (format str "Socket ~A not connected!" (socket+unconnected-error-name cond))))

; (sys~define-condition
;  socket+bind-error (sys+error)
;  ((port) (name))
;  (lambda (cond str)
;    (format str "Cannot bind port ~A to socket ~A!"
; 	   (socket+bind-error-port cond)
; 	   (socket+bind-error-name cond))))

; (sys~define-condition
;  socket+accept-error (sys+error)
;  ((name))
;  (lambda (cond str)
;    (format str "Cannot accept socket ~A!" (socket+accept-error-name cond))))

; cebrown - 8/25/2001
#-(and allegro-version>= (version>= 5 0))
(defun socket~bind-new-port (socketname)
  (tps-warning "sockets are not supported in this version of lisp"))

#+(and allegro-version>= (version>= 5 0))
(defun socket~bind-new-port (socketname)
  (setf (gethash socketname (socket=sockets))
	(socket:make-socket :connect :passive)))

#-(and allegro-version>= (version>= 5 0))
(defun socket~port (socketname)
  (throwfail "sockets are not supported in this version of lisp"))

#+(and allegro-version>= (version>= 5 0))
(defun socket~port (socketname)
  (socket:local-port (gethash socketname (socket=sockets))))
