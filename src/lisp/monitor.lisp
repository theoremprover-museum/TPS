;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of MATING)

;;;
;;; File : Monitor
;;; 

(deffile monitor
  (part-of mating)
  (extension clisp)
  (mhelp "Defines the monitor functions."))

(context mating-search)

;; *** MONITOR-CHECK ***

(defmonitor monitor-check
  (argtypes string)
  (argnames prefix)
  (arghelp "Marker string")
  (mainfns monitor-chk)
  (mhelp "Prints out the given string every time the monitor is called, 
followed by the place from which it was called."))

(defun monitor-chk (string)
  (setq *current-monitorfn* 'monitor-check)
  (setq *current-monitorfn-params* string)
  (setq *monitorfn-params-print* 'msg))

(defun monitor-check (place alist)
  (declare (ignore alist))
  (msg *current-monitorfn-params* place t)) 

;; *** FOCUS-MATING and FOCUS-MATING* ***

(defmonitor focus-mating
  (argnames mating flaglist valuelist)
  (argtypes matingpairlist tpsflaglist anything-list)
  (arghelp "Mating to watch for" "Flags to change" "New values for these flags")
  (defaultfns (lambda (mating y z) (list (if (eq mating '$) (figure-out-mating) mating) y z)))
  (mainfns focus-mating-fn)
  (mhelp "Reset some flags when a particular mating is reached. 
The default mating is the mating that is current at the
time when this command is invoked (so the user can often enter the 
mate top level, construct the mating manually and then type 
FOCUS-MATING). Otherwise, the mating should be typed in the form
((LEAFa . LEAFb) (LEAFc . LEAFd) ...) The order in which the connections
are specified within the mating, and the order of the literals within each
connection, do not matter."))

(defun focus-mating-fn (mpairlist flaglist valuelist)
  (check-mpairlist mpairlist)
  (setq *current-monitorfn* 'focus-mating)
  (setq *current-monitorfn-params* (cons mpairlist (cons (shuffle-flaglists flaglist valuelist nil) nil)))
  (setq *monitorfn-params-print* 'focus-print))

(defun check-mpairlist (mpl)
  (if (or ms-jform (and current-eproof (eproof-jform current-eproof)))
      (dolist (m mpl)
	      (if (not (find-jform-name (complete-leaf-name (car m) leaf-name) (or ms-jform (eproof-jform current-eproof))))
		  (msg "I can't find a leaf called " (car m) ", but I'll continue anyway." t))
	      (if (not (find-jform-name (complete-leaf-name (cdr m) leaf-name) (or ms-jform (eproof-jform current-eproof))))
		  (msg "I can't find a leaf called " (cdr m) ", but I'll continue anyway." t)))
    (msg "I can't find any of these leaves; there is no current jform." t "I'll continue anyway.")))


(defun figure-out-mating ()
  (if (not active-mating) nil
    (progn (let ((result nil))
	     (dolist (connection (mating-clist active-mating))
		     (let ((conn (car (gethash connection (eproof-connections-array master-eproof)))))
		       (setq result (cons (cons (show-other-name (literal-name (car conn))) 
						(show-other-name (literal-name (cdr conn)))) result))))
	     result))))

(defun shuffle-flaglists (flaglist valuelist returnlist)
  (if (null flaglist) returnlist
    (if (null valuelist) returnlist
      (if (funcall (get (get (car flaglist) 'flagtype) 'testfn) (car valuelist))
	  (shuffle-flaglists (cdr flaglist) (cdr valuelist) (cons (cons (car flaglist) (car valuelist)) returnlist))
	(progn (msg "Omitting " (car flaglist) " because " (car valuelist) " is of the wrong type.")
	       (shuffle-flaglists (cdr flaglist) (cdr valuelist) returnlist))))))

(defun focus-mating (place alist)
  (if (eq place 'added-conn); we're in the right place
      (if (cdr (assoc 'npfd alist))
	  (progn (if (mating-equal (car *current-monitorfn-params*) (figure-out-mating))
		     (progn (msg t "*** Monitor invoked; resetting flags ***" t)
			    (dolist (pair (cadr *current-monitorfn-params*))
				    (set-flag (car pair) (cdr pair))))))
	(progn (if (mating-equal (car *current-monitorfn-params*) (process-pfd-mating (cdr (assoc 'mating alist))))
		   (progn (msg t "*** Monitor invoked; resetting flags ***" t)
			  (dolist (pair (cadr *current-monitorfn-params*))
				  (set-flag (car pair) (cdr pair)))))))))


(defun process-pfd-mating (mlist)
  (if (null mlist) nil
    (cons (cons (show-other-name (literal-name (caaar mlist))) 
		(show-other-name (literal-name (cdaar mlist))))
	  (process-pfd-mating (cddr mlist)))))

(defun focus-print (params)
  (msg "When the current mating is : " (car params) t)
  (msg "The following flags will be changed to the given new values : " t)
  (if (null (cddr params))
      (progn (alist-msg (cadr params)) (msg t))
    (progn (alist-msg (cadr params)) (msg t "and afterwards, they will be changed back to the values : " t)
	   (alist-msg (cddr params)) (msg t))))

(defun alist-msg (params)
  (if (not (null params))
      (progn (if (cdr (car params)) (msg (car params)) (msg "(" (caar params) " . NIL)"))
	     (alist-msg (cdr params)))))

(defun mating-equal (alist1 alist2)
  (if (and (null alist1) (null alist2)) t     ;we reached the end of both lists
    (if (or (null alist1) (null alist2)) nil  ;we reached the end of one list but not the other
      (mating-equal (filter-alist (car alist1) alist1) (filter-alist (car alist1) alist2)))))

;      (progn
;	(let ((element (car alist1)))
;	  (if (or (string-equal (princ-to-string (assoc (car element) alist2)) (princ-to-string element))
;		  (string-equal (princ-to-string (rassoc (car element) alist2)) (princ-to-string (cons (cdr element) (car element)))))
;		(mating-equal (filter-alist element alist1) (filter-alist element alist2))
;	    nil)))))

(defun filter-alist (element alist)
  (if (null alist) nil
    (if (string-equal (princ-to-string (car alist)) (princ-to-string element)) 
	(filter-alist element (cdr alist))
      (if (string-equal (princ-to-string (car alist)) (princ-to-string (cons (cdr element) (car element)))) 
	  (filter-alist element (cdr alist))
	(cons (car alist) (filter-alist element (cdr alist)))))))

(defmonitor focus-mating*
  (argnames mating flaglist valuelist)
  (argtypes matingpairlist tpsflaglist anything-list)
  (arghelp "Mating to watch for" "Flags to change" "New values for these flags")
  (defaultfns (lambda (mating y z) (list (if (eq mating '$) (figure-out-mating) mating) y z)))
  (mainfns focus-mating*-fn)
  (mhelp "Reset some flags when a particular mating is reached. Differs
from FOCUS-MATING in that it returns the flags to their original 
settings afterwards. The default mating is the mating that 
is current at the time when this command is invoked (so the user 
can often enter the mate top level, construct the mating manually and 
then type FOCUS-MATING*). Otherwise, the mating should be typed in the form
((LEAFa . LEAFb) (LEAFc . LEAFd) ...). The order in which the connections
are specified within the mating, and the order of the literals within each
connection, do not matter.
The values used for the \"original\" flag settings will also
be those that are current at the time when this command is invoked."))

(defun focus-mating*-fn (mpairlist flaglist valuelist)
  (setq *current-monitorfn* 'focus-mating*)
  (check-mpairlist mpairlist)
  (let ((sf (shuffle-flaglists flaglist valuelist nil)))
    (setq *current-monitorfn-params* (cons mpairlist (cons sf (read-flagvals sf)))) 
    (setq *monitorfn-params-print* 'focus-print)))

(defun read-flagvals (flaglist)
  (if (null flaglist) nil
    (cons (cons (caar flaglist) (eval (caar flaglist)))
	  (read-flagvals (cdr flaglist)))))

(defvar focus-storage nil)

(defun focus-mating* (place alist)
  (if (eq place 'added-conn); we're in the right place
      (if (cdr (assoc 'npfd alist))
	  (if (mating-equal (car *current-monitorfn-params*) (figure-out-mating))
	      (progn (msg t "*** Monitor invoked; resetting flags ***" t)
		     (setq focus-storage t)
		     (dolist (pair (cadr *current-monitorfn-params*))
			     (set-flag (car pair) (cdr pair))))
	    (if focus-storage (progn (msg t "*** Monitor invoked; resetting flags ***" t)
				    (setq focus-storage nil)
				    (dolist (pair (cddr *current-monitorfn-params*))
					    (set-flag (car pair) (cdr pair))))))
	(if (mating-equal (car *current-monitorfn-params*) (process-pfd-mating (cdr (assoc 'mating alist))))
	    (progn (msg t "*** Monitor invoked; resetting flags ***" t)
		   (setq focus-storage t)
		   (dolist (pair (cadr *current-monitorfn-params*))
			   (set-flag (car pair) (cdr pair))))
	  (if focus-storage (progn (msg t "*** Monitor invoked; resetting flags ***" t)
				   (setq focus-storage nil)
				   (dolist (pair (cddr *current-monitorfn-params*))
					   (set-flag (car pair) (cdr pair)))))))))


;;; FOCUS-OSET and FOCUS-OSET*

(defmonitor focus-oset
  (argnames oset flaglist valuelist)
  (argtypes string tpsflaglist anything-list)
  (arghelp "Option set to watch for" "Flags to change" "New values for these flags")
  (mainfns focus-oset-fn)
  (mhelp "Reset some flags when a particular option set is reached. 
The option set should be entered in the form \"oset-n\" where n is a
positive integer. See also FOCUS-OSET*.
This only works for the procedures MS91-6 and MS91-7.
There is a similar monitor function for MS89 and MS90-9, 
called FOCUS-OTREE."))

(defun focus-oset-fn (oset flaglist valuelist)
  (if (check-oset oset)
      (progn (setq *current-monitorfn* 'focus-oset)
	     (setq *current-monitorfn-params* (cons (string-downcase oset) (cons (shuffle-flaglists flaglist valuelist nil) nil)))
	     (setq *monitorfn-params-print* 'focus-oset-print))
    (msg oset " is not the name of an oset." t "It should be of the form oset-n where n is an integer."
	 t "Not changing monitor function.")))

(defun focus-oset-print (params)
  (msg "When the current oset is : " (car params) t)
  (msg "The following flags will be changed to the given new values : " t)
  (if (null (cddr params))
      (progn (alist-msg (cadr params)) (msg t))
    (progn (alist-msg (cadr params)) (msg t "and afterwards, they will be changed back to the values : " t)
	   (alist-msg (cddr params)) (msg t))))

(defmonitor focus-oset*
  (argnames oset flaglist valuelist)
  (argtypes string tpsflaglist anything-list)
  (arghelp "Option set to watch for" "Flags to change" "New values for these flags")
  (mainfns focus-oset-fn*)
  (mhelp "Reset some flags when a particular option set is reached, 
and then set the flags back again when the option set changes again.
The option set should be entered in the form \"oset-n\" where n is a
positive integer.
The values for the flags to revert to are those which are current at 
the time you typed FOCUS-OSET*. See also FOCUS-OSET.
This only works for the procedures MS91-6 and MS91-7.
There is a similar monitor function for MS89 and MS90-9, 
called FOCUS-OTREE*."))

(defun focus-oset-fn* (oset flaglist valuelist)
  (if (check-oset oset)
      (progn (let ((sf (shuffle-flaglists flaglist valuelist nil)))
	       (setq *current-monitorfn* 'focus-oset*)
	       (setq *current-monitorfn-params* (cons (string-downcase oset) (cons sf (read-flagvals sf))))
	       (setq *monitorfn-params-print* 'focus-oset-print)))
    (msg oset " is not the name of an oset." t "It should be of the form oset-n where n is an integer."
	 t "Not changing monitor function.")))

(defun check-oset (oset)
  (let ((str (string-downcase oset)))
    (if (and (string= str "oset-" :start1 0 :end1 5)
	     (or (string> str "oset-0") (string= str "oset-0")) ; number is either 0 or has no leading 0
	     (string< str (concatenate 'string "oset-" (make-string (length str) :initial-element #\9))))
	t
      nil)))

;; i.e. if the string is bigger than "oset-0" and smaller than "oset-999999..." for a lot of 9's
;; If anybody ever invents a character set in which "9">="0", this will fail miserably...

(defun focus-oset (place alist)
  (if (eq place 'new-oset); we're in the right place
      (if (string-equal (car *current-monitorfn-params*) (cdr (assoc 'oset alist)))
	  (progn (msg t "*** Monitor invoked; resetting flags ***" t)
		 (dolist (pair (cadr *current-monitorfn-params*))
			 (set-flag (car pair) (cdr pair)))))))

(defun focus-oset* (place alist)
  (if (eq place 'new-oset); we're in the right place
      (if (string-equal (car *current-monitorfn-params*) (cdr (assoc 'oset alist)))
	  (progn (msg t "*** Monitor invoked; resetting flags ***" t)
		 (setq focus-storage t)
		 (dolist (pair (cadr *current-monitorfn-params*))
			 (set-flag (car pair) (cdr pair))))
	(if focus-storage (progn (msg t "*** Monitor invoked; resetting flags ***" t)
				 (setq focus-storage nil)
				 (dolist (pair (cddr *current-monitorfn-params*))
					 (set-flag (car pair) (cdr pair))))))))


;;; FOCUS-OTREE and FOCUS-OTREE*

(defmonitor focus-otree
  (argnames otree flaglist valuelist)
  (argtypes string tpsflaglist anything-list)
  (arghelp "Option tree to watch for" "Flags to change" "New values for these flags")
  (mainfns focus-otree-fn)
  (mhelp "Reset some flags when a particular option tree is reached. 
The option tree should be entered in the form \"OPTn\" where n is a
positive integer. This only works for the procedures MS89 and MS90-9.
See also FOCUS-OTREE*.
There is a similar monitor function for MS91-6 and MS91-7, 
called FOCUS-OSET."))

(defun focus-otree-fn (otree flaglist valuelist)
  (if (check-otree otree)
      (progn (setq *current-monitorfn* 'focus-otree)
	     (setq *current-monitorfn-params* (cons (string-upcase otree) (cons (shuffle-flaglists flaglist valuelist nil) nil)))
	     (setq *monitorfn-params-print* 'focus-otree-print))
    (msg otree " is not the name of an otree." t "It should be of the form OPTn where n is an integer."
	 t "Not changing monitor function.")))

(defun focus-otree-print (params)
  (msg "When the current otree is : " (car params) t)
  (msg "The following flags will be changed to the given new values : " t)
  (if (null (cddr params))
      (progn (alist-msg (cadr params)) (msg t))
    (progn (alist-msg (cadr params)) (msg t "and afterwards, they will be changed back to the values : " t)
	   (alist-msg (cddr params)) (msg t))))

(defmonitor focus-otree*
  (argnames otree flaglist valuelist)
  (argtypes string tpsflaglist anything-list)
  (arghelp "Option set to watch for" "Flags to change" "New values for these flags")
  (mainfns focus-otree-fn*)
  (mhelp "Reset some flags when a particular option tree is reached, 
and then set the flags back again when the option tree changes again.
The option tree should be entered in the form \"OPTn\" where n is a
positive integer.
The values for the flags to revert to are those which are current at 
the time you typed FOCUS-OTREE*. See also FOCUS-OTREE.
This only works for the procedures MS89 and MS90-9.
There is a similar monitor function for MS91-6 and MS91-7, 
called FOCUS-OSET*."))

(defun focus-otree-fn* (otree flaglist valuelist)
  (if (check-otree otree)
      (progn (let ((sf (shuffle-flaglists flaglist valuelist nil)))
	       (setq *current-monitorfn* 'focus-otree*)
	       (setq *current-monitorfn-params* (cons (string-upcase otree) (cons sf (read-flagvals sf))))
	       (setq *monitorfn-params-print* 'focus-otree-print)))
    (msg otree " is not the name of an otree." t "It should be of the form OPTn where n is an integer."
	 t "Not changing monitor function.")))

(defun check-otree (otree)
  (let ((str (string-upcase otree)))
    (if (and (string= str "OPT" :start1 0 :end1 3)
	     (or (string> str "OPT0") (string= str "OPT0")) ; number is either 0 or has no leading 0
	     (string< str (concatenate 'string "OPT" (make-string (length str) :initial-element #\9))))
	t
      nil)))

;; i.e. if the string is bigger than "OPT0" and smaller than "OPT999999..." for a lot of 9's
;; If anybody ever invents a character set in which "9"<="0", this will fail miserably...

(defun focus-otree (place alist)
  (if (eq place 'new-otree); we're in the right place
      (if (string-equal (car *current-monitorfn-params*) (cdr (assoc 'otree alist)))
	  (progn (msg t "*** Monitor invoked; resetting flags ***" t)
		 (dolist (pair (cadr *current-monitorfn-params*))
			 (set-flag (car pair) (cdr pair)))))))

(defun focus-otree* (place alist)
  (if (eq place 'new-otree); we're in the right place
      (if (string-equal (car *current-monitorfn-params*) (cdr (assoc 'otree alist)))
	  (progn (msg t "*** Monitor invoked; resetting flags ***" t)
		 (setq focus-storage t)
		 (dolist (pair (cadr *current-monitorfn-params*))
			 (set-flag (car pair) (cdr pair))))
	(if focus-storage (progn (msg t "*** Monitor invoked; resetting flags ***" t)
				 (setq focus-storage nil)
				 (dolist (pair (cddr *current-monitorfn-params*))
					 (set-flag (car pair) (cdr pair))))))))

; ** PUSH-MATING **

(defmonitor push-mating
  (argnames mating)
  (argtypes matingpairlist)
  (arghelp "Mating to watch for")
  (defaultfns (lambda (mating) (list (if (eq mating '$) (figure-out-mating) mating))))
  (mainfns push-mating-fn)
  (mhelp "Executes a PUSH (i.e. halts and starts a new top level) when a 
particular mating is reached. The default mating is the mating that is current 
at the time when this command is invoked (so the user can often enter the 
mate top level, construct the mating manually and then type 
PUSH-MATING). Otherwise, the mating should be typed in the form
((LEAFa . LEAFb) (LEAFc . LEAFd) ...) The order in which the connections
are specified within the mating, and the order of the literals within each
connection, do not matter.
When PUSH-MATING is invoked, typing POP will leave the new top level and
continue with the search."))

(defun push-mating-fn (mpairlist)
  (check-mpairlist mpairlist)
  (setq *current-monitorfn* 'push-mating)
  (setq *current-monitorfn-params* (list mpairlist))
  (setq *monitorfn-params-print* 'push-print))

(defun push-print (params)
  (msg "When the current mating is : " (car params) t)
  (msg "The search will be halted and a PUSHed top level will be started." t))


(defun push-mating (place alist)
  (if (eq place 'added-conn); we're in the right place
      (if (cdr (assoc 'npfd alist))
	  (progn (if (mating-equal (car *current-monitorfn-params*) (figure-out-mating))
		     (progn (msg t "*** Monitor invoked; starting PUSHed top level ***" t "*** Type POP to continue ***")
			    (top))))
	(progn (if (mating-equal (car *current-monitorfn-params*) (process-pfd-mating (cdr (assoc 'mating alist))))
		   (progn (msg t "*** Monitor invoked; starting PUSHed top level ***" t "*** Type POP to continue ***")
			    (top)))))))

