;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         This code was written as part of the TPS project at         ;;;
;;;                     Carnegie-Mellon University.                     ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@K.CS.CMU.EDU)               ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of BARE)

(deffile tops20
  (part-of bare)
  (extension lisp)
  (mhelp "System-dependent and implementation-dependent functions."))

#+tops-20
(progn
 (defconstnt control-g-char #\^G)
 (defconstnt control-d-char #\^D)
 (defconstnt control-e-char #\^E)
 (defconstnt escape-char #\escape)
 )

#+(or lucid :sbcl :cmu)
(progn
 (defconstnt control-g-char (code-char 7))
 (defconstnt control-d-char (code-char 4))
 (defconstnt control-e-char (code-char 5))
 (defconstnt escape-char #\ESC)
 )

#-(or tops-20 lucid :sbcl :cmu)
(progn
 (defconstnt control-g-char (code-char 7))
 (defconstnt control-d-char (code-char 4))
 (defconstnt control-e-char (code-char 5))
 (defconstnt escape-char (code-char 27))
 )

#-kcl
(defmacro tpsbell () `(princ #\bell))

#+kcl
(defmacro tpsbell () `(princ #\^G))

#+tops-20
(defun linelength (&optional (n nil))
  (cond ((null n) (nth 1 (member :wrap (get-terminal-modes *terminal-io*))))
	((= n 0) (set-terminal-modes *terminal-io* :wrap nil))
	(t (set-terminal-modes *terminal-io* :wrap n))))

#+lucid
(defun linelength (&optional (n nil))
  (cond ((null n) *pp-line-length*)
	((= n 0) (setq *pp-line-length* 0))
	(t (setq *pp-line-length* n))))

#+(or ibcl kcl)
(defun linelength (&optional (n nil))
  (cond ((null n) *print-length*)
	((= n 0) (setq *print-length* 0))
	(t (setq *print-length* n))))

#+:cmu
(defun linelength (&optional (n nil))
  (declare (ignore n))
  (or (lisp::line-length) 80))

#+(and (or :allegro :excl) (not (or :allegro-v4.3  :allegro-v4.2 :allegro-v4.1 :allegro-v4.0)) (not (and allegro-version>= (version>= 5 0))))

(defun linelength (&optional (n nil))
  (cond ((null n) excl::pp-line-length)
	((= n 0) (setq excl::pp-line-length 0))
	(t (setq excl::pp-line-length n))))

#+(or :sbcl :allegro-v4.2 :allegro-v4.1 :allegro-v4.0 :allegro-v4.3 (and allegro-version>= (version>= 5 0)))
(defun linelength (&optional (n nil))
  (if (null n) 
      common-lisp:*print-right-margin*
    (setq common-lisp:*print-right-margin* n)))

; cebrown - not sure how to handle this for clisp,
; just return a default value of 80
; NOTE: clisp means Gnu Common Lisp (http://clisp.cons.org/)
#+:clisp
(defun linelength (&optional (n nil))
  80)

;;; In contrast to the previous function, LINEWIDTH will always return
;;; an integer.  It simply defaults the linewidth to 80, unless 
;;; different information is available.  Linewidth may not be used
;;; to change the linewidth.

(defun linewidth ()
  (let ((l (linelength nil)))
    (if (integerp l) l 79)))

;;; For :cmu (RT) need to have two separate functions, one for
;;; concept, one for console (X)
;*;#+(or lucid :cmu)
;*;(defun terminal-image-mode (&optional (stream *terminal-io*))
;*;  (format *error-output* "~&Cannot put terminal in image mode."))

#-tops-20
(defun terminal-image-mode (&optional (stream *terminal-io*))
  (declare (ignore stream))
  nil)

#+tops-20
(defun terminal-image-mode (&optional (stream *terminal-io*))
  "Sets up the terminal so control characters may be printed using
  write-char."
  (set-terminal-modes *terminal-io* :translate nil :wrap nil))

;*;(defmacro %top-ctatch% (form &rest rest)
;*;  `(catch 'lisp::break-loop-catcher 
;*;	(catch 'lisp::top-level-catcher ,form))
;*;  ;;(if (eq style 'concept-s)
;*;    ;; (set-terminal-modes *terminal-io* :wrap T))
;*;  )

;*;(defmacro %top-catch% (form &rest rest)
;*;  (if rest
;*;      `(%catch% ,form (lisp::break-loop-catcher  ,@rest)
;*;		       (lisp::top-level-catcher ,@rest))
;*;      `(catch 'lisp::break-loop-catcher 
;*;	      (catch 'lisp::top-level-catcher ,form))))

(defvar saving-work-p nil)
(defvar save-work-echo-stream nil)

(defun write-^G ()
  (if (and (boundp 'saving-work-p) saving-work-p
	   (output-stream-p save-work-echo-stream))
      (progn
       (write-char #\space save-work-echo-stream)
       (write-char control-g-char save-work-echo-stream)
       (terpri save-work-echo-stream))))

#+tops-20
(defmacro %top-catch% (form &rest rest)
  `(%catch% ,form (lisp::break-loop-catcher (write-^G) ,@rest)
	    (lisp::top-level-catcher (write-^G) ,@rest)))

#+tops-20
(defun throw-^G ()
  (throw 'lisp::break-loop-catcher nil))

#+(and lucid (not :lcl3.0))
(defmacro %top-catch% (form &rest rest)
  `(%catch% ,form
	    (lucid::top-level (write-^G) ,@rest)))

;#+:lcl3.0
;(defmacro %top-catch% (form &rest rest)
;  `(%catch% ,form
;	    (system::top-level (write-^G) ,@rest)))

#+(and lucid (not :lcl3.0))
(defun throw-^G ()
  (throw 'lucid::top-level nil))

;#+:lcl3.0
;(defun throw-^G ()
;  (throw 'system::top-level nil))

#+(or :cmu)
(defmacro %top-catch% (form &rest rest)
  `(%catch% ,form (debug::debug-loop-catcher (write-^G) ,@rest)
	    (lisp::top-level-catcher (write-^G) ,@rest)))


#+(or :cmu)
(defun throw-^G ()
  (throw 'debug::debug-loop-catcher nil))

#+kcl
(setq si::*quit-tag* ':kcl-quit-tag)

#+kcl
(defmacro %top-catch% (form &rest rest)
  `(%catch% ,form (:kcl-quit-tag (write-^G) ,@rest)))

#+kcl
(defun throw-^G ()
  (throw ':kcl-quit-tag nil))

#+ibcl
(defmacro %top-catch% (form &rest rest)
  `(cond::restart-case (progn ,form)
     (conditions:abort ()
        :report (lambda (stream)
		  (format stream "TPS Top"))
       ,@rest)))

#+:lcl3.0
(defmacro %top-catch% (form &rest rest)
  `(lcl:restart-case (progn ,form)
     (lcl:abort ()
        :report "Return to TPS Top"
       ,@rest)))

#+:lcl3.0
(defun throw-^G ()
  (write-^g) 
  (lcl:invoke-restart 'lcl:abort))

; took care of catching at top level by changing system::*driver* - ceb 5/29/2004
#+(or :sbcl :clisp)
(defmacro %top-catch% (form &rest rest)
  form)

#+(or :allegro :excl)
(defmacro %top-catch% (form &rest rest)
  `(%catch% ,form ;(debug::debug-loop-catcher (write-^G) ,@rest)
	    (:top-level-reset (write-^G) ,@rest)))


;#+ibcl
;(defmacro throw-^G ()
;  `(cond::invoke-restart 'conditions:abort))


#+ibcl
(defun throw-^G ()
  (write-^g) 
  (cond::invoke-restart 'conditions:abort))

#+(or :allegro :excl)
(defun throw-^G ()
  (write-^g) 
  (throw :top-level-reset nil))





;;Assuming that NEWS-DIR is the same as the one on which TPS3 is located.
;; This is defined only for tops20.
#+tops-20
(defun initialize-sys-dir ()
  (let ((exe-file
	 (make-pathname% :device core-name :name core-name :type "exe")))
    (setq sys-dir (if (probe-file exe-file)
		       (pathname-directory (truename exe-file))
		       nil))))

(defvar date-tps3-saved nil)

(defun finish-time-record ()
  (setq building-finish-time (get-universal-time))
;  (setq smart-load T)
  (setq date-tps3-saved (stringdt nil)))

#+tops-20
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (gc)
  (save save-file))

#+tops-20
(defun lisp::%top-level () (secondary-top-main))

#+(or ibcl kcl)
(defun si::top-level () (secondary-top-main))

#+(or :allegro-v4.2 :allegro-v4.1 :allegro-v4.3 (and allegro-version>= (version>= 5 0)))
(setq excl:*enable-package-locked-errors* nil)

#+(or :allegro :excl)
(defun top-level::top-level-read-eval-print-loop () 
  (secondary-top-main))

;; set up :out as a way to get out of break loop
#+(or :allegro :excl)
(top-level:alias "out" () (throw-^g))


#+:allegro
(defun excl::copyright-banner () (values))

; ceb - 5/26/2004
; setting *driver* to secondary-top-main instead of system::main-loop makes
; sure we never get thrown into the lisp top level instead of TPS top level. - ceb 5/29/2004
#+:clisp
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (setq system::*driver* #'secondary-top-main)
  (ext:saveinitmem save-file :quiet t :init-function 'secondary-top-main))

#+lucid
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (disksave save-file :restart-function 'secondary-top-main
	              :gc t :verbose t))

#+(or :cmu)
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (gc)
  (extensions:save-lisp save-file  
			:init-function 'secondary-top-main :print-herald nil))

#+(or :sbcl)
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (gc)
  (sb-ext:save-lisp-and-die save-file  
			:toplevel 'secondary-top-main))

#+(or ibcl kcl)
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (gbc nil)
  (save save-file))

#+(and :allegro (not (or :allegro-v4.3 (and allegro-version>= (version>= 5 0)))))
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (excl:gc :tenure)
  (excl:gc :tenure)
  (excl:gc :tenure)
  (excl:gc t)
  (excl:dumplisp :name save-file :checkpoint t
		 :read-init-file nil
                 :restart-function 'secondary-top-main)
  (excl:exit 0)
)

#+(or :allegro-v4.3 (and allegro-version>= (version>= 5 0)))
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (excl:gc t)
  (excl:gc t)
  (excl:gc t)
  (excl:gc :tenure)
  (excl:gc t)
  (setq excl:*read-init-files* nil)
  (setq excl:*restart-init-function* nil)
  (setq excl:*restart-app-function* 'core::secondary-top-main)
  (excl:dumplisp :name save-file); :checkpoint t)
;the other arguments have been removed; we used to imitate :restart-function in the
;initialization string by adding -e "(secondary-top-main)" but now we don't need to.
;see /afs/andrew/mcs/math/etps/sun4_55/etps   for an example.
;MB Tue Jul  1 15:29:06 1997
  (excl:exit 0)
)

#+(and :excl (not (or :allegro-v4.3 (and allegro-version>= (version>= 5 0)))))
(defun tps3-save ()
  (finish-time-record)
  (organize-all)
  (excl:gc)
  (excl:dumplisp :name save-file 
		 :read-init-file nil
                 :restart-function 'secondary-top-main)
  (excl:exit 0)
)

;;; Following code is used to ensure that oldspace is periodically
;;; gc'd.

#+allegro
(eval-when (compile load eval)
  (setq excl:*global-gc-behavior* :auto))

#+tops-20
(defun exit-from-lisp () (lisp::exit))

#+lucid
(defun exit-from-lisp () (quit))

#+:clisp
(defun exit-from-lisp () (exit))

#+:cmu
(defun exit-from-lisp () (extensions:quit))

#+:sbcl
(defun exit-from-lisp () (sb-ext:quit))

#+(or ibcl kcl)
(defun exit-from-lisp () (bye))

#+(or :allegro :excl)
(defun exit-from-lisp () (excl:exit))

#+(and tops-20 (not :mswindows))
(defun status-userid () (pathname-directory (user-homedir-pathname)))

#+(and lucid (not :mswindows))
(defun status-userid ()
  (intern
   (or (lucid-common-lisp::environment-variable "CL-USER")
       (with-input-from-string 
	     (in (with-output-to-string (out)
		   (lucid-common-lisp::run-unix-program "whoami" nil 
							:wait t :output out)))
	 (read-line in))
       (car (last (pathname-directory (truename (user-homedir-pathname))))))))

#+(and :excl (not :allegro))
(defun status-userid ()
  (let ((dir (pathname-directory (user-homedir-pathname))))
    (intern (subseq dir (1+ (position #\/ dir :from-end t))))))

#+:cmu
(defun status-userid ()
  (intern 
   ;; let's trust the user not to be trying to trick us
   (or (cdr (assoc :cl-user ext:*environment-list*))
       ;; if for some reason USER is not defined in environment,
       ;; call whoami 
       (with-input-from-string 
	   (in (with-output-to-string (out)
		 (ext:run-program "whoami" nil :wait t :output out)))
	 (read-line in)))))

#+(or :clisp :sbcl)
(defun status-userid ()
  (intern (car (last (pathname-directory (user-homedir-pathname))))))

#+(or ibcl kcl :allegro)
(defun status-userid ()
  (intern (or (system:getenv "CL-USER")
	      (car (last (pathname-directory (user-homedir-pathname)))))))

;;; CALL-SYSTEM should be used for UNIX systems only.  It should take
;;; a string which is to be evaluated by a new shell, and should return
;;; the status of the process, i.e., 0 if it succeeds.

#+(or ibcl kcl)
(defun call-system (string)
  (system string))


; #+:cmu
; This is the old version which no longer works in CMUCL.
; A new version is below.  - cebrown 8/3/99
;;(defun call-system (string)
;;"Passes STRING to a new csh -f. Output from the shell will go to the
;;standard output unless redirected by STRING.  If any error output is
;;generated, then 1 will be returned, else 0."
;;  (multiple-value-bind
;;    (pid csh-stream out-stream error-stream)
;;    (ext:run-program "/bin/csh" '("-f") 
;;		     :input :stream :wait nil :output t
;;		     :error :stream
;;		     )
;;    (declare (ignore pid out-stream))
;;    (format csh-stream "~A~%" string)
;;    (format csh-stream "exit~%")
;;    (sleep 1) ; give time for any output to get to error-stream
;;    (prog1
;;	(if (listen error-stream)
;;	    1
;;	    0)
;;      (close csh-stream)
;;      (close error-stream)
;;      )))

#+:cmu
(defun call-system (string)
"Passes STRING to a new csh -f. Output from the shell will go to the
standard output unless redirected by STRING.  If any error output is
generated, then 1 will be returned, else 0."
(let* ((pid (ext:run-program "/bin/csh" '("-f") 
			     :input :stream :wait nil :output t
			     :error :stream
			     ))
       (csh-stream (process-input pid))
       (error-stream (process-error pid)))
  (format csh-stream "~A~%" string)
  (format csh-stream "exit~%")
  (sleep 1) ; give time for any output to get to error-stream
  (prog1
      (if (listen error-stream)
	  1
	0)
    (close csh-stream)
    (close error-stream))))

#+:sbcl
(defun call-system (string)
"Passes STRING to a new csh -f. Output from the shell will go to the
standard output unless redirected by STRING.  If any error output is
generated, then 1 will be returned, else 0."
(let* ((pid (sb-ext:run-program "/bin/csh" '("-f") 
			     :input :stream :wait nil :output t
			     :error :stream
			     ))
       (csh-stream (process-input pid))
       (error-stream (process-error pid)))
  (format csh-stream "~A~%" string)
  (format csh-stream "exit~%")
  (sleep 1) ; give time for any output to get to error-stream
  (prog1
      (if (listen error-stream)
	  1
	0)
    (close csh-stream)
    (close error-stream))))

#+lucid
(defun call-system (string)
"Passes STRING to a new csh -f. Output from the shell will go to the
standard output unless redirected by STRING.  If any error output is
generated, then 1 will be returned, else 0."
  (multiple-value-bind
    (csh-stream error-stream )
    (run-unix-program "/bin/csh" :arguments '("-f") 
		     :input :stream :wait nil 
		     :error-output :stream
		     )
    (format csh-stream "~A~%" string)
    (format csh-stream "exit~%")
    (sleep 1)
    (prog1
	(if (listen error-stream) ; give time for any output to get to error-stream
	    1
	    0)
      (close csh-stream)
      (close error-stream)
      )))		    

#+(and :lucid (not :mswindows))
(defun call-system-pid (string args)
  (multiple-value-bind
   (csh-stream error-stream whatever pid)
   (run-unix-program (concatenate 'string "/usr/local/bin/" string) :arguments args
		     :input :stream :wait nil 
		     :error-output :stream
		     )
   (close csh-stream)
   (close error-stream)
   pid))		    

; mkaminski -- ext:run-program is not really what we need for clisp below

#+:clisp
(defun call-system (string) (ext:run-shell-command string :wait t))
;(defun call-system (string)
;  (ext:run-program string :wait t))

#+(and (or :allegro :excl) (not :mswindows))
(defun call-system (string)
"Passes STRING to a new shell. Output from the shell will go to the
standard output unless redirected by STRING.  If any error output is
generated, then 1 will be returned, else 0."
(excl:run-shell-command string
		     :wait t ))

#+:mswindows
(defun call-system (string)
  (msgf "Cannot Make A System Call For Windows: " t string)
  t)

#-(or ibcl kcl :sbcl :cmu lucid :allegro :excl :clisp)
(defun call-system (str)
  (declare (ignore str))
  (throwfail "Don't know how to make a call to the system."))

(context maint::tps-maintenance)
(defflag lisp-implementation-type
   (flagtype string)
   (default "")
   (subjects system)
   (change-fn (lambda (x y z) (declare (ignore x y z))
		      (msgf "Sorry, you aren't allowed to change this flag.")))
   (mhelp "Tells what Common Lisp we are running on.  Initialized 
when TPS starts up.  Can't be changed."))

(defflag machine-type
   (flagtype string)
   (default "")
   (subjects system)
   (pre-change-fn (lambda (x y z) 
		    (declare (ignore x))
		    (unless (equal y z)
		      (throwfail "Sorry, you aren't allowed to change this flag."))))
   (mhelp "Tells what hardware that we are running on.  Initialized 
when TPS starts up.  Can't be changed."))

(defflag short-site-name
   (flagtype string)
   (default "")
   (subjects system)
   (change-fn (lambda (x y z) (declare (ignore x y z))
		      (msgf "Sorry, you aren't allowed to change this flag.")))
   (mhelp "Tells what site we are running at.  Initialized
when TPS starts up.  Can't be changed."))   

(defflag machine-instance
   (flagtype string)
   (default "")
   (subjects system)
   (pre-change-fn (lambda (x y z)
		    (declare (ignore x))
		    (unless (equal y z)
		      (throwfail "Sorry, you aren't allowed to change this flag."))))
   (mhelp "Tells what particular machine we are running on.  Initialized
when TPS starts up.  Can't be changed."))   

(defun string-to-integer2 (string)
  (let ((nstring (string-left-trim '(#\Space) string))
	(num 0))
   (do ((i 0 (+ 1 i)))
       ((eq (schar nstring i) #\Space))
       (setq num (+ (- (char-int (schar nstring i)) 48) (* 10 num))))
   num))

#+comment(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
 ; was for (and :allegro :andrew)
  (let ((dummy (multiple-value-list (excl:run-shell-command
				     (format nil "exec xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn vtsymbol -fb vtsymbold -e ~A '~A'"
					     title title place command outfilename)
				     :wait nil)))
	(pid 0))
    (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
    (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
    (excl:run-shell-command "ps > /tmp/tmp-ps-file")
    (excl:run-shell-command (format nil "grep '~A' /tmp/tmp-ps-file > /tmp/tmp-grep-file" (concatenate 'string command " " outfilename)))
    (do () ((with-open-file (foo "/tmp/tmp-grep-file" :direction :input) (> (file-length foo) 0)))
	(if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
	(if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
	(excl:run-shell-command "ps > /tmp/tmp-ps-file")
	(excl:run-shell-command (format nil "grep '~A' /tmp/tmp-ps-file > /tmp/tmp-grep-file" (concatenate 'string command " " outfilename))))
    (setq pid (string-to-integer2 (with-open-file (ifile "/tmp/tmp-grep-file" :direction :input)
						  (read-line ifile))))
    (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
    (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
    pid))

#+comment(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
	   ;; was for (and :allegro :andrew)
  (let ((dummy (multiple-value-list (excl:run-shell-command
				     (format nil "exec xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn gallant.r.19 -fb galsymbold -e ~A '~A'"
					     title title place command outfilename)
				     :wait nil)))
	(pid 0))
    (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
    (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
    (excl:run-shell-command "ps > /tmp/tmp-ps-file")
    (excl:run-shell-command (format nil "grep '~A' /tmp/tmp-ps-file > /tmp/tmp-grep-file" (concatenate 'string command " " outfilename)))
    (do () ((with-open-file (foo "/tmp/tmp-grep-file" :direction :input) (> (file-length foo) 0)))
	(if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
	(if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
	(excl:run-shell-command "ps > /tmp/tmp-ps-file")
	(excl:run-shell-command (format nil "grep '~A' /tmp/tmp-ps-file > /tmp/tmp-grep-file" (concatenate 'string command " " outfilename))))
    (setq pid (string-to-integer2 (with-open-file (ifile "/tmp/tmp-grep-file" :direction :input)
						  (read-line ifile))))
    (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
    (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
    pid))

#+lucid
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (declare (ignore command))
  (call-system-pid "xterm" (list "+l" "-n" title "-T" title
				 "-geometry" place "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "vtsymbol"
				 "-fb" "vtsymbold" "-e" "tail" "-f" outfilename)))

#+lucid
(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (declare (ignore command))
  (call-system-pid "xterm" (list "+l" "-n" title "-T" title
				 "-geometry" place "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "gallant.r.19"
				 "-fb" "galsymbold" "-e" "tail" "-f" outfilename)))

#+:mswindows
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (declare (ignore title outfilename place command))
  "XTerms are not supported under Windows.  There should be a
way to simulate this under Windows, but it's not yet implemented."
  t)

; mkaminski - 9/16/2005
#+:clisp
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
      (with-open-file (ofile "/tmp/tps-clisp-window" :direction :output)
            (princ (format nil "xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn vtsymbol -fb vtsymbold -e ~A '~A' &"
		            title title place command outfilename) ofile)
            (terpri ofile)
            (princ "echo $!" ofile))
      (ext:run-shell-command "sh /tmp/tps-clisp-window > /tmp/tps-clisp-window-pid")
      (string-to-integer 
           (with-open-file (ifile "/tmp/tps-clisp-window-pid" :direction :input)
              (read-line ifile))))

;;#+(and :allegro (not :andrew))
#+(and :allegro (not :mswindows))
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (car (cdr (cdr (multiple-value-list
	     (excl:run-shell-command
	      (format nil "exec xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn vtsymbol -fb vtsymbold -e ~A '~A'"
		      title title place command outfilename)
	      :wait nil))))))

#+:mswindows
(defun setup-big-xterm-window (title outfilename place
				     &optional (command "tail -f -n 1000"))
  (declare (ignore title outfilename place command))
  "XTerms are not supported under Windows.  There should be a
way to simulate this under Windows, but it's not yet implemented."
  t)

; mkaminski - 9/16/2005
#+:clisp
(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
      (with-open-file (ofile "/tmp/tps-clisp-window" :direction :output)
           (princ (format nil "xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn gallant.r.19 -fb galsymbold -e ~A '~A' &"
		            title title place command outfilename) ofile)
           (terpri ofile)
           (princ "echo $!" ofile))
      (ext:run-shell-command "sh /tmp/tps-clisp-window > /tmp/tps-clisp-window-pid")
      (string-to-integer 
           (with-open-file (ifile "/tmp/tps-clisp-window-pid" :direction :input)
              (read-line ifile))))
;(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
;  (car (cdr (cdr (multiple-value-list
;	     (ext:run-shell-command
;	      (format nil "exec xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn gallant.r.19 -fb galsymbold -e ~A '~A'"
;		      title title place command outfilename)
;	      :wait nil))))))



;;#+(and :allegro (not :andrew))
#+(and :allegro (not :mswindows))
(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (car (cdr (cdr (multiple-value-list
	     (excl:run-shell-command
	      (format nil "exec xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn gallant.r.19 -fb galsymbold -e ~A '~A'"
		      title title place command outfilename)
	      :wait nil))))))


;;;The following function is used in the kcl version of setup-xterm-window
;;;because SYSTEM returns too little to get a PID.
(defun string-to-integer (nstring)
  (let ((num 0))
   (dotimes (i (length nstring) num)
     (setq num (+ (- (char-int (schar nstring i)) 48) (* 10 num))))))

#+kcl
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
      (with-open-file (ofile "/tmp/tps-ackl-window" :direction :output)
            (princ (format nil "xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn vtsymbol -fb vtsymbold -e ~A '~A' &"
		            title title place command outfilename) ofile)
            (terpri ofile)
            (princ "echo $!" ofile))
      (system "sh /tmp/tps-ackl-window > /tmp/tps-ackl-window-pid")
      (string-to-integer 
           (with-open-file (ifile "/tmp/tps-ackl-window-pid" :direction :input)
              (read-line ifile))))

#+kcl
(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
      (with-open-file (ofile "/tmp/tps-ackl-window" :direction :output)
           (princ (format nil "xterm +l -n '~A' -T '~A' -geometry '~A' -sb -xrm XTerm.vt100.showBlinkAsBold:true -fn gallant.r.19 -fb galsymbold -e ~A '~A' &"
		            title title place command outfilename) ofile)
           (terpri ofile)
           (princ "echo $!" ofile))
      (system "sh /tmp/tps-ackl-window > /tmp/tps-ackl-window-pid")
      (string-to-integer 
           (with-open-file (ifile "/tmp/tps-ackl-window-pid" :direction :input)
              (read-line ifile))))


(defun simple-parse (string)
  (let ((j 0) result)
    (dotimes (i (length string) (if (< j i) (push (subseq string j i) result)))
         (if (member (elt string i) '(#\SPACE #\NEWLINE) :test #'char=)
             (progn (if (< j i) (push (subseq string j i) result)) (setq j (1+ i)))))
    (nreverse result)))


#+(and :cmu (not :andrew))
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (ext:run-program "xterm" 
		    (concatenate 'list
		       (list "+l" "-n" title "-T" title "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "vtsymbol"
			     "-geometry" place
		             "-fb" "vtsymbold" "-e")
                       (simple-parse command) (list outfilename))
		    :env (remove-extra-displays ext:*environment-list* nil)
		    :input :stream :wait nil))

#+(and :sbcl (not :andrew))
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (sb-ext:run-program "xterm" 
		    (concatenate 'list
		       (list "+l" "-n" title "-T" title "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "vtsymbol"
			     "-geometry" place
		             "-fb" "vtsymbold" "-e")
                       (simple-parse command) (list outfilename))
		    :input :stream :wait nil))

#+(and :cmu (not :andrew))
(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (ext:run-program "xterm" 
                    (concatenate 'list
		       (list "+l" "-n" title "-T" title "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "gallant.r.19"
			     "-geometry" place
		             "-fb" "galsymbold" "-e")
                       (simple-parse command) (list outfilename))
		    :env (remove-extra-displays ext:*environment-list* nil)
		    :input :stream :wait nil))

#+(and :sbcl (not :andrew))
(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (sb-ext:run-program "xterm" 
                    (concatenate 'list
		       (list "+l" "-n" title "-T" title "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "gallant.r.19"
			     "-geometry" place
		             "-fb" "galsymbold" "-e")
                       (simple-parse command) (list outfilename))
		    :input :stream :wait nil))

#+(and :cmu :andrew)
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (let ((dummy (ext:run-program "xterm" 
		    (concatenate 'list
		       (list "+l" "-n" title "-T" title "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "vtsymbol"
			     "-geometry" place
		             "-fb" "vtsymbold" "-e")
                       (simple-parse command) (list outfilename))
		    :env (remove-extra-displays ext:*environment-list* nil)
		    :input :stream :wait nil))
	(pid 0))
  (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
  (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
  (ext:run-program "ps" nil :input :stream :output "/tmp/tmp-ps-file")
  (ext:run-program "grep" (list (concatenate 'string command " " outfilename)) :input "/tmp/tmp-ps-file" :output "/tmp/tmp-grep-file")
  (do () ((with-open-file (foo "/tmp/tmp-grep-file" :direction :input) (> (file-length foo) 0)))
      (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
      (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
      (ext:run-program "ps" nil :input :stream :output "/tmp/tmp-ps-file")
      (ext:run-program "grep" (list (concatenate 'string command " " outfilename)) :input "/tmp/tmp-ps-file" :output "/tmp/tmp-grep-file"))
  (setq pid (string-to-integer2 (with-open-file (ifile "/tmp/tmp-grep-file" :direction :input)
						(read-line ifile))))
  (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
  (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
  pid))

#+(and :sbcl :andrew)
(defun setup-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (let ((dummy (sb-ext:run-program "xterm" 
		    (concatenate 'list
		       (list "+l" "-n" title "-T" title "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "vtsymbol"
			     "-geometry" place
		             "-fb" "vtsymbold" "-e")
                       (simple-parse command) (list outfilename))
		    :input :stream :wait nil))
	(pid 0))
  (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
  (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
  (sb-ext:run-program "ps" nil :input :stream :output "/tmp/tmp-ps-file")
  (sb-ext:run-program "grep" (list (concatenate 'string command " " outfilename)) :input "/tmp/tmp-ps-file" :output "/tmp/tmp-grep-file")
  (do () ((with-open-file (foo "/tmp/tmp-grep-file" :direction :input) (> (file-length foo) 0)))
      (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
      (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
      (sb-ext:run-program "ps" nil :input :stream :output "/tmp/tmp-ps-file")
      (sb-ext:run-program "grep" (list (concatenate 'string command " " outfilename)) :input "/tmp/tmp-ps-file" :output "/tmp/tmp-grep-file"))
  (setq pid (string-to-integer2 (with-open-file (ifile "/tmp/tmp-grep-file" :direction :input)
						(read-line ifile))))
  (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
  (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
  pid))

#+(and :cmu :andrew)
(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (let ((dummy (ext:run-program "xterm" 
		    (concatenate 'list
		       (list "+l" "-n" title "-T" title "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "gallant.r.19"
			     "-geometry" place
		             "-fb" "galsymbold" "-e")
                       (simple-parse command) (list outfilename))
		    :env (remove-extra-displays ext:*environment-list* nil)
		    :input :stream :wait nil))
	(pid 0))
  (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
  (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
  (ext:run-program "ps" nil :input :stream :output "/tmp/tmp-ps-file")
  (ext:run-program "grep" (list (concatenate 'string command " " outfilename)) :input "/tmp/tmp-ps-file" :output "/tmp/tmp-grep-file")
  (do () ((with-open-file (foo "/tmp/tmp-grep-file" :direction :input) (> (file-length foo) 0)))
      (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
      (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
      (ext:run-program "ps" nil :input :stream :output "/tmp/tmp-ps-file")
      (ext:run-program "grep" (list (concatenate 'string command " " outfilename)) :input "/tmp/tmp-ps-file" :output "/tmp/tmp-grep-file"))
  (setq pid (string-to-integer2 (with-open-file (ifile "/tmp/tmp-grep-file" :direction :input)
						(read-line ifile))))
  (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
  (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
  pid))

#+(and :sbcl :andrew)
(defun setup-big-xterm-window (title outfilename place &optional (command "tail -f -n 1000"))
  (let ((dummy (sb-ext:run-program "xterm" 
		    (concatenate 'list
		       (list "+l" "-n" title "-T" title "-sb" "-xrm" "XTerm.vt100.showBlinkAsBold:true" "-fn" "gallant.r.19"
			     "-geometry" place
		             "-fb" "galsymbold" "-e")
                       (simple-parse command) (list outfilename))
		    :input :stream :wait nil))
	(pid 0))
  (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
  (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
  (sb-ext:run-program "ps" nil :input :stream :output "/tmp/tmp-ps-file")
  (sb-ext:run-program "grep" (list (concatenate 'string command " " outfilename)) :input "/tmp/tmp-ps-file" :output "/tmp/tmp-grep-file")
  (do () ((with-open-file (foo "/tmp/tmp-grep-file" :direction :input) (> (file-length foo) 0)))
      (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
      (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
      (sb-ext:run-program "ps" nil :input :stream :output "/tmp/tmp-ps-file")
      (sb-ext:run-program "grep" (list (concatenate 'string command " " outfilename)) :input "/tmp/tmp-ps-file" :output "/tmp/tmp-grep-file"))
  (setq pid (string-to-integer2 (with-open-file (ifile "/tmp/tmp-grep-file" :direction :input)
						(read-line ifile))))
  (if (probe-file "/tmp/tmp-grep-file") (delete-file "/tmp/tmp-grep-file"))
  (if (probe-file "/tmp/tmp-ps-file") (delete-file "/tmp/tmp-ps-file"))
  pid))

#+:cmu
(defun remove-extra-displays (envlist flag)
  (if (null envlist)
    nil
    (if flag 
	(if (equal (car (car envlist)) ':DISPLAY)
	    (remove-extra-displays (cdr envlist) t)
	  (cons (car envlist) (remove-extra-displays (cdr envlist) t)))
      (if (equal (car (car envlist)) ':DISPLAY)
	  (cons (car envlist) (remove-extra-displays (cdr envlist) t))
	(cons (car envlist) (remove-extra-displays (cdr envlist) nil))))))

;;; A brief explanation : CMU lisp, when it compiles, seems to attach the 
;;; name of the machine on which it was compiled to the ext:*environment-list*
;;; list, as :DISPLAY. This means that any new windows that are started will
;;; appear on the machine on which TPS was compiled, rather than the one
;;; from which it is being run. The remove-extra-displays function is an
;;; (inefficient) attempt to remove this spurious :DISPLAY ; as long as the
;;; user's DISPLAY is set correctly on login, this should do the trick.


(defun kill-xterm-window (window)
  #+:mswindows (declare (ignore window))
  #+(and :cmu (not :andrew)) (ext:run-program "kill" (list "-9" (princ-to-string (ext:process-pid window))))
  #+(and :cmu :andrew) (ext:run-program "kill" (list "-9" (princ-to-string window)))
  #+(and :sbcl (not :andrew)) (sb-ext:run-program "kill" (list "-9" (princ-to-string (sb-ext:process-pid window))))
  #+(and :sbcl :andrew) (sb-ext:run-program "kill" (list "-9" (princ-to-string window)))
  #+(and :allegro (not :mswindows))(excl:run-shell-command (format nil "kill -9 '~A'" window))
  #+clisp (progn (ext:run-shell-command (format nil "kill -9 '~A'" window))
		 (if (probe-file "/tmp/tps-clisp-window") (delete-file "/tmp/tps-clisp-window"))
		 (if (probe-file "/tmp/tps-clisp-window-pid") (delete-file "/tmp/tps-clisp-window-pid")))
  #+lucid (run-unix-program "kill" :arguments (list "-9" (princ-to-string window)))
  #+kcl (system (format nil "kill -9 '~A'" window))
  nil)

#+allegro
(eval-when (compile load eval)
(defun tps-get-internal-run-time ()
  (let ((hxtimes (multiple-value-list (excl::get-internal-run-times))))
        (+ (car hxtimes) (cadr hxtimes)))))

#-allegro
(eval-when (compile load eval)
(defun tps-get-internal-run-time ()
  (get-internal-run-time)))

