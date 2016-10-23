;;; -*- Mode:LISP; Package:CORE -*-
;;; Last Modified: Sun Feb 28 01:36:58 1988
;;; ******************************************************************* ;;;
;;;         This code was written as part of the TPS project at         ;;;
;;;                     Carnegie Mellon University.                     ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :cl-user) ;;;to get rid of warning from LUCID CL

;;;Note that always the most recent available compiled version is
;;; loaded.

;;; first pick up an argument giving TPS root directory
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

(setq tps-dir NIL)
(when (and (stringp (cadr args))
	   (probe-file (concatenate 'string (cadr args) "tps3.sys")))
  (setq tps-dir (cadr args)))

; FIX THE ABSOLUTE DIRECTORY IF BUILDING MANUALLY
(unless tps-dir
  (setq tps-dir "C:\\Program Files\\TPS\\"))

;;; The following bootstrap the package-facilities

;;; Need to use strings right below, so Lisp doesn't make a symbol CORE in
;;; the USER package which will later conflict with the symol CORE:CORE.


(make-package "CORE" :use '("LISP" #-:lcl3.0 "SYSTEM" 
			    #+:lcl3.0 "LUCID-COMMON-LISP"
			    #+:cmu "EXTENSIONS" #+:cmu "DEBUG" 
			    #+(or :allegro :excl) "EXCL"
                            #+(or :allegro :excl) "FRANZ") )
(in-package "CORE")
(use-package '("CORE") (find-package "CL-USER"))

(make-package "MAINT" :use (cons (find-package "CORE")
				 (package-use-list (find-package "CORE"))))
(make-package "AUTO" :use '("LISP" "CORE" #+(and (or kcl ibcl lucid)
						 (not :lcl3.0))  "SYSTEM"
				   #+:lcl3.0 "LUCID-COMMON-LISP"	 
				   #+:cmu "EXTENSIONS" #+:cmu "DEBUG"
                                   #+(or :allegro :excl) "EXCL"
                                   #+(or :allegro :excl) "FRANZ"
))
(make-package "ML" :use (cons (find-package "AUTO")
			      (package-use-list (find-package "AUTO")))
)
(make-package "TEACHER" :use (package-use-list (find-package "AUTO")))
#+lucid(let ((windows-pack (find-package :windows)))
	 (when (and windows-pack 
		    (member windows-pack (package-use-list 
					  (find-package :cl-user))))
	   (unuse-package windows-pack (find-package :cl-user))))
(let ((flavors-pack (find-package :flavors)))
  (when (and flavors-pack 
	     (member flavors-pack (package-use-list 
				   (find-package :cl-user))))
    (unuse-package flavors-pack (find-package :cl-user))))
	
;;; CHANGE FILENAMES HERE IF INCORRECT

(load (concatenate 'string cl-user::tps-dir "lisp\\special.exp") :verbose t)
(load (concatenate 'string cl-user::tps-dir "lisp\\core.exp") :verbose t)
(load (concatenate 'string cl-user::tps-dir "lisp\\maint.exp") :verbose t)
(load (concatenate 'string cl-user::tps-dir "lisp\\auto.exp") :verbose t)
(load (concatenate 'string cl-user::tps-dir "lisp\\ml.exp") :verbose t)
(load (concatenate 'string cl-user::tps-dir "lisp\\teacher.exp") :verbose t)

(use-package '("ML") (find-package "CL-USER"))

(use-package '("MAINT") (find-package "TEACHER"))


(setq *source-files-only* '())

(load (concatenate 'string cl-user::tps-dir "etps.sys") :verbose t)

;; Change directories or extensions if necessary

(let ((compiled-file-name (concatenate 'string cl-user::tps-dir "bin\\boot0." compiled-extension)))
  (compile-file (concatenate 'string cl-user::tps-dir "lisp\\boot0.lisp") :output-file compiled-file-name)
  (load compiled-file-name :verbose t))

(let ((compiled-file-name (concatenate 'string cl-user::tps-dir "bin\\boot1." compiled-extension)))
  (compile-file (concatenate 'string cl-user::tps-dir "lisp\\boot1.lisp") :output-file compiled-file-name)
  (load compiled-file-name :verbose t))

(let ((compiled-file-name (concatenate 'string cl-user::tps-dir "bin\\defpck." compiled-extension)))
  (compile-file (concatenate 'string cl-user::tps-dir "lisp\\defpck.lisp")
		:output-file compiled-file-name)
  (load compiled-file-name :verbose t))

(setf (module-loaded-p 'bootstrap) t)

(setq core::*always-compile-source-if-newer* t)

(cload-module 'tpsdef )
(cload-module 'bare )			; Loads everything but the
					; above four files.

(cload-modules
 '(tps-modules
   maintain tps-help environment))
(cload-modules '( wffs wff-print wff-parse   
   concept-bare concept-wff 
   tex-wff 
   wff-ops-abb wff-ops1 wff-ops2
   wff-editor weak-label save-wffs lambda-calc 
   ;; expansion-tree
   ;; skolemizing development-seqs 
   jforms 
   ;;vpforms 
   tps2-rulep 
   ;; primitive-subst
   review-flags save-tps-work saving-modes
   logic-scribe scribe-wff sail-wff	
   event-signal  metawffs wffmatch ops-otlrules otlrules
   otlnl otlrulep otlsuggest otlcleanup read-rules
   otlhelp otlschema2 otlscribe replace otladvice 
   ;; otlgo
   theorems
   ;;; rules				; Include for now.
   ;;auto-doc tactics
   external-interface
   )) 

(cload-modules '(mode-ml))

(in-mode ml (cload-modules '(math-logic-2-wffs)))

(in-mode rules
  (in-mode ml (cload-modules '(math-logic-2-rules ;;ml-tactics
))))
(in-mode ml (cload-modules '(math-logic-2-exercises)))


(cload-modules '(events etps-events)) ;; report 

;; REMOVE XWINDOWS IF YOU CAN'T RUN X WINDOWS OR OTHER INTERFACE
;(cload-modules '(xwindows library))
(cload-modules '(xwindows)) 
(cload-modules '(grader-top grader))

(core::exit-from-lisp)
