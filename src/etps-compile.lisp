
;;; The following bootstrap the package-facilities

;;; Need to use strings right below, so Lisp doesn't make a symbol CORE in
;;; the USER package which will later conflict with the symol CORE:CORE.

#+:sbcl (dolist (package (list-all-packages))
	  (sb-ext:unlock-package package))

(make-package "CORE" :use #-:sbcl '("LISP" #-:lcl3.0 "SYSTEM" 
				    #+:lcl3.0 "LUCID-COMMON-LISP"
				    #+:cmu "EXTENSIONS" #+:cmu "DEBUG" 
				    #+(or :allegro :excl) "EXCL"
				    #+(or :allegro :excl) "FRANZ")
	                  #+:sbcl (package-use-list "CL-USER"))
(in-package "CORE")
(use-package '("CORE") (find-package "CL-USER"))

(make-package "MAINT" :use (cons (find-package "CORE")
				 (package-use-list (find-package "CORE"))))
(make-package "AUTO" :use #-:sbcl '("LISP" "CORE" #+(and (or kcl ibcl lucid)
							 (not :lcl3.0))  "SYSTEM"
						 #+:lcl3.0 "LUCID-COMMON-LISP"	 
						 #+:cmu "EXTENSIONS" #+:cmu "DEBUG"
						 #+(or :allegro :excl) "EXCL"
						 #+(or :allegro :excl) "FRANZ")
	                  #+:sbcl (package-use-list "CL-USER"))

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

(load "../lisp/special.exp" :verbose t)
(load "../lisp/core.exp" :verbose t)
(load "../lisp/maint.exp" :verbose t)
(load "../lisp/auto.exp" :verbose t)
(load "../lisp/ml.exp" :verbose t)
(load "../lisp/teacher.exp" :verbose t)

(use-package '("ML") (find-package "CL-USER"))

(use-package '("MAINT") (find-package "TEACHER"))


(setq *source-files-only* '())

(load "../etps.sys" :verbose t)

;; Change directories or extensions if necessary
(compile-file "../lisp/boot0.lisp" :output-file (concatenate 'string "../bin/boot0." compiled-extension))
(load (concatenate 'string "../bin/boot0." compiled-extension) :verbose t)
(compile-file "../lisp/boot1.lisp" :output-file (concatenate 'string "../bin/boot1." compiled-extension))
(load (concatenate 'string "../bin/boot1." compiled-extension) :verbose t)
(compile-file "../lisp/defpck.lisp" :output-file (concatenate 'string "../bin/defpck." compiled-extension))
(load (concatenate 'string "../bin/defpck." compiled-extension) :verbose t)

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
   external-interface
   ;;; rules				; Include for now.
   ;;auto-doc tactics
   )) 

(cload-modules '(mode-ml))

(in-mode ml (cload-modules '(math-logic-2-wffs)))

(in-mode rules
  (in-mode ml (cload-modules '(math-logic-2-rules ;;ml-tactics
))))
(in-mode ml (cload-modules '(math-logic-2-exercises)))


(cload-modules '(events etps-events)) ;; report 

;; REMOVE XWINDOWS IF YOU CAN'T RUN X WINDOWS
;(cload-modules '(xwindows library))
(cload-modules '(xwindows))
(cload-modules '(grader-top grader))

