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

;;; The following bootstrap the package-facilities

;;; Need to use strings right below, so Lisp doesn't make a symbol CORE in
;;; the USER package which will later conflict with the symol CORE:CORE.

#+:cmu (ext:unlock-all-packages)
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
(make-package "AUTO" :use #-:sbcl '("LISP" 
				    "CORE" #+(and (or kcl ibcl lucid)
						 (not :lcl3.0))  "SYSTEM"
				   #+:lcl3.0 "LUCID-COMMON-LISP"
				   #+:cmu "EXTENSIONS" #+:cmu "DEBUG"
                                   #+(or :allegro :excl) "EXCL"
                                   #+(or :allegro :excl) "FRANZ")
                          #+:sbcl (package-use-list "CL-USER")
)
(make-package "ML" :use (cons (find-package "AUTO")
			      (package-use-list (find-package "AUTO")))
)
(make-package "TEACHER" :use (package-use-list (find-package "AUTO")))
#+lucid(let ((windows-pack (find-package :windows)))
	 (when (and windows-pack 
		    (member windows-pack (package-use-list 
					  (find-package :cl-user))))
	   (unuse-package windows-pack (find-package :cl-user))))
#-:allegro-v4.3(let ((flavors-pack (find-package :flavors)))
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

;; Set this to a list of files which you don't want compiled
(setq *source-files-only* nil)

(load "../tps3.sys" :verbose t)

;; Change directories or extensions if necessary

(let ((compiled-file-name (concatenate 'string sys-dir "bin/boot0." compiled-extension)))
  (compile-file "../lisp/boot0.lisp" :output-file compiled-file-name)
  (load compiled-file-name :verbose t))

(let ((compiled-file-name (concatenate 'string sys-dir "bin/boot1." compiled-extension)))
  (compile-file "../lisp/boot1.lisp" :output-file compiled-file-name)
  (load compiled-file-name :verbose t))

(let ((compiled-file-name (concatenate 'string sys-dir "bin/defpck." compiled-extension)))
  (compile-file "../lisp/defpck.lisp" :output-file compiled-file-name)
  (load compiled-file-name :verbose t))

(setf (module-loaded-p 'bootstrap) t)

(setq *always-compile-source-if-newer* t)


(cload-module 'tpsdef)
(cload-module 'bare)			; Loads everything but the
					; above four files.

(dolist (module 
	    '(tps-modules
	      maintain 
	      tps-help 
	      ;; Comment next line if you don't run X windows
	      xwindows 
	      review-flags
	      environment 
	      event-signal 
	      wffs 
	      wff-print 
	      wff-parse
	      auto-doc
	      wff-ops-abb
	      wff-ops1
	      wff-ops2
	      wff-editor
	      tex-wff
	      logic-scribe 
	      scribe-wff 
	      sail-wff	
	      weak-label
	      save-wffs
	      lambda-calc
	      jforms
	      vpforms
	      auto-basic
	      mating
	      expansion-tree
	      events
	      unification
	      unification-interface
	      skolemizing
	      save-tps-work 
	      primitive-subst
	      ms88 
;;	      development-seqs 
	      tps2-rulep 
	      concept-bare 
	      concept-wff 
	      metawffs
	      wffmatch 
	      ops-otlrules 
	      otlrules
	      otlnl 
	      otlrulep 
	      otlsuggest 
	      otlcleanup 
	      read-rules
	      otlhelp 
	      otlschema2
	      otlscribe 
	      replace 
	      otladvice 
	      otlgo
	      theorems
	      rules			; Include for now.
	      mode-ml 
	      tactics 
	      mating-transform 
	      etr-nat
	      ext-dags
	      semantics
	      grader-top 
	      grader  
              external-services
	      s-eqn
))
  (cload-module module))

(in-mode ml (cload-module 'math-logic-2-wffs))

(in-mode rules
  (in-mode ml (dolist (module '(math-logic-2-rules 
				ml-tactics
				ml-etr-tactics
				tactics-nd))
		(cload-module module))))

(in-mode ml (cload-module 'math-logic-2-exercises))


; Added 24MAY90 DAN
(dolist (module '( ;; Uncomment the following lines if you want etps stuff
		   ;; etps-events 
		   ;; report
		   ms89 
		   ms90-3
		   ms90-9
                   ms91
		   mst
		   library
		   rrules
		   ms98
))
  (cload-module module))

(in-mode rules
	 (in-mode ml (cload-module 'ml2-rewrite)))

(setq *always-compile-source-if-newer* nil)



