;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :maint)
(part-of RULES)

(deffile rule-cmds
  (part-of rules)
  (extension clisp)
  (mhelp "Defines some commands, argument types etc. which are useful when
running the RULES module."))

(context rule-run)

(defmexpr assemble-mod
  (argtypes tps-module)
  (argnames module)
  (arghelp "Module with the rules files")
  (mhelp "Produce a file with rule commands for every rule file in a module."))


;;; Changed to make sure that "rule-file" doesn't contain a package name, 
;;; since the module is defined in a different package -- 7/28/87 DAN

(defun assemble-mod (module)
  (unless (module-loaded-p module)
    (load-macro-files module))
  (dolist (rule-file (module-files module))
    (assemble-file (string-downcase (symbol-name rule-file)) module)))

(defmexpr build
  (argtypes rule)
  (argnames rule)
  (arghelp "Rule to be processed")
  (mhelp "Process a rule without writing the resulting code to a file."))

(defun build (rule)
  (build-arglist rule)
  (build-rule-build rule)
  (build-rule-enter rule)
  (build-rule-defaults rule)
  (build-rule-hyp-defaults rule)
  (build-rule-check rule)
  (build-rule-hyp-checks rule)
  (build-rule-wff-checks rule)
  (when build-match 
    (build-rule-match rule)
    (build-rule-match1 rule)
    (build-rule-short rule)))


(defun write-rule-file (rule)
  (let ((*print-case* :downcase)) 
    (write-rule-cmd rule)
    (write-rule-wffs rule)
    (when build-match (write-rule-match rule))
    (write-rule-fns rule)))

(defun write-rule-cmd (rule)
  (msgf ";;; The rule command definition." t t)
  (msg "(defmexpr " rule
       t (t 2) (e (prin1 (cons 'argtypes (get rule 'all-argtypes))))
       t (t 2) "(wffargtypes ")
  (dolist (types (get rule 'all-wffargtypelist))
    (msg " " (types . typesym-nil)))
  (msg ")" (t 2) "(wffop-typelist")
  (dolist (type (get rule 'all-wffop-typelist))
    (msg " " (type . typesym)))
  (msg ")"
       t (t 2) (e (prin1 (cons 'argnames (get rule 'argnames))))
       t (t 2) (e (prin1 (cons 'arghelp (get rule 'all-arghelps))))
       t (t 2) "(defaultfns " (e (prin1 (cadr (get rule 'default-fn-defn))))
       ")"
       t (t 2) "(mainfns " (e (prin1 (cadr (get rule 'check-fn-defn))))
       " " (e (prin1 (cadr (get rule 'rule-build-fn))))
       ")"
       t (t 2) "(enterfns " (e (prin1 (cadr (get rule 'rule-enter-fn))))
       ")")
  (when (assoc 'iruledef (get rule 'mhelp))
    (msg t (t 2) "(mhelp " (e (prin1 (cdr (assoc 'iruledef
						 (get rule 'mhelp)))))
	 ")"))
  (msg ")" -2))


(defun write-rule-match (rule)
  (msgf ";;; The suggesting rule definition." t t)
  (msg "(defsrule " rule
       ;; For the moment we don't write the priority - it is in a
       ;; separate file.
       ;; t (t 2) "(priority " (get rule 'priority) ")"
       t (t 2) "(matchfn " (e (prin1 (cadr (get rule 'match-fn-defn)))) ")"
       t (t 2) "(match1fn " (e (prin1 (cadr (get rule 'match1-fn-defn)))) ")"
       t (t 2) "(shortfn " (e (prin1 (cadr (get rule 'short-fn-defn)))) ")"
       ")" t t))

(defun write-rule-wffs (rule)
  (msgf ";;; The line assertions justifications and restrictions" t t)
  (in-mode re-read
	   (msg "(defrulewffs " rule t (t 2) "(unique-lines ")
	   (do ((u-lines (get rule 'unique-linenames) (cdr u-lines))
		(lines (get rule 'lines))
		(meta-lines (get rule 'm-line-args) (cdr meta-lines)))
	       ((null u-lines))
	     (msg t (t 5) "(" (car u-lines) " "
		  ((cadddr (assoc (caar meta-lines) lines)) . gwff) ")"))
	   (msg ")" t (t 2) "(unique-restrictions ")
	   (do ((u-restr (get rule 'unique-restrictions) (cdr u-restr))
		(restrictions (get rule 'restrictions) (cdr restrictions)))
	       ((null u-restr))
	     (msg t (t 5) "(" (car u-restr) " (" (caar restrictions))
	     (do ((restr-wffs (cdar restrictions) (cdr restr-wffs)))
		 ((null restr-wffs))
	       (msg " " ((car restr-wffs) . gwff)))
	     (msg "))"))
	   (msg ")")
	   ;; Unique justifications must go in here, if defined.
	   (msg ")" -2)))

(defmacro write-rule-fn (fn-comment fn-defn)
  `(msg t ,fn-comment -2 (e (pprint ,fn-defn)) t))


(defun write-rule-fns (rule)
  (write-rule-fn ";;; The building function. "
		 (get rule 'rule-build-fn))
  (remprop rule 'rule-build-fn)
  (write-rule-fn ";;; The entering function. "
		 (get rule 'rule-enter-fn))
  (remprop rule 'rule-enter-fn)
  (write-rule-fn ";;; The default function. "
		 (get rule 'default-fn-defn))
  (remprop rule 'default-fn-defn)
  (write-rule-fn ";;; The hypotheses default function. "
		 (get rule 'hyp-default-fn-defn))
  (remprop rule 'hyp-default-fn-defn)
  (when build-match
	(write-rule-fn ";;; The matching function. "
		       (get rule 'match-fn-defn))
	(remprop rule 'match-fn-defn)
	(write-rule-fn ";;; The new matching function.  "
		       (get rule 'match1-fn-defn))
	(remprop rule 'match1-fn-defn)
	(write-rule-fn ";;; The short version of the rule as a function.  "
		       (get rule 'short-fn-defn))
	(remprop rule 'short-fn-defn))
  (write-rule-fn ";;; The checking function. "
		 (get rule 'check-fn-defn))
  (remprop rule 'check-fn-defn)
  (write-rule-fn ";;; The hypotheses checking function. "
		 (get rule 'hyp-check-fn-defn))
  (remprop rule 'hyp-check-fn-defn)
  (write-rule-fn ";;; The restriction checking function. "
		 (get rule 'wff-check-fn))
  (remprop rule 'wff-check-fn))
  
(defmexpr assemble-file
  (argtypes filespec symbol)
  (argnames rule-file part-of)
  (arghelp "Rule source file to be assembled" "Module the file is part of")
  (defaultfns
    (lambda (rule-file part-of)
      (when (eq part-of '$)
	    (setq part-of (if build-match 'otlsuggest 'otlrules)))
      (list rule-file part-of)))
  (mhelp "Parse, build and write every rule in a given rule file.
Be sure to set the correct mode (MODE RULES) before using this command."))

(defun assemble-file (rule-file part-of)
  (let* ((rule-file-ext (merge-pathnames (pathname-name
					  (parse-namestring rule-file))
					 (make-pathname :directory nil 
							:type "rules")))
	 (rule-file-probed (locate-tps-file rule-file-ext t))
	 assm-file new-irules)
    (unless rule-file-probed
      (throwfail "No rule file " (rule-file-ext . filespec)
		 " found."))
    (setq assm-file
	  (merge-pathnames 
	    (make-pathname% :type source-extension)
	    rule-file-probed))
    (let ((old-irulelist global-irulelist)
	  (global-irulelist nil)
	  (current-context 'rule-commands)) ;default context for rules.
      (declare (special current-context))
      (in-mode rules (load rule-file-probed))
      (setq new-irules global-irulelist
	    global-irulelist
	    (delete-duplicates (append global-irulelist old-irulelist))))
    (reroute-output assm-file *default-pathname-defaults*
      (msg ";;; -*- Mode:LISP; Package:" (or (get part-of 'lisp-pack) "ML") " -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "
	   ((namestring (pathname *standard-output*)) . filespec) t
	   ";;;  assembled from " (rule-file-probed . filespec) t
	   ";;;" t ";;; contains rules" t
	   ";;; " (l new-irules) t ";;;" -2
	   "(in-package :" 
	   (or (get part-of 'lisp-pack) "ML")
	   ")" t
	   "(part-of " part-of ")" -2
	   "(defrulefile " (e (princ (string (pathname-name rule-file)))) 
	   ;; changed so that if user specifies directory name,
	   ;;it won't show up here 7/29/87 DAN
	   t (t 2) "(contents " (l new-irules) "))" -2
	   "(context rule-commands)" t)
      (let ((curr-rule-context 'rule-commands))
	(dolist (rule new-irules)
	  (let ((rule-context (cdr (assoc 'iruledef
					  (get rule 'core::contexts)))))
	    (unless (eq curr-rule-context rule-context)
	      (msg t "(context " rule-context ")" t)
	      (setq curr-rule-context rule-context))
	    (msg -2 ";;;" t ";;; Rule: " rule t ";;;" -2)
	    (format *error-output* "~A " rule)
	    (build rule)
	    (write-rule-file rule)))))))

(defmexpr write-rule
  (argtypes rule filespec)
  (argnames rule filename)
  (arghelp "Rule to Write" "File to Write Rule to")
  (defaultfns (lambda (rule filename)
		(cond ((eq filename '$)
		       (when (not (eq rule '$))
			     (setq filename
				   (namestring
				    (make-pathname% :name
						     (string-downcase
						       (symbol-name rule))
						   :type source-extension)
				    )))))
		(list rule filename)))
  (mhelp "Write the various functions and definitions for a rule into a file.")
)

(defun write-rule (rule filename)
  (reroute-output filename (make-pathname% :name rule :type source-extension)
    (write-rule-file rule)))
