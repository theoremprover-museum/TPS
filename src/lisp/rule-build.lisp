;;; -*- Mode:LISP; Package:MAINT -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :MAINT)
(part-of RULES)

;;;
;;; File: Rule-Build
;;; Package: Rules
;;;
;;; contains functions building the rule command from the intermediate
;;; rule definition.
;;;


(deffile rule-build
  (part-of rules)
  (extension clisp)
  (mhelp "Contains functions building the rule command from the intermediate
rule definition."))


;;; BUILD-ARGLIST will take an intermediate rule definition and build
;;; the list of argument types, argument names, argument helps for
;;; the rule command.

(defun build-arglist (rule)
  (let ((sorted-lines (get rule 'lines))
	(support-transformation (get rule 'support-transformation))
	;;(wffconstlist (get rule 'wffconsts))
	(propsymalist nil)
	(dlines nil) (plines nil) (hlines nil)
	all-args all-argtypes all-wffargtypelist all-arghelps
	;line-args 
	ex-line-args new-line-args
	m-line-args ex-m-line-args new-m-line-args
	ex-lineargnames new-lineargnames
	wffarglist wffargtypelist all-wffop-typelist
	old-wff-propsym new-wff-arglist
	hypargnames unique-linenames unique-restrictions
	true-lineargnames multi-default-lines)
    (declare (special wffconstlist propsymalist))
    ;; Sort out the hlines, dlines, plines
    (dolist (sline sorted-lines)
      (let ((line-arg (car sline)))
	(cond ((member line-arg (nth 2 sline)) (push line-arg hlines))
	      ((null (nth 4 sline)) (push line-arg dlines))
	      (t (push line-arg plines)))))
    (putprop rule hlines 'hlines)
    (putprop rule dlines 'dlines)
    (putprop rule plines 'plines)
    ;; Start by establishing the order for the d, h, and plines using
    ;; the support-transformation argument.
    (let ((lddir (do ((ddir (apply #'append
				   (car support-transformation))
			    (cdr ddir))
		      (lddir nil
			     (let ((full-line (assoc (car ddir) sorted-lines)))
			       (if full-line (cons full-line lddir) lddir))))
		     ((null ddir) (nreverse lddir)))))
      (setq ex-line-args lddir)
      (setq new-line-args (set-difference sorted-lines lddir)))
    ;; Now we replace the wffs by meta-wffs, also replacing
    ;; hypothesis wffsets by a unique name for them.  If a wffop
    ;; is encountered, extra arguments for the inverse operation
    ;; are pushed onto a special list INVERSE-EXTRA-ARGS
    (let ((meta-var-name (prepend rule '-mv))
	  (meta-bdvar-name (prepend rule '-bd))
	  (meta-label-name (prepend rule '-ml)))
      (declare (special meta-var-name meta-bdvar-name meta-label-name))
      (reset-name-counter meta-var-name)
      (reset-name-counter meta-bdvar-name)
      (reset-name-counter meta-label-name)
      (do ((ex-line-args ex-line-args (cdr ex-line-args))
	   (meta-ex-line-args nil (cons (meta-ize (car ex-line-args))
				     meta-ex-line-args)))
	  ((null ex-line-args)
	   (setq ex-m-line-args (nreverse meta-ex-line-args))))
      (setq old-wff-propsym (copy-list propsymalist))
      (do ((new-line-args new-line-args (cdr new-line-args))
	   (meta-new-line-args nil (cons (meta-ize (car new-line-args))
				     meta-new-line-args)))
	  ((null new-line-args)
	   (setq new-m-line-args (nreverse meta-new-line-args))))
      ;(setq line-args (append ex-line-args new-line-args))
      (setq m-line-args (append ex-m-line-args new-m-line-args))
      (do ((mline-args m-line-args (cdr mline-args))
	   (meta-line-args
	    nil (cons (meta-ize-justifications (car mline-args))
		      meta-line-args)))
	  ((null mline-args)
	   (setq m-line-args (nreverse meta-line-args))))
      )
    ;; Now we generate names for the wff args to the rule, also
    ;; generating possible extra arguments for an inverse application
    ;; of a rule.  If no such inverse arguments have been defined
    ;; for the wffop (like for LNORM), we simply ask for the full
    ;; wff, then check wether its legal when the the original wffop
    ;; is applied to it.
    (do ((pa propsymalist (cdr pa))
	 (push-flag nil (and (not (member (car pa) old-wff-propsym))
			     (not (eq (get (cdar pa) 'flavor) 'meta))))
	 (wffargs nil (cons (generate-arg-name (car pa)) wffargs))
	 (wffargtypes nil (cons (type (cdar pa)) wffargtypes)))
	((null pa)
	 (when push-flag (push (car wffargs) new-wff-arglist))
	 (setq wffarglist (nreverse wffargs))
	 (setq wffargtypelist (nreverse wffargtypes))
	 (setq new-wff-arglist (nreverse new-wff-arglist)))
      (when push-flag (push (car wffargs) new-wff-arglist)))
    ;;(do ((wffarglist wffarglist (cdr wffarglist))
	;; (tw nil
	  ;;   (if (symbolp (caar wffarglist))
		;; (cons (caar wffarglist) tw)
		;; (nconc (nreverse (mapcar #'car (car wffarglist))) tw)))
	 ;;(tt nil
	     ;;(if (symbolp (caar wffarglist))
		 ;;(cons (cdar wffarglist) tt)
		 ;;(nconc (nreverse (mapcar #'cdr (car wffarglist))) tt))))
	;;((null wffarglist)
	 ;;(setq true-wffarglist (nreverse tw))
	 ;;(setq true-wffargtypelist (nreverse tt))))
    ;; Next we generate the list of arguments for the hypotheses sets
    ;; which are not constants.
    ;; Now we generate the complete list of arguments.
    (setq true-lineargnames (mapcar #'car m-line-args))
    (setq ex-lineargnames (mapcar #'car ex-m-line-args))
    (setq new-lineargnames (mapcar #'car new-m-line-args))
    (setq multi-default-lines
	  (set-of larg true-lineargnames
	    (and (not (exists def-ex (car support-transformation)
			      (member larg def-ex)))
		 (> (length (set-of def-new (cadr support-transformation)
			      (member larg def-new)))
		    1))))
    (setq hypargnames (mapcar #'gen-hypargname true-lineargnames))
    (setq unique-restrictions
	  (do ((restrs (get rule 'restrictions) (cdr restrs))
	       (i 0 (+ i 1))
	       (unique-restrs nil (cons (prepend rule '-restr (format nil "~A"
								      i))
					unique-restrs)))
	      ((null restrs) (nreverse unique-restrs))))
    (setq unique-linenames (mapcar #'(lambda (linename)
				       (prepend rule '- linename))
				   true-lineargnames))
    (setq all-args (append true-lineargnames
			   wffarglist
			   hypargnames))
    (setq all-argtypes
	  (append (mapcar #'(lambda (line)
			      (declare (ignore line)) 'line)
			  m-line-args)
		  (mapcar #'(lambda (wffarg)
			      (declare (ignore wffarg)) 'gwff)
			  wffarglist)
		  (mapcar #'(lambda (wffset)
			      (declare (ignore wffset)) 'linelist)
			  hypargnames)))
    (setq all-wffargtypelist
	  (append (mapcar #'nil-const true-lineargnames)
		  wffargtypelist
		  (mapcar #'nil-const hypargnames)))
    (setq all-wffop-typelist (find-type-vars wffargtypelist))
    (let ((itemshelp (get rule 'itemshelp)))
      (setq all-arghelps
	  (append (find-line-helps itemshelp m-line-args)
		  (find-wff-helps itemshelp wffarglist)
		  (find-hyphelps itemshelp hypargnames))))
    (putprop rule all-argtypes 'all-argtypes)
    (putprop rule all-wffargtypelist 'all-wffargtypelist)
    (putprop rule all-wffop-typelist 'all-wffop-typelist)
    (putprop rule new-wff-arglist 'new-wff-arglist)
    (putprop rule all-arghelps 'all-arghelps)
    (putprop rule m-line-args 'm-line-args)
    (putprop rule multi-default-lines 'multi-default-lines)
    (putprop rule unique-linenames 'unique-linenames)
    (putprop rule unique-restrictions 'unique-restrictions)
    (putprop rule propsymalist 'propsymalist)
    (putprop rule all-args 'argnames)
    (putprop rule true-lineargnames 'lineargnames)
    (putprop rule ex-lineargnames 'ex-lineargnames)
    (putprop rule new-lineargnames 'new-lineargnames)
    (putprop rule wffarglist 'wffargnames)
    (putprop rule hypargnames 'hypargnames)
    t))

;;; meta-ize puts in metawffs, except for the justifications, which
;;; must be processed later.

(defun meta-ize (line-arg)
  (list (nth 0 line-arg)
	(nth 1 line-arg)
	(nth 2 line-arg)
	(meta-ize-gwff (nth 3 line-arg))
	(nth 4 line-arg)))

(defun meta-ize-justifications (line-arg)
  (list (nth 0 line-arg)
	(nth 1 line-arg)
	(nth 2 line-arg)
	(nth 3 line-arg)
	(meta-ize-just (nth 4 line-arg))))

;*;(defun name-wffsetarg (line-label)
;*;  (declare (special wffsetconsts))
;*;  (do ((wffsets wffsets (cdr wffsets))
;*;       (wffset-names nil (let ((ws (car wffsets)))
;*;			   (if (member ws wffsetconsts) wffset-names
;*;			       (cons (gen-wffset-name line-label ws)
;*;				     wffset-names)))))
;*;      ((null wffsets) (nreverse wffset-names))))

;*;(defun gen-wffset-name (line-label ws)
;*;  (let ((unique-wffset-name 
;*;	 (implode (nconc (explodec line-label) (cons '- (explodec ws))))))
;*;    (putprop unique-wffset-name ws 'wffset-name)
;*;    (putprop unique-wffset-name
;*;	     `("Hypotheses for line " ',line-label)
;*;	     'help-string)
;*;    unique-wffset-name))

(defun gen-hypargname (line-label)
  (let ((hypargname (prepend line-label '-hyps)))
    (putprop hypargname line-label 'comes-from)
    hypargname))

(defun meta-ize-just (just)
  (if (null just) nil
      (list (car just) (mapcar #'meta-ize-gwff (cadr just)) (caddr just))))

;;; The next sections contains functions for generating argument names
;;; in a sophisticated way.

(defun generate-arg-name (gvar-label)
  (let ((gvar (car gvar-label))
	(var-label (cdr gvar-label)))
    (declare (ignore var-label))	; For now.
    (let ((argname (getnameroot gvar)))
      (putprop argname gvar 'gvar)
      argname)))

;;; The next section contains functions finding or generating help strings.

(defun find-line-helps (itemshelp m-linelist)
  (do ((ls m-linelist (cdr ls))
       (help-strings
	nil
	(cons (let ((provided-help (assoc (caar ls) itemshelp)))
		(if (not provided-help) '"" (cdr provided-help)))
	      help-strings)))
      ((null ls) (nreverse help-strings))))

(defun find-wff-helps (itemshelp wffarglist)
  (do ((wargs wffarglist (cdr wargs))
       (help-strings
	nil
	(cons (let ((provided-help
		     (or (assoc (get (car wargs) 'gvar) itemshelp)
			 (assoc (car wargs) itemshelp))))
		(if (not provided-help) '""  (cdr provided-help)))
	      help-strings)))
      ((null wargs) (nreverse help-strings))))


(defun find-hyphelps (itemshelp hyp-arglist)
  (do ((hypargs hyp-arglist (cdr hypargs))
       (help-strings
	nil
	(cons (if treat-hlines-as-dlines ; If T, we almost never prompt for
		  "Hypotheses"		; hypothesis - make short.
		  (conc-strings "Hypothesis for "
				  (cdr (assoc (get (car hypargs) 'comes-from)
					       itemshelp))))
	      help-strings)))
      ((null hypargs) (nreverse help-strings))))

(defun nil-const (dummy)
  (declare (ignore dummy))
  nil)



