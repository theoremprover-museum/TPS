;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLHELP)

;;;
;;; File: Otl-Help
;;; 
;;; Contains help function for rules.
;;;


(part-of otlhelp)

(deffile otl-help
  (part-of otlhelp)
  (extension lsp)
  (mhelp "Defines help function for rule definitions."))

(context help-obj)

(defcategory rulehelp
  (define defrulehelp)
  (properties
   (lines read-lines)
   (restrictions read-restrictions)
   (priority single)
   (support-transformation multiple)
   (itemshelp drop-prop)
   (mhelp single))
  (global-list global-hrulelist)
  (mhelp-line "intermediate rule definition")
  (mhelp-fn (declare (ignore category))(prep-print-help rulehelp)))

(defun drop-prop (prop-value category)
  (declare (ignore prop-value category))
  nil)


(defun help-rule (rule)
  (if (get rule 'rulehelp) (prep-print-help rule)
      (let ((rule-file (find-rule-file rule)))
	(if rule-file (rule-help rule rule-file)
	    (throwfail
	     "If this really is a rule, I don't know anything about it.")))))

(defun find-rule-file (rule)
  (declare (special rules-table))
  (car (find-if #'(lambda (elm) (member rule (cdr elm))) rules-table)))

(defun rule-help (rule rulefile)
  (if (get-rule-help rule rulefile)
      (prep-print-help rule)
      (throwfail "Rule " rule " not found.")))

(defun prep-print-help (rule)
  (prepare-rule-help rule)
  (print-rule-help rule))

(defun prepare-rule-help (rule)
  (let (hyps just assertion)
    (dolist (line (get rule 'lines))
      (if (and (not treat-hlines-as-dlines) (member (car line) (nth 2 line)))
	  (setq hyps (list (car line)))
	  (setq hyps (append (nth 1 line) (nth 2 line))))
      (when (nth 1 line)
	(putprop (car (nth 1 line)) (car (nth 1 line)) 'linenumber))
      (setq assertion (nth 3 line))
      (setq just (or (nth 4 line) '(| | () ())))
      (setq line (nth 0 line))
      (when (and (not treat-hlines-as-dlines) (member line hyps))
	(setq just `(,hline-justification () ())))
      (putprop line line 'linenumber)
      (putprop line hyps 'hypotheses)
      (putprop line just 'justification)
      (putprop line assertion 'assertion))))

(defun get-rule-help (rule rulefile)
  (in-mode rules
    (in-mode higher-order
     (let* ((rulefile
	     (locate-tps-file
	       (make-pathname% :name (if (symbolp rulefile)
					 (string-downcase 
					   (symbol-name rulefile))
					 rulefile)
			       :type "rules"
			       :defaults nil)
	      nil)))
	(when rulefile
	  (with-open-file (rulef rulefile :direction :input)
	    (do ((form (read rulef nil :$eof$) (read rulef nil :$eof$)))
		((eq form :$eof$) nil)
	      (when (and (consp form)
			 ;; next test must be string-equal because of
			 ;; package qualifiers.
			 (string-equal (string (car form)) "defirule")
			 (consp (cdr form))
			 ;; next test should remain eq, because rule
			 ;; should be accessible.  This outlaws
			 ;; a request like HELP SIEG:AE if SIEG is
			 ;; not USE'd. The reason is the following
			 ;; eval which would define the wrong symbol.
			 (eq (cadr form) rule))
		(eval `(defrulehelp ,@(cdr form)))
		(return t)))))))))

;;; For the next function we assume that the help is already
;;; loaded (defined).

(defun print-rule-help (rule)
  (let ((ppwfflag nil)
	(rightmargin 55)
	(turnstile-indent-auto 'fix)
	(turnstyle-indent-auto 'fix)
	(support-transformation (get rule 'support-transformation)))
    ;; The above let should become a mode
    (dolist (line (mapcar #'car (get rule 'lines)))
      (if (exists ps-pair (car support-transformation) (memq line ps-pair))
	  (msgf "*")
	  (msgf " "))
      (pcall print-line line))
    (when (get rule 'restrictions)
      (msgf "Restrictions:"))
    (when (get rule 'hyp-restrict)
	  (msgf "Hypotheses of upper line must be a subset of those on lower line"))
    (dolist (restriction (get rule 'restrictions))
      (msg (tx 15) "(" (car restriction))
      (let ((scope t))
	(dolist (restr (cdr restriction))
	  (msg " " (restr . gwff)))
	(msg ")")))
    (msgf "Transformation: ")
    (dolist (ps-pair (car support-transformation))
      (print-ps-pair ps-pair) (msg " "))
    (msg "==> ")
    (dolist (ps-pair (cadr support-transformation))
      (print-ps-pair ps-pair) (msg " "))))

(defun print-ps-pair (ps-pair)
  (msg "(")
  (if (consp (car ps-pair)) (msg "pp") (msg (car ps-pair)))
  (msg " ")
  (do ((lines (cdr ps-pair) (cdr lines)))
      ((null lines))
    (if (consp (car lines)) (msg "ss") (msg (car lines)))
    (when (cdr lines) (msg " ")))
  (msg ")"))


(defmexpr list-rules
  (mhelp "List all rules with their suggestion priority."))

; old version - commented out by cebrown 2/14/03
;(defun list-rules ()
;  (let ((sorted-rules (sort-rules global-srulelist)))
;    (do ((ssrules sorted-rules (cdr ssrules)))
;	((null ssrules))
;      (if (integerp (caar ssrules))
;	  (progn (msgf (caar ssrules) ":" (t 5)) (setq curpos 6))
;	(progn (msgf "99:" (t 5)) (setq curpos 6)))
;      (dolist (rule (cdar ssrules))
;	      (when alpha-lower-flag (setq rule (string-downcase (princ-to-string rule))))
;	      (let ((alpha-lower-flag t))
;		(pcall margin-correct rule))))))

; new version - cebrown 2/14/03
(defun list-rules ()
  (let ((context-rule-assoc nil)
	(ctxt nil)
	(gsrl global-srulelist))
    ; add some special cases that aren't technically "rules"
    (when (get 'ML::rulep 'mainfns)
      (push 'RULEP gsrl)
      (push '(ML::RULES-2-PROP) gsrl))
    (when (get 'ML::TYPESUBST 'MAINFNS)
      (push 'TYPESUBST gsrl)
      (push '(ML::RULES-5-SUBST) gsrl))
    (when (get 'assert 'mainfns)
      (push 'ASSERT gsrl)
      (push '(ML::RULES-1-MISC) gsrl))
    (when (get 'add-hyps 'mainfns)
      (push 'ADD-HYPS gsrl)
      (push '(ML::RULES-1-MISC) gsrl))
    (when (get 'delete-hyps 'mainfns)
      (push 'DELETE-HYPS gsrl)
      (push '(ML::RULES-1-MISC) gsrl))
    (dolist (r gsrl)
      (if (symbolp r)
	  (if ctxt
	      (let ((a (assoc ctxt context-rule-assoc)))
		(if a
		    (setq context-rule-assoc
			  (cons (cons ctxt (cons r (cdr a)))
				(remove a context-rule-assoc)))
		  (push (list ctxt r) context-rule-assoc)))
	    nil)
	(when (and (consp r) (symbolp (car r)))
	  (setq ctxt (car r)))))
    (dolist (crl (reverse context-rule-assoc))
      (msgf (or (get (car crl) 'SHORT-ID)
		(car crl))
	    ":" (t 5)) (setq curpos 6)
      (dolist (rule (sort (cdr crl) #'string<))
	(when alpha-lower-flag (setq rule (string-downcase (princ-to-string rule))))
	(let ((alpha-lower-flag t))
	  (pcall margin-correct rule))))))

(defmexpr list-rules*
    (mhelp "List all rules with their intermediate rule definition help"))

; old version - the new version lists them in the same order as list-rules, separated by contexts
;(defun list-rules* ()
;  (let ((sorted-rules (sort-rules global-srulelist)))
;    (do ((ssrules sorted-rules (cdr ssrules)))
;	((null ssrules))
;      (when (integerp (caar ssrules)) ; don't list those with "priority 99", apparently the default priority - cebrown 5/7/00
;	(dolist (rule (cdar ssrules))
;	  (msgf rule t)
;	  (help-rule rule)
;	  (msgf "-------------------------------------------------------------" t))))))

(defun list-rules* ()
  (let ((context-rule-assoc nil)
	(ctxt nil)
	(gsrl global-srulelist))
    ; add some special cases that aren't technically "rules"
    (when (get 'ML::rulep 'mainfns)
      (push 'RULEP gsrl)
      (push '(ML::RULES-2-PROP) gsrl))
    (when (get 'assert 'mainfns)
      (push 'ASSERT gsrl)
      (push '(ML::RULES-1-MISC) gsrl))
    (when (get 'add-hyps 'mainfns)
      (push 'ADD-HYPS gsrl)
      (push '(ML::RULES-1-MISC) gsrl))
    (when (get 'delete-hyps 'mainfns)
      (push 'DELETE-HYPS gsrl)
      (push '(ML::RULES-1-MISC) gsrl))
    (dolist (r gsrl)
      (if (symbolp r)
	  (if ctxt
	      (let ((a (assoc ctxt context-rule-assoc)))
		(if a
		    (setq context-rule-assoc
			  (cons (cons ctxt (cons r (cdr a)))
				(remove a context-rule-assoc)))
		  (push (list ctxt r) context-rule-assoc)))
	    nil)
	(when (and (consp r) (symbolp (car r)))
	  (setq ctxt (car r)))))
    (dolist (crl (reverse context-rule-assoc))
      (msgf "=====================")
      (msgf (or (get (car crl) 'SHORT-ID)
		(car crl))
	    ":" t)
      (msgf "=====================")
      (dolist (rule (sort (cdr crl) #'string<))
	(msgf rule t)
	(if (member rule '(RULEP ASSERT ADD-HYPS DELETE-HYPS))
	    (mhelp rule)
	  (help-rule rule))
	(msgf "-------------------------------------------------------------" t)))))

(defmexpr search
  (argnames phrase search-names)
  (argtypes string boolean)
  (arghelp "String to search for" "Search names of objects only?")
  (defaultfns (lambda (phrase search-names)
		(list phrase (if (eq search-names '$) nil search-names))))
  (mainfns review-keys-all)
  (mhelp "Look for a key phrase in all help strings (or just all names) 
of TPS objects. See also KEY, in the review top level (where it searches 
through the flags) and the library top level (where it searches through
the library objects)."))

(defun review-keys-all (phrase names)
  (let ((gdefl (remove-duplicates global-definelist :test #'(lambda (x y) (equal (cdr x) (cdr y))))))
  (dolist (whateverlist gdefl)
    (let ((current-list (remove-duplicates (eval (get (cdr whateverlist) 'global-list)) :test #'equal)))
      (dolist (flag current-list)
	    (if (not (listp flag))
	  (let* ((mhelp-text (get flag 'mhelp))
		 (flagtext (if (and (consp (car mhelp-text)) (listp (cdar mhelp-text))) 
			       (string-downcase (tacktogether (mapcar #'princ-to-string (cdar mhelp-text))))
			     (if (not (listp (car mhelp-text)))
				 (string-downcase (tacktogether (mapcar #'princ-to-string mhelp-text)))
			       (string-downcase (cdar mhelp-text)))))
		(searchtext (string-downcase phrase))
		(objectname (string-downcase (princ-to-string flag)))
		(offset (length phrase))
		(found nil))
	    (if names (setq flagtext objectname) (setq flagtext (concatenate 'string objectname " " flagtext)))	    
	    (do ((counter 0 (1+ counter)))
		((or found (>= (+ counter offset) (1+ (length flagtext)))))
		(setq found (string= flagtext searchtext :start1 counter :end1 (+ offset counter)))
		(if found (msg (cdr whateverlist) " : " flag t))))))))))
