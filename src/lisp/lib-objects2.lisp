;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(context  miscellaneous)

;;;
;;; File: LIB-objects
;;; Package: library
;;;

(deffile lib-objects2
  (part-of rrules)
  (extension lisp)
  (mhelp "Functions to handle rewrite rules, theories and other types
of objects not loaded into all versions of the library."))


;;REWRITE

(deflibobject rrule
  (lib-promptfn lib-promptfn-rewrite)
  (lib-descr-readfn libparse-rewrite)
  (lib-tpsobject get-lib-rewrite)
  (lib-printfn libprint-rewrite)
  (mhelp "Rewrite rule"))

(defun lib-promptfn-rewrite (name type help file modify comment old-value)
  (declare (ignore comment modify file help type))
  (let* ((gwff1 (if old-value 
		    (car (libitem-description old-value))
		  (if (rewrule-p name)
		      (get name 'before)
		    '$)))
	 (gwff2 (if old-value 
		    (cdr (libitem-description old-value))
		  (if (rewrule-p name)
		      (get name 'after)
		    '$)))
	 (func (if old-value 
		   (cadr (assoc 'function (libitem-other-attributes old-value)))
		 (if (rewrule-p name) (get name 'rewfn)
		 nil)))
	 (appfn (if old-value 
		    (cadr (assoc 'appfn (libitem-other-attributes old-value)))
		 (if (rewrule-p name) (get name 'appfn)
		 nil)))
	 (bidir (if old-value
		    (cadr (assoc 'bidirectional (libitem-other-attributes old-value)))
		  (if (rewrule-p name) (get name 'bidirectional)
		    t)))
	 (typelist (if old-value
		       (mapcar #'(lambda (x) (get-stringtype (princ-to-string x)))
			       (cadr (assoc 'typelist (libitem-other-attributes old-value))))
		     (if (rewrule-p name) (get name 'rtypelist)
		       nil)))
	 (varlist (if old-value
		      (mapcar #'(lambda (x) x)  ; nonsensical
			      (cadr (assoc 'variables (libitem-other-attributes old-value))))
		    (if (rewrule-p name) (get name 'variables)
		      nil)))
	 description1 description2)
    (do ((fv1 nil (free-vars-of description1))
	 (fv2 nil (free-vars-of description2))
	 (not-first nil t))
	((and not-first
	      (or (equal 
		   (type description1) (type description2)) (msgf "These two gwffs are of different types. Try again."))
	      (or (and (null (setdiff fv1 fv2)) (null (setdiff fv2 fv1)))
		  (query "These two gwffs have different free variables. Accept them anyway?" nil))))
	(prompt-read description1 nil (msgf "Gwff on left-hand side? ") 'gwff gwff1
		     ((? (msgf "the gwff to be rewritten."))))
	(prompt-read description2 nil (msgf "Gwff on right-hand side? ") 'gwff gwff2
		     ((? (msgf "the result of rewriting this gwff.")))))
    (prompt-read func nil (msgf "Function to apply after rewriting? ") 'symbol func
		 ((? (msgf "a lisp function or NIL."))))
    (prompt-read appfn nil (msgf "Function to test applicability of rewrite (or NIL)? ") 'symbol appfn
		 ((? (msgf "a lisp function or NIL."))))
    (prompt-read typelist nil (msgf "List of polymorphic types") 'typesymlist typelist
		   ((? (msgf "List of type variables, eg., (\"A\" \"B\") "))))
    (prompt-read bidir nil (msgf "Bidirectional rule?") 'yesno bidir
		   ((? (msgf "T if the rule can be applied in both directions"))))
    (prompt-read varlist nil (msgf "List of free variables?") 'gvarlist varlist
		   ((? (msgf "List of variables which occur free in the rewrite rule,
eg., (\"x\" \"y\")"))))
    (values (cons description1 description2) 'unclassified 
	    (list (list 'function func) (list 'appfn appfn) (list 'bidirectional bidir) (list 'typelist typelist) (list 'variables varlist) (list 'derived-in nil)))))

(defun libprint-rewrite (rule libstyle)
  (if (eq libstyle 'write-to-file)
      (in-mode re-read (msg "(" ((car rule) . gwff) " . " ((cdr rule) . gwff) ")"))
      (msg ((car rule) . gwff) (if (eq style 'tex) "\\partformula{$\\longrightarrow$}" " --> ") ((cdr rule) . gwff))))

(defun libparse-rewrite (rule)
  (let ((foo (gettype 'gwff (concatenate 'string "= [" (car rule) "] [" (cdr rule) "]"))))
    (cons (cdar foo) (cdr foo))))
  ;was 
  ;(cons (gettype 'gwff (car rule)) (gettype 'gwff (cdr rule))))
  ; but the above forces it to be well-typed.

(defun get-lib-rewrite (libitem)
  (declare (special global-theory-list))  ; mkaminski 11/13/2005
  (let ((name (libitem-name libitem))
	(rew-object nil))
    (push (list 'mhelp (libitem-mhelp libitem)) rew-object)
    (push (list 'before (car (libitem-description libitem))) rew-object)
    (push (list 'after (cdr (libitem-description libitem))) rew-object)
    (push (list 'rewfn (cadr (assoc 'function (libitem-other-attributes libitem)))) rew-object)
    (push (list 'appfn (cadr (assoc 'appfn (libitem-other-attributes libitem)))) rew-object)
    (push (list 'active t) rew-object)
    (push (list 'bidirectional (cadr (assoc 'bidirectional (libitem-other-attributes libitem)))) rew-object)
    (push (list 'rtypelist (cadr (assoc 'typelist (libitem-other-attributes libitem)))) rew-object)
    (push (list 'variables (cadr (assoc 'variables (libitem-other-attributes libitem)))) rew-object)
    (push (list 'derived-in (cadr (assoc 'derived-in (libitem-other-attributes libitem)))) rew-object)
    (mapcar
     #'(lambda (theory)
	 (mapcar
	  #'(lambda (th)
	      (when (eq theory th)
		(unless (cadr (assoc 'appfn rew-object))
		  (when (get th 'derived-appfn)
		    (setq rew-object
			  (cons (list 'appfn (get th 'derived-appfn))
				(delete-if #'(lambda (p) (eq (car p) 'appfn))
					   rew-object)))))
		(unless (cadr (assoc 'rewfn rew-object))
		  (when (get th 'derived-rewfn)
		    (setq rew-object
			  (cons (list 'rewfn (get th 'derived-rewfn))
				(delete-if #'(lambda (p) (eq (car p) 'rewfn))
					   rew-object)))))
		(setf (get th 'rrules)
		      (remove-duplicates
		       (list (cons name
				   (car (get th 'rrules))))))))
	  global-theory-list))
     (cadr (assoc 'derived-in (libitem-other-attributes libitem))))
    (when (and (rewrule-p name) load-warn-p)
      (complain "Redefining rewrite rule : " name))
    (eval (nconc (list 'defrewrule name) rew-object))))

;;THEORY

(deflibobject theory
  (lib-promptfn lib-promptfn-theory)
  (lib-descr-readfn libparse-theory)
  (lib-tpsobject get-lib-theory)
  (lib-printfn libprint-theory)
  (mhelp "A theory (a set of axioms and rewrite rules)."))

; mkaminski 11/24/2005 -- the new declaration is right below

;(defun lib-promptfn-theory (name type help file modify comment old-value)
;  (declare (ignore comment modify file help type old-value))
;  (values (princ-to-string name) 'unclassified nil))

(defun lib-promptfn-theory (name type help file modify comment old-value)
  (declare (ignore type file))
  (let ((relation-sign (if old-value
			   (cdr (assoc 'relation-sign
				       (libitem-other-attributes old-value)))
			 (if (theory-p name) (get name 'relation-sign) '=)))
	(reflexive (if old-value
		       (cdr (assoc 'reflexive
				   (libitem-other-attributes old-value)))
		     (if (theory-p name) (get name 'reflexive) t)))
	(congruent (if old-value
		       (cdr (assoc 'congruent
				   (libitem-other-attributes old-value)))
		     (if (theory-p name) (get name 'congruent) t)))
	(der-appfn (if old-value
		       (cdr (assoc 'derived-appfn
				   (libitem-other-attributes old-value)))
		     (if (theory-p name) (get name 'derived-appfn) nil)))
	(der-rewfn (if old-value
		       (cdr (assoc 'derived-rewfn
				   (libitem-other-attributes old-value)))
		     (if (theory-p name) (get name 'derived-rewfn) nil))))
    (prompt-read relation-sign nil (msgf "Relation sign? ")
		 'symbol relation-sign ((? (msgf "A lisp symbol."))))
    (prompt-read reflexive nil (msgf "Is the represented relation reflexive? ")
		 'yesno reflexive ((?
		 (msgf "T iff the theory describes a reflexive relation."))))
    (prompt-read congruent nil
		 (msgf "Is the represented relation a congruence?")
		 'yesno congruent
		 ((? (msgf "T iff the theory describes a congruence."))))
    (prompt-read der-appfn nil
		 (msgf "Procedure to test applicability of derived rewrite rules?")
		 'symbol der-appfn
		 ((? (msgf "A lisp function symbol or NIL." t
			   "Commonly used values: S-EQN-AXIOM-APPFN, INFERENCE-SCHEME-APPFN"))))
    (prompt-read der-rewfn nil
		 (msgf "Procedure to apply after rewriting using derived rewrite rules?")
		 'symbol der-rewfn
		 ((? (msgf "A lisp function symbol or NIL." t
			   "Commonly used values: S-EQN-AXIOM-REWFN, INFERENCE-MATCH-BINDERS-REWFN"))))
    (values (princ-to-string name) 'unclassified
	    (list (list 'relation-sign relation-sign)
		  (list 'reflexive reflexive)
		  (list 'congruent congruent)
		  (list 'derived-appfn der-appfn)
		  (list 'derived-rewfn der-rewfn)))))

(defun libprint-theory (theory libstyle)
  (declare (ignore libstyle))
  (princ theory))

(defun libparse-theory (theory)
  theory)

(defun get-lib-theory (libitem)
  (let ((name (libitem-name libitem))
	(gwff-list (remove-if-not #'(lambda (x) (and (gwff-q x)
						     (not (theory-p x))
						     (eq (type x) 'O)))
				  (libitem-needed-objects libitem)))
	(rrule-list (remove-if-not 'rewrule-p (libitem-needed-objects libitem)))
	(theory-list (remove-if-not 'theory-p (libitem-needed-objects libitem)))
	(other-list (remove-if 'theory-p (remove-if 'rewrule-p 
			       (remove-if #'(lambda (x) (and (gwff-q x)
							     (not (theory-p x))
							     (eq (type x) 'O)))
					  (libitem-needed-objects libitem)))))
	(relation-sign (cond ((cadr (assoc 'relation-sign
					  (libitem-other-attributes libitem))))
			     (t '=)))
	(reflexive (let ((reflex (assoc 'reflexive
					(libitem-other-attributes libitem))))
		     (if reflex (cadr reflex) t)))
	(congruent (let ((cong (assoc 'congruent
				      (libitem-other-attributes libitem))))
		     (if cong (cadr cong) t)))
	(der-appfn (cadr (assoc 'derived-appfn
				(libitem-other-attributes libitem))))
	(der-rewfn (cadr (assoc 'derived-rewfn
				(libitem-other-attributes libitem))))
	theory-object abbrev-object)
    (push (list 'defn (theory-to-gwff gwff-list rrule-list theory-list)) abbrev-object)
    (push (list 'printnotype T) abbrev-object)
    (push (list 'typelist (mapcar #'type-to-string-2 (remove-duplicates 
			   (reduce 'append (mapcar #'(lambda (x) (get x 'typelist)) rrule-list))))) abbrev-object)
    (unless (cdar abbrev-object) (pop abbrev-object))
    (push (list 'type "O") abbrev-object)
    (push (list 'fo-single-symbol T) abbrev-object)
    (push (list 'mhelp (libitem-mhelp libitem)) abbrev-object)
    (push (list 'mhelp (libitem-mhelp libitem)) theory-object)
    (push (list 'gwffs gwff-list) theory-object)
    (push (list 'rrules rrule-list) theory-object)
    (push (list 'extends theory-list) theory-object)
    (push (list 'other-stuff other-list) theory-object)
    (push (list 'relation-sign relation-sign) theory-object)
    (push (list 'reflexive reflexive) theory-object)
    (push (list 'congruent congruent) theory-object)
    (push (list 'derived-appfn der-appfn) theory-object)
    (push (list 'derived-rewfn der-rewfn) theory-object)
    (when (and (theory-p name) load-warn-p)
	  (complain "Redefining theory : " name))
    (eval (nconc (list 'deftheory name) theory-object))
    (eval (nconc (list 'def-abbrev name) abbrev-object))))

(defun rrule-to-gwff (rule)
  (let ((bef (get rule 'before))
	(aft (get rule 'after))
	(typ (type (get rule 'before)))
	(bi (get rule 'bidirectional)))
    (if (or bi (neq typ 'O))
	(setq bef (mbed=left bef aft))
      (setq bef (mbed-implics-left bef aft)))
    (dolist (v (free-vars-of bef))
	    (setq bef (cons (cons v 'forall) bef)))
    bef))

(defun theory-to-gwff (gwff-list rrule-list theory-list)
  (setq rrule-list (remove-if 'null (append 
		    theory-list ;these theories are abbrevs by now
		    (mapcar #'(lambda (x) (get x 'represents)) gwff-list)
		    (mapcar 'rrule-to-gwff rrule-list))))
  (setq rrule-list 
	(reduce #'(lambda (x y) (cons (cons 'and x) y)) rrule-list))
  (with-output-to-string
   (*standard-output*)
   (in-mode re-read (let ((style 'generic))
		      (msg (rrule-list . gwff))))))

; mkaminski 11/16/2005

(defun save-rrule (name)
  (unless (get name 'rewrite-rule)
    (throwfail name " is not a known rewrite rule."))
  (let ((before (get name 'before))
	(after (get name 'after))
	(bidir (get name 'bidirectional))
	(derived-in (get name 'derived-in))
	(types (get name 'rtypelist))
	(rewfn (get name 'rewfn))
	(appfn (get name 'appfn))
	(variables (get name 'variables))
	(help (cdr (assoc 'rewrite-rule (get name 'mhelp))))
	(needed-objects nil))
  (msgf "Checking for existing objects..." t)
  (let* ((found (locate-item name :type 'rrule :writeable t
			     :multiple t)) ; ignore show-all-libobjects -- cebrown 7/26/99
	 (otherfound (unless found
		       (locate-item name :type 'rrule :writeable nil
				    :multiple t))) ; ignore show-all-libobjects -- cebrown 7/26/99
	 (count 0)
	 (allowable-dirlist (mapcar 'directory-namestring default-lib-dir))
	 directory modify old-value file provability keywords update-help
	 comment)
    (when (null allowable-dirlist) (throwfail "You need to set DEFAULT-LIB-DIR"))
    (when (cdr (remove-duplicates (mapcar #'car 
					  (remove-if
					   #'(lambda (x) (eq (car x) 'rrule))
					   (gethash name *lib-masterindex*)))))
      (msgf "WARNING:  There are conflicting library objects with name " name))
    (when otherfound
      (if (cdr otherfound)
	  (complain "There are " (length otherfound) " items " name " in the backup library." t)
	(complain "There is an item " name " in the backup library." t))
      (msg "It is strongly recommended that you copy ")
      (if (cdr otherfound)
	  (msg "one of these")
	(msg "it"))
      (msg " into your library directory," t)
      (msg "then modify associated information as you insert this item.")
      (when (query "Copy A Libobject?" t)
	(let ((fname (file-namestring (cdar otherfound))))
	  (prompt-read fname nil (msg "Please input a filename for this record.") 'filespec fname
		       ((? (mhelp 'filespec)) (?? (mhelp 'filespec))))
	  (copy-libobject name 'rrule fname t)
	  (setq found (locate-item name :type 'rrule :writeable t
				   :multiple t)))))
    (when (and (eq style 'istyle) (not *simple-interface-prompts*))  (start-prompt-msg))
    (when found
      (complain (length found) " writeable items called " name " found. They are as follows:" t)
      (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
      (dolist (elt found)
	(incf count) 
	(complain count ") MODIFY " (car elt) " in file " (pfile (namestring (cdr elt))))
	(setq allowable-dirlist (delete (directory-namestring (cdr elt)) allowable-dirlist :test 'string=)))
      (unless show-all-libobjects
	(throwfail t "Please either specify the type of the item or set SHOW-ALL-LIBOBJECTS to T."))) ; cebrown: changed NIL to T.
    (unless found
      (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options)))
    (when allowable-dirlist
      (dolist (elt allowable-dirlist)
	(incf count)
	(complain count ") SAVE-RRULE into directory " (pfile elt))))
    (incf count) 
    (complain count ") DO NOTHING.")
    (setq count (1- (get-a-number count)))
    (if (nth count found)
	(setq found (nth count found))
      (let ((found-l (length found))) ; cebrown 7/22/99
	(setq found nil directory (nth (- count found-l) allowable-dirlist))))
    (unless (and (null found) (null directory))
      (when found 
	(msgf "The SAVE-RRULE command can be used to modify existing rewrite" t
	      "rules in the library. This modification may not change the basic" t
	      "shape of a rule, its polymorphic types or its free variables." t

	      "Whatever you specify under \"additional remarks\" will be" t
	      "added to whatever is already there. If you don't want to add" t
	      "additional remarks, respond with <space><return>." t)
	(setq modify t)
	(setq old-value (retrieve-item name :type 'rrule :writeable t :multiple nil 
				       :preferred-dir (cdr found))))
      (when old-value
	(let* ((misc (libitem-other-attributes old-value))
	       (old-rewfn (cadr (assoc 'function misc)))
	       (old-bidir (cadr (assoc 'bidirectional misc)))
	       (old-appfn (cadr (assoc 'appfn misc)))
	       (old-types (cadr (assoc 'typelist misc)))
	       (old-variables (cadr (assoc 'variables misc)))
	       (old-derived-in (cadr (assoc 'derived-in misc))))
	  (unless (and (wffeq-ab (libitem-description old-value)
				 (cons before after))
		       (eq bidir old-bidir)
		       (eq appfn old-appfn)
		       (eq rewfn old-rewfn)
		       (equal types old-types)
		       (equal variables old-variables))
	    ;(format t "~&description: ~A,  ~A,  ~A~%bidir: ~A,  ~A,  ~A~%appfn: ~A,  ~A,  ~A~%rewfn: ~A,  ~A,  ~A~%types: ~A,  ~A,  ~A~%variables: ~A,  ~A,  ~A" (libitem-description old-value) (cons before after)
		;    (wffeq-ab (libitem-description old-value)
		;	      (cons before after))
		;    old-bidir bidir (eq bidir old-bidir)
		;    old-appfn appfn (eq appfn old-appfn)
		;    old-rewfn rewfn (eq rewfn old-rewfn)
		;    old-types types (equal types old-types)
		;    old-variables variables (equal variables old-variables))
	    (throwfail "The rewrite rule to be saved has the same name
as a rule already present in the library, but it does not extend
this rule. To save the rule nevertheless, please delete the old one
first or use the INSERT command from the LIBRARY top level." t))
	  (setq derived-in (remove-duplicates
			    (append derived-in old-derived-in)))
	  (msgf "Saved help message for the rule:" t (libitem-mhelp old-value))
	  (msgf "New help message for the rule:" t help)
	  (prompt-read update-help nil
		       (msgf "UPDATE-HELP (YESNO): Update help message for the rewrite rule?")
		       'yesno nil
		       ((? (mhelp 'string)) (?? (mhelp 'string))))
	  (unless update-help (setq help (libitem-mhelp old-value)))
	  (setq needed-objects (libitem-needed-objects old-value))))
      (prompt-read needed-objects nil 
		   (msgf "NEEDED OBJECTS (LIST): Other library objects")
		   'symbollist (if old-value 
				   (libitem-needed-objects old-value) 
				 (if (and (fboundp name) 
					  (or (and (get 'exercise 'testfn) 
						   (funcall (get 'exercise 'testfn) name)
						   (get name 'assertion))
					      (gwff-q name))) ; avoiding asking to search for it in the lib
				     (new-defs name) nil))
		   ((? (msgf " List of objects needed to define the current object."))
		    (?? (mhelp 'symbollist))))
      (let ((load-warn-p nil))
	(dolist (needed needed-objects) 
	  (retrieve-libobject needed :multiple nil
			      :preferred-dir (and found (directory-namestring (cdr found))))))
      (let ((filename (or (if (and old-value (libitem-file old-value))
			      (file-namestring (libitem-file old-value))
			    (concatenate 'string "rrule" 
					 "." default-libfile-type)))))
	(prompt-read file nil (msgf "FILE (FILESPEC): File to be saved in ")
		     'string filename ((? (mhelp 'string)) (?? (mhelp 'string))))
	(setq file (namestring (merge-pathnames (pathname-name file)
						(concatenate 'string 
							     (or (and directory (namestring directory))
								 (and found (directory-namestring (cdr found))))
							     (pathname-name filename) "."
							     default-libfile-type)))))
      (prompt-read comment nil (msgf "COMMENTS (STRING): Additional Remarks")
		   'string (if old-value (libitem-other-remarks old-value) "")
		   ((? (when old-value (msg "Comment will be appended to the existing comment." t))
		       (mhelp 'string))
		    (?? (mhelp 'string))))
      (when old-value (setq keywords (libitem-keywords old-value)))
      (let ((item (make-libitem
		   :name name :type 'rrule :description (cons before after)
		   :mhelp help :context 'unclassified
		   :keywords keywords
		   :provability nil
		   :proof-date nil
		   :other-attributes (list `#',rewfn
					   (list 'appfn appfn)
					   (list 'bidirectional bidir)
					   (list 'typelist types)
					   (list 'variables variables)
					   (list 'derived-in derived-in))
		   :needed-objects needed-objects
		   :other-remarks 
		   (if (and old-value
			    (not (string= (libitem-other-remarks old-value)
					  "")))
		       (concatenate 'string
				    (libitem-other-remarks old-value)
				    (string #\newline) comment (string #\newline))
		     (concatenate 'string comment (string #\newline)))
		   :file (namestring (pathname file)))))
	(setf (libitem-keywords item) (input-keywords item))
	(if modify 
	    (update-libitem item)
	  (store-item item)))))))
