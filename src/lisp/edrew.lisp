;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of rrules)

;;;
;;; File: EDLMBD
;;;
;;; defines editor operations connected with the lambda calculus
;;; (like LNORM, RED etc.)
;;;

(deffile edrew
  (part-of rrules)
  (extension lisp)
  (mhelp "Contains operations on rewrite rules."))

(context rewriting)

(eval-when (compile load eval)
(defcategory rewrite-rule
  (define defrewrule)
  (properties 
   (before single)
   (after single)
   (rewfn singlefn)
   (rtypelist single)
   (appfn single)
   (bidirectional single)
   (variables single)
   (derived-in single)
   (active single)
   (mhelp single))
  (global-list global-rewrite-rule-list)
  (mhelp-line "rewrite rule")
  (mhelp-fn rewrite-mhelp))
)

(defun rewrite-mhelp (keyword category)
  (when (categ-mhelp keyword category) (princ-mhelp keyword category))
  (if (get keyword 'bidirectional)
      (msgf keyword " : " ((get keyword 'before) . gwff) " <--> " ((get keyword 'after) . gwff))
    (msgf keyword " : " ((get keyword 'before) . gwff) " --> " ((get keyword 'after) . gwff)))
  (when (get keyword 'appfn)
	(msgf "  if the term to be rewritten satisfies " (get keyword 'appfn) t))
  (when (get keyword 'rtypelist) 
	(msgf "  (polymorphic types are: " (l (get keyword 'rtypelist)) ")" t))
  (when (get keyword 'rewfn)
	(msgf "  then apply function " (get keyword 'rewfn) t)))

(defun rewrule-p (rule) (and (not (listp rule)) (member rule global-rewrite-rule-list)))
(defun get-rewrule (rule) (let ((r (rewrule-p rule))) (if r (car r) ;nil
; mkaminski 10/9/2005 -- whenever its argument is not a rewrite rule,
;  get-rewrule should signal an error rather than return nil
							(throwfail rule " is not a known rewrite rule"))))
(defun active-p (rule) (and (rewrule-p rule) (get rule 'active)))

(eval-when (compile load eval)
(defcategory theory
  (define deftheory)
  (properties 
   (gwffs multiple)
   (rrules multiple)
   (extends multiple)
   (other-stuff multiple)
   (relation-sign single)  ; mkaminski 11/24/2005
   (reflexive single)      ;
   (congruent single)      ;
   (derived-appfn single)  ;
   (derived-rewfn single)  ;
   (mhelp single))
  (global-list global-theory-list)
  (mhelp-line "theory")
  (mhelp-fn theory-mhelp))
)

(defun theory-mhelp (keyword category)
  (when (categ-mhelp keyword category) (princ-mhelp keyword category))
  (when (car (get keyword 'extends))
	(msgf keyword " is an extension of " (l (get keyword 'extends)) t))
  (when (car (get keyword 'gwffs))
	(msgf "Axioms are : " (l (get keyword 'gwffs)) t))
  (when (car (get keyword 'rrules))
	(msgf "Rewrite rules are: " (l (get keyword 'rrules)) t))
  (when (car (get keyword 'other-stuff))
	(msgf "Other associated objects are : " (l (get keyword 'other-stuff)) t))
  (when (get keyword 'relation-sign)
        (msgf "Rewrite relation sign: " (get keyword 'relation-sign) t))
  (when (get keyword 'reflexive)
        (msgf "Rewrite relation is reflexive. "))
  (when (get keyword 'congruent)
        (msgf "Rewrite relation is a congruence." t)))

(defun theory-p (theory) (and (not (listp theory)) (member theory global-theory-list)))
(defun get-theory (theory) (let ((r (theory-p theory))) (if r (car r) 
							  (if (retrieve-libobject theory :type 'theory
										  :multiple show-all-libobjects)
							      (car (theory-p theory))
							    nil))))

; This worked for theories without relation signs, reflexivity and congruence
; attributes -- mkaminski 11/26/2005

;(defmexpr make-theory
;  (argtypes symbol symbollist symbollist symbollist symbollist string)
;  (argnames name extends axioms rrules other mhelp)
;  (arghelp "Name of new theory" 
;	   "Theories which this theory extends." "Axioms" "Rewrite rules" "Other library objects" "Help message")
;  (defaultfns (lambda (a b c d e f) (list a (if (eq b '$) nil b) (if (eq c '$) nil c) 
;					  (if (eq d '$) nil d) (if (eq e '$) nil e)
;					  (if (eq f '$) "" f))))
;  (mhelp "Create a new theory. A theory is defined by (optionally) starting 
;from an old theory, and adding rewrite rules and axioms. You can also attach
;other library objects to the theory, which will then be loaded with it.
;This will also make an abbreviation of the same name.
;All of the objects in the theory should be defined in the library."))

(defmexpr make-theory
  (argtypes symbol symbollist symbollist symbollist symbollist symbol yesno
	    yesno symbol symbol string)
  (argnames name extends axioms rrules other sign reflexive congruence mhelp)
  (arghelp "Name of new theory" "Theories which this theory extends."
	   "Axioms" "Rewrite rules" "Other library objects" "Relation sign"
	   "Relation reflexive?" "Relation congruent?"
	   "Procedure to test applicability of derived rewrite rules"
	   "Procedure to apply after rewriting using a derived rule"
	   "Help message")
  (defaultfns (lambda (a b c d e f g h i j k) (list a (if (eq b '$) nil b) (if (eq c '$) nil c) 
					  (if (eq d '$) nil d)
					  (if (eq e '$) nil e)
					  (if (eq f '$) '= f)
					  (if (eq g '$) t g)
					  (if (eq h '$) t h)
					  (if (eq i '$) nil i)
					  (if (eq j '$) nil j)
					  (if (eq k '$) "" k))))
  (mainfns make-theory2)
  (mhelp "Create a new theory. A theory is defined by (optionally) starting 
from an old theory, and adding rewrite rules and axioms. You can also attach
other library objects to the theory, which will then be loaded with it.
This will also make an abbreviation of the same name.
All of the objects in the theory should be defined in the library."))

; mkaminski 11/24/2005

(defun make-theory2 (name extends axioms rrules other relsign refl cong
		     der-appfn der-rewfn mhelp)
  (make-theory name extends axioms rrules other mhelp)
  (setf (get name 'relation-sign) relsign)
  (setf (get name 'reflexive) refl)
  (setf (get name 'congruent) cong)
  (setf (get name 'derived-appfn) der-appfn)
  (setf (get name 'derived-rewfn) der-rewfn))

(defun make-theory (name extends axioms rrules other mhelp)
  (let ((th (list (list 'mhelp mhelp) (list 'gwffs axioms) (list 'rrules rrules)
		  (list 'other-stuff other) (list 'extends extends)))
	(ab (list (list 'defn (theory-to-gwff axioms rrules extends)) (list 'printnotype T)
		  (list 'type "O") (list 'fo-single-symbol T) (list 'mhelp mhelp))))
    (push (list 'typelist (mapcar #'type-to-string-2 (remove-duplicates 
			   (reduce 'append (mapcar #'(lambda (x) (get x 'typelist)) rrules))))) ab)
    (unless (cdar ab) (pop ab))
    (eval (nconc (list 'deftheory name) th))
    (eval (nconc (list 'def-abbrev name) ab))))

(defedop make-rrule
  (alias create-rewrite-rule)
  (result-> ignore)
  (edwff-argname gwff1)
  (mhelp "Create a rewrite rule whose left-hand side is 
the current edwff."))

(defwffop create-rewrite-rule
  (argtypes symbol gwff gwff symbol typesymlist yesno symbol string)
  (wffargtypes NIL "A" "A" NIL NIL NIL NIL NIL)
  (wffop-typelist "A")
  (resulttype gwff)
  (argnames name gwff1 gwff2 func types bidir appfn mhelp)
  (arghelp "name of rule" "left-hand side" "right-hand side" "function to apply" "polymorphic type vars" 
	   "bidirectional rule?" "function to test for applicability" "help message")
  (defaultfns (lambda (w x y z v b c u) (list w x y (if (eq z '$) NIL z) (if (eq v '$) NIL v) (if (eq b '$) t b) 
					    (if (eq c '$) NIL c) u)))
  (mhelp "Creates a new rewrite rule with the given left and right
sides, such that the left-hand gwff rewrites to the result of 
applying the function to the right-hand gwff."))

(defun create-rewrite-rule (name gwff1 gwff2 func types bidir appfn mhelp)
  (let ((rew (list (list 'mhelp mhelp) (list 'before gwff1) (list 'bidirectional bidir) 
		   (list 'after gwff2) (list 'active t) (list 'rewfn func) (list 'appfn appfn)
		   (list 'rtypelist types))))
    (eval (nconc (list 'defrewrule name) rew))
    gwff1))

(defmexpr list-rrules
  (mhelp "Show all the current rewrite rules."))

(defun list-rrules ()
  (msgf "Currently defined rewrites are: " (remove-if 'listp global-rewrite-rule-list) t
"These are defined as follows:" t)
  (dolist (rew global-rewrite-rule-list)
	  (when (rewrule-p rew)
		(rewrite-mhelp rew 'rewrite-rule)))
  (msgf t "Of these, " (or (remove-if-not 'active-p global-rewrite-rule-list)
			   "none" )
	" are active." t))

(defmexpr make-inverse-rrule
  (argtypes rrule symbol)
  (argnames rule newname)
  (arghelp "Rule to reverse" "Name for new rule")
  (mhelp "Make the inverse rewrite rule of an existing
rule."))

(defun make-inverse-rrule (rule name)
  (let ((rew (list
	      (list 'mhelp (cdr (assoc 'rewrite-rule (get rule 'mhelp))))
	      (list 'before (get rule 'after))
	      (list 'after (get rule 'before))
	      (list 'rewfn (get rule 'rewfn))
	      (list 'appfn (get rule 'appfn))
	      (list 'active t)
	      (list 'bidirectional (get rule 'bidirectional))
	      (list 'variables (get rule 'variables))
	      (list 'derived-in
		    (when (get rule 'bidirectional)
		      (mapcar
		       #'(lambda (theory)
			   (mapcar
			    #'(lambda (th)
				(when (eq theory th)
				  (setf (get th 'rrules)
					(remove-duplicates
					 (list
					  (cons name
						(car (get th 'rrules))))))))
			    global-theory-list))
		       (get rule 'derived-in))
		      (get rule 'derived-in)))
		   ;it makes sense to invert a bidirectional rule, because we distinguish "forward" and "backward".
	      (list 'rtypelist (get rule 'rtypelist)))))
    (eval (nconc (list 'defrewrule name) rew))))

(defmexpr make-abbrev-rrule
  (argtypes symbol yesno)
  (argnames name bidir)
  (arghelp "Abbreviation to use" "Make a bidirectional rule?")
  (defaultfns (lambda (x y) (list x (if (eq y '$) t y))))
  (mhelp "Make a rewrite rule corresponding to a known
abbreviation."))

(defun type-to-string-2 (tp)
  (with-open-stream (string-stream (make-string-output-stream))
    (princ "(" string-stream)
    (type-to-lib-stream tp string-stream)
    (princ ")" string-stream)
    (get-output-stream-string string-stream)))

(defun make-abbrev-rrule (abbrev bidir)
  (if (abbrev-q abbrev)
      (let ((rew (list (list 'mhelp (cdr (assoc 'abbrev (get abbrev 'mhelp))))
		       (list 'bidirectional bidir)
		       (list 'variables nil)
		       (list 'derived-in nil)
		       (list 'before (getwff-subtype 'gwff-p 
						     (concatenate 'string (princ-to-string abbrev)
								  (type-to-string-2 (get abbrev 'type)))))
		       (list 'after (get abbrev 'defn))
		       (list 'active t)
		       (list 'rewfn nil) (list 'appfn nil)
		       (list 'rtypelist (get abbrev 'typelist)))))
	(eval (nconc (list 'defrewrule abbrev) rew)))
    (msgf abbrev " is not an abbreviation.")))

(defmexpr permute-rrules
  (mhelp "Permute the list of rewrite rules."))

(defun permute-rrules ()
  (let (newlist)
    (setq global-rewrite-rule-list (remove-if 'listp global-rewrite-rule-list))
    (prompt-read newlist nil (msgf "ORDER (LIST): New order for rules")
		 'rrulelist global-rewrite-rule-list ((? (mhelp 'rrulelist))))
  (do ()
      ((and (or (eq (length newlist) (length (remove-duplicates newlist)))
		(msgf "New list may not contain duplicate entries."))
	    ;relies on msgf returning NIL always.
	    (or (dolist (n newlist t) (unless (rewrule-p n) (return nil)))
		(msgf "New list can only contain existing rewrite rules."))
	    (or (eq (length newlist) (length global-rewrite-rule-list))
		(msgf "New list must contain all of the entries in the existing list."))
	    (or (not (setdiff newlist global-rewrite-rule-list))
		(not (setdiff global-rewrite-rule-list newlist))
		(msgf "New list is not a permutation of the old list.")))
       (setq global-rewrite-rule-list newlist))
      (prompt-read newlist nil (msgf "ORDER (LIST): New order for rules")
		   'rrulelist global-rewrite-rule-list ((? (mhelp 'rrulelist)))))))

(defmexpr delete-rrule
  (argnames rule)
  (argtypes rrule)
  (arghelp "Rule to delete")
  (mhelp "Delete a rewrite rule from TPS."))

(defun delete-rrule (rule)
  (remprop rule 'before)
  (remprop rule 'after)
  (remprop rule 'rewfn)
  (remprop rule 'appfn)
  (remprop rule 'active)
  (remprop rule 'bidirectional)
  (remprop rule 'rewrite-rule)
  (remprop rule 'rtypelist)
  (remprop rule 'variables)  ; mkaminski 10/30/2005
  (remprop rule 'derived-in) ;
  (setf (get rule 'mhelp) (remove-if #'(lambda (x) (eq (car x) 'rewrite-rule)) (get rule 'mhelp)))
  (setf (get rule 'core::contexts) (remove-if #'(lambda (x) (eq (car x) 'rewrite-rule)) (get rule 'core::contexts)))
  (setq global-rewrite-rule-list (delete rule global-rewrite-rule-list)))

(defedop arr1
  (alias apply-rrule-1)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Apply a rewrite rule (active or inactive) to the 
current edwff. If the rule is bidirectional, you will be 
prompted about which direction to apply it in."))

(defedop arr1*
  (alias apply-rrule-1*)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Apply a rewrite rule (active or inactive) repeatedly 
to the current edwff. If the rule is bidirectional, you will 
be prompted about which direction to apply it in.
CAUTION: may not terminate."))

(defedop arr
  (alias apply-rrule-any)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Apply one active rewrite rule to the current edwff; attempt 
different active rules in the order in which they are listed by 
LIST-RRULES until one works. If any current rules are 
bidirectional, you will be prompted about which direction to 
apply them in."))

(defedop arr*
  (alias apply-rrule-any*)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Apply one active rewrite rule to the current edwff; attempt 
different active rules in the order in which they are listed by 
LIST-RRULES until one works. If any current rules are 
bidirectional, you will be prompted about which direction to 
apply them in. Repeat this until no more rules
are applicable. CAUTION: may not terminate."))

(defedop unarr1
  (alias unapply-rrule-1)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Unapply a rewrite rule (active or inactive) to the current 
edwff. (i.e. apply it in the reverse direction).
If the rule is bidirectional, you will be prompted about
which direction to apply it in."))

(defedop unarr1*
  (alias unapply-rrule-1*)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Unapply a rewrite rule (active or inactive) repeatedly to 
the current edwff. (i.e. apply it in the reverse direction).
If the rule is bidirectional, you will be prompted about
which direction to apply it in.
CAUTION: may not terminate."))

(defedop unarr
  (alias unapply-rrule-any)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Unapply one active rewrite rule to the current edwff (i.e. apply
it in the reverse direction); attempt different active rules in the 
order in which they are listed by LIST-RRULES until one works.
If any current rules are bidirectional, you will be prompted 
about which direction to apply them in."))

(defedop unarr*
  (alias unapply-rrule-any*)
  (result-> edwff)
  (edwff-argname gwff)
  (mhelp "Unapply one active rewrite rule to the current edwff (i.e. 
apply it in the reverse direction); attempt different active rules in 
the order in which they are listed by LIST-RRULES until one works. 
Repeat this until no more rules are applicable. If any current rules
are bidirectional, you will be prompted about which direction to 
apply them in. CAUTION: may not terminate."))

(defwffop apply-rrule-1
  (argtypes gwff symbol)
  (wffargtypes "A" NIL)
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff rule)
  (arghelp "gwff before rewriting" "name of rule")
  (mhelp "Apply a rewrite rule (active or inactive) to the 
current edwff. If the rule is bidirectional, you will be 
prompted about which direction to apply it in."))

(defwffop apply-rrule-1*
  (argtypes gwff symbol)
  (wffargtypes "A" NIL)
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff rule)
  (arghelp "gwff before rewriting" "name of rule")
  (mhelp "Apply a rewrite rule (active or inactive) repeatedly 
to the current edwff. If the rule is bidirectional, you will 
be prompted about which direction to apply it in.
CAUTION: may not terminate."))

(defwffop apply-rrule-any
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff before rewriting")
  (mhelp "Apply one active rewrite rule to the current edwff; attempt 
different active rules in the order in which they are listed by 
LIST-RRULES until one works. If any current rules are 
bidirectional, you will be prompted about which direction to 
apply them in."))

(defwffop apply-rrule-any*
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff before rewriting")
  (mhelp "Apply one active rewrite rule to the current edwff; attempt 
different active rules in the order in which they are listed by 
LIST-RRULES until one works. If any current rules are 
bidirectional, you will be prompted about which direction to 
apply them in. Repeat this until no more rules
are applicable. CAUTION: may not terminate."))

(defwffop simplify-down
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff before rewriting")
  (mhelp "Apply any active rewrite rule
A --> B or A <--> B to the current gwff in the forward direction.
(i.e. subformulas A are rewritten to B, modulo any functions
attached to the rules, so that the resulting formula will be a 
rewrite instance of the original formula.)"))

(defun simplify-down (gwff)
  (apply-rrule-any gwff t))

(defwffop simplify-up
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff after rewriting")
  (mhelp "Apply any one active rewrite rule B <--> A in the backward 
direction. (i.e. subformulas A are rewritten to B, modulo any functions
attached to the rules, so that the original gwff will be a rewrite
instance of the resulting gwff.)"))

(defun simplify-up (gwff)
  (let ((global-rewrite-rule-list (remove-if #'(lambda (x) (or (not (rewrule-p x)) (not (get x 'bidirectional))))
					     global-rewrite-rule-list)))
    (unapply-rrule-any gwff nil)))

(defwffop simplify-down*
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff before rewriting")
  (mhelp "Apply all active rewrite rules
A --> B or A <--> B to the current gwff in the forward direction.
(i.e. subformulas A are rewritten to B, modulo any functions
attached to the rules, so that the resulting formula will be a 
rewrite instance of the original formula.)"))

(defun simplify-down* (gwff)
  (apply-rrule-any* gwff t))

(defwffop simplify-up*
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff after rewriting")
  (mhelp "Unapply all active rewrite rules
A --> B, and apply all active rewrite rules B <--> A in the backward 
direction. (i.e. subformulas A are rewritten to B, modulo any functions
attached to the rules, so that the original gwff will be a rewrite
instance of the resulting gwff.) "))

(defun simplify-up* (gwff)
  (let ((global-rewrite-rule-list (remove-if #'(lambda (x) (or (not (rewrule-p x)) (not (get x 'bidirectional))))
					     global-rewrite-rule-list)))
    (unapply-rrule-any* gwff nil)))

(defwffop unapply-rrule-1
  (argtypes gwff symbol)
  (wffargtypes "A" NIL)
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff rule)
  (arghelp "gwff after rewriting" "name of rule")
  (mhelp "Unapply a rewrite rule (active or inactive) to the current 
edwff. (i.e. apply it in the reverse direction).
If the rule is bidirectional, you will be prompted about
which direction to apply it in."))

(defwffop unapply-rrule-1*
  (argtypes gwff symbol)
  (wffargtypes "A" NIL)
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff rule)
  (arghelp "gwff after rewriting" "name of rule")
  (mhelp "Unapply a rewrite rule (active or inactive) repeatedly to 
the current edwff. (i.e. apply it in the reverse direction).
If the rule is bidirectional, you will be prompted about
which direction to apply it in.
CAUTION: may not terminate."))

(defwffop unapply-rrule-any
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff after rewriting")
  (mhelp "Unapply one active rewrite rule to the current edwff (i.e. apply
it in the reverse direction); attempt different active rules in the 
order in which they are listed by LIST-RRULES until one works.
If any current rules are bidirectional, you will be prompted 
about which direction to apply them in."))

(defwffop unapply-rrule-any*
  (argtypes gwff)
  (wffargtypes "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype gwff)
  (argnames gwff)
  (arghelp "gwff after rewriting")
  (mhelp "Unapply one active rewrite rule to the current edwff (i.e. 
apply it in the reverse direction); attempt different active rules in 
the order in which they are listed by LIST-RRULES until one works. 
Repeat this until no more rules are applicable. If any current rules
are bidirectional, you will be prompted about which direction to 
apply them in. CAUTION: may not terminate."))

(defun apply-rrule-1 (gwff rule &optional (dir 99))
  (let ((oneflag nil)
	(dir (if (eq dir 99) 
		 (if (get rule 'bidirectional)
		     (query "Apply rule in the forward direction?" t)
		   t)
	       dir)))
    (declare (special oneflag))
    (apply-rrule-poly gwff 
     (if (or dir (not (get rule 'bidirectional))) (get rule 'before) (get rule 'after))
     (if (or dir (not (get rule 'bidirectional))) (get rule 'after) (get rule 'before))     
     (get rule 'rewfn) (get rule 'appfn) (get rule 'rtypelist)
     #'(lambda (abbsym chkarg)
	 (declare (ignore abbsym chkarg) (special oneflag))
	 (prog1 (not oneflag) (setq oneflag t)))
     nil)))

(defun apply-rrule-1* (gwff rule &optional (dir 99) (prompt t))
  (let ((dir (if (eq dir 99)
		 (if (get rule 'bidirectional)
		     (query "Apply rule in the forward direction?" t)
		   t)
	       dir)))
  (do ((gwffout (apply-rrule-1 gwff rule dir) (apply-rrule-1 gwffout rule dir))
       (oldgwff nil gwffout)
       (count 0 (1+ count)))
      ((or (wffeq gwffout oldgwff)
	   (and prompt
		(= count 25)
		(setq count 0)
		(not (query "25 rules applied... continue?" nil))))
       gwffout))))

(defun unapply-rrule-1 (gwff rule &optional (dir 99))
  (unless (get rule 'rewfn)
   (let ((oneflag nil)
	 (dir (if (eq dir 99) 
		  (if (get rule 'bidirectional)
		      (query "Apply rule in the backward direction?" t)
		    t)
		dir)))
     (declare (special oneflag))
     (apply-rrule-poly gwff 
      (if (or dir (not (get rule 'bidirectional))) (get rule 'after) (get rule 'before))
      (if (or dir (not (get rule 'bidirectional))) (get rule 'before) (get rule 'after))
      (get rule 'rewfn) (get rule 'appfn) (get rule 'rtypelist)
     #'(lambda (abbsym chkarg)
	 (declare (ignore abbsym chkarg) (special oneflag))
	 (prog1 (not oneflag) (setq oneflag t)))
     nil))))

(defun unapply-rrule-1* (gwff rule &optional (dir 99) (prompt t))
  (unless (get rule 'rewfn)
  (let ((dir (if (eq dir 99)
		 (if (get rule 'bidirectional)
		     (query "Apply rule in the backward direction?" t)
		   t)
	       dir)))
    (do ((gwffout (unapply-rrule-1 gwff rule dir) (unapply-rrule-1 gwffout rule dir))
	 (oldgwff nil gwffout)
	 (count 0 (1+ count)))
	((or (wffeq gwffout oldgwff)
	     (and prompt
		  (= count 25)
		  (setq count 0)
		  (not (query "25 rules applied... continue?" nil))))
	 gwffout)))))

(defun bidir-rrules-exist ()
  (not (null (remove-if 'null (mapcar #'(lambda (x) (and (rewrule-p x) (get x 'bidirectional))) 
				      global-rewrite-rule-list)))))

(defun active-bidir-rrules-exist ()
  (not (null (remove-if 'null (mapcar #'(lambda (x) (and (active-p x) (get x 'bidirectional))) 
				      global-rewrite-rule-list)))))

(defun apply-rrule-any* (gwff &optional (dir 99) (prompt t))
  (setq dir (if (eq dir 99)
		(if (active-bidir-rrules-exist)
		    (query "Apply bidirectional rules in the forward direction?" t)
		  t)
	      dir))
  (do ((gwffout (apply-rrule-any gwff dir) (apply-rrule-any gwffout dir))
       (oldgwff nil gwffout)
       (count 0 (1+ count)))
      ((or (wffeq gwffout oldgwff)
	   (and prompt
		(= count 25)
		(setq count 0)
		(not (query "25 rules applied... continue?" nil))))
       gwffout)))

(defun apply-rrule-any (gwff &optional (dir 99))
  (setq dir (if (eq dir 99)
		(if (active-bidir-rrules-exist)
		    (query "Apply bidirectional rules in the forward direction?" t)
		  t)
	      dir))
  (let ((oneflag nil))
    (declare (special oneflag))
    (dolist (rule (remove-if-not 'active-p global-rewrite-rule-list) gwff)
	    (when (rewrule-p rule)
		  (setq gwff 
			(apply-rrule-poly gwff 
			 (if (or dir (not (get rule 'bidirectional))) (get rule 'before) (get rule 'after))
			 (if (or dir (not (get rule 'bidirectional))) (get rule 'after) (get rule 'before))
			 (get rule 'rewfn) (get rule 'appfn) (get rule 'rtypelist)
			 #'(lambda (abbsym chkarg)
			     (declare (ignore abbsym chkarg) (special oneflag))
			     (prog1 (not oneflag) (setq oneflag t)))
			 nil)))
	    (when oneflag (return gwff)))))

(defun unapply-rrule-any* (gwff &optional (dir 99) (prompt t))
  (setq dir (if (eq dir 99)
		(if (active-bidir-rrules-exist)
		    (query "Apply bidirectional rules in the backward direction?" t)
		  t)
	      dir))
  (do ((gwffout (unapply-rrule-any gwff dir) (unapply-rrule-any gwffout dir))
       (oldgwff nil gwffout)
       (count 0 (1+ count)))
      ((or (wffeq gwffout oldgwff)
	   (and prompt
		(= count 25)
		(setq count 0)
		(not (query "25 rules applied... continue?" nil))))
       gwffout)))

(defun unapply-rrule-any (gwff &optional (dir 99))
  (setq dir (if (eq dir 99)
		(if (active-bidir-rrules-exist)
		    (query "Apply bidirectional rules in the backward direction?" t)
		  t)
	      dir))
  (let ((oneflag nil))
    (declare (special oneflag))
    (dolist (rule (remove-if-not 'active-p global-rewrite-rule-list) gwff)
	    (when (and (rewrule-p rule) (not (get rule 'rewfn)))
		  (setq gwff 
			(apply-rrule-poly gwff
			 (if (or dir (not (get rule 'bidirectional))) (get rule 'after) (get rule 'before))
			 (if (or dir (not (get rule 'bidirectional))) (get rule 'before) (get rule 'after))
			 (get rule 'rewfn) (get rule 'appfn) (get rule 'rtypelist)
			 #'(lambda (abbsym chkarg)
			     (declare (ignore abbsym chkarg) (special oneflag))
			     (prog1 (not oneflag) (setq oneflag t)))
			 nil)))
	    (when oneflag (return gwff)))))

;;here are the functions that actually do the work

(defvar *binding-subs* nil)
(defvar *type-subs* nil)
(defvar *binding-subs-bdstack* nil)

(defun apply-rrule-poly (inwff before after func appfn types chkfn chkarg)
  (setq *binding-subs* nil)
  (setq *type-subs* nil)
  (setq *binding-subs-bdstack* nil)
  (cond ((lsymbol-q inwff)
	 (if (rrule-instance-poly appfn inwff before after types)
	     (if (funcall chkfn nil chkarg)
		 (if func (funcall func (replace-rrule-poly inwff before after types) before after)
		   (replace-rrule-poly inwff before after types))
	       inwff)
	   inwff))
	((boundwff-q inwff)
	 (if (rrule-instance-poly appfn inwff before after types)
	     (if (funcall chkfn nil chkarg)
		 (if func (funcall func (replace-rrule-poly inwff before after types) before after)
		   (replace-rrule-poly inwff before after types))
	       inwff)
	   (cons (car inwff)
		 (apply-rrule-poly (gdr inwff) before after func appfn types chkfn chkarg))))
	(t (if (rrule-instance-poly appfn inwff before after types)
	       (if (funcall chkfn nil chkarg)
		   (if func (funcall func (replace-rrule-poly inwff before after types) before after)
		     (replace-rrule-poly inwff before after types))
		 inwff)
	     (let ((newcar (apply-rrule-poly (car inwff) before after func appfn types chkfn chkarg)))
	       (cons newcar
		     (apply-rrule-poly (cdr inwff) before after func appfn types chkfn chkarg)))))))

(defun suggest-types (inwff before types)
  (let* ((t1 (type inwff))
	 (t2 (type before))
	 (tp (compare-typelists t1 t2)))
    (do ((check tp (cdr check))
	 (rlist nil))
	((null check) rlist)
	(when (or (and (assoc (caar check) (cdr check))
		       (not (equal (cdar check) (cdr (assoc (caar check) (cdr check))))))
		  (and (not (equal (caar check) (cdar check)))
		       (not (memq (caar check) types))))
	      (return nil))
	(when (and (memq (caar check) types)
		   (not (assoc (caar check) rlist)))
	      (push (car check) rlist)))))
	
(defun compare-typelists (t1 t2)
  (if (eq t1 t2) (list (cons t2 t1)) ;really do mean eq -- we want them to be single letters.
    (if (consp t1)
	(if (consp t2)
	    (append (compare-typelists (car t1) (car t2)) (compare-typelists (cdr t1) (cdr t2)))
	  (list (cons t2 t1)))
      (if (consp t2) nil (list (cons t2 t1))))))

; mkaminski 14/10/2005

;(defun rrule-instance-poly (appfn inwff before types)
;  (if appfn
;      (and (funcall appfn inwff)
;	   (rrule-instance-real inwff before types))
;    (rrule-instance-real inwff before types)))
;
;(defun rrule-instance-real (inwff before types)
;  (cond ((lsymbol-q before)
;	 (let ((newtypelist (suggest-types inwff before types)))
;	   (setq before (substitute-types newtypelist before))
;	   (setq *type-subs* (nconc newtypelist *type-subs*))
;	   (or (wffeq inwff before)
;	       (and (free-vars-of before) 
;		    (type-equal inwff before)
;		    (or (and (not (assoc before *binding-subs*)) (push (cons before inwff) *binding-subs*))
;			(wffeq-ab (cdr (assoc before *binding-subs*)) inwff))))))
;	((boundwff-q before)
;	 (and (boundwff-q inwff)
;	      (eq (binder inwff) (binder before))
;	      (push (cons (bdvar before) (bdvar inwff)) *binding-subs*)
;	      (rrule-instance-real (gdr inwff) (gdr before) types)))
;	((boundwff-q inwff) nil)
;	(t (and (consp inwff) (consp before)
;		(rrule-instance-real (car inwff) (car before) types)
;		(rrule-instance-real (cdr inwff) (cdr before) types)))))

; mkaminski 11/1/2005

;(defun rrule-instance-poly (appfn inwff before types)
;  (if appfn
;      (and (funcall appfn inwff)
;	   (rrule-instance-real inwff before types nil))
;    (rrule-instance-real inwff before types nil)))

(defun rrule-instance-poly (appfn inwff before after types)
  (if appfn
      (and (funcall appfn inwff before after types)
	   (rrule-instance-real inwff before types nil))
    (rrule-instance-real inwff before types nil)))

(defun rrule-instance-real (inwff before types bdsubs)
  (cond ((lsymbol-q before)
	 (let* ((newtypelist (suggest-types inwff before types))
		(before-type-inst (substitute-types newtypelist before)))
	   ;(setq before (substitute-types newtypelist before))
	   (setq *type-subs* (nconc newtypelist *type-subs*))
	   (cond ((assoc before bdsubs)
		  (wffeq-ab1 before inwff bdsubs))
		 ((assoc before *binding-subs*)
		  (let ((sub (cdr (assoc before *binding-subs*))))
		    (when (wffeq-ab sub inwff)
		      (let ((substack (assoc sub *binding-subs-bdstack*)))
			(rplacd substack (append bdsubs (cdr substack)))
			t))))
		 (t (or (wffeq inwff before-type-inst)
			(and (propsym-p before)
			     (not (anyabbrev-q before))
			     (type-equal inwff before-type-inst)
			     (push (cons inwff bdsubs) *binding-subs-bdstack*)
			     (push (cons before inwff) *binding-subs*)))))))
	((boundwff-q before)
	 (and (boundwff-q inwff)
	      (eq (binder inwff) (binder before))
	      (progn
		(setq *type-subs* (nconc (suggest-types (bdvar inwff)
							(bdvar before) types)
					 *type-subs*))
		t)
	      ;(push (cons (bdvar before) (bdvar inwff)) *binding-subs*)
	      (rrule-instance-real (gdr inwff) (gdr before) types
				   (acons (bdvar before)
					  (bdvar inwff) bdsubs))))
	((boundwff-q inwff) nil)
	(t (and (consp inwff) (consp before)
		(rrule-instance-real (car inwff) (car before) types bdsubs)
		(rrule-instance-real (cdr inwff) (cdr before) types bdsubs)))))

; mkaminski 15/10/2005 -- a variant of substitute-term-var which regards two
; variables as equal if they have the same name regardless of their types.

(defun substitute-lt-term-var (term var inwff)
  (or (subst-lt-term-rec (intern-subst term var) var inwff) inwff))

(defun lt-free-in (var inwff)
  (cond
   ((label-q inwff) (apply-label inwff (lt-free-in var inwff)))
   ((propsym-p inwff) (eq (getnameroot var)
			  (getnameroot inwff)))
   ((lsymbol-q inwff) (eq var inwff))
   ((boundwff-q inwff) (and (not (eq var (caar inwff)))
			    (lt-free-in var (cdr inwff))))
   (t (or (lt-free-in var (car inwff)) (lt-free-in var (cdr inwff))))))

(defun subst-lt-term-rec (term var inwff)
  (declare (special lt-restored-bdvars))
  (cond ((label-q inwff)
	 (apply-label inwff (subst-lt-term-rec term var inwff)))
  	((lsymbol-q inwff) (if (eq var inwff) term nil))
	((boundwff-q inwff)
	 (if (eq (bdvar inwff) var) nil
	   (let ((bdvarsub (cdr (assoc (bdvar inwff)
				       (cdr (assoc term *binding-subs-bdstack*
						   :test #'equal))))))
	     (if (and bdvarsub (not (member (bdvar inwff) lt-restored-bdvars)))
		 (progn (push bdvarsub lt-restored-bdvars)
			(subst-lt-term-rec
			 term var
			 (acons bdvarsub (binder inwff)
				(substitute-term-var bdvarsub (bdvar inwff)
						     (cdr inwff)))))
	       (cond ((lt-free-in (bdvar inwff) term)
		      (if (lt-free-in var (cdr inwff))
			  (if (member (bdvar inwff) lt-restored-bdvars)
			      (cons (car inwff)
				    (subst-lt-term-rec term var (cdr inwff)))
			    (let ((ren-inwff (rename-bd-var inwff)))
		      ;(setq lt-bdvar-subs (acons (bdvar ren-inwff)
			;			 (bdvar inwff) lt-bdvar-subs))
			      (subst-lt-term-rec term var ren-inwff)))
			nil))
		     (t (let ((new-wff (subst-lt-term-rec term var
							  (cdr inwff))))
			  (if new-wff (cons (car inwff) new-wff) nil))))))))
	(t (let ((left (or (subst-lt-term-rec term var (car inwff))
			   (car inwff)))
		 (right (or (subst-lt-term-rec term var (cdr inwff))
			    (cdr inwff))))
	     ;; Returns nil unless one of the two parts of the wff
	     ;; has been changed by the substitution
	     (unless (and (eq left (car inwff)) (eq right (cdr inwff)))
	       (cons left right))))))

(defun replace-rrule-poly (inwff before after types)
  (declare (ignore inwff before types)
	   (special lt-restored-bdvars))
  (setq lt-restored-bdvars nil)
  (let ((after2 after))
    (dolist (bs *binding-subs* after2)
	    (setq after2 (substitute-lt-term-var (cdr bs) (car bs) after2)))
    (setq after2 (substitute-types *type-subs* after2))))

(defmexpr activate-rules
  (argtypes rrulelist)
  (argnames rlist)
  (arghelp "Rules to activate")
  (defaultfns (lambda (x) (list (if (eq x '$) (remove-if-not 'rewrule-p global-rewrite-rule-list) x))))
  (mhelp "Activate a list of rewrite rules.
Activating a rule which is already active has no effect."))

(defun activate-rules (rlist)
  (dolist (r rlist)
	  (setf (get r 'active) t)))

(defmexpr deactivate-rules
  (argtypes rrulelist)
  (argnames rlist)
  (arghelp "Rules to deactivate")
  (defaultfns (lambda (x) (list (if (eq x '$) (remove-if-not 'rewrule-p global-rewrite-rule-list) x))))
  (mhelp "Deactivate a list of rewrite rules.
Deactivating a rule which is already inactive has no effect."))

(defun deactivate-rules (rlist)
  (dolist (r rlist)
	  (setf (get r 'active) nil)))

(defmexpr use-theory
  (argtypes theory)
  (argnames theory)
  (arghelp "Theory to activate")
  (mhelp "Activate all the rewrite rules in a theory, and
deactivate all other rewrite rules."))

; mkaminski 10/29/2005
(defvar *active-rewrite-theory* nil)

(defun use-theory (theory)
  (let ((rrules-used (rrules-used-by theory)))
    ; mkaminski 10/29/2005 -- added the following line
    (setq *active-rewrite-theory* theory)
    (dolist (r (remove-if-not 'rewrule-p global-rewrite-rule-list))
      (if (memq r rrules-used)
	  (setf (get r 'active) t)
	(setf (get r 'active) nil)))))

; mkaminski 10/29/2005
(defmexpr deactivate-theory
  (mhelp "Deactivate all the rewrite rules in the active theory."))

; mkaminski 10/29/2005
(defmexpr active-theory
  (mainfns (lambda () (princ *active-rewrite-theory*)))
  (mhelp "Show which theory is currently active. Any new derivation in the
REWRITING top level will use this theory."))

(defun deactivate-theory ();(theory)
  (let* ((theory *active-rewrite-theory*)
	 (rrules-used (rrules-used-by theory)))
    (setq *active-rewrite-theory* nil)
    (dolist (r (remove-if-not 'rewrule-p global-rewrite-rule-list))
      (if (memq r rrules-used)
	  (setf (get r 'active) nil)))))

(defun rrules-used-by (theory)
  (if (car (get theory 'extends))
      (append (car (get theory 'rrules)) 
	      (reduce 'append (mapcar 'rrules-used-by (car (get theory 'extends)))))
    (car (get theory 'rrules))))

(defvar *hopeless-fail* nil)

(defun apply-rrule-once (inwff outwff before after func appfn types)
  (setq *binding-subs* nil)
  (setq *type-subs* nil)
  (if *hopeless-fail* nil
    (cond ((lsymbol-q inwff)
	   (if (rrule-instance-poly appfn inwff before after types)
	       (let ((newwff (if func (funcall func (replace-rrule-poly inwff before after types))
			       (replace-rrule-poly inwff before after types))))
		 (if (wffeq-ab newwff outwff)
		     t
		   (if (wffeq-ab inwff outwff) nil (progn (setq *hopeless-fail* t) nil))))
	     (if (wffeq-ab inwff outwff) nil (progn (setq *hopeless-fail* t) nil))))
	  ((boundwff-q inwff)
	   (if (rrule-instance-poly appfn inwff before after types)
	       (let ((newwff (if func (funcall func (replace-rrule-poly inwff before after types))
			       (replace-rrule-poly inwff before after types))))
		 (if (wffeq-ab newwff outwff)
		     t
		   (apply-rrule-once (gdr inwff) (gdr outwff) before after func appfn types)))
	     (when (and (consp outwff))
	       (apply-rrule-once (gdr inwff) (gdr outwff) before after func appfn types))))
	  (t (if (rrule-instance-poly appfn inwff before after types)
		 (let ((newwff (if func (funcall func (replace-rrule-poly inwff before after types))
				 (replace-rrule-poly inwff before after types))))
		   (if (wffeq-ab newwff outwff)
		       t
		     (and (consp outwff)
			  (or (apply-rrule-once (car inwff) (car outwff) before after func appfn types)
			      (apply-rrule-once (cdr inwff) (cdr outwff) before after func appfn types)))))
	       (and (consp outwff)
		    (or (apply-rrule-once (car inwff) (car outwff) before after func appfn types)
			(apply-rrule-once (cdr inwff) (cdr outwff) before after func appfn types))))))))

;;
;(defun apply-rrule-once (inwff outwff before after func appfn types)
;  (apply-rrule-once1 inwff outwff before after func appfn types nil))
;
;(defun apply-rrule-once1 (inwff outwff before after func appfn types bdvars)
;  (setq *binding-subs* nil)
;  (setq *type-subs* nil)
;  (if *hopeless-fail* nil
;    (cond ((lsymbol-q inwff)
;	   (if (rrule-instance-poly appfn inwff before after types)
;	       (let ((newwff (if func (funcall func (replace-rrule-poly inwff before after types) before after)
;			       (replace-rrule-poly inwff before after types))))
;		 (if (wffeq-ab1 newwff outwff bdvars)
;		     t
;		   (if (wffeq-ab1 inwff outwff bdvars) nil
;		     (progn (setq *hopeless-fail* t) nil))))
;	     (if (wffeq-ab1 inwff outwff bdvars) nil
;	       (progn (setq *hopeless-fail* t) nil))))
;	  ((boundwff-q inwff)
;	   (if (rrule-instance-poly appfn inwff before after types)
;	       (let ((newwff (if func (funcall func (replace-rrule-poly inwff before after types) before after)
;			       (replace-rrule-poly inwff before after types))))
;		 (cond ((wffeq-ab1 newwff outwff bdvars) t)
;		       ((boundwff-q outwff)
;			(apply-rrule-once1 (gdr inwff) (gdr outwff)
;					   before after func appfn
;					   types (acons (bdvar inwff)
;							(bdvar outwff)
;							bdvars)))
;		       (t (setq *hopeless-fail* t) nil)))
;	     (when (and (consp outwff))
;	       (apply-rrule-once1 (gdr inwff) (gdr outwff) before after func
;				  appfn types bdvars))))
;	  (t (if (rrule-instance-poly appfn inwff before after types)
;		 (let ((newwff (if func (funcall func (replace-rrule-poly inwff before after types) before after)
;				 (replace-rrule-poly inwff before after types))))
;		   (if (wffeq-ab1 newwff outwff bdvars)
;		       t
;		     (and (consp outwff)
;			  (or (apply-rrule-once1 (car inwff) (car outwff) before after func appfn types bdvars)
;			      (apply-rrule-once1 (cdr inwff) (cdr outwff) before after func appfn types bdvars)))))
;	       (and (consp outwff)
;		    (or (apply-rrule-once1 (car inwff) (car outwff) before after func appfn types bdvars)
;			(apply-rrule-once1 (cdr inwff) (cdr outwff) before after func appfn types bdvars))))))))

(defwffop instance-of-rewriting
  (argtypes gwff gwff)
  (wffargtypes "A" "A")
  (wffop-typelist "A")
  (wffop-type "A")
  (resulttype boolean)
  (argnames inwff outwff)
  (arghelp "gwff before rewriting" "gwff after rewriting")
  (mhelp "Test to see whether one gwff can be obtained from another
by non-overlapping rewrite rules."))

(defun instance-of-rewriting (inwff outwff)
  (dolist (rule (remove-if-not 'active-p global-rewrite-rule-list) nil)
	  (when (rewrule-p rule)
		(setq *hopeless-fail* nil)
		(when (apply-rrule-once inwff outwff
					(get rule 'before) (get rule 'after)
					(get rule 'rewfn) (get rule 'appfn) (get rule 'rtypelist))
		      (when (not *hopeless-fail*) (return t)))
		(setq *hopeless-fail* nil)
		(when (get rule 'bidirectional)
		      (when (apply-rrule-once inwff outwff
					      (get rule 'after) (get rule 'before)
					      (get rule 'rewfn) (get rule 'appfn) (get rule 'rtypelist))
			    (when (not *hopeless-fail*) (return t)))))))

