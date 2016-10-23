;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(deffile ml2-prior
  (part-of math-logic-2-rules)
  (extension lsp)
  (mhelp "Flag settings for Mathematical Logic II."))

(defmode math-logic-2-mode
  (flag-settings
   (first-order-mode-parse nil)
   (type-iota-mode t)
   (first-order-print-mode nil)
   (printtypes t)
   (treat-hlines-as-dlines t)
   (default-wffeq wffeq-ab))
  (mhelp "Mode to be used for Math Logic II."))


;;;Not defining the following, because we are not loading ADVICE
;;;facilities.


(defmacro defprior (rule priority)
  `(putprop ',rule ',priority 'priority))

(defmacro defhint (rule hint)
  `(putprop ',rule ',hint 'hint))

(defprior same 0)
(defhint same "A support line is the same as a planned line.")

(defprior cases 1)
(defhint cases "Some support line is a disjunction.")
(defprior econj 1)
(defhint econj "Some support line is a conjunction.")
(defprior rulec 1)
(defhint rulec "Some support line is existentially quantified.")
(defprior iconj 1)
(defhint iconj "Some planned line is a conjunction.")
(defprior ugen 1)
(defhint ugen "Some planned line is universally quantified.")
(defprior pullneg 1)
(defhint pullneg "Some planned line is negated.
   You could perhaps push in the negation.")
(defprior pushneg 1)
(defhint pushneg "Some support line is negated.
  You could perhaps push in the negation.")
(defprior implics-equiv 1)
(defhint implics-equiv "Some planned line is an equivalence.
  It could be rewritten as the conjunction of two implications.")
(defprior ext=0 1)
(defhint ext=0 "Some planned line is an equality at type O.
  Using extensionality it could be rewritten as an equivalence.")

(defprior deduct 2)
(defhint deduct "Some planned line is an implication.")

(defprior egen 3)
(defhint egen "Some planned line is existentially quantified.
   Perhaps you should think about which term to use for existential
   generalization.")
(defprior ui 3)
(defhint ui "Some support line is universally quantified.
  Perhaps you should think about which term to use for universal
  instantiation.")
(defprior ext= 3)
;(defhint EXT= "Some support line is an equality.  You may be able
;  to use extensionality to prove it.")
;;; Fixed 13MAR91 DAN
(defhint ext= "Some planned line is an equality.  You may be able
  to use extensionality to prove it.")

(defprior subst=l 5)
(defhint subst=l "One of the support lines is an equality.
  Perhaps you can replace its left-hand side by its right-hand side somewhere.")
(defprior subst=r 5)
(defhint subst=r "One of the support lines is an equality.
  Perhaps you can replace its right-hand side by its left-hand side somewhere.")
(defprior edef 4)
(defhint edef "One of the support lines is a definition.")
(defprior idef 4)
(defhint idef "One of the planned lines is a definition.")


(defprior mp 4)
(defhint mp "Some support line is an implication.
  A likely possibility here would be to try to establish its antecedent and
  prove the planned line from its succedent (using Modus Ponens).")

(defprior indirect 6)
(defhint indirect "I can't see much beyond what I have already told you.
  Of course, one could always try an indirect proof.")


(defprior lemma 9)
(defhint lemma "Using a lemma is usually unnecessarily complicated for simple problems.
  But if you have a good idea what that lemma should be, go ahead.")

(defprior hyp 10)
(defhint hyp "One can always introduce a new hypothesis.
But it rarely turns out to be useful, because it's hard to get rid of it.")

;;;the following priorities are new...
(defprior equiv-implics 1)
(defhint equiv-implics "Some support line is an equivalence.
  It could be rewritten as the conjunction of two implications.")

(defprior lcontr* 1)
(defhint lcontr* "Some support line is not in lambda-normal form.
Perhaps lambda-normalizing would help.")

(defprior lexpd* 1)
(defhint lexpd* "Some planned line is not in lambda-normal form.
Perhaps lambda-normalizing would help.")

(defprior equiv-eq-expd 5)
(defhint equiv-eq-expd "Some support line is an equality which can
be expanded using Leibniz' rule.")

(defprior equiv-eq-contr 5)
(defhint equiv-eq-contr "Some planned line is an equality which can
be expanded using Leibniz' rule.")

;;; Now comes a rule which was not formulated in the RULES package.

(context rules-1-misc)

(defmexpr assert
  (argtypes theorem line)
  (argnames theorem line)
  (arghelp "Name of Theorem" "Line with Theorem Instance")
  (mainfns assert-legal assert%)
  (mhelp "Use a theorem as a lemma in the current proof.
If the line already exists, ETPS will check whether it is a legal
instance of the theorem schema, otherwise it will prompt for the
metavariables in the theorem schema (usually x or P, Q, ...)."))

(defmexpr assert2
  (argtypes theorem line)
  (argnames theorem line)
  (arghelp "Name of Theorem" "Line with Theorem Instance")
  (mainfns assert-legal assert2)
  (mhelp "Use a theorem as a lemma in the current proof.
If the line already exists, ETPS will check whether it is a legal
instance of the theorem schema, otherwise it will prompt for the
metavariables in the theorem schema (usually x or P, Q, ...).
This version of ASSERT ensures correct behaviour for theorems
containing bound variables."))

;(context rulep-test)
(context RULES-2-PROP)

(defmexpr rulep
  (argtypes line linelist)
  (argnames conclusion antecedents)
  (arghelp "Line to be Proved" "Lines to Use")
  (defaultfns rulep-defaults)
  (mainfns setup-rulep)
  (enterfns rulep-enter-fn)
  (mhelp "Justify the CONSEQUENT line by RULEP using the lines in the
list ANTECEDENTS. "))

(defun rulep-enter-fn (plan-line existing-line-list)
  (rulep-legal-hyps plan-line existing-line-list)
  (apply #'rulep-enter (rulep-mainfun plan-line existing-line-list)))

(eval-when (compile load eval)

(defvar rulep-mainfn-list '(rulep-simple rulep-deluxe))

(deftype% rulep-mainfn-type
  (getfn testfn)
  (testfn (lambda (x) (if (memq x rulep-mainfn-list) x)))
  (printfn princ)
  (mhelp ("A RuleP main function. Currently, one of the following:
" (e (prinlc rulep-mainfn-list)))))
)

(defflag rulep-mainfn
  (flagtype rulep-mainfn-type)
  (default rulep-deluxe)
  (subjects rules-mod); was tps2-rulep)
  (mhelp "The main function used for RULEP.  Defaults to RULEP-DELUXE, in
which case RULEP will find a minimal subset of the support lines
which suffices to justify the planned line.  If set to RULEP-SIMPLE,
RULEP will merely check that the planned line follows from the
support lines that are specified by the user."))

#+comment(defun rulep-mainfun (plan-line existing-linelist)
  (if (memq rulep-mainfn rulep-mainfn-list)
      (funcall rulep-mainfn plan-line existing-linelist)
      (throwfail t rulep-mainfn " is not a valid RULEP function.")))


(defun rulep-mainfun (plan-line existing-linelist)
  (if (memq rulep-mainfn rulep-mainfn-list)
      (funcall rulep-mainfn plan-line existing-linelist)
      (throwfail t rulep-mainfn " is not a valid RULEP function.")))

(defun setup-rulep (plan-line-num supp-line-nums)
  (let* ((supp-lines (get-rulep-supp-lines supp-line-nums))
	 (plan-line (get-rulep-plan-line plan-line-num supp-lines)))
    (list plan-line supp-lines)))

(defun get-rulep-supp-lines (nums)
  (let ((supp-lines nil))
    (dolist (num nums (nreverse supp-lines))
      (push (get-a-line num) supp-lines))))

(defun get-rulep-plan-line (num supp-lines)
  (get-a-line num supp-lines))

(defun get-a-line (num &optional (supp-lines nil supp-lines-p))
  (let* ((existing-line (numalias num))
	 (new-line (or existing-line (car (line-label-vec 1)))))
    (unless existing-line
      ;; need support, hypotheses, linenumber, justification, assertion
      (setf (line-linenumber new-line) num)
      (setf (line-support new-line) nil)
      (setf (line-justification new-line) (nextplan))
      
      (setf (line-assertion new-line) 
	(prompt-read-return-vals 
	 (msg t "Assertion for line " num "?")
	 'gwff0 '$
	 ((? (msg t "Line does not exist, so I need an assertion.")))))
      (setf (line-hypotheses new-line)
	(if supp-lines-p
	    (sort 
	     (delete-duplicates
	      (copy-list
	       (apply #'append
		      (mapcar #'(lambda (x) (line-hypotheses x))
			      supp-lines))))
	     #'< :key #'linealias)
	  (prompt-read-return-vals 
	   (msg t "Hypotheses of line " num "?")
	   'existing-linelist 
	   (if supp-lines-p
	       supp-lines
	     (list new-line))
	   ((? (msg t "Line does not exist, so I need to know what its
hypotheses should be.")))))))
    new-line))

; The following is the implementation of the typesubst inference rule.
; It is not part of the rules package.  -- cebrown
(context rules-5-subst)

(defmexpr typesubst
  (argtypes line line typesym typesym)
  (argnames d p a b)
  (arghelp "Line before Type Substitution"
	   "Line after Type Substitution"
	   "Type Variable" "Type to Substitute for Type Variable")
  (defaultfns typesubst-defaults)
  (mainfns typesubst-legal typesubst-build)
  (mhelp "Substitute for a type variable in one line to infer another line.
The type variable must not appear in any hypothesis."))

; d: given line
; p: line to infer
; a: typevar
; b: type
; typesubst-legal checks that the typevar a does not
; occur in the hypotheses of d.  Also, if the line p
; already exists, this checks that the type substitution
; actually does give the formula in line p.
(defun typesubst-legal (d p a b)
  (let ((dlab (numalias d))
	(plab (numalias p)))
    (unless
     (and (typevar-p a)
	  (symbolp dlab)
	  (< d p)
	  (not-typevar-in-wffset
	   a (mapcar #'(lambda (x)
			 (line-assertion x))
		     (line-hypotheses dlab)))
	  (or (not plab)
	      (and (subsetp (line-hypotheses dlab)
			    (line-hypotheses plab))
		   (wffeq (line-assertion plab)
			  (substitute-types
			   (acons a b nil)
			   (line-assertion dlab))))))
     (throwfail "Illegal application of Type Subst."))))

; d: given line
; p: line to infer
; a: typevar
; b: type
; typesubst-build does the work of updating the proof outline.
(defun typesubst-build (d p a b)
  (let ((dummya ; absolute hack to see types where gwffs are expected
	 (read-from-string (format nil "_<~d>" (gensym))))
	(dummyb 
	 (read-from-string (format nil "_<~d>" (gensym))))
	(dlab (numalias d))
	(plab (numalias p)))
    (if plab
	(progn ; update plan structure
	  (setf (proof-plans dproof)
		(remove plab (proof-plans dproof)
			:test #'(lambda (x y)
				  (equal x (car y))))))
      (progn ; create new line
	(setq plab (gensym "LINE"))
	(setf (line-linenumber plab) p)
	(setf (line-hypotheses plab)
	      (line-hypotheses dlab))
	(setf (line-assertion plab)
	      (substitute-types (acons a b nil)
				(line-assertion dlab)))
	(let ((lines (proof-lines dproof))
	      (lines2 nil))
	  (do ((ll lines (cdr ll)))
	      ((not ll) (setq lines2 (reverse (cons plab lines2))))
	      (if (< (linealias (car ll)) p)
		  (setq lines2 (cons (car ll) lines2))
		(progn
		  (setq lines2 (append (reverse (cons plab lines2))
				       ll))
		  (return nil))))
	  (setf (proof-lines dproof) lines2)
	  (setf (proof-linealiases dproof)
		(mapcar #'(lambda (l)
			    (cons (line-linenumber l) l))
			lines2))
	  ; update support structure
	  (if (and (proof-plans dproof)
		   (member dlab (cdar (proof-plans dproof))))
	      (progn
		(unsponsor (caar (proof-plans dproof)) (list dlab))
		(sponsor (caar (proof-plans dproof)) (list plab)))))))
    (setf (get dummya 'type) a)
    (setf (get dummya 'unabbreviated-type) a)
    (setf (get dummyb 'type) b)
    (setf (get dummyb 'unabbreviated-type) b)
    (setf (line-justification plab)
	  (list "Type Subst" 
		(list dummyb dummya)
		(list dlab)))))

; The default function.  This is essentially a copy of
; the default function for substitute (substitute-defaults),
; with some changes (mainly deletions). -- cebrown
(defun typesubst-defaults (d1 d2 a b)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::unique-line (substitute-d1 substitute-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (substitute-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss d1))
                     (line-no-defaults-to (eval-destruct (('pp d1 'ss)))
                                          (eval-destruct (('pp d2 'ss d1))))))
    (list d1 d2 a b)))

