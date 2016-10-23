;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
;;This is an altered version of master-tactic.lisp, 
;;which adds the code for go2

(in-package :ml)

(deffile master-tactic
  (part-of tactics-nd)
  (extension lisp)
  (mhelp "Defines monstro-tac and ui-herbrand-tac for doing a lot of work
   in natural deduction proofs. Defines go2-tac which is same as
   monstro-tac, except that it does not invoke ui-herbrand-tac."))

(context compound-tactics)

;(eval-when (load eval)
;(alias monstro "use-tactic monstro-tac nat-ded"))
; redefined as a mexpr at the end of this file... MB 6/24/93

(deftactic monstro-tac
   (nat-ded
    (repeat
     (orelse
     ;(call ^p)
     ;(call prfw-pall)
      (call print-routines)
      same-tac
      refl=-tac
      sym=-tac
      rulep-tac
      prop-intro-rules-tac
      prop-elim-rules-tac
      pushneg-tac
      ugen-tac
      rulec-tac
      sub=-tac
      pullneg-tac
      ui-herbrand-tac
      indirect-exists-pline-tac
      indirect-disj-pline-tac
;     imp-disj-tac
      equiv-eq-contr-tac
      equiv-eq-expd-tac
      ext=-tac
      ext=0-tac
      elim-defns-tac
      lexpd*-vary-tac
      lcontr*-vary-tac))))

;(eval-when (load eval)
;(alias go2 "use-tactic go2-tac nat-ded"))
;redefined as a mexpr at the end of this file. MB 3/17/93

(deftactic go2-tac
   (nat-ded
    (repeat
     (orelse
     ;(call ^p)
     ;(call prfw-pall)
      (call print-routines)
      same-tac
      refl=-tac
      sym=-tac
      rulep-tac
      prop-intro-rules-tac
      prop-elim-rules-tac
      pushneg-tac
      ugen-tac
      rulec-tac
      sub=-tac
      pullneg-tac
      indirect-exists-pline-tac
      indirect-disj-pline-tac
;      imp-disj-tac
      equiv-eq-contr-tac
      equiv-eq-expd-tac
      ext=-tac
      ext=0-tac
      elim-defns-tac
      lexpd*-vary-tac
      lcontr*-vary-tac))))

(deftactic prop-intro-rules-tac
  (nat-ded
   (orelse
     truth-tac
     absurd-tac
     make-room
     iconj*-tac ;was just iconj-tac
     deduct-tac
     ineg-tac
     implics-equiv-tac
)))

(deftactic prop-elim-rules-tac
  (nat-ded
   (orelse
     indirect2-tac
     make-room
     econj*-tac ; was just econj-tac
     ;mp-tac this tactic can be nonproductive
     cases-tac
     equiv-implics-tac)))

(deftactic elim-defns-tac
  (nat-ded
   (orelse
    edef-tac
    idef-tac)))

(deftactic sub=-tac
  (nat-ded
   (orelse
    subst=l-tac
    subst=r-tac)))

(context prop-tactics)

(deftactic indirect-exists-pline-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not #'egen-match1 (list pline)))
	    (matched-plan
	     (if (eq tacmode 'interactive)
		 (find-if #'(lambda (x) 
			      (query 
			       (format nil 
				       "Apply INDIRECT-EXISTS-PLINE to line ~D?"
				       (linealias x)) t))
			  matched-plans)
	       (car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans
						   dproof))))
	    (oldsupports (cdr (assoc pline (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied INDIRECT-EXISTS-PLINE.")
		 (newplan nil)
		 (neg-line nil))
	     (tactic-output msg t)
	     (indirect-short pline)
	     (setq newplan
	       (car (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)))
	     (setq neg-line
	       (car (set-difference (cdr (assoc newplan (proof-plans
						    dproof)))
				    oldsupports)))
;added the above cdr 6/12/94 MB (without it, try using GO2 on THM120B...)
	     (pushneg-short neg-line)
	     (values (list newplan) msg 'succeed))
	   (let ((msg (if matched-plans
			  "Not applying INDIRECT-EXISTS-PLINE."
			  "Can't apply INDIRECT-EXISTS-PLINE.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
	   )))
   "Applies INDIRECT rule, then pushes negation through quantifier, 
if planned line is an existentially quantified line."))

(deftactic indirect-disj-pline-tac
  (nat-ded
   (lambda (pline)
     (let* ((matched-plans (remove-if-not 
			    #'(lambda (x) 
				(or-p (line-assertion x)))
			    (list pline)))
	    (matched-plan
	     (if (eq tacmode 'interactive)
		 (find-if #'(lambda (x) 
			      (query 
			       (format nil 
				       "Apply INDIRECT-DISJ-PLINE to line ~D?"
				       (linealias x)) t))
			  matched-plans)
	       (car matched-plans)))
	    (oldplans (delete pline (mapcar #'car (proof-plans
						   dproof))))
	    (oldsupports (cdr (assoc pline (proof-plans dproof)))))
       (if matched-plan
	   (let ((msg "Applied INDIRECT-DISJ-PLINE.")
		 (newplan nil)
		 (neg-line nil))
	     (tactic-output msg t)
	     (indirect-short pline)
	     (setq newplan
	       (car (set-difference (mapcar #'car (proof-plans dproof))
				     oldplans)))
	     (setq neg-line
	       (car (set-difference (cdr (assoc newplan (proof-plans
						    dproof)))
				    oldsupports)))
;added above cdr after doing same to indirect-pline-tac, above. MB
	     (pushneg-short neg-line)
	     (values (list newplan) msg 'succeed))
	   (let ((msg (if matched-plans
			  "Not applying INDIRECT-DISJ-PLINE."
			  "Can't apply INDIRECT-DISJ-PLINE.")))
	     (tactic-output msg nil)
	     (values (list pline) msg 'fail))
	   )))
   "Applies INDIRECT rule, then pushes negation through quantifier, 
if planned line is a disjunction."))

(context quant-tactics)

(defflag ui-herbrand-limit
  (flagtype posinteger)
  (subjects tactics)
  (default 3)
  (mhelp "Maximum number of times to apply ui-herbrand-tac to the
same universally-quantified formula."))

;;; make this a first-order version
(deftactic ui-herbrand-tac
  (nat-ded
   (lambda (pline)
     (let* ((supports (cdr (assoc pline (proof-plans dproof))))
	   (matched-supports
	    (remove-if #'(lambda (x) (>= (or (get x 'ui-herbrand-num) 0)
					ui-herbrand-limit))
		       (remove-if-not #'ui-match1 supports)))
	   (progress-made nil))
       (if matched-supports
	   (do* ((matched-supports matched-supports (cdr matched-supports))
		 (support (car matched-supports) 
			  (car matched-supports))
		 (matched-support
		  (if (and support (eq tacmode 'interactive))
		      (if (query (format nil 
					 "Apply UI-HERBRAND to line ~D?" 
					 (linealias support))
				 t)
			  support)
		    support)
		  (if (and support (eq tacmode 'interactive))
		      (if (query (format nil 
					 "Apply UI-HERBRAND to line ~D?" 
					 (linealias support))
				 t)
			  support)
		    support))
		 (msg (when matched-support
			(format nil "Applied UI-HERBRAND to line ~D." 
				(linealias matched-support)))
		      (when matched-support
			(format nil "Applied UI-HERBRAND to line ~D." 
				(linealias matched-support))))
		 (var-type 
		  (if matched-support
		      (or (get (caar (line-assertion matched-support)) 
			       'core::unabbreviated-type)
			  (get (caar (line-assertion matched-support)) 
			       'core::type)))
		  (if matched-support
		      (or (get (caar (line-assertion matched-support)) 
			       'core::unabbreviated-type)
			  (get (caar (line-assertion matched-support)) 
			       'core::type))))

		 (other-supports 
		  (remove matched-support supports)
		  (remove matched-support supports))
		 (terms 
		  (if matched-support
		      (make-possible-terms 
		       (reduce #'(lambda (x y) (cons (cons 'and x) y))
			       (mapcar #'(lambda (y) (line-assertion y))
				       (cons pline supports)))
		       var-type))
		  (or terms 
		      (if matched-support
		      (make-possible-terms 
		       (reduce #'(lambda (x y) (cons (cons 'and x) y))
			       (mapcar #'(lambda (y) (line-assertion y))
				       (cons pline supports)))
		       var-type))))
		 (used-terms
		  (when matched-support
		    (mapcan #'(lambda (x) 
				(if (and (string-equal "UI"
						       (line-just-rule x))
					 (eq (car (line-just-lines
						   x))
					     matched-support))
				    (copy-list (line-just-terms x))))
			    other-supports))
		  (when matched-support
		    (mapcan #'(lambda (x) 
				(if (and (string-equal "UI"
						       (line-just-rule x))
					 (eq (car (line-just-lines
						   x))
					     matched-support))
				    (copy-list (line-just-terms x))))
			    other-supports))))
	       ((null support)
		(values (list pline) "Applied UI-HERBRAND." 
			(if progress-made 'succeed 'fail)))
	     (when matched-support
	       (tactic-output msg t)
	       (setf (get matched-support 'ui-herbrand-num)
		 (if (get matched-support 'ui-herbrand-num)
		     (1+ (get matched-support 'ui-herbrand-num))
		   1))
	       (dolist (term (or (set-difference terms used-terms :test
						 #'equal)
				 (unless terms
				   (list (caar (line-assertion
						matched-support))))))
		 (setq progress-made t)
		 (make-room-after matched-support 1)
		 (comdecode (list 'ui (linealias matched-support)
				  (1+ (linealias matched-support))
				  (list 'quote term) '$ '$ '$ '$ '$))
		 )))
	 (let ((msg (if matched-supports
			"Not applying UI-HERBRAND."
		      "Can't apply UI-HERBRAND.")))
	   (tactic-output msg nil)
	   (values (list pline) msg 'fail)))))
   "UI-HERBRAND-TAC is a tactic for automatically applying universal
instantiation.  The terms that are used are generated by finding all
subterms of the appropriate type (except quantified variables) and applying
to them all functions of the appropriate type to get all possible new terms.
I.e., you can think of it as constructing the Herbrand universe one level
at a time.  The number of times that this can be done for any individual 
quantified formula is controlled by the flag UI-HERBRAND-LIMIT."))

(defun get-fns-of-type (wff type)
  "Return all functions in WFF which have TYPE as their codomain, or
free variables (functions of no arguments) of type TYPE."
  (cond ((lsymbol-q wff)
	 (let ((wfftype (get wff 'core::unabbreviated-type)))
	   (do ((wfftype* wfftype (car wfftype*)))
	       ((or (atom wfftype*) (equal wfftype* type))
		(if (equal wfftype* type) 
		    (if (equal wfftype* wfftype)
			(values nil (list wff))
		      (values (list wff) nil)))))))
	((boundwff-q wff)
	 (multiple-value-bind (fns terms)
		 (get-fns-of-type (cdr wff) type)
	   (values fns (delete (caar wff) terms))))
	(t
	 (multiple-value-bind (fns1 terms1)
	     (get-fns-of-type (car wff) type)
	   (multiple-value-bind (fns2 terms2)
		(get-fns-of-type (cdr wff) type)
	     (values 
	      (nconc fns1 fns2)
	      (nconc terms1 terms2 (if (equal (type wff) type) (list wff)))
	      ))))))

(defun make-possible-terms (wff type)
  (multiple-value-bind (fns terms)
      (get-fns-of-type wff type)
    (setq fns (delete-duplicates fns))
    (setq terms (delete-duplicates terms :test #'wffeq))
    (delete-duplicates
     (nconc terms (make-terms-of-type fns terms type))
     :test #'wffeq)))


#+comment
(defun make-terms-of-type (fns terms type)
  (mapcan 
   #'(lambda (fn)
       (make-terms-of-type-aux fn terms type))
   fns))

#+comment
(defun make-terms-of-type-aux (fn terms type)
  (if (equal (type fn) type)
      (list fn)
    (mapcan #'(lambda (term)
		(make-terms-of-type-aux (cons fn term) terms type))
	    terms)))

;;;There is a bug in the two functions above. I redefined
;;;make-terms-of-type in the following way.

(defun make-terms-of-type (fns terms type)
  (make-terms-of-type-fn fns terms type nil))

(defun make-terms-of-type-fn (fns terms type result)
  (declare (special result))
  (dolist (fn fns result)
     (make-terms-of-type-term fn terms type)))

(defun make-terms-of-type-term (fn terms type)
   (declare (special result))
   (if (equal (type fn) type)
       (push fn result)
       (dolist (term terms)
          (if (eq (cdr (type fn)) (type term))
              (make-terms-of-type-term (cons fn term) terms type)))))      


(defun get-subterms-of-type (wff type)
  "Return all subterms in WFF which have type TYPE."
  (nconc (if (equal (get wff 'unabbreviated-type) type)
	     (list wff))
	 (cond ((lsymbol-q wff) nil)
	       ((boundwff-q wff)
		(delete (caar wff) 
			(get-subterms-of-type (cdr wff) type)))
	       (t
		(nconc (get-fns-of-type (car wff) type)
		       (get-fns-of-type (cdr wff) type))))))

(context suggestions)

(defmexpr go2
  (argtypes tactic-mode)
  (argnames tacmode)
  (arghelp "Mode")
  (defaultfns
    (lambda (%1)
      (list (if (eq %1 '$) tacmode %1))))
  (mhelp "Apply all possible invertible tactics, until no more are possible.
This is equivalent to typing USE-TACTIC GO2-TAC NAT-DED.
The amount of output to the main window and the proofwindows is 
determined by the flag ETREE-NAT-VERBOSE."))

(defun go2 (tm)
  (use-tactic 'go2-tac 'nat-ded tm))

#+comment(defmexpr go3
  (argtypes tactic-mode)
  (argnames tacmode)
  (arghelp "Mode")
  (defaultfns
    (lambda (%1)
      (list (if (eq %1 '$) tacmode %1))))
  (mhelp "Exactly like GO2, but only sends output to the proofwindows."))

#+comment(defun go3 (tm)
  (use-tactic 'go3-tac 'nat-ded tm))

(defmexpr monstro
  (argtypes tactic-mode)
  (argnames tacmode)
  (arghelp "Mode")
  (defaultfns
    (lambda (%1)
      (list (if (eq %1 '$) tacmode %1))))
  (mhelp "This is equivalent to typing USE-TACTIC MONSTRO-TAC NAT-DED.
It applies all the same tactics as GO2, and also ui-herbrand-tac.
The amount of output to the main window and the proofwindows is 
determined by the flag ETREE-NAT-VERBOSE."))

(defun monstro (tm)
  (use-tactic 'monstro-tac 'nat-ded tm))
