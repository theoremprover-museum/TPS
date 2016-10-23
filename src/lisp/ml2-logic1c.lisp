;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-logic1c.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-logic1c.rules"
;;;
;;; contains rules
;;; INDIRECT INDIRECT2 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-logic1c
  (contents INDIRECT INDIRECT2 ))

(context rule-commands)

(context RULES-2-PROP)


;;;
;;; Rule: INDIRECT
;;;

;;; The rule command definition.

(defmexpr indirect
  (argtypes line line line gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" nil nil nil) (wffop-typelist)
  (argnames p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (arghelp "Line to be Proven by Contradiction" "Line with Contradiction" "Line with Assumed Negation" "Assertion to be Proven by Contradiction" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns indirect-defaults)
  (mainfns indirect-legal indirect-build)
  (enterfns indirect-enter)
  (mhelp "Rule of Indirect Proof."))

;;; The line assertions justifications and restrictions

(defrulewffs indirect
  (unique-lines 
     (indirect-p3 "A(O)")
     (indirect-p2 "FALSEHOOD")
     (indirect-h1 "~A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule indirect
  (matchfn indirect-match)
  (match1fn indirect-match1)
  (shortfn indirect-short))


;;; The building function. 


(defun indirect-build (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (indirect-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (indirect-p3 indirect-p2 indirect-h1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p3 p2 h1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Indirect" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list p2)))
                 (nextplan)
                 (list "Assume negation" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p3-hyps p2-hyps h1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p3-hyps p2-hyps h1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a p3-hyps p2-hyps h1-hyps))))

;;; The entering function. 


(defun indirect-enter (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (indirect-legal p3 p2 h1 a p3-hyps p2-hyps h1-hyps))
  (update-plan (eval-destruct ((p3 'ss)))
               (eval-destruct ((p2 h1 'ss)))))

;;; The default function. 


(defun indirect-defaults (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (indirect-mv0))
               (unquoted maint::wffarg (a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (indirect-p3 indirect-p2 indirect-h1))
               (unquoted maint::linearg (p3 p2 h1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (indirect-mv0))
               (unquoted maint::wffarg (a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p3-hyps p2-hyps h1-hyps))
      (if (not (member '$ (list p3 p2 h1)))
          (setq-destruct (p3-hyps p2-hyps h1-hyps)
                         (indirect-hyp-defaults p3 p2 h1 a p3-hyps
                           p2-hyps h1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p3-hyps p2-hyps h1-hyps)))))
    (setq-destruct ((p3 'ss))
                   (line-no-defaults-from (eval-destruct ((p3 'ss)))))
    (when (not (member '$ (list p3 'ss)))
      (setq-destruct ((p2 h1 'ss))
                     (line-no-defaults-to (eval-destruct ((p3 'ss)))
                                          (eval-destruct
                                           ((p2 h1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil) (mapcar #'specified-p (list a))
                  maint::strong-hypdefaults))
    (list p3 p2 h1 a p3-hyps p2-hyps h1-hyps)))

;;; The hypotheses default function. 


(defun indirect-hyp-defaults (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p3 p2 h1))
               (unquoted maint::hyparg (p3-hyps p2-hyps h1-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
                                (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p3-hyps)
      (setq maint::hupper (meet-h maint::hupper p3-hyps)))
    (when (specified-p p2-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p2-hyps (list h1)))))
    (when (specified-p h1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h1-hyps (list h1)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p3-hyps p2-hyps h1-hyps)))
      (progn (cond ((eq maint::hlower '$)
                    (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$)
                    (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference

                                 maint::hlower
                                 maint::hupper)
                                . linelist)
                               ".")))
             (when (not auto-generate-hyps)
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p3-hyps p2-hyps h1-hyps))))
             (when (not (specified-p p3-hyps))
               (setq p3-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p p2-hyps))
               (setq p2-hyps (ordered-join-h maint::hupper (list h1))))
             (when (not (specified-p h1-hyps))
               (setq h1-hyps (ordered-join-h maint::hupper (list h1))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p3-hyps p2-hyps h1-hyps))))))
    (list p3-hyps p2-hyps h1-hyps)))

;;; The matching function. 


(defun indirect-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'indirect-p3
                                       'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail
                           (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr nil))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when (%catch% (not
                                      (apply
                                       (car maint::rstr)
                                       (mapcar
                                        #'meta-subst
                                        (cdr maint::rstr))))
                                     (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun indirect-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'indirect-p3
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun indirect-short (p3)
  (funcall #'comdecode
           (append (list 'indirect) (list (linealias p3)) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun indirect-legal (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (indirect-legal-hyps p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
    (indirect-legal-wffs p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
    t))

;;; The hypotheses checking function. 


(defun indirect-legal-hyps (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p3 p2 h1))
               (unquoted maint::hyparg (p3-hyps p2-hyps h1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p3-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p2-hyps (list h1))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h1-hyps h1)))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
      (progn (cond ((eq maint::hlower '$)
                    (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$)
                    (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference

                                 maint::hlower
                                 maint::hupper)
                                . linelist)
                               ".")))))
    (setq rule-hupper maint::hupper)
    (setq rule-hlower maint::hlower)
    t))

;;; The restriction checking function. 


(defun indirect-legal-wffs (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p3-hyps p2-hyps h1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (indirect-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (indirect-p3 indirect-p2 indirect-h1))
               (unquoted maint::linearg (p3 p2 h1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: INDIRECT2
;;;

;;; The rule command definition.

(defmexpr indirect2
  (argtypes line line line line gwff gwff linelist linelist linelist linelist)
  (wffargtypes  nil nil nil nil "O" "O" nil nil nil nil) (wffop-typelist)
  (argnames p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)
  (arghelp "Line to be Proven by Contradiction" "Line with Negated Consequence of Assumption" "Line with Positive Consequence of Assumption" "Line with Assumed Negation" "One of Two Contradictory Consequences of Assumption" "Assertion to be Proven by Contradiction" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns indirect2-defaults)
  (mainfns indirect2-legal indirect2-build)
  (enterfns indirect2-enter)
  (mhelp "Rule of Indirect Proof Using Two Contradictory Lines."))

;;; The line assertions justifications and restrictions

(defrulewffs indirect2
  (unique-lines 
     (indirect2-p4 "A(O)")
     (indirect2-p3 "~B(O)")
     (indirect2-p2 "B(O)")
     (indirect2-h1 "~A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule indirect2
  (matchfn indirect2-match)
  (match1fn indirect2-match1)
  (shortfn indirect2-short))


;;; The building function. 


(defun indirect2-build
    (p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)
  (let ((maint::new-line-labels (line-label-vec 4))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (indirect2-mv1 indirect2-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (indirect2-p4 indirect2-p3 indirect2-p2 indirect2-h1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p4 p3 p2 h1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Indirect" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list p2 p3)))
                 (nextplan) (nextplan)
                 (list "Assume negation" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg
                (p4-hyps p3-hyps p2-hyps h1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg
                (p4-hyps p3-hyps p2-hyps h1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list b a p4-hyps p3-hyps p2-hyps h1-hyps))))

;;; The entering function. 


(defun indirect2-enter
    (p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (indirect2-legal p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps))
  (update-plan (eval-destruct ((p4 'ss)))
               (eval-destruct ((p2 h1 'ss) (p3 h1 'ss)))))

;;; The default function. 


(defun indirect2-defaults
    (p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (indirect2-mv1 indirect2-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (indirect2-p4 indirect2-p3 indirect2-p2 indirect2-h1))
               (unquoted maint::linearg (p4 p3 p2 h1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (indirect2-mv1 indirect2-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p4-hyps p3-hyps p2-hyps h1-hyps))
      (if (not (member '$ (list p4 p3 p2 h1)))
          (setq-destruct (p4-hyps p3-hyps p2-hyps h1-hyps)
                         (indirect2-hyp-defaults p4 p3 p2 h1 b a
                           p4-hyps p3-hyps p2-hyps h1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p4-hyps p3-hyps p2-hyps h1-hyps)))))
    (setq-destruct ((p4 'ss))
                   (line-no-defaults-from (eval-destruct ((p4 'ss)))))
    (when (not (member '$ (list p4 'ss)))
      (setq-destruct-multi (h1) ((p2 h1 'ss) (p3 h1 'ss))
                           (line-no-defaults-to (eval-destruct
                                                 ((p4 'ss)))
                                                (eval-destruct
                                                 ((p2 h1 'ss)
                                                  (p3 h1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)))

;;; The hypotheses default function. 


(defun indirect2-hyp-defaults
    (p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p4 p3 p2 h1))
               (unquoted maint::hyparg
                (p4-hyps p3-hyps p2-hyps h1-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
                                (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p4-hyps)
      (setq maint::hupper (meet-h maint::hupper p4-hyps)))
    (when (specified-p p3-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p3-hyps (list h1)))))
    (when (specified-p p2-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p2-hyps (list h1)))))
    (when (specified-p h1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h1-hyps (list h1)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p4-hyps p3-hyps p2-hyps h1-hyps)))
      (progn (cond ((eq maint::hlower '$)
                    (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$)
                    (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference

                                 maint::hlower
                                 maint::hupper)
                                . linelist)
                               ".")))
             (when (not auto-generate-hyps)
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p4-hyps p3-hyps p2-hyps h1-hyps))))
             (when (not (specified-p p4-hyps))
               (setq p4-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p p3-hyps))
               (setq p3-hyps (ordered-join-h maint::hupper (list h1))))
             (when (not (specified-p p2-hyps))
               (setq p2-hyps (ordered-join-h maint::hupper (list h1))))
             (when (not (specified-p h1-hyps))
               (setq h1-hyps (ordered-join-h maint::hupper (list h1))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p4-hyps p3-hyps p2-hyps
                                   h1-hyps))))))
    (list p4-hyps p3-hyps p2-hyps h1-hyps)))

;;; The matching function. 


(defun indirect2-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'indirect2-p4
                                       'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail
                           (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr nil))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when (%catch% (not
                                      (apply
                                       (car maint::rstr)
                                       (mapcar
                                        #'meta-subst
                                        (cdr maint::rstr))))
                                     (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun indirect2-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'indirect2-p4
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun indirect2-short (p4 b)
  (funcall #'comdecode
           (append (list 'indirect2) (list (linealias p4)) (list '$)
                   (list '$) (list '$)
                   (append (list (append (list 'quote) (list b) 'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun indirect2-legal
    (p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (indirect2-legal-hyps p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps
                          h1-hyps)
    (indirect2-legal-wffs p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps
                          h1-hyps)
    t))

;;; The hypotheses checking function. 


(defun indirect2-legal-hyps
    (p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p4 p3 p2 h1))
               (unquoted maint::hyparg
                (p4-hyps p3-hyps p2-hyps h1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p4-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p3-hyps (list h1))))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p2-hyps (list h1))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h1-hyps h1)))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
      (progn (cond ((eq maint::hlower '$)
                    (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$)
                    (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference

                                 maint::hlower
                                 maint::hupper)
                                . linelist)
                               ".")))))
    (setq rule-hupper maint::hupper)
    (setq rule-hlower maint::hlower)
    t))

;;; The restriction checking function. 


(defun indirect2-legal-wffs
    (p4 p3 p2 h1 b a p4-hyps p3-hyps p2-hyps h1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p4-hyps p3-hyps p2-hyps h1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (indirect2-mv1 indirect2-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (indirect2-p4 indirect2-p3 indirect2-p2 indirect2-h1))
               (unquoted maint::linearg (p4 p3 p2 h1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))
