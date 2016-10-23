;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-logic1b.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-logic1b.rules"
;;;
;;; contains rules
;;; INDIRECT1 CASES 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-logic1b
  (contents INDIRECT1 CASES ))

(context rule-commands)

(context RULES-2-PROP)


;;;
;;; Rule: INDIRECT1
;;;

;;; The rule command definition.

(defmexpr indirect1
  (argtypes line line line gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" nil nil nil) (wffop-typelist)
  (argnames p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
  (arghelp "Line to be Proven by Contradiction" "Line with Contradiction" "Line with Assumed Negation" "Positive Conjunct of Contradictory Assertion" "Assertion to be Proven by Contradiction" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns indirect1-defaults)
  (mainfns indirect1-legal indirect1-build)
  (enterfns indirect1-enter)
  (mhelp "Rule of Indirect Proof Using One Contradictory Line."))

;;; The line assertions justifications and restrictions

(defrulewffs indirect1
  (unique-lines 
     (indirect1-p3 "A(O)")
     (indirect1-p2 "B(O) AND ~B(O)")
     (indirect1-h1 "~A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule indirect1
  (matchfn indirect1-match)
  (match1fn indirect1-match1)
  (shortfn indirect1-short))


;;; The building function. 


(defun indirect1-build (p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (indirect1-mv1 indirect1-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (indirect1-p3 indirect1-p2 indirect1-h1))
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
    (append maint::new-line-labels (list b a p3-hyps p2-hyps h1-hyps))))

;;; The entering function. 


(defun indirect1-enter (p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (indirect1-legal p3 p2 h1 b a p3-hyps p2-hyps h1-hyps))
  (update-plan (eval-destruct ((p3 'ss)))
               (eval-destruct ((p2 h1 'ss)))))

;;; The default function. 


(defun indirect1-defaults (p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (indirect1-mv1 indirect1-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (indirect1-p3 indirect1-p2 indirect1-h1))
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
    (macro-do ((quoted maint::metavar (indirect1-mv1 indirect1-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p3-hyps p2-hyps h1-hyps))
      (if (not (member '$ (list p3 p2 h1)))
          (setq-destruct (p3-hyps p2-hyps h1-hyps)
                         (indirect1-hyp-defaults p3 p2 h1 b a p3-hyps
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
          (append '(nil nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)))

;;; The hypotheses default function. 


(defun indirect1-hyp-defaults (p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
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


(defun indirect1-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'indirect1-p3
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


(defun indirect1-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'indirect1-p3
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun indirect1-short (p3 b)
  (funcall #'comdecode
           (append (list 'indirect1) (list (linealias p3)) (list '$)
                   (list '$)
                   (append (list (append (list 'quote) (list b) 'nil)))
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun indirect1-legal (p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (indirect1-legal-hyps p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
    (indirect1-legal-wffs p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
    t))

;;; The hypotheses checking function. 


(defun indirect1-legal-hyps (p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun indirect1-legal-wffs (p3 p2 h1 b a p3-hyps p2-hyps h1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p3-hyps p2-hyps h1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (indirect1-mv1 indirect1-mv0))
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
                (indirect1-p3 indirect1-p2 indirect1-h1))
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
;;; Rule: CASES
;;;

;;; The rule command definition.

(defmexpr cases
  (argtypes line line line line line line gwff gwff gwff linelist linelist linelist linelist linelist linelist)
  (wffargtypes  nil nil nil nil nil nil "O" "O" "O" nil nil nil nil nil nil) (wffop-typelist)
  (argnames p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (arghelp "Conclusion for Both Cases" "Line with Disjunction" "Conclusion in Case 2" "Line with Assumption for Case 2 (Right Disjunct)" "Conclusion in Case 1" "Line with Assumption for Case 1 (Left Disjunct)" "Right Disjunct" "Left Disjunct" "Conclusion" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns cases-defaults)
  (mainfns cases-legal cases-build)
  (enterfns cases-enter)
  (mhelp "Rule of Cases."))

;;; The line assertions justifications and restrictions

(defrulewffs cases
  (unique-lines 
     (cases-p6 "C(O)")
     (cases-d1 "A(O) OR B(O)")
     (cases-p5 "C(O)")
     (cases-h4 "B(O)")
     (cases-p3 "C(O)")
     (cases-h2 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule cases
  (matchfn cases-match)
  (match1fn cases-match1)
  (shortfn cases-short))


;;; The building function. 


(defun cases-build
    (p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
     h2-hyps)
  (let ((maint::new-line-labels (line-label-vec 6))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (cases-mv2 cases-mv1 cases-mv0))
               (unquoted maint::wffarg (b a c)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (cases-p6 cases-d1 cases-p5 cases-h4 cases-p3
                 cases-h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p6 d1 p5 h4 p3 h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Cases" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1 p3 p5)))
                 (nextplan) (nextplan)
                 (list "Case 2" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))
                 (nextplan)
                 (list "Case 1" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg
                (p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps h2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg
                (p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps h2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
                  h2-hyps))))

;;; The entering function. 


(defun cases-enter
    (p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
     h2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (cases-legal p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps
                 h4-hyps p3-hyps h2-hyps))
  (update-plan (eval-destruct ((p6 d1 'ss)))
               (eval-destruct ((p3 h2 'ss) (p5 h4 'ss)))))

;;; The default function. 


(defun cases-defaults
    (p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
     h2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (cases-mv2 cases-mv1 cases-mv0))
               (unquoted maint::wffarg (b a c)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (cases-p6 cases-d1 cases-p5 cases-h4 cases-p3
                 cases-h2))
               (unquoted maint::linearg (p6 d1 p5 h4 p3 h2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (cases-mv2 cases-mv1 cases-mv0))
               (unquoted maint::wffarg (b a c)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$
                  (list p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
                        h2-hyps))
      (if (not (member '$ (list p6 d1 p5 h4 p3 h2)))
          (setq-destruct (p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
                          h2-hyps)
                         (cases-hyp-defaults p6 d1 p5 h4 p3 h2 b a c
                                             p6-hyps d1-hyps p5-hyps
                                             h4-hyps p3-hyps h2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
                            h2-hyps)))))
    (setq-destruct ((p6 d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct ((p6 d1 'ss)))))
    (when (not (member '$ (list p6 d1 'ss)))
      (setq-destruct ((p3 h2 'ss) (p5 h4 'ss))
                     (line-no-defaults-to (eval-destruct ((p6 d1 'ss)))
                                          (eval-destruct
                                           ((p3 h2 'ss)
                                            (p5 h4 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil nil nil nil)
                  (mapcar #'specified-p (list b a c))
                  maint::strong-hypdefaults))
    (list p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps
          p3-hyps h2-hyps)))

;;; The hypotheses default function. 


(defun cases-hyp-defaults
    (p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
     h2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a c))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p6 d1 p5 h4 p3 h2))
               (unquoted maint::hyparg
                (p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps h2-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
                                (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p6-hyps)
      (setq maint::hupper (meet-h maint::hupper p6-hyps)))
    (when (specified-p d1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d1-hyps (list)))))
    (when (specified-p p5-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p5-hyps (list h4)))))
    (when (specified-p h4-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h4-hyps (list h4)))))
    (when (specified-p p3-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p3-hyps (list h2)))))
    (when (specified-p h2-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h2-hyps (list h2)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
                            h2-hyps)))
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
                             (list p6-hyps d1-hyps p5-hyps h4-hyps
                                   p3-hyps h2-hyps))))
             (when (not (specified-p p6-hyps))
               (setq p6-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p p5-hyps))
               (setq p5-hyps (ordered-join-h maint::hupper (list h4))))
             (when (not (specified-p h4-hyps))
               (setq h4-hyps (ordered-join-h maint::hupper (list h4))))
             (when (not (specified-p p3-hyps))
               (setq p3-hyps (ordered-join-h maint::hupper (list h2))))
             (when (not (specified-p h2-hyps))
               (setq h2-hyps (ordered-join-h maint::hupper (list h2))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p6-hyps d1-hyps p5-hyps h4-hyps
                                   p3-hyps h2-hyps))))))
    (list p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps h2-hyps)))

;;; The matching function. 


(defun cases-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'cases-p6 'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail
                           (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (do ((maint::supps (cdr maint::plan-support)
                (cdr maint::supps))
               (maint::legal-supports nil))
              ((null maint::supps)
               (if (null maint::legal-supports)
                   (throwfail "No support line matched.")
                 (nreverse maint::legal-supports)))
            (let ((wffbindings wffbindings))
              (declare (special wffbindings))
              (%catch% (progn (match-bind (get
                                           'cases-d1
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted maint::restr nil))
                                        (let
                                         ((maint::rstr
                                           (get
                                            maint::restr
                                            'restr-call)))
                                         (when
                                          (%catch%
                                           (not
                                            (apply
                                             (car maint::rstr)
                                             (mapcar
                                              #'meta-subst
                                              (cdr maint::rstr))))
                                           (fail nil))
                                          (throwfail nil))))
                              (push (car maint::supps)
                                    maint::legal-supports))
                       (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun cases-match1 (pline maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'cases-p6 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'cases-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun cases-short (p6 d1)
  (funcall #'comdecode
           (append (list 'cases) (list (linealias p6))
                   (list (linealias d1)) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun cases-legal
    (p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
     h2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (cases-legal-hyps p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps
                      h4-hyps p3-hyps h2-hyps)
    (cases-legal-wffs p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps
                      h4-hyps p3-hyps h2-hyps)
    t))

;;; The hypotheses checking function. 


(defun cases-legal-hyps
    (p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
     h2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a c))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p6 d1 p5 h4 p3 h2))
               (unquoted maint::hyparg
                (p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps h2-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p6-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p5-hyps (list h4))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h4-hyps h4)))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p3-hyps (list h2))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h2-hyps h2)))
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


(defun cases-legal-wffs
    (p6 d1 p5 h4 p3 h2 b a c p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps
     h2-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p6-hyps d1-hyps p5-hyps h4-hyps p3-hyps h2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (cases-mv2 cases-mv1 cases-mv0))
               (unquoted maint::wffarg (b a c)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (cases-p6 cases-d1 cases-p5 cases-h4 cases-p3
                 cases-h2))
               (unquoted maint::linearg (p6 d1 p5 h4 p3 h2)))
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
