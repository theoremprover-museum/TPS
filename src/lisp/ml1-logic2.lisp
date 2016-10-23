;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml1-logic2.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml1-logic2.rules"
;;;
;;; contains rules
;;; EGEN RULEC UI UGEN 
;;;

(in-package :ML)
(part-of MATH-LOGIC-1-RULES)

(defrulefile ml1-logic2
  (contents EGEN RULEC UI UGEN ))

(context rule-commands)

(context RULES-4-QUANT)


;;;
;;; Rule: EGEN
;;;

;;; The rule command definition.

(defmexpr egen
  (argtypes line line gwff gwff gwff gwff linelist linelist)
  (wffargtypes  nil nil "I" "O" "I" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 |t| a |x| s p2-hyps p1-hyps)
  (arghelp "Existentially Quantified Line" "Line without Existential Quantifier" "Term to be Generalized Upon" "Scope of Existential Quantifier" "Existentially Quantified Variable" "Assertion of Line to be Generalized" "Hypotheses" "Hypotheses")
  (defaultfns egen-defaults)
  (mainfns egen-legal egen-build)
  (enterfns egen-enter)
  (mhelp "Rule of Existential Generalization."))

;;; The line assertions justifications and restrictions

(defrulewffs egen
  (unique-lines 
     (egen-p2 "EXISTS x(I) A(O)")
     (egen-p1 "`(S  t(I)  x(I)  A(O))"))
  (unique-restrictions 
     (egen-restr0 (is-variable "x(I)"))
     (egen-restr1 (free-for "t(I)" "x(I)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule egen
  (matchfn egen-match)
  (match1fn egen-match1)
  (shortfn egen-short))


;;; The building function. 


(defun egen-build (p2 p1 |t| a |x| s p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (egen-mv2 egen-mv1 egen-mv0 egen-ml0))
               (unquoted maint::wffarg (|t| a |x| s)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (egen-p2 egen-p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p2 p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "EGen" (mapcar #'meta-subst '(egen-mv2))
                       (subst-labels maint::num-alist (list p1)))
                 (nextplan)))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list |t| a |x| s p2-hyps p1-hyps))))

;;; The entering function. 


(defun egen-enter (p2 p1 |t| a |x| s p2-hyps p1-hyps)
  (declare (ignore |t| a |x| s p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun egen-defaults (p2 p1 |t| a |x| s p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (egen-mv2 egen-mv1 egen-mv0 egen-ml0))
               (unquoted maint::wffarg (|t| a |x| s)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do ((quoted maint::unique-line (egen-p2 egen-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (egen-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (egen-mv2 egen-mv1 egen-mv0 egen-ml0))
               (unquoted maint::wffarg (|t| a |x| s)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (egen-hyp-defaults p2 p1 |t| a |x| s p2-hyps
                                            p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps p1-hyps)))))
    (setq-destruct ((p2 'ss))
                   (line-no-defaults-from (eval-destruct ((p2 'ss)))))
    (when (not (member '$ (list p2 'ss)))
      (setq-destruct ((p1 'ss))
                     (line-no-defaults-to (eval-destruct ((p2 'ss)))
                                          (eval-destruct ((p1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list |t| a |x| s))
                  maint::strong-hypdefaults))
    (list p2 p1 |t| a |x| s p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun egen-hyp-defaults (p2 p1 |t| a |x| s p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore |t| a |x| s))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 p1))
               (unquoted maint::hyparg (p2-hyps p1-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq maint::hyparg
                                       (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p2-hyps)
      (setq maint::hupper (meet-h maint::hupper p2-hyps)))
    (when (specified-p p1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p1-hyps (list)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps p1-hyps)))
      (progn (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$) (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference maint::hlower maint::hupper)
                                . linelist)
                               ".")))
             (when (not auto-generate-hyps)
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list p2-hyps p1-hyps))))
             (when (not (specified-p p2-hyps))
               (setq p2-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p p1-hyps))
               (setq p1-hyps (ordered-join-h maint::hupper (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list p2-hyps p1-hyps))))))
    (list p2-hyps p1-hyps)))

;;; The matching function. 


(defun egen-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'egen-p2 'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr (egen-restr0 egen-restr1)))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when (%catch% (not (apply
                                           (car maint::rstr)
                                           (mapcar
                                            #'meta-subst
                                            (cdr maint::rstr))))
                                     (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun egen-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'egen-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t
         (let ((maint::rstr (get 'egen-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'egen-restr1 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun egen-short (p2 |t|)
  (funcall #'comdecode
           (append (list 'egen) (list (linealias p2)) (list '$)
                   (append (list (append (list 'quote) (list |t|) 'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun egen-legal (p2 p1 |t| a |x| s p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (egen-legal-hyps p2 p1 |t| a |x| s p2-hyps p1-hyps)
    (egen-legal-wffs p2 p1 |t| a |x| s p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun egen-legal-hyps (p2 p1 |t| a |x| s p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore |t| a |x| s))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 p1))
               (unquoted maint::hyparg (p2-hyps p1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p2-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p1-hyps (list))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
      (progn (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$) (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference maint::hlower maint::hupper)
                                . linelist)
                               ".")))))
    (setq rule-hupper maint::hupper)
    (setq rule-hlower maint::hlower)
    t))

;;; The restriction checking function. 


(defun egen-legal-wffs (p2 p1 |t| a |x| s p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (egen-mv2 egen-mv1 egen-mv0 egen-ml0))
               (unquoted maint::wffarg (|t| a |x| s)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (egen-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (egen-p2 egen-p1))
               (unquoted maint::linearg (p2 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (egen-restr0 egen-restr1)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: RULEC
;;;

;;; The rule command definition.

(defmexpr rulec
  (argtypes line line line line gwff gwff gwff linelist linelist linelist linelist)
  (wffargtypes  nil nil nil nil "O" "I" "O" nil nil nil nil) (wffop-typelist)
  (argnames p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
  (arghelp "Conclusion without Additional Hypothesis" "Existentially Quantified Line" "Conclusion with Additional Hypothesis" "Hypothesis with Chosen Variable" "Scope of Existential Quantifier" "Existentially Quantified Variable" "Conclusion to be Proven Using Existentially Quantified Line" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns rulec-defaults)
  (mainfns rulec-legal rulec-build)
  (enterfns rulec-enter)
  (mhelp "Rule of Choice (RuleC)"))

;;; The line assertions justifications and restrictions

(defrulewffs rulec
  (unique-lines 
     (rulec-p4 "A(O)")
     (rulec-d1 "EXISTS x(I) B(O)")
     (rulec-p3 "A(O)")
     (rulec-h2 "B(O)"))
  (unique-restrictions 
     (rulec-restr0 (not-free-in-hyps "x(I)"))
     (rulec-restr1 (is-variable "x(I)"))
     (rulec-restr2 (not-free-in "x(I)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule rulec
  (matchfn rulec-match)
  (match1fn rulec-match1)
  (shortfn rulec-short))


;;; The building function. 


(defun rulec-build (p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
  (let ((maint::new-line-labels (line-label-vec 4))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (rulec-mv2 rulec-mv1 rulec-mv0))
               (unquoted maint::wffarg (b |x| a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (rulec-p4 rulec-d1 rulec-p3 rulec-h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p4 d1 p3 h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "RuleC" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1 p3)))
                 (nextplan) (nextplan)
                 (list "Choose" (mapcar #'meta-subst '(rulec-mv1))
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p4-hyps d1-hyps p3-hyps h2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p4-hyps d1-hyps p3-hyps h2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list b |x| a p4-hyps d1-hyps p3-hyps h2-hyps))))

;;; The entering function. 


(defun rulec-enter (p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
  (declare (ignore b |x| a p4-hyps d1-hyps p3-hyps h2-hyps))
  (update-plan (eval-destruct ((p4 d1 'ss))) (eval-destruct ((p3 h2 'ss)))))

;;; The default function. 


(defun rulec-defaults (p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (rulec-mv2 rulec-mv1 rulec-mv0))
               (unquoted maint::wffarg (b |x| a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do ((quoted maint::unique-line
                (rulec-p4 rulec-d1 rulec-p3 rulec-h2))
               (unquoted maint::linearg (p4 d1 p3 h2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (rulec-mv2 rulec-mv1 rulec-mv0))
               (unquoted maint::wffarg (b |x| a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p4-hyps d1-hyps p3-hyps h2-hyps))
      (if (not (member '$ (list p4 d1 p3 h2)))
          (setq-destruct (p4-hyps d1-hyps p3-hyps h2-hyps)
                         (rulec-hyp-defaults p4 d1 p3 h2 b |x| a p4-hyps
                                             d1-hyps p3-hyps h2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p4-hyps d1-hyps p3-hyps h2-hyps)))))
    (setq-destruct ((p4 d1 'ss))
                   (line-no-defaults-from (eval-destruct ((p4 d1 'ss)))))
    (when (not (member '$ (list p4 d1 'ss)))
      (setq-destruct ((p3 h2 'ss))
                     (line-no-defaults-to (eval-destruct ((p4 d1 'ss)))
                                          (eval-destruct ((p3 h2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil nil) (mapcar #'specified-p (list b |x| a))
                  maint::strong-hypdefaults))
    (list p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)))

;;; The hypotheses default function. 


(defun rulec-hyp-defaults
    (p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b |x| a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p4 d1 p3 h2))
               (unquoted maint::hyparg (p4-hyps d1-hyps p3-hyps h2-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq maint::hyparg
                                       (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p4-hyps)
      (setq maint::hupper (meet-h maint::hupper p4-hyps)))
    (when (specified-p d1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d1-hyps (list)))))
    (when (specified-p p3-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p3-hyps (list h2)))))
    (when (specified-p h2-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h2-hyps (list h2)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p4-hyps d1-hyps p3-hyps h2-hyps)))
      (progn (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$) (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference maint::hlower maint::hupper)
                                . linelist)
                               ".")))
             (when (not auto-generate-hyps)
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p4-hyps d1-hyps p3-hyps h2-hyps))))
             (when (not (specified-p p4-hyps))
               (setq p4-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p p3-hyps))
               (setq p3-hyps (ordered-join-h maint::hupper (list h2))))
             (when (not (specified-p h2-hyps))
               (setq h2-hyps (ordered-join-h maint::hupper (list h2))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p4-hyps d1-hyps p3-hyps h2-hyps))))))
    (list p4-hyps d1-hyps p3-hyps h2-hyps)))

;;; The matching function. 


(defun rulec-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'rulec-p4 'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (do ((maint::supps (cdr maint::plan-support) (cdr maint::supps))
               (maint::legal-supports nil))
              ((null maint::supps)
               (if (null maint::legal-supports)
                   (throwfail "No support line matched.")
                 (nreverse maint::legal-supports)))
            (let ((wffbindings wffbindings))
              (declare (special wffbindings))
              (%catch% (progn (match-bind (get 'rulec-d1 'meta-assertion)
                                          (get (car maint::supps) 'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (rulec-restr0
                                           rulec-restr1
                                           rulec-restr2)))
                                        (let
                                         ((maint::rstr
                                           (get maint::restr 'restr-call)))
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
                              (push (car maint::supps) maint::legal-supports))
                       (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun rulec-match1 (pline maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'rulec-p4 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'rulec-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'rulec-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'rulec-restr1 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'rulec-restr2 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun rulec-short (p4 d1)
  (funcall #'comdecode
           (append (list 'rulec) (list (linealias p4)) (list (linealias d1))
                   (list '$) (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun rulec-legal (p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (rulec-legal-hyps p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
    (rulec-legal-wffs p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
    t))

;;; The hypotheses checking function. 


(defun rulec-legal-hyps (p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b |x| a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p4 d1 p3 h2))
               (unquoted maint::hyparg (p4-hyps d1-hyps p3-hyps h2-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p4-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p3-hyps (list h2))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h2-hyps h2)))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
      (progn (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$) (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference maint::hlower maint::hupper)
                                . linelist)
                               ".")))))
    (setq rule-hupper maint::hupper)
    (setq rule-hlower maint::hlower)
    t))

;;; The restriction checking function. 


(defun rulec-legal-wffs (p4 d1 p3 h2 b |x| a p4-hyps d1-hyps p3-hyps h2-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p4-hyps d1-hyps p3-hyps h2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (rulec-mv2 rulec-mv1 rulec-mv0))
               (unquoted maint::wffarg (b |x| a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (rulec-p4 rulec-d1 rulec-p3 rulec-h2))
               (unquoted maint::linearg (p4 d1 p3 h2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (rulec-restr0 rulec-restr1 rulec-restr2)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: UI
;;;

;;; The rule command definition.

(defmexpr ui
  (argtypes line line gwff gwff gwff gwff linelist linelist)
  (wffargtypes  nil nil "I" "O" "I" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 |t| a |x| s d1-hyps d2-hyps)
  (arghelp "Universally Quantified Line" "Instantiated Line" "Substitution Term" "Scope of Universal Quantifier" "Universally Quantified Variable" "Assertion of Instantiated Line" "Hypotheses" "Hypotheses")
  (defaultfns ui-defaults)
  (mainfns ui-legal ui-build)
  (enterfns ui-enter)
  (mhelp "Rule of Universal Instantiation."))

;;; The line assertions justifications and restrictions

(defrulewffs ui
  (unique-lines 
     (ui-d1 "FORALL x(I) A(O)")
     (ui-d2 "`(S  t(I)  x(I)  A(O))"))
  (unique-restrictions 
     (ui-restr0 (is-variable "x(I)"))
     (ui-restr1 (free-for "t(I)" "x(I)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule ui
  (matchfn ui-match)
  (match1fn ui-match1)
  (shortfn ui-short))


;;; The building function. 


(defun ui-build (d1 d2 |t| a |x| s d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ui-mv2 ui-mv1 ui-mv0 ui-ml0))
               (unquoted maint::wffarg (|t| a |x| s)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (ui-d1 ui-d2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (d1 d2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((nextplan)
                 (list "UI" (mapcar #'meta-subst '(ui-mv2))
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list |t| a |x| s d1-hyps d2-hyps))))

;;; The entering function. 


(defun ui-enter (d1 d2 |t| a |x| s d1-hyps d2-hyps)
  (declare (ignore |t| a |x| s d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss d1)))))

;;; The default function. 


(defun ui-defaults (d1 d2 |t| a |x| s d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (ui-mv2 ui-mv1 ui-mv0 ui-ml0))
               (unquoted maint::wffarg (|t| a |x| s)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do ((quoted maint::unique-line (ui-d1 ui-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (ui-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (ui-mv2 ui-mv1 ui-mv0 ui-ml0))
               (unquoted maint::wffarg (|t| a |x| s)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (ui-hyp-defaults d1 d2 |t| a |x| s d1-hyps d2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss d1))
                     (line-no-defaults-to (eval-destruct (('pp d1 'ss)))
                                          (eval-destruct (('pp d2 'ss d1))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list |t| a |x| s))
                  maint::strong-hypdefaults))
    (list d1 d2 |t| a |x| s d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun ui-hyp-defaults (d1 d2 |t| a |x| s d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore |t| a |x| s))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d1 d2))
               (unquoted maint::hyparg (d1-hyps d2-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq maint::hyparg
                                       (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p d1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d1-hyps (list)))))
    (when (specified-p d2-hyps)
      (setq maint::hupper (meet-h maint::hupper d2-hyps)))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d1-hyps d2-hyps)))
      (progn (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$) (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference maint::hlower maint::hupper)
                                . linelist)
                               ".")))
             (when (not auto-generate-hyps)
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list d1-hyps d2-hyps))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p d2-hyps))
               (setq d2-hyps (ordered-join-h maint::hlower (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list d1-hyps d2-hyps))))))
    (list d1-hyps d2-hyps)))

;;; The matching function. 


(defun ui-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan nil)
    (setq maint::matched-support
          (do ((maint::supps (cdr maint::plan-support) (cdr maint::supps))
               (maint::legal-supports nil))
              ((null maint::supps)
               (if (null maint::legal-supports)
                   (throwfail "No support line matched.")
                 (nreverse maint::legal-supports)))
            (let ((wffbindings wffbindings))
              (declare (special wffbindings))
              (%catch% (progn (match-bind (get 'ui-d1 'meta-assertion)
                                          (get (car maint::supps) 'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (ui-restr0 ui-restr1)))
                                        (let
                                         ((maint::rstr
                                           (get maint::restr 'restr-call)))
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
                              (push (car maint::supps) maint::legal-supports))
                       (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun ui-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'ui-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'ui-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'ui-restr1 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun ui-short (d1 |t|)
  (funcall #'comdecode
           (append (list 'ui) (list (linealias d1)) (list '$)
                   (append (list (append (list 'quote) (list |t|) 'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun ui-legal (d1 d2 |t| a |x| s d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (ui-legal-hyps d1 d2 |t| a |x| s d1-hyps d2-hyps)
    (ui-legal-wffs d1 d2 |t| a |x| s d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun ui-legal-hyps (d1 d2 |t| a |x| s d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore |t| a |x| s))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d1 d2))
               (unquoted maint::hyparg (d1-hyps d2-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hupper (meet-h maint::hupper d2-hyps))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
      (progn (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$) (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference maint::hlower maint::hupper)
                                . linelist)
                               ".")))))
    (setq rule-hupper maint::hupper)
    (setq rule-hlower maint::hlower)
    t))

;;; The restriction checking function. 


(defun ui-legal-wffs (d1 d2 |t| a |x| s d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ui-mv2 ui-mv1 ui-mv0 ui-ml0))
               (unquoted maint::wffarg (|t| a |x| s)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (ui-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (ui-d1 ui-d2))
               (unquoted maint::linearg (d1 d2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (ui-restr0 ui-restr1)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: UGEN
;;;

;;; The rule command definition.

(defmexpr ugen
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "I" nil nil) (wffop-typelist)
  (argnames p2 p1 a |x| p2-hyps p1-hyps)
  (arghelp "Universally Quantified Line" "Line with Scope of Universal Quantifier" "Scope of Universal Quantifier" "Universally Quantified Variable" "Hypotheses" "Hypotheses")
  (defaultfns ugen-defaults)
  (mainfns ugen-legal ugen-build)
  (enterfns ugen-enter)
  (mhelp "Rule of Universal Generalization."))

;;; The line assertions justifications and restrictions

(defrulewffs ugen
  (unique-lines 
     (ugen-p2 "FORALL x(I) A(O)")
     (ugen-p1 "A(O)"))
  (unique-restrictions 
     (ugen-restr0 (not-free-in-hyps "x(I)"))
     (ugen-restr1 (is-variable "x(I)"))))

;;; The suggesting rule definition.

(defsrule ugen
  (matchfn ugen-match)
  (match1fn ugen-match1)
  (shortfn ugen-short))


;;; The building function. 


(defun ugen-build (p2 p1 a |x| p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ugen-mv1 ugen-mv0))
               (unquoted maint::wffarg (a |x|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (ugen-p2 ugen-p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p2 p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "UGen" (mapcar #'meta-subst '(ugen-mv0))
                       (subst-labels maint::num-alist (list p1)))
                 (nextplan)))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a |x| p2-hyps p1-hyps))))

;;; The entering function. 


(defun ugen-enter (p2 p1 a |x| p2-hyps p1-hyps)
  (declare (ignore a |x| p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun ugen-defaults (p2 p1 a |x| p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (ugen-mv1 ugen-mv0))
               (unquoted maint::wffarg (a |x|)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do ((quoted maint::unique-line (ugen-p2 ugen-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (ugen-mv1 ugen-mv0))
               (unquoted maint::wffarg (a |x|)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (ugen-hyp-defaults p2 p1 a |x| p2-hyps p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps p1-hyps)))))
    (setq-destruct ((p2 'ss))
                   (line-no-defaults-from (eval-destruct ((p2 'ss)))))
    (when (not (member '$ (list p2 'ss)))
      (setq-destruct ((p1 'ss))
                     (line-no-defaults-to (eval-destruct ((p2 'ss)))
                                          (eval-destruct ((p1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list a |x|))
                  maint::strong-hypdefaults))
    (list p2 p1 a |x| p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun ugen-hyp-defaults (p2 p1 a |x| p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a |x|))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 p1))
               (unquoted maint::hyparg (p2-hyps p1-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq maint::hyparg
                                       (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p2-hyps)
      (setq maint::hupper (meet-h maint::hupper p2-hyps)))
    (when (specified-p p1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p1-hyps (list)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps p1-hyps)))
      (progn (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$) (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference maint::hlower maint::hupper)
                                . linelist)
                               ".")))
             (when (not auto-generate-hyps)
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list p2-hyps p1-hyps))))
             (when (not (specified-p p2-hyps))
               (setq p2-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p p1-hyps))
               (setq p1-hyps (ordered-join-h maint::hupper (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list p2-hyps p1-hyps))))))
    (list p2-hyps p1-hyps)))

;;; The matching function. 


(defun ugen-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'ugen-p2 'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr (ugen-restr0 ugen-restr1)))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when (%catch% (not (apply
                                           (car maint::rstr)
                                           (mapcar
                                            #'meta-subst
                                            (cdr maint::rstr))))
                                     (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun ugen-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'ugen-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t
         (let ((maint::rstr (get 'ugen-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'ugen-restr1 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun ugen-short (p2)
  (funcall #'comdecode
           (append (list 'ugen) (list (linealias p2)) (list '$) (list '$)
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun ugen-legal (p2 p1 a |x| p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (ugen-legal-hyps p2 p1 a |x| p2-hyps p1-hyps)
    (ugen-legal-wffs p2 p1 a |x| p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun ugen-legal-hyps (p2 p1 a |x| p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a |x|))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 p1))
               (unquoted maint::hyparg (p2-hyps p1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p2-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p1-hyps (list))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
      (progn (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
                   ((eq maint::hupper '$) (setq maint::hupper maint::hlower))
                   ((not (contained-p maint::hlower maint::hupper))
                    (throwfail "Illegal extra hypotheses in conclusion: "
                               ((set-difference maint::hlower maint::hupper)
                                . linelist)
                               ".")))))
    (setq rule-hupper maint::hupper)
    (setq rule-hlower maint::hlower)
    t))

;;; The restriction checking function. 


(defun ugen-legal-wffs (p2 p1 a |x| p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ugen-mv1 ugen-mv0))
               (unquoted maint::wffarg (a |x|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (ugen-p2 ugen-p1))
               (unquoted maint::linearg (p2 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (ugen-restr0 ugen-restr1)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))
