;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-logic2b.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-logic2b.rules"
;;;
;;; contains rules
;;; EGEN RULEC 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-logic2b
  (contents EGEN RULEC ))

(context rule-commands)

(context RULES-4-QUANT)


;;;
;;; Rule: EGEN
;;;

;;; The rule command definition.

(defmexpr egen
  (argtypes line line gwff gwff gwff gwff linelist linelist)
  (wffargtypes  nil nil "A" "O" "A" "O" nil nil) (wffop-typelist "A")
  (argnames p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
  (arghelp "Existentially Quantified Line" "Line to be Existentially Generalized" "Term to be Generalized Upon" "Scope of Existential Quantifier" "Existentially Quantified Variable" "Assertion of Line to be Generalized" "Hypotheses" "Hypotheses")
  (defaultfns egen-defaults)
  (mainfns egen-legal egen-build)
  (enterfns egen-enter)
  (mhelp "Rule of Existential Generalization."))

;;; The line assertions justifications and restrictions

(defrulewffs egen
  (unique-lines 
     (egen-p2 "EXISTS x(A) A(O)")
     (egen-p1 "`(LCONTR  [[LAMBDA x(A) A(O)] t(A)])"))
  (unique-restrictions 
     (egen-restr0 (is-variable "x(A)"))))

;;; The suggesting rule definition.

(defsrule egen
  (matchfn egen-match)
  (match1fn egen-match1)
  (shortfn egen-short))


;;; The building function. 


(defun egen-build (p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (egen-mv2 egen-mv1 egen-mv0 egen-ml0))
               (unquoted maint::wffarg (|t| a |x| lcontr)))
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
    (append maint::new-line-labels
            (list |t| a |x| lcontr p2-hyps p1-hyps))))

;;; The entering function. 


(defun egen-enter (p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (egen-legal p2 p1 |t| a |x| lcontr p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun egen-defaults (p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (egen-mv2 egen-mv1 egen-mv0 egen-ml0))
               (unquoted maint::wffarg (|t| a |x| lcontr)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (egen-p2 egen-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (egen-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (egen-mv2 egen-mv1 egen-mv0 egen-ml0))
               (unquoted maint::wffarg (|t| a |x| lcontr)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (egen-hyp-defaults p2 p1 |t| a |x| lcontr
                                            p2-hyps p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps p1-hyps)))))
    (setq-destruct ((p2 'ss))
                   (line-no-defaults-from (eval-destruct ((p2 'ss)))))
    (when (not (member '$ (list p2 'ss)))
      (setq-destruct ((p1 'ss))
                     (line-no-defaults-to (eval-destruct ((p2 'ss)))
                                          (eval-destruct ((p1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil)
                  (mapcar #'specified-p (list |t| a |x| lcontr))
                  maint::strong-hypdefaults))
    (list p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun egen-hyp-defaults (p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults)
           (ignore |t| a |x| lcontr))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 p1))
               (unquoted maint::hyparg (p2-hyps p1-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
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
                          (fail
                           (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr (egen-restr0)))
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
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun egen-short (p2 |t|)
  (funcall #'comdecode
           (append (list 'egen) (list (linealias p2)) (list '$)
                   (append (list (append
                                  (list 'quote)
                                  (list |t|)
                                  'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun egen-legal (p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (egen-legal-hyps p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
    (egen-legal-wffs p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun egen-legal-hyps (p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore |t| a |x| lcontr))
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


(defun egen-legal-wffs (p2 p1 |t| a |x| lcontr p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (egen-mv2 egen-mv1 egen-mv0 egen-ml0))
               (unquoted maint::wffarg (|t| a |x| lcontr)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (egen-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (egen-p2 egen-p1))
               (unquoted maint::linearg (p2 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (egen-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: RULEC
;;;

;;; The rule command definition.

(defmexpr rulec
  (argtypes line line line line gwff gwff gwff gwff gwff linelist linelist linelist linelist)
  (wffargtypes  nil nil nil nil "A" "O" "A" "O" "O" nil nil nil nil) (wffop-typelist "A")
  (argnames p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps)
  (arghelp "Conclusion without Additional Hypothesis" "Existentially Quantified Line" "Conclusion with Additional Hypothesis" "Hypothesis with Chosen Variable" "Chosen Variable Name" "Scope of Existential Quantifier" "Existentially Quantified Variable" "Conclusion to be Proven Using Existentially Quantified Line" "Assertion of Hypothesis" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns rulec-defaults)
  (mainfns rulec-legal rulec-build)
  (enterfns rulec-enter)
  (mhelp "RuleC"))

;;; The line assertions justifications and restrictions

(defrulewffs rulec
  (unique-lines 
     (rulec-p4 "A(O)")
     (rulec-d1 "EXISTS x(A) B(O)")
     (rulec-d3 "A(O)")
     (rulec-h2 "`(LCONTR  [[LAMBDA x(A) B(O)] y(A)])"))
  (unique-restrictions 
     (rulec-restr0 (is-variable "y(A)"))
     (rulec-restr1 (not-free-in-hyps "y(A)"))
     (rulec-restr2 (not-free-in "y(A)" "EXISTS x(A) B(O)"))
     (rulec-restr3 (not-free-in "y(A)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule rulec
  (matchfn rulec-match)
  (match1fn rulec-match1)
  (shortfn rulec-short))


;;; The building function. 


(defun rulec-build
    (p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps)
  (let ((maint::new-line-labels (line-label-vec 4))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (rulec-mv3 rulec-mv2 rulec-mv1 rulec-mv0 rulec-ml0))
               (unquoted maint::wffarg (|y| b |x| a lcontr)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (rulec-p4 rulec-d1 rulec-d3 rulec-h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p4 d1 d3 h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "RuleC" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1 d3)))
                 (nextplan) (nextplan)
                 (list "Choose" (mapcar #'meta-subst '(rulec-mv3))
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg
                (p4-hyps d1-hyps d3-hyps h2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg
                (p4-hyps d1-hyps d3-hyps h2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps))))

;;; The entering function. 


(defun rulec-enter
    (p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (rulec-legal p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps
                 h2-hyps))
  (update-plan (eval-destruct ((p4 d1 'ss)))
               (eval-destruct ((d3 h2 'ss)))))

;;; The default function. 


(defun rulec-defaults
    (p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (rulec-mv3 rulec-mv2 rulec-mv1 rulec-mv0 rulec-ml0))
               (unquoted maint::wffarg (|y| b |x| a lcontr)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (rulec-p4 rulec-d1 rulec-d3 rulec-h2))
               (unquoted maint::linearg (p4 d1 d3 h2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (rulec-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (rulec-mv3 rulec-mv2 rulec-mv1 rulec-mv0 rulec-ml0))
               (unquoted maint::wffarg (|y| b |x| a lcontr)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p4-hyps d1-hyps d3-hyps h2-hyps))
      (if (not (member '$ (list p4 d1 d3 h2)))
          (setq-destruct (p4-hyps d1-hyps d3-hyps h2-hyps)
                         (rulec-hyp-defaults p4 d1 d3 h2 |y| b |x| a
                                             lcontr p4-hyps d1-hyps
                                             d3-hyps h2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p4-hyps d1-hyps d3-hyps h2-hyps)))))
    (setq-destruct ((p4 d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct ((p4 d1 'ss)))))
    (when (not (member '$ (list p4 d1 'ss)))
      (setq-destruct ((d3 h2 'ss))
                     (line-no-defaults-to (eval-destruct ((p4 d1 'ss)))
                                          (eval-destruct
                                           ((d3 h2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil nil)
                  (mapcar #'specified-p (list |y| b |x| a lcontr))
                  maint::strong-hypdefaults))
    (list p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps
          h2-hyps)))

;;; The hypotheses default function. 


(defun rulec-hyp-defaults
    (p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special maint::strong-hypdefaults)
           (ignore |y| b |x| a lcontr))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p4 d1 d3 h2))
               (unquoted maint::hyparg
                (p4-hyps d1-hyps d3-hyps h2-hyps)))
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
    (when (specified-p d1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d1-hyps (list)))))
    (when (specified-p d3-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d3-hyps (list h2)))))
    (when (specified-p h2-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h2-hyps (list h2)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p4-hyps d1-hyps d3-hyps h2-hyps)))
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
                             (list p4-hyps d1-hyps d3-hyps h2-hyps))))
             (when (not (specified-p p4-hyps))
               (setq p4-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p d3-hyps))
               (setq d3-hyps (ordered-join-h maint::hupper (list h2))))
             (when (not (specified-p h2-hyps))
               (setq h2-hyps (ordered-join-h maint::hupper (list h2))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p4-hyps d1-hyps d3-hyps
                                   h2-hyps))))))
    (list p4-hyps d1-hyps d3-hyps h2-hyps)))

;;; The matching function. 


(defun rulec-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'rulec-p4 'meta-assertion)
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
                                           'rulec-d1
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (rulec-restr0
                                           rulec-restr1
                                           rulec-restr2
                                           rulec-restr3)))
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
                    (fail t)))
         (let ((maint::rstr (get 'rulec-restr3 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun rulec-short (p4 d1 |y|)
  (funcall #'comdecode
           (append (list 'rulec) (list (linealias p4))
                   (list (linealias d1)) (list '$) (list '$)
                   (append (list (append
                                  (list 'quote)
                                  (list |y|)
                                  'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun rulec-legal
    (p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (rulec-legal-hyps p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps
                      d3-hyps h2-hyps)
    (rulec-legal-wffs p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps
                      d3-hyps h2-hyps)
    t))

;;; The hypotheses checking function. 


(defun rulec-legal-hyps
    (p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special rule-hupper rule-hlower)
           (ignore |y| b |x| a lcontr))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p4 d1 d3 h2))
               (unquoted maint::hyparg
                (p4-hyps d1-hyps d3-hyps h2-hyps)))
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
          (join-h maint::hlower (set-difference d3-hyps (list h2))))
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


(defun rulec-legal-wffs
    (p4 d1 d3 h2 |y| b |x| a lcontr p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p4-hyps d1-hyps d3-hyps h2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (rulec-mv3 rulec-mv2 rulec-mv1 rulec-mv0 rulec-ml0))
               (unquoted maint::wffarg (|y| b |x| a lcontr)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (rulec-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (rulec-p4 rulec-d1 rulec-d3 rulec-h2))
               (unquoted maint::linearg (p4 d1 d3 h2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr
                (rulec-restr0 rulec-restr1 rulec-restr2 rulec-restr3)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))
