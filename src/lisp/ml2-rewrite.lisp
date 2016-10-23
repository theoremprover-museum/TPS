;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/afs/cs.cmu.edu/project/tps/tps/lisp/ml2-rewrite.lisp"
;;;  assembled from "/afs/cs.cmu.edu/project/tps/tps/lisp/ml2-rewrite.rules"
;;;
;;; contains rules
;;; USE-RRULES SIMPLIFY-PLAN SIMPLIFY-SUPP SIMPLIFY-PLAN* SIMPLIFY-SUPP* UNREWRITE-PLAN* REWRITE-SUPP* UNREWRITE-PLAN1 REWRITE-SUPP1 
;;;

(in-package :ML)
(part-of ML2-REWRITE)

(defrulefile ml2-rewrite
  (contents USE-RRULES SIMPLIFY-PLAN SIMPLIFY-SUPP SIMPLIFY-PLAN* SIMPLIFY-SUPP* UNREWRITE-PLAN* REWRITE-SUPP* UNREWRITE-PLAN1 REWRITE-SUPP1 ))

(context rule-commands)

(context REWRITING)


;;;
;;; Rule: USE-RRULES
;;;

;;; The rule command definition.

(defmexpr use-rrules
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a b p2-hyps p1-hyps)
  (arghelp "Line after rewriting (higher-numbered)" "Line before rewriting (lower-numbered)" "Wff before rewriting" "Wff after rewriting" "Hypotheses" "Hypotheses")
  (defaultfns use-rrules-defaults)
  (mainfns use-rrules-legal use-rrules-build)
  (enterfns use-rrules-enter)
  (mhelp "Rewrite a line. The line may be rewritten several steps,
but rewrites may not be nested."))

;;; The line assertions justifications and restrictions

(defrulewffs use-rrules
  (unique-lines 
     (use-rrules-p2 "B(O)")
     (use-rrules-p1 "A(O)"))
  (unique-restrictions 
     (use-rrules-restr0 (instance-of-rewriting "A(O)" "B(O)"))))

;;; The suggesting rule definition.

(defsrule use-rrules
  (matchfn use-rrules-match)
  (match1fn use-rrules-match1)
  (shortfn use-rrules-short))


;;; The building function. 


(defun use-rrules-build (p2 p1 a b p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (use-rrules-mv1 use-rrules-mv0))
      (unquoted maint::wffarg (a b)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (use-rrules-p2 use-rrules-p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (p2 p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((list "Rewrite"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list p1)))
        (nextplan)))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a b p2-hyps p1-hyps))))

;;; The entering function. 


(defun use-rrules-enter (p2 p1 a b p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (use-rrules-legal p2 p1 a b p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun use-rrules-defaults (p2 p1 a b p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (use-rrules-mv1 use-rrules-mv0))
      (unquoted maint::wffarg (a b)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (use-rrules-p2 use-rrules-p1))
      (unquoted maint::linearg (p2 p1)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (use-rrules-mv1 use-rrules-mv0))
      (unquoted maint::wffarg (a b)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (use-rrules-hyp-defaults p2 p1 a b p2-hyps p1-hyps))
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
                  (mapcar #'specified-p (list a b))
                  maint::strong-hypdefaults))
    (list p2 p1 a b p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun use-rrules-hyp-defaults (p2 p1 a b p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a b))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun use-rrules-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn
           (%catch%
            (match-bind (get 'use-rrules-p2 'meta-assertion)
                        (get (car maint::plan-support) 'assertion))
            (fail (throwfail "Planned lines did not match.")))
           (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr (use-rrules-restr0)))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when
                          (%catch%
                           (not
                            (apply (car maint::rstr)
                                   (mapcar #'meta-subst (cdr maint::rstr))))
                           (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun use-rrules-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and
     (not
      (eq 'maint::failed
          (%catch%
           (match-bind (get 'use-rrules-p2 'meta-assertion)
                       (get pline 'assertion))
           (fail 'maint::failed))))
     t
     (let ((maint::rstr (get 'use-rrules-restr0 'restr-call)))
       (%catch%
        (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
        (fail t))))))

;;; The short version of the rule as a function.  


(defun use-rrules-short (p2 a)
  (funcall #'comdecode
           (append (list 'use-rrules)
                   (list (linealias p2))
                   (list '$)
                   (append (list (append (list 'quote) (list a) 'nil)))
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun use-rrules-legal (p2 p1 a b p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (use-rrules-legal-hyps p2 p1 a b p2-hyps p1-hyps)
    (use-rrules-legal-wffs p2 p1 a b p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun use-rrules-legal-hyps (p2 p1 a b p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a b))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p2-hyps))
    (setq maint::hlower (join-h maint::hlower (set-difference p1-hyps (list))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun use-rrules-legal-wffs (p2 p1 a b p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (use-rrules-mv1 use-rrules-mv0))
      (unquoted maint::wffarg (a b)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (use-rrules-p2 use-rrules-p1))
      (unquoted maint::linearg (p2 p1)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (use-rrules-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))


;;;
;;; Rule: SIMPLIFY-PLAN
;;;

;;; The rule command definition.

(defmexpr simplify-plan
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a simplify-up p2-hyps p1-hyps)
  (arghelp "Line after rewriting (higher-numbered)" "Line before rewriting (lower-numbered)" "Wff after rewriting" "Wff before rewriting" "Hypotheses" "Hypotheses")
  (defaultfns simplify-plan-defaults)
  (mainfns simplify-plan-legal simplify-plan-build)
  (enterfns simplify-plan-enter)
  (mhelp "Justify a planned line using the first rewrite rule that 
applies."))

;;; The line assertions justifications and restrictions

(defrulewffs simplify-plan
  (unique-lines 
     (simplify-plan-p2 "
A(O)")
     (simplify-plan-p1 "`(SIMPLIFY-UP  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule simplify-plan
  (matchfn simplify-plan-match)
  (match1fn simplify-plan-match1)
  (shortfn simplify-plan-short))


;;; The building function. 


(defun simplify-plan-build (p2 p1 a simplify-up p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (simplify-plan-mv0 simplify-plan-ml0))
      (unquoted maint::wffarg (a simplify-up)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (simplify-plan-p2 simplify-plan-p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (p2 p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((list "Rewrite"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list p1)))
        (nextplan)))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a simplify-up p2-hyps p1-hyps))))

;;; The entering function. 


(defun simplify-plan-enter (p2 p1 a simplify-up p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (simplify-plan-legal p2 p1 a simplify-up p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun simplify-plan-defaults (p2 p1 a simplify-up p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (simplify-plan-mv0 simplify-plan-ml0))
      (unquoted maint::wffarg (a simplify-up)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (simplify-plan-p2 simplify-plan-p1))
      (unquoted maint::linearg (p2 p1)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (simplify-plan-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (simplify-plan-mv0 simplify-plan-ml0))
      (unquoted maint::wffarg (a simplify-up)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (simplify-plan-hyp-defaults p2 p1 a simplify-up
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
                  (mapcar #'specified-p (list a simplify-up))
                  maint::strong-hypdefaults))
    (list p2 p1 a simplify-up p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun simplify-plan-hyp-defaults (p2 p1 a simplify-up p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a simplify-up))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun simplify-plan-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn
           (%catch%
            (match-bind (get 'simplify-plan-p2 'meta-assertion)
                        (get (car maint::plan-support) 'assertion))
            (fail (throwfail "Planned lines did not match.")))
           (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr nil))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when
                          (%catch%
                           (not
                            (apply (car maint::rstr)
                                   (mapcar #'meta-subst (cdr maint::rstr))))
                           (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun simplify-plan-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and
     (not
      (eq 'maint::failed
          (%catch%
           (match-bind (get 'simplify-plan-p2 'meta-assertion)
                       (get pline 'assertion))
           (fail 'maint::failed))))
     t)))

;;; The short version of the rule as a function.  


(defun simplify-plan-short (p2)
  (funcall #'comdecode
           (append (list 'simplify-plan)
                   (list (linealias p2))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun simplify-plan-legal (p2 p1 a simplify-up p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (simplify-plan-legal-hyps p2 p1 a simplify-up p2-hyps p1-hyps)
    (simplify-plan-legal-wffs p2 p1 a simplify-up p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun simplify-plan-legal-hyps (p2 p1 a simplify-up p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a simplify-up))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p2-hyps))
    (setq maint::hlower (join-h maint::hlower (set-difference p1-hyps (list))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun simplify-plan-legal-wffs (p2 p1 a simplify-up p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (simplify-plan-mv0 simplify-plan-ml0))
      (unquoted maint::wffarg (a simplify-up)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (simplify-plan-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (simplify-plan-p2 simplify-plan-p1))
      (unquoted maint::linearg (p2 p1)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))


;;;
;;; Rule: SIMPLIFY-SUPP
;;;

;;; The rule command definition.

(defmexpr simplify-supp
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a simplify-down d1-hyps d2-hyps)
  (arghelp "Line before rewriting (lower-numbered)" "Line after rewriting (higher-numbered)" "Wff before rewriting" "Wff after rewriting" "Hypotheses" "Hypotheses")
  (defaultfns simplify-supp-defaults)
  (mainfns simplify-supp-legal simplify-supp-build)
  (enterfns simplify-supp-enter)
  (mhelp "Rewrite a supporting line using the first rewrite 
rule that applies."))

;;; The line assertions justifications and restrictions

(defrulewffs simplify-supp
  (unique-lines 
     (simplify-supp-d1 "A(O)")
     (simplify-supp-d2 "`(SIMPLIFY-DOWN  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule simplify-supp
  (matchfn simplify-supp-match)
  (match1fn simplify-supp-match1)
  (shortfn simplify-supp-short))


;;; The building function. 


(defun simplify-supp-build (d1 d2 a simplify-down d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (simplify-supp-mv0 simplify-supp-ml0))
      (unquoted maint::wffarg (a simplify-down)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (simplify-supp-d1 simplify-supp-d2))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (d1 d2))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((nextplan)
        (list "Rewrite"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list d1)))))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a simplify-down d1-hyps d2-hyps))))

;;; The entering function. 


(defun simplify-supp-enter (d1 d2 a simplify-down d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (simplify-supp-legal d1 d2 a simplify-down d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun simplify-supp-defaults (d1 d2 a simplify-down d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (simplify-supp-mv0 simplify-supp-ml0))
      (unquoted maint::wffarg (a simplify-down)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (simplify-supp-d1 simplify-supp-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (simplify-supp-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (simplify-supp-mv0 simplify-supp-ml0))
      (unquoted maint::wffarg (a simplify-down)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (simplify-supp-hyp-defaults d1 d2 a simplify-down
                          d1-hyps d2-hyps))
          (setq maint::strong-hypdefaults
                (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss))
                     (line-no-defaults-to (eval-destruct (('pp d1 'ss)))
                                          (eval-destruct (('pp d2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil)
                  (mapcar #'specified-p (list a simplify-down))
                  maint::strong-hypdefaults))
    (list d1 d2 a simplify-down d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun simplify-supp-hyp-defaults (d1 d2 a simplify-down d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a simplify-down))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (d1 d2))
      (unquoted maint::hyparg (d1-hyps d2-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun simplify-supp-match (maint::plan-support)
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
              (%catch%
               (progn
                (match-bind (get 'simplify-supp-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do ((quoted maint::restr nil))
                          (let ((maint::rstr (get maint::restr 'restr-call)))
                            (when
                                (%catch%
                                 (not
                                  (apply (car maint::rstr)
                                         (mapcar #'meta-subst
                                                 (cdr maint::rstr))))
                                 (fail nil))
                              (throwfail nil))))
                (push (car maint::supps) maint::legal-supports))
               (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun simplify-supp-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'simplify-supp-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun simplify-supp-short (d1)
  (funcall #'comdecode
           (append (list 'simplify-supp)
                   (list (linealias d1))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun simplify-supp-legal (d1 d2 a simplify-down d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (simplify-supp-legal-hyps d1 d2 a simplify-down d1-hyps d2-hyps)
    (simplify-supp-legal-wffs d1 d2 a simplify-down d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun simplify-supp-legal-hyps (d1 d2 a simplify-down d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a simplify-down))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (d1 d2))
      (unquoted maint::hyparg (d1-hyps d2-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hlower (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hupper (meet-h maint::hupper d2-hyps))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun simplify-supp-legal-wffs (d1 d2 a simplify-down d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (simplify-supp-mv0 simplify-supp-ml0))
      (unquoted maint::wffarg (a simplify-down)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (simplify-supp-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (simplify-supp-d1 simplify-supp-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))


;;;
;;; Rule: SIMPLIFY-PLAN*
;;;

;;; The rule command definition.

(defmexpr simplify-plan*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a simplify-up* p2-hyps p1-hyps)
  (arghelp "Line after rewriting (higher-numbered)" "Line before rewriting (lower-numbered)" "Wff after rewriting" "Wff before rewriting" "Hypotheses" "Hypotheses")
  (defaultfns simplify-plan*-defaults)
  (mainfns simplify-plan*-legal simplify-plan*-build)
  (enterfns simplify-plan*-enter)
  (mhelp "Justify a planned line using the first rewrite rule that 
applies."))

;;; The line assertions justifications and restrictions

(defrulewffs simplify-plan*
  (unique-lines 
     (simplify-plan*-p2 "A(O)")
     (simplify-plan*-p1 "`(SIMPLIFY-UP*  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule simplify-plan*
  (matchfn simplify-plan*-match)
  (match1fn simplify-plan*-match1)
  (shortfn simplify-plan*-short))


;;; The building function. 


(defun simplify-plan*-build (p2 p1 a simplify-up* p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (simplify-plan*-mv0 simplify-plan*-ml0))
      (unquoted maint::wffarg (a simplify-up*)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (simplify-plan*-p2 simplify-plan*-p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (p2 p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((list "Rewrite"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list p1)))
        (nextplan)))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a simplify-up* p2-hyps p1-hyps))))

;;; The entering function. 


(defun simplify-plan*-enter (p2 p1 a simplify-up* p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (simplify-plan*-legal p2 p1 a simplify-up* p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun simplify-plan*-defaults (p2 p1 a simplify-up* p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (simplify-plan*-mv0 simplify-plan*-ml0))
      (unquoted maint::wffarg (a simplify-up*)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (simplify-plan*-p2 simplify-plan*-p1))
      (unquoted maint::linearg (p2 p1)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (simplify-plan*-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (simplify-plan*-mv0 simplify-plan*-ml0))
      (unquoted maint::wffarg (a simplify-up*)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (simplify-plan*-hyp-defaults p2 p1 a simplify-up*
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
                  (mapcar #'specified-p (list a simplify-up*))
                  maint::strong-hypdefaults))
    (list p2 p1 a simplify-up* p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun simplify-plan*-hyp-defaults (p2 p1 a simplify-up* p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a simplify-up*))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun simplify-plan*-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn
           (%catch%
            (match-bind (get 'simplify-plan*-p2 'meta-assertion)
                        (get (car maint::plan-support) 'assertion))
            (fail (throwfail "Planned lines did not match.")))
           (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr nil))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when
                          (%catch%
                           (not
                            (apply (car maint::rstr)
                                   (mapcar #'meta-subst (cdr maint::rstr))))
                           (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun simplify-plan*-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and
     (not
      (eq 'maint::failed
          (%catch%
           (match-bind (get 'simplify-plan*-p2 'meta-assertion)
                       (get pline 'assertion))
           (fail 'maint::failed))))
     t)))

;;; The short version of the rule as a function.  


(defun simplify-plan*-short (p2)
  (funcall #'comdecode
           (append (list 'simplify-plan*)
                   (list (linealias p2))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun simplify-plan*-legal (p2 p1 a simplify-up* p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (simplify-plan*-legal-hyps p2 p1 a simplify-up* p2-hyps p1-hyps)
    (simplify-plan*-legal-wffs p2 p1 a simplify-up* p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun simplify-plan*-legal-hyps (p2 p1 a simplify-up* p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a simplify-up*))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p2-hyps))
    (setq maint::hlower (join-h maint::hlower (set-difference p1-hyps (list))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun simplify-plan*-legal-wffs (p2 p1 a simplify-up* p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (simplify-plan*-mv0 simplify-plan*-ml0))
      (unquoted maint::wffarg (a simplify-up*)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (simplify-plan*-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (simplify-plan*-p2 simplify-plan*-p1))
      (unquoted maint::linearg (p2 p1)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))


;;;
;;; Rule: SIMPLIFY-SUPP*
;;;

;;; The rule command definition.

(defmexpr simplify-supp*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a simplify-down* d1-hyps d2-hyps)
  (arghelp "Line before rewriting (lower-numbered)" "Line after rewriting (higher-numbered)" "Wff before rewriting" "Wff after rewriting" "Hypotheses" "Hypotheses")
  (defaultfns simplify-supp*-defaults)
  (mainfns simplify-supp*-legal simplify-supp*-build)
  (enterfns simplify-supp*-enter)
  (mhelp "Rewrite a supporting line using the first rewrite 
rule that applies."))

;;; The line assertions justifications and restrictions

(defrulewffs simplify-supp*
  (unique-lines 
     (simplify-supp*-d1 "A(O)")
     (simplify-supp*-d2 "`(SIMPLIFY-DOWN*  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule simplify-supp*
  (matchfn simplify-supp*-match)
  (match1fn simplify-supp*-match1)
  (shortfn simplify-supp*-short))


;;; The building function. 


(defun simplify-supp*-build (d1 d2 a simplify-down* d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (simplify-supp*-mv0 simplify-supp*-ml0))
      (unquoted maint::wffarg (a simplify-down*)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (simplify-supp*-d1 simplify-supp*-d2))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (d1 d2))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((nextplan)
        (list "Rewrite"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list d1)))))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a simplify-down* d1-hyps d2-hyps))))

;;; The entering function. 


(defun simplify-supp*-enter (d1 d2 a simplify-down* d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (simplify-supp*-legal d1 d2 a simplify-down* d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun simplify-supp*-defaults (d1 d2 a simplify-down* d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (simplify-supp*-mv0 simplify-supp*-ml0))
      (unquoted maint::wffarg (a simplify-down*)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (simplify-supp*-d1 simplify-supp*-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (simplify-supp*-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (simplify-supp*-mv0 simplify-supp*-ml0))
      (unquoted maint::wffarg (a simplify-down*)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (simplify-supp*-hyp-defaults d1 d2 a simplify-down*
                          d1-hyps d2-hyps))
          (setq maint::strong-hypdefaults
                (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss))
                     (line-no-defaults-to (eval-destruct (('pp d1 'ss)))
                                          (eval-destruct (('pp d2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil)
                  (mapcar #'specified-p (list a simplify-down*))
                  maint::strong-hypdefaults))
    (list d1 d2 a simplify-down* d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun simplify-supp*-hyp-defaults (d1 d2 a simplify-down* d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a simplify-down*))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (d1 d2))
      (unquoted maint::hyparg (d1-hyps d2-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun simplify-supp*-match (maint::plan-support)
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
              (%catch%
               (progn
                (match-bind (get 'simplify-supp*-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do ((quoted maint::restr nil))
                          (let ((maint::rstr (get maint::restr 'restr-call)))
                            (when
                                (%catch%
                                 (not
                                  (apply (car maint::rstr)
                                         (mapcar #'meta-subst
                                                 (cdr maint::rstr))))
                                 (fail nil))
                              (throwfail nil))))
                (push (car maint::supps) maint::legal-supports))
               (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun simplify-supp*-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'simplify-supp*-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun simplify-supp*-short (d1)
  (funcall #'comdecode
           (append (list 'simplify-supp*)
                   (list (linealias d1))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun simplify-supp*-legal (d1 d2 a simplify-down* d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (simplify-supp*-legal-hyps d1 d2 a simplify-down* d1-hyps d2-hyps)
    (simplify-supp*-legal-wffs d1 d2 a simplify-down* d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun simplify-supp*-legal-hyps (d1 d2 a simplify-down* d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a simplify-down*))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (d1 d2))
      (unquoted maint::hyparg (d1-hyps d2-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hlower (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hupper (meet-h maint::hupper d2-hyps))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun simplify-supp*-legal-wffs (d1 d2 a simplify-down* d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (simplify-supp*-mv0 simplify-supp*-ml0))
      (unquoted maint::wffarg (a simplify-down*)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (simplify-supp*-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (simplify-supp*-d1 simplify-supp*-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))


;;;
;;; Rule: UNREWRITE-PLAN*
;;;

;;; The rule command definition.

(defmexpr unrewrite-plan*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
  (arghelp "Line after rewriting (higher-numbered)" "Line before rewriting (lower-numbered)" "Wff after rewriting" "Wff before rewriting" "Hypotheses" "Hypotheses")
  (defaultfns unrewrite-plan*-defaults)
  (mainfns unrewrite-plan*-legal unrewrite-plan*-build)
  (enterfns unrewrite-plan*-enter)
  (mhelp "Justify a planned line using all rewrite rules possible."))

;;; The line assertions justifications and restrictions

(defrulewffs unrewrite-plan*
  (unique-lines 
     (unrewrite-plan*-p2 "A(O)")
     (unrewrite-plan*-p1 "`(UNAPPLY-RRULE-ANY*  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule unrewrite-plan*
  (matchfn unrewrite-plan*-match)
  (match1fn unrewrite-plan*-match1)
  (shortfn unrewrite-plan*-short))


;;; The building function. 


(defun unrewrite-plan*-build (p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (unrewrite-plan*-mv0 unrewrite-plan*-ml0))
      (unquoted maint::wffarg (a unapply-rrule-any*)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (unrewrite-plan*-p2 unrewrite-plan*-p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (p2 p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((list "Rewrites"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list p1)))
        (nextplan)))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list a unapply-rrule-any* p2-hyps p1-hyps))))

;;; The entering function. 


(defun unrewrite-plan*-enter (p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (unrewrite-plan*-legal p2 p1 a unapply-rrule-any* p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun unrewrite-plan*-defaults (p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (unrewrite-plan*-mv0 unrewrite-plan*-ml0))
      (unquoted maint::wffarg (a unapply-rrule-any*)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (unrewrite-plan*-p2 unrewrite-plan*-p1))
      (unquoted maint::linearg (p2 p1)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (unrewrite-plan*-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (unrewrite-plan*-mv0 unrewrite-plan*-ml0))
      (unquoted maint::wffarg (a unapply-rrule-any*)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (unrewrite-plan*-hyp-defaults p2 p1 a
                          unapply-rrule-any* p2-hyps p1-hyps))
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
                  (mapcar #'specified-p (list a unapply-rrule-any*))
                  maint::strong-hypdefaults))
    (list p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun unrewrite-plan*-hyp-defaults
       (p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a unapply-rrule-any*))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun unrewrite-plan*-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn
           (%catch%
            (match-bind (get 'unrewrite-plan*-p2 'meta-assertion)
                        (get (car maint::plan-support) 'assertion))
            (fail (throwfail "Planned lines did not match.")))
           (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr nil))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when
                          (%catch%
                           (not
                            (apply (car maint::rstr)
                                   (mapcar #'meta-subst (cdr maint::rstr))))
                           (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun unrewrite-plan*-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and
     (not
      (eq 'maint::failed
          (%catch%
           (match-bind (get 'unrewrite-plan*-p2 'meta-assertion)
                       (get pline 'assertion))
           (fail 'maint::failed))))
     t)))

;;; The short version of the rule as a function.  


(defun unrewrite-plan*-short (p2)
  (funcall #'comdecode
           (append (list 'unrewrite-plan*)
                   (list (linealias p2))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun unrewrite-plan*-legal (p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (unrewrite-plan*-legal-hyps p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
    (unrewrite-plan*-legal-wffs p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun unrewrite-plan*-legal-hyps (p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a unapply-rrule-any*))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p2-hyps))
    (setq maint::hlower (join-h maint::hlower (set-difference p1-hyps (list))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun unrewrite-plan*-legal-wffs (p2 p1 a unapply-rrule-any* p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (unrewrite-plan*-mv0 unrewrite-plan*-ml0))
      (unquoted maint::wffarg (a unapply-rrule-any*)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (unrewrite-plan*-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (unrewrite-plan*-p2 unrewrite-plan*-p1))
      (unquoted maint::linearg (p2 p1)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))


;;;
;;; Rule: REWRITE-SUPP*
;;;

;;; The rule command definition.

(defmexpr rewrite-supp*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
  (arghelp "Line before rewriting (lower-numbered)" "Line after rewriting (higher-numbered)" "Wff before rewriting" "Wff after rewriting" "Hypotheses" "Hypotheses")
  (defaultfns rewrite-supp*-defaults)
  (mainfns rewrite-supp*-legal rewrite-supp*-build)
  (enterfns rewrite-supp*-enter)
  (mhelp "Rewrite a supporting line using all rewrite rules 
possible."))

;;; The line assertions justifications and restrictions

(defrulewffs rewrite-supp*
  (unique-lines 
     (rewrite-supp*-d1 "A(O)")
     (rewrite-supp*-d2 "`(APPLY-RRULE-ANY*  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule rewrite-supp*
  (matchfn rewrite-supp*-match)
  (match1fn rewrite-supp*-match1)
  (shortfn rewrite-supp*-short))


;;; The building function. 


(defun rewrite-supp*-build (d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (rewrite-supp*-mv0 rewrite-supp*-ml0))
      (unquoted maint::wffarg (a apply-rrule-any*)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (rewrite-supp*-d1 rewrite-supp*-d2))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (d1 d2))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((nextplan)
        (list "Rewrites"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list d1)))))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a apply-rrule-any* d1-hyps d2-hyps))))

;;; The entering function. 


(defun rewrite-supp*-enter (d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (rewrite-supp*-legal d1 d2 a apply-rrule-any* d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun rewrite-supp*-defaults (d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (rewrite-supp*-mv0 rewrite-supp*-ml0))
      (unquoted maint::wffarg (a apply-rrule-any*)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (rewrite-supp*-d1 rewrite-supp*-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (rewrite-supp*-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (rewrite-supp*-mv0 rewrite-supp*-ml0))
      (unquoted maint::wffarg (a apply-rrule-any*)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (rewrite-supp*-hyp-defaults d1 d2 a apply-rrule-any*
                          d1-hyps d2-hyps))
          (setq maint::strong-hypdefaults
                (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss))
                     (line-no-defaults-to (eval-destruct (('pp d1 'ss)))
                                          (eval-destruct (('pp d2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil)
                  (mapcar #'specified-p (list a apply-rrule-any*))
                  maint::strong-hypdefaults))
    (list d1 d2 a apply-rrule-any* d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun rewrite-supp*-hyp-defaults (d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a apply-rrule-any*))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (d1 d2))
      (unquoted maint::hyparg (d1-hyps d2-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun rewrite-supp*-match (maint::plan-support)
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
              (%catch%
               (progn
                (match-bind (get 'rewrite-supp*-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do ((quoted maint::restr nil))
                          (let ((maint::rstr (get maint::restr 'restr-call)))
                            (when
                                (%catch%
                                 (not
                                  (apply (car maint::rstr)
                                         (mapcar #'meta-subst
                                                 (cdr maint::rstr))))
                                 (fail nil))
                              (throwfail nil))))
                (push (car maint::supps) maint::legal-supports))
               (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun rewrite-supp*-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'rewrite-supp*-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun rewrite-supp*-short (d1)
  (funcall #'comdecode
           (append (list 'rewrite-supp*)
                   (list (linealias d1))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun rewrite-supp*-legal (d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (rewrite-supp*-legal-hyps d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
    (rewrite-supp*-legal-wffs d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun rewrite-supp*-legal-hyps (d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a apply-rrule-any*))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (d1 d2))
      (unquoted maint::hyparg (d1-hyps d2-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hlower (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hupper (meet-h maint::hupper d2-hyps))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun rewrite-supp*-legal-wffs (d1 d2 a apply-rrule-any* d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (rewrite-supp*-mv0 rewrite-supp*-ml0))
      (unquoted maint::wffarg (a apply-rrule-any*)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (rewrite-supp*-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (rewrite-supp*-d1 rewrite-supp*-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))


;;;
;;; Rule: UNREWRITE-PLAN1
;;;

;;; The rule command definition.

(defmexpr unrewrite-plan1
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
  (arghelp "Line after rewriting (higher-numbered)" "Line before rewriting (lower-numbered)" "Wff after rewriting" "Wff before rewriting" "Hypotheses" "Hypotheses")
  (defaultfns unrewrite-plan1-defaults)
  (mainfns unrewrite-plan1-legal unrewrite-plan1-build)
  (enterfns unrewrite-plan1-enter)
  (mhelp "Justify a planned line using the first rewrite rule that 
applies."))

;;; The line assertions justifications and restrictions

(defrulewffs unrewrite-plan1
  (unique-lines 
     (unrewrite-plan1-p2 "A(O)")
     (unrewrite-plan1-p1 "`(UNAPPLY-RRULE-ANY  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule unrewrite-plan1
  (matchfn unrewrite-plan1-match)
  (match1fn unrewrite-plan1-match1)
  (shortfn unrewrite-plan1-short))


;;; The building function. 


(defun unrewrite-plan1-build (p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (unrewrite-plan1-mv0 unrewrite-plan1-ml0))
      (unquoted maint::wffarg (a unapply-rrule-any)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (unrewrite-plan1-p2 unrewrite-plan1-p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (p2 p1))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((list "Rewrite"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list p1)))
        (nextplan)))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a unapply-rrule-any p2-hyps p1-hyps))))

;;; The entering function. 


(defun unrewrite-plan1-enter (p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (unrewrite-plan1-legal p2 p1 a unapply-rrule-any p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun unrewrite-plan1-defaults (p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (unrewrite-plan1-mv0 unrewrite-plan1-ml0))
      (unquoted maint::wffarg (a unapply-rrule-any)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (unrewrite-plan1-p2 unrewrite-plan1-p1))
      (unquoted maint::linearg (p2 p1)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (unrewrite-plan1-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (unrewrite-plan1-mv0 unrewrite-plan1-ml0))
      (unquoted maint::wffarg (a unapply-rrule-any)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (unrewrite-plan1-hyp-defaults p2 p1 a
                          unapply-rrule-any p2-hyps p1-hyps))
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
                  (mapcar #'specified-p (list a unapply-rrule-any))
                  maint::strong-hypdefaults))
    (list p2 p1 a unapply-rrule-any p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun unrewrite-plan1-hyp-defaults (p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a unapply-rrule-any))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun unrewrite-plan1-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn
           (%catch%
            (match-bind (get 'unrewrite-plan1-p2 'meta-assertion)
                        (get (car maint::plan-support) 'assertion))
            (fail (throwfail "Planned lines did not match.")))
           (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr nil))
                    (let ((maint::rstr (get maint::restr 'restr-call)))
                      (when
                          (%catch%
                           (not
                            (apply (car maint::rstr)
                                   (mapcar #'meta-subst (cdr maint::rstr))))
                           (fail nil))
                        (throwfail "Some restriction not satisfied.")))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun unrewrite-plan1-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and
     (not
      (eq 'maint::failed
          (%catch%
           (match-bind (get 'unrewrite-plan1-p2 'meta-assertion)
                       (get pline 'assertion))
           (fail 'maint::failed))))
     t)))

;;; The short version of the rule as a function.  


(defun unrewrite-plan1-short (p2)
  (funcall #'comdecode
           (append (list 'unrewrite-plan1)
                   (list (linealias p2))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun unrewrite-plan1-legal (p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (unrewrite-plan1-legal-hyps p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
    (unrewrite-plan1-legal-wffs p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun unrewrite-plan1-legal-hyps (p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a unapply-rrule-any))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (p2 p1))
      (unquoted maint::hyparg (p2-hyps p1-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p2-hyps))
    (setq maint::hlower (join-h maint::hlower (set-difference p1-hyps (list))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun unrewrite-plan1-legal-wffs (p2 p1 a unapply-rrule-any p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (unrewrite-plan1-mv0 unrewrite-plan1-ml0))
      (unquoted maint::wffarg (a unapply-rrule-any)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (unrewrite-plan1-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (unrewrite-plan1-p2 unrewrite-plan1-p1))
      (unquoted maint::linearg (p2 p1)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))


;;;
;;; Rule: REWRITE-SUPP1
;;;

;;; The rule command definition.

(defmexpr rewrite-supp1
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a apply-rrule-any d1-hyps d2-hyps)
  (arghelp "Line before rewriting (lower-numbered)" "Line after rewriting (higher-numbered)" "Wff before rewriting" "Wff after rewriting" "Hypotheses" "Hypotheses")
  (defaultfns rewrite-supp1-defaults)
  (mainfns rewrite-supp1-legal rewrite-supp1-build)
  (enterfns rewrite-supp1-enter)
  (mhelp "Rewrite a supporting line using the first rewrite 
rule that applies."))

;;; The line assertions justifications and restrictions

(defrulewffs rewrite-supp1
  (unique-lines 
     (rewrite-supp1-d1 "A(O)")
     (rewrite-supp1-d2 "`(APPLY-RRULE-ANY  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule rewrite-supp1
  (matchfn rewrite-supp1-match)
  (match1fn rewrite-supp1-match1)
  (shortfn rewrite-supp1-short))


;;; The building function. 


(defun rewrite-supp1-build (d1 d2 a apply-rrule-any d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (rewrite-supp1-mv0 rewrite-supp1-ml0))
      (unquoted maint::wffarg (a apply-rrule-any)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (rewrite-supp1-d1 rewrite-supp1-d2))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label
              (meta-subst (get maint::u-line 'meta-assertion))
              'assertion))
    (macro-do
     ((unquoted maint::line-arg (d1 d2))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::line-arg 'linenumber)
     (push (cons maint::line-arg maint::line-label) maint::num-alist))
    (macro-do
     ((unquoted maint::just
       ((nextplan)
        (list "Rewrite"
              (mapcar #'meta-subst 'nil)
              (subst-labels maint::num-alist (list d1)))))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do
     ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
      (local maint::line-label maint::new-line-labels))
     (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a apply-rrule-any d1-hyps d2-hyps))))

;;; The entering function. 


(defun rewrite-supp1-enter (d1 d2 a apply-rrule-any d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (rewrite-supp1-legal d1 d2 a apply-rrule-any d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun rewrite-supp1-defaults (d1 d2 a apply-rrule-any d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (rewrite-supp1-mv0 rewrite-supp1-ml0))
      (unquoted maint::wffarg (a apply-rrule-any)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (rewrite-supp1-d1 rewrite-supp1-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (rewrite-supp1-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (rewrite-supp1-mv0 rewrite-supp1-ml0))
      (unquoted maint::wffarg (a apply-rrule-any)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (rewrite-supp1-hyp-defaults d1 d2 a apply-rrule-any
                          d1-hyps d2-hyps))
          (setq maint::strong-hypdefaults
                (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss))
                     (line-no-defaults-to (eval-destruct (('pp d1 'ss)))
                                          (eval-destruct (('pp d2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil)
                  (mapcar #'specified-p (list a apply-rrule-any))
                  maint::strong-hypdefaults))
    (list d1 d2 a apply-rrule-any d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun rewrite-supp1-hyp-defaults (d1 d2 a apply-rrule-any d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a apply-rrule-any))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (d1 d2))
      (unquoted maint::hyparg (d1-hyps d2-hyps)))
     (when (existent-p maint::linearg)
       (if (specified-p maint::hyparg)
           (when (not (set-eq maint::hyparg (hypnums maint::linearg)))
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
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun rewrite-supp1-match (maint::plan-support)
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
              (%catch%
               (progn
                (match-bind (get 'rewrite-supp1-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do ((quoted maint::restr nil))
                          (let ((maint::rstr (get maint::restr 'restr-call)))
                            (when
                                (%catch%
                                 (not
                                  (apply (car maint::rstr)
                                         (mapcar #'meta-subst
                                                 (cdr maint::rstr))))
                                 (fail nil))
                              (throwfail nil))))
                (push (car maint::supps) maint::legal-supports))
               (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun rewrite-supp1-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'rewrite-supp1-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun rewrite-supp1-short (d1)
  (funcall #'comdecode
           (append (list 'rewrite-supp1)
                   (list (linealias d1))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun rewrite-supp1-legal (d1 d2 a apply-rrule-any d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (rewrite-supp1-legal-hyps d1 d2 a apply-rrule-any d1-hyps d2-hyps)
    (rewrite-supp1-legal-wffs d1 d2 a apply-rrule-any d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun rewrite-supp1-legal-hyps (d1 d2 a apply-rrule-any d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a apply-rrule-any))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do
     ((unquoted maint::linearg (d1 d2))
      (unquoted maint::hyparg (d1-hyps d2-hyps)))
     (when
         (and (existent-p maint::linearg)
              (not (set-eq maint::hyparg (hypnums maint::linearg))))
       (throwfail "Hypothesis specified for line "
                  (maint::linearg . line)
                  " are not the same as the ones in the proof.")))
    (setq maint::hlower (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hupper (meet-h maint::hupper d2-hyps))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        t
        (progn
         (cond ((eq maint::hlower '$) (setq maint::hlower maint::hupper))
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


(defun rewrite-supp1-legal-wffs (d1 d2 a apply-rrule-any d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (rewrite-supp1-mv0 rewrite-supp1-ml0))
      (unquoted maint::wffarg (a apply-rrule-any)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (rewrite-supp1-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (rewrite-supp1-d1 rewrite-supp1-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr nil))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when
                    (not
                     (apply (car maint::rstr)
                            (mapcar #'meta-subst (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  "
                             t
                             "Restrictions "
                             (car maint::rstr)
                             " not satisfied.  "))))))
