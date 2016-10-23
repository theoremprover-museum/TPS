;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/afs/cs.cmu.edu/project/tps/tps/lisp/ml1-logic4.lisp"
;;;  assembled from "/afs/cs.cmu.edu/project/tps/tps/lisp/ml1-logic4.rules"
;;;
;;; contains rules
;;; SUBST-WFF SUBST-TERM IDEF EDEF AB* ABE ABU IMP-DISJ DISJ-IMP 
;;;

(in-package :ML)
(part-of MATH-LOGIC-1-RULES)

(defrulefile ml1-logic4
  (contents SUBST-WFF SUBST-TERM IDEF EDEF AB* ABE ABU IMP-DISJ DISJ-IMP ))

(context rule-commands)

(context RULES-5-SUBST)


;;;
;;; Rule: SUBST-WFF
;;;

;;; The rule command definition.

(defmexpr subst-wff
  (argtypes line line gwff gwff gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 c b a s d1-hyps d2-hyps)
  (arghelp "Line Before Substitution" "Line After Substitution" "Propositional Variable to Substitute for" "Wff to Substitute" "Wff to Substitute into" "Wff after Substitution" "Hypotheses" "Hypotheses")
  (defaultfns subst-wff-defaults)
  (mainfns subst-wff-legal subst-wff-build)
  (enterfns subst-wff-enter)
  (mhelp "Rule to substitute a wff for a propositional variable."))

;;; The line assertions justifications and restrictions

(defrulewffs subst-wff
  (unique-lines 
     (subst-wff-d1 "A(O)")
     (subst-wff-d2 "`(S  B(O)  C(O)  A(O))"))
  (unique-restrictions 
     (subst-wff-restr0 (not-free-in-hyps "C(O)"))
     (subst-wff-restr1 (free-for "B(O)" "C(O)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule subst-wff
  (matchfn subst-wff-match)
  (match1fn subst-wff-match1)
  (shortfn subst-wff-short))


;;; The building function. 


(defun subst-wff-build (d1 d2 c b a s d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar
       (subst-wff-mv2 subst-wff-mv1 subst-wff-mv0 subst-wff-ml0))
      (unquoted maint::wffarg (c b a s)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (subst-wff-d1 subst-wff-d2))
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
        (list "Subst"
              (mapcar #'meta-subst '(subst-wff-mv1 subst-wff-mv2))
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
    (append maint::new-line-labels (list c b a s d1-hyps d2-hyps))))

;;; The entering function. 


(defun subst-wff-enter (d1 d2 c b a s d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (subst-wff-legal d1 d2 c b a s d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun subst-wff-defaults (d1 d2 c b a s d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar
       (subst-wff-mv2 subst-wff-mv1 subst-wff-mv0 subst-wff-ml0))
      (unquoted maint::wffarg (c b a s)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (subst-wff-d1 subst-wff-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (subst-wff-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar
       (subst-wff-mv2 subst-wff-mv1 subst-wff-mv0 subst-wff-ml0))
      (unquoted maint::wffarg (c b a s)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (subst-wff-hyp-defaults d1 d2 c b a s d1-hyps
                          d2-hyps))
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
                  (mapcar #'specified-p (list c b a s))
                  maint::strong-hypdefaults))
    (list d1 d2 c b a s d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun subst-wff-hyp-defaults (d1 d2 c b a s d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore c b a s))
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


(defun subst-wff-match (maint::plan-support)
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
                (match-bind (get 'subst-wff-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do
                 ((quoted maint::restr (subst-wff-restr0 subst-wff-restr1)))
                 (let ((maint::rstr (get maint::restr 'restr-call)))
                   (when
                       (%catch%
                        (not
                         (apply (car maint::rstr)
                                (mapcar #'meta-subst (cdr maint::rstr))))
                        (fail nil))
                     (throwfail nil))))
                (push (car maint::supps) maint::legal-supports))
               (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun subst-wff-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'subst-wff-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed))))
         (let ((maint::rstr (get 'subst-wff-restr0 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t)))
         (let ((maint::rstr (get 'subst-wff-restr1 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t))))))

;;; The short version of the rule as a function.  


(defun subst-wff-short (d1 c b)
  (funcall #'comdecode
           (append (list 'subst-wff)
                   (list (linealias d1))
                   (list '$)
                   (append (list (append (list 'quote) (list c) 'nil)))
                   (append (list (append (list 'quote) (list b) 'nil)))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun subst-wff-legal (d1 d2 c b a s d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (subst-wff-legal-hyps d1 d2 c b a s d1-hyps d2-hyps)
    (subst-wff-legal-wffs d1 d2 c b a s d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun subst-wff-legal-hyps (d1 d2 c b a s d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore c b a s))
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


(defun subst-wff-legal-wffs (d1 d2 c b a s d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar
       (subst-wff-mv2 subst-wff-mv1 subst-wff-mv0 subst-wff-ml0))
      (unquoted maint::wffarg (c b a s)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (subst-wff-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (subst-wff-d1 subst-wff-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (subst-wff-restr0 subst-wff-restr1)))
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
;;; Rule: SUBST-TERM
;;;

;;; The rule command definition.

(defmexpr subst-term
  (argtypes line line gwff gwff gwff gwff linelist linelist)
  (wffargtypes  nil nil "I" "I" "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 |x| |t| a s d1-hyps d2-hyps)
  (arghelp "Line Before Substitution" "Line After Substitution" "Variable to Substitute for" "Term to Substitute" "Wff to Substitute into" "Wff after Substitution" "Hypotheses" "Hypotheses")
  (defaultfns subst-term-defaults)
  (mainfns subst-term-legal subst-term-build)
  (enterfns subst-term-enter)
  (mhelp "Rule to substitute a term for an individual variable."))

;;; The line assertions justifications and restrictions

(defrulewffs subst-term
  (unique-lines 
     (subst-term-d1 "A(O)")
     (subst-term-d2 "`(S  t(I)  x(I)  A(O))"))
  (unique-restrictions 
     (subst-term-restr0 (not-free-in-hyps "x(I)"))
     (subst-term-restr1 (is-variable "x(I)"))
     (subst-term-restr2 (free-for "t(I)" "x(I)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule subst-term
  (matchfn subst-term-match)
  (match1fn subst-term-match1)
  (shortfn subst-term-short))


;;; The building function. 


(defun subst-term-build (d1 d2 |x| |t| a s d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar
       (subst-term-mv2 subst-term-mv1 subst-term-mv0 subst-term-ml0))
      (unquoted maint::wffarg (|x| |t| a s)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (subst-term-d1 subst-term-d2))
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
        (list "Subst"
              (mapcar #'meta-subst '(subst-term-mv1 subst-term-mv2))
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
    (append maint::new-line-labels (list |x| |t| a s d1-hyps d2-hyps))))

;;; The entering function. 


(defun subst-term-enter (d1 d2 |x| |t| a s d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (subst-term-legal d1 d2 |x| |t| a s d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss d1)))))

;;; The default function. 


(defun subst-term-defaults (d1 d2 |x| |t| a s d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar
       (subst-term-mv2 subst-term-mv1 subst-term-mv0 subst-term-ml0))
      (unquoted maint::wffarg (|x| |t| a s)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (subst-term-d1 subst-term-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (subst-term-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar
       (subst-term-mv2 subst-term-mv1 subst-term-mv0 subst-term-ml0))
      (unquoted maint::wffarg (|x| |t| a s)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (subst-term-hyp-defaults d1 d2 |x| |t| a s d1-hyps
                          d2-hyps))
          (setq maint::strong-hypdefaults
                (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss d1))
                     (line-no-defaults-to (eval-destruct (('pp d1 'ss)))
                                          (eval-destruct (('pp d2 'ss d1))))))
    (setq strong-defaultlist
          (append '(nil nil)
                  (mapcar #'specified-p (list |x| |t| a s))
                  maint::strong-hypdefaults))
    (list d1 d2 |x| |t| a s d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun subst-term-hyp-defaults (d1 d2 |x| |t| a s d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore |x| |t| a s))
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


(defun subst-term-match (maint::plan-support)
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
                (match-bind (get 'subst-term-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do
                 ((quoted maint::restr
                   (subst-term-restr0 subst-term-restr1 subst-term-restr2)))
                 (let ((maint::rstr (get maint::restr 'restr-call)))
                   (when
                       (%catch%
                        (not
                         (apply (car maint::rstr)
                                (mapcar #'meta-subst (cdr maint::rstr))))
                        (fail nil))
                     (throwfail nil))))
                (push (car maint::supps) maint::legal-supports))
               (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun subst-term-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'subst-term-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed))))
         (let ((maint::rstr (get 'subst-term-restr0 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t)))
         (let ((maint::rstr (get 'subst-term-restr1 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t)))
         (let ((maint::rstr (get 'subst-term-restr2 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t))))))

;;; The short version of the rule as a function.  


(defun subst-term-short (d1 |x| |t|)
  (funcall #'comdecode
           (append (list 'subst-term)
                   (list (linealias d1))
                   (list '$)
                   (append (list (append (list 'quote) (list |x|) 'nil)))
                   (append (list (append (list 'quote) (list |t|) 'nil)))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun subst-term-legal (d1 d2 |x| |t| a s d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (subst-term-legal-hyps d1 d2 |x| |t| a s d1-hyps d2-hyps)
    (subst-term-legal-wffs d1 d2 |x| |t| a s d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun subst-term-legal-hyps (d1 d2 |x| |t| a s d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore |x| |t| a s))
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


(defun subst-term-legal-wffs (d1 d2 |x| |t| a s d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar
       (subst-term-mv2 subst-term-mv1 subst-term-mv0 subst-term-ml0))
      (unquoted maint::wffarg (|x| |t| a s)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (subst-term-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (subst-term-d1 subst-term-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do
     ((quoted maint::restr
       (subst-term-restr0 subst-term-restr1 subst-term-restr2)))
     (let ((maint::rstr (get maint::restr 'restr-call)))
       (when
           (not
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr))))
         (throwfail "Rule not applicable.  "
                    t
                    "Restrictions "
                    (car maint::rstr)
                    " not satisfied.  "))))))

(context RULES-7-DEFN)


;;;
;;; Rule: IDEF
;;;

;;; The rule command definition.

(defmexpr idef
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a inst-def p2-hyps p1-hyps)
  (arghelp "Line with Top Level Definition" "Line with Instantiated Definition" "Wff with Definition" "Wff After Instantiating Definition" "Hypotheses" "Hypotheses")
  (defaultfns idef-defaults)
  (mainfns idef-legal idef-build)
  (enterfns idef-enter)
  (mhelp "Rule to introduce a top level definition."))

;;; The line assertions justifications and restrictions

(defrulewffs idef
  (unique-lines 
     (idef-p2 "A(O)")
     (idef-p1 "`(INST-DEF  A(O))"))
  (unique-restrictions 
     (idef-restr0 (top-level-defn "
A(O)"))))

;;; The suggesting rule definition.

(defsrule idef
  (matchfn idef-match)
  (match1fn idef-match1)
  (shortfn idef-short))


;;; The building function. 


(defun idef-build (p2 p1 a inst-def p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (idef-mv0 idef-ml0))
      (unquoted maint::wffarg (a inst-def)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (idef-p2 idef-p1))
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
       ((list "Defn"
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
    (append maint::new-line-labels (list a inst-def p2-hyps p1-hyps))))

;;; The entering function. 


(defun idef-enter (p2 p1 a inst-def p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (idef-legal p2 p1 a inst-def p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun idef-defaults (p2 p1 a inst-def p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (idef-mv0 idef-ml0))
      (unquoted maint::wffarg (a inst-def)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (idef-p2 idef-p1))
      (unquoted maint::linearg (p2 p1)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (idef-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (idef-mv0 idef-ml0))
      (unquoted maint::wffarg (a inst-def)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (idef-hyp-defaults p2 p1 a inst-def p2-hyps p1-hyps))
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
                  (mapcar #'specified-p (list a inst-def))
                  maint::strong-hypdefaults))
    (list p2 p1 a inst-def p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun idef-hyp-defaults (p2 p1 a inst-def p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a inst-def))
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


(defun idef-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn
           (%catch%
            (match-bind (get 'idef-p2 'meta-assertion)
                        (get (car maint::plan-support) 'assertion))
            (fail (throwfail "Planned lines did not match.")))
           (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr (idef-restr0)))
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


(defun idef-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and
     (not
      (eq 'maint::failed
          (%catch%
           (match-bind (get 'idef-p2 'meta-assertion) (get pline 'assertion))
           (fail 'maint::failed))))
     t
     (let ((maint::rstr (get 'idef-restr0 'restr-call)))
       (%catch%
        (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
        (fail t))))))

;;; The short version of the rule as a function.  


(defun idef-short (p2)
  (funcall #'comdecode
           (append (list 'idef)
                   (list (linealias p2))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun idef-legal (p2 p1 a inst-def p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (idef-legal-hyps p2 p1 a inst-def p2-hyps p1-hyps)
    (idef-legal-wffs p2 p1 a inst-def p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun idef-legal-hyps (p2 p1 a inst-def p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a inst-def))
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


(defun idef-legal-wffs (p2 p1 a inst-def p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (idef-mv0 idef-ml0))
      (unquoted maint::wffarg (a inst-def)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (idef-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (idef-p2 idef-p1))
      (unquoted maint::linearg (p2 p1)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (idef-restr0)))
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
;;; Rule: EDEF
;;;

;;; The rule command definition.

(defmexpr edef
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a inst-def d1-hyps d2-hyps)
  (arghelp "Line with Top Level Definition" "Line with Instantiated Definition" "Wff with Definition" "Wff After Instantiating Definition" "Hypotheses" "Hypotheses")
  (defaultfns edef-defaults)
  (mainfns edef-legal edef-build)
  (enterfns edef-enter)
  (mhelp "Rule to eliminate top level definition."))

;;; The line assertions justifications and restrictions

(defrulewffs edef
  (unique-lines 
     (edef-d1 "A(O)")
     (edef-d2 "`(INST-DEF  A(O))"))
  (unique-restrictions 
     (edef-restr0 (top-level-defn "A(O)"))))

;;; The suggesting rule definition.

(defsrule edef
  (matchfn edef-match)
  (match1fn edef-match1)
  (shortfn edef-short))


;;; The building function. 


(defun edef-build (d1 d2 a inst-def d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (edef-mv0 edef-ml0))
      (unquoted maint::wffarg (a inst-def)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (edef-d1 edef-d2))
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
        (list "Defn"
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
    (append maint::new-line-labels (list a inst-def d1-hyps d2-hyps))))

;;; The entering function. 


(defun edef-enter (d1 d2 a inst-def d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (edef-legal d1 d2 a inst-def d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun edef-defaults (d1 d2 a inst-def d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (edef-mv0 edef-ml0))
      (unquoted maint::wffarg (a inst-def)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (edef-d1 edef-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (edef-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (edef-mv0 edef-ml0))
      (unquoted maint::wffarg (a inst-def)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (edef-hyp-defaults d1 d2 a inst-def d1-hyps d2-hyps))
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
                  (mapcar #'specified-p (list a inst-def))
                  maint::strong-hypdefaults))
    (list d1 d2 a inst-def d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun edef-hyp-defaults (d1 d2 a inst-def d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a inst-def))
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


(defun edef-match (maint::plan-support)
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
                (match-bind (get 'edef-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do ((quoted maint::restr (edef-restr0)))
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


(defun edef-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'edef-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed))))
         (let ((maint::rstr (get 'edef-restr0 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t))))))

;;; The short version of the rule as a function.  


(defun edef-short (d1)
  (funcall #'comdecode
           (append (list 'edef)
                   (list (linealias d1))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun edef-legal (d1 d2 a inst-def d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (edef-legal-hyps d1 d2 a inst-def d1-hyps d2-hyps)
    (edef-legal-wffs d1 d2 a inst-def d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun edef-legal-hyps (d1 d2 a inst-def d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a inst-def))
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


(defun edef-legal-wffs (d1 d2 a inst-def d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (edef-mv0 edef-ml0))
      (unquoted maint::wffarg (a inst-def)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (edef-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (edef-d1 edef-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (edef-restr0)))
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

(context RULES-4-QUANT)


;;;
;;; Rule: AB*
;;;

;;; The rule command definition.

(defmexpr ab*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Lower Line" "Higher Line" "Assertion of Higher Line" "Assertion of Lower Line" "Hypotheses" "Hypotheses")
  (defaultfns ab*-defaults)
  (mainfns ab*-legal ab*-build)
  (enterfns ab*-enter)
  (mhelp "Rule to alphabetically change embedded quantified variables."))

;;; The line assertions justifications and restrictions

(defrulewffs ab*
  (unique-lines 
     (ab*-d1 "A(O)")
     (ab*-d2 "B(O)"))
  (unique-restrictions 
     (ab*-restr0 (wffeq-ab "A(O)" "B(O)"))))

;;; The suggesting rule definition.

(defsrule ab*
  (matchfn ab*-match)
  (match1fn ab*-match1)
  (shortfn ab*-short))


;;; The building function. 


(defun ab*-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (ab*-mv1 ab*-mv0)) (unquoted maint::wffarg (b a)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (ab*-d1 ab*-d2))
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
        (list "AB"
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
    (append maint::new-line-labels (list b a d1-hyps d2-hyps))))

;;; The entering function. 


(defun ab*-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (ab*-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun ab*-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (ab*-mv1 ab*-mv0)) (unquoted maint::wffarg (b a)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (ab*-d1 ab*-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (ab*-mv1 ab*-mv0)) (unquoted maint::wffarg (b a)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (ab*-hyp-defaults d1 d2 b a d1-hyps d2-hyps))
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
                  (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list d1 d2 b a d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun ab*-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
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


(defun ab*-match (maint::plan-support)
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
                (match-bind (get 'ab*-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do ((quoted maint::restr (ab*-restr0)))
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


(defun ab*-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'ab*-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed))))
         (let ((maint::rstr (get 'ab*-restr0 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t))))))

;;; The short version of the rule as a function.  


(defun ab*-short (d1 b)
  (funcall #'comdecode
           (append (list 'ab*)
                   (list (linealias d1))
                   (list '$)
                   (append (list (append (list 'quote) (list b) 'nil)))
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun ab*-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (ab*-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (ab*-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun ab*-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun ab*-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (ab*-mv1 ab*-mv0)) (unquoted maint::wffarg (b a)))
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
     ((quoted maint::unique-line (ab*-d1 ab*-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::restr (ab*-restr0)))
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
;;; Rule: ABE
;;;

;;; The rule command definition.

(defmexpr abe
  (argtypes line line gwff gwff gwff gwff linelist linelist)
  (wffargtypes  nil nil "I" "O" "I" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 |y| a |x| s d1-hyps d2-hyps)
  (arghelp "Lower Existentially Quantified Line" "Higher Existentially Quantified Line" "Existentially Quantified Variable in Higher Line" "Scope of Existential Quantifier in Lower Line" "Existentially Quantified Variable in Lower Line" "Scope of Existential Quantifier in Higher Line" "Hypotheses" "Hypotheses")
  (defaultfns abe-defaults)
  (mainfns abe-legal abe-build)
  (enterfns abe-enter)
  (mhelp "Rule to change a top level occurrence of an existentially quantified
variable."))

;;; The line assertions justifications and restrictions

(defrulewffs abe
  (unique-lines 
     (abe-d1 "EXISTS x(I) A(O)")
     (abe-d2 "EXISTS y(I) `(S  y  x(I)  A(O))"))
  (unique-restrictions 
     (abe-restr0 (free-for "y(I)" "x(I)" "A(O)"))
     (abe-restr1 (is-variable "y(I)"))
     (abe-restr2 (is-variable "x(I)"))
     (abe-restr3 (not-free-in "y(I)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule abe
  (matchfn abe-match)
  (match1fn abe-match1)
  (shortfn abe-short))


;;; The building function. 


(defun abe-build (d1 d2 |y| a |x| s d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (abe-mv2 abe-mv1 abe-mv0 abe-ml0))
      (unquoted maint::wffarg (|y| a |x| s)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (abe-d1 abe-d2))
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
        (list "AB"
              (mapcar #'meta-subst '(abe-mv2))
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
    (append maint::new-line-labels (list |y| a |x| s d1-hyps d2-hyps))))

;;; The entering function. 


(defun abe-enter (d1 d2 |y| a |x| s d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (abe-legal d1 d2 |y| a |x| s d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun abe-defaults (d1 d2 |y| a |x| s d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (abe-mv2 abe-mv1 abe-mv0 abe-ml0))
      (unquoted maint::wffarg (|y| a |x| s)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (abe-d1 abe-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (abe-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (abe-mv2 abe-mv1 abe-mv0 abe-ml0))
      (unquoted maint::wffarg (|y| a |x| s)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (abe-hyp-defaults d1 d2 |y| a |x| s d1-hyps d2-hyps))
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
                  (mapcar #'specified-p (list |y| a |x| s))
                  maint::strong-hypdefaults))
    (list d1 d2 |y| a |x| s d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun abe-hyp-defaults (d1 d2 |y| a |x| s d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore |y| a |x| s))
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


(defun abe-match (maint::plan-support)
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
                (match-bind (get 'abe-d1 'meta-assertion)
                            (get (car maint::supps) 'assertion))
                (macro-do
                 ((quoted maint::restr
                   (abe-restr0 abe-restr1 abe-restr2 abe-restr3)))
                 (let ((maint::rstr (get maint::restr 'restr-call)))
                   (when
                       (%catch%
                        (not
                         (apply (car maint::rstr)
                                (mapcar #'meta-subst (cdr maint::rstr))))
                        (fail nil))
                     (throwfail nil))))
                (push (car maint::supps) maint::legal-supports))
               (fail nil)))))
    (list maint::matched-plan maint::matched-support)))

;;; The new matching function.  


(defun abe-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'abe-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed))))
         (let ((maint::rstr (get 'abe-restr0 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t)))
         (let ((maint::rstr (get 'abe-restr1 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t)))
         (let ((maint::rstr (get 'abe-restr2 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t)))
         (let ((maint::rstr (get 'abe-restr3 'restr-call)))
           (%catch%
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
            (fail t))))))

;;; The short version of the rule as a function.  


(defun abe-short (d1 |y|)
  (funcall #'comdecode
           (append (list 'abe)
                   (list (linealias d1))
                   (list '$)
                   (append (list (append (list 'quote) (list |y|) 'nil)))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun abe-legal (d1 d2 |y| a |x| s d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (abe-legal-hyps d1 d2 |y| a |x| s d1-hyps d2-hyps)
    (abe-legal-wffs d1 d2 |y| a |x| s d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun abe-legal-hyps (d1 d2 |y| a |x| s d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore |y| a |x| s))
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


(defun abe-legal-wffs (d1 d2 |y| a |x| s d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (abe-mv2 abe-mv1 abe-mv0 abe-ml0))
      (unquoted maint::wffarg (|y| a |x| s)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (abe-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (abe-d1 abe-d2))
      (unquoted maint::linearg (d1 d2)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do
     ((quoted maint::restr (abe-restr0 abe-restr1 abe-restr2 abe-restr3)))
     (let ((maint::rstr (get maint::restr 'restr-call)))
       (when
           (not
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr))))
         (throwfail "Rule not applicable.  "
                    t
                    "Restrictions "
                    (car maint::rstr)
                    " not satisfied.  "))))))


;;;
;;; Rule: ABU
;;;

;;; The rule command definition.

(defmexpr abu
  (argtypes line line gwff gwff gwff gwff linelist linelist)
  (wffargtypes  nil nil "I" "O" "I" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 |y| a |x| s p2-hyps p1-hyps)
  (arghelp "Higher Universally Quantified Line" "Lower Universally Quantified Line" "Universally Quantified Variable in Lower Line" "Scope of Quantifier in Higher Line" "Universally Quantified Variable in Higher Line" "Scope of Quantifer in Lower Line" "Hypotheses" "Hypotheses")
  (defaultfns abu-defaults)
  (mainfns abu-legal abu-build)
  (enterfns abu-enter)
  (mhelp "Rule to change a top level occurrence of a universally quantified
variable."))

;;; The line assertions justifications and restrictions

(defrulewffs abu
  (unique-lines 
     (abu-p2 "FORALL x(I) A(O)")
     (abu-p1 "FORALL y(I) `(S  y  x(I)  A(O))"))
  (unique-restrictions 
     (abu-restr0 (free-for "y(I)" "x(I)" "A(O)"))
     (abu-restr1 (is-variable "y(I)"))
     (abu-restr2 (is-variable "x(I)"))
     (abu-restr3 (not-free-in "y(I)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule abu
  (matchfn abu-match)
  (match1fn abu-match1)
  (shortfn abu-short))


;;; The building function. 


(defun abu-build (p2 p1 |y| a |x| s p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (abu-mv2 abu-mv1 abu-mv0 abu-ml0))
      (unquoted maint::wffarg (|y| a |x| s)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (abu-p2 abu-p1))
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
       ((list "AB"
              (mapcar #'meta-subst '(abu-mv0))
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
    (append maint::new-line-labels (list |y| a |x| s p2-hyps p1-hyps))))

;;; The entering function. 


(defun abu-enter (p2 p1 |y| a |x| s p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (abu-legal p2 p1 |y| a |x| s p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun abu-defaults (p2 p1 |y| a |x| s p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (abu-mv2 abu-mv1 abu-mv0 abu-ml0))
      (unquoted maint::wffarg (|y| a |x| s)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (abu-p2 abu-p1))
      (unquoted maint::linearg (p2 p1)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel (abu-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (abu-mv2 abu-mv1 abu-mv0 abu-ml0))
      (unquoted maint::wffarg (|y| a |x| s)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (abu-hyp-defaults p2 p1 |y| a |x| s p2-hyps p1-hyps))
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
                  (mapcar #'specified-p (list |y| a |x| s))
                  maint::strong-hypdefaults))
    (list p2 p1 |y| a |x| s p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun abu-hyp-defaults (p2 p1 |y| a |x| s p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore |y| a |x| s))
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


(defun abu-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn
           (%catch%
            (match-bind (get 'abu-p2 'meta-assertion)
                        (get (car maint::plan-support) 'assertion))
            (fail (throwfail "Planned lines did not match.")))
           (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do
           ((quoted maint::restr
             (abu-restr0 abu-restr1 abu-restr2 abu-restr3)))
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


(defun abu-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and
     (not
      (eq 'maint::failed
          (%catch%
           (match-bind (get 'abu-p2 'meta-assertion) (get pline 'assertion))
           (fail 'maint::failed))))
     t
     (let ((maint::rstr (get 'abu-restr0 'restr-call)))
       (%catch%
        (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
        (fail t)))
     (let ((maint::rstr (get 'abu-restr1 'restr-call)))
       (%catch%
        (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
        (fail t)))
     (let ((maint::rstr (get 'abu-restr2 'restr-call)))
       (%catch%
        (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
        (fail t)))
     (let ((maint::rstr (get 'abu-restr3 'restr-call)))
       (%catch%
        (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr)))
        (fail t))))))

;;; The short version of the rule as a function.  


(defun abu-short (p2 |y|)
  (funcall #'comdecode
           (append (list 'abu)
                   (list (linealias p2))
                   (list '$)
                   (append (list (append (list 'quote) (list |y|) 'nil)))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun abu-legal (p2 p1 |y| a |x| s p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (abu-legal-hyps p2 p1 |y| a |x| s p2-hyps p1-hyps)
    (abu-legal-wffs p2 p1 |y| a |x| s p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun abu-legal-hyps (p2 p1 |y| a |x| s p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore |y| a |x| s))
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


(defun abu-legal-wffs (p2 p1 |y| a |x| s p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (abu-mv2 abu-mv1 abu-mv0 abu-ml0))
      (unquoted maint::wffarg (|y| a |x| s)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (abu-ml0)))
              (when
                  (not
                   (same-match-p maint::metalabel
                                 (wffeval maint::metalabel)
                                 (meta-label-eval maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do
     ((quoted maint::unique-line (abu-p2 abu-p1))
      (unquoted maint::linearg (p2 p1)))
     (when (existent-p maint::linearg)
       (match-bind (get maint::unique-line 'meta-assertion)
                   (get (numalias maint::linearg) 'assertion))))
    (macro-do
     ((quoted maint::restr (abu-restr0 abu-restr1 abu-restr2 abu-restr3)))
     (let ((maint::rstr (get maint::restr 'restr-call)))
       (when
           (not
            (apply (car maint::rstr) (mapcar #'meta-subst (cdr maint::rstr))))
         (throwfail "Rule not applicable.  "
                    t
                    "Restrictions "
                    (car maint::rstr)
                    " not satisfied.  "))))))

(context RULES-2-PROP)


;;;
;;; Rule: IMP-DISJ
;;;

;;; The rule command definition.

(defmexpr imp-disj
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Line with Implication" "Line with Disjunction" "Succedent of Implication" "Antecedent of Implication" "Hypotheses" "Hypotheses")
  (defaultfns imp-disj-defaults)
  (mainfns imp-disj-legal imp-disj-build)
  (enterfns imp-disj-enter)
  (mhelp "Rule to replace an implication by a disjunction."))

;;; The line assertions justifications and restrictions

(defrulewffs imp-disj
  (unique-lines 
     (imp-disj-d1 "A(O) IMPLIES B(O)")
     (imp-disj-d2 "~A(O) OR B(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule imp-disj
  (matchfn imp-disj-match)
  (match1fn imp-disj-match1)
  (shortfn imp-disj-short))


;;; The building function. 


(defun imp-disj-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (imp-disj-mv1 imp-disj-mv0))
      (unquoted maint::wffarg (b a)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (imp-disj-d1 imp-disj-d2))
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
        (list "Imp-Disj"
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
    (append maint::new-line-labels (list b a d1-hyps d2-hyps))))

;;; The entering function. 


(defun imp-disj-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (imp-disj-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun imp-disj-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (imp-disj-mv1 imp-disj-mv0))
      (unquoted maint::wffarg (b a)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (imp-disj-d1 imp-disj-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (imp-disj-mv1 imp-disj-mv0))
      (unquoted maint::wffarg (b a)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (imp-disj-hyp-defaults d1 d2 b a d1-hyps d2-hyps))
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
                  (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list d1 d2 b a d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun imp-disj-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
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


(defun imp-disj-match (maint::plan-support)
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
                (match-bind (get 'imp-disj-d1 'meta-assertion)
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


(defun imp-disj-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'imp-disj-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun imp-disj-short (d1)
  (funcall #'comdecode
           (append (list 'imp-disj)
                   (list (linealias d1))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun imp-disj-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (imp-disj-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (imp-disj-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun imp-disj-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun imp-disj-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (imp-disj-mv1 imp-disj-mv0))
      (unquoted maint::wffarg (b a)))
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
     ((quoted maint::unique-line (imp-disj-d1 imp-disj-d2))
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
;;; Rule: DISJ-IMP
;;;

;;; The rule command definition.

(defmexpr disj-imp
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Line with Disjunction" "Line with Implication" "Succedent of Implication" "Antecedent of Implication" "Hypotheses" "Hypotheses")
  (defaultfns disj-imp-defaults)
  (mainfns disj-imp-legal disj-imp-build)
  (enterfns disj-imp-enter)
  (mhelp "Rule to replace a disjunction by an implication."))

;;; The line assertions justifications and restrictions

(defrulewffs disj-imp
  (unique-lines 
     (disj-imp-d1 "~A(O) OR B(O)")
     (disj-imp-d2 "A(O) IMPLIES B(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule disj-imp
  (matchfn disj-imp-match)
  (match1fn disj-imp-match1)
  (shortfn disj-imp-short))


;;; The building function. 


(defun disj-imp-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (disj-imp-mv1 disj-imp-mv0))
      (unquoted maint::wffarg (b a)))
     (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do
     ((quoted maint::u-line (disj-imp-d1 disj-imp-d2))
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
        (list "Disj-Imp"
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
    (append maint::new-line-labels (list b a d1-hyps d2-hyps))))

;;; The entering function. 


(defun disj-imp-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (disj-imp-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss))) (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun disj-imp-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do
     ((quoted maint::metavar (disj-imp-mv1 disj-imp-mv0))
      (unquoted maint::wffarg (b a)))
     (when (specified-p maint::wffarg)
       (push (cons maint::metavar maint::wffarg) wffbindings)))
    (macro-do
     ((quoted maint::unique-line (disj-imp-d1 disj-imp-d2))
      (unquoted maint::linearg (d1 d2)))
     (if (and (not (eq maint::linearg '$)) (existent-p maint::linearg))
         (match-bind (get maint::unique-line 'meta-assertion)
                     (get (numalias maint::linearg) 'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do
     ((quoted maint::metavar (disj-imp-mv1 disj-imp-mv0))
      (unquoted maint::wffarg (b a)))
     (let ((maint::wffval (wffeval maint::metavar)))
       (when maint::wffval (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (disj-imp-hyp-defaults d1 d2 b a d1-hyps d2-hyps))
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
                  (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list d1 d2 b a d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun disj-imp-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
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


(defun disj-imp-match (maint::plan-support)
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
                (match-bind (get 'disj-imp-d1 'meta-assertion)
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


(defun disj-imp-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not
          (eq 'maint::failed
              (%catch%
               (match-bind (get 'disj-imp-d1 'meta-assertion)
                           (get maint::sline 'assertion))
               (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun disj-imp-short (d1)
  (funcall #'comdecode
           (append (list 'disj-imp)
                   (list (linealias d1))
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$)
                   (list '$))))

;;; The checking function. 


(defun disj-imp-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (disj-imp-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (disj-imp-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun disj-imp-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun disj-imp-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do
     ((quoted maint::metavar (disj-imp-mv1 disj-imp-mv0))
      (unquoted maint::wffarg (b a)))
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
     ((quoted maint::unique-line (disj-imp-d1 disj-imp-d2))
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
