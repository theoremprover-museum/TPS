;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml1-logic0.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml1-logic0.rules"
;;;
;;; contains rules
;;; SAME LEMMA HYP 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml1-logic0
  (contents SAME LEMMA HYP ))

(context rule-commands)

(context RULES-1-MISC)


;;;
;;; Rule: SAME
;;;

;;; The rule command definition.

(defmexpr same
  (argtypes line line gwff linelist linelist)
  (wffargtypes  nil nil "O" nil nil) (wffop-typelist)
  (argnames p2 d1 a p2-hyps d1-hyps)
  (arghelp "Higher Line" "Lower Line" "Assertion of Both Lines" "Hypotheses" "Hypotheses")
  (defaultfns same-defaults)
  (mainfns same-legal same-build)
  (enterfns same-enter)
  (mhelp "Use the fact that two lines are identical to justify a planned line."))

;;; The line assertions justifications and restrictions

(defrulewffs same
  (unique-lines 
     (same-p2 "A(O)")
     (same-d1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule same
  (matchfn same-match)
  (match1fn same-match1)
  (shortfn same-short))


;;; The building function. 


(defun same-build (p2 d1 a p2-hyps d1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (same-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (same-p2 same-d1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p2 d1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Same as" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))
                 (nextplan)))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps d1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps d1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a p2-hyps d1-hyps))))

;;; The entering function. 


(defun same-enter (p2 d1 a p2-hyps d1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (same-legal p2 d1 a p2-hyps d1-hyps))
  (update-plan (eval-destruct ((p2 d1 'ss))) (eval-destruct nil)))

;;; The default function. 


(defun same-defaults (p2 d1 a p2-hyps d1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (same-mv0))
               (unquoted maint::wffarg (a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (same-p2 same-d1))
               (unquoted maint::linearg (p2 d1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (same-mv0))
               (unquoted maint::wffarg (a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps d1-hyps))
      (if (not (member '$ (list p2 d1)))
          (setq-destruct (p2-hyps d1-hyps)
                         (same-hyp-defaults p2 d1 a p2-hyps d1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps d1-hyps)))))
    (setq-destruct ((p2 d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct ((p2 d1 'ss)))))
    (when (not (member '$ (list p2 d1 'ss)))
      (setq-destruct nil
                     (line-no-defaults-to (eval-destruct ((p2 d1 'ss)))
                                          (eval-destruct nil))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list a))
                  maint::strong-hypdefaults))
    (list p2 d1 a p2-hyps d1-hyps)))

;;; The hypotheses default function. 


(defun same-hyp-defaults (p2 d1 a p2-hyps d1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 d1))
               (unquoted maint::hyparg (p2-hyps d1-hyps)))
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
    (when (specified-p d1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d1-hyps (list)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps d1-hyps)))
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
                     (mapcar #'specified-p (list p2-hyps d1-hyps))))
             (when (not (specified-p p2-hyps))
               (setq p2-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list p2-hyps d1-hyps))))))
    (list p2-hyps d1-hyps)))

;;; The matching function. 


(defun same-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'same-p2 'meta-assertion)
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
                                           'same-d1
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


(defun same-match1 (pline maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'same-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'same-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun same-short (p2 d1)
  (funcall #'comdecode
           (append (list 'same) (list (linealias p2))
                   (list (linealias d1)) (list '$) (list '$)
                   (list '$))))

;;; The checking function. 


(defun same-legal (p2 d1 a p2-hyps d1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (same-legal-hyps p2 d1 a p2-hyps d1-hyps)
    (same-legal-wffs p2 d1 a p2-hyps d1-hyps)
    t))

;;; The hypotheses checking function. 


(defun same-legal-hyps (p2 d1 a p2-hyps d1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 d1))
               (unquoted maint::hyparg (p2-hyps d1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p2-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d1-hyps (list))))
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


(defun same-legal-wffs (p2 d1 a p2-hyps d1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps d1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (same-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (same-p2 same-d1))
               (unquoted maint::linearg (p2 d1)))
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
;;; Rule: LEMMA
;;;

;;; The rule command definition.

(defmexpr lemma
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a b p2-hyps p1-hyps)
  (arghelp "Line to be Proven Using Lemma" "Line with Lemma" "Assertion of Lemma" "Assertion of Line to be Proven" "Hypotheses" "Hypotheses")
  (defaultfns lemma-defaults)
  (mainfns lemma-legal lemma-build)
  (enterfns lemma-enter)
  (mhelp "Introduce a Lemma."))

;;; The line assertions justifications and restrictions

(defrulewffs lemma
  (unique-lines 
     (lemma-p2 "B(O)")
     (lemma-p1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule lemma
  (matchfn lemma-match)
  (match1fn lemma-match1)
  (shortfn lemma-short))


;;; The building function. 


(defun lemma-build (p2 p1 a b p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lemma-mv1 lemma-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (lemma-p2 lemma-p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p2 p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just ((nextplan) (nextplan)))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps p1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a b p2-hyps p1-hyps))))

;;; The entering function. 


(defun lemma-enter (p2 p1 a b p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (lemma-legal p2 p1 a b p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss)))
               (eval-destruct ((p2 p1 'ss) (p1 'ss)))))

;;; The default function. 


(defun lemma-defaults (p2 p1 a b p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (lemma-mv1 lemma-mv0))
               (unquoted maint::wffarg (a b)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (lemma-p2 lemma-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (lemma-mv1 lemma-mv0))
               (unquoted maint::wffarg (a b)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (lemma-hyp-defaults p2 p1 a b p2-hyps
                                             p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps p1-hyps)))))
    (setq-destruct ((p2 'ss))
                   (line-no-defaults-from (eval-destruct ((p2 'ss)))))
    (when (not (member '$ (list p2 'ss)))
      (setq-destruct-multi (p1) ((p2 p1 'ss) (p1 'ss))
                           (line-no-defaults-to (eval-destruct
                                                 ((p2 'ss)))
                                                (eval-destruct
                                                 ((p2 p1 'ss)
                                                  (p1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list a b))
                  maint::strong-hypdefaults))
    (list p2 p1 a b p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun lemma-hyp-defaults (p2 p1 a b p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a b))
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
      (setq maint::hlower
            (join-h maint::hlower (set-difference p2-hyps (list)))))
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
               (setq p2-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p p1-hyps))
               (setq p1-hyps (ordered-join-h maint::hupper (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list p2-hyps p1-hyps))))))
    (list p2-hyps p1-hyps)))

;;; The matching function. 


(defun lemma-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'lemma-p2 'meta-assertion)
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


(defun lemma-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'lemma-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun lemma-short (p2 a)
  (funcall #'comdecode
           (append (list 'lemma) (list (linealias p2)) (list '$)
                   (append (list (append (list 'quote) (list a) 'nil)))
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun lemma-legal (p2 p1 a b p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (lemma-legal-hyps p2 p1 a b p2-hyps p1-hyps)
    (lemma-legal-wffs p2 p1 a b p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun lemma-legal-hyps (p2 p1 a b p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a b))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 p1))
               (unquoted maint::hyparg (p2-hyps p1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p2-hyps (list))))
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


(defun lemma-legal-wffs (p2 p1 a b p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lemma-mv1 lemma-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (lemma-p2 lemma-p1))
               (unquoted maint::linearg (p2 p1)))
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
;;; Rule: HYP
;;;

;;; The rule command definition.

(defmexpr hyp
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 h1 a b p2-hyps h1-hyps)
  (arghelp "Line to be Proven using Hypothesis" "Line with Hypothesis" "Hypothesis" "Theorem to be Proven with extra Hypothesis" "Hypotheses" "Hypotheses")
  (defaultfns hyp-defaults)
  (mainfns hyp-legal hyp-build)
  (enterfns hyp-enter)
  (mhelp "Introduce a new hypothesis line into the proof outline."))

;;; The line assertions justifications and restrictions

(defrulewffs hyp
  (unique-lines 
     (hyp-p2 "B(O)")
     (hyp-h1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule hyp
  (matchfn hyp-match)
  (match1fn hyp-match1)
  (shortfn hyp-short))


;;; The building function. 


(defun hyp-build (p2 h1 a b p2-hyps h1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (hyp-mv1 hyp-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (hyp-p2 hyp-h1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p2 h1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((nextplan)
                 (list "Hyp" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps h1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p2-hyps h1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a b p2-hyps h1-hyps))))

;;; The entering function. 


(defun hyp-enter (p2 h1 a b p2-hyps h1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (hyp-legal p2 h1 a b p2-hyps h1-hyps))
  (update-plan (eval-destruct ((p2 'ss)))
               (eval-destruct ((p2 h1 'ss)))))

;;; The default function. 


(defun hyp-defaults (p2 h1 a b p2-hyps h1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (hyp-mv1 hyp-mv0))
               (unquoted maint::wffarg (a b)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (hyp-p2 hyp-h1))
               (unquoted maint::linearg (p2 h1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (hyp-mv1 hyp-mv0))
               (unquoted maint::wffarg (a b)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps h1-hyps))
      (if (not (member '$ (list p2 h1)))
          (setq-destruct (p2-hyps h1-hyps)
                         (hyp-hyp-defaults p2 h1 a b p2-hyps h1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps h1-hyps)))))
    (setq-destruct ((p2 'ss))
                   (line-no-defaults-from (eval-destruct ((p2 'ss)))))
    (when (not (member '$ (list p2 'ss)))
      (setq-destruct ((p2 h1 'ss))
                     (line-no-defaults-to (eval-destruct ((p2 'ss)))
                                          (eval-destruct
                                           ((p2 h1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list a b))
                  maint::strong-hypdefaults))
    (list p2 h1 a b p2-hyps h1-hyps)))

;;; The hypotheses default function. 


(defun hyp-hyp-defaults (p2 h1 a b p2-hyps h1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a b))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 h1))
               (unquoted maint::hyparg (p2-hyps h1-hyps)))
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
      (setq maint::hlower
            (join-h maint::hlower (set-difference p2-hyps (list)))))
    (when (specified-p h1-hyps)
      (when (not (set-eq h1-hyps (list h1)))
        (throwfail "Illegal hypotheses for hyp line " (h1 . line)
                   ".")))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps h1-hyps)))
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
                     (mapcar #'specified-p (list p2-hyps h1-hyps))))
             (when (not (specified-p p2-hyps))
               (setq p2-hyps (ordered-join-h maint::hupper (list))))
             (setq h1-hyps (list h1))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list p2-hyps h1-hyps))))))
    (list p2-hyps h1-hyps)))

;;; The matching function. 


(defun hyp-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'hyp-p2 'meta-assertion)
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


(defun hyp-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'hyp-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun hyp-short (p2 a)
  (funcall #'comdecode
           (append (list 'hyp) (list (linealias p2)) (list '$)
                   (append (list (append (list 'quote) (list a) 'nil)))
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun hyp-legal (p2 h1 a b p2-hyps h1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (hyp-legal-hyps p2 h1 a b p2-hyps h1-hyps)
    (hyp-legal-wffs p2 h1 a b p2-hyps h1-hyps)
    t))

;;; The hypotheses checking function. 


(defun hyp-legal-hyps (p2 h1 a b p2-hyps h1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a b))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p2 h1))
               (unquoted maint::hyparg (p2-hyps h1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p2-hyps (list))))
    (when (not (set-eq h1-hyps (list h1)))
      (throwfail "Illegal hypotheses for hyp line " (h1 . line) "."))
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


(defun hyp-legal-wffs (p2 h1 a b p2-hyps h1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps h1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (hyp-mv1 hyp-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (hyp-p2 hyp-h1))
               (unquoted maint::linearg (p2 h1)))
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
