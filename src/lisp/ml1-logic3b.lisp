;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml1-logic3b.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml1-logic3b.rules"
;;;
;;; contains rules
;;; ABSURD ENEG INEG 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml1-logic3b
  (contents ABSURD ENEG INEG ))

(context rule-commands)

(context RULES-3-NEG)


;;;
;;; Rule: ABSURD
;;;

;;; The rule command definition.

(defmexpr absurd
  (argtypes line line gwff linelist linelist)
  (wffargtypes  nil nil "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a p2-hyps p1-hyps)
  (arghelp "Line to be Proved" "Line with Falsehood" "Wff to be Proved" "Hypotheses" "Hypotheses")
  (defaultfns absurd-defaults)
  (mainfns absurd-legal absurd-build)
  (enterfns absurd-enter)
  (mhelp "Rule of Intuitionistic Absurdity."))

;;; The line assertions justifications and restrictions

(defrulewffs absurd
  (unique-lines 
     (absurd-p2 "A(O)")
     (absurd-p1 "FALSEHOOD"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule absurd
  (matchfn absurd-match)
  (match1fn absurd-match1)
  (shortfn absurd-short))


;;; The building function. 


(defun absurd-build (p2 p1 a p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (absurd-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (absurd-p2 absurd-p1))
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
                ((list "Absurd" (mapcar #'meta-subst 'nil)
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
    (append maint::new-line-labels (list a p2-hyps p1-hyps))))

;;; The entering function. 


(defun absurd-enter (p2 p1 a p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (absurd-legal p2 p1 a p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun absurd-defaults (p2 p1 a p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (absurd-mv0))
               (unquoted maint::wffarg (a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (absurd-p2 absurd-p1))
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
    (macro-do ((quoted maint::metavar (absurd-mv0))
               (unquoted maint::wffarg (a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (absurd-hyp-defaults p2 p1 a p2-hyps p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p2-hyps p1-hyps)))))
    (setq-destruct ((p2 'ss))
                   (line-no-defaults-from (eval-destruct ((p2 'ss)))))
    (when (not (member '$ (list p2 'ss)))
      (setq-destruct ((p1 'ss))
                     (line-no-defaults-to (eval-destruct ((p2 'ss)))
                                          (eval-destruct ((p1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list a))
                  maint::strong-hypdefaults))
    (list p2 p1 a p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun absurd-hyp-defaults (p2 p1 a p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a))
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


(defun absurd-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'absurd-p2 'meta-assertion)
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


(defun absurd-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'absurd-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun absurd-short (p2)
  (funcall #'comdecode
           (append (list 'absurd) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun absurd-legal (p2 p1 a p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (absurd-legal-hyps p2 p1 a p2-hyps p1-hyps)
    (absurd-legal-wffs p2 p1 a p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun absurd-legal-hyps (p2 p1 a p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a))
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


(defun absurd-legal-wffs (p2 p1 a p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (absurd-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (absurd-p2 absurd-p1))
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
;;; Rule: ENEG
;;;

;;; The rule command definition.

(defmexpr eneg
  (argtypes line line line gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" nil nil nil) (wffop-typelist)
  (argnames p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
  (arghelp "Line with Falsehood" "Line with Negative Wff" "Line with Positive Wff" "Wff" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns eneg-defaults)
  (mainfns eneg-legal eneg-build)
  (enterfns eneg-enter)
  (mhelp "Rule of Negation Elimination."))

;;; The line assertions justifications and restrictions

(defrulewffs eneg
  (unique-lines 
     (eneg-p3 "FALSEHOOD")
     (eneg-d1 "~A(O)")
     (eneg-p2 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule eneg
  (matchfn eneg-match)
  (match1fn eneg-match1)
  (shortfn eneg-short))


;;; The building function. 


(defun eneg-build (p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (eneg-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (eneg-p3 eneg-d1 eneg-p2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p3 d1 p2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "NegElim" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1 p2)))
                 (nextplan) (nextplan)))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p3-hyps d1-hyps p2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p3-hyps d1-hyps p2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a p3-hyps d1-hyps p2-hyps))))

;;; The entering function. 


(defun eneg-enter (p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (eneg-legal p3 d1 p2 a p3-hyps d1-hyps p2-hyps))
  (update-plan (eval-destruct ((p3 d1 'ss)))
               (eval-destruct ((p2 'ss)))))

;;; The default function. 


(defun eneg-defaults (p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (eneg-mv0))
               (unquoted maint::wffarg (a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (eneg-p3 eneg-d1 eneg-p2))
               (unquoted maint::linearg (p3 d1 p2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (eneg-mv0))
               (unquoted maint::wffarg (a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p3-hyps d1-hyps p2-hyps))
      (if (not (member '$ (list p3 d1 p2)))
          (setq-destruct (p3-hyps d1-hyps p2-hyps)
                         (eneg-hyp-defaults p3 d1 p2 a p3-hyps d1-hyps
                                            p2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p3-hyps d1-hyps p2-hyps)))))
    (setq-destruct ((p3 d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct ((p3 d1 'ss)))))
    (when (not (member '$ (list p3 d1 'ss)))
      (setq-destruct ((p2 'ss))
                     (line-no-defaults-to (eval-destruct ((p3 d1 'ss)))
                                          (eval-destruct ((p2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil) (mapcar #'specified-p (list a))
                  maint::strong-hypdefaults))
    (list p3 d1 p2 a p3-hyps d1-hyps p2-hyps)))

;;; The hypotheses default function. 


(defun eneg-hyp-defaults (p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p3 d1 p2))
               (unquoted maint::hyparg (p3-hyps d1-hyps p2-hyps)))
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
    (when (specified-p d1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d1-hyps (list)))))
    (when (specified-p p2-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p2-hyps (list)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p3-hyps d1-hyps p2-hyps)))
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
                             (list p3-hyps d1-hyps p2-hyps))))
             (when (not (specified-p p3-hyps))
               (setq p3-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p p2-hyps))
               (setq p2-hyps (ordered-join-h maint::hupper (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p3-hyps d1-hyps p2-hyps))))))
    (list p3-hyps d1-hyps p2-hyps)))

;;; The matching function. 


(defun eneg-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'eneg-p3 'meta-assertion)
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
                                           'eneg-d1
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


(defun eneg-match1 (pline maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'eneg-p3 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'eneg-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun eneg-short (p3 d1)
  (funcall #'comdecode
           (append (list 'eneg) (list (linealias p3))
                   (list (linealias d1)) (list '$) (list '$) (list '$)
                   (list '$) (list '$))))

;;; The checking function. 


(defun eneg-legal (p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (eneg-legal-hyps p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
    (eneg-legal-wffs p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
    t))

;;; The hypotheses checking function. 


(defun eneg-legal-hyps (p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p3 d1 p2))
               (unquoted maint::hyparg (p3-hyps d1-hyps p2-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p3-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p2-hyps (list))))
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


(defun eneg-legal-wffs (p3 d1 p2 a p3-hyps d1-hyps p2-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p3-hyps d1-hyps p2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (eneg-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (eneg-p3 eneg-d1 eneg-p2))
               (unquoted maint::linearg (p3 d1 p2)))
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
;;; Rule: INEG
;;;

;;; The rule command definition.

(defmexpr ineg
  (argtypes line line line gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" nil nil nil) (wffop-typelist)
  (argnames p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (arghelp "Line with Negation" "Line with Contradiction" "Line with Assumption" "Wff Whose Negation is to be Proved" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns ineg-defaults)
  (mainfns ineg-legal ineg-build)
  (enterfns ineg-enter)
  (mhelp "Rule of Negation Introduction"))

;;; The line assertions justifications and restrictions

(defrulewffs ineg
  (unique-lines 
     (ineg-p3 "~A(O)")
     (ineg-p2 "FALSEHOOD")
     (ineg-h1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule ineg
  (matchfn ineg-match)
  (match1fn ineg-match1)
  (shortfn ineg-short))


;;; The building function. 


(defun ineg-build (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ineg-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (ineg-p3 ineg-p2 ineg-h1))
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
                ((list "NegIntro" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list p2)))
                 (nextplan)
                 (list "Hyp" (mapcar #'meta-subst 'nil)
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


(defun ineg-enter (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (ineg-legal p3 p2 h1 a p3-hyps p2-hyps h1-hyps))
  (update-plan (eval-destruct ((p3 'ss)))
               (eval-destruct ((p2 h1 'ss)))))

;;; The default function. 


(defun ineg-defaults (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (ineg-mv0))
               (unquoted maint::wffarg (a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (ineg-p3 ineg-p2 ineg-h1))
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
    (macro-do ((quoted maint::metavar (ineg-mv0))
               (unquoted maint::wffarg (a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p3-hyps p2-hyps h1-hyps))
      (if (not (member '$ (list p3 p2 h1)))
          (setq-destruct (p3-hyps p2-hyps h1-hyps)
                         (ineg-hyp-defaults p3 p2 h1 a p3-hyps p2-hyps
                                            h1-hyps))
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


(defun ineg-hyp-defaults (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
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


(defun ineg-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'ineg-p3 'meta-assertion)
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


(defun ineg-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'ineg-p3 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun ineg-short (p3)
  (funcall #'comdecode
           (append (list 'ineg) (list (linealias p3)) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun ineg-legal (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (ineg-legal-hyps p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
    (ineg-legal-wffs p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
    t))

;;; The hypotheses checking function. 


(defun ineg-legal-hyps (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
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


(defun ineg-legal-wffs (p3 p2 h1 a p3-hyps p2-hyps h1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p3-hyps p2-hyps h1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ineg-mv0))
               (unquoted maint::wffarg (a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (ineg-p3 ineg-p2 ineg-h1))
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
