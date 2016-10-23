;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-logic7c.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-logic7c.rules"
;;;
;;; contains rules
;;; ETA* BETA* LAMBDA* LET 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-logic7c
  (contents ETA* BETA* LAMBDA* LET ))

(context rule-commands)

(context RULES-8-LAMBDA)


;;;
;;; Rule: ETA*
;;;

;;; The rule command definition.

(defmexpr eta*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Lower Line" "Higher Line" "Wff on Higher Line" "Wff on Lower Line" "Hypotheses" "Hypotheses")
  (defaultfns eta*-defaults)
  (mainfns eta*-legal eta*-build)
  (enterfns eta*-enter)
  (mhelp "Rule to infer a line from one which is equal up to lambda conversion
using eta rule (but NOT beta rule) and alphabetic change of bound variables."))

;;; The line assertions justifications and restrictions

(defrulewffs eta*
  (unique-lines 
     (eta*-d1 "A(O)")
     (eta*-d2 "B(O)"))
  (unique-restrictions 
     (eta*-restr0 (wffeq-ab-eta "A(O)" "B(O)"))))

;;; The suggesting rule definition.

(defsrule eta*
  (matchfn eta*-match)
  (match1fn eta*-match1)
  (shortfn eta*-short))


;;; The building function. 


(defun eta*-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (eta*-mv1 eta*-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (eta*-d1 eta*-d2))
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
                 (list "Eta Rule" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list b a d1-hyps d2-hyps))))

;;; The entering function. 


(defun eta*-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (eta*-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun eta*-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (eta*-mv1 eta*-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (eta*-d1 eta*-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (eta*-mv1 eta*-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (eta*-hyp-defaults d1 d2 b a d1-hyps d2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss))
                     (line-no-defaults-to (eval-destruct
                                           (('pp d1 'ss)))
                                          (eval-destruct
                                           (('pp d2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list d1 d2 b a d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun eta*-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d1 d2))
               (unquoted maint::hyparg (d1-hyps d2-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
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


(defun eta*-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan nil)
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
                                           'eta*-d1
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (eta*-restr0)))
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


(defun eta*-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'eta*-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'eta*-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun eta*-short (d1 b)
  (funcall #'comdecode
           (append (list 'eta*) (list (linealias d1)) (list '$)
                   (append (list (append (list 'quote) (list b) 'nil)))
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun eta*-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (eta*-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (eta*-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun eta*-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun eta*-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (eta*-mv1 eta*-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (eta*-d1 eta*-d2))
               (unquoted maint::linearg (d1 d2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (eta*-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: BETA*
;;;

;;; The rule command definition.

(defmexpr beta*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Lower Line" "Higher Line" "Wff on Higher Line" "Wff on Lower Line" "Hypotheses" "Hypotheses")
  (defaultfns beta*-defaults)
  (mainfns beta*-legal beta*-build)
  (enterfns beta*-enter)
  (mhelp "Rule to infer a line from one which is equal up to lambda conversion
using beta rule (but NOT eta rule) and alphabetic change of bound variables."))

;;; The line assertions justifications and restrictions

(defrulewffs beta*
  (unique-lines 
     (beta*-d1 "A(O)")
     (beta*-d2 "B(O)"))
  (unique-restrictions 
     (beta*-restr0 (wffeq-ab-beta "A(O)" "B(O)"))))

;;; The suggesting rule definition.

(defsrule beta*
  (matchfn beta*-match)
  (match1fn beta*-match1)
  (shortfn beta*-short))


;;; The building function. 


(defun beta*-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (beta*-mv1 beta*-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (beta*-d1 beta*-d2))
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
                 (list "Beta Rule" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list b a d1-hyps d2-hyps))))

;;; The entering function. 


(defun beta*-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (beta*-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun beta*-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (beta*-mv1 beta*-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (beta*-d1 beta*-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (beta*-mv1 beta*-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (beta*-hyp-defaults d1 d2 b a d1-hyps
                          d2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss))
                     (line-no-defaults-to (eval-destruct
                                           (('pp d1 'ss)))
                                          (eval-destruct
                                           (('pp d2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list d1 d2 b a d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun beta*-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d1 d2))
               (unquoted maint::hyparg (d1-hyps d2-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
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


(defun beta*-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan nil)
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
                                           'beta*-d1
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (beta*-restr0)))
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


(defun beta*-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'beta*-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'beta*-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun beta*-short (d1 b)
  (funcall #'comdecode
           (append (list 'beta*) (list (linealias d1)) (list '$)
                   (append (list (append (list 'quote) (list b) 'nil)))
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun beta*-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (beta*-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (beta*-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun beta*-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun beta*-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (beta*-mv1 beta*-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (beta*-d1 beta*-d2))
               (unquoted maint::linearg (d1 d2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (beta*-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: LAMBDA*
;;;

;;; The rule command definition.

(defmexpr lambda*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Lower Line" "Higher Line" "Wff on Higher Line" "Wff on Lower Line" "Hypotheses" "Hypotheses")
  (defaultfns lambda*-defaults)
  (mainfns lambda*-legal lambda*-build)
  (enterfns lambda*-enter)
  (mhelp "Rule to infer a line from one which is equal up to lambda conversion
using both beta and eta rules and alphabetic change of bound variables."))

;;; The line assertions justifications and restrictions

(defrulewffs lambda*
  (unique-lines 
     (lambda*-d1 "A(O)")
     (lambda*-d2 "B(O)"))
  (unique-restrictions 
     (lambda*-restr0 (wffeq-ab-lambda "A(O)" "B(O)"))))

;;; The suggesting rule definition.

(defsrule lambda*
  (matchfn lambda*-match)
  (match1fn lambda*-match1)
  (shortfn lambda*-short))


;;; The building function. 


(defun lambda*-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lambda*-mv1 lambda*-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (lambda*-d1 lambda*-d2))
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
                 (list "Lambda=" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list b a d1-hyps d2-hyps))))

;;; The entering function. 


(defun lambda*-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (lambda*-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun lambda*-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (lambda*-mv1 lambda*-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (lambda*-d1 lambda*-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (lambda*-mv1 lambda*-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (lambda*-hyp-defaults d1 d2 b a d1-hyps
                          d2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d1-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 'ss))
                     (line-no-defaults-to (eval-destruct
                                           (('pp d1 'ss)))
                                          (eval-destruct
                                           (('pp d2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list d1 d2 b a d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun lambda*-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d1 d2))
               (unquoted maint::hyparg (d1-hyps d2-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
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


(defun lambda*-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan nil)
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
                                           'lambda*-d1
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (lambda*-restr0)))
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


(defun lambda*-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'lambda*-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'lambda*-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun lambda*-short (d1 b)
  (funcall #'comdecode
           (append (list 'lambda*) (list (linealias d1)) (list '$)
                   (append (list (append (list 'quote) (list b) 'nil)))
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun lambda*-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (lambda*-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (lambda*-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun lambda*-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun lambda*-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lambda*-mv1 lambda*-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (lambda*-d1 lambda*-d2))
               (unquoted maint::linearg (d1 d2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (lambda*-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))

(context RULES-6-EQUALITY)


;;;
;;; Rule: LET
;;;

;;; The rule command definition.

(defmexpr let
  (argtypes line line line line line gwff gwff gwff linelist linelist linelist linelist linelist)
  (wffargtypes  nil nil nil nil nil "A" "A" "O" nil nil nil nil nil) (wffop-typelist "A")
  (argnames p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
  (arghelp "Plan Line" "New Plan Line" "Line where variable is chosen" "Auxiliary existential line" "Auxiliary line with reflexivity" "Term" "Variable" "" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns let-defaults)
  (mainfns let-legal let-build)
  (enterfns let-enter)
  (mhelp "Bind a  variable to a term."))

;;; The line assertions justifications and restrictions

(defrulewffs let
  (unique-lines 
     (let-p5 "C(O)")
     (let-p4 "C(O)")
     (let-h3 "x(A) =(OAA) A(A)")
     (let-d2 "EXISTS x(A).x(A) =(OAA) A(A)")
     (let-d1 "A(A) =(OAA) A(A)"))
  (unique-restrictions 
     (let-restr0 (not-free-in-hyps "x(A)"))
     (let-restr1 (is-variable "x(A)"))
     (let-restr2 (not-free-in "x(A)" "C(O)"))))

;;; The suggesting rule definition.

(defsrule let
  (matchfn let-match)
  (match1fn let-match1)
  (shortfn let-short))


;;; The building function. 


(defun let-build
    (p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
  (let ((maint::new-line-labels (line-label-vec 5))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (let-mv2 let-mv1 let-mv0))
               (unquoted maint::wffarg (a |x| c)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (let-p5 let-p4 let-h3 let-d2 let-d1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p5 p4 h3 d2 d1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "RuleC" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d2 p4)))
                 (nextplan)
                 (list "Choose" (mapcar #'meta-subst '(let-mv1))
                       (subst-labels maint::num-alist (list)))
                 (list "EGen" (mapcar #'meta-subst '(let-mv1))
                       (subst-labels maint::num-alist (list d1)))
                 (list "Refl=" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg
                (p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg
                (p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps))))

;;; The entering function. 


(defun let-enter
    (p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (let-legal p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps
     d1-hyps))
  (update-plan (eval-destruct ((p5 'ss)))
               (eval-destruct ((p4 'ss d1 d2 h3)))))

;;; The default function. 


(defun let-defaults
    (p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (let-mv2 let-mv1 let-mv0))
               (unquoted maint::wffarg (a |x| c)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (let-p5 let-p4 let-h3 let-d2 let-d1))
               (unquoted maint::linearg (p5 p4 h3 d2 d1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (let-mv2 let-mv1 let-mv0))
               (unquoted maint::wffarg (a |x| c)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps))
      (if (not (member '$ (list p5 p4 h3 d2 d1)))
          (setq-destruct (p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
                         (let-hyp-defaults p5 p4 h3 d2 d1 a |x| c
                          p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p5-hyps p4-hyps h3-hyps d2-hyps
                            d1-hyps)))))
    (setq-destruct ((p5 'ss))
                   (line-no-defaults-from (eval-destruct ((p5 'ss)))))
    (when (not (member '$ (list p5 'ss)))
      (setq-destruct ((p4 'ss d1 d2 h3))
                     (line-no-defaults-to (eval-destruct ((p5 'ss)))
                                          (eval-destruct
                                           ((p4 'ss d1 d2 h3))))))
    (setq strong-defaultlist
          (append '(nil nil nil nil nil)
                  (mapcar #'specified-p (list a |x| c))
                  maint::strong-hypdefaults))
    (list p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps
          d1-hyps)))

;;; The hypotheses default function. 


(defun let-hyp-defaults
    (p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a |x| c))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p5 p4 h3 d2 d1))
               (unquoted maint::hyparg
                (p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
                                (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p5-hyps)
      (setq maint::hupper (meet-h maint::hupper p5-hyps)))
    (when (specified-p p4-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p4-hyps (list h3)))))
    (when (specified-p h3-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h3-hyps (list h3)))))
    (when (specified-p d2-hyps)
      (setq maint::hupper (meet-h maint::hupper d2-hyps)))
    (when (specified-p d1-hyps)
      (setq maint::hupper (meet-h maint::hupper d1-hyps)))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)))
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
                             (list p5-hyps p4-hyps h3-hyps d2-hyps
                                   d1-hyps))))
             (when (not (specified-p p5-hyps))
               (setq p5-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p p4-hyps))
               (setq p4-hyps (ordered-join-h maint::hupper (list h3))))
             (when (not (specified-p h3-hyps))
               (setq h3-hyps (ordered-join-h maint::hupper (list h3))))
             (when (not (specified-p d2-hyps))
               (setq d2-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hlower (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p5-hyps p4-hyps h3-hyps d2-hyps
                                   d1-hyps))))))
    (list p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)))

;;; The matching function. 


(defun let-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'let-p5 'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail
                           (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr
                      (let-restr0 let-restr1 let-restr2)))
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


(defun let-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'let-p5 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t
         (let ((maint::rstr (get 'let-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'let-restr1 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'let-restr2 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun let-short (p5 a |x|)
  (funcall #'comdecode
           (append (list 'let) (list (linealias p5)) (list '$)
                   (list '$) (list '$) (list '$)
                   (append (list (append (list 'quote) (list a) 'nil)))
                   (append (list (append
                                  (list 'quote)
                                  (list |x|)
                                  'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$))))

;;; The checking function. 


(defun let-legal
    (p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (let-legal-hyps p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps
     d2-hyps d1-hyps)
    (let-legal-wffs p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps
     d2-hyps d1-hyps)
    t))

;;; The hypotheses checking function. 


(defun let-legal-hyps
    (p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a |x| c))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p5 p4 h3 d2 d1))
               (unquoted maint::hyparg
                (p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p5-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p4-hyps (list h3))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h3-hyps h3)))
    (setq maint::hupper (meet-h maint::hupper d2-hyps))
    (setq maint::hupper (meet-h maint::hupper d1-hyps))
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


(defun let-legal-wffs
    (p5 p4 h3 d2 d1 a |x| c p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p5-hyps p4-hyps h3-hyps d2-hyps d1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (let-mv2 let-mv1 let-mv0))
               (unquoted maint::wffarg (a |x| c)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (let-p5 let-p4 let-h3 let-d2 let-d1))
               (unquoted maint::linearg (p5 p4 h3 d2 d1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr
                (let-restr0 let-restr1 let-restr2)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))
