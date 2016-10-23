;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-logic5b.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-logic5b.rules"
;;;
;;; contains rules
;;; EQUIV-IMPLICS LEXPD*-ETA LCONTR*-ETA LEXPD*-BETA LCONTR*-BETA LEXPD* LCONTR* 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-logic5b
  (contents EQUIV-IMPLICS LEXPD*-ETA LCONTR*-ETA LEXPD*-BETA LCONTR*-BETA LEXPD* LCONTR* ))

(context rule-commands)

(context RULES-2-PROP)


;;;
;;; Rule: EQUIV-IMPLICS
;;;

;;; The rule command definition.

(defmexpr equiv-implics
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 r p d1-hyps d2-hyps)
  (arghelp "Line with Equivalence" "Line with Implications in Both Directions" "Right Equivalent" "Left Equivalent" "Hypotheses" "Hypotheses")
  (defaultfns equiv-implics-defaults)
  (mainfns equiv-implics-legal equiv-implics-build)
  (enterfns equiv-implics-enter)
  (mhelp "Rule to convert an equivalence into twin implications."))

;;; The line assertions justifications and restrictions

(defrulewffs equiv-implics
  (unique-lines 
     (equiv-implics-d1 "P(O) EQUIV R(O)")
     (equiv-implics-d2 "[P(O) IMPLIES R(O)] AND.R(O) IMPLIES P(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule equiv-implics
  (matchfn equiv-implics-match)
  (match1fn equiv-implics-match1)
  (shortfn equiv-implics-short))


;;; The building function. 


(defun equiv-implics-build (d1 d2 r p d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-implics-mv1 equiv-implics-mv0))
               (unquoted maint::wffarg (r p)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (equiv-implics-d1 equiv-implics-d2))
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
                 (list "EquivImp" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list r p d1-hyps d2-hyps))))

;;; The entering function. 


(defun equiv-implics-enter (d1 d2 r p d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (equiv-implics-legal d1 d2 r p d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun equiv-implics-defaults (d1 d2 r p d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (equiv-implics-mv1 equiv-implics-mv0))
               (unquoted maint::wffarg (r p)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (equiv-implics-d1 equiv-implics-d2))
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
    (macro-do ((quoted maint::metavar
                (equiv-implics-mv1 equiv-implics-mv0))
               (unquoted maint::wffarg (r p)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (equiv-implics-hyp-defaults d1 d2 r p d1-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list r p))
                  maint::strong-hypdefaults))
    (list d1 d2 r p d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun equiv-implics-hyp-defaults (d1 d2 r p d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore r p))
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


(defun equiv-implics-match (maint::plan-support)
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
                                           'equiv-implics-d1
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


(defun equiv-implics-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'equiv-implics-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun equiv-implics-short (d1)
  (funcall #'comdecode
           (append (list 'equiv-implics) (list (linealias d1))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun equiv-implics-legal (d1 d2 r p d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (equiv-implics-legal-hyps d1 d2 r p d1-hyps d2-hyps)
    (equiv-implics-legal-wffs d1 d2 r p d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun equiv-implics-legal-hyps (d1 d2 r p d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore r p))
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


(defun equiv-implics-legal-wffs (d1 d2 r p d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-implics-mv1 equiv-implics-mv0))
               (unquoted maint::wffarg (r p)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (equiv-implics-d1 equiv-implics-d2))
               (unquoted maint::linearg (d1 d2)))
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

(context RULES-8-LAMBDA)


;;;
;;; Rule: LEXPD*-ETA
;;;

;;; The rule command definition.

(defmexpr lexpd*-eta
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a lnorm-eta p2-hyps p1-hyps)
  (arghelp "Line not in eta-normal Form" "Line in eta-normal Form" "Wff not in eta-normal Form" "" "Hypotheses" "Hypotheses")
  (defaultfns lexpd*-eta-defaults)
  (mainfns lexpd*-eta-legal lexpd*-eta-build)
  (enterfns lexpd*-eta-enter)
  (mhelp "Rule to put a planned line into eta-normal form."))

;;; The line assertions justifications and restrictions

(defrulewffs lexpd*-eta
  (unique-lines 
     (lexpd*-eta-p2 "A(O)")
     (lexpd*-eta-p1 "`(LNORM-ETA  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule lexpd*-eta
  (matchfn lexpd*-eta-match)
  (match1fn lexpd*-eta-match1)
  (shortfn lexpd*-eta-short))


;;; The building function. 


(defun lexpd*-eta-build (p2 p1 a lnorm-eta p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lexpd*-eta-mv0 lexpd*-eta-ml0))
               (unquoted maint::wffarg (a lnorm-eta)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (lexpd*-eta-p2 lexpd*-eta-p1))
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
                ((list "Eta rule" (mapcar #'meta-subst 'nil)
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
    (append maint::new-line-labels (list a lnorm-eta p2-hyps p1-hyps))))

;;; The entering function. 


(defun lexpd*-eta-enter (p2 p1 a lnorm-eta p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (lexpd*-eta-legal p2 p1 a lnorm-eta p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun lexpd*-eta-defaults (p2 p1 a lnorm-eta p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (lexpd*-eta-mv0 lexpd*-eta-ml0))
               (unquoted maint::wffarg (a lnorm-eta)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (lexpd*-eta-p2 lexpd*-eta-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (lexpd*-eta-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (lexpd*-eta-mv0 lexpd*-eta-ml0))
               (unquoted maint::wffarg (a lnorm-eta)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (lexpd*-eta-hyp-defaults p2 p1 a lnorm-eta
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
          (append '(nil nil) (mapcar #'specified-p (list a lnorm-eta))
                  maint::strong-hypdefaults))
    (list p2 p1 a lnorm-eta p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun lexpd*-eta-hyp-defaults (p2 p1 a lnorm-eta p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a lnorm-eta))
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


(defun lexpd*-eta-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'lexpd*-eta-p2
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


(defun lexpd*-eta-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'lexpd*-eta-p2
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun lexpd*-eta-short (p2)
  (funcall #'comdecode
           (append (list 'lexpd*-eta) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun lexpd*-eta-legal (p2 p1 a lnorm-eta p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (lexpd*-eta-legal-hyps p2 p1 a lnorm-eta p2-hyps p1-hyps)
    (lexpd*-eta-legal-wffs p2 p1 a lnorm-eta p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun lexpd*-eta-legal-hyps (p2 p1 a lnorm-eta p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a lnorm-eta))
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


(defun lexpd*-eta-legal-wffs (p2 p1 a lnorm-eta p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lexpd*-eta-mv0 lexpd*-eta-ml0))
               (unquoted maint::wffarg (a lnorm-eta)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (lexpd*-eta-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (lexpd*-eta-p2 lexpd*-eta-p1))
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
;;; Rule: LCONTR*-ETA
;;;

;;; The rule command definition.

(defmexpr lcontr*-eta
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a lnorm-eta d1-hyps d2-hyps)
  (arghelp "Line not in eta-normal Form" "Line in eta-normal Form" "Wff not in eta-normal Form" "" "Hypotheses" "Hypotheses")
  (defaultfns lcontr*-eta-defaults)
  (mainfns lcontr*-eta-legal lcontr*-eta-build)
  (enterfns lcontr*-eta-enter)
  (mhelp "Rule to put an inferred line into eta-normal form."))

;;; The line assertions justifications and restrictions

(defrulewffs lcontr*-eta
  (unique-lines 
     (lcontr*-eta-d1 "A(O)")
     (lcontr*-eta-d2 "`(LNORM-ETA  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule lcontr*-eta
  (matchfn lcontr*-eta-match)
  (match1fn lcontr*-eta-match1)
  (shortfn lcontr*-eta-short))


;;; The building function. 


(defun lcontr*-eta-build (d1 d2 a lnorm-eta d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (lcontr*-eta-mv0 lcontr*-eta-ml0))
               (unquoted maint::wffarg (a lnorm-eta)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (lcontr*-eta-d1 lcontr*-eta-d2))
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
                 (list "Eta rule" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a lnorm-eta d1-hyps d2-hyps))))

;;; The entering function. 


(defun lcontr*-eta-enter (d1 d2 a lnorm-eta d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (lcontr*-eta-legal d1 d2 a lnorm-eta d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun lcontr*-eta-defaults (d1 d2 a lnorm-eta d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (lcontr*-eta-mv0 lcontr*-eta-ml0))
               (unquoted maint::wffarg (a lnorm-eta)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (lcontr*-eta-d1 lcontr*-eta-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (lcontr*-eta-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (lcontr*-eta-mv0 lcontr*-eta-ml0))
               (unquoted maint::wffarg (a lnorm-eta)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (lcontr*-eta-hyp-defaults d1 d2 a lnorm-eta
                           d1-hyps d2-hyps))
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
          (append '(nil nil) (mapcar #'specified-p (list a lnorm-eta))
                  maint::strong-hypdefaults))
    (list d1 d2 a lnorm-eta d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun lcontr*-eta-hyp-defaults (d1 d2 a lnorm-eta d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a lnorm-eta))
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


(defun lcontr*-eta-match (maint::plan-support)
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
                                           'lcontr*-eta-d1
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


(defun lcontr*-eta-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'lcontr*-eta-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun lcontr*-eta-short (d1)
  (funcall #'comdecode
           (append (list 'lcontr*-eta) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun lcontr*-eta-legal (d1 d2 a lnorm-eta d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (lcontr*-eta-legal-hyps d1 d2 a lnorm-eta d1-hyps d2-hyps)
    (lcontr*-eta-legal-wffs d1 d2 a lnorm-eta d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun lcontr*-eta-legal-hyps (d1 d2 a lnorm-eta d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a lnorm-eta))
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


(defun lcontr*-eta-legal-wffs (d1 d2 a lnorm-eta d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (lcontr*-eta-mv0 lcontr*-eta-ml0))
               (unquoted maint::wffarg (a lnorm-eta)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (lcontr*-eta-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (lcontr*-eta-d1 lcontr*-eta-d2))
               (unquoted maint::linearg (d1 d2)))
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
;;; Rule: LEXPD*-BETA
;;;

;;; The rule command definition.

(defmexpr lexpd*-beta
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a lnorm-beta p2-hyps p1-hyps)
  (arghelp "Line not in beta-normal Form" "Line in beta-normal Form" "Wff not in beta-normal Form" "" "Hypotheses" "Hypotheses")
  (defaultfns lexpd*-beta-defaults)
  (mainfns lexpd*-beta-legal lexpd*-beta-build)
  (enterfns lexpd*-beta-enter)
  (mhelp "Rule to put a planned line into beta-normal form."))

;;; The line assertions justifications and restrictions

(defrulewffs lexpd*-beta
  (unique-lines 
     (lexpd*-beta-p2 "A(O)")
     (lexpd*-beta-p1 "`(LNORM-BETA  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule lexpd*-beta
  (matchfn lexpd*-beta-match)
  (match1fn lexpd*-beta-match1)
  (shortfn lexpd*-beta-short))


;;; The building function. 


(defun lexpd*-beta-build (p2 p1 a lnorm-beta p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (lexpd*-beta-mv0 lexpd*-beta-ml0))
               (unquoted maint::wffarg (a lnorm-beta)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (lexpd*-beta-p2 lexpd*-beta-p1))
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
                ((list "Beta rule" (mapcar #'meta-subst 'nil)
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
            (list a lnorm-beta p2-hyps p1-hyps))))

;;; The entering function. 


(defun lexpd*-beta-enter (p2 p1 a lnorm-beta p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (lexpd*-beta-legal p2 p1 a lnorm-beta p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun lexpd*-beta-defaults (p2 p1 a lnorm-beta p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (lexpd*-beta-mv0 lexpd*-beta-ml0))
               (unquoted maint::wffarg (a lnorm-beta)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (lexpd*-beta-p2 lexpd*-beta-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (lexpd*-beta-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (lexpd*-beta-mv0 lexpd*-beta-ml0))
               (unquoted maint::wffarg (a lnorm-beta)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (lexpd*-beta-hyp-defaults p2 p1 a lnorm-beta
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
          (append '(nil nil) (mapcar #'specified-p (list a lnorm-beta))
                  maint::strong-hypdefaults))
    (list p2 p1 a lnorm-beta p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun lexpd*-beta-hyp-defaults (p2 p1 a lnorm-beta p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a lnorm-beta))
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


(defun lexpd*-beta-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'lexpd*-beta-p2
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


(defun lexpd*-beta-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'lexpd*-beta-p2
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun lexpd*-beta-short (p2)
  (funcall #'comdecode
           (append (list 'lexpd*-beta) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun lexpd*-beta-legal (p2 p1 a lnorm-beta p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (lexpd*-beta-legal-hyps p2 p1 a lnorm-beta p2-hyps p1-hyps)
    (lexpd*-beta-legal-wffs p2 p1 a lnorm-beta p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun lexpd*-beta-legal-hyps (p2 p1 a lnorm-beta p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a lnorm-beta))
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


(defun lexpd*-beta-legal-wffs (p2 p1 a lnorm-beta p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (lexpd*-beta-mv0 lexpd*-beta-ml0))
               (unquoted maint::wffarg (a lnorm-beta)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (lexpd*-beta-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (lexpd*-beta-p2 lexpd*-beta-p1))
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
;;; Rule: LCONTR*-BETA
;;;

;;; The rule command definition.

(defmexpr lcontr*-beta
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a lnorm-beta d1-hyps d2-hyps)
  (arghelp "Line not in beta-normal Form" "Line in beta-normal Form" "Wff not in beta-normal Form" "" "Hypotheses" "Hypotheses")
  (defaultfns lcontr*-beta-defaults)
  (mainfns lcontr*-beta-legal lcontr*-beta-build)
  (enterfns lcontr*-beta-enter)
  (mhelp "Rule to put an inferred line into beta-normal form."))

;;; The line assertions justifications and restrictions

(defrulewffs lcontr*-beta
  (unique-lines 
     (lcontr*-beta-d1 "A(O)")
     (lcontr*-beta-d2 "`(LNORM-BETA  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule lcontr*-beta
  (matchfn lcontr*-beta-match)
  (match1fn lcontr*-beta-match1)
  (shortfn lcontr*-beta-short))


;;; The building function. 


(defun lcontr*-beta-build (d1 d2 a lnorm-beta d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (lcontr*-beta-mv0 lcontr*-beta-ml0))
               (unquoted maint::wffarg (a lnorm-beta)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (lcontr*-beta-d1 lcontr*-beta-d2))
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
                 (list "Beta rule" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list a lnorm-beta d1-hyps d2-hyps))))

;;; The entering function. 


(defun lcontr*-beta-enter (d1 d2 a lnorm-beta d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (lcontr*-beta-legal d1 d2 a lnorm-beta d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun lcontr*-beta-defaults (d1 d2 a lnorm-beta d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (lcontr*-beta-mv0 lcontr*-beta-ml0))
               (unquoted maint::wffarg (a lnorm-beta)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (lcontr*-beta-d1 lcontr*-beta-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (lcontr*-beta-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (lcontr*-beta-mv0 lcontr*-beta-ml0))
               (unquoted maint::wffarg (a lnorm-beta)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (lcontr*-beta-hyp-defaults d1 d2 a lnorm-beta
                           d1-hyps d2-hyps))
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
          (append '(nil nil) (mapcar #'specified-p (list a lnorm-beta))
                  maint::strong-hypdefaults))
    (list d1 d2 a lnorm-beta d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun lcontr*-beta-hyp-defaults (d1 d2 a lnorm-beta d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a lnorm-beta))
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


(defun lcontr*-beta-match (maint::plan-support)
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
                                           'lcontr*-beta-d1
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


(defun lcontr*-beta-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'lcontr*-beta-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun lcontr*-beta-short (d1)
  (funcall #'comdecode
           (append (list 'lcontr*-beta) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun lcontr*-beta-legal (d1 d2 a lnorm-beta d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (lcontr*-beta-legal-hyps d1 d2 a lnorm-beta d1-hyps d2-hyps)
    (lcontr*-beta-legal-wffs d1 d2 a lnorm-beta d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun lcontr*-beta-legal-hyps (d1 d2 a lnorm-beta d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a lnorm-beta))
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


(defun lcontr*-beta-legal-wffs (d1 d2 a lnorm-beta d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (lcontr*-beta-mv0 lcontr*-beta-ml0))
               (unquoted maint::wffarg (a lnorm-beta)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (lcontr*-beta-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (lcontr*-beta-d1 lcontr*-beta-d2))
               (unquoted maint::linearg (d1 d2)))
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
;;; Rule: LEXPD*
;;;

;;; The rule command definition.

(defmexpr lexpd*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a lnorm p2-hyps p1-hyps)
  (arghelp "Line not in Lambda-normal Form" "Line in Lambda-normal Form" "Wff not in Lambda-normal Form" "Wff in Lambda-normal Form" "Hypotheses" "Hypotheses")
  (defaultfns lexpd*-defaults)
  (mainfns lexpd*-legal lexpd*-build)
  (enterfns lexpd*-enter)
  (mhelp "Rule to put a planned line into Lambda-normal form using both 
beta and eta conversion."))

;;; The line assertions justifications and restrictions

(defrulewffs lexpd*
  (unique-lines 
     (lexpd*-p2 "A(O)")
     (lexpd*-p1 "`(LNORM  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule lexpd*
  (matchfn lexpd*-match)
  (match1fn lexpd*-match1)
  (shortfn lexpd*-short))


;;; The building function. 


(defun lexpd*-build (p2 p1 a lnorm p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lexpd*-mv0 lexpd*-ml0))
               (unquoted maint::wffarg (a lnorm)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (lexpd*-p2 lexpd*-p1))
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
                ((list "Lambda" (mapcar #'meta-subst 'nil)
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
    (append maint::new-line-labels (list a lnorm p2-hyps p1-hyps))))

;;; The entering function. 


(defun lexpd*-enter (p2 p1 a lnorm p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (lexpd*-legal p2 p1 a lnorm p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun lexpd*-defaults (p2 p1 a lnorm p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (lexpd*-mv0 lexpd*-ml0))
               (unquoted maint::wffarg (a lnorm)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (lexpd*-p2 lexpd*-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (lexpd*-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (lexpd*-mv0 lexpd*-ml0))
               (unquoted maint::wffarg (a lnorm)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (lexpd*-hyp-defaults p2 p1 a lnorm p2-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list a lnorm))
                  maint::strong-hypdefaults))
    (list p2 p1 a lnorm p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun lexpd*-hyp-defaults (p2 p1 a lnorm p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a lnorm))
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


(defun lexpd*-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'lexpd*-p2 'meta-assertion)
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


(defun lexpd*-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'lexpd*-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun lexpd*-short (p2)
  (funcall #'comdecode
           (append (list 'lexpd*) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun lexpd*-legal (p2 p1 a lnorm p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (lexpd*-legal-hyps p2 p1 a lnorm p2-hyps p1-hyps)
    (lexpd*-legal-wffs p2 p1 a lnorm p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun lexpd*-legal-hyps (p2 p1 a lnorm p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a lnorm))
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


(defun lexpd*-legal-wffs (p2 p1 a lnorm p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lexpd*-mv0 lexpd*-ml0))
               (unquoted maint::wffarg (a lnorm)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (lexpd*-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (lexpd*-p2 lexpd*-p1))
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
;;; Rule: LCONTR*
;;;

;;; The rule command definition.

(defmexpr lcontr*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a lnorm d1-hyps d2-hyps)
  (arghelp "Line not in Lambda-normal Form" "Line in Lambda-normal Form" "Wff not in Lambda-normal Form" "Wff in Lambda-normal Form" "Hypotheses" "Hypotheses")
  (defaultfns lcontr*-defaults)
  (mainfns lcontr*-legal lcontr*-build)
  (enterfns lcontr*-enter)
  (mhelp "Rule to put an inferred line into Lambda-normal form using both 
beta and eta conversion."))

;;; The line assertions justifications and restrictions

(defrulewffs lcontr*
  (unique-lines 
     (lcontr*-d1 "A(O)")
     (lcontr*-d2 "`(LNORM  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule lcontr*
  (matchfn lcontr*-match)
  (match1fn lcontr*-match1)
  (shortfn lcontr*-short))


;;; The building function. 


(defun lcontr*-build (d1 d2 a lnorm d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lcontr*-mv0 lcontr*-ml0))
               (unquoted maint::wffarg (a lnorm)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (lcontr*-d1 lcontr*-d2))
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
                 (list "Lambda" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a lnorm d1-hyps d2-hyps))))

;;; The entering function. 


(defun lcontr*-enter (d1 d2 a lnorm d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (lcontr*-legal d1 d2 a lnorm d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun lcontr*-defaults (d1 d2 a lnorm d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (lcontr*-mv0 lcontr*-ml0))
               (unquoted maint::wffarg (a lnorm)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (lcontr*-d1 lcontr*-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (lcontr*-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (lcontr*-mv0 lcontr*-ml0))
               (unquoted maint::wffarg (a lnorm)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (lcontr*-hyp-defaults d1 d2 a lnorm d1-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list a lnorm))
                  maint::strong-hypdefaults))
    (list d1 d2 a lnorm d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun lcontr*-hyp-defaults (d1 d2 a lnorm d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a lnorm))
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


(defun lcontr*-match (maint::plan-support)
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
                                           'lcontr*-d1
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


(defun lcontr*-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'lcontr*-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun lcontr*-short (d1)
  (funcall #'comdecode
           (append (list 'lcontr*) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun lcontr*-legal (d1 d2 a lnorm d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (lcontr*-legal-hyps d1 d2 a lnorm d1-hyps d2-hyps)
    (lcontr*-legal-wffs d1 d2 a lnorm d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun lcontr*-legal-hyps (d1 d2 a lnorm d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a lnorm))
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


(defun lcontr*-legal-wffs (d1 d2 a lnorm d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (lcontr*-mv0 lcontr*-ml0))
               (unquoted maint::wffarg (a lnorm)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (lcontr*-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (lcontr*-d1 lcontr*-d2))
               (unquoted maint::linearg (d1 d2)))
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
