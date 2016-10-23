;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-hacks.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-hacks.rules"
;;;
;;; contains rules
;;; SUBST-EQUIV IDISJ-RIGHT IDISJ-LEFT RULEC1 ASSOC-LEFT CASES4 CASES3 IMP-DISJ-R IMP-DISJ-L DISJ-IMP-R DISJ-IMP-L 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-hacks
  (contents SUBST-EQUIV IDISJ-RIGHT IDISJ-LEFT RULEC1 ASSOC-LEFT CASES4 CASES3 IMP-DISJ-R IMP-DISJ-L DISJ-IMP-R DISJ-IMP-L ))

(context rule-commands)

(context RULES-2-PROP)


;;;
;;; Rule: SUBST-EQUIV
;;;

;;; The rule command definition.

(defmexpr subst-equiv
  (argtypes line line line gwff gwff gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" "O" "O" nil nil nil) (wffop-typelist)
  (argnames d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (arghelp "Line with Equivalence" "Line after Substituting Some Occurrences" "Line before Substituting Some Occurrences" "Formula Before Substitution" "Formula After Substitution" "Right-Hand Side of Equivalence" "Left-Hand Side of Equivalence" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns subst-equiv-defaults)
  (mainfns subst-equiv-legal subst-equiv-build)
  (enterfns subst-equiv-enter)
  (mhelp "Substitution of Equivalence.  Usable when R and P are the same modulo
the equivalence s EQUIV t."))

;;; The line assertions justifications and restrictions

(defrulewffs subst-equiv
  (unique-lines 
     (subst-equiv-d2 "s(O) EQUIV t(O)")
     (subst-equiv-d3 "R(O)")
     (subst-equiv-p1 "P(O)"))
  (unique-restrictions 
     (subst-equiv-restr0 (same-modulo-equality "P(O)" "R(O)" "s(O)" "t(O)"))))

;;; The suggesting rule definition.

(defsrule subst-equiv
  (matchfn subst-equiv-match)
  (match1fn subst-equiv-match1)
  (shortfn subst-equiv-short))


;;; The building function. 


(defun subst-equiv-build (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (subst-equiv-mv3 subst-equiv-mv2 subst-equiv-mv1
                 subst-equiv-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (subst-equiv-d2 subst-equiv-d3 subst-equiv-p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (d2 d3 p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((nextplan)
                 (list "Sub-equiv" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list p1 d2)))
                 (nextplan)))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d2-hyps d3-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d2-hyps d3-hyps p1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list p r |t| |s| d2-hyps d3-hyps p1-hyps))))

;;; The entering function. 


(defun subst-equiv-enter (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (subst-equiv-legal d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps))
  (update-plan (eval-destruct (('pp d2 'ss)))
               (eval-destruct ((p1 'ss) ('pp d3 'ss p1 d2)))))

;;; The default function. 


(defun subst-equiv-defaults
    (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (subst-equiv-mv3 subst-equiv-mv2 subst-equiv-mv1
                 subst-equiv-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (subst-equiv-d2 subst-equiv-d3 subst-equiv-p1))
               (unquoted maint::linearg (d2 d3 p1)))
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
                (subst-equiv-mv3 subst-equiv-mv2 subst-equiv-mv1
                 subst-equiv-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d2-hyps d3-hyps p1-hyps))
      (if (not (member '$ (list d2 d3 p1)))
          (setq-destruct (d2-hyps d3-hyps p1-hyps)
                         (subst-equiv-hyp-defaults d2 d3 p1 p r |t| |s|
                          d2-hyps d3-hyps p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d2-hyps d3-hyps p1-hyps)))))
    (setq-destruct (('pp d2 'ss))
                   (line-no-defaults-from
                     (eval-destruct (('pp d2 'ss)))))
    (when (not (member '$ (list 'pp d2 'ss)))
      (setq-destruct-multi (p1) ((p1 'ss) ('pp d3 'ss p1 d2))
                           (line-no-defaults-to (eval-destruct
                                                 (('pp d2 'ss)))
                                                (eval-destruct
                                                 ((p1 'ss)
                                                  ('pp
                                                   d3
                                                   'ss
                                                   p1
                                                   d2))))))
    (setq strong-defaultlist
          (append '(nil nil nil)
                  (mapcar #'specified-p (list p r |t| |s|))
                  maint::strong-hypdefaults))
    (list d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun subst-equiv-hyp-defaults
    (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore p r |t| |s|))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d2 d3 p1))
               (unquoted maint::hyparg (d2-hyps d3-hyps p1-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
                                (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p d2-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d2-hyps (list)))))
    (when (specified-p d3-hyps)
      (setq maint::hupper (meet-h maint::hupper d3-hyps)))
    (when (specified-p p1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p1-hyps (list)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d2-hyps d3-hyps p1-hyps)))
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
                             (list d2-hyps d3-hyps p1-hyps))))
             (when (not (specified-p d2-hyps))
               (setq d2-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p d3-hyps))
               (setq d3-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p p1-hyps))
               (setq p1-hyps (ordered-join-h maint::hupper (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list d2-hyps d3-hyps p1-hyps))))))
    (list d2-hyps d3-hyps p1-hyps)))

;;; The matching function. 


(defun subst-equiv-match (maint::plan-support)
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
                                           'subst-equiv-d2
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (subst-equiv-restr0)))
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


(defun subst-equiv-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'subst-equiv-d2
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'subst-equiv-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun subst-equiv-short (d2 p r)
  (funcall #'comdecode
           (append (list 'subst-equiv) (list (linealias d2)) (list '$)
                   (list '$)
                   (append (list (append (list 'quote) (list p) 'nil)))
                   (append (list (append (list 'quote) (list r) 'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun subst-equiv-legal (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (subst-equiv-legal-hyps d2 d3 p1 p r |t| |s| d2-hyps d3-hyps
     p1-hyps)
    (subst-equiv-legal-wffs d2 d3 p1 p r |t| |s| d2-hyps d3-hyps
     p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun subst-equiv-legal-hyps
    (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore p r |t| |s|))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d2 d3 p1))
               (unquoted maint::hyparg (d2-hyps d3-hyps p1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d2-hyps (list))))
    (setq maint::hupper (meet-h maint::hupper d3-hyps))
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


(defun subst-equiv-legal-wffs
    (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore d2-hyps d3-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (subst-equiv-mv3 subst-equiv-mv2 subst-equiv-mv1
                 subst-equiv-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (subst-equiv-d2 subst-equiv-d3 subst-equiv-p1))
               (unquoted maint::linearg (d2 d3 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (subst-equiv-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: IDISJ-RIGHT
;;;

;;; The rule command definition.

(defmexpr idisj-right
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a b p2-hyps p1-hyps)
  (arghelp "Line containing disjunction" "Line without disjunction" "" "Wff to left of disjunction" "Hypotheses" "Hypotheses")
  (defaultfns idisj-right-defaults)
  (mainfns idisj-right-legal idisj-right-build)
  (enterfns idisj-right-enter)
  (mhelp "Introduce a disjunction (right version)."))

;;; The line assertions justifications and restrictions

(defrulewffs idisj-right
  (unique-lines 
     (idisj-right-p2 "B(O) OR A(O)")
     (idisj-right-p1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule idisj-right
  (matchfn idisj-right-match)
  (match1fn idisj-right-match1)
  (shortfn idisj-right-short))


;;; The building function. 


(defun idisj-right-build (p2 p1 a b p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (idisj-right-mv1 idisj-right-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (idisj-right-p2 idisj-right-p1))
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
                ((list "Idisj-R" (mapcar #'meta-subst 'nil)
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
    (append maint::new-line-labels (list a b p2-hyps p1-hyps))))

;;; The entering function. 


(defun idisj-right-enter (p2 p1 a b p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (idisj-right-legal p2 p1 a b p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun idisj-right-defaults (p2 p1 a b p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (idisj-right-mv1 idisj-right-mv0))
               (unquoted maint::wffarg (a b)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (idisj-right-p2 idisj-right-p1))
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
    (macro-do ((quoted maint::metavar
                (idisj-right-mv1 idisj-right-mv0))
               (unquoted maint::wffarg (a b)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (idisj-right-hyp-defaults p2 p1 a b p2-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list a b))
                  maint::strong-hypdefaults))
    (list p2 p1 a b p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun idisj-right-hyp-defaults (p2 p1 a b p2-hyps p1-hyps)
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


(defun idisj-right-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'idisj-right-p2
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


(defun idisj-right-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'idisj-right-p2
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun idisj-right-short (p2)
  (funcall #'comdecode
           (append (list 'idisj-right) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun idisj-right-legal (p2 p1 a b p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (idisj-right-legal-hyps p2 p1 a b p2-hyps p1-hyps)
    (idisj-right-legal-wffs p2 p1 a b p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun idisj-right-legal-hyps (p2 p1 a b p2-hyps p1-hyps)
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


(defun idisj-right-legal-wffs (p2 p1 a b p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (idisj-right-mv1 idisj-right-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (idisj-right-p2 idisj-right-p1))
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
;;; Rule: IDISJ-LEFT
;;;

;;; The rule command definition.

(defmexpr idisj-left
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 b a p2-hyps p1-hyps)
  (arghelp "Line containing disjunction" "Line without disjunction" "Wff to right of disjunction" "" "Hypotheses" "Hypotheses")
  (defaultfns idisj-left-defaults)
  (mainfns idisj-left-legal idisj-left-build)
  (enterfns idisj-left-enter)
  (mhelp "Introduce a disjunction (left version)."))

;;; The line assertions justifications and restrictions

(defrulewffs idisj-left
  (unique-lines 
     (idisj-left-p2 "A(O) OR B(O)")
     (idisj-left-p1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule idisj-left
  (matchfn idisj-left-match)
  (match1fn idisj-left-match1)
  (shortfn idisj-left-short))


;;; The building function. 


(defun idisj-left-build (p2 p1 b a p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (idisj-left-mv1 idisj-left-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (idisj-left-p2 idisj-left-p1))
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
                ((list "Idisj-L" (mapcar #'meta-subst 'nil)
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
    (append maint::new-line-labels (list b a p2-hyps p1-hyps))))

;;; The entering function. 


(defun idisj-left-enter (p2 p1 b a p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (idisj-left-legal p2 p1 b a p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun idisj-left-defaults (p2 p1 b a p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (idisj-left-mv1 idisj-left-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (idisj-left-p2 idisj-left-p1))
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
    (macro-do ((quoted maint::metavar (idisj-left-mv1 idisj-left-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (idisj-left-hyp-defaults p2 p1 b a p2-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list p2 p1 b a p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun idisj-left-hyp-defaults (p2 p1 b a p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
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


(defun idisj-left-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'idisj-left-p2
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


(defun idisj-left-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'idisj-left-p2
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun idisj-left-short (p2)
  (funcall #'comdecode
           (append (list 'idisj-left) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun idisj-left-legal (p2 p1 b a p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (idisj-left-legal-hyps p2 p1 b a p2-hyps p1-hyps)
    (idisj-left-legal-wffs p2 p1 b a p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun idisj-left-legal-hyps (p2 p1 b a p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun idisj-left-legal-wffs (p2 p1 b a p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (idisj-left-mv1 idisj-left-mv0))
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
                (idisj-left-p2 idisj-left-p1))
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

(context RULES-4-QUANT)


;;;
;;; Rule: RULEC1
;;;

;;; The rule command definition.

(defmexpr rulec1
  (argtypes line line line line gwff gwff gwff linelist linelist linelist linelist)
  (wffargtypes  nil nil nil nil "O" "A" "O" nil nil nil nil) (wffop-typelist "A")
  (argnames p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)
  (arghelp "Conclusion without Additional Hypothesis" "Existentially Quantified Line" "Conclusion with Additional Hypothesis" "Hypothesis with Chosen Variable" "Scope of Existential Quantifier" "Existentially Quantified Variable" "Conclusion to be Proven Using Existentially Quantified Line" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns rulec1-defaults)
  (mainfns rulec1-legal rulec1-build)
  (enterfns rulec1-enter)
  (mhelp "RuleC1 -- the special case of RULEC where the chosen
variable has the same name as the bound variable."))

;;; The line assertions justifications and restrictions

(defrulewffs rulec1
  (unique-lines 
     (rulec1-p4 "A(O)")
     (rulec1-d1 "EXISTS x(A) B(O)")
     (rulec1-d3 "A(O)")
     (rulec1-h2 "B(O)"))
  (unique-restrictions 
     (rulec1-restr0 (not-free-in-hyps "x(A)"))
     (rulec1-restr1 (is-variable "x(A)"))
     (rulec1-restr2 (not-free-in "x(A)" "A(O)"))))

;;; The suggesting rule definition.

(defsrule rulec1
  (matchfn rulec1-match)
  (match1fn rulec1-match1)
  (shortfn rulec1-short))


;;; The building function. 


(defun rulec1-build
    (p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)
  (let ((maint::new-line-labels (line-label-vec 4))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (rulec1-mv2 rulec1-mv1 rulec1-mv0))
               (unquoted maint::wffarg (b |x| a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (rulec1-p4 rulec1-d1 rulec1-d3 rulec1-h2))
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
                 (list "Choose" (mapcar #'meta-subst '(rulec1-mv1))
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
            (list b |x| a p4-hyps d1-hyps d3-hyps h2-hyps))))

;;; The entering function. 


(defun rulec1-enter
    (p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (rulec1-legal p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps))
  (update-plan (eval-destruct ((p4 d1 'ss)))
               (eval-destruct ((d3 h2 'ss)))))

;;; The default function. 


(defun rulec1-defaults
    (p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (rulec1-mv2 rulec1-mv1 rulec1-mv0))
               (unquoted maint::wffarg (b |x| a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (rulec1-p4 rulec1-d1 rulec1-d3 rulec1-h2))
               (unquoted maint::linearg (p4 d1 d3 h2)))
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
                (rulec1-mv2 rulec1-mv1 rulec1-mv0))
               (unquoted maint::wffarg (b |x| a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p4-hyps d1-hyps d3-hyps h2-hyps))
      (if (not (member '$ (list p4 d1 d3 h2)))
          (setq-destruct (p4-hyps d1-hyps d3-hyps h2-hyps)
                         (rulec1-hyp-defaults p4 d1 d3 h2 b |x| a
                          p4-hyps d1-hyps d3-hyps h2-hyps))
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
                  (mapcar #'specified-p (list b |x| a))
                  maint::strong-hypdefaults))
    (list p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)))

;;; The hypotheses default function. 


(defun rulec1-hyp-defaults
    (p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b |x| a))
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


(defun rulec1-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'rulec1-p4 'meta-assertion)
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
                                           'rulec1-d1
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (rulec1-restr0
                                           rulec1-restr1
                                           rulec1-restr2)))
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


(defun rulec1-match1 (pline maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'rulec1-p4 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'rulec1-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'rulec1-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'rulec1-restr1 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'rulec1-restr2 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun rulec1-short (p4 d1)
  (funcall #'comdecode
           (append (list 'rulec1) (list (linealias p4))
                   (list (linealias d1)) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$))))

;;; The checking function. 


(defun rulec1-legal
    (p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (rulec1-legal-hyps p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps
     h2-hyps)
    (rulec1-legal-wffs p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps
     h2-hyps)
    t))

;;; The hypotheses checking function. 


(defun rulec1-legal-hyps
    (p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b |x| a))
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


(defun rulec1-legal-wffs
    (p4 d1 d3 h2 b |x| a p4-hyps d1-hyps d3-hyps h2-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p4-hyps d1-hyps d3-hyps h2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (rulec1-mv2 rulec1-mv1 rulec1-mv0))
               (unquoted maint::wffarg (b |x| a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (rulec1-p4 rulec1-d1 rulec1-d3 rulec1-h2))
               (unquoted maint::linearg (p4 d1 d3 h2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr
                (rulec1-restr0 rulec1-restr1 rulec1-restr2)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))

(context RULES-2-PROP)


;;;
;;; Rule: ASSOC-LEFT
;;;

;;; The rule command definition.

(defmexpr assoc-left
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 p assoc-l d1-hyps d2-hyps)
  (arghelp "Line to reassociate to the left" "Line after reassociation." "Wff of original line" "Wff of reassociated line" "Hypotheses" "Hypotheses")
  (defaultfns assoc-left-defaults)
  (mainfns assoc-left-legal assoc-left-build)
  (enterfns assoc-left-enter)
  (mhelp "Rule to associate a support line leftwards. Use before
calling CASES3 or CASES4."))

;;; The line assertions justifications and restrictions

(defrulewffs assoc-left
  (unique-lines 
     (assoc-left-d1 "P(O)")
     (assoc-left-d2 "`(ASSOC-L  P(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule assoc-left
  (matchfn assoc-left-match)
  (match1fn assoc-left-match1)
  (shortfn assoc-left-short))


;;; The building function. 


(defun assoc-left-build (d1 d2 p assoc-l d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (assoc-left-mv0 assoc-left-ml0))
               (unquoted maint::wffarg (p assoc-l)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (assoc-left-d1 assoc-left-d2))
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
                 (list "Assoc" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list p assoc-l d1-hyps d2-hyps))))

;;; The entering function. 


(defun assoc-left-enter (d1 d2 p assoc-l d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (assoc-left-legal d1 d2 p assoc-l d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun assoc-left-defaults (d1 d2 p assoc-l d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (assoc-left-mv0 assoc-left-ml0))
               (unquoted maint::wffarg (p assoc-l)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (assoc-left-d1 assoc-left-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (assoc-left-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (assoc-left-mv0 assoc-left-ml0))
               (unquoted maint::wffarg (p assoc-l)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (assoc-left-hyp-defaults d1 d2 p assoc-l
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
          (append '(nil nil) (mapcar #'specified-p (list p assoc-l))
                  maint::strong-hypdefaults))
    (list d1 d2 p assoc-l d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun assoc-left-hyp-defaults (d1 d2 p assoc-l d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore p assoc-l))
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


(defun assoc-left-match (maint::plan-support)
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
                                           'assoc-left-d1
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


(defun assoc-left-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'assoc-left-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun assoc-left-short (d1)
  (funcall #'comdecode
           (append (list 'assoc-left) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun assoc-left-legal (d1 d2 p assoc-l d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (assoc-left-legal-hyps d1 d2 p assoc-l d1-hyps d2-hyps)
    (assoc-left-legal-wffs d1 d2 p assoc-l d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun assoc-left-legal-hyps (d1 d2 p assoc-l d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore p assoc-l))
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


(defun assoc-left-legal-wffs (d1 d2 p assoc-l d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (assoc-left-mv0 assoc-left-ml0))
               (unquoted maint::wffarg (p assoc-l)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (assoc-left-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (assoc-left-d1 assoc-left-d2))
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
;;; Rule: CASES4
;;;

;;; The rule command definition.

(defmexpr cases4
  (argtypes line line line line line line line line line line gwff gwff gwff gwff gwff linelist linelist linelist linelist linelist linelist linelist linelist linelist linelist)
  (wffargtypes  nil nil nil nil nil nil nil nil nil nil "O" "O" "O" "O" "O" nil nil nil nil nil nil nil nil nil nil) (wffop-typelist)
  (argnames p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (arghelp "Conclusion for All Four Cases" "Line with Disjunction" "Conclusion in Case 4" "Line with Assumption for Case 4" "Conclusion in Case 3" "Line with Assumption for Case 3" "Conclusion in Case 2" "Line with Assumption for Case 2" "Conclusion in Case 1" "Line with Assumption for Case 1" "Disjunct Four" "Disjunct Three" "Disjunct Two" "Disjunct One" "Conclusion" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns cases4-defaults)
  (mainfns cases4-legal cases4-build)
  (enterfns cases4-enter)
  (mhelp "Rule of Cases."))

;;; The line assertions justifications and restrictions

(defrulewffs cases4
  (unique-lines 
     (cases4-p10 "E(O)")
     (cases4-d1 "A(O) OR B(O) OR C(O) OR D(O)")
     (cases4-p9 "E(O)")
     (cases4-h8 "D(O)")
     (cases4-p7 "E(O)")
     (cases4-h6 "C(O)")
     (cases4-p5 "E(O)")
     (cases4-h4 "B(O)")
     (cases4-p3 "E(O)")
     (cases4-h2 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule cases4
  (matchfn cases4-match)
  (match1fn cases4-match1)
  (shortfn cases4-short))


;;; The building function. 


(defun cases4-build
    (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps p9-hyps
     h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (let ((maint::new-line-labels (line-label-vec 10))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (cases4-mv4 cases4-mv3 cases4-mv2 cases4-mv1
                 cases4-mv0))
               (unquoted maint::wffarg (d c b a e)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (cases4-p10 cases4-d1 cases4-p9 cases4-h8 cases4-p7
                 cases4-h6 cases4-p5 cases4-h4 cases4-p3 cases4-h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg
                (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Cases" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist
                                     (list d1 p3 p5 p7 p9)))
                 (nextplan) (nextplan)
                 (list "Case 4" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))
                 (nextplan)
                 (list "Case 3" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))
                 (nextplan)
                 (list "Case 2" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))
                 (nextplan)
                 (list "Case 1" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg
                (p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps
                 p5-hyps h4-hyps p3-hyps h2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg
                (p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps
                 p5-hyps h4-hyps p3-hyps h2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list d c b a e p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps
                  h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps))))

;;; The entering function. 


(defun cases4-enter
    (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps p9-hyps
     h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (cases4-legal p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps
     d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps
     h2-hyps))
  (update-plan (eval-destruct ((p10 d1 'ss)))
               (eval-destruct ((p3 h2 'ss) (p5 h4 'ss) (p7 h6 'ss)
                               (p9 h8 'ss)))))

;;; The default function. 


(defun cases4-defaults
    (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps p9-hyps
     h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (cases4-mv4 cases4-mv3 cases4-mv2 cases4-mv1
                 cases4-mv0))
               (unquoted maint::wffarg (d c b a e)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (cases4-p10 cases4-d1 cases4-p9 cases4-h8 cases4-p7
                 cases4-h6 cases4-p5 cases4-h4 cases4-p3 cases4-h2))
               (unquoted maint::linearg
                (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2)))
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
                (cases4-mv4 cases4-mv3 cases4-mv2 cases4-mv1
                 cases4-mv0))
               (unquoted maint::wffarg (d c b a e)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$
                  (list p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps
                        h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps))
      (if (not (member '$ (list p10 d1 p9 h8 p7 h6 p5 h4 p3 h2)))
          (setq-destruct (p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps
                          h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
                         (cases4-hyp-defaults p10 d1 p9 h8 p7 h6 p5 h4
                          p3 h2 d c b a e p10-hyps d1-hyps p9-hyps
                          h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps
                          p3-hyps h2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps
                            h6-hyps p5-hyps h4-hyps p3-hyps
                            h2-hyps)))))
    (setq-destruct ((p10 d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct ((p10 d1 'ss)))))
    (when (not (member '$ (list p10 d1 'ss)))
      (setq-destruct ((p3 h2 'ss) (p5 h4 'ss) (p7 h6 'ss) (p9 h8 'ss))
                     (line-no-defaults-to (eval-destruct
                                           ((p10 d1 'ss)))
                                          (eval-destruct
                                           ((p3 h2 'ss)
                                            (p5 h4 'ss)
                                            (p7 h6 'ss)
                                            (p9 h8 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil nil nil nil nil nil nil nil)
                  (mapcar #'specified-p (list d c b a e))
                  maint::strong-hypdefaults))
    (list p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps
          p9-hyps h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps
          h2-hyps)))

;;; The hypotheses default function. 


(defun cases4-hyp-defaults
    (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps p9-hyps
     h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore d c b a e))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg
                (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2))
               (unquoted maint::hyparg
                (p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps
                 p5-hyps h4-hyps p3-hyps h2-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
                                (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p10-hyps)
      (setq maint::hupper (meet-h maint::hupper p10-hyps)))
    (when (specified-p d1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d1-hyps (list)))))
    (when (specified-p p9-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p9-hyps (list h8)))))
    (when (specified-p h8-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h8-hyps (list h8)))))
    (when (specified-p p7-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p7-hyps (list h6)))))
    (when (specified-p h6-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h6-hyps (list h6)))))
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
                      (list p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps
                            h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)))
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
                             (list p10-hyps d1-hyps p9-hyps h8-hyps
                                   p7-hyps h6-hyps p5-hyps h4-hyps
                                   p3-hyps h2-hyps))))
             (when (not (specified-p p10-hyps))
               (setq p10-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p p9-hyps))
               (setq p9-hyps (ordered-join-h maint::hupper (list h8))))
             (when (not (specified-p h8-hyps))
               (setq h8-hyps (ordered-join-h maint::hupper (list h8))))
             (when (not (specified-p p7-hyps))
               (setq p7-hyps (ordered-join-h maint::hupper (list h6))))
             (when (not (specified-p h6-hyps))
               (setq h6-hyps (ordered-join-h maint::hupper (list h6))))
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
                             (list p10-hyps d1-hyps p9-hyps h8-hyps
                                   p7-hyps h6-hyps p5-hyps h4-hyps
                                   p3-hyps h2-hyps))))))
    (list p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps p5-hyps
          h4-hyps p3-hyps h2-hyps)))

;;; The matching function. 


(defun cases4-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'cases4-p10 'meta-assertion)
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
                                           'cases4-d1
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


(defun cases4-match1 (pline maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'cases4-p10
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'cases4-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun cases4-short (p10 d1)
  (funcall #'comdecode
           (append (list 'cases4) (list (linealias p10))
                   (list (linealias d1)) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun cases4-legal
    (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps p9-hyps
     h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (cases4-legal-hyps p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e
     p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps
     p3-hyps h2-hyps)
    (cases4-legal-wffs p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e
     p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps
     p3-hyps h2-hyps)
    t))

;;; The hypotheses checking function. 


(defun cases4-legal-hyps
    (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps p9-hyps
     h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore d c b a e))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg
                (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2))
               (unquoted maint::hyparg
                (p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps
                 p5-hyps h4-hyps p3-hyps h2-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p10-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p9-hyps (list h8))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h8-hyps h8)))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p7-hyps (list h6))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h6-hyps h6)))
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


(defun cases4-legal-wffs
    (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2 d c b a e p10-hyps d1-hyps p9-hyps
     h8-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p10-hyps d1-hyps p9-hyps h8-hyps p7-hyps h6-hyps
            p5-hyps h4-hyps p3-hyps h2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (cases4-mv4 cases4-mv3 cases4-mv2 cases4-mv1
                 cases4-mv0))
               (unquoted maint::wffarg (d c b a e)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (cases4-p10 cases4-d1 cases4-p9 cases4-h8 cases4-p7
                 cases4-h6 cases4-p5 cases4-h4 cases4-p3 cases4-h2))
               (unquoted maint::linearg
                (p10 d1 p9 h8 p7 h6 p5 h4 p3 h2)))
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
;;; Rule: CASES3
;;;

;;; The rule command definition.

(defmexpr cases3
  (argtypes line line line line line line line line gwff gwff gwff gwff linelist linelist linelist linelist linelist linelist linelist linelist)
  (wffargtypes  nil nil nil nil nil nil nil nil "O" "O" "O" "O" nil nil nil nil nil nil nil nil) (wffop-typelist)
  (argnames p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
  (arghelp "Conclusion for All Three Cases" "Line with Disjunction" "Conclusion in Case 3" "Line with Assumption for Case 3 (Right Disjunct)" "Conclusion in Case 2" "Line with Assumption for Case 2 (Middle Disjunct)" "Conclusion in Case 1" "Line with Assumption for Case 1 (Left Disjunct)" "Right Disjunct" "Middle Disjunct" "Left Disjunct" "Conclusion" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns cases3-defaults)
  (mainfns cases3-legal cases3-build)
  (enterfns cases3-enter)
  (mhelp "Rule of Cases."))

;;; The line assertions justifications and restrictions

(defrulewffs cases3
  (unique-lines 
     (cases3-p8 "D(O)")
     (cases3-d1 "A(O) OR B(O) OR C(O)")
     (cases3-p7 "D(O)")
     (cases3-h6 "C(O)")
     (cases3-p5 "D(O)")
     (cases3-h4 "B(O)")
     (cases3-p3 "D(O)")
     (cases3-h2 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule cases3
  (matchfn cases3-match)
  (match1fn cases3-match1)
  (shortfn cases3-short))


;;; The building function. 


(defun cases3-build
    (p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps h6-hyps
     p5-hyps h4-hyps p3-hyps h2-hyps)
  (let ((maint::new-line-labels (line-label-vec 8))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (cases3-mv3 cases3-mv2 cases3-mv1 cases3-mv0))
               (unquoted maint::wffarg (c b a d)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (cases3-p8 cases3-d1 cases3-p7 cases3-h6 cases3-p5
                 cases3-h4 cases3-p3 cases3-h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p8 d1 p7 h6 p5 h4 p3 h2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Cases" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist
                                     (list d1 p3 p5 p7)))
                 (nextplan) (nextplan)
                 (list "Case 3" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))
                 (nextplan)
                 (list "Case 2" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))
                 (nextplan)
                 (list "Case 1" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg
                (p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps h4-hyps
                 p3-hyps h2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg
                (p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps h4-hyps
                 p3-hyps h2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels
            (list c b a d p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps
                  h4-hyps p3-hyps h2-hyps))))

;;; The entering function. 


(defun cases3-enter
    (p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps h6-hyps
     p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (cases3-legal p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps
     p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps))
  (update-plan (eval-destruct ((p8 d1 'ss)))
               (eval-destruct ((p3 h2 'ss) (p5 h4 'ss) (p7 h6 'ss)))))

;;; The default function. 


(defun cases3-defaults
    (p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps h6-hyps
     p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (cases3-mv3 cases3-mv2 cases3-mv1 cases3-mv0))
               (unquoted maint::wffarg (c b a d)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (cases3-p8 cases3-d1 cases3-p7 cases3-h6 cases3-p5
                 cases3-h4 cases3-p3 cases3-h2))
               (unquoted maint::linearg (p8 d1 p7 h6 p5 h4 p3 h2)))
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
                (cases3-mv3 cases3-mv2 cases3-mv1 cases3-mv0))
               (unquoted maint::wffarg (c b a d)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$
                  (list p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps h4-hyps
                        p3-hyps h2-hyps))
      (if (not (member '$ (list p8 d1 p7 h6 p5 h4 p3 h2)))
          (setq-destruct (p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps
                          h4-hyps p3-hyps h2-hyps)
                         (cases3-hyp-defaults p8 d1 p7 h6 p5 h4 p3 h2 c
                          b a d p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps
                          h4-hyps p3-hyps h2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p
                      (list p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps
                            h4-hyps p3-hyps h2-hyps)))))
    (setq-destruct ((p8 d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct ((p8 d1 'ss)))))
    (when (not (member '$ (list p8 d1 'ss)))
      (setq-destruct ((p3 h2 'ss) (p5 h4 'ss) (p7 h6 'ss))
                     (line-no-defaults-to (eval-destruct ((p8 d1 'ss)))
                                          (eval-destruct
                                           ((p3 h2 'ss)
                                            (p5 h4 'ss)
                                            (p7 h6 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil nil nil nil nil nil)
                  (mapcar #'specified-p (list c b a d))
                  maint::strong-hypdefaults))
    (list p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps
          h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)))

;;; The hypotheses default function. 


(defun cases3-hyp-defaults
    (p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps h6-hyps
     p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore c b a d))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p8 d1 p7 h6 p5 h4 p3 h2))
               (unquoted maint::hyparg
                (p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps h4-hyps
                 p3-hyps h2-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
                                (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p8-hyps)
      (setq maint::hupper (meet-h maint::hupper p8-hyps)))
    (when (specified-p d1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d1-hyps (list)))))
    (when (specified-p p7-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p7-hyps (list h6)))))
    (when (specified-p h6-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h6-hyps (list h6)))))
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
                      (list p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps
                            h4-hyps p3-hyps h2-hyps)))
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
                             (list p8-hyps d1-hyps p7-hyps h6-hyps
                                   p5-hyps h4-hyps p3-hyps h2-hyps))))
             (when (not (specified-p p8-hyps))
               (setq p8-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p p7-hyps))
               (setq p7-hyps (ordered-join-h maint::hupper (list h6))))
             (when (not (specified-p h6-hyps))
               (setq h6-hyps (ordered-join-h maint::hupper (list h6))))
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
                             (list p8-hyps d1-hyps p7-hyps h6-hyps
                                   p5-hyps h4-hyps p3-hyps
                                   h2-hyps))))))
    (list p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps
          h2-hyps)))

;;; The matching function. 


(defun cases3-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'cases3-p8 'meta-assertion)
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
                                           'cases3-d1
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


(defun cases3-match1 (pline maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'cases3-p8 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'cases3-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun cases3-short (p8 d1)
  (funcall #'comdecode
           (append (list 'cases3) (list (linealias p8))
                   (list (linealias d1)) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun cases3-legal
    (p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps h6-hyps
     p5-hyps h4-hyps p3-hyps h2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (cases3-legal-hyps p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps
     p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
    (cases3-legal-wffs p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps
     p7-hyps h6-hyps p5-hyps h4-hyps p3-hyps h2-hyps)
    t))

;;; The hypotheses checking function. 


(defun cases3-legal-hyps
    (p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps h6-hyps
     p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore c b a d))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p8 d1 p7 h6 p5 h4 p3 h2))
               (unquoted maint::hyparg
                (p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps h4-hyps
                 p3-hyps h2-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p8-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hlower
          (join-h maint::hlower (set-difference p7-hyps (list h6))))
    (setq maint::hlower (join-h maint::hlower (hypsetdiff h6-hyps h6)))
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


(defun cases3-legal-wffs
    (p8 d1 p7 h6 p5 h4 p3 h2 c b a d p8-hyps d1-hyps p7-hyps h6-hyps
     p5-hyps h4-hyps p3-hyps h2-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p8-hyps d1-hyps p7-hyps h6-hyps p5-hyps h4-hyps
            p3-hyps h2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (cases3-mv3 cases3-mv2 cases3-mv1 cases3-mv0))
               (unquoted maint::wffarg (c b a d)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (cases3-p8 cases3-d1 cases3-p7 cases3-h6 cases3-p5
                 cases3-h4 cases3-p3 cases3-h2))
               (unquoted maint::linearg (p8 d1 p7 h6 p5 h4 p3 h2)))
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
;;; Rule: IMP-DISJ-R
;;;

;;; The rule command definition.

(defmexpr imp-disj-r
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a b d1-hyps d2-hyps)
  (arghelp "Line with Implication" "Line with Disjunction" "Antecedent of Implication" "Succedent of Implication" "Hypotheses" "Hypotheses")
  (defaultfns imp-disj-r-defaults)
  (mainfns imp-disj-r-legal imp-disj-r-build)
  (enterfns imp-disj-r-enter)
  (mhelp "Rule to replace an implication by a disjunction."))

;;; The line assertions justifications and restrictions

(defrulewffs imp-disj-r
  (unique-lines 
     (imp-disj-r-d1 "~B(O) IMPLIES A(O)")
     (imp-disj-r-d2 "A(O) OR B(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule imp-disj-r
  (matchfn imp-disj-r-match)
  (match1fn imp-disj-r-match1)
  (shortfn imp-disj-r-short))


;;; The building function. 


(defun imp-disj-r-build (d1 d2 a b d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (imp-disj-r-mv1 imp-disj-r-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (imp-disj-r-d1 imp-disj-r-d2))
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
                 (list "Imp-Disj-R" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a b d1-hyps d2-hyps))))

;;; The entering function. 


(defun imp-disj-r-enter (d1 d2 a b d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (imp-disj-r-legal d1 d2 a b d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun imp-disj-r-defaults (d1 d2 a b d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (imp-disj-r-mv1 imp-disj-r-mv0))
               (unquoted maint::wffarg (a b)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (imp-disj-r-d1 imp-disj-r-d2))
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
    (macro-do ((quoted maint::metavar (imp-disj-r-mv1 imp-disj-r-mv0))
               (unquoted maint::wffarg (a b)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (imp-disj-r-hyp-defaults d1 d2 a b d1-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list a b))
                  maint::strong-hypdefaults))
    (list d1 d2 a b d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun imp-disj-r-hyp-defaults (d1 d2 a b d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a b))
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


(defun imp-disj-r-match (maint::plan-support)
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
                                           'imp-disj-r-d1
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


(defun imp-disj-r-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'imp-disj-r-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun imp-disj-r-short (d1)
  (funcall #'comdecode
           (append (list 'imp-disj-r) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun imp-disj-r-legal (d1 d2 a b d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (imp-disj-r-legal-hyps d1 d2 a b d1-hyps d2-hyps)
    (imp-disj-r-legal-wffs d1 d2 a b d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun imp-disj-r-legal-hyps (d1 d2 a b d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a b))
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


(defun imp-disj-r-legal-wffs (d1 d2 a b d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (imp-disj-r-mv1 imp-disj-r-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (imp-disj-r-d1 imp-disj-r-d2))
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
;;; Rule: IMP-DISJ-L
;;;

;;; The rule command definition.

(defmexpr imp-disj-l
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Line with Implication" "Line with Disjunction" "Succedent of Implication" "Antecedent of Implication" "Hypotheses" "Hypotheses")
  (defaultfns imp-disj-l-defaults)
  (mainfns imp-disj-l-legal imp-disj-l-build)
  (enterfns imp-disj-l-enter)
  (mhelp "Rule to replace an implication by a disjunction."))

;;; The line assertions justifications and restrictions

(defrulewffs imp-disj-l
  (unique-lines 
     (imp-disj-l-d1 "~A(O) IMPLIES B(O)")
     (imp-disj-l-d2 "A(O) OR B(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule imp-disj-l
  (matchfn imp-disj-l-match)
  (match1fn imp-disj-l-match1)
  (shortfn imp-disj-l-short))


;;; The building function. 


(defun imp-disj-l-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (imp-disj-l-mv1 imp-disj-l-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (imp-disj-l-d1 imp-disj-l-d2))
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
                 (list "Imp-Disj-L" (mapcar #'meta-subst 'nil)
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


(defun imp-disj-l-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (imp-disj-l-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun imp-disj-l-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (imp-disj-l-mv1 imp-disj-l-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (imp-disj-l-d1 imp-disj-l-d2))
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
    (macro-do ((quoted maint::metavar (imp-disj-l-mv1 imp-disj-l-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (imp-disj-l-hyp-defaults d1 d2 b a d1-hyps
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


(defun imp-disj-l-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
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


(defun imp-disj-l-match (maint::plan-support)
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
                                           'imp-disj-l-d1
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


(defun imp-disj-l-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'imp-disj-l-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun imp-disj-l-short (d1)
  (funcall #'comdecode
           (append (list 'imp-disj-l) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun imp-disj-l-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (imp-disj-l-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (imp-disj-l-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun imp-disj-l-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
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


(defun imp-disj-l-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (imp-disj-l-mv1 imp-disj-l-mv0))
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
                (imp-disj-l-d1 imp-disj-l-d2))
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
;;; Rule: DISJ-IMP-R
;;;

;;; The rule command definition.

(defmexpr disj-imp-r
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Line with Disjunction" "Line with Implication" "Succedent of Implication" "Antecedent of Implication" "Hypotheses" "Hypotheses")
  (defaultfns disj-imp-r-defaults)
  (mainfns disj-imp-r-legal disj-imp-r-build)
  (enterfns disj-imp-r-enter)
  (mhelp "Rule to replace a disjunction by an implication."))

;;; The line assertions justifications and restrictions

(defrulewffs disj-imp-r
  (unique-lines 
     (disj-imp-r-d1 "A(O) OR B(O)")
     (disj-imp-r-d2 "~B(O) IMPLIES A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule disj-imp-r
  (matchfn disj-imp-r-match)
  (match1fn disj-imp-r-match1)
  (shortfn disj-imp-r-short))


;;; The building function. 


(defun disj-imp-r-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (disj-imp-r-mv1 disj-imp-r-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (disj-imp-r-d1 disj-imp-r-d2))
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
                 (list "Disj-Imp-R" (mapcar #'meta-subst 'nil)
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


(defun disj-imp-r-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (disj-imp-r-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun disj-imp-r-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (disj-imp-r-mv1 disj-imp-r-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (disj-imp-r-d1 disj-imp-r-d2))
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
    (macro-do ((quoted maint::metavar (disj-imp-r-mv1 disj-imp-r-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (disj-imp-r-hyp-defaults d1 d2 b a d1-hyps
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


(defun disj-imp-r-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
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


(defun disj-imp-r-match (maint::plan-support)
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
                                           'disj-imp-r-d1
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


(defun disj-imp-r-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'disj-imp-r-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun disj-imp-r-short (d1)
  (funcall #'comdecode
           (append (list 'disj-imp-r) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun disj-imp-r-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (disj-imp-r-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (disj-imp-r-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun disj-imp-r-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
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


(defun disj-imp-r-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (disj-imp-r-mv1 disj-imp-r-mv0))
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
                (disj-imp-r-d1 disj-imp-r-d2))
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
;;; Rule: DISJ-IMP-L
;;;

;;; The rule command definition.

(defmexpr disj-imp-l
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Line with Disjunction" "Line with Implication" "Succedent of Implication" "Antecedent of Implication" "Hypotheses" "Hypotheses")
  (defaultfns disj-imp-l-defaults)
  (mainfns disj-imp-l-legal disj-imp-l-build)
  (enterfns disj-imp-l-enter)
  (mhelp "Rule to replace a disjunction by an implication."))

;;; The line assertions justifications and restrictions

(defrulewffs disj-imp-l
  (unique-lines 
     (disj-imp-l-d1 "A(O) OR B(O)")
     (disj-imp-l-d2 "~A(O) IMPLIES B(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule disj-imp-l
  (matchfn disj-imp-l-match)
  (match1fn disj-imp-l-match1)
  (shortfn disj-imp-l-short))


;;; The building function. 


(defun disj-imp-l-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (disj-imp-l-mv1 disj-imp-l-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (disj-imp-l-d1 disj-imp-l-d2))
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
                 (list "Disj-Imp-L" (mapcar #'meta-subst 'nil)
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


(defun disj-imp-l-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (disj-imp-l-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun disj-imp-l-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (disj-imp-l-mv1 disj-imp-l-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (disj-imp-l-d1 disj-imp-l-d2))
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
    (macro-do ((quoted maint::metavar (disj-imp-l-mv1 disj-imp-l-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (disj-imp-l-hyp-defaults d1 d2 b a d1-hyps
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


(defun disj-imp-l-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
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


(defun disj-imp-l-match (maint::plan-support)
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
                                           'disj-imp-l-d1
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


(defun disj-imp-l-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'disj-imp-l-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun disj-imp-l-short (d1)
  (funcall #'comdecode
           (append (list 'disj-imp-l) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun disj-imp-l-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (disj-imp-l-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (disj-imp-l-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun disj-imp-l-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
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


(defun disj-imp-l-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (disj-imp-l-mv1 disj-imp-l-mv0))
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
                (disj-imp-l-d1 disj-imp-l-d2))
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
