;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-logic7b.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-logic7b.rules"
;;;
;;; contains rules
;;; SUBST= SYM= EXT=0 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-logic7b
  (contents SUBST= SYM= EXT=0 ))

(context rule-commands)

(context RULES-6-EQUALITY)


;;;
;;; Rule: SUBST=
;;;

;;; The rule command definition.

(defmexpr subst=
  (argtypes line line line gwff gwff gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" "A" "A" nil nil nil) (wffop-typelist "A")
  (argnames d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (arghelp "Line with Equality" "Line after Substituting Some Occurrences" "Line before Substituting Some Occurrences" "Formula Before Substitution" "Formula After Substitution" "Right-Hand Side of Equality" "Left-Hand Side of Equality" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns subst=-defaults)
  (mainfns subst=-legal subst=-build)
  (enterfns subst=-enter)
  (mhelp "Substitution of Equality.  Usable when R and P are the same modulo
the equality s=t."))

;;; The line assertions justifications and restrictions

(defrulewffs subst=
  (unique-lines 
     (subst=-d2 "s(A) =(OAA) t(A)")
     (subst=-d3 "R(O)")
     (subst=-p1 "P(O)"))
  (unique-restrictions 
     (subst=-restr0 (same-modulo-equality "P(O)" "R(O)" "s(A)" "t(A)"))))

;;; The suggesting rule definition.

(defsrule subst=
  (matchfn subst=-match)
  (match1fn subst=-match1)
  (shortfn subst=-short))


;;; The building function. 


(defun subst=-build (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (subst=-mv3 subst=-mv2 subst=-mv1 subst=-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (subst=-d2 subst=-d3 subst=-p1))
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
                 (list "Sub=" (mapcar #'meta-subst 'nil)
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


(defun subst=-enter (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (subst=-legal d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps))
  (update-plan (eval-destruct (('pp d2 'ss)))
               (eval-destruct ((p1 'ss) ('pp d3 'ss p1 d2)))))

;;; The default function. 


(defun subst=-defaults (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (subst=-mv3 subst=-mv2 subst=-mv1 subst=-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (subst=-d2 subst=-d3 subst=-p1))
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
                (subst=-mv3 subst=-mv2 subst=-mv1 subst=-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d2-hyps d3-hyps p1-hyps))
      (if (not (member '$ (list d2 d3 p1)))
          (setq-destruct (d2-hyps d3-hyps p1-hyps)
                         (subst=-hyp-defaults d2 d3 p1 p r |t| |s|
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


(defun subst=-hyp-defaults
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


(defun subst=-match (maint::plan-support)
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
                                           'subst=-d2
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (subst=-restr0)))
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


(defun subst=-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'subst=-d2 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'subst=-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun subst=-short (d2 p r)
  (funcall #'comdecode
           (append (list 'subst=) (list (linealias d2)) (list '$)
                   (list '$)
                   (append (list (append (list 'quote) (list p) 'nil)))
                   (append (list (append (list 'quote) (list r) 'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun subst=-legal (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (subst=-legal-hyps d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
    (subst=-legal-wffs d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun subst=-legal-hyps (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
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


(defun subst=-legal-wffs (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore d2-hyps d3-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (subst=-mv3 subst=-mv2 subst=-mv1 subst=-mv0))
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
                (subst=-d2 subst=-d3 subst=-p1))
               (unquoted maint::linearg (d2 d3 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (subst=-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: SYM=
;;;

;;; The rule command definition.

(defmexpr sym=
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "A" "A" nil nil) (wffop-typelist "A")
  (argnames p2 p1 a b p2-hyps p1-hyps)
  (arghelp "Higher Line" "Lower Line" "Left Hand Side of Lower Equality" "Right Hand Side of Lower Equality" "Hypotheses" "Hypotheses")
  (defaultfns sym=-defaults)
  (mainfns sym=-legal sym=-build)
  (enterfns sym=-enter)
  (mhelp "Rule of Symmetry of Equality."))

;;; The line assertions justifications and restrictions

(defrulewffs sym=
  (unique-lines 
     (sym=-p2 "B(A) =(OAA) A(A)")
     (sym=-p1 "A(A) =(OAA) B(A)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule sym=
  (matchfn sym=-match)
  (match1fn sym=-match1)
  (shortfn sym=-short))


;;; The building function. 


(defun sym=-build (p2 p1 a b p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (sym=-mv1 sym=-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (sym=-p2 sym=-p1))
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
                ((list "Sym=" (mapcar #'meta-subst 'nil)
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


(defun sym=-enter (p2 p1 a b p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (sym=-legal p2 p1 a b p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun sym=-defaults (p2 p1 a b p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (sym=-mv1 sym=-mv0))
               (unquoted maint::wffarg (a b)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (sym=-p2 sym=-p1))
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
    (macro-do ((quoted maint::metavar (sym=-mv1 sym=-mv0))
               (unquoted maint::wffarg (a b)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (sym=-hyp-defaults p2 p1 a b p2-hyps p1-hyps))
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


(defun sym=-hyp-defaults (p2 p1 a b p2-hyps p1-hyps)
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


(defun sym=-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'sym=-p2 'meta-assertion)
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


(defun sym=-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'sym=-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun sym=-short (p2)
  (funcall #'comdecode
           (append (list 'sym=) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun sym=-legal (p2 p1 a b p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (sym=-legal-hyps p2 p1 a b p2-hyps p1-hyps)
    (sym=-legal-wffs p2 p1 a b p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun sym=-legal-hyps (p2 p1 a b p2-hyps p1-hyps)
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


(defun sym=-legal-wffs (p2 p1 a b p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (sym=-mv1 sym=-mv0))
               (unquoted maint::wffarg (a b)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (sym=-p2 sym=-p1))
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
;;; Rule: EXT=0
;;;

;;; The rule command definition.

(defmexpr ext=0
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 r p p2-hyps p1-hyps)
  (arghelp "Line with Equality" "Line with Equivalence" "Right Equivalent" "Left Equivalent" "Hypotheses" "Hypotheses")
  (defaultfns ext=0-defaults)
  (mainfns ext=0-legal ext=0-build)
  (enterfns ext=0-enter)
  (mhelp "Rule to convert equality at type o into an equivalence."))

;;; The line assertions justifications and restrictions

(defrulewffs ext=0
  (unique-lines 
     (ext=0-p2 "P(O) =(OOO) R(O)")
     (ext=0-p1 "P(O) EQUIV R(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule ext=0
  (matchfn ext=0-match)
  (match1fn ext=0-match1)
  (shortfn ext=0-short))


;;; The building function. 


(defun ext=0-build (p2 p1 r p p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ext=0-mv1 ext=0-mv0))
               (unquoted maint::wffarg (r p)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (ext=0-p2 ext=0-p1))
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
                ((list "Ext=" (mapcar #'meta-subst 'nil)
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
    (append maint::new-line-labels (list r p p2-hyps p1-hyps))))

;;; The entering function. 


(defun ext=0-enter (p2 p1 r p p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (ext=0-legal p2 p1 r p p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun ext=0-defaults (p2 p1 r p p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (ext=0-mv1 ext=0-mv0))
               (unquoted maint::wffarg (r p)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (ext=0-p2 ext=0-p1))
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
    (macro-do ((quoted maint::metavar (ext=0-mv1 ext=0-mv0))
               (unquoted maint::wffarg (r p)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (ext=0-hyp-defaults p2 p1 r p p2-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list r p))
                  maint::strong-hypdefaults))
    (list p2 p1 r p p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun ext=0-hyp-defaults (p2 p1 r p p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore r p))
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


(defun ext=0-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'ext=0-p2 'meta-assertion)
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


(defun ext=0-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'ext=0-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun ext=0-short (p2)
  (funcall #'comdecode
           (append (list 'ext=0) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun ext=0-legal (p2 p1 r p p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (ext=0-legal-hyps p2 p1 r p p2-hyps p1-hyps)
    (ext=0-legal-wffs p2 p1 r p p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun ext=0-legal-hyps (p2 p1 r p p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore r p))
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


(defun ext=0-legal-wffs (p2 p1 r p p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ext=0-mv1 ext=0-mv0))
               (unquoted maint::wffarg (r p)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (ext=0-p2 ext=0-p1))
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
