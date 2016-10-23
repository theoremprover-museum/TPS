;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-logic7a.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-logic7a.rules"
;;;
;;; contains rules
;;; SUBST=R SUBST=L EQUIV-EQ-CONTR* EQUIV-EQ-EXPD* EQUIV-EQ-CONTR EQUIV-EQ-EXPD EQUIV-EQ EXT= 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-logic7a
  (contents SUBST=R SUBST=L EQUIV-EQ-CONTR* EQUIV-EQ-EXPD* EQUIV-EQ-CONTR EQUIV-EQ-EXPD EQUIV-EQ EXT= ))

(context rule-commands)

(context RULES-6-EQUALITY)


;;;
;;; Rule: SUBST=R
;;;

;;; The rule command definition.

(defmexpr subst=r
  (argtypes line line line gwff gwff gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" "A" "A" nil nil nil) (wffop-typelist "A")
  (argnames d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
  (arghelp "Line with Equality" "Line after Substituting Some Occurrences" "Line before Substituting Some Occurrences" "Formula Before Substitution" "Formula After Substitution" "Right-Hand Side of Equality" "Left-Hand Side of Equality" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns subst=r-defaults)
  (mainfns subst=r-legal subst=r-build)
  (enterfns subst=r-enter)
  (mhelp "Substitution of Equality.  Replaces some occurrences of the right
hand side by the left hand side."))

;;; The line assertions justifications and restrictions

(defrulewffs subst=r
  (unique-lines 
     (subst=r-d2 "t(A) =(OAA) s(A)")
     (subst=r-d3 "R(O)")
     (subst=r-p1 "P(O)"))
  (unique-restrictions 
     (subst=r-restr0 (r-prime-restr "s(A)" "P(O)" "t(A)" "R(O)"))))

;;; The suggesting rule definition.

(defsrule subst=r
  (matchfn subst=r-match)
  (match1fn subst=r-match1)
  (shortfn subst=r-short))


;;; The building function. 


(defun subst=r-build (d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (subst=r-mv3 subst=r-mv2 subst=r-mv1 subst=r-mv0))
               (unquoted maint::wffarg (p r |s| |t|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (subst=r-d2 subst=r-d3 subst=r-p1))
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
                 (list "Subst=" (mapcar #'meta-subst 'nil)
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
            (list p r |s| |t| d2-hyps d3-hyps p1-hyps))))

;;; The entering function. 


(defun subst=r-enter (d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (subst=r-legal d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps))
  (update-plan (eval-destruct (('pp d2 'ss)))
               (eval-destruct ((p1 'ss) ('pp d3 'ss p1 d2)))))

;;; The default function. 


(defun subst=r-defaults (d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (subst=r-mv3 subst=r-mv2 subst=r-mv1 subst=r-mv0))
               (unquoted maint::wffarg (p r |s| |t|)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (subst=r-d2 subst=r-d3 subst=r-p1))
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
                (subst=r-mv3 subst=r-mv2 subst=r-mv1 subst=r-mv0))
               (unquoted maint::wffarg (p r |s| |t|)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d2-hyps d3-hyps p1-hyps))
      (if (not (member '$ (list d2 d3 p1)))
          (setq-destruct (d2-hyps d3-hyps p1-hyps)
                         (subst=r-hyp-defaults d2 d3 p1 p r |s| |t|
                                               d2-hyps d3-hyps
                                               p1-hyps))
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
                  (mapcar #'specified-p (list p r |s| |t|))
                  maint::strong-hypdefaults))
    (list d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun subst=r-hyp-defaults
    (d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore p r |s| |t|))
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


(defun subst=r-match (maint::plan-support)
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
                                           'subst=r-d2
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (subst=r-restr0)))
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


(defun subst=r-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'subst=r-d2
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'subst=r-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun subst=r-short (d2 p r)
  (funcall #'comdecode
           (append (list 'subst=r) (list (linealias d2)) (list '$)
                   (list '$)
                   (append (list (append (list 'quote) (list p) 'nil)))
                   (append (list (append (list 'quote) (list r) 'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun subst=r-legal (d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (subst=r-legal-hyps d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
    (subst=r-legal-wffs d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun subst=r-legal-hyps
    (d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore p r |s| |t|))
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


(defun subst=r-legal-wffs
    (d2 d3 p1 p r |s| |t| d2-hyps d3-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore d2-hyps d3-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (subst=r-mv3 subst=r-mv2 subst=r-mv1 subst=r-mv0))
               (unquoted maint::wffarg (p r |s| |t|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (subst=r-d2 subst=r-d3 subst=r-p1))
               (unquoted maint::linearg (d2 d3 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (subst=r-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: SUBST=L
;;;

;;; The rule command definition.

(defmexpr subst=l
  (argtypes line line line gwff gwff gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" "A" "A" nil nil nil) (wffop-typelist "A")
  (argnames d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (arghelp "Line with Equality" "Line after Substituting Some Occurrences" "Line before Substituting Some Occurrences" "Formula Before Substitution" "Formula After Substitution" "Right-Hand Side of Equality" "Left-Hand Side of Equality" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns subst=l-defaults)
  (mainfns subst=l-legal subst=l-build)
  (enterfns subst=l-enter)
  (mhelp "Substitution of Equality.  Replaces some occurrences of the left hand
side by the right hand side."))

;;; The line assertions justifications and restrictions

(defrulewffs subst=l
  (unique-lines 
     (subst=l-d2 "s(A) =(OAA) t(A)")
     (subst=l-d3 "R(O)")
     (subst=l-p1 "P(O)"))
  (unique-restrictions 
     (subst=l-restr0 (r-prime-restr "s(A)" "P(O)" "t(A)" "R(O)"))))

;;; The suggesting rule definition.

(defsrule subst=l
  (matchfn subst=l-match)
  (match1fn subst=l-match1)
  (shortfn subst=l-short))


;;; The building function. 


(defun subst=l-build (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (subst=l-mv3 subst=l-mv2 subst=l-mv1 subst=l-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (subst=l-d2 subst=l-d3 subst=l-p1))
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
                 (list "Subst=" (mapcar #'meta-subst 'nil)
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


(defun subst=l-enter (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (subst=l-legal d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps))
  (update-plan (eval-destruct (('pp d2 'ss)))
               (eval-destruct ((p1 'ss) ('pp d3 'ss p1 d2)))))

;;; The default function. 


(defun subst=l-defaults (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (subst=l-mv3 subst=l-mv2 subst=l-mv1 subst=l-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (subst=l-d2 subst=l-d3 subst=l-p1))
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
                (subst=l-mv3 subst=l-mv2 subst=l-mv1 subst=l-mv0))
               (unquoted maint::wffarg (p r |t| |s|)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d2-hyps d3-hyps p1-hyps))
      (if (not (member '$ (list d2 d3 p1)))
          (setq-destruct (d2-hyps d3-hyps p1-hyps)
                         (subst=l-hyp-defaults d2 d3 p1 p r |t| |s|
                                               d2-hyps d3-hyps
                                               p1-hyps))
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


(defun subst=l-hyp-defaults
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


(defun subst=l-match (maint::plan-support)
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
                                           'subst=l-d2
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (subst=l-restr0)))
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


(defun subst=l-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'subst=l-d2
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'subst=l-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun subst=l-short (d2 p r)
  (funcall #'comdecode
           (append (list 'subst=l) (list (linealias d2)) (list '$)
                   (list '$)
                   (append (list (append (list 'quote) (list p) 'nil)))
                   (append (list (append (list 'quote) (list r) 'nil)))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun subst=l-legal (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (subst=l-legal-hyps d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
    (subst=l-legal-wffs d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun subst=l-legal-hyps
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


(defun subst=l-legal-wffs
    (d2 d3 p1 p r |t| |s| d2-hyps d3-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore d2-hyps d3-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (subst=l-mv3 subst=l-mv2 subst=l-mv1 subst=l-mv0))
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
                (subst=l-d2 subst=l-d3 subst=l-p1))
               (unquoted maint::linearg (d2 d3 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (subst=l-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: EQUIV-EQ-CONTR*
;;;

;;; The rule command definition.

(defmexpr equiv-eq-contr*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a instantiate-equalities p2-hyps p1-hyps)
  (arghelp "Line in = form" "Line in Leibniz form" "Wff with = symbols" "Expanded wff" "Hypotheses" "Hypotheses")
  (defaultfns equiv-eq-contr*-defaults)
  (mainfns equiv-eq-contr*-legal equiv-eq-contr*-build)
  (enterfns equiv-eq-contr*-enter)
  (mhelp "Rule to contract all instances of the Leibniz definition of 
equality into instances of the symbol = ."))

;;; The line assertions justifications and restrictions

(defrulewffs equiv-eq-contr*
  (unique-lines 
     (equiv-eq-contr*-p2 "A(O)")
     (equiv-eq-contr*-p1 "`(INSTANTIATE-EQUALITIES  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule equiv-eq-contr*
  (matchfn equiv-eq-contr*-match)
  (match1fn equiv-eq-contr*-match1)
  (shortfn equiv-eq-contr*-short))


;;; The building function. 


(defun equiv-eq-contr*-build
    (p2 p1 a instantiate-equalities p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-eq-contr*-mv0 equiv-eq-contr*-ml0))
               (unquoted maint::wffarg (a instantiate-equalities)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (equiv-eq-contr*-p2 equiv-eq-contr*-p1))
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
                ((list "Equiv-eq" (mapcar #'meta-subst 'nil)
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
            (list a instantiate-equalities p2-hyps p1-hyps))))

;;; The entering function. 


(defun equiv-eq-contr*-enter
    (p2 p1 a instantiate-equalities p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (equiv-eq-contr*-legal p2 p1 a instantiate-equalities p2-hyps
     p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun equiv-eq-contr*-defaults
    (p2 p1 a instantiate-equalities p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (equiv-eq-contr*-mv0 equiv-eq-contr*-ml0))
               (unquoted maint::wffarg (a instantiate-equalities)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (equiv-eq-contr*-p2 equiv-eq-contr*-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (equiv-eq-contr*-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (equiv-eq-contr*-mv0 equiv-eq-contr*-ml0))
               (unquoted maint::wffarg (a instantiate-equalities)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (equiv-eq-contr*-hyp-defaults p2 p1 a
                          instantiate-equalities p2-hyps p1-hyps))
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
                  (mapcar #'specified-p
                          (list a instantiate-equalities))
                  maint::strong-hypdefaults))
    (list p2 p1 a instantiate-equalities p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun equiv-eq-contr*-hyp-defaults
    (p2 p1 a instantiate-equalities p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults)
           (ignore a instantiate-equalities))
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


(defun equiv-eq-contr*-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'equiv-eq-contr*-p2
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


(defun equiv-eq-contr*-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'equiv-eq-contr*-p2
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun equiv-eq-contr*-short (p2)
  (funcall #'comdecode
           (append (list 'equiv-eq-contr*) (list (linealias p2))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun equiv-eq-contr*-legal
    (p2 p1 a instantiate-equalities p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (equiv-eq-contr*-legal-hyps p2 p1 a instantiate-equalities p2-hyps
     p1-hyps)
    (equiv-eq-contr*-legal-wffs p2 p1 a instantiate-equalities p2-hyps
     p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun equiv-eq-contr*-legal-hyps
    (p2 p1 a instantiate-equalities p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower)
           (ignore a instantiate-equalities))
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


(defun equiv-eq-contr*-legal-wffs
    (p2 p1 a instantiate-equalities p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-eq-contr*-mv0 equiv-eq-contr*-ml0))
               (unquoted maint::wffarg (a instantiate-equalities)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (equiv-eq-contr*-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (equiv-eq-contr*-p2 equiv-eq-contr*-p1))
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
;;; Rule: EQUIV-EQ-EXPD*
;;;

;;; The rule command definition.

(defmexpr equiv-eq-expd*
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a instantiate-equalities d1-hyps d2-hyps)
  (arghelp "Line in = form" "Line in Leibniz form" "Wff with = symbols" "Expanded wff" "Hypotheses" "Hypotheses")
  (defaultfns equiv-eq-expd*-defaults)
  (mainfns equiv-eq-expd*-legal equiv-eq-expd*-build)
  (enterfns equiv-eq-expd*-enter)
  (mhelp "Rule to expand all equalities using the Leibniz definition."))

;;; The line assertions justifications and restrictions

(defrulewffs equiv-eq-expd*
  (unique-lines 
     (equiv-eq-expd*-d1 "A(O)")
     (equiv-eq-expd*-d2 "`(INSTANTIATE-EQUALITIES  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule equiv-eq-expd*
  (matchfn equiv-eq-expd*-match)
  (match1fn equiv-eq-expd*-match1)
  (shortfn equiv-eq-expd*-short))


;;; The building function. 


(defun equiv-eq-expd*-build
    (d1 d2 a instantiate-equalities d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-eq-expd*-mv0 equiv-eq-expd*-ml0))
               (unquoted maint::wffarg (a instantiate-equalities)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (equiv-eq-expd*-d1 equiv-eq-expd*-d2))
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
                 (list "Equiv-eq" (mapcar #'meta-subst 'nil)
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
            (list a instantiate-equalities d1-hyps d2-hyps))))

;;; The entering function. 


(defun equiv-eq-expd*-enter
    (d1 d2 a instantiate-equalities d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (equiv-eq-expd*-legal d1 d2 a instantiate-equalities d1-hyps
     d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun equiv-eq-expd*-defaults
    (d1 d2 a instantiate-equalities d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (equiv-eq-expd*-mv0 equiv-eq-expd*-ml0))
               (unquoted maint::wffarg (a instantiate-equalities)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (equiv-eq-expd*-d1 equiv-eq-expd*-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (equiv-eq-expd*-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (equiv-eq-expd*-mv0 equiv-eq-expd*-ml0))
               (unquoted maint::wffarg (a instantiate-equalities)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (equiv-eq-expd*-hyp-defaults d1 d2 a
                          instantiate-equalities d1-hyps d2-hyps))
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
          (append '(nil nil)
                  (mapcar #'specified-p
                          (list a instantiate-equalities))
                  maint::strong-hypdefaults))
    (list d1 d2 a instantiate-equalities d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun equiv-eq-expd*-hyp-defaults
    (d1 d2 a instantiate-equalities d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults)
           (ignore a instantiate-equalities))
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


(defun equiv-eq-expd*-match (maint::plan-support)
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
                                           'equiv-eq-expd*-d1
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


(defun equiv-eq-expd*-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'equiv-eq-expd*-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun equiv-eq-expd*-short (d1)
  (funcall #'comdecode
           (append (list 'equiv-eq-expd*) (list (linealias d1))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun equiv-eq-expd*-legal
    (d1 d2 a instantiate-equalities d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (equiv-eq-expd*-legal-hyps d1 d2 a instantiate-equalities d1-hyps
     d2-hyps)
    (equiv-eq-expd*-legal-wffs d1 d2 a instantiate-equalities d1-hyps
     d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun equiv-eq-expd*-legal-hyps
    (d1 d2 a instantiate-equalities d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower)
           (ignore a instantiate-equalities))
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


(defun equiv-eq-expd*-legal-wffs
    (d1 d2 a instantiate-equalities d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-eq-expd*-mv0 equiv-eq-expd*-ml0))
               (unquoted maint::wffarg (a instantiate-equalities)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (equiv-eq-expd*-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (equiv-eq-expd*-d1 equiv-eq-expd*-d2))
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
;;; Rule: EQUIV-EQ-CONTR
;;;

;;; The rule command definition.

(defmexpr equiv-eq-contr
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a instantiate-top-equality p2-hyps p1-hyps)
  (arghelp "Line in = form" "Line in Leibniz form" "Wff with = symbols" "Expanded wff" "Hypotheses" "Hypotheses")
  (defaultfns equiv-eq-contr-defaults)
  (mainfns equiv-eq-contr-legal equiv-eq-contr-build)
  (enterfns equiv-eq-contr-enter)
  (mhelp "Rule to contract the outermost instance of the Leibniz definition of 
equality into instances of the symbol = ."))

;;; The line assertions justifications and restrictions

(defrulewffs equiv-eq-contr
  (unique-lines 
     (equiv-eq-contr-p2 "A(O)")
     (equiv-eq-contr-p1 "`(INSTANTIATE-TOP-EQUALITY  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule equiv-eq-contr
  (matchfn equiv-eq-contr-match)
  (match1fn equiv-eq-contr-match1)
  (shortfn equiv-eq-contr-short))


;;; The building function. 


(defun equiv-eq-contr-build
    (p2 p1 a instantiate-top-equality p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-eq-contr-mv0 equiv-eq-contr-ml0))
               (unquoted maint::wffarg (a instantiate-top-equality)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (equiv-eq-contr-p2 equiv-eq-contr-p1))
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
                ((list "Equiv-eq" (mapcar #'meta-subst 'nil)
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
            (list a instantiate-top-equality p2-hyps p1-hyps))))

;;; The entering function. 


(defun equiv-eq-contr-enter
    (p2 p1 a instantiate-top-equality p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (equiv-eq-contr-legal p2 p1 a instantiate-top-equality p2-hyps
                          p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun equiv-eq-contr-defaults
    (p2 p1 a instantiate-top-equality p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (equiv-eq-contr-mv0 equiv-eq-contr-ml0))
               (unquoted maint::wffarg (a instantiate-top-equality)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (equiv-eq-contr-p2 equiv-eq-contr-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (equiv-eq-contr-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (equiv-eq-contr-mv0 equiv-eq-contr-ml0))
               (unquoted maint::wffarg (a instantiate-top-equality)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (equiv-eq-contr-hyp-defaults p2 p1 a
                           instantiate-top-equality p2-hyps p1-hyps))
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
                  (mapcar #'specified-p
                          (list a instantiate-top-equality))
                  maint::strong-hypdefaults))
    (list p2 p1 a instantiate-top-equality p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun equiv-eq-contr-hyp-defaults
    (p2 p1 a instantiate-top-equality p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults)
           (ignore a instantiate-top-equality))
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


(defun equiv-eq-contr-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'equiv-eq-contr-p2
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


(defun equiv-eq-contr-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'equiv-eq-contr-p2
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun equiv-eq-contr-short (p2)
  (funcall #'comdecode
           (append (list 'equiv-eq-contr) (list (linealias p2))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun equiv-eq-contr-legal
    (p2 p1 a instantiate-top-equality p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (equiv-eq-contr-legal-hyps p2 p1 a instantiate-top-equality p2-hyps
      p1-hyps)
    (equiv-eq-contr-legal-wffs p2 p1 a instantiate-top-equality p2-hyps
      p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun equiv-eq-contr-legal-hyps
    (p2 p1 a instantiate-top-equality p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower)
           (ignore a instantiate-top-equality))
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


(defun equiv-eq-contr-legal-wffs
    (p2 p1 a instantiate-top-equality p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-eq-contr-mv0 equiv-eq-contr-ml0))
               (unquoted maint::wffarg (a instantiate-top-equality)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (equiv-eq-contr-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (equiv-eq-contr-p2 equiv-eq-contr-p1))
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
;;; Rule: EQUIV-EQ-EXPD
;;;

;;; The rule command definition.

(defmexpr equiv-eq-expd
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a instantiate-top-equality d1-hyps d2-hyps)
  (arghelp "Line in = form" "Line in Leibniz form" "Wff with = symbols" "Expanded wff" "Hypotheses" "Hypotheses")
  (defaultfns equiv-eq-expd-defaults)
  (mainfns equiv-eq-expd-legal equiv-eq-expd-build)
  (enterfns equiv-eq-expd-enter)
  (mhelp "Rule to expand the outermost equality using the Leibniz definition."))

;;; The line assertions justifications and restrictions

(defrulewffs equiv-eq-expd
  (unique-lines 
     (equiv-eq-expd-d1 "A(O)")
     (equiv-eq-expd-d2 "`(INSTANTIATE-TOP-EQUALITY  A(O))"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule equiv-eq-expd
  (matchfn equiv-eq-expd-match)
  (match1fn equiv-eq-expd-match1)
  (shortfn equiv-eq-expd-short))


;;; The building function. 


(defun equiv-eq-expd-build
    (d1 d2 a instantiate-top-equality d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-eq-expd-mv0 equiv-eq-expd-ml0))
               (unquoted maint::wffarg (a instantiate-top-equality)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line
                (equiv-eq-expd-d1 equiv-eq-expd-d2))
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
                 (list "Equiv-eq" (mapcar #'meta-subst 'nil)
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
            (list a instantiate-top-equality d1-hyps d2-hyps))))

;;; The entering function. 


(defun equiv-eq-expd-enter
    (d1 d2 a instantiate-top-equality d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (equiv-eq-expd-legal d1 d2 a instantiate-top-equality d1-hyps
                         d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun equiv-eq-expd-defaults
    (d1 d2 a instantiate-top-equality d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar
                (equiv-eq-expd-mv0 equiv-eq-expd-ml0))
               (unquoted maint::wffarg (a instantiate-top-equality)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (equiv-eq-expd-d1 equiv-eq-expd-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (equiv-eq-expd-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar
                (equiv-eq-expd-mv0 equiv-eq-expd-ml0))
               (unquoted maint::wffarg (a instantiate-top-equality)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (equiv-eq-expd-hyp-defaults d1 d2 a
                           instantiate-top-equality d1-hyps d2-hyps))
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
          (append '(nil nil)
                  (mapcar #'specified-p
                          (list a instantiate-top-equality))
                  maint::strong-hypdefaults))
    (list d1 d2 a instantiate-top-equality d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun equiv-eq-expd-hyp-defaults
    (d1 d2 a instantiate-top-equality d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults)
           (ignore a instantiate-top-equality))
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


(defun equiv-eq-expd-match (maint::plan-support)
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
                                           'equiv-eq-expd-d1
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


(defun equiv-eq-expd-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'equiv-eq-expd-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun equiv-eq-expd-short (d1)
  (funcall #'comdecode
           (append (list 'equiv-eq-expd) (list (linealias d1))
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun equiv-eq-expd-legal
    (d1 d2 a instantiate-top-equality d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (equiv-eq-expd-legal-hyps d1 d2 a instantiate-top-equality d1-hyps
      d2-hyps)
    (equiv-eq-expd-legal-wffs d1 d2 a instantiate-top-equality d1-hyps
      d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun equiv-eq-expd-legal-hyps
    (d1 d2 a instantiate-top-equality d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower)
           (ignore a instantiate-top-equality))
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


(defun equiv-eq-expd-legal-wffs
    (d1 d2 a instantiate-top-equality d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar
                (equiv-eq-expd-mv0 equiv-eq-expd-ml0))
               (unquoted maint::wffarg (a instantiate-top-equality)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (equiv-eq-expd-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (equiv-eq-expd-d1 equiv-eq-expd-d2))
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
;;; Rule: EQUIV-EQ
;;;

;;; The rule command definition.

(defmexpr equiv-eq
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 b a d1-hyps d2-hyps)
  (arghelp "Lower Line" "Higher Line" "Wff on Higher Line" "Wff on Lower Line" "Hypotheses" "Hypotheses")
  (defaultfns equiv-eq-defaults)
  (mainfns equiv-eq-legal equiv-eq-build)
  (enterfns equiv-eq-enter)
  (mhelp "Rule to infer a line from one which is equal up to 
definitions, lambda conversion, alphabetic change of bound variables 
and the Leibniz definition of the symbol = . You may use the editor 
command EXPAND= to create the desired line from the existing one."))

;;; The line assertions justifications and restrictions

(defrulewffs equiv-eq
  (unique-lines 
     (equiv-eq-d1 "A(O)")
     (equiv-eq-d2 "B(O)"))
  (unique-restrictions 
     (equiv-eq-restr0 (wffeq-defeq "A(O)" "B(O)"))))

;;; The suggesting rule definition.

(defsrule equiv-eq
  (matchfn equiv-eq-match)
  (match1fn equiv-eq-match1)
  (shortfn equiv-eq-short))


;;; The building function. 


(defun equiv-eq-build (d1 d2 b a d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (equiv-eq-mv1 equiv-eq-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (equiv-eq-d1 equiv-eq-d2))
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
                 (list "Equiv-eq" (mapcar #'meta-subst 'nil)
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


(defun equiv-eq-enter (d1 d2 b a d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (equiv-eq-legal d1 d2 b a d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun equiv-eq-defaults (d1 d2 b a d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (equiv-eq-mv1 equiv-eq-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (equiv-eq-d1 equiv-eq-d2))
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
    (macro-do ((quoted maint::metavar (equiv-eq-mv1 equiv-eq-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (equiv-eq-hyp-defaults d1 d2 b a d1-hyps
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


(defun equiv-eq-hyp-defaults (d1 d2 b a d1-hyps d2-hyps)
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


(defun equiv-eq-match (maint::plan-support)
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
                                           'equiv-eq-d1
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (equiv-eq-restr0)))
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


(defun equiv-eq-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'equiv-eq-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'equiv-eq-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun equiv-eq-short (d1 b)
  (funcall #'comdecode
           (append (list 'equiv-eq) (list (linealias d1)) (list '$)
                   (append (list (append (list 'quote) (list b) 'nil)))
                   (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun equiv-eq-legal (d1 d2 b a d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (equiv-eq-legal-hyps d1 d2 b a d1-hyps d2-hyps)
    (equiv-eq-legal-wffs d1 d2 b a d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun equiv-eq-legal-hyps (d1 d2 b a d1-hyps d2-hyps)
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


(defun equiv-eq-legal-wffs (d1 d2 b a d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (equiv-eq-mv1 equiv-eq-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (equiv-eq-d1 equiv-eq-d2))
               (unquoted maint::linearg (d1 d2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (equiv-eq-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: EXT=
;;;

;;; The rule command definition.

(defmexpr ext=
  (argtypes line line gwff gwff gwff linelist linelist)
  (wffargtypes  nil nil "B" "AB" "AB" nil nil) (wffop-typelist "A" "B")
  (argnames p2 p1 |x| |g| |f| p2-hyps p1-hyps)
  (arghelp "Line with Equality" "Universally Quantified Equality" "Universally Quantified Variable" "Function on Right Side of Equality" "Function on Left Side of Equality" "Hypotheses" "Hypotheses")
  (defaultfns ext=-defaults)
  (mainfns ext=-legal ext=-build)
  (enterfns ext=-enter)
  (mhelp "Rule of Extensionality."))

;;; The line assertions justifications and restrictions

(defrulewffs ext=
  (unique-lines 
     (ext=-p2 "f(AB) =(O(AB)(AB)) g(AB)")
     (ext=-p1 "FORALL x(B).f(AB) x(B) =(OAA) g(AB) x(B)"))
  (unique-restrictions 
     (ext=-restr0 (is-variable "x(B)"))
     (ext=-restr1 (not-free-in "x(B)" "f(AB)"))
     (ext=-restr2 (not-free-in "x(B)" "g(AB)"))))

;;; The suggesting rule definition.

(defsrule ext=
  (matchfn ext=-match)
  (match1fn ext=-match1)
  (shortfn ext=-short))


;;; The building function. 


(defun ext=-build (p2 p1 |x| |g| |f| p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ext=-mv2 ext=-mv1 ext=-mv0))
               (unquoted maint::wffarg (|x| |g| |f|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (ext=-p2 ext=-p1))
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
    (append maint::new-line-labels (list |x| |g| |f| p2-hyps p1-hyps))))

;;; The entering function. 


(defun ext=-enter (p2 p1 |x| |g| |f| p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (ext=-legal p2 p1 |x| |g| |f| p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun ext=-defaults (p2 p1 |x| |g| |f| p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (ext=-mv2 ext=-mv1 ext=-mv0))
               (unquoted maint::wffarg (|x| |g| |f|)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (ext=-p2 ext=-p1))
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
    (macro-do ((quoted maint::metavar (ext=-mv2 ext=-mv1 ext=-mv0))
               (unquoted maint::wffarg (|x| |g| |f|)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (ext=-hyp-defaults p2 p1 |x| |g| |f| p2-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list |x| |g| |f|))
                  maint::strong-hypdefaults))
    (list p2 p1 |x| |g| |f| p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun ext=-hyp-defaults (p2 p1 |x| |g| |f| p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore |x| |g| |f|))
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


(defun ext=-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'ext=-p2 'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail
                           (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr
                      (ext=-restr0 ext=-restr1 ext=-restr2)))
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


(defun ext=-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'ext=-p2 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t
         (let ((maint::rstr (get 'ext=-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'ext=-restr1 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t)))
         (let ((maint::rstr (get 'ext=-restr2 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun ext=-short (p2 |x|)
  (funcall #'comdecode
           (append (list 'ext=) (list (linealias p2)) (list '$)
                   (append (list (append
                                  (list 'quote)
                                  (list |x|)
                                  'nil)))
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun ext=-legal (p2 p1 |x| |g| |f| p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (ext=-legal-hyps p2 p1 |x| |g| |f| p2-hyps p1-hyps)
    (ext=-legal-wffs p2 p1 |x| |g| |f| p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun ext=-legal-hyps (p2 p1 |x| |g| |f| p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore |x| |g| |f|))
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


(defun ext=-legal-wffs (p2 p1 |x| |g| |f| p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (ext=-mv2 ext=-mv1 ext=-mv0))
               (unquoted maint::wffarg (|x| |g| |f|)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (ext=-p2 ext=-p1))
               (unquoted maint::linearg (p2 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr
                (ext=-restr0 ext=-restr1 ext=-restr2)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))
