;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml1-logic3a.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml1-logic3a.rules"
;;;
;;; contains rules
;;; NNF-EXPAND NNF PULLNEG PUSHNEG 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml1-logic3a
  (contents NNF-EXPAND NNF PULLNEG PUSHNEG ))

(context rule-commands)

(context RULES-3-NEG)


;;;
;;; Rule: NNF-EXPAND
;;;

;;; The rule command definition.

(defmexpr nnf-expand
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a neg-norm p2-hyps p1-hyps)
  (arghelp "Line not in Negation Normal Form" "Line in Negation Normal Form" "Wff" "Wff in Negation Normal Form." "Hypotheses" "Hypotheses")
  (defaultfns nnf-expand-defaults)
  (mainfns nnf-expand-legal nnf-expand-build)
  (enterfns nnf-expand-enter)
  (mhelp "Expand Wff from Negation Normal Form."))

;;; The line assertions justifications and restrictions

(defrulewffs nnf-expand
  (unique-lines 
     (nnf-expand-p2 "A(O)")
     (nnf-expand-p1 "`(NEG-NORM  A(O))"))
  (unique-restrictions 
     (nnf-expand-restr0 (non-atomic "A(O)"))))

;;; The suggesting rule definition.

(defsrule nnf-expand
  (matchfn nnf-expand-match)
  (match1fn nnf-expand-match1)
  (shortfn nnf-expand-short))


;;; The building function. 


(defun nnf-expand-build (p2 p1 a neg-norm p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (nnf-expand-mv0 nnf-expand-ml0))
               (unquoted maint::wffarg (a neg-norm)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (nnf-expand-p2 nnf-expand-p1))
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
                ((list "NNF-Expand" (mapcar #'meta-subst 'nil)
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
    (append maint::new-line-labels (list a neg-norm p2-hyps p1-hyps))))

;;; The entering function. 


(defun nnf-expand-enter (p2 p1 a neg-norm p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (nnf-expand-legal p2 p1 a neg-norm p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun nnf-expand-defaults (p2 p1 a neg-norm p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (nnf-expand-mv0 nnf-expand-ml0))
               (unquoted maint::wffarg (a neg-norm)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (nnf-expand-p2 nnf-expand-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (nnf-expand-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (nnf-expand-mv0 nnf-expand-ml0))
               (unquoted maint::wffarg (a neg-norm)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (nnf-expand-hyp-defaults p2 p1 a neg-norm
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
          (append '(nil nil) (mapcar #'specified-p (list a neg-norm))
                  maint::strong-hypdefaults))
    (list p2 p1 a neg-norm p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun nnf-expand-hyp-defaults (p2 p1 a neg-norm p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a neg-norm))
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


(defun nnf-expand-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get
                                       'nnf-expand-p2
                                       'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail
                           (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr (nnf-expand-restr0)))
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


(defun nnf-expand-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'nnf-expand-p2
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t
         (let ((maint::rstr (get 'nnf-expand-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun nnf-expand-short (p2)
  (funcall #'comdecode
           (append (list 'nnf-expand) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun nnf-expand-legal (p2 p1 a neg-norm p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (nnf-expand-legal-hyps p2 p1 a neg-norm p2-hyps p1-hyps)
    (nnf-expand-legal-wffs p2 p1 a neg-norm p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun nnf-expand-legal-hyps (p2 p1 a neg-norm p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a neg-norm))
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


(defun nnf-expand-legal-wffs (p2 p1 a neg-norm p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (nnf-expand-mv0 nnf-expand-ml0))
               (unquoted maint::wffarg (a neg-norm)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (nnf-expand-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line
                (nnf-expand-p2 nnf-expand-p1))
               (unquoted maint::linearg (p2 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (nnf-expand-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: NNF
;;;

;;; The rule command definition.

(defmexpr nnf
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a neg-norm d1-hyps d2-hyps)
  (arghelp "Line not in Negation Normal Form" "Line in Negation Normal Form" "Wff" "Wff in Negation Normal Form." "Hypotheses" "Hypotheses")
  (defaultfns nnf-defaults)
  (mainfns nnf-legal nnf-build)
  (enterfns nnf-enter)
  (mhelp "Put Wff in Negation Normal Form."))

;;; The line assertions justifications and restrictions

(defrulewffs nnf
  (unique-lines 
     (nnf-d1 "A(O)")
     (nnf-d2 "`(NEG-NORM  A(O))"))
  (unique-restrictions 
     (nnf-restr0 (non-atomic-or-truthvalue "A(O)"))))

;;; The suggesting rule definition.

(defsrule nnf
  (matchfn nnf-match)
  (match1fn nnf-match1)
  (shortfn nnf-short))


;;; The building function. 


(defun nnf-build (d1 d2 a neg-norm d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (nnf-mv0 nnf-ml0))
               (unquoted maint::wffarg (a neg-norm)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (nnf-d1 nnf-d2))
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
                 (list "NNF" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list a neg-norm d1-hyps d2-hyps))))

;;; The entering function. 


(defun nnf-enter (d1 d2 a neg-norm d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (nnf-legal d1 d2 a neg-norm d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun nnf-defaults (d1 d2 a neg-norm d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (nnf-mv0 nnf-ml0))
               (unquoted maint::wffarg (a neg-norm)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (nnf-d1 nnf-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (nnf-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (nnf-mv0 nnf-ml0))
               (unquoted maint::wffarg (a neg-norm)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (nnf-hyp-defaults d1 d2 a neg-norm d1-hyps
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
          (append '(nil nil) (mapcar #'specified-p (list a neg-norm))
                  maint::strong-hypdefaults))
    (list d1 d2 a neg-norm d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun nnf-hyp-defaults (d1 d2 a neg-norm d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore a neg-norm))
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


(defun nnf-match (maint::plan-support)
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
              (%catch% (progn (match-bind (get 'nnf-d1 'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (nnf-restr0)))
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


(defun nnf-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'nnf-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'nnf-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun nnf-short (d1)
  (funcall #'comdecode
           (append (list 'nnf) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun nnf-legal (d1 d2 a neg-norm d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (nnf-legal-hyps d1 d2 a neg-norm d1-hyps d2-hyps)
    (nnf-legal-wffs d1 d2 a neg-norm d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun nnf-legal-hyps (d1 d2 a neg-norm d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a neg-norm))
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


(defun nnf-legal-wffs (d1 d2 a neg-norm d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (nnf-mv0 nnf-ml0))
               (unquoted maint::wffarg (a neg-norm)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (nnf-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (nnf-d1 nnf-d2))
               (unquoted maint::linearg (d1 d2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (nnf-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: PULLNEG
;;;

;;; The rule command definition.

(defmexpr pullneg
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames p2 p1 a push-negation p2-hyps p1-hyps)
  (arghelp "Line with Negation" "Line after Pushing in Negation one Step" "Scope of Negation" "Wff after Pushing in Negation" "Hypotheses" "Hypotheses")
  (defaultfns pullneg-defaults)
  (mainfns pullneg-legal pullneg-build)
  (enterfns pullneg-enter)
  (mhelp "Pull out negation."))

;;; The line assertions justifications and restrictions

(defrulewffs pullneg
  (unique-lines 
     (pullneg-p2 "~A(O)")
     (pullneg-p1 "`(PUSH-NEGATION  [~A(O)])"))
  (unique-restrictions 
     (pullneg-restr0 (non-atomic "A(O)"))))

;;; The suggesting rule definition.

(defsrule pullneg
  (matchfn pullneg-match)
  (match1fn pullneg-match1)
  (shortfn pullneg-short))


;;; The building function. 


(defun pullneg-build (p2 p1 a push-negation p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (pullneg-mv0 pullneg-ml0))
               (unquoted maint::wffarg (a push-negation)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (pullneg-p2 pullneg-p1))
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
                ((list "Neg" (mapcar #'meta-subst 'nil)
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
            (list a push-negation p2-hyps p1-hyps))))

;;; The entering function. 


(defun pullneg-enter (p2 p1 a push-negation p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (pullneg-legal p2 p1 a push-negation p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p2 'ss))) (eval-destruct ((p1 'ss)))))

;;; The default function. 


(defun pullneg-defaults (p2 p1 a push-negation p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (pullneg-mv0 pullneg-ml0))
               (unquoted maint::wffarg (a push-negation)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (pullneg-p2 pullneg-p1))
               (unquoted maint::linearg (p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (pullneg-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (pullneg-mv0 pullneg-ml0))
               (unquoted maint::wffarg (a push-negation)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p2-hyps p1-hyps))
      (if (not (member '$ (list p2 p1)))
          (setq-destruct (p2-hyps p1-hyps)
                         (pullneg-hyp-defaults p2 p1 a push-negation
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
                  (mapcar #'specified-p (list a push-negation))
                  maint::strong-hypdefaults))
    (list p2 p1 a push-negation p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun pullneg-hyp-defaults (p2 p1 a push-negation p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults)
           (ignore a push-negation))
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


(defun pullneg-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'pullneg-p2 'meta-assertion)
                                      (get
                                       (car maint::plan-support)
                                       'assertion))
                          (fail
                           (throwfail "Planned lines did not match.")))
                 (list (car maint::plan-support))))
    (setq maint::matched-support
          (macro-do ((quoted maint::restr (pullneg-restr0)))
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


(defun pullneg-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'pullneg-p2
                                        'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t
         (let ((maint::rstr (get 'pullneg-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun pullneg-short (p2)
  (funcall #'comdecode
           (append (list 'pullneg) (list (linealias p2)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun pullneg-legal (p2 p1 a push-negation p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (pullneg-legal-hyps p2 p1 a push-negation p2-hyps p1-hyps)
    (pullneg-legal-wffs p2 p1 a push-negation p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun pullneg-legal-hyps (p2 p1 a push-negation p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a push-negation))
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


(defun pullneg-legal-wffs (p2 p1 a push-negation p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (pullneg-mv0 pullneg-ml0))
               (unquoted maint::wffarg (a push-negation)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (pullneg-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (pullneg-p2 pullneg-p1))
               (unquoted maint::linearg (p2 p1)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (pullneg-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))


;;;
;;; Rule: PUSHNEG
;;;

;;; The rule command definition.

(defmexpr pushneg
  (argtypes line line gwff gwff linelist linelist)
  (wffargtypes  nil nil "O" "O" nil nil) (wffop-typelist)
  (argnames d1 d2 a push-negation d1-hyps d2-hyps)
  (arghelp "Line with Negation" "Line after Pushing in Negation one Step" "Scope of Negation" "Wff after Pushing in Negation" "Hypotheses" "Hypotheses")
  (defaultfns pushneg-defaults)
  (mainfns pushneg-legal pushneg-build)
  (enterfns pushneg-enter)
  (mhelp "Push in negation."))

;;; The line assertions justifications and restrictions

(defrulewffs pushneg
  (unique-lines 
     (pushneg-d1 "~A(O)")
     (pushneg-d2 "`(PUSH-NEGATION  [~A(O)])"))
  (unique-restrictions 
     (pushneg-restr0 (non-atomic-or-truthvalue "A(O)"))))

;;; The suggesting rule definition.

(defsrule pushneg
  (matchfn pushneg-match)
  (match1fn pushneg-match1)
  (shortfn pushneg-short))


;;; The building function. 


(defun pushneg-build (d1 d2 a push-negation d1-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 2))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (pushneg-mv0 pushneg-ml0))
               (unquoted maint::wffarg (a push-negation)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (pushneg-d1 pushneg-d2))
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
                 (list "Neg" (mapcar #'meta-subst 'nil)
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
            (list a push-negation d1-hyps d2-hyps))))

;;; The entering function. 


(defun pushneg-enter (d1 d2 a push-negation d1-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (pushneg-legal d1 d2 a push-negation d1-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 'ss)))))

;;; The default function. 


(defun pushneg-defaults (d1 d2 a push-negation d1-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (pushneg-mv0 pushneg-ml0))
               (unquoted maint::wffarg (a push-negation)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (pushneg-d1 pushneg-d2))
               (unquoted maint::linearg (d1 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel (pushneg-ml0)))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (pushneg-mv0 pushneg-ml0))
               (unquoted maint::wffarg (a push-negation)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d2-hyps))
      (if (not (member '$ (list d1 d2)))
          (setq-destruct (d1-hyps d2-hyps)
                         (pushneg-hyp-defaults d1 d2 a push-negation
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
          (append '(nil nil)
                  (mapcar #'specified-p (list a push-negation))
                  maint::strong-hypdefaults))
    (list d1 d2 a push-negation d1-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun pushneg-hyp-defaults (d1 d2 a push-negation d1-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults)
           (ignore a push-negation))
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


(defun pushneg-match (maint::plan-support)
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
                                           'pushneg-d1
                                           'meta-assertion)
                                          (get
                                           (car maint::supps)
                                           'assertion))
                              (macro-do ((quoted
                                          maint::restr
                                          (pushneg-restr0)))
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


(defun pushneg-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get
                                        'pushneg-d1
                                        'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed))))
         (let ((maint::rstr (get 'pushneg-restr0 'restr-call)))
           (%catch% (apply (car maint::rstr)
                           (mapcar #'meta-subst (cdr maint::rstr)))
                    (fail t))))))

;;; The short version of the rule as a function.  


(defun pushneg-short (d1)
  (funcall #'comdecode
           (append (list 'pushneg) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun pushneg-legal (d1 d2 a push-negation d1-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (pushneg-legal-hyps d1 d2 a push-negation d1-hyps d2-hyps)
    (pushneg-legal-wffs d1 d2 a push-negation d1-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun pushneg-legal-hyps (d1 d2 a push-negation d1-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore a push-negation))
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


(defun pushneg-legal-wffs (d1 d2 a push-negation d1-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper) (ignore d1-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (pushneg-mv0 pushneg-ml0))
               (unquoted maint::wffarg (a push-negation)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel (pushneg-ml0)))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (pushneg-d1 pushneg-d2))
               (unquoted maint::linearg (d1 d2)))
              (when (existent-p maint::linearg)
                (match-bind (get maint::unique-line 'meta-assertion)
                            (get (numalias maint::linearg)
                                 'assertion))))
    (macro-do ((quoted maint::restr (pushneg-restr0)))
              (let ((maint::rstr (get maint::restr 'restr-call)))
                (when (not (apply (car maint::rstr)
                                  (mapcar
                                   #'meta-subst
                                   (cdr maint::rstr))))
                  (throwfail "Rule not applicable.  " t "Restrictions "
                             (car maint::rstr) " not satisfied.  "))))))
