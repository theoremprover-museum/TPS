;;; -*- Mode:LISP; Package:ML -*- 
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File: "/home/theorem/tps/lisp/ml2-logic1a.lisp"
;;;  assembled from "/home/theorem/tps/lisp/ml2-logic1a.rules"
;;;
;;; contains rules
;;; ITRUTH ICONJ ECONJ MP DEDUCT 
;;;

(in-package :ML)
(part-of MATH-LOGIC-2-RULES)

(defrulefile ml2-logic1a
  (contents ITRUTH ICONJ ECONJ MP DEDUCT ))

(context rule-commands)

(context RULES-2-PROP)


;;;
;;; Rule: ITRUTH
;;;

;;; The rule command definition.

(defmexpr itruth
  (argtypes line linelist)
  (wffargtypes  nil nil) (wffop-typelist)
  (argnames p1 p1-hyps)
  (arghelp "Line with TRUTH" "Hypotheses")
  (defaultfns itruth-defaults)
  (mainfns itruth-legal itruth-build)
  (enterfns itruth-enter)
  (mhelp "Rule to infer TRUTH"))

;;; The line assertions justifications and restrictions

(defrulewffs itruth
  (unique-lines 
     (itruth-p1 "TRUTH"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule itruth
  (matchfn itruth-match)
  (match1fn itruth-match1)
  (shortfn itruth-short))


;;; The building function. 


(defun itruth-build (p1 p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 1))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar nil)
               (unquoted maint::wffarg nil))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (itruth-p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Truth" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list p1-hyps))))

;;; The entering function. 


(defun itruth-enter (p1 p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again (itruth-legal p1 p1-hyps))
  (update-plan (eval-destruct ((p1 'ss))) (eval-destruct nil)))

;;; The default function. 


(defun itruth-defaults (p1 p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar nil)
               (unquoted maint::wffarg nil))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (itruth-p1))
               (unquoted maint::linearg (p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar nil)
               (unquoted maint::wffarg nil))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p1-hyps))
      (if (not (member '$ (list p1)))
          (setq-destruct (p1-hyps) (itruth-hyp-defaults p1 p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p1-hyps)))))
    (setq-destruct ((p1 'ss))
                   (line-no-defaults-from (eval-destruct ((p1 'ss)))))
    (when (not (member '$ (list p1 'ss)))
      (setq-destruct nil
                     (line-no-defaults-to (eval-destruct ((p1 'ss)))
                                          (eval-destruct nil))))
    (setq strong-defaultlist
          (append '(nil) (mapcar #'specified-p (list))
                  maint::strong-hypdefaults))
    (list p1 p1-hyps)))

;;; The hypotheses default function. 


(defun itruth-hyp-defaults (p1 p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p1))
               (unquoted maint::hyparg (p1-hyps)))
              (when (existent-p maint::linearg)
                (if (specified-p maint::hyparg)
                    (when (not (set-eq
                                maint::hyparg
                                (hypnums maint::linearg)))
                      (throwfail "Hypothesis specified for line "
                                 (maint::linearg . line)
                                 " are not the same as the one in the proof."))
                  (setq maint::hyparg (hypnums maint::linearg)))))
    (when (specified-p p1-hyps)
      (setq maint::hupper (meet-h maint::hupper p1-hyps)))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p1-hyps)))
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
                     (mapcar #'specified-p (list p1-hyps))))
             (when (not (specified-p p1-hyps))
               (setq p1-hyps (ordered-join-h maint::hlower (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p (list p1-hyps))))))
    (list p1-hyps)))

;;; The matching function. 


(defun itruth-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'itruth-p1 'meta-assertion)
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


(defun itruth-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'itruth-p1 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun itruth-short (p1)
  (funcall #'comdecode
           (append (list 'itruth) (list (linealias p1)) (list '$))))

;;; The checking function. 


(defun itruth-legal (p1 p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (itruth-legal-hyps p1 p1-hyps)
    (itruth-legal-wffs p1 p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun itruth-legal-hyps (p1 p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p1))
               (unquoted maint::hyparg (p1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p1-hyps))
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


(defun itruth-legal-wffs (p1 p1-hyps)
  (declare (special rule-hlower rule-hupper) (ignore p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar nil)
               (unquoted maint::wffarg nil))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (itruth-p1))
               (unquoted maint::linearg (p1)))
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
;;; Rule: ICONJ
;;;

;;; The rule command definition.

(defmexpr iconj
  (argtypes line line line gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" nil nil nil) (wffop-typelist)
  (argnames p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
  (arghelp "Line with Conjunction" "Line with Right Conjunct" "Line with Left Conjunct" "Right Conjunct" "Left Conjunct" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns iconj-defaults)
  (mainfns iconj-legal iconj-build)
  (enterfns iconj-enter)
  (mhelp "Rule to infer a conjunction from two conjuncts."))

;;; The line assertions justifications and restrictions

(defrulewffs iconj
  (unique-lines 
     (iconj-p3 "A(O) AND B(O)")
     (iconj-p2 "B(O)")
     (iconj-p1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule iconj
  (matchfn iconj-match)
  (match1fn iconj-match1)
  (shortfn iconj-short))


;;; The building function. 


(defun iconj-build (p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (iconj-mv1 iconj-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (iconj-p3 iconj-p2 iconj-p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p3 p2 p1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Conj" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list p1 p2)))
                 (nextplan) (nextplan)))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p3-hyps p2-hyps p1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p3-hyps p2-hyps p1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list b a p3-hyps p2-hyps p1-hyps))))

;;; The entering function. 


(defun iconj-enter (p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (iconj-legal p3 p2 p1 b a p3-hyps p2-hyps p1-hyps))
  (update-plan (eval-destruct ((p3 'ss)))
               (eval-destruct ((p1 'ss) (p2 'ss)))))

;;; The default function. 


(defun iconj-defaults (p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (iconj-mv1 iconj-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (iconj-p3 iconj-p2 iconj-p1))
               (unquoted maint::linearg (p3 p2 p1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (iconj-mv1 iconj-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p3-hyps p2-hyps p1-hyps))
      (if (not (member '$ (list p3 p2 p1)))
          (setq-destruct (p3-hyps p2-hyps p1-hyps)
                         (iconj-hyp-defaults p3 p2 p1 b a p3-hyps
                                             p2-hyps p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p3-hyps p2-hyps p1-hyps)))))
    (setq-destruct ((p3 'ss))
                   (line-no-defaults-from (eval-destruct ((p3 'ss)))))
    (when (not (member '$ (list p3 'ss)))
      (setq-destruct ((p1 'ss) (p2 'ss))
                     (line-no-defaults-to (eval-destruct ((p3 'ss)))
                                          (eval-destruct
                                           ((p1 'ss) (p2 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun iconj-hyp-defaults (p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p3 p2 p1))
               (unquoted maint::hyparg (p3-hyps p2-hyps p1-hyps)))
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
            (join-h maint::hlower (set-difference p2-hyps (list)))))
    (when (specified-p p1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference p1-hyps (list)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p3-hyps p2-hyps p1-hyps)))
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
                             (list p3-hyps p2-hyps p1-hyps))))
             (when (not (specified-p p3-hyps))
               (setq p3-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p p2-hyps))
               (setq p2-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p p1-hyps))
               (setq p1-hyps (ordered-join-h maint::hupper (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p3-hyps p2-hyps p1-hyps))))))
    (list p3-hyps p2-hyps p1-hyps)))

;;; The matching function. 


(defun iconj-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'iconj-p3 'meta-assertion)
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


(defun iconj-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'iconj-p3 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun iconj-short (p3)
  (funcall #'comdecode
           (append (list 'iconj) (list (linealias p3)) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$))))

;;; The checking function. 


(defun iconj-legal (p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (iconj-legal-hyps p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
    (iconj-legal-wffs p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun iconj-legal-hyps (p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p3 p2 p1))
               (unquoted maint::hyparg (p3-hyps p2-hyps p1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p3-hyps))
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


(defun iconj-legal-wffs (p3 p2 p1 b a p3-hyps p2-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p3-hyps p2-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (iconj-mv1 iconj-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (iconj-p3 iconj-p2 iconj-p1))
               (unquoted maint::linearg (p3 p2 p1)))
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
;;; Rule: ECONJ
;;;

;;; The rule command definition.

(defmexpr econj
  (argtypes line line line gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" nil nil nil) (wffop-typelist)
  (argnames d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
  (arghelp "Line with Conjunction" "Line with Right Conjunct" "Line with Left Conjunct" "Right Conjunct" "Left Conjunct" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns econj-defaults)
  (mainfns econj-legal econj-build)
  (enterfns econj-enter)
  (mhelp "Rule to infer two conjuncts from a conjunction."))

;;; The line assertions justifications and restrictions

(defrulewffs econj
  (unique-lines 
     (econj-d1 "A(O) AND B(O)")
     (econj-d3 "B(O)")
     (econj-d2 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule econj
  (matchfn econj-match)
  (match1fn econj-match1)
  (shortfn econj-short))


;;; The building function. 


(defun econj-build (d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (econj-mv1 econj-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (econj-d1 econj-d3 econj-d2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (d1 d3 d2))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((nextplan)
                 (list "Conj" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))
                 (list "Conj" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d1)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d3-hyps d2-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (d1-hyps d3-hyps d2-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list b a d1-hyps d3-hyps d2-hyps))))

;;; The entering function. 


(defun econj-enter (d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (econj-legal d1 d3 d2 b a d1-hyps d3-hyps d2-hyps))
  (update-plan (eval-destruct (('pp d1 'ss)))
               (eval-destruct (('pp d2 d3 'ss)))))

;;; The default function. 


(defun econj-defaults (d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (econj-mv1 econj-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (econj-d1 econj-d3 econj-d2))
               (unquoted maint::linearg (d1 d3 d2)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (econj-mv1 econj-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d1-hyps d3-hyps d2-hyps))
      (if (not (member '$ (list d1 d3 d2)))
          (setq-destruct (d1-hyps d3-hyps d2-hyps)
                         (econj-hyp-defaults d1 d3 d2 b a d1-hyps
                                             d3-hyps d2-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d1-hyps d3-hyps d2-hyps)))))
    (setq-destruct (('pp d1 'ss))
                   (line-no-defaults-from
                     (eval-destruct (('pp d1 'ss)))))
    (when (not (member '$ (list 'pp d1 'ss)))
      (setq-destruct (('pp d2 d3 'ss))
                     (line-no-defaults-to (eval-destruct
                                           (('pp d1 'ss)))
                                          (eval-destruct
                                           (('pp d2 d3 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)))

;;; The hypotheses default function. 


(defun econj-hyp-defaults (d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d1 d3 d2))
               (unquoted maint::hyparg (d1-hyps d3-hyps d2-hyps)))
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
    (when (specified-p d3-hyps)
      (setq maint::hupper (meet-h maint::hupper d3-hyps)))
    (when (specified-p d2-hyps)
      (setq maint::hupper (meet-h maint::hupper d2-hyps)))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d1-hyps d3-hyps d2-hyps)))
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
                             (list d1-hyps d3-hyps d2-hyps))))
             (when (not (specified-p d1-hyps))
               (setq d1-hyps (ordered-join-h maint::hupper (list))))
             (when (not (specified-p d3-hyps))
               (setq d3-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d2-hyps))
               (setq d2-hyps (ordered-join-h maint::hlower (list))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list d1-hyps d3-hyps d2-hyps))))))
    (list d1-hyps d3-hyps d2-hyps)))

;;; The matching function. 


(defun econj-match (maint::plan-support)
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
                                           'econj-d1
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


(defun econj-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'econj-d1 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun econj-short (d1)
  (funcall #'comdecode
           (append (list 'econj) (list (linealias d1)) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$))))

;;; The checking function. 


(defun econj-legal (d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (econj-legal-hyps d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
    (econj-legal-wffs d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
    t))

;;; The hypotheses checking function. 


(defun econj-legal-hyps (d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (d1 d3 d2))
               (unquoted maint::hyparg (d1-hyps d3-hyps d2-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d1-hyps (list))))
    (setq maint::hupper (meet-h maint::hupper d3-hyps))
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


(defun econj-legal-wffs (d1 d3 d2 b a d1-hyps d3-hyps d2-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore d1-hyps d3-hyps d2-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (econj-mv1 econj-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (econj-d1 econj-d3 econj-d2))
               (unquoted maint::linearg (d1 d3 d2)))
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
;;; Rule: MP
;;;

;;; The rule command definition.

(defmexpr mp
  (argtypes line line line gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" nil nil nil) (wffop-typelist)
  (argnames d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
  (arghelp "Line with Implication" "Line with Succedent of Implication" "Line with Antecedent of Implication" "Succedent of Implication" "Antecedent of Implication" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns mp-defaults)
  (mainfns mp-legal mp-build)
  (enterfns mp-enter)
  (mhelp "Modus Ponens."))

;;; The line assertions justifications and restrictions

(defrulewffs mp
  (unique-lines 
     (mp-d2 "A(O) IMPLIES B(O)")
     (mp-d3 "B(O)")
     (mp-p1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule mp
  (matchfn mp-match)
  (match1fn mp-match1)
  (shortfn mp-short))


;;; The building function. 


(defun mp-build (d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (mp-mv1 mp-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (mp-d2 mp-d3 mp-p1))
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
                 (list "MP" (mapcar #'meta-subst 'nil)
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
    (append maint::new-line-labels (list b a d2-hyps d3-hyps p1-hyps))))

;;; The entering function. 


(defun mp-enter (d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (mp-legal d2 d3 p1 b a d2-hyps d3-hyps p1-hyps))
  (update-plan (eval-destruct (('pp d2 'ss)))
               (eval-destruct ((p1 'ss) ('pp d3 'ss p1)))))

;;; The default function. 


(defun mp-defaults (d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (mp-mv1 mp-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line (mp-d2 mp-d3 mp-p1))
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
    (macro-do ((quoted maint::metavar (mp-mv1 mp-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list d2-hyps d3-hyps p1-hyps))
      (if (not (member '$ (list d2 d3 p1)))
          (setq-destruct (d2-hyps d3-hyps p1-hyps)
                         (mp-hyp-defaults d2 d3 p1 b a d2-hyps d3-hyps
                                          p1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list d2-hyps d3-hyps p1-hyps)))))
    (setq-destruct (('pp d2 'ss))
                   (line-no-defaults-from
                     (eval-destruct (('pp d2 'ss)))))
    (when (not (member '$ (list 'pp d2 'ss)))
      (setq-destruct-multi (p1) ((p1 'ss) ('pp d3 'ss p1))
                           (line-no-defaults-to (eval-destruct
                                                 (('pp d2 'ss)))
                                                (eval-destruct
                                                 ((p1 'ss)
                                                  ('pp d3 'ss p1))))))
    (setq strong-defaultlist
          (append '(nil nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)))

;;; The hypotheses default function. 


(defun mp-hyp-defaults (d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
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


(defun mp-match (maint::plan-support)
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
              (%catch% (progn (match-bind (get 'mp-d2 'meta-assertion)
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


(defun mp-match1 (maint::sline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and t
         (not (eq 'maint::failed
                  (%catch% (match-bind (get 'mp-d2 'meta-assertion)
                                       (get maint::sline 'assertion))
                           (fail 'maint::failed)))))))

;;; The short version of the rule as a function.  


(defun mp-short (d2)
  (funcall #'comdecode
           (append (list 'mp) (list (linealias d2)) (list '$) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$))))

;;; The checking function. 


(defun mp-legal (d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (mp-legal-hyps d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
    (mp-legal-wffs d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
    t))

;;; The hypotheses checking function. 


(defun mp-legal-hyps (d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
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


(defun mp-legal-wffs (d2 d3 p1 b a d2-hyps d3-hyps p1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore d2-hyps d3-hyps p1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (mp-mv1 mp-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::metalabel nil))
              (when (not (same-match-p maint::metalabel
                                       (wffeval maint::metalabel)
                                       (meta-label-eval
                                        maint::metalabel)))
                (mismatch% (wffeval maint::metalabel)
                           (meta-label-eval maint::metalabel))))
    (macro-do ((quoted maint::unique-line (mp-d2 mp-d3 mp-p1))
               (unquoted maint::linearg (d2 d3 p1)))
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
;;; Rule: DEDUCT
;;;

;;; The rule command definition.

(defmexpr deduct
  (argtypes line line line gwff gwff linelist linelist linelist)
  (wffargtypes  nil nil nil "O" "O" nil nil nil) (wffop-typelist)
  (argnames p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (arghelp "Line with Implication" "Line with Conclusion" "Line with Hypothesis" "Succedent of Implication" "Antecendent of Implication" "Hypotheses" "Hypotheses" "Hypotheses")
  (defaultfns deduct-defaults)
  (mainfns deduct-legal deduct-build)
  (enterfns deduct-enter)
  (mhelp "The deduction rule."))

;;; The line assertions justifications and restrictions

(defrulewffs deduct
  (unique-lines 
     (deduct-p3 "A(O) IMPLIES B(O)")
     (deduct-d2 "B(O)")
     (deduct-h1 "A(O)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule deduct
  (matchfn deduct-match)
  (match1fn deduct-match1)
  (shortfn deduct-short))


;;; The building function. 


(defun deduct-build (p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (let ((maint::new-line-labels (line-label-vec 3))
        (wffbindings nil)
        (maint::num-alist nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (deduct-mv1 deduct-mv0))
               (unquoted maint::wffarg (b a)))
              (push (cons maint::metavar maint::wffarg) wffbindings))
    (macro-do ((quoted maint::u-line (deduct-p3 deduct-d2 deduct-h1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label
                       (meta-subst (get maint::u-line 'meta-assertion))
                       'assertion))
    (macro-do ((unquoted maint::line-arg (p3 d2 h1))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::line-arg 'linenumber)
              (push (cons maint::line-arg maint::line-label)
                    maint::num-alist))
    (macro-do ((unquoted maint::just
                ((list "Deduct" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list d2)))
                 (nextplan)
                 (list "Hyp" (mapcar #'meta-subst 'nil)
                       (subst-labels maint::num-alist (list)))))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::just 'justification))
    (macro-do ((unquoted maint::hyp-arg (p3-hyps d2-hyps h1-hyps)))
              (setq maint::hyp-arg
                    (subst-labels maint::num-alist maint::hyp-arg)))
    (macro-do ((unquoted maint::hyp-arg (p3-hyps d2-hyps h1-hyps))
               (local maint::line-label maint::new-line-labels))
              (putprop maint::line-label maint::hyp-arg 'hypotheses))
    (append maint::new-line-labels (list b a p3-hyps d2-hyps h1-hyps))))

;;; The entering function. 


(defun deduct-enter (p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (declare (special core::check-hyps-again))
  (when core::check-hyps-again
    (deduct-legal p3 d2 h1 b a p3-hyps d2-hyps h1-hyps))
  (update-plan (eval-destruct ((p3 'ss)))
               (eval-destruct ((d2 h1 'ss)))))

;;; The default function. 


(defun deduct-defaults (p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (declare (special strong-defaultlist))
  (let ((wffbindings nil) maint::strong-hypdefaults)
    (declare (special wffbindings maint::strong-hypdefaults))
    (macro-do ((quoted maint::metavar (deduct-mv1 deduct-mv0))
               (unquoted maint::wffarg (b a)))
              (when (specified-p maint::wffarg)
                (push (cons maint::metavar maint::wffarg)
                      wffbindings)))
    (macro-do ((quoted maint::unique-line
                (deduct-p3 deduct-d2 deduct-h1))
               (unquoted maint::linearg (p3 d2 h1)))
              (if (and (not (eq maint::linearg '$))
                       (existent-p maint::linearg))
                  (match-bind (get maint::unique-line 'meta-assertion)
                              (get (numalias maint::linearg)
                                   'assertion))))
    (macro-do ((quoted maint::metalabel nil))
              (let ((maint::wffval (wffeval maint::metalabel)))
                (when (not maint::wffval)
                  (%catch% (meta-subst maint::metalabel) (fail nil)))))
    (macro-do ((quoted maint::metavar (deduct-mv1 deduct-mv0))
               (unquoted maint::wffarg (b a)))
              (let ((maint::wffval (wffeval maint::metavar)))
                (when maint::wffval
                  (setq maint::wffarg maint::wffval))))
    (when (member '$ (list p3-hyps d2-hyps h1-hyps))
      (if (not (member '$ (list p3 d2 h1)))
          (setq-destruct (p3-hyps d2-hyps h1-hyps)
                         (deduct-hyp-defaults p3 d2 h1 b a p3-hyps
                                              d2-hyps h1-hyps))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p3-hyps d2-hyps h1-hyps)))))
    (setq-destruct ((p3 'ss))
                   (line-no-defaults-from (eval-destruct ((p3 'ss)))))
    (when (not (member '$ (list p3 'ss)))
      (setq-destruct ((d2 h1 'ss))
                     (line-no-defaults-to (eval-destruct ((p3 'ss)))
                                          (eval-destruct
                                           ((d2 h1 'ss))))))
    (setq strong-defaultlist
          (append '(nil nil nil) (mapcar #'specified-p (list b a))
                  maint::strong-hypdefaults))
    (list p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)))

;;; The hypotheses default function. 


(defun deduct-hyp-defaults (p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (declare (special maint::strong-hypdefaults) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p3 d2 h1))
               (unquoted maint::hyparg (p3-hyps d2-hyps h1-hyps)))
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
    (when (specified-p d2-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference d2-hyps (list h1)))))
    (when (specified-p h1-hyps)
      (setq maint::hlower
            (join-h maint::hlower (set-difference h1-hyps (list h1)))))
    (if (and (eq maint::hlower '$) (eq maint::hupper '$))
        (setq maint::strong-hypdefaults
              (mapcar #'specified-p (list p3-hyps d2-hyps h1-hyps)))
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
                             (list p3-hyps d2-hyps h1-hyps))))
             (when (not (specified-p p3-hyps))
               (setq p3-hyps (ordered-join-h maint::hlower (list))))
             (when (not (specified-p d2-hyps))
               (setq d2-hyps (ordered-join-h maint::hupper (list h1))))
             (when (not (specified-p h1-hyps))
               (setq h1-hyps (ordered-join-h maint::hupper (list h1))))
             (when auto-generate-hyps
               (setq maint::strong-hypdefaults
                     (mapcar #'specified-p
                             (list p3-hyps d2-hyps h1-hyps))))))
    (list p3-hyps d2-hyps h1-hyps)))

;;; The matching function. 


(defun deduct-match (maint::plan-support)
  (let ((wffbindings nil) maint::matched-plan maint::matched-support)
    (declare (special wffbindings))
    (setq maint::matched-plan
          (progn (%catch% (match-bind (get 'deduct-p3 'meta-assertion)
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


(defun deduct-match1 (pline)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (and (not (eq 'maint::failed
                  (%catch% (match-bind (get 'deduct-p3 'meta-assertion)
                                       (get pline 'assertion))
                           (fail 'maint::failed))))
         t)))

;;; The short version of the rule as a function.  


(defun deduct-short (p3)
  (funcall #'comdecode
           (append (list 'deduct) (list (linealias p3)) (list '$)
                   (list '$) (list '$) (list '$) (list '$) (list '$)
                   (list '$))))

;;; The checking function. 


(defun deduct-legal (p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (let ((rule-hupper nil) (rule-hlower nil))
    (declare (special rule-hupper rule-hlower))
    (setq core::check-hyps-again nil)
    (deduct-legal-hyps p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
    (deduct-legal-wffs p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
    t))

;;; The hypotheses checking function. 


(defun deduct-legal-hyps (p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (declare (special rule-hupper rule-hlower) (ignore b a))
  (let ((maint::hupper '$) (maint::hlower '$))
    (macro-do ((unquoted maint::linearg (p3 d2 h1))
               (unquoted maint::hyparg (p3-hyps d2-hyps h1-hyps)))
              (when (and (existent-p maint::linearg)
                         (not (set-eq maint::hyparg
                                      (hypnums maint::linearg))))
                (throwfail "Hypothesis specified for line "
                           (maint::linearg . line)
                           " are not the same as the ones in the proof.")))
    (setq maint::hupper (meet-h maint::hupper p3-hyps))
    (setq maint::hlower
          (join-h maint::hlower (set-difference d2-hyps (list h1))))
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


(defun deduct-legal-wffs (p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (declare (special rule-hlower rule-hupper)
           (ignore p3-hyps d2-hyps h1-hyps))
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (macro-do ((quoted maint::metavar (deduct-mv1 deduct-mv0))
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
                (deduct-p3 deduct-d2 deduct-h1))
               (unquoted maint::linearg (p3 d2 h1)))
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
