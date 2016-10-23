;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ML)
(part-of MATH-LOGIC-1-RULES)

;;;
;;; File: PS:<TPS.CLISP>ML1-LOGIC1.LSP.2
;;;  assembled from PS:<TPS.CLISP>ML1-LOGIC1.RULES.1
;;;
;;; contains rules
;;; DEDUCT MP ECONJ ICONJ CASES IDISJ-RIGHT IDISJ-LEFT INDIRECT 
;;;

(part-of MATH-LOGIC-1-RULES)

(deffile ml1-logic1
  (part-of math-logic-1-rules)
  (extension lisp)
  (mhelp "Defines DEDUCT, MP, ECONJ, ICONJ, CASES, IDISJ, INDIRECT."))

(defrulefile ml1-logic1
  (contents DEDUCT MP ECONJ ICONJ CASES IDISJ-RIGHT IDISJ-LEFT INDIRECT ))

(context rule-commands)

(context rules-2-prop)


;;;
;;; Rule: DEDUCT
;;;

;;; The rule command definition.

(defmexpr deduct
  (argtypes line line line gwff gwff linelist linelist linelist)
  (wffargtypes nil nil nil "o" "o" nil nil nil)
  (wffop-typelist)
  (argnames p3 d2 h1 b a p3-hyps d2-hyps h1-hyps)
  (arghelp "Line with Implication" "Line with Conclusion" "Line with Hypothesis"
   "Succedent of Implication" "Antecendent of Implication" "Hypotheses"
   "Hypotheses" "Hypotheses")
  (defaultfns deduct-defaults)
  (mainfns deduct-legal deduct-build)
  (enterfns deduct-enter)
  (mhelp "The deduction rule."))

;;; The line assertions justifications and restrictions

(defrulewffs deduct
  (unique-lines 
     (deduct-p3 "a(o) IMPLIES b(o)")
     (deduct-d2 "b(o)")
     (deduct-h1 "a(o)"))
  (unique-restrictions ))

;;; The suggesting rule definition.

(defsrule deduct
  (matchfn deduct-match))


;;; The building function. 

(defun deduct-build (P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
  (LET ((NEW-LINE-LABELS (LINE-LABEL-VEC 3))
        (WFFBINDINGS NIL)
        (NUM-ALIST NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO ((QUOTED METAVAR (DEDUCT-MV1 DEDUCT-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO
     ((QUOTED U-LINE (DEDUCT-P3 DEDUCT-D2 DEDUCT-H1))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL (META-SUBST (GET U-LINE 'META-ASSERTION)) 'ASSERTION))
    (MACRO-DO ((UNQUOTED LINE-ARG (P3 D2 H1)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL LINE-ARG 'LINENUMBER)
     (PUSH (CONS LINE-ARG LINE-LABEL) NUM-ALIST))
    (MACRO-DO
     ((UNQUOTED JUST
       ((LIST "Deduct" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST D2)))
        (NEXTPLAN)
        (LIST "Hyp" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST)))))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL JUST 'JUSTIFICATION))
    (MACRO-DO ((UNQUOTED HYP-ARG (P3-HYPS D2-HYPS H1-HYPS)))
     (SETQ HYP-ARG (SUBST-LABELS NUM-ALIST HYP-ARG)))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (P3-HYPS D2-HYPS H1-HYPS))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL HYP-ARG 'HYPOTHESES))
    (APPEND NEW-LINE-LABELS (LIST B A P3-HYPS D2-HYPS H1-HYPS))))

;;; The entering function. 

(defun deduct-enter (P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
  (DECLARE (IGNORE B A P3-HYPS D2-HYPS H1-HYPS))
  (UPDATE-PLAN (EVAL-DESTRUCT ((P3 'SS))) (EVAL-DESTRUCT ((D2 H1 'SS)))))

;;; The default function. 

(defun deduct-defaults (P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
  (DECLARE (SPECIAL STRONG-DEFAULTLIST))
  (LET ((WFFBINDINGS NIL)
        STRONG-HYPDEFAULTS)
    (DECLARE (SPECIAL WFFBINDINGS STRONG-HYPDEFAULTS))
    (MACRO-DO ((QUOTED METAVAR (DEDUCT-MV1 DEDUCT-MV0)) (UNQUOTED WFFARG (B A)))
     (WHEN (SPECIFIED-P WFFARG)
       (PUSH (CONS METAVAR WFFARG) WFFBINDINGS)))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (DEDUCT-P3 DEDUCT-D2 DEDUCT-H1))
      (UNQUOTED LINEARG (P3 D2 H1)))
     (IF (AND (NOT (EQ LINEARG '$)) (EXISTENT-P LINEARG))
         (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                     (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (LET ((WFFVAL (WFFEVAL METALABEL)))
       (WHEN (NOT WFFVAL)
         (%CATCH% (META-SUBST METALABEL) (FAIL NIL)))))
    (MACRO-DO ((QUOTED METAVAR (DEDUCT-MV1 DEDUCT-MV0)) (UNQUOTED WFFARG (B A)))
     (LET ((WFFVAL (WFFEVAL METAVAR)))
       (WHEN WFFVAL
         (SETQ WFFARG WFFVAL))))
    (WHEN (MEMBER '$ (LIST P3-HYPS D2-HYPS H1-HYPS))
      (IF (NOT (MEMBER '$ (LIST P3 D2 H1)))
          (SETQ-DESTRUCT (P3-HYPS D2-HYPS H1-HYPS)
           (DEDUCT-HYP-DEFAULTS P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS))
          (SETQ STRONG-HYPDEFAULTS
                (MAPCAR #'SPECIFIED-P (LIST P3-HYPS D2-HYPS H1-HYPS)))))
    (SETQ-DESTRUCT ((P3 'SS)) (LINE-NO-DEFAULTS-FROM (EVAL-DESTRUCT ((P3 'SS)))))
    (WHEN (NOT (MEMBER '$ (LIST P3 'SS)))
      (SETQ-DESTRUCT ((D2 H1 'SS))
       (LINE-NO-DEFAULTS-TO (EVAL-DESTRUCT ((P3 'SS)))
        (EVAL-DESTRUCT ((D2 H1 'SS))))))
    (SETQ STRONG-DEFAULTLIST
          (APPEND '(NIL NIL NIL) (MAPCAR #'SPECIFIED-P (LIST B A))
                  STRONG-HYPDEFAULTS))
    (LIST P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)))

;;; The hypotheses default function. 

(defun deduct-hyp-defaults (P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
  (DECLARE (SPECIAL STRONG-HYPDEFAULTS) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (P3 D2 H1)) (UNQUOTED HYPARG (P3-HYPS D2-HYPS H1-HYPS)))
     (WHEN (EXISTENT-P LINEARG)
       (IF (SPECIFIED-P HYPARG)
           (WHEN (NOT (SET-EQ HYPARG (HYPNUMS LINEARG)))
             (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                        " are not the same as the one in the proof."))
           (SETQ HYPARG (HYPNUMS LINEARG)))))
    (WHEN (SPECIFIED-P P3-HYPS)
      (SETQ HUPPER (MEET-H HUPPER P3-HYPS)))
    (WHEN (SPECIFIED-P D2-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF D2-HYPS (LIST H1)))))
    (WHEN (SPECIFIED-P H1-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF H1-HYPS (LIST H1)))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$))
        (SETQ STRONG-HYPDEFAULTS
              (MAPCAR #'SPECIFIED-P (LIST P3-HYPS D2-HYPS H1-HYPS)))
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))
          (WHEN (NOT AUTO-GENERATE-HYPS)
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P3-HYPS D2-HYPS H1-HYPS))))
          (WHEN (NOT (SPECIFIED-P P3-HYPS))
            (SETQ P3-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN (NOT (SPECIFIED-P D2-HYPS))
            (SETQ D2-HYPS (ORDERED-JOIN-H HUPPER (LIST H1))))
          (WHEN (NOT (SPECIFIED-P H1-HYPS))
            (SETQ H1-HYPS (ORDERED-JOIN-H HUPPER (LIST H1))))
          (WHEN AUTO-GENERATE-HYPS
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P3-HYPS D2-HYPS H1-HYPS))))))
    (LIST P3-HYPS D2-HYPS H1-HYPS)))

;;; The matching function. 

(defun deduct-match (PLAN-SUPPORT)
  (LET ((WFFBINDINGS NIL)
        (MATCHED-PLAN)
        (MATCHED-SUPPORT))
    (DECLARE (SPECIAL WFFBINDINGS))
    (SETQ MATCHED-PLAN
          (PROGN
            (%CATCH%
             (MATCH-BIND (GET 'DEDUCT-P3 'META-ASSERTION)
                         (GET (CAR PLAN-SUPPORT) 'ASSERTION))
             (FAIL (THROWFAIL "Planned lines did not match.")))
            (LIST (CAR PLAN-SUPPORT))))
    (SETQ MATCHED-SUPPORT
          (MACRO-DO ((QUOTED RESTR NIL))
           (LET ((RSTR (GET RESTR 'RESTR-CALL)))
             (WHEN (%CATCH% (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
                       (FAIL NIL))
               (THROWFAIL "Some restriction not satisfied.")))))
    (LIST MATCHED-PLAN MATCHED-SUPPORT)))

;;; The checking function. 

(defun deduct-legal (P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
  (LET ((RULE-HUPPER NIL)
        (RULE-HLOWER NIL))
    (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER))
    (DEDUCT-LEGAL-HYPS P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
    (DEDUCT-LEGAL-WFFS P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
    T))

;;; The hypotheses checking function. 

(defun deduct-legal-hyps (P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
  (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (P3 D2 H1)) (UNQUOTED HYPARG (P3-HYPS D2-HYPS H1-HYPS)))
     (WHEN (AND (EXISTENT-P LINEARG) (NOT (SET-EQ HYPARG (HYPNUMS LINEARG))))
       (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                  " are not the same as the ones in the proof.")))
    (SETQ HUPPER (MEET-H HUPPER P3-HYPS))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF D2-HYPS (LIST H1))))
    (SETQ HLOWER (JOIN-H HLOWER (HYPSETDIFF H1-HYPS H1)))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$)) T
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))))
    (SETQ RULE-HUPPER HUPPER)
    (SETQ RULE-HLOWER HLOWER)
    T))

;;; The restriction checking function. 

(defun deduct-legal-wffs (P3 D2 H1 B A P3-HYPS D2-HYPS H1-HYPS)
  (DECLARE (SPECIAL RULE-HLOWER RULE-HUPPER) (IGNORE P3-HYPS D2-HYPS H1-HYPS))
  (LET ((WFFBINDINGS NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO ((QUOTED METAVAR (DEDUCT-MV1 DEDUCT-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (WHEN (NOT
       (SAME-MATCH-P METALABEL (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL)))
       (MISMATCH% (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL))))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (DEDUCT-P3 DEDUCT-D2 DEDUCT-H1))
      (UNQUOTED LINEARG (P3 D2 H1)))
     (WHEN (EXISTENT-P LINEARG)
       (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                   (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED RESTR NIL))
     (LET ((RSTR (GET RESTR 'RESTR-CALL)))
       (WHEN (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
         (THROWFAIL "Rule not applicable.  " T "Restrictions " (CAR RSTR)
                    " not satisfied.  "))))))


;;;
;;; Rule: MP
;;;

;;; The rule command definition.

(defmexpr mp
  (ARGTYPES LINE LINE LINE GWFF GWFF LINELIST LINELIST LINELIST)
  (WFFARGTYPES NIL NIL NIL "O" "O" NIL NIL NIL)
  (WFFOP-TYPELIST)
  (ARGNAMES D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
  (ARGHELP "Line with Implication" "Line with Succedent of Implication"
   "Line with Antecedent of Implication" "Succedent of Implication"
   "Antecedent of Implication" "Hypotheses" "Hypotheses" "Hypotheses")
  (DEFAULTFNS MP-DEFAULTS)
  (MAINFNS MP-LEGAL MP-BUILD)
  (ENTERFNS MP-ENTER)
  (MHELP "Modus Ponens."))

;;; The line assertions justifications and restrictions

(defrulewffs mp
  (UNIQUE-LINES 
     (MP-D2 "A(O) IMPLIES B(O)")
     (MP-D3 "B(O)")
     (MP-P1 "A(O)"))
  (UNIQUE-RESTRICTIONS ))

;;; The suggesting rule definition.

(defsrule mp
  (MATCHFN MP-MATCH))


;;; The building function. 

(defun mp-build (d2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
  (LET ((NEW-LINE-LABELS (LINE-LABEL-VEC 3))
        (WFFBINDINGS NIL)
        (NUM-ALIST NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO ((QUOTED METAVAR (MP-MV1 MP-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO
     ((QUOTED U-LINE (MP-D2 MP-D3 MP-P1)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL (META-SUBST (GET U-LINE 'META-ASSERTION)) 'ASSERTION))
    (MACRO-DO ((UNQUOTED LINE-ARG (D2 D3 P1)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL LINE-ARG 'LINENUMBER)
     (PUSH (CONS LINE-ARG LINE-LABEL) NUM-ALIST))
    (MACRO-DO
     ((UNQUOTED JUST
       ((NEXTPLAN)
        (LIST "MP" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST P1 D2)))
        (NEXTPLAN)))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL JUST 'JUSTIFICATION))
    (MACRO-DO ((UNQUOTED HYP-ARG (D2-HYPS D3-HYPS P1-HYPS)))
     (SETQ HYP-ARG (SUBST-LABELS NUM-ALIST HYP-ARG)))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (D2-HYPS D3-HYPS P1-HYPS))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL HYP-ARG 'HYPOTHESES))
    (APPEND NEW-LINE-LABELS (LIST B A D2-HYPS D3-HYPS P1-HYPS))))

;;; The entering function. 

(defun mp-enter (D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
  (DECLARE (IGNORE B A D2-HYPS D3-HYPS P1-HYPS))
  (UPDATE-PLAN (EVAL-DESTRUCT (('PP D2 'SS)))
   (EVAL-DESTRUCT ((P1 'SS) ('PP D3 'SS P1)))))

;;; The default function. 

(defun mp-defaults (D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
  (DECLARE (SPECIAL STRONG-DEFAULTLIST))
  (LET ((WFFBINDINGS NIL)
        STRONG-HYPDEFAULTS)
    (DECLARE (SPECIAL WFFBINDINGS STRONG-HYPDEFAULTS))
    (MACRO-DO ((QUOTED METAVAR (MP-MV1 MP-MV0)) (UNQUOTED WFFARG (B A)))
     (WHEN (SPECIFIED-P WFFARG)
       (PUSH (CONS METAVAR WFFARG) WFFBINDINGS)))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (MP-D2 MP-D3 MP-P1)) (UNQUOTED LINEARG (D2 D3 P1)))
     (IF (AND (NOT (EQ LINEARG '$)) (EXISTENT-P LINEARG))
         (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                     (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (LET ((WFFVAL (WFFEVAL METALABEL)))
       (WHEN (NOT WFFVAL)
         (%CATCH% (META-SUBST METALABEL) (FAIL NIL)))))
    (MACRO-DO ((QUOTED METAVAR (MP-MV1 MP-MV0)) (UNQUOTED WFFARG (B A)))
     (LET ((WFFVAL (WFFEVAL METAVAR)))
       (WHEN WFFVAL
         (SETQ WFFARG WFFVAL))))
    (WHEN (MEMBER '$ (LIST D2-HYPS D3-HYPS P1-HYPS))
      (IF (NOT (MEMBER '$ (LIST D2 D3 P1)))
          (SETQ-DESTRUCT (D2-HYPS D3-HYPS P1-HYPS)
           (MP-HYP-DEFAULTS D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS))
          (SETQ STRONG-HYPDEFAULTS
                (MAPCAR #'SPECIFIED-P (LIST D2-HYPS D3-HYPS P1-HYPS)))))
    (SETQ-DESTRUCT (('PP D2 'SS))
     (LINE-NO-DEFAULTS-FROM (EVAL-DESTRUCT (('PP D2 'SS)))))
    (WHEN (NOT (MEMBER '$ (LIST 'PP D2 'SS)))
      (SETQ-DESTRUCT-MULTI (P1) ((P1 'SS) ('PP D3 'SS P1))
       (LINE-NO-DEFAULTS-TO (EVAL-DESTRUCT (('PP D2 'SS)))
        (EVAL-DESTRUCT ((P1 'SS) ('PP D3 'SS P1))))))
    (SETQ STRONG-DEFAULTLIST
          (APPEND '(NIL NIL NIL) (MAPCAR #'SPECIFIED-P (LIST B A))
                  STRONG-HYPDEFAULTS))
    (LIST D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)))

;;; The hypotheses default function. 

(defun mp-hyp-defaults (D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
  (DECLARE (SPECIAL STRONG-HYPDEFAULTS) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (D2 D3 P1)) (UNQUOTED HYPARG (D2-HYPS D3-HYPS P1-HYPS)))
     (WHEN (EXISTENT-P LINEARG)
       (IF (SPECIFIED-P HYPARG)
           (WHEN (NOT (SET-EQ HYPARG (HYPNUMS LINEARG)))
             (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                        " are not the same as the one in the proof."))
           (SETQ HYPARG (HYPNUMS LINEARG)))))
    (WHEN (SPECIFIED-P D2-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF D2-HYPS (LIST)))))
    (WHEN (SPECIFIED-P D3-HYPS)
      (SETQ HUPPER (MEET-H HUPPER D3-HYPS)))
    (WHEN (SPECIFIED-P P1-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P1-HYPS (LIST)))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$))
        (SETQ STRONG-HYPDEFAULTS
              (MAPCAR #'SPECIFIED-P (LIST D2-HYPS D3-HYPS P1-HYPS)))
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))
          (WHEN (NOT AUTO-GENERATE-HYPS)
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST D2-HYPS D3-HYPS P1-HYPS))))
          (WHEN (NOT (SPECIFIED-P D2-HYPS))
            (SETQ D2-HYPS (ORDERED-JOIN-H HUPPER (LIST))))
          (WHEN (NOT (SPECIFIED-P D3-HYPS))
            (SETQ D3-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN (NOT (SPECIFIED-P P1-HYPS))
            (SETQ P1-HYPS (ORDERED-JOIN-H HUPPER (LIST))))
          (WHEN AUTO-GENERATE-HYPS
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST D2-HYPS D3-HYPS P1-HYPS))))))
    (LIST D2-HYPS D3-HYPS P1-HYPS)))

;;; The matching function. 

(defun mp-match (PLAN-SUPPORT)
  (LET ((WFFBINDINGS NIL)
        (MATCHED-PLAN)
        (MATCHED-SUPPORT))
    (DECLARE (SPECIAL WFFBINDINGS))
    (SETQ MATCHED-PLAN NIL)
    (SETQ MATCHED-SUPPORT
          (DO ((SUPPS (CDR PLAN-SUPPORT) (CDR SUPPS))
               (LEGAL-SUPPORTS NIL))
              ((NULL SUPPS)
               (IF (NULL LEGAL-SUPPORTS) (THROWFAIL "No support line matched.")
                   (NREVERSE LEGAL-SUPPORTS)))
            (LET ((WFFBINDINGS WFFBINDINGS))
              (DECLARE (SPECIAL WFFBINDINGS))
              (%CATCH%
               (PROGN
                 (MATCH-BIND (GET 'MP-D2 'META-ASSERTION)
                             (GET (CAR SUPPS) 'ASSERTION))
                 (MACRO-DO ((QUOTED RESTR NIL))
                  (LET ((RSTR (GET RESTR 'RESTR-CALL)))
                    (WHEN (%CATCH%
                      (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
                      (FAIL NIL))
                      (THROWFAIL NIL))))
                 (PUSH (CAR SUPPS) LEGAL-SUPPORTS))
               (FAIL NIL)))))
    (LIST MATCHED-PLAN MATCHED-SUPPORT)))

;;; The checking function. 

(defun mp-legal (D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
  (LET ((RULE-HUPPER NIL)
        (RULE-HLOWER NIL))
    (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER))
    (MP-LEGAL-HYPS D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
    (MP-LEGAL-WFFS D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
    T))

;;; The hypotheses checking function. 

(defun mp-legal-hyps (D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
  (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (D2 D3 P1)) (UNQUOTED HYPARG (D2-HYPS D3-HYPS P1-HYPS)))
     (WHEN (AND (EXISTENT-P LINEARG) (NOT (SET-EQ HYPARG (HYPNUMS LINEARG))))
       (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                  " are not the same as the ones in the proof.")))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF D2-HYPS (LIST))))
    (SETQ HUPPER (MEET-H HUPPER D3-HYPS))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P1-HYPS (LIST))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$)) T
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))))
    (SETQ RULE-HUPPER HUPPER)
    (SETQ RULE-HLOWER HLOWER)
    T))

;;; The restriction checking function. 

(defun mp-legal-wffs (D2 D3 P1 B A D2-HYPS D3-HYPS P1-HYPS)
  (DECLARE (SPECIAL RULE-HLOWER RULE-HUPPER) (IGNORE D2-HYPS D3-HYPS P1-HYPS))
  (LET ((WFFBINDINGS NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO ((QUOTED METAVAR (MP-MV1 MP-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (WHEN (NOT
       (SAME-MATCH-P METALABEL (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL)))
       (MISMATCH% (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL))))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (MP-D2 MP-D3 MP-P1)) (UNQUOTED LINEARG (D2 D3 P1)))
     (WHEN (EXISTENT-P LINEARG)
       (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                   (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED RESTR NIL))
     (LET ((RSTR (GET RESTR 'RESTR-CALL)))
       (WHEN (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
         (THROWFAIL "Rule not applicable.  " T "Restrictions " (CAR RSTR)
                    " not satisfied.  "))))))


;;;
;;; Rule: ECONJ
;;;

;;; The rule command definition.

(defmexpr econj
  (ARGTYPES LINE LINE LINE GWFF GWFF LINELIST LINELIST LINELIST)
  (WFFARGTYPES NIL NIL NIL "O" "O" NIL NIL NIL)
  (WFFOP-TYPELIST)
  (ARGNAMES D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
  (ARGHELP "Line with Conjunction" "Line with Right Conjunct"
   "Line with Left Conjunct" "Right Conjunct" "Left Conjunct" "Hypotheses"
   "Hypotheses" "Hypotheses")
  (DEFAULTFNS ECONJ-DEFAULTS)
  (MAINFNS ECONJ-LEGAL ECONJ-BUILD)
  (ENTERFNS ECONJ-ENTER)
  (MHELP "Rule to infer two conjuncts from a conjunction."))

;;; The line assertions justifications and restrictions

(defrulewffs econj
  (UNIQUE-LINES 
     (ECONJ-D1 "A(O) AND B(O)")
     (ECONJ-D3 "B(O)")
     (ECONJ-D2 "A(O)"))
  (UNIQUE-RESTRICTIONS ))

;;; The suggesting rule definition.

(defsrule econj
  (MATCHFN ECONJ-MATCH))


;;; The building function. 

(defun econj-build (D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
  (LET ((NEW-LINE-LABELS (LINE-LABEL-VEC 3))
        (WFFBINDINGS NIL)
        (NUM-ALIST NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO ((QUOTED METAVAR (ECONJ-MV1 ECONJ-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO
     ((QUOTED U-LINE (ECONJ-D1 ECONJ-D3 ECONJ-D2))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL (META-SUBST (GET U-LINE 'META-ASSERTION)) 'ASSERTION))
    (MACRO-DO ((UNQUOTED LINE-ARG (D1 D3 D2)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL LINE-ARG 'LINENUMBER)
     (PUSH (CONS LINE-ARG LINE-LABEL) NUM-ALIST))
    (MACRO-DO
     ((UNQUOTED JUST
       ((NEXTPLAN)
        (LIST "Conj" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST D1)))
        (LIST "Conj" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST D1)))))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL JUST 'JUSTIFICATION))
    (MACRO-DO ((UNQUOTED HYP-ARG (D1-HYPS D3-HYPS D2-HYPS)))
     (SETQ HYP-ARG (SUBST-LABELS NUM-ALIST HYP-ARG)))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (D1-HYPS D3-HYPS D2-HYPS))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL HYP-ARG 'HYPOTHESES))
    (APPEND NEW-LINE-LABELS (LIST B A D1-HYPS D3-HYPS D2-HYPS))))

;;; The entering function. 

(defun econj-enter (D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
  (DECLARE (IGNORE B A D1-HYPS D3-HYPS D2-HYPS))
  (UPDATE-PLAN (EVAL-DESTRUCT (('PP D1 'SS))) (EVAL-DESTRUCT (('PP D2 D3 'SS)))))

;;; The default function. 

(defun econj-defaults (D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
  (DECLARE (SPECIAL STRONG-DEFAULTLIST))
  (LET ((WFFBINDINGS NIL)
        STRONG-HYPDEFAULTS)
    (DECLARE (SPECIAL WFFBINDINGS STRONG-HYPDEFAULTS))
    (MACRO-DO ((QUOTED METAVAR (ECONJ-MV1 ECONJ-MV0)) (UNQUOTED WFFARG (B A)))
     (WHEN (SPECIFIED-P WFFARG)
       (PUSH (CONS METAVAR WFFARG) WFFBINDINGS)))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (ECONJ-D1 ECONJ-D3 ECONJ-D2))
      (UNQUOTED LINEARG (D1 D3 D2)))
     (IF (AND (NOT (EQ LINEARG '$)) (EXISTENT-P LINEARG))
         (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                     (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (LET ((WFFVAL (WFFEVAL METALABEL)))
       (WHEN (NOT WFFVAL)
         (%CATCH% (META-SUBST METALABEL) (FAIL NIL)))))
    (MACRO-DO ((QUOTED METAVAR (ECONJ-MV1 ECONJ-MV0)) (UNQUOTED WFFARG (B A)))
     (LET ((WFFVAL (WFFEVAL METAVAR)))
       (WHEN WFFVAL
         (SETQ WFFARG WFFVAL))))
    (WHEN (MEMBER '$ (LIST D1-HYPS D3-HYPS D2-HYPS))
      (IF (NOT (MEMBER '$ (LIST D1 D3 D2)))
          (SETQ-DESTRUCT (D1-HYPS D3-HYPS D2-HYPS)
           (ECONJ-HYP-DEFAULTS D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS))
          (SETQ STRONG-HYPDEFAULTS
                (MAPCAR #'SPECIFIED-P (LIST D1-HYPS D3-HYPS D2-HYPS)))))
    (SETQ-DESTRUCT (('PP D1 'SS))
     (LINE-NO-DEFAULTS-FROM (EVAL-DESTRUCT (('PP D1 'SS)))))
    (WHEN (NOT (MEMBER '$ (LIST 'PP D1 'SS)))
      (SETQ-DESTRUCT (('PP D2 D3 'SS))
       (LINE-NO-DEFAULTS-TO (EVAL-DESTRUCT (('PP D1 'SS)))
        (EVAL-DESTRUCT (('PP D2 D3 'SS))))))
    (SETQ STRONG-DEFAULTLIST
          (APPEND '(NIL NIL NIL) (MAPCAR #'SPECIFIED-P (LIST B A))
                  STRONG-HYPDEFAULTS))
    (LIST D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)))

;;; The hypotheses default function. 

(defun econj-hyp-defaults (D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
  (DECLARE (SPECIAL STRONG-HYPDEFAULTS) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (D1 D3 D2)) (UNQUOTED HYPARG (D1-HYPS D3-HYPS D2-HYPS)))
     (WHEN (EXISTENT-P LINEARG)
       (IF (SPECIFIED-P HYPARG)
           (WHEN (NOT (SET-EQ HYPARG (HYPNUMS LINEARG)))
             (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                        " are not the same as the one in the proof."))
           (SETQ HYPARG (HYPNUMS LINEARG)))))
    (WHEN (SPECIFIED-P D1-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF D1-HYPS (LIST)))))
    (WHEN (SPECIFIED-P D3-HYPS)
      (SETQ HUPPER (MEET-H HUPPER D3-HYPS)))
    (WHEN (SPECIFIED-P D2-HYPS)
      (SETQ HUPPER (MEET-H HUPPER D2-HYPS)))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$))
        (SETQ STRONG-HYPDEFAULTS
              (MAPCAR #'SPECIFIED-P (LIST D1-HYPS D3-HYPS D2-HYPS)))
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))
          (WHEN (NOT AUTO-GENERATE-HYPS)
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST D1-HYPS D3-HYPS D2-HYPS))))
          (WHEN (NOT (SPECIFIED-P D1-HYPS))
            (SETQ D1-HYPS (ORDERED-JOIN-H HUPPER (LIST))))
          (WHEN (NOT (SPECIFIED-P D3-HYPS))
            (SETQ D3-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN (NOT (SPECIFIED-P D2-HYPS))
            (SETQ D2-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN AUTO-GENERATE-HYPS
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST D1-HYPS D3-HYPS D2-HYPS))))))
    (LIST D1-HYPS D3-HYPS D2-HYPS)))

;;; The matching function. 

(defun econj-match (PLAN-SUPPORT)
  (LET ((WFFBINDINGS NIL)
        (MATCHED-PLAN)
        (MATCHED-SUPPORT))
    (DECLARE (SPECIAL WFFBINDINGS))
    (SETQ MATCHED-PLAN NIL)
    (SETQ MATCHED-SUPPORT
          (DO ((SUPPS (CDR PLAN-SUPPORT) (CDR SUPPS))
               (LEGAL-SUPPORTS NIL))
              ((NULL SUPPS)
               (IF (NULL LEGAL-SUPPORTS) (THROWFAIL "No support line matched.")
                   (NREVERSE LEGAL-SUPPORTS)))
            (LET ((WFFBINDINGS WFFBINDINGS))
              (DECLARE (SPECIAL WFFBINDINGS))
              (%CATCH%
               (PROGN
                 (MATCH-BIND (GET 'ECONJ-D1 'META-ASSERTION)
                             (GET (CAR SUPPS) 'ASSERTION))
                 (MACRO-DO ((QUOTED RESTR NIL))
                  (LET ((RSTR (GET RESTR 'RESTR-CALL)))
                    (WHEN (%CATCH%
                      (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
                      (FAIL NIL))
                      (THROWFAIL NIL))))
                 (PUSH (CAR SUPPS) LEGAL-SUPPORTS))
               (FAIL NIL)))))
    (LIST MATCHED-PLAN MATCHED-SUPPORT)))

;;; The checking function. 

(defun econj-legal (D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
  (LET ((RULE-HUPPER NIL)
        (RULE-HLOWER NIL))
    (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER))
    (ECONJ-LEGAL-HYPS D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
    (ECONJ-LEGAL-WFFS D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
    T))

;;; The hypotheses checking function. 

(defun econj-legal-hyps (D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
  (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (D1 D3 D2)) (UNQUOTED HYPARG (D1-HYPS D3-HYPS D2-HYPS)))
     (WHEN (AND (EXISTENT-P LINEARG) (NOT (SET-EQ HYPARG (HYPNUMS LINEARG))))
       (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                  " are not the same as the ones in the proof.")))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF D1-HYPS (LIST))))
    (SETQ HUPPER (MEET-H HUPPER D3-HYPS))
    (SETQ HUPPER (MEET-H HUPPER D2-HYPS))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$)) T
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))))
    (SETQ RULE-HUPPER HUPPER)
    (SETQ RULE-HLOWER HLOWER)
    T))

;;; The restriction checking function. 

(defun econj-legal-wffs (D1 D3 D2 B A D1-HYPS D3-HYPS D2-HYPS)
  (DECLARE (SPECIAL RULE-HLOWER RULE-HUPPER) (IGNORE D1-HYPS D3-HYPS D2-HYPS))
  (LET ((WFFBINDINGS NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO ((QUOTED METAVAR (ECONJ-MV1 ECONJ-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (WHEN (NOT
       (SAME-MATCH-P METALABEL (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL)))
       (MISMATCH% (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL))))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (ECONJ-D1 ECONJ-D3 ECONJ-D2))
      (UNQUOTED LINEARG (D1 D3 D2)))
     (WHEN (EXISTENT-P LINEARG)
       (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                   (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED RESTR NIL))
     (LET ((RSTR (GET RESTR 'RESTR-CALL)))
       (WHEN (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
         (THROWFAIL "Rule not applicable.  " T "Restrictions " (CAR RSTR)
                    " not satisfied.  "))))))


;;;
;;; Rule: ICONJ
;;;

;;; The rule command definition.

(defmexpr iconj
  (ARGTYPES LINE LINE LINE GWFF GWFF LINELIST LINELIST LINELIST)
  (WFFARGTYPES NIL NIL NIL "O" "O" NIL NIL NIL)
  (WFFOP-TYPELIST)
  (ARGNAMES P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
  (ARGHELP "Line with Conjunction" "Line with Right Conjunct"
   "Line with Left Conjunct" "Right Conjunct" "Left Conjunct" "Hypotheses"
   "Hypotheses" "Hypotheses")
  (DEFAULTFNS ICONJ-DEFAULTS)
  (MAINFNS ICONJ-LEGAL ICONJ-BUILD)
  (ENTERFNS ICONJ-ENTER)
  (MHELP "Rule to infer a conjunction from two conjuncts."))

;;; The line assertions justifications and restrictions

(defrulewffs iconj
  (UNIQUE-LINES 
     (ICONJ-P3 "A(O) AND B(O)")
     (ICONJ-P2 "B(O)")
     (ICONJ-P1 "A(O)"))
  (UNIQUE-RESTRICTIONS ))

;;; The suggesting rule definition.

(defsrule iconj
  (MATCHFN ICONJ-MATCH))


;;; The building function. 

(defun iconj-build (P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
  (LET ((NEW-LINE-LABELS (LINE-LABEL-VEC 3))
        (WFFBINDINGS NIL)
        (NUM-ALIST NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO ((QUOTED METAVAR (ICONJ-MV1 ICONJ-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO
     ((QUOTED U-LINE (ICONJ-P3 ICONJ-P2 ICONJ-P1))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL (META-SUBST (GET U-LINE 'META-ASSERTION)) 'ASSERTION))
    (MACRO-DO ((UNQUOTED LINE-ARG (P3 P2 P1)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL LINE-ARG 'LINENUMBER)
     (PUSH (CONS LINE-ARG LINE-LABEL) NUM-ALIST))
    (MACRO-DO
     ((UNQUOTED JUST
       ((LIST "Conj" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST P1 P2)))
        (NEXTPLAN) (NEXTPLAN)))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL JUST 'JUSTIFICATION))
    (MACRO-DO ((UNQUOTED HYP-ARG (P3-HYPS P2-HYPS P1-HYPS)))
     (SETQ HYP-ARG (SUBST-LABELS NUM-ALIST HYP-ARG)))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (P3-HYPS P2-HYPS P1-HYPS))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL HYP-ARG 'HYPOTHESES))
    (APPEND NEW-LINE-LABELS (LIST B A P3-HYPS P2-HYPS P1-HYPS))))

;;; The entering function. 

(defun iconj-enter (P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
  (DECLARE (IGNORE B A P3-HYPS P2-HYPS P1-HYPS))
  (UPDATE-PLAN (EVAL-DESTRUCT ((P3 'SS))) (EVAL-DESTRUCT ((P1 'SS) (P2 'SS)))))

;;; The default function. 

(defun iconj-defaults (P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL STRONG-DEFAULTLIST))
  (LET ((WFFBINDINGS NIL)
        STRONG-HYPDEFAULTS)
    (DECLARE (SPECIAL WFFBINDINGS STRONG-HYPDEFAULTS))
    (MACRO-DO ((QUOTED METAVAR (ICONJ-MV1 ICONJ-MV0)) (UNQUOTED WFFARG (B A)))
     (WHEN (SPECIFIED-P WFFARG)
       (PUSH (CONS METAVAR WFFARG) WFFBINDINGS)))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (ICONJ-P3 ICONJ-P2 ICONJ-P1))
      (UNQUOTED LINEARG (P3 P2 P1)))
     (IF (AND (NOT (EQ LINEARG '$)) (EXISTENT-P LINEARG))
         (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                     (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (LET ((WFFVAL (WFFEVAL METALABEL)))
       (WHEN (NOT WFFVAL)
         (%CATCH% (META-SUBST METALABEL) (FAIL NIL)))))
    (MACRO-DO ((QUOTED METAVAR (ICONJ-MV1 ICONJ-MV0)) (UNQUOTED WFFARG (B A)))
     (LET ((WFFVAL (WFFEVAL METAVAR)))
       (WHEN WFFVAL
         (SETQ WFFARG WFFVAL))))
    (WHEN (MEMBER '$ (LIST P3-HYPS P2-HYPS P1-HYPS))
      (IF (NOT (MEMBER '$ (LIST P3 P2 P1)))
          (SETQ-DESTRUCT (P3-HYPS P2-HYPS P1-HYPS)
           (ICONJ-HYP-DEFAULTS P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS))
          (SETQ STRONG-HYPDEFAULTS
                (MAPCAR #'SPECIFIED-P (LIST P3-HYPS P2-HYPS P1-HYPS)))))
    (SETQ-DESTRUCT ((P3 'SS)) (LINE-NO-DEFAULTS-FROM (EVAL-DESTRUCT ((P3 'SS)))))
    (WHEN (NOT (MEMBER '$ (LIST P3 'SS)))
      (SETQ-DESTRUCT ((P1 'SS) (P2 'SS))
       (LINE-NO-DEFAULTS-TO (EVAL-DESTRUCT ((P3 'SS)))
        (EVAL-DESTRUCT ((P1 'SS) (P2 'SS))))))
    (SETQ STRONG-DEFAULTLIST
          (APPEND '(NIL NIL NIL) (MAPCAR #'SPECIFIED-P (LIST B A))
                  STRONG-HYPDEFAULTS))
    (LIST P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)))

;;; The hypotheses default function. 

(defun iconj-hyp-defaults (P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL STRONG-HYPDEFAULTS) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (P3 P2 P1)) (UNQUOTED HYPARG (P3-HYPS P2-HYPS P1-HYPS)))
     (WHEN (EXISTENT-P LINEARG)
       (IF (SPECIFIED-P HYPARG)
           (WHEN (NOT (SET-EQ HYPARG (HYPNUMS LINEARG)))
             (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                        " are not the same as the one in the proof."))
           (SETQ HYPARG (HYPNUMS LINEARG)))))
    (WHEN (SPECIFIED-P P3-HYPS)
      (SETQ HUPPER (MEET-H HUPPER P3-HYPS)))
    (WHEN (SPECIFIED-P P2-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P2-HYPS (LIST)))))
    (WHEN (SPECIFIED-P P1-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P1-HYPS (LIST)))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$))
        (SETQ STRONG-HYPDEFAULTS
              (MAPCAR #'SPECIFIED-P (LIST P3-HYPS P2-HYPS P1-HYPS)))
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))
          (WHEN (NOT AUTO-GENERATE-HYPS)
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P3-HYPS P2-HYPS P1-HYPS))))
          (WHEN (NOT (SPECIFIED-P P3-HYPS))
            (SETQ P3-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN (NOT (SPECIFIED-P P2-HYPS))
            (SETQ P2-HYPS (ORDERED-JOIN-H HUPPER (LIST))))
          (WHEN (NOT (SPECIFIED-P P1-HYPS))
            (SETQ P1-HYPS (ORDERED-JOIN-H HUPPER (LIST))))
          (WHEN AUTO-GENERATE-HYPS
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P3-HYPS P2-HYPS P1-HYPS))))))
    (LIST P3-HYPS P2-HYPS P1-HYPS)))

;;; The matching function. 

(defun iconj-match (plan-support)
  (LET ((WFFBINDINGS NIL)
        (MATCHED-PLAN)
        (MATCHED-SUPPORT))
    (DECLARE (SPECIAL WFFBINDINGS))
    (SETQ MATCHED-PLAN
          (PROGN
            (%CATCH%
             (MATCH-BIND (GET 'ICONJ-P3 'META-ASSERTION)
                         (GET (CAR PLAN-SUPPORT) 'ASSERTION))
             (FAIL (THROWFAIL "Planned lines did not match.")))
            (LIST (CAR PLAN-SUPPORT))))
    (SETQ MATCHED-SUPPORT
          (MACRO-DO ((QUOTED RESTR NIL))
           (LET ((RSTR (GET RESTR 'RESTR-CALL)))
             (WHEN (%CATCH% (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
                       (FAIL NIL))
               (THROWFAIL "Some restriction not satisfied.")))))
    (LIST MATCHED-PLAN MATCHED-SUPPORT)))

;;; The checking function. 

(defun iconj-legal (P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
  (LET ((RULE-HUPPER NIL)
        (RULE-HLOWER NIL))
    (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER))
    (ICONJ-LEGAL-HYPS P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
    (ICONJ-LEGAL-WFFS P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
    T))

;;; The hypotheses checking function. 

(defun iconj-legal-hyps (P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (P3 P2 P1)) (UNQUOTED HYPARG (P3-HYPS P2-HYPS P1-HYPS)))
     (WHEN (AND (EXISTENT-P LINEARG) (NOT (SET-EQ HYPARG (HYPNUMS LINEARG))))
       (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                  " are not the same as the ones in the proof.")))
    (SETQ HUPPER (MEET-H HUPPER P3-HYPS))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P2-HYPS (LIST))))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P1-HYPS (LIST))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$)) T
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))))
    (SETQ RULE-HUPPER HUPPER)
    (SETQ RULE-HLOWER HLOWER)
    T))

;;; The restriction checking function. 

(defun iconj-legal-wffs (P3 P2 P1 B A P3-HYPS P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL RULE-HLOWER RULE-HUPPER) (IGNORE P3-HYPS P2-HYPS P1-HYPS))
  (LET ((WFFBINDINGS NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO ((QUOTED METAVAR (ICONJ-MV1 ICONJ-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (WHEN (NOT
       (SAME-MATCH-P METALABEL (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL)))
       (MISMATCH% (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL))))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (ICONJ-P3 ICONJ-P2 ICONJ-P1))
      (UNQUOTED LINEARG (P3 P2 P1)))
     (WHEN (EXISTENT-P LINEARG)
       (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                   (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED RESTR NIL))
     (LET ((RSTR (GET RESTR 'RESTR-CALL)))
       (WHEN (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
         (THROWFAIL "Rule not applicable.  " T "Restrictions " (CAR RSTR)
                    " not satisfied.  "))))))


;;;
;;; Rule: CASES
;;;

;;; The rule command definition.

(defmexpr cases
  (ARGTYPES LINE LINE LINE LINE LINE LINE GWFF GWFF GWFF LINELIST LINELIST
   LINELIST LINELIST LINELIST LINELIST)
  (WFFARGTYPES NIL NIL NIL NIL NIL NIL "O" "O" "O" NIL NIL NIL NIL NIL NIL)
  (WFFOP-TYPELIST)
  (ARGNAMES P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS
   H2-HYPS)
  (ARGHELP "Conclusion for Both Cases" "Line with Disjunction"
   "Conclusion in Case 2" "Line with Assumption for Case 2 (Right Disjunct)"
   "Conclusion in Case 1" "Line with Assumption for Case 1 (Left Disjunct)"
   "Right Disjunct" "Left Disjunct" "Conclusion" "Hypotheses" "Hypotheses"
   "Hypotheses" "Hypotheses" "Hypotheses" "Hypotheses")
  (DEFAULTFNS CASES-DEFAULTS)
  (MAINFNS CASES-LEGAL CASES-BUILD)
  (ENTERFNS CASES-ENTER)
  (MHELP "Rule of Cases"))

;;; The line assertions justifications and restrictions

(defrulewffs cases
  (UNIQUE-LINES 
     (CASES-P6 "C(O)")
     (CASES-D1 "A(O) OR B(O)")
     (CASES-P5 "C(O)")
     (CASES-H4 "B(O)")
     (CASES-P3 "C(O)")
     (CASES-H2 "A(O)"))
  (UNIQUE-RESTRICTIONS ))

;;; The suggesting rule definition.

(defsrule cases
  (MATCHFN CASES-MATCH))


;;; The building function. 

(defun cases-build (P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)
  (LET ((NEW-LINE-LABELS (LINE-LABEL-VEC 6))
        (WFFBINDINGS NIL)
        (NUM-ALIST NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO
     ((QUOTED METAVAR (CASES-MV2 CASES-MV1 CASES-MV0)) (UNQUOTED WFFARG (B A C)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO
     ((QUOTED U-LINE (CASES-P6 CASES-D1 CASES-P5 CASES-H4 CASES-P3 CASES-H2))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL (META-SUBST (GET U-LINE 'META-ASSERTION)) 'ASSERTION))
    (MACRO-DO
     ((UNQUOTED LINE-ARG (P6 D1 P5 H4 P3 H2)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL LINE-ARG 'LINENUMBER)
     (PUSH (CONS LINE-ARG LINE-LABEL) NUM-ALIST))
    (MACRO-DO
     ((UNQUOTED JUST
       ((LIST "Cases" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST D1 P3 P5)))
        (NEXTPLAN) (NEXTPLAN)
        (LIST "Case 2" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST D1)))
        (NEXTPLAN)
        (LIST "Case 1" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST D1)))))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL JUST 'JUSTIFICATION))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)))
     (SETQ HYP-ARG (SUBST-LABELS NUM-ALIST HYP-ARG)))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL HYP-ARG 'HYPOTHESES))
    (APPEND NEW-LINE-LABELS
            (LIST B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS))))

;;; The entering function. 

(defun cases-enter (P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)
  (DECLARE (IGNORE B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS))
  (UPDATE-PLAN (EVAL-DESTRUCT ((P6 D1 'SS)))
   (EVAL-DESTRUCT ((P3 H2 'SS) (P5 H4 'SS)))))

;;; The default function. 

(defun cases-defaults (P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)
  (DECLARE (SPECIAL STRONG-DEFAULTLIST))
  (LET ((WFFBINDINGS NIL)
        STRONG-HYPDEFAULTS)
    (DECLARE (SPECIAL WFFBINDINGS STRONG-HYPDEFAULTS))
    (MACRO-DO
     ((QUOTED METAVAR (CASES-MV2 CASES-MV1 CASES-MV0)) (UNQUOTED WFFARG (B A C)))
     (WHEN (SPECIFIED-P WFFARG)
       (PUSH (CONS METAVAR WFFARG) WFFBINDINGS)))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (CASES-P6 CASES-D1 CASES-P5 CASES-H4 CASES-P3 CASES-H2))
      (UNQUOTED LINEARG (P6 D1 P5 H4 P3 H2)))
     (IF (AND (NOT (EQ LINEARG '$)) (EXISTENT-P LINEARG))
         (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                     (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (LET ((WFFVAL (WFFEVAL METALABEL)))
       (WHEN (NOT WFFVAL)
         (%CATCH% (META-SUBST METALABEL) (FAIL NIL)))))
    (MACRO-DO
     ((QUOTED METAVAR (CASES-MV2 CASES-MV1 CASES-MV0)) (UNQUOTED WFFARG (B A C)))
     (LET ((WFFVAL (WFFEVAL METAVAR)))
       (WHEN WFFVAL
         (SETQ WFFARG WFFVAL))))
    (WHEN (MEMBER '$ (LIST P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS))
      (IF (NOT (MEMBER '$ (LIST P6 D1 P5 H4 P3 H2)))
          (SETQ-DESTRUCT (P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)
           (CASES-HYP-DEFAULTS P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS
            H4-HYPS P3-HYPS H2-HYPS))
          (SETQ STRONG-HYPDEFAULTS
                (MAPCAR #'SPECIFIED-P
                        (LIST P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)))))
    (SETQ-DESTRUCT ((P6 D1 'SS))
     (LINE-NO-DEFAULTS-FROM (EVAL-DESTRUCT ((P6 D1 'SS)))))
    (WHEN (NOT (MEMBER '$ (LIST P6 D1 'SS)))
      (SETQ-DESTRUCT ((P3 H2 'SS) (P5 H4 'SS))
       (LINE-NO-DEFAULTS-TO (EVAL-DESTRUCT ((P6 D1 'SS)))
        (EVAL-DESTRUCT ((P3 H2 'SS) (P5 H4 'SS))))))
    (SETQ STRONG-DEFAULTLIST
          (APPEND '(NIL NIL NIL NIL NIL NIL) (MAPCAR #'SPECIFIED-P (LIST B A C))
                  STRONG-HYPDEFAULTS))
    (LIST P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)))

;;; The hypotheses default function. 

(defun cases-hyp-defaults (P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)
  (DECLARE (SPECIAL STRONG-HYPDEFAULTS) (IGNORE B A C))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (P6 D1 P5 H4 P3 H2))
      (UNQUOTED HYPARG (P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)))
     (WHEN (EXISTENT-P LINEARG)
       (IF (SPECIFIED-P HYPARG)
           (WHEN (NOT (SET-EQ HYPARG (HYPNUMS LINEARG)))
             (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                        " are not the same as the one in the proof."))
           (SETQ HYPARG (HYPNUMS LINEARG)))))
    (WHEN (SPECIFIED-P P6-HYPS)
      (SETQ HUPPER (MEET-H HUPPER P6-HYPS)))
    (WHEN (SPECIFIED-P D1-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF D1-HYPS (LIST)))))
    (WHEN (SPECIFIED-P P5-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P5-HYPS (LIST H4)))))
    (WHEN (SPECIFIED-P H4-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF H4-HYPS (LIST H4)))))
    (WHEN (SPECIFIED-P P3-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P3-HYPS (LIST H2)))))
    (WHEN (SPECIFIED-P H2-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF H2-HYPS (LIST H2)))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$))
        (SETQ STRONG-HYPDEFAULTS
              (MAPCAR #'SPECIFIED-P
                      (LIST P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)))
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))
          (WHEN (NOT AUTO-GENERATE-HYPS)
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P
                          (LIST P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS))))
          (WHEN (NOT (SPECIFIED-P P6-HYPS))
            (SETQ P6-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN (NOT (SPECIFIED-P D1-HYPS))
            (SETQ D1-HYPS (ORDERED-JOIN-H HUPPER (LIST))))
          (WHEN (NOT (SPECIFIED-P P5-HYPS))
            (SETQ P5-HYPS (ORDERED-JOIN-H HUPPER (LIST H4))))
          (WHEN (NOT (SPECIFIED-P H4-HYPS))
            (SETQ H4-HYPS (ORDERED-JOIN-H HUPPER (LIST H4))))
          (WHEN (NOT (SPECIFIED-P P3-HYPS))
            (SETQ P3-HYPS (ORDERED-JOIN-H HUPPER (LIST H2))))
          (WHEN (NOT (SPECIFIED-P H2-HYPS))
            (SETQ H2-HYPS (ORDERED-JOIN-H HUPPER (LIST H2))))
          (WHEN AUTO-GENERATE-HYPS
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P
                          (LIST P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS))))))
    (LIST P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)))

;;; The matching function. 

(defun cases-match (PLAN-SUPPORT)
  (LET ((WFFBINDINGS NIL)
        (MATCHED-PLAN)
        (MATCHED-SUPPORT))
    (DECLARE (SPECIAL WFFBINDINGS))
    (SETQ MATCHED-PLAN
          (PROGN
            (%CATCH%
             (MATCH-BIND (GET 'CASES-P6 'META-ASSERTION)
                         (GET (CAR PLAN-SUPPORT) 'ASSERTION))
             (FAIL (THROWFAIL "Planned lines did not match.")))
            (LIST (CAR PLAN-SUPPORT))))
    (SETQ MATCHED-SUPPORT
          (DO ((SUPPS (CDR PLAN-SUPPORT) (CDR SUPPS))
               (LEGAL-SUPPORTS NIL))
              ((NULL SUPPS)
               (IF (NULL LEGAL-SUPPORTS) (THROWFAIL "No support line matched.")
                   (NREVERSE LEGAL-SUPPORTS)))
            (LET ((WFFBINDINGS WFFBINDINGS))
              (DECLARE (SPECIAL WFFBINDINGS))
              (%CATCH%
               (PROGN
                 (MATCH-BIND (GET 'CASES-D1 'META-ASSERTION)
                             (GET (CAR SUPPS) 'ASSERTION))
                 (MACRO-DO ((QUOTED RESTR NIL))
                  (LET ((RSTR (GET RESTR 'RESTR-CALL)))
                    (WHEN (%CATCH%
                      (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
                      (FAIL NIL))
                      (THROWFAIL NIL))))
                 (PUSH (CAR SUPPS) LEGAL-SUPPORTS))
               (FAIL NIL)))))
    (LIST MATCHED-PLAN MATCHED-SUPPORT)))

;;; The checking function. 

(defun cases-legal (P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)
  (LET ((RULE-HUPPER NIL)
        (RULE-HLOWER NIL))
    (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER))
    (CASES-LEGAL-HYPS P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS
     P3-HYPS H2-HYPS)
    (CASES-LEGAL-WFFS P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS
     P3-HYPS H2-HYPS)
    T))

;;; The hypotheses checking function. 

(defun cases-legal-hyps (P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)
  (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER) (IGNORE B A C))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (P6 D1 P5 H4 P3 H2))
      (UNQUOTED HYPARG (P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)))
     (WHEN (AND (EXISTENT-P LINEARG) (NOT (SET-EQ HYPARG (HYPNUMS LINEARG))))
       (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                  " are not the same as the ones in the proof.")))
    (SETQ HUPPER (MEET-H HUPPER P6-HYPS))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF D1-HYPS (LIST))))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P5-HYPS (LIST H4))))
    (SETQ HLOWER (JOIN-H HLOWER (HYPSETDIFF H4-HYPS H4)))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P3-HYPS (LIST H2))))
    (SETQ HLOWER (JOIN-H HLOWER (HYPSETDIFF H2-HYPS H2)))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$)) T
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))))
    (SETQ RULE-HUPPER HUPPER)
    (SETQ RULE-HLOWER HLOWER)
    T))

;;; The restriction checking function. 

(defun cases-legal-wffs (P6 D1 P5 H4 P3 H2 B A C P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS)
  (DECLARE (SPECIAL RULE-HLOWER RULE-HUPPER)
           (IGNORE P6-HYPS D1-HYPS P5-HYPS H4-HYPS P3-HYPS H2-HYPS))
  (LET ((WFFBINDINGS NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO
     ((QUOTED METAVAR (CASES-MV2 CASES-MV1 CASES-MV0)) (UNQUOTED WFFARG (B A C)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (WHEN (NOT
       (SAME-MATCH-P METALABEL (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL)))
       (MISMATCH% (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL))))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (CASES-P6 CASES-D1 CASES-P5 CASES-H4 CASES-P3 CASES-H2))
      (UNQUOTED LINEARG (P6 D1 P5 H4 P3 H2)))
     (WHEN (EXISTENT-P LINEARG)
       (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                   (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED RESTR NIL))
     (LET ((RSTR (GET RESTR 'RESTR-CALL)))
       (WHEN (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
         (THROWFAIL "Rule not applicable.  " T "Restrictions " (CAR RSTR)
                    " not satisfied.  "))))))


;;;
;;; Rule: IDISJ-RIGHT
;;;

;;; The rule command definition.

(defmexpr idisj-right
  (ARGTYPES LINE LINE GWFF GWFF LINELIST LINELIST)
  (WFFARGTYPES NIL NIL "O" "O" NIL NIL)
  (WFFOP-TYPELIST)
  (ARGNAMES P2 P1 B A P2-HYPS P1-HYPS)
  (ARGHELP "Line with Disjunction" "Line with Left Disjunct" "Right Disjunct"
   "Left Disjunct" "Hypotheses" "Hypotheses")
  (DEFAULTFNS IDISJ-RIGHT-DEFAULTS)
  (MAINFNS IDISJ-RIGHT-LEGAL IDISJ-RIGHT-BUILD)
  (ENTERFNS IDISJ-RIGHT-ENTER)
  (MHELP "Infer a disjunction from left disjunct."))

;;; The line assertions justifications and restrictions

(defrulewffs idisj-right
  (UNIQUE-LINES 
     (IDISJ-RIGHT-P2 "A(O) OR B(O)")
     (IDISJ-RIGHT-P1 "A(O)"))
  (UNIQUE-RESTRICTIONS ))

;;; The suggesting rule definition.

(defsrule idisj-right
  (MATCHFN IDISJ-RIGHT-MATCH))


;;; The building function. 

(defun idisj-right-build (P2 P1 B A P2-HYPS P1-HYPS)
  (LET ((NEW-LINE-LABELS (LINE-LABEL-VEC 2))
        (WFFBINDINGS NIL)
        (NUM-ALIST NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO
     ((QUOTED METAVAR (IDISJ-RIGHT-MV1 IDISJ-RIGHT-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO
     ((QUOTED U-LINE (IDISJ-RIGHT-P2 IDISJ-RIGHT-P1))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL (META-SUBST (GET U-LINE 'META-ASSERTION)) 'ASSERTION))
    (MACRO-DO ((UNQUOTED LINE-ARG (P2 P1)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL LINE-ARG 'LINENUMBER)
     (PUSH (CONS LINE-ARG LINE-LABEL) NUM-ALIST))
    (MACRO-DO
     ((UNQUOTED JUST
       ((LIST "Disj" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST P1)))
        (NEXTPLAN)))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL JUST 'JUSTIFICATION))
    (MACRO-DO ((UNQUOTED HYP-ARG (P2-HYPS P1-HYPS)))
     (SETQ HYP-ARG (SUBST-LABELS NUM-ALIST HYP-ARG)))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (P2-HYPS P1-HYPS)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL HYP-ARG 'HYPOTHESES))
    (APPEND NEW-LINE-LABELS (LIST B A P2-HYPS P1-HYPS))))

;;; The entering function. 

(defun idisj-right-enter (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (IGNORE B A P2-HYPS P1-HYPS))
  (UPDATE-PLAN (EVAL-DESTRUCT ((P2 'SS))) (EVAL-DESTRUCT ((P1 'SS)))))

;;; The default function. 

(defun idisj-right-defaults (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL STRONG-DEFAULTLIST))
  (LET ((WFFBINDINGS NIL)
        STRONG-HYPDEFAULTS)
    (DECLARE (SPECIAL WFFBINDINGS STRONG-HYPDEFAULTS))
    (MACRO-DO
     ((QUOTED METAVAR (IDISJ-RIGHT-MV1 IDISJ-RIGHT-MV0)) (UNQUOTED WFFARG (B A)))
     (WHEN (SPECIFIED-P WFFARG)
       (PUSH (CONS METAVAR WFFARG) WFFBINDINGS)))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (IDISJ-RIGHT-P2 IDISJ-RIGHT-P1))
      (UNQUOTED LINEARG (P2 P1)))
     (IF (AND (NOT (EQ LINEARG '$)) (EXISTENT-P LINEARG))
         (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                     (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (LET ((WFFVAL (WFFEVAL METALABEL)))
       (WHEN (NOT WFFVAL)
         (%CATCH% (META-SUBST METALABEL) (FAIL NIL)))))
    (MACRO-DO
     ((QUOTED METAVAR (IDISJ-RIGHT-MV1 IDISJ-RIGHT-MV0)) (UNQUOTED WFFARG (B A)))
     (LET ((WFFVAL (WFFEVAL METAVAR)))
       (WHEN WFFVAL
         (SETQ WFFARG WFFVAL))))
    (WHEN (MEMBER '$ (LIST P2-HYPS P1-HYPS))
      (IF (NOT (MEMBER '$ (LIST P2 P1)))
          (SETQ-DESTRUCT (P2-HYPS P1-HYPS)
           (IDISJ-RIGHT-HYP-DEFAULTS P2 P1 B A P2-HYPS P1-HYPS))
          (SETQ STRONG-HYPDEFAULTS (MAPCAR #'SPECIFIED-P (LIST P2-HYPS P1-HYPS)))))
    (SETQ-DESTRUCT ((P2 'SS)) (LINE-NO-DEFAULTS-FROM (EVAL-DESTRUCT ((P2 'SS)))))
    (WHEN (NOT (MEMBER '$ (LIST P2 'SS)))
      (SETQ-DESTRUCT ((P1 'SS))
       (LINE-NO-DEFAULTS-TO (EVAL-DESTRUCT ((P2 'SS))) (EVAL-DESTRUCT ((P1 'SS))))))
    (SETQ STRONG-DEFAULTLIST
          (APPEND '(NIL NIL) (MAPCAR #'SPECIFIED-P (LIST B A)) STRONG-HYPDEFAULTS))
    (LIST P2 P1 B A P2-HYPS P1-HYPS)))

;;; The hypotheses default function. 

(defun idisj-right-hyp-defaults (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL STRONG-HYPDEFAULTS) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO ((UNQUOTED LINEARG (P2 P1)) (UNQUOTED HYPARG (P2-HYPS P1-HYPS)))
     (WHEN (EXISTENT-P LINEARG)
       (IF (SPECIFIED-P HYPARG)
           (WHEN (NOT (SET-EQ HYPARG (HYPNUMS LINEARG)))
             (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                        " are not the same as the one in the proof."))
           (SETQ HYPARG (HYPNUMS LINEARG)))))
    (WHEN (SPECIFIED-P P2-HYPS)
      (SETQ HUPPER (MEET-H HUPPER P2-HYPS)))
    (WHEN (SPECIFIED-P P1-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P1-HYPS (LIST)))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$))
        (SETQ STRONG-HYPDEFAULTS (MAPCAR #'SPECIFIED-P (LIST P2-HYPS P1-HYPS)))
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))
          (WHEN (NOT AUTO-GENERATE-HYPS)
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P2-HYPS P1-HYPS))))
          (WHEN (NOT (SPECIFIED-P P2-HYPS))
            (SETQ P2-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN (NOT (SPECIFIED-P P1-HYPS))
            (SETQ P1-HYPS (ORDERED-JOIN-H HUPPER (LIST))))
          (WHEN AUTO-GENERATE-HYPS
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P2-HYPS P1-HYPS))))))
    (LIST P2-HYPS P1-HYPS)))

;;; The matching function. 

(defun idisj-right-match (PLAN-SUPPORT)
  (LET ((WFFBINDINGS NIL)
        (MATCHED-PLAN)
        (MATCHED-SUPPORT))
    (DECLARE (SPECIAL WFFBINDINGS))
    (SETQ MATCHED-PLAN
          (PROGN
            (%CATCH%
             (MATCH-BIND (GET 'IDISJ-RIGHT-P2 'META-ASSERTION)
                         (GET (CAR PLAN-SUPPORT) 'ASSERTION))
             (FAIL (THROWFAIL "Planned lines did not match.")))
            (LIST (CAR PLAN-SUPPORT))))
    (SETQ MATCHED-SUPPORT
          (MACRO-DO ((QUOTED RESTR NIL))
           (LET ((RSTR (GET RESTR 'RESTR-CALL)))
             (WHEN (%CATCH% (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
                       (FAIL NIL))
               (THROWFAIL "Some restriction not satisfied.")))))
    (LIST MATCHED-PLAN MATCHED-SUPPORT)))

;;; The checking function. 

(defun idisj-right-legal (P2 P1 B A P2-HYPS P1-HYPS)
  (LET ((RULE-HUPPER NIL)
        (RULE-HLOWER NIL))
    (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER))
    (IDISJ-RIGHT-LEGAL-HYPS P2 P1 B A P2-HYPS P1-HYPS)
    (IDISJ-RIGHT-LEGAL-WFFS P2 P1 B A P2-HYPS P1-HYPS)
    T))

;;; The hypotheses checking function. 

(defun idisj-right-legal-hyps (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO ((UNQUOTED LINEARG (P2 P1)) (UNQUOTED HYPARG (P2-HYPS P1-HYPS)))
     (WHEN (AND (EXISTENT-P LINEARG) (NOT (SET-EQ HYPARG (HYPNUMS LINEARG))))
       (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                  " are not the same as the ones in the proof.")))
    (SETQ HUPPER (MEET-H HUPPER P2-HYPS))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P1-HYPS (LIST))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$)) T
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))))
    (SETQ RULE-HUPPER HUPPER)
    (SETQ RULE-HLOWER HLOWER)
    T))

;;; The restriction checking function. 

(defun idisj-right-legal-wffs (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL RULE-HLOWER RULE-HUPPER) (IGNORE P2-HYPS P1-HYPS))
  (LET ((WFFBINDINGS NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO
     ((QUOTED METAVAR (IDISJ-RIGHT-MV1 IDISJ-RIGHT-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (WHEN (NOT
       (SAME-MATCH-P METALABEL (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL)))
       (MISMATCH% (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL))))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (IDISJ-RIGHT-P2 IDISJ-RIGHT-P1))
      (UNQUOTED LINEARG (P2 P1)))
     (WHEN (EXISTENT-P LINEARG)
       (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                   (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED RESTR NIL))
     (LET ((RSTR (GET RESTR 'RESTR-CALL)))
       (WHEN (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
         (THROWFAIL "Rule not applicable.  " T "Restrictions " (CAR RSTR)
                    " not satisfied.  "))))))


;;;
;;; Rule: IDISJ-LEFT
;;;

;;; The rule command definition.

(defmexpr idisj-left
  (ARGTYPES LINE LINE GWFF GWFF LINELIST LINELIST)
  (WFFARGTYPES NIL NIL "O" "O" NIL NIL)
  (WFFOP-TYPELIST)
  (ARGNAMES P2 P1 B A P2-HYPS P1-HYPS)
  (ARGHELP "Line with Disjunction" "Line with Right Disjunct" "Right Disjunct"
   "Left Disjunct" "Hypotheses" "Hypotheses")
  (DEFAULTFNS IDISJ-LEFT-DEFAULTS)
  (MAINFNS IDISJ-LEFT-LEGAL IDISJ-LEFT-BUILD)
  (ENTERFNS IDISJ-LEFT-ENTER)
  (MHELP "Infer a disjunction from right disjunct."))

;;; The line assertions justifications and restrictions

(defrulewffs idisj-left
  (UNIQUE-LINES 
     (IDISJ-LEFT-P2 "A(O) OR B(O)")
     (IDISJ-LEFT-P1 "B(O)"))
  (UNIQUE-RESTRICTIONS ))

;;; The suggesting rule definition.

(defsrule idisj-left
  (MATCHFN IDISJ-LEFT-MATCH))


;;; The building function. 

(defun idisj-left-build (P2 P1 B A P2-HYPS P1-HYPS)
  (LET ((NEW-LINE-LABELS (LINE-LABEL-VEC 2))
        (WFFBINDINGS NIL)
        (NUM-ALIST NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO
     ((QUOTED METAVAR (IDISJ-LEFT-MV1 IDISJ-LEFT-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO
     ((QUOTED U-LINE (IDISJ-LEFT-P2 IDISJ-LEFT-P1))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL (META-SUBST (GET U-LINE 'META-ASSERTION)) 'ASSERTION))
    (MACRO-DO ((UNQUOTED LINE-ARG (P2 P1)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL LINE-ARG 'LINENUMBER)
     (PUSH (CONS LINE-ARG LINE-LABEL) NUM-ALIST))
    (MACRO-DO
     ((UNQUOTED JUST
       ((LIST "Disj" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST P1)))
        (NEXTPLAN)))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL JUST 'JUSTIFICATION))
    (MACRO-DO ((UNQUOTED HYP-ARG (P2-HYPS P1-HYPS)))
     (SETQ HYP-ARG (SUBST-LABELS NUM-ALIST HYP-ARG)))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (P2-HYPS P1-HYPS)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL HYP-ARG 'HYPOTHESES))
    (APPEND NEW-LINE-LABELS (LIST B A P2-HYPS P1-HYPS))))

;;; The entering function. 

(defun idisj-left-enter (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (IGNORE B A P2-HYPS P1-HYPS))
  (UPDATE-PLAN (EVAL-DESTRUCT ((P2 'SS))) (EVAL-DESTRUCT ((P1 'SS)))))

;;; The default function. 

(defun idisj-left-defaults (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL STRONG-DEFAULTLIST))
  (LET ((WFFBINDINGS NIL)
        STRONG-HYPDEFAULTS)
    (DECLARE (SPECIAL WFFBINDINGS STRONG-HYPDEFAULTS))
    (MACRO-DO
     ((QUOTED METAVAR (IDISJ-LEFT-MV1 IDISJ-LEFT-MV0)) (UNQUOTED WFFARG (B A)))
     (WHEN (SPECIFIED-P WFFARG)
       (PUSH (CONS METAVAR WFFARG) WFFBINDINGS)))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (IDISJ-LEFT-P2 IDISJ-LEFT-P1))
      (UNQUOTED LINEARG (P2 P1)))
     (IF (AND (NOT (EQ LINEARG '$)) (EXISTENT-P LINEARG))
         (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                     (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (LET ((WFFVAL (WFFEVAL METALABEL)))
       (WHEN (NOT WFFVAL)
         (%CATCH% (META-SUBST METALABEL) (FAIL NIL)))))
    (MACRO-DO
     ((QUOTED METAVAR (IDISJ-LEFT-MV1 IDISJ-LEFT-MV0)) (UNQUOTED WFFARG (B A)))
     (LET ((WFFVAL (WFFEVAL METAVAR)))
       (WHEN WFFVAL
         (SETQ WFFARG WFFVAL))))
    (WHEN (MEMBER '$ (LIST P2-HYPS P1-HYPS))
      (IF (NOT (MEMBER '$ (LIST P2 P1)))
          (SETQ-DESTRUCT (P2-HYPS P1-HYPS)
           (IDISJ-LEFT-HYP-DEFAULTS P2 P1 B A P2-HYPS P1-HYPS))
          (SETQ STRONG-HYPDEFAULTS (MAPCAR #'SPECIFIED-P (LIST P2-HYPS P1-HYPS)))))
    (SETQ-DESTRUCT ((P2 'SS)) (LINE-NO-DEFAULTS-FROM (EVAL-DESTRUCT ((P2 'SS)))))
    (WHEN (NOT (MEMBER '$ (LIST P2 'SS)))
      (SETQ-DESTRUCT ((P1 'SS))
       (LINE-NO-DEFAULTS-TO (EVAL-DESTRUCT ((P2 'SS))) (EVAL-DESTRUCT ((P1 'SS))))))
    (SETQ STRONG-DEFAULTLIST
          (APPEND '(NIL NIL) (MAPCAR #'SPECIFIED-P (LIST B A)) STRONG-HYPDEFAULTS))
    (LIST P2 P1 B A P2-HYPS P1-HYPS)))

;;; The hypotheses default function. 

(defun idisj-left-hyp-defaults (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL STRONG-HYPDEFAULTS) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO ((UNQUOTED LINEARG (P2 P1)) (UNQUOTED HYPARG (P2-HYPS P1-HYPS)))
     (WHEN (EXISTENT-P LINEARG)
       (IF (SPECIFIED-P HYPARG)
           (WHEN (NOT (SET-EQ HYPARG (HYPNUMS LINEARG)))
             (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                        " are not the same as the one in the proof."))
           (SETQ HYPARG (HYPNUMS LINEARG)))))
    (WHEN (SPECIFIED-P P2-HYPS)
      (SETQ HUPPER (MEET-H HUPPER P2-HYPS)))
    (WHEN (SPECIFIED-P P1-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P1-HYPS (LIST)))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$))
        (SETQ STRONG-HYPDEFAULTS (MAPCAR #'SPECIFIED-P (LIST P2-HYPS P1-HYPS)))
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))
          (WHEN (NOT AUTO-GENERATE-HYPS)
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P2-HYPS P1-HYPS))))
          (WHEN (NOT (SPECIFIED-P P2-HYPS))
            (SETQ P2-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN (NOT (SPECIFIED-P P1-HYPS))
            (SETQ P1-HYPS (ORDERED-JOIN-H HUPPER (LIST))))
          (WHEN AUTO-GENERATE-HYPS
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P2-HYPS P1-HYPS))))))
    (LIST P2-HYPS P1-HYPS)))

;;; The matching function. 

(defun idisj-left-match (PLAN-SUPPORT)
  (LET ((WFFBINDINGS NIL)
        (MATCHED-PLAN)
        (MATCHED-SUPPORT))
    (DECLARE (SPECIAL WFFBINDINGS))
    (SETQ MATCHED-PLAN
          (PROGN
            (%CATCH%
             (MATCH-BIND (GET 'IDISJ-LEFT-P2 'META-ASSERTION)
                         (GET (CAR PLAN-SUPPORT) 'ASSERTION))
             (FAIL (THROWFAIL "Planned lines did not match.")))
            (LIST (CAR PLAN-SUPPORT))))
    (SETQ MATCHED-SUPPORT
          (MACRO-DO ((QUOTED RESTR NIL))
           (LET ((RSTR (GET RESTR 'RESTR-CALL)))
             (WHEN (%CATCH% (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
                       (FAIL NIL))
               (THROWFAIL "Some restriction not satisfied.")))))
    (LIST MATCHED-PLAN MATCHED-SUPPORT)))

;;; The checking function. 

(defun idisj-left-legal (P2 P1 B A P2-HYPS P1-HYPS)
  (LET ((RULE-HUPPER NIL)
        (RULE-HLOWER NIL))
    (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER))
    (IDISJ-LEFT-LEGAL-HYPS P2 P1 B A P2-HYPS P1-HYPS)
    (IDISJ-LEFT-LEGAL-WFFS P2 P1 B A P2-HYPS P1-HYPS)
    T))

;;; The hypotheses checking function. 

(defun idisj-left-legal-hyps (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO ((UNQUOTED LINEARG (P2 P1)) (UNQUOTED HYPARG (P2-HYPS P1-HYPS)))
     (WHEN (AND (EXISTENT-P LINEARG) (NOT (SET-EQ HYPARG (HYPNUMS LINEARG))))
       (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                  " are not the same as the ones in the proof.")))
    (SETQ HUPPER (MEET-H HUPPER P2-HYPS))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P1-HYPS (LIST))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$)) T
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))))
    (SETQ RULE-HUPPER HUPPER)
    (SETQ RULE-HLOWER HLOWER)
    T))

;;; The restriction checking function. 

(defun idisj-left-legal-wffs (P2 P1 B A P2-HYPS P1-HYPS)
  (DECLARE (SPECIAL RULE-HLOWER RULE-HUPPER) (IGNORE P2-HYPS P1-HYPS))
  (LET ((WFFBINDINGS NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO
     ((QUOTED METAVAR (IDISJ-LEFT-MV1 IDISJ-LEFT-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (WHEN (NOT
       (SAME-MATCH-P METALABEL (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL)))
       (MISMATCH% (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL))))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (IDISJ-LEFT-P2 IDISJ-LEFT-P1))
      (UNQUOTED LINEARG (P2 P1)))
     (WHEN (EXISTENT-P LINEARG)
       (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                   (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED RESTR NIL))
     (LET ((RSTR (GET RESTR 'RESTR-CALL)))
       (WHEN (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
         (THROWFAIL "Rule not applicable.  " T "Restrictions " (CAR RSTR)
                    " not satisfied.  "))))))


;;;
;;; Rule: INDIRECT
;;;

;;; The rule command definition.

(defmexpr indirect
  (ARGTYPES LINE LINE LINE LINE GWFF GWFF LINELIST LINELIST LINELIST LINELIST)
  (WFFARGTYPES NIL NIL NIL NIL "O" "O" NIL NIL NIL NIL)
  (WFFOP-TYPELIST)
  (ARGNAMES P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
  (ARGHELP "Line to be Proven by Contradiction"
   "Line with Negated Consequence of Assumption"
   "Line with Positive Consequence of Assumption" "Line with Assumed Negation"
   "One of Two Contradictory Consequences of Assumption"
   "Assertion to be Proven by Contradiction" "Hypotheses" "Hypotheses"
   "Hypotheses" "Hypotheses")
  (DEFAULTFNS INDIRECT-DEFAULTS)
  (MAINFNS INDIRECT-LEGAL INDIRECT-BUILD)
  (ENTERFNS INDIRECT-ENTER)
  (MHELP "Rule of Indirect Proof."))

;;; The line assertions justifications and restrictions

(defrulewffs indirect
  (UNIQUE-LINES 
     (INDIRECT-P4 "A(O)")
     (INDIRECT-P3 "~ B(O)")
     (INDIRECT-P2 "B(O)")
     (INDIRECT-H1 "~ A(O)"))
  (UNIQUE-RESTRICTIONS ))

;;; The suggesting rule definition.

(defsrule indirect
  (MATCHFN INDIRECT-MATCH))


;;; The building function. 

(defun indirect-build (P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
  (LET ((NEW-LINE-LABELS (LINE-LABEL-VEC 4))
        (WFFBINDINGS NIL)
        (NUM-ALIST NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO
     ((QUOTED METAVAR (INDIRECT-MV1 INDIRECT-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO
     ((QUOTED U-LINE (INDIRECT-P4 INDIRECT-P3 INDIRECT-P2 INDIRECT-H1))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL (META-SUBST (GET U-LINE 'META-ASSERTION)) 'ASSERTION))
    (MACRO-DO
     ((UNQUOTED LINE-ARG (P4 P3 P2 H1)) (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL LINE-ARG 'LINENUMBER)
     (PUSH (CONS LINE-ARG LINE-LABEL) NUM-ALIST))
    (MACRO-DO
     ((UNQUOTED JUST
       ((LIST "Indirect" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST P2 P3)))
        (NEXTPLAN) (NEXTPLAN)
        (LIST "Assume negation" (MAPCAR #'META-SUBST (QUOTE NIL))
              (SUBST-LABELS NUM-ALIST (LIST)))))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL JUST 'JUSTIFICATION))
    (MACRO-DO ((UNQUOTED HYP-ARG (P4-HYPS P3-HYPS P2-HYPS H1-HYPS)))
     (SETQ HYP-ARG (SUBST-LABELS NUM-ALIST HYP-ARG)))
    (MACRO-DO
     ((UNQUOTED HYP-ARG (P4-HYPS P3-HYPS P2-HYPS H1-HYPS))
      (LOCAL LINE-LABEL NEW-LINE-LABELS))
     (PUTPROP LINE-LABEL HYP-ARG 'HYPOTHESES))
    (APPEND NEW-LINE-LABELS (LIST B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS))))

;;; The entering function. 

(defun indirect-enter (P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
  (DECLARE (IGNORE B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS))
  (UPDATE-PLAN (EVAL-DESTRUCT ((P4 'SS)))
   (EVAL-DESTRUCT ((P2 H1 'SS) (P3 H1 'SS)))))

;;; The default function. 

(defun indirect-defaults (P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
  (DECLARE (SPECIAL STRONG-DEFAULTLIST))
  (LET ((WFFBINDINGS NIL)
        STRONG-HYPDEFAULTS)
    (DECLARE (SPECIAL WFFBINDINGS STRONG-HYPDEFAULTS))
    (MACRO-DO
     ((QUOTED METAVAR (INDIRECT-MV1 INDIRECT-MV0)) (UNQUOTED WFFARG (B A)))
     (WHEN (SPECIFIED-P WFFARG)
       (PUSH (CONS METAVAR WFFARG) WFFBINDINGS)))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (INDIRECT-P4 INDIRECT-P3 INDIRECT-P2 INDIRECT-H1))
      (UNQUOTED LINEARG (P4 P3 P2 H1)))
     (IF (AND (NOT (EQ LINEARG '$)) (EXISTENT-P LINEARG))
         (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                     (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (LET ((WFFVAL (WFFEVAL METALABEL)))
       (WHEN (NOT WFFVAL)
         (%CATCH% (META-SUBST METALABEL) (FAIL NIL)))))
    (MACRO-DO
     ((QUOTED METAVAR (INDIRECT-MV1 INDIRECT-MV0)) (UNQUOTED WFFARG (B A)))
     (LET ((WFFVAL (WFFEVAL METAVAR)))
       (WHEN WFFVAL
         (SETQ WFFARG WFFVAL))))
    (WHEN (MEMBER '$ (LIST P4-HYPS P3-HYPS P2-HYPS H1-HYPS))
      (IF (NOT (MEMBER '$ (LIST P4 P3 P2 H1)))
          (SETQ-DESTRUCT (P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
           (INDIRECT-HYP-DEFAULTS P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS))
          (SETQ STRONG-HYPDEFAULTS
                (MAPCAR #'SPECIFIED-P (LIST P4-HYPS P3-HYPS P2-HYPS H1-HYPS)))))
    (SETQ-DESTRUCT ((P4 'SS)) (LINE-NO-DEFAULTS-FROM (EVAL-DESTRUCT ((P4 'SS)))))
    (WHEN (NOT (MEMBER '$ (LIST P4 'SS)))
      (SETQ-DESTRUCT-MULTI (H1) ((P2 H1 'SS) (P3 H1 'SS))
       (LINE-NO-DEFAULTS-TO (EVAL-DESTRUCT ((P4 'SS)))
        (EVAL-DESTRUCT ((P2 H1 'SS) (P3 H1 'SS))))))
    (SETQ STRONG-DEFAULTLIST
          (APPEND '(NIL NIL NIL NIL) (MAPCAR #'SPECIFIED-P (LIST B A))
                  STRONG-HYPDEFAULTS))
    (LIST P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)))

;;; The hypotheses default function. 

(defun indirect-hyp-defaults (P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
  (DECLARE (SPECIAL STRONG-HYPDEFAULTS) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (P4 P3 P2 H1))
      (UNQUOTED HYPARG (P4-HYPS P3-HYPS P2-HYPS H1-HYPS)))
     (WHEN (EXISTENT-P LINEARG)
       (IF (SPECIFIED-P HYPARG)
           (WHEN (NOT (SET-EQ HYPARG (HYPNUMS LINEARG)))
             (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                        " are not the same as the one in the proof."))
           (SETQ HYPARG (HYPNUMS LINEARG)))))
    (WHEN (SPECIFIED-P P4-HYPS)
      (SETQ HUPPER (MEET-H HUPPER P4-HYPS)))
    (WHEN (SPECIFIED-P P3-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P3-HYPS (LIST H1)))))
    (WHEN (SPECIFIED-P P2-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P2-HYPS (LIST H1)))))
    (WHEN (SPECIFIED-P H1-HYPS)
      (SETQ HLOWER (JOIN-H HLOWER (SETDIFF H1-HYPS (LIST H1)))))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$))
        (SETQ STRONG-HYPDEFAULTS
              (MAPCAR #'SPECIFIED-P (LIST P4-HYPS P3-HYPS P2-HYPS H1-HYPS)))
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))
          (WHEN (NOT AUTO-GENERATE-HYPS)
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P4-HYPS P3-HYPS P2-HYPS H1-HYPS))))
          (WHEN (NOT (SPECIFIED-P P4-HYPS))
            (SETQ P4-HYPS (ORDERED-JOIN-H HLOWER (LIST))))
          (WHEN (NOT (SPECIFIED-P P3-HYPS))
            (SETQ P3-HYPS (ORDERED-JOIN-H HUPPER (LIST H1))))
          (WHEN (NOT (SPECIFIED-P P2-HYPS))
            (SETQ P2-HYPS (ORDERED-JOIN-H HUPPER (LIST H1))))
          (WHEN (NOT (SPECIFIED-P H1-HYPS))
            (SETQ H1-HYPS (ORDERED-JOIN-H HUPPER (LIST H1))))
          (WHEN AUTO-GENERATE-HYPS
            (SETQ STRONG-HYPDEFAULTS
                  (MAPCAR #'SPECIFIED-P (LIST P4-HYPS P3-HYPS P2-HYPS H1-HYPS))))))
    (LIST P4-HYPS P3-HYPS P2-HYPS H1-HYPS)))

;;; The matching function. 

(defun indirect-match (PLAN-SUPPORT)
  (LET ((WFFBINDINGS NIL)
        (MATCHED-PLAN)
        (MATCHED-SUPPORT))
    (DECLARE (SPECIAL WFFBINDINGS))
    (SETQ MATCHED-PLAN
          (PROGN
            (%CATCH%
             (MATCH-BIND (GET 'INDIRECT-P4 'META-ASSERTION)
                         (GET (CAR PLAN-SUPPORT) 'ASSERTION))
             (FAIL (THROWFAIL "Planned lines did not match.")))
            (LIST (CAR PLAN-SUPPORT))))
    (SETQ MATCHED-SUPPORT
          (MACRO-DO ((QUOTED RESTR NIL))
           (LET ((RSTR (GET RESTR 'RESTR-CALL)))
             (WHEN (%CATCH% (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
                       (FAIL NIL))
               (THROWFAIL "Some restriction not satisfied.")))))
    (LIST MATCHED-PLAN MATCHED-SUPPORT)))

;;; The checking function. 

(defun indirect-legal (P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
  (LET ((RULE-HUPPER NIL)
        (RULE-HLOWER NIL))
    (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER))
    (INDIRECT-LEGAL-HYPS P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
    (INDIRECT-LEGAL-WFFS P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
    T))

;;; The hypotheses checking function. 

(defun indirect-legal-hyps (P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
  (DECLARE (SPECIAL RULE-HUPPER RULE-HLOWER) (IGNORE B A))
  (LET ((HUPPER '$)
        (HLOWER '$))
    (MACRO-DO
     ((UNQUOTED LINEARG (P4 P3 P2 H1))
      (UNQUOTED HYPARG (P4-HYPS P3-HYPS P2-HYPS H1-HYPS)))
     (WHEN (AND (EXISTENT-P LINEARG) (NOT (SET-EQ HYPARG (HYPNUMS LINEARG))))
       (THROWFAIL "Hypothesis specified for line " (LINEARG . LINE)
                  " are not the same as the ones in the proof.")))
    (SETQ HUPPER (MEET-H HUPPER P4-HYPS))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P3-HYPS (LIST H1))))
    (SETQ HLOWER (JOIN-H HLOWER (SETDIFF P2-HYPS (LIST H1))))
    (SETQ HLOWER (JOIN-H HLOWER (HYPSETDIFF H1-HYPS H1)))
    (IF (AND (EQ HLOWER '$) (EQ HUPPER '$)) T
        (PROGN
          (COND ((EQ HLOWER '$)
                 (SETQ HLOWER HUPPER))
                ((EQ HUPPER '$)
                 (SETQ HUPPER HLOWER))
                ((NOT (CONTAINED-P HLOWER HUPPER))
                 (THROWFAIL "Illegal extra hypotheses in conclusion: "
                            ((SETDIFF HLOWER HUPPER) . LINELIST) ".")))))
    (SETQ RULE-HUPPER HUPPER)
    (SETQ RULE-HLOWER HLOWER)
    T))

;;; The restriction checking function. 

(defun indirect-legal-wffs (P4 P3 P2 H1 B A P4-HYPS P3-HYPS P2-HYPS H1-HYPS)
  (DECLARE (SPECIAL RULE-HLOWER RULE-HUPPER)
           (IGNORE P4-HYPS P3-HYPS P2-HYPS H1-HYPS))
  (LET ((WFFBINDINGS NIL))
    (DECLARE (SPECIAL WFFBINDINGS))
    (MACRO-DO
     ((QUOTED METAVAR (INDIRECT-MV1 INDIRECT-MV0)) (UNQUOTED WFFARG (B A)))
     (PUSH (CONS METAVAR WFFARG) WFFBINDINGS))
    (MACRO-DO ((QUOTED METALABEL NIL))
     (WHEN (NOT
       (SAME-MATCH-P METALABEL (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL)))
       (MISMATCH% (WFFEVAL METALABEL) (META-LABEL-EVAL METALABEL))))
    (MACRO-DO
     ((QUOTED UNIQUE-LINE (INDIRECT-P4 INDIRECT-P3 INDIRECT-P2 INDIRECT-H1))
      (UNQUOTED LINEARG (P4 P3 P2 H1)))
     (WHEN (EXISTENT-P LINEARG)
       (MATCH-BIND (GET UNIQUE-LINE 'META-ASSERTION)
                   (GET (NUMALIAS LINEARG) 'ASSERTION))))
    (MACRO-DO ((QUOTED RESTR NIL))
     (LET ((RSTR (GET RESTR 'RESTR-CALL)))
       (WHEN (NOT (APPLY (CAR RSTR) (MAPCAR #'META-SUBST (CDR RSTR))))
         (THROWFAIL "Rule not applicable.  " T "Restrictions " (CAR RSTR)
                    " not satisfied.  "))))))
