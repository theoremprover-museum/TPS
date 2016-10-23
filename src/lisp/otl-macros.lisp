;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLNL)

(part-of OTLNL)

(deffile otl-macros
  (part-of otlnl)
  (extension lsp)
  (mhelp "Macro file for the outline package."))

(context otl-object)

(defmode scribe-otl
  (flag-settings
   (allscopeflag nil)
   (atomvalflag nil)
   (displaywff nil)
   (flushleftflag nil)
   (leftmargin 0)
   (localleftflag nil)
   (ppwfflag T)			;Why was this nil? Changing it to T (SI) 3-5-86
   (printdepth 0)
   (rightmargin 70)
   (scope nil)
   (style scribe))
  (mhelp "Mode used for printing proofs in Scribe."))

(defmode tex-1-otl
  (flag-settings
   (allscopeflag nil)
   (atomvalflag nil)
   (displaywff nil)
   (flushleftflag nil)
   (leftmargin 0)
   (localleftflag nil)
   (ppwfflag t)			
   (printdepth 0)
   (rightmargin 85)
   (scope nil)
   (style tex-1))
  (mhelp "mode used for printing proofs in tex."))

(defmode tex-otl
  (flag-settings
   (allscopeflag nil)
   (atomvalflag nil)
   (displaywff nil)
   (flushleftflag nil)
   (leftmargin 0)
   (localleftflag nil)
   (ppwfflag t)			
   (printdepth 0)
   (rightmargin 70)
   (scope nil)
   (style tex))
  (mhelp "mode used for printing proofs in tex."))
;;;hx: this mode is not quite fit to tex style. 
;;;    Maybe change it later.


(defflag printlineflag
  (flagtype boolean)
  (default T)
  (subjects otl-vars printing)
  (mhelp "If nil, lines in the proof outline are not printed."))

(defflag print-dots
  (flagtype boolean)
  (default T)
  (subjects otl-vars printing)
  (mhelp "If nil, ... are not printed before a plan line."))

(defflag short-help
  (flagtype boolean)
  (default nil)
  (subjects otl-vars)
  (mhelp "If T, only the rule specification will be shown when asking for help
on a rule, and the command format of a command will not be shown."))


(defflag cleanup-same
  (flagtype boolean)
  (default t)
  (subjects otl-vars)
  (mhelp "If NIL, identical lines are not replaced when doing CLEANUP."))

;;; Combine with cleanup-same????
(defflag cleanup-rulec
  (flagtype boolean)
  (default t)
  (subjects otl-vars)
  (mhelp
   "If T, cleanup-same works on lines with multiple-line justifications."))

; mkaminski 10/1/2005
(defflag assert-rrules
  (flagtype boolean)
  (default nil)
  (subjects otl-object)
  (mhelp "When T, PROVE adds to the asserted line the active rewrite
rules as equational premises."))

;; Returns current gaps in outline.  Makes gaps a property of dproof,
;; rather than being global.  -- 6/19/87 DAN
(defmacro gaps nil
  `(get dproof 'gaps))

(defmacro nextplan-no (proof)
  `(get ,proof 'nextplan-no))


;;; Following macros added 7/19/87 DAN
(defmacro line-hypotheses (line)
  `(get ,line 'hypotheses))

(defmacro line-justification (line)
  `(get ,line 'justification))

(defmacro line-just-rule (line)
  `(car (get ,line 'justification)))

(defmacro line-just-terms (line)
  `(cadr (get ,line 'justification)))

(defmacro line-just-lines (line)
  `(caddr (get ,line 'justification)))

(defmacro line-assertion (line)
  `(get ,line 'assertion))

(defmacro line-support (line)
  `(get ,line 'support))

(defmacro line-linenumber (label)
  `(get ,label 'linenumber))

(defmacro proof-lines (proof)
  `(get ,proof 'lines))

(defmacro proof-plans (proof)
  `(get ,proof 'plans))

(defmacro proof-linealiases (proof)
  `(get ,proof 'linealiases))

(defmacro proof-key (proof)
  `(get ,proof 'key))

(defmacro proof-assertion (proof)
  `(get ,proof 'assertion))

(defmacro proof-nodealiases (proof)
  `(get ,proof 'nodealiases))

(defmacro line-mating (label)
  `(get ,label 'mating))

(defmacro line-node (label)
  `(get ,label 'node))

