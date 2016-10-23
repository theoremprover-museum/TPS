;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLADVICE)

;;;
;;; File: Otl-Advice
;;;
;;; Commands giving advice to the student.
;;;

(part-of otladvice)

(deffile otl-advice
  (part-of otladvice)
  (extension lsp)
  (mhelp "Defines commands giving advice to the student."))

(context suggestions)

;*;(defmexpr dangle
;*;  (mhelp "Looks whether the current proof seems sound."))

(defmexpr check-structure
  (mhelp "Check various structural properties of the current proof.
You will be informed about suspect constellations in the incomplete proof
which may make it difficult for ETPS to provide advice or for you to
finish the proof."))

(defun check-structure ()
  (declare (special dproof))
  (if (not (get dproof 'lines))
      (msgf "There are no lines.")
      (if (not (get dproof 'plans))
	  (msgf "Your proof is complete."
		t "CLEANUP is the command dealing with completed proofs.")
	  (progn
	   (msgf "Now checking for lines not connected to any planned line ...")
	   (dangle-main (get dproof 'plans) (get dproof 'lines))
	   (msgf "Now checking for extra hypotheses in the support lines ...")
	   (check-hyps-main (get dproof 'plans))))))

;*;(defun dangle ()
;*;  (declare (special dproof))
;*;  (if (not (get dproof 'lines))
;*;      (msgf "There are no lines.")
;*;      (if (not (get dproof 'plans))
;*;	  (msgf "Your proof is complete."
;*;		t "CLEANUP is the command dealing with completed proofs.")
;*;	  (dangle-main (get dproof 'plans) (get dproof 'lines)))))

(defun dangle-main (plan-supports all-lines)
  (let ((dangling-lines
	 (dangling-lines plan-supports all-lines)))
    (if dangling-lines
	(msgf "You have the following dangling lines: "
	      (dangling-lines . existing-linelist) "."
	      t "Perhaps you need to use SPONSOR to show me where they could be used.")
	(msgf "There are no dangling lines."))))

(defun dangling-lines (plan-support all-lines)
  (declare (special dproof))
  ;; We take the planned lines in the proof, working forward from them,
  ;; and the support lines, working backward from them.  Any line
  ;; not encountered thus is ``dangling''.
  (let ((plines (mapcar #'car plan-support))
	(slines (slinelist plan-support)))
    ;; Now to the slines we also must add those lines which appear
    ;; in a justfication of a depending-on-pline.  For example, after
    ;; RULEC, the existentially quantified line does not occur in the
    ;; justification of the new hypothesis, but in only in the
    ;; justification of the justified by RuleC.
    ;; We do this by looking through the depending-on-pline and
    ;; collecting those lines which are used to justify them, but do
    ;; not themselves depend on a pline.
    (let* ((depending-on-pline (dependent-lines plines all-lines))
	   (justifying-support-line (justifying-lines
				     (add-special-lines depending-on-pline
							slines))))
      (let ((all-dangling-lines
	     (set-of line all-lines
	       (not (or (memq line depending-on-pline)
			(memq line justifying-support-line))))))
	;; From all-dangling-lines we could perhaps subtract the non-extremal
	;; ones, but for now show them all.
	all-dangling-lines))))


(defun slinelist (plan-support)
  (do ((plan-support plan-support (cdr plan-support))
       (slines nil (do ((curr-slines (cdar plan-support) (cdr curr-slines))
			(slines slines (pushnew (car curr-slines) slines)))
		       ((null curr-slines) slines))))
      ((null plan-support) slines)))

(defun dependent-lines (start-lines all-lines)
  ;; Hypotheses are ignored for now.
  ;; We are using the fact that all-lines are sorted in ascending order
  (do ((all-lines all-lines (cdr all-lines))
       (curr-line)
       (dependent-lines start-lines))
      ((null all-lines) dependent-lines)
    (setq curr-line (car all-lines))
    (when (exists jline (caddr (get curr-line 'justification))
		  (memq jline dependent-lines))
	  (push curr-line dependent-lines))))


(defun add-special-lines (depend-on-pline slines)
  (do ((dplines depend-on-pline (cdr dplines))
       (jlines))
      ((null dplines) slines)
    (setq jlines (set-of jline (caddr (get (car dplines) 'justification))
		   (not (memq jline depend-on-pline))))
    ;; Hopefully there aren't too may duplicates here.
    (setq slines (append jlines slines))))

;;; The next section is the ETPS ADVICE command.  In a number of
;;; global variables, we keep the state of the advice-giver, since
;;; It will do different things when called twice in a row.
;;; Any operation on the proof structure will reset the current state.

;;; *advice-plan-support* is the plan-support structure as it was
;;; when advice was invoked last time.  If it's EQ to the current
;;; plan-support structure, we proceed by giving more advice, otherwise
;;; we recalculate.
;;; *advice-sorted-rules* is the next priority to investigate
;;; *advice-hint-given-p* whether a hint was already given.

(defvar *advice-plan-support* nil)
(defvar *advice-sorted-rules* nil)
(defvar *advice-hint-given-p* nil)

(defmexpr advice
  (mhelp "Give some advice on how to proceed with the current proof."))

(defun advice ()
  (declare (special dproof))
  (signal-event 'advice-asked *advice-hint-given-p*)
  (if (null (get dproof 'plans))
      (if (null (get dproof 'lines))
	  (msgf "You haven't even started a proof yet!"
		t "My advice is to pick an exercise and start in with the EXERCISE command.")
	  (msgf "You are done!  My advice is to use DONE, perhaps CLEANUP and"
		t "PRINTPROOF or SCRIBEPROOF."))
      (advice-main (get dproof 'plans))))

(defun advice-main (plan-supports)
  (when (not (eq plan-supports *advice-plan-support*))
	(init-advice))
  (do ((plan-support (car *advice-plan-support*))
       (suggestions))
      ((null *advice-sorted-rules*)
       (msgf "I don't have any other suggestions for planned line "
	     ((car plan-support) . existing-line) "."
	     t "If you want advice for another planned line use SUBPROOF."
	     t "I will start over with my initial suggestions, if you call ADVICE again.")
       (init-advice))
    (setq suggestions
	  (find-suggests plan-support (cdar *advice-sorted-rules*)))
    (cond (suggestions
	   (if (not *advice-hint-given-p*)
	       (give-hints suggestions)
	       (give-rules suggestions))
	   (setq *advice-hint-given-p* (not *advice-hint-given-p*))
	   (if (not *advice-hint-given-p*)
	       (setq *advice-sorted-rules* (cdr *advice-sorted-rules*)))
	   (return t))
	  (t (setq *advice-sorted-rules* (cdr *advice-sorted-rules*))))))

(defun init-advice ()
  (declare (special dproof))
  (setq *advice-sorted-rules*
	(sort-rules (set-of srule global-srulelist
		      (and (symbolp srule) (get srule 'priority)))))
  (setq *advice-hint-given-p* nil)
  (setq *advice-plan-support* (get dproof 'plans)))

(defun give-hints (suggestions)
  (do ((suggestions suggestions (cdr suggestions)))
      ((null suggestions))
    (give-hint (caar suggestions))))

(defun give-hint (rule)
  (let ((hint (get rule 'hint)))
    (if (not hint) (msgf "No hint for suggested rule " rule ".")
	(msgf hint))))

(defun give-rules (suggestions)
  (show-suggestions suggestions))

;;;
;;; The next section is about hypotheses.  We look through the plan
;;; support pairs and see whether there is any problem with too many
;;; hypotheses in the support.
;;;

;*;(defmexpr check-hyps
;*;  (mhelp "Checks whether there are extra hypotheses in the supports of
;*;the current planned lines.  This often leads to proofs which can't be
;*;completed."))

;*;(defun check-hyps ()
;*;  (declare (special dproof))
;*;  (if (not (get dproof 'lines))
;*;      (msgf "There are no lines.")
;*;      (if (not (get dproof 'plans))
;*;	  (msgf "Your proof is complete."
;*;		t "CLEANUP is the command dealing with completed proofs.")
;*;	  (check-hyps-main (get dproof 'plans)))))

(defun check-hyps-main (plan-supports)
  (do ((ps-pairs plan-supports (cdr ps-pairs))
       (found-p nil)
       (supp-hyps) (plan-hyps) (extra-hyps))
      ((null ps-pairs)
       (if found-p
	   (msgf "Unless you have a good plan how to get rid of those extra"
		 t "hypotheses, you seem to be on the wrong track."
		 t "Of course it could be that you made a mistake in using SPONSOR."
		 t "In that case you may use UNSPONSOR to remove the lines with"
		 t "extraneous hypotheses from the support of some planned lines.")
	   (msgf "The structure of your hypotheses looks fine.")))
    (setq supp-hyps (joint-hyps (cdar ps-pairs)))
    (setq plan-hyps (get (caar ps-pairs) 'hypotheses))
    (setq extra-hyps (setdiff supp-hyps plan-hyps))
    (when extra-hyps
	  (msgf "The hypotheses " (extra-hyps . existing-linelist)
		" are extraneous in the support of planned line "
		((caar ps-pairs) . existing-line) ".")
	  (setq found-p t))))

(defun joint-hyps (lines)
    (do ((lines lines (cdr lines))
	 (joint-hyps nil (join-h joint-hyps (get (car lines) 'hypotheses))))
	((null lines) joint-hyps)))
