;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)

(deffile ms92-9-top
  (part-of ms90-3)
  (extension lisp)
  (mhelp "Definitions, functions, etc., needed by ms92-9 and not already
          provided by ms90-3."))

(context ms92-9)

(defun ms92-9-controller (&optional
                          (etree (eproof-etree current-eproof))
			  (time-limit nil))      ; think time-limit has to default to this...?
  (declare (special dummy-N-O-D dup-record-flag))
; We will use the former to store the value of NUM-OF-DUPS, which we're going to mess with.
; The latter will be true if either dup-record is true, or we've run out of time on a search.
; --- i.e. it's true iff it's time for ms92-9-controller to give up looking...
  (unwind-protect
      (progn
	(setq ms90-3-jform (etree-to-jform etree))
	(setq *no-internal-duplications* T)
	(setq dummy-N-O-D NUM-OF-DUPS)
	(setq dup-record-flag nil)
	(setq NUM-OF-DUPS 0)
	(setq dup-record nil)
	(do* ((loopcount (+ 1 dummy-N-O-D) (- loopcount 1)))
	     ((or (zerop loopcount) dup-record-flag)
; we've finished if dup-record-flag is set, or we've duplicated quantifiers NUM-OF-DUPS times.
	      (progn (setq *no-internal-duplications* nil)
		     (setq NUM-OF-DUPS dummy-N-O-D)
		     (if time-limit
			 (if dup-record			; mating-found
			     (values 'succeed nil)
			   (values 'fail nil))
		       dup-record)))
	     (progn
	       (when (neq mating-verbose 'min)
		     (msg T "Displaying VP diagram ..." t)
		     (when (eq style 'concept-s) (tyol 85) (tyol 106 32))
		     (display-vp-diag ms90-3-jform))
	       (remove-sk-labels ms90-3-jform)
	       (if show-time 
		   (let ((*trace-output* *standard-output*))
		     (time (multiple-value-setq (dup-record ms90-3-mating unif-prob)
						(catch 'duplicate-everything-now (msearch ms90-3-jform time-limit)))))
		 (multiple-value-setq (dup-record ms90-3-mating unif-prob)
				      (catch 'duplicate-everything-now (msearch ms90-3-jform time-limit))))
; if ms90-3-mating is set to "bigger", this is msearch's way of telling us that it's finished and
; it can't find a mating. If this isn't why it finished, and dup-record isn't true (so we 
; haven't found a mating), then we must be out of time...

		     (if (and (not dup-record) (not (eq loopcount 1)) (eq ms90-3-mating 'bigger))
			 (progn
			   (princ "Duplicating outermost quantifier...")
			   (setq etree (deepen-to-literals (duplicate-all-outer-vars etree)))
			   (setq ms90-3-jform (etree-to-jform etree))
			   (setq dup-record-flag nil))
		       (setq dup-record-flag t))
		     (if (and (eq loopcount 1) (eq ms90-3-mating 'bigger) (eq default-ms 'ms92-9))
			 (throwfail t "No proof at current depth (" dummy-N-O-D
				    "). Try with a higher bound on NUM-OF-DUPS."))
		     (if (and (eq loopcount 1) (eq ms90-3-mating 'bigger) (eq default-ms 'ms93-1))
			 (throw 'option-tree-search-kill (values 'fail nil)))
		     )))
  (progn (setq *no-internal-duplications* nil)
	 (setq NUM-OF-DUPS dummy-N-O-D)
	 (if time-limit
	     (if dup-record			; mating-found
		 (values 'succeed nil)
	       (values 'fail nil))
	   dup-record))))


(defmateop ms92-9
  (mate-alias ms92-9-real) ;was ms92-9-controller
  (mhelp "Call mating search procedure MS92-9 on the current eproof.  This
procedure uses a naive level-saturation method, exhaustively searching
a single jform before applying any duplications. Quantifier duplications
are applied uniformly to outermost quantifiers. Will try primitive
substitution for outermost variable only.  Works on only a single
jform at a time.
The procedure is almost identical to MS88, except that the flag
NUM-OF-DUPS is used to govern how many times the outermost quantifier
may be duplicated. The internal representation of variables is as in
MS90-3."))


(defun ms92-9-real ()
  (let ((default-ms 'ms92-9))
    (matingsearch-controller)))
