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

;;; File: otl-rearrange
;;; Package: otlnl
;;;

(part-of otlnl)

(deffile otl-rearrange
  (part-of otlnl)
  (extension lsp)
  (mhelp "Defines the functions for rearranging the proof outline."))

(context otl-rearranging)

(defmexpr delete
  (argtypes existing-linelist)
  (argnames del-lines)
  (arghelp "delete lines")
  (mainfns dellines)
  (mhelp "	Delete lines from the proof outline."))

(defmexpr lock-line
  (argtypes existing-line)
  (argnames line)
  (arghelp "line")
  (mainfns lock-line)
  (mhelp "Prevent a line from being deleted."))

(defmexpr unlock-line
  (argtypes existing-line)
  (argnames line)
  (arghelp "line")
  (mainfns unlock-line)
  (mhelp "The opposite of LOCK-LINE."))

(defun lock-line (line) (setf (get line 'locked) t))

(defun unlock-line (line) (setf (get line 'locked) nil))

;;; removed gaps as a special variable -- 6/19/87 dan
(defun dellines (del-lines)
  (dolist (line del-lines) (delete-line line))
  (find-all-gaps))

;;; renamed delete*%* to delete-line.  fpf.

(defun delete-line (label)
  (when (dolist (x (proof-lines dproof) t)
	  (when (and (or (eq label x) (memq label (line-hypotheses x)))
		     (get x 'locked))
	    (msgf "Delete not allowed: line " (linealias x) " is locked.")
	    (return nil)))
  (dolist (x (proof-lines dproof))
	    (let ((y (line-just-lines x))
		  (z (line-hypotheses x)))
	      (cond ((eq label x)
		     (setf (line-justification x) '("Deleted" () ()))
		     (%prtline x)
		     (setf (line-justification x) y))
		    ((memq label z) (delete-line x))
		    (y (when (memq label y)
			     (plan x) (%prtline x))))))
  (setf (proof-lines dproof) (delete label (proof-lines dproof)))
  (when (planp label)
	(putprop label (assoc label (proof-plans dproof)) 'support)
	(setf (proof-plans dproof)
	      (delete label (proof-plans dproof) :key #'car)))
  (dolist (x (proof-plans dproof)) (delete label x))
  (setf (proof-linealiases dproof)
	(delete label (proof-linealiases dproof) :key #'cdr))
  (remprop-lines label)
  ))

;;; benign-delete-line does no checking, but simply deletes lines
;;; and removes those lines from the hypotheses, if they exist
;;; anywhere.  this is useful in cleanup.

(defun benign-delete-line (label)
  (when (dolist (x (proof-lines dproof) t)
	  (when (and (or (eq label x) (memq label (line-hypotheses x)))
		     (get x 'locked))
	    (msgf "Delete not allowed: line " (linealias x) "is locked.")
	    (return nil)))
  (dolist (x (proof-lines dproof))
    (let ((z (line-hypotheses x)))
      (cond ((eq label x)) 
	    ((memq label z)
	     (setf (line-hypotheses x) (delete label z))
	     ))))
  (setf (line-justification label) '("Deleted" () ()))
  (%prtline label)
  (setf (proof-lines dproof) (delete label (proof-lines dproof)))
  (when (planp label)
	(setf (line-support label) (assoc label (proof-plans dproof)))
	(setf (proof-plans dproof)
	      (delete label (proof-plans dproof) :key #'car)))
  (dolist (x (proof-plans dproof)) (delete label x))
  (setf (proof-linealiases dproof)
	(delete label (proof-linealiases dproof) :key #'cdr))
  (remprop-lines label)
  ))

(defmexpr move
  (argtypes existing-line line)
  (argnames old-line new-line)
  (arghelp "Present line number" "New line number")
;  (closefns princ princsans)
  (mhelp "	Renumber one particular line."))

(defun move (alias num)
  (let ((line (linealias alias)))
    (when (numalias num)
	  (throwfail "The destination line is already present."))
    (ck-just alias num nil)
    (ck-hyp alias num nil)
    (do ((linelabel (car (proof-lines dproof)) (car lines))
	 (lines (cdr (proof-lines dproof)) (cdr lines)))
	((null linelabel))
      (if (neq linelabel alias)	     
	  (when (and (memq alias (line-hypotheses linelabel))
		     (> num (linealias linelabel)))
		(msg f "Line " (linealias linelabel)
		     " which contains line " line " as hypotheses"
		     " occurs before the destination line " num "."))
	  (when (and (memq alias (line-just-lines linelabel))
		      (> num (linealias linelabel)))
		 (msg f "Line " (linealias linelabel)
		      " which is justified by line " line
		      " occurs before the destination line "
		      num "."))))
    (setf (proof-linealiases dproof)
	  (delete alias (proof-linealiases dproof) :key #'cdr))
    ;; Following will insert new dotted pair in order of linenumber
    (setf (proof-linealiases dproof)
	  (do* ((remaining (proof-linealiases dproof) (cdr remaining))
		(previous nil (cons first previous))
		(first (car (proof-linealiases dproof)) (car remaining)))
	      ((or (null first)
		   (> (car first) num))
	       (nconc (nreverse previous)
		      (cons (cons num alias) remaining)))))
    (setf (line-linenumber alias) num)
    (when (cdr (proof-lines dproof))
	  ;; linealiases are already ordered
	  (setf (proof-lines dproof)
		(mapcar #'cdr (proof-linealiases dproof))))
    (find-all-gaps)
    (list line '"moved to" num)))

(defmexpr plan
  (argtypes existing-line)
  (argnames line)
  (arghelp "Number of justified line")
  (closefns %prtlines)
  (mhelp "	Change a justified line to a planned line."))

(defun plan (linelabel)
  (when (not (planp linelabel))
	(setf (line-justification linelabel) (nextplan))
	(push (plan-again linelabel) (proof-plans dproof)))
  (list linelabel))
					
(defun plan-again (linelabel)
  (let* ((cur-lines (proof-lines dproof))
	 (supports (intersection cur-lines (line-support linelabel))))
    (setf (line-support linelabel) supports)
    (cons linelabel supports)))


(defmexpr renumberall
  (argtypes posinteger)
  (argnames num)
  (arghelp "Increment")
  (defaultfns 
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '(10) rest)))
  (mhelp "Renumber all the lines in the current proof."))

(defun renumberall (inc)
  (do* ((aliases (proof-linealiases dproof) (cdr aliases))
	(num inc (+ num inc))
	(line (cdar aliases) (cdar aliases)))
       ((null line) (find-all-gaps))
       (rplaca aliases (cons num line))
       (setf (line-linenumber line) num)))

(defmexpr squeeze 
  (mainfns remove-unnecessary-gaps)
  (mhelp "Removes unnecessary gaps from the proof structure."))

(defmexpr modify-gaps
  (argtypes posinteger posinteger)
  (argnames num1 num2)
  (arghelp "min length of gap" "max length of gap")
  (defaultfns 
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '(10 10) rest)))
  (mhelp "Remove unnecessary gaps from the proof structure, and modify 
line numbers so that the length of each gap is neither less than the first
argument, nor greater than the second."))

(defun modify-gaps (incr bound)
  (when (< bound incr)
	(throwfail t "The maximum gap length (" bound 
") is less than the minimal gap length (" incr ")."))
  (do ((linelabel  (car (proof-lines dproof)) (car lines))
       (lines (cdr (proof-lines dproof))  (cdr lines))
       (next-line (if (planp (car (proof-lines dproof))) (1+ incr) 1) 
		  (1+ next-line)))
      ((null linelabel)
       (do ((lines (proof-lines dproof) (cdr lines))
	    (aliases nil))
	   ((null lines) (setf (proof-linealiases dproof) (nreverse aliases)))
	 (push (cons (line-linenumber (car lines)) (car lines))
	       aliases))
       (find-all-gaps))
    (setf (line-linenumber linelabel) next-line)
    (when (planp (car lines))
	  (let ((incr1 (- (line-linenumber (car lines)) 1
			  (line-linenumber linelabel))))
	    (setq next-line
		   (+ next-line (cond ((< incr1 incr) incr)
				      ((> incr1 bound) bound)
				      (t incr1))))))))

(defmexpr introduce-gap
  (argtypes existing-line posinteger)
  (argnames line num)
  (arghelp "Line where gap is to be introduced" "Increment")
  (defaultfns 
   (lambda (&rest rest)
     (mapcar #'(lambda (argdefault arg) (if (eq arg '$) argdefault arg))
	     '($ 10) rest)))
  (mhelp "Introduce a gap in an existing proof."))

;;; Here we use that lines are kept in increasing order.
;;; This could be done much faster if we knew that
;;; linealiases were also kept in increasing order.

(defun introduce-gap (line num)
  (let ((lines-to-be-changed (member line
				     (proof-lines dproof))))
    (dolist (x lines-to-be-changed)
      (incf (line-linenumber x) num)
      (rplaca (member x (proof-linealiases dproof) :key #'cdr)
	      (cons (line-linenumber x) x)))
    (find-all-gaps)))

;;; Added move* and delete* 11NOV90 DAN

(defmexpr move*
  (argtypes line-range line)
  (argnames range-to-move new-start)
  (arghelp "range to move" "new start of range")
  (mainfns move*)
  (mhelp "Move all proof lines in given range to begin at new start
number, but preserving the relative distances between the lines."))

(defun move* (line-range new-start)
  (let* ((lines (pl (car line-range) (cdr line-range)))
	 (new-start (if (< (car line-range) new-start) 
			(+ new-start (- (linealias (car (last lines))) (linealias (car lines))))
		     new-start))
	 (lines (if (< (car line-range) new-start) (reverse lines) lines)))
    (do* ((line (car lines) (car lines))
	  (lines (cdr lines) (cdr lines))
	  (diff 0 (if line
		      (- (linealias line)
			 first-line)))
	  (first-line (linealias line)))
	((null line))
      (move line (+ new-start diff)))))

(defmexpr delete*
  (argtypes line-range-list)
  (argnames ranges)
  (arghelp "ranges to delete")
  (mainfns dellines*)
  (mhelp "Delete ranges of lines from the proof outline."))

(defun dellines* (ranges)
  (dellines
   (mapcan #'(lambda (x)
	       (let ((first (car x))
		     (last (cdr x))
		     (list nil))
		 (dotimes (i (1+ (- last first)) (nreverse list))
		   (if (numalias (+ i first))
		       (push (numalias (+ i first)) list))))
	       )
	   ranges)))

; Author: cebrown

; A Discharged Hypothesis is one of the following:
; Hyp: (discharged by Deduct)
; Choose: (discharged by RuleC)
; Case [1-4]: (discharged by Cases)

; ADD-HYPS:
; First, we check to make sure the given line will still be
; legally justified if we add the hypotheses.  If this is not
; the case, we give the user the option of adding the hypotheses
; to the line, leaving the line as planned.  If the line will still be
; legally justified, then the first thing the following code does
; is find the line discharging the hypothesis (disch-line) and
; the "boundary" for the hypothesis (this is the discharge line
; unless the hypothesis is introduced by Cases, in which case the
; boundary is the corresponding case conclusion).
; Next, we find all the lines to which we will also need to add
; some hypotheses.  If any of these are after the boundary for one of the
; hypotheses, then we get a boundary-warning (discharge problem).
; Finally, we need to check that applications
; of RuleC, UGen, Substitute, TypeSubst, and
; Subst=[l,r] are still legal (since these depend on certain variables
; not occurring free in the hypotheses).  If everything is legal,
; then the user may either add the hypotheses to all the lines,
; or only to the line given (making any lines that immediately depend on it
; planned lines), or do nothing.
;
; When there are boundary-warnings (discharge problems) then
; the user may still add the hypotheses to the lines, but he
; may view the discharge problems for more information before
; choosing what to do.

; DELETE-HYPS:
; First we're given a line and some hypotheses.
; We find all lines after the given line from
; which we can delete some hypotheses.  Also,
; we find the lines before it from which we *can* delete
; some hypotheses.  If this traces back all the way to one of
; the hyps, then we don't give the option of deleting the lines
; before it.  Aside from this, the options are:
;   1. Delete the hyps from the given line, leaving it as planned.
;   2. Delete the hyps from the lines after the given line (leaving given line planned).
; (Unless the statement above applies . . .)
;     3. Delete the hyps from the lines before the given line
;     4. Delete the hyps from all the lines (after and before).
;   3/5. DO NOTHING

(defmexpr add-hyps
  (argtypes existing-linelist existing-line)
  (argnames hyps line)
  (arghelp "Hyp Lines" "Line to which Hypotheses should be Added")
  (defaultfns (lambda (hyps line)
		(list hyps line)))
  (mainfns add-hyps)
  (mhelp "Weaken a line to include extra hypotheses.
Adding the hypotheses to the line may cause some lines to
become planned lines.  If possible, the user is given the option 
of adding hypotheses to lines after the given line so that no 
lines will become planned."))

(defmexpr delete-hyps
  (argtypes existing-linelist existing-line)
  (argnames hyps line)
  (arghelp "Hyp Lines" "Line from which Hypotheses should be Deleted")
  (defaultfns (lambda (hyps line)
		(list hyps line)))
  (mainfns delete-hyps)
  (mhelp "Delete some hypotheses from the given line.
This may leave the given line as a planned line.
The user is given the option of also deleting some hypotheses
from lines after the given line.  If possible, the user
is given the option of deleting some hypotheses from lines
before the given line so that the given line does not become
a planned line."))

(defun add-hyps (hyps line)
  (let ((hyp-linenums (mapcar #'linealias hyps))
	(linenum (linealias line))
	(immediate-consequences nil)
	(legal t)
	(add-to-lines (acons line (set-difference hyps (line-hypotheses line))
			     nil))
	(disch-lines nil)
	(boundary-lines nil)
	(boundary-warnings nil)
	(first-boundary-warnings nil)
	(finished nil)
	(count nil))
    (when hyps
	  (dolist (h hyps)
		  (unless (member h (line-hypotheses h))
			  (throwfail "The line " (linealias h) " is not a hypothesis.")))
	  (unless (cdar add-to-lines)
		  (if (cdr hyps)
		      (throwfail "The lines " hyp-linenums
				 " are already hypotheses for line " linenum)
		    (throwfail "The line " (car hyp-linenums)
			       " is already a hypothesis for line " linenum)))
	  (dolist (n hyp-linenums)
		  (when (< linenum n)
			(throwfail "The line " linenum 
				   " occurs before hypothesis " n)))
	  (if (double-check-legal (cdar add-to-lines) line)
	      (progn
		(dolist (h (cdar add-to-lines))
			(multiple-value-bind
			 (disch-line boundary-line)
			 (find-disch-line-and-boundary-line h)
			 (when (and disch-line boundary-line)
			       (setq disch-lines (acons h disch-line disch-lines)
				     boundary-lines (acons h
							   (cons boundary-line
								 (if (equal (car (line-justification
										  disch-line))
									    "Cases")
								     "case conclusion line "
								   "discharge line "))
							   boundary-lines)))))
		(dolist (h hyps)
			(let ((hb (assoc h boundary-lines)))
			  (when (and hb
				     (<= (cadr hb) linenum))
				(setq boundary-warnings
				      (cons (list linenum
						  (linealias h) (cadr hb)
						  (cddr hb))
					    boundary-warnings)))))
		(setq first-boundary-warnings boundary-warnings)
		(dolist (current (proof-lines dproof))
			(let* ((cdeps (caddr (line-justification current)))
			       (chyps (line-hypotheses current))
			       (nhyps (set-difference
				       (union-list
					(mapcar #'(lambda (d)
						    (let ((ahyps (assoc d add-to-lines)))
						      (if ahyps
							  (cdr ahyps)
							nil)))
						cdeps))
				       (append chyps
					       (let ((dis (rassoc current disch-lines)))
						 (if dis
						     (list (car dis))
						   nil))))))
			  (when (intersection cdeps
					      (mapcar #'car add-to-lines))
				(unless (double-check-legal nhyps current)
					(setq legal nil)))
			  (when nhyps
				(dolist (h nhyps)
					(let ((hb (assoc h boundary-lines)))
					  (when (and hb
						     (<= (cadr hb) (linealias current)))
						(unless (and (= (cadr hb) (linealias current))
							     (string= (cddr hb)
								      "case conclusion line "))
							(setq boundary-warnings
							      (cons (list (linealias current)
									  (linealias h) (cadr hb)
									  (cddr hb))
								    boundary-warnings))))))
				(when (member line cdeps)
				      (setq immediate-consequences
					    (cons current immediate-consequences)))
				(when legal
				      (setq add-to-lines
					    (acons current nhyps add-to-lines))))))
		(setq boundary-warnings (reverse boundary-warnings))
		(do nil
		    (finished)
		    (setq count 2)
		    (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
		    (if immediate-consequences
			(if first-boundary-warnings
			    (complain "1) Add hyps only to line " linenum 
				      " making " (sort (mapcar #'linealias immediate-consequences) #'<)
				      " PLANNED ignoring discharge problems.")
			  (complain "1) Add hyps only to line " linenum 
				    " making " (sort (mapcar #'linealias immediate-consequences) #'<)
				    " PLANNED."))
		      (if first-boundary-warnings
			  (complain "1) Add hyps to line " linenum " ignoring discharge problems.")
			(complain "1) Add hyps to line " linenum)))
		    (if (and immediate-consequences legal)
			(if boundary-warnings
			    (progn
			      (complain "2) Add hyps to lines "
					(sort (mapcar #'(lambda (x)
							  (linealias (car x)))
						      add-to-lines) #'<)
					" ignoring discharge problems.")
			      (complain "3) Show discharge problems before making a choice.")
			      (setq count (+ count 2)))
			  (progn
			    (incf count)
			    (complain "2) Add hyps to lines "
				      (sort (mapcar #'(lambda (x)
							(linealias (car x)))
						    add-to-lines) #'<))))
		      (when first-boundary-warnings
			    (complain "2) Show discharge problems before making a choice.")
			    (incf count)))
		    (complain count ") DO NOTHING")
		    (setq count (get-a-number count))
		    (cond ((= count 1)
			   (setf (line-hypotheses line)
				 (sort
				  (union (line-hypotheses line) hyps) #'lineordering))
			   (setq finished t)
			   (dolist (current immediate-consequences)
				   (plan current)))
			  ((and immediate-consequences legal
				(= count 2))
			   (setq finished t)
			   (dolist (current-hyps add-to-lines)
				   (setf (line-hypotheses (car current-hyps))
					 (sort
					  (union (line-hypotheses (car current-hyps))
						 (cdr current-hyps))
					  #'lineordering))))
			  ((and (not (and immediate-consequences legal))
				first-boundary-warnings
				(= count 2))
			   (complain "Discharge Problems:")
			   (dolist (boundary-warning first-boundary-warnings)
				   (if (= (car boundary-warning) (caddr boundary-warning))
				       (complain "Line " (car boundary-warning) " is the "
						 (cadddr boundary-warning)
						 "for hyp "
						 (cadr boundary-warning))
				     (complain "Line " (car boundary-warning) " occurs after line "
					       (caddr boundary-warning) ", the "
					       (cadddr boundary-warning)
					       "for hyp "
					       (cadr boundary-warning))))
			   (msg t t))
			  ((and immediate-consequences legal
				boundary-warnings
				(= count 3))
			   (complain "Discharge Problems:")
			   (dolist (boundary-warning boundary-warnings)
				   (if (= (car boundary-warning) (caddr boundary-warning))
				       (complain "Line " (car boundary-warning) " is the "
						 (cadddr boundary-warning)
						 "for hyp "
						 (cadr boundary-warning))
				     (complain "Line " (car boundary-warning) " occurs after line "
					       (caddr boundary-warning) ", the "
					       (cadddr boundary-warning)
					       "for hyp "
					       (cadr boundary-warning))))
			   (msg t t))
			  (t
			   (setq finished t)))))
; If we can't legally add the hypotheses to the given line without changing it to planned, then . . .
	    (progn 
	      (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	      (complain "1) Add hyps to line " linenum " making it PLANNED.")
	      (complain "2) DO NOTHING.")
	      (setq count (get-a-number 2))
	      (when (= count 1)
		    (plan line)
		    (setf (line-hypotheses line)
			  (sort (append (line-hypotheses line)
					(cdar add-to-lines))
				#'lineordering))))))))

; returns values (line linenumber) 
;   where line is the line discharging the hyp h and linenumber is the boundary line number for h
;         (The boundary line is the same as the discharge line for Deduct and RuleC,
;           but is the appropriate case conclusion line for Cases.)
; or (nil nil) if we cannot find a discharge line.
;   - cebrown 8/11/99
(defun find-disch-line-and-boundary-line (h)
  (let ((disch-line nil)
	(boundary-linenum nil))
    (cond ((equal (car (line-justification h)) "Hyp") ; Deduct
	   (dolist (l2 (proof-lines dproof))
		   (when (and (equal (car (line-justification l2)) "Deduct")
			      (member h (line-hypotheses
					 (caaddr (line-justification l2))))
			      (not (member h (line-hypotheses l2))))
			 (setq disch-line l2)
			 (setq boundary-linenum (linealias l2))
			 (return nil))))
	  ((member (car (line-justification h)) '("Case 1" "Case 2" "Case 3"
						  "Case 4")
		   :test #'equal) ; Cases
	   (dolist (l2 (proof-lines dproof))
		   (when (and (equal (car (line-justification l2)) "Cases")
			      (not (member h (line-hypotheses l2))))
			 (dolist (l3 (cdr (caddr (line-justification l2))))
				 (when (member h (line-hypotheses l3))
				       (setq disch-line l2)
				       (setq boundary-linenum (linealias l3))
				       (return nil)))
			 (if disch-line
			     (return nil)))))
	  ((equal (car (line-justification h)) "Choose") ; RuleC
	   (dolist (l2 (proof-lines dproof))
		   (when (and (equal (car (line-justification l2)) "RuleC")
			      (member h (line-hypotheses
					 (cadr (caddr (line-justification l2)))))
			      (not (member h (line-hypotheses l2))))
			 (setq disch-line l2)
			 (setq boundary-linenum (linealias l2))
			 (return nil)))))
    (values disch-line boundary-linenum)))

(defun delete-hyps (hyps line)
  (let* ((hyp-linenums (mapcar #'linealias hyps))
	 (linenum (linealias line))
	 (immediate-consequences nil)
	 (hyps2 (intersection hyps (line-hypotheses line)))
	 (remove-from-post-lines (acons line hyps2
					nil))
	 (pre-line-stack remove-from-post-lines)
	 (pre-line-done nil)
	 (remove-from-pre-lines nil)
	 (cannot-remove-from-pre-lines nil)
	 (count 2))
    (when hyps
	  (dolist (h hyps)
		  (unless (member h (line-hypotheses h))
			  (throwfail "The line " (linealias h) " is not a hypothesis.")))
	  (unless hyps2
		  (if (cdr hyps)
		      (throwfail "None of the lines " hyp-linenums
				 " are hypotheses for line " linenum)
		    (throwfail "The line " (car hyp-linenums)
			       " is not a hypothesis for line " linenum)))
	  (dolist (current (proof-lines dproof))
		  (when (and (> (linealias current) linenum)
			     (not (member current ; not a hyp line ("Choose" and "Case n" are hyps with lines in the justification)
					  (line-hypotheses current))))
			(let* ((cdeps (caddr (line-justification current)))
			       (chyps (line-hypotheses current))
			       (rhypsl (mapcar #'(lambda (d)
						   (let ((ahyps (assoc d remove-from-post-lines)))
						     (if ahyps
							 (cdr ahyps)
						       nil)))
					       cdeps))
			       (lhypsl (mapcar #'(lambda (d r)
						   (set-difference
						    (line-hypotheses d)
						    r)) cdeps rhypsl))
			       (rhyps nil))
			  (dolist (ch chyps)
				  (when (and (member ch rhypsl
						     :test 
						     #'(lambda (x y)
							 (member x y)))
					     (not (member ch lhypsl
							  :test
							  #'(lambda (x y)
							      (member x y)))))
					(setq rhyps (cons ch rhyps))))
			  (when rhyps
				(setq remove-from-post-lines
				      (acons current rhyps
					     remove-from-post-lines))))))
	  (do nil
	      ((not pre-line-stack))
	      (let* ((current (caar pre-line-stack))
		     (rhyps (cdar pre-line-stack))
		     (cdeps (caddr (line-justification current))))
		(setq pre-line-done (cons current pre-line-done))
		(setq pre-line-stack (cdr pre-line-stack))
		(when (and (member current (line-hypotheses current))
			   (member current rhyps))
		      (setq cannot-remove-from-pre-lines t)
		      (return nil))
		(dolist (p cdeps)
			(unless (member p pre-line-done)
				(let ((prhyps (intersection rhyps (line-hypotheses p))))
				  (when prhyps
					(setq pre-line-stack
					      (acons p prhyps pre-line-stack))
					(setq remove-from-pre-lines
					      (acons p prhyps remove-from-pre-lines))))))))
	  (when (and (eq style 'istyle) (not *simple-interface-prompts*)) (start-prompt-options))
	  (if (or cannot-remove-from-pre-lines
		  remove-from-pre-lines)
	      (complain "1) Delete hyps only from line " linenum " leaving it PLANNED.")
	    (complain "1) Delete hyps only from line " linenum))
	  (when (cdr remove-from-post-lines)
		(if (or cannot-remove-from-pre-lines
			remove-from-pre-lines)
		    (complain "2) Delete hyps from lines " 
			      (sort (mapcar #'(lambda (x)
						(linealias (car x))) remove-from-post-lines)
				    #'<) " leaving " linenum " PLANNED.")
		  (complain "2) Delete hyps from lines " 
			    (sort (mapcar #'(lambda (x)
					      (linealias (car x))) remove-from-post-lines)
				  #'<)))
		(incf count))
	  (unless cannot-remove-from-pre-lines
		  (when remove-from-pre-lines
			(complain count ") Delete hyps from lines "
				  (sort (mapcar #'(lambda (x)
						    (linealias (car x)))
						(acons line hyps2 remove-from-pre-lines))
					#'<))
			(incf count))
		  (when (and remove-from-pre-lines (cdr remove-from-post-lines))
			(complain count ") Delete hyps from lines "
				  (sort (mapcar #'(lambda (x)
						    (linealias (car x)))
						(append remove-from-pre-lines
							remove-from-post-lines))
					#'<))
			(incf count)))
	  (complain count ") DO NOTHING")
	  (setq count (get-a-number count))
	  (cond ((= count 1)
		 (when (or cannot-remove-from-pre-lines
			   remove-from-pre-lines)
		       (plan line))
		 (setf (line-hypotheses line) (sort
					       (set-difference
						(line-hypotheses line)
						hyps2) #'lineordering)))
		((and (cdr remove-from-post-lines)
		      (= count 2))
		 (when (or cannot-remove-from-pre-lines
			   remove-from-pre-lines)
		       (plan line))
		 (dolist (r remove-from-post-lines)
			 (setf (line-hypotheses (car r))
			       (sort
				(set-difference (line-hypotheses (car r))
						(cdr r)) #'lineordering))))
		((and (not cannot-remove-from-pre-lines)
		      remove-from-pre-lines
		      (or (and (cdr remove-from-post-lines)
			       (= count 3))
			  (and (not (cdr remove-from-post-lines))
			       (= count 2))))
		 (dolist (r (acons line hyps2 remove-from-pre-lines))
			 (setf (line-hypotheses (car r))
			       (sort
				(set-difference (line-hypotheses (car r))
						(cdr r)) #'lineordering))))
		((and (not cannot-remove-from-pre-lines)
		      remove-from-pre-lines
		      (cdr remove-from-post-lines)
		      (= count 4))
		 (dolist (r (append remove-from-pre-lines remove-from-post-lines))
			 (setf (line-hypotheses (car r))
			       (sort
				(set-difference (line-hypotheses (car r))
						(cdr r)) #'lineordering))))))))

(defun double-check-legal (nhyps l)
  (let* ((j (line-justification l))
	 (rule (car j))
	 (deps (caddr j))
	 (rule-hupper (union nhyps (line-hypotheses l)))
	 (frees (free-in-hyps)))
    (cond ((member rule (list "Sub=" "Subst=") :test #'equal)
	   (let* ((preline (car deps)) ; compare to remove-subst=-line
		  (eqline (cadr deps)) ; in nat-etr.lisp
		  (eqas (line-assertion eqline))
		  (term1 (cdar eqas))
		  (term2 (cdr eqas))
		  (wff2 (line-assertion l))
		  (wff1 (line-assertion preline))
		  (pvs (intersection (union (free-vars-of term1)
					    (free-vars-of term2))
				     frees)))
	     (or (subst-occs term1 wff1 term2 wff2 pvs)
		 (subst-occs term2 wff1 term1 wff2 pvs)
		 (same-modulo-equality wff1 wff2 term1 term2))))
	  ((equal rule "RuleC")
	   (not (member (caadr (line-justification 
				(car (set-difference (line-hypotheses 
						      (cadr (caddr
							     (line-justification l))))
						     (line-hypotheses l)))))
			frees)))
	  ((equal rule "UGen")
	   (not 
	    (member (caadr j) frees)))
	  ((equal rule "Subst")
	   (not 
	    (member (cadadr j) frees)))
	  ((equal rule "Type Subst")
	   (dolist (h nhyps t)
		   (when (typevar-in 
			  (type (cadadr j))
			  (line-assertion h))
			 (return nil))))
	  (t t))))

(defmexpr make-assert-a-hyp
  (argtypes existing-line)
  (argnames l)
  (arghelp "Assert Line")
  (defaultfns (lambda (l)
		(list l)))
  (mainfns make-assert-a-hyp)
  (mhelp "Take a line justified by Assert, change its justification to Hyp,
make lines after it include this as a hypothesis, and perform a 
Deduct at the end so that the new proof does not depend on the Assert.

We may want to use this before calling nat-etree, since this does
not handle most Asserts."))

(defun make-assert-a-hyp (l)
  (unless (string= (car (line-justification l)) "Assert" :end1 (min 6 (length (car (line-justification l)))))
    (throwfail "Line " l " is not an Assert."))
  (let ((params nil)
	(fvars (free-vars-of (line-assertion l))))
  (dolist (l2 (proof-lines dproof)) ; be careful to miss UGen/RuleC vars (this is extra careful)
	  (when (and (string= (car (line-justification l2)) "UGen")
		     (member (caar (line-assertion l2)) fvars))
	    (push (caar (line-assertion l2)) params))
	  (when (and (string= (car (line-justification l2)) "Choose")
		     (member (caadr (line-justification l2)) fvars))
	    (push (caadr (line-justification l2)) params)))
  (dolist (a params)
	  (introduce-gap l 1)
	  (plan l)
	  (let ((n (linealias l))
		(wff (line-assertion l)))
	    (comdecode (list 'cl-user::lemma n (- n 1)
			     (list 'quote (acons a 'FORALL wff))
			     '$ '$ '$))
	    (comdecode (list 'cl-user::ui (- n 1) n
			     (list 'quote a)
			     (list 'quote wff)
			     (list 'quote a)
			     '$ '$ '$))
	    (setq l (numalias (- n 1)))))
  (setf (line-justification l)
    (cons "Hyp" (cdr (line-justification l))))
  (dolist (l2 (proof-lines dproof))
    (when (>= (linealias l2) (linealias l))
      (setf (line-hypotheses l2)
	(cons l (line-hypotheses l2)))))
  (let ((last (car (last (proof-lines dproof)))))
    (funcall #'comdecode 
	     (list 'cl-user::deduct (+ (linealias last) 1) (linealias last)
		   (linealias l) '$ '$ '$ '$ '$)))))
