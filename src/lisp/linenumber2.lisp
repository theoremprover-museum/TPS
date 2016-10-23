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

;;;
;;; File: Linenumber2
;;; Package: OTLNL
;;;
;;; defines functions that  update proof structure, and
;;; provide  defaults.
;;;
;;;LINES are arranged in increasing order.


(part-of otlnl)

(deffile linenumber2
  (part-of otlnl)
  (extension lsp)
  (mhelp "Defines functions which update the proof outline and provide
 defaults for line numbers."))



(defun update-plan (default-exist default-new)
  (declare (special dproof))
  (let ((ck-hyp-just-lines nil))
    (declare (special ck-hyp-just-lines))
    (putprop dproof
	     (update-plan1
	      (mapcar #'(lambda (xxx)
			  (mapcar #'(lambda (yyy)
				      (if (numalias (get yyy 'linenumber))
					  (modify-just yyy nil)
					  yyy))
				  xxx))
		      default-exist)
	      default-new) 'plans)))
 
;*;(defun update-plan1 (default-exist default-new)  
;*;  (declare (special dproof gaps  ck-hyp-just-lines))
;*;  (let ((current-plan-structure (copy (get dproof 'plans)))
;*;	(new-plan-structure nil)
;*;	(new-plan-structure1 nil)
;*;	(new-current-plan-structure nil)
;*;	(listofsymbols nil)
;*;	(listofnewlines nil))
;*;    (declare (special listofnewlines current-plan-structure))
;*;    (do ((old-plan (car default-exist) (car default-exist))
;*;	 (default-exist (cdr default-exist) (cdr default-exist))
;*;	 (new-plan-lines nil nil))
;*;	((null old-plan))
;*;      (let* ((plan-line (car old-plan))
;*;	     (plan-support-pair (assoc plan-line current-plan-structure)))
;*;	(cond (plan-support-pair
;*;	       (do ((linelabel (cadr old-plan) (car old-plan))
;*;		    (old-plan (cddr old-plan) (cdr old-plan))
;*;		    (plan-support-pair1 (copy (cdr plan-support-pair)))
;*;		    (var-name nil))
;*;		   ((null linelabel)
;*;		    (cond (var-name
;*;			   (push (cons var-name plan-support-pair1)
;*;				 listofsymbols)))
;*;		    (setq new-plan-structure
;*;			  (append (introduce-new-plan-line
;*;				   new-plan-lines
;*;				   (append old-plan plan-support-pair1))
;*;				  new-plan-structure))
;*;		    (setq current-plan-structure
;*;			  (delete plan-support-pair
;*;				  current-plan-structure :test #'equal)))
;*;		 ;;I assume that if a support line is missing
;*;		 ;;from the plan support pair associated with a
;*;		 ;;plan line, then it was improperly erased, and
;*;		 ;;just ignore the fact that it is missing.
;*;		 ;;**To check whether a line on the left is new, I
;*;		 ;;check whether it is a member of lines associated with
;*;		 ;;the proof.
;*;		 (cond ((not (get linelabel 'linenumber))
;*;			(setq var-name linelabel))
;*;		       ((member linelabel (get dproof 'lines))
;*;			 (setq plan-support-pair1
;*;				(delete linelabel plan-support-pair1)))
;*;		       (T (push linelabel new-plan-lines)))))
;*;	      ;;((member plan-line (get dproof 'lines))
;*;	      ;;plan-line exists, but is not a pline.
;*;	      ;;(find-new-lines old-plan)
;*;	      ;;(push (plan-again plan-line) current-plan-structure))
;*;	      ((get plan-line 'linenumber)
;*;	       ;;A rule meant to be used backwards, was applied in the
;*;	       ;;forward direction. 
;*;	       (setq new-plan-structure
;*;		     (append (find-new-lines (cdr old-plan))
;*;			     new-plan-structure))
;*;	       (let ((num (get plan-line 'linenumber)))
;*;		 (cond ((not (or (numalias num) (assoc num listofnewlines)))
;*;			(ck-just plan-line num T)
;*;			(push (cons num plan-line) listofnewlines))))
;*;	       (do ((xxx current-plan-structure (cdr xxx)) (yyy nil))
;*;		   ((null xxx) (setq current-plan-structure (nreverse yyy)))
;*;		 ;;replace "any" support line in the intersection of
;*;		 ;;each plan pair in the current plan structure by plan-line.
;*;		 (cond ((and (set-intersect-p (cdr old-plan) (cdar xxx))
;*;			     (not (memq plan-line (car xxx))))
;*;			(do ((aaa (cdar xxx) (cdr aaa))
;*;			     (bbb (list (caar xxx)))
;*;			     (old-plan (cdr old-plan)))
;*;			    ((memq (car aaa) old-plan)
;*;			     (push plan-line bbb)
;*;			     (push (append (nreverse bbb) aaa) yyy))
;*;			  (push (car aaa) bbb)))
;*;		       (T (push (car xxx) yyy)))))
;*;	      ((symbolp plan-line)
;*;	       (do ((plan-support-pair (car (get dproof 'plans))
;*;				       (car old-current-plan-structure))
;*;		    (old-current-plan-structure (cdr (get dproof 'plans))
;*;		     (cdr old-current-plan-structure))
;*;		    (new-plan-lines nil nil)
;*;		    (matched nil))
;*;		   ((null plan-support-pair)
;*;		    (cond ((not matched)
;*;			   (setq new-plan-structure1
;*;				 (append (find-new-lines old-plan)
;*;					 new-plan-structure1)))))
;*;		 (do ((linelabel (cadr old-plan) (car old-plan))
;*;		      (old-plan (cddr old-plan) (cdr old-plan))
;*;		      (plan-support-pair1 (copy (cdr plan-support-pair)))
;*;		      (hyp (get (car plan-support-pair) 'hypotheses))
;*;		      (var-name nil)
;*;		      (flag nil))
;*;		     ((or (null linelabel) flag)		      
;*;		      (cond (var-name
;*;			     (push (cons var-name plan-support-pair1)
;*;				   listofsymbols)))
;*;		      (cond ((not flag)
;*;			     (let ((new-pairs
;*;				    (find-new-pairs plan-line var-name
;*;						    (car plan-support-pair)
;*;						    plan-support-pair1
;*;						    default-new)))
;*;			       (cond ((eq new-pairs 'no))
;*;				     (T
;*;				      (setq matched T)
;*;				      (setq new-current-plan-structure
;*;					    (append
;*;					     new-pairs
;*;					     new-current-plan-structure))
;*;				      (setq new-plan-structure
;*;					    (append (introduce-new-plan-line
;*;						     new-plan-lines
;*;						     plan-support-pair)
;*;						    new-plan-structure))
;*;				      (setq current-plan-structure
;*;					    (delete plan-support-pair
;*;						    current-plan-structure
;*;						    :test #'equal))
;*;				      ))))))
;*;		   (cond ((get linelabel 'linenumber)
;*;			  (cond ((not (and
;*;				       (contained-p 
;*;					(get linelabel 'hypotheses) hyp)
;*;				       (< (get linelabel 'linenumber)
;*;					  (get (car plan-support-pair)
;*;					       'linenumber))
;*;				       (memq linelabel
;*;					     (cdr plan-support-pair))))
;*;				 (setq flag T))
;*;				((not (memq linelabel (get dproof 'lines)))
;*;				 (push linelabel new-plan-lines))
;*;				(T
;*;				 (setq plan-support-pair1
;*;				       (delete linelabel
;*;					       plan-support-pair1)))))
;*;			 (T (setq var-name linelabel)))))))))    
;*;    (let ((support-lines-on-left
;*;	   (apply #'append (mapcar #'cdr default-exist))))      
;*;      (setq new-current-plan-structure
;*;	    (append (update-plan2 support-lines-on-left listofsymbols
;*;				  default-new)
;*;		    (nreverse new-current-plan-structure))))
;*;    (putprop dproof (append listofnewlines (get dproof 'linealiases))
;*;	     'linealiases)      
;*;    (setq gaps (update-gaps gaps (mapcar #'cdr listofnewlines)))
;*;    (cond (new-plan-structure1
;*;	   (setq new-plan-structure
;*;		 (append new-plan-structure
;*;			 (update-plan-pairs new-plan-structure1
;*;					    (append
;*;					     new-current-plan-structure
;*;					     new-plan-structure
;*;					     current-plan-structure))
;*;			 ))))
;*;    (putprop dproof (sort (append (mapcar #'cdr listofnewlines)
;*;			   (get dproof 'lines)) #'lineordering) 'lines)
;*;    (cond (ck-hyp-just-lines
;*;	   (setq ck-hyp-just-lines (append ck-hyp-just-lines
;*;					   (mapcar #'cdr listofnewlines)))))
;*;    (do ((line (car ck-hyp-just-lines) (car linelist))
;*;	 (linelist (cdr ck-hyp-just-lines) (cdr linelist)))
;*;	((null line))
;*;      (setq line (numalias (linealias line)))
;*;      (do ((hyp (get line 'hypotheses) (cdr hyp))
;*;	   (new-hyp nil))
;*;	  ((null hyp)(putprop line (nreverse new-hyp) 'hypotheses))
;*;	(setq new-hyp (cons (numalias (linealias (car hyp))) new-hyp)))
;*;      (do ((justification (caddr (get line 'justification))
;*;			  (cdr justification))
;*;	   (new-j nil))
;*;	  ((null justification)
;*;	   (putprop line (list (car  (get line 'justification))
;*;			       (cadr  (get line 'justification))
;*;			       (nreverse new-j))
;*;		    'justification))
;*;	(setq new-j (cons (numalias (linealias (car justification))) new-j))))
;*;    (do ((xxx (append new-current-plan-structure new-plan-structure
;*;		      current-plan-structure)
;*;	      (cdr xxx)) (return-list nil) (list-of-plan-lines nil))
;*;	((null xxx)(setq new-plan-structure (nreverse return-list)))
;*;      (do ((yyy (cond ((memq (caar xxx) list-of-plan-lines)
;*;		       (let ((pre-support (assoc (caar xxx) return-list)))
;*;			 (setq return-list (delete pre-support return-list))
;*;			 (append (cdr pre-support) (cdar xxx))))
;*;		      (T (push (caar xxx) list-of-plan-lines)
;*;			 (cdar xxx)))
;*;		(cdr yyy))
;*;	   (zzz nil) (num (get (caar xxx) 'linenumber)))
;*;	  ((null yyy)
;*;	   (putprop (caar xxx) (nreverse zzz) 'support)
;*;	   (push (cons (caar xxx) (get (caar xxx) 'support)) return-list))
;*;	(cond ((and (> num (get (car yyy) 'linenumber))
;*;		    (not (memq (car yyy) zzz)))
;*;	       (push (car yyy) zzz)))))
;*;    (cond (printlineflag
;*;	   (for-each xxx (sort listofnewlines
;*;			       '(lambda (xxx yyy) (< (car xxx) (car yyy))))
;*;		     (%prtline (cdr xxx)))))
;*;    new-plan-structure))

;;;Removed gaps as a special variable -- 6/19/87 DAN

(defun update-plan1 (default-exist default-new)  
  (declare (special ck-hyp-just-lines))
  (let ((current-plan-structure (copy (get dproof 'plans)))
	(new-plan-structure nil)
	(new-plan-structure1 nil)
	(new-current-plan-structure nil)
	(listofsymbols nil)
	(listofnewlines nil))
    (declare (special listofnewlines current-plan-structure))
    (dolist (old-plan default-exist)
      (let* ((new-plan-lines nil)
	     (plan-line (car old-plan))
	     (plan-support-pair (assoc plan-line current-plan-structure)))
	(cond (plan-support-pair
	       (do ((linelabel (cadr old-plan) (car old-plan))
		    (old-plan (cddr old-plan) (cdr old-plan))
		    (plan-support-pair1 (copy (cdr plan-support-pair)))
		    (var-name nil))
		   ((null linelabel)
		    (if var-name
			(push (cons var-name plan-support-pair1)
			      listofsymbols))
		    (setq new-plan-structure
			  (append (introduce-new-plan-line
				   new-plan-lines
				   (append old-plan plan-support-pair1))
				  new-plan-structure))
		    (setq current-plan-structure
			  (delete plan-support-pair
				  current-plan-structure :test #'equal)))
		 ;;I assume that if a support line is missing
		 ;;from the plan support pair associated with a
		 ;;plan line, then it was improperly erased, and
		 ;;just ignore the fact that it is missing.
		 ;;**To check whether a line on the left is new, I
		 ;;check whether it is a member of lines associated with
		 ;;the proof.
		 (cond ((not (get linelabel 'linenumber))
			(setq var-name linelabel))
		       ((member linelabel (get dproof 'lines))
			(setq plan-support-pair1
			      (delete linelabel plan-support-pair1)))
		       (T (push linelabel new-plan-lines)))))
	      ;;((member plan-line (get dproof 'lines))
	      ;;plan-line exists, but is not a pline.
	      ;;(find-new-lines old-plan)
	      ;;(push (plan-again plan-line) current-plan-structure))
	      ((get plan-line 'linenumber)
	       ;;A rule meant to be used backwards, was applied in the
	       ;;forward direction. 
	       (setq new-plan-structure
		     (append (find-new-lines (cdr old-plan))
			     new-plan-structure))
	       (let ((num (get plan-line 'linenumber)))
		 (unless (or (numalias num) (assoc num listofnewlines))
		   (ck-just plan-line num T)
		   (push (cons num plan-line) listofnewlines)))
	       (do ((xxx current-plan-structure (cdr xxx)) (yyy nil))
		   ((null xxx) (setq current-plan-structure (nreverse yyy)))
		 ;;replace "any" support line in the intersection of
		 ;;each plan pair in the current plan structure by plan-line.
		 (if (and (set-intersect-p (cdr old-plan) (cdar xxx))
			  (not (memq plan-line (car xxx))))
		     (do ((aaa (cdar xxx) (cdr aaa))
			  (bbb (list (caar xxx)))
			  (old-plan (cdr old-plan)))
			 ((memq (car aaa) old-plan)
			  (push plan-line bbb)
			  (push (append (nreverse bbb) aaa) yyy))
		       (push (car aaa) bbb))
		     (push (car xxx) yyy))))
	      ((symbolp plan-line)
	       (do ((plan-support-pair (car (get dproof 'plans))
				       (car old-current-plan-structure))
		    (old-current-plan-structure (cdr (get dproof 'plans))
						(cdr old-current-plan-structure))
		    (new-plan-lines nil nil)
		    (matched nil))
		   ((null plan-support-pair)
		    (unless matched
		      (setq new-plan-structure1
			    (append (find-new-lines old-plan)
				    new-plan-structure1))))
		 (do ((linelabel (cadr old-plan) (car old-plan))
		      (old-plan (cddr old-plan) (cdr old-plan))
		      (plan-support-pair1 (copy (cdr plan-support-pair)))
		      (hyp (get (car plan-support-pair) 'hypotheses))
		      (var-name nil)
		      (flag nil))
		     ((or (null linelabel) flag)		      
		      (when var-name
			(push (cons var-name plan-support-pair1)
			      listofsymbols))
		      (unless flag
			(let ((new-pairs (find-new-pairs
					   plan-line var-name
					   (car plan-support-pair)
					   plan-support-pair1 default-new)))
			  (unless (eq new-pairs 'no)
			    (setq matched T
				  new-current-plan-structure
				  (append
				    new-pairs new-current-plan-structure)
				  new-plan-structure
				  (append (introduce-new-plan-line
					    new-plan-lines plan-support-pair)
					  new-plan-structure)
				  current-plan-structure
				  (delete plan-support-pair
					  current-plan-structure
					  :test #'equal))))))
		   (if (get linelabel 'linenumber)
		       (cond ((not (and
				     (contained-p 
				       (get linelabel 'hypotheses) hyp)
				     (< (get linelabel 'linenumber)
					(get (car plan-support-pair)
					     'linenumber))
				     (memq linelabel (cdr plan-support-pair))))
			      (setq flag T))
			     ((not (memq linelabel (get dproof 'lines)))
			      (push linelabel new-plan-lines))
			     (T (setq plan-support-pair1
				      (delete linelabel plan-support-pair1))))
		       (setq var-name linelabel))))))))
    
    (let ((support-lines-on-left
	    (append (apply #'append (mapcar #'cdr default-exist))
		    (apply #'append (mapcar #'cdr listofsymbols)))))
      (setq new-current-plan-structure
	    (append (update-plan2 support-lines-on-left listofsymbols
				  default-new)
		    (nreverse new-current-plan-structure))))
    (dolist (x listofnewlines)
      (let ((num (car x))
	    (alias (cdr x)))
	(setf (proof-linealiases dproof)
	      (do* ((remaining (proof-linealiases dproof) (cdr remaining))
		    (previous nil (cons first previous))
		    (first (car (proof-linealiases dproof)) (car remaining)))
		   ((or (null first) (> (car first) num))
		    (nreconc previous (acons num alias remaining)))))))
    (setf (gaps) (update-gaps (gaps) (mapcar #'cdr listofnewlines)))
    (when new-plan-structure1
      (setq new-plan-structure
	    (append new-plan-structure
		    (update-plan-pairs new-plan-structure1
				       (append
					new-current-plan-structure
					new-plan-structure
					current-plan-structure)))))

;; Linealiases already in order
    (setf (proof-lines dproof)
;	  (sort (nconc (mapcar #'cdr listofnewlines) (proof-lines dproof))
;		#'lineordering)
	  (mapcar #'cdr (proof-linealiases dproof)))
    (when ck-hyp-just-lines
      (setq ck-hyp-just-lines (nconc ck-hyp-just-lines
				     (mapcar #'cdr listofnewlines))))
    (dolist (line ck-hyp-just-lines)
      (setq line (numalias (linealias line)))
      (do ((hyp (line-hypotheses line) (cdr hyp))
	   (new-hyp nil))
	  ((null hyp)(setf (line-hypotheses line) (nreverse new-hyp)))
	(setq new-hyp (cons (numalias (linealias (car hyp))) new-hyp)))
      (do ((justification (line-just-lines line) (cdr justification))
	   (new-j nil))
	  ((null justification)
	   (setf (line-justification line)
		 (list (line-just-rule line) (line-just-terms line)
		       (nreverse new-j))))
	(setq new-j (cons (numalias (linealias (car justification))) new-j))))
    (do ((xxx (append new-current-plan-structure new-plan-structure
		      current-plan-structure)
	      (cdr xxx)) (return-list nil) (list-of-plan-lines nil))
	((null xxx)(setq new-plan-structure (nreverse return-list)))
      (do ((yyy (if (memq (caar xxx) list-of-plan-lines)
		    (let ((pre-support (assoc (caar xxx) return-list)))
		      (setq return-list (delete pre-support return-list))
		      (append (cdr pre-support) (cdar xxx)))
		    (progn (push (caar xxx) list-of-plan-lines) (cdar xxx)))
		(cdr yyy))
	   (zzz nil) (num (line-linenumber (caar xxx))))
	  ((null yyy)
	   (setf (line-support (caar xxx)) (nreverse zzz))
	   (push (cons (caar xxx) (line-support (caar xxx))) return-list))
	(when (and (> num (get (car yyy) 'linenumber))
		   (not (memq (car yyy) zzz)))
	  (push (car yyy) zzz))))
    (when printlineflag
      (dolist (xxx (sort listofnewlines #'< :key #'car))
	(%prtline (cdr xxx))))
    new-plan-structure))

;*;(defun update-plan2 (listofsymbols default-new)
;*;  (declare (special listofnewlines current-plan-structure dproof))
;*;  (do ((default-new default-new (cdr default-new))
;*;       (new-plan-structure nil) (new-list))
;*;      ((null default-new) (nreverse new-plan-structure))
;*;    (setq new-list (cond ((find-old-label (caar default-new) listofsymbols))
;*;			 (T '(no))))
;*;    (do ((plan-support-pair (cdar default-new)(cdr plan-support-pair)))
;*;	((null plan-support-pair))
;*;      (setq new-list (append new-list
;*;			     (find-old-label (car plan-support-pair)
;*;					     listofsymbols))))
;*;    ;;or plan-line new-line
;*;    (cond ((or (planp (car new-list))
;*;	       (and (get (caar default-new) 'linenumber)
;*;		    (not (numalias
;*;			  (get (caar default-new) 'linenumber)))))
;*;	   (push new-list new-plan-structure)))))

(defun update-plan2 (support-lines-on-left listofsymbols default-new)
  (declare (special ck-hyp-just-lines listofnewlines dproof
		    current-plan-structure))
  (do ((default-new default-new (cdr default-new)) 
       (plan-line-flag T T)
       (new-plan-structure nil) (new-list) (new-numbers))
      ((null default-new) (nreverse new-plan-structure))
    (let ((plan-line (find-old-label (caar default-new) listofsymbols
				     support-lines-on-left)))
      (setq new-list (if (and plan-line (= (length plan-line) 1))
			 plan-line '(no))))
    (setq new-numbers (list (get (car new-list) 'linenumber)))
    (dolist (pair (cdar default-new))
      (let ((new-label
	     (find-old-label pair listofsymbols support-lines-on-left)))
	(cond ((and (= (length new-label) 1)
		    (member (get (car new-label) 'linenumber) new-numbers))
	       (setq new-label (car new-label))
	       (cond ((equal (get new-label 'linenumber)
			     (car (last new-numbers))) ;;closing a gap
		      (setq plan-line-flag nil)
		      (when (get pair 'justification)
			(if (assoc (get new-label 'linenumber) listofnewlines)
			    (progn
			     (ck-just pair (get new-label 'linenumber) T)
			     (putprop new-label (get pair 'justification)
				      'justification))
			    (modify-just pair nil)))
		      (setq new-list (append new-list (last new-list))))))
	      (new-label (setq new-list (append new-list new-label))
			 (setq new-numbers
			       (nconc (mapcar #'linealias new-label)
				      new-numbers))))))
    ;;or plan-line new-line
    (when (and plan-line-flag
	       (or (planp (car new-list))
		   (and (get (caar default-new) 'linenumber)
			(not
			 (numalias (get (caar default-new) 'linenumber))))))
      (push new-list new-plan-structure))))

;*;(defun find-old-label (linelabel listofsymbols)
;*;  (declare (special current-plan-structure listofnewlines))
;*;  (cond ((get linelabel 'linenumber)
;*;	 (cond ((numalias (get linelabel 'linenumber))
;*;		(list (modify-just linelabel)))
;*;	       (T
;*;		(cond ((not (member
;*;			     (cons (get linelabel 'linenumber) linelabel)
;*;			     listofnewlines))
;*;		       (ck-just linelabel (get linelabel 'linenumber) T)
;*;		       (ck-hyp linelabel (get linelabel 'linenumber) nil)
;*;		       (push (cons (get linelabel 'linenumber) linelabel)
;*;			listofnewlines)))
;*;		  (list linelabel))))
;*;	((cdr (assoc linelabel listofsymbols)))))

(defun hyp-eq (set1 set2)
  (do ((set1 set1 (cdr set1))
       (set2 (mapcar #'linealias set2)))
      ((null set1) (if set2 nil T))
    (setq set2 (delete (linealias (car set1)) set2))))

(defun find-old-label (linelabel listofsymbols support-lines-on-left)
  (declare (special ck-hyp-just-lines current-plan-structure
		    listofnewlines default-wffeq))
  (let ((linenumber (get linelabel 'linenumber)))
    (if linenumber
	(cond ((numalias linenumber)
	       (list (modify-just linelabel support-lines-on-left)))
	      ((assoc linenumber listofnewlines)
	       (let ((old-label (cdr (assoc linenumber listofnewlines))))
		 (cond ((eq old-label linelabel))
		       ((not
			 (funcall default-wffeq (get linelabel 'assertion)
				  (get old-label 'assertion)))
			(throwfail T "New line " (linenumber . line)
				   " can't assert both \""
				   ((get linelabel 'assertion) . gwff) 
				   "\" and \"" ((get old-label 'assertion) . gwff)
				   "\"."))
		       ((not (hyp-eq (get linelabel 'hypotheses)
				     (get old-label 'hypotheses)))
			(throwfail T "New line " (linenumber . line) 
				   " is used twice with different hypotheses."))
		       (T (push old-label ck-hyp-just-lines)))
		 (list old-label)))
	      (T (ck-just linelabel linenumber T)
		 (ck-hyp linelabel linenumber nil)
		 (push (cons linenumber linelabel) listofnewlines)
		 (list linelabel)))
	(cdr (assoc linelabel listofsymbols)))))

;*;(defun find-new-pairs (name var-name plan-line binding default-new)
;*;  (declare (special listofnewlines))
;*;  (do ((default-new  default-new (cdr default-new))
;*;       (plan-structure nil)(flag nil))
;*;      ((or flag (null default-new))
;*;       (cond (flag nil)(T (nreverse plan-structure))))
;*;    (cond ((eq name (caar default-new))
;*;	   (do ((xxx (cdar default-new) (cdr xxx))
;*;		(new-pair (list plan-line)))
;*;	       ((null xxx) (push new-pair plan-structure))
;*;	     (cond ((eq (car xxx) var-name)
;*;		    (setq new-pair (append new-pair binding)))
;*;		   ((not (get (car xxx) 'linenumber)))
;*;		   ((numalias (get (car xxx) 'linenumber))
;*;		    (let ((old-label
;*;			   (numalias (get (car xxx) 'linenumber))))
;*;		      (cond ((eq old-label plan-line)
;*;			     (setq flag T))
;*;			    (T (setq new-pair
;*;				     (append new-pair (list old-label)))))))
;*;		   (T
;*;		    (cond ((not (member
;*;				 (cons (get (car xxx) 'linenumber) (car xxx))
;*;				 listofnewlines))
;*;			   (ck-just xxx (get xxx 'linenumber) T)
;*;			   (ck-hyp xxx (get xxx 'linenumber) nil)
;*;			   (push (cons (get (car xxx) 'linenumber) (car xxx))
;*;				 listofnewlines)))
;*;		     (setq new-pair (append new-pair (list (car xxx)))))
;*;		   ))))))

(defun insert-new-line (label)
  (declare (special listofnewlines))
  (unless (assoc (get label 'linenumber) listofnewlines)
    (ck-just label (get label 'linenumber) T)
    (ck-hyp label (get label 'linenumber) nil)
    (push (cons (get label 'linenumber) label) listofnewlines)))

(defun find-new-pairs (name var-name plan-line binding default-new)
  (declare (special listofnewlines))
  (do ((default-new  default-new (cdr default-new))
       (plan-structure nil)(flag nil)(wrong-pair T))
      ((or flag (null default-new))
       (cond (flag nil)(wrong-pair 'no)(T (nreverse plan-structure))))
    (when (eq name (caar default-new))
      (do ((xxx (cdar default-new) (cdr xxx))
	   (new-pair (list plan-line))
	   (wrong-pair1 nil))
	  ((or wrong-pair1 (null xxx))
	   (unless wrong-pair1
	     (setq wrong-pair nil)
	     (push new-pair plan-structure)))
	(cond ((eq (car xxx) var-name)
	       (setq new-pair (append new-pair binding)))
	      ((not (get (car xxx) 'linenumber)))
	      ((numalias (get (car xxx) 'linenumber))
	       (ck-just (car xxx) (get (car xxx) 'linenumber) T)
	       (let ((old-label (numalias (get (car xxx) 'linenumber))))
		 (cond ((eq old-label plan-line)
			(setq flag T)
			(modify-just (car xxx) nil))
		       ((contained-p (get old-label 'hypotheses)
				     (get plan-line 'hypotheses))
			(setq new-pair
			      (append new-pair (list old-label))))
		       (T (setq wrong-pair1 T)))))
	      (T (insert-new-line (car xxx))
		 (when (contained-p (get (car xxx) 'hypotheses)
				    (get plan-line 'hypotheses))
		   (setq new-pair (append new-pair (list (car xxx)))))))))))

(defun planp (linelabel)
  (declare (special dproof))
  (assoc linelabel (get dproof 'plans)))

(defun modify-just (new-label support-lines-on-left)
  (declare (special ck-hyp-just-lines current-plan-structure))
  (let ((old-label (numalias (get new-label 'linenumber))))
    (ck-just new-label (get new-label 'linenumber) T)
    (when (and (planp old-label)
	       (not (member old-label support-lines-on-left)))
      (push old-label ck-hyp-just-lines)
      (putprop old-label (get new-label 'justification) 'justification)
      (when (boundp 'current-plan-structure) 
	(setq current-plan-structure
	      (delete (assoc old-label current-plan-structure)
		      current-plan-structure))))
    old-label))
	
(defun find-new-lines (plan-support-pair)
  (declare (special listofnewlines))
  (let ((new-plan-lines nil))
    (dolist (label plan-support-pair
		   (introduce-new-plan-line new-plan-lines plan-support-pair))
      (let ((num (get label 'linenumber)))
	(cond ((and num (not (numalias num)))
	       (push label new-plan-lines)
	       (unless (assoc num listofnewlines)
		 (push (cons num label) listofnewlines)))
	      ((get label 'linenumber)
	       (ck-just label (get label 'linenumber) T)))))))

(defun update-plan-pairs (plan-support-pairs new-plan-structure)
  (declare (special dproof))
  (do ((plan-support-pairs plan-support-pairs (cdr plan-support-pairs))
       (old-support-pairs
	(apply #'append (mapcar #'cdr (setdiff (get dproof 'plans)
					       new-plan-structure))))
       (new-pairs nil))
      ((null plan-support-pairs) (nreverse new-pairs))
    (push (append (car plan-support-pairs)
		  (find-support-lines
		   (get (caar plan-support-pairs) 'linenumber)
		   (cdar plan-support-pairs)
		   old-support-pairs))
	  new-pairs)))

(defun find-support-lines (line-num new-list old-plan-support-pair)
  (let ((new-list (copy new-list)))
    (dolist (linelabel old-plan-support-pair (nreverse new-list))
      (when (and (get linelabel 'linenumber)
		 (> line-num (get linelabel 'linenumber)))
	(pushnew linelabel new-list)))))

