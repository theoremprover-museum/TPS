;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLCLEANUP)

(part-of otlcleanup)

(deffile otl-cleanup
  (part-of otlcleanup)
  (extension lsp)
  (mhelp "Defines cleanup commands."))

(context otl-entering)

(defmexpr cleanup
  (mhelp "If the proof is complete, will delete unnecessary lines 
from a proof.  It may also eliminate or suggest eliminating
unnecessary hypotheses.
If the proof is incomplete, will do a partial cleanup in which
only unnecessary lines justified by SAME will be removed."))

;;;(defun cleanup ()
;;;  (when cleanup-same (cleanup-same))
;;;  (when (not treat-hlines-as-dlines) (cleanup-hypotheses))
;;;  (cleanup-justifying))

;;; Note change in order. Cleanup-same checks hypotheses-subset, so
;;; hypotheses should be cleaned up first. The differing behavior
;;; when TREAT-HLINES-AS-DLINES is T is moved down into the
;;; CLEANUP-HYPOTHESES function.

(defun cleanup ()
  (cond ((get dproof 'plans)
	 (if cleanup-same
	     (progn
	       (msg "There are still planned lines. Will clean up applications of" t
		    "SAME, as much as possible.")
	       (cleanup-same))
	   (throwfail t "There are still planned lines. Cleanup not applicable.")))
	(T (let ((num-lines (length (proof-lines dproof))))
             (cleanup-hypotheses)
             (when (or cleanup-rulec cleanup-same) (cleanup-same))
             (cleanup-justifying 
              (< (length (proof-lines dproof)) num-lines))
	     (when (and (fboundp 'auto::lazy2-used) (auto::lazy2-used)) (cleanup-odd-defs))))))

(defun cleanup-justifying (lines-already-deleted)
  (let ((dlines (setdiff (get dproof 'lines) 
			 (justifying-lines (last (get dproof 'lines))))))
    (if (endp dlines)
	(unless lines-already-deleted
          (msg "No lines can be deleted." t))
      (if (and (boundp 'auto:query-user) 
               (neq auto:query-user T)) ; changed 7/23/93 MB
	     (dolist (line dlines (find-all-gaps)) (benign-delete-line line))
	(progn
	 (msg "The following line")
	 (if (> (length dlines) 1)
	     (msg "s"))
	 (msg " can be deleted: ")
	 (dolist (line dlines) (msg (line . existing-line) " "))
	 (msg t)
         (when (query '"Should they be deleted"  t)
	     (dolist (line dlines (find-all-gaps)) (benign-delete-line line))))))))

(defun justifying-lines (justified-lines)
  ;; Hyps are ignored for now.
  (do ((jlines justified-lines (append just-lines (cdr jlines)))
       (curr-jline)
       (just-lines)
       (justifying-lines justified-lines
			 (pushnew (car jlines) justifying-lines)))
      ((endp jlines) justifying-lines)
    (setq curr-jline (car jlines))
    (setq just-lines (caddr (get curr-jline 'justification)))
    (when (and (null (car just-lines))
	       (not (or (and (> (length (car (get curr-jline 'justification))) 5)
			     (member (subseq (car (get curr-jline 'justification)) 0 6) '("Assert" "Assume" "Automatic") 
				     :test #'string=))
			(member (car (get curr-jline 'justification)) '("RuleP" "Hyp" "Choose" "Truth" "Refl=") :test #'string=)))) ; added "Truth" for ITRUTH rule - 5/6/01 - cebrown, added "Choose" for RuleC - 5/9/01; added "Refl=" which sometimes has no support 9/8/01
      (throwfail "Line " (linealias curr-jline) " has a faulty justification; this proof is damaged."))))

(defun cleanup-hypotheses ()
  ;; We are using the fact that the lines are sorted in ascending order.
  (dolist (line (get dproof 'lines))
    (let ((hyps (line-hypotheses line)))
      (setf (line-hypotheses line)
	    (if (member line hyps)    ;; a hypothesis
		(if treat-hlines-as-dlines
		    (nconc (meet-h (justifying-hyps line) hyps) (list line))
		    (list line))
		(meet-h (justifying-hyps line) hyps))))))

(defun justifying-hyps (line)
  (let ((jhyps nil))
    (dolist (jline (nth 2 (get line 'justification)))
      (setq jhyps (union (get jline 'hypotheses) jhyps)))
    jhyps))

;;;why do we only use justification lines? I made the function
;;;behave much more aggressively so that we can get rid of more
;;;things which are not needed.
(defun cleanup-same ()
  (let* ((lines (get dproof 'lines))
	 (lastline (car (last lines))))
    (dolist (line lines)
      (let ((assertion  (get line 'assertion)))
       (dolist (bline (cdr (member line lines))) 
	   (if (contained-p (get line 'hypotheses) (get bline 'hypotheses))
	       (if (wffeq assertion (get bline 'assertion))
		   (if (eq bline lastline) ; fixed a problem with deleting the last line - cebrown 6/8/2002
		       (dellines (cdr (member line lines)))
		     (change-ref bline line))
		   (if (wffeq-ab assertion (get bline 'assertion))
		       (setf (get bline 'justification) (list "AB" nil (list line)))))))))
    (find-all-gaps)))

(defun change-ref (from-line to-line)
  (dolist (line (get dproof 'lines))
    (dsubst to-line from-line (get line 'hypotheses))
    (dsubst to-line from-line (nth 2 (get line 'justification))))
  (dellines (list from-line)))

(defun cleanup-odd-defs ()
  (cleanup-odd-defs1)
  (cleanup-odd-defs2)
  t)

(defun cleanup-odd-defs1 ()
  (let ((lines (get dproof 'lines))
	(notable-lines nil))
    (dolist (line lines)
	    (when (and (string= (car (get line 'justification)) "EquivWffs")
		       (consp (get line 'assertion))
		       (consp (car (get line 'assertion)))
		       (eq (caar (get line 'assertion)) 'and)
		       (wffeq (cdar (get line 'assertion)) ;i.e. "A"
			      (get (caaddr (get line 'justification)) 'assertion))
		       (or (wffeq-def (cdar (get line 'assertion)) (cdr (get line 'assertion))) ;i.e. "(def A)"
			   (and (module-loaded-p 'expansion-tree)
				(eq 'core::= (get (caar (cdar (get line 'assertion))) 'core::stands-for))
				(wffeq (lnorm (auto::expand-top= (cdar (get line 'assertion))))
				       (cdr (get line 'assertion))))))
		  (push (linealias line) notable-lines)))
    ;notable-lines is now the list of lines of the form "A and (def A)" which are created by 
    ;equivwffs from a line of the form "A"
    (dolist (line notable-lines)
	    (let ((correct-lines nil)
		  (wff (cdr (get (numalias line) 'assertion))))
	      (dolist (l lines)
		      (when (memq line (mapcar #'linealias (caddr (get l 'justification))))
			    (if (wffeq (get l 'assertion) wff)
				(push l correct-lines)
			      (progn (setq correct-lines nil) (return nil)))))
	      ;now correct-lines is the set of all the lines of form "(def A)" which are derived 
	      ;from the line "A and (def A)"
	      (if correct-lines
		  (progn (dolist (l correct-lines)
				 (setf (get l 'justification) (list "EquivWffs" nil (caddr (get (numalias line) 'justification)))))
			 (dellines (list (numalias line))))
		(setf (get (numalias line) 'assertion) (cdr (get (numalias line) 'assertion))))))))

(defun cleanup-odd-defs2 ()
  (let ((lines (get dproof 'lines))
	(cases-lines nil)
	(indirect-lines nil)
	(other-lines nil)
	(newline nil))
    (dolist (line lines)
	    (setq newline nil)
	    (when (and (string= (car (get line 'justification)) "EquivWffs")
		       (setq newline (caaddr (get line 'justification)))
		       (consp (get newline 'assertion))
		       (consp (car (get newline 'assertion)))
		       (eq (caar (get newline 'assertion)) 'or)
		       (wffeq (cdar (get newline 'assertion)) ;i.e. "A"
			      (get line 'assertion))
		       (or (wffeq-def (cdar (get newline 'assertion)) (cdr (get newline 'assertion))) ;i.e. "(def A)"
			   (and (module-loaded-p 'expansion-tree)
				(eq 'core::= (get (caar (cdar (get newline 'assertion))) 'core::stands-for))
				(wffeq (lnorm (auto::expand-top= (cdar (get newline 'assertion))))
				       (cdr (get newline 'assertion))))))
		  (if (string= (car (get newline 'justification)) "Cases")
		      (push newline cases-lines)
		    (if (string= (car (get newline 'justification)) "Indirect")
			(push newline indirect-lines)
		      (push newline other-lines)))))
    ;notable-lines is now the list of lines of the form "A and (def A)" which are created by 
    ;equivwffs from a line of the form "A"
    (dolist (line cases-lines)
	    (let ((first-line (cadar (cddr (get line 'justification))))
		  (second-line (caddar (cddr (get line 'justification))))
		  (doing-a nil))
	      (if (and (member (car (get first-line 'justification)) '("Idisj-L" "Idisj-R") :test 'string=)
		       (member (car (get second-line 'justification)) '("Idisj-L" "Idisj-R") :test 'string=))
		  (progn (if (or (string= (car (get first-line 'justification)) "Idisj-R")
				 (string= (car (get second-line 'justification)) "Idisj-R"))
			     (setq doing-a nil)
			   (setq doing-a t))
			 (if doing-a 
			     (setf (get line 'assertion) (cadr (get line 'assertion))) ;let the wff be A
			   (setf (get line 'assertion) (cdr (get line 'assertion)))) ;let the wff be A'
			 (if (or doing-a (string= (car (get first-line 'justification)) "Idisj-R"))
			     (progn (setf (car (get first-line 'justification)) "Same as")
				    (if doing-a 
					(setf (get first-line 'assertion) (cdar (get first-line 'assertion)))
				      (setf (get first-line 'assertion) (cdr (get first-line 'assertion)))))
			   (progn (setf (car (get first-line 'justification)) "EquivWffs")
				  (setf (get first-line 'assertion) (cdr (get first-line 'assertion)))))
			 (if (or doing-a (string= (car (get second-line 'justification)) "Idisj-R"))
			     (progn (setf (car (get second-line 'justification)) "Same as")
				    (if doing-a 
					(setf (get second-line 'assertion) (cdar (get second-line 'assertion)))
				      (setf (get second-line 'assertion) (cdr (get second-line 'assertion)))))
			   (progn (setf (car (get second-line 'justification)) "EquivWffs")
				  (setf (get second-line 'assertion) (cdr (get second-line 'assertion))))))
		(push line other-lines))))
    (dolist (iline indirect-lines)
	    (let ((hypline (car (last (get (caaddr (get iline 'justification)) 'hypotheses))))
		  (lines (get dproof 'lines))
		  (notable-lines nil))
	      (dolist (line lines)
		      (when (and (string= (car (get line 'justification)) "Neg")
				 (consp (get line 'assertion))
				 (eq hypline (caaddr (get line 'justification))))
			    (push (linealias line) notable-lines)))
    ;notable-lines is now the list of lines of the form "~A and ~(def A)" which are created by 
    ;equivwffs from a line of the form "A"
	      (dolist (line notable-lines)
		      (let ((correct-lines nil)
			    (wff (cdr (get (numalias line) 'assertion))))
			(dolist (l lines)
				(when (memq line (mapcar #'linealias (caddr (get l 'justification))))
				      (if (wffeq (get l 'assertion) wff)
					  (push l correct-lines)
					(progn (setq correct-lines nil) (return nil)))))
	      ;now correct-lines is the set of all the lines of form "~(def A)" which are derived 
	      ;from the line "~A and ~(def A)"
			(if correct-lines
			    (progn (dolist (l correct-lines)
					   (setf (get l 'justification) 
						 (list "EquivWffs" nil (caddr (get (numalias line) 'justification)))))
				   (dellines (list (numalias line))))
			  (setf (get (numalias line) 'assertion) (cdr (get (numalias line) 'assertion))))))
	      (setf (get iline 'assertion) (cdar (get iline 'assertion)))
	      (setf (get hypline 'assertion) (cons 'not (get iline 'assertion))))))
  (cleanup-same))

;;;from here to the asterisks isn't used anywhere in TPS yet... MB Wed May 28 23:37:28 1997
(defun compress-tree ()
  (let ((lines (get dproof 'lines))
	(*marked-lines* nil))
    (declare (special *marked-lines*))
    (mark-lines lines)
    (compress-remove-lines (car (last lines)))))

(defun mark-lines (lines)
  (declare (special *marked-lines*))
  (let ((justs (apply 'append (mapcar #'(lambda (x) (caddr (get x 'justification))) lines))))
    (do ((j justs (cdr j)))
	((null j))
	(when (memq (car j) (cdr j))
	      (pushnew (car j) *marked-lines*)))
    (dolist (line lines)
	    (when (or (member (car (line-justification line)) '("UI" "Choose" "Hyp") :test 'string=)
		      (memq line (line-hypotheses line)))
		  (pushnew line *marked-lines*)))))

#+comment(defun mark-lines (lines)
  (declare (special *marked-lines*))
  (let ((justs (apply 'append (mapcar #'(lambda (x) (caddr (get x 'justification))) lines))))
    (do ((j justs (cdr j)))
	((null j))
	(when (memq (car j) (cdr j)) (pushnew (car j) *marked-lines*))))
  (get-tree-same (car (last lines)))
  (setq *marked-lines* (remove-duplicates *marked-lines*))
  (dolist (line lines)
	  (when (or nil ;(member (car (line-justification line)) '("UI" "Choose" "Hyp") :test 'string=)
		    (memq line (line-hypotheses line)))
		(pushnew line *marked-lines*))))

(defun get-tree-same (line)
  (declare (special *marked-lines*))
  (if (null (caddr (get line 'justification)))
      (pushnew line *marked-lines*)
    (dolist (j (caddr (get line 'justification)))
	    (if (or (string= (car (line-justification line))
			     (car (line-justification j)))
		    (member (car (line-justification line))
			    (member (car (line-justification j))
				    '("Equiv-eq" "Lambda" "EquivImp" "ImpEquiv" "Conj"
				      "Equiv-eq" "Lambda" "EquivImp" "ImpEquiv" "Conj")
				    :test 'string=)
			    :test 'string=)
		    (and (member (car (line-justification line)) '("AB" "Lambda") :test 'string=)
			 (member (car (line-justification j))
				 '("Egen" "Ugen" "UI" "Choose")
				 :test 'string=))
		    (and (member (car (line-justification j)) '("AB" "Lambda") :test 'string=)
			 (member (car (line-justification line))
				 '("Egen" "Ugen" "UI" "Choose")
				 :test 'string=)))
		(get-tree-same j)
	      (progn (pushnew j *marked-lines*) (get-tree-same j))))))
	    
(defun compress-remove-lines (line)
  (let ((stops nil)
	(deletes nil))
    (declare (special stops deletes))
    (get-tree-above line)
    (setq deletes (remove-if #'(lambda (x) (memq x stops)) deletes))
    (when deletes
	  (msgf "Suggest deleting lines " (mapcar 'linealias deletes) t
		"  and justifying line " (linealias line) " from lines " (mapcar 'linealias stops))
	  (setf (line-justification line)
		(list (if (member (car (line-justification line))
				  '("Equiv-eq" "Lambda" "EquivImp" "ImpEquiv" "Conj")
				  :test 'string=)
			  "Eq/lambda"
			(concatenate 'string (car (line-justification line)) "+"))
		      NIL
		      stops))
	  (dellines deletes))
    (dolist (s (remove-if #'(lambda (x) (null (caddr (get x 'justification)))) stops))
	    (compress-remove-lines s))))

(defun get-tree-above (line)
  (declare (special stops deletes *marked-lines*))
  (if (null (caddr (get line 'justification)))
      (pushnew line stops)
    (dolist (j (caddr (get line 'justification)))
	    (if (memq j *marked-lines*)
		(pushnew j stops)
	      (progn (pushnew j deletes)
		     (get-tree-above j))))))
;;;******************************************************************

(context otl-status) 

(defmexpr are-we-using
  (argtypes linelist)
  (argnames linelist)
  (arghelp "List of line numbers")
  (mhelp "Determines if given lines are being used to justify 
any other lines. Notice that the argument is a list of lines,
not a range (i.e. 1 2 3 4 rather than 1--4)."))

(defun are-we-using (linelist)
  (dolist (line linelist)
	  (are-we-using-2 line)))

(defun are-we-using-2 (line)
  (let ((lines (proof-lines dproof))
	(flag t))
    (dolist (l1 lines)
	    (let ((l2 (linealias l1)))
	      (if (neq l2 line)
		  (let ((just (caddr (get l1 'justification))))
		    (if (memq line (mapcar #'linealias just))
			(progn 
			  (msgf "Line " line " justifies line " (linealias l1) "." t)
			  (setq flag nil)))))))
	    (if flag (msgf "Line " line " is not used to justify any line in this proof." t))))
