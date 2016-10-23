;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of WFF-EDITOR)

;;;
;;; File: EDTOP
;;; Package: WFF-EDITOR
;;;
;;; defines the editor top-level as well as the ED command.
;;;

(deffile prenex
  (part-of wff-editor)
  (extension lsp)
  (mhelp "Check for prenex equiv of two terms."))

(defmexpr prenex-iterate
  (argtypes symbollist filespec)
  (argnames namestems texfile)
  (arghelp "Exercises to check" "tex file for report")
  (defaultfns (lambda (namestems texfile)
		(list namestems
		      (if (eq texfile '$)
			  (make-pathname%
			   :name "prenex-report"
			   :type "tex")
			texfile))))
  (mhelp "Given a list of exercises, say (FOO1 FOO2), checks all saved wffs with
names FOO{1,2}-NAME or NAME-FOO{1,2} and produces a report in texfile.tex listing
1) whether FOO{1,2}-NAME is in prenex normal form
2) whether FOO{1,2}-NAME is equivalent to FOO{1,2}."))

					; I've written this using unwind-protect so that errors in search do not prevent
					; reporting. - cebrown 10/24/99
					; Also, I have modified this to take a list of names of gwff's
					; instead of just one. - cebrown 10/24/99
(defun prenex-iterate (names texfile)
  (let (checklist prenex-iterate-retlist)
    (declare (special prenex-iterate-retlist))
    (dolist (name names)
      (dolist (g global-savedwfflist)
	(when (and (> (length (princ-to-string g)) (length (princ-to-string name)))
		   (or (string= (princ-to-string name) (princ-to-string g) :end2 (length (princ-to-string name)))
		       (string= (princ-to-string name) (princ-to-string g) ; cebrown, added this to recognize
				:start2 (- (length (princ-to-string g)) ; NAME-X2200 as well as X2200-NAME
					   (length (princ-to-string name))))))
	  (push (cons name g) checklist))))
    (unwind-protect
	(prenex-iterate-checklist checklist)
      (prenex-iterate-report prenex-iterate-retlist texfile)
      (msgf "Accumulated Error Messages May Appear.  These may be safely ignored."))))

					; side effect: updates prenex-iterate-retlist
					; I've written this using unwind-protect so that errors in search do not prevent
					; processing the rest of the formulas and reporting  - cebrown 10/24/99
(defun prenex-iterate-checklist (checklist)
  (declare (special prenex-iterate-retlist))
  (if checklist
      (let ((temp nil))
	(unwind-protect
	    (setq temp (prenex-wffeq (caar checklist) (cdar checklist)))
	  (progn
	    (if temp
		(push (cons temp (car checklist)) prenex-iterate-retlist)
	      (push (cons (cons t nil) (car checklist)) prenex-iterate-retlist))
	    (prenex-iterate-checklist (cdr checklist)))))
    nil))

(defun prenex-iterate-report (retlist texfile)
  (setq retlist (sort retlist #'(lambda (x y)
				  (or (string< (cadr x) (cadr y))
				      (and (eq (cadr x) (cadr y))
					   (string< (cddr x) (cddr y)))))))
  (prenex-iterate-report-w retlist)	; to screen
  (reroute-output texfile		; this is essentially copied from texproof in otl-scribeout.lisp - cebrown 10/25/99
		  (namestring (make-pathname%  ; print report to tex file
			       :name "prenex-report"
			       :type "tex"))
		  (progn
		    (if latex-emulation
			(msgf latex-preamble t "\\markhack{{\\bf PRENEX REPORT}\\hfill " 
				(stringdt nil) "\\hfill{\\bf " (status-userid) "}}")
			(progn (princ "\\input ") (princ tpstex) (terpri)
			       (princ "\\headline={\\noindent{\\bf PRENEX REPORT}\\hfil\\folio\\hfil{\\bf ")
			       (princ (status-userid))
			       (princ "}}") (msgf "\\vskip36pt " t)
			       (princ "\\footline={\\hfil ") (princ (stringdt nil)) (princ "}")))
		      (if use-internal-print-mode
			  (if tex-mimic-scribe
			      (in-mode tex-1-otl
				       (unless latex-emulation (msgf tex-1-preamble))
				       (prenex-iterate-report-tex retlist)
				       (if latex-emulation (msgf latex-postamble) (msgf tex-1-postamble)))
			    (in-mode tex-otl
				     (unless latex-emulation (msgf tex-preamble) (msgf *tex-halign-preamble*))
				     (prenex-iterate-report-tex retlist)
				     (msg "}}\\vfill " t)
				     (if latex-emulation (msgf latex-postamble) (msgf tex-postamble))))
			(let ((style 'tex))
			  (unless latex-emulation (msgf tex-preamble) (msgf *tex-align-preamble*))
			  (prenex-iterate-report-tex retlist)
			  (msg "}}\\vfill " t)
			  (if latex-emulation (msgf latex-postamble) (msgf tex-postamble))))
		      )))

(defun prenex-iterate-report-w (retlist)
  (msgf t t t t t t t "RESULTS:" t)
  (let ((current-wff nil))
    (dolist (r retlist)
      (unless (eq current-wff (cadr r))
 	(msgf t t "Results for " (cadr r) ":" t t)
 	(setq current-wff (cadr r)))
      (msgf t t (cddr r) "  " ((get-gwff0 (cddr r)) . gwff) t " is ")
      (if (and (caar r) (cdar r)) (msg "correct (prenex nf and equivalent to " (cadr r) " )")
 	(if (not (caar r)) (msg "incorrect: not in prenex normal form: " (cdar r))
 	  (if (not (cdar r)) (msg "dubious: it's in prenex nf, but I can't prove the equivalence.")))))))

(defun prenex-iterate-report-tex (retlist)
  (msg "RESULTS:\\vskip10pt" t)
  (let ((current-wff nil))
    (dolist (r (sort retlist #'(lambda (x y)
				 (or (string< (cadr x) (cadr y))
				     (and (eq (cadr x) (cadr y))
					  (string< (cddr x) (cddr y)))))))
      (unless (eq current-wff (cadr r))
	(msgf "Results for " (cadr r) ":")
	(msgf "\\vskip10pt ")
	(setq current-wff (cadr r)))
      (msg (cddr r) "  " ((get-gwff0 (cddr r)) . gwff))
      (msgf "{} \\vskip5pt " )
      (if (and (caar r) (cdar r)) (msg "is correct (prenex nf and equivalent to " (cadr r) " )" t "\\vskip10pt " t)
	(if (not (caar r)) (msg "is incorrect: not in prenex normal form: " (cdar r) t "\\vskip10pt " t)
	  (if (not (cdar r)) (msg "is dubious: it's in prenex nf, but I can't prove the equivalence." t "\\vskip10pt " t)))))))

(defmexpr prenex-wffeq
  (argtypes gwff gwff)
  (argnames wff1 wff2)
  (arghelp "original wff" "wff in prenex nf")
  (mhelp "Tests for equivalence of two terms modulo prenex normal form. 
Wff2 should be in prenex nf."))

(defvar *prenex-error*)

(defun prenex-wffeq (wff1 wff2)
  (when (or (label-p wff1) (symbolp wff1)) (setq wff1 (get-gwff0 wff1)))
  (when (or (label-p wff2) (symbolp wff2)) (setq wff2 (get-gwff0 wff2)))
  (setq *prenex-error* "")
  (if (valid-prenex-normal-form wff2) 
      (cons t (prenex-really-eq (cons (cons 'equiv wff1) wff2)))
    (cons nil *prenex-error*)))
	  

(defun prenex-really-eq (thm)
  (mode 'quiet)
  (msgf "Trying to prove equivalence " (thm . gwff))
  (maint::mate-partial thm t nil)
  (setq *doing-prenex* t)
  (unwind-protect
      (if (eq (catch 'next (matingsearch-controller)) 'fail)
	  nil
	(progn
	  (complete-p) 
	  (if (mating-completep active-mating)
	      (progn (msgf t t "The second wff is in prenex normal form, and the wffs are equivalent") t)
	    (progn (msgf t t "The second wff is in prenex normal form, but I can't finish the proof") nil))))
    (setq *doing-prenex* nil)))

(defun valid-prenex-normal-form (wff &optional (bound nil) (binder-done nil))
  (cond ((lsymbol-q wff) 
	 (if (abbrev-q wff)
	     (if (eq wff 'equiv)	; this is exceptional as an abbreviation - cebrown 10/25/99
		 t
	       (progn (setq *prenex-error* "contains an abbreviation.") nil))
	   (if (equality-p wff) (progn (setq *prenex-error* "contains an equality.") nil)
	     (if (binder-p wff) (progn (setq *prenex-error* "contains an embedded binder.") nil)
	       t))))
	((boundwff-q wff)
	 (if binder-done (progn (setq *prenex-error* "contains an embedded quantifier.") nil)
	   (if (not (memq (caar wff) (free-vars-of (cdr wff))))
	       (progn (setq *prenex-error* "contains a vacuous quantifier.") nil)
	     (if (memq (caar wff) bound)
		 (progn (setq *prenex-error* "same variable quantified twice.") nil)
	       (valid-prenex-normal-form (cdr wff) (cons (caar wff) bound))))))
	(t (and (valid-prenex-normal-form (car wff) bound t)
		(valid-prenex-normal-form (cdr wff) bound t)))))
