;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of EXPANSION-TREE)

;;;
;;; File: FTREE-SEQ  - cebrown - 6/14/01
;;; Definition and functions for a sequent calculus similar to ftrees
;;; A sequent calculus deriviation with no cuts can be converted to ftrees
;;; in a straightforward way.  The main reason for needing this sequent
;;; calculus is to do cut elimination

(deffile ftree-seq
    (part-of EXPANSION-TREE)
  (extension clisp)
  (mhelp "Implementation of a Sequent Calculus corresponding to Ftrees"))

(defvar *seqder-list* nil)
(defvar *current-seqder* nil)
(defvar ftree-seq-verbose nil)

(context expansion-trees)

; All the rules operate on the beginning of sequents, eg,
;     A, B, Gamma => Delta
;    ------------------------- and+
;     A and B, Gamma => Delta
;
;       Gamma => A,B,Delta
;    --------------------------------- or-
;       Gamma => A or B, Delta
;
;     A, A, Gamma => Delta
;    ------------------------ merge+
;        A, Gamma => Delta
;
; etc.
;
; FOCUS steps are used to bring formulas to the beginning of sequents
;
;       A_0,...,A_{n-1}, Gamma => Delta
;      ------------------------------------------ Focus+n
;       A_{n-1},A_0,..., A_{n-2}, Gamma => Delta

(defstruct (ftree-seq
	    (:print-function print-ftree-seq))
  (pos-wffs nil) ; the members of the left side of the sequent (order is important)
  (neg-wffs nil) ; the members of the right side of the sequent (order is important)
  (pos-rule nil) ; t if principal formula is pos, nil if neg
  (kind nil)				; CON, DIS, IMP, TRUE, FALSE, NEG, REW, EXP, SEL, INIT
					; CUT, MERGE, FOCUS, WEAKEN
  (focus-n 0 :type integer)
  (rew-just nil :type symbol)
  (exp-term nil)
  (sel-var nil)
  (prem1 nil)
  (prem2 nil))

(defflag pseq-use-labels
  (flagtype boolean)
  (default T)
  (subjects etr-nat)
  (mhelp "Set to T if pseq should abbreviate formulas and print a legend."))

(defmexpr seq-to-nat
  (ARGTYPES symbol symbol)
  (ARGNAMES sname prefix)
  (ARGHELP "Sequent Derivation to be Translated" "Name of ND proof to Create")
  (defaultfns (lambda (sname prefix)
		(list (if (and (eq sname '$) *current-seqder*)
			  *current-seqder*
			sname)
		      (if (and (eq prefix '$) dproof)
			  (intern (gensym (format nil "~A-seq" dproof)))
			prefix))))
  (mhelp "Translates a Sequent Calculus Derivation (possibly with Cuts)
to a Natural Deduction Proof"))

(defun new-ftree-seq-der (name der)
  (do ((i 0 (+ i 1))
       (name2 name (intern-str (format nil "~d-~d" name i))))
      ((not (get name2 'FTREE-SEQ))
       (push name2 *seqder-list*)
       (setq *current-seqder* name2)
       (setf (get name2 'FTREE-SEQ) der)
       der)))

(defmexpr seqlist
  (mhelp "Print a list of all sequent calculus derivations currently in memory."))

(defmexpr pseq
  (ARGTYPES symbol)
  (ARGNAMES prefix)
  (ARGHELP "Name of Sequent Derivation")
  (defaultfns (lambda (prefix)
		(if (eq prefix '$)
		    (list *current-seqder*)
		  (list prefix))))
  (mhelp "Print a Sequent Calculus Derivation

SEE ALSO: pseq-use-labels, pseql"))

(defmexpr pseql
  (ARGTYPES symbol integer+ integer+)
  (ARGNAMES prefix lbd ubd)
  (ARGHELP "Name of Sequent Derivation"
	   "Lower Bound for Lines to Print"
	   "Upper Bound for Lines to Print")
  (defaultfns (lambda (prefix lbd ubd)
		(list (if (eq prefix '$) *current-seqder* prefix)
		      (if (eq lbd '$) 1 lbd)
		      (if (eq ubd '$) 100 ubd))))
  (mhelp "Print a Sequent Calculus Derivation

SEE ALSO: pseq-use-labels, pseq"))

(defun print-ftree-seq (d s k)
  (declare (ignore k))
  (if ftree-seq-verbose
      (let ((*standard-output* s)
	    (g (gensym)))
	(declare (special *standard-output*))
	(setf (get g 'ftree-seq) d)
	(pseq g))
    (progn
      (format s "~%" (ftree-seq-kind d))
      (dolist (wff (ftree-seq-pos-wffs d))
	      (pwff wff))
      (format s "~% ->~%")
      (dolist (wff (ftree-seq-neg-wffs d))
	      (pwff wff)))))

(defun seqlist ()
  (let ((lit-labels nil))
    (declare (special lit-labels))
    (dolist (sdname (append (reverse *extseqder-list*) (reverse *seqder-list*)))
      (msgf sdname)
      (if (ext-seq-p (get sdname 'ext-seq))
	  (print-ext-seq (get sdname 'ext-seq) *standard-output* 1)
	(let ((ftree-seq-verbose nil))
	  (declare (special ftree-seq-verbose))
	  (print-ftree-seq (get sdname 'FTREE-SEQ)
			   *standard-output* 1))))))

(defun seqder-to-fake-nd (seqder)
  (declare (special seqnode-line-assoc next-line-no))
  (let ((al (assoc seqder seqnode-line-assoc)))
    (if al
	(cdr al)
      (let ((pl1 (when (ftree-seq-prem1 seqder)
		   (seqder-to-fake-nd (ftree-seq-prem1 seqder))))
	    (pl2 (when (ftree-seq-prem2 seqder)
		   (seqder-to-fake-nd (ftree-seq-prem2 seqder))))
	    (dummyline (gensym)))
	(setf (get dproof 'lines) (append (get dproof 'lines)
					  (list dummyline)))
	(setf (get dproof 'linealiases)
	      (append (get dproof 'linealiases)
		      (list (cons next-line-no dummyline))))
	(setf (get dummyline 'linenumber) next-line-no)
	(incf next-line-no)
	(push (cons seqder dummyline) seqnode-line-assoc)
	(let ((pos-labs (mapcar #'(lambda (w)
				    (if pseq-use-labels
					(ftree-seq-label-wff w)
				      (let ((str-str (make-string-output-stream)))
					(let ((*standard-output* str-str))
					  (declare (special *standard-output*))
					  (pwff w))
					(prog1
					    (string-trim '(#\Newline)
							 (get-output-stream-string str-str))
					  (close str-str)))))
				(ftree-seq-pos-wffs seqder)))
	      (neg-labs (if (= (length (ftree-seq-neg-wffs seqder)) 1)
			    (ftree-seq-neg-wffs seqder)
			  (if pseq-use-labels
			      (mapcar #'ftree-seq-label-wff
				      (ftree-seq-neg-wffs seqder))
			    (ftree-seq-neg-wffs seqder)))))
	  (setf (get dummyline 'hypotheses)
		(mapcar #'(lambda (x)
			    (let ((dummyhyp (gensym)))
			      (setf (get dummyhyp 'linenumber) x)
			      dummyhyp))
			pos-labs))
	  (if neg-labs
	      (do ((neg-labs2 (cdr neg-labs) (cdr neg-labs2))
		   (w (car neg-labs) (acons '|,| w (car neg-labs2))))
		  ((null neg-labs2)
		   (setf (get dummyline 'assertion) w)))
	    (setf (get dummyline 'assertion) '|.|))
	  (setf (get dummyline 'justification)
		(case (ftree-seq-kind seqder)
		      (INIT
		       (list (format nil "~d" (ftree-seq-kind seqder)) nil nil))
		      (TRUE
		       (list "TRUE-R" nil nil))
		      (FALSE
		       (list "FALSE-L" nil nil))
		      (NEG
		       (list (format nil "NEG-~d" 
				     (if (ftree-seq-pos-rule seqder) "L" "R"))
			     nil (list pl1)))
		      (REW
		       (list (format nil "~d-~d"
				     (ftree-seq-rew-just seqder)
				     (if (ftree-seq-pos-rule seqder) "L" "R"))
			     nil (list pl1)))
		      (CON
		       (if (ftree-seq-pos-rule seqder)
			   (list "CON-L" nil (list pl1))
			 (list "CON-R" nil (list pl1 pl2))))
		      (DIS
		       (if (ftree-seq-pos-rule seqder)
			   (list "DIS-L" nil (list pl1 pl2))
			 (list "DIS-R" nil (list pl1))))
		      (IMP
		       (if (ftree-seq-pos-rule seqder)
			   (list "IMP-L" nil (list pl1 pl2))
			 (list "IMP-R" nil (list pl1))))
		      (EXP
		       (if (ftree-seq-pos-rule seqder)
			   (list "ALL-L" (list (ftree-seq-exp-term seqder))
				 (list pl1))
			 (list "EX-R" (list (ftree-seq-exp-term seqder))
			       (list pl1))))
		      (SEL
		       (if (ftree-seq-pos-rule seqder)
			   (list "EX-L" (list (ftree-seq-sel-var seqder))
				 (list pl1))
			 (list "ALL-R" (list (ftree-seq-sel-var seqder))
			       (list pl1))))
		      (CUT
		       (list "CUT" nil (list pl1 pl2)))
		      (MERGE
		       (list (format nil "MERGE-~d"
				     (if (ftree-seq-pos-rule seqder) "L" "R"))
			     nil (list pl1)))
		      (FOCUS
		       (list (format nil "FOCUS-~d-~d"
				     (if (ftree-seq-pos-rule seqder) "L" "R")
				     (ftree-seq-focus-n seqder))
			     nil
			     (list pl1)))
		      (WEAKEN
		       (list (format nil "WEAKEN-~d"
				     (if (ftree-seq-pos-rule seqder) "L" "R"))
			     nil
			     (list pl1)))))
	  dummyline)))))

(defun pseq (prefix)
  (pseql prefix nil nil))

(defun pseql (prefix lbd ubd)
  (let ((lit-labels nil))
    (declare (special lit-labels))
    (if (not (get prefix 'ftree-seq))
	(if (get prefix 'ext-seq)
	    (pext-seql prefix lbd ubd)
	  (throwfail prefix " is not the name of a sequent calculus derivation"))
      (progn
	(setq *current-seqder* prefix)
	(let* ((seqder (get prefix 'FTREE-SEQ))
	       (seqnode-line-assoc nil)
	       (next-line-no 1)
	       (dproof (if pseq-use-labels
			   (or (get prefix 'seqder-lab-fake-nd) (gensym))
			 (or (get prefix 'seqder-fake-nd) (gensym))))
	       (old-print-line-gen (get 'generic 'print-line))
	       (old-print-line-xterm (get 'xterm 'print-line))
	       (comma-infix (get '|,| 'infix)))
	  (declare (special dproof seqnode-line-assoc next-line-no))
	  (unwind-protect
	      (progn
		(setf (get 'generic 'print-line) #'print-seq-line-generic)
		(setf (get 'xterm 'print-line) #'print-seq-line-generic)
		(setf (get '|,| 'infix) 4)
		(if pseq-use-labels
		    (if (get prefix 'seqder-lab-fake-nd)
			(setq lit-labels (get prefix 'seqder-labels))
		      (progn
			(seqder-to-fake-nd seqder)
			(setf (get prefix 'seqder-lab-fake-nd) dproof)
			(setf (get prefix 'seqder-labels) lit-labels)))
		  (progn
		    (unless (get prefix 'seqder-fake-nd)
		      (seqder-to-fake-nd seqder))
		    (setf (get prefix 'seqder-fake-nd) dproof)))
		(if (and lbd ubd)
		    (dolist (line (proof-lines dproof))
		      (when (and (>= (line-linenumber line) lbd)
				 (<= (line-linenumber line) ubd))
			(%prtline line)))
		  (pall)))
	    (progn
	      (setf (get 'generic 'print-line) old-print-line-gen)
	      (setf (get 'xterm 'print-line) old-print-line-xterm)
	      (setf (get '|,| 'infix) comma-infix))))
	(when pseq-use-labels
	  (print-lit-legend lit-labels))))))

(defun print-lit-legend (lit-labels)
  (when lit-labels
    (msgf "*** Legend:"))
  (dolist (l (reverse lit-labels))
	  (msgf (cdr l) ": " ((car l) . gwff))))

(defun ftree-seq-label-wff (w)
  (declare (special lit-labels))
  (let ((a (assoc w lit-labels :test #'wffeq)))
    (if a
	(cdr a)
      (let ((l (intern-str (next-ftree-seq-lit-label))))
	(setq lit-labels (acons w l lit-labels))
	l))))

(defun next-ftree-seq-lit-label ()
  (declare (special lit-labels))
  (let* ((n (length lit-labels))
	 (i (/ n 26)))
    (multiple-value-bind
     (q r)
     (floor i)
     (format nil "~d~d"
	     (nth (floor (* r 26))
		  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"
		    "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
	     (if (> q 0)
		 q
	       "")))))

      
(defun print-seq-line-generic (line)
  (let ((hatomalist nil) (curpos 0) justificationlength)
    (declare (special hatomalist))
    (msg "(" (linealias line) ") ") 
    (if (eq turnstile-indent-auto 'compress) 
	(setq curpos (+ curpos 3 (length (princ-to-string (linealias line)))))
      (progn (spaces (- 3 (length (princ-to-string (linealias line)))))
	     (setq curpos 6)))
    (let ((hyp (format nil "~{ ~d ,~}" (mapcar #'linealias (get line 'hypotheses)))))
      (setq curpos (+ curpos (length (string-right-trim '(#\,) hyp))))
      (princ (string-right-trim '(#\,) hyp))
      (if (neq (length (string-right-trim '(#\,) hyp)) 0) (progn (princ " ") (setq curpos (1+ curpos)))))
    (if (eq turnstile-indent-auto 'fix) 
	(progn (if (> curpos turnstile-indent) (progn (msg t) (setq curpos -1)))
	       (indentation turnstile-indent) (setq curpos (1- turnstile-indent)))
      (if (eq turnstile-indent-auto 'min) 
	  (progn (let ((point (1+ (core::figure-out-indent)))) (indentation point) (setq curpos (1- point))))))
    (pprinc "->")
    (if (eq turnstile-indent-auto 'compress) (setq curpos (1+ curpos)) (setq curpos (+ 2 curpos)))
    (cond ((not (fixp rightmargin))
	   (setq rightmargin (linewidth))))
    (if (eq turnstile-indent-auto 'compress)
	(setq hatomalist 
	      (prtwff (get line 'assertion)
		      (leftmargin (+ 1 curpos))))
      (setq hatomalist 
	    (prtwff (get line 'assertion)
		    (leftmargin (+ 2 curpos)))))
    (setq justificationlength (justlength (get line 'justification)))
    ;;Call this function after wff has been printed, because the
    ;;variable HATOMALIST is set when the wff is being printed. SI.
    (when (> (+ curpos justificationlength) rightmargin)
      (setq curpos 0)
      (terpri))
    (indentation (1+ (- rightmargin justificationlength)))
    (princjustification (get line 'justification))
    (setq curpos rightmargin)
    (when (and print-comments (get line 'comment) (listp (get line 'comment))) ;if it's a string, it's ""
	  (msg t "   ")
	  (eval (get line 'comment))
	  (msg t)
	  (setq curpos 0))))

; returns three values:
;  list of pos ftrees &
;  list of neg ftrees (in order they occur in fs)
;  and conn list of ftree nodes giving complete mating
(defun cutfree-ftree-seq-to-ftrees (fs)
  (let ((ftree-clist nil))
    (declare (special ftree-clist))
    (multiple-value-bind
     (pw1 nw1)
     (cutfree-ftree-seq-to-ftrees-rec fs)
   (let ((ftrees (append pw1 nw1)))
     (dolist (conn ftree-clist)
	     (unless (find-ftree-node-list (ftree-name (car conn)) ftrees)
	       (throwfail "Missing node " (car conn) " in conn " conn))
	     (unless (find-ftree-node-list (ftree-name (cdr conn)) ftrees)
	       (throwfail "Missing node " (cdr conn) " in conn " conn))))
   (values pw1 nw1 ftree-clist))))

(defun cutfree-ftree-seq-to-ftrees-rec (fs)
  (declare (special ftree-clist))
  (let ((pos-wffs (ftree-seq-pos-wffs fs))
	(neg-wffs (ftree-seq-neg-wffs fs))
	(prem1 (ftree-seq-prem1 fs))
	(prem2 (ftree-seq-prem2 fs)))
    (if (eq (ftree-seq-kind fs) 'INIT)
	(let* ((pwff (car pos-wffs)) ; should be the same as (car neg-wffs)
	       (p (make-ftree-leaf pwff t))
	       (n (make-ftree-leaf pwff nil)))
	  (push (cons p n) ftree-clist)
	  (values (list p) (list n)))
      (if (ftree-seq-pos-rule fs)
					; principal formula is on the left
	  (case (ftree-seq-kind fs)
		(FALSE (values (list (make-ftree-false t)) nil))
		(TRUE (throwfail "TRUE can only be used on the right"))
		(DIS
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (multiple-value-bind
		   (pos2 neg2)
		   (cutfree-ftree-seq-to-ftrees-rec prem2)
		   (values
		    (cons (make-ftree-dis (car pos1) (car pos2))
			  (append (cdr pos1) (cdr pos2)))
		    (append neg1 neg2)))))
		(CON
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (values
		   (cons (make-ftree-con (car pos1) (cadr pos1))
			 (cddr pos1))
		   neg1)))
		(IMP
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (multiple-value-bind
		   (pos2 neg2)
		   (cutfree-ftree-seq-to-ftrees-rec prem2)
		   (values
		    (cons (make-ftree-imp (car neg1) (car pos2))
			  (append pos1 (cdr pos2)))
		    (append (cdr neg1) neg2)))))
		(NEG
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (values
		   (cons (make-ftree-neg (car neg1))
			 pos1)
		   (cdr neg1))))
		(EXP
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (values
		   (cons (make-ftree-exp (car pos-wffs)
					 (list (ftree-seq-exp-term fs))
					 (list (car pos1)))
			 (cdr pos1))
		   neg1)))
		(SEL
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (values
		   (cons (make-ftree-sel (car pos-wffs)
					 (ftree-seq-sel-var fs)
					 (car pos1))
			 (cdr pos1))
		   neg1)))
		(REW
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (values
		   (cons (make-ftree-rew (car pos-wffs)
					 (ftree-seq-rew-just fs)
					 (car pos1))
			 (cdr pos1))
		   neg1)))
		(WEAKEN
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (values (cons (make-ftree-leaf (car (ftree-seq-pos-wffs fs)) t)
				pos1)
			  neg1)))
		(FOCUS
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (values (focus-nth (ftree-seq-focus-n fs) pos1) neg1)))
		(MERGE
		 (multiple-value-bind
		  (pos1 neg1)
		  (cutfree-ftree-seq-to-ftrees-rec prem1)
		  (let* ((f1 (car pos1))
			 (f2 (cadr pos1))
			 (m (list 'MERGE (list f1 f2)))
			 (merge-result nil))
		    (declare (special merge-result))
		    (dolist (pos (cddr pos1))
			    (setq merge-result (append (ftree-assoc pos pos) merge-result)))
		    (dolist (neg neg1)
			    (setq merge-result (append (ftree-assoc neg neg) merge-result)))
		    (dolist (k (append (ftree-components f1) (ftree-components f2)))
			    (setq merge-result (append (ftree-assoc k k) merge-result)))
		    (setq merge-result
			  (acons f1 m (acons f2 m merge-result)))
		    (merge-ftree-real)
                    (flet ((f-a (g) (or (cdr (assoc g merge-result))
					g)))
			  (setq ftree-clist
				(mapcar #'(lambda (x)
					    (cons (f-a (car x))
						  (f-a (cdr x))))
					ftree-clist)))
		    (values (cons (cdr (assoc f1 merge-result))
				  (mapcar #'(lambda (f)
					      (cdr (assoc f merge-result)))
					  (cddr pos1)))
			    (mapcar #'(lambda (f)
					(cdr (assoc f merge-result)))
				    neg1)))))
		(CUT (throwfail "Cannot translate CUT to ftree - first perform cut elim")))
					; principal formula is on the right
	(case (ftree-seq-kind fs)
	      (TRUE (values nil (list (make-ftree-true nil))))
	      (FALSE (throwfail "FALSE can only be used on the left"))
	      (CON
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(multiple-value-bind
		 (pos2 neg2)
		 (cutfree-ftree-seq-to-ftrees-rec prem2)
		 (values
		  (append pos1 pos2)
		  (cons (make-ftree-con (car neg1) (car neg2))
			(append (cdr neg1)
				(cdr neg2)))))))
	      (DIS
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(values
		 pos1
		 (cons (make-ftree-dis (car neg1) (cadr neg1))
		       (cddr neg1)))))
	      (IMP
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(values
		 (cdr pos1)
		 (cons (make-ftree-imp (car pos1) (car neg1))
		       (cdr neg1)))))
	      (NEG
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(values
		 (cdr pos1)
		 (cons (make-ftree-neg (car pos1)) neg1))))
	      (EXP
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(values pos1
			(cons (make-ftree-exp (car neg-wffs)
					      (list (ftree-seq-exp-term fs))
					      (list (car neg1)))
			      (cdr neg1)))))
	      (SEL
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(values pos1
			(cons (make-ftree-sel (car neg-wffs)
					      (ftree-seq-sel-var fs)
					      (car neg1))
			      (cdr neg1)))))
	      (REW
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(values pos1
			(cons (make-ftree-rew (car neg-wffs)
					      (ftree-seq-rew-just fs)
					      (car neg1))
			      (cdr neg1)))))
	      (WEAKEN
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(values pos1
			(cons (make-ftree-leaf (car (ftree-seq-neg-wffs fs)) nil)
			      neg1))))
	      (FOCUS
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(values pos1 (focus-nth (ftree-seq-focus-n fs) neg1))))
	      (MERGE
	       (multiple-value-bind
		(pos1 neg1)
		(cutfree-ftree-seq-to-ftrees-rec prem1)
		(let* ((f1 (car neg1))
		       (f2 (cadr neg1))
		       (m (list 'MERGE (list f1 f2)))
		       (merge-result nil))
		  (declare (special merge-result))
		  (dolist (pos pos1)
			  (setq merge-result (append (ftree-assoc pos pos) merge-result)))
		  (dolist (neg (cddr neg1))
			  (setq merge-result (append (ftree-assoc neg neg) merge-result)))
		  (dolist (k (append (ftree-components f1) (ftree-components f2)))
			  (setq merge-result (append (ftree-assoc k k) merge-result)))
		  (setq merge-result (acons f1 m (acons f2 m merge-result)))
		  (merge-ftree-real)
		  (flet ((f-a (g) (or (cdr (assoc g merge-result))
				      g)))
			(setq ftree-clist
			      (mapcar #'(lambda (x)
					  (cons (f-a (car x))
						(f-a (cdr x))))
				      ftree-clist)))
		  (values (mapcar #'(lambda (f)
				      (cdr (assoc f merge-result)))
				  pos1)
			  (cons (cdr (assoc f1 merge-result))
				(mapcar #'(lambda (f)
					    (cdr (assoc f merge-result)))
					(cddr neg1)))))))
	      (CUT (throwfail "Cannot translate CUT to ftree - first perform cut elim")))))))

(defun focus-nth (n l)
  (if (= n 0)
      l
    (cons (nth n l) (remove-nth n l))))

(defun unfocus-nth (n l)
  (if (= n 0)
      l
    (unfocus-nth-rec n (car l) (cdr l))))

(defun unfocus-nth-rec (n a l)
  (if (= n 0)
      (cons a l)
    (cons (car l) (unfocus-nth-rec (- n 1) a (cdr l)))))

(defun focus-preimage (nfoc n)
  (if (= n 0)
      nfoc
    (if (<= n nfoc)
	(- n 1)
      n)))

(defun focus-image (nfoc n)
  (if (= n nfoc)
      0
    (if (< n nfoc)
	(+ n 1)
      n)))

(defun remove-nth (n l)
  (if (and (> n 0) l)
      (cons (car l) (remove-nth (- n 1) (cdr l)))
    (if (= n 0)
	(if l
	    (cdr l)
	  (throwfail "remove-nth error"))
      l)))

					; eliminate all cuts
(defun ftree-seq-cut-elim (fs)
  (let* ((prem1 (ftree-seq-prem1 fs))
	 (prem2 (ftree-seq-prem2 fs))
	 (prem3 (if prem1 (ftree-seq-cut-elim prem1) nil))
	 (prem4 (if prem2 (ftree-seq-cut-elim prem2) nil)))
    (if (eq (ftree-kind fs) 'CUT)
	(multiple-value-bind
	 (fselim0 posinds neginds)
	 (ftree-seq-mix-elim-1 prem3 prem4 (list 0) (list 0))
	 (ftree-seq-mix-elim-finish prem3 prem4 fselim0 posinds neginds))
      (if (and (equal prem1 prem3) (equal prem2 prem4))
	  fs
	(let ((fs2 (copy-ftree-seq fs)))
	  (setf (ftree-seq-prem1 fs2) prem3)
	  (setf (ftree-seq-prem2 fs2) prem4)
	  fs2)))))

(defun ftree-seq-mix-elim-finish (fs1 fs2 fs posinds neginds)
  (do ((n (- (length (ftree-seq-pos-wffs fs2)) 1) (- n 1)))
      ((= n 0))
      (multiple-value-setq
       (fs posinds)
       (ftree-seq-bring-to-front
	(cons nil n) (nth n (ftree-seq-pos-wffs fs2))
	fs posinds t)))
  (do ((n (- (length (ftree-seq-pos-wffs fs1)) 1) (- n 1)))
      ((< n 0))
      (multiple-value-setq
       (fs posinds)
       (ftree-seq-bring-to-front
	(cons t n) (nth n (ftree-seq-pos-wffs fs1))
	fs posinds t)))
  (do ((n (- (length (ftree-seq-neg-wffs fs2)) 1) (- n 1)))
      ((< n 0))
      (multiple-value-setq
       (fs neginds)
       (ftree-seq-bring-to-front
	(cons nil n) (nth n (ftree-seq-neg-wffs fs2))
	fs neginds nil)))
  (do ((n (- (length (ftree-seq-neg-wffs fs1)) 1) (- n 1)))
      ((= n 0))
      (multiple-value-setq
       (fs neginds)
       (ftree-seq-bring-to-front
	(cons t n) (nth n (ftree-seq-neg-wffs fs1))
	fs neginds nil)))
  fs)

(defun ftree-seq-bring-to-front (ind wff fs inds pos)
  (let ((num 0))
    (do ((inds2 inds (cdr inds2))
	 (n 0 (+ n 1)))
	((null inds2))
	(when (equal ind (car inds2))
	  (setq inds
		(focus-nth n inds))
	  (setq fs (make-ftree-seq-focus n fs pos))
	  (incf num)))
    (if (= num 0)
	(progn
	  (setq fs (make-ftree-seq-weaken wff fs pos))
	  (setq inds (cons ind inds)))
      (dotimes (i (- num 1))
	       (if pos
		   (setq fs (make-ftree-seq-merge-pos fs))
		 (setq fs (make-ftree-seq-merge-neg fs)))
	       (setq inds (cdr inds))))
    (values fs inds)))
      
		


					; eliminate a single cut, assuming fs1 and fs2 are cut free,
					; nl1 gives positions of neg mix wffs in fs1,
					; nl2 gives position of pos mix wffs in fs2
					; returns fs::Gamma -> Delta
					; posinds list of (<bool> . <int>)
					; neginds list of (<bool> . <int>)
					; where posinds and Gamma have the same length
					; and neginds and Delta have the same length
					; (<bool> . <int>) indicates to which wff in fs1,fs2
					; the member of Gamma/Delta corresponds
					; This prevents us from having to do lots of
					; focus/merge/weaken steps
(defun ftree-seq-mix-elim-1 (fs1 fs2 nl1 nl2)
  (let ((neg-mix1 nil)
	(pos-mix2 nil))
    (dolist (n1 nl1)
	    (push (nth n1 (ftree-seq-neg-wffs fs1)) neg-mix1))
    (dolist (n2 nl2)
	    (push (nth n2 (ftree-seq-pos-wffs fs2)) pos-mix2))
    (multiple-value-bind
     (fs posinds neginds)
     (ftree-seq-mix-elim-2 fs1 fs2 nl1 nl2)
     (let ((gamma1 (ftree-seq-pos-wffs fs1))
	   (gamma2 (ftree-seq-pos-wffs fs2))
	   (delta1 (ftree-seq-neg-wffs fs1))
	   (delta2 (ftree-seq-neg-wffs fs2))
	   (gamma3 (ftree-seq-pos-wffs fs))
	   (delta3 (ftree-seq-neg-wffs fs))
	   (n 0))
       (dolist (ind posinds)
	       (if (car ind)
		   (unless (wffeq-ab (nth n gamma3) (nth (cdr ind) gamma1))
		     (throwfail "bad pos mix result: " ind))
		 (progn
		   (when (member (cdr ind) nl2 :test #'=)
		     (throwfail "did not remove pos mix " n))
		   (unless (wffeq-ab (nth n gamma3) (nth (cdr ind) gamma2))
		     (throwfail "bad pos mix result: " ind))))
	       (incf n))
       (setq n 0)
       (dolist (ind neginds)
	       (if (car ind)
		   (progn
		     (when (member (cdr ind) nl1 :test #'=)
		       (throwfail "did not remove neg mix " n))
		     (unless (wffeq-ab (nth n delta3) (nth (cdr ind) delta1))
		       (throwfail "bad neg mix result: " ind)))
		 (unless (wffeq-ab (nth n delta3) (nth (cdr ind) delta2))
		   (throwfail "bad neg mix result: " ind)))
	       (incf n)))
     (values fs posinds neginds))))

(defun ftree-seq-mix-elim-2 (fs1 fs2 nl1 nl2)
  (if nl1
      (if nl2
	  (cond ((eq (ftree-seq-kind fs1) 'INIT)
					; may need to do a focus on fs2 to get the proper order
		 (let* ((cut1 (car (ftree-seq-neg-wffs fs1)))
			(fs3 fs2)
			(posinds nil)
			(neginds nil))
		   ; replace mix wff's with equivalent wff
		   (dolist (n nl2)
			   (setq fs3 (ftree-seq-replace-equivwffs cut1 fs3 n t)))
		   (dotimes (n (length (ftree-seq-neg-wffs fs2)))
			    (push (cons nil n) neginds))
		   (dotimes (n (length (ftree-seq-pos-wffs fs2)))
			    (if (member n nl2 :test #'=)
				(push (cons t 0) posinds)
			      (push (cons nil n) posinds)))
		   (values fs3 
			   (reverse posinds)
			   (reverse neginds))))
		((eq (ftree-seq-kind fs2) 'INIT)
		 (let* ((cut2 (car (ftree-seq-pos-wffs fs2)))
			(fs3 fs1)
			(posinds nil)
			(neginds nil))
		   ; replace mix wff's with equivalent wff
		   (dolist (i nl1)
			   (setq fs3 (ftree-seq-replace-equivwffs cut2 fs3 i nil)))
		   (dotimes (n (length (ftree-seq-neg-wffs fs1)))
			    (if (member n nl1 :test #'=)
				(push (cons nil 0) neginds)
			      (push (cons t n) neginds)))
		   (dotimes (n (length (ftree-seq-pos-wffs fs1)))
			    (push (cons t n) posinds))
		   (values fs3
			   (reverse posinds)
			   (reverse neginds))))
		((eq (ftree-seq-kind fs1) 'FOCUS)
		 (let ((pos (ftree-seq-pos-rule fs1))
		       (focus-n (ftree-seq-focus-n fs1)))
		   (multiple-value-bind
		    (fs3 posinds neginds)
		    (ftree-seq-mix-elim-1
		     (ftree-seq-prem1 fs1) fs2
		     (if pos
			 nl1
		       (mapcar #'(lambda (i)
				   (focus-preimage focus-n i))
			       nl1))
		     nl2)
		    (values fs3 
			    (if pos
				(mapcar #'(lambda (x)
					    (if (car x) ; from 1st premiss
						(cons t (focus-image focus-n (cdr x)))
					      x))
					posinds)
			      posinds)
			    (if pos
				neginds
			      (mapcar #'(lambda (x)
					  (if (car x) ; from 1st premiss
					      (cons t (focus-image focus-n (cdr x)))
					    x))
				      neginds))))))
		((eq (ftree-seq-kind fs2) 'FOCUS)
		 (let ((pos (ftree-seq-pos-rule fs2))
		       (focus-n (ftree-seq-focus-n fs2)))
		   (multiple-value-bind
		    (fs3 posinds neginds)
		    (ftree-seq-mix-elim-1
		     fs1 (ftree-seq-prem1 fs2)
		     nl1
		     (if pos
			 (mapcar #'(lambda (i)
				     (focus-preimage focus-n i))
				 nl2)
		       nl2))
		    (values fs3
			    (if pos
				(mapcar #'(lambda (x)
					    (if (car x)
						x
					      (cons nil (focus-image focus-n (cdr x)))))
					posinds)
			      posinds)
			    (if pos
				neginds
			      (mapcar #'(lambda (x)
					  (if (car x)
					      x
					    (cons nil (focus-image focus-n (cdr x)))))
				      neginds))))))
		((and (not (ftree-seq-pos-rule fs1))
		      (member 0 nl1 :test #'=)
		      (eq (ftree-seq-kind fs1) 'REW)
		      (member (ftree-seq-rew-just fs1)
			      '(LAMBDA BETA ETA EQUIVWFFS LEIBNIZ= EXT= REFL=)))
		 (let ((cut11 (car (ftree-seq-neg-wffs (ftree-seq-prem1 fs1)))))
		   (dolist (n2 nl2)
			   (let ((cut2 (nth n2 (ftree-seq-pos-wffs fs2))))
			     (unless (common-defn-eq-refl=-lam-reduct cut11 cut2 nil)
			       (msgf "Nonanalytic use of wffs being equivalent:" t (cut11 . gwff) t " and " t (cut2 . gwff))
			       (msgf "Nontrivial use of Extensionality?")
			       (throwfail "Cut Elimination Failed"))))
		   (ftree-seq-mix-elim-1 (ftree-seq-prem1 fs1) fs2 nl1 nl2)))
		((and (ftree-seq-pos-rule fs2)
		      (member 0 nl2 :test #'=)
		      (eq (ftree-seq-kind fs2) 'REW)
		      (member (ftree-seq-rew-just fs2)
			      '(LAMBDA BETA ETA EQUIVWFFS LEIBNIZ= EXT= REFL=)))
		 (let ((cut21 (car (ftree-seq-pos-wffs (ftree-seq-prem1 fs2)))))
		   (dolist (n1 nl1)
			   (let ((cut1 (nth n1 (ftree-seq-neg-wffs fs1))))
			     (unless (common-defn-eq-refl=-lam-reduct cut1 cut21 nil)
			       (msgf "Nonanalytic use of wffs being equivalent:" t (cut1 . gwff) t " and " t (cut21 . gwff))
			       (msgf "Nontrivial use of Extensionality?")
			       (throwfail "Cut Elimination Failed"))))
		   (ftree-seq-mix-elim-1 fs1 (ftree-seq-prem1 fs2) nl1 nl2)))
		((eq (ftree-seq-kind fs1) 'CUT)
		 (throwfail "ftree-seq-mix-elim-1 expects to get two cutfree derivations" fs1))
		((eq (ftree-seq-kind fs2) 'CUT)
		 (throwfail "ftree-seq-mix-elim-1 expects to get two cutfree derivations" fs2))
		((or (member (ftree-seq-kind fs1) '(WEAKEN MERGE))
		     (not (member 0 nl1 :test #'=))
		     (not (ftree-seq-principal-p fs1 0 nil)))
		 (multiple-value-bind
		  (nl11 nl12)
		  (ftree-seq-invert-position-list fs1 nl1 nil)
		  (multiple-value-bind
		   (fs3 posinds3 neginds3)
		   (ftree-seq-mix-elim-1 (ftree-seq-prem1 fs1) fs2 nl11 nl2)
		   (multiple-value-bind
		    (fs4 posinds4 neginds4)
		    (if (ftree-seq-prem2 fs1)
			(ftree-seq-mix-elim-1 (ftree-seq-prem2 fs1)
					      (ftree-seq-refresh-sel-vars fs2)
					      nl12 nl2)
		      (values nil nil nil))
		    (ftree-seq-mix-elim-imitate-rule fs1 fs3 fs4
						     posinds3 neginds3
						     posinds4 neginds4
						     t)))))
		((or (member (ftree-seq-kind fs2) '(WEAKEN MERGE))
		     (not (member 0 nl2 :test #'=))
		     (not (ftree-seq-principal-p fs2 0 t)))
		 (multiple-value-bind
		  (nl21 nl22)
		  (ftree-seq-invert-position-list fs2 nl2 t)
		  (multiple-value-bind
		   (fs3 posinds3 neginds3)
		   (ftree-seq-mix-elim-1 fs1 (ftree-seq-prem1 fs2) nl1 nl21)
		   (multiple-value-bind
		    (fs4 posinds4 neginds4)
		    (if (ftree-seq-prem2 fs2)
			(ftree-seq-mix-elim-1 (ftree-seq-refresh-sel-vars fs1)
					      (ftree-seq-prem2 fs2) nl1 nl22)
		      (values nil nil nil))
		    (ftree-seq-mix-elim-imitate-rule fs2 fs3 fs4
						     posinds3 neginds3 posinds4 neginds4
						     nil)))))
		(t ; otherwise both are principal
		 (ftree-seq-mix-elim-principal fs1 fs2 nl1 nl2)))
					; else, nl2 is nil
	(let ((posinds nil)
	      (neginds nil))
	  (dotimes (i (length (ftree-seq-pos-wffs fs2)))
		   (push (cons nil i) posinds))
	  (dotimes (i (length (ftree-seq-neg-wffs fs2)))
		   (push (cons nil i) neginds))
	  (values fs2 (reverse posinds) (reverse neginds))))
					; else, nl1 is nil
    (let ((posinds nil)
	  (neginds nil))
      (dotimes (i (length (ftree-seq-pos-wffs fs1)))
	       (push (cons t i) posinds))
      (dotimes (i (length (ftree-seq-neg-wffs fs1)))
	       (push (cons t i) neginds))
      (values fs1 (reverse posinds) (reverse neginds)))))

					; Assume they're not FOCUS's,CUT's,INIT's,MERGE's, or WEAKEN's
					; k1 & k2 cannot be TRUE or FALSE, since the
					; other would have to be MERGE or WEAKEN
					; in particular, fs1 & fs2 must have at least one premiss
(defun ftree-seq-mix-elim-principal (fs1 fs2 nl1 nl2)
  (flet ((preimage-indices (y inds)
			   (let ((i 0)
				 (ml nil))
			     (dolist (x inds ml)
				     (when (equal x y)
				       (push i ml))
				     (incf i))))
	 (compose-indices (inds inds1 inds2)
			  (mapcar #'(lambda (x)
				      (if (car x)
					  (nth (cdr x) inds1)
					(nth (cdr x) inds2)))
				  inds)))
	(multiple-value-bind
	 (nl11 nl12)
	 (ftree-seq-invert-position-list fs1 nl1 nil)
	 (multiple-value-bind
	  (nl21 nl22)
	  (ftree-seq-invert-position-list fs2 nl2 t)
	  (let ((k1 (ftree-seq-kind fs1))
		(k2 (ftree-seq-kind fs2))
		(prem11 (ftree-seq-prem1 fs1))
		(prem21 (ftree-seq-prem1 fs2)))
					; first get rid of nonprincipal mixes
	    (multiple-value-bind
	     (fs3 posinds3 neginds3)
	     (ftree-seq-mix-elim-1 fs1 prem21 nl1 nl21)
	     (multiple-value-bind
	      (fs4 posinds4 neginds4)
	      (ftree-seq-mix-elim-1 (ftree-seq-refresh-sel-vars prem11)
				    (ftree-seq-refresh-sel-vars fs2)
				    nl11 nl2)
	      (cond ((and (eq k1 'REW) (eq k2 'REW))
		     (if (and (eq (ftree-seq-rew-just fs1) 'EQUIV-IMPLICS)
			      (eq (ftree-seq-rew-just fs2) 'EQUIV-IMPLICS))
			 (let ((ml1 (preimage-indices (cons t 0) neginds4))
			       (ml2 (preimage-indices (cons nil 0) posinds3)))
			   (multiple-value-bind
			    (fs5 posinds5 neginds5)
			    (ftree-seq-mix-elim-1 fs4 fs3 ml1 ml2)
			    (values fs5
				    (compose-indices posinds5 posinds4 posinds3)
				    (compose-indices neginds5 neginds4 neginds3))))
		       (throwfail "Cannot Handle Principal Cut Pair with Rewrites " t
				  (ftree-seq-rew-just fs1) t "and " (ftree-seq-rew-just fs2))))
		    ((and (eq k1 'EXP) (eq k2 'SEL))
		     (let ((ml1 (preimage-indices (cons t 0) neginds4))
			   (ml2 (preimage-indices (cons nil 0) posinds3)))
		       (multiple-value-bind
			(fs5 posinds5 neginds5)
			(ftree-seq-mix-elim-1
			 fs4
			 (ftree-seq-substitute (ftree-seq-exp-term fs1) (ftree-seq-sel-var fs2)
					       fs3)
			 ml1 ml2)
			(values fs5
				(compose-indices posinds5 posinds4 posinds3)
				(compose-indices neginds5 neginds4 neginds3)))))
		    ((and (eq k1 'SEL) (eq k2 'EXP))
		     (let ((ml1 (preimage-indices (cons t 0) neginds4))
			   (ml2 (preimage-indices (cons nil 0) posinds3)))
		       (multiple-value-bind
			(fs5 posinds5 neginds5)
			(ftree-seq-mix-elim-1
			 (ftree-seq-substitute (ftree-seq-exp-term fs2) (ftree-seq-sel-var fs1)
					       fs4)
			 fs3 ml1 ml2)
			(values fs5
				(compose-indices posinds5 posinds4 posinds3)
				(compose-indices neginds5 neginds4 neginds3)))))
		    ((eq k1 k2)
		     (case k1 ; start edit from here
			   (CON  ; cut formula A and B
			    (multiple-value-bind
			     (fs5 posinds5 neginds5)
			     (ftree-seq-mix-elim-1 (ftree-seq-prem2 fs1) 
						   fs2
						   nl12 nl2)
			     (multiple-value-bind
			      (fs6 posinds6 neginds6)
			      (ftree-seq-mix-elim-1
			       (ftree-seq-refresh-sel-vars fs5)
			       fs3
			       (preimage-indices (cons t 0) neginds5)
			       (preimage-indices (cons nil 1) posinds3))
			      (let ((al nil)
				    (i 0))
				(dolist (x posinds6)
					(unless (car x)
					  (when (equal (nth (cdr x) posinds3)
						       (cons nil 0))
					    (push i al)))
					(incf i))
				(multiple-value-bind
				 (fs7 posinds7 neginds7)
				 (ftree-seq-mix-elim-1
				  fs4 fs6
				  (preimage-indices (cons t 0) neginds4)
				  al)
				 (let ((posinds nil)
				       (neginds nil))
				   (dolist (x posinds7)
					   (push
					    (if (car x)
						(let ((y (nth (cdr x) posinds4)))
						  (if (car y)
					; from prem11::G1=>A,D1
						      (cons t (cdr y))
					; from fs2::A and B,G3=>D3
						    (cons nil (cdr y))))
					      (let ((y (nth (cdr x) posinds6)))
						(if (car y)
						    (let ((z (nth (cdr y) posinds5)))
						      (if (car z)
					; from prem12::G2=>B,D2
							  (cons t (+ (cdr z)
								     (length
								      (ftree-seq-pos-wffs
								       prem11))))
					; from fs2::A and B,G3=>D3
							(cons nil (cdr z))))
						  (let ((z (nth (cdr y) posinds3)))
						    (if (car z)
					; from fs1::G1,G2=>A and B,D1,D2
							(cons t (cdr z))
					; from prem21::A,B,G3=>D3
						      (cons nil (- (cdr z) 1)))))))
					    posinds))
				   (dolist (x neginds7)
					   (push
					    (if (car x)
						(let ((y (nth (cdr x) neginds4)))
						  (if (car y)
					; from prem11::G1=>A,D1
						      (cons t (cdr y))
					; from fs2::A and B,G3=>D3
						    (push (cons nil (cdr y)) neginds)))
					      (let ((y (nth (cdr x) neginds6)))
						(if (car y)
						    (let ((z (nth (cdr y) neginds5)))
						      (if (car z)
					; from prem12::G2=>B,D2
							  (cons t (+ (- (cdr z) 1)
								     (length (ftree-seq-neg-wffs prem11))))
					; from fs2::A and B,G3=>D3
							(cons nil (cdr z))))
						  (let ((z (nth (cdr y) neginds3)))
						    (if (car z)
					; from fs1::G1,G2=>A and B,D1,D2
							(cons t (cdr z))
					; from prem21::A,B,G3=>D3
						      (cons nil (cdr z)))))))
					    neginds))
				   (values fs7 (reverse posinds) (reverse neginds))))))))
		 (DIS ; cut formula A or B
		  (multiple-value-bind
		   (fs5 posinds5 neginds5)
		   (ftree-seq-mix-elim-1 fs1 (ftree-seq-prem2 fs2) nl1 nl22)
		   (multiple-value-bind
		    (fs6 posinds6 neginds6)
		    (ftree-seq-mix-elim-1
		     fs4 fs5 
		     (preimage-indices (cons t 1) neginds4)
		     (preimage-indices (cons nil 0) posinds5))
		    (let ((al nil)
			  (i 0))
		      (dolist (x neginds6)
			      (when (car x)
				(when (equal (nth (cdr x) neginds4)
					     (cons t 0))
				  (push i al)))
			      (incf i))
		      (multiple-value-bind
		       (fs7 posinds7 neginds7)
		       (ftree-seq-mix-elim-1
			fs6 (ftree-seq-refresh-sel-vars fs3)
			al
			(preimage-indices (cons nil 0) posinds3))
		       (let ((posinds nil)
			     (neginds nil))
			 (dolist (x posinds7)
				 (push
				  (if (car x)
				      (let ((y (nth (cdr x) posinds6)))
					(if (car y)
					    (let ((z (nth (cdr y) posinds4)))
					      (if (car z)
						  ; prem11
						  (cons t (cdr z))
						  ; fs2
						(cons nil (cdr z))))
					  (let ((z (nth (cdr y) posinds5)))
					    (if (car z)
					; fs1
						(cons t (cdr z))
					; prem22
					      (cons nil (+ (- (cdr z) 1)
							    (length (ftree-seq-pos-wffs prem21))))))))
				    (let ((y (nth (cdr x) posinds3)))
				      (if (car y)
					; fs1
					  (cons t (cdr y))
					  ; prem21
					(cons nil (cdr y)))))
				  posinds))
			 (dolist (x neginds7)
				 (push
				  (if (car x)
				      (let ((y (nth (cdr x) neginds6)))
					(if (car y)
					    (let ((z (nth (cdr y) neginds4)))
					      (if (car z)
						  ; prem11
						  (cons t (- (cdr z) 1))
						  ; fs2
						(cons nil (cdr z))))
					  (let ((z (nth (cdr y) neginds5)))
					    (if (car z)
					; fs1
						(cons t (cdr z))
					; prem22
					      (cons nil (+ (cdr z)
							   (length (ftree-seq-neg-wffs prem21))))))))
				    (let ((y (nth (cdr x) neginds3)))
				      (if (car y)
					  ; fs1
					  (cons t (cdr y))
					  ; prem21
					(cons nil (cdr y)))))
				  neginds))
			 (values fs7 (reverse posinds) (reverse neginds))))))))
		 (IMP ; cut formula A imp B
		  (multiple-value-bind
		   (fs5 posinds5 neginds5)
		   (ftree-seq-mix-elim-1 fs1 (ftree-seq-prem2 fs2) nl1 nl22)
		   (multiple-value-bind
		    (fs6 posinds6 neginds6)
		    (ftree-seq-mix-elim-1
		     fs4 fs5
		     (preimage-indices (cons t 0) neginds4)
		     (preimage-indices (cons nil 0) posinds5))
		    (let ((al nil)
			  (i 0))
		      (dolist (x posinds6)
			      (when (car x)
				(when (equal (nth (cdr x) posinds4)
					     (cons t 0))
				  (push i al)))
			      (incf i))
		      (multiple-value-bind
		       (fs7 posinds7 neginds7)
		       (ftree-seq-mix-elim-1
			fs3 (ftree-seq-refresh-sel-vars fs6)
			(preimage-indices (cons nil 0) neginds3)
			al)
		       (let ((posinds nil)
			     (neginds nil))
			 (dolist (x posinds7)
				 (push
				  (if (car x)
				      (let ((y (nth (cdr x) posinds3)))
					(if (car y)
					; from fs1
					    (cons t (cdr y))
					; from prem21
					  (cons nil (+ (cdr y) 1))))
				    (let ((y (nth (cdr x) posinds6)))
				      (if (car y)
					  (let ((z (nth (cdr y) posinds4)))
					    (if (car z)
					; prem11
						(cons t (- (cdr z) 1))
					; fs2
					      (cons nil (cdr z))))
					(let ((z (nth (cdr y) posinds5)))
					  (if (car z)
					; fs1
					      (cons t (cdr z))
					; prem22
					    (cons nil (+ (cdr z)
							 (length (ftree-seq-pos-wffs
								  prem21)))))))))
				  posinds))
			 (dolist (x neginds7)
				 (push
				  (if (car x)
				      (let ((y (nth (cdr x) neginds3)))
					(if (car y)
					; from fs1
					    (cons t (cdr y))
					; from prem21
					  (cons nil (- (cdr y) 1))))
				    (let ((y (nth (cdr x) neginds6)))
				      (if (car y)
					  (let ((z (nth (cdr y) neginds4)))
					    (if (car z)
					; prem11
						(cons t (cdr z))
					; fs2
					      (cons nil (cdr z))))
					(let ((z (nth (cdr y) neginds5)))
					  (if (car z)
					; fs1
					      (cons t (cdr z))
					; prem22
					    (cons nil (+ (cdr z)
							 (- (length
							     (ftree-seq-neg-wffs prem21))
							    1))))))))
				  neginds))
			 (values fs7 (reverse posinds) (reverse neginds))))))))
		 (NEG 
		  (multiple-value-bind
		   (fs5 posinds5 neginds5)
		   (ftree-seq-mix-elim-1 fs3 fs4
					 (preimage-indices (cons nil 0) neginds3)
					 (preimage-indices (cons t 0) posinds4))
		   (let ((posinds nil)
			 (neginds nil))
		     (dolist (x posinds5)
			     (push (if (car x)
				       (let ((y (nth (cdr x) posinds3)))
					 (if (car y)
					     ; from fs1
					     (cons t (cdr y))
					   ; from prem21
					   (cons nil (+ (cdr y) 1))))
				     (let ((y (nth (cdr x) posinds4)))
				       (if (car y)
					   ; from prem11
					   (cons t (- (cdr y) 1))
					   ; from fs2
					 (cons nil (cdr y)))))
				   posinds))
		     (dolist (x neginds5)
			     (push (if (car x)
				       (let ((y (nth (cdr x) neginds3)))
					 (if (car y)
					     ; from fs1
					     (cons t (cdr y))
					   ; from prem21
					   (cons nil (- (cdr y) 1))))
				     (let ((y (nth (cdr x) neginds4)))
				       (if (car y)
					   ; from prem11
					   (cons t (+ (cdr y) 1))
					   ; from fs2
					 (cons nil (cdr y)))))
				   neginds))
		     (values fs5 (reverse posinds) (reverse neginds)))))
		 (t (throwfail "unexpected case in cut elim " k1 " & " k2 " rules"))))
		    (t (throwfail "unexpected case in cut elim " k1 " & " k2 " rules"))))))))))

(defun ftree-seq-principal-p (fs n pos)
  (and (= n 0)
       (equal (ftree-seq-pos-rule fs) pos)
       (not (eq (ftree-seq-kind fs) 'CUT))))

(defun ftree-seq-replace-equivwffs (wff fs n pos)
  (let ((old-wff (if pos
		     (nth n (ftree-seq-pos-wffs fs))
		   (nth n (ftree-seq-neg-wffs fs))))
	(k (ftree-seq-kind fs))
	(prem1 (ftree-seq-prem1 fs))
	(prem2 (ftree-seq-prem2 fs)))
    (if (eq k 'INIT)
	(if pos
	    (ftree-seq-initial-equivwffs wff old-wff)
	  (ftree-seq-initial-equivwffs old-wff wff))
      (if (ftree-seq-principal-p fs n pos)
	  (if (or (and (not (or (not-p wff) (and-p wff) (or-p wff) (implies-p wff)
				(ae-bd-wff-p wff)
				(member wff '(TRUTH FALSEHOOD))))
		       (member k '(EXP SEL NEG IMP CON DIS TRUE FALSE)))
		  (and (not (equiv-p wff))
		       (eq k 'REW)
		       (eq (ftree-seq-rew-just fs) 'EQUIV-IMPLICS)))
	      (let ((lwff (lnorm wff)))
		(if (wffeq lwff wff)
		    (if (and (equals-p wff)
			     (wffeq (cdar wff) (cdr wff)) ; refl=
			     (eq old-wff 'TRUTH))
			(if pos
			    (make-ftree-seq-rew-pos wff 'REFL= fs)
			  (make-ftree-seq-rew-neg wff 'REFL= fs))
		      (if (equals-p wff) ; figure out if it's Leibniz or Ext and
					; expand accordingly
			  (let* ((etp (cdr (type (caar wff))))
				 (qtp (if (a-bd-wff-p old-wff)
					  (type (bindvar old-wff))
					(if (and (lambda-bd-p (caar old-wff))
						 (lambda-bd-p (cdaar old-wff))
						 (a-bd-wff-p (cddaar old-wff)))
					    (type (bindvar (cddaar old-wff)))
					  nil))))
			    (let ((REWRITE-EQUALITIES
				   (if qtp ; qtp is NIL iff prop ext used, ow it's the type of the forall var
				       (if (and (consp qtp) (equal (cons 'O etp) qtp)) ; Leibniz
					   'LEIBNIZ
					 'ALL) ; Func Ext
				     'ALL))) ; Prop Ext
			      (declare (special REWRITE-EQUALITIES))
			      (let* ((wff2 (instantiate-top-equality wff))
				     (lwff2 (lnorm wff2))
				     (fs2 (make-ftree-seq-lrew
					   wff2 (ftree-seq-replace-equivwffs lwff2 fs n pos)
					   pos)))
				(if pos
				    (make-ftree-seq-rew-pos wff
							    (if (eq rewrite-equalities 'LEIBNIZ)
								'LEIBNIZ=
							      'EXT=)
							    fs2)
				  (make-ftree-seq-rew-neg wff
							  (if (eq rewrite-equalities 'LEIBNIZ)
							      'LEIBNIZ=
							    'EXT=)
							  fs2)))))
			(let* ((h (head wff)) ; otherwise expand abbrev at head
			       (abb (if (pmabbrev-p h) (get h 'core::stands-for) h))
			       (wff2 (instantiate-1-from-list wff (list abb)))
			       (lwff2 (lnorm wff2))
			       (fs2
				(make-ftree-seq-lrew
				 wff2 (ftree-seq-replace-equivwffs lwff2 fs n pos) pos)))
			  (if pos
			      (make-ftree-seq-rew-pos wff 'EQUIVWFFS fs2)
			    (make-ftree-seq-rew-neg wff 'EQUIVWFFS fs2)))))
					; lambda normalize wff
		  (let ((fs2 (ftree-seq-replace-equivwffs lwff fs n pos)))
		    (if pos
			(make-ftree-seq-rew-pos wff 'LAMBDA fs2)
		      (make-ftree-seq-rew-neg wff 'LAMBDA fs2)))))
	    (cond ((eq k 'REW)
		   (if (eq (ftree-seq-rew-just fs) 'EQUIV-IMPLICS)
		       (if pos
			   (make-ftree-seq-rew-pos
			    wff 'EQUIV-IMPLICS
			    (ftree-seq-replace-equivwffs
			     (acons 'AND (acons 'IMPLIES (cdar wff) (cdr wff))
				    (acons 'IMPLIES (cdr wff) (cdar wff)))
			     prem1 n pos))
			 (make-ftree-seq-rew-neg
			  wff 'EQUIV-IMPLICS
			  (ftree-seq-replace-equivwffs
			   (acons 'AND (acons 'IMPLIES (cdar wff) (cdr wff))
				  (acons 'IMPLIES (cdr wff) (cdar wff)))
			   prem1 n pos)))
		     (ftree-seq-replace-equivwffs
		      wff prem1 n pos)))
		  ((eq k 'EXP)
		   (let* ((iwff (substitute-l-term-var (ftree-seq-exp-term fs)
						       (bindvar wff) (cdr wff)))
			  (prem3 (ftree-seq-replace-equivwffs iwff prem1 0 pos)))
		     (if pos
			 (make-ftree-seq-exp-pos wff (ftree-seq-exp-term fs) prem3)
		       (make-ftree-seq-exp-neg wff (ftree-seq-exp-term fs) prem3))))
		  ((eq k 'SEL)
		   (let* ((iwff (substitute-l-term-var (ftree-seq-sel-var fs)
						       (bindvar wff) (cdr wff)))
			  (prem3 (ftree-seq-replace-equivwffs iwff prem1 0 pos)))
		     (if pos
			 (make-ftree-seq-sel-pos wff (ftree-seq-sel-var fs) prem3)
		       (make-ftree-seq-sel-neg wff (ftree-seq-sel-var fs) prem3))))
		  ((eq k 'WEAKEN)
		   (make-ftree-seq-weaken wff prem1 pos))
		  ((eq k 'FOCUS)
		   (ftree-seq-imitate-rule
		    fs (ftree-seq-replace-equivwffs wff prem1 (ftree-seq-focus-n fs) pos)
		    nil))
		  ((eq k 'MERGE)
		   (ftree-seq-imitate-rule
		    fs (ftree-seq-replace-equivwffs
			wff (ftree-seq-replace-equivwffs wff prem1 1 pos) 0 pos) nil))
		  ((or (and (eq k 'CON) pos)
		       (and (eq k 'DIS) (not pos)))
		   (ftree-seq-imitate-rule
		    fs (ftree-seq-replace-equivwffs
			(cdar wff) (ftree-seq-replace-equivwffs (cdr wff)
								prem1 1 pos) 0 pos) nil))
		  ((and (eq k 'IMP) (not pos))
		   (ftree-seq-imitate-rule
		    fs (ftree-seq-replace-equivwffs
			(cdr wff) (ftree-seq-replace-equivwffs
				   (cdar wff) prem1 0 t) 0 nil) nil))
		  ((or (and (eq k 'CON) (not pos))
		       (and (eq k 'DIS) pos))
		   (ftree-seq-imitate-rule
		    fs
		    (ftree-seq-replace-equivwffs (cdar wff) prem1 0 pos)
		    (ftree-seq-replace-equivwffs (cdr wff) prem2 0 pos)))
		  ((and (eq k 'IMP) pos)
		   (ftree-seq-imitate-rule
		    fs
		    (ftree-seq-replace-equivwffs (cdar wff) prem1 0 nil)
		    (ftree-seq-replace-equivwffs (cdr wff) prem2 0 t)))
		  ((eq k 'NEG)
		   (ftree-seq-imitate-rule
		    fs (ftree-seq-replace-equivwffs (cdr wff) prem1 0 (not pos)) nil))
		  ((member k '(TRUE FALSE)) ; we ensured above that wff is not some
					; wff with abbrev's that expand & l-norm to TRUTH/FALSEHOOD
		   fs)
		  (t ; (CUT has no princ wff)
		   (throwfail "Unexpected case in ftree-seq-replace-equivwffs: " fs))))
	(multiple-value-bind
	 (which-prem pn)
	 (ftree-seq-invert-position fs n pos)
	 (if (eq which-prem 'PREM1)
	     (ftree-seq-imitate-rule fs 
				     (ftree-seq-replace-equivwffs wff prem1 pn pos)
				     prem2)
	   (ftree-seq-imitate-rule fs prem1
				   (ftree-seq-replace-equivwffs wff prem2 pn pos))))))))

(defun ftree-seq-initial-equivwffs (wff1 wff2)
  (let ((c (common-defn-eq-refl=-lam-reduct wff1 wff2 nil)))
    (let ((rews1 (reverse (car c)))
	  (rews2 (reverse (cdr c))))
      (let ((fs (make-ftree-seq-init (car rews1)))) ; same as (car rews2)
	(do ((pos-rews (cdr rews1) (cddr pos-rews)))
	    ((null pos-rews))
	    (setq fs (make-ftree-seq-rew-pos
		      (cadr pos-rews) (case (caar pos-rews)
					    (DEFN 'EQUIVWFFS)
					    (LEIBNIZ 'LEIBNIZ=)
					    (EXT 'EXT=)
					    (AB 'LAMBDA)
					    (REFL 'REFL=)
					    (t (caar pos-rews))) fs)))
	(do ((neg-rews (cdr rews2) (cddr neg-rews)))
	    ((null neg-rews))
	    (setq fs (make-ftree-seq-rew-neg
		      (cadr neg-rews) (case (caar neg-rews)
					    (DEFN 'EQUIVWFFS)
					    (LEIBNIZ 'LEIBNIZ=)
					    (EXT 'EXT=)
					    (AB 'LAMBDA)
					    (REFL 'REFL=)
					    (t (caar neg-rews))) fs)))
	fs))))

					; fs::Gamma, A_1,...,A_p, A_1,...,A_p => Delta, B_1,...,B_n, B_1,...,B_n
					; returns fs1::Gamma, A_1,...,A_p => Delta, B_1,...,B_n
(defun ftree-seq-post-merge-duplicates (fs p n)
  (let ((fs1 fs)
	(m1 (- (length (ftree-seq-pos-wffs fs)) 1))
	(m2 (- (length (ftree-seq-neg-wffs fs)) 1)))
    (dotimes (i p)
	     (setq fs1 (make-ftree-seq-merge-pos
			(make-ftree-seq-focus
			 m1
			 (make-ftree-seq-focus
			  (- m1 (- p i)) fs1 t) t)))
	     (setq m1 (- m1 1)))
					; fs1::A_1,...,A_p,Gamma => Delta, B_1,...,B_n, B_1,...,B_n
					; m1 == p + length(Gamma) - 1 (pos of last elt)
    (dotimes (i (+ (- m1 p) 1))
	     (setq fs1 (make-ftree-seq-focus m1 fs1 t)))
					; fs1::Gamma, A_1,...,A_p => Delta, B_1,...,B_n, B_1,...,B_n
    (dotimes (i n)
	     (setq fs1 (make-ftree-seq-merge-neg
			(make-ftree-seq-focus
			 m2
			 (make-ftree-seq-focus
			  (- m2 (- n i)) fs1 nil) nil)))
	     (setq m2 (- m2 1)))
    (dotimes (i (+ (- m2 n) 1))
	     (setq fs1 (make-ftree-seq-focus m2 fs1 nil)))
    fs1))

; fs::A_1,...,A_p, A_1,...,A_p,Gamma => B_1,...,B_n, B_1,...,B_n,Delta
; returns fs1::A_1,...,A_p,Gamma => B_1,...,B_n,Delta
(defun ftree-seq-pre-merge-duplicates (fs p n)
  (let ((fs1 fs)
	(m1 (- (* 2 p) 1))
	(m2 (- (* 2 n) 1)))
    (dotimes (i p)
	    (setq fs1 (make-ftree-seq-merge-pos
		       (make-ftree-seq-focus
			m1
			(make-ftree-seq-focus
			 (- p 1) fs1 t) t)))
	    (setq m1 (- m1 1)))
    (dotimes (i n)
	    (setq fs1 (make-ftree-seq-merge-neg
		       (make-ftree-seq-focus
			m2
			(make-ftree-seq-focus
			 (- n 1) fs1 nil) nil)))
	    (setq m2 (- m2 1)))
    fs1))

(defun ftree-seq-weaken-by (fs pos-wffs neg-wffs)
  (dolist (wff pos-wffs)
    (setq fs (make-ftree-seq-weaken wff fs t)))
  (dolist (wff neg-wffs)
    (setq fs (make-ftree-seq-weaken wff fs nil)))
  fs)

(defun ftree-seq-order-by (fs pos-wffs neg-wffs)
  (let ((pos-wffs1 (ftree-seq-pos-wffs fs))
	(neg-wffs1 (ftree-seq-neg-wffs fs))
	(i 0))
    (dolist (a (reverse pos-wffs))
      (let ((n (position a pos-wffs1 :test #'wffeq-ab)))
	(unless n
	  (throwfail "Problem ordering " (a . gwff) " in sequent " fs))
	(setq fs (make-ftree-seq-focus (+ i n) fs t))
	(setq pos-wffs1 (remove-nth n pos-wffs1))
	(incf i)))
    (setq i 0)
    (dolist (a (reverse neg-wffs))
      (let ((n (position a neg-wffs1 :test #'wffeq-ab)))
	(unless n
	  (throwfail "Problem ordering " (a . gwff) " in sequent " fs))
	(setq fs (make-ftree-seq-focus (+ i n) fs nil))
	(setq neg-wffs1 (remove-nth n neg-wffs1))
	(incf i)))
    fs))

(defun make-ftree-seq-weaken (wff fs pos)
  (make-ftree-seq :kind 'WEAKEN
		  :pos-rule pos
		  :pos-wffs (if pos
				(cons wff (ftree-seq-pos-wffs fs))
			      (ftree-seq-pos-wffs fs))
		  :neg-wffs (if pos
				(ftree-seq-neg-wffs fs)
			      (cons wff (ftree-seq-neg-wffs fs)))
		  :prem1 fs))

(defun make-ftree-seq-focus (n fs pos)
  (if (= n 0)
      fs
    (let ((pos-wffs (if pos
			(focus-nth n (ftree-seq-pos-wffs fs))
		      (ftree-seq-pos-wffs fs)))
	  (neg-wffs (if pos
			(ftree-seq-neg-wffs fs)
		      (focus-nth n (ftree-seq-neg-wffs fs)))))
      (or (find-previous-focus fs pos-wffs neg-wffs)
	  (make-ftree-seq :kind 'FOCUS
			  :pos-rule pos
			  :pos-wffs pos-wffs
			  :neg-wffs neg-wffs
			  :focus-n n
			  :prem1 fs)))))

; if we don't do this, we end up with long chains of focus steps
(defun find-previous-focus (fs pos-wffs neg-wffs)
  (if (or (member nil
		  (mapcar #'(lambda (x y)
			      (wffeq x y))
			  pos-wffs (ftree-seq-pos-wffs fs)))
	  (member nil
		  (mapcar #'(lambda (x y)
			      (wffeq x y))
			  neg-wffs (ftree-seq-neg-wffs fs))))
      (if (eq (ftree-kind fs) 'FOCUS)
	  (find-previous-focus (ftree-seq-prem1 fs) pos-wffs neg-wffs)
	nil)
    fs))

(defun make-ftree-seq-focus-2 (n1 n2 fs pos)
  (cond ((and (= n1 0) (= n2 1)) fs)
	((= n2 0) (make-ftree-seq-focus n1 fs pos))
	(t (make-ftree-seq-focus 
	    (if (> n2 n1) (+ n1 1) n1)
	    (make-ftree-seq-focus n2 fs pos) pos))))

; cycle until the first wff is in the nth position
(defun make-ftree-seq-unfocus (n fs pos)
  (dotimes (i n fs)
	   (setq fs (make-ftree-seq-focus n fs pos))))

; move nth position to the end
(defun make-ftree-seq-move-to-end (n fs pos)
  (let ((fs1 fs)
	(lastpos (- (if pos
			(length (ftree-seq-pos-wffs fs))
		      (length (ftree-seq-neg-wffs fs)))
		    1)))
  ; first cycle until n is at the end
    (dotimes (i (- lastpos n))
	    (setq fs1 (make-ftree-seq-focus lastpos fs1 pos)))
   ; then cycle all but last to get proper order
    (dotimes (i n)
	    (setq fs1 (make-ftree-seq-focus (- lastpos 1) fs1 pos)))
    fs1))

(defun make-ftree-seq-init (wff)
  (make-ftree-seq :pos-wffs (list wff) :neg-wffs (list wff) :kind 'INIT))

(defun make-ftree-seq-con-pos (fs1)
  (let ((pos-wffs (ftree-seq-pos-wffs fs1)))
    (make-ftree-seq :kind 'CON
		    :pos-rule t
		    :pos-wffs (cons (acons 'AND (car pos-wffs) (cadr pos-wffs))
				    (cddr pos-wffs))
		    :neg-wffs (ftree-seq-neg-wffs fs1)
		    :prem1 fs1)))

(defun make-ftree-seq-con-neg (fs1 fs2)
  (let ((neg-wffs1 (ftree-seq-neg-wffs fs1))
	(neg-wffs2 (ftree-seq-neg-wffs fs2)))
    (make-ftree-seq :kind 'CON
		    :pos-rule nil
		    :pos-wffs (append (ftree-seq-pos-wffs fs1)
				      (ftree-seq-pos-wffs fs2))
		    :neg-wffs (cons (acons 'AND (car neg-wffs1) (car neg-wffs2))
				    (append (cdr neg-wffs1) (cdr neg-wffs2)))
		    :prem1 fs1 :prem2 fs2)))

(defun make-ftree-seq-dis-pos (fs1 fs2)
  (let ((pos-wffs1 (ftree-seq-pos-wffs fs1))
	(pos-wffs2 (ftree-seq-pos-wffs fs2)))
    (make-ftree-seq :kind 'DIS
		    :pos-rule t
		    :neg-wffs (append (ftree-seq-neg-wffs fs1)
				      (ftree-seq-neg-wffs fs2))
		    :pos-wffs (cons (acons 'OR (car pos-wffs1) (car pos-wffs2))
				    (append (cdr pos-wffs1) (cdr pos-wffs2)))
		    :prem1 fs1 :prem2 fs2)))

(defun make-ftree-seq-dis-neg (fs1)
  (let ((neg-wffs (ftree-seq-neg-wffs fs1)))
    (make-ftree-seq :kind 'DIS
		    :pos-rule nil
		    :neg-wffs (cons (acons 'OR (car neg-wffs) (cadr neg-wffs))
				    (cddr neg-wffs))
		    :pos-wffs (ftree-seq-pos-wffs fs1)
		    :prem1 fs1)))

(defun make-ftree-seq-imp-pos (fs1 fs2)
  (let ((neg-wffs1 (ftree-seq-neg-wffs fs1))
	(pos-wffs2 (ftree-seq-pos-wffs fs2)))
    (make-ftree-seq :kind 'IMP
		    :pos-rule t
		    :neg-wffs (append (cdr neg-wffs1) (ftree-seq-neg-wffs fs2))
		    :pos-wffs (cons (acons 'IMPLIES (car neg-wffs1) (car pos-wffs2))
				    (append (ftree-seq-pos-wffs fs1) (cdr pos-wffs2)))
		    :prem1 fs1 :prem2 fs2)))

(defun make-ftree-seq-imp-neg (fs1)
  (let ((neg-wffs (ftree-seq-neg-wffs fs1))
	(pos-wffs (ftree-seq-pos-wffs fs1)))
    (make-ftree-seq :kind 'IMP
		    :pos-rule nil
		    :neg-wffs (cons (acons 'IMPLIES (car pos-wffs) (car neg-wffs))
				    (cdr neg-wffs))
		    :pos-wffs (cdr pos-wffs)
		    :prem1 fs1)))

(defun make-ftree-seq-neg-pos (fs1)
  (let ((neg-wffs (ftree-seq-neg-wffs fs1)))
    (make-ftree-seq :kind 'NEG
		    :pos-rule t
		    :neg-wffs (cdr neg-wffs)
		    :pos-wffs (cons (cons 'NOT (car neg-wffs))
				    (ftree-seq-pos-wffs fs1))
		    :prem1 fs1)))

(defun make-ftree-seq-neg-neg (fs1)
  (let ((pos-wffs (ftree-seq-pos-wffs fs1)))
    (make-ftree-seq :kind 'NEG
		    :pos-rule nil
		    :pos-wffs (cdr pos-wffs)
		    :neg-wffs (cons (cons 'NOT (car pos-wffs))
				    (ftree-seq-neg-wffs fs1))
		    :prem1 fs1)))

(defun make-ftree-seq-exp-pos (wff trm fs1)
  (let ((wffs (ftree-seq-pos-wffs fs1)))
    (make-ftree-seq :kind 'EXP :pos-rule t
		    :neg-wffs (ftree-seq-neg-wffs fs1)
		    :pos-wffs (cons wff (cdr wffs))
		    :exp-term trm
		    :prem1 fs1)))

(defun make-ftree-seq-exp-neg (wff trm fs1)
  (let ((wffs (ftree-seq-neg-wffs fs1)))
    (make-ftree-seq :kind 'EXP :pos-rule nil
		    :pos-wffs (ftree-seq-pos-wffs fs1)
		    :neg-wffs (cons wff (cdr wffs))
		    :exp-term trm
		    :prem1 fs1)))

(defun make-ftree-seq-sel-pos (wff a fs1)
  (let ((wffs (ftree-seq-pos-wffs fs1)))
    (make-ftree-seq :kind 'SEL :pos-rule t
		    :neg-wffs (ftree-seq-neg-wffs fs1)
		    :pos-wffs (cons wff (cdr wffs))
		    :sel-var a
		    :prem1 fs1)))

(defun make-ftree-seq-sel-neg (wff a fs1)
  (let ((wffs (ftree-seq-neg-wffs fs1)))
    (make-ftree-seq :kind 'SEL :pos-rule nil
		    :pos-wffs (ftree-seq-pos-wffs fs1)
		    :neg-wffs (cons wff (cdr wffs))
		    :sel-var a
		    :prem1 fs1)))

(defun make-ftree-seq-rew-pos (wff rew-just fs1)
  (make-ftree-seq :kind 'REW :pos-rule t
		  :neg-wffs (ftree-seq-neg-wffs fs1)
		  :pos-wffs (cons wff (cdr (ftree-seq-pos-wffs fs1)))
		  :rew-just rew-just
		  :prem1 fs1))

(defun make-ftree-seq-rew-neg (wff rew-just fs1)
  (make-ftree-seq :kind 'REW :pos-rule nil
		  :pos-wffs (ftree-seq-pos-wffs fs1)
		  :neg-wffs (cons wff (cdr (ftree-seq-neg-wffs fs1)))
		  :rew-just rew-just
		  :prem1 fs1))

(defun make-ftree-seq-cut (fs1 fs2)
  (unless (wffeq-ab (car (ftree-seq-neg-wffs fs1))
		    (car (ftree-seq-pos-wffs fs2)))
    (throwfail "Cut formulas " ((car (ftree-seq-neg-wffs fs1)) . gwff) t
	       " and " ((car (ftree-seq-pos-wffs fs2)) . gwff) t
	       " do not match"))
  (make-ftree-seq :kind 'CUT 
		  :neg-wffs (append (cdr (ftree-seq-neg-wffs fs1))
				    (ftree-seq-neg-wffs fs2))
		  :pos-wffs (append (ftree-seq-pos-wffs fs1)
				    (cdr (ftree-seq-pos-wffs fs2)))
		  :prem1 fs1
		  :prem2 fs2))

(defun make-ftree-seq-merge-pos (fs1)
  (unless (wffeq-ab (car (ftree-seq-pos-wffs fs1))
		    (cadr (ftree-seq-pos-wffs fs1)))
    (throwfail "Merge formulas " ((car (ftree-seq-pos-wffs fs1)) . gwff) t
	       " and " ((cadr (ftree-seq-pos-wffs fs1)) . gwff) t
	       " do not match"))
  (make-ftree-seq :kind 'MERGE :pos-rule t
		  :neg-wffs (ftree-seq-neg-wffs fs1)
		  :pos-wffs (cdr (ftree-seq-pos-wffs fs1))
		  :prem1 fs1))

(defun make-ftree-seq-merge-neg (fs1)
  (unless (wffeq-ab (car (ftree-seq-neg-wffs fs1))
		    (cadr (ftree-seq-neg-wffs fs1)))
    (throwfail "Merge formulas " ((car (ftree-seq-neg-wffs fs1)) . gwff) t
	       " and " ((cadr (ftree-seq-neg-wffs fs1)) . gwff) t
	       " do not match"))
  (make-ftree-seq :kind 'MERGE :pos-rule nil
		  :neg-wffs (cdr (ftree-seq-neg-wffs fs1))
		  :pos-wffs (ftree-seq-pos-wffs fs1)
		  :prem1 fs1))

; fs::Gamma => Delta
; returns fs2::A, Gamma_1 => Delta where A does not occur in Gamma_1
(defun ftree-seq-merge-focus-all-pos (A fs)
  (let ((fs1 fs))
    (do ((i 0 (+ i 1))
	 (flag nil)
	 (wffs (ftree-seq-pos-wffs fs) (cdr wffs)))
	((null wffs)
	 (unless flag
	   (setq fs1 (make-ftree-seq-weaken A fs1 t)))) ; to make sure there is an A at the beginning
	(when (wffeq-ab A (car wffs))
	  (setq flag t)
	  (setq fs1 (make-ftree-seq-focus i fs1 t))))
    ; fs1::A,...,A, Gamma_1 => Delta  where A does not occur in Gamma_1
    (do ((fs2 fs1 (make-ftree-seq-merge-pos fs2)))
	((or (not (ftree-seq-pos-wffs fs2))
	     (not (cdr (ftree-seq-pos-wffs fs2)))
	     (not (wffeq-ab (car (ftree-seq-pos-wffs fs2))
			    (cadr (ftree-seq-pos-wffs fs2)))))
	 fs2))))
	
; fs::Gamma => Delta
; returns fs2::Gamma => A, Delta_1 where A does not occur in Delta_1
(defun ftree-seq-merge-focus-all-neg (A fs)
  (let ((fs1 fs))
    (do ((i 0 (+ i 1))
	 (flag nil)
	 (wffs (ftree-seq-neg-wffs fs) (cdr wffs)))
	((null wffs)
	 (unless flag
	   (setq fs1 (make-ftree-seq-weaken A fs1 nil)))) ; to make sure there is an A at the beginning
	(when (wffeq-ab A (car wffs))
	  (setq fs1 (make-ftree-seq-focus i fs1 nil))))
    ; fs1::A,...,A, Gamma_1 => Delta  where A does not occur in Gamma_1
    (do ((fs2 fs1 (make-ftree-seq-merge-neg fs2)))
	((or (not (ftree-seq-neg-wffs fs2))
	     (not (cdr (ftree-seq-neg-wffs fs2)))
	     (not (wffeq-ab (car (ftree-seq-neg-wffs fs2))
			    (cadr (ftree-seq-neg-wffs fs2)))))
	 fs2))))
	
(defvar *ftree-seq-true*
  (make-ftree-seq :kind 'TRUE :pos-rule nil :neg-wffs (list 'TRUTH)))

(defvar *ftree-seq-false*
  (make-ftree-seq :kind 'FALSE :pos-rule t :pos-wffs (list 'FALSEHOOD)))

; makes lambda rewrites, if nec,
; this assumes the wff lambda normalizes to the first pos/neg wff of fs
(defun make-ftree-seq-lrew (wff fs pos)
  (let ((wff1 (if pos (car (ftree-seq-pos-wffs fs)) (car (ftree-seq-neg-wffs fs)))))
    (cond ((wffeq-ab wff wff1) (make-ftree-seq-ab-rew wff fs pos))
	  ((eq LAMBDA-CONV 'BETA-ETA-TOGETHER)
	   (let* ((wff2 (lambda-norm wff))
		  (wff3 (etanorm wff2))
		  (fs1
		   (if (wffeq-ab wff1 wff2)
		       (make-ftree-seq-ab-rew wff2 fs pos)
		     (make-ftree-seq-ab-rew wff3 fs pos))))
	     (if pos
		 (make-ftree-seq-rew-pos wff 'LAMBDA fs1)
	       (make-ftree-seq-rew-neg wff 'LAMBDA fs1))))
	  ((eq LAMBDA-CONV 'BETA-ETA-SEPARATE)
	   (let* ((wff2 (lambda-norm wff))
		  (wff3 (etanorm wff2))
		  (fs1
		   (if (wffeq-ab wff1 wff2)
		       (make-ftree-seq-ab-rew wff2 fs pos)
		     (make-ftree-seq-ab-rew wff3 fs pos)))
		  (fs2
		   (if (wffeq-ab wff1 wff2)
		       fs1
		     (if pos
			 (make-ftree-seq-rew-pos wff2 'ETA fs1)
		       (make-ftree-seq-rew-neg wff2 'ETA fs1)))))
	     (if (wffeq wff2 wff3)
		 fs2
	       (if pos
		   (make-ftree-seq-rew-pos wff 'BETA fs2)
		 (make-ftree-seq-rew-neg wff 'BETA fs2)))))
	  ((eq LAMBDA-CONV 'BETA-ONLY)
	   (let* ((wff2 (lambda-norm wff))
		  (fs1 (make-ftree-seq-ab-rew wff2 fs pos)))
	     (if pos
		 (make-ftree-seq-rew-pos wff 'BETA fs1)
	       (make-ftree-seq-rew-neg wff 'BETA fs1)))))))

; makes an AB rewrite, if nec,
; this assumes the wff is AB= to the first pos/neg wff of fs
(defun make-ftree-seq-ab-rew (wff fs pos)
  (let ((wff1 (if pos (car (ftree-seq-pos-wffs fs)) (car (ftree-seq-neg-wffs fs)))))
    (cond ((wffeq wff wff1) fs)
	  ((wffeq-ab wff wff1)
	   (if pos
	       (make-ftree-seq-rew-pos wff 'AB fs)
	     (make-ftree-seq-rew-neg wff 'AB fs)))
	  (t 
	   (throwfail (wff . gwff) " is not AB= to " (wff1 . gwff))))))

; must first bring the proper wff's to the front before performing the rule
; returns new ftree-seq & resulting posinds & neginds
(defun ftree-seq-mix-elim-imitate-rule (fs prem1 prem2 posinds1 neginds1 posinds2 neginds2 fst)
  (flet
   ((adjust-indices (n inds)
		    (mapcar #'(lambda (x)
				(if (equal (car x) fst)
				    (cons fst (+ (cdr x) n))
				  x))
			    inds)))
   (let ((pos (ftree-seq-pos-rule fs)))
     (case (ftree-seq-kind fs)
	   (INIT (values fs (list (cons fst 0)) (list (cons fst 0))))
	   (TRUE (values fs nil (list (cons fst 0))))
	   (FALSE (values fs (list (cons fst 0)) nil))
	   (CUT (throwfail "Mix Elim expects a Cut-Free derivation: " t fs))
	   (MERGE 
	    (values prem1
		    (if pos
			(mapcar #'(lambda (x)
				    (if (equal (car x) fst)
					(cons fst (if (= (cdr x) 0)
						      0
						    (- (cdr x) 1)))
				      x))
				posinds1)
		      posinds1)
		    (if pos
			neginds1
		      (mapcar #'(lambda (x)
				  (if (equal (car x) fst)
				      (cons fst (if (= (cdr x) 0)
						    0
						  (- (cdr x) 1)))
				    x))
			      neginds1))))
	   (WEAKEN
	    (values prem1
		    (if pos
			(adjust-indices 1 posinds1)
		      posinds1)
		    (if pos
			neginds1
		      (adjust-indices 1 neginds1))))
	   (FOCUS 
	    (let ((focus-n (ftree-seq-focus-n fs)))
	      (values prem1
		      (if pos
			  (mapcar #'(lambda (x)
				      (if (equal (car x) fst)
					  (cons fst (focus-image focus-n (cdr x)))
					x))
				  posinds1)
			posinds1)
		      (if pos
			  neginds1
			(mapcar #'(lambda (x)
				    (if (equal (car x) fst)
					(cons fst (focus-image focus-n (cdr x)))
				      x))
				neginds1)))))
	   (t
	    (let ((pos-wffs1 (ftree-seq-pos-wffs (ftree-seq-prem1 fs)))
		  (neg-wffs1 (ftree-seq-neg-wffs (ftree-seq-prem1 fs)))
		  (pos-wffs2 (when (ftree-seq-prem2 fs)
			       (ftree-seq-pos-wffs (ftree-seq-prem2 fs))))
		  (neg-wffs2 (when (ftree-seq-prem2 fs)
			       (ftree-seq-neg-wffs (ftree-seq-prem2 fs)))))
	      (if pos
		  (case (ftree-seq-kind fs)
			(CON
			 (multiple-value-bind
			  (fs3 posinds3)
			  (ftree-seq-bring-to-front
			   (cons fst 1) (cadr pos-wffs1) prem1 posinds1 t)
			  (multiple-value-bind
			   (fs4 posinds4)
			   (ftree-seq-bring-to-front
			    (cons fst 0) (car pos-wffs1) fs3 posinds3 t)
			   (values
			    (make-ftree-seq-con-pos fs4)
			    (cons (cons fst 0)
				  (adjust-indices -1 (cddr posinds4)))
			    neginds1))))
			(DIS
			 (multiple-value-bind
			  (fs3 posinds3)
			  (ftree-seq-bring-to-front
			   (cons fst 0) (car pos-wffs1) prem1 posinds1 t)
			  (multiple-value-bind
			   (fs4 posinds4)
			   (ftree-seq-bring-to-front
			    (cons fst 0) (car pos-wffs2) prem2 posinds2 t)
			   (values (make-ftree-seq-dis-pos fs3 fs4)
				   (cons (cons fst 0)
					 (append (cdr posinds3)
						 (adjust-indices (- (length pos-wffs1) 1)
								 (cdr posinds4))))
				   (append neginds1 
					   (adjust-indices (length neg-wffs1)
							   neginds2))))))
			(IMP
			 (multiple-value-bind
			  (fs3 neginds3)
			  (ftree-seq-bring-to-front
			   (cons fst 0) (car neg-wffs1) prem1 neginds1 nil)
			  (multiple-value-bind
			   (fs4 posinds4)
			   (ftree-seq-bring-to-front
			    (cons fst 0) (car pos-wffs2) prem2 posinds2 t)
			   (values (make-ftree-seq-imp-pos fs3 fs4)
				   (cons (cons fst 0)
					 (append (adjust-indices 1 posinds1)
						 (adjust-indices (length pos-wffs1)
								 (cdr posinds4))))
				   (append (adjust-indices -1 (cdr neginds3))
					   (adjust-indices (- (length neg-wffs1) 1)
							   neginds2))))))
			(NEG 
			 (multiple-value-bind
			  (fs3 neginds3)
			  (ftree-seq-bring-to-front
			   (cons fst 0) (car neg-wffs1) prem1 neginds1 nil)
			  (values (make-ftree-seq-neg-pos fs3)
				  (cons (cons fst 0)
					(adjust-indices 1 posinds1))
				  (adjust-indices -1 (cdr neginds3)))))
			(EXP 
			 (multiple-value-bind
			  (fs3 posinds3)
			  (ftree-seq-bring-to-front
			   (cons fst 0) (car pos-wffs1) prem1 posinds1 t)
			  (values (make-ftree-seq-exp-pos (car (ftree-seq-pos-wffs fs))
							  (ftree-seq-exp-term fs)
							  fs3)
				  posinds3
				  neginds1)))
			(SEL
			 (multiple-value-bind
			  (fs3 posinds3)
			  (ftree-seq-bring-to-front
			   (cons fst 0) (car pos-wffs1) prem1 posinds1 t)
			  (let* ((old-param (ftree-seq-sel-var fs))
				 (new-param (fresh-var (type old-param)
						       (getnameroot old-param))))
			    (values (make-ftree-seq-sel-pos
				     (car (ftree-seq-pos-wffs fs))
				     new-param
				     (ftree-seq-substitute new-param old-param fs3))
				    posinds3
				    neginds1))))
			(REW
			 (multiple-value-bind
			  (fs3 posinds3)
			  (ftree-seq-bring-to-front
			   (cons fst 0) (car pos-wffs1) prem1 posinds1 t)
			  (values (make-ftree-seq-rew-pos (car (ftree-seq-pos-wffs fs))
							  (ftree-seq-rew-just fs)
							  fs3)
				  posinds3
				  neginds1))))
		(case (ftree-seq-kind fs)
		      (CON
		       (multiple-value-bind
			(fs3 neginds3)
			(ftree-seq-bring-to-front
			 (cons fst 0) (car neg-wffs1) prem1 neginds1 nil)
			(multiple-value-bind
			 (fs4 neginds4)
			 (ftree-seq-bring-to-front
			  (cons fst 0) (car neg-wffs2) prem2 neginds2 nil)
			 (values (make-ftree-seq-con-neg fs3 fs4)
				 (append posinds1
					 (adjust-indices (length pos-wffs1)
							 posinds2))
				 (cons (cons fst 0)
				       (append (cdr neginds3)
					       (adjust-indices (- (length neg-wffs1) 1)
							       (cdr neginds4))))))))
		      (DIS
		       (multiple-value-bind
			(fs3 neginds3)
			(ftree-seq-bring-to-front
			 (cons fst 1) (cadr neg-wffs1) prem1 neginds1 nil)
			(multiple-value-bind
			 (fs4 neginds4)
			 (ftree-seq-bring-to-front
			  (cons fst 0) (car neg-wffs1) fs3 neginds3 nil)
			 (values (make-ftree-seq-dis-neg fs4)
				 posinds1
				 (cons (cons fst 0) (adjust-indices -1 (cddr neginds4)))))))
		      (IMP 
		       (multiple-value-bind
			(fs3 posinds3)
			(ftree-seq-bring-to-front
			 (cons fst 0) (car pos-wffs1) prem1 posinds1 t)
			(multiple-value-bind
			 (fs4 neginds4)
			 (ftree-seq-bring-to-front
			  (cons fst 0) (car neg-wffs1) fs3 neginds1 nil)
			 (values (make-ftree-seq-imp-neg fs4)
				 (adjust-indices -1 (cdr posinds3))
				 neginds4))))
		      (NEG
		       (multiple-value-bind
			(fs3 posinds3)
			(ftree-seq-bring-to-front
			 (cons fst 0) (car pos-wffs1) prem1 posinds1 t)
			(values (make-ftree-seq-neg-neg fs3)
				(adjust-indices -1 (cdr posinds3))
				(cons (cons fst 0)
				      (adjust-indices 1 neginds1)))))
		      (EXP
		       (multiple-value-bind
			(fs3 neginds3)
			(ftree-seq-bring-to-front
			 (cons fst 0) (car neg-wffs1) prem1 neginds1 nil)
			(values (make-ftree-seq-exp-neg (car (ftree-seq-neg-wffs fs))
							(ftree-seq-exp-term fs)
							fs3)
				posinds1
				neginds3)))
		      (SEL
		       (multiple-value-bind
			(fs3 neginds3)
			(ftree-seq-bring-to-front
			 (cons fst 0) (car neg-wffs1) prem1 neginds1 nil)
			(let* ((old-param (ftree-seq-sel-var fs))
			       (new-param (fresh-var (type old-param)
						     (getnameroot old-param))))
			  (values (make-ftree-seq-sel-neg
				   (car (ftree-seq-neg-wffs fs))
				   new-param
				   (ftree-seq-substitute new-param old-param fs3))
				  posinds1
				  neginds3))))
		      (REW
		       (multiple-value-bind
			(fs3 neginds3)
			(ftree-seq-bring-to-front
			 (cons fst 0) (car neg-wffs1) prem1 neginds1 nil)
			(values (make-ftree-seq-rew-neg (car (ftree-seq-neg-wffs fs))
							(ftree-seq-rew-just fs)
							fs3)
				posinds1
				neginds3)))))))))))

(defun ftree-seq-imitate-rule (fs prem1 prem2)
  (case (ftree-seq-kind fs)
	((INIT TRUE FALSE) fs)
	(CUT (make-ftree-seq-cut prem1 prem2))
	(FOCUS (make-ftree-seq-focus (ftree-seq-focus-n fs) prem1
				     (ftree-seq-pos-rule fs)))
	(t
	 (if (ftree-seq-pos-rule fs)
	     (case (ftree-seq-kind fs)
		   (CON (make-ftree-seq-con-pos prem1))
		   (DIS (make-ftree-seq-dis-pos prem1 prem2))
		   (IMP (make-ftree-seq-imp-pos prem1 prem2))
		   (NEG (make-ftree-seq-neg-pos prem1))
		   (EXP (make-ftree-seq-exp-pos (car (ftree-seq-pos-wffs fs))
						(ftree-seq-exp-term fs)
						prem1))
		   (SEL (make-ftree-seq-sel-pos (car (ftree-seq-pos-wffs fs))
						(ftree-seq-sel-var fs)
						prem1))
		   (REW (make-ftree-seq-rew-pos (car (ftree-seq-pos-wffs fs))
						(ftree-seq-rew-just fs)
						prem1))
		   (MERGE (make-ftree-seq-merge-pos prem1))
		   (WEAKEN (make-ftree-seq-weaken (car (ftree-seq-pos-wffs fs))
						  prem1 t)))
	   (case (ftree-seq-kind fs)
		 (CON (make-ftree-seq-con-neg prem1 prem2))
		 (DIS (make-ftree-seq-dis-neg prem1))
		 (IMP (make-ftree-seq-imp-neg prem1))
		 (NEG (make-ftree-seq-neg-neg prem1))
		 (EXP (make-ftree-seq-exp-neg (car (ftree-seq-neg-wffs fs))
					      (ftree-seq-exp-term fs)
					      prem1))
		 (SEL (make-ftree-seq-sel-neg (car (ftree-seq-neg-wffs fs))
					      (ftree-seq-sel-var fs)
					      prem1))
		 (REW (make-ftree-seq-rew-neg (car (ftree-seq-neg-wffs fs))
					      (ftree-seq-rew-just fs)
					      prem1))
		 (MERGE (make-ftree-seq-merge-neg prem1))
		 (WEAKEN (make-ftree-seq-weaken (car (ftree-seq-neg-wffs fs))
						prem1 nil)))))))

; assumes (n . pos) does not indicate the principal formula,
; ie, either n is not 0 or the rule application has different parity than pos or is a CUT
(defun ftree-seq-invert-position (fs n pos)
  (let* ((prem1 (ftree-seq-prem1 fs))
	 (wffs1 (if prem1 (if pos
			      (ftree-seq-pos-wffs prem1)
			    (ftree-seq-neg-wffs prem1))
		  nil))
	 (ln (length wffs1)))
    (case (ftree-seq-kind fs)
	  ((FALSE TRUE INIT)
	   (throwfail fs " has no premisses"))
	  (CON
	   (if (ftree-seq-pos-rule fs)
	       (if pos
		   (values 'PREM1 (+ n 1))
		 (values 'PREM1 n))
	     (if pos
		 (if (< n ln)
		     (values 'PREM1 n)
		   (values 'PREM2 (- n ln)))
	       (if (< n ln)
		   (values 'PREM1 n)
		 (values 'PREM2 (+ 1 (- n ln)))))))
	  (DIS
	   (if (ftree-seq-pos-rule fs)
	       (if pos
		   (if (< n ln)
		       (values 'PREM1 n)
		     (values 'PREM2 (+ 1 (- n ln))))
		 (if (< n ln)
		     (values 'PREM1 n)
		   (values 'PREM2 (- n ln))))
	     (if pos
		 (values 'PREM1 n)
	       (values 'PREM1 (+ n 1)))))
	  (IMP
	   (if (ftree-seq-pos-rule fs)
	       (if pos
		   (if (<= n ln)
		       (values 'PREM1 (- n 1))
		     (values 'PREM2 (- n ln)))
		 (if (< n (- ln 1))
		     (values 'PREM1 (+ n 1))
		   (values 'PREM2 (+ 1 (- n ln)))))
	     (if pos
		 (values 'PREM1 (+ 1 n))
	       (values 'PREM1 n))))
	  (NEG
	   (if (equal (ftree-seq-pos-rule fs) pos)
	       (values 'PREM1 (- n 1))
	     (values 'PREM1 (+ n 1))))
	  ((EXP SEL REW)
	   (values 'PREM1 n))
	  (MERGE
	   (if (equal (ftree-seq-pos-rule fs) pos)
	       (values 'PREM1 (+ n 1))
	     (values 'PREM1 n)))
	  (WEAKEN
	   (if (equal (ftree-seq-pos-rule fs) pos)
	       (values 'PREM1 (- n 1))
	     (values 'PREM1 n)))
	  (FOCUS
	   (if (equal (ftree-seq-pos-rule fs) pos)
	       (values 'PREM1
		       (focus-preimage (ftree-seq-focus-n fs) n))
	     (values 'PREM1 n)))
	  (CUT
	   (if pos
	       (if (< n ln)
		   (values 'PREM1 n)
		 (values 'PREM2 (+ 1 (- n ln))))
	     (if (< n (- ln 1))
		 (values 'PREM1 (+ n 1))
	       (values 'PREM2 (- n (- ln 1)))))))))

(defun ftree-seq-invert-position-list (fs nl pos)
  (case (ftree-seq-kind fs)
	(WEAKEN
	 (if (equal (ftree-seq-pos-rule fs) pos)
	     (values (mapcar #'(lambda (n)
				 (- n 1))
			     (remove 0 nl :test #'=))
		     nil)
	   (values nl nil)))
	(MERGE
	 (if (equal (ftree-seq-pos-rule fs) pos)
	     (values (if (member 0 nl :test #'=)
			 (cons 0 (mapcar #'(lambda (n)
					     (+ n 1)) nl))
		       (mapcar #'(lambda (n)
				   (+ n 1)) nl))
		     nil)
	   (values nl nil)))
	(t
	 (let ((nl1 nil)
	       (nl2 nil))
	   (dolist (n nl)
		   (unless (ftree-seq-principal-p fs n pos)
		     (multiple-value-bind
		      (which-prem n0)
		      (ftree-seq-invert-position fs n pos)
		      (if (eq which-prem 'PREM1)
			  (push n0 nl1)
			(push n0 nl2)))))
	   (values nl1 nl2)))))

; changes parameters to be fresh
(defun ftree-seq-refresh-sel-vars (fs)
  (ftree-seq-simul-substitute nil fs))

(defun ftree-seq-substitute (trm var fs)
  (ftree-seq-simul-substitute (acons var trm nil) fs))

(defun ftree-seq-simul-substitute (theta fs)
  (let* ((pos-wffs (ftree-seq-pos-wffs fs))
	 (neg-wffs (ftree-seq-neg-wffs fs))
	 (prem1 (ftree-seq-prem1 fs))
	 (prem2 (ftree-seq-prem2 fs))
	 (old-param (when (eq (ftree-seq-kind fs) 'SEL) (ftree-seq-sel-var fs)))
	 (new-param (when old-param
		      (fresh-var (type old-param) (getnameroot old-param))))
	 (prem3 (when prem1 (ftree-seq-simul-substitute
			     (if old-param
				 (acons old-param new-param theta)
			       theta)
			     prem1)))
	 (prem4 (when prem2 (ftree-seq-simul-substitute
			     (if old-param
				 (acons old-param new-param theta)
			       theta)
			     prem2)))
	 (vars (mapcar #'car theta))
	 (free-in nil))
    (when (or (eq (ftree-seq-kind fs) 'SEL)
	      (and (eq (ftree-seq-kind fs) 'EXP)
		   (intersection vars 
				 (free-vars-of (ftree-seq-exp-term fs)))))
      (setq free-in t))
    (unless free-in
      (dolist (w (append pos-wffs neg-wffs))
	      (when (intersection vars (free-vars-of w))
		(setq free-in t))))
    (if (or free-in (not (eq prem1 prem3)) (not (eq prem2 prem4)))
	(let ((fsnew (copy-ftree-seq fs)))
	  (setf (ftree-seq-pos-wffs fsnew)
		(mapcar #'(lambda (w)
			    (simul-substitute-l-term-var theta w))
			pos-wffs))
	  (setf (ftree-seq-neg-wffs fsnew)
		(mapcar #'(lambda (w)
			    (simul-substitute-l-term-var theta w))
			neg-wffs))
	  (setf (ftree-seq-prem1 fsnew) prem3)
	  (setf (ftree-seq-prem2 fsnew) prem4)
	  (when (eq (ftree-seq-kind fs) 'SEL)
	    (setf (ftree-seq-sel-var fsnew) new-param))
	  (when (eq (ftree-seq-kind fs) 'EXP)
	    (setf (ftree-seq-exp-term fsnew)
		  (simul-substitute-l-term-var theta (ftree-seq-exp-term fs))))
	  fsnew)
      fs)))


; Given fs::Gamma => Delta and (n1 . . . nk)
; where each position ni of Delta is Falsehood
; returns fs1::Gamma => Delta' where Delta' is Delta with positions n1,...,nk removed
(defun ftree-seq-remove-neg-false (fs nl)
  (if nl
      (if (eq (ftree-seq-kind fs) 'INIT)
	  *ftree-seq-false*
	(let ((nl1 nil)
	      (nl2 nil))
	  (dolist (n nl)
		  (if (ftree-seq-principal-p fs n nil) ; can only happen w/ MERGE|FOCUS|WEAKEN
		      (case (ftree-seq-kind fs)
			    (MERGE (push 1 nl1) (push 0 nl1))
			    (FOCUS (push (ftree-seq-focus-n fs) nl1)))
		    (multiple-value-bind
		     (which-prem pn)
		     (ftree-seq-invert-position fs n nil)
		     (if (eq which-prem 'PREM1)
			 (push pn nl1)
		       (push pn nl2)))))
	  (let* ((prem1 (ftree-seq-prem1 fs))
		 (prem2 (ftree-seq-prem2 fs))
		 (prem3 (when prem1
			  (ftree-seq-remove-neg-false prem1 nl1)))
		 (prem4 (when prem2
			  (ftree-seq-remove-neg-false prem2 nl2))))
	    (if (and (member (ftree-seq-kind fs) '(WEAKEN FOCUS MERGE))
		     (member 0 nl :test #'equal)
		     (not (ftree-seq-pos-rule fs)))
		prem3
	      (if (and (equal prem1 prem3)
		       (equal prem2 prem4))
		  fs
		(let* ((fs1 (copy-ftree-seq fs))
		       (neg-wffs1 nil)
		       (neg-focus (and (not (ftree-seq-pos-rule fs))
				       (eq (ftree-seq-kind fs) 'FOCUS)))
		       (focus-n (ftree-seq-focus-n fs))
		       (focus-n1 focus-n))
		  (do ((neg-wffs0 (ftree-seq-neg-wffs fs) (cdr neg-wffs0))
		       (i 0 (+ i 1)))
		      ((null neg-wffs0))
		      (if (member i nl :test #'equal)
			  (when (and neg-focus (<= i focus-n))
			    (setq focus-n1 (- focus-n1 1)))
			(push (car neg-wffs0) neg-wffs1)))
		  (setf (ftree-seq-focus-n fs1) focus-n1)
		  (setf (ftree-seq-neg-wffs fs1) (reverse neg-wffs1))
		  (setf (ftree-seq-prem1 fs1) prem3)
		  (setf (ftree-seq-prem2 fs1) prem4)
		  fs1))))))
    fs))

; Given fs::Gamma => Delta & nl::(n1 . . . nk)  (RHS empty)
; where the each pos ni wff of Gamma is [not A],
; returns fs1::Gamma' => Delta, A, . . ., A
; and an integer m (A is repeated m times after Delta)
; where Gamma' is Gamma with positions ni removed
(defun ftree-seq-pos-not-A-to-neg-A (fs nl)
  (if nl
      (if (eq (ftree-seq-kind fs) 'INIT) ; must be -A => -A.  return => -A,A
	  (values
	   (make-ftree-seq-neg-neg
	    (make-ftree-seq-init (cdar (ftree-seq-pos-wffs fs))))
	   1)
	(let ((nl1 nil)
	      (nl2 nil))
	  (dolist (n nl)
		  (if (ftree-seq-principal-p fs n t) ; can only happen w/ MERGE|FOCUS|WEAKEN|NEG
		      (case (ftree-seq-kind fs)
			    (MERGE (push 1 nl1) (push 0 nl1))
			    (FOCUS (push (ftree-seq-focus-n fs) nl1)))
		    (multiple-value-bind
		     (which-prem pn)
		     (ftree-seq-invert-position fs n t)
		     (if (eq which-prem 'PREM1)
			 (push pn nl1)
		       (push pn nl2)))))
	  (let* ((prem1 (ftree-seq-prem1 fs))
		 (prem2 (ftree-seq-prem2 fs)))
	    (multiple-value-bind
	     (prem3 m3)
	     (when prem1
	       (ftree-seq-pos-not-A-to-neg-A prem1 nl1))
	     (multiple-value-bind
	      (prem4 m4)
	      (when prem2
		(ftree-seq-pos-not-A-to-neg-A prem2 nl2))
	      (if (and (member (ftree-seq-kind fs) '(WEAKEN FOCUS MERGE))
		       (ftree-seq-pos-rule fs)
		       (member 0 nl :test #'equal)) 
		  (values prem3 m3)
		(if (and (eq (ftree-seq-kind fs) 'NEG)
			 (ftree-seq-pos-rule fs)
			 (member 0 nl :test #'equal)) ; don't apply rule, instead push A to the end
		    (let ((fs1 prem3)
			  (ln (- (length (ftree-seq-neg-wffs prem3)) 1)))
		      (dotimes (i ln)
			       (setq fs1 (make-ftree-seq-focus ln fs1 nil)))
		      (values fs1 (+ m3 1)))
		(if (and (equal prem1 prem3)
			 (equal prem2 prem4))
		    (values fs 0)
		  (if (and (ftree-seq-pos-rule fs)
			   (eq (ftree-seq-kind fs) 'FOCUS))
		      (values
		       (make-ftree-seq-focus
			(do ((focus-n (ftree-seq-focus-n fs))
			     (nl0 nl (cdr nl0)))
			    ((null nl0) focus-n)
			    (when (<= (car nl0) (ftree-seq-focus-n fs))
			      (setq focus-n (- focus-n 1))))
			prem3 t)
		       m3)
		    (let ((fs1 (ftree-seq-imitate-rule fs prem3 prem4)))
		      (when (and m3 prem2 (ftree-seq-neg-wffs prem2)
				 (> m3 0)) ; then there are some A's in the middle - a rule with 2 prem's was applied
					; shuffle the A's
			(let ((lastpos3 (if (eq (ftree-seq-kind fs) 'CON)
					    (- (length (ftree-seq-neg-wffs prem3)) 1)
					  (- (length (ftree-seq-neg-wffs prem3)) 2)))) ; else, IMP|DIS|CUT, length of Delta decreases by one
			  (when (> (length (ftree-seq-neg-wffs prem1)) 0) ; first bring the middle A's to the front
			    (dotimes (i m3)
				     (setq fs1 (make-ftree-seq-focus lastpos3 fs1 nil))))
					; merge these A's we brought to the front to a single A
			  (dotimes (i (- m3 1))
				   (setq fs1 (make-ftree-seq-merge-neg fs1)))
			  (let ((lastpos (- (length (ftree-seq-neg-wffs fs1)) 1)))
					; now bring the A's at the end to the front
			    (dotimes (i m4)
				     (setq fs1 (make-ftree-seq-focus lastpos fs1 nil)))
					; merge these A's to a single A
			    (dotimes (i m4)
				     (setq fs1 (make-ftree-seq-merge-neg fs1))))
			  (let ((lastpos (- (length (ftree-seq-neg-wffs fs1)) 1)))
					; finally, cycle so that the single A is at the end
			    (dotimes (i lastpos)
				     (setq fs1 (make-ftree-seq-focus lastpos fs1 nil))))
			  (setq m3 0)
			  (setq m4 1)))
		      (values fs1 
			      (if m3
				  (if m4
				      (+ m3 m4)
				    m3)
				(or m4 0)))))))))))))
    (values fs 0)))

(defun make-ftree-seq-rec-dnot-true (A)
  (if (eq A 'TRUTH)
      *ftree-seq-true*
    (if (not-p A)
	(make-ftree-seq-neg-neg
	 (make-ftree-seq-rec-dnot-false (cdr A)))
      (throwfail A " is not ~~'s of TRUTH"))))

(defun make-ftree-seq-rec-dnot-false (A)
  (if (eq A 'FALSEHOOD)
      *ftree-seq-false*
    (if (not-p A)
	(make-ftree-seq-neg-pos
	 (make-ftree-seq-rec-dnot-true (cdr A)))
      (throwfail A " is not ~~'s of FALSEHOOD"))))

; A and B should have the same NNF
; returns a seq derivation of A => B
(defun make-ftree-seq-nnf-init (A B)
  (cond ((wffeq A B) (make-ftree-seq-init A))
	((wffeq-ab A B) (make-ftree-seq-rew-neg B 'AB (make-ftree-seq-init A)))
	((not-p A) 
	 (make-ftree-seq-neg-pos
	  (make-ftree-seq-nnf-neg (cdr A) B)))
	((not-p B) 
	 (make-ftree-seq-neg-neg
	  (make-ftree-seq-nnf-pos (cdr B) A)))
	((and (and-p A)
	      (and-p B))
	     (make-ftree-seq-con-pos
	      (make-ftree-seq-con-neg
	       (make-ftree-seq-nnf-init (cdar A) (cdar B))
	       (make-ftree-seq-nnf-init (cdr A) (cdr B)))))
	((and (or-p A) (or-p B))
	 (make-ftree-seq-dis-neg
	  (make-ftree-seq-dis-pos
	   (make-ftree-seq-nnf-init (cdar A) (cdar B))
	   (make-ftree-seq-nnf-init (cdr A) (cdr B)))))
	((and (implies-p A) (implies-p B))
	 (make-ftree-seq-imp-neg
	  (make-ftree-seq-focus
	   1
	   (make-ftree-seq-imp-pos
	    (make-ftree-seq-focus
	     1
	     (make-ftree-seq-weaken (cdr B) (make-ftree-seq-nnf-init (cdar B) (cdar A)) nil)
	     nil)
	    (make-ftree-seq-focus
	     1
	     (make-ftree-seq-weaken (cdar B) (make-ftree-seq-nnf-init (cdr A) (cdr B)) t)
	     t))
	   t)))
					; 2 cases with imp & dis mixed
	((and (or-p A) (implies-p B))
	 (make-ftree-seq-merge-neg ; (A1 or A2) => (B1 imp B2)
	  (make-ftree-seq-dis-pos ; (A1 or A2) => (B1 imp B2),(B1 imp B2)
	   (make-ftree-seq-imp-neg ; A1=>(B1 imp B2)
	    (make-ftree-seq-weaken  ; B1,A1=>B2
	     (cdr B)
	     (make-ftree-seq-nnf-pos (cdar B) (cdar A)) ; B1,A1=>
	     nil))
	   (make-ftree-seq-imp-neg ; A2=>(B1 imp B2)
	    (make-ftree-seq-weaken
	     (cdar B) ; B1,A2 => B2
	     (make-ftree-seq-nnf-init (cdr A) (cdr B)) ; A2 => B2
	     t)))))
	((and (implies-p A) (or-p B))
	 (make-ftree-seq-dis-neg ; A1 imp A2 => B1 or B2
	  (make-ftree-seq-imp-pos ; A1 imp A2 => B1,B2
	   (make-ftree-seq-focus ; A1,B1,B2
	    2 (make-ftree-seq-focus ; B1,B2,A1
	       2 (make-ftree-seq-weaken ; B2,A1,B1
		  (cdr B) 
		  (make-ftree-seq-nnf-neg (cdar A) (cdar B)) ; A1,B1
		  nil) nil) nil)
	   (make-ftree-seq-weaken ; A2=>B1,B2
	    (cdar B)
	    (make-ftree-seq-nnf-init (cdr A) (cdr B)) ; A2 => B2
	    nil))))
	((and (e-bd-wff-p A) (e-bd-wff-p B))
	 (let ((x (fresh-var (type (bindvar A)) (getnameroot (bindvar A)))))
	   (make-ftree-seq-sel-pos
	    A x
	    (make-ftree-seq-exp-neg
	     B x
	     (make-ftree-seq-nnf-init
	      (substitute-l-term-var x (bindvar A) (cdr A))
	      (substitute-l-term-var x (bindvar B) (cdr B)))))))
	((and (a-bd-wff-p A) (a-bd-wff-p B))
	 (let ((x (fresh-var (type (bindvar B)) (getnameroot (bindvar B)))))
	   (make-ftree-seq-sel-neg
	    B x
	    (make-ftree-seq-exp-pos
	     A x
	     (make-ftree-seq-nnf-init
	      (substitute-l-term-var x (bindvar A) (cdr A))
	      (substitute-l-term-var x (bindvar B) (cdr B)))))))
					; not handling EQUIV
	(t (throwfail "Formulas " (A . gwff) " and " t (B . gwff) t
		      " should be the same up to NNF."))))

; A and ~B should be the same up to NNF
; return derivation of => A,B
(defun make-ftree-seq-nnf-neg (A B)
  (cond ((not-p A)
	 (make-ftree-seq-neg-neg
	  (make-ftree-seq-nnf-init (cdr A) B)))
	((not-p B)
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-neg-neg
	     (make-ftree-seq-nnf-init (cdr B) A)) nil))
	((and (and-p A) (or-p B)) ; reduce to or/and case
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-nnf-neg B A) nil))
	((and (and-p A) (implies-p B)) ; reduce to implies/and case
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-nnf-neg B A) nil))
	((and (or-p A) (and-p B))
	 (make-ftree-seq-dis-neg ; A1 or A2, B1 and B2
	  (make-ftree-seq-focus
	   2 ; A1, A2, B1 and B2
	   (make-ftree-seq-focus
	    2 ; A2, B1 and B2, A1
	    (make-ftree-seq-con-neg ; B1 and B2, A1, A2
	     (make-ftree-seq-nnf-neg (cdar B) (cdar A)) ; B1,A1
	     (make-ftree-seq-nnf-neg (cdr B) (cdr A))) nil) nil))) ; B2,A2
	((and (implies-p A) (and-p B))
	 (make-ftree-seq-imp-neg ; A1 imp A2, B1 and B2
	  (make-ftree-seq-focus ; A1 => A2, B1 and B2
	   1 (make-ftree-seq-con-neg ; A1 => B1 and B2, A2
	      (make-ftree-seq-nnf-init (cdar A) (cdar B)) ; A1 => B1
	      (make-ftree-seq-nnf-neg (cdr B) (cdr A))) nil))) ; B2,A2
	((and (a-bd-wff-p A) (e-bd-wff-p B))
	 (let ((x (fresh-var (type (bindvar A)) (getnameroot (bindvar A)))))
	   (make-ftree-seq-sel-neg ; Ax A1(x), Ex B1(x)
	    A x
	    (make-ftree-seq-focus ; A1(x), Ex B1(x)
	     1 (make-ftree-seq-exp-neg ; Ex B1(x), A1(x)
		B x
		(make-ftree-seq-nnf-neg ; B1(x), A1(x)
		 (substitute-l-term-var x (bindvar B) (cdr B))
		 (substitute-l-term-var x (bindvar A) (cdr A)))) nil))))
	((and (e-bd-wff-p A) (a-bd-wff-p B)) ; reduce to all/exists
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-nnf-neg B A) nil))
	((eq A 'TRUTH)
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-weaken B *ftree-seq-true* nil) nil))
	((eq B 'TRUTH)
	 (make-ftree-seq-weaken A *ftree-seq-true* nil))
	(t (throwfail "Formulas " (A . gwff) " and " t "~"(B . gwff) t
		      " should be the same up to NNF."))))

; A and ~B should be the same up to NNF
; return derivation of A,B =>
(defun make-ftree-seq-nnf-pos (A B)
  (cond ((not-p A)
	 (make-ftree-seq-neg-pos
	  (make-ftree-seq-nnf-init B (cdr A))))
	((not-p B)
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-neg-pos
	     (make-ftree-seq-nnf-init A (cdr B))) t))
	((and (and-p A) (or-p B))
	 (make-ftree-seq-con-pos
	  (make-ftree-seq-focus
	   2 (make-ftree-seq-focus
	      2 (make-ftree-seq-dis-pos
		 (make-ftree-seq-nnf-pos (cdar B) (cdar A))
		 (make-ftree-seq-nnf-pos (cdr B) (cdr A))) t) t)))
	((and (and-p A) (implies-p B))
	 (make-ftree-seq-con-pos
	  (make-ftree-seq-focus
	   2 (make-ftree-seq-focus
	      2 (make-ftree-seq-imp-pos
		 (make-ftree-seq-nnf-init (cdar A) (cdar B))
		 (make-ftree-seq-nnf-pos (cdr B) (cdr A))) t) t)))
	((and (or-p A) (and-p B))
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-nnf-pos B A) t))
	((and (implies-p A) (and-p B))
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-nnf-pos B A) t))
	((and (a-bd-wff-p A) (e-bd-wff-p B))
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-nnf-pos B A) t))
	((and (e-bd-wff-p A) (a-bd-wff-p B))
	 (let ((x (fresh-var (type (bindvar A)) (getnameroot (bindvar A)))))
	   (make-ftree-seq-sel-pos ; Ex A(x),Ax B(x) =>
	    A x
	    (make-ftree-seq-focus ; A(x),Ax B(x) =>
	     1 (make-ftree-seq-exp-pos ; Ax B(x),A(x) =>
		B x
		(make-ftree-seq-nnf-pos ; B(x),A(x) =>
		 (substitute-l-term-var x (bindvar B) (cdr B))
		 (substitute-l-term-var x (bindvar A) (cdr A)))) t))))
	((eq A 'FALSEHOOD)
	 (make-ftree-seq-focus
	  1 (make-ftree-seq-weaken B *ftree-seq-false* t) t))
	((eq B 'FALSEHOOD)
	 (make-ftree-seq-weaken A *ftree-seq-false* t))
	(t (throwfail "Formulas " (A . gwff) " and " t "~"(B . gwff) t
		      " should be the same up to NNF."))))
	 
; Takes Gamma => B,Delta
; Returns Gamma => A,Delta
(defun ftree-seq-nnf-neg (A fs)
  (let ((B (car (ftree-seq-neg-wffs fs)))
	(ln (length (cdr (ftree-seq-neg-wffs fs)))))
    (make-ftree-seq-focus
     ln
     (make-ftree-seq-cut fs (make-ftree-seq-nnf-init B A)) nil)))

; Takes B,Gamma => Delta
; Returns A,Gamma => Delta
(defun ftree-seq-nnf-pos (A fs)
  (let ((B (car (ftree-seq-pos-wffs fs))))
    (make-ftree-seq-cut (make-ftree-seq-nnf-init A B) fs)))

; takes fs::Gamma->Delta &
; returns values fs'::Gamma'->Delta'  (p1 . . . pl)  (n1 . . . nk)
; where Gamma' is obtained by removing the pi's from Gamma
; and Delta' is obtained by removing the ni's from Delta
(defun ftree-seq-weaken-early (fs) ; debugging wrap
  (multiple-value-bind
   (fs0 weaken-pos weaken-neg)
   (ftree-seq-weaken-early-1 fs)
   (check-ftree-seq fs0)
   (let ((wp2 (sort (copy-list weaken-pos) #'>))
	 (wn2 (sort (copy-list weaken-neg) #'>))
	 (neg-wffs (ftree-seq-neg-wffs fs))
	 (pos-wffs (ftree-seq-pos-wffs fs))
	 (neg-wffs0 (ftree-seq-neg-wffs fs0))
	 (pos-wffs0 (ftree-seq-pos-wffs fs0)))
     (dolist (n wp2)
	     (setq pos-wffs (remove-nth n pos-wffs)))
     (dolist (n wn2)
	     (setq neg-wffs (remove-nth n neg-wffs)))
     (unless (and (equal pos-wffs pos-wffs0)
		  (equal neg-wffs neg-wffs0))
       (throwfail "Problem in weaken early"))
     (values fs0 weaken-pos weaken-neg))))
    
(defun ftree-seq-weaken-early-1 (fs)
  (let ((k (ftree-seq-kind fs))
	(prem1 (ftree-seq-prem1 fs))
	(prem2 (ftree-seq-prem2 fs)))
    (cond ((member k '(INIT TRUE FALSE)) (values fs nil nil))
	  ((eq k 'CUT)
	   (let ((lnneg1 (- (length (ftree-seq-neg-wffs prem1)) 1))
		 (lnpos1 (length (ftree-seq-pos-wffs prem1))))
	   (multiple-value-bind
	    (fs1 weaken-pos1 weaken-neg1)
	    (ftree-seq-weaken-early prem1)
	    (if (member 0 weaken-neg1 :test #'=) ; then the cut wasn't really needed
					; don't even need to process second prem
		(let ((weaken-neg (mapcar #'(lambda (x)
					      (- x 1))
					  (remove 0 weaken-neg1 :test #'=)))
		      (weaken-pos weaken-pos1))
		  (dotimes (i (length (ftree-seq-neg-wffs prem2)))
			   (push (+ i lnneg1) weaken-neg))
		  (dotimes (i (length (cdr (ftree-seq-pos-wffs prem2))))
			   (push (+ i lnpos1) weaken-pos))
		  (values fs1 weaken-pos weaken-neg))
	      (multiple-value-bind
	       (fs2 weaken-pos2 weaken-neg2)
	       (ftree-seq-weaken-early prem2)
	       (if (member 0 weaken-pos2 :test #'=) ; again, the cut wasn't needed
		   (let ((weaken-neg (mapcar #'(lambda (x)
						 (+ lnneg1 x))
					     weaken-neg2))
			 (weaken-pos (mapcar #'(lambda (x)
						 (+ lnpos1 x))
					     weaken-pos2)))
		     (dotimes (i (length (cdr (ftree-seq-neg-wffs prem1))))
			      (push i weaken-neg))
		     (dotimes (i (length (ftree-seq-pos-wffs prem1)))
			      (push i weaken-pos))
		     (values fs2 weaken-pos weaken-neg))
		 (values
		  (make-ftree-seq-cut fs1 fs2)
		  (append weaken-pos1 (mapcar #'(lambda (x)
						  (+ lnpos1 (- x 1)))
					      weaken-pos2))
		  (append (mapcar #'(lambda (x) (- x 1)) weaken-neg1)
			  (mapcar #'(lambda (x) (+ lnneg1 x)) weaken-neg2)))))))))
	  (t
	   (if (ftree-seq-pos-rule fs)
	       (case k
		     (WEAKEN (multiple-value-bind
			      (fs1 weaken-pos1 weaken-neg1)
			      (ftree-seq-weaken-early prem1)
			      (values fs1 (cons 0 (mapcar #'(lambda (x) (+ x 1))
							  weaken-pos1))
				      weaken-neg1)))
		     (FOCUS (multiple-value-bind
			     (fs1 weaken-pos1 weaken-neg1)
			     (ftree-seq-weaken-early prem1)
			     (let ((n (ftree-seq-focus-n fs)))
			       (if (member n weaken-pos1 :test #'=)
				   (values fs1 
					   (cons 0 (mapcar #'(lambda (x)
							       (if (< x n) (+ x 1) x))
							   (remove n weaken-pos1 :test #'=)))
					   weaken-neg1)
				 (let ((n1 n))
				   (dolist (i weaken-pos1)
					   (when (< i n)
					     (setq n1 (- n1 1))))
				   (values (make-ftree-seq-focus n1 fs1 t)
					   (mapcar #'(lambda (x)
						       (if (< x n) (+ x 1) x))
						   weaken-pos1)
					   weaken-neg1))))))
		     (MERGE (multiple-value-bind
			     (fs1 weaken-pos1 weaken-neg1)
			     (ftree-seq-weaken-early prem1)
			     (cond ((member 0 weaken-pos1 :test #'=)
				    (values fs1
					    (mapcar #'(lambda (x)
							(- x 1))
						    (remove 0 weaken-pos1 :test #'=))
					    weaken-neg1))
				   ((member 1 weaken-pos1 :test #'=) ; and 0 not in it
				    (values fs1
					    (mapcar #'(lambda (x)
							(- x 1))
						    (remove 1 weaken-pos1 :test #'=))
					    weaken-neg1))
				   (t
				    (values (make-ftree-seq-merge-pos fs1)
					    (mapcar #'(lambda (x)
							(- x 1))
						    weaken-pos1)
					    weaken-neg1)))))
		     (CON (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (let ((weaken-pos (mapcar #'(lambda (x)
							 (- x 1))
						     (set-difference weaken-pos1
								     '(0 1) :test #'=))))
			     (if (member 1 weaken-pos1 :test #'=)
				 (if (member 0 weaken-pos1 :test #'=)
				     (values fs1 
					     (cons 0 weaken-pos)
					     weaken-neg1)
				   (values 
				    (make-ftree-seq-con-pos
				     (make-ftree-seq-focus
				      1
				      (make-ftree-seq-weaken (cadr (ftree-seq-pos-wffs prem1))
							     fs1 t) t))
				    weaken-pos weaken-neg1))
			       (if (member 0 weaken-pos1 :test #'=)
				   (values
				    (make-ftree-seq-con-pos
				     (make-ftree-seq-weaken (car (ftree-seq-pos-wffs prem1)) fs1 t))
				    weaken-pos weaken-neg1)
				 (values
				  (make-ftree-seq-con-pos fs1)
				  weaken-pos weaken-neg1))))))
		     (DIS (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (let* ((lnpos1 (length (ftree-seq-pos-wffs prem1)))
				  (lnneg1 (length (ftree-seq-neg-wffs prem1))))
			     (if (member 0 weaken-pos1 :test #'=) ; need not consider other case
				 (let ((weaken-pos weaken-pos1)
				       (weaken-neg weaken-neg1))
				   (dotimes (i (length (cdr (ftree-seq-pos-wffs prem2))))
					    (push (+ i lnpos1) weaken-pos))
				   (dotimes (i (length (ftree-seq-neg-wffs prem2)))
					    (push (+ i lnneg1) weaken-neg))
				   (values fs1 weaken-pos weaken-neg))
			       (multiple-value-bind
				(fs2 weaken-pos2 weaken-neg2)
				(ftree-seq-weaken-early prem2)
				(let ((weaken-pos (mapcar #'(lambda (x)
							      (+ lnpos1 x))
							  (remove 0 weaken-pos2)))
				      (weaken-neg (mapcar #'(lambda (x)
							      (+ lnneg1 x))
							  weaken-neg2)))
				  (if (member 0 weaken-pos2 :test #'=) ; need not consider first case
				      (progn
					(dotimes (i lnneg1)
						 (push i weaken-neg))
					(dotimes (i lnpos1)
						 (push i weaken-pos))
					(values fs2 weaken-pos weaken-neg))
				    (values (make-ftree-seq-dis-pos fs1 fs2)
					    (append weaken-pos1 weaken-pos)
					    (append weaken-neg1 weaken-neg)))))))))
		     (IMP (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (let* ((lnpos1 (+ 1 (length (ftree-seq-pos-wffs prem1))))
				  (lnneg1 (- (length (ftree-seq-neg-wffs prem1)) 1)))
			     (if (member 0 weaken-neg1 :test #'=) ; need not consider other case
				 (let ((weaken-pos (cons 0 (mapcar #'(lambda (x) (+ x 1))
								   weaken-pos1)))
				       (weaken-neg (mapcar #'(lambda (x) (- x 1))
							   (remove 0 weaken-neg1 :test #'=))))
				   (dotimes (i (length (cdr (ftree-seq-pos-wffs prem2))))
					    (push (+ i lnpos1) weaken-pos))
				   (dotimes (i (length (ftree-seq-neg-wffs prem2)))
					    (push (+ i lnneg1) weaken-neg))
				   (values fs1 weaken-pos weaken-neg))
			       (multiple-value-bind
				(fs2 weaken-pos2 weaken-neg2)
				(ftree-seq-weaken-early prem2)
				(let ((weaken-pos (mapcar #'(lambda (x)
							      (+ lnpos1 x))
							  (remove 0 weaken-pos2)))
				      (weaken-neg (mapcar #'(lambda (x)
							      (+ lnneg1 x))
							  weaken-neg2)))
				  (if (member 0 weaken-pos2 :test #'=) ; need not consider first case
				      (progn
					(dotimes (i lnneg1)
						 (push i weaken-neg))
					(dotimes (i lnpos1)
						 (push i weaken-pos))
					(values fs2 weaken-pos weaken-neg))
				    (values (make-ftree-seq-imp-pos fs1 fs2)
					    (append (mapcar #'(lambda (x)
								(+ 1 x)) weaken-pos1)
						    weaken-pos)
					    (append (mapcar #'(lambda (x) (- x 1))
							    (remove 0 weaken-neg1 :test #'=))
						    weaken-neg)))))))))
		     (NEG (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (let ((weaken-pos (mapcar #'(lambda (x) (+ x 1)) weaken-pos1))
				 (weaken-neg (mapcar #'(lambda (x) (- x 1))
						     (remove 0 weaken-neg1 :test #'=))))
			     (if (member 0 weaken-neg1 :test #'=)
				 (values fs1 (cons 0 weaken-pos) weaken-neg)
			       (values (make-ftree-seq-neg-pos fs1) weaken-pos weaken-neg)))))
		     (EXP (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (values (if (member 0 weaken-pos1 :test #'=)
				       fs1
				     (make-ftree-seq-exp-pos (car (ftree-seq-pos-wffs fs))
							     (ftree-seq-exp-term fs)
							     fs1))
				   weaken-pos1 weaken-neg1)))
		     (SEL (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (values (if (member 0 weaken-pos1 :test #'=)
				       fs1
				     (make-ftree-seq-sel-pos (car (ftree-seq-pos-wffs fs))
							     (ftree-seq-sel-var fs)
							     fs1))
				   weaken-pos1 weaken-neg1)))
		     (REW (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (values (if (member 0 weaken-pos1 :test #'=)
				       fs1
				     (make-ftree-seq-rew-pos (car (ftree-seq-pos-wffs fs))
							     (ftree-seq-rew-just fs)
							     fs1))
				   weaken-pos1 weaken-neg1)))
		     (t (throwfail "Unexpected case " fs " in weaken early")))
	     (case k
		   (WEAKEN (multiple-value-bind
			    (fs1 weaken-pos1 weaken-neg1)
			    (ftree-seq-weaken-early prem1)
			    (values fs1 weaken-pos1
				    (cons 0 (mapcar #'(lambda (x) (+ x 1))
						    weaken-neg1)))))
		   (FOCUS (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (let ((n (ftree-seq-focus-n fs)))
			     (if (member n weaken-neg1 :test #'=)
				 (values fs1 
					 weaken-pos1
					 (cons 0 (mapcar #'(lambda (x)
							     (if (< x n) (+ x 1) x))
							 (remove n weaken-neg1 :test #'=))))
			       (let ((n1 n))
				 (dolist (i weaken-neg1)
					 (when (< i n)
					   (setq n1 (- n1 1))))
				 (values (make-ftree-seq-focus n1 fs1 nil)
					 weaken-pos1
					 (mapcar #'(lambda (x)
						     (if (< x n) (+ x 1) x))
						 weaken-neg1)))))))
		   (MERGE (multiple-value-bind
			   (fs1 weaken-pos1 weaken-neg1)
			   (ftree-seq-weaken-early prem1)
			   (cond ((member 0 weaken-neg1 :test #'=)
				  (values fs1
					  weaken-pos1
					  (mapcar #'(lambda (x)
						      (- x 1))
						  (remove 0 weaken-neg1 :test #'=))))
				 ((member 1 weaken-neg1 :test #'=) ; and 0 not in it
				  (values fs1
					  weaken-pos1
					  (mapcar #'(lambda (x)
						      (- x 1))
						  (remove 1 weaken-neg1 :test #'=))))
				 (t
				  (values (make-ftree-seq-merge-neg fs1)
					  weaken-pos1
					  (mapcar #'(lambda (x)
						      (- x 1))
						  (remove 1 weaken-neg1 :test #'=)))))))
		   (CON (multiple-value-bind
			 (fs1 weaken-pos1 weaken-neg1)
			 (ftree-seq-weaken-early prem1)
			 (let* ((lnpos1 (length (ftree-seq-pos-wffs prem1)))
				(lnneg1 (length (ftree-seq-neg-wffs prem1))))
			   (if (member 0 weaken-neg1 :test #'=) ; need not consider other case
			       (let ((weaken-pos weaken-pos1)
				     (weaken-neg weaken-neg1))
				 (dotimes (i (length (ftree-seq-pos-wffs prem2)))
					  (push (+ i lnpos1) weaken-pos))
				 (dotimes (i (length (cdr (ftree-seq-neg-wffs prem2))))
					  (push (+ i lnneg1) weaken-neg))
				 (values fs1 weaken-pos weaken-neg))
			     (multiple-value-bind
			      (fs2 weaken-pos2 weaken-neg2)
			      (ftree-seq-weaken-early prem2)
			      (let ((weaken-pos (mapcar #'(lambda (x)
							    (+ lnpos1 x))
							weaken-pos2))
				    (weaken-neg (mapcar #'(lambda (x)
							    (+ lnneg1 x))
							(remove 0 weaken-neg2))))
				(if (member 0 weaken-neg2 :test #'=) ; need not consider first case
				    (progn
				      (dotimes (i lnneg1)
					       (push i weaken-neg))
				      (dotimes (i lnpos1)
					       (push i weaken-pos))
				      (values fs2 weaken-pos weaken-neg))
				  (values (make-ftree-seq-con-neg fs1 fs2)
					  (append weaken-pos1 weaken-pos)
					  (append weaken-neg1 weaken-neg)))))))))
		   (DIS (multiple-value-bind
			 (fs1 weaken-pos1 weaken-neg1)
			 (ftree-seq-weaken-early prem1)
			 (let ((weaken-neg (mapcar #'(lambda (x)
						       (- x 1))
						   (set-difference weaken-neg1
								   '(0 1) :test #'=))))
			   (if (member 1 weaken-neg1 :test #'=)
			       (if (member 0 weaken-neg1 :test #'=)
				   (values fs1 
					   weaken-pos1
					   (cons 0 weaken-neg))
				 (values 
				  (make-ftree-seq-dis-neg
				   (make-ftree-seq-focus
				    1
				    (make-ftree-seq-weaken (cadr (ftree-seq-neg-wffs prem1))
							   fs1 nil) nil))
				  weaken-pos1 weaken-neg))
			     (if (member 0 weaken-neg1 :test #'=)
				 (values
				  (make-ftree-seq-dis-neg
				   (make-ftree-seq-weaken (car (ftree-seq-neg-wffs prem1)) fs1 nil))
				  weaken-pos1 weaken-neg)
			       (values
				(make-ftree-seq-dis-neg fs1)
				weaken-pos1 weaken-neg))))))
		   (IMP (multiple-value-bind
			 (fs1 weaken-pos1 weaken-neg1)
			 (ftree-seq-weaken-early prem1)
			 (let ((weaken-pos (mapcar #'(lambda (x) (- x 1))
						   (remove 0 weaken-pos1 :test #'=))))
			   (if (member 0 weaken-pos1 :test #'=)
			       (if (member 0 weaken-neg1 :test #'=)
				   (values fs1 weaken-pos weaken-neg1)
				 (values (make-ftree-seq-imp-neg
					  (make-ftree-seq-weaken (car (ftree-seq-pos-wffs prem1))
								 fs1 t))
					 (remove 0 weaken-pos :test #'=)
					 weaken-neg1))
			     (if (member 0 weaken-neg1 :test #'=)
				 (values (make-ftree-seq-imp-neg
					  (make-ftree-seq-weaken (car (ftree-seq-neg-wffs prem1))
								 fs1 nil))
					 weaken-pos (remove 0 weaken-neg1 :test #'=))
			       (values (make-ftree-seq-imp-neg fs1)
				       weaken-pos weaken-neg1))))))
		   (NEG (multiple-value-bind
			 (fs1 weaken-pos1 weaken-neg1)
			 (ftree-seq-weaken-early prem1)
			 (let ((weaken-neg (mapcar #'(lambda (x) (+ x 1)) weaken-neg1))
			       (weaken-pos (mapcar #'(lambda (x) (- x 1))
						   (remove 0 weaken-pos1 :test #'=))))
			   (if (member 0 weaken-pos1 :test #'=)
			       (values fs1 weaken-pos (cons 0 weaken-neg))
			     (values (make-ftree-seq-neg-neg fs1) weaken-pos weaken-neg)))))
		   (EXP (multiple-value-bind
			 (fs1 weaken-pos1 weaken-neg1)
			 (ftree-seq-weaken-early prem1)
			 (values (if (member 0 weaken-neg1 :test #'=)
				     fs1
				   (make-ftree-seq-exp-neg (car (ftree-seq-neg-wffs fs))
							   (ftree-seq-exp-term fs)
							   fs1))
				 weaken-pos1 weaken-neg1)))
		   (SEL (multiple-value-bind
			 (fs1 weaken-pos1 weaken-neg1)
			 (ftree-seq-weaken-early prem1)
			 (values (if (member 0 weaken-neg1 :test #'=)
				     fs1
				   (make-ftree-seq-sel-neg (car (ftree-seq-neg-wffs fs))
							   (ftree-seq-sel-var fs)
							   fs1))
				 weaken-pos1 weaken-neg1)))
		   (REW (multiple-value-bind
			 (fs1 weaken-pos1 weaken-neg1)
			 (ftree-seq-weaken-early prem1)
			 (values (if (member 0 weaken-neg1 :test #'=)
				     fs1
				   (make-ftree-seq-rew-neg (car (ftree-seq-neg-wffs fs))
							   (ftree-seq-rew-just fs)
							   fs1))
				 weaken-pos1 weaken-neg1)))
		   (t (throwfail "Unexpected case " fs " in weaken early"))))))))

(defun ftree-seq-raise-lambdas (fs)
  (let* ((k (ftree-seq-kind fs))
	 (prem1 (ftree-seq-prem1 fs))
	 (prem2 (ftree-seq-prem2 fs))
	 (prem1* (when prem1 (ftree-seq-raise-lambdas prem1)))
	 (prem2* (when prem2 (ftree-seq-raise-lambdas prem2))))
    (case k
	  ((TRUE FALSE) fs)
	  (INIT
	   (let* ((wff (car (ftree-seq-pos-wffs fs)))
		  (wff* (lnorm wff)))
	     (if (wffeq wff wff*)
		 fs
	       (make-ftree-seq-init wff*))))
	  (WEAKEN
	   (let* ((pwff (if (ftree-seq-pos-rule fs)
			    (car (ftree-seq-pos-wffs fs))
			  (car (ftree-seq-neg-wffs fs))))
		  (pwff* (lnorm pwff)))
	     (make-ftree-seq-weaken pwff* prem1* (ftree-seq-pos-rule fs))))
	  (EXP
	   (let* ((etrm (ftree-seq-exp-term fs))
		  (etrm* (lnorm etrm)))
	     (if (ftree-seq-pos-rule fs)
		 (let* ((pwff (car (ftree-seq-pos-wffs fs)))
			(pwff* (lnorm pwff))
			(wffe* (substitute-l-term-var etrm* (bindvar pwff*) (cdr pwff*))))
		   (make-ftree-seq-exp-pos pwff* etrm*
					   (make-ftree-seq-lrew wffe* prem1* t)))
	       (let* ((pwff (car (ftree-seq-neg-wffs fs)))
		      (pwff* (lnorm pwff))
		      (wffe* (substitute-l-term-var etrm* (bindvar pwff*) (cdr pwff*))))
		 (make-ftree-seq-exp-neg pwff* etrm*
					 (make-ftree-seq-lrew wffe* prem1* nil))))))
	  (SEL
	   (let ((svar (ftree-seq-sel-var fs)))
	     (if (ftree-seq-pos-rule fs)
		 (let* ((pwff (car (ftree-seq-pos-wffs fs)))
			(pwff* (lnorm pwff)))
		   (make-ftree-seq-sel-pos pwff* svar prem1*))
	       (let* ((pwff (car (ftree-seq-neg-wffs fs)))
		      (pwff* (lnorm pwff)))
		 (make-ftree-seq-sel-neg pwff* svar prem1*)))))
	  (REW
	   (let ((rj (ftree-seq-rew-just fs)))
	     (if (ftree-seq-pos-rule fs)
		 (let* ((pwff (car (ftree-seq-pos-wffs fs)))
			(pwff* (lnorm pwff)))
		   (if (member rj '(LAMBDA BETA ETA AB))
		       (make-ftree-seq-lrew pwff* prem1* t)
		     (make-ftree-seq-rew-pos pwff* rj prem1*)))
	       (let* ((pwff (car (ftree-seq-neg-wffs fs)))
		      (pwff* (lnorm pwff)))
		 (if (member rj '(LAMBDA BETA ETA AB))
		     (make-ftree-seq-lrew pwff* prem1* nil)
		   (make-ftree-seq-rew-neg pwff* rj prem1*))))))
	  (t (ftree-seq-imitate-rule fs prem1* prem2*)))))

; checks the structure of the ftree-seq derivation
(defun check-ftree-seq (fs &optional params)
  (let ((prem1 (ftree-seq-prem1 fs))
	(prem2 (ftree-seq-prem2 fs))
	(neg-wffs (ftree-seq-neg-wffs fs))
	(pos-wffs (ftree-seq-pos-wffs fs)))
    (when (and (ftree-seq-prem2 fs)
	       (not (ftree-seq-prem1 fs)))
      (throwfail "Sequent " fs " has a second but not a first premiss"))
    (case (ftree-seq-kind fs)
	  (INIT
	   (unless (and (= (length pos-wffs) 1)
			(= (length neg-wffs) 1)
			(wffeq-ab (car pos-wffs) (car neg-wffs)))
	     (throwfail "Bad INIT sequent " fs))
	   (when prem1
	     (throwfail "INIT " fs " sequent has a premiss?")))
	  (NEG
	   (unless (and prem1 (not prem2))
	     (throwfail "NEG should have one premiss" fs))
	   (if (ftree-seq-pos-rule fs)
	       (unless (and (ftree-seq-neg-wffs prem1)
			    pos-wffs
			    (not-p (car pos-wffs))
			    (wffeq-ab (cdar pos-wffs)
				      (car (ftree-seq-neg-wffs prem1))))
		 (throwfail "Ill-formed NEG+"))
	       (unless (and (ftree-seq-pos-wffs prem1)
			    neg-wffs
			    (not-p (car neg-wffs))
			    (wffeq-ab (cdar neg-wffs)
				      (car (ftree-seq-pos-wffs prem1))))
		 (throwfail "Ill-formed NEG-"))))
	  (CON
	   (if (ftree-seq-pos-rule fs)
	       (unless (and prem1 (not prem2)
			    (ftree-seq-pos-wffs prem1)
			    (cdr (ftree-seq-pos-wffs prem1))
			    pos-wffs
			    (wffeq-ab (acons 'AND 
					     (car (ftree-seq-pos-wffs prem1))
					     (cadr (ftree-seq-pos-wffs prem1)))
				      (car pos-wffs)))
		 (throwfail "Ill-formed CON+"))
	       (unless (and prem1 prem2
			    (ftree-seq-neg-wffs prem2)
			    (ftree-seq-neg-wffs prem1)
			    neg-wffs
			    (wffeq-ab (acons 'AND 
					     (car (ftree-seq-neg-wffs prem1))
					     (car (ftree-seq-neg-wffs prem2)))
				      (car neg-wffs)))
		 (throwfail "Ill-formed CON-"))))
	  (DIS
	   (if (ftree-seq-pos-rule fs)
	       (unless (and prem1 prem2
			    (ftree-seq-pos-wffs prem1)
			    (ftree-seq-pos-wffs prem2)
			    pos-wffs
			    (wffeq-ab (acons 'OR
					     (car (ftree-seq-pos-wffs prem1))
					     (car (ftree-seq-pos-wffs prem2)))
				      (car pos-wffs)))
		 (throwfail "Ill-formed DIS+"))
	     (unless (and prem1 (not prem2)
			  (ftree-seq-neg-wffs prem1)
			  (cdr (ftree-seq-neg-wffs prem1))
			  neg-wffs
			  (wffeq-ab (acons 'OR
					   (car (ftree-seq-neg-wffs prem1))
					   (cadr (ftree-seq-neg-wffs prem1)))
				    (car neg-wffs)))
	       (throwfail "Ill-formed DIS-"))))
	  (IMP
	   (if (ftree-seq-pos-rule fs)
	       (unless (and prem1 prem2
			    (ftree-seq-neg-wffs prem1)
			    (ftree-seq-pos-wffs prem2)
			    pos-wffs
			    (equal (cdr pos-wffs)
				   (append (ftree-seq-pos-wffs prem1)
					   (cdr (ftree-seq-pos-wffs prem2))))
			    (equal neg-wffs
				   (append (cdr (ftree-seq-neg-wffs prem1))
					   (ftree-seq-neg-wffs prem2)))
			    (wffeq-ab (acons 'IMPLIES
					     (car (ftree-seq-neg-wffs prem1))
					     (car (ftree-seq-pos-wffs prem2)))
				      (car pos-wffs)))
		 (throwfail "Ill-formed IMP+"))
	     (unless (and prem1 (not prem2)
			  (ftree-seq-neg-wffs prem1)
			  (ftree-seq-pos-wffs prem1)
			  neg-wffs
			  (wffeq-ab (acons 'IMPLIES
					   (car (ftree-seq-pos-wffs prem1))
					   (car (ftree-seq-neg-wffs prem1)))
				    (car neg-wffs)))
	       (throwfail "Ill-formed IMP-"))))
	  (TRUE
	   (if (ftree-seq-pos-rule fs)
	       (throwfail "Cannot be a + application of TRUE")
	     (unless (and (not prem1) (not prem2)
			  (null pos-wffs)
			  (equal neg-wffs '(TRUTH)))
	       (throwfail "Ill-formed TRUE-"))))
	  (FALSE
	   (if (ftree-seq-pos-rule fs)
	       (unless (and (not prem1) (not prem2)
			    (null neg-wffs)
			    (equal pos-wffs '(FALSEHOOD)))
		 (throwfail "Ill-formed TRUE-"))
	     (throwfail "Cannot be a - application of FALSE")))
	  (REW
	   (unless (and prem1 (not prem2))
	     (throwfail "REW " fs " should have exactly one premiss"))
	   (let ((wff (if (ftree-seq-pos-rule fs)
			  (car pos-wffs)
			(car neg-wffs)))
		 (wff2 (if (ftree-seq-pos-rule fs)
			   (car (ftree-seq-pos-wffs prem1))
			 (car (ftree-seq-neg-wffs prem1)))))
	     (case (ftree-seq-rew-just fs)
		   (EQUIV-IMPLICS
		    (unless (equiv-p wff)
		      (throwfail "Princ formula of EQUIV-IMPLICS REW "
				 fs " is not an equiv"))
		    (unless (wffeq wff2 (acons 'AND (acons 'IMPLIES (cdar wff) (cdr wff))
					      (acons 'IMPLIES (cdr wff) (cdar wff))))
		      (throwfail "Illegal EQUIV-IMPLICS REW " fs)))
;		   (EXT=
;		    (unless (or (equals-p wff) (and (not-p wff) (equals-p (cdr wff))))
;		      (throwfail "Princ formula of EXT= REW " fs " is not an =")))
;		   (LEIBNIZ=
;		    (unless (or (equals-p wff) (and (not-p wff) (equals-p (cdr wff))))
;		      (throwfail "Princ formula of Leibniz= REW " fs " is not an =")))
		   (LAMBDA
		    (unless (wffeq-ab (lnorm wff) wff2)
		      (throwfail "Illegal Lambda REW " fs)))
		   (BETA
		    (unless (wffeq-ab (lambda-norm wff) wff2)
		      (throwfail "Illegal Beta REW " fs)))
		   (ETA
		    (unless (wffeq-ab (etanorm wff) wff2)
		      (throwfail "Illegal Eta REW " fs))))))
	  (EXP
	   (unless (and prem1 (not prem2))
	     (throwfail "EXP " fs " should have exactly one premiss"))
	   (if (ftree-seq-pos-rule fs)
	       (unless (and pos-wffs
			    (a-bd-wff-p (car pos-wffs))
			    (equal neg-wffs (ftree-seq-neg-wffs prem1))
			    (equal (cdr pos-wffs) (cdr (ftree-seq-pos-wffs prem1)))
			    (wffeq-ab (substitute-l-term-var (ftree-seq-exp-term fs)
							     (bindvar (car pos-wffs))
							     (cdar pos-wffs))
				      (car (ftree-seq-pos-wffs prem1))))
		 (throwfail "Ill-formed EXP+"))
	       (unless (and neg-wffs
			    (e-bd-wff-p (car neg-wffs))
			    (equal pos-wffs (ftree-seq-pos-wffs prem1))
			    (equal (cdr neg-wffs) (cdr (ftree-seq-neg-wffs prem1)))
			    (wffeq-ab (substitute-l-term-var (ftree-seq-exp-term fs)
							     (bindvar (car neg-wffs))
							     (cdar neg-wffs))
				      (car (ftree-seq-neg-wffs prem1))))
		 (throwfail "Ill-formed EXP-"))))
	  (SEL
	   (unless (and prem1 (not prem2))
	     (throwfail "SEL " fs " should have exactly one premiss"))
	   (let ((param (ftree-seq-sel-var fs)))
	     (when (member param params)
	       (throwfail "SEL param " param " is not new"))
	     (push param params)
	     (if (ftree-seq-pos-rule fs)
		 (unless (and pos-wffs
			    (e-bd-wff-p (car pos-wffs))
			    (equal neg-wffs (ftree-seq-neg-wffs prem1))
			    (equal (cdr pos-wffs) (cdr (ftree-seq-pos-wffs prem1)))
			    (wffeq-ab (substitute-l-term-var (ftree-seq-sel-var fs)
							     (bindvar (car pos-wffs))
							     (cdar pos-wffs))
				      (car (ftree-seq-pos-wffs prem1))))
		   (throwfail "Ill-formed SEL+"))
	       (unless (and neg-wffs
			    (a-bd-wff-p (car neg-wffs))
			    (equal pos-wffs (ftree-seq-pos-wffs prem1))
			    (equal (cdr neg-wffs) (cdr (ftree-seq-neg-wffs prem1)))
			    (wffeq-ab (substitute-l-term-var (ftree-seq-sel-var fs)
							     (bindvar (car neg-wffs))
							     (cdar neg-wffs))
				      (car (ftree-seq-neg-wffs prem1))))
		 (throwfail "Ill-formed SEL-")))))
	  (CUT
	   (unless (and prem1 prem2
			(ftree-seq-neg-wffs prem1)
			(ftree-seq-pos-wffs prem2)
			(wffeq-ab (car (ftree-seq-neg-wffs prem1))
				  (car (ftree-seq-pos-wffs prem2)))
			(equal (ftree-seq-pos-wffs fs)
			       (append (ftree-seq-pos-wffs prem1)
				       (cdr (ftree-seq-pos-wffs prem2))))
			(equal (ftree-seq-neg-wffs fs)
			       (append (cdr (ftree-seq-neg-wffs prem1))
				       (ftree-seq-neg-wffs prem2))))
	     (throwfail "Ill-formed CUT")))
	  (MERGE
	   (unless (and prem1 (not prem2))
	     (throwfail "MERGE " fs " should have exactly one premiss"))
	   (let ((wffs1 (if (ftree-seq-pos-rule fs)
			    (ftree-seq-pos-wffs prem1)
			  (ftree-seq-neg-wffs prem1)))
		 (wffs (if (ftree-seq-pos-rule fs)
			   pos-wffs
			 neg-wffs)))
	     (unless (and wffs1 (cdr wffs1) wffs
			  (wffeq-ab (car wffs1) (car wffs))
			  (wffeq-ab (cadr wffs1) (car wffs))
			  (equal (cddr wffs1) (cdr wffs)))
	       (throwfail "MERGE ill-formed"))))
	  (FOCUS
	   (unless (and prem1 (not prem2))
	     (throwfail "FOCUS " fs " should have exactly one premiss"))
	   (let ((wffs1 (if (ftree-seq-pos-rule fs)
			    (ftree-seq-pos-wffs prem1)
			  (ftree-seq-neg-wffs prem1)))
		 (wffs (if (ftree-seq-pos-rule fs)
			   pos-wffs
			 neg-wffs)))
	     (unless (= (length wffs1) (length wffs))
	       (throwfail "FOCUS " fs " should not change the number of wffs in sequent"))
	     (unless (wffeq-ab (car wffs) (nth (ftree-seq-focus-n fs) wffs1))
	       (throwfail "Ill-formed FOCUS"))))
	  (WEAKEN
	   (unless (and prem1 (not prem2))
	     (throwfail "WEAKEN " fs " should have exactly one premiss"))
	   (if (ftree-seq-pos-rule fs)
	       (unless (and (equal (cdr (ftree-seq-pos-wffs fs))
				   (ftree-seq-pos-wffs prem1))
			    (equal (ftree-seq-neg-wffs fs)
				   (ftree-seq-neg-wffs prem1)))
		 (throwfail "Ill-formed WEAKEN+"))
	       (unless (and (equal (ftree-seq-pos-wffs fs)
				   (ftree-seq-pos-wffs prem1))
			    (equal (cdr (ftree-seq-neg-wffs fs))
				   (ftree-seq-neg-wffs prem1)))
		 (throwfail "Ill-formed WEAKEN-"))))
	  (t (throwfail "Unknown kind of seq calculus rule " fs)))
    (when prem1
      (check-ftree-seq prem1 params))
    (when prem2
      (check-ftree-seq prem2 params))))

(defun seq-to-nat (sname prefix)
  (declare (special *current-seqder*))
  (let ((der (or (get sname 'FTREE-SEQ) (get sname 'EXT-SEQ))))
    (unless der
      (throwfail sname " is not the name of a sequent calculus derivation"))
    (setq *current-seqder* sname)
    (if (ftree-seq-p der)
	(let ((neg-wffs (ftree-seq-neg-wffs der))
	      (pos-wffs (ftree-seq-pos-wffs der)))
	  (if neg-wffs
	      (progn
		(prove2 (car neg-wffs) prefix 1)
		(let ((conc (numalias 1))
		      (ln 1))
		  (ftree-seq-to-proof-rec
		   der (mapcar #'(lambda (w)
				   (introduce-gap conc 1)
				   (my-comdecode (list 'cl-user::hyp (+ ln 1) ln
						       (list 'quote w)
						       '$ '$ '$))
				   (prog1
				       (numalias ln)
				     (incf ln)))
			       pos-wffs)
		   (cons 'CONC
			 (mapcar #'(lambda (w)
				     (introduce-gap conc 1)
				     (my-comdecode (list 'cl-user::hyp (+ ln 1) ln
							 (list 'quote (cons 'NOT w))
							 '$ '$ '$))
				     (prog1
					 (numalias ln)
				       (incf ln)))
				 (cdr neg-wffs)))
		   conc)))
	    (progn
	      (prove1 'FALSEHOOD prefix 1)
	      (let ((conc (numalias 1))
		    (ln 1))
		(ftree-seq-to-proof-ab
		 der (mapcar #'(lambda (w)
				 (introduce-gap conc 1)
				 (my-comdecode (list 'cl-user::hyp (+ ln 1) ln
						     (list 'quote w)
						     '$ '$ '$))
				 (prog1
				     (numalias ln)
				   (incf ln)))
			     pos-wffs)
		 nil conc))))
	  (cleanup)
	  (remove-unnecessary-gaps))
      (let ((wffs (ext-seq-wffs der))
	    (wff (find-if-not #'not-p wffs)))
	(if wff
	    (progn
	      (prove2 wff prefix 1)
	      (let ((conc (numalias 1))
		    (ln 1))
		(ext-seq-to-proof-rec
		 der 
		 (mapcar #'(lambda (w)
			     (if (eq w wff)
				 (progn
				   (setq wff nil)
				   (cons w 'CONC))
			       (progn
				 (introduce-gap conc 1)
				 (if (not-p w)
				     (my-comdecode (list 'cl-user::hyp (+ ln 1) ln
							 (list 'quote (cdr w))
							 '$ '$ '$))
				   (my-comdecode (list 'cl-user::hyp (+ ln 1) ln
						       (list 'quote (cons 'NOT w))
						       '$ '$ '$)))
				 (prog1
				     (cons w (numalias ln))
				   (incf ln)))))
			 wffs)
		 conc)))
	  (progn
	    (prove1 'FALSEHOOD prefix 1)
	    (let ((conc (numalias 1))
		  (ln 1))
	      (ext-seq-to-proof-indirect
	       der (mapcar #'(lambda (w)
			       (introduce-gap conc 1)
			       (my-comdecode (list 'cl-user::hyp (+ ln 1) ln
						   (list 'quote (cdr w))
						   '$ '$ '$))
			       (prog1
				   (cons w (numalias ln))
				 (incf ln)))
			   wffs)
	       conc))))))))

(defun ftree-seq-to-proof-rec (der pos-lines neg-lines conc)
  (unless (eq conc (caar (proof-plans dproof)))
    (subproof conc))
  (let ((ln (linealias conc)))
    (case (ftree-seq-kind der)
	  (INIT
	   (if (eq (car neg-lines) 'CONC)
	       (my-comdecode (list 'cl-user::same ln
				(linealias (car pos-lines))
				'$ '$ '$))
	     (if (wffeq (cons 'NOT (car (ftree-seq-neg-wffs der)))
			(line-assertion (car neg-lines)))
		 (my-comdecode (list 'cl-user::eneg ln
				  (linealias (car neg-lines))
				  (linealias (car pos-lines))
				  '$ '$ '$ '$))
	       (my-comdecode (list 'cl-user::eneg ln
				(linealias (car pos-lines))
				(linealias (car neg-lines))
				'$ '$ '$ '$)))))
	  (CON
	   (if (ftree-seq-pos-rule der)
	     ; con left
	       (progn
		 (introduce-gap conc 2)
		 (my-comdecode (list 'cl-user::econj (linealias (car pos-lines))
				  (+ ln 1) ln
				  '$ '$ '$ '$ '$))
		 (ftree-seq-to-proof-ab
		  (ftree-seq-prem1 der)
		  (cons (numalias ln)
			(cons (numalias (+ ln 1))
			      (cdr pos-lines)))
		  neg-lines
		  conc))
	     (if (eq (car neg-lines) 'CONC)
		 (progn
		   (introduce-gap conc 2)
		   (my-comdecode (list 'cl-user::iconj (+ ln 2)
				    (+ ln 1) ln '$ '$ '$ '$ '$))
		   (let ((p1 (numalias ln))
			 (p2 (numalias (+ ln 1)))
			 (prem1 (ftree-seq-prem1 der))
			 (prem2 (ftree-seq-prem2 der))
			 (pos-lines1 nil)
			 (neg-lines1 nil)
			 (pos-lines2 pos-lines)
			 (neg-lines2 (cdr neg-lines)))
		     (dotimes (i (length (ftree-seq-pos-wffs prem1)))
			      (push (car pos-lines2) pos-lines1)
			      (pop pos-lines2))
		     (setq pos-lines1 (reverse pos-lines1))
		     (dotimes (i (length (cdr (ftree-seq-neg-wffs prem1))))
			      (push (car neg-lines2) neg-lines1)
			      (pop neg-lines2))
		     (setq neg-lines1 (reverse neg-lines1))
		     (ftree-seq-to-proof-ab
		      prem1 pos-lines1
		      (cons 'CONC neg-lines1)
		      p1)
		     (ftree-seq-to-proof-ab
		      prem2 pos-lines2
		      (cons 'CONC neg-lines2)
		      p2)))
	       (let ((ln2 (linealias (car neg-lines))))
		 (introduce-gap conc 5)
		 (my-comdecode (list 'cl-user::pushneg
				  ln2 ln
				  '$ '$ '$ '$))
		 (my-comdecode (list 'cl-user::cases (+ ln 5) ln
				  (+ ln 4) (+ ln 3)
				  (+ ln 2) (+ ln 1)
				  '$ '$ '$ '$ '$ '$ '$ '$ '$))
		 (let ((case1 (numalias (+ ln 1)))
		       (case1-conc (numalias (+ ln 2)))
		       (case2 (numalias (+ ln 3)))
		       (case2-conc (numalias (+ ln 4)))
		       (pos-lines1 nil)
		       (neg-lines1 nil)
		       (pos-lines2 pos-lines)
		       (neg-lines2 (cdr neg-lines))
		       (prem1 (ftree-seq-prem1 der)))
		   (dotimes (i (length (ftree-seq-pos-wffs prem1)))
			    (push (car pos-lines2) pos-lines1)
			    (pop pos-lines2))
		   (setq pos-lines1 (reverse pos-lines1))
		   (dotimes (i (length (cdr (ftree-seq-neg-wffs prem1))))
			    (push (car neg-lines2) neg-lines1)
			    (pop neg-lines2))
		   (setq neg-lines1 (reverse neg-lines1))
		   (ftree-seq-to-proof-ab
		    (ftree-seq-prem1 der)
		    pos-lines1
		    (cons case1 neg-lines1)
		    case1-conc)
		   (ftree-seq-to-proof-ab
		    (ftree-seq-prem2 der)
		    pos-lines2
		    (cons case2 neg-lines2)
		    case2-conc))))))
	(DIS
	 (if (ftree-seq-pos-rule der)
	     ; dis left - cases
	     (progn
	       (introduce-gap conc 4)
	       (my-comdecode (list 'cl-user::cases (+ ln 4) (linealias (car pos-lines))
				(+ ln 3) (+ ln 2)
				(+ ln 1) ln
				'$ '$ '$ '$ '$ '$ '$ '$ '$))
	       (let ((case1 (numalias ln))
		     (case1-conc (numalias (+ ln 1)))
		     (case2 (numalias (+ ln 2)))
		     (case2-conc (numalias (+ ln 3)))
		     (pos-lines1 nil)
		     (neg-lines1 nil)
		     (pos-lines2 (cdr pos-lines))
		     (neg-lines2 neg-lines)
		     (prem1 (ftree-seq-prem1 der)))
		 (dotimes (i (length (cdr (ftree-seq-pos-wffs prem1))))
			  (push (car pos-lines2) pos-lines1)
			  (pop pos-lines2))
		 (setq pos-lines1 (reverse pos-lines1))
		 (dotimes (i (length (ftree-seq-neg-wffs prem1)))
			  (push (car neg-lines2) neg-lines1)
			  (pop neg-lines2))
		 (setq neg-lines1 (reverse neg-lines1))
		 (ftree-seq-to-proof-ab
		  (ftree-seq-prem1 der)
		  (cons case1 pos-lines1)
		  neg-lines1
		  case1-conc)
		 (ftree-seq-to-proof-ab
		  (ftree-seq-prem2 der)
		  (cons case2 pos-lines2)
		  neg-lines2
		  case2-conc)))
	   ; dis R  - calling weaken-early first will result in fewer indirect
					; proofs here
	   (if (eq (car neg-lines) 'CONC)
	       (let ((prem1 (ftree-seq-prem1 der)))
		 (cond ((and (eq (ftree-seq-kind prem1) 'WEAKEN)
			     (not (ftree-seq-pos-rule prem1)))
			(introduce-gap conc 1)
			(my-comdecode (list 'cl-user::idisj-right (+ ln 1) ln
					 '$ '$ '$ '$))
			(ftree-seq-to-proof-ab
			 (ftree-seq-prem1 prem1)
			 pos-lines neg-lines (numalias ln)))
		       ((and (eq (ftree-seq-kind prem1) 'FOCUS)
			     (not (ftree-seq-pos-rule prem1))
			     (eq (ftree-seq-focus-n prem1) 1)
			     (eq (ftree-seq-kind (ftree-seq-prem1 prem1)) 'WEAKEN)
			     (not (ftree-seq-pos-rule (ftree-seq-prem1 prem1))))
			(introduce-gap conc 1)
			(my-comdecode (list 'cl-user::idisj-left (+ ln 1) ln
					 '$ '$ '$ '$))
			(ftree-seq-to-proof-ab
			 (ftree-seq-prem1 (ftree-seq-prem1 prem1))
			 pos-lines neg-lines (numalias ln)))
		       (t
			(introduce-gap conc 2)
			(my-comdecode (list 'cl-user::indirect (+ ln 2) (+ ln 1) ln
					 '$ '$ '$ '$))
			(ftree-seq-to-proof-ab
			 der pos-lines (cons (numalias ln) (cdr neg-lines))
			 (numalias (+ ln 1))))))
	     (let ((ln2 (linealias (car neg-lines))))
	       (introduce-gap conc 3)
	       (my-comdecode (list 'cl-user::pushneg ln2 ln '$ '$ '$ '$))
	       (my-comdecode (list 'cl-user::econj ln (+ ln 2) (+ ln 1)
				'$ '$ '$ '$ '$))
	       (ftree-seq-to-proof-ab
		(ftree-seq-prem1 der)
		pos-lines (cons (numalias (+ ln 1)) (cons (numalias (+ ln 2))
							   (cdr neg-lines)))
		conc)))))
	(IMP
	 (if (ftree-seq-pos-rule der)
	     ; imp left - MP
	     (let ((pos-lines1 nil)
		   (neg-lines1 nil)
		   (pos-lines2 (cdr pos-lines))
		   (neg-lines2 neg-lines))
	       (dotimes (i (length (ftree-seq-pos-wffs
				    (ftree-seq-prem1 der))))
			(push (car pos-lines2) pos-lines1)
			(pop pos-lines2))
	       (setq pos-lines1 (reverse pos-lines1))
	       (dotimes (i (length (cdr (ftree-seq-neg-wffs
					 (ftree-seq-prem1 der)))))
			(if (eq (car neg-lines2) 'CONC)
			    (progn
			      (introduce-gap conc 2)
			      (my-comdecode (list 'cl-user::indirect (+ ln 2) (+ ln 1) ln
					       '$ '$ '$ '$))
			      (setq conc (numalias (+ ln 1)))
			      (push (numalias ln) neg-lines1))
			  (push (car neg-lines2) neg-lines1))
			(pop neg-lines2))
	       (setq neg-lines1 (reverse neg-lines1))
	       (let ((ln9 (linealias conc))
		     (hyps (mapcar #'(lambda (x) ; note that we may have done an indirect adding a hyp ("Assume Negation")
				       (linealias x))
				   (line-hypotheses conc))))
		 (introduce-gap conc 2)
		 (my-comdecode (list 'cl-user::mp (linealias (car pos-lines))
				  (+ ln9 1) ln9 '$ '$ '$ 
				  (list 'quote hyps) (list 'quote hyps)))
		 (let ((mp-ant (numalias ln9))
		       (mp-conc (numalias (+ ln9 1))))
		   (ftree-seq-to-proof-ab
		    (ftree-seq-prem1 der)
		    pos-lines1
		    (cons 'CONC neg-lines1)
		    mp-ant)
		   (ftree-seq-to-proof-ab
		    (ftree-seq-prem2 der)
		    (cons mp-conc pos-lines2)
		    neg-lines2
		    conc))))
	   ; Deduct
	   (if (eq (car neg-lines) 'CONC)
	       (progn
		 (introduce-gap conc 2)
		 (my-comdecode (list 'cl-user::deduct (+ ln 2) (+ ln 1) ln
				  '$ '$ '$ '$ '$))
		 (ftree-seq-to-proof-ab
		  (ftree-seq-prem1 der)
		  (cons (numalias ln) pos-lines)
		  (cons 'CONC (cdr neg-lines))
		  (numalias (+ ln 1))))
	     (progn
	       (introduce-gap conc 3)
	       (my-comdecode (list 'cl-user::pushneg (+ (linealias (car neg-lines)))
				ln '$ '$ '$ '$))
	       (my-comdecode (list 'cl-user::econj ln (+ ln 2) (+ ln 1)
				'$ '$ '$ '$ '$))
	       (ftree-seq-to-proof-ab
		(ftree-seq-prem1 der)
		(cons (numalias (+ ln 1)) pos-lines)
		(cons (numalias (+ ln 2)) (cdr neg-lines))
		conc)))))
	(TRUE
	 (if (eq (car neg-lines) 'CONC)
	     (my-comdecode (list 'cl-user::itruth ln '$))
	   (progn
	     (introduce-gap conc 1)
	     (my-comdecode (list 'cl-user::pushneg (linealias (car neg-lines)) ln
			      '$ '$ '$ '$))
	     (my-comdecode (list 'cl-user::absurd (+ ln 1) ln '$ '$ '$)))))
	(FALSE
	 (my-comdecode (list 'cl-user::absurd ln (linealias (car pos-lines))
			  '$ '$ '$)))
	(NEG
	 (if (ftree-seq-pos-rule der)
	     (ftree-seq-to-proof-ab
	      (ftree-seq-prem1 der)
	      (cdr pos-lines)
	      (cons (car pos-lines)
		    neg-lines)
	      conc)
	   (if (eq (car neg-lines) 'CONC)
	       (progn
		 (introduce-gap conc 2)
		 (my-comdecode (list 'cl-user::ineg (+ ln 2) (+ ln 1) ln
				  '$ '$ '$ '$))
		 (ftree-seq-to-proof-ab
		  (ftree-seq-prem1 der)
		  (cons (numalias ln) pos-lines)
		  (cdr neg-lines)
		  (numalias (+ ln 1))))
					; there are two possibilities here -
					; 1. the neg wff A is a supp line ~A
					; 2. the neg wff ~A is a supp line A
	       (if (wffeq (line-assertion (car neg-lines))
			  (cons 'not (car (ftree-seq-neg-wffs der)))) ; 1.
		   (progn
		     (introduce-gap conc 1)
		     (my-comdecode (list 'cl-user::pushneg (linealias (car neg-lines)) ln
				      '$ '$ '$ '$))
		     (ftree-seq-to-proof-ab
		      (ftree-seq-prem1 der)
		      (cons (numalias ln) pos-lines)
		      (cdr neg-lines)
		      conc))
		 (progn ; 2.
		   (ftree-seq-to-proof-ab
		    (ftree-seq-prem1 der)
		    (cons (car neg-lines) pos-lines)
		    (cdr neg-lines)
		    conc))))))
	(REW
	 (let ((prem1 (ftree-seq-prem1 der)))
	   (if (ftree-seq-pos-rule der)
	       (progn
		 (introduce-gap conc 1)
		 (if (eq (ftree-seq-rew-just der) 'EQUIV-IMPLICS)
		     (my-comdecode (list 'cl-user::equiv-implics (linealias (car pos-lines)) ln
				      '$ '$ '$ '$))
		   (my-comdecode (list (case (ftree-seq-rew-just der)
					  (LAMBDA 'cl-user::lambda*)
					  (BETA 'cl-user::beta*)
					  (ETA 'cl-user::eta*)
					  (AB 'cl-user::ab*)
					  (EQUIVWFFS 'cl-user::equiv-wffs)
					  (REFL= 'cl-user::refl=)
					  ((LEIBNIZ= EXT=) 'cl-user::equiv-eq))
				    (linealias (car pos-lines)) ln
				    (list 'quote (car (ftree-seq-pos-wffs prem1)))
				    (list 'quote (car (ftree-seq-pos-wffs der)))
				    '$ '$)))
		 (ftree-seq-to-proof-ab
		  prem1 (cons (numalias ln) (cdr pos-lines)) neg-lines conc))
	     (if (eq (car neg-lines) 'CONC)
		 (progn
		   (introduce-gap conc 1)
		   (if (eq (ftree-seq-rew-just der) 'EQUIV-IMPLICS)
		       (my-comdecode (list 'cl-user::implics-equiv (+ ln 1) ln '$ '$ '$ '$))
		     (my-comdecode (list (case (ftree-seq-rew-just der)
					    (LAMBDA 'cl-user::lambda*)
					    (BETA 'cl-user::beta*)
					    (ETA 'cl-user::eta*)
					    (AB 'cl-user::ab*)
					    (EQUIVWFFS 'cl-user::equiv-wffs)
					    (REFL= 'cl-user::refl=)
					    ((LEIBNIZ= EXT=) 'cl-user::equiv-eq))
				      ln (+ ln 1)
				      (list 'quote (car (ftree-seq-neg-wffs der)))
				      (list 'quote (car (ftree-seq-neg-wffs prem1)))
				      '$ '$)))
		   (ftree-seq-to-proof-ab
		    prem1 pos-lines neg-lines (numalias ln)))
	       (progn
		 (introduce-gap conc 1)
		 (if (eq (ftree-seq-rew-just der) 'EQUIV-IMPLICS)
					; this case is a bit tough
		       (if (member 'CONC (cdr neg-lines))
			   (progn
			     (introduce-gap conc 2)
			     (my-comdecode (list 'cl-user::indirect (+ ln 2) (+ ln 1) ln
					      '$ '$ '$ '$))
			     (ftree-seq-to-proof-ab
			      der pos-lines 
			      (mapcar #'(lambda (nline)
					  (if (eq nline 'CONC)
					      (numalias ln)
					    nline))
				      neg-lines)
			      (numalias (+ ln 1))))
			 (progn
			   (introduce-gap conc 1)
			   (my-comdecode (list 'cl-user::eneg (+ ln 1)
					    (linealias (car neg-lines))
					    ln '$ '$ '$ '$))
			   (ftree-seq-to-proof-ab
			    der pos-lines (cons 'CONC (cdr neg-lines))
			    (numalias ln))))
		   (progn
		     (my-comdecode (list (case (ftree-seq-rew-just der)
					    (LAMBDA 'cl-user::lambda*)
					    (BETA 'cl-user::beta*)
					    (ETA 'cl-user::eta*)
					    (AB 'cl-user::ab*)
					    (EQUIVWFFS 'cl-user::equiv-wffs)
					    (REFL= 'cl-user::refl=)
					    ((LEIBNIZ= EXT=) 'cl-user::equiv-eq))
				      (linealias (car neg-lines)) ln
				      (list 'quote (cons 'NOT (car (ftree-seq-neg-wffs prem1))))
				      (list 'quote (cons 'NOT (car (ftree-seq-neg-wffs der))))
				      '$ '$))
		     (ftree-seq-to-proof-ab
		      prem1 pos-lines (cons (numalias ln) (cdr neg-lines))
		      conc))))))))
	(SEL
	 (if (ftree-seq-pos-rule der)
					; ruleC
	     (progn
	       (introduce-gap conc 2)
	       (my-comdecode (list 'cl-user::rulec (+ ln 2) (linealias (car pos-lines))
				(+ ln 1) ln 
				(list 'quote (ftree-seq-sel-var der))
				'$ '$ '$ '$ '$ '$ '$ '$))
	       (ftree-seq-to-proof-ab
		(ftree-seq-prem1 der)
		(cons (numalias ln) (cdr pos-lines))
		neg-lines
		(numalias (+ ln 1))))
	   (if (eq (car neg-lines) 'CONC)
	       (let ((wff (car (ftree-seq-neg-wffs der)))
		     (z (ftree-seq-sel-var der)))
		 (if (eq (bindvar wff) z)
		     (progn
		       (introduce-gap conc 1)
		       (my-comdecode (list 'cl-user::ugen (+ ln 1) ln '$ '$ '$ '$)))
		   (progn
		     (introduce-gap conc 2)
		     (my-comdecode (list 'cl-user::ab* (+ ln 1) (+ ln 2)
				      (list 'quote wff)
				      (list 'quote (acons (ftree-seq-sel-var der)
							  'FORALL
							  (substitute-l-term-var
							   z (bindvar wff)
							   (cdr wff))))
				      '$ '$))
		     (my-comdecode (list 'cl-user::ugen (+ ln 1) ln '$ '$ '$ '$))))
		 (ftree-seq-to-proof-ab
		  (ftree-seq-prem1 der)
		  pos-lines neg-lines
		  (numalias ln)))
	     (progn
	       (introduce-gap conc 3)
	       (my-comdecode (list 'cl-user::pushneg (linealias (car neg-lines)) ln
				'$ '$ '$ '$))
	       (my-comdecode (list 'cl-user::rulec (+ ln 3) ln
				(+ ln 2) (+ ln 1)
				(list 'quote (ftree-seq-sel-var der))
				'$ '$ '$ '$ '$ '$ '$ '$))
	       (ftree-seq-to-proof-ab
		(ftree-seq-prem1 der)
		pos-lines
		(cons (numalias (+ ln 1)) (cdr neg-lines))
		(numalias (+ ln 2)))))))
	(EXP
	 (let ((prem1 (ftree-seq-prem1 der))
	       (e (ftree-seq-exp-term der)))
	   (if (ftree-seq-pos-rule der)
	       (progn
		 (introduce-gap conc 1)
		 (my-comdecode (list 'cl-user::ui (linealias (car pos-lines)) ln
				  (list 'quote e)
				  '$ '$ '$ '$ '$))
		 (ftree-seq-to-proof-ab
		  prem1 (cons (numalias ln) (cdr pos-lines)) neg-lines
		  conc))
	     (if (eq (car neg-lines) 'CONC)
		 (progn
		   (introduce-gap conc 1)
		   (my-comdecode (list 'cl-user::egen (+ ln 1) ln (list 'quote e)
				    '$ '$ '$ '$ '$))
		   (ftree-seq-to-proof-ab
		    prem1 pos-lines neg-lines
		    (numalias ln)))
	       (progn
		 (introduce-gap conc 2)
		 (my-comdecode (list 'cl-user::pushneg (linealias (car neg-lines)) ln
				  '$ '$ '$ '$))
		 (my-comdecode (list 'cl-user::ui ln (+ ln 1)
				  (list 'quote e)
				  '$ '$ '$ '$ '$))
		 (ftree-seq-to-proof-ab
		  prem1 pos-lines (cons (numalias (+ ln 1)) (cdr neg-lines))
		  conc))))))
	(CUT
	 (let* ((prem1 (ftree-seq-prem1 der))
		(prem2 (ftree-seq-prem2 der))
		(pos-lines1 nil)
		(neg-lines1 nil)
		(pos-lines2 pos-lines)
		(neg-lines2 neg-lines))
	   (dotimes (i (length (ftree-seq-pos-wffs prem1)))
		    (push (car pos-lines2) pos-lines1)
		    (pop pos-lines2))
	   (setq pos-lines1 (reverse pos-lines1))
	   (dotimes (i (length (cdr (ftree-seq-neg-wffs prem1))))
		    (push (car neg-lines2) neg-lines1)
		    (pop neg-lines2))
	   (setq neg-lines1 (reverse neg-lines1))
	   (introduce-gap conc 1)
	   (my-comdecode (list 'cl-user::lemma (+ ln 1) ln
			    (list 'quote (car (ftree-seq-pos-wffs prem2)))
			    '$ '$ '$))
	   (let ((cutline (numalias ln)))
	     (if (member 'CONC neg-lines1)
		 (progn
		   (introduce-gap cutline 2)
		   (my-comdecode (list 'cl-user::indirect (+ ln 2) (+ ln 1) ln
				    '$ '$ '$ '$))
		   (ftree-seq-to-proof-ab prem1 pos-lines1
					   (cons (numalias ln) neg-lines1)
					   cutline))
	       (progn
		 (ftree-seq-to-proof-ab prem1 pos-lines1
					 (cons 'CONC neg-lines1) cutline)
		 (ftree-seq-to-proof-ab prem2 
					 (cons cutline pos-lines2)
					 neg-lines2 conc))))))
	(MERGE
	 (if (ftree-seq-pos-rule der)
	     (ftree-seq-to-proof-ab
	      (ftree-seq-prem1 der)
	      (cons (car pos-lines) pos-lines)
	      neg-lines conc)
	   (if (eq (car neg-lines) 'CONC)
	       (progn
		 (introduce-gap conc 2)
		 (my-comdecode (list 'cl-user::indirect (+ ln 2) (+ ln 1) ln
				  '$ '$ '$ '$))
		 (ftree-seq-to-proof-ab
		  (ftree-seq-prem1 der)
		  pos-lines (cons (numalias ln) (cons (numalias ln) (cdr neg-lines)))
		  (numalias (+ ln 1))))
	     (ftree-seq-to-proof-ab
	      (ftree-seq-prem1 der)
	      pos-lines
	      (cons (car neg-lines) neg-lines)
	      conc))))
	(FOCUS
	 (if (ftree-seq-pos-rule der)
	     (ftree-seq-to-proof-ab
	      (ftree-seq-prem1 der)
	      (unfocus-nth (ftree-seq-focus-n der) pos-lines)
	      neg-lines conc)
	   (ftree-seq-to-proof-ab
	    (ftree-seq-prem1 der) pos-lines
	    (unfocus-nth (ftree-seq-focus-n der) neg-lines)
	    conc)))
	(WEAKEN
	 (if (ftree-seq-pos-rule der)
	     (ftree-seq-to-proof-ab
	      (ftree-seq-prem1 der) (cdr pos-lines) neg-lines conc)
	   (if (and (eq (car neg-lines) 'CONC)
		    (not (false-p (line-assertion conc))))
	       (progn
		 (introduce-gap conc 1)
		 (my-comdecode (list 'cl-user::absurd (+ ln 1) ln '$ '$ '$))
		 (ftree-seq-to-proof-ab
		  (ftree-seq-prem1 der) pos-lines (cdr neg-lines)
		  (numalias ln)))
	     (ftree-seq-to-proof-ab
	      (ftree-seq-prem1 der) pos-lines (cdr neg-lines) conc)))))))

(defun ftree-seq-to-proof-ab (der pos-lines neg-lines conc)
  (let ((a-conc nil)
	(pos-lines1 nil)
	(neg-lines1 nil)
	(pos-wffs (ftree-seq-pos-wffs der))
	(neg-wffs (ftree-seq-neg-wffs der)))
    (mapc #'(lambda (x y)
	      (let ((wff (line-assertion y)))
		(if (wffeq x wff)
		    (push y pos-lines1)
		  (if (wffeq-ab x (line-assertion y))
		      (progn
			(introduce-gap conc 1)
			(my-comdecode (list 'cl-user::ab* (linealias (car pos-lines))
					 (- (linealias conc) 1)
					 (list 'quote x)
					 (list 'quote wff) '$ '$))
			(push (numalias (- (linealias conc) 1)) pos-lines1))
		    (throwfail "Positive Wff " (x . gwff) t
			       "Does Not Match Support Line " (y . existing-line))))))
	  pos-wffs pos-lines)
    (mapc #'(lambda (x y)
	      (if (eq y 'CONC)
		  (if a-conc
		      (throwfail "Multiple Concs:" (x . gwff) t
				 "and " (a-conc . gwff))
		    (let ((wff (line-assertion conc)))
		      (push 'CONC neg-lines1)
		      (setq a-conc x)
		      (unless (wffeq x wff)
			(if (wffeq-ab x wff)
			    (let ((ln (linealias conc)))
			      (introduce-gap conc 1)
			      (my-comdecode (list 'cl-user::ab* ln (+ ln 1)
					       (list 'quote wff)
					       (list 'quote x) '$ '$))
			      (setq conc (numalias ln)))
			  (throwfail "Conclusion " (x . gwff) t
				     "Does Not Match Planned Line "
				     (conc . existing-line))))))
		(let ((wff (line-assertion y)))
		  (if (or (wffeq (cons 'NOT x) wff)
			  (wffeq x (cons 'NOT wff)))
		      (push y neg-lines1)
		    (if (wffeq-ab (cons 'NOT x) wff)
			(progn
			  (introduce-gap conc 1)
			  (my-comdecode (list 'cl-user::ab* (linealias y)
					   (- (linealias conc) 1)
					   (list 'quote (cons 'NOT x))
					   (list 'quote wff) '$ '$))
			  (push (numalias (- (linealias conc) 1)) neg-lines1))
		      (if (wffeq-ab x (cons 'NOT wff))
			  (progn
			    (introduce-gap conc 1)
			    (my-comdecode (list 'cl-user::ab* (linealias y)
					     (- (linealias conc) 1)
					     (list 'quote (cdr x))
					     (list 'quote wff) '$ '$))
			    (push (numalias (- (linealias conc) 1)) neg-lines1))
			(throwfail "Negative Wff " (x . gwff) t
				   "Does Not Match Support Line "
				   (y . existing-line))))))))
	  neg-wffs neg-lines)
    (unless (or a-conc
		(eq 'FALSEHOOD (line-assertion conc)))
      (introduce-gap conc 1)
      (my-comdecode (list 'cl-user::absurd (linealias conc) (- (linealias conc) 1) '$ '$ '$))
      (setq conc (numalias (- (linealias conc) 1))))
    (unless (= (length pos-lines1) (length pos-lines))
      (throwfail "bad pos"))
    (unless (= (length neg-lines1) (length neg-lines))
      (throwfail "bad neg"))
    (ftree-seq-to-proof-rec der (reverse pos-lines1) (reverse neg-lines1)
			    conc)))

(defun check-lines-before (lines conc)
  (when lines
    (check-lines-before (cdr lines) conc)
    (unless (eq (car lines) 'conc)
      (when (>= (linealias (car lines)) (linealias conc))
	(throwfail "Line " ((car lines) . existing-line) t
		   "Should Preceed Line " (conc . existing-line))))))

(defun my-comdecode (com)
  (if query-user
      (when (query (format nil "Apply ~D?" (car com)) t)
	(when (proof-plans dproof) (pplan (caar (proof-plans dproof))))
	(comdecode com))
    (comdecode com)))

