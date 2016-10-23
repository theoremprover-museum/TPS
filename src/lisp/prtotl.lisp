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
;;; File: prtotl
;;;

(deffile prtotl
  (part-of otlnl)
  (extension clisp)
  (mhelp "Defines functions associated with printing of lines."))

(eval-when (load compile eval) ; otherwise defsynonym gets confused
(defflag turnstyle-indent
  (flagtype integer+)
  (default 13)
  (subjects otl-vars printing printing-tex)
  (mhelp "Number of columns (from leftmargin) that turnstile should be 
indented when writing proofs in a SCRIBE file or on the screen. Notice 
that slides use a different flag, SLIDES-TURNSTYLE-INDENT.
This flag and TURNSTILE-INDENT are synonymous.")))

(eval-when (load compile eval)
(defflag turnstyle-indent-auto
  (flagtype indentation)
  (default VARY)
  (subjects otl-vars printing printing-tex)
  (mhelp "Decides how turnstiles are printed in proofs. This flag works in
all styles other than TEX; in particular, it works in XTERM, GENERIC, 
SCRIBE and SLIDES styles. There are four possible settings:
FIX : put the turnstile in the column indicated by TURNSTYLE-INDENT 
      (or SLIDES-TURNSTYLE-INDENT, in style SLIDES).
MIN : print the turnstile as far to the left as possible while still having
      it in the same column on every line. (If this puts it off the right 
      margin, then this will default to the same behaviour as FIX.)
COMPRESS : similar to VARY, but also removes spaces at other points in the
           proof (e.g. around dots, and between line numbers and hypotheses).
VARY : print the turnstile one space after the hypotheses in each line
       (so it will move from line to line).")))

;The following comments are still true, but were removed because they're 
;confusing... MB 6/94
;Notice that TURNSTYLE-INDENT and SCRIBE-TURNSTYLE-INDENT are still used 
;to determine where the hypotheses are printed in styles SCRIBE and SLIDES; 
;if these are greater than 6, the hypotheses are lined up on column 6, 
;otherwise they are as close to the line number as possible. So you may 
;still want to set TURNSTYLE-INDENT, even if TURNSTYLE-INDENT-AUTO is MIN.

(definfo fix
  (mhelp "A flag setting for TURNSTILE-INDENT-AUTO.
When printing a proof, fixes the turnstiles in the column given by 
TURNSTILE-INDENT (so they'll all line up with one another).
Lines with large numbers of hypotheses will push the turnstile onto the
following line."))

(definfo min
  (mhelp "A flag setting for TURNSTILE-INDENT-AUTO.
When printing a proof, fixes the turnstiles as far to the left as possible
while still putting it in the same column on every line.
Lines with large numbers of hypotheses will push the column of turnstiles
far to the right of the page; if it moves too far to the right, then 
this flag will be treated as though it were set to FIX instead.

MIN is also a setting for a good many other flags, where it is mostly
self-explanatory."))

(definfo compress
  (mhelp "A flag setting for TURNSTILE-INDENT-AUTO.
Similar to VARY, but also removes other spaces in the proof (e.g. around
dots, and between line numbers and hypotheses)."))

(definfo vary
  (mhelp "A flag setting for TURNSTILE-INDENT-AUTO.
Print the turnstile one space after the hypotheses in each line, so 
the turnstiles will not all line up in one column in the final proof."))

(defsynonym turnstile-indent-auto
  (synonym turnstyle-indent-auto)
  (replace-old t)
  (mhelp "Decides how turnstiles are printed in proofs. This flag works in
all styles other than TEX; in particular, it works in XTERM, GENERIC, 
SCRIBE and SLIDES styles. There are four possible settings:
FIX : put the turnstile in the column indicated by TURNSTYLE-INDENT 
      (or SLIDES-TURNSTYLE-INDENT, in style SLIDES).
MIN : print the turnstile as far to the left as possible while still having
      it in the same column on every line.  (If this puts it off the right 
      margin, then this will default to the same behaviour as FIX.)
COMPRESS : similar to VARY, but also removes spaces at other points in the
           proof (e.g. around dots, and between line numbers and hypotheses).
VARY : print the turnstile one space after the hypotheses in each line
       (so it will move from line to line)."))

(defsynonym turnstile-indent
  (synonym turnstyle-indent)
  (replace-old t)
  (mhelp "Number of columns (from leftmargin) that turnstile should be 
indented when writing proofs in a SCRIBE file. Notice that slides use 
a different flag, SLIDES-TURNSTILE-INDENT.
This flag and TURNSTYLE-INDENT are synonymous."))

(eval-when (load compile eval) ; otherwise defsynonym gets confused
(defflag slides-turnstyle-indent
  (flagtype integer+)
  (default 4)
  (subjects otl-vars)
  (mhelp "Number of columns (from leftmargin) that turnstile should be
indented when making slides. Compare TURNSTYLE-INDENT.
This flag and SLIDES-TURNSTILE-INDENT are synonymous.")))

(defsynonym slides-turnstile-indent
  (synonym slides-turnstyle-indent)
  (replace-old t)
  (mhelp "Number of columns (from leftmargin) that turnstile should be
indented when making slides. Compare TURNSTILE-INDENT.
This flag and SLIDES-TURNSTYLE-INDENT are synonymous."))

(defun cstatus (&rest ignore)
  (declare (ignore ignore))
  ;; This used to print status information if certain flags were
  ;; set right.  Now it's a NOOP.
  nil)

(defun princsans (xxx) 
  (declare (ignore xxx)) nil)

(defun print-line-generic (line)
  (let ((hatomalist nil) (curpos 0) justificationlength)
    (declare (special hatomalist))
    (msg "(" (linealias line) ") ") 
    (if (eq turnstile-indent-auto 'compress) 
	(setq curpos (+ curpos 3 (length (princ-to-string (linealias line)))))
      (progn (spaces (- 3 (length (princ-to-string (linealias line)))))
	     (setq curpos 6)))
    (let ((hyp (format nil "~{~S,~}"
		       (mapcar #'linealias (get line 'hypotheses)))))
      (setq curpos (+ curpos (length (string-right-trim '(#\,) hyp))))
      (princ (string-right-trim '(#\,) hyp))
      (if (neq (length (string-right-trim '(#\,) hyp)) 0) (progn (princ " ") (setq curpos (1+ curpos)))))
    (if (eq turnstile-indent-auto 'fix) 
	(progn (if (> curpos turnstile-indent) (progn (msg t) (setq curpos -1)))
	       (indentation turnstile-indent) (setq curpos (1- turnstile-indent)))
      (if (eq turnstile-indent-auto 'min) 
	  (progn (let ((point (1+ (figure-out-indent)))) (indentation point) (setq curpos (1- point))))))
    (pcall print-symbol 'assert)
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

(defun %prtline (line)
  (declare (special printlineflag))
  (when printlineflag
    (msg F) (setq curpos 0) (pcall print-line line)))

(defun prtline (line)
  (let ((printlineflag T))
    (declare (special printlineflag))
    (%prtline line)))

(defun %prtlines (linelist)
  (dolist (line linelist)
    (%prtline line)))

;;Function PRTLINES assumes that lines are in increasing order.

(defun prtlines (linelist)
  (declare (special print-dots))
  (let* ((plan-lines (sort (mapcar #'car (get dproof 'plans)) #'lineordering))
	 (dot-lines
	  (mapcar #'(lambda (yyy) (get (get yyy 'max-label) 'linenumber))
		  (mapcar #'(lambda (xxx) (find-closest-gap (gaps) xxx))
			  (set-of pline plan-lines (memq pline linelist))))))
    (setq dot-lines (delete nil dot-lines))
  (do ((linelist linelist (cdr linelist))
       (next-dots (if print-dots (car dot-lines) nil))
       (dot-lines (cdr dot-lines)))
      ((null linelist))
    (when (and next-dots (not (< (get (car linelist) 'linenumber)
				 next-dots)))
      (case style
        (tex (msg F (t 15) "&&&$\\cdots\\cdots$&\\cr\\noalign{\\vskip6pt}"))
	(tex-1 (msg T "\\vbox{\\indent" T "\\hbox to 112pt {\\hfil$\\cdots\\cdots$\\hfil}" T "}\\filbreak"))
	(t (msg F (t 15) "...")))
;;;   hx: print(...) in tex file 9/15/92
      (do ((xxx dot-lines  (cdr xxx))) ;; (cdr dot-lines)
	  ((or (null xxx) (< (get (car linelist) 'linenumber) (car xxx)))
	   (setq next-dots (car xxx))
	   (setq dot-lines (cdr xxx)))))
    (prtline (car linelist)))))

(defun justlength (justification)  
  (declare (special hatomalist))
  (let ((ppvirtflag t)
	(ppwfflength 0)
	(ppwfflist nil)
	(temp hatomalist))
    (declare (special ppvirtflag ppwfflength ppwfflist))
    (princjustification justification)
    (setq hatomalist temp)
    ppwfflength))

(defun princjustification (j)
  (declare (special ppvirtflag ppwfflength ppwfflist hatomalist))
  (let (pc hatomalist)
    (declare (special pc hatomalist))
    (cond ((null j) (pprinc " "))
	  ((symbolp (car j))
	   (pcall print-symbol (car j)))
	  (T (pprinc (car j))))
    (when (or (cadr j) (caddr j))
      (pprinc ":")
      (setq pc '| |)
      (if (and (or (eq style 'tex) (eq style 'tex-1)) (cadr j))
	  (pprinc0 "\\ $"));;hx:print math symbols for tex (9/17/92)
      (dolist (wff (cadr j))
	      (pprinc " ")
	      (printwffscope wff nil 1))
      (if (and (or (eq style 'tex) (eq style 'tex-1)) (cadr j)) 
	  (pprinc0 "$\\ "))  ;hx:print math symbols for tex (9/17/92) 
      (dolist (line (caddr j))
	(pprinc " ")
	(pprinc (princ-to-string (linealias line)))))))

(defun hack-about (wff)
  (multiple-value-bind (binders rest)
		       (strip-out-lambda-binders wff '())
		       (if (and binders (equal (type rest) 'O))
			   (list binders rest)
			 (list nil wff))))

(defun strip-out-lambda-binders (wff bd)
  (if (and (boundwff-q wff) (eq (cdar wff) 'LAMBDA))
      (strip-out-lambda-binders (cdr wff) (cons (caar wff) bd))
    (values (reverse bd) wff)))

(context otl-printing)

(defmexpr pwtypes
  (argnames gwff)
  (argtypes gwff)
  (mhelp "Prints a wff showing types."))

(defmexpr showtypes
  (mhelp "From now on show the types on all wffs."))

(defun showtypes () (setq printtypes t))

(defmexpr shownotypes
  (mhelp "Suppress the printing of types on all wffs."))

(defun shownotypes () (setq printtypes nil))
