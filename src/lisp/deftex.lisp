;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of TEX-WFF)

;;;
;;; File: DefTeX
;;; Package: TeX-Wff
;;;
;;; defines an output format for formulas that may be printed by
;;; TeX.
;;;

(part-of tex-wff)

(deffile deftex
  (part-of tex-wff)
  (extension lsp)
  (mhelp "Creates TeX style printing."))

(context tex-style)

(defflag pagelength
  (subjects printing printing-tex)
  (flagtype posinteger)
  (default 55)
  (mhelp "Number of lines on an output page.  Used by printing routines to
determine where to break output."))

(defflag pagewidth
  (subjects printing-tex)
  (flagtype posinteger)
  (default 85)
  (mhelp "Width of a page.  When creating a TeX file, RIGHTMARGIN gets
temporarily set to this value.

See Also: RIGHTMARGIN"))

;;;
;;; First the style is defined, TeX special characters come later.
;;; TeX formulas (for now) should never be pretty-printed and
;;; should be short enough to fit on one line.  It may be advisable
;;; to set PRINTDEPTH to a fairly small value when printing formulas
;;; into a TeX file.
;;;

(defflag in-tex-math-mode
  (subjects printing-tex)
  (flagtype boolean)
  (default nil)
  (mhelp "If T, $'s will not be printed around wffs in style TeX."))

(defflag tex-mimic-scribe
  (subjects printing-tex)
  (flagtype boolean)
  (default T)
  (mhelp "If T, TEXPROOF will give a good-looking tex output.
If NIL, TEXPROOF cannot break formulas in terms of the connectives in it.
So the output is a little bit ugly. Change the flag into NIL only when you
cannot get a good-looking output by setting it to T."))

(defflag latex-emulation
  (subjects printing-tex)
  (flagtype boolean)
  (default nil)
  (mhelp "If T, all of the printing commands that produce TeX output
will produce output suitable for LaTeX instead. See LATEX-PREAMBLE,
LATEX-POSTAMBLE."))

(defflag tex-break-before-symbols
  (subjects printing-tex)
  (flagtype symbollist)
;  (default (AND OR IMPLIES EQUIV
;		= LESS LESSEQ COND EQP EQUIV FINITE MU NAT NC ONE RECURSION
;		SIGMA1 SUBSET SUCC UNITSET ZERO % COMPLEMENT EQUIVS INTERSECT
;		POWERSET SETEQUIV SETINTERSECT SETUNION UNION SUBRELATION
;		COMPOSE PRODUCT SETDIFF PLUS ADD1 ADDPAIR
;		EXISTS EXISTS1 EXISTSN FORALL FORALLN MU-BIND THAT))
  (default nil)
  (mhelp "A list of symbols that TeX will allow linebreaks before (when the
flags PPWFFLAG and DISPLAYWFF are NIL).  The command TEXPROOF already allows
line breaks before logical constants, quantifiers, abbreviations and
infix constants.

Users normally don't need to change this flag."))

(defvar tex-space-unit 4.5)
(defvar *in-tex-math-mode* nil)
(defvar *in-tex-display-mode* nil)

(defstyle tex
  (print-symbol pp-symbol-tex)
  (print-space-p pp-space-p-tex)
  (terpri-heuristics terpri-heuristics-tex)
  (print-typesym pp-typesym-tex)
  (print-type-char print-type-char-tex)
  (print-indent indent-tex)
  (print-tab tab-tex) ;new remyc
  (print-nextpar nextpar-tex) ;new
  (margin-correct margin-tex) ;new
  (display-prefix display-prefix-tex)
  (display-postfix display-postfix-tex)
  (begin-environment begin-env-tex) ;new
  (end-environment end-env-tex) ;new
  (text-prefix text-prefix-tex)
  (text-postfix text-postfix-tex)
  (print-line print-line-tex-1) ;was print-line-tex, but this is never what we want
  (char-cat tex-char)
  (mhelp "TEX stands for an output style to be run through TeX
(or LaTeX, if the flag LATEX-EMULATION is set)."))


(defstyle tex-1
  (print-symbol pp-symbol-tex)
  (print-space-p pp-space-p-tex)
  (terpri-heuristics terpri-heuristics-tex)
  (print-typesym pp-typesym-tex)
  (print-type-char print-type-char-tex)
  (print-indent indent-tex-1)
  (display-prefix display-prefix-tex)
  (display-postfix display-postfix-tex)
  (text-prefix text-prefix-tex)
  (text-postfix text-postfix-tex)
  (print-line print-line-tex-1)
  (mhelp "TEX-1 stands for an output style to be run through TeX
(or LaTeX, if the flag LATEX-EMULATION is set)."))

;;;-----------************-----------
;;;hx: I add this function here to strengthen the function of 'pp-symbol-tex'.

(defun upget (symbol prop)
    (get (intern (string-upcase (symbol-name symbol))) prop))

;;;-----------***********-----------

(defun pp-symbol-tex (symbol)
  (declare (special ppvirtflag))
  (cond ((stringp symbol) 
	 (if (string= symbol " ")
	     (pprinc0 symbol)
	   (if (or (string= symbol "&") (string= symbol "#") (string= symbol "\\")
		   (string= symbol "/") (string= symbol "_")
		   (string= symbol "%") (string= symbol "^") (string= symbol "~"))
	       (progn (pprinc "\\") (pprinc symbol)
		      (unless ppvirtflag (incf curpos)))
	     (progn
	       (unless ppvirtflag (setq curpos (+ curpos (length symbol))))
	       (pprinc symbol)))))
	((upget symbol 'texname)
	 (pp-tex-special-symbol symbol))
	((get symbol 'face)
	 (unless (eq TURNSTILE-INDENT-AUTO 'COMPRESS)
	   (unless ppvirtflag (incf curpos))
	   (pprinc "\\,"))
	 (do ((face (get symbol 'face) (cdr face))
	      (sup-flag nil)
	      (sub-flag nil))
	     ((null face)
	      (when (or sup-flag sub-flag) (pprinc0 "}")))
	   (let ((hxstring (if (symbolp (car face)) (symbol-name (car face)) (car face))))
	     (if (> (length hxstring) 2)
		 (let ((really-sub (or (= (length hxstring) 3)
				       (memq (elt hxstring 3) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\SPACE #\\)))))
		   (if (and really-sub (string= hxstring "SUP" :end1 3))
		       (progn
			 (unless ppvirtflag (setq curpos (+ curpos (* .5 (- (length hxstring) 3)))))
			 (unless sup-flag
			   (setq sup-flag t)
			   (pprinc0 "^{"))
			 (pprinc (string-nthcdr-2 3 hxstring)))
		     (when sup-flag (pprinc0 "}") (setq sup-flag nil)))
		   (if (and really-sub (string= hxstring "SUB" :end1 3))
		       (progn
			 (unless ppvirtflag (setq curpos (+ curpos (* .5 (- (length hxstring) 3)))))
			 (unless sub-flag
			   (setq sub-flag t)
			   (pprinc0 "_{"))
			 (pprinc (string-nthcdr-2 3 hxstring)))
		     (when sub-flag (pprinc0 "}") (setq sub-flag nil)))
		   (unless (or sup-flag sub-flag)
		     (pp-tex-special-symbol (car face) nil)))
	       (pp-tex-special-symbol (car face) nil)))))
	(t 
	 (unless ppvirtflag (setq curpos (+ 1 curpos (length (princ-to-string symbol)))))
	 (if (abbrev-q symbol)
	     (progn (pprinc "\\hbox{") (pprinc symbol) (pprinc " }"))
	   (progn (unless (or (and (> (length (princ-to-string symbol)) 3)
				   (string= "PLAN" (princ-to-string symbol) :end2 4))
			      (eq TURNSTILE-INDENT-AUTO 'COMPRESS))
		    (unless ppvirtflag (incf curpos)) ; cebrown 4/25/02
		    (pprinc "\\,"))
		  (pprinc symbol))))))

(defun string-nthcdr-2 (nth string)
   (remove-if #'(lambda (x) (declare (ignore x)) T) string :end nth))

(defun tex-break-before-symbol-p (symbol)
  (or (logconst-p symbol)
      (anyabbrev-p symbol)
      (eq (binder-p symbol) 'O)
      (get symbol 'infix) ; e.g., some library constants
      (member symbol tex-break-before-symbols)))

(defun pp-tex-special-symbol (symbol &optional (roman nil))
  (declare (special *in-tex-math-mode* ppvirtflag))
  (let ((sp-sym (and (symbolp symbol) (upget symbol 'texname))))
    (when (and *in-tex-math-mode*
	       (not ppvirtflag)
	       (tex-break-before-symbol-p symbol))
      (pprinc "$ $"))
    (when (and *in-tex-display-mode*
	       (not ppwfflag)
	       (not ppvirtflag)
	       (> (+ curpos 5) rightmargin)
	       (tex-break-before-symbol-p symbol))
      (setq curpos 0)
      (princ "$$ $$"))
    (if sp-sym
	(progn
	  (pprinc "\\")
	  (pprinc0 sp-sym)
	  (unless ppvirtflag (incf curpos))
	  )
      (progn
	(unless ppvirtflag (setq curpos (+ curpos 1 (length (princ-to-string symbol))))) ; cebrown 4/26/02
	(if (or (abbrev-q symbol) roman)
	    (progn (pprinc "\\hbox{") (pprinc symbol) (pprinc " }"))
	  (pprinc symbol))))))

(defun pp-space-p-tex (symbol x) 
  (declare (ignore symbol x))
  t)

(defun terpri-heuristics-tex (symbol)
  (declare (ignore symbol))
  (when (and rightmargin (> (curpos) (- rightmargin 10)))
    (terpri)))

(defun pp-typesym-tex (tp)
  (pprinc0 "_{")
  (pp-typesym tp)
  (pprinc0 "}"))

(defun print-type-char-tex (char)
  (if (eq char '$)
      (pprinc0 "\\$")
    (progn
      (pprinc0 "\\greek")
      (pp-lowercase char))))

;;;Indentation in TeX

(defun indent-tex (indent) 
  (indent-tex-1 indent))

(defun indent-tex-1 (indent)
  (when (< curpos indent)
    (princ (format nil "\\hskip~Apt " (* (- indent curpos) tex-space-unit)))
    (setq curpos indent)))

(defun display-prefix-tex () 
  (setq *in-tex-display-mode* t) ; cebrown 4/25/02
  (princ "$$"))

(defun display-postfix-tex ()
  (setq *in-tex-display-mode* nil) ; cebrown 4/25/02
  (princ "$$"))

(defun text-prefix-tex ()
  (when (not in-tex-math-mode)
    (setq *in-tex-math-mode* t) ; cebrown 4/25/02
    (if ppwfflag ; cebrown 3/8/03
	(princ "\\partformula{$")
      (princ "$"))))

(defun text-postfix-tex () 
  (when (not in-tex-math-mode)
    (setq *in-tex-math-mode* nil) ; cebrown 4/25/02
    (if ppwfflag ; cebrown 3/8/03
	(princ "$}")
      (princ "$"))))

(defcategory tex-char
  (define deftexfont)
  (properties
   (texname single)
   (mhelp single))
  (global-list global-texcharlist)
  (mhelp-line "tex special character")
  (mhelp-fn princ-mhelp-char))

(defun tab-tex () (msg " ~  "))

(defun nextpar-tex () (msg f) (terpri))

(defun margin-tex (item) (msg item " "))

(defun begin-env-tex (env)
  (msg t)) ;Bad...

(defun end-env-tex (env)
  (msg t)) ;Bad...


